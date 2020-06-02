library ieee;
use     ieee.std_logic_1164.all;
use     ieee.std_logic_unsigned.all;
use     ieee.std_logic_misc.all;

entity SERIAL_SUM is
  generic (
    F_ZEGARA		:natural := 20_000_000;			-- czestotliwosc zegata w [Hz]
    L_BODOW		:natural := 9600;			-- predkosc nadawania w [bodach]
    B_SLOWA		:natural := 8;				-- liczba bitow slowa danych (5-8)
    B_STOPOW		:natural := 2;				-- liczba bitow stopu (1-2)
    L_CYFR		:natural := 3;				-- liczba cyfr dziesietnych
    L_BODOW_PRZERWY	:natural := 0				-- czas przerwy w nadawaniu w [bodach]
  );
  port (
    R			:in  std_logic;				-- sygnal resetowania
    C			:in  std_logic;				-- zegar taktujacy
    RX			:in  std_logic;				-- odbierany sygnal szeregowy
    TX			:out std_logic				-- wysylany sygnal szeregowy
  );
end SERIAL_SUM;

architecture behavioural of SERIAL_SUM is

  signal   rx_slowo	:std_logic_vector(B_SLOWA-1 downto 0);	-- odebrane slowo danych
  signal   rx_gotowe	:std_logic;				-- flaga potwierdzenia odbioru
  signal   rx_blad	:std_logic;				-- flaga wykrycia bledu w odbiorze

  signal   tx_slowo	:std_logic_vector(B_SLOWA-1 downto 0);	-- wysylane slowo danych
  signal   tx_nadaj	:std_logic;				-- flaga zadania nadawania
  signal   tx_wysylanie	:std_logic;				-- flaga potwierdzenia nadawania

  type     INSTRUKCJA	is (ARGUMENT1, ARGUMENT2, OBLICZAJ, STOJ); -- lista instrukcji pracy interpretera
  signal   rozkaz	:INSTRUKCJA;				-- rejestr maszyny stanow interpretera

  subtype  CYFRA	is natural range 0 to 9;		-- typ cyfry dziesietnej
  type     LICZBA	is array(natural range <>) of CYFRA;	-- typ liczby dziesietnej zlozonej z cyfr

  signal   arg1		:LICZBA(L_CYFR-1 downto 0);		-- liczba argumentu 1
  signal   arg2		:LICZBA(L_CYFR-1 downto 0);		-- liczba argumentu 2
  signal   suma		:LICZBA(L_CYFR-1 downto 0);		-- liczba sumy argumentow
  signal   lcyfr	:natural range 0 to L_CYFR;		-- licznik cyfr argumentu

  type     OBLICZANIE	is (SUMOWANIE, WYSYLANIE, CZEKANIE);	-- lista instrukcji wyznaczania wyniku
  signal   liczenie	:OBLICZANIE;				-- rejestr maszyny stanow wyznaczania wyniku
  signal   przenos	:natural range 0 to 1;			-- wartosc przeniesienia czastkowego sumowania

  constant SLOWO_ZERO	:std_logic_vector(B_SLOWA-1 downto 0) := (others => '0'); -- slowo z ustawiona wartoscia 0

  constant T_PRZERWY	:integer := (F_ZEGARA/L_BODOW)*L_BODOW_PRZERWY; -- liczba okresow zegara przerwy w nadawaniu
  signal   lprzerwy	:natural range 0 to T_PRZERWY;		-- licznik taktow przerwy w nadawaniu
  
begin								-- cialo architekury sumowania

  srx: entity work.SERIAL_RX(behavioural)			-- instancja odbirnika szeregowego 'SERIAL_RX'
    generic map(						-- mapowanie parametrow biezacych
      F_ZEGARA             => F_ZEGARA,				-- czestotliwosc zegata w [Hz]
      L_BODOW              => L_BODOW,				-- predkosc odbierania w [bodach]
      B_SLOWA              => B_SLOWA,				-- liczba bitow slowa danych (5-8)
      B_STOPOW             => B_STOPOW				-- liczba bitow stopu (1-2)
    )
    port map(							-- mapowanie sygnalow do portow
      R                    => R,				-- sygnal resetowania
      C                    => C,				-- zegar taktujacy
      RX                   => RX,				-- odebrany sygnal szeregowy
      SLOWO                => rx_slowo,				-- odebrane slowo danych
      GOTOWE               => rx_gotowe,			-- flaga potwierdzenia odbioru
      BLAD                 => rx_blad				-- flaga wykrycia bledu w odbiorze
    );

  stx: entity work.SERIAL_TX(behavioural)			-- instancja nadajnika szeregowego 'SERIAL_TX'
    generic map(						-- mapowanie parametrow biezacych
      F_ZEGARA             => F_ZEGARA,				-- czestotliwosc zegata w [Hz]
      L_BODOW              => L_BODOW,				-- predkosc nadawania w [bodach]
      B_SLOWA              => B_SLOWA,				-- liczba bitow slowa danych (5-8)
      B_STOPOW             => B_STOPOW				-- liczba bitow stopu (1-2)
    )
    port map(							-- mapowanie sygnalow do portow
      R                    => R,				-- sygnal resetowania
      C                    => C,				-- zegar taktujacy
      TX                   => tx,				-- nadawany sygnal szeregowy
      SLOWO                => tx_slowo,				-- nadawane slowo danych
      NADAJ                => tx_nadaj,				-- flaga zadania nadawania
      WYSYLANIE            => tx_wysylanie			-- flaga potwierdzenia nadawania
    );

   process (R, C) is						-- proces kalkulatora

     function kod_znaku(c :character) return std_logic_vector is -- konwersja kodu znaku do rozmiaru slowa
     begin							-- cialo funkcji
       return(SLOWO_ZERO+character'pos(c));			-- wyznaczenia i zwrocenie wartosci slowa
     end function;						-- zakonczenie funkcji

     constant BLAD_ODBIORU    :std_logic_vector := kod_znaku('!'); -- slowo z kodem przypisanym do bledu odbioru
     constant BLAD_INSTRUKCJI :std_logic_vector := kod_znaku('?'); -- slowo z kodem przypisanym do bledu instrukcji

     function wyzn_cyfre(a :std_logic_vector) return natural is -- konwersja kodu slowa zawierajacego cyfre na warosc
     begin							-- cialo funkcji
       if (a>=kod_znaku('0') and a<=kod_znaku('9')) then	-- zbadanie czy kod slowa jest cyfra
         return(CONV_INTEGER(a)-character'pos('0'));		-- wyznaczenia i zwrocenie wartosci cyfry
       else							-- lowo nie jest cyfra
         return(10);						-- zwrocenie flagi bledu jako wartosci 10
       end if;							-- zakonczenie instukcji warunkowej
     end function;						-- zakonczenie funkcji

     variable suma_cyfr :natural range 0 to 19;

   begin							-- poczatek ciala procesu kalkulatora

     if (R='1') then						-- asynchroniczna inicjalizacja rejestrow

       tx_slowo	<= (others => '0');				-- wyzerowanie nadawanego slowa danych
       tx_nadaj <= '0';						-- wyzerowanie flagi zadania nadawania
       rozkaz   <= ARGUMENT1;					-- poczatkowy stan pracy interpretera
       arg1     <= (others => 0);				-- wyzerowanie argumetu 1
       arg2     <= (others => 0);				-- wyzerowanie argumetu 2
       suma     <= (others => 0);				-- wyzerowanie sumy argumentow
       lcyfr    <= 0;						-- wyzerowanie licznika cyfr
       przenos  <= 0;						-- wyzerowanie wartosci przeniesienia
       lprzerwy  <= 0;						-- wyzerowanie licznika przerwy w nadawaniu

     elsif (rising_edge(C)) then				-- synchroniczna praca kalkulatora

       tx_nadaj	<= '0';						-- defaultowe ustawienie flagi zadania nadawania

       if (rx_blad='1') then					-- obsluga bledu odbioru zgloszonego przez 'SERIAL_RX'
         tx_slowo <= BLAD_ODBIORU;				-- ustawienie slowa nadawania na BLAD_ODBIORU
         tx_nadaj <= '1';					-- ustawienie flagi zadania nadawania przez 'SERIAL_TX'
         rozkaz   <= STOJ;					-- przejscie awaryjne do stanu STOJ
       elsif (rx_gotowe='1') then				-- obsluga potwierdzenia odbioru slowa przez 'SERIAL_RX'
         tx_slowo <= rx_slowo;					-- ustawienie slowa nadawania na slowo odebrane (echo)
         tx_nadaj <= '1';					-- ustawienie flagi zadania nadawania przez 'SERIAL_TX'
         if (rx_slowo=kod_znaku(LF) or rx_slowo=kod_znaku(CR)) then -- zbadanie zadania inicjalizacji
           rozkaz <= ARGUMENT1;					-- poczatkowy stan pracy interpretera
           arg1   <= (others => 0);				-- wyzerowanie argumetu 1
           arg2   <= (others => 0);				-- wyzerowanie argumetu 2
           suma   <= (others => 0);				-- wyzerowanie sumy argumentow
           lcyfr  <= 0;						-- wyzerowanie licznika cyfr
	 else							-- interpretacja odebranego slowa
	   case rozkaz is					-- badanie aktualnego stanu maszyny interpretera 

	     when ARGUMENT1 =>					-- obsluga stanu ARGUMENT1
	       if (rx_slowo=kod_znaku('+')) then		-- odebrano znak operatora sumowania
	         lcyfr  <= 0;					-- wyzerowanie licznika cyfr
	         rozkaz <= ARGUMENT2;				-- przejscie do stanu ARGUMENT2
	       elsif (wyzn_cyfre(rx_slowo)/=10) then		-- odebrano znak cyfry
	         arg1(0) <= wyzn_cyfre(rx_slowo);		-- zapamietanie warosci cyfry w wektorze arg1
	         arg1(arg1'left downto 1) <= arg1(arg1'left-1 downto 0); -- przesuniecie w lewo wektora arg1
		 if (lcyfr /= L_CYFR) then			-- badanie liczby odebrsnych cyfr
	           lcyfr  <= lcyfr + 1;				-- zwiekszenie o 1 liczby odebrsnych cyfr
		 else						-- przekroczono liczbe odebrsnych cyfr
                   tx_slowo <= BLAD_INSTRUKCJI;			-- ustawienie slowa nadawania na BLAD_INSTRUKCJI
	           rozkaz   <= STOJ;				-- przejscie awaryjne do stanu STOJ
		 end if;					-- zakonczenie instukcji warunkowej
	       else						-- odebrano nieprawidlowy znak
                 tx_slowo <= BLAD_INSTRUKCJI;			-- ustawienie slowa nadawania na BLAD_INSTRUKCJI
	         rozkaz   <= STOJ;				-- przejscie awaryjne do stanu STOJ
	       end if;						-- zakonczenie instukcji warunkowej

	     when ARGUMENT2 =>					-- obsluga stanu ARGUMENT2
	       if (rx_slowo=kod_znaku('=')) then		-- odebrano znak operatora rownosci
	         lcyfr  <= 0;					-- wyzerowanie licznika cyfr
	         rozkaz <= OBLICZAJ;				-- przejscie do stanu OBLICZAJ
	       elsif (wyzn_cyfre(rx_slowo)/=10) then		-- odebrano znak cyfry
	         arg2(0) <= wyzn_cyfre(rx_slowo);		-- zapamietanie warosci cyfry w wektorze arg2
	         arg2(arg2'left downto 1) <= arg2(arg2'left-1 downto 0); -- przesuniecie w lewo wektora arg2
		 if (lcyfr /= L_CYFR) then			-- badanie liczby odebrsnych cyfr
	           lcyfr  <= lcyfr + 1;				-- zwiekszenie o 1 liczby odebrsnych cyfr
		 else						-- przekroczono liczbe odebrsnych cyfr
                   tx_slowo <= BLAD_INSTRUKCJI;			-- ustawienie slowa nadawania na BLAD_INSTRUKCJI
	           rozkaz   <= STOJ;				-- przejscie awaryjne do stanu STOJ
		 end if;					-- zakonczenie instukcji warunkowej
	       else						-- odebrano nieprawidlowy znak
                 tx_slowo <= BLAD_INSTRUKCJI;			-- ustawienie slowa nadawania na BLAD_INSTRUKCJI
	         rozkaz   <= STOJ;				-- przejscie awaryjne do stanu STOJ
	       end if;						-- zakonczenie instukcji warunkowej

	     when OBLICZAJ => null;				-- pusta obsluga stanu OBLICZAJ

	     when STOJ =>					-- pusta obsluga stanu STOJ
               tx_slowo <= BLAD_INSTRUKCJI;			-- ustawienie slowa nadawania na BLAD_INSTRUKCJI

	   end case;						-- zakonczenie instukcji warunkowego wyboru
         end if;						-- zakonczenie instukcji warunkowej
       end if;							-- zakonczenie instukcji warunkowej

       if (rozkaz /= OBLICZAJ) then				-- oczekiwanie na stan OBLICZAJ interpretera
         przenos  <= 0;						-- wyzerowanie wartosci przeniesienia
         liczenie <= SUMOWANIE;					-- ustawienie poczatkowe stanu SUMOWANIE
         lprzerwy  <= 0;					-- wyzerowanie licznika przerwy w nadawaniu
       else							-- osiagnieto stan OBLICZAJ
         case liczenie is

           when SUMOWANIE =>					-- obsluga stanu SUMOWANIE
	     suma_cyfr := arg1(0) + arg2(0) + przenos;		-- wyznaczenie sumy czastkowej
	     arg1(arg1'left-1 downto 0) <= arg1(arg1'left downto 1); -- przesuniecie w prawo wektora arg1
	     arg2(arg2'left-1 downto 0) <= arg2(arg2'left downto 1); -- przesuniecie w prawo wektora arg2
	     if (suma_cyfr<10) then				-- zbadanie czy nie powstalo przeniesienie
	       suma(0) <= suma_cyfr;				-- zapamietanie warosci cyfry w wektorze sumy
	       przenos <= 0;					-- wyzerowanie wartosci przeniesienia
	     else						-- wariant gdy powstalo przeniesienie
	       suma(0) <= suma_cyfr-10;				-- zapamietanie warosci cyfry w wektorze sumy
	       przenos <= 1;					-- ustawienie wartosci przeniesienia
	     end if;						-- zakonczenie instukcji warunkowej
	     suma(suma'left downto 1) <= suma(suma'left-1 downto 0); -- przesuniecie w lewo wektora sumy
             if (lcyfr /= L_CYFR-1) then			-- badanie czy pozostaly cyfry do sunowania
               lcyfr <= lcyfr + 1;				-- zwiekszenie o 1 liczby zsumowanych cyfr
             else						-- wykonano sumowanie czastkowe wszystkich cyfr
	       lcyfr    <= 0;					-- wyzerowanie licznika cyfr
	       przenos  <= 0;					-- wyzerowanie wartosci przeniesienia
								 
									   
	       liczenie <= CZEKANIE;				-- przejscie do stanu CZEKANIE
							
	     end if;						-- zakonczenie instukcji warunkowej

           when WYSYLANIE =>					-- obsluga stanu WYSYLANIE
             if (lcyfr /= L_CYFR) then				-- badanie czy pozostaly cyfry do wyslania
               lcyfr <= lcyfr + 1;				-- zwiekszenie o 1 liczby wyslanych cyfr
	       if (przenos=1 or suma(0)/=0 or lcyfr=L_CYFR-1) then -- badanie czy nalezy wyslac cyfre 
                 tx_slowo <= SLOWO_ZERO+character'pos('0')+suma(0); -- wyznaczenie i ustawienie kodu wysylanej cyfry
                 tx_nadaj <= '1';				-- ustawienie flagi zadania nadawania przez 'SERIAL_TX'
		 przenos  <= 1;					-- ustawienie flagi przeniesienia jako znacznika wysylania
	         liczenie <= CZEKANIE;				-- przejscie do stanu CZEKANIE
	       end if;						-- zakonczenie instukcji warunkowej
             else						-- wykonano wyslanie wszystkich cyfr
	       liczenie <= SUMOWANIE;				-- przejscie do stanu SUMOWANIE
	       rozkaz   <= STOJ;				-- przejscie do stanu STOJ interpretera
	     end if;						-- zakonczenie instukcji warunkowej
	     suma(suma'left-1 downto 0) <= suma(suma'left downto 1); -- przesuniecie w prawo wektora sumy

           when CZEKANIE =>					-- obsluga stanu CZEKANIE
	     if (tx_nadaj='0' and tx_wysylanie='0') then	-- badanie czy 'SERIAL_TX' nie jest aktywny
	       if (lprzerwy /= T_PRZERWY) then			-- badanie czy trwa przerwa w nadawniu
	         lprzerwy <= lprzerwy + 1;			-- wzwieksza licznika przerwy w nadawaniu
	       else						-- osiagnieto czas przerwy w nadawaniu
                 lprzerwy  <= 0;				-- wyzerowanie licznika przerwy w nadawaniu
	         liczenie <= WYSYLANIE;				-- przejscie do stanu WYSYLANIE
	       end if;						-- zakonczenie instukcji warunkowej
	     else						-- wariant, gdy 'SERIAL_TX' jest aktywny
               lprzerwy  <= 0;					-- wyzerowanie licznika przerwy w nadawaniu
	     end if;						-- zakonczenie instukcji warunkowej

         end case;
       end if;							-- zakonczenie instukcji warunkowej
 
     end if;							-- zakonczenie instukcji warunkowej procesu

   end process;							-- zakonczenie ciala kalkulatora
   
end behavioural;

