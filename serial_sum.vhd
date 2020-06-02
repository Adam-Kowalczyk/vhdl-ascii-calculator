library ieee;
use     ieee.std_logic_1164.all;
use     ieee.std_logic_unsigned.all;
use     ieee.std_logic_misc.all;
use STD.textio.all;                     -- basic I/O
use IEEE.std_logic_textio.all;          -- I/O for logic types

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

  type     INSTRUKCJA	is (WCZYTAJ, WYSYLAJ, STOJ); -- lista instrukcji pracy interpretera
  signal   rozkaz	:INSTRUKCJA;				-- rejestr maszyny stanow interpretera
  
  type		DZIALANIE is (INICJUJ, DODAJ, ODEJMIJ, LICZ);
  signal 	operacja: DZIALANIE;

  signal   wynik		:integer ;		-- liczba argumentu 1
  signal   obecny: integer;

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
	  
	  variable wczytana :natural range 0 to 9;

   begin							-- poczatek ciala procesu kalkulatora

     if (R='1') then						-- asynchroniczna inicjalizacja rejestrow
      tx_slowo	<= (others => '0');				-- wyzerowanie nadawanego slowa danych
      tx_nadaj <= '0';						-- wyzerowanie flagi zadania nadawania
      rozkaz   <= WCZYTAJ;					-- poczatkowy stan pracy interpretera
		operacja <= INICJUJ;
		wynik <= 0;
		obecny <= 0;

     elsif (rising_edge(C)) then				-- synchroniczna praca kalkulatora

       tx_nadaj	<= '0';						-- defaultowe ustawienie flagi zadania nadawania

		if (rx_gotowe='1') then				-- obsluga potwierdzenia odbioru slowa przez 'SERIAL_RX'
         tx_slowo <= rx_slowo;					-- ustawienie slowa nadawania na slowo odebrane (echo)
         tx_nadaj <= '1';					-- ustawienie flagi zadania nadawania przez 'SERIAL_TX'
			case rozkaz is					-- badanie aktualnego stanu maszyny interpretera 
				when WCZYTAJ =>					-- obsluga stanu ARGUMENT1
				if (rx_slowo=kod_znaku('+')) then
					case operacja is
						when INICJUJ => wynik <= obecny;
						when DODAJ => wynik <= wynik + obecny;
						when ODEJMIJ => wynik <= wynik - obecny;
						when LICZ => rozkaz <= STOJ;
					end case;
					operacja <= DODAJ;
					obecny <= 0;
				elsif (rx_slowo=kod_znaku('-')) then
					case operacja is
						when INICJUJ => wynik <= obecny;
						when DODAJ => wynik <= wynik + obecny;
						when ODEJMIJ => wynik <= wynik - obecny;
						when LICZ => rozkaz <= STOJ;
					end case;
					operacja <= ODEJMIJ;
					obecny <= 0;
				elsif (rx_slowo=kod_znaku('=')) then
					case operacja is
						when INICJUJ => wynik <= obecny;
						when DODAJ => wynik <= wynik + obecny;
						when ODEJMIJ => wynik <= wynik - obecny;
						when LICZ => rozkaz <= STOJ;
					end case;
					operacja <= LICZ;
					obecny <= 0;
				elsif (wyzn_cyfre(rx_slowo)/=10) then		-- odebrano znak cyfry
					wczytana := wyzn_cyfre(rx_slowo);		-- zapamietanie warosci cyfry w wektorze arg1
					obecny <= (obecny * 10) + wczytana;
				 else
					rozkaz <= STOJ;
				 end if;
				 
				 when WYSYLAJ => null;
				 
				 when STOJ => null;
				 
         end case;
       end if;							-- zakonczenie instukcji warunkowej
 
     end if;							-- zakonczenie instukcji warunkowej proces

   end process;							-- zakonczenie ciala kalkulatora
  my_print : process is                  -- a process is parallel
			variable my_line : line;  -- type 'line' comes from textio
		 begin
			write(my_line, string'("Wyniki:"));   -- formatting
			writeline(output, my_line);               -- write to "output"
			write(my_line, string'("  wynik= "));
			write(my_line, wynik);  -- format 'counter' as integer
			write(my_line, string'("  obecny= "));
			write(my_line, obecny);                     -- format time
			writeline(output, my_line);              -- write to display
			wait for 100 ns;
		 end process my_print;
end behavioural;

