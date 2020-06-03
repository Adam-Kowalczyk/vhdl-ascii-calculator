library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.std_logic_misc.all;
use ieee.math_real.all;
use STD.textio.all; 
use IEEE.std_logic_textio.all; 

entity SERIAL_CALCULATOR is
	generic (
		CZESTOTLIWOSC_ZEGARA : natural := 20_000_000; 
		ILOSC_BODOW : natural := 9600; 
		WIELKOSC_SLOWA : natural := 8; 
		LICZBA_BITOW_STOPU : natural := 2 
	);
	port (
		R : in std_logic; 
		C : in std_logic; 
		RX : in std_logic; 
		TX : out std_logic 
	);
end SERIAL_CALCULATOR;

architecture behavioural of SERIAL_CALCULATOR is

	signal urx_slowo : std_logic_vector(WIELKOSC_SLOWA - 1 downto 0); 
	signal urx_odczytane : std_logic; 
	signal urx_blad : std_logic; 

	signal utx_slowo : std_logic_vector(WIELKOSC_SLOWA - 1 downto 0); 
	signal utx_wysylaj : std_logic; 
	signal utx_wysylanie : std_logic; 

	type INSTRUKCJA is (WCZYTAJ, ZACZWYSYL, WYSYLAJ, STOJ, CZEKAJ);
	signal rozkaz : INSTRUKCJA; 
 
	type DZIALANIE is (INICJUJ, DODAJ, ODEJMIJ, MNOZENIE);
	signal operacja : DZIALANIE;
	signal przed_mnozeniem : DZIALANIE;

	signal wynik : integer range -1000000 to 1000000; 
	signal obecny : integer range -1000000 to 1000000;
	signal wyn_mnozenia : integer range -1000000 to 1000000;
	signal odbieranie : std_logic;
	signal dlugosc_wyniku : integer range 0 to 10;

	constant WYZEROWANE_SLOWO : std_logic_vector(WIELKOSC_SLOWA - 1 downto 0) := (others => '0');
 
begin

	utx : entity work.UART_TX(behavioural) 
		generic map(
			CZESTOTLIWOSC_ZEGARA => CZESTOTLIWOSC_ZEGARA, 
			ILOSC_BODOW => ILOSC_BODOW, 
			WIELKOSC_SLOWA => WIELKOSC_SLOWA, 
			LICZBA_BITOW_STOPU => LICZBA_BITOW_STOPU 
		)
		port map(
			R => R, 
			C => C, 
			TX => tx, 
			SLOWO => utx_slowo, 
			WYSYLAJ => utx_wysylaj, 
			WYSYLANIE => utx_wysylanie 
		);
	
	urx : entity work.UART_RX(behavioural) 
		generic map(
			CZESTOTLIWOSC_ZEGARA => CZESTOTLIWOSC_ZEGARA, 
			ILOSC_BODOW => ILOSC_BODOW, 
			WIELKOSC_SLOWA => WIELKOSC_SLOWA, 
			LICZBA_BITOW_STOPU => LICZBA_BITOW_STOPU 
		)
		port map(
			R => R, 
			C => C, 
			RX => RX, 
			SLOWO => urx_slowo, 
			ODCZYTANE => urx_odczytane, 
			BLAD => urx_blad 
		);

	process (R, C) is 

		function kod_znaku(c : character) return std_logic_vector is
			begin
				return(WYZEROWANE_SLOWO + character'pos(c)); 
		end function; 

		constant BLAD_ODBIORU : std_logic_vector := kod_znaku('!');
		constant BLAD_INSTRUKCJI : std_logic_vector := kod_znaku('?');

		function wyzn_cyfre(a : std_logic_vector) return natural is
			begin
				if (a >= kod_znaku('0') and a <= kod_znaku('9')) then return(CONV_INTEGER(a) - character'pos('0')); 
				else
					return(10); 
				end if; 
		end function;
		
		function f_log10 (x : integer) return natural is
			variable i : natural;
		begin
			i := 0;  
			while (10**i <= x) and i < 10 loop
				i := i + 1;
			end loop;
			return i;
		end function;
		
		function pot10 (x : integer) return natural is
			variable wyn :natural;
			variable i : natural;
		begin
			i := 1;
			wyn := 1;  
			while (i <= x) loop
				wyn := wyn * 10;
				i := i + 1;
			end loop;
			return wyn;
		end function;
		
		function findFirst (liczba, pot : natural) return natural is
			variable temp : natural;
			variable i : natural;
		begin
			i := 0;
			temp := liczba;  
			while (temp >= pot) loop
				temp := temp - pot;
				i := i + 1;
			end loop;
			return i;
		end function;
		
		function divisionRest (liczba, pot : natural) return natural is
			variable temp : natural;
			variable i : natural;
		begin
			i := 0;
			temp := liczba;  
			while (temp >= pot) loop
				temp := temp - pot;
				i := i + 1;
			end loop;
			return temp;
		end function;

		variable wczytana : natural range 0 to 9;

		variable my_line : line;

	begin
		if (R = '1') then 
			utx_slowo <= (others => '0'); 
			utx_wysylaj <= '0'; 
			rozkaz <= WCZYTAJ; 
			operacja <= INICJUJ;
			wynik <= 0;
			obecny <= 0;
			odbieranie <= '1';
			wyn_mnozenia <= 0;

		elsif (rising_edge(C)) then 

			utx_wysylaj <= '0'; 

			if (odbieranie = '1') then
				if (urx_odczytane = '1') then 
					case rozkaz is 
					
						when WCZYTAJ => 
							if (urx_slowo = kod_znaku('+') or urx_slowo = kod_znaku('-') or urx_slowo = kod_znaku('=')) then
								case operacja is
									when INICJUJ => wynik <= obecny;
									when DODAJ => wynik <= wynik + obecny;
									when ODEJMIJ => wynik <= wynik - obecny;
									when MNOZENIE => 
										case przed_mnozeniem is
											when INICJUJ => wynik <= wyn_mnozenia * obecny;
											when DODAJ => wynik <= wynik + wyn_mnozenia * obecny;
											when ODEJMIJ => wynik <= wynik - wyn_mnozenia * obecny;
											when others => null;
									end case;
								end case;						
							end if;
							if (urx_slowo = kod_znaku('+')) then
								operacja <= DODAJ;
								obecny <= 0;
							elsif (urx_slowo = kod_znaku('-')) then
								operacja <= ODEJMIJ;
								obecny <= 0;
							elsif (urx_slowo = kod_znaku('=')) then
								rozkaz <= ZACZWYSYL;
								obecny <= 0;
								odbieranie <= '0';
							elsif (urx_slowo = kod_znaku('*')) then
								if (operacja /= MNOZENIE) then
									wyn_mnozenia <= obecny;
									przed_mnozeniem <= operacja;
								else
									wyn_mnozenia <= wyn_mnozenia * obecny;
								end if;
								operacja <= MNOZENIE;
								obecny <= 0;
							elsif (wyzn_cyfre(urx_slowo) /= 10) then 
								wczytana := wyzn_cyfre(urx_slowo); 
								obecny <= (obecny * 10) + wczytana;
							else
								rozkaz <= STOJ;
							end if;

						when others => null;
					end case;
				end if; 
			else
				case rozkaz is
				
					when ZACZWYSYL => 
						if (wynik = 0) then
							dlugosc_wyniku <= 1;
							rozkaz <= WYSYLAJ;
						elsif (wynik < 0) then
							utx_slowo <= WYZEROWANE_SLOWO + character'pos('-');
							utx_wysylaj <= '1';
							wynik <= - wynik;
							rozkaz <= CZEKAJ;
							dlugosc_wyniku <= f_log10(-wynik);
						else
							dlugosc_wyniku <= f_log10(wynik);
							rozkaz <= WYSYLAJ;
						end if;
						
					when WYSYLAJ => 
						if (dlugosc_wyniku > 0) then
							utx_slowo <= WYZEROWANE_SLOWO + character'pos('0') + findFirst(wynik, pot10(dlugosc_wyniku - 1));
							wynik <= divisionRest(wynik, pot10(dlugosc_wyniku - 1));
							utx_wysylaj <= '1';
							rozkaz <= CZEKAJ;
							dlugosc_wyniku <= dlugosc_wyniku - 1;
						else
							rozkaz <= STOJ;
						end if;

					when CZEKAJ => 
						if (utx_wysylaj = '0' and utx_wysylanie = '0') then
							rozkaz <= WYSYLAJ;
						end if;

					when others => null;
				end case;
			end if;
		end if;

	end process;
	
end behavioural;