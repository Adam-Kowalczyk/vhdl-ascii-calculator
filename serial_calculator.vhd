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

	signal rx_slowo : std_logic_vector(WIELKOSC_SLOWA - 1 downto 0); 
	signal rx_gotowe : std_logic; 
	signal rx_blad : std_logic; 

	signal tx_slowo : std_logic_vector(WIELKOSC_SLOWA - 1 downto 0); 
	signal tx_nadaj : std_logic; 
	signal tx_wysylanie : std_logic; 

	type INSTRUKCJA is (WCZYTAJ, ZACZWYSYL, WYSYLAJ, STOJ, CZEKAJ);
	signal rozkaz : INSTRUKCJA; 
 
	type DZIALANIE is (INICJUJ, DODAJ, ODEJMIJ, MNOZENIE);
	signal operacja : DZIALANIE;
	signal przed_mnozeniem : DZIALANIE;

	signal wynik : integer; 
	signal obecny : integer;
	signal wyn_mnozenia : integer;
	signal odbieranie : std_logic;
	signal dlugosc_wyniku : integer;

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
			SLOWO => tx_slowo, 
			WYSYLAJ => tx_nadaj, 
			WYSYLANIE => tx_wysylanie 
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
			SLOWO => rx_slowo, 
			WYSLANE => rx_gotowe, 
			BLAD => rx_blad 
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

		variable wczytana : natural range 0 to 9;

		variable my_line : line;

	begin
		if (R = '1') then 
			tx_slowo <= (others => '0'); 
			tx_nadaj <= '0'; 
			rozkaz <= WCZYTAJ; 
			operacja <= INICJUJ;
			wynik <= 0;
			obecny <= 0;
			odbieranie <= '1';
			wyn_mnozenia <= 0;

		elsif (rising_edge(C)) then 

			tx_nadaj <= '0'; 

			if (odbieranie = '1') then
				if (rx_gotowe = '1') then 
					case rozkaz is 
					
						when WCZYTAJ => 
							if (rx_slowo = kod_znaku('+') or rx_slowo = kod_znaku('-') or rx_slowo = kod_znaku('=')) then
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
							if (rx_slowo = kod_znaku('+')) then
								operacja <= DODAJ;
								obecny <= 0;
							elsif (rx_slowo = kod_znaku('-')) then
								operacja <= ODEJMIJ;
								obecny <= 0;
							elsif (rx_slowo = kod_znaku('=')) then
								rozkaz <= ZACZWYSYL;
								obecny <= 0;
								odbieranie <= '0';
							elsif (rx_slowo = kod_znaku('*')) then
								if (operacja /= MNOZENIE) then
									wyn_mnozenia <= obecny;
									przed_mnozeniem <= operacja;
								else
									wyn_mnozenia <= wyn_mnozenia * obecny;
								end if;
								operacja <= MNOZENIE;
								obecny <= 0;
							elsif (wyzn_cyfre(rx_slowo) /= 10) then 
								wczytana := wyzn_cyfre(rx_slowo); 
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
							tx_slowo <= WYZEROWANE_SLOWO + character'pos('-');
							tx_nadaj <= '1';
							wynik <= - wynik;
							rozkaz <= CZEKAJ;
							dlugosc_wyniku <= integer(ceil(log10(real( - wynik))));
						else
							dlugosc_wyniku <= integer(ceil(log10(real(wynik))));
							rozkaz <= WYSYLAJ;
						end if;
						
					when WYSYLAJ => 
						if (dlugosc_wyniku > 0) then
							tx_slowo <= WYZEROWANE_SLOWO + character'pos('0') + wynik/(10 ** (dlugosc_wyniku - 1));
							wynik <= wynik mod (10 ** (dlugosc_wyniku - 1));
							tx_nadaj <= '1';
							rozkaz <= CZEKAJ;
							dlugosc_wyniku <= dlugosc_wyniku - 1;
						else
							rozkaz <= STOJ;
						end if;

					when CZEKAJ => 
						if (tx_nadaj = '0' and tx_wysylanie = '0') then
							rozkaz <= WYSYLAJ;
						end if;

					when others => null;
				end case;
			end if;

			write(my_line, string'("Wyniki:")); 
			writeline(output, my_line); 
			write(my_line, string'(" wynik= "));
			write(my_line, wynik); 
			write(my_line, string'(" wczytana liczba= "));
			write(my_line, obecny);
			write(my_line, string'(" mnozona liczba= "));
			write(my_line, wyn_mnozenia);
			writeline(output, my_line);

		end if;

	end process;
	
end behavioural;