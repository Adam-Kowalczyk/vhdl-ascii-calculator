library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.std_logic_misc.all;

entity UART_RX is
	generic (
		CZESTOTLIWOSC_ZEGARA : natural := 20000000;
		ILOSC_BODOW : natural := 9600;
		WIELKOSC_SLOWA : natural := 8;
		LICZBA_BITOW_STOPU : natural := 2
	);
	port (
		R : in std_logic; 
		C : in std_logic; 
		RX : in std_logic; 
		SLOWO : out std_logic_vector(WIELKOSC_SLOWA - 1 downto 0); 
		ODCZYTANE : out std_logic; 
		BLAD : out std_logic 
	);
end UART_RX;

architecture behavioural of UART_RX is

	signal wejscie : std_logic_vector(0 to 1); 

	type ETAP is (START, CZYTANIE, CZEKANIE, STOP);
	signal stan : ETAP; 

	constant T : positive := CZESTOTLIWOSC_ZEGARA/ILOSC_BODOW - 1; 
	signal l_czasu : natural range 0 to T; 
	signal l_bitow : natural range 0 to WIELKOSC_SLOWA - 1; 
 
	signal bufor : std_logic_vector(SLOWO'range); 
	signal problem : std_logic; 

begin
	process (R, C) is 
	begin
		if (R = '1') then 
			wejscie <= (others => '0'); 
			stan <= CZEKANIE; 
			l_czasu <= 0; 
			l_bitow <= 0; 
			bufor <= (others => '0'); 
			problem <= '0'; 
			SLOWO <= (others => '0'); 
			ODCZYTANE <= '0'; 
			BLAD <= '0'; 

		elsif (rising_edge(C)) then 

			ODCZYTANE <= '0'; 
			BLAD <= '0'; 
			wejscie(0) <= RX; 
			wejscie(1) <= wejscie(0); 

			case stan is 

				when START => 
					if (l_czasu /= T/2) then 
						l_czasu <= l_czasu + 1; 
					else 
						l_czasu <= 0; 
						stan <= CZYTANIE; 
						if (wejscie(1) = '0') then 
							problem <= '1'; 
						end if; 
					end if; 

				when CZYTANIE => 
					if (l_czasu /= T) then 
						l_czasu <= l_czasu + 1; 
					else 
						bufor(bufor'left) <= wejscie(1); 
						bufor(bufor'LEFT - 1 downto 0) <= bufor(bufor'LEFT downto 1);
						l_czasu <= 0; 
 
						if (l_bitow /= WIELKOSC_SLOWA - 1) then 
							l_bitow <= l_bitow + 1; 
						else 
							l_bitow <= 0; 
							stan <= STOP; 
						end if; 
					end if; 

				when CZEKANIE => 
					l_czasu <= 0; 
					l_bitow <= 0; 
					bufor <= (others => '0'); 
					problem <= '0'; 
					if (wejscie(1) = '0' and wejscie(0) = '1') then 
						stan <= START; 
					end if; 

				when STOP => 
					if (l_czasu /= T) then 
						l_czasu <= l_czasu + 1; 
					else 
						l_czasu <= 0; 

						if (l_bitow /= LICZBA_BITOW_STOPU - 1) then 
							l_bitow <= l_bitow + 1; 
							if (wejscie(1) = '1') then 
								problem <= '1'; 
							end if; 
						else 
							if (problem = '0' and wejscie(1) = '0') then 
								SLOWO <= bufor; 
								ODCZYTANE <= '1'; 
							else 
								SLOWO <= (others => '0'); 
								BLAD <= '1'; 
							end if; 
							stan <= CZEKANIE; 
						end if; 

					end if; 

			end case; 

		end if; 

	end process; 
 
end behavioural;