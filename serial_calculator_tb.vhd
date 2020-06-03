library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_misc.all;

entity SERIAL_CALCULATOR_TB is
	generic (
		CZESTOTLIWOSC_ZEGARA : natural := 20_000_000; 
		ILOSC_BODOW : natural := 5_000_000; 
		WIELKOSC_SLOWA : natural := 8; 
		LICZBA_BITOW_STOPU : natural := 2 
	);
end SERIAL_CALCULATOR_TB;

architecture behavioural of SERIAL_CALCULATOR_TB is

	signal R : std_logic;
	signal C : std_logic;
	signal RX, RXO : std_logic;
	signal TX, TXI : std_logic;
 
	constant O_ZEGARA : time := 1 sec/CZESTOTLIWOSC_ZEGARA;
	constant O_BITU : time := 1 sec/ILOSC_BODOW;

	constant ROZKAZ : string := "-60+5*4*10+80*20=";
	signal WYNIK : string(15 downto 1);

begin
	process is
	begin
		C <= '1';
		wait for O_ZEGARA/2;
		C <= '0';
		wait for O_ZEGARA/2;
	end process;
 
	process is
	variable SLOWO : std_logic_vector(WIELKOSC_SLOWA - 1 downto 0);
		begin
			R <= '1';
			RX <= '0';
			SLOWO := (others => '0');
			wait for 10 ns;
			R <= '0';
			for i in 1 to rozkaz'LENGTH loop
				wait for 10 * O_BITU;
				SLOWO := CONV_STD_LOGIC_VECTOR(character'pos(rozkaz(i)), SLOWO'length);
				RX <= '1';
				wait for O_BITU;
				for i in 0 to WIELKOSC_SLOWA - 1 loop
					RX <= SLOWO(i);
					wait for O_BITU;
				end loop;
				for i in 0 to LICZBA_BITOW_STOPU - 1 loop
					RX <= '0';
					wait for O_BITU;
				end loop;
			end loop;
			wait;
		end process;
 
		RXO <= RX;
		serial_calculator_inst : entity work.SERIAL_CALCULATOR(behavioural)
				generic map(
				CZESTOTLIWOSC_ZEGARA => CZESTOTLIWOSC_ZEGARA, 
				ILOSC_BODOW => ILOSC_BODOW, 
				WIELKOSC_SLOWA => WIELKOSC_SLOWA, 
				LICZBA_BITOW_STOPU => LICZBA_BITOW_STOPU
				) 
				port map(
					R => R, 
					C => C, 
					RX => RXO, 
					TX => TXI
				);

					TX <= TXI;
					process is
					variable SLOWO : std_logic_vector(WIELKOSC_SLOWA - 1 downto 0);
					variable blad : boolean;
			begin
				SLOWO := (others => '0');
				WYNIK <= (others => ' ');
				loop
				blad := FALSE;
				wait until TX = '1';
				wait for O_BITU/2;
				if (TX /= '1') then
					blad := TRUE;
				end if;
				wait for O_BITU;
				for i in 0 to WIELKOSC_SLOWA - 1 loop
					SLOWO(SLOWO'LEFT - 1 downto 0) := SLOWO(SLOWO'LEFT downto 1);
					SLOWO(SLOWO'left) := TX;
					wait for O_BITU;
				end loop;
				for i in 0 to LICZBA_BITOW_STOPU - 1 loop
					if (TX /= '0') then
						blad := TRUE;
					end if;
				end loop;
				WYNIK(WYNIK'LEFT downto 2) <= WYNIK(WYNIK'LEFT - 1 downto 1);
				WYNIK(1) <= character'val(CONV_INTEGER(SLOWO));
			end loop;
			end process;
 

end behavioural;