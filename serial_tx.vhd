library ieee;
use     ieee.std_logic_1164.all;
use     ieee.std_logic_unsigned.all;
use     ieee.std_logic_misc.all;

entity UART_TX is
  generic (
    F_ZEGARA		:natural := 20000000;			
    L_BODOW		:natural := 9600;			
    B_SLOWA		:natural := 8;				
    B_STOPOW		:natural := 2			
  );
  port (
    R		:in  std_logic;					
    C		:in  std_logic;					
    TX		:out std_logic;					
    SLOWO	:in  std_logic_vector(B_SLOWA-1 downto 0);	
    NADAJ	:in  std_logic;					
    WYSYLANIE	:out std_logic					
  );
end UART_TX;

architecture behavioural of UART_TX is

  signal   bufor	:std_logic_vector(SLOWO'range);		

  type     ETAP		is (CZEKANIE, START, DANA, STOP); 
  signal   stan		:ETAP;					

  signal   nadawaj	:std_logic;				

  constant T		:positive := F_ZEGARA/L_BODOW-1;	
  signal   l_czasu  	:natural range 0 to T;			
  signal   l_bitow  	:natural range 0 to B_SLOWA-1;		
  

begin

   process (R, C) is						
   begin

     if (R='1') then						
       bufor <= (others => '0');				
       stan	<= CZEKANIE;				
       l_czasu <= 0;					
       l_bitow <= 0;					
       nadawaj	<= '0';					
       WYSYLANIE <= '0';					

     elsif (rising_edge(C)) then				

       nadawaj   <= '0';					
       WYSYLANIE <= '1';					

       case stan is						

         when CZEKANIE =>					
				  l_czasu <= 0;					
				  l_bitow <= 0;					
				if (NADAJ='1') then					
					stan <= START;				
					bufor <= SLOWO;				
				else							
					WYSYLANIE <= '0';					
				end if;						

         when START =>						
				nadawaj <= '1';					
				if (l_czasu /= T) then				
					l_czasu <= l_czasu + 1;				
				else							
					l_czasu <= 0;					
					stan <= DANA;					
				end if;						

         when DANA =>						
				nadawaj <= bufor(0);					
				if (l_czasu /= T) then				
				  l_czasu <= l_czasu + 1;				
				else							
				  bufor(bufor'left) <= '0';				
				  bufor(bufor'left-1 downto 0) <= bufor(bufor'left downto 1); 
					l_czasu <= 0;									  
				  if (l_bitow /= B_SLOWA-1) then			
					 l_bitow <= l_bitow + 1;				
				  else						
					 l_bitow <= 0;					
						stan <= STOP;					
					end if; 						
				end if;						       

         when STOP =>						
				if (l_czasu /= T) then				
				  l_czasu <= l_czasu + 1;				
				else							
					l_czasu <= 0;					

				  if (l_bitow /= B_STOPOW-1) then			
					 l_bitow <= l_bitow + 1;				
				  else						
					WYSYLANIE <= '0';				
					stan <= CZEKANIE;				
				  end if;						
				end if;						
			end case;						
		end if;							
   end process;							

   TX <= nadawaj;		
   
end behavioural;