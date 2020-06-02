library ieee;
use     ieee.std_logic_1164.all;
use     ieee.std_logic_unsigned.all;
use     ieee.std_logic_misc.all;

entity SERIAL_RX is
  generic (
    F_ZEGARA		:natural := 20000000;
    L_BODOW		:natural := 9600;
    B_SLOWA		:natural := 8;
    B_STOPOW		:natural := 2
  );
  port (
    R		:in  std_logic;					
    C		:in  std_logic;					
    RX		:in  std_logic;					
    SLOWO	:out std_logic_vector(B_SLOWA-1 downto 0);	
    GOTOWE	:out std_logic;					
    BLAD	:out std_logic					
  );
end SERIAL_RX;

architecture behavioural of SERIAL_RX is

  signal   wejscie	:std_logic_vector(0 to 1);		

  type     ETAP		is (CZEKANIE, START, DANA, STOP); 
  signal   stan		:ETAP;					

  constant T		:positive := F_ZEGARA/L_BODOW-1;	
  signal   l_czasu  	:natural range 0 to T;			
  signal   l_bitow  	:natural range 0 to B_SLOWA-1;		
  
  signal   bufor	:std_logic_vector(SLOWO'range);		
  signal   problem	:std_logic;				

begin

   process (R, C) is						
   begin							

     if (R='1') then						
       wejscie	<= (others => '0');				
       stan	<= CZEKANIE;					
       l_czasu  <= 0;						
       l_bitow  <= 0;						
       bufor	<= (others => '0');				
       problem 	<= '0';						
       SLOWO	<= (others => '0');				
       GOTOWE	<= '0';						
       BLAD	<= '0';						

     elsif (rising_edge(C)) then				

       GOTOWE     <= '0';					
       BLAD       <= '0';					
       wejscie(0) <= RX;					
       wejscie(1) <= wejscie(0);				

       case stan is						

         when CZEKANIE =>					
           l_czasu <= 0;					
           l_bitow <= 0;					
           bufor   <= (others => '0');				
           problem <= '0';					
	   if (wejscie(1)='0' and wejscie(0)='1') then		
	     stan   <= START;					
	   end if;						

         when START =>						
	   if (l_czasu /= T/2) then				
	     l_czasu <= l_czasu + 1;				
	   else							
             l_czasu <= 0;					
	     stan    <= DANA;					
	     if (wejscie(1) = '0') then				
               problem <= '1';					
	     end if;						
	   end if;						

         when DANA =>						
	   if (l_czasu /= T) then				
	     l_czasu <= l_czasu + 1;				
	   else							
	     bufor(bufor'left) <= wejscie(1);			
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
	       if (wejscie(1) = '1') then			
                 problem <= '1';				
	       end if; 						
	     else						
	       if (problem = '0' and wejscie(1) = '0') then	
                 SLOWO <= bufor;				
                 GOTOWE <= '1';					
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
