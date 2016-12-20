-- cpu.vhd: Simple 8-bit CPU (BrainLove interpreter)
-- Copyright (C) 2016 Brno University of Technology,
--                    Faculty of Information Technology
-- Author(s): DOPLNIT
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

-- ----------------------------------------------------------------------------
--                        Entity declaration
-- ----------------------------------------------------------------------------
entity cpu is
 port (
   CLK   : in std_logic;  -- hodinovy signal
   RESET : in std_logic;  -- asynchronni reset procesoru
   EN    : in std_logic;  -- povoleni cinnosti procesoru
 
   -- synchronni pamet ROM
   CODE_ADDR : out std_logic_vector(11 downto 0); -- adresa do pameti
   CODE_DATA : in std_logic_vector(7 downto 0);   -- CODE_DATA <- rom[CODE_ADDR] pokud CODE_EN='1'
   CODE_EN   : out std_logic;                     -- povoleni cinnosti
   
   -- synchronni pamet RAM
   DATA_ADDR  : out std_logic_vector(9 downto 0); -- adresa do pameti
   DATA_WDATA : out std_logic_vector(7 downto 0); -- mem[DATA_ADDR] <- DATA_WDATA pokud DATA_EN='1'
   DATA_RDATA : in std_logic_vector(7 downto 0);  -- DATA_RDATA <- ram[DATA_ADDR] pokud DATA_EN='1'
   DATA_RDWR  : out std_logic;                    -- cteni (1) / zapis (0)
   DATA_EN    : out std_logic;                    -- povoleni cinnosti
   
   -- vstupni port
   IN_DATA   : in std_logic_vector(7 downto 0);   -- IN_DATA <- stav klavesnice pokud IN_VLD='1' a IN_REQ='1'
   IN_VLD    : in std_logic;                      -- data platna
   IN_REQ    : out std_logic;                     -- pozadavek na vstup data
   
   -- vystupni port
   OUT_DATA : out  std_logic_vector(7 downto 0);  -- zapisovana data
   OUT_BUSY : in std_logic;                       -- LCD je zaneprazdnen (1), nelze zapisovat
   OUT_WE   : out std_logic                       -- LCD <- OUT_DATA pokud OUT_WE='1' a OUT_BUSY='0'
 );
end cpu;


-- ----------------------------------------------------------------------------
--                      Architecture declaration
-- ----------------------------------------------------------------------------
architecture behavioral of cpu is

	signal PC_reg     : std_logic_vector(11 downto 0);
	signal PC_inc     : std_logic;
	signal PC_dec     : std_logic;
	
	signal PTR_reg    : std_logic_vector(9 downto 0);
	signal PTR_inc    : std_logic;
	signal PTR_dec    : std_logic;
	
  signal TMP_reg    : std_logic_vector(7 downto 0);
	signal TMP_LD     : std_logic;
  signal TMP_allow  : std_logic;	
  
	signal CNT_reg    : std_logic_vector(11 downto 0);
	signal CNT_inc    : std_logic;
	signal CNT_dec    : std_logic;
  signal CNT_beggin : std_logic;
	
	type IID is (i_ptr_inc, i_ptr_dec, i_mem_inc, i_mem_dec, i_loop_s, i_loop_e, i_print, i_read, i_LD, i_ST, i_HALT, i_nop);
	
  signal ireg_code  : std_logic_vector(7 downto 0);       -- instruction taken from memory
	signal ireg       : IID;                                -- decoded instruction
	signal ireg_ld    : std_logic := '0';                   -- signal to actualise ireg
	
	type FSM_state is (
    start,
    fetch, fetch2, translate_instruction,
    s_ptr_inc, s_ptr_dec, actualise,
    s_mem_inc, s_mem_inc2,
		s_mem_dec, s_mem_dec2,
    s_loop_s, s_loop_return, 
		s_loop_if, s_loop_return_if,
		s_loop_skip, s_loop_skip_step, s_loop_skip_step2,
		s_loop_return1, s_loop_return2, 
		s_loop_return_step, s_loop_return_step2,
		
    s_print, s_print2, s_read, s_read2, 
    s_HALT, s_nop,
    s_LD, s_ST
  );
	
  signal current_state : FSM_state;
	signal next_state    : FSM_state;	

begin

-- ----------------------------------------------------------------------------
--                        register of program counter
-- ----------------------------------------------------------------------------
  
  program_counter: process(CLK, RESET)
	begin
    if (RESET = '1') then                                    -- reset
			PC_reg <= (others => '0');	                           -- or just 000000000000
		elsif (CLK'event) and (CLK = '1') then
			if (PC_inc = '1') and (PC_dec = '0') then              -- increment PC
				PC_reg <= PC_reg + 1;
			elsif (PC_dec = '1') and (PC_inc = '0') then           -- decrement pc
				PC_reg <= PC_reg - 1;
			end if;
		end if;	
	end process; 

-- ----------------------------------------------------------------------------
--                              ROM adressing
-- ----------------------------------------------------------------------------

  CODE_ADDR <= PC_reg;
	
-- ----------------------------------------------------------------------------
--                     register of instruction register
-- ----------------------------------------------------------------------------

	instruction_register: process(RESET, CLK)
	begin
		if (RESET = '1') then
		 ireg_code <= (others => '0');		
		elsif (CLK'event) and (CLK = '1') and (RESET = '0') then
			if (ireg_ld = '1') then    -- load instruction when you are told to
				ireg_code <= CODE_DATA;
			end if;
		end if;
	end process;
  
-- ----------------------------------------------------------------------------
--                          instruction decoder
-- ----------------------------------------------------------------------------
  instruction_decoder: process(ireg_code)
    begin
      case ireg_code is
        when X"3E" => ireg <= i_ptr_inc;    --">"
        when X"3C" => ireg <= i_ptr_dec;    --"<"
        when X"2B" => ireg <= i_mem_inc;    --"+"
        when X"2D" => ireg <= i_mem_dec;    --"-"
        when X"5B" => ireg <= i_loop_s;     --"["
        when X"5D" => ireg <= i_loop_e;     --"]"
        when X"2E" => ireg <= i_print;      --"."
        when X"2C" => ireg <= i_read;       --","
        when X"24" => ireg <= i_LD;         --"$"
        when X"21" => ireg <= i_ST;         --"!"
        when X"00" => ireg <= i_HALT;       --"EOF"
        when others => ireg <= i_nop; 
      end case;
	end process;  
  
-- ----------------------------------------------------------------------------
--                             loop controll
-- ----------------------------------------------------------------------------
  
  CNT: process(CLK, RESET)
	begin
	  if (RESET = '1') then
		  CNT_reg <= (others=>'0');
    elsif (RESET = '0') and (CLK'event) and (CLK = '1') then
	    if (CNT_beggin = '0') then
		    if  (CNT_inc = '1') and (CNT_dec = '0') then   -- increment cnt
					CNT_reg <= CNT_reg + 1;
				elsif (CNT_dec = '1') and (CNT_inc = '0') then -- decrement cnt
	        CNT_reg <= CNT_reg - 1;
	      end if;
		  else CNT_reg <= "000000000001";                  -- set cnt to 1
		  end if;
		end if;	
	end process;
  
-- ----------------------------------------------------------------------------
--                                 RAM pointer
-- ----------------------------------------------------------------------------
	
	RAM_pointer: process(CLK, RESET)
	begin
		if (RESET = '1') then
			PTR_reg <= (others => '0');		
		elsif (RESET = '0') and (CLK'event) and (CLK = '1') then
      if (PTR_inc = '1') and (PTR_dec = '0') then     -- increment pointer to memory
				PTR_reg <= PTR_reg + 1;
			elsif (PTR_dec = '1') and (PTR_inc = '0') then  -- decremnt pointer to memory
				PTR_reg <= PTR_reg - 1;
			end if;
		end if;	
	end process;
	
-- ----------------------------------------------------------------------------
--                                 Register TMP
-- ----------------------------------------------------------------------------
	
	TMP_register: process(CLK, RESET)
	begin
		if (RESET = '1') then
			TMP_reg <= (others => '0');		
		elsif (RESET = '0') and (CLK'event) and (CLK = '1') then
      if (TMP_LD = '1') then        -- store data from memory
				TMP_reg <= DATA_RDATA;
			end if;
		end if;	
	end process;
  
-- ----------------------------------------------------------------------------
--                        point the pointer into memory
-- ----------------------------------------------------------------------------

  DATA_ADDR <= PTR_reg;

-- ----------------------------------------------------------------------------
--                           current state actualise
-- ----------------------------------------------------------------------------
  fsm_pstate: process(EN, RESET, CLK)
	begin
	  if (RESET='1') then
		 current_state <= start;
		elsif (CLK'event) and (CLK='1') then
      if(EN = '1') then
        current_state <= next_state; 
      end if;
	  end if;
	end process;

  
-- ----------------------------------------------------------------------------
--                             FSM next_state logic
-- ----------------------------------------------------------------------------
	fsm_current_state_logic: process(current_state, ireg)
	VARIABLE c : bit_vector(7 downto 0);
	
	begin
    -- default - reset
    next_state <= start;

    DATA_EN   <= '0';
	  CODE_EN   <= '0';
	  PTR_dec   <= '0';
	  PTR_inc   <= '0';
	  ireg_ld   <= '0';
	  PC_dec    <= '0';
		PC_inc    <= '0';
	  OUT_WE    <= '0';
	  CNT_inc   <= '0';
	  CNT_dec   <= '0';
		TMP_allow <= '0';
    TMP_LD    <= '0';
		CNT_beggin<= '0';
		
		case current_state is	
			when start =>
				next_state <= fetch;

			 -- Loading instruction
			when fetch =>
				CODE_EN <= '1';
				next_state <= fetch2;
				
			when fetch2 =>
        ireg_ld <= '1';	
				next_state <= translate_instruction;
				
			 -- decode instruction here
			when translate_instruction =>
				case ireg is
					when i_HALT =>
						next_state <= s_HALT;

					when i_ptr_inc =>
						next_state <= s_ptr_inc;

					when i_ptr_dec =>
						next_state <= s_ptr_dec;
						
					when i_mem_inc =>
						next_state <= s_mem_inc;
						
					when i_mem_dec =>
						next_state <= s_mem_dec;
						
					when i_loop_s =>
						next_state <= s_loop_s;
						
					when i_loop_e =>
						next_state <= s_loop_return;
						
					when i_print =>
						next_state <= s_print;

					when i_read =>
						next_state <= s_read;
						
				  when i_LD =>
            next_state <= s_LD;
          
					when i_ST => 
            next_state <= s_ST;
					
					when i_nop =>
						next_state <= s_nop;
				end case;	
			
			-- HALT
			when s_HALT =>
				next_state <= s_HALT;
				
			-- nop
			when s_nop =>
				PC_dec <= '0';
				PC_inc <= '1';
				
				next_state <= fetch;
				
			-- INCREASE PTR
			when s_ptr_inc =>
				PTR_inc <= '1';
				PTR_dec <= '0';
				
				PC_dec <= '0';
				PC_inc <= '1';
				next_state <= actualise;

				
			-- DECREASE PTR
			when s_ptr_dec =>
				PTR_inc <= '0';
				PTR_dec <= '1';
        
				PC_dec <= '0';
				PC_inc <= '1';
				next_state <= actualise;
      
			when actualise =>
			  DATA_EN <= '1';
				DATA_RDWR <= '1';
				next_state <= fetch;

		  -- INCREASE VAL

			when s_mem_inc =>
			  DATA_EN <= '1';
				DATA_RDWR <= '1';
				
				next_state <= s_mem_inc2;
			
			when s_mem_inc2 =>
				DATA_EN <= '1';
				DATA_RDWR <= '0';
        DATA_WDATA <= DATA_RDATA + 1;
				PC_dec <= '0';
				PC_inc <= '1';
				next_state <= fetch;
				
			-- INCREASE VAL
			when s_mem_dec =>
				DATA_EN <= '1';
				DATA_RDWR <= '1';  --read
				
				next_state <= s_mem_dec2;
			
			when s_mem_dec2 =>
				DATA_EN <= '1';
				DATA_RDWR <= '0';  -- write
				DATA_WDATA <= DATA_RDATA - 1; 
				PC_dec <= '0';
				PC_inc <= '1'; 
		    next_state <= fetch;
				
			-- PRINT			
			when s_print =>
				DATA_EN <= '1';
				DATA_RDWR <= '1';
				
				next_state <= s_print2;
			
			when s_print2 => 
				if (OUT_BUSY = '0') then
					OUT_WE <= '1';

					OUT_DATA <= DATA_RDATA ;
					
					PC_dec <= '0';
					PC_inc <= '1';
					next_state <= fetch;	
				else next_state <= s_print2;       -- wait
				end if;	
        next_state <= fetch;
			-- READ
			when s_read =>
				IN_REQ <= '1';			
				next_state <= s_read2;
				
			when s_read2 =>
				if (IN_VLD = '1') then
					DATA_EN <= '1';	
					DATA_RDWR <= '0';
					DATA_WDATA <= IN_DATA;
					
					IN_REQ <= '0';
					
					PC_dec <= '0';
					PC_inc <= '1';
					next_state <= fetch;	
				else next_state <= s_read2;
				end if;				
			-- LD
		  when s_LD =>
				DATA_EN <= '1';
				DATA_RDWR <= '1';
				TMP_LD <= '1';
				PC_dec <= '0';
				PC_inc <= '1';
				next_state <= fetch;
			when s_ST =>
				DATA_EN <= '1';
				DATA_RDWR <= '0';
				DATA_WDATA <= TMP_reg;
				PC_dec <= '0';
				PC_inc <= '1';
			  next_state <= fetch;
		  -------------------------------
			when s_loop_s =>
				PC_dec <= '0';
				PC_inc <= '1';
				
				DATA_EN <= '1';
				DATA_RDWR <= '1';
				
				next_state <= s_loop_if;			
				
			when s_loop_if =>
				if (DATA_RDATA = "00000000") then
					CNT_beggin <= '1';
					next_state <= s_loop_skip_step;
				else
					next_state <= fetch;
				end if;

			when s_loop_skip_step =>
				
				next_state <= s_loop_skip_step2; 
				CODE_EN <= '1';
				
			when s_loop_skip_step2 =>
				next_state <= s_loop_skip;
				ireg_ld <= '1';
			
			when s_loop_skip =>  
				if (CNT_reg /= "000000000000") then
					if (ireg_code = "000001011011") then     -- [
						CNT_inc <= '1';
						CNT_dec <= '0';
					elsif (ireg_code = "000001011101") then  -- ]
						CNT_inc <= '0';
						CNT_dec <= '1';
					end if;
					
					next_state <= s_loop_skip_step;
				else
					next_state <= fetch;
				end if;
				
				PC_dec <= '0';
				PC_inc <= '1';
			
			
			--END WHILE			
			when s_loop_return =>				
				DATA_EN <= '1';	
				DATA_RDWR <= '1';
				
				next_state <= s_loop_return_if;
			when s_loop_return_if =>	  
				if (DATA_RDATA = "00000000") then					
					PC_dec <= '0';
					PC_inc <= '1';
					
					next_state <= fetch;			
				else
					CNT_beggin <= '1';
					PC_dec <= '1';
					PC_inc <= '0';
				
					next_state <= s_loop_return_step;						
				end if;				
				
			when s_loop_return_step =>
			
				next_state <= s_loop_return_step2;
				 
				CODE_EN <= '1';
				
			when s_loop_return_step2 =>	  
				next_state <= s_loop_return1;
				ireg_ld <= '1';
			
			when s_loop_return1 =>
				if (CNT_reg /= "000000000000") then
					if (ireg_code = "000001011011") then     -- [
						CNT_inc <= '0';
						CNT_dec <= '1';
					elsif (ireg_code = "000001011101") then  -- ]
						CNT_inc <= '1';
						CNT_dec <= '0';
					end if;
					next_state <= s_loop_return2;
				else
					next_state <= fetch;
				end if;
				
			when s_loop_return2 =>
				if (CNT_reg = "000000000000") then
					PC_dec <= '0';
					PC_inc <= '1';
				else
					PC_dec <= '1';
					PC_inc <= '0';
				end if;
				
				next_state <= s_loop_return_step;			
			
			when others => 
				next_state <= fetch;
		end case;
	end process;
  
end behavioral;
 
