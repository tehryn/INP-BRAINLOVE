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

type fsm_states is (
  state_idle,
  state_fetch,
  state_decode,
  state_nop,
  state_reg_inc,                                        -- register increment
  state_reg_dec,                                        -- register decrement
  state_ptr_inc_load, state_ptr_inc_do,                 -- memory increment
  state_ptr_dec_load, state_ptr_dec_do,                 -- memory decrement
  state_ptr_while_start_load, state_ptr_while_start_if,
  state_ptr_while_start_skip, state_ptr_while_start_check,
  state_ptr_while_start_repeat,
  state_ptr_while_end_load, state_ptr_while_end_if,
  state_ptr_while_end_skip, state_ptr_while_end_check,
  state_ptr_while_end_repeat,
  state_ptr_print_load, state_ptr_print_do,             -- print memory value
  state_ptr_read_load, state_ptr_read_do,               -- read memory value
  state_ptr_store_load, state_ptr_store_do,             -- store memory temporary value
  state_ptr_load,                                       -- load memory temporary value
  state_halt                                            -- end
);
signal next_state : fsm_states;
signal current_state : fsm_states;

type instruction is (
  nop,                                            -- no operation
  reg_inc,                                        -- register increment
  reg_dec,                                        -- register decrement
  ptr_inc,                                        -- memory increment
  ptr_dec,                                        -- memory decrement
  ptr_while_start,                                -- while start 
  ptr_while_end,                                  -- while end
  ptr_print,                                      -- print memory value
  ptr_read,                                       -- read memory value
  ptr_store,                                      -- store memory temporary value
  ptr_load,                                       -- load memory temporary value
  halt                                            -- end
);
signal current: instruction;                      --current instruction

signal reg_pc_cnt : std_logic_vector(11 downto 0);    -- program counter register 
signal reg_pc_inc : std_logic;                      	-- program counter increment
signal reg_pc_dec : std_logic;                        -- program counter decrement

signal reg_ptr_cnt : std_logic_vector(9 downto 0);     -- memory register counter 
signal reg_ptr_inc : std_logic;                      	 -- memory register increment
signal reg_ptr_dec : std_logic;                        -- memory register decrement 

signal reg_cnt_cnt : std_logic_vector(9 downto 0);
signal reg_cnt_inc : std_logic;                      	 -- count register increment
signal reg_cnt_dec : std_logic;                        -- count register decrement 


signal reg_tmp : std_logic_vector(7 downto 0);

signal mem_write_sel : std_logic_vector(1 downto 0);

begin

reg_pc : process( RESET, CLK )
begin
  if(RESET = '1') then -- async reset
    reg_pc_cnt <= (others => '0');
 elsif (CLK'event) and (CLK='1') then
    if(reg_pc_inc = '1') then
      reg_pc_cnt <= reg_pc_cnt + 1;
    elsif(reg_pc_dec = '1') then
      reg_pc_cnt <= reg_pc_cnt - 1;
    end if;
  end if;
end process ; -- pc_cntr

reg_ptr : process( RESET, CLK )
begin
  if(RESET = '1') then -- async reset
    reg_ptr_cnt <= (others => '0');
 elsif (CLK'event) and (CLK='1') then
    if(reg_ptr_inc = '1') then
      reg_ptr_cnt <= reg_ptr_cnt + 1;
    elsif(reg_ptr_dec = '1') then
      reg_ptr_cnt <= reg_ptr_cnt - 1;
    end if;
  end if;
end process ; -- pc_cntr

reg_cnt : process ( RESET, CLK )
begin
	if(RESET = '1') then
		reg_cnt_cnt <= (others => '0');
	elsif (CLK'event) and (CLK='1') then
		if(reg_cnt_inc = '1') then
			reg_cnt_cnt <= reg_cnt_cnt + 1;
		elsif (reg_cnt_dec = '1') then
			reg_cnt_cnt <= reg_cnt_cnt - 1;
		end if;
	end if;
end process;


decode_instr : process( CODE_DATA )
begin
  case(CODE_DATA) is
    when X"3E" => current <= reg_inc; -- >
    when X"3C" => current <= reg_dec; -- <
    when X"2B" => current <= ptr_inc; -- +
    when X"2D" => current <= ptr_dec; -- -
    when X"5B" => current <= ptr_while_start; -- [
    when X"5D" => current <= ptr_while_end; -- ]
    when X"2E" => current <= ptr_print; -- .
    when X"2C" => current <= ptr_read; -- ,
    when X"24" => current <= ptr_store; -- $
    when X"21" => current <= ptr_load; -- !
	 when X"00" => current <= halt; --null
    when others => current <= nop;
  end case;
end process ; -- decode_instr

memory_multiplexor: process(CLK, mem_write_sel, DATA_RDATA, IN_DATA)
begin
	case (mem_write_sel) is
		when "00" =>
			DATA_WDATA <= IN_DATA;
		when "01" =>
			DATA_WDATA <= DATA_RDATA + 1;
		when "10" =>
			DATA_WDATA <= DATA_RDATA - 1;
		when "11" =>
			DATA_WDATA <= reg_tmp;
		when others =>
		end case;
end process;

fsm_current_state_process: process(RESET, CLK)
begin
	if (RESET = '1') then
		current_state <= state_idle;
	elsif (CLK'event) and (CLK = '1') then
		if (EN = '1') then
			current_state <= next_state;
		end if;
	end if;
end process;



fsm_next_state_process : process(CODE_DATA, IN_VLD, OUT_BUSY, current, current_state)
begin
  -- default values
  DATA_EN <= '0';
  CODE_EN <= '0';
  DATA_ADDR <= reg_ptr_cnt;
  reg_pc_inc  <= '0';
  reg_pc_dec  <= '0';
  reg_ptr_inc <= '0';
  reg_ptr_dec <= '0';
  reg_cnt_inc <= '0';
  reg_cnt_dec <= '0';
  IN_REQ  <= '0';
  OUT_WE  <= '0';

  case( current_state ) is
  
    when state_idle =>
      next_state <= state_fetch;
    when state_fetch =>
      next_state <= state_decode;
      CODE_EN <= '1';
		CODE_ADDR <= reg_pc_cnt;
    when state_decode =>
      case ( current ) is
        when halt =>
          next_state <= state_halt;
        when nop =>
          next_state <= state_nop;
        when reg_inc =>
          next_state <= state_reg_inc;
        when reg_dec =>
          next_state <= state_reg_dec;
        when ptr_inc =>
          next_state <= state_ptr_inc_load;
        when ptr_dec =>
          next_state <= state_ptr_dec_load;
        when ptr_while_start =>
          next_state <= state_ptr_while_start_load;
        when ptr_while_end =>
          next_state <= state_ptr_while_end_load;
        when ptr_print =>
          next_state <= state_ptr_print_load;
        when ptr_read =>
          next_state <= state_ptr_read_load;
        when ptr_store =>
          next_state <= state_ptr_store_load;
        when ptr_load =>
          next_state <= state_ptr_load;
        when others =>
          next_state <= state_nop;
			  end case;
		-- memory pointer incrementation
		when state_reg_inc =>
			reg_ptr_inc <= '1';
			reg_pc_inc <= '1';
			next_state <= state_fetch;
		-- memory pointer decrementation
		when state_reg_dec =>
			reg_ptr_dec <= '1';
			reg_pc_inc <= '1';
			next_state <= state_fetch;
		-- memory value incrementation
		when state_ptr_inc_load =>
			DATA_EN <= '1';
			DATA_RDWR <= '1';
			next_state <= state_ptr_inc_do;
		when state_ptr_inc_do =>
			DATA_EN <= '1';
			DATA_RDWR <= '0';
			mem_write_sel <= "01";
			reg_pc_inc <= '1';
			next_state <= state_fetch;
		-- memory value decrementation
		when state_ptr_dec_load =>
			DATA_EN <= '1';
			DATA_RDWR <= '1';
			next_state <= state_ptr_dec_do;
		when state_ptr_dec_do =>
			DATA_EN <= '1';
			DATA_RDWR <= '0';
			mem_write_sel <= "10";
			reg_pc_inc <= '1';
			next_State <= state_fetch;
		-- memory value print
		when state_ptr_print_load =>
			DATA_EN <= '1';
			DATA_RDWR <= '1';
			next_state <= state_ptr_print_do;
		when state_ptr_print_do =>
			next_state <= state_ptr_print_do;
			if (OUT_BUSY = '0') then
				OUT_WE <= '1';
				OUT_DATA <= DATA_RDATA;
				reg_pc_inc <= '1';
				next_state <= state_fetch;
			end if;
		-- memory value read
		when state_ptr_read_load =>
			IN_REQ <= '1';
			next_state <= state_ptr_read_do;
		when state_ptr_read_do =>
			IN_REQ <= '1';
			next_state <= state_ptr_read_do;
			if(IN_VLD = '1') then
				DATA_EN <= '1';
				DATA_RDWR <= '0';
				mem_write_sel <= "00";
				reg_pc_inc <= '1';
				next_state <= state_fetch;
				end if;
		when state_halt => 
			next_state <= state_halt;
		when state_nop =>
			reg_pc_inc <= '1';
			next_state <= state_fetch;
		-- memory value to temp
		when state_ptr_store_load =>
			DATA_EN <= '1';
			DATA_RDWR <= '1';
			next_state <= state_ptr_store_do;
		when state_ptr_store_do =>
			reg_tmp <= DATA_RDATA;
			reg_pc_inc <= '1';
			next_state <= state_fetch;
		-- tmp to memory value
		when state_ptr_load => 
			DATA_EN <= '1';
			DATA_RDWR <= '0';
			mem_write_sel <= "11";
			reg_pc_inc <= '1';
			next_state <= state_fetch;
		-- while start
		when state_ptr_while_start_load =>
			DATA_EN <= '1';
			DATA_RDWR <= '1';
			reg_pc_inc <= '1';
			next_state <= state_ptr_while_start_if;
		when state_ptr_while_start_if =>
			next_state <= state_fetch;
			if(DATA_RDATA = "000000") then
				reg_cnt_inc <= '1';
				next_state <= state_ptr_while_start_skip;
			end if;
		when state_ptr_while_start_skip =>
			next_state <= state_ptr_while_start_check;
			CODE_EN <= '1';
			CODE_ADDR <= reg_pc_cnt;
		when state_ptr_while_start_check =>
			next_state <= state_ptr_while_start_repeat;
			reg_pc_inc <= '1';
			if( current = ptr_while_start ) then
				reg_cnt_inc <= '1';
			elsif (current = ptr_while_end) then
				reg_cnt_dec <= '1';
			end if;
		when state_ptr_while_start_repeat => 
			next_state <= state_ptr_while_start_skip;
			if(reg_cnt_cnt = "0000000000") then
				next_state <= state_fetch;
			end if;
		-- while end
		when state_ptr_while_end_load =>
			DATA_EN <= '1';
			DATA_RDWR <= '1';
			next_state <= state_ptr_while_end_if;
		when state_ptr_while_end_if =>
			next_state <= state_fetch;
			if(DATA_RDATA = "000000") then
				next_state <= state_fetch;
				reg_pc_inc <= '1';
			else
				next_state <= state_ptr_while_end_skip;
				reg_cnt_inc <= '1';
				reg_pc_dec <= '1';
			end if;
		when state_ptr_while_end_skip =>
			next_state <= state_ptr_while_end_check;
			CODE_EN <= '1';
			CODE_ADDR <= reg_pc_cnt;
		when state_ptr_while_end_check =>
			next_state <= state_ptr_while_end_repeat;
			if( current = ptr_while_start ) then
				reg_cnt_dec <= '1';
			elsif (current = ptr_while_end) then
				reg_cnt_inc <= '1';
			end if;
		when state_ptr_while_end_repeat => 
			next_state <= state_fetch;
			if(reg_cnt_cnt = "0000000000") then
				reg_pc_inc <= '1';
			else 
				reg_pc_dec <= '1';
				next_state <= state_ptr_while_end_skip;
			end if;
		when others =>
			next_state <= state_nop;
  end case ;

end process ; -- fsm_next_state_process

end behavioral;
