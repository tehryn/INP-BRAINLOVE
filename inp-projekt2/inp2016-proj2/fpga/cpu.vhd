-- cpu.vhd: Simple 8-bit CPU (BrainLove interpreter)
-- Copyright (C) 2016 Brno University of Technology,
--                    Faculty of Information Technology
-- Author(s): Matejka Jiri (xmatej52)
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
  type instructions is (
    ptr_inc,   -- >
    ptr_dec,   -- <
    val_inc,   -- +
    val_dec,   -- -
    loop_beg,  -- [
    loop_end,  -- ]
    putc,      -- .
    getc,      -- ,
    val2tmp,   -- $
    tmp2val,   -- !
    eof,       -- null
    empty      -- do
  );
  type states is (
    state_idle, state_run,
    state_decode,
    state_ptr_inc, state_ptr_dec,
--    state_ptr_inc_proc, state_ptr_dec_proc,
    state_val_inc, state_val_dec,
    state_val_inc_proc, state_val_dec_proc,
    state_putc, state_getc,
    state_putc_proc, state_getc_proc,
    state_loop_beg, state_loop_beg_cond, state_loop_beg_jump, state_loop_beg_cnt, state_loop_beg_proc,
    state_loop_end, state_loop_end_cond, state_loop_end_jump, state_loop_end_cnt, state_loop_end_proc,
    state_val2tmp, state_tmp2val,
    state_val2tmp_proc,
    state_eof,
    state_empty
  );
  signal instruction: instructions;                            -- vector that represents loaded instruction
  signal curr_state: states;                                   -- vector that represents current state
  signal next_state: states;                                   -- vector that represents next state

  signal pc_register_addres:  std_logic_vector(11 downto 0);   -- Value of PC register
  signal pc_register_inc:     std_logic;                       -- 1 - increment PC register, 0 do not increment
  signal pc_register_dec:     std_logic;                       -- 1 - decrement PC register, 0 do not decrement

  signal ptr_register_addres: std_logic_vector(9 downto 0);    -- Value of PTR register
  signal ptr_register_inc:    std_logic;                       -- 1 - increment PC register, 0 do not increment
  signal ptr_register_dec:    std_logic;                       -- 1 - decrement PC register, 0 do not decrement

  signal cnt_register_addres: std_logic_vector(7 downto 0);    -- Value of CNT register
  signal cnt_register_inc:    std_logic;                       -- 1 - increment PC register, 0 do not increment
  signal cnt_register_dec:    std_logic;                       -- 1 - decrement PC register, 0 do not decrement

  signal tmp_register_addres: std_logic_vector(7 downto 0);    -- Value of TMP register
  signal tmp_register_id:     std_logic;                       -- 1 load new value into register, 0 do not load

  signal sel: std_logic_vector(1 downto 0);                    -- Select signal in multiplexor
begin

  pc_register: process (CLK, RESET, pc_register_inc, pc_register_dec)
  begin
    if (RESET = '1') then
      pc_register_addres <= "000000000000";
    elsif ((CLK'event) and (CLK = '1')) then
      if (pc_register_inc = '1') then
        pc_register_addres <= pc_register_addres + 1;
      elsif (pc_register_dec = '1') then
        pc_register_addres <= pc_register_addres - 1;
      end if;
    end if;
  end process;

  ptr_register: process (RESET, CLK, ptr_register_inc, ptr_register_dec)
  begin
    if (RESET = '1') then
      ptr_register_addres <= "0000000000";
    elsif ((CLK'event) and (CLK = '1')) then
      if (ptr_register_inc = '1') then
        ptr_register_addres <= ptr_register_addres + 1;
      elsif (ptr_register_dec = '1') then
        ptr_register_addres <= ptr_register_addres - 1;
      end if;
    end if;
  end process;

  cnt_register: process (RESET, CLK, cnt_register_inc, cnt_register_dec)
  begin
    if (RESET = '1') then
      cnt_register_addres <= "00000000";
    elsif ((CLK'event) and (CLK = '1')) then
      if (cnt_register_inc = '1') then
        cnt_register_addres <= cnt_register_addres + 1;
      elsif (cnt_register_dec = '1') then
        cnt_register_addres <= cnt_register_addres - 1;
      end if;
    end if;
  end process;

  tmp_register: process (RESET, CLK, tmp_register_id)
  begin
    if (RESET = '1') then
      tmp_register_addres <= "00000000";
    elsif ((CLK'event) and (CLK = '1') and (tmp_register_id = '1')) then
      tmp_register_addres <= DATA_RDATA;
    end if;
  end process;

  decode: process (CODE_DATA) -- decode instructions
  begin
    case (CODE_DATA) is
      when X"3E"  => instruction <= ptr_inc;
      when X"3C"  => instruction <= ptr_dec;
      when X"2B"  => instruction <= val_inc;
      when X"2D"  => instruction <= val_dec;
      when X"5B"  => instruction <= loop_beg;
      when X"5D"  => instruction <= loop_end;
      when X"2E"  => instruction <= putc;
      when X"2C"  => instruction <= getc;
      when X"24"  => instruction <= val2tmp;
      when X"21"  => instruction <= tmp2val;
      when X"00"  => instruction <= eof;
      when others => instruction <= empty;
    end case;
  end process;

  multiplexor: process (sel, IN_DATA, DATA_RDATA)
  begin
    case sel is
      when "00"   => DATA_WDATA <= IN_DATA;
      when "01"   => DATA_WDATA <= tmp_register_addres;
      when "10"   => DATA_WDATA <= DATA_RDATA + 1;
      when "11"   => DATA_WDATA <= DATA_RDATA - 1;
      when others =>
    end case;
  end process;

  fsm_state: process (RESET, CLK, EN)
  begin
    if (RESET = '1') then
      curr_state <= state_idle;
    elsif ((CLK'event) and (CLK = '1')) then
      if (EN = '1') then
        curr_state <= next_state;
      end if;
    end if;
  end process;

  fsm: process (DATA_RDATA, instruction, IN_VLD, cnt_register_addres, OUT_BUSY, curr_state)
  begin
    DATA_EN          <= '0';
    CODE_EN          <= '0';
    DATA_ADDR        <= ptr_register_addres;
    pc_register_inc  <= '0';
    pc_register_dec  <= '0';
    ptr_register_inc <= '0';
    ptr_register_dec <= '0';
    cnt_register_inc <= '0';
    cnt_register_dec <= '0';
    tmp_register_id  <= '0';
    IN_REQ           <= '0';
    OUT_WE           <= '0';

    case curr_state is
      when state_idle => next_state <= state_run;             -- default state
      when state_run =>                                       -- Retrieve new command from source code
        CODE_EN    <= '1';
        CODE_ADDR  <= pc_register_addres;
        next_state <= state_decode;
      when state_decode =>                                    -- instruction into state
        case instruction is
          when ptr_inc   => next_state <= state_ptr_inc;
          when ptr_dec   => next_state <= state_ptr_dec;
          when val_inc   => next_state <= state_val_inc;
          when val_dec   => next_state <= state_val_dec;
          when loop_beg  => next_state <= state_loop_beg;
          when loop_end  => next_state <= state_loop_end;
          when getc      => next_state <= state_getc;
          when putc      => next_state <= state_putc;
          when tmp2val   => next_state <= state_tmp2val;
          when val2tmp   => next_state <= state_val2tmp;
          when eof       => next_state <= state_eof;
          when others    => next_state <= state_empty;
        end case;
      when state_ptr_inc =>                                   -- increment pointer
        ptr_register_inc <= '1';
        pc_register_inc  <= '1';
        next_state       <= state_run;
      when state_ptr_dec =>                                   -- decrement pointer
        ptr_register_dec <= '1';
        pc_register_inc  <= '1';
        next_state       <= state_run;
      when state_val_inc =>                                   -- asks for data for incrementation
        DATA_RDWR       <= '1';
        DATA_EN         <= '1';
        next_state      <= state_val_inc_proc;
      when state_val_inc_proc =>                              -- process incrementation
        DATA_EN         <= '1';
        DATA_RDWR       <= '0';
        pc_register_inc <= '1';
        sel             <= "10";
        next_state      <= state_run;
      when state_val_dec =>                                   -- asks for data for decrementation
        DATA_EN         <= '1';
        DATA_RDWR       <= '1';
        next_state      <= state_val_dec_proc;
      when state_val_dec_proc =>                              -- process decrementation
        DATA_EN         <= '1';
        DATA_RDWR       <= '0';
        sel             <= "11";
        pc_register_inc <= '1';
        next_state <= state_run;
      when state_loop_beg =>                                 -- prepare data for condition in loop
        DATA_RDWR       <= '1';
        DATA_EN         <= '1';
        pc_register_inc <= '1';
        next_state      <= state_loop_beg_cond;
      when state_loop_beg_cond =>                            -- process condition
        if (DATA_RDATA = "00000000") then
          cnt_register_inc <= '1';
          next_state       <= state_loop_beg_jump;
        else
          next_state       <= state_run;
        end if;
      when state_loop_beg_jump =>                            -- ignore command in loop
        CODE_EN            <= '1';
        CODE_ADDR          <= pc_register_addres;
        next_state         <= state_loop_beg_cnt;
      when state_loop_beg_cnt =>                             -- decect [ and ] and increment or decrement counter
        if (instruction = loop_beg) then
          cnt_register_inc <= '1';
        elsif (instruction = loop_end) then
          cnt_register_dec <= '1';
        end if;
        pc_register_inc    <= '1';
        next_state         <= state_loop_beg_proc;
      when state_loop_beg_proc =>                            -- process loop
        if (cnt_register_addres = "00000000") then
          next_state       <= state_run;
        else
          next_state       <= state_loop_beg_jump;
        end if;
      when state_loop_end =>                                  -- prepare data for condition at the end of loop
        DATA_EN            <= '1';
        DATA_RDWR          <= '1';
        next_state         <= state_loop_end_cond;
      when state_loop_end_cond =>                             -- process condition
        if (DATA_RDATA = "00000000") then
          pc_register_inc  <= '1';
          next_state       <= state_run;
        else
          pc_register_dec  <= '1';
          cnt_register_inc <= '1';
          next_state       <= state_loop_end_jump;
        end if;
      when state_loop_end_jump =>                             -- ignore command
        CODE_EN            <= '1';
        CODE_ADDR          <= pc_register_addres;
        next_state         <= state_loop_end_cnt;
      when state_loop_end_cnt =>                              -- decect [ and ] and increment or decrement counter
        if (instruction = loop_beg) then
          cnt_register_dec <= '1';
        elsif (instruction = loop_end) then
          cnt_register_inc <= '1';
        end if;
        next_state <= state_loop_end_proc;
      when state_loop_end_proc =>                             -- process loop
        if (cnt_register_addres = "00000000") then
          pc_register_inc <= '1';
          next_state      <= state_run;
        else
          pc_register_dec <= '1';
          next_state      <= state_loop_end_jump;
        end if;
      when state_putc =>                                      -- asks for data for printing value
        DATA_EN           <= '1';
        DATA_RDWR         <= '1';
        next_state        <= state_putc_proc;
      when state_putc_proc =>                                 -- print value
        if (OUT_BUSY = '0') then
          OUT_WE          <= '1';
          OUT_DATA        <= DATA_RDATA;
          next_state      <= state_run;
          pc_register_inc <= '1';
        else
          next_state      <= state_putc_proc;
        end if;
      when state_getc =>                                      -- asks for reading
        IN_REQ            <= '1';
        next_state        <= state_getc_proc;
      when state_getc_proc =>                                 -- if possible, print data, otherwise try again
        IN_REQ            <= '1';
        if (IN_VLD = '1') then
          next_state      <= state_run;
          DATA_EN         <= '1';
          DATA_RDWR       <= '0';
          sel             <= "00";
          pc_register_inc <= '1';
        else
          next_state      <= state_getc_proc;
        end if;
      when state_val2tmp =>                                   -- asks for data for making copy
        DATA_EN         <= '1';
        DATA_RDWR       <= '1';
        next_state      <= state_val2tmp_proc;
      when state_val2tmp_proc =>                              -- copy data into tmp register
        tmp_register_id <= '1';
        pc_register_inc <= '1';
        next_state      <= state_run;
      when state_tmp2val =>                                   -- copy data from tmp into memory
        sel             <= "01";
        DATA_RDWR       <= '0';
        DATA_EN         <= '1';
        pc_register_inc <= '1';
        next_state      <= state_run;
      when state_empty =>                                     -- ignore command
        pc_register_inc <= '1';
        next_state      <= state_run;
      when state_eof => next_state <= state_eof;              -- end of source file
      when others    => next_state <= state_empty;            -- unknown command or comment
    end case;
  end process;
end behavioral;

