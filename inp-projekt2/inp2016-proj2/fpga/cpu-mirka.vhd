-- cpu.vhd: Simple 8-bit CPU (BrainLove interpreter)
-- Copyright (C) 2016 Brno University of Technology,
--                    Faculty of Information Technology
-- Author(s): Miroslava Misova, xmisov00
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
  -- ------------------- SIGNALY a ENUMY ------------------------------
  -- PC register - posouvani ukazatele v souboru
  signal PC_registr_inc: std_logic;
  signal PC_registr_dec: std_logic;
  signal PC_registr_pointer: std_logic_vector (11 downto 0);

  -- CNT register - na pocitani zavorek cyklu
  signal CNT_registr_inc: std_logic;
  signal CNT_registr_dec: std_logic;
  signal CNT_registr_pointer: std_logic_vector (11 downto 0);

  -- TMP register - na ukladani docasne promenne
  signal TMP_registr_id: std_logic;
  signal TMP_registr_pointer: std_logic_vector (7 downto 0);

  -- PTR register - ukazatel do pameti RAM
  signal PTR_registr_inc: std_logic;
  signal PTR_registr_dec: std_logic;
  signal PTR_registr_pointer: std_logic_vector (9 downto 0);

  -- instrukce pro dekoder
  type prikazy is (
    prikaz_return,  -- null

    prikaz_save_tmp,  -- $
    prikaz_load_tmp,  -- !

    prikaz_ptr_val_inc,   -- +
    prikaz_ptr_val_dec,   -- -

    prikaz_print_c, -- .
    prikaz_load_c,  -- ,

    prikaz_ptr_inc,   -- >
    prikaz_ptr_dec,   -- <

    prikaz_while_begin, -- [
    prikaz_while_end,   -- ]

    prikaz_comment  -- cokoliv jineho
  );
  signal prikaz: prikazy;

  -- pro multiplexor
  signal sel: std_logic_vector (1 downto 0);

  type stavy_automatu is (
    stav_nope, stav_nacteni, stav_dekodovani,
    stav_return, stav_comment,
    stav_ptr_inc,   -- >
    stav_ptr_dec,   -- <
    stav_ptr_val_inc_nacteni, stav_ptr_val_inc_vykonani,   -- +
    stav_ptr_val_dec_nacteni, stav_ptr_val_dec_vykonani,   -- -
    stav_print_c_nacteni, stav_print_c_vykonani, -- .
    stav_load_c_nacteni, stav_load_c_vykonani, -- ,
    stav_save_tmp_nacteni, stav_save_tmp_vykonani,  -- $
    stav_load_tmp,  -- !
    stav_while_begin_nacteni_while, stav_while_begin_vyhodnoceni_podminky, stav_while_begin_skocit_na_konec, stav_while_begin_pocitadlo, stav_while_begin_cyklus,
    stav_while_end_nacteni_while, stav_while_end_vyhodnoceni_podminky, stav_while_end_skocit_na_zacatek, stav_while_end_pocitadlo, stav_while_end_cyklus

  );
  signal aktual_stav: stavy_automatu;
  signal nasled_stav: stavy_automatu;

-- ------------------- VLASTNI KOD ------------------------------------------
begin
  PC_registr: process (CLK, RESET)
  begin
    if (RESET = '1') then  PC_registr_pointer <= (others => '0');
    elsif ( (CLK'event) and (CLK = '1') ) then
      if    (PC_registr_inc = '1') then  PC_registr_pointer <= PC_registr_pointer + 1;
      elsif (PC_registr_dec = '1') then  PC_registr_pointer <= PC_registr_pointer - 1;
      end if;
    end if;
  end process;

  CNT_registr: process (CLK, RESET)
  begin
    if (RESET = '1') then  CNT_registr_pointer <= (others => '0');
    elsif ( (CLK'event) and (CLK = '1') ) then
      if    (CNT_registr_inc = '1') then  CNT_registr_pointer <= CNT_registr_pointer + 1;
      elsif (CNT_registr_dec = '1') then  CNT_registr_pointer <= CNT_registr_pointer - 1;
      end if;
    end if;
  end process;

  TMP_registr: process (CLK, RESET)
  begin
    if (RESET = '1') then  TMP_registr_pointer <= (others => '0');
    elsif ( (CLK'event) and (CLK = '1') and (TMP_registr_id = '1') ) then  TMP_registr_pointer <= DATA_RDATA;
    end if;
  end process;

  PTR_registr: process (CLK, RESET)
  begin
    if (RESET = '1') then  PTR_registr_pointer <= (others => '0');
    elsif ( (CLK'event) and (CLK = '1') ) then
      if    (PTR_registr_inc = '1') then  PTR_registr_pointer <= PTR_registr_pointer + 1;
      elsif (PTR_registr_dec = '1') then  PTR_registr_pointer <= PTR_registr_pointer - 1;
      end if;
    end if;
  end process;

  multiplexor: process (CLK, sel, DATA_RDATA, IN_DATA, TMP_registr_pointer)
  begin
    case sel is
      when "00" => DATA_WDATA <= IN_DATA;
      when "01" => DATA_WDATA <= TMP_registr_pointer;
      when "10" => DATA_WDATA <= DATA_RDATA - 1;
      when "11" => DATA_WDATA <= DATA_RDATA + 1;
      when others =>
    end case;
  end process;

  dekoder: process (CODE_DATA)
  begin
    case CODE_DATA is

      when X"00"  => prikaz <= prikaz_return;  -- null

      when X"21"  => prikaz <= prikaz_load_tmp;  -- !
      when X"24"  => prikaz <= prikaz_save_tmp;  -- $

      when X"2B"  => prikaz <= prikaz_ptr_val_inc;   -- +
      when X"2D"  => prikaz <= prikaz_ptr_val_dec;   -- -

      when X"2E"  => prikaz <= prikaz_print_c; -- .
      when X"2C"  => prikaz <= prikaz_load_c;  -- ,

      when X"3E"  => prikaz <= prikaz_ptr_inc;   -- >
      when X"3C"  => prikaz <= prikaz_ptr_dec;   -- <

      when X"5B"  => prikaz <= prikaz_while_begin; -- [
      when X"5D"  => prikaz <= prikaz_while_end;   -- ]

      when others => prikaz <= prikaz_comment;  -- cokoliv jineho

    end case;
  end process;

  ----------- FSM --------------

  nastaveni_stavu_FSM: process (RESET, CLK)
  begin
    if (RESET = '1') then  aktual_stav <= stav_nope;
    elsif ((CLK'event) and (CLK = '1') and (EN = '1')) then  aktual_stav <= nasled_stav;
    end if;
  end process;

  vyhodnoceni_stavu_FSM: process (CODE_DATA, IN_VLD, OUT_BUSY, prikaz, aktual_stav)
  begin
    -- nastaveni pocatecnich hodnot
    -- I/O
      OUT_WE    <= '0';
      IN_REQ    <= '0';
    -- ROM
      CODE_EN   <= '0';
    -- RAM
      DATA_EN   <= '0';
      DATA_ADDR <= PTR_registr_pointer;
    -- registry
      PC_registr_inc  <= '0';
      PC_registr_dec  <= '0';
      PTR_registr_inc <= '0';
      PTR_registr_dec <= '0';
      CNT_registr_inc <= '0';
      CNT_registr_dec <= '0';
      TMP_registr_id  <= '0';

    -- stavovy automat
    case aktual_stav is

        when stav_nope => nasled_stav <= stav_nacteni;

        when stav_nacteni =>
            CODE_EN     <= '1';
            CODE_ADDR   <= PC_registr_pointer;
            nasled_stav <= stav_dekodovani;

        when stav_dekodovani =>
            case prikaz is
                when prikaz_return      => nasled_stav <= stav_return;  -- null

                when prikaz_load_tmp    => nasled_stav <= stav_load_tmp;  -- !
                when prikaz_save_tmp    => nasled_stav <= stav_save_tmp_nacteni;  -- $

                when prikaz_ptr_val_inc => nasled_stav <= stav_ptr_val_inc_nacteni;   -- +
                when prikaz_ptr_val_dec => nasled_stav <= stav_ptr_val_dec_nacteni;   -- -

                when prikaz_print_c     => nasled_stav <= stav_print_c_nacteni; -- .
                when prikaz_load_c      => nasled_stav <= stav_load_c_nacteni;  -- ,

                when prikaz_ptr_inc     => nasled_stav <= stav_ptr_inc;   -- >
                when prikaz_ptr_dec     => nasled_stav <= stav_ptr_dec;   -- <

                when prikaz_while_begin => nasled_stav <= stav_while_begin_nacteni_while; -- [
                when prikaz_while_end   => nasled_stav <= stav_while_end_nacteni_while;   -- ]

                when others             => nasled_stav <= stav_comment;  -- cokoliv jineho
            end case; -- prikaz

        when stav_return => nasled_stav <= stav_return;

        when stav_comment => -- cokoliv jineho
            PC_registr_inc  <= '1';
            nasled_stav     <= stav_nacteni;

        when stav_ptr_inc =>  -- >
            PC_registr_inc  <= '1';
            PTR_registr_inc <= '1';
            nasled_stav     <= stav_nacteni;

        when stav_ptr_dec =>  -- <
            PC_registr_inc  <= '1';
            PTR_registr_dec <= '1';
            nasled_stav     <= stav_nacteni;

        when stav_ptr_val_inc_nacteni =>  -- +
            DATA_RDWR   <= '1';
            DATA_EN     <= '1';
            nasled_stav <= stav_ptr_val_inc_vykonani;

        when stav_ptr_val_inc_vykonani => -- +
            DATA_RDWR       <= '0';
            DATA_EN         <= '1';
            sel             <= "11";
            PC_registr_inc  <= '1';
            nasled_stav     <= stav_nacteni;

        when stav_ptr_val_dec_nacteni =>  -- -
            DATA_RDWR   <= '1';
            DATA_EN     <= '1';
            nasled_stav <= stav_ptr_val_dec_vykonani;

        when stav_ptr_val_dec_vykonani => -- -
            DATA_RDWR       <= '0';
            DATA_EN         <= '1';
            sel             <= "10";
            PC_registr_inc  <= '1';
            nasled_stav     <= stav_nacteni;

        when stav_print_c_nacteni => -- .
            DATA_RDWR   <= '1';
            DATA_EN     <= '1';
            nasled_stav <= stav_print_c_vykonani;

        when stav_print_c_vykonani => -- .
            if (OUT_BUSY = '0') then
                OUT_DATA        <= DATA_RDATA;
                OUT_WE          <= '1';
                PC_registr_inc  <= '1';
                nasled_stav     <= stav_nacteni;
            else
                nasled_stav     <= stav_print_c_vykonani;
            end if;

        when stav_load_c_nacteni => -- ,
            IN_REQ      <= '1';
            nasled_stav <= stav_load_c_vykonani;

        when stav_load_c_vykonani => -- ,
            -- IN_REQ <= '1';
            if (IN_VLD = '1') then
                DATA_RDWR       <= '0';
                DATA_EN         <= '1';
                sel             <= "00";
                PC_registr_inc  <= '1';
                nasled_stav     <= stav_nacteni;
            else
                nasled_stav     <= stav_load_c_vykonani;
            end if;

        when stav_save_tmp_nacteni => -- $
            DATA_RDWR   <= '1';
            DATA_EN     <= '1';
            nasled_stav <= stav_save_tmp_vykonani;

        when stav_save_tmp_vykonani => -- $
            TMP_registr_id  <= '1';
            PC_registr_inc  <= '1';
            nasled_stav     <= stav_nacteni;

        when stav_load_tmp => -- !
            DATA_RDWR       <= '0';
            DATA_EN         <= '1';
            sel             <= "01";
            PC_registr_inc  <= '1';
            nasled_stav     <= stav_nacteni;

        -- while begin
        when stav_while_begin_nacteni_while =>
            DATA_RDWR       <= '1';
            DATA_EN         <= '1';
            PC_registr_inc  <= '1';
            nasled_stav     <= stav_while_begin_vyhodnoceni_podminky;

        when stav_while_begin_vyhodnoceni_podminky =>
            if (DATA_RDATA = "00000000") then
                CNT_registr_inc <= '1';
                nasled_stav <= stav_while_begin_skocit_na_konec;
            else
                nasled_stav <= stav_nacteni;
            end if;

        when stav_while_begin_skocit_na_konec =>
            CODE_EN     <= '1';
            CODE_ADDR   <= PC_registr_pointer;
            nasled_stav <= stav_while_begin_pocitadlo;

        when stav_while_begin_pocitadlo =>
            if  (prikaz = prikaz_while_begin) then  CNT_registr_inc <= '1';
            elsif (prikaz = prikaz_while_end) then  CNT_registr_dec <= '1';
            end if;
            PC_registr_inc  <= '1';
            nasled_stav     <= stav_while_begin_cyklus;

        when stav_while_begin_cyklus =>
            if (CNT_registr_pointer = "000000000000") then  nasled_stav <= stav_nacteni;
            else  nasled_stav <= stav_while_begin_skocit_na_konec;
            end if;

        -- while end
        when stav_while_end_nacteni_while =>
            DATA_RDWR   <= '1';
            DATA_EN     <= '1';
            nasled_stav <= stav_while_end_vyhodnoceni_podminky;

        when stav_while_end_vyhodnoceni_podminky =>
            if (DATA_RDATA = "00000000") then
                PC_registr_inc <= '1';
                nasled_stav    <= stav_nacteni;
            else
                PC_registr_dec  <= '1';
                CNT_registr_inc <= '1';
                nasled_stav <= stav_while_end_skocit_na_zacatek;
            end if;

        when stav_while_end_skocit_na_zacatek =>
            CODE_EN     <= '1';
            CODE_ADDR   <= PC_registr_pointer;
            nasled_stav <= stav_while_end_pocitadlo;

        when stav_while_end_pocitadlo =>
            if  (prikaz = prikaz_while_begin) then  CNT_registr_dec <= '1';
            elsif (prikaz = prikaz_while_end) then CNT_registr_inc <= '1';
            end if;
            nasled_stav <= stav_while_end_cyklus;

        when stav_while_end_cyklus =>
            if (CNT_registr_pointer = "000000000000") then
                PC_registr_inc <= '1';
                nasled_stav <= stav_nacteni;
            else
                PC_registr_dec <= '1';
                nasled_stav <= stav_while_end_skocit_na_zacatek;
            end if;

        when others => nasled_stav <= stav_comment;
    end case; -- aktual_stav

  end process;

end behavioral;