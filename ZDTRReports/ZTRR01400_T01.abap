*&---------------------------------------------------------------------*
*&  Include           ZTRR01400_T01
*&---------------------------------------------------------------------*

*------------------------------------------------------------------*
*  Type Definition
*------------------------------------------------------------------*
TYPE-POOLS: icon.
*------------------------------------------------------------------*
*  Table Definition
*------------------------------------------------------------------*
TABLES : bkpf, bseg.
*------------------------------------------------------------------*
*   Data Definition
*------------------------------------------------------------------*
DATA : it_skb1  LIKE skb1  OCCURS 0 WITH HEADER LINE.
DATA : it_bkpf  LIKE bkpf OCCURS 0 WITH HEADER LINE.
DATA : it_bsak  LIKE bsak OCCURS 0 WITH HEADER LINE.
DATA : it_bsad  LIKE bsad OCCURS 0 WITH HEADER LINE.
DATA : it_bsas  LIKE bsas OCCURS 0 WITH HEADER LINE.
DATA : it_bseg  LIKE bseg OCCURS 0 WITH HEADER LINE.
DATA : gt_bseg  LIKE bseg OCCURS 0 WITH HEADER LINE.
DATA : gt_btax  LIKE bseg OCCURS 0 WITH HEADER LINE.

* When you change following interanl tabel, then change
* also declaration of select_faglflexa_gl_data in lztrcmf04

DATA : BEGIN OF it_list OCCURS 0,
         belnr     LIKE  bkpf-belnr,      "
         gjahr     LIKE  bkpf-gjahr,      "
         buzei     LIKE  bseg-buzei,
         buze2     LIKE  bseg-buzei,
         augbl     LIKE  bseg-augbl,
         augdt     LIKE  bseg-augdt,
* Cash Related
         beln1     LIKE  bkpf-belnr,      "
         gjah1     LIKE  bkpf-gjahr,      "
         buze1     LIKE  bseg-buzei,
         buda1     LIKE  bkpf-budat,

         blart     LIKE  bkpf-blart,
         budat     LIKE  bkpf-budat,
         bldat     LIKE  bkpf-bldat,
         cpudt     LIKE  bkpf-cpudt,
         hkont     LIKE  bseg-hkont,
         txt50     LIKE  skat-txt50,
         shkzg     LIKE  bseg-shkzg,
         koart     LIKE  bseg-koart,
         zlsch     LIKE  bseg-zlsch,
         mwskz     LIKE  bseg-mwskz,
         wrbtr     LIKE  bseg-wrbtr,
         dmbtr     LIKE  bseg-dmbtr,
         stjah     LIKE  bkpf-stjah,
         stblg     LIKE  bkpf-stblg,
         icon  LIKE icon-id,
      END OF it_list.



RANGES: r_hkont1    FOR skb1-saknr OCCURS 0.

*// "Financial Management Area
CONSTANTS: gc_fikrs   LIKE  fmifiit-fikrs  VALUE 'H201',
           gc_ktopl   LIKE  skat-ktopl     VALUE 'HNA1'.

*Icon constants
CONSTANTS: gc_led_red(4)     VALUE '@5C@'.  "ICON_LED_RED
CONSTANTS: gc_led_green(4)   VALUE '@5B@'.  "ICON_LED_GREEN
CONSTANTS: gc_led_yellow(4)  VALUE '@5D@'.  "ICON_LED_YELLOW
CONSTANTS: char_x(1)         VALUE 'X'.

*------------------------------------------------------------------*
*  ALV                                                     *
*------------------------------------------------------------------*
TYPE-POOLS: slis, icon.


DATA: alv_event      TYPE slis_t_event,
      alv_keyinfo    TYPE  slis_keyinfo_alv,
      alv_layout     TYPE slis_layout_alv,
      alv_fieldcat   TYPE slis_t_fieldcat_alv,
      g_repid        LIKE sy-repid,
      alv_sort       TYPE slis_t_sortinfo_alv   WITH HEADER LINE.

DATA : top_of_page TYPE slis_t_listheader.



*&----------------------------------------------------------------------
*&     MACRO DEFINITION.
*&----------------------------------------------------------------------
*___Macro Definition
DEFINE make_ranges.
  move:  'I'     to &2-sign,
         'EQ'    to &2-option,
         &1      to &2-low.
  append &2.
END-OF-DEFINITION.

*------------------------------------------------------------------*
* SelectionScreen
*------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE text-pa1.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) text-h01.
PARAMETERS: p_bukrs  LIKE t001-bukrs   OBLIGATORY  MEMORY ID buk.
SELECTION-SCREEN COMMENT 52(40) p_butxt.
SELECTION-SCREEN END OF LINE.

SELECT-OPTIONS : s_gjahr  FOR bkpf-gjahr OBLIGATORY
                                         default sy-datum(4).

SELECT-OPTIONS : s_budat  FOR bkpf-budat OBLIGATORY.
SELECT-OPTIONS : s_belnr  FOR bkpf-belnr.
SELECT-OPTIONS : s_hkont  FOR bseg-hkont.

SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-t03.

PARAMETERS : p_eca AS CHECKBOX.
PARAMETERS : p_memory NO-DISPLAY.
*SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN END OF BLOCK b2.
SELECTION-SCREEN END OF BLOCK b0.
