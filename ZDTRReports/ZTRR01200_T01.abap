*&---------------------------------------------------------------------*
*&  Include           ZTRR01200_T01
*&---------------------------------------------------------------------*

*------------------------------------------------------------------*
*  Type Definition
*------------------------------------------------------------------*
TYPE-POOLS: icon.
*------------------------------------------------------------------*
*  Table Definition
*------------------------------------------------------------------*
TABLES: t001,
        t087j,
        t035t,        "Planning Group Texts
        coas,     "Order Master for Controlling(all Fields in AUFK)
        imzo,     "Assignment Table: CO Object - Capital Investment
        impr,         "Investment Program Positions
        impu,         "Texts for cap. inv. program positions
        zttr0009,     "[TR-CM] I/O Planning Group
        ztim9000.     "[IM] Invest. category & Std. items for P_IO

*------------------------------------------------------------------*
*   Data Definition
*------------------------------------------------------------------*

DATA: it_coas  LIKE coas  OCCURS 0 WITH HEADER LINE.
DATA: it_zttr0009  LIKE zttr0009  OCCURS 0 WITH HEADER LINE.
DATA: it_ztim9000  LIKE ztim9000  OCCURS 0 WITH HEADER LINE.
"[IM] Invest. category & Std. items for P_IO


DATA: BEGIN OF it_aufk OCCURS 0,
        aufnr     LIKE  aufk-aufnr,    "Order Number
        auart     LIKE  aufk-auart,    "Order Type
        autyp     LIKE  aufk-autyp,    "Order category
        aedat     LIKE  aufk-aedat,    "Order
        ktext     LIKE  aufk-ktext,    "Description
     END OF it_aufk.


DATA: BEGIN OF it_list OCCURS 0,
        icon      LIKE  icon-id,
        aufnr     LIKE  coas-aufnr,    "Order Number
        ktext     LIKE  coas-ktext,    "Description
        akstl     LIKE  coas-akstl,    "Requesting cost center
        prnam     LIKE  raip1-prnam,   "Investment program name
        gjahr     LIKE  raip1-gjahr,   "Approval year
        posid     LIKE  raip1-posid,   "Identification for an investment
        post1     LIKE  impu-post1,    "Short description
        izwek     LIKE  coas-izwek,    "Reason for investment
        txt50     LIKE  t087j-txt50,   "
        fdgrv     LIKE  fdsr-grupp,    "
        textl     LIKE  t035t-textl,   "
        count     TYPE i,
     END OF it_list.

*// for Various text.
DATA: gt_t087j  LIKE t087j  OCCURS 0 WITH HEADER LINE.
DATA: gt_imzo   LIKE imzo   OCCURS 0 WITH HEADER LINE.
DATA: gt_impr   LIKE impr   OCCURS 0 WITH HEADER LINE.
DATA: gt_impu   LIKE impu   OCCURS 0 WITH HEADER LINE.
DATA: gt_t035t  LIKE t035t  OCCURS 0 WITH HEADER LINE.


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

* BDC
DATA: BEGIN OF it_bdc OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF it_bdc.

DATA: BEGIN OF st_opt OCCURS 0.
        INCLUDE STRUCTURE ctu_params.
DATA: END OF st_opt.

*Icon constants
CONSTANTS: gc_led_red(4)     VALUE '@5C@'.  "ICON_LED_RED
CONSTANTS: gc_led_green(4)   VALUE '@5B@'.  "ICON_LED_GREEN
CONSTANTS: gc_led_yellow(4)  VALUE '@5D@'.  "ICON_LED_YELLOW
CONSTANTS: char_x(1)         VALUE 'X'.

RANGES: r_aufnr    FOR aufk-aufnr OCCURS 0.

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
PARAMETERS: p_bukrs  LIKE t001-bukrs   OBLIGATORY  default 'H201'.
SELECTION-SCREEN COMMENT 52(40) p_butxt.
SELECTION-SCREEN END OF LINE.



SELECT-OPTIONS : s_aufnr  FOR coas-aufnr OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b0.
*
