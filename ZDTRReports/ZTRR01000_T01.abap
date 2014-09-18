*&---------------------------------------------------------------------*
*&  Include           ZTRR01000_T01
*&---------------------------------------------------------------------*
*------------------------------------------------------------------*
*  Type Definition
*------------------------------------------------------------------*
TYPE-POOLS: icon.
*------------------------------------------------------------------*
*  Table Definition
*------------------------------------------------------------------*
TABLES : bkpf, faglflext, skat, t038, t038t, faglflexa.
*------------------------------------------------------------------*
*   Data Definition
*------------------------------------------------------------------*

DATA : it_skb1  LIKE skb1  OCCURS 0 WITH HEADER LINE.

DATA : it_flext  LIKE faglflext OCCURS 0 WITH HEADER LINE.
DATA : it_flexa  LIKE faglflexa OCCURS 0 WITH HEADER LINE.



*// ALV output List
DATA : BEGIN OF it_list OCCURS 0,
         bankl     LIKE bnka-bankl,       "Bank Keys
         banka     LIKE bnka-banka,       "Name of bank
         racct     LIKE  bseg-hkont,
         txt50     LIKE  skat-txt50,      " Bank Name
         fdlev     LIKE  skb1-fdlev,      " Planning Level
         glied     LIKE  t038-glied,      " Grouping
         textu     LIKE  t038t-textu,     " Grouping Name
         amt01     LIKE  bseg-wrbtr,      " Balance
         amt02     LIKE  bseg-wrbtr,      " Incoming In
         amt03     LIKE  bseg-wrbtr,      " Incoming Clearing
         amt04     LIKE  bseg-wrbtr,      " Outgoing In
         amt05     LIKE  bseg-wrbtr,      " Outgoing Clearing
         amt06     LIKE  bseg-wrbtr,      " Outgoing Clearing
      END OF it_list.

RANGES: r_hkont    FOR skb1-saknr OCCURS 0.

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

*PARAMETER : p_bukrs  LIKE bkpf-bukrs OBLIGATORY  MEMORY ID buk.
PARAMETER : p_budat  LIKE bkpf-budat OBLIGATORY DEFAULT sy-datum.
SELECT-OPTIONS : s_saknr  FOR faglflext-racct NO-DISPLAY.
SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN  BEGIN OF BLOCK bl2 WITH FRAME TITLE text-h02.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_rb1 RADIOBUTTON GROUP gr1. "form print
SELECTION-SCREEN   COMMENT    3(27) text-h03.
PARAMETERS: p_rb2 RADIOBUTTON GROUP gr1 DEFAULT 'X'.     "display ALV
SELECTION-SCREEN   COMMENT    33(15) text-h04.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN  END   OF BLOCK bl2.

SELECTION-SCREEN END OF BLOCK b0.
