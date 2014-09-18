*----------------------------------------------------------------------*
*   INCLUDE ZIPP502L_ENGIN_PS_TOP                                      *
*----------------------------------------------------------------------*
TABLES: ztppes,
        mara.

TYPE-POOLS: slis.
*----------------------------------------------------------------------*
* I/T
*----------------------------------------------------------------------*
DATA: it_ztppes  LIKE TABLE OF zsppes_rfc WITH HEADER LINE,
      *it_ztppes LIKE TABLE OF ztppes WITH HEADER LINE.
DATA: it_vmaster1 LIKE TABLE OF bapi1003_alloc_values_num
                                         WITH HEADER LINE,
      it_vmaster  LIKE TABLE OF bapi1003_alloc_values_char
                                         WITH HEADER LINE,
      it_vmaster2 LIKE TABLE OF bapi1003_alloc_values_curr
                                         WITH HEADER LINE,
      return LIKE TABLE OF bapiret2 WITH HEADER LINE.

DATA: BEGIN OF it_list OCCURS 0,
        flag(3),
        en_item TYPE ztppes-en_item,
        maktx    TYPE makt-maktx   ,
        en_veh_model TYPE ztppes-en_veh_model,
        en_head TYPE ztppes-en_head,
*        EN_REG TYPE ZTPPES-EN_REG,
        en_spc01 TYPE ztppes-en_spc01,
        en_spc02 TYPE ztppes-en_spc02,
        en_spc03 TYPE ztppes-en_spc03,
        en_spc04 TYPE ztppes-en_spc04,
        en_spc05 TYPE ztppes-en_spc05,
        en_spc06 TYPE ztppes-en_spc06,
        en_spc07 TYPE ztppes-en_spc07,
        en_spc08 TYPE ztppes-en_spc08,
        en_spc09 TYPE ztppes-en_spc09,
        en_spc10 TYPE ztppes-en_spc10,
        en_spc11 TYPE ztppes-en_spc11,
        en_spc12 TYPE ztppes-en_spc12,
        en_spc13 TYPE ztppes-en_spc13,
        en_spc14 TYPE ztppes-en_spc14,
        en_spc15 TYPE ztppes-en_spc15,
        en_spc16 TYPE ztppes-en_spc16,
        en_spc17 TYPE ztppes-en_spc17,
        en_spc18 TYPE ztppes-en_spc18,
        en_spc19 TYPE ztppes-en_spc19,
        en_spc20 TYPE ztppes-en_spc20,
        en_spc21 TYPE ztppes-en_spc21,
        en_spc22 TYPE ztppes-en_spc22,
        en_spc23 TYPE ztppes-en_spc23,
        en_spc24 TYPE ztppes-en_spc24,
        en_spc25 TYPE ztppes-en_spc25,
        en_spc26 TYPE ztppes-en_spc26,
        en_spc27 TYPE ztppes-en_spc27,
        en_spc28 TYPE ztppes-en_spc28,
        en_spc29 TYPE ztppes-en_spc29,
        en_spc30 TYPE ztppes-en_spc30,
        en_spc31 TYPE ztppes-en_spc31,
        en_spc32 TYPE ztppes-en_spc32,
        en_spc33 TYPE ztppes-en_spc33,
        en_spc34 TYPE ztppes-en_spc34,
        en_spc35 TYPE ztppes-en_spc35,
        en_spc36 TYPE ztppes-en_spc36,
        en_spc37 TYPE ztppes-en_spc37,
        en_spc38 TYPE ztppes-en_spc38,
        en_spc39 TYPE ztppes-en_spc39,
        en_spc40 TYPE ztppes-en_spc40,
        en_spc41 TYPE ztppes-en_spc41,
        en_spc42 TYPE ztppes-en_spc42,
        en_spc43 TYPE ztppes-en_spc43,
        en_spc44 TYPE ztppes-en_spc44,
        en_spc45 TYPE ztppes-en_spc45,
        en_spc46 TYPE ztppes-en_spc46,
        en_spc47 TYPE ztppes-en_spc47,
        en_spc48 LIKE ztppes-en_spc48,
        en_spc49 LIKE ztppes-en_spc49,
        en_spc50 LIKE ztppes-en_spc50,
        en_spc51 LIKE ztppes-en_spc51,
        en_spc52 LIKE ztppes-en_spc52,
        en_spc53 LIKE ztppes-en_spc53,
        en_spc54 LIKE ztppes-en_spc54,
        en_spc55 LIKE ztppes-en_spc55,
        en_spc56 LIKE ztppes-en_spc56,
        en_spc57 LIKE ztppes-en_spc57,
        en_spc58 LIKE ztppes-en_spc58,
        en_spc59 LIKE ztppes-en_spc59,
        en_spc60 LIKE ztppes-en_spc60,
        zuser LIKE ztppes-zuser,
        zsdat LIKE ztppes-zsdat,
        zstim LIKE ztppes-zstim,
        zedat LIKE ztppes-zedat,
        zetim LIKE ztppes-zetim,
        zmode LIKE ztppes-zmode,
        zresult LIKE ztppes-zresult,
        zmsg LIKE ztppes-zmsg,
      END OF it_list.

*---- 2004.02.19 Changed --- Mr. Moon
DATA : BEGIN OF it_marc OCCURS 0,
        matnr    TYPE  marc-matnr ,   "Material #
        fevor    TYPE  marc-fevor .   "Production scheduler
DATA : END OF it_marc.

DATA : it_vm   LIKE TABLE OF zspp_vin_value WITH HEADER LINE.
*----------------------------------------------------------------------*
* DATA
*----------------------------------------------------------------------*
DATA : z_total LIKE ztca_if_log-total,
       z_succ LIKE ztca_if_log-zsucc,
       z_fail LIKE ztca_if_log-zsucc.
DATA: i_ztca_if_log LIKE ztca_if_log.

DATA: ok_code LIKE sy-ucomm,
      okcode LIKE sy-ucomm.

*----------------------------------------------------------------------*
*  CONSTANS
*----------------------------------------------------------------------*
CONSTANTS  : c_dest(10) VALUE 'WMPP01'.
"Outbound Interface Destination
CONSTANTS : c_formname_top_of_page TYPE slis_formname
                                        VALUE 'TOP_OF_PAGE'.
CONSTANTS : c_mark VALUE 'X',
*            C_E001    TYPE  T001W-WERKS VALUE  'E001' ,
            c_sea     TYPE  marc-fevor  VALUE  'SEA'  ,
            c_sec     TYPE  marc-fevor  VALUE  'SEC'  .
*----------------------------------------------------------------------*
* Macro
*----------------------------------------------------------------------*
DEFINE append_fieldcat.
  &1 = &1 + 1.
  w_fieldcat-col_pos       = &1.
  w_fieldcat-fieldname     = &2.
  w_fieldcat-ref_fieldname = &3.
  w_fieldcat-key           = &4.
*    W_FIELDCAT-QFIELDNAME    = &5.
*    W_FIELDCAT-CFIELDNAME    = &6.
  w_fieldcat-seltext_l     = &5.
  w_fieldcat-seltext_m     = &5.
  w_fieldcat-seltext_s     = &5.
  w_fieldcat-outputlen     = &6.
  w_fieldcat-no_out        = &7.
  append w_fieldcat.
  clear : w_fieldcat.
END-OF-DEFINITION.

*----------------------------------------------------------------------*
* Define variable for ALV
*----------------------------------------------------------------------*
DATA : w_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
       w_eventcat TYPE slis_t_event WITH HEADER LINE,
       w_sortcat  TYPE slis_t_sortinfo_alv WITH HEADER LINE,
       w_col_pos  TYPE i,
       w_program  LIKE sy-repid,
       w_top_of_page TYPE slis_t_listheader.

*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS : s_matnr FOR ztppes-en_item OBLIGATORY NO INTERVALS.
** for E002
PARAMETERS: p_werks LIKE t001w-werks OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-006.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 5.
PARAMETERS      r_1 RADIOBUTTON GROUP rad1 DEFAULT 'X'.
SELECTION-SCREEN POSITION 7.
SELECTION-SCREEN COMMENT (25) text-007 FOR FIELD r_1.
PARAMETERS      r_2 RADIOBUTTON GROUP rad1.
SELECTION-SCREEN COMMENT (25) text-008 FOR FIELD r_2.
PARAMETERS      r_3 RADIOBUTTON GROUP rad1.
SELECTION-SCREEN COMMENT (20) text-009 FOR FIELD r_3.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-004.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 5.
PARAMETERS       r1 RADIOBUTTON GROUP radi DEFAULT 'X'.
SELECTION-SCREEN POSITION 7.
SELECTION-SCREEN COMMENT (30) text-002 FOR FIELD r1.
PARAMETERS       r2 RADIOBUTTON GROUP radi.
SELECTION-SCREEN COMMENT (25) text-003 FOR FIELD r2.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b3.
