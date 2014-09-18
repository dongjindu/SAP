*----------------------------------------------------------------------*
*   INCLUDE ZIPP501L_ENGIN_PP_TOP                                      *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* TABLE
*----------------------------------------------------------------------*
TABLES: plaf,
        ztppep2,
        zsppep_rfc.

TYPE-POOLS: slis.
*----------------------------------------------------------------------*
* INTERNALTABLE
*----------------------------------------------------------------------*
DATA: BEGIN OF it_plaf OCCURS 0,
        plnum LIKE plaf-plnum,
        psttr LIKE plaf-psttr,
        matnr LIKE plaf-matnr,
        gsmng LIKE plaf-gsmng,
        meins LIKE plaf-meins,
        plgrp LIKE plaf-plgrp,
      END OF it_plaf.

DATA: *it_plaf LIKE it_plaf OCCURS 0 WITH HEADER LINE.
DATA: it_ztppep2 LIKE zsppep2_rfc OCCURS 0 WITH HEADER LINE.
DATA: *it_ztppep2 LIKE ztppep2 OCCURS 0 WITH HEADER LINE.
DATA: *it_ztppep LIKE TABLE OF ztppep WITH HEADER LINE.
DATA: it_compa LIKE ztppep2 OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF it_ztppep2_eng OCCURS 0.
        INCLUDE STRUCTURE zsppep2_rfc.
DATA: en_spc15(5),
      END OF it_ztppep2_eng.

DATA: BEGIN OF it_list OCCURS 0,
        flag(3),
        pdate   LIKE ztppep2-pdate,
        plnum   LIKE ztppep2-plnum,
        pitem   LIKE ztppep2-pitem,
        maktx   LIKE makt-maktx  ,
        gsmng   LIKE ztppep2-gsmng,
        meins(4),
        plgrp(4),
        zuser   LIKE ztppep2-zuser,
        zedat   LIKE ztppep2-zedat,
        zetim   LIKE ztppep2-zetim,
        zmsg(50),
        en_spc15(5),
      END OF it_list.

DATA: it_eng1 LIKE TABLE OF zsppep_rfc WITH HEADER LINE,
      it_eng2 LIKE TABLE OF it_ztppep2 WITH HEADER LINE.

*----------------------------------------------------------------------*
* DATA
*----------------------------------------------------------------------*
DATA: wa_ztppep2 LIKE ztppep2.
DATA : z_total LIKE ztca_if_log-total,  "TOTAL COUNT
       z_succ LIKE ztca_if_log-zsucc,   "SUCCESS COUNT
       z_fail LIKE ztca_if_log-zsucc.   "ERROR COUNT
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
CONSTANTS : c_mark  VALUE 'X'.
** for E002
*CONSTANTS : C_E001  TYPE  T001W-WERKS   VALUE  'E001' ,
CONSTANTS : c_sea   TYPE  marc-fevor    VALUE  'SEA'  ,
            c_sec   TYPE  marc-fevor    VALUE  'SEC'  ,
            c_me1   TYPE  plaf-dispo    VALUE  'ME1'  .
*----------------------------------------------------------------------*
*  RANGES
*----------------------------------------------------------------------*
RANGES: s_date   FOR ztppep2-pdate,
        r_plgrp  FOR plaf-plgrp.    "ProdScheduler

*----------------------------------------------------------------------*
* Macro
*----------------------------------------------------------------------*
*  DEFINE APPEND_FIELDCAT.
*    &1 = &1 + 1.
*    W_FIELDCAT-COL_POS       = &1.
*    W_FIELDCAT-FIELDNAME     = &2.
*    W_FIELDCAT-REF_FIELDNAME = &3.
*    W_FIELDCAT-KEY           = &4.
*    W_FIELDCAT-QFIELDNAME    = &5.
*    W_FIELDCAT-CFIELDNAME    = &6.
*    W_FIELDCAT-QTABNAME      = &10.
*    W_FIELDCAT-SELTEXT_L     = &7.
*    W_FIELDCAT-SELTEXT_M     = &7.
*    W_FIELDCAT-SELTEXT_S     = &7.
*    W_FIELDCAT-OUTPUTLEN     = &8.
*    W_FIELDCAT-NO_OUT        = &9.
*    APPEND W_FIELDCAT.
*    CLEAR : W_FIELDCAT.
*  END-OF-DEFINITION.

*----------------------------------------------------------------------
* BDC
*----------------------------------------------------------------------
DATA: BEGIN OF it_bdc OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF it_bdc.

DATA: it_msg LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
DATA: it_bdcdata LIKE TABLE OF bdcdata WITH HEADER LINE.
DATA: wa_bdcgroup LIKE  sy-uname,
      p_tcode     LIKE  tstc-tcode.

*----------------------------------------------------------------------*
* Define variable for ALV
*----------------------------------------------------------------------*
DATA : w_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
       w_eventcat TYPE slis_t_event WITH HEADER LINE,
       w_selfield TYPE slis_selfield,
       w_sortcat  TYPE slis_t_sortinfo_alv WITH HEADER LINE,
       w_col_pos  TYPE i,
       w_program  LIKE sy-repid,
       w_top_of_page TYPE slis_t_listheader.

*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

SELECT-OPTIONS: s_pdate FOR ztppep2-pdate OBLIGATORY NO-EXTENSION,
                s_plnum FOR ztppep2-plnum MODIF ID abc,
                s_matnr FOR ztppep2-pitem MODIF ID abc.
*                                         NO-EXTENSION
*                                         NO INTERVALS
PARAMETERS: p_werks LIKE t001w-werks OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-005.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 5.
PARAMETERS      r_1 RADIOBUTTON GROUP rad1 DEFAULT 'X'.
SELECTION-SCREEN POSITION 7.
SELECTION-SCREEN COMMENT (25) text-006 FOR FIELD r_1.
*SELECTION-SCREEN POSITION 25.
PARAMETERS      r_2 RADIOBUTTON GROUP rad1.
*SELECTION-SCREEN POSITION 26.
SELECTION-SCREEN COMMENT (25) text-007 FOR FIELD r_2.
*SELECTION-SCREEN POSITION 55.
PARAMETERS      r_3 RADIOBUTTON GROUP rad1.
*SELECTION-SCREEN POSITION 56.
SELECTION-SCREEN COMMENT (20) text-008 FOR FIELD r_3.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-004.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 5.
PARAMETERS       r1 RADIOBUTTON GROUP rad2 DEFAULT 'X'.
SELECTION-SCREEN POSITION 7.
SELECTION-SCREEN COMMENT (30) text-002 FOR FIELD r1.
*SELECTION-SCREEN POSITION 40.
PARAMETERS       r2 RADIOBUTTON GROUP rad2.
*SELECTION-SCREEN POSITION 41.
SELECTION-SCREEN COMMENT (25) text-003 FOR FIELD r2.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b3.

PARAMETERS : p_mode LIKE ctu_params-dismode DEFAULT 'N' NO-DISPLAY .
