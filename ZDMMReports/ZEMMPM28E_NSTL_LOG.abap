************************************************************************
* Program Name      : ZEMMPM28E_NSTL_LOG
* Author            : Sung-Tae, Lim
* Creation Date     : 2004.04.29.
* Specifications By : Sung-Tae, Lim
* Development Request No : UD1K907802
* Addl Documentation:
* Description       : Supply to Line Log Display
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2004.04.29.     Sung-Tae Lim     UD1K907802     Initial Coding
*
*
************************************************************************

REPORT zemmpm28e_nstl_log NO STANDARD PAGE HEADING
                          LINE-SIZE 400
*                          LINE-COUNT 64(1)
                          MESSAGE-ID zmmm.

**---
INCLUDE : zrmmpmxxr_incl.


**---
DATA : BEGIN OF it_itab OCCURS 0.
        INCLUDE STRUCTURE ztmm_nstl_log.
DATA :   maktx LIKE makt-maktx,
         linecolor(4),
         mark(1),
       END OF it_itab.

* Log Date
DATA: fr_logdate  LIKE sy-datum.
DATA: to_logdate  LIKE sy-datum.

RANGES : r_msgty FOR ztmm_nstl_log-msgty.

*----- BDC
DATA : BEGIN OF it_bdc OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA : END OF it_bdc.

DATA : BEGIN OF it_mess OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA : END OF it_mess.

DATA : it_message LIKE it_mess OCCURS 0 WITH HEADER LINE.

DATA : w_mode LIKE ctu_params-dismode VALUE 'N'.

**--- Macro
DEFINE append_fieldcat.
  &1 = &1 + 1.
  w_fieldcat-col_pos    = &1.
  w_fieldcat-fieldname  = &2.
  w_fieldcat-outputlen  = &3.
  w_fieldcat-seltext_l  = &4.
  w_fieldcat-seltext_m  = &4.
  w_fieldcat-seltext_s  = &4.
  w_fieldcat-datatype   = &5.
  w_fieldcat-key        = &6.
  w_fieldcat-qfieldname = &7.
  w_fieldcat-cfieldname = &8.
  append w_fieldcat.
  clear : w_fieldcat.
END-OF-DEFINITION.

DEFINE append_top.
  clear : w_line.
  if not &3 is initial or not &4 is initial.
    w_line-typ   = &1.
    w_line-key   = &2.
    concatenate &3 '~' &4 into w_line-info separated by space.
    append w_line to w_top_of_page.
  endif.
END-OF-DEFINITION.

DEFINE append_sortcat.
  w_sortcat-spos      = &1.
  w_sortcat-fieldname = &2.
  w_sortcat-tabname   = &3.
  w_sortcat-up        = &4.
  w_sortcat-subtot    = &5.
  append w_sortcat.
  clear : w_sortcat.
END-OF-DEFINITION.

**---
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 01.
SELECTION-SCREEN COMMENT (12) text-010.
SELECTION-SCREEN POSITION 33.
PARAMETERS p_succe AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT (10) text-011 FOR FIELD p_succe.
SELECTION-SCREEN POSITION 46.
PARAMETERS p_error AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT (10) text-012 FOR FIELD p_error.
SELECTION-SCREEN POSITION 58.
*PARAMETERS p_space AS CHECKBOX DEFAULT 'X'.
*SELECTION-SCREEN COMMENT (10) text-013 FOR FIELD p_space.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN ULINE.

SELECT-OPTIONS : " s_log_h   FOR ztmm_nstl_log-logno_h,
                 " s_ztcode  FOR ztmm_nstl_log-ztcode,
                 " s_zpgm    FOR ztmm_nstl_log-zprogramm,
                 s_sdate   FOR ztmm_nstl_log-sdate,
                 s_stime   FOR ztmm_nstl_log-stime,
                 s_dispo   FOR ztmm_nstl_log-dispo,
                 s_feedr   FOR ztmm_nstl_log-feedr,
                 s_matnr   FOR ztmm_nstl_log-matnr,
                 s_tanum   FOR ztmm_nstl_log-tanum,
                 s_ernam   FOR ztmm_nstl_log-ernam,
                 s_erdat   FOR ztmm_nstl_log-erdat,
                 s_erzet   FOR ztmm_nstl_log-erzet.

SELECTION-SCREEN ULINE.

PARAMETERS : p_kquit AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK block1.

**---
INITIALIZATION.
  PERFORM init.
  PERFORM event_build USING w_eventcat[].


**---
TOP-OF-PAGE.
  PERFORM top_of_page.


**---
START-OF-SELECTION.
  PERFORM get_data.


**---
END-OF-SELECTION.
  IF it_itab[] IS INITIAL.
    MESSAGE s999 WITH text-m01.
  ELSE.
    PERFORM comment_build.
    PERFORM make_alv_grid.
  ENDIF.



*&---------------------------------------------------------------------*
*&      Form  init
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init.
*--- For User
  MOVE: 'I'      TO s_ernam-sign,
        'EQ'     TO s_ernam-option,
        sy-uname TO s_ernam-low.
  APPEND s_ernam.

*--- For Log Date
  fr_logdate = sy-datum - 1.
  to_logdate = sy-datum.
  MOVE: 'I'        TO s_erdat-sign,
        'BT'       TO s_erdat-option,
        fr_logdate TO s_erdat-low,
        to_logdate TO s_erdat-high.
  APPEND s_erdat.
ENDFORM.                    " init

*&---------------------------------------------------------------------*
*&      Form  comment_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM comment_build.
*---
  CLEAR : w_line.
  w_line-typ  = 'H'.
  w_line-info = text-002.
  APPEND w_line TO w_top_of_page.

*  CLEAR : w_line.
*  APPEND INITIAL LINE TO w_top_of_page.
*
*  append_top :
*      'S' text-003 s_matnr-low s_matnr-high.
ENDFORM.                    " comment_build

*&---------------------------------------------------------------------*
*&      Form  make_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_alv_grid.
*---
  PERFORM build_fieldcat.
  PERFORM build_sortcat.

  CLEAR : w_program.

  MOVE : sy-repid TO w_program.

  MOVE : 'LINECOLOR' TO w_layout-info_fieldname,
         'X'         TO w_layout-colwidth_optimize,
         'MARK'      TO w_layout-box_fieldname.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program       = w_program
            i_callback_pf_status_set = 'SET_STATUS'
            i_callback_user_command  = 'USER_COMMAND'
            is_layout                = w_layout
            it_fieldcat              = w_fieldcat[]
            it_events                = w_eventcat[]
            it_sort                  = w_sortcat[]
            i_save                   = 'A'
       TABLES
            t_outtab                 = it_itab
       EXCEPTIONS
            program_error            = 1
            OTHERS                   = 2.
ENDFORM.                    " make_alv_grid

*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcat.
**--- &1 : position       &2 : field name       &3 : field length
**--- &4 : description    &5 : field type       &6 : key
**--- &7 : qty field      &8 : color
  append_fieldcat :
    w_col_pos 'LOGNO_H'   10 'Log Number'     'NUMC' 'X' ''      '',
    w_col_pos 'MATNR'     18 'Material'       'CHAR' 'X' ''      '',
    w_col_pos 'MAKTX'     30 'Material Desc'  'CHAR' 'X' ''      '',
    w_col_pos 'SDATE'     10 'TO S/Date'      'DATS' ''  ''      '',
    w_col_pos 'STIME'     08 'TO S/Time'      'TIMS' ''  ''      '',
    w_col_pos 'EDATE'     10 'TO E/Date'      'DATS' ''  ''      '',
    w_col_pos 'ETIME'     08 'TO E/Time'      'TIMS' ''  ''      '',
    w_col_pos 'BDMNG'     12 'Quantity'       'QUAN' ''  'MEINS' '',
    w_col_pos 'GESME'     12 'Current Stock'  'QUAN' ''  'MEINS' '',
    w_col_pos 'LPMIN'     12 'Safety Stock'   'QUAN' ''  'MEINS' '',
    w_col_pos 'BFERR'     12 'B/F Error Qty'  'QUAN' ''  'MEINS' '',
    w_col_pos 'VSOLA'     12 'Open TO Qty'    'QUAN' ''  'MEINS' '',
    w_col_pos 'MEINS'     03 'UoM'            'UNIT' ''  ''      '',
    w_col_pos 'WORKS'     05 'Workstation'    'CHAR' ''  ''      '',
    w_col_pos 'RH_LH'     02 'RH/LH'          'CHAR' ''  ''      '',
    w_col_pos 'ZLINE'     02 'Line'           'CHAR' ''  ''      '',
    w_col_pos 'DISPO'     03 'MRP Controller' 'CHAR' ''  ''      '',
    w_col_pos 'FEED_CYCLE'   04 'Feed Cycle'  'NUMC' ''  'MEINS' '',
    w_col_pos 'ZTIME'     03 'Time for STL'   'NUMC' ''  ''      '',
    w_col_pos 'RDMNG'     12 'Rounding Qty'   'QUAN' ''  'MEINS' '',
    w_col_pos 'TQTY'      12 'TO Qty'         'QUAN' ''  'MEINS' '',
    w_col_pos 'FEEDR'     05 'Feeder'         'CHAR' ''  ''      '',
    w_col_pos 'TANUM'     10 'TO Number'      'CHAR' ''  ''      '',
    w_col_pos 'SRC_LGTYP' 03 'Src S/Type'     'CHAR' ''  ''      '',
    w_col_pos 'SRC_LGPLA' 10 'Src S/Bin'      'CHAR' ''  ''      '',
    w_col_pos 'DES_LGTYP' 03 'Des S/Type'     'CHAR' ''  ''      '',
    w_col_pos 'DES_LGPLA' 10 'Des S/Bin'      'CHAR' ''  ''      '',
    w_col_pos 'STOCK_CHECK' 01 'Stock Check'  'CHAR' ''  ''      '',
    w_col_pos 'MESSA'     80 'Message'        'CHAR' ''  ''      ''.
ENDFORM.                    " build_fieldcat

*&---------------------------------------------------------------------*
*&      Form  build_sortcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_sortcat.
**--- &1 : position     &2 : field name     &3 : tab name
**--- &4 : up           &5 : sub total
  append_sortcat : '1' 'LOGNO_H' 'IT_ITAB' 'X' '',
                   '2' 'MATNR'   'IT_ITAB' 'X' ''.
ENDFORM.                    " build_sortcat

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
*---
  PERFORM make_message_type_ranges.

  CLEAR : it_itab, it_itab[].

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_itab
           FROM ztmm_nstl_log
          WHERE ernam     IN s_ernam
            AND erdat     IN s_erdat
            AND erzet     IN s_erzet
            AND msgty     IN r_msgty
            AND matnr     IN s_matnr
            AND tanum     IN s_tanum
            AND sdate     IN s_sdate
            AND stime     IN s_stime
            AND dispo     IN s_dispo
            AND feedr     IN s_feedr.

*---
  IF p_kquit NE space.
    DELETE it_itab WHERE tanum EQ space.
    LOOP AT it_itab.
      CLEAR : ltak.
      SELECT SINGLE kquit INTO ltak-kquit
                          FROM ltak
                         WHERE tanum EQ it_itab-tanum
                           AND kquit NE space.
      IF sy-subrc NE 0.
        DELETE it_itab.
      ENDIF.
    ENDLOOP.
  ENDIF.

*---
  DATA : l_tabix LIKE sy-tabix.

  LOOP AT it_itab.     " WHERE msgty NE space.
    MOVE : sy-tabix TO l_tabix.
    IF it_itab-msgty EQ 'E'.
      MOVE : c_red               TO it_itab-linecolor.
    ELSEIF it_itab-msgty EQ 'S'.
      MOVE : c_green             TO it_itab-linecolor.
    ENDIF.
    IF it_itab-cancl EQ 'X'.
      MOVE : c_yell              TO it_itab-linecolor.
    ENDIF.
    PERFORM get_material_desc USING it_itab-matnr.
    MOVE : makt-maktx TO it_itab-maktx.
    MODIFY it_itab INDEX l_tabix.
  ENDLOOP.
ENDFORM.                    " get_data

*---------------------------------------------------------------------*
*       FORM set_status                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  EXTAB                                                         *
*---------------------------------------------------------------------*
FORM set_status USING extab TYPE slis_t_extab.
*---
  SET PF-STATUS 'BASE'.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM user_command                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  UCOMM                                                         *
*  -->  SELFIELD                                                      *
*---------------------------------------------------------------------*
FORM user_command USING ucomm LIKE sy-ucomm
                       selfield TYPE slis_selfield.
*---
  DATA : l_tanum LIKE ltak-tanum,
         l_answer(1).

  READ TABLE it_itab INDEX selfield-tabindex.

  CASE ucomm.
    WHEN 'EXEC'.
      READ TABLE it_itab WITH KEY mark = 'X'.
      IF sy-subrc EQ 0.
        PERFORM confirm_step USING l_answer text-004.
        IF l_answer EQ 'J'.
          MOVE : 'X' TO selfield-refresh.
          PERFORM create_transfer_order.
        ENDIF.
      ELSE.
        MESSAGE e999 WITH text-m03.
      ENDIF.
    WHEN 'CANCEL'.
      READ TABLE it_itab WITH KEY mark = 'X'.
      IF sy-subrc EQ 0.
        PERFORM confirm_step USING l_answer text-005.
        IF l_answer EQ 'J'.
          MOVE : 'X' TO selfield-refresh.
          PERFORM cancel_transfer_order.
        ENDIF.
      ELSE.
        MESSAGE e999 WITH text-m03.
      ENDIF.
    WHEN '&IC1'.
      CHECK sy-subrc EQ 0.
      CLEAR : l_tanum.
      MOVE : it_itab-tanum TO l_tanum.
      SET PARAMETER ID 'TAN' FIELD l_tanum.
      SET PARAMETER ID 'LGN' FIELD 'P01'.
      CALL TRANSACTION 'LT21' AND SKIP FIRST SCREEN.
      CLEAR : it_itab.
  ENDCASE.
ENDFORM.                    "user_command

*&---------------------------------------------------------------------*
*&      Form  confirm_step
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_ANSWER  text
*----------------------------------------------------------------------*
FORM confirm_step USING    p_l_answer
                           p_text.
*---
  CLEAR : p_l_answer.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      defaultoption        = 'Y'
      textline1            = p_text
*     TEXTLINE2            = ' '
      titel                = text-003
      start_column         = 25
      start_row            = 6
      cancel_display       = 'X'
    IMPORTING
      answer               = p_l_answer.
ENDFORM.                    " confirm_step

*&---------------------------------------------------------------------*
*&      Form  create_transfer_order
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_transfer_order.
*---
  DATA : lv_bwlvs_002     TYPE bdcdata-fval,   "Movement type
         lv_matnr_003     TYPE bdcdata-fval,
         lv_anfme_004     TYPE bdcdata-fval,
         lv_anfme_007     TYPE bdcdata-fval,
         lv_altme_008     TYPE bdcdata-fval,
         lv_vltyp_009     TYPE bdcdata-fval,
         lv_vlpla_010     TYPE bdcdata-fval,
         lv_nltyp_011     TYPE bdcdata-fval,
         lv_nlpla_012     TYPE bdcdata-fval,
         lv_refnr_013     TYPE bdcdata-fval.   "Group(Feeder)

  DATA : lv_tanum_001     TYPE bdcdata-fval,  "TO number
         lv_lgnum_002     TYPE bdcdata-fval,  "Warehouse number
         lv_stdat_003     TYPE bdcdata-fval,  "Start date
         lv_stuzt_004     TYPE bdcdata-fval,  "Start time
         lv_endat_005     TYPE bdcdata-fval,  "End date
         lv_enuzt_006     TYPE bdcdata-fval.  "End time

  DATA : l_subrc LIKE sy-subrc,
         l_tabix LIKE sy-tabix,
         l_messa(80).

  CLEAR : it_message, it_message[].

*---
  LOOP AT it_itab WHERE mark NE space
                    AND msgty EQ 'E'
                    AND ( tanum IS initial AND cancl EQ space
                       OR NOT tanum IS initial AND cancl NE space ).
    MOVE : sy-tabix TO l_tabix.

    CLEAR : it_mess, it_mess[].

    IF it_itab-stats NE 'H'.     " except header change error item

      lv_bwlvs_002 = '850'.
      lv_refnr_013 =  it_itab-feedr. "Group(Feeder)
      lv_matnr_003  = it_itab-matnr. "Material '327003K100'
      lv_anfme_004  = it_itab-tqty.
      lv_anfme_007  = it_itab-tqty.
      lv_altme_008  = it_itab-meins.
      lv_vltyp_009  = it_itab-src_lgtyp. "Src Storage Type
      lv_vlpla_010  = it_itab-src_lgpla. "Src Storage Bin
      lv_nltyp_011  = it_itab-des_lgtyp. "Des Storage Type
      lv_nlpla_012  = it_itab-des_lgpla. "Des Storage Bin

      CONDENSE : lv_bwlvs_002,  "Movement type
                 lv_matnr_003,
                 lv_anfme_004,
                 lv_anfme_007,
                 lv_altme_008,
                 lv_vltyp_009,
                 lv_vlpla_010,
                 lv_nltyp_011,
                 lv_nlpla_012,
                 lv_refnr_013.

*--- BDC for LT01(Create TO)
      CALL FUNCTION 'Z_FMM_6012_01'
           EXPORTING
                lgnum_001 = 'P01'  "Warehouse number
                refnr_013 = lv_refnr_013  "Group(Feeder)
                bwlvs_002 = lv_bwlvs_002  "Movement type '999'
                matnr_003 = lv_matnr_003  "Material '327003K100'
                anfme_004 = lv_anfme_004
                werks_005 = 'P001'  "Plant
                lgort_006 = 'P400'  "Storage Location
                anfme_007 = lv_anfme_007
                altme_008 = lv_altme_008
                vltyp_009 = lv_vltyp_009  "Src Storage Type '434'
                vlpla_010 = lv_vlpla_010  "Src Storage Bin 'AA-01-11'
                nltyp_011 = lv_nltyp_011  "Des Storage Type '443'
                nlpla_012 = lv_nlpla_012  "Des Storage Bin 'TS-01'
           IMPORTING
                subrc     = l_subrc
           TABLES
                messtab   = it_mess[].

      APPEND LINES OF it_mess TO it_message.

    ENDIF.

    IF l_subrc EQ 0.
      CLEAR : it_mess, l_subrc.
      READ TABLE it_mess WITH KEY msgtyp = 'S'.
      IF sy-subrc EQ 0.
        lv_tanum_001 = it_mess-msgv1.
        lv_lgnum_002 = 'P01'.
        WRITE : it_itab-sdate TO lv_stdat_003,
                it_itab-edate TO lv_endat_005.
        lv_stuzt_004 = it_itab-stime.
        lv_enuzt_006 = it_itab-etime.

        CONDENSE : lv_tanum_001,
                   lv_lgnum_002,
                   lv_stdat_003,
                   lv_stuzt_004,
                   lv_endat_005,
                   lv_enuzt_006.

*--- BDC for LTA1(Change TO Header)
        CALL FUNCTION 'Z_FMM_6012_02'
             EXPORTING
                  tanum_001 = lv_tanum_001
                  lgnum_002 = lv_lgnum_002
                  stdat_003 = lv_stdat_003
                  stuzt_004 = lv_stuzt_004
                  endat_005 = lv_endat_005
                  enuzt_006 = lv_enuzt_006
             IMPORTING
                  subrc     = l_subrc
             TABLES
                  messtab   = it_mess[].

        IF it_mess[] IS INITIAL.     " success
          CLEAR : it_mess.
          it_mess-tcode   = 'LT1A'.
          it_mess-msgtyp  = 'S'.  "SUCCESS
          it_mess-msgspra = 'E'.
          it_mess-msgid   = 'ZMMM'.
          it_mess-msgnr   = '999'.
          it_mess-msgv1   = 'Transfer order'.
          it_mess-msgv2   = lv_tanum_001.
          it_mess-msgv3   = 'Start/End Date/Time'.
          it_mess-msgv4   = 'is changed.'.
          APPEND it_mess.
        ENDIF.

        IF l_subrc EQ 0.
          CLEAR : it_mess.
          READ TABLE it_mess INDEX 1.
          MOVE : it_mess-msgv2 TO it_itab-tanum.
          MOVE : 'S'           TO it_itab-msgty,
                 it_mess-msgv1 TO it_itab-tanum.
          MOVE : 'C'           TO it_itab-stats,
                 space         TO it_itab-cancl.
        ELSE.
        ENDIF.
      ENDIF.

      CLEAR : it_mess, l_messa.
      READ TABLE it_mess WITH KEY msgtyp = 'E'.
      IF sy-subrc EQ 0.
        PERFORM get_message USING    it_mess-msgid
                                     it_mess-msgnr
                                     it_mess-msgv1
                                     it_mess-msgv2
                                     it_mess-msgv3
                                     it_mess-msgv4
                            CHANGING l_messa.
      ELSE.
        READ TABLE it_mess WITH KEY msgtyp = 'S'.
        IF sy-subrc EQ 0.
          PERFORM get_message USING    it_mess-msgid
                                       it_mess-msgnr
                                       it_mess-msgv1
                                       it_mess-msgv2
                                       it_mess-msgv3
                                       it_mess-msgv4
                              CHANGING l_messa.
        ENDIF.
      ENDIF.

    ELSE.
      CLEAR : it_mess, l_messa.
      READ TABLE it_mess WITH KEY msgtyp = 'E'.
      IF sy-subrc EQ 0.
        PERFORM get_message USING    it_mess-msgid
                                     it_mess-msgnr
                                     it_mess-msgv1
                                     it_mess-msgv2
                                     it_mess-msgv3
                                     it_mess-msgv4
                            CHANGING l_messa.
      ELSE.
        READ TABLE it_mess WITH KEY msgtyp = 'S'.
        IF sy-subrc EQ 0.
          PERFORM get_message USING    it_mess-msgid
                                       it_mess-msgnr
                                       it_mess-msgv1
                                       it_mess-msgv2
                                       it_mess-msgv3
                                       it_mess-msgv4
                              CHANGING l_messa.
        ENDIF.
      ENDIF.
    ENDIF.

    MOVE : l_messa       TO it_itab-messa,
           it_mess-msgid TO it_itab-msgid,
           it_mess-msgnr TO it_itab-msgnr.

    it_itab-aenam = sy-uname.
    it_itab-aedat = sy-datum.
    it_itab-aezet = sy-uzeit.

    MODIFY it_itab INDEX l_tabix.

    CLEAR : ztmm_nstl_log.
    MOVE-CORRESPONDING it_itab TO ztmm_nstl_log.
    MODIFY ztmm_nstl_log.
    MESSAGE s999 WITH l_messa.
  ENDLOOP.
ENDFORM.                    " create_transfer_order

*&---------------------------------------------------------------------*
*&      Form  make_message_type_ranges
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_message_type_ranges.
*---
  CLEAR : r_msgty, r_msgty[].

  IF p_succe NE space.
    MOVE : 'I'     TO r_msgty-sign,
           'EQ'    TO r_msgty-option,
           'S'     TO r_msgty-low.
    APPEND r_msgty.
  ENDIF.

  IF p_error NE space.
    MOVE : 'I'     TO r_msgty-sign,
           'EQ'    TO r_msgty-option,
           'E'     TO r_msgty-low.
    APPEND r_msgty.
  ENDIF.

*  IF p_space NE space.
*    MOVE : 'I'     TO r_msgty-sign,
*           'EQ'    TO r_msgty-option,
*           ' '     TO r_msgty-low.
*    APPEND r_msgty.
*  ENDIF.
ENDFORM.                    " make_message_type_ranges

*&---------------------------------------------------------------------*
*&      Form  cancel_transfer_order
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cancel_transfer_order.
*---
  DATA : l_messa(100),
         l_tabix LIKE sy-tabix.

  LOOP AT it_itab WHERE mark NE space
                    AND msgty EQ 'S'
                    AND tanum NE space.

    MOVE : sy-tabix TO l_tabix.

    CLEAR : it_bdc, it_bdc[], l_messa, ltak.

*    SELECT SINGLE kquit INTO ltak-kquit
*                        FROM ltak
*                       WHERE tanum EQ it_itab-tanum
*                         AND kquit NE space.
*
*    IF sy-subrc EQ 0.
*
*      MESSAGE s999 WITH text-m04 it_itab-tanum.
*
*    ELSE.

    PERFORM dynpro USING : 'X'  'SAPML03T'          '0118',
                           ' '  'LTAK-TANUM'        it_itab-tanum,
                           ' '  'LTAK-LGNUM'        'P01',
                           ' '  'RL03T-RHELL'       space,
                           ' '  'RL03T-RDNKL'       'X',
                           ' '  'BDC_OKCODE'        '/00'.

    CALL TRANSACTION 'LT15' USING it_bdc
                            MODE w_mode
                            UPDATE 'S'
                            MESSAGES INTO it_mess.

    APPEND LINES OF it_mess TO it_message.

    READ TABLE it_mess INDEX 1.

    PERFORM get_message USING    it_mess-msgid
                                 it_mess-msgnr
                                 it_mess-msgv1
                                 it_mess-msgv2
                                 it_mess-msgv3
                                 it_mess-msgv4
                        CHANGING l_messa.

    READ TABLE it_mess WITH KEY msgtyp = 'S'.
    IF sy-subrc EQ 0.
      MOVE : 'E'           TO it_itab-msgty,
             space         TO it_itab-stats,
             'X'           TO it_itab-cancl.
      MOVE : l_messa       TO it_itab-messa,
             it_mess-msgid TO it_itab-msgid,
             it_mess-msgnr TO it_itab-msgnr.
      it_itab-aenam = sy-uname.
      it_itab-aedat = sy-datum.
      it_itab-aezet = sy-uzeit.
    ENDIF.

    MODIFY it_itab INDEX l_tabix.

    CLEAR : ztmm_nstl_log.

    MOVE-CORRESPONDING it_itab TO ztmm_nstl_log.
    MODIFY ztmm_nstl_log.

    MESSAGE s999 WITH l_messa.

*    ENDIF.
  ENDLOOP.
ENDFORM.                    " cancel_transfer_order

*&---------------------------------------------------------------------*
*&      Form  dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1117   text
*      -->P_1118   text
*      -->P_1119   text
*----------------------------------------------------------------------*
FORM dynpro USING    dynbegin
                     name
                     value.
*---
  IF dynbegin = 'X'.
    CLEAR : it_bdc.
    MOVE : name  TO it_bdc-program,
           value TO it_bdc-dynpro,
           'X'   TO it_bdc-dynbegin.
    APPEND it_bdc.
  ELSE .
    CLEAR : it_bdc.
    MOVE : name  TO it_bdc-fnam,
           value TO it_bdc-fval.
    APPEND it_bdc.
  ENDIF.
ENDFORM.                    " dynpro

*&---------------------------------------------------------------------*
*&      Form  get_message
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_MESS_MSGID  text
*      -->P_IT_MESS_MSGNR  text
*      -->P_IT_MESS_MSGV1  text
*      -->P_IT_MESS_MSGV2  text
*      -->P_IT_MESS_MSGV3  text
*      -->P_IT_MESS_MSGV4  text
*      <--P_L_MESSA  text
*----------------------------------------------------------------------*
FORM get_message USING    p_msgid
                          p_msgnr
                          p_msgv1
                          p_msgv2
                          p_msgv3
                          p_msgv4
                 CHANGING p_l_messa.
*---
  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
       EXPORTING
            msgid               = p_msgid
            msgnr               = p_msgnr
            msgv1               = p_msgv1
            msgv2               = p_msgv2
            msgv3               = p_msgv3
            msgv4               = p_msgv4
       IMPORTING
            message_text_output = p_l_messa.
ENDFORM.                    " get_message
