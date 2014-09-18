************************************************************************
* Program Name      : ZRMMPM02R_DAGR
* Author            : Sung-Tae, Lim
* Creation Date     : 2003.08.11.
* Specifications By : Sung-Tae, Lim
* Pattern           : Report 1-1
* Development Request No : UD1K901864
* Addl Documentation:
* Description       : Daily Goods Receipt List
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2003.08.11.     Sung-Tae Lim     UD1K901864     Initial Coding
*
*
************************************************************************

REPORT zrmmpm02r_dagr NO STANDARD PAGE HEADING
                      LINE-SIZE 132
                      LINE-COUNT 64(1)
                      MESSAGE-ID zmmm.

**---
INCLUDE : zrmmpmxxr_incl.

DATA : BEGIN OF it_itab OCCURS 0,
         matnr LIKE mara-matnr,     " material
         maktx LIKE makt-maktx,     " material desc.
         profl LIKE mara-profl,     " KD/LP
         dispo LIKE marc-dispo,     " manager(MRP controller)
         mblnr LIKE mseg-mblnr,     " GR Doc. No.
         budat LIKE mkpf-budat,     " GR Date
         lifnr LIKE lfa1-lifnr,     " Vendor
         name1 LIKE lfa1-name1,     " Vendor Desc.
         sakto LIKE mseg-sakto,     " account
         bwart LIKE mseg-bwart,     " Movement type
         grund LIKE mseg-grund,     " Reason Code
         char2(8) TYPE n,
         menge LIKE mseg-menge,     " quantity
         meins LIKE mseg-meins,     " unit of measure
         dmbtr LIKE mseg-dmbtr,     " amount
         waers LIKE mseg-waers,     " currency key
         ebeln LIKE mseg-ebeln,     " PO No.
         ebelp LIKE mseg-ebelp,     " PO Item No.
         eindt LIKE eket-eindt,     " Delivery Date
         menge1 LIKE mseg-menge,     " PO Quantity(Schedule Line)
         usnam LIKE mkpf-usnam,     " Created by
         cpudt LIKE mkpf-cpudt,     " document entry date
         cputm LIKE mkpf-cputm,     " entry time
       END OF it_itab.

DATA : BEGIN OF it_temp OCCURS 0.
        INCLUDE STRUCTURE it_itab.
DATA :   werks LIKE mseg-werks,
       END OF it_temp.

**--- Variables
RANGES : r_budat FOR mkpf-budat.

DATA : w_matnr_s LIKE mara-matnr,
       w_matnr_e LIKE mara-matnr,
       w_mtart_s LIKE mara-mtart,
       w_mtart_e LIKE mara-mtart,
       w_werks_s LIKE ekpo-werks,
       w_werks_e LIKE ekpo-werks,
       w_lgort_s LIKE ekpo-lgort,
       w_lgort_e LIKE ekpo-lgort,
       w_dispo_s LIKE marc-dispo,
       w_dispo_e LIKE marc-dispo,
       w_lifnr_s LIKE lfa1-lifnr,
       w_lifnr_e LIKE lfa1-lifnr,
       w_sakto_s LIKE mseg-sakto,
       w_sakto_e LIKE mseg-sakto,
       w_profl_s LIKE mara-profl,
       w_profl_e LIKE mara-profl,
       w_bwart_s LIKE mseg-bwart,
       w_bwart_e LIKE mseg-bwart,
       w_grund_s LIKE mseg-grund,
       w_grund_e LIKE mseg-grund,
       w_budat_s LIKE mkpf-budat,
       w_budat_e LIKE mkpf-budat.

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
*  w_fieldcat-key        = &6.
  w_fieldcat-do_sum     = &6.
  w_fieldcat-qfieldname = &7.
  w_fieldcat-cfieldname = &8.
  w_fieldcat-no_out     = &9.
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
SELECT-OPTIONS : s_matnr FOR mara-matnr,
                 s_mtart FOR mara-mtart DEFAULT 'ROH',
                 s_werks FOR mseg-werks OBLIGATORY,
                 s_lgort FOR mseg-lgort OBLIGATORY,
                 s_dispo FOR marc-dispo,
                 s_lifnr FOR lfa1-lifnr,
                 s_sakto FOR mseg-sakto,
                 s_profl FOR mara-profl,
                 s_bwart FOR mseg-bwart,
                 s_grund FOR mseg-grund,
                 s_budat FOR mkpf-budat OBLIGATORY.
*                 s_spmon FOR s031-spmon NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK block1.

**---
AT SELECTION-SCREEN.
  PERFORM check_input_value.

***---
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_spmon-low.
*  PERFORM help_spmon.
*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_spmon-high.
*  PERFORM help_spmon.

**---
INITIALIZATION.
  PERFORM event_build USING w_eventcat[].
  PERFORM set_init.

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
    PERFORM comment_build.     " USING w_top_of_page[].
    PERFORM make_alv_grid.
  ENDIF.



**---

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
***---
*  PERFORM calcu_month_to_date.

**---
  CLEAR : it_itab, it_itab[], it_temp, it_temp[].

***---
*  PERFORM make_select_options.

**---
  SELECT b~matnr  maktx    profl    dispo    b~mblnr
         budat    b~lifnr  name1    sakto    bwart
         grund    menge    b~meins  dmbtr    waers
         ebeln    ebelp    b~werks  usnam    cpudt
         cputm
                 INTO CORRESPONDING FIELDS OF TABLE it_temp
                 FROM mkpf AS a INNER JOIN mseg AS b
                   ON b~mandt EQ a~mandt
                  AND b~mblnr EQ a~mblnr
                  AND b~mjahr EQ a~mjahr
                      INNER JOIN marc AS c
                         ON c~mandt EQ b~mandt
                        AND c~matnr EQ b~matnr
                        AND c~werks EQ b~werks
                            INNER JOIN mara AS d
                               ON d~mandt EQ c~mandt
                              AND d~matnr EQ c~matnr
                                  INNER JOIN makt AS e
                                     ON e~mandt EQ d~mandt
                                    AND e~matnr EQ d~matnr
                                    AND e~spras EQ sy-langu
                                        INNER JOIN lfa1 AS f
                                           ON f~mandt EQ b~mandt
                                          AND f~lifnr EQ b~lifnr
                WHERE d~matnr IN s_matnr
                  AND mtart IN s_mtart
                  AND b~werks IN s_werks
                  AND lgort IN s_lgort
                  AND dispo IN s_dispo
                  AND f~lifnr IN s_lifnr
                  AND sakto IN s_sakto
                  AND bwart IN s_bwart
                  AND ( bwart EQ '101' OR
                        bwart EQ '102' )      " only GR with PO
                  AND grund IN s_grund
                  AND budat IN s_budat.
*                  AND budat IN r_budat.

  LOOP AT it_temp.
*--- move it_temp to it_itab
    MOVE-CORRESPONDING it_temp TO it_itab.
*--- get Delivery Date & PO Quantity
    PERFORM get_po_info.
*---
    IF it_itab-bwart EQ '102'.
      it_itab-menge = it_itab-menge * -1.
      it_itab-dmbtr = it_itab-dmbtr * -1.
    ENDIF.
*--- append it_itab
    APPEND it_itab.
    CLEAR : it_temp, it_itab.
  ENDLOOP.
ENDFORM.                    " get_data

*&---------------------------------------------------------------------*
*&      Form  comment_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_TOP_OF_PAGE[]  text
*----------------------------------------------------------------------*
FORM comment_build.  " USING    p_w_top_of_page TYPE slis_t_listheader.
**---
  CLEAR : w_line.
  w_line-typ  = 'H'.
  w_line-info = text-002.
  APPEND w_line TO w_top_of_page.

  CLEAR : w_line.
  APPEND INITIAL LINE TO w_top_of_page.

  append_top :
      'S' text-003 s_matnr-low s_matnr-high,
      'S' text-004 s_mtart-low s_mtart-high,
      'S' text-005 s_werks-low s_werks-high,
      'S' text-006 s_lgort-low s_lgort-high,
      'S' text-007 s_dispo-low s_dispo-high,
      'S' text-008 s_lifnr-low s_lifnr-high,
      'S' text-009 s_sakto-low s_sakto-high,
      'S' text-010 s_bwart-low s_bwart-high,
      'S' text-011 s_profl-low s_profl-high,
      'S' text-012 s_grund-low s_grund-high,
      'S' text-013 s_budat-low s_budat-high.

*  CLEAR : w_line.
*  w_line-typ  = 'S'.
*  w_line-key  = text-003.
*  MOVE : s_matnr-low  TO w_line-info(18),
*         'to'         TO w_line-info+19(2),
*         s_matnr-high TO w_line-info+23(18).
*  APPEND w_line TO p_w_top_of_page.

*  CLEAR : w_line.
*  w_line-typ  = 'S'.
*  w_line-key  = text-004.
*  MOVE : s_mtart-low  TO w_line-info(18),
*         'to'         TO w_line-info+19(2),
*         s_mtart-high TO w_line-info+23(18).
*  APPEND w_line TO p_w_top_of_page.
*
*  CLEAR : w_line.
*  w_line-typ  = 'S'.
*  w_line-key  = text-005.
*  MOVE : s_werks-low  TO w_line-info(18),
*         'to'         TO w_line-info+19(2),
*         s_werks-high TO w_line-info+23(18).
*  APPEND w_line TO p_w_top_of_page.
*
*  CLEAR : w_line.
*  w_line-typ  = 'S'.
*  w_line-key  = text-006.
*  MOVE : s_lgort-low  TO w_line-info(18),
*         'to'         TO w_line-info+19(2),
*         s_lgort-high TO w_line-info+23(18).
*  APPEND w_line TO p_w_top_of_page.
*
*  CLEAR : w_line.
*  w_line-typ  = 'S'.
*  w_line-key  = text-007.
*  MOVE : s_dispo-low  TO w_line-info(18),
*         'to'         TO w_line-info+19(2),
*         s_dispo-high TO w_line-info+23(18).
*  APPEND w_line TO p_w_top_of_page.
*
*  CLEAR : w_line.
*  w_line-typ  = 'S'.
*  w_line-key  = text-008.
*  MOVE : s_lifnr-low  TO w_line-info(18),
*         'to'         TO w_line-info+19(2),
*         s_lifnr-high TO w_line-info+23(18).
*  APPEND w_line TO p_w_top_of_page.
*
*  CLEAR : w_line.
*  w_line-typ  = 'S'.
*  w_line-key  = text-009.
*  MOVE : s_sakto-low  TO w_line-info(18),
*         'to'         TO w_line-info+19(2),
*         s_sakto-high TO w_line-info+23(18).
*  APPEND w_line TO p_w_top_of_page.
*
*  CLEAR : w_line.
*  w_line-typ  = 'S'.
*  w_line-key  = text-010.
**  move : KD/LP        to w_line-info(18),
**         'to'         to w_line-info+19(2),
**         KD/LP        to w_line-info+23(18).
*  APPEND w_line TO p_w_top_of_page.
*
*  CLEAR : w_line.
*  w_line-typ  = 'S'.
*  w_line-key  = text-011.
*  MOVE : s_bwart-low  TO w_line-info(18),
*         'to'         TO w_line-info+19(2),
*         s_bwart-high TO w_line-info+23(18).
*  APPEND w_line TO p_w_top_of_page.
*
*  CLEAR : w_line.
*  w_line-typ  = 'S'.
*  w_line-key  = text-012.
*  MOVE : s_grund-low  TO w_line-info(18),
*         'to'         TO w_line-info+19(2),
*         s_grund-high TO w_line-info+23(18).
*  APPEND w_line TO p_w_top_of_page.
*
*  CLEAR : w_line.
*  w_line-typ  = 'S'.
*  w_line-key  = text-013.
*  MOVE : s_budat-low  TO w_line-info(18),
*         'to'         TO w_line-info+19(2),
*         s_budat-high TO w_line-info+23(18).
*  APPEND w_line TO p_w_top_of_page.
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
**---
  PERFORM build_fieldcat.
  PERFORM build_sortcat.

  CLEAR : w_program.

  MOVE : sy-repid TO w_program.

  w_layout-colwidth_optimize = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program = w_program
            is_layout          = w_layout
            it_fieldcat        = w_fieldcat[]
            it_events          = w_eventcat[]
            it_sort            = w_sortcat[]
            i_save             = 'A'
       TABLES
            t_outtab           = it_itab
       EXCEPTIONS
            program_error      = 1
            OTHERS             = 2.
ENDFORM.                    " make_alv_grid

*&---------------------------------------------------------------------*
*&      Form  help_spmon
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0145   text
*----------------------------------------------------------------------*
FORM help_spmon.
**--- This Subroutine copied from Include program 'RMCS0F0M'

  DATA: BEGIN OF mf_dynpfields OCCURS 1.
          INCLUDE STRUCTURE dynpread.
  DATA: END   OF mf_dynpfields.
  DATA: mf_returncode   LIKE sy-subrc,
        mf_monat        LIKE isellist-month,
        mf_hlp_repid    LIKE sy-repid.
  FIELD-SYMBOLS: <mf_feld>.

* Wert von Dynpro lesen
  GET CURSOR FIELD mf_dynpfields-fieldname.
  APPEND mf_dynpfields.
  mf_hlp_repid = sy-repid.
  DO 2 TIMES.
    CALL FUNCTION 'DYNP_VALUES_READ'
         EXPORTING
              dyname               = mf_hlp_repid
              dynumb               = sy-dynnr
         TABLES
              dynpfields           = mf_dynpfields
         EXCEPTIONS
              invalid_abapworkarea = 01
              invalid_dynprofield  = 02
              invalid_dynproname   = 03
              invalid_dynpronummer = 04
              invalid_request      = 05
              no_fielddescription  = 06
              undefind_error       = 07.
    IF sy-subrc = 3.
*     Aktuelles Dynpro ist Wertemengenbild
      mf_hlp_repid = 'SAPLALDB'.
    ELSE.
      READ TABLE mf_dynpfields INDEX 1.
*     Unterstriche durch Blanks ersetzen
      TRANSLATE mf_dynpfields-fieldvalue USING '_ '.
      EXIT.
    ENDIF.
  ENDDO.
  IF sy-subrc = 0.
*   Konvertierung ins interne Format
    CALL FUNCTION 'CONVERSION_EXIT_PERI_INPUT'
         EXPORTING
              input         = mf_dynpfields-fieldvalue
         IMPORTING
              output        = mf_monat
         EXCEPTIONS
              error_message = 1.
    IF mf_monat IS INITIAL.
*     Monat ist initial => Vorschlagswert aus akt. Datum ableiten
      mf_monat = sy-datlo(6).
    ENDIF.
    CALL FUNCTION 'POPUP_TO_SELECT_MONTH'
         EXPORTING
              actual_month               = mf_monat
         IMPORTING
              selected_month             = mf_monat
              return_code                = mf_returncode
         EXCEPTIONS
              factory_calendar_not_found = 01
              holiday_calendar_not_found = 02
              month_not_found            = 03.
    IF sy-subrc = 0 AND mf_returncode = 0.
*     ASSIGN (MF_DYNPFIELDS-FIELDNAME) TO <MF_FELD>. " ==>> note 148804
*     <MF_FELD> = MF_MONAT.
      CALL FUNCTION 'CONVERSION_EXIT_PERI_OUTPUT'
           EXPORTING
                input  = mf_monat
           IMPORTING
                output = mf_dynpfields-fieldvalue.
      COLLECT mf_dynpfields.
      CALL FUNCTION 'DYNP_VALUES_UPDATE'
           EXPORTING
                dyname               = mf_hlp_repid
                dynumb               = sy-dynnr
           TABLES
                dynpfields           = mf_dynpfields
           EXCEPTIONS
                invalid_abapworkarea = 01
                invalid_dynprofield  = 02
                invalid_dynproname   = 03
                invalid_dynpronummer = 04
                invalid_request      = 05
                no_fielddescription  = 06
                undefind_error       = 07. "<<== note 148804
    ENDIF.
  ENDIF.
ENDFORM.                    " help_spmon

*&---------------------------------------------------------------------*
*&      Form  calcu_month_to_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calcu_month_to_date.
***---
*  DATA : w_low_begda TYPE d,
*         w_low_endda TYPE d,
*         w_high_begda TYPE d,
*         w_high_endda TYPE d.
*
*  CLEAR : r_budat, r_budat[].
*
*  READ TABLE s_spmon INDEX 1.
*
*  CHECK sy-subrc EQ 0.
*
*  IF s_spmon-low NE space.
*    CALL FUNCTION 'HRWPC_BL_DATES_MONTH_INTERVAL'
*         EXPORTING
*              datum          = s_spmon-low
*         IMPORTING
*              begda          = w_low_begda
*              endda          = w_low_endda
*         EXCEPTIONS
*              invalid_values = 1
*              OTHERS         = 2.
*  ENDIF.
*
*  IF s_spmon-high NE space.
*    CALL FUNCTION 'HRWPC_BL_DATES_MONTH_INTERVAL'
*         EXPORTING
*              datum          = s_spmon-high
*         IMPORTING
*              begda          = w_high_begda
*              endda          = w_high_endda
*         EXCEPTIONS
*              invalid_values = 1
*              OTHERS         = 2.
*  ENDIF.
*
**---
*  MOVE : 'I'         TO r_budat-sign,
*         'BT'        TO r_budat-option.
*
*  IF     s_spmon-low NE space AND s_spmon-high NE space.
*    MOVE : w_low_begda TO r_budat-low,
*           w_high_endda TO r_budat-high.
*  ELSEIF s_spmon-low NE space AND s_spmon-high EQ space.
*    MOVE : w_low_begda TO r_budat-low,
*           w_low_endda TO r_budat-high.
*  ELSEIF s_spmon-low EQ space AND s_spmon-high NE space.
*    MOVE : w_high_endda TO r_budat-high.
*  ENDIF.
*
*  APPEND r_budat.
ENDFORM.                    " calcu_month_to_date

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
**--- &7 : qty field      &8 : cur field        &9 : no out
  append_fieldcat :
   w_col_pos 'MATNR' 18 'Material'       'CHAR' ''  ''      '' '',
   w_col_pos 'MAKTX' 30 'Material Desc.' 'CHAR' ''  ''      '' '',
   w_col_pos 'PROFL'  5 'KD/LP'          'CHAR' ''  ''      '' '',
   w_col_pos 'DISPO'  3 'POC'            'CHAR' ''  ''      '' '',
   w_col_pos 'MBLNR' 10 'GR No.'         'CHAR' ''  ''      '' '',
   w_col_pos 'BUDAT' 10 'GR Date'        'DATS' ''  ''      '' '',
   w_col_pos 'LIFNR' 10 'Vendor'         'CHAR' ''  ''      '' '',
   w_col_pos 'NAME1' 20 'Vendor Desc.'   'CHAR' ''  ''      '' '',
   w_col_pos 'SAKTO' 10 'Account'        'CHAR' ''  ''      '' '',
   w_col_pos 'BWART'  3 'Mvt'            'CHAR' ''  ''      '' '',
   w_col_pos 'GRUND'  4 'Reason Code'    'NUMC' ''  ''      '' '',
   w_col_pos 'CHAR2'  8 'Seq. No.'       'NUMC' ''  ''      '' '',
   w_col_pos 'MENGE' 15 'Quantity'       'QUAN' 'X'  'MEINS' '' '',
   w_col_pos 'MEINS'  3 'UoM'            'UNIT' ''  ''      '' '',
   w_col_pos 'DMBTR' 15 'Amount'         'CURR' 'X'  ''      'WAERS' '',
   w_col_pos 'WAERS'  3 'Cur'            'CUKY' ''  ''      '' '',
   w_col_pos 'EBELN' 10 'PO No.'         'CHAR' ''  ''      '' '',
   w_col_pos 'EINDT' 10 'Del. Date'      'DATS' ''  ''      '' '',
   w_col_pos 'MENGE1' 15 'PO Qty.'        'QUAN' 'X'  'MEINS' '' '',
   w_col_pos 'USNAM' 12 'Created by'     'CHAR' ''  ''      '' '',
   w_col_pos 'CPUDT' 10 'Entry Date'     'DATS' ''  ''      '' 'X',
   w_col_pos 'CPUTM'  8 'Entry Time'     'TIMS' ''  ''      '' 'X'.
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
**--- &1 : position       &2 : field name       &3 : tab name
**--- &4 : up
  IF s_budat-high IS INITIAL.
    append_sortcat : '1' 'USNAM' 'IT_ITAB' 'X' '',
                     '2' 'CPUDT' 'IT_ITAB' 'X' '',
                     '3' 'CPUTM' 'IT_ITAB' 'X' '',
                     '4' 'MATNR' 'IT_ITAB' 'X' ''.
  ELSE.
    IF NOT s_lifnr[] IS INITIAL.
      append_sortcat : '1' 'LIFNR' 'IT_ITAB' 'X' 'X',
                       '2' 'MATNR' 'IT_ITAB' 'X' 'X',
                       '3' 'CPUDT' 'IT_ITAB' 'X' '',
                       '4' 'CPUTM' 'IT_ITAB' 'X' ''.
    ELSEIF NOT s_dispo[] IS INITIAL.
      append_sortcat : '1' 'DISPO' 'IT_ITAB' 'X' 'X',
                       '2' 'MATNR' 'IT_ITAB' 'X' 'X',
                       '3' 'CPUDT' 'IT_ITAB' 'X' '',
                       '4' 'CPUTM' 'IT_ITAB' 'X' ''.
    ENDIF.
  ENDIF.
ENDFORM.                    " build_sortcat

*&---------------------------------------------------------------------*
*&      Form  check_input_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_input_value.
**---
  CHECK NOT s_budat-high IS INITIAL.

  IF s_lifnr[] IS INITIAL AND s_dispo[] IS INITIAL.
    MESSAGE e999 WITH text-m02.
  ENDIF.

  IF NOT s_lifnr[] IS INITIAL AND NOT s_dispo[] IS INITIAL.
    MESSAGE e999 WITH text-m03.
  ENDIF.
ENDFORM.                    " check_input_value

*&---------------------------------------------------------------------*
*&      Form  set_init
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_init.
*---
  MOVE : 'I'      TO s_budat-sign,
         'EQ'     TO s_budat-option,
         sy-datum TO s_budat-low.

  APPEND s_budat.
ENDFORM.                    " set_init

*&---------------------------------------------------------------------*
*&      Form  make_select_options
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_select_options.
*--- s_matnr
  CALL FUNCTION 'Z_FMM_SELECT_OPTIONS'
       EXPORTING
            i_low  = s_matnr-low
            i_high = s_matnr-high
       IMPORTING
            e_low  = w_matnr_s
            e_high = w_matnr_e.

*--- s_mtart
  CALL FUNCTION 'Z_FMM_SELECT_OPTIONS'
       EXPORTING
            i_low  = s_mtart-low
            i_high = s_mtart-high
       IMPORTING
            e_low  = w_mtart_s
            e_high = w_mtart_e.

*--- s_werks
  CALL FUNCTION 'Z_FMM_SELECT_OPTIONS'
       EXPORTING
            i_low  = s_werks-low
            i_high = s_werks-high
       IMPORTING
            e_low  = w_werks_s
            e_high = w_werks_e.

*--- s_lgort
  CALL FUNCTION 'Z_FMM_SELECT_OPTIONS'
       EXPORTING
            i_low  = s_lgort-low
            i_high = s_lgort-high
       IMPORTING
            e_low  = w_lgort_s
            e_high = w_lgort_e.

*--- s_dispo
  CALL FUNCTION 'Z_FMM_SELECT_OPTIONS'
       EXPORTING
            i_low  = s_dispo-low
            i_high = s_dispo-high
       IMPORTING
            e_low  = w_dispo_s
            e_high = w_dispo_e.

*--- s_lifnr
  CALL FUNCTION 'Z_FMM_SELECT_OPTIONS'
       EXPORTING
            i_low  = s_lifnr-low
            i_high = s_lifnr-high
       IMPORTING
            e_low  = w_lifnr_s
            e_high = w_lifnr_e.

*--- s_sakto
  CALL FUNCTION 'Z_FMM_SELECT_OPTIONS'
       EXPORTING
            i_low  = s_sakto-low
            i_high = s_sakto-high
       IMPORTING
            e_low  = w_sakto_s
            e_high = w_sakto_e.

*--- s_profl
  CALL FUNCTION 'Z_FMM_SELECT_OPTIONS'
       EXPORTING
            i_low  = s_profl-low
            i_high = s_profl-high
       IMPORTING
            e_low  = w_profl_s
            e_high = w_profl_e.

*--- s_bwart
  CALL FUNCTION 'Z_FMM_SELECT_OPTIONS'
       EXPORTING
            i_low  = s_bwart-low
            i_high = s_bwart-high
       IMPORTING
            e_low  = w_bwart_s
            e_high = w_bwart_e.

*--- s_grund
  CALL FUNCTION 'Z_FMM_SELECT_OPTIONS'
       EXPORTING
            i_low  = s_grund-low
            i_high = s_grund-high
       IMPORTING
            e_low  = w_grund_s
            e_high = w_grund_e.

*--- s_budat
  IF s_budat-low IS INITIAL AND s_budat-high IS INITIAL.
    MOVE : '00000000'   TO w_budat_s,
           '99991231'   TO w_budat_e.
  ELSEIF NOT s_budat-low IS INITIAL AND s_budat-high IS INITIAL.
    MOVE : s_budat-low  TO w_budat_s,
           s_budat-low  TO w_budat_e.
  ELSEIF s_budat-low IS INITIAL AND NOT s_budat-high IS INITIAL.
    MOVE : '00000000'   TO w_budat_s,
           s_budat-high TO w_budat_e.
  ELSEIF NOT s_budat-low IS INITIAL AND NOT s_budat-high IS INITIAL.
    MOVE : s_budat-low  TO w_budat_s,
           s_budat-high TO w_budat_e.
  ENDIF.
ENDFORM.                    " make_select_options

*&---------------------------------------------------------------------*
*&      Form  get_po_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_po_info.
*--- if po item has one schedule line, display EKET-EINDT
*--- if po item has many schedule lines, GR date = Inb.Del Date
  DATA : BEGIN OF it_eindt OCCURS 0,
           eindt LIKE eket-eindt,
           menge LIKE eket-menge,
         END OF it_eindt.

  DATA : l_lines TYPE i,
         l_vgpos LIKE lips-vgpos.

  CLEAR : eket, likp, lips, l_lines, it_eindt, it_eindt[].

  SELECT eindt
         menge INTO CORRESPONDING FIELDS OF TABLE it_eindt
               FROM eket
              WHERE ebeln EQ it_itab-ebeln
                AND ebelp EQ it_itab-ebelp.

  DESCRIBE TABLE it_eindt LINES l_lines.

  IF l_lines EQ 1.
    READ TABLE it_eindt INDEX 1.
    MOVE : it_eindt-eindt TO it_itab-eindt,
           it_eindt-menge TO it_itab-menge1.
  ELSE.
    CLEAR : l_vgpos.
    MOVE : it_itab-ebelp TO l_vgpos.
    PERFORM conversion_exit_alpha USING l_vgpos.
    SELECT SINGLE lfimg
                  lfdat INTO (lips-lfimg, likp-lfdat)
                        FROM lips AS a INNER JOIN likp AS b
                          ON a~mandt EQ b~mandt
                         AND a~vbeln EQ b~vbeln
                       WHERE lfdat EQ it_itab-budat
                         AND vgbel EQ it_itab-ebeln
                         AND vgpos EQ l_vgpos.
    IF sy-subrc EQ 0.
      MOVE : likp-lfdat TO it_itab-eindt,
             lips-lfimg TO it_itab-menge1.
    ELSE.
    ENDIF.
  ENDIF.
ENDFORM.                    " get_po_info
