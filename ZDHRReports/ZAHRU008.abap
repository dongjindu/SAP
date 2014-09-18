*----------------------------------------------------------------------
* Program ID        : ZAHRU008
* Title             : [HR] Attendance Verification Summary Report
* Created on        : 4/30/2009
* Created by        : I.G.MOON
* Specifications By : EUNA LEE
* Description       : [HR] Attendance Verification Summary Report
*
* Modification Log
* Date        Developer Issue No    Description
*======================================================================
* 07/31/2012  Valerian  UD1K955283  Modify logic to get the shift code
* 08/16/2012  Valerian  UD1K955419  Fix logic to get the shift code
* 06/19/2013  T00303    UD1K957440  U1: Apply Archiving
*----------------------------------------------------------------------
REPORT zahru008 MESSAGE-ID zmco.

TABLES: pa0001, t526, t527x, *zshrattncor, abdbg, sscrfields,
zthrattncor.

INCLUDE : z_moon_alv_top,
          z_moon_alv_fnc.

INCLUDE <icon>.                        " icon

DATA i_t526 LIKE t526 OCCURS 0 WITH HEADER LINE.
DATA i_t527x LIKE t527x OCCURS 0 WITH HEADER LINE.
DATA i_cskt LIKE cskt OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF con_list OCCURS 0,
          werks LIKE t526-werks,
          sachx LIKE t526-sachx,
          sachn LIKE t526-sachn,
      END OF con_list.

DATA: BEGIN OF it_subarea OCCURS 0,
        werks TYPE persa,
        btrtl	LIKE t001p-btrtl,
        btext LIKE t001p-btext,
      END OF it_subarea.

DATA: dyname         TYPE progname,
      dynumb         TYPE sychar04,
      exc_exctab     TYPE slis_t_extab,
      popup_fieldcat TYPE slis_t_fieldcat_alv,
      f              TYPE slis_fieldcat_alv,
      selfield       TYPE slis_selfield,
      exitfield,
      color_active(3)  VALUE 'C50',
      tabix LIKE sy-tabix.

DATA: BEGIN OF help_field OCCURS 0.
        INCLUDE STRUCTURE help_value.
DATA: END OF help_field.

DATA: BEGIN OF help_vtab OCCURS 0.
        INCLUDE STRUCTURE help_vtab.
DATA: END OF help_vtab.

DATA: BEGIN OF help_value OCCURS 0,
      value LIKE help_vtab-value,
      END OF help_value.

DATA: BEGIN OF dynpfields OCCURS 3.
        INCLUDE STRUCTURE dynpread.
DATA: END OF dynpfields.

DATA: BEGIN OF t_t526 OCCURS 0,
        sachx LIKE t526-sachx ,
        sachn LIKE t526-sachn ,
        admncode(100) TYPE c  ,
      END OF t_t526           .

DATA: BEGIN OF it_pernr OCCURS 0,
        pernr LIKE zshrattncor-pernr,
        employeenumber LIKE zthr_bhisthmma-employeenumber,
        sname  LIKE pa0001-sname,
        kostl  LIKE pa0001-kostl,
        orgeh(8), "  LIKE pa0001-orgeh,
        ename  LIKE pa0001-ename,
        sachz  LIKE pa0001-sachz,
        schkz  LIKE pa0007-schkz,
        stat2  LIKE pa0000-stat2,
        perflg,
        persg  LIKE pa0001-persg,
        persk  LIKE pa0001-persk,
        btrtl  LIKE pa0001-btrtl,
*        nachn  LIKE pa0002-nachn,
*        vorna  LIKE pa0002-vorna,
END   OF it_pernr.

DATA: BEGIN OF itab OCCURS 0,
          employeenumber LIKE zthr_bhisthmma-employeenumber,
          pernr LIKE pa0001-pernr,
          cnt TYPE i,
          readerid LIKE zthr_bhisthmma-readerid,
          rdate LIKE zthr_bhisthmma-rdate,
          rtime LIKE zthr_bhisthmma-rtime,
          inout,
          $str(20),
          door_desc LIKE zthrdoor-zhdrds,
          badge LIKE zthr_bhisthmma-badge,
          flag(1),
      END OF itab.

DATA: BEGIN OF it_status OCCURS 0,
        pernr LIKE pa0000-pernr,
        begda LIKE pa0000-begda,
        massn LIKE pa0000-massn,
        massg LIKE pa0000-massg,
        stat2 LIKE pa0000-stat2,
      END OF it_status           .

DATA: BEGIN OF it_door OCCURS 0,
        zhdoor LIKE zthrdoor-zhdoor,
        zhdrio LIKE zthrdoor-zhdrio,
        zhdrds LIKE zthrdoor-zhdrds,
      END OF it_door           .

DATA: BEGIN OF it_ws OCCURS 0,
        schkz	TYPE schkn,
        rtext	TYPE retext,
        anzsh TYPE anzschicht,
        tprog TYPE tprog,
      END OF it_ws           .

TYPES: BEGIN OF ty_row_tab.
        INCLUDE STRUCTURE zshrattncor.
TYPES: END OF ty_row_tab.

TYPES: BEGIN OF ty_out,
        cnt_here TYPE i,
        cnt_not_here TYPE i.
        INCLUDE STRUCTURE zshrattncor.
TYPES: END OF ty_out.

DATA i_zthrattncor LIKE zthrattncor OCCURS 0 WITH HEADER LINE.
DATA  : gt_row_tab TYPE TABLE OF ty_row_tab WITH HEADER LINE,
        it_row_tab TYPE TABLE OF ty_row_tab WITH HEADER LINE,
        gt_out     TYPE TABLE OF ty_out     WITH HEADER LINE.

DATA $ix TYPE i.
DATA g_ans.

DATA: w_sachn      LIKE t526-sachn            ,
      w_enddate(8) TYPE c VALUE '99991231'    .

DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.

DEFINE u_break.
  if p_debug eq true.
    break-point.
  endif.
END-OF-DEFINITION.

DEFINE __change_desc.
  read table p_grouplist with key groupname = &1 binary search.
  if sy-subrc eq 0.
    p_grouplist-descript = &2.
    modify p_grouplist index sy-tabix transporting descript.
  endif.
END-OF-DEFINITION.

CONSTANTS:  false VALUE ' ',
            true  VALUE 'X'.

DATA: g_error(1), g_import .

DATA :top_line    TYPE i,
      line_count  TYPE i,
      tab_lines   TYPE i,
      bottom_line TYPE i.

DATA  okcode(4).

RANGES : r_press FOR csks-kostl OCCURS 0,
         r_body FOR csks-kostl OCCURS 0,
         r_paint FOR csks-kostl OCCURS 0,
         r_ga FOR csks-kostl OCCURS 0,
         r_lambda FOR csks-kostl OCCURS 0,
         r_theta FOR csks-kostl OCCURS 0,
         r_prd FOR csks-kostl OCCURS 0,
         r_admin FOR csks-kostl OCCURS 0.

*- U1 Start
DATA: gt_zthrattncor_a TYPE TABLE OF zthrattncor WITH HEADER LINE,
      gt_zthr_bhisthmma_a TYPE TABLE OF zthr_bhisthmma WITH HEADER LINE.
*- U1 End

SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_btrtl FOR pa0001-btrtl NO INTERVALS.
SELECT-OPTIONS: s_orgeh FOR pa0001-orgeh NO INTERVALS.
SELECT-OPTIONS: s_sachx FOR t526-sachx NO INTERVALS.
PARAMETERS : p_date LIKE sy-datum OBLIGATORY DEFAULT sy-datum.
SELECTION-SCREEN END OF BLOCK block1.
SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME TITLE text-002.
SELECT-OPTIONS: s_pernr FOR pa0001-pernr NO INTERVALS NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK block2.

SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE text-023.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 2(12) text-p50 FOR FIELD p_tp1.
PARAMETER p_tp1 RADIOBUTTON GROUP r_tp USER-COMMAND ucom.

SELECTION-SCREEN COMMENT 24(21) text-p40 FOR FIELD p_tp2.
PARAMETER p_tp2 RADIOBUTTON GROUP r_tp.

SELECTION-SCREEN COMMENT 53(20) text-p41 FOR FIELD p_tp3.
PARAMETER p_tp3 RADIOBUTTON GROUP r_tp.

SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b5.

* Layout
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-010.
PARAMETER p_vari TYPE slis_vari.
SELECTION-SCREEN END OF BLOCK b4.

*- U1 Start
INCLUDE ziarch_comm01.
*- U1 End

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM alv_variant_f4 CHANGING p_vari.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.

  PERFORM default_variant.
  PERFORM modi_screen.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_sachx-low.
  PERFORM tmcode_input_help CHANGING s_sachx-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_sachx-high.
  PERFORM tmcode_input_help CHANGING s_sachx-high.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_btrtl-low.
  PERFORM popup_btrtl USING s_btrtl-low.

AT SELECTION-SCREEN .
  CASE sscrfields-ucomm.
    WHEN 'UCOM'.
      CASE true.
        WHEN p_tp1.
          p_vari = '/SUMMARY'.
        WHEN p_tp2.
          p_vari = '/TIMEADM'.
        WHEN p_tp3.
          p_vari = '/CCENTER'.
      ENDCASE.
  ENDCASE.

START-OF-SELECTION.

  IF sy-tcode EQ 'START_REPORT'.

    CALL FUNCTION 'Z_POPUP_TO_GET_DATE'
      EXPORTING
        fieldname           = 'DATUM'
        tabname             = 'SYST'
        titel               = ''
        valuein             = p_date
      IMPORTING
        answer              = g_ans
        valueout            = p_date
      EXCEPTIONS
        fieldname_not_found = 1
        OTHERS              = 2.

    IF sy-subrc <> 0.
*         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    IF g_ans NE ''.
      LEAVE PROGRAM.
    ENDIF.

  ENDIF.

  g_import = false.
  PERFORM initialize            .
  PERFORM get_from_table.
  CHECK g_error EQ space.
  PERFORM show_progress USING 'Get pernr...' 10.
  PERFORM get_pernr USING p_date.
  PERFORM show_progress USING 'Filling info...' 80.
  PERFORM get_text.
  PERFORM fill_table.
  PERFORM finalize.

END-OF-SELECTION.

  IF g_error EQ space.
    PERFORM move_out.
    PERFORM set_output.
  ENDIF.

  IF sy-tcode EQ 'START_REPORT' AND sy-ucomm EQ '&F03'.
    LEAVE TO TRANSACTION 'ZHR008_CCENTER'.
  ELSEIF sy-tcode EQ 'START_REPORT' AND sy-ucomm EQ 'PICK'
         AND g_error EQ 'X'.
    LEAVE TO TRANSACTION 'ZHR008_CCENTER'.
  ENDIF.



*&---------------------------------------------------------------------*
*&      Form  show_progress
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1894   text
*      -->P_1895   text
*----------------------------------------------------------------------*
FORM show_progress USING    pf_text
                            value(pf_val).

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = pf_val
      text       = pf_text.

ENDFORM.                    " SHOW_PROGRESS
*&---------------------------------------------------------------------*
*&      Form  initialize
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialize.

  CLEAR g_error.
  __cls : it_row_tab, it_door, t_t526, it_pernr, it_status, gt_out.

  __cls : i_t526, i_t527x, i_cskt.

  SELECT * INTO TABLE i_t526 FROM t526 WHERE sachx IN s_sachx.
  SELECT * INTO TABLE i_t527x FROM t527x WHERE sprsl EQ sy-langu
                                           AND orgeh IN s_orgeh.
  SELECT * INTO TABLE i_cskt FROM cskt WHERE spras EQ sy-langu
                              AND kokrs EQ 'H201'
                              AND datbi EQ '99991231'.

  SORT : i_t526 BY sachx,
         i_t527x BY orgeh,
         i_cskt BY kostl.

ENDFORM.                    " initialize
*&---------------------------------------------------------------------*
*&      Form  move_out
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM move_out.

  DATA  $flag.
  DATA  $att_total TYPE zherenotherenum.
  DATA  $total TYPE zherenotherenum.
  DATA  $zrate TYPE zsrate.


  IF p_tp1 EQ true.

    LOOP AT it_row_tab.
      CLEAR gt_out.
      gt_out-rdate = it_row_tab-rdate.
      gt_out-kostl = it_row_tab-kostl.
      gt_out-khinr = it_row_tab-khinr.
      gt_out-khinrt = it_row_tab-khinrt.
      gt_out-orgeh = it_row_tab-orgeh.
      gt_out-orgtx = it_row_tab-orgtx.
      gt_out-sachz = it_row_tab-sachz.
      gt_out-btrtl = it_row_tab-btrtl.
      gt_out-btext = it_row_tab-btext.
      READ TABLE i_t526 WITH KEY sachx = it_row_tab-sachz BINARY SEARCH.
      IF sy-subrc EQ 0.
        gt_out-sachn = i_t526-sachn.
      ENDIF.
      READ TABLE i_t527x WITH KEY orgeh = it_row_tab-orgeh BINARY SEARCH.
      IF sy-subrc EQ 0.
        gt_out-orgtx = i_t527x-orgtx.
      ENDIF.

      READ TABLE i_cskt WITH KEY kostl = it_row_tab-kostl BINARY SEARCH.
      IF sy-subrc EQ 0.
        gt_out-kostx = i_cskt-ktext.
      ENDIF.

      IF it_row_tab-zhere EQ true.
        gt_out-cnt_here = 1.
      ENDIF.
      IF it_row_tab-znothere EQ true.
        gt_out-cnt_not_here = 1.
      ENDIF.
      gt_out-zorgeh = gt_out-orgeh.
      COLLECT gt_out.
    ENDLOOP.

  ELSE.
    LOOP AT it_row_tab.
      MOVE-CORRESPONDING it_row_tab TO gt_out.
      READ TABLE i_t526 WITH KEY sachx = it_row_tab-sachz BINARY SEARCH.
      IF sy-subrc EQ 0.
        gt_out-sachn = i_t526-sachn.
      ENDIF.
      READ TABLE i_t527x WITH KEY orgeh = it_row_tab-orgeh BINARY SEARCH.
      IF sy-subrc EQ 0.
        gt_out-orgtx = i_t527x-orgtx.
      ENDIF.

      READ TABLE i_cskt WITH KEY kostl = it_row_tab-kostl BINARY SEARCH.
      IF sy-subrc EQ 0.
        gt_out-kostx = i_cskt-ktext.
      ENDIF.

      IF gt_out-zhere EQ true.
        gt_out-zherenum = '1'.
        gt_out-znotherenum = '0'.
      ENDIF.

      IF gt_out-znothere EQ true.
        gt_out-znotherenum = '1'.
        gt_out-zherenum = '0'.
      ENDIF.
      gt_out-ztotal = '1'.
      gt_out-zorgeh = gt_out-orgeh.
      APPEND gt_out.
    ENDLOOP.

    SORT gt_out BY kostl zshift .
    LOOP AT gt_out.
      CLEAR $flag.
      IF gt_out-zhere EQ true.
        ADD 1 TO $att_total.
      ENDIF.

      ADD 1 TO $total.
      AT END OF zshift.
        $flag = true.
      ENDAT.

      IF $flag EQ true.
        gt_out-zrate = $att_total / $total * 100.
        MODIFY gt_out TRANSPORTING zrate WHERE kostl = gt_out-kostl
                                           AND zshift = gt_out-zshift.
        CLEAR : $att_total, $total .
      ENDIF.


    ENDLOOP.

  ENDIF.


ENDFORM.                    " move_out
*&---------------------------------------------------------------------*
*&      Form  default_variant
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM default_variant.
  DATA: h_subrc   TYPE sysubrc,
        h_repid   TYPE syrepid,
        h_variant TYPE raldb_vari.
  p_vari = '/SUMMARY'.

  h_repid = sy-repid.
  CLEAR h_variant.
  h_variant = 'U_'.
  WRITE sy-uname TO h_variant+2.

  h_variant = '_DEFAULT'.

  CALL FUNCTION 'RS_VARIANT_EXISTS'
    EXPORTING
      report  = h_repid
      variant = h_variant
    IMPORTING
      r_c     = h_subrc.

  IF NOT h_subrc IS INITIAL.
    CLEAR h_variant.
    h_variant = 'SAP_TCODE_'.
    WRITE sy-tcode TO h_variant+10.
    CALL FUNCTION 'RS_VARIANT_EXISTS'
      EXPORTING
        report  = h_repid
        variant = h_variant
      IMPORTING
        r_c     = h_subrc.

    IF NOT h_subrc IS INITIAL.
      CLEAR h_variant.
      h_variant = 'SAP&TCODE_'.
      WRITE sy-tcode TO h_variant+10.
      CALL FUNCTION 'RS_VARIANT_EXISTS'
        EXPORTING
          report  = h_repid
          variant = h_variant
        IMPORTING
          r_c     = h_subrc.
    ENDIF.
  ENDIF.

  IF h_subrc IS INITIAL.
    CALL FUNCTION 'RS_SUPPORT_SELECTIONS'
      EXPORTING
        report               = h_repid
        variant              = h_variant
      EXCEPTIONS
        variant_not_existent = 01
        variant_obsolete     = 02.
  ENDIF.

ENDFORM.                    " default_variant
*&---------------------------------------------------------------------*
*&      Form  tmcode_input_help
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_S_SACHX_LOW  text
*----------------------------------------------------------------------*
FORM tmcode_input_help CHANGING p_tmcode.

  DATA j LIKE sy-index.
  CLEAR : con_list.

  SELECT
          werks
          sachx
          sachn
  INTO TABLE con_list
  FROM t526.

  SORT con_list BY werks sachx .
  DELETE con_list WHERE sachx EQ space.
  LOOP AT con_list.
    help_value-value = con_list-sachx.
    APPEND help_value.
    help_value-value = con_list-sachn.
    APPEND help_value.
    help_value-value = con_list-werks.
    APPEND help_value.
  ENDLOOP.

  PERFORM add_fields USING: 'T526'  'SACHX' 'X',
                            'T526'  'SACHN' ' ',
                            'T526'  'WERKS' ' '.

  PERFORM value_help CHANGING j.

  IF j > 0.
    READ TABLE con_list INDEX j.
    p_tmcode = con_list-sachx.
  ENDIF.

  CLEAR: dynpfields.
  REFRESH: con_list, help_field, help_vtab, help_value, dynpfields.

ENDFORM.                    " tmcode_input_help

*---------------------------------------------------------------------*
*       FORM add_fields                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_TABNAME                                                     *
*  -->  P_FIELDNAME                                                   *
*  -->  P_FLAG                                                        *
*---------------------------------------------------------------------*
FORM add_fields USING  p_tabname p_fieldname p_flag.
  help_field-tabname = p_tabname.
  help_field-fieldname = p_fieldname.
  help_field-selectflag = p_flag.
  APPEND help_field.
  CLEAR help_field.
ENDFORM.                    " add_fields

*---------------------------------------------------------------------*
*       FORM value_help                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_J                                                           *
*---------------------------------------------------------------------*
FORM value_help CHANGING p_j.

  CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
    EXPORTING
      display              = ' '
      title_in_values_list = 'Administrator Code'
    IMPORTING
      index                = p_j
    TABLES
      fields               = help_field
      select_values        = help_vtab
      valuetab             = help_value.

ENDFORM.                    " value_help
*&---------------------------------------------------------------------*
*&      Form  get_pernr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_pernr USING check_date.

  w_sachn = '%NO LONGER VALID%'.
  IF NOT s_sachx[] IS INITIAL.
    CLEAR w_sachn.
  ENDIF.

  SELECT sachx sachn
               FROM t526
               INTO TABLE t_t526
               WHERE sachx IN s_sachx
                 AND sachn NOT LIKE w_sachn.

  CHECK sy-subrc EQ 0.

  SELECT a~pernr a~sname a~kostl a~orgeh a~ename a~sachz
            b~schkz a~persg a~persk a~btrtl
*            c~nachn c~vorna
            INTO CORRESPONDING FIELDS OF TABLE it_pernr
               FROM pa0001 AS a INNER JOIN pa0007 AS b
                 ON b~pernr EQ a~pernr
*                 INNER JOIN pa0002 AS c
*                 ON c~pernr EQ b~pernr
                 FOR ALL ENTRIES IN t_t526
                 WHERE a~sachz = t_t526-sachx
                   AND a~begda LE check_date
                   AND a~endda GE check_date
                   AND b~begda LE check_date
                   AND b~endda GE check_date
*                   AND c~begda LE check_date
*                   AND c~endda GE check_date
                   AND a~pernr IN s_pernr
                   AND a~btrtl IN s_btrtl
                %_HINTS ORACLE 'FIRST_ROWS(10)'.

ENDFORM.                    " get_pernr
*&---------------------------------------------------------------------*
*&      Form  get_stauts
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DATE  text
*----------------------------------------------------------------------*
FORM get_stauts USING check_date.


  CHECK NOT it_pernr[] IS INITIAL.

  SELECT pernr  begda massn massg stat2 INTO TABLE it_status
  FROM pa0000
  FOR ALL ENTRIES IN it_pernr
  WHERE pernr EQ it_pernr-pernr
    AND begda <= check_date
  %_HINTS ORACLE 'FIRST_ROWS(10)'.

  SORT it_status BY pernr ASCENDING
                    begda DESCENDING .

  DELETE ADJACENT DUPLICATES FROM it_status
      COMPARING pernr.

ENDFORM.                    " get_stauts
*&---------------------------------------------------------------------*
*&      Form  get_attn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DATE  text
*----------------------------------------------------------------------*
FORM get_attn USING check_date.

  LOOP AT it_pernr.
    $ix = sy-tabix.
    it_pernr-employeenumber = it_pernr-pernr+2.
    MODIFY it_pernr INDEX $ix TRANSPORTING employeenumber.
  ENDLOOP.

  SELECT employeenumber readerid rdate rtime badge
    FROM zthr_bhisthmma
      INTO CORRESPONDING FIELDS OF TABLE itab
      FOR ALL ENTRIES IN it_pernr
      WHERE employeenumber EQ  it_pernr-employeenumber
        AND rdate EQ check_date
      %_HINTS ORACLE 'FIRST_ROWS(10)'.

*- U1 Start
  IF p_arch EQ 'X'.
    PERFORM archive_read_zthr_bhisthmma USING check_date.
  ENDIF.
*- U1 End

ENDFORM.                    " get_attn
*&---------------------------------------------------------------------*
*&      Form  modi_itab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CHECK_DATE  text
*----------------------------------------------------------------------*
FORM modi_itab USING check_date.

  LOOP AT itab.
    $ix = sy-tabix.

    READ TABLE it_door WITH KEY zhdoor = itab-readerid
                            BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-inout = it_door-zhdrio.
      itab-pernr = itab-employeenumber.
      itab-door_desc = it_door-zhdrds.
      MODIFY itab INDEX $ix TRANSPORTING pernr inout door_desc
      employeenumber.
    ELSE.
      DELETE itab INDEX $ix.
      CONTINUE.
    ENDIF.

  ENDLOOP.

  DELETE itab WHERE employeenumber EQ space.

  LOOP AT itab.
    CONCATENATE itab-rdate itab-rtime INTO itab-$str.
    MODIFY itab.
  ENDLOOP.

  SORT itab BY employeenumber ASCENDING
               $str DESCENDING .

  DATA : $flag,
         $cnt TYPE i.

  DATA $itab LIKE itab OCCURS 0 WITH HEADER LINE.
  DATA $fr LIKE sy-tabix.
  DATA delete_ok.
  $itab[] = itab[].

  SORT $itab BY employeenumber. " ascending

  LOOP AT itab.

    $ix = sy-tabix.

    IF itab-inout EQ '1' AND itab-rdate < check_date.

      READ TABLE $itab WITH KEY employeenumber = itab-employeenumber
                                BINARY SEARCH.
      IF sy-subrc EQ 0.
        $fr = sy-tabix.
        CLEAR delete_ok.
        LOOP AT $itab FROM $fr.
          IF $itab-employeenumber NE itab-employeenumber.
            EXIT.
          ENDIF.
          IF $itab-inout = '0' AND $itab-rdate <= itab-rdate.
            delete_ok = true.
          ENDIF.

          IF $itab-inout = '1' AND $itab-rdate = itab-rdate.
            delete_ok = true.
          ENDIF.

        ENDLOOP.
      ENDIF.
      IF delete_ok = true.
        DELETE itab WHERE employeenumber = itab-employeenumber
                      AND rdate < check_date.
      ENDIF.
    ENDIF.
  ENDLOOP.

  LOOP AT itab.
    CONCATENATE itab-rdate itab-rtime INTO itab-$str.
    MODIFY itab.
  ENDLOOP.

  SORT itab BY employeenumber ASCENDING
               $str DESCENDING .

  LOOP AT itab.
    $ix = sy-tabix.
    AT NEW employeenumber. "pernr.
      CLEAR $cnt.
    ENDAT.
    ADD 1 TO $cnt.
    itab-cnt = $cnt.
    MODIFY itab INDEX $ix TRANSPORTING cnt.
    IF itab-inout EQ '0'.
      ADD 1 TO $cnt.
    ENDIF.
  ENDLOOP.


  SORT itab BY employeenumber cnt .

  LOOP AT itab.
    AT NEW employeenumber.
      CONTINUE.
    ENDAT.
    AT END OF employeenumber.
      CONTINUE.
    ENDAT.
    itab-flag = true.
    MODIFY itab INDEX sy-tabix TRANSPORTING flag.
  ENDLOOP.

  DELETE itab WHERE flag EQ true.

  __cls $itab.
  $itab[] = itab[].

  SORT $itab BY employeenumber inout ASCENDING
                cnt DESCENDING .

  LOOP AT itab.
    $ix = sy-tabix.
    READ TABLE $itab WITH KEY employeenumber = itab-employeenumber
                                inout = itab-inout BINARY SEARCH.
    IF sy-subrc EQ 0 AND $itab-cnt > itab-cnt.
      DELETE itab INDEX $ix.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " modi_itab
*&---------------------------------------------------------------------*
*&      Form  get_basic_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DATE  text
*----------------------------------------------------------------------*
FORM get_basic_info USING check_date.

  DATA $flag.
  DATA $tprog TYPE  tprog.

  SELECT zhdoor zhdrio zhdrds INTO TABLE it_door FROM zthrdoor
  WHERE zhdrcns EQ true.

  SORT it_door BY zhdoor.


  SELECT
         schkz rtext INTO TABLE it_ws
    FROM t508s
  WHERE sprsl EQ sy-langu.

  SORT it_ws BY schkz .

*  LOOP AT it_ws.
*    AT NEW schkz.
*      $flag = true.
*    ENDAT.
*    CHECK $flag EQ true.
*    CLEAR $flag.
*
*    CALL FUNCTION 'Z_CO_GET_DWS_IG'
*         EXPORTING
*              schkz                          = it_ws-schkz
*              datum                          = p_date
*         IMPORTING
*              tprog                          = $tprog
*         EXCEPTIONS
*              not_found_work_schedule_rules  = 1
*              invalid_date                   = 2
*              not_found_period_work_schedule = 3
*              OTHERS                         = 4.
*
*    IF sy-subrc <> 0.
*      $tprog = it_ws-schkz.
*    ENDIF.
*
*    it_ws-tprog = $tprog.
*
*    CASE $tprog.
*      WHEN '0002' OR '1003' OR '1002'.
*        it_ws-anzsh = '2'.
*      WHEN OTHERS.
*        it_ws-anzsh = '1'.
*    ENDCASE.
*    MODIFY it_ws TRANSPORTING tprog anzsh WHERE schkz = it_ws-schkz.
*
*  ENDLOOP.

ENDFORM.                    " get_basic_info
*&---------------------------------------------------------------------*
*&      Form  move_to_result
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM move_to_result USING check_date.
  DATA $cnt TYPE i.

  SORT itab BY pernr.


  LOOP AT it_pernr.

    MOVE-CORRESPONDING it_pernr TO it_row_tab.

    IF NOT it_pernr-pernr IS INITIAL.
      READ TABLE it_status WITH KEY pernr = it_pernr-pernr
      BINARY SEARCH.
      IF sy-subrc EQ 0 AND
      ( it_status-stat2 EQ '1' OR it_status-stat2 EQ '3' ).
        it_row_tab-stat2 = it_status-stat2.
      ELSE.
*        IF it_row_tab-kostl EQ '0000033301'.
*          IF it_status-massn EQ 'ZX' AND it_status-massg EQ '17'.
*            it_row_tab-stat2 = it_status-stat2.
*          ELSE.
*            CLEAR it_row_tab.
*            CONTINUE.
*          ENDIF.
*        ELSE.
        CLEAR it_row_tab.
        CONTINUE.
*        ENDIF.
      ENDIF.
    ENDIF.

    IF it_pernr-perflg EQ false.
      it_row_tab-zflgtmp = true. " It's a temp. emp.
    ENDIF.

    READ TABLE it_ws WITH KEY schkz = it_row_tab-schkz BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_row_tab-rtext = it_ws-rtext.
      it_row_tab-anzsh = it_ws-anzsh.
    ENDIF.
    it_row_tab-rdate = check_date.
    APPEND it_row_tab.CLEAR it_row_tab.
  ENDLOOP.

  SORT it_row_tab BY employeenumber.

  LOOP AT itab.
    AT NEW pernr.
      CLEAR $cnt.
    ENDAT.
    ADD 1 TO $cnt.

    READ TABLE it_row_tab WITH KEY employeenumber = itab-employeenumber
BINARY SEARCH.

*    IF sy-subrc EQ 0 AND it_row_tab-anzsh EQ '2'.
*    ELSE.
*      CHECK $cnt LE 2.
*    ENDIF.
*
    IF sy-subrc EQ 0.
      $ix = sy-tabix.
      CHECK itab-inout EQ '0' OR itab-inout EQ '1'.
      IF itab-inout EQ '0'. " In
        IF it_row_tab-zclkin IS INITIAL.
          it_row_tab-rdatei  = itab-rdate.
          it_row_tab-zdooridi = itab-readerid.
          it_row_tab-zdooridit = itab-door_desc.
          it_row_tab-zclkin  = itab-rtime.
          IF $cnt EQ 1. " Last read is 'in'...
            $cnt = 10.
          ENDIF.
        ENDIF.
        it_row_tab-zhere = true.
      ENDIF.
      IF itab-inout EQ '1'. " Out
        IF it_row_tab-zclkout IS INITIAL.
          it_row_tab-rdateo  = itab-rdate.
          it_row_tab-zdoorido = itab-readerid.
          it_row_tab-zdooridot = itab-door_desc.
          it_row_tab-zclkout = itab-rtime.
        ENDIF.
        it_row_tab-zhere = false.
      ENDIF.
      MODIFY it_row_tab INDEX $ix.
    ENDIF.

  ENDLOOP.

  READ TABLE it_row_tab INDEX 1.

  IF sy-subrc NE 0.
    g_error = true.
    MESSAGE s000 WITH 'No entry was found.'.
  ENDIF.

ENDFORM.                    " move_to_result
*&---------------------------------------------------------------------*
*&      Form  view_from_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_from_table.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_row_tab
  FROM  zthrattncor WHERE rdate EQ p_date
                            AND sachz IN s_sachx
                            AND orgeh IN s_orgeh
                            AND pernr IN s_pernr.

*- U1 Start
  IF p_arch EQ 'X'.
    PERFORM archive_read_zthrattncor.
  ENDIF.
*- U1 End

*  IF sy-subrc NE 0.         "U1- UD1K957440  Apply Archiving
  IF it_row_tab[] IS INITIAL."U1+ UD1K957440  Apply Archiving
    MESSAGE s000 WITH 'No Data has been found!'.
    g_error = true.
    EXIT.
  ENDIF.

ENDFORM.                    " view_from_table

*---------------------------------------------------------------------*
*       FORM fill_table                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM fill_table.
* BEGIN OF UD1K955283
  DATA: l_tprog   TYPE tprog,
        $mosid(2) TYPE n.
* END OF UD1K955283

  SORT it_pernr BY pernr.

  LOOP AT it_row_tab.
    $ix = sy-tabix.
    READ TABLE it_pernr WITH KEY pernr = it_row_tab-pernr BINARY SEARCH.
    IF sy-subrc EQ 0.
      MOVE-CORRESPONDING it_pernr TO it_row_tab.

      READ TABLE it_subarea WITH KEY werks = it_row_tab-werks
                                     btrtl = it_row_tab-btrtl
                                     BINARY SEARCH.
      IF sy-subrc EQ 0.
        it_row_tab-btext = it_subarea-btext.
      ENDIF.

* BEGIN OF UD1K955283
* Get Shift code
      IF it_row_tab-schkz = '8000_A' OR
         it_row_tab-schkz = '8000_B' OR
         it_row_tab-schkz = '8000_C'.

        CALL FUNCTION 'Z_CO_GET_DWS_IG'
          EXPORTING
            schkz                          = it_row_tab-schkz
            datum                          = p_date         "UD1K955419
*           datum                          = '99991231'     "UD1K955419
          IMPORTING
            tprog                          = l_tprog
          EXCEPTIONS
            not_found_work_schedule_rules  = 1
            invalid_date                   = 2
            not_found_period_work_schedule = 3
            OTHERS                         = 4.

        CASE l_tprog.

          WHEN '0002' OR '1003' OR '1002' OR '1009' OR '2006'.

            it_row_tab-zshift = '2'.

          WHEN OTHERS.

            it_row_tab-zshift = '1'.

        ENDCASE.

      ELSE.
        CASE it_row_tab-btrtl.
          WHEN '0004' OR '0005' OR '0001' OR '0002'.
            $mosid = '10'.
          WHEN '0003'.
            $mosid = '9'.
          WHEN '0006'.
            $mosid = '8'.
          WHEN OTHERS.
        ENDCASE.

        CLEAR it_row_tab-zshift.
        SELECT SINGLE anzsh INTO it_row_tab-zshift
           FROM ztco_mh_ws WHERE kokrs EQ 'H201'
                           AND   mosid EQ $mosid
                           AND   schkz EQ it_row_tab-schkz.

      ENDIF.

*      IF it_row_tab-schkz+3(1) EQ '2' OR
*          it_row_tab-schkz+3(1) EQ '3'.
*        it_row_tab-zshift = '2nd'.
*      ELSE.
*        it_row_tab-zshift = '1st'.
*      ENDIF.
* END OF UD1K955283

      MODIFY it_row_tab INDEX $ix.
    ELSE.
      DELETE it_row_tab INDEX $ix.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " view_from_table

*&---------------------------------------------------------------------*
*&      Form  finalize
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM finalize.

*  PERFORM get_cc_group  TABLES
*            : r_press USING p_date 'HMMA-321', " Press
*              r_body USING p_date 'HMMA-322',  " body
*              r_paint USING p_date 'HMMA-323', " Paint
*              r_ga USING p_date 'HMMA-324',    " GA
*              r_lambda USING p_date 'HMMA-331',"Lambda
*              r_theta USING p_date 'HMMA-332', "Theta
*              r_prd USING p_date 'HMMA-300'.  " Prd. Support
*
*  PERFORM get_cc_group_excl  TABLES r_prd USING p_date :
*                      'HMMA-320',
*                      'HMMA-331',
*                      'HMMA-332'.
*
*  PERFORM get_cc_group  TABLES r_admin USING p_date    :
*                      'HMMA-100',
*                      'HMMA-200',
*                      'HMMA-400'.

  PERFORM get_cc_group  TABLES
            : r_press USING p_date 'H201_TM_03', " Press
              r_body USING p_date 'H201_TM_04',  " body
              r_paint USING p_date 'H201_TM_05', " Paint
              r_ga USING p_date 'H201_TM_06',    " GA
              r_lambda USING p_date 'H201_TM_07',"Lambda
              r_theta USING p_date 'H201_TM_08', "Theta
              r_prd USING p_date 'H201_TM_02',  " Prd. Support
              r_admin USING p_date 'H201_TM_01'.  " admin

  SORT : r_press BY low,
         r_body  BY low,
         r_paint  BY low,
         r_ga  BY low,
         r_lambda  BY low,
         r_theta  BY low,
         r_prd  BY low,
         r_admin  BY low.

  LOOP AT it_row_tab.
    $ix = sy-tabix.
    CASE it_row_tab-stat2.
      WHEN '0'.
        it_row_tab-zactive = 'Withdrawn'.
      WHEN '1'.
        it_row_tab-zactive = 'Inactive'.
      WHEN '2'.
        it_row_tab-zactive = 'Retiree'.
      WHEN '3'.
        it_row_tab-zactive = 'Active'.
      WHEN OTHERS.
        it_row_tab-zactive = ''.
    ENDCASE.

    IF it_row_tab-zhere EQ false.
      it_row_tab-znothere = true.
    ENDIF.
    it_row_tab-zorgeh = it_row_tab-orgeh.

    READ TABLE r_body WITH KEY low = it_row_tab-kostl BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_row_tab-khinr = 'B'.
      it_row_tab-khinrt = 'Body'.
      it_row_tab-ccgroup = 'H201_TM_04'.
      MODIFY it_row_tab INDEX $ix.
      CONTINUE.
    ENDIF.

    READ TABLE r_press WITH KEY low = it_row_tab-kostl BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_row_tab-khinr = 'A'.
      it_row_tab-khinrt = 'Press'.
      it_row_tab-ccgroup = 'H201_TM_03'.
      MODIFY it_row_tab INDEX $ix.
      CONTINUE.
    ENDIF.

    READ TABLE r_paint WITH KEY low = it_row_tab-kostl BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_row_tab-khinr = 'C'.
      it_row_tab-khinrt = 'Paint'.
      it_row_tab-ccgroup = 'H201_TM_05'.
      MODIFY it_row_tab INDEX $ix.
      CONTINUE.
    ENDIF.

    READ TABLE r_ga WITH KEY low = it_row_tab-kostl BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_row_tab-khinr = 'D'.
      it_row_tab-khinrt = 'GA'.
      it_row_tab-ccgroup = 'H201_TM_06'.
      MODIFY it_row_tab INDEX $ix.
      CONTINUE.
    ENDIF.

    READ TABLE r_lambda WITH KEY low = it_row_tab-kostl BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_row_tab-khinr = 'E'.
      it_row_tab-khinrt = 'Lambda'.
      it_row_tab-ccgroup = 'H201_TM_07'.
      MODIFY it_row_tab INDEX $ix.
      CONTINUE.
    ENDIF.

    READ TABLE r_theta WITH KEY low = it_row_tab-kostl BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_row_tab-khinr = 'F'.
      it_row_tab-khinrt = 'Theta'.
      it_row_tab-ccgroup = 'H201_TM_08'.
      MODIFY it_row_tab INDEX $ix.
      CONTINUE.
    ENDIF.

    READ TABLE r_prd WITH KEY low = it_row_tab-kostl BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_row_tab-khinr = 'G'.
      it_row_tab-khinrt = 'Prod. Support'.
      it_row_tab-ccgroup = 'H201_TM_02'.
      MODIFY it_row_tab INDEX $ix.
      CONTINUE.
    ENDIF.

    READ TABLE r_admin WITH KEY low = it_row_tab-kostl BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_row_tab-khinr = 'H'.
      it_row_tab-khinrt = 'Admin'.
      it_row_tab-ccgroup = 'H201_TM_01'.
      MODIFY it_row_tab INDEX $ix.
      CONTINUE.
    ENDIF.

    it_row_tab-khinr = 'O'.
    it_row_tab-khinrt = 'Others'.
    MODIFY it_row_tab INDEX $ix.

  ENDLOOP.
  SORT it_row_tab BY ccgroup khinr sachz pernr.

  DATA  $flag.
  DATA  $text(20).

  "break-point.

  DELETE it_row_tab WHERE ccgroup EQ space.

  LOOP AT it_row_tab.

    AT NEW ccgroup.
      $flag = true.
    ENDAT.
    CHECK $flag EQ true.
    CLEAR $flag.
    CONCATENATE 'KNH201' it_row_tab-ccgroup INTO $text.
    AUTHORITY-CHECK OBJECT 'K_CCA'
    ID 'CO_ACTION'  FIELD '0003'
*    ID 'KSTAR'      FIELD space
    ID 'RESPAREA'   FIELD $text.

    IF sy-subrc <> 0.
      DELETE it_row_tab WHERE ccgroup EQ it_row_tab-ccgroup.
    ENDIF.

  ENDLOOP.

  __cls gt_row_tab.

  gt_row_tab[] = it_row_tab[].

ENDFORM.                    " finalize
*&---------------------------------------------------------------------*
*&      Form  set_output
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_output.
  CHECK g_error IS INITIAL.

  PERFORM show_progress     USING 'Preparing screen...' '95'.
  PERFORM init_alv_parm.
  PERFORM fieldcat_init     USING gt_fieldcat[].
  PERFORM sort_build        USING gt_sort[].
  PERFORM alv_events_get    USING:  'P', 'T'.
  PERFORM alv_grid_display  TABLES  gt_out USING ''.

ENDFORM.                    " set_output
*&---------------------------------------------------------------------*
*&      Form  init_alv_parm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_alv_parm.

  __cls   :  gt_fieldcat, gt_sort, gt_events, gt_listheader,
             gt_sp_group.

  CLEAR   :  gs_layout.

  gs_layout-colwidth_optimize = 'X'.

*   Set variant
  gv_repid = gs_variant-report = sy-repid.
  gs_variant-variant = p_vari.

ENDFORM.                    " INIT_ALV_PARM
*&---------------------------------------------------------------------*
*&      Form  fieldcat_init
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCAT[]  text
*----------------------------------------------------------------------*
FORM fieldcat_init USING ft_fieldcat TYPE slis_t_fieldcat_alv .

  DATA: l_pos TYPE i.

  __cls ft_fieldcat.

  DEFINE __catalog.
    l_pos = l_pos + 1.
    clear gs_fieldcat.
    gs_fieldcat-col_pos       = l_pos.
    gs_fieldcat-key           = &1.
    gs_fieldcat-fieldname     = &2.
    gs_fieldcat-seltext_m     = &3.        " Column heading
    gs_fieldcat-outputlen     = &4.        " Column width
    gs_fieldcat-datatype      = &5.        " Data type
    gs_fieldcat-emphasize     = &6.
    gs_fieldcat-cfieldname    = &7.
    gs_fieldcat-no_zero       = &8.

    if p_tp1 eq false.
      if  &2 eq 'ZHERE' or &2 eq 'ZNOTHERE'.
        gs_fieldcat-checkbox = 'X'.
      endif.
    endif.

    append gs_fieldcat to  ft_fieldcat.
  END-OF-DEFINITION.

  IF p_tp1 EQ true.
    __catalog :
      'X'  'BTRTL'     'P.SubArea'     4  'CHAR' '' '' '',
      'X'  'BTEXT'     'Subarea'      30  'CHAR' '' '' '',
      'X'  'SACHZ'     'TAdmin'        3  'CHAR' '' '' '',
      'X'  'SACHN'     'Admin.Name'   20  'CHAR' '' '' '',
      'X'  'CCGROUP'   'CC.Grp.key'   10  'CHAR' '' '' '',
      'X'  'KHINRT'    'CC.Group'     30  'CHAR' '' '' '',
      'X'  'KOSTL'     'Cost.Ctr'     10  'CHAR' '' '' ' ',
      'X'  'KOSTX'     'CC.Text'      40  'CHAR' '' '' '',
      'X'  'ZORGEH'     'Org.Unit'     8  'CHAR' '' '' ' ',
      'X'  'ORGTX'     'Org.Name'     25  'CHAR' '' '' '',
      ' '  'CNT_HERE'     'Attend'    10  'DEC' '' '' ' ',
      ' '  'CNT_NOT_HERE' 'Absence'   10  'DEC' '' '' ' '.
  ELSE.
    __catalog :
      'X'  'BTRTL'     'P.SubArea'     4  'CHAR' '' '' '',
      'X'  'BTEXT'     'Subarea'      30  'CHAR' '' '' '',
      'X'  'SACHZ'     'TAdmin'        3  'CHAR' '' '' '',
      'X'  'SACHN'     'Admin.Name'   20  'CHAR' '' '' '',
      'X'  'CCGROUP'   'CC.Grp.key'   10  'CHAR' '' '' '',
      'X'  'KHINRT'    'CC.Group'     30  'CHAR' '' '' '',
      'X'  'KOSTL'     'Cost.Ctr'     10  'CHAR' '' '' ' ',
      'X'  'KOSTX'     'CC.Text'      40  'CHAR' '' '' '',
      'X'  'ZORGEH'     'Org.Unit'      8  'CHAR' '' '' 'X',
      'X'  'ORGTX'     'Org.Name'     25  'CHAR' '' '' '',
      'X'  'PERNR'     'Emp#'         10  'CHAR' '' '' 'X',
      'X'  'SNAME'     'Name'         30  'CHAR' '' '' '',
      ' '  'ZSHIFT'    'Shift'       10   'CHAR' '' '' '',
      ' '  'ZHERENUM'   'Attend'      10  'DEC' '' '' 'X',
      ' '  'ZNOTHERENUM' 'Absence'   10  'DEC' '' '' 'X',
      ' '  'ZEXEPTION' 'Exception'    50  'CHAR' '' '' '',
      ' '  'ZEXREASON' 'Reason'       50  'CHAR' '' '' '',
      ' '  'ZRATE'       'Attend%'    10  'DEC' '' '' ' ',
      ' '  'ZTOTAL'      'Total'    10  'DEC' '' '' ' '.
  ENDIF.

ENDFORM.                    " fieldcat_init
*&---------------------------------------------------------------------*
*&      Form  sort_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_SORT[]  text
*----------------------------------------------------------------------*
FORM sort_build USING    ft_sort TYPE slis_t_sortinfo_alv.

  DEFINE sort_tab.
    clear gs_sort.
    gs_sort-fieldname = &1.
    gs_sort-spos      = &2.
    gs_sort-up        = &3.
    gs_sort-group     = &4.
    gs_sort-comp      = &5.
    append gs_sort to ft_sort.
  END-OF-DEFINITION.

  IF p_tp1 EQ true.
    sort_tab :
       'SACHZ'    ' ' 'X' 'X' 'X',
       'SACHN'    ' ' 'X' 'X' 'X',
       'ZORGEH'    ' ' 'X' 'X' 'X',
       'ORGTX'    ' ' 'X' 'X' 'X'.
  ELSE.
    sort_tab :
       'BTRTL'    ' ' 'X' 'X' 'X',
       'BTEXT'    ' ' 'X' 'X' 'X',
       'SACHZ'    ' ' 'X' 'X' 'X',
       'SACHN'    ' ' 'X' 'X' 'X',
       'ZORGEH'    ' ' 'X' 'X' 'X',
       'ORGTX'    ' ' 'X' 'X' 'X',
       'PERNR'    ' ' 'X' 'X' 'X',
       'SNAME'    ' ' 'X' 'X' 'X'.
  ENDIF.

ENDFORM.                    " SORT_BUILD
*---------------------------------------------------------------------*
*       FORM user_command                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  FP_UCOMM                                                      *
*  -->  FS                                                            *
*---------------------------------------------------------------------*
FORM user_command USING fp_ucomm LIKE sy-ucomm
                        fs       TYPE slis_selfield.
  CLEAR : g_error.

ENDFORM.                    "USER_COMMAND

*---------------------------------------------------------------------*
*       FORM top_of_page                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM top_of_page.
  DATA l_text(60).
  REFRESH gt_listheader.

  l_text = 'Attendance Verification Summary Report'.
  PERFORM set_header_line USING:
          'P' 'H' ''      l_text       '',
          'D' 'S' 'Date:' p_date ''.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = gt_listheader.

ENDFORM.                    "top_of_page

**---------------------------------------------------------------------*
**       FORM PF_STATUS_SET
**---------------------------------------------------------------------*
FORM pf_status_set USING  ft_extab TYPE slis_t_extab.
  SET PF-STATUS '100'.
ENDFORM.                    "PF_STATUS_SET
*&---------------------------------------------------------------------*
*&      Form  popup_btrtl
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_S_BTRTL_LOW  text
*----------------------------------------------------------------------*
FORM popup_btrtl USING p_fieldname.

  DATA: BEGIN OF lt_btrtl OCCURS 0,
            werks TYPE werks_d,
            name1 TYPE pbtxt,
            btrtl TYPE btrtl,
            btext(15),
         END OF lt_btrtl.

  DATA: BEGIN OF fields_tab OCCURS 1,
            werks TYPE werks_d,
            name1 TYPE pbtxt,
            btrtl TYPE btrtl,
            btext(15),
            color(3),
         END OF fields_tab.

  CLEAR: dynpfields, dyname, dynumb, exc_exctab, popup_fieldcat,
         f, selfield, exitfield, color_active, tabix, fields_tab.
  REFRESH: dynpfields, fields_tab.

  dynpfields-fieldname = p_fieldname.
  APPEND dynpfields.

  dyname = sy-repid.
  dynumb = sy-dynnr.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname             = dyname
      dynumb             = dynumb
      translate_to_upper = 'X'
    TABLES
      dynpfields         = dynpfields
    EXCEPTIONS
      OTHERS             = 9.

  __cls lt_btrtl.

  SELECT a~werks b~name1 a~btrtl a~btext INTO TABLE lt_btrtl
  FROM  t001p AS a
  INNER JOIN t500p AS b
  ON b~persa EQ a~werks
   AND b~bukrs EQ 'H201'.

  SORT lt_btrtl BY btrtl.

  f-reptext_ddic  = 'Pers.Area'.
  f-fieldname = 'WERKS'.
  f-outputlen = 4.
  APPEND f TO popup_fieldcat.
  CLEAR f.

  f-reptext_ddic  = 'PA.Text'.
  f-fieldname = 'NAME1'.
  DESCRIBE FIELD fields_tab-name1 LENGTH f-outputlen.
  APPEND f TO popup_fieldcat.
  CLEAR f.

  f-reptext_ddic  = 'PSubarea'.
  f-fieldname = 'BTRTL'.
  f-outputlen = 4.
  APPEND f TO popup_fieldcat.
  CLEAR f.

  f-reptext_ddic = 'P.subarea Text'.
  f-fieldname = 'BTEXT'.
  DESCRIBE FIELD fields_tab-btext LENGTH f-outputlen.
  APPEND f TO popup_fieldcat.

  tabix = sy-tabix.
  SORT lt_btrtl BY werks btrtl.

  LOOP AT lt_btrtl.
    fields_tab-werks = lt_btrtl-werks.
    fields_tab-name1 = lt_btrtl-name1.
    fields_tab-btrtl = lt_btrtl-btrtl.
    fields_tab-btext = lt_btrtl-btext.
    APPEND fields_tab.
    CLEAR fields_tab.
  ENDLOOP.

  CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
    EXPORTING
      i_linemark_fieldname    = 'COLOR'
      i_tabname               = 'FIELDS_TAB'
      it_fieldcat             = popup_fieldcat
      i_callback_user_command = 'USER_COMMAND_POPUP_LIGHTS_N'
      i_callback_program      = dyname
      it_excluding            = exc_exctab
    IMPORTING
      es_selfield             = selfield
      e_exit                  = exitfield
    TABLES
      t_outtab                = fields_tab.

  READ TABLE fields_tab INDEX tabix.
  MODIFY fields_tab INDEX tabix.

  IF exitfield IS INITIAL.
    READ TABLE fields_tab INDEX selfield-tabindex.
    p_fieldname = fields_tab-btrtl.

    dynpfields-fieldname = p_fieldname.
    dynpfields-fieldvalue = fields_tab-btrtl.
    APPEND dynpfields.

    dyname = sy-repid.
    dynumb = sy-dynnr.

    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        dyname     = dyname
        dynumb     = dynumb
      TABLES
        dynpfields = dynpfields.

  ENDIF.



ENDFORM.                    " popup_btrtl
*&---------------------------------------------------------------------*
*&      Form  get_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_text.

  IF it_subarea[] IS INITIAL.
    SELECT werks btrtl btext INTO TABLE it_subarea FROM t001p.
    SORT it_subarea BY werks btrtl.
  ENDIF.

ENDFORM.                    " get_text
*&---------------------------------------------------------------------*
*&      Form  get_cc_group
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_R_PRESS  text
*      -->P_CHECK_DATE  text
*      -->P_2306   text
*----------------------------------------------------------------------*
FORM get_cc_group TABLES rtable STRUCTURE /sdf/rangesc10
                  USING check_date costcentergroup.

  DATA: BEGIN OF costcenter_list OCCURS 0.
          INCLUDE STRUCTURE bapi0012_2.
  DATA: END OF costcenter_list.

  CALL FUNCTION 'BAPI_COSTCENTER_GETLIST'
    EXPORTING
      controllingarea = 'H201'
      companycode     = 'H201'
      date            = check_date
      costcentergroup = costcentergroup
    TABLES
      costcenter_list = costcenter_list.

  LOOP AT costcenter_list.
    rtable-sign = 'I'.
    rtable-option = 'EQ'.
    rtable-low = costcenter_list-costcenter.
    APPEND rtable.
  ENDLOOP.

ENDFORM.                    " get_cc_group
*&---------------------------------------------------------------------*
*&      Form  get_cc_group_excl
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_R_PRD  text
*      -->P_P_DATE  text
*      -->P_2355   text
*----------------------------------------------------------------------*
FORM get_cc_group_excl TABLES rtable STRUCTURE /sdf/rangesc10
                  USING check_date costcentergroup.

  DATA: BEGIN OF costcenter_list OCCURS 0.
          INCLUDE STRUCTURE bapi0012_2.
  DATA: END OF costcenter_list.
  DATA $ix TYPE i.

  CALL FUNCTION 'BAPI_COSTCENTER_GETLIST'
    EXPORTING
      controllingarea = 'H201'
      companycode     = 'H201'
      date            = check_date
      costcentergroup = costcentergroup
    TABLES
      costcenter_list = costcenter_list.

  SORT costcenter_list BY costcenter.
  LOOP AT rtable.
    $ix = sy-tabix.
    READ TABLE costcenter_list WITH KEY costcenter = rtable-low
        BINARY SEARCH.
    IF sy-subrc EQ 0.
      DELETE rtable INDEX $ix.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " get_cc_group
*&---------------------------------------------------------------------*
*&      Form  modi_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modi_screen.

ENDFORM.                    " modi_screen
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_ZTHR_BHISTHMMA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CHECK_DATE  text
*----------------------------------------------------------------------*
FORM archive_read_zthr_bhisthmma  USING  p_date.

  TYPES: BEGIN OF ty_zthr_bhisthmma,
         rdate TYPE zdrdate,
         rtime TYPE zclksc,
         badge TYPE zbadge,
         readerid TYPE zhdoor,
         employeenumber TYPE zempnumber,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_zthr_bhisthmma.

  DATA: l_handle    TYPE sytabix,
        lt_zthr_bhisthmma TYPE TABLE OF zthr_bhisthmma WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_zthr_bhisthmma TYPE TABLE OF ty_zthr_bhisthmma,
        ls_inx_zthr_bhisthmma TYPE ty_zthr_bhisthmma.

  CONSTANTS: c_zthr_bhisthmma_001(14) VALUE 'ZTHR_BHIST_001'.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = c_zthr_bhisthmma_001.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_zthr_bhisthmma[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_zthr_bhisthmma
    FROM (l_gentab)
    FOR ALL ENTRIES IN it_pernr
   WHERE employeenumber = it_pernr-employeenumber
     AND rdate          = p_date.

  CHECK NOT lt_inx_zthr_bhisthmma[] IS INITIAL.

  LOOP AT lt_inx_zthr_bhisthmma INTO ls_inx_zthr_bhisthmma.
    MOVE-CORRESPONDING ls_inx_zthr_bhisthmma TO itab.
    APPEND itab.  CLEAR itab.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_ZTHR_BHISTHMMA
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_ZTHRATTNCOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM archive_read_zthrattncor .

  TYPES: BEGIN OF ty_zthrattncor,
         rdate TYPE zdrdate,
         sachz TYPE sachx,
         orgeh TYPE orgeh,
         pernr TYPE pernr_d,
         kostl TYPE kostl,
         btrtl TYPE btrtl,
         schkz TYPE schkn,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_zthrattncor.

  DATA: l_handle    TYPE sytabix,
        lt_zthrattncor TYPE TABLE OF zthrattncor WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_zthrattncor TYPE TABLE OF ty_zthrattncor,
        ls_inx_zthrattncor TYPE ty_zthrattncor.

  CONSTANTS: c_zthrattncor_001(14) VALUE 'ZTHRATTNCO_001'.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = c_zthrattncor_001.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_zthrattncor[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_zthrattncor
    FROM (l_gentab)
   WHERE rdate EQ p_date
     AND sachz IN s_sachx
     AND orgeh IN s_orgeh
     AND pernr IN s_pernr.

  CHECK NOT lt_inx_zthrattncor[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: gt_zthrattncor_a, gt_zthrattncor_a[].
  LOOP AT lt_inx_zthrattncor INTO ls_inx_zthrattncor.
*  4.1 Read information from archivekey & offset
    CLEAR l_handle.
    CALL FUNCTION 'ARCHIVE_READ_OBJECT'
      EXPORTING
        object                    = 'ZTHRATTNCO'
        archivkey                 = ls_inx_zthrattncor-archivekey
        offset                    = ls_inx_zthrattncor-archiveofs
      IMPORTING
        archive_handle            = l_handle
      EXCEPTIONS
        no_record_found           = 1
        file_io_error             = 2
        internal_error            = 3
        open_error                = 4
        cancelled_by_user         = 5
        archivelink_error         = 6
        object_not_found          = 7
        filename_creation_failure = 8
        file_already_open         = 9
        not_authorized            = 10
        file_not_found            = 11
        error_message             = 12
        OTHERS                    = 13.

    CHECK sy-subrc = 0.

*  4.2 Read table from information
    CLEAR: lt_zthrattncor, lt_zthrattncor[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        archive_handle          = l_handle
        record_structure        = 'ZTHRATTNCOR'
        all_records_of_object   = 'X'
      TABLES
        table                   = lt_zthrattncor
      EXCEPTIONS
        end_of_object           = 1
        internal_error          = 2
        wrong_access_to_archive = 3
        OTHERS                  = 4.

    CHECK sy-subrc = 0 AND NOT lt_zthrattncor[] IS INITIAL.

*    DELETE lt_zthrattncor WHERE .

* 5. Append archived data table to finally interal table
    INSERT LINES OF lt_zthrattncor INTO TABLE gt_zthrattncor_a.
  ENDLOOP.

  SORT gt_zthrattncor_a.
  DELETE ADJACENT DUPLICATES FROM gt_zthrattncor_a COMPARING ALL FIELDS.

  LOOP AT gt_zthrattncor_a.
    MOVE-CORRESPONDING gt_zthrattncor_a TO it_row_tab.
    APPEND it_row_tab.  CLEAR it_row_tab.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_ZTHRATTNCOR
