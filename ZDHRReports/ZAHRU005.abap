*----------------------------------------------------------------------
* Program ID        : ZAHRU005
* Title             : [HR] Attendance Verification
* Created on        : 4/20/2009
* Created by        : I.G.MOON
* Specifications By : EUNA LEE
* Description       : [HR] Attendance Verification
*&---------------------------------------------------------------------
* Modification Logs
* Date       Developer  Request    Description
* 06/19/2013  T00303    UD1K957433  U1: Apply Archiving
*----------------------------------------------------------------------
REPORT zahru005 MESSAGE-ID zmco.

TABLES: pa0001, t526, *zshrattncor, abdbg, sscrfields, zthrattncor,
zthr_bhisthmma, pa0007.

INCLUDE <icon>.                        " icon

DATA: BEGIN OF con_list OCCURS 0,
          werks LIKE t526-werks,
          sachx LIKE t526-sachx,
          sachn LIKE t526-sachn,
      END OF con_list.
DATA  date_save TYPE sy-datum.
DATA  run_hr.

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
        orgeh  LIKE pa0001-orgeh,
        ename  LIKE pa0001-ename,
        sachz  LIKE pa0001-sachz,
        schkz  LIKE pa0007-schkz,
        stat2  LIKE pa0000-stat2,
        perflg,
        persg  LIKE pa0001-persg,
        persk  LIKE pa0001-persk,
        werks  LIKE pa0001-werks,
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

RANGES  r_admin FOR csks-kostl OCCURS 0.

TYPES: BEGIN OF ty_row_tab.
        INCLUDE STRUCTURE zshrattncor.
TYPES: END OF ty_row_tab.

TYPES: BEGIN OF ty_out.
INCLUDE  TYPE ty_row_tab.
TYPES: END OF ty_out.

DATA i_zthrattncor LIKE zthrattncor OCCURS 0 WITH HEADER LINE.
DATA  : gt_row_tab TYPE TABLE OF ty_row_tab WITH HEADER LINE,
        it_row_tab TYPE TABLE OF ty_row_tab WITH HEADER LINE,
        all_row_tab TYPE TABLE OF ty_row_tab WITH HEADER LINE,
        gt_out     TYPE TABLE OF ty_out     WITH HEADER LINE.

DATA $ix TYPE i.

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

*- U1 Start
DATA: gt_zthrattncor_a TYPE TABLE OF zthrattncor WITH HEADER LINE,
      gt_zthr_bhisthmma_a TYPE TABLE OF zthr_bhisthmma WITH HEADER LINE.
*- U1 End

SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
PARAMETERS : p_date LIKE sy-datum OBLIGATORY DEFAULT sy-datum.
SELECT-OPTIONS: s_sachx FOR t526-sachx NO INTERVALS." OBLIGATORY.
SELECT-OPTIONS: s_schkz FOR pa0007-schkz NO INTERVALS." OBLIGATORY.
SELECTION-SCREEN END OF BLOCK block1.
SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME TITLE text-002.
SELECT-OPTIONS: s_pernr FOR pa0001-pernr NO INTERVALS.
SELECTION-SCREEN END OF BLOCK block2.

SELECTION-SCREEN BEGIN OF BLOCK view-result WITH FRAME TITLE text-t03.
SELECTION-SCREEN PUSHBUTTON  1(34) timpr USER-COMMAND timpr.
SELECTION-SCREEN END OF BLOCK view-result.
PARAMETERS p_force AS CHECKBOX MODIF ID plt.
PARAMETERS : p_time LIKE  zthr_bhisthmma-rtime MODIF ID plt.
PARAMETERS p_save AS CHECKBOX MODIF ID pll.

SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS: s_date FOR sy-datum MODIF ID pll.
PARAMETERS p_runp AS CHECKBOX MODIF ID pll.

*- U1 Start
INCLUDE ziarch_comm01.
*- U1 End

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  sy-title =   '[HR] Attendance Verification'.

  LOOP AT SCREEN.
    IF screen-group1 = 'PLL'.
      IF sy-uname NE 'HIS20065' AND sy-uname NE '103569'.
        screen-input = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

    IF screen-group1 = 'PLT'.
      IF sy-uname NE '103569' AND sy-uname NE 'HIS20065' AND
         sy-uname NE '100206' AND sy-uname NE '102955'.
        screen-input = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

  ENDLOOP.

  PERFORM button_.
  PERFORM default_variant.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

  DATA l_answer.
  DATA $msg(50).
  DATA $date(10).

  CASE sscrfields-ucomm.
    WHEN 'TIMPR'.

      CLEAR run_hr.

      IF p_runp EQ true.
        IF s_date[] IS INITIAL.
          MESSAGE s000 WITH 'Please enter the period.'.
          STOP.
        ENDIF.
      ELSE.
        __cls s_date.
      ENDIF.

      IF NOT s_date[] IS INITIAL AND
      ( sy-uname EQ 'HIS20065' OR sy-uname EQ '103569' OR
        sy-uname EQ '100206' OR sy-uname EQ '102955' )
       AND p_runp EQ true.
        run_hr = true.
      ENDIF.

      CLEAR g_error.

      IF run_hr IS INITIAL.
        SELECT SINGLE * FROM zthrattncor WHERE rdate EQ p_date
                                         AND sachz IN s_sachx
                                         AND pernr IN s_pernr
                                         AND schkz IN s_schkz.
*- U1 Start
        IF p_arch EQ 'X' AND sy-subrc <> 0.
          PERFORM archive_read_zthrattncor.
        ENDIF.
*- U1 End

        IF sy-subrc EQ 0.
          WRITE p_date TO $date.
          CONCATENATE 'The data for' $date 'already exists in table.' INTO
                                                   $msg SEPARATED BY space.

          IF p_force EQ false.
            MESSAGE i000 WITH $msg.
            EXIT.
          ENDIF.

          PERFORM pop_up USING
                 $msg
                 'Do you want to refresh it?' 'X'
                  CHANGING l_answer.
          CHECK l_answer EQ 'J'.

        ENDIF.

      ENDIF.

      g_import = true.
      PERFORM import_.

      PERFORM view_screen.
  ENDCASE.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_sachx-low.
  PERFORM tmcode_input_help CHANGING s_sachx-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_sachx-high.
  PERFORM tmcode_input_help CHANGING s_sachx-high.


START-OF-SELECTION.

  IF p_save EQ true AND sy-batch EQ true..

    SELECT SINGLE * FROM zthr_bhisthmma
    WHERE rdate EQ p_date.

*- U1 Start
    IF p_arch EQ 'X' AND sy-subrc NE 0.
      PERFORM archive_read_zthr_bhisthmma.
    ENDIF.
*- U1 End

    IF sy-subrc NE 0.
      WRITE:/ 'No Gate Data was found!'.
      g_error = true.
      EXIT.
    ENDIF.

    PERFORM  initialize            .
    PERFORM get_basic_info USING p_date.
    PERFORM get_pernr USING p_date.
    READ TABLE it_pernr INDEX 1.
    IF sy-subrc NE 0.
      WRITE:/ 'No Emp# was found.'.
      g_error = true.
      EXIT.
    ENDIF.
    PERFORM get_stauts USING p_date.
    PERFORM get_attn USING p_date.
    CHECK g_error EQ false.
    PERFORM modi_itab USING p_date.
    CHECK g_error EQ false.
    PERFORM move_to_result USING p_date.
    PERFORM finalize.
    PERFORM save_.
  ELSE.
    PERFORM view_.
  ENDIF.

END-OF-SELECTION.
  CHECK p_save NE true.
  PERFORM view_screen.
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


  LOOP AT it_row_tab.
    MOVE-CORRESPONDING it_row_tab TO gt_out.
    APPEND gt_out.
  ENDLOOP.

  SORT gt_out BY pernr.

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
            b~schkz a~persg a~persk a~kostl a~btrtl a~werks
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
                   AND b~schkz IN s_schkz
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

  IF sy-uname NE '103569' AND sy-uname NE 'HIS20065'
    AND sy-uname EQ '100206' AND sy-uname EQ '102955'.
    SELECT employeenumber readerid rdate rtime badge
      FROM zthr_bhisthmma
        INTO CORRESPONDING FIELDS OF TABLE itab
        FOR ALL ENTRIES IN it_pernr
        WHERE employeenumber EQ  it_pernr-employeenumber
          AND rdate EQ check_date
        %_HINTS ORACLE 'FIRST_ROWS(10)'.
  ELSE.
    IF p_time IS INITIAL.
      SELECT employeenumber readerid rdate rtime badge
        FROM zthr_bhisthmma
          INTO CORRESPONDING FIELDS OF TABLE itab
          FOR ALL ENTRIES IN it_pernr
          WHERE employeenumber EQ  it_pernr-employeenumber
            AND rdate EQ check_date
          %_HINTS ORACLE 'FIRST_ROWS(10)'.
    ELSE.
      SELECT employeenumber readerid rdate rtime badge
        FROM zthr_bhisthmma
          INTO CORRESPONDING FIELDS OF TABLE itab
          FOR ALL ENTRIES IN it_pernr
          WHERE employeenumber EQ  it_pernr-employeenumber
            AND rdate EQ check_date
            AND rtime LE p_time
          %_HINTS ORACLE 'FIRST_ROWS(10)'.
    ENDIF.
  ENDIF.

*- U1 Start
  IF p_arch EQ 'X'.
    PERFORM archive_read_zthr_bhisthmma_2 USING check_date.
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
      IF itab-inout IS INITIAL.
        itab-inout = '0'.
      ENDIF.
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

  DELETE ADJACENT DUPLICATES FROM itab COMPARING employeenumber.

*  LOOP AT itab.
*    $ix = sy-tabix.
*    AT NEW employeenumber. "pernr.
*      CLEAR $cnt.
*    ENDAT.
*    ADD 1 TO $cnt.
*    itab-cnt = $cnt.
*    MODIFY itab INDEX $ix TRANSPORTING cnt.
*    IF itab-inout EQ '0'.
*      ADD 1 TO $cnt.
*    ENDIF.
*  ENDLOOP.
*
*
*  SORT itab BY employeenumber cnt .
*
*  LOOP AT itab.
*    AT NEW employeenumber.
*      CONTINUE.
*    ENDAT.
*    AT END OF employeenumber.
*      CONTINUE.
*    ENDAT.
*    itab-flag = true.
*    MODIFY itab INDEX sy-tabix TRANSPORTING flag.
*  ENDLOOP.
*
*  DELETE itab WHERE flag EQ true.
*
*  __cls $itab.
*  $itab[] = itab[].
*
*  SORT $itab BY employeenumber inout ASCENDING
*                cnt DESCENDING .
*
*  LOOP AT itab.
*    $ix = sy-tabix.
*    READ TABLE $itab WITH KEY employeenumber = itab-employeenumber
*                                inout = itab-inout BINARY SEARCH.
*    IF sy-subrc EQ 0 AND $itab-cnt > itab-cnt.
*      DELETE itab INDEX $ix.
*    ENDIF.
*  ENDLOOP.
*
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

* { get the Cost Centers for admin
  PERFORM get_cc_group  TABLES
              r_admin USING p_date 'H201_TM_01'.  " admin
  SORT r_admin  BY low.
* }


  SORT it_row_tab BY employeenumber.

  LOOP AT itab.
    AT NEW pernr.
      CLEAR $cnt.
    ENDAT.
    ADD 1 TO $cnt.

    READ TABLE it_row_tab WITH KEY employeenumber = itab-employeenumber
BINARY SEARCH.

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
        IF it_row_tab-znothere NE true.
          it_row_tab-zhere = true.
        ENDIF.
      ENDIF.
      IF itab-inout EQ '1'. " Out
        IF it_row_tab-zclkout IS INITIAL.
          it_row_tab-rdateo  = itab-rdate.
          it_row_tab-zdoorido = itab-readerid.
          it_row_tab-zdooridot = itab-door_desc.
          it_row_tab-zclkout = itab-rtime.
        ENDIF.
        it_row_tab-zhere = false.
        it_row_tab-znothere = true.
      ENDIF.
      IF it_row_tab-zhere EQ true.
        it_row_tab-zorghere = true.
      ENDIF.

* for admin : set the "here" flag on if scan data exists. {

      READ TABLE r_admin WITH KEY low = it_row_tab-kostl BINARY SEARCH.
      IF sy-subrc EQ 0.
        IF itab-inout EQ '1' OR itab-inout EQ '0'.
          it_row_tab-zhere = true.
          it_row_tab-znothere = false.
          it_row_tab-zorghere = true.
        ENDIF.
      ENDIF.

* }
      MODIFY it_row_tab INDEX $ix.

    ENDIF.

  ENDLOOP.

ENDFORM.                    " move_to_result
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9000 OUTPUT.

  SET PF-STATUS '9000'.
  SET TITLEBAR '9000'.

ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  CASE okcode.
    WHEN '%SC'.    PERFORM find.
    WHEN '%SC+'.   PERFORM find_more.
    WHEN 'P--'.    PERFORM paging USING 'P--'.
    WHEN 'P-'.     PERFORM paging USING 'P-'.
    WHEN 'P+'.     PERFORM paging USING 'P+'.
    WHEN 'P++'.    PERFORM paging USING 'P++'.
    WHEN 'SALL'.   PERFORM marks_a USING true.
    WHEN 'DALL'.   PERFORM marks_a USING false.
    WHEN 'REFL'.   PERFORM refresh.
    WHEN 'SAVE'.   PERFORM save_.
  ENDCASE.
  CLEAR okcode.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Form  PAGING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1751   text
*----------------------------------------------------------------------*
FORM paging USING code.
  DATA: linno TYPE i,
        i TYPE i.

  linno = tab_lines.

  CASE code.
    WHEN 'P--'. top_line = 1.
    WHEN 'P-'.
      top_line = top_line - line_count.
      IF top_line LE 0. top_line = 1. ENDIF.
    WHEN 'P+'.
      i = top_line + line_count.
      linno = linno - line_count + 1.
      IF linno LE 0. linno = 1. ENDIF.
      IF i LE linno.
        top_line = i.
      ELSE.
        top_line = linno.
      ENDIF.
    WHEN 'P++'.
      top_line = linno - line_count + 1.
      IF top_line LE 0. top_line = 1. ENDIF.
  ENDCASE.

ENDFORM.                               " PAGING

*&---------------------------------------------------------------------*
*&      Module  line_position  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE line_position OUTPUT.
  DESCRIBE TABLE it_row_tab LINES tab_lines.
  bottom_line = top_line + line_count - 1.
  IF bottom_line > tab_lines.
    bottom_line = tab_lines.
  ENDIF.
  IF tab_lines EQ 0.  top_line = 0.  ENDIF.
ENDMODULE.                 " LINE_POSITION  OUTPUT

*---------------------------------------------------------------------*
*       MODULE SET_LINE_COUNT OUTPUT                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
MODULE set_line_count OUTPUT.
  line_count = sy-loopc.
ENDMODULE.                 " SET_LINE_COUNT  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_SCREEN_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_screen_9000 OUTPUT.

  MOVE-CORRESPONDING  it_row_tab  TO *zshrattncor.

ENDMODULE.                 " DISPLAY_SCREEN_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  user_exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_exit INPUT.
  DATA save_ok(4).

  save_ok = okcode. CLEAR okcode.

  CASE save_ok.
    WHEN 'BACK'.  SET SCREEN 0.
    WHEN 'EXIT'.  SET SCREEN 0.
    WHEN 'CANC'.  SET SCREEN 0.
  ENDCASE.

  LEAVE SCREEN.

ENDMODULE.                 " user_exit  INPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_TAB  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_tab INPUT.
  MODIFY it_row_tab INDEX top_line.
ENDMODULE.                 " MODIFY_TAB  INPUT
*&---------------------------------------------------------------------*
*&      Module  ADBMO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE adbmo OUTPUT.
  SET PF-STATUS 'POPSEARC'.
  SET TITLEBAR 'SEARCH'.

ENDMODULE.                 " ADBMO  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  ADBR2  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE adbr2 INPUT.
  IF abdbg-popok EQ 'OK'.
    PERFORM search_string USING '1'.
  ENDIF.
ENDMODULE.                 " ADBR2  INPUT
*&---------------------------------------------------------------------*
*&      Form  FIND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM find.
  CALL SCREEN 300 STARTING AT  8     5 .
ENDFORM.                    " FIND
*&---------------------------------------------------------------------*
*&      Form  search_string
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM search_string USING p_line.

  CHECK abdbg-searchstr NE space.
  SEARCH it_row_tab FOR abdbg-searchstr
  STARTING AT p_line.
  IF sy-subrc EQ 0.
    top_line = sy-tabix.
  ENDIF.
ENDFORM.                    " search_string
*&---------------------------------------------------------------------*
*&      Form  find_more
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM find_more.
  DATA moreline TYPE i.
  moreline = top_line + 1.
  PERFORM search_string USING moreline.
ENDFORM.                    " find_more
*&---------------------------------------------------------------------*
*&      Form  MARKS_A
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TRUE  text
*----------------------------------------------------------------------*
FORM marks_a USING how.

  LOOP AT it_row_tab.
    $ix = sy-tabix.
    it_row_tab-mark = how.
    MODIFY it_row_tab INDEX $ix.
  ENDLOOP.

ENDFORM.                               " MARKS_A
*&---------------------------------------------------------------------*
*&      Form  refresh
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh.
  IF g_import EQ true.
    PERFORM import_.
  ELSE.
    PERFORM view_.
  ENDIF.
ENDFORM.                    " refresh
*&---------------------------------------------------------------------*
*&      Form  load_from_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM import_.

  __cls all_row_tab.

  IF run_hr EQ true.

    date_save = p_date.
    p_date = s_date-low - 1.

    DO 1000 TIMES.
      ADD 1 TO p_date.
      CHECK p_date IN s_date.

      SELECT SINGLE * FROM zthr_bhisthmma
      WHERE rdate EQ p_date.

*- U1 Start
      IF p_arch EQ 'X' AND sy-subrc NE 0.
        PERFORM archive_read_zthr_bhisthmma.
      ENDIF.
*- U1 End

      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.

      PERFORM make_gt_out.
      APPEND LINES OF it_row_tab TO all_row_tab.
    ENDDO.
    p_date = date_save.

    __cls it_row_tab.
    it_row_tab[] = all_row_tab[].

  ELSE.

    SELECT SINGLE * FROM zthr_bhisthmma
    WHERE rdate EQ p_date.

*- U1 Start
    IF p_arch EQ 'X' AND sy-subrc NE 0.
      PERFORM archive_read_zthr_bhisthmma.
    ENDIF.
*- U1 End

    IF sy-subrc NE 0.
      MESSAGE s000 WITH 'No Gate Data was found!'.
      g_error = true.
    ENDIF.

    IF g_error EQ false.
      PERFORM make_gt_out.
    ENDIF.

  ENDIF.

  READ TABLE it_row_tab INDEX 1.

  IF sy-subrc NE 0.
    g_error = true.
    MESSAGE s000 WITH 'No entry was found.'.
  ENDIF.

  PERFORM show_progress USING 'Filling info...' 80.
  PERFORM finalize.
ENDFORM.                    " load_from_table
*&---------------------------------------------------------------------*
*&      Module  CHECK_INPUT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_input INPUT.
  MOVE :
         *zshrattncor-mark      TO it_row_tab-mark,
         *zshrattncor-pernr	    TO it_row_tab-pernr,
         *zshrattncor-sname     TO it_row_tab-sname,
*         *zshrattncor-vorna      TO it_row_tab-vorna,
*         *zshrattncor-nachn      TO it_row_tab-nachn,
         *zshrattncor-zactive   TO it_row_tab-zactive,
         *zshrattncor-sachz	    TO it_row_tab-sachz,
         *zshrattncor-schkz	    TO it_row_tab-schkz,
         *zshrattncor-zhere	    TO it_row_tab-zhere,
         *zshrattncor-znothere  TO it_row_tab-znothere,
         *zshrattncor-zexeption TO it_row_tab-zexeption,
         *zshrattncor-zexreason TO it_row_tab-zexreason,
         *zshrattncor-rdate	    TO it_row_tab-rdate.

ENDMODULE.                 " CHECK_INPUT  INPUT
*&---------------------------------------------------------------------*
*&      Form  view_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM view_.
  g_import = false.
  PERFORM initialize            .
  PERFORM show_progress USING 'Get pernr...' 10.
  PERFORM get_pernr USING p_date.
  READ TABLE it_pernr INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE s000 WITH 'No Emp# was found.'.
    g_error = true.
    EXIT.
  ENDIF.
  PERFORM view_from_table.
  PERFORM show_progress USING 'Filling info...' 80.
  PERFORM finalize.

ENDFORM.                    " view_
*&---------------------------------------------------------------------*
*&      Form  view_from_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM view_from_table.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_row_tab
  FROM  zthrattncor WHERE rdate EQ p_date
                            AND sachz IN s_sachx
                            AND pernr IN s_pernr
                            AND schkz IN s_schkz.

*- U1 Start
  IF p_arch EQ 'X'.
    PERFORM archive_read_zthrattncor_2.
  ENDIF.
*- U1 End

*  IF sy-subrc NE 0.   "U1- T00303    UD1K957433  U1: Apply Archiving
  IF it_row_tab[] IS INITIAL. "U1+    UD1K957433  U1: Apply Archiving
    MESSAGE s000 WITH 'No Data was found!'.
    g_error = true.
    EXIT.
  ENDIF.

  SORT it_pernr BY pernr.

  LOOP AT it_row_tab.
    $ix = sy-tabix.
    READ TABLE it_pernr WITH KEY pernr = it_row_tab-pernr BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_row_tab-sname = it_pernr-sname.
    ENDIF.

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

    MODIFY it_row_tab INDEX $ix.
  ENDLOOP.

ENDFORM.                    " view_from_table
*&---------------------------------------------------------------------*
*&      Form  button_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM button_.

  WRITE:
          icon_import AS ICON TO timpr,
         'Import Data from Gate History' TO timpr+4(29).

ENDFORM.                    " button_
*&---------------------------------------------------------------------*
*&      Form  view_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM view_screen.

  CHECK g_error EQ false.

  DESCRIBE TABLE it_row_tab LINES tab_lines.
  CALL SCREEN 9000.


ENDFORM.                    " view_screen
*&---------------------------------------------------------------------*
*&      Form  save_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_.
  DATA i_cnt TYPE i.
  DATA : $text(100), $cnt(10).

  PERFORM show_progress USING 'Please wait for saving...' 0.

  DELETE FROM zthrattncor WHERE rdate EQ p_date
                            AND sachz IN s_sachx
                            AND pernr IN s_pernr
                            AND schkz IN s_schkz.

  COMMIT WORK.
  __cls i_zthrattncor.
  LOOP AT it_row_tab.
    CLEAR i_zthrattncor.
    MOVE-CORRESPONDING it_row_tab TO i_zthrattncor.
    i_zthrattncor-aedat = sy-datum.
    i_zthrattncor-aenam = sy-uname.

    READ TABLE gt_row_tab WITH KEY sachz  = it_row_tab-sachz
                                   pernr  = it_row_tab-pernr
                                   BINARY SEARCH.
    IF sy-subrc EQ 0.

      IF gt_row_tab-zorghere EQ it_row_tab-zhere.
      ELSE.
        i_zthrattncor-manupd = true.
        i_zthrattncor-zutime = sy-uzeit.
      ENDIF.

*      IF gt_row_tab-zhere NE it_row_tab-zhere OR
*         gt_row_tab-zexeption NE it_row_tab-zexeption OR
*         gt_row_tab-zexreason NE it_row_tab-zexreason.
*      ENDIF.

    ENDIF.
    APPEND i_zthrattncor.
    ADD 1 TO i_cnt.
  ENDLOOP.

  MODIFY zthrattncor FROM TABLE i_zthrattncor.
  COMMIT WORK.
  IF sy-subrc EQ 0.
    IF p_save EQ true.
      $cnt = i_cnt.
      CONCATENATE 'Data has been save successfully.:' $cnt
      INTO $text.
      WRITE:/ $text.
    ELSE.
      MESSAGE s000 WITH 'Data has been save successfully.(' i_cnt ')'.
    ENDIF.
  ENDIF.
ENDFORM.                    " save_
*&---------------------------------------------------------------------*
*&      Form  finalize
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM finalize.
*0  Withdrawn
*1  Inactive
*2  Retiree
*3  Active

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
    MODIFY it_row_tab INDEX $ix.
  ENDLOOP.
  SORT it_row_tab BY rdate sachz pernr.
  __cls gt_row_tab.

  gt_row_tab[] = it_row_tab[].


ENDFORM.                    " finalize

*---------------------------------------------------------------------*
*       FORM pop_up                                                   *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_TEXT                                                        *
*  -->  P_TEXT2                                                       *
*  -->  P_CANC                                                        *
*  -->  P_ANSWER                                                      *
*---------------------------------------------------------------------*
FORM pop_up USING    p_text p_text2 p_canc
            CHANGING p_answer.
  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      textline1      = p_text
      textline2      = p_text2
      titel          = 'Check!'
      cancel_display = p_canc
    IMPORTING
      answer         = p_answer.


ENDFORM.                    " POP_UP
*&---------------------------------------------------------------------*
*&      Form  get_cc_group
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_R_ADMIN  text
*      -->P_P_DATE  text
*      -->P_1418   text
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
*&      Form  make_gt_out
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_gt_out.

  PERFORM  initialize            .
  PERFORM get_basic_info USING p_date.

  PERFORM show_progress USING 'Get pernr...' 10.
  PERFORM get_pernr USING p_date.
  READ TABLE it_pernr INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE s000 WITH 'No Emp# was found.'.
    g_error = true.
    EXIT.
  ENDIF.

  PERFORM show_progress USING 'Get Status...' 20.
  PERFORM get_stauts USING p_date.

  PERFORM show_progress USING 'Get Door data...' 30.
  PERFORM get_attn USING p_date.
  CHECK g_error EQ false.

  PERFORM show_progress USING 'Modify data...' 40.
  PERFORM modi_itab USING p_date.

  CHECK g_error EQ false.
  PERFORM show_progress USING 'Move to result...' 70.
  PERFORM move_to_result USING p_date.

ENDFORM.                    " make_gt_out
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_ZTHR_BHISTHMMA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM archive_read_zthr_bhisthmma .

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
   WHERE rdate EQ p_date.

  CHECK sy-subrc = 0.

ENDFORM.                    " ARCHIVE_READ_ZTHR_BHISTHMMA
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_ZTHR_BHISTHMMA_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CHECK_DATE  text
*----------------------------------------------------------------------*
FORM archive_read_zthr_bhisthmma_2 USING p_date.

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
  IF sy-uname NE '103569' AND sy-uname NE 'HIS20065' AND
     sy-uname EQ '100206' AND sy-uname EQ '102955'.
    CLEAR lt_inx_zthr_bhisthmma[].
    SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_zthr_bhisthmma
      FROM (l_gentab)
      FOR ALL ENTRIES IN it_pernr
     WHERE employeenumber = it_pernr-employeenumber
       AND rdate          = p_date.
  ELSE.
    IF p_time IS INITIAL.
      CLEAR lt_inx_zthr_bhisthmma[].
      SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_zthr_bhisthmma
        FROM (l_gentab)
        FOR ALL ENTRIES IN it_pernr
       WHERE employeenumber = it_pernr-employeenumber
         AND rdate          = p_date.
    ELSE.
      CLEAR lt_inx_zthr_bhisthmma[].
      SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_zthr_bhisthmma
        FROM (l_gentab)
        FOR ALL ENTRIES IN it_pernr
       WHERE employeenumber = it_pernr-employeenumber
         AND rdate          = p_date
         AND rtime          LE p_time.
    ENDIF.
  ENDIF.

  CHECK NOT lt_inx_zthr_bhisthmma[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: gt_zthr_bhisthmma_a, gt_zthr_bhisthmma_a[].
  LOOP AT lt_inx_zthr_bhisthmma INTO ls_inx_zthr_bhisthmma.
*  4.1 Read information from archivekey & offset
    CLEAR l_handle.
    CALL FUNCTION 'ARCHIVE_READ_OBJECT'
      EXPORTING
        object                    = 'ZTHRATTNCO'
        archivkey                 = ls_inx_zthr_bhisthmma-archivekey
        offset                    = ls_inx_zthr_bhisthmma-archiveofs
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
    CLEAR: lt_zthr_bhisthmma, lt_zthr_bhisthmma[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        archive_handle          = l_handle
        record_structure        = 'ZTHR_BHISTHMMA'
        all_records_of_object   = 'X'
      TABLES
        table                   = lt_zthr_bhisthmma
      EXCEPTIONS
        end_of_object           = 1
        internal_error          = 2
        wrong_access_to_archive = 3
        OTHERS                  = 4.

    CHECK sy-subrc = 0 AND NOT lt_zthr_bhisthmma[] IS INITIAL.

*    DELETE lt_zthrattncor WHERE .

* 5. Append archived data table to finally interal table
    INSERT LINES OF lt_zthr_bhisthmma INTO TABLE gt_zthr_bhisthmma_a.
  ENDLOOP.

  SORT gt_zthr_bhisthmma_a.
  DELETE ADJACENT DUPLICATES FROM gt_zthr_bhisthmma_a COMPARING ALL FIELDS.

  LOOP AT gt_zthr_bhisthmma_a.
    MOVE-CORRESPONDING gt_zthr_bhisthmma_a TO itab.
    APPEND itab.  CLEAR itab.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_ZTHR_BHISTHMMA_2
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
     AND pernr IN s_pernr
     AND schkz IN s_schkz.

ENDFORM.                    " ARCHIVE_READ_ZTHRATTNCOR
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_ZTHRATTNCOR_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM archive_read_zthrattncor_2 .

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
     AND pernr IN s_pernr
     AND schkz IN s_schkz.

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

ENDFORM.                    " ARCHIVE_READ_ZTHRATTNCOR_2
