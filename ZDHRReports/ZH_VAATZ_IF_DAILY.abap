REPORT zh_vaatz_if_daily MESSAGE-ID zmco.

*-----------------------------------------------------------------------
* Name: ZH_VAATZ_IF- HR VAATZ daily Interface
* Tech. Resource: Imtiaz Ahmad
* Desc: HR VAATZ daily interface
*----------------------------------------------------------------------
*----------------------------------------------------------------------*
*  Title          : ZH_VAATZ_IF_DAILY
*  Author         : ig.moon
*  Creation Data  : 8/5/2008
*  Requirements by: Imtiaz Ahmad
*  Description    : HR VAATZ daily interface.

************************************************************************
* CHANGE LOG
*-----------------------------------------------------------------------
* DATE      |  NAME          |Transport | Issue #  |      DESC
* 2011.09.15   yn.kim         UP1K920005            ECC6.0 Upgrade.
*-----------------------------------------------------------------------

***********************************************************************

*                      --- TABLES ---
*----------------------------------------------------------------------
TABLES: zshrvaatz ,
        hrp1001,hrp1000,
       *hrp1001,*hrp1000,t001p,zthrvaatz,
        *rfcdes.

DATA itab LIKE hrp1000 OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF $dep_code OCCURS 0,
        objid LIKE hrp1001-objid,
       END OF $dep_code.

DATA : BEGIN OF $sobid OCCURS 0,
        sobid LIKE hrp1001-sobid,
       END OF $sobid.

DATA : BEGIN OF $sup_dep_code OCCURS 0,
        objid LIKE hrp1001-sobid,
        sobid LIKE hrp1001-objid,
       $sobid LIKE hrp1001-objid,
       END OF   $sup_dep_code.

DATA : BEGIN OF $mgr_pos_id OCCURS 0,
        objid LIKE hrp1001-sobid,
        sobid LIKE hrp1001-objid,
       $sobid LIKE hrp1001-objid,
       END OF   $mgr_pos_id.

DATA : BEGIN OF $mgr_emp_id OCCURS 0,
        objid LIKE hrp1001-sobid,
        sobid LIKE hrp1001-objid,
       $sobid LIKE hrp1001-objid,
       END OF   $mgr_emp_id.

DATA g_zthrvaatz LIKE zthrvaatz OCCURS 0 WITH HEADER LINE .

DATA   num(12) VALUE ' 0123456789'.

DATA  it_t001p LIKE t001p OCCURS 20 WITH HEADER LINE.

*--------------------------------------------------------------------*
* SELECTION-SCREEN
*--------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK 0 WITH FRAME TITLE text-100.
PARAMETERS : par_date LIKE sy-datum DEFAULT sy-datum.
SELECTION-SCREEN END   OF BLOCK 0.

SELECTION-SCREEN BEGIN OF BLOCK 1 WITH FRAME TITLE text-101.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS:
  par_r1 RADIOBUTTON GROUP 1.
SELECTION-SCREEN:
  COMMENT 03(29) text-003 FOR FIELD par_r1.
PARAMETERS:
  par_file(50).

SELECTION-SCREEN:
  END OF LINE,
  BEGIN OF LINE.
SELECTION-SCREEN:
  END OF LINE,
  BEGIN OF LINE.
PARAMETERS:
  par_r2 RADIOBUTTON GROUP 1 DEFAULT 'X'.
SELECTION-SCREEN:
  COMMENT 03(50) text-005 FOR FIELD par_r2,
  END OF LINE.
PARAMETERS : par_dest LIKE rfcdes-rfcdest
                       DEFAULT 'WMHR01'.
SELECTION-SCREEN END   OF BLOCK 1.

PARAMETERS p_all AS CHECKBOX.
PARAMETERS p_debug NO-DISPLAY.

DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.
DEFINE u_break.
  if not p_debug is initial.
    break-point.
  endif.
END-OF-DEFINITION.
DEFINE __process.
  perform show_progress using &1 &2.
END-OF-DEFINITION.

CONSTANTS:  false VALUE ' ',
            true  VALUE 'X'.

****************************** Global Data *****************************


DATA BEGIN OF i_zshrvaatz OCCURS 0.
        INCLUDE STRUCTURE zshrvaatz.
DATA : flag,
       objid TYPE hrobjid,
       mc_stext TYPE hr_mcstext,
       sname(30),
       level TYPE i,
     END OF i_zshrvaatz.

DATA  final_zshrvaatz TYPE TABLE OF zshrvaatz WITH HEADER LINE.

INITIALIZATION.

  par_file    = 'c:\temp\HR_VAATZ'.
  par_file+17  = '-'.
  WRITE sy-datlo TO par_file+18(6) YYMMDD.
  par_file+25 = '-'.
  par_file+26 = sy-timlo.
  CONCATENATE par_file '.txt' INTO par_file.
  CONDENSE par_file NO-GAPS.

*--------------------------------------------------------------------*
START-OF-SELECTION.
*--------------------------------------------------------------------*

  __cls : itab, i_zshrvaatz.

  u_break.

  SELECT * INTO TABLE itab
  FROM hrp1000
  WHERE plvar EQ '01'
    AND otype EQ 'O'
    AND istat EQ '1'
    AND langu EQ 'EN'
    AND endda >= par_date
    AND begda <= par_date.

  IF sy-subrc EQ 0..
  ELSE.
    MESSAGE s000 WITH 'No data was found!'.
    STOP.
  ENDIF.

  __cls : $dep_code,it_t001p.

  LOOP AT itab.
    $dep_code-objid = itab-objid.
    APPEND $dep_code.
  ENDLOOP.

  SORT $dep_code BY objid.
  DELETE ADJACENT DUPLICATES FROM $dep_code.

  IF NOT $dep_code[] IS INITIAL.
    __cls $sobid.
    LOOP AT $dep_code.
      $sobid-sobid = $dep_code-objid.
      APPEND $sobid.
    ENDLOOP.

    SORT $sobid.
    DELETE ADJACENT DUPLICATES FROM $sobid.
  ENDIF.

  PERFORM get_sup_dep_code.
  PERFORM get_mgr_emp_id.

  DATA $strlen TYPE i.

  LOOP AT itab.
    CLEAR i_zshrvaatz.
    i_zshrvaatz-objid = itab-objid.
    i_zshrvaatz-corp_gb     = 'A1'.
    i_zshrvaatz-com_cd      = 'HMMA'.

    i_zshrvaatz-open_date   = itab-begda.
    i_zshrvaatz-close_date  = itab-endda.
    i_zshrvaatz-modi_date   = itab-aedtm.
    i_zshrvaatz-tel_no      = '   '.
    i_zshrvaatz-work_area   = 'A1'.

    READ TABLE $mgr_pos_id WITH KEY $sobid = itab-objid
                             BINARY SEARCH.

    IF sy-subrc EQ 0.
      READ TABLE $mgr_emp_id WITH KEY $sobid = $mgr_pos_id-sobid
                               BINARY SEARCH.
      IF sy-subrc EQ 0.
        i_zshrvaatz-manager_emp_no = $mgr_emp_id-sobid+2.

        CLEAR i_zshrvaatz-up_dept_cd.
        SELECT SINGLE kostl INTO i_zshrvaatz-dept_cd
        FROM pa0001 WHERE orgeh EQ itab-objid
                      AND pernr EQ $mgr_emp_id-sobid
                      AND endda >= par_date
                      AND begda <= par_date.

        i_zshrvaatz-cost_center = i_zshrvaatz-dept_cd.

        READ TABLE $sup_dep_code WITH KEY $sobid = itab-objid
                                 BINARY SEARCH.
        IF sy-subrc EQ 0.
          IF $sup_dep_code-sobid EQ itab-objid.
            i_zshrvaatz-up_dept_cd  = i_zshrvaatz-dept_cd.
          ELSE.
            SELECT SINGLE kostl INTO i_zshrvaatz-up_dept_cd
            FROM pa0001 WHERE orgeh EQ $sup_dep_code-sobid
                          AND pernr EQ $mgr_emp_id-sobid
                          AND endda >= par_date
                          AND begda <= par_date.
          ENDIF.
        ELSE.
          i_zshrvaatz-up_dept_cd  = i_zshrvaatz-dept_cd.
        ENDIF.

      ENDIF.
      SELECT SINGLE sname INTO i_zshrvaatz-sname
            FROM pa0001
            WHERE pernr EQ $mgr_emp_id-sobid
              AND endda >= par_date
              AND begda <= par_date.

      IF sy-subrc NE 0.
        CLEAR i_zshrvaatz-sname.
      ENDIF.
      CLEAR $mgr_emp_id.
    ENDIF.

    SELECT SINGLE mc_stext INTO i_zshrvaatz-mc_stext
        FROM hrp1000
                      WHERE otype EQ 'S'
                        AND plvar EQ '01'
                        AND istat EQ '1'
                        AND begda <= par_date
                        AND objid EQ $mgr_pos_id-sobid
                        AND endda >= par_date
                        AND begda <= par_date.

    IF sy-subrc NE 0.
      CLEAR i_zshrvaatz-mc_stext.
    ENDIF.

    i_zshrvaatz-input_emp_no = sy-uname.

    SELECT SINGLE ktext INTO i_zshrvaatz-dept_nm_eng
    FROM cskt WHERE
    spras EQ sy-langu
    AND kokrs EQ 'H201'
    AND kostl EQ  i_zshrvaatz-dept_cd
    AND datbi EQ '99991231'.

    i_zshrvaatz-dept_nm_ext = i_zshrvaatz-dept_nm_eng.

    IF p_all NE true.
      $strlen = strlen( i_zshrvaatz-dept_cd ).
      IF $strlen EQ 10.
        i_zshrvaatz-dept_cd      = i_zshrvaatz-dept_cd+5.
        i_zshrvaatz-cost_center  = i_zshrvaatz-cost_center+5.
        i_zshrvaatz-up_dept_cd   = i_zshrvaatz-dept_cd.
        i_zshrvaatz-trans_date = sy-datum.
        APPEND i_zshrvaatz.
      ENDIF.
    ELSE.
      IF i_zshrvaatz-dept_cd CN num.
      ELSE.
        i_zshrvaatz-dept_cd      = i_zshrvaatz-dept_cd+5.
        i_zshrvaatz-cost_center  = i_zshrvaatz-cost_center+5.
      ENDIF.

      IF i_zshrvaatz-up_dept_cd CN num.
      ELSE.
        i_zshrvaatz-up_dept_cd   = i_zshrvaatz-up_dept_cd+5.
      ENDIF.

      i_zshrvaatz-trans_date = sy-datum.

      IF itab-stext IS INITIAL.
        itab-stext = '          '.
      ENDIF.

      APPEND i_zshrvaatz.
    ENDIF.

  ENDLOOP.

  PERFORM filter_table.

  SORT i_zshrvaatz BY dept_cd up_dept_cd level ASCENDING
                      open_date manager_emp_no DESCENDING  .

  DELETE ADJACENT DUPLICATES FROM i_zshrvaatz COMPARING
  dept_cd. " up_dept_cd.

  __cls final_zshrvaatz.

  LOOP AT i_zshrvaatz.
    MOVE-CORRESPONDING i_zshrvaatz TO final_zshrvaatz.
    APPEND final_zshrvaatz.
  ENDLOOP.
*--------------------------------------------------------------------*
END-OF-SELECTION.
*--------------------------------------------------------------------*

  IF par_r1 EQ true.
    PERFORM create_file.
  ELSE.
    PERFORM vaatz_eai.
  ENDIF.


*&---------------------------------------------------------------------*
*&      Form  gqms_eai
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM vaatz_eai.

  DATA : $result,
         $del_date LIKE sy-datum,
         $date LIKE sy-datum,
         $time LIKE sy-uzeit.

  SELECT SINGLE * FROM rfcdes  INTO *rfcdes
                WHERE rfcdest EQ par_dest.
  IF sy-subrc NE 0.
    MESSAGE s000 WITH 'No RFC Destination was found!'.
    EXIT.
  ENDIF.

  CALL FUNCTION 'Z_HR_VAATZ_IF_DAILY'
    DESTINATION par_dest
    TABLES
      zshrvaatz = final_zshrvaatz.

  IF sy-subrc NE 0.
    $result = 'L'.
    MESSAGE s000 WITH 'I/F error was occured!'.
  ENDIF.

* by ig.moon 3/6/2009 {

  $del_date = sy-datum - 10.
  $date = sy-datum.
  $time = sy-uzeit.

  DELETE FROM zthrvaatz WHERE zedat <= $del_date.
  COMMIT WORK.

  __cls g_zthrvaatz.

  LOOP AT final_zshrvaatz.

    CLEAR g_zthrvaatz.
    MOVE-CORRESPONDING final_zshrvaatz TO g_zthrvaatz.
    g_zthrvaatz-zedat = $date.
    g_zthrvaatz-zetim = $time.
    g_zthrvaatz-zresult =  $result.
    g_zthrvaatz-zuser = sy-uname.
    APPEND g_zthrvaatz.

  ENDLOOP.

  MODIFY zthrvaatz FROM TABLE g_zthrvaatz.
  COMMIT WORK.

* }


ENDFORM.                    " gqms_eai
*&---------------------------------------------------------------------*
*&      Form  create_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_file.

  DATA : l_filename   TYPE  string.

  IF par_file EQ space.
    par_file    = 'HR_VAATZ'.
    par_file+8  = '-'.
    WRITE sy-datlo TO par_file+9(6) YYMMDD.
    par_file+15 = '-'.
    par_file+16 = sy-timlo.
    CONCATENATE par_file '.txt' INTO par_file.
    CONDENSE par_file NO-GAPS.
  ENDIF.

*// === 2011.09.15 ECC6 Upgrade change by yn.kim ===== //*
  l_filename = par_file.

***  CALL FUNCTION 'WS_DOWNLOAD'
***       EXPORTING
***            filename        = par_file
***       TABLES
***            data_tab        = final_zshrvaatz
***       EXCEPTIONS
***            file_open_error = 1
***            OTHERS          = 2.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename                        = l_filename
      filetype                        = 'DAT'
*   IMPORTING
*     FILELENGTH                      =
    TABLES
      data_tab                        = final_zshrvaatz
    EXCEPTIONS
      file_write_error                = 1
      no_batch                        = 2
      gui_refuse_filetransfer         = 3
      invalid_type                    = 4
      no_authority                    = 5
      unknown_error                   = 6
      header_not_allowed              = 7
      separator_not_allowed           = 8
      filesize_not_allowed            = 9
      header_too_long                 = 10
      dp_error_create                 = 11
      dp_error_send                   = 12
      dp_error_write                  = 13
      unknown_dp_error                = 14
      access_denied                   = 15
      dp_out_of_memory                = 16
      disk_full                       = 17
      dp_timeout                      = 18
      file_not_found                  = 19
      dataprovider_exception          = 20
      control_flush_error             = 21
      OTHERS                          = 22.

*//  =============== change end =================== //*

  IF sy-subrc <> 0.
    MESSAGE s000 WITH 'ERROR OPENING/DOWNLOADING TO PC FILE.'.
  ELSE.
    MESSAGE s000 WITH 'File was created successfully!:' par_file.
  ENDIF.

ENDFORM.                    " create_file
**&---------------------------------------------------------------------
**
**&      Form  get_dep_code
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*FORM get_dep_code.
*
*
*  CHECK NOT $job_pos[] IS INITIAL.
*
*  DATA : BEGIN OF $sobid OCCURS 0,
*          objid LIKE hrp1000-objid,
*         END OF $sobid.
*
*  LOOP AT $job_pos.
*    $sobid-objid = $job_pos-$sobid.
*    APPEND $sobid.
*  ENDLOOP.
*
*  SORT $sobid.
*  DELETE ADJACENT DUPLICATES FROM $sobid.
*
*  CHECK NOT $sobid[] IS INITIAL.
*
*  SELECT objid sobid INTO TABLE $dep_code
*      FROM hrp1001
*      FOR ALL ENTRIES IN $sobid
*                    WHERE otype EQ 'S'
*                      AND objid EQ $sobid-objid
*                      AND rsign EQ 'A'
*                      AND relat = '003'
*                      AND endda >= par_date
*                      AND begda <= par_date.
*
*  LOOP AT $dep_code.
*    $dep_code-$sobid = $dep_code-sobid(8).
*    MODIFY $dep_code INDEX sy-tabix TRANSPORTING $sobid.
*  ENDLOOP.
*  SORT  $dep_code BY  objid $sobid.
*
*ENDFORM.                    " get_dep_code
*---------------------------------------------------------------------*
*       FORM show_progress                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  PF_TEXT                                                       *
*  -->  VALUE(PF_VAL)                                                 *
*---------------------------------------------------------------------*
FORM show_progress USING    pf_text
                            value(pf_val).

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = pf_val
      text       = pf_text.

ENDFORM.                    " SHOW_PROGRESS

*&---------------------------------------------------------------------*
*&      Form  get_sup_dep_code
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_sup_dep_code.

  __cls $sup_dep_code.

  CHECK NOT $sobid[] IS INITIAL.

  SELECT sobid objid INTO TABLE $sup_dep_code
      FROM hrp1001
      FOR ALL ENTRIES IN $sobid
                    WHERE otype EQ 'O'
                      AND plvar EQ '01'
                      AND istat EQ '1'
                      AND endda EQ '99991231'
                      AND subty EQ 'B002'
                      AND sclas EQ 'O'
                      AND sobid EQ $sobid-sobid.

  PERFORM make_tab_for_sobid TABLES $sup_dep_code.

ENDFORM.                    " get_sup_dep_code
*&---------------------------------------------------------------------*
*&      Form  get_mgr_emp_id
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_mgr_emp_id.

  DATA : BEGIN OF $posid OCCURS 0,
          sobid LIKE hrp1001-sobid,
         END OF $posid.

  __cls : $mgr_pos_id,$mgr_emp_id.

  CHECK NOT $sobid[] IS INITIAL.

  SELECT sobid objid INTO TABLE $mgr_pos_id
      FROM hrp1001
      FOR ALL ENTRIES IN $sobid
                    WHERE otype EQ 'S'
                      AND plvar EQ '01'
                      AND istat EQ '1'
                      AND endda EQ '99991231'
                      AND subty EQ 'A012'
                      AND sclas EQ 'O'
                      AND sobid EQ $sobid-sobid.

  PERFORM make_tab_for_sobid TABLES $mgr_pos_id.

  LOOP AT $mgr_pos_id.
    $posid-sobid = $mgr_pos_id-sobid.
    APPEND $posid.
  ENDLOOP.

  SORT $posid.
  DELETE ADJACENT DUPLICATES FROM $posid.

  SELECT sobid objid INTO TABLE $mgr_emp_id
      FROM hrp1001
      FOR ALL ENTRIES IN $posid
                    WHERE otype EQ 'P'
                      AND plvar EQ '01'
                      AND istat EQ '1'
                      AND endda EQ '99991231'
                      AND subty EQ 'B008'
                      AND sclas EQ 'S'
                      AND sobid EQ $posid-sobid.

  PERFORM make_tab_for_sobid TABLES $mgr_emp_id.

ENDFORM.                    " get_mgr_emp_id
*&---------------------------------------------------------------------*
*&      Form  make_tab_for_sobid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$MGR_POS_ID  text
*----------------------------------------------------------------------*
FORM make_tab_for_sobid TABLES  p_tab STRUCTURE $mgr_pos_id.

  LOOP AT p_tab.
    p_tab-$sobid = p_tab-objid(8).
    MODIFY p_tab INDEX sy-tabix TRANSPORTING $sobid.
  ENDLOOP.
  SORT  p_tab BY $sobid.

ENDFORM.                    " make_tab_for_sobid
*&---------------------------------------------------------------------*
*&      Form  filter_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM filter_table.

  DATA $i_zshrvaatz LIKE i_zshrvaatz OCCURS 0 WITH HEADER LINE.
  DATA g_zshrvaatz LIKE i_zshrvaatz OCCURS 0 WITH HEADER LINE.

  DATA : $flag, $dept_cd LIKE i_zshrvaatz-dept_cd.

  SORT i_zshrvaatz BY dept_cd.

  LOOP AT i_zshrvaatz.
    AT NEW dept_cd.
      $flag = true.
      $dept_cd = i_zshrvaatz-dept_cd.
    ENDAT.
    CHECK $flag EQ true.
    CLEAR $flag.
    __cls $i_zshrvaatz.

    $i_zshrvaatz[] = i_zshrvaatz[].
    DELETE $i_zshrvaatz WHERE dept_cd NE $dept_cd.
    PERFORM take_manager TABLES $i_zshrvaatz.
    APPEND LINES OF $i_zshrvaatz TO g_zshrvaatz.
  ENDLOOP.

  __cls i_zshrvaatz.
  i_zshrvaatz[] = g_zshrvaatz[].

  __cls g_zshrvaatz.

  LOOP AT i_zshrvaatz.
    AT NEW dept_cd.
      $flag = true.
      $dept_cd = i_zshrvaatz-dept_cd.
    ENDAT.
    CHECK $flag EQ true.
    CLEAR $flag.
    __cls $i_zshrvaatz.

    $i_zshrvaatz[] = i_zshrvaatz[].
    DELETE $i_zshrvaatz WHERE dept_cd NE $dept_cd.
    PERFORM pick_top_hir TABLES $i_zshrvaatz.
    APPEND LINES OF $i_zshrvaatz TO g_zshrvaatz.
  ENDLOOP.

  __cls i_zshrvaatz.
  i_zshrvaatz[] = g_zshrvaatz[].


ENDFORM.                    " filter_table
*&---------------------------------------------------------------------*
*&      Form  take_manager
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$I_ZSHRVAATZ  text
*----------------------------------------------------------------------*
FORM take_manager TABLES p_zshrvaatz STRUCTURE i_zshrvaatz.

  DATA $line TYPE i.
  DATA $found.

  DEFINE __filter.

    clear $found.
    perform search_string tables p_zshrvaatz
                          using &1
                       changing $found.

    if $found eq true.
      delete p_zshrvaatz where mc_stext np &1.
    endif.

  END-OF-DEFINITION.

  DELETE p_zshrvaatz WHERE manager_emp_no EQ space.

  DESCRIBE TABLE p_zshrvaatz LINES $line.

  IF $line > 1.

    __filter : 'GENERAL COORDINATOR*',
               'DIRECTOR*',
               'SENIOR COORDINATOR*',
               'SENIOR MANAGER*'.

    CLEAR $found.
    LOOP AT p_zshrvaatz.
      IF p_zshrvaatz-mc_stext(7) EQ 'MANAGER'.
        $found = true.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF $found EQ true.
      DELETE p_zshrvaatz WHERE mc_stext(7) NE 'MANAGER'.
    ENDIF.

  ELSE.
  ENDIF.


ENDFORM.                    " take_manager
*&---------------------------------------------------------------------*
*&      Form  search_string
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_ZSHRVAATZ  text
*      -->P_1222   text
*----------------------------------------------------------------------*
FORM search_string TABLES   p_zshrvaatz STRUCTURE i_zshrvaatz
                   USING    value(p_string)
                CHANGING    $found.

  LOOP AT p_zshrvaatz.
    IF p_zshrvaatz-mc_stext CP p_string.
      $found = true.
      EXIT.
    ENDIF.
  ENDLOOP.


ENDFORM.                    " search_string
*&---------------------------------------------------------------------*
*&      Form  pick_top_hir
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$I_ZSHRVAATZ  text
*----------------------------------------------------------------------*
FORM pick_top_hir TABLES p_zshrvaatz STRUCTURE i_zshrvaatz.

  DATA $line TYPE i.
  DATA $found.
  DATA $cnt TYPE i.
  DATA $ix TYPE i.
  DATA $objid TYPE hrobjid.

  DESCRIBE TABLE p_zshrvaatz LINES $line.

  $cnt = 1.

  IF $line > 1.

    LOOP AT p_zshrvaatz.
      $ix = sy-tabix.
      CLEAR $cnt.
      hrp1001-objid = p_zshrvaatz-objid.

      DO 10 TIMES.
        $objid = hrp1001-objid.
        SELECT SINGLE * FROM hrp1001 WHERE otype EQ 'O'
                            AND plvar EQ '01'
                            AND istat EQ '1'
                            AND endda EQ '99991231'
                            AND subty EQ 'B002'
                            AND sclas EQ 'O'
                            AND sobid EQ $objid.
        IF sy-subrc EQ 0.
        ELSE.
          $cnt = sy-index.
          EXIT.
        ENDIF.
      ENDDO.
      p_zshrvaatz-level = $cnt.
      MODIFY  p_zshrvaatz INDEX $ix TRANSPORTING level.
    ENDLOOP.
  ELSE.
  ENDIF.

ENDFORM.                    " pick_top_hir
