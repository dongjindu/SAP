*&---------------------------------------------------------------------*
*&  Include           ZH_GQMS_IF_DAILY_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  INITIAL_PROC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initial_proc .

  par_file    = 'c:\temp\H_GQ_D'.
  par_file+16  = '-'.
  WRITE sy-datlo TO par_file+17(6) YYMMDD.
  par_file+24 = '-'.
  par_file+25 = sy-timlo.
  CONCATENATE par_file '.txt' INTO par_file.
  CONDENSE par_file NO-GAPS.

ENDFORM.                    " INITIAL_PROC
*&---------------------------------------------------------------------*
*&      Form  DATA_SELECT1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_select1 .


  __cls : itab, i_zshrgqms2.
  __cls : $job_pos,$dep_code,$dep_name, it_t001p.

  u_break.

  __process 'Get from p0001...' '10'.

  SELECT a~pernr a~plans a~sname a~ename
         a~btrtl a~orgeh a~werks a~stell
         c~telnr c~aedtm b~stat2
        FROM pa0001 AS a
        INNER JOIN pa0000 AS b
        ON b~pernr EQ a~pernr
       AND b~begda <= par_date
       AND b~endda >= par_date
       AND b~stat2 EQ '3'
        INNER JOIN pa0006 AS c
        ON c~pernr EQ a~pernr
       AND c~begda <= par_date
       AND c~endda >= par_date
      INTO CORRESPONDING FIELDS OF TABLE itab
    WHERE a~pernr IN s_pernr
      AND a~begda <= par_date
      AND a~endda >= par_date.

  IF sy-subrc NE 0.
    MESSAGE s000 WITH 'No data was found!'.
    STOP.
  ENDIF.

  SORT itab BY pernr ASCENDING aedtm DESCENDING.
  DELETE ADJACENT DUPLICATES FROM itab
    COMPARING pernr.

  __process 'Get div.text...' '20'.
  SELECT * INTO TABLE it_t001p FROM t001p.
  SORT it_t001p BY werks btrtl.

  SORT itab BY pernr.

  __process 'Get job position...' '30'.
  PERFORM  get_job_pos.

*  __process 'Get dep.code...' '40'.
*  PERFORM  get_dep_code.                  " need to be changed

  __process 'Get grp.code...' '50'.
  PERFORM  get_grp_name.

  __process 'Get dep.name...' '60'.
  PERFORM  get_dep_name.

  __process 'Making I/F data...' '70'.

  PERFORM get_stell_desc.

* 6/8/2009 by ig.moon {
* Calling weekly report to get dept code.
  SUBMIT zh_gqms_if_weekly WITH p_call EQ true
         AND RETURN.

  IMPORT : i_zshrgqms1 FROM MEMORY ID 'ZHGQMSWEEKLY',
           i_mc_stext FROM MEMORY ID 'ZHGQMSWEEKLYMC'.

*  }
  LOOP AT itab.

    i_zshrgqms2-company_code = 'A'.
    i_zshrgqms2-user_id      = itab-pernr.

    CONCATENATE 'A' itab-pernr+2 INTO i_zshrgqms2-user_id.

    i_zshrgqms2-emp_name_kor = itab-sname.
    i_zshrgqms2-emp_name_eng = itab-sname.
    i_zshrgqms2-emp_name_loc = itab-ename.

    i_zshrgqms2-grp_code     = itab-orgeh.


    READ TABLE $grp_name WITH KEY objid = itab-orgeh
                              BINARY SEARCH.
    IF sy-subrc EQ 0.
      i_zshrgqms2-grp_name = $grp_name-stext.
    ELSE.
      u_break.
    ENDIF.

* 6/8/2009 by ig.moon {

    PERFORM get_top_dept USING itab-orgeh
                      CHANGING i_zshrgqms2-dep_code
                               i_zshrgqms2-dep_name.

    IF i_zshrgqms2-dep_code IS INITIAL.

      i_zshrgqms2-dep_code = i_zshrgqms2-grp_code.
      i_zshrgqms2-dep_name = i_zshrgqms2-grp_name.

    ENDIF.

* }

    i_zshrgqms2-div_code     = itab-btrtl.

    READ TABLE it_t001p WITH KEY werks = itab-werks
                                 btrtl = itab-btrtl
                                 BINARY SEARCH.
    IF sy-subrc EQ 0.
      i_zshrgqms2-div_name = it_t001p-btext.
    ELSE.
      u_break.
    ENDIF.

*    ELSE.
*      u_break.
*    ENDIF.


*    i_zshrgqms2-chg_work = itab-plans.
    i_zshrgqms2-job_pos  = itab-stell. "$job_pos-sobid.

* 6/6/2009 - Disabled : Dept. Code extracted by relationship 'Z01' {
*    read table $dep_code_new with key pernr = itab-pernr binary search.
*    if sy-subrc eq 0.
*      i_zshrgqms2-dep_code = $dep_code_new-plans.
*      read table i_dept with key  sobid = $dep_code_new-plans binary
*search.
*      if sy-subrc eq 0.
*        i_zshrgqms2-DEP_NAME = i_dept-stext.
*      endif.
*    else.
*      i_zshrgqms2-dep_code = space. "$dep_code-sobid.
*    endif.
* }

    READ TABLE $stell_desc WITH KEY stell = itab-stell BINARY SEARCH.
    IF sy-subrc EQ 0.
      i_zshrgqms2-chg_work = $stell_desc-stext.
    ENDIF.

    i_zshrgqms2-region       = itab-werks.
    i_zshrgqms2-telnr        = itab-telnr.
    i_zshrgqms2-c_telnr      = space.
    i_zshrgqms2-e_mail       = space.
    i_zshrgqms2-stat2        = space. "itab-stat2.
    i_zshrgqms2-eai_date     = par_date.
    i_zshrgqms2-eai_id       = 'HMMAEAI'.
    APPEND i_zshrgqms2.
    CLEAR i_zshrgqms2.

  ENDLOOP.

  __process 'Finalizing...' '95'.


ENDFORM.                    " DATA_SELECT1
*&---------------------------------------------------------------------*
*&      Form  gqms_eai
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gqms_eai.

  SELECT SINGLE * FROM rfcdes  INTO *rfcdes
                WHERE rfcdest EQ par_dest.
  IF sy-subrc NE 0.
    MESSAGE s000 WITH 'No RFC Destination was found!'.
    EXIT.
  ENDIF.
  CALL FUNCTION 'Z_HR_GQMS_IF_DAILY'
    DESTINATION par_dest
    TABLES
      zshrgqms2 = i_zshrgqms2.

  IF sy-subrc NE 0.
    MESSAGE s000 WITH 'I/F error was occured!'.
  ENDIF.

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
    par_file    = 'HR_GQMS'.
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
***            data_tab        = i_zshrgqms2
***       EXCEPTIONS
***            file_open_error = 1
***            OTHERS          = 2.

*//
  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename                = l_filename
      filetype                = 'DAT'
    TABLES
      data_tab                = i_zshrgqms2
    EXCEPTIONS
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
      OTHERS                  = 22.


*//  =============== change end =================== //*

*//
  IF sy-subrc <> 0.
    MESSAGE s000 WITH 'ERROR OPENING/DOWNLOADING TO PC FILE.'.
  ELSE.
    MESSAGE s000 WITH 'File was created successfully!:' par_file.
  ENDIF.

ENDFORM.                    " create_file
*&---------------------------------------------------------------------*
*&      Form  get_job_pos
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_job_pos.

  SELECT objid sobid
      INTO TABLE $job_pos
      FROM hrp1001
      FOR ALL ENTRIES IN itab
            WHERE otype EQ 'P'
                  AND objid EQ itab-pernr
                  AND rsign EQ 'B'
                  AND relat = '008'
                  AND endda >= par_date
                  AND begda <= par_date.
  LOOP AT $job_pos.
    $job_pos-$sobid = $job_pos-sobid(8).
    MODIFY $job_pos INDEX sy-tabix TRANSPORTING $sobid.
  ENDLOOP.
  SORT  $job_pos BY objid.

ENDFORM.                    " get_job_pos
*&---------------------------------------------------------------------*
*&      Form  get_dep_code
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_dep_code.
  DATA $ix LIKE sy-tabix.

  __cls i_dept.

  SELECT a~objid b~stext a~sobid INTO TABLE i_dept
  FROM hrp1001 AS a INNER JOIN hrp1000 AS b
  ON b~otype EQ a~otype
  AND b~endda EQ a~endda
  AND b~objid EQ a~objid
               WHERE a~otype EQ 'O'
                AND a~endda EQ '99991231'
                AND a~relat EQ 'Z01' .

  LOOP AT i_dept.
    $ix = sy-tabix.
    i_dept-plans = i_dept-sobid.
    MODIFY i_dept INDEX $ix.
  ENDLOOP.

  CHECK NOT i_dept[] IS INITIAL.

  __cls $dep_code_new.

  SELECT pernr plans INTO TABLE $dep_code_new
      FROM pa0001
      FOR ALL ENTRIES IN i_dept
                    WHERE plans EQ i_dept-plans
                      AND endda EQ '99991231'.

  SORT $dep_code_new BY pernr.
  SORT i_dept BY sobid.

*  check not $job_pos[] is initial.
*
*  data : begin of $sobid occurs 0,
*          objid like hrp1000-objid,
*         end of $sobid.
*
*  loop at $job_pos.
*    $sobid-objid = $job_pos-$sobid.
*    append $sobid.
*  endloop.
*
*  sort $sobid.
*  delete adjacent duplicates from $sobid.
*
*  check not $sobid[] is initial.
*
*  select objid sobid into table $dep_code
*      from hrp1001
*      for all entries in $sobid
*                    where otype eq 'S'
*                      and objid eq $sobid-objid
*                      and rsign eq 'A'
*                      and relat = '003'
*                      and endda >= par_date
*                      and begda <= par_date.
*
*  loop at $dep_code.
*    $dep_code-$sobid = $dep_code-sobid(8).
*    modify $dep_code index sy-tabix transporting $sobid.
*  endloop.
*  sort  $dep_code by  objid $sobid.

ENDFORM.                    " get_dep_code
*&---------------------------------------------------------------------*
*&      Form  get_dep_name
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_dep_name.

  CHECK NOT $dep_code[] IS INITIAL.

  DATA : BEGIN OF $sobid OCCURS 0,
          objid LIKE hrp1000-objid,
         END OF $sobid.

  LOOP AT $dep_code.
    $sobid-objid = $dep_code-$sobid.
    APPEND $sobid.
  ENDLOOP.

  SORT $sobid.
  DELETE ADJACENT DUPLICATES FROM $sobid.

  CHECK NOT $sobid[] IS INITIAL.

  SELECT objid stext INTO TABLE $dep_name
      FROM hrp1000
      FOR ALL ENTRIES IN $sobid
                      WHERE otype EQ 'O'
                        AND objid EQ $sobid-objid
                        AND endda >= par_date
                        AND begda <= par_date .

  SORT $dep_name BY objid.

ENDFORM.                    " get_dep_name
*&---------------------------------------------------------------------*
*&      Form  get_grp_code
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_grp_name.

  CHECK NOT itab[] IS INITIAL.

  DATA : BEGIN OF $orgeh OCCURS 0,
          orgeh LIKE hrp1000-objid,
         END OF $orgeh.

  LOOP AT itab.
    $orgeh-orgeh = itab-orgeh.
    APPEND $orgeh.
  ENDLOOP.

  SORT $orgeh.
  DELETE ADJACENT DUPLICATES FROM $orgeh.

  CHECK NOT $orgeh[] IS INITIAL.

  SELECT objid stext INTO TABLE $grp_name
      FROM hrp1000
      FOR ALL ENTRIES IN $orgeh
                      WHERE otype EQ 'O'
                        AND objid EQ $orgeh-orgeh
                        AND endda >= par_date
                        AND begda <= par_date .

  SORT  $grp_name BY  objid.

ENDFORM.                    " get_grp_code

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
*&      Form  get_stell_desc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_stell_desc.
  __cls $stell_desc.

  CHECK NOT itab[] IS INITIAL.

  SELECT objid stext INTO TABLE $stell_desc FROM hrp1000
  FOR ALL ENTRIES IN itab WHERE otype EQ 'C'
                        AND objid EQ itab-stell.

  SORT $stell_desc BY stell.

ENDFORM.                    " get_stell_desc
*&---------------------------------------------------------------------*
*&      Form  get_top_dept
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ITAB_ORGEH  text
*      <--P_I_ZSHRGQMS2_DEP_CODE  text
*      <--P_I_ZSHRGQMS2_DEP_NAME  text
*----------------------------------------------------------------------*
FORM get_top_dept USING    p_orgeh
                  CHANGING p_dep_code
                           p_dep_name.

  DATA $p_orgeh LIKE itab-orgeh.
  DATA $mc_stext LIKE hrp1000-mc_stext.

  $p_orgeh = p_orgeh.

  READ TABLE i_zshrgqms1 WITH KEY qz31code = $p_orgeh.
  IF sy-subrc EQ 0.
    p_dep_code = i_zshrgqms1-qz31code.
    p_dep_name = i_zshrgqms1-qz31desk.
    EXIT.
  ENDIF.

  DO 30 TIMES.

    SELECT SINGLE * FROM hrp1001
                    WHERE sobid = $p_orgeh
                      AND rsign = 'B'
                      AND relat = '002'
                      AND endda EQ '99991231'.

    IF sy-subrc EQ 0.
      READ TABLE i_zshrgqms1 WITH KEY qz31code = hrp1001-objid.
      IF sy-subrc NE 0.
        $p_orgeh = hrp1001-objid.
      ELSE.
        p_dep_code = i_zshrgqms1-qz31code.
        p_dep_name = i_zshrgqms1-qz31desk.
        EXIT.
      ENDIF.
    ELSE.
      EXIT.
    ENDIF.

  ENDDO.


  IF p_dep_code IS INITIAL.
    READ TABLE i_mc_stext WITH KEY objid = p_orgeh.
    IF sy-subrc EQ 0.
      $mc_stext = i_mc_stext-mc_stext.
      REPLACE 'COORDINATOR' WITH 'DEPARTMENT' INTO $mc_stext.
    ENDIF.
    READ TABLE i_zshrgqms1 WITH KEY qz31desk = $mc_stext.
    IF sy-subrc EQ 0.
      p_dep_code = i_zshrgqms1-qz31code.
      p_dep_name = i_zshrgqms1-qz31desk.
    ENDIF.
  ENDIF.

ENDFORM.                    " get_top_dept
