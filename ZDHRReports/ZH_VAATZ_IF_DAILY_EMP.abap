REPORT zh_vaatz_if_daily_emp MESSAGE-ID zmco.

*-----------------------------------------------------------------------
* Name: ZH_VAATZ_IF- HR VAATZ daily Interface for Employee
* Tech. Resource: Imtiaz Ahmad
* Desc: HR VAATZ daily interface for Employee
*----------------------------------------------------------------------
*----------------------------------------------------------------------*
*  Title          : ZH_VAATZ_IF_DAILY_EMP
*  Author         : ig.moon
*  Creation Data  : 8/12/2008
*  Requirements by: Imtiaz Ahmad
*  Description    : HR VAATZ daily interface for Employee.

************************************************************************
* CHANGE LOG
*-----------------------------------------------------------------------
* DATE      |  NAME          |Transport | Issue #  |      DESC
*-----------------------------------------------------------------------
* 2011.09.15   yn.kim         UP1K920005            ECC6.0 Upgrade.
* 01/24/2012   Valerian       UD1K953779  Bug fix (Some TM's records
*                                         are suppressed)
* 02/17/2012   Valerian       UD1K953997  Remove prefix '00000' of the
*                                         cost center
*-----------------------------------------------------------------------

***********************************************************************

*                      --- TABLES ---
*----------------------------------------------------------------------
TABLES: p0001,             "Infotype 0001 - org. data
        zshrvaatzemp ,        "Structure for VAATZ Interface
        hrp1001,hrp1000,
       *hrp1001,*hrp1000,t001p, zthrvaatzemp,
        *rfcdes.

DATA : BEGIN OF itab OCCURS 0,
        pernr LIKE p0001-pernr,
        plans LIKE p0001-plans,
        sname LIKE p0001-sname,
        ename LIKE p0001-ename,
        btrtl LIKE p0001-btrtl,
        orgeh LIKE p0001-orgeh,
        werks LIKE p0001-werks,
        stell LIKE p0001-stell,
        telnr LIKE p0006-telnr,
        num01 LIKE p0006-num01,
        aedtm LIKE p0006-aedtm,
        stat2 LIKE p0000-stat2,
        begda LIKE p0000-begda,
        kostl LIKE p0001-kostl,
        massn LIKE p0000-massn,
        massg LIKE p0000-massg,
       END OF itab.

DATA itab_33301 LIKE itab OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF $job_pos OCCURS 0,
        objid LIKE hrp1001-objid,
        sobid LIKE hrp1001-sobid,
       $sobid LIKE hrp1001-objid,
       END OF $job_pos.

DATA : BEGIN OF $job_name OCCURS 0,
        objid LIKE hrp1001-objid,
        mc_stext TYPE hr_mcstext,
       END OF $job_name.

DATA : BEGIN OF $dep_code OCCURS 0,
        objid LIKE hrp1001-objid,
        sobid LIKE hrp1001-sobid,
       $sobid LIKE hrp1001-objid,
       END OF $dep_code.

DATA : BEGIN OF $manager OCCURS 0,
        objid LIKE hrp1001-objid,
       END OF $manager.

DATA : BEGIN OF $dep_name OCCURS 0,
        objid LIKE hrp1000-objid,
        stext LIKE hrp1000-stext,
       END OF $dep_name.

DATA : BEGIN OF $grp_name OCCURS 0,
        objid LIKE hrp1000-objid,
        stext LIKE hrp1000-stext,
       END OF $grp_name.

DATA : BEGIN OF $dep_name_cskt OCCURS 0,
        kostl LIKE cskt-kostl,
        ktext LIKE cskt-ktext,
       END OF $dep_name_cskt.

DATA   num(12) VALUE ' 0123456789'.


DATA  it_t001p LIKE t001p OCCURS 20 WITH HEADER LINE.

*--------------------------------------------------------------------*
* SELECTION-SCREEN
*--------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK 0 WITH FRAME TITLE text-100.
SELECT-OPTIONS s_pernr FOR p0001-pernr MATCHCODE OBJECT prem.
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

DATA BEGIN OF i_zshrvaatzemp OCCURS 0.
        INCLUDE STRUCTURE zshrvaatzemp.
DATA : job_pos  LIKE hrp1000-objid,
       div_code LIKE p0001-btrtl,
       grp_code LIKE pa0001-orgeh,
       dep_name LIKE hrp1000-stext,
       grp_name LIKE hrp1000-stext,
       div_name LIKE hrp1000-stext,
       mc_stext LIKE hrp1000-mc_stext.
DATA END OF i_zshrvaatzemp.

DATA BEGIN OF g_zshrvaatzemp OCCURS 0.
        INCLUDE STRUCTURE zshrvaatzemp.
*DATA : job_pos  LIKE hrp1000-objid.
DATA END OF g_zshrvaatzemp.

DATA g_zthrvaatzemp LIKE zthrvaatzemp OCCURS 0 WITH HEADER LINE .

INITIALIZATION.

  par_file    = 'c:\temp\HRVZEMP'.
  par_file+16  = '-'.
  WRITE sy-datlo TO par_file+17(6) YYMMDD.
  par_file+24 = '-'.
  par_file+25 = sy-timlo.
  CONCATENATE par_file '.txt' INTO par_file.
  CONDENSE par_file NO-GAPS.

*--------------------------------------------------------------------*
START-OF-SELECTION.
*--------------------------------------------------------------------*

  __cls : itab, i_zshrvaatzemp.
  __cls : $job_pos,$dep_code,$dep_name, it_t001p.

  u_break.

  __process 'Get from p0001...' '10'.

  SELECT a~pernr a~plans a~sname a~ename
         a~btrtl a~orgeh a~werks a~stell a~kostl
         c~telnr c~num01 c~aedtm b~stat2 b~begda
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

* by ig.moon 3/9/2009 {

  __cls itab_33301.

  SELECT a~pernr a~plans a~sname a~ename
         a~btrtl a~orgeh a~werks a~stell a~kostl
         c~telnr c~num01 c~aedtm b~stat2 b~begda b~massn b~massg
        FROM pa0001 AS a
        INNER JOIN pa0000 AS b
        ON b~pernr EQ a~pernr
       AND b~begda <= par_date
       AND b~endda >= par_date
       AND ( b~stat2 NE '1' AND b~stat2 NE '3' )
        INNER JOIN pa0006 AS C
        ON c~pernr EQ a~pernr
       AND c~begda <= par_date
       AND c~endda >= par_date
      INTO CORRESPONDING FIELDS of TABLE itab_33301
    WHERE a~pernr IN s_pernr
      AND a~begda <= par_date
      AND a~endda >= par_date
      AND a~kostl EQ '0000033301'.

  DATA $ix TYPE i.

  LOOP AT itab_33301.
    $ix = sy-tabix.
    IF itab_33301-massn EQ 'ZX' AND itab_33301-massg EQ '17'.
    ELSE.
      DELETE itab_33301 INDEX $ix.
    ENDIF.
  ENDLOOP.

  APPEND LINES OF itab_33301 TO itab.
* }

  IF itab[] IS INITIAL.                                     "UD1K953779
* IF sy-subrc NE 0.                                         "UD1K953779
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

  __process 'Get dep.code...' '40'.
  PERFORM  get_dep_code.

  __process 'Get grp.code...' '50'.
  PERFORM  get_grp_name.

  __process 'Get dep.name...' '60'.
  PERFORM  get_dep_name.

  __process 'Making I/F data...' '70'.

  LOOP AT itab.

    i_zshrvaatzemp-corp_gb      = 'A1'.
    i_zshrvaatzemp-com_cd       = 'HMMA'.
    i_zshrvaatzemp-emp_no       = itab-pernr+2.

    REPLACE 'Mr ' WITH '' INTO itab-ename.
    REPLACE 'Mrs ' WITH '' INTO itab-ename.
    REPLACE 'Miss ' WITH '' INTO itab-ename.
    REPLACE 'Ms '  WITH '' INTO itab-ename.

    i_zshrvaatzemp-user_nm_ext = itab-ename.
    i_zshrvaatzemp-user_nm_eng = itab-ename.

*    i_zshrvaatzemp-dept_cd    = itab-orgeh.
*    i_zshrvaatzemp-up_dept_cd = itab-orgeh.

    CLEAR $job_pos.

    READ TABLE $job_pos WITH KEY objid = itab-pernr
                               BINARY SEARCH.

    IF sy-subrc EQ 0.
      i_zshrvaatzemp-job_pos = $job_pos-$sobid.
      READ TABLE $dep_code WITH KEY objid = $job_pos-$sobid
                                 BINARY SEARCH.
      IF sy-subrc EQ 0.

* disbled by ig.moon {
*        i_zshrvaatzemp-dept_cd = $dep_code-$sobid.
*        i_zshrvaatzemp-up_dept_cd = $dep_code-$sobid.
*
*        READ TABLE $dep_name WITH KEY objid = $dep_code-$sobid
*                                  BINARY SEARCH.
*        IF sy-subrc EQ 0.
*
*          i_zshrvaatzemp-dep_name = $dep_name-stext.
*
*        ELSE.
*          u_break.
*        ENDIF.
* }

        READ TABLE $manager WITH KEY objid = $job_pos-$sobid
                                   BINARY SEARCH.
        IF sy-subrc EQ 0.
          i_zshrvaatzemp-position_yn  = 'Y'.
        ELSE.
          i_zshrvaatzemp-position_yn  = 'N'.
        ENDIF.

        READ TABLE $job_name WITH KEY objid = $job_pos-$sobid
                                   BINARY SEARCH.
        IF sy-subrc EQ 0.
          i_zshrvaatzemp-mc_stext = $job_name-mc_stext.
        ENDIF.

      ELSE.
        u_break.
      ENDIF.

      i_zshrvaatzemp-grp_code     = itab-orgeh.
      READ TABLE $grp_name WITH KEY objid = itab-orgeh
                                BINARY SEARCH.
      IF sy-subrc EQ 0.
        i_zshrvaatzemp-grp_name = $grp_name-stext.
      ELSE.
        u_break.
      ENDIF.

      i_zshrvaatzemp-div_code     = itab-btrtl.

      READ TABLE it_t001p WITH KEY werks = itab-werks
                                   btrtl = itab-btrtl
                                   BINARY SEARCH.
      IF sy-subrc EQ 0.
        i_zshrvaatzemp-div_name = it_t001p-btext.
      ELSE.
        u_break.
      ENDIF.
    ELSE.
      u_break.
    ENDIF.

* {

    IF itab-kostl CN num.
      i_zshrvaatzemp-dept_cd    = itab-kostl.
      i_zshrvaatzemp-up_dept_cd = itab-kostl.
    ELSE.
      i_zshrvaatzemp-dept_cd    = itab-kostl+5.
      i_zshrvaatzemp-up_dept_cd = itab-kostl+5.
    ENDIF.

*    IF i_zshrvaatzemp-dept_cd CN '0123456789 '.
*      CONCATENATE  '^^^^^' i_zshrvaatzemp-dept_cd
*        INTO i_zshrvaatzemp-dept_cd.
*      REPLACE '^^^^^' with '     ' INTO i_zshrvaatzemp-dept_cd.
*    ELSE.
* BEGIN OF UD1K953997
*    CONCATENATE  '00000' i_zshrvaatzemp-dept_cd
*      INTO i_zshrvaatzemp-dept_cd.
*    ENDIF.

*    READ TABLE $dep_name_cskt WITH KEY kostl = i_zshrvaatzemp-dept_cd
*                              BINARY SEARCH.
    READ TABLE $dep_name_cskt WITH KEY kostl = itab-kostl
                              BINARY SEARCH.
* END OF UD1K953997
    IF sy-subrc EQ 0.

      i_zshrvaatzemp-dep_name = $dep_name_cskt-ktext.

    ELSE.
      u_break.
    ENDIF.

* }
    i_zshrvaatzemp-tel_no        = itab-telnr.
    i_zshrvaatzemp-cell_phone_no = itab-num01.
    i_zshrvaatzemp-fax_no = space.
    i_zshrvaatzemp-enter_base = space.
    i_zshrvaatzemp-enter_date = itab-begda.
    i_zshrvaatzemp-work_area  = 'A1'.

    i_zshrvaatzemp-official_order = space.
    i_zshrvaatzemp-official_order_date = space.

    IF itab-stat2 EQ '0' OR itab-stat2 EQ '2'.
      i_zshrvaatzemp-retr_yn = 'Y'.
    ENDIF.

    i_zshrvaatzemp-trans_date   = par_date.
    APPEND i_zshrvaatzemp.
    CLEAR i_zshrvaatzemp.

  ENDLOOP.

  __process 'Finalizing...' '95'.

  __cls g_zshrvaatzemp.

  LOOP AT i_zshrvaatzemp.

    CLEAR g_zshrvaatzemp.

    PERFORM get_pos_cd CHANGING i_zshrvaatzemp-pos_cd.
    IF i_zshrvaatzemp-dep_name CP '*Sub Division*'
      AND i_zshrvaatzemp-pos_cd EQ '02'.
      i_zshrvaatzemp-duty_cd = '65'.
    ELSE.

*01   President        President
*02   Vice President  Vice President

*03   General Director  General Director
*04   Senior Director Senior Director
*05   Director         Director
*20   Director (Etc)  Director (Etc)

*22   Senior Manager  Senior Manager

*24   General Manager General Manager
*26   Manager          Manager
*28   Assistant Manager Assistant Manager
*30   Team Mumber        Team Member

      CASE i_zshrvaatzemp-pos_cd.
        WHEN '01'.
          i_zshrvaatzemp-duty_cd = 'B'.
        WHEN '02'.
          i_zshrvaatzemp-duty_cd = 'C'.
        WHEN '22'.
          i_zshrvaatzemp-duty_cd = 'F'.
        WHEN '24' OR '26'.
          i_zshrvaatzemp-duty_cd = 'G'.
        WHEN OTHERS.
          i_zshrvaatzemp-duty_cd = ' '.
      ENDCASE.

*      CASE i_zshrvaatzemp-pos_cd.
*        WHEN '01'.
*          i_zshrvaatzemp-duty_cd = 'B'.
*        WHEN '02'.
*          i_zshrvaatzemp-duty_cd = 'C'.
*        WHEN '22'.
*          IF i_zshrvaatzemp-position_yn EQ 'Y'.
*            i_zshrvaatzemp-duty_cd = 'F'.
*          ELSE.
*            i_zshrvaatzemp-pos_cd = '26'.
*            i_zshrvaatzemp-duty_cd = 'G'.
*          ENDIF.
*        WHEN '24' OR '26'.
*          i_zshrvaatzemp-duty_cd = 'G'.
*        WHEN OTHERS.
*          i_zshrvaatzemp-duty_cd = ' '.
*      ENDCASE.

    ENDIF.

    MOVE-CORRESPONDING i_zshrvaatzemp TO g_zshrvaatzemp.

    g_zshrvaatzemp-trans_date           = sy-datum.

    APPEND g_zshrvaatzemp.
    CLEAR g_zshrvaatzemp.

  ENDLOOP.

*--------------------------------------------------------------------*
END-OF-SELECTION.
*--------------------------------------------------------------------*

  IF par_r1 EQ true.
    PERFORM create_file.
  ELSE.
    PERFORM gqms_eai.
  ENDIF.


*&---------------------------------------------------------------------*
*&      Form  gqms_eai
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gqms_eai.

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

  CALL FUNCTION 'Z_HR_VAATZ_IF_DAILY_EMP'
    DESTINATION par_dest
    TABLES
      zshrvaatzemp = g_zshrvaatzemp.

  IF sy-subrc NE 0.
    $result = 'L'.
    MESSAGE s000 WITH 'I/F error was occured!'.
  ENDIF.

* by ig.moon 3/6/2009 {

  $del_date = sy-datum - 10.
  $date = sy-datum.
  $time = sy-uzeit.

  DELETE FROM zthrvaatzemp WHERE zedat <= $del_date.
  COMMIT WORK.

  __cls g_zthrvaatzemp.

  LOOP AT g_zshrvaatzemp.

    CLEAR g_zthrvaatzemp.
    MOVE-CORRESPONDING g_zshrvaatzemp TO g_zthrvaatzemp.
    g_zthrvaatzemp-zedat = $date.
    g_zthrvaatzemp-zetim = $time.
    g_zthrvaatzemp-zresult =  $result.
    g_zthrvaatzemp-zuser = sy-uname.
    APPEND g_zthrvaatzemp.

  ENDLOOP.

  MODIFY zthrvaatzemp FROM TABLE g_zthrvaatzemp.
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
    par_file    = 'HRVZEMP'.
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
***            data_tab        = g_zshrvaatzemp
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
      data_tab                        = g_zshrvaatzemp
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

  CHECK NOT $job_pos[] IS INITIAL.

  DATA : BEGIN OF $sobid OCCURS 0,
          objid LIKE hrp1000-objid,
         END OF $sobid.

  LOOP AT $job_pos.
    $sobid-objid = $job_pos-$sobid.
    APPEND $sobid.
  ENDLOOP.

  SORT $sobid.
  DELETE ADJACENT DUPLICATES FROM $sobid.

  CHECK NOT $sobid[] IS INITIAL.

  SELECT objid sobid INTO TABLE $dep_code
      FROM hrp1001
      FOR ALL ENTRIES IN $sobid
                    WHERE otype EQ 'S'
                      AND objid EQ $sobid-objid
                      AND rsign EQ 'A'
                      AND relat = '003'
                      AND endda >= par_date
                      AND begda <= par_date.

  LOOP AT $dep_code.
    $dep_code-$sobid = $dep_code-sobid(8).
    MODIFY $dep_code INDEX sy-tabix TRANSPORTING $sobid.
  ENDLOOP.
  SORT  $dep_code BY  objid $sobid.

  __cls $manager.

  SELECT objid INTO TABLE $manager
      FROM hrp1001
      FOR ALL ENTRIES IN $sobid
                    WHERE otype EQ 'S'
                      AND objid EQ $sobid-objid
                      AND plvar EQ '01'
                      AND istat EQ '1'
                      AND begda <= par_date
                      AND endda EQ '99991231'
                      AND subty EQ 'A012'.

  SORT $manager BY objid.

  __cls $job_name.
  SELECT objid mc_stext INTO TABLE $job_name
      FROM hrp1000
      FOR ALL ENTRIES IN $sobid
                    WHERE

                          otype EQ 'S'
                      AND plvar EQ '01'
                      AND istat EQ '1'
                      AND begda <= par_date

                      AND objid EQ $sobid-objid
                      AND endda EQ '99991231'.

  SORT $job_name BY objid.
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

*  CHECK NOT $sobid[] IS INITIAL.
*
*  SELECT objid stext INTO TABLE $dep_name
*      FROM hrp1000
*      FOR ALL ENTRIES IN $sobid
*                      WHERE otype EQ 'O'
*                        AND objid EQ $sobid-objid
*                        AND endda >= par_date
*                        AND begda <= par_date .
*
*  SORT $dep_name BY objid.

  __cls $dep_name_cskt.

  SELECT kostl ktext INTO TABLE $dep_name_cskt
  FROM cskt WHERE
  spras EQ sy-langu
  AND kokrs EQ 'H201'
  AND datbi EQ '99991231'.

  SORT $dep_name_cskt BY kostl.

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
*&      Form  get_pos_cd
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_G_ZSHRVAATZEMP_POS_CD  text
*----------------------------------------------------------------------*
FORM get_pos_cd CHANGING p_cd.

*01   President        President
*02   Vice President  Vice President

*03   General Director  General Director
*04   Senior Director Senior Director
*05   Director         Director
*20   Director (Etc)  Director (Etc)

*22   Senior Manager  Senior Manager
*24   General Manager General Manager
*26   Manager          Manager
*28   Assistant Manager Assistant Manager
*30   Team Mumber        Team Mumber

  DATA $grp_name LIKE i_zshrvaatzemp-grp_name.

  $grp_name = i_zshrvaatzemp-grp_name.
  TRANSLATE $grp_name TO UPPER CASE.

  IF i_zshrvaatzemp-mc_stext CP '*PRESIDENT OF HMMA*'.
    p_cd = '01'.
    EXIT.
  ENDIF.

  " Vice President.
  IF i_zshrvaatzemp-mc_stext CP '*VICE PRESIDENT OF*'.
    p_cd = '02'.
    EXIT.
  ELSE.
    IF $grp_name CP '*SUB DIVISION*'.
      IF i_zshrvaatzemp-mc_stext CP '*DIRECTOR OF*'.
        p_cd = '02'.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.

  IF i_zshrvaatzemp-mc_stext CP  '*GENERAL DIRECTOR*'.
    p_cd = '03'.
    EXIT.
  ENDIF.

  IF i_zshrvaatzemp-mc_stext CP  '*SENIOR DIRECTOR*'
      OR i_zshrvaatzemp-mc_stext CP '*SR. DIRECTOR*'.
    p_cd = '04'.
    EXIT.
  ENDIF.

  IF i_zshrvaatzemp-mc_stext CP  '*DIRECTOR*'.
    p_cd = '05'.
    EXIT.
  ENDIF.

  IF i_zshrvaatzemp-mc_stext CP  '*SENIOR MANAGER*'.
    p_cd = '22'.
    EXIT.
  ENDIF.

  IF i_zshrvaatzemp-mc_stext CP  '*SR MANAGER*'.
    p_cd = '22'.
    EXIT.
  ENDIF.

  IF i_zshrvaatzemp-mc_stext CP  '*COORDI*'.
    p_cd = '22'.
    EXIT.
  ENDIF.

  IF i_zshrvaatzemp-mc_stext CP  '*ASSISTANT MANAGER*'.
    p_cd = '28'.
    EXIT.
  ENDIF.
  IF i_zshrvaatzemp-mc_stext CP  '*ASSISTANT MGR*'.
    p_cd = '28'.
    EXIT.
  ENDIF.
  IF i_zshrvaatzemp-mc_stext CP  '*SENIOR CONSULTANT*'..
    p_cd = '28'.
    EXIT.
  ENDIF.

  IF i_zshrvaatzemp-mc_stext CP  '*MANAGER*'.
    p_cd = '26'.
    EXIT.
  ENDIF.


  IF i_zshrvaatzemp-mc_stext CP  '*MGR*'.
    p_cd = '26'.
    EXIT.
  ENDIF.

  p_cd = '30'.

ENDFORM.                    " get_pos_cd
