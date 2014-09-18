*&----------------------------------------------------------------------
*& Development ID : ZAHRU01F
*& Program ID     : ZAHRU01F
*& Program Name   : [HR] GeC-Employee Interface(HMMA)
*& Created by     : IG Moon
*& Created on     : 03/19/2012
*& Reference Pgm  : N/A
*&
*& Modification Log
*& Date        Developer Issue No  Description
*&======================================================================
*
*&----------------------------------------------------------------------

REPORT  zahru01f.

TYPE-POOLS: slis.

TABLES: pa0000,pa0001,zshrgecig.

TABLES: pa0006.

*- File Catalog
DATA : gt_fieldcat TYPE slis_t_fieldcat_alv,
       gs_fieldcat TYPE slis_fieldcat_alv.

*- Macro
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

*- Output Structure
DATA: BEGIN OF it_out OCCURS 0.
        INCLUDE STRUCTURE zshrgecig.
DATA: END OF it_out.

DATA: BEGIN OF it_data OCCURS 0.
        INCLUDE STRUCTURE zshrgecig.
DATA: END OF it_data.

DATA: BEGIN OF it_change OCCURS 0,
        aedtm TYPE pa0000-aedtm,
      END OF it_change.

DATA: BEGIN OF it_pernr OCCURS 0,
        pernr TYPE pa0000-pernr,
      END OF it_pernr.

DATA v_sdate TYPE sy-datum.
DATA i_t001 LIKE t001 OCCURS 0 WITH HEADER LINE.
DATA i_t500p LIKE t500p OCCURS 0 WITH HEADER LINE.

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-t01.
SELECT-OPTIONS: s_pernr FOR pa0000-pernr.
PARAMETERS: p_test AS CHECKBOX DEFAULT 'X' USER-COMMAND chck,
            p_actv AS CHECKBOX,
            p_kdate TYPE datum DEFAULT sy-datum OBLIGATORY.

SELECT-OPTIONS: s_cdate FOR sy-datum
        DEFAULT v_sdate TO sy-datum OPTION BT SIGN I.

SELECTION-SCREEN ULINE MODIF ID chk.
PARAMETERS: p_rfcdes TYPE rfcdest DEFAULT 'WMHR01'
                     MATCHCODE OBJECT vers_rfcdest_sh
                     MODIF ID chk.
SELECTION-SCREEN END OF BLOCK blk1.

INITIALIZATION.
  v_sdate = sy-datum - 6.
  READ TABLE s_cdate INDEX 1.
  IF sy-subrc EQ 0.
    s_cdate-low = v_sdate.
    MODIFY s_cdate INDEX sy-tabix.
  ENDIF.

AT SELECTION-SCREEN OUTPUT.
*  LOOP AT SCREEN.
*    IF screen-group1 = 'CHK'.
*      IF NOT p_test IS INITIAL.
*        screen-active = 0.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.

AT SELECTION-SCREEN.
  CHECK sy-ucomm NE 'CHCK'.
  IF p_test IS INITIAL AND
     p_rfcdes IS INITIAL.
    MESSAGE e208(00) WITH 'Please Enter RFC Destination'.
  ENDIF.

START-OF-SELECTION.
  PERFORM select_data.
  CHECK NOT it_out[] IS INITIAL.

  IF NOT p_actv IS INITIAL.
    DELETE it_out WHERE work_yn = '0'.
  ENDIF.
  SORT it_out BY usn.

  it_out-apply_yn = 'N'.
  it_out-indate = sy-datum.
  modify it_out transporting apply_yn indate where apply_yn <> 'N'.

  IF p_test IS INITIAL.
    PERFORM transfer_data.
  ELSE.
    PERFORM display_data.
  ENDIF.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  ALV_GRID_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_grid_display TABLES ft_outtab.

  DATA : gs_layout TYPE slis_layout_alv,
         l_repid TYPE sy-repid.

  l_repid = sy-repid.
  gs_layout-colwidth_optimize = 'X'.
  gs_layout-zebra = 'X'.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = l_repid
      i_structure_name       = 'ZSHRGECIG'
    CHANGING
      ct_fieldcat            = gt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Set key field
  READ TABLE gt_fieldcat INTO gs_fieldcat
                         WITH KEY fieldname = 'USN'.
  IF sy-subrc = 0.
    gs_fieldcat-key = 'X'.
    MODIFY gt_fieldcat FROM gs_fieldcat INDEX sy-tabix
                       TRANSPORTING key.
  ENDIF.

  PERFORM rename USING:
        'USN'                    'Personnel number',
        'USR_NM'		 'Full Name',
        'DPRTMT_CD'		 'Department Code',
        'DPRTMT_NM'		 'Department Name',
        'JOB_TITL_CD'		 'Duty(Responsible) Code',
        'JOB_TITL_NM'		 'Duty(Responsible) Name',
        'EMAIL'			 'E-Mail Address',
        'CMPNY_CD'		 'Company Code',
        'CMPNY_NM'		 'Company Name',
        'HOME_PHONE_NUM'	 'Work phone',
        'FAX_NUM'		 'Fax',
        'MP_NUM'		 'Mobile Number',
        'BP_NUM'		 'Pager',
        'WRKPLC_CD'		 'Work Area Code',
        'WRKPLC_NM'		 'Work Area Name',
        'TITLE_CD'		 'Position Code',
        'TITLE_NM'		 'Position Name',
        'TITLE_LEV'		 'Position Level',
        'DPCO_YN'		 'Inpat',
        'REBUSI_NM'		 'Duty(Responsible) Name',
        'NAME_EN'		 'Full Name (Eng)',
        'DTY_CD'		 'Duty(Responsible) Code',
        'DAYNNIGHT_ASRTMNT'	 'Shift',
        'PSTN_CD'		 'Job level',
        'JOIN_CMPNY_DATE'	 'Hire date',
        'EMPLOYEE_GROUP'	 'EE group',
        'EMPLOYEE_SUB_GROUP'	 'EE subgroup',
        'WORK_YN'		 'Employment Status',
        'BOTM_SCTR_NM'		 'Team Name',
        'BOTM_SCTR_CD'		 'Team Code',
        'APPLY_YN'         'Apply Y/N',
        'INDATE' 'In Date'.


  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = l_repid
      is_layout          = gs_layout
      it_fieldcat        = gt_fieldcat
    TABLES
      t_outtab           = ft_outtab
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " ALV_GRID_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  RENAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FNAME  text
*      -->P_FVALUE  text
*----------------------------------------------------------------------*
FORM rename USING p_fname  TYPE c
                  p_fvalue TYPE c.

  READ TABLE gt_fieldcat INTO gs_fieldcat
                         WITH KEY fieldname = p_fname.
  IF sy-subrc = 0.
    gs_fieldcat-seltext_l = p_fvalue.
    MODIFY gt_fieldcat FROM gs_fieldcat INDEX sy-tabix
               TRANSPORTING seltext_l.
  ENDIF.

ENDFORM.                    " RENAME
*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_data .

  __cls : it_out, i_t001, i_t500p.

  SELECT * INTO TABLE i_t001 FROM t001.
  SORT i_t001 BY bukrs.

  SELECT * INTO TABLE i_t500p FROM t500p.
  SORT i_t500p BY persa.


  PERFORM get_pernr.

  IF NOT it_pernr[] IS INITIAL.
    LOOP AT it_pernr.
      SELECT * FROM pa0000
      WHERE pernr = it_pernr-pernr
        AND endda = '99991231'.

        CLEAR  it_out.
        it_out-work_yn = pa0000-stat2.
        PERFORM gather_data USING pa0000-pernr.

        APPEND it_out.

      ENDSELECT.
    ENDLOOP.
  ELSE.
    MESSAGE s000(00) WITH 'No record has been found'.
    STOP.
  ENDIF.
ENDFORM.                    " SELECT_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data .
  PERFORM alv_grid_display  TABLES it_out.
ENDFORM.                    " DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Form  TRANSFER_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM transfer_data.

  CALL FUNCTION 'Z_HR_GEC_IF_EMPLOYEE'
    DESTINATION p_rfcdes
    TABLES
      zshrgecig                      = it_out
    EXCEPTIONS
      call_function_destination_no_t = 1
      call_function_no_dest          = 2
      call_function_remote_error     = 3
      rfc_no_authority               = 4
      OTHERS                         = 5.
  IF sy-subrc <> 0.
    MESSAGE e368(00) WITH 'Error when calling RFC destination:'
                           p_rfcdes.
  ELSE.
    MESSAGE s368(00) WITH 'Data has been sent to:' p_rfcdes.
  ENDIF.

ENDFORM.                    " TRANSFER_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_DEPT_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM get_dept_info USING p_orgeh
                   CHANGING p_dprtmt_cd p_dprtmt_nm.

  DATA: it_objec TYPE objec OCCURS 0 WITH HEADER LINE,
        it_struc TYPE struc OCCURS 0 WITH HEADER LINE,
        l_level  TYPE struc-level.

  DATA: BEGIN OF it_dept OCCURS 0,
          pernr TYPE pa0001-pernr,
          endda TYPE pa0001-endda,
          begda TYPE pa0001-begda,
          orgeh TYPE pa0001-orgeh,
        END OF it_dept.

  CALL FUNCTION 'RH_STRUC_GET'
    EXPORTING
      act_otype      = 'O'
      act_objid      = p_orgeh
      act_wegid      = 'O-O'
      act_begda      = sy-datum
      act_endda      = sy-datum
    TABLES
      result_objec   = it_objec
      result_struc   = it_struc
    EXCEPTIONS
      no_plvar_found = 1
      no_entry_found = 2
      OTHERS         = 3.
  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  LOOP AT it_struc. " WHERE vpriox NE space.
    IF it_struc-vpriox <= '25'.
      p_dprtmt_cd = it_struc-objid.
      READ TABLE it_objec WITH KEY objid = p_dprtmt_cd.
      IF sy-subrc EQ 0.
        p_dprtmt_nm = it_objec-stext.
      ENDIF.
      EXIT.
    ENDIF.
  ENDLOOP.

*  IF p_dprtmt_cd IS INITIAL.
*    p_dprtmt_cd = p_orgeh.
*    READ TABLE it_objec WITH KEY objid = p_orgeh.
*    IF sy-subrc EQ 0.
*      p_dprtmt_nm = it_objec-stext.
*    ENDIF.
*  ENDIF.

ENDFORM.                    " GET_DEPT_INFO

*&---------------------------------------------------------------------*
*&      Form  SHOW_PROGRESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_&1  text
*      -->P_&2  text
*----------------------------------------------------------------------*
FORM show_progress USING    pf_text
                            pf_val.

  STATICS: l_text(50) TYPE c,
           l_baseval  TYPE i,
           l_percent  TYPE i,
           l_counter  TYPE i.

  IF l_text NE pf_text.
    l_text = pf_text.
    CLEAR: l_baseval,
           l_percent,
           l_counter.
  ENDIF.

  IF NOT l_baseval IS INITIAL.
    l_counter = l_counter - 1.
    CHECK l_counter LE 0.
    l_percent = l_percent + 10.
    CHECK l_percent LE 100.
    l_counter = l_baseval.
  ELSE.
    l_baseval = pf_val DIV 10.
    l_counter = l_baseval.
  ENDIF.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = l_percent
      text       = pf_text.

ENDFORM.                    " SHOW_PROGRESS
*&---------------------------------------------------------------------*
*&      Form  GATHER_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PA0000_PERNR  text
*----------------------------------------------------------------------*
FORM gather_data  USING p_pernr.

  DATA: l_com TYPE pa0006-com01,
        l_num TYPE pa0006-num01.

  DATA $yyyymmdd TYPE datum.

  DATA $ename TYPE emnam.

  it_out-usn = p_pernr.

  SELECT SINGLE * FROM pa0001
  WHERE pernr EQ p_pernr
    AND endda GE p_kdate
    AND begda LE p_kdate.

  IF sy-subrc EQ 0.
    $ename = pa0001-ename.
    REPLACE : 'Mrs' WITH '' INTO $ename,
              'Ms' WITH '' INTO $ename,
              'Miss' WITH '' INTO $ename,
              'Mr' WITH '' INTO $ename.
    SHIFT $ename LEFT DELETING LEADING space.
    it_out-usr_nm = $ename.

    SELECT SINGLE stext INTO it_out-botm_sctr_nm FROM hrp1000 WHERE objid = pa0001-orgeh
                                       AND otype = 'O'
                                       AND endda = '99991231'.

    it_out-botm_sctr_cd = pa0001-orgeh.

  ENDIF.

  PERFORM get_dept_info USING pa0001-orgeh
                 CHANGING it_out-dprtmt_cd it_out-dprtmt_nm.

  it_out-job_titl_cd = pa0001-stell.

  SELECT SINGLE stext INTO it_out-job_titl_nm FROM hrp1000
  WHERE plvar = '01'
    AND otype = 'C'
    AND endda EQ '99991231'
    AND objid = pa0001-stell.

  SELECT SINGLE usrid_long INTO it_out-email FROM pa0105
  WHERE pernr = p_pernr
    AND usrty = '0010'
    AND endda EQ '99991231'.

  IF it_out-email IS INITIAL.
    SELECT SINGLE usrid_long INTO it_out-email FROM pa0105
    WHERE pernr = p_pernr
      AND usrty = '0030'
      AND endda EQ '99991231'.
  ENDIF.

  it_out-cmpny_cd = pa0001-bukrs.
  READ TABLE i_t001 WITH KEY bukrs = pa0001-bukrs BINARY SEARCH.
  IF sy-subrc EQ 0.
    it_out-cmpny_nm = i_t001-butxt.
  ENDIF.

* Get Telephone Number
  SELECT *
    FROM pa0006
   UP TO 1 ROWS
   WHERE pernr = p_pernr
     AND subty = '1'
     AND endda GE p_kdate
     AND begda LE p_kdate.
  ENDSELECT.

  CLEAR: l_com, l_num.
  DO 6 TIMES VARYING l_com FROM pa0006-com01
                           NEXT pa0006-com02
             VARYING l_num FROM pa0006-num01
                           NEXT pa0006-num02.
    IF l_com = 'WORK'.
      it_out-home_phone_num = l_num.
      EXIT.
    ENDIF.
  ENDDO.

  it_out-wrkplc_cd = pa0001-werks.
  READ TABLE i_t500p WITH KEY persa = pa0001-werks BINARY SEARCH.
  IF sy-subrc EQ 0.
    it_out-wrkplc_nm = i_t500p-name1.
  ENDIF.

  it_out-title_cd = pa0001-plans.

  SELECT SINGLE stext INTO it_out-title_nm FROM hrp1000
  WHERE objid = pa0001-plans
    AND plvar = '01'
    AND otype = 'S'
    AND endda EQ '99991231'.

  IF pa0001-persg = '9' AND pa0001-persk = 'U2'.
    it_out-dpco_yn = true.
  ELSE.
  ENDIF.

  SELECT SINGLE stext INTO it_out-rebusi_nm FROM hrp1000
  WHERE objid = pa0001-stell
    AND plvar = '01'
    AND otype = 'C'
    AND endda EQ '99991231'.

  it_out-name_en = pa0001-ename.
  it_out-dty_cd = pa0001-stell.

  SELECT SINGLE b~anzsh INTO it_out-daynnight_asrtmnt
  FROM pa0007 AS a INNER JOIN ztco_mh_ws AS b
  ON b~schkz = a~schkz
  WHERE a~pernr = p_pernr
   AND a~endda = '99991231'.

  SELECT SINGLE begda INTO $yyyymmdd FROM pa0000
      WHERE pernr = p_pernr
        AND ( massn = 'Z0'  OR massn = 'Z6'  OR massn = 'Z9' ).

  WRITE  $yyyymmdd TO it_out-join_cmpny_date USING NO EDIT MASK YYMMDD.
  CONCATENATE $yyyymmdd(2) it_out-join_cmpny_date INTO it_out-join_cmpny_date.

  SELECT SINGLE ptext INTO it_out-employee_group
  FROM t501t WHERE persg = pa0001-persg
   AND sprsl = sy-langu.

  SELECT SINGLE ptext INTO it_out-employee_sub_group
  FROM t503t WHERE persk = pa0001-persk
   AND sprsl = sy-langu.

ENDFORM.                    " GATHER_DATE
*&---------------------------------------------------------------------*
*&      Form  GET_PERNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_pernr .

  __cls it_pernr.

  SELECT pernr FROM pa0000
  INTO TABLE it_pernr
  WHERE endda GE p_kdate
    AND begda LE p_kdate
    AND aedtm IN s_cdate
    AND pernr IN s_pernr.

  SELECT pernr FROM pa0001
  APPENDING TABLE it_pernr
  WHERE endda GE p_kdate
    AND begda LE p_kdate
    AND aedtm IN s_cdate
    AND pernr IN s_pernr.

*  SELECT pernr FROM pa0002
*  APPENDING TABLE it_pernr
*  WHERE endda GE p_kdate
*    AND begda LE p_kdate
*    AND aedtm IN s_cdate
*    AND pernr IN s_pernr.

  SELECT pernr FROM pa0006
  APPENDING TABLE it_pernr
  WHERE endda GE p_kdate
    AND begda LE p_kdate
    AND aedtm IN s_cdate
    AND pernr IN s_pernr.

  SELECT pernr FROM pa0105
  APPENDING TABLE it_pernr
  WHERE endda GE p_kdate
    AND begda LE p_kdate
    AND aedtm IN s_cdate
    AND pernr IN s_pernr.

  SELECT pernr FROM pa0007
  APPENDING TABLE it_pernr
  WHERE endda GE p_kdate
    AND begda LE p_kdate
    AND aedtm IN s_cdate
    AND pernr IN s_pernr.

  SORT it_pernr.
  DELETE ADJACENT DUPLICATES FROM it_pernr.

ENDFORM.                    " GET_PERNR
