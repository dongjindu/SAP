**&----------------------------------------------------------------------
*& Development ID : ZAHRU01E
*& Program ID     : ZAHRU01E
*& Program Name   : [HR] GeC-Org.Unit Interface(HMMA)
*& Created by     : IG Moon
*& Created on     : 03/16/2012
*& Reference Pgm  : N/A
*&
*& Modification Log
*& Date        Developer Issue No  Description
*&======================================================================
*
*&----------------------------------------------------------------------

REPORT  zahru01e.

TYPE-POOLS: slis.

TABLES: hrp1000,hrp1001,hrp1008,zshrgecif,p1008.

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
        INCLUDE STRUCTURE zshrgecif.
DATA: END OF it_out.

DATA: BEGIN OF it_txt OCCURS 0,
      endda   TYPE datum,
      dept_nm LIKE it_out-dept_nm,
END OF it_txt.

DATA: v_sdate TYPE sy-datum.

DATA org_info_table LIKE hrcm_orginfo OCCURS 0 WITH HEADER LINE.

DATA: gt_hrobject TYPE TABLE OF hrobject.
DATA: wa_hrobject TYPE hrobject.

DATA inh_persa LIKE p1008-persa.
DATA inh_bukrs LIKE p1008-bukrs.
DATA dummy LIKE p1008.

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-t01.
PARAMETERS: p_test AS CHECKBOX DEFAULT 'X' USER-COMMAND chck,
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

  it_out-apply_yn = 'N'.
  it_out-indate = sy-datum.
  MODIFY it_out TRANSPORTING apply_yn indate WHERE apply_yn <> 'N'.

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
      i_structure_name       = 'ZSHRGECIF'
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
                         WITH KEY fieldname = 'EMPNO'.
  IF sy-subrc = 0.
    gs_fieldcat-key = 'X'.
    MODIFY gt_fieldcat FROM gs_fieldcat INDEX sy-tabix
                       TRANSPORTING key.
  ENDIF.

  PERFORM rename USING:
      'DEPT_ID'           'Org.Unit code',
      'DEPT_NM'	    'Org.Unit name',
      'UPR_DEPT_ID'	    'Upper Org.Unit code',
      'MNGR_JOB_TITL_CD'  'Manager Position code',
      'MNGR_JOB_TITL_NM'  'Manager Position name',
      'MNGR_USN'	    'Manager ID',
      'MNGR_NM'	    'Manager Name',
      'WRKPLC_CD'	    'Workplace code',
      'WRKPLC_NM'	    'Workplace name',
      'CORP_CD'	    'Company code',
      'CORP_NM'	    'Company name',
      'DEPT_LV'	    'Org.unit level code',
      'DEPT_LV_NM'	    'Org.unit level name',
      'USE_YN'           'Use'.

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
    gs_fieldcat-seltext_m = p_fvalue.
    gs_fieldcat-seltext_s = p_fvalue.
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

  __cls : it_out.

  SELECT * FROM hrp1000
  WHERE plvar = '01'
    AND otype = 'O'
    AND langu = 'EN'
*    AND endda GE p_kdate
    AND begda LE p_kdate
    AND aedtm IN s_cdate.

    PERFORM gather_record USING hrp1000-objid.

  ENDSELECT.

  SELECT * FROM hrp1001
  WHERE plvar = '01'
    AND otype = 'O'
*    AND endda GE p_kdate
    AND begda LE p_kdate
    AND aedtm IN s_cdate.

    PERFORM gather_record USING hrp1001-objid.

  ENDSELECT.

  SELECT * FROM hrp1008
  WHERE plvar = '01'
    AND otype = 'O'
*    AND endda GE p_kdate
    AND begda LE p_kdate
    AND aedtm IN s_cdate.

    PERFORM gather_record USING hrp1008-objid.

  ENDSELECT.

  SORT it_out.
  DELETE ADJACENT DUPLICATES FROM it_out.

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

  CALL FUNCTION 'Z_HR_GEC_IF_ORG'
    DESTINATION p_rfcdes
    TABLES
      zshrgecif                      = it_out
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
*&      Form  GATHER_RECORD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_HRP1000_OBJID  text
*----------------------------------------------------------------------*
FORM gather_record  USING p_objid.

  DATA: g_sobid     LIKE hrsobid-sobid.

  IF p_objid(1) EQ '5'.
    EXIT.
  ENDIF.

  CLEAR it_out.
  it_out-dept_id = p_objid.

  SELECT SINGLE stext INTO it_out-dept_nm FROM hrp1000
  WHERE plvar = '01'
    AND otype = 'O'
    AND langu = 'EN'
    AND endda EQ '99991231'
    AND objid = it_out-dept_id.

  IF sy-subrc NE 0.
    __cls it_txt.

    SELECT endda stext INTO TABLE it_txt FROM hrp1000
    WHERE plvar = '01'
      AND otype = 'O'
      AND langu = 'EN'
      AND objid = it_out-dept_id
      AND endda NE '99991231'.

    SORT it_txt BY endda DESCENDING.
    READ TABLE it_txt INDEX 1.
    it_out-dept_nm = it_txt-dept_nm.
    it_out-use_yn = 'N'.
  ENDIF.

  __cls org_info_table.

  CALL FUNCTION 'HRCM_ORGSTRC_INFO_TABLE_GET'
    EXPORTING
      plvar          = '01'
      root_otype     = 'O'
      root_objid     = it_out-dept_id
      begda          = sy-datum
      endda          = '99991231'
      path_id        = 'O-O '
      path_depth     = '2'
    TABLES
      org_info_table = org_info_table.
  IF sy-subrc <> 0.
    it_out-upr_dept_id = space.
  ELSE.
    READ TABLE org_info_table WITH KEY tlevel = '2'.
    IF sy-subrc EQ 0.
      it_out-upr_dept_id = org_info_table-objid.
    ENDIF.
  ENDIF.

  g_sobid = it_out-dept_id.

  CLEAR gt_hrobject[].

  CALL FUNCTION 'RH_GET_LEADING_POSITION'
    EXPORTING
      plvar             = '01'
      otype             = 'O'
      sobid             = g_sobid
      date              = sy-datum
      auth              = 'X'
      buffer_mode       = 'X'
      consider_vac_pos  = 'X'
    TABLES
      leading_pos       = gt_hrobject
    EXCEPTIONS
      no_lead_pos_found = 1
      OTHERS            = 2.

  IF sy-subrc <> 0.
  ELSE.

    LOOP AT gt_hrobject INTO wa_hrobject.
      it_out-mngr_job_titl_cd = wa_hrobject-objid.
      EXIT.
    ENDLOOP.

    SELECT SINGLE stext INTO it_out-mngr_job_titl_nm FROM hrp1000
    WHERE plvar = '01'
      AND otype = 'S'
      AND endda EQ '99991231'
      AND objid = it_out-mngr_job_titl_cd.

    SELECT SINGLE sobid INTO it_out-mngr_usn FROM hrp1001
    WHERE plvar = '01'
      AND otype = 'S'
      AND endda EQ '99991231'
      AND subty = 'A008'
      AND objid = it_out-mngr_job_titl_cd.


    SELECT SINGLE ename INTO it_out-mngr_nm FROM pa0001
    WHERE pernr = it_out-mngr_usn.

    p1008-plvar = '01'.
    p1008-otype = 'O'.
    p1008-objid = it_out-dept_id.
    p1008-begda = sy-datum.
    p1008-endda = '12319999'.

    IF it_out-dept_id EQ '90001922'.
      SELECT SINGLE persa bukrs INTO (inh_persa,inh_bukrs)
            FROM hrp1008 WHERE plvar EQ '01'
                           AND otype EQ 'O'
                           AND objid EQ it_out-dept_id
                           AND endda EQ '99991231'.
    ELSE.

      CALL FUNCTION 'RH_CHECK_ACC_INPUT'
        EXPORTING
          plvar          = '01'
          otype          = 'O'
          objid          = it_out-dept_id
          begda          = sy-datum
          endda          = '99991231'
          act_1008       = dummy
*         om_buffer_mode = 'X'
        IMPORTING
          inh_pers_area  = inh_persa
          inh_bukrs      = inh_bukrs.
    ENDIF.

    IF sy-subrc EQ 0.

      it_out-wrkplc_cd = inh_persa.
      it_out-corp_cd = inh_bukrs.

      SELECT SINGLE name1 INTO it_out-wrkplc_nm FROM t500p
      WHERE persa = it_out-wrkplc_cd.

      SELECT SINGLE butxt INTO it_out-corp_nm FROM t001
      WHERE bukrs = it_out-corp_cd.

    ENDIF.

    SELECT SINGLE sobid INTO it_out-dept_lv FROM hrp1001
    WHERE plvar = '01'
      AND otype = 'O'
      AND endda EQ '99991231'
      AND subty = 'AZ03'
      AND sclas = 'OL'
      AND objid = it_out-dept_id.

    SELECT SINGLE stext INTO it_out-dept_lv_nm FROM hrp1000
    WHERE plvar = '01'
      AND otype = 'OL'
      AND langu = 'EN'
      AND endda EQ '99991231'
      AND objid = it_out-dept_lv.

  ENDIF.

  APPEND it_out.

ENDFORM.                    " GATHER_RECORD
