************************************************************************
* Program Name      : ZIPP302U_BOM_BDC_03_CT
* Author            : Byung Sung Bae
* Creation Date     : 2005.03.30.
* Specifications By : Byung Sung Bae
* Pattern           : 2.1
* Development Request No : UD1K915268
* Addl Documentation:
* Description       : BOM Structure Main Control
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
************************************************************************
REPORT zipp302u_bom_bdc_03_ct NO STANDARD PAGE HEADING LINE-SIZE 194.
TABLES: ztbm_bom_ecm.

DATA: BEGIN OF it_bom OCCURS 0,
        mtno   LIKE   ztbm_bom_ecm-mtno,
        cnt    TYPE   i,
      END   OF it_bom.

DATA: BEGIN OF it_job OCCURS 0,
        mtno_f     LIKE   ztbm_bom_ecm-mtno,
        mtno_t     LIKE   ztbm_bom_ecm-mtno,
        cnt        TYPE   i,
        jobname    LIKE   btch1140-jobname,
*        execserver LIKE   btch1140-execserver,
        jobcnt     LIKE tbtcjob-jobcount,
        msg(100),
      END   OF it_job.

*DATA: BEGIN OF it_server OCCURS 0,
*        name   LIKE   sapwlserv-name,
*      END   OF it_server.

*---// Selection screens
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-t01.
SELECT-OPTIONS: s_mtno FOR ztbm_bom_ecm-mtno,
                s_comp FOR ztbm_bom_ecm-comp.
PARAMETERS: p_count TYPE i OBLIGATORY DEFAULT 1000.
SELECTION-SCREEN END   OF BLOCK bl1.

*---// Check & Read data
AT SELECTION-SCREEN.
  CHECK sy-ucomm EQ 'ONLI'.
  PERFORM get_data.

START-OF-SELECTION.
  PERFORM set_it_job.
  PERFORM create_batch_job.
  PERFORM display_rtn.
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  PERFORM get_bom_ecm.
  PERFORM get_server_list.
ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  SET_IT_JOB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_it_job.
  DATA: lw_start_flg VALUE 'X',
        lw_index(2)  TYPE  n.

  SORT it_bom BY mtno.
  LOOP AT it_bom.

    IF lw_start_flg EQ 'X'.
      MOVE: it_bom-mtno TO it_job-mtno_f.
    ENDIF.

    MOVE: it_bom-mtno TO it_job-mtno_t.

    it_job-cnt = it_job-cnt + it_bom-cnt.

    IF it_job-cnt >= p_count.
      MOVE: 'X' TO lw_start_flg.

      APPEND it_job. CLEAR: it_job.
    ELSE.
      CLEAR: lw_start_flg.
    ENDIF.

    AT LAST.
      IF it_job-cnt <  p_count AND
         it_job-cnt NE 0.
        APPEND it_job.
      ENDIF.
    ENDAT.
  ENDLOOP.

  DATA: lw_lines  TYPE i,
        lw_server TYPE i.

*  DESCRIBE TABLE it_server LINES lw_lines.

  LOOP AT it_job.
*    lw_server = lw_server + 1.
*
*    READ TABLE it_server INDEX lw_server.
*    IF sy-subrc EQ 0.
*      MOVE: it_server-name TO it_job-execserver.
*    ELSE.
*      MOVE: 1 TO lw_server.
*
*      READ TABLE it_server INDEX lw_server.
*      IF sy-subrc NE 0.
*        MESSAGE e000(zz) WITH text-m01.
*      ENDIF.
*
*      MOVE: it_server-name TO it_job-execserver.
*    ENDIF.

    MOVE: sy-tabix TO lw_index.
    CONCATENATE 'Z_BOM_UPLOAD_' it_job-mtno_f INTO it_job-jobname.

    MODIFY it_job.
  ENDLOOP.
ENDFORM.                    " SET_IT_JOB
*&---------------------------------------------------------------------*
*&      Form  get_bom_ecm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_bom_ecm.
*---// Read BOM Interface information.
  SELECT  mtno COUNT( * ) AS cnt
    INTO CORRESPONDING FIELDS OF TABLE it_bom
    FROM ztbm_bom_ecm
   WHERE mtno IN s_mtno
     AND comp IN s_comp
   GROUP BY MTNO.
*     AND zresult IN (space,'E','L')
ENDFORM.                    " get_bom_ecm
*&---------------------------------------------------------------------*
*&      Form  get_server_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_server_list.
*  SELECT name INTO CORRESPONDING FIELDS OF TABLE it_server
*    FROM sapwlserv
*   WHERE NOT name LIKE 'r3ci%'.
ENDFORM.                    " get_server_list
*&---------------------------------------------------------------------*
*&      Form  create_batch_job
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_batch_job.
  LOOP AT it_job.
    PERFORM call_job_open USING    it_job-jobname
                          CHANGING it_job-jobcnt.

    IF it_job-msg EQ space.
      SUBMIT zipp302u_bom_bdc_03_bk WITH s_mtno BETWEEN it_job-mtno_f
                                                    AND it_job-mtno_t
                                    USER    sy-uname
                                    VIA JOB it_job-jobname
                                    NUMBER  it_job-jobcnt
                                    AND RETURN.
    ENDIF.

    PERFORM call_job_close USING it_job-jobname it_job-jobcnt.

    MODIFY it_job.
  ENDLOOP.
ENDFORM.                    " create_batch_job
*&---------------------------------------------------------------------*
*&      Form  CALL_JOB_OPEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_JOBNAM  text
*      <--P_L_INT  text
*----------------------------------------------------------------------*
FORM call_job_open USING    pw_jobnam
                   CHANGING pw_jobcnt.

  DATA: lw_jobcnt LIKE tbtcjob-jobcount.

  CALL FUNCTION 'JOB_OPEN'
       EXPORTING
            jobname          = pw_jobnam
            jobclass         = 'A'
       IMPORTING
            jobcount         = lw_jobcnt
       EXCEPTIONS
            cant_create_job  = 1
            invalid_job_data = 2
            jobname_missing  = 3
            OTHERS           = 4.
  IF sy-subrc <> 0.
    PERFORM get_message USING it_job-msg.
  ENDIF.

  MOVE: lw_jobcnt TO pw_jobcnt.
ENDFORM.                    " CALL_JOB_OPEN
*&---------------------------------------------------------------------*
*&      Form  get_message
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_JOB_MSG  text
*----------------------------------------------------------------------*
FORM get_message USING    pw_msg.
  CALL FUNCTION 'RKC_MSG_STRING'
       EXPORTING
            id      = sy-msgid
            mtype   = sy-msgty
            number  = sy-msgno
            par1    = sy-msgv1
            par2    = sy-msgv2
            par3    = sy-msgv3
            par4    = sy-msgv4
       IMPORTING
            msg_lin = pw_msg.
ENDFORM.                    " get_message
*&---------------------------------------------------------------------*
*&      Form  CALL_JOB_CLOSE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_JOB_JOBNAME  text
*      -->P_IT_JOB_JOBCNT  text
*----------------------------------------------------------------------*
FORM call_job_close USING pw_jobnam pw_jobcnt.
  CHECK it_job-msg EQ space.

  CALL FUNCTION 'JOB_CLOSE'
       EXPORTING
            jobcount  = pw_jobcnt
            jobname   = pw_jobnam
            strtimmed = 'X'
       EXCEPTIONS
            OTHERS    = 4.
ENDFORM.                    " CALL_JOB_CLOSE
*&---------------------------------------------------------------------*
*&      Form  display_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_rtn.
  SORT it_job BY jobname.

  WRITE:/30 'Separated BOM Upload'.
  SKIP.

  FORMAT COLOR COL_HEADING.

  ULINE.
  WRITE: text-h01 NO-GAP,
         text-h02.
  ULINE.

  LOOP AT it_job.
    FORMAT COLOR COL_KEY.
    WRITE:/      '|' NO-GAP,
                 it_job-jobname    NO-GAP, '|' NO-GAP,
                 it_job-jobcnt     NO-GAP, '|' NO-GAP.

    FORMAT COLOR COL_NORMAL.

    WRITE:  (18) it_job-mtno_f     NO-GAP, '|' NO-GAP,
            (18) it_job-mtno_t     NO-GAP, '|' NO-GAP,
                 it_job-cnt        NO-GAP, '|' NO-GAP,
*                 it_job-execserver NO-GAP, '|' NO-GAP,
                 it_job-msg        NO-GAP, '|'.
  ENDLOOP.

  ULINE.
ENDFORM.                    " display_rtn
