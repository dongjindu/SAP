************************************************************************
* Program Name      : ZIPP302U_BOM_BDC_03_CTN
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
REPORT ZIPP302U_BOM_BDC_03_CT NO STANDARD PAGE HEADING LINE-SIZE 194.
TABLES: ZTBM_EBOM_ECM.

DATA: BEGIN OF IT_BOM OCCURS 0,
        MTNO   LIKE   ZTBM_EBOM_ECM-MTNO,
        CNT    TYPE   I,
      END   OF IT_BOM.

DATA: BEGIN OF IT_JOB OCCURS 0,
        MTNO_F     LIKE   ZTBM_EBOM_ECM-MTNO,
        MTNO_T     LIKE   ZTBM_EBOM_ECM-MTNO,
        CNT        TYPE   I,
        JOBNAME    LIKE   BTCH1140-JOBNAME,
*        execserver LIKE   btch1140-execserver,
        JOBCNT     LIKE TBTCJOB-JOBCOUNT,
        MSG(100),
      END   OF IT_JOB.

*DATA: BEGIN OF it_server OCCURS 0,
*        name   LIKE   sapwlserv-name,
*      END   OF it_server.

*---// Selection screens
SELECTION-SCREEN BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-T01.
SELECT-OPTIONS: S_MTNO FOR ZTBM_EBOM_ECM-MTNO,
                S_COMP FOR ZTBM_EBOM_ECM-COMP.
PARAMETERS: P_COUNT TYPE I OBLIGATORY DEFAULT 1000.
SELECTION-SCREEN END   OF BLOCK BL1.

*---// Check & Read data
AT SELECTION-SCREEN.
  CHECK SY-UCOMM EQ 'ONLI'.
** Changed by Furong on 11/19/08
  PERFORM CHECK_MULTI_COLOR.
** End of change pn 11/19/08
  PERFORM GET_DATA.

START-OF-SELECTION.
  PERFORM SET_IT_JOB.
  PERFORM CREATE_BATCH_JOB.
  PERFORM DISPLAY_RTN.
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA.
  PERFORM GET_BOM_ECM.
  PERFORM GET_SERVER_LIST.
ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  SET_IT_JOB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_IT_JOB.
  DATA: LW_START_FLG VALUE 'X',
        LW_INDEX(2)  TYPE  N.

  SORT IT_BOM BY MTNO.
  LOOP AT IT_BOM.

    IF LW_START_FLG EQ 'X'.
      MOVE: IT_BOM-MTNO TO IT_JOB-MTNO_F.
    ENDIF.

    MOVE: IT_BOM-MTNO TO IT_JOB-MTNO_T.

    IT_JOB-CNT = IT_JOB-CNT + IT_BOM-CNT.

    IF IT_JOB-CNT >= P_COUNT.
      MOVE: 'X' TO LW_START_FLG.

      APPEND IT_JOB. CLEAR: IT_JOB.
    ELSE.
      CLEAR: LW_START_FLG.
    ENDIF.

    AT LAST.
      IF IT_JOB-CNT <  P_COUNT AND
         IT_JOB-CNT NE 0.
        APPEND IT_JOB.
      ENDIF.
    ENDAT.
  ENDLOOP.

  DATA: LW_LINES  TYPE I,
        LW_SERVER TYPE I.

*  DESCRIBE TABLE it_server LINES lw_lines.

  LOOP AT IT_JOB.
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

    MOVE: SY-TABIX TO LW_INDEX.
    CONCATENATE 'Z_BOM_UPLOAD_' IT_JOB-MTNO_F INTO IT_JOB-JOBNAME.

    MODIFY IT_JOB.
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
FORM GET_BOM_ECM.
*---// Read BOM Interface information.
  SELECT  MTNO COUNT( * ) AS CNT
    INTO CORRESPONDING FIELDS OF TABLE IT_BOM
    FROM ZTBM_EBOM_ECM
   WHERE MTNO IN S_MTNO
     AND COMP IN S_COMP
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
FORM GET_SERVER_LIST.
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
FORM CREATE_BATCH_JOB.
  LOOP AT IT_JOB.
    PERFORM CALL_JOB_OPEN USING    IT_JOB-JOBNAME
                          CHANGING IT_JOB-JOBCNT.

    IF IT_JOB-MSG EQ SPACE.
      SUBMIT ZIPP302U_BOM_BDC_03_01 WITH S_MTNO BETWEEN IT_JOB-MTNO_F
                                                    AND IT_JOB-MTNO_T
                                    USER    SY-UNAME
                                    VIA JOB IT_JOB-JOBNAME
                                    NUMBER  IT_JOB-JOBCNT
                                    AND RETURN.
    ENDIF.

    PERFORM CALL_JOB_CLOSE USING IT_JOB-JOBNAME IT_JOB-JOBCNT.

    MODIFY IT_JOB.
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
FORM CALL_JOB_OPEN USING    PW_JOBNAM
                   CHANGING PW_JOBCNT.

  DATA: LW_JOBCNT LIKE TBTCJOB-JOBCOUNT.

  CALL FUNCTION 'JOB_OPEN'
       EXPORTING
            JOBNAME          = PW_JOBNAM
            JOBCLASS         = 'A'
       IMPORTING
            JOBCOUNT         = LW_JOBCNT
       EXCEPTIONS
            CANT_CREATE_JOB  = 1
            INVALID_JOB_DATA = 2
            JOBNAME_MISSING  = 3
            OTHERS           = 4.
  IF SY-SUBRC <> 0.
    PERFORM GET_MESSAGE USING IT_JOB-MSG.
  ENDIF.

  MOVE: LW_JOBCNT TO PW_JOBCNT.
ENDFORM.                    " CALL_JOB_OPEN
*&---------------------------------------------------------------------*
*&      Form  get_message
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_JOB_MSG  text
*----------------------------------------------------------------------*
FORM GET_MESSAGE USING    PW_MSG.
  CALL FUNCTION 'RKC_MSG_STRING'
       EXPORTING
            ID      = SY-MSGID
            MTYPE   = SY-MSGTY
            NUMBER  = SY-MSGNO
            PAR1    = SY-MSGV1
            PAR2    = SY-MSGV2
            PAR3    = SY-MSGV3
            PAR4    = SY-MSGV4
       IMPORTING
            MSG_LIN = PW_MSG.
ENDFORM.                    " get_message
*&---------------------------------------------------------------------*
*&      Form  CALL_JOB_CLOSE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_JOB_JOBNAME  text
*      -->P_IT_JOB_JOBCNT  text
*----------------------------------------------------------------------*
FORM CALL_JOB_CLOSE USING PW_JOBNAM PW_JOBCNT.
  CHECK IT_JOB-MSG EQ SPACE.

  CALL FUNCTION 'JOB_CLOSE'
       EXPORTING
            JOBCOUNT  = PW_JOBCNT
            JOBNAME   = PW_JOBNAM
            STRTIMMED = 'X'
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
FORM DISPLAY_RTN.
  SORT IT_JOB BY JOBNAME.

  WRITE:/30 'Separated BOM Upload'.
  SKIP.

  FORMAT COLOR COL_HEADING.

  ULINE.
  WRITE: TEXT-H01 NO-GAP,
         TEXT-H02.
  ULINE.

  LOOP AT IT_JOB.
    FORMAT COLOR COL_KEY.
    WRITE:/      '|' NO-GAP,
                 IT_JOB-JOBNAME    NO-GAP, '|' NO-GAP,
                 IT_JOB-JOBCNT     NO-GAP, '|' NO-GAP.

    FORMAT COLOR COL_NORMAL.

    WRITE:  (18) IT_JOB-MTNO_F     NO-GAP, '|' NO-GAP,
            (18) IT_JOB-MTNO_T     NO-GAP, '|' NO-GAP,
                 IT_JOB-CNT        NO-GAP, '|' NO-GAP,
*                 it_job-execserver NO-GAP, '|' NO-GAP,
                 IT_JOB-MSG        NO-GAP, '|'.
  ENDLOOP.

  ULINE.
ENDFORM.                    " display_rtn
*&---------------------------------------------------------------------*
*&      Form  check_multi_color
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_MULTI_COLOR.
  DATA: BEGIN OF LT_TEMP OCCURS 0,
   MTNO LIKE ZTBM_EBOM_ECM-MTNO,
   CLPT LIKE ZTBM_EBOM_ECM-CLPT,
   STGB LIKE ZTBM_EBOM_ECM-STGB,
   PLNT LIKE ZTBM_EBOM_ECM-PLNT,
   USAG LIKE ZTBM_EBOM_ECM-USAG,
  END OF LT_TEMP.
  DATA: L_MTNO LIKE ZTBM_EBOM_ECM-MTNO.

  SELECT MTNO CLPT STGB PLNT USAG INTO TABLE LT_TEMP
    FROM ZTBM_EBOM_ECM
   WHERE PLNT = 'P001'
     AND USAG = '1'
     AND CLPT = 'C'
     AND STGB = 'C'
     AND ZRESULT = ' '.

  LOOP AT LT_TEMP.
    SELECT SINGLE VALU1 INTO L_MTNO
     FROM ZTBM_FSC_CRE_INF
     WHERE VALU1 = LT_TEMP-MTNO
       AND VALU2 = LT_TEMP-PLNT
       AND VALU3 = LT_TEMP-USAG.
    IF SY-SUBRC = 0.
      UPDATE ZTBM_EBOM_ECM SET ZRESULT = 'I'
              WHERE MTNO = LT_TEMP-MTNO
                AND PLNT = LT_TEMP-PLNT
                AND USAG = LT_TEMP-USAG
                AND CLPT = LT_TEMP-CLPT
                AND STGB = LT_TEMP-STGB.
    ENDIF.
    CLEAR L_MTNO.
  ENDLOOP.
ENDFORM.                    " check_multi_color
