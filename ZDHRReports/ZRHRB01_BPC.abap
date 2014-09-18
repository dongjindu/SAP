*&---------------------------------------------------------------------*
*& Program Name      : ZRHRB01_BPC                                     *
*& Author            : Ho-Joong, Hwang                                 *
*& Creation Date     : 2004.01.13                                      *
*& Specifications By : Ho-Joong, Hwang                                 *
*& Development Request No : UD1K905879                                 *
*& Addl Documentation:                                                 *
*& Description       : HR - Basic pay Change                           *
*&                                                                     *
*&                                                                     *
*& Modification Logs                                                   *
*& Date            Developer        RequestNo      Description         *
*& 10/27/2004      Yongping Li      UD1K912672     Upload one more field
*                                                  P008-TRFST          *
* 05/14/12        t-code was deleted with recommended by APM system    *
*&---------------------------------------------------------------------*
REPORT ZRHRB01_BPC MESSAGE-ID ZMHR
                   LINE-SIZE 132
                   LINE-COUNT 65
                   NO STANDARD PAGE HEADING.
************************************************************************
*                          DATA SOURCES                                *
************************************************************************
TABLES: PA0001,
        PA0008.

FIELD-SYMBOLS:  <WT> LIKE PA0008-LGA01,
                <AM> LIKE PA0008-BET01.
************************************************************************
*                           VARIABLES                                  *
************************************************************************

*... internal tables
DATA: BEGIN OF IT_UPLOD OCCURS 0,
      PERNR    LIKE PA0001-PERNR,
      BEGDA    LIKE PA0008-BEGDA,
      BETRG    TYPE P DECIMALS 2,
      ANSAL    TYPE P DECIMALS 2,
      STVOR    LIKE PA0008-STVOR,
      PREAS    LIKE PA0008-PREAS,
*-->REQUESTED BY LATINA CARHEE, CHANGED BY YONGPING LI
      TRFST    LIKE PA0008-TRFST .
*-->END OF CHANGE ON 10/27/2004
DATA: END OF IT_UPLOD.

DATA: BEGIN OF IT_ERROR OCCURS 0,
      PERNR    LIKE PA0001-PERNR,
      MESSG(100).
DATA: END OF IT_ERROR.

DATA: S_P0008  LIKE P0008,
      RETURN   LIKE BAPIRETURN1.

DATA: INTERN TYPE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE.

DATA: BDC_TAB LIKE BDCDATA OCCURS 0 WITH HEADER LINE,
      BDC_MSG LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.

*... variants
DATA: CHANGE  LIKE PSPAR-ACTIO VALUE 'MOD'.

DATA: W_BEGDA LIKE P0008-BEGDA,
      W_ENDDA LIKE P0008-ENDDA.

DATA: W_LGART(20),
      W_BETRG(20),
      W_COUNT(2)    TYPE N,
      W_INFTY(4)    TYPE C VALUE '0008',
      W_TABIX       LIKE SY-TABIX,
      W_PERCN       TYPE I,
      W_DBCNT       LIKE SY-TFILL,
      W_PTEXT(50).
************************************************************************
*                           PARAMETERS                                 *
************************************************************************
PARAMETERS: W_FNAME LIKE RLGRAP-FILENAME,
            W_cmode LIKE ctu_params-dismode DEFAULT 'N' OBLIGATORY.
************************************************************************
*                        AT SELECTION-SCREEN                           *
************************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR W_FNAME.
  CALL FUNCTION 'F4_FILENAME'
       EXPORTING
            PROGRAM_NAME  = SY-CPROG
            DYNPRO_NUMBER = SY-DYNNR
            FIELD_NAME    = ' '
       IMPORTING
            FILE_NAME     = W_FNAME.

************************************************************************
*                         START-OF-SELECTION                           *
************************************************************************
START-OF-SELECTION.
*... 1. file upload
  PERFORM FILE_UPLOAD.
  IF IT_UPLOD[] IS INITIAL.
    MESSAGE S001 WITH 'File Upload Failed!!'.
    EXIT.
  ENDIF.
*... 2. check condition of uploded person
  PERFORM CHECK_CONDITION.
*... 3. change data --> make BDC code
* PERFORM MODIFY_BASICPAY_DATA.
  PERFORM MAKE_BDC_DATA.
*... 4. write error list
  PERFORM WRITE_LIST.
************************************************************************
*                            SUBROUTINES                               *
************************************************************************
*&---------------------------------------------------------------------*
*&      Form  FILE_UPLOAD
*&---------------------------------------------------------------------*
FORM FILE_UPLOAD.
  CLEAR IT_UPLOD. REFRESH IT_UPLOD.
  CLEAR INTERN. REFRESH INTERN.
*
  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
       EXPORTING
            FILENAME                      = W_FNAME
            I_BEGIN_COL                   = 1
            I_BEGIN_ROW                   = 2
            I_END_COL                     = 6
            I_END_ROW                     = 2000
       TABLES
            INTERN                        = INTERN
       EXCEPTIONS
            INCONSISTENT_PARAMETERS       = 1
            UPLOAD_OLE                    = 2
            OTHERS                        = 3.
*
  DESCRIBE TABLE INTERN LINES W_DBCNT.
  W_DBCNT = W_DBCNT / 5.
*
  CLEAR W_TABIX.
  LOOP AT INTERN.
    CASE INTERN-COL.
      WHEN 1. IT_UPLOD-PERNR = INTERN-VALUE.
      WHEN 2. IT_UPLOD-BEGDA = INTERN-VALUE.
      WHEN 3. IT_UPLOD-BETRG = INTERN-VALUE.
*     WHEN 4. IT_UPLOD-ANSAL = INTERN-VALUE.
      WHEN 4. IT_UPLOD-STVOR = INTERN-VALUE.
      WHEN 5. IT_UPLOD-PREAS = INTERN-VALUE.
*-->REQUESTED BY LATINA CARHEE, CHANGED BY YONGPING LI
      WHEN 6. IT_UPLOD-TRFST = INTERN-VALUE.
*-->END OF CHANGE ON 10/27/2004
        APPEND IT_UPLOD. CLEAR IT_UPLOD.
        W_TABIX = W_TABIX + 1.
        W_PERCN = ( 100 * W_TABIX ) / W_DBCNT.
        W_PTEXT = 'processing...'.
        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
             EXPORTING
                  PERCENTAGE = W_PERCN
                  TEXT       = W_PTEXT.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    " FILE_UPLOAD
*&---------------------------------------------------------------------*
*&      Form  CHECK_CONDITION
*&---------------------------------------------------------------------*
FORM CHECK_CONDITION.
  CLEAR IT_ERROR. REFRESH IT_ERROR.
*
  LOOP AT IT_UPLOD.
    CLEAR PA0001.
    SELECT SINGLE PERSK INTO PA0001-PERSK
      FROM PA0001 WHERE PERNR = IT_UPLOD-PERNR
                    AND ENDDA = '99991231'.
    IF PA0001-PERSK <> 'U0'.
      IT_ERROR-PERNR = IT_UPLOD-PERNR.
      IT_ERROR-MESSG = 'Incorrect EE subgroup; not hourly person'.
      APPEND IT_ERROR. CLEAR IT_ERROR.
      DELETE IT_UPLOD.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " CHECK_CONDITION
*&---------------------------------------------------------------------*
*&      Form  MODIFY_BASICPAY_DATA
*&---------------------------------------------------------------------*
FORM MODIFY_BASICPAY_DATA.
  LOOP AT IT_UPLOD.
    CONCATENATE IT_UPLOD-PERNR 'processing' INTO W_PTEXT
                SEPARATED BY SPACE.
    CLEAR: S_P0008, RETURN.
    SELECT SINGLE * INTO CORRESPONDING FIELDS OF S_P0008
      FROM PA0008 WHERE PERNR = IT_UPLOD-PERNR
                    AND ENDDA = '99991231'.
    S_P0008-INFTY = '0008'.
    W_BEGDA = S_P0008-BEGDA.
    W_ENDDA = S_P0008-ENDDA.
    S_P0008-BEGDA = IT_UPLOD-BEGDA.
    S_P0008-ANSAL = IT_UPLOD-ANSAL.
    S_P0008-STVOR = IT_UPLOD-STVOR.
    S_P0008-PREAS = IT_UPLOD-PREAS.


    PERFORM MODIFY_AMOUNT.

    CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
         EXPORTING NUMBER = S_P0008-PERNR.

    CALL FUNCTION 'HR_INFOTYPE_OPERATION'
         EXPORTING
              INFTY                  = '0008'
              NUMBER                 = S_P0008-PERNR
              SUBTYPE                = S_P0008-SUBTY
              OBJECTID               = S_P0008-OBJPS
              LOCKINDICATOR          = S_P0008-SPRPS
              VALIDITYEND            = W_ENDDA
              VALIDITYBEGIN          = W_BEGDA
              RECORDNUMBER           = S_P0008-SEQNR
              RECORD                 = S_P0008
              OPERATION              = CHANGE
              NOCOMMIT               = ' '
         IMPORTING
              RETURN                 = RETURN.

    CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
         EXPORTING NUMBER = S_P0008-PERNR.

    W_PERCN = ( 100 * SY-TABIX ) / W_DBCNT.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
         EXPORTING
              PERCENTAGE = W_PERCN
              TEXT       = W_PTEXT
         EXCEPTIONS
              OTHERS     = 1.

    IF RETURN-TYPE = 'E'.
      IT_ERROR-PERNR = S_P0008-PERNR.
      IT_ERROR-MESSG = RETURN-MESSAGE.
      APPEND IT_ERROR. CLEAR IT_ERROR.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " MODIFY_BASICPAY_DATA
*&---------------------------------------------------------------------*
*&      Form  MODIFY_AMOUNT
*&---------------------------------------------------------------------*
FORM MODIFY_AMOUNT.
  CLEAR W_COUNT.
*
  DO 20 TIMES.
    CLEAR: W_LGART, W_BETRG.
    W_COUNT = W_COUNT + 1.
    CONCATENATE 'S_P0008-LGA' W_COUNT INTO W_LGART.
    ASSIGN (W_LGART) TO <WT>.

    IF <WT> = '0001'.
      CONCATENATE 'S_P0008-BET' W_COUNT INTO W_BETRG.
      ASSIGN (W_BETRG) TO <AM>.
      <AM> = IT_UPLOD-BETRG.
      EXIT.
    ENDIF.
  ENDDO.
ENDFORM.                    " MODIFY_AMOUNT
*&---------------------------------------------------------------------*
*&      Form  WRITE_LIST
*&---------------------------------------------------------------------*
FORM WRITE_LIST.
  IF NOT IT_ERROR[] IS INITIAL.
    WRITE: /1(12) 'Personnel No', 15(100) 'Message'.
    ULINE.
    LOOP AT IT_ERROR.
      WRITE: /1(12) IT_ERROR-PERNR COLOR 6 INTENSIFIED OFF,
             15(100) IT_ERROR-MESSG COLOR 3 INTENSIFIED.
    ENDLOOP.
  ELSE.
    MESSAGE S001 WITH 'DATA CHANGED!!'.
  ENDIF.
ENDFORM.                    " WRITE_LIST
*&---------------------------------------------------------------------*
*&      Form  MAKE_BDC_DATA
*&---------------------------------------------------------------------*
FORM MAKE_BDC_DATA.
  DATA: C_BETRG(12).
**** INSERT BY JSLEE
 DATA : W_BDC_MODE TYPE C VALUE 'N'.

  SORT IT_UPLOD BY PERNR.
*
  LOOP AT IT_UPLOD.
    CONCATENATE IT_UPLOD-PERNR 'processing' INTO W_PTEXT
                SEPARATED BY SPACE.

    CLEAR BDC_TAB. REFRESH BDC_TAB.
    CLEAR BDC_MSG. REFRESH BDC_MSG.
    C_BETRG = IT_UPLOD-BETRG.

    PERFORM DYNPRO USING:
            'X' 'SAPMP50A'    '1000',
            ' ' 'RP50G-PERNR' IT_UPLOD-PERNR,
            ' ' 'BDC_SUBSCR'  'SAPMP50A',
            ' ' 'RP50G-CHOIC' W_INFTY,
            ' ' 'BDC_OKCODE'   '=COP'.

    PERFORM DYNPRO USING:
            'X' 'MP000800' '2010',
            ' ' 'P0008-BEGDA' IT_UPLOD-BEGDA,
            ' ' 'P0008-PREAS' IT_UPLOD-PREAS,
            ' ' 'P0008-STVOR' IT_UPLOD-STVOR,
*--->REQUESTED BY LATINA CARHEE, CHANGED BY YONGPING LI
            ' ' 'P0008-TRFST' IT_UPLOD-TRFST.
*--->END OF CHANGE ON 10/27/2004
    PERFORM GET_WAGE_TYPE_POSITION.

    PERFORM DYNPRO USING:
            ' ' W_BETRG       C_BETRG,
            ' ' 'BDC_OKCODE'  '/11'.

    CALL TRANSACTION 'PA30' USING BDC_TAB
                            MODE  W_cmode
                            MESSAGES INTO BDC_MSG.

    W_PERCN = ( 100 * SY-TABIX ) / W_DBCNT.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
         EXPORTING
              PERCENTAGE = W_PERCN
              TEXT       = W_PTEXT
         EXCEPTIONS
              OTHERS     = 1.

    READ TABLE BDC_MSG WITH KEY MSGTYP = 'E'.
    IF RETURN-TYPE = 'E'.
      IT_ERROR-PERNR = S_P0008-PERNR.
      IT_ERROR-MESSG = RETURN-MESSAGE.
      APPEND IT_ERROR. CLEAR IT_ERROR.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " MAKE_BDC_DATA
*&---------------------------------------------------------------------*
*&      Form  DYNPRO
*&---------------------------------------------------------------------*
FORM DYNPRO USING DYNBEGIN NAME VALUE.
  IF DYNBEGIN = 'X'.
    MOVE: NAME  TO BDC_TAB-PROGRAM,
          VALUE TO BDC_TAB-DYNPRO,
          'X'   TO BDC_TAB-DYNBEGIN.
  ELSE.
    MOVE: NAME  TO BDC_TAB-FNAM,
          VALUE TO BDC_TAB-FVAL.
  ENDIF.
*
  APPEND BDC_TAB. CLEAR BDC_TAB.
ENDFORM.                    " DYNPRO
*&---------------------------------------------------------------------*
*&      Form  GET_WAGE_TYPE_POSITION
*&---------------------------------------------------------------------*
FORM GET_WAGE_TYPE_POSITION.
  CLEAR PA0008.
  SELECT SINGLE * INTO CORRESPONDING FIELDS OF S_P0008
    FROM PA0008 WHERE PERNR = IT_UPLOD-PERNR
                  AND ENDDA = '99991231'.
*
  W_COUNT = 0.
  DO.
    W_COUNT = W_COUNT + 1.
    CONCATENATE 'S_P0008-LGA' W_COUNT INTO W_LGART.
    ASSIGN (W_LGART) TO <WT>.

    IF <WT> = '0001'.
      CONCATENATE 'Q0008-BETRG(' W_COUNT ')' INTO W_BETRG.
      EXIT.
    ENDIF.
  ENDDO.
ENDFORM.                    " GET_WAGE_TYPE_POSITION
