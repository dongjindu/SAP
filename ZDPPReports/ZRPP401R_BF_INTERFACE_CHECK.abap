REPORT ZRPP401R_BF_INTERFACE_CHECK  MESSAGE-ID ZMPP
        NO STANDARD PAGE HEADING.

************************************************************************
* Program Name      : ZRPP401R_BF_INTERFACE_CHECK
* Author            : Yongping
* Creation Date     : 2004.09.14.
* Specifications By :
* Pattern           :
* Development Request No : UD1K912210
* Addl Documentation:
* Description       : Check for table entry ZTPPVR and ZTPP_BFST
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
***********************************************************************
*GLOBAL DATA
***********************************************************************

TABLES: ZTPP_BFST, ZTPPVR.

DATA: I_LINES TYPE I.
DATA: S_STATUS. " 'X': NO DATA.
***********************************************************************
*INTERNAL TABLES
***********************************************************************
DATA: BEGIN OF IT_RESULT OCCURS 0,
        MODEL LIKE ZTPPVR-P_MODEL,
        BODY_SER LIKE ZTPPVR-P_BODY_SERIAL,
        RPID LIKE ZTPPVR-P_STATUS,
        DATE  LIKE ZTPPVR-K04PDAT,
        FLAG LIKE ZTPPVR-FLAG,
      END OF IT_RESULT.

DATA: IT_REC_TPPVR LIKE IT_RESULT OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF IT_REC_BFST OCCURS 0,
        MODEL LIKE ZTPPVR-P_MODEL,
        BODY_SER LIKE ZTPPVR-P_BODY_SERIAL,
      END OF IT_REC_BFST.


*****************************************************************
*END OF  DATA DECLARATION
*****************************************************************


*****************************************************************
*SELECTION-SCREEN
*****************************************************************

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_case1         RADIOBUTTON  GROUP rg   DEFAULT 'X'  .
SELECTION-SCREEN COMMENT  (40) text-002 FOR FIELD p_case1  .
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  10(10) text-003 FOR FIELD p_model.
PARAMETERS: P_MODEL TYPE ZTPPVR-P_MODEL.
SELECTION-SCREEN COMMENT  30(18) text-004 FOR FIELD p_serial .
PARAMETERS: P_SERIAL TYPE ZTPPVR-P_BODY_SERIAL.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_case2         RADIOBUTTON  GROUP rg   .
SELECTION-SCREEN COMMENT  (40) text-005 FOR FIELD p_case2 .
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  10(10) text-006 FOR FIELD p_date.
select-options: p_date  for sy-datum default sy-datum.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP.

SELECTION-SCREEN END   OF BLOCK b1.


START-OF-SELECTION.

   PERFORM SELECT_RECORD.
   IF S_STATUS IS INITIAL.
    PERFORM COMPARE_RECORD.
   ENDIF.


END-OF-SELECTION.

   PERFORM WRITE_RESULT.



*****************************************************
** FORMS
*****************************************************
*&---------------------------------------------------------------------*
*&      Form  SELECT_RECORD
*&---------------------------------------------------------------------*
*       RETRIEVE THE RECORDS FROM TABLE ZTPPVR
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
FORM SELECT_RECORD.
   CASE 'X'.
     WHEN P_CASE1.
      IF P_SERIAL IS INITIAL.
       SELECT P_MODEL P_BODY_SERIAL P_STATUS K04PDAT FLAG
         INTO TABLE IT_REC_TPPVR
         FROM ZTPPVR
         WHERE P_STATUS = 'B01' AND
               P_MODEL  = P_MODEL AND
*               P_BODY_SERIAL = P_SERIAL AND
               ZRESULT <> 'E'.
        IF SY-SUBRC <> 0.
          S_STATUS = 'X'.
        ENDIF.
      ELSE.
       SELECT P_MODEL P_BODY_SERIAL P_STATUS K04PDAT FLAG
         INTO TABLE IT_REC_TPPVR
         FROM ZTPPVR
         WHERE P_STATUS = 'B01' AND
               P_MODEL  = P_MODEL AND
               P_BODY_SERIAL = P_SERIAL AND
               ZRESULT <> 'E'.
        IF SY-SUBRC <> 0.
          S_STATUS = 'X'.
        ENDIF.
      ENDIF.

     WHEN P_CASE2.

       SELECT P_MODEL P_BODY_SERIAL P_STATUS K04PDAT FLAG
         INTO TABLE IT_REC_TPPVR
         FROM ZTPPVR
         WHERE P_STATUS = 'B01' AND
               K04PDAT IN P_DATE AND
               ZRESULT <> 'E'.
       IF SY-SUBRC <> 0.
        S_STATUS = 'X'.
       ENDIF.

   ENDCASE.
ENDFORM.                    " SELECT_RECORD
*&---------------------------------------------------------------------*
*&      Form  COMPARE_RECORD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM COMPARE_RECORD.
    DATA: L_TABIX LIKE SY-TABIX.
*     DELETE THE DUPLICATE ENTRIS IN IT_REC_TPPVR
      SORT IT_REC_TPPVR BY RPID.
*      DELETE ADJACENT DUPLICATES FROM IT_REC_TPPVR
*        COMPARING RPID.

*     SELECT ALL ENTRIES IN B/F STATUS TABLE
      SELECT MODEL BODY_SER
        INTO CORRESPONDING FIELDS OF TABLE IT_REC_BFST
        FROM ZTPP_BFST
        FOR ALL ENTRIES IN IT_REC_TPPVR
        WHERE MODEL = IT_REC_TPPVR-MODEL AND
              BODY_SER = IT_REC_TPPVR-BODY_SER.
      IF SY-SUBRC <> 0.
        S_STATUS = 'N'. "NO DATA IN B/F TABLE
*     MOVE ALL DATA INTO IT_RESULT.
        IT_RESULT[] = IT_REC_TPPVR[].
        EXIT.
      ENDIF.

*     COMPARE ENTRIES IN TWO TABLES
      LOOP AT IT_REC_TPPVR.
*         L_TABIX = SY-TABIX.
         READ TABLE IT_REC_BFST
                WITH KEY MODEL = IT_REC_TPPVR-MODEL
                     BODY_SER = IT_REC_TPPVR-BODY_SER.
         IF SY-SUBRC <> 0.
*         STORE THE RESULT TO IT_RESULT
          APPEND IT_REC_TPPVR TO IT_RESULT.
         ENDIF.

      ENDLOOP.

ENDFORM.                    " COMPARE_RECORD
*&---------------------------------------------------------------------*
*&      Form  WRITE_RESULT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM WRITE_RESULT.
   DATA: I_COUNT TYPE I.
   DATA: I_MOD TYPE I.

   IF S_STATUS = 'X'.
     WRITE: / 'NO DATA FOUND'.
     EXIT.
   ENDIF.
   SORT IT_RESULT BY MODEL BODY_SER.
   DESCRIBE TABLE IT_RESULT LINES I_LINES.
   WRITE : / 'THE FOLLOWING ENTRIES IN ZTPPVR HAVE NO',
             'CORRESPONDING ENTRIS IN B/F STATUS TABLE:_LINES',
             I_LINES.

   WRITE :/ SY-ULINE.

   IF I_LINES <> 0.

*   WRITE THE FIELD HEADER
     WRITE: /2(8) TEXT-007, 11(12) TEXT-008,
              24(6) TEXT-009, 31(15) TEXT-010,
              47(5) TEXT-011.
     WRITE :/ SY-ULINE.
     LOOP AT IT_RESULT.
      I_COUNT = I_COUNT + 1.
      WRITE : /2(8) IT_RESULT-MODEL,11(12) IT_RESULT-BODY_SER,
               24(6) IT_RESULT-RPID, 31(15) IT_RESULT-DATE,
               47(5) IT_RESULT-FLAG.
      I_MOD = I_COUNT MOD 5.
      IF I_MOD EQ 0.
        SKIP.
      ENDIF.
     ENDLOOP.
   ELSE.
     WRITE:/ 'NO ERROR FOUND'.
   ENDIF.
ENDFORM.                    " WRITE_RESULT
