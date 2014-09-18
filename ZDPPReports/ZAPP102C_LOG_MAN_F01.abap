*----------------------------------------------------------------------*
*   INCLUDE ZASD04R_STOCK_OVERVIEW_F01                                 *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DATA.

  SELECT *
  INTO CORRESPONDING FIELDS OF TABLE LTAB
  FROM ZTPP_PP_LOG_HEAD
  WHERE LOGKEY    IN S_LOGKEY
  AND   PROGRAMM  IN S_PROG
  AND   LOGTYPE   IN S_LTYPE
  AND   JOBTYPE   IN S_JTYPE
  AND   LOGSTEP   IN S_LSTEP
  AND   MSG       IN S_MSG
  AND   LDATE     IN S_LDATE
  AND   LTIME     IN S_LTIME
  AND   LUSER     IN S_LUSER.

ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MODIFY_DATA.

  CLEAR: LTAB.
  LOOP AT LTAB .

   MOVE-CORRESPONDING LTAB TO T_ITAB1.
   APPEND T_ITAB1 TO ITAB1.

   CLEAR: T_ITAB1, LTAB.
  ENDLOOP.

ENDFORM.                    " MODIFY_DATA
*&---------------------------------------------------------------------*
*&      Form  CALL_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_SCREEN.

  SORT ITAB1.
  DESCRIBE TABLE ITAB1 LINES W_CNT.
  IF W_CNT = 0.
    MESSAGE I000 WITH TEXT-M01.
  ELSE.
    CALL SCREEN 9000.
  ENDIF.

ENDFORM.                    " CALL_SCREEN
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INITIALIZATION.

ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  SUBMIT_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SUBMIT_PROCESS USING SEL_WTAB-PROGRAMM.

      RANGES : KEYDATA FOR ZTPP_PP_LOG_DETA-KEYDATA.

      LOOP AT LTAB1.

         KEYDATA-SIGN   = 'I'.
         KEYDATA-OPTION = 'EQ'.
         KEYDATA-LOW    = LTAB1-KEYDATA.
         APPEND KEYDATA.

         CLEAR: LTAB1, KEYDATA.
      ENDLOOP.

*     EXPORT LTAB1 TO DATABASE INDX(ZZ) ID VARIANT1.
     SUBMIT (SEL_WTAB-PROGRAMM) WITH VARIANT1 IN KEYDATA
                                AND RETURN.

*     EXPORT LTAB1 TO DATABASE INDX(ZZ) ID VARIANT1.
*     SUBMIT (SEL_WTAB-PROGRAMM) WITH VARIANT1 EQ VARIANT1
*                                AND RETURN.

ENDFORM.                    " SUBMIT_PROCESS
