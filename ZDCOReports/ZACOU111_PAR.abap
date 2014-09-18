*----------------------------------------------------------------------
* Program ID        : ZACOU111_PAR
* Title             : [CO] Collect Price Variance: Parallel processing
* Created on        : 09/13/2006
* Created by        : Michelle Jeong
* Specifications By : Andy Choi
* Description       : Parallel Collect Price Variance
*----------------------------------------------------------------------
REPORT ZACOU111_PAR NO STANDARD PAGE HEADING MESSAGE-ID ZMCO.

INCLUDE ZACOU111_TOP.

* Parameter for progressing
DATA : BEGIN OF GT_TASK OCCURS 0,
         NAME(20),
         STATUS(1),
       END OF GT_TASK.

DATA : BEGIN OF GT_RETURN OCCURS 0,
         TIMES(10),
         RETURN(1),
       END OF GT_RETURN.

DATA: GT_MAT_TEMP TYPE TABLE OF ZSCOUMAT WITH HEADER LINE,
      G_COUNT(5)  TYPE N.

*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-000.
PARAMETERS: P_BUKRS LIKE T001T-BUKRS DEFAULT 'H201' OBLIGATORY,
            P_MONTH LIKE S001-SPMON DEFAULT SY-DATUM(6) OBLIGATORY.
SELECT-OPTIONS: S_MTART FOR MARA-MTART DEFAULT 'ROH',
                S_MATNR FOR MARA-MATNR,
                S_BELNR FOR MLHD-BELNR.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
PARAMETERS: P_GR AS CHECKBOX DEFAULT 'X',
            P_IV AS CHECKBOX,
            P_RV AS CHECKBOX,
            P_IM AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK B2.

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-002.
PARAMETERS :  P_PR_CNT  TYPE I DEFAULT 1,
              P_TA_CNT  TYPE I DEFAULT 5,
              P_WT_SCN  TYPE I DEFAULT 5.
SELECTION-SCREEN END OF BLOCK B3.

*----------------------------------------------------------------------*
* Initialization
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM SET_INITIALIZATION IN PROGRAM ZACOU111.

*----------------------------------------------------------------------*
* Start of selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM GET_MATERIAL IN PROGRAM ZACOU111.

  IF GT_MAT[] IS INITIAL.
    MESSAGE S000 WITH 'No data found.'.
    EXIT.
  ELSE.
    PERFORM CALL_PARALLE_FUNCTION.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  CALL_PARALLE_FUNCTION
*&---------------------------------------------------------------------*
FORM CALL_PARALLE_FUNCTION.
  DATA : L_START TYPE I,
         L_END   TYPE I,
         L_TEXT(100).

* P_PR_CNT : line count for one tasking
  L_START = 1.
  L_END   = P_PR_CNT.

  SORT GT_MAT BY MATNR.

  DO.
    G_COUNT = G_COUNT + 1.
    GT_RETURN-TIMES = G_COUNT.
    APPEND GT_RETURN.

    REFRESH GT_MAT_TEMP.
    APPEND LINES OF GT_MAT_TEMP FROM L_START TO L_END TO GT_MAT.

    DELETE GT_MAT_TEMP FROM L_START TO L_END.

* Call function for submit real program.
    PERFORM CALL_RFC_FUNCTION TABLES GT_MAT
                              USING SY-INDEX.

* Progress Parameter  -2
    IF GT_MAT_TEMP[] IS INITIAL.
      EXIT.
    ENDIF.

  ENDDO.

* Finish if no working Job
  DO.
    READ TABLE GT_TASK WITH KEY STATUS = 'W'.
    IF SY-SUBRC <> 0.
      EXIT.
    ENDIF.
    WAIT UP TO 5 SECONDS.
  ENDDO.

ENDFORM.                    " CALL_PARALLE_FUNCTION
*&---------------------------------------------------------------------*
*&      Form  CALL_RFC_FUNCTION
*&---------------------------------------------------------------------*
FORM CALL_RFC_FUNCTION  TABLES   PT_MAT     STRUCTURE GT_MAT
                        USING    P_TIMES LIKE SY-INDEX.

  DATA : MSG_TXT(80),
         L_INDEX  TYPE SY-TABIX,
         L_TEXT(100),
         L_SERIAL TYPE I .

  DO.
* find working idle task
    READ TABLE GT_TASK WITH KEY STATUS = 'I'.
    IF SY-SUBRC = 0 .
      L_INDEX = SY-TABIX.

      READ TABLE PT_MAT INDEX 1.

*      CONCATENATE GT_TASK-NAME G_COUNT
*                  GT_TASK-STATUS INTO L_TEXT SEPARATED BY SPACE.

      CONCATENATE GT_TASK-NAME PT_MAT-MATNR G_COUNT GT_TASK-STATUS
                  '...started' INTO L_TEXT SEPARATED BY SPACE.

      PERFORM PROGRESS_IND IN PROGRAM ZACOU111 USING '20' L_TEXT.

      CALL FUNCTION 'Z_COLLECT_PRICE_DIFF_PARALLEL'
           STARTING NEW TASK GT_TASK-NAME
           DESTINATION IN GROUP ''
           PERFORMING GET_RESULT ON END OF TASK
           EXPORTING
              P_BUKRS = P_BUKRS
              P_MONTH = P_MONTH
              P_GR    = P_GR
              P_IV    = P_IV
              P_RV    = P_RV
              P_IM    = P_IM
           TABLES
              IT_MAT  = GT_MAT
           EXCEPTIONS
              COMMUNICATION_FAILURE = 1
              SYSTEM_FAILURE        = 2
              RESOURCEFAILURE       = 3
              OTHERS                = 4.

      IF SY-SUBRC <> 0.  " Don't assign JOB in Error TASK
        GT_TASK-STATUS = 'E'.
        MODIFY GT_TASK INDEX L_INDEX.
      ELSE.
        GT_TASK-STATUS = 'W'.
        MODIFY GT_TASK INDEX L_INDEX.
      ENDIF.
      EXIT.
    ELSE.
      WAIT UP TO P_WT_SCN SECONDS.  "waiting
    ENDIF.
  ENDDO.

ENDFORM.                    " CALL_RFC_FUNCTION

*---------------------------------------------------------------------*
*       FORM GET_RESULT                                               *
*---------------------------------------------------------------------*
FORM GET_RESULT  USING TASK_NAME.
  DATA : LT_ERROR TYPE TABLE OF ZSCOUMAT WITH HEADER LINE,
         L_TIMES  LIKE SY-INDEX.

* Get Result
  RECEIVE RESULTS FROM FUNCTION 'Z_COLLECT_PRICE_DIFF_PARALLEL'
     IMPORTING
        E_TIMES  = L_TIMES
     TABLES
        IT_ERROR = LT_ERROR
       EXCEPTIONS
           COMMUNICATION_FAILURE = 1
           SYSTEM_FAILURE        = 2 .


  READ TABLE GT_RETURN WITH KEY TIMES = L_TIMES.
  IF SY-SUBRC = 0 .
    GT_RETURN-RETURN = 'X'.
    MODIFY GT_RETURN INDEX SY-TABIX.
  ENDIF.

* TASK RELEASE.
  READ TABLE GT_TASK WITH KEY NAME = TASK_NAME.
  GT_TASK-STATUS = 'I'.
  MODIFY GT_TASK INDEX SY-TABIX.

ENDFORM.
