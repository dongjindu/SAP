
***********************************************************************
* Program Name      : ZAPP805R_MITU_REFRESH
* Author            : YONGPING
* Creation Date     : 2004.09.21.
* Specifications By :
* Pattern           :
* Development Request No :UD1K912260
* Addl Documentation:
* Description       : Update Input Plan MITU Rescheduling date
*
* Modification Logs
* Date       Developer    RequestNo    Description
* Change : Change the Time Tule(Tack-Time --> Lead Time)
************************************************************************
REPORT ZAPP903R_INPUTPLAN_MITU_UPDATE MESSAGE-ID ZMPP.

*----------------------------------------------------------------------
* TABLES DECLARATION
*----------------------------------------------------------------------
TABLES:
        ztpp_input_plan,
        ausp ,
        crhd ,
        plaf.

DATA: L_WORKING_DATE LIKE SY-DATUM.
DATA: I_LINES TYPE I.
DATA: I_COUNT TYPE I.
DATA: L_START LIKE SY-UZEIT.
DATA: L_END LIKE SY-UZEIT.
*----------------------------------------------------------------------
* INTERNAL-TABLES DECLARATION
*----------------------------------------------------------------------
DATA: IT_INPUT_PLAN LIKE TABLE OF ZTPP_INPUT_PLAN WITH HEADER LINE.


*----------------------------------------------------------------------
* WORKING-AREA DECLARATION
*----------------------------------------------------------------------

*----------------------------------------------------------------------
* FIELD-SYMBOLS DECLARATION
*----------------------------------------------------------------------

*----------------------------------------------------------------------
* SCREEN-VARIABLES DECLARATION
*----------------------------------------------------------------------

*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-004.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (23) TEXT-001 FOR FIELD P_WDATE.
PARAMETERS: p_WDATE       LIKE SY-DATUM .
SELECTION-SCREEN COMMENT 40(8) TEXT-002 FOR FIELD P_DAYS.
PARAMETERS: P_DAYS  TYPE I DEFAULT '3'  .
SELECTION-SCREEN COMMENT 64(5) TEXT-003.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b3.

AT SELECTION-SCREEN OUTPUT.
  PERFORM INITIALIZE_DATE.
AT SELECTION-SCREEN.
  PERFORM GET_RESCHEDULE_DATE.

START-OF-SELECTION.
  GET TIME.
  L_START = SY-UZEIT.
  PERFORM GET_WORKING_DATE.
  PERFORM UPDATE_MITU_PLAN_DATE.
  PERFORM UPDATE_PLANNED_ORDER.
  PERFORM WRITE_RESULT.

END-OF-SELECTION.



*****************************************************************
**** FORMS
*****************************************************************


* P_WDATE = SY-DATUM + 3.
*&---------------------------------------------------------------------*
*&      Form  INITIALIZE_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INITIALIZE_DATE.
   P_WDATE = SY-DATUM.
  LOOP AT SCREEN.
    IF SCREEN-NAME = 'P_WDATE'.
      SCREEN-INPUT = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " INITIALIZE_DATE
*&---------------------------------------------------------------------*
*&      Form  READ_WORKING_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_WORKING_DATE.
  DATA: L_KALID  TYPE  kako-kalid .
    PERFORM read_shop_calid   USING L_kalid.
    PERFORM read_working_date USING '-'  L_KALID  P_WDATE  .

ENDFORM.                    " READ_WORKING_DATE


*&---------------------------------------------------------------------*
*&      Form  READ_SHOP_CALID
*&---------------------------------------------------------------------*
*       READING SHOP CALENDAR
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*


FORM read_shop_calid USING    pa_kalid.
  SELECT SINGLE kalid INTO pa_kalid
    FROM zvpp_capacity
   WHERE arbpl = 'B'   .
ENDFORM.                    " READ_SHOP_CALID


*&---------------------------------------------------------------------*
*&      Form  READ_WORKING_DATE
*&---------------------------------------------------------------------*
*       READING WORKING DATE
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*


FORM read_working_date USING  pa_type  pa_kalid  pa_wdate.
  DATA: L_DATE LIKE SY-DATUM.
  L_DATE = PA_WDATE.
  CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
       EXPORTING
            correct_option               = pa_type
            date                         = pa_wdate
            factory_calendar_id          = pa_kalid
       IMPORTING
            date                         = pa_wdate
       EXCEPTIONS
            calendar_buffer_not_loadable = 1
            correct_option_invalid       = 2
            date_after_range             = 3
            date_before_range            = 4
            date_invalid                 = 5
            factory_calendar_not_found   = 6
            OTHERS                       = 7.
   IF SY-SUBRC <> 0.
      MESSAGE E000 WITH 'WORKING DATE READING ERROR.'.
*     PA_WDATE = L_DATE.
   ENDIF.
ENDFORM.                    " read_working_date


*&---------------------------------------------------------------------*
*&      Form  UPDATE_MITU_PLAN_DATE
*&---------------------------------------------------------------------*
*       UPDATE THE MITU CAR RESCHEDULE DATE IN
*       TABLE ZTPP_INPUT_PLAN
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_MITU_PLAN_DATE.
*  READ THE MITU RECORDS FROM ZTPP_INPUT_PLAN
 DATA: L_DATE TYPE RSNUM.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_INPUT_PLAN
      FROM ZTPP_INPUT_PLAN
      WHERE MITU = 'Y'.

  IF SY-SUBRC = 0.
      CONCATENATE P_WDATE+4(2) '/' P_WDATE+6(2) '/' P_WDATE(4)
         INTO L_DATE.
      UPDATE ZTPP_INPUT_PLAN
       SET RSNUM = L_DATE
       WHERE MITU = 'Y'.
  ENDIF.
* IF NO RECORD FOUND DO NOTHING
ENDFORM.                    " UPDATE_MITU_PLAN_DATE
*&---------------------------------------------------------------------*
*&      Form  UPDATE_PLANNED_ORDER
*&---------------------------------------------------------------------*
*       UPDATE PLANNED ORDER TABLE FOR MITU CAR'S
*       SCHEDULE START, FINISH AND PLANNED DATE
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_PLANNED_ORDER.
   CLEAR I_COUNT.
   DESCRIBE TABLE IT_INPUT_PLAN LINES I_LINES.
   IF I_LINES > 0.
*    UPDATE THE PLANNED ORDER TABLE
     LOOP AT IT_INPUT_PLAN.

      CALL FUNCTION 'Z_FPP_PLANORDER_RESCHEDULE'
        EXPORTING
        p_porder                    = IT_INPUT_PLAN-PLNUM
        p_date                      = P_WDATE
        p_fin_date                  = p_wdate
        EXCEPTIONS
        COMMUNICATION_FAILURE       = 1
        SYSTEM_FAILURE              = 2
        RESOURCE_FAILURE            = 3
        OTHERS                      = 4 .
      IF SY-SUBRC <> 0.
        WRITE:/ TEXT-005,IT_INPUT_PLAN-PLNUM.
      ELSE.
        I_COUNT = I_COUNT + 1.
      ENDIF.
     ENDLOOP.
   ENDIF.

ENDFORM.                    " UPDATE_PLANNED_ORDER
*&---------------------------------------------------------------------*
*&      Form  WRITE_RESULT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM WRITE_RESULT.
*
   GET TIME.
   L_END = SY-UZEIT.
   WRITE: / 'EXECUTION TIME: FROM ',L_START, 'TO ',L_END.
   WRITE: / 'Total rescheduled mitu records: ',
            I_COUNT.
ENDFORM.                    " COMMIT_WRITE_RESULT
*&---------------------------------------------------------------------*
*&      Form  GET_RESCHEDULE_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_RESCHEDULE_DATE.
   IF P_DAYS > 0.
     P_WDATE = P_WDATE + P_DAYS.
   ELSE.
     MESSAGE E000 WITH 'PLEASE ENTER VALID DAYS'.
   ENDIF.
ENDFORM.                    " GET_RESCHEDULE_DATE
