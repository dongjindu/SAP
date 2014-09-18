
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
*
* 04/25/2006 Furong Wang               Parallel processing
*
************************************************************************
REPORT zapp903r_inputplan_mitu_update MESSAGE-ID zmpp.

*----------------------------------------------------------------------
* TABLES DECLARATION
*----------------------------------------------------------------------
TABLES:
        ztpp_input_plan,
        ausp ,
        crhd ,
        plaf.

DATA: l_working_date LIKE sy-datum.
DATA: i_lines TYPE i.
DATA: i_count TYPE i.
DATA: l_start LIKE sy-uzeit.
DATA: l_end LIKE sy-uzeit.

DATA: wa_snd_jobs TYPE i,
      wa_rcv_jobs  TYPE i,
      wa_taskname(4)  TYPE n VALUE '0001',
      wa_excp_flag    TYPE c,
      wa_error        TYPE c,
      wa_flag         TYPE c.

*----------------------------------------------------------------------
* INTERNAL-TABLES DECLARATION
*----------------------------------------------------------------------
DATA: it_input_plan LIKE TABLE OF ztpp_input_plan WITH HEADER LINE.


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
SELECTION-SCREEN COMMENT (23) text-001 FOR FIELD p_wdate.
PARAMETERS: p_wdate       LIKE sy-datum .
SELECTION-SCREEN COMMENT 40(8) text-002 FOR FIELD p_days.
PARAMETERS: p_days  TYPE i DEFAULT '3'  .
SELECTION-SCREEN COMMENT 64(5) text-003.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b3.

AT SELECTION-SCREEN OUTPUT.
  PERFORM initialize_date.

AT SELECTION-SCREEN.
  PERFORM get_reschedule_date.

START-OF-SELECTION.
  GET TIME.
  l_start = sy-uzeit.
  PERFORM get_working_date.
  PERFORM update_mitu_plan_date.
  PERFORM update_planned_order.
  PERFORM write_result.

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
FORM initialize_date.
  p_wdate = sy-datum.
  LOOP AT SCREEN.
    IF screen-name = 'P_WDATE'.
      screen-input = 0.
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
FORM get_working_date.
  DATA: l_kalid  TYPE  kako-kalid .
  PERFORM read_shop_calid   USING l_kalid.
  PERFORM read_working_date USING '-'  l_kalid  p_wdate  .

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
  DATA: l_date LIKE sy-datum.
  l_date = pa_wdate.
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
  IF sy-subrc <> 0.
    MESSAGE e000 WITH 'WORKING DATE READING ERROR.'.
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
FORM update_mitu_plan_date.
*  READ THE MITU RECORDS FROM ZTPP_INPUT_PLAN
  DATA: l_date TYPE rsnum.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_input_plan
      FROM ztpp_input_plan
      WHERE mitu = 'Y'.

  IF sy-subrc = 0.
    CONCATENATE p_wdate+4(2) '/' p_wdate+6(2) '/' p_wdate(4)
       INTO l_date.
    UPDATE ztpp_input_plan
     SET rsnum = l_date
     WHERE mitu = 'Y'.
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
FORM update_planned_order.

  CLEAR i_count.
  DESCRIBE TABLE it_input_plan LINES i_lines.
  IF i_lines > 0.
*    UPDATE THE PLANNED ORDER TABLE
    LOOP AT it_input_plan.

      DO .
        CALL FUNCTION 'Z_FPP_PLANORDER_RESCHEDULE'
            STARTING NEW TASK wa_taskname DESTINATION IN GROUP 'PG_SEQ'
               PERFORMING receive_task ON END OF TASK
            EXPORTING
              p_porder                    = it_input_plan-plnum
              p_date                      = p_wdate
              p_fin_date                  = p_wdate
            EXCEPTIONS
              communication_failure       = 1
              system_failure              = 2
              RESOURCE_FAILURE            = 3
              OTHERS                      = 4 .

        CASE sy-subrc.
          WHEN 0.
            wa_taskname = wa_taskname  + 1.
            wa_snd_jobs = wa_snd_jobs  + 1.
            i_count = i_count + 1.
            CLEAR: wa_excp_flag .
            EXIT.
          WHEN 1 OR 2.
            wa_excp_flag = 'X'.
          WHEN 3.
*Receive reply to asynchronous RFC calls
            IF wa_excp_flag = space.
              wa_excp_flag = 'X'.
*First attempt for RESOURCE_Failure handling
             WAIT UNTIL wa_rcv_jobs >= wa_snd_jobs UP TO '0.01' SECONDS.
            ELSE.
*Second attempt for RESOURCE_Failure handling
              WAIT UNTIL wa_rcv_jobs >= wa_snd_jobs UP TO '0.1' SECONDS.
            ENDIF.
            IF sy-subrc = 0.
              CLEAR wa_excp_flag. " Reset flag
*        ELSE.
*          EXIT.
            ENDIF.
        ENDCASE.
      ENDDO.
*      CALL FUNCTION 'Z_FPP_PLANORDER_RESCHEDULE'
*        EXPORTING
*        p_porder                    = IT_INPUT_PLAN-PLNUM
*        p_date                      = P_WDATE
*        p_fin_date                  = p_wdate
*        EXCEPTIONS
*        COMMUNICATION_FAILURE       = 1
*        SYSTEM_FAILURE              = 2
*        RESOURCE_FAILURE            = 3
**        OTHERS                      = 4 .
*      IF SY-SUBRC <> 0.
*        WRITE:/ TEXT-005,IT_INPUT_PLAN-PLNUM.
*      ELSE.
*        I_COUNT = I_COUNT + 1.
*      ENDIF.
    ENDLOOP.
    WAIT UNTIL wa_rcv_jobs >= wa_snd_jobs.
    IF sy-subrc <> 0.
      WRITE:/ text-005,wa_rcv_jobs,wa_snd_jobs.
    ELSE.
      WRITE:/ text-004, i_count.
    ENDIF.

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
FORM write_result.
*
  GET TIME.
  l_end = sy-uzeit.
  WRITE: / 'EXECUTION TIME: FROM ',l_start, 'TO ',l_end.
  WRITE: / 'Total rescheduled mitu records: ',
           i_count.
ENDFORM.                    " COMMIT_WRITE_RESULT
*&---------------------------------------------------------------------*
*&      Form  GET_RESCHEDULE_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_reschedule_date.
  IF p_days > 0.
    p_wdate = p_wdate + p_days.
  ELSE.
    MESSAGE e000 WITH 'PLEASE ENTER VALID DAYS'.
  ENDIF.
ENDFORM.                    " GET_RESCHEDULE_DATE
*&---------------------------------------------------------------------*
*&      Form  receive_task
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM receive_task USING p_taskname.
  RECEIVE RESULTS FROM FUNCTION 'Z_FPP_PLANORDER_RESCHEDULE'
        EXCEPTIONS
        communication_failure       = 1
        system_failure              = 2
        RESOURCE_FAILURE            = 3
        OTHERS                      = 4.

  CHECK sy-subrc = 0.
  wa_rcv_jobs  = wa_rcv_jobs + 1.

ENDFORM.                    " receive_task
