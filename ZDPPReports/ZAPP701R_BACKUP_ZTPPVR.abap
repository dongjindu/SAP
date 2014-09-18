************************************************************************
* Program Name      : ZAPP701R_BACKUP_ZTPPVR
* Author            : Bobby
* Creation Date     : 2004.01.11.
* Specifications By : Bobby
* Pattern           : 2.1
* Development Request No : UD1K905549
* Addl Documentation:
* Description       : 'ZAPP701R_BACKUP_ZTPPVR
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT ZAPP701R_BACKUP_ZTPPVR     MESSAGE-ID zmpp.

*----------------------------------------------------------------------*
* STANDARD-TABLE AREA ( TABLE)
*----------------------------------------------------------------------*
TABLES: ZTPPVR,
        ZTPPVR_HIS.

*----------------------------------------------------------------------*
* INTERNAL-TABLE AREA
*----------------------------------------------------------------------*
DATA: it_DATA                LIKE TABLE OF ZTPPVR      WITH HEADER LINE.

*----------------------------------------------------------------------*
* Working AREA
*----------------------------------------------------------------------*
DATA: wa_error                 TYPE c,
      wa_check                 TYPE c,
      wa_atinn                 LIKE ausp-atinn,
      wa_atnam                 LIKE cabn-atnam.


*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
*SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-002.
*PARAMETERS:
*  p_BDATE                    LIKE cabn-atnam  OBLIGATORY.
*SELECTION-SCREEN END   OF BLOCK b1.

START-OF-SELECTION.
  PERFORM BACKUP_PROCESSING .

END-OF-SELECTION.


*&---------------------------------------------------------------------*
*&      Form  BACKUP_PROCESSING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BACKUP_PROCESSING   .
  DATA: L_DATE               TYPE D  .

  PERFORM CHECK_DATE USING L_DATE.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_DATA
    from ZTPPVR
   where K04PDAT <= L_DATE .

  READ TABLE IT_DATA WITH KEY ZRESULT = 'E'.
  IF SY-SUBRC = 0.
     MESSAGE E001 WITH TEXT-005 .
     STOP.
  ELSE.
     MODIFY ZTPPVR_HIS FROM TABLE IT_DATA.
  ENDIF.
ENDFORM.                    " BACKUP_PROCESSING


*&---------------------------------------------------------------------*
*&      Form  CHECK_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_DATE  text
*----------------------------------------------------------------------*
FORM CHECK_DATE USING    pA_date.
  IF SY-UZEIT < '063000'.
     PA_DATE = SY-DATUM - 1 .
  ELSE.
     PA_DATE = SY-DATUM.
  ENDIF.

  DO 3 TIMES.
    PA_DATE =  PA_DATE - 1.
    PERFORM read_working_date USING '-'  'T'  PA_date.
  ENDDO.
ENDFORM.                    " CHECK_DATE

*&---------------------------------------------------------------------*
*&      Form  READ_WORKING_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
*----------------------------------------------------------------------*
FORM read_working_date USING  pa_type  pa_kalid  pa_wdate.
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
ENDFORM.                    " READ_WORKING_DATE
