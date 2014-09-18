************************************************************************
* Program Name      : ZAPP903R_DAY_PLAN
* Author            : Bobby
* Creation Date     : 2003.12.23.
* Specifications By : Bobby
* Pattern           : 2.1
* Development Request No : UD1K902288
* Addl Documentation:
* Description       :
*          Production Plan(Daily) : Saving the Sequence Data from APS
* Modification Logs
* Date       Developer    RequestNo    Description
* Change : Change the Time Tule(Tack-Time --> Lead Time)
************************************************************************
REPORT  zapp903r_day_plan     MESSAGE-ID zmpp.

*----------------------------------------------------------------------
* TABLES DECLARATION
*----------------------------------------------------------------------
TABLES: ztpp_pmt07jb_b,
        ztpp_day_plan .

*----------------------------------------------------------------------
* INTERNAL-TABLES DECLARATION
*----------------------------------------------------------------------
DATA: it_data             LIKE TABLE OF ztpp_pmt07jb_b WITH HEADER LINE.

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
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
PARAMETERS: p_run         TYPE c   AS CHECKBOX DEFAULT 'X' .
SELECTION-SCREEN END OF BLOCK b3.

*----------------------------------------------------------------------
* INCLUDE PROGRAM.....
*----------------------------------------------------------------------

*----------------------------------------------------------------------
INITIALIZATION.
*----------------------------------------------------------------------


*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------
  IF p_run = 'X'                       .
    PERFORM read_data                  .
    PERFORM save_data                  .
    PERFORM display_data               .
  ENDIF.

*----------------------------------------------------------------------
END-OF-SELECTION.
*----------------------------------------------------------------------


*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data     .
  DATA: lt_data                 LIKE TABLE OF it_data  WITH HEADER LINE.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_data
    FROM ztpp_pmt07jb_b .

  lt_data[] = it_data[] .
  DELETE ADJACENT DUPLICATES FROM lt_data COMPARING sqdt .

  LOOP AT lt_data.
    DELETE FROM ztpp_day_plan WHERE sqdt = lt_data-sqdt.
  ENDLOOP.
ENDFORM.                    " READ_DATA

*&---------------------------------------------------------------------*
*&      Form  save_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_data.
  MODIFY ZTPP_DAY_PLAN FROM TABLE IT_DATA .
  IF SY-SUBRC = 0.
     MESSAGE S001 WITH TEXT-001 .
  ELSE.
     MESSAGE e002 WITH TEXT-002 sy-subrc.
  ENDIF.
ENDFORM.                    " save_data

*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data.
  " Display the Data...
ENDFORM.                    " display_data
