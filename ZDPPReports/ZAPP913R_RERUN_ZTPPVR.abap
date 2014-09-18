************************************************************************
* Program Name      : ZAPP913R_RERUN_ZTPPVR
* Author            : Bobby
* Creation Date     : 2004.01.11.
* Specifications By : Bobby
* Pattern           : 2.1
* Development Request No : UD1K905549
* Addl Documentation:
* Description       : 'Re-Run the Data using the RFC Function
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT zapp913r_rerun_ztppvr .

DATA: it_zsppvr          LIKE TABLE OF zsppvr   WITH HEADER LINE.

*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_flag        LIKE ztppvr-flag  OBLIGATORY,
            p_date        LIKE sy-datum     OBLIGATORY,
            p_result      LIKE ztppvr-zresult         .
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------
  PERFORM read_data.
  PERFORM run_data .
  PERFORM write_result.


*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_zsppvr
    FROM ztppvr
   WHERE flag    = p_flag
     AND zsdat   = p_date
     AND zresult = p_result
     AND zmsg   NE space   .

ENDFORM.                    " READ_DATA

*&---------------------------------------------------------------------*
*&      Form  RUN_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM run_data.
  CALL FUNCTION 'Z_FPP_GET_ZTPPVR'
       TABLES
            i_zsppvr = it_zsppvr.
ENDFORM.                    " RUN_DATA

*&---------------------------------------------------------------------*
*&      Form  WRITE_RESULT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_result.
  LOOP AT it_zsppvr .
    write at: /001(001) it_zsppvr-ZZRET,
               003(001) it_zsppvr-zresult,
               005(030) it_zsppvr-zmsg ,
               036(400) it_zsppvr      .
  ENDLOOP.
ENDFORM.                    " WRITE_RESULT
