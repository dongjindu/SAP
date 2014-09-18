************************************************************************
* Program Name      : ZEMMPM41E_PRC_ANALY
* Author            : Byung sung Bae
* Creation Date     : 2003.12.01.
* Specifications By : Min-su Park
* Pattern           : Report 1-1
* Development Request No : UD1K901872
* Addl Documentation:
* Description       : Master Inspection Characteristic Uploading
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT  zemmpm41e_prc_anal.
TABLES: mara.

*----- Selection screens
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-t01.
PARAMETERS:     p_werks  LIKE t001w-werks DEFAULT 'P001' OBLIGATORY.
PARAMETERS:     p_period LIKE s001-spmon DEFAULT sy-datum(6) OBLIGATORY.
SELECT-OPTIONS: s_matnr  FOR  mara-matnr NO-EXTENSION.
SELECTION-SCREEN END   OF BLOCK bl1.

*----- Read data
at selection-screen.
perform read_data.

*&---------------------------------------------------------------------*
*&      Form  read_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_data.
*  EXEC SQL PERFORMING APPEND_ITAB.
*    SELECT
*      INTO
*      FROM MSEG A, EKPO B
*     WHERE A.MANDT = :SY-MANDT
*       AND A.ZBUDAT
*  ENDEXEC.
endform.                    " read_data
