************************************************************************
* Program Name      : ZAPP933C_UPDATE_MODC01
* Author            : Byung Sung Bae
* Creation Date     : 2005.11.09.
* Specifications By : Byung Sung Bae
* Pattern           : 2.1
* Development Request No : UD1K918255
* Addl Documentation:
* Description       : Engineering Change Master
* Modification Logs
* Date       Developer    RequestNo    Description
*
************************************************************************
REPORT zapp933c_update_modc01.

*---// Internal tables
DATA: it_abxc01dt LIKE ztbm_abxc01dt OCCURS 0 WITH HEADER LINE.

*---// Global variables
DATA: c_check VALUE 'X'.
DATA: w_error(1).

*---// Selection screens
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-t01.
SELECT-OPTIONS: s_date FOR sy-datum DEFAULT sy-datum.
SELECTION-SCREEN END   OF BLOCK bl1.

SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE text-t04.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 33.
PARAMETERS: rd_udt  RADIOBUTTON GROUP rd1 DEFAULT 'X'.
SELECTION-SCREEN COMMENT  35(20) text-t03.
SELECTION-SCREEN POSITION 58.
PARAMETERS: rd_del RADIOBUTTON GROUP rd1.
SELECTION-SCREEN COMMENT  60(20) text-t02.
SELECTION-SCREEN END   OF LINE.
SELECTION-SCREEN END   OF BLOCK bl2.

*---// Check & Read data
AT SELECTION-SCREEN.
  CHECK sy-ucomm EQ 'ONLI'.
  PERFORM get_data.

START-OF-SELECTION.
  IF w_error = 'X'.
    EXIT.
  ENDIF.
  PERFORM update_table.

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  SELECT * INTO TABLE it_abxc01dt
    FROM ztbm_abxc01dt
   WHERE zsdat          IN s_date.
  IF sy-subrc NE 0.
    w_error = 'X'.
    MESSAGE i000(zz) WITH text-m02.
  ENDIF.
ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  update_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_table.
  CASE c_check.
    WHEN rd_udt.
      MODIFY ztbm_modc01 FROM TABLE it_abxc01dt.
    WHEN rd_del.
      DELETE FROM ztbm_modc01 WHERE zsdat IN s_date.

      INSERT ztbm_modc01 FROM TABLE it_abxc01dt
                         ACCEPTING DUPLICATE KEYS.
      IF sy-subrc NE 0.
        MESSAGE e000(zz) WITH text-m03.
      ENDIF.
  ENDCASE.

  MESSAGE s000(zz) WITH text-m04.
ENDFORM.                    " update_table
