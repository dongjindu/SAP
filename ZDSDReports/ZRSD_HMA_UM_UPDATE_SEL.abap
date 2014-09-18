
*----------------------------------------------------------------------*
*   INCLUDE ZRPP_HMA_ZPODER_SEL                                        *
*----------------------------------------------------------------------*
*-------------------------------------------------------------------*
*  SELECT-OPTIONS
*-------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECTION-SCREEN SKIP 1.
PARAMETERS : " P_CREATE AS CHECKBOX DEFAULT 'X',
             p_update AS CHECKBOX DEFAULT 'X',
             p_fillv  AS CHECKBOX DEFAULT 'X',
             p_view   AS CHECKBOX USER-COMMAND disp,
             p_mestyp LIKE edidc-mestyp DEFAULT 'ZPODER_MST' NO-DISPLAY.
SELECTION-SCREEN SKIP.
PARAMETERS : p_zvin   AS CHECKBOX.
PARAMETERS : p_nation LIKE ztpp_vm-wo_nation.

SELECT-OPTIONS : s_credat FOR sy-datum MODIF ID s1.

SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
*     AT SELECTION-SCREEN                                              *
*----------------------------------------------------------------------*

AT SELECTION-SCREEN OUTPUT.
  IF p_view EQ 'X' .
    LOOP AT SCREEN.
      CASE screen-group1.
        WHEN 'S1'.
          screen-active = 1.
      ENDCASE.
      MODIFY SCREEN.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      CASE screen-group1.
        WHEN 'S1'.
          screen-active = 0.
      ENDCASE.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.
