*----------------------------------------------------------------------*
***INCLUDE MZEMMPM45E_MODULEO01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  SET PF-STATUS '100'.
  SET TITLEBAR '100'.
TC_100-LINES = 25.

ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.
  SET PF-STATUS '200'.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0300 OUTPUT.
  SET PF-STATUS '300'.
  SET TITLEBAR  '300'.

ENDMODULE.                 " STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GET_TC_LINE  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_TC_LINE OUTPUT.
  MOVE : SY-LOOPC TO WA_LINE.
ENDMODULE.                 " GET_TC_LINE  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0400  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0400 OUTPUT.
  SET PF-STATUS '400'.
  SET TITLEBAR '400'.

ENDMODULE.                 " STATUS_0400  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GET_TC_LINE_400  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_TC_LINE_400 OUTPUT.
  DATA : ICON LIKE ICON-NAME,
         NAME(40) TYPE C.

* TABLE  ICON REFERANCE
  CASE IT_400-MODE .
    WHEN 'E'.
      NAME = 'ICON_RED_LIGHT'.
    WHEN OTHERS .
      NAME = 'ICON_GREEN_LIGHT'.
  ENDCASE.

  CALL FUNCTION 'ICON_CREATE'
       EXPORTING
            NAME                  = NAME
            INFO                  = 'Status'
            ADD_STDINF            = 'X'
       IMPORTING
            RESULT                = ICON
       EXCEPTIONS
            ICON_NOT_FOUND        = 1
            OUTPUTFIELD_TOO_SHORT = 2
            OTHERS                = 3.

****- TABLE CONTROL
  MOVE : SY-LOOPC TO WA_LINE.
****
ENDMODULE.                 " GET_TC_LINE_400  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SCREEN_MODIY  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SCREEN_MODIY OUTPUT.

  LOOP AT SCREEN.
    IF   SCREEN-GROUP1 = 'GA1' AND IT_400-FLAG = 'N'.
      SCREEN-INPUT = '1'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDMODULE.                 " SCREEN_MODIY  OUTPUT
