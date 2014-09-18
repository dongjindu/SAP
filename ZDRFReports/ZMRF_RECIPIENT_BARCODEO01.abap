*----------------------------------------------------------------------*
*   INCLUDE ZMRF_RECIPIENT_BARCODEO01                                  *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9900  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9900 OUTPUT.
  REFRESH IT_FCODE. CLEAR IT_FCODE.
  IF CHANGE_MODE EQ 'X'.
      IT_FCODE-FCODE = 'NLINE'.
      APPEND IT_FCODE.
  ENDIF.
  SET PF-STATUS '9900' EXCLUDING IT_FCODE.
  SET TITLEBAR '9900'.

ENDMODULE.                 " STATUS_9900  OUTPUT

*&spwizard: output module for tc 'TC_RECP'. do not change this line!
*&spwizard: copy ddic-table to itab
MODULE TC_RECP_INIT OUTPUT.
  IF G_TC_RECP_COPIED IS INITIAL.
*&spwizard: copy ddic-table 'ZTRF_RECIPIENT'
*&spwizard: into internal table 'g_TC_RECP_itab'
    SELECT * FROM ZTRF_RECIPIENT
       INTO CORRESPONDING FIELDS
       OF TABLE G_TC_RECP_ITAB.
    G_TC_RECP_COPIED = 'X'.
    REFRESH CONTROL 'TC_RECP' FROM SCREEN '9900'.
  ENDIF.
ENDMODULE.

*&spwizard: output module for tc 'TC_RECP'. do not change this line!
*&spwizard: move itab to dynpro
MODULE TC_RECP_MOVE OUTPUT.
  MOVE-CORRESPONDING G_TC_RECP_WA TO ZTRF_RECIPIENT.
ENDMODULE.

*&spwizard: output module for tc 'TC_RECP'. do not change this line!
*&spwizard: get lines of tablecontrol
MODULE TC_RECP_GET_LINES OUTPUT.
  G_TC_RECP_LINES = SY-LOOPC.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SCREEN_MODIFY  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SCREEN_MODIFY OUTPUT.
  IF CHANGE_MODE EQ 'X'.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 EQ 'GR1'.
        SCREEN-INPUT = '1'.
        MODIFY SCREEN.
      ENDIF.
      IF NOT ZTRF_RECIPIENT-PERNR IS INITIAL.
      IF SCREEN-GROUP1 EQ 'GR1'.
        SCREEN-INPUT = '0'.
        MODIFY SCREEN.
      ENDIF.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 EQ 'GR1'.
        SCREEN-INPUT = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDMODULE.                 " SCREEN_MODIFY  OUTPUT
