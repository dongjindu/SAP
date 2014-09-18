*----------------------------------------------------------------------*
*   INCLUDE MZAHR0002O01                                               *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  SET PF-STATUS 'PS9000'.
  SET TITLEBAR '900'.
ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  INIT_INTERNAL_TABLE  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INIT_INTERNAL_TABLE OUTPUT.
  IF W_INITF = ' '.
    CLEAR IT_S9000. REFRESH IT_S9000.
    IT_S9000-INDEX = 1. APPEND IT_S9000.

    DO 99 TIMES.
      IT_S9000-INDEX = IT_S9000-INDEX + 1.
      APPEND IT_S9000.
    ENDDO.

    REFRESH CONTROL 'TC9000' FROM SCREEN 9000.
    DESCRIBE TABLE IT_S9000 LINES TC9000-LINES.

    W_ZVERS = 311.
    PERFORM GET_PERSONNEL_COUNT.
    W_INITF = 'X'.
  ENDIF.
ENDMODULE.                 " INIT_INTERNAL_TABLE  OUTPUT
