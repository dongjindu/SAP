*&---------------------------------------------------------------------*
*&      Module  STATUS  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS OUTPUT.
  CASE SY-DYNNR.
    WHEN '9000'.
      SELECT SINGLE * FROM TBTCO
                     WHERE JOBNAME = 'ZASD03_01'
                     AND   STATUS = 'R'. "ACTIVE
      IF SY-SUBRC = 0.
        REFRESH TAB. CLEAR TAB.
        MOVE 'CALC' TO WA_TAB-FCODE.
        APPEND WA_TAB TO TAB.
        MOVE 'RECA' TO WA_TAB-FCODE.
        APPEND WA_TAB TO TAB.
        MOVE 'CONF' TO WA_TAB-FCODE.
        APPEND WA_TAB TO TAB.
        SET PF-STATUS 'ASD03M' EXCLUDING TAB.
      ELSE.
        SET PF-STATUS 'ASD03M'.
      ENDIF.
  ENDCASE.
  set titlebar 'TITLE_9000'.
ENDMODULE.                 " STATUS  OUTPUT



















************************************************************************
***INCLUDE ZISD05L_CMR_CREATE_PBO .
* OUTPUT MODULE FOR TABLECONTROL 'TC_9000':
* COPY DDIC-TABLE TO ITAB
MODULE TC_9000_INIT OUTPUT.
  IF G_TC_9000_COPIED IS INITIAL.
* COPY DDIC-TABLE 'ZTSD_ACL_L'
* INTO INTERNAL TABLE 'g_TC_9000_itab'
*   ==> READ_DATA
    G_TC_9000_COPIED = 'X'.
    REFRESH CONTROL 'TC_9000' FROM SCREEN '9000'.
  ENDIF.
ENDMODULE.

* OUTPUT MODULE FOR TABLECONTROL 'TC_9000':
* MOVE ITAB TO DYNPRO
MODULE TC_9000_MOVE OUTPUT.
  MOVE-CORRESPONDING G_TC_9000_WA TO ZTSD_ACL_L.
ENDMODULE.

* OUTPUT MODULE FOR TABLECONTROL 'TC_9000':
* GET LINES OF TABLECONTROL
MODULE TC_9000_GET_LINES OUTPUT.
  G_TC_9000_LINES = SY-LOOPC.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  GET_LINES  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module GET_LINES output.
  DESCRIBE TABLE IT_REC_L LINES TC9001-LINES.
  DESCRIBE TABLE IT_REC_H LINES TC9002-LINES.
  DESCRIBE TABLE IT_REC_I LINES TC9003-LINES.
endmodule.                 " GET_LINES  OUTPUT
