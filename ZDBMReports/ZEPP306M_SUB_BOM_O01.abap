*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  REFRESH IT_FCODE. CLEAR IT_FCODE.
  CASE SY-DYNNR.
    WHEN '8000'.
      IT_FCODE-FCODE = 'V_CRT'.
      APPEND IT_FCODE.
    WHEN '9000'.
      CASE SY-TCODE.
        WHEN 'ZPPE306'.
          IT_FCODE-FCODE = 'DELE'.
          APPEND IT_FCODE.

        WHEN 'ZPPE306_01'.

      ENDCASE.
  ENDCASE.
  SET PF-STATUS 'Z_9000' EXCLUDING IT_FCODE.
  SET TITLEBAR '9000' WITH MODE_TITLE.

ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY OUTPUT.
  MOVE-CORRESPONDING IT_UPGV TO ZSBM_MODEL_VALS.
  PERFORM READ_MAKT.

ENDMODULE.                 " DISPLAY  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
MODULE MODIFY_SCREEN OUTPUT.
  CASE ZMODE.
    WHEN 'CREATE'.
      LOOP AT SCREEN.
        CASE SCREEN-NAME.
          WHEN 'T001W-WERKS' OR
               'MARA-MATNR'  OR
               'MARA-MTART'.
            SCREEN-INPUT = 0.
        ENDCASE.
        MODIFY SCREEN.
        CLEAR SCREEN.

      ENDLOOP.
    WHEN 'CHANGE'.
      LOOP AT SCREEN.
        CASE SCREEN-NAME.
          WHEN 'ZSBM_MODEL_VALS-WERKS' OR
               'ZSBM_MODEL_VALS-MATNR' OR
               'ZSBM_MODEL_VALS-Z_NATION'.
            SCREEN-INPUT = 0.
        ENDCASE.
        MODIFY SCREEN.
        CLEAR SCREEN.

      ENDLOOP.
  ENDCASE.
ENDMODULE.                 " MODIFY_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  INITIAL  OUTPUT
*&---------------------------------------------------------------------*
MODULE INITIAL OUTPUT.
  REFRESH: IT_BOM_VEL, IT_UPGV.
  CLEAR: IT_BOM_VEL, IT_UPGV.
ENDMODULE.                 " INITIAL  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_9100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9100 OUTPUT.
  SET PF-STATUS 'Z_9100'.
  SET TITLEBAR '9100'.

ENDMODULE.                 " STATUS_9100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_9100  OUTPUT
*&---------------------------------------------------------------------*
MODULE DISPLAY_9100 OUTPUT.
  MOVE-CORRESPONDING IT_MODL_VAL TO ZTBM_MODEL_VAL.
ENDMODULE.                 " DISPLAY_9100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CHOICE_MODE  OUTPUT
*&---------------------------------------------------------------------*
MODULE CHOICE_MODE OUTPUT.
  CLEAR: CREATE_MODE, CHANGE_MODE, DELETE_MODE.

  CASE SY-TCODE.
    WHEN 'ZPPE306'.
      CREATE_MODE = 'X'.
      ZMODE = 'CREATE'.
      MODE_TITLE = text-001.
    WHEN 'ZPPE306_01'.
      CHANGE_MODE = 'X'.
      ZMODE = 'CHANGE'.
      MODE_TITLE = text-002.
  ENDCASE.
ENDMODULE.                 " CHOICE_MODE  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_CURSOR  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_CURSOR OUTPUT.
  SET CURSOR FIELD WA_ZFIELD1
             LINE WA_CURSOR_LINE.
ENDMODULE.                 " SET_CURSOR  OUTPUT
