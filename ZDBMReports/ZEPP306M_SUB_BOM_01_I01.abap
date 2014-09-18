*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE EXIT INPUT.
  OKCODE = OK_CODE.
  CASE OKCODE.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
*      PERFORM DATA_CHECK_SAVE.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.
  OKCODE = OK_CODE.
  CLEAR OK_CODE.
  CASE OKCODE.
    WHEN 'SAVE'.
      PERFORM SAVE_PROCESS.
    WHEN 'ENTR'.
      PERFORM READ_MARA_MTART.
    WHEN 'V_CRT'.
      CLEAR WA_FIELD.
      GET CURSOR FIELD WA_FIELD.
      PERFORM CREATE_MODEL_VALUE.
    WHEN 'CREATE'.
      PERFORM REFRESH_DATA.
      CALL TRANSACTION 'ZPPE306N'.
    WHEN 'CHANGE'.
      PERFORM REFRESH_DATA.
      CALL TRANSACTION 'ZPPE306N_01'.
    WHEN 'DELE'.
      PERFORM DELETE_PROCESS.
    WHEN 'ZTBM_VL_NV'.
      CALL TRANSACTION 'ZTBM_VL_NV'.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY  INPUT
*&---------------------------------------------------------------------*
MODULE MODIFY INPUT.
  PERFORM MODIFY_9000.
ENDMODULE.                 " MODIFY  INPUT
*&---------------------------------------------------------------------*
*&      Module  FIELD_VELUE  INPUT
*&---------------------------------------------------------------------*
MODULE FIELD_VELUE INPUT.
  PERFORM FIELD_VALUE_CHECK.
ENDMODULE.                 " FIELD_VELUE  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_HELP_Z_CAR  INPUT
*&---------------------------------------------------------------------*
MODULE SEARCH_HELP_Z_CAR INPUT.
  PERFORM SEARCH_HELP USING   '01'
                              'ZSBM_MODEL_VALS_01-Z_CAR'.
ENDMODULE.                 " SEARCH_HELP_Z_CAR  INPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_9100  INPUT
*&---------------------------------------------------------------------*
MODULE MODIFY_9100 INPUT.
*  DATA: L_COUNT TYPE I.
*  IF    NOT ZTBM_MODEL_VAL_N-ZFIELD IS INITIAL
*    AND NOT ZTBM_MODEL_VAL_N-ZVALUE IS INITIAL.
*    LOOP AT IT_MODL_VAL WHERE ZFIELD EQ ZTBM_MODEL_VAL_N-ZFIELD
*                        AND   ZVALUE EQ ZTBM_MODEL_VAL_N-ZVALUE.
*      L_COUNT = L_COUNT + 1.
*    ENDLOOP.
*  ENDIF.
*  IF L_COUNT GT 1.
**    MESSAGE E000 WITH 'SAME DATA'.
*  ELSE.
  MOVE-CORRESPONDING ZTBM_MODEL_VAL_N TO IT_MODL_VAL.
  MODIFY IT_MODL_VAL INDEX T_9100-CURRENT_LINE.
  IF SY-SUBRC NE 0.
    APPEND IT_MODL_VAL.
  ENDIF.
*  ENDIF.
*  DESCRIBE TABLE IT_MODL_VAL LINES WA_LINES.
*  T_9100-LINES = WA_LINES + 1.
ENDMODULE.                 " MODIFY_9100  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9100  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_9100 INPUT.
  OKCODE = OK_CODE.

  CLEAR OK_CODE.

  CASE OKCODE.
    WHEN 'SAVE'.
      DELETE IT_MODL_VAL WHERE ZVALUE IS INITIAL.
      PERFORM SAVE_ZTBM_MODEL_VAL_N.
      DELETE IT_MODL_DEL WHERE ZVALUE IS INITIAL.
      PERFORM MODIFY_ZTBM_MODEL_VAL_N.
    WHEN 'LINE_DEL' OR 'LINE_ROW' OR 'LINE_INS'.
      PERFORM LINE_INSERT.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9100  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_HELP_Z_YEAR  INPUT
*&---------------------------------------------------------------------*
MODULE SEARCH_HELP_Z_YEAR INPUT.
  PERFORM SEARCH_HELP USING   '02' 'ZSBM_MODEL_VALS_01-Z_YEAR'.
ENDMODULE.                 " SEARCH_HELP_Z_YEAR  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_HELP_Z_NATION  INPUT
*&---------------------------------------------------------------------*
MODULE SEARCH_HELP_Z_NATION INPUT.
  PERFORM SEARCH_HELP USING   '03' 'ZSBM_MODEL_VALS_01-Z_NATION'.

ENDMODULE.                 " SEARCH_HELP_Z_NATION  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_HELP_Z_BT  INPUT
*&---------------------------------------------------------------------*
MODULE SEARCH_HELP_Z_BT INPUT.
  PERFORM SEARCH_HELP USING   '04' 'ZSBM_MODEL_VALS_01-Z_BT'.

ENDMODULE.                 " SEARCH_HELP_Z_BT  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_HELP_Z_TL  INPUT
*&---------------------------------------------------------------------*
MODULE SEARCH_HELP_Z_TL INPUT.
  PERFORM SEARCH_HELP USING   '05' 'ZSBM_MODEL_VALS_01-Z_TL'.

ENDMODULE.                 " SEARCH_HELP_Z_TL  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_HELP_Z_EC  INPUT
*&---------------------------------------------------------------------*
MODULE SEARCH_HELP_Z_EC INPUT.
  PERFORM SEARCH_HELP USING   '06' 'ZSBM_MODEL_VALS_01-Z_EC'.

ENDMODULE.                 " SEARCH_HELP_Z_EC  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_HELP_Z_FT  INPUT
*&---------------------------------------------------------------------*
MODULE SEARCH_HELP_Z_FT INPUT.
  PERFORM SEARCH_HELP USING   '07' 'ZSBM_MODEL_VALS_01-Z_FT'.

ENDMODULE.                 " SEARCH_HELP_Z_FT  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_HELP_Z_TM  INPUT
*&---------------------------------------------------------------------*
MODULE SEARCH_HELP_Z_TM INPUT.
  PERFORM SEARCH_HELP USING   '08' 'ZSBM_MODEL_VALS_01-Z_TM'.

ENDMODULE.                 " SEARCH_HELP_Z_TM  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_HELP_Z_ST  INPUT
*&---------------------------------------------------------------------*
MODULE SEARCH_HELP_Z_ST INPUT.
  PERFORM SEARCH_HELP USING   '09' 'ZSBM_MODEL_VALS_01-Z_ST'.
ENDMODULE.                 " SEARCH_HELP_Z_ST  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_HELP_Z_COLOR  INPUT
*&---------------------------------------------------------------------*
MODULE SEARCH_HELP_Z_COLOR INPUT.
  PERFORM SEARCH_HELP USING   '10' 'ZSBM_MODEL_VALS_01-Z_COLOR'.

ENDMODULE.                 " SEARCH_HELP_Z_COLOR  INPUT
*&---------------------------------------------------------------------*
*&      Module  FIELD_ZVALUE  INPUT
*&---------------------------------------------------------------------*
MODULE FIELD_ZVALUE INPUT.
  PERFORM FIELD_ZVALUE.
ENDMODULE.                 " FIELD_ZVALUE  INPUT
*&---------------------------------------------------------------------*
*&      Module  FIELD_MATNR  INPUT
*&---------------------------------------------------------------------*
MODULE FIELD_MATNR INPUT.
  PERFORM FIELD_MATNR.
ENDMODULE.                 " FIELD_MATNR  INPUT
*&---------------------------------------------------------------------*
*&      Module  FIELD_VALUE  INPUT
*&---------------------------------------------------------------------*
MODULE FIELD_VALUE INPUT.
  PERFORM FIELD_VALUE.
ENDMODULE.                 " FIELD_VALUE  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_CURSOR  INPUT
*&---------------------------------------------------------------------*
MODULE GET_CURSOR INPUT.
  PERFORM GET_CURSOR.

ENDMODULE.                 " GET_CURSOR  INPUT

MODULE SEARCH_HELP_Z_DEALER INPUT.
  PERFORM SEARCH_HELP USING   '11' 'ZSBM_MODEL_VALS_01-Z_DEALER'.
ENDMODULE.                 " SEARCH_HELP_Z_DEALER  INPUT

MODULE SEARCH_HELP_Z_ET INPUT.
  PERFORM SEARCH_HELP USING   '12' 'ZSBM_MODEL_VALS_01-Z_ET'.
ENDMODULE.                 " SEARCH_HELP_Z_ET  INPUT

MODULE SEARCH_HELP_Z_OCN INPUT.
  PERFORM SEARCH_HELP USING   '13' 'ZSBM_MODEL_VALS_01-Z_OCN'.

ENDMODULE.                 " SEARCH_HELP_Z_OCN  INPUT
