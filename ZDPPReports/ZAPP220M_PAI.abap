*----------------------------------------------------------------------*
***INCLUDE ZAPP220M_PAI .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  exit_100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_100 INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " exit_100  INPUT
*&---------------------------------------------------------------------*
*&      Module  MODEL_DESC_FIND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE model_desc_find INPUT.

  SELECT SINGLE * FROM  ztpp_veh_model
   WHERE  model =  is219-model.
  IF sy-subrc NE  0.
    MESSAGE e000  WITH 'It is model code that is not registered !'.
  ENDIF.

ENDMODULE.                 " MODEL_DESC_FIND  INPUT
*&---------------------------------------------------------------------*
*&      Module  COLUMN_VALUE_FIND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE column_value_find INPUT.
  perform 219_column_value.


ENDMODULE.                 " COLUMN_VALUE_FIND  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN  'PCOL'.
      PERFORM  PREV_column_select.
    WHEN  'NCOL'.
      PERFORM  next_column_select.
    WHEN  'ENTER'.
      PERFORM  comlumn_value_select.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
