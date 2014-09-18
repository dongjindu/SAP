*{TC Begin} generation http://intranet.sap.com/materialversion
*Do not change coding between begin and end comments. KA5 20010508

MODULE MGV_D4002 OUTPUT.
  CALL FUNCTION 'MATNR_TC_COLWIDTH'
       EXPORTING
            repid         = 'SAPML01S'
            tcname        = 'D4002'
       CHANGING
            table_control = D4002.
ENDMODULE.


MODULE MGV_D4003 OUTPUT.
  CALL FUNCTION 'MATNR_TC_COLWIDTH'
       EXPORTING
            repid         = 'SAPML01S'
            tcname        = 'D4003'
       CHANGING
            table_control = D4003.
ENDMODULE.

*{TC End} generation
