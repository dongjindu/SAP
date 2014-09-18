FUNCTION Z_FSD_MODULE_COMP .
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      T_MODULE_COMP STRUCTURE  ZSSD_MODULE_BOM
*"  EXCEPTIONS
*"      NO_DATA
*"----------------------------------------------------------------------
DATA: T_COMP  LIKE ZTSD_MODULE_COMP OCCURS 0 WITH HEADER LINE.
DATA: L_LINE  TYPE I.
T_COMP[] = T_MODULE_COMP[].

* CHECK THE DATA
  DESCRIBE TABLE T_COMP[] LINES L_LINE.
  IF L_LINE = 0.
    RAISE NO_DATA.
  ENDIF.

  MODIFY ZTSD_MODULE_COMP FROM TABLE T_COMP.

  IF sy-subrc EQ 0.
    COMMIT WORK AND WAIT.
    T_MODULE_COMP-ZRESULT = 'S'.
    MODIFY T_MODULE_COMP TRANSPORTING ZRESULT
                             WHERE ZSORC GE space
                               AND ZMODL GE SPACE
                               AND MATNR GE SPACE.
  ELSE.
    ROLLBACK WORK.
    T_MODULE_COMP-ZRESULT = 'E'.
    MODIFY T_MODULE_COMP TRANSPORTING ZRESULT
                             WHERE ZSORC GE space
                               AND ZMODL GE SPACE
                               AND MATNR GE SPACE.

  ENDIF.





ENDFUNCTION.
