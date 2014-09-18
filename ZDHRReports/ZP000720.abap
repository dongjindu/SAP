*----------------------------------------------------------------------*
***INCLUDE ZP000720 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  MODULE_PBO_MMMM  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODULE_PBO_0007 OUTPUT.
*  CALL_PROG = 'MP000700'.
*  PERFORM GET_INFOTYPE_STRUCTURES IN PROGRAM (CALL_PROG) USING PPPAR.
*  PERFORM DYNAMIC_SCREEN_VARIATION(SAPFH5AM) USING  PPPAR-DVARY.

IF sy-pfkey = 'DIS'.
  LOOP AT screen.
    IF screen-name = 'P0007-ZZPAYRULE'.
      screen-input = '0'.
      MODIFY screen.
    ENDIF.
  ENDLOOP.
ENDIF.

ENDMODULE.                 " MODULE_PBO_MMMM  OUTPUT
