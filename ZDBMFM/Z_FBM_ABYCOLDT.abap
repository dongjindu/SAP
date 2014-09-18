FUNCTION Z_FBM_ABYCOLDT.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  EXPORTING
*"     VALUE(Z_RESULT) LIKE  BAPIRET2-TYPE
*"  TABLES
*"      T_ABYCOLDT STRUCTURE  ZTBM_ABYCOLDT
* Created by Haseeb Mohammad on request of Daniel Kim.
*"UD1K944388
*"----------------------------------------------------------------------

DATA : WA_DATA LIKE ZTBM_ABYCOLDT.
DATA: it_abycoldt TYPE ZTBM_ABYCOLDT OCCURS 0 WITH HEADER LINE.

  LOOP AT T_ABYCOLDT.
    MOVE-CORRESPONDING T_ABYCOLDT TO it_abycoldt.
    APPEND it_abycoldt.
  ENDLOOP.

MODIFY ZTBM_ABYCOLDT FROM TABLE it_abycoldt.
*MODIFY ZTBM_ABYCOLDT FROM T_ABYCOLDT.

 IF SY-SUBRC NE 0.
    Z_RESULT = 'E'.
 ELSE.
    COMMIT WORK.
    Z_RESULT = 'S'.

 ENDIF.





ENDFUNCTION.
