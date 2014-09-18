*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZG_VM_RPID
*   generation date: 09/14/2010 at 15:06:44 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZG_VM_RPID         .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
