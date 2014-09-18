*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGMM_ZTBKPF
*   generation date: 10/13/2010 at 16:52:26 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGMM_ZTBKPF        .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
