*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZTMM_VEND_LIST
*   generation date: 04/30/2007 at 10:36:14 by user P00181
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZTMM_VEND_LIST     .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
