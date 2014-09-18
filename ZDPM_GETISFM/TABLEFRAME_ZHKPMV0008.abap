*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZHKPMV0008
*   generation date: 07/01/2014 at 02:04:50 by user HMC_DEV
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZHKPMV0008         .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
