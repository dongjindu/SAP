*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGQM_VEND_PORTAL
*   generation date: 11/29/2012 at 14:43:20 by user HIS20037
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGQM_VEND_PORTAL   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
