*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGMM_JISGR
*   generation date: 02/01/2013 at 12:42:08 by user HIS20037
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGMM_JISGR         .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
