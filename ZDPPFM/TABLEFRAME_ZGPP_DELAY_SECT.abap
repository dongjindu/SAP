*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_DELAY_SECT
*   generation date: 01/09/2013 at 13:36:35 by user HIS20037
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_DELAY_SECT    .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
