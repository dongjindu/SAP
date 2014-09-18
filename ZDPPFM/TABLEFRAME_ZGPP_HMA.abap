*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_HMA
*   generation date: 02/03/2012 at 08:18:29 by user HIS20088
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_HMA           .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
