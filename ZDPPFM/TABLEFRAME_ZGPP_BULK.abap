*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_BULK
*   generation date: 08/07/2013 at 08:56:03 by user T00306
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_BULK          .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
