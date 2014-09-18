*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGHR_MAINTVIEW02
*   generation date: 06/19/2012 at 09:43:07 by user T00281
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGHR_MAINTVIEW02   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
