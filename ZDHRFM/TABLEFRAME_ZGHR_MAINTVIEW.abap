*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGHR_MAINTVIEW
*   generation date: 12/04/2013 at 16:12:02 by user T00306
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGHR_MAINTVIEW     .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
