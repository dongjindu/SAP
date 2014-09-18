*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGEB_GENERAL
*   generation date: 02/27/2007 at 14:55:14 by user P00181
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGEB_GENERAL       .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
