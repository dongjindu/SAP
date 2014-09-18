*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZTIEPORT
*   generation date: 2003.06.16 at 12:02:20 by user INFOLINK7
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZTIEPORT           .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
