*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZTIMIMG23
*   generation date: 2002.09.30 at 11:10:57 by user ABAP0035
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZTIMIMG23          .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
