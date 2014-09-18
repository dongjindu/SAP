*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_Z_FII_MT
*   generation date: 09-14-2003 at 19:15:21 by user ZZANG4YOU
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_Z_FII_MT           .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
