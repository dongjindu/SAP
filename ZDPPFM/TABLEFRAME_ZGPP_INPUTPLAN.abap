*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_INPUTPLAN
*   generation date: 06/20/2011 at 10:11:29 by user HIS20037
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_INPUTPLAN     .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
