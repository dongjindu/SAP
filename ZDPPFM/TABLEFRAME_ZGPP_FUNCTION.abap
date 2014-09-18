*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_FUNCTION
*   generation date: 01/30/2004 at 18:04:21 by user BOBBY
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_FUNCTION      .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
