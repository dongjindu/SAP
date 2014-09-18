*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_MAPPING
*   generation date: 03/17/2004 at 18:01:46 by user JOKIM
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_MAPPING       .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
