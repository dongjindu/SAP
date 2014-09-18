*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_RESB
*   generation date: 01/22/2008 at 10:33:29 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_RESB          .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
