*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_ZTPPPB
*   generation date: 04/06/2004 at 18:56:39 by user JOKIM
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_ZTPPPB        .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
