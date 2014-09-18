*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_ZTPPVHP
*   generation date: 12/04/2003 at 17:16:32 by user BOBBY
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_ZTPPVHP       .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
