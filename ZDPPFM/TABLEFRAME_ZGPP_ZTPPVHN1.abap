*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_ZTPPVHN1
*   generation date: 12/04/2003 at 00:51:37 by user EZLOVE
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_ZTPPVHN1      .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
