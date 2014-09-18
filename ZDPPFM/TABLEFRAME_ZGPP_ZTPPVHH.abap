*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_ZTPPVHH
*   generation date: 12/04/2003 at 18:03:00 by user EZLOVE
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_ZTPPVHH       .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
