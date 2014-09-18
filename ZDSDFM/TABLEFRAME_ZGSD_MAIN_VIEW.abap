*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGSD_MAIN_VIEW
*   generation date: 2003.08.08 at 03:12:31 by user JUSTIN8055
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGSD_MAIN_VIEW     .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
