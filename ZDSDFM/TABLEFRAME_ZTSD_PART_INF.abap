*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZTSD_PART_INF
*   generation date: 11/03/2006 at 09:33:14 by user P00181
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZTSD_PART_INF      .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
