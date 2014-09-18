*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZTSD_ACM_H
*   generation date: 04/05/2006 at 13:14:28 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZTSD_ACM_H         .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
