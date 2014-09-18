*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGCO_MAINTVIEW4
*   generation date: 06/13/2005 at 15:39:04 by user 100701
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGCO_MAINTVIEW4    .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
