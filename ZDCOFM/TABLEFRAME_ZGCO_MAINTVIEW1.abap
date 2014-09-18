*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGCO_MAINTVIEW1
*   generation date: 08-05-2003 at 20:51:18 by user HJYOUN
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGCO_MAINTVIEW1    .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
