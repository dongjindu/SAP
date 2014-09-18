*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGCO_MAINTVIEW3
*   generation date: 10/31/2003 at 02:51:20 by user EHJUNG
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGCO_MAINTVIEW3    .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
