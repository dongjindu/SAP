*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZTCO_SHOP
*   generation date: 09/13/2006 at 12:55:19 by user T00137
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZTCO_SHOP          .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
