*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZTCO_UNITUSER
*   generation date: 05/26/2011 at 15:47:48 by user HIS20094
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZTCO_UNITUSER      .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
