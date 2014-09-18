*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZFTA_DUTY
*   generation date: 06/01/2012 at 08:44:45 by user HIS20094
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZFTA_DUTY          .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
