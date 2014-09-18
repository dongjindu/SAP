*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGMM_QM_GQMS
*   generation date: 03/26/2013 at 14:21:48 by user HIS20037
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGMM_QM_GQMS       .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
