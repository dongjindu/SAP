*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGMM_KD_ASN
*   generation date: 07/17/2008 at 08:15:37 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGMM_KD_ASN        .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
