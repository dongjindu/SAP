*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGMM_KD_ASN_MAIN
*   generation date: 07/18/2008 at 09:06:17 by user 101457
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGMM_KD_ASN_MAIN   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
