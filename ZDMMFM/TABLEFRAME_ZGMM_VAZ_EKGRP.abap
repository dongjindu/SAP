*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGMM_VAZ_EKGRP
*   generation date: 08/18/2011 at 13:11:56 by user HIS20037
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGMM_VAZ_EKGRP     .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
