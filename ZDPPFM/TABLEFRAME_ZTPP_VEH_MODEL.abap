*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZTPP_VEH_MODEL
*   generation date: 01/26/2004 at 22:52:29 by user SOFTNT
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZTPP_VEH_MODEL     .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
