*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGMM_MV_GROUP
*   generation date: 04/29/2004 at 08:21:19 by user STLIMSIS
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGMM_MV_GROUP      .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
