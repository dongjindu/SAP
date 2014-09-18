*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZSD_CAM
*   generation date: 03/10/2005 at 11:14:54 by user 100701
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZSD_CAM            .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
