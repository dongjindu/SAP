*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGPP_VEHICLE_WRA
*   generation date: 10/18/2011 at 13:40:21 by user HIS20037
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGPP_VEHICLE_WRA   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
