*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_Z_BW_002
*   generation date: 02/02/2012 at 15:55:03 by user HIS20018
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_Z_BW_002           .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
