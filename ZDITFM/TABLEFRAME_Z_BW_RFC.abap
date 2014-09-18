*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_Z_BW_RFC
*   generation date: 01/15/2014 at 15:24:23 by user HIS20036
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_Z_BW_RFC           .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
