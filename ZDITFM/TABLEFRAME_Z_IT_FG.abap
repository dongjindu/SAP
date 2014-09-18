*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_Z_IT_FG
*   generation date: 09/17/2013 at 08:50:33 by user HIS20029
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_Z_IT_FG            .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
