*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZTIMIMGTX
*   generation date: 2001/10/27 at 15:02:43 by user ESBKANG
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZTIMIMGTX          .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
