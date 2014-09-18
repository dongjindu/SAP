*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZGCO_GL_TTYPE
*   generation date: 02/23/2007 at 13:34:47 by user 101679
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZGCO_GL_TTYPE      .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
