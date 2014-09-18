*----------------------------------------------------------------------*
*   INCLUDE ZXM08U16                                                   *
*----------------------------------------------------------------------*
*"  IMPORTING
*"     VALUE(E_TRBKPV) TYPE  MRM_RBKPV
*"  TABLES
*"      E_TDRSEG TYPE  MMCR_TDRSEG
*DATA:    BEGIN OF xnast OCCURS 50.     " Tabelle der Nachrichten (akt.)
*        INCLUDE STRUCTURE vnast.
*DATA:    END OF xnast.
*
*
*REFRESH xnast.
*
*LOOP AT e_tdrseg.
*
*  IF e_tdrseg-mwskz = 'U0'.
*    xnast-KAPPL = 'FI'.
*    append xnast.
*
*    CALL FUNCTION 'RV_MESSAGES_INSERT'
*         TABLES
*              tab_xnast = xnast.
*  ENDIF.
*ENDLOOP.
