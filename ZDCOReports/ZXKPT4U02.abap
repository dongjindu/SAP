*----------------------------------------------------------------------*
*   INCLUDE ZXKPT4U02                                                  *
*----------------------------------------------------------------------*
*"       IMPORTING
*"             VALUE(IS_PARX) LIKE  RKPLV_PARX STRUCTURE  RKPLV_PARX
*"             VALUE(ID_CURSW_P) LIKE  RKPLV_FLGX-FLG_CURSW_P
*"             VALUE(ID_PQPLAN) TYPE  C
*"       TABLES
*"              CT_RKPLV2 STRUCTURE  RKPLV2


  loop at ct_rkplv2 WHERE LSTAR = 'MAN_HR' or LSTAR = 'MCH_HR'.
    clear: ct_rkplv2-LST001.
    clear: ct_rkplv2-LST002.
    clear: ct_rkplv2-LST003.
    clear: ct_rkplv2-LST004.
    clear: ct_rkplv2-LST005.
    clear: ct_rkplv2-LST006.
    clear: ct_rkplv2-LST007.
    clear: ct_rkplv2-LST008.
    clear: ct_rkplv2-LST009.
    clear: ct_rkplv2-LST010.
    clear: ct_rkplv2-LST011.
    clear: ct_rkplv2-LST012.

    modify ct_rkplv2.
  endloop.
