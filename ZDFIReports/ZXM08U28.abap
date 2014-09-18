*----------------------------------------------------------------------*
*   INCLUDE ZXM08U28                                                   *
*----------------------------------------------------------------------*
*ANDY
*"  IMPORTING
*"     VALUE(IS_RBKPV) TYPE  MRM_RBKPV
*"     VALUE(IS_FRSEG) TYPE  MMCR_FRSEG
*"  EXPORTING
*"     VALUE(ES_FRSEG_CHANGE) TYPE  MMCR_FRSEG
*"     VALUE(EF_CHANGE) TYPE  C

*TABLES: ztfi_ctl.
DATA: l_mwskz LIKE ekpo-mwskz.

SELECT COUNT( * ) INTO sy-index FROM ztfi_ctl
   WHERE bukrs = is_rbkpv-bukrs
     AND categ = 'ZXM08U28'.
CHECK sy-subrc = 0.

* overwrite tax code from current PO
SELECT SINGLE mwskz INTO l_mwskz FROM ekpo
  WHERE ebeln = is_frseg-ebeln
    AND ebelp = is_frseg-ebelp.
IF l_mwskz <> is_frseg-mwskz.
  es_frseg_change = is_frseg.
  es_frseg_change-mwskz = l_mwskz.
  ef_change = 'X'.
ENDIF.
