*&---------------------------------------------------------------------*
*& Report  ZDI_BF_MD11
*&  - Create Planned Order
*&---------------------------------------------------------------------*
* THIS IS FOR TESTING PURPOSE
*   - spec by Andy Choi
*============
REPORT  zdi_bf_md11 MESSAGE-ID ppc1pr.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-sl1.
PARAMETERS:
  p_kdauf LIKE plaf-kdauf MEMORY ID aun,
  p_kdpos LIKE plaf-kdpos,
  p_matnr LIKE plaf-matnr MEMORY ID mat,
  p_plwrk LIKE plaf-plwrk MEMORY ID wrk,
  p_psttr LIKE plaf-psttr DEFAULT sy-datum.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-sl2.
PARAMETERS:
  p_plscn LIKE plaf-plscn. " DEFAULT '900'.
SELECTION-SCREEN END OF BLOCK b3.

PARAMETERS:
  p_verid LIKE plaf-verid,
  p_lgort TYPE lgort_d MEMORY ID lag,
  p_mcomp TYPE bapco.  " '1'.

DATA: l_return1  LIKE  bapiret1.
DATA: l_return2  LIKE  bapiret2.
DATA: l_plnum    LIKE plaf-plnum,
      l_lgort    TYPE lgort_d.
DATA: l_bapiplaf LIKE bapiplaf_i1.

START-OF-SELECTION.

  IF NOT p_kdauf IS INITIAL.
    SELECT SINGLE matnr werks INTO (p_matnr, p_plwrk)
        FROM vbap
        WHERE vbeln = p_kdauf
          AND posnr = p_kdpos.
  ENDIF.

*-individual customer order
  l_bapiplaf-pldord_profile    = 'KD'.
  l_bapiplaf-material          = p_matnr.
  l_bapiplaf-plan_plant        = p_plwrk.

* select first version from valid versions
  IF p_verid IS INITIAL.
    SELECT SINGLE verid alort INTO (p_verid, l_lgort)
       FROM mkal
       WHERE matnr = p_matnr
         AND werks = p_plwrk
         AND serkz = 'X'      "repetitive manuf.
         AND mdv01 <> space   "production line
         AND adatu <= p_psttr
         AND bdatu >= p_psttr.
  ENDIF.
  l_bapiplaf-version           = p_verid.
  IF p_lgort IS INITIAL.
    p_lgort = l_lgort.
  ENDIF.
  l_bapiplaf-plng_scenario_lt  = p_plscn.

  l_bapiplaf-total_plord_qty   = 1.

*  IF p_psttr < sy-datum.
*    l_bapiplaf-order_fin_date  = sy-datum.
* ELSE.
  l_bapiplaf-order_fin_date  = p_psttr.
* ENDIF.

  l_bapiplaf-firming_ind       = 'X'.

*-"'1' for manual component
  l_bapiplaf-manual_component  = p_mcomp.

*-account assignment - customer stock
  l_bapiplaf-acctasscat       = 'M'.

*-sales order for variant configuration
  l_bapiplaf-sales_ord         = p_kdauf.
  l_bapiplaf-s_ord_item        = p_kdpos.

*   CALL BAPI
  CALL FUNCTION 'BAPI_PLANNEDORDER_CREATE'
    EXPORTING
      headerdata   = l_bapiplaf
    IMPORTING
      plannedorder = l_plnum
      return       = l_return1.

*  CALL FUNCTION 'BAPI_PLANNEDORDER_EXIST_CHECK'
*    EXPORTING
*      plannedorder = p_plnum
*    IMPORTING
*      return       = l_return1.
*  IF l_return1-type CA 'AE'.
*  ENDIF.

  IF l_plnum IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    WRITE:/ l_return1-type, l_return1-id, l_return1-number,
            l_return1-message.

  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = ' '.
    WRITE:/ l_plnum, ' is created'.

  ENDIF.

END-OF-SELECTION.
