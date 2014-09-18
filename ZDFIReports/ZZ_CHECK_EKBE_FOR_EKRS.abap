REPORT zm_check_ekbe_for_ekrs.

*---------------------------------------------------------------------*
* select options:
* SO_BELNR = MM invoice document
* SO_GJAHR = Document year
* SO_BUDAT = Posting date of MM invoice document
* SO_BUKRS = Company Code of Purchase Order
*---------------------------------------------------------------------*

TABLES: ekko, ekpo, rbkp, rseg, ekrs.

DATA: tab_rbkp LIKE rbkp OCCURS 10,
      s_rbkp LIKE rbkp.

DATA: tab_rseg LIKE rseg OCCURS 10,
      s_rseg LIKE rseg.

DATA: BEGIN OF tab_check_ebeln OCCURS 10,
      ebeln LIKE ekpo-ebeln,
      ebelp LIKE ekpo-ebelp.
DATA: END OF tab_check_ebeln.

DATA: tab_ekko LIKE ekko OCCURS 10 WITH HEADER LINE,
      s_ekko LIKE ekko.

DATA: tab_ekpo LIKE ekpo OCCURS 10,
      s_ekpo LIKE ekpo.

DATA: tab_ek08rn LIKE ek08rn OCCURS 10.
DATA: s_ek08rn LIKE ek08rn.

DATA: BEGIN OF tab_hist_error OCCURS 10,
       ebeln LIKE ekko-ebeln,
       ebelp LIKE ekpo-ebelp,
       lfgja LIKE ek08rn-lfgja,
       lfbnr LIKE ek08rn-lfbnr,
       lfpos LIKE ek08rn-lfpos,
       wemng LIKE ek08rn-wemng,
       remng LIKE ek08rn-remng,
       wewrt LIKE ek08rn-wewrt,
       arewr LIKE ek08rn-arewr.
DATA: END OF tab_hist_error.

DATA: tab_ekbe LIKE ekbe OCCURS 0 WITH HEADER LINE,
      tab_ekrs LIKE ekrs OCCURS 0 WITH HEADER LINE.

DATA: f_lines TYPE i,
      f_diff_menge LIKE ek08rn-remng,
      f_diff_value LIKE ek08rn-wewrt,
      f_diff_theoretical_value LIKE ek08rn-wewrt,
      f_quot_we TYPE p DECIMALS 2,
      f_quot_re TYPE p DECIMALS 2,
      f_quot_compare TYPE p DECIMALS 2,
      f_quot TYPE p DECIMALS 2 VALUE 5,
      f_add_check TYPE boole-boole.

SELECT-OPTIONS: so_belnr FOR rbkp-belnr,
                so_gjahr FOR rbkp-gjahr,
                so_cpudt FOR rbkp-cpudt,
                so_bukrs FOR ekko-bukrs.
PARAMETERS: p_test TYPE xfeld DEFAULT 'X'.
* ---- selection ------------------------------------------------------*

SELECT * FROM  rbkp  INTO TABLE tab_rbkp
       WHERE  belnr  IN so_belnr
       AND    gjahr  IN so_gjahr
       AND    cpudt  IN so_cpudt.

IF sy-subrc NE 0.
  WRITE: 'No RBKP entries found'.
  EXIT.
ENDIF.

SELECT * FROM rseg INTO TABLE tab_rseg
                   FOR ALL ENTRIES IN tab_rbkp
       WHERE  belnr  = tab_rbkp-belnr
       AND    gjahr  = tab_rbkp-gjahr.

IF sy-subrc NE 0.
  WRITE: 'No RSEG entries found'.
  EXIT.
ENDIF.

* ---- fill tab_check_ebeln -------------------------------------------*
LOOP AT tab_rseg INTO s_rseg.
  tab_check_ebeln-ebeln = s_rseg-ebeln.
  tab_check_ebeln-ebelp = s_rseg-ebelp.
  COLLECT tab_check_ebeln.
ENDLOOP.

SELECT * FROM ekko INTO TABLE tab_ekko
                   FOR ALL ENTRIES IN tab_check_ebeln
       WHERE  ebeln  = tab_check_ebeln-ebeln
       AND    bukrs  IN so_bukrs.

IF sy-subrc NE 0.
  WRITE: 'No EKKO entries found'.
  EXIT.
ENDIF.

SELECT * FROM ekpo INTO TABLE tab_ekpo
         FOR ALL ENTRIES IN tab_check_ebeln
         WHERE  ebeln  = tab_check_ebeln-ebeln
         AND    ebelp  = tab_check_ebeln-ebelp
         AND    bukrs  IN so_bukrs
         AND    pstyp  NE 9
         AND    repos NE space
         AND    fplnr = space
         AND    webre = 'X'
         AND    xersy = 'X'.

IF sy-subrc NE 0.
  WRITE: 'No EKPO entries found'.
  EXIT.
ENDIF.

* ---- analyzation ----------------------------------------------------*

LOOP AT tab_ekpo INTO s_ekpo.
  CHECK NOT s_ekpo-wepos IS INITIAL.
  CHECK s_ekpo-weunb IS INITIAL.
  READ TABLE tab_ekko INTO s_ekko WITH KEY ebeln = s_ekpo-ebeln.
  CALL FUNCTION 'ME_READ_ITEM_INVOICE'
       EXPORTING
            display        = 'X'
            ebelp          = s_ekpo-ebelp
            iekko          = s_ekko
            re_kursf       = s_ekko-wkurs
            re_waers       = s_ekko-waers
            re_wwert       = sy-datum
       TABLES
            xek08rn        = tab_ek08rn
       EXCEPTIONS
            not_found_any  = 1
            not_found_one  = 2
            not_valid_any  = 3
            not_valid_one  = 4
            enqueue_failed = 5
            OTHERS         = 6.
  IF sy-subrc <> 0.
    CONTINUE.
  ENDIF.

  LOOP AT tab_ek08rn INTO s_ek08rn.
    PERFORM check_differences.
  ENDLOOP.
ENDLOOP.


*-------- List of wrong PO histories ---------------------------------*
CLEAR f_lines.
DESCRIBE TABLE tab_hist_error LINES f_lines.
WRITE: /  'PO history with difference for qty in GRs and Invoices'.
ULINE .
IF f_lines EQ 0.
  WRITE: / 'No PO items with a wrong PO history found' .
ELSE.
  PERFORM write_header.
  REFRESH tab_ekrs.
  LOOP AT tab_hist_error.
    AT NEW ebelp.
      REFRESH tab_ekbe.
      CALL FUNCTION 'ME_READ_HISTORY'
           EXPORTING
                ebeln = tab_hist_error-ebeln
                ebelp = tab_hist_error-ebelp
                webre = 'X'
           TABLES
                xekbe = tab_ekbe.
    ENDAT.
    WRITE:  /1  tab_hist_error-ebeln ,
             11  tab_hist_error-ebelp ,
             17  tab_hist_error-lfbnr ,
             28  tab_hist_error-wemng ,
             43  tab_hist_error-remng ,
             58  tab_hist_error-wewrt ,
             73  tab_hist_error-arewr.
*   Preparation of EKRS entries:
    LOOP AT tab_ekbe WHERE ebeln = tab_hist_error-ebeln
                     AND   ebelp = tab_hist_error-ebelp
                     AND   vgabe = '1'
                     AND   lfgja = tab_hist_error-lfgja
                     AND   lfbnr = tab_hist_error-lfbnr
                     AND   lfpos = tab_hist_error-lfpos.
      CLEAR tab_ekrs.
      MOVE-CORRESPONDING tab_ekbe TO tab_ekrs.
      IF tab_ekbe-zekkn NE 0  AND  tab_ekbe-packno IS INITIAL.
        CLEAR tab_ekrs-lfpos.
      ENDIF.
      READ TABLE tab_ekko WITH KEY ebeln = tab_hist_error-ebeln.
      IF sy-subrc = 0.
        tab_ekrs-lifnr = tab_ekko-lifnr.
        tab_ekrs-bukrs = tab_ekko-bukrs.
      ENDIF.
*     Check against existence
      SELECT SINGLE * FROM ekrs WHERE budat = tab_ekrs-budat
                                AND   lifnr = tab_ekrs-lifnr
                                AND   belnr = tab_ekrs-belnr
                                AND   buzei = tab_ekrs-buzei
                                AND   gjahr = tab_ekrs-gjahr.
      IF sy-subrc <> 0.
        APPEND tab_ekrs.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
* Creation of EKRS entries
  IF p_test IS INITIAL.
    CALL FUNCTION 'ME_UPDATE_EKRS'
         EXPORTING
              i_insert = 'X'
              i_delete = ' '
         TABLES
              t_ekrs   = tab_ekrs.
    REFRESH tab_ekrs.
  ENDIF.
ENDIF.
ULINE.
WRITE:  / , / , / .
ULINE.

*---------------------------------------------------------------------*

FORM check_differences.

*------ calculate differences
  f_diff_menge = s_ek08rn-wemng - s_ek08rn-remng.
*------ check and fill error_tables---------------------------------*
  CHECK f_diff_menge <> 0.
  IF f_diff_menge LT 0 OR f_diff_menge GT 0.
    tab_hist_error-ebeln = s_ekpo-ebeln.
    tab_hist_error-ebelp = s_ekpo-ebelp.
    tab_hist_error-lfgja = s_ek08rn-lfgja.
    tab_hist_error-lfbnr = s_ek08rn-lfbnr.
    tab_hist_error-lfpos = s_ek08rn-lfpos.
    tab_hist_error-wemng = s_ek08rn-wemng.
    tab_hist_error-remng = s_ek08rn-remng.
    tab_hist_error-wewrt = s_ek08rn-wewrt.
    tab_hist_error-arewr = s_ek08rn-arewr.
    APPEND tab_hist_error.
  ENDIF.
ENDFORM.                        "check_differences.

*---------------------------------------------------------------------*
*       FORM write_header                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM write_header.

  WRITE:  /1   'PO number' ,
           11  'item' ,
           17  'Material doc' ,
           30  'Delivered qty' ,
           46  'Invoiced qty' ,
           61  'Deliv. value' ,
           74  'Invoices value' .

  WRITE:  /1   'EBELN' ,
           11  'EBELP' ,
           17  'LFBNR' ,
           30  'EK08RN-WEMNG' ,
           46  'EK08RN-REMNG' ,
           61  'EK08RN-WEWRT' ,
           74  'EK08RN-AREWR' .
ENDFORM.                        "write_header.
