FUNCTION Z_CO_READ_ML_MATERIALS_ML.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(P_KOKRS) LIKE  T001K-BUKRS
*"     REFERENCE(P_BDATJ) TYPE  CKMLRUNPERIOD-GJAHR
*"     REFERENCE(P_POPER) TYPE  CKMLRUNPERIOD-POPER
*"     REFERENCE(P_ML) TYPE  CHAR01 DEFAULT 'X'
*"  TABLES
*"      IT_MAT STRUCTURE  ZSCO_IT_MAT
*"      IR_MATNR STRUCTURE  ZSCO_RANGES OPTIONAL
*"----------------------------------------------------------------------
  DATA : it_ckmlkeph LIKE ckmlkeph OCCURS 0 WITH HEADER LINE.
  DATA : BEGIN OF it_ckmlmv001 OCCURS 0,
          kalnr      LIKE ckmlmv001-kalnr,
          proc_kalnr LIKE ckmlmv001-proc_kalnr,  "proc.process
          pmatn_nd   LIKE ckmlmv001-pmatn_nd,
          verid_nd   LIKE ckmlmv001-verid_nd,
         END OF it_ckmlmv001.
  DATA: ld_wa_db_price LIKE ckmllacr,
        ld_wa_db_keph  LIKE ckmlprkeph,
        ld_wa_db_keko  LIKE ckmlprkeko.

  RANGES: r_bwkey FOR ckmlhd-bwkey,
          r_matnr FOR ckmlhd-matnr.
  DATA:   l_idx    LIKE sy-tabix.
  DATA: l_elehk type CK_ELESMHK.
  DATA : it_tckh3      LIKE tckh3         OCCURS 0 WITH HEADER LINE.
  DATA: lt_cc_amt LIKE gt_cc_amt OCCURS 0 WITH HEADER LINE.


  REFRESH it_tckh3.
  select single elehk into l_elehk from tck07 where bukrs = p_kokrs.
  SELECT * INTO TABLE it_tckh3
      FROM tckh3
        WHERE elehk = l_elehk.
  check sy-subrc = 0.

* select materials
  LOOP AT ir_matnr.
    r_matnr-sign   = ir_matnr-sign.
    r_matnr-option = ir_matnr-option.
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
         EXPORTING
              input        = ir_matnr-low
         IMPORTING
              output       = r_matnr-low
         EXCEPTIONS
              length_error = 1
              OTHERS       = 2.
    IF sy-subrc <> 0.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
         EXPORTING
              input        = ir_matnr-high
         IMPORTING
              output       = r_matnr-high
         EXCEPTIONS
              length_error = 1
              OTHERS       = 2.
    IF sy-subrc <> 0.
    ENDIF.
    APPEND r_matnr.
  ENDLOOP.
* ensure cost est. no.
  loop at it_mat where kaln1 = space.
    SELECT single kalnr INTO it_mat-kaln1
      FROM ckmlhd
     WHERE matnr = it_mat-matnr.
    modify it_mat transporting kaln1.
  endloop.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE  it_ckmlkeph
    FROM ckmlkeph
     FOR ALL ENTRIES IN it_ckmlmv001
   WHERE kalnr = it_ckmlmv001-kalnr
     AND bdatj = p_bdatj
     AND poper = p_poper
     AND untper = ''
     AND categ = 'ZU'    "receipt
     AND ptyp  = 'BF'    "production
     AND mlcct = 'E'
     AND bvalt = it_ckmlmv001-kalnr
     AND kkzst = '' .    "sum
  SORT it_ckmlkeph BY kalnr categ mlcct bvalt .

  data: l_amt type dmbtr.

  LOOP AT it_ckmlmv001.
*-- FIXME ; production version...
    READ TABLE it_mat WITH KEY matnr = it_ckmlmv001-pmatn_nd.
    l_idx = sy-tabix.

    CLEAR it_ckmlkeph.
    READ TABLE it_ckmlkeph  WITH KEY
               kalnr = it_mat-kaln1
               categ = 'ZU'
               mlcct = 'E'
               bvalt = it_ckmlmv001-kalnr
         BINARY SEARCH.

    CHECK sy-subrc = 0 .
    MOVE-CORRESPONDING it_ckmlkeph TO ld_wa_db_keph .

    PERFORM extract_cc_amt TABLES lt_cc_amt
                                  it_tckh3
                           USING  ld_wa_db_keph.
    CLEAR: l_amt.
    LOOP AT lt_cc_amt.
      l_amt = l_amt + lt_cc_amt-dmbtr.
    endloop.

    MODIFY it_mat INDEX l_idx TRANSPORTING verpr.

  ENDLOOP.


ENDFUNCTION.
