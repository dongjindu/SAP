FUNCTION Z_CO_READ_ML_MATERIALS.
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

  RANGES: r_bwkey FOR ckmlhd-bwkey,
          r_matnr FOR ckmlhd-matnr.
  TABLES: t001k.
  DATA:   l_idx    LIKE sy-tabix.
  DATA: BEGIN OF lt_ckmlcr OCCURS 500,
          kalnr   LIKE ckmlcr-kalnr,
          peinh   LIKE ckmlcr-peinh,
          vprsv   LIKE ckmlcr-vprsv,
          stprs   LIKE ckmlcr-stprs,  "STD
          pvprs   LIKE ckmlcr-pvprs,  "MAP
          abprd_o LIKE ckmlcr-abprd_o, "price diff
          abkdm_o LIKE ckmlcr-abkdm_o,
          zuprd_o LIKE ckmlcr-zuprd_o,
          zukdm_o LIKE ckmlcr-zukdm_o,

          abkumo  LIKE ckmlpp-abkumo, "begin inv.
          zukumo  LIKE ckmlpp-zukumo, "GR
        END OF lt_ckmlcr.

* select materials
  r_bwkey-sign = 'I'. r_bwkey-option = 'EQ'.
  SELECT * FROM t001k WHERE bukrs = p_kokrs.
    r_bwkey-low = t001k-bwkey. APPEND r_bwkey.
  ENDSELECT.

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

* MARA + MBEW(current period) = MACKU, MARC
  DATA: l_abrechdat LIKE ckmlhd-abrechdat.

  IF p_ml = 'X'.
    SELECT  ckmlmv011~matnr ckmlmv011~bwkey AS werks
            ckmlmv011~kalnr AS kaln1 bklas ckmlmv011~mtart
ckmlmv011~matkl meins stawn
            ckmlrunperiod~gjahr as LFGJA ckmlrunperiod~poper as LFMON
     INTO CORRESPONDING FIELDS OF TABLE  it_mat
             FROM ( ckmlmv011 INNER JOIN mara
                            ON ckmlmv011~matnr = mara~matnr
                         INNER JOIN marc
                            ON ckmlmv011~matnr = marc~matnr
                           AND ckmlmv011~bwkey = marc~werks
                         INNER JOIN ckmlrunperiod
                            ON ckmlmv011~laufid = ckmlrunperiod~run_id
                         INNER JOIN ckmlhd
                            ON ckmlmv011~kalnr = ckmlhd~kalnr )
             WHERE ckmlrunperiod~gjahr = p_bdatj
               AND ckmlrunperiod~poper = p_poper
               AND ckmlmv011~bwkey IN r_bwkey
               AND ckmlhd~abrechdat <> l_abrechdat
               AND ckmlmv011~matnr IN r_matnr.
  ELSE.
    SELECT  ckmlhd~matnr ckmlhd~bwkey AS werks
            ckmlhd~kalnr AS kaln1 bklas mara~mtart mara~matkl meins
stawn
            mbew~LFGJA mbew~LFMON
     INTO CORRESPONDING FIELDS OF TABLE  it_mat
             FROM ( ckmlhd INNER JOIN mara
                            ON mara~matnr = ckmlhd~matnr
                         INNER JOIN marc
                            ON marc~matnr = ckmlhd~matnr
                           AND marc~werks = ckmlhd~bwkey
                         INNER JOIN mbew
                            ON mbew~KALN1 = ckmlhd~KALNR )
             WHERE ckmlhd~matnr IN r_matnr
               AND ckmlhd~bwkey IN r_bwkey
               AND ckmlhd~abrechdat <> l_abrechdat.

  ENDIF.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_ckmlcr
    FROM ckmlcr INNER JOIN ckmlpp
                   ON ckmlcr~kalnr  = ckmlpp~kalnr
                  AND ckmlcr~bdatj  = ckmlpp~bdatj
                  AND ckmlcr~poper  = ckmlpp~poper
                  AND ckmlcr~untper = ckmlpp~untper
    FOR ALL entries IN it_mat
    WHERE ckmlcr~kalnr  = it_mat-kaln1
      AND ckmlcr~bdatj  = p_bdatj
      AND ckmlcr~poper  = p_poper
      AND ckmlcr~untper = space
      AND ckmlcr~curtp  = '10'.

  SORT lt_ckmlcr BY kalnr.
*-update material info
  LOOP AT it_mat.
    l_idx = sy-tabix.

    CLEAR lt_ckmlcr.
    READ TABLE lt_ckmlcr WITH KEY kalnr = it_mat-kaln1 BINARY SEARCH.
    it_mat-peinh = lt_ckmlcr-peinh.
    it_mat-stprs = lt_ckmlcr-stprs.
    it_mat-verpr = lt_ckmlcr-pvprs.

    MODIFY it_mat INDEX l_idx.

  ENDLOOP.

ENDFUNCTION.
