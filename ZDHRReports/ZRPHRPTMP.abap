*----------------------------------------------------------------------*
*   INCLUDE RPHRPTMP                                                   *
*----------------------------------------------------------------------*
* ERP2005
* =======
* SAM850091 06/01/2005  MSC: Payroll reconciliation shows incorrect
*                       values
* LCP
* WVQL9BK029483 09092000 note 323221 (improvements)
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  extract_ru_data
*&---------------------------------------------------------------------*
FORM extract_ru_data USING erd_seqnr LIKE rgdir-seqnr.
  rx-key-pernr = pernr-pernr.
  UNPACK erd_seqnr TO rx-key-seqno.
*  rp-imp-c2-ru.
  CLEAR:
    oru-version,                                "OBJECTS_FOR_CLEAR
    versc,
    wpbp, wpbp[],
    rt, rt[],
    c0, c0[],
    c1, c1[],
    perm,
    tax, tax[],
    taxr, taxr[],
    taxpr, taxpr[],
    bal, bal[],
    unb, unb[],
    xdfrt, xdfrt[].

  IMPORT
    ru-version TO oru-version                   "OBJECTS_FOR_IMPORT
    versc
    wpbp
    rt
    c0
    c1
    perm
    tax
    taxr
    taxpr
    bal
    unb
    xdfrt
  FROM DATABASE pcl2(ru) ID rx-key USING pcl2_exp_imp.
  rp-imp-ru-subrc = sy-subrc.
  ru-version-number = '02'.
ENDFORM.                               " extract_ru_data
*&---------------------------------------------------------------------*
*&      Form  DETERMINE_EE_ER_TAX
*&---------------------------------------------------------------------*
FORM determine_ee_er_tax USING    det_lgart     LIKE t512w-lgart
                         CHANGING det_ee_er_ind TYPE char2. "SAM850091
  DATA: aux_lgart LIKE t512w-lgart.

  MOVE  det_lgart TO aux_lgart.
  CASE aux_lgart.                                         " nt.  0370149
    WHEN '/NG3'.                                                  "
      aux_lgart = '/N03'.                                         "
    WHEN '/NG5'.                                                  "
      aux_lgart = '/N05'.                                         "
  ENDCASE.                                                        "
  MOVE '4' TO aux_lgart+1(1). "The processing class 72 is maintained
  " only for /4* wage types.
* Read PRCL table only for /4xx lgart. i.e lgart = /301 -> read /401
  SELECT SINGLE * FROM t512w WHERE lgart = aux_lgart
                               AND molga = '10'
                               AND endda > p_beg.

  IF sy-subrc = 0.
    CASE t512w-vklas+71(1).
      WHEN '1'.
        det_ee_er_ind = 'EE'.
      WHEN '2'.
        det_ee_er_ind = 'ER'.
      WHEN OTHERS.
        WRITE: / text-e10. STOP.
    ENDCASE.
  ELSE.
* there is no corresponding /4xx lgart in the table. I.e. there is
* an /301 wage type but not the /401 !.
    WRITE: / text-e10. STOP.
  ENDIF.
ENDFORM.                               " DETERMINE_EE_ER_TAX
*&---------------------------------------------------------------------*
*&      Form  build_sort_data_table
*&---------------------------------------------------------------------*
FORM build_sort_data_table TABLES bsdt_data_table STRUCTURE data_table
                                  bsdt_sort_table STRUCTURE data_table.

  DATA: s_string_tmp LIKE bsdt_sort_table-sort_strng.

  LOOP AT bsdt_data_table.
    CLEAR: s_string_tmp, bsdt_sort_table.
    PERFORM sort_options_loop TABLES bsdt_data_table       "  Nt. 360625
                              USING s_string_tmp.                  "
    MOVE-CORRESPONDING bsdt_data_table TO bsdt_sort_table.
    MOVE s_string_tmp TO bsdt_sort_table-sort_strng.
    MOVE s_string_tmp TO bsdt_data_table-sort_strng.
    COLLECT bsdt_sort_table.
    MODIFY bsdt_data_table.
  ENDLOOP.

ENDFORM.                               " build_sort_data_table
*&---------------------------------------------------------------------*
*&      Form  PRINT_SORT_HEADER
*&---------------------------------------------------------------------*
FORM print_sort_header TABLES psh_data_table STRUCTURE data_table_write
                              psh_sort_option STRUCTURE pnpstringt
                        USING psh_inf.
  WRITE:/.

  FORMAT COLOR 1 INTENSIFIED ON.
  ULINE.
  LOOP AT psh_sort_option.
    CASE psh_sort_option-shortstrg.    "Build Sort String
      WHEN 'A'.                        " BUKRS - Company Code
        WRITE:/    psh_sort_option-optiontext,
                25 psh_data_table-bukrs,
               132 ''.
      WHEN 'B'.                        " WERKS - Personnel Area
        WRITE:/    psh_sort_option-optiontext,
                25 psh_data_table-werks,
               132 ''.
      WHEN 'C'.                        " BTRTL - Personnel Subarea
        WRITE:/    psh_sort_option-optiontext,
                25 psh_data_table-btrtl,
               132 ''.
      WHEN 'D'.                        " KOSTL - Cost Center
        WRITE:/    psh_sort_option-optiontext,
                25 psh_data_table-kostl,
               132 ''.
      WHEN 'E'.                        " IABKRS - Payroll Area
        WRITE:/    psh_sort_option-optiontext,
                25 psh_data_table-iabkrs,
               132 ''.
      WHEN 'F'.                        " PERSG - Employee Group
        WRITE:/    psh_sort_option-optiontext,
                25 psh_data_table-persg,
               132 ''.
      WHEN 'G'.                        " PERSK - Employee Subgroup
        WRITE:/    psh_sort_option-optiontext,
                25 psh_data_table-persk,
               132 ''.
      WHEN 'H'.                        " VDSK1 - Organizational Key
        WRITE:/    psh_sort_option-optiontext,
                25 psh_data_table-vdsk1,
               132 ''.
      WHEN 'I'.                        " STAT2 - Employement Status
        CLEAR t529u.
        SELECT SINGLE * FROM t529u WHERE sprsl = sy-langu
                                     AND statn = '2'
                                     AND statv = psh_data_table-stat2.
        WRITE:/    psh_sort_option-optiontext,
                25 t529u-text1,
               132 ''.
      WHEN 'J'.                        " PERNR - Personnel Number
        IF psh_inf = 'X'.

          SELECT SINGLE * FROM pa0001
                          WHERE pernr = psh_data_table-pernr
                            AND begda <= pn-endda   "Note 509312
                            AND endda >= pn-endda.  "Note 509312

          SELECT SINGLE * FROM pa0002
                          WHERE pernr = psh_data_table-pernr
                            AND begda <= pn-endda   "Note 509312
                            AND endda >= pn-endda.  "Note 509312
          WRITE:/  psh_sort_option-optiontext,
                20 psh_data_table-pernr,
                45 pa0001-ename,
               132 space.
        ELSE.
          WRITE:/    psh_sort_option-optiontext,
                  30 psh_data_table-pernr,
                 132 space.
        ENDIF.
      WHEN 'K'.                      " TXCMP - Tax Company
        WRITE:/    psh_sort_option-optiontext,
                25 psh_data_table-txcmp,
               132 ''.
      WHEN 'L'.                      " TAXAU - Tax Authority
        WRITE:/    psh_sort_option-optiontext,
                25 psh_data_table-taxau,
               132 ''.
    ENDCASE.
  ENDLOOP.
  ULINE.
  FORMAT COLOR OFF.
ENDFORM.                             " PRINT_SORT_HEADER
*&---------------------------------------------------------------------*
*&      Form  BUILD_PERIOD_STRING
*&---------------------------------------------------------------------*
FORM build_period_string TABLES   bps_data_table
                                  STRUCTURE data_table_write
                         CHANGING bps_prd_str.
  bps_prd_str = space.
*  IF BPS_DATA_TABLE-FPPER EQ '000000'.      "note304334
  IF bps_data_table-fpper EQ '000000'                       "note304334
  OR bps_data_table-fpper IS INITIAL.                       "note304334
    bps_prd_str(02)   =  bps_data_table-paydt+04(02).
    bps_prd_str+2(01) =  '/'(202).
    bps_prd_str+3(02) =  bps_data_table-paydt+06(02).
    bps_prd_str+5(01) =  '/'(202).
    bps_prd_str+6(04) =  bps_data_table-paydt(04).
    bps_prd_str+11(01) = bps_data_table-payty(01).
    bps_prd_str+12(01) = bps_data_table-payid.
  ELSE.
    bps_prd_str(02)   = bps_data_table-fpper+04(02).
    bps_prd_str+2(01) = '.'(201).
    bps_prd_str+3(02) = bps_data_table-fpper+02(02).
    bps_prd_str+5(01) = '-'(200).
    bps_prd_str+6(02) = bps_data_table-inper+04(02).
    bps_prd_str+8(01) = '.'(201).
    bps_prd_str+9(02) = bps_data_table-inper+02(02).
  ENDIF.
ENDFORM.                               " BUILD_PERIOD_STRING
*&---------------------------------------------------------------------*
*&      Form  fill_dtable_hdr
*&---------------------------------------------------------------------*
FORM fill_dtable_hdr TABLES fdh_dtable STRUCTURE data_table
                            fdh_dtable_tmp STRUCTURE data_table.

  MOVE fdh_dtable-box     TO fdh_dtable_tmp-box.
  MOVE fdh_dtable-iabkrs  TO fdh_dtable_tmp-iabkrs.
  MOVE fdh_dtable-bukrs   TO fdh_dtable_tmp-bukrs.
  MOVE fdh_dtable-werks   TO fdh_dtable_tmp-werks.
  MOVE fdh_dtable-btrtl   TO fdh_dtable_tmp-btrtl.
  MOVE fdh_dtable-kostl   TO fdh_dtable_tmp-kostl.
  MOVE fdh_dtable-persg   TO fdh_dtable_tmp-persg.
  MOVE fdh_dtable-persk   TO fdh_dtable_tmp-persk.
  MOVE fdh_dtable-vdsk1   TO fdh_dtable_tmp-vdsk1.
  MOVE fdh_dtable-stat2   TO fdh_dtable_tmp-stat2.
  MOVE fdh_dtable-txcmp   TO fdh_dtable_tmp-txcmp.
  MOVE fdh_dtable-taxau   TO fdh_dtable_tmp-taxau.
  MOVE fdh_dtable-taxty   TO fdh_dtable_tmp-taxty.
  MOVE fdh_dtable-amt_curr TO fdh_dtable_tmp-amt_curr.
  MOVE fdh_dtable-btype   TO fdh_dtable_tmp-btype.

ENDFORM.                               " fill_dtable_hdr
*&---------------------------------------------------------------------*
*&      Form  BUILD_ACCUM_VALUES
*&---------------------------------------------------------------------*
FORM build_accum_values TABLES   bav_sort_tmp_1 STRUCTURE sort_table
                                 bav_sort_accum STRUCTURE sort_table
                                 bav_sort_table STRUCTURE sort_table
                                 bav_i_range  STRUCTURE range_prds
                        USING    bav_p_mtd
                                 bav_p_qtd
                                 bav_p_ytd
                                 bav_p_inf.

  DATA: sort_tmp_2 LIKE sort_table OCCURS 0 WITH HEADER LINE,
        sort_tmp_3 LIKE sort_table OCCURS 0 WITH HEADER LINE.

  LOOP AT bav_sort_accum.

    READ TABLE sort_tmp_2 WITH KEY pernr = bav_sort_accum-pernr
                                   paydt = bav_sort_accum-paydt
                                     box = bav_sort_accum-box
                                   taxau = bav_sort_accum-taxau
                              sort_strng = bav_sort_accum-sort_strng.
    IF sy-subrc <> 0.
      IF  bav_p_mtd = 'X'.
        CHECK bav_sort_accum-perid_m <> space.
      ENDIF.
      IF  bav_p_qtd = 'X'.
        CHECK bav_sort_accum-perid_q <> space.
      ENDIF.
      IF  bav_p_ytd = 'X'.
        CHECK bav_sort_accum-perid_y <> space.
      ENDIF.
      CLEAR sort_tmp_3. REFRESH sort_tmp_3.

      MOVE-CORRESPONDING bav_sort_accum TO sort_tmp_3.
      LOOP AT bav_sort_accum WHERE pernr      = sort_tmp_3-pernr
                               AND box        = sort_tmp_3-box
                               AND taxau      = sort_tmp_3-taxau
                               AND sort_strng = sort_tmp_3-sort_strng
                               AND paydt     <= sort_tmp_3-paydt.

        IF  bav_p_mtd = 'X'.
          READ TABLE bav_i_range WITH KEY per_id = sort_tmp_3-perid_m.
          IF bav_sort_accum-paydt BETWEEN bav_i_range-begda
                                  AND bav_i_range-endda.
            MOVE bav_sort_accum-perid_m TO sort_tmp_3-perid_m.
            MOVE bav_sort_accum-betrg_m TO sort_tmp_3-betrg_m.
          ELSE.
            sort_tmp_3-betrg_m = 0.
          ENDIF.
        ENDIF.
        IF  bav_p_qtd = 'X'.
          READ TABLE bav_i_range WITH KEY per_id = sort_tmp_3-perid_q.
          IF bav_sort_accum-paydt BETWEEN bav_i_range-begda
                                      AND bav_i_range-endda.
            MOVE bav_sort_accum-perid_q TO sort_tmp_3-perid_q.
            MOVE bav_sort_accum-betrg_q TO sort_tmp_3-betrg_q.
          ELSE.
            sort_tmp_3-betrg_q = 0.
          ENDIF.
        ENDIF.
        IF  bav_p_ytd = 'X'.
          READ TABLE bav_i_range WITH KEY per_id = sort_tmp_3-perid_y.
          IF bav_sort_accum-paydt BETWEEN bav_i_range-begda
                                      AND bav_i_range-endda.
            MOVE bav_sort_accum-perid_y TO sort_tmp_3-perid_y.
            MOVE bav_sort_accum-betrg_y TO sort_tmp_3-betrg_y.
          ELSE.
            sort_tmp_3-betrg_y = 0.
          ENDIF.
        ENDIF.
        COLLECT sort_tmp_3.
      ENDLOOP.

      LOOP AT sort_tmp_3.
        MOVE-CORRESPONDING sort_tmp_3 TO sort_tmp_2.
        sort_tmp_2-betrg = 0.
        sort_tmp_2-anzhl = 0.
        sort_tmp_2-atype = 'P'.
        APPEND sort_tmp_2.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

  CLEAR bav_sort_accum.   FREE  bav_sort_accum.
  CLEAR sort_tmp_3.  FREE sort_tmp_3.

  LOOP AT bav_sort_tmp_1.
    MOVE-CORRESPONDING bav_sort_tmp_1 TO bav_sort_table.
    READ TABLE sort_tmp_2 WITH KEY pernr = bav_sort_tmp_1-pernr
                                   box   = bav_sort_tmp_1-box
                                   taxau = bav_sort_tmp_1-taxau
                              sort_strng = bav_sort_tmp_1-sort_strng
                                   paydt = bav_sort_tmp_1-paydt.
    IF sy-subrc = 0.
      IF  bav_p_mtd = 'X'.
        MOVE sort_tmp_2-perid_m TO bav_sort_table-perid_m.
        MOVE sort_tmp_2-betrg_m TO bav_sort_table-betrg_m.
        MOVE sort_tmp_2-anzhl_m TO bav_sort_table-anzhl_m.
      ENDIF.
      IF  bav_p_qtd = 'X'.
        MOVE sort_tmp_2-perid_q TO bav_sort_table-perid_q.
        MOVE sort_tmp_2-betrg_q TO bav_sort_table-betrg_q.
        MOVE sort_tmp_2-anzhl_q TO bav_sort_table-anzhl_q.
      ENDIF.
      IF  bav_p_ytd = 'X'.
        MOVE sort_tmp_2-perid_y TO bav_sort_table-perid_y.
        MOVE sort_tmp_2-betrg_y TO bav_sort_table-betrg_y.
        MOVE sort_tmp_2-anzhl_y TO bav_sort_table-anzhl_y.
      ENDIF.
      IF bav_p_inf <> 'X'.
        bav_sort_table-pernr = space.
      ENDIF.
      COLLECT bav_sort_table.
    ENDIF.
  ENDLOOP.
  CLEAR bav_sort_tmp_1. FREE bav_sort_tmp_1.

  IF sy-subrc <> 0.
    LOOP AT sort_tmp_2.
      MOVE-CORRESPONDING sort_tmp_2 TO bav_sort_table.
      IF bav_p_inf <> 'X'.
        bav_sort_table-pernr = space.
      ENDIF.
      COLLECT bav_sort_table.
    ENDLOOP.
  ENDIF.

  CLEAR sort_tmp_2.  FREE sort_tmp_2.

ENDFORM.                               " BUILD_ACCUM_VALUES
*&---------------------------------------------------------------------*
*&      Form  PRINT_LOG
*&---------------------------------------------------------------------*
FORM print_log TABLES  pl_ee_table STRUCTURE ee_table
                USING  pl_frmtyp
                       pl_molga
                       pl_forms
                       pl_payty
                       pl_payid
                       pl_off
                       pl_special_pay.

  DATA: nonacctd  TYPE i VALUE 0,
        accounted TYPE i VALUE 0.

  FORMAT COLOR 5.
  ULINE.
  WRITE:/65'LOG'(416),
        132''.
  ULINE.
  dtable_lines = 0.

  DESCRIBE TABLE pl_ee_table LINES dtable_lines.
  FORMAT COLOR OFF.

  LOOP AT pl_ee_table.
    IF pl_ee_table-status = 'Y'.
      accounted = accounted + 1.
    ELSE.
      nonacctd = nonacctd + 1.
    ENDIF.
  ENDLOOP.

  WRITE:/.
  WRITE:/ 'Country code'(315),
         35':'(203), pl_molga.
  WRITE:/ 'Total number of employees selected'(310),
         35':'(203), dtable_lines.
  WRITE:/ 'Number of employees accounted for'(319),
         35':'(203), accounted.
  WRITE:/ 'Number of employees not accounted for'(317),
         35':'(203), nonacctd.
  IF pl_frmtyp = 'X'.
    WRITE:/ 'Pre-defined WT selection'(309),
          35':'(203), pl_forms.
  ELSE.
    WRITE:/ 'Ad hoc WT selection'(308).
  ENDIF.

  IF pl_special_pay = 'X'.
    WRITE:/ 'Off-cycle processing'(101),
          35':'(203),
          '             Pay type'(099), pl_payty,
          '             Pay ID'(098), pl_payid.
  ELSEIF pl_off = 'X'.
    WRITE:/ 'Off-cycle processing'(101),
          35':'(203),
          'ALL'(318).
  ENDIF.
ENDFORM.                               " PRINT_LOG
*&---------------------------------------------------------------------*
*&      Form  PRINT_RPT_HEADER
*&---------------------------------------------------------------------*
FORM print_rpt_header USING prh_form  LIKE t596a-appl
                            prh_pagno LIKE sy-pagno
                            prh_beg   LIKE pnpbegda
                            prh_end   LIKE pnpendda.

  SELECT SINGLE * FROM t596b WHERE sprsl = sy-langu
                               AND molga = '10'
                               AND appl  = prh_form.

  FORMAT COLOR 1 INTENSIFIED ON.
  WRITE:/.
  ULINE.
  WRITE: /51 sy-title,
        132 ''.
  WRITE: /1 'WT application -'(001),
         18 '(', prh_form, ')', t596b-text,
        110 'Page no.  -'(003),
        122  prh_pagno,
        132 ''.

  WRITE: /1 'Pay period     -'(004),
         18 pn-pabrp,
         20 '.',
         22 pn-pabrj,
         27 prh_beg DD/MM/YYYY NO-ZERO,
         38 '-',
         40 prh_end DD/MM/YYYY NO-ZERO,
         94 'Run date/time:'(005),
        110 sy-datum DD/MM/YYYY,
        122 sy-uzeit USING EDIT MASK '__:__:__',
        132''.

  ULINE.
  FORMAT COLOR OFF INTENSIFIED INVERSE OFF.


ENDFORM.                               " PRINT_RPT_HEADER

*&---------------------------------------------------------------------*
*&      Form  SELECT_WAGE_TYPES
*&---------------------------------------------------------------------*
FORM select_wage_types.

*<SAM850091>
* locals
  DATA: lv_ee_er_ind TYPE char2.
*</SAM850091>

*<SAM889346>
  CLEAR: swgtyp, swgtyp[].
*</SAM889346>

  IF wtype1 = 'X'.      "Report forms"    "NT. 0363102
    p_form = p_forms-low.
    SELECT * FROM t596i WHERE appl = p_form.
      CHECK p_end <= t596i-endda.
*<SAM889346>
      CLEAR swgtyp.
*</SAM889346>
      MOVE t596i-lgart TO swgtyp-lgart.
      MOVE t596i-sumlg TO swgtyp-sumlg.
      MOVE t596i-rechz TO swgtyp-rechz.
      SELECT SINGLE * FROM t5uto WHERE molga = '10'
                                   AND lgart = t596i-lgart
                                   AND endda >= p_end
                                   AND begda <= p_end.  "#EC CI_GENBUFF
      "YBHPL0K034044

      IF sy-subrc = 0.
        swgtyp-type = 'T'.
      ELSE.
        swgtyp-type = space.
      ENDIF.
      IF swgtyp-type = 'T'.
        PERFORM determine_ee_er_tax USING    swgtyp-lgart
                                    CHANGING lv_ee_er_ind.
        swgtyp-eree = lv_ee_er_ind.
      ELSE.
        CLEAR swgtyp-eree.
      ENDIF.
      COLLECT swgtyp.
    ENDSELECT.
  ELSE.                                "Custom"
    SELECT * FROM t512t WHERE lgart IN s_lgart
                          AND molga EQ '10'
                          AND sprsl EQ sy-langu.
*<SAM889346>
      CLEAR swgtyp.
*</SAM889346>
      MOVE-CORRESPONDING t512t TO swgtyp.
      MOVE t512t-lgart TO swgtyp-sumlg.
      SELECT SINGLE * FROM t5uto WHERE molga = '10'
                                   AND lgart = t512t-lgart
                                   AND endda >= p_end
                                   AND begda <= p_end.  "#EC CI_GENBUFF
      "YBHPL0K034044
      IF sy-subrc = 0.
        swgtyp-type = 'T'.
      ENDIF.
      APPEND swgtyp.
    ENDSELECT.
  ENDIF.

*<SAM850091>
*  lgart-sign   = 'I'.
*  lgart-option = 'EQ'.
*  CLEAR lgart-high.
*  LOOP AT swgtyp.
*    lgart-low = swgtyp-lgart.
*    COLLECT lgart.
*  ENDLOOP.
*<SAM850091>

ENDFORM.                               " Select_wage_types

*&---------------------------------------------------------------------*
*&      Form  READ_RU_TABLES
*&---------------------------------------------------------------------*
FORM read_rt_table.

* locals
  FIELD-SYMBOLS: <ls_swgtyp> LIKE LINE OF swgtyp.

*<SAM889346>
* get additional tax wage types not maintained in T5UTO
  LOOP AT swgtyp ASSIGNING <ls_swgtyp>.
    LOOP AT rt WHERE lgart = <ls_swgtyp>-lgart.
      IF NOT rt-cntr1 IS INITIAL.
        <ls_swgtyp>-type = 'T'.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
*</SAM889346>

*<SAM850091>
*  CLEAR ee_er_ind.                  " Employee/Employer Indikator
*  READ TABLE rt WITH KEY lgart = '/5PY'.                  "nt.0377694
*  IF sy-subrc = 0 AND rt-betrg < 0                        "nt.0377694
*  AND consider_outflow = 'X'.                             "nt.0377694
*    EXIT.                                                 "nt.0377694
*  ENDIF.                                                  "nt.0377694
*
*  PERFORM add_xdfrt_wagetypes.                            "Note 492427
*  REFRESH wts_read.                                       "Note 516833

* set flag to adjust tax related wage types
  READ TABLE rt WITH KEY lgart = '/5PY'.
  IF ( sy-subrc = 0 ) AND ( rt-betrg < 0 ).
    LOOP AT swgtyp WHERE type = 'T'.
      swgtyp-adjtax = 'X'.
    ENDLOOP.
  ENDIF.
*</SAM850091>

*<SAM850091>
* only process wage types to be reconciled
  LOOP AT swgtyp.
*   then fetch matching lgart from RT
*</SAM850091>
    LOOP AT rt WHERE lgart = swgtyp-lgart.                  "SAM850091
*               and   ( betrg ne 0 or                      "Note 492427
*                       anzhl ne 0 ).                      "Note 492427
      LOOP AT wpbp WHERE apznr = rt-apznr. ENDLOOP.
      IF sy-subrc NE 0.
        LOOP AT wpbp. ENDLOOP.
      ENDIF.
      CHECK wpbp-bukrs IN pnpbukrs AND
            wpbp-werks IN pnpwerks AND
            wpbp-btrtl IN pnpbtrtl AND
            wpbp-kostl IN pnpkostl AND
            wpbp-persg IN pnppersg AND
            wpbp-persk IN pnppersk AND
            wpbp-orgeh IN pnporgeh AND
            wpbp-stat2 IN pnpstat2.
*<SAM850091>
*     only account for RT amounts when there is no negative /5PY
*     this accounts for the fact that TCRT is not updated when /5PY < 0
      CHECK swgtyp-adjtax IS INITIAL.

**   only process wage types to be reconciled
*    LOOP AT swgtyp WHERE lgart = rt-lgart.
*</SAM850091>
      IF swgtyp-type = 'T'.
*     Load EE/ER taxes or both, in dependence of the user parameter
*     EMP_TAX
        CASE p_emptax.
          WHEN 'A'.
*           store data only if it is an employee tax
            CHECK swgtyp-eree = 'EE'.
          WHEN 'B'.
*           store data only if it is an employer tax
            CHECK swgtyp-eree = 'ER'.
          WHEN 'C'.
*           store taxes whatever the value of PRCL_72 is
        ENDCASE.
*<SAM850091>
*     adjust RT amounts for tax related wage types to the unaccounted
*     amounts of XDFRT if they are present, this corrects the fact
*     that US payroll generally follows the when-paid principle
        READ TABLE xdfrt WITH KEY lgart = swgtyp-lgart.
        IF sy-subrc = 0.
          rt-betrg = xdfrt-accam.
          rt-anzhl = xdfrt-accno.
        ENDIF.
*/SAM850091>
        PERFORM build_data_table_tax.
      ELSE.                              "Wagetype Report
        PERFORM build_data_table_wgtyp.
      ENDIF.
*<SAM850091>
*    ENDLOOP.
*<SAM850091>
    ENDLOOP.
  ENDLOOP.

ENDFORM.                               " READ_RU_TABLES

*&---------------------------------------------------------------------*
*&      Form  BUILD_DATA_TABLE_TAX
*&---------------------------------------------------------------------*
FORM build_data_table_tax.
  DATA: cntr1_num LIKE t5utt-taxty,
        save_rt LIKE rt,
        save_tax LIKE tax.

  CLEAR data_table_tmp.
  CLEAR tax.                                                "nt. 408545
  cntr1_num = rt-lgart+2(2).      "Take tax type aus lgart
* i.e. /301 -> TaxType = 01.
  SELECT SINGLE * FROM t5utt WHERE taxty = cntr1_num.
  data_table_tmp-taxty = t5utt-taxty.
  LOOP AT tax WHERE cntr1 = rt-cntr1.
    EXIT.
  ENDLOOP.
  CHECK tax-taxau  IN p_taxau.
  CHECK perm-txcmp IN p_txcmp.
  MOVE-CORRESPONDING evp  TO data_table_tmp.
  data_table_tmp-ipydt  = org_rgdir-paydt.
  data_table_tmp-inper  = org_rgdir-inper.
  data_table_tmp-inpty  = org_rgdir-payty.
  data_table_tmp-inpid  = org_rgdir-payid.
  data_table_tmp-iabkrs = org_rgdir-abkrs.
  MOVE-CORRESPONDING wpbp TO data_table_tmp.
  MOVE-CORRESPONDING rt   TO data_table_tmp.
  CLEAR p0000.
  rp-provide-from-last p0000 space pnpbegda pnpendda.
  data_table_tmp-stat2 = p0000-stat2.
  IF swgtyp-rechz = '-'.
*<SAM850091>
*    MULTIPLY data_table_tmp-betrg BY -1.
*    MULTIPLY data_table_tmp-anzhl BY -1.
    data_table_tmp-betrg = 0 - data_table_tmp-betrg.
    data_table_tmp-anzhl = 0 - data_table_tmp-anzhl.
*</SAM850091>
  ENDIF.
  IF evp-srtza NE 'A'.
*<SAM850091>
*    MULTIPLY data_table_tmp-betrg BY -1.
*    MULTIPLY data_table_tmp-anzhl BY -1.
    data_table_tmp-betrg = 0 - data_table_tmp-betrg.
    data_table_tmp-anzhl = 0 - data_table_tmp-anzhl.
*</SAM850091>
  ENDIF.
*<SAM850091>
*  IF rt-apznr < 2.                                      "nt. 0380586
*    PERFORM consider_outflow.
*  ENDIF.                                                "nt. 0380586
*</SAM850091>
  data_table_tmp-pernr     = pernr-pernr.
  data_table_tmp-atype     = atype_flag.
  data_table_tmp-taxty     = t5utt-taxty.
  data_table_tmp-taxau     = tax-taxau.
  data_table_tmp-txcmp     = perm-txcmp.
  data_table_tmp-box       = swgtyp-sumlg.

* cost split and data assignment            "nt. 0362632 and nt. 0380193
  PERFORM cost_assign.                                      "nt. 0397528

ENDFORM.                               " BUILD_DATA_TABLE_TAX
*&----------------------------------------------------------endform.
*&      Form  BUILD_DATA_TABLE_WGTYP
*&---------------------------------------------------------------------*
FORM build_data_table_wgtyp.

  CLEAR data_table_tmp.
  CHECK perm-txcmp IN p_txcmp.
  MOVE-CORRESPONDING evp  TO data_table_tmp.
  data_table_tmp-ipydt     = org_rgdir-paydt.
  data_table_tmp-inper     = org_rgdir-inper.
  data_table_tmp-inpty     = org_rgdir-inpty.
  data_table_tmp-inpid     = org_rgdir-inpid.
  MOVE-CORRESPONDING rt   TO data_table_tmp.
  IF swgtyp-rechz = '-'.
*<SAM850091>
*    MULTIPLY data_table_tmp-betrg BY -1.
*    MULTIPLY data_table_tmp-anzhl BY -1.
    data_table_tmp-betrg = 0 - data_table_tmp-betrg.
    data_table_tmp-anzhl = 0 - data_table_tmp-anzhl.
*</SAM850091>
  ENDIF.
  IF evp-srtza NE 'A'.
*<SAM850091>
*    MULTIPLY data_table_tmp-betrg BY -1.
*    MULTIPLY data_table_tmp-anzhl BY -1.
    data_table_tmp-betrg = 0 - data_table_tmp-betrg.
    data_table_tmp-anzhl = 0 - data_table_tmp-anzhl.
*</SAM850091>
  ENDIF.
  MOVE-CORRESPONDING wpbp TO data_table_tmp.
  CLEAR p0000.
  rp-provide-from-last p0000 space pnpbegda pnpendda.
  data_table_tmp-stat2 = p0000-stat2.
*<SAM850091>
*  READ TABLE wts_read WITH KEY lgart = rt-lgart.
*  IF sy-subrc NE 0.                       "nt. 0380586  "note 0516833
*    PERFORM consider_outflow.
*    wts_read-lgart = rt-lgart.                          "note 0516833
*    COLLECT wts_read.                                   "note 0516833
*  ENDIF.                                                 "nt. 0380586
*</SAM850091>
  data_table_tmp-atype = atype_flag.
  data_table_tmp-pernr = pernr-pernr.
  data_table_tmp-box   = swgtyp-sumlg.
  data_table_tmp-txcmp = perm-txcmp.
* cost split and data assignment            "nt. 0362632 and nt. 0380193
  PERFORM cost_assign.                                      "nt. 0397528

ENDFORM.                               " BUILD_DATA_TABLE_WGTYP
*&---------------------------------------------------------------------*
*&      Form  assign_data_to_tables
*&---------------------------------------------------------------------*
FORM assign_data_to_tables.
  CHECK NOT data_table_tmp-anzhl IS INITIAL OR
        NOT data_table_tmp-betrg IS INITIAL.

* if 'select off-cycles only' selected, process only type 'C'
* (Types 'C's are picked up, too even cretaed by a retro-calculation -
*  This check makes sure that only the type 'C' is evaluated
  IF 'C' IN p_paytyr AND
    p_offcyc = 'X'.
    IF NOT org_rgdir-payty IN p_paytyr.                   "note 0323221
      CHECK evp-payty = 'C'.
    ENDIF.                                                "note 0323221
  ENDIF.

* for XTD determination before the interval, just store in accum. table
  IF org_rgdir-paydt LT p_beg.
    PERFORM store_in_accumulation_table.
  ENDIF.
* for anything else, store in general data tables, too
  CHECK org_rgdir-paydt GE p_beg.

* process taxreporter adjustments:
* a) payrolls paid out before the as of date of the form for a previous
*    should be processed
* b) payrolls paid out after the interval until the as of date of
*    the form for the current form should be processed
  IF ( org_rgdir-paydt LE p_asop AND
       evp-paydt       LT p_beg ) OR
     org_rgdir-paydt GT p_end.
    PERFORM process_taxrep_adjustments.
  ENDIF.

  CHECK org_rgdir-paydt LE p_end.

*<SAM889346>
* for NAMC runs, consider the NAMC check date and not original period
* check date. This is applicable only for tax reporter views.
  IF p_fi IS INITIAL AND evp-payty = 'C'.              "Note 771647
    CHECK evp-paydt LE p_end.                          "Note 771647
  ELSE.
    CHECK org_rgdir-paydt LE p_end.
  ENDIF.
*</SAM889346>

* prior period adjustment will be listeted twice, first as prior period
* adjustment, then as a regular retro calculation
  IF evp-paydt  LT p_beg.
*   no accumulation here since this result will show up in retro calc
    data_table_ppadj = data_table_tmp.
    data_table_ppadj-btype = 'A'.
    COLLECT data_table_ppadj.
  ENDIF.

* store retro-calculation differences
  IF evp-seqnr NE org_rgdir-seqnr.
    PERFORM store_in_accumulation_table.
    data_table_retro = data_table_tmp.
    data_table_retro-btype = 'R'.
    COLLECT data_table_retro.
  ELSE.

* store current period amounts
*<SAM889346>
*    IF org_rgdir LE p_end.
    IF org_rgdir-paydt <= p_end.
*</SAM889346>
      PERFORM store_in_accumulation_table.
    ENDIF.
    data_table_cp = data_table_tmp.
    data_table_cp-btype = 'N'.
    COLLECT data_table_cp.
  ENDIF.
ENDFORM.                    " assign_data_to_tables
*&---------------------------------------------------------------------*
*&      Form  prepare_XTD_values
*&---------------------------------------------------------------------*
FORM prepare_xtd_values.
  DATA: sw_retro,
        xtd_begda LIKE p_beg,
        a_rgdir LIKE rgdir OCCURS 1 WITH HEADER LINE.
  DATA: s_accum LIKE data_table_accum OCCURS 10.
  CHECK NOT p_ytd IS INITIAL OR
        NOT p_qtd IS INITIAL OR
        NOT p_mtd IS INITIAL.
  IF NOT p_ytd IS INITIAL.
    READ TABLE i_range WITH KEY per_id = 'Yr'.
    xtd_begda = i_range-begda.
  ELSEIF NOT p_qtd IS INITIAL.
    LOOP AT i_range WHERE per_id CA 'Q'.
      EXIT.
    ENDLOOP.
    xtd_begda = i_range-begda.
  ELSEIF NOT p_mtd IS INITIAL.
    LOOP AT i_range WHERE per_id NA 'QY'.
      EXIT.
    ENDLOOP.
    xtd_begda = i_range-begda.
  ENDIF.
  CHECK NOT xtd_begda IS INITIAL.
* Code deleted because of problems with not selecting all     Nt. 360625
* necessary results for processing                                 "
* --------------------------------                                 "
** first, pick up the first payroll paid out within the interval   "
*  sw_retro = 'X'.                                                 "
*  loop at rgdir where paydt ge p_beg                              "
*                and   void  is initial.                           "
*    call function 'CD_RETROCALC_PERIOD'                           "
*         exporting entry = rgdir                                  "
*         importing calcd = sw_retro.                              "
*    check sw_retro eq ' '.                                        "
*    exit.                                                         "
*  endloop.                                                        "
** no payroll within the reporting interval -> keep all rgdir entries
*  if sw_retro is initial.                                         "
*    rgdir-rundt = '99991231'.                                     "
*    rgdir-runtm = '235959'.                                       "
*  endif.                                                          "
*  a_rgdir[] = rgdir[].                                            "
*  delete a_rgdir where rundt gt rgdir-rundt.                      "
*  delete a_rgdir where rundt eq rgdir-rundt                       "
*                 and   runtm ge rgdir-runtm.                      "
  a_rgdir[] = rgdir[].                                             "
* SET ACTIVE PASSIVE FOR DIFFERENCE-CALCULATION
  CALL FUNCTION 'CD_REORG_RGDIR'
    TABLES
      rgdir = a_rgdir.
  IF NOT p_inf IS INITIAL.
    CLEAR data_table_o_acc. REFRESH data_table_o_acc.
  ENDIF.
* now pick up all payrolls starting from the begin date of the first
* relevant XTD intervall to consider payrolls before given interval
* begin date
* to do this, it's enough just to evaluate the latest payrolls
* status = 'A'
  s_accum[] = data_table_accum[].
  LOOP AT a_rgdir WHERE paydt GE xtd_begda
                  AND   paydt LT p_beg
                  AND   void  IS INITIAL.
    CALL FUNCTION 'CD_RETROCALC_PERIOD'
      EXPORTING
        entry = a_rgdir
      IMPORTING
        calcd = sw_retro.
    CHECK sw_retro EQ ' '.
    org_rgdir = a_rgdir.
    CHECK org_rgdir-abkrs IN pnpabkrs.                   "Note 566169
    PERFORM get_all_involved_pays.
    LOOP AT evp.
*     check evp-abkrs in pnpabkrs.                       "note306832
      PERFORM extract_ru_data USING evp-seqnr.
      CLEAR: data_table_accum. REFRESH data_table_accum.
      PERFORM read_rt_table.
      PERFORM add_only_to_old_accum.
    ENDLOOP.
  ENDLOOP.
  data_table_accum[] = s_accum[].
ENDFORM.                    " prepare_XTD_values
*&---------------------------------------------------------------------*
*&      Form  store_in_accumulation_table
*&---------------------------------------------------------------------*
FORM store_in_accumulation_table.
  data_table_tmp-btype = 'N'.
  MOVE-CORRESPONDING data_table_tmp TO data_table_accum.
  CLEAR: data_table_accum-betrg,
         data_table_accum-anzhl.
  CLEAR: data_table_accum-fpper,
         data_table_accum-paydt,
         data_table_accum-payty,
         data_table_accum-payid.
  PERFORM clear_not_used_fields USING data_table_accum.
* cumulate according paydate of for-period as of in-period paydate
  LOOP AT p_range WHERE begda LE org_rgdir-paydt
                  AND   endda GE org_rgdir-paydt.
    IF p_range-per_id CS 'Yr' AND NOT p_ytd IS INITIAL.
      data_table_accum-perid_y = p_range-per_id.
      data_table_accum-betrg_y = data_table_tmp-betrg.
      data_table_accum-anzhl_y = data_table_tmp-anzhl.
      COLLECT data_table_accum.
      CLEAR: data_table_accum-perid_y,
             data_table_accum-betrg_y,
             data_table_accum-anzhl_y.
    ENDIF.
    IF p_range-per_id CA 'Q' AND NOT p_qtd IS INITIAL.
      data_table_accum-perid_q = p_range-per_id.
      data_table_accum-betrg_q = data_table_tmp-betrg.
      data_table_accum-anzhl_q = data_table_tmp-anzhl.
      COLLECT data_table_accum.
      CLEAR: data_table_accum-perid_q,
             data_table_accum-betrg_q,
             data_table_accum-anzhl_q.
    ENDIF.
    IF p_range-per_id NA 'QY' AND NOT p_mtd IS INITIAL.
      data_table_accum-perid_m = p_range-per_id.
      data_table_accum-betrg_m = data_table_tmp-betrg.
      data_table_accum-anzhl_m = data_table_tmp-anzhl.
      COLLECT data_table_accum.
      CLEAR: data_table_accum-perid_m,
             data_table_accum-betrg_m,
             data_table_accum-anzhl_m.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " store_in_accumulation_table

*&---------------------------------------------------------------------*
*&      Form  get_tax_rep_asof_dates
*&---------------------------------------------------------------------*
FORM get_tax_rep_asof_dates.
  DATA: genda_tmp LIKE v_5uxy_a-genda,
        gentm_tmp LIKE v_5uxy_a-gentm,
        asofd_tmp LIKE v_5uxy_a-asofd,
        last_asofd_tmp LIKE v_5uxy_a-asofd,
        qf_endda LIKE pnpendda,
        ql_endda LIKE pnpendda.
  DATA: itab_5uxy_tmp LIKE v_5uxy_a OCCURS 1 WITH HEADER LINE,
        form_prd(5),
        rtype(1).
  CLEAR: genda_tmp, gentm_tmp, asofd_tmp.

*<SAM850091>
*  consider_outflow = 'X'.
*<SAM850091>

  IF p_w2 = 'X'.
    tr_form_typ = 'W2'.
  ELSEIF p_941 = 'X'.
    tr_form_typ = '941'.
  ELSEIF p_sui = 'X'.
    tr_form_typ = 'SUI'.
  ELSEIF p_fi = 'X'.
    tr_form_typ = 'FI'.
*<SAM850091>
*    CLEAR consider_outflow.
*<SAM850091>
  ENDIF.

* Determine the End Date of the quarter preceding the reporting period *
  LOOP AT i_range WHERE per_id CA 'Q'.
    IF qf_endda IS INITIAL.
      qf_endda = i_range-begda - 1.
    ELSEIF qf_endda > i_range-begda.
      qf_endda = i_range-begda - 1.
    ENDIF.
  ENDLOOP.

*  Determine the End Date of the last quarter of the reporting period  *
  LOOP AT i_range WHERE per_id CA 'Q'.
    IF ql_endda IS INITIAL.
      ql_endda = i_range-endda.
    ELSEIF ql_endda < i_range-endda.
      ql_endda = i_range-endda.
    ENDIF.
  ENDLOOP.

  CASE tr_form_typ.
* Locate the main form name for W-2                                    *
    WHEN 'W2'.
      IF p_tfrcl IS INITIAL.
        p_tfrcl = 'A100'.
      ENDIF.
    WHEN '941'.
      IF p_tfrcl IS INITIAL.
        p_tfrcl = 'Q100'.
      ENDIF.
    WHEN 'SUI'.
      IF p_tfrcl IS INITIAL.
        p_tfrcl = 'Q200'.
      ENDIF.
  ENDCASE.

  PERFORM fill_itab_5uxy USING qf_endda
                               ql_endda.
  PERFORM set_taxrep_adj_date USING qf_endda
                                    ql_endda.
ENDFORM.                    " get_tax_rep_asof_dates
*&---------------------------------------------------------------------*
*&      Form  FILL_ITAB_5UXY
*&---------------------------------------------------------------------*
FORM fill_itab_5uxy USING fi5_qf_endda LIKE pnpendda
                          fi5_ql_endda LIKE pnpendda.
  DATA: genda_tmp LIKE v_5uxy_a-genda,
        gentm_tmp LIKE v_5uxy_a-gentm,
        asofd_tmp LIKE v_5uxy_a-asofd,
        last_asofd_tmp LIKE v_5uxy_a-asofd.

  DATA: BEGIN OF i_5uxy_a OCCURS 2.
          INCLUDE STRUCTURE v_5uxy_a.
  DATA: END OF i_5uxy_a.
  DATA: form_exists.
  DATA: BEGIN OF i5uxy_tmp.
          INCLUDE STRUCTURE v_5uxy_a.
  DATA: END OF i5uxy_tmp.
  DATA: form_prd(5),
        rtype(1).

  CHECK p_trprd = 'X'.

  CLEAR: genda_tmp, gentm_tmp, asofd_tmp.
* first: get all tax forms which match the tax form class
  PERFORM re5ut7 TABLES trfrm_itab USING p_tfrcl.
  PERFORM re5ut8 TABLES trfrm_itab USING p_tfrcl.
  PERFORM re51t8 TABLES trfrm_itab.

* second: get the as of dates for the previous quarter for all selected
*         tax forms (including correction fomrs)
  LOOP AT trfrm_itab.
    SELECT * FROM v_5uxy_a WHERE molga =  '10'
                             AND txfrm = trfrm_itab-txfrm
                             AND txcmp IN p_txcmp
                             AND taxau IN p_taxau
                             AND endda = fi5_qf_endda.
      MOVE-CORRESPONDING v_5uxy_a TO i_5uxy_a.
      APPEND i_5uxy_a.
    ENDSELECT.
    IF sy-subrc NE 0.
      CLEAR i_5uxy_a.
      i_5uxy_a-txfrm = trfrm_itab-txfrm.
      i_5uxy_a-taxau = trfrm_itab-taxau.
      APPEND i_5uxy_a.
    ENDIF.
    SELECT * FROM v_5uxy_a WHERE molga =  '10'
                             AND txfrm = trfrm_itab-crnfm
                             AND txcmp IN p_txcmp
                             AND taxau IN p_taxau
                             AND endda = fi5_qf_endda.
      MOVE-CORRESPONDING v_5uxy_a TO i_5uxy_a.
      APPEND i_5uxy_a.
    ENDSELECT.
    IF sy-subrc NE 0.
      CLEAR i_5uxy_a.
      i_5uxy_a-txfrm = trfrm_itab-crnfm.
      i_5uxy_a-taxau = trfrm_itab-ctxau.
      APPEND i_5uxy_a.
    ENDIF.
  ENDLOOP.
* for this previous form, store the latest as of dates
  SORT i_5uxy_a.
  LOOP AT i_5uxy_a.
    MOVE-CORRESPONDING i_5uxy_a TO i5uxy_tmp.
    AT END OF txcmp.
*     make sure that current form is the original form
      READ TABLE trfrm_itab WITH KEY txfrm = i5uxy_tmp-txfrm
                                     taxau = i5uxy_tmp-taxau.
      IF sy-subrc NE 0.
        READ TABLE trfrm_itab WITH KEY crnfm = i5uxy_tmp-txfrm
                                       ctxau = i5uxy_tmp-taxau.
      ENDIF.
*     if there is already an entry for this form, make sure that
*     this form has a later as  of date
      CLEAR form_exists.
      READ TABLE itrdt WITH KEY txfrm = trfrm_itab-txfrm
                                taxau = i5uxy_tmp-taxau
                                txcmp = i5uxy_tmp-txcmp.
      IF sy-subrc = 0.
        CHECK itrdt-last_asofd LT i5uxy_tmp-asofd.
        form_exists = 'X'.
      ENDIF.
      itrdt-txfrm      = trfrm_itab-txfrm.
      itrdt-txcmp      = i5uxy_tmp-txcmp.
      itrdt-taxau      = i5uxy_tmp-taxau.
      itrdt-last_asofd = i5uxy_tmp-asofd.
      itrdt-last_gentm = i5uxy_tmp-gentm.
      itrdt-last_genda = i5uxy_tmp-genda.
      CLEAR: itrdt-tsobj, itrdt-frmnm, itrdt-filda,
             itrdt-cntrl.
      IF form_exists = 'X'.
        MODIFY itrdt INDEX sy-tabix.
      ELSE.
        itrdt-txcmp = i_5uxy_a-txcmp.
        APPEND itrdt.
      ENDIF.
    ENDAT.
  ENDLOOP.
  CLEAR i_5uxy_a. REFRESH i_5uxy_a.
* third: do the same for the current period form
  LOOP AT trfrm_itab.
    SELECT * FROM v_5uxy_a WHERE molga =  '10'
                             AND txfrm = trfrm_itab-txfrm
                             AND txcmp IN p_txcmp
                             AND taxau IN p_taxau
                             AND endda = fi5_ql_endda.
      MOVE-CORRESPONDING v_5uxy_a TO i_5uxy_a.
      APPEND i_5uxy_a.
    ENDSELECT.
    IF sy-subrc NE 0.
      CLEAR i_5uxy_a.
      i_5uxy_a-txfrm = trfrm_itab-txfrm.
      i_5uxy_a-taxau = trfrm_itab-taxau.
      APPEND i_5uxy_a.
    ENDIF.
    SELECT * FROM v_5uxy_a WHERE molga =  '10'
                             AND txfrm = trfrm_itab-crnfm
                             AND txcmp IN p_txcmp
                             AND taxau IN p_taxau
                             AND endda = fi5_ql_endda.
      MOVE-CORRESPONDING v_5uxy_a TO i_5uxy_a.
      APPEND i_5uxy_a.
    ENDSELECT.
    IF sy-subrc NE 0.
      CLEAR i_5uxy_a.
      i_5uxy_a-txfrm = trfrm_itab-txfrm.
      i_5uxy_a-taxau = trfrm_itab-ctxau.
      APPEND i_5uxy_a.
    ENDIF.
  ENDLOOP.
* for this previous form, store the latest as of dates
  SORT i_5uxy_a.
  LOOP AT i_5uxy_a.
    MOVE-CORRESPONDING i_5uxy_a TO i5uxy_tmp.
    AT END OF txcmp.
*     make sure that current form is the original form
      READ TABLE trfrm_itab WITH KEY txfrm = i5uxy_tmp-txfrm
                                     taxau = i5uxy_tmp-taxau.
      IF sy-subrc NE 0.
        READ TABLE trfrm_itab WITH KEY crnfm = i5uxy_tmp-txfrm
                                       ctxau = i5uxy_tmp-taxau.
      ENDIF.
*     if there is already an entry for this form, make sure that
*     this form has a later as  of date
      CLEAR form_exists.
      READ TABLE itrdt WITH KEY txfrm = trfrm_itab-txfrm
                                taxau = i5uxy_tmp-taxau
                                txcmp = i5uxy_tmp-txcmp.
      IF sy-subrc = 0.
        CHECK itrdt-asofd LT i5uxy_tmp-asofd.
        form_exists = 'X'.
      ENDIF.
      itrdt-txfrm      = trfrm_itab-txfrm.
      itrdt-txcmp      = i5uxy_tmp-txcmp.
      itrdt-taxau      = i5uxy_tmp-taxau.
      itrdt-asofd = i5uxy_tmp-asofd.
      itrdt-gentm = i5uxy_tmp-gentm.
      itrdt-genda = i5uxy_tmp-genda.
      CLEAR: itrdt-tsobj, itrdt-frmnm, itrdt-filda,
             itrdt-cntrl.
      IF form_exists = 'X'.
        MODIFY itrdt INDEX sy-tabix.
      ELSE.
        itrdt-txcmp = i_5uxy_a-txcmp.
        APPEND itrdt.
      ENDIF.
    ENDAT.
  ENDLOOP.
ENDFORM.                    " FILL_ITAB_5UXY
*&---------------------------------------------------------------------*
*&      Form  RE51T8
*&---------------------------------------------------------------------*
FORM re51t8 TABLES re_trfrm_itab STRUCTURE trfrm_itab.

  DATA: i_51t8_tmp LIKE t51t8 OCCURS 0 WITH HEADER LINE.
  DATA: sy_tabix LIKE sy-tabix.

  SELECT * FROM t51t8 INTO i_51t8_tmp.
    APPEND i_51t8_tmp.                    "#EC CI_GENBUFF YBHPL0K034044
  ENDSELECT.
  LOOP AT re_trfrm_itab.
    sy_tabix = sy-tabix.
    LOOP AT i_51t8_tmp WHERE molga = '10'
                         AND endda >= p_end
                         AND begda <= p_end
                         AND crnfr = re_trfrm_itab-txfrm.
    ENDLOOP.
    IF sy-subrc = 0.
      IF i_51t8_tmp-taxau IN p_taxau.
        re_trfrm_itab-crnfm = i_51t8_tmp-txfrm.
        re_trfrm_itab-ctxau = i_51t8_tmp-taxau.
        MODIFY re_trfrm_itab INDEX sy_tabix.
      ENDIF.
    ENDIF.
    LOOP AT i_51t8_tmp WHERE molga = '10'
                        AND endda >= p_end
                        AND begda <= p_end
                        AND txfrm = re_trfrm_itab-txfrm.
    ENDLOOP.
    IF sy-subrc <> 0.
      DELETE re_trfrm_itab.
    ELSE.
      IF NOT i_51t8_tmp-taxau IN p_taxau.
        DELETE re_trfrm_itab.
      ELSE.
        re_trfrm_itab-taxau = i_51t8_tmp-taxau.
        MODIFY re_trfrm_itab INDEX sy_tabix.
      ENDIF.
    ENDIF.
  ENDLOOP.
  CLEAR i_51t8_tmp. FREE i_51t8_tmp.
ENDFORM.                                                    " RE51T8

*&---------------------------------------------------------------------*
*&      Form  add_accumulation_data
*&---------------------------------------------------------------------*
FORM add_accumulation_data TABLES p_data  STRUCTURE data_table
                                  p_accum STRUCTURE data_table_accum.
  DATA: sy_tabix LIKE sy-tabix,
        found.
  CHECK NOT p_ytd IS INITIAL OR
        NOT p_qtd IS INITIAL OR
        NOT p_mtd IS INITIAL.
  SORT p_data   BY ipydt.
  SORT p_accum BY ipydt.
  SORT i_range ASCENDING.
  LOOP AT i_range WHERE endda GE p_beg
                  AND   begda LE p_end.
    LOOP AT p_accum WHERE perid_y = i_range-per_id
                    OR    perid_q = i_range-per_id
                    OR    perid_m = i_range-per_id.
      CLEAR found.
      CHECK NOT p_accum-betrg_y IS INITIAL OR
            NOT p_accum-anzhl_y IS INITIAL OR
            NOT p_accum-betrg_q IS INITIAL OR
            NOT p_accum-anzhl_q IS INITIAL OR
            NOT p_accum-betrg_m IS INITIAL OR
            NOT p_accum-anzhl_m IS INITIAL.
      LOOP AT p_data WHERE
              box    = p_accum-box    AND
              lgart  = p_accum-lgart  AND
              abkrs  = p_accum-abkrs  AND
              iabkrs = p_accum-iabkrs AND
              bukrs  = p_accum-bukrs  AND
              werks  = p_accum-werks  AND
              btrtl  = p_accum-btrtl  AND
              kostl  = p_accum-kostl  AND
              persg  = p_accum-persg  AND
              persk  = p_accum-persk  AND
              vdsk1  = p_accum-vdsk1  AND
              stat2  = p_accum-stat2  AND
              txcmp  = p_accum-txcmp  AND
              taxau  = p_accum-taxau  AND
              taxty  = p_accum-taxty  AND
              ipydt GE p_accum-ipydt  AND
              btype  = 'N'            AND
            ( perid_y = i_range-per_id OR
              perid_q = i_range-per_id OR
              perid_m = i_range-per_id ).
        IF p_data-perid_y = i_range-per_id OR
           p_data-perid_q = i_range-per_id OR
           p_data-perid_m = i_range-per_id.
          found = 'X'.
        ENDIF.
        sy_tabix = sy-tabix.
        PERFORM assign_xtd_values USING p_accum p_data
                                        i_range-per_id.
        MODIFY p_data INDEX sy_tabix.
      ENDLOOP.
      IF found IS INITIAL.
        PERFORM add_existing_accum TABLES p_data
                                   USING  p_accum i_range-per_id.

      ENDIF.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    " add_accumulation_data
*&---------------------------------------------------------------------*
*&      Form  add_existing_accum
*&---------------------------------------------------------------------*
FORM add_existing_accum TABLES pp_data  STRUCTURE data_table
                        USING  pp_accum STRUCTURE data_table_accum
                               p_per_id  LIKE i_range-per_id.
  DATA: s_string_tmp LIKE pp_data-sort_strng.              "  Nt. 360625
  CLEAR s_string_tmp.                                              "
  CLEAR pp_data.
  PERFORM get_last_entry TABLES pp_data
                         USING  pp_accum.
  pp_accum-betrg_y = pp_accum-betrg_y + pp_data-betrg_y.
  pp_accum-betrg_q = pp_accum-betrg_q + pp_data-betrg_q.
  pp_accum-betrg_m = pp_accum-betrg_m + pp_data-betrg_m.
  pp_accum-anzhl_y = pp_accum-anzhl_y + pp_data-anzhl_y.
  pp_accum-anzhl_q = pp_accum-anzhl_q + pp_data-anzhl_q.
  pp_accum-anzhl_m = pp_accum-anzhl_m + pp_data-anzhl_m.
  pp_data       = pp_accum.
  pp_data-ipydt = pp_accum-ipydt.
  PERFORM set_other_perid USING pp_accum pp_data p_per_id.
  IF p_inf IS INITIAL.                                     "  Nt. 360625
    PERFORM sort_options_loop TABLES pp_data USING s_string_tmp.   "
  ENDIF.                                                           "
  MOVE s_string_tmp TO pp_data-sort_strng.                         "
*  perform assign_xtd_values tables pp_accum pp_data
*                            using  p_per_id.
  COLLECT pp_data.
  SORT pp_data BY ipydt.
ENDFORM.                    " add_existing_accum
*&---------------------------------------------------------------------*
*&      Form  set_accumulation_data
*&---------------------------------------------------------------------*
FORM set_accumulation_data.
* if there are no Ee data to be diplayed within the detailed list
* data_table_accum will carry the accumulation data for all employees
* therefore no refresh of data_table_accum per period
* if there are Ee data to be diplayed within the detailed list
* data_table_accum is only a temporary table and the XTD information
* will be directly stored in the main data table
* therefore refresh data_table_accum
  IF NOT p_inf IS INITIAL.
    REFRESH data_table_accum.
    CLEAR   data_table_accum.
  ENDIF.
ENDFORM.                    " set_accumulation_data
*&---------------------------------------------------------------------*
*&      Form  assign_XTD_values
*&---------------------------------------------------------------------*
FORM assign_xtd_values USING pp_accum STRUCTURE data_table_accum
                             pp_data   STRUCTURE data_table
                             x_per_id LIKE i_range-per_id.
  IF x_per_id = pp_data-perid_y.
    pp_data-perid_y = x_per_id.
    pp_data-betrg_y = pp_data-betrg_y + pp_accum-betrg_y.
    pp_data-anzhl_y = pp_data-anzhl_y + pp_accum-anzhl_y.
  ENDIF.
  IF x_per_id = pp_data-perid_q.
    pp_data-perid_q = x_per_id.
    pp_data-betrg_q = pp_data-betrg_q + pp_accum-betrg_q.
    pp_data-anzhl_q = pp_data-anzhl_q + pp_accum-anzhl_q.
  ENDIF.
  IF x_per_id = pp_data-perid_m.
    pp_data-perid_m = x_per_id.
    pp_data-betrg_m = pp_data-betrg_m + pp_accum-betrg_m.
    pp_data-anzhl_m = pp_data-anzhl_m + pp_accum-anzhl_m.
  ENDIF.
  PERFORM set_other_perid USING pp_accum pp_data x_per_id.
ENDFORM.                    " assign_XTD_values
*&---------------------------------------------------------------------*
*&      Form  complete_data_table
*&---------------------------------------------------------------------*
FORM complete_data_table TABLES p_data_table STRUCTURE data_table.
  CHECK NOT p_ytd IS INITIAL OR
        NOT p_qtd IS INITIAL OR
        NOT p_mtd IS INITIAL.
  LOOP AT p_data_table WHERE ( perid_y IS INITIAL
                       OR      perid_q IS INITIAL
                       OR      perid_m IS INITIAL )
                       AND     btype = 'N'.
    IF p_data_table-perid_y IS INITIAL.
      LOOP AT i_range WHERE begda  LE p_data_table-paydt
                        AND endda  GE p_data_table-paydt
                        AND per_id = 'Yr'.
        p_data_table-perid_y = i_range-per_id.
        MODIFY p_data_table.
      ENDLOOP.
    ENDIF.
    IF p_data_table-perid_q IS INITIAL.
      LOOP AT i_range WHERE begda  LE p_data_table-paydt
                       AND endda  GE p_data_table-paydt
                       AND per_id CA 'Q'.
        p_data_table-perid_q = i_range-per_id.
        MODIFY p_data_table.
      ENDLOOP.
    ENDIF.
    IF p_data_table-perid_m IS INITIAL.
      LOOP AT i_range WHERE begda  LE p_data_table-paydt
                        AND endda  GE p_data_table-paydt
                        AND per_id NA 'QY'.
        p_data_table-perid_m = i_range-per_id.
        MODIFY p_data_table.
      ENDLOOP.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " complete_data_table
*&---------------------------------------------------------------------*
*&      Form  ADD_TO_OLD_ACCUM
*&---------------------------------------------------------------------*
FORM add_to_old_accum.
  data_table_o_acc = data_table_accum.
  CLEAR: data_table_o_acc-inper, data_table_o_acc-inpty,
         data_table_o_acc-inpid, data_table_o_acc-ipydt.
  data_table_o_acc-ipydt = p_beg - 1.
  COLLECT data_table_o_acc.
ENDFORM.                    " ADD_TO_OLD_ACCUM

*&---------------------------------------------------------------------*
*&      Form  ADD_ONLY_TO_OLD_ACCUM
*&---------------------------------------------------------------------*
FORM add_only_to_old_accum.
  LOOP AT data_table_accum.
    PERFORM clear_not_used_fields USING data_table_accum.
    PERFORM add_to_old_accum.
  ENDLOOP.
ENDFORM.                    " ADD_ONLY_TO_OLD_ACCUM

*&---------------------------------------------------------------------*
*&      Form  COMPLETE_MISSING_ACCUM
*&---------------------------------------------------------------------*
FORM complete_missing_accum.
  DATA: s_accum LIKE data_table_o_acc OCCURS 1 WITH HEADER LINE.
  data_table_accum = data_table_o_acc.
  s_accum[] = data_table_o_acc[].
  LOOP AT p_range WHERE begda LE p_beg
                  AND   endda GE p_beg.
    LOOP AT s_accum WHERE iabkrs = data_table_o_acc-iabkrs
                    AND   box    = data_table_o_acc-box
                    AND   lgart  = data_table_o_acc-lgart
                    AND   bukrs  = data_table_o_acc-bukrs
                    AND   werks  = data_table_o_acc-werks
                    AND   btrtl  = data_table_o_acc-btrtl
                    AND   kostl  = data_table_o_acc-kostl
                    AND   persg  = data_table_o_acc-persg
                    AND   persk  = data_table_o_acc-persk
                    AND   vdsk1  = data_table_o_acc-vdsk1
                    AND   stat2  = data_table_o_acc-stat2
                    AND   txcmp  = data_table_o_acc-txcmp
                    AND   taxau  = data_table_o_acc-taxau
                    AND   taxty  = data_table_o_acc-taxty
                    AND   btype  = 'N'
                    AND ( perid_y = p_range-per_id OR
                          perid_q = p_range-per_id OR
                          perid_m = p_range-per_id ).
      IF NOT s_accum-perid_y IS INITIAL.
        data_table_accum-perid_y = s_accum-perid_y.
        data_table_accum-betrg_y = s_accum-betrg_y.
        data_table_accum-anzhl_y = s_accum-anzhl_y.
      ENDIF.
      IF NOT s_accum-perid_q IS INITIAL.
        data_table_accum-perid_q = s_accum-perid_q.
        data_table_accum-betrg_q = s_accum-betrg_q.
        data_table_accum-anzhl_q = s_accum-anzhl_q.
      ENDIF.
      IF NOT s_accum-perid_m IS INITIAL.
        data_table_accum-perid_m = s_accum-perid_m.
        data_table_accum-betrg_m = s_accum-betrg_m.
        data_table_accum-anzhl_m = s_accum-anzhl_m.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    " COMPLETE_MISSING_ACCUM
*&---------------------------------------------------------------------*
*&      Form  SET_TAXREP_ADJ_DATE
*&---------------------------------------------------------------------*
FORM set_taxrep_adj_date USING p_asofp LIKE rgdir-paydt
                               p_asofc LIKE rgdir-paydt.

* Just in case, put a dummy entry into i5uxy_tmp which
* has the as of date for all other tax companies
  CLEAR itrdt.
  IF p_trsim = 'X'.
    IF p_lasfd IS INITIAL.
      p_lasfd = p_beg - 1.
    ENDIF.
    itrdt-last_asofd = p_lasfd.
    itrdt-last_genda = '99991231'.
    itrdt-last_gentm = '235959'.
    IF p_asofd IS INITIAL.
      p_asofd = p_end.
    ENDIF.
    itrdt-asofd      = p_asofd.
    IF p_genda IS INITIAL.
      p_genda = '99991231'.
    ENDIF.
    itrdt-genda      = p_genda.
    IF p_gentm IS INITIAL.
      p_gentm = '235959'.
    ENDIF.
    itrdt-gentm      = p_gentm.
    APPEND itrdt.
  ELSE.
    itrdt-last_asofd = p_beg - 1.
    itrdt-last_genda = p_beg - 1.
    itrdt-last_gentm = '235959'.
    itrdt-asofd      = p_end.
    itrdt-genda      = p_end.
    itrdt-gentm      = '235959'.
    APPEND itrdt.
  ENDIF.
  p_asop = p_asofp.
  p_asoc = p_asofc.
  LOOP AT itrdt.
    IF itrdt-asofd IS INITIAL.
      itrdt-asofd      = p_end.
      itrdt-genda      = p_end.
      itrdt-gentm      = '235959'.
      MODIFY itrdt INDEX sy-tabix.
    ENDIF.
    IF itrdt-last_asofd IS INITIAL.
      itrdt-last_asofd = p_beg - 1.
      itrdt-last_genda = p_beg - 1.
      itrdt-last_gentm = '235959'.
      MODIFY itrdt INDEX sy-tabix.
    ENDIF.
    IF itrdt-asofd GT p_asoc.
      p_asoc = itrdt-asofd.
    ENDIF.
    IF itrdt-last_asofd GT p_asop.
      p_asop = itrdt-last_asofd.
    ENDIF.
  ENDLOOP.

*<SAM889346>
*Comment at the request of Payroll to remove msg>>>HASSAN-UD1K921280
* if tax reporter form class assigned
*  IF ( p_trprd = 'X' ) AND ( p_end <> p_asoc ).
*    DATA: lv_text1 TYPE char10,
*          lv_text2 TYPE char10.
*    WRITE p_end TO lv_text1.
*    WRITE p_asoc TO lv_text2.
*    MESSAGE i001(3g) WITH 'Selection enddate'(002) lv_text1
*                          'overridden by tax reporter date'(006)
*                          lv_text2.
*  ENDIF.

* for the case that there is a tax reporter simulation and no tax form
* class is selected and teh in-period paydate is not within the period
* selected then override the as-of-date witht the selecteion period end
* date
  IF ( p_trprd IS INITIAL ) AND ( p_tfrcl IS INITIAL ) AND
     ( org_rgdir-paydt > p_end ).
    p_asoc = p_end.
  ENDIF.
*</SAM889346>

ENDFORM.                    " SET_TAXREP_ADJ_DATE
*&---------------------------------------------------------------------*
*&      Form  PROCESS_SELECTION_SCREEN
*&---------------------------------------------------------------------*
FORM process_selection_screen.
  PERFORM process_screen_commands.
  PERFORM general_screen_processing.
ENDFORM.                    " PROCESS_SELECTION_SCREEN
*&---------------------------------------------------------------------*
*&      Form  SET_OTHER_PERID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_X_PER_ID  text
*----------------------------------------------------------------------*
FORM set_other_perid USING pp_accum STRUCTURE data_table
                           pp_data  STRUCTURE data_table
                           x_per_id LIKE i_range-per_id.
  IF pp_data-perid_y IS INITIAL OR
     pp_data-perid_q IS INITIAL OR
     pp_data-perid_m IS INITIAL.
    LOOP AT p_range WHERE per_id NE x_per_id
                    AND   begda LE pp_accum-ipydt
                    AND   endda GE pp_accum-ipydt.
      IF pp_data-perid_y IS INITIAL AND
         p_range-per_id CA 'Y'.
        pp_data-perid_y = p_range-per_id.
      ENDIF.
      IF pp_data-perid_q IS INITIAL AND
         p_range-per_id CA 'Q'.
        pp_data-perid_q = p_range-per_id.
      ENDIF.
      IF pp_data-perid_m IS INITIAL AND
         p_range-per_id NA 'YQ'.
        pp_data-perid_m = p_range-per_id.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " SET_OTHER_PERID
*&---------------------------------------------------------------------*
*&      Form  GET_LAST_ENTRY
*&---------------------------------------------------------------------*
FORM get_last_entry TABLES pp_data  STRUCTURE data_table
                    USING  pp_accum STRUCTURE data_table.
  LOOP AT pp_data WHERE
          box    = pp_accum-box    AND
          lgart  = pp_accum-lgart  AND
          abkrs  = pp_accum-abkrs  AND
          iabkrs = pp_accum-iabkrs AND
          bukrs  = pp_accum-bukrs  AND
          werks  = pp_accum-werks  AND
          btrtl  = pp_accum-btrtl  AND
          kostl  = pp_accum-kostl  AND
          persg  = pp_accum-persg  AND
          persk  = pp_accum-persk  AND
          vdsk1  = pp_accum-vdsk1  AND
          stat2  = pp_accum-stat2  AND
          txcmp  = pp_accum-txcmp  AND
          taxau  = pp_accum-taxau  AND
          taxty  = pp_accum-taxty  AND
          ipydt LE pp_accum-ipydt  AND
          btype  = 'N'.
  ENDLOOP.
  IF sy-subrc NE 0.
    CLEAR pp_data.
  ENDIF.
ENDFORM.                    " GET_LAST_ENTRY
*&---------------------------------------------------------------------*
*&      Form  SORT_OPTIONS_LOOP
*&---------------------------------------------------------------------*
"  Nt. 360625
*---------------------------------------------------------------------*
*       FORM SORT_OPTIONS_LOOP                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  L_DATA_TABLE                                                  *
*  -->  L_STRING_TMP                                                  *
*---------------------------------------------------------------------*
FORM sort_options_loop TABLES l_data_table STRUCTURE data_table    "
                       USING  l_string_tmp LIKE data_table-sort_strng.
  "
  LOOP AT sort_options.                                          "
    CASE sort_options-shortstrg. "Build Sort String              "
      WHEN 'A'.                    " BUKRS - Company Code        "
        CONCATENATE l_string_tmp l_data_table-bukrs '/'          "
        INTO l_string_tmp.                                       "
      WHEN 'B'.                    " WERKS - Personnel Area      "
        CONCATENATE l_string_tmp l_data_table-werks '/'          "
        INTO l_string_tmp.                                       "
      WHEN 'C'.                    " BTRTL - Personnel Subarea   "
        CONCATENATE l_string_tmp l_data_table-btrtl '/'          "
        INTO l_string_tmp.                                       "
      WHEN 'D'.                    " KOSTL - Cost Center         "
        CONCATENATE l_string_tmp l_data_table-kostl '/'          "
        INTO l_string_tmp.                                       "
      WHEN 'E'.                    " IABKRS - Payroll Area       "
        CONCATENATE l_string_tmp l_data_table-iabkrs '/'         "
        INTO l_string_tmp.                                       "
      WHEN 'F'.                    " PERSG - Employee Group      "
        CONCATENATE l_string_tmp l_data_table-persg '/'          "
        INTO l_string_tmp.                                       "
      WHEN 'G'.                    " PERSK - Employee Subgroup   "
        CONCATENATE l_string_tmp l_data_table-persk '/'          "
        INTO l_string_tmp.                                       "
      WHEN 'H'.                    " VDSK1 - Organizational Key  "
        CONCATENATE l_string_tmp l_data_table-vdsk1 '/'          "
        INTO l_string_tmp.                                       "
      WHEN 'I'.                    " STAT2 - Employement Status  "
        CONCATENATE l_string_tmp l_data_table-stat2 '/'          "
        INTO l_string_tmp.                                       "
      WHEN 'J'.                    " PERNR - Personnel Number    "
        CONCATENATE l_string_tmp l_data_table-pernr '/'          "
        INTO l_string_tmp.                                       "
      WHEN 'K'.                    " TXCMP - Tax Company         "
        CONCATENATE l_string_tmp l_data_table-txcmp '/'          "
        INTO l_string_tmp.                                       "
      WHEN 'L'.                    " TAXAU - Tax Authority       "
        CONCATENATE l_string_tmp l_data_table-taxau '/'          "
        INTO l_string_tmp.                                       "
    ENDCASE.                                                     "
  ENDLOOP.                                                       "
ENDFORM.                    " sort_options_loop                    "
*&---------------------------------------------------------------------*
*&      Form  cost_assign
*&---------------------------------------------------------------------*

FORM cost_assign.                                           "nt. 0397528
  DATA: l_total  LIKE c0-kprnn.          " tracking % of assigned totals
  DATA: l_betrg  LIKE data_table_tmp-betrg,
        l_anzhl  LIKE data_table_tmp-anzhl,
        l_sumb   LIKE data_table_tmp-betrg,
        l_suma   LIKE data_table_tmp-anzhl,
        l_deltab LIKE data_table_tmp-betrg,
        l_deltaa LIKE data_table_tmp-anzhl.

  CLEAR: l_betrg, l_anzhl, l_sumb, l_suma.
  l_betrg = data_table_tmp-betrg.
  l_anzhl = data_table_tmp-anzhl.
  IF rt-apznr = '00'.
    LOOP AT wpbp. ENDLOOP.
    rt-apznr = wpbp-apznr.
  ENDIF.
  IF rt-apznr <> '00' AND wpbp-kostvjn = 'X'
     AND rt-c1znr = '0000'.
    CLEAR g_acct_type.
    LOOP AT g_acct_type WHERE lgart EQ rt-lgart
                        AND   endda GE evp-paydt.
      EXIT.
    ENDLOOP.
    LOOP AT c0 WHERE apznr = rt-apznr.
      data_table_tmp-betrg = l_betrg * c0-kprnn / 100.
      data_table_tmp-anzhl = l_anzhl * c0-kprnn / 100.
      IF g_acct_type-koart(1) EQ 'C'.
        data_table_tmp-bukrs = c0-kbunn.
        data_table_tmp-kostl = c0-kstnn.
      ELSE.
        data_table_tmp-bukrs = wpbp-bukrs.
        data_table_tmp-kostl = wpbp-kostl.
      ENDIF.
      l_total = c0-kprnn + l_total.
      l_suma = data_table_tmp-anzhl + l_suma.
      l_sumb = data_table_tmp-betrg + l_sumb.
*-- to solve a rounding problem, the deltas are added to the last entry
*        AT LAST.
*          check l_total = 100.                             "nt-0409899
      IF l_total = 100.                                     "nt-0409899
        l_deltab = l_sumb - l_betrg.
        IF NOT l_deltab = 0.
          data_table_tmp-betrg = data_table_tmp-betrg - l_deltab.
        ENDIF.
        l_deltaa =  l_suma - l_anzhl.
        IF NOT l_deltaa = 0.
          data_table_tmp-anzhl = data_table_tmp-anzhl - l_deltaa.
        ENDIF.
      ENDIF.                                                "nt-0409899
*        ENDAT.
      PERFORM clear_not_used_fields USING data_table_tmp.
      PERFORM assign_data_to_tables.
    ENDLOOP.
*-- when costs are not fully assigned through C0 table in the cluster
    CHECK l_total < 100.
    data_table_tmp-betrg = l_betrg * ( 100 - l_total ) / 100.
    data_table_tmp-anzhl = l_anzhl * ( 100 - l_total ) / 100.
    data_table_tmp-bukrs = wpbp-bukrs.
    data_table_tmp-kostl = wpbp-kostl.
    l_suma = data_table_tmp-anzhl + l_suma.
    l_sumb = data_table_tmp-betrg + l_sumb.
*-- to solve a rounding problem, the deltas are added to the last entry
    l_deltab = l_sumb - l_betrg.
    IF NOT l_deltab = 0.
      data_table_tmp-betrg = data_table_tmp-betrg - l_deltab.
    ENDIF.
    l_deltaa =  l_suma - l_anzhl.
    IF NOT l_deltaa = 0.
      data_table_tmp-anzhl = data_table_tmp-anzhl - l_deltaa.
    ENDIF.
    PERFORM clear_not_used_fields USING data_table_tmp.
    PERFORM assign_data_to_tables.
    CLEAR l_total.
  ELSE.
    IF rt-c1znr <> '0000'.
      CLEAR g_acct_type.
      LOOP AT g_acct_type WHERE lgart EQ rt-lgart
                          AND   endda GE evp-paydt.
        EXIT.
      ENDLOOP.
      LOOP AT c1 WHERE c1znr = rt-c1znr.
        IF g_acct_type-koart(1) EQ 'C'.
          data_table_tmp-bukrs = c1-bukrs.
          data_table_tmp-kostl = c1-kostl.
        ELSE.
          data_table_tmp-bukrs = wpbp-bukrs.
          data_table_tmp-kostl = wpbp-kostl.
        ENDIF.
      ENDLOOP.
    ENDIF.
    PERFORM clear_not_used_fields USING data_table_tmp.
    PERFORM assign_data_to_tables.
  ENDIF.

ENDFORM.                    " COST_ASSIGN                   "nt. 0397528
