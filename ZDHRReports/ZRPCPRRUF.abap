*----------------------------------------------------------------------*
*   INCLUDE RPCPRRUF  - REPORT SPECIFIC FORMS                          *
*----------------------------------------------------------------------*
* ERP2005
* =======
* SAM869832 08/10/2005 REC: Tax authority selection not working
*                      correctly
* SAM850091 06/01/2005 MSC: Payroll reconciliation shows incorrect
*                      values
* nt.418921 07.13.2001 Duplicate WT values appear after a mid-period tax
*                      company change
* nt.420034 07.17.2001 Missing Retro Active Adjustments after applying
*                      Note 396886
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  S_LGART_VAL
*&---------------------------------------------------------------------*
*   Generates a Table with all Selected Wagetypes based on user
*   selection of report type and wagetypes.
*   The table s_lgart contains the wagetype selection from the
*   selection screen.  The rpt_typ contains the selected report type.
*
*   This form:
*  1) Reads Feature EVALC with current report type = (DEDU0 or ERNU0).
*  2) Loads s_itab with valid values for wagetype evaluation class 2
*     as defined in EVALC by user.
*  3) Reads T512W for wagetypes with eval class 2 equal to values that
*     are set in s_itab.
*  4) Checks wagetype to see if it is set in s_lgart.
*  5) loads the valid wagetype (lgart) into swgtyp.
*  6) Reads T512T for valid wagetype's text and loads lgtxt into
*     swgtyp.
*----------------------------------------------------------------------*
FORM s_lgart_val TABLES slv_swgtyp STRUCTURE swgtyp
                        slv_lgart STRUCTURE tmp_itab
                  USING slv_molga
                        slv_frmtyp
                        slv_enddt.
  IF slv_frmtyp = space.
    pme34-repid = 'CUSTOM'.
    PERFORM re549d USING 'EVALC' space subty_str rc.
    CLEAR itab. REFRESH itab.
    SPLIT subty_str AT '/' INTO TABLE itab.
    CLEAR s_itab. REFRESH s_itab.
    LOOP AT itab.
      MOVE itab-evalc TO s_itab-low.
      s_itab-sign = 'I'.
      s_itab-option = 'EQ'.
      APPEND s_itab.
    ENDLOOP.
  ENDIF.
  DESCRIBE TABLE slv_lgart LINES slgart_lines.
  CLEAR slv_swgtyp. REFRESH slv_swgtyp.
  IF slv_enddt IS INITIAL.
    slv_enddt = sy-datum.
  ENDIF.
  SELECT * FROM t512w WHERE molga = slv_molga
                          AND endda >= slv_enddt
                          AND begda <= slv_enddt.

    IF slv_frmtyp = space.
      CHECK t512w-aklas+2(2) IN s_itab.
    ENDIF.
    IF slgart_lines NE 0 AND t512w-lgart IN slv_lgart.
      CONTINUE.
    ENDIF.
    MOVE-CORRESPONDING t512w TO slv_swgtyp.
    SELECT SINGLE * FROM t512t WHERE sprsl = sy-langu
                                 AND molga = slv_molga
                                 AND lgart = t512w-lgart.
    MOVE t512t-lgtxt TO slv_swgtyp-lgtxt.
    slv_swgtyp-sumlg = t512w-lgart.
    COLLECT slv_swgtyp.
  ENDSELECT.

ENDFORM.                               " S_LGART_VAL
*&---------------------------------------------------------------------*
*&      Form  FIND_LGART_TXT
*&---------------------------------------------------------------------*
FORM find_lgart_txt.

  LOOP AT swgtyp WHERE lgart = wgtyp.
  ENDLOOP.
  lgart_txt = swgtyp-lgtxt.

ENDFORM.                               " FIND_LGART_TXT
*&---------------------------------------------------------------------*
*&      Form  PRINT_DATA_TABLE
*&---------------------------------------------------------------------*
*       Loops over the sorted data_table and checks to see
*  if the payarea, company code, or the wagetype has changed.  If there
*  is a change, the current wagetype total is printed at the bottom of
*  the current list and a new page with header is created.  If no change
*  is detected the current employee name, ID, and wagetype amounts are
*  listed.  If no records are found in data_table, a message is
*  generated and the report is exited.
*----------------------------------------------------------------------*
FORM print_data_table.
  DATA: data_tmp  LIKE data_table OCCURS 0 WITH HEADER LINE,
        data_table_tmp LIKE data_table_write OCCURS 0 WITH HEADER LINE.


  SORT data_table ASCENDING BY sort_strng btype pernr box ipydt
                               inper fpper.
  LOOP AT data_table.
    MOVE-CORRESPONDING data_table TO data_tmp.

    LOOP AT data_table WHERE iabkrs = data_tmp-iabkrs
                          AND bukrs = data_tmp-bukrs
                          AND werks = data_tmp-werks
                          AND btrtl = data_tmp-btrtl
                          AND kostl = data_tmp-kostl
                          AND persg = data_tmp-persg
                          AND persk = data_tmp-persk
                          AND vdsk1 = data_tmp-vdsk1
                          AND stat2 = data_tmp-stat2
                          AND txcmp = data_tmp-txcmp
                          AND taxau = data_tmp-taxau.
      MOVE-CORRESPONDING data_table TO data_table_tmp.
      IF NOT data_table_tmp-betrg IS INITIAL OR
         NOT data_table_tmp-anzhl IS INITIAL OR
         NOT data_table_tmp-betrg_y IS INITIAL OR
         NOT data_table_tmp-anzhl_y IS INITIAL OR
         NOT data_table_tmp-betrg_q IS INITIAL OR
         NOT data_table_tmp-anzhl_q IS INITIAL OR
         NOT data_table_tmp-betrg_m IS INITIAL OR
         NOT data_table_tmp-anzhl_m IS INITIAL.
        APPEND data_table_tmp.
      ENDIF.
      DELETE data_table.
    ENDLOOP.

    LOOP AT data_table_tmp.

      PERFORM print_sort_header TABLES data_table_tmp
                                       sort_options
                                 USING p_inf.

      LOOP AT data_table_tmp WHERE pernr = data_table_tmp-pernr.
        MOVE-CORRESPONDING data_table_tmp TO data_table_write.
        APPEND data_table_write.
        DELETE data_table_tmp.
      ENDLOOP.

      PERFORM print_data_out TABLES data_table_write
                                    sort_options
                                    swgtyp
                              USING p_form
                                    '10'
                                    p_beg
                                    p_end
                                    p_inf.

      CLEAR data_table_write. REFRESH data_table_write.

    ENDLOOP.
  ENDLOOP.
ENDFORM.                               " PRINT_DATA_TABLE
*&---------------------------------------------------------------------*
*&      Form  CONVERT_DATE_STR
*&---------------------------------------------------------------------*
FORM convert_date_str USING    cds_date
                      CHANGING cds_date_str.

  cds_date_str(2)   = cds_date+4(2).
  cds_date_str+2(1) = '/'.
  cds_date_str+3(2) = cds_date+6(2).
  cds_date_str+5(1) = '/'.
  cds_date_str+6(4) = cds_date(4).


ENDFORM.                               " CONVERT_DATE_STR
*&---------------------------------------------------------------------*
*&      Form  SET_SELECTION_MODE
*&---------------------------------------------------------------------*
FORM set_selection_mode.
  IF NOT p_paytyr[] IS INITIAL.
    p_offcyc = 'X'.
  ELSEIF p_offcyc = 'X'.
    p_paytyr-low = 'A'.
    p_paytyr-high = 'C'.
    p_paytyr-sign = 'I'.
    p_paytyr-option = 'BT'.
    APPEND p_paytyr.
  ENDIF.
ENDFORM.                               " SET_SELECTION_MODE
*&---------------------------------------------------------------------*
*&      Form  GET_PERIOD_DATES
*&---------------------------------------------------------------------*
FORM get_period_dates.
* Translate period into date range
* Precedence:
*   1. Date Range ( P_BEG and P_END )
*   2. Period     ( PNPPABRP & PNPPABRJ = P_PER )
  IF NOT pnpxabkr IS INITIAL.
    SELECT SINGLE * FROM t549a WHERE abkrs = pnpxabkr.

    SELECT SINGLE * FROM t549q WHERE permo EQ t549a-permo
*                              and pabrj eq PNPPABRJ       "note304334
*                              and pabrp eq PNPPABRP.      "note304334
                               AND pabrj EQ pn-pabrj        "note304334
                               AND pabrp EQ pn-pabrp.       "note304334

    SELECT SINGLE * FROM t549s WHERE molga EQ '10'
                               AND datmo EQ t549a-datmo
                               AND permo EQ t549q-permo
                               AND pabrj EQ t549q-pabrj
                               AND pabrp EQ t549q-pabrp
                               AND datid EQ '01'.
    p_beg = t549s-pdate.
    p_end = t549s-pdate.
*   concatenate PNPPABRJ PNPPABRP into p_per.               "note304334
    CONCATENATE pn-pabrj pn-pabrp INTO p_per.               "note304334
  ELSE.
*   p_beg = PNPBEGDA.                                       "note304334
*   p_end = PNPENDDA.                                       "note304334
    p_beg = pn-begda.                                       "note304334
    p_end = pn-endda.                                       "note304334

*<SAM850091>
    pn-pabrj = pn-endda(4).
*<SAM850091>
  ENDIF.

  IF p_beg IS INITIAL.
    MESSAGE i016(rp) WITH 'Date range is not valid'(E05).
    EXIT.
  ENDIF.
  p_asoc = p_end.
  p_asop = p_beg.

*<SAM850091>
*  pyyr  = p_end(4).
*<SAM850091>

  PERFORM convert_date_str USING    p_beg
                           CHANGING per_beg_str.
  PERFORM convert_date_str USING    p_end
                           CHANGING per_end_str.

  CONCATENATE per_beg_str
              space(1)
              '-'
              space(1)
              per_end_str  INTO period.

  CALL FUNCTION 'BUILD_PERIOD_TABLE'
    EXPORTING
      year         = pn-pabrj  "pyyr SAM850091
    TABLES
      period_table = p_range
    EXCEPTIONS
      OTHERS       = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CLEAR i_range.  REFRESH i_range.

  LOOP AT p_range WHERE per_id CA 'Q'.
    CHECK p_end >= p_range-begda AND
          p_beg <= p_range-endda.
    MOVE-CORRESPONDING p_range TO i_range.
    APPEND i_range.
  ENDLOOP.

  LOOP AT p_range WHERE per_id CA 'Y'.
    CHECK p_end >= p_range-begda AND
          p_beg <= p_range-endda.
    MOVE-CORRESPONDING p_range TO i_range.
    APPEND i_range.
  ENDLOOP.

  LOOP AT p_range WHERE per_id NA 'QY'.
    CHECK p_end >= p_range-begda AND
          p_beg <= p_range-endda.
    MOVE-CORRESPONDING p_range TO i_range.
    APPEND i_range.
  ENDLOOP.
  LOOP AT i_range WHERE per_id = 'Yr'.
    MOVE 'Yd' TO i_range-per_id.
    MOVE 'YTD PyAr'(306) TO i_range-period.
    APPEND i_range.
    EXIT.
  ENDLOOP.
ENDFORM.                               " GET_PERIOD_DATES

*&---------------------------------------------------------------------*
*&      Form  INIT
*----------------------------------------------------------------------*
*       general initialization                                         *
*----------------------------------------------------------------------*
FORM init.
  save_pnpabkrs = pnpabkrs.                                 "Note 489295
  save_pnpabkrs[] = pnpabkrs[].                             "Note 489295
  CLEAR pnpabkrs. REFRESH pnpabkrs.

  save_begps = pn-begda.                                    "Note 512368
  save_endps = pn-endda.                                    "Note 512368
  pn-begps = '18000101'.                                    "Note 512368
  pn-endps = '99991231'.                                    "Note 512368

  PERFORM set_selection_mode.
  PERFORM get_period_dates.
  PERFORM select_wage_types.
  PERFORM build_sort_option.
  PERFORM get_tax_rep_asof_dates.
  PERFORM set_flags_for_processing.
  PERFORM get_account_type.
  PERFORM get_employees_from_index_table.

*<SAM869832>
** Start NY wage reporting changes - Note 686069
** If Wage Reporting check is enabled for NY State and City of Yonkers
** and the reporting year is 2003 or higher
*  CHECK pn-pabrj >= 2003.  "pyyr   SAM850091
**<SAM850091>
**  CHECK p_w2 = 'X' OR p_sui = 'X'.
**<SAM850091>
*  IF NOT p_taxau[] IS INITIAL.
*    IF ( 'NY' IN p_taxau OR 'NY02' IN p_taxau ) AND
*       NOT 'FED' IN p_taxau.
*      g_fed_added = 'X'.
*      p_taxau-sign = 'I'.
*      p_taxau-option = 'EQ'.
*      p_taxau-low = 'FED'.
*      APPEND p_taxau.
*    ENDIF.
*  ENDIF.
*</SAM869832>

ENDFORM.                    "init

*&---------------------------------------------------------------------*
*&      Form  build_sort_option
*&---------------------------------------------------------------------*
FORM build_sort_option.
  DATA: s_field(20).
  DATA: option_tmp(100) VALUE ' '.
  option_tmp = option.
  REFRESH sel_tab_2.
  WHILE option NE space.
    READ TABLE sel_tab WITH KEY shortstrg = option_tmp+0(1).
    IF sy-subrc EQ 0.
      MOVE sel_tab TO sel_tab_2.
      APPEND sel_tab_2.
    ENDIF.
    SHIFT option UP TO '/'.
    SHIFT option BY 1 PLACES.
    option_tmp = option.
  ENDWHILE.
  sort_options[] = sel_tab_2[].
ENDFORM.                               " build_sort_option
*&---------------------------------------------------------------------*
*&      Form  SORT_DATA_TABLE
*&---------------------------------------------------------------------*
FORM sort_data_table TABLES   sdt_sort_accum STRUCTURE sort_table
                              sdt_sort_cp STRUCTURE sort_table
                              sdt_data_table STRUCTURE data_table
                              sdt_sort_options STRUCTURE pnpstringt
                              sdt_i_range STRUCTURE range_prds
                              sdt_sort_ppadj STRUCTURE sort_table
                              sdt_sort_cpadj STRUCTURE sort_table
                              sdt_sort_retro STRUCTURE sort_table
                      USING   sdt_p_inf
                              sdt_p_mtd
                              sdt_p_qtd
                              sdt_p_ytd.

  DATA: sort_data_cp LIKE sort_table OCCURS 0 WITH HEADER LINE,
        data_table_ppadj LIKE data_table OCCURS 0 WITH HEADER LINE,
        data_table_cpadj LIKE data_table OCCURS 0 WITH HEADER LINE,
        data_table_retro LIKE data_table OCCURS 0 WITH HEADER LINE.



  IF sdt_p_mtd = 'X' OR sdt_p_qtd = 'X' OR sdt_p_ytd = 'X'.

    PERFORM build_accum_values TABLES sdt_sort_cp     "<----IN
                                      sdt_sort_accum  "<----IN
                                      sort_data_cp    "---->OUT
                                      sdt_i_range
                               USING  sdt_p_mtd
                                      sdt_p_qtd
                                      sdt_p_ytd
                                      sdt_p_inf.

  ELSE.
    sort_data_cp[] = sdt_sort_cp[].
  ENDIF.
  CLEAR sdt_sort_cp. FREE sdt_sort_cp.


  PERFORM sort TABLES  sort_data_cp    "<---IN
                       sdt_data_table  "--->OUT
                       sdt_sort_options
                USING  sdt_p_inf.

  PERFORM sort TABLES  sdt_sort_retro  "<---IN
                       data_table_retro"--->OUT
                       sdt_sort_options
                USING  sdt_p_inf.

  LOOP AT data_table_retro.
    APPEND data_table_retro TO sdt_data_table.
  ENDLOOP.
  CLEAR data_table_retro.  FREE data_table_retro.

  PERFORM sort TABLES  sdt_sort_ppadj  "<---IN
                       data_table_ppadj"--->OUT
                       sdt_sort_options
                USING  sdt_p_inf.

  LOOP AT data_table_ppadj.
    APPEND data_table_ppadj TO sdt_data_table.
  ENDLOOP.
  CLEAR data_table_ppadj.  FREE data_table_ppadj.

  PERFORM sort TABLES  sdt_sort_cpadj  "<---IN
                       data_table_cpadj"--->OUT
                       sdt_sort_options
                USING  sdt_p_inf.

  LOOP AT data_table_cpadj.
    APPEND data_table_cpadj TO sdt_data_table.
  ENDLOOP.
  CLEAR data_table_cpadj.  FREE data_table_cpadj.

ENDFORM.                               " SORT_DATA_TABLE
*&---------------------------------------------------------------------*
*&      Form  print_data_out
*&---------------------------------------------------------------------*
FORM print_data_out TABLES pdo_data_table_out STRUCTURE data_table_write
                             pdo_sort_option  STRUCTURE sort_options
                             pdo_swgtyp       STRUCTURE swgtyp
                    USING    pdo_forms        LIKE t596a-appl
                             pdo_molga
                             pdo_begdt        LIKE pnpbegda
                             pdo_enddt        LIKE pnpendda
                             pdo_inf.

  DATA: nwpg(1) TYPE c VALUE 'Y',
        nwbx(1) TYPE c VALUE 'Y'.

  DESCRIBE TABLE pdo_data_table_out LINES dtable_lines.

  IF wtype1 = 'X'.            " Report Forms       NT. 0363102
    SELECT * FROM t596g WHERE molga = pdo_molga
                                  AND appl = pdo_forms.


      CLEAR: i_box, i_btype, i_pernr. REFRESH: i_box, i_btype, i_pernr.
      cntr = 0.
      total_betrg = 0.
      total_anzhl = 0.

      LOOP AT pdo_data_table_out WHERE box = t596g-sumlg.
        line_number = sy-linno + 7.

        IF line_number >= sy-linct AND dtable_lines > 1.
          NEW-PAGE.
          PERFORM print_rpt_header USING pdo_forms
                                         sy-pagno
                                         pdo_begdt
                                         pdo_enddt.

          PERFORM print_sort_header TABLES pdo_data_table_out
                                           pdo_sort_option
                                     USING pdo_inf.
          nwpg = 'Y'.
          nwbx = 'N'.

        ENDIF.

        PERFORM build_period_string TABLES pdo_data_table_out
                                  CHANGING prd_str.

        PERFORM convert_date_str USING pdo_data_table_out-ipydt
                               CHANGING paydt_str.

        SELECT SINGLE * FROM t596h WHERE sprsl = sy-langu
                                AND molga = pdo_molga
                                AND appl  = pdo_forms
                                AND sumlg = pdo_data_table_out-box.
        box_name = t596h-sumlg.
        box_text = t596h-sumtx.

        SET EXTENDED CHECK OFF.

        AT NEW box.
          READ TABLE i_box WITH KEY box = pdo_data_table_out-box.
          IF sy-subrc <> 0.
            WRITE:/.
            FORMAT COLOR 2.
            ULINE.
            WRITE:/'|',
                    2 box_name ,
                    7 box_text,
                  132 '|'.
            FORMAT COLOR OFF.
            ULINE.

            i_box-box = pdo_data_table_out-box.
            APPEND i_box.
            CLEAR: i_btype, i_pernr. REFRESH: i_btype, i_pernr.
            nwbx = 'Y'.
          ENDIF.
        ENDAT.

        IF nwpg = 'Y' AND nwbx = 'N'.
          WRITE:/.
          FORMAT COLOR 2.
          ULINE.
          WRITE:/'|',
                  2 box_name ,
                  7 'Continued...'(311),
                132 '|'.
          FORMAT COLOR OFF.
          ULINE.
        ENDIF.
        nwpg = 'N'.
        PERFORM print_data TABLES   pdo_data_table_out
                                    pdo_sort_option
                                    i_box
                                    i_btype
                                    i_pernr
                           USING    prd_str
                                    paydt_str
                                    pdo_begdt
                                    pdo_enddt
                                    pdo_forms
                                    dtable_lines
                           CHANGING cntr
                                    total_betrg
                                    total_anzhl
                                    sub_total_betrg
                                    sub_total_anzhl.
        AT END OF box.
          IF pdo_data_table_out-btype CA 'NRT'.
            FORMAT COLOR 3.
            ULINE.
            WRITE:/'|',
                   5 'TOTALS:',
                   32 '|',
                   34(7) sub_total_anzhl NO-ZERO,
                  43(13) sub_total_betrg NO-ZERO CURRENCY rp_curr,'|',
                  132 '|'.
            ULINE.
            sub_total_betrg = 0.
            sub_total_anzhl = 0.
            FORMAT COLOR OFF.
            cntr = cntr + 1.
          ELSE.
            ULINE.
          ENDIF.
        ENDAT.
      ENDLOOP.
      IF cntr > 1.
        FORMAT COLOR 3.
        WRITE:/'|',
               5 'GROUP TOTALS:',
               32 '|',
               34(7) total_anzhl NO-ZERO,
               43(13) total_betrg NO-ZERO CURRENCY rp_curr,'|',
              132 '|'.
        ULINE.
        total_betrg = 0.
        total_anzhl = 0.
        FORMAT COLOR OFF.
        cntr = 0.
      ENDIF.


    ENDSELECT.

  ELSE.
    LOOP AT pdo_swgtyp.

      CLEAR: i_box, i_btype, i_pernr. REFRESH: i_box, i_btype, i_pernr.
      cntr = 0.
      total_betrg = 0.
      total_anzhl = 0.

      LOOP AT pdo_data_table_out WHERE box = pdo_swgtyp-sumlg.
        line_number = sy-linno + 5.

        IF line_number >= sy-linct AND dtable_lines > 1.
          NEW-PAGE.
          PERFORM print_rpt_header USING pdo_forms
                                         sy-pagno
                                         pdo_begdt
                                         pdo_enddt.

          PERFORM print_sort_header TABLES pdo_data_table_out
                                           pdo_sort_option
                                     USING pdo_inf.
          nwpg = 'Y'.
          nwbx = 'N'.
        ENDIF.

        PERFORM build_period_string TABLES pdo_data_table_out
                                  CHANGING prd_str.

        PERFORM convert_date_str USING pdo_data_table_out-ipydt
                               CHANGING paydt_str.

        box_name = pdo_swgtyp-lgart.
        box_text = pdo_swgtyp-lgtxt.

        AT NEW box.
          READ TABLE i_box WITH KEY box = pdo_data_table_out-box.
          IF sy-subrc <> 0.
            WRITE:/.
            FORMAT COLOR 2.
            ULINE.
            WRITE:/'|',
                    2 box_name ,
                    7 box_text,
                  132 '|'.
            FORMAT COLOR OFF.
            ULINE.

            i_box-box = pdo_data_table_out-box.
            APPEND i_box.
            CLEAR: i_btype, i_pernr. REFRESH: i_btype, i_pernr.
            nwbx = 'Y'.
          ENDIF.
        ENDAT.

        IF nwpg = 'Y' AND nwbx = 'N'.
          WRITE:/.
          FORMAT COLOR 2.
          ULINE.
          WRITE:/'|',
                  2 box_name ,
                  7 'Continued...'(311),
                132 '|'.
          FORMAT COLOR OFF.
          ULINE.
        ENDIF.
        nwpg = 'N'.
        PERFORM print_data TABLES   pdo_data_table_out
                                    pdo_sort_option
                                    i_box
                                    i_btype
                                    i_pernr
                           USING    prd_str
                                    paydt_str
                                    pdo_begdt
                                    pdo_enddt
                                    pdo_forms
                                    dtable_lines
                           CHANGING cntr
                                    total_betrg
                                    total_anzhl
                                    sub_total_betrg
                                    sub_total_anzhl.
        AT END OF box.
          IF pdo_data_table_out-btype CA 'NRT'.
            FORMAT COLOR 3.
            ULINE.
            WRITE:/'|',
                   5 'TOTALS:',
                   32 '|',
                   34(7) sub_total_anzhl NO-ZERO,
                  43(13) sub_total_betrg NO-ZERO CURRENCY rp_curr,'|',
                  132 '|'.
            ULINE.
            sub_total_betrg = 0.
            sub_total_anzhl = 0.
            FORMAT COLOR OFF.
            cntr = cntr + 1.
          ELSE.
            ULINE.
          ENDIF.
        ENDAT.
        SET EXTENDED CHECK ON.
        dtable_lines = dtable_lines - 1.
      ENDLOOP.
      IF cntr > 1.
        FORMAT COLOR 3.
        WRITE:/'|',
               5 'GROUP TOTALS:',
               32 '|',
               34(7) total_anzhl NO-ZERO,
               43(13) total_betrg NO-ZERO CURRENCY rp_curr,'|',
              132 '|'.
        ULINE.
        total_betrg = 0.
        total_anzhl = 0.
        FORMAT COLOR OFF.
        cntr = 0.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                               " print_data_out
*&---------------------------------------------------------------------*
*&      Form  SORT
*&---------------------------------------------------------------------*
FORM sort TABLES   s_sort_data    STRUCTURE sort_table
                   s_data_table   STRUCTURE data_table
                   s_sort_options STRUCTURE sort_options
           USING   s_p_inf.

  SORT s_sort_data ASCENDING BY atype
                                taxau
                                sort_strng
                                box
                                paydt
                                pernr.

  LOOP AT s_sort_data.
    IF s_p_inf <> 'X'.
      MOVE space TO s_sort_data-pernr.
    ENDIF.
    MOVE-CORRESPONDING s_sort_data  TO s_data_table.
    s_data_table-taxty = space.
    CLEAR d_itab. REFRESH d_itab.
    SPLIT s_sort_data-sort_strng AT '/' INTO TABLE d_itab.
    tab_indx = 0.
    LOOP AT d_itab.
      tab_indx = tab_indx + 1.
      READ TABLE s_sort_options INDEX tab_indx.

      CASE s_sort_options-shortstrg.
        WHEN 'A'.                      " BUKRS - Company Code
          MOVE d_itab-sort_value TO s_data_table-bukrs.
        WHEN 'B'.                      " WERKS - Personnel Area
          MOVE d_itab-sort_value TO s_data_table-werks.
        WHEN 'C'.                      " BTRTL - Personnel Subarea
          MOVE d_itab-sort_value TO s_data_table-btrtl.
        WHEN 'D'.                      " KOSTL - Cost Center
          MOVE d_itab-sort_value TO s_data_table-kostl.
        WHEN 'E'.                      " iabkrs - Payroll Area
          MOVE d_itab-sort_value TO s_data_table-iabkrs.
        WHEN 'F'.                      " PERSG - Employee Group
          MOVE d_itab-sort_value TO s_data_table-persg.
        WHEN 'G'.                      " PERSK - Employee Subgroup
          MOVE d_itab-sort_value TO s_data_table-persk.
        WHEN 'H'.                      " VDSK1 - Organizational Key
          MOVE d_itab-sort_value TO s_data_table-vdsk1.
        WHEN 'I'.                      " STAT2 - Employement Status
          MOVE d_itab-sort_value TO s_data_table-stat2.
        WHEN 'J'.                      " PERNR - Personnel Number
          MOVE d_itab-sort_value TO s_data_table-pernr.
        WHEN 'K'.                      " TXCMP - Tax Company
          MOVE d_itab-sort_value TO s_data_table-txcmp.
        WHEN 'L'.                      " TAXAU - Tax Authority
          MOVE d_itab-sort_value TO s_data_table-taxau.
      ENDCASE.
    ENDLOOP.
    COLLECT s_data_table.
  ENDLOOP.
  CLEAR s_sort_data.  FREE s_sort_data.
ENDFORM.                               " SORT

*&---------------------------------------------------------------------*
*&      Form  PRINT_DATA
*&---------------------------------------------------------------------*
FORM print_data  TABLES   pd_data_table_out STRUCTURE data_table_write
                          pd_sort_option    STRUCTURE pnpstringt
                          pd_box            STRUCTURE i_box
                          pd_btype          STRUCTURE i_btype
                          pd_pernr          STRUCTURE i_pernr
                 USING    pd_prd_str        LIKE prd_str
                          pd_paydt_str      LIKE paydt_str
                          pd_begdt          LIKE pnpbegda
                          pd_enddt          LIKE pnpendda
                          pd_forms          LIKE t596a-appl
                          pd_dtable_lines   LIKE dtable_lines
                 CHANGING pd_cntr           LIKE cntr
                          pd_total_betrg    LIKE data_table-betrg
                          pd_total_anzhl    LIKE data_table-anzhl
                          pd_sub_total_betrg LIKE data_table-betrg
                          pd_sub_total_anzhl LIKE data_table-anzhl.

  DATA: line_number LIKE sy-linno.
*----------------------------------------------------------------------*
*               At New Box print header.                               *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                      At New Type                                     *
*      A= Prior Period adjustments,  C= Current Period Adjustments     *
*      R= Retro Period Adjustments,  N= Normal Period Results          *
*----------------------------------------------------------------------*
*      A= Prior Period adjustments
  IF pd_data_table_out-btype = 'A'.
    READ TABLE pd_btype WITH KEY btype = pd_data_table_out-btype.
    IF sy-subrc <> 0.
      FORMAT COLOR 2 INVERSE ON.
      WRITE:/'|  PRIOR PERIOD ADJUSTMENTS      ',
             54 '|    AMOUNT OF ADJUSTMENT    |',
             132 '|'.
      ULINE.
      FORMAT COLOR OFF INVERSE OFF.
      pd_btype-btype = pd_data_table_out-btype.
      APPEND pd_btype.
      CLEAR pd_pernr. REFRESH pd_pernr.
    ENDIF.
    READ TABLE pd_pernr WITH KEY pernr = pd_data_table_out-pernr.
    IF sy-subrc <> 0 AND NOT ( pd_data_table_out-pernr IS INITIAL ).
      FORMAT COLOR 4.
      WRITE:/ '|',pd_data_table_out-pernr NO-ZERO,
             20 '|', ' ********** ',
             32 '|',
            132 '|'.
      FORMAT COLOR OFF.
      pd_pernr-pernr = pd_data_table_out-pernr.
      APPEND pd_pernr.
    ENDIF.
    WRITE:/'|',
           5 pd_prd_str,
           20 '|',pd_paydt_str,
           32 '|',
           55(7) pd_data_table_out-anzhl NO-ZERO,
           62(13)  pd_data_table_out-betrg NO-ZERO
                                                 CURRENCY rp_curr,
           132 '|'.

*   C= Current Period Adjustments
  ELSEIF pd_data_table_out-btype = 'T'.
    READ TABLE pd_btype WITH KEY btype = pd_data_table_out-btype.
    IF sy-subrc <> 0.
      FORMAT COLOR 2 INVERSE ON.
      WRITE:/'|  TAX REPORTER ADJUSTMENTS     ',
             32 '|--AMOUNT OF ADJUSTMENT--|'(350),
            132 '|'.
      ULINE.
      FORMAT COLOR OFF INVERSE OFF.
      pd_btype-btype = pd_data_table_out-btype.
      APPEND pd_btype.
      CLEAR pd_pernr. REFRESH pd_pernr.
    ENDIF.
    READ TABLE pd_pernr WITH KEY pernr = pd_data_table_out-pernr.
    IF sy-subrc <> 0 AND NOT ( pd_data_table_out-pernr IS INITIAL ).
      FORMAT COLOR 4.
      WRITE:/ '|',pd_data_table_out-pernr NO-ZERO,
             20 '|', ' ********** ',
             32 '|',
            132 '|'.
      FORMAT COLOR OFF.
      pd_pernr-pernr = pd_data_table_out-pernr.
      APPEND pd_pernr.
    ENDIF.
    WRITE:/'|',
           5 pd_prd_str,
           20 '|',pd_paydt_str,
           32 '|',
           34(7) pd_data_table_out-anzhl NO-ZERO,
           43(13)  pd_data_table_out-betrg NO-ZERO
                                                 CURRENCY rp_curr,
           57  '|',
           132 '|'.
*  N= Normal Period Results
  ELSEIF pd_data_table_out-btype = 'N'.
    READ TABLE pd_btype WITH KEY btype = pd_data_table_out-btype.
    IF sy-subrc <> 0.
      FORMAT COLOR 2 INVERSE ON.
      WRITE:/'|  CURRENT PERIOD RESULTS       ',
             32 '|-----Current Period-----|'(300),
             57 '|--------- M-T-D---------|'(301),
             82 '|--------- Q-T-D---------|'(302),
            107 '|--------- Y-T-D---------|'(303),
            132 '|'.
      ULINE.
      FORMAT COLOR OFF INVERSE OFF.
      pd_btype-btype = pd_data_table_out-btype.
      APPEND pd_btype.
      CLEAR pd_pernr. REFRESH pd_pernr.
    ENDIF.
    READ TABLE pd_pernr WITH KEY pernr = pd_data_table_out-pernr.
    IF sy-subrc <> 0 AND NOT ( pd_data_table_out-pernr IS INITIAL ).
      FORMAT COLOR 4.
      WRITE:/ '|',pd_data_table_out-pernr NO-ZERO,
             20 '|', ' ********** ',
             32 '|',
             57 '|',
             82 '|',
            107 '|',
            132 '|'.
      FORMAT COLOR OFF.
      pd_pernr-pernr = pd_data_table_out-pernr.
      APPEND pd_pernr.
    ENDIF.
    WRITE:/'|',
            5 pd_prd_str,
            20 '|',pd_paydt_str,
            32 '|',
            34(7) pd_data_table_out-anzhl NO-ZERO,
            43(13) pd_data_table_out-betrg NO-ZERO
                                                 CURRENCY rp_curr,
            57 '|',
            59(7) pd_data_table_out-anzhl_m NO-ZERO,
            68(13) pd_data_table_out-betrg_m NO-ZERO
                                                 CURRENCY rp_curr,
            82 '|',
            84(7) pd_data_table_out-anzhl_q NO-ZERO,
            92(13) pd_data_table_out-betrg_q NO-ZERO
                                                 CURRENCY rp_curr,
            107 '|',
            109(7) pd_data_table_out-anzhl_y NO-ZERO,
            118(13) pd_data_table_out-betrg_y NO-ZERO
                                                 CURRENCY rp_curr,
            132 '|'.

*      R= Retro Period Adjustments
  ELSEIF pd_data_table_out-btype = 'R'.
    READ TABLE pd_btype WITH KEY btype = pd_data_table_out-btype.
    IF sy-subrc <> 0.
      FORMAT COLOR 2 INVERSE ON.
      WRITE:/'|   RETRO ACTIVE ADJUSTMENTS      ',
             32 '|-----Current Period-----|'(300),
            132 '|'.
      ULINE.
      FORMAT COLOR OFF INVERSE OFF.
      pd_btype-btype = pd_data_table_out-btype.
      APPEND pd_btype.
      CLEAR pd_pernr. REFRESH pd_pernr.
    ENDIF.
    READ TABLE pd_pernr WITH KEY pernr = pd_data_table_out-pernr.
    IF sy-subrc <> 0 AND NOT ( pd_data_table_out-pernr IS INITIAL ).
      FORMAT COLOR 4.
      WRITE:/ '|',pd_data_table_out-pernr NO-ZERO,
        20 '|', ' ********** ',
        32 '|',
        57 '|',
       132 '|'.
      FORMAT COLOR OFF.
      pd_pernr-pernr = pd_data_table_out-pernr.
      APPEND pd_pernr.
    ENDIF.
    WRITE:/'|',
            5 pd_prd_str,
            20 '|', pd_paydt_str,
            32 '|',
            34(7) pd_data_table_out-anzhl NO-ZERO,
            43(13) pd_data_table_out-betrg NO-ZERO
                                                 CURRENCY rp_curr,
            57 '|',
           132 '|'.
  ENDIF.
  IF pd_data_table_out-btype CA 'NRT'.
    pd_sub_total_betrg = sub_total_betrg + pd_data_table_out-betrg.
    pd_sub_total_anzhl = sub_total_anzhl + pd_data_table_out-anzhl.
    pd_total_betrg = pd_total_betrg + pd_data_table_out-betrg.
    pd_total_anzhl = pd_total_anzhl + pd_data_table_out-anzhl.
  ENDIF.
ENDFORM.                               " PRINT_DATA
*&---------------------------------------------------------------------*
*&      Form  RE5UT7
*&---------------------------------------------------------------------*
FORM re5ut7 TABLES   p_txfrm_itab STRUCTURE trfrm_itab
            USING    p_tfrcl_tmp  LIKE t5ut7-tfrcl.
  SELECT * FROM t5ut7 WHERE molga = '10'
                        AND tfrcl = p_tfrcl_tmp
                        AND endda >= p_end
                        AND begda <= p_end.
    p_txfrm_itab-txfrm = t5ut7-txfrm.
    APPEND p_txfrm_itab.
  ENDSELECT.
ENDFORM.                                                    " RE5UT7

*&---------------------------------------------------------------------*
*&      Form  RE5UT8
*&---------------------------------------------------------------------*
FORM re5ut8 TABLES p_txfrm_itab STRUCTURE trfrm_itab
            USING  p_tfrcl_tmp  LIKE t5ut7-tfrcl.
  SELECT * FROM t5ut8 WHERE molga = '10'
                        AND tfrcl = p_tfrcl_tmp.
    READ TABLE p_txfrm_itab WITH KEY txfrm = t5ut8-txfrm.
    IF sy-subrc = 0 AND
       NOT ( t5ut8-csfrm IS INITIAL ).
      LOOP AT p_txfrm_itab WHERE txfrm = t5ut8-txfrm.
        p_txfrm_itab-txfrm = t5ut8-csfrm.
        APPEND p_txfrm_itab.
      ENDLOOP.
    ELSE.
      IF sy-subrc NE 0.
        p_txfrm_itab-txfrm  = t5ut8-txfrm.
        APPEND p_txfrm_itab.
      ENDIF.
    ENDIF.
  ENDSELECT.
ENDFORM.                                                    " RE5UT8
*&---------------------------------------------------------------------*
*&      Form  SORT_TOTALS_TABLE
*&---------------------------------------------------------------------*
FORM sort_totals_tables TABLES stt_srt_retro STRUCTURE total_srt_table
                               stt_srt_cpadj STRUCTURE total_srt_table
                               stt_srt_cp    STRUCTURE total_srt_table
                               stt_srt_ppadj STRUCTURE total_srt_table
                               stt_sort_options STRUCTURE sort_options
                               stt_table     STRUCTURE totals_table.
  PERFORM sort_srt_tbls TABLES stt_srt_cp       "<-----IN
                               stt_sort_options "<-----IN
                               stt_table.       "----->OUT

  CLEAR stt_srt_cp.  FREE stt_srt_cp.

  PERFORM sort_srt_tbls TABLES stt_srt_retro    "<-----IN
                               stt_sort_options "<-----IN
                               stt_table.       "----->OUT

  CLEAR stt_srt_retro.  FREE stt_srt_retro.

  PERFORM sort_srt_tbls TABLES stt_srt_ppadj    "<-----IN
                               stt_sort_options "<-----IN
                               stt_table.       "----->OUT

  CLEAR stt_srt_ppadj.  FREE stt_srt_ppadj.

  PERFORM sort_srt_tbls TABLES stt_srt_cpadj    "<-----IN
                               stt_sort_options "<-----IN
                               stt_table.       "----->OUT

  SORT stt_table BY btype
                    sort_strng
                    box
                    pernr.
  CLEAR stt_srt_cpadj.  FREE stt_srt_cpadj.

ENDFORM.                               " SORT_TOTALS_TABLE
*&---------------------------------------------------------------------*
*&      Form  SORT_SRT_TBLS
*&---------------------------------------------------------------------*
FORM sort_srt_tbls TABLES   sst_srt_table    STRUCTURE total_srt_table
                            sst_sort_options STRUCTURE sort_options
                            sst_tls_table    STRUCTURE totals_table.

  SORT sst_srt_table ASCENDING BY sort_strng
                                  box
                                  btype
                                  pernr.
  LOOP AT sst_srt_table.
    MOVE-CORRESPONDING sst_srt_table TO sst_tls_table.
    CLEAR d_itab. REFRESH d_itab.
    SPLIT sst_srt_table-sort_strng AT '/' INTO TABLE d_itab.
    tab_indx = 0.
    IF sst_srt_table-btype = 'R'.
      sst_tls_table-btype = 'N'.
    ENDIF.
    LOOP AT d_itab.
      tab_indx = tab_indx + 1.
      READ TABLE sst_sort_options INDEX tab_indx.

      CASE sst_sort_options-shortstrg.
        WHEN 'A'.                      " BUKRS - Company Code
          MOVE d_itab-sort_value TO sst_tls_table-bukrs.
        WHEN 'B'.                      " WERKS - Personnel Area
          MOVE d_itab-sort_value TO sst_tls_table-werks.
        WHEN 'C'.                      " BTRTL - Personnel Subarea
          MOVE d_itab-sort_value TO sst_tls_table-btrtl.
        WHEN 'D'.                      " KOSTL - Cost Center
          MOVE d_itab-sort_value TO sst_tls_table-kostl.
        WHEN 'E'.                      " iabkrs - Payroll Area
          MOVE d_itab-sort_value TO sst_tls_table-iabkrs.
        WHEN 'F'.                      " PERSG - Employee Group
          MOVE d_itab-sort_value TO sst_tls_table-persg.
        WHEN 'G'.                      " PERSK - Employee Subgroup
          MOVE d_itab-sort_value TO sst_tls_table-persk.
        WHEN 'H'.                      " VDSK1 - Organizational Key
          MOVE d_itab-sort_value TO sst_tls_table-vdsk1.
        WHEN 'I'.                      " STAT2 - Employement Status
          MOVE d_itab-sort_value TO sst_tls_table-stat2.
        WHEN 'K'.                      " TXCMP - Tax Company
          MOVE d_itab-sort_value TO sst_tls_table-txcmp.
        WHEN 'L'.                      " TAXAU - Tax Authority
          MOVE d_itab-sort_value TO sst_tls_table-taxau.
      ENDCASE.
    ENDLOOP.
    COLLECT sst_tls_table.
    IF sst_srt_table-btype = 'T'.
      sst_tls_table-btype = 'N'.
      COLLECT sst_tls_table.
    ENDIF.
  ENDLOOP.

ENDFORM.                               " SORT_SRT_TBLS
*&---------------------------------------------------------------------*
*&      Form  PRINT_TOTALS_TABLE
*&---------------------------------------------------------------------*
FORM print_totals_table TABLES ptt_totals_table STRUCTURE totals_table
                               ptt_sort_options STRUCTURE sort_options
                               ptt_swgtyp STRUCTURE swgtyp
                        USING  ptt_forms
                               ptt_molga
                               ptt_begdt
                               ptt_enddt.

  cntr = 11.
  PERFORM print_total_data TABLES ptt_totals_table
                           USING  'N'
                                  ptt_forms
                                  ptt_molga
                                  ptt_begdt
                                  ptt_enddt
                        CHANGING  cntr.
  READ TABLE ptt_totals_table WITH KEY btype = 'T'.
  IF sy-subrc = 0.
    PERFORM print_total_data TABLES ptt_totals_table
                             USING  'T'
                                    ptt_forms
                                    ptt_molga
                                    ptt_begdt
                                    ptt_enddt
                          CHANGING  cntr.
  ENDIF.
  READ TABLE ptt_totals_table WITH KEY btype = 'A'.
  IF sy-subrc = 0.
    PERFORM print_total_data TABLES ptt_totals_table
                             USING  'A'
                                    ptt_forms
                                    ptt_molga
                                    ptt_begdt
                                    ptt_enddt
                          CHANGING  cntr.
  ENDIF.
ENDFORM.                               " PRINT_TOTALS_TABLE
*&---------------------------------------------------------------------*
*&      Form  TOTALS_HEADER
*&---------------------------------------------------------------------*
FORM totals_header USING th_btype LIKE totals_table-btype.
  FORMAT COLOR 3 ON.
  ULINE.
  IF th_btype = 'A'.
    WRITE:/49 'TOTALS (Prior period adjustments)'(417),
          132''.
  ELSEIF th_btype = 'T'.
    WRITE:/49 'TOTALS (Tax Reporter adjustments)'(418),
          132''.
  ELSE.
    WRITE:/44 'TOTALS (Curr. Period + Retro Adj. + TaxRep Adj.)'(402),
          132''.
  ENDIF.
  ULINE.
  WRITE:/.
  FORMAT COLOR 4 ON.
  WRITE:/2'WtBkt'(403),
          8'CoCd'(404),
         13'TxCo'(405),
         18'PyAr'(406),
         23'TxAu'(407),
         28'PA'(408),
         33'PSArea'(409),
         40'Cost ctr'(410),
         51'EG'(411),
         54'ESGrp'(412),
         60'Org. key'(413),
         76'Emply ID'(419),                                 "note 193315
         86'Amount'(414),                                   "note 193315
        107'Number'(415),                                   "note 193315
        132''.
  ULINE.
  FORMAT COLOR OFF.

ENDFORM.                               " TOTALS_HEADER
*&---------------------------------------------------------------------*
*&      Form  PRINT_TOTAL_DATA
*&---------------------------------------------------------------------*
FORM print_total_data TABLES   ptd_totals_table STRUCTURE totals_table
                      USING    btype     LIKE totals_table-btype
                               ptd_forms
                               ptd_molga LIKE rp_molga
                               ptd_begdt LIKE pnpbegda
                               ptd_enddt LIKE pnpendda
                   CHANGING    ptd_cntr  LIKE cntr.

  DATA: toggle TYPE i VALUE 0,
        box_tmp LIKE t596i-sumlg.
  DATA: sum_bx LIKE totals_table-betrg.
  DATA: anz_bx LIKE totals_table-anzhl.

  CLEAR box_tmp.
  b_type = btype.
  NEW-PAGE.

  LOOP AT ptd_totals_table.
    CHECK ptd_totals_table-btype = btype.
    FORMAT INTENSIFIED OFF.
    IF ptd_totals_table-box <> box_tmp.
      box_tmp = ptd_totals_table-box.
      WRITE:/.
      toggle = 0.
    ENDIF.

    sum_bx = sum_bx + ptd_totals_table-betrg.
    anz_bx = anz_bx + ptd_totals_table-anzhl.

    IF toggle = 0.
      toggle = 1.
      FORMAT COLOR 5 ON.
    ELSE.
      toggle = 0.
      FORMAT COLOR OFF.
    ENDIF.

    WRITE:/2 ptd_totals_table-box,
           8 ptd_totals_table-bukrs,
           13 ptd_totals_table-txcmp,
           18 ptd_totals_table-iabkrs,
           23 ptd_totals_table-taxau,
           28 ptd_totals_table-werks,
           33 ptd_totals_table-btrtl,
           40 ptd_totals_table-kostl,
           51 ptd_totals_table-persg,
           54 ptd_totals_table-persk,
           60 ptd_totals_table-vdsk1,
           76 ptd_totals_table-pernr NO-ZERO,
         86 ptd_totals_table-betrg NO-ZERO CURRENCY rp_curr,
        107 ptd_totals_table-anzhl NO-ZERO,
        132''.
    AT END OF taxau.
      FORMAT: COLOR 3 ON, INTENSIFIED OFF.
      WRITE:/2 ptd_totals_table-box,
             8 ptd_totals_table-bukrs,
            13 ptd_totals_table-txcmp,
            18 ptd_totals_table-iabkrs,
            23 ptd_totals_table-taxau,
            86 sum_bx NO-ZERO CURRENCY rp_curr,
           107 anz_bx NO-ZERO,
           132''.
      toggle = 0.
      CLEAR: sum_bx, anz_bx.
    ENDAT.
  ENDLOOP.
ENDFORM.                    " PRINT_TOTAL_DATA

*&---------------------------------------------------------------------*
*&      Form  init_tables_per_ee
*&---------------------------------------------------------------------*
FORM init_tables_per_ee.
  rp-init-buffer.
  CLEAR: rgdir, evp, evp_cp, evp_ppadj, evp_cpadj.
  REFRESH: rgdir, evp, evp_cp, evp_ppadj, evp_cpadj.
  CLEAR sort_tmp_retro.   FREE sort_tmp_retro.
  CLEAR sort_accum_cp.    FREE sort_accum_cp.
  CLEAR sort_tmp_cp.      FREE sort_tmp_cp.
  CLEAR sort_tmp_ppadj.   FREE sort_tmp_ppadj.
  CLEAR sort_tmp_cpadj.   FREE sort_tmp_cpadj.
  CLEAR sort_tmp_retro.   FREE sort_tmp_retro.
  CLEAR data_table_cp.    FREE data_table_cp.
  CLEAR data_table_ppadj. FREE data_table_ppadj.
  CLEAR data_table_cpadj. FREE data_table_cpadj.
  CLEAR data_table_retro. FREE data_table_retro.
  IF NOT p_inf IS INITIAL.
    CLEAR data_table_accum. FREE data_table_accum.
    CLEAR data_table_o_acc. FREE data_table_o_acc.
  ENDIF.
  IF pnpabkrs[] IS INITIAL.                                 "Note 489295
    pnpabkrs[] = save_pnpabkrs[].                           "Note 489295
    pnpabkrs = save_pnpabkrs.                               "Note 489295
  ENDIF.                                                    "Note 489295
ENDFORM.                    " init_tables_per_ee
*&---------------------------------------------------------------------*
*&      Form  get_payroll_results
*&---------------------------------------------------------------------*
FORM get_payroll_results.
  DATA: molga LIKE versc-molga VALUE '10',
        l_year_begda LIKE pn-begda,
        sw_retro.

  DATA: BEGIN OF rgdir_a OCCURS 10.
          INCLUDE STRUCTURE pc261a.
  DATA: END OF rgdir_a.
  DATA: rgdir_c LIKE rgdir OCCURS 100 WITH HEADER LINE.
  DATA: arc_groups LIKE t51b3 OCCURS 10 WITH HEADER LINE.

  REFRESH rgdir.

* if user has selected to process archived payroll results also.
  IF p_arch EQ 'X'.
*   Read archived payroll results.
    CALL FUNCTION 'CA_READ_RGDIR'
      EXPORTING
        persnr          = pernr-pernr
      IMPORTING
        molga           = molga
      TABLES
        ca_rgdir        = rgdir_a
      EXCEPTIONS
        no_record_found = 1
        OTHERS          = 2.

    IF sy-subrc EQ 0.
      rgdir[] = rgdir_a[].
      LOOP AT rgdir_a WHERE NOT arc_group IS INITIAL.
        MOVE rgdir_a-arc_group TO arc_groups-arc_group.
        COLLECT arc_groups.
      ENDLOOP.

      CALL FUNCTION 'RP_ARCHIVE_READ_ARCHIVE_CALC'
        EXPORTING
          arc_pernr             = pernr-pernr
        TABLES
          buffer                = tbuff
          buffer_directory      = buffer_dir
          arc_groups            = arc_groups
        EXCEPTIONS
          error_reading_archive = 1
          OTHERS                = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.
  ENDIF.
  CALL FUNCTION 'CU_READ_RGDIR'
    EXPORTING
      persnr          = pernr-pernr
    IMPORTING
      molga           = molga
    TABLES
      in_rgdir        = rgdir_c
    EXCEPTIONS
      no_record_found = 1
      OTHERS          = 2.

  IF sy-subrc EQ 0.
    APPEND LINES OF rgdir_c TO rgdir.
  ENDIF.

  IF p_last = 'X'.
    LOOP AT rgdir WHERE paydt  LE p_end
                  AND   iabkrs IN pnpabkrs.
      CALL FUNCTION 'CD_RETROCALC_PERIOD'
        EXPORTING
          entry = rgdir
        IMPORTING
          calcd = sw_retro.
      CHECK sw_retro EQ ' '.
      evp-seqnr = rgdir-seqnr.
    ENDLOOP.
    READ TABLE rgdir WITH KEY evp-seqnr.
    IF sy-subrc NE 0.
      CLEAR rgdir.
    ENDIF.
    REFRESH rgdir.
    APPEND rgdir.
  ENDIF.

  IF p_log = 'X'.
    IF sy-subrc <> 0.
      MOVE pernr-pernr TO ee_table-pernr.
      MOVE 'Z' TO ee_table-status.
      APPEND ee_table.
    ELSE.
      MOVE pernr-pernr TO ee_table-pernr.
      MOVE 'Y' TO ee_table-status.
      APPEND ee_table.
    ENDIF.
  ENDIF.
  LOOP AT rgdir WHERE iabkrs IN pnpabkrs.  ENDLOOP.
  IF sy-subrc NE 0.
    REJECT.
  ENDIF.
  IF NOT pnpbegda IS INITIAL AND
     NOT pnpendda IS INITIAL.
*   if the year is the same.
    IF pnpbegda(4) = pnpendda(4).
      READ TABLE i_range WITH KEY per_id = 'Yr'.
      l_year_begda = i_range-begda.
    ELSE.
      l_year_begda = pnpbegda.
    ENDIF.
    LOOP AT rgdir WHERE paydt BETWEEN l_year_begda AND pnpendda.
    ENDLOOP.
    IF sy-subrc NE 0.
      REJECT.
    ENDIF.
  ENDIF.
ENDFORM.                    " get_payroll_results

*&---------------------------------------------------------------------*
*&      Form  get_original_payrolls
*&---------------------------------------------------------------------*
FORM get_original_payrolls.
  DATA: p_rgdir LIKE rgdir OCCURS 0 WITH HEADER LINE,
        temp_rgdir LIKE rgdir OCCURS 0 WITH HEADER LINE,
        void_rgdir LIKE rgdir OCCURS 0 WITH HEADER LINE,
        temp_org LIKE rgdir,
        sw_retro.
  DATA: sy_tabix LIKE sy-tabix.
  REFRESH org_rgdir.
  p_rgdir[] = rgdir[].
  IF ( p_voids EQ ' ' ) AND ( p_offcyc EQ ' ' ).
* delete voided payroll runs from the directory.
    DELETE p_rgdir WHERE void NE ' '.
  ELSE.
    LOOP AT p_rgdir WHERE void EQ 'V'.
      APPEND p_rgdir TO temp_rgdir.
    ENDLOOP.
    IF p_offcyc EQ 'X'.
      void_rgdir[] = temp_rgdir[].
      REFRESH temp_rgdir.
      LOOP AT p_rgdir.
        LOOP AT void_rgdir WHERE seqnr EQ p_rgdir-seqnr.
          sy_tabix = sy-tabix.
        ENDLOOP.
        IF p_voids = 'X'.
          CHECK sy-subrc = 0.
        ELSE.
          CHECK sy-subrc NE 0.
        ENDIF.
        IF p_rgdir-inpty IN p_paytyr.
          APPEND p_rgdir TO temp_rgdir.
        ELSEIF ( 'C' IN p_paytyr ) AND ( p_rgdir-payty EQ 'C' ).
          LOOP AT temp_rgdir WHERE payty EQ p_rgdir-payty
                             AND   payid EQ p_rgdir-payid
                             AND   paydt EQ p_rgdir-paydt
                             AND   void  EQ ' '.
          ENDLOOP.
          CHECK sy-subrc NE 0.
          APPEND p_rgdir TO temp_rgdir.
          CALL FUNCTION 'HR_CA_GET_ORIGINAL_PAYROLL'
            EXPORTING
              in_rgdir  = p_rgdir
            IMPORTING
              out_rgdir = temp_org
            TABLES
              rgdir     = p_rgdir
            EXCEPTIONS
              OTHERS    = 1.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
          LOOP AT temp_rgdir WHERE seqnr EQ temp_org-seqnr.
          ENDLOOP.
          CHECK sy-subrc NE 0.
          APPEND temp_org TO temp_rgdir.
        ELSE.
          IF p_voids = 'X'.
            DELETE temp_rgdir INDEX sy_tabix.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
    REFRESH p_rgdir.
    p_rgdir[] = temp_rgdir[].
  ENDIF.

* FIND ALL ORIGINAL PAYROLLS AND FILL INTERNAL TABLE ORG_RGDIR
  LOOP AT p_rgdir.
    CALL FUNCTION 'CD_RETROCALC_PERIOD'
      EXPORTING
        entry = p_rgdir
      IMPORTING
        calcd = sw_retro.

    CHECK sw_retro EQ ' '.
    org_rgdir = p_rgdir.
    APPEND org_rgdir.
  ENDLOOP.                             "LOOP AT RGDIR.
* sort org_rgdir.                                          "nt.418921
  SORT org_rgdir BY inper iperm inpid inpty bondt           "nt.418921
                    seqnr DESCENDING.                       "nt.418921
  DELETE ADJACENT DUPLICATES FROM org_rgdir                 "nt.418921
         COMPARING abkrs inper iperm inpid inpty bondt.     "nt.480289
  SORT org_rgdir BY seqnr.                                  "nt.418921
  DESCRIBE TABLE org_rgdir LINES dtable_lines.
  IF dtable_lines = 0 AND p_log = 'X'.
    LOOP AT ee_table WHERE pernr = pernr-pernr.
      MOVE 'N' TO ee_table-status.
      MODIFY ee_table.
      EXIT.
    ENDLOOP.
    REJECT.
  ENDIF.
ENDFORM.                    " get_original_payrolls

*&---------------------------------------------------------------------*
*&      Form  extract_payroll_data
*&---------------------------------------------------------------------*
FORM extract_payroll_data.
* get YTD, QTD, MTD as before the interval
  PERFORM prepare_xtd_values.
* get amounts for reporting interval
* interval is extended by the as of date of the forms for tax
* reporter variances - for FI view it's the beginn/end of interval
  PERFORM set_accumulation_data.
  LOOP AT org_rgdir WHERE paydt GE p_beg
                    AND   paydt LE p_asoc.
*    perform extract_ru_data using org_rgdir-seqnr.      "nt.420034
*    check perm-txcmp in p_txcmp.                        "nt.420034
    CHECK org_rgdir-abkrs IN pnpabkrs.                   "Note 566169

*<SAM850091>
*    REFRESH save_rt.                                     "Note 492427
*<SAM850091>

    PERFORM get_all_involved_pays.
    SORT evp BY seqnr ASCENDING.
    LOOP AT evp.
*<SAM889346>
*     check for NAMC run check date for tax reporter runs  "Note 771647
      IF p_fi IS INITIAL AND evp-payty = 'C'.              "Note 771647
        CHECK evp-paydt BETWEEN p_beg AND p_end.           "Note 771647
      ENDIF.                                               "Note 771647
*</SAM889346>
**     for tax rep adjustment after interval before as of date,
**     only consider retro into the interval
*      IF org_rgdir-paydt GT p_end.
*        CHECK evp-paydt LE p_end.
*      ENDIF.
*     for all others do not accept any results beyond the highest
*     selected pay date
      CHECK ( evp-paydt <= p_end ).
*</SAM889346>
      PERFORM extract_ru_data USING evp-seqnr.
      CHECK perm-txcmp IN p_txcmp.                          "nt.420034
      PERFORM read_rt_table.
    ENDLOOP.
  ENDLOOP.
  PERFORM complete_data_table TABLES data_table_cp.
* with employee details activated, do merge of XTD and data table now,
* w/o emplyee info, do it at the very end
  IF NOT p_inf IS INITIAL.
    PERFORM add_accumulation_data TABLES data_table_cp
                                         data_table_o_acc.
    PERFORM add_accumulation_data TABLES data_table_cp
                                         data_table_accum.
  ENDIF.
ENDFORM.                    " extract_payroll_data

*&---------------------------------------------------------------------*
*&      Form  get_all_involved_pays
*&---------------------------------------------------------------------*
FORM get_all_involved_pays.
  DATA: s_rgdir LIKE rgdir OCCURS 20 WITH HEADER LINE.
  REFRESH evp.
  RANGES l_rng_abkrs FOR rgdir-abkrs.                       "note307910
* Payroll area must be passed to cd_evaluation_periods so  note307910
* that the result to be evaluated can be uniquely          note307910
* identfied where  there is a retro over a payroll area    note307910
* transfer and the payroll area modifiers are the same.    note307910
  CLEAR l_rng_abkrs. REFRESH l_rng_abkrs.                   "note307910
  l_rng_abkrs-sign   = 'I'.                                 "note307910
  l_rng_abkrs-option = 'EQ'.                                "note307910
  l_rng_abkrs-low    = org_rgdir-iabkrs.                    "note307910
  APPEND l_rng_abkrs.                                       "note307910
*
  IF org_rgdir-void EQ ' '.
    CALL FUNCTION 'CD_EVALUATION_PERIODS'
      EXPORTING
        bonus_date         = org_rgdir-bondt
        inper_modif        = org_rgdir-iperm
        inper              = org_rgdir-inper
        pay_type           = org_rgdir-payty
        pay_ident          = org_rgdir-payid
*        all_results_of_run = space
      TABLES
        rgdir              = rgdir
        evpdir             = evp
        iabkrs             = l_rng_abkrs                    "note307910
      EXCEPTIONS
        OTHERS             = 1.
  ELSE.
    s_rgdir[] = rgdir[].
    LOOP AT s_rgdir WHERE void  = 'V'
                    AND   seqnr < org_rgdir-seqnr.
      IF  s_rgdir-voidd GE org_rgdir-rundt OR
        ( s_rgdir-voidd = org_rgdir-rundt AND
          s_rgdir-voidt > org_rgdir-runtm ).
        s_rgdir-void = ' '.
        MODIFY s_rgdir INDEX sy-tabix.
      ENDIF.
    ENDLOOP.
    CALL FUNCTION 'CD_EVALUATION_PERIODS_VOID'
      EXPORTING
        bonus_date         = org_rgdir-bondt
        inper_modif        = org_rgdir-iperm
        inper              = org_rgdir-inper
        pay_type           = org_rgdir-payty
        pay_ident          = org_rgdir-payid
        special_void       = 'V'
*        all_results_of_run = space
      TABLES
        rgdir              = s_rgdir
        evpdir             = evp
        iabkrs             = l_rng_abkrs                    "note307910
      EXCEPTIONS
        no_record_found    = 1
        OTHERS             = 2.
  ENDIF.
  CHECK sy-subrc = 0.
* SET ACTIVE PASSIVE FOR DIFFERENCE-CALCULATION
  CALL FUNCTION 'CD_REORG_RGDIR'
    TABLES
      rgdir = evp.
ENDFORM.                    " get_all_involved_pays
*&---------------------------------------------------------------------*
*&      Form  set_flags_for_processing
*&---------------------------------------------------------------------*
FORM set_flags_for_processing.
  IF r_beg NE space.
    p_void = 'X'.
  ELSE.
    CLEAR p_void.
  ENDIF.
ENDFORM.                    " set_flags_for_processing
*&---------------------------------------------------------------------*
*&      Form  sort_data_tables
*&---------------------------------------------------------------------*
FORM sort_data_tables.
* sort table containing retro-calculation differences
  PERFORM build_sort_data_table TABLES data_table_retro
                                       sort_tmp_retro.
* sort table containing amounts of original period
  IF p_ctd = 'X' OR p_tls = 'X'.
    PERFORM build_sort_data_table TABLES data_table_cp
                                         sort_tmp_cp.
  ENDIF.
* sort table containing prior period adjustments
  PERFORM build_sort_data_table TABLES data_table_ppadj
                                       sort_tmp_ppadj.
  PERFORM build_sort_data_table TABLES data_table_cpadj
                                       sort_tmp_cpadj.
ENDFORM.                    " sort_data_tables
*&---------------------------------------------------------------------*
*&      Form  fill_final_tables
*&---------------------------------------------------------------------*
FORM fill_final_tables.
  dtable_lines = 0.
  DESCRIBE TABLE sort_tmp_cp LINES dtable_lines.

  LOOP AT sort_tmp_cp.
    IF p_ytd = 'X' OR
       p_qtd = 'X' OR
       p_mtd = 'X' OR
       p_ctd = 'X'.
      MOVE-CORRESPONDING sort_tmp_cp TO sort_table_cp.
      PERFORM clear_not_used_fields USING sort_table_cp.
      COLLECT sort_table_cp.
      MOVE-CORRESPONDING sort_table_cp TO data_table.
      CHECK NOT data_table-betrg   IS INITIAL OR
            NOT data_table-anzhl   IS INITIAL OR
            NOT data_table-betrg_m IS INITIAL OR
            NOT data_table-anzhl_m IS INITIAL OR
            NOT data_table-betrg_q IS INITIAL OR
            NOT data_table-anzhl_q IS INITIAL OR
            NOT data_table-betrg_y IS INITIAL OR
            NOT data_table-anzhl_y IS INITIAL.
      COLLECT data_table.
    ENDIF.
    IF p_tls = 'X'.
      MOVE-CORRESPONDING sort_tmp_cp TO total_srt_cp.
      PERFORM clear_fields_for_sort USING total_srt_cp.
      CHECK NOT total_srt_cp-betrg IS INITIAL OR
            NOT total_srt_cp-anzhl IS INITIAL.
      COLLECT total_srt_cp.
    ENDIF.
  ENDLOOP.

  LOOP AT sort_tmp_ppadj.
    IF p_ytd = 'X' OR
       p_qtd = 'X' OR
       p_mtd = 'X' OR
       p_ctd = 'X'.
      MOVE-CORRESPONDING sort_tmp_ppadj TO sort_table_ppadj.
      PERFORM clear_not_used_fields USING sort_table_ppadj.
      COLLECT sort_table_ppadj.
      MOVE-CORRESPONDING sort_table_ppadj TO data_table.
      CHECK NOT data_table-betrg IS INITIAL OR
            NOT data_table-anzhl IS INITIAL.
      COLLECT data_table.
    ENDIF.
    IF p_tls = 'X'.
      MOVE-CORRESPONDING sort_tmp_ppadj TO total_srt_ppadj.
      PERFORM clear_fields_for_sort USING total_srt_ppadj.
      CHECK NOT total_srt_ppadj-betrg IS INITIAL OR
            NOT total_srt_ppadj-anzhl IS INITIAL.
      COLLECT total_srt_ppadj.
    ENDIF.
  ENDLOOP.

  LOOP AT sort_tmp_cpadj.
    IF p_ytd = 'X' OR
       p_qtd = 'X' OR
       p_mtd = 'X' OR
       p_ctd = 'X'.
      MOVE-CORRESPONDING sort_tmp_cpadj TO sort_table_cpadj.
      PERFORM clear_not_used_fields USING sort_table_cpadj.
      COLLECT sort_table_cpadj.
      MOVE-CORRESPONDING sort_table_cpadj TO data_table.
      CHECK NOT data_table-betrg IS INITIAL OR
            NOT data_table-anzhl IS INITIAL.
      COLLECT data_table.
    ENDIF.
    IF p_tls = 'X'.
      MOVE-CORRESPONDING sort_tmp_cpadj TO total_srt_cpadj.
      PERFORM clear_fields_for_sort USING total_srt_cpadj.
      CHECK NOT total_srt_cpadj-betrg IS INITIAL OR
            NOT total_srt_cpadj-anzhl IS INITIAL.
      COLLECT total_srt_cpadj.
    ENDIF.
  ENDLOOP.

  LOOP AT sort_tmp_retro.
    IF p_ytd = 'X' OR
       p_qtd = 'X' OR
       p_mtd = 'X' OR
       p_ctd = 'X'.
      MOVE-CORRESPONDING sort_tmp_retro TO sort_table_retro.
      PERFORM clear_not_used_fields USING sort_table_retro.
      CHECK NOT sort_table_retro-betrg IS INITIAL OR
            NOT sort_table_retro-anzhl IS INITIAL.
      COLLECT sort_table_retro.
      MOVE-CORRESPONDING sort_table_retro TO data_table.
      CHECK NOT data_table-betrg IS INITIAL OR
            NOT data_table-anzhl IS INITIAL.
      COLLECT data_table.
    ENDIF.
    IF p_tls = 'X'.
      MOVE-CORRESPONDING sort_tmp_retro TO total_srt_retro.
      PERFORM clear_fields_for_sort USING total_srt_retro.
      CHECK NOT total_srt_retro-betrg IS INITIAL OR
            NOT total_srt_retro-anzhl IS INITIAL.
      COLLECT total_srt_retro.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " fill_final_tables
*&---------------------------------------------------------------------*
*&      Form  clear_not_used_fields
*&---------------------------------------------------------------------*
FORM clear_not_used_fields USING p_data_table LIKE data_table.
  CLEAR p_data_table-lgart.
  CLEAR p_data_table-taxty.
  READ TABLE sort_options WITH KEY shortstrg = 'A'.
  IF sy-subrc NE 0.
    CLEAR p_data_table-bukrs.
  ENDIF.
  READ TABLE sort_options WITH KEY shortstrg = 'B'.
  IF sy-subrc NE 0.
    CLEAR p_data_table-werks.
  ENDIF.
  READ TABLE sort_options WITH KEY shortstrg = 'C'.
  IF sy-subrc NE 0.
    CLEAR p_data_table-btrtl.
  ENDIF.
  READ TABLE sort_options WITH KEY shortstrg = 'D'.
  IF sy-subrc NE 0.
    CLEAR p_data_table-kostl.
  ENDIF.
  READ TABLE sort_options WITH KEY shortstrg = 'E'.
  IF sy-subrc NE 0.
    CLEAR p_data_table-iabkrs.
    CLEAR p_data_table-abkrs.
  ENDIF.
  READ TABLE sort_options WITH KEY shortstrg = 'F'.
  IF sy-subrc NE 0.
    CLEAR p_data_table-persg.
  ENDIF.
  READ TABLE sort_options WITH KEY shortstrg = 'G'.
  IF sy-subrc NE 0.
    CLEAR p_data_table-persk.
  ENDIF.
  READ TABLE sort_options WITH KEY shortstrg = 'H'.
  IF sy-subrc NE 0.
    CLEAR p_data_table-vdsk1.
  ENDIF.
  READ TABLE sort_options WITH KEY shortstrg = 'I'.
  IF sy-subrc NE 0.
    CLEAR p_data_table-stat2.
  ENDIF.
  IF p_inf IS INITIAL.
    READ TABLE sort_options WITH KEY shortstrg = 'J'.
    IF sy-subrc NE 0.
      CLEAR p_data_table-pernr.
    ENDIF.
  ENDIF.
  READ TABLE sort_options WITH KEY shortstrg = 'K'.
  IF sy-subrc NE 0.
    CLEAR p_data_table-txcmp.
  ENDIF.
  READ TABLE sort_options WITH KEY shortstrg = 'L'.
  IF sy-subrc NE 0.
    CLEAR p_data_table-taxau.
  ENDIF.
ENDFORM.                    " clear_not_used_fields
*&---------------------------------------------------------------------*
*&      Form  clear_fields_for_sort
*&---------------------------------------------------------------------*
FORM clear_fields_for_sort USING p_data_table LIKE total_srt_table.
  CLEAR p_data_table-taxty.
  READ TABLE sort_options WITH KEY shortstrg = 'A'.
  IF sy-subrc NE 0.
    CLEAR p_data_table-bukrs.
  ENDIF.
  READ TABLE sort_options WITH KEY shortstrg = 'B'.
  IF sy-subrc NE 0.
    CLEAR p_data_table-werks.
  ENDIF.
  READ TABLE sort_options WITH KEY shortstrg = 'C'.
  IF sy-subrc NE 0.
    CLEAR p_data_table-btrtl.
  ENDIF.
  READ TABLE sort_options WITH KEY shortstrg = 'D'.
  IF sy-subrc NE 0.
    CLEAR p_data_table-kostl.
  ENDIF.
  READ TABLE sort_options WITH KEY shortstrg = 'F'.
  IF sy-subrc NE 0.
    CLEAR p_data_table-persg.
  ENDIF.
  READ TABLE sort_options WITH KEY shortstrg = 'G'.
  IF sy-subrc NE 0.
    CLEAR p_data_table-persk.
  ENDIF.
  IF p_inf IS INITIAL.
    READ TABLE sort_options WITH KEY shortstrg = 'J'.
    IF sy-subrc NE 0.
      CLEAR p_data_table-pernr.
    ENDIF.
  ENDIF.
  READ TABLE sort_options WITH KEY shortstrg = 'K'.
  IF sy-subrc NE 0.
    CLEAR p_data_table-txcmp.
  ENDIF.
  READ TABLE sort_options WITH KEY shortstrg = 'L'.
  IF sy-subrc NE 0.
    CLEAR p_data_table-taxau.
  ENDIF.
ENDFORM.                    " clear_fields_for_sort

*<SAM850091>
**&---------------------------------------------------------------------
**
**&      Form  CONSIDER_OUTFLOW
**&---------------------------------------------------------------------
**
*FORM consider_outflow.
*  DATA: delta_xdfrt LIKE rt-betrg,
*        delta_bal   LIKE rt-betrg.
**  data: l_neg_5py(1) value '?'.                            "nt-0371422
*
*  CHECK consider_outflow = 'X'.
*  LOOP AT xdfrt WHERE lgart = rt-lgart.
**   if negative good money (/5py < 0), ignore erroneous      nt-0371422
**   XDFRT accounted entries by setting to 0.  Payroll        nt-0371422
**   should have done this but will fix later.  When /5PY is  nt-0371422
**   negative, nothing is accounted. l_neg_5py set inside     nt-0371422
**   i51t0 loop below to minimize reads to table RT           nt-0371422
**    if l_neg_5py = 'T'.                                    "nt-0371422
**      xdfrt-accam = 0.                                     "nt-0371422
**    endif.                                                 "nt-0371422
**   see associated comments above for information about      nt-0371422
**   /5PY checking                                            nt-0371422
**    if l_neg_5py = '?'.                                    "nt-0371422
**      read table rt with key lgart = '/5PY'.               "nt-0371422
**      if sy-subrc = 0 and rt-betrg < 0.                    "nt-0371422
**         l_neg_5py = 'T'.                                  "nt-0371422
**        xdfrt-accam = 0.                                   "nt-0371422
**      endif.                                               "nt-0371422
**     endif.                                                "nt-0371422
*    IF swgtyp-rechz NE '-'.
*      ADD      xdfrt-unaccam TO   delta_xdfrt.
*      SUBTRACT xdfrt-accam   FROM delta_xdfrt.
*    ELSE.
*      SUBTRACT xdfrt-unaccam FROM delta_xdfrt.
*      ADD      xdfrt-accam   TO   delta_xdfrt.
*    ENDIF.
*  ENDLOOP.
*  IF evp-srtza NE 'A'.
*    delta_xdfrt = delta_xdfrt * -1.
*  ENDIF.
*  data_table_tmp-betrg = data_table_tmp-betrg - delta_xdfrt.
*
** Start Note 492427
*  IF delta_xdfrt <> 0.
*    MOVE rt-lgart TO save_rt-lgart.
*    COLLECT save_rt.
*  ENDIF.
** End Note 492427
*
*  LOOP AT bal WHERE lgart = rt-lgart.
*    IF swgtyp-rechz NE '-'.
*      ADD      bal-unaccam TO   delta_bal.
*      SUBTRACT bal-accam   FROM delta_bal.
*    ELSE.
*      SUBTRACT bal-unaccam FROM delta_bal.
*      ADD      bal-accam   TO   delta_bal.
*    ENDIF.
*  ENDLOOP.
*  IF evp-srtza NE 'A'.
*    delta_bal = delta_bal * -1.
*  ENDIF.
*  data_table_tmp-betrg = data_table_tmp-betrg + delta_bal.
*ENDFORM.                    " CONSIDER_OUTFLOW
*<SAM850091>

*&---------------------------------------------------------------------*
*&      Form  PRINT_OUT_DETAILS
*&---------------------------------------------------------------------*
FORM print_out_details.
  IF p_ytd = 'X' OR
     p_qtd = 'X' OR
     p_mtd = 'X' OR
     p_ctd = 'X'.

    IF p_inf IS INITIAL.
      PERFORM add_accumulation_data TABLES data_table
                                           data_table_o_acc.
      PERFORM add_accumulation_data TABLES data_table
                                           data_table_accum.
    ENDIF.

    dtable_lines = 0.
    DESCRIBE TABLE data_table LINES dtable_lines.
    PERFORM print_rpt_header USING p_form
                                   pagno
                                   p_beg
                                   p_end.

    IF dtable_lines > 0.
      PERFORM print_data_table.
    ENDIF.
  ENDIF.

ENDFORM.                    " PRINT_OUT_DETAILS
*&---------------------------------------------------------------------*
*&      Form  PRINT_OUT_TOTALS
*&---------------------------------------------------------------------*
FORM print_out_totals.
  IF p_tls = 'X'.
    PERFORM sort_totals_tables TABLES total_srt_retro "<-----IN
                                      total_srt_cpadj "<-----IN
                                      total_srt_cp    "<-----IN
                                      total_srt_ppadj "<-----IN
                                      sort_options    "<-----IN
                                      totals_table.   "----->OUT
    dtable_lines = 0.
    DESCRIBE TABLE totals_table LINES dtable_lines.
    IF dtable_lines > 0.
      page_event = 'T'.
      PERFORM print_totals_table TABLES totals_table
                                        sort_options
                                        swgtyp
                                  USING p_form
                                        p_molga-low
                                        p_beg
                                        p_end.
    ELSE.
      WRITE:/ '****** No records found ******'(114).
    ENDIF.
  ENDIF.
  IF dtable_lines = 0.
    WRITE:/ '****** No records found ******'(114).
  ENDIF.

ENDFORM.                    " PRINT_OUT_TOTALS
*&---------------------------------------------------------------------*
*&      Form  PRINT_OUT_LOG
*&---------------------------------------------------------------------*
FORM print_out_log.
  CLEAR page_event.
  IF p_log = 'X'.
    NEW-PAGE.
    PERFORM print_rpt_header USING p_form
                                   pagno
                                   p_beg
                                   p_end.

    PERFORM print_log TABLES ee_table
                      USING  wtype1         " NT. 0363102
                             p_molga-low
                             p_form
                             p_payty
                             p_payid
                             p_off
                             special_pay.
  ENDIF.
ENDFORM.                    " PRINT_OUT_LOG
*&---------------------------------------------------------------------*
*&      Form  PRINT_PAGE_HEADER
*&---------------------------------------------------------------------*
FORM print_page_header.
  IF page_event = 'T'.
    PERFORM print_rpt_header USING p_form
                                   sy-pagno
                                   p_beg
                                   p_end.

    PERFORM totals_header USING b_type.
  ENDIF.
ENDFORM.                    " PRINT_PAGE_HEADER
*&---------------------------------------------------------------------*
*&      Form  PROCESS_TAXREP_ADJUSTMENTS
*&---------------------------------------------------------------------*
FORM process_taxrep_adjustments.
*<SAM850091>
*  CHECK consider_outflow = 'X'.
*<SAM850091>
*<SAM889346>
*  data_table_cpadj = data_table_tmp.
*</SAM889346>

*<SAM889346>
* get as of dates for current company
  READ TABLE itrdt WITH KEY txcmp = data_table_tmp-txcmp
                            taxau = data_table_tmp-taxau.
  IF sy-subrc NE 0.
    READ TABLE itrdt WITH KEY txcmp = data_table_tmp-txcmp.
    IF sy-subrc NE 0.
      READ TABLE itrdt WITH KEY txcmp = space.
    ENDIF.
  ENDIF.
*</SAM889346>
  CHECK sy-subrc = 0.
  CHECK ( org_rgdir-paydt LE itrdt-last_asofd AND
          evp-paydt       LT p_beg ) OR
          org_rgdir-paydt GT p_end.

*<SAM889346>
  data_table_cpadj = data_table_tmp.
*<SAM889346>

* if correction to previous form, check against run parameter of
* previous form
  IF org_rgdir-paydt LE itrdt-last_asofd AND
     evp-paydt       LT p_beg.
    IF org_rgdir-rundt = itrdt-last_genda.
      CHECK org_rgdir-runtm LE itrdt-last_gentm.
    ELSE.
      CHECK org_rgdir-rundt LE itrdt-last_genda.
    ENDIF.
* backout corrections done before the as of date of the form for
* a previous interval
* corrections done after the current interval end date before
* the as of date for the form of the current interval must be added
    data_table_cpadj-betrg = data_table_cpadj-betrg * -1.
    data_table_cpadj-anzhl = data_table_cpadj-anzhl * -1.
  ENDIF.
* if correction to current interval, check against run parameter of
* current form
  IF org_rgdir-paydt GT p_end.
    IF org_rgdir-rundt = itrdt-genda.
      CHECK org_rgdir-runtm LE itrdt-gentm.
    ELSE.
      CHECK org_rgdir-rundt LE itrdt-genda.
    ENDIF.
  ENDIF.
  data_table_cpadj-btype = 'T'.
  COLLECT data_table_cpadj.
ENDFORM.                    " PROCESS_TAXREP_ADJUSTMENTS
*&---------------------------------------------------------------------*
*&      Form  PROCESS_SCREEN_COMMANDS
*&---------------------------------------------------------------------*
FORM process_screen_commands.
  CASE sscrfields-ucomm.
    WHEN '3PR'.
      IF p_void = 'X'.
        r_end = '99991231'.
        p_void = ''.
      ELSE.
        r_tim = ''.
        r_beg = ''.
        r_end = ''.
        p_void = 'X'.
      ENDIF.
    WHEN 'SELO'.
      text_title_tmp     = 'Reportable fields'(T0T).
      text_left_tmp      = 'Possible fields'(T0L).
      text_right_tmp     = 'Chosen fields'(T0R).
      CALL FUNCTION 'RP_OPTIONS_INTO_STRING'
        EXPORTING
          delimiter_sign             = '/'
          text_title                 = text_title_tmp
          text_left                  = text_left_tmp
          text_right                 = text_right_tmp
          status                     = 'ORDER'
          text_object                = 'TXF_SORT_INFO'
        IMPORTING
          return_code                = retcode
        TABLES
          text_symbol_relation_tab   = sel_tab
        CHANGING
          string_value               = option
        EXCEPTIONS
          table_string_inconsistency = 01
          unknown_status             = 02
          OTHERS                     = 03.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
      p_srtopt = option.
    WHEN 'SFRM'.
      p_form = space.
      IF p_frmtyp = 'X'.
        p_frmtyp = ''.
      ELSE.
        p_frmtyp = 'X'.
      ENDIF.
  ENDCASE.

ENDFORM.                    " PROCESS_SCREEN_COMMANDS
*&---------------------------------------------------------------------*
*&      Form  GENERAL_SCREEN_PROCESSING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM general_screen_processing.
  IF p_ytd = 'X' OR
     p_qtd = 'X' OR
     p_mtd = 'X'.
    p_ctd = 'X'.
  ENDIF.
ENDFORM.                    " GENERAL_SCREEN_PROCESSING

*<SAM850091>
**&---------------------------------------------------------------------
**
**&      Form  add_xdfrt_wagetypes
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*FORM add_xdfrt_wagetypes.
*  LOOP AT save_rt.
*    READ TABLE bal WITH KEY lgart = save_rt-lgart.
*    CHECK sy-subrc = 0.
*    READ TABLE rt  WITH KEY lgart = save_rt-lgart.
*    CHECK sy-subrc NE 0.
*    CLEAR rt.
*    rt-lgart = save_rt-lgart.
*    APPEND rt.
*  ENDLOOP.
*
** Note 702696
*  LOOP AT xdfrt WHERE lgart IN lgart.
*    READ TABLE rt WITH KEY lgart = xdfrt-lgart.
*    CHECK sy-subrc NE 0.
*    CLEAR rt.
*    rt-lgart = xdfrt-lgart.
*    APPEND rt.
*  ENDLOOP.
** end Note 702696
*ENDFORM.                    " add_xdfrt_wagetypes
*</SAM850091>

*&---------------------------------------------------------------------*
*&      Form  get_account_type
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_account_type.

  SELECT DISTINCT a~lgart a~symko a~endda b~koart
    INTO CORRESPONDING FIELDS OF TABLE g_acct_type
    FROM t52el AS a INNER JOIN t52ek AS b
    ON a~mandt = b~mandt AND
       a~symko = b~symko
    WHERE
       a~molga = p_molga-low AND
       b~koart LIKE 'C%'.

  SORT g_acct_type BY lgart endda ASCENDING.
ENDFORM.                    " get_account_type

*&---------------------------------------------------------------------*
*&      Form  ny_reconciliation
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ny_reconciliation.

*<SAM889346>
* locals
   DATA: lt_data LIKE data_table OCCURS 0.

* Start NY wage reporting changes - Note 686069
* If Wage Reporting check is enabled for NY State and City of Yonkers
* and the reporting year is 2003 or higher
  CHECK pn-pabrj >= 2003.  "pyyr   SAM850091


* eliminate records from current periods that cancel out each other
  PERFORM adjust_ny_ballance TABLES data_table_cp
                                    data_table_cp
                                    lt_data.      "dummy

* eliminate records from retro periods that cancel out current period
* results and eliminate corresponding prior-period adjustments as well
  PERFORM adjust_ny_ballance TABLES data_table_retro
                                    data_table_cp
                                    data_table_ppadj.
*</SAM889346>

*<SAM889346>
**<SAM889346>
** locals
**  DATA: lv_idx1 TYPE sytabix,
**        lv_idx2 TYPE sytabix,
**        lv_diff TYPE maxbt.
**</SAM889346>
*
** Start NY wage reporting changes - Note 686069
** If Wage Reporting check is enabled for NY State and City of Yonkers
** and the reporting year is 2003 or higher
*  CHECK pn-pabrj >= 2003.  "pyyr   SAM850091
*
**<SAM869832>
***<SAM850091>
***  CHECK p_w2 = 'X' OR p_sui = 'X'.
***<SAM850091>
**  IF g_fed_added = 'X'.
**    DELETE p_taxau WHERE low = 'FED'.
**  ENDIF.
***<SAM850091>
**** for W2s
***  IF p_w2 = 'X'.
***</SAM850091>
**</SAM869832>
*
** we want to eliminate any NY State or Yonkers items that seem to have
** been errors rather than active work performed in these authorities;
** hence remove entries that lead to a $0.00 retro difference
*  LOOP AT data_table_retro WHERE taxau = 'NY  '
*                              OR taxau = 'NY02'.
*    lv_idx1 = sy-tabix.
**   regular retro
*    READ TABLE data_table_cp WITH KEY taxau = data_table_retro-taxau
*                                      txcmp = data_table_retro-txcmp
*                                      fpper = data_table_retro-fpper
*                                      inper = data_table_retro-fpper.
*    IF sy-subrc = 0.
*     lv_idx2 = sy-tabix.
*     lv_diff = data_table_cp-betrg + data_table_retro-betrg.
*     IF lv_diff = 0.
*       DELETE data_table_retro INDEX lv_idx1.
*       DELETE data_table_cp INDEX lv_idx2.
*     ENDIF.
*    ENDIF.
*  ENDLOOP.
*
**<SAM889346>
**  PERFORM ny_w2 TABLES data_table_retro.
**  PERFORM ny_w2 TABLES data_table_cp.
**  PERFORM ny_w2 TABLES data_table_ppadj.
**  PERFORM ny_w2 TABLES data_table_cpadj.
**  PERFORM ny_w2 TABLES data_table_accum.
**<SAM889346>
*
**<SAM869832>
***<SAM850091>
***  ENDIF.
**** for SUI
***  IF p_sui = 'X'.
***    PERFORM ny_sui TABLES data_table_retro.
***    PERFORM ny_sui TABLES data_table_cp.
***    PERFORM ny_sui TABLES data_table_ppadj.
***    PERFORM ny_sui TABLES data_table_cpadj.
***    PERFORM ny_sui TABLES data_table_accum.
***  ENDIF.
***</SAM850091>
**
*** Add FED back to p_taxau for processing next employee "Note 810276
**  IF g_fed_added = 'X'.
**    p_taxau-sign = 'I'.
**    p_taxau-option = 'EQ'.
**    p_taxau-low = 'FED'.
**    COLLECT p_taxau.
**  ENDIF.
**</SAM869832>
*</SAM889346>

ENDFORM.                    " ny_reconciliation

*&--------------------------------------------------------------------*
*&      Form  adjust_ny_ballance
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->CT_DATA1   text
*      -->CT_DATA2   text
*      -->CT_DATA3   text
*---------------------------------------------------------------------*
FORM adjust_ny_ballance TABLES ct_data1 STRUCTURE data_table
                               ct_data2 STRUCTURE data_table
                               ct_data3 STRUCTURE data_table.

* locals
  DATA: lv_idx1 TYPE sytabix,
        lv_idx2 TYPE sytabix,
        lv_diff TYPE maxbt,
        lt_data1 LIKE data_table OCCURS 0 WITH HEADER LINE,
        lt_data2 LIKE data_table OCCURS 0 WITH HEADER LINE,
        lt_data3 LIKE data_table OCCURS 0 WITH HEADER LINE.

* since tables only passes references
  lt_data1[] = ct_data1[].
  lt_data2[] = ct_data2[].
  lt_data3[] = ct_data3[].

* we want to eliminate any NY State or Yonkers items that seem to have
* been errors rather than active work performed in these authorities;
* hence remove entries that lead to a $0.00 retro difference
  LOOP AT lt_data1 WHERE taxau = 'NY  '
                      OR taxau = 'NY02'.
    lv_idx1 = sy-tabix.
    CLEAR lt_data2.
*   regular retro
    LOOP AT lt_data2 WHERE taxau = lt_data1-taxau
                       AND txcmp = lt_data1-txcmp.
      lv_idx2 = sy-tabix.
*     get difference
      lv_diff = lt_data2-betrg + lt_data1-betrg.
*     if difference is $0.00
      IF lv_diff = 0.
*       find corresponding 3. table entry since it could be a copy of
*       table ct_data2
        CLEAR lt_data3.
        READ TABLE lt_data3 WITH KEY taxau = lt_data2-taxau
                                     txcmp = lt_data2-txcmp
                                     fpper = lt_data2-fpper
                                     inper = lt_data2-fpper.
        IF sy-subrc = 0.
*         if found delete it
          DELETE lt_data3 INDEX sy-tabix.
        ENDIF.
*       remove entry from 1. table and other corresponding
*       entries from 2. table
        DELETE lt_data1 INDEX lv_idx1.
        DELETE lt_data2 INDEX lv_idx2.
*       remove existing entry in 2. table that matches 1. table
        READ TABLE lt_data2 WITH KEY taxau = lt_data1-taxau
                                     txcmp = lt_data1-txcmp
                                     fpper = lt_data1-fpper
                                     inper = lt_data1-fpper.
        IF sy-subrc = 0.
          DELETE lt_data2 INDEX sy-tabix.
        ENDIF.
*       since there should be only one match per records in tables
*       1 and 2 we need to exit the loop at 2 here
        EXIT. "process next lt_data1 entry
      ENDIF.
    ENDLOOP.
  ENDLOOP.
  CLEAR: ct_data1, ct_data1[],
         ct_data2, ct_data2[],
         ct_data3, ct_data3[].
  ct_data1[] = lt_data1[].
  ct_data2[] = lt_data2[].
  ct_data3[] = lt_data3[].

ENDFORM.                    "adjust_ny_ballance

*<SAM889346>
**&---------------------------------------------------------------------
*
**&      Form  ny_w2
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
**      -->P_DATA_TABLE  text
**----------------------------------------------------------------------
*
*FORM ny_w2 TABLES p_data_table STRUCTURE data_table.
*  RANGES: l_ny_w2  FOR t596i-sumlg.
*  DATA: ny   TYPE c,
*        ny02 TYPE c,
*        l_data_table LIKE data_table OCCURS 0 WITH HEADER LINE,
*        save_data_table LIKE data_table OCCURS 0 WITH HEADER LINE.
*
*  READ TABLE p_data_table WITH KEY taxau = 'NY'.
*  IF sy-subrc = 0.
*    ny = 'X'.
*  ENDIF.
*
*  READ TABLE p_data_table WITH KEY taxau = 'NY02'.
*  IF sy-subrc = 0.
*    ny02 = 'X'.
*  ENDIF.
*
*  IF NOT ( ny = 'X' OR ny02 = 'X' ).
*    IF NOT p_taxau[] IS INITIAL.
*      DELETE p_data_table WHERE NOT taxau IN p_taxau.
*    ENDIF.
*    EXIT.
*  ENDIF.
*
** W2 wage types and boxes
*  l_ny_w2-sign = 'I'.
*  l_ny_w2-option = 'EQ'.
*  l_ny_w2-low = '/701'.
*  APPEND l_ny_w2.
*  l_ny_w2-low = '/Q01'.
*  APPEND l_ny_w2.
*  l_ny_w2-low = 'Y701'.
*  APPEND l_ny_w2.
*  l_ny_w2-low = 'Q701'.
*  APPEND l_ny_w2.
*
*  DELETE p_data_table WHERE ( taxau = 'NY' OR taxau = 'NY02' ) AND
*                              box IN l_ny_w2.
*
*  l_data_table[] = p_data_table[].
*  DELETE l_data_table WHERE NOT ( taxau EQ 'FED' AND box IN l_ny_w2 ).
*  save_data_table[] = l_data_table[].
*  LOOP AT save_data_table INTO l_data_table.
*    IF ny = 'X'.
*      l_data_table-taxau = 'NY'.
*      MODIFY l_data_table INDEX sy-tabix.
*    ELSEIF ny02 = 'X'.
*      l_data_table-taxau = 'NY02'.
*      MODIFY l_data_table INDEX sy-tabix.
*      CONTINUE.
*    ENDIF.
*    IF ny02 = 'X'.
*      l_data_table-taxau = 'NY02'.
*      APPEND l_data_table.
*    ENDIF.
*  ENDLOOP.
*  APPEND LINES OF l_data_table TO p_data_table.
*  IF NOT p_taxau[] IS INITIAL.
*    DELETE p_data_table WHERE NOT taxau IN p_taxau.
*  ENDIF.
*ENDFORM.                                                    " ny_w2
*<SAM889346>

*<SAM850091>
**&---------------------------------------------------------------------
**
**&      Form  ny_sui
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
**      -->P_DATA_TABLE  text
**----------------------------------------------------------------------
**
*FORM ny_sui TABLES p_data_table STRUCTURE data_table.
*  RANGES: l_ny_sui FOR t596i-sumlg.
*  DATA: l_data_table LIKE data_table OCCURS 0 WITH HEADER LINE,
*        l_q4_begda TYPE sy-datum,
*        l_q4_endda TYPE sy-datum.
*
** SUI wage types and boxes
*  l_ny_sui-sign = 'I'.
*  l_ny_sui-option = 'EQ'.
*  l_ny_sui-low = '/610'.
*  APPEND l_ny_sui.
*  l_ny_sui-low = 'Q610'.
*  APPEND l_ny_sui.
*  l_ny_sui-low = 'Y610'.
*  APPEND l_ny_sui.
*  READ TABLE p_data_table WITH KEY taxau = 'NY'.
*  IF sy-subrc NE 0.
*    IF NOT p_taxau[] IS INITIAL.
*      DELETE p_data_table WHERE NOT taxau IN p_taxau.
*    ENDIF.
*    EXIT.
*  ENDIF.
*
** Store Q4 Begin Date and End Date
*  READ TABLE p_range WITH KEY per_id = 'Q4'.
*  IF sy-subrc = 0.
*    l_q4_begda = p_range-begda.
*    l_q4_endda = p_range-endda.
*  ENDIF.
** SUI Wages should be replaced by Federal Values only for Q4
*  DELETE p_data_table WHERE taxau = 'NY'
*                        AND box IN l_ny_sui
*                        AND paydt BETWEEN l_q4_begda and l_q4_endda.
*
*  l_data_table[] = p_data_table[].
*  DELETE l_data_table WHERE taxau NE 'FED'.
*  DELETE l_data_table WHERE NOT paydt BETWEEN l_q4_begda and l_q4_endda
*.
*  LOOP AT l_data_table WHERE box IN l_ny_sui.
*    l_data_table-taxau = 'NY'.
*    MODIFY l_data_table.
*  ENDLOOP.
*  APPEND LINES OF l_data_table TO p_data_table.
*  IF NOT p_taxau[] IS INITIAL.
*    DELETE p_data_table WHERE NOT taxau IN p_taxau.
*  ENDIF.
*ENDFORM.                                                      "ny_sui
*</SAM850091>

*&---------------------------------------------------------------------*
*&      Form  get_employees_from_index_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_employees_from_index_table .

* locals
  DATA: lt_t5ux1 LIKE t5ux1 OCCURS 0 WITH HEADER LINE.

  CHECK pnppernr[] IS INITIAL.
  CHECK ( NOT p_txcmp IS INITIAL ) OR ( NOT p_taxau IS INITIAL ).

  SELECT * FROM t5ux1 INTO TABLE lt_t5ux1
*<SAM889346>
*<SAM865164>
*                      WHERE tyear = pn-pabrj    "pyyr SAM850091
*                      WHERE tyear <= pn-endda(4)
*                        AND tyear >= pn-begda(4)
* dates should be according to payroll year when run for ABKRS
* and otherwise according to selection interval see get_period_dates
                      WHERE tyear <= p_end(4)
                        AND tyear >= p_beg(4)
*</SAM865164>
*</SAM889346>
                        AND txcmp IN p_txcmp
                        AND taxau IN p_taxau.
* add tax authority to selection
  LOOP AT lt_t5ux1.
    CLEAR pnppernr.
    pnppernr-sign   = 'I'.
    pnppernr-option = 'EQ'.
    pnppernr-low = lt_t5ux1-pernr.
    COLLECT pnppernr.
  ENDLOOP.
ENDFORM.                    " get_employees_from_index_table
