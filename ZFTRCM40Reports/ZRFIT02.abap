*&--------------------------------------------------------------------
*& Author                 : JIPARK
*& Creation Date          : 11/27/2003
*& Specification By       : JIPARK
*& Pattern                : Report 1-2
*& Development Request No : UD1K904507
*& Addl documentation     :
*& Description  : Cash flow report(Monthly/Weekly)
*&
*& Modification Log
*& Date     Developer      Request ID      Description
*&--------------------------------------------------------------------
REPORT zrfit02 MESSAGE-ID zmfi
               NO STANDARD PAGE HEADING LINE-SIZE 195.


**********************************************************************
* Data Declaration
**********************************************************************
TABLES: ztfi_cmal, ztfi_des, fdsr, fdsb, fdt1, fdes.
TABLES : tzpa, t036v, t036r, t037, t036p, pyordh, knb1.

DATA: icmal LIKE ztfi_cmal OCCURS 0 WITH HEADER LINE.
DATA : BEGIN OF it_ztfi_cmal OCCURS 0, "SUMMARY
       sumflg(1) TYPE c,
       fstflg(2) TYPE c,                                    "category 1
       scdflg(10) TYPE c,                                   "category 2
       grupp    LIKE fdes-grupp,
       mamt01   LIKE glt0-hslvt,
       mamt02   LIKE glt0-hslvt,
       mamt03   LIKE glt0-hslvt,
       mamt04   LIKE glt0-hslvt,
       mamt05   LIKE glt0-hslvt,
       mamt06   LIKE glt0-hslvt,
       mamt07   LIKE glt0-hslvt,
       mamt08   LIKE glt0-hslvt,
       mamt09   LIKE glt0-hslvt,
       mamt10   LIKE glt0-hslvt,
       mamt11   LIKE glt0-hslvt,
       mamt12   LIKE glt0-hslvt,
       amount   LIKE glt0-hslvt, "amount
       sumamt   LIKE glt0-hslvt, "sum
       dispw    LIKE ztfi_cmal-dispw,
END OF it_ztfi_cmal.
DATA : iztfi_cmap LIKE ztfi_cmap OCCURS 0 WITH HEADER LINE.
DATA : BEGIN OF isum01,
         amt01 LIKE glt0-hslvt,
         amt02 LIKE glt0-hslvt,
         amt03 LIKE glt0-hslvt,
         amt04 LIKE glt0-hslvt,
         amt05 LIKE glt0-hslvt,
         amt06 LIKE glt0-hslvt,
         amt07 LIKE glt0-hslvt,
         amt08 LIKE glt0-hslvt,
         amt09 LIKE glt0-hslvt,
         amt10 LIKE glt0-hslvt,
         amt11 LIKE glt0-hslvt,
         amt12 LIKE glt0-hslvt,
       END OF isum01.
DATA : isum02 LIKE isum01,
       isum03 LIKE isum01.

DATA : it_fdes LIKE it_ztfi_cmal OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF it_fdes01 OCCURS 0.  "plan detail data
        INCLUDE STRUCTURE fdes.
DATA: orign LIKE t036-orign,         "..Original Source
      o_ebe LIKE fdes-ebene.         "..Original plan level
DATA: END OF it_fdes01.

DATA : it_pltm LIKE ztfi_pltm OCCURS 0 WITH HEADER LINE.

*-------drill-down data
DATA: BEGIN OF it_ddwn OCCURS 0,
        budat   LIKE sy-datum,          "(posting)date
        dispw   LIKE ztfi_cmal-dispw,   "currency
        wrshb   LIKE ztfi_cmal-wrshb,   "amt
        dmshb   LIKE ztfi_cmal-dmshb,   "local amt
        saknr   LIKE ztfi_cmal-saknr,   "bank acct
        grupp   LIKE ztfi_cmal-grupp,   "group
        grptt(30),                      "group desc.
        koart   LIKE ztfi_cmal-koart,   "account type
        kacct   LIKE ztfi_cmal-hkont,   "vnd/cust/account
        fipos   LIKE ztfi_fmal-fipos,   "cmltm
        rftext(25),                     "cmltm desc.
        fincode LIKE ztfi_fmal-fincode, "fund
        fistl   LIKE ztfi_fmal-fistl,   "fctr
        sbewart LIKE ztfi_cmal-sbewart, "flow type
        belnr   LIKE ztfi_cmal-belnr,   "document
        finacct(1)  TYPE c,             "fund acct
        findesc(40) TYPE c,             "fund desc.
        bukrs   LIKE ztfi_cmal-bukrs,
        gjahr   LIKE ztfi_cmal-gjahr,
        knbelnr LIKE ztfi_cmal-belnr,   "invoice doc.
        kngjahr LIKE ztfi_cmal-gjahr,   "invoice year
      END OF it_ddwn.

DATA: BEGIN OF i036 OCCURS 100.
        INCLUDE STRUCTURE t036.
DATA: END OF i036.
DATA: BEGIN OF i036q OCCURS 10.
        INCLUDE STRUCTURE t036q.
DATA: END OF i036q.
DATA: BEGIN OF i037 OCCURS 100.
        INCLUDE STRUCTURE t037.
DATA: END OF i037.
DATA: BEGIN OF i039 OCCURS 10.
        INCLUDE STRUCTURE t039.
DATA: END OF i039.
DATA: wabnkko     LIKE fdsb-bnkko,
      wabukrs     LIKE fdsb-bukrs.
DATA: lines       TYPE i.
DATA: BEGIN OF gkoarttab OCCURS 10,
        koart LIKE bseg-koart,
        grupp LIKE t035-grupp,
      END   OF gkoarttab.
DATA: BEGIN OF zus_fdtab_felder,
        diskb  LIKE fdsb-bnkko,        "dispositive Kontobezeichnung
        azshb  LIKE fdsb-wrshb,
      END   OF zus_fdtab_felder.
DATA: BEGIN OF fdtab OCCURS 0.
*     hier darf nichts eingefügt werden, sonst Fehler nach Rückkehr vom
*     FB cash_forecast_select_and_compr, da aus Performancegründen
*     statt move-corresponding nur move verwendet wird
        INCLUDE STRUCTURE rf40v.
        INCLUDE STRUCTURE zus_fdtab_felder.
DATA: END   OF fdtab.

*variable..
DATA : sv_spras LIKE t001-spras,
       sv_ktopl LIKE t001-ktopl,
       sv_waers LIKE t001-waers,
       sv_trunctxt(15),
       sv_fr_datum LIKE ztfi_cmal-datum,                    "
       sv_to_datum LIKE ztfi_cmal-datum,
       sv_fstflg(20) TYPE c,
       sv_scdflg(20) TYPE c,
       sv_textl    LIKE t035t-textl,
       sv_subamt1  LIKE glt0-hslvt, "accumulated amount(weekly, monthly)
       sv_subamt2  LIKE glt0-hslvt,
       sv_totamt   LIKE glt0-hslvt,
*      sv_hslvt     LIKE ztfi_dss-hslvt,
*      sv_hslvt1    LIKE ztfi_dss-hslvt,
*      sv_hslvt2    LIKE ztfi_dss-hslvt,
*      sv_hslvt3    LIKE ztfi_dss-hslvt,
*      sv_hslvt4    LIKE ztfi_dss-hslvt,
*      sv_hslvt5    LIKE ztfi_dss-hslvt,
       sv_func,
       dispw_lin   TYPE i.

DATA:  v_field(20),
       v_week LIKE sy-datum(6).

DATA: i_line1(3) TYPE n,
      i_line2(3) TYPE n,
      i_line3(3) TYPE n,
      i_line4(3) TYPE n,
      i_line5(3) TYPE n,
      w_col(3) TYPE n.

*Ranges..
RANGES: s_datum FOR sy-datum,
        s_gjahr FOR bppe-gjahr,
        s_week  FOR sy-datum(6),
        r_datum FOR sy-datum,
        r_dispw FOR sv_waers.

CONSTANTS : c_cate1(4) VALUE 'A100', "Beg. Bal.
            c_catea(4) VALUE 'A110', "surplus/deficit
            c_cate2(4) VALUE 'A200', "open transaction
            c_cate3(4) VALUE 'A300', "Bank reconciliation
            c_cate4(4) VALUE 'A400'. "End. book bal.

*constants..
CONSTANTS : h_fstflg(20)  VALUE  '',
            h_scdflg(20)  VALUE  '',
*            h_group(10)   VALUE  'PlnGr',
            h_group(5)   VALUE  'PlnGr',
            h_textl(30)   VALUE  'Description',
*            h_total(21)   VALUE  'Accu. amt',
            h_total(9)   VALUE  'Accu. amt',
            h_head(118)   VALUE  'Cash Flow Report',
            h_headline(118) VALUE '=========================',
            h_plan(118)      VALUE '(Plan)',
            h_actual(118)    VALUE '(Actual)'.

*DATA: h_dmshb(21),
DATA: h_dmshb(9),
      fs_amt(21).
FIELD-SYMBOLS: <fs_amt> TYPE ANY.

**********************************************************************
*  SELECTION-SCREEN
**********************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS : s_bukrs FOR ztfi_cmal-bukrs OBLIGATORY DEFAULT 'H201'
                         NO-EXTENSION NO INTERVALS,
                 s_gsber FOR fdsb-gsber.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS : p_gjahr LIKE bkpf-gjahr OBLIGATORY DEFAULT sy-datum(4),
             p_month(2) TYPE n OBLIGATORY DEFAULT sy-datum+4(2),
             p_perid(2) TYPE n OBLIGATORY DEFAULT '04'.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS : p_plan RADIOBUTTON GROUP rd2 DEFAULT 'X' USER-COMMAND exe1.
SELECTION-SCREEN COMMENT 5(26) text-005.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 3.
PARAMETERS : p_wrk RADIOBUTTON GROUP rd4 DEFAULT 'X'.  "working
SELECTION-SCREEN COMMENT 5(26) text-041.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 3.
PARAMETERS : p_frz RADIOBUTTON GROUP rd4.  "freezing
SELECTION-SCREEN COMMENT 5(26) text-042.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 3.
PARAMETERS : p_fnl RADIOBUTTON GROUP rd4.  "finalize
SELECTION-SCREEN COMMENT 5(26) text-043.
SELECTION-SCREEN END OF LINE.
*PARAMETERS : p_wrk RADIOBUTTON GROUP rd4 DEFAULT 'X',  "working
*             p_frz RADIOBUTTON GROUP rd4,              "freezing
*             p_fnl RADIOBUTTON GROUP rd4.              "finalize
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS : p_actual RADIOBUTTON GROUP rd2.
SELECTION-SCREEN COMMENT 5(26) text-006.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-008.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS : p_wkly RADIOBUTTON GROUP rd1 DEFAULT 'X' USER-COMMAND exe2.
SELECTION-SCREEN COMMENT 5(26) text-003.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 3.
PARAMETERS : p_end RADIOBUTTON GROUP rd3 DEFAULT 'X'.  "   - Month end
SELECTION-SCREEN COMMENT 5(26) text-051.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 3.
PARAMETERS : p_over RADIOBUTTON GROUP rd3.  "   - Month over
SELECTION-SCREEN COMMENT 5(26) text-052.
SELECTION-SCREEN END OF LINE.

*PARAMETERS : p_end  RADIOBUTTON GROUP rd3 DEFAULT 'X',
*             p_over RADIOBUTTON GROUP rd3.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS : p_mhly RADIOBUTTON GROUP rd1.
SELECTION-SCREEN COMMENT 5(26) text-004.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-007.
PARAMETERS : p_dispw LIKE t001-waers DEFAULT 'USD',
             p_glied LIKE t038-glied DEFAULT 'P1' OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b4.

*...display option
SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE text-00a.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (15) text-00b.
SELECTION-SCREEN POSITION 33.
*--(scale)
PARAMETERS: p_trunc LIKE rfpdo1-ts70skal  DEFAULT '3'.
SELECTION-SCREEN COMMENT 35(1) text-00c.
*--(decimals)
PARAMETERS: p_decl LIKE rfpdo1-ts70skal  DEFAULT '0'.
SELECTION-SCREEN END OF LINE.
PARAMETERS : p_dst AS CHECKBOX,   "distribution rule
             p_pay AS CHECKBOX.   "payment cycle
SELECTION-SCREEN END OF BLOCK b5.

***********************************************************************
* AT SELECTION-SCREEN
***********************************************************************
AT SELECTION-SCREEN.
  SELECT spras ktopl waers INTO (sv_spras, sv_ktopl, sv_waers)
         FROM t001
         WHERE bukrs IN s_bukrs.
    EXIT.
  ENDSELECT.

* distribution rule, payment cycle
  IF p_dst EQ space AND p_pay EQ space.
    sv_func = 0.   "don't adjust
  ELSEIF p_dst EQ 'X' AND p_pay EQ space.
    sv_func = 1.   "only distribution rule
  ELSEIF p_dst EQ space AND p_pay EQ 'X'.
    sv_func = 2.   "only payment cycle
  ELSEIF p_dst EQ 'X' AND p_pay EQ 'X'.
    sv_func = 3.   "all adjust
  ENDIF.

AT SELECTION-SCREEN OUTPUT.
  IF p_wkly EQ 'X'.
    p_perid = 1.
    LOOP AT SCREEN.
      IF screen-name EQ 'P_PERID'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
*     p_perid = 4.
      IF screen-name EQ 'P_PERID'.
        screen-input = 1.
        MODIFY SCREEN.
      ELSEIF screen-name EQ 'P_END' OR
             screen-name EQ 'P_OVER'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF p_actual EQ 'X'.
    LOOP AT SCREEN.
      IF screen-name EQ 'P_WRK' OR
         screen-name EQ 'P_FRZ' OR
         screen-name EQ 'P_FNL'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
***********************************************************************
* INITIALIZATION
***********************************************************************
INITIALIZATION.
***********************************************************************
* TOP-OF-PAGE
***********************************************************************
TOP-OF-PAGE.
  WRITE :/ h_head LEFT-JUSTIFIED.        "CENTERED.
  WRITE :/ h_headline LEFT-JUSTIFIED.    "CENTERED.
  IF p_plan EQ 'X'.
    WRITE :/ h_plan LEFT-JUSTIFIED.      "CENTERED.
  ELSE.
    WRITE :/ h_actual LEFT-JUSTIFIED.    "CENTERED.
  ENDIF.
  SKIP 1.

  READ TABLE s_datum INDEX 1.
  WRITE :/  'Period : ', s_datum-low.
  DESCRIBE TABLE s_datum LINES sy-subrc.
  READ TABLE s_datum INDEX sy-subrc.
  IF  NOT s_datum-high IS INITIAL.
    WRITE : ' ~ ', s_datum-high.
  ENDIF.
  WRITE AT w_col 'Date : '.
  WRITE sy-datum.
  WRITE :/  'Currency : ', p_dispw.
  WRITE AT w_col 'Page : '.
  WRITE sy-pagno.

  ULINE AT /0(i_line2).
  FORMAT COLOR COL_HEADING.
  WRITE :/ sy-vline NO-GAP, h_fstflg NO-GAP,
           sy-vline NO-GAP, h_scdflg NO-GAP,
           sy-vline NO-GAP, h_group  NO-GAP,
           sy-vline NO-GAP, h_textl  NO-GAP.

  IF p_wkly EQ 'X'.  "weekly
    LOOP AT s_week.
      h_dmshb = s_week-low.
      WRITE : sy-vline NO-GAP, h_dmshb NO-GAP.
    ENDLOOP.
  ELSE.  "monthly
    LOOP AT s_datum.
      CONCATENATE s_datum-low(4) s_datum-low+4(2)
                          INTO h_dmshb SEPARATED BY '/'.
      WRITE : sy-vline NO-GAP, h_dmshb NO-GAP.
    ENDLOOP.
  ENDIF.
  WRITE : sy-vline NO-GAP, h_total NO-GAP.
  WRITE : sy-vline NO-GAP.
  FORMAT COLOR OFF.
*  FORMAT COLOR COL_HEADING.
*  write : / sy-vline no-gap, (40) space, sy-vline NO-GAP,
*            h_group2 UNDER h_group1 NO-GAP,
*            sy-vline NO-GAP.
*  write at (i_line5) space.
* write sy-vline no-gap.
*  FORMAT COLOR OFF.
  ULINE AT /0(i_line2).

***********************************************************************
* START-OF-SELECTION
***********************************************************************
START-OF-SELECTION.
  PERFORM setting_date.

  IF p_plan EQ 'X'.
    IF p_wrk EQ 'X'.
      PERFORM get_plan_wrk.  "working plan
    ELSEIF p_frz EQ 'X'.
      PERFORM get_plan_frz.  "freezing plan
    ELSEIF p_fnl EQ 'X'.
      PERFORM get_plan_fnl.  "frozen plan(finalize plan)
    ENDIF.
  ELSE.
    PERFORM get_actual.
  ENDIF.

*----flexible line-size..
  i_line1 = 85. "layout part
  IF p_wkly EQ 'X'.
    LOOP AT s_week.
*      i_line1 = i_line1 + 22.
      i_line1 = i_line1 + 10.
    ENDLOOP.
  ELSE.
    LOOP AT s_datum.
*      i_line1 = i_line1 + 22.
      i_line1 = i_line1 + 10.
    ENDLOOP.
  ENDIF.
  w_col = i_line1 - 14.
*  i_line1 = i_line1 + 22.  "accumulate part
  i_line2 = i_line1 + 5.
  i_line1 = i_line1 + 10.  "accumulate part
*  i_line2 = i_line1.
*  i_line3 = i_line1 - 42.
  i_line3 = i_line1 - 47.
*  i_line4 = i_line1 - 21.
  i_line4 = i_line1 - 5.
  i_line5 = i_line3 - 9.

  NEW-PAGE LINE-SIZE i_line1.

***********************************************************************
* END-OF-SELECTION
***********************************************************************
END-OF-SELECTION.
  IF p_plan EQ 'X'.
    PERFORM write_plan_data.
  ELSE.
    PERFORM write_actual_data.
  ENDIF.

***********************************************************************
* AT LINE-SELECTION
***********************************************************************
AT LINE-SELECTION.
  CLEAR: r_datum[], r_datum.

  GET CURSOR FIELD v_field.
  IF p_plan EQ 'X'.
    PERFORM get_period USING v_field+12(2).
    PERFORM call_drill_down_plan.
  ELSE.
    PERFORM get_period USING v_field+17(2).
    PERFORM call_drill_down_actual.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  get_plan_wrk
*&---------------------------------------------------------------------*
FORM get_plan_wrk.
  CLEAR: it_fdes[], it_fdes, iztfi_cmap[], iztfi_cmap,
         it_fdes01[], it_fdes01.

  IF p_mhly EQ 'X'.
    CALL FUNCTION 'Z_FFI_GET_CASH_FLOW'
         EXPORTING
              ioption = '05'
              iwaers  = p_dispw
              igroup  = p_glied
              ifunc   = sv_func
*             iorign  = p_orign  "<=== ???
         TABLES
              tbukrs  = s_bukrs
              tdatum  = s_datum
              ftab01  = it_fdes
              ftab02  = iztfi_cmap
              ftab04  = it_fdes01.  "using drill-down

  ELSE.
    CALL FUNCTION 'Z_FFI_GET_CASH_FLOW'
         EXPORTING
              ioption = '06'
              iwaers  = p_dispw
              igroup  = p_glied
              ifunc   = sv_func
*             iorign  = p_orign  "<=== ???
         TABLES
              tbukrs  = s_bukrs
              tdatum  = s_datum
              ftab01  = it_fdes
              ftab02  = iztfi_cmap
              ftab04  = it_fdes01   "using drill-down
              fweek   = s_week. "week
  ENDIF.
ENDFORM.                    " get_plan_wrk
*&---------------------------------------------------------------------*
*&      Form  get_plan_frz
*&---------------------------------------------------------------------*
FORM get_plan_frz.  "freezing data
  CLEAR: it_fdes[], it_fdes, iztfi_cmap[], iztfi_cmap,
         it_fdes01[], it_fdes01.

  IF p_mhly EQ 'X'.
    CALL FUNCTION 'Z_FFI_GET_CASH_FLOW'
         EXPORTING
              ioption = '07'
              iwaers  = p_dispw
         TABLES
              tbukrs  = s_bukrs
              tdatum  = s_datum
              ftab01  = it_fdes
              ftab02  = iztfi_cmap
              ftab04  = it_fdes01. "using drill-down

  ELSE.
    CALL FUNCTION 'Z_FFI_GET_CASH_FLOW'
         EXPORTING
              ioption = '08'
              iwaers  = p_dispw
         TABLES
              tbukrs  = s_bukrs
              tdatum  = s_datum
              ftab01  = it_fdes
              ftab02  = iztfi_cmap
              ftab04  = it_fdes01  "using drill-down
              fweek   = s_week. "week

  ENDIF.
ENDFORM.                    " get_plan_frz
*&---------------------------------------------------------------------*
*&      Form  get_plan_fnl
*&---------------------------------------------------------------------*
FORM get_plan_fnl.
  CLEAR: it_fdes[], it_fdes, iztfi_cmap[], iztfi_cmap,
         it_fdes01[], it_fdes01.

  IF p_mhly EQ 'X'.
    CALL FUNCTION 'Z_FFI_GET_CASH_FLOW'
         EXPORTING
              ioption = '09'
              iwaers  = p_dispw
         TABLES
              tbukrs  = s_bukrs
              tdatum  = s_datum
              ftab01  = it_fdes
              ftab02  = iztfi_cmap
              ftab04  = it_fdes01.

  ELSE.
    CALL FUNCTION 'Z_FFI_GET_CASH_FLOW'
         EXPORTING
              ioption = '10'
              iwaers  = p_dispw
         TABLES
              tbukrs  = s_bukrs
              tdatum  = s_datum
              ftab01  = it_fdes
              ftab02  = iztfi_cmap
              ftab04  = it_fdes01
              fweek   = s_week. "week

  ENDIF.
ENDFORM.                    " get_plan_fnl
*&---------------------------------------------------------------------*
*&      Form  get_actual
*&---------------------------------------------------------------------*
FORM get_actual.
  CLEAR: it_ztfi_cmal[], it_ztfi_cmal, iztfi_cmap[], iztfi_cmap.

  IF p_wkly EQ 'X'.
    CALL FUNCTION 'Z_FFI_GET_CASH_FLOW'
         EXPORTING
              ioption = '04'
              iwover  = p_over
              iwaers  = p_dispw
         TABLES
              tbukrs  = s_bukrs
              tdatum  = s_datum
              ftab01  = it_ztfi_cmal
              ftab02  = iztfi_cmap
              ftab03  = icmal  "add 2004/02/17
              fweek   = s_week. "week

  ELSE.  "monthly print
    CALL FUNCTION 'Z_FFI_GET_CASH_FLOW'
         EXPORTING
              ioption = '03'
              iwaers  = p_dispw
         TABLES
              tbukrs  = s_bukrs
              tdatum  = s_datum
              ftab01  = it_ztfi_cmal
              ftab02  = iztfi_cmap
              ftab03  = icmal. "add 2004/02/17

  ENDIF.
ENDFORM.                    " get_actual
*&---------------------------------------------------------------------*
*&      Form  setting_date
*&---------------------------------------------------------------------*
FORM setting_date.
  DATA: l_mon(2) TYPE n.

  l_mon = p_perid - 1.

  s_datum-sign = 'I'.   s_datum-option = 'BT'.
  CONCATENATE p_gjahr p_month '01' INTO s_datum-low.
  APPEND s_datum.

  DO l_mon TIMES.
    CALL FUNCTION 'MONTH_PLUS_DETERMINE'
         EXPORTING
              months  = 1
              olddate = s_datum-low
         IMPORTING
              newdate = s_datum-low.
    APPEND s_datum.
  ENDDO.

  LOOP AT s_datum.
    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
         EXPORTING
              day_in            = s_datum-low
         IMPORTING
              last_day_of_month = s_datum-high.
    MODIFY s_datum.
  ENDLOOP.
ENDFORM.                    " setting_date
*&---------------------------------------------------------------------*
*&      Form  format_weekly_actual
*&---------------------------------------------------------------------*
FORM format_weekly_actual.

ENDFORM.                    " format_weekly_actual
*&---------------------------------------------------------------------*
*&      Form  write_actual_data
*&---------------------------------------------------------------------*
FORM write_actual_data.
  DATA : l_mode LIKE sy-tabix,
         l_idx(2) TYPE n.
  DATA: w_tmp(9) TYPE c.
* FORMAT COLOR 2.

  SORT it_ztfi_cmal BY fstflg scdflg grupp.

  CLEAR: it_ztfi_cmal.
  CLEAR: isum01, isum02, isum03, sv_subamt1, sv_subamt2, sv_totamt.
  LOOP AT it_ztfi_cmal.
*   FORMAT COLOR 2.
*   l_mode = sy-tabix MOD 2.

*   PERFORM set_mode USING l_mode.
    FORMAT INTENSIFIED OFF.
    NEW-LINE.
    CLEAR: sv_fstflg, sv_scdflg, sv_subamt1, sv_subamt2, sv_totamt.

    AT NEW fstflg.
      PERFORM fstflg_text USING it_ztfi_cmal-fstflg.
    ENDAT.
    AT NEW scdflg.  "add by JIPARK
      PERFORM scdflg_text USING it_ztfi_cmal-scdflg.
    ENDAT.

    PERFORM get_t035t USING it_ztfi_cmal-fstflg it_ztfi_cmal-grupp
                            it_ztfi_cmal-scdflg.

    WRITE : sy-vline NO-GAP, sv_fstflg UNDER h_fstflg NO-GAP.

    IF it_ztfi_cmal-fstflg = '3' OR "divestiture
       it_ztfi_cmal-fstflg = '5' OR "tax
       it_ztfi_cmal-fstflg = '9'.   "account transfer
      CLEAR sv_scdflg.
      WRITE : sv_scdflg UNDER h_scdflg NO-GAP.
    ELSE.
      WRITE : sy-vline NO-GAP, sv_scdflg UNDER h_scdflg NO-GAP.
    ENDIF.

    IF it_ztfi_cmal-fstflg = '9' OR "account transfer
       it_ztfi_cmal-fstflg = 'A'.
      CLEAR it_ztfi_cmal-grupp.
      WRITE :  it_ztfi_cmal-grupp(5) UNDER h_group NO-GAP,
               sy-vline NO-GAP, sv_textl UNDER h_textl NO-GAP.
    ELSE.
    WRITE : sy-vline NO-GAP, it_ztfi_cmal-grupp(5) UNDER h_group NO-GAP,
                         sy-vline NO-GAP, sv_textl UNDER h_textl NO-GAP.
    ENDIF.

    IF p_wkly EQ 'X'.
      LOOP AT s_week.
        l_idx = sy-tabix.
        CONCATENATE 'IT_ZTFI_CMAL-MAMT' l_idx INTO fs_amt.
        ASSIGN (fs_amt) TO <fs_amt>.
*        WRITE : sy-vline NO-GAP, <fs_amt> "UNDER h_dmshb
*                ROUND p_trunc DECIMALS p_decl CURRENCY sv_waers NO-GAP.
        CLEAR w_tmp.
        WRITE <fs_amt> ROUND  3 DECIMALS 0 TO w_tmp.
        WRITE : sy-vline NO-GAP, w_tmp CURRENCY sv_waers NO-GAP.
        sv_subamt1 = sv_subamt1 + <fs_amt>.
      ENDLOOP.
    ELSE.
      LOOP AT s_datum.
        CONCATENATE 'IT_ZTFI_CMAL-MAMT' s_datum-low+4(2) INTO fs_amt.
        ASSIGN (fs_amt) TO <fs_amt>.
*        WRITE : sy-vline NO-GAP, <fs_amt> "UNDER h_dmshb
*               ROUND p_trunc DECIMALS p_decl CURRENCY sv_waers NO-GAP.
        CLEAR w_tmp.
        WRITE <fs_amt> ROUND 3 DECIMALS 0 TO w_tmp.
        WRITE : sy-vline NO-GAP, w_tmp CURRENCY sv_waers NO-GAP.
        sv_subamt1 = sv_subamt1 + <fs_amt>.
      ENDLOOP.
    ENDIF.

*---monthly/weekly cash Balance calculation rule---------------------*
    CASE it_ztfi_cmal-scdflg.
      WHEN c_cate1.   "begin balance
*       first month(week), begin balance => accumulated begin balance
        IF p_wkly EQ 'X'.
          l_idx = '01'.
          CONCATENATE 'IT_ZTFI_CMAL-MAMT' l_idx INTO fs_amt.
        ELSE.
          READ TABLE s_datum INDEX 1.
          CONCATENATE 'IT_ZTFI_CMAL-MAMT' s_datum-low+4(2) INTO fs_amt.
        ENDIF.
        ASSIGN (fs_amt) TO <fs_amt>.
        sv_subamt1 = <fs_amt>.
      WHEN c_cate2 OR c_cate3 OR c_catea.  "open tran, bank recon..
*         => summarize amount
      WHEN c_cate4. "ending balance
*       last month(week), begin balance => accumulated ending balance
        IF p_wkly EQ 'X'.
          l_idx = sy-tfill.  "Internal tables, current number of lines
          CONCATENATE 'IT_ZTFI_CMAL-MAMT' l_idx INTO fs_amt.
        ELSE.
          SORT s_datum BY low DESCENDING.
          READ TABLE s_datum INDEX 1.
          CONCATENATE 'IT_ZTFI_CMAL-MAMT' s_datum-low+4(2) INTO fs_amt.
        ENDIF.
        ASSIGN (fs_amt) TO <fs_amt>.
        sv_subamt1 = <fs_amt>.
    ENDCASE.

    SORT s_datum BY low.

*   IF it_ztfi_cmal-fstflg EQ 'A'.  "cash balance
*     CLEAR sv_subamt1.
*   ENDIF.
*---------------------------------------------------------------------*

*    WRITE : sy-vline NO-GAP, sv_subamt1 "UNDER h_dmshb
*            ROUND p_trunc DECIMALS p_decl CURRENCY sv_waers NO-GAP.

    CLEAR w_tmp.
    WRITE sv_subamt1 ROUND 3 DECIMALS 0 TO w_tmp.
    WRITE: sy-vline NO-GAP, w_tmp CURRENCY sv_waers NO-GAP.
    WRITE : sy-vline NO-GAP.

    HIDE : it_ztfi_cmal-grupp.

    AT END OF fstflg.
      SUM.
      NEW-LINE.
      FORMAT COLOR OFF.
      CASE it_ztfi_cmal-fstflg.
        WHEN '5' OR "tax
             '3'.   "Divestiture (+03/29)
          WRITE: AT 1(1) sy-vline, 43(1) sy-vline.
          WRITE AT 43(i_line3) sy-uline.
        WHEN '9'.   "account transfer
          WRITE AT 1(i_line2) sy-uline.
          CONTINUE.
        WHEN 'A'.
          ULINE AT /0(i_line2).
          CONTINUE.
        WHEN OTHERS.
          WRITE: AT 1(1) sy-vline, 17(1) sy-vline.
          WRITE AT 1(i_line4) sy-uline.
      ENDCASE.

      FORMAT COLOR 3.
      FORMAT INTENSIFIED OFF.
*      WRITE :/ sy-vline NO-GAP, (83) space.
      WRITE :/ sy-vline NO-GAP, (78) space.

      IF p_wkly EQ 'X'.
        CLEAR l_idx.
        LOOP AT s_week.
          l_idx = sy-tabix.
          CONCATENATE 'IT_ZTFI_CMAL-MAMT' l_idx INTO fs_amt.
          ASSIGN (fs_amt) TO <fs_amt>.
          IF sy-tabix = 1.
*            WRITE :(21) <fs_amt> "UNDER h_dmshb
*                        ROUND p_trunc DECIMALS p_decl
*                        CURRENCY sv_waers NO-GAP RIGHT-JUSTIFIED.
            CLEAR w_tmp.
            WRITE <fs_amt> ROUND 3 DECIMALS 0 TO w_tmp.
            WRITE : (9) w_tmp CURRENCY sv_waers NO-GAP RIGHT-JUSTIFIED.
          ELSE.
*            WRITE :(22) <fs_amt> "UNDER h_dmshb
*                        ROUND p_trunc DECIMALS p_decl
*                        CURRENCY sv_waers NO-GAP RIGHT-JUSTIFIED.
            CLEAR w_tmp.
            WRITE <fs_amt> ROUND 3 DECIMALS 0 TO w_tmp.
            WRITE : (10) w_tmp CURRENCY sv_waers NO-GAP RIGHT-JUSTIFIED.

          ENDIF.
          sv_subamt2 = sv_subamt2 + <fs_amt>.  "total
        ENDLOOP.
      ELSE.
        LOOP AT s_datum.
          CONCATENATE 'IT_ZTFI_CMAL-MAMT' s_datum-low+4(2) INTO fs_amt.
          ASSIGN (fs_amt) TO <fs_amt>.
          IF sy-tabix = 1.
*            WRITE :(21) <fs_amt> "UNDER h_dmshb
*                        ROUND p_trunc DECIMALS p_decl
*                        CURRENCY sv_waers NO-GAP RIGHT-JUSTIFIED.
            CLEAR w_tmp.
            WRITE <fs_amt> ROUND 3 DECIMALS 0 TO w_tmp.
            WRITE: (9) w_tmp CURRENCY sv_waers NO-GAP RIGHT-JUSTIFIED.
          ELSE.
*            WRITE :(22) <fs_amt> "UNDER h_dmshb
*                        ROUND p_trunc DECIMALS p_decl
*                        CURRENCY sv_waers NO-GAP RIGHT-JUSTIFIED.
            CLEAR w_tmp.
            WRITE <fs_amt> ROUND 3 DECIMALS 0 TO w_tmp.
            WRITE: (10) w_tmp CURRENCY sv_waers NO-GAP RIGHT-JUSTIFIED.
          ENDIF.

          sv_subamt2 = sv_subamt2 + <fs_amt>.  "total
        ENDLOOP.
      ENDIF.
*      WRITE :(22) sv_subamt2 ROUND p_trunc DECIMALS p_decl
*                           CURRENCY sv_waers NO-GAP RIGHT-JUSTIFIED.
      CLEAR w_tmp.
      WRITE sv_subamt2 ROUND 3 DECIMALS 0 TO w_tmp.
      WRITE: (10) w_tmp CURRENCY sv_waers NO-GAP RIGHT-JUSTIFIED.
      WRITE : sy-vline NO-GAP.
      FORMAT COLOR OFF.
*      ULINE at /0(100).
      ULINE AT /0(i_line2).
    ENDAT.

    AT END OF sumflg.
      IF it_ztfi_cmal-sumflg <> space.
        SUM.
        PERFORM write_sub_total USING it_ztfi_cmal-sumflg.
        IF p_wkly EQ 'X'.
          CLEAR l_idx.
          LOOP AT s_week.
            l_idx = sy-tabix.
            CONCATENATE 'IT_ZTFI_CMAL-MAMT' l_idx INTO fs_amt.
            ASSIGN (fs_amt) TO <fs_amt>.

            PERFORM add_summary USING l_idx <fs_amt>
                                      it_ztfi_cmal-sumflg.
*            WRITE :(22) <fs_amt> CURRENCY sv_waers
*                        ROUND p_trunc DECIMALS p_decl
*                        NO-GAP RIGHT-JUSTIFIED.
            CLEAR w_tmp.
            WRITE <fs_amt> ROUND 3 DECIMALS 0 TO w_tmp.
            WRITE :(10) w_tmp CURRENCY sv_waers NO-GAP RIGHT-JUSTIFIED.
            sv_totamt = sv_totamt + <fs_amt>.  "total
          ENDLOOP.
        ELSE.
          LOOP AT s_datum.
            CONCATENATE 'IT_ZTFI_CMAL-MAMT' s_datum-low+4(2)
                                            INTO fs_amt.
            ASSIGN (fs_amt) TO <fs_amt>.

            PERFORM add_summary USING s_datum-low+4(2) <fs_amt>
                                      it_ztfi_cmal-sumflg.
*            WRITE :(22) <fs_amt> CURRENCY sv_waers
*                        ROUND p_trunc DECIMALS p_decl
*                        NO-GAP RIGHT-JUSTIFIED.
            CLEAR w_tmp.
            WRITE <fs_amt> ROUND 3 DECIMALS 0 TO w_tmp.
            WRITE :(10) w_tmp CURRENCY sv_waers NO-GAP RIGHT-JUSTIFIED.
            sv_totamt = sv_totamt + <fs_amt>.  "total
          ENDLOOP.
        ENDIF.
      ENDIF.

*      WRITE :(22) sv_totamt ROUND p_trunc DECIMALS p_decl
*                            CURRENCY sv_waers NO-GAP RIGHT-JUSTIFIED.
      CLEAR w_tmp.
      WRITE sv_totamt ROUND 3 DECIMALS 0 TO w_tmp.
      WRITE :(10) w_tmp CURRENCY sv_waers NO-GAP RIGHT-JUSTIFIED.
      WRITE : sy-vline NO-GAP.
      ULINE AT (i_line2).
      FORMAT COLOR OFF.
    ENDAT.
  ENDLOOP.
ENDFORM.                    " write_actual_data
*&---------------------------------------------------------------------*
*&      Form  format_weekly_plan
*&---------------------------------------------------------------------*
FORM format_weekly_plan.

ENDFORM.                    " format_weekly_plan
*&---------------------------------------------------------------------*
*&      Form  write_plan_data
*&---------------------------------------------------------------------*
FORM write_plan_data.
  DATA : l_mode LIKE sy-tabix,
         l_idx(2) TYPE n.
  DATA: w_tmp(9) TYPE c.
* FORMAT COLOR 2.

  SORT it_fdes BY fstflg scdflg grupp.

  CLEAR: it_fdes.
  CLEAR: isum01, isum02, isum03.
  LOOP AT it_fdes.
    CLEAR : sv_subamt1, sv_subamt2, sv_totamt,
            w_tmp.
*   FORMAT COLOR 2.
*   l_mode = sy-tabix MOD 2.

*   PERFORM set_mode USING l_mode.
    FORMAT INTENSIFIED OFF.
    NEW-LINE.
    CLEAR: sv_fstflg, sv_scdflg.

    AT NEW fstflg.
      PERFORM fstflg_text USING it_fdes-fstflg.
    ENDAT.
    AT NEW scdflg.
      PERFORM scdflg_text USING it_fdes-scdflg.
    ENDAT.

    PERFORM get_t035t USING it_fdes-fstflg it_fdes-grupp
                            it_fdes-scdflg.

    WRITE : sy-vline NO-GAP, sv_fstflg UNDER h_fstflg NO-GAP.

    IF it_fdes-fstflg = '3' OR "divestiture
       it_fdes-fstflg = '5' OR "tax
       it_fdes-fstflg = '9'.   "account transfer
      CLEAR sv_scdflg.
      WRITE : sv_scdflg UNDER h_scdflg NO-GAP.
    ELSE.
      WRITE : sy-vline NO-GAP, sv_scdflg UNDER h_scdflg NO-GAP.
    ENDIF.

    IF it_fdes-fstflg = '9' OR  "account transfer
       it_fdes-fstflg = 'A'.
      CLEAR it_fdes-grupp.
      WRITE :  it_fdes-grupp(5) UNDER h_group NO-GAP,
               sy-vline NO-GAP, sv_textl UNDER h_textl NO-GAP.
    ELSE.
      WRITE : sy-vline NO-GAP, it_fdes-grupp(5) UNDER h_group NO-GAP,
              sy-vline NO-GAP, sv_textl UNDER h_textl NO-GAP.
    ENDIF.

    IF p_wkly EQ 'X'.
      LOOP AT s_week.
        l_idx = sy-tabix.
        CONCATENATE 'IT_FDES-MAMT' l_idx INTO fs_amt.
        ASSIGN (fs_amt) TO <fs_amt>.
*        WRITE : sy-vline NO-GAP, <fs_amt> "UNDER h_dmshb
*                ROUND p_trunc DECIMALS p_decl CURRENCY sv_waers NO-GAP.
        CLEAR w_tmp.
        WRITE <fs_amt> ROUND 3 DECIMALS 0 TO w_tmp.
        WRITE : sy-vline NO-GAP, w_tmp CURRENCY sv_waers NO-GAP.
        sv_subamt1 = sv_subamt1 + <fs_amt>.
      ENDLOOP.
    ELSE.
      LOOP AT s_datum.
        CONCATENATE 'IT_FDES-MAMT' s_datum-low+4(2) INTO fs_amt.
        ASSIGN (fs_amt) TO <fs_amt>.
*        WRITE : sy-vline NO-GAP, <fs_amt> "UNDER h_dmshb
*                ROUND p_trunc DECIMALS p_decl CURRENCY sv_waers NO-GAP.
        CLEAR w_tmp.
        WRITE <fs_amt> ROUND 3 DECIMALS 0 TO w_tmp.
        WRITE : sy-vline NO-GAP, w_tmp CURRENCY sv_waers NO-GAP.
        sv_subamt1 = sv_subamt1 + <fs_amt>.
      ENDLOOP.
    ENDIF.

*---monthly/weekly cash Balance calculation rule---------------------*
    CASE it_fdes-scdflg.
      WHEN c_cate1.   "begin balance : A100
*       first month(week), begin balance => accumulated begin balance
        IF p_wkly EQ 'X'.
          l_idx = '01'.
          CONCATENATE 'IT_FDES-MAMT' l_idx INTO fs_amt.
        ELSE.
          READ TABLE s_datum INDEX 1.
          CONCATENATE 'IT_FDES-MAMT' s_datum-low+4(2) INTO fs_amt.
        ENDIF.
        ASSIGN (fs_amt) TO <fs_amt>.
        sv_subamt1 = <fs_amt>.
      WHEN c_cate2 OR c_cate3 OR c_catea. "open tran, bank recon
*         => summarize amount
      WHEN c_cate4. "ending balance : A400
*       last month(week), begin balance => accumulated ending balance
        IF p_wkly EQ 'X'.
          l_idx = sy-tfill.  "Internal tables, current number of lines
          CONCATENATE 'IT_FDES-MAMT' l_idx INTO fs_amt.
        ELSE.
          SORT s_datum BY low DESCENDING.
          READ TABLE s_datum INDEX 1.
          CONCATENATE 'IT_FDES-MAMT' s_datum-low+4(2) INTO fs_amt.
        ENDIF.
        ASSIGN (fs_amt) TO <fs_amt>.
        sv_subamt1 = <fs_amt>.
    ENDCASE.

    SORT s_datum BY low.
*---------------------------------------------------------------------*
*    WRITE : sy-vline NO-GAP, sv_subamt1 ROUND p_trunc DECIMALS p_decl
*                             CURRENCY sv_waers NO-GAP.
    CLEAR w_tmp.
    WRITE sv_subamt1 ROUND 3 DECIMALS 0 TO w_tmp.
    WRITE : sy-vline NO-GAP, w_tmp CURRENCY sv_waers NO-GAP.
    WRITE : sy-vline NO-GAP.

    HIDE : it_fdes-grupp.

    AT END OF fstflg.
      SUM.
      NEW-LINE.
      FORMAT COLOR OFF.
      CASE it_fdes-fstflg.
        WHEN '5' OR "tax
             '3'.   "Divestiture (+03/29)
          WRITE: AT 1(1) sy-vline, 43(1) sy-vline.
          WRITE AT 43(i_line3) sy-uline.
        WHEN '9' OR "account transfer
             'A'.   "balance..
          WRITE AT 1(i_line2) sy-uline.
          CONTINUE.
        WHEN OTHERS.
          WRITE: AT 1(1) sy-vline, 17(1) sy-vline.
          WRITE AT 1(i_line4) sy-uline.
      ENDCASE.

      FORMAT COLOR 3.
      FORMAT INTENSIFIED OFF.
*      WRITE :/ sy-vline NO-GAP, (83) space.
      WRITE :/ sy-vline NO-GAP, (78) space.

      IF p_wkly EQ 'X'.
        CLEAR l_idx.
        LOOP AT s_week.
          l_idx = sy-tabix.
          CONCATENATE 'IT_FDES-MAMT' l_idx INTO fs_amt.
          ASSIGN (fs_amt) TO <fs_amt>.
          IF sy-tabix = 1.
*            WRITE :(21) <fs_amt> "UNDER h_dmshb
*                        ROUND p_trunc DECIMALS p_decl
*                        CURRENCY sv_waers NO-GAP RIGHT-JUSTIFIED.
            CLEAR w_tmp.
            WRITE <fs_amt> ROUND 3 DECIMALS 0 TO w_tmp.
            WRITE : (9) w_tmp CURRENCY sv_waers NO-GAP RIGHT-JUSTIFIED.
          ELSE.
*            WRITE :(22) <fs_amt> "UNDER h_dmshb
*                        ROUND p_trunc DECIMALS p_decl
*                        CURRENCY sv_waers NO-GAP RIGHT-JUSTIFIED.
            CLEAR w_tmp.
            WRITE <fs_amt> ROUND 3 DECIMALS 0 TO w_tmp.
            WRITE : (10) w_tmp CURRENCY sv_waers NO-GAP RIGHT-JUSTIFIED.
          ENDIF.
          sv_subamt2 = sv_subamt2 + <fs_amt>.  "total
        ENDLOOP.
      ELSE.
        LOOP AT s_datum.
          CONCATENATE 'IT_FDES-MAMT' s_datum-low+4(2) INTO fs_amt.
          ASSIGN (fs_amt) TO <fs_amt>.
          IF sy-tabix = 1.
*            WRITE :(21) <fs_amt> "UNDER h_dmshb
*                        ROUND p_trunc DECIMALS p_decl
*                        CURRENCY sv_waers NO-GAP RIGHT-JUSTIFIED.
            CLEAR w_tmp.
            WRITE <fs_amt> ROUND 3 DECIMALS 0 TO w_tmp.
            WRITE : (9) w_tmp CURRENCY sv_waers NO-GAP RIGHT-JUSTIFIED.
          ELSE.
*            WRITE :(22) <fs_amt> "UNDER h_dmshb
*                        ROUND p_trunc DECIMALS p_decl
*                        CURRENCY sv_waers NO-GAP RIGHT-JUSTIFIED.
            CLEAR w_tmp.
            WRITE <fs_amt> ROUND 3 DECIMALS 0 TO w_tmp.
            WRITE : (10) w_tmp CURRENCY sv_waers NO-GAP RIGHT-JUSTIFIED.
          ENDIF.
          sv_subamt2 = sv_subamt2 + <fs_amt>.  "total
        ENDLOOP.
      ENDIF.
*      WRITE :(22) sv_subamt2 ROUND p_trunc DECIMALS p_decl
*                             CURRENCY sv_waers NO-GAP RIGHT-JUSTIFIED.
      CLEAR w_tmp.
      WRITE sv_subamt2 ROUND 3 DECIMALS 0 TO w_tmp.
      WRITE : (10) w_tmp CURRENCY sv_waers NO-GAP RIGHT-JUSTIFIED.
      WRITE : sy-vline NO-GAP.
      FORMAT COLOR OFF.
      ULINE AT /0(i_line2).
    ENDAT.

    AT END OF sumflg.
      IF it_fdes-sumflg <> space.
        SUM.
        PERFORM write_sub_total USING it_fdes-sumflg.
        IF p_wkly EQ 'X'.
          CLEAR l_idx.
          LOOP AT s_week.
            l_idx = sy-tabix.
            CONCATENATE 'IT_FDES-MAMT' l_idx INTO fs_amt.
            ASSIGN (fs_amt) TO <fs_amt>.

            PERFORM add_summary USING l_idx <fs_amt>
                                      it_fdes-sumflg.
*            WRITE :(22) <fs_amt> CURRENCY sv_waers
*                                 ROUND p_trunc DECIMALS p_decl
*                                 NO-GAP RIGHT-JUSTIFIED.
            CLEAR w_tmp.
            WRITE <fs_amt> ROUND 3 DECIMALS 0 TO w_tmp.
            WRITE : (10) w_tmp CURRENCY sv_waers NO-GAP RIGHT-JUSTIFIED.
            sv_totamt = sv_totamt + <fs_amt>.  "total
          ENDLOOP.
        ELSE.
          LOOP AT s_datum.
            CONCATENATE 'IT_FDES-MAMT' s_datum-low+4(2)
                                       INTO fs_amt.
            ASSIGN (fs_amt) TO <fs_amt>.

            PERFORM add_summary USING s_datum-low+4(2) <fs_amt>
                                      it_fdes-sumflg.
*            WRITE :(22) <fs_amt> CURRENCY sv_waers
*                                 ROUND p_trunc DECIMALS p_decl
*                                 NO-GAP RIGHT-JUSTIFIED.
            CLEAR w_tmp.
            WRITE <fs_amt> ROUND 3 DECIMALS 0 TO w_tmp.
            WRITE : (10) w_tmp CURRENCY sv_waers NO-GAP RIGHT-JUSTIFIED.
            sv_totamt = sv_totamt + <fs_amt>.  "total
          ENDLOOP.
        ENDIF.
      ENDIF.
*      WRITE :(22) sv_totamt ROUND p_trunc DECIMALS p_decl
*                            CURRENCY sv_waers NO-GAP RIGHT-JUSTIFIED.
      CLEAR w_tmp.
      WRITE sv_totamt ROUND 3 DECIMALS 0 TO w_tmp.
      WRITE : (10) w_tmp CURRENCY sv_waers NO-GAP RIGHT-JUSTIFIED.
      WRITE : sy-vline NO-GAP.
      ULINE AT (i_line2).
      FORMAT COLOR OFF.
      IF it_fdes-sumflg = '1' OR it_fdes-sumflg = '3'.
        NEW-PAGE.
      ENDIF.
    ENDAT.
  ENDLOOP.
ENDFORM.                    " write_plan_data
*&---------------------------------------------------------------------
*&      Form  SET_MODE
*&---------------------------------------------------------------------
FORM set_mode USING    p_mode.
  IF p_mode = 0.
    FORMAT INTENSIFIED ON.
  ELSE.
    FORMAT INTENSIFIED OFF.
  ENDIF.
ENDFORM.                    " SET_MODE
*&---------------------------------------------------------------------
*&      Form  FSTFLG_TEXT
*&---------------------------------------------------------------------
FORM fstflg_text USING fstflg.
  CASE fstflg.
    WHEN '1'.
      MOVE text-011  TO sv_fstflg.
    WHEN '2'.
      MOVE text-012  TO sv_fstflg.
    WHEN '3'.
      MOVE text-013  TO sv_fstflg.
    WHEN '4'.
      MOVE text-014  TO sv_fstflg.
    WHEN '5'.
      MOVE text-015  TO sv_fstflg.
    WHEN '6'.
      MOVE text-016  TO sv_fstflg.
    WHEN '7'.
      MOVE text-017  TO sv_fstflg.
    WHEN '8'.
      MOVE text-018  TO sv_fstflg.
    WHEN '9'.
      MOVE text-019  TO sv_fstflg.
    WHEN 'A'.
      MOVE text-01a  TO sv_fstflg.
  ENDCASE.
ENDFORM.                    " FSTFLG_TEXT
*&---------------------------------------------------------------------
*&      Form  SCDFLG_text
*&---------------------------------------------------------------------
FORM scdflg_text USING p_fstflg.
  READ TABLE iztfi_cmap WITH KEY grptm = p_fstflg.
  IF sy-subrc EQ 0.
    sv_scdflg = iztfi_cmap-grptt.
  ENDIF.
ENDFORM.                    " SCDFLG_text
*&---------------------------------------------------------------------
*&      Form  GET_T035T
*&---------------------------------------------------------------------
FORM get_t035t USING p_fstflg p_grupp p_scdflg.
  CLEAR : sv_textl.

  CASE p_fstflg.
*   available cash bal.->bank clg. acct.
    WHEN 'A'.
      SELECT SINGLE ltext FROM t036t INTO sv_textl "level name
       WHERE spras = sv_spras
         AND ebene = p_grupp.
      IF   sy-subrc <> 0.
        MOVE space  TO sv_textl.
      ENDIF.

*   account transfer
    WHEN '9'.
*     PERFORM set_detail_itemtext USING text-020  text-021 ''
*                                       p_scdflg.
      READ TABLE iztfi_cmap WITH KEY grptm = p_scdflg.
      IF sy-subrc EQ 0.
        MOVE iztfi_cmap-grptt TO sv_textl.
      ENDIF.

    WHEN OTHERS.
      SELECT SINGLE textl FROM t035t INTO sv_textl "group name
       WHERE spras = sv_spras
         AND grupp = p_grupp.
      IF   sy-subrc <> 0.
        MOVE space  TO sv_textl.
      ENDIF.
  ENDCASE.
ENDFORM.                                                    " GET_T035T
*&---------------------------------------------------------------------
*&      Form  write_sub_total
*&---------------------------------------------------------------------
FORM write_sub_total USING p_flag.
  FORMAT COLOR 5.
  FORMAT INTENSIFIED OFF.
  CASE p_flag.
    WHEN '1'.
      WRITE : / sy-vline NO-GAP, text-030.
    WHEN '2'.
      WRITE : / sy-vline NO-GAP, text-031.
    WHEN '3'.
      WRITE : / sy-vline NO-GAP, text-032.
    WHEN '4'.
      WRITE : / sy-vline NO-GAP, text-033.
  ENDCASE.
*  WRITE :(38) space.
  WRITE :(33) space.
ENDFORM.                    " write_sub_total
*&---------------------------------------------------------------------
*&      Form  set_detail_itemtext
*&---------------------------------------------------------------------
FORM set_detail_itemtext USING p_value1 p_value2 p_value3 p_value4.
  IF p_value4+1(1) EQ '0'.
    sv_textl = p_value1.
  ELSEIF p_value4+1(1) EQ '1'.
    sv_textl = p_value2.
  ELSEIF p_value4+1(1) EQ '2'.
    sv_textl = p_value3.
  ENDIF.
ENDFORM.                    " set_detail_itemtext
*&---------------------------------------------------------------------*
*&      Form  add_summary
*&---------------------------------------------------------------------*
FORM add_summary USING p_idx p_sum p_flag.
  IF p_flag EQ '1'.
    CASE p_idx.
      WHEN '01'.
        isum01-amt01 = p_sum.
      WHEN '02'.
        isum01-amt02 = p_sum.
      WHEN '03'.
        isum01-amt03 = p_sum.
      WHEN '04'.
        isum01-amt04 = p_sum.
      WHEN '05'.
        isum01-amt05 = p_sum.
      WHEN '06'.
        isum01-amt06 = p_sum.
      WHEN '07'.
        isum01-amt07 = p_sum.
      WHEN '08'.
        isum01-amt08 = p_sum.
      WHEN '09'.
        isum01-amt09 = p_sum.
      WHEN '10'.
        isum01-amt10 = p_sum.
      WHEN '11'.
        isum01-amt11 = p_sum.
      WHEN '12'.
        isum01-amt12 = p_sum.
    ENDCASE.
  ELSEIF p_flag EQ '2'.
    CASE p_idx.
      WHEN '01'.
        isum02-amt01 = p_sum.
        p_sum = p_sum + isum01-amt01.
      WHEN '02'.
        isum02-amt02 = p_sum.
        p_sum = p_sum + isum01-amt02.
      WHEN '03'.
        isum02-amt03 = p_sum.
        p_sum = p_sum + isum01-amt03.
      WHEN '04'.
        isum02-amt04 = p_sum.
        p_sum = p_sum + isum01-amt04.
      WHEN '05'.
        isum02-amt05 = p_sum.
        p_sum = p_sum + isum01-amt05.
      WHEN '06'.
        isum02-amt06 = p_sum.
        p_sum = p_sum + isum01-amt06.
      WHEN '07'.
        isum02-amt07 = p_sum.
        p_sum = p_sum + isum01-amt07.
      WHEN '08'.
        isum02-amt08 = p_sum.
        p_sum = p_sum + isum01-amt08.
      WHEN '09'.
        isum02-amt09 = p_sum.
        p_sum = p_sum + isum01-amt09.
      WHEN '10'.
        isum02-amt10 = p_sum.
        p_sum = p_sum + isum01-amt10.
      WHEN '11'.
        isum02-amt11 = p_sum.
        p_sum = p_sum + isum01-amt11.
      WHEN '12'.
        isum02-amt12 = p_sum.
        p_sum = p_sum + isum01-amt12.
    ENDCASE.
  ELSEIF p_flag EQ '3'.
    CASE p_idx.
      WHEN '01'.
        isum03-amt01 = p_sum.
        p_sum = p_sum + isum02-amt01 + isum01-amt01.
      WHEN '02'.
        isum03-amt02 = p_sum.
        p_sum = p_sum + isum02-amt02 + isum01-amt02.
      WHEN '03'.
        isum03-amt03 = p_sum.
        p_sum = p_sum + isum02-amt03 + isum01-amt03.
      WHEN '04'.
        isum03-amt04 = p_sum.
        p_sum = p_sum + isum02-amt04 + isum01-amt04.
      WHEN '05'.
        isum03-amt05 = p_sum.
        p_sum = p_sum + isum02-amt05 + isum01-amt05.
      WHEN '06'.
        isum03-amt06 = p_sum.
        p_sum = p_sum + isum02-amt06 + isum01-amt06.
      WHEN '07'.
        isum03-amt07 = p_sum.
        p_sum = p_sum + isum02-amt07 + isum01-amt07.
      WHEN '08'.
        isum03-amt08 = p_sum.
        p_sum = p_sum + isum02-amt08 + isum01-amt08.
      WHEN '09'.
        isum03-amt09 = p_sum.
        p_sum = p_sum + isum02-amt09 + isum01-amt09.
      WHEN '10'.
        isum03-amt10 = p_sum.
        p_sum = p_sum + isum02-amt10 + isum01-amt10.
      WHEN '11'.
        isum03-amt11 = p_sum.
        p_sum = p_sum + isum02-amt11 + isum01-amt11.
      WHEN '12'.
        isum03-amt12 = p_sum.
        p_sum = p_sum + isum02-amt12 + isum01-amt12.
    ENDCASE.
  ELSEIF p_flag EQ '4'.
    CASE p_idx.
      WHEN '01'.
        p_sum = p_sum + isum03-amt01
                + isum02-amt01 + isum01-amt01.
      WHEN '02'.
        p_sum = p_sum + isum03-amt02
                + isum02-amt02 + isum01-amt02.
      WHEN '03'.
        p_sum = p_sum + isum03-amt03
                + isum02-amt03 + isum01-amt03.
      WHEN '04'.
        p_sum = p_sum + isum03-amt04
                + isum02-amt04 + isum01-amt04.
      WHEN '05'.
        p_sum = p_sum + isum03-amt05
                + isum02-amt05 + isum01-amt05.
      WHEN '06'.
        p_sum = p_sum + isum03-amt06
                + isum02-amt06 + isum01-amt06.
      WHEN '07'.
        p_sum = p_sum + isum03-amt07
                + isum02-amt07 + isum01-amt07.
      WHEN '08'.
        p_sum = p_sum + isum03-amt08
                + isum02-amt08 + isum01-amt08.
      WHEN '09'.
        p_sum = p_sum + isum03-amt09
                + isum02-amt09 + isum01-amt09.
      WHEN '10'.
        p_sum = p_sum + isum03-amt10
                + isum02-amt10 + isum01-amt10.
      WHEN '11'.
        p_sum = p_sum + isum03-amt11
                + isum02-amt11 + isum01-amt11.
      WHEN '12'.
        p_sum = p_sum + isum03-amt12
                + isum02-amt12 + isum01-amt12.
    ENDCASE.
  ENDIF.
ENDFORM.                    " add_summary
*&---------------------------------------------------------------------*
*&      Form  call_drill_down_plan
*&---------------------------------------------------------------------*
FORM call_drill_down_plan.
  IF p_frz EQ 'X' OR p_fnl EQ 'X'.
    CLEAR: it_pltm[], it_pltm.

    LOOP AT it_fdes01 WHERE grupp EQ it_fdes-grupp
                      AND   datum IN r_datum.
      IF p_wkly EQ 'X'.
        CALL FUNCTION 'DATE_GET_WEEK'
             EXPORTING
                  date = it_fdes01-datum
             IMPORTING
                  week = v_week.
        CHECK v_week EQ s_week-low.
      ENDIF.

      MOVE-CORRESPONDING it_fdes01 TO it_pltm.
      APPEND it_pltm.  CLEAR it_pltm.
    ENDLOOP.

    CHECK NOT it_pltm[] IS INITIAL.
    EXPORT it_pltm sv_waers sv_spras TO MEMORY ID 'Z15'.
    SUBMIT zrfit15 AND RETURN.

  ELSE.
*----Blocking 2004/06/07
*    SORT it_fdes01 BY bukrs bnkko grupp ebene.
*    LOOP AT it_fdes01 WHERE grupp EQ it_fdes-grupp
*                      AND   datum IN r_datum.
*      IF p_wkly EQ 'X'.
*        CALL FUNCTION 'DATE_GET_WEEK'
*             EXPORTING
*                  date = it_fdes01-datum
*             IMPORTING
*                  week = v_week.
*        CHECK v_week EQ s_week-low.
*      ENDIF.
*
*      AT NEW ebene.
*        PERFORM drill_down_plan USING it_fdes01-grupp
*                                      it_fdes01-ebene
*                                      it_fdes01-bnkko
*                                      it_fdes01-bukrs.
*      ENDAT.
*    ENDLOOP.
  ENDIF.
ENDFORM.                    " call_drill_down_plan
*&---------------------------------------------------------------------*
*&      Form  call_drill_down_actual
*&---------------------------------------------------------------------*
FORM call_drill_down_actual.
  CLEAR: it_ddwn[], it_ddwn.

  LOOP AT icmal WHERE grupp EQ it_ztfi_cmal-grupp
                AND   budat IN r_datum.
    IF p_wkly EQ 'X'.
      CALL FUNCTION 'DATE_GET_WEEK'
           EXPORTING
                date = icmal-budat
           IMPORTING
                week = v_week.
      CHECK v_week EQ s_week-low.
    ENDIF.

    MOVE-CORRESPONDING icmal  TO  it_ddwn.
    CASE icmal-koart.
      WHEN 'D'.  "customer
        it_ddwn-kacct = icmal-kunnr.
      WHEN 'K'.  "vendors
        it_ddwn-kacct = icmal-lifnr.
      WHEN 'S'.  "g/l accounts
        it_ddwn-kacct = icmal-hkont.
    ENDCASE.
*...FM data
    SELECT SINGLE fipos  fincode fistl knbelnr kngjahr
              INTO (it_ddwn-fipos,  it_ddwn-fincode, it_ddwn-fistl,
                    it_ddwn-knbelnr, it_ddwn-kngjahr)
                  FROM ztfi_fmal
                  WHERE bukrs EQ icmal-bukrs
                  AND   gjahr EQ icmal-gjahr
                  AND   belnr EQ icmal-belnr
                  AND   datum EQ icmal-budat.

*...group desc.
    SELECT SINGLE textl INTO it_ddwn-grptt
                  FROM t035t
                  WHERE spras = sv_spras
                  AND   grupp = icmal-grupp.

    APPEND it_ddwn.  CLEAR it_ddwn.
  ENDLOOP.

  CHECK NOT it_ddwn[] IS INITIAL.
  SORT it_ddwn BY budat dispw.
  EXPORT it_ddwn sv_waers TO MEMORY ID 'Z10'.
  SUBMIT zrfit10 AND RETURN.
ENDFORM.                    " call_drill_down_actual
*&---------------------------------------------------------------------*
*&      Form  drill_down_plan
*&---------------------------------------------------------------------*
FORM drill_down_plan USING grupp ebene bnkko bukrs.
  DATA: l_fdgrp LIKE fdsr-grupp.
  STATICS l_st_immo_gsart LIKE RANGE OF t036v-gsart.
  DATA:   l_immo_gsart LIKE LINE OF l_st_immo_gsart.

*----------------------------Verprobung Ebene------------------------*
  PERFORM t036_lesen.                  "Dispositionsebenen-Tabelle
  LOOP AT i036 WHERE ebene = ebene.
    EXIT.
  ENDLOOP.
  IF sy-subrc <> 0.
    MESSAGE e105(rq) WITH ebene.
  ENDIF.

*------ Selektionszeitraum bestimmen --------------------------------*
  PERFORM ermittlung_von_bis_datum.
*------ bei Liq.vorschau ggf. von/bis-Daten ändern, falls Zahlungs-
*------ termine bei FF70-Anzeige berücksichtigt wurden
*  if  p_zahll    = 'X'      "X-Zahlungstermine berücksichtigen
** 1-es gibt Verschiebungen aufgrund von Zahlungsterminen
*  and zahll-flag = '1'
*  and i039-xtfst = space.   "Space - Liquiditätsvorschau
*    call function 'CASH_FORECAST_PAYMENT_RUN_2'
*         exporting
*              i_group     = grupp
*              i_level     = ebene
*              i_date_from = vondt
*              i_date_to   = bisdt
*         importing
*              e_date_from = vondt
*              e_date_to   = bisdt
*         tables
*              s_bukrs     = s_bukrs.
*  endif.

*----------------------------Verprobung Herkunftssymbol---------------*
  PERFORM t039_lesen.                  "Herkunftssymbole
  LOOP AT i039 WHERE orign = i036-orign.
    EXIT.
  ENDLOOP.
  IF sy-subrc <> 0.
    MESSAGE e106(rq) WITH i036-orign.
  ENDIF.

*--------Range aufbauen mit Produktarten, die zu Immobilien gehören
  IF l_st_immo_gsart[] IS INITIAL.
    l_immo_gsart-sign   = 'I'.
    l_immo_gsart-option = 'EQ'.
    SELECT gsart FROM tzpa INTO tzpa-gsart
           WHERE rantyp = '3'    "Produktarten von Immo
           OR    rantyp = '8'
           OR    rantyp = '9'.
      l_immo_gsart-low = tzpa-gsart.
      APPEND l_immo_gsart TO l_st_immo_gsart.
    ENDSELECT.
  ENDIF.

  IF i039-xtfst <> space.              "X - Tagesfinanzstatus
    l_fdgrp = space.   "in GRUPP steht dann die dispositve Kontobez.
    WRITE: bnkko TO wabnkko,
           bukrs TO wabukrs.

*----------------------------Ebene gehört zum Treasury ? ------------*
*   Ebene gehört zum Tagesfinanzstatus -> in T036V wird in den Feldern
*   FDLEVSK und FDLEVSK2 nachgesehen, ob die Ebene für das große
*   Treasury verwendet wird.
    IF l_st_immo_gsart[] IS INITIAL.
      SELECT bukrs FROM t036v INTO t036v-bukrs UP TO 1 ROWS
             WHERE ( fdlevsk = ebene OR fdlevsk2 = ebene ).
      ENDSELECT.
    ELSE.
      SELECT bukrs FROM t036v INTO t036v-bukrs UP TO 1 ROWS
             WHERE ( fdlevsk = ebene OR fdlevsk2 = ebene )
             AND   NOT gsart IN l_st_immo_gsart.   "aber nicht Immo
      ENDSELECT.
    ENDIF.
  ELSE.                                "X - Liquiditätsvorschau
    l_fdgrp = grupp.                   "Dispogruppe aus T035
*   Ebene gehört nicht zum Tagesfinanzstatus -> FDLEVPK und FDLEVPK2
*   in T036V relevant.
    CLEAR: wabukrs.
    IF l_st_immo_gsart[] IS INITIAL.
      SELECT bukrs FROM t036v INTO t036v-bukrs UP TO 1 ROWS
             WHERE ( fdlevpk = ebene OR fdlevpk2 = ebene ).
      ENDSELECT.
    ELSE.
      SELECT bukrs FROM t036v INTO t036v-bukrs UP TO 1 ROWS
             WHERE ( fdlevpk = ebene OR fdlevpk2 = ebene )
             AND   NOT gsart IN l_st_immo_gsart.   "aber nicht Immo
      ENDSELECT.
    ENDIF.
  ENDIF.

  IF sy-subrc = 0.                     "-> Ebene gehört zum Treasury
    CALL FUNCTION 'CASH_FORECAST_TR_SELECT_ITEM'
         EXPORTING
              ebene            = ebene
              grupp            = l_fdgrp
              date_low         = r_datum-low
              date_high        = r_datum-high
              bankk            = wabnkko
              bukrs            = wabukrs
         TABLES
              s_bukrs          = s_bukrs
              s_dispw          = r_dispw
         EXCEPTIONS
              e_no_items_found = 1
              e_no_authority   = 2
              OTHERS           = 3.
    CASE sy-subrc.
      WHEN 1.
        MESSAGE e147(rq) WITH ebene.       "Nothing found
      WHEN 2.
        MESSAGE e188(rq) WITH wabukrs.     "No authority
      WHEN 3.
        MESSAGE e380(rq).                  "No display possible
    ENDCASE.
    CLEAR wabnkko.

    EXIT.
  ENDIF.                               "if sy-subrc = 0

*----------------------------Ebene gehört zum Immobilienmanagement ?
  IF  i039-xtfst = space                "d.h. Liq.vorschau
  AND NOT l_st_immo_gsart[] IS INITIAL.
*   Derzeit schreibt Immo nur auf Debitor oder Kreditor fort.
*   Ebene gehört nicht zum Tagesfinanzstatus -> FDLEVPK und FDLEVPK2
*   in T036V relevant.
    CLEAR: wabukrs.
    SELECT bukrs FROM t036v INTO t036v-bukrs UP TO 1 ROWS
           WHERE ( fdlevpk = ebene OR fdlevpk2 = ebene )
           AND   gsart IN l_st_immo_gsart.   "Immo
    ENDSELECT.

    IF sy-subrc = 0.                   "-> Ebene gehört zu Immobilien
      CALL FUNCTION 'CASH_FORECAST_RE_SELECT_ITEM'
           EXPORTING
                ebene            = ebene
                fdgrp            = l_fdgrp
                date_low         = r_datum-low
                date_high        = r_datum-high
                bukrs            = wabukrs
           TABLES
                s_dispw          = r_dispw
                s_bukrs          = s_bukrs
           EXCEPTIONS
                e_no_items_found = 1
                e_no_authority   = 2
                OTHERS           = 3.

      CASE sy-subrc.
        WHEN 1.
          MESSAGE e217(rq) WITH ebene.     "Nothing found
        WHEN 2.
          MESSAGE e188(rq) WITH wabukrs.   "No authority
        WHEN 3.
          MESSAGE e380(rq).                "No display possible
      ENDCASE.

      EXIT.
    ENDIF.                             "if sy-subrc = 0
  ENDIF.                               "if i039-xtfst = space ...

*----------------------------Ebene gehört zu den Zahlungsanordnungen ?
  SELECT ebene FROM t036r INTO t036r-ebene UP TO 1 ROWS
          WHERE levpr = ebene.
  ENDSELECT.

  IF sy-subrc = 0.   "-> Ebene gehört zu den Zahlungsanordnungen
    CALL FUNCTION 'CASH_FORECAST_TR_SELECT_ITEM'
         EXPORTING
              ebene            = ebene
              grupp            = l_fdgrp
              date_low         = r_datum-low
              date_high        = r_datum-high
              bankk            = wabnkko
              bukrs            = wabukrs
         TABLES
              s_dispw          = r_dispw
              s_bukrs          = s_bukrs
         EXCEPTIONS
              e_no_items_found = 1
              e_no_authority   = 2
              OTHERS           = 3.
    CASE sy-subrc.
      WHEN 1.
        MESSAGE e148(rq) WITH ebene.       "Nothing found
      WHEN 2.
        MESSAGE e188(rq) WITH wabukrs.     "No authority
      WHEN 3.
        MESSAGE e380(rq).                  "No display possible
    ENDCASE.
    CLEAR wabnkko.

    EXIT.
  ENDIF.

*----------------------------Einzelsatz (FF63) ? --------------------*
  PERFORM t037_lesen.                  "Dispositionsarten-Tabelle
  LOOP AT i037 WHERE ebene = ebene.
    EXIT.
  ENDLOOP.
*----------------------------Eintrag gefunden -> Aufruf RFTS6500-----*
*----------------------------(Sachkonto oder Debitor/Kreditor)-------*
  IF sy-subrc = 0.
    PERFORM submit_ff65 USING grupp bukrs ebene.
    EXIT.
  ENDIF.

*----------------------------Ebene gehört zu Agenturgeschäften ?-----*
  PERFORM t036p_t036q_lesen.           "Ebenenzuordnung fuer Logistik
  LOOP AT i036q
       WHERE inteb = '201'             "201-Agenturgeschäft
       AND   ebene = ebene.            "vom User ausgewählte Ebene
    EXIT.
  ENDLOOP.

  IF sy-subrc = 0.   "-> Ebene gehört zu den Agenturgeschäften

    CALL FUNCTION 'CASH_FORECAST_LO_SELECT_ITEM'
         EXPORTING
              ebene            = ebene
              fdgrp            = l_fdgrp
              date_low         = r_datum-low
              date_high        = r_datum-high
              bukrs            = wabukrs
         TABLES
              s_dispw          = r_dispw
              s_bukrs          = s_bukrs
              s_gsber          = s_gsber
         EXCEPTIONS
              e_no_items_found = 1
              e_no_authority   = 2
              OTHERS           = 3.

    CASE sy-subrc.
      WHEN 1.
        MESSAGE e152 WITH ebene.       "Nothing found
      WHEN 2.
        MESSAGE e188 WITH wabukrs.     "No authority
      WHEN 3.
        MESSAGE e380.                  "No display possible
    ENDCASE.

    EXIT.
  ENDIF.

*----------------------------Oben nichts gefunden -> Einzelposten an-
*----------------------------zeigen bei Sachkonten, MM/SD-Belegen; bei
*----------------------------Debitoren/Kreditoren in die FI-Infosysteme
*----------------------------springen--------------------------------*

*----------------------------Tagesfinanzstatus: Sachkonten Einzelposten
  IF i039-xtfst <> space.              "X - Tagesfinanzstatus
*   Meldung falls es Zahlungsaufträge gibt, da diese die gleiche
*   Ebene haben wie die Buchungen auf das Sachkonto und damit keine
*   Möglichkeit besteht, diese separat anzuspringen
    SELECT COUNT(*) FROM pyordh INTO lines
           WHERE waers IN r_dispw
           AND   zbukr =  wabukrs
*          einen Geschäftsbereich gibt es in pyordh nicht
           AND   valut >= r_datum-low
           AND   valut <= r_datum-high
           AND   fdlev =  ebene
           AND   bnkko =  wabnkko.

    IF lines <> 0.
*     Neben den Einzelposten gibt es auch & passende Zahlungsaufträge
      MESSAGE i159 WITH lines.
    ENDIF.

    PERFORM show_gl_account_fi_items
            TABLES r_dispw s_gsber
            USING wabnkko wabukrs ebene r_datum-low r_datum-high.

  ELSE.                                "d.h. Liquiditätsvorschau
*----------------------------Debitor oder Kreditor: prüfen, ob in die
*----------------------------Einzelposten von MM/SD-Einzelposten zu--*
*----------------------------springen ist oder in die FI-Infosysteme-*

*----------------------------MM/SD-Einzelposten?---------------------*
    PERFORM t036p_t036q_lesen.         "Ebenenzuordnung fuer MM/SD
    LOOP AT i036q WHERE ebene = ebene.
      EXIT.
    ENDLOOP.
*----------------------------Eintrag gefunden -> MM/SD-Einzelposten
*----------------------------anzeigen--------------------------------*
    IF sy-subrc = 0.
      CALL FUNCTION 'CASH_FORECAST_LO_SELECT_ITEM'
           EXPORTING
                ebene            = ebene
                fdgrp            = l_fdgrp
                date_low         = r_datum-low
                date_high        = r_datum-high
                bukrs            = wabukrs
           TABLES
                s_dispw          = r_dispw
                s_bukrs          = s_bukrs
                s_gsber          = s_gsber
           EXCEPTIONS
                e_no_items_found = 1
                e_no_authority   = 2
                OTHERS           = 3.

      CASE sy-subrc.
        WHEN 1.
          MESSAGE e235 WITH ebene.     "Nothing found
        WHEN 2.
          MESSAGE e188 WITH wabukrs.   "No authority
        WHEN 3.
          MESSAGE e380.                "No display possible
      ENDCASE.
      EXIT.
    ELSE.
*----------------------------Eintrag nicht gefunden -> Aufruf FIS für
*----------------------------die Debitor- bzw. Kreditorposten--------*
*     PERFORM infosystem_fi USING grupp.
*     PERFORM list3.
      PERFORM check_zdrcm USING s_bukrs-low. "fdtab-bukrs.

*     READ TABLE fdtab WITH KEY diskb = grupp.
      SUBMIT zrfit21 WITH s_bukrs  IN s_bukrs
                     WITH s_gsber  IN s_gsber
                     WITH p_ebene  = ebene
                     WITH p_grupp  = grupp
                     WITH s_datum  IN r_datum
*                    WITH vondt     = r_datum-low
*                    WITH bisdt     = r_datum-high
*                    WITH azart     = azart
*                    WITH p_skalv   = p_skalv
*                    WITH p_skaln   = p_skaln
*                    WITH kwaer     = p_zwaer
*                    WITH awaer     = awaer
                     AND RETURN.
    ENDIF.
  ENDIF.
ENDFORM.                    " drill_down_plan
*&---------------------------------------------------------------------*
*&      Form  CHECK_ZDRCM
*&---------------------------------------------------------------------*
FORM check_zdrcm USING    p_fdtab_bukrs.

  TABLES: ztfi_drcm.

  SELECT * FROM ztfi_drcm WHERE bukrs = p_fdtab_bukrs
                     ORDER BY tstlo DESCENDING .
    IF ztfi_drcm-okcod = space.
      MESSAGE e437(ds) WITH text-010.
    ENDIF.
    EXIT.
  ENDSELECT.


ENDFORM.                    " CHECK_ZDRCM
*---------------------------------------------------------------------*
*       FORM T036_LESEN                                               *
*---------------------------------------------------------------------*
FORM t036_lesen.
  DESCRIBE TABLE i036 LINES lines.
  IF lines = 0.
    SELECT * FROM t036 INTO TABLE i036.
  ENDIF.
ENDFORM.
*---------------------------------------------------------------------*
*       FORM T039_LESEN                                               *
*---------------------------------------------------------------------*
FORM t039_lesen.
  DESCRIBE TABLE i039 LINES lines.
  IF lines = 0.
    SELECT * FROM t039 INTO TABLE i039.
  ENDIF.
ENDFORM.
*---------------------------------------------------------------------*
*       FORM T037_LESEN                                               *
*---------------------------------------------------------------------*
FORM t037_lesen.
  DESCRIBE TABLE i037 LINES lines.
  IF lines = 0.
    SELECT * FROM t037 INTO TABLE i037.
  ENDIF.
ENDFORM.
*--------------------------------------------------------------------*
* FORM SUBMIT_FF65                                                   *
*--------------------------------------------------------------------*
FORM submit_ff65 USING grupp bukrs ebene.
  DATA: l_diskb LIKE t035d-diskb.

  RANGES s_valut FOR fdes-datum.

*--- Selektionszeitraum eingrenzen -----------------------------------
  REFRESH s_valut. CLEAR s_valut.
  MOVE: r_datum-low  TO s_valut-low,
        r_datum-high TO s_valut-high,
        'I'          TO s_valut-sign,
        'BT'         TO s_valut-option.
  APPEND s_valut.

*----------------------------alle, die von RFTS6500 gelesen werden---*
  SET PARAMETER ID 'BUK' FIELD space.
  SET PARAMETER ID 'GSB' FIELD space.
  SET PARAMETER ID 'FFX' FIELD space.
  SET PARAMETER ID 'FFC' FIELD space.
  SET PARAMETER ID 'FFY' FIELD space.
  SET PARAMETER ID 'FFG' FIELD space.
  SET PARAMETER ID 'FFE' FIELD space.
  SET PARAMETER ID 'FFA' FIELD space.
  SET PARAMETER ID 'FDW' FIELD space.
  SET PARAMETER ID 'FDT' FIELD space.
  SET PARAMETER ID 'FFI' FIELD space.
  SET PARAMETER ID 'FFK' FIELD space.

  IF i039-xtfst <> space.              "Tagesfinanzstatus
    l_diskb = grupp.                   "Defaultwert setzen

    SUBMIT rfts6500 WITH s_avdat GE sy-datum
*----------------------------beim Tagesfinanzstatus BK immer eindeutig
                    WITH s_bukrs EQ bukrs
                    WITH s_gsber IN s_gsber
                    WITH p_tfrec EQ 'X'
                    WITH s_ebene EQ ebene
                    WITH s_datum IN s_valut
                    WITH s_diskb EQ l_diskb AND RETURN.
  ELSE.
    SUBMIT rfts6500 WITH s_avdat GE sy-datum
                    WITH s_bukrs IN s_bukrs
                    WITH s_gsber IN s_gsber
                    WITH p_fdrec EQ 'X'
                    WITH s_ebene EQ ebene
                    WITH s_datum IN s_valut
                    WITH s_grupp EQ grupp AND RETURN.
  ENDIF.
ENDFORM.
*---------------------------------------------------------------------*
*       FORM T036P_T036Q_LESEN                                        *
*---------------------------------------------------------------------*
FORM t036p_t036q_lesen.
  DESCRIBE TABLE i036q LINES lines.
  IF lines = 0.
    SELECT * FROM t036q INTO TABLE i036q.
*   i036p wird nicht benötigt
*   select * from t036p into table i036p where spras = sy-langu.
  ENDIF.
ENDFORM.
*----------------------------------------------------------------------*
*  Form SHOW_GL_ACCOUNT_FI_ITEMS
*----------------------------------------------------------------------*
*  Entspricht Aufruf von FBL3N - Anzeige der FI-Sachkonteneinzelposten
*----------------------------------------------------------------------*
FORM show_gl_account_fi_items
     TABLES u_t_dispw
            u_t_gsber
     USING  u_saknr LIKE bseg-hkont
            u_bukrs LIKE bseg-bukrs
            u_fdlev LIKE bseg-fdlev
            u_vondt LIKE bsis-valut
            u_bisdt LIKE bsis-valut.

  TYPE-POOLS: rsds.
  DATA: l_vari_alv    TYPE slis_vari.
  DATA: l_trange TYPE rsds_trange   WITH HEADER LINE,
        l_frange TYPE rsds_frange_t WITH HEADER LINE,
        l_selopt TYPE rsds_selopt_t WITH HEADER LINE,
        l_twhere TYPE rsds_twhere,
        l_texpr  TYPE rsds_texpr.

* Ranges
  RANGES: r_dispw FOR bsis-waers,
          r_gsber FOR bseg-gsber,
          r_saknr FOR bseg-hkont,
          r_bukrs FOR bseg-bukrs.

  r_dispw[] = u_t_dispw[].
  r_gsber[] = u_t_gsber[].

* Report RFITEMGL verwendet Datenbank SDF. In SDF gibt es für die
* Knoten SKA1, SKB1 und BSIS freie Abgrenzungen. Zusätzlich werden vom
* Report auch die angegebenen freien Abgrenzungen auf VBSEGS und VBKPF
* angewendet, d.h. es gilt hier auch für VBSEGS-GSBER, VBSEGS-VALUT
* und VBKPF-WAERS.
* Nicht möglich für BSEG-FDLEV: es gibt zwar bei den freien Abgrenzungen
* die Möglichkeit, auf Ebene einzuschränken, aber das bezieht sich auf
* SKB1-FDLEV und das nützt hier nichts. Da aber alle FI-Buchungen auf
* einem Sachkonto die gleiche Ebene haben (nämlich die Ebene gemäß
* Sachkontostamm), kann man die Abfrage aber auch weglassen.
* Beim Füllen der nachfolgenden Tabellen ist zu beachten, daß jeder
* Knoten (z.B. BSIS) nur einen Eintrag haben darf.

* 1) freie Abgrenzungen für BSIS
* a) Währung (BSIS-WAERS)
  CLEAR: l_selopt.  REFRESH: l_selopt.
  CLEAR: l_frange.  REFRESH: l_frange.
  CLEAR: l_trange.  REFRESH: l_trange.
  LOOP AT r_dispw.
*   nicht l_selopt[] = r_dispw[] wegen Länge von LOW und HIGH
    l_selopt-low    = r_dispw-low.
    l_selopt-high   = r_dispw-high.
    l_selopt-sign   = r_dispw-sign.
    l_selopt-option = r_dispw-option.
    APPEND l_selopt.
  ENDLOOP.

  l_frange-fieldname = 'WAERS'.
  l_frange-selopt_t[] = l_selopt[].
  APPEND l_frange.
  APPEND LINES OF l_frange TO l_trange-frange_t[].

* b) Geschäftsbereich (BSIS-GSBER)
  CLEAR: l_selopt.  REFRESH: l_selopt.
  CLEAR: l_frange.  REFRESH: l_frange.
  LOOP AT r_gsber.
*   nicht l_selopt[] = r_gsber[] wegen Länge von LOW und HIGH
    l_selopt-low    = r_gsber-low.
    l_selopt-high   = r_gsber-high.
    l_selopt-sign   = r_gsber-sign.
    l_selopt-option = r_gsber-option.
    APPEND l_selopt.
  ENDLOOP.

  l_frange-fieldname = 'GSBER'.
  l_frange-selopt_t[] = l_selopt[].
  APPEND l_frange.
  APPEND LINES OF l_frange TO l_trange-frange_t[].

* c) Valutadatum (BSIS-VALUT)
  CLEAR: l_selopt.  REFRESH: l_selopt.
  CLEAR: l_frange.  REFRESH: l_frange.
  l_selopt-low    = u_vondt.
  l_selopt-high   = u_bisdt.
  l_selopt-sign   = 'I'.
  l_selopt-option = 'BT'.
  APPEND l_selopt.

  l_frange-fieldname = 'VALUT'.
  l_frange-selopt_t[] = l_selopt[].
  APPEND l_frange.
  APPEND LINES OF l_frange TO l_trange-frange_t[].

  l_trange-tablename = 'BSIS'.
  APPEND l_trange.

************************************

* (Vorlage: FB FI_PRQ_PAYMENT_RUN_FREE_SEL)
* Freie Abgrenzungen: Konvertierung Format RSDS_TRANGE ==> RSDS_TWHERE
  CALL FUNCTION 'FREE_SELECTIONS_RANGE_2_WHERE'
       EXPORTING
            field_ranges  = l_trange[]
       IMPORTING
            where_clauses = l_twhere[].

* Freie Abgrenzungen: Konvertierung Format RSDS_TRANGE ==> RSDS_TEXPR
  CALL FUNCTION 'FREE_SELECTIONS_RANGE_2_EX'
       EXPORTING
            field_ranges = l_trange[]
       IMPORTING
            expressions  = l_texpr[].

* Ranges erstellen für Submit
  r_saknr-low    = u_saknr.
  r_saknr-sign   = 'I'.
  r_saknr-option = 'EQ'.
  APPEND r_saknr.
  r_bukrs-low    = u_bukrs.
  r_bukrs-sign   = 'I'.
  r_bukrs-option = 'EQ'.
  APPEND r_bukrs.

* ALV Listvariante gemäß Benutzervorgaben, sofern bei den Parametern
* eine Variante gepflegt ist
  GET PARAMETER ID 'FIT_ALV_GL' FIELD l_vari_alv.
* if l_vari_alv is initial.
*   Standardvariante 1SAP nicht mitgeben, da sonst ggf. eine definierte
*   ALV-Einstiegsvariante verloren geht. Gibt es keine, wird vom
*   Report RFITEMGL die 1SAP automatisch gezogen
*   l_vari_alv = '1SAP'.   "Standardvariante
* endif.

  SUBMIT rfitemgl
    WITH FREE SELECTIONS l_texpr
    WITH sd_saknr IN r_saknr
    WITH sd_bukrs IN r_bukrs
    WITH x_opsel  = 'X'
    WITH x_clsel  = space              "ausgeglichene Posten
    WITH x_aisel  = space              "alle Posten
    WITH pa_stida = '99991231'
    WITH x_norm   = 'X'                "normale Posten
    WITH x_merk   = space              "Merkposten
    WITH x_park   = 'X'                "vorerfasste Posten
    WITH pa_vari  = l_vari_alv
    AND RETURN.
ENDFORM.                               "show_gl_account_fi_items
*--------------------------------------------------------------------*
* FORM INFOSYSTEM_FI                                                 *
*--------------------------------------------------------------------*
* (FIS) Aufruf RFDRRANZ (für Debitoren) bzw. RFKRRANZ (für Kred.)    *
* (Beide Reports sind stichtagsbezogen. Stichtag wurde beim letzten  *
*  Lauf, als die Datenbestände aufgebaut wurden, angegeben. Beide    *
*  Reports haben keine Abgrenzungen, sondern nach dem Aufruf kann    *
*  man mit F2 jeweils weiter aufreißen)                              *
*--------------------------------------------------------------------*
FORM infosystem_fi USING grupp.
* ermitteln, ob es die Gruppe zu Debitoren oder Kreditoren gehört
  LOOP AT gkoarttab
       WHERE grupp = grupp.            "ausgewählte Gruppe
    EXIT.
  ENDLOOP.
  IF sy-subrc <> 0.
*   Zuordnung Gruppe zu Kontoart noch nicht ermittelt für diese Gruppe
    SELECT kunnr FROM knb1 INTO knb1-kunnr UP TO 1 ROWS
                      WHERE fdgrv = grupp.
    ENDSELECT.
    IF sy-dbcnt = 0.
*     da kein Debitor mit dieser Gruppe gefunden -> Kreditorgruppe
      gkoarttab-koart = 'K'.
    ELSE.
      gkoarttab-koart = 'D'.
    ENDIF.
    gkoarttab-grupp = grupp.
    APPEND gkoarttab.
  ENDIF.

  IF gkoarttab-koart = 'D'.
    SUBMIT rfdrranz AND RETURN.
  ELSE.                                "kann nur 'K' sein
    SUBMIT rfkrranz AND RETURN.
  ENDIF.
ENDFORM.                               "infosystem_fi

*--------------------------------------------------------------------*
* FORM ermittlung_von_bis_datum                                      *
*--------------------------------------------------------------------*
FORM ermittlung_von_bis_datum.
*  CLEAR: bisd1,
*         biswo,
*         vondt,
*         bisdt.
*
*  GET CURSOR FIELD cucol.
** gilt bei Anzeigeart K immer,bei D nur für erste Spalte,bei A gar
*nicht
*  vondt = mindat.
*
*  IF     cucol = 'AZTAB-DMSH1'         "Vaterbild
*  OR     cucol = 'GETAB-DMSH1'         "Sohnbild
*  OR     cucol = 'WRTAB-DMSH1'.        "Enkelbild
*    IF azart+0(1) = 'A'.
*      vondt = p_perdt.
*    ENDIF.
*    bisdt = zb-d1.
*  ELSEIF cucol = 'AZTAB-DMSH2'
*  OR     cucol = 'GETAB-DMSH2'
*  OR     cucol = 'WRTAB-DMSH2'.
*    IF azart+0(1) = 'D' OR azart+0(1) = 'A'.
**----------------------------Uebergabeparameter: vorherige Spalte----*
*      vondt = zb-d1 + 1.
*    ENDIF.
*    bisdt = zb-d2.
*  ELSEIF cucol = 'AZTAB-DMSH3'
*  OR     cucol = 'GETAB-DMSH3'
*  OR     cucol = 'WRTAB-DMSH3'.
*    IF azart+0(1) = 'D' OR azart+0(1) = 'A'.
*      vondt = zb-d2 + 1.
*    ENDIF.
*    bisdt = zb-d3.
*  ELSEIF cucol = 'AZTAB-DMSH4'
*  OR     cucol = 'GETAB-DMSH4'
*  OR     cucol = 'WRTAB-DMSH4'.
*    IF azart+0(1) = 'D' OR azart+0(1) = 'A'.
*      vondt = zb-d3 + 1.
*    ENDIF.
*    bisdt = zb-d4.
*  ELSEIF cucol = 'AZTAB-DMSH5'
*  OR     cucol = 'GETAB-DMSH5'
*  OR     cucol = 'WRTAB-DMSH5'.
*    IF azart+0(1) = 'D' OR azart+0(1) = 'A'.
*      vondt = zb-d4 + 1.
*    ENDIF.
*    bisdt = zb-d5.
*  ELSEIF cucol = 'AZTAB-DMSH6'
*  OR     cucol = 'GETAB-DMSH6'
*  OR     cucol = 'WRTAB-DMSH6'.
*    IF azart+0(1) = 'D' OR azart+0(1) = 'A'.
*      vondt = zb-d5 + 1.
*    ENDIF.
*    bisdt = zb-d6.
*  ELSEIF cucol = 'AZTAB-DMSH7'
*  OR     cucol = 'GETAB-DMSH7'
*  OR     cucol = 'WRTAB-DMSH7'.
*    IF azart+0(1) = 'D' OR azart+0(1) = 'A'.
*      vondt = zb-d6 + 1.
*    ENDIF.
*    bisdt = zb-d7.
*  ELSEIF cucol = 'AZTAB-DMSH8'
*  OR     cucol = 'GETAB-DMSH8'
*  OR     cucol = 'WRTAB-DMSH8'.
*    IF azart+0(1) = 'D' OR azart+0(1) = 'A'.
*      vondt = zb-d7 + 1.
*    ENDIF.
*    bisdt = zb-d8.
*  ELSEIF cucol = 'AZTAB-DMSH9'
*  OR     cucol = 'GETAB-DMSH9'
*  OR     cucol = 'WRTAB-DMSH9'.
*    IF azart+0(1) = 'D' OR azart+0(1) = 'A'.
*      vondt = zb-d8 + 1.
*    ENDIF.
*    bisdt = zb-d9.
*  ELSEIF cucol = 'AZTAB-DMSH10'
*  OR     cucol = 'GETAB-DMSH10'
*  OR     cucol = 'WRTAB-DMSH10'.
*    IF azart+0(1) = 'D' OR azart+0(1) = 'A'.
*      vondt = zb-d9 + 1.
*    ENDIF.
*    bisdt = zb-d10.
*  ELSEIF cucol = 'AZTAB-DMSH11'
*  OR     cucol = 'GETAB-DMSH11'
*  OR     cucol = 'WRTAB-DMSH11'.
*    IF azart+0(1) = 'D' OR azart+0(1) = 'A'.
*      vondt = zb-d10 + 1.
*    ENDIF.
*    bisdt = zb-d11.
*  ELSEIF cucol = 'AZTAB-DMSH12'
*  OR     cucol = 'GETAB-DMSH12'
*  OR     cucol = 'WRTAB-DMSH12'.
*    IF azart+0(1) = 'D' OR azart+0(1) = 'A'.
*      vondt = zb-d11 + 1.
*    ENDIF.
*    bisdt = zb-d12.
*  ELSEIF cucol = 'AZTAB-DMSH13'
*  OR     cucol = 'GETAB-DMSH13'
*  OR     cucol = 'WRTAB-DMSH13'.
*    IF azart+0(1) = 'D' OR azart+0(1) = 'A'.
*      vondt = zb-d12 + 1.
*    ENDIF.
*    bisdt = zb-d13.
*  ELSEIF cucol = 'AZTAB-DMSH14'
*  OR     cucol = 'GETAB-DMSH14'
*  OR     cucol = 'WRTAB-DMSH14'.
*    IF azart+0(1) = 'D' OR azart+0(1) = 'A'.
*      vondt = zb-d13 + 1.
*    ENDIF.
*    bisdt = zb-d14.
*  ELSEIF cucol = 'AZTAB-DMSH15'
*  OR     cucol = 'GETAB-DMSH15'
*  OR     cucol = 'WRTAB-DMSH15'.
*    IF azart+0(1) = 'D' OR azart+0(1) = 'A'.
*      vondt = zb-d14 + 1.
*    ENDIF.
*    bisdt = zb-d15.
*  ELSEIF cucol = 'AZTAB-DMSH16'
*  OR     cucol = 'GETAB-DMSH16'
*  OR     cucol = 'WRTAB-DMSH16'.
*    IF azart+0(1) = 'D' OR azart+0(1) = 'A'.
*      vondt = zb-d15 + 1.
*    ENDIF.
*    bisdt = zb-d16.
*  ELSEIF cucol = 'AZTAB-DMSH17'
*  OR     cucol = 'GETAB-DMSH17'
*  OR     cucol = 'WRTAB-DMSH17'.
*    IF azart+0(1) = 'D' OR azart+0(1) = 'A'.
*      vondt = zb-d16 + 1.
*    ENDIF.
*    bisdt = zb-d17.
*  ELSEIF cucol = 'AZTAB-DMSH18'
*  OR     cucol = 'GETAB-DMSH18'
*  OR     cucol = 'WRTAB-DMSH18'.
*    IF azart+0(1) = 'D' OR azart+0(1) = 'A'.
*      vondt = zb-d17 + 1.
*    ENDIF.
*    bisdt = zb-d18.
*  ELSEIF cucol = 'AZTAB-DMSH19'
*  OR     cucol = 'GETAB-DMSH19'
*  OR     cucol = 'WRTAB-DMSH19'.
*    IF azart+0(1) = 'D' OR azart+0(1) = 'A'.
*      vondt = zb-d18 + 1.
*    ENDIF.
*    bisdt = zb-d19.
*  ELSEIF cucol = 'AZTAB-DMSH20'
*  OR     cucol = 'GETAB-DMSH20'
*  OR     cucol = 'WRTAB-DMSH20'.
*    IF azart+0(1) = 'D' OR azart+0(1) = 'A'.
*      vondt = zb-d19 + 1.
*    ENDIF.
*    bisdt = zb-d20.
*  ELSEIF cucol = 'AZTAB-DMSH21'
*  OR     cucol = 'GETAB-DMSH21'
*  OR     cucol = 'WRTAB-DMSH21'.
*    IF azart+0(1) = 'D' OR azart+0(1) = 'A'.
*      vondt = zb-d20 + 1.
*    ENDIF.
*    bisdt = zb-d21.
*  ELSEIF cucol = 'AZTAB-DMSH22'
*  OR     cucol = 'GETAB-DMSH22'
*  OR     cucol = 'WRTAB-DMSH22'.
*    IF azart+0(1) = 'D' OR azart+0(1) = 'A'.
*      vondt = zb-d21 + 1.
*    ENDIF.
*    bisdt = zb-d22.
*  ELSEIF cucol = 'AZTAB-DMSH23'
*  OR     cucol = 'GETAB-DMSH23'
*  OR     cucol = 'WRTAB-DMSH23'.
*    IF azart+0(1) = 'D' OR azart+0(1) = 'A'.
*      vondt = zb-d22 + 1.
*    ENDIF.
*    bisdt = maxdat.
*  ELSE.
*    MESSAGE e031.  "bitte Cursor auf gültiges Betragsfeld positionieren
*  ENDIF.
ENDFORM.                               "ermittlung_von_bis_datum
*&---------------------------------------------------------------------*
*&      Form  get_period
*&---------------------------------------------------------------------*
FORM get_period USING p_in_mon.
*--> monthly
  IF p_mhly EQ 'X'.
    r_datum-sign = 'I'.   r_datum-option = 'BT'.
    CONCATENATE p_gjahr p_in_mon '01' INTO r_datum-low.
    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
         EXPORTING
              day_in            = r_datum-low
         IMPORTING
              last_day_of_month = r_datum-high.
    APPEND r_datum.
*--> weekly
  ELSE.
    READ TABLE s_week INDEX p_in_mon.  "==> search week

    r_datum-sign = 'I'.   r_datum-option = 'BT'.
    CONCATENATE p_gjahr p_month '01' INTO r_datum-low.
  ENDIF.

  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
       EXPORTING
            day_in            = r_datum-low
       IMPORTING
            last_day_of_month = r_datum-high.
  APPEND r_datum.
ENDFORM.                    " get_period
