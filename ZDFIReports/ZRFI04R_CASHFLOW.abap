REPORT zrfi04r_cashflow MESSAGE-ID zmfi
                        LINE-COUNT 90 LINE-SIZE 130
                        NO STANDARD PAGE HEADING.
*&---------------------------------------------------------------------*
*   PROGRAM-ID   : ZRFI04R_CASHFLOW                                    *
*   MODULE       : FI                                                  *
*   CREATED BY   : HO JOONG. HWANG                                     *
*   CREATED ON   : 2003.09.19                                          *
*   DESCRIPTION  :                                                     *
*
*&---------------------------------------------------------------------*
INCLUDE: <icon>,
         <symbol>.

TABLES: glfunct,
        mcs0,
        t001,
        ztfi_cashflow.
FIELD-SYMBOLS: <fs1> LIKE glfunct-hsl01,
               <fs2> LIKE glfunct-hsl01.
*..... internal tables
DATA: it_flow LIKE ztfi_cashflow OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF it_1std OCCURS 0,
      zcfid    LIKE ztfi_cashflow-zcfid,   " CF ID
      ztext    LIKE ztfi_cashflow-ztext,   " DESCRIPTION
      zseti    LIKE ztfi_cashflow-zseti,   " SET ID
      fracc    LIKE glfunct-racct,         " ACCOUNT FROM
      tracc    LIKE glfunct-racct,         " ACCOUNT TO
      basea    LIKE glfunct-hsl01,         " BASE AMOUNT
      compa    LIKE glfunct-hsl01,         " COMP AMOUNT
      diffa    LIKE glfunct-hsl01,         " DIFF AMOUNT
      rtcur    LIKE glfunct-rtcur,         " CURRENCY
      icons(2).                            " ICON STATUS
DATA: END OF it_1std.

DATA: it_2ndd LIKE it_1std OCCURS 0 WITH HEADER LINE,
      it_3rdd LIKE it_1std OCCURS 0 WITH HEADER LINE,
*     IT_4THD LIKE IT_1STD OCCURS 0 WITH HEADER LINE,
      it_temp LIKE it_1std OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF it_4thd OCCURS 0,
      zcfid    LIKE ztfi_cashflow-zcfid,
      rtcur    LIKE glfunct-rtcur,
      racct    LIKE glfunct-racct,
      basea    LIKE glfunct-hsl01,
      compa    LIKE glfunct-hsl01,
      diffa    LIKE glfunct-hsl01,
      hsl01    LIKE glfunct-hsl01,
      hsl02    LIKE glfunct-hsl02.
DATA: END OF it_4thd.

DATA: BEGIN OF it_calc OCCURS 0,
      ryear    LIKE glfunct-ryear,
*Issue # 20041111-012 Requested by Paul, Changed by wskim, on 20041116
*Error
*-----Start
*      rtcur    LIKE glfunct-rtcur,
*-----End
      drcrk    LIKE glfunct-drcrk,
      racct    LIKE glfunct-racct,
      hslvt    LIKE glfunct-hslvt,
      hsl01    LIKE glfunct-hsl01,
      hsl02    LIKE glfunct-hsl02,
      hsl03    LIKE glfunct-hsl03,
      hsl04    LIKE glfunct-hsl04,
      hsl05    LIKE glfunct-hsl05,
      hsl06    LIKE glfunct-hsl06,
      hsl07    LIKE glfunct-hsl07,
      hsl08    LIKE glfunct-hsl08,
      hsl09    LIKE glfunct-hsl09,
      hsl10    LIKE glfunct-hsl10,
      hsl11    LIKE glfunct-hsl11,
      hsl12    LIKE glfunct-hsl12.
DATA: END OF it_calc.

DATA: BEGIN OF it_acct OCCURS 0,
      zcfid    LIKE ztfi_cashflow-zcfid,
      zseti    LIKE ztfi_cashflow-zseti,
      fracc    LIKE glfunct-racct,
      tracc    LIKE glfunct-racct.
DATA: END OF it_acct.

DATA: BEGIN OF it_splt OCCURS 0,
      zword(2).
DATA: END OF it_splt.

DATA: l_valu_tab LIKE grpvalues OCCURS 0 WITH HEADER LINE.

RANGES: r_byear FOR glfunct-ryear,
        r_cyear FOR glfunct-ryear.
*..... variants
DATA: w_width      TYPE i,
      w_fname(30),
      w_fvalu(30),
      w_fldn1(20),
      w_fldn2(20),
      w_lastf,
      w_count(2)   TYPE n,
      w_numbe(2)   TYPE n,
      w_zsign,
      w_zcfid      LIKE ztfi_cashflow-zcfid.

DATA: w_fracc      LIKE glfunct-racct,
      w_tracc      LIKE glfunct-racct,
      w_pagno      LIKE sy-pagno,
      w_linno      LIKE sy-linno.

DATA: w_basea      LIKE glfunct-hsl01,
      w_compa      LIKE glfunct-hsl01.
*Issue # 20041111-012 Requested by Paul, Changed by wskim, on 20041116
*----Start
RANGES :  s_bdate FOR mcs0-spmon,
          s_cdate FOR mcs0-spmon.
DATA : p_rldnr LIKE glfunct-rldnr value '0F',
       p_rrcty LIKE glfunct-rrcty value '0'.
*----End
************************************************************************
*                        SELECT OPTION                                 *
************************************************************************
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
*Issue # 20041111-012 Requested by Paul, Changed by wskim, on 20041116
*Error
*---Start
PARAMETERS:
            p_bukrs LIKE glfunct-rbukrs,
*SELECT-OPTIONS: s_bdate FOR mcs0-spmon,
*                s_cdate FOR mcs0-spmon.
            p_bdate LIKE mcs0-spmon,
            p_cdate LIKE mcs0-spmon.
*---End

SELECTION-SCREEN: END OF BLOCK b1.
*
SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(15) text-005.
SELECTION-SCREEN POSITION 33.
PARAMETERS: p_radi1 RADIOBUTTON GROUP rad1.
SELECTION-SCREEN COMMENT 35(8) text-006 FOR FIELD p_radi1.
SELECTION-SCREEN POSITION 58.
PARAMETERS: p_radi2 RADIOBUTTON GROUP rad1.
SELECTION-SCREEN COMMENT 60(6) text-007 FOR FIELD p_radi2.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK b2.
*
INITIALIZATION.
  PERFORM init_select_options.
*
AT SELECTION-SCREEN.
  PERFORM clear_inter_table.
*
TOP-OF-PAGE.
  w_width = 130.
  PERFORM head.
*
TOP-OF-PAGE DURING LINE-SELECTION.
  w_width = 130.
  PERFORM head.
************************************************************************
*                         MAIN PROGRAM                                 *
************************************************************************
START-OF-SELECTION.
* 1. select maint view
  PERFORM get_node.
* 2. separate node
  PERFORM get_depth USING '0'.
  PERFORM get_depth USING '1'.
  PERFORM get_depth USING '2'.
* 3. get amount
  PERFORM get_base_comp_amount.
* 4. modify current icon status
  PERFORM modify_icons.
* 5. write list
  PERFORM write_body.

END-OF-SELECTION.
************************************************************************
*                             EVENTS                                   *
************************************************************************
AT LINE-SELECTION.
  sy-lsind = 0.
  GET CURSOR FIELD w_fname VALUE w_fvalu.
  w_zcfid = sy-lisel+1(4).
  w_pagno = sy-pagno.
  w_linno = sy-linno.
  PERFORM modify_inter_table_1.
  PERFORM modify_inter_table_2.
  PERFORM modify_inter_table_3.
  PERFORM write_body.
*&---------------------------------------------------------------------*
*&      Form  INIT_SELECT_OPTIONS
*&---------------------------------------------------------------------*
FORM init_select_options.
  p_bukrs = 'H201'.
*Issue # 20041111-012 Requested by Paul, Changed by wskim, on 20041116
*Error
*---End
*  s_bdate-low = s_bdate-high = sy-datum+(6).
*  s_bdate-low+4(2) = '01'.
*  s_bdate-sign = 'I'. s_bdate-option = 'BT'.
*  APPEND s_bdate.
**
*  s_cdate-low = s_bdate-low.
*  s_cdate-low+(4) = s_bdate-low+(4) - 1.
*  s_cdate-high = s_bdate-high.
*  s_cdate-high+(4) = s_bdate-high+(4) - 1.
*  s_cdate-sign = 'I'. s_cdate-option = 'BT'.
*  APPEND s_cdate.

  p_bdate = sy-datum+(6).
  p_cdate = sy-datum+(6).


*---End

ENDFORM.                    " INIT_SELECT_OPTIONS
*&---------------------------------------------------------------------*
*&      Form  CLEAR_INTER_TABLE
*&---------------------------------------------------------------------*
FORM clear_inter_table.
  CLEAR: it_flow, it_1std, it_2ndd, it_3rdd, it_4thd, it_acct.
  REFRESH: it_flow, it_1std, it_2ndd, it_3rdd, it_4thd, it_acct.
*
  CLEAR: r_byear, r_cyear.

*Issue # 20041111-012 Requested by Paul, Changed by wskim, on 20041116
*Error
*---Start
  REFRESH: r_byear, r_cyear,s_bdate,s_cdate.
  s_bdate-low(4)   = p_bdate(4).
  s_bdate-low+4(2) = '01'.
  s_bdate-high = p_bdate.
  s_bdate-sign = 'I'. s_bdate-option = 'BT'.
  APPEND s_bdate.
*
  s_cdate-low(4)  = p_cdate(4).
  s_cdate-low+4(2) = '01'.
  s_cdate-high = p_cdate.
  s_cdate-sign = 'I'. s_cdate-option = 'BT'.
  APPEND s_cdate.
*
*-----End
  r_byear-sign = 'I'.
  IF s_bdate-low+(4) <> s_bdate-high+(4).
    r_byear-option = 'BT'.
    r_byear-low = s_bdate-low+(4).
    r_byear-high = s_bdate-high+(4).
  ELSE.
    r_byear-option = 'EQ'.
    r_byear-low = s_bdate-low+(4).
  ENDIF.
  APPEND r_byear.
*
  r_cyear-sign = 'I'.
  IF s_cdate-low+(4) <> s_cdate-high+(4).
    r_cyear-option = 'BT'.
    r_cyear-low = s_cdate-low+(4).
    r_cyear-high = s_cdate-high+(4).
  ELSE.
    r_cyear-option = 'EQ'.
    r_cyear-low = s_cdate-low+(4).
  ENDIF.
  APPEND r_cyear.
ENDFORM.                    " CLEAR_INTER_TABLE
*&---------------------------------------------------------------------*
*&      Form  GET_NODE
*&---------------------------------------------------------------------*
FORM get_node.
  IF p_radi1 = 'X'.                                " indirect
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_flow
      FROM ztfi_cashflow WHERE zcfid NOT LIKE 'E%'.
  ENDIF.
*
  IF p_radi2 = 'X'.                                " direct
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_flow
      FROM ztfi_cashflow WHERE zcfid NOT LIKE 'A%'
                           AND zcfid NOT LIKE 'B%'
                           AND zcfid NOT LIKE 'C%'
                           AND zcfid NOT LIKE 'D%'.
  ENDIF.
* get account range
  LOOP AT it_flow WHERE zseti NE 'D'.
    it_acct-zcfid = it_flow-zcfid.
    it_acct-zseti = it_flow-zseti.
    PERFORM get_account_range USING w_fracc w_tracc.
    LOOP AT l_valu_tab.
      it_acct-fracc = l_valu_tab-vfrom.
      it_acct-tracc = l_valu_tab-vto.
      APPEND it_acct.
    ENDLOOP.
    CLEAR it_acct.
  ENDLOOP.
  SORT it_acct BY zcfid fracc.
ENDFORM.                    " GET_NODE
*&---------------------------------------------------------------------*
*&      Form  GET_DEPTH
*&---------------------------------------------------------------------*
FORM get_depth USING p_value.
  LOOP AT it_flow WHERE zindt = p_value.
    CLEAR: w_fracc, w_tracc.
    CASE p_value.
      WHEN '0'.
        it_1std-zcfid = it_flow-zcfid.
        it_1std-ztext = it_flow-ztext.
        it_1std-zseti = it_flow-zseti.
        IF it_flow-zseti NE 'D'.
          PERFORM get_account_range USING w_fracc w_tracc.
          it_1std-fracc = w_fracc.
          it_1std-tracc = w_tracc.
        ENDIF.
        it_1std-icons = '4'.
        APPEND it_1std. CLEAR it_1std.
      WHEN '1'.
        it_2ndd-zcfid = it_flow-zcfid.
        it_2ndd-ztext = it_flow-ztext.
        it_2ndd-zseti = it_flow-zseti.
        IF it_flow-zseti NE 'D'.
          PERFORM get_account_range USING w_fracc w_tracc.
          it_2ndd-fracc = w_fracc.
          it_2ndd-tracc = w_tracc.
        ENDIF.
        it_2ndd-icons = '4'.
        APPEND it_2ndd. CLEAR it_2ndd.
      WHEN '2'.
        it_3rdd-zcfid = it_flow-zcfid.
        it_3rdd-ztext = it_flow-ztext.
        it_3rdd-zseti = it_flow-zseti.
        IF it_flow-zseti NE 'D'.
          PERFORM get_account_range USING w_fracc w_tracc.
          it_3rdd-fracc = w_fracc.
          it_3rdd-tracc = w_tracc.
        ENDIF.
        it_3rdd-icons = '4'.
        APPEND it_3rdd. CLEAR it_3rdd.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    " GET_DEPTH
*&---------------------------------------------------------------------*
*&      Form  HEAD
*&---------------------------------------------------------------------*
FORM head.
  WRITE: AT /(w_width) 'CASH FLOW' CENTERED NO-GAP.
  WRITE: AT /(w_width) '---------' CENTERED NO-GAP.
*
  WRITE: /2(14) 'Company code :', p_bukrs COLOR 6 INTENSIFIED.
  CLEAR t001.
  SELECT SINGLE butxt INTO t001-butxt
    FROM t001 WHERE bukrs = p_bukrs.
  WRITE: (25) t001-butxt COLOR 1 INVERSE.

  WRITE: 71(17) 'Baseline period :',
                 s_bdate-low COLOR 1 INVERSE,
                 '~' COLOR 1 INVERSE,
                 s_bdate-high COLOR 1 INVERSE.
  WRITE: /2(14) 'User id      :', sy-uname COLOR 1 INVERSE.
  WRITE: 71(17) 'Compare period  :',
                 s_cdate-low COLOR 1 INVERSE,
                 '~' COLOR 1 INVERSE,
                 s_cdate-high COLOR 1 INVERSE.
*
  FORMAT COLOR COL_KEY INTENSIFIED OFF.
  ULINE AT (w_width).
  WRITE: / sy-vline NO-GAP.
  WRITE: (4) 'CFID' NO-GAP, sy-vline NO-GAP.
  WRITE: (63) 'Description' CENTERED NO-GAP, sy-vline NO-GAP.
  WRITE: (19) 'Baseline Amount' NO-GAP, sy-vline NO-GAP.
  WRITE: (19) 'Compared Amount' NO-GAP, sy-vline NO-GAP.
  WRITE: (19) 'Diff. Amount' NO-GAP, AT w_width sy-vline.
  ULINE AT (w_width).
ENDFORM.                    " HEAD
*&---------------------------------------------------------------------*
*&      Form  WRITE_BODY
*&---------------------------------------------------------------------*
FORM write_body.
  SORT: it_1std BY zcfid,
        it_2ndd BY zcfid,
        it_3rdd BY zcfid.
*
  LOOP AT it_1std.
    FORMAT COLOR COL_NORMAL INTENSIFIED.
    CLEAR w_lastf.
    AT LAST.
      w_lastf = 'X'.
    ENDAT.
    WRITE: / sy-vline NO-GAP.
    WRITE: (4) it_1std-zcfid LEFT-JUSTIFIED NO-GAP, sy-vline NO-GAP.
    READ TABLE it_2ndd WITH KEY zcfid+(1) = it_1std-zcfid+(1).
    CASE it_1std-icons.
      WHEN '00'. WRITE: (3) ' ' NO-GAP.
      WHEN '4'. WRITE: (3) sym_plus_folder AS SYMBOL NO-GAP HOTSPOT.
      WHEN '5'. WRITE: (3) sym_minus_folder AS SYMBOL NO-GAP HOTSPOT.
    ENDCASE.
    WRITE: (60) it_1std-ztext LEFT-JUSTIFIED NO-GAP, sy-vline NO-GAP.
    WRITE: (19) it_1std-basea CURRENCY it_1std-rtcur NO-GAP,
                sy-vline NO-GAP.
    WRITE: (19) it_1std-compa CURRENCY it_1std-rtcur NO-GAP,
                sy-vline NO-GAP.
    WRITE: (19) it_1std-diffa CURRENCY it_1std-rtcur NO-GAP ,
                sy-vline NO-GAP.
    IF it_1std-icons = '5'.
      PERFORM write_2nd_body.
    ENDIF.
    IF it_1std-zcfid = 'I'.
      w_basea = it_1std-basea.
    ENDIF.
    IF it_1std-zcfid = 'J'.
      w_compa = it_1std-basea.
    ENDIF.
  ENDLOOP.
  ULINE AT (w_width).
*
  IF w_basea <> w_compa.
    MESSAGE s000 WITH 'W: Abnormal Cash Flow Statement'.
  ENDIF.
ENDFORM.                    " WRITE_BODY
*&---------------------------------------------------------------------*
*&      Form  MODIFY_INTER_TABLE_1
*&---------------------------------------------------------------------*
FORM modify_inter_table_1.
  READ TABLE it_1std WITH KEY zcfid = w_zcfid.
  IF sy-subrc = 0.
    CASE w_fvalu.
      WHEN '4'.
        it_1std-icons = '5'.
      WHEN '5'.
        it_1std-icons = '4'.
    ENDCASE.
    MODIFY it_1std TRANSPORTING icons WHERE zcfid = w_zcfid.
  ENDIF.
ENDFORM.                    " MODIFY_INTER_TABLE_1
*&---------------------------------------------------------------------*
*&      Form  MODIFY_INTER_TABLE_2
*&---------------------------------------------------------------------*
FORM modify_inter_table_2.
  READ TABLE it_2ndd WITH KEY zcfid = w_zcfid.
  IF sy-subrc = 0.
    CASE w_fvalu.
      WHEN '4'.
        it_2ndd-icons = '5'.
      WHEN '5'.
        it_2ndd-icons = '4'.
    ENDCASE.
    MODIFY it_2ndd TRANSPORTING icons WHERE zcfid = w_zcfid.
  ENDIF.
ENDFORM.                    " MODIFY_INTER_TABLE_2
*&---------------------------------------------------------------------*
*&      Form  MODIFY_INTER_TABLE_3
*&---------------------------------------------------------------------*
FORM modify_inter_table_3.
  READ TABLE it_3rdd WITH KEY zcfid = w_zcfid.
  IF sy-subrc = 0.
    CASE w_fvalu.
      WHEN '4'.
        it_3rdd-icons = '5'.
      WHEN '5'.
        it_3rdd-icons = '4'.
    ENDCASE.
    MODIFY it_3rdd TRANSPORTING icons WHERE zcfid = w_zcfid.
  ENDIF.
ENDFORM.                    " MODIFY_INTER_TABLE_3
*&---------------------------------------------------------------------*
*&      Form  WRITE_2ND_BODY
*&---------------------------------------------------------------------*
FORM write_2nd_body.
  CLEAR it_temp. REFRESH it_temp.
  LOOP AT it_2ndd WHERE zcfid+(1) = it_1std-zcfid+(1).
    MOVE-CORRESPONDING it_2ndd TO it_temp.
    APPEND it_temp. CLEAR it_temp.
  ENDLOOP.
*
  ULINE AT (w_width).
  LOOP AT it_2ndd WHERE zcfid+(1) = it_1std-zcfid+(1).
    FORMAT COLOR COL_GROUP INTENSIFIED.
    WRITE: / sy-vline NO-GAP.
    WRITE: (4) it_2ndd-zcfid NO-GAP, sy-vline NO-GAP.
    WRITE: (5) ' ' NO-GAP.
    CASE it_2ndd-icons.
      WHEN '00'. WRITE: (3) ' ' NO-GAP.
      WHEN '4'. WRITE: (3) sym_plus_folder AS SYMBOL NO-GAP HOTSPOT.
      WHEN '5'. WRITE: (3) sym_minus_folder AS SYMBOL NO-GAP HOTSPOT.
    ENDCASE.
    WRITE: (55) it_2ndd-ztext NO-GAP, sy-vline NO-GAP.
    WRITE: (19) it_2ndd-basea CURRENCY it_2ndd-rtcur NO-GAP,
                sy-vline NO-GAP.
    WRITE: (19) it_2ndd-compa CURRENCY it_2ndd-rtcur NO-GAP,
                sy-vline NO-GAP.
    WRITE: (19) it_2ndd-diffa CURRENCY it_2ndd-rtcur NO-GAP,
                sy-vline NO-GAP.
    IF it_2ndd-icons = '5'.
      PERFORM write_3rd_body.
    ENDIF.
  ENDLOOP.
*
  IF sy-subrc = 0.
    IF w_lastf = ' '.
      ULINE AT (w_width).
    ENDIF.
  ELSE.
    PERFORM write_4th_body USING it_1std-zcfid.
  ENDIF.
ENDFORM.                    " WRITE_2ND_BODY
*&---------------------------------------------------------------------*
*&      Form  WRITE_3RD_BODY
*&---------------------------------------------------------------------*
FORM write_3rd_body.
  SORT it_temp BY zcfid DESCENDING.
  READ TABLE it_temp INDEX 1.
*
  ULINE AT (w_width).
  LOOP AT it_3rdd WHERE zcfid+(2) = it_2ndd-zcfid+(2).
    FORMAT COLOR COL_GROUP INTENSIFIED OFF.
    WRITE: / sy-vline NO-GAP.
    WRITE: (4) it_3rdd-zcfid NO-GAP, sy-vline NO-GAP.
    WRITE: (10) ' ' NO-GAP.
    CASE it_3rdd-icons.
      WHEN '00'. WRITE: (3) ' ' NO-GAP.
      WHEN '4'. WRITE: (3) sym_plus_folder AS SYMBOL NO-GAP HOTSPOT.
      WHEN '5'. WRITE: (3) sym_minus_folder AS SYMBOL NO-GAP HOTSPOT.
    ENDCASE.
    WRITE: (50) it_3rdd-ztext NO-GAP, sy-vline NO-GAP.
    WRITE: (19) it_3rdd-basea CURRENCY it_3rdd-rtcur NO-GAP,
                sy-vline NO-GAP.
    WRITE: (19) it_3rdd-compa CURRENCY it_3rdd-rtcur NO-GAP,
                sy-vline NO-GAP.
    WRITE: (19) it_3rdd-diffa CURRENCY it_3rdd-rtcur NO-GAP,
                sy-vline NO-GAP.
    IF it_3rdd-icons = '5'.
      PERFORM write_4th_body USING it_3rdd-zcfid.
    ENDIF.
  ENDLOOP.
*
  IF sy-subrc = 0.
    IF it_3rdd-zcfid+(2) <> it_temp-zcfid+(2).
      ULINE AT (w_width).
    ENDIF.
  ELSE.
    PERFORM write_4th_body USING it_2ndd-zcfid.
  ENDIF.
ENDFORM.                    " WRITE_3RD_BODY
*&---------------------------------------------------------------------*
*&      Form  MODIFY_ICONS
*&---------------------------------------------------------------------*
FORM modify_icons.
  LOOP AT it_1std.
    CLEAR it_2ndd.
    READ TABLE it_2ndd WITH KEY zcfid+(1) = it_1std-zcfid+(1).
    IF sy-subrc <> 0.
      READ TABLE it_4thd WITH KEY zcfid = it_1std-zcfid.
      IF sy-subrc <> 0.
        it_1std-icons = '00'.
        MODIFY it_1std. CLEAR it_1std.
      ENDIF.
    ENDIF.
  ENDLOOP.
*
  LOOP AT it_2ndd.
    CLEAR it_3rdd.
    READ TABLE it_3rdd WITH KEY zcfid+(2) = it_2ndd-zcfid+(2).
    IF sy-subrc <> 0.
      READ TABLE it_4thd WITH KEY zcfid = it_2ndd-zcfid.
      IF sy-subrc <> 0.
        it_2ndd-icons = '00'.
        MODIFY it_2ndd. CLEAR it_2ndd.
      ENDIF.
    ENDIF.
  ENDLOOP.
*
  LOOP AT it_3rdd.
    CLEAR it_4thd.
    READ TABLE it_4thd WITH KEY zcfid+(3) = it_3rdd-zcfid+(3).
    IF sy-subrc <> 0.
      it_3rdd-icons = '00'.
      MODIFY it_3rdd. CLEAR it_3rdd.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " MODIFY_ICONS
*&---------------------------------------------------------------------*
*&      Form  GET_ACCOUNT_RANGE
*&---------------------------------------------------------------------*
FORM get_account_range USING p_fracc p_tracc.
  DATA: l_setid LIKE sethier-setid,
        l_cinfo LIKE grphinfo.
  DATA: l_node_tab LIKE grpobjects OCCURS 0 WITH HEADER LINE.
  CONCATENATE '0000' it_flow-zseti INTO l_setid.
  CLEAR l_node_tab. REFRESH l_node_tab.
  CLEAR l_valu_tab. REFRESH l_valu_tab.
*
  CALL FUNCTION 'K_HIERARCHY_TABLES_READ'
       EXPORTING
            e_class                           = '0000'
            e_setid                           = l_setid
            e_kokrs                           = p_bukrs
            e_mandt                           = sy-mandt
*           E_STRUCTURE                       =
       TABLES
            t_nodes                           = l_node_tab
            t_values                          = l_valu_tab
       CHANGING
            c_info                            = l_cinfo
            c_overwrite                       = sy-datar
       EXCEPTIONS
            no_controlling_area               = 1
            no_chart_of_account               = 2
            different_controlling_areas       = 3
            different_chart_of_accounts       = 4
            set_not_found                     = 5
            illegal_field_replacement         = 6
            illegal_table_replacement         = 7
            fm_raise                          = 8
            convert_error                     = 9
            no_overwrite_standard_hier        = 10
            no_bukrs_for_kokrs                = 11
            OTHERS                            = 12.
*
  IF sy-subrc = 0.
    SORT l_valu_tab BY vfrom.
    READ TABLE l_valu_tab INDEX 1.
    p_fracc = l_valu_tab-vfrom.
    SORT l_valu_tab BY vfrom DESCENDING.
    READ TABLE l_valu_tab INDEX 1.
    p_tracc = l_valu_tab-vto.
  ENDIF.
ENDFORM.                    " GET_ACCOUNT_RANGE
*&---------------------------------------------------------------------*
*&      Form  GET_BASE_COMP_AMOUNT
*&---------------------------------------------------------------------*
FORM get_base_comp_amount.
  CLEAR it_4thd. REFRESH it_4thd.
*
  LOOP AT it_3rdd WHERE zseti <> 'D'.
    CLEAR: w_basea, w_compa.
    PERFORM get_base_amount USING it_3rdd-zcfid
                                  it_3rdd-fracc it_3rdd-tracc.
    CLEAR: w_basea, w_compa.
    PERFORM get_comp_amount USING it_3rdd-zcfid
                                  it_3rdd-fracc it_3rdd-tracc.
    it_3rdd-diffa = it_3rdd-basea - it_3rdd-compa.
    READ TABLE it_calc INDEX 1.
*Issue # 20041111-012 Requested by Paul, Changed by wskim, on 20041116
*Error
*-----Start
*    it_3rdd-rtcur = it_calc-rtcur.
*-----End
*    READ TABLE IT_4THD WITH KEY ZCFID = IT_3RDD-ZCFID.
*    IF SY-SUBRC = 0.
*      IT_3RDD-ICONS = '4'.
*    ELSE.
*      IT_3RDD-ICONS = '00'.
*    ENDIF.
    MODIFY it_3rdd. CLEAR it_3rdd.
  ENDLOOP.
*
  LOOP AT it_2ndd.
    CASE it_2ndd-zseti.
      WHEN 'D'.
        PERFORM sum_step3.
      WHEN OTHERS.
        CLEAR: w_basea, w_compa.
        PERFORM get_base_amount USING it_2ndd-zcfid
                                      it_2ndd-fracc it_2ndd-tracc.
        it_2ndd-basea = it_3rdd-basea.
        CLEAR it_3rdd-basea.
        CLEAR: w_basea, w_compa.
        PERFORM get_comp_amount USING it_2ndd-zcfid
                                      it_2ndd-fracc it_2ndd-tracc.
        it_2ndd-compa = it_3rdd-compa.
        CLEAR it_3rdd-compa.
        it_2ndd-diffa = it_2ndd-basea - it_2ndd-compa.
        READ TABLE it_calc INDEX 1.
*Issue # 20041111-012 Requested by Paul, Changed by wskim, on 20041116
*Error
*-----Start
*        it_2ndd-rtcur = it_calc-rtcur.
*-----End
    ENDCASE.
    MODIFY it_2ndd. CLEAR it_2ndd.
  ENDLOOP.
*
  LOOP AT it_1std.
    CASE it_1std-zseti.
      WHEN 'D'.
        PERFORM sum_step2.
      WHEN OTHERS.
        CLEAR: w_basea, w_compa.
        PERFORM get_base_amount USING it_1std-zcfid
                                      it_1std-fracc it_1std-tracc.
        it_1std-basea = it_3rdd-basea.
        CLEAR it_3rdd-basea.
        CLEAR: w_basea, w_compa.
        PERFORM get_comp_amount USING it_1std-zcfid
                                      it_1std-fracc it_1std-tracc.
        it_1std-compa = it_3rdd-compa.
        CLEAR it_3rdd-compa.
        it_1std-diffa = it_1std-basea - it_1std-compa.
        READ TABLE it_calc INDEX 1.
*Issue # 20041111-012 Requested by Paul, Changed by wskim, on 20041116
*Error
*-----Start
*        it_1std-rtcur = it_calc-rtcur.
*-----End
    ENDCASE.
    MODIFY it_1std. CLEAR it_1std.
  ENDLOOP.
*
  CLEAR it_temp. REFRESH it_temp.
  it_temp[] = it_1std[].
*
  PERFORM calculate_by_formula.
*---2004/03/15
*  DELETE it_4thd WHERE diffa = 0.
  SORT it_4thd BY zcfid racct.
ENDFORM.                    " GET_BASE_COMP_AMOUNT
*&---------------------------------------------------------------------*
*&      Form  GET_BASE_AMOUNT
*&---------------------------------------------------------------------*
FORM get_base_amount USING p_zcfid p_fracc p_tracc.
  CLEAR it_calc. REFRESH it_calc.
*
  READ TABLE s_bdate INDEX 1.
  READ TABLE r_byear INDEX 1.
  CLEAR glfunct.
*Issue # 20041111-012 Requested by Paul, Changed by wskim, on 20041116
*Error
*  SELECT ryear rtcur drcrk racct hslvt hsl01 hsl02 hsl03 hsl04
*         hsl05 hsl06 hsl07 hsl08 hsl09 hsl10 hsl11 hsl12
*    INTO (it_calc-ryear, it_calc-rtcur, it_calc-drcrk, it_calc-racct,
*          it_calc-hslvt, it_calc-hsl01, it_calc-hsl02, it_calc-hsl03,
*          it_calc-hsl04, it_calc-hsl05, it_calc-hsl06, it_calc-hsl07,
*          it_calc-hsl08, it_calc-hsl09, it_calc-hsl10, it_calc-hsl11,
*          it_calc-hsl12)
  SELECT ryear  drcrk racct hslvt hsl01 hsl02 hsl03 hsl04
         hsl05 hsl06 hsl07 hsl08 hsl09 hsl10 hsl11 hsl12
    INTO (it_calc-ryear, it_calc-drcrk, it_calc-racct,
          it_calc-hslvt, it_calc-hsl01, it_calc-hsl02, it_calc-hsl03,
          it_calc-hsl04, it_calc-hsl05, it_calc-hsl06, it_calc-hsl07,
          it_calc-hsl08, it_calc-hsl09, it_calc-hsl10, it_calc-hsl11,
          it_calc-hsl12)
*    FROM glfunct where ryear IN r_byear
     FROM glfunct WHERE rldnr EQ p_rldnr
                   AND  rrcty EQ p_rrcty
                   AND  ryear IN r_byear
*-----End
                   AND rbukrs = p_bukrs
                   AND racct >= p_fracc
                   AND racct <= p_tracc.
    COLLECT it_calc. CLEAR it_calc.
  ENDSELECT.
  READ TABLE it_flow WITH KEY zcfid = p_zcfid.
  PERFORM check_account_number.
*
  CASE it_flow-zcalc.
    WHEN '1'.
      PERFORM get_amount_entry_b1.
      it_3rdd-basea = w_basea - w_compa.
      LOOP AT it_4thd WHERE zcfid = p_zcfid.
        it_4thd-basea = it_4thd-hsl01 - it_4thd-hsl02.
        it_4thd-hsl01 = 0.
        it_4thd-hsl02 = 0.
        MODIFY it_4thd. CLEAR it_4thd.
      ENDLOOP.
    WHEN '2' OR '0'.
      PERFORM get_amount_entry_b1.
      it_3rdd-basea = -1 * ( w_compa - w_basea ).
      LOOP AT it_4thd WHERE zcfid = p_zcfid.
        it_4thd-basea = -1 * ( it_4thd-hsl02 - it_4thd-hsl01 ).
        it_4thd-hsl01 = 0.
        it_4thd-hsl02 = 0.
        MODIFY it_4thd. CLEAR it_4thd.
      ENDLOOP.
    WHEN '3'.
      PERFORM get_amount_entry_b2 USING 'S'.
      w_compa = w_basea.
      CLEAR w_basea.
      PERFORM get_amount_entry_b2 USING 'H'.
      it_3rdd-basea = w_compa - w_basea.
      LOOP AT it_4thd WHERE zcfid = p_zcfid.
        it_4thd-basea = it_4thd-hsl01 - it_4thd-hsl02.
        it_4thd-hsl01 = 0.
        it_4thd-hsl02 = 0.
        MODIFY it_4thd. CLEAR it_4thd.
      ENDLOOP.
    WHEN '4'.
      PERFORM get_amount_entry_b2 USING 'S'.
      w_compa = w_basea.
      CLEAR w_basea.
      PERFORM get_amount_entry_b2 USING 'H'.
      it_3rdd-basea = w_basea - w_compa.
      LOOP AT it_4thd WHERE zcfid = p_zcfid.
        it_4thd-basea = it_4thd-hsl02 - it_4thd-hsl01.
        it_4thd-hsl01 = 0.
        it_4thd-hsl02 = 0.
        MODIFY it_4thd. CLEAR it_4thd.
      ENDLOOP.
    WHEN '5'.
      PERFORM get_amount_entry_b2 USING 'S'.
      it_3rdd-basea = w_basea.
      LOOP AT it_4thd WHERE zcfid = p_zcfid.
        it_4thd-basea = it_4thd-hsl01.
        it_4thd-hsl01 = 0.
        it_4thd-hsl02 = 0.
        MODIFY it_4thd. CLEAR it_4thd.
      ENDLOOP.
    WHEN '6'.
      PERFORM get_amount_entry_b2 USING 'H'.
      it_3rdd-basea = w_basea.
      LOOP AT it_4thd WHERE zcfid = p_zcfid.
        it_4thd-basea = it_4thd-hsl02.
        it_4thd-hsl01 = 0.
        it_4thd-hsl02 = 0.
        MODIFY it_4thd. CLEAR it_4thd.
      ENDLOOP.
    WHEN '7'.
      PERFORM get_amount_entry_b3 USING 'B'.
      it_3rdd-basea = w_basea.
      LOOP AT it_4thd WHERE zcfid = p_zcfid.
        it_4thd-basea = it_4thd-hsl01.
        it_4thd-hsl01 = 0.
        it_4thd-hsl02 = 0.
        MODIFY it_4thd. CLEAR it_4thd.
      ENDLOOP.
    WHEN '8'.
      PERFORM get_amount_entry_b3 USING 'E'.
      it_3rdd-basea = w_basea.
      LOOP AT it_4thd WHERE zcfid = p_zcfid.
        it_4thd-basea = it_4thd-hsl01.
        it_4thd-hsl01 = 0.
        it_4thd-hsl02 = 0.
        MODIFY it_4thd. CLEAR it_4thd.
      ENDLOOP.
  ENDCASE.
ENDFORM.                    " GET_BASE_AMOUNT
*&---------------------------------------------------------------------*
*&      Form  GET_COMP_AMOUNT
*&---------------------------------------------------------------------*
FORM get_comp_amount USING p_zcfid p_fracc p_tracc.
  CLEAR it_calc. REFRESH it_calc.
*
  READ TABLE s_cdate INDEX 1.
  READ TABLE r_cyear INDEX 1.
  CLEAR glfunct.
*Issue # 20041111-012 Requested by Paul, Changed by wskim, on 20041116
*Error
*  SELECT ryear rtcur drcrk racct hslvt hsl01 hsl02 hsl03 hsl04
*         hsl05 hsl06 hsl07 hsl08 hsl09 hsl10 hsl11 hsl12
*    INTO (it_calc-ryear, it_calc-rtcur, it_calc-drcrk, it_calc-racct,
*          it_calc-hslvt, it_calc-hsl01, it_calc-hsl02, it_calc-hsl03,
*          it_calc-hsl04, it_calc-hsl05, it_calc-hsl06, it_calc-hsl07,
*          it_calc-hsl08, it_calc-hsl09, it_calc-hsl10, it_calc-hsl11,
*          it_calc-hsl12)
  SELECT ryear  drcrk racct hslvt hsl01 hsl02 hsl03 hsl04
         hsl05 hsl06 hsl07 hsl08 hsl09 hsl10 hsl11 hsl12
    INTO (it_calc-ryear, it_calc-drcrk, it_calc-racct,
          it_calc-hslvt, it_calc-hsl01, it_calc-hsl02, it_calc-hsl03,
          it_calc-hsl04, it_calc-hsl05, it_calc-hsl06, it_calc-hsl07,
          it_calc-hsl08, it_calc-hsl09, it_calc-hsl10, it_calc-hsl11,
          it_calc-hsl12)
*    FROM glfunct where ryear IN r_cyear
     FROM glfunct WHERE rldnr EQ p_rldnr
                   AND  rrcty EQ p_rrcty
                   AND  ryear IN r_cyear
*-----End
                   AND  rbukrs = p_bukrs
                   AND  racct >= p_fracc
                   AND  racct <= p_tracc.
    COLLECT it_calc. CLEAR it_calc.
  ENDSELECT.
  READ TABLE it_flow WITH KEY zcfid = p_zcfid.
  PERFORM check_account_number.

  CASE it_flow-zcalc.
    WHEN '1'.
      PERFORM get_amount_entry_c1.
      it_3rdd-compa = w_basea - w_compa.
      LOOP AT it_4thd WHERE zcfid = p_zcfid.
        it_4thd-compa = it_4thd-hsl01 - it_4thd-hsl02.
        it_4thd-diffa = it_4thd-basea - it_4thd-compa.
        it_4thd-hsl01 = 0.
        it_4thd-hsl02 = 0.
        MODIFY it_4thd. CLEAR it_4thd.
      ENDLOOP.
    WHEN '2' OR '0'.
      PERFORM get_amount_entry_c1.
      it_3rdd-compa = -1 * ( w_compa - w_basea ).
      LOOP AT it_4thd WHERE zcfid = p_zcfid.
        it_4thd-compa = -1 * ( it_4thd-hsl02 - it_4thd-hsl01 ).
        it_4thd-diffa = it_4thd-basea - it_4thd-compa.
        it_4thd-hsl01 = 0.
        it_4thd-hsl02 = 0.
        MODIFY it_4thd. CLEAR it_4thd.
      ENDLOOP.
    WHEN '3'.
      PERFORM get_amount_entry_c2 USING 'S'.
      w_compa = w_basea.
      CLEAR w_basea.
      PERFORM get_amount_entry_c2 USING 'H'.
      it_3rdd-compa = w_compa - w_basea.
      LOOP AT it_4thd WHERE zcfid = p_zcfid.
        it_4thd-compa = it_4thd-hsl01 - it_4thd-hsl02.
        it_4thd-diffa = it_4thd-basea - it_4thd-compa.
        it_4thd-hsl01 = 0.
        it_4thd-hsl02 = 0.
        MODIFY it_4thd. CLEAR it_4thd.
      ENDLOOP.
    WHEN '4'.
      PERFORM get_amount_entry_c2 USING 'S'.
      w_compa = w_basea.
      CLEAR w_basea.
      PERFORM get_amount_entry_c2 USING 'H'.
      it_3rdd-compa = w_basea - w_compa.
      LOOP AT it_4thd WHERE zcfid = p_zcfid.
        it_4thd-compa = it_4thd-hsl02 - it_4thd-hsl01.
        it_4thd-diffa = it_4thd-basea - it_4thd-compa.
        it_4thd-hsl01 = 0.
        it_4thd-hsl02 = 0.
        MODIFY it_4thd. CLEAR it_4thd.
      ENDLOOP.
    WHEN '5'.
      PERFORM get_amount_entry_c2 USING 'S'.
      it_3rdd-compa = w_basea.
      LOOP AT it_4thd WHERE zcfid = p_zcfid.
        it_4thd-compa = it_4thd-hsl01.
        it_4thd-diffa = it_4thd-basea - it_4thd-compa.
        it_4thd-hsl01 = 0.
        it_4thd-hsl02 = 0.
        MODIFY it_4thd. CLEAR it_4thd.
      ENDLOOP.
    WHEN '6'.
      PERFORM get_amount_entry_c2 USING 'H'.
      it_3rdd-compa = w_basea.
      LOOP AT it_4thd WHERE zcfid = p_zcfid.
        it_4thd-compa = it_4thd-hsl02.
        it_4thd-diffa = it_4thd-basea - it_4thd-compa.
        it_4thd-hsl01 = 0.
        it_4thd-hsl02 = 0.
        MODIFY it_4thd. CLEAR it_4thd.
      ENDLOOP.
    WHEN '7'.
      PERFORM get_amount_entry_c3 USING 'B'.
      it_3rdd-compa = w_basea.
      LOOP AT it_4thd WHERE zcfid = p_zcfid.
        it_4thd-compa = it_4thd-hsl01.
        it_4thd-diffa = it_4thd-basea - it_4thd-compa.
        it_4thd-hsl01 = 0.
        it_4thd-hsl02 = 0.
        MODIFY it_4thd. CLEAR it_4thd.
      ENDLOOP.
    WHEN '8'.
      PERFORM get_amount_entry_c3 USING 'E'.
      it_3rdd-compa = w_basea.
      LOOP AT it_4thd WHERE zcfid = p_zcfid.
        it_4thd-compa = it_4thd-hsl01.
        it_4thd-diffa = it_4thd-basea - it_4thd-compa.
        it_4thd-hsl01 = 0.
        it_4thd-hsl02 = 0.
        MODIFY it_4thd. CLEAR it_4thd.
      ENDLOOP.
  ENDCASE.
ENDFORM.                    " GET_COMP_AMOUNT
*&---------------------------------------------------------------------*
*&      Form  GET_AMOUNT_ENTRY_B1
*&---------------------------------------------------------------------*
FORM get_amount_entry_b1.
  CLEAR: w_count, w_numbe.
*
  IF r_byear-option = 'EQ'.
    LOOP AT it_calc.
      w_count = s_bdate-low+4(2).
      w_count = w_count - 1.
      w_numbe = '01'.
      DO w_count TIMES.
        CLEAR w_fldn1.
        CONCATENATE 'IT_CALC-HSL' w_numbe INTO w_fldn1.
        ASSIGN (w_fldn1) TO <fs1>.
        w_basea = w_basea + <fs1>.
        it_4thd-hsl01 = it_4thd-hsl01 + <fs1>.
        w_numbe = w_numbe + 1.
      ENDDO.
      w_count = s_bdate-high+4(2).
      w_numbe = '01'.
      DO w_count TIMES.
        CLEAR w_fldn2.
        CONCATENATE 'IT_CALC-HSL' w_numbe INTO w_fldn2.
        ASSIGN (w_fldn2) TO <fs2>.
        w_compa = w_compa + <fs2>.
        it_4thd-hsl02 = it_4thd-hsl02 + <fs2>.
        w_numbe = w_numbe + 1.
      ENDDO.
      w_basea = w_basea + it_calc-hslvt.
      w_compa = w_compa + it_calc-hslvt.
      it_4thd-zcfid = it_flow-zcfid.
*Issue # 20041111-012 Requested by paul,changed by wskim,on 20041116
*error
*-----Start
*     it_4thd-rtcur = it_calc-rtcur.
*-----End
      it_4thd-racct = it_calc-racct.
      it_4thd-hsl01 = it_4thd-hsl01 + it_calc-hslvt.
      it_4thd-hsl02 = it_4thd-hsl02 + it_calc-hslvt.
      COLLECT it_4thd. CLEAR it_4thd.
    ENDLOOP.
  ELSE.
    LOOP AT it_calc WHERE ryear = r_byear-low.
      w_count = s_bdate-low+4(2).
      w_count = w_count - 1.
      w_numbe = '01'.
      DO w_count TIMES.
        CLEAR w_fldn1.
        CONCATENATE 'IT_CALC-HSL' w_numbe INTO w_fldn1.
        ASSIGN (w_fldn1) TO <fs1>.
        w_basea = w_basea + <fs1>.
        it_4thd-hsl01 = it_4thd-hsl01 + <fs1>.
        w_numbe = w_numbe + 1.
      ENDDO.
      w_basea = w_basea + it_calc-hslvt.
      it_4thd-zcfid = it_flow-zcfid.
*Issue # 20041111-012 Requested by paul,changed by wskim,on 20041116
*error
*-----Start
*      it_4thd-rtcur = it_calc-rtcur.
*-----End
      it_4thd-racct = it_calc-racct.
      it_4thd-hsl01 = it_4thd-hsl01 + it_calc-hslvt.
      COLLECT it_4thd. CLEAR it_4thd.
    ENDLOOP.
    LOOP AT it_calc WHERE ryear = r_byear-high.
      w_count = s_bdate-high+4(2).
      w_numbe = '01'.
      DO w_count TIMES.
        CLEAR w_fldn2.
        CONCATENATE 'IT_CALC-HSL' w_numbe INTO w_fldn2.
        ASSIGN (w_fldn2) TO <fs2>.
        w_compa = w_compa + <fs2>.
        it_4thd-hsl02 = it_4thd-hsl02 + <fs2>.
        w_numbe = w_numbe + 1.
      ENDDO.
      w_compa = w_compa + it_calc-hslvt.
      it_4thd-zcfid = it_flow-zcfid.
*Issue # 20041111-012 Requested by paul,changed by wskim,on 20041116
*error
*-----Start
*      it_4thd-rtcur = it_calc-rtcur.
*-----End
      it_4thd-racct = it_calc-racct.
      it_4thd-hsl02 = it_4thd-hsl02 + it_calc-hslvt.
      COLLECT it_4thd. CLEAR it_4thd.
    ENDLOOP.
  ENDIF.
*
  IF it_flow-zdmth = '1'.
    w_basea = -1 * w_basea.
    w_compa = -1 * w_compa.
    LOOP AT it_4thd WHERE zcfid = it_flow-zcfid.
      it_4thd-hsl01 = -1 * it_4thd-hsl01.
      it_4thd-hsl02 = -1 * it_4thd-hsl02.
      MODIFY it_4thd. CLEAR it_4thd.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " GET_AMOUNT_ENTRY_B1
*&---------------------------------------------------------------------*
*&      Form  GET_AMOUNT_ENTRY_C1
*&---------------------------------------------------------------------*
FORM get_amount_entry_c1.
  CLEAR: w_count, w_numbe.
*
  IF r_cyear-option = 'EQ'.
    LOOP AT it_calc.
      w_count = s_cdate-low+4(2).
      w_count = w_count - 1.
      w_numbe = '01'.
      DO w_count TIMES.
        CLEAR w_fldn1.
        CONCATENATE 'IT_CALC-HSL' w_numbe INTO w_fldn1.
        ASSIGN (w_fldn1) TO <fs1>.
        w_basea = w_basea + <fs1>.
        it_4thd-hsl01 = it_4thd-hsl01 + <fs1>.
        w_numbe = w_numbe + 1.
      ENDDO.
      w_count = s_cdate-high+4(2).
      w_numbe = '01'.
      DO w_count TIMES.
        CLEAR w_fldn2.
        CONCATENATE 'IT_CALC-HSL' w_numbe INTO w_fldn2.
        ASSIGN (w_fldn2) TO <fs2>.
        w_compa = w_compa + <fs2>.
        it_4thd-hsl02 = it_4thd-hsl02 + <fs2>.
        w_numbe = w_numbe + 1.
      ENDDO.
      w_basea = w_basea + it_calc-hslvt.
      w_compa = w_compa + it_calc-hslvt.
      it_4thd-zcfid = it_flow-zcfid.
*Issue # 20041111-012 Requested by paul,changed by wskim,on 20041116
*error
*-----Start
*      it_4thd-rtcur = it_calc-rtcur.
*-----End
      it_4thd-racct = it_calc-racct.
      it_4thd-hsl01 = it_4thd-hsl01 + it_calc-hslvt.
      it_4thd-hsl02 = it_4thd-hsl02 + it_calc-hslvt.
      COLLECT it_4thd. CLEAR it_4thd.
    ENDLOOP.
  ELSE.
    LOOP AT it_calc WHERE ryear = r_cyear-low.
      w_count = s_cdate-low+4(2).
      w_count = w_count - 1.
      w_numbe = '01'.
      DO w_count TIMES.
        CLEAR w_fldn1.
        CONCATENATE 'IT_CALC-HSL' w_numbe INTO w_fldn1.
        ASSIGN (w_fldn1) TO <fs1>.
        w_basea = w_basea + <fs1>.
        it_4thd-hsl01 = it_4thd-hsl01 + <fs1>.
        w_numbe = w_numbe + 1.
      ENDDO.
      w_basea = w_basea + it_calc-hslvt.
      it_4thd-zcfid = it_flow-zcfid.
*Issue # 20041111-012 Requested by paul,changed by wskim,on 20041116
*error
*-----Start
*      it_4thd-rtcur = it_calc-rtcur.
*-----End
      it_4thd-racct = it_calc-racct.
      it_4thd-hsl01 = it_4thd-hsl01 + it_calc-hslvt.
      COLLECT it_4thd. CLEAR it_4thd.
    ENDLOOP.
    LOOP AT it_calc WHERE ryear = r_cyear-high.
      w_count = s_cdate-high+4(2).
      w_numbe = '01'.
      DO w_count TIMES.
        CLEAR w_fldn2.
        CONCATENATE 'IT_CALC-HSL' w_numbe INTO w_fldn2.
        ASSIGN (w_fldn2) TO <fs2>.
        w_compa = w_compa + <fs2>.
        it_4thd-hsl02 = it_4thd-hsl02 + <fs2>.
        w_numbe = w_numbe + 1.
      ENDDO.
      w_compa = w_compa + it_calc-hslvt.
      it_4thd-zcfid = it_flow-zcfid.
*Issue # 20041111-012 Requested by paul,changed by wskim,on 20041116
*error
*-----Start
*      it_4thd-rtcur = it_calc-rtcur.
*-----End
      it_4thd-racct = it_calc-racct.
      it_4thd-hsl02 = it_4thd-hsl02 + it_calc-hslvt.
      COLLECT it_4thd. CLEAR it_4thd.
    ENDLOOP.
  ENDIF.
*
  IF it_flow-zdmth = '1'.
    w_basea = -1 * w_basea.
    w_compa = -1 * w_compa.
    LOOP AT it_4thd WHERE zcfid = it_flow-zcfid.
      it_4thd-hsl01 = -1 * it_4thd-hsl01.
      it_4thd-hsl02 = -1 * it_4thd-hsl02.
      MODIFY it_4thd. CLEAR it_4thd.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " GET_AMOUNT_ENTRY_C1
*&---------------------------------------------------------------------*
*&      Form  GET_AMOUNT_ENTRY_B2
*&---------------------------------------------------------------------*
FORM get_amount_entry_b2 USING p_value.
  IF r_byear-option = 'EQ'.
    w_count = s_bdate-high+4(2) - s_bdate-low+4(2) + 1.
    LOOP AT it_calc WHERE drcrk = p_value.
*----2004/03/15
      w_count = s_bdate-high+4(2) - s_bdate-low+4(2) + 1.
      IF w_count = 1.
        w_count = s_bdate-low+4(2).
        w_numbe = s_bdate-low+4(2).
      ELSE.
        w_numbe = '01'.
      ENDIF.
      DO w_count TIMES.
*----2004/03/15
        IF w_numbe > w_count.
          EXIT.
        ENDIF.
        CLEAR w_fldn1.
        CONCATENATE 'IT_CALC-HSL' w_numbe INTO w_fldn1.
        ASSIGN (w_fldn1) TO <fs1>.
        IF p_value = 'S'.
          w_basea = w_basea + <fs1>.
          it_4thd-hsl01 = it_4thd-hsl01 + <fs1>.
        ELSEIF p_value = 'H'.
          w_basea = w_basea + ( -1 * <fs1> ).
          it_4thd-hsl02 = it_4thd-hsl02 + ( -1 * <fs1> ).
        ENDIF.
        w_numbe = w_numbe + 1.
      ENDDO.
      it_4thd-zcfid = it_flow-zcfid.
*Issue # 20041111-012 Requested by paul,changed by wskim,on 20041116
*error
*-----Start
*      it_4thd-rtcur = it_calc-rtcur.
*-----End
      it_4thd-racct = it_calc-racct.
      COLLECT it_4thd. CLEAR it_4thd.
    ENDLOOP.
  ELSE.
    w_count = 12 - s_bdate-low+4(2) + 1.
    LOOP AT it_calc WHERE ryear = r_byear-low
                      AND drcrk = p_value.
      w_numbe = '01'.
      DO w_count TIMES.
        CLEAR w_fldn1.
        CONCATENATE 'IT_CALC-HSL' w_numbe INTO w_fldn1.
        ASSIGN (w_fldn1) TO <fs1>.
        IF p_value = 'S'.
          w_basea = w_basea + <fs1>.
          it_4thd-hsl01 = it_4thd-hsl01 + <fs1>.
        ELSEIF p_value = 'H'.
          w_basea = w_basea + ( -1 * <fs1> ).
          it_4thd-hsl02 = it_4thd-hsl02 + ( -1 * <fs1> ).
        ENDIF.
        w_numbe = w_numbe + 1.
      ENDDO.
      it_4thd-zcfid = it_flow-zcfid.
*Issue # 20041111-012 Requested by paul,changed by wskim,on 20041116
*error
*-----Start
*      it_4thd-rtcur = it_calc-rtcur.
*-----End
      it_4thd-racct = it_calc-racct.
      COLLECT it_4thd. CLEAR it_4thd.
    ENDLOOP.

    w_count = s_bdate-high+4(2).
    LOOP AT it_calc WHERE ryear = r_byear-high
                      AND drcrk = p_value.
      w_numbe = '01'.
      DO w_count TIMES.
        CLEAR w_fldn1.
        CONCATENATE 'IT_CALC-HSL' w_numbe INTO w_fldn1.
        ASSIGN (w_fldn1) TO <fs1>.
        w_basea = w_basea + <fs1>.
        w_numbe = w_numbe + 1.
        IF p_value = 'S'.
          it_4thd-hsl01 = it_4thd-hsl01 + <fs1>.
        ELSEIF p_value = 'H'.
          it_4thd-hsl02 = it_4thd-hsl02 + <fs1>.
        ENDIF.
      ENDDO.
      it_4thd-zcfid = it_flow-zcfid.
*Issue # 20041111-012 Requested by paul,changed by wskim,on 20041116
*error
*-----Start
*      it_4thd-rtcur = it_calc-rtcur.
*-----End
      it_4thd-racct = it_calc-racct.
      COLLECT it_4thd. CLEAR it_4thd.
    ENDLOOP.
  ENDIF.
*
  IF it_flow-zdmth = '1'.
    w_basea = -1 * w_basea.
    w_compa = -1 * w_compa.
    LOOP AT it_4thd WHERE zcfid = it_flow-zcfid.
      it_4thd-hsl01 = -1 * it_4thd-hsl01.
      it_4thd-hsl02 = -1 * it_4thd-hsl02.
      MODIFY it_4thd. CLEAR it_4thd.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " GET_AMOUNT_ENTRY_B2
*&---------------------------------------------------------------------*
*&      Form  GET_AMOUNT_ENTRY_C2
*&---------------------------------------------------------------------*
FORM get_amount_entry_c2 USING p_value.
  IF r_cyear-option = 'EQ'.
    w_count = s_cdate-high+4(2) - s_cdate-low+4(2) + 1.
    LOOP AT it_calc WHERE drcrk = p_value.
      w_numbe = '01'.
      DO w_count TIMES.
        CLEAR w_fldn1.
        CONCATENATE 'IT_CALC-HSL' w_numbe INTO w_fldn1.
        ASSIGN (w_fldn1) TO <fs1>.
        IF p_value = 'S'.
          w_basea = w_basea + <fs1>.
          it_4thd-hsl01 = it_4thd-hsl01 + <fs1>.
        ELSEIF p_value = 'H'.
          w_basea = w_basea + ( -1 * <fs1> ).
          it_4thd-hsl02 = it_4thd-hsl02 + ( -1 * <fs1> ).
        ENDIF.
        w_numbe = w_numbe + 1.
      ENDDO.
      it_4thd-zcfid = it_flow-zcfid.
*Issue # 20041111-012 Requested by paul,changed by wskim,on 20041116
*error
*-----Start
*      it_4thd-rtcur = it_calc-rtcur.
*-----End
      it_4thd-racct = it_calc-racct.
      COLLECT it_4thd. CLEAR it_4thd.
    ENDLOOP.
  ELSE.
    w_count = 12 - s_cdate-low+4(2) + 1.
    LOOP AT it_calc WHERE ryear = r_cyear-low
                      AND drcrk = p_value.
      w_numbe = '01'.
      DO w_count TIMES.
        CLEAR w_fldn1.
        CONCATENATE 'IT_CALC-HSL' w_numbe INTO w_fldn1.
        ASSIGN (w_fldn1) TO <fs1>.
        IF p_value = 'S'.
          w_basea = w_basea + <fs1>.
          it_4thd-hsl01 = it_4thd-hsl01 + <fs1>.
        ELSEIF p_value = 'H'.
          w_basea = w_basea + ( -1 * <fs1> ).
          it_4thd-hsl02 = it_4thd-hsl02 + ( -1 * <fs1> ).
        ENDIF.
        w_numbe = w_numbe + 1.
      ENDDO.
      it_4thd-zcfid = it_flow-zcfid.
*Issue # 20041111-012 Requested by paul,changed by wskim,on 20041116
*error
*-----Start
*      it_4thd-rtcur = it_calc-rtcur.
*-----End
      it_4thd-racct = it_calc-racct.
      COLLECT it_4thd. CLEAR it_4thd.
    ENDLOOP.

    w_count = s_cdate-high+4(2).
    LOOP AT it_calc WHERE ryear = r_cyear-high
                      AND drcrk = p_value.
      w_numbe = '01'.
      DO w_count TIMES.
        CLEAR w_fldn1.
        CONCATENATE 'IT_CALC-HSL' w_numbe INTO w_fldn1.
        ASSIGN (w_fldn1) TO <fs1>.
        w_basea = w_basea + <fs1>.
        w_numbe = w_numbe + 1.
        IF p_value = 'S'.
          it_4thd-hsl01 = it_4thd-hsl01 + <fs1>.
        ELSEIF p_value = 'H'.
          it_4thd-hsl02 = it_4thd-hsl02 + <fs1>.
        ENDIF.
      ENDDO.
      it_4thd-zcfid = it_flow-zcfid.
*Issue # 20041111-012 Requested by paul,changed by wskim,on 20041116
*error
*-----Start
*      it_4thd-rtcur = it_calc-rtcur.
*-----End
      it_4thd-racct = it_calc-racct.
      COLLECT it_4thd. CLEAR it_4thd.
    ENDLOOP.
  ENDIF.
*
  IF it_flow-zdmth = '1'.
    w_basea = -1 * w_basea.
    w_compa = -1 * w_compa.
    LOOP AT it_4thd WHERE zcfid = it_flow-zcfid.
      it_4thd-hsl01 = -1 * it_4thd-hsl01.
      it_4thd-hsl02 = -1 * it_4thd-hsl02.
      MODIFY it_4thd. CLEAR it_4thd.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " GET_AMOUNT_ENTRY_C2
*&---------------------------------------------------------------------*
*&      Form  GET_AMOUNT_ENTRY_B3
*&---------------------------------------------------------------------*
FORM get_amount_entry_b3 USING p_value.
  CLEAR: w_count, w_numbe.
*
  CASE p_value.
    WHEN 'B'.
      LOOP AT it_calc WHERE ryear = s_bdate-low+(4).
        w_count = s_bdate-low+4(2).
        w_count = w_count - 1.
        w_numbe = '01'.
        DO w_count TIMES.
          CLEAR w_fldn1.
          CONCATENATE 'IT_CALC-HSL' w_numbe INTO w_fldn1.
          ASSIGN (w_fldn1) TO <fs1>.
          w_basea = w_basea + <fs1>.
          it_4thd-hsl01 = it_4thd-hsl01 + <fs1>.
          w_numbe = w_numbe + 1.
        ENDDO.
        w_basea = w_basea + it_calc-hslvt.
        it_4thd-zcfid = it_flow-zcfid.
*Issue # 20041111-012 Requested by paul,changed by wskim,on 20041116
*error
*-----Start
*        it_4thd-rtcur = it_calc-rtcur.
*-----End
        it_4thd-racct = it_calc-racct.
        it_4thd-hsl01 = it_4thd-hsl01 + it_calc-hslvt.
        COLLECT it_4thd. CLEAR it_4thd.
      ENDLOOP.
    WHEN 'E'.
      LOOP AT it_calc WHERE ryear = s_bdate-high+(4).
        w_count = s_bdate-high+4(2).
        w_numbe = '01'.
        DO w_count TIMES.
          CLEAR w_fldn1.
          CONCATENATE 'IT_CALC-HSL' w_numbe INTO w_fldn1.
          ASSIGN (w_fldn1) TO <fs1>.
          w_basea = w_basea + <fs1>.
          it_4thd-hsl01 = it_4thd-hsl01 + <fs1>.
          w_numbe = w_numbe + 1.
        ENDDO.
        w_basea = w_basea + it_calc-hslvt.
        it_4thd-zcfid = it_flow-zcfid.
*Issue # 20041111-012 Requested by paul,changed by wskim,on 20041116
*error
*-----Start
*        it_4thd-rtcur = it_calc-rtcur.
*-----End
        it_4thd-racct = it_calc-racct.
        it_4thd-hsl01 = it_4thd-hsl01 + it_calc-hslvt.
        COLLECT it_4thd. CLEAR it_4thd.
      ENDLOOP.
  ENDCASE.
*
  IF it_flow-zdmth = '1'.
    w_basea = -1 * w_basea.
    w_compa = -1 * w_compa.
    LOOP AT it_4thd WHERE zcfid = it_flow-zcfid.
      it_4thd-hsl01 = -1 * it_4thd-hsl01.
      MODIFY it_4thd. CLEAR it_4thd.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " GET_AMOUNT_ENTRY_B3
*&---------------------------------------------------------------------*
*&      Form  GET_AMOUNT_ENTRY_C3
*&---------------------------------------------------------------------*
FORM get_amount_entry_c3 USING p_value.
  CLEAR: w_count, w_numbe.
*
  CASE p_value.
    WHEN 'B'.
      LOOP AT it_calc WHERE ryear = s_cdate-low+(4).
        w_count = s_cdate-low+4(2).
        w_count = w_count - 1.
        w_numbe = '01'.
        DO w_count TIMES.
          CLEAR w_fldn1.
          CONCATENATE 'IT_CALC-HSL' w_numbe INTO w_fldn1.
          ASSIGN (w_fldn1) TO <fs1>.
          w_basea = w_basea + <fs1>.
          it_4thd-hsl01 = it_4thd-hsl01 + <fs1>.
          w_numbe = w_numbe + 1.
        ENDDO.
        w_basea = w_basea + it_calc-hslvt.
        it_4thd-zcfid = it_flow-zcfid.
*Issue # 20041111-012 Requested by paul,changed by wskim,on 20041116
*error
*-----Start
*        it_4thd-rtcur = it_calc-rtcur.
*-----End
        it_4thd-racct = it_calc-racct.
        it_4thd-hsl01 = it_4thd-hsl01 + it_calc-hslvt.
        COLLECT it_4thd. CLEAR it_4thd.
      ENDLOOP.
    WHEN 'E'.
      LOOP AT it_calc WHERE ryear = s_cdate-high+(4).
        w_count = s_cdate-high+4(2).
        w_numbe = '01'.
        DO w_count TIMES.
          CLEAR w_fldn1.
          CONCATENATE 'IT_CALC-HSL' w_numbe INTO w_fldn1.
          ASSIGN (w_fldn1) TO <fs1>.
          w_basea = w_basea + <fs1>.
          it_4thd-hsl01 = it_4thd-hsl01 + <fs1>.
          w_numbe = w_numbe + 1.
        ENDDO.
        w_basea = w_basea + it_calc-hslvt.
        it_4thd-zcfid = it_flow-zcfid.
*Issue # 20041111-012 Requested by paul,changed by wskim,on 20041116
*error
*-----Start
*        it_4thd-rtcur = it_calc-rtcur.
*-----End
        it_4thd-racct = it_calc-racct.
        it_4thd-hsl01 = it_4thd-hsl01 + it_calc-hslvt.
        COLLECT it_4thd. CLEAR it_4thd.
      ENDLOOP.
  ENDCASE.
*
  IF it_flow-zdmth = '1'.
    w_basea = -1 * w_basea.
    w_compa = -1 * w_compa.
    LOOP AT it_4thd WHERE zcfid = it_flow-zcfid.
      it_4thd-hsl01 = -1 * it_4thd-hsl01.
      MODIFY it_4thd. CLEAR it_4thd.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " GET_AMOUNT_ENTRY_C3
*&---------------------------------------------------------------------*
*&      Form  SUM_STEP3
*&---------------------------------------------------------------------*
FORM sum_step3.
  CLEAR: w_basea, w_compa.
  LOOP AT it_3rdd WHERE zcfid+(2) = it_2ndd-zcfid.
    w_basea = w_basea + it_3rdd-basea.
    w_compa = w_compa + it_3rdd-compa.
  ENDLOOP.
  it_2ndd-basea = w_basea.
  it_2ndd-compa = w_compa.
  it_2ndd-diffa = w_basea - w_compa.
  it_2ndd-rtcur = it_3rdd-rtcur.
ENDFORM.                                                    " SUM_STEP3
*&---------------------------------------------------------------------*
*&      Form  SUM_STEP2
*&---------------------------------------------------------------------*
FORM sum_step2.
  CLEAR: w_basea, w_compa.
  READ TABLE it_flow WITH KEY zcfid = it_1std-zcfid.
*
  CASE it_flow-zform.
    WHEN 'S'.
      LOOP AT it_2ndd WHERE zcfid+(1) = it_1std-zcfid.
        w_basea = w_basea + it_2ndd-basea.
        w_compa = w_compa + it_2ndd-compa.
      ENDLOOP.
      it_1std-basea = w_basea.
      it_1std-compa = w_compa.
      it_1std-diffa = w_basea - w_compa.
      it_1std-rtcur = it_2ndd-rtcur.
  ENDCASE.
ENDFORM.                                                    " SUM_STEP2
*&---------------------------------------------------------------------*
*&      Form  AMOUNT_CALCULATE
*&---------------------------------------------------------------------*
FORM amount_calculate.
  DATA: l_basa1 LIKE glfunct-hsl01,
        l_basa2 LIKE glfunct-hsl01,
        l_basa3 LIKE glfunct-hsl01,
        l_coma1 LIKE glfunct-hsl01,
        l_coma2 LIKE glfunct-hsl01,
        l_coma3 LIKE glfunct-hsl01,
        l_sign1,
        l_sign2.
  CLEAR it_splt. REFRESH it_splt.
  CLEAR: l_basa1, l_basa2, l_basa3, l_coma1, l_coma2, l_coma3,
         l_sign1, l_sign2.
*
  SPLIT it_flow-zform AT space INTO TABLE it_splt.
  READ TABLE it_splt INDEX 1.
  READ TABLE it_temp WITH KEY zcfid = it_splt-zword.
  IF sy-subrc = 0.
    l_basa1 = it_temp-basea.
    l_coma1 = it_temp-compa.
  ELSE.
    READ TABLE it_2ndd WITH KEY zcfid = it_splt-zword.
    l_basa1 = it_2ndd-basea.
    l_coma1 = it_2ndd-compa.
  ENDIF.
*
  READ TABLE it_splt INDEX 2.
  l_sign1 = it_splt-zword+(1).
*
  READ TABLE it_splt INDEX 3.
  READ TABLE it_temp WITH KEY zcfid = it_splt-zword.
  IF sy-subrc = 0.
    l_basa2 = it_temp-basea.
    l_coma2 = it_temp-compa.
  ELSE.
    READ TABLE it_2ndd WITH KEY zcfid = it_splt-zword.
    l_basa2 = it_2ndd-basea.
    l_coma2 = it_2ndd-compa.
  ENDIF.
*
  READ TABLE it_splt INDEX 4.
  IF sy-subrc = 0.
    l_sign2 = it_splt-zword+(1).
    READ TABLE it_splt INDEX 5.
    READ TABLE it_temp WITH KEY zcfid = it_splt-zword.
    IF sy-subrc = 0.
      l_basa3 = it_temp-basea.
      l_coma3 = it_temp-compa.
    ELSE.
      READ TABLE it_2ndd WITH KEY zcfid = it_splt-zword.
      l_basa3 = it_2ndd-basea.
      l_coma3 = it_2ndd-compa.
    ENDIF.
  ENDIF.
*
  CALL FUNCTION 'Z_FFI_EVAL_FORMULA'
       EXPORTING
            i_basa1 = l_basa1
            i_basa2 = l_basa2
            i_basa3 = l_basa3
            i_coma1 = l_coma1
            i_coma2 = l_coma2
            i_coma3 = l_coma3
            i_sign1 = l_sign1
            i_sign2 = l_sign2
       IMPORTING
            e_basea = w_basea
            e_compa = w_compa.
ENDFORM.                    " AMOUNT_CALCULATE
*&---------------------------------------------------------------------*
*&      Form  CALCULATE_BY_FORMULA
*&---------------------------------------------------------------------*
FORM calculate_by_formula.
  DO 2 TIMES.
    LOOP AT it_1std WHERE zseti = 'D'.
      READ TABLE it_flow WITH KEY zcfid = it_1std-zcfid.
      IF it_flow-zform <> 'S'.
        CLEAR: w_basea, w_compa.
        PERFORM amount_calculate.
        it_1std-basea = w_basea.
        it_1std-compa = w_compa.
        it_1std-diffa = w_basea - w_compa.
        it_1std-rtcur = it_temp-rtcur.
        CLEAR it_temp.
        it_temp-basea = w_basea.
        it_temp-compa = w_compa.
        it_temp-diffa = w_basea - w_compa.
        MODIFY it_temp TRANSPORTING basea compa diffa
         WHERE zcfid = it_1std-zcfid
           AND zseti = it_1std-zseti.
        MODIFY it_1std. CLEAR it_1std.
      ENDIF.
    ENDLOOP.
    CLEAR it_temp. REFRESH it_temp.
    it_temp[] = it_1std[].
  ENDDO.
ENDFORM.                    " CALCULATE_BY_FORMULA
*&---------------------------------------------------------------------*
*&      Form  WRITE_4TH_BODY
*&---------------------------------------------------------------------*
FORM write_4th_body USING p_zcfid.
* ULINE AT (W_WIDTH).
  LOOP AT it_4thd WHERE zcfid = p_zcfid.
    FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
    WRITE: / sy-vline NO-GAP.
    WRITE: (4) ' ' NO-GAP, sy-vline NO-GAP.
    WRITE: (18) ' ' NO-GAP.
    WRITE: (45) it_4thd-racct NO-GAP, sy-vline NO-GAP.
    WRITE: (19) it_4thd-basea CURRENCY it_4thd-rtcur NO-GAP,
                sy-vline NO-GAP.
    WRITE: (19) it_4thd-compa CURRENCY it_4thd-rtcur NO-GAP,
                sy-vline NO-GAP.
    WRITE: (19) it_4thd-diffa CURRENCY it_4thd-rtcur NO-GAP,
                sy-vline NO-GAP.
  ENDLOOP.
*
* ULINE AT (W_WIDTH).
ENDFORM.                    " WRITE_4TH_BODY
*&---------------------------------------------------------------------*
*&      Form  CHECK_ACCOUNT_NUMBER
*&---------------------------------------------------------------------*
FORM check_account_number.
  LOOP AT it_calc.
    LOOP AT it_acct WHERE zcfid = it_flow-zcfid
                      AND fracc <= it_calc-racct
                      AND tracc >= it_calc-racct.
    ENDLOOP.
    IF sy-subrc <> 0.
      DELETE it_calc.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " CHECK_ACCOUNT_NUMBER
