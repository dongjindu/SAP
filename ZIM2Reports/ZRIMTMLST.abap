*&---------------------------------------------------------------------*
*& Report  ZRIMTMLST                                                   *
*&---------------------------------------------------------------------*
*&  PROGRAM NAME : Material-in-transit list                            *
*&    Created On : Na Hyun Joo(INFO-LINK)                              *
*&    Created by : 04.29.2004                                          *
*&---------------------------------------------------------------------*
*&  Change History :                                                   *
*&---------------------------------------------------------------------*
REPORT  zrimtmlst     MESSAGE-ID zim
                      LINE-SIZE 200
                      NO STANDARD PAGE HEADING.

*-----------------------------------------------------------------------
* Tables & Variable Define
*-----------------------------------------------------------------------
INCLUDE : <icon>,
          zrimtmtop,
          zrimbdccom.

*-----------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_bukrs    FOR ztbkpf-bukrs  NO-EXTENSION
                                             NO INTERVALS,
                s_yymm     FOR sy-datum(6)   NO-EXTENSION
                                             NO INTERVALS,
                s_ebeln    FOR ekpo-ebeln                ,
                s_matnr    FOR ekpo-matnr                .
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
SELECTION-SCREEN : BEGIN OF LINE.
SELECTION-SCREEN :   COMMENT 2(23) text-r01.
PARAMETERS : r_goods RADIOBUTTON GROUP rdg DEFAULT 'X'.
SELECTION-SCREEN :   COMMENT 33(4) text-r02.
PARAMETERS : r_duty  RADIOBUTTON GROUP rdg.
SELECTION-SCREEN :   COMMENT 47(7) text-r03.
PARAMETERS : r_fret  RADIOBUTTON GROUP rdg.
SELECTION-SCREEN :   COMMENT 64(12) text-r04.
PARAMETERS : r_other RADIOBUTTON GROUP rdg.
SELECTION-SCREEN : END OF LINE.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
SELECTION-SCREEN : BEGIN OF LINE.
SELECTION-SCREEN :      COMMENT 33(10) text-p01.
PARAMETERS : p_po  AS   CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN :      COMMENT 47(12) text-p02.
PARAMETERS : p_sum AS   CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN :      COMMENT 64(14) text-p03.
PARAMETERS : p_total AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN : END OF LINE.
SELECTION-SCREEN END OF BLOCK b3.

*-----------------------------------------------------------------------
* INITIALIZATION
*-----------------------------------------------------------------------
INITIALIZATION.
  PERFORM  p1000_set_bukrs.
  SET  TITLEBAR 'ZIMY13'.

*-----------------------------------------------------------------------
* TOP OF PAGE
*-----------------------------------------------------------------------
TOP-OF-PAGE.
  IF p_po EQ 'X'.
    PERFORM  p1000_title_write_po.
  ELSE.
    PERFORM  p1000_title_write.
  ENDIF.

*-----------------------------------------------------------------------
* START OF SELECTION .
*-----------------------------------------------------------------------
START-OF-SELECTION.

*>> Selection Condition Check.
  PERFORM p2000_search_cond_check  USING  w_err_chk.
  IF w_err_chk EQ 'Y'.  EXIT.  ENDIF.

*>> DATA SELECT!
  PERFORM p2000_read_text      USING  w_err_chk.
  IF w_err_chk EQ 'Y'.  MESSAGE s966. EXIT.  ENDIF.

*>> Data Write!
  PERFORM p2000_write_text     USING  w_err_chk.
  IF w_err_chk EQ 'Y'.   EXIT.  ENDIF.

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.

  CASE sy-ucomm.
    WHEN 'DIPO'.

*------- Abbrechen (CNCL) ----------------------------------------------
    WHEN 'CNCL'.
      SET SCREEN 0.    LEAVE SCREEN.
*------- Suchen (SUCH) -------------------------------------------------
    WHEN 'SUCH'.
*------- Sortieren nach Feldbezeichnung (SORB) -------------------------
    WHEN 'SORB'.
*------- Sortieren nach Feldname (SORF) --------------------------------
    WHEN 'SORF'.
*------- Techn. Name ein/aus (TECH) ------------------------------------
    WHEN 'TECH'.
*------- Weiter suchen (WESU) ------------------------------------------
    WHEN 'WESU'.
    WHEN OTHERS.
  ENDCASE.

*-----------------------------------------------------------------------
*&   Event AT LINE-SELECTION
*-----------------------------------------------------------------------
AT LINE-SELECTION.

  DATA : l_text(30).
  GET CURSOR FIELD l_text.
  CASE  l_text.
    WHEN 'IT_TAB-EBELN'  OR  'IT_TAB-EBELP'.
      SET PARAMETER ID 'BES'  FIELD it_tab-ebeln.
      SET PARAMETER ID 'BSP'  FIELD it_tab-ebelp.
      CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
  ENDCASE.

*&---------------------------------------------------------------------*
*&      Form  P2000_READ_TEXT
*&---------------------------------------------------------------------*
FORM p2000_read_text USING    w_err_chk.

  CLEAR : w_err_chk.

  SET PF-STATUS 'ZIMY13'.
  SET TITLEBAR  'ZIMY13'.

  " Selection Period Set.
  PERFORM  p3000_period_set.

  " P/O SELECT.
  IF r_other EQ 'X'.
    PERFORM  p3000_po_select.
  ENDIF.

  " This Month Data Select
  IF r_goods EQ 'X'.
    PERFORM  p3000_ekbe_table_select.
  ELSEIF r_duty EQ 'X'.
    PERFORM  p3000_ekbz_table_select.
  ELSEIF r_fret EQ 'X'.
    PERFORM  p3000_ekbz_table_select.
  ELSE.
    PERFORM  p3000_ztbdiv_table_select.
  ENDIF.

  IF r_other NE 'X'.
    " Account Set.
    PERFORM  p3000_account_set.

    " Last Month Amount Get.
    PERFORM  p3000_last_open_amount.

  ENDIF.

  " IMPORT P/O
  PERFORM  p3000_delete_po.

  " Internal Table For Display
  PERFORM  p3000_write_it_tab.

  DESCRIBE TABLE it_tab LINES  w_line.
  IF w_line EQ 0.  w_err_chk = 'Y'. ENDIF.

ENDFORM.                    " P2000_READ_TEXT

*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_IT_TAB
*&---------------------------------------------------------------------*
FORM p3000_write_it_tab.

  REFRESH : it_tab.

  SORT : it_temp BY  ebeln ebelp.

  LOOP  AT  it_temp.

    IF sy-tabix EQ 1.
      MOVE : it_temp-ebeln   TO  it_tab-ebeln,
             it_temp-ebelp   TO  it_tab-ebelp.
    ENDIF.

    IF it_temp-ebeln  NE  it_tab-ebeln  OR
       it_temp-ebelp  NE  it_tab-ebelp.

      SELECT SINGLE matnr txz01 menge meins
      INTO   (it_tab-matnr,  it_tab-maktx,
              it_tab-po_qty, it_tab-meins)
      FROM   ekpo
      WHERE  ebeln   EQ  it_tab-ebeln
      AND    ebelp   EQ  it_tab-ebelp.

      IF r_other EQ 'X'.
        it_tab-gr_qty = it_tab-gr_qty / 2.
      ENDIF.

      IF r_other NE 'X'.
        IF w_temp_amt  GT  0.
          MOVE  w_temp_amt   TO  it_tab-iv_last.
        ELSE.
          MOVE  w_temp_amt   TO  it_tab-gr_last.
        ENDIF.
      ENDIF.

      it_tab-balance = it_tab-iv_last + it_tab-iv_this -
                       it_tab-gr_last - it_tab-gr_this -
                       it_tab-settled.

      APPEND  it_tab.
      CLEAR : it_tab, w_temp_amt.
      MOVE : it_temp-ebeln  TO  it_tab-ebeln,
             it_temp-ebelp  TO  it_tab-ebelp.
    ENDIF.

    " Amount Compute.
    IF it_temp-vgabe EQ '1' AND it_temp-shkzg EQ 'H' .
      it_temp-dmbtr  =  it_temp-dmbtr * ( -1 ).
      it_temp-menge  =  it_temp-menge * ( -1 ).
    ELSEIF it_temp-vgabe EQ '2' AND it_temp-shkzg EQ 'H'.
      it_temp-dmbtr  =  it_temp-dmbtr * ( -1 ).
      it_temp-menge  =  it_temp-menge * ( -1 ).
    ENDIF.

    CASE  it_temp-bewtp.
        "-------------------------------------------------
        " Goods Price   : 'P', 'Q', 'N'
        " Duty, Freight : 'M'
        " Ohter Expense : 'D'
        "-------------------------------------------------
      WHEN 'P' OR 'Q' OR 'N' OR 'M'.
        it_tab-iv_this = it_tab-iv_this + it_temp-dmbtr.
        "-------------------------------------------------
        " Goods Price   : 'E'
        " Duty, Freight : 'F'
        " Ohter Expense : 'C'
        "-------------------------------------------------
      WHEN 'E' .
        it_tab-gr_this  = it_tab-gr_this + it_temp-dmbtr.
        it_tab-gr_qty   = it_tab-gr_qty  + it_temp-menge.
      WHEN 'F' OR 'C'.
        it_temp-dmbtr   = it_temp-dmbtr  * ( -1 ).
        it_tab-gr_this  = it_tab-gr_this + it_temp-dmbtr.
        it_tab-gr_qty   = it_tab-gr_qty  + it_temp-menge.
      WHEN 'L'.
        w_temp_amt  =  w_temp_amt + it_temp-dmbtr.
      WHEN OTHERS.
        IF NOT it_temp-budat IS INITIAL  AND
               it_temp-vgabe IS INITIAL.
          IF it_temp-budat LT w_first_date.
            it_tab-iv_last = it_tab-iv_last + it_temp-dmbtr.
          ELSE.
            it_tab-iv_this = it_tab-iv_this + it_temp-dmbtr.
          ENDIF.
        ELSE.
          it_tab-settled  =  it_tab-settled +  it_temp-lc_act +
                             it_temp-bl_act -  it_temp-lc_pld -
                             it_temp-bl_pld.
        ENDIF.
    ENDCASE.

    AT LAST.
      SELECT SINGLE matnr txz01 menge meins
      INTO   (it_tab-matnr,  it_tab-maktx,
              it_tab-po_qty, it_tab-meins)
      FROM   ekpo
      WHERE  ebeln   EQ  it_tab-ebeln
      AND    ebelp   EQ  it_tab-ebelp.

      IF r_other EQ 'X'.
        it_tab-gr_qty = it_tab-gr_qty / 2.
      ENDIF.

      IF r_other NE 'X'.
        IF w_temp_amt  GT  0.
          MOVE  w_temp_amt   TO  it_tab-iv_last.
        ELSE.
          MOVE  w_temp_amt   TO  it_tab-gr_last.
        ENDIF.
      ENDIF.

      it_tab-balance = it_tab-iv_last + it_tab-iv_this -
                       it_tab-gr_last - it_tab-gr_this -
                       it_tab-settled.

      APPEND  it_tab.
      CLEAR : it_tab, w_temp_amt.
      MOVE : it_temp-ebeln  TO  it_tab-ebeln,
             it_temp-ebelp  TO  it_tab-ebelp.
    ENDAT.

  ENDLOOP.

ENDFORM.                    " P3000_WRITE_IT_TAB
*&---------------------------------------------------------------------*
*&      Form  P1000_SET_BUKRS
*&---------------------------------------------------------------------*
FORM p1000_set_bukrs.

  CLEAR : ztimimg00, p_bukrs.
  SELECT SINGLE * FROM ztimimg00.
  IF NOT ztimimg00-zfbufix IS INITIAL.
    MOVE  ztimimg00-zfbukrs   TO  p_bukrs.
  ENDIF.

*>> Company Code Set.
  MOVE: 'I'           TO s_bukrs-sign,
        'EQ'          TO s_bukrs-option,
        p_bukrs       TO s_bukrs-low.
  APPEND s_bukrs.

*>> Year, Month Set.
  MOVE: 'I'           TO s_yymm-sign,
        'EQ'          TO s_yymm-option,
        sy-datum(6)   TO s_yymm-low.
  APPEND s_yymm.

ENDFORM.                    " P1000_SET_BUKRS
*&---------------------------------------------------------------------*
*&      Form  P1000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM p1000_title_write.

  SKIP 2.
  WRITE:/100 '[Material-in-transit list]'
                            COLOR COL_HEADING INTENSIFIED OFF.
  SKIP 2.
  WRITE:/ 'Date:',sy-datum.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.

  IF r_other EQ 'X'.
    WRITE:/ sy-uline.
  ELSE.
    WRITE:/(183) sy-uline.
  ENDIF.

  WRITE:/                                          sy-vline NO-GAP,
         (16) '   '                        NO-GAP, sy-vline NO-GAP,
         (37) '   '                        NO-GAP, sy-vline NO-GAP,
         (05) '    '                       NO-GAP, sy-vline NO-GAP,
         (17) '      '            CENTERED NO-GAP, sy-vline NO-GAP,
         (17) '       '           CENTERED NO-GAP, sy-vline NO-GAP,
         (33) 'MIT(I/V) '         CENTERED NO-GAP, sy-vline NO-GAP,
         (33) 'MIT(G/R)'          CENTERED NO-GAP, sy-vline NO-GAP.

  IF r_other EQ 'X'.
    WRITE: (16) '   '            CENTERED NO-GAP, sy-vline NO-GAP,
           (16) '   '            CENTERED NO-GAP, sy-vline NO-GAP.
  ELSE.
    WRITE: (16) '   '            CENTERED NO-GAP, sy-vline NO-GAP.
  ENDIF.

  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE:/                                          sy-vline NO-GAP,
         (16) 'P/O-Item'                   NO-GAP, sy-vline NO-GAP,
         (37) 'Material No'                NO-GAP, sy-vline NO-GAP,
         (05) 'Unit'                       NO-GAP, sy-vline NO-GAP,
         (17) 'P/O Quantity'      CENTERED NO-GAP, sy-vline NO-GAP,
         (17) 'G/R Quantity'      CENTERED NO-GAP, sy-vline NO-GAP,
         (33)  sy-uline                    NO-GAP, sy-vline NO-GAP,
         (33)  sy-uline                    NO-GAP, sy-vline NO-GAP.

  IF r_other EQ 'X'.
    WRITE: (16) 'Settled Amount' CENTERED NO-GAP, sy-vline NO-GAP,
           (16) 'Balance'        CENTERED NO-GAP, sy-vline NO-GAP.
  ELSE.
    WRITE: (16) 'Balance'        CENTERED NO-GAP, sy-vline NO-GAP.
  ENDIF.

  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE:/                                          sy-vline NO-GAP,
         (16) '     '                      NO-GAP, sy-vline NO-GAP,
         (37) '     '                      NO-GAP, sy-vline NO-GAP,
         (05) '     '                      NO-GAP, sy-vline NO-GAP,
         (17) '     '             CENTERED NO-GAP, sy-vline NO-GAP,
         (17) '     '             CENTERED NO-GAP, sy-vline NO-GAP,
         (16) '~Last Month'       CENTERED NO-GAP, sy-vline NO-GAP,
         (16) 'This Month'        CENTERED NO-GAP, sy-vline NO-GAP,
         (16) '~Last Month'       CENTERED NO-GAP, sy-vline NO-GAP,
         (16) 'This Month'        CENTERED NO-GAP, sy-vline NO-GAP.

  IF r_other EQ 'X'.
    WRITE: (16) '   '            CENTERED NO-GAP, sy-vline NO-GAP,
           (16) '   '            CENTERED NO-GAP, sy-vline NO-GAP.
  ELSE.
    WRITE: (16) '   '            CENTERED NO-GAP, sy-vline NO-GAP.
  ENDIF.

  IF r_other EQ 'X'.
    WRITE:/ sy-uline.
  ELSE.
    WRITE:/(183) sy-uline.
  ENDIF.

ENDFORM.                    " P1000_TITLE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P2000_WRITE_TEXT
*&---------------------------------------------------------------------*
FORM p2000_write_text USING    p_w_err_chk.

  SORT  it_tab  BY  ebeln  ebelp.
  CLEAR : w_ebeln, w_ebelp.

  LOOP AT it_tab.

    IF w_ebeln NE it_tab-ebeln .
      IF sy-tabix NE 1.
        PERFORM  p3000_po_line_write.
        CLEAR : it_tab_po.
      ELSEIF sy-tabix NE 1.
        IF p_po IS INITIAL.
          IF r_other EQ 'X'.
            WRITE : sy-uline.
          ELSE.
            WRITE : (183) sy-uline.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    w_mod  =  sy-tabix MOD 2.
    IF p_po IS INITIAL.
      PERFORM  p3000_line_write.
    ENDIF.

    " P/O Sum, Total Compute
    PERFORM  p4000_sum_compute.

    " P/O-Item Move
    MOVE : it_tab-ebeln  TO  w_ebeln.
  ENDLOOP.

  " P/O Sum Line Write
  PERFORM  p3000_po_line_write.

  " Total Line Write.
  IF p_sum EQ 'X'.
    PERFORM  p4000_total_line_write.
  ENDIF.

  IF p_po IS INITIAL AND p_sum IS INITIAL.
    IF r_other EQ 'X'.
      WRITE : sy-uline.
    ELSE.
      WRITE: (183) sy-uline.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_WRITE_TEXT
*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM p3000_line_write.

  IF it_tab-iv_this EQ 0 AND it_tab-gr_this GT 0.
    FORMAT RESET.
    FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
  ELSEIF w_mod EQ 0.
    FORMAT RESET.
    FORMAT COLOR COL_NORMAL   INTENSIFIED ON.
  ELSE.
    FORMAT RESET.
    FORMAT COLOR COL_NORMAL   INTENSIFIED OFF.
  ENDIF.

  IF it_tab-ebeln EQ 'ZZZZZZZZZZ'.
    MOVE 'Carry Forward'  TO  it_tab-ebeln.
  ENDIF.
  IF it_tab-ebelp EQ 'ZZZZZ'.
    CLEAR : it_tab-ebeln.
  ENDIF.

  WRITE:/                                          sy-vline NO-GAP,
        (10) it_tab-ebeln                 NO-GAP,
        (01) '-'                          NO-GAP,
        (05) it_tab-ebelp                 NO-GAP, sy-vline NO-GAP,
        (16) it_tab-matnr                 NO-GAP,
        (01) ' '                          NO-GAP,
        (20) it_tab-maktx                 NO-GAP, sy-vline NO-GAP,
        (05) it_tab-meins                 NO-GAP, sy-vline NO-GAP,
        (17) it_tab-po_qty    UNIT        it_tab-meins
                                          NO-GAP, sy-vline NO-GAP,
        (17) it_tab-gr_qty    UNIT        it_tab-meins
                                          NO-GAP, sy-vline NO-GAP,
        (16) it_tab-iv_last   CURRENCY    t001-waers
                                          NO-GAP, sy-vline NO-GAP,
        (16) it_tab-iv_this   CURRENCY    t001-waers
                                          NO-GAP, sy-vline NO-GAP,
        (16) it_tab-gr_last   CURRENCY    t001-waers
                                          NO-GAP, sy-vline NO-GAP,
        (16) it_tab-gr_this   CURRENCY    t001-waers
                                          NO-GAP, sy-vline NO-GAP.
  IF r_other EQ 'X'.
    WRITE: (16) it_tab-settled   CURRENCY    t001-waers
                                             NO-GAP, sy-vline NO-GAP,
           (16) it_tab-balance   CURRENCY    t001-waers
                                             NO-GAP, sy-vline NO-GAP.
  ELSE.
    WRITE: (16) it_tab-balance   CURRENCY    t001-waers
                                             NO-GAP, sy-vline NO-GAP.
  ENDIF.

  HIDE  it_tab.

ENDFORM.                    " P3000_LINE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P4000_TOTAL_LINE_WRITE
*&---------------------------------------------------------------------*
FORM p4000_total_line_write.

  FORMAT RESET.
  FORMAT COLOR COL_GROUP INTENSIFIED OFF.

  IF NOT p_po IS INITIAL.
    IF r_other EQ 'X'.
      WRITE:/(114) sy-uline.
    ELSE.
      WRITE:/(97) sy-uline.
    ENDIF.
  ENDIF.

  IF p_po IS INITIAL.
    WRITE:/                                       sy-vline NO-GAP,
          (96) 'Total'          CENTERED NO-GAP,  sy-vline NO-GAP.
  ELSE.
    WRITE:/                                       sy-vline NO-GAP,
          (10) 'Total'           CENTERED NO-GAP,  sy-vline NO-GAP.
  ENDIF.

  WRITE:(16) it_tab_total-iv_last CURRENCY t001-waers
                                           NO-GAP, sy-vline NO-GAP,
        (16) it_tab_total-iv_this CURRENCY t001-waers
                                           NO-GAP, sy-vline NO-GAP,
        (16) it_tab_total-gr_last CURRENCY t001-waers
                                           NO-GAP, sy-vline NO-GAP,
        (16) it_tab_total-gr_this CURRENCY t001-waers
                                           NO-GAP, sy-vline NO-GAP.
  IF r_other EQ 'X'.
    WRITE: (16) it_tab_total-settled CURRENCY t001-waers
                                     NO-GAP, sy-vline NO-GAP,
           (16) it_tab_total-balance CURRENCY t001-waers
                                     NO-GAP, sy-vline NO-GAP.
  ELSE.
    WRITE: (16) it_tab_total-balance CURRENCY t001-waers
                                     NO-GAP, sy-vline NO-GAP.
  ENDIF.

  IF p_po IS INITIAL.
    IF r_other EQ 'X'.
      WRITE : sy-uline.
    ELSE.
      WRITE : (183) sy-uline.
    ENDIF.
  ELSE.
    IF r_other EQ 'X'.
      WRITE:/(114) sy-uline.
    ELSE.
      WRITE:/(97) sy-uline.
    ENDIF.
  ENDIF.
ENDFORM.                    " P4000_TOTAL_LINE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P2000_SEARCH_COND_CHECK
*&---------------------------------------------------------------------*
FORM p2000_search_cond_check USING    p_w_err_chk.

*>> Company Check.
  IF s_bukrs IS INITIAL.
    w_err_chk  =  'Y'.
    MESSAGE  s977 WITH 'Input Company Code!' .
    EXIT.
  ENDIF.

*>> Year, Month Check.
  IF s_yymm IS INITIAL.
    w_err_chk  =  'Y'.
    MESSAGE  s977 WITH 'Input Year, Month!' .
    EXIT.
  ENDIF.

*>> Company Code Data Get
  CLEAR : t001.
  SELECT SINGLE * FROM t001 WHERE bukrs EQ s_bukrs-low.

ENDFORM.                    " P2000_SEARCH_COND_CHECK
*&---------------------------------------------------------------------*
*&      Form  P3000_PERIOD_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM p3000_period_set.

  " Last Month Compute.
  w_ym =  s_yymm-low - 1.
  CONCATENATE  s_yymm-low  '31'  INTO  w_last_date.
  CONCATENATE  s_yymm-low  '01'  INTO  w_first_date.
  CONCATENATE  W_YM        '01'  INTO  W_BEFORE_FIRST.
  CONCATENATE  W_YM        '31'  INTO  W_BEFORE_END.
ENDFORM.                    " P3000_PERIOD_SET
*&---------------------------------------------------------------------*
*&      Form  P3000_EKBE_TABLE_SELECT
*&---------------------------------------------------------------------*
FORM p3000_ekbe_table_select.

  REFRESH : it_temp.
  SELECT  a~ebeln AS ebeln  a~ebelp AS ebelp  a~bewtp AS bewtp
          a~vgabe AS vgabe  a~shkzg AS shkzg  a~budat AS budat
          a~menge AS menge  a~dmbtr AS dmbtr
  INTO    CORRESPONDING FIELDS OF TABLE it_temp
  FROM    ekbe  AS  a  INNER JOIN ekpo AS b
  ON      a~ebeln      EQ    b~ebeln
  AND     a~ebelp      EQ    b~ebelp
  WHERE   a~budat      GE    w_first_date
  AND     a~budat      LE    w_last_date
  AND     a~ebeln      IN    s_ebeln
  AND     b~knttp      EQ    space
  AND     a~bewtp      IN    ('Q', 'R', 'N', 'E').

ENDFORM.                    " P3000_EKBE_TABLE_SELECT
*&---------------------------------------------------------------------*
*&      Form  P3000_EKBZ_TABLE_SELECT
*&---------------------------------------------------------------------*
FORM p3000_ekbz_table_select.

  REFRESH : it_temp.
  IF r_duty EQ 'X'.
    SELECT SINGLE * FROM ztimimg08
    WHERE  zfcdty   EQ   '006'
    AND    zfcd     EQ   '001'.
  ELSE.
    SELECT SINGLE * FROM ztimimg08
    WHERE  zfcdty   EQ   '004'
    AND    zfcd     EQ   'OBC'.
  ENDIF.

  SELECT  a~ebeln AS ebeln  a~ebelp AS ebelp  a~bewtp AS bewtp
          a~vgabe AS vgabe  a~shkzg AS shkzg  a~budat AS budat
          a~menge AS menge  a~dmbtr AS dmbtr
  INTO    CORRESPONDING FIELDS OF TABLE it_temp
  FROM    ekbz  AS  a  INNER JOIN ekpo AS b
  ON      a~ebeln      EQ    b~ebeln
  AND     a~ebelp      EQ    b~ebelp
  WHERE   a~budat      GE    w_first_date
  AND     a~budat      LE    w_last_date
  AND     a~kschl      EQ    ztimimg08-cond_type
  AND     b~knttp      EQ    space
  AND     a~ebeln      IN    s_ebeln
  AND     a~bewtp      IN    ('M', 'F').

ENDFORM.                    " P3000_EKBZ_TABLE_SELECT
*&---------------------------------------------------------------------*
*&      Form  P3000_ZTBDIV_TABLE_SELECT
*&---------------------------------------------------------------------*
FORM p3000_ztbdiv_table_select.

  ">> Actual Cost Get.
  REFRESH : it_temp.
  SELECT  a~ebeln  a~ebelp   a~matnr
          b~budat  a~menge   a~dmbtr  a~zfsetyn AS vgabe
  INTO CORRESPONDING FIELDS OF TABLE it_temp
  FROM ( ztbdiv AS a INNER JOIN ztbkpf AS b
  ON     a~bukrs     EQ     b~bukrs
  AND    a~belnr     EQ     b~belnr
  AND    a~gjahr     EQ     b~gjahr    )
  INNER  JOIN  ekpo  AS     c
  ON     a~ebeln     EQ     c~ebeln
  AND    a~ebelp     EQ     c~ebelp
  WHERE  b~budat     GE     w_first_date
  AND    b~budat     LE     w_last_date
  AND    a~ebeln     IN     s_ebeln
  AND    c~knttp     EQ     space
  AND    a~zfdcstx   NE     'X'
  AND    b~zfposyn   EQ     'Y'.

  ">> G/R Cost Get
  SELECT  a~ebeln AS ebeln  a~ebelp AS ebelp  a~bewtp AS bewtp
          a~vgabe AS vgabe  a~shkzg AS shkzg  a~budat AS budat
          a~menge AS menge  a~dmbtr AS dmbtr
  APPENDING  CORRESPONDING FIELDS OF TABLE it_temp
  FROM    ekbz  AS  a  INNER JOIN ekpo AS b
  ON      a~ebeln      EQ    b~ebeln
  AND     a~ebelp      EQ    b~ebelp
  WHERE   a~budat      GE    w_first_date
  AND     a~budat      LE    w_last_date
  AND     a~ebeln      IN    s_ebeln
  AND     b~knttp      EQ    space
  AND     a~bewtp      EQ    'C'.

  ">> Settled Amount Get
  SELECT a~ebeln    AS ebeln  a~ebelp   AS ebelp  a~zfsetqty AS menge
         a~zfsetamt AS dmbtr  a~zfsetlc AS lc_act a~zfsetbl  AS bl_act
         a~zfpldlc  AS lc_pld a~zfpldbl AS bl_pld
  APPENDING  CORRESPONDING FIELDS OF TABLE it_temp
  FROM   ztsetac AS a  INNER JOIN ekpo AS b
  ON     a~ebeln       EQ    b~ebeln
  AND    a~ebelp       EQ    b~ebelp
  WHERE  b~knttp       EQ    space
  AND    a~ebeln       IN    s_ebeln
  AND    a~zfsetym     LE    s_yymm-low.

ENDFORM.                    " P3000_ZTBDIV_TABLE_SELECT

*&---------------------------------------------------------------------*
*&      Form  P4000_SUM_COMPUTE
*&---------------------------------------------------------------------*
FORM p4000_sum_compute.

  ADD : it_tab-iv_last  TO  it_tab_po-iv_last,
        it_tab-iv_this  TO  it_tab_po-iv_this,
        it_tab-gr_last  TO  it_tab_po-gr_last,
        it_tab-gr_this  TO  it_tab_po-gr_this,
        it_tab-settled  TO  it_tab_po-settled,
        it_tab-balance  TO  it_tab_po-balance,
        it_tab-iv_last  TO  it_tab_total-iv_last,
        it_tab-iv_this  TO  it_tab_total-iv_this,
        it_tab-gr_last  TO  it_tab_total-gr_last,
        it_tab-gr_this  TO  it_tab_total-gr_this,
        it_tab-settled  TO  it_tab_total-settled,
        it_tab-balance  TO  it_tab_total-balance.

ENDFORM.                    " P4000_SUM_COMPUTE
*&---------------------------------------------------------------------*
*&      Form  P3000_PO_LINE_WRITE
*&---------------------------------------------------------------------*
FORM p3000_po_line_write.

  IF p_po EQ 'X'.
    IF it_tab_po-iv_this EQ 0 AND it_tab_po-gr_this GT 0.
      FORMAT RESET.
      FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
    ELSE.
      FORMAT RESET.
      FORMAT COLOR COL_NORMAL   INTENSIFIED OFF.
    ENDIF.
  ELSE.
    FORMAT RESET.
    FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
  ENDIF.

  IF w_ebeln EQ 'ZZZZZZZZZZ'.
    MOVE 'Carry Forward'   TO  w_ebeln.
  ENDIF.

  IF p_po EQ 'X'.
    WRITE:/                                          sy-vline NO-GAP,
          (10) w_ebeln                      NO-GAP,  sy-vline NO-GAP.
  ELSE.
    WRITE:/                                          sy-vline NO-GAP,
          (10) w_ebeln                      NO-GAP,
          (86) '  '                         NO-GAP,  sy-vline NO-GAP.
  ENDIF.

  WRITE:(16) it_tab_po-iv_last   CURRENCY t001-waers
                                          NO-GAP, sy-vline NO-GAP,
        (16) it_tab_po-iv_this   CURRENCY t001-waers
                                          NO-GAP, sy-vline NO-GAP,
        (16) it_tab_po-gr_last   CURRENCY t001-waers
                                          NO-GAP, sy-vline NO-GAP,
        (16) it_tab_po-gr_this   CURRENCY t001-waers
                                          NO-GAP, sy-vline NO-GAP.
  IF r_other EQ 'X'.
    WRITE: (16) it_tab_po-settled   CURRENCY t001-waers
                                         NO-GAP, sy-vline NO-GAP,
           (16) it_tab_po-balance   CURRENCY t001-waers
                                         NO-GAP, sy-vline NO-GAP.
  ELSE.
    WRITE: (16) it_tab_po-balance   CURRENCY t001-waers
                                         NO-GAP, sy-vline NO-GAP.
  ENDIF.

  IF p_po IS INITIAL.
    IF r_other EQ 'X'.
      WRITE : sy-uline.
    ELSE.
      WRITE : (183) sy-uline.
    ENDIF.
  ENDIF.
ENDFORM.                    " P3000_PO_LINE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_ACCOUNT_SET
*&---------------------------------------------------------------------*
FORM p3000_account_set.

  REFRESH : r_account.

  IF r_goods EQ 'X'.
    MOVE : 'I'           TO      r_account-sign,
           'EQ'          TO      r_account-option,
           '0000138000'  TO      r_account-low,
           space         TO      r_account-high.
    APPEND  r_account.
  ELSEIF r_duty EQ 'X'.
    MOVE : 'I'           TO      r_account-sign,
           'EQ'          TO      r_account-option,
           '0000138400'  TO      r_account-low,
           space         TO      r_account-high.
    APPEND  r_account.
  ELSEIF r_fret EQ 'X'.
    MOVE : 'I'           TO      r_account-sign,
           'EQ'          TO      r_account-option,
           '0000138200'  TO      r_account-low,
           space         TO      r_account-high.
    APPEND  r_account.
  ENDIF.

ENDFORM.                    " P3000_ACCOUNT_SET
*&---------------------------------------------------------------------*
*&      Form  P3000_PO_SELECT
*&---------------------------------------------------------------------*
FORM p3000_po_select.

  REFRESH : it_po.
  CLEAR   : w_line, w_err_chk.

  SELECT ebeln ebelp INTO CORRESPONDING FIELDS OF TABLE it_po
  FROM   ztreqit
  WHERE  ebeln       IN   s_ebeln
  AND    matnr       IN   s_matnr
  GROUP BY
         EBELN ebelp.

  LOOP AT it_po.
    w_tabix  =  sy-tabix.

    SELECT SINGLE * FROM ztsetac
    WHERE  ebeln    EQ   it_po-ebeln
    AND    ebelp    EQ   it_po-ebelp
    AND    zfsetym  LT   w_ym
    AND    zfsetyn  EQ   'X'.

    IF sy-subrc EQ 0.
      DELETE  it_po  INDEX  w_tabix.
      CONTINUE.
    ENDIF.
  ENDLOOP.

  DESCRIBE  TABLE it_po LINES w_line.
  IF w_line EQ 0. w_err_chk = 'Y'.  ENDIF.

ENDFORM.                    " P3000_PO_SELECT
*&---------------------------------------------------------------------*
*&      Form  P3000_LAST_OPEN_AMOUNT
*&---------------------------------------------------------------------*
FORM p3000_last_open_amount.

  PERFORM  p4000_open_belnr_select.
  PERFORM  p4000_bseg_amount_get.

ENDFORM.                    " P3000_LAST_OPEN_AMOUNT
*&---------------------------------------------------------------------*
*&      Form  P4000_OPEN_BELNR_SELECT
*&---------------------------------------------------------------------*
FORM p4000_open_belnr_select.

  REFRESH : it_belnr.
  CLEAR : w_init_date.

*   SELECT    GJAHR  BELNR  BUZEI BUDAT
*   APPENDING CORRESPONDING FIELDS OF TABLE IT_BELNR
*   FROM      BSIS
*   WHERE     BUDAT   GE    W_BEFORE_FIRST
*   AND       BUDAT   LE    W_BEFORE_END
*   AND       AUGDT   NE    W_INIT_DATE
*   AND       HKONT   IN    R_ACCOUNT.

  SELECT    gjahr  belnr  buzei budat
  APPENDING CORRESPONDING FIELDS OF TABLE it_belnr
  FROM      bsis
  WHERE     budat   LE    w_before_end
  AND       augdt   EQ    w_init_date
  AND       hkont   IN    r_account.

*   SELECT    GJAHR  BELNR  BUZEI BUDAT
*   APPENDING CORRESPONDING FIELDS OF TABLE IT_BELNR
*   FROM      BSAS
*   WHERE     BUDAT   GE    W_BEFORE_FIRST
*   AND       BUDAT   LE    W_BEFORE_END
*   AND       AUGDT   NE    W_INIT_DATE
*   AND       HKONT   IN    R_ACCOUNT.

  SELECT    gjahr  belnr  buzei budat
  APPENDING CORRESPONDING FIELDS OF TABLE it_belnr
  FROM      bsas
  WHERE     budat   LE    w_before_end
  AND       augdt   EQ    w_init_date
  AND       hkont   IN    r_account.

ENDFORM.                    " P4000_OPEN_BELNR_SELECT

*&---------------------------------------------------------------------*
*&      Form  P4000_BSEG_AMOUNT_GET
*&---------------------------------------------------------------------*
FORM p4000_bseg_amount_get.

  DATA : w_loop_cnt TYPE i.

  LOOP AT it_belnr.

    CLEAR : it_temp.
    SELECT  SINGLE ebeln  ebelp  shkzg  menge  dmbtr
    INTO    (it_temp-ebeln, it_temp-ebelp, it_temp-shkzg,
             it_temp-menge, it_temp-dmbtr)
    FROM    bseg
    WHERE   gjahr     EQ   it_belnr-gjahr
    AND     belnr     EQ   it_belnr-belnr
    AND     buzei     EQ   it_belnr-buzei.

    CLEAR : w_loop_cnt.
    LOOP AT it_temp WHERE ebeln EQ it_temp-ebeln
                    AND   ebelp EQ it_temp-ebelp
                    AND   bewtp NE 'L'.
      w_loop_cnt = w_loop_cnt + 1.
    ENDLOOP.

    IF w_loop_cnt GT 0.
      IF p_total EQ 'X'.
        MOVE :'ZZZZZZZZZZ' TO   it_temp-ebeln,
              'ZZZZZ'      TO   it_temp-ebelp.
      ENDIF.
    ENDIF.

    MOVE:   'L'             TO   it_temp-bewtp,
            '1'             TO   it_temp-vgabe,
            it_belnr-budat  TO   it_temp-budat.
    APPEND  it_temp.

  ENDLOOP.

ENDFORM.                    " P4000_BSEG_AMOUNT_GET
*&---------------------------------------------------------------------*
*&      Form  P3000_DELETE_PO
*&---------------------------------------------------------------------*
FORM p3000_delete_po.

  LOOP AT it_temp.
    w_tabix  =  sy-tabix.
    SELECT SINGLE * FROM  ztreqit
    WHERE  ebeln    EQ    it_temp-ebeln
    AND    ebelp    EQ    it_temp-ebelp.
    IF sy-subrc NE 0.
      DELETE  it_temp  INDEX  w_tabix.
      CONTINUE.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " P3000_DELETE_PO
*&---------------------------------------------------------------------*
*&      Form  P1000_TITLE_WRITE_PO
*&---------------------------------------------------------------------*
FORM p1000_title_write_po.

  SKIP 2.
  WRITE:/40 '[Material-in-transit list]'
                            COLOR COL_HEADING INTENSIFIED OFF.
  SKIP 2.
  WRITE:/ 'Date:',sy-datum.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.

  IF r_other EQ 'X'.
    WRITE:/(114) sy-uline.
  ELSE.
    WRITE:/(97)  sy-uline.
  ENDIF.

  WRITE:/                                          sy-vline NO-GAP,
         (10) '   '                        NO-GAP, sy-vline NO-GAP,
         (33) 'MIT(I/V) '         CENTERED NO-GAP, sy-vline NO-GAP,
         (33) 'MIT(G/R)'          CENTERED NO-GAP, sy-vline NO-GAP.

  IF r_other EQ 'X'.
    WRITE: (16) '   '            CENTERED NO-GAP, sy-vline NO-GAP,
           (16) '   '            CENTERED NO-GAP, sy-vline NO-GAP.
  ELSE.
    WRITE: (16) '   '            CENTERED NO-GAP, sy-vline NO-GAP.
  ENDIF.

  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE:/                                          sy-vline NO-GAP,
         (10) 'P/O'               CENTERED NO-GAP, sy-vline NO-GAP,
         (33)  sy-uline                    NO-GAP, sy-vline NO-GAP,
         (33)  sy-uline                    NO-GAP, sy-vline NO-GAP.

  IF r_other EQ 'X'.
    WRITE: (16) 'Settled Amount' CENTERED NO-GAP, sy-vline NO-GAP,
           (16) 'Balance'        CENTERED NO-GAP, sy-vline NO-GAP.
  ELSE.
    WRITE: (16) 'Balance'        CENTERED NO-GAP, sy-vline NO-GAP.
  ENDIF.

  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE:/                                          sy-vline NO-GAP,
         (10) '     '                      NO-GAP, sy-vline NO-GAP,
         (16) '~Last Month'       CENTERED NO-GAP, sy-vline NO-GAP,
         (16) 'This Month'        CENTERED NO-GAP, sy-vline NO-GAP,
         (16) '~Last Month'       CENTERED NO-GAP, sy-vline NO-GAP,
         (16) 'This Month'        CENTERED NO-GAP, sy-vline NO-GAP.

  IF r_other EQ 'X'.
    WRITE: (16) '   '            CENTERED NO-GAP, sy-vline NO-GAP,
           (16) '   '            CENTERED NO-GAP, sy-vline NO-GAP.
  ELSE.
    WRITE: (16) '   '            CENTERED NO-GAP, sy-vline NO-GAP.
  ENDIF.

  IF r_other EQ 'X'.
    WRITE:/(114) sy-uline.
  ELSE.
    WRITE:/(97)  sy-uline.
  ENDIF.

ENDFORM.                    " P1000_TITLE_WRITE_PO
