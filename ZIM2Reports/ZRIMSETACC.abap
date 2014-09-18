*&---------------------------------------------------------------------*
*& Report  ZRIMSETACC                                                  *
*&---------------------------------------------------------------------*
*&  PROGRAM NAME : Material-in-transit list to Settle Accounts         *
*&    Created On : Na Hyun Joo(INFO-LINK)                              *
*&    Created by : 10.31.2003                                          *
*&---------------------------------------------------------------------*
*&  Change History :                                                   *
*&---------------------------------------------------------------------*
REPORT  zrimsetacc    MESSAGE-ID zim
                      LINE-SIZE 225
                      NO STANDARD PAGE HEADING.

*-----------------------------------------------------------------------
* Tables & Variable Define
*-----------------------------------------------------------------------
INCLUDE : <icon>,
          zrimsetacctop,
          zrimbdccom.

*-----------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS:     p_bukrs  LIKE bkpf-bukrs  OBLIGATORY MEMORY ID buk,
                p_year   LIKE bkpf-gjahr  OBLIGATORY ,
                p_month  LIKE bkpf-monat  OBLIGATORY .
PARAMETERS:     p_fi       AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.


PARAMETERS: p_mode     LIKE ctu_params-dismode DEFAULT 'N' NO-DISPLAY,
            p_sim(1)   TYPE c DEFAULT 'X'  NO-DISPLAY.

SELECT-OPTIONS: s_ebeln  FOR ekpo-ebeln,
                s_ebelp  FOR ekpo-ebelp.

SELECTION-SCREEN SKIP 9.
SELECTION-SCREEN SKIP 9.
SELECTION-SCREEN SKIP 9.
SELECTION-SCREEN SKIP 9.
PARAMETERS: p_alv      AS   CHECKBOX.

*--- ALV
TYPE-POOLS: slis.
DATA : w_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
       w_eventcat TYPE slis_t_event WITH HEADER LINE,
       w_selfield TYPE slis_selfield,
       w_sortcat  TYPE slis_t_sortinfo_alv WITH HEADER LINE,
       w_col_pos  TYPE i,
       w_program  LIKE sy-repid,
       w_top_of_page TYPE slis_t_listheader,
       w_line1 TYPE slis_listheader.

DATA: gt_fieldcat TYPE slis_t_fieldcat_alv,
      gs_layout   TYPE slis_layout_alv,
      gt_sp_group TYPE slis_t_sp_group_alv,
      gt_events   TYPE slis_t_event,
      gt_sorts    TYPE slis_t_sortinfo_alv WITH HEADER LINE,
      gs_prnt     TYPE slis_print_alv,
      gv_user_command TYPE slis_formname VALUE 'USER_COMMAND'.

*---- ALV

*-----------------------------------------------------------------------
* INITIALIZATION
*-----------------------------------------------------------------------
INITIALIZATION.
  SET  TITLEBAR 'ZIMY12'.

*-----------------------------------------------------------------------
* TOP OF PAGE
*-----------------------------------------------------------------------
TOP-OF-PAGE.
  IF include NE 'POPU'.
    PERFORM  p1000_title_write.
  ENDIF.

*-----------------------------------------------------------------------
* START OF SELECTION .
*-----------------------------------------------------------------------
START-OF-SELECTION.

*>> DATA SELECT!
  PERFORM p2000_read_text      USING  w_err_chk.

  IF w_err_chk EQ 'Y'.  MESSAGE s966. EXIT.  ENDIF.

*>> Data Write!
  IF p_alv = 'X'.
    PERFORM display_out.   "ANDY
  ELSE.
    PERFORM p2000_write_text     USING  w_err_chk.
  ENDIF.

  IF w_err_chk EQ 'Y'.   EXIT.  ENDIF.

*>> Batch Job
  IF NOT sy-batch IS INITIAL.
    PERFORM  p5000_settlment_account_proc.
    IF w_line LE 0.
      MESSAGE s908.
    ENDIF.
  ENDIF.

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.

  CASE sy-ucomm.
    WHEN 'POST'.
      PERFORM  p5000_settlment_account_proc.
      DESCRIBE  TABLE return   LINES  w_line.
      IF w_line GT 0.
        include = 'POPU'.
        CALL SCREEN 0100 STARTING AT  05   3
                         ENDING   AT  100 12.
        CLEAR : include.
      ELSE.
        MESSAGE s908.
      ENDIF.
      " REFRESH.
      PERFORM p2000_read_text       USING  w_err_chk.
      IF w_err_chk EQ 'Y'.
        LEAVE TO SCREEN 0.
      ENDIF.
      MOVE 0 TO sy-lsind.
      PERFORM p1000_title_write.
      PERFORM p2000_write_text      USING  w_err_chk.
    WHEN 'REFR'.
      " REFRESH.
      PERFORM p2000_read_text       USING  w_err_chk.
      IF w_err_chk EQ 'Y'.
        LEAVE TO SCREEN 0.
      ENDIF.
      MOVE 0 TO sy-lsind.
      PERFORM p1000_title_write.
      PERFORM p2000_write_text      USING  w_err_chk.

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
FORM p2000_read_text   USING    w_err_chk.

  CLEAR : w_err_chk.

  SET PF-STATUS 'ZIMY12'.
  SET TITLEBAR  'ZIMY12'.

  " Selection Period Set.
  PERFORM  p3000_period_set.

  " P/O SELECT.
  PERFORM  p3000_po_select.
  IF w_err_chk  EQ 'Y'.  EXIT.  ENDIF.

  " FI Data Select.
  PERFORM  p3000_setteld_data_select.

  " Internal Table For Display
  PERFORM  p3000_write_it_tab.

  DESCRIBE TABLE it_tab LINES  w_line.
  IF w_line EQ 0.  w_err_chk = 'Y'. ENDIF.

ENDFORM.                    " P2000_READ_TEXT

*&---------------------------------------------------------------------*
*&      Form  P3000_PO_SELECT
*&---------------------------------------------------------------------*
FORM p3000_po_select.

  REFRESH : it_po, it_bsis.
  CLEAR   : w_line, w_err_chk.

  CONCATENATE  p_year  p_month  INTO  w_ym.

  SELECT DISTINCT zuonr
    INTO CORRESPONDING FIELDS OF TABLE it_bsis
    FROM bsis
   WHERE hkont   IN   ('0000138400', '0000138490').

  LOOP AT it_bsis.
    MOVE : it_bsis-zuonr(10)     TO  it_po-ebeln,
           it_bsis-zuonr+10(05)  TO  it_po-ebelp.

    CHECK it_po-ebeln IN s_ebeln AND it_po-ebelp IN s_ebelp.

    APPEND it_po.
  ENDLOOP.

*   DELETE ADJACENT DUPLICATES FROM IT_BSIS COMPARING ALL FIELDS.

  DESCRIBE  TABLE it_po LINES w_line.
  IF w_line EQ 0. w_err_chk = 'Y'.  ENDIF.

ENDFORM.                    " P3000_PO_SELECT
*&---------------------------------------------------------------------*
*&      Form  P3000_SETTELD_DATA_SELECT
*&---------------------------------------------------------------------*
FORM p3000_setteld_data_select.

  PERFORM  p4000_actual_cst_select.
  PERFORM  p4000_planned_cst_select.
  PERFORM  p4000_setteld_cst_select.

ENDFORM.                    " P3000_SETTELD_DATA_SELECT

*&---------------------------------------------------------------------*
*&      Form  P4000_ACTUAL_CST_SELECT
*&---------------------------------------------------------------------*
FORM p4000_actual_cst_select.

*>> Actual Cost Selct.
  SELECT  b~ebeln      b~ebelp b~zfimdno      a~bukrs a~zffiyr a~zfacdo
          b~zfcstgrp AS cstgrp b~cond_type AS kschl   b~wrbtr  b~dmbtr
          b~hwaer      b~menge b~meins        b~matnr
  INTO    CORRESPONDING FIELDS OF TABLE it_div
  FROM    ztbkpf       AS  a  INNER  JOIN  ztbdiv  AS  b
  ON      a~bukrs      EQ  b~bukrs
  AND     a~gjahr      EQ  b~gjahr
  AND     a~belnr      EQ  b~belnr
  FOR     ALL ENTRIES  IN  it_po
  WHERE   b~ebeln      EQ  it_po-ebeln
  AND     b~ebelp      EQ  it_po-ebelp
  AND     a~zfposyn    EQ  'Y'
  AND     a~zfdcstx    EQ  ' '
  AND     a~budat      LE  w_date.

ENDFORM.                    " P4000_ACTUAL_CST_SELECT
*&---------------------------------------------------------------------*
*&      Form  P4000_PLANNED_CST_SELECT
*&---------------------------------------------------------------------*
FORM p4000_planned_cst_select.

  SELECT  *  INTO CORRESPONDING FIELDS OF TABLE it_ekbz
  FROM    ekbz
  FOR     ALL ENTRIES  IN  it_po
  WHERE   ebeln        EQ  it_po-ebeln
  AND     ebelp        EQ  it_po-ebelp
  AND     vgabe        EQ  '1'
  AND     bewtp        EQ  'C'
  AND     budat        LE  w_date.

ENDFORM.                    " P4000_PLANNED_CST_SELECT
*&---------------------------------------------------------------------*
*&      Form  P3000_PERIOD_SET
*&---------------------------------------------------------------------*
FORM p3000_period_set.

  CLEAR : ztimimg11.
  SELECT SINGLE * FROM ztimimg11 WHERE bukrs EQ p_bukrs.

  CONCATENATE  p_year  p_month  '31'  INTO  w_date.

ENDFORM.                    " P3000_PERIOD_SET
*&---------------------------------------------------------------------*
*&      Form  P4000_SETTELD_CST_SELECT
*&---------------------------------------------------------------------*
FORM p4000_setteld_cst_select.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_set
  FROM   ztsetac
  FOR    ALL ENTRIES  IN  it_po
  WHERE  ebeln        EQ  it_po-ebeln
  AND    ebelp        EQ  it_po-ebelp.

* settled amount(actual-l/c)
* settled amount(actual-b/l)
* settled amount(planned-lc)
* settled amount(planned-bl)
* Settled Quantity
*         it_tab-lc_set_ac  =  it_tab-lc_set_ac + it_set-zfsetlc.
*         it_tab-bl_set_ac  =  it_tab-bl_set_ac + it_set-zfsetbl.
*         it_tab-lc_set_pl  =  it_tab-lc_set_pl + it_set-zfpldlc.
*         it_tab-bl_set_pl  =  it_tab-bl_set_pl + it_set-zfpldbl.
*         w_set_qty         =  w_set_qty        + it_set-zfsetqty.


ENDFORM.                    " P4000_SETTELD_CST_SELECT
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_IT_TAB
*&---------------------------------------------------------------------*
FORM p3000_write_it_tab.

  REFRESH : it_tab.

  SORT : it_div  BY  ebeln ebelp cstgrp zfimdno,
         it_ekbz BY  ebeln ebelp belnr,
         it_set  BY  ebeln ebelp,
         it_po   BY  ebeln ebelp.

  LOOP  AT  it_po.

    CHECK it_po-ebeln IN s_ebeln AND it_po-ebelp IN s_ebelp.

    CLEAR : ekpo, w_imdno, w_belnr, it_tab, w_count, w_chk, w_set_qty.
    SELECT SINGLE * FROM ekpo
    WHERE  ebeln    EQ   it_po-ebeln
    AND    ebelp    EQ   it_po-ebelp.

    MOVE : it_po-ebeln   TO  it_tab-ebeln,
           it_po-ebelp   TO  it_tab-ebelp,
           ekpo-matnr    TO  it_tab-matnr,
           ekpo-knttp    TO  it_tab-knttp,
           ekpo-meins    TO  it_tab-meins.

    IF NOT ekpo-knttp IS INITIAL.
      CONTINUE.
    ENDIF.

    " Settled Complete Data Skip.
    SELECT SINGLE * FROM ztsetac
    WHERE  ebeln    EQ   it_tab-ebeln
    AND    ebelp    EQ   it_tab-ebelp
    AND    zfsetyn  EQ   'X'.
    IF sy-subrc EQ 0.
      CONTINUE.
    ENDIF.

    " Actual Amount Get.
    LOOP AT it_div WHERE ebeln EQ it_po-ebeln
                   AND   ebelp EQ it_po-ebelp.
      IF it_div-cstgrp EQ '003'.
        IF it_div-zfimdno  NE  w_imdno.
          ADD it_div-menge   TO  it_tab-lc_qty.
        ENDIF.
        MOVE : it_div-zfimdno TO  w_imdno,
               it_div-hwaer   TO  it_tab-waers.    " Currency
        ADD    it_div-dmbtr   TO  it_tab-lc_act.   " Amount.
      ENDIF.

      IF it_div-cstgrp NE '003'.
        IF it_div-cstgrp NE '006' .
          IF it_div-zfimdno NE w_imdno.
            ADD  it_div-menge  TO  it_tab-bl_qty.
          ENDIF.
          MOVE : it_div-zfimdno  TO  w_imdno,
                 it_div-hwaer    TO  it_tab-waers.
          ADD    it_div-dmbtr    TO  it_tab-bl_act.
        ELSE.
          ADD    it_div-dmbtr    TO  it_tab-bl_act.
        ENDIF.
      ENDIF.
      w_count  =  w_count  +  1.
    ENDLOOP.

    READ TABLE it_ekbz  WITH KEY ebeln = it_po-ebeln
                                 ebelp = it_po-ebelp
                                 kschl = 'ZOTI'.
    IF sy-subrc NE 0.
      w_chk  =  'X'.
    ENDIF.

    " Planned Amount.
    LOOP AT it_ekbz WHERE ebeln EQ it_po-ebeln
                    AND   ebelp EQ it_po-ebelp.

      " L/C Cost
      IF it_ekbz-kschl EQ 'ZOTH'.
        IF it_ekbz-shkzg EQ 'H'.
          it_tab-lc_pld  =  it_tab-lc_pld  +  it_ekbz-dmbtr.
        ELSE.
          it_tab-lc_pld  =  it_tab-lc_pld  -  it_ekbz-dmbtr.
        ENDIF.
      ENDIF.

      " B/L Cost
      IF it_ekbz-kschl EQ 'ZOTI'.
        IF it_ekbz-shkzg EQ 'H'.
          it_tab-bl_pld  =  it_tab-bl_pld  +  it_ekbz-dmbtr.
        ELSE.
          it_tab-bl_pld  =  it_tab-bl_pld  -  it_ekbz-dmbtr.
        ENDIF.
        IF w_belnr NE it_ekbz-belnr.
          IF it_ekbz-shkzg EQ 'H'.
            it_tab-gr_qty  =  it_ekbz-menge + it_tab-gr_qty.
          ELSE.
            it_tab-gr_qty  =  it_tab-gr_qty - it_ekbz-menge.
          ENDIF.
        ENDIF.
      ENDIF.
      " L/C Cost
      IF it_ekbz-kschl EQ 'ZOTH' AND w_chk EQ 'X'.
        IF w_belnr NE it_ekbz-belnr.
          IF it_ekbz-shkzg EQ 'H'.
            it_tab-gr_qty  =  it_ekbz-menge + it_tab-gr_qty.
          ELSE.
            it_tab-gr_qty  =  it_tab-gr_qty - it_ekbz-menge.
          ENDIF.
        ENDIF.
      ENDIF.
      w_count             =  w_count  +  1.
    ENDLOOP.

    " Settled Amount.
    LOOP AT it_set WHERE ebeln EQ it_po-ebeln
                   AND   ebelp EQ it_po-ebelp.
      it_tab-lc_set_ac  =  it_tab-lc_set_ac + it_set-zfsetlc.
      it_tab-bl_set_ac  =  it_tab-bl_set_ac + it_set-zfsetbl.
      it_tab-lc_set_pl  =  it_tab-lc_set_pl + it_set-zfpldlc.
      it_tab-bl_set_pl  =  it_tab-bl_set_pl + it_set-zfpldbl.
      w_set_qty         =  w_set_qty        + it_set-zfsetqty.
      w_count           =  w_count  +  1.
    ENDLOOP.

    " Difference Amount.
    it_tab-lc_cha  =  it_tab-lc_act  -  it_tab-lc_pld.
    it_tab-bl_cha  =  it_tab-bl_act  -  it_tab-bl_pld.

    " To Settled Amount.
    it_tab-to_qty      =  it_tab-gr_qty  -  w_set_qty.
    it_tab-lc_tset_pl  =  it_tab-lc_pld - it_tab-lc_set_pl.
    it_tab-bl_tset_pl  =  it_tab-bl_pld - it_tab-bl_set_pl.

    IF it_tab-to_qty LE 0.
      it_tab-to_qty = 0.
    ENDIF.

    IF ekpo-elikz EQ 'X' OR it_tab-lc_qty EQ it_tab-gr_qty.
      it_tab-lc_tset_ac = it_tab-lc_act - it_tab-lc_set_ac.
      it_tab-bl_tset_ac = it_tab-bl_act - it_tab-bl_set_ac.
      it_tab-check      = 'X'.
    ELSE.
      IF it_tab-gr_qty GT 0.
        IF it_tab-lc_qty EQ 0.
          it_tab-lc_tset_ac = 0.
        ELSE.
          it_tab-lc_tset_ac = ( it_tab-lc_act - it_tab-lc_set_ac )
                            * ( it_tab-gr_qty / it_tab-lc_qty ).
        ENDIF.
        IF it_tab-bl_qty EQ 0.
          it_tab-bl_tset_ac = 0.
        ELSE.
          it_tab-bl_tset_ac = ( it_tab-bl_act - it_tab-bl_set_ac )
                            * ( it_tab-gr_qty / it_tab-bl_qty ).
        ENDIF.
      ELSE.
        it_tab-lc_tset_ac  = 0.
        it_tab-bl_tset_ac  = 0.
      ENDIF.
    ENDIF.

    it_tab-lc_nset  =  it_tab-lc_act - it_tab-lc_set_ac
                                     - it_tab-lc_tset_ac.
    it_tab-bl_nset  =  it_tab-bl_act - it_tab-bl_set_ac
                                     - it_tab-bl_tset_ac.
    IF w_count LE 0. CONTINUE. ENDIF.

    IF it_tab-lc_act EQ 0 AND it_tab-bl_act EQ 0 AND
       it_tab-lc_pld EQ 0 AND it_tab-bl_pld EQ 0.
      CONTINUE.
    ENDIF.

    IF it_tab-waers IS INITIAL.
      MOVE  'USD'   TO  it_tab-waers.
    ENDIF.

    APPEND  it_tab.

  ENDLOOP.

ENDFORM.                    " P3000_WRITE_IT_TAB
*&---------------------------------------------------------------------*
*&      Form  P1000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM p1000_title_write.

  SKIP 2.
  WRITE:/100 '[Material-in-transit list to Settle an Account ]'
                            COLOR COL_HEADING INTENSIFIED OFF.
  SKIP 2.
  WRITE:/ 'Date:',sy-datum.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE:/ sy-uline.
  WRITE:/                                          sy-vline NO-GAP,
         (18) '        '                   NO-GAP, sy-vline NO-GAP,
         (20) '        '                   NO-GAP, sy-vline NO-GAP.
  SET LEFT SCROLL-BOUNDARY.
  WRITE: (05) '     '                      NO-GAP, sy-vline NO-GAP,
         (35) 'Quantity'          CENTERED NO-GAP, sy-vline NO-GAP,
         (05) '     '                      NO-GAP, sy-vline NO-GAP,
         (16) '        '                   NO-GAP, sy-vline NO-GAP,
         (16) '        '                   NO-GAP, sy-vline NO-GAP,
         (16) '        '                   NO-GAP, sy-vline NO-GAP,
         (33) 'Setteled Amount'   CENTERED NO-GAP, sy-vline NO-GAP,
         (33) 'To Settle Amount'  CENTERED NO-GAP, sy-vline NO-GAP,
         (16) ''                           NO-GAP, sy-vline .

  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE:/                                          sy-vline NO-GAP,
         (18) 'P/O-Item'                   NO-GAP, sy-vline NO-GAP,
         (20) 'Condition'                  NO-GAP, sy-vline NO-GAP.
  SET LEFT SCROLL-BOUNDARY.
  WRITE: (05) 'Unit'                       NO-GAP, sy-vline NO-GAP,
         (35)                     sy-uline NO-GAP, sy-vline NO-GAP,
         (05) 'Cur.'                       NO-GAP, sy-vline NO-GAP,
         (16) 'Actaul'                     NO-GAP, sy-vline NO-GAP,
         (16) 'Planned'                    NO-GAP, sy-vline NO-GAP,
         (16) 'Difference'                 NO-GAP, sy-vline NO-GAP,
         (33)                     sy-uline NO-GAP, sy-vline NO-GAP,
         (33)                     sy-uline NO-GAP, sy-vline NO-GAP,
         (16) 'Not Settle Amount' CENTERED NO-GAP, sy-vline .

  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE:/                                          sy-vline NO-GAP,
         (18) '     '                      NO-GAP, sy-vline NO-GAP,
         (20) '     '                      NO-GAP, sy-vline NO-GAP.
  SET LEFT SCROLL-BOUNDARY.
  WRITE: (05) '    '                       NO-GAP, sy-vline NO-GAP,
         (17) 'Qty'               CENTERED NO-GAP, sy-vline NO-GAP,
         (17) 'G/R Qty'           CENTERED NO-GAP, sy-vline NO-GAP,
         (05) '    '                       NO-GAP, sy-vline NO-GAP,
         (16) '     '                      NO-GAP, sy-vline NO-GAP,
         (16) '     '                      NO-GAP, sy-vline NO-GAP,
         (16) '     '                      NO-GAP, sy-vline NO-GAP,
         (16) 'Actual'            CENTERED NO-GAP, sy-vline NO-GAP,
         (16) 'Planned'           CENTERED NO-GAP, sy-vline NO-GAP,
         (16) 'Actual'            CENTERED NO-GAP, sy-vline NO-GAP,
         (16) 'Planned'           CENTERED NO-GAP, sy-vline NO-GAP,
         (16) '    '                       NO-GAP, sy-vline .

  WRITE:/ sy-uline.

ENDFORM.                    " P1000_TITLE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P2000_WRITE_TEXT
*&---------------------------------------------------------------------*
FORM p2000_write_text USING    p_w_err_chk.

  CLEAR : w_lc_text, w_bl_text, t685t.

  SELECT SINGLE * FROM t685t
  WHERE  spras    EQ   sy-langu
  AND    kschl    EQ   'ZOTH'.
  CONCATENATE  'ZOTH : ' t685t-vtext  INTO  w_lc_text.

  SELECT SINGLE * FROM t685t
  WHERE  spras    EQ   sy-langu
  AND    kschl    EQ   'ZOTI'.
  CONCATENATE  'ZOTI : ' t685t-vtext  INTO  w_bl_text.

  CLEAR : w_tot_actual, w_tot_pland,   w_tot_cha,   w_set_actual,
          w_set_pland,  w_tset_actual, w_tset_pland,w_nset.

  LOOP AT it_tab.

    PERFORM  p3000_line_write.

    " Actual, Planned Amount Total Get.
    w_tot_actual  =  it_tab-lc_act   +  it_tab-bl_act + w_tot_actual.
    w_tot_pland   =  it_tab-lc_pld   +  it_tab-bl_pld + w_tot_pland.
    w_tot_cha     =  it_tab-lc_cha   +  it_tab-bl_cha + w_tot_cha.
    w_set_actual  =  w_set_actual    +
                     it_tab-lc_set_ac  +  it_tab-bl_set_ac.
    w_set_pland   =  w_set_pland       +
                     it_tab-lc_set_pl  +  it_tab-bl_set_pl.
    w_tset_actual =  w_tset_actual     +
                     it_tab-lc_tset_ac +  it_tab-bl_tset_ac.
    w_tset_pland  =  w_tset_pland      +
                     it_tab-lc_tset_pl +  it_tab-bl_tset_pl.
    w_nset        =  it_tab-lc_nset    +  it_tab-bl_nset + w_nset .

  ENDLOOP.

  " Total Line Write.
  PERFORM  p4000_total_line_write.

ENDFORM.                    " P2000_WRITE_TEXT
*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM p3000_line_write.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.

  WRITE:/                                          sy-vline NO-GAP,
         (10) it_tab-ebeln                 NO-GAP,
         (03) ' - '                        NO-GAP,
         (05) it_tab-ebelp                 NO-GAP, sy-vline NO-GAP,
         (20) w_lc_text                    NO-GAP, sy-vline NO-GAP,
         (05) it_tab-meins                 NO-GAP, sy-vline NO-GAP,
         (17) it_tab-lc_qty     UNIT     it_tab-meins
                                           NO-GAP, sy-vline NO-GAP,
         (17) it_tab-gr_qty     UNIT     it_tab-meins
                                           NO-GAP, sy-vline NO-GAP,
         (05) it_tab-waers                 NO-GAP, sy-vline NO-GAP,
         (16) it_tab-lc_act     CURRENCY it_tab-waers
                                           NO-GAP, sy-vline NO-GAP,
         (16) it_tab-lc_pld     CURRENCY it_tab-waers
                                           NO-GAP, sy-vline NO-GAP,
         (16) it_tab-lc_cha     CURRENCY it_tab-waers
                                           NO-GAP, sy-vline NO-GAP,
         (16) it_tab-lc_set_ac  CURRENCY it_tab-waers
                                           NO-GAP, sy-vline NO-GAP,
         (16) it_tab-lc_set_pl  CURRENCY it_tab-waers
                                           NO-GAP, sy-vline NO-GAP,
         (16) it_tab-lc_tset_ac CURRENCY it_tab-waers
                                           NO-GAP, sy-vline NO-GAP,
         (16) it_tab-lc_tset_pl CURRENCY it_tab-waers
                                           NO-GAP, sy-vline NO-GAP,
         (16) it_tab-lc_nset    CURRENCY it_tab-waers
                                           NO-GAP, sy-vline NO-GAP.

  HIDE  it_tab.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.

  WRITE:/                                          sy-vline NO-GAP,
*         (10) '        '                   no-gap,
*         (01) ' '                          no-gap,
*         (05) '    '                       no-gap, sy-vline no-gap,
         (18) it_tab-matnr                 NO-GAP, sy-vline NO-GAP,
         (20) w_bl_text                    NO-GAP, sy-vline NO-GAP,
         (05) it_tab-meins                 NO-GAP, sy-vline NO-GAP,
         (17) it_tab-bl_qty     UNIT     it_tab-meins
                                           NO-GAP, sy-vline NO-GAP,
         (17) it_tab-gr_qty     UNIT     it_tab-meins
                                           NO-GAP, sy-vline NO-GAP,
         (05) it_tab-waers                 NO-GAP, sy-vline NO-GAP,
         (16) it_tab-bl_act     CURRENCY it_tab-waers
                                           NO-GAP, sy-vline NO-GAP,
         (16) it_tab-bl_pld     CURRENCY it_tab-waers
                                           NO-GAP, sy-vline NO-GAP,
         (16) it_tab-bl_cha     CURRENCY it_tab-waers
                                           NO-GAP, sy-vline NO-GAP,
         (16) it_tab-bl_set_ac  CURRENCY it_tab-waers
                                           NO-GAP, sy-vline NO-GAP,
         (16) it_tab-bl_set_pl  CURRENCY it_tab-waers
                                           NO-GAP, sy-vline NO-GAP,
         (16) it_tab-bl_tset_ac CURRENCY it_tab-waers
                                           NO-GAP, sy-vline NO-GAP,
         (16) it_tab-bl_tset_pl CURRENCY it_tab-waers
                                           NO-GAP, sy-vline NO-GAP,
         (16) it_tab-bl_nset    CURRENCY it_tab-waers
                                           NO-GAP, sy-vline NO-GAP.
  HIDE  it_tab.
  WRITE : sy-uline.

ENDFORM.                    " P3000_LINE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P5000_SETTLMENT_ACCOUNT_PROC
*&---------------------------------------------------------------------*
FORM p5000_settlment_account_proc.

  REFRESH : return.
  CLEAR   : return.

*>> NCW Insert 2004.01.14
  SELECT SINGLE * FROM ztimimg00.
  IF ztimimg00-zfbdcyn EQ 'X'.
    MOVE 'A' TO p_mode.
  ENDIF.

  ">> Mark of Allow Posting to Previous Period
  SELECT SINGLE * FROM marv
  WHERE  bukrs    EQ  p_bukrs.
  IF marv-xruem IS INITIAL.
    MESSAGE  i977 WITH
'Please check Indicator that allows posting to previous period'.
    EXIT.
  ENDIF.

*ANDY
  PERFORM bdc_open.

  LOOP AT it_tab.

    CLEAR : w_subrc.

    " IF Amount to settle is zero then skip.
    IF it_tab-lc_tset_ac EQ 0 AND it_tab-bl_tset_ac EQ 0 AND
       it_tab-lc_tset_pl EQ 0 AND it_tab-bl_tset_pl EQ 0.
      CONTINUE.
    ENDIF.

    IF it_tab-to_qty EQ 0.
      CONTINUE.
    ENDIF.

    " Reference No Get.
    CONCATENATE it_tab-ebeln '-' it_tab-ebelp INTO w_text.

    " Settlement Account.
    PERFORM  p6000_settle_account_get.

    " Posting Date Get.
    PERFORM  p6000_postdate_get.

    " L/C No OR B/L No Get.
    PERFORM  p6000_refrence_no_get.

    w_settle_amount = it_tab-lc_tset_ac + it_tab-bl_tset_ac -
                      it_tab-lc_tset_pl - it_tab-bl_tset_pl.


    "&--------------------------------------------------------"
    "& Actual Amount eq Planned Amount => FI Document Create. "
    "& Planned Amount | Actual Amount                         "
    "&--------------------------------------------------------"
    IF w_settle_amount EQ 0.
      PERFORM  p6000_zero_settlement_proc.

      "&--------------------------------------------------------"
      "& Acutal Amount  > Planned Amount => FI Doc, MR22(T-CODE)"
      "& Planned Amount  | Actual Amount                        "
      "& CO Account(UMB) |                                      "
      "&--------------------------------------------------------"
    ELSEIF w_settle_amount GT 0.
      PERFORM  p6000_plus_settlement_proc.

      "&--------------------------------------------------------"
      "& Acutal Amount  < Planned Amount => FI Doc, MR22(T-CODE)"
      "& Planned Amount  | Actual Amount                        "
      "&                 | CO Account(UMB)                      "
      "&--------------------------------------------------------"
    ELSE.
      PERFORM  p6000_minus_settlement_proc.
    ENDIF.
  ENDLOOP.

  IF p_sim = 'X'.
    CALL FUNCTION 'BDC_CLOSE_GROUP'.
  ENDIF.

  "&-------------------------------------------------------"
  "& Import Setteled Table Insert.                         "
  "&-------------------------------------------------------"
*TEMP FIX
*  check p_sim = ' '.
  PERFORM  p6000_db_insert.

ENDFORM.                    " P5000_SETTLMENT_ACCOUNT_PROC
*&---------------------------------------------------------------------*
*&      Form  P6000_SETTLE_ACCOUNT_GET
*&---------------------------------------------------------------------*
FORM p6000_settle_account_get.

  CLEAR : ekpo, w_act_acc, w_pld_acc, w_co_acc, w_pri_acc.
  SELECT SINGLE * FROM ekpo
  WHERE  ebeln    EQ   it_tab-ebeln
  AND    ebelp    EQ   it_tab-ebelp.

  " Actual Account
  PERFORM   p7000_get_account  USING ekpo-matnr
                                     ekpo-werks
                                     'ZR3'
                            CHANGING w_act_acc.

  " Planned Account
  PERFORM   p7000_get_account  USING ekpo-matnr
                                     ekpo-werks
                                     'FR3'
                            CHANGING w_pld_acc.

  " CO Account
  PERFORM   p7000_get_account  USING ekpo-matnr
                                     ekpo-werks
                                     'UMB'
                            CHANGING w_co_acc.

  " Price Difference Account
  w_pri_acc   =  ztimimg11-zfiocac32.

ENDFORM.                    " P6000_SETTLE_ACCOUNT_GET

*&---------------------------------------------------------------------*
*&      Form  P7000_GET_ACCOUNT
*&---------------------------------------------------------------------*
FORM p7000_get_account USING    p_matnr
                                p_werks
                                p_acc_key
                       CHANGING p_account.

  CLEAR : mbew, t001w, t001k, t030.

  " Valuation Class
  SELECT SINGLE * FROM mbew
  WHERE  matnr    EQ   p_matnr
  AND    bwkey    EQ   p_werks.

  " Valuation Modification Key
  SELECT SINGLE * FROM t001w
  WHERE  werks    EQ   p_werks.

  " Valuation Modification Group
  SELECT SINGLE * FROM t001k
  WHERE  bukrs    EQ   ztimimg11-bukrs
  AND    bwkey    EQ   t001w-bwkey.

  IF ztimimg11-zfvcyn EQ 'X'.
    IF ztimimg11-zfvmyn EQ 'X'.
      SELECT SINGLE *  FROM  t030
      WHERE  ktopl     EQ    ztimimg11-ktopl
      AND    ktosl     EQ    p_acc_key
      AND    bwmod     EQ    t001k-bwmod
      AND    komok     EQ    ztimimg11-komok
      AND    bklas     EQ    mbew-bklas.
    ELSE.
      SELECT SINGLE *  FROM  t030
      WHERE  ktopl     EQ     ztimimg11-ktopl
      AND    ktosl     EQ     p_acc_key
      AND    komok     EQ     ztimimg11-komok
      AND    bklas     EQ     mbew-bklas.
    ENDIF.
  ELSE.
    IF ztimimg11-zfvmyn EQ 'X'.
      SELECT SINGLE * FROM t030
      WHERE  ktopl    EQ   ztimimg11-ktopl
      AND    ktosl    EQ   p_acc_key
      AND    bwmod    EQ   t001k-bwmod
      AND    komok    EQ   ztimimg11-komok.
    ELSE.
      SELECT SINGLE * FROM t030
      WHERE  ktopl    EQ   ztimimg11-ktopl
      AND    ktosl    EQ   p_acc_key
      AND    komok    EQ   ztimimg11-komok.
    ENDIF.
  ENDIF.

  p_account  = t030-konts.

ENDFORM.                    " P7000_GET_ACCOUNT
*&---------------------------------------------------------------------*
*&      Form  P6000_ZERO_SETTLEMENT_PROC
*&---------------------------------------------------------------------*
FORM p6000_zero_settlement_proc.

  REFRESH : bdcdata.
  CLEAR   : w_belnr, w_gjahr, l_menge, l_meins.

  WRITE it_tab-to_qty     TO  l_menge UNIT  it_tab-meins.
  WRITE it_tab-meins      TO  l_meins.

  " First Screen.
  PERFORM p2000_dynpro USING :
          'X' 'SAPMF05A'    '0100',
          ' ' 'BKPF-BLDAT'  w_budat,
          ' ' 'BKPF-BUDAT'  w_budat,
          ' ' 'BKPF-BLART'  'SV',
          ' ' 'BKPF-XBLNR'  w_text,
          ' ' 'BKPF-BKTXT' 'IMPORT SETTLEMENT',
          ' ' 'BKPF-BUKRS'  p_bukrs,
          ' ' 'BKPF-WAERS'  it_tab-waers,
          ' ' 'RF05A-NEWBS' '40',
          ' ' 'RF05A-NEWKO' w_pld_acc,
          ' ' 'BDC_OKCODE'  '/00'.

  " Planned Amount.
  IF it_tab-lc_tset_pl GT 0 AND it_tab-bl_tset_pl GT 0.

    WRITE it_tab-lc_tset_pl TO  temp_wrbtr CURRENCY  it_tab-waers.
    PERFORM p2000_write_no_mask  CHANGING  temp_wrbtr.

    PERFORM p2000_dynpro USING :
            'X' 'SAPMF05A'    '0300',
            ' ' 'BSEG-WRBTR'  temp_wrbtr,
            ' ' 'BSEG-EBELN'  it_tab-ebeln,
            ' ' 'BSEG-EBELP'  it_tab-ebelp,
            ' ' 'BSEG-MENGE'  l_menge,
            ' ' 'BSEG-MEINS'  l_meins,
            ' ' 'BSEG-SGTXT'  it_tab-matnr,
            ' ' 'RF05A-NEWBS' '40',
            ' ' 'RF05A-NEWKO' w_pld_acc,
            ' ' 'BDC_OKCODE'  '/00'.

    PERFORM p2000_dynpro USING :
            'X' 'SAPLKACB' '0002',
            ' ' 'COBL-MATNR'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=ENTE'.

    WRITE it_tab-bl_tset_pl TO  temp_wrbtr CURRENCY  it_tab-waers.
    PERFORM p2000_write_no_mask  CHANGING  temp_wrbtr.
    PERFORM p2000_dynpro USING :
            'X' 'SAPMF05A'    '0300',
            ' ' 'BSEG-WRBTR'  temp_wrbtr,
            ' ' 'BSEG-EBELN'  it_tab-ebeln,
            ' ' 'BSEG-EBELP'  it_tab-ebelp,
            ' ' 'BSEG-MENGE'  l_menge,
            ' ' 'BSEG-MEINS'  l_meins,
            ' ' 'BSEG-SGTXT'  it_tab-matnr,
            ' ' 'RF05A-NEWBS' '50',
            ' ' 'RF05A-NEWKO' w_act_acc,
            ' ' 'BDC_OKCODE'  '/00'.

    PERFORM p2000_dynpro USING :
            'X' 'SAPLKACB' '0002',
            ' ' 'COBL-MATNR'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=ENTE'.

  ELSEIF it_tab-lc_tset_pl GT 0.
    WRITE it_tab-lc_tset_pl TO  temp_wrbtr CURRENCY  it_tab-waers.
    PERFORM p2000_write_no_mask  CHANGING  temp_wrbtr.

    PERFORM p2000_dynpro USING :
            'X' 'SAPMF05A'    '0300',
            ' ' 'BSEG-WRBTR'  temp_wrbtr,
            ' ' 'BSEG-EBELN'  it_tab-ebeln,
            ' ' 'BSEG-EBELP'  it_tab-ebelp,
            ' ' 'BSEG-MENGE'  l_menge,
            ' ' 'BSEG-MEINS'  l_meins,
            ' ' 'BSEG-SGTXT'  it_tab-matnr,
            ' ' 'RF05A-NEWBS' '50',
            ' ' 'RF05A-NEWKO' w_act_acc,
            ' ' 'BDC_OKCODE'  '/00'.

    PERFORM p2000_dynpro USING :
            'X' 'SAPLKACB' '0002',
            ' ' 'COBL-MATNR'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=ENTE'.

  ELSEIF it_tab-bl_tset_pl GT 0.

    WRITE it_tab-bl_tset_pl TO  temp_wrbtr CURRENCY  it_tab-waers.
    PERFORM p2000_write_no_mask  CHANGING  temp_wrbtr.
    PERFORM p2000_dynpro USING :
            'X' 'SAPMF05A'    '0300',
            ' ' 'BSEG-WRBTR'  temp_wrbtr,
            ' ' 'BSEG-EBELN'  it_tab-ebeln,
            ' ' 'BSEG-EBELP'  it_tab-ebelp,
            ' ' 'BSEG-MENGE'  l_menge,
            ' ' 'BSEG-MEINS'  l_meins,
            ' ' 'BSEG-SGTXT'  it_tab-matnr,
            ' ' 'RF05A-NEWBS' '50',
            ' ' 'RF05A-NEWKO' w_act_acc,
            ' ' 'BDC_OKCODE'  '/00'.

    PERFORM p2000_dynpro USING :
            'X' 'SAPLKACB' '0002',
            ' ' 'COBL-MATNR'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=ENTE'.
  ENDIF.

  " Actual Amount Setting.
  IF it_tab-lc_tset_ac GT 0 AND it_tab-bl_tset_ac GT 0.

    WRITE it_tab-lc_tset_ac TO  temp_wrbtr CURRENCY  it_tab-waers.
    PERFORM p2000_write_no_mask  CHANGING  temp_wrbtr.

    PERFORM p2000_dynpro USING :
            'X' 'SAPMF05A'    '0300',
            ' ' 'BSEG-WRBTR'  temp_wrbtr,
            ' ' 'BSEG-EBELN'  it_tab-ebeln,
            ' ' 'BSEG-EBELP'  it_tab-ebelp,
            ' ' 'BSEG-MENGE'  l_menge,
            ' ' 'BSEG-MEINS'  l_meins,
            ' ' 'BSEG-SGTXT'  it_tab-matnr,
            ' ' 'RF05A-NEWBS' '40',
            ' ' 'RF05A-NEWKO' w_act_acc,
            ' ' 'BDC_OKCODE'  '=ZK'.

    PERFORM p2000_dynpro USING :
            'X' 'SAPLKACB' '0002',
            ' ' 'COBL-MATNR'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=ENTE'.

    PERFORM p2000_dynpro USING :
            'X' 'SAPMF05A'    '0330',
            ' ' 'BSEG-XREF3'  w_lcno,
            ' ' 'BDC_OKCODE'  '/00'.

    WRITE it_tab-bl_tset_ac TO  temp_wrbtr CURRENCY  it_tab-waers.
    PERFORM p2000_write_no_mask  CHANGING  temp_wrbtr.

    PERFORM p2000_dynpro USING :
            'X' 'SAPMF05A'    '0300',
            ' ' 'BSEG-WRBTR'  temp_wrbtr,
            ' ' 'BSEG-EBELN'  it_tab-ebeln,
            ' ' 'BSEG-EBELP'  it_tab-ebelp,
            ' ' 'BSEG-MENGE'  l_menge,
            ' ' 'BSEG-MEINS'  l_meins,
            ' ' 'BSEG-SGTXT'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=ZK'.

    PERFORM p2000_dynpro USING :
            'X' 'SAPLKACB' '0002',
            ' ' 'COBL-MATNR'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=ENTE'.

    PERFORM p2000_dynpro USING :
            'X' 'SAPMF05A'    '0330',
            ' ' 'BSEG-XREF3'  w_blno,
            ' ' 'BDC_OKCODE'  '=BU'.

  ELSEIF it_tab-lc_tset_ac GT 0.
    WRITE it_tab-lc_tset_ac TO  temp_wrbtr CURRENCY  it_tab-waers.
    PERFORM p2000_write_no_mask  CHANGING  temp_wrbtr.

    PERFORM p2000_dynpro USING :
            'X' 'SAPMF05A'    '0300',
            ' ' 'BSEG-WRBTR'  temp_wrbtr,
            ' ' 'BSEG-EBELN'  it_tab-ebeln,
            ' ' 'BSEG-EBELP'  it_tab-ebelp,
            ' ' 'BSEG-MENGE'  l_menge,
            ' ' 'BSEG-MEINS'  l_meins,
            ' ' 'BSEG-SGTXT'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=ZK'.

    PERFORM p2000_dynpro USING :
            'X' 'SAPLKACB' '0002',
            ' ' 'COBL-MATNR'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=ENTE'.

    PERFORM p2000_dynpro USING :
            'X' 'SAPMF05A'    '0330',
            ' ' 'BSEG-XREF3'  w_lcno,
            ' ' 'BDC_OKCODE'  '=BU'.

  ELSEIF it_tab-bl_tset_ac GT 0.
    WRITE it_tab-bl_tset_ac TO  temp_wrbtr CURRENCY  it_tab-waers.
    PERFORM p2000_write_no_mask  CHANGING  temp_wrbtr.

    PERFORM p2000_dynpro USING :
            'X' 'SAPMF05A'    '0300',
            ' ' 'BSEG-WRBTR'  temp_wrbtr,
            ' ' 'BSEG-EBELN'  it_tab-ebeln,
            ' ' 'BSEG-EBELP'  it_tab-ebelp,
            ' ' 'BSEG-MENGE'  l_menge,
            ' ' 'BSEG-MEINS'  l_meins,
            ' ' 'BSEG-SGTXT'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=ZK'.

    PERFORM p2000_dynpro USING :
            'X' 'SAPLKACB' '0002',
            ' ' 'COBL-MATNR'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=ENTE'.

    PERFORM p2000_dynpro USING :
            'X' 'SAPMF05A'    '0330',
            ' ' 'BSEG-XREF3'  w_blno,
            ' ' 'BDC_OKCODE'  '=BU'.
  ENDIF.

  SET PARAMETER ID 'BLN' FIELD ''.        " Document No
  SET PARAMETER ID 'GJR' FIELD ''.        " Year

  PERFORM call_bdc  USING 'FB01'.

*    " BDC CALL.
*    CALL TRANSACTION 'FB01'  USING       bdcdata
*                             MODE        p_mode
*                             UPDATE      'S'
*                             MESSAGES    INTO   messtab.
*    w_subrc = sy-subrc.
*
*  IF w_subrc NE 0.      ">> ERROR .
*    LOOP AT messtab.
*      MOVE : messtab-msgtyp  TO     return-msgtyp,
*             messtab-msgid   TO     return-msgid,
*             messtab-msgnr   TO     return-msgnr,
*             messtab-msgv1   TO     return-msgv1,
*             messtab-msgv2   TO     return-msgv2,
*             messtab-msgv3   TO     return-msgv3,
*             messtab-msgv4   TO     return-msgv4.
*
*      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
*           EXPORTING
*                msgid               = return-msgid
*                msgnr               = return-msgnr
*                msgv1               = return-msgv1
*                msgv2               = return-msgv2
*                msgv3               = return-msgv3
*                msgv4               = return-msgv4
*           IMPORTING
*                message_text_output = return-messtxt.
*      APPEND  return.
*    ENDLOOP.
*    w_subrc = 4.
*  ELSE.
*    GET PARAMETER ID 'BLN' FIELD w_belnr.
*    GET PARAMETER ID 'GJR' FIELD w_gjahr.
*
*    IF w_belnr IS INITIAL.
*      w_subrc = 4.
*      MOVE : 'E'             TO     return-msgtyp,
*             'ZIM1'          TO     return-msgid,
*             '003'           TO     return-msgnr,
*             it_tab-ebeln    TO     return-msgv1,
*             it_tab-ebelp    TO     return-msgv2,
*             space           TO     return-msgv3,
*             space           TO     return-msgv4.
*
*      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
*           EXPORTING
*                msgid               = return-msgid
*                msgnr               = return-msgnr
*                msgv1               = return-msgv1
*                msgv2               = return-msgv2
*                msgv3               = return-msgv3
*                msgv4               = return-msgv4
*           IMPORTING
*                message_text_output = return-messtxt.
*      APPEND  return.
*    ELSE.
*      w_subrc = 0.
*      CLEAR : it_setac.
*      CONCATENATE  s_year-low  s_month-low  INTO  w_ym.
*
*      MOVE  : ekpo-ebeln         TO  it_setac-ebeln,
*              ekpo-ebelp         TO  it_setac-ebelp,
*              w_ym               TO  it_setac-zfsetym,
*              it_tab-lc_tset_ac  TO  it_setac-zfsetlc,
*              it_tab-bl_tset_ac  TO  it_setac-zfsetbl,
*              it_tab-lc_tset_pl  TO  it_setac-zfpldlc,
*              it_tab-bl_tset_pl  TO  it_setac-zfpldbl,
*              w_settle_amount    TO  it_setac-zfsetamt,
*              it_tab-waers       TO  it_setac-waers,
*              it_tab-to_qty      TO  it_setac-zfsetqty,
*              it_tab-meins       TO  it_setac-meins,
*              it_tab-check       TO  it_setac-zfsetyn.
*      APPEND  it_setac.
*    ENDIF.
*
*  ENDIF.
*<--------------------- FI Document End ------------------------------>*

ENDFORM.                    " P6000_ZERO_SETTLEMENT_PROC

*&---------------------------------------------------------------------*
*&      Form  P6000_PLUS_SETTLEMENT_PROC
*&---------------------------------------------------------------------*
FORM p6000_plus_settlement_proc.

  REFRESH : bdcdata.
  CLEAR   : w_belnr, w_gjahr, l_menge, l_meins.

  WRITE it_tab-to_qty     TO  l_menge UNIT  it_tab-meins.
  WRITE it_tab-meins      TO  l_meins.

*<----------------- FI Account Document Create ----------------------->*
  " First Screen.
  PERFORM p2000_dynpro USING :
          'X' 'SAPMF05A'    '0100',
          ' ' 'BKPF-BLDAT'  w_budat,
          ' ' 'BKPF-BUDAT'  w_budat,
          ' ' 'BKPF-BLART'  'SV',
          ' ' 'BKPF-XBLNR'  w_text,
          ' ' 'BKPF-BKTXT' 'IMPORT SETTLEMENT',
          ' ' 'BKPF-BUKRS'  p_bukrs,
          ' ' 'BKPF-WAERS'  it_tab-waers,
          ' ' 'RF05A-NEWBS' '40',
          ' ' 'RF05A-NEWKO' w_co_acc,
          ' ' 'BDC_OKCODE'  '/00'.

  WRITE w_settle_amount TO  temp_wrbtr CURRENCY  it_tab-waers.
  PERFORM p2000_write_no_mask  CHANGING  temp_wrbtr.

  PERFORM p2000_dynpro USING :
          'X' 'SAPMF05A'    '0300',
          ' ' 'BSEG-WRBTR'  temp_wrbtr,
          ' ' 'BSEG-EBELN'  it_tab-ebeln,
          ' ' 'BSEG-EBELP'  it_tab-ebelp,
          ' ' 'BDC_OKCODE'  '=ZK'.

  PERFORM p2000_dynpro USING :
          'X' 'SAPLKACB' '0002',
          ' ' 'COBL-MATNR'  it_tab-matnr,
          ' ' 'BDC_OKCODE'  '=ENTE'.

  PERFORM p2000_dynpro USING :
          'X' 'SAPMF05A'    '0330',
          ' ' 'BDC_OKCODE'  '/00'.

  " Planned Amount.
  IF it_tab-lc_tset_pl GT 0 AND it_tab-bl_tset_pl GT 0.

    PERFORM p2000_dynpro USING :
            ' ' 'RF05A-NEWBS' '40',
            ' ' 'RF05A-NEWKO' w_pld_acc,
            ' ' 'BDC_OKCODE'  '/00'.

    WRITE it_tab-lc_tset_pl TO  temp_wrbtr CURRENCY  it_tab-waers.
    PERFORM p2000_write_no_mask  CHANGING  temp_wrbtr.

    PERFORM p2000_dynpro USING :
            'X' 'SAPMF05A'    '0300',
            ' ' 'BSEG-WRBTR'  temp_wrbtr,
            ' ' 'BSEG-EBELN'  it_tab-ebeln,
            ' ' 'BSEG-EBELP'  it_tab-ebelp,
            ' ' 'BSEG-MENGE'  l_menge,
            ' ' 'BSEG-MEINS'  l_meins,
            ' ' 'BSEG-SGTXT'  it_tab-matnr,
            ' ' 'RF05A-NEWBS' '40',
            ' ' 'RF05A-NEWKO' w_pld_acc,
            ' ' 'BDC_OKCODE'  '/00'.

    PERFORM p2000_dynpro USING :
            'X' 'SAPLKACB' '0002',
            ' ' 'COBL-MATNR'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=ENTE'.

    WRITE it_tab-bl_tset_pl TO  temp_wrbtr CURRENCY  it_tab-waers.
    PERFORM p2000_write_no_mask  CHANGING  temp_wrbtr.

    PERFORM p2000_dynpro USING :
            'X' 'SAPMF05A'    '0300',
            ' ' 'BSEG-WRBTR'  temp_wrbtr,
            ' ' 'BSEG-EBELN'  it_tab-ebeln,
            ' ' 'BSEG-EBELP'  it_tab-ebelp,
            ' ' 'BSEG-MENGE'  l_menge,
            ' ' 'BSEG-MEINS'  l_meins,
            ' ' 'BSEG-SGTXT'  it_tab-matnr.

  ELSEIF it_tab-lc_tset_pl GT 0.

    PERFORM p2000_dynpro USING :
            ' ' 'RF05A-NEWBS' '40',
            ' ' 'RF05A-NEWKO' w_pld_acc,
            ' ' 'BDC_OKCODE'  '/00'.

    WRITE it_tab-lc_tset_pl TO  temp_wrbtr CURRENCY  it_tab-waers.
    PERFORM p2000_write_no_mask  CHANGING  temp_wrbtr.

    PERFORM p2000_dynpro USING :
            'X' 'SAPMF05A'    '0300',
            ' ' 'BSEG-WRBTR'  temp_wrbtr,
            ' ' 'BSEG-EBELN'  it_tab-ebeln,
            ' ' 'BSEG-EBELP'  it_tab-ebelp,
            ' ' 'BSEG-MENGE'  l_menge,
            ' ' 'BSEG-MEINS'  l_meins,
            ' ' 'BSEG-SGTXT'  it_tab-matnr.

  ELSEIF it_tab-bl_tset_pl GT 0.

    PERFORM p2000_dynpro USING :
            ' ' 'RF05A-NEWBS' '40',
            ' ' 'RF05A-NEWKO' w_pld_acc,
            ' ' 'BDC_OKCODE'  '/00'.

    WRITE it_tab-bl_tset_pl TO  temp_wrbtr CURRENCY  it_tab-waers.
    PERFORM p2000_write_no_mask  CHANGING  temp_wrbtr.

    PERFORM p2000_dynpro USING :
            'X' 'SAPMF05A'    '0300',
            ' ' 'BSEG-WRBTR'  temp_wrbtr,
            ' ' 'BSEG-EBELN'  it_tab-ebeln,
            ' ' 'BSEG-EBELP'  it_tab-ebelp,
            ' ' 'BSEG-MENGE'  l_menge,
            ' ' 'BSEG-MEINS'  l_meins,
            ' ' 'BSEG-SGTXT'  it_tab-matnr.
  ENDIF.

  " Actual Amount Setting.
  IF it_tab-lc_tset_ac GT 0 AND it_tab-bl_tset_ac GT 0.

    PERFORM p2000_dynpro USING :
            ' ' 'RF05A-NEWBS' '50',
            ' ' 'RF05A-NEWKO' w_act_acc,
            ' ' 'BDC_OKCODE'  '/00'.

    PERFORM p2000_dynpro USING :
            'X' 'SAPLKACB' '0002',
            ' ' 'COBL-MATNR'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=ENTE'.

    WRITE it_tab-lc_tset_ac TO  temp_wrbtr CURRENCY  it_tab-waers.
    PERFORM p2000_write_no_mask  CHANGING  temp_wrbtr.

    PERFORM p2000_dynpro USING :
            'X' 'SAPMF05A'    '0300',
            ' ' 'BSEG-WRBTR'  temp_wrbtr,
            ' ' 'BSEG-EBELN'  it_tab-ebeln,
            ' ' 'BSEG-EBELP'  it_tab-ebelp,
            ' ' 'BSEG-MENGE'  l_menge,
            ' ' 'BSEG-MEINS'  l_meins,
            ' ' 'BSEG-SGTXT'  it_tab-matnr,
            ' ' 'RF05A-NEWBS' '50',
            ' ' 'RF05A-NEWKO' w_act_acc,
            ' ' 'BDC_OKCODE'  '=ZK'.

    PERFORM p2000_dynpro USING :
            'X' 'SAPLKACB' '0002',
            ' ' 'COBL-MATNR'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=ENTE'.

    PERFORM p2000_dynpro USING :
            'X' 'SAPMF05A'    '0330',
            ' ' 'BSEG-XREF3'  w_lcno,
            ' ' 'BDC_OKCODE'  '/00'.

    WRITE it_tab-bl_tset_ac TO  temp_wrbtr CURRENCY  it_tab-waers.
    PERFORM p2000_write_no_mask  CHANGING  temp_wrbtr.

    PERFORM p2000_dynpro USING :
            'X' 'SAPMF05A'    '0300',
            ' ' 'BSEG-WRBTR'  temp_wrbtr,
            ' ' 'BSEG-EBELN'  it_tab-ebeln,
            ' ' 'BSEG-EBELP'  it_tab-ebelp,
            ' ' 'BSEG-MENGE'  l_menge,
            ' ' 'BSEG-MEINS'  l_meins,
            ' ' 'BSEG-SGTXT'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=ZK'.

    PERFORM p2000_dynpro USING :
            'X' 'SAPLKACB' '0002',
            ' ' 'COBL-MATNR'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=ENTE'.

    PERFORM p2000_dynpro USING :
            'X' 'SAPMF05A'    '0330',
            ' ' 'BSEG-XREF3'  w_blno,
            ' ' 'BDC_OKCODE'  '=BU'.

  ELSEIF it_tab-lc_tset_ac GT 0.

    PERFORM p2000_dynpro USING :
            ' ' 'RF05A-NEWBS' '50',
            ' ' 'RF05A-NEWKO' w_act_acc,
            ' ' 'BDC_OKCODE'  '/00'.

    PERFORM p2000_dynpro USING :
            'X' 'SAPLKACB' '0002',
            ' ' 'COBL-MATNR'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=ENTE'.

    WRITE it_tab-lc_tset_ac TO  temp_wrbtr CURRENCY  it_tab-waers.
    PERFORM p2000_write_no_mask  CHANGING  temp_wrbtr.

    PERFORM p2000_dynpro USING :
            'X' 'SAPMF05A'    '0300',
            ' ' 'BSEG-WRBTR'  temp_wrbtr,
            ' ' 'BSEG-EBELN'  it_tab-ebeln,
            ' ' 'BSEG-EBELP'  it_tab-ebelp,
            ' ' 'BSEG-MENGE'  l_menge,
            ' ' 'BSEG-MEINS'  l_meins,
            ' ' 'BSEG-SGTXT'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=ZK'.

    PERFORM p2000_dynpro USING :
            'X' 'SAPLKACB' '0002',
            ' ' 'COBL-MATNR'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=ENTE'.

    PERFORM p2000_dynpro USING :
            'X' 'SAPMF05A'    '0330',
            ' ' 'BSEG-XREF3'  w_lcno,
            ' ' 'BDC_OKCODE'  '=BU'.

  ELSEIF it_tab-bl_tset_ac GT 0.

    PERFORM p2000_dynpro USING :
            ' ' 'RF05A-NEWBS' '50',
            ' ' 'RF05A-NEWKO' w_act_acc,
            ' ' 'BDC_OKCODE'  '/00'.

    PERFORM p2000_dynpro USING :
            'X' 'SAPLKACB' '0002',
            ' ' 'COBL-MATNR'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=ENTE'.

    WRITE it_tab-bl_tset_ac TO  temp_wrbtr CURRENCY  it_tab-waers.
    PERFORM p2000_write_no_mask  CHANGING  temp_wrbtr.

    PERFORM p2000_dynpro USING :
            'X' 'SAPMF05A'    '0300',
            ' ' 'BSEG-WRBTR'  temp_wrbtr,
            ' ' 'BSEG-EBELN'  it_tab-ebeln,
            ' ' 'BSEG-EBELP'  it_tab-ebelp,
            ' ' 'BSEG-MENGE'  l_menge,
            ' ' 'BSEG-MEINS'  l_meins,
            ' ' 'BSEG-SGTXT'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=ZK'.

    PERFORM p2000_dynpro USING :
            'X' 'SAPLKACB' '0002',
            ' ' 'COBL-MATNR'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=ENTE'.

    PERFORM p2000_dynpro USING :
            'X' 'SAPMF05A'    '0330',
            ' ' 'BSEG-XREF3'  w_blno,
            ' ' 'BDC_OKCODE'  '=BU'.
  ENDIF.

  SET PARAMETER ID 'BLN' FIELD ''.        " Document No
  SET PARAMETER ID 'GJR' FIELD ''.        " Year

  PERFORM call_bdc  USING 'FB01'.

*    " BDC CALL.
*    CALL TRANSACTION 'FB01'  USING       bdcdata
*                             MODE        p_mode
*                             UPDATE      'S'
*                             MESSAGES    INTO   messtab.
*  w_subrc = sy-subrc.
*  IF w_subrc NE 0.      ">> ERROR
*    LOOP AT messtab.
*      MOVE : messtab-msgtyp  TO     return-msgtyp,
*             messtab-msgid   TO     return-msgid,
*             messtab-msgnr   TO     return-msgnr,
*             messtab-msgv1   TO     return-msgv1,
*             messtab-msgv2   TO     return-msgv2,
*             messtab-msgv3   TO     return-msgv3,
*             messtab-msgv4   TO     return-msgv4.
*
*      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
*           EXPORTING
*                msgid               = return-msgid
*                msgnr               = return-msgnr
*                msgv1               = return-msgv1
*                msgv2               = return-msgv2
*                msgv3               = return-msgv3
*                msgv4               = return-msgv4
*           IMPORTING
*                message_text_output = return-messtxt.
*      APPEND  return.
*    ENDLOOP.
*    w_subrc = 4.
*  ELSE.
*    GET PARAMETER ID 'BLN' FIELD w_belnr.
*    GET PARAMETER ID 'GJR' FIELD w_gjahr.
*
*    IF w_belnr IS INITIAL.
*      w_subrc = 4.
*      MOVE : 'E'             TO     return-msgtyp,
*             'ZIM1'          TO     return-msgid,
*             '003'           TO     return-msgnr,
*             it_tab-ebeln    TO     return-msgv1,
*             it_tab-ebelp    TO     return-msgv2,
*             space           TO     return-msgv3,
*             space           TO     return-msgv4.
*
*      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
*           EXPORTING
*                msgid               = return-msgid
*                msgnr               = return-msgnr
*                msgv1               = return-msgv1
*                msgv2               = return-msgv2
*                msgv3               = return-msgv3
*                msgv4               = return-msgv4
*           IMPORTING
*                message_text_output = return-messtxt.
*      APPEND  return.
*    ELSE.
*      w_subrc = 0.
*    ENDIF.
*  ENDIF.
*<--------------------- FI Document End ------------------------------>*

*<--------------------- MM Document Create --------------------------->*
  IF w_subrc NE 0.  EXIT.  ENDIF.

  PERFORM p7000_mm_call  USING  w_settle_amount
                                ekpo-matnr
                                w_budat.
  IF w_subrc EQ 0.
    CLEAR : it_setac.
    CONCATENATE  p_year  p_month  INTO  w_ym.
    MOVE  : ekpo-ebeln         TO  it_setac-ebeln,
            ekpo-ebelp         TO  it_setac-ebelp,
            w_ym               TO  it_setac-zfsetym,
            it_tab-lc_tset_ac  TO  it_setac-zfsetlc,
            it_tab-bl_tset_ac  TO  it_setac-zfsetbl,
            it_tab-lc_tset_pl  TO  it_setac-zfpldlc,
            it_tab-bl_tset_pl  TO  it_setac-zfpldbl,
            w_settle_amount    TO  it_setac-zfsetamt,
            it_tab-waers       TO  it_setac-waers,
            it_tab-to_qty      TO  it_setac-zfsetqty,
            it_tab-meins       TO  it_setac-meins,
            it_tab-check       TO  it_setac-zfsetyn.
    APPEND  it_setac.
  ENDIF.

ENDFORM.                    " P6000_PLUS_SETTLEMENT_PROC

*&---------------------------------------------------------------------*
*&      Form  P6000_MINUS_SETTLEMENT_PROC
*&---------------------------------------------------------------------*
FORM p6000_minus_settlement_proc.

  CLEAR : mbew, t001w.
  SELECT SINGLE * FROM t001w
  WHERE  werks    EQ   ekpo-werks.

  SELECT SINGLE * FROM mbew
  WHERE  matnr    EQ   ekpo-matnr
  AND    bwkey    EQ   t001w-bwkey
  AND    bwtar    EQ   ekpo-bwtar.

*<--------------------- FI Document Create --------------------------->*
  IF mbew-vprsv EQ 'V'.
    w_settle_amount =  w_settle_amount * -1.
    w_minus_amount  =  w_settle_amount - mbew-salk3.
  ELSEIF mbew-vprsv EQ 'S'.
    w_settle_amount =  w_settle_amount * -1.
    w_minus_amount  =  0.
  ENDIF.

  IF w_minus_amount GT 0.
    w_minus_amount =  mbew-salk3.
    PERFORM  p7000_price_difference_proc.
  ELSE.
    w_minus_amount  =  w_settle_amount.
    PERFORM  p7000_minus_co_proc.
  ENDIF.

*<--------------------- MM Document Create --------------------------->*
  IF w_subrc NE 0.  EXIT.  ENDIF.

  w_minus_amount  =  w_minus_amount * -1.
  IF w_minus_amount NE 0.
    PERFORM p7000_mm_call  USING  w_minus_amount
                                  ekpo-matnr
                                  w_budat.
  ENDIF.

  ">> Internal Table Insert!
  IF w_subrc EQ 0.
    CLEAR : it_setac.
    CONCATENATE  p_year  p_month  INTO  w_ym.
    MOVE  : ekpo-ebeln         TO  it_setac-ebeln,
            ekpo-ebelp         TO  it_setac-ebelp,
            w_ym               TO  it_setac-zfsetym,
            it_tab-lc_tset_ac  TO  it_setac-zfsetlc,
            it_tab-bl_tset_ac  TO  it_setac-zfsetbl,
            it_tab-lc_tset_pl  TO  it_setac-zfpldlc,
            it_tab-bl_tset_pl  TO  it_setac-zfpldbl,
            w_settle_amount    TO  it_setac-zfsetamt,
            it_tab-waers       TO  it_setac-waers,
            it_tab-to_qty      TO  it_setac-zfsetqty,
            it_tab-meins       TO  it_setac-meins,
            it_tab-check       TO  it_setac-zfsetyn.
    APPEND  it_setac.
  ENDIF.

ENDFORM.                    " P6000_MINUS_SETTLEMENT_PROC

*&---------------------------------------------------------------------*
*&      Form  P6000_POSTDATE_GET
*&---------------------------------------------------------------------*
FORM p6000_postdate_get.

  CASE  p_month.
    WHEN '01' OR '03' OR '05' OR '07' OR '08' OR '10' OR '12'.
      w_day  =  '31'.
    WHEN '04' OR '06' OR '09' OR '11'.
      w_day  =  '30'.
    WHEN OTHERS.
      w_day  =  '28'.
  ENDCASE.
  CONCATENATE  p_year  p_month  w_day INTO w_char_date.
  MOVE  w_char_date   TO  w_budat.

  CALL FUNCTION 'ZIM_BDC_DATE_CONVERT_EXTERNAL'
       EXPORTING
            i_date = w_budat
       IMPORTING
            e_date = w_budat
       EXCEPTIONS
            OTHERS = 4.


ENDFORM.                    " P6000_POSTDATE_GET
*&---------------------------------------------------------------------*
*&      Form  P6000_REFRENCE_NO_GET
*&---------------------------------------------------------------------*
FORM p6000_refrence_no_get.

  CLEAR : w_lcno, w_blno.

  " L/C No Get.
  SELECT SINGLE * FROM ztreqhd WHERE ebeln EQ it_tab-ebeln.
  MOVE  ztreqhd-zfopnno  TO  w_lcno.
  IF w_lcno IS INITIAL.
    MOVE  'N/A'       TO  w_lcno.
  ENDIF.

  " B/L No Get.
  SELECT * FROM ztblit UP TO 1 ROWS
  WHERE    ebeln       EQ    it_tab-ebeln
  AND      ebelp       EQ    it_tab-ebelp
  ORDER BY zfblno DESCENDING.

    SELECT SINGLE * FROM ztbl
    WHERE  zfblno   EQ   ztblit-zfblno.
    MOVE  ztbl-zfhblno   TO  w_blno.

  ENDSELECT.

ENDFORM.                    " P6000_REFRENCE_NO_GET
*&---------------------------------------------------------------------*
*&      Form  P2000_WRITE_NO_MASK
*&---------------------------------------------------------------------*
FORM p2000_write_no_mask CHANGING p_text_amount.

  SELECT SINGLE * FROM usr01 WHERE bname EQ sy-uname.

  CASE usr01-dcpfm.
    WHEN 'X'.    " Decimal point is period: N,NNN.NN
      PERFORM    p2000_change_symbol    USING p_text_amount ',' ' '.
      CONDENSE         p_text_amount    NO-GAPS.
    WHEN 'Y'.    " Decimal point is N NNN NNN,NN
      PERFORM    p2000_change_symbol    USING p_text_amount  ',' '.'.
      CONDENSE         p_text_amount    NO-GAPS.
    WHEN OTHERS. " Decimal point is comma: N.NNN,NN
      PERFORM    p2000_change_symbol    USING p_text_amount  '.' ' '.
      PERFORM    p2000_change_symbol    USING p_text_amount  ',' '.'.
      CONDENSE         p_text_amount    NO-GAPS.
  ENDCASE.

ENDFORM.                    " P2000_WRITE_NO_MASK

*&---------------------------------------------------------------------*
*&      Form  P2000_CHANGE_SYMBOL
*&---------------------------------------------------------------------*
FORM p2000_change_symbol USING    p_amount  p_from  p_to.

  DO.
    REPLACE  p_from   WITH   p_to  INTO    p_amount.
    IF  sy-subrc  <>    0.
      EXIT.
    ENDIF.
  ENDDO.

ENDFORM.                    " P2000_CHANGE_SYMBOL
*&---------------------------------------------------------------------*
*&      Form  P7000_MM_CALL
*&---------------------------------------------------------------------*
FORM p7000_mm_call USING    p_amount
                            p_matnr
                            p_budat.
* post FI & ML
  CHECK p_fi = space.

  REFRESH : bdcdata.

  WRITE p_amount TO  temp_wrbtr CURRENCY  it_tab-waers.
  PERFORM    p2000_write_no_mask     CHANGING  temp_wrbtr.

  PERFORM p2000_dynpro USING :
          'X' 'SAPRCKM_MR22'           '0201',
          ' ' 'MR21HEAD-BUDAT'         p_budat,
          ' ' 'MR21HEAD-BUKRS'         p_bukrs,
          ' ' 'MR21HEAD-WERKS'         ekpo-werks,
          ' ' 'MR21HEAD-XBLNR'         w_text,
          ' ' 'MR21HEAD-BKTXT'         'IMPORT SETTLEMENT',
          ' ' 'BDC_OKCODE'             '=ENTR'.

  PERFORM p2000_dynpro USING :
          'X' 'SAPRCKM_MR22'          '0201',
       ' ' 'MR21HEAD-SCREEN_VARIANT'  'LAGERMATERIAL - OHNE BWKEY_025',
       ' ' 'CKI_MR22_0250-MATNR(01)'  p_matnr,
       ' ' 'CKI_MR22_0250-ZUUMB(01)'  temp_wrbtr,
       ' ' 'BDC_OKCODE'               '=ENTR'.

  PERFORM p2000_dynpro USING :
          'X' 'SAPRCKM_MR22'           '0201',
       ' ' 'BDC_OKCODE'                '=SAVE'.

  SET PARAMETER ID 'MLN' FIELD ''.        " Document No
  SET PARAMETER ID 'MLJ' FIELD ''.        " Document Year

  PERFORM call_bdc USING 'MR22'.


ENDFORM.                    " P7000_MM_CALL
*&---------------------------------------------------------------------*
*&      Form  P7000_MINUS_CO_PROC
*&---------------------------------------------------------------------*
FORM p7000_minus_co_proc.

  REFRESH : bdcdata.
  CLEAR   : w_belnr, w_gjahr.

*<----------------- FI Account Document Create ----------------------->*
  WRITE it_tab-to_qty     TO  l_menge UNIT  it_tab-meins.
  WRITE it_tab-meins      TO  l_meins.

  " First Screen.
  PERFORM p2000_dynpro USING :
          'X' 'SAPMF05A'    '0100',
          ' ' 'BKPF-BLDAT'  w_budat,
          ' ' 'BKPF-BUDAT'  w_budat,
          ' ' 'BKPF-BLART'  'SV',
          ' ' 'BKPF-BUKRS'  p_bukrs,
          ' ' 'BKPF-WAERS'  it_tab-waers,
          ' ' 'BKPF-XBLNR'  w_text,
          ' ' 'BKPF-BKTXT' 'IMPORT SETTLEMENT',
          ' ' 'RF05A-NEWBS' '50',
          ' ' 'RF05A-NEWKO' w_co_acc,
          ' ' 'BDC_OKCODE'  '/00'.

  WRITE w_settle_amount TO  temp_wrbtr CURRENCY  it_tab-waers.
  PERFORM p2000_write_no_mask  CHANGING  temp_wrbtr.

  PERFORM p2000_dynpro USING :
          'X' 'SAPMF05A'    '0300',
          ' ' 'BSEG-WRBTR'  temp_wrbtr,
          ' ' 'BSEG-EBELN'  it_tab-ebeln,
          ' ' 'BSEG-EBELP'  it_tab-ebelp,
          ' ' 'BDC_OKCODE'  '=ZK'.

  PERFORM p2000_dynpro USING :
          'X' 'SAPLKACB'    '0002',
          ' ' 'COBL-MATNR'  it_tab-matnr,
          ' ' 'BDC_OKCODE'  '=ENTE'.

  PERFORM p2000_dynpro USING :
          'X' 'SAPMF05A'    '0330',
          ' ' 'BDC_OKCODE'  '/00'.

  " Actual Amount Setting.
  IF it_tab-lc_tset_ac GT 0 AND it_tab-bl_tset_ac GT 0.

    PERFORM p2000_dynpro USING :
            ' ' 'RF05A-NEWBS' '50',
            ' ' 'RF05A-NEWKO' w_act_acc,
            ' ' 'BDC_OKCODE'  '/00'.

    WRITE it_tab-lc_tset_ac TO  temp_wrbtr CURRENCY  it_tab-waers.
    PERFORM p2000_write_no_mask  CHANGING  temp_wrbtr.

    PERFORM p2000_dynpro USING :
            'X' 'SAPMF05A'    '0300',
            ' ' 'BSEG-WRBTR'  temp_wrbtr,
            ' ' 'BSEG-EBELN'  it_tab-ebeln,
            ' ' 'BSEG-EBELP'  it_tab-ebelp,
            ' ' 'BSEG-MENGE'  l_menge,
            ' ' 'BSEG-MEINS'  l_meins,
            ' ' 'BSEG-SGTXT'  it_tab-matnr,
            ' ' 'RF05A-NEWBS' '50',
            ' ' 'RF05A-NEWKO' w_act_acc,
            ' ' 'BDC_OKCODE'  '=ZK'.

    PERFORM p2000_dynpro USING :
            'X' 'SAPLKACB' '0002',
            ' ' 'COBL-MATNR'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=ENTE'.

    PERFORM p2000_dynpro USING :
            'X' 'SAPMF05A'    '0330',
            ' ' 'BSEG-XREF3'  w_lcno,
            ' ' 'BDC_OKCODE'  '/00'.

    WRITE it_tab-bl_tset_ac TO  temp_wrbtr CURRENCY  it_tab-waers.
    PERFORM p2000_write_no_mask  CHANGING  temp_wrbtr.

    PERFORM p2000_dynpro USING :
            'X' 'SAPMF05A'    '0300',
            ' ' 'BSEG-WRBTR'  temp_wrbtr,
            ' ' 'BSEG-EBELN'  it_tab-ebeln,
            ' ' 'BSEG-EBELP'  it_tab-ebelp,
            ' ' 'BSEG-MENGE'  l_menge,
            ' ' 'BSEG-MEINS'  l_meins,
            ' ' 'BSEG-SGTXT'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=ZK'.

    PERFORM p2000_dynpro USING :
            'X' 'SAPLKACB' '0002',
            ' ' 'COBL-MATNR'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=ENTE'.

    PERFORM p2000_dynpro USING :
            'X' 'SAPMF05A'    '0330',
            ' ' 'BSEG-XREF3'  w_blno,
            ' ' 'BDC_OKCODE'  '/00'.

  ELSEIF it_tab-lc_tset_ac GT 0.

    PERFORM p2000_dynpro USING :
            ' ' 'RF05A-NEWBS' '50',
            ' ' 'RF05A-NEWKO' w_act_acc,
            ' ' 'BDC_OKCODE'  '/00'.

    WRITE it_tab-lc_tset_ac TO  temp_wrbtr CURRENCY  it_tab-waers.
    PERFORM p2000_write_no_mask  CHANGING  temp_wrbtr.

    PERFORM p2000_dynpro USING :
            'X' 'SAPMF05A'    '0300',
            ' ' 'BSEG-WRBTR'  temp_wrbtr,
            ' ' 'BSEG-EBELN'  it_tab-ebeln,
            ' ' 'BSEG-EBELP'  it_tab-ebelp,
            ' ' 'BSEG-MENGE'  l_menge,
            ' ' 'BSEG-MEINS'  l_meins,
            ' ' 'BSEG-SGTXT'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=ZK'.

    PERFORM p2000_dynpro USING :
            'X' 'SAPLKACB' '0002',
            ' ' 'COBL-MATNR'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=ENTE'.

    PERFORM p2000_dynpro USING :
            'X' 'SAPMF05A'    '0330',
            ' ' 'BSEG-XREF3'  w_lcno,
            ' ' 'BDC_OKCODE'  '/00'.

  ELSEIF it_tab-bl_tset_ac GT 0.

    PERFORM p2000_dynpro USING :
            ' ' 'RF05A-NEWBS' '50',
            ' ' 'RF05A-NEWKO' w_act_acc,
            ' ' 'BDC_OKCODE'  '/00'.

    WRITE it_tab-bl_tset_ac TO  temp_wrbtr CURRENCY  it_tab-waers.
    PERFORM p2000_write_no_mask  CHANGING  temp_wrbtr.

    PERFORM p2000_dynpro USING :
            'X' 'SAPMF05A'    '0300',
            ' ' 'BSEG-WRBTR'  temp_wrbtr,
            ' ' 'BSEG-EBELN'  it_tab-ebeln,
            ' ' 'BSEG-EBELP'  it_tab-ebelp,
            ' ' 'BSEG-MENGE'  l_menge,
            ' ' 'BSEG-MEINS'  l_meins,
            ' ' 'BSEG-SGTXT'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=ZK'.

    PERFORM p2000_dynpro USING :
            'X' 'SAPLKACB' '0002',
            ' ' 'COBL-MATNR'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=ENTE'.

    PERFORM p2000_dynpro USING :
            'X' 'SAPMF05A'    '0330',
            ' ' 'BSEG-XREF3'  w_blno,
            ' ' 'BDC_OKCODE'  '/00'.
  ENDIF.

  " Planned Amount.
  IF it_tab-lc_tset_pl GT 0 AND it_tab-bl_tset_pl GT 0.

    PERFORM p2000_dynpro USING :
            ' ' 'RF05A-NEWBS' '40',
            ' ' 'RF05A-NEWKO' w_pld_acc,
            ' ' 'BDC_OKCODE'  '/00'.

    WRITE it_tab-lc_tset_pl TO  temp_wrbtr CURRENCY  it_tab-waers.
    PERFORM p2000_write_no_mask  CHANGING  temp_wrbtr.

    PERFORM p2000_dynpro USING :
            'X' 'SAPMF05A'    '0300',
            ' ' 'BSEG-WRBTR'  temp_wrbtr,
            ' ' 'BSEG-EBELN'  it_tab-ebeln,
            ' ' 'BSEG-EBELP'  it_tab-ebelp,
            ' ' 'BSEG-MENGE'  l_menge,
            ' ' 'BSEG-MEINS'  l_meins,
            ' ' 'BSEG-SGTXT'  it_tab-matnr,
            ' ' 'RF05A-NEWBS' '40',
            ' ' 'RF05A-NEWKO' w_pld_acc,
            ' ' 'BDC_OKCODE'  '/00'.

    PERFORM p2000_dynpro USING :
            'X' 'SAPLKACB' '0002',
            ' ' 'COBL-MATNR'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=ENTE'.

    WRITE it_tab-bl_tset_pl TO  temp_wrbtr CURRENCY  it_tab-waers.
    PERFORM p2000_write_no_mask  CHANGING  temp_wrbtr.

    PERFORM p2000_dynpro USING :
            'X' 'SAPMF05A'    '0300',
            ' ' 'BSEG-WRBTR'  temp_wrbtr,
            ' ' 'BSEG-EBELN'  it_tab-ebeln,
            ' ' 'BSEG-EBELP'  it_tab-ebelp,
            ' ' 'BSEG-MENGE'  l_menge,
            ' ' 'BSEG-MEINS'  l_meins,
            ' ' 'BSEG-SGTXT'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=BU'.

    PERFORM p2000_dynpro USING :
            'X' 'SAPLKACB' '0002',
            ' ' 'COBL-MATNR'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=ENTE'.

  ELSEIF it_tab-lc_tset_pl GT 0.

    PERFORM p2000_dynpro USING :
            ' ' 'RF05A-NEWBS' '40',
            ' ' 'RF05A-NEWKO' w_pld_acc,
            ' ' 'BDC_OKCODE'  '/00'.

    WRITE it_tab-lc_tset_pl TO  temp_wrbtr CURRENCY  it_tab-waers.
    PERFORM p2000_write_no_mask  CHANGING  temp_wrbtr.

    PERFORM p2000_dynpro USING :
            'X' 'SAPMF05A'    '0300',
            ' ' 'BSEG-WRBTR'  temp_wrbtr,
            ' ' 'BSEG-EBELN'  it_tab-ebeln,
            ' ' 'BSEG-EBELP'  it_tab-ebelp,
            ' ' 'BSEG-MENGE'  l_menge,
            ' ' 'BSEG-MEINS'  l_meins,
            ' ' 'BSEG-SGTXT'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=BU'.

    PERFORM p2000_dynpro USING :
            'X' 'SAPLKACB' '0002',
            ' ' 'COBL-MATNR'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=ENTE'.

  ELSEIF it_tab-bl_tset_pl GT 0.

    PERFORM p2000_dynpro USING :
            ' ' 'RF05A-NEWBS' '40',
            ' ' 'RF05A-NEWKO' w_pld_acc,
            ' ' 'BDC_OKCODE'  '/00'.

    WRITE it_tab-bl_tset_pl TO  temp_wrbtr CURRENCY  it_tab-waers.
    PERFORM p2000_write_no_mask  CHANGING  temp_wrbtr.

    PERFORM p2000_dynpro USING :
            'X' 'SAPMF05A'    '0300',
            ' ' 'BSEG-WRBTR'  temp_wrbtr,
            ' ' 'BSEG-EBELN'  it_tab-ebeln,
            ' ' 'BSEG-EBELP'  it_tab-ebelp,
            ' ' 'BSEG-MENGE'  l_menge,
            ' ' 'BSEG-MEINS'  l_meins,
            ' ' 'BSEG-SGTXT'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=BU'.

    PERFORM p2000_dynpro USING :
            'X' 'SAPLKACB' '0002',
            ' ' 'COBL-MATNR'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=ENTE'.

  ENDIF.

  SET PARAMETER ID 'BLN' FIELD ''.        " Document No
  SET PARAMETER ID 'GJR' FIELD ''.        " Year

  PERFORM call_bdc USING 'FB01'.

*    " BDC CALL.
*    CALL TRANSACTION 'FB01'  USING       bdcdata
*                             MODE        p_mode
*                             UPDATE      'S'
*                             MESSAGES    INTO   messtab.
*  ENDIF.
*
*  w_subrc = sy-subrc.
*
*  IF w_subrc NE 0.      ">> ERROR
*    LOOP AT messtab.
*      MOVE : messtab-msgtyp  TO     return-msgtyp,
*             messtab-msgid   TO     return-msgid,
*             messtab-msgnr   TO     return-msgnr,
*             messtab-msgv1   TO     return-msgv1,
*             messtab-msgv2   TO     return-msgv2,
*             messtab-msgv3   TO     return-msgv3,
*             messtab-msgv4   TO     return-msgv4.
*
*      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
*           EXPORTING
*                msgid               = return-msgid
*                msgnr               = return-msgnr
*                msgv1               = return-msgv1
*                msgv2               = return-msgv2
*                msgv3               = return-msgv3
*                msgv4               = return-msgv4
*           IMPORTING
*                message_text_output = return-messtxt.
*      APPEND  return.
*    ENDLOOP.
*    w_subrc = 4.
*  ELSE.
*    GET PARAMETER ID 'BLN' FIELD w_belnr.
*    GET PARAMETER ID 'GJR' FIELD w_gjahr.
*
*    IF w_belnr IS INITIAL.
*      w_subrc = 4.
*      MESSAGE s648.
*      MOVE : 'E'             TO     return-msgtyp,
*             'ZIM1'          TO     return-msgid,
*             '003'           TO     return-msgnr,
*             it_tab-ebeln    TO     return-msgv1,
*             it_tab-ebelp    TO     return-msgv2,
*             space           TO     return-msgv3,
*             space           TO     return-msgv4.
*
*      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
*           EXPORTING
*                msgid               = return-msgid
*                msgnr               = return-msgnr
*                msgv1               = return-msgv1
*                msgv2               = return-msgv2
*                msgv3               = return-msgv3
*                msgv4               = return-msgv4
*           IMPORTING
*                message_text_output = return-messtxt.
*      APPEND  return.
*    ELSE.
*      w_subrc = 0.
*    ENDIF.
*  ENDIF.
*<--------------------- FI Document End ------------------------------>*

ENDFORM.                    " P7000_MINUS_CO_PROC
*&---------------------------------------------------------------------*
*&      Form  P7000_PRICE_DIFFERENCE_PROC
*&---------------------------------------------------------------------*
FORM p7000_price_difference_proc.

  REFRESH : bdcdata.
  CLEAR   : w_belnr, w_gjahr.

*<----------------- FI Account Document Create ----------------------->*
  WRITE it_tab-to_qty     TO  l_menge UNIT  it_tab-meins.
  WRITE it_tab-meins      TO  l_meins.

  " First Screen.
  ">> Price Difference Account.
  PERFORM p2000_dynpro USING :
          'X' 'SAPMF05A'    '0100',
          ' ' 'BKPF-BLDAT'  w_budat,
          ' ' 'BKPF-BUDAT'  w_budat,
          ' ' 'BKPF-BLART'  'SV',
          ' ' 'BKPF-BUKRS'  p_bukrs,
          ' ' 'BKPF-WAERS'  it_tab-waers,
          ' ' 'BKPF-BKTXT' 'IMPORT SETTLEMENT',
          ' ' 'BKPF-XBLNR'  w_text,
          ' ' 'RF05A-NEWBS' '50',
          ' ' 'RF05A-NEWKO' w_pri_acc,
          ' ' 'BDC_OKCODE'  '/00'.

  w_price_amount  = w_settle_amount - w_minus_amount.
  WRITE w_price_amount TO  temp_wrbtr CURRENCY  it_tab-waers.
  PERFORM p2000_write_no_mask  CHANGING  temp_wrbtr.

  PERFORM p2000_dynpro USING :
          'X' 'SAPMF05A'    '0300',
          ' ' 'BSEG-WRBTR'  temp_wrbtr,
          ' ' 'BSEG-EBELN'  it_tab-ebeln,
          ' ' 'BSEG-EBELP'  it_tab-ebelp,
          ' ' 'BDC_OKCODE'  '=ZK'.

  PERFORM p2000_dynpro USING :
          'X' 'SAPLKACB'    '0002',
          ' ' 'COBL-MATNR'  it_tab-matnr,
          ' ' 'BDC_OKCODE'  '=ENTE'.

  PERFORM p2000_dynpro USING :
          'X' 'SAPMF05A'    '0330',
          ' ' 'BDC_OKCODE'  '/00'.

  IF w_minus_amount GT 0.

    PERFORM p2000_dynpro USING :
            ' ' 'RF05A-NEWBS' '50',
            ' ' 'RF05A-NEWKO' w_co_acc,
            ' ' 'BDC_OKCODE'  '/00'.

    WRITE w_minus_amount TO  temp_wrbtr CURRENCY  it_tab-waers.
    PERFORM p2000_write_no_mask  CHANGING  temp_wrbtr.

    PERFORM p2000_dynpro USING :
            'X' 'SAPMF05A'    '0300',
            ' ' 'BSEG-WRBTR'  temp_wrbtr,
            ' ' 'BSEG-EBELN'  it_tab-ebeln,
            ' ' 'BSEG-EBELP'  it_tab-ebelp,
            ' ' 'BDC_OKCODE'  '=ZK'.

    PERFORM p2000_dynpro USING :
            'X' 'SAPLKACB'    '0002',
            ' ' 'COBL-MATNR'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=ENTE'.

    PERFORM p2000_dynpro USING :
            'X' 'SAPMF05A'    '0330',
            ' ' 'BDC_OKCODE'  '/00'.
  ENDIF.

  " Actual Amount Setting.
  IF it_tab-lc_tset_ac GT 0 AND it_tab-bl_tset_ac GT 0.

    PERFORM p2000_dynpro USING :
            ' ' 'RF05A-NEWBS' '50',
            ' ' 'RF05A-NEWKO' w_act_acc,
            ' ' 'BDC_OKCODE'  '/00'.

    WRITE it_tab-lc_tset_ac TO  temp_wrbtr CURRENCY  it_tab-waers.
    PERFORM p2000_write_no_mask  CHANGING  temp_wrbtr.

    PERFORM p2000_dynpro USING :
            'X' 'SAPMF05A'    '0300',
            ' ' 'BSEG-WRBTR'  temp_wrbtr,
            ' ' 'BSEG-EBELN'  it_tab-ebeln,
            ' ' 'BSEG-EBELP'  it_tab-ebelp,
            ' ' 'BSEG-MENGE'  l_menge,
            ' ' 'BSEG-MEINS'  l_meins,
            ' ' 'BSEG-SGTXT'  it_tab-matnr,
            ' ' 'RF05A-NEWBS' '50',
            ' ' 'RF05A-NEWKO' w_act_acc,
            ' ' 'BDC_OKCODE'  '=ZK'.

    PERFORM p2000_dynpro USING :
            'X' 'SAPLKACB' '0002',
            ' ' 'COBL-MATNR'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=ENTE'.

    PERFORM p2000_dynpro USING :
            'X' 'SAPMF05A'    '0330',
            ' ' 'BSEG-XREF3'  w_lcno,
            ' ' 'BDC_OKCODE'  '/00'.

    WRITE it_tab-bl_tset_ac TO  temp_wrbtr CURRENCY  it_tab-waers.
    PERFORM p2000_write_no_mask  CHANGING  temp_wrbtr.

    PERFORM p2000_dynpro USING :
            'X' 'SAPMF05A'    '0300',
            ' ' 'BSEG-WRBTR'  temp_wrbtr,
            ' ' 'BSEG-EBELN'  it_tab-ebeln,
            ' ' 'BSEG-EBELP'  it_tab-ebelp,
            ' ' 'BSEG-MENGE'  l_menge,
            ' ' 'BSEG-MEINS'  l_meins,
            ' ' 'BSEG-SGTXT'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=ZK'.

    PERFORM p2000_dynpro USING :
            'X' 'SAPLKACB' '0002',
            ' ' 'COBL-MATNR'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=ENTE'.

    PERFORM p2000_dynpro USING :
            'X' 'SAPMF05A'    '0330',
            ' ' 'BSEG-XREF3'  w_blno,
            ' ' 'BDC_OKCODE'  '/00'.

  ELSEIF it_tab-lc_tset_ac GT 0.

    PERFORM p2000_dynpro USING :
            ' ' 'RF05A-NEWBS' '50',
            ' ' 'RF05A-NEWKO' w_act_acc,
            ' ' 'BDC_OKCODE'  '/00'.

    WRITE it_tab-lc_tset_ac TO  temp_wrbtr CURRENCY  it_tab-waers.
    PERFORM p2000_write_no_mask  CHANGING  temp_wrbtr.

    PERFORM p2000_dynpro USING :
            'X' 'SAPMF05A'    '0300',
            ' ' 'BSEG-WRBTR'  temp_wrbtr,
            ' ' 'BSEG-EBELN'  it_tab-ebeln,
            ' ' 'BSEG-EBELP'  it_tab-ebelp,
            ' ' 'BSEG-MENGE'  l_menge,
            ' ' 'BSEG-MEINS'  l_meins,
            ' ' 'BSEG-SGTXT'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=ZK'.

    PERFORM p2000_dynpro USING :
            'X' 'SAPLKACB' '0002',
            ' ' 'COBL-MATNR'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=ENTE'.

    PERFORM p2000_dynpro USING :
            'X' 'SAPMF05A'    '0330',
            ' ' 'BSEG-XREF3'  w_lcno,
            ' ' 'BDC_OKCODE'  '/00'.

  ELSEIF it_tab-bl_tset_ac GT 0.

    PERFORM p2000_dynpro USING :
            ' ' 'RF05A-NEWBS' '50',
            ' ' 'RF05A-NEWKO' w_act_acc,
            ' ' 'BDC_OKCODE'  '/00'.

    WRITE it_tab-bl_tset_ac TO  temp_wrbtr CURRENCY  it_tab-waers.
    PERFORM p2000_write_no_mask  CHANGING  temp_wrbtr.

    PERFORM p2000_dynpro USING :
            'X' 'SAPMF05A'    '0300',
            ' ' 'BSEG-WRBTR'  temp_wrbtr,
            ' ' 'BSEG-EBELN'  it_tab-ebeln,
            ' ' 'BSEG-EBELP'  it_tab-ebelp,
            ' ' 'BSEG-MENGE'  l_menge,
            ' ' 'BSEG-MEINS'  l_meins,
            ' ' 'BSEG-SGTXT'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=ZK'.

    PERFORM p2000_dynpro USING :
            'X' 'SAPLKACB' '0002',
            ' ' 'COBL-MATNR'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=ENTE'.

    PERFORM p2000_dynpro USING :
            'X' 'SAPMF05A'    '0330',
            ' ' 'BSEG-XREF3'  w_blno,
            ' ' 'BDC_OKCODE'  '/00'.
  ENDIF.

  " Planned Amount.
  IF it_tab-lc_tset_pl GT 0 AND it_tab-bl_tset_pl GT 0.

    PERFORM p2000_dynpro USING :
            ' ' 'RF05A-NEWBS' '40',
            ' ' 'RF05A-NEWKO' w_pld_acc,
            ' ' 'BDC_OKCODE'  '/00'.

    WRITE it_tab-lc_tset_pl TO  temp_wrbtr CURRENCY  it_tab-waers.
    PERFORM p2000_write_no_mask  CHANGING  temp_wrbtr.

    PERFORM p2000_dynpro USING :
            'X' 'SAPMF05A'    '0300',
            ' ' 'BSEG-WRBTR'  temp_wrbtr,
            ' ' 'BSEG-EBELN'  it_tab-ebeln,
            ' ' 'BSEG-EBELP'  it_tab-ebelp,
            ' ' 'BSEG-MENGE'  l_menge,
            ' ' 'BSEG-MEINS'  l_meins,
            ' ' 'BSEG-SGTXT'  it_tab-matnr,
            ' ' 'RF05A-NEWBS' '40',
            ' ' 'RF05A-NEWKO' w_pld_acc,
            ' ' 'BDC_OKCODE'  '/00'.

    PERFORM p2000_dynpro USING :
            'X' 'SAPLKACB' '0002',
            ' ' 'COBL-MATNR'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=ENTE'.

    WRITE it_tab-bl_tset_pl TO  temp_wrbtr CURRENCY  it_tab-waers.
    PERFORM p2000_write_no_mask  CHANGING  temp_wrbtr.

    PERFORM p2000_dynpro USING :
            'X' 'SAPMF05A'    '0300',
            ' ' 'BSEG-WRBTR'  temp_wrbtr,
            ' ' 'BSEG-EBELN'  it_tab-ebeln,
            ' ' 'BSEG-EBELP'  it_tab-ebelp,
            ' ' 'BSEG-MENGE'  l_menge,
            ' ' 'BSEG-MEINS'  l_meins,
            ' ' 'BSEG-SGTXT'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=BU'.

    PERFORM p2000_dynpro USING :
            'X' 'SAPLKACB' '0002',
            ' ' 'COBL-MATNR'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=ENTE'.

  ELSEIF it_tab-lc_tset_pl GT 0.

    PERFORM p2000_dynpro USING :
            ' ' 'RF05A-NEWBS' '40',
            ' ' 'RF05A-NEWKO' w_pld_acc,
            ' ' 'BDC_OKCODE'  '/00'.

    WRITE it_tab-lc_tset_pl TO  temp_wrbtr CURRENCY  it_tab-waers.
    PERFORM p2000_write_no_mask  CHANGING  temp_wrbtr.

    PERFORM p2000_dynpro USING :
            'X' 'SAPMF05A'    '0300',
            ' ' 'BSEG-WRBTR'  temp_wrbtr,
            ' ' 'BSEG-EBELN'  it_tab-ebeln,
            ' ' 'BSEG-EBELP'  it_tab-ebelp,
            ' ' 'BSEG-MENGE'  l_menge,
            ' ' 'BSEG-MEINS'  l_meins,
            ' ' 'BSEG-SGTXT'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=BU'.

    PERFORM p2000_dynpro USING :
            'X' 'SAPLKACB' '0002',
            ' ' 'COBL-MATNR'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=ENTE'.

  ELSEIF it_tab-bl_tset_pl GT 0.

    PERFORM p2000_dynpro USING :
            ' ' 'RF05A-NEWBS' '40',
            ' ' 'RF05A-NEWKO' w_pld_acc,
            ' ' 'BDC_OKCODE'  '/00'.

    WRITE it_tab-bl_tset_pl TO  temp_wrbtr CURRENCY  it_tab-waers.
    PERFORM p2000_write_no_mask  CHANGING  temp_wrbtr.

    PERFORM p2000_dynpro USING :
            'X' 'SAPMF05A'    '0300',
            ' ' 'BSEG-WRBTR'  temp_wrbtr,
            ' ' 'BSEG-EBELN'  it_tab-ebeln,
            ' ' 'BSEG-EBELP'  it_tab-ebelp,
            ' ' 'BSEG-MENGE'  l_menge,
            ' ' 'BSEG-MEINS'  l_meins,
            ' ' 'BSEG-SGTXT'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=BU'.

    PERFORM p2000_dynpro USING :
            'X' 'SAPLKACB' '0002',
            ' ' 'COBL-MATNR'  it_tab-matnr,
            ' ' 'BDC_OKCODE'  '=ENTE'.

  ENDIF.

  SET PARAMETER ID 'BLN' FIELD ''.        " Document No
  SET PARAMETER ID 'GJR' FIELD ''.        " Year

  PERFORM call_bdc  USING 'FB01'.

*    " BDC CALL.
*    CALL TRANSACTION 'FB01'  USING       bdcdata
*                             MODE        p_mode
*                             UPDATE      'S'
*                             MESSAGES    INTO   messtab.
*
*  w_subrc = sy-subrc.
*
*  IF w_subrc NE 0.      ">> ERROR
*    LOOP AT messtab.
*      MOVE : messtab-msgtyp  TO     return-msgtyp,
*             messtab-msgid   TO     return-msgid,
*             messtab-msgnr   TO     return-msgnr,
*             messtab-msgv1   TO     return-msgv1,
*             messtab-msgv2   TO     return-msgv2,
*             messtab-msgv3   TO     return-msgv3,
*             messtab-msgv4   TO     return-msgv4.
*
*      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
*           EXPORTING
*                msgid               = return-msgid
*                msgnr               = return-msgnr
*                msgv1               = return-msgv1
*                msgv2               = return-msgv2
*                msgv3               = return-msgv3
*                msgv4               = return-msgv4
*           IMPORTING
*                message_text_output = return-messtxt.
*      APPEND  return.
*    ENDLOOP.
*    w_subrc = 4.
*  ELSE.
*    GET PARAMETER ID 'BLN' FIELD w_belnr.
*    GET PARAMETER ID 'GJR' FIELD w_gjahr.
*
*    IF w_belnr IS INITIAL.
*      w_subrc = 4.
*      MOVE : 'E'             TO     return-msgtyp,
*             'ZIM1'          TO     return-msgid,
*             '003'           TO     return-msgnr,
*             it_tab-ebeln    TO     return-msgv1,
*             it_tab-ebelp    TO     return-msgv2,
*             space           TO     return-msgv3,
*             space           TO     return-msgv4.
*
*      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
*           EXPORTING
*                msgid               = return-msgid
*                msgnr               = return-msgnr
*                msgv1               = return-msgv1
*                msgv2               = return-msgv2
*                msgv3               = return-msgv3
*                msgv4               = return-msgv4
*           IMPORTING
*                message_text_output = return-messtxt.
*      APPEND  return.
*    ELSE.
*      w_subrc = 0.
*    ENDIF.
*  ENDIF.
*<--------------------- FI Document End ------------------------------>*

ENDFORM.                    " P7000_PRICE_DIFFERENCE_PROC
*&---------------------------------------------------------------------*
*&      Module  D0100_STATUS_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE d0100_status_scr0100 OUTPUT.

  SET PF-STATUS 'STDLISW'.

  CASE include.
    WHEN 'POPU'.
      SET TITLEBAR 'PUPU' WITH 'Status LIST'.
    WHEN OTHERS.
  ENDCASE.

  SUPPRESS DIALOG.

ENDMODULE.                 " D0100_STATUS_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  D0100_LIST_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE d0100_list_check_scr0100 INPUT.

  LEAVE TO LIST-PROCESSING.
  CASE include.
    WHEN 'POPU'.
      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
      WRITE : / sy-uline(96),    /   sy-vline NO-GAP,
                'Type'   NO-GAP,     sy-vline NO-GAP,
                'Message Text',  94 sy-vline NO-GAP,
                'T'      NO-GAP,     sy-vline,
              / sy-uline(96).
      LOOP AT return.
        w_mod  =  sy-tabix MOD 2.
        FORMAT RESET.
        IF w_mod EQ 0.
          FORMAT COLOR COL_NORMAL  INTENSIFIED ON.
        ELSE.
          FORMAT COLOR COL_NORMAL  INTENSIFIED OFF.
        ENDIF.
        WRITE : / sy-vline NO-GAP, return-icon(4) NO-GAP,
                  sy-vline NO-GAP, return-messtxt(87) NO-GAP,
                  sy-vline NO-GAP.

        CASE return-msgtyp.
          WHEN 'E'.
            FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
          WHEN 'W'.
            FORMAT COLOR COL_KEY      INTENSIFIED OFF.
          WHEN 'I'.
            FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
          WHEN 'S'.
            FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
        ENDCASE.

        WRITE : return-msgtyp(1) NO-GAP, sy-vline NO-GAP.
        HIDE:return.
      ENDLOOP.
      WRITE : / sy-uline(96).
      CLEAR : return.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " D0100_LIST_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  P6000_DB_INSERT
*&---------------------------------------------------------------------*
FORM p6000_db_insert.

  LOOP AT it_setac.

    MOVE-CORRESPONDING it_setac  TO  ztsetac.

    " Sequence Get.
    SELECT MAX( zfseq ) INTO w_max_seq
    FROM   ztsetac
    WHERE zfsetym      EQ   it_setac-zfsetym
      AND ebeln        EQ   it_setac-ebeln
      AND ebelp        EQ   it_setac-ebelp.

    IF sy-subrc NE 0  OR  w_max_seq IS INITIAL.
      w_max_seq  =  1.
    ELSE.
      w_max_seq  =  w_max_seq  +  1.
    ENDIF.

    MOVE  w_max_seq   TO  ztsetac-zfseq.

    ztsetac-cdat  = sy-datum.
    ztsetac-ernam = sy-uname.
    INSERT  ztsetac.

  ENDLOOP.

ENDFORM.                    " P6000_DB_INSERT
*&---------------------------------------------------------------------*
*&      Form  P4000_TOTAL_LINE_WRITE
*&---------------------------------------------------------------------*
FORM p4000_total_line_write.

  FORMAT RESET.
  FORMAT COLOR COL_TOTAL INTENSIFIED ON.

  WRITE:/                                          sy-vline NO-GAP,
         (81) 'Total'           CENTERED   NO-GAP, sy-vline NO-GAP,
         (05) it_tab-waers                 NO-GAP, sy-vline NO-GAP,
         (16) w_tot_actual      CURRENCY it_tab-waers
                                           NO-GAP, sy-vline NO-GAP,
         (16) w_tot_pland       CURRENCY it_tab-waers
                                           NO-GAP, sy-vline NO-GAP,
         (16) w_tot_cha         CURRENCY it_tab-waers
                                           NO-GAP, sy-vline NO-GAP,
         (16) w_set_actual      CURRENCY it_tab-waers
                                           NO-GAP, sy-vline NO-GAP,
         (16) w_set_pland       CURRENCY it_tab-waers
                                           NO-GAP, sy-vline NO-GAP,
         (16) w_tset_actual     CURRENCY it_tab-waers
                                           NO-GAP, sy-vline NO-GAP,
         (16) w_tset_pland      CURRENCY it_tab-waers
                                           NO-GAP, sy-vline NO-GAP,
         (16) w_nset            CURRENCY it_tab-waers
                                           NO-GAP, sy-vline NO-GAP.
  WRITE : sy-uline.

ENDFORM.                    " P4000_TOTAL_LINE_WRITE
*----------------------------------------------------------------------*
*        Start new transaction according to parameters                 *
*----------------------------------------------------------------------*
FORM call_bdc USING tcode.

  IF p_sim = 'X'.
    CALL FUNCTION 'BDC_INSERT'
         EXPORTING
              tcode     = tcode
         TABLES
              dynprotab = bdcdata.
  ELSE.

    CALL TRANSACTION tcode   USING       bdcdata
                             MODE        p_mode
                             UPDATE      'S'
                             MESSAGES    INTO   messtab.
  ENDIF.
  w_subrc = sy-subrc.

  IF w_subrc NE 0.
    LOOP AT messtab.
      MOVE : messtab-msgtyp  TO     return-msgtyp,
             messtab-msgid   TO     return-msgid,
             messtab-msgnr   TO     return-msgnr,
             messtab-msgv1   TO     return-msgv1,
             messtab-msgv2   TO     return-msgv2,
             messtab-msgv3   TO     return-msgv3,
             messtab-msgv4   TO     return-msgv4.

      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
           EXPORTING
                msgid               = return-msgid
                msgnr               = return-msgnr
                msgv1               = return-msgv1
                msgv2               = return-msgv2
                msgv3               = return-msgv3
                msgv4               = return-msgv4
           IMPORTING
                message_text_output = return-messtxt.
      APPEND  return.
    ENDLOOP.
    w_subrc = 4.
  ELSE.
    IF p_sim = space.
      GET PARAMETER ID 'MLN' FIELD w_belnr.
      GET PARAMETER ID 'MLJ' FIELD w_gjahr.

* if call transaction, get document number...
      IF w_belnr IS INITIAL.
        w_subrc = 4.
        MOVE : 'E'             TO     return-msgtyp,
               'ZIM1'          TO     return-msgid,
               '003'           TO     return-msgnr,
               it_tab-ebeln    TO     return-msgv1,
               it_tab-ebelp    TO     return-msgv2,
               space           TO     return-msgv3,
               space           TO     return-msgv4.

        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
             EXPORTING
                  msgid               = return-msgid
                  msgnr               = return-msgnr
                  msgv1               = return-msgv1
                  msgv2               = return-msgv2
                  msgv3               = return-msgv3
                  msgv4               = return-msgv4
             IMPORTING
                  message_text_output = return-messtxt.
        APPEND  return.
      ENDIF.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  bdc_open
*&---------------------------------------------------------------------*
FORM bdc_open.
  IF p_sim = 'X'.
    CALL FUNCTION 'BDC_OPEN_GROUP'
         EXPORTING
              client   = sy-mandt
              group    = 'IM_SETTLE'
              user     = sy-uname
              keep     = ' '
              holddate = '20000101'. "sy-datum.
  ENDIF.
ENDFORM.                    " bdc_open
*&---------------------------------------------------------------------*
*&      Form  display_out
*&---------------------------------------------------------------------*
FORM display_out.
  PERFORM field_setting TABLES gt_fieldcat USING :
   'EBELN'      'PO'            '10' 'X' 'L'  ' '  ' '  '  ' ' '  ' ',
   'EBELP'      'ITM'           '05' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'MATNR'      'Material'      '18' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'KNTTP'      'AAC'           '01' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'LC_QTY'     'LC_QTY'        '10' ' ' 'L'  ' '  '0'  '  ' ' '  ' ',
   'BL_QTY'     'BL_QTY'        '10' ' ' 'L'  ' '  '0'  '  ' ' '  ' ',
   'GR_QTY'     'GR_QTY'        '10' ' ' 'L'  ' '  '0'  '  ' ' '  ' ',
   'TO_QTY'     'TO_QTY'        '10' ' ' 'L'  ' '  '0'  '  ' ' '  ' ',

   'LC_ACT'     'LC_ACT'        '17' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
   'LC_PLD'     'LC_PLD'        '17' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
   'LC_CHA'     'LC_CHA'        '17' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
   'LC_SET_AC'  'LC_SET_AC'     '17' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
   'LC_SET_PL'  'LC_SET_PL'     '17' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
   'LC_TSET_AC' 'LC_TSET_AC'    '17' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
   'LC_TSET_PL' 'LC_TSET_PL'    '17' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
   'LC_NSET'    'LC_NSET'       '17' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
   'BL_ACT'     'BL_ACT'        '17' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
   'BL_PLD'     'BL_PLD'        '17' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
   'BL_CHA'     'BL_CHA'        '17' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
   'BL_SET_AC'  'BL_SET_AC'     '17' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
   'BL_SET_PL'  'BL_SET_PL'     '17' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
   'BL_TSET_AC' 'BL_TSET_AC'    '17' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
   'BL_TSET_PL' 'BL_TSET_PL'    '17' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
   'BL_NSET'    'BL_NSET'       '17' ' ' 'R'  ' '  ' '  '  ' ' '  'X'.

  w_program = sy-repid.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
       EXPORTING
            i_callback_program = w_program
            it_fieldcat        = gt_fieldcat
            i_save             = 'A'
       TABLES
            t_outtab           = it_tab
       EXCEPTIONS
            program_error      = 1
            OTHERS             = 2.

ENDFORM.                    " display_out
*ALV
*&--------------------------------------------------------------------
*&      Form  make_field_category
*&--------------------------------------------------------------------
FORM field_setting TABLES p_fieldcat_t LIKE gt_fieldcat USING
                                  p_fieldname       " FIELD name
                                  p_title           " field titlw
                                  p_outputlen       " length
                                  p_key             "
                                  p_just            "
                                  p_noout           "
                                  p_round           "
                                  p_cfield          " currency field nam
                                  p_qfield          " quantity field nam
                                  p_dosum           " make sum
                                  .

  DATA: ls_fieldcat TYPE slis_fieldcat_alv.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname  = p_fieldname.
*  ls_fieldcat-seltext_s = p_title.
*  ls_fieldcat-seltext_m = p_title.
  ls_fieldcat-seltext_l  = p_title.
  ls_fieldcat-outputlen  = p_outputlen.
  ls_fieldcat-key        = p_key.
  ls_fieldcat-just       = p_just.
  ls_fieldcat-edit       = ''.   "p_edit.
  ls_fieldcat-no_out     = p_noout.
  ls_fieldcat-decimals_out   = p_round.
*  ls_fieldcat-cfieldname = p_cfield.
  ls_fieldcat-currency   = p_cfield.
  ls_fieldcat-qfieldname = p_qfield.
  ls_fieldcat-do_sum     = p_dosum.

  APPEND ls_fieldcat TO gt_fieldcat.

ENDFORM.                    " fill_field_category
