*&----------------------------------------------------------------------
*& Development ID : ZRFIF01
*& Program ID     : ZFMR0001
*& Program Name   : Monthly Report
*& Created by     : Byung Sung Bae
*& Created on     : 07/28/2005
*& Reference Pgm  : ZRFIF01(HMMA)
*&
*& Modification Log
*& Date        Developer Issue No Description
*&======================================================================
*& YYYY/MM/DD
*& 2006/05/16 BJ.Kim     Change Category 'Commitment' in detail.
*& 04/06/2010 Valerian   Add logic to select commitment item based on
*&                       commitment item group.
*&----------------------------------------------------------------------

REPORT  zfmr0001 LINE-SIZE 252
                 LINE-COUNT 65
                 NO STANDARD PAGE HEADING.
INCLUDE <icon>.
INCLUDE zfm_auth_form.
TYPE-POOLS zfmcm.
TABLES: fmci, fmfctr, bppe, tbpfe, aufk, fm01.

DATA: zsfm0008 LIKE zsfm0008.

*---// Internal tables
DATA: it_9000 TYPE STANDARD TABLE OF zsfm0008
                                     WITH HEADER LINE.
DATA: gt_9000 TYPE STANDARD TABLE OF zsfm0008              "VALERIAN
                                     WITH HEADER LINE.     "VALERIAN


DATA: BEGIN OF it_download OCCURS 0,
        geber(10),
        fictr(16),
        fipex(24),
        bezei(20),
        ktext(40),
        profil(7),
        cttxt(10),
        total(21),
        tslvt(21),
        wtp01(21),
        wtp02(21),
        wtp03(21),
        wtp04(21),
        wtp05(21),
        wtp06(21),
        wtp07(21),
        wtp08(21),
        wtp09(21),
        wtp10(21),
        wtp11(21),
        wtp12(21),
      END   OF it_download.

DATA: BEGIN OF it_field OCCURS 0,
        text(60),
      END   OF it_field.

*---// Global Variables
DATA: v_fname  LIKE rlgrap-filename VALUE 'c:\temp\fm_actual.xls'.

*---// Constants
CONSTANTS: c_versn      LIKE bppe-versn     VALUE zfmcm_versn_0,      "Version
           c_hivarnt    LIKE fmhisv-hivarnt VALUE zfmcm_hivarnt,      "Hierarchy
           c_original   TYPE i              VALUE  1,
           c_supplement TYPE i              VALUE  2,
           c_transfer   TYPE i              VALUE  3,
           c_return     TYPE i              VALUE  4,
           c_current    TYPE i              VALUE  5,
           c_released   TYPE i              VALUE  6,
           c_commitment TYPE i              VALUE  7,
           c_invoice    TYPE i              VALUE  8,
           c_payments   TYPE i              VALUE  9,
           c_available  TYPE i              VALUE 10,
           c_unknown    TYPE i              VALUE 11.

DATA: c_orig_oth   TYPE i              VALUE  1,  "Original
      c_supp_oth   TYPE i              VALUE  2,  "Supplement
      c_tran_oth   TYPE i              VALUE  3,  "Transfer
      c_retu_oth   TYPE i              VALUE  4,  "Return
      c_curr_oth   TYPE i              VALUE  5,  "Current
      c_rele_oth   TYPE i              VALUE  6,  "Released
      c_pr_oth     TYPE i              VALUE  7,  "PR & PO
      c_po_oth     TYPE i              VALUE  8,  "PR & PO
      c_park_oth   TYPE i              VALUE  9,  "Parked Doc.
      c_down_oth   TYPE i              VALUE 10,  "Down payment
      c_invo_oth   TYPE i              VALUE 11,  "Invoice
      c_godi_oth   TYPE i              VALUE 12,  "Goods issue
      c_paym_oth   TYPE i              VALUE 13,  "Payment
      c_avai_oth   TYPE i              VALUE 14,  "Available
      c_unkn_oth   TYPE i              VALUE 15.
DATA : l_fund TYPE bp_geber VALUE '',
       l_fictr TYPE fistl VALUE '',
       l_fipex TYPE fm_fipex VALUE ''.

* BEGIN OF VALERIAN
CONSTANTS : C_CLASS  LIKE SETNODE-SETCLASS VALUE '0311'.

DEFINE ICLEAR.
  CLEAR : &1, &1[].
END-OF-DEFINITION.

DATA:  GT_NODE LIKE SETNODE OCCURS 0 WITH HEADER LINE,
       GT_LEAF LIKE SETLEAF OCCURS 0 WITH HEADER LINE.

* END OF VALERIAN
*---// Selection screen
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-t01.
PARAMETERS: p_fikrs LIKE fmci-fikrs MEMORY ID fik OBLIGATORY
                                    DEFAULT zfmcm_fm_area,
            p_gjahr LIKE fmci-gjahr MEMORY ID gjr OBLIGATORY
                                    DEFAULT sy-datum(4).
***            p_fictr TYPE fm_fictr.
SELECT-OPTIONS: s_geber  FOR bppe-geber,
                s_fictr  FOR fmfctr-fictr OBLIGATORY,
                s_fipex  FOR fmci-fipex,
                s_profil FOR tbpfe-profil,
*                s_potyp  FOR fmci-potyp DEFAULT '3'.
                s_potyp  FOR fmci-potyp.
SELECTION-SCREEN SKIP 1.                                   "VALERIAN
PARAMETERS: P_GRPID LIKE GRPDYNP-NAME_COALL OBLIGATORY.    "VALERIAN
SELECTION-SCREEN END OF BLOCK bl1.

* BEGIN OF VALERIAN
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_GRPID.

  DATA : SETID       LIKE  RGSBS-SETNR.

  CALL FUNCTION 'K_GROUP_SELECT'
    EXPORTING
      CLASS           = C_CLASS
      FIELD_NAME      = ' '
      SEARCHFLD       = ' '
      SEARCHFLD_INPUT = ' '
      SET             = '*'
    IMPORTING
      SETID           = SETID
    EXCEPTIONS
      NO_SET_PICKED   = 1.

  P_GRPID = SETID+8(10).
* END OF VALERIAN

INITIALIZATION.
  CONCATENATE 'KMMA_' SY-DATUM(4) INTO P_GRPID.            "VALERIAN
  SET PARAMETER ID 'FIC' FIELD l_fund.
  SET PARAMETER ID 'FIS' FIELD l_fictr.
  SET PARAMETER ID 'FPS' FIELD l_fipex.
  s_geber = 'IEQ'.
  APPEND s_geber.
*---// Input value check & Read data
AT SELECTION-SCREEN.
*
*  IF sy-subrc NE 0.
*    MESSAGE e001(zz) WITH '??? ???.'.
*  ENDIF.

  CHECK sy-ucomm EQ 'ONLI'.
*  PERFORM user_auth_check TABLES s_fictr[]
*                          USING p_fikrs.
  PERFORM user_auth_check_geber TABLES s_geber[]
                                       s_fictr[]
                                 USING p_fikrs.
  CHECK g_auth_check IS INITIAL.
  PERFORM check_rtn.
  PERFORM GET_COMMITMENT_ITEM_GROUP.                       "VALERIAN
  PERFORM read_data.

TOP-OF-PAGE.
  PERFORM display_header.

START-OF-SELECTION.
  SET PF-STATUS 'BASIC'.
  SET TITLEBAR  '9000'.
  PERFORM display_rtn.

AT USER-COMMAND.
  CASE sy-ucomm.
    WHEN 'DOWNLOAD'.
      CLEAR: sy-ucomm.
      PERFORM download_rtn.
    WHEN 'HIERARCHY'.
* BEGIN OF VALERIAN
      DATA : p_grpid1 LIKE grpdynp-name_coall.
      CONCATENATE 'KMMA_' sy-datum(4) INTO p_grpid1.
      SUBMIT zfmr0030  WITH p_fikrs   = p_fikrs
                       WITH p_gjahr   = p_gjahr
                       WITH p_period  = sy-datum+4(2)
                       WITH s_geber  IN s_geber
                       WITH s_fictr  IN s_fictr
                       WITH s_fipex  IN s_fipex
                       WITH s_profil IN s_profil
                       WITH s_potyp  IN s_potyp
                       WITH p_grpid   = p_grpid1
                       AND RETURN.

*      DATA : p_grpid LIKE grpdynp-name_coall.
*      CONCATENATE 'KMMA_' sy-datum(4) INTO p_grpid.
*      SUBMIT zfmr0030  WITH p_fikrs   = p_fikrs
*                       WITH p_gjahr   = p_gjahr
*                       WITH p_period  = sy-datum+4(2)
*                       WITH s_geber  IN s_geber
*                       WITH s_fictr  IN s_fictr
*                       WITH s_fipex  IN s_fipex
*                       WITH s_profil IN s_profil
*                       WITH s_potyp  IN s_potyp
*                       WITH p_grpid   = p_grpid
*                       AND RETURN.
* END OF VALERIAN
    WHEN 'DRILL'.
      DATA : c_field(20), c_line(20), p_per_fr(2), p_per_to(2).
      RANGES : r_geber  FOR bppe-geber,
               r_fictr  FOR fmfctr-fictr,
               r_fipex  FOR fmci-fipex.
      REFRESH : r_geber, r_fictr, r_fipex.
      CLEAR   : r_geber, r_fictr, r_fipex.
      GET CURSOR  FIELD c_field
                  LINE  c_line.

      CLEAR  it_9000-fipex.
*      DO.                                                 "VALERIAN
       WHILE c_line GE 0.                                  "VALERIAN
        READ LINE c_line FIELD VALUE it_9000-fipex INTO r_fipex-low.
        IF sy-subrc = 0 AND r_fipex-low NE space.
          r_fipex-sign = 'I'.
          r_fipex-option = 'EQ'.
          APPEND r_fipex.

          READ LINE c_line FIELD VALUE it_9000-geber r_geber-low.
          IF r_geber-low NE space.
            r_geber-sign = 'I'.
            r_geber-option = 'EQ'.
            APPEND r_geber.
          ENDIF.
          c_line = c_line + 1.

          READ LINE c_line FIELD VALUE it_9000-fictr
                                  INTO r_fictr-low.
          IF sy-subrc = 0 AND r_fictr-low NE space.
            r_fictr-sign = 'I'.
            r_fictr-option = 'EQ'.
            APPEND r_fictr.
            EXIT.
          ENDIF.

          EXIT.
        ENDIF.
        c_line = c_line - 1.
*      ENDDO.                                              "VALERIAN
      ENDWHILE.                                            "VALERIAN

* BEGIN OF VALERIAN
      IF c_line LT 0.
        MESSAGE i208(00) WITH 'Please select a line item'.
        EXIT.
      ENDIF.
* END OF VALERIAN

      IF c_field(11) = 'IT_9000-WTP'.
        p_per_fr = c_field+11(2).
        p_per_to = c_field+11(2).
      ELSE.
        p_per_fr = '01'.
        p_per_to = '12'.
      ENDIF.

      SUBMIT zrffmepgax
        WITH s_fikrs-low = p_fikrs
        WITH p_fyr_fr    = p_gjahr
        WITH p_per_fr    = p_per_fr
        WITH p_fyr_to    = p_gjahr
        WITH p_per_to    = p_per_to
        WITH s_fonds    IN r_geber
        WITH s_fictr    IN r_fictr
        WITH s_fipex    IN r_fipex
        WITH p_maxsel    = ''
*        VIA SELECTION-SCREEN
        AND RETURN.

  ENDCASE.

*&---------------------------------------------------------------------*
*&      Form  CHECK_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_rtn .
  PERFORM check_fikrs.
ENDFORM.                    " CHECK_RTN
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data .
  CALL FUNCTION 'Z_FM_GET_MONTHLY_BUDGET_OTH'
    EXPORTING
      i_fikrs            = p_fikrs
      i_gjahr            = p_gjahr
    TABLES
      t_geber            = s_geber
      t_fictr            = s_fictr
      t_fipex            = s_fipex
      t_profil           = s_profil
      t_potyp            = s_potyp
      t_itab             = gt_9000                         "VALERIAN
*     t_itab             = it_9000                         "VALERIAN
    EXCEPTIONS
      no_fm_area         = 1
      no_fund            = 2
      no_funds_center    = 3
      no_commitment_item = 4
      no_profile         = 5
      no_category        = 6
      no_original        = 7
      OTHERS             = 8.
  CASE sy-subrc.
    WHEN 1.
      MESSAGE e000(zz) WITH text-m03.
    WHEN 2.
      MESSAGE e000(zz) WITH text-m05.
    WHEN 3.
      MESSAGE e000(zz) WITH text-m04.
    WHEN 4.
      MESSAGE e000(zz) WITH text-m08.
    WHEN 5.
      MESSAGE e000(zz) WITH text-m09.
    WHEN 6.
      MESSAGE e000(zz) WITH text-m10.
    WHEN 7.
      MESSAGE e000(zz) WITH text-m02.
    WHEN 8.
      MESSAGE e000(zz) WITH text-m11.
  ENDCASE.
ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  get_budget
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_budget .
*---// Read Original, Supplement, Transfer, Return, Releleased

  DATA: BEGIN OF lt_bppe OCCURS 0.
          INCLUDE STRUCTURE bppe.
  DATA:   fictr   LIKE   fmfctr-fictr,
          fipex   LIKE   fmcit-fipex,
          potyp   LIKE   fmci-potyp,
          bezei   LIKE   fmcit-bezei,
        END   OF lt_bppe.

  SELECT a~fictr d~fipex d~potyp e~bezei
         c~objnr c~posit c~gjahr c~geber
         c~versn c~vorga c~twaer c~farea
         c~wtp01 c~wtp02 c~wtp03 c~wtp04
         c~wtp05 c~wtp06 c~wtp07 c~wtp08
         c~wtp09 c~wtp10 c~wtp11 c~wtp12
    INTO CORRESPONDING FIELDS OF TABLE lt_bppe
    FROM fmfctr AS a INNER JOIN fmhisv AS b
                        ON a~fikrs   = b~fikrs
                       AND a~fictr   = b~fistl
                       AND b~hivarnt = c_hivarnt
                     INNER JOIN bppe AS c
                        ON a~ctr_objnr = c~objnr
                     INNER JOIN fmci AS d
                        ON c~posit = d~posit
                       AND a~fikrs = d~fikrs
                     INNER JOIN fmcit AS e
                        ON d~fikrs = e~fikrs
                       AND d~fipex = e~fipex
                       AND e~spras = sy-langu
                       AND e~gjahr = '0000'
   WHERE a~fikrs     EQ p_fikrs
     AND a~fictr     IN s_fictr
     AND b~parent_st NE space
     AND c~gjahr     EQ p_gjahr
     AND c~versn     EQ c_versn
     AND c~geber     IN s_geber
     AND d~fipex     IN s_fipex
     AND d~potyp     IN s_potyp.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m02.
  ENDIF.

  LOOP AT lt_bppe.
    CLEAR: it_9000.

    MOVE: lt_bppe-fictr TO it_9000-fictr,
          lt_bppe-potyp TO it_9000-potyp,
          lt_bppe-fipex TO it_9000-fipex,
          lt_bppe-geber TO it_9000-geber,
          lt_bppe-bezei TO it_9000-bezei,
          fm01-waers    TO it_9000-waers,
          lt_bppe-wtp01 TO it_9000-wtp01,
          lt_bppe-wtp02 TO it_9000-wtp02,
          lt_bppe-wtp03 TO it_9000-wtp03,
          lt_bppe-wtp04 TO it_9000-wtp04,
          lt_bppe-wtp05 TO it_9000-wtp05,
          lt_bppe-wtp06 TO it_9000-wtp06,
          lt_bppe-wtp07 TO it_9000-wtp07,
          lt_bppe-wtp08 TO it_9000-wtp08,
          lt_bppe-wtp09 TO it_9000-wtp09,
          lt_bppe-wtp10 TO it_9000-wtp10,
          lt_bppe-wtp11 TO it_9000-wtp11,
          lt_bppe-wtp12 TO it_9000-wtp12.

    PERFORM get_budget_profile USING   p_fikrs       lt_bppe-posit
                                       lt_bppe-objnr lt_bppe-gjahr
                                       lt_bppe-geber lt_bppe-farea
                              CHANGING it_9000-profil.

    CHECK it_9000-profil IN s_profil.

    CASE lt_bppe-vorga.
      WHEN 'KBUD'.
        it_9000-ctgry = c_original.    "Original
        COLLECT it_9000.
        it_9000-ctgry = c_current.     "Current
        COLLECT it_9000.
      WHEN 'KBN0'.
        it_9000-ctgry = c_supplement.  "Supplement
        COLLECT it_9000.
        it_9000-ctgry = c_current.     "Current
        COLLECT it_9000.
      WHEN 'KBR0'.
        it_9000-ctgry = c_return.      "Return
        COLLECT it_9000.
        it_9000-ctgry = c_current.     "Current
        COLLECT it_9000.
      WHEN 'KBUS'.
        it_9000-ctgry = c_transfer.    "Transfer
        COLLECT it_9000.
        it_9000-ctgry = c_current.     "Current
        COLLECT it_9000.
      WHEN 'KBUE'.
        it_9000-ctgry = c_transfer.    "Transfer
        COLLECT it_9000.
        it_9000-ctgry = c_current.     "Current
        COLLECT it_9000.
      WHEN 'KBFR'.
        it_9000-ctgry = c_released.    "Released
        COLLECT it_9000.
      WHEN OTHERS.
        it_9000-ctgry = c_unknown.
        COLLECT it_9000.
    ENDCASE.
  ENDLOOP.
ENDFORM.        " get_budget

*&---------------------------------------------------------------------*
*&      Module  status  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status OUTPUT.
  CASE sy-dynnr.
    WHEN 9000.
      SET PF-STATUS '9000'.
  ENDCASE.
ENDMODULE.                 " status  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  CASE sy-ucomm.
    WHEN 'EXIT' OR 'CANC'.
      CLEAR: sy-ucomm.
      CLEAR: v_fname.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " exit  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      CLEAR sy-ucomm.
      LEAVE TO SCREEN 0.
    WHEN 'ENTER'.
      CLEAR: sy-ucomm.
      PERFORM check_filename.
  ENDCASE.
ENDMODULE.                 " user_command_9000  INPUT
*&---------------------------------------------------------------------*
*&      Form  get_actual
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_actual .
  DATA: BEGIN OF lt_v_fmifi OCCURS 0.
          INCLUDE STRUCTURE v_fmifi.
  DATA:   fictr     LIKE   fmfctr-fictr,
          ctr_objnr LIKE   fmfctr-ctr_objnr,
          posit     LIKE   fmci-posit,
          potyp     LIKE   fmci-potyp,
          bezei     LIKE   fmcit-bezei,
        END   OF lt_v_fmifi.

  SELECT a~fictr d~fipex d~potyp e~bezei
         a~ctr_objnr     d~posit
         c~farea c~fonds c~wrttp c~btart
         c~perio c~fkbtr c~gjahr
    INTO CORRESPONDING FIELDS OF TABLE lt_v_fmifi
    FROM fmfctr AS a INNER JOIN fmhisv AS b
                        ON a~fikrs   = b~fikrs
                       AND a~fictr   = b~fistl
                       AND b~hivarnt = c_hivarnt
                     INNER JOIN v_fmifi AS c
                        ON a~fictr = c~fistl
                     INNER JOIN fmci AS d
                        ON a~fikrs = d~fikrs
                       AND c~fipex = d~fipex
                       AND d~gjahr = '0000'
                     INNER JOIN fmcit AS e
                        ON d~fikrs = e~fikrs
                       AND d~fipex = e~fipex
                       AND d~gjahr = e~gjahr
                       AND e~spras = sy-langu
   WHERE a~fikrs     EQ p_fikrs
     AND a~fictr     IN s_fictr
     AND b~parent_st NE space
     AND c~gjahr     EQ p_gjahr
     AND c~fonds     IN s_geber
     AND c~fipex     IN s_fipex
     AND d~potyp     IN s_potyp.

  LOOP AT lt_v_fmifi.
    CLEAR: it_9000.

    MOVE: lt_v_fmifi-fictr TO it_9000-fictr,
          lt_v_fmifi-potyp TO it_9000-potyp,
          lt_v_fmifi-fipex TO it_9000-fipex,
          lt_v_fmifi-fonds TO it_9000-geber,
          lt_v_fmifi-bezei TO it_9000-bezei,
          fm01-waers       TO it_9000-waers.

    PERFORM get_budget_profile USING p_fikrs              lt_v_fmifi-posit
                                     lt_v_fmifi-ctr_objnr lt_v_fmifi-gjahr
                                     lt_v_fmifi-fonds     lt_v_fmifi-farea
                              CHANGING it_9000-profil.

    CHECK it_9000-profil IN s_profil.

    PERFORM set_actual_amount USING lt_v_fmifi-perio lt_v_fmifi-fkbtr.

    CASE lt_v_fmifi-wrttp.
      WHEN '50' OR '51' OR '60' OR '61'.
        it_9000-ctgry = c_commitment.     "Commitment
      WHEN '54' OR '66' OR '95'.
        IF lt_v_fmifi-btart EQ '0100'.
          it_9000-ctgry = c_invoice.        "Invoice
        ENDIF.
      WHEN '61'.
        IF lt_v_fmifi-btart EQ '0250'.
          it_9000-ctgry = c_payments.       "Payment
        ENDIF.
    ENDCASE.

    COLLECT it_9000.

*---// Calculate Carry forward
    CHECK lt_v_fmifi-wrttp EQ '50'   OR lt_v_fmifi-wrttp EQ '51'   OR
          lt_v_fmifi-btart EQ '0150' OR lt_v_fmifi-btart EQ '0350' OR
          lt_v_fmifi-btart EQ '0500'.


  ENDLOOP.
ENDFORM.                    " get_actual
*&---------------------------------------------------------------------*
*&      Form  set_actual_amount
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_actual_amount USING pv_perio pv_fkbtr.
  DATA: lv_amount(50),
        lv_month(2) TYPE n.

  FIELD-SYMBOLS: <amount>.

  MOVE: pv_perio TO lv_month.

  CONCATENATE 'IT_9000-WTP' lv_month INTO lv_amount.

  ASSIGN (lv_amount) TO <amount>.

  MOVE: pv_fkbtr TO <amount>.
ENDFORM.                    " set_actual_amount
*&---------------------------------------------------------------------*
*&      Form  set_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_text_n_total.
  LOOP AT it_9000.
    it_9000-total = it_9000-wtp01 + it_9000-wtp02 + it_9000-wtp03 +
                    it_9000-wtp04 + it_9000-wtp05 + it_9000-wtp06 +
                    it_9000-wtp07 + it_9000-wtp08 + it_9000-wtp09 +
                    it_9000-wtp10 + it_9000-wtp11 + it_9000-wtp12.

    CASE it_9000-ctgry.
      WHEN c_original.
        MOVE: text-b01 TO it_9000-cttxt.
      WHEN c_supplement.
        MOVE: text-b02 TO it_9000-cttxt.
      WHEN c_transfer.
        MOVE: text-b03 TO it_9000-cttxt.
      WHEN c_return.
        MOVE: text-b04 TO it_9000-cttxt.
      WHEN c_current.
        MOVE: text-b05 TO it_9000-cttxt.
      WHEN c_released.
        MOVE: text-b06 TO it_9000-cttxt.
      WHEN c_commitment.
        MOVE: text-b07 TO it_9000-cttxt.
      WHEN c_invoice.
        MOVE: text-b08 TO it_9000-cttxt.
      WHEN c_payments.
        MOVE: text-b09 TO it_9000-cttxt.
      WHEN c_available.
        MOVE: text-b10 TO it_9000-cttxt.
      WHEN c_unknown.
        MOVE: text-b11 TO it_9000-cttxt.
    ENDCASE.

    MODIFY it_9000.
  ENDLOOP.
ENDFORM.                    " set_text
*&---------------------------------------------------------------------*
*&      Form  get_available
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_available .
  DATA: lt_avail LIKE it_9000 OCCURS 0 WITH HEADER LINE.

  DATA: ls_9000 LIKE it_9000.

  SORT it_9000 BY geber fictr fipex ctgry.
  LOOP AT it_9000.
    MOVE: it_9000 TO ls_9000.

    AT NEW fipex.
      CLEAR: lt_avail.

      MOVE: ls_9000-geber  TO lt_avail-geber,
            ls_9000-fictr  TO lt_avail-fictr,
            ls_9000-fipex  TO lt_avail-fipex,
            ls_9000-bezei  TO lt_avail-bezei,
            ls_9000-potyp  TO lt_avail-potyp,
            ls_9000-profil TO lt_avail-profil,
            c_available    TO lt_avail-ctgry,
            fm01-waers     TO lt_avail-waers.
    ENDAT.

    CASE it_9000-profil.
      WHEN 'B'.
        PERFORM calculate_available_profile_b USING lt_avail.
      WHEN OTHERS.
        PERFORM calculate_available_others    USING lt_avail.
    ENDCASE.

    AT END OF fipex.
      APPEND lt_avail.
    ENDAT.
  ENDLOOP.

  APPEND LINES OF lt_avail TO it_9000.
ENDFORM.                    " get_available
*&---------------------------------------------------------------------*
*&      Form  get_budget_period
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_budget_profile USING   pv_fikrs pv_posit pv_objnr
                                pv_gjahr pv_geber pv_farea
                       CHANGING pv_profil.
  CALL FUNCTION 'KBPA_FIFM_GET_PROFIL'
    EXPORTING
      i_posit         = pv_posit
      i_objnr         = pv_objnr
      i_gjahr         = pv_gjahr
      i_geber         = pv_geber
      i_farea         = pv_farea
    IMPORTING
      e_profil        = pv_profil
    EXCEPTIONS
      no_profil_found = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
    CALL FUNCTION 'FM5B_GET_PROFILE'
      EXPORTING
        i_fikrs           = pv_fikrs
        i_fincode         = pv_geber
      IMPORTING
        e_profil          = pv_profil
      EXCEPTIONS
        fm_area_not_found = 1
        OTHERS            = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

ENDFORM.                    " get_budget_period
*&---------------------------------------------------------------------*
*&      Form  calculate_available_profile_b
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calculate_available_profile_b USING ps_avail STRUCTURE it_9000.
  CASE it_9000-ctgry.
    WHEN c_current.
      ps_avail-wtp01 = ps_avail-wtp01 + it_9000-wtp01.
      ps_avail-wtp02 = ps_avail-wtp02 + it_9000-wtp02.
      ps_avail-wtp03 = ps_avail-wtp03 + it_9000-wtp03.
      ps_avail-wtp04 = ps_avail-wtp04 + it_9000-wtp04.
      ps_avail-wtp05 = ps_avail-wtp05 + it_9000-wtp05.
      ps_avail-wtp06 = ps_avail-wtp06 + it_9000-wtp06.
      ps_avail-wtp07 = ps_avail-wtp07 + it_9000-wtp07.
      ps_avail-wtp08 = ps_avail-wtp08 + it_9000-wtp08.
      ps_avail-wtp09 = ps_avail-wtp09 + it_9000-wtp09.
      ps_avail-wtp10 = ps_avail-wtp10 + it_9000-wtp10.
      ps_avail-wtp11 = ps_avail-wtp11 + it_9000-wtp11.
      ps_avail-wtp12 = ps_avail-wtp12 + it_9000-wtp12.
    WHEN c_commitment OR c_invoice.
      ps_avail-wtp01 = ps_avail-wtp01 - it_9000-wtp01.
      ps_avail-wtp02 = ps_avail-wtp02 - it_9000-wtp02.
      ps_avail-wtp03 = ps_avail-wtp03 - it_9000-wtp03.
      ps_avail-wtp04 = ps_avail-wtp04 - it_9000-wtp04.
      ps_avail-wtp05 = ps_avail-wtp05 - it_9000-wtp05.
      ps_avail-wtp06 = ps_avail-wtp06 - it_9000-wtp06.
      ps_avail-wtp07 = ps_avail-wtp07 - it_9000-wtp07.
      ps_avail-wtp08 = ps_avail-wtp08 - it_9000-wtp08.
      ps_avail-wtp09 = ps_avail-wtp09 - it_9000-wtp09.
      ps_avail-wtp10 = ps_avail-wtp10 - it_9000-wtp10.
      ps_avail-wtp11 = ps_avail-wtp11 - it_9000-wtp11.
      ps_avail-wtp12 = ps_avail-wtp12 - it_9000-wtp12.
  ENDCASE.
ENDFORM.                    " calculate_available_profile_b
*&---------------------------------------------------------------------*
*&      Form  calculate_available_others
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_9000  text
*----------------------------------------------------------------------*
FORM calculate_available_others  USING ps_avail STRUCTURE it_9000.
  CASE it_9000-ctgry.
    WHEN c_released.
      ps_avail-wtp01 = ps_avail-wtp01 + it_9000-wtp01.
      ps_avail-wtp02 = ps_avail-wtp02 + it_9000-wtp02.
      ps_avail-wtp03 = ps_avail-wtp03 + it_9000-wtp03.
      ps_avail-wtp04 = ps_avail-wtp04 + it_9000-wtp04.
      ps_avail-wtp05 = ps_avail-wtp05 + it_9000-wtp05.
      ps_avail-wtp06 = ps_avail-wtp06 + it_9000-wtp06.
      ps_avail-wtp07 = ps_avail-wtp07 + it_9000-wtp07.
      ps_avail-wtp08 = ps_avail-wtp08 + it_9000-wtp08.
      ps_avail-wtp09 = ps_avail-wtp09 + it_9000-wtp09.
      ps_avail-wtp10 = ps_avail-wtp10 + it_9000-wtp10.
      ps_avail-wtp11 = ps_avail-wtp11 + it_9000-wtp11.
      ps_avail-wtp12 = ps_avail-wtp12 + it_9000-wtp12.
    WHEN c_commitment OR c_invoice.
      ps_avail-wtp01 = ps_avail-wtp01 - it_9000-wtp01.
      ps_avail-wtp02 = ps_avail-wtp02 - it_9000-wtp02.
      ps_avail-wtp03 = ps_avail-wtp03 - it_9000-wtp03.
      ps_avail-wtp04 = ps_avail-wtp04 - it_9000-wtp04.
      ps_avail-wtp05 = ps_avail-wtp05 - it_9000-wtp05.
      ps_avail-wtp06 = ps_avail-wtp06 - it_9000-wtp06.
      ps_avail-wtp07 = ps_avail-wtp07 - it_9000-wtp07.
      ps_avail-wtp08 = ps_avail-wtp08 - it_9000-wtp08.
      ps_avail-wtp09 = ps_avail-wtp09 - it_9000-wtp09.
      ps_avail-wtp10 = ps_avail-wtp10 - it_9000-wtp10.
      ps_avail-wtp11 = ps_avail-wtp11 - it_9000-wtp11.
      ps_avail-wtp12 = ps_avail-wtp12 - it_9000-wtp12.
  ENDCASE.
ENDFORM.                    " calculate_available_others
*&---------------------------------------------------------------------*
*&      Form  set_blank_category
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_blank_category .
  DATA: lt_9000 LIKE it_9000 OCCURS 0 WITH HEADER LINE.

  lt_9000[] = it_9000[].

  CLEAR: it_9000, it_9000[].

  SORT lt_9000 BY geber fictr fipex ctgry.

  DO.
    READ TABLE lt_9000 INDEX 1.
    IF sy-subrc NE 0.
      EXIT.
    ENDIF.

    DO c_available TIMES.
      READ TABLE lt_9000 WITH KEY geber = lt_9000-geber
                                  fictr = lt_9000-fictr
                                  fipex = lt_9000-fipex
                                  ctgry = sy-index.
      IF sy-subrc EQ 0.
        DELETE lt_9000 INDEX sy-tabix.

        CLEAR: it_9000.
        MOVE: lt_9000 TO it_9000.
        APPEND it_9000.
      ELSE.
        CLEAR: it_9000.
        MOVE: lt_9000-geber  TO it_9000-geber,
              lt_9000-fictr  TO it_9000-fictr,
              lt_9000-fipex  TO it_9000-fipex,
              lt_9000-bezei  TO it_9000-bezei,
              sy-index       TO it_9000-ctgry,
              lt_9000-profil TO it_9000-profil,
              lt_9000-potyp  TO it_9000-potyp,
              fm01-waers     TO it_9000-waers.
        APPEND it_9000.
      ENDIF.
    ENDDO.
  ENDDO.
ENDFORM.                    " set_blank_category
*&---------------------------------------------------------------------*
*&      Form  check_fikrs
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_fikrs .
  SELECT SINGLE * FROM fm01 WHERE fikrs = p_fikrs.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m03.
  ENDIF.
ENDFORM.                    " check_fikrs
*&---------------------------------------------------------------------*
*&      Form  SET_COLOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_COLOR  text
*----------------------------------------------------------------------*
FORM set_color.
  CASE it_9000-ctgry.
    WHEN c_orig_oth   OR c_supp_oth OR c_tran_oth OR
         c_retu_oth.
      FORMAT COLOR COL_NORMAL   INTENSIFIED OFF.
    WHEN c_curr_oth    OR c_rele_oth.
      FORMAT COLOR COL_NORMAL   INTENSIFIED ON.
    WHEN c_pr_oth OR c_po_oth OR c_park_oth OR c_down_oth OR
         c_invo_oth OR c_godi_oth .
      FORMAT COLOR COL_GROUP   INTENSIFIED OFF.
    WHEN c_paym_oth.
      FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
    WHEN c_avai_oth.
      FORMAT COLOR COL_TOTAL INTENSIFIED ON.
  ENDCASE.

ENDFORM.                    " SET_COLOR
*&---------------------------------------------------------------------*
*&      Form  display_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_rtn .
* BEGIN OF VALERIAN
  LOOP AT GT_LEAF.

    ICLEAR R_FIPEX.

    R_FIPEX-SIGN   = GT_LEAF-VALSIGN.
    R_FIPEX-OPTION = GT_LEAF-VALOPTION.
    R_FIPEX-LOW    = GT_LEAF-VALFROM.
    R_FIPEX-HIGH   = GT_LEAF-VALTO.
    APPEND R_FIPEX. CLEAR R_FIPEX.

    LOOP AT GT_9000 WHERE FIPEX IN R_FIPEX.
      IT_9000 = GT_9000.
      APPEND IT_9000.
    ENDLOOP.
  ENDLOOP.

  FREE GT_9000.
* END OF VALERIAN

  SORT it_9000 BY geber fictr fipex ctgry.

  LOOP AT it_9000.
    PERFORM display_key_fields.
    PERFORM display_line.

    AT END OF potyp. ULINE. ENDAT.
  ENDLOOP.
ENDFORM.                    " display_rtn
*&---------------------------------------------------------------------*
*&      Form  display_key_fields
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_CTGRY  text
*----------------------------------------------------------------------*
FORM display_key_fields.
  FORMAT COLOR COL_KEY INTENSIFIED ON.

  CASE it_9000-ctgry.
    WHEN '1'.
      WRITE:/      '|' NO-GAP,
              (16) it_9000-geber,
                   it_9000-fipex,
                   it_9000-profil NO-GAP,
                   '|' NO-GAP.
    WHEN '2'.
      WRITE:/      '|' NO-GAP,
                   it_9000-fictr,
              (24) it_9000-bezei,
              (06) it_9000-potyp  NO-GAP,
                   '|' NO-GAP.
    WHEN '3'.
      WRITE:/      '|' NO-GAP,
                   it_9000-ktext NO-GAP,
              (08) space NO-GAP,
                   '|' NO-GAP.
    WHEN OTHERS.
      WRITE:/      '|' NO-GAP,
              (48) space NO-GAP,
                   '|' NO-GAP.
  ENDCASE.
ENDFORM.                    " display_key_fields
*&---------------------------------------------------------------------*
*&      Form  display_line
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_line .
  PERFORM set_color.

  WRITE:       it_9000-cttxt,
               it_9000-total CURRENCY it_9000-waers NO-GAP,
               '|' NO-GAP,
          (12) it_9000-tslvt CURRENCY it_9000-waers,
          (12) it_9000-wtp01 CURRENCY it_9000-waers,
          (12) it_9000-wtp02 CURRENCY it_9000-waers,
          (12) it_9000-wtp03 CURRENCY it_9000-waers,
          (12) it_9000-wtp04 CURRENCY it_9000-waers,
          (12) it_9000-wtp05 CURRENCY it_9000-waers,
          (12) it_9000-wtp06 CURRENCY it_9000-waers,
          (12) it_9000-wtp07 CURRENCY it_9000-waers,
          (12) it_9000-wtp08 CURRENCY it_9000-waers,
          (12) it_9000-wtp09 CURRENCY it_9000-waers,
          (12) it_9000-wtp10 CURRENCY it_9000-waers,
          (12) it_9000-wtp11 CURRENCY it_9000-waers,
          (12) it_9000-wtp12 CURRENCY it_9000-waers NO-GAP,
               '|'.
ENDFORM.                    " display_line
*&---------------------------------------------------------------------*
*&      Form  display_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_header .
  RESERVE 10 LINES.

  SET LEFT SCROLL-BOUNDARY COLUMN 84.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED ON.

  WRITE:/01(252) text-h01 CENTERED.
  WRITE:/01(252) text-h06 CENTERED.
  SKIP.

  WRITE:   /2 text-h07, p_fikrs,
           50 text-h13, p_gjahr,
          100 text-h08, s_geber-low,  '~', s_geber-high.

  WRITE:   /2 text-h09, s_profil-low, '~', s_profil-high,
          100 text-h10, s_fictr-low,  '~', s_fictr-high.

  WRITE:   /2 text-h11, s_potyp-low NO-ZERO,  '~', s_potyp-high NO-ZERO,
          100 text-h12, s_fipex-low,  '~', s_fipex-high.

  FORMAT COLOR COL_HEADING INTENSIFIED ON.

  ULINE.
  WRITE:/ text-h02 NO-GAP,
          text-h03.
  WRITE:/ text-h04 NO-GAP,
          text-h05.
  ULINE.
ENDFORM.                    " display_header
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM download_rtn .
* MOVE: 'c:\temp\fm_actual.xls' TO v_fname.

  CALL SCREEN 9000 STARTING AT 20 5 ENDING AT 90 7.

  IF v_fname IS INITIAL.
    MOVE: 'c:\temp\fm_actual.xls' TO v_fname.
    EXIT.
  ENDIF.

  TRANSLATE v_fname TO UPPER CASE.
  REPLACE ALL OCCURRENCES OF '.XLS' IN v_fname WITH ''.

  PERFORM set_field_name.
  PERFORM set_download_data.

  CALL FUNCTION 'MS_EXCEL_OLE_STANDARD_DAT'
    EXPORTING
      file_name                 = v_fname
      create_pivot              = 0
    TABLES
      data_tab                  = it_download
      fieldnames                = it_field
    EXCEPTIONS
      file_not_exist            = 1
      filename_expected         = 2
      communication_error       = 3
      ole_object_method_error   = 4
      ole_object_property_error = 5
      invalid_pivot_fields      = 6
      download_problem          = 7
      OTHERS                    = 8.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " DOWNLOAD_RTN
*&---------------------------------------------------------------------*
*&      Form  set_field_name
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_field_name .
  CLEAR: it_field, it_field[].

  it_field-text = text-h14. APPEND it_field.
  it_field-text = text-h15. APPEND it_field.
  it_field-text = text-h16. APPEND it_field.
  it_field-text = text-h17. APPEND it_field.
  it_field-text = text-h35. APPEND it_field.
  it_field-text = text-h18. APPEND it_field.
  it_field-text = text-h19. APPEND it_field.
  it_field-text = text-h20. APPEND it_field.
  it_field-text = text-h21. APPEND it_field.
  it_field-text = text-h22. APPEND it_field.
  it_field-text = text-h23. APPEND it_field.
  it_field-text = text-h24. APPEND it_field.
  it_field-text = text-h25. APPEND it_field.
  it_field-text = text-h26. APPEND it_field.
  it_field-text = text-h27. APPEND it_field.
  it_field-text = text-h28. APPEND it_field.
  it_field-text = text-h29. APPEND it_field.
  it_field-text = text-h30. APPEND it_field.
  it_field-text = text-h31. APPEND it_field.
  it_field-text = text-h32. APPEND it_field.
  it_field-text = text-h33. APPEND it_field.
*  it_field-text = text-h34. APPEND it_field.
ENDFORM.                    " set_field_name
*&---------------------------------------------------------------------*
*&      Form  set_download_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_download_data .
  CLEAR: it_download[].

  LOOP AT it_9000.
    CLEAR: it_download.
    MOVE: it_9000-geber  TO it_download-geber,
          it_9000-fictr  TO it_download-fictr,
          it_9000-fipex  TO it_download-fipex,
          it_9000-bezei  TO it_download-bezei,
          it_9000-ktext  TO it_download-ktext,
          it_9000-profil TO it_download-profil,
          it_9000-cttxt  TO it_download-cttxt.

    WRITE: it_9000-total TO it_download-total CURRENCY it_9000-waers.
    PERFORM sign_in_front CHANGING  it_download-total.

    WRITE: it_9000-tslvt TO it_download-tslvt CURRENCY it_9000-waers.
    PERFORM sign_in_front CHANGING  it_download-tslvt.

    WRITE: it_9000-wtp01 TO it_download-wtp01 CURRENCY it_9000-waers.
    PERFORM sign_in_front CHANGING  it_download-wtp01.

    WRITE: it_9000-wtp02 TO it_download-wtp02 CURRENCY it_9000-waers.
    PERFORM sign_in_front CHANGING  it_download-wtp02.

    WRITE: it_9000-wtp03 TO it_download-wtp03 CURRENCY it_9000-waers.
    PERFORM sign_in_front CHANGING  it_download-wtp03.

    WRITE: it_9000-wtp04 TO it_download-wtp04 CURRENCY it_9000-waers.
    PERFORM sign_in_front CHANGING  it_download-wtp04.

    WRITE: it_9000-wtp05 TO it_download-wtp05 CURRENCY it_9000-waers.
    PERFORM sign_in_front CHANGING  it_download-wtp05.

    WRITE: it_9000-wtp06 TO it_download-wtp06 CURRENCY it_9000-waers.
    PERFORM sign_in_front CHANGING  it_download-wtp06.

    WRITE: it_9000-wtp07 TO it_download-wtp07 CURRENCY it_9000-waers.
    PERFORM sign_in_front CHANGING  it_download-wtp07.

    WRITE: it_9000-wtp08 TO it_download-wtp08 CURRENCY it_9000-waers.
    PERFORM sign_in_front CHANGING  it_download-wtp08.

    WRITE: it_9000-wtp09 TO it_download-wtp09 CURRENCY it_9000-waers.
    PERFORM sign_in_front CHANGING  it_download-wtp09.

    WRITE: it_9000-wtp10 TO it_download-wtp10 CURRENCY it_9000-waers.
    PERFORM sign_in_front CHANGING  it_download-wtp10.

    WRITE: it_9000-wtp11 TO it_download-wtp11 CURRENCY it_9000-waers.
    PERFORM sign_in_front CHANGING  it_download-wtp11.

    WRITE: it_9000-wtp12 TO it_download-wtp12 CURRENCY it_9000-waers.
    PERFORM sign_in_front CHANGING  it_download-wtp12.

    APPEND it_download.
  ENDLOOP.
ENDFORM.                    " set_download_data
*&---------------------------------------------------------------------*
*&      Module  fname_pov  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE fname_pov INPUT.
  DATA: lv_path  LIKE rlgrap-filename VALUE 'c:\'.

*  CALL FUNCTION 'C13G0_GET_FILENAME_F4'
*    EXPORTING
*      i_filepath    = lv_path
*      i_static      = ' '
**      i_mask        = '*.xls'
*    CHANGING
*      x_filename    = v_fname
*    EXCEPTIONS
*      mask_too_long = 1
*      OTHERS        = 2.
*  IF sy-subrc <> 0.
*    MESSAGE e000(zz) WITH text-m08.
*  ENDIF.

* 2007.08.23-hguybaek - Get File Name ??
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = ' '
    IMPORTING
      file_name     = v_fname.


ENDMODULE.                 " fname_pov  INPUT
*&---------------------------------------------------------------------*
*&      Form  CHECK_FILENAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_filename .
  IF v_fname IS INITIAL.
    MESSAGE s000(zz) WITH text-m07.
  ENDIF.

  LEAVE TO SCREEN 0.
ENDFORM.                    " CHECK_FILENAME
*&---------------------------------------------------------------------*
*&      Form  SIGN_IN_FRONT
*&---------------------------------------------------------------------*
FORM sign_in_front  CHANGING p_value.
  SHIFT p_value UP TO '-' LEFT CIRCULAR.
  CONDENSE p_value .
ENDFORM.                    " SIGN_IN_FRONT

*&---------------------------------------------------------------------*
*&      Form  GET_COMMITMENT_ITEM_GROUP
*&---------------------------------------------------------------------*
FORM GET_COMMITMENT_ITEM_GROUP .

  DATA : LT_NODE LIKE SETNODE OCCURS 0 WITH HEADER LINE.

  ICLEAR : GT_NODE, LT_NODE, GT_LEAF.

  SELECT * INTO TABLE LT_NODE
    FROM SETNODE
   WHERE SETCLASS = C_CLASS
     AND SUBCLASS = P_FIKRS
     AND SETNAME  = P_GRPID.

  CHECK SY-SUBRC = 0.
  APPEND LINES OF LT_NODE TO GT_NODE.

  DO.
    SELECT * INTO TABLE LT_NODE
    FROM SETNODE FOR ALL ENTRIES IN LT_NODE
   WHERE SETCLASS = LT_NODE-SUBSETCLS
     AND SUBCLASS = LT_NODE-SUBSETSCLS
     AND SETNAME  = LT_NODE-SUBSETNAME.

    IF SY-SUBRC NE 0.
      EXIT.
    ENDIF.
    APPEND LINES OF LT_NODE TO GT_NODE.
  ENDDO.

  SELECT * INTO TABLE GT_LEAF
    FROM SETLEAF FOR ALL ENTRIES IN GT_NODE
   WHERE SETCLASS = GT_NODE-SUBSETCLS
     AND SUBCLASS = GT_NODE-SUBSETSCLS
     AND SETNAME  = GT_NODE-SUBSETNAME.

ENDFORM.                    " GET_COMMITMENT_ITEM_GROUP
