*============
* THIS IS FOR TESTING PURPOSE
*  ; ref PPC_INTERNAL_TEST_BACKFLUSH
*============
* spec by andy choi
REPORT  zdi_bf_pre MESSAGE-ID ppc1pr.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-sl1.
PARAMETERS:
  p_plnum LIKE ppc_comp_conf-plnum OBLIGATORY MEMORY ID paf,
*-external order number is from planned order
  p_plext TYPE ppc_ordernr,
  p_repnt TYPE ppc_reppoint_ext OBLIGATORY,
  p_confq TYPE ppc_headconfquant DEFAULT 1.

PARAMETERS:
  p_budat  LIKE ppc_comp_conf-budat MEMORY ID ks8 DEFAULT sy-datum OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-sl2.
PARAMETERS:
  p_normal RADIOBUTTON GROUP proc,
  p_rev    RADIOBUTTON GROUP proc,
  p_fscrap RADIOBUTTON GROUP proc.

PARAMETERS: p_scrapr TYPE ppc_scrapreason,
            p_scrapt TYPE ppc_scraptext.
SELECTION-SCREEN END OF BLOCK b2.

*Reversal...
* if p_rptly checked, keep only the components confirmed at this reporting point
* others, keep all the components confirmed UNTIL this reporting point
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-sl3.
PARAMETERS:
  p_ppc   TYPE char1  DEFAULT 'X' NO-DISPLAY,
  p_rptly TYPE char1  DEFAULT 'X' NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-sl4.
SELECTION-SCREEN END OF BLOCK b4.

SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE text-sl5.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN:  POSITION 20,
                   COMMENT 1(25) text-m01
                   FOR FIELD p_gr_ind.
PARAMETERS: p_gr_ind LIKE ppc_comp_conf-gr_ind.

SELECTION-SCREEN: POSITION 36,
                  COMMENT 36(20) text-m03.
PARAMETERS: p_sobkz type sobkz.
SELECTION-SCREEN END OF LINE.

PARAMETERS:
p_gi_ind LIKE ppc_comp_conf-gi_ind DEFAULT 'X',
p_ac_ind LIKE ppc_comp_conf-gi_ind DEFAULT 'X'.  "Activity posting
*  p_alort  like ppc_comp_conf-alort.
*  p_last  like ppc_comp_conf-stsor default '18' obligatory.
PARAMETERS: p_test AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b5.

*P_BUDAT  Posting Date
*P_CONFQ  Backfl.Qty for HeadMat
*P_PLNUM  Planned order
*P_PLEXT  External order number
*P_PPC  Reversed based on PPC
*P_GI_IND	Goods issue for components
*P_GR_IND	Goods receipt for assembly
*P_AC_IND	Activity posting
*P_REPNT  Reporting Point
*P_REV  Post reversal
*P_RPTLY  Reporting Point only(Reversal)
*P_TEST	Test Run

DATA: ls_head TYPE bapi_ppc_apoheads.

* --------------------------------------------------------------------- *
DATA: h_dontpanic   LIKE sy-datlo.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-name = 'P_TEST'.
      GET PARAMETER ID 'DONTPANIC' FIELD h_dontpanic.
      IF h_dontpanic = sy-datlo.
        screen-input = '1'.
      ELSE.
*        screen-input = '0'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.


*&---------------------------------------------------------------------*
START-OF-SELECTION.
*&---------------------------------------------------------------------*
  DATA: l_return1 LIKE  bapiret1.
  DATA: l_return2 LIKE  bapiret2.
  DATA: BEGIN OF ls_plaf,
          plnum  TYPE plnum,
          matnr  TYPE plmat,
          plwrk  TYPE plwrk,
          pwwrk  TYPE pwwrk,
          kdauf  TYPE kdauf,
          kdpos  TYPE kdpos,
        END OF ls_plaf.
  DATA: lv_reppoint LIKE ppc_rp-reppoint.

*  if p_plext is not initial.
*- read vehicle master and get planned order # ???
  SELECT SINGLE plnum matnr plwrk pwwrk kdauf kdpos
         INTO ls_plaf FROM plaf
           WHERE plnum = p_plnum.
  CHECK sy-subrc = 0.


* check reporting point
  SELECT SINGLE reppoint INTO lv_reppoint FROM ppc_rp
          WHERE reppoint_ext = p_repnt.
  CHECK sy-subrc = 0.
  ls_head-reppoint  = lv_reppoint.
  ls_head-orderid   = p_plnum.
  IF p_plext = SPACE.
    ls_head-ordernr   = p_plnum.
  ELSE.
    ls_head-ordernr   = p_plext.
  ENDIF.

* check b/f exist
  data: iv_confq type PPC_HEADCONFQUANT.
  CALL FUNCTION 'Z_PPC1TP_ORD_RP_CHECK'
    EXPORTING
      p_plnum           = p_plnum
      p_rpext           = p_repnt
    IMPORTING
      E_CONFQ           = iv_confq
    EXCEPTIONS
      ORDER_ERROR       = 1.
  if iv_confq > 0.
    write: 'Warning:', p_plnum, p_repnt, ' had backflush count ', iv_confq.
  endif.

*  ls_head-REPPOINT      = p_repnt  "external RP
  ls_head-pstng_date = p_budat.
  ls_head-flg_scrap      = p_fscrap.
  IF p_fscrap EQ 'X'.
    ls_head-scrapreason  = p_scrapr.
    ls_head-scraptext    = p_scrapt.
  ENDIF.
  ls_head-flg_reversal   = p_rev.
  ls_head-confquant      = p_confq.
  ls_head-confunit       = 'EA'.

  ls_head-sales_ord      = ls_plaf-kdauf.
  ls_head-sales_ord_item = ls_plaf-kdpos.

  CALL FUNCTION 'Z_PPC_RP_CONFIRMATION'
    EXPORTING
      p_head   = ls_head
      p_sobkz  = p_sobkz
      p_gr_ind = p_gr_ind
      p_gi_ind = p_gi_ind
      p_ac_ind = p_ac_ind
      p_rptly  = p_rptly
      p_ppc    = p_ppc
      p_test   = p_test
    IMPORTING
      return   = l_return2.

  WRITE:/ '***RESULT***'.
  WRITE:/ l_return2-type, l_return2-id, l_return2-number,
          l_return2-message.


END-OF-SELECTION.
