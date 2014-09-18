************************************************************************
* Program Name      : ZIPP603I_PRESS_PR
* Author            : HyungYul, Kim
* Creation Date     : 2003.09.03.
* Specifications By : HyungYul, Kim
* Pattern           : 5.1.4.1
* Development Request No : UD1K902114
* Addl Documentation:
* Description       : Transfer of Press Production Result
*                     From MES to SAP [BDC & UPDATE]
* Modification Logs
* Date       Developer    RequestNo    Description
*10/11/2004  Yongping Li  UD1K912811   Object Lock when using BAPI to
**                                     confirm the prodution order.
*                                      Change the code to re-arange the
*                                      order and delay if neccessory.
* 04/26/2007 Manju        UD1K940429   Add posting logic for Reporting
*                                      point RP16.
*
* 07/30/2007 IG.MOON      UD1K941154   Add Blank Rework Scrap
************************************************************************
REPORT zipp603i_press_pr  NO STANDARD PAGE HEADING
                          LINE-SIZE 1023
                          MESSAGE-ID zmpp.

************************************************************************
* INCLUDE
************************************************************************
INCLUDE zipp603l_press_pr_top.
INCLUDE zipp603l_press_pr_f.

************************************************************************
* INITIALIZAION
************************************************************************
INITIALIZATION.

************************************************************************
* TOP-OF-PAGE.
************************************************************************
TOP-OF-PAGE.

************************************************************************
* AT SELECTION-SCREEN
************************************************************************
AT SELECTION-SCREEN.
  PERFORM at_selection_screen.

************************************************************************
* START-OF-SELECTION
************************************************************************
START-OF-SELECTION.
  PERFORM execute_process.
************************************************************************
* END-OF-SELECTION
************************************************************************
END-OF-SELECTION.
*  PERFORM LIST_PROCESS.
*&---------------------------------------------------------------------*
*&      Form  MB1A_GI_R23
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_PRPID  text
*----------------------------------------------------------------------*
FORM mb1a_gi_r23 USING    lp_prpid.
  CLEAR : goodsmvt_header, it_goodsmvt_item,
          it_goodsmvt_item[].

*-----> GOODS MOVEMENT HEADER

  goodsmvt_header-pstng_date = wa_datum       .
  goodsmvt_header-header_txt = it_ztpppr-rtime.
*  GM_Code 01: Goods receipt for purchase order
*  GM_Code 02: Goods receipt for production order
*  GM_Code 03: Goods issue
*  GM_Code 04: Transfer posting
*  GM_Code 05: Other goods receipts
*  GM_Code 06: Reversal of goods movements
  goodsmvt_code = '03'.

  CLEAR : ztpp_mip_stk_tra, mkal, marc.

  SELECT SINGLE *
            FROM mkal
            WHERE werks EQ 'P001'
              AND matnr EQ it_ztpppr-pnlno
              AND verid EQ '01'.
  IF sy-subrc NE 0.
    SELECT SINGLE * FROM mara WHERE matnr EQ it_ztpppr-pnlno.
    IF sy-subrc EQ 0.
      MOVE: 'E'      TO wa_return-type,
            text-m01 TO wa_return-message.
    ELSE.
      MOVE: 'E'      TO wa_return-type,
            text-m02 TO wa_return-message.
    ENDIF.

    EXIT.
  ENDIF.

  IF it_ztpppr-eusage EQ '01'.
    SELECT SINGLE *
           FROM ztpp_mip_stk_tra
           WHERE werks  EQ mkal-werks
             AND eusage EQ it_ztpppr-eusage.
    it_goodsmvt_item-move_type  = ztpp_mip_stk_tra-bwart."MVT type
    it_goodsmvt_item-move_stloc = ztpp_mip_stk_tra-lgort.
    it_goodsmvt_item-stge_loc   = mkal-alort.        "FROM SLOC
  ELSE.
    SELECT SINGLE *
              FROM marc
              WHERE werks EQ 'P001'
                AND matnr EQ it_ztpppr-pnlno.
    it_goodsmvt_item-move_type  = '551'.
  ENDIF.
  IF lp_prpid EQ 'R23'.
    it_goodsmvt_item-stge_loc = 'P230'.           "TO SLOC
  ELSEIF lp_prpid EQ 'R14'.
    it_goodsmvt_item-stge_loc = 'P120'.
  ELSEIF lp_prpid EQ 'R15'.
*    it_goodsmvt_item-stge_loc = 'P121'.
    it_goodsmvt_item-stge_loc = 'P120'.

  ENDIF.

* it_goodsmvt_item-stge_loc   = mkal-alort.             "FROM SLOC

  it_goodsmvt_item-material   = it_ztpppr-pnlno.        "MATERIAL
  it_goodsmvt_item-plant      = mkal-werks.             "PLANT
  it_goodsmvt_item-entry_qnt  = it_ztpppr-psqty.        "SCRAP QTY
  it_goodsmvt_item-entry_uom  = it_ztpppr-meins.        "UoM
  it_goodsmvt_item-withdrawn = 'Y'.
  it_goodsmvt_item-orderid = 'CP001'.
  APPEND it_goodsmvt_item.

  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
    EXPORTING
      goodsmvt_header             = goodsmvt_header
      goodsmvt_code               = goodsmvt_code
    IMPORTING
      materialdocument            = it_ztpppr-mblnr
      matdocumentyear             = it_ztpppr-mjahr
    TABLES
      goodsmvt_item               = it_goodsmvt_item
*      GOODSMVT_SERIALNUMBER       =
      return                      = it_return.

  LOOP AT it_return.
    MOVE-CORRESPONDING it_return TO wa_return.
  ENDLOOP.

ENDFORM.                    " MB1A_GI_R23
*&---------------------------------------------------------------------*
*&      Form  pr_cid_r04
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ZTPPPR_PRPID  text
*----------------------------------------------------------------------*
FORM pr_cid_r04 USING p_prpid.

  IF it_ztpppr-psqty  > 0. " Yield
    CLEAR : goodsmvt_header, it_goodsmvt_item,
            it_goodsmvt_item[].

*-----> GOODS MOVEMENT HEADER
    goodsmvt_header-pstng_date = wa_datum. " Reporting date
    goodsmvt_header-doc_date   =   wa_datum. " System Date
    goodsmvt_header-header_txt = 'Rework Scrap'.  " Header text
*  GM_Code 01: Goods receipt for purchase order
*  GM_Code 02: Goods receipt for production order
*  GM_Code 03: Goods issue
*  GM_Code 04: Transfer posting
*  GM_Code 05: Other goods receipts
*  GM_Code 06: Reversal of goods movements
    goodsmvt_code = '03'.

    CLEAR : ztpp_mip_stk_tra, mkal, marc.

    SELECT SINGLE *
              FROM mkal
              WHERE werks EQ 'P001'
                AND matnr EQ it_ztpppr-pnlno
                AND verid EQ '01'.
    IF sy-subrc NE 0.
      SELECT SINGLE * FROM mara WHERE matnr EQ it_ztpppr-pnlno.
      IF sy-subrc EQ 0.
        MOVE: 'E'      TO wa_return-type,
              text-m01 TO wa_return-message.
      ELSE.
        MOVE: 'E'      TO wa_return-type,
              text-m02 TO wa_return-message.
      ENDIF.

      EXIT.
    ENDIF.

    it_goodsmvt_item-move_type  = '551'.    "Movement Type
    it_goodsmvt_item-stge_loc   = 'P110'.   "TO SLOC
    it_goodsmvt_item-move_reas  = '9800'.   "Reason code

    it_goodsmvt_item-material   = it_ztpppr-pnlno.        "MATERIAL
    it_goodsmvt_item-plant      = mkal-werks.             "PLANT
    it_goodsmvt_item-entry_qnt  = it_ztpppr-psqty.       "QTY
    it_goodsmvt_item-entry_uom  = it_ztpppr-meins.       "UoM
    it_goodsmvt_item-orderid  = it_ztpppr-pprdno.        "UoM
    it_goodsmvt_item-withdrawn = 'Y'.
    APPEND it_goodsmvt_item.


    CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
         EXPORTING
              goodsmvt_header  = goodsmvt_header
              goodsmvt_code    = goodsmvt_code
         IMPORTING
              materialdocument = it_ztpppr-mblnr
              matdocumentyear  = it_ztpppr-mjahr
         TABLES
              goodsmvt_item    = it_goodsmvt_item
              return           = it_return.

    LOOP AT it_return.
      MOVE-CORRESPONDING it_return TO wa_return.
    ENDLOOP.

*-----Start
*  ELSEIF it_ztpppr-psqty > 0. "scrap
*    PERFORM mb1a_gi_r23 USING p_prpid.
*-----End
  ENDIF.

  IF wa_return-type NE 'E'.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
         EXPORTING
              wait = 'X'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
    wa_return-type = 'E'.
  ENDIF.


ENDFORM.                    " pr_cid_r04
