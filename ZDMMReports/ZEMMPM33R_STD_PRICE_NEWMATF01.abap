************************************************************************
* Program Name : ZEMMPM33R_STD_PRICE_NEWMAT
* Created by   : Min-su Park
* Created on   : 2003.10.17.
* Pattern      :
* Description  : Manage Standard Price for NEW Purchase Material
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2003.10.17.     Min-su Park    UD1K901873     Initial Coding
************************************************************************

*----------------------------------------------------------------------*
*   INCLUDE ZEMMPM33R_STD_PRICE_NEWMATF01                              *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  UPDATE_MASTER_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_master_data USING it_mara STRUCTURE mara.
  DATA: wa_head       LIKE bapimathead, "Header with control information
        wa_plant      LIKE bapi_marc  , "plant-specific material DATA
        wa_plantx     LIKE bapi_marcx ,
        wa_mbew       LIKE bapi_mbew  ,
        wa_mbewx      LIKE bapi_mbewx .
  DATA: it_bapiret2   LIKE bapiret2 OCCURS 0 WITH HEADER LINE.

  wa_head-material     = it_mara-matnr.
  wa_mbew-val_area     = it_info-werks.
  wa_mbew-plndprice1   = w_effpr      .
  wa_mbew-plndprdate1  = sy-datum     .
  wa_mbewx-val_area    = it_info-werks.
  wa_mbewx-plndprice1  = 'X'          .
  wa_mbewx-plndprdate1 = 'X'          .

  CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
       EXPORTING
            headdata       = wa_head
            valuationdata  = wa_mbew
            valuationdatax = wa_mbewx
       TABLES
            returnmessages = it_bapiret2.

  READ TABLE it_bapiret2 WITH KEY type = 'E'.
  IF sy-subrc = 0.  "Error Occurred !
    MOVE-CORRESPONDING it_bapiret2 TO it_log.
    it_log-numb  = it_bapiret2-number.
    it_log-serial = it_log-serial + 1.
    it_log-matnr  = it_mara-matnr.
    it_log-tcode = sy-tcode.
    APPEND it_log.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
*     EXPORTING
*       WAIT          =
*     IMPORTING
*       RETURN        =    .
    PERFORM get_ztmm_analy .
  ENDIF.

*  DATA : STATUS VALUE 'N'.
*  DATA : EFFPR1(14), FDATE(10).
*  WRITE : W_EFFPR TO EFFPR1 CURRENCY 'USD',
*          SY-DATUM TO FDATE             .
*  CLEAR   : IT_BDC, IT_MESSAGE.
*  REFRESH : IT_BDC, IT_MESSAGE.
*  PERFORM BDC_PASS USING:
*       'X' 'SAPLMGMM'     '0060'       ,
*       ' ' 'RMMG1-MATNR'  IT_MARA-MATNR,
*       ' ' 'BDC_OKCODE'   '=AUSW'      .
*
*  PERFORM BDC_PASS USING:
*       'X' 'SAPLMGMM'     '0070'       ,
*       ' ' 'MSICHTAUSW-KZSEL(18)' 'X'  ,
*       ' ' 'BDC_OKCODE'   '=ENTR'      .
*
*  PERFORM BDC_PASS USING:
*       'X' 'SAPLMGMM'     '0080'       ,
*       ' ' 'RMMG1-WERKS'  IT_INFO-WERKS,
*       ' ' 'BDC_OKCODE'   '=ENTR'      .
*  CASE W_MARK.
*    WHEN R1.
*      PERFORM BDC_PASS USING:
*         'X' 'SAPLMGMM'     '4000'       ,
*         ' ' 'MBEW-ZPLP3'   EFFPR1       ,
*         ' ' 'MBEW-ZPLD3'   FDATE        ,
*         ' ' 'BDC_OKCODE'   '=BU'        .
*
*    WHEN R2.
*      PERFORM BDC_PASS USING:
*           'X' 'SAPLMGMM'     '4000'       ,
*           ' ' 'MBEW-ZPLP1'   EFFPR1       ,
*           ' ' 'MBEW-ZPLD1'   FDATE        ,
*           ' ' 'BDC_OKCODE'   '=BU'        .
*  ENDCASE.
*  CALL TRANSACTION 'MM02'
*           USING IT_BDC
*           MODE STATUS
*           UPDATE'S'
*           MESSAGES INTO IT_MESSAGE.
ENDFORM.                    " UPDATE_MASTER_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_ZTMM_ANALY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_ztmm_analy.
  it_analy-base_d  = p_date       . "Base date
  it_analy-lifnr   = it_a017-lifnr.
  it_analy-werks   = it_a017-werks. "Plant
  it_analy-matnr   = it_mara-matnr. "Material Number
  it_analy-ekorg   = it_a017-ekorg.
*                                   "Reason Code
  SELECT SINGLE kzust
           INTO it_analy-kzust
           FROM konh
          WHERE knumh = it_a017-knumh.

  it_analy-valid_d = it_a017-datab. "Valid from
*                                   "Price
*  SELECT SINGLE kbetr konwa
*         INTO (it_analy-kbetr, it_analy-konwa)
*         FROM konp
*        WHERE knumh = it_a017-knumh
*          AND kschl = 'PB00'      .
*  it_analy-source  = 'I'          . "SOURCE
*  it_analy-erdat   = sy-datum     . "WORK DAY(Creation Day)
*
*  it_analy-erzet   = sy-uzeit     .
*  it_analy-ernam   = sy-uname     .
*  APPEND it_analy.
ENDFORM.                    " GET_ZTMM_ANALY
