FUNCTION z_fmm_6010_03.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(IM_ZDOCNO) TYPE  NUM10
*"     VALUE(IMS_IDITEMS_WITHOUT_POST) TYPE  ZSMM_6010_01
*"----------------------------------------------------------------------

**/ App Doc No
*  PERFORM number_get_next USING    nro_nr_09     "NRO Interval
*                                   nro_object    "NRO Object
*                          CHANGING w_zdocno.     "App Doc No
*  COMMIT WORK.
************************************************************************
  DATA: wa_iditems_without_post LIKE ims_iditems_without_post.
  DATA: it_iditems_without_post LIKE TABLE OF wa_iditems_without_post.

******** Make goodsmvt_item
  CLEAR: it_goodsmvt_item, wa_goodsmvt_item.
  wa_goodsmvt_item-material  =
           ims_iditems_without_post-matnr.  "'85850-3K100'.

  wa_goodsmvt_item-move_type =
           ims_iditems_without_post-bwart.  "Movement type "'101'.
  wa_goodsmvt_item-vendor    =
           ims_iditems_without_post-lifnr.  "'0000400016'.
  wa_goodsmvt_item-entry_qnt =
           ims_iditems_without_post-lfimg.                  "'1000'.
  wa_goodsmvt_item-entry_uom =
           ims_iditems_without_post-meins.  "'EA'.
  wa_goodsmvt_item-po_number =
           ims_iditems_without_post-vgbel.  "'4200000239'.  (POno)
  wa_goodsmvt_item-po_item   =
           ims_iditems_without_post-vgpos.                  "'00001'.

*  wa_goodsmvt_item-deliv_numb_to_search =   "h important
*           ims_iditems_without_post-vbeln.  "Inbound Delivery
*  wa_goodsmvt_item-deliv_item_to_search   =  "h important
*           ims_iditems_without_post-posnr.  "Inbound Delivery Item

  wa_goodsmvt_item-deliv_numb =
           ims_iditems_without_post-vbeln.  "Inbound Delivery
  wa_goodsmvt_item-deliv_item   =
           ims_iditems_without_post-posnr.  "Inbound Delivery Item


  wa_goodsmvt_item-mvt_ind   = 'B'.
  "Goods receipt for purchase order
  APPEND wa_goodsmvt_item TO it_goodsmvt_item.

********* Make goodsmvt_header
  wa_goodsmvt_header-pstng_date = sy-datum.
  wa_goodsmvt_header-doc_date   = sy-datum.
  wa_goodsmvt_header-ref_doc_no =
              ims_iditems_without_post-vbeln.  " '180000335'.
********* Make goodsmvt_code
  wa_goodsmvt_code-gm_code = '01'.
  "Goods receipt for purchase order
********* Execute BAPI for Post
  PERFORM bapi_goodsmvt_create TABLES   it_goodsmvt_item
                                        it_bapiret2
                               USING    wa_goodsmvt_header
                                        wa_goodsmvt_code
                               CHANGING wa_goodsmvt_headret.
*
**** (Begin)BAPI Log to the table ZTLOG
  IF it_bapiret2 IS INITIAL.  "SUCCESS
    CLEAR: wa_bapiret2.
    wa_bapiret2-type = 'S'.  "SUCCESS
    wa_bapiret2-id         = 'ZMMM'.
    wa_bapiret2-number     = '999'.
    wa_bapiret2-message_v1 = 'Material document'.
    wa_bapiret2-message_v2 = wa_goodsmvt_headret-mat_doc.
    wa_bapiret2-message_v3 = wa_goodsmvt_headret-doc_year.
    wa_bapiret2-message_v4 = 'is created.'.
    APPEND wa_bapiret2 TO it_bapiret2.
  ENDIF.

  DATA: logno_h       TYPE num10.
  DATA: lv_ztcode     TYPE tcode.
  DATA: lv_zprogramm  TYPE programm.
  DATA: lv_tcode      TYPE tcode.
  DATA: lv_fm_name    TYPE rs38l_fnam.

  lv_ztcode    = sy-tcode.
  lv_zprogramm = sy-cprog.
  lv_tcode     = 'MIGO_GR'.
  lv_fm_name   = 'BAPI_GOODSMVT_CREATE'.

  CALL FUNCTION 'Z_FMM_6001_03_LOG_TO_ZTABLE'
    IN UPDATE TASK
    EXPORTING
      im_zdocno            = im_zdocno
      im_ztcode            = lv_ztcode
      im_zprogramm         = lv_zprogramm
      im_tcode             = lv_tcode
      im_fm_name           = lv_fm_name
   TABLES
*     imt_bdcmsgcoll       = it_bdcmsgcoll
     imt_bapiret2         = it_bapiret2.
  COMMIT WORK.
**** (End)BAPI Log to the table ZTLOG

ENDFUNCTION.
