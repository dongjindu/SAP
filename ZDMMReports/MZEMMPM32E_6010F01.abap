*----------------------------------------------------------------------*
*   INCLUDE MZEMMPM32E_6010F01                                         *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  desc_bwlvs
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LTAK_BWLVS  text
*      <--P_T333T_LBWAT  text
*----------------------------------------------------------------------*
FORM desc_bwlvs USING    value(p_spras)   "Language key
                         value(p_lgnum)   "Warehouse Number
                         value(p_bwlvs)   "Movement type
                CHANGING value(p_lbwat).  "Movement type description
  CLEAR: p_lbwat.
  SELECT SINGLE lbwat INTO p_lbwat
    FROM t333t
    WHERE spras = p_spras AND
          lgnum = p_lgnum AND
          bwlvs = p_bwlvs.
ENDFORM.                    " desc_bwlvs
*&---------------------------------------------------------------------*
*&      Form  desc_letyp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_LANGU  text
*      -->P_LTAK_LGNUM  text
*      -->P_LEIN_LETYP  text
*      <--P_T307T_LETYP  text
*----------------------------------------------------------------------*
FORM desc_letyp USING    value(p_spras)
                         value(p_lgnum)
                         value(p_letyp)
                CHANGING value(p_desc_letyp).
  CLEAR: p_desc_letyp.
  SELECT SINGLE letyt INTO p_desc_letyp
    FROM t307t
    WHERE spras = p_spras AND
          lgnum = p_lgnum AND
          letyp = p_letyp.

ENDFORM.                    " desc_letyp
*&---------------------------------------------------------------------*
*&      Form  desc_nltyp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_LANGU  text
*      -->P_LTAK_LGNUM  text
*      -->P_LEIN_LETYP  text
*      <--P_T301T_LTYPT  text
*----------------------------------------------------------------------*
FORM desc_nltyp USING    value(p_spras)
                         value(p_lgnum)
                         value(p_nltyp)
                CHANGING value(p_ltypt).
  CLEAR: p_ltypt.
  SELECT SINGLE ltypt INTO p_ltypt
    FROM t301t
    WHERE spras = p_spras AND
          lgnum = p_lgnum AND
          lgtyp = p_nltyp.

ENDFORM.                    " desc_nltyp
*&---------------------------------------------------------------------*
*&      Form  get_rearcharacters
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LEIN_LENUM  text
*      -->P_10     text
*      <--P_LV_LENUM  text
*----------------------------------------------------------------------*
FORM get_rearcharacters USING    value(p_f)
                                 value(p_rearcharacters_no)
                        CHANGING value(p_rearcharacters).
* By Hakchin Kim
  DATA l_offset TYPE i.
  l_offset = strlen( p_f ) - p_rearcharacters_no.
  MOVE p_f+l_offset(p_rearcharacters_no) TO p_rearcharacters.
  WRITE:/ p_rearcharacters.
ENDFORM.                    "get_rearcharacters
*&---------------------------------------------------------------------*
*&      Form  check_lenum
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LEIN_LENUM  text
*----------------------------------------------------------------------*
FORM check_lenum USING value(p_lenum).
  DATA: ls_lein LIKE lein.
  SELECT SINGLE * INTO ls_lein
    FROM lein
    WHERE lenum = p_lenum.
ENDFORM.                    " check_lenum
*&---------------------------------------------------------------------*
*&      Form  check_letyp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LEIN_LETYP  text
*----------------------------------------------------------------------*
FORM check_letyp USING    value(p_lgnum)
                          value(p_letyp).
  DATA: ls_t307 LIKE t307.
  SELECT SINGLE * INTO ls_t307
    FROM t307
    WHERE lgnum = p_lgnum    AND
          letyp = p_letyp.
ENDFORM.                    " check_letyp
*&---------------------------------------------------------------------*
*&      Form  qty_comparison
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_LIPS_VGBEL  text
*      -->P_LS_LIPS_VGPOS  text
*      <--P_w_qty_message  text
*----------------------------------------------------------------------*
FORM qty_comparison USING    value(p_ebeln)
                             value(p_ebelp)
                    CHANGING value(p_w_qty_message) TYPE char2.
  CLEAR: p_w_qty_message.
  DATA: lv_ekpo_menge LIKE ekpo-menge.
  DATA: lv_ekes_menge LIKE ekes-menge.
  SELECT SINGLE menge INTO lv_ekpo_menge
    FROM ekpo
    WHERE ebeln = p_ebeln AND
          ebelp = p_ebelp.
  SELECT SINGLE menge INTO lv_ekes_menge
    FROM ekes
    WHERE ebeln = p_ebeln AND
          ebelp = p_ebelp AND
          etens = '0001'.
  IF lv_ekpo_menge = lv_ekes_menge.
    p_w_qty_message = 'EQ'.
  ELSE.
    p_w_qty_message = 'NE'.
  ENDIF.
ENDFORM.                    "qty_comparison
*&---------------------------------------------------------------------*
*&      Form  iditems_without_post
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_iditems_without_post  text
*      -->P_LIPS_KDMAT  text
*----------------------------------------------------------------------*
FORM iditems_without_post
   TABLES ext_iditems_without_post
       STRUCTURE wa_iditems_without_post
   USING value(p_kdmat) LIKE lips-kdmat. "Vendor Mat. no.

  SELECT lips~vbeln lips~posnr lips~matnr
         lips~werks    "Plant
         lips~lgort    "Storage location
         lips~bwart
         likp~lifnr
         lips~lfimg lips~meins
         lips~vgbel lips~vgpos
         vbup~kosta vbup~wbsta
    INTO CORRESPONDING FIELDS OF TABLE ext_iditems_without_post
    FROM lips
      INNER JOIN likp
        ON likp~vbeln = lips~vbeln
      INNER JOIN vbup
        ON vbup~vbeln = lips~vbeln AND
           vbup~posnr = lips~posnr AND
           vbup~kosta <> space     AND  "Picking status/Putaway status
           vbup~wbsta = 'A'             "GdsMvtStat (Not Yet Processed)
    WHERE lips~kdmat = p_kdmat.   "Vendor Mat. no.
*   Not relevant
*A  Not yet processed
*B  Partially processed
*C  Completely processed
ENDFORM.                    "iditems_without_post
*&---------------------------------------------------------------------*
*&      Form  bapi_goodsmvt_create
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bapi_goodsmvt_create
      TABLES   imt_goodsmvt_item
                   STRUCTURE bapi2017_gm_item_create
               ext_return
                   STRUCTURE bapiret2
      USING    value(im_goodsmvt_header)  LIKE bapi2017_gm_head_01
               value(im_goodsmvt_code)    LIKE bapi2017_gm_code
      CHANGING value(ex_goodsmvt_headret) LIKE bapi2017_gm_head_ret.

  CLEAR: ext_return, ext_return[].
  CLEAR: ex_goodsmvt_headret.
  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
    EXPORTING
      goodsmvt_header             = im_goodsmvt_header
      goodsmvt_code               = im_goodsmvt_code
*    TESTRUN                     = 'X'
    IMPORTING
      goodsmvt_headret            = ex_goodsmvt_headret
*   MATERIALDOCUMENT            =
*   MATDOCUMENTYEAR             =
    TABLES
      goodsmvt_item               = imt_goodsmvt_item
*   GOODSMVT_SERIALNUMBER       =
      return                      = ext_return.

  CLEAR: ext_return.
  READ TABLE ext_return WITH KEY type = 'E'.
  IF sy-subrc = 0.  "Error Occurred !
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
         EXPORTING
              wait = 'X'.
*         IMPORTING
*              return =.
  ENDIF.
ENDFORM.                    "bapi_goodsmvt_create
*&---------------------------------------------------------------------*
*&      Form  user_date_format
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_UNAME  text
*      -->P_0107   text
*      <--P_LV_C_DATBI  text
*----------------------------------------------------------------------*
FORM user_date_format USING    value(p_user)     LIKE sy-uname
                               value(p_date)     LIKE sy-datum
                      CHANGING value(p_userdate).
  CLEAR: p_userdate.
  DATA: yyyy(4).  "year
  DATA: mm(2).    "day
  DATA: dd(2).    "month
  DATA: datfm LIKE usr01-datfm.  "date format

  SELECT SINGLE datfm INTO datfm
    FROM usr01
    WHERE bname = p_user.
** datfm
*1 DD.MM.YYYY
*2 MM/DD/YYYY
*3 MM-DD-YYYY
*4 YYYY.MM.DD
*5 YYYY/MM/DD
*6 YYYY-MM-DD
  yyyy = p_date+0(4).
  mm   = p_date+4(2).
  dd   = p_date+6(2).

  CASE datfm.
    WHEN 1.
      p_userdate+0(2) = dd.
      p_userdate+2(2) = mm.
      p_userdate+4(4) = yyyy.
    WHEN 2 OR 3.
      p_userdate+0(2) = mm.
      p_userdate+2(2) = dd.
      p_userdate+4(4) = yyyy.
    WHEN 4 OR 5 OR 6.
      p_userdate+0(4) = yyyy.
      p_userdate+4(2) = mm.
      p_userdate+6(2) = dd.
  ENDCASE.

ENDFORM.                    " user_date_format
*&---------------------------------------------------------------------*
*&      Form  make_goodsmvt_item
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_goodsmvt_item.
  CLEAR: it_goodsmvt_item, wa_goodsmvt_item.
  wa_goodsmvt_item-material  =
           wa_iditems_without_post-matnr.  "'85850-3K100'.


  wa_goodsmvt_item-move_type =
           wa_iditems_without_post-bwart.  "Movement type "'101'.
  wa_goodsmvt_item-vendor    =
           wa_iditems_without_post-lifnr.  "Vendor '0000400016'.
  wa_goodsmvt_item-entry_qnt =
           wa_iditems_without_post-lfimg.  "Quantity"'1000'.
  wa_goodsmvt_item-entry_uom =
           wa_iditems_without_post-meins.  "Unit 'EA'.

  wa_goodsmvt_item-po_number =
           wa_iditems_without_post-vgbel.  "PO Number
  wa_goodsmvt_item-po_item   =
           wa_iditems_without_post-vgpos.  "PO ITEM  "'00001'.

*  wa_goodsmvt_item-plant  =
*           wa_iditems_without_post-werks.  "Plant
*  wa_goodsmvt_item-stge_loc  =
*           wa_iditems_without_post-lgort.  "Storage Location

*  wa_goodsmvt_item-po_pr_qnt =
*           wa_iditems_without_post-lfimg.                   "'1000'.
*  wa_goodsmvt_item-orderpr_un =
*           wa_iditems_without_post-meins.  "'EA'.
*  wa_goodsmvt_item-st_un_qtyy_1 =
*           wa_iditems_without_post-lfimg.                   "'1000'.
*  wa_goodsmvt_item-st_un_qtyy_2 =
*           wa_iditems_without_post-lfimg.                   "'1000'.
*  wa_goodsmvt_item-deliv_numb_to_search =   "h important
*           wa_iditems_without_post-vbeln.  "Inbound Delivery
*  wa_goodsmvt_item-deliv_item_to_search   =  "h important
*           wa_iditems_without_post-posnr.  "Inbound Delivery Item
*  wa_goodsmvt_item-deliv_numb =
*           wa_iditems_without_post-vbeln.  "Inbound Delivery
*  wa_goodsmvt_item-deliv_item   =
*           wa_iditems_without_post-posnr.  "Inbound Delivery Item
*  wa_goodsmvt_item-no_more_gr = 'X'.
*  ""Delivery completed" indicator
  wa_goodsmvt_item-mvt_ind   = 'B'.
  "Goods receipt for purchase order
  APPEND wa_goodsmvt_item TO it_goodsmvt_item.
ENDFORM.                    " make_goodsmvt_item
*&---------------------------------------------------------------------*
*&      Form  make_goodsmvt_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_goodsmvt_header.
  DATA: lv_c_today(8). CLEAR: lv_c_today.
  PERFORM user_date_format USING    sy-uname
                                    sy-datum                "'99991231'
                           CHANGING lv_c_today.
*      goodsmvt_header-pstng_date = lv_c_today.
*      goodsmvt_header-doc_date   = lv_c_today.

  wa_goodsmvt_header-pstng_date = sy-datum.
  wa_goodsmvt_header-doc_date   = sy-datum.
  wa_goodsmvt_header-ref_doc_no =
              wa_iditems_without_post-vbeln.  " '180000335'.
ENDFORM.                    " make_goodsmvt_header
*&---------------------------------------------------------------------*
*&      Form  bdc_processing_gto
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_BDCMSGCOLL  text
*      <--P_w_subrc  text
*----------------------------------------------------------------------*
FORM bdc_processing_gto TABLES ext_bdcmsgcoll
                                STRUCTURE bdcmsgcoll
                        USING    value(p_zdocno)
                        CHANGING value(p_subrc).
  CLEAR: ext_bdcmsgcoll, ext_bdcmsgcoll[], p_subrc.

  DATA: lv_vbeln LIKE bdcdata-fval.
  DATA: lv_posnr LIKE bdcdata-fval.
  DATA: lv_wdatu LIKE bdcdata-fval.
  DATA: lv_lmen2 LIKE bdcdata-fval.
  DATA: lv_kdmat LIKE bdcdata-fval.

  lv_vbeln = wa_iditems_without_post-vbeln. "Inbound Delivery
  lv_posnr = wa_iditems_without_post-posnr. "Item no.
**** (Begin)Adjust Date format in user by user
  DATA: lv_c_today(8). CLEAR: lv_c_today.
  PERFORM user_date_format USING    sy-uname
                                    sy-datum                "'99991231'
                           CHANGING lv_c_today.
  lv_wdatu = lv_c_today.           "Doc Date & Post Date
**** (End)Adjust Date format in user by user
  WRITE wa_iditems_without_post-lfimg TO lv_lmen2           "'1000'.
          UNIT wa_iditems_without_post-meins.
  CONDENSE lv_lmen2.   "In order to adjust to Screen Field

  lv_kdmat = lips-kdmat.                    "Vendor mat. no.
  w_bdcmode = 'N'.
*  w_bdcmode = 'A'.

*BDC for LT03(Create TO for Delivery Order)
  CALL FUNCTION 'Z_FMM_6010_01'
   EXPORTING
*   CTU             = 'X'
     mode            = w_bdcmode
    UPDATE          = 'S'
*   GROUP           =
*   USER            =
*   KEEP            =
*   HOLDDATE        =
*   NODATA          = '/'
     lgnum_001       = 'P01'             " Warehouse number
     vbeln_002       = lv_vbeln  " '180000335'
     posnr_003       = lv_posnr                             " '000001'
     alakt_004       = 'X'
     wdatu_005       = lv_wdatu          " '10/08/2003'
     anzl2_006       = '1'
     lmen2_007       = lv_lmen2          " Delivery quantity '1,000'
     wdatu_008       = lv_wdatu          " '10/08/2003'
     lmen2_009       = lv_lmen2          " Delivery quantity '1,000'
   IMPORTING
     subrc           = p_subrc
   TABLES
     messtab         = ext_bdcmsgcoll[].

**** (Begin)BDC Log to the table ZTLOG
  CALL FUNCTION 'Z_FMM_6001_03_LOG_TO_ZTABLE'
    IN UPDATE TASK
    EXPORTING
      im_zdocno            = p_zdocno
      im_ztcode            = sy-tcode
      im_zprogramm         = sy-cprog
*            IM_TCODE             =
*            IM_FM_NAME           =
   TABLES
     imt_bdcmsgcoll       = ext_bdcmsgcoll[]
*           IMT_BAPIRET2         =
            .
  COMMIT WORK.
**** (End)BDC Log to the table ZTLOG

ENDFORM.                    " bdc_processing_gto
*&---------------------------------------------------------------------*
*&      Form  post
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM post USING value(p_zdocno).

* Make goodsmvt_item
  PERFORM make_goodsmvt_item.
* Make goodsmvt_header
  PERFORM make_goodsmvt_header.
* Make goodsmvt_code
  wa_goodsmvt_code-gm_code = '01'.
  "Goods receipt for purchase order
* Execute BAPI for Post
  PERFORM bapi_goodsmvt_create TABLES   it_goodsmvt_item
                                        it_bapiret2
                               USING    wa_goodsmvt_header
                                        wa_goodsmvt_code
                               CHANGING wa_goodsmvt_headret.

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
      im_zdocno            = p_zdocno
      im_ztcode            = lv_ztcode
      im_zprogramm         = lv_zprogramm
      im_tcode             = lv_tcode
      im_fm_name           = lv_fm_name
   TABLES
*     imt_bdcmsgcoll       = it_bdcmsgcoll
     imt_bapiret2         = it_bapiret2.
  COMMIT WORK.
**** (End)BAPI Log to the table ZTLOG
ENDFORM.                    " post
*&---------------------------------------------------------------------*
*&      Form  number_get_next
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_c_nro_nr_09  text
*      -->P_w_nro_object  text
*      <--P_W_ZDOCNO  text
*----------------------------------------------------------------------*
FORM number_get_next
           USING    value(p_nro_interval) LIKE inri-nrrangenr
                    value(p_nro_object)   LIKE inri-object
           CHANGING value(p_nro_next).
  CALL FUNCTION 'NUMBER_GET_NEXT'
       EXPORTING
            nr_range_nr             = p_nro_interval
            object                  = p_nro_object
       IMPORTING
            number                  = p_nro_next
       EXCEPTIONS
            interval_not_found      = 1
            number_range_not_intern = 2
            object_not_found        = 3
            quantity_is_0           = 4
            quantity_is_not_1       = 5
            interval_overflow       = 6
            OTHERS                  = 7.
  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    "number_get_next
*&---------------------------------------------------------------------*
*&      Form  call_message_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM call_message_screen
              USING value(im_msgid)
                    value(im_lang)
                    value(im_msgno)
                    value(im_msgv1)
                    value(im_msgv2)
                    value(im_msgv3)
                    value(im_msgv4).

  CALL FUNCTION 'CALL_MESSAGE_SCREEN'
    EXPORTING
      i_msgid                = im_msgid  "'ZMMM'
      i_lang                 = im_lang   "'E'
      i_msgno                = im_msgno                     "'999'
      i_msgv1                = im_msgv1  "'Failure!'
      i_msgv2                = im_msgv2
      i_msgv3                = im_msgv3
      i_msgv4                = im_msgv4
*   I_SEPERATE             = ' '
     i_condense             = 'X'
     i_message_screen       = '0999'
     i_line_size            = '40'
     i_lines                = '4'
     i_non_lmob_envt        = 'X'
    "You can test this parameter with 'X' or space.
    "Space is default and this is especially for Tx /nLM00
* IMPORTING
*   O_ANSWER               =
* TABLES
*   T_MSG_TEXT             =
* EXCEPTIONS
*   INVALID_MESSAGE1       = 1
*   OTHERS                 = 2
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " call_message_screen
*&---------------------------------------------------------------------*
*&      Form  bdc_processing_vl32n
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_BDCMSGCOLL  text
*      -->P_W_ZDOCNO  text
*      <--P_W_SUBRC  text
*----------------------------------------------------------------------*
FORM bdc_processing_vl32n
                        TABLES ext_bdcmsgcoll
                                STRUCTURE bdcmsgcoll
                        USING    value(im_zdocno)
                        CHANGING value(ex_subrc).
  CLEAR: ext_bdcmsgcoll, ext_bdcmsgcoll[], ex_subrc.

  DATA: lv_vbeln   LIKE bdcdata-fval. " Inbound Del Doc no

  DATA: lv_bldat   LIKE bdcdata-fval. " Doc. Date
  DATA: lv_bldat_d LIKE sy-datum.     " Doc. Date
  DATA: lv_budat   LIKE bdcdata-fval. " Posting Date
  DATA: lv_budat_d LIKE sy-datum.     " Posting Date
  DATA: lv_vlief   LIKE bdcdata-fval. " Inbound Delivery no.

  CONSTANTS: lc_time000000 TYPE t VALUE '000000'.
  CONSTANTS: lc_time035959 TYPE t VALUE '035959'.

* Doc. Date
  lv_bldat_d = w_button_click_date.   "Doc. Date
  PERFORM user_date_format USING    sy-uname
                                    lv_bldat_d
                           CHANGING lv_bldat.

* Posting Date
  IF w_button_click_time GE lc_time000000 AND
     w_button_click_time LE lc_time035959.
    lv_budat_d = w_button_click_date - 1.
  ELSE.
    lv_budat_d = w_button_click_date.
  ENDIF.
  PERFORM user_date_format USING    sy-uname
                                    lv_budat_d
                           CHANGING lv_budat.

* Inbound Delivery no.
  lv_vbeln = wa_iditems_without_post-vbeln. "Inbound Delivery
* Condense
  CONDENSE: lv_vbeln,
            lv_bldat,
            lv_budat.

* Call function module
  CALL FUNCTION 'Z_FMM_60XX_VL32N'
    EXPORTING
*   CTU                    = 'X'
*   MODE                   = 'N'
*   UPDATE                 = 'L'
*   GROUP                  =
*   USER                   =
*   KEEP                   =
*   HOLDDATE               =
*   NODATA                 = '/'
      vbeln_001              = lv_vbeln
      bldat_002              = lv_bldat
      wadat_ist_la_003       = lv_budat
     IMPORTING
       subrc           = ex_subrc
     TABLES
       messtab         = ext_bdcmsgcoll.


**** (Begin)BDC Log to the table ZTLOG
  CALL FUNCTION 'Z_FMM_6001_03_LOG_TO_ZTABLE'
    IN UPDATE TASK
    EXPORTING
      im_zdocno            = im_zdocno
      im_ztcode            = sy-tcode
      im_zprogramm         = sy-cprog
*            IM_TCODE             =
*            IM_FM_NAME           =
   TABLES
     imt_bdcmsgcoll       = ext_bdcmsgcoll[]
*           IMT_BAPIRET2         =
            .
  COMMIT WORK.
**** (End)BDC Log to the table ZTLOG


ENDFORM.                    " bdc_processing_vl32n
*&---------------------------------------------------------------------*
*&      Form  bdc_processing_lt03_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_BDCMSGCOLL  text
*      -->P_W_ZDOCNO  text
*      <--P_W_SUBRC  text
*----------------------------------------------------------------------*
FORM bdc_processing_lt03_header

                        TABLES ext_bdcmsgcoll
                                STRUCTURE bdcmsgcoll
                        USING    value(im_zdocno)
                        CHANGING value(ex_subrc).
  CLEAR: ext_bdcmsgcoll, ext_bdcmsgcoll[], ex_subrc.

  DATA: lv_vbeln LIKE bdcdata-fval.
  lv_vbeln = wa_iditems_without_post-vbeln. "Inbound Delivery
  CONDENSE: lv_vbeln.
  w_bdcmode = 'N'.
*  w_bdcmode = 'A'.

*BDC for LT03(Create TO for Delivery Order) - Header Level
  CALL FUNCTION 'Z_FMM_6010_02'
   EXPORTING
*   CTU             = 'X'
     mode            = w_bdcmode
     UPDATE          = 'S'
*   GROUP           =
*   USER            =
*   KEEP            =
*   HOLDDATE        =
*   NODATA          = '/'
     lgnum_001       = 'P01'
     vbeln_002       = lv_vbeln   "Inbound Delivery '180000631'
     alakt_003       = 'X'
   IMPORTING
     subrc           = ex_subrc
   TABLES
     messtab         = ext_bdcmsgcoll.

**** (Begin)BDC Log to the table ZTLOG
  CALL FUNCTION 'Z_FMM_6001_03_LOG_TO_ZTABLE'
    IN UPDATE TASK
    EXPORTING
      im_zdocno            = im_zdocno
      im_ztcode            = sy-tcode
      im_zprogramm         = sy-cprog
*            IM_TCODE             =
*            IM_FM_NAME           =
   TABLES
     imt_bdcmsgcoll       = ext_bdcmsgcoll[]
*           IMT_BAPIRET2         =
            .
  COMMIT WORK.
**** (End)BDC Log to the table ZTLOG

ENDFORM.                    " bdc_processing_lt03_header
*&---------------------------------------------------------------------*
*&      Form  bdc_processing_mb01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_BDCMSGCOLL  text
*      -->P_W_ZDOCNO  text
*      <--P_W_SUBRC  text
*----------------------------------------------------------------------*
FORM bdc_processing_mb01
                        TABLES   ext_bdcmsgcoll
                                   STRUCTURE bdcmsgcoll
                        USING    value(im_zdocno)
                        CHANGING value(ex_subrc).
  CLEAR: ext_bdcmsgcoll, ext_bdcmsgcoll[], ex_subrc.

  DATA: lv_bldat   LIKE bdcdata-fval. " Doc. Date
  DATA: lv_bldat_d LIKE sy-datum.     " Doc. Date
  DATA: lv_budat   LIKE bdcdata-fval. " Posting Date
  DATA: lv_budat_d LIKE sy-datum.     " Posting Date
  DATA: lv_vlief   LIKE bdcdata-fval. " Inbound Delivery no.

  CONSTANTS: lc_time000000 TYPE t VALUE '000000'.
  CONSTANTS: lc_time035959 TYPE t VALUE '035959'.

* Doc. Date
  lv_bldat_d = w_button_click_date.   "Doc. Date
  PERFORM user_date_format USING    sy-uname
                                    lv_bldat_d
                           CHANGING lv_bldat.
* Posting Date
  IF w_button_click_time GE lc_time000000 AND
     w_button_click_time LE lc_time035959.
    lv_budat_d = w_button_click_date - 1.
  ELSE.
    lv_budat_d = w_button_click_date.
  ENDIF.
  PERFORM user_date_format USING    sy-uname
                                    lv_budat_d
                           CHANGING lv_budat.
* Inbound Delivery no.
  lv_vlief = wa_iditems_without_post-vbeln. "Inbound Delivery
* Condense
  CONDENSE: lv_bldat,
            lv_budat,
            lv_vlief.
* Call function module
  CALL FUNCTION 'Z_FMM_60XX_MB01'
   EXPORTING
*   CTU               = 'X'
*   MODE              = 'N'
*   UPDATE            = 'L'
*   GROUP             =
*   USER              =
*   KEEP              =
*   HOLDDATE          =
*   NODATA            = '/'
     bldat_001         = lv_bldat
     budat_002         = lv_budat
     xfull_003         = 'X'
     wvers1_004        = 'X'
     bwartwe_005       = '101'
     vlief_006         = lv_vlief   "  '180000081'
   IMPORTING
     subrc             = ex_subrc
   TABLES
     messtab           = ext_bdcmsgcoll.

**** (Begin)BDC Log to the table ZTLOG
  CALL FUNCTION 'Z_FMM_6001_03_LOG_TO_ZTABLE'
    IN UPDATE TASK
    EXPORTING
      im_zdocno            = im_zdocno
      im_ztcode            = sy-tcode
      im_zprogramm         = sy-cprog
*            IM_TCODE             =
*            IM_FM_NAME           =
   TABLES
     imt_bdcmsgcoll       = ext_bdcmsgcoll[]
*           IMT_BAPIRET2         =
            .
  COMMIT WORK.
**** (End)BDC Log to the table ZTLOG
ENDFORM.                    " bdc_processing_mb01
