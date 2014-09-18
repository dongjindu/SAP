*----------------------------------------------------------------------*
*   INCLUDE ZEMMGM15E_6018F01                                          *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
*/ Select from PLAF(Planned Order Table)
  SELECT
         plaf~plnum           "Planned order number FOR INFO
         plaf~paart
*         '1'         AS BNFPO "Item number of purchase requisition
         plaf~matnr           "Material
         plaf~gsmng           "Total planned order quantity
         plaf~pedtr           "Order finish date in the planned order
         plaf~pwwrk           "Production plant in planned order
         plaf~lgort           "Storage location
         marc~ekgrp           "Purchasing Group
*         'LTP'       AS BEDNR "Requirement Tracking Number
         plaf~flief           "Fixed Vendor
*         'PU01'      AS ekorg "Purchasing organization
    INTO CORRESPONDING FIELDS OF TABLE it_plaf
    FROM plaf
      INNER JOIN marc
      ON marc~matnr = plaf~matnr AND
         marc~werks = plaf~pwwrk
    WHERE plaf~dispo = p_dispo AND   "MRP Controller
          plaf~pedtr IN s_lfdat.     "Order finish (Delivery date)

*/ Fill the default values
  LOOP AT it_plaf ASSIGNING <fs_plaf>.
    <fs_plaf>-bnfpo = '1'.    "Item number of purchase requisition
*    <fs_plaf>-bednr = 'LTP'.  "Requirement Tracking Number
    <fs_plaf>-ekorg = 'PU01'. "Purchasing organization
  ENDLOOP.
ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  process_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_data.
*/ App Doc No

  PERFORM number_get_next USING    c_nro_nr_09     "NRO Interval
                                   w_nro_object    "NRO Object
                          CHANGING w_zdocno.     "App Doc No
  COMMIT WORK.
************************************************************************
*/ Get it_requisition_items
  LOOP AT it_plaf ASSIGNING <fs_plaf>.
    CLEAR: wa_requisition_items.
    MOVE:
       <fs_plaf>-paart TO wa_requisition_items-doc_type,
       "PR Doc Type
       <fs_plaf>-bnfpo TO wa_requisition_items-preq_item,
       "Item number of purchase requisition
       '-'             TO wa_requisition_items-preq_name,
       "Name of requisitioner/requester
       <fs_plaf>-matnr TO wa_requisition_items-material,
       "Material
       <fs_plaf>-gsmng TO wa_requisition_items-quantity,
       "Total planned order quantity
       <fs_plaf>-pedtr TO wa_requisition_items-deliv_date,
       "Order finish date in the planned order
       <fs_plaf>-pwwrk TO wa_requisition_items-plant,
       "Production plant in planned order
       <fs_plaf>-lgort TO wa_requisition_items-store_loc,
       "Storage location
       <fs_plaf>-ekgrp TO wa_requisition_items-pur_group,
       "Purchasing Group
       <fs_plaf>-bednr TO wa_requisition_items-trackingno,
       "Requirement Tracking Number
*/ Begin of Added by Hakchin (20040114)
       'X'             TO wa_requisition_items-gr_ind,
       "Goods receipt indicator
       'X'             TO wa_requisition_items-ir_ind,
       "Invoice receipt indicator
*/ End of Added by Hakchin (20040114)
       <fs_plaf>-flief TO wa_requisition_items-fixed_vend,
       "Fixed Vendor
       <fs_plaf>-ekorg TO wa_requisition_items-purch_org.
    "Purchasing organization
    APPEND wa_requisition_items TO it_requisition_items.

*/ Create Purchase Requistion from planned order using
*  BAPI_REQUISITION_CREATE.
    PERFORM bapi_requisition_create
                      TABLES   it_requisition_items
                               it_bapireturn
                      CHANGING wa_requisition_items-preq_no.
    CLEAR: it_requisition_items.

*/ BAPI Log to the table ZTLOG
    PERFORM bapilog_to_ztlog
                  TABLES it_bapireturn
                         it_bapiret2
                  USING  w_zdocno.
*/ Begin of Added by Hakchin(20040114)
    READ TABLE it_bapiret2 WITH KEY type = 'E'
                             TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.  "Success Case
*/ Delete Planned Order
      PERFORM bapi_plannedorder_delete
                        USING    <fs_plaf>-plnum  "Planned Order Number
                        CHANGING wa_bapireturn1.

*/ BAPI Log to the table ZTLOG
      PERFORM wa_bapireturn1_to_ztlog
                           USING  w_zdocno
                                  wa_bapireturn1.
    ENDIF.
*/ End of Added by Hakchin(20040114)

  ENDLOOP.


*/ Begin of Deleted by Hakchin (20040114)
*/ BDC - MRP Run (/nMD01)
**For plant 'P001'
*  PERFORM bdc_processing_md01
*                   TABLES   it_bdcmsgcoll
*                   USING    w_zdocno
*                            'P001'   "Plant
*                            p_dispo  "MRP Controller
*                   CHANGING w_subrc.
*
**For plant 'E001'
*  PERFORM bdc_processing_md01
*                   TABLES   it_bdcmsgcoll
*                   USING    w_zdocno
*                            'E001'   "Plant
*                            p_dispo  "MRP Controller
*                   CHANGING w_subrc.
*/ End of Deleted by Hakchin (20040114)
ENDFORM.                    " process_data
*&---------------------------------------------------------------------*
*&      Form  bapi_requisition_create
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_REQUISITION_ITEMS  text
*      -->P_IT_BAPIRETURN  text
*      <--P_'prno'  text
*----------------------------------------------------------------------*
FORM bapi_requisition_create
                  TABLES   imt_requisition_items
                             STRUCTURE bapiebanc
                           ext_bapireturn
                             STRUCTURE bapireturn
                  CHANGING value(ex_prno) TYPE bapiebanc-preq_no.
  CLEAR: ext_bapireturn, ext_bapireturn[].

  CALL FUNCTION 'BAPI_REQUISITION_CREATE'
* EXPORTING
*   SKIP_ITEMS_WITH_ERROR                =
*   AUTOMATIC_SOURCE                     = 'X'
    IMPORTING
      number                               = ex_prno
    TABLES
      requisition_items                    = imt_requisition_items
*   REQUISITION_ACCOUNT_ASSIGNMENT       =
*   REQUISITION_ITEM_TEXT                =
*   REQUISITION_LIMITS                   =
*   REQUISITION_CONTRACT_LIMITS          =
*   REQUISITION_SERVICES                 =
*   REQUISITION_SRV_ACCASS_VALUES        =
      return                               = ext_bapireturn.
*   REQUISITION_SERVICES_TEXT            =
*   REQUISITION_ADDRDELIVERY             =
*   EXTENSIONIN                          =
  .

*/ Transaction Commit
  CLEAR: ext_bapireturn.
  READ TABLE ext_bapireturn WITH KEY type = 'E'.
  IF sy-subrc = 0.  "Error Occurred !
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
*     IMPORTING
*       RETURN        =
              .
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait          = 'X'
*     IMPORTING
*       RETURN        =    .
         .
  ENDIF.


ENDFORM.                    "bapi_requisition_create
*&---------------------------------------------------------------------*
*&      Form  bapilog_to_ztlog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_Z_DOCNO  text
*----------------------------------------------------------------------*
FORM bapilog_to_ztlog
               TABLES imt_bapireturn
                        STRUCTURE bapireturn
                      ext_bapiret2
                        STRUCTURE bapiret2
               USING value(im_w_zdocno) TYPE num10.
  CLEAR: ext_bapiret2, ext_bapiret2[].
  DATA: ls_bapiret2 LIKE bapiret2.
  DATA: lt_bapiret2 LIKE TABLE OF ls_bapiret2.
  FIELD-SYMBOLS: <fs_bapireturn> LIKE bapireturn.
**** (Begin)BAPI Log to the table ZTLOG
*/1.
  CLEAR: lt_bapiret2.
  LOOP AT imt_bapireturn ASSIGNING <fs_bapireturn>.
    CLEAR: ls_bapiret2.
    MOVE:
     <fs_bapireturn>-type      TO ls_bapiret2-type,
     <fs_bapireturn>-code(2)   TO ls_bapiret2-id,
     <fs_bapireturn>-code+2(3) TO ls_bapiret2-number,
     <fs_bapireturn>-message   TO ls_bapiret2-message,
*       <fs_bapireturn>-log_no to ls_bapiret2-
*       <fs_bapireturn>-log_msg_no to ls_bapiret2-
    <fs_bapireturn>-message_v1 TO ls_bapiret2-message_v1,
    <fs_bapireturn>-message_v2 TO ls_bapiret2-message_v2,
    <fs_bapireturn>-message_v3 TO ls_bapiret2-message_v3,
    <fs_bapireturn>-message_v4 TO ls_bapiret2-message_v4.
    APPEND ls_bapiret2 TO lt_bapiret2.
  ENDLOOP.

*/2.
  IF lt_bapiret2 IS INITIAL.  "SUCCESS
    CLEAR: ls_bapiret2.
    ls_bapiret2-type       = 'S'.  "SUCCESS
    ls_bapiret2-id         = 'ZMMM'.
    ls_bapiret2-number     = '999'.
    ls_bapiret2-message_v1 = 'Puchase Requisition'.
    ls_bapiret2-message_v2 = wa_requisition_items-preq_no..
    ls_bapiret2-message_v3 = 'is created from planned order'.
    ls_bapiret2-message_v4 = <fs_plaf>-plnum.
    APPEND ls_bapiret2 TO lt_bapiret2.
  ENDIF.

  DATA: logno_h       TYPE num10.
  DATA: lv_ztcode     TYPE tcode.
  DATA: lv_zprogramm  TYPE programm.
  DATA: lv_tcode      TYPE tcode.
  DATA: lv_fm_name    TYPE rs38l_fnam.

  lv_ztcode    = sy-tcode.
  lv_zprogramm = sy-cprog.
  lv_tcode     = 'ME51N'.
  lv_fm_name   = 'BAPI_REQUISITION_CREATE'.

  CALL FUNCTION 'Z_FMM_6001_03_LOG_TO_ZTABLE'
    IN UPDATE TASK
    EXPORTING
      im_zdocno            = im_w_zdocno
      im_ztcode            = lv_ztcode
      im_zprogramm         = lv_zprogramm
      im_tcode             = lv_tcode
      im_fm_name           = lv_fm_name
   TABLES
*     imt_bdcmsgcoll       = it_bdcmsgcoll
     imt_bapiret2         = lt_bapiret2.
  COMMIT WORK.
**** (End)BAPI Log to the table ZTLOG
  ext_bapiret2[] = lt_bapiret2.
ENDFORM.                    " bapilog_to_ztlog
*&---------------------------------------------------------------------*
*&      Form  number_get_next
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NRO_NR_09  text
*      -->P_NRO_OBJECT  text
*      <--P_W_ZDOCNO  text
*----------------------------------------------------------------------*
FORM number_get_next
           USING    value(p_nro_interval) LIKE inri-nrrangenr
                    value(p_nro_object)   LIKE inri-object
           CHANGING value(p_nro_next).
  CLEAR: p_nro_next.
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
*&      Form  f4_sov_dispo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4_sov_dispo
                  TABLES imt_value_tab STRUCTURE wa_f4_dispo
                         ext_return_tab STRUCTURE ddshretval
                  USING  value(p_retfield)
                         value(p_dynpprog)
                         value(p_dynpnr)
                         value(p_dynprofield)
                         value(p_stepl).
  CLEAR: ext_return_tab, ext_return_tab[].
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     ddic_structure         = ' '
     retfield               = p_retfield  "p_value_tab? ? field
*     PVALKEY                = ' '
     dynpprog               = p_dynpprog
     dynpnr                 = p_dynpnr
     dynprofield            = p_dynprofield  "???? ?? ?? field
     stepl                  = p_stepl
*     WINDOW_TITLE           =
*     VALUE                  = ' '
     value_org              = 'S'
*     MULTIPLE_CHOICE        = ' '
*     DISPLAY                = ' '
*     CALLBACK_PROGRAM       = ' '
*     CALLBACK_FORM          = ' '
    TABLES
      value_tab              = imt_value_tab
*     FIELD_TAB              =
     return_tab             = ext_return_tab
*     DYNPFLD_MAPPING        =
   EXCEPTIONS
     parameter_error        = 1
     no_values_found        = 2
     OTHERS                 = 3.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    " f4_sov_dispo
*&---------------------------------------------------------------------*
*&      Form  get_it_f4_dispo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_it_f4_dispo.
  SELECT
*       werks
       dispo
       dsnam
    INTO CORRESPONDING FIELDS OF TABLE it_f4_dispo
    FROM t024d
    WHERE dispo = 'P01' OR
          dispo = 'P02'.
  SORT it_f4_dispo BY dispo.
  DELETE ADJACENT DUPLICATES FROM it_f4_dispo
                                    COMPARING dispo.


ENDFORM.                    " get_it_f4_dispo
*&---------------------------------------------------------------------*
*&      Form  bdc_processing_md01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_BDCMSGCOLL  text
*      -->P_W_ZDOCNO  text
*      <--P_W_SUBRC  text
*----------------------------------------------------------------------*
FORM bdc_processing_md01
         TABLES ext_bdcmsgcoll
                  STRUCTURE bdcmsgcoll
         USING    value(im_zdocno)
                  value(im_plant)
                  value(im_dispo) TYPE t024d-dispo "MRP controller
         CHANGING value(ex_subrc).
  CLEAR: ext_bdcmsgcoll, ext_bdcmsgcoll[], ex_subrc.

  DATA: lv_werks_001 LIKE bdcdata-fval,  "Plant 'P001'
        lv_dispd_008 LIKE bdcdata-fval,
        lv_uxpar_010 LIKE bdcdata-fval.  "MRP Controller



  DATA: lv_loacaldate(8).
  PERFORM user_date_format USING    sy-uname
                                    sy-datum
                           CHANGING lv_loacaldate.


  lv_werks_001 = im_plant.  "Plant 'P001'
  lv_dispd_008 = lv_loacaldate.
  lv_uxpar_010 = im_dispo.  "MRP Controller
  CONDENSE: lv_werks_001,
            lv_dispd_008,
            lv_uxpar_010.


  CALL FUNCTION 'Z_FMM_60XX_MD01'
   EXPORTING
*   CTU             = 'X'
*   MODE            = 'N'
*   UPDATE          = 'L'
*   GROUP           =
*   USER            =
*   KEEP            =
*   HOLDDATE        =
*   NODATA          = '/'
     werks_001       = lv_werks_001  "Plant 'P001'
     versl_002       = 'NETCH'
     baner_003       = '1'
     lifkz_004       = '3'
     diser_005       = '1'
     plmod_006       = '3'
     trmpl_007       = '1'
     dispd_008       = lv_dispd_008  "What happens? when yesterday.
     uxkey_009       = '001'
     uxpar_010       = lv_uxpar_010 "MRP Controller
   IMPORTING
     subrc           = w_subrc
   TABLES
     messtab         = ext_bdcmsgcoll.

*/ BDC Log to the table ZTLOG
  PERFORM z_fmm_6001_03_log_to_ztable
                             TABLES ext_bdcmsgcoll
                             USING  im_zdocno
                                    sy-tcode
                                    sy-cprog.

ENDFORM.                    " bdc_processing_md01
*&---------------------------------------------------------------------*
*&      Form  z_fmm_6001_03_log_to_ztable
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_EXT_BDCMSGCOLL  text
*      -->P_IM_ZDOCNO  text
*      -->P_SY_TCODE  text
*      -->P_SY_CPROG  text
*----------------------------------------------------------------------*
FORM z_fmm_6001_03_log_to_ztable
                TABLES   imt_bdcmsgcoll
                           STRUCTURE bdcmsgcoll
                USING    im_zdocno TYPE num10
                         im_tcode  TYPE tcode
                         im_cprog  TYPE programm.
**** (Begin)BDC Log to the table ZTLOG
  CALL FUNCTION 'Z_FMM_6001_03_LOG_TO_ZTABLE'
    IN UPDATE TASK
    EXPORTING
      im_zdocno            = im_zdocno
      im_ztcode            = im_tcode
      im_zprogramm         = im_cprog
*            IM_TCODE             =
*            IM_FM_NAME           =
   TABLES
     imt_bdcmsgcoll       = imt_bdcmsgcoll
*           IMT_BAPIRET2         =
            .
  COMMIT WORK.
**** (End)BDC Log to the table ZTLOG
ENDFORM.                    " z_fmm_6001_03_log_to_ztable
*&---------------------------------------------------------------------*
*&      Form  display_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_log.
*/ For Select-Options of external pgm
  DATA: BEGIN OF lt_zdocno OCCURS 0,
          sign(1),
          option(2),
          low  LIKE w_zdocno,
          high LIKE w_zdocno,
         END OF lt_zdocno.

  lt_zdocno-sign   = 'I'.
  lt_zdocno-option = 'EQ'.
  lt_zdocno-low    = w_zdocno.
  APPEND lt_zdocno.

* Log Display
  SUBMIT zrmmgm99r_logview
          WITH s_zdocno IN lt_zdocno
          AND RETURN.
ENDFORM.                    " display_log
*&---------------------------------------------------------------------*
*&      Form  user_date_format
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_UNAME  text
*      -->P_SY_DATUM  text
*      <--P_LV_DISPD_008  text
*----------------------------------------------------------------------*
FORM user_date_format USING    value(p_user)     LIKE sy-uname
                               value(p_date)     LIKE sy-datum
                      CHANGING value(p_userdate) TYPE char8.
* Authored by Hakchin.
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
*&      Form  bapi_PLANNEDORDER_DELETE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_BAPIRETURN  text
*      -->P_<FS_PLAF>_PLNUM  text
*----------------------------------------------------------------------*
FORM bapi_plannedorder_delete
            USING    value(im_plannedorder) LIKE bapi_pldord-pldord_num
            CHANGING value(exs_bapireturn1) LIKE bapireturn1.

  CALL FUNCTION 'BAPI_PLANNEDORDER_DELETE'
    EXPORTING
      plannedorder          = im_plannedorder
*   USE_COLL_UPDATE       = ' '
*   LAST_ORDER            = ' '
 IMPORTING
   return                = exs_bapireturn1.

*/ Transaction Commit
  IF exs_bapireturn1-type = 'E'.  "Error Occurred !
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
*     IMPORTING
*       RETURN        =
              .
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait          = 'X'
*     IMPORTING
*       RETURN        =    .
         .
  ENDIF.

ENDFORM.                    "BAPI_PLANNEDORDER_DELETE
*&---------------------------------------------------------------------*
*&      Form  wa_bapireturn1_to_ztlog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ZDOCNO  text
*      -->P_WA_BAPIRETURN1  text
*----------------------------------------------------------------------*
FORM wa_bapireturn1_to_ztlog
               USING value(im_zdocno) TYPE num10
                     value(ims_bapireturn1) TYPE bapireturn1.

**** (Begin)BAPI Log to the table ZTLOG
  CLEAR: wa_bapiret2, it_bapiret2.
  MOVE:
   ims_bapireturn1-type       TO wa_bapiret2-type,
   ims_bapireturn1-id         TO wa_bapiret2-id,
   ims_bapireturn1-number     TO wa_bapiret2-number,
   ims_bapireturn1-message    TO wa_bapiret2-message,
   ims_bapireturn1-log_no     TO wa_bapiret2-log_no,
   ims_bapireturn1-log_msg_no TO wa_bapiret2-log_msg_no,
   ims_bapireturn1-message_v1 TO wa_bapiret2-message_v1,
   ims_bapireturn1-message_v2 TO wa_bapiret2-message_v2,
   ims_bapireturn1-message_v3 TO wa_bapiret2-message_v3,
   ims_bapireturn1-message_v4 TO wa_bapiret2-message_v4.
  APPEND wa_bapiret2 TO it_bapiret2.

  DATA: logno_h       TYPE num10.
  DATA: lv_ztcode     TYPE tcode.
  DATA: lv_zprogramm  TYPE programm.
  DATA: lv_tcode      TYPE tcode.
  DATA: lv_fm_name    TYPE rs38l_fnam.

  lv_ztcode    = sy-tcode.
  lv_zprogramm = sy-cprog.
  lv_tcode     = 'MD12'. "Planned Order Change(Delete)
  lv_fm_name   = 'BAPI_PLANNEDORDER_DELETE'.

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
ENDFORM.                    " wa_bapireturn1_to_ztlog
