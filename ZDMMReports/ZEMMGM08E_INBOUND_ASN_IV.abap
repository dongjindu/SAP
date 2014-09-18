************************************************************************
* Program name : ZEMMGM08E_INBOUND_ASN_IV(Inbound)
* Created by   : Min-su Park
* Created on   : 2003.10.07
* Pattern      : Report 1-1
* Description  : Create Inbound Delivery From ASN & IV.
*                Called from the FM Z_FMM_GET_ASN_IV.
* Modification Log
* Date            Developer        Request No.    Description
* 2003.10.07.     Min-su Park      UD1K901873     Initial Coding
*
************************************************************************
REPORT  zemmgm08e_inbound_asn_iv MESSAGE-ID zmmm.
INCLUDE zemmgm08e_top.

START-OF-SELECTION.
  CASE w_mark.
    WHEN r1.
      PERFORM get_data.
      PERFORM bdc.
      PERFORM create_if_log.
*   PERFORM EXECUTE_BDC.
    WHEN r2.
      PERFORM get_data.
      PERFORM bdc.
      PERFORM create_if_log.
*   PERFORM EXECUTE_BDC.
    WHEN r3.
      PERFORM get_data.
  ENDCASE.
  PERFORM list.
*&---------------------------------------------------------------------*
*&      Form  BDC_PASS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0010   text
*      -->P_0011   text
*      -->P_0012   text
*----------------------------------------------------------------------*
FORM bdc_pass USING par1 par2 par3.
  CLEAR it_bdc.
  IF par1 = 'X'.
    it_bdc-dynbegin = 'X'.
    it_bdc-program  = par2.
    it_bdc-dynpro   = par3.
    APPEND it_bdc.
  ELSE.
    it_bdc-fnam = par2.
    it_bdc-fval = par3.
    APPEND it_bdc.
  ENDIF.
ENDFORM.                    " BDC_PASS
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  CASE w_mark.
    WHEN r1.
      SELECT * FROM ztmm_asn_iv
               INTO TABLE it_asn_iv
               WHERE flag EQ ' '
                 AND zslno IS not NULL
                 AND zslno IN s_zslno
                 AND zsdat IN s_zsdat.
      DESCRIBE TABLE it_asn_iv LINES w_total.
    WHEN r2.
      SELECT * FROM ztmm_asn_iv
               INTO TABLE it_asn_iv
               WHERE flag EQ 'E'
                 AND zslno IN s_zslno
                 AND zsdat IN s_zsdat.
      DESCRIBE TABLE it_asn_iv LINES w_total.
    WHEN r3.
      SELECT * FROM ztmm_asn_iv
               INTO TABLE it_asn_iv
               WHERE flag EQ 'S'
                 AND zslno IN s_zslno
                 AND zsdat IN s_zsdat.
  ENDCASE.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  EXECUTE_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM execute_bdc.
  DATA    : tmp_field LIKE it_bdc_field.
  CLEAR   : it_bdc, it_message.
  REFRESH : it_bdc, it_message, it_bdc_field.
* [ 1 ] Get Data for Inbound Delivery.
  LOOP AT it_asn_iv.
    MOVE-CORRESPONDING it_asn_iv TO it_bdc_field.
    APPEND it_bdc_field.
  ENDLOOP.

* [ 2 ] Sort IT_BDC_FIELD            .
  SORT it_bdc_field BY po_no line_no.

* [ 3 ] Create Inbound Delivery from Data tranfered by C/CRT system.
  LOOP AT it_bdc_field.
    MOVE-CORRESPONDING it_bdc_field TO tmp_field.
    AT NEW po_no.
*    [ 3 ] - 1. Get Ekpo Data by PO Number.
      CLEAR : it_ekpo, w_posnr.
      REFRESH it_ekpo.
      SELECT * FROM ekpo
               INTO CORRESPONDING FIELDS OF TABLE it_ekpo
              WHERE ebeln = it_bdc_field-po_no
                AND loekz <> 'X'.
*    'SAPMV50B' '0107'
      PERFORM bdc_first_screen USING tmp_field-depen_place
                                     tmp_field-request_date.
    ENDAT.
*    [ 3 ] - 2. Compare Ekpo Data and IT_BDC_FIELD data.
    w_posnr = w_posnr + 1.
    READ TABLE it_ekpo WITH KEY ebeln = it_bdc_field-po_no
                                ebelp = it_bdc_field-line_no.
*    [ 3 ] - 3. Decision Inbound Delivery Item for Deletion
*               and Quantity and Vendor Batch
    IF sy-subrc = 0.
      PERFORM bdc_item USING sy-subrc.
    ELSE.

    ENDIF.
    AT END OF po_no.
*    [ 3 ] - 4. Create Inbound Delivery by Changed IT_EKPO
*
      PERFORM bdc_pass USING :
                'X' 'SAPMV50A'   '1000'                ,
                ' ' 'BDC_OKCODE' '=SICH_T'             .

      CALL TRANSACTION 'VL31N'
                 USING it_bdc
                  MODE p_mode
                UPDATE 'S'
              MESSAGES INTO it_message.
*     [ 3 ] - 5. Parameter Setting(System Message)
*                And go to [ 3 ] - 1.
    ENDAT.
  ENDLOOP.
ENDFORM.                    " EXECUTE_BDC
*&---------------------------------------------------------------------*
*&      Form  BDC_FIRST_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bdc_first_screen USING lifnr
                            request_date.
  CLEAR   : it_bdc, it_message.
  REFRESH : it_bdc, it_message.

  PERFORM bdc_pass USING :
                'X' 'SAPMV50B'    '0107'                ,
                ' ' 'LIKP-LIFNR'  lifnr                 ,
                ' ' 'LV50C-BSTNR' it_bdc_field-po_no    ,
                ' ' 'RV50A-LFDAT_LA' request_date       ,
                ' ' 'BDC_OKCODE'  '/00'                 .
ENDFORM.                    " BDC_FIRST_SCREEN
*&---------------------------------------------------------------------*
*&      Form  BDC_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_SUBRC  text
*----------------------------------------------------------------------*
FORM bdc_item USING  subrc.
  DATA : qty(20)       ,
         cnt(02) TYPE n,
         batch(20)     ,
         line(20)      .

*Place Cursor
  PERFORM bdc_pass USING :
          'X' 'SAPMV50A'    '1000'                ,
          ' ' 'BDC_OKCODE'  '=POPO_T'             .

*Item Position
  PERFORM bdc_pass USING :
          'X' 'SAPMV50A'    '0111'                ,
          ' ' 'RV50A-POSNR' w_posnr                 ,
          ' ' 'BDC_OKCODE'  '=WEIT'               .

  cnt = w_posnr+3(2).
  CASE subrc.
    WHEN 0.
*Quantity Field
      CONCATENATE 'LIPSD-G_LFIMG(' cnt ')' INTO qty.
*Vendor Batch
      CONCATENATE 'LIPS-LICHN(' cnt ')'    INTO batch.
      PERFORM bdc_pass USING :
            'X' 'SAPMV50A'   '1000'                  ,
            ' ' qty          it_bdc_field-request_qty,
            ' ' batch        it_bdc_field-depen_no   ,
            ' ' 'BDC_OKCODE' '/00'                   .
    WHEN OTHERS.
*Line Deletion
      CONCATENATE 'RV50A-LIPS_SELKZ(' cnt ')' INTO line.
      PERFORM bdc_pass USING :
            'X' 'SAPMV50A'  '1000'                  ,
            ' ' line        'X'                     ,
            ' ' 'BDC_OKCODE' '=POLO_T'              .
  ENDCASE.
ENDFORM.                    " BDC_ITEM
*&---------------------------------------------------------------------*
*&      Form  BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bdc.
  CLEAR   : w_zsucc             .
  CLEAR   : it_bdc, it_message.
  REFRESH : it_bdc, it_message.

  LOOP AT it_asn_iv.
*   First Screen
    PERFORM bdc_pass USING :
         'X' 'SAPMV50B'    '0107'                          ,
         ' ' 'LIKP-LIFNR'  it_asn_iv-depen_place           ,
         ' ' 'LV50C-BSTNR' it_asn_iv-po_no                 ,
         ' ' 'RV50A-LFDAT_LA' it_asn_iv-request_date       ,
         ' ' 'BDC_OKCODE'  '/00'                           .
*   Second Screen
    PERFORM bdc_pass USING :
         'X' 'SAPMV50A'   '1000'                      ,
         ' ' 'LIPSD-G_LFIMG(01)' it_asn_iv-request_qty,
         ' ' 'LIPS-LICHN(01)'    it_asn_iv-depen_no   ,
         ' ' 'BDC_OKCODE' '/00'                       .
*   Save
    PERFORM bdc_pass USING :
         'X' 'SAPMV50A'   '1000'                ,
         ' ' 'BDC_OKCODE' '=SICH_T'             .
*   Call Transaction
    CALL TRANSACTION 'VL31N'
               USING it_asn_iv
                MODE p_mode
              UPDATE 'S'
            MESSAGES INTO it_message.
    IF sy-subrc = 0.
      w_zsucc = w_zsucc + 1.
      PERFORM get_message.
      PERFORM build_zsca_if_time_stamp USING sy-subrc.
    ELSE.
      PERFORM get_message.
      PERFORM build_zsca_if_time_stamp USING sy-subrc.
    ENDIF.
    UPDATE ztmm_asn_iv FROM it_asn_iv.
    MODIFY it_asn_iv.
  ENDLOOP.
ENDFORM.                    " BDC
*&---------------------------------------------------------------------*
*&      Form  CREATE_IF_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_if_log.
  DATA : i_ztca_if_log LIKE ztca_if_log.

  CHECK w_total <> 0.
  READ TABLE it_asn_iv INDEX 1.
  i_ztca_if_log-tcode    = 'ZMMI01'.
  i_ztca_if_log-zslno    = it_asn_iv-zslno.
  i_ztca_if_log-jobcount = it_asn_iv-jobcount.
  i_ztca_if_log-total    = w_total.
  i_ztca_if_log-zsucc  = w_zsucc.
  i_ztca_if_log-error    = w_total - w_zsucc.
  i_ztca_if_log-erdat    = sy-datum. "Created on.
  i_ztca_if_log-erzet    = sy-uzeit. "Created time.
  i_ztca_if_log-ernam    = sy-uname. "Created by.

  CALL FUNCTION 'Z_FCA_EAI_INTERFACE_LOG'
    EXPORTING
      i_ztca_if_log     = i_ztca_if_log
*    IMPORTING
*      E_ZTCA_IF_LOG     =
 EXCEPTIONS
   update_failed              = 1
   number_range_error         = 2
   tcode_does_not_exist       = 3
   OTHERS                     = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " CREATE_IF_LOG
*&---------------------------------------------------------------------*
*&      Form  BUILD_ZSCA_IF_TIME_STAMP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_SUBRC  text
*----------------------------------------------------------------------*
FORM build_zsca_if_time_stamp USING subrc.
*    ZSCA_IF_TIME_STAMP
*     IT_ASN_IV-ZUSER
*     IT_ASN_IV-ZSDAT
*     IT_ASN_IV-ZSTIM
  it_asn_iv-zedat = sy-datum.
  it_asn_iv-zetim = sy-uzeit.
  it_asn_iv-zbdat = sy-datum.  "SAP BDC EXECUTED DATE
  it_asn_iv-zbtim = sy-uzeit.  "SAP BDC EXECUTED TIME
  it_asn_iv-zbnam = sy-uname.  "BDC User ID
  it_asn_iv-zmode = 'C'     .
  it_asn_iv-zresult = sy-msgty.
*     IT_ASN_IV-ZMSG
  CASE subrc.
    WHEN 0.
      it_asn_iv-flag   = 'S'  .
      it_asn_iv-status = 'END'.
    WHEN OTHERS.
      it_asn_iv-flag  = 'E'.
  ENDCASE.
ENDFORM.                    " BUILD_ZSCA_IF_TIME_STAMP
*&---------------------------------------------------------------------*
*&      Form  GET_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_message.

  DATA : txt LIKE t100-text.
  DATA : msgnr(3) TYPE n.
  READ TABLE it_message INDEX 1.

  msgnr = it_message-msgnr.
  CALL FUNCTION 'TB_MESSAGE_BUILD_TEXT'
       EXPORTING
            langu = sy-langu
            msgid = it_message-msgid
            msgno = msgnr
            msgv1 = it_message-msgv1+0(50)
            msgv2 = it_message-msgv2+0(50)
            msgv3 = it_message-msgv3+0(50)
            msgv4 = it_message-msgv4+0(50)
       IMPORTING
            text  = txt.
  it_asn_iv-zmsg  = txt.
ENDFORM.                    " GET_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM list.
  PERFORM alv_build.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
     i_callback_program           = w_repid
     it_events                    = wa_events[]
     it_fieldcat                  = it_fieldcat[]
     i_callback_user_command      = 'USER_COMMAND'
* IMPORTING
*   E_EXIT_CAUSED_BY_CALLER        =
*   ES_EXIT_CAUSED_BY_USER         =
   TABLES
     t_outtab                     = it_asn_iv
* EXCEPTIONS
*   PROGRAM_ERROR                  = 1
*   OTHERS                         = 2
           .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " LIST
*&---------------------------------------------------------------------*
*&      Form  ALV_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_build.
  w_repid = sy-repid.
  CLEAR : it_fieldcat[], wa_events[],
          wa_list_top_of_page[]     .
  PERFORM fieldcat_init  USING it_fieldcat[].
  PERFORM eventtab_build USING wa_events[].
  PERFORM comment_build  USING wa_list_top_of_page[].
ENDFORM.                    " ALV_BUILD
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT[]  text
*----------------------------------------------------------------------*
FORM fieldcat_init USING rt_fieldcat TYPE slis_t_fieldcat_alv.
  DATA: ls_fieldcat TYPE slis_fieldcat_alv.
  DATA: pos TYPE i VALUE 1.

*Document Type
  CLEAR ls_fieldcat.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'DOC_TYPE'.
  ls_fieldcat-ref_fieldname = 'DOC_TYPE'.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Document Type'.
  ls_fieldcat-seltext_m     = 'Document Type'.
  ls_fieldcat-seltext_s     = 'Document Type'.
  ls_fieldcat-outputlen     = '10'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

*Request Number
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'DEPEN_NO'.
  ls_fieldcat-ref_fieldname = 'DEPEN_NO'.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Request No'.
  ls_fieldcat-seltext_m     = 'Request No'.
  ls_fieldcat-seltext_s     = 'Request No'.
  ls_fieldcat-outputlen     = '20'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

*Item No
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'ITEM_NO'.
  ls_fieldcat-ref_fieldname = 'ITEM_NO'.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Item No'.
  ls_fieldcat-seltext_m     = 'Item No'.
  ls_fieldcat-seltext_s     = 'Item No'.
  ls_fieldcat-outputlen     = '17'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

*Request Vendor
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'DEPEN_PLACE'.
  ls_fieldcat-ref_fieldname = 'DEPEN_PLACE'.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Request Vendor'.
  ls_fieldcat-seltext_m     = 'Request Vendor'.
  ls_fieldcat-seltext_s     = 'Request Vendor'.
  ls_fieldcat-outputlen     = '10'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.


*Po Number
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'PO_NO'.
  ls_fieldcat-ref_fieldname = 'PO_NO'.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'PO Number'.
  ls_fieldcat-seltext_m     = 'PO Number'.
  ls_fieldcat-seltext_s     = 'PO Number'.
  ls_fieldcat-outputlen     = '20'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

*Line Number
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'LINE_NO'.
  ls_fieldcat-ref_fieldname = 'LINE_NO'.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Line Number'.
  ls_fieldcat-seltext_m     = 'Line Number'.
  ls_fieldcat-seltext_s     = 'Line Number'.
  ls_fieldcat-outputlen     = '3'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

*Message
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'ZMSG'.
  ls_fieldcat-ref_fieldname = 'ZMSG'.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Message Text'.
  ls_fieldcat-seltext_m     = 'Message Text'.
  ls_fieldcat-seltext_s     = 'Message Text'.
  ls_fieldcat-outputlen     = '200'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

ENDFORM.                    " FIELDCAT_INIT
*&---------------------------------------------------------------------*
*&      Form  EVENTTAB_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_EVENTS[]  text
*----------------------------------------------------------------------*
FORM eventtab_build USING e03_lt_events TYPE slis_t_event.
  DATA: ls_event TYPE slis_alv_event.
*
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            i_list_type = 0
       IMPORTING
            et_events   = e03_lt_events.
  READ TABLE e03_lt_events WITH KEY name =  slis_ev_top_of_page
                           INTO ls_event.
  IF sy-subrc = 0.
    MOVE w_formname_top_of_page  TO ls_event-form.
    APPEND ls_event TO e03_lt_events.
  ENDIF.
ENDFORM.                    " EVENTTAB_BUILD
*&---------------------------------------------------------------------*
*&      Form  COMMENT_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_LIST_TOP_OF_PAGE[]  text
*----------------------------------------------------------------------*
FORM comment_build USING lt_top_of_page TYPE slis_t_listheader.
  DATA: ls_line TYPE slis_listheader,
        l_date(50),
        l_list(50),
        l_dsnam LIKE t024d-dsnam,
        l_h_dsnam LIKE t024d-dsnam,
        l_total(5) TYPE i,
        l_succ(5) TYPE i,
        l_ldate(10),
        l_hdate(10).

*----- Title
  CLEAR ls_line.
  ls_line-typ  = 'H'.
  ls_line-info = text-a01.
  APPEND ls_line TO lt_top_of_page.

*----- User
  ls_line-typ  = 'S'.
  ls_line-key  = 'User: '.
  ls_line-info = sy-uname.
  APPEND ls_line TO lt_top_of_page.

*----- Date
*  IF S_ZSDAT-LOW IS NOT INITIAL.
*    LS_LINE-TYP  = 'S'.
*    LS_LINE-KEY  = 'Date: '.
*    CONCATENATE 'FROM: ' -LOW '  TO: ' S_ZSDAT-HIGH INTO L_LIST.
*    LS_LINE-INFO = L_LIST.
*    APPEND LS_LINE TO LT_TOP_OF_PAGE.
*  ENDIF.

*----- Total
  ls_line-typ  = 'S'.
  ls_line-key  = 'Total: '.
  ls_line-info = w_total.
  APPEND ls_line TO lt_top_of_page.

*----- Success
  ls_line-typ  = 'S'.
  ls_line-key  = 'Success: '.
  ls_line-info = w_zsucc.
  APPEND ls_line TO lt_top_of_page.

*----- Success
  ls_line-typ  = 'S'.
  ls_line-key  = 'Failed: '.
  ls_line-info = w_total - w_zsucc.
  APPEND ls_line TO lt_top_of_page.

ENDFORM.                    " COMMENT_BUILD
*---------------------------------------------------------------------*
*       FORM TOP_OF_PAGE                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM top_of_page.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
*           i_logo             = 'HTMLCNTL_TESTHTM2_SAPLOGO'
*           I_LOGO             = 'ENJOYSAP_LOGO'
            it_list_commentary = wa_list_top_of_page.
ENDFORM.
