************************************************************************
* Program Name      : ZIMMGM09_HMMA_STOCK
* Author            : hj.song
* Creation Date     : 2003.11.20
* Specifications By : hj.song
* Pattern           : Report 1-1
* Development Request No : UD1K902172
* Addl Documentation:
* Description       : HMMA Stock ( outbound : SAP->HYSCO )
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2003.11.20.     hj.song          UD1K902172     Initial Coding
*
*
************************************************************************
REPORT  zimmgm09_hmma_stock NO STANDARD PAGE HEADING LINE-SIZE 255
                            MESSAGE-ID zmmm.

INCLUDE zimmgm09_hmma_stock_top.


START-OF-SELECTION.
  PERFORM set_date.
  PERFORM read_data.
  PERFORM execute_rfc_with_eai.

END-OF-SELECTION.
  PERFORM display_data.
* interface log
  PERFORM create_interface_log.

*&---------------------------------------------------------------------*
*&      Form  read_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM read_data.

  IF     rv1  =  'X'.   "Movement Info
    PERFORM read_movement.
  ELSEIF rv2  =  'X'.   "HMMA Stock Info
    PERFORM read_stock.
  ENDIF.

ENDFORM.                    " read_data
*&---------------------------------------------------------------------*
*&      Form  read_move_pro_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM read_move_pro_data.
* Tcode:MMBE와 ME2O의 정보를 전송 (MMBE->stock ME2O->trans)
* but ME2O의 정보만 outbound처리하도록 요청받았음 (by bhhan)
  CLEAR :  it_basic[], it_basic,
           it_list[], it_list.

  SELECT   b~werks
           b~lgort
           b~bwart
           b~matnr
           b~lifnr
           b~charg
           b~erfmg
           b~erfme
         INTO CORRESPONDING FIELDS OF TABLE it_basic
         FROM mkpf AS a INNER JOIN mseg AS b
           ON a~mandt  EQ  b~mandt
          AND a~mblnr  EQ  b~mblnr
          AND a~mjahr  EQ  b~mjahr
              INNER JOIN mara AS c
                 ON b~mandt  EQ  c~mandt
                AND b~matnr  EQ  c~matnr
        WHERE a~budat  IN  s_budat
        AND   a~bldat  IN  s_bldat
        AND   b~bwart  IN  ('541', '311')
        AND   b~xauto  EQ  ''
        AND   c~mtart  EQ  'ROH1'
        AND   c~matkl  EQ  'AM'.

  LOOP AT it_basic.
    it_list-zwerks   =  it_basic-werks.
    it_list-zlgort   =  it_basic-lgort.
    it_list-doc_type =  it_basic-bwart.
    it_list-zmatnr   =  it_basic-matnr.
    it_list-zlifnr   =  it_basic-lifnr.
    it_list-zcharg   =  it_basic-charg.
    it_list-zbdmng   =  it_basic-erfmg.
    it_list-zmeins   =  it_basic-erfme.

* read characterisitc
    CALL FUNCTION 'Z_FMM_GET_CHARACT'
         EXPORTING
              i_matnr        = it_list-zmatnr
         IMPORTING
              es_zsmm_class  = wa_zsmm_class
         EXCEPTIONS
              data_not_found = 1
              OTHERS         = 2.
    IF sy-subrc EQ 0.
      it_list-zprop      =  wa_zsmm_class-zprop.
      it_list-zcoating   =  wa_zsmm_class-zcoating.
      it_list-zthick     =  wa_zsmm_class-zthick.
      it_list-zwidth     =  wa_zsmm_class-zwidth.
      it_list-zlength    =  wa_zsmm_class-zlength.
    ENDIF.
    COLLECT it_list. CLEAR it_list.
  ENDLOOP.


ENDFORM.                    " read_move_pro_data
*&---------------------------------------------------------------------*
*&      Form  read_reprocessing_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM read_reprocessing_data.

  SELECT * INTO TABLE it_list
           FROM ztmm_roh_stock
          WHERE flag EQ 'E'.

ENDFORM.                    " read_reprocessing_data
*&---------------------------------------------------------------------*
*&      Form  execute_rfc_with_eai
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM execute_rfc_with_eai.

*  IF r1 EQ 'X'.
* execute rfc fm on all data
  PERFORM exe_rfc.
*  ENDIF.
* update ztable 'ztmm_roh_stock'
  PERFORM modify_ztable.

ENDFORM.                    " execute_rfc_with_eai
*&---------------------------------------------------------------------*
*&      Form  exe_rfc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM exe_rfc.

  DATA : lw_msgtxt(100),
         lw_tabix  LIKE sy-tabix.
  CLEAR: lw_msgtxt, lw_tabix.

  CHECK NOT it_list[] IS INITIAL.
  CALL FUNCTION 'Z_FMM_ROH_STOCK'
    DESTINATION              c_dest
    TABLES
      et_ztmm_roh_stock      = it_list
    EXCEPTIONS
      communication_failure  = 1  MESSAGE lw_msgtxt
      system_failure         = 2  MESSAGE lw_msgtxt.

  LOOP AT it_list.
    lw_tabix = sy-tabix.
    IF it_list-zzret = 'S'.
      it_list-flag = it_list-zzret.
      MODIFY it_list INDEX lw_tabix.
    ELSE.
      it_list-zzret = 'E'.
      it_list-flag  = it_list-zzret.
      IF lw_msgtxt NE ''.
        it_list-zmsg  = lw_msgtxt.
      ENDIF.
      MODIFY it_list INDEX lw_tabix.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " exe_rfc
*&---------------------------------------------------------------------*
*&      Form  modify_ztable
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM modify_ztable.

  MODIFY ztmm_roh_stock FROM TABLE it_list.
  IF sy-subrc EQ 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

ENDFORM.                    " modify_ztable
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------
FORM display_data.

  w_repid = sy-repid.
  CLEAR : it_fieldcat[], wa_events[],
          wa_list_top_of_page[]     .
* set fields
  PERFORM fieldcat_init.
* set event
  PERFORM eventtab_build USING wa_events[].
* set list heading
  PERFORM comment_build  USING wa_list_top_of_page[].
* call fun
  PERFORM alv_display.


ENDFORM.                    " display_data
*&---------------------------------------------------------------------*
*&      Form  create_interface_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM create_interface_log.

  CLEAR : wa_ztca_if_log, w_total.
  DESCRIBE TABLE it_list LINES w_total.
  LOOP AT it_list.
    IF     it_list-flag = 'S'.
      wa_ztca_if_log-zsucc = wa_ztca_if_log-zsucc + 1.
    ELSEIF it_list-flag = 'E'.
      wa_ztca_if_log-error = wa_ztca_if_log-error + 1.
    ENDIF.
  ENDLOOP.

  CHECK w_total <> 0.
  wa_ztca_if_log-tcode    = 'ZMMI09'.
*  wa_ZTCA_IF_LOG-ZSLNO    = WA_JOB-SLNO.
*  wa_ZTCA_IF_LOG-JOBCOUNT = WA_JOB-INT.
  wa_ztca_if_log-total    = w_total.
  wa_ztca_if_log-erdat    = sy-datum. "Created on.
  wa_ztca_if_log-erzet    = sy-uzeit. "Created time.
  wa_ztca_if_log-ernam    = sy-uname. "Created by.

  CALL FUNCTION 'Z_FCA_EAI_INTERFACE_LOG'
    EXPORTING
      i_ztca_if_log              = wa_ztca_if_log
*   IMPORTING
*     E_ZTCA_IF_LOG              =
   EXCEPTIONS
     update_failed              = 1
     number_range_error         = 2
     tcode_does_not_exist       = 3
     OTHERS                     = 4
            .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

** message
*  IF     w_total  =  wa_ztca_if_log-zsucc.
*    MESSAGE i028.
*  ELSEIF w_total  =  wa_ztca_if_log-error.
*    MESSAGE i029.
*  ELSE.
*    MESSAGE i029.
*  ENDIF.

ENDFORM.                    " create_interface_log
*&---------------------------------------------------------------------*
*&      Form  fieldcat_init
*&---------------------------------------------------------------------*
FORM fieldcat_init.

  build_fieldcat  'ZMATNR'  'ZMATNR'  'X'  space  space  'Material'
                  'Material'  'Material'  '18'.
  build_fieldcat  'FLAG'  'FLAG'  ''  ''  ''  'Return flag'
                  'Return flag' 'Return flag'  '1'.
  build_fieldcat  'ZMSG'  'ZMSG'  ''  ''  ''  'Return message'
                  'Return message' 'Return message'  '220'.

ENDFORM.                    " fieldcat_init
*&---------------------------------------------------------------------*
*&      Form  eventtab_build
*&---------------------------------------------------------------------*
FORM eventtab_build  USING e03_lt_events TYPE slis_t_event.

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
    MOVE c_formname_top_of_page TO ls_event-form.
    APPEND ls_event TO e03_lt_events.
  ENDIF.

ENDFORM.                    " eventtab_build
*&---------------------------------------------------------------------*
*&      Form  comment_build
*&---------------------------------------------------------------------*
FORM comment_build USING lt_top_of_page TYPE slis_t_listheader.

  DATA: ls_line TYPE slis_listheader.
  DATA: info_txt(50).

  CLEAR ls_line.
  ls_line-typ  = 'H'.
* LS_LINE-KEY:  not used for this type
  ls_line-info = text-h01.
  APPEND ls_line TO lt_top_of_page.

*Date Selection Range Display
  CLEAR info_txt.
*  INFO_TXT+0(4)   = 'Date'    .
  info_txt+5(10)  = sy-datum.
  CLEAR ls_line.
  ls_line-typ  = 'S'.
  ls_line-key  = 'Date:'.
  ls_line-info = info_txt.
  APPEND ls_line TO lt_top_of_page.

ENDFORM.                    " comment_build
*&---------------------------------------------------------------------*
*&      Form  alv_display
*&---------------------------------------------------------------------*
FORM alv_display.

  CHECK NOT it_list[] IS INITIAL.
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
      t_outtab                     = it_list
* EXCEPTIONS
*   PROGRAM_ERROR                  = 1
*   OTHERS                         = 2
            .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " alv_display
*&---------------------------------------------------------------------*
*&      Form  set_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_date.

  IF     s_budat-high EQ '' AND s_budat-low EQ ''.
    s_budat-high  =  sy-datum.
    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
         EXPORTING
              date      = s_budat-high
              days      = '1'
              months    = '0'
              signum    = '-'
              years     = '0'
         IMPORTING
              calc_date = s_budat-low.
    s_budat-option  =  'EQ'.
    s_budat-sign    =  'I'.
    APPEND s_budat.
  ELSEIF s_budat-high NE '' AND s_budat-low EQ ''.
    s_budat-low     =  sy-datum.
    s_budat-option  =  'BT'.
    s_budat-sign    =  'I'.
    APPEND s_budat.
  ENDIF.

ENDFORM.                    " set_date
*&---------------------------------------------------------------------*
*&      Form  read_Movement
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_movement.

  IF     r1 EQ 'X'.
* read all data
    PERFORM read_move_pro_data.
  ELSE.
* read error data
    PERFORM read_reprocessing_data.
  ENDIF.

ENDFORM.                    " read_Movement
*&---------------------------------------------------------------------*
*&      Form  read_stock
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_stock.

  IF     r1 EQ 'X'.
* read all data
    PERFORM read_stock_pro_data.
  ELSE.
* read error data
    PERFORM read_reprocessing_data.
  ENDIF.

ENDFORM.                    " read_stock
*&---------------------------------------------------------------------*
*&      Form  read_stock_pro_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM read_stock_pro_data.

  CLEAR :  it_stock[], it_stock,
           it_list[], it_list.

  SELECT matnr  werks  lgort  clabs  cinsm
         INTO CORRESPONDING FIELDS OF TABLE it_stock
         FROM mchb
        WHERE matnr  LIKE 'AM%'.

  LOOP AT it_stock.
    it_list-zwerks   =  it_stock-werks.
    it_list-zlgort   =  it_stock-lgort.
    it_list-doc_type =  '000'.
    it_list-zmatnr   =  it_stock-matnr.
    it_list-zlabst   =  it_stock-clabs.
    it_list-zinsme   =  it_stock-cinsm.
*    it_list-zbdmng   =  it_stock-clabs.
* vendor
    SELECT SINGLE lifnr INTO it_list-zlifnr
           FROM mcha
          WHERE matnr  EQ  it_stock-matnr
          AND   werks  EQ  it_stock-werks.
* UOM
    SELECT SINGLE meins INTO it_list-zmeins
           FROM mara
          WHERE matnr  EQ  it_stock-matnr.
* read characterisitc
    CALL FUNCTION 'Z_FMM_GET_CHARACT'
         EXPORTING
              i_matnr        = it_list-zmatnr
         IMPORTING
              es_zsmm_class  = wa_zsmm_class
         EXCEPTIONS
              data_not_found = 1
              OTHERS         = 2.
    IF sy-subrc EQ 0.
      it_list-zprop      =  wa_zsmm_class-zprop.
      it_list-zcoating   =  wa_zsmm_class-zcoating.
      it_list-zthick     =  wa_zsmm_class-zthick.
      it_list-zwidth     =  wa_zsmm_class-zwidth.
      it_list-zlength    =  wa_zsmm_class-zlength.
    ENDIF.
    COLLECT it_list. CLEAR it_list.
  ENDLOOP.


ENDFORM.                    " read_stock_pro_data
