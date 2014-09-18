*&----------------------------------------------------------------------
*& Development ID :
*& Program ID     : ZMMI01140T
*& Program Name   : Request of Unit Price Registration to G-POS
*& Created by     : T00304
*& Created on     : 07.31.2013
*& Issue Doc No.  :
*&
*& Modification Log
*& Date        Developer Issue No Description
*&======================================================================
*& RFC func. : Z_MM_IF_OB_10_NO_PRICE_MAT
*& Stru.     : ZMMS_GS001
*&----------------------------------------------------------------------

REPORT zmmi01140t MESSAGE-ID zmpp.

TABLES : mseg, mara, mast, eord,
         a018.                   "Material Info Record

*- ALV
TYPE-POOLS: slis.
DATA: gt_fieldcat         TYPE slis_t_fieldcat_alv,
      gs_layout           TYPE slis_layout_alv,
      gs_sort             TYPE slis_sortinfo_alv,
      gt_sort             TYPE slis_t_sortinfo_alv,
      gs_light            TYPE lvc_s_layo,
      gs_print            TYPE slis_print_alv,
      gt_sp_group         TYPE slis_t_sp_group_alv,
      gt_events           TYPE slis_t_event,
      gs_events           LIKE  LINE OF gt_events,
      g_save              VALUE 'A',
      gx_variant          LIKE disvariant,
      g_variant           LIKE disvariant.

DATA : ls_title         TYPE slis_listheader, "alv header
       alv_t_listheader TYPE slis_t_listheader.

DATA : g_extab          TYPE slis_t_extab,
       g_extab_ln       LIKE   LINE  OF  g_extab.

DATA : g_user_command  TYPE slis_formname VALUE 'USER_COMMAND'.
DATA : t_colinfo_table TYPE slis_t_specialcol_alv WITH HEADER LINE.
DATA : g_repid         LIKE sy-repid.


DATA : BEGIN OF it_data OCCURS 0.
        INCLUDE STRUCTURE zmms_gs001.
DATA : mark TYPE c.
DATA : END OF it_data.

DATA : it_send    LIKE zmms_gs001 OCCURS 0 WITH HEADER LINE.

*- RETURN MESSAGE
DATA : e_return TYPE zmms0053.
DATA : l_msgtxt(200).

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.
PARAMETERS : p_bukrs     LIKE mseg-bukrs MEMORY ID buk OBLIGATORY
                         DEFAULT 'H201' .
SELECT-OPTIONS : s_werks FOR mseg-werks MEMORY ID wrk OBLIGATORY,
*                                        DEFAULT 'KVA1' NO INTERVALS,
                 s_mtart FOR mara-mtart NO INTERVALS,
                 s_matnr FOR mara-matnr .
*                 s_aeszn FOR mara-aeszn.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-b02.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(15) text-t01.
PARAMETERS : p_send RADIOBUTTON GROUP r1 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 30(15) text-t02.
PARAMETERS : p_alv  RADIOBUTTON GROUP r1.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b2.

*----------------------------------------------------------------------*
* INITIALIZATION.
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM init.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN.
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  PERFORM check_if_request USING p_send
                                 sy-ucomm.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT.
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

*----------------------------------------------------------------------*
* START-OF-SELECTION.
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM select_data.
  PERFORM modify_data.

*----------------------------------------------------------------------*
* END-OF-SELECTION.
*----------------------------------------------------------------------*
END-OF-SELECTION.
  IF p_send = 'X'.
    PERFORM pro_batch.
  ELSE.
    PERFORM pro_alv.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
FORM select_data .
  DATA : BEGIN OF lt_temp OCCURS 0.
          INCLUDE STRUCTURE zmms_gs001.
  DATA: dispo LIKE marc-dispo,
        normt LIKE mara-normt,
        werks LIKE marc-werks,
        matkl LIKE mara-matkl.
  DATA: END OF lt_temp.
  DATA: l_kd(2).

  l_kd = 'KD'.

  SELECT a~matnr
         a~mtart
         a~profl AS labor
         c~dispo
         a~normt
         c~werks
         a~matkl
         INTO CORRESPONDING FIELDS OF TABLE lt_temp
         FROM mara AS a INNER JOIN marc AS c
          ON ( a~mandt = c~mandt AND
               a~matnr = c~matnr )
         WHERE a~matnr IN s_matnr
           AND a~mtart IN s_mtart
*           AND a~aeszn IN s_aeszn
           AND c~werks IN s_werks
           AND ( c~mmsta = '11' OR c~mmsta = '12')
           AND ( a~mstae = '11' OR a~mstae = '12')
*           AND a~matkl <> 'INIT'
           AND NOT EXISTS
                   ( SELECT matnr FROM eina
                       WHERE matnr = a~matnr
                         AND werks = c~werks
                         AND loekz = space  ).

  SORT lt_temp BY matnr werks DESCENDING.
  .
  DELETE ADJACENT DUPLICATES FROM lt_temp COMPARING matnr.

  LOOP AT lt_temp.
    MOVE-CORRESPONDING lt_temp TO it_send.
    IF lt_temp-dispo = 'M01' AND lt_temp-normt = 'M'.
      it_send-zflag = 'M'.
    ELSE.
      it_send-zflag = 'E'.
    ENDIF.
    IF lt_temp-labor = ' ' OR lt_temp-labor = 'M'.
      IF lt_temp-matkl CA l_kd.
        it_send-labor = 'K'.
      ELSE.
        it_send-labor = 'V'.
      ENDIF.
    ENDIF.
    APPEND it_send.
  ENDLOOP.
ENDFORM.                    " SELECT_DATA
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
FORM modify_data .
  DATA: l_date LIKE sy-datum.

  l_date = sy-datum + 3.
  LOOP AT it_send.
    it_send-bukrs = p_bukrs.

*// Demand Plan + 3 day .
* Check Components List for Vehicle
* maybe exist it is emergency sending.

    SELECT SINGLE mandt INTO sy-mandt
           FROM resb
           WHERE "werks = it_send-werks
              matnr = it_send-matnr
             AND bdter BETWEEN sy-datum AND l_date
             and bdmng > 0.

    IF sy-subrc EQ 0.
      it_send-zfupr = 'X'.
    ELSE.
      SELECT SINGLE mandt INTO sy-mandt
       FROM eban
       WHERE matnr = it_send-matnr
         AND lfdat BETWEEN sy-datum AND l_date.
      IF sy-subrc = 0.
        it_send-zfupr = 'X'.
      ENDIF.
    ENDIF.

    it_send-wdatu = sy-datum.

    CASE it_send-zfupr.
      WHEN 'X'.            " Emergency
        it_send-zdprl = it_send-wdatu + 1.
        it_send-ztprl = '120000'.
      WHEN OTHERS.
        it_send-zdprl = it_send-wdatu + 3.
        it_send-ztprl = '150000'.


    ENDCASE.

    MODIFY it_send.
  ENDLOOP.
ENDFORM.                    " MODIFY_DATA
*&---------------------------------------------------------------------*
*&      Form  PRO_BATCH
*&---------------------------------------------------------------------*
FORM pro_batch .
  DATA : v_dest(30).
  v_dest = 'WMPM01'.
  CLEAR : e_return.

  CHECK NOT it_send[] IS INITIAL.

*  SELECT SINGLE dest INTO (v_dest)
*          FROM zdest
*          WHERE sy_sysid = sy-sysid
*            AND sy_mandt = sy-mandt.
*
  IF v_dest IS INITIAL.
    MESSAGE e000 WITH 'EAI setting is incorrect.'
                      'Please check ZDEST table'.
  ENDIF.

  CALL FUNCTION 'Z_MM_IF_OB_10_NO_PRICE_MAT' DESTINATION v_dest
    IMPORTING
      e_return              = e_return
    TABLES
      t_data                = it_send
    EXCEPTIONS
      communication_failure = 1  MESSAGE l_msgtxt
      system_failure        = 2  MESSAGE l_msgtxt.

  IF e_return-type = 'S' AND  sy-subrc = 0.   "Success
    PERFORM save_log  USING 'S' 'Success'    ''.
    MESSAGE s000 WITH 'Interface : Success'.
  ELSE.
    PERFORM save_log  USING 'E' e_return-message l_msgtxt.
    MESSAGE s000 WITH  e_return-message l_msgtxt
                 DISPLAY LIKE 'I'.
  ENDIF.

  PERFORM pro_alv.

ENDFORM.                    " PRO_BATCH

*&---------------------------------------------------------------------*
*&      Form  SAVE_LOG
*&---------------------------------------------------------------------*
FORM save_log  USING    p_type p_msg1 p_msg2.

  DATA : lt_zmmt_gs001 LIKE zmmt_gs001 OCCURS 10 WITH HEADER LINE.

  DATA : l_zseq(10) TYPE n.

*// Current date max Seqence number.

  SELECT zseq INTO l_zseq
    FROM zmmt_gs001
    UP TO 1 ROWS
    WHERE zdate = sy-datum
    ORDER BY zseq DESCENDING.
  ENDSELECT.

  LOOP AT it_send.
    MOVE-CORRESPONDING it_send TO lt_zmmt_gs001.

    lt_zmmt_gs001-zdate = sy-datum.
    lt_zmmt_gs001-zseq  = l_zseq + sy-tabix.
    lt_zmmt_gs001-ztime = sy-uzeit.
    lt_zmmt_gs001-ernam = sy-uname.
    lt_zmmt_gs001-zrslt = p_type.
    IF p_type = 'E'.
      IF p_msg1 IS NOT INITIAL.
        lt_zmmt_gs001-zmsg  = p_msg1.
      ELSE.
        lt_zmmt_gs001-zmsg  = p_msg2.
      ENDIF.
    ENDIF.

    APPEND lt_zmmt_gs001.
    CLEAR : lt_zmmt_gs001.

  ENDLOOP.

  INSERT zmmt_gs001 FROM TABLE lt_zmmt_gs001
                             ACCEPTING DUPLICATE KEYS .
  COMMIT WORK AND WAIT.

ENDFORM.                    " SAVE_LOG
*&--------------------------------------------------------------------*
*&      Form  PRO_ALV
*&---------------------------------------------------------------------*
FORM pro_alv .
  CHECK sy-batch EQ space.
  it_data[] = it_send[].

  PERFORM layout_build       USING   gs_layout.
*  PERFORM sorttab_build      USING   gt_sort.
  PERFORM fieldcat           TABLES  gt_fieldcat
                             USING   'ZMMS_GS001'.

  PERFORM list_header_write USING alv_t_listheader[].
  PERFORM append_alv_event  CHANGING   gt_events.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = g_repid
      i_callback_pf_status_set = 'PF_STATUS'
      i_callback_user_command  = g_user_command
      is_layout                = gs_layout
      it_fieldcat              = gt_fieldcat
      it_sort                  = gt_sort
      i_save                   = g_save
      it_events                = gt_events[]
    TABLES
      t_outtab                 = it_data[]
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

ENDFORM.                    " PRO_ALV
*&---------------------------------------------------------------------*
*&      Form  LAYOUT_BUILD
*&---------------------------------------------------------------------*
FORM layout_build  USING  p_layout TYPE slis_layout_alv.

  p_layout-zebra             = 'X'.
  p_layout-colwidth_optimize = 'X'.
*  p_layout-key_hotspot = 'X'.
  p_layout-box_fieldname  =    'MARK'.  "SELECTION FIELD
*  p_layout-coltab_fieldname = 'COL_COLOR'. "color field of itabe
*  p_layout-cell_merge        = 'X'.
*  p_layout-detail_popup      = 'X'.
*  p_layout-detail_titlebar   = sy-title.
*  p_layout-no_subtotals      = ''.

ENDFORM.                    " LAYOUT_BUILD
*&---------------------------------------------------------------------*
*&      Form  SORTTAB_BUILD
*&---------------------------------------------------------------------*
FORM sorttab_build  USING   p_sort TYPE slis_t_sortinfo_alv.

  CLEAR: gs_sort, p_sort[].


  gs_sort-spos      = '2'.
  gs_sort-tabname   = 'IT_SEND'.
  gs_sort-fieldname = 'MATNR'.
  gs_sort-up        = 'X'.
  gs_sort-group     = 'BL'.
  gs_sort-subtot    = ''.
  APPEND gs_sort TO p_sort.

ENDFORM.                    " SORTTAB_BUILD
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT
*&---------------------------------------------------------------------*
FORM fieldcat  TABLES   pt_fieldcat TYPE  slis_t_fieldcat_alv
               USING    p_name      TYPE  slis_tabname.

  DATA: l_datum(08).

  sy-datum = sy-datum + 1.
  MOVE sy-datum TO l_datum.
  SET PARAMETER ID 'ALVBUFFER' FIELD l_datum.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = g_repid
      i_structure_name   = p_name
      i_bypassing_buffer = 'X'
*     i_internal_tabname = p_name
*     i_inclname         = g_repid
    CHANGING
      ct_fieldcat        = pt_fieldcat[].



ENDFORM.                    " FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  LIST_HEADER_WRITE
*&---------------------------------------------------------------------*
FORM list_header_write  USING alv_t_listheader TYPE slis_t_listheader.

  CLEAR   : ls_title, alv_t_listheader.
  REFRESH : alv_t_listheader.

  DATA : h_title(30), s_title(60),  a_title(60).
  DATA : lv_date(10),
         lv_lines(10),
         lv_count TYPE i .

  ls_title-typ = 'S'. "(H:Header, S:Selection, A:Action)
  ls_title-key = 'Company Code '.
  ls_title-info = p_bukrs.
  APPEND ls_title TO alv_t_listheader.

  ls_title-typ = 'S'.
  WRITE  sy-datum TO lv_date.
  ls_title-key = 'Date'.
  ls_title-info = lv_date.
  APPEND ls_title TO alv_t_listheader.

  IF 'X' = p_send.
    ls_title-typ = 'S'.
    WRITE  sy-datum TO lv_date.
    ls_title-key = 'TYPE'.
    ls_title-info = e_return-type.
    APPEND ls_title TO alv_t_listheader.

    ls_title-typ = 'S'.
    WRITE  sy-datum TO lv_date.
    ls_title-key = 'Message'.
    ls_title-info = e_return-message.
    APPEND ls_title TO alv_t_listheader.
  ENDIF.


  DESCRIBE TABLE it_send LINES  lv_count.
  WRITE lv_count TO lv_lines.
  ls_title-typ = 'S'.
  ls_title-key = ''.
  CONCATENATE '*Lines : ' lv_lines  INTO ls_title-info.
  APPEND ls_title TO alv_t_listheader.

ENDFORM.                    " LIST_HEADER_WRITE
*&---------------------------------------------------------------------*
*&      Form  APPEND_ALV_EVENT
*&---------------------------------------------------------------------*
FORM append_alv_event  CHANGING p_alv_event TYPE slis_t_event.
* TOP-OF-PAGE Event

  DATA ls_events TYPE slis_alv_event.
  ls_events-name  =  'TOP_OF_PAGE'.
  ls_events-form  =  'TOP_OF_PAGE'.
  APPEND ls_events TO p_alv_event.

ENDFORM.                    " APPEND_ALV_EVENT

*&---------------------------------------------------------------------
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------
FORM top_of_page.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = alv_t_listheader.

ENDFORM. " TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  PF_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM pf_status USING lt_extab  TYPE slis_t_extab  .

  SET PF-STATUS 'PF_001'
    EXCLUDING lt_extab.

ENDFORM.                    "PF_STATUS
*&--------------------------------------------------------------------*
*&      Form  ALV_USER_COMMAND
*&--------------------------------------------------------------------*
FORM  user_command USING ucomm    LIKE sy-ucomm
                    p_selfield    TYPE slis_selfield.
* double click : UCOMM = &IC1
  CASE ucomm.
    WHEN 'ZEMG'.
      PERFORM z_user_set_emg_set USING 'X'.
      p_selfield-refresh    = 'X'.
      p_selfield-col_stable = 'X'.
      p_selfield-row_stable = 'X'.

    WHEN 'ZNOR'.
      PERFORM z_user_set_emg_set USING ' '.
      p_selfield-refresh    = 'X'.
      p_selfield-col_stable = 'X'.
      p_selfield-row_stable = 'X'.

    WHEN 'ZSEND'.
      READ TABLE it_data WITH KEY mark = 'X'.
      IF sy-subrc NE 0.
        MESSAGE e021  .
      ENDIF.
      PERFORM check_if_request USING p_alv 'ONLI'.

      PERFORM z_send_request_gpos  .
      p_selfield-refresh    = 'X'.
      p_selfield-col_stable = 'X'.
      p_selfield-row_stable = 'X'.

    WHEN OTHERS.

  ENDCASE.

ENDFORM.                    "user_command
*&---------------------------------------------------------------------*
*&      Form  INIT
*&---------------------------------------------------------------------*
FORM init .

  s_mtart-low    = 'ROH'.
  s_mtart-sign   = 'I'.
  s_mtart-option = 'EQ'.
  APPEND s_mtart.

*  s_mtart-low    = 'ROH1'.
*  s_mtart-sign   = 'I'.
*  s_mtart-option = 'EQ'.
*  APPEND s_mtart.

*  s_mtart-low    = 'ERSA'.
*  s_mtart-sign   = 'I'.
*  s_mtart-option = 'EQ'.
*  APPEND s_mtart.

  s_werks-low    = 'P001'.
  s_werks-sign   = 'I'.
  s_werks-option = 'EQ'.
  APPEND s_werks.

  s_werks-low    = 'E001'.
  APPEND s_werks.

  s_werks-low    = 'E002'.
  APPEND s_werks.

*  s_aeszn-low    = '02'.
*  s_aeszn-sign   = 'I'.
*  s_aeszn-option = 'EQ'.
*  APPEND s_aeszn.
*  s_aeszn-low    = '11'.
*  APPEND s_aeszn.
*
*  s_aeszn-low    = '12'.
*  APPEND s_aeszn.
*
*  s_aeszn-low    = '13'.
*  APPEND s_aeszn.
*
*  s_aeszn-low    = '14'.
*  APPEND s_aeszn.

  g_repid = sy-repid.

ENDFORM.                    " INIT
*&---------------------------------------------------------------------*
*&      Form  CAL_MONTH_LASTDATE
*&---------------------------------------------------------------------*
FORM cal_month_lastdate  USING    p_spmon
                                  p_edate.
  DATA : lv_date TYPE sy-datum.
  CONCATENATE p_spmon '01' INTO lv_date.

  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = lv_date
    IMPORTING
      last_day_of_month = p_edate
    EXCEPTIONS
      day_in_no_date    = 1
      OTHERS            = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " CAL_MONTH_LASTDATE
*&---------------------------------------------------------------------*
*&      Form  PROGRESS_BAR
*&---------------------------------------------------------------------*
FORM progress_bar  USING    p_tabix
                            p_lines.
  DATA : lv_per TYPE i .
  DATA : lv_text(50).
  DATA : lv_mode TYPE i.
  DATA : lv_lines(7) TYPE n,
         lv_tabix(7)  TYPE n.

  lv_lines  = p_lines.
  lv_tabix  = p_tabix.

  lv_per = ( p_tabix * 100 / p_lines ) .
  CONCATENATE 'Processing : ' lv_tabix ' / ' lv_lines INTO lv_text.
  lv_mode =  lv_per MOD 5.
  IF lv_mode   =  0.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = lv_per
        text       = lv_text.
  ENDIF.
ENDFORM.                    " PROGRESS_BAR
*&---------------------------------------------------------------------*
*&      Form  CHECK_IF_REQUEST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_if_request USING p_flg
                            p_com.

  DATA : lv_answer TYPE c.

  CHECK sy-batch EQ space AND
        p_flg    EQ 'X'   AND
        'ONLI'   EQ p_com .

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirm'
      text_question         = 'Do you want send Request ?'
      display_cancel_button = ' '
    IMPORTING
      answer                = lv_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  IF lv_answer NE '1'.
    MESSAGE e001(zmmm) WITH 'Cancle' DISPLAY LIKE 'S'.
  ENDIF.
ENDFORM.                    " CHECK_IF_REQUEST
*&---------------------------------------------------------------------*
*&      Form  Z_USER_SET_EMG_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_user_set_emg_set USING p_emg.

  CASE p_emg.
    WHEN 'X'.
      it_data-zdprl = sy-datum + 1.
      it_data-ztprl = '120000'.
      it_data-zfupr = 'X'.
      MODIFY it_data TRANSPORTING zdprl ztprl zfupr
                     WHERE mark = 'X'.
    WHEN OTHERS.

      it_data-zdprl = sy-datum + 3.
      it_data-ztprl = '150000'.
      it_data-zfupr = ' '.
      MODIFY it_data TRANSPORTING zdprl ztprl zfupr
                     WHERE mark = 'X'.

  ENDCASE.


ENDFORM.                    " Z_USER_SET_EMG_SET
*&---------------------------------------------------------------------*
*&      Form  Z_SEND_REQUEST_GPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_send_request_gpos .
  DATA : v_dest(30).
  v_dest = 'WMPM01'.
  CLEAR : e_return.

  CHECK NOT it_send[] IS INITIAL.

*// Selected entry .
  REFRESH it_send. CLEAR it_send.
  LOOP AT it_data WHERE mark = 'X'.
    MOVE-CORRESPONDING it_data TO it_send.
    APPEND it_send. CLEAR it_send.

  ENDLOOP.
*
*  SELECT SINGLE dest INTO (v_dest)
*          FROM zdest
*          WHERE sy_sysid = sy-sysid
*            AND sy_mandt = sy-mandt.
*
  IF v_dest IS INITIAL.
    MESSAGE e000 WITH 'EAI setting is incorrect.'
                      'Please check ZDEST table'.
  ENDIF.

  CALL FUNCTION 'Z_MM_IF_OB_10_NO_PRICE_MAT' DESTINATION v_dest
    IMPORTING
      e_return              = e_return
    TABLES
      t_data                = it_send
    EXCEPTIONS
      communication_failure = 1  MESSAGE l_msgtxt
      system_failure        = 2  MESSAGE l_msgtxt.

  IF e_return-type = 'S' AND  sy-subrc = 0.   "Success
    PERFORM save_log  USING 'S' 'Success'    ''.
    MESSAGE s000 WITH 'Interface : Success'.
  ELSE.
    PERFORM save_log  USING 'E' e_return-message l_msgtxt.
    MESSAGE s000 WITH  e_return-message l_msgtxt
                 DISPLAY LIKE 'I'.
  ENDIF.



ENDFORM.                    " Z_SEND_REQUEST_GPOS
