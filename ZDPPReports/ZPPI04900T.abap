*&----------------------------------------------------------------------
*& Development ID :
*& Program ID     : ZPPI04900T
*& Program Name   : Send Stamping Production Result to GETIS
*& Created by     :
*& Created on     : 08.28.2013
*& Issue Doc No.  : copied from KMMG
*&
*& Modification Log
*& Date        Developer Issue No Description
*&======================================================================
*& RFC func. : Z_STAMPING_RESULT_TO_DETIS
*& Stru.     : ZSPP_GETIS
*&----------------------------------------------------------------------

REPORT  zppi04900t MESSAGE-ID zmpp.

TABLES : zspp_getis.


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
       g_extab_ln       TYPE slis_extab.

DATA : g_user_command  TYPE slis_formname VALUE 'USER_COMMAND'.
DATA : t_colinfo_table TYPE slis_t_specialcol_alv WITH HEADER LINE.
DATA : g_repid         LIKE sy-repid.

*- RETURN MESSAGE
DATA : ls_return TYPE zmms0053.
DATA : lv_msgtxt(200).

DATA : BEGIN OF it_data OCCURS 0.
        INCLUDE STRUCTURE zspp_getis.
        INCLUDE STRUCTURE zmms0053.
DATA :
       mark TYPE c.
DATA : END OF it_data.


DATA : it_send  LIKE zspp_getis OCCURS 0 WITH HEADER LINE.
DATA : BEGIN OF  it_send_tmp OCCURS 0.
        INCLUDE STRUCTURE zspp_getis.
DATA :     pqty  LIKE ztpppr-pqty,
           prqty LIKE ztpppr-prqty,
           psqty LIKE ztpppr-psqty.
DATA : END OF it_send_tmp .


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.
SELECT-OPTIONS : s_werks FOR zspp_getis-werks MEMORY ID wrk OBLIGATORY
                                        DEFAULT 'HVA1' NO INTERVALS,
                 s_budat FOR zspp_getis-budat  OBLIGATORY  .
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
  IF it_send_tmp[] IS INITIAL.
    MESSAGE s001 WITH 'There is No data'.
    STOP.
  ELSE.
    PERFORM modify_data.
  ENDIF.

  IF p_send = 'X'.
    PERFORM pro_batch.
  ELSE.
    PERFORM pro_alv.
  ENDIF.

*----------------------------------------------------------------------*
* END-OF-SELECTION.
*----------------------------------------------------------------------*
END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init .
  CLEAR : s_budat[], s_budat.

  g_repid = sy-repid.
  s_budat-sign     = 'I'.
  s_budat-option   = 'EQ'.
  s_budat-low = sy-datum - 1.
  APPEND s_budat.

ENDFORM.                    " INIT
*&---------------------------------------------------------------------*
*&      Form  CHECK_IF_REQUEST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_SEND  text
*      -->P_SY_UCOMM  text
*----------------------------------------------------------------------*
FORM check_if_request  USING p_flg
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
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_data .

*  SELECT b~zbudat AS budat a~matnr b~werks SUM( b~menge )
*      INTO CORRESPONDING FIELDS OF TABLE it_send
*    FROM ztco_mat AS a INNER JOIN mseg AS b
*            ON a~matnr  = b~matnr
*    WHERE  a~zresp  =  'STAMPING'
*      AND  a~mtart  =  'HALB'
*      AND  b~bwart  =  '101'
*      AND  b~zbudat IN s_budat
*    GROUP BY b~zbudat b~werks a~matnr.

  SELECT rdate AS  budat    pnlno AS matnr  SUM( pqty ) AS pqty
         SUM( prqty ) AS prqty SUM( psqty ) AS psqty
  INTO CORRESPONDING FIELDS OF TABLE it_send_tmp
  FROM  ztpppr
  WHERE rdate IN s_budat
  GROUP BY rdate  pnlno .


ENDFORM.                    " SELECT_DATA
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_data .
  TYPES : BEGIN OF ty_mat ,
           matnr LIKE mara-matnr,
           maktx LIKE makt-maktx,
          END   OF ty_mat.

  DATA : lt_mat TYPE SORTED TABLE OF
                                ty_mat WITH UNIQUE KEY matnr
                                       WITH HEADER LINE.

  CLEAR : it_send[], it_send.

  FIELD-SYMBOLS : <fs_line> LIKE LINE OF it_send_tmp.

  SORT it_send_tmp BY matnr.

  SELECT a~matnr
         t~maktx
         INTO CORRESPONDING FIELDS OF TABLE lt_mat
         FROM mara AS a INNER JOIN makt  AS t
          ON ( a~matnr = t~matnr )

         FOR ALL ENTRIES IN it_send_tmp
         WHERE a~matnr = it_send_tmp-matnr
           AND t~spras = sy-langu .

  LOOP AT it_send_tmp ASSIGNING <fs_line>.
    READ TABLE lt_mat WITH TABLE KEY matnr = <fs_line>-matnr.
    IF sy-subrc EQ 0.
      <fs_line>-maktx = lt_mat-maktx.
    ENDIF.
*    IF <fs_line>-werks = '1'.
    <fs_line>-werks = 'HVA1'. ""P001?
*    ENDIF.

    <fs_line>-total_qty =
            <fs_line>-pqty + <fs_line>-prqty + <fs_line>-psqty.
  ENDLOOP.

  LOOP AT it_send_tmp.
    MOVE-CORRESPONDING it_send_tmp TO it_send.
    APPEND it_send.
  ENDLOOP.

  SORT it_send BY matnr budat.
ENDFORM.                    " MODIFY_DATA
*&---------------------------------------------------------------------*
*&      Form  PRO_BATCH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pro_batch .
  DATA : v_dest(30) VALUE 'WMHR01'.   "Interface Destination.



  CALL FUNCTION 'Z_STAMPING_RESULT_TO_GETIS' DESTINATION v_dest
    IMPORTING
      e_return              = ls_return
    TABLES
      t_table               = it_send
    EXCEPTIONS
      communication_failure = 1  MESSAGE lv_msgtxt
      system_failure        = 2  MESSAGE lv_msgtxt.

  IF ls_return-type = 'S' AND  sy-subrc = 0.   "Success
    PERFORM save_log  USING 'S' 'Success'    ''.
    MESSAGE s000 WITH 'Interface : Success'.
  ELSE.
    PERFORM save_log  USING 'E' ls_return-message lv_msgtxt.
    MESSAGE s000 WITH  ls_return-message lv_msgtxt
                 DISPLAY LIKE 'I'.
  ENDIF.

  PERFORM pro_alv.

ENDFORM.                    " PRO_BATCH
*&---------------------------------------------------------------------*
*&      Form  PRO_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pro_alv .
  CHECK sy-batch EQ space.

  it_data[] = it_send[].


  IF ls_return IS NOT INITIAL.
    it_data-type     =  ls_return-type.
    it_data-message  =  ls_return-message.

    MODIFY it_data TRANSPORTING type message
                   WHERE type IS INITIAL.
  ENDIF.

  REFRESH gt_fieldcat.

  PERFORM layout_build       USING   gs_layout.
  PERFORM fieldcat           TABLES  gt_fieldcat
                             USING   'ZSPP_GETIS'.

  PERFORM fieldcat           TABLES  gt_fieldcat
                             USING   'ZMMS0053'.

  PERFORM fieldcat_modify TABLES gt_fieldcat.

  PERFORM list_header_write USING alv_t_listheader[].
  PERFORM append_alv_event  CHANGING   gt_events.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = g_repid
*     i_callback_pf_status_set = 'PF_STATUS'
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
*       text
*----------------------------------------------------------------------*
*      -->P_GS_LAYOUT  text
*----------------------------------------------------------------------*
FORM layout_build  USING  p_layout TYPE slis_layout_alv.

  p_layout-zebra             = 'X'.
  p_layout-colwidth_optimize = 'X'.
  p_layout-box_fieldname     = 'MARK'.  "SELECTION FIELD

ENDFORM.                    " LAYOUT_BUILD
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCAT  text
*      -->P_0328   text
*----------------------------------------------------------------------*
FORM fieldcat  TABLES   pt_fieldcat TYPE  slis_t_fieldcat_alv
               USING    p_name      TYPE  slis_tabname.

  DATA: l_datum(08).

  DATA lt_fcat TYPE slis_t_fieldcat_alv.
  FIELD-SYMBOLS <fs_fcat> TYPE slis_fieldcat_alv.

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
      ct_fieldcat        = lt_fcat[].
  APPEND LINES OF lt_fcat[] TO pt_fieldcat[].

  LOOP AT pt_fieldcat  ASSIGNING <fs_fcat>.
    <fs_fcat>-col_pos = sy-tabix.
  ENDLOOP.


ENDFORM.                    " FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  LIST_HEADER_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ALV_T_LISTHEADER[]  text
*----------------------------------------------------------------------*
FORM list_header_write  USING alv_t_listheader TYPE slis_t_listheader.

  CLEAR   : ls_title, alv_t_listheader.
  REFRESH : alv_t_listheader.

  DATA : h_title(30), s_title(60),  a_title(60).
  DATA : lv_date(10),
         lv_lines(10),
         lv_count TYPE i .

*  ls_title-typ = 'H'. "(H:Header, S:Selection, A:Action)
*  ls_title-info = 'Stamping Production result to GETIS'.
*  APPEND ls_title TO alv_t_listheader.

  ls_title-typ = 'S'. "(H:Header, S:Selection, A:Action)
  ls_title-key = 'Plant '.
  ls_title-info = s_werks-low.
  APPEND ls_title TO alv_t_listheader.

  ls_title-typ = 'S'.
  WRITE  sy-datum TO lv_date.
  ls_title-key = 'Date'.
  ls_title-info = lv_date.
  APPEND ls_title TO alv_t_listheader.

ENDFORM.                    " LIST_HEADER_WRITE
*&---------------------------------------------------------------------*
*&      Form  APPEND_ALV_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_EVENTS  text
*----------------------------------------------------------------------*
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
*&      Form  SAVE_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0515   text
*      -->P_0516   text
*      -->P_0517   text
*----------------------------------------------------------------------*
FORM save_log  USING    p_type p_msg1 p_msg2.

  DATA : lt_ztpp_getis_log LIKE ztpp_getis_log
                            OCCURS 10 WITH HEADER LINE.

  DATA : l_zseq(10) TYPE n.

*// Current date max Seqence number.
  IF p_type = 'E'.
    ls_return-type  = p_type.
    IF p_msg1 IS INITIAL .
      ls_return-message = lv_msgtxt   .
    ENDIF.
  ENDIF.

  SELECT zseq INTO l_zseq
    FROM ztpp_getis_log
    UP TO 1 ROWS
    WHERE zdate = sy-datum
    ORDER BY zseq DESCENDING.
  ENDSELECT.

  LOOP AT it_send.
    MOVE-CORRESPONDING it_send TO lt_ztpp_getis_log.

    lt_ztpp_getis_log-zdate = sy-datum.
    lt_ztpp_getis_log-zseq  = l_zseq + sy-tabix.
    lt_ztpp_getis_log-ztime = sy-uzeit.
    lt_ztpp_getis_log-ernam = sy-uname.
    lt_ztpp_getis_log-erdat = sy-datum.

    lt_ztpp_getis_log-type  = lt_ztpp_getis_log-zif_res = p_type.
    IF p_type = 'E'.

      IF p_msg1 IS NOT INITIAL.
        lt_ztpp_getis_log-message  = p_msg1.

      ELSE.
        lt_ztpp_getis_log-message  = p_msg2.
      ENDIF.

    ENDIF.

    APPEND lt_ztpp_getis_log.
    CLEAR : lt_ztpp_getis_log.
    MODIFY it_send.
  ENDLOOP.

  INSERT ztpp_getis_log FROM TABLE lt_ztpp_getis_log
                             ACCEPTING DUPLICATE KEYS .
  COMMIT WORK AND WAIT.

ENDFORM.                    " SAVE_LOG
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_MODIFY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCAT  text
*----------------------------------------------------------------------*
FORM fieldcat_modify  TABLES   pt_fieldcat TYPE  slis_t_fieldcat_alv.

  FIELD-SYMBOLS <fs_fcat> TYPE slis_fieldcat_alv.


  LOOP AT pt_fieldcat  ASSIGNING <fs_fcat>.

    CASE <fs_fcat>-fieldname.
      WHEN 'GOOD_QTY'.
        <fs_fcat>-seltext_l      =
        <fs_fcat>-seltext_m      =
        <fs_fcat>-seltext_s      =
        <fs_fcat>-reptext_ddic   = 'Good Qty'.

      WHEN 'BAD_QTY'.
        <fs_fcat>-seltext_l      =
        <fs_fcat>-seltext_m      =
        <fs_fcat>-seltext_s      =
        <fs_fcat>-reptext_ddic   = 'Bad Qty'.

      WHEN 'TOTAL_QTY'.
        <fs_fcat>-seltext_l      =
        <fs_fcat>-seltext_m      =
        <fs_fcat>-seltext_s      =
        <fs_fcat>-reptext_ddic   = 'Total Qty'.

    ENDCASE.
  ENDLOOP.
ENDFORM.                    " FIELDCAT_MODIFY
