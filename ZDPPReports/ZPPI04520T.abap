*&----------------------------------------------------------------------
*& Development ID :
*& Program ID     : ZPPI04520T
*& Program Name   : AFFW Backlog Interface
*& Created by     : Victor Park
*& Created on     : 06.20.2011
*& Issue Doc No.  :
*&
*& Modification Log
*& Date        Developer Issue No Description
*& 01.05.2012  Victor    use Posting Date
*&======================================================================
*& RFC func. :
*& Stru.     : ZSPPAFFW_LOG, ZSPPAFFW_CNT,  ZTPPAFFW_LOG
*&----------------------------------------------------------------------

REPORT zppi04520t MESSAGE-ID zmpp.

TABLES : affw, makt, mara, marc.

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
        INCLUDE STRUCTURE zsppaffw_log.
DATA :  msgv1   LIKE affw-msgv1,
        msgv2   LIKE affw-msgv2,
        msgv3   LIKE affw-msgv3,
        msgv4   LIKE affw-msgv4,
        err_cnt TYPE z_count.
DATA : END OF it_data.

DATA : it_data_tmp LIKE it_data OCCURS 0 WITH HEADER LINE.


DATA : wa_save TYPE ztppaffw_log.
DATA : it_save TYPE STANDARD TABLE OF ztppaffw_log WITH HEADER LINE.

DATA : it_send  LIKE zsppaffw_log OCCURS 0 WITH HEADER LINE.
DATA : it_interface  LIKE zsppaffw_log OCCURS 0 WITH HEADER LINE.
DATA : it_cnt   LIKE zsppaffw_cnt OCCURS 0 WITH HEADER LINE.

*- RETURN MESSAGE
DATA : e_return TYPE zmms0053.
DATA : l_msgtxt(200).

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.
SELECT-OPTIONS :  so_matnr FOR mara-matnr,
                so_werks FOR affw-werks, " OBLIGATORY,
                so_lgort FOR affw-lgort,
                so_mtart FOR mara-mtart,
                so_matkl FOR mara-matkl ,
                so_dispo FOR marc-dispo,
                so_msgid FOR affw-msgid,
                s_ersda  FOR affw-ersda.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-b02.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS : p_send    RADIOBUTTON GROUP r2 USER-COMMAND ra.
SELECTION-SCREEN COMMENT (25) text-t21 FOR FIELD p_send.
PARAMETERS : p_alv     RADIOBUTTON GROUP r2 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 30(20) text-t22 FOR FIELD p_alv.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b2.
*----------------------------------------------------------------------*
* INITIALIZATION.
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM initial.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN.
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

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

  IF p_send = 'X'.
    PERFORM  pro_batch.
  ELSE.
    perform  pro_alv.
  ENDIF.

*----------------------------------------------------------------------*
* END-OF-SELECTION.
*----------------------------------------------------------------------*
END-OF-SELECTION.


*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
FORM select_data .
  DATA : l_zvin(18) TYPE n.

  SELECT a~weblnr a~weblpos a~matnr b~maktx a~werks a~lgort a~charg
         a~bwart  a~sobkz   a~erfmg a~erfme a~aufnr a~autyp a~msgid
         a~msgno  a~msgty   a~ersda a~erzet a~ersda a~fwdat a~fwzet
         a~fevor  a~dispo  a~msgv1   a~msgv2 a~msgv3 a~msgv4
    INTO CORRESPONDING FIELDS OF TABLE it_data
  FROM affw AS a INNER JOIN makt AS b
                ON a~matnr = b~matnr
                 INNER JOIN mara AS c
                ON a~matnr  = c~matnr
  WHERE a~budat   IN s_ersda  "Posting Date
    AND a~bwart   = '261'
*    AND a~lgort   <> 'L001'
    AND a~matnr   IN so_matnr
    AND a~werks   IN so_werks
    AND a~lgort   IN so_lgort
    AND a~dispo   IN so_dispo
    AND a~msgid   IN so_msgid
    AND c~mtart   IN so_mtart
    AND c~matkl   IN so_matkl
    AND a~msgty   = 'E'
 "exclude System Lock: 10.31.2011 by Victor
    AND a~msgid   <> 'M3'    "exclude System Lock: 10.31.2011 by Victor
    AND ( a~msgid   = 'M7'   AND a~msgno  <> '112' )
    AND b~spras   =  sy-langu .

ENDFORM.                    " SELECT_DATA
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
FORM modify_data .
  DATA : lv_zzcdate TYPE ztimestamp.

  CLEAR : it_cnt[], it_cnt, it_send[], it_send,
          it_interface[], it_interface.

  CONCATENATE sy-datum sy-uzeit INTO lv_zzcdate.

*-FWDAT + MATNR
  it_data_tmp[]  = it_data[].
  SORT it_data_tmp BY ersda matnr.
  DELETE ADJACENT DUPLICATES FROM it_data_tmp
                    COMPARING ersda matnr.
  LOOP AT it_data.

    it_send-ersda = it_data-ersda.
    it_send-matnr = it_data-matnr.
    it_send-erfmg = it_data-erfmg.

    COLLECT it_send.

  ENDLOOP.

*--final table : it_interface[]
  SORT it_send BY ersda matnr.
  LOOP AT it_data_tmp.
    it_data_tmp-err_cnt = 1.
    it_data_tmp-zzcdate =  lv_zzcdate.

* by Daniel on 07/19/11 {
*    IF it_data_tmp-werks = 'P001'.
*      it_data_tmp-werks   = 'HVA1'.
*    ELSE.
*      it_data_tmp-werks   = 'HEA1'. "Engine E001 -> HEA1
*    ENDIF.
    it_data_tmp-werks   = 'HVA1'.
* }

    MOVE-CORRESPONDING it_data_tmp TO it_interface.
    MOVE-CORRESPONDING it_data_tmp TO it_cnt.

    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
      EXPORTING
        msgid               = it_data_tmp-msgid
        msgnr               = it_data_tmp-msgno
        msgv1               = it_data_tmp-msgv1
        msgv2               = it_data_tmp-msgv2
        msgv3               = it_data_tmp-msgv3
        msgv4               = it_data_tmp-msgv4
      IMPORTING
        message_text_output = it_interface-msgtext.

    CLEAR : it_send.
    READ TABLE it_send WITH KEY ersda = it_data_tmp-ersda
                                matnr = it_data_tmp-matnr
                                BINARY SEARCH.
    IF sy-subrc = 0.
      it_interface-erfmg  = it_send-erfmg.
      APPEND it_interface.
      COLLECT it_cnt.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " MODIFY_DATA
*&---------------------------------------------------------------------*
*&      Form  PRO_BATCH
*&---------------------------------------------------------------------*
FORM pro_batch .
  DATA : v_dest(30) VALUE 'WMPP01'.   "Interface Destination.

  CLEAR : e_return.

*  CHECK NOT it_interface[] IS INITIAL.

*if case of no data, send only summary line.
  IF  it_interface[] IS INITIAL.
    it_cnt-werks  =	'HVA1'.
    IF s_ersda-high = '00000000'.
      it_cnt-fwdat  = s_ersda-low.
    ELSE.
      it_cnt-fwdat  = s_ersda-high.
    ENDIF.
    it_cnt-err_cnt  = 0.
    CONCATENATE sy-datum sy-uzeit INTO it_cnt-zzcdate.
    APPEND it_cnt.
  ENDIF.

  CALL FUNCTION 'Z_PP_IF_OB_BACKLOG' DESTINATION v_dest
    IMPORTING
      e_return              = e_return
    TABLES
      t_data                = it_interface
      t_cnt                 = it_cnt
    EXCEPTIONS
      communication_failure = 1  MESSAGE l_msgtxt
      system_failure        = 2  MESSAGE l_msgtxt.

  IF e_return-type = 'S' AND  sy-subrc = 0.   "Success
    PERFORM save_log  USING 'S' 'Success'    ''.
    WRITE : 'Interface : Success'.
    MESSAGE s003 WITH 'Interface : Success'.
  ELSE.
    PERFORM save_log  USING 'E' e_return-message l_msgtxt.
    WRITE :/ e_return-message, l_msgtxt.      "For Spool
    MESSAGE e003 WITH  e_return-message l_msgtxt.
  ENDIF.

ENDFORM.                    " PRO_BATCH

*&---------------------------------------------------------------------*
*&      Form  SAVE_LOG
*&---------------------------------------------------------------------*
FORM save_log  USING    p_type p_msg1 p_msg2.
  DATA : l_zseq(10) TYPE n.

  CLEAR : it_save[], it_save.

  CHECK NOT it_interface[] IS INITIAL.                      "07.06.2011

  SELECT zseq INTO l_zseq
    FROM ztppaffw_log
    UP TO 1 ROWS
    WHERE zdate = sy-datum
    ORDER BY zseq DESCENDING.
  ENDSELECT.

  LOOP AT it_interface.
    MOVE-CORRESPONDING it_interface TO it_save.

    it_save-zdate = sy-datum.
    it_save-zseq  = l_zseq + sy-tabix.
    it_save-ztime = sy-uzeit.
    it_save-ernam = sy-uname.
    it_save-zrslt = p_type.
    IF p_type = 'E'.
      IF NOT p_msg1 IS  INITIAL.
        it_save-zmsg  = p_msg1.
      ELSE.
        it_save-zmsg  = p_msg2.
      ENDIF.
    ENDIF.

    APPEND it_save.
    CLEAR : it_save.

  ENDLOOP.

  INSERT ztppaffw_log FROM TABLE it_save
                             ACCEPTING DUPLICATE KEYS .
  COMMIT WORK AND WAIT.


ENDFORM.                    " SAVE_LOG
*&---------------------------------------------------------------------*
*&      Form  INITIAL
*&---------------------------------------------------------------------*
FORM initial .

  g_repid  = sy-repid.

  s_ersda-sign   = 'I'.
  s_ersda-option = 'EQ'.
  s_ersda-low = sy-datum - 1.
  APPEND s_ersda.

ENDFORM.                    " INITIAL
*&---------------------------------------------------------------------*
*&      Form  PRO_ALV
*&---------------------------------------------------------------------*
form PRO_ALV .
 FIELD-SYMBOLS : <itab>  TYPE STANDARD TABLE.
  DATA : lv_name(30),
         lv_stru(30).

    lv_name  = 'IT_INTERFACE[]'.
    lv_stru  = 'IT_INTERFACE'.

  ASSIGN : (lv_name) TO <itab>.

  PERFORM layout_build       USING   gs_layout.
  PERFORM sorttab_build      USING   gt_sort.
  PERFORM fieldcat           TABLES  gt_fieldcat
                             USING   lv_stru.

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
*     is_variant               = g_variant
      it_events                = gt_events[]
    TABLES
      t_outtab                 = <itab>
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
*  p_layout-box_fieldname  =    'CHK'.  "SELECTION FIELD
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


    gs_sort-spos      = '1'.
    gs_sort-tabname   = 'IT_INTERFACE'.
    gs_sort-fieldname = 'MATNR'.
    gs_sort-up        = 'X'.
    gs_sort-group     = 'BL'.
    gs_sort-subtot    = ''.
    APPEND gs_sort TO p_sort.

    gs_sort-spos      = '2'.
    gs_sort-tabname   = 'IT_INFERFACE'.
    gs_sort-fieldname = 'ERSDA'.
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
*     i_structure_name   = p_name
      i_internal_tabname = p_name
      i_inclname         = g_repid
    CHANGING
      ct_fieldcat        = pt_fieldcat[].

  LOOP AT pt_fieldcat.
    CASE pt_fieldcat-fieldname.
    ENDCASE.
    pt_fieldcat-reptext_ddic =
    pt_fieldcat-seltext_s    =
    pt_fieldcat-seltext_l    =
    pt_fieldcat-seltext_m.

    MODIFY pt_fieldcat.

  ENDLOOP.

ENDFORM.                    " FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  LIST_HEADER_WRITE
*&---------------------------------------------------------------------*
FORM list_header_write   USING alv_t_listheader TYPE slis_t_listheader.
  DATA : lv_cnt(7).

  CLEAR   : ls_title, alv_t_listheader.
  REFRESH : alv_t_listheader.

  DATA : h_title(30), s_title(60),  a_title(60).

    DESCRIBE TABLE it_interface LINES lv_cnt.

    ls_title-typ = 'H'. "(H:Header, S:Selection, A:Action)
    CONCATENATE '*Display records : ' lv_cnt   INTO ls_title-info.
    APPEND ls_title TO alv_t_listheader.

    ls_title-typ = 'S'. "(H:Header, S:Selection, A:Action)
    CONCATENATE '*Creation Date : '  s_ersda-low '~' s_ersda-high
                                     INTO ls_title-info.
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

*&--------------------------------------------------------------------*
*&      Form  ALV_USER_COMMAND
*&--------------------------------------------------------------------*
FORM  user_command USING ucomm    LIKE sy-ucomm
                    p_selfield    TYPE slis_selfield.
* double click : UCOMM = &IC1
  CASE ucomm.

  ENDCASE.

  p_selfield-refresh = 'X'.
ENDFORM.                    "user_command
