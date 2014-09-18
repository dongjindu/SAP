*&----------------------------------------------------------------------
*& Development ID :
*& Program ID     : ZSDR04400T
*& Program Name   : Sales Export Price Interface with VAATZ
*& Created by     : Victor Park
*& Created on     : 08.11.2011
*& Issue Doc No.  :
*&
*& Modification Log
*& Date        Developer Issue No Description
*&======================================================================
*& RFC func. :  Z_SD_VAATZ_PRICE
*& Stru.     :  ZSSD_VAATZ01,  ZTSD_VAATZ01
*&----------------------------------------------------------------------

REPORT zsdr04400t MESSAGE-ID zmpp.

TABLES : ztpp_vm, ztpp_rpid ,
          vbrk,      "Billing Header Data
          vbrp,       "Billing Item Data
          vbak,       "Sales  Header Data
          vbap.       "Sales  Item Data

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
DATA : chk(1).
        INCLUDE STRUCTURE zssd_vaatz01.
DATA :  wo_serial LIKE ztpp_vm-wo_serial,
        wo_nation LIKE ztpp_vm-wo_nation,
        wo_dealer LIKE ztpp_vm-wo_dealer,
        model_code LIKE ztpp_vm-model_code,
        body_no    LIKE ztpp_vm-body_no,
        matnr      LIKE vbrp-matnr,
        vgbel      LIKE vbrp-vgbel.
DATA : END OF it_data.

DATA : it_collect LIKE zssd_vaatz01 OCCURS 0 WITH HEADER LINE.


DATA : wa_save TYPE ztsd_vaatz01.
DATA : it_save TYPE STANDARD TABLE OF ztsd_vaatz01 WITH HEADER LINE.

DATA : it_vm   LIKE ztpp_vm OCCURS 0 WITH HEADER LINE.

*- RETURN MESSAGE
DATA : e_return TYPE zmms0053.
DATA : l_msgtxt(200).

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.
SELECT-OPTIONS : s_date    FOR vbrk-erdat DEFAULT sy-datum
                                TO sy-datum OPTION BT SIGN I.
SELECT-OPTIONS : s_dest    FOR ztpp_vm-dest_code.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-b02.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(10) text-t01.
PARAMETERS : p_send RADIOBUTTON GROUP r1 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 20(5) text-t02.
PARAMETERS : p_alv  RADIOBUTTON GROUP r1.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b2.
*----------------------------------------------------------------------*
* INITIALIZATION.
*----------------------------------------------------------------------*
INITIALIZATION.
  g_repid      = sy-repid.

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

*----------------------------------------------------------------------*
* END-OF-SELECTION.
*----------------------------------------------------------------------*
END-OF-SELECTION.
  IF p_send = 'X'.
    PERFORM  pro_batch.
  ELSE.
    PERFORM pro_alv.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
FORM select_data .
  "vbrp-VGBEL : Model + body

*-Convert Int_price  & e_price
  SELECT  a~erdat a~kunag AS dest_code c~matnr AS erp_fsc    "c~stawn
*          b~netwr AS e_price b~wavwr AS int_price     a~kurrf a~waerk
          b~netwr AS int_price b~wavwr AS e_price     a~kurrf a~waerk
          b~vgbel b~matnr
    INTO  CORRESPONDING FIELDS OF TABLE it_data
  FROM vbrk AS a INNER JOIN vbrp  AS b
                ON a~vbeln  = b~vbeln
                 INNER JOIN marc AS c
                ON b~matnr  = c~matnr
  WHERE a~erdat IN s_date
    AND a~kunag IN s_dest
    AND a~fkart  = 'ZVF2'.


  LOOP AT it_data.
    it_data-model_code  =  it_data-vgbel+0(3).
    it_data-body_no     =  it_data-vgbel+3(6).
    MODIFY it_data.
  ENDLOOP.

  CHECK NOT  it_data[] IS INITIAL.

  SELECT *
  INTO CORRESPONDING FIELDS OF TABLE it_vm
  FROM ztpp_vm  AS  a
    FOR ALL ENTRIES IN it_data
  WHERE a~model_code = it_data-model_code
    AND a~body_no    = it_data-body_no.

ENDFORM.                    " SELECT_DATA
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
FORM modify_data .
  DATA : lv_zzcdate TYPE  ztimestamp.
  DATA : lv_b(4).

  CLEAR : it_collect[], it_collect.

  SORT it_vm BY model_code body_no.

  CONCATENATE sy-datum sy-uzeit INTO lv_zzcdate.

  LOOP AT it_data.
    it_data-bukrs      = 'HMMA'.
    it_data-goods_seg  = 'V'.
    it_data-sales_seg  = 'O'.
*    it_data-bonus      = it_data-erp_fsc+14(1). "No need in HMMA
    it_data-zzcdate    = lv_zzcdate.

    READ TABLE it_vm WITH KEY model_code  = it_data-model_code
                              body_no     = it_data-body_no
                              BINARY SEARCH.
    IF sy-subrc = 0.
      CONCATENATE it_vm-wo_serial it_vm-wo_nation it_vm-wo_dealer
                         INTO it_data-work_ord_no.

      lv_b  =  it_data-erp_fsc+14(4).
      it_data-erp_fsc+14(3)  =  '   '.
      it_data-erp_fsc+17(4)  =  lv_b.
      IF it_vm-ver = '00'.
        it_data-erp_fsc+21(2)  =  '  '.
      ELSE.
        it_data-erp_fsc+21(2)  =  it_vm-ver.
      ENDIF.
    ELSE.
      CONTINUE.
    ENDIF.

    MOVE-CORRESPONDING it_data TO it_collect.
*    COLLECT it_collect.
    APPEND it_collect.
  ENDLOOP.

  SORT it_collect BY erdat  bukrs goods_seg sales_seg work_ord_no
                     erp_fsc.

  DELETE ADJACENT DUPLICATES FROM it_collect COMPARING erdat  bukrs
                             goods_seg sales_seg work_ord_no  erp_fsc.

ENDFORM.                    " MODIFY_DATA
*&---------------------------------------------------------------------*
*&      Form  PRO_BATCH
*&---------------------------------------------------------------------*
FORM pro_batch .
  DATA : v_dest(30) VALUE 'WMHR01'.   "Interface Destination.

  CLEAR : e_return.

  CHECK NOT it_collect[] IS INITIAL.

  CALL FUNCTION 'Z_SD_VAATZ_PRICE' DESTINATION v_dest
    IMPORTING
      e_return              = e_return
    TABLES
      t_data                = it_collect
    EXCEPTIONS
      communication_failure = 1  MESSAGE l_msgtxt
      system_failure        = 2  MESSAGE l_msgtxt.

  IF e_return-type = 'S' AND  sy-subrc = 0.   "Success
    PERFORM save_log  USING 'S' 'Success'    ''.
    MESSAGE s000 WITH 'Interface : Success'.
  ELSE.
    PERFORM save_log  USING 'E' e_return-message l_msgtxt.
    MESSAGE e000 WITH  e_return-message l_msgtxt.
  ENDIF.

ENDFORM.                    " PRO_BATCH

*&---------------------------------------------------------------------*
*&      Form  SAVE_LOG
*&---------------------------------------------------------------------*
FORM save_log  USING    p_type p_msg1 p_msg2.
  DATA : l_zseq(10) TYPE n.

  CLEAR : it_save[], it_save.

  SELECT zseq INTO l_zseq
    FROM ztsd_vaatz01
    UP TO 1 ROWS
    WHERE zdate = sy-datum
    ORDER BY zseq DESCENDING.
  ENDSELECT.

  LOOP AT it_collect.
    MOVE-CORRESPONDING it_collect TO it_save.

    it_save-zdate = sy-datum.
    it_save-zseq  = l_zseq + sy-tabix.
    it_save-ztime = sy-uzeit.
    it_save-ernam = sy-uname.
    it_save-zrslt = p_type.
    IF p_type = 'E'.
      IF  NOT p_msg1 IS INITIAL.
        it_save-zmsg  = p_msg1.
      ELSE.
        it_save-zmsg  = p_msg2.
      ENDIF.
    ENDIF.

    APPEND it_save.
    CLEAR : it_save.

  ENDLOOP.

  INSERT ztsd_vaatz01 FROM TABLE it_save
                             ACCEPTING DUPLICATE KEYS .
  COMMIT WORK AND WAIT.


ENDFORM.                    " SAVE_LOG
*&---------------------------------------------------------------------*
*&      Form  PRO_ALV
*&---------------------------------------------------------------------*
FORM pro_alv .


  PERFORM layout_build       USING   gs_layout.
  PERFORM sorttab_build      USING   gt_sort.
  PERFORM fieldcat           TABLES  gt_fieldcat
                             USING   'IT_COLLECT'.

  PERFORM list_header_write USING alv_t_listheader[].
  PERFORM append_alv_event  CHANGING   gt_events.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = g_repid
*      i_callback_pf_status_set = 'PF_STATUS'
      i_callback_user_command = g_user_command
      is_layout          = gs_layout
      it_fieldcat        = gt_fieldcat
      it_sort            = gt_sort
      i_save             = g_save
*      is_variant         = g_variant
      it_events          = gt_events[]
    TABLES
      t_outtab           = it_collect[]
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

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

*  CLEAR: gs_sort, p_sort[].
*
*  gs_sort-spos      = '1'.
*  gs_sort-tabname   = 'IT_DATA'.
*  gs_sort-fieldname = 'RP18_SDATE'.
*  gs_sort-up        = 'X'.
*  gs_sort-group     = 'BL'.
*  gs_sort-subtot    = ''.
*  APPEND gs_sort TO p_sort.
*
*  gs_sort-spos      = '2'.
*  gs_sort-tabname   = 'IT_DATA'.
*  gs_sort-fieldname = 'WORK_ORD_NO'.
*  gs_sort-up        = 'X'.
*  gs_sort-group     = 'BL'.
*  gs_sort-subtot    = ''.
*  APPEND gs_sort TO p_sort.
*
*  gs_sort-spos      = '3'.
*  gs_sort-tabname   = 'IT_DATA'.
*  gs_sort-fieldname = 'EXTC'.
*  gs_sort-up        = 'X'.
*  gs_sort-group     = 'BL'.
*  gs_sort-subtot    = ''.
*  APPEND gs_sort TO p_sort.
*
*  gs_sort-spos      = '4'.
*  gs_sort-tabname   = 'IT_DATA'.
*  gs_sort-fieldname = 'INTC'.
*  gs_sort-up        = 'X'.
*  gs_sort-group     = 'BL'.
*  gs_sort-subtot    = 'X'.    "Sub Total
*  APPEND gs_sort TO p_sort.

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
*      i_structure_name      = p_name
      i_internal_tabname = p_name
      i_inclname         = g_repid
    CHANGING
      ct_fieldcat        = pt_fieldcat[].

  LOOP AT pt_fieldcat.
    CASE pt_fieldcat-fieldname.
      WHEN 'BUKRS'.
        pt_fieldcat-seltext_m    = 'Company'.
      WHEN 'WORK_ORD_NO'.
        pt_fieldcat-seltext_m    = 'Work Order No.'.
      WHEN 'GOODS_SEG'.
        pt_fieldcat-seltext_m    = 'Goods Segment(CKD/FSC)'.
      WHEN 'SALES_SEG'.
        pt_fieldcat-seltext_m    = 'Sale Price segment'.
      WHEN 'E_PRICE'.
        pt_fieldcat-seltext_m    = 'Export Price(FOB)'.
*        pt_fieldcat-do_sum       = 'X'.   "Sub Total
      WHEN 'KZWI6'.
        pt_fieldcat-seltext_m    = 'Price(Internal Dicision)'.
      WHEN 'SALES_FEE'.
        pt_fieldcat-seltext_m    = 'Sales support Fee'.
      WHEN 'E_PRICE1'.
        pt_fieldcat-seltext_m    = 'Export Price(FOB)'.
      WHEN 'KURRF'.
        pt_fieldcat-seltext_m    = 'Exchange rate'.
      WHEN 'STAWN'.
        pt_fieldcat-seltext_m    = 'HSCODE'.
      WHEN 'BONUS'.
        pt_fieldcat-seltext_m    = 'Grade'.
      WHEN 'ZZCDATE'.
        pt_fieldcat-seltext_m    = 'Send Date'.
      WHEN OTHERS.

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
FORM list_header_write  USING alv_t_listheader TYPE slis_t_listheader.

  CLEAR   : ls_title, alv_t_listheader.
  REFRESH : alv_t_listheader.

  DATA : h_title(30), s_title(60),  a_title(60).

  ls_title-typ = 'S'. "(H:Header, S:Selection, A:Action)
  CONCATENATE '*Shop Date : ' s_date-low ' ~ '  s_date-high
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
