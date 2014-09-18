*&----------------------------------------------------------------------
*& Development ID :
*& Program ID     : ZPPI04400T
*& Program Name   : Interface: Vehicle Production Result to Vaatz
*& Created by     : Victor Park
*& Created on     : 06.20.2011
*& Issue Doc No.  : FS_PP_154
*&
*& Modification Log
*& Date        Developer Issue No Description
*&======================================================================
*& RFC func. :
*& Stru.     : ZSPP04300
*&----------------------------------------------------------------------

REPORT zppi04400t MESSAGE-ID zmpp.

TABLES :  ztpp_vm, ztpp_rpid.

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
        INCLUDE STRUCTURE zspp04300.
DATA :  wo_serial LIKE ztpp_vm-wo_serial,
        wo_nation LIKE ztpp_vm-wo_nation,
        wo_dealer LIKE ztpp_vm-wo_dealer,
        model_code LIKE ztpp_vm-model_code,
        body_no    LIKE ztpp_vm-body_no,
        prod_date  LIKE sy-datum,
        ver        LIKE ztpp_vm-ver.
DATA : END OF it_data.

DATA : it_collect LIKE zspp04300 OCCURS 0 WITH HEADER LINE.
DATA : it_collect_tmp LIKE zspp04300 OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_wrkno OCCURS 0,
        work_ord_no(50),
        END OF it_wrkno.


DATA : it_ausp LIKE ausp OCCURS 0 WITH HEADER LINE.
DATA : wa_save TYPE ztpp_prd_vaatz_l.
DATA : it_save TYPE STANDARD TABLE OF ztpp_prd_vaatz_l WITH HEADER LINE.

*- RETURN MESSAGE
DATA : e_return TYPE zmms0053.
DATA : l_msgtxt(200).

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01.
SELECT-OPTIONS : s_serial  FOR ztpp_vm-wo_serial.
SELECT-OPTIONS : s_nation  FOR ztpp_vm-wo_nation.
SELECT-OPTIONS : s_dealer  FOR ztpp_vm-wo_dealer.
SELECT-OPTIONS : s_extc    FOR ztpp_vm-extc.
SELECT-OPTIONS : s_intc    FOR ztpp_vm-intc.
SELECT-OPTIONS : s_date    FOR ztpp_rpid-rp18_sdate OBLIGATORY.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-b02.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(10) text-t01.
PARAMETERS : p_send RADIOBUTTON GROUP r1 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 20(5) text-t02.
PARAMETERS : p_alv  RADIOBUTTON GROUP r1.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-b03.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(7) text-t03.
PARAMETERS : p_daily RADIOBUTTON GROUP r2 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 20(8) text-t04.
PARAMETERS : p_month  RADIOBUTTON GROUP r2.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b3.
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
  DATA : l_zvin(18) TYPE n.

  SELECT a~wo_serial  a~wo_nation a~wo_dealer a~extc a~intc
         a~dest_code  b~rp18_sdate AS prod_date a~model_code a~body_no
         c~fsc AS erp_fsc  a~ver
    INTO CORRESPONDING FIELDS OF TABLE it_data
  FROM ztpp_vm AS a INNER JOIN ztpp_rpid AS b
                ON a~model_code = b~model_code
               AND a~body_no    = b~body_no
                    INNER JOIN ztpp_wosum AS c
                ON a~wo_serial = c~wo_ser
               AND a~wo_nation = c~nation
               AND a~wo_dealer = c~dealer
               AND a~extc      = c~extc
               AND a~intc      = c~intc
  WHERE a~wo_serial   IN s_serial
    AND a~wo_nation   IN s_nation
    AND a~wo_dealer   IN s_dealer
    AND a~extc     IN s_extc
    AND a~intc     IN s_intc
    AND b~rp18_sdate  IN s_date
    AND a~wo_dealer   LIKE 'A%'
    AND a~usg_car     = 'P' .                      "Production

ENDFORM.                    " SELECT_DATA
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
FORM modify_data .

  DATA : lv_atinn_u_1 LIKE ausp-atinn.
  DATA : lv_atinn_u_2 LIKE ausp-atinn.
  DATA : lv_b(4).
  CLEAR : it_collect[], it_collect, it_wrkno[].

  LOOP AT it_data.
    CLEAR :  lv_b.
    it_data-company = 'HMMA'.
    it_data-werks   = 'HVA1'.
    it_data-seq_qty = 1.
    it_data-rp18_sdate  =  it_data-prod_date.
    CONCATENATE it_data-wo_serial it_data-wo_nation it_data-wo_dealer
                       INTO it_data-work_ord_no.

    lv_b  =  it_data-erp_fsc+14(4).
    it_data-erp_fsc+14(3)  =  '   '.
    it_data-erp_fsc+17(4)  =  lv_b.
    IF it_data-ver = '00'.
      it_data-erp_fsc+21(2)  =  '  '.
    ELSE.
      it_data-erp_fsc+21(2)  =  it_data-ver.
    ENDIF.
    MODIFY it_data.

    MOVE-CORRESPONDING it_data TO it_collect.
    COLLECT it_collect.
  ENDLOOP.

*Engine No, T/M No.
  LOOP AT it_collect.
    it_wrkno-work_ord_no  =  it_collect-work_ord_no.
    APPEND it_wrkno.
  ENDLOOP.


  CLEAR : lv_atinn_u_1, lv_atinn_u_2.
  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
    EXPORTING
      input  = 'P_ALC_U_1'
    IMPORTING
      output = lv_atinn_u_1.

  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
    EXPORTING
      input  = 'P_ALC_U_2'
    IMPORTING
      output = lv_atinn_u_2.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_ausp
  FROM ausp
    FOR ALL ENTRIES IN it_wrkno
    WHERE objek  =  it_wrkno-work_ord_no
      AND klart  =  '001'
        AND ( atinn  = lv_atinn_u_1 OR atinn  = lv_atinn_u_2 ).

  SORT it_ausp BY objek  atinn.


  LOOP AT it_collect.
    CLEAR : it_ausp.
    READ TABLE it_ausp WITH KEY objek  =  it_collect-work_ord_no
                                atinn  =  lv_atinn_u_1
                                BINARY SEARCH.
    IF sy-subrc = 0.
      it_collect-eng_no  =  it_ausp-atwrt.  "Eng. No
    ENDIF.

    CLEAR : it_ausp.
    READ TABLE it_ausp WITH KEY objek  =  it_collect-work_ord_no
                                atinn  =  lv_atinn_u_2
                                BINARY SEARCH.
    IF sy-subrc = 0.
      it_collect-tm_no  =  it_ausp-atwrt.    "T/M No
    ENDIF.

    MODIFY it_collect.
  ENDLOOP.

  IF p_month = 'X'.
    LOOP AT it_collect.
      MOVE-CORRESPONDING it_collect TO it_collect_tmp.
      it_collect_tmp-rp18_sdate  =  it_collect_tmp-rp18_sdate+0(6).
      COLLECT it_collect_tmp.
    ENDLOOP.

    CLEAR : it_collect[].
    it_collect[]  =  it_collect_tmp[].
  ENDIF.
ENDFORM.                    " MODIFY_DATA
*&---------------------------------------------------------------------*
*&      Form  PRO_BATCH
*&---------------------------------------------------------------------*
FORM pro_batch .
  DATA : v_dest(30) VALUE 'WMHR01'.   "Interface Destination.
  DATA: lt_sdata LIKE TABLE OF it_collect WITH HEADER LINE.
  DATA : l_model(3),
         l_model_year LIKE ztpp_vm-model_year.

  CLEAR : e_return.

  CHECK NOT it_collect[] IS INITIAL.

*  SELECT SINGLE dest INTO (v_dest)
*          FROM zdest
*          WHERE sy_sysid = sy-sysid
*            AND sy_mandt = sy-mandt.
*
*  IF v_dest IS INITIAL.
*    MESSAGE e000 WITH 'EAI setting is incorrect.'
*                      'Please check ZDEST table'.
*  ENDIF.

** Furong on 09/07/12 for convert color code from 2 to 3
  lt_sdata[] = it_collect[].
  LOOP AT lt_sdata.
    l_model  = lt_sdata-erp_fsc+5(2).
    l_model_year  =  lt_sdata-erp_fsc+0(1).
    CALL FUNCTION 'Z_FPP_CONVERT_COLOR'
      EXPORTING
        i_model = l_model
        i_year  = l_model_year
        i_gubn  = ''            "HMMA -> HAC
        i_extc  = lt_sdata-extc
        i_intc  = lt_sdata-intc
      IMPORTING
        e_extc  = lt_sdata-extc
        e_intc  = lt_sdata-intc.
    MODIFY lt_sdata.
  ENDLOOP.
  CALL FUNCTION 'Z_PP_IF_OB_PRDRSLT_VAATZ' DESTINATION v_dest
    IMPORTING
      e_return              = e_return
    TABLES
      t_data                = lt_sdata
    EXCEPTIONS
      communication_failure = 1  MESSAGE l_msgtxt
      system_failure        = 2  MESSAGE l_msgtxt.
*  CALL FUNCTION 'Z_PP_IF_OB_PRDRSLT_VAATZ' DESTINATION v_dest
*    IMPORTING
*      e_return              = e_return
*    TABLES
*      t_data                = it_collect
*    EXCEPTIONS
*      communication_failure = 1  MESSAGE l_msgtxt
*      system_failure        = 2  MESSAGE l_msgtxt.

** end on 09/07/12
  IF e_return-type = 'S' AND  sy-subrc = 0.   "Success
    PERFORM save_log  USING 'S' 'Success'    ''.
    MESSAGE s003 WITH 'Interface : Success'.
  ELSE.
    PERFORM save_log  USING 'E' e_return-message l_msgtxt.
    MESSAGE e003 WITH  e_return-message l_msgtxt.
  ENDIF.

ENDFORM.                    " PRO_BATCH

*&---------------------------------------------------------------------*
*&      Form  SAVE_LOG
*&---------------------------------------------------------------------*
FORM save_log  USING    p_type p_msg1 p_msg2.
  DATA : l_zseq(10) TYPE n.

  CLEAR : it_save[], it_save.

  SELECT zseq INTO l_zseq
    FROM ztpp_prd_vaatz_l
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
      IF NOT p_msg1 IS INITIAL.
        it_save-zmsg  = p_msg1.
      ELSE.
        it_save-zmsg  = p_msg2.
      ENDIF.
    ENDIF.

    APPEND it_save.
    CLEAR : it_save.

    WRITE : / it_collect-rp18_sdate, it_collect-work_ord_no,
              it_collect-extc, it_collect-intc, it_collect-seq_qty.
  ENDLOOP.

  INSERT ztpp_prd_vaatz_l FROM TABLE it_save
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
                             USING   'IT_DATA'.

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
  p_layout-box_fieldname  =    'CHK'.  "SELECTION FIELD
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
  gs_sort-tabname   = 'IT_DATA'.
  gs_sort-fieldname = 'RP18_SDATE'.
  gs_sort-up        = 'X'.
  gs_sort-group     = 'BL'.
  gs_sort-subtot    = ''.
  APPEND gs_sort TO p_sort.

  gs_sort-spos      = '2'.
  gs_sort-tabname   = 'IT_DATA'.
  gs_sort-fieldname = 'WORK_ORD_NO'.
  gs_sort-up        = 'X'.
  gs_sort-group     = 'BL'.
  gs_sort-subtot    = ''.
  APPEND gs_sort TO p_sort.

  gs_sort-spos      = '3'.
  gs_sort-tabname   = 'IT_DATA'.
  gs_sort-fieldname = 'EXTC'.
  gs_sort-up        = 'X'.
  gs_sort-group     = 'BL'.
  gs_sort-subtot    = ''.
  APPEND gs_sort TO p_sort.

  gs_sort-spos      = '4'.
  gs_sort-tabname   = 'IT_DATA'.
  gs_sort-fieldname = 'INTC'.
  gs_sort-up        = 'X'.
  gs_sort-group     = 'BL'.
  gs_sort-subtot    = 'X'.    "Sub Total
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
      WHEN 'COMPANY'.
        pt_fieldcat-seltext_m    = 'Company'.
        pt_fieldcat-no_out       = 'X'.
      WHEN 'WORK_ORD_NO'.
        pt_fieldcat-seltext_m    = 'Work Order No.'.
      WHEN 'RP18_SDATE'.
*        pt_fieldcat-edit_mask    = ''.
        pt_fieldcat-seltext_m    = 'Prod.Date'.
      WHEN 'ERP_FSC'.
        pt_fieldcat-seltext_m    = 'FSC Code.'.
      WHEN 'SEQ_QTY'.
        pt_fieldcat-seltext_m    = 'Quantity(Vehicle)'.
        pt_fieldcat-do_sum       = 'X'.   "Sub Total
      WHEN 'ENG_NO'.
        pt_fieldcat-seltext_m    = 'Engine Code'.
        pt_fieldcat-no_out       = 'X'.
      WHEN 'TM_NO'.
        pt_fieldcat-seltext_m    = 'T/M Code'.
        pt_fieldcat-no_out       = 'X'.
      WHEN 'SEAT_CODE'.
        pt_fieldcat-seltext_m    = 'Seat Code'.
        pt_fieldcat-no_out       = 'X'.
      WHEN 'PROD_DATE'.
        pt_fieldcat-no_out       = 'X'.
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
  CONCATENATE '*Shop Date : ' s_date-low ' ~ '  s_date-low
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
