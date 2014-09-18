*&---------------------------------------------------------------------*
*& Report  ZMMR_IF001                                                  *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  zmmr_if001   MESSAGE-ID zmm_if
                     NO STANDARD PAGE HEADING.

TABLES : ztmm_if005,
         ztmm_if006,
         zsmm_if001,
         eban,
         ebkn.

*-------------------------------------------------------------*
*  Globale Type(ALV)
*-------------------------------------------------------------*
TYPE-POOLS: slis.

************************************************************************
* VARIANTS                                                             *
************************************************************************
*---// Select data checking field
DATA count   TYPE i.

*-------------------------------------------------------------*
*  RANGES
*-------------------------------------------------------------*
RANGES : r_type FOR ztmm_if005-type,
         r_flag FOR ztmm_if005-flag.

DATA: BEGIN OF it_tab OCCURS 0,
        icon(4).
DATA: chk.
        INCLUDE STRUCTURE ztmm_if005.
DATA : END OF it_tab.

DATA it_head LIKE TABLE OF ztmm_if005 WITH HEADER LINE.
DATA it_item LIKE TABLE OF ztmm_if006 WITH HEADER LINE.

DATA : it_rep LIKE it_tab OCCURS 0 WITH HEADER LINE.
DATA : w_rep_index TYPE i,
       w_rep_lines TYPE i.


*-------------------------------------------------------------*
*  ALV : Function
*-------------------------------------------------------------*
* General Work fields
DATA : g_exit_caused_by_caller  TYPE c,
       g_repid                  TYPE sy-repid,
       g_save                   TYPE c,
       g_program_name           LIKE sy-repid,
       g_inclname               LIKE trdir-name.

* Structures
DATA : g_layout_s               TYPE slis_layout_alv,
       gs_fieldcat              TYPE slis_fieldcat_alv,
       g_exit_caused_by_user_s  TYPE slis_exit_by_user,
       g_variant_s              TYPE disvariant.

* Internal tables
DATA : g_events_t               TYPE slis_t_event,
       g_list_top_of_page_t     TYPE slis_t_listheader,
       gt_fieldcat              TYPE slis_t_fieldcat_alv,
       gt_sort                  TYPE slis_t_sortinfo_alv.

DATA : alv_print                TYPE slis_print_alv.
DATA : alv_repid        LIKE sy-repid,
       alv_variant      LIKE disvariant.
DATA: alv_detail_func(30).

*--------------------------------------------------------------*
*  CONSTANTS:
*--------------------------------------------------------------*
CONSTANTS : c_status_set   TYPE slis_formname VALUE 'PF_STATUS_SET',
            c_user_command TYPE slis_formname VALUE 'USER_COMMAND',
            c_top_of_page  TYPE slis_formname VALUE 'TOP_OF_PAGE'.


*--------------------------------------------------------------*
*  CONTROL
*--------------------------------------------------------------*
DATA : g_custom_container                    ">Display Screen
       TYPE REF TO cl_gui_custom_container,
       alv_grid
       TYPE REF TO cl_gui_alv_grid.

DATA: gs_layout TYPE lvc_s_layo,
      gs_fdcat  TYPE lvc_s_fcat,
      gt_fdcat  TYPE lvc_t_fcat.

DATA it_out LIKE TABLE OF ztmm_if006 WITH HEADER LINE.

DATA : ok_code LIKE sy-ucomm.


************************************************************************
* SELECT-OPTIONS / PARAMETERS                                          *
************************************************************************
*Search condition
SELECTION-SCREEN BEGIN OF BLOCK box1 WITH FRAME TITLE text-001.
SELECT-OPTIONS:
  s_banfn  FOR ztmm_if005-banfn       MODIF ID iog,
  "P/R Number.
  s_serno  FOR ztmm_if005-serno       NO-EXTENSION NO INTERVALS,
  s_matnr  FOR ztmm_if005-matnr,
  s_date   FOR sy-datum.
"Date on which the record was created.
*  S_TIME   FOR SY-UZEIT               NO-EXTENSION.      "Entry time.
SELECTION-SCREEN END OF BLOCK box1.

*SELECTION-SCREEN ULINE.

*Division
SELECTION-SCREEN BEGIN OF BLOCK box2 WITH FRAME TITLE text-002.
PARAMETERS: p_create    RADIOBUTTON GROUP ab1,
            p_change    RADIOBUTTON GROUP ab1,
            p_outbnd    RADIOBUTTON GROUP ab1,
            p_indel     RADIOBUTTON GROUP ab1,
            p_all       RADIOBUTTON GROUP ab1 DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK box2.

*SELECTION-SCREEN ULINE.

*Result
SELECTION-SCREEN BEGIN OF BLOCK box3 WITH FRAME TITLE text-003.
PARAMETERS: p_sucess    RADIOBUTTON GROUP ab2,
            p_error     RADIOBUTTON GROUP ab2,
            p_rep       RADIOBUTTON GROUP ab2,     " re-processing
            p_schall    RADIOBUTTON GROUP ab2 DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK box3.

************************************************************************
* INITIALIZATION Event                                                 *
************************************************************************
INITIALIZATION.
* g_repid = sy-repid.
  PERFORM layout_init USING g_layout_s.
  PERFORM eventtab_build USING g_events_t[].
  g_repid                = sy-repid.


************************************************************************
* START-OF-SELECTION Event                                             *
************************************************************************
START-OF-SELECTION.
  PERFORM range_conversion.

  PERFORM get_log_data.

  IF sy-subrc NE 0.
    MESSAGE s001.
    EXIT.
  ENDIF.

END-OF-SELECTION.
  PERFORM build_fieldcat.
  PERFORM build_sortcat.
  PERFORM alv_display.






*&---------------------------------------------------------------------*
*&      Form  RANGE_CONVERSION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM range_conversion .
*--FLAG & TYPE RANGE
  CLEAR : r_flag, r_type.
  REFRESH : r_flag, r_type.

  IF     p_create  = 'X'.
    r_flag-sign    = 'I'.
    r_flag-option  = 'EQ'.
    r_flag-low     = '1'.
    APPEND r_flag. CLEAR r_flag.

  ELSEIF p_change = 'X'.
    r_flag-sign   = 'I'.
    r_flag-option = 'EQ'.
    r_flag-low    = '2'.
    APPEND r_flag. CLEAR r_flag.

  ELSEIF p_indel  = 'X'.
    r_flag-sign   = 'I'.
    r_flag-option = 'EQ'.
    r_flag-low    = '4'.
    APPEND r_flag. CLEAR r_flag.

  ELSEIF p_outbnd = 'X'.
    r_flag-sign   = 'I'.
    r_flag-option = 'EQ'.
    r_flag-low    = '3'.
    APPEND r_flag. CLEAR r_flag.
  ENDIF.

*  IF    p_sucess  = 'X'.
*    r_type-sign   = 'I'.
*    r_type-option = 'EQ'.
*    r_type-low    = 'S'.
*    APPEND r_type. CLEAR r_type.
*
*  ELSEIF p_error  = 'X'.
*    r_type-sign   = 'I'.
*    r_type-option = 'EQ'.
*    r_type-low    = 'E'.
*    APPEND r_type. CLEAR r_type.
*  ENDIF.
ENDFORM.                    " RANGE_CONVERSION
*&---------------------------------------------------------------------*
*&      Form  alv_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_display .
*> ALV reuse function call..
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program      = g_repid
*            i_background_id         = 'ALV_BACKGROUND'
            i_callback_user_command = 'USER_COMMAND'
            it_events               = g_events_t[]
            it_fieldcat             = gt_fieldcat[]
            it_sort                 = gt_sort[]
            i_save                  = 'X'
            is_layout               = g_layout_s
       TABLES
            t_outtab                = it_tab[].
ENDFORM.                    " alv_display
*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcat .
  PERFORM fill_field_category USING :
            'S' 'FIELDNAME'        'ICON',
            'E' 'REPTEXT_DDIC'     'Status',

            'S' 'FIELDNAME'        'SERNO',
            'E' 'REPTEXT_DDIC'     'Serial No',

            'S' 'FIELDNAME'        'FLAG',
            'E' 'REPTEXT_DDIC'     'Flag No',

            'S' 'FIELDNAME'        'TYPE',
            'E' 'REPTEXT_DDIC'     'Type',

*            'S' 'FIELDNAME'        'MESSAGE',
*            'E' 'REPTEXT_DDIC'     'MESSAGE',

            'S' 'FIELDNAME'        'TRAN_DATE',
            'E' 'REPTEXT_DDIC'     'Date',

            'S' 'FIELDNAME'        'TRAN_TIME',
            'E' 'REPTEXT_DDIC'     'Time',

            'S' 'FIELDNAME'        'ZR2PRO',
            'E' 'REPTEXT_DDIC'      'Re-Processing Ind',

            'S' 'FIELDNAME'        'ZREDOC',
            'E' 'REPTEXT_DDIC'     'Re-Processing Doc',

            'S' 'FIELDNAME'        'BANFN',
            ' ' 'EMPHASIZE'        'C100',
            ' ' 'HOTSPOT'          'X',
            'E' 'REPTEXT_DDIC'     'P/R Number',

            'S' 'FIELDNAME'        'BNFPO',
            'E' 'REPTEXT_DDIC'     'Item Number',

            'S' 'FIELDNAME'        'BSART',
            'E' 'REPTEXT_DDIC'     'P/R Doc Type',

            'S' 'FIELDNAME'        'KNTTP',
            'E' 'REPTEXT_DDIC'     'Account Assignment Category',

            'S' 'FIELDNAME'        'PSTYP',
            'E' 'REPTEXT_DDIC'     'Item Category',

            'S' 'FIELDNAME'        'MATNR',
            'E' 'REPTEXT_DDIC'     'Material Number',

            'S' 'FIELDNAME'        'TXZ01',
            'E' 'REPTEXT_DDIC'     'Short Text',

            'S' 'FIELDNAME'        'WERKS',
            'E' 'REPTEXT_DDIC'     'Plant',

            'S' 'FIELDNAME'        'LGORT',
            ' ' 'REPTEXT_DDIC'     'Storage Location',

            'S' 'FIELDNAME'        'MATKL',
            'E' 'REPTEXT_DDIC'     'Material Group',

            'S' 'FIELDNAME'        'MENGE',
            ' ' 'REPTEXT_DDIC'     'P/R Quantity',

            'S' 'FIELDNAME'        'MEINS',
            'E' 'REPTEXT_DDIC'     'P/R Unit of Measure',

            'S' 'FIELDNAME'        'LPEIN',
            'E' 'REPTEXT_DDIC'     'Category of Delivery Date',

            'S' 'FIELDNAME'        'LFDAT',
            'E' 'REPTEXT_DDIC'     'Item Delivery Date',

            'S' 'FIELDNAME'        'PREIS',
            'E' 'REPTEXT_DDIC'     'P/R Price',

            'S' 'FIELDNAME'        'PEINH',
            'E' 'REPTEXT_DDIC'     'Price Unit',

            'S' 'FIELDNAME'        'EKGRP',
            'E' 'REPTEXT_DDIC'     'P/R Group',

            'S' 'FIELDNAME'        'EKORG',
            'E' 'REPTEXT_DDIC'     'Purchasing Organization',

            'S' 'FIELDNAME'        'AFNAM',
            'E' 'REPTEXT_DDIC'     'Name of Requisitioner/Requester',

            'S' 'FIELDNAME'        'BEDNR',
            'E' 'REPTEXT_DDIC'     'Requirement Tracking Number',

            'S' 'FIELDNAME'        'LIFNR',
            'E' 'REPTEXT_DDIC'     'Desired Vendor',

            'S' 'FIELDNAME'        'DISPO',
            'E' 'REPTEXT_DDIC'     'MRP Controller',

            'S' 'FIELDNAME'        'FLIEF',
            'E' 'REPTEXT_DDIC'     'Fixed Vendor',

            'S' 'FIELDNAME'        'WAERS',
            'E' 'REPTEXT_DDIC'     'Currency',

            'S' 'FIELDNAME'        'KOSTL',
            'E' 'REPTEXT_DDIC'     'Cost Center',

            'S' 'FIELDNAME'        'AUFNR',
            'E' 'REPTEXT_DDIC'     'Order Number',

            'S' 'FIELDNAME'        'ANLN1',
            'E' 'REPTEXT_DDIC'     'Asset',

            'S' 'FIELDNAME'        'SAKTO',
            'E' 'REPTEXT_DDIC'     'G/L Account Number',

            'S' 'FIELDNAME'        'KOKRS',
            'E' 'REPTEXT_DDIC'     'Controlling Area',

            'S' 'FIELDNAME'        'GEBER',
            'E' 'REPTEXT_DDIC'     'Fund',

            'S' 'FIELDNAME'        'FISTL',
            'E' 'REPTEXT_DDIC'     'Funds Center',

            'S' 'FIELDNAME'        'FIPOS',
            'E' 'REPTEXT_DDIC'     'Commitment Item'.
*---// Modification 2006.01.24
*--In case is outbound, check as do not display field on screen--------*
  IF p_outbnd NE 'X'.
    PERFORM fill_field_category USING :
      'S' 'FIELDNAME'        'ZZVZ_PR',
      'E' 'REPTEXT_DDIC'     'VAATZ P/R Unique number',

      'S' 'FIELDNAME'        'ZZVZ_ITEM',
      'E' 'REPTEXT_DDIC'     'VZZTZ P/R ITEM Unique number'.
  ENDIF.
*----------------------------------------------------------------------*
ENDFORM.                    " build_fieldcat
*&---------------------------------------------------------------------*
*&      Form  fill_field_category
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0302   text
*      -->P_0303   text
*      -->P_0304   text
*----------------------------------------------------------------------*
FORM fill_field_category  USING  p_gub p_fname p_con.
  IF p_gub = 'S'.
    CLEAR gs_fieldcat.
  ENDIF.
* Field symbol MOVE
  DATA l_col(40).
  FIELD-SYMBOLS <fs>.
  CONCATENATE 'GS_FIELDCAT-' p_fname  INTO l_col.
  ASSIGN      (l_col)         TO       <fs>.
  MOVE         p_con          TO       <fs>.

  IF p_gub = 'E'.
    APPEND gs_fieldcat TO gt_fieldcat.
  ENDIF.
ENDFORM.                    " FILL_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*&      Form  layout_init
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_LAYOUT_S  text
*----------------------------------------------------------------------*
FORM layout_init USING p_layout_s TYPE slis_layout_alv.
  p_layout_s-colwidth_optimize = 'X'.
  p_layout_s-zebra             = 'X'.
  p_layout_s-box_fieldname     = 'CHK'.

* PRINTING SETTINGS
  p_layout_s-get_selinfos = 'X'.
  p_layout_s-group_change_edit = 'X'.

  alv_print-no_print_selinfos = 'X'.
  alv_print-no_coverpage = 'X'.
  alv_print-no_print_listinfos = 'X'.
ENDFORM.                    " layout_init
*&------------------------------------------------------------------*
*&      Form  PF_STATUS_SET
*&------------------------------------------------------------------*
*       ??(REUSE_ALV_EVENTS_GET)?? CALL? FORM ???.
*-------------------------------------------------------------------*
FORM  pf_status_set USING p_rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD'.
  SET TITLEBAR  'LIST'.
ENDFORM.                    "PF_STATUS_SET
*&---------------------------------------------------------------------*
*&      Form  eventtab_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_EVENTS_T[]  text
*----------------------------------------------------------------------*
FORM eventtab_build USING p_events_t TYPE slis_t_event.

  DATA : l_event_s     TYPE slis_alv_event.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            i_list_type = 0
       IMPORTING
            et_events   = g_events_t.

** REUSE_ALV_EVENTS_GET ?? ???? - FORM PF_STATUS_SET
  READ TABLE g_events_t WITH KEY name = slis_ev_pf_status_set
                        INTO l_event_s.

  IF sy-subrc EQ 0.

    MOVE  c_status_set  TO l_event_s-form.
    APPEND l_event_s    TO g_events_t.

  ENDIF.
** REUSE_ALV_EVENTS_GET ?? ???? - FORM USER_COMMAND
  READ TABLE g_events_t WITH KEY name = slis_ev_user_command
                        INTO l_event_s.

  IF sy-subrc EQ 0.

    MOVE   c_user_command TO l_event_s-form.
    APPEND l_event_s      TO g_events_t.

  ENDIF.

** REUSE_ALV_EVENTS_GET 에서 수행된다 - FORM TOP_OF_PAGE
  READ TABLE g_events_t WITH KEY name = slis_ev_top_of_page
                        INTO l_event_s.

  IF sy-subrc EQ 0.

    MOVE   c_top_of_page TO l_event_s-form.
    APPEND l_event_s     TO g_events_t.

  ENDIF.
ENDFORM.                    " eventtab_build
*&---------------------------------------------------------------------*
*&      Form  GET_LOG_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_log_data .
*---
  CLEAR it_tab. REFRESH it_tab.

  CASE 'X'.
    WHEN p_sucess.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE it_tab
               FROM ztmm_if005
              WHERE flag IN r_flag
                AND banfn IN s_banfn
                AND serno IN s_serno
                AND tran_date IN s_date
                AND type IN ('S', 'R').
    WHEN p_error.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE it_tab
               FROM ztmm_if005
              WHERE flag IN r_flag
                AND banfn IN s_banfn
                AND serno IN s_serno
                AND tran_date IN s_date
                AND type EQ 'E'.
    WHEN p_rep.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE it_tab
               FROM ztmm_if005
              WHERE flag IN r_flag
                AND banfn IN s_banfn
                AND serno IN s_serno
                AND tran_date IN s_date
                AND ( type EQ 'R' OR zr2pro EQ 'S' ).
    WHEN p_schall.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE it_tab
               FROM ztmm_if005
              WHERE flag IN r_flag
                AND banfn IN s_banfn
                AND serno IN s_serno
                AND tran_date IN s_date.
  ENDCASE.

*  SELECT * FROM ztmm_if005
*           INTO CORRESPONDING FIELDS OF TABLE it_tab
*          WHERE flag       IN r_flag
*            AND type       IN r_type
*            AND banfn      IN s_banfn
*            AND serno      IN s_serno
*            AND matnr      IN s_matnr
*            AND tran_date  IN s_date.

  DATA : l_tabix LIKE sy-tabix.

  LOOP AT it_tab.
    MOVE : sy-tabix TO l_tabix.
    CASE it_tab-type.
      WHEN 'S'.
        MOVE : '@08@' TO it_tab-icon.
      WHEN 'E'.
        IF it_tab-zr2pro EQ 'S'.
          MOVE : '@09@' TO it_tab-icon.
        ELSE.
          MOVE : '@0A@' TO it_tab-icon.
        ENDIF.
      WHEN 'R'.
        MOVE : '@09@' TO it_tab-icon.
    ENDCASE.
    MODIFY it_tab INDEX l_tabix.
  ENDLOOP.

  SORT it_tab BY serno DESCENDING
                 cunt ASCENDING
                 banfn ASCENDING
                 bnfpo ASCENDING.
ENDFORM.                    " GET_LOG_DATA
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
FORM user_command USING   r_ucomm      LIKE sy-ucomm
                          rs_selfield  TYPE slis_selfield.
  DATA l_ans.
  rs_selfield-refresh = 'X'.

  CASE r_ucomm.

*---// Refresh button click
    WHEN 'REFR'.
      PERFORM get_log_data.
*---// P/R No click -> ME53N displsy
    WHEN '&IC1'.
      READ TABLE it_tab INDEX rs_selfield-tabindex.
      IF sy-subrc = 0.
        IF rs_selfield-fieldname = 'BANFN'.
          SET PARAMETER ID 'BAN' FIELD it_tab-banfn.
          CALL TRANSACTION 'ME53N'.
        ENDIF.
      ENDIF.
*---// Detail button click -> Header & item detail display
    WHEN 'REPV'.
      CLEAR:   it_out, count.
*---// multi selection search checking
      LOOP AT it_tab.
        IF it_tab-chk = 'X'.
          count = count + 1.
        ENDIF.
      ENDLOOP.
*---// searching display multi select error
      IF count > 1.
        MESSAGE i002.

*        MESSAGE 'Selected several case.Please check your select is
*possible inquiry of only one case.' TYPE 'I'.

      ELSE.
        DATA : l_serno LIKE it_tab-serno,
               l_banfn LIKE it_tab-banfn,
               l_flag  LIKE it_tab-flag.
        READ TABLE it_tab WITH KEY chk = 'X'.
        IF sy-subrc = 0.
          CLEAR : it_rep, it_rep[], l_serno, l_banfn, l_flag.
          MOVE : it_tab-serno TO l_serno,
                 it_tab-banfn TO l_banfn,
                 it_tab-flag  TO l_flag.
          LOOP AT it_tab WHERE serno EQ l_serno
                           AND banfn EQ l_banfn.
            MOVE-CORRESPONDING it_tab TO it_rep.
            APPEND it_rep.
          ENDLOOP.

*          MOVE-CORRESPONDING it_tab TO ztmm_if005.
          PERFORM read_error_log USING l_serno l_flag.
          DESCRIBE TABLE it_rep LINES w_rep_lines.
          MOVE : 1 TO w_rep_index.
          READ TABLE it_rep INDEX w_rep_index.
          MOVE-CORRESPONDING it_rep TO ztmm_if005.
          CALL SCREEN '0100'.
        ELSE.
          MESSAGE i001.
*          MESSAGE'No data with search conditions are existed.' TYPE 'I'
          .
        ENDIF.
      ENDIF.
  ENDCASE.
ENDFORM.                    " USER_COMMAND
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
*---
  DATA : answer,
         save_okcode LIKE ok_code.

  MOVE : ok_code TO save_okcode.

  CLEAR : ok_code.

  CASE save_okcode.
    WHEN '&F03'.
      CLEAR : save_okcode.
      CLEAR : it_out, it_out[].
      LEAVE TO SCREEN 0.
    WHEN 'REP'.
      CLEAR : save_okcode.
      PERFORM popup_to_confirm USING    text-p01
                                        text-p02
                               CHANGING answer.
      CHECK answer = 'J'.

      PERFORM re_process_material.
    WHEN 'PREV'.
      CLEAR : save_okcode.
      IF w_rep_index EQ 1.
        MESSAGE s000 WITH 'The First Item'.
      ELSE.
        w_rep_index = w_rep_index - 1.
        IF w_rep_index LE 0.
          MOVE : 1 TO w_rep_index.
        ENDIF.
        CLEAR : it_rep.
        READ TABLE it_rep INDEX w_rep_index.
        MOVE-CORRESPONDING it_rep TO ztmm_if005.
      ENDIF.
    WHEN 'NEXT'.
      CLEAR : save_okcode.
      IF w_rep_index EQ w_rep_lines.
        MESSAGE s000 WITH 'The Last Item'.
      ELSE.
        w_rep_index = w_rep_index + 1.
        IF w_rep_index GE w_rep_lines.
          MOVE : w_rep_lines TO w_rep_index.
        ENDIF.
        CLEAR : it_rep.
        READ TABLE it_rep INDEX w_rep_index.
        MOVE-CORRESPONDING it_rep TO ztmm_if005.
      ENDIF.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  IF it_rep-icon NE '@0A@' OR it_rep-flag EQ '3'.
    SET PF-STATUS 'DETAIL' EXCLUDING 'REP'.
  ELSE.
    SET PF-STATUS 'DETAIL'.
  ENDIF.

*  SET TITLEBAR  'Detail List'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CREATE_OBJECT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE create_object OUTPUT.
  IF g_custom_container IS INITIAL.
    CREATE OBJECT g_custom_container
      EXPORTING container_name = 'CONTROL_AREA'.
    CREATE OBJECT alv_grid
      EXPORTING       i_parent = g_custom_container.
  ENDIF.
ENDMODULE.                 " CREATE_OBJECT  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TRANSFER_DATA  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE transfer_data OUTPUT.
* Set a layout for the grid control
  PERFORM layout.
  PERFORM field_cat.
  CALL METHOD alv_grid->set_table_for_first_display
    EXPORTING
      is_layout       = gs_layout
    CHANGING
      it_fieldcatalog = gt_fdcat
      it_outtab       = it_out[].
ENDMODULE.                 " TRANSFER_DATA  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM layout .
  CLEAR gs_layout.
  gs_layout-cwidth_opt = 'X'.  "??? ???
  gs_layout-zebra      = 'X'.
ENDFORM.                    " LAYOUT
*&---------------------------------------------------------------------*
*&      Form  FIELD_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM field_cat .
  REFRESH gt_fdcat.
  PERFORM field_cat_build USING :

    'S' 'FIELDNAME'       'SERNOI',
    'E' 'COLTEXT'         'Item Sequential No',

    'S' 'FIELDNAME'       'FLAG',
    'E' 'COLTEXT'         'FLAG',

    'S' 'FIELDNAME'       'TYPE',
    'E' 'COLTEXT'         'TYPE',

    'S' 'FIELDNAME'       'MESSAGE',
    'E' 'COLTEXT'         'MESSAGE'.
*---// Modification 2006.01.24
*--In case is outbound, check as do not display field on screen--------*
  IF p_outbnd NE 'X'.
    PERFORM field_cat_build USING :
      'S' 'FIELDNAME'       'ZZVZ_PR',
      'E' 'COLTEXT'         'VAATZ P/R Unique number',

      'S' 'FIELDNAME'       'ZZVZ_ITEM',
      'E' 'COLTEXT'         'VAATZ P/R ITEM Unique number'.
  ENDIF.
*----------------------------------------------------------------------*
ENDFORM.                    " FIELD_CAT
*&---------------------------------------------------------------------*
*&      Form  FIELD_CAT_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1286   text
*      -->P_1287   text
*      -->P_1288   text
*----------------------------------------------------------------------*
FORM field_cat_build  USING    value(p_gub)
                               value(p_fname)
                               value(p_con).
  IF p_gub = 'S'.
    CLEAR gs_fdcat.
  ENDIF.
* ?? MOVE
  DATA l_col(40).
  FIELD-SYMBOLS <fs>.
  CONCATENATE 'GS_FDCAT-' p_fname  INTO l_col.
  ASSIGN      (l_col)         TO       <fs>.
  MOVE         p_con          TO       <fs>.

  IF p_gub = 'E'.
    APPEND gs_fdcat TO gt_fdcat.
  ENDIF.
ENDFORM.                    " FIELD_CAT_BUILD

*---------------------------------------------------------------------*
*       FORM top_of_page                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM top_of_page.
  DATA: lv_mode TYPE i.
  DATA: header TYPE slis_listheader OCCURS 0 WITH HEADER LINE.

  REFRESH header.

  header-typ = 'H'.

  header-info = 'Purchase Requisition log List'(t20).
  CLEAR: header-key.
  APPEND header.

  header-typ = 'S'.
  header-key = 'Print date'(106).
  WRITE: sy-datlo TO header-info.
  APPEND header.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            i_logo             = 'KMS_ALV_LOGO'
            it_list_commentary = header[].
ENDFORM.                    "top_of_page

*---------------------------------------------------------------------*
*       MODULE screen_control OUTPUT                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
MODULE screen_control OUTPUT.
  CHECK it_rep-icon EQ '@0A@'.  " error
  CHECK it_rep-zflag NE 'D'.    " delete
  CHECK it_rep-flag NE '3'.     " outbound

  LOOP AT SCREEN.
    screen-input = 1.
    MODIFY SCREEN.
  ENDLOOP.
ENDMODULE.                 " screen_control  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  popup_to_confirm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXT_P01  text
*      -->P_TEXT_P02  text
*      <--P_ANSWER  text
*----------------------------------------------------------------------*
FORM popup_to_confirm USING    p_text_p01
                               p_text_p02
                      CHANGING p_answer.
*----------------------------------------------------------------------*
*  MESSAGE POPUP
*----------------------------------------------------------------------*
  DATA: BEGIN OF pop,
        titel     LIKE spop-titel,
        diagnose1 LIKE spop-diagnose1,
        diagnose2 LIKE spop-diagnose2,
        diagnose3 LIKE spop-diagnose3,
        textline1 LIKE spop-textline1,
        textline2 LIKE spop-textline2,
        textline3 LIKE spop-textline3,
        option1   LIKE spop-varoption1,
        option2   LIKE spop-varoption2,
        default,
        answer,
        END OF pop.

  DATA: cancel_display.

  MOVE: p_text_p01 TO pop-textline1,
        p_text_p02 TO pop-titel.

  CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_MESSAGE'
    EXPORTING
*     DEFAULTOPTION        = 'Y'
      diagnosetext1        = pop-diagnose1
*     DIAGNOSETEXT2        = ' '
*     DIAGNOSETEXT3        = ' '
      textline1            = pop-textline1
      textline2            = pop-textline2
      titel                = pop-titel
*     START_COLUMN         = 25
*     START_ROW            = 6
      cancel_display       = cancel_display
    IMPORTING
      answer               = pop-answer
    EXCEPTIONS
      othsers              = 1.

  p_answer = pop-answer.
ENDFORM.                    " popup_to_confirm

*&---------------------------------------------------------------------*
*&      Form  re_process_material
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM re_process_material.
*---
  DATA : it_zsmm_if001 LIKE zsmm_if001 OCCURS 0 WITH HEADER LINE,
         it_zsmm_if017 LIKE zsmm_if017 OCCURS 0 WITH HEADER LINE.

  CLEAR : it_zsmm_if001, it_zsmm_if001[],
          it_zsmm_if017, it_zsmm_if017[].

  LOOP AT it_rep.
    MOVE-CORRESPONDING it_rep TO it_zsmm_if001.
    APPEND it_zsmm_if001.
  ENDLOOP.

*  READ TABLE it_tab WITH KEY chk = 'X'.
*
*  MOVE-CORRESPONDING it_tab TO it_zsmm_if001.
*  APPEND it_zsmm_if001.

  CALL FUNCTION 'ZMMF_IF_PR_INBOUND'
       TABLES
            it_zsmm_if001 = it_zsmm_if001
            e_return      = it_zsmm_if017.

*---
  DATA : l_serno LIKE ztmm_if005-serno.

  CLEAR : l_serno, it_rep.

  READ TABLE it_rep INDEX 1.

  SELECT MAX( serno ) INTO l_serno
                     FROM ztmm_if005
                    WHERE banfn EQ it_rep-banfn.

  IF it_zsmm_if017[] IS INITIAL.     " Success
    UPDATE ztmm_if005 SET zr2pro = 'S'
                          zredoc = l_serno
                    WHERE serno EQ it_rep-serno
                      AND banfn EQ it_rep-banfn.
    COMMIT WORK AND WAIT.
    UPDATE ztmm_if005 SET type = 'R'
                          zredoc = it_rep-serno
                    WHERE serno EQ l_serno.
    COMMIT WORK AND WAIT.
    MESSAGE i000(zmmm) WITH 'Success!!'.
  ELSE.                              " Error
    DELETE FROM ztmm_if005 WHERE serno EQ l_serno.
    DELETE FROM ztmm_if006 WHERE serno EQ l_serno.
    COMMIT WORK AND WAIT.
    DATA : it_xmsg LIKE ztismessage OCCURS 0 WITH HEADER LINE.
    LOOP AT it_zsmm_if017.
      MOVE: it_zsmm_if017-type    TO it_xmsg-msgty,
            it_zsmm_if017-message TO it_xmsg-msgtx.
      APPEND it_xmsg.
    ENDLOOP.
    CALL FUNCTION 'ZMM_IF_POPUP_TO_ERROR_MESSAGE'
         EXPORTING
              xdocno_show = 'X'
         TABLES
              xmsg        = it_xmsg.
  ENDIF.

*---
  PERFORM range_conversion.
  PERFORM get_log_data.

  LEAVE TO SCREEN 0.
ENDFORM.                    " re_process_material

*&---------------------------------------------------------------------*
*&      Form  build_sortcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_sortcat.
*---
  DATA : l_sort LIKE LINE OF gt_sort.

  CLEAR : l_sort, gt_sort[].

  l_sort-spos = 1.
  l_sort-fieldname = 'SERNO'.
  l_sort-down = 'X'.
  APPEND l_sort TO gt_sort.

  CLEAR : l_sort.

  l_sort-spos = 2.
  l_sort-fieldname = 'BANFN'.
  l_sort-up = 'X'.
  APPEND l_sort TO gt_sort.

*  CLEAR : l_sort.
*
*  l_sort-spos = 3.
*  l_sort-fieldname = 'BNFPO'.
*  l_sort-up = 'X'.
*  APPEND l_sort TO gt_sort.
ENDFORM.                    " build_sortcat

*&---------------------------------------------------------------------*
*&      Form  read_error_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_error_log USING p_serno p_flag.
*---
  CLEAR : it_out, it_out[].

  SELECT * FROM ztmm_if006
           INTO CORRESPONDING FIELDS OF TABLE it_out
          WHERE serno = p_serno
            AND flag  = p_flag.
ENDFORM.                    " read_error_log

*&---------------------------------------------------------------------*
*&      Module  modify_rep_tab  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_rep_tab INPUT.
*---
  MOVE-CORRESPONDING ztmm_if005 TO it_rep.

  MODIFY it_rep INDEX w_rep_index.
ENDMODULE.                 " modify_rep_tab  INPUT
