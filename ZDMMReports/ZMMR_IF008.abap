*&---------------------------------------------------------------------*
*& Report  ZMMR_IF008                                                  *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  zmmr_if008 MESSAGE-ID zmmr_if
                   NO STANDARD PAGE HEADING.

TABLES: ztmm_if012, " Goods Receipt Log table
        ztmm_if013. " Goods Receipt Error Log table

TYPE-POOLS: slis.

RANGES: r_type FOR ztmm_if013-type,
        r_flag FOR ztmm_if012-zflag.

DATA: BEGIN OF it_data OCCURS 10.
DATA: check.
DATA: icon(4).
        INCLUDE STRUCTURE ztmm_if012.
*      message LIKE ztmm_if013-message.
DATA: END OF it_data.

*---// Errors concerning selected item
DATA: BEGIN OF it_item_detail OCCURS 10.
        INCLUDE STRUCTURE ztmm_if013.
DATA: END OF it_item_detail.

DATA: BEGIN OF it_result OCCURS 10.
        INCLUDE STRUCTURE bapireturn.
DATA: END OF it_result.

DATA it_item  LIKE TABLE OF ztmm_if012 WITH HEADER LINE.
DATA it_error LIKE TABLE OF ztmm_if013 WITH HEADER LINE.

DATA is_goods_receipt LIKE ztmm_if012.

*---// ALV general field
DATA: g_exit_caused_by_caller  TYPE c,
      g_repid                  TYPE sy-repid,
      g_save                   TYPE c,
      g_program_name           LIKE sy-repid,
      g_inclname               LIKE trdir-name.

*---// Structures
DATA: g_st_layout              TYPE slis_layout_alv,
      g_st_fieldcat            TYPE slis_fieldcat_alv,
      g_st_exit_caused_by_user TYPE slis_exit_by_user,
      g_st_variant             TYPE disvariant.

DATA: g_it_events              TYPE slis_t_event,
      g_it_list_top_of_page    TYPE slis_t_listheader,
      g_it_fieldcat            TYPE slis_t_fieldcat_alv,
      g_it_sort                TYPE slis_t_sortinfo_alv.

DATA: alv_print            TYPE slis_print_alv.
DATA: alv_repid            LIKE sy-repid,
      alv_variant          LIKE disvariant.
DATA: alv_detail_func(30).
*-----------------------------------------------------------------------
*  Constants
*-----------------------------------------------------------------------
CONSTANTS: c_status_set   TYPE slis_formname VALUE 'PF_STATUS_SET',
           c_user_command TYPE slis_formname VALUE 'USER_COMMAND'.

*-----------------------------------------------------------------------
*  Controls
*-----------------------------------------------------------------------
DATA: g_custom_container TYPE REF TO cl_gui_custom_container,
      alv_grid           TYPE REF TO cl_gui_alv_grid.

DATA: g_st_detail_layout TYPE lvc_s_layo,
      g_st_detail_fdcat  TYPE lvc_s_fcat,
      g_it_detail_fdcat  TYPE lvc_t_fcat.
*-----------------------------------------------------------------------
*  Work fields
*-----------------------------------------------------------------------
DATA: g_subrc LIKE sy-subrc.

*---// Selection-screen layout
*** Search condition
SELECTION-SCREEN BEGIN OF BLOCK box1 WITH FRAME TITLE text-t01.
SELECT-OPTIONS:
  so_lfsnr   FOR ztmm_if012-lfsnr  MODIF ID iog,   " Delivery note
  so_ebeln   FOR ztmm_if012-ebeln,                 " PO number
  so_serno   FOR ztmm_if012-serno  NO-EXTENSION NO INTERVALS, " Serial
  so_mblnr   FOR ztmm_if012-mblnr,       " Material Document No.
  so_date    FOR sy-datum.      " Date on which the record was created
SELECTION-SCREEN END OF BLOCK box1.

*** Division
SELECTION-SCREEN BEGIN OF BLOCK box2 WITH FRAME TITLE text-t02.
PARAMETERS:
  p_create RADIOBUTTON GROUP gb2,     " create goods receipt log
  p_return RADIOBUTTON GROUP gb2,     " return goods receipt log
  p_c_all  RADIOBUTTON GROUP gb2 DEFAULT 'X'.       " all
SELECTION-SCREEN END OF BLOCK box2.

*** Result
SELECTION-SCREEN BEGIN OF BLOCK box3 WITH FRAME TITLE text-t03.
PARAMETERS:
  p_succ  RADIOBUTTON GROUP gb3,                    " success log
  p_error RADIOBUTTON GROUP gb3,                    " error log
  p_repro RADIOBUTTON GROUP gb3,                    " Re-process log
  p_r_all RADIOBUTTON GROUP gb3 DEFAULT 'X'.        " all
SELECTION-SCREEN END OF BLOCK box3.

*---// Initialization event
INITIALIZATION.
*** ALV layout initialization
  PERFORM alv_layout_init USING g_st_layout.
*** ALV event initialization
  PERFORM event_build     USING g_it_events[].

  g_repid              = sy-repid.
  g_program_name       = sy-repid.
  g_inclname           = sy-repid.
  g_st_variant-variant = '/ZMMR_IF008'.

*---// Start-of-selection event
START-OF-SELECTION.

  PERFORM range_input.
*** data selection using search conditions
  PERFORM get_data.

  READ TABLE it_data INDEX 1 TRANSPORTING NO FIELDS.
  IF sy-subrc NE 0.
    MESSAGE s001.
    EXIT.
  ENDIF.

END-OF-SELECTION.
*** field category define
  PERFORM build_fieldcat.
*** ALV display execution
  PERFORM alv_display.
*&---------------------------------------------------------------------*
*&      Form  alv_layout_init
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_ST_LAYOUT  text
*----------------------------------------------------------------------*
FORM alv_layout_init  USING    p_st_layout TYPE slis_layout_alv.
  p_st_layout-colwidth_optimize = 'X'.
  p_st_layout-zebra             = 'X'.
  p_st_layout-box_fieldname     = 'CHECK'.

  p_st_layout-get_selinfos      = 'X'.
  p_st_layout-group_change_edit = 'X'.

  alv_print-no_print_selinfos   = 'X'.
  alv_print-no_coverpage        = 'X'.
  alv_print-no_print_listinfos  = 'X'.
ENDFORM.                    " alv_layout_init
*&---------------------------------------------------------------------*
*&      Form  event_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_IT_EVENTS[]  text
*----------------------------------------------------------------------*
FORM event_build  USING    p_it_events TYPE slis_t_event.
  DATA: lst_event TYPE slis_alv_event.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            i_list_type = 0
       IMPORTING
            et_events   = p_it_events.
*   EXCEPTIONS
*     LIST_TYPE_WRONG       = 1
*     OTHERS                = 2

*** executed in REUSE_ALV_EVENTS_GET - form pf_status_set
  READ TABLE p_it_events WITH KEY name = slis_ev_pf_status_set
                         INTO lst_event.
  IF sy-subrc = 0.
    MOVE c_status_set TO lst_event-form.
    APPEND lst_event  TO g_it_events.
  ENDIF.
*** executed in REUSE_ALV_EVENTS_GET - form user_command
  READ TABLE p_it_events WITH KEY name = slis_ev_user_command
                         INTO lst_event.
  IF sy-subrc = 0.
    MOVE c_user_command TO lst_event-form.
    APPEND lst_event    TO g_it_events.
  ENDIF.
ENDFORM.                    " event_build
*&---------------------------------------------------------------------*
*&      Form  range_input
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM range_input .
  CLEAR: r_type, r_flag.
  CLEAR: it_data.

  REFRESH: r_type, r_flag.
  REFRESH: it_data.

*  IF p_succ EQ 'X'.
*    r_type-sign   = 'I'.
*    r_type-option = 'EQ'.
*    r_type-low    = 'S'.
*    APPEND r_type.
*    CLEAR  r_type.
*  ELSEIF p_error EQ 'X'.
*    r_type-sign   = 'I'.
*    r_type-option = 'EQ'.
*    r_type-low    = 'E'.
*    APPEND r_type.
*    CLEAR  r_type.
*  ENDIF.

  IF p_create EQ 'X'.
    r_flag-sign   = 'I'.
    r_flag-option = 'EQ'.
    r_flag-low    = 'C'.   " Create flag
    APPEND r_flag.
    CLEAR  r_flag.
  ELSEIF p_return EQ 'X'.
    r_flag-sign   = 'I'.
    r_flag-option = 'EQ'.
    r_flag-low    = 'D'.   " Return flag
    APPEND r_flag.
    CLEAR  r_flag.
  ENDIF.
ENDFORM.                    " range_input
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data .
  CLEAR: it_data, it_item.
  REFRESH: it_data, it_item.

*  SELECT * FROM ztmm_if012 AS dt INNER JOIN ztmm_if013 AS et
*           ON dt~serno = et~serno
*           INTO CORRESPONDING FIELDS OF TABLE it_data
*           WHERE dt~zflag IN r_flag
*           AND   et~type  IN r_type
*           AND   dt~lfsnr IN so_lfsnr
*           AND   dt~ebeln IN so_ebeln
*           AND   dt~serno IN so_serno
*           AND   dt~mblnr IN so_mblnr
*           AND   dt~erdat IN so_date.
*
*  DELETE ADJACENT DUPLICATES FROM it_data COMPARING type serno.
*  SORT it_data BY erdat DESCENDING serno DESCENDING.

  CASE 'X'.
    WHEN p_succ.
      SELECT * FROM ztmm_if012
               INTO CORRESPONDING FIELDS OF TABLE it_item
               WHERE type IN ('S', 'R')
               AND   lfsnr IN so_lfsnr
               AND   ebeln IN so_ebeln
               AND   serno IN so_serno
               AND   erdat IN so_date
               AND   zflag IN r_flag.

      SORT it_item BY erdat DESCENDING serno DESCENDING.
    WHEN p_error.
      SELECT * FROM ztmm_if012
               INTO CORRESPONDING FIELDS OF TABLE it_item
               WHERE type EQ 'E'
               AND   lfsnr IN so_lfsnr
               AND   ebeln IN so_ebeln
               AND   serno IN so_serno
               AND   erdat IN so_date
               AND   zflag IN r_flag.

      DELETE it_item WHERE zr2pro EQ 'S'.
      SORT it_item BY erdat DESCENDING serno DESCENDING.
    WHEN p_repro.
      SELECT * FROM ztmm_if012
               INTO CORRESPONDING FIELDS OF TABLE it_item
               WHERE ( type EQ 'R' OR zr2pro EQ 'S' )
               AND   lfsnr IN so_lfsnr
               AND   ebeln IN so_ebeln
               AND   serno IN so_serno
               AND   erdat IN so_date
               AND   zflag IN r_flag.

      SORT it_item BY erdat DESCENDING serno DESCENDING.
    WHEN OTHERS.
      SELECT * FROM ztmm_if012
               INTO CORRESPONDING FIELDS OF TABLE it_item
               WHERE lfsnr IN so_lfsnr
               AND   ebeln IN so_ebeln
               AND   serno IN so_serno
               AND   erdat IN so_date
               AND   zflag IN r_flag.

      SORT it_item BY erdat DESCENDING serno DESCENDING.
  ENDCASE.

  LOOP AT it_item.
    MOVE-CORRESPONDING it_item TO it_data.
    CASE it_item-type.
      WHEN 'S'.
        it_data-icon = '@08@'.
      WHEN 'E'.
        IF it_data-zr2pro = 'S'.
          it_data-icon = '@09@'.
        ELSE.
          it_data-icon = '@0A@'.
        ENDIF.
      WHEN 'R'.
        it_data-icon = '@09@'.
    ENDCASE.
    APPEND it_data.
    CLEAR  it_data.
  ENDLOOP.

ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcat .
  PERFORM fill_field_category USING:

    'S'    'FIELDNAME'         'ICON',
    ' '    'ICON'              'X',
    'E'    'REPTEXT_DDIC'      'Status',

    'S'    'FIELDNAME'         'SERNO',
    ' '    'JUST'              'R',
*    ' '    'KEY'               'X',
    ' '    'EDIT_MASK'         '==ALPHA',
    'E'    'REPTEXT_DDIC'      'Serial No',

*    'S'    'FIELDNAME'         'SEQNO',
*    'E'    'REPTEXT_DDIC'      'SEQUENCE',

    'S'    'FIELDNAME'         'ZFLAG',
    ' '    'JUST'              'L',
*    ' '    'KEY'               'X',
    'E'    'REPTEXT_DDIC'      'Flag',

*    'S'    'FIELDNAME'         'MESSAGE',
*    'E'    'REPTEXT_DDIC'      'MESSAGE',

    'S'    'FIELDNAME'         'LFSNR',
    ' '    'JUST'              'L',
*    ' '    'KEY'               'X',
    'E'    'REPTEXT_DDIC'      'Delivery note',

    'S'    'FIELDNAME'         'MBLNR',
    ' '    'JUST'              'L',
*    ' '    'KEY'               'X',
    ' '    'EMPHASIZE'         'C100',
    ' '    'HOTSPOT'            'X',
    'E'    'REPTEXT_DDIC'      'Material Doc No',

    'S'    'FIELDNAME'         'TYPE',
    ' '    'JUST'              'L',
*    ' '    'KEY'               'X',
    'E'    'REPTEXT_DDIC'      'Type',

    'S'    'FIELDNAME'         'ZR2PRO',
    ' '    'JUST'              'L',
    ' '    'EMPHASIZE'         'C300',
    'E'    'REPTEXT_DDIC'      'Re-processing Ind',

    'S'    'FIELDNAME'         'ZREDOC',
    ' '    'JUST'              'R',
    ' '    'EMPHASIZE'         'C300',
    'E'    'REPTEXT_DDIC'      'Re-processing Doc',

    'S'    'FIELDNAME'         'MJAHR',
    ' '    'JUST'              'R',
    'E'    'REPTEXT_DDIC'      'Document Year',

    'S'    'FIELDNAME'         'BLDAT',
    ' '    'JUST'              'R',
    'E'    'REPTEXT_DDIC'      'Document Date',

    'S'    'FIELDNAME'         'BUDAT',
    ' '    'JUST'              'R',
    'E'    'REPTEXT_DDIC'      'Posting Date',

    'S'    'FIELDNAME'         'FRBNR',
    ' '    'JUST'              'L',
    'E'    'REPTEXT_DDIC'      'Bill of Lading',

    'S'    'FIELDNAME'         'BKTXT',
    ' '    'JUST'              'L',
    'E'    'REPTEXT_DDIC'      'Header Text',

    'S'    'FIELDNAME'         'MATNR',
    ' '    'JUST'              'L',
    'E'    'REPTEXT_DDIC'      'Material',

    'S'    'FIELDNAME'         'ERFMG',
    ' '    'JUST'              'R',
    'E'    'REPTEXT_DDIC'      'Qty in Unit',

    'S'    'FIELDNAME'         'ERFME',
    ' '    'JUST'              'L',
    'E'    'REPTEXT_DDIC'      'Unit',

    'S'    'FIELDNAME'         'BWARTWA',
    ' '    'JUST'              'R',
    'E'    'REPTEXT_DDIC'      'Movement Type',

    'S'    'FIELDNAME'         'GRUND',
    ' '    'JUST'              'R',
    'E'    'REPTEXT_DDIC'      'Reason Code',

    'S'    'FIELDNAME'         'WERKS',
    ' '    'JUST'              'L',
    'E'    'REPTEXT_DDIC'      'Plant',

    'S'    'FIELDNAME'         'WEMPF',
    ' '    'JUST'              'R',
    'E'    'REPTEXT_DDIC'      'Recipient',

    'S'    'FIELDNAME'         'LGORT',
    ' '    'JUST'              'L',
    'E'    'REPTEXT_DDIC'      'Storage Loc.',

    'S'    'FIELDNAME'         'FISTL',
    ' '    'JUST'              'L',
    'E'    'REPTEXT_DDIC'      'Funds Center',

    'S'    'FIELDNAME'         'FIPOS',
    ' '    'JUST'              'R',
    'E'    'REPTEXT_DDIC'      'Commitment Item',

    'S'    'FIELDNAME'         'KNTTP',
    ' '    'JUST'              'L',
    'E'    'REPTEXT_DDIC'      'Account Assign Cat.',

    'S'    'FIELDNAME'         'EBELN',
    ' '    'JUST'              'L',
    'E'    'REPTEXT_DDIC'      'PO Number',

    'S'    'FIELDNAME'         'EBELP',
    ' '    'JUST'              'R',
    'E'    'REPTEXT_DDIC'      'PO Item No.',

    'S'    'FIELDNAME'         'ABLAD',
    ' '    'JUST'              'R',
    'E'    'REPTEXT_DDIC'      'Unloading Point',

    'S'    'FIELDNAME'         'ERDAT',
    ' '    'JUST'              'R',
*    ' '    'KEY'               'X',
    'E'    'REPTEXT_DDIC'      'Created Date',

    'S'    'FIELDNAME'         'ERZET',
    ' '    'JUST'              'R',
*    ' '    'KEY'               'X',
    'E'    'REPTEXT_DDIC'      'Created Time',

    'S'    'FIELDNAME'         'ERNAM',
    ' '    'JUST'              'L',
    'E'    'REPTEXT_DDIC'      'User_name'.


ENDFORM.                    " build_fieldcat
*&---------------------------------------------------------------------*
*&      Form  alv_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_display .
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
     i_callback_program                = g_repid
     i_background_id                   = 'ALV_BACKGROUND'
*     I_GRID_TITLE                      =
*     I_GRID_SETTINGS                   =
     is_layout                         = g_st_layout
     it_fieldcat                       = g_it_fieldcat[]
*     IT_EXCLUDING                      =
*     IT_SPECIAL_GROUPS                 =
     it_sort                           = g_it_sort
*     IT_FILTER                         =
*     IS_SEL_HIDE                       =
*     I_DEFAULT                         = 'X'
     i_save                            = g_save
     is_variant                        = alv_variant
     it_events                         = g_it_events[]
*     IT_EVENT_EXIT                     =
     is_print                          = alv_print
*     IS_REPREP_ID                      =
*     I_SCREEN_START_COLUMN             = 0
*     I_SCREEN_START_LINE               = 0
*     I_SCREEN_END_COLUMN               = 0
*     I_SCREEN_END_LINE                 = 0
*     IT_ALV_GRAPHICS                   =
*     IT_HYPERLINK                      =
*     IT_ADD_FIELDCAT                   =
*     IT_EXCEPT_QINFO                   =
*     I_HTML_HEIGHT_TOP                 =
*     I_HTML_HEIGHT_END                 =
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER           =
*     ES_EXIT_CAUSED_BY_USER            =
    TABLES
      t_outtab                          = it_data[]
   EXCEPTIONS
     program_error                     = 1
     OTHERS                            = 2.

  IF sy-subrc <> 0.
    CASE sy-subrc.
      WHEN 1.
        MESSAGE s002.
      WHEN 2.
        MESSAGE s003.
    ENDCASE.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " alv_display
*&---------------------------------------------------------------------*
*&      Form  fill_field_category
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0514   text
*      -->P_0515   text
*      -->P_0516   text
*----------------------------------------------------------------------*
FORM fill_field_category  USING    p_cat p_fnam p_con.
  IF p_cat = 'S'.
    CLEAR g_st_fieldcat.
  ENDIF.

  DATA lv_col(40).
  FIELD-SYMBOLS <fs>.
  CONCATENATE 'G_ST_FIELDCAT-' p_fnam INTO lv_col.
  ASSIGN (lv_col)     TO <fs>.
  MOVE    p_con       TO <fs>.

  IF p_cat = 'E'.
    APPEND g_st_fieldcat TO g_it_fieldcat.
  ENDIF.
ENDFORM.                    " fill_field_category
*&---------------------------------------------------------------------*
*&      Form  pf_status_set
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_RT_EXTAB  text
*----------------------------------------------------------------------*
FORM pf_status_set  USING    p_rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD'.
  SET TITLEBAR  'TITLE'.

ENDFORM.                    " pf_status_set
*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_R_UCOMM  text
*      -->P_RS_SELFIELD  text
*----------------------------------------------------------------------*
FORM user_command  USING    r_ucomm      LIKE sy-ucomm
                            rs_selfield  TYPE slis_selfield.
  DATA lv_ans.
  rs_selfield-refresh = 'X'.

  CASE r_ucomm.
    WHEN 'REFR'. " refresh
      PERFORM get_data.
*---// " move to migo when user selects material document number
    WHEN '&IC1'.
      READ TABLE it_data INDEX rs_selfield-tabindex.
      IF sy-subrc = 0.
        IF rs_selfield-fieldname = 'MBLNR' AND
        NOT it_data-mblnr IS INITIAL.
          CALL FUNCTION 'MIGO_DIALOG'
           EXPORTING
             i_action                  = 'A04'
             i_refdoc                  = 'R02'
             i_notree                  = 'X'
             i_no_auth_check           = ' '
             i_skip_first_screen       = 'X'
             i_deadend                 = 'X'
             i_okcode                  = 'OK_GO'
*           I_LEAVE_AFTER_POST        =
*           I_NEW_ROLLAREA            = 'X'
*           I_SYTCODE                 =
*           I_EBELN                   =
*           I_EBELP                   =
             i_mblnr                   = it_data-mblnr.
*           I_MJAHR                   =
*           I_ZEILE                   =
*           I_TRANSPORT               =
*           I_ORDER_NUMBER            =
*           I_ORDER_ITEM              =
*           I_TRANSPORT_MEANS         =
*           I_TRANSPORTIDENT          =
*           I_INBOUND_DELIV           =
*           I_OUTBOUND_DELIV          =
*           I_RESERVATION_NUMB        =
*           I_RESERVATION_ITEM        =
*         EXCEPTIONS
*           ILLEGAL_COMBINATION       = 1
*           OTHERS                    = 2
        ENDIF.
      ENDIF.
*    WHEN 'DETA'. " selected line detail list view
*      CLEAR: it_item_detail.
*
*      READ TABLE it_data WITH KEY check = 'X'.
*      IF sy-subrc = 0.
*        MOVE-CORRESPONDING it_data TO ztmm_if012.
*
*        ztmm_if012-erfmg = abs( it_data-erfmg ).
*
*        SELECT * FROM ztmm_if013
*                 INTO CORRESPONDING FIELDS OF TABLE it_item_detail
*                 WHERE serno = it_data-serno.
*        CALL SCREEN '0100'.
*      ELSE.
*        MESSAGE i004.
*      ENDIF.
    WHEN 'REPV'.                  " Re-processing Goods Receipt
      CLEAR it_item_detail.
      REFRESH it_item_detail.

      READ TABLE it_data WITH KEY check = 'X'.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING it_data TO ztmm_if012.

        SELECT * FROM ztmm_if013
                 INTO CORRESPONDING FIELDS OF TABLE it_item_detail
                 WHERE serno = it_data-serno.
        CALL SCREEN '0100'.
      ELSE.
        MESSAGE i004.
      ENDIF.
  ENDCASE.
ENDFORM.                    " user_command
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  IF ztmm_if012-type   EQ 'S'
  OR ztmm_if012-type   EQ 'R'
  OR ztmm_if012-zr2pro EQ 'S'.

    SET PF-STATUS 'DETAIL' EXCLUDING 'REP'.
  ELSE.
    SET PF-STATUS 'DETAIL'.
  ENDIF.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
*  IF sy-ucomm = '&F03'.
*    CLEAR   it_item_detail.
*    REFRESH it_item_detail.
*    LEAVE TO SCREEN 0.
*  ENDIF.
  DATA answer.
  CASE sy-ucomm.
    WHEN '&F03'.
      CLEAR it_item_detail.
      REFRESH it_item_detail.
      LEAVE TO SCREEN 0.
    WHEN 'REP'.
      PERFORM popup_to_confirm USING    text-p01
                                        text-p02
                               CHANGING answer.
      CHECK answer = 'J'.

      PERFORM re_process_goods_receipt.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  create_object  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE create_object OUTPUT.
  IF g_custom_container IS INITIAL.
    CREATE OBJECT g_custom_container
      EXPORTING container_name = 'CONTROL_AREA'.
    CREATE OBJECT alv_grid
      EXPORTING i_parent = g_custom_container.
  ENDIF.

ENDMODULE.                 " create_object  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  transfer_data  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE transfer_data OUTPUT.
  PERFORM detail_layout.
  PERFORM detail_field_cat.
  CALL METHOD alv_grid->set_table_for_first_display
    EXPORTING
      is_layout       = g_st_detail_layout
    CHANGING
      it_fieldcatalog = g_it_detail_fdcat
      it_outtab       = it_item_detail[].

ENDMODULE.                 " transfer_data  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  detail_layout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM detail_layout .
  CLEAR g_st_detail_layout.
  g_st_detail_layout-cwidth_opt = 'X'.
  g_st_detail_layout-zebra      = 'X'.

ENDFORM.                    " detail_layout
*&---------------------------------------------------------------------*
*&      Form  detail_field_cat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM detail_field_cat .
  REFRESH g_it_detail_fdcat.
  PERFORM detail_fdcat_build USING:

    'S'    'FIELDNAME'         'SERNO',
    'E'    'COLTEXT'           'SERIAL No',

    'S'    'FIELDNAME'         'E_SEQNO',
    'E'    'COLTEXT'           'SEQUENCE No',

    'S'    'FIELDNAME'         'TYPE',
    'E'    'COLTEXT'           'TYPE',

    'S'    'FIELDNAME'         'MESSAGE',
    'E'    'COLTEXT'           'MESSAGE'.

ENDFORM.                    " detail_field_cat
*&---------------------------------------------------------------------*
*&      Form  detail_fdcat_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1200   text
*      -->P_1201   text
*      -->P_1202   text
*----------------------------------------------------------------------*
FORM detail_fdcat_build  USING    value(p_cat)
                                  value(p_fnam)
                                  value(p_con).
  IF p_cat = 'S'.
    CLEAR g_st_detail_fdcat.
  ENDIF.

  DATA l_col(40).
  FIELD-SYMBOLS <fs>.
  CONCATENATE 'G_ST_DETAIL_FDCAT-' p_fnam INTO l_col.
  ASSIGN (l_col)     TO <fs>.
  MOVE    p_con      TO <fs>.

  IF p_cat = 'E'.
    APPEND g_st_detail_fdcat TO g_it_detail_fdcat.
  ENDIF.

ENDFORM.                    " detail_fdcat_build
*&---------------------------------------------------------------------*
*&      Module  screen_control  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE screen_control OUTPUT.
  IF ztmm_if012-type   EQ 'S'
  OR ztmm_if012-type   EQ 'R'
  OR ztmm_if012-zr2pro EQ 'S'.

    LOOP AT SCREEN.
      IF screen-group1 = 'G1' OR screen-group2 = 'G2'.
        screen-input = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ELSE.
    IF ztmm_if012-zflag = 'D'.
      LOOP AT SCREEN.
        IF screen-group1 = 'G1'.
          screen-input = 0.
        ENDIF.
        MODIFY SCREEN.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDMODULE.                 " screen_control  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  display_alv  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_alv OUTPUT.
  DATA: row_no   TYPE lvc_s_roid,
        row_info TYPE lvc_s_row,
        col_info TYPE lvc_s_col.

  IF g_custom_container IS INITIAL.
    CREATE OBJECT g_custom_container
      EXPORTING container_name = 'CONTROL_AREA'.

    CREATE OBJECT alv_grid
      EXPORTING i_parent = g_custom_container.

    PERFORM detail_layout.

    PERFORM detail_field_cat.

    CALL METHOD alv_grid->set_table_for_first_display
      EXPORTING
        is_layout       = g_st_detail_layout
      CHANGING
        it_fieldcatalog = g_it_detail_fdcat
        it_outtab       = it_item_detail[].
  ELSE.
    CALL METHOD alv_grid->get_scroll_info_via_id
      IMPORTING
        es_row_no   = row_no
        es_row_info = row_info
        es_col_info = col_info.

    CALL METHOD alv_grid->refresh_table_display.

    CALL METHOD alv_grid->set_scroll_info_via_id
      EXPORTING
        is_row_info = row_info
        is_col_info = col_info
        is_row_no   = row_no.
  ENDIF.
ENDMODULE.                 " display_alv  OUTPUT
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
*&      Form  re_process_goods_receipt
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM re_process_goods_receipt.

  DATA: is_gr_header LIKE zsmm_if003,
        lv_gr_data   LIKE ztmm_if012,
        lv_r_serno   LIKE ztmm_if012-serno,
        lv_serno     LIKE ztmm_if012-serno.

  DATA: it_xmsg LIKE TABLE OF ztismessage WITH HEADER LINE.
  DATA: it_gr_item LIKE TABLE OF zsmm_if004 WITH HEADER LINE.

  CLEAR: it_result, it_xmsg, it_gr_item.
  CLEAR: is_gr_header, lv_gr_data,
         lv_r_serno, lv_serno.

  REFRESH: it_result, it_xmsg, it_gr_item.

  MOVE-CORRESPONDING ztmm_if012 TO lv_gr_data.
  MOVE-CORRESPONDING ztmm_if012 TO is_gr_header.
  MOVE: ztmm_if012-serno        TO lv_r_serno.

  MOVE-CORRESPONDING ztmm_if012 TO it_gr_item.
  APPEND it_gr_item.
  CLEAR  it_gr_item.

  CLEAR ztmm_if012.

  CALL FUNCTION 'ZMMF_IF_GOODS_RECEIPT'
       EXPORTING
            i_gr_header = is_gr_header
       TABLES
            t_gr_item   = it_gr_item
            e_return    = it_result.

  IF sy-subrc EQ 0.
    READ TABLE it_result WITH KEY type = 'S'.

    IF sy-subrc EQ 0.
      SELECT MAX( serno ) INTO lv_serno
                          FROM ztmm_if012
                          WHERE lfsnr = lv_gr_data-lfsnr.
      IF sy-subrc EQ 0.
        UPDATE ztmm_if012 SET   zr2pro = 'S'
                                zredoc = lv_serno
                          WHERE serno  = lv_r_serno.
        COMMIT WORK AND WAIT.

        IF sy-subrc EQ 0.
          CLEAR: ztmm_if012.
          UPDATE ztmm_if012 SET   type   = 'R'
                                  zredoc = lv_r_serno
                            WHERE serno  = lv_serno.
          COMMIT WORK AND WAIT.
        ENDIF.
      ENDIF.
      PERFORM get_data.
      MESSAGE s005.
      LEAVE TO SCREEN 0.
    ELSE.
      SELECT MAX( serno ) INTO lv_serno
                          FROM ztmm_if012
                          WHERE lfsnr   = lv_gr_data-lfsnr.
      IF sy-subrc EQ 0.
        DELETE FROM ztmm_if012 WHERE serno = lv_serno.
        DELETE FROM ztmm_if013 WHERE serno = lv_serno.
        COMMIT WORK AND WAIT.
      ENDIF.

      CLEAR it_xmsg.
      LOOP AT it_result.
        MOVE: it_result-type    TO it_xmsg-msgty,
              it_result-message TO it_xmsg-msgtx.
        APPEND it_xmsg.
      ENDLOOP.

      CALL FUNCTION 'ZMM_IF_POPUP_TO_ERROR_MESSAGE'
        EXPORTING
*          xlogno      =
          xdocno_show = 'X'
        TABLES
          xmsg   = it_xmsg.

      LEAVE TO SCREEN 0.
    ENDIF.
  ENDIF.
ENDFORM.                    " re_process_goods_receipt
