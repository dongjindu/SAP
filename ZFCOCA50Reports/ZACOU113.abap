*----------------------------------------------------------------------
* Program ID        : ZACOU113
* Title             : [CO] Mass-Raw Mat.Cost for New Material
* Created on        : 09/19/2006
* Created by        : Michelle Jeong
* Specifications By : Andy Choi
* Description       : Mass-Raw Mat.Cost program for new material.
*   <Processing>
*    1. Change Material: BAPI_MATERIAL_SAVEDATA
*    2. Create Cost estimate for material : CK11
*    3. Price Change : MR21
*    4. Modify Costing Result Table ZTCOU102
*    5. Save log table ZTCOU113: error only
*       Delete log table ZTCOU113: success only
*    6. Display result
*
*   <Reprocessing>
*    1. Display log
*    2. Modify cost result (vendor, price, reason)
*    3. Display price history (Prog.ZACOU112)
*    4. Processing remaining steps
*    5. Save log table ZTCOU113: error only
*       Delete log table ZTCOU113: success only
*----------------------------------------------------------------------
REPORT ZACOU113 NO STANDARD PAGE HEADING MESSAGE-ID ZMCO.

*INCLUDE ZACOUI00.
*---------------------------------------------------------------------*
*  Include           ZACOUI00
*---------------------------------------------------------------------*
*  Define Variables and Internal Tables & Subroutines for ALV
*  Common include...
*---------------------------------------------------------------------*

* Define Variables and Internal Tables for ALV
type-pools: slis.

include <icon>.
include <symbol>.

constants: gc_formname_top_of_page type slis_formname
                                   value 'TOP_OF_PAGE',
           gc_var_save       type c value  'A',
           gc_pf_status_set  type slis_formname value 'PF_STATUS_SET',
           gc_user_command   type slis_formname value 'USER_COMMAND',
           gc_tvers          type ck_tvers      value '01'.

data: gt_list_top_of_page  type slis_t_listheader,
      gt_list_top_of_page1 type slis_t_listheader,
      gt_fieldcat          type slis_t_fieldcat_alv,
      gs_fieldcat          type slis_fieldcat_alv,
      gs_layout            type slis_layout_alv,
      gt_events            type slis_t_event,
      gt_specialcol        type slis_t_specialcol_alv,
      gs_specialcol        type slis_specialcol_alv.

data: gv_default(1)  type c,
      gs_variant  like disvariant,
      gs_variant1 like disvariant,
      gv_repid    like sy-repid.

* for ALV Grid
data : gt_exclude   type ui_functions,
       gt_exclude1  type ui_functions,
       gs_print     type lvc_s_prnt,
       container    type scrfname value 'G_CUSTOM_CONTAINER',
       container1   type scrfname value 'G_CUSTOM_CONTAINER1',
       gs_fcat      type lvc_s_fcat,
       gt_fcat      type lvc_t_fcat,
       gs_layo      type lvc_s_layo,
       gs_fcat1     type lvc_s_fcat,
       gt_fcat1     type lvc_t_fcat,
       gs_layo1     type lvc_s_layo,
       gs_f4        type lvc_s_f4,
       gt_f4        type lvc_t_f4,
       gs_sort      type lvc_s_sort,
       gt_sort      type lvc_t_sort,
       gs_sort_alv  type slis_sortinfo_alv,
       gt_sort_alv  type slis_t_sortinfo_alv.

data : ok_code      type sy-ucomm,
       save_ok_code type sy-ucomm.

* Define internal tables &sstructures for Possible Entry
data : gs_values type seahlpres,
       gt_fields type table of dfies with header line,
       gt_values type table of seahlpres with header line,
       gs_fields type dfies,
       ls_f4     type ddshretval,
       ls_modi   type lvc_s_modi.

* define fields and field-symbols for data-update
field-symbols : <f4tab> type lvc_t_modi.

* reference to custom container: neccessary to bind ALV Control
class cl_gui_resources definition load.
*CLASS LCL_EVENT_RECEIVER DEFINITION DEFERRED.

data : g_custom_container  type ref to cl_gui_custom_container,
       g_grid              type ref to cl_gui_alv_grid,
       g_custom_container1 type ref to cl_gui_custom_container,
       g_grid1             type ref to cl_gui_alv_grid.

data: gt_row   type lvc_t_row,
      gs_row   type lvc_s_row,
      gt_roid  type lvc_t_roid.

* define internal table for BDC
data: gt_bdc type table of bdcdata    with header line,
      gt_msg type table of bdcmsgcoll with header line,
      gs_opt like ctu_params.

* for possible entry
data: begin of dynpfields occurs 3.
        include structure dynpread.
data: end of dynpfields.

data: dyname         type progname,
      dynumb         type sychar04,
      exc_exctab     type slis_t_extab,
      popup_fieldcat type slis_t_fieldcat_alv,
      f              type slis_fieldcat_alv,
      selfield       type slis_selfield,
      exitfield,
      color_active(3)  value 'C50',
      tabix like sy-tabix.

* possible entry for reason code
types: begin of ty_ztcoum02,
         rgrp2 type zrgrp2,
         text  type zrtext,
       end of ty_ztcoum02.

types: begin of ty_rsn,
         kzust type kzust,
         text  type zrtext,
       end of ty_rsn.

data: gt_ztcoum02 type table of ty_ztcoum02 with header line,
      gt_rsn      type table of ty_rsn      with header line.

*&---------------------------------------------------------------------
*&      Form  FILL_FIELD_CATEGORY
*&---------------------------------------------------------------------
*       Build field catalog for ALV grid
*----------------------------------------------------------------------
*      -->P_FNAME Field name
*      -->P_TXT   Column heading
*      -->P_LEN   Column width
*      -->P_TYPE  Data type
*----------------------------------------------------------------------
form fill_field_category using p_pos   type lvc_colpos
                               p_fname type lvc_fname
                               p_txt   type lvc_txtcol
                               p_len   type lvc_outlen
                               p_type  type datatype_d.

  clear gs_fcat.

  gs_fcat-col_pos   = p_pos.     " Column position
  gs_fcat-fieldname = p_fname.   " Field name
  gs_fcat-coltext   = p_txt.     " Column heading
  gs_fcat-outputlen = p_len.     " Column width
  gs_fcat-datatype  = p_type.    " Data type

  append gs_fcat to gt_fcat.

endform.                    " FILL_FIELD_CATEGORY
*&---------------------------------------------------------------------
*&      Form  APPEND_EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------
*       Append excluding functions
*----------------------------------------------------------------------
*      -->P_TABNAME   Table name
*      -->P_VALUE     Excluding value
*----------------------------------------------------------------------
form append_exclude_functions tables p_table
                              using p_value.
  data ls_exclude type ui_func.

  ls_exclude = p_value.
  append ls_exclude to p_table.

endform.                    " APPEND_EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------
*&      Form  BUILD_FIELD_CATEGORY
*&---------------------------------------------------------------------
*       Build field catalog for ALV list
*----------------------------------------------------------------------
form build_field_category using p_fieldname type slis_fieldname
                                p_key       type c
                                p_text      type scrtext_l
                                p_len       type outputlen
                                p_type      type datatype_d.

  clear gs_fieldcat.

  gs_fieldcat-fieldname = p_fieldname.
  gs_fieldcat-key       = p_key.
  gs_fieldcat-seltext_l = p_text.
  gs_fieldcat-outputlen = p_len.
  gs_fieldcat-datatype  = p_type.

  append gs_fieldcat to gt_fieldcat.

endform.                    " BUILD_FIELD_CATEGORY
*&---------------------------------------------------------------------
*&      Form  SET_LAYOUT
*&---------------------------------------------------------------------
form set_layout using    p_edit_mode
                         p_box_fname
                changing cs_layo type slis_layout_alv.
  cs_layo-edit_mode         = p_edit_mode.
  cs_layo-numc_sum          = 'X'.
  cs_layo-box_fieldname     = p_box_fname.
  cs_layo-group_buttons     = 'X'.
  cs_layo-group_change_edit = 'X'.
  cs_layo-coltab_fieldname  = 'TABCOLOR'.
  cs_layo-colwidth_optimize = 'X'.

endform.                    " SET_LAYOUT
*&---------------------------------------------------------------------
*&      Form  SET_EVENTS
*&---------------------------------------------------------------------
form set_events changing ct_events type slis_t_event.
  data ls_event type slis_alv_event.

  call function 'REUSE_ALV_EVENTS_GET'
       exporting
            i_list_type = 0
       importing
            et_events   = ct_events.

  read table ct_events with key name =  slis_ev_top_of_page
                            into ls_event.
  if     sy-subrc = 0.
    move   gc_formname_top_of_page to ls_event-form.
    append ls_event to ct_events.
  endif.

endform.                    " SET_EVENTS
*&---------------------------------------------------------------------
*&      Form  COMMENT_BUILD
*&---------------------------------------------------------------------
form comment_build using lt_top_of_page type slis_t_listheader.
  data ls_line type slis_listheader.

  clear ls_line.
  ls_line-typ  = 'S'.
  ls_line-key  = 'Date:'.
  concatenate sy-datum+0(4) sy-datum+4(2) sy-datum+6(2)
         into ls_line-info separated by '.'.

  append ls_line to lt_top_of_page.

  ls_line-key  = 'User:'.
  ls_line-info = sy-uname.

  append ls_line to lt_top_of_page.

  ls_line-key  = ''.
  ls_line-info = ''.
  append ls_line to lt_top_of_page.

endform.                    " COMMENT_BUILD
*&---------------------------------------------------------------------
*&  FORM TOP_OF_LIST
*&---------------------------------------------------------------------
form top_of_list.
  new-line  no-scrolling.
  write : /1 'PGID: ', 8 sy-repid, 33 'Date:', sy-datum,
          62 'Time:', sy-uzeit, 92 sy-uname.

endform.                    "TOP_OF_LIST
*&---------------------------------------------------------------------
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------
form top_of_page.
  call function 'REUSE_ALV_COMMENTARY_WRITE'
       exporting
            it_list_commentary = gt_list_top_of_page.
endform.                    "TOP_OF_PAGE
*&---------------------------------------------------------------------
*&      Form  END_OF_PAGE
*&---------------------------------------------------------------------
form alv_event_end_of_page.
  data lv_page(10).

  new-line.
  uline.

  write: sy-pagno to lv_page,
         /(120) lv_page centered.

endform.                    "END_OF_PAGE
*&---------------------------------------------------------------------
*&      Form  REFRESH_FIELD
*&---------------------------------------------------------------------
*       Refresh for display
*----------------------------------------------------------------------
form refresh_field.
  call method g_grid->set_frontend_fieldcatalog
       exporting
         it_fieldcatalog = gt_fcat.

  call method g_grid->set_frontend_layout
       exporting
         is_layout = gs_layo.

  call method g_grid->refresh_table_display.
  call method cl_gui_cfw=>flush.

endform.                    " REFRESH_FIELD
*&---------------------------------------------------------------------
*&      Form  CREATE_OBJECT
*&---------------------------------------------------------------------
*       Create custom container control & instance
*----------------------------------------------------------------------
form create_object.
* create a custom container control for our alv control
  create object g_custom_container
      exporting
          container_name = container
        exceptions
            cntl_error = 1
            cntl_system_error = 2
            create_error = 3
            lifetime_error = 4
            lifetime_dynpro_dynpro_link = 5.
  if sy-subrc ne 0.
  endif.
* Create an Instance of ALV Control
  create object g_grid
        exporting i_parent = g_custom_container.

endform.                    " CREATE_OBJECT

* For BDC
*---------------------------------------------------------------------*
*       Form DYNPRO                                                   *
*---------------------------------------------------------------------*
form dynpro using p_dynbegin p_name p_value.
  clear gt_bdc.

  if p_dynbegin = 'X'.
    gt_bdc-program = p_name.
    gt_bdc-dynpro = p_value.
    gt_bdc-dynbegin = p_dynbegin.
  else.
    gt_bdc-fnam = p_name.
    gt_bdc-fval = p_value.
  endif.

  append gt_bdc.

endform.                    " DYNPRO
*---------------------------------------------------------------------*
*       Form GET_OPT
*---------------------------------------------------------------------*
form get_opt using p_mode.
  clear gs_opt.

  gs_opt-dismode  = p_mode.
  gs_opt-updmode  = 'X'.
  gs_opt-racommit = 'X'.
  gs_opt-nobinpt  = 'X'.

endform.                    " GET_OPT
*---------------------------------------------------------------------*
*       Form GET_MSG
*---------------------------------------------------------------------*
form get_msg changing p_msg.
  call function 'RKC_MSG_STRING'
       exporting
            id      = sy-msgid
            mtype   = sy-msgty
            number  = sy-msgno
            par1    = sy-msgv1
            par2    = sy-msgv2
            par3    = sy-msgv3
            par4    = sy-msgv4
       importing
            msg_lin = p_msg
       exceptions
            others  = 1.

endform.                    " RKC_MSG_STRING
*&---------------------------------------------------------------------
*&      Form  DATA_INPUT_ERROR
*&---------------------------------------------------------------------
*       Error Message Display
*----------------------------------------------------------------------
form data_input_error
        using rr_data_changed type ref to cl_alv_changed_data_protocol
              rs_mod_cells    type lvc_s_modi
              p_msgty         type symsgty
              p_msgv1         type symsgv
              p_fieldname     type lvc_fname.

* Error Message Display
  call method rr_data_changed->add_protocol_entry
   exporting
      i_msgid     = '0K'
      i_msgno     = '000'
      i_msgty     =  p_msgty
      i_msgv1     =  p_msgv1
      i_msgv2     = ' '
      i_msgv3     = ' '
      i_fieldname =  p_fieldname
      i_row_id    =  rs_mod_cells-row_id.

endform.                    " DATA_INPUT_ERROR

*&---------------------------------------------------------------------
*&      Form  POPUP_KALKA
*&---------------------------------------------------------------------
*       Possible Enter for Costing types
*----------------------------------------------------------------------
form popup_kalka using pa_kalka     type ck_kalka
                       p_fieldname type dynfnam.
  data: begin of lt_tck02 occurs 0,
           kalka type ck_kalka,
           txkla type ck_txkla,
         end of lt_tck02.

  data: begin of fields_tab occurs 1,
            kalka type ck_kalka,
            txkla type ck_txkla,
            color(3),
         end of fields_tab.

  clear: dynpfields, dyname, dynumb, exc_exctab, popup_fieldcat,
         f, selfield, exitfield, color_active, tabix, fields_tab.
  refresh: dynpfields, fields_tab.

  dynpfields-fieldname = p_fieldname.
  append dynpfields.

  dyname = sy-repid.
  dynumb = sy-dynnr.

  call function 'DYNP_VALUES_READ'
       exporting
            dyname             = dyname
            dynumb             = dynumb
            translate_to_upper = 'X'
       tables
            dynpfields         = dynpfields
       exceptions
            others             = 9.

  clear lt_tck02.
  refresh lt_tck02.

  select kalka txkla into table lt_tck02
    from tck02
   where spras = sy-langu.

  delete lt_tck02
    where not ( kalka+0(1) = 'U' or
                kalka+0(1) = 'M' or
                kalka+0(1) = 'B' or
                kalka+0(1) = 'R' ).

  sort lt_tck02 by kalka.

  f-reptext_ddic  = 'Costing Type'.
  f-fieldname = 'KALKA'.
  f-outputlen = 2.
  append f to popup_fieldcat.
  clear f.

  f-reptext_ddic = 'Desc.'.
  f-fieldname = 'TXKLA'.

  describe field fields_tab-txkla length f-outputlen.
  append f to popup_fieldcat.

* Excluding-Table
  append: '%SC ' to exc_exctab,       " Search
          '%SC+' to exc_exctab,       " Search+
          '&OUP' to exc_exctab,       " Sort Up
          '&ODN' to exc_exctab,       " Sort Dn
          '&ILT' to exc_exctab,       " Filter
          '&OL0' to exc_exctab.

* Popup
  tabix = sy-tabix.

  loop at lt_tck02.
    fields_tab-kalka = lt_tck02-kalka.
    fields_tab-txkla = lt_tck02-txkla.
    append fields_tab.
    clear fields_tab.
  endloop.

  call function 'REUSE_ALV_POPUP_TO_SELECT'
       exporting
            i_linemark_fieldname    = 'COLOR'
            i_tabname               = 'FIELDS_TAB'
            it_fieldcat             = popup_fieldcat
            i_callback_user_command = 'USER_COMMAND_POPUP_LIGHTS_N'
            i_callback_program      = dyname
            it_excluding            = exc_exctab
       importing
            es_selfield             = selfield
            e_exit                  = exitfield
       tables
            t_outtab                = fields_tab.

  read table fields_tab index tabix.
  clear fields_tab-color.
  modify fields_tab index tabix.

  if exitfield is initial.
    read table fields_tab index selfield-tabindex.
    pa_kalka = fields_tab-kalka.

    dynpfields-fieldname = p_fieldname.
    dynpfields-fieldvalue = fields_tab-kalka.
    append dynpfields.

    dyname = sy-repid.
    dynumb = sy-dynnr.

    call function 'DYNP_VALUES_UPDATE'
         exporting
              dyname     = dyname
              dynumb     = dynumb
         tables
              dynpfields = dynpfields.

  endif.

endform.                    " POPUP_KALKA

*&---------------------------------------------------------------------
*       Possible Enter for Reason
*----------------------------------------------------------------------
form popup_rsn using p_rsn       type kzust
                     p_fieldname type dynfnam.

  data: begin of fields_tab occurs 1,
           kzust type kzust,
           text  type zrtext,
           color(3),
         end of fields_tab.

  clear: dynpfields, dyname, dynumb, exc_exctab, popup_fieldcat,
         f, selfield, exitfield, color_active, tabix, fields_tab.
  refresh: dynpfields, fields_tab.

  dynpfields-fieldname = p_fieldname.
  append dynpfields.

  dyname = sy-repid.
  dynumb = sy-dynnr.

  call function 'DYNP_VALUES_READ'
       exporting
            dyname             = dyname
            dynumb             = dynumb
            translate_to_upper = 'X'
       tables
            dynpfields         = dynpfields
       exceptions
            others             = 9.

  perform get_reason_for_possible_entry.

  f-reptext_ddic  = 'Reason'.
  f-fieldname = 'KZUST'.
  f-outputlen = 3.
  append f to popup_fieldcat.
  clear f.

  f-reptext_ddic = 'Desc.'.
  f-fieldname = 'TEXT'.
  describe field fields_tab-text length f-outputlen.
  append f to popup_fieldcat.

* Excluding-Table
  append: '%SC ' to exc_exctab,       " Search
          '%SC+' to exc_exctab,       " Search+
          '&OUP' to exc_exctab,       " Sort Up
          '&ODN' to exc_exctab,       " Sort Dn
          '&ILT' to exc_exctab,       " Filter
          '&OL0' to exc_exctab.

* Popup
  tabix = sy-tabix.

  loop at gt_rsn.
    fields_tab-kzust = gt_rsn-kzust.
    fields_tab-text = gt_rsn-text.
    append fields_tab.
    clear fields_tab.
  endloop.

  call function 'REUSE_ALV_POPUP_TO_SELECT'
       exporting
            i_linemark_fieldname    = 'COLOR'
            i_tabname               = 'FIELDS_TAB'
            it_fieldcat             = popup_fieldcat
            i_callback_user_command = 'USER_COMMAND_POPUP_LIGHTS_N'
            i_callback_program      = dyname
            it_excluding            = exc_exctab
       importing
            es_selfield             = selfield
            e_exit                  = exitfield
       tables
            t_outtab                = fields_tab.

  read table fields_tab index tabix.
  clear fields_tab-color.
  modify fields_tab index tabix.

  if exitfield is initial.
    read table fields_tab index selfield-tabindex.
    p_rsn = fields_tab-kzust.

    dynpfields-fieldname = p_fieldname.
    dynpfields-fieldvalue = fields_tab-kzust.
    append dynpfields.

    dyname = sy-repid.
    dynumb = sy-dynnr.

    call function 'DYNP_VALUES_UPDATE'
         exporting
              dyname     = dyname
              dynumb     = dynumb
         tables
              dynpfields = dynpfields.

  endif.

endform.                    " POPUP_RSN

*&---------------------------------------------------------------------
*       Possible Enter for Reason in ALV Grid
*----------------------------------------------------------------------
form f4_reason using e_fieldname   type lvc_fname.
* Fill internal table for possible entry
  clear  : gt_values, gt_fields.
  refresh: gt_values, gt_fields.

  perform get_reason_for_possible_entry.

  loop at gt_rsn.
    gt_values-string = gt_rsn-kzust.
    append gt_values.

    gt_values-string = gt_rsn-text.
    append gt_values.
  endloop.

  clear gt_fields.
  refresh gt_fields.

  gt_fields-fieldname = e_fieldname.
  gt_fields-position  = 1.
  gt_fields-intlen    = 3.
  gt_fields-outputlen = 3.
  gt_fields-reptext   = 'Reason Code'.
  append gt_fields.
  clear gt_fields.

  gt_fields-fieldname = 'TEXT'.
  gt_fields-position  = 2.
  gt_fields-intlen    = 50.
  gt_fields-outputlen = 50.
  gt_fields-reptext = 'Desc.'.
  append gt_fields.
  clear gt_fields.

endform.                                                    " F4_REASON

*&---------------------------------------------------------------------*
*&      Form  GET_REASON_FOR_POSSIBLE_ENTRY
*&---------------------------------------------------------------------*
*       Get Posssible entry data for reason code
*----------------------------------------------------------------------*
form get_reason_for_possible_entry.
  types: begin of ty_t686d,
           kzust type kzust,
           vtext type vtext,
         end of ty_t686d.

  data lt_t686d type table of ty_t686d with header line.

  clear: gt_ztcoum02, lt_t686d, gt_rsn.
  refresh: gt_ztcoum02, lt_t686d, gt_rsn.

  select rgrp2 text into table gt_ztcoum02
    from ztcoum02
   where grp1 <> 'Z'.

  loop at gt_ztcoum02.
    gt_rsn-kzust = gt_ztcoum02-rgrp2.
    gt_rsn-text  = gt_ztcoum02-text.
    append gt_rsn.
    clear gt_rsn.
  endloop.

  select kzust vtext
    into table lt_t686d
    from t686d
   where spras = sy-langu
     and kzust like 'X%'.

  loop at lt_t686d.
    gt_rsn-kzust = lt_t686d-kzust.
    gt_rsn-text  = lt_t686d-vtext.

    append gt_rsn.
    clear gt_rsn.
  endloop.

  sort gt_rsn by kzust.

endform.                    " GET_REASON_FOR_POSSIBLE_ENTRY
*&---------------------------------------------------------------------*
*&      Form  ALV_VARIANT_F4
*&---------------------------------------------------------------------*
form alv_variant_f4 changing p_vari.
  data: rs_variant like disvariant,
        lv_nof4 type c.

  clear lv_nof4.
  loop at screen.
    if screen-name = 'PA_VARI'.
      if screen-input = 0.
        lv_nof4 = 'X'.
      endif.
    endif.
  endloop.

  clear rs_variant.
  rs_variant-report   = sy-repid.
  rs_variant-username = sy-uname.

  call function 'REUSE_ALV_VARIANT_F4'
       exporting
            is_variant = rs_variant
            i_save     = 'A'
       importing
            es_variant = rs_variant
       exceptions
            others     = 1.

  if sy-subrc = 0 and lv_nof4 = space.
    p_vari = rs_variant-variant.
  endif.

endform.                    " ALV_VARIANT_F4

*----------------------------------------------------------------------*
*   INCLUDE ZACOU113_TOP                                               *
*----------------------------------------------------------------------*

TABLES: T001W,           " Plants/Branches
        MARC,
        mara,
        MKPF,            " Header: Material Document
        ZTCOU102,        " [CO] Costing Result
        ZTCOU113.        " [CO]  Mass-Raw Mat.Cost fo New Material Log

TYPES: BEGIN OF TY_MAT.
        INCLUDE STRUCTURE ZTCOU113.
TYPES:  MAKTX TYPE MAKTX,
        kpein type kpein,
        PMEHT TYPE PMEHT,
        STAWN type STAWN, "duty code
        FRG   TYPE ZFRG1,
        OTH   TYPE ZOTH1,
        QTA   TYPE ZQTA,
      END OF TY_MAT.

TYPES: BEGIN OF TY_OUT.
         INCLUDE TYPE TY_MAT.
TYPES:   del,
         CELLTAB TYPE LVC_T_STYL,
      END OF TY_OUT.

TYPES: BEGIN OF TY_EINA,
         INFNR TYPE INFNR,          " Purchasing info record No.
         MATNR TYPE MATNR,          " Material number
         LIFNR TYPE LIFNR,          " Vendor
       END OF TY_EINA.

TYPES: BEGIN OF TY_A018,
         LIFNR TYPE LIFNR,          " Vendor
         MATNR TYPE MATNR,          " Material number
         DATBI TYPE KODATBI,        " Validity end date
         DATAB TYPE KODATAB,        " Validity start date
         KNUMH TYPE KNUMH,          " Condition record number
       END OF TY_A018.

DATA: GT_116      TYPE TABLE OF ztcou116 WITH HEADER LINE,
      GT_MAT      TYPE TABLE OF TY_MAT   WITH HEADER LINE,
      GT_OUT      TYPE TABLE OF TY_OUT   WITH HEADER LINE,
      GT_EINA     TYPE TABLE OF TY_EINA  WITH HEADER LINE,
      GT_A018     TYPE TABLE OF TY_A018  WITH HEADER LINE,
      GT_BAPIRET  TYPE TABLE OF BAPIRET2 WITH HEADER LINE,
      GV_CNT      TYPE I,
      GV_SCNT     TYPE I,
      GV_ECNT     TYPE I,
      GV_INDEX    TYPE I.

CONSTANTS: C_BUKRS TYPE BUKRS VALUE 'H201',
           C_STAT0    VALUE '0',        " Initial
           C_STAT1    VALUE '1',        " Change costing lot size
           C_STAT2    VALUE '2',        " Create Cost Estimate for Mat.
           C_STAT3    VALUE '3',        " Change material cost estimate
           C_STEP1(5) VALUE 'STEP1',
           C_STEP2(5) VALUE 'STEP2',
           C_STEP3(5) VALUE 'STEP3'.

RANGES R_WERKS FOR T001K-BWKEY.
RANGES: R_KSCHL FOR KONP-KSCHL,             " Condition type
        R_MATNR FOR MARA-MATNR.

*----------------------------------------------------------------------*
* Define local class
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER DEFINITION.
  PUBLIC SECTION.
    METHODS HANDLE_DATA_CHANGED
               FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
               IMPORTING ER_DATA_CHANGED.

ENDCLASS.                   " LCL_EVENT_RECEIVER Definition
*----------------------------------------------------------------------*
* Implementation local class
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.
* Change data
  METHOD HANDLE_DATA_CHANGED.
    PERFORM DATA_CHANGED USING ER_DATA_CHANGED.
  ENDMETHOD.                    " handle_data_changed

ENDCLASS.              " LCL_EVENT_RECEIVER Implementation

DATA G_EVENT_RECEIVER  TYPE REF TO LCL_EVENT_RECEIVER.


*----------------------------------------------------------------------
* Selection-Screen
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETER      P_KOKRS LIKE ZTCOU113-KOKRS OBLIGATORY
                                    MEMORY ID CAC
                                    MATCHCODE OBJECT FC_KOKRS.
SELECT-OPTIONS: S_MATNR FOR ZTCOU102-MATNR,
                S_WERKS FOR MARC-WERKS,
                S_MMSTA FOR MARC-MMSTA   NO intervals,
                S_MTART FOR MARA-MTART   NO intervals,
                S_BESKZ FOR MARC-BESKZ   NO intervals,
                S_DISPO FOR MARC-DISPO   NO intervals.
SELECTION-SCREEN END OF BLOCK B1.

PARAMETERS: P_RP   AS CHECKBOX,
            P_TEST AS CHECKBOX.

INCLUDE ZACOU113_F01.

*----------------------------------------------------------------------
* INITIALIZATION
*----------------------------------------------------------------------
INITIALIZATION.
  S_MMSTA-SIGN = 'I'.
  S_MMSTA-OPTION = 'EQ'.
  S_MMSTA-LOW = '  '. APPEND S_MMSTA.
  S_MMSTA-LOW = '11'. APPEND S_MMSTA.
  S_MMSTA-LOW = '12'. APPEND S_MMSTA.

  S_BESKZ-SIGN = 'I'.
  S_BESKZ-OPTION = 'EQ'.
  S_BESKZ-LOW = 'F'.     APPEND S_BESKZ.

  S_MTART-SIGN = 'I'.
  S_MTART-OPTION = 'EQ'.
  S_MTART-LOW = 'ROH'.   APPEND S_MTART.
  S_MTART-LOW = 'ROH1'.  APPEND S_MTART.

  S_DISPO-SIGN = 'I'.
  S_DISPO-OPTION = 'NE'.
  S_DISPO-LOW = 'M02'.   APPEND S_DISPO.

  R_KSCHL-SIGN = 'I'.
  R_KSCHL-OPTION = 'EQ'.
  R_KSCHL-LOW = 'PB00'.  APPEND R_KSCHL.
  R_KSCHL-LOW = 'ZTIR'.  APPEND R_KSCHL.

*----------------------------------------------------------------------
* Start of selection
*----------------------------------------------------------------------
START-OF-SELECTION.
* Reprocessing
  IF P_RP = 'X'.
    PERFORM GET_DATA.
    PERFORM GET_LOG.
    CALL SCREEN 100.

* Processing
  ELSE.
    PERFORM GET_DATA.
* Get Material Information
    perform get_gt_mat.

    PERFORM EXEC_MATL_COST.
    PERFORM DISP_RESULT.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS '100'.
  SET TITLEBAR '100'.

  PERFORM CREATE_ALV_CONTROL.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CASE OK_CODE.
*   Display Price History
    WHEN 'HIST'.
      PERFORM CALL_PRICE_HISTORY.

*   Reprocessing
    WHEN 'REPR'.
      PERFORM REPROCESSING.

    when 'SAVE'.
      perform manual_price.

    WHEN 'BACK' OR 'CANC' OR 'EXIT'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  DELETE_LOG
*&---------------------------------------------------------------------*
FORM DELETE_LOG.
  SELECT COUNT(*) INTO SY-INDEX
         FROM ZTCOU113 WHERE KOKRS = GT_MAT-KOKRS
                         AND KALKA = GT_MAT-KALKA
                         AND WERKS = GT_MAT-WERKS
                         AND MATNR = GT_MAT-MATNR.

  IF SY-SUBRC = 0.
    DELETE FROM ZTCOU113 WHERE KOKRS = GT_MAT-KOKRS
                           AND KALKA = GT_MAT-KALKA
                           AND WERKS = GT_MAT-WERKS
                           AND MATNR = GT_MAT-MATNR.
  ENDIF.

ENDFORM.                    " DELETE_LOG
*&---------------------------------------------------------------------*
*&      Form  manual_price
*&---------------------------------------------------------------------*
form manual_price.
  data: l_idx    like sy-tabix,
        lw_gt_mat like gt_mat.

  clear: gt_row[], gt_roid[],
         gv_cnt, gv_scnt, gv_ecnt.

* Get selected rows
  call method g_grid->get_selected_rows
              importing et_index_rows = gt_row
                        et_row_no = gt_roid.

  loop at gt_row into gs_row.
    read table gt_out index gs_row-index.
    check sy-subrc = 0.

    check gt_out-lifnr <> space and gt_out-kbetr > 0.

*  1. Process remaining steps
    clear gt_mat.
    move-corresponding gt_out to gt_mat.

    perform exec_mr21 using 'M'.

    gt_mat-aedat = sy-datum.
    gt_mat-aenam = sy-uname.

    read table gt_mat into lw_gt_mat
                      with key matnr = gt_out-matnr
                               werks = gt_out-werks
                      binary search.
    l_idx = sy-tabix.
    modify gt_mat index l_idx
           transporting lifnr kbetr stat eflag msg aedat aenam.

    if gt_mat-eflag = 'E'.
      gv_ecnt = gv_ecnt + 1.
      perform save_log.

      move-corresponding gt_mat to gt_out.
      append gt_out.
      clear gt_out.
    else.
      gv_scnt = gv_scnt + 1.
      perform insert_ztcou102.         " Update Table ZTCOU102
      perform delete_log.

      gt_out-del = 'X'.
      modify gt_out index gs_row-index transporting del.
    endif.

  endloop.

  delete gt_out where del = 'X'.

  perform refresh_field.

endform.                    " manual_price
