*&--------------------------------------------------------------------
*& Author                 : HS.Jeong (Tuned by IG.MOON)
*& Creation Date          : 06/10/2003
*& Specification By       : Andy Choi
*& Pattern                : Report 1-2
*& Development Request No : UD1K904466
*& Addl documentation     :
*& Description  : [FI-IM] IM Progress Report
*&
*& Modification Log
*& Date     Developer      Request ID      Description
*& 10.18.2006  Michelle J  Andy Choi       Add %Progress
*&--------------------------------------------------------------------
* Bug:
* - downpayment;
* - original budget; standard ledger, z-table.

REPORT zrfii07 MESSAGE-ID  zmfi.
TYPE-POOLS: vrm.
INCLUDE zacoui00.
INCLUDE <icon>.
INCLUDE <symbol>.
CONSTANTS c_f2code LIKE sy-ucomm VALUE '&ETA'.

*class cl_gui_resources definition load.
*data: gt_fieldcat type slis_t_fieldcat_alv,
*      gs_layout   type slis_layout_alv,
*      gt_sp_group type slis_t_sp_group_alv,
*      gt_events   type slis_t_event,
*      gt_sorts    type slis_t_sortinfo_alv with header line,
*      gs_prnt     type slis_print_alv.
*
*data: wa_repid like sy-repid,
*      wa_var_save(1) type c             value  'A',
*      wa_default(1)  type c,
*      wa_exit(1) type c,
*      wa_variant like disvariant,
*      wa_var like disvariant,
*      wa_alv_function_name(30) type c value 'REUSE_ALV_GRID_LIST',
*      wa_alv_get_info_name(40) type c.

*--- ALV
*data: w_fieldcat type slis_t_fieldcat_alv with header line,
*      w_eventcat type slis_t_event with header line,
*      w_selfield type slis_selfield,
*      w_sortcat  type slis_t_sortinfo_alv with header line,
*      w_col_pos  type i,
*      w_program  like sy-repid,
*      w_top_of_page type slis_t_listheader,
*      w_line1 type slis_listheader.

*----------------------------------------------------------------------
* define tables and internal structure
*----------------------------------------------------------------------
TABLES: aufk, impr, fmfctr, ripasw, codia, bkpf, ztfi_imfm, imak, ania,
        imavz, imav.

TYPES: BEGIN OF ty_itab,
         aufnr TYPE aufnr,
         posnr TYPE im_posnr,
         posid TYPE im_posid,
         prnam TYPE im_prnam,
         objnr TYPE im_objnr,
         kostl TYPE kostl,
       END OF ty_itab.

TYPES: BEGIN OF ty_imak,
         posid TYPE ima_posid,
       END OF ty_imak.

TYPES: BEGIN OF ty_status,
         objnr TYPE j_objnr,
         stat TYPE j_status,
       END OF ty_status.

DATA: BEGIN OF it_impr OCCURS 0,
         posid LIKE impr-posid,
         objnr LIKE impr-objnr,
         gjahr LIKE impr-gjahr,
         kostl TYPE kostl,
         arno  LIKE imak-posid,
         prnam LIKE impr-prnam,
         impr_objnr LIKE impr-objnr,
         ergso LIKE impr-ergso,
         usr03 LIKE imak-usr03,
         usr09 LIKE imak-usr09,
         bukrs LIKE impr-bukrs,
         kokrs LIKE impr-kokrs,
         posnr LIKE imak-posnr,
      END OF it_impr.

DATA: BEGIN OF it_bpge OCCURS 0,
        objnr LIKE impr-objnr,
        vorga TYPE bp_vorgang,
        wlges TYPE bp_wgl,
      END OF it_bpge.

DATA: gt_status TYPE TABLE OF ty_status WITH HEADER LINE.

*--summary by activity
DATA : BEGIN OF it_sum OCCURS 0,
        posid LIKE impr-posid,
        gjahr LIKE impr-gjahr,
        prnam LIKE ztfi_imfm-prnam,
        objnr LIKE impr-objnr,
        kostl TYPE kostl,
        gubun LIKE ztfi_imfm-gubun,
        tot   LIKE ztfi_imfm-tot,
        usr03 LIKE imak-usr03,
        usr09 LIKE imak-usr09,
        bukrs LIKE impr-bukrs,
        kokrs LIKE impr-kokrs,
        posnr LIKE imak-posnr,
       impr_objnr  TYPE j_objnr,
        stat,     " Status: 1 Created 2 Rel 3 Locked
       END OF it_sum.

DATA: it_out       TYPE TABLE OF impr WITH HEADER LINE,

      it_imfm      TYPE TABLE OF ztfi_imfm WITH HEADER LINE,

      it_aufk      TYPE TABLE OF aufk WITH HEADER LINE,
      it_io_budget TYPE TABLE OF zfi_io_budget WITH HEADER LINE,
      it_io_actual TYPE TABLE OF zfi_io_actual WITH HEADER LINE,
      it_budget    TYPE TABLE OF zfi_pi_budget WITH HEADER LINE,
      it_actual    TYPE TABLE OF zfi_pi_actual_act WITH HEADER LINE,
      it_imak      TYPE TABLE OF ty_imak WITH HEADER LINE,
      itab         TYPE TABLE OF ty_itab WITH HEADER LINE.

* by ig.moon {
* Type for ALV
TYPES: BEGIN OF ty_out.
INCLUDE  STRUCTURE zsfi_imfm.
TYPES :   anlkl  TYPE anlkl,
          aktiv  TYPE am_aktivp,
          usr03  TYPE ima_usr03,
          usr09  TYPE ima_usr09,
          bukrs  TYPE bukrs,
          kokrs  TYPE kokrs,
          posnr  TYPE ima_posnr,
         $objnr  TYPE j_objnr,
     impr_objnr  TYPE j_objnr.
TYPES   celltab  TYPE lvc_t_styl.
TYPES   tabcolor TYPE slis_t_specialcol_alv.
TYPES: END OF ty_out.

DATA   gt_out     TYPE TABLE OF ty_out     WITH HEADER LINE.
DATA  $gt_out     TYPE TABLE OF ty_out     WITH HEADER LINE.

*----for combox
DATA: it_val TYPE vrm_values,
      w_line LIKE LINE OF it_val.
*---WORK AREA
DATA : wa_t_cnt      TYPE i,
*      WA_T_CNT1     TYPE I,
       wa_prnam      LIKE  ztfi_imfm-prnam,
       wa_before     LIKE  imzo-gjahr,
       wa_last       LIKE  imzo-gjahr,
       wa_year       LIKE  imzo-gjahr,
       wa_year1      LIKE  imzo-gjahr,
       wa_year2      LIKE  imzo-gjahr,
       wa_year3      LIKE  imzo-gjahr,
       wa_year4      LIKE  imzo-gjahr,
       wa_year5      LIKE  imzo-gjahr,
       wa_year6      LIKE  imzo-gjahr,
       wa_after      LIKE  imzo-gjahr,
       wa_before_txt(10),
       wa_after_txt(10),
       wa_org_amt    LIKE  ztfi_imfm-tot,
       wa_sup_amt    LIKE  ztfi_imfm-tot,
       wa_ret_amt    LIKE  ztfi_imfm-tot,
       wa_cur_amt    LIKE  ztfi_imfm-tot,
       wa_before_amt LIKE  cosp-wtg001,
       wa_after_amt  LIKE  cosp-wtg001,
*---Currenty
        wa_n_tot     LIKE  cosp-wtg001,
        wa_n_before  LIKE  cosp-wtg001,
        wa_n_last    LIKE  cosp-wtg001,
        wa_n_year    LIKE  cosp-wtg001,
        wa_n_year1   LIKE  cosp-wtg001,
        wa_n_year2   LIKE  cosp-wtg001,
        wa_n_year3   LIKE  cosp-wtg001,
        wa_n_year4   LIKE  cosp-wtg001,
        wa_n_year5   LIKE  cosp-wtg001,
        wa_n_after   LIKE  cosp-wtg001.
*---Actual
DATA :  wa_a_tot     LIKE  cosp-wtg001,
        wa_a_before  LIKE  cosp-wtg001,
        wa_a_last    LIKE  cosp-wtg001,
        wa_a_year    LIKE  cosp-wtg001,
        wa_a_year1   LIKE  cosp-wtg001,
        wa_a_year2   LIKE  cosp-wtg001,
        wa_a_year3   LIKE  cosp-wtg001,
        wa_a_year4   LIKE  cosp-wtg001,
        wa_a_year5   LIKE  cosp-wtg001,
        wa_a_after   LIKE  cosp-wtg001.
*---Commitment
DATA :  wa_c_tot     LIKE  cosp-wtg001,
        wa_c_before  LIKE  cosp-wtg001,
        wa_c_last    LIKE  cosp-wtg001,
        wa_c_year    LIKE  cosp-wtg001,
        wa_c_year1   LIKE  cosp-wtg001,
        wa_c_year2   LIKE  cosp-wtg001,
        wa_c_year3   LIKE  cosp-wtg001,
        wa_c_year4   LIKE  cosp-wtg001,
        wa_c_year5   LIKE  cosp-wtg001,
        wa_c_after   LIKE  cosp-wtg001.
*---Downpayment
DATA :  wa_d_tot     LIKE  cosp-wtg001,
        wa_d_before  LIKE  cosp-wtg001,
        wa_d_last    LIKE  cosp-wtg001,
        wa_d_year    LIKE  cosp-wtg001,
        wa_d_year1   LIKE  cosp-wtg001,
        wa_d_year2   LIKE  cosp-wtg001,
        wa_d_year3   LIKE  cosp-wtg001,
        wa_d_year4   LIKE  cosp-wtg001,
        wa_d_year5   LIKE  cosp-wtg001,
        wa_d_after   LIKE  cosp-wtg001.

* ALV

*----------------------------------------------------------------------*
* Define local class
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.

    TYPES: BEGIN OF zsfi_imfm_k,
              posid TYPE im_posid,
              kostl TYPE kostl,
           END OF zsfi_imfm_k.

    TYPES: zsfi_imfm_key   TYPE STANDARD TABLE OF zsfi_imfm_k,
           zsfi_imfm_table TYPE STANDARD TABLE OF zsfi_imfm.

    METHODS:
      handle_data_changed
         FOR EVENT data_changed OF cl_gui_alv_grid
             IMPORTING er_data_changed,
                       get_deleted_rows
             EXPORTING
                       deleted_rows TYPE zsfi_imfm_table,

      refresh_delta_tables,

      handle_double_click FOR EVENT double_click OF cl_gui_alv_grid
              IMPORTING e_row
                        e_column
                        es_row_no.

  PRIVATE SECTION.
    DATA deleted_rows TYPE STANDARD TABLE OF zsfi_imfm.

* This flag is set if any error occured in one of the
* following methods:
    DATA: error_in_data TYPE c.
    METHODS:
      update_delta_tables
         IMPORTING
            pr_data_changed TYPE REF TO cl_alv_changed_data_protocol.

ENDCLASS.                   " LCL_EVENT_RECEIVER Definition

*----------------------------------------------------------------------*
* Implementation local class
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

* Setting for Change data
  METHOD handle_data_changed.

* remember deleted lines for saving
    CALL METHOD update_delta_tables( er_data_changed ).

    PERFORM data_changed USING er_data_changed.
  ENDMETHOD.                    " handle_data_changed

  METHOD get_deleted_rows.
    deleted_rows = me->deleted_rows.
  ENDMETHOD.

  METHOD refresh_delta_tables.
    CLEAR me->deleted_rows[].
  ENDMETHOD.

  METHOD update_delta_tables.
    DATA: l_del_row TYPE lvc_s_moce,
          ls_zsfi_imfm TYPE zsfi_imfm,
          ls_outtab LIKE LINE OF gt_out.

    LOOP AT pr_data_changed->mt_deleted_rows INTO l_del_row.
      READ TABLE gt_out INTO ls_outtab INDEX l_del_row-row_id.
      IF sy-subrc NE 0.
        MESSAGE i000(0k) WITH text-e01. "Internal error
      ELSE.
        MOVE-CORRESPONDING ls_outtab TO ls_zsfi_imfm.
        APPEND ls_zsfi_imfm TO deleted_rows.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

* Double Click
  METHOD handle_double_click.
    PERFORM double_click USING e_row
                               e_column
                               es_row_no.
  ENDMETHOD.                    " handle_double_click

ENDCLASS.                   " LCL_EVENT_RECEIVER Implementation

DATA g_event_receiver  TYPE REF TO lcl_event_receiver.
DATA: g_error(1),
      g_repid  LIKE sy-repid,
      gv_index LIKE sy-tabix.

************************************************************************
DATA  : flag_data_changed,
        info(80).
DATA: BEGIN OF ftab OCCURS 10,
        fcode(6),
      END OF ftab.
CONSTANTS:  false VALUE ' ',
            true  VALUE 'X'.

*----------------------------------------------------------------------*
*   Macro
*----------------------------------------------------------------------*
DEFINE __focus.
  call method cl_gui_control=>set_focus
      exporting
        control = &1 .
END-OF-DEFINITION.
DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.

DEFINE __process.
  perform show_progress using &1 &2.
END-OF-DEFINITION.

DEFINE __message.
  call function 'POPUP_TO_INFORM'
       exporting
            titel = &1
            txt1  = &2
            txt2  = sy-subrc.
END-OF-DEFINITION.

DATA:
    lt_ania  LIKE rania OCCURS 0 WITH HEADER LINE,
    lt_anib  LIKE STANDARD TABLE OF ranib
                  INITIAL SIZE 10 WITH HEADER LINE,
    lt_anib1 LIKE STANDARD TABLE OF ranib
                  INITIAL SIZE 10 WITH HEADER LINE.

DATA: i_imakpa LIKE imakpa OCCURS 0 WITH HEADER LINE.
DATA: i_imak   LIKE imak OCCURS 0 WITH HEADER LINE.


*----------------------------------------------------------------------
* SELECTION-SCREEN
*----------------------------------------------------------------------
* General selections
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE text-010.
* ...Position ID
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (15) c_pi.
PARAMETER r_1 RADIOBUTTON GROUP r1.
SELECTION-SCREEN POSITION 30.
SELECT-OPTIONS s_posid   FOR   imak-posid. "impr-posid.
SELECTION-SCREEN END OF LINE.

* ...Internal Order
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (15) c_or.
PARAMETER r_2 RADIOBUTTON GROUP r1.
SELECTION-SCREEN POSITION 30.
SELECT-OPTIONS s_aufnr  FOR   aufk-aufnr.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b0.

* Run Parameter
SELECTION-SCREEN BEGIN OF BLOCK s1 WITH FRAME TITLE c010.
PARAMETER p_ayear LIKE impr-gjahr   MEMORY ID gjr OBLIGATORY
                                      DEFAULT sy-datum+0(4).
PARAMETER p_arver   LIKE imavz-versi DEFAULT 'IM'.
SELECT-OPTIONS s_gjahr FOR impr-gjahr .


SELECTION-SCREEN END OF BLOCK s1.

* Select option
SELECTION-SCREEN BEGIN OF BLOCK s3 WITH FRAME TITLE c030.

SELECT-OPTIONS s_prnam FOR impr-prnam MEMORY ID imt."bligatory.


PARAMETERS: p_mon(2)  TYPE n NO-DISPLAY,
            p_auth(1) TYPE c DEFAULT 'X' NO-DISPLAY.

SELECT-OPTIONS: s_kostl    FOR impr-kostl,
                s_ergso    FOR impr-ergso.
* AR Selection
SELECT-OPTIONS: s_vkokrs  FOR imak-vkokrs,
                s_usr03   FOR imak-usr03,
                s_agjahr  FOR imak-gjahr,
                s_ivart   FOR imak-ivart.
SELECTION-SCREEN END OF BLOCK s3.

* Depreciation Selection
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-003.
PARAMETERS     : p_io  RADIOBUTTON GROUP selm,
                 p_pi  RADIOBUTTON GROUP selm,
                 p_ar  RADIOBUTTON GROUP selm DEFAULT 'X'.
PARAMETERS: p_darver LIKE imavz-versi DEFAULT 'ID'.

PARAMETERS: p_yearp LIKE impr-gjahr . " Plan Year

*            p_s as checkbox.           " Simulation
SELECTION-SCREEN END OF BLOCK b4.

* Status
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-002.
PARAMETERS: p_crtd NO-DISPLAY,
            p_rel  NO-DISPLAY,
            p_lkd  NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK b3.

* Select layout
SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE text-004.
PARAMETER p_vari TYPE slis_vari.
SELECTION-SCREEN END OF BLOCK b5.

PARAMETER p_disp DEFAULT 'E'.

*----------------------------------------------------------------------*
*   at selection screen                                                *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            id     = 'P_ACT'
            values = it_val.
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM alv_variant_f4 CHANGING p_vari.

*----------------------------------------------------------------------
* INITIALIZATION
*----------------------------------------------------------------------
INITIALIZATION.
** ==> Change Variant saving type
*  wa_var_save = 'A'.
** ==> Change first mode   GRID or LIST
*  wa_alv_function_name = 'REUSE_ALV_GRID_DISPLAY'.
**  wa_alv_function_name = 'REUSE_ALV_LIST_DISPLAY'.
*  refresh : gt_fieldcat.
*  clear   : gs_layout.


*--title set
  c010 = 'Source Year / Plan Budget '.
*  c020 = 'Select option'.
  c030 = 'Select option'.

*  wa_repid = sy-repid.
  c_pi = 'Position ID'.
  c_or = 'Internal Order'.

* for combo box
*---------------------------------------------------------------------
*    M   A   I   N
*---------------------------------------------------------------------
START-OF-SELECTION.

  PERFORM select_data.

  IF gt_out[] IS INITIAL.
    MESSAGE s000(zmfi) WITH 'No found data '.
    EXIT.
  ENDIF.

  PERFORM make_out.

  CALL SCREEN 100.

END-OF-SELECTION.

*&---------------------------------------------------------------------
*&      Form  select_data
*&---------------------------------------------------------------------
FORM select_data.
*  refresh : it_imfm, it_sum, gt_out.
*  clear   : it_imfm, it_sum, gt_out.

  __cls : it_bpge, it_sum, gt_out.

* Position ID
  IF r_1 = 'X'.
    PERFORM cbo_pi_process.
* Order
  ELSE.
    PERFORM order_rpocess.
  ENDIF.


  PERFORM get_opt USING p_disp.

ENDFORM.                    " select_data

**---------------------------------------------------------------------*
**  FORM alv_event_pf_status_set
**---------------------------------------------------------------------*
*form alv_event_pf_status_set using rt_extab type slis_t_extab.
*  if wa_alv_function_name = 'REUSE_ALV_GRID_DISPLAY'.
*    set pf-status 'STANDARD_GRID'.
*  else.
*    set pf-status 'STANDARD' excluding rt_extab.
*  endif.
*  set titlebar  'STANDARD'.
*
*endform.                    "alv_event_pf_status_set
**---------------------------------------------------------------------*
**  FORM alv_event_user_command
**---------------------------------------------------------------------*
*form alv_event_user_command using r_ucomm like sy-ucomm
*                                  rs_selfield type slis_selfield.
*
*  case r_ucomm.
**   ---------------------------------- processing on double click.
*    when '&IC1'.
*      read table gt_out index rs_selfield-tabindex.
*      case rs_selfield-fieldname.
*        when 'POSID'.
*          set parameter id 'IMT' field s_prnam-low.
*          set parameter id 'IMP' field gt_out-posid.
*          set parameter id 'GJR' field p_ayear.
*          call transaction 'ZIMR' and skip first screen.
*      endcase.
**   ---------------------------------- switching view type grid or list
*    when 'LIST' or 'GRID'.
*      perform switch_list_or_grid using r_ucomm.
*
*    when 'PRE' or 'NEXT'.
*      perform change_status using r_ucomm.        " Change status
*      rs_selfield-refresh = 'X'.
*
*    when 'DPCR'.
*      perform create_dpcr.             " Create depreciation data
*      rs_selfield-refresh = 'X'.
*
*    when 'CAL1' or 'CAL2' or 'CAL3'.
*      perform cal_dpcr_amt using r_ucomm.
*      rs_selfield-refresh = 'X'.
*
*  endcase.
*
*  check r_ucomm eq 'LIST' or
*        r_ucomm eq 'GRID'.
*
*  rs_selfield-exit = 'X'.
*
*endform.                    "alv_event_user_command
**&---------------------------------------------------------------------
**&      Form  set_variant
**&---------------------------------------------------------------------
*form set_variant changing cs_vari type disvariant.
*  check p_layout ne space.
*
*  cs_vari-report      = sy-repid.
*  cs_vari-handle      = space.
*  cs_vari-log_group   = space.
*  cs_vari-username    = space.
*  cs_vari-variant     = p_layout.
*  cs_vari-text        = space.
*  cs_vari-dependvars  = space.
*
*endform.                    " set_variant
**&---------------------------------------------------------------------
**&      Form  set_events
**&---------------------------------------------------------------------
*form set_events changing ct_events type slis_t_event.
*
*  field-symbols: <ls_event> type slis_alv_event.
*
*  data: l_event type lvc_fname.
*
*  call function 'REUSE_ALV_EVENTS_GET'
*       exporting
*            i_list_type     = 0
*       importing
*            et_events       = ct_events
*       exceptions
*            list_type_wrong = 1
*            others          = 2.
*
*  if sy-subrc <> 0.
*    message id sy-msgid type sy-msgty number sy-msgno
*            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  else.
*    delete ct_events where name ne 'END_OF_PAGE'
*                       and name ne 'TOP_OF_PAGE'
*                       and name ne 'TOP_OF_LIST'
*                       and name ne 'END_OF_LIST'.
*    loop at ct_events assigning <ls_event>.
*      concatenate 'ALV_EVENT_'
*                  <ls_event>-name
*                  into <ls_event>-form.
*    endloop.
*  endif.
*
*endform.                    " f01_set_evts
**&---------------------------------------------------------------------
**&      Form  set_layout
**&---------------------------------------------------------------------
*form set_layout changing cs_layo type slis_layout_alv.
*  cs_layo-numc_sum               = 'X'.
*  cs_layo-group_buttons          = 'X'.
*  cs_layo-group_change_edit      = 'X'.
*  cs_layo-detail_popup           = 'X'.
*  cs_layo-coltab_fieldname       = 'TABCOLOR'.
*  cs_layo-list_append            = space.
*  cs_layo-colwidth_optimize      = 'X'.
*  cs_layo-box_fieldname          = 'CHK'.
*
*endform.                    " set_layout
**&---------------------------------------------------------------------
**
**&      Form  dispaly_heager
**----------------------------------------------------------------------
**
*form display_header.
*  call function 'REUSE_ALV_COMMENTARY_WRITE'
*       exporting
*            it_list_commentary = w_top_of_page.
*endform.                    " top_of_page
**&---------------------------------------------------------------------
**&      Form  switch_list_or_grid
**&---------------------------------------------------------------------
*form switch_list_or_grid using r_ucomm.
*  data: ls_vari      type disvariant,
*        ls_slis_layo type slis_layout_alv,
*        lt_slis_fcat type slis_t_fieldcat_alv,
*        lt_slis_sort type slis_t_sortinfo_alv,
*        lt_slis_filt type slis_t_filter_alv,
*        ls_slis_prnt type slis_print_alv.
*
*  if r_ucomm = 'LIST' and
*     wa_alv_function_name = 'REUSE_ALV_LIST_DISPLY'.
*    exit.
*  endif.
*
*  if r_ucomm = 'GRID' and
*     wa_alv_function_name = 'REUSE_ALV_GRID_DISPLAY'.
*    exit.
*  endif.
*
*  case wa_alv_function_name.
*    when 'REUSE_ALV_LIST_DISPLAY'.
*      wa_alv_get_info_name = 'REUSE_ALV_LIST_LAYOUT_INFO_GET'.
*    when 'REUSE_ALV_GRID_DISPLAY'.
*      wa_alv_get_info_name = 'REUSE_ALV_GRID_LAYOUT_INFO_GET'.
*  endcase.
*
*  call function wa_alv_get_info_name
*       importing
*            es_layout     = ls_slis_layo
*            et_fieldcat   = lt_slis_fcat
*            et_sort       = lt_slis_sort
*            et_filter     = lt_slis_filt
*            es_variant    = ls_vari
*       exceptions
*            no_infos      = 1
*            program_error = 2
*            others        = 3.
*  if sy-subrc <> 0.
*    message id sy-msgid type sy-msgty number sy-msgno
*            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  endif.
*
*  if r_ucomm = 'LIST'.
*    wa_alv_function_name = 'REUSE_ALV_LIST_DISPLAY'.
*    call function wa_alv_function_name
*         exporting
*              i_callback_program       = wa_repid
*              i_callback_pf_status_set = 'ALV_EVENT_PF_STATUS_SET'
*              i_callback_user_command  = 'ALV_EVENT_USER_COMMAND'
*              is_layout                = ls_slis_layo
*              it_fieldcat              = lt_slis_fcat
*              it_sort                  = lt_slis_sort
*              it_filter                = lt_slis_filt
*              i_default                = ' '  "gs_test-vari_default
*              i_save                   = wa_var_save
*              is_variant               = ls_vari
*              is_print                 = ls_slis_prnt
*              it_events                = gt_events[]
*         tables
*              t_outtab                 = gt_out
*         exceptions
*              program_error            = 1
*              others                   = 2.
*  endif.
*
*  if r_ucomm = 'GRID'.
*    wa_alv_function_name = 'REUSE_ALV_GRID_DISPLAY'.
*    call function wa_alv_function_name
*         exporting
*              i_callback_program       = wa_repid
*              i_callback_pf_status_set = 'ALV_EVENT_PF_STATUS_SET'
*              i_callback_user_command  = 'ALV_EVENT_USER_COMMAND'
*              is_layout                = ls_slis_layo
*              it_fieldcat              = lt_slis_fcat
*              it_sort                  = lt_slis_sort
*              it_filter                = lt_slis_filt
*              i_default                = ' '  "gs_test-vari_default
*              i_save                   = wa_var_save
*              is_variant               = ls_vari
*              is_print                 = ls_slis_prnt
*         tables
*              t_outtab                 = gt_out
*         exceptions
*              program_error            = 1
*              others                   = 2.
*  endif.
*
*  if sy-subrc <> 0.
*    message id sy-msgid type sy-msgty number sy-msgno
*            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  endif.
*
*endform.                    " switch_list_or_grid
**&--------------------------------------------------------------------
**&      Form  make_field_category
**&--------------------------------------------------------------------
*form build_field_category using   p_fieldname       " field name
*                                  p_title           " field title
*                                  p_outputlen       " length
*                                  p_key
*                                  p_just.
*
*  data ls_fieldcat type slis_fieldcat_alv.
*
*  clear ls_fieldcat.
*  ls_fieldcat-fieldname = p_fieldname.
*  ls_fieldcat-seltext_l = p_title.
*  ls_fieldcat-outputlen = p_outputlen.
*  ls_fieldcat-key       = p_key.
*  ls_fieldcat-just      = p_just.
*
*  if p_fieldname = 'ICON'.
*    ls_fieldcat-icon = 'X'.
*  endif.
*
*  append ls_fieldcat to gt_fieldcat.
*
*endform.                    " fill_field_category
**&---------------------------------------------------------------------
**
**&      Form  f4_variant
**&---------------------------------------------------------------------
**
*form f4_variant changing c_variant type disvariant-variant.
*  data: ls_variant type disvariant,
*        l_exit     type char1.
*
*  ls_variant-report = sy-repid.
*
*  call function 'REUSE_ALV_VARIANT_F4'
*       exporting
*            is_variant = ls_variant
*            i_save     = 'A'
*       importing
*            e_exit     = l_exit
*            es_variant = ls_variant
*       exceptions
*            not_found  = 2.
*
*  if sy-subrc = 2.
*    message id sy-msgid type 'S' number sy-msgno
*            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  else.
*    if l_exit eq space.
*      c_variant = ls_variant-variant.
*    endif.
*  endif.
*
*endform.                    " f4_variant
**&---------------------------------------------------------------------
**
**&      Form  build_sort_table
**&---------------------------------------------------------------------
**
*form build_sort_table using  p_spos
*                             p_fieldname
*                             p_up
*                             p_subtot
*                             p_group.
*  data ls_sort type slis_sortinfo_alv.
*
*  ls_sort-spos      = p_spos.
*  ls_sort-fieldname = p_fieldname.
*  ls_sort-up        = p_up.
*  ls_sort-subtot    = p_subtot.
*  ls_sort-group     = p_group.
*  append ls_sort to gt_sorts.
*
*endform.                    " build_sort_table
**&---------------------------------------------------------------------
**
**&      Form  set_line_color
**&---------------------------------------------------------------------
**
*form set_line_color using    p_color.
*  data: ls_fieldcat   type slis_fieldcat_alv,
*        lt_color      type slis_t_specialcol_alv,
*        ls_color      type slis_specialcol_alv.
*
*  refresh lt_color.
*  clear   lt_color.
*
*  loop at gt_fieldcat into ls_fieldcat.
*    ls_color-fieldname = ls_fieldcat-fieldname.
*    ls_color-color-col = p_color.
*    ls_color-color-int = cl_gui_resources=>list_intensified.
*    ls_color-color-inv = 0.
*    ls_color-nokeycol  = 'X'.
*    append ls_color to lt_color.
*    gt_out-tabcolor = lt_color.
*  endloop.
*
*endform.                    " set_line_color
**&---------------------------------------------------------------------
**
**&      Form  build_field_category1
**&---------------------------------------------------------------------
**
*form build_field_category1 using
*                                  p_fieldname       " field name
*                                  p_title           " field title
*                                  p_outputlen       " length
*                                  p_key             "
*                                  p_just            "
*                                  p_noout           "
*                                  p_edit            "
*                                  p_cfield          " currency field
*nam
*                                  p_qfield          " quantity field
*nam
*                                  .
*
*  data ls_fieldcat type slis_fieldcat_alv.
*  clear ls_fieldcat.
*
*  ls_fieldcat-fieldname = p_fieldname.
*  ls_fieldcat-seltext_l = p_title.
*  ls_fieldcat-outputlen = p_outputlen.
*  ls_fieldcat-key       = p_key.
*  ls_fieldcat-just      = p_just.
*  ls_fieldcat-edit      = p_edit.
*  ls_fieldcat-no_out     = p_noout.
*  ls_fieldcat-cfieldname = p_cfield.
*  ls_fieldcat-qfieldname = p_qfield.
*  append ls_fieldcat to gt_fieldcat.
*endform.                    " build_field_category1
**&---------------------------------------------------------------------
**
**&      Form  comment_build
**&---------------------------------------------------------------------
**
*form comment_build using  lt_top_of_page type slis_t_listheader.
*  data: ls_line type slis_listheader,
*          l_manager(50),
*          l_date(50),
*          l_list(50),
*          l_dsnam like t024d-dsnam,
*          l_h_dsnam like t024d-dsnam,
*          l_ldate(10),
*          l_hdate(10).
**-------------- HEADER
*  clear ls_line.
*  ls_line-typ  = 'H'.
*  ls_line-info = text-h01.     "HEADER TITLE (H001)
*  append ls_line to lt_top_of_page.
*
*  ls_line-typ  = 'S'.
*  ls_line-key  = 'Investment program : '.
*  ls_line-info = s_prnam-low.
*  append ls_line to lt_top_of_page.
**--
*  ls_line-typ  = 'S'.
*  ls_line-key  = 'Approval Year : '.
*  ls_line-info = p_ayear.
*  append ls_line to lt_top_of_page.
*  ls_line-typ  = 'S'.
*  ls_line-key  = 'Fiscal Year : '.
**  ls_line-info = S_GJAHR-LOW.
*  concatenate   s_gjahr-low  ' ~'  s_gjahr-high into l_list.
*  ls_line-info = l_list.
*  append ls_line to lt_top_of_page.
**--
*  ls_line-typ  = 'S'.
*  ls_line-key  = 'Position ID : '.
*  concatenate   s_posid-low  ' ~'  s_posid-high into l_list.
*  ls_line-info = l_list.
*  append ls_line to lt_top_of_page.
**
*endform.                    " comment_build
**&---------------------------------------------------------------------
**
**&      Form  set_build_event
**&---------------------------------------------------------------------
**
*form set_build_event.
*  w_eventcat-name = 'TOP_OF_PAGE'.
*  w_eventcat-form = 'DISPLAY_HEADER'.
*  append w_eventcat.
*
*endform.                    " set_build_event
*&---------------------------------------------------------------------*
*&      Form  get_pi_plan
*&---------------------------------------------------------------------*
FORM get_pi_plan USING    u_posid
                          u_ayear
                          u_prnam.
  REFRESH it_budget.
  CLEAR   it_budget.

  CALL FUNCTION 'Z_FFI_GET_PI_BUDGET'
       EXPORTING
            posid = u_posid
            prnam = u_prnam
            gjahr = u_ayear
       TABLES
            out   = it_budget.
*Issue Number : FI-20041118-007, Requested by Robin Simmons
*Changed on 2004/12/10, by WSKIM
*---Start
  IF  s_gjahr-low = space.
    READ TABLE it_budget WITH KEY posid = u_posid
                                 gjahr = '1111'.
    IF sy-subrc = 0.
      MOVE it_budget-plan TO gt_out-zplan.
    ENDIF.
  ELSE.
    READ TABLE it_budget WITH KEY posid = u_posid
                                  gjahr = s_gjahr-low.
    IF sy-subrc = 0.
      MOVE it_budget-plan TO gt_out-zplan.
    ENDIF.
  ENDIF.
*---End
ENDFORM.                    " get_pi_plan
*&---------------------------------------------------------------------*
*&      Form  get_pi_ACTUAL
*&---------------------------------------------------------------------*
FORM get_pi_actual USING    u_posid
                            u_ayear
                            u_prnam.
  DATA: l_name(20),
        l_tot TYPE wtgxxx,
        n(2) TYPE n.

  FIELD-SYMBOLS <fs> TYPE ANY.

  REFRESH it_actual.
  CLEAR   it_actual.

  CALL FUNCTION 'Z_FFI_GET_PI_ACTUAL_ACT'
       EXPORTING
            posid = u_posid
            gjahr = u_ayear
            prnam = u_prnam
       TABLES
            out   = it_actual.

  IF p_mon IS INITIAL.
*---2004/03/23
*Issue Number : FI-20041118-007, Requested by Robin Simmons
*Changed on 2004/12/01, by WSKIM
*---Start
*  LOOP AT it_actual WHERE gjahr IN s_gjahr
*                    AND   ippos = ' '.
    LOOP AT it_actual WHERE ippos <>  0.
      IF s_gjahr-low <> ' '.
        CHECK it_actual-gjahr = s_gjahr-low.
      ENDIF.
*---End
      CASE it_actual-wrttp.
        WHEN '21'. "PR
          ADD it_actual-tot TO gt_out-pr_amt.
        WHEN '22'. "PO
          ADD it_actual-tot TO gt_out-po_amt.
        WHEN '04' OR '11'. " ACTUAL
          ADD it_actual-tot TO gt_out-act_amt.
*Issue Number : FI-20041118-007, Requested by Robin Simmons
*Changed on 2004/12/10, by WSKIM
*---Start
        WHEN '12'.  "downpayment
          ADD it_actual-tot TO gt_out-dow_amt.
*---End
      ENDCASE.
    ENDLOOP.

  ELSE.
    n = p_mon.

    LOOP AT it_actual WHERE ippos <> 0.
      IF s_gjahr-low <> ' '.
        CHECK it_actual-gjahr = s_gjahr-low.
      ENDIF.

      CLEAR l_tot.
      DO p_mon TIMES.
        CLEAR l_name.
        CONCATENATE 'IT_ACTUAL-WTG0' n INTO l_name.
        ASSIGN (l_name) TO <fs>.
        l_tot = l_tot + <fs>.

        n = n + 1.
      ENDDO.

      CASE it_io_actual-wrttp.
        WHEN '21'. "PR
          ADD l_tot TO gt_out-pr_amt.
        WHEN '22'. "P0
          ADD l_tot TO gt_out-po_amt.
        WHEN '04' OR '11'.
          ADD l_tot TO gt_out-act_amt.
        WHEN '12' .
          ADD l_tot TO gt_out-dow_amt.
      ENDCASE.

    ENDLOOP.

  ENDIF.

ENDFORM.                    " get_pi_ACTUAL
*&---------------------------------------------------------------------*
*&      Form  get_io_plan
*&---------------------------------------------------------------------*
FORM get_io_plan USING    u_posid
                          u_ayear
                          u_prnam.

  REFRESH it_budget.
  CLEAR   it_budget.

  CALL FUNCTION 'Z_FFI_GET_PI_BUDGET_IO'
       EXPORTING
            posid = u_posid
            prnam = u_prnam
            gjahr = u_ayear
       TABLES
            out   = it_budget.
*Issue Number : FI-20041118-007, Requested by Robin Simmons
*Changed on 2004/12/10, by WSKIM
*---Start
  IF  s_gjahr-low = space.
    READ TABLE it_budget WITH KEY posid = u_posid
                                 gjahr = '1111'.
    IF sy-subrc = 0.
      MOVE it_budget-wtjhr TO gt_out-io_amt.
    ENDIF.
  ELSE.
    READ TABLE it_budget WITH KEY posid = u_posid
                                  gjahr = s_gjahr-low.
    IF sy-subrc = 0.
      MOVE it_budget-wtjhr TO gt_out-io_amt.
    ENDIF.
  ENDIF.
*---End
ENDFORM.                    " get_io_plan
*&---------------------------------------------------------------------*
*&      Form  order_rpocess
*&---------------------------------------------------------------------*
FORM order_rpocess.
*  SELECT * INTO TABLE IT_AUFK FROM AUFK
*    WHERE AUFNR IN S_AUFNR
*    AND   KOSTL IN S_KOSTL.
*
**--GET IMZO
*  CLEAR WA_T_CNT.
*  DESCRIBE TABLE IT_AUFK LINES WA_T_CNT.
*  IF WA_T_CNT > 0.
*    SELECT * INTO TABLE IT_IMZO FROM IMZO
*    FOR ALL ENTRIES IN IT_AUFK
*    WHERE OBJNR = IT_AUFK-OBJNR.
*  ENDIF.
*
**---GET IMPR
*  CLEAR WA_T_CNT.
*  DESCRIBE TABLE IT_IMZO LINES WA_T_CNT.
*  IF WA_T_CNT > 0.
*    SELECT * INTO TABLE IT_IMPR FROM IMPR
*    FOR ALL ENTRIES IN IT_IMZO
*    WHERE POSNR = IT_IMZO-POSNR
*    AND   POSID IN S_POSID
*    AND   GJAHR = IT_IMZO-GJAHR
*    AND   PRNAM IN S_PRNAM.
*  ENDIF.
**====*
*
*  CLEAR GT_OUT.
*  LOOP AT IT_AUFK.
*    MOVE-CORRESPONDING IT_AUFK TO GT_OUT.
*    READ TABLE IT_IMZO WITH KEY OBJNR = IT_AUFK-OBJNR
*                                GJAHR = P_AYEAR.
*    IF SY-SUBRC = 0.
*      READ TABLE IT_IMPR WITH KEY POSNR = IT_IMZO-POSNR
*                                  GJAHR = IT_IMZO-GJAHR.
*      IF SY-SUBRC = 0.
*        MOVE IT_IMPR-POSID TO GT_OUT-POSID.
*        PERFORM GET_IO_BUDGET USING IT_AUFK-AUFNR.
*      ENDIF.
*    ENDIF.
*
**----2004/04/07
*    PERFORM GET_IO_ACTUAL USING IT_AUFK-AUFNR.
*
**<< Start of addition on 10.18.2006 by Michelle
*    IF GT_OUT-ASS_AMT <> 0 AND GT_OUT-CUR_AMT <> 0.
*      GT_OUT-AR_RATE = GT_OUT-ASS_AMT / GT_OUT-CUR_AMT.
*    ENDIF.
** End of addition on 10.18.2006 by Michelle >>
*
*    APPEND GT_OUT.
*    CLEAR  GT_OUT.
*  ENDLOOP.
  DATA: l_indx  TYPE i,
        l_posnr TYPE ima_posnr,
        l_usr03 TYPE ima_usr03,
        l_usr09 TYPE ima_usr09,
        l_bukrs TYPE bukrs,
        l_kokrs TYPE kokrs,
        l_chk VALUE 'X',
        l_stat,
        l_crtd,
        l_rel,
        l_lkd,
        l_ayear TYPE gjahr.

  CLEAR: itab, gt_out, l_ayear.
  REFRESH: itab, gt_out.

*  if p_s = 'X'.
*    l_ayear = p_ayear - 1.
*  else.
  l_ayear = p_ayear.
*  endif.

  SELECT a~aufnr c~posnr c~posid c~prnam c~objnr a~kostl
    INTO TABLE itab
    FROM aufk AS a
    JOIN imzo AS b
      ON b~objnr = a~objnr
    JOIN impr AS c
      ON c~posnr = b~posnr
     AND c~gjahr = b~gjahr
   WHERE a~aufnr IN s_aufnr
     AND a~kostl IN s_kostl
     AND c~posid IN s_posid
     AND c~prnam IN s_prnam
     AND c~gjahr = l_ayear.
  CLEAR l_indx.

  __cls gt_status.
  SELECT objnr stat INTO TABLE gt_status
    FROM jest
    FOR ALL ENTRIES IN itab
   WHERE objnr = itab-objnr
     AND ( stat = 'I0001' OR stat = 'I0002' OR stat = 'I0043' )
     AND inact <> 'X'.
  SORT  gt_status BY objnr stat.

* not important. {
*--------------------------------------------------------*
  DATA : total_lines TYPE i,
         count TYPE i.
  DATA  : total_doc_cnt TYPE i,
          current_doc_cnt TYPE i.
  DATA : percentage TYPE p,$mod TYPE i,
         $prog_text(50),$current_cnt(10),$total_cnt(10),$text(30) .
*--------------------------------------------------------*
* }

  DESCRIBE TABLE itab LINES total_doc_cnt.
  LOOP AT itab.

* not important. {
*--------------------------------------------------------*
    ADD 1 TO current_doc_cnt.
    $current_cnt = current_doc_cnt.
    $total_cnt = total_doc_cnt.
    $mod = current_doc_cnt MOD 10.
    IF $mod EQ 0.
      CONCATENATE $current_cnt '/' $total_cnt
      INTO $text.
      CONDENSE $text .
      CONCATENATE 'Please wait...' $text INTO $prog_text.
      percentage = current_doc_cnt / total_doc_cnt * 100.
      PERFORM show_progress USING $prog_text percentage.
    ENDIF.
*--------------------------------------------------------*
* }

    l_indx = l_indx + 1.

    CLEAR l_posnr.
    SELECT SINGLE posnr usr03 usr09 vkokrs vbukrs
    INTO (l_posnr,l_usr03,l_usr09,l_kokrs,l_bukrs)
      FROM imak
     WHERE posid = itab-posid
       AND ivart  IN s_ivart
       AND vkokrs IN s_vkokrs
       AND gjahr  IN s_agjahr
       AND usr03  IN s_usr03.

    IF sy-subrc <> 0.
      DELETE itab INDEX l_indx.
      CONTINUE.
    ELSE.
      CLEAR: l_stat, l_crtd, l_rel, l_lkd.

*      perform chk_status using    itab-objnr
*                         changing l_stat
*                                  l_crtd
*                                  l_rel
*                                  l_lkd
*                                  l_chk.

      PERFORM chk_status_all USING    itab-objnr
                             CHANGING l_stat
                                      l_crtd
                                      l_rel
                                      l_lkd
                                      l_chk.

      IF l_chk IS INITIAL.
        gt_out-posid = itab-posid.
        gt_out-aufnr = itab-aufnr.
        gt_out-objnr = itab-objnr.
        gt_out-kostl = itab-kostl.
        gt_out-prnam = itab-prnam.

        PERFORM get_io_budget USING itab-aufnr.
        PERFORM get_io_actual USING itab-aufnr.

        gt_out-stat = l_stat.
        gt_out-crtd = l_crtd.
        gt_out-rel = l_rel.
        gt_out-lkd = l_lkd.
        gt_out-usr03 = l_usr03.
        gt_out-usr09 = l_usr09.
        gt_out-kokrs = l_kokrs.
        gt_out-bukrs = l_bukrs.
        gt_out-posnr = l_posnr.
        APPEND gt_out.
        CLEAR  gt_out.
      ENDIF.

    ENDIF.
  ENDLOOP.

ENDFORM.                    " order_rpocess
*&---------------------------------------------------------------------*
*&      Form  cbo_pi_process
*&---------------------------------------------------------------------*
FORM cbo_pi_process.
  DATA l_ayear TYPE im_gnjhr.

  CLEAR l_ayear.

*  if p_s = 'X'.
*    l_ayear = p_ayear - 1.
*  else.
  l_ayear = p_ayear.
*  endif.

  CLEAR it_impr.
  REFRESH it_impr.

* disabled by IG.MOON 11/07 {
*  SELECT A~POSID A~OBJNR A~GJAHR A~KOSTL B~POSID
*     INTO TABLE IT_IMPR
*     FROM IMPR AS A
*     INNER JOIN IMAK AS B
*        ON B~POSID = A~POSID
*     WHERE A~GJAHR = L_AYEAR
*       AND A~POSID IN S_POSID
*       AND A~PRNAM IN S_PRNAM
*       AND A~KOSTL IN S_KOSTL
*       AND A~ERGSO IN S_ERGSO
*       AND B~IVART  IN S_IVART
*       AND B~VKOKRS IN S_VKOKRS
*       AND B~GJAHR  IN S_AGJAHR
*       AND B~USR03  IN S_USR03.
* }

*  IF s_agjahr IS INITIAL.
*    s_agjahr = 'IEQ'.
*    s_agjahr-low = p_ayear.
*    APPEND s_agjahr.
*  ENDIF.

  SELECT a~posid
         a~objnr
         a~gjahr
         a~vkostl AS kostl
         a~usr03
         a~usr09
         a~vkokrs AS kokrs
         a~vbukrs AS bukrs
         a~posnr
         b~posid AS arno
         b~prnam
         b~objnr AS impr_objnr
         b~ergso
     INTO CORRESPONDING FIELDS OF TABLE it_impr
     FROM imak AS a
     LEFT OUTER JOIN impr AS b
        ON b~posid = a~posid
     WHERE a~ivart  IN s_ivart
       AND a~vkokrs IN s_vkokrs
       AND a~gjahr  IN s_agjahr
       AND a~usr03  IN s_usr03
       AND a~posid  IN s_posid
       AND a~vkostl IN s_kostl.

  DATA $ix LIKE sy-tabix.
  LOOP AT it_impr.
    $ix = sy-tabix.
    IF NOT it_impr-prnam IN s_prnam.
      DELETE it_impr INDEX $ix.CONTINUE.
    ENDIF.
    IF NOT it_impr-ergso IN s_ergso.
      DELETE it_impr INDEX $ix.CONTINUE.
    ENDIF.
  ENDLOOP.

  CLEAR wa_t_cnt.
  DESCRIBE TABLE it_impr LINES wa_t_cnt.

  IF wa_t_cnt > 0.

*    __CLS it_imfm.
*
*    select * into table it_imfm
*      from ztfi_imfm
*       for all entries in it_impr
*     where posid = it_impr-posid
*       and posid in s_posid
*       and ayear = l_ayear
*       and status = 'A'
*       and gjahr in s_gjahr
*       and prnam in s_prnam
*       and kostl in s_kostl.
*
    SORT it_impr BY impr_objnr.

    SELECT objnr vorga wlges
    INTO CORRESPONDING FIELDS OF TABLE it_bpge
      FROM bpge
       FOR ALL ENTRIES IN it_impr
     WHERE lednr EQ '0001'
       AND objnr EQ it_impr-impr_objnr
       AND wrttp EQ '47'.

    __cls gt_status.
    SELECT objnr stat INTO TABLE gt_status
      FROM jest
      FOR ALL ENTRIES IN it_impr
     WHERE objnr = it_impr-impr_objnr
       AND ( stat = 'I0001' OR stat = 'I0002' OR stat = 'I0043' )
       AND inact <> 'X'.
    SORT  gt_status BY objnr stat.

  ENDIF.

*  if not it_bpge[] is initial.
**    perform get_it_sum.
*
*    loop at it_bpge.
*      read table it_impr with key impr_objnr = it_bpge-objnr
*      binary search.
*
*      check sy-subrc eq 0.
*
*      move-corresponding it_bpge to it_sum.
*      it_sum-gjahr = it_impr-gjahr.
*      it_sum-kostl = it_impr-kostl.
*      it_sum-posid = it_impr-posid.
*      it_sum-prnam = it_impr-prnam.
*      it_sum-tot   = it_bpge-wlges.
*
*      case it_bpge-vorga.
*        when 'KBUD'.
*          move '1' to it_sum-gubun.
*        when 'KBN0'.
*          move '2' to it_sum-gubun.
*        when 'KBR0'.
*          move '3' to it_sum-gubun.
*      endcase.
*
*      collect it_sum.
*      clear   it_sum.
*    endloop.
*
*    sort it_sum ascending by posid.
*
*  endif.

  SORT it_bpge BY objnr.
  DATA $fr TYPE i.

  LOOP AT it_impr.

    READ TABLE it_bpge WITH KEY objnr = it_impr-impr_objnr
    BINARY SEARCH.

    IF sy-subrc EQ 0.
      $fr = sy-tabix.

      LOOP AT it_bpge FROM $fr.
        IF it_bpge-objnr NE it_impr-impr_objnr.
          EXIT.
        ENDIF.

        MOVE-CORRESPONDING it_impr TO it_sum.

        it_sum-gjahr = it_impr-gjahr.
        it_sum-kostl = it_impr-kostl.
        it_sum-posid = it_impr-posid.
        it_sum-prnam = it_impr-prnam.
        it_sum-tot   = it_bpge-wlges.
        it_sum-impr_objnr = it_impr-impr_objnr.
        CASE it_bpge-vorga.
          WHEN 'KBUD'.
            MOVE '1' TO it_sum-gubun.
          WHEN 'KBN0'.
            MOVE '2' TO it_sum-gubun.
          WHEN 'KBR0'.
            MOVE '3' TO it_sum-gubun.
        ENDCASE.

        COLLECT it_sum.
        CLEAR   it_sum.

      ENDLOOP.

    ELSE.
      MOVE-CORRESPONDING it_impr TO it_sum.

      it_sum-gjahr = it_impr-gjahr.
      it_sum-kostl = it_impr-kostl.
      it_sum-posid = it_impr-posid.
      it_sum-prnam = it_impr-prnam.
*      it_sum-tot   = it_bpge-wlges.

      COLLECT it_sum.
      CLEAR   it_sum.

    ENDIF.
  ENDLOOP.

  SORT it_sum ASCENDING BY posid.

  IF NOT it_sum[] IS INITIAL.
    DATA: l_chk VALUE 'X',
          l_stat,
          l_crtd,
          l_rel,
          l_lkd.

* not important. {
*--------------------------------------------------------*
    DATA : total_lines TYPE i,
           count TYPE i.
    DATA  : total_doc_cnt TYPE i,
            current_doc_cnt TYPE i.
    DATA : percentage TYPE p,$mod TYPE i,
           $prog_text(50),$current_cnt(10),$total_cnt(10),$text(30) .
*--------------------------------------------------------*
* }
    CLEAR: wa_org_amt, wa_sup_amt, wa_ret_amt, wa_prnam.
    DESCRIBE TABLE it_sum LINES total_doc_cnt.
    LOOP AT it_sum.

* not important. {
*--------------------------------------------------------*
      ADD 1 TO current_doc_cnt.
      $current_cnt = current_doc_cnt.
      $total_cnt = total_doc_cnt.
      $mod = current_doc_cnt MOD 10.
      IF $mod EQ 0.
        CONCATENATE $current_cnt '/' $total_cnt
        INTO $text.
        CONDENSE $text .
        CONCATENATE 'Please wait...' $text INTO $prog_text.
        percentage = current_doc_cnt / total_doc_cnt * 100.
        PERFORM show_progress USING $prog_text percentage.
      ENDIF.
*--------------------------------------------------------*
* }

      gt_out-posid = it_sum-posid.
      gt_out-prnam = it_sum-prnam.
      gt_out-objnr = it_sum-objnr.
      gt_out-kostl = it_sum-kostl.

      wa_prnam = it_sum-prnam.

      CASE it_sum-gubun.
        WHEN '1'.
          MOVE it_sum-tot TO gt_out-org_amt.
        WHEN '2'.
          MOVE it_sum-tot TO gt_out-sup_amt.
        WHEN '3'.
          MOVE it_sum-tot TO gt_out-ret_amt.
      ENDCASE.

      gt_out-cur_amt
        = gt_out-org_amt + gt_out-sup_amt + gt_out-ret_amt.

      gt_out-usr03 = it_sum-usr03.
      gt_out-usr09 = it_sum-usr09.
      gt_out-kokrs = it_sum-kokrs.
      gt_out-bukrs = it_sum-bukrs.
      gt_out-posnr = it_sum-posnr.
      gt_out-impr_objnr = it_sum-impr_objnr.
      AT END OF posid.
        CLEAR: l_stat, l_crtd, l_rel, l_lkd.

* by ig.moon
* {
*        perform chk_status using    gt_out-objnr
*                           changing l_stat
*                                    l_crtd
*                                    l_rel
*                                    l_lkd
*                                    l_chk.

        PERFORM chk_status_all USING    gt_out-impr_objnr
                               CHANGING l_stat
                                        l_crtd
                                        l_rel
                                        l_lkd
                                        l_chk.

* }

*        if l_chk is initial.
*          if p_s = 'X'.
*            clear l_ayear.
        l_ayear = p_ayear.                                  " - 1.
*          endif.


        PERFORM get_ar_plan USING it_sum-posid p_ayear.

        PERFORM get_pi_plan USING it_sum-posid l_ayear wa_prnam.
        PERFORM get_io_plan USING it_sum-posid l_ayear wa_prnam.
        PERFORM get_pi_actual USING it_sum-posid l_ayear wa_prnam.

        gt_out-difa_amt = gt_out-act_amt - gt_out-org_amt.
        gt_out-difb_amt = gt_out-act_amt - gt_out-cur_amt.

        IF gt_out-dpcr_amt < 0.
          gt_out-dpcr_amt = 0.
        ENDIF.

        gt_out-stat = l_stat.
        gt_out-crtd = l_crtd.
        gt_out-rel = l_rel.
        gt_out-lkd = l_lkd.

        APPEND gt_out.
        CLEAR: gt_out, wa_prnam.
*        endif.

      ENDAT.

    ENDLOOP.
  ENDIF.

*data $ix like sy-tabix.
*
*  loop at gt_out.
*    $ix = sy-tabix.
*    if p_crtd eq true.
*      gt_out-crtd eq true.
*      delete gt_out index $ix.
*    endif.
*    if p_rel eq true.
*    if p_lkd eq true.
*  endloop.
*
ENDFORM.                    " cbo_pi_process
*&---------------------------------------------------------------------*
*&      Form  GET_IO_BUDGET
*&---------------------------------------------------------------------*
FORM get_io_budget USING    u_aufnr.
  REFRESH : it_io_budget.
  CLEAR   : it_io_budget.
  CALL FUNCTION 'Z_FFI_GET_IO_BUDGET'
       EXPORTING
            aufnr = u_aufnr
       TABLES
            out   = it_io_budget.
  READ TABLE it_io_budget WITH KEY aufnr = u_aufnr
                                   gjahr = '1111'.
  IF sy-subrc = 0.
    MOVE it_io_budget-plan TO gt_out-zplan.
    MOVE it_io_budget-org  TO gt_out-org_amt.
    MOVE it_io_budget-supp TO gt_out-sup_amt.

    gt_out-cur_amt =  gt_out-org_amt
                   +  gt_out-sup_amt - gt_out-ret_amt.
    gt_out-io_amt =  gt_out-org_amt
                   +  gt_out-sup_amt - gt_out-ret_amt.

  ENDIF.

ENDFORM.                    " GET_IO_BUDGET
*&---------------------------------------------------------------------*
*&      Form  get_io_actual
*&---------------------------------------------------------------------*
FORM get_io_actual USING    u_aufnr.
  DATA: l_name(20),
        l_tot TYPE wtgxxx,
        n(2) TYPE n.

  FIELD-SYMBOLS <fs> TYPE ANY.

  REFRESH it_io_actual.
  CLEAR   it_io_actual.

  CALL FUNCTION 'Z_FFI_GET_IO_ACTUAL'
       EXPORTING
            aufnr = u_aufnr
       TABLES
            out   = it_io_actual.

  IF p_mon IS INITIAL.
    CASE it_io_actual-wrttp.
      WHEN '21'. "PR
        ADD it_io_actual-tot TO gt_out-pr_amt.
      WHEN '22'. "P0
        ADD it_io_actual-tot TO gt_out-po_amt.
      WHEN '04' OR '11'.
        ADD it_io_actual-tot TO gt_out-act_amt.
*Issue Number : FI-20041118-007, Requested by Robin Simmons
*Changed on 2004/12/10, by WSKIM
*---Start
      WHEN '12' .
        ADD it_io_actual-tot TO gt_out-dow_amt.
*---End
    ENDCASE.

  ELSE.
    n = p_mon.

    LOOP AT it_io_actual WHERE gjahr IN s_gjahr.
      CLEAR l_tot.
      DO p_mon TIMES.
        CLEAR l_name.
        CONCATENATE 'IT_IO_ACTUAL-WTG0' n INTO l_name.
        ASSIGN (l_name) TO <fs>.
        l_tot = l_tot + <fs>.

        n = n + 1.
      ENDDO.

      CASE it_io_actual-wrttp.
        WHEN '21'. "PR
          ADD l_tot TO gt_out-pr_amt.
        WHEN '22'. "P0
          ADD l_tot TO gt_out-po_amt.
        WHEN '04' OR '11'.
          ADD l_tot TO gt_out-act_amt.
        WHEN '12' .
          ADD l_tot TO gt_out-dow_amt.
      ENDCASE.

    ENDLOOP.
  ENDIF.

ENDFORM.                    " get_io_actual
*&---------------------------------------------------------------------*
*&      Form  MAKE_OUT
*&---------------------------------------------------------------------*
FORM make_out.

  DATA: BEGIN OF it_aina OCCURS 0,
          objnr   LIKE   ania-objnr,
          anlkl   LIKE   ania-anlkl,
          aktiv   LIKE   ania-aktiv,
        END   OF it_aina.

  SELECT objnr anlkl aktiv INTO TABLE it_aina
    FROM ania
    FOR ALL ENTRIES IN gt_out
    WHERE objnr = gt_out-$objnr.

  SORT it_aina BY objnr.

  LOOP AT gt_out.
*Issue Number : FI-20041118-005, Requested by YCYOON
*Changed on 2004/12/09, by WSKIM
*---Start
* Available...
    gt_out-res_amt = gt_out-cur_amt - gt_out-act_amt - gt_out-po_amt
                     - gt_out-pr_amt.
*---End
    gt_out-ass_amt = gt_out-act_amt + gt_out-po_amt + gt_out-pr_amt.

    IF gt_out-org_amt = 0.
      gt_out-difa_amt = 0.
    ELSE.
      gt_out-difa_amt = ( gt_out-act_amt + gt_out-dow_amt )
                          / gt_out-org_amt.
    ENDIF.

    IF gt_out-cur_amt = 0.
      gt_out-difb_amt = 0.
    ELSE.
      gt_out-difb_amt = ( gt_out-act_amt + gt_out-dow_amt )
                          / gt_out-cur_amt.
    ENDIF.

    SELECT SINGLE txt50 INTO gt_out-txt50
        FROM imakt
        WHERE spras = sy-langu
          AND posnr = gt_out-posid.

    IF gt_out-ass_amt <> 0 AND gt_out-arplan <> 0.
      gt_out-ar_rate = 100 * gt_out-ass_amt / gt_out-arplan.
    ENDIF.

    IF gt_out-ass_amt <> 0 AND gt_out-cur_amt <> 0.
      gt_out-pi_rate = 100 * gt_out-ass_amt / gt_out-cur_amt.
    ENDIF.

    gt_out-twaer = 'USD'.

    READ TABLE it_aina WITH KEY objnr = gt_out-$objnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      gt_out-anlkl =  it_aina-anlkl.
      gt_out-aktiv =  it_aina-aktiv.
      IF NOT gt_out-anlkl IS INITIAL.
        gt_out-icon = icon_led_green.
      ENDIF.
    ENDIF.

    MODIFY gt_out.
  ENDLOOP.

ENDFORM.                    " MAKE_OUT
*&---------------------------------------------------------------------*
*&      Form  get_ar_plan
*&---------------------------------------------------------------------*
FORM get_ar_plan USING    u_posid
                          u_ayear.

  __cls : lt_ania, lt_anib.

  DATA: BEGIN OF i_plan_tot OCCURS 0.
          INCLUDE STRUCTURE bapiappreqplantotalmulti.
  DATA: END OF i_plan_tot.

  DATA it_variant LIKE bapiappreqvarntassignmulti OCCURS 0
                                                  WITH HEADER LINE.

  CALL FUNCTION 'BAPI_APPREQUEST_GETDETAIL'
       EXPORTING
            externalnumber     = u_posid
       TABLES
            plan_total         = i_plan_tot
            variant_to_version = it_variant.

  READ TABLE it_variant WITH KEY appr_year    = u_ayear
                                 plan_version = p_arver.
  IF sy-subrc = 0.
    gt_out-arvrnt = it_variant-appreqvrnt.

    READ TABLE i_plan_tot WITH KEY appreqvrnt = it_variant-appreqvrnt.
    IF sy-subrc EQ 0.
      MOVE i_plan_tot-investment_costs TO gt_out-arplan.
    ENDIF.
  ENDIF.

*  refresh: lt_ania, lt_anib.
*
*  select * into corresponding fields of table lt_ania
*      from ania where objnr = gt_out-objnr.
*
*  check sy-subrc eq 0.
*  select * into corresponding fields of table lt_anib
*      from anib where objnr = gt_out-objnr.
*
*  check sy-subrc eq 0.

  READ TABLE it_variant WITH KEY appr_year    = p_yearp "u_ayear
                                 plan_version = p_darver.
  IF sy-subrc = 0.

    READ TABLE i_plan_tot WITH KEY appreqvrnt = it_variant-appreqvrnt.
    IF sy-subrc EQ 0.
      gt_out-dpcr_amt = i_plan_tot-investment_costs.

      MOVE i_plan_tot-investment_costs TO gt_out-dpplan.

      SELECT SINGLE * FROM imavz
          WHERE gjahr = p_yearp
            AND posnr = gt_out-posnr
            AND versi = p_darver.

      IF sy-subrc = 0.
        SELECT SINGLE * FROM imav
           WHERE posnr = gt_out-posnr
             AND varnt = imavz-varnt.
        gt_out-$objnr =  imav-objnr.
      ENDIF.

    ENDIF.
  ENDIF.

ENDFORM.                    " get_ar_plan

* For BDC
**---------------------------------------------------------------------*
**       Form DYNPRO                                                   *
**---------------------------------------------------------------------*
*form dynpro using p_dynbegin p_name p_value.
*  clear gt_bdc.
*
*  if p_dynbegin = 'X'.
*    gt_bdc-program = p_name.
*    gt_bdc-dynpro = p_value.
*    gt_bdc-dynbegin = p_dynbegin.
*  else.
*    gt_bdc-fnam = p_name.
*    gt_bdc-fval = p_value.
*  endif.
*
*  append gt_bdc.
*
*endform.                    " DYNPRO
**---------------------------------------------------------------------*
**       Form GET_MSG
**
**---------------------------------------------------------------------*
*form get_msg changing p_msg.
*  call function 'RKC_MSG_STRING'
*       exporting
*            id      = sy-msgid
*            mtype   = sy-msgty
*            number  = sy-msgno
*            par1    = sy-msgv1
*            par2    = sy-msgv2
*            par3    = sy-msgv3
*            par4    = sy-msgv4
*       importing
*            msg_lin = p_msg
*       exceptions
*            others  = 1.
*
*endform.                    " RKC_MSG_STRING
*&---------------------------------------------------------------------*
*&      Form  CHANGE_STATUS
*&---------------------------------------------------------------------*
*       Change to status
*----------------------------------------------------------------------*
FORM change_status USING p_ucomm TYPE syucomm.
  DATA: l_okcode(5),
        l_chk VALUE 'X',
        l_stat.

  LOOP AT gt_out WHERE chk = 'X'.
    CLEAR l_okcode.

    IF p_ucomm = 'PRE'.
      PERFORM pre_step CHANGING l_okcode.
    ELSEIF p_ucomm = 'NEXT'.
      PERFORM next_step CHANGING l_okcode.
    ENDIF.

    IF NOT l_okcode IS INITIAL.
      PERFORM bdc_im12 USING l_okcode.
    ENDIF.

    READ TABLE gt_msg WITH KEY msgtyp = 'S'
                               msgid = 'AP'
                               msgnr = '010'.

    IF sy-subrc = 0.
      WAIT UP TO 1 SECONDS.
      CLEAR: gt_out-stat, gt_out-crtd, gt_out-rel, gt_out-lkd.

      PERFORM chk_status USING    gt_out-objnr
                         CHANGING gt_out-stat
                                  gt_out-crtd
                                  gt_out-rel
                                  gt_out-lkd
                                  l_chk.
      MODIFY gt_out.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " CHANGE_STATUS
*&---------------------------------------------------------------------*
*&      Form  GET_IT_SUM
*&---------------------------------------------------------------------*
FORM get_it_sum.

*  loop at it_imfm.
*    read table it_impr with key posid = it_imfm-posid binary search.
*    check sy-subrc eq 0.
*
*    move-corresponding it_imfm to it_sum.
*    it_sum-gjahr = it_impr-gjahr.
*    it_sum-objnr = it_impr-impr_objnr.
*    it_sum-kostl = it_impr-kostl.
*
*    collect it_sum.
*    clear   it_sum.
*  endloop.
*
**Refer ZRFII02 - IM-PI Budget/Actual Report
**FIXME
*
*  sort it_sum ascending by posid.
*
ENDFORM.                    " GET_IT_SUM
*&---------------------------------------------------------------------*
*&      Form  GET_IT_IMAK
*&---------------------------------------------------------------------*
*       AR
*----------------------------------------------------------------------*
*FORM GET_IT_IMAK.
*  CLEAR IT_IMAK.
*  REFRESH IT_IMAK.
*
*  SELECT POSID INTO TABLE IT_IMAK
*    FROM IMAK
*     FOR ALL ENTRIES IN IT_IMPR
*   WHERE POSID = IT_IMPR-POSID
*     AND POSNR  IN S_POSNR
*     AND IVART  IN S_IVART
*     AND VKOKRS IN S_VKOKRS
*     AND VPRCTR IN S_VPRCTR
*     AND GJAHR  IN S_AGJAHR
*     AND USR03  IN S_USR03.
*
*  CLEAR WA_T_CNT1.
*  DESCRIBE TABLE IT_IMAK LINES WA_T_CNT1.
*
*ENDFORM.                    " GET_IT_IMAK
*&---------------------------------------------------------------------*
*&      Form  CHK_STATUS
*&---------------------------------------------------------------------*
*       Check Status
*----------------------------------------------------------------------*
FORM chk_status USING    p_objnr TYPE im_objnr
                CHANGING p_stat
                         l_crtd
                         l_rel
                         l_lkd
                         p_chk.

  DATA: lt_status TYPE TABLE OF ty_status WITH HEADER LINE.

* Check Status
*   : I0001	CRTD	Created
*     I0002	REL	Released
*     I0043	LKD	Locked
  CLEAR lt_status.
  REFRESH lt_status.

  SELECT objnr stat INTO TABLE lt_status
    FROM jest
   WHERE objnr = p_objnr
     AND ( stat = 'I0001' OR stat = 'I0002' OR stat = 'I0043' )
     AND inact <> 'X'.

  IF sy-subrc = 0.
    SORT lt_status BY stat.

    READ TABLE lt_status WITH KEY stat = 'I0001' BINARY SEARCH.
    IF sy-subrc = 0.
      l_crtd = 'X'.
      p_stat = 1.
    ENDIF.

    READ TABLE lt_status WITH KEY stat = 'I0002' BINARY SEARCH.
    IF sy-subrc = 0.
      l_crtd = 'X'.
      l_rel = 'X'.
      p_stat = 2.
    ENDIF.

    READ TABLE lt_status WITH KEY stat = 'I0043' BINARY SEARCH.
    IF sy-subrc = 0.
      l_crtd = 'X'.
      l_lkd = 'X'.
      p_stat = 3.
    ENDIF.
  ENDIF.

* Created
  IF p_crtd = 'X'.
    IF l_crtd = 'X'.
      CLEAR p_chk.
    ENDIF.
  ENDIF.

* Released
  IF p_rel = 'X'.
    IF l_rel = 'X'.
      CLEAR p_chk.
    ENDIF.
  ENDIF.

* Locked
  IF p_lkd = 'X'.
    IF l_lkd = 'X'.
      CLEAR p_chk.
    ENDIF.
  ENDIF.

ENDFORM.                    " CHK_STATUS
*&---------------------------------------------------------------------*
*&      Form  BDC_IM12
*&---------------------------------------------------------------------*
*       Change IM
*----------------------------------------------------------------------*
FORM bdc_im12 USING p_okcode.
  REFRESH: gt_bdc, gt_msg.
  CLEAR  : gt_bdc, gt_msg.

  PERFORM dynpro USING: 'X'  'SAPLAIP2'          '0500',
                        ' '  'BDC_OKCODE'        '/00',
                        ' '  'IMPR-PRNAM'        gt_out-prnam,
                        ' '  'IMPR-POSID'        gt_out-posid,
                        ' '  'IMPR-GJAHR'        it_impr-gjahr,

                        'X'  'SAPLAIP2'          '0600',
                        ' '  'BDC_OKCODE'        p_okcode,

                        'X'  'SAPLAIP2'          '0600',
                        ' '  'BDC_OKCODE'        '=UPD'.

  CALL TRANSACTION 'IM12'  USING         gt_bdc
                           OPTIONS FROM  gs_opt
                           MESSAGES INTO gt_msg.


ENDFORM.                                                    " BDC_IM12
*&---------------------------------------------------------------------*
*&      Form  PRE_STEP
*&---------------------------------------------------------------------*
*  Change to previous status
*----------------------------------------------------------------------*
FORM pre_step CHANGING p_okcode.
  CASE gt_out-stat.
    WHEN 2.
      p_okcode = '=BFRZ'.    " Cancel Release
    WHEN 3.
      p_okcode = '=BUNL'.    " Unlock
  ENDCASE.

ENDFORM.                    " PRE_STEP
*&---------------------------------------------------------------------*
*&      Form  NEXT_STEP
*&---------------------------------------------------------------------*
*  Change to next status
*----------------------------------------------------------------------*
FORM next_step CHANGING p_okcode.
  CASE gt_out-stat.
    WHEN 1.
      p_okcode = '=BFRE'.    " Release
    WHEN 2.
      p_okcode = '=BLOC'.    " Block
  ENDCASE.

ENDFORM.                    " NEXT_STEP
*&---------------------------------------------------------------------*
*&      Form  CREATE_DPCR
*&---------------------------------------------------------------------*
*       Create depreciation data
*----------------------------------------------------------------------*
FORM create_dpcr.
  DATA: lt_variant    TYPE TABLE OF bapiappreqvarntassignmulti
                                    WITH HEADER LINE,
        lt_plan_tot   TYPE TABLE OF bapiappreqplantotalmulti
                                    WITH HEADER LINE,
        lt_version TYPE TABLE OF bapiappreqvarntassign
                                 WITH HEADER LINE,
        lt_plan_year  TYPE TABLE OF bapiappreqplanyear WITH HEADER LINE,
        lt_return     TYPE TABLE OF bapiret2 WITH HEADER LINE,
        wa_plan_total LIKE bapiappreqplantotal,
        l_index       TYPE sytabix,
        l_arvrnt      TYPE ima_varnt.

  CLEAR: l_index, l_arvrnt.

  LOOP AT gt_out WHERE chk = 'X'.
*    if gt_out-dpcr_amt is initial.
*      message s000 with 'Check depreciation amount!'.
*      continue.
*    endif.


    l_index = sy-tabix.

*   Check AR Variant
    CLEAR: lt_variant, lt_plan_tot.
    REFRESH: lt_variant, lt_plan_tot.

    CALL FUNCTION 'BAPI_APPREQUEST_GETDETAIL'
         EXPORTING
              externalnumber     = gt_out-posid
              language           = sy-langu
         TABLES
              variant_to_version = lt_variant
              plan_tot           = lt_plan_tot.

    READ TABLE lt_variant WITH KEY appr_year = p_yearp "p_ayear
                                   plan_version = p_darver. "p_arver.

    IF sy-subrc = 0.
      l_arvrnt = lt_variant-appreqvrnt.
    ENDIF.

    wa_plan_total-investment_costs = gt_out-dpcr_amt.

    CLEAR: lt_version, lt_plan_year, lt_return.
    REFRESH: lt_version, lt_plan_year, lt_return.


    lt_plan_year-fiscal_year    = p_yearp. "p_ayear.
    lt_plan_year-investment_costs = wa_plan_total-investment_costs.
    APPEND lt_plan_year.

    IF l_arvrnt IS INITIAL.
*     Create Appropriation Request Variant

      lt_version-appr_year    = p_yearp. "p_ayear.
      lt_version-plan_version = p_darver. "p_arver.
      APPEND lt_version.


      CALL FUNCTION 'BAPI_APPREQUEST_ADDVARIANT'
           EXPORTING
                externalnumber     = gt_out-posid
                plan_total         = wa_plan_total
           TABLES
                variant_to_version = lt_version
                plan_year          = lt_plan_year
                return             = lt_return.

    ELSE.

*     Change Plan Values of Appropriation Request Variant
      CALL FUNCTION 'BAPI_APPREQUEST_SETPLANVALUES'
           EXPORTING
                externalnumber              = gt_out-posid
                appropriationrequestvariant = l_arvrnt
                plan_total                  = wa_plan_total
           TABLES
                plan_year                   = lt_plan_year
                return                      = lt_return.
    ENDIF.

*   Get result
    READ TABLE lt_return WITH KEY type = 'E'.

    IF sy-subrc <> 0.
*     Commit
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
           EXPORTING
                wait = 'X'.
* disabled by ig.moon 11/25/2008 {
***      IF gt_out-posnr NE space.
**** copied from ZCFII11 {
***
***        SELECT SINGLE * FROM imavz
***            WHERE gjahr = p_yearp
***              AND posnr = gt_out-posnr
***              AND versi = p_darver.
***
***        IF sy-subrc = 0.
***
***          SELECT SINGLE * FROM imav
***             WHERE posnr = gt_out-posnr
***               AND varnt = imavz-varnt.
***
***          PERFORM simulate_depr USING gt_out-usr03
***                                      gt_out-usr09
***                                      imav-objnr
***                                      gt_out-kokrs
***                                      gt_out-bukrs
***                                      imav-posnr.
***
***          CALL FUNCTION 'DEPREC_SIMUL_MODIFIKATION'
***               TABLES
***                    t_ania = lt_ania
***                    t_anib = lt_anib1.
***
***          gt_out-anlkl = lt_ania-anlkl.
***          gt_out-aktiv = lt_ania-aktiv.
***
***          gt_out-icon = icon_led_green.
***
***        ENDIF.
**** }
***      ENDIF.
* }
    ELSE.
      gt_out-icon = icon_led_red.
*     Rollback
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDIF.


    MODIFY gt_out INDEX l_index TRANSPORTING icon anlkl aktiv.
  ENDLOOP.

ENDFORM.                    " CREATE_DPCR
*&---------------------------------------------------------------------*
*&      Form  CAL_DPCR_AMT
*&---------------------------------------------------------------------*
*       Calculate Depreciation amount
*----------------------------------------------------------------------*
FORM cal_dpcr_amt USING p_ucomm TYPE syucomm.
  DATA: l_index TYPE sytabix,
        l_amt   TYPE bp_wpt.

  CLEAR l_index.

  LOOP AT gt_out WHERE chk = 'X'.
    l_index = sy-tabix.
    CLEAR l_amt.

    CASE p_ucomm.
      WHEN 'CAL1'.
        l_amt = gt_out-io_amt.
      WHEN 'CAL2'.
        l_amt = gt_out-cur_amt.
      WHEN 'CAL3'.
*        l_amt = gt_out-zplan.
        l_amt = gt_out-arplan.
    ENDCASE.

    gt_out-dpcr_amt =  l_amt - gt_out-act_amt - gt_out-dow_amt.

    IF gt_out-dpcr_amt < 0.
      gt_out-dpcr_amt = 0.
    ENDIF.

    MODIFY gt_out  INDEX l_index TRANSPORTING dpcr_amt.
  ENDLOOP.

ENDFORM.                    " CAL_DPCR_AMT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET TITLEBAR '100'.
*   Exclude toolbar
  PERFORM exclude_functions.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  exclude_functions
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM exclude_functions.
  PERFORM append_exclude_functions
           TABLES gt_exclude[]
           USING: cl_gui_alv_grid=>mc_fc_loc_undo,
                  cl_gui_alv_grid=>mc_fc_average,
                  cl_gui_alv_grid=>mc_fc_graph,
                  cl_gui_alv_grid=>mc_fc_info,
                  cl_gui_alv_grid=>mc_fc_loc_copy_row,
                  cl_gui_alv_grid=>mc_fc_loc_append_row,
                  cl_gui_alv_grid=>mc_fc_loc_cut,
                  cl_gui_alv_grid=>mc_fc_loc_insert_row,
                  cl_gui_alv_grid=>mc_fc_loc_move_row,
                  cl_gui_alv_grid=>mc_fc_loc_paste_new_row.


ENDFORM.                    " exclude_functions
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_alv_100 OUTPUT.

  IF g_custom_container IS INITIAL.
    PERFORM create_and_init_alv.

*   Display alv grid
    CALL METHOD g_grid->set_table_for_first_display
         EXPORTING is_layout            = gs_layo
                   it_toolbar_excluding = gt_exclude
                   i_save               = gc_var_save
                   is_variant           = gs_variant
         CHANGING  it_outtab            = gt_out[]
                   it_fieldcatalog      = gt_fcat[]
                   it_sort              = gt_sort[].
  ELSE.
    CALL METHOD g_grid->refresh_table_display.
  ENDIF.
  __focus g_grid.

  PERFORM user_status.


ENDMODULE.                 " DISPLAY_ALV_100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  user_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_status.
  __cls ftab.

  ftab-fcode = 'PRE'.
  APPEND ftab.
  ftab-fcode = 'NEXT'.
  APPEND ftab.
  ftab-fcode = 'CAL1'.
  APPEND ftab.
  ftab-fcode = 'CAL2'.
  APPEND ftab.
  ftab-fcode = 'CAL3'.
  APPEND ftab.

  SET PF-STATUS '100' EXCLUDING ftab.

  CASE 'X'.
    WHEN p_io.
      info = 'IO Budget'.
    WHEN p_pi.
      info = 'Current Budget'.
    WHEN p_ar.
      info = 'AR Plan'.
  ENDCASE.

ENDFORM.                    " user_status
*&---------------------------------------------------------------------*
*&      Form  show_progress
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_&1  text
*      -->P_&2  text
*----------------------------------------------------------------------*
FORM show_progress USING    pf_text
                            value(pf_val).
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            percentage = pf_val
            text       = pf_text.

ENDFORM.                    " SHOW_PROGRESS
*&---------------------------------------------------------------------*
*&      Form  create_and_init_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_and_init_alv.
*   Create object
  PERFORM create_object.

*  Create Object to verify input values.
  CREATE OBJECT g_event_receiver.
  SET HANDLER : g_event_receiver->handle_data_changed FOR g_grid,
                g_event_receiver->handle_double_click FOR g_grid.

*   Create field category
  PERFORM create_field_category USING false.

  CALL METHOD g_grid->register_edit_event
       EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD g_grid->set_ready_for_input
     EXPORTING
            i_ready_for_input = 0.

  PERFORM sort_build USING gt_sort[].

*   Setting for layout
  PERFORM set_lvc_layout.

*   Set colors
  PERFORM set_color.

*   Set variant
  gv_repid = gs_variant-report = sy-repid.
  gs_variant-variant = p_vari.

*   Define cell attribute
  PERFORM build_cell_attr.

ENDFORM.                    " create_and_init_alv
*&---------------------------------------------------------------------*
*&      Form  build_cell_attr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_cell_attr.
  DATA: lt_celltab TYPE lvc_t_styl,
        ls_celltab TYPE lvc_s_styl.

  CLEAR lt_celltab.
  REFRESH lt_celltab.

  CLEAR gs_fcat.

  LOOP AT gt_fcat INTO gs_fcat.
    ls_celltab-fieldname = gs_fcat-fieldname.
    IF ls_celltab-fieldname EQ 'DPCR_AMT'.
      ls_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
    ELSE.
      ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    ENDIF.
    INSERT ls_celltab INTO TABLE lt_celltab.
  ENDLOOP.

  CLEAR gt_out-celltab.
  INSERT LINES OF lt_celltab INTO TABLE gt_out-celltab.
  MODIFY gt_out TRANSPORTING celltab WHERE celltab IS initial.
  PERFORM build_cell_attr1_lock.
ENDFORM.                    " build_cell_attr
*&---------------------------------------------------------------------*
*&      Form  create_field_category
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FALSE  text
*----------------------------------------------------------------------*
FORM create_field_category USING mode_edit.

  DATA: l_pos       TYPE i.
  DEFINE __catalog.
    l_pos = l_pos + 1.
    clear gs_fcat.
    gs_fcat-col_pos       = l_pos.
    gs_fcat-key           = &1.
    gs_fcat-fieldname     = &2.
    gs_fcat-coltext       = &3.     " Column heading
    gs_fcat-outputlen     = &4.     " Column width
    gs_fcat-datatype      = &5.     " Data type
    append gs_fcat to gt_fcat.
  END-OF-DEFINITION.

  IF r_1 = 'X'.
    __catalog :
     'X' 'POSID'     'Position ID'     '24' 'CHAR',
     'X' 'TXT50'     'Description'     '25' 'CHAR',
     ' ' 'KOSTL'     'Cost Ctr'        '10' 'CHAR',
     ' ' 'CRTD'       'Created'        '1'  'CHAR',
     ' ' 'REL'       'Released'        '1'  'CHAR',
     ' ' 'LKD'       'Locked'          '1'  'CHAR',
     ' ' 'ARPLAN'    'AR Plan'         '21' 'CHAR',
     ' ' 'DPPLAN'    'DP Plan'         '21' 'CHAR',
     ' ' 'ZPLAN'     'PI Plan'         '21' 'CHAR',
     ' ' 'ORG_AMT'   'A.Original'      '21' 'CURR',
     ' ' 'SUP_AMT'   'B.Supplement'    '21' 'CURR',
     ' ' 'RET_AMT'   'C.Return'        '21' 'CURR',
     ' ' 'CUR_AMT'   'D.Cur(A+B+C)'    '21' 'CURR',
     ' ' 'IO_AMT'    'E.IO Budget'     '21' 'CURR',
     ' ' 'PR_AMT'    'F.Commit-PR'     '21' 'CURR',
     ' ' 'PO_AMT'    'G.Cmmit-PO'      '21' 'CURR',
     ' ' 'ACT_AMT'   'H.Actual'        '21' 'CURR',
     ' ' 'ASS_AMT'   'I.Assign(F+G+H)' '21' 'CURR',
     ' ' 'RES_AMT'   'J.Avail.(D-I)'   '21' 'CURR',
     ' ' 'DOW_AMT'   'Downpayment'     '21' 'CURR',
     ' ' 'DIFA_AMT'  'Org-Act'         '21' 'CURR',
     ' ' 'DIFB_AMT'  'Cur-Act'         '21' 'CURR',
     ' ' 'DPCR_AMT'  'Balance for DP'  '21' 'CURR',
     ' ' 'AR_RATE'   'AR %Progress'    '21' 'CURR',
     ' ' 'PI_RATE'   'PI %Progress'    '21' 'CURR',
     ' ' 'ANLKL'     'Asset Class'     '8'  'CHAR',
     ' ' 'AKTIV'     'Cap.Date'        '8'  'DATS',
     ' ' 'ICON'      'Depr.'    '3' 'ICON'.
  ELSE.
    __catalog :
    'X' 'POSID'     'Position ID'     '24' 'CHAR',
    'X' 'TXT50'     'Description'     '25' 'CHAR',
    'X' 'AUFNR'     'I.Order'         '10' 'CHAR',
    ' ' 'KOSTL'     'Cost Ctr'        '10' 'CHAR',
     ' ' 'CRTD'       'Created'        '1'  'CHAR',
    ' ' 'REL'       'Released'        '1'  'CHAR',
    ' ' 'LKD'       'Locked'          '1'  'CHAR',
    ' ' 'ZPLAN'      'Plan'           '21' 'CURR',
    ' ' 'ORG_AMT'   'A.Original'      '21' 'CURR',
    ' ' 'SUP_AMT'   'B.Supplement'    '21' 'CURR',
    ' ' 'RET_AMT'   'C.Return'        '21' 'CURR',
    ' ' 'CUR_AMT'   'D.Cur(A+B+C)'    '21' 'CURR',
    ' ' 'IO_AMT'    'E.IO Budget'     '21' 'CURR',
    ' ' 'PR_AMT'    'F.Commit-PR'     '21' 'CURR',
    ' ' 'PO_AMT'    'G.Cmmit-PO'      '21' 'CURR',
    ' ' 'ACT_AMT'   'H.Actual'        '21' 'CURR',
    ' ' 'ASS_AMT'   'I.Assign(F+G+H)' '21' 'CURR',
    ' ' 'RES_AMT'   'J.Avail.(D-I)'   '21' 'CURR',
    ' ' 'DOW_AMT'   'Downpayment'     '21' 'CURR',
    ' ' 'DIFA_AMT'  'Org-Act'         '21' 'CURR',
    ' ' 'DIFB_AMT'  'Cur-Act'         '21' 'CURR',
    ' ' 'DPCR_AMT'  'Balance for DP'  '21' 'CURR',
    ' ' 'RATE'      '%Progress'       '21' 'CURR',
    ' ' 'ANLKL'     'Asset Class'     '8'  'CHAR',
    ' ' 'AKTIV'     'Cap.Date'        '8'  'DATS',
    ' ' 'ICON'      'Depr.'   '3'  'ICON'.
  ENDIF.

  LOOP AT gt_fcat INTO gs_fcat.
    IF gs_fcat-fieldname CP '*AMT' OR gs_fcat-fieldname CP '*PLAN'
    OR gs_fcat-fieldname CP '*RATE'.
*      gs_fcat-cfieldname = 'TWAER'.
      gs_fcat-just = 'R'.
      gs_fcat-no_zero = 'X'.
    ENDIF.

    gs_fcat-ref_table = 'ZSFI_IMFM'.
    gs_fcat-ref_field = gs_fieldcat-fieldname.
    MODIFY gt_fcat FROM gs_fcat.


  ENDLOOP.

ENDFORM.                    " create_field_category
*&---------------------------------------------------------------------*
*&      Form  sort_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_SORT[]  text
*----------------------------------------------------------------------*
FORM sort_build USING ft_sort TYPE lvc_t_sort.
  DEFINE sort_tab.
    clear gs_sort.
    gs_sort-fieldname = &1.
    gs_sort-spos      = &2.
    gs_sort-up        = &3.
    gs_sort-group     = &4.
    gs_sort-subtot    = &5.
    gs_sort-comp      = &6.
    append gs_sort to ft_sort.
  END-OF-DEFINITION.

  sort_tab :
             'POSID'        ' ' 'X' 'X' 'X' '',
             'TXT50'        ' ' 'X' 'X' 'X' '',
             'KOSTL'        ' ' 'X' 'X' 'X' ''.
ENDFORM.                    " sort_build
*&---------------------------------------------------------------------*
*&      Form  set_lvc_layout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_lvc_layout.
  CLEAR gs_layo.
  gs_layo-edit       = 'X'.
  gs_layo-zebra      = 'X'.
  gs_layo-sel_mode   = 'A'.       " Column and row selection
  gs_layo-cwidth_opt = 'X'.
  gs_layo-ctab_fname = 'TABCOLOR'.
  gs_layo-stylefname = 'CELLTAB'.

ENDFORM.                    " set_lvc_layout
*&---------------------------------------------------------------------*
*&      Form  set_color
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_color.

  CLEAR: gs_specialcol, gt_specialcol[], gt_out-tabcolor[].

  DEFINE __color.
    gs_specialcol-fieldname = &1 .
    gs_specialcol-color-col = &2 .
    gs_specialcol-color-int = &3 .
    append gs_specialcol to gt_specialcol .
  END-OF-DEFINITION.

  IF r_1 = 'X'.
    __color :
       'POSID'    '1' '0',
       'TXT50'    '1' '0',
       'KOSTL'    '2' '0',
       'CRTD'     '3' '0',
       'REL'      '3' '0',
       'LKD'      '3' '0',

       'ARPLAN'   '4' '0',
       'ZPLAN'    '4' '0',
       'ORG_AMT'  '4' '0',
       'SUP_AMT'  '4' '0',
       'RET_AMT'  '4' '0',
       'CUR_AMT'  '4' '0',

       'IO_AMT'   '5' '0',

       'PR_AMT'   '6' '0',
       'PO_AMT'   '6' '0',
       'ACT_AMT'  '6' '0',
       'ASS_AMT'  '6' '0',

       'RES_AMT'  '7' '0',
       'DOW_AMT'  '8' '0',
       'DIFA_AMT' '3' '0',
       'DIFB_AMT' '3' '0',
       'DPCR_AMT' '4' '0',
       'AR_RATE'  '2' '0',
       'PI_RATE'  '2' '0',
       'ICON'     '2' '0'.
  ELSE.
    __color :
      'POSID'     '1' '0',
      'TXT50'     '1' '0',
      'AUFNR'     '1' '0',
      'KOSTL'     '2' '0',
*      'CRTD'      '3' '0',
      'REL'       '3' '0',
      'LKD'       '3' '0',

      'ZPLAN'     '4' '0',
      'ORG_AMT'   '4' '0',
      'SUP_AMT'   '4' '0',
      'RET_AMT'   '4' '0',
      'CUR_AMT'   '4' '0',

      'IO_AMT'    '5' '0',

      'PR_AMT'    '6' '0',
      'PO_AMT'    '6' '0',
      'ACT_AMT'   '6' '0',
      'ASS_AMT'   '6' '0',

      'RES_AMT'   '7' '0',
      'DOW_AMT'   '8' '0',
      'DIFA_AMT'  '3' '0',
      'DIFB_AMT'  '3' '0',
      'DPCR_AMT'  '4' '0',
      'RATE'      '2' '0',
      'ICON'      '2' '0'.
  ENDIF.
  	

gt_out-tabcolor[] = gt_specialcol[].
  MODIFY gt_out TRANSPORTING tabcolor WHERE tabcolor IS initial.


ENDFORM.                    " set_color
*&---------------------------------------------------------------------*
*&      Form  data_changed
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ER_data_changed  text
*----------------------------------------------------------------------*
FORM data_changed USING rr_data_changed
                        TYPE REF TO cl_alv_changed_data_protocol.

  flag_data_changed = true.

  DATA: ls_mod_cells TYPE lvc_s_modi,
        ls_cells     TYPE lvc_s_modi,
        lt_values TYPE TABLE OF bapi_char_values WITH HEADER LINE.

  LOOP AT rr_data_changed->mt_good_cells INTO ls_mod_cells.
    READ TABLE gt_out INDEX ls_mod_cells-row_id.
    IF sy-subrc = 0.
      CALL METHOD rr_data_changed->modify_cell
                EXPORTING i_row_id    = ls_mod_cells-row_id
                          i_fieldname = ls_mod_cells-fieldname
                          i_value     = ls_mod_cells-value.
    ENDIF.
  ENDLOOP.

  __set_refresh_mode true.
  CALL METHOD g_grid->refresh_table_display
       EXPORTING is_stable = stable.

ENDFORM.                    " DATA_CHANGED
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  DATA $code TYPE syucomm.

  CLEAR : g_error.

  ok_code = sy-ucomm.
  CLEAR sy-ucomm.
  CASE ok_code.

    WHEN 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'SWITCH'.
      IF sy-dynnr EQ '0100'.
        PERFORM switch_edit_mode.
      ENDIF.
      __focus g_grid.

    WHEN 'REL' OR 'LOCK'.
      PERFORM get_selected_row.
      PERFORM : toggle_release_lock USING ok_code,
                refresh_alv.
      __focus g_grid.

    WHEN 'UREL' OR 'UNLK'.
      PERFORM get_selected_row.
      PERFORM : toggle_unrelease_unlock USING ok_code,
                refresh_alv.
      __focus g_grid.

    WHEN 'PRE' OR 'NEXT'.
      PERFORM get_selected_row.
      PERFORM : change_status USING ok_code,        " Change status
                refresh_alv.
      __focus g_grid.

    WHEN 'DPCR'.
      PERFORM get_selected_row.
      PERFORM : create_dpcr, " Create depreciation data
                refresh_alv.
      __focus g_grid.

    WHEN 'RSTD'.
      PERFORM get_selected_row.
      PERFORM : reset_dpcr,   " reset depreciation data
                refresh_alv.
      __focus g_grid.

    WHEN 'CAL1' OR 'CAL2' OR 'CAL3'.
      PERFORM get_selected_row.
      PERFORM : cal_dpcr_amt USING ok_code,
                refresh_alv.
      __focus g_grid.

    WHEN 'CAL'.
      PERFORM get_selected_row.

      CASE 'X'.
        WHEN p_io.
          $code = 'CAL1'.
        WHEN p_pi.
          $code = 'CAL2'.
        WHEN p_ar.
          $code = 'CAL3'.
      ENDCASE.

      PERFORM : cal_dpcr_amt USING $code,
                refresh_alv.
      __focus g_grid.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  switch_edit_mode
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM switch_edit_mode.

  DATA answer.
  IF g_grid->is_ready_for_input( ) EQ 0.
    CALL METHOD g_grid->set_ready_for_input
                     EXPORTING i_ready_for_input = 1.
    SET PF-STATUS '100'.
  ELSE.
    CALL METHOD g_grid->set_ready_for_input
                     EXPORTING i_ready_for_input = 0.
    SET PF-STATUS '100'.
  ENDIF.

  PERFORM build_cell_attr.

ENDFORM.                    " switch_edit_mode
*&---------------------------------------------------------------------*
*&      Form  build_cell_attr1_lock
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_cell_attr1_lock.

*  data: lt_celltab type lvc_t_styl,
*        ls_celltab type lvc_s_styl.
*
*  clear lt_celltab.
*  refresh lt_celltab.
*
*  __cls gt_out-celltab.
*  modify gt_out transporting celltab where belnr ne space.
*
*  clear gs_fcat.
*
*  loop at gt_fcat into gs_fcat.
*    ls_celltab-fieldname = gs_fcat1-fieldname.
*    ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
*    insert ls_celltab into table lt_celltab.
*  endloop.
*
*  insert lines of lt_celltab into table gt_out-celltab.
*  modify gt_out transporting celltab where belnr ne space.
*
ENDFORM.                    " build_cell_attr1_lock
*&---------------------------------------------------------------------*
*&      Form  refresh_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh_alv.
  __set_refresh_mode true.
  CALL METHOD g_grid->refresh_table_display
       EXPORTING is_stable = stable.

ENDFORM.                    " refresh_alv
*&---------------------------------------------------------------------*
*&      Form  get_selected_row
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_selected_row.

  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
        lt_row_no TYPE lvc_t_roid. "Numeric IDs of Selected Rows
  DATA: l_line TYPE i.

  CALL METHOD g_grid->get_selected_rows
           IMPORTING et_index_rows = lt_rows[]
                     et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    MESSAGE e000
    WITH 'Error founded during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000 WITH 'Please select a data.'.
    EXIT.
  ENDIF.

  PERFORM get_posting_data TABLES lt_rows
                                  lt_row_no .

ENDFORM.                    " get_selected_row
*&---------------------------------------------------------------------*
*&      Form  get_posting_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ROWS  text
*      -->P_LT_ROW_NO  text
*----------------------------------------------------------------------*
FORM get_posting_data TABLES pt_rows STRUCTURE lvc_s_row
                             pt_row_no STRUCTURE lvc_s_roid.

  CLEAR gt_out-chk.
  MODIFY gt_out TRANSPORTING chk WHERE chk = 'X'.

* Selected Row by row selection
  LOOP AT pt_rows WHERE rowtype IS initial.
    READ TABLE gt_out INDEX pt_rows-index.
    gt_out-chk = true .
    MODIFY gt_out INDEX pt_rows-index TRANSPORTING chk.
  ENDLOOP.

ENDFORM.                    " GET_POSTING_DATA
*&---------------------------------------------------------------------*
*&      Form  toggle_release
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM toggle_release_lock USING $code.

  DATA: l_okcode(5),
        l_chk VALUE 'X',
        l_stat.

  LOOP AT gt_out WHERE chk = 'X'.
    CLEAR l_okcode.

    IF $code EQ 'REL'.
      IF gt_out-rel EQ true.
*        if gt_out-lkd eq false.
*          l_okcode = '=BFRZ'.    " Cancel Release
*        else.
*          message s000 with
*          'Please unlock first before unrelease the record!'.
*          clear l_okcode.
*        endif.
      ELSE.
        IF gt_out-crtd EQ true.
          l_okcode = '=BFRE'.    " Release
        ELSE.
          MESSAGE s000 WITH
          'Please create first before release the record!'.
          CLEAR l_okcode.
        ENDIF.
      ENDIF.
    ELSEIF $code EQ 'LOCK'.
      IF gt_out-lkd EQ true.
*        l_okcode = '=BUNL'.    " Unlock
      ELSE.
        IF gt_out-rel EQ true.
          l_okcode = '=BLOC'.    " Lock
        ELSE.
          MESSAGE s000 WITH
          'Please release first before lock the record!'.
          CLEAR l_okcode.
        ENDIF.
      ENDIF.
    ENDIF.

    CHECK NOT l_okcode IS INITIAL.

    PERFORM bdc_im12 USING l_okcode.

    READ TABLE gt_msg WITH KEY msgtyp = 'S'
                               msgid = 'AP'
                               msgnr = '010'.

    IF sy-subrc = 0.
      CASE l_okcode.
        WHEN '=BFRZ'.
          gt_out-rel = false.
          gt_out-stat = '1'. " created
        WHEN '=BFRE'.
          gt_out-rel = true.
          gt_out-stat = '2'. " released
        WHEN '=BUNL'.
          gt_out-lkd = false.
          gt_out-stat = '2'. " released
        WHEN '=BLOC'.
          gt_out-lkd = true.
          gt_out-stat = '3'. " locked
      ENDCASE.

      MODIFY gt_out.
    ENDIF.

  ENDLOOP.


ENDFORM.                    " toggle_release
*&---------------------------------------------------------------------*
*&      Form  toggle_unrelease_unlock
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_OK_CODE  text
*----------------------------------------------------------------------*
FORM toggle_unrelease_unlock USING $code.

  DATA: l_okcode(5),
        l_chk VALUE 'X',
        l_stat.

  LOOP AT gt_out WHERE chk = 'X'.
    CLEAR l_okcode.

    IF $code EQ 'UREL'.
      IF gt_out-rel EQ true.
        IF gt_out-lkd EQ false.
          l_okcode = '=BFRZ'.    " Cancel Release
        ELSE.
          MESSAGE s000 WITH
          'Please unlock first before unrelease the record!'.
          CLEAR l_okcode.
        ENDIF.
      ELSE.
*        if gt_out-crtd eq true.
*          l_okcode = '=BFRE'.    " Release
*        else.
*          message s000 with
*          'Please create first before release the record!'.
*          clear l_okcode.
*        endif.
      ENDIF.
    ELSEIF $code EQ 'UNLK'.
      IF gt_out-lkd EQ true.
        l_okcode = '=BUNL'.    " Unlock
      ELSE.
*        if gt_out-rel eq true.
*          l_okcode = '=BLOC'.    " Lock
*        else.
*          message s000 with
*          'Please release first before lock the record!'.
*          clear l_okcode.
*        endif.
      ENDIF.
    ENDIF.

    CHECK NOT l_okcode IS INITIAL.

    PERFORM bdc_im12 USING l_okcode.

    READ TABLE gt_msg WITH KEY msgtyp = 'S'
                               msgid = 'AP'
                               msgnr = '010'.

    IF sy-subrc = 0.
      CASE l_okcode.
        WHEN '=BFRZ'.
          gt_out-rel = false.
          gt_out-stat = '1'. " created
        WHEN '=BFRE'.
          gt_out-rel = true.
          gt_out-stat = '2'. " released
        WHEN '=BUNL'.
          gt_out-lkd = false.
          gt_out-stat = '2'. " released
        WHEN '=BLOC'.
          gt_out-lkd = true.
          gt_out-stat = '3'. " locked
      ENDCASE.

      MODIFY gt_out.
    ENDIF.

  ENDLOOP.


ENDFORM.                    " toggle_release
*&---------------------------------------------------------------------*
*&      Form  double_click
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*      -->P_E_COLUMN  text
*      -->P_ES_ROW_NO  text
*----------------------------------------------------------------------*
FORM double_click USING  e_row     TYPE lvc_s_row
                         e_column  TYPE lvc_s_col
                         es_row_no TYPE lvc_s_roid.

  CLEAR gv_index.
  gv_index = e_row-index.

  READ TABLE gt_out INDEX gv_index.
  IF sy-subrc = 0.
    IF e_column = 'POSID'.
      SET PARAMETER ID 'IMT' FIELD s_prnam-low.
      SET PARAMETER ID 'IMP' FIELD gt_out-posid.
      SET PARAMETER ID 'GJR' FIELD p_ayear.
      CALL TRANSACTION 'ZIMR' AND SKIP FIRST SCREEN.
    ENDIF.
    IF e_column = 'DPCR_AMT'.
      SET PARAMETER ID 'IAF' FIELD gt_out-posid.
      CALL TRANSACTION 'IMA3N' AND SKIP FIRST SCREEN.
    ENDIF.
  ENDIF.

  CALL METHOD cl_gui_control=>set_focus EXPORTING control = g_grid.

ENDFORM.                    " DOUBLE_CLICK
*&---------------------------------------------------------------------*
*&      Form  get_status_all
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_BPGE_OBJNR  text
*      <--P_IT_SUM_STATUS  text
*----------------------------------------------------------------------*
FORM chk_status_all USING    p_objnr TYPE im_objnr
                  CHANGING p_stat
                           l_crtd
                           l_rel
                           l_lkd
                           p_chk.

  CLEAR p_chk.
  READ TABLE gt_status WITH KEY objnr = p_objnr
                                stat = 'I0001' BINARY SEARCH.
  IF sy-subrc = 0.
    l_crtd = 'X'.
    p_stat = 1.
  ENDIF.

  READ TABLE gt_status WITH KEY objnr = p_objnr
                                stat = 'I0002' BINARY SEARCH.
  IF sy-subrc = 0.
    l_crtd = 'X'.
    l_rel = 'X'.
    p_stat = 2.
  ENDIF.

  READ TABLE gt_status WITH KEY objnr = p_objnr
                                stat = 'I0043' BINARY SEARCH.
  IF sy-subrc = 0.
    l_crtd = 'X'.
    l_lkd = 'X'.
    p_stat = 3.
  ENDIF.

** Created
*  if p_crtd = 'X'.
*    if l_crtd = 'X'.
*      clear p_chk.
*    endif.
*  endif.
*
** Released
*  if p_rel = 'X'.
*    if l_rel = 'X'.
*      clear p_chk.
*    endif.
*  endif.
*
** Locked
*  if p_lkd = 'X'.
*    if l_lkd = 'X'.
*      clear p_chk.
*    endif.
*  endif.

ENDFORM.                    " get_status_all
*&---------------------------------------------------------------------*
*&      Form  reset_dpcr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reset_dpcr.
  DATA:
      lt_ania  LIKE rania OCCURS 0 WITH HEADER LINE,
      lt_anib  LIKE STANDARD TABLE OF ranib
                    INITIAL SIZE 10 WITH HEADER LINE.

  DATA: lt_variant    TYPE TABLE OF bapiappreqvarntassignmulti
                                    WITH HEADER LINE,
        lt_plan_tot   TYPE TABLE OF bapiappreqplantotalmulti
                                    WITH HEADER LINE,
        lt_version TYPE TABLE OF bapiappreqvarntassign
                                 WITH HEADER LINE,
      lt_plan_year  TYPE TABLE OF bapiappreqplanyear WITH HEADER LINE,
        lt_return     TYPE TABLE OF bapiret2 WITH HEADER LINE,
        wa_plan_total LIKE bapiappreqplantotal,
        l_index       TYPE sytabix,
        l_arvrnt      TYPE ima_varnt.

  CLEAR: l_index, l_arvrnt.

  LOOP AT gt_out WHERE chk = 'X'.

    l_index = sy-tabix.

*    SELECT SINGLE * FROM imavz
*        WHERE gjahr = p_yearp
*          AND posnr = gt_out-posnr
*          AND versi = p_darver.
*
*    CHECK sy-subrc = 0.
*
*    SELECT SINGLE * FROM imav
*       WHERE posnr = gt_out-posnr
*         AND varnt = imavz-varnt.
*
*    REFRESH: lt_ania, lt_anib.
*    SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_ania
*        FROM ania WHERE objnr = imav-objnr.
*
*    CHECK sy-subrc EQ 0.
*    SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_anib
*        FROM anib WHERE objnr = imav-objnr.
*
*    CHECK sy-subrc EQ 0.
*    lt_ania-kz    = 'D'.
*    lt_anib-kz    = 'D'.
*
*    MODIFY lt_ania TRANSPORTING kz WHERE objnr = imav-objnr.
*    MODIFY lt_anib TRANSPORTING kz WHERE objnr = imav-objnr.
*
*    CALL FUNCTION 'DEPREC_SIMUL_MODIFIKATION'
*         TABLES
*              t_ania = lt_ania
*              t_anib = lt_anib.



    CALL FUNCTION 'BAPI_APPREQUEST_GETDETAIL'
         EXPORTING
              externalnumber     = gt_out-posid
              language           = sy-langu
         TABLES
              variant_to_version = lt_variant
              plan_tot           = lt_plan_tot.

    READ TABLE lt_variant WITH KEY appr_year = p_yearp "p_ayear
                                   plan_version = p_darver. "p_arver.


    IF sy-subrc EQ 0.


      wa_plan_total-investment_costs = 0.

      CLEAR: lt_version, lt_plan_year, lt_return.
      REFRESH: lt_version, lt_plan_year, lt_return.


      lt_plan_year-fiscal_year    = p_yearp. "p_ayear.
      lt_plan_year-investment_costs = 0.
      APPEND lt_plan_year.

      CALL FUNCTION 'BAPI_APPREQUEST_SETPLANVALUES'
           EXPORTING
                externalnumber              = gt_out-posid
                appropriationrequestvariant = lt_variant-appreqvrnt
                plan_total                  = wa_plan_total
           TABLES
                plan_year                   = lt_plan_year
                return                      = lt_return.


***      CALL FUNCTION 'BAPI_APPREQUEST_UASSGNVRNTVRSN'
***        EXPORTING
***          externalnumber                    = gt_out-posid
***          appropriationrequestvariant       = lt_variant-appreqvrnt
***           TABLES
***                RETURN                      = lt_return          .
***
***      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
***
***      CALL FUNCTION 'BAPI_APPREQUEST_REMOVEVARIANT'
***           EXPORTING
***                externalnumber              = gt_out-posid
***                appropriationrequestvariant = lt_variant-appreqvrnt
***           TABLES
***                RETURN                      = lt_return          .

    ENDIF.

    IF sy-subrc EQ 0.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
      gt_out-icon = space.

      gt_out-anlkl = space.
      gt_out-aktiv = space.

      MODIFY gt_out INDEX l_index TRANSPORTING icon anlkl aktiv.
    ELSE.
      gt_out-icon = icon_led_red.
*     Rollback
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDIF.


  ENDLOOP.


ENDFORM.                    " reset_dpcr
*&---------------------------------------------------------------------*
*&      Form  SIMULATE_DEPR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_USR03  text
*----------------------------------------------------------------------*
FORM simulate_depr USING p_usr03
                         p_usr09
                         p_objnr
                         p_kokrs
                         p_bukrs
                         p_posnr.

  DATA:
    ls_rania LIKE rania,
    ls_anib  LIKE ranib,
    l_subrc  LIKE sy-subrc,
    l_nodep  LIKE sy-subrc.

  DATA:
        i_objnr LIKE  ania-objnr,
        i_anlkl LIKE  ania-anlkl,
        i_aktiv LIKE  ania-aktiv,
        i_bukrs LIKE  t001-bukrs,
        i_kokrs LIKE  tka01-kokrs,
        i_ivpro LIKE  taif1-ivpro,
        i_repid LIKE  sy-repid,
        i_dynnr LIKE  sy-dynnr,
        i_aktyp LIKE  t020-aktyp.

  DATA: it_afasplit LIKE afasplit OCCURS 0.

*asset class
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
            input  = p_usr03
       IMPORTING
            output = i_anlkl.
*cap.date
  i_aktiv = p_usr09.
  i_objnr = p_objnr.

*investment profile
  SELECT SINGLE ivpro INTO i_ivpro
     FROM taprf
     WHERE anlkl_s = i_anlkl.
  IF sy-subrc = 0.
    l_nodep = 0.
  ELSE.
* no depr.asset... delete..simulation data.
    l_nodep = 1.
  ENDIF.

  i_dynnr = sy-dynnr.
  i_repid = sy-repid.

  CLEAR: lt_anib, lt_anib1.
  REFRESH: lt_anib, lt_anib1.

  CALL FUNCTION 'AIPS_SIMUL_CHECK'
       EXPORTING
            i_kokrs             = p_kokrs
            i_bukrs             = p_bukrs
            i_anlkl             = i_anlkl
            i_aktiv             = i_aktiv
            i_objnr             = i_objnr
            i_ivpro             = i_ivpro
            i_dynnr             = i_dynnr
            i_repid             = i_repid
       IMPORTING
            es_ania             = ls_rania
            e_subrc             = l_subrc
       TABLES
            et_anib             = lt_anib
       EXCEPTIONS
            kein_bukrs          = 1
            bukrs_kein_am_bukrs = 2
            perioden_nicht_def  = 3.

  CHECK l_subrc IS INITIAL.

  CLEAR i_imakpa.
  REFRESH i_imakpa.

  SELECT * INTO TABLE i_imakpa
    FROM imakpa WHERE posnr = p_posnr.

  CLEAR lt_ania.
  REFRESH lt_ania.
  MOVE-CORRESPONDING ls_rania TO lt_ania.

*change user, date
  lt_ania-ernam = sy-uname.
  lt_ania-erdat = sy-datum.

  SELECT SINGLE * FROM ania WHERE objnr = i_objnr.

  IF sy-subrc = 0.
    PERFORM insert_appr_req USING i_objnr.

    PERFORM delete_appr_req USING i_objnr.

  ELSE.
    PERFORM insert_appr_req USING i_objnr.
  ENDIF.
ENDFORM.                    " SIMULATE_DEPR
*&---------------------------------------------------------------------*
*&      Form  INSERT_APPR_REQ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_OBJNR  text
*----------------------------------------------------------------------*
FORM insert_appr_req USING p_objnr TYPE j_objnr.
  lt_ania-kz = 'I'.
  PERFORM append_lt_ania USING 'I'.

  lt_anib-lfdnr = lt_ania-lfdnr.
  lt_anib-kz = 'I'.
  PERFORM append_lt_anib1 USING p_objnr.

ENDFORM.                    " INSERT_APPR_REQ
*&---------------------------------------------------------------------*
*&      Form  APPEND_LT_ANIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_5928   text
*----------------------------------------------------------------------*
FORM append_lt_ania USING p_kz.
  LOOP AT i_imakpa.
    IF p_kz = 'I'.
      IF ania IS INITIAL.
        CLEAR lt_ania-lfdnr.
      ELSE.
        lt_ania-lfdnr = ania-lfdnr + 1.
      ENDIF.

    ELSEIF p_kz = 'D'.
      MOVE-CORRESPONDING ania TO lt_ania.
    ENDIF.

    lt_ania-kostl = i_imakpa-akostl.
    lt_ania-aufpr = i_imakpa-aproz.
    APPEND lt_ania.
  ENDLOOP.

ENDFORM.                    " APPEND_LT_ANIA
*&---------------------------------------------------------------------*
*&      Form  DELETE_APPR_REQ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_OBJNR  text
*----------------------------------------------------------------------*
FORM delete_appr_req USING p_objnr TYPE j_objnr.
  lt_ania-kz = 'D'.
  PERFORM append_lt_ania USING 'D'.

  lt_anib-lfdnr = lt_ania-lfdnr.
  lt_anib-kz = 'D'.
  PERFORM append_lt_anib1 USING p_objnr.

ENDFORM.                    " DELETE_APPR_REQ
*&---------------------------------------------------------------------*
*&      Form  APPEND_LT_ANIB1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_OBJNR  text
*----------------------------------------------------------------------*
FORM append_lt_anib1 USING p_objnr TYPE j_objnr.
  MODIFY lt_anib TRANSPORTING lfdnr kz WHERE objnr = p_objnr.

  LOOP AT lt_anib.
    MOVE-CORRESPONDING lt_anib TO lt_anib1.
    APPEND lt_anib1.
    CLEAR lt_anib1.
  ENDLOOP.

ENDFORM.                    " APPEND_LT_ANIB1
