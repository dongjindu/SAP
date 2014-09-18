*&---------------------------------------------------------------------*
*&  Include           ZRDA_QA_DOC_ARCHR_TOP
*&---------------------------------------------------------------------*

TABLES: ztda_posts, ztda_postmeta,   ztda_term_rel, ztda_meta_name,
        ztda_term_taxon, ztda_terms, ztsd_acm_h, ztpp_vm,
        zvda_arch01, lfa1.

* Screen buff
DATA  : g_aclnoonly        TYPE c,
        g_acl_no           TYPE zacln,
        g_vin_no           TYPE ze_vin,
        g_title(200)       TYPE c,
        g_lname            TYPE zedalname,
        g_fname            TYPE zedafname,
        g_year             type gjahr,
        g_month            TYPE spmon,
        g_cmonth           TYPE spmon,
        g_rmonth           TYPE spmon,
        g_link             TYPE c,
        g_first            TYPE c,
        g_lifnr            TYPE lifnr,
        g_name1            TYPE name1_gp,
        g_claimtitle(12).

DATA  : BEGIN OF gt_data OCCURS 0.
        INCLUDE STRUCTURE zsda_arch03l.
DATA  : post_desc          TYPE zemetadesc,  " post title
        post_desc_c        TYPE zemetadesc,  " post title
        meta_value_c       TYPE zemetaval.   " vin number
DATA  : END   OF gt_data.
DATA  : gs_data            LIKE LINE OF gt_data.

DATA  : BEGIN OF it_data OCCURS 0.
        INCLUDE STRUCTURE zsda_arch03l.
DATA  : style              TYPE lvc_t_styl,   " cell style   stylefname
        color              TYPE lvc_t_scol,   " cell color   ctab_fname
        del                type c.
DATA  : END   OF it_data.
DATA  : is_data            LIKE LINE OF it_data,
        is_data_o          like line of it_data.

DATA: BEGIN OF gt_dele OCCURS 0.
        INCLUDE STRUCTURE zvda_arch01.
DATA  END   OF gt_dele.

DATA  : BEGIN OF is_g,
          aclno            TYPE zacln,
          vinno            TYPE ze_vin,
          cdate            TYPE dats,
          rdate            TYPE dats,
          err              TYPE c,
        END   OF is_g,
        BEGIN OF is_key,
          acl              TYPE c,
          vin              TYPE c,
          name             TYPE c,
          date             TYPE c,
        END   OF is_key.



DATA  : BEGIN OF is_buyback,
          aclonly          TYPE c,
          acl_no           TYPE zacln,
          vin_no           TYPE ze_vin,
          title(200)       TYPE c,
        END   OF is_buyback,
        is_buyback_s       LIKE is_buyback,

        BEGIN OF is_legal,
          aclonly          TYPE c,
          acl_no           TYPE zacln,
          vin_no           TYPE ze_vin,
          last_name        TYPE zemetaval,
          first_name       TYPE zemetaval,
          title(200)       TYPE c,
        END   OF is_legal,
        is_legal_s         LIKE is_legal,

        BEGIN OF is_claim,
          year_month       TYPE spmon,
          title(200)       TYPE c,
        END   OF is_claim.
DATA  : is_claim_s         LIKE is_claim.


DATA  : BEGIN OF is_ztda_posts.
        INCLUDE STRUCTURE ztda_posts.
DATA  : END   OF is_ztda_posts.


DATA  : BEGIN OF it_ztda_postmeta OCCURS 0.
        INCLUDE STRUCTURE ztda_postmeta.
DATA  : END   OF it_ztda_postmeta.


DATA  : BEGIN OF is_ztda_meta_name.
        INCLUDE STRUCTURE ztda_meta_name.
DATA  : END   OF is_ztda_meta_name.


DATA  : BEGIN OF is_ztda_term_rel.
        INCLUDE STRUCTURE ztda_term_rel.
DATA  : END   OF is_ztda_term_rel.


DATA  : BEGIN OF is_ztda_term_taxon.
        INCLUDE STRUCTURE ztda_term_taxon.
DATA  : END   OF is_ztda_term_taxon.
DATA  : cb1          TYPE c,
        cb2          TYPE c,
        cb3          TYPE c,
        cb4          TYPE c.


DATA  : g_post_id     TYPE zepostid,
        g_fldname(40) TYPE c,
        g_meta_id     TYPE zemetaid,
        g_aclno_o     TYPE zacln,
        g_postid_o    TYPE zepostid,
        g_title_o     TYPE zdl_post_title,
        g_meta_key    TYPE zemetakey,
        g_date        TYPE sydatum,
        g_date_tm     TYPE syuzeit,
        gv_msg(80)    type c,
        g_only,
        g_flg1,

        g_flg,
        g_proc        TYPE c,
        g_af          TYPE c,
        g_tid(5)      TYPE n.

DATA  : ok_code       LIKE sy-ucomm,
        save_ok       LIKE sy-ucomm,
        w_repid       LIKE sy-repid,
        w_cnt         TYPE   i.
*****************************************************************
DATA  : BEGIN OF last_document,
        archiv_id     LIKE toav0-archiv_id,
        arc_doc_id    LIKE toav0-arc_doc_id,
        doc_type      LIKE toadv-doc_type,
        END OF last_document.

DATA  : g_object_id   LIKE sapb-sapobjid.
*****************************************************************


CONSTANTS: c_buyback  TYPE string VALUE `Buyback data input`,
           c_legal    TYPE string VALUE `Legal data input`,
           c_claim    TYPE string VALUE `Claim data input`,
           c_rclaim   TYPE string VALUE `Re-Claim data input`,
           gc_vinno   TYPE string VALUE 'VIN_NO',
           gc_name    TYPE string VALUE 'NAME',
           gc_date    TYPE string VALUE 'DATE',
           gc_only    TYPE string VALUE 'ACL_NO',
           gc_claim   type string value 'Claim Year',
           gc_reclaim type string value 'Reclaim Year',
           gc_agree   TYPE string VALUE 'AGREEMENT',
           gc_meeting TYPE string VALUE 'MEETING',
           gc_report  TYPE string VALUE 'REPORT',
           gc_object  TYPE string VALUE 'OBJECTION',
           gc_a       TYPE string VALUE 'A',
           gc_e       TYPE string VALUE 'E'
           .

**************************************************************
** For ALV list
**************************************************************
TYPE-POOLS: slis.

DATA: gt_list_top_of_page TYPE slis_t_listheader. " value 'TOP_OF_PAGE'.

DATA: g_repid        TYPE sy-repid,
      gt_fieldcat    TYPE slis_t_fieldcat_alv,
      gs_fieldcat    LIKE LINE OF gt_fieldcat,
      gs_layout      TYPE slis_layout_alv,
      gt_sort        TYPE slis_t_sortinfo_alv WITH HEADER LINE,
      gs_variant     LIKE disvariant,            "VARIANT
      gt_event       TYPE slis_t_event,
      gs_print       TYPE slis_print_alv,
      gs_keyinfo     TYPE slis_keyinfo_alv,
      g_status       TYPE slis_formname VALUE 'STATUS_SET',
      g_top_of_page  TYPE slis_formname VALUE 'TOP_OF_THE_PAGE',
      gt_extab       TYPE slis_t_extab,
      g_user_command TYPE slis_formname VALUE 'USER_COMMAND',

      g_pos          TYPE i.


**************************************************************
** ALV Screen
**************************************************************
TYPE-POOLS icon.
TABLES sscrfields.
DATA functxt TYPE smp_dyntxt.

DATA ct_0215         TYPE REF TO cl_gui_custom_container.
DATA ct_0225         TYPE REF TO cl_gui_custom_container.
DATA ct_0235         TYPE REF TO cl_gui_custom_container.
DATA ct_0255         TYPE REF TO cl_gui_custom_container.

DATA g_grid1         TYPE REF TO cl_gui_alv_grid.
DATA g_grid2         TYPE REF TO cl_gui_alv_grid.

DATA: gt_fieldcat1    TYPE lvc_t_fcat,
      gs_fieldcat1    TYPE lvc_s_fcat.
DATA: gt_fieldcat2    TYPE lvc_t_fcat,
      gs_fieldcat2    TYPE lvc_s_fcat.
DATA: gt_sort1        TYPE lvc_t_sort,
      gs_sort1        TYPE lvc_s_sort.
DATA: gt_sort2        TYPE lvc_t_sort,
      gs_sort2        TYPE lvc_s_sort.
DATA  gs_layout1      TYPE lvc_s_layo.
DATA  gs_layout2      TYPE lvc_s_layo.
DATA  g_colpos1       TYPE i.
DATA  g_colpos2       TYPE i.
DATA  gs_variant1     TYPE disvariant.
DATA  gs_variant2     TYPE disvariant.
DATA  gs_stable1      TYPE lvc_s_stbl.
DATA  gs_stable2      TYPE lvc_s_stbl.
DATA  gt_exclude1     TYPE ui_functions.
DATA  gs_exclude1     TYPE ui_func.

FIELD-SYMBOLS:
      <fs_fcat>       TYPE lvc_t_fcat,
      <fs_celltab>    TYPE lvc_t_styl.

DATA: g_pointer       TYPE REF TO data.

*----ALV
DATA gs_stable        TYPE lvc_s_stbl.

DATA: g_okcode        TYPE sy-ucomm,
      ok_code_2       TYPE sy-ucomm.

DATA: g_valid .
DATA  g_change.
DATA  g_answer.

DATA: gt_mod_cells    TYPE lvc_t_modi WITH HEADER LINE,
      g_error_flag.

*********************************************************************
**** Constant ***********************
*********************************************************************
CONSTANTS:
      c_icon_okay     TYPE iconname   VALUE 'ICON_OKAY',
      c_icon_no       TYPE iconname   VALUE 'ICON_NO',
      c_icon_cancel   TYPE iconname   VALUE 'ICON_CANCEL',
      c_gs_0100(08)                   VALUE 'GS_0100-',
      c_1                             VALUE '1',
      c_2                             VALUE '2',
      c_x                             VALUE 'X'.

CLASS: lcl_event1 DEFINITION DEFERRED.
*CLASS: lcl_event2 DEFINITION DEFERRED.
DATA : g_event1       TYPE REF TO lcl_event1.
DATA : g_event2       TYPE REF TO lcl_event1.


*********************************************************************
**** Variant Definition ****
*********************************************************************
DATA: g_total           TYPE i,
      g_kia             TYPE i,
      g_auc             TYPE i,
      g_match           TYPE i,
      g_kiaon           TYPE i,
      g_aucon           TYPE i.

DATA : gt_selected_rows TYPE lvc_t_roid,
       gs_selected_row  TYPE lvc_s_roid.

DATA : gv_row           TYPE i,
       gv_value(30)     TYPE c,
       gv_col           TYPE i,
       gs_row_id        TYPE lvc_s_row,
       gs_col_id        TYPE lvc_s_col,
       gs_row_no        TYPE lvc_s_roid,
       gv_input         TYPE i.

*----------------------------------------------------------------------*
*       CLASS lcl_event1 DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event1 DEFINITION.

  PUBLIC SECTION.
    METHODS :

*-- Uush Button
              handle_button_click FOR EVENT button_click OF cl_gui_alv_grid
                 IMPORTING  es_col_id   es_row_no,

*--Search Help
    handle_on_f4 FOR EVENT onf4 OF cl_gui_alv_grid
    IMPORTING sender
              e_fieldname
              e_fieldvalue
              es_row_no
              er_event_data
              et_bad_cells
              e_display,
*
**--Controlling data changes when ALV Grid is editable
   handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
                       IMPORTING er_data_changed
                                 e_onf4 e_ucomm,

*--Controlling data changes finished when ALV Grid is editable ?
   handle_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
                                IMPORTING e_modified et_good_cells,

*--Double Click event
   handle_double_click FOR EVENT double_click OF cl_gui_alv_grid
                       IMPORTING e_row e_column es_row_no,

*--To add new functional buttons to the ALV toolbar
   handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
                  IMPORTING e_object,

*--To implement user commands
   handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
                       IMPORTING e_ucomm.

ENDCLASS.                    "lcl_event1 DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_event1 IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event1 IMPLEMENTATION.

*--Search Help
  METHOD handle_button_click.

    DATA : ls_data LIKE it_data.

*    READ TABLE IT_DATA INTO LS_DATA INDEX ES_ROW_NO-ROW_ID.
*    CLEAR LS_DATA-BEDAT.
*    MODIFY IT_DATA  FROM LS_DATA    INDEX ES_ROW_NO-ROW_ID.

    CALL METHOD g_grid1->refresh_table_display
      EXPORTING
        is_stable = gs_stable1.         " Refresh
*    PERFORM button_click USING "es_col_id
*                               es_row_no.
  ENDMETHOD.                    "handle_button_click

  METHOD handle_on_f4 .
    PERFORM handle_f4  USING sender
                             e_fieldname
                             e_fieldvalue
                             es_row_no
                             er_event_data
                             et_bad_cells
                             e_display.

*    PERFORM F4_HELP_LOCAT USING E_FIELDNAME ES_ROW_NO ER_EVENT_DATA.

*    PERFORM F4_HELP USING E_FIELDNAME ES_ROW_NO ER_EVENT_DATA.

  ENDMETHOD.                    "handle_on_f4
*--Handle Data Changed
  METHOD handle_data_changed .
    PERFORM data_changed  USING er_data_changed
                                e_onf4  e_ucomm.
  ENDMETHOD.                    "handle_data_changed

*--Handle Data Changed Finished
  METHOD handle_data_changed_finished.
    PERFORM data_changed_finished  USING e_modified
                                         et_good_cells.
  ENDMETHOD.                    "handle_data_changed

*--Handle Double Click
  METHOD handle_double_click .
    PERFORM handle_double_click USING e_row e_column.
  ENDMETHOD.                    "handle_double_click

  METHOD handle_toolbar.
    PERFORM handle_toolbar1 USING e_object.
  ENDMETHOD.                    "handle_toolbar

  METHOD handle_user_command.
    PERFORM handle_user_command USING e_ucomm.
  ENDMETHOD.                    "handle_double_click

ENDCLASS.                    "lcl_event1 IMPLEMENTATION
