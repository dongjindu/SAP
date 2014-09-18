*&------------------------------------------------------------------
*& Program ID     : ZMMITOP01
*& Profram Name   : MMPM Global TOP include
*& Created by     : Yang
*& Created on     : 25.02.2008
*& Development ID :
*& Reference Pgm. :
*& Description    :
*&
*& Modification Log
*&====================================================================
*& Date     Developer      Request ID      Description
*&
*&--------------------------------------------------------------------
*&---------------------------------------------------------------------*
*&  Include           ZMMITOP01                                        *
*&---------------------------------------------------------------------*
**** TYPE-POOLS ****
TYPE-POOLS : slis, kkblo.

**** ALV ****
DATA : it_fieldcat     TYPE slis_t_fieldcat_alv  ,
       it_fieldcat2    TYPE slis_t_fieldcat_alv  .
DATA : is_layout       TYPE slis_layout_alv,
       is_layout2      TYPE slis_layout_alv.
DATA : i_grid_settings TYPE lvc_s_glay.
DATA : alv_header      TYPE slis_t_listheader.

DATA: g_lights_fieldname  TYPE slis_fieldname VALUE 'LIGHTS',
      gs_light            TYPE lvc_s_layo.

DATA : g_repid         TYPE sy-repid.
DATA : g_status        TYPE slis_formname VALUE 'ALV_STATUS'.
DATA : ga_status       TYPE slis_formname VALUE 'ALV_STATUS_A'.
DATA : gc_status       TYPE slis_formname VALUE 'ALV_STATUS_C'.
DATA : g_user_command  TYPE slis_formname VALUE 'ALV_COMMAND'.
DATA : gd_user_command TYPE slis_formname VALUE 'ALV_COMMAND_D'.
DATA : g_background_id TYPE bapibds01-objkey VALUE 'ALV_BACKGROUND'.
DATA : it_events       TYPE slis_t_event.
DATA : it_events1      TYPE slis_t_event.
DATA : it_events2      TYPE slis_t_event.
DATA : it_event_exit   TYPE slis_t_event_exit.
DATA : it_sort         TYPE slis_t_sortinfo_alv.
DATA : is_print        TYPE slis_print_alv.
DATA : g_top_of_page   TYPE slis_formname VALUE 'TOP_OF_PAGE',
       gt_list_top_of_page  TYPE slis_t_listheader,  "ALV HEADER
       gt_list_top_of_page2 TYPE slis_t_listheader.  "ALV HEADER
DATA : i_grid_title    TYPE lvc_title.
DATA : is_keyinfo      TYPE slis_keyinfo_alv.

**** SCREEN 100  VARIANTS ****
DATA : ok_code LIKE sy-ucomm.
DATA : g_mode VALUE 'M'.  "D = display, M = modify
DATA : g_loopc LIKE sy-loopc.
CONTROLS: tc_100 TYPE TABLEVIEW USING SCREEN 100.

**** GUI ALV ****
DATA : g_custom_container1 TYPE REF TO cl_gui_custom_container,
       g_custom_container2 TYPE REF TO cl_gui_custom_container,
       g_container1        TYPE scrfname VALUE 'CC_100',
       g_container2        TYPE scrfname VALUE 'CC_200',
       g_grid1             TYPE REF TO cl_gui_alv_grid,
       g_grid2             TYPE REF TO cl_gui_alv_grid,
       g_grid3             TYPE REF TO cl_gui_alv_grid,
       split_container     TYPE REF TO cl_gui_splitter_container,
       block1_container    TYPE REF TO cl_gui_container,
       block2_container    TYPE REF TO cl_gui_container,
       result              TYPE i.

DATA : gt_exclude          TYPE ui_functions.
data : gt_sort             type LVC_T_SORT.
DATA : it_fieldcatalog     TYPE lvc_t_fcat.

DATA : gs_fcatlayo         TYPE lvc_s_layo.
* data for event handling
DATA : gs_f4              TYPE lvc_s_f4.        "F4
DATA : gt_f4              TYPE lvc_t_f4.

DATA : lt_rows            TYPE lvc_t_row.     " ALV table line
DATA : g_selected_row     LIKE lvc_s_row.

DATA : g_dynnr            TYPE sy-dynnr.
DATA : g_sub_scrno        TYPE sy-dynnr   VALUE '100'.
*---------------------------------------------------*
* BAPI data
*---------------------------------------------------*
*# GR
DATA : goodsmvt_header       LIKE bapi2017_gm_head_01,
       goodsmvt_code         LIKE bapi2017_gm_code,
       testrun               LIKE bapi2017_gm_gen-testrun,
       goodsmvt_headret      LIKE bapi2017_gm_head_ret,
       materialdocument      TYPE bapi2017_gm_head_ret-mat_doc,
       matdocumentyear       TYPE bapi2017_gm_head_ret-doc_year,
       goodsmvt_item         LIKE TABLE OF bapi2017_gm_item_create
                                  WITH HEADER LINE,
       goodsmvt_serialnumber LIKE TABLE OF bapi2017_gm_serialnumber
                                  WITH HEADER LINE,
       goodsmvt_return       LIKE TABLE OF bapiret2
                                  WITH HEADER LINE.
*---------------------------------------------------*
* BDC
*---------------------------------------------------*
DATA: BEGIN OF bdcdata OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF bdcdata.
DATA : it_message   TYPE TABLE OF bdcmsgcoll  WITH HEADER LINE.
DATA : bdc_mode(1)  VALUE 'N'.
DATA : ctu_params TYPE ctu_params.
*---------------------------------------------------*
* MENU CONTROL
*---------------------------------------------------*
DATA : BEGIN OF it_menu OCCURS 0,
        ucomm LIKE sy-ucomm,
       END OF it_menu.

*---------------------------------------------------*
* SMARTFORM PRINT CONTROL
*---------------------------------------------------*
DATA : fm_name       TYPE rs38l_fnam,
       formname      TYPE tdsfname.   "form name

DATA : g_control     TYPE ssfctrlop,  "Smart Forms Control Structure
       g_options     TYPE ssfcompop,  "Smart Forms Composer Options
       g_output      TYPE ssfcresop,  "Smart Forms Return Value.
       g_usersetting TYPE tdbool.     "Yes or No.

*---------------------------------------------------*
* Describe type pools
*---------------------------------------------------*
TYPE-POOLS : sydes.
DATA : it_sydes   TYPE sydes_desc,
       it_types   TYPE sydes_typeinfo,
       it_names   TYPE sydes_nameinfo.
