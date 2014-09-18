*&---------------------------------------------------------------------*
*&  Include           ZRHR_APP_MORNITORTOP
*&---------------------------------------------------------------------*
TYPE-POOLS: vrm.

DATA: ok_code                 TYPE sy-ucomm.

* alv variable definition
DATA: gr_cont                 TYPE REF TO cl_gui_custom_container,
      gr_grid                 TYPE REF TO cl_gui_alv_grid.
DATA: gs_layo                 TYPE lvc_s_layo.
DATA: gt_fcat                 TYPE lvc_t_fcat WITH HEADER LINE.

DATA: gt_values     TYPE vrm_values,
      gs_value      LIKE LINE OF gt_values,
      g_fieldname   TYPE vrm_id.

DATA: BEGIN OF gt_result OCCURS 0,
        ap_start_date         TYPE hap_ap_start_date,       " Duration(From)
        ap_end_date           TYPE hap_ap_end_date,         " Duration(To)
        appraisee_id          TYPE hap_appraisee_id,        " Team Member ID
        vorna                 TYPE p0002-vorna,             " Team Member First Name
        nachn                 TYPE p0002-nachn,             " Team Member Last Name
        ap_status             TYPE hap_ap_status,           " Status
        ap_status_name        TYPE hap_ap_status_name,      " Status Text
        ap_status_sub         TYPE hap_ap_status_sub,       " SubStatus
        ap_status_sub_name    TYPE hap_ap_status_sub_name,  " SubStatus Text
        appraiser_id          TYPE hap_appraiser_id,        " 1st Evaluator ID
        appraiser_name        TYPE hap_appraiser_name,      " 1st Evaluator Name
        cor01                 TYPE hap_other_id,            " 1st Coordinator ID
        cornm01               TYPE emnam,                   " 1st Coordinator Name
        eva02                 TYPE hap_other_id,            " 2nd Evaluator ID
        evanm02               TYPE emnam,                   " 2nd Evaluator Name
        cor02                 TYPE hap_other_id,            " 2nd Coordinator ID
        cornm02               TYPE emnam,                   " 2nd Coordinator Name
        eva03                 TYPE hap_other_id,            " 3rd Evaluator ID
        evanm03               TYPE emnam,                   " 3rd Evaluator Name
        cor03                 TYPE hap_other_id,            " 3rd Coordinator ID
        cornm03               TYPE emnam,                   " 3rd Coordinator Name
        apprv                 TYPE hap_other_id,            " Approver ID
        apprvnm               TYPE emnam,                   " Approver Name
        cor04                 TYPE hap_other_id,            " 4th Coordinator ID
        cornm04               TYPE emnam,                   " 4th Coordinator Name
        hrteam                TYPE hap_other_id,            " HR Team ID
        hrteamnm              TYPE emnam,                   " HR Team Name
      END OF gt_result.

*&********************************************************************
*    Selection Screen
*&********************************************************************
SELECTION-SCREEN BEGIN OF SCREEN 200 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t80.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 2(10) text-t81 FOR FIELD p_year.
PARAMETERS: p_year  TYPE zdhr_year AS LISTBOX VISIBLE LENGTH 10.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 2(10) text-t82 FOR FIELD p_st.
PARAMETERS: p_st    TYPE hap_ap_status AS LISTBOX VISIBLE LENGTH 20 USER-COMMAND st.
SELECTION-SCREEN COMMENT 45(10) text-t83 FOR FIELD p_subst.
PARAMETERS: p_subst TYPE hap_ap_status_sub AS LISTBOX VISIBLE LENGTH 25.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN END OF SCREEN 200 .
