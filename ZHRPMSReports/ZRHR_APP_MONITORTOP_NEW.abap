*&---------------------------------------------------------------------*
*&  Include           ZRHR_APP_MORNITORTOP
*&---------------------------------------------------------------------*
type-pools: vrm.

data: begin of gt_notif occurs 0,
        receiver              type hap_appraisee_id,        " E-mail Receiver
        name                  type emnam,                   " Full Name
        email                 type pa0105-usrid_long,       " E-mail Address
        appraisee_id          type hap_appraisee_id,        " Team Member ID
        vorna                 type p0002-vorna,             " Team Member First Name
        nachn                 type p0002-nachn,             " Team Member Last Name
      end of gt_notif.

data: begin of gt_duplicate occurs 0,
        appraisee_id          type hap_appraisee_id,        " Team Member ID
        vorna                 type p0002-vorna,             " Team Member First Name
        nachn                 type p0002-nachn,             " Team Member Last Name
      end of gt_duplicate.

data: g_duedate type zthr_evapd-ap_end_date,
      g_spelldate(50) type c,
      gt_subst  type vrm_values with header line,
      gt_st  type vrm_values with header line,
      gt_month_names type t247 occurs 0 with header line.

data: p_st_text like gt_st-text,
      p_subst_text like gt_subst-text.

data: ok_code                 type sy-ucomm.

* alv variable definition
data: gr_cont                 type ref to cl_gui_custom_container,
      gr_grid                 type ref to cl_gui_alv_grid.
data: gs_layo                 type lvc_s_layo.
data: gt_fcat                 type lvc_t_fcat with header line.

data: gt_values     type vrm_values,
      gs_value      like line of gt_values,
      g_fieldname   type vrm_id.

data: begin of gt_result occurs 0,
        ap_start_date         type hap_ap_start_date,       " Duration(From)
        ap_end_date           type hap_ap_end_date,         " Duration(To)
        appraisee_id          type hap_appraisee_id,        " Team Member ID
        vorna                 type p0002-vorna,             " Team Member First Name
        nachn                 type p0002-nachn,             " Team Member Last Name
        orgeh                 type p0001-orgeh,             " Org. Unit
        stext                 type hrp1000-stext,           " Org. Unit Description
        ap_status             type hap_ap_status,           " Status
        ap_status_name        type hap_ap_status_name,      " Status Text
        ap_status_sub         type hap_ap_status_sub,       " SubStatus
        ap_status_sub_name    type hap_ap_status_sub_name,  " SubStatus Text
        count                 type i,                       " Count
        appraiser_id          type hap_appraiser_id,        " 1st Evaluator ID
        appraiser_name        type hap_appraiser_name,      " 1st Evaluator Name
        cor01                 type hap_other_id,            " 1st Coordinator ID
        cornm01               type emnam,                   " 1st Coordinator Name
        eva02                 type hap_other_id,            " 2nd Evaluator ID
        evanm02               type emnam,                   " 2nd Evaluator Name
        cor02                 type hap_other_id,            " 2nd Coordinator ID
        cornm02               type emnam,                   " 2nd Coordinator Name
        eva03                 type hap_other_id,            " 3rd Evaluator ID
        evanm03               type emnam,                   " 3rd Evaluator Name
        cor03                 type hap_other_id,            " 3rd Coordinator ID
        cornm03               type emnam,                   " 3rd Coordinator Name
        apprv                 type hap_other_id,            " Approver ID
        apprvnm               type emnam,                   " Approver Name
        cor04                 type hap_other_id,            " 4th Coordinator ID
        cornm04               type emnam,                   " 4th Coordinator Name
        hrteam                type hap_other_id,            " HR Team ID
        hrteamnm              type emnam,                   " HR Team Name
        change_date           type hap_change_date,         " Change Date
        kostl                 type kostl,                   " Coster Center
        ktext                 type hrp1000-stext,           " Coster Center Description
* Changed on 11/14/15
        CLFTX                 type ZDHR_CLFTX,              " classification
      end of gt_result.

*&********************************************************************
*    Selection Screen
*&********************************************************************
selection-screen begin of block b2 with frame title text-t80.
selection-screen begin of line.
selection-screen comment 2(10) text-t81 for field p_year2.
parameters: p_year2  type zdhr_year as listbox visible length 10.
selection-screen end of line.
selection-screen begin of line.
selection-screen comment 2(10) text-t82 for field p_st2.
parameters: p_st2    type hap_ap_status as listbox visible length 20 user-command st.
selection-screen comment 45(10) text-t83 for field p_subst2.
parameters: p_subst2 type hap_ap_status_sub as listbox visible length 25.
selection-screen end of line.
selection-screen skip.
parameter: r_disp radiobutton group opt default 'X',
           r_mail radiobutton group opt.
selection-screen end of block b2.

selection-screen begin of screen 200 as subscreen.
selection-screen begin of block b1 with frame title text-t80.
selection-screen begin of line.
selection-screen comment 2(10) text-t81 for field p_year.
parameters: p_year  type zdhr_year as listbox visible length 10 modif id alv.
selection-screen end of line.
selection-screen begin of line.
selection-screen comment 2(10) text-t82 for field p_st.
parameters: p_st    type hap_ap_status as listbox visible length 20 user-command st modif id alv.
selection-screen comment 45(10) text-t83 for field p_subst..
parameters: p_subst type hap_ap_status_sub as listbox visible length 25 modif id alv.
selection-screen end of line.
selection-screen end of block b1.
selection-screen end of screen 200 .
