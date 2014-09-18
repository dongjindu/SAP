*&---------------------------------------------------------------------*
*&  Include           ZSAPBF_CONF_GO_TOP
*&---------------------------------------------------------------------*
TYPE-POOLS: ppcpr.

TABLES: ppc_act,
        ppc_act_reproc,
        ppc_bw_extr_cust,
        ppc_comat,
        ppc_conf_act,
        ppc_conf_act_var,
        ppc_conf_mat,
        ppc_conf_mat_var,
        ppc_head,
        ppc_mat,
        ppc_mat_det,
        ppc_ord_inf,
        ppc_profile,
        ppc_profile_t,
        ppc_res_hdr,
        ppc_rp,
        ppc_rp_vers,
        ppc_rp_vers_dat,
        ppc_scrap,
        ppc_step2,
        mara,
        marc,
        sscrfields. " to activate additional pushbuttons

TYPES: tt_ui_maint TYPE STANDARD TABLE OF zsapbf_ui_maint.


TYPES: BEGIN OF tt_plt_para,
         check,
         mark,
*        INCLUDE STRUCTURE zsapbf_plt_para.
         mandt  TYPE mandt,
         tran_code  TYPE zsapbf_lock_tran,
         werks  TYPE werks_d,
         classname  TYPE wsl_ctrl-serv_grp,
         wp_quota  TYPE zsapbf_ppc_wps_req,
         aedat  TYPE aedat,
         aezet  TYPE aezet,
         aenam  TYPE spcpcmname.
TYPES: END OF tt_plt_para.

DATA: ls_plt_para TYPE tt_plt_para.
DATA: it_plt_para TYPE STANDARD TABLE OF tt_plt_para WITH HEADER LINE.
DATA it_rzllitab TYPE STANDARD TABLE OF rzllitab WITH HEADER LINE.

DATA:    BEGIN OF it_f4hlp OCCURS 1.
        INCLUDE STRUCTURE dynpread.
DATA:    END OF it_f4hlp.

DATA:     BEGIN    OF        it_help_vtab       OCCURS 0.
INCLUDE  STRUCTURE help_vtab.
DATA:     END      OF        it_help_vtab.

DATA:     BEGIN    OF        it_help_value      OCCURS 0.
INCLUDE  STRUCTURE help_value.
DATA:     END      OF        it_help_value.

DATA:     BEGIN    OF        it_valuetab          OCCURS 0,
          value              LIKE  help_vtab-value,
          END      OF        it_valuetab.

DATA:     BEGIN    OF        it_dynpfields        OCCURS 5.
INCLUDE  STRUCTURE dynpread.
DATA:     END      OF        it_dynpfields.
DATA:     g_sel_index             TYPE  sytabix.

CONTROLS: tc_0100  TYPE TABLEVIEW USING SCREEN 0100.
DATA: ok_code LIKE sy-ucomm,
      save_ok LIKE sy-ucomm.

DATA: ls_ui_option  TYPE zsapbf_ui_option,
      lt_ui_options TYPE STANDARD TABLE OF zsapbf_ui_option.

DATA: lv_dynnr     TYPE sy-dynnr,
      lv_activetab TYPE sy-ucomm.

DATA: lv_flag      TYPE c VALUE IS INITIAL.
DATA: lv_clear_rpt TYPE c VALUE IS INITIAL.
DATA: lt_ui_maint  TYPE tt_ui_maint.
DATA: lt_plant_para_setting TYPE zsapbf_tt_plant_para_setting.

DATA: gv_tran_code TYPE zsapbf_lock_tran.
CONSTANTS:
        lc_0         TYPE c        VALUE '0',
        lc_1         TYPE c        VALUE '1',
        lc_2         TYPE c        VALUE '2',
        lc_3         TYPE c        VALUE '3',
        lc_x         TYPE c        VALUE 'X',
        lc_p         TYPE c        VALUE 'P',
        lc_s         TYPE c        VALUE 'S',
        lc_a         TYPE c        VALUE 'A',
        lc_sins_rpt  TYPE progname VALUE 'ZSAPBF_CONF_GO_SINGLE_UI',
        lc_tss1_rpt  TYPE progname VALUE 'ZSAPBF_CONF_GO_TSS1_UI',
        lc_tss2_rpt  TYPE progname VALUE 'ZSAPBF_CONF_GO_TSS2_UI',
        lc_parameter TYPE char9    VALUE 'PARAMETER',
        lc_data      TYPE char4    VALUE 'DATA',
        lc_type      TYPE char4    VALUE 'TYPE',
        lc_like      TYPE char4    VALUE 'LIKE',
        lc_modif     TYPE char12   VALUE 'MODIF ID BL1',
        lc_so        TYPE char15   VALUE 'SELECT-OPTIONS:',
        lc_for       TYPE char3    VALUE 'FOR',
        lc_symbol    TYPE c        VALUE '.',
        lc_ref       TYPE char4    VALUE 'ref_',
        lc_hyphen    TYPE c        VALUE '-',
        lc_en        TYPE char2    VALUE  'EN',
        lc_methflag  TYPE char8    VALUE  'METHFLAG',
        lc_clearpt   TYPE char7    VALUE  'CLEARPT',
        lc_plants    TYPE char6    VALUE  'PLANTS',
        lc_bl1       TYPE char3    VALUE  'BL1',
        lc_parafill  TYPE char8    VALUE  'PARAFILL',
        lc_meth      TYPE char4    VALUE  'METH',
        lc_fc02      TYPE char4    VALUE  'FC02',
        lc_methflg   TYPE char7    VALUE  'METHFLG',
        lc_clearrpt  TYPE char8    VALUE  'CLEARRPT',
        lc_paratsso  TYPE char9    VALUE  'PARATSSO',
        lc_methdflag TYPE char9    VALUE  'METHDFLAG',
        lc_clear     TYPE char5    VALUE  'CLEAR',
        lc_paratsst  TYPE char8    VALUE  'PARATSST',
        lc_pltset    TYPE char6    VALUE  'PLTSET'.
