*----------------------------------------------------------------------*
*   INCLUDE ZIPP503L_ENGIN_PR_TOP                                      *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* TABLE
*----------------------------------------------------------------------*
TABLES: ztpperm,       "Transfer of Engine Production Result (MES -> PP)
        mara,           "General Material Data
        marc,           "Plant Data for Material
        equi,           "Equipment master data
        ztpper_mapping. "Basis info table to use in ZIPP503I_ENGIN_PR
TABLES : ztpp_mip_stk_tra .
TABLES : blpk ,        "Document log header
         blpp .        "Document log item
TABLES : ztpp_pp_log_deta.
TYPE-POOLS : slis, sp01r.

*----------------------------------------------------------------------*
* INTERNAL TABLE
*----------------------------------------------------------------------*
DATA: it_ztpperm   LIKE ztpperm OCCURS 0 WITH HEADER LINE,
      it_vmaster  LIKE TABLE OF zspp_vin_value WITH HEADER LINE,
      it_vals     LIKE TABLE OF zspp_vin_value WITH HEADER LINE,
      it_error    LIKE TABLE OF ztpperm         WITH HEADER LINE.
DATA: wa_aufnr    TYPE  aufnr ,
      wa_matnr    LIKE  it_ztpperm-eitem,
      wa_erpid(3) TYPE  c .

*----------------------------------------------------------------------*
* BAPI
*----------------------------------------------------------------------*
** For Cerate Functional Location BAPI
DATA: external_number LIKE bapi_itob_parms-equipment,
      data_general    LIKE bapi_itob,
      data_specific   LIKE bapi_itob_eq_only,
      valid_date      LIKE bapi_itob_parms-inst_date,
      data_install    LIKE bapi_itob_eq_install,
** For Change Functional Location BAPI
      data_generalx   LIKE bapi_itobx,
      data_specificx  LIKE bapi_itob_eq_onlyx,
      return          LIKE bapiret2 .

DATA: bflushflags   LIKE bapi_rm_flg,
      bflushdatagen LIKE bapi_rm_datgen,
      bflushdatamts LIKE bapi_rm_datstock,
      wa_return LIKE bapiret2 ,
      it_serialnr LIKE bapi_rm_datserial OCCURS 0 WITH HEADER LINE.

*----------------------------------------------------------------------*
* DATA
*----------------------------------------------------------------------*
DATA: wa_equnr       LIKE equi-equnr,
      wa_flag                       ,
      wa_ztpperm      LIKE ztpperm,
      wa_datum        LIKE sy-datum ,
      wa_msgid        LIKE sy-msgid,
      wa_msgty        LIKE sy-msgty,
      wa_msgno        LIKE sy-msgno,
      wa_msgv1        LIKE sy-msgv1,
      wa_msgv2        LIKE sy-msgv2,
      wa_msgv3        LIKE sy-msgv3,
      wa_msgv4        LIKE sy-msgv4,
      wa_confirmation TYPE bapi_rm_datkey-confirmation,
      wa_cancco       TYPE bapi_rm_datkey-cancconfirmation,
      wa_old_cid      LIKE it_ztpperm-erpid.

*----------------------------------------------------------------------*
* ALV DECLARATION.
*----------------------------------------------------------------------*
DATA : ok_code       LIKE  sy-ucomm,
       save_ok_code  LIKE  sy-ucomm.
DATA : alv_grid               TYPE REF TO cl_gui_alv_grid,
       gs_custom_container    TYPE REF TO cl_gui_custom_container,
       wa_container           TYPE scrfname VALUE 'CONTAINER'.
DATA : gs_variant        TYPE disvariant ,  "Display Variant
       gs_layout         TYPE lvc_s_layo ,  "Layout
       gs_print          TYPE lvc_s_prnt ,  "Print control
       gt_special_groups TYPE lvc_t_sgrp ,  "Field groups
       gt_toolbar_excluding TYPE ui_functions , "Exclu Toolbar Std FUNC
       gt_header         TYPE TABLE OF slis_listheader WITH HEADER LINE,
       gt_fieldcat       TYPE lvc_t_fcat ,  "Field Catalog
       gt_sort           TYPE lvc_t_sort ,  "Sort criteria
       gt_filter         TYPE lvc_t_filt ,  "Filter criteria
       wa_fieldcat     TYPE lvc_s_fcat   ,
       wa_fname_tx(40),
       wa_saveline_ix     LIKE  sy-index.
DATA: w_oitem LIKE equi-matnr,
      w_batch_job(1).

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS: c_formname_top_of_page TYPE slis_formname
                                        VALUE 'TOP_OF_PAGE',
           c_mark     VALUE 'X' .

*----------------------------------------------------------------------*
* Macro
*----------------------------------------------------------------------*
DEFINE append_fieldcat.
  &1 = &1 + 1.
  w_fieldcat-col_pos       = &1.
  w_fieldcat-fieldname     = &2.
  w_fieldcat-ref_fieldname = &3.
  w_fieldcat-key           = &4.
  w_fieldcat-qfieldname    = &5.
  w_fieldcat-cfieldname    = &6.
  w_fieldcat-seltext_l     = &7.
  w_fieldcat-seltext_m     = &7.
  w_fieldcat-seltext_s     = &7.
  w_fieldcat-outputlen     = &8.
  w_fieldcat-no_out        = &9.
  append w_fieldcat.
  clear : w_fieldcat.
END-OF-DEFINITION.

*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_zsdat FOR ztpperm-zsdat NO-EXTENSION.
SELECT-OPTIONS: s_zslno FOR ztpperm-zslno. "NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-005.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 5.
PARAMETERS       r1 RADIOBUTTON GROUP radi DEFAULT 'X'.  "PROCESSING
SELECTION-SCREEN POSITION 7.
SELECTION-SCREEN COMMENT 8(20) text-002 FOR FIELD r1.
PARAMETERS       r2 RADIOBUTTON GROUP radi.       "ERRROR RE-PROCESSING
SELECTION-SCREEN COMMENT  (25) text-003 FOR FIELD r2.
PARAMETERS       r3 RADIOBUTTON GROUP radi.       "DISPLAY
SELECTION-SCREEN COMMENT  (20) text-004 FOR FIELD r3.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b2.
