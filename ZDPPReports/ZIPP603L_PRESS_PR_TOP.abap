*----------------------------------------------------------------------*
*   INCLUDE ZIPP503L_ENGIN_PR_TOP                                      *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* TABLE
*----------------------------------------------------------------------*
TABLES: ztpppr, "Press Production Result MES to PP(MIP)
        marc,   "Plant Data for Material
        mara,
        AFWI,
        mkal,   "Production Versions of Material
        ztpp_mip_stk_tra.   "[PP]MIP Stock Transfer Usage

TYPE-POOLS : slis, sp01r.
*----------------------------------------------------------------------*
* INTERNALTABLE
*----------------------------------------------------------------------*
DATA: it_vmaster  LIKE TABLE OF zspp_vin_value WITH HEADER LINE.
DATA: it_ztpppr   LIKE ztpppr OCCURS 0 WITH HEADER LINE.
DATA: it_error    LIKE TABLE OF ztpppr WITH HEADER LINE.
DATA: wa_aufnr    TYPE  aufnr ,
      wa_matnr    LIKE  it_ztpppr-pnlno,
      wa_prpid(3) TYPE  c .


*----------------------------------------------------------------------*
* DATA
*----------------------------------------------------------------------*
DATA: z_flag.

DATA: wa_ztpppr   LIKE  ztpppr.
DATA: wa_pprdno   TYPE  ztpppr-pprdno .    "PRDORD #

DATA: wa_msgid LIKE sy-msgid,
      wa_msgty LIKE sy-msgty,
      wa_msgno LIKE sy-msgno,
      wa_msgv1 LIKE sy-msgv1.
DATA: wa_datum   TYPE  sy-datum  .   "Actual Date

DATA : bdcdata     LIKE TABLE OF bdcdata  WITH HEADER LINE.
*----------------------------------------------------------------------*
* ALV DECLARATION.
*----------------------------------------------------------------------*
DATA : ok_code       LIKE  sy-ucomm,
       save_ok_code  LIKE  sy-ucomm.
data:  CTUMODE LIKE CTU_PARAMS-DISMODE.

DATA : alv_grid               TYPE REF TO cl_gui_alv_grid,
       gs_custom_container    TYPE REF TO cl_gui_custom_container,
       wa_container           TYPE scrfname VALUE 'CONTAINER'.
*       GS_APPLICATION         TYPE REF TO LCL_APPLICATION,
DATA : gs_variant        TYPE disvariant ,  "Display Variant
       gs_layout         TYPE lvc_s_layo ,  "Layout
       gs_print          TYPE lvc_s_prnt ,  "Print control
       gt_special_groups TYPE lvc_t_sgrp ,  "Field groups
       gt_toolbar_excluding TYPE ui_functions , "Exclu Toolbar Std FUNC
       gt_header         TYPE TABLE OF slis_listheader WITH HEADER LINE,
       gt_fieldcat       TYPE lvc_t_fcat ,  "Field Catalog
       gt_sort           TYPE lvc_t_sort ,  "Sort criteria
       gt_filter         TYPE lvc_t_filt .  "Filter criteria
DATA : wa_fieldcat     TYPE lvc_s_fcat.

DATA : wa_fname_tx(40),
       wa_saveline_ix     LIKE  sy-index.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS : c_formname_top_of_page TYPE slis_formname
                                        VALUE 'TOP_OF_PAGE'.
CONSTANTS : c_mark     VALUE 'X' .
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
* BAPI
*----------------------------------------------------------------------*
** CO11
DATA: wa_propose     LIKE bapi_pp_conf_prop.  "
DATA: it_athdrlevels LIKE bapi_pp_hdrlevel
                          OCCURS 0 WITH HEADER LINE,
      it_timeevents  LIKE bapi_pp_timeevent
                          OCCURS 0 WITH HEADER LINE,
      it_timetickets LIKE bapi_pp_timeticket
                          OCCURS 0 WITH HEADER LINE,
      it_goodsmovements LIKE bapi2017_gm_item_create
                          OCCURS 0 WITH HEADER LINE,
      it_link_conf_goodsmov LIKE bapi_link_conf_goodsmov
                          OCCURS 0 WITH HEADER LINE,
      it_detail_return  LIKE bapi_coru_return
                          OCCURS 0 WITH HEADER LINE,
      it_movt_type     LIKE BAPI2017_GM_ITEM_SHOW
                        OCCURS 0 WITH HEADER LINE.

                          .

** MB1B
DATA: goodsmvt_code     LIKE  bapi2017_gm_code.
DATA: goodsmvt_header LIKE bapi2017_gm_head_01,
      materialdocument TYPE bapi2017_gm_head_ret-mat_doc,
      matdocumentyear  TYPE bapi2017_gm_head_ret-doc_year.

DATA: it_goodsmvt_item LIKE bapi2017_gm_item_create
                          OCCURS 0 WITH HEADER LINE,
      it_return  LIKE bapiret2 OCCURS 0 WITH HEADER LINE.

DATA: return LIKE bapiret1,
      wa_return LIKE bapiret1.

*--->**ADDED BY YPL ON 10/11/2004*UD1K912811****
DATA: BEGIN OF it_mat OCCURS 1,
       matnr LIKE mara-matnr,
      END OF it_mat.
*--->**ADDED BY YPL ON 10/11/2004*UD1K912811*****


*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_zsdat FOR ztpppr-zsdat NO-EXTENSION.
SELECT-OPTIONS: s_zslno FOR ztpppr-zslno. "NO INTERVALS NO-EXTENSION.
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
