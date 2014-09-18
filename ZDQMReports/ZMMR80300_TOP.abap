*&---------------------------------------------------------------------*
*&  Include           ZKEMMR04001_TOP                                  *
*&---------------------------------------------------------------------*
*-- tables
TABLES : mara, marc, makt, mard.

TABLES sscrfields.
*-- includes
INCLUDE <icon>.
INCLUDE <symbol>.

FIELD-SYMBOLS : <fs1>, <fs2>,
<ft1> TYPE table.

DATA : lt_dynpread LIKE dynpread OCCURS 0 WITH HEADER LINE,
       g_atnam2    LIKE cabn-atnam.

*-- Types
TYPE-POOLS: abap, vrm.

* SAPGUI_PROGRESS_INDICATOR
CONSTANTS : text(35) VALUE 'Processing...............',
percentage(3) TYPE n VALUE '30',
c_zexkn(2)    TYPE n VALUE '01',
c_x           TYPE c VALUE 'X',
c_space       TYPE c VALUE ' '.

CONTROLS : ts_200    TYPE TABSTRIP.    " SCREEN 200 TAB

*----------------------------------------------------------------------*
* ??
*----------------------------------------------------------------------*
CONSTANTS :
            c_container1   TYPE scrfname VALUE 'GS_ALV_TEXT',
            c_readmode     TYPE i        VALUE 1,
            c_chanmode     TYPE i        VALUE 0,
            c_green        LIKE icons-l4    VALUE '@5B@',     "GREEN
            c_red          LIKE icons-l4    VALUE '@5C@',     "RED
            c_yes          TYPE c        VALUE 'Y',
            c_no           TYPE c        VALUE 'N',
            c_st_nm        LIKE  dd02l-tabname VALUE 'ZMMS0154'.

DATA: gt_values TYPE vrm_value OCCURS 0 WITH HEADER LINE.

*--- Internal tables

DATA : gt_return TYPE bapiret2 OCCURS 0 WITH HEADER LINE.

* BDC Detail Message.
DATA:
BEGIN OF gt_bdcmsg OCCURS 0,
  updkz TYPE updkz_d,
  zrsnum LIKE mara-matnr.
        INCLUDE STRUCTURE bapiret2.
DATA:
END OF gt_bdcmsg.

DATA: BEGIN OF gt_itab OCCURS 0.
        INCLUDE STRUCTURE zmms0154.
DATA: END OF gt_itab.


DATA : gt_mard   LIKE mard    OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF bdc OCCURS 0,
        matnr(18),
        lgort(4),
        lgpbe(10),
        lminb(13),
        lbstf(13),
       END OF bdc.


TYPES : BEGIN OF it_matnr_type,
  matnr LIKE mara-matnr,
  maktx LIKE makt-maktx,
END OF it_matnr_type.

DATA : it_matnr TYPE SORTED TABLE OF it_matnr_type
      WITH UNIQUE KEY matnr WITH HEADER LINE.


DATA: BEGIN OF gt_display OCCURS 0,
        check(1),
        icon(5).
        INCLUDE STRUCTURE gt_itab.

DATA :  f_col           TYPE lvc_t_scol,
        celltab         TYPE lvc_t_styl,

END   OF gt_display.

DATA: gs_display  LIKE gt_display.


DATA : BEGIN OF g_cond OCCURS 0,
  text(72),
END OF g_cond.

*--> FOR TABLE CONTROL HANDLING
DATA: BEGIN OF cxtab_column,
  screen       LIKE screen,
  index        TYPE i,
  selected(1)  TYPE c,
  vislength    LIKE icon-oleng,
  invisible(1) TYPE c,
END   OF cxtab_column.


*--- Ranges

*-- Data
DATA : ok_code          LIKE  sy-ucomm,
      s_ok_code        LIKE  sy-ucomm,
      g_err(1)         VALUE 'N',   "????
      g_index          LIKE  sy-tabix,
      g_tabix          LIKE  sy-tabix,
      g_tabix2         LIKE  sy-tabix,
      g_ucomm          LIKE  sy-ucomm,
      g_check(1),
      g_statu,
      g_rc,
      p_fname LIKE rlgrap-filename VALUE 'C:\',
      g_cnt(5)         TYPE n.
DATA : g_repid           TYPE sy-repid,         " program name
      g_dynnr           TYPE sy-dynnr,         " screen name
      g_start_col       TYPE i VALUE '1',      " excel
      g_start_row       TYPE i VALUE '1',      " excel
      g_end_col         TYPE i VALUE '256',    " excel
      g_end_row         TYPE i VALUE '65536',  " excel
      g_row             TYPE i           ,
      g_error01(20),
      g_error02(20),
      gs_text(50),
      g_trtyp LIKE t180-trtyp,
      gs_dynnr       TYPE sy-dynnr VALUE '0210',
      gc_continue.
DATA  g_type.
DATA: error_in_data TYPE c.

DATA : gc_name LIKE thead-tdname.
DATA : gc_ebeln LIKE eban-ebeln,
      gc_retxt(50) TYPE c,
      g_budat  LIKE mkpf-budat,
      gi_rscnt TYPE i.
DATA : g_lno TYPE i,
      g_lin TYPE i,
      gc_filename  LIKE dms_phio2file-filename,
      g_msgtxt(50) TYPE c,
      g_runst      TYPE c,
      g_field(20).
DATA: BEGIN OF gs_cursor    ,
  screen LIKE sy-dynnr,
  field(50)           ,
  lines TYPE i        ,
END   OF gs_cursor    .

*----------------------------------------------------------------------*
*  CONTROLS
*----------------------------------------------------------------------*
CONTROLS : tc_tab31 TYPE TABLEVIEW USING SCREEN 200,
tc_tab32 TYPE TABLEVIEW USING SCREEN 210,
tc_tab33 TYPE TABLEVIEW USING SCREEN 230.

*------ File UPLOAD -----------
DATA : g_sfpath   TYPE string VALUE '/WorkDir/IMSI/',  " ????
      g_sfname   TYPE string,                         " ?????
      g_saname   LIKE rlgrap-filename,                " ??????
      g_encode   TYPE abap_encoding,
      g_srtfd    TYPE srtfd,
      g_lfpath   TYPE string,                         " ???? (UP)
      g_ldpath   TYPE rlgrap-filename,                " ???? (DOWN)
      g_lfname   TYPE string,                         " ?????
      g_laname   TYPE string,                         " ??????
      g_lflens   TYPE i,                              " ??????

      g_action   TYPE i,                              " USER_ACTION
      g_itmmax   TYPE n.

DATA : BEGIN OF it_trans OCCURS 100,
  clustd(100),
END OF it_trans.

DATA : BEGIN OF gs_fields OCCURS 0.
        INCLUDE STRUCTURE help_value.
DATA : END OF gs_fields.

DATA : BEGIN OF gs_dynpfields OCCURS 0.
        INCLUDE STRUCTURE dynpread.
DATA : END OF gs_dynpfields.

DATA : BEGIN OF gt_valuetab OCCURS 0,
  value(80).
DATA : END OF gt_valuetab.

DATA : BEGIN OF gs_select_values OCCURS 0.
        INCLUDE STRUCTURE help_vtab.
DATA : END OF gs_select_values.

* ?? SET
DATA : g_selectfield   LIKE  help_info-fieldname,
      g_it_fields     LIKE  help_value OCCURS 1 WITH HEADER LINE,
      g_select_value  LIKE  help_info-fldvalue,
      g_ld_tabix      LIKE  sy-tabix.

* ?? ?? ??. -> ??? ?? ?? ??? ??? ? ??.
DATA : g_fields LIKE dynpread OCCURS 0 WITH HEADER LINE.


*-- D e f i n e d   f o r BAPI C o n t r o l -----------------
*---------------------------------------------------*
* BAPI data
*---------------------------------------------------*


*- D e f i n e d   f o r   A L V   c o n t r o l ----------------------*
*// ALV Global Field
TYPE-POOLS : slis, f4typ.

DATA: gt_outtab LIKE gt_itab OCCURS 0 WITH HEADER LINE,
      gs_private TYPE slis_data_caller_exit,
      gs_selfield TYPE slis_selfield,
      gt_fieldcat_s TYPE slis_t_fieldcat_alv WITH HEADER LINE,
      g_exit(1) TYPE c.

*// ALV Variant
DATA: alv_variant   LIKE disvariant,
      alv_repid     LIKE sy-repid.

*// ALV Attribute
DATA: gt_exclude   TYPE ui_functions,   " Tool Bar Button??
      gs_fieldcat  TYPE lvc_s_fcat,     " ?? ?? Set
      gt_fieldcat  TYPE lvc_t_fcat,     " ?? ?? ??
      gt_dfieldcat TYPE lvc_t_fcat,     " ?? ?? ?? (DETAIL)
      gs_f4        TYPE lvc_s_f4,                           " F4 ??
      gt_f4        TYPE lvc_t_f4,
      gs_layocat   TYPE lvc_s_layo,     " Grid ?? ??(Display)
      gs_sort      TYPE lvc_s_sort,
      gt_sort      TYPE lvc_t_sort,
      g_title      TYPE lvc_title,
      g_title2     TYPE lvc_title,
      ls_col       TYPE sy-tabix,
      l_scroll     TYPE lvc_s_stbl.

DATA: gt_exclude2   TYPE ui_functions,   " Tool Bar Button??
      gs_fieldcat2  TYPE lvc_s_fcat,     " ?? ?? Set
      gt_fieldcat2  TYPE lvc_t_fcat,     " ?? ?? ??
      gt_dfieldcat2 TYPE lvc_t_fcat,     " ?? ?? ?? (DETAIL)
      gs_f42        TYPE lvc_s_f4,                          " F4 ??
      gt_f42        TYPE lvc_t_f4,
      gs_layocat2   TYPE lvc_s_layo,     " Grid ?? ??(Display)
      gs_sort2      TYPE lvc_s_sort,
      gt_sort2      TYPE lvc_t_sort,
      ls_col2       TYPE sy-tabix,
      l_scroll2     TYPE lvc_s_stbl.

CONSTANTS : c_container    TYPE scrfname VALUE 'CC02'.  "ALV

*// CLASS Reference ??
CLASS lcl_event_handler       DEFINITION DEFERRED.
CLASS lcl_alv_grid            DEFINITION DEFERRED.

DATA : g_docking_container    TYPE REF TO cl_gui_docking_container,
      custom_container       TYPE REF TO cl_gui_custom_container,
      g_grid                 TYPE REF TO lcl_alv_grid,
      g_grid2                TYPE REF TO lcl_alv_grid,
      g_event_handler        TYPE REF TO lcl_event_handler,
      container_0            TYPE REF TO cl_gui_container,
      g_custom_container     TYPE REF TO cl_gui_custom_container.

*===> charicteristic
DATA : g_class  LIKE klah-class VALUE '9002',
      g_klart  TYPE klassenart VALUE '300',
      g_atnam  TYPE cabn-atnam VALUE 'MD'.
*
*DATA : it_tfeatures LIKE TABLE OF klvmera WITH HEADER LINE,
*      it_tvalues   LIKE TABLE OF api_vali WITH HEADER LINE.
*DATA : it_charic TYPE TABLE OF zcsdp011 WITH HEADER LINE.

*===> F4 model code
*===> F4 model code
DATA : BEGIN OF f4_help OCCURS 0,
  key   TYPE cawn-atwrt,
  text  TYPE cawnt-atwtb,
END OF f4_help.

DATA : BEGIN OF f4_help_w OCCURS 0,
  key   TYPE t001w-werks,
  text  TYPE t001w-name1,
END OF f4_help_w.

DATA : BEGIN OF f4_help_m OCCURS 0,
  key   TYPE makt-matnr,
  text  TYPE makt-maktx,
END OF f4_help_m.
*--> bdc data
DATA BEGIN OF gt_bdctab OCCURS 100.
        INCLUDE STRUCTURE bdcdata.
DATA END OF gt_bdctab.

DATA: gt_message LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

DATA: gv_bdcmode  LIKE rfpdo-allgazmd VALUE 'E',     "BDC MODE
      gv_suc      LIKE sy-index,
      gv_err      LIKE sy-index,
      gv_message(132),
      gv_lines    LIKE sy-index.

*----------------------------------------------------------------------*
* DECLARATION FOR SEARCH HELP
*----------------------------------------------------------------------*
DATA DYNPREAD LIKE DYNPREAD OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF VALUETAB OCCURS 0,
          VALUE(80).
DATA: END OF VALUETAB.

DATA: BEGIN OF FIELDS OCCURS 0.
        INCLUDE STRUCTURE HELP_VALUE.
DATA: END OF FIELDS.

DATA: BEGIN OF DYNPFIELDS  OCCURS 0.
        INCLUDE STRUCTURE DYNPREAD.
DATA: END OF DYNPFIELDS.

DATA  SELECT_INDEX LIKE SY-TABIX.

DATA: BEGIN OF SELECT_VALUES OCCURS 0.
        INCLUDE STRUCTURE HELP_VTAB.
DATA: END OF SELECT_VALUES.
*----------------------------------------------------------------------*
*  selection-screen
*----------------------------------------------------------------------*
*// #1

*----------------------------------------------------------------------*
* ???? ???(??????)
*----------------------------------------------------------------------*
*- STANDARD SELECTION SCREEN ------------------------------------------*
*--------------------------------------------------------------------*
*   SELECTION SCREEN                                                 *
*--------------------------------------------------------------------*
*SELECTION-SCREEN: BEGIN OF SCREEN 1100 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-002.

SELECTION-SCREEN : BEGIN OF LINE
  , COMMENT (15)    text-020. "  FOR FIELD ra.
PARAMETERS : ra   RADIOBUTTON GROUP r1 USER-COMMAND radi DEFAULT 'X'.
"??
SELECTION-SCREEN COMMENT 20(20) text-021 FOR FIELD ra.
PARAMETERS : ra_1 RADIOBUTTON GROUP r1.
SELECTION-SCREEN COMMENT 45(40) text-022 FOR FIELD ra_1. "Y
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b1.


SELECTION-SCREEN BEGIN OF BLOCK b2
                                  WITH FRAME TITLE text-001.
PARAMETERS : p_werks LIKE marc-werks DEFAULT 'P001'  MODIF ID g2.
SELECT-OPTIONS : s_lgort FOR mard-lgort MODIF ID g2,
                 s_ersda FOR mara-ersda MODIF ID g2,
                 s_matnr FOR mara-matnr MODIF ID g2.

PARAMETERS: fname LIKE rlgrap-filename  MODIF ID g1.

SELECTION-SCREEN END OF BLOCK b2.


*SELECTION-SCREEN: END OF SCREEN 1100.

**----------------------------------------------------------------------
*
*INITIALIZATION.
**----------------------------------------------------------------------
*
*  PERFORM INITIAL_DATE.
