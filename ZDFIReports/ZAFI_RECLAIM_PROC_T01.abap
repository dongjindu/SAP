*&---------------------------------------------------------------------*
*&  Include           ZAFI_RECLAIM_PROC_T01
*&---------------------------------------------------------------------*

TABLES : bkpf,
         ztfi_reclaim_dat,
         ztfi_reclaim_bak, " backup table
         sscrfields,  *ztfi_reclaim_dat.

INCLUDE <icon>.                        " icon

*----------------------------------------------------------------------*
* Data Declarations
*----------------------------------------------------------------------*

DATA:
  gd_documentheader    LIKE bapiache01,
  gd_customercpd       LIKE bapiacpa00,
  it_accountreceivable LIKE bapiacar01 OCCURS 0 WITH HEADER LINE,
  it_accountgl         LIKE bapiacgl01 OCCURS 0 WITH HEADER LINE,
  it_accounttax        LIKE bapiactx01 OCCURS 0 WITH HEADER LINE,
  it_criteria          LIKE bapiackecr OCCURS 0 WITH HEADER LINE,
  it_valuefield        LIKE bapiackeva OCCURS 0 WITH HEADER LINE,
  it_currencyamount    LIKE bapiaccr01 OCCURS 0 WITH HEADER LINE,
  it_return            LIKE bapiret2   OCCURS 0 WITH HEADER LINE,
  it_salesorder        LIKE bapiacso00 OCCURS 0 WITH HEADER LINE,
  it_salesamount       LIKE bapiaccrso OCCURS 0 WITH HEADER LINE,
  it_receivers         LIKE bdi_logsys OCCURS 0 WITH HEADER LINE.

DATA: $obj_key LIKE  bapiache01-obj_key,
       obj_sys LIKE bapiache01-obj_sys,
       obj_type LIKE bapiache01-obj_type.

DATA : it_data LIKE ztfi_reclaim_dat OCCURS 0 WITH HEADER LINE.

* ALV Declaration
DATA: wa_custom_control TYPE    scrfname VALUE 'ALV_CONTAINER',
      alv_grid          TYPE REF TO cl_gui_alv_grid,
      grid_container    TYPE REF TO cl_gui_custom_container.

* Global variables for attributes or etc of ALV GRID
DATA : it_fieldcat     TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldcat_fi  TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldcat_co  TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldname    TYPE slis_t_fieldcat_alv,
       it_sort         TYPE lvc_t_sort WITH HEADER LINE,
       it_fieldcat_det TYPE lvc_t_fcat WITH HEADER LINE. "/Detail

DATA : wa_is_layout TYPE lvc_s_layo, "/The Layout Structure
       w_fieldname    LIKE LINE OF it_fieldcat.

DATA : wa_variant TYPE disvariant. "for parameter IS_VARIANT

DATA: wa_save    TYPE c   VALUE 'A'.   "for Parameter I_SAVE

DATA : BEGIN OF gt_prodh OCCURS 0,
         matnr  LIKE mvke-matnr,
         prodh  LIKE mvke-prodh,
       END OF gt_prodh.

* UD1K940843 by IG.MOON 6/15/2007
DATA : BEGIN OF gt_ctry OCCURS 0,
         kunnr  LIKE kna1-kunnr,
         land1  LIKE kna1-land1,
       END OF gt_ctry.

DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.
* end of * UD1K940843

DATA : BEGIN OF it_output OCCURS 0,
        kunnr LIKE ztfi_reclaim_dat-vndr,
        natn      TYPE znatn,
        rono      TYPE zrono,
        issu   LIKE ztfi_reclaim_dat-issu,
        model     TYPE matnr,
        cty       TYPE land1_gp,
        amount LIKE ztfi_reclaim_dat-bjpt,
        tabcolor TYPE slis_t_specialcol_alv,
        msg LIKE ztfi_reclaim_dat-message,
        msgrvs LIKE ztfi_reclaim_dat-msgrvs,
       END OF it_output.
DATA : wt_output LIKE it_output OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_final OCCURS 0.
        INCLUDE STRUCTURE zsreclaim_alv.
*        cty(2) type c,
*        kunnr like ZTFI_RECLAIM_DAT-vndr,
*        MODEL(6) type c,
*        BJPT like ZTFI_RECLAIM_DAT-BJPT,
*        BJLA like ZTFI_RECLAIM_DAT-BJLA,
*        BJMI like ZTFI_RECLAIM_DAT-BJMI,
*        sub_tot1 like ZTFI_RECLAIM_DAT-BJMI,
*        OJPT like ZTFI_RECLAIM_DAT-OJPT,
*        OJLA like ZTFI_RECLAIM_DAT-OJLA,
*        OJMI like ZTFI_RECLAIM_DAT-OJMI,
*        sub_tot2 like ZTFI_RECLAIM_DAT-BJMI,
*        msg(50) type c,
data:   icon(4),
        waers    like  t001-waers,
        changed type c,
        mark,
      END OF it_final.

DATA: BEGIN OF it_tab OCCURS 0,    " +UD1K942469
        fcode LIKE rsmpe-func ,    " +UD1K942469
      END OF it_tab           .    " +UD1K942469

* DATA ??
data: begin  of gt_grid occurs 0.
        include structure zsreclaim_alv.
data:   icon(4),
        waers    like  t001-waers,
        changed type c,
        mark,
       end    of gt_grid.


DATA: l_text(100),
      l_answer.

DATA:  w_accdoc LIKE bapiacrev-ac_doc_no,
       w_accdoc_rvs LIKE bapiacrev-ac_doc_no,
       w_msgrvs(80),
       w_msg(50) TYPE c.    " +UD1K942469

data: gv_index  like  sy-tabix.

data: ty_rows            type lvc_t_row,
      ty_row             type lvc_s_row,    "ALV
      ty_roid            type lvc_t_roid.

* BEGIN OF UD1K953781
data: it_rec_posted like ztfi_rec_posted occurs 0 with header line.
* END OF UD1K953781
*&---------------------------------------------------------------------*
*&  * Class  defind
*&---------------------------------------------------------------------*
class lcl_event_receiver_grid1 definition deferred.
data: g_event_receiver_grid1   type ref to lcl_event_receiver_grid1.

*--------------------------------------------------------------------
*   Constants
*--------------------------------------------------------------------
constants: gc_waers     like tcurc-waers  value 'USD'.

*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-bl1 .
PARAMETERS :     p_bukrs LIKE bkpf-bukrs OBLIGATORY DEFAULT 'H201',
                 p_gjahr LIKE bkpf-gjahr OBLIGATORY DEFAULT sy-datum(4).
*      UD1K940683 by IG.MOON 5/29/2007
*                 P_MONAT like BKPF-MONAT obligatory,
SELECTION-SCREEN END OF BLOCK bl1.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003 .
SELECT-OPTIONS : s_vdgb  FOR ztfi_reclaim_dat-vdgb, " Type of Vendor
                 s_doex  FOR ztfi_reclaim_dat-doex, " DO/EX
* UD1K940839 by IG.MOON
                 " S_RONO  FOR ZTFI_RECLAIM_DAT-RONO, " RONO
                 s_natn  FOR ztfi_reclaim_dat-natn, " Country
* end of UD1K940839
                 s_vndr  FOR ztfi_reclaim_dat-vndr, " Vendor
                 s_issu  FOR ztfi_reclaim_dat-issu, " Issue #
                 s_carc  FOR ztfi_reclaim_dat-carc. " Model
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS :     p_budat LIKE bkpf-budat DEFAULT sy-datum,
                 p_bldat LIKE bkpf-bldat DEFAULT sy-datum,
*                 " Warrenty
*                 " Campaign
                 p_hkont1 LIKE bseg-hkont DEFAULT '0000620105',
                 p_hkont2 LIKE bseg-hkont DEFAULT '0000536015',
                 p_head(100) TYPE c,
                 p_item(100) TYPE c.
SELECTION-SCREEN END OF BLOCK b2.
*      end of UD1K940683

SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE text-bl2 .
PARAMETERS  : p_post AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK bl2.

** changed by Furong on 09/07/07 ; UD1K941549
SELECTION-SCREEN BEGIN OF BLOCK bl3 WITH FRAME TITLE text-bl3 .
PARAMETERS: r_01 RADIOBUTTON GROUP grp DEFAULT 'X',
            r_02 RADIOBUTTON GROUP grp.
PARAMETER: p_reason(2) TYPE c.
SELECTION-SCREEN END OF BLOCK bl3.
** end of change

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-004.
SELECTION-SCREEN SKIP 2.
SELECTION-SCREEN PUSHBUTTON 1(4) bkup USER-COMMAND bkup. " backup
SELECTION-SCREEN END OF BLOCK b4.
