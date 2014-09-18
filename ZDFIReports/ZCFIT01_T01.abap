*&---------------------------------------------------------------------*
*&  Include           ZCFIT01_T01
*&---------------------------------------------------------------------*


***********************************************************************
*     DATA DECLARATION
***********************************************************************
TABLES: fmci, bppe, ztfi_imfm, fmit, ztfi_map1, ztfi_fmal.
TABLES: t549q, t549s, fmfctr, fmhictr.

DATA: imap1 LIKE ztfi_map1 OCCURS 0 WITH HEADER LINE.  "get target

DATA: BEGIN OF it_fm01 OCCURS 0.  "target master info
        INCLUDE STRUCTURE  imap1.

DATA: gcode1(20),
      gcode2(20),
*     rlamt LIKE bppe-wtp01,  "released amount
*     acamt LIKE bppe-wtp01,  "commitment+invoice(acutals amount)
*     rsamt LIKE bppe-wtp01,  "residuals
      END OF it_fm01.
DATA: BEGIN OF it_fm02 OCCURS 0.  "master monthly amount

DATA: gcode1(20),
      gcode2(20),
      month(7),
      rlamt LIKE bppe-wtp01,  "released amount
      acamt LIKE bppe-wtp01,  "commitment+invoice(acutals amount)
      grptm1 LIKE imap1-grptm1,
      typtm1 LIKE imap1-typtm1,
      rattm1 LIKE imap1-rattm1,
      grptm2 LIKE imap1-grptm1,
      typtm2 LIKE imap1-typtm1,
      rattm2 LIKE imap1-rattm1,
*     rsamt LIKE bppe-wtp01,  "residuals
      hrflg,
      END OF it_fm02.

DATA: it_bppe LIKE bppe OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_fmfctr OCCURS 0,
         ctr_objnr  LIKE fmfctr-ctr_objnr,
         fictr      LIKE fmfctr-fictr,
         parent_obj LIKE fmhictr-parent_obj,
       END OF it_fmfctr.

DATA: i_bppe   LIKE bppe OCCURS 0 WITH HEADER LINE,
      i_fmit01 LIKE fmit OCCURS 0 WITH HEADER LINE,
      i_fmit02 LIKE fmit OCCURS 0 WITH HEADER LINE,
      i_zimfm  LIKE ztfi_imfm OCCURS 0 WITH HEADER LINE.

DATA month_name LIKE t247 OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF it_list OCCURS 0,  "final conversion itab
      month(10),
      gcode1(20),
      datum  LIKE sy-datum,
      totamt LIKE bppe-wtp01,
      grptm1 LIKE ztfi_map1-grptm1,
      typtm1 LIKE ztfi_map1-typtm1,
      amttm1 LIKE bppe-wtp01,
      grptm2 LIKE ztfi_map1-grptm1,
      typtm2 LIKE ztfi_map1-typtm1,
      amttm2 LIKE bppe-wtp01,
      END OF it_list.

DATA: wk_fdes LIKE fdes_import OCCURS 0 WITH HEADER LINE.
DATA: it_dele LIKE fdes        OCCURS 0 WITH HEADER LINE.
DATA: wk_dele LIKE fdes        OCCURS 0 WITH HEADER LINE.

DATA: fs_wtp(20),
      sv_waers LIKE t001-waers.

DATA: l_amt LIKE bppe-wtp01,
      l_idx(2) TYPE n,
      l_mon(2) TYPE n,
      l_prd(2) TYPE n,
      l_posid(20),
      l_gjahr LIKE sy-datum(4).

RANGES: s_datum  FOR sy-datum,
        s_gjahr  FOR bppe-gjahr,
        r_fund   FOR ztfi_imfm-posid,
        r_gcode1 FOR it_fm02-gcode1,
        r_gcode2 FOR it_fm02-gcode2,
        r_posid  FOR it_fm02-gcode2.

FIELD-SYMBOLS: <fs_wtp>.
CONSTANTS : c_wkrefer(4) VALUE 'IMFM'.

* USING ALV REPORTING..
TYPE-POOLS : slis.

INCLUDE rvreuse_global_data.
INCLUDE rvreuse_local_data.
INCLUDE rvreuse_forms.

DATA : gs_layout    TYPE slis_layout_alv,
       gt_fieldcat  TYPE slis_t_fieldcat_alv,
       gt_field     TYPE slis_t_fieldcat_alv,
       g_fieldcat_s TYPE slis_fieldcat_alv,  " ?? ??? ??.
       gt_events    TYPE slis_t_event,
       ls_sort      TYPE slis_sortinfo_alv,
       gt_sort      TYPE slis_t_sortinfo_alv,
       g_save(1)    TYPE c,
       g_exit(1)    TYPE c,
       gx_variant   LIKE disvariant,
       g_variant    LIKE disvariant,
       g_repid      LIKE sy-repid,
       g_cnt(2)     TYPE n.

CONSTANTS : c_status_set   TYPE slis_formname
                           VALUE 'PF_STATUS_SET',
            c_user_command TYPE slis_formname
                           VALUE 'USER_COMMAND',
            c_top_of_page  TYPE slis_formname VALUE 'TOP_OF_PAGE',
            c_top_of_list  TYPE slis_formname VALUE 'TOP_OF_LIST',
            c_end_of_list  TYPE slis_formname VALUE 'END_OF_LIST'.

***********************************************************************
* SELECTION-SCREEN
***********************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) text-h01.
PARAMETERS: p_bukrs  LIKE t001-bukrs   OBLIGATORY  MEMORY ID buk.
SELECTION-SCREEN COMMENT 52(40) p_butxt.
SELECTION-SCREEN END OF LINE.

PARAMETERS : p_gjahr LIKE bppe-gjahr OBLIGATORY DEFAULT sy-datum(4),
             p_month(2) TYPE n OBLIGATORY DEFAULT sy-datum+4(2),
             p_perid(2) TYPE n OBLIGATORY DEFAULT '04'.
SELECTION-SCREEN SKIP 1.

PARAMETERS : p_im       AS CHECKBOX USER-COMMAND exe.
SELECTION-SCREEN SKIP 1.
PARAMETERS : p_yearly   AS CHECKBOX USER-COMMAND exe.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
SELECT-OPTIONS : rfipex FOR fmci-fipos,  "commitment item
                 rfonds FOR bppe-geber,   "fund
                 rfistl FOR fmci-fictr.  "fund center
SELECTION-SCREEN END OF BLOCK b2.

***********************************************************************
* TOP-OF-PAGE
***********************************************************************
TOP-OF-PAGE.

***********************************************************************
* AT SELECTION-SCREEN
***********************************************************************
AT SELECTION-SCREEN OUTPUT.
  IF p_yearly EQ 'X'.
    p_month = '01'.
    p_perid = '12'.
    LOOP AT SCREEN.
      IF screen-name EQ 'P_MONTH' OR
         screen-name EQ 'P_PERID'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
*    p_month = sy-datum+4(2).
*    p_perid = '04'.
  ENDIF.
