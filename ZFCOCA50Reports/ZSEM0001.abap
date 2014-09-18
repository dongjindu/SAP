*
* Andy Choi
*
*When MRP list is not stored in table MDTB, it is stored in compressed
*table MDTC. Whether the MRP list is to be saved in MDTB or MDTC is a
*config setting (plant-wise). Check IMG node Production -> MRP -> Check
*Performance Settings -> Check Storage Mode for MRP List. MDTC
*(compressed table) is default (recommended) and check-box available to
*switch it to MDTB (transparent table).

REPORT zsem0001.

TABLES sscrfields.
INCLUDE zacoui00.

INCLUDE <icon>.                        " icon

TABLES: ztbw_pmei, ztbw_eis2.
TABLES: mara, mbew, t001k, mard,eord,marc.

DEFINE __focus.
  call method cl_gui_control=>set_focus
      exporting
        control = &1 .
END-OF-DEFINITION.

****************************** constants *******************************
CONSTANTS:  false VALUE ' ',
            true  VALUE 'X'.
DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.

DATA  g_error.
*----------------------------------------------------------------------*
* Define local class
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.

  PRIVATE SECTION.

ENDCLASS.                   " LCL_EVENT_RECEIVER Definition

*----------------------------------------------------------------------*
* Implementation local class
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

ENDCLASS.                   " LCL_EVENT_RECEIVER Implementation

*----------------------------------------------------------------------*
* Define local class
*----------------------------------------------------------------------*
CLASS lcl_event_route DEFINITION.
  PUBLIC SECTION.

*    TYPES: BEGIN OF del_k,
*             werks TYPE plwrk,
*             matnr TYPE matnr,
*             type(1),
*             kostl  TYPE kostl,
*           END OF del_k.
*
*    TYPES: del_key   TYPE STANDARD TABLE OF del_k,
*           del_table TYPE STANDARD TABLE OF ty_display.
*
*    METHODS:
*      handle_data_changed
*         FOR EVENT data_changed OF cl_gui_alv_grid
*             IMPORTING er_data_changed,
*                       get_deleted_rows
*             EXPORTING
*                       deleted_rows TYPE del_table,
*         refresh_delta_tables.
*
*  PRIVATE SECTION.
*    DATA del_rows_display TYPE STANDARD TABLE OF ty_display.
*
** This flag is set if any error occured in one of the
** following methods:
*    DATA: error_in_data TYPE c.
*    METHODS:
*      update_delta_tables
*         IMPORTING
*            pr_data_changed TYPE REF TO cl_alv_changed_data_protocol.
*
*    METHODS:
*      get_cell_values
*           IMPORTING
*             row_id          TYPE int4
*             pr_data_changed TYPE REF TO cl_alv_changed_data_protocol
*           EXPORTING
*             key             TYPE del_k.

ENDCLASS.                   " LCL_EVENT_RECEIVER Definition


*---------------------------------------------------------------------*
*       CLASS lcl_event_route IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS lcl_event_route IMPLEMENTATION.


ENDCLASS.                   " LCL_EVENT_RECEIVER Implementation

DATA g_event_receiver  TYPE REF TO lcl_event_receiver.
DATA g_event_route  TYPE REF TO lcl_event_route.

DATA: wa_mara LIKE mara,
      wa_mbew LIKE mbew.

DATA $ix TYPE i.

DATA: i_mdkp LIKE mdkp OCCURS 0 WITH HEADER LINE.

CONSTANTS:
      mddisp(2) TYPE c VALUE 'MD',     "Materialdisposition
      lfplan(2) TYPE c VALUE 'LP'.     "Langfristplanung

DATA: lt_mard LIKE mard OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF material_info,
        datum LIKE sy-datum,
        matnr LIKE mara-matnr,
        bwkey LIKE mbew-bwkey,
*------ characteristics
        sobsl LIKE marc-sobsl,
        mandt LIKE mara-mandt,
        bukrs LIKE t001k-bukrs,
        bklas LIKE mbew-bklas,
        waers TYPE waers,
        vvatage TYPE vvatage,
        maktx LIKE makt-maktx,
        raube LIKE mara-raube,
        matkl LIKE mara-matkl,
        mtart LIKE mara-mtart,
        mstae LIKE mara-mstae,
        mmsta LIKE marc-mmsta,
        lifnr LIKE eord-lifnr,
        name1 LIKE lfa1-name1,
        dispo LIKE marc-dispo,
        ekgrp LIKE marc-ekgrp,
        meins LIKE mara-meins,
        profl LIKE mara-profl,


        peinh LIKE mbew-peinh,
        lbkum LIKE mbew-lbkum,
        stprs LIKE mbew-stprs,
        verpr LIKE mbew-verpr,
        salk3 LIKE mbew-salk3,

       $peinh(10),
       $stprs(10),
       $verpr(10),

      END OF material_info.

DATA: BEGIN OF it_collect occurs 0,
        matnr LIKE mara-matnr,
        bwkey LIKE mbew-bwkey,
        dispo LIKE marc-dispo,
        $peinh(15),
        $stprs(15),
        $verpr(15),
        lifnr LIKE eord-lifnr,
        ekgrp LIKE marc-ekgrp,
        bklas LIKE mbew-bklas,
        mstae LIKE mara-mstae,
        mmsta LIKE marc-mmsta,
      END OF it_collect.

DATA: BEGIN OF alv_out.
        INCLUDE STRUCTURE material_info.
DATA:
*------ key figures...
        mng00    LIKE mdsu-mng03,  "receipt
        zzbdmng0 LIKE mbew-lbkum,

        zzbdmng1 LIKE mdsu-mng01,
        mng01    LIKE mdsu-mng03,  "receipt
        zzlbkum1 LIKE mbew-lbkum,
        zzsalk1  LIKE mbew-salk3,
        zzbdmng2 LIKE mdsu-mng01,
        mng02    LIKE mdsu-mng03,  "receipt
        zzlbkum2 LIKE mbew-lbkum,
        zzsalk2  LIKE mbew-salk3,
        zzbdmng3 LIKE mdsu-mng01,
        mng03    LIKE mdsu-mng03,  "receipt
        zzlbkum3 LIKE mbew-lbkum,
        zzsalk3  LIKE mbew-salk3,
        dsdat    LIKE mdkp-dsdat,
      END OF alv_out.

DATA: wa_mdsu LIKE mdsu.
DATA: w_menge LIKE wa_mdsu-mng01.

DATA: it_mat  LIKE TABLE OF material_info WITH HEADER LINE,
      it_mat2 LIKE TABLE OF material_info WITH HEADER LINE,
      it_out  LIKE TABLE OF alv_out       WITH HEADER LINE,
      it_mdsu LIKE TABLE OF wa_mdsu.
DATA $it_mat LIKE it_mat OCCURS 0 WITH HEADER LINE.

*DATA: BEGIN OF i_out OCCURS 0.
*        INCLUDE STRUCTURE ztbw_pmei.
*DATA:  maktx LIKE makt-maktx,
*       raube LIKE mara-raube,
*       matkl LIKE mara-matkl,
*       mtart LIKE mara-mtart,
*       mstae LIKE mara-mstae,
*      END OF i_out.

data: gv_dsdat like mdkp-dsdat.

DATA: g_next_30 LIKE sy-datum,
      g_next_60 LIKE sy-datum,
      g_next_90 LIKE sy-datum,
      g_next_99 LIKE sy-datum.


DATA : BEGIN OF it_mrp_org OCCURS 0,
        werks LIKE plaf-plwrk,  "FIXME production plant : PWWRK
        matnr LIKE pbim-matnr,
        pedtr LIKE plaf-pedtr,  "finish date
        gsmng LIKE plaf-gsmng,  "receipt (+)
        bdmng LIKE plaf-bdmng,  "requirement (-)
        delkz LIKE mdtb-delkz, "MRP element

*        dsdat LIKE mdkp-dsdat,

       END OF it_mrp_org.

DATA : BEGIN OF it_mrp OCCURS 0,
        werks LIKE plaf-plwrk,  "FIXME production plant : PWWRK
        matnr LIKE pbim-matnr,
        pedtr LIKE plaf-pedtr,  "finish date
        gsmng LIKE plaf-gsmng,  "receipt (+)
        bdmng LIKE plaf-bdmng,  "requirement (-)
*        dsdat LIKE mdkp-dsdat,
       END OF it_mrp.

DATA : BEGIN OF it_vmat OCCURS 0,
        matnr LIKE eord-matnr,
        lifnr LIKE eord-lifnr,
        name1 LIKE lfa1-name1,
       END OF it_vmat.

DATA: w_cmonth_end LIKE sy-datum,
      w_fst_mon LIKE sy-datum,
      w_scd_mon LIKE sy-datum,
      w_thd_mon LIKE sy-datum,
      w_nlines  TYPE i.

FIELD-SYMBOLS: <fs_mat> LIKE LINE OF it_out,
               <fs_mdsu> LIKE LINE OF it_mdsu.

RANGES: r_bwkey FOR mbew-bwkey.
RANGES: r_delkz FOR mdtb-delkz.

*--- ALV
TYPE-POOLS: slis.
DATA : w_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
       w_eventcat TYPE slis_t_event WITH HEADER LINE,
       w_selfield TYPE slis_selfield,
       w_sortcat  TYPE slis_t_sortinfo_alv WITH HEADER LINE,
       w_col_pos  TYPE i,
       w_program  LIKE sy-repid,
       w_top_of_page TYPE slis_t_listheader,
       w_line1    TYPE slis_listheader,
       x_layout   TYPE disvariant,
       l_variant  TYPE disvariant,  "Display Variant
       l_layout   TYPE slis_layout_alv.  "List layout specifications

*DATA: gt_fieldcat TYPE slis_t_fieldcat_alv,
*      gs_layout   TYPE slis_layout_alv,
*      gt_sp_group TYPE slis_t_sp_group_alv,
*      gt_events   TYPE slis_t_event,
*      gt_sorts    TYPE slis_t_sortinfo_alv WITH HEADER LINE,
*      gs_prnt     TYPE slis_print_alv,
*      g_repid     LIKE sy-repid.
DATA  g_vmode.
*---- ALV

SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-004.

PARAMETERS    :
  p_bukrs LIKE t001-bukrs DEFAULT 'H201',
  p_keydt LIKE sy-datum OBLIGATORY DEFAULT sy-datum.
*  p_dsdat LIKE mdkp-dsdat OBLIGATORY DEFAULT sy-datum.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (15) c_text1.
SELECTION-SCREEN POSITION 33.
PARAMETERS:  p_mon1 LIKE t5a4a-dlydy DEFAULT '30'.
SELECTION-SCREEN POSITION 38.
PARAMETERS:  p_mon2 LIKE t5a4a-dlydy DEFAULT '60'.
SELECTION-SCREEN POSITION 43.
PARAMETERS:  p_mon3 LIKE t5a4a-dlydy DEFAULT '90'.
SELECTION-SCREEN END OF LINE.

*select-options: s_mtart for mara-matnr no-display.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (15) c_text2.
SELECTION-SCREEN POSITION 33.
*S: Simulate, U:Update, D:Display
PARAMETERS:  p_mode(4) TYPE c DEFAULT 'S'.
SELECTION-SCREEN POSITION 40.
PARAMETERS: p_spec AS CHECKBOX.
SELECTION-SCREEN POSITION 43.
SELECTION-SCREEN COMMENT (30) c_text3.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK bl1.



SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE text-002.

PARAMETERS    :
  p_mrp AS CHECKBOX DEFAULT ' '.

SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN POSITION 2.
PARAMETERS:p_ltp AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT (15) c_text4.
SELECTION-SCREEN POSITION 33.
PARAMETERS:p_plscn LIKE rm61r-plscn  DEFAULT '900'.
SELECTION-SCREEN END OF LINE.

PARAMETERS    :
*  p_old     AS CHECKBOX DEFAULT 'X',
*  p_rcpt    AS CHECKBOX DEFAULT 'X',
             p_split AS CHECKBOX,
             p_limit LIKE mbew-salk3   DEFAULT 100.

SELECTION-SCREEN END OF BLOCK bl2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-006.
PARAMETERS    :
            p_old     AS CHECKBOX DEFAULT 'X',
            p_rcpt    AS CHECKBOX DEFAULT 'X'.
PARAMETERS: p_9999  AS CHECKBOX DEFAULT 'X'.

PARAMETERS:
            p_zero  AS CHECKBOX,
            p_nomrp AS CHECKBOX DEFAULT 'X'.
PARAMETER p_vari    LIKE disvariant-variant.
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK bl4 WITH FRAME TITLE text-005.
SELECT-OPTIONS: s_matnr  FOR mara-matnr.

SELECT-OPTIONS: s_mtart  FOR mara-mtart DEFAULT 'ROH',
                s_matkl  FOR mara-matkl,
                s_mstae  FOR mara-mstae,
                s_profl  FOR mara-profl,
                s_bklas  FOR mbew-bklas,
                s_lifnr  FOR eord-lifnr,
                s_werks  FOR marc-werks,
                s_dispo  FOR marc-dispo.

SELECTION-SCREEN END OF BLOCK bl4.
SELECTION-SCREEN PUSHBUTTON  1(24) timpr USER-COMMAND timpr.
SELECTION-SCREEN BEGIN OF BLOCK bl7 WITH FRAME TITLE text-007.
*MRP element
PARAMETERS:
  p_mrpar  AS CHECKBOX MODIF ID pll USER-COMMAND ttt,
  p_mrpba  AS CHECKBOX MODIF ID pll USER-COMMAND ttt,
  p_mrpbe  AS CHECKBOX MODIF ID pll USER-COMMAND ttt,
  p_mrpla  AS CHECKBOX MODIF ID pll USER-COMMAND ttt,
  p_mrple  AS CHECKBOX MODIF ID pll USER-COMMAND ttt,
  p_mrpsa  AS CHECKBOX MODIF ID pll USER-COMMAND ttt,
  p_mrpsb  AS CHECKBOX MODIF ID pll USER-COMMAND ttt,
  p_mrpsm  AS CHECKBOX MODIF ID pll USER-COMMAND ttt,
  p_mrpu2  AS CHECKBOX MODIF ID pll USER-COMMAND ttt,
  p_mrpu3  AS CHECKBOX MODIF ID pll USER-COMMAND ttt.
SELECTION-SCREEN END OF BLOCK bl7.

PARAMETERS par_cb1(1) TYPE c NO-DISPLAY.   "Close Block 1
PARAMETERS p_mrp_a DEFAULT 'X' AS CHECKBOX USER-COMMAND tta.
*------------------------------------------------------------*
*  At Selection-Screen
*-------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  DATA: rs_variant LIKE disvariant,
        nof4 TYPE c.

  CLEAR nof4.
  LOOP AT SCREEN.
    IF screen-name = 'P_VARI'.
      IF screen-input = 0.
        nof4 = 'X'.
      ENDIF.
    ENDIF.
  ENDLOOP.
  rs_variant-report   = sy-repid.
  rs_variant-username = sy-uname.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
       EXPORTING
            is_variant = rs_variant
            i_save     = 'A'
       IMPORTING
            es_variant = rs_variant
       EXCEPTIONS
            OTHERS     = 1.
  IF sy-subrc = 0 AND nof4 EQ space.
    p_vari = rs_variant-variant.

  ENDIF.

AT SELECTION-SCREEN.
  CASE sscrfields-ucomm.
    WHEN 'TTA'.
      CLEAR : p_mrpar,
              p_mrpba,
              p_mrpbe,
              p_mrpla,
              p_mrple,
              p_mrpsa,
              p_mrpsb,
              p_mrpsm,
              p_mrpu2,
              p_mrpu3.
    WHEN 'TTT'.
      IF    p_mrpar = 'X' OR
            p_mrpba = 'X' OR
            p_mrpbe = 'X' OR
            p_mrpla = 'X' OR
            p_mrple = 'X' OR
            p_mrpsa = 'X' OR
            p_mrpsb = 'X' OR
            p_mrpsm = 'X' OR
            p_mrpu2 = 'X' OR
            p_mrpu3 = 'X'.
        p_mrp_a = ' '.
      ENDIF.

      IF    p_mrpar = ' ' AND
            p_mrpba = ' ' AND
            p_mrpbe = ' ' AND
            p_mrpla = ' ' AND
            p_mrple = ' ' AND
            p_mrpsa = ' ' AND
            p_mrpsb = ' ' AND
            p_mrpsm = ' ' AND
            p_mrpu2 = ' ' AND
            p_mrpu3 = ' '.

        p_mrp_a = 'X'.

      ENDIF.
    WHEN 'TIMPR'.
      IF par_cb1 = ' '.
        par_cb1 = 'X'.
      ELSE.
        par_cb1 = ' '.
      ENDIF.
    WHEN OTHERS.
      EXIT.
  ENDCASE.


AT SELECTION-SCREEN OUTPUT.
  PERFORM modi_screen.

*&---------------------------------------------------------------------*
* INITIALIZATION.
*&---------------------------------------------------------------------*
INITIALIZATION.
  c_text1 = 'Date Ranges'.
  c_text2 = 'Task mode'.
  c_text3 = 'Update for target inventory'.
  c_text4 = 'Include LTP'.

  CLEAR x_layout.
  x_layout-report = sy-repid.

*&---------------------------------------------------------------------*
*& start-of-selection.
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  IF p_mode = ' '.
    PERFORM select_cbo.
    PERFORM display.

  ELSEIF p_mode = 'DEL.'.
*    PERFORM select_cbo.
    PERFORM delete_cbo.

  ELSE.
    PERFORM init_data.
    PERFORM get_source_list.

    PERFORM get_materials.
    PERFORM get_9999_stock.

    PERFORM get_mrp_materials.
    PERFORM get_mrp_data.
*   PERFORM get_ltp_data.

    PERFORM make_itab.

* save is possible if date is end of month.
    IF p_mode = 'U'.
      IF p_keydt <> w_cmonth_end.
        MESSAGE ID 'ZMMM' TYPE 'I' NUMBER '009' WITH
            'Key date is not end of month'.
        PERFORM display.
      ELSE.
        IF p_spec EQ 'X'.
          PERFORM update_to_db2.
        ELSE.
          PERFORM update_to_db.
        ENDIF.
      ENDIF.
* just display...
    ELSE.
      PERFORM display.

    ENDIF.
  ENDIF.



END-OF-SELECTION.


*&---------------------------------------------------------------------*
*&      Form  button_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM button_.

  IF par_cb1 EQ 'X'.
    WRITE:
           icon_collapse AS ICON TO timpr,
          'MRP/LTP element' TO timpr+4(18).
  ELSE.
    WRITE:
           icon_expand AS ICON TO timpr,
          'MRP/LTP element' TO timpr+4(18).
  ENDIF.

ENDFORM.                    " button_
*&---------------------------------------------------------------------*
*&      Form  modi_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modi_screen.

  LOOP AT SCREEN.
    IF screen-group1 = 'PLL'.
      IF  par_cb1 EQ 'X'.
        screen-active = '1'.
        MODIFY SCREEN.
      ELSE.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

  PERFORM button_.

ENDFORM.                    " modi_screen
*&---------------------------------------------------------------------*
*&      Form  get_next_month
*&---------------------------------------------------------------------*
FORM get_next_month USING p_olddate CHANGING p_newdate.

  CALL FUNCTION 'MONTH_PLUS_DETERMINE'
       EXPORTING
            months  = 1
            olddate = p_olddate
       IMPORTING
            newdate = p_newdate.

ENDFORM.                    " get_next_month
*&---------------------------------------------------------------------*
*&      Form  get_month_end
*&---------------------------------------------------------------------*
FORM get_month_end USING    p_dayin
                   CHANGING p_dayend.

  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
       EXPORTING
            day_in            = p_dayin
       IMPORTING
            last_day_of_month = p_dayend
       EXCEPTIONS
            day_in_no_date    = 1
            OTHERS            = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " get_month_end
*&---------------------------------------------------------------------*
*&      Form  get_stock_requirement
*&---------------------------------------------------------------------*
*FORM get_stock_requirement USING    p_matnr
*                                    p_bwkey.
*
*  CALL FUNCTION 'MD_STOCK_REQUIREMENTS_LIST_API'
*   EXPORTING
*     plscn                          = '900'
*     matnr                          = p_matnr
*     werks                          = p_bwkey
**   BERID                          =
**   ERGBZ                          =
**   AFIBZ                          =
*     inper                          = 'M'
**   DISPLAY_LIST_MDPSX             =
**   DISPLAY_LIST_MDEZX             =
**   DISPLAY_LIST_MDSUX             =
** IMPORTING
**   E_MT61D                        =
**   E_MDKP                         =
*  TABLES
**   MDPSX                          =
**   MDEZX                          =
*    mdsux                          = it_mdsu
*  EXCEPTIONS
*    material_plant_not_found       = 1
*    plant_not_found                = 2
*    OTHERS                         = 3 .
*
*  IF sy-subrc <> 0.
*    WRITE:/ 'Error'.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
*
*ENDFORM.                    " get_stock_requirement
*&---------------------------------------------------------------------*
*&      Form  init_data
*&---------------------------------------------------------------------*
FORM init_data.
  r_bwkey-sign = 'I'. r_bwkey-option = 'EQ'.
  SELECT * FROM t001k
    WHERE bukrs = p_bukrs
      AND bwkey IN s_werks.

    r_bwkey-low = t001k-bwkey. APPEND r_bwkey.
  ENDSELECT.

  PERFORM get_r_delkz.

  IF p_keydt+6(2) NE '01'.
    PERFORM get_month_end USING p_keydt CHANGING w_cmonth_end.
    w_fst_mon = w_cmonth_end + 1.
  ELSE.
    w_fst_mon = p_keydt.
  ENDIF.
  PERFORM get_next_month USING w_fst_mon CHANGING w_scd_mon.
  PERFORM get_next_month USING w_scd_mon CHANGING w_thd_mon.

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
       EXPORTING
            date      = p_keydt
            days      = p_mon1
            months    = '00'
            signum    = '+'
            years     = '00'
       IMPORTING
            calc_date = g_next_30.

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
       EXPORTING
            date      = p_keydt
            days      = p_mon2
            months    = '00'
            signum    = '+'
            years     = '00'
       IMPORTING
            calc_date = g_next_60.

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
       EXPORTING
            date      = p_keydt
            days      = p_mon3
            months    = '00'
            signum    = '+'
            years     = '00'
       IMPORTING
            calc_date = g_next_90.

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
       EXPORTING
            date      = p_keydt
            days      = '99'
            months    = '00'
            signum    = '+'
            years     = '00'
       IMPORTING
            calc_date = g_next_99.

ENDFORM.                    " init_data
*&---------------------------------------------------------------------*
*&      Form  get_mrp_data
*&---------------------------------------------------------------------*
FORM get_mrp_data.
  DATA: l_date LIKE sy-datum.

* V_HTNM Vendor-Material Relationships and Conditions
* MDTC
  TABLES: mdps.
  DATA: BEGIN OF i_mdpsx OCCURS 100.
          INCLUDE STRUCTURE mdps.
  DATA: END OF i_mdpsx.
  DATA: BEGIN OF i_mdtbx OCCURS 100.
          INCLUDE STRUCTURE mdtb.
  DATA: END OF i_mdtbx.


  DATA: dtart LIKE mdkp-dtart.

*  if p_plscn is initial or p_plscn = '000'.
*    dtart = mddisp. " MRP
*  else.
*    dtart = lfplan. " LTP
*  endif.

  LOOP AT i_mdkp.
    REFRESH i_mdtbx.

    IF i_mdkp-dtart = mddisp.
*      REFRESH i_mdtbx.
*    IMPORT i_mdtbx FROM DATABASE MDTC(AR) ID i_mdkp-dtnum.
      CALL FUNCTION 'READ_MRP_LIST'
           EXPORTING
                idtnum = i_mdkp-dtnum
                icflag = i_mdkp-cflag
           TABLES
                mdtbx  = i_mdtbx. "rec/req.
    ELSE.
      CALL FUNCTION 'DISPOBELEG_LESEN'
           EXPORTING
                dtnum = i_mdkp-dtnum
                cflag = i_mdkp-cflag
                mandt = i_mdkp-mandt
           TABLES
                mdpsx = i_mdpsx. "rec/req.
      LOOP AT i_mdpsx.
        MOVE-CORRESPONDING i_mdpsx TO i_mdtbx.
        APPEND i_mdtbx.
      ENDLOOP.

    ENDIF.

* summarize MRP/LTP using date range
    LOOP AT i_mdtbx.
      CLEAR it_mrp_org.
      it_mrp_org-matnr = i_mdkp-matnr.
      it_mrp_org-werks = i_mdkp-plwrk.

      IF i_mdtbx-dat00 < p_keydt AND p_old = 'X'.
        it_mrp_org-pedtr = p_keydt.
      ELSEIF ( i_mdtbx-dat00 >= p_keydt AND i_mdtbx-dat00 < g_next_30 ).
        it_mrp_org-pedtr = g_next_30.
      ELSEIF i_mdtbx-dat00 < g_next_60.
        it_mrp_org-pedtr = g_next_60.
      ELSEIF i_mdtbx-dat00 < g_next_90.
        it_mrp_org-pedtr = g_next_90.
      ELSE.
        it_mrp_org-pedtr = g_next_99.
      ENDIF.

      IF NOT it_mrp_org-pedtr IS INITIAL.
        IF i_mdtbx-plumi = '-'.  "+/-
          it_mrp_org-bdmng = - i_mdtbx-mng01.
        ELSEIF i_mdtbx-plumi = '+'.  "+/-
          it_mrp_org-gsmng = i_mdtbx-mng01.
        ENDIF.

        it_mrp_org-delkz = i_mdtbx-delkz.

*******  It's not been actiavted.
*******  it_mrp_org-dsdat = i_mdkp-dsdat.

        COLLECT it_mrp_org.
      ENDIF.

    ENDLOOP.
  ENDLOOP.

*  SORT it_mrp BY matnr pedtr.

*MRP 	HMMA	MRP	LTP
*AR	X	X	X
*BA	X	X	X
*BE	X	X	X
*KB	X	X	
*LA	X	X	X
*LE	X	X	X
*SA	X		X
*SB	X	X	
*SM	X		X
*U2	X	X	
*U3	X		X
*WB	X	X	X

*i_mdtbx-DELKZ : MRP element
*AR	Dependent reservation
*BA	Purchase requisition
*BB	Subcontractor requirements of material provided
*BE	Order item schedule line
*BP	Gross requirements planning
*BR	Process order
*CH	Batch stock
*ER	End of replenishment lead time
*E1	Subcontracting purchasing
*FE	Production order
*FH	End of planning time fence
*IH	Maintenance order
*JI	JIT call
*IM	Actual goods receipt quantity
*IW	In plant (only relevant for IS Automotive)
*KB	Individual customer stock
*KD	Customer independent requirement (currently not used)
*KK	Consignment stock for customer (availability check)
*LA	Shipping notification
*LB	Storage location stock
*LC	Batch/storage location stock
*LE	SA schedule line
*LF	JIT delivery schedule
*LK	Stock with subcontractor
*LL	Forecast delivery schedule
*MB	Goods issue
*MR	Reservation
*MS	Direct production
*NE	Network order
*PA	Planned order
*PB	Project stock
*PP	Planned independent requirement
*PR	Forecast requirement
*QM	Inspection lot for quality management
*RP	Returns item
*RR	MRP requirement (only relevant for IS-Automotive)
*RU	Confirmation
*S2	Simulated requirement from availability check
*SA	Simulation order
*SB	Dependent requirement
*SF	Safety requirement
*SH	Safety stock
*SI	Simulation requirement
*SM	Sim. dependent reqmt
*SU	Total requirements
*TB	Transfer requirements WMS
*U1	Release order for a stock transfer order
*U2	Release order for a stock transfer requisition
*U3	Transfer requirement for simulation order
*U4	Release order for stock transport scheduling agreement
*UB	Unplanned requirement
*UL	Reservation in another plant
*UR	Stock transfer reservation
*VA	Request for quotation
*VB	Quotation
*VC	Order
*VE	SD scheduling agreement
*VF	SD scheduling agreement; external service agent
*VG	Contract
*VI	Delivery w/o charge
*VJ	Delivery
*VP	Planning
*VT	Returns (availability check)
*VW	External sales order
*WA	Goods issue
*WB	Plant stock
*WE	Goods receipt
*WH	End replenishment period
*DD	Effective-out date


ENDFORM.                    " get_mrp_data
*&---------------------------------------------------------------------*
*&      Form  get_materials
*&---------------------------------------------------------------------*
FORM get_materials.
  DATA: l_mat LIKE it_out.

* limit material using source list...
  IF g_vmode EQ 'X'.
    SELECT t1~mandt t1~matnr t1~raube t1~mtart
           t1~mstae t4~mmsta t4~dispo t4~ekgrp
           t2~bwkey matkl meins profl
           t4~sobsl
*         lbkum stprs verpr salk3 t2~lfgja t2~lfmon maktx
           bklas lbkum stprs verpr salk3 peinh maktx
           h~bukrs h~waers
          INTO CORRESPONDING FIELDS OF TABLE it_mat
           FROM mara AS t1
              INNER JOIN makt AS t3
                 ON t3~matnr = t1~matnr
                AND t3~spras EQ sy-langu
              INNER JOIN marc AS t4
                 ON t1~matnr = t4~matnr
              INNER JOIN mbew AS t2
                 ON t2~matnr = t4~matnr
                AND t2~bwkey = t4~werks
              INNER JOIN t001k AS g
                 ON g~bwkey = t2~bwkey
              INNER JOIN t001  AS h
                 ON h~bukrs = g~bukrs
          FOR ALL ENTRIES IN it_vmat
            WHERE t2~bwkey IN r_bwkey
              AND t1~mtart IN s_mtart
              AND t2~salk3 > p_limit
*           and   mstae = '12'
              AND t1~matnr IN s_matnr
              AND t1~matnr EQ it_vmat-matnr
              AND t1~matkl IN s_matkl
              AND t1~mstae IN s_mstae
              AND t1~profl IN s_profl
              AND t2~bklas IN s_bklas
              AND t4~dispo IN s_dispo
              AND t4~werks IN s_werks.

*            ORDER BY T2~BWKEY descending.


  ELSE.
    SELECT t1~mandt t1~matnr t1~raube t1~mtart
           t1~mstae t4~mmsta t4~dispo t4~ekgrp
           t2~bwkey matkl meins profl
           t4~sobsl
*         lbkum stprs verpr salk3 t2~lfgja t2~lfmon maktx
           bklas lbkum stprs verpr salk3 peinh maktx
           h~bukrs h~waers
          INTO CORRESPONDING FIELDS OF TABLE it_mat
           FROM mara AS t1
              INNER JOIN makt AS t3
                 ON t3~matnr = t1~matnr
                AND t3~spras EQ sy-langu
              INNER JOIN marc AS t4
                 ON t1~matnr = t4~matnr
              INNER JOIN mbew AS t2
                 ON t2~matnr = t4~matnr
                AND t2~bwkey = t4~werks
              INNER JOIN t001k AS g
                 ON g~bwkey = t2~bwkey
              INNER JOIN t001  AS h
                 ON h~bukrs = g~bukrs
            WHERE t2~bwkey IN r_bwkey
              AND t1~mtart IN s_mtart
              AND t2~salk3 > p_limit
*           and   mstae = '12'
              AND t1~matnr IN s_matnr
              AND t1~matkl IN s_matkl
              AND t1~mstae IN s_mstae
              AND t1~profl IN s_profl
              AND t2~bklas IN s_bklas
              AND t4~dispo IN s_dispo
              AND t4~werks IN s_werks
            ORDER BY T2~BWKEY descending.
  ENDIF.

  IF sy-subrc NE 0.
    MESSAGE ID 'ZMMM' TYPE 'E' NUMBER '999' WITH text-001.
    EXIT.
  ENDIF.


* by ig.moon 11/10/2009 {
  IF p_split EQ space.
    __cls $it_mat.
    __cls it_collect.

    LOOP AT it_mat.
      it_collect-matnr = it_mat-matnr.
      it_collect-bwkey = it_mat-bwkey.
      it_collect-dispo = it_mat-dispo.
      it_collect-$peinh = it_mat-peinh.
      it_collect-$stprs = it_mat-stprs.
      it_collect-$verpr = it_mat-verpr.
      it_collect-lifnr = it_mat-lifnr.
      it_collect-ekgrp = it_mat-ekgrp.
      it_collect-bklas = it_mat-bklas.
      it_collect-MSTAE = it_mat-MSTAE.
      it_collect-MMSTA = it_mat-MMSTA.
      APPEND it_collect.
    ENDLOOP.
    SORT it_collect BY matnr bwkey DESCENDING.
    DELETE ADJACENT DUPLICATES FROM it_collect COMPARING matnr.
    SORT it_collect BY matnr.

    LOOP AT it_mat.
      $it_mat = it_mat.
      CLEAR : $it_mat-bwkey,$it_mat-sobsl,$it_mat-dispo.
      CLEAR : $it_mat-lifnr,$it_mat-ekgrp,$it_mat-bklas.
      CLEAR : $it_mat-MSTAE,$it_mat-MMSTA.
      COLLECT $it_mat.
    ENDLOOP.
    __cls it_mat.
    it_mat[] = $it_mat[].

    LOOP AT it_mat.
      $ix = sy-tabix.

      READ TABLE it_collect WITH KEY matnr = it_mat-matnr BINARY SEARCH.
      IF sy-subrc EQ 0.
        it_mat-peinh    =  it_collect-$peinh .
        it_mat-stprs    =  it_collect-$stprs .
        it_mat-verpr    =  it_collect-$verpr .
        it_mat-lifnr    =  it_collect-lifnr .
        it_mat-ekgrp    =  it_collect-ekgrp .
        it_mat-dispo    =  it_collect-dispo.
        it_mat-bklas    =  it_collect-bklas.
        it_mat-MSTAE    =  it_collect-MSTAE.
        it_mat-MMSTA    =  it_collect-MMSTA.
        it_mat-bwkey    =  it_collect-bwkey.
      ENDIF.

      MODIFY it_mat INDEX $ix.
    ENDLOOP.

  ENDIF.
* }

  SORT it_mat BY matnr bwkey DESCENDING.

ENDFORM.                    " get_materials
*&---------------------------------------------------------------------*
*&      Form  process_data
*&---------------------------------------------------------------------*
FORM process_data.
  DATA $fr TYPE i.

  LOOP AT it_out ASSIGNING <fs_mat>.
*   Initial stock
    <fs_mat>-zzlbkum1 = <fs_mat>-lbkum.
    <fs_mat>-zzlbkum2 = <fs_mat>-lbkum.
    <fs_mat>-zzlbkum3 = <fs_mat>-lbkum.

    READ TABLE it_mrp WITH KEY matnr = <fs_mat>-matnr
                                werks = <fs_mat>-bwkey
                       BINARY SEARCH.
    IF sy-subrc EQ 0.
      $fr = sy-tabix.
      LOOP AT it_mrp FROM $fr.
        IF  it_mrp-matnr = <fs_mat>-matnr
        AND it_mrp-werks = <fs_mat>-bwkey.
        ELSE.
          EXIT.
        ENDIF.

        CASE it_mrp-pedtr.
          WHEN g_next_30.
            <fs_mat>-zzbdmng1 = it_mrp-bdmng.

            <fs_mat>-zzlbkum1 = <fs_mat>-zzlbkum1 + <fs_mat>-zzbdmng1.
            <fs_mat>-zzlbkum2 = <fs_mat>-zzlbkum2 + <fs_mat>-zzbdmng1.
            <fs_mat>-zzlbkum3 = <fs_mat>-zzlbkum3 + <fs_mat>-zzbdmng1.
            IF p_rcpt = 'X'. "include receipt
              <fs_mat>-mng01 = it_mrp-gsmng.
              <fs_mat>-zzlbkum1 = <fs_mat>-zzlbkum1 + <fs_mat>-mng01.
              <fs_mat>-zzlbkum2 = <fs_mat>-zzlbkum2 + <fs_mat>-mng01.
              <fs_mat>-zzlbkum3 = <fs_mat>-zzlbkum3 + <fs_mat>-mng01.
            ENDIF.

            <fs_mat>-zzsalk1 = <fs_mat>-zzlbkum1
                               * <fs_mat>-verpr / <fs_mat>-peinh.
            <fs_mat>-zzsalk2 = <fs_mat>-zzlbkum2
                               * <fs_mat>-verpr / <fs_mat>-peinh.
            <fs_mat>-zzsalk3 = <fs_mat>-zzlbkum3
                               * <fs_mat>-verpr / <fs_mat>-peinh.
          WHEN g_next_60.
            <fs_mat>-zzbdmng2 = it_mrp-bdmng.

            <fs_mat>-zzlbkum2 = <fs_mat>-zzlbkum2 + <fs_mat>-zzbdmng2.
            <fs_mat>-zzlbkum3 = <fs_mat>-zzlbkum3 + <fs_mat>-zzbdmng2.
            IF p_rcpt = 'X'. "include receipt
              <fs_mat>-mng02 = it_mrp-gsmng.
              <fs_mat>-zzlbkum2 = <fs_mat>-zzlbkum2 + <fs_mat>-mng02.
              <fs_mat>-zzlbkum3 = <fs_mat>-zzlbkum3 + <fs_mat>-mng02.
            ENDIF.

            <fs_mat>-zzsalk2 = <fs_mat>-zzlbkum2
                               * <fs_mat>-verpr / <fs_mat>-peinh.
            <fs_mat>-zzsalk3 = <fs_mat>-zzlbkum3
                               * <fs_mat>-verpr / <fs_mat>-peinh.
          WHEN g_next_90.
            <fs_mat>-zzbdmng3 = it_mrp-bdmng.

            <fs_mat>-zzlbkum3 = <fs_mat>-zzlbkum3 + <fs_mat>-zzbdmng3.
            IF p_rcpt = 'X'. "include receipt
              <fs_mat>-mng03 = it_mrp-gsmng.
              <fs_mat>-zzlbkum3 = <fs_mat>-zzlbkum3 + <fs_mat>-mng03.
            ENDIF.

            <fs_mat>-zzsalk3 = <fs_mat>-zzlbkum3
                               * <fs_mat>-verpr / <fs_mat>-peinh.
        ENDCASE.
      ENDLOOP.
    ENDIF.
    <fs_mat>-datum = p_keydt.

  ENDLOOP.


ENDFORM.                    " process_data
*&---------------------------------------------------------------------*
*&      Form  select_cbo
*&---------------------------------------------------------------------*
FORM select_cbo.

  IF g_vmode EQ 'X'.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_out
       FROM ztbw_pmei AS a INNER JOIN mara AS b
               ON b~matnr = a~matnr
               INNER JOIN makt AS c
               ON c~matnr = a~matnr
               INNER JOIN marc AS d
               ON d~matnr = c~matnr
      FOR ALL ENTRIES IN it_vmat
       WHERE a~bukrs = p_bukrs
         AND a~gjahr = p_keydt(4)
         AND a~monat = p_keydt+4(2)
         AND b~mtart IN s_mtart
         AND a~matnr IN s_matnr
         AND a~matnr EQ it_vmat-matnr
         AND c~spras = sy-langu
         AND d~dispo IN s_dispo
         AND d~werks IN s_werks.
    LOOP AT it_out.
      $ix = sy-tabix.
      READ TABLE it_vmat WITH KEY matnr = it_out-matnr BINARY SEARCH.
      IF sy-subrc EQ 0.
        it_out-lifnr = it_vmat-lifnr.
        MODIFY it_out INDEX $ix.
      ENDIF.
    ENDLOOP.

  ELSE.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_out
       FROM ztbw_pmei AS a INNER JOIN mara AS b
               ON b~matnr = a~matnr
                           INNER JOIN makt AS c
               ON c~matnr = a~matnr
               INNER JOIN marc AS d
               ON d~matnr = c~matnr
       WHERE a~bukrs = p_bukrs
         AND a~gjahr = p_keydt(4)
         AND a~monat = p_keydt+4(2)
         AND b~mtart IN s_mtart
         AND a~matnr IN s_matnr
         AND c~spras = sy-langu
         AND d~dispo IN s_dispo
         AND d~werks IN s_werks.
  ENDIF.

ENDFORM.                    " select_cbo
*&---------------------------------------------------------------------*
*&      Form  fill_out
*&---------------------------------------------------------------------*
FORM fill_out.

ENDFORM.                    " fill_out
*&---------------------------------------------------------------------*
*&      Form  display
*&---------------------------------------------------------------------*
FORM display.

  PERFORM display_new.

*  PERFORM field_setting TABLES gt_fieldcat USING :
*   'DATUM'    'Date'            '10' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
*   'RAUBE'    'Shop'            '02' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
*   'MTART'    'M.type'          '04' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
*   'BKLAS'    'V.Cls'           '04' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
*   'PROFL'    'MIP/LP/KD'       '01' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
*   'MATKL'    'MatGroup'        '10' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
*   'MSTAE'    'X-Stat'          '02' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
*   'MMSTA'    'P-Stat'          '02' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
*   'VVATAGE'  'LstMvmt'         '06' ' ' 'R'  ' '  ' '  ' '  ' '  ' ',
*   'MATNR'    'Material'        '18' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
*   'MAKTX'    'Material Desc'   '40' ' ' 'L'  ' '  ' '  ' '  ' '  ' ',
*   'LBKUM'    'Current Stock'   '13' ' ' 'R'  ' '  '0'  ' '  ' '  ' ',
*   'SALK3'    'Current Value'   '16' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
*
*   'ZZBDMNG0' 'Req:Old   '      '13' ' ' 'R'  ' '  '0'  ' '  ' '  ' ',
*   'MNG00'    'Rec:Old   '      '13' ' ' 'R'  ' '  '0'  ' '  ' '  ' ',
*
*   'ZZBDMNG1' 'Req:M     '      '13' ' ' 'R'  ' '  '0'  ' '  ' '  ' ',
*   'MNG01'    'Rec:M     '      '13' ' ' 'R'  ' '  '0'  ' '  ' '  ' ',
*   'ZZLBKUM1' 'EndQty:M  '      '13' ' ' 'R'  ' '  '0'  ' '  ' '  ' ',
*   'ZZSALK1'  'EndVal:M  '      '16' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
*
*   'ZZBDMNG2' 'Req:M+1   '      '13' ' ' 'R'  ' '  '0'  ' '  ' '  ' ',
*   'MNG02'    'Rec:M+1   '      '13' ' ' 'R'  ' '  '0'  ' '  ' '  ' ',
*   'ZZLBKUM2' 'EndQty:M+1'      '13' ' ' 'R'  ' '  '0'  ' '  ' '  ' ',
*   'ZZSALK2'  'EndVal:M+1'      '16' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
*
*   'ZZBDMNG3' 'Req:M+2   '      '13' ' ' 'R'  ' '  '0'  ' '  ' '  ' ',
*   'MNG03'    'Rec:M+2   '      '13' ' ' 'R'  ' '  '0'  ' '  ' '  ' ',
*   'ZZLBKUM3' 'EndQty:M+2'      '13' ' ' 'R'  ' '  '0'  ' '  ' '  ' ',
*   'ZZSALK3'  'EndVal:M+2'      '16' ' ' 'R'  ' '  ' '  ' '  ' '  'X',
*
*   'STPRS'    'Std.Price'       '10' ' ' 'R'  ' '  ' '  ' '  ' '  ' ',
*   'VERPR'    'Act.Price'       '10' ' ' 'R'  ' '  ' '  ' '  ' '  ' ',
*   'MEINS'    'UoM'             '05' ' ' 'L'  ' '  ' '  ' '  ' '  ' ',
*   'PEINH'    'PrcUnit'         '06' ' ' 'R'  ' '  ' '  ' '  ' '  ' ',
*
*   'LIFNR'    'SourceList'      '10' ' ' 'L'  ' '  ' '  ' '  ' '  ' ',
*   'NAME1'    'Vnd Name'        '20' ' ' 'L'  ' '  ' '  ' '  ' '  ' ',
*   'DISPO'    'MRP Ctl'         '3'  ' ' 'L'  ' '  ' '  ' '  ' '  ' ',
*   'EKGRP'    'Pur.Grp'         '3'  ' ' 'L'  ' '  ' '  ' '  ' '  ' ',
*   'BWKEY'    'Plant'           '4'  ' ' 'L'  ' '  ' '  ' '  ' '  ' '.
*
*
*  g_repid = sy-repid.
*  l_variant-variant = p_vari.
*  l_variant-report  = sy-repid.
*
*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*       EXPORTING
*            i_callback_program = g_repid
*            it_fieldcat        = gt_fieldcat
*            is_variant         = l_variant
*            i_save             = 'A'
*       TABLES
*            t_outtab           = it_out
*       EXCEPTIONS
*            program_error      = 1
*            OTHERS             = 2.
*
ENDFORM.                    " display
*ALV
*&--------------------------------------------------------------------
*&      Form  make_field_category
*&--------------------------------------------------------------------
FORM field_setting TABLES p_fieldcat_t LIKE gt_fieldcat USING
                                  p_fieldname       " FIELD name
                                  p_title           " field titlw
                                  p_outputlen       " length
                                  p_key             "
                                  p_just            "
                                  p_noout           "
                                  p_round           "
                                  p_cfield          " currency field nam
                                  p_qfield          " quantity field nam
                                  p_dosum           " make sum
                                  .

  DATA: ls_fieldcat TYPE slis_fieldcat_alv.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname  = p_fieldname.
*  ls_fieldcat-seltext_s = p_title.
*  ls_fieldcat-seltext_m = p_title.
  ls_fieldcat-seltext_l  = p_title.
  ls_fieldcat-outputlen  = p_outputlen.
  ls_fieldcat-key        = p_key.
  ls_fieldcat-just       = p_just.
  ls_fieldcat-edit       = ''.   "p_edit.
  ls_fieldcat-no_out     = p_noout.
  ls_fieldcat-decimals_out   = p_round.
*  ls_fieldcat-cfieldname = p_cfield.
  ls_fieldcat-currency   = p_cfield.
  ls_fieldcat-qfieldname = p_qfield.
  ls_fieldcat-do_sum     = p_dosum.

  APPEND ls_fieldcat TO gt_fieldcat.

ENDFORM.                    " fill_field_category
*ALV
*&---------------------------------------------------------------------*
*&      Form  get_ltp_data
*&---------------------------------------------------------------------*
FORM get_ltp_data.
  EXIT.

* LTP
  TABLES: mdsm.
  DATA: BEGIN OF i_ltp  OCCURS 0,
          matnr LIKE mdsm-matnr,
          werks LIKE mdsm-werks,
          bdter LIKE mdsm-bdter,
          bdmng LIKE mdsm-bdmng, "req.Qty
        END OF i_ltp.

  DATA: BEGIN OF ws_plaf OCCURS 0,
          matnr LIKE mdsm-matnr,
          pedtr LIKE mdsm-bdter,
          gsmng LIKE mdsm-bdmng, "req.Qty
        END OF ws_plaf.

  CHECK p_ltp = 'X'.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE i_ltp
           FROM mdsm
           FOR ALL ENTRIES IN it_out
           WHERE plscn = p_plscn
             AND  ( sbart = space                        "note 190321
              OR    sbart = '1' )                        "note 190321
             AND matnr = it_out-matnr
             AND ( bdter >= p_keydt AND bdter <= g_next_90 )
             AND werks IN r_bwkey.

  LOOP AT i_ltp.
    CLEAR it_mrp.
    it_mrp-matnr = i_ltp-matnr.
    it_mrp-werks = i_ltp-werks.

    IF i_ltp-bdter < p_keydt AND p_old = 'X'.
      it_mrp-pedtr = g_next_30.
    ELSEIF ( i_ltp-bdter >= p_keydt AND i_ltp-bdter < g_next_30 ).
      it_mrp-pedtr = g_next_30.
    ELSEIF i_ltp-bdter < g_next_60.
      it_mrp-pedtr = g_next_60.
    ELSEIF i_ltp-bdter < g_next_90.
      it_mrp-pedtr = g_next_90.
    ELSE.
      it_mrp-pedtr = g_next_99.
    ENDIF.


    IF NOT it_mrp-pedtr IS INITIAL.
      it_mrp-gsmng = - i_ltp-bdmng.
      COLLECT it_mrp.
    ENDIF.


  ENDLOOP.


*Special Procurement
* 0	Standard external procurement
* 2	Consignment
* 3	Subcontracting
* 7	Stock transfer
* E	Standard in-house production
* P	Production in plant 2
* 9	IS-OIL only: Stock transfer reservation

*  select matnr pedtr gsmng
*         into corresponding fields of table ws_plaf
*               from plaf
*               for all entries in it_out
*                where beskz in ('X', 'F')  "procurement type
*                  and sobes in ('0', '2')  "special proc.type
*                  and plscn eq p_plscn     "planning scenario
**                 AND psttr IN r_datum
**                 and pedtr in r_datum
*                  and matnr = it_out-matnr
*                  and PWWRK in s_bwkey.
*
*  loop at ws_plaf.
*    it_mrp-matnr = ws_plaf-matnr.
*
*    if ws_plaf-pedtr < g_next_30.
*      it_mrp-pedtr = g_next_30.
*    elseif ws_plaf-pedtr < g_next_60.
*      it_mrp-pedtr = g_next_60.
*    elseif ws_plaf-pedtr < g_next_90.
*      it_mrp-pedtr = g_next_90.
*    else.
*      it_mrp-pedtr = g_next_99.
*    endif.
*
*    it_mrp-gsmng = ws_plaf-gsmng.
*    collect it_mrp.
*  endloop.

ENDFORM.                    " get_ltp_data
*&---------------------------------------------------------------------*
*&      Form  get_last_mvmt
*&---------------------------------------------------------------------*
FORM get_last_mvmt.
  DATA: l_idx  LIKE sy-tabix,
        l_bdatj TYPE bdatj,
        l_poper TYPE poper,
        l_letd TYPE letztbew.


  LOOP AT it_out.
    l_idx = sy-tabix.

    CLEAR l_letd.
    SELECT MAX( letztbew ) INTO l_letd
      FROM s032 WHERE matnr = it_out-matnr.

    IF sy-subrc = 0.
      IF l_letd IS INITIAL.
        it_out-vvatage = 9999.

        CLEAR l_bdatj.
        SELECT MAX( b~bdatj ) MAX( b~poper ) INTO (l_bdatj, l_poper)
          FROM ckmlhd AS a
          INNER JOIN ckmlpp AS b
             ON a~kalnr = b~kalnr
          WHERE a~matnr = it_out-matnr
            AND b~vnkumo <> 0.
*          order by b~bdatj descending
*                   b~poper descending.
*          exit.
*        endselect.
        IF NOT l_bdatj IS INITIAL.
          CONCATENATE l_bdatj l_poper+1(2) '15' INTO l_letd.
          it_out-vvatage = p_keydt - l_letd.
        ENDIF.
      ELSE.
        it_out-vvatage = p_keydt - l_letd.
      ENDIF.
      MODIFY it_out INDEX l_idx TRANSPORTING vvatage.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " get_last_mvmt
*&---------------------------------------------------------------------*
*&      Form  update_to_db
*&---------------------------------------------------------------------*
FORM update_to_db.
  DATA: l_cnt TYPE i,
        l_text(100),
        l_answer.

*&-------Delete the previous run data.
  SELECT COUNT( * ) FROM ztbw_pmei
    WHERE bukrs = p_bukrs
      AND gjahr = p_keydt(4)
      AND monat = p_keydt+4(2).

  CLEAR: l_text, l_answer.

  IF sy-dbcnt = 0.
    l_answer = 'J'.
  ELSE.
    l_text = 'Table has data already. Are you sure to refresh?'.
    PERFORM pop_up USING l_text
                   CHANGING l_answer.
  ENDIF.

  IF l_answer = 'J'.
    IF sy-dbcnt > 0.
      DELETE FROM ztbw_pmei
        WHERE bukrs = p_bukrs
          AND gjahr = p_keydt(4)
          AND monat = p_keydt+4(2).
    ENDIF.

    LOOP AT it_out.
      MOVE-CORRESPONDING it_out TO ztbw_pmei.
      ztbw_pmei-gjahr = p_keydt(4).
      ztbw_pmei-monat = p_keydt+4(2).
      ztbw_pmei-erdat = sy-datum.
      ztbw_pmei-erzet = sy-uzeit.
      ztbw_pmei-ernam = sy-uname.

      INSERT ztbw_pmei.
    ENDLOOP.

    IF sy-subrc NE 0.
      ROLLBACK WORK.
      WRITE:/ 'Completed with Error'.
    ELSE.
      COMMIT WORK AND WAIT.
      WRITE:/ 'Completed without Error'.
    ENDIF.
  ENDIF.

ENDFORM.                    " update_to_db
*&---------------------------------------------------------------------*
*&      Form  POP_UP
*&---------------------------------------------------------------------*
FORM pop_up USING    p_text
            CHANGING p_answer.
  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
       EXPORTING
            textline1      = p_text
            titel          = 'Check!'
            cancel_display = 'X'
       IMPORTING
            answer         = p_answer.


ENDFORM.                    " POP_UP
*&---------------------------------------------------------------------*
*&      Form  delete_zero_record
*&---------------------------------------------------------------------*
FORM delete_zero_record.

  IF p_zero = ' '.
    DELETE it_out
       WHERE zzlbkum1 <= 0.
  ENDIF.

ENDFORM.                    " delete_zero_record
*&---------------------------------------------------------------------*
*&      Form  update_to_db2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_to_db2.

  DATA: l_cnt TYPE i,
        l_text(100),
        l_answer.

*&-------Delete the previous run data.
  SELECT COUNT( * ) FROM ztbw_eis2
    WHERE bukrs = p_bukrs
      AND gjahr = p_keydt(4)
      AND monat = p_keydt+4(2).

  CLEAR: l_text, l_answer.

  IF sy-dbcnt = 0.
    l_answer = 'J'.
  ELSE.
    l_text = 'Table has data already. Are you sure to refresh?'.
    PERFORM pop_up USING l_text
                   CHANGING l_answer.
  ENDIF.

  IF l_answer = 'J'.
    IF sy-dbcnt > 0.
      DELETE FROM ztbw_eis2
        WHERE bukrs = p_bukrs
          AND gjahr = p_keydt(4)
          AND monat = p_keydt+4(2).
    ENDIF.

    LOOP AT it_out.
      MOVE-CORRESPONDING it_out TO ztbw_eis2.
      ztbw_eis2-gjahr = p_keydt(4).
      ztbw_eis2-monat = p_keydt+4(2).
      ztbw_eis2-erdat = sy-datum.
      ztbw_eis2-erzet = sy-uzeit.
      ztbw_eis2-ernam = sy-uname.

      INSERT ztbw_eis2.
    ENDLOOP.

    IF sy-subrc NE 0.
      ROLLBACK WORK.
      WRITE:/ 'Completed with Error'.
    ELSE.
      COMMIT WORK AND WAIT.
      WRITE:/ 'Completed without Error'.
    ENDIF.
  ENDIF.


ENDFORM.                    " update_to_db2
*&---------------------------------------------------------------------*
*&      Form  delete_cbo
*&---------------------------------------------------------------------*
FORM delete_cbo.

  IF g_vmode EQ 'X'.
    LOOP AT it_vmat.
      IF NOT s_dispo[] IS INITIAL.
        SELECT SINGLE * FROM marc WHERE matnr EQ it_vmat-matnr
                                   AND werks IN s_werks
                                   AND dispo IN s_dispo.
        IF sy-subrc NE 0.
          CONTINUE.
        ENDIF.
      ENDIF.

      DELETE FROM ztbw_pmei
         WHERE bukrs = p_bukrs
           AND gjahr = p_keydt(4)
           AND monat = p_keydt+4(2)
           AND matnr IN s_matnr
           AND matnr EQ it_vmat-matnr.
    ENDLOOP.
  ELSE.
    DELETE FROM ztbw_pmei
       WHERE bukrs = p_bukrs
         AND gjahr = p_keydt(4)
         AND monat = p_keydt+4(2)
         AND matnr IN s_matnr
         AND matnr IN ( select MATNR from MARC
                          WHERE matnr IN s_matnr
                           AND dispo IN s_dispo
                           AND werks IN s_werks ).
  ENDIF.

  WRITE:/ 'Data is deleted'.

ENDFORM.                    " delete_cbo
*&---------------------------------------------------------------------*
*&      Form  get_mrp_materials
*&---------------------------------------------------------------------*
FORM get_mrp_materials.

* MRP
  IF p_mrp = 'X'.
    SELECT  a~mandt
            a~DTART a~MATNR a~PLWRK a~PLSCN a~DTNUM a~DSDAT a~BDBKZ
            a~SLKZ1 a~SLKZ2 a~SLKZ3 a~SLKZ4 a~SLKZ5 a~SLKZ6 a~SLKZ7
            a~SLKZ8 a~VRKZ1 a~VRKZ2 a~VRKZ3 a~MTART a~MEINS a~DISST
            a~BESKZ a~SOBSL a~SOBES a~WRK02 a~DISMM a~DISVF a~DISPO
            a~PLDIS a~EKGRP a~MTWZT a~WEBAZ a~BEAZT a~FIXTR a~MFHKZ
            a~DISLS a~LOSVF a~LOSKZ a~PERAZ a~EISBE a~MINBE a~HOEBE
            a~BSTMI a~BSTMA a~BSTFX a~BSTRF a~SUM01 a~SUM02 a~SUM03
            a~SUM04 a~SUM05 a~NEGBS a~MSGID a~MSGAR a~MSGNR a~MSGV1
            a~MSGV2 a~MSGV3 a~MSGV4 a~DISGR a~PERIV a~MRPPP a~BDARF
            a~LFRHY a~RDPRF a~BERW1 a~BERW2 a~KZAUS a~AUSDT a~NFMAT
            a~AUSZ1 a~AUSZ2 a~AUSZ3 a~AUSZ4 a~AUSZ5 a~AUSZ6 a~AUSZ7
            a~AUSZ8 a~BEADA a~NAUKZ a~SAUFT a~KZPROMO a~SHFLG a~SHZET
            a~FABKZ a~MFXDT a~BSKFL a~MAABC a~CFLAG a~GRREL a~RWPRO
            a~SHPRO a~AHDIS a~BERW4

    INTO TABLE i_mdkp
                 FROM mdkp as a
                 inner join marc as b
                 on  b~matnr eq a~matnr
                 and b~werks eq a~PLWRK
                 FOR ALL ENTRIES IN it_mat
                  WHERE a~dtart = mddisp
                    AND a~matnr = it_mat-matnr
                    AND a~plwrk = it_mat-bwkey  "IN r_bwkey.
*                    AND a~dsdat >= p_dsdat
                    and b~DISMM NE 'ND'.
*                    AND sobsl < '40'.        "spec.proc.type
  ENDIF.

* LTP = Planned Order + Simulated Qty
  IF p_ltp = 'X'.

    SELECT a~mandt
          a~DTART a~MATNR a~PLWRK a~PLSCN a~DTNUM a~DSDAT a~BDBKZ
          a~SLKZ1 a~SLKZ2 a~SLKZ3 a~SLKZ4 a~SLKZ5 a~SLKZ6 a~SLKZ7
          a~SLKZ8 a~VRKZ1 a~VRKZ2 a~VRKZ3 a~MTART a~MEINS a~DISST
          a~BESKZ a~SOBSL a~SOBES a~WRK02 a~DISMM a~DISVF a~DISPO
          a~PLDIS a~EKGRP a~MTWZT a~WEBAZ a~BEAZT a~FIXTR a~MFHKZ
          a~DISLS a~LOSVF a~LOSKZ a~PERAZ a~EISBE a~MINBE a~HOEBE
          a~BSTMI a~BSTMA a~BSTFX a~BSTRF a~SUM01 a~SUM02 a~SUM03
          a~SUM04 a~SUM05 a~NEGBS a~MSGID a~MSGAR a~MSGNR a~MSGV1
          a~MSGV2 a~MSGV3 a~MSGV4 a~DISGR a~PERIV a~MRPPP a~BDARF
          a~LFRHY a~RDPRF a~BERW1 a~BERW2 a~KZAUS a~AUSDT a~NFMAT
          a~AUSZ1 a~AUSZ2 a~AUSZ3 a~AUSZ4 a~AUSZ5 a~AUSZ6 a~AUSZ7
          a~AUSZ8 a~BEADA a~NAUKZ a~SAUFT a~KZPROMO a~SHFLG a~SHZET
          a~FABKZ a~MFXDT a~BSKFL a~MAABC a~CFLAG a~GRREL a~RWPRO
          a~SHPRO a~AHDIS a~BERW4
    APPENDING TABLE i_mdkp
                 FROM mdkp as a
                 inner join marc as b
                 on  b~matnr eq a~matnr
                 and b~werks eq a~PLWRK
                 FOR ALL ENTRIES IN it_mat
                  WHERE a~dtart = lfplan
                    AND a~matnr = it_mat-matnr
                    AND a~plwrk = it_mat-bwkey   "IN r_bwkey
                    AND a~plscn = p_plscn
*                      AND a~dsdat >= p_dsdat
                    and b~DISMM NE 'ND'.
*                    AND sobsl < '40'.        "spec.proc.type
  ENDIF.

  SORT i_mdkp BY matnr plwrk.
  read table i_mdkp index 1.
  gv_dsdat = i_mdkp-dsdat.


ENDFORM.                    " get_mrp_materials
*&---------------------------------------------------------------------*
*&      Form  process_data_new
*&---------------------------------------------------------------------*
FORM process_data_new.
  DATA $fr TYPE i.

  __cls : it_mrp,it_out.

* summarize MRP/LTP
  LOOP AT it_mrp_org.
    CHECK it_mrp_org-delkz IN r_delkz.

    it_mrp-werks = it_mrp_org-werks.
    it_mrp-matnr = it_mrp_org-matnr.
    it_mrp-pedtr = it_mrp_org-pedtr.
    it_mrp-gsmng = it_mrp_org-gsmng.
    it_mrp-bdmng = it_mrp_org-bdmng.
*******  It's not been actiavted.
*    it_mrp-dsdat = it_mrp_org-dsdat.
    COLLECT it_mrp.
  ENDLOOP.

  LOOP AT it_mrp.
    CASE it_mrp-pedtr.
      WHEN p_keydt.
        IF p_old = 'X'.
          it_out-zzbdmng1 = it_mrp-bdmng.  "it_mrp-pedtr = p_keydt.
          it_out-zzbdmng0 = it_mrp-bdmng.
        ENDIF.
      WHEN g_next_30.
        it_out-zzbdmng1 = it_mrp-bdmng.
      WHEN g_next_60.
        it_out-zzbdmng2 = it_mrp-bdmng.
      WHEN g_next_90.
        it_out-zzbdmng3 = it_mrp-bdmng.
      WHEN OTHERS.
        CONTINUE.
    ENDCASE.

* --"include receipt
    IF p_rcpt = 'X'.
      CASE it_mrp-pedtr.
        WHEN p_keydt.
          IF p_old = 'X'.
            it_out-mng01 = it_mrp-gsmng.  "it_mrp-pedtr = p_keydt.
            it_out-mng00 = it_mrp-gsmng.
          ENDIF.
        WHEN g_next_30.
          it_out-mng01 = it_mrp-gsmng.
        WHEN g_next_60.
          it_out-mng02 = it_mrp-gsmng.
        WHEN g_next_90.
          it_out-mng03 = it_mrp-gsmng.
        WHEN OTHERS.
      ENDCASE.
    ENDIF.

*-- key part
    it_out-matnr = it_mrp-matnr.
    IF p_split = 'X'.
      it_out-bwkey = it_mrp-werks.
    ENDIF.
    it_out-datum = p_keydt.
    COLLECT it_out. CLEAR it_out.
  ENDLOOP.

ENDFORM.                    " process_data_new
*&---------------------------------------------------------------------*
*&      Form  prepare_it_out_new
*&---------------------------------------------------------------------*
FORM prepare_it_out_new.

  DATA: $ix LIKE sy-tabix.

  SORT it_mat BY matnr sobsl DESCENDING.
  LOOP AT it_out.
    $ix = sy-tabix.

    LOOP AT it_mat WHERE matnr = it_out-matnr.
      PERFORM move_mat_to_out.

      IF p_split = ''
      OR ( p_split = 'X' AND it_out-bwkey = it_mat-bwkey ).

        it_out-lbkum = it_out-lbkum + it_mat-lbkum.

      ENDIF.

    ENDLOOP.

*---exclude 9999 stock
    IF p_split = 'X'.
      READ TABLE lt_mard WITH KEY matnr = it_out-matnr
                                  werks = it_out-bwkey BINARY SEARCH.
    ELSE.
      READ TABLE lt_mard WITH KEY matnr = it_out-matnr BINARY SEARCH.
    ENDIF.
    IF sy-subrc = 0.
      it_out-lbkum = it_out-lbkum - lt_mard-labst.
    ENDIF.

    it_out-zzlbkum1 = it_out-lbkum.
    it_out-zzlbkum2 = it_out-lbkum.
    it_out-zzlbkum3 = it_out-lbkum.

    MODIFY it_out INDEX $ix.
  ENDLOOP.


ENDFORM.                    " prepare_it_out_new
*&---------------------------------------------------------------------*
*&      Form  get_9999_stock
*&---------------------------------------------------------------------*
FORM get_9999_stock.

*exclude 999 stock
  IF p_9999 = 'X'.
    SELECT * INTO TABLE lt_mard FROM mard
       FOR ALL ENTRIES IN it_mat
       WHERE matnr = it_mat-matnr
         AND werks = it_mat-bwkey
         AND lgort = '9999'.
  ENDIF.

*get 999 WM stock
*CC – Glovis CC storage types
*IP – HMMA lineside storage types
*PH – Engine expansion storage types

*        SELECT WERKS MATNR LQNUM GESME
*                        INTO CORRESPONDING FIELDS OF TABLE LT_LQUA_TEMP
*                         FROM LQUA AS A
*                         INNER JOIN ZTMM_STG_GRP AS B
*                          ON A~LGTYP = B~LGTYP
*                          FOR ALL ENTRIES IN IT_mat
*                         WHERE MATNR EQ IT_mat-MATNR
*                           AND B~ZSTGP = 'PH'.
**                    AND lgtyp BETWEEN '991' AND '999'.
**                    AND ( lgtyp = '991' OR lgtyp = '999' ).


ENDFORM.                    " get_9999_stock
*&---------------------------------------------------------------------*
*&      Form  get_source_list
*&---------------------------------------------------------------------*
FORM get_source_list.

  CLEAR : it_vmat[], it_vmat, g_vmode. "vendor restriction

  SELECT eord~matnr eord~lifnr lfa1~name1 INTO TABLE it_vmat
  FROM eord
  INNER JOIN lfa1
          ON eord~lifnr = lfa1~lifnr
            WHERE eord~lifnr IN s_lifnr
              AND eord~vdatu <= p_keydt
              AND eord~bdatu >= p_keydt
              AND eord~werks IN s_werks.
  READ TABLE it_vmat INDEX 1.
  IF sy-subrc EQ 0.
    SORT it_vmat BY matnr lifnr.
    IF NOT s_lifnr[] IS INITIAL.
      g_vmode = 'X'.
    ENDIF.
  ENDIF.

ENDFORM.                    " get_source_list
*&---------------------------------------------------------------------*
*&      Form  get_vendor_info
*&---------------------------------------------------------------------*
FORM get_vendor_info.

  LOOP AT it_out.
    $ix = sy-tabix.
    READ TABLE it_vmat WITH KEY matnr = it_out-matnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_out-lifnr = it_vmat-lifnr.
      it_out-name1 = it_vmat-name1.
*      it_out-dsdat = p_dsdat.

      MODIFY it_out  INDEX $ix TRANSPORTING lifnr name1 dsdat datum.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " get_vendor_info
*&---------------------------------------------------------------------*
*&      Form  add_no_mrp_stock
*&---------------------------------------------------------------------*
FORM add_no_mrp_stock.

  DATA: $ix LIKE sy-tabix.
  DATA  $it_out LIKE it_out OCCURS 0 WITH HEADER LINE.

  SORT it_mat BY matnr bwkey DESCENDING.
  SORT it_out BY matnr bklas.

  LOOP AT it_mat.
    $ix = sy-tabix.

    IF p_split = ''.
      READ TABLE it_out WITH KEY matnr = it_mat-matnr BINARY SEARCH.
    ELSE.
      READ TABLE it_out WITH KEY matnr = it_mat-matnr
                                 bwkey = it_mat-bwkey BINARY SEARCH.
    ENDIF.

    IF sy-subrc <> 0.
      CLEAR it_out.
      PERFORM move_mat_to_out.
      READ TABLE $it_out WITH KEY matnr = it_mat-matnr.
      IF sy-subrc EQ 0.
        IF p_split = ''
        OR ( p_split = 'X' AND it_out-bwkey = it_mat-bwkey ).
          it_out-lbkum = $it_out-lbkum + it_mat-lbkum.
        ELSE.
          it_out-lbkum = it_mat-lbkum.
        ENDIF.
      ELSE.
        it_out-lbkum = it_mat-lbkum.
      ENDIF.

*---exclude 9999 stock
      IF p_split = 'X'.
        READ TABLE lt_mard WITH KEY matnr = it_out-matnr
                                    werks = it_out-bwkey BINARY SEARCH.
      ELSE.
        READ TABLE lt_mard WITH KEY matnr = it_out-matnr BINARY SEARCH.
      ENDIF.
      IF sy-subrc = 0.
        it_out-lbkum = it_out-lbkum - lt_mard-labst.
      ENDIF.

      it_out-zzlbkum1 = it_out-lbkum.
      it_out-zzlbkum2 = it_out-lbkum.
      it_out-zzlbkum3 = it_out-lbkum.

*-- key part
      it_out-matnr = it_mat-matnr.
      IF p_split = 'X'.
        it_out-bwkey = it_mat-bwkey.
      ENDIF.
      it_out-datum = p_keydt.

      APPEND it_out TO $it_out.

    ENDIF.

  ENDLOOP.

  APPEND LINES OF $it_out TO it_out.

ENDFORM.                    " add_no_mrp_stock
*&---------------------------------------------------------------------*
*&      Form  it_out_valuation
*&---------------------------------------------------------------------*
FORM it_out_valuation.
  DATA: $ix LIKE sy-tabix.

* inventory & value calculation
  LOOP AT it_out.
    $ix = sy-tabix.

    it_out-zzlbkum1 = it_out-lbkum    + it_out-zzbdmng1.
    it_out-zzlbkum2 = it_out-zzlbkum1 + it_out-zzbdmng2.
    it_out-zzlbkum3 = it_out-zzlbkum2 + it_out-zzbdmng3.
    IF p_rcpt = 'X'. "include receipt
      it_out-zzlbkum1 = it_out-zzlbkum1 + it_out-mng01.
      it_out-zzlbkum2 = it_out-zzlbkum2 + it_out-mng02.
      it_out-zzlbkum3 = it_out-zzlbkum3 + it_out-mng03.
    ENDIF.


    it_out-salk3   = it_out-lbkum    * it_out-verpr / it_out-peinh.
    it_out-zzsalk1 = it_out-zzlbkum1 * it_out-verpr / it_out-peinh.
    it_out-zzsalk2 = it_out-zzlbkum2 * it_out-verpr / it_out-peinh.
    it_out-zzsalk3 = it_out-zzlbkum3 * it_out-verpr / it_out-peinh.

    MODIFY it_out INDEX $ix
           TRANSPORTING zzlbkum1 zzlbkum2 zzlbkum3
                        salk3 zzsalk1 zzsalk2 zzsalk3.
  ENDLOOP.

  SORT it_out BY matnr.

ENDFORM.                    " it_out_valuation
*&---------------------------------------------------------------------*
*&      Form  move_mat_to_out
*&---------------------------------------------------------------------*
FORM move_mat_to_out.

  it_out-sobsl   = it_mat-sobsl.
  it_out-bukrs   = it_mat-bukrs.
  it_out-bklas   = it_mat-bklas.
  it_out-waers   = it_mat-waers.
  it_out-vvatage = it_mat-vvatage.
  it_out-maktx   = it_mat-maktx.
  it_out-raube   = it_mat-raube.
  it_out-matkl   = it_mat-matkl.
  it_out-mtart   = it_mat-mtart.
  it_out-mstae   = it_mat-mstae.
  it_out-mmsta   = it_mat-mmsta.
  it_out-lifnr   = it_mat-lifnr.
  it_out-dispo   = it_mat-dispo.
  it_out-ekgrp   = it_mat-ekgrp.
  it_out-meins   = it_mat-meins.
  it_out-profl   = it_mat-profl.

  it_out-stprs   = it_mat-stprs.
  it_out-verpr   = it_mat-verpr.
  it_out-salk3   = it_mat-salk3.
  it_out-peinh   = it_mat-peinh.

ENDFORM.                    " move_mat_to_out
*&---------------------------------------------------------------------*
*&      Form  display_new
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_new.

  CALL SCREEN 100.

ENDFORM.                    " display_new
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  DATA: p1(20) TYPE c,
        p2(20) TYPE c.

  write p_keydt to p1.
  write gv_dsdat to p2.

  SET TITLEBAR '100' OF PROGRAM sy-REPID WITH p1 ' - as of 'p2.
*  SET TITLEBAR '100'.
  SET PF-STATUS '100'.
*   Exclude toolbar
*  PERFORM exclude_functions.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  exclude_functions
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM exclude_functions.
  PERFORM append_exclude_functions
           TABLES gt_exclude[]
           USING: cl_gui_alv_grid=>mc_fc_loc_undo,
*                  cl_gui_alv_grid=>mc_fc_average,
*                  cl_gui_alv_grid=>mc_fc_graph,
*                  cl_gui_alv_grid=>mc_fc_info,
                  cl_gui_alv_grid=>mc_fc_loc_copy_row,
                  cl_gui_alv_grid=>mc_fc_loc_append_row,
                  cl_gui_alv_grid=>mc_fc_loc_cut,
                  cl_gui_alv_grid=>mc_fc_loc_insert_row,
                  cl_gui_alv_grid=>mc_fc_loc_move_row,
                  cl_gui_alv_grid=>mc_fc_loc_paste_new_row.

ENDFORM.                    " exclude_functions
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_alv_100 OUTPUT.

  IF g_custom_container IS INITIAL.
    PERFORM create_and_init_alv.
*   Display alv grid
    CALL METHOD g_grid->set_table_for_first_display
         EXPORTING is_layout            = gs_layo
                   it_toolbar_excluding = gt_exclude
                   i_save               = gc_var_save
                   is_variant           = gs_variant
         CHANGING  it_outtab            = it_out[]
                   it_fieldcatalog      = gt_fcat[]
                   it_sort              = gt_sort[].
  ELSE.
    CALL METHOD g_grid->refresh_table_display.
  ENDIF.
  __focus g_grid.

ENDMODULE.                 " DISPLAY_ALV_100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  create_and_init_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_and_init_alv.
*   Create object
  PERFORM create_object.

*  Create Object to verify input values.
  CREATE OBJECT g_event_receiver.

*   Create field category
  PERFORM create_field_category USING false.

  CALL METHOD g_grid->register_edit_event
       EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD g_grid->set_ready_for_input
     EXPORTING
            i_ready_for_input = 0.

*  PERFORM sort_build USING gt_sort[].

*   Setting for layout
  PERFORM set_lvc_layout.

*   Set variant
  gv_repid = gs_variant-report = sy-repid.
  gs_variant-variant = p_vari.

ENDFORM.                    " create_and_init_alv
*&---------------------------------------------------------------------*
*&      Form  create_field_category
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FALSE  text
*----------------------------------------------------------------------*
FORM create_field_category USING mode_edit.
  DATA: l_pos       TYPE i.
  DEFINE __catalog.
    l_pos = l_pos + 1.
    clear gs_fcat.
    gs_fcat-col_pos       = l_pos.
    gs_fcat-key           = &1.
    gs_fcat-fieldname     = &2.
    gs_fcat-coltext       = &3.     " Column heading
    gs_fcat-outputlen     = &4.     " Column width
    gs_fcat-datatype      = &5.     " Data type
    gs_fcat-emphasize     = &6.
    gs_fcat-just          = &7.
    gs_fcat-QFIELDNAME    = &8.

    append gs_fcat to gt_fcat.
  END-OF-DEFINITION.

  data: s_0_gr(10) type c, s_0_gi(10) type c,
        s_0_eq(10) type c, s_0_ev(10) type c,
        s_1_gr(10) type c, s_1_gi(10) type c,
        s_1_eq(10) type c, s_1_ev(10) type c,
        s_2_gr(10) type c, s_2_gi(10) type c,
        s_2_eq(10) type c, s_2_ev(10) type c,
        s_3_gr(10) type c, s_3_gi(10) type c,
        s_3_eq(10) type c, s_3_ev(10) type c.

  concatenate 'GR:'  p_keydt+4(2) '/' p_keydt+6(2)  into s_0_gr.
  concatenate 'GI:'  p_keydt+4(2) '/' p_keydt+6(2)  into s_0_gi.
  concatenate 'CurQ:' p_keydt+4(2) '/' p_keydt+6(2)  into s_0_eq.
  concatenate 'Cur$:' p_keydt+4(2) '/' p_keydt+6(2)  into s_0_ev.

  concatenate 'GR:' g_next_30+4(2) '/' g_next_30+6(2)  into s_1_gr.
  concatenate 'GI:' g_next_30+4(2) '/' g_next_30+6(2)  into s_1_gi.
  concatenate 'EQ:' g_next_30+4(2) '/' g_next_30+6(2)  into s_1_eq.
  concatenate 'EV:' g_next_30+4(2) '/' g_next_30+6(2)  into s_1_ev.
  concatenate 'GR:' g_next_60+4(2) '/' g_next_60+6(2)  into s_1_gr.
  concatenate 'GI:' g_next_60+4(2) '/' g_next_60+6(2)  into s_1_gi.
  concatenate 'EQ:' g_next_60+4(2) '/' g_next_60+6(2)  into s_1_eq.
  concatenate 'EV:' g_next_60+4(2) '/' g_next_60+6(2)  into s_1_ev.
  concatenate 'GR:' g_next_90+4(2) '/' g_next_90+6(2)  into s_1_gr.
  concatenate 'GI:' g_next_90+4(2) '/' g_next_90+6(2)  into s_1_gi.
  concatenate 'EQ:' g_next_90+4(2) '/' g_next_90+6(2)  into s_1_eq.
  concatenate 'EV:' g_next_90+4(2) '/' g_next_90+6(2)  into s_1_ev.

  __catalog :
     'X' 'DATUM'    'Date'            10 'DATS' '' 'L' '',
     'X' 'RAUBE'    'Shop'            02 'CHAR' '' 'L' '',
     'X' 'MTART'    'M.type'          04 'CHAR' '' 'L' '',
     'X' 'BKLAS'    'V.Cls'           04 'CHAR' '' 'L' '',
     'X' 'PROFL'    'MIP/LP/KD'       01 'CHAR' '' 'L' '',
     'X' 'MATKL'    'MatGroup'        10 'CHAR' '' 'L' '',
     'X' 'MSTAE'    'X-Stat'          02 'CHAR' '' 'L' '',
     'X' 'MMSTA'    'P-Stat'          02 'CHAR' '' 'L' '',
     ' ' 'VVATAGE'  'LstMvmt'         06 'DEC'  '' 'R' '',
     'X' 'MATNR'    'Material'        18 'CHAR' '' 'L' '',
     ' ' 'MAKTX'    'Material Desc'   40 'CHAR' '' 'L' '',
     ' ' 'LBKUM'     s_0_eq           13 'DEC' '' 'R' 'MEINS',
     ' ' 'SALK3'     s_0_ev           16 'DEC' '' 'R' '',
     ' ' 'ZZBDMNG0'  s_0_gi           13 'DEC' '' 'R' 'MEINS',
     ' ' 'MNG00'     s_0_gr           13 'DEC' '' 'R' 'MEINS',
     ' ' 'ZZBDMNG1'  s_1_gi           13 'DEC' '' 'R' 'MEINS',
     ' ' 'MNG01'     s_1_gr           13 'DEC' '' 'R' 'MEINS',
     ' ' 'ZZLBKUM1'  s_1_eq           13 'DEC' '' 'R' 'MEINS',
     ' ' 'ZZSALK1'   s_1_ev           16 'DEC' '' 'R' '',
     ' ' 'ZZBDMNG2'  s_2_gi           13 'DEC' '' 'R' 'MEINS',
     ' ' 'MNG02'     s_2_gr           13 'DEC' '' 'R' 'MEINS',
     ' ' 'ZZLBKUM2'  s_2_eq           13 'DEC' '' 'R' 'MEINS',
     ' ' 'ZZSALK2'   s_2_ev           16 'DEC' '' 'R' '',
     ' ' 'ZZBDMNG3'  s_3_gi           13 'DEC' '' 'R' 'MEINS',
     ' ' 'MNG03'     s_3_gr           13 'DEC' '' 'R' 'MEINS',
     ' ' 'ZZLBKUM3'  s_3_eq           13 'DEC' '' 'R' 'MEINS',
     ' ' 'ZZSALK3'   s_3_ev           16 'DEC' '' 'R' '',
     ' ' 'STPRS'    'Std.Price'       10 'DEC' '' 'R' '',
     ' ' 'VERPR'    'Act.Price'       10 'DEC' '' 'R' '',
     ' ' 'MEINS'    'UoM'             05 'CHAR' '' 'L' '',
     ' ' 'PEINH'    'PrcUnit'         06 'CHAR' '' 'L' '',
     ' ' 'LIFNR'    'SourceList'      10 'CHAR' '' 'L' '',
     ' ' 'NAME1'    'Vnd Name'        20 'CHAR' '' 'L' '',
     ' ' 'DISPO'    'MRP Ctl'         3  'CHAR' '' 'L' '',
     ' ' 'EKGRP'    'Pur.Grp'         3 'CHAR'  '' 'L' '',
     ' ' 'BWKEY'    'Plant'           4 'CHAR'  '' 'L' '',
     ' ' 'DSDAT'    'MRP date'        8 'DATS'  '' 'L' ''.

  LOOP AT gt_fcat INTO gs_fcat.
*    CASE gs_fcat-fieldname.
    CASE gs_fcat-datatype.
      WHEN 'DEC'.
        gs_fcat-decimals = '0'.
        MODIFY gt_fcat FROM gs_fcat.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " CREATE_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*&      Form  set_lvc_layout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_lvc_layout.

  CLEAR gs_layo.

  gs_layo-edit       = 'X'.
  gs_layo-zebra      = 'X'.
  gs_layo-sel_mode   = 'A'.       " Column and row selection
  gs_layo-cwidth_opt = 'X'.
  gs_layo-ctab_fname = 'TABCOLOR'.
  gs_layo-stylefname = 'CELLTAB'.


ENDFORM.                    " set_lvc_layout
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CLEAR : g_error.

  ok_code = sy-ucomm.
  CLEAR sy-ucomm.
  CASE ok_code.
    WHEN 'BACK' OR 'CANC'.
      PERFORM free_container.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.

    WHEN 'REPL'.

      PERFORM get_r_delkz.
      PERFORM make_itab.

      PERFORM refresh_alv.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  free_container
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM free_container.

  IF NOT g_event_receiver IS INITIAL.
    FREE g_event_receiver.
  ENDIF.
  IF NOT g_event_route IS INITIAL.
    FREE g_event_route.
  ENDIF.

  IF NOT g_grid IS INITIAL.
    CALL METHOD g_grid->free.
  ENDIF.

  IF NOT g_custom_container IS INITIAL.
    CALL METHOD g_custom_container->free.
  ENDIF.

  FREE : g_grid,g_custom_container.

  CLEAR :  gs_layo,gt_exclude,it_out[],gt_fcat[],gt_sort[].

ENDFORM.                    " free_container
*&---------------------------------------------------------------------*
*&      Form  get_r_delkz
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_r_delkz.

  CLEAR : r_delkz[], r_delkz.

* MRP element
  IF p_mrp_a EQ 'X'.
    p_mrpar = 'X'.
    p_mrpba = 'X'.
    p_mrpbe = 'X'.
    p_mrpla = 'X'.
    p_mrple = 'X'.
    p_mrpsa = 'X'.
    p_mrpsb = 'X'.
    p_mrpsm = 'X'.
    p_mrpu2 = 'X'.
    p_mrpu3 = 'X'.
  ENDIF.

  r_delkz-sign = 'I'. r_delkz-option = 'EQ'.
  IF p_mrpar = 'X'. r_delkz-low = 'AR'. APPEND r_delkz. ENDIF.
  IF p_mrpba = 'X'. r_delkz-low = 'BA'. APPEND r_delkz. ENDIF.
  IF p_mrpbe = 'X'. r_delkz-low = 'BE'. APPEND r_delkz. ENDIF.
  IF p_mrpla = 'X'. r_delkz-low = 'LA'. APPEND r_delkz. ENDIF.
  IF p_mrple = 'X'. r_delkz-low = 'LE'. APPEND r_delkz. ENDIF.
  IF p_mrpsa = 'X'. r_delkz-low = 'SA'. APPEND r_delkz. ENDIF.
  IF p_mrpsb = 'X'. r_delkz-low = 'SB'. APPEND r_delkz. ENDIF.
  IF p_mrpsm = 'X'. r_delkz-low = 'SM'. APPEND r_delkz. ENDIF.
  IF p_mrpu2 = 'X'. r_delkz-low = 'U2'. APPEND r_delkz. ENDIF.
  IF p_mrpu3 = 'X'. r_delkz-low = 'U3'. APPEND r_delkz. ENDIF.

  READ TABLE r_delkz INDEX 1.
  IF sy-subrc NE 0.
    r_delkz = 'IEQ'.
    r_delkz-low = '^^'.
    APPEND r_delkz.
  ENDIF.

ENDFORM.                    " get_r_delkz
*&---------------------------------------------------------------------*
*&      Form  make_itab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_itab.

  PERFORM process_data_new.
  PERFORM prepare_it_out_new.
  IF p_nomrp = 'X'.
    PERFORM add_no_mrp_stock.
  ENDIF.

  PERFORM it_out_valuation.

*    PERFORM process_data.
*    PERFORM prepare_it_out.

  PERFORM delete_zero_record.
  PERFORM get_last_mvmt.
  PERFORM get_vendor_info.

ENDFORM.                    " make_itab
*&---------------------------------------------------------------------*
*&      Form  refresh_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh_alv.

*  __set_refresh_mode true.
  CALL METHOD g_grid->refresh_table_display
       EXPORTING is_stable = stable.

ENDFORM.                    " refresh_alv
