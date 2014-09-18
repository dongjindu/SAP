*&----------------------------------------------------------------------
*& Development ID :
*& Program ID     : ZAPP_ASMP_PIR
*& Program Name   : Create AS/MP Production Plan
*& Created by     : Victor Park
*& Created on     : 06.03.2014
*& Issue Doc No.  :
*&
*& Modification Log
*& Date        Developer Issue No Description
*&======================================================================
*&
*&----------------------------------------------------------------------
*& Desc.:
*&
*&----------------------------------------------------------------------

REPORT zapp_asmp_pir NO STANDARD PAGE HEADING
                                   LINE-SIZE 132
                                   LINE-COUNT 64(1)
                                   MESSAGE-ID zmpp.
TABLES: ztpp_asmp_pir, agr_users.
TYPE-POOLS: slis, vrm.
DATA: BEGIN OF it_tab_temp OCCURS 0,
      werks LIKE marc-werks,
      matnr LIKE mara-matnr,
      alc_vals LIKE ztpp_seq_sum02-alc_vals,
      END OF it_tab_temp.

DATA : BEGIN OF it_material OCCURS 0,
      matnr LIKE mara-matnr,
      maktx LIKE makt-maktx,
      bom_desc(10),
      idnrk LIKE stpo-idnrk,
      END OF it_material.

DATA: BEGIN OF it_tab OCCURS 0,
      werks LIKE marc-werks,
      matnr LIKE mara-matnr,
      matnr_s LIKE mara-matnr,
      labst LIKE mard-labst,
      seq(1),
      desc(15),
      mtd LIKE mdsm-bdmng,
      d-1 LIKE mdsm-bdmng,
      due_in  LIKE mdsm-bdmng,
      past_due LIKE mdsm-bdmng,
      qtyd_01 LIKE mdsm-bdmng,
      qtyd_02 LIKE mdsm-bdmng,
      qtyd_03 LIKE mdsm-bdmng,
      qtyd_04 LIKE mdsm-bdmng,
      qtyd_05 LIKE mdsm-bdmng,
      qtyd_06 LIKE mdsm-bdmng,
      qtyd_07 LIKE mdsm-bdmng,
      qtyd_08 LIKE mdsm-bdmng,
      qtyd_09 LIKE mdsm-bdmng,
      qtyd_10 LIKE mdsm-bdmng,
      qtyd_11 LIKE mdsm-bdmng,
      qtyd_12 LIKE mdsm-bdmng,
      qtyd_13 LIKE mdsm-bdmng,
      qtyd_14 LIKE mdsm-bdmng,
      qtyd_15 LIKE mdsm-bdmng,
      qtyd_16 LIKE mdsm-bdmng,
      qtyd_17 LIKE mdsm-bdmng,
      qtyd_18 LIKE mdsm-bdmng,
      qtyd_19 LIKE mdsm-bdmng,
      qtyd_20 LIKE mdsm-bdmng,
      qtyd_21 LIKE mdsm-bdmng,
      qtyw_04 LIKE mdsm-bdmng,
      qtyw_05 LIKE mdsm-bdmng,
      qtyw_06 LIKE mdsm-bdmng,
      qtyw_07 LIKE mdsm-bdmng,
      qtyw_08 LIKE mdsm-bdmng,
      qtyw_09 LIKE mdsm-bdmng,
      qtyw_10 LIKE mdsm-bdmng,
      qtyw_11 LIKE mdsm-bdmng,
      qtyw_12 LIKE mdsm-bdmng,
      qtyw_13 LIKE mdsm-bdmng,
      qtyw_14 LIKE mdsm-bdmng,
      qtyw_15 LIKE mdsm-bdmng,
      qtyw_16 LIKE mdsm-bdmng,
      qtyw_17 LIKE mdsm-bdmng,
      qtyw_18 LIKE mdsm-bdmng,
      qtyw_19 LIKE mdsm-bdmng,
      qtyw_20 LIKE mdsm-bdmng,
      qtyw_21 LIKE mdsm-bdmng,
      meins LIKE mara-meins,
      total LIKE mdsm-bdmng,
      maktx LIKE makt-maktx,
      if(4) TYPE c,
      celltab TYPE lvc_t_styl,
      cellcolor     TYPE  slis_t_specialcol_alv,
     END OF it_tab.

DATA: BEGIN OF it_output OCCURS 0,
      werks LIKE marc-werks,
      matnr LIKE mara-matnr,
      matnr_s LIKE mara-matnr,
      labst LIKE mard-labst,
      seq(1),
      desc(15),
*      mtd(13),
*      d-1(13),
      mtd  LIKE mdsm-bdmng,
      d-1 LIKE mdsm-bdmng,
      due_in  LIKE mdsm-bdmng,
      past_due LIKE mdsm-bdmng,
      qtyd_01(13),
      qtyd_02(13),
      qtyd_03(13),
      qtyd_04(13),
      qtyd_05(13),
      qtyd_06(13),
      qtyd_07(13),
      qtyd_08(13),
      qtyd_09(13),
      qtyd_10(13),
      qtyd_11(13),
      qtyd_12(13),
      qtyd_13(13),
      qtyd_14(13),
      qtyd_15(13),
      qtyd_16(13),
      qtyd_17(13),
      qtyd_18(13),
      qtyd_19(13),
      qtyd_20(13),
      qtyd_21(13),
      qtyw_04(13),
      qtyw_05(13),
      qtyw_06(13),
      qtyw_07(13),
      qtyw_08(13),
      qtyw_09(13),
      qtyw_10(13),
      qtyw_11(13),
      qtyw_12(13),
      qtyw_13(13),
      qtyw_14(13),
      qtyw_15(13),
      qtyw_16(13),
      qtyw_17(13),
      qtyw_18(13),
      qtyw_19(13),
      qtyw_20(13),
      qtyw_21(13),
*      MEINS LIKE MARA-MEINS,
      total LIKE mdsm-bdmng,
      maktx LIKE makt-maktx,
      if(4) TYPE c,
      celltab TYPE lvc_t_styl,
      cellcolor     TYPE  slis_t_specialcol_alv,
     END OF it_output.

DATA: wa_tot1 LIKE it_tab,
      wa_tot2 LIKE it_tab,
      wa_tot3 LIKE it_tab.

DATA: BEGIN OF it_total OCCURS 0,
      werks LIKE marc-werks,
      matnr LIKE mara-matnr,
      matnr_s LIKE mara-matnr,
      labst LIKE mard-labst,
      seq(1),
      desc(15),
*      mtd(13),
*      d-1(13),
      mtd  LIKE mdsm-bdmng,
      d-1 LIKE mdsm-bdmng,
      due_in  LIKE mdsm-bdmng,
      past_due LIKE mdsm-bdmng,
      qtyd_01(13),
      qtyd_02(13),
      qtyd_03(13),
      qtyd_04(13),
      qtyd_05(13),
      qtyd_06(13),
      qtyd_07(13),
      qtyd_08(13),
      qtyd_09(13),
      qtyd_10(13),
      qtyd_11(13),
      qtyd_12(13),
      qtyd_13(13),
      qtyd_14(13),
      qtyd_15(13),
      qtyd_16(13),
      qtyd_17(13),
      qtyd_18(13),
      qtyd_19(13),
      qtyd_20(13),
      qtyd_21(13),
      qtyw_04(13),
      qtyw_05(13),
      qtyw_06(13),
      qtyw_07(13),
      qtyw_08(13),
      qtyw_09(13),
      qtyw_10(13),
      qtyw_11(13),
      qtyw_12(13),
      qtyw_13(13),
      qtyw_14(13),
      qtyw_15(13),
      qtyw_16(13),
      qtyw_17(13),
      qtyw_18(13),
      qtyw_19(13),
      qtyw_20(13),
      qtyw_21(13),
*      MTD LIKE MDSM-BDMNG,
*      D-1 LIKE MDSM-BDMNG,
*      QTYD_01 LIKE MDSM-BDMNG,
*      QTYD_02 LIKE MDSM-BDMNG,
*      QTYD_03 LIKE MDSM-BDMNG,
*      QTYD_04 LIKE MDSM-BDMNG,
*      QTYD_05 LIKE MDSM-BDMNG,
*      QTYD_06 LIKE MDSM-BDMNG,
*      QTYD_07 LIKE MDSM-BDMNG,
*      QTYD_08 LIKE MDSM-BDMNG,
*      QTYD_09 LIKE MDSM-BDMNG,
*      QTYD_10 LIKE MDSM-BDMNG,
*      QTYD_11 LIKE MDSM-BDMNG,
*      QTYD_12 LIKE MDSM-BDMNG,
*      QTYD_13 LIKE MDSM-BDMNG,
*      QTYD_14 LIKE MDSM-BDMNG,
*      QTYD_15 LIKE MDSM-BDMNG,
*      QTYD_16 LIKE MDSM-BDMNG,
*      QTYD_17 LIKE MDSM-BDMNG,
*      QTYD_18 LIKE MDSM-BDMNG,
*      QTYD_19 LIKE MDSM-BDMNG,
*      QTYD_20 LIKE MDSM-BDMNG,
*      QTYD_21 LIKE MDSM-BDMNG,
*      QTYW_04 LIKE MDSM-BDMNG,
*      QTYW_05 LIKE MDSM-BDMNG,
*      QTYW_06 LIKE MDSM-BDMNG,
*      QTYW_07 LIKE MDSM-BDMNG,
*      QTYW_08 LIKE MDSM-BDMNG,
*      QTYW_09 LIKE MDSM-BDMNG,
*      QTYW_10 LIKE MDSM-BDMNG,
*      QTYW_11 LIKE MDSM-BDMNG,
*      QTYW_12 LIKE MDSM-BDMNG,
*      QTYW_13 LIKE MDSM-BDMNG,
*      QTYW_14 LIKE MDSM-BDMNG,
*      QTYW_15 LIKE MDSM-BDMNG,
*      QTYW_16 LIKE MDSM-BDMNG,
*      QTYW_17 LIKE MDSM-BDMNG,
*      QTYW_18 LIKE MDSM-BDMNG,
*      QTYW_19 LIKE MDSM-BDMNG,
*      QTYW_20 LIKE MDSM-BDMNG,
*      QTYW_21 LIKE MDSM-BDMNG,
*      MEINS LIKE MARA-MEINS,
      total LIKE mdsm-bdmng,
      maktx LIKE makt-maktx,
      if(4) TYPE c,
      celltab TYPE lvc_t_styl,
      cellcolor     TYPE  slis_t_specialcol_alv,
     END OF it_total.

DATA : lt_color TYPE slis_t_specialcol_alv. "Cell Color

*-G/I Qty
DATA : BEGIN OF it_gi_qty OCCURS 0,
        matnr LIKE mara-matnr,
        vbelv LIKE vbfa-vbelv,
        posnv LIKE vbfa-posnv,
        vbeln LIKE vbfa-vbeln,
        posnn LIKE mseg-zeile, "for Join
        mjahr LIKE mseg-mjahr,
        bwart LIKE mseg-bwart,
        erfmg LIKE mseg-erfmg,
        zbudat LIKE mseg-zbudat,
       END OF it_gi_qty.


*-Due In
DATA : it_duein_tmp LIKE ztsd_mobis_or OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_duein OCCURS 0,
        zwptno LIKE ztsd_mobis_or-zwptno,
        matnr  LIKE mara-matnr,
        zvkorg LIKE ztsd_mobis_or-zvkorg,
        zvtweg LIKE ztsd_mobis_or-zvtweg,
        zspart LIKE ztsd_mobis_or-zspart,
        zwreqqty TYPE i,
       END OF it_duein..

DATA : BEGIN OF it_stock OCCURS 0,
       matnr LIKE mard-matnr,
       sum01 LIKE mdkp-sum01,
       END OF it_stock.

DATA: BEGIN OF it_day OCCURS 21,
        seq(2)  TYPE n,
        datum   LIKE   sy-datum,
      END   OF it_day.
DATA : it_day_tmp LIKE it_day OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF it_day_conv OCCURS 21,
        seq(2)  TYPE n,
        datum   LIKE   sy-datum,
      END   OF it_day_conv.

DATA: BEGIN OF it_week OCCURS 21,
        seq(2)  TYPE n,
        datum   LIKE   sy-datum,
        datum_to LIKE  sy-datum,
      END   OF it_week.

DATA : z_max_date LIKE sy-datum,
       z_beg_date LIKE sy-datum,
       z_max_week LIKE sy-datum,
       z_beg_week LIKE sy-datum,
       w_max_day_cn(2) TYPE n,
       w_datum LIKE sy-datum,
       w_prgrp LIKE pgmi-prgrp,
       w_d-1 LIKE sy-datum,
       w_mtd LIKE sy-datum,
       l_yesterday  LIKE sy-datum.

DATA: ok_code      LIKE sy-ucomm,
      w_repid  LIKE sy-repid,
      w_cnt       TYPE   i,
      w_no_data(1),
      w_prdt(10),
      w_flag(1).

DATA: xname    TYPE vrm_id,
      xlist    TYPE vrm_values,
      xvalue   LIKE LINE OF xlist.

DATA:  l_kalid LIKE kako-kalid.

DATA: wa_stbl  TYPE lvc_s_stbl.
DATA : it_fieldcat     TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldcat_tot  TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldname    TYPE slis_t_fieldcat_alv,
       it_fname_tot    TYPE slis_t_fieldcat_alv,
       it_sort         TYPE lvc_t_sort WITH HEADER LINE,
       it_exclude      TYPE ui_functions,
       it_exclude_tot  TYPE ui_functions.

DATA : wa_is_layout TYPE lvc_s_layo, "/The Layout Structure
       wa_tot_layout TYPE lvc_s_layo,
       w_fieldname  LIKE LINE OF it_fieldcat.

DATA: wa_save    TYPE c   VALUE 'A',   "for Parameter I_SAVE
      wa_variant TYPE disvariant,      "for parameter IS_VARIANT
      wa_tot_save  TYPE c   VALUE 'A',   "for Parameter I_SAVE
      wa_tot_variant TYPE disvariant.     "for parameter IS_VARIANT

DATA: wa_custom_control TYPE        scrfname VALUE 'ALV_CONTAINER',
      alv_grid          TYPE REF TO cl_gui_alv_grid,
      grid_container    TYPE REF TO cl_gui_custom_container.

DATA: wa_custom_control_tot TYPE    scrfname VALUE 'ALV_CONTAINER_TOT',
      alv_grid_tot          TYPE REF TO cl_gui_alv_grid,
      grid_container_tot    TYPE REF TO cl_gui_custom_container.

FIELD-SYMBOLS : <fs01>, <fs-qty>.

**--- Constants

** for E002
*CONSTANTS : C_WERKS LIKE MARC-WERKS VALUE 'E001'.

DATA:  w_refresh(1),
       w_new(1) VALUE 'X'.

DATA : w_werks TYPE werks_d.  "MARC-WERKS.
*DATA  SAVE_DATA.

* -------------------------------------------------------------
* EVent class
*-----------------------------------------------------------
* local class to handle semantic checks
CLASS lcl_event_receiver DEFINITION DEFERRED.

DATA: g_event_receiver TYPE REF TO lcl_event_receiver.

*************************************************************
* LOCAL CLASS Definition
**************************************************************
*§4.Define and implement event handler to handle event DATA_CHANGED.
*
CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.
    METHODS:
      handle_data_changed
         FOR EVENT data_changed OF cl_gui_alv_grid
              IMPORTING er_data_changed.

    DATA: error_in_data TYPE c.

ENDCLASS.                    "LCL_EVENT_RECEIVER DEFINITION
DATA :it_lvc  LIKE lvc_s_row.
*************************************************************
* LOCAL CLASS IMPLEMENTATION
**************************************************************
CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD handle_data_changed.

    DATA: ls_good TYPE lvc_s_modi,
          lv_value TYPE lvc_value,
          w_qty(13),
          lvc_t_row TYPE lvc_t_row.

    error_in_data = space.
    LOOP AT er_data_changed->mt_good_cells INTO ls_good.
      CASE ls_good-fieldname.
* check if column Name1 of this row was changed
        WHEN 'QTYD_01' OR 'QTYD_02' OR 'QTYD_03' OR 'QTYD_04' OR
             'QTYD_05' OR 'QTYD_06' OR 'QTYD_07' OR 'QTYD_08' OR
             'QTYD_09' OR 'QTYD_10' OR 'QTYD_11' OR 'QTYD_12' OR
             'QTYD_13' OR 'QTYD_14' OR 'QTYD_15' OR 'QTYD_16' OR
             'QTYD_17' OR 'QTYD_18' OR 'QTYD_19' OR 'QTYD_20' OR
             'QTYD_21'.
          CALL METHOD er_data_changed->get_cell_value
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = ls_good-fieldname
            IMPORTING
              e_value     = lv_value.
          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = ls_good-fieldname
              i_value     = lv_value.
        WHEN 'QTYW_04' OR 'QTYW_05' OR 'QTYW_06' OR 'QTYW_07' OR
             'QTYW_08' OR 'QTYW_09' OR 'QTYW_10' OR 'QTYW_11' OR
             'QTYW_12' OR 'QTYW_13' OR 'QTYW_14' OR 'QTYW_15' OR
             'QTYW_16' OR 'QTYW_17' OR 'QTYW_18' OR 'QTYW_19' OR
             'QTYW_20' OR 'QTYW_21'.

          CALL METHOD er_data_changed->get_cell_value
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = ls_good-fieldname
            IMPORTING
              e_value     = lv_value.
          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = ls_good-fieldname
              i_value     = lv_value.

      ENDCASE.
    ENDLOOP.

*§7.Display application log if an error has occured.
    IF error_in_data EQ 'X'.
      CALL METHOD er_data_changed->display_protocol.
    ENDIF.

  ENDMETHOD.                    "HANDLE_DATA_CHANGED

ENDCLASS.                    "LCL_EVENT_RECEIVER IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
FORM get_data.
  PERFORM get_material.
  PERFORM get_order_quantity.
  PERFORM get_stock_quantity.

ENDFORM.                    "GET_DATA



*---------------------------------------------------------------------*
*       FORM set_days                                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM set_days.
  DATA: l_count TYPE i.
  DATA: l_date LIKE sy-datum.

  CLEAR: it_day, it_day[].

*  SELECT SINGLE KALID INTO L_KALID
*    FROM ZVPP_CAPACITY
*   WHERE ARBPL = 'T'   .

** Furong on 02/07/12 for E002
* SELECT SINGLE KALID INTO L_KALID
*    FROM ZVPP_CAPACITY
*   WHERE ARBPL = 'T'
*     AND WERKS = W_WERKS.

  IF w_werks = 'P001'.
    SELECT SINGLE kalid INTO l_kalid
       FROM zvpp_capacity
      WHERE arbpl = 'B'.
  ELSE.
*    l_kalid = 'HM'.
    l_kalid = 'ZS'.
  ENDIF.
** End on 02/07/12

*  PERFORM READ_SHOP_CALID  USING L_KALID.
** Furong on 02/08/12 for E002
*  SELECT MAX( SQDT ) INTO Z_MAX_DATE
*    FROM ZTPP_PMT07JB_A
*     WHERE GUBB = 'A'.

  z_max_date = z_beg_week.
** End on 02/08/12

  IF z_beg_date > z_max_date.
    z_max_date = z_beg_date.
  ENDIF.

  it_day-seq = 1.
  it_day-datum = z_beg_date.
  APPEND it_day.
  l_count = '01'.
  l_date = z_beg_date.

  WHILE l_date < z_max_date.
    l_count  = l_count + 1.
    l_date   = l_date  + 1.
    PERFORM read_working_date USING '+' l_kalid  l_date.
    IF l_date >= z_max_date.
      EXIT.
    ELSE.
      it_day-seq     = l_count.
      it_day-datum   = l_date .
      APPEND it_day.  CLEAR: it_day.
    ENDIF.
  ENDWHILE.

*-< Victor 06.09.2014  fixed Daily period : only display 21 days
  DELETE it_day WHERE seq > 21.
*->

** Furong on 02/14/12 for E002
*  IF w_max_day_cn > 1.
*    w_max_day_cn = l_count - 1.
*  ELSE.
*    w_max_day_cn = l_count.
*  ENDIF.
  DESCRIBE TABLE it_day LINES w_max_day_cn.
** END ON 02/14/12
  w_d-1 = z_beg_date - 1.
  CONCATENATE w_d-1+0(6) '01' INTO w_mtd.
ENDFORM.                    " set_DAYS
*&---------------------------------------------------------------------*
*&      Form  read_shop_calid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_KALID  text
*----------------------------------------------------------------------*
FORM read_shop_calid USING p_l_kalid.

** Furong on 02/07/12
*  SELECT SINGLE KALID INTO P_L_KALID
*  FROM ZVPP_CAPACITY
* WHERE ARBPL = 'T'   .
  IF w_werks = 'P001'.
    SELECT SINGLE kalid INTO l_kalid
    FROM zvpp_capacity
    WHERE arbpl = 'T'.
  ELSE.
*    l_kalid = 'HM'.
    l_kalid = 'ZS'.
  ENDIF.
** End on 02/107/12


ENDFORM.                    " read_shop_calid
*---------------------------------------------------------------------*
*       FORM read_working_date                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  PA_TYPE                                                       *
*  -->  PA_KALID                                                      *
*  -->  PA_WDATE                                                      *
*---------------------------------------------------------------------*
FORM read_working_date USING  pa_type  pa_kalid  pa_wdate.
  CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
    EXPORTING
      correct_option               = pa_type
      date                         = pa_wdate
      factory_calendar_id          = pa_kalid
    IMPORTING
      date                         = pa_wdate
    EXCEPTIONS
      calendar_buffer_not_loadable = 1
      correct_option_invalid       = 2
      date_after_range             = 3
      date_before_range            = 4
      date_invalid                 = 5
      factory_calendar_not_found   = 6
      OTHERS                       = 7.
ENDFORM.                    " READ_WORKING_DATE


INCLUDE zapp_asmp_pir_pbo.
INCLUDE zapp_asmp_pir_pai.

*&---------------------------------------------------------------------*
*&      Form  SET_WEEKS
*&---------------------------------------------------------------------*
FORM set_weeks.
  DATA: BEGIN OF lt_week OCCURS 0,
        sqdt LIKE sy-datum,
        END OF lt_week.
  DATA: l_cn(2) TYPE n.

  CLEAR: it_week, it_week[].
  SELECT sqdt INTO TABLE lt_week
   FROM ztpp_pmt07jb_a
    WHERE gubb EQ 'B'.

  SORT lt_week BY sqdt.
  DELETE ADJACENT DUPLICATES FROM lt_week COMPARING sqdt.

  l_cn = '04'.
  LOOP AT lt_week.
    it_week-seq = l_cn.
    it_week-datum = lt_week-sqdt.
    it_week-datum_to = lt_week-sqdt + 6. "Sunday : from Mon.to Sun.
    APPEND it_week.
    l_cn =  l_cn + 1.
  ENDLOOP.

  DESCRIBE TABLE it_week LINES l_cn.
  READ TABLE it_week INDEX 1.
  z_beg_week = it_week-datum.

  READ TABLE it_week INDEX l_cn.
  z_max_week = it_week-datum.

ENDFORM.                    " SET_WEEKS
*&---------------------------------------------------------------------*
*&      Form  SELECT_EDIT_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_edit_line.
  DATA: lt_celltab TYPE lvc_t_styl,
        w_celltab TYPE lvc_s_styl,
        l_index TYPE i,
        l_mode TYPE raw4.

  LOOP AT it_tab.
    l_index = sy-tabix.
    REFRESH lt_celltab.
    IF it_tab-seq = '3'.
      l_mode = cl_gui_alv_grid=>mc_style_enabled.
    ELSE.
      l_mode = cl_gui_alv_grid=>mc_style_disabled.
    ENDIF.

    w_celltab-fieldname = 'MATNR_S'.
    w_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'MAKTX'.
    w_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'DESC'.
    w_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'MTD'.
    w_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'D-1'.
    w_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT w_celltab INTO TABLE lt_celltab.

    w_celltab-fieldname = 'DUE_IN'.
    w_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT w_celltab INTO TABLE lt_celltab.

    w_celltab-fieldname = 'PAST_DUE'.
    w_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT w_celltab INTO TABLE lt_celltab.

    w_celltab-fieldname = 'QTYD_01'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYD_02'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYD_03'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYD_04'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYD_05'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYD_06'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYD_07'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYD_08'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYD_09'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYD_10'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYD_11'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYD_12'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYD_13'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYD_14'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYD_15'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYD_16'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYD_17'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYD_18'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYD_19'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYD_20'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYD_21'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.

    w_celltab-fieldname = 'QTYW_04'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYW_05'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYW_06'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYW_07'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYW_08'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYW_09'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYW_10'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYW_11'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYW_12'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYW_13'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYW_14'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYW_15'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYW_16'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYW_17'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYW_18'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYW_19'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYW_20'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYW_21'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.

    INSERT LINES OF lt_celltab INTO TABLE it_tab-celltab.
    MODIFY it_tab INDEX l_index.
  ENDLOOP.

ENDFORM.                    " SELECT_EDIT_LINE
*&---------------------------------------------------------------------*
*&      Form  PREPARE_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM prepare_display.
  DATA: l_qty TYPE i,
        l_cn(2) TYPE n,
        l_qty1(13), " LIKE IT_TAB-QTYD_01.
        l_text(35).
  DATA: BEGIN OF lt_temp OCCURS 0,
        werks LIKE marc-werks,
        matnr LIKE mara-matnr,
        matnr_s LIKE mara-matnr,
        labst LIKE mard-labst,
        seq(1),
        desc(15),
        mtd TYPE i,
        d-1 TYPE i,
        due_in  LIKE mdsm-bdmng,
        past_due LIKE mdsm-bdmng,
        qtyd_01 TYPE i,
        qtyd_02 TYPE i,
        qtyd_03 TYPE i,
        qtyd_04 TYPE i,
        qtyd_05 TYPE i,
        qtyd_06 TYPE i,
        qtyd_07 TYPE i,
        qtyd_08 TYPE i,
        qtyd_09 TYPE i,
        qtyd_10 TYPE i,
        qtyd_11 TYPE i,
        qtyd_12 TYPE i,
        qtyd_13 TYPE i,
        qtyd_14 TYPE i,
        qtyd_15 TYPE i,
        qtyd_16 TYPE i,
        qtyd_17 TYPE i,
        qtyd_18 TYPE i,
        qtyd_19 TYPE i,
        qtyd_20 TYPE i,
        qtyd_21 TYPE i,
        qtyw_04 TYPE i,
        qtyw_05 TYPE i,
        qtyw_06 TYPE i,
        qtyw_07 TYPE i,
        qtyw_08 TYPE i,
        qtyw_09 TYPE i,
        qtyw_10 TYPE i,
        qtyw_11 TYPE i,
        qtyw_12 TYPE i,
        qtyw_13 TYPE i,
        qtyw_14 TYPE i,
        qtyw_15 TYPE i,
        qtyw_16 TYPE i,
        qtyw_17 TYPE i,
        qtyw_18 TYPE i,
        qtyw_19 TYPE i,
        qtyw_20 TYPE i,
        qtyw_21 TYPE i,
        meins LIKE mara-meins,
        total LIKE mdsm-bdmng,
        maktx LIKE makt-maktx,
        if(4) TYPE c,
        celltab TYPE lvc_t_styl,
       END OF lt_temp.

  CLEAR: it_output, it_output[], it_total, it_total[],
         wa_tot1, wa_tot2, wa_tot3.
  LOOP AT it_tab.
    MOVE-CORRESPONDING it_tab TO it_output.
    l_qty = it_tab-mtd.
    it_output-mtd = l_qty.
    l_qty = it_tab-d-1.
    it_output-d-1 = l_qty.

    CASE it_tab-seq.
      WHEN '1'.
        wa_tot1-mtd = wa_tot1-mtd + it_tab-mtd.
        wa_tot1-d-1 = wa_tot1-d-1 + it_tab-d-1.
        wa_tot1-due_in = wa_tot1-due_in + it_tab-due_in.
        wa_tot1-past_due = wa_tot1-past_due + it_tab-past_due.
      WHEN '2'.
        wa_tot2-mtd = wa_tot2-mtd + it_tab-mtd.
        wa_tot2-d-1 = wa_tot2-d-1 + it_tab-d-1.
      WHEN '3'.
        wa_tot3-mtd = wa_tot3-mtd + it_tab-mtd.
        wa_tot3-d-1 = wa_tot3-d-1 + it_tab-d-1.
    ENDCASE.

    l_cn = '00'.
    DO 21 TIMES.
      l_cn =  l_cn + 1.
      CONCATENATE 'IT_TAB-QTYD_' l_cn INTO l_text.
      ASSIGN (l_text) TO <fs01>.
      IF sy-subrc = 0.
        l_qty = <fs01>.
        l_qty1 = l_qty.

        CASE it_tab-seq.
          WHEN '1'.
            CONCATENATE 'WA_TOT1-QTYD_' l_cn INTO l_text.
            ASSIGN (l_text) TO <fs01>.
            IF sy-subrc = 0.
              <fs01> = <fs01> + l_qty.
            ENDIF.
          WHEN '2'.
            CONCATENATE 'WA_TOT2-QTYD_' l_cn INTO l_text.
            ASSIGN (l_text) TO <fs01>.
            IF sy-subrc = 0.
              <fs01> = <fs01> + l_qty.
            ENDIF.
          WHEN '3'.
            CONCATENATE 'WA_TOT3-QTYD_' l_cn INTO l_text.
            ASSIGN (l_text) TO <fs01>.
            IF sy-subrc = 0.
              <fs01> = <fs01> + l_qty.
            ENDIF.
        ENDCASE.

        CONCATENATE 'IT_OUTPUT-QTYD_' l_cn INTO l_text.
        ASSIGN (l_text) TO <fs-qty>.
        IF sy-subrc = 0.
          <fs-qty> = l_qty1.
        ENDIF.
      ENDIF.
    ENDDO.
    l_cn = '03'.
    DO 18 TIMES.
      l_cn =  l_cn + 1.
      CONCATENATE 'IT_TAB-QTYW_' l_cn INTO l_text.
      ASSIGN (l_text) TO <fs01>.
      IF sy-subrc = 0.
        l_qty = <fs01>.
        l_qty1 = l_qty.
** total
*        CONCATENATE 'IT_TOTAL-QTYW_' L_CN INTO L_TEXT.
*        ASSIGN (L_TEXT) TO <FS01>.
*        IF SY-SUBRC = 0.
*          <FS01> = <FS01> + L_QTY.
*        ENDIF.
**
        CASE it_tab-seq.
          WHEN '1'.
            CONCATENATE 'WA_TOT1-QTYW_' l_cn INTO l_text.
            ASSIGN (l_text) TO <fs01>.
            IF sy-subrc = 0.
              <fs01> = <fs01> + l_qty.
            ENDIF.
          WHEN '2'.
            CONCATENATE 'WA_TOT2-QTYW_' l_cn INTO l_text.
            ASSIGN (l_text) TO <fs01>.
            IF sy-subrc = 0.
              <fs01> = <fs01> + l_qty.
            ENDIF.
          WHEN '3'.
            CONCATENATE 'WA_TOT3-QTYW_' l_cn INTO l_text.
            ASSIGN (l_text) TO <fs01>.
            IF sy-subrc = 0.
              <fs01> = <fs01> + l_qty.
            ENDIF.
        ENDCASE.

      ENDIF.
      CONCATENATE 'IT_OUTPUT-QTYW_' l_cn INTO l_text.
      ASSIGN (l_text) TO <fs-qty>.
      IF sy-subrc = 0.
        <fs-qty> = l_qty1.
      ENDIF.
    ENDDO.
    APPEND it_output.
  ENDLOOP.
*  wa_tot1-matnr_s = 'Total'. "07.18.2014
  CONCATENATE w_prdt 'Total' INTO wa_tot1-maktx SEPARATED BY space.
  IF w_prdt = 'MP-PNL'.
    wa_tot1-desc = 'To be used'.
  ELSE.
    wa_tot1-desc = 'Order Qty'.
  ENDIF.
  wa_tot1-if = 'C410'.
  MOVE-CORRESPONDING wa_tot1 TO lt_temp.
  MOVE-CORRESPONDING lt_temp TO it_total.
  APPEND it_total.
  wa_tot2-desc = 'Stock(MV)'.
  wa_tot2-if = 'C410'.
  MOVE-CORRESPONDING wa_tot2 TO lt_temp.
  MOVE-CORRESPONDING lt_temp TO it_total.
  APPEND it_total.
  wa_tot3-desc = 'Plan'.
  wa_tot3-if = 'C410'.
  MOVE-CORRESPONDING wa_tot3 TO lt_temp.
  MOVE-CORRESPONDING lt_temp TO it_total.
  APPEND it_total.

*  MOVE-CORRESPONDING WA_TOT1 TO IT_TOTAL.
*  APPEND IT_TOTAL.
*  WA_TOT2-DESC = 'Stock'.
*  WA_TOT2-IF = 'C410'.
*  MOVE-CORRESPONDING WA_TOT2 TO IT_TOTAL.
*  APPEND IT_TOTAL.
*  WA_TOT3-DESC = 'Plan'.
*  WA_TOT3-IF = 'C410'.
*  MOVE-CORRESPONDING WA_TOT3 TO IT_TOTAL.
*  APPEND IT_TOTAL.
ENDFORM.                    " PREPARE_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  CLEAR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_data.
  REFRESH: it_tab_temp, it_tab, it_output, it_stock, it_total.
  CLEAR: it_tab_temp, it_tab, it_output, it_stock, it_total, wa_tot1,
         wa_tot2, wa_tot3.
ENDFORM.                    " CLEAR_DATA
*&---------------------------------------------------------------------*
*&      Form  SET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_data.
  DATA: c_jobs(40) VALUE 'ZAPP903R_INPUT_PLAN',
        c_key1(18) VALUE 'BODY_INPUT'.

  SELECT SINGLE dates INTO z_beg_date
    FROM ztpp_common_vals
   WHERE jobs = c_jobs
     AND key2 = c_key1.

  SELECT MAX( wdatu ) INTO w_datum
  FROM ztpp_asmp_pir.

ENDFORM.                    " SET_DATA
*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_data.
  PERFORM clear_data.

  PERFORM get_data.

  IF sy-tcode = 'ZAPP_ASMP_PIR'.
    PERFORM select_edit_line.
  ENDIF.
  PERFORM prepare_display.

ENDFORM.                    " SELECT_DATA
*&---------------------------------------------------------------------*
*&      Form  SET_DAYS_CONVER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_days_conver .
  DATA: l_count TYPE i.
  DATA: l_date LIKE sy-datum.
  DATA: l_kalid_conv LIKE kako-kalid.

  CLEAR: it_day_conv, it_day_conv[].

*  l_kalid_conv = 'HM'.
  l_kalid_conv = 'ZS'.

  it_day_conv-seq = 1.
  it_day_conv-datum = z_beg_date.
  APPEND it_day_conv.
  l_count = '01'.
  l_date = z_beg_date.

  WHILE l_date < z_max_date.
    l_count  = l_count + 1.
    l_date   = l_date  + 1.
    PERFORM read_working_date USING '+' l_kalid_conv  l_date.
    IF l_date >= z_max_date.
      EXIT.
    ELSE.
      it_day_conv-seq     = l_count.
      it_day_conv-datum   = l_date .
      APPEND it_day_conv.  CLEAR: it_day_conv.
    ENDIF.
  ENDWHILE.

ENDFORM.                    " SET_DAYS_CONVER
*&---------------------------------------------------------------------*
*&      Form  GET_MATERIAL
*&---------------------------------------------------------------------*
FORM get_material .
  DATA : BEGIN OF it_stpo OCCURS 0,
          matnr LIKE mara-matnr,
          idnrk LIKE stpo-idnrk,
        END OF it_stpo.

  SELECT a~nrmit AS matnr b~maktx
  INTO CORRESPONDING FIELDS OF TABLE it_material
  FROM pgmi AS a INNER JOIN makt AS b
              ON a~nrmit  = b~matnr
  WHERE werks =  w_werks
    AND prgrp =  w_prdt.

  IF it_material[] IS INITIAL.
    MESSAGE e000 WITH 'There is No data'.

  ENDIF.

  SELECT  a~matnr b~idnrk
    INTO CORRESPONDING FIELDS OF TABLE it_stpo
  FROM mast AS a INNER JOIN stpo AS b
           ON a~stlnr = b~stlnr
    FOR ALL ENTRIES IN it_material
  WHERE a~matnr = it_material-matnr
    AND a~werks = w_werks
    AND a~stlan = '1'
*    AND A~STLAL =    "Alt BOM????
    AND b~stlty = 'M'
    AND b~idnrk LIKE 'MV%'.

  SORT it_stpo BY matnr.

  LOOP AT it_material.
    READ TABLE it_stpo WITH KEY matnr = it_material-matnr
                                       BINARY SEARCH.
    IF sy-subrc = 0.
      it_material-idnrk = it_stpo-idnrk.
    ELSE.
      it_material-bom_desc = 'No BOM'.
    ENDIF.
    MODIFY it_material.
  ENDLOOP.
ENDFORM.                    " GET_MATERIAL
*&---------------------------------------------------------------------*
*&      Form  GET_QUANTITY
*&---------------------------------------------------------------------*
FORM get_order_quantity .
  DATA : BEGIN OF it_order_qty OCCURS 0,
          matnr LIKE mara-matnr,
          vbeln LIKE vapma-vbeln,
          posnr LIKE vapma-posnr,
          wmeng LIKE vbep-wmeng,
          edatu LIKE vbep-edatu,
        END OF it_order_qty.

  DATA : BEGIN OF it_cal_order OCCURS 0,
          matnr LIKE mara-matnr,
          edatu LIKE vbep-edatu,
          wmeng LIKE vbep-wmeng,
          cal_qty LIKE vbep-wmeng,
          seq(2)  TYPE n,
          holiday_chk(1),
        END OF it_cal_order.

  DATA : BEGIN OF it_cal_gi OCCURS 0,
          matnr LIKE mara-matnr,
          erfmg LIKE mseg-erfmg,
         END OF it_cal_gi.


  DATA : it_gi_qty_tmp LIKE it_gi_qty OCCURS 0 WITH HEADER LINE.

  DATA : l_sum_qty  LIKE vbep-wmeng,
         l_rest_qty LIKE vbep-wmeng,
         l_week_qty LIKE vbep-wmeng.


  DATA : l_fdate_tmp TYPE sy-datum,
         l_ldate_tmp TYPE sy-datum.

  DATA: l_line TYPE i,
        l_cn(2) TYPE n,
        l_text(30).


  DATA : wa_knmt TYPE knmt,
         l_kdmat LIKE knmt-kdmat.

  CHECK it_material[] IS NOT INITIAL.

  CLEAR : it_duein[], it_duein.

  l_yesterday = sy-datum - 1.

  IF w_prdt <> 'MP-PNL'.

*-Sales Order Qty
    SELECT a~matnr a~vbeln a~posnr c~wmeng c~edatu
      INTO CORRESPONDING FIELDS OF TABLE it_order_qty
    FROM vapma AS a INNER JOIN vbup AS b
               ON a~vbeln = b~vbeln
              AND a~posnr = b~posnr
               INNER JOIN vbep AS c
               ON a~vbeln = c~vbeln
              AND a~posnr = c~posnr
      FOR ALL ENTRIES IN it_material
     WHERE a~matnr = it_material-matnr
       AND a~vkorg = 'D100'
       AND a~vtweg = '30'
       AND a~spart = '30'
       AND a~kunnr = 'MOBIS'
       AND b~lfsta IN ('A', 'B')
       and b~lfgsa in ('A', 'B')
       AND c~wmeng > 0.

*-GI Qty
    IF it_order_qty[] IS NOT INITIAL.

      SELECT a~matnr  a~vbelv a~posnv a~vbeln a~posnn  a~mjahr
         "    b~mjahr  b~bwart b~erfmg b~zbudat
        INTO CORRESPONDING FIELDS OF TABLE it_gi_qty_tmp
      FROM vbfa AS a  "INNER JOIN mseg AS b
*            ON a~vbeln  = b~mblnr
**           AND a~posnn  = b~zeile
*           AND a~mjahr  = b~mjahr
        FOR ALL ENTRIES IN it_order_qty
      WHERE vbelv = it_order_qty-vbeln
        AND posnv = it_order_qty-posnr
        AND a~vbtyp_n = 'R'.
    ENDIF.

    IF it_gi_qty_tmp[] IS NOT INITIAL.
      SELECT b~mblnr AS vbeln b~mjahr  b~zeile AS posnn
             b~bwart b~erfmg b~zbudat
        INTO CORRESPONDING FIELDS OF TABLE it_gi_qty
      FROM  mseg AS b
        FOR ALL ENTRIES IN it_gi_qty_tmp
      WHERE b~mblnr = it_gi_qty_tmp-vbeln
        AND b~zeile = it_gi_qty_tmp-posnn
        AND b~mjahr = it_gi_qty_tmp-mjahr
        AND b~bwart = '601'
        AND NOT EXISTS ( SELECT * FROM mseg
                         WHERE mseg~smbln  = b~mblnr
                           AND mseg~smblp  = b~zeile
                           AND mseg~sjahr  = b~mjahr
                           AND mseg~lfbja  <> '0000'
        %_HINTS ORACLE 'INDEX(t_100 "MSEG~S")'
 ).
    ENDIF.

    SORT it_gi_qty_tmp BY  vbeln posnn  mjahr.
    LOOP AT it_gi_qty.
      READ TABLE  it_gi_qty_tmp WITH KEY vbeln = it_gi_qty-vbeln
                                         posnn = it_gi_qty-posnn
                                         mjahr = it_gi_qty-mjahr
                                         BINARY SEARCH.
      IF sy-subrc = 0.
        it_gi_qty-matnr = it_gi_qty_tmp-matnr.
        it_gi_qty-vbelv = it_gi_qty_tmp-vbelv.
        it_gi_qty-posnv =  it_gi_qty_tmp-posnv.
        MODIFY  it_gi_qty.
      ENDIF.
    ENDLOOP.

*-Due In Qty
    SELECT  zwptno  zvkorg  zvtweg  zspart zwreqqty
            INTO CORRESPONDING FIELDS OF TABLE it_duein_tmp
    FROM ztsd_mobis_or
    WHERE vbeln = ''.

    LOOP AT it_duein_tmp.
      MOVE-CORRESPONDING it_duein_tmp TO it_duein.
      COLLECT it_duein.
    ENDLOOP.

    LOOP AT it_duein.
      CLEAR : wa_knmt.

      l_kdmat  = it_duein-zwptno.
      CALL FUNCTION 'RV_CUSTOMER_MATERIAL_READ'
        EXPORTING
          cmr_kdmat      = l_kdmat
          cmr_kunnr      = 'MOBIS'
          cmr_spart      = it_duein-zspart
          cmr_vkorg      = it_duein-zvkorg
          cmr_vtweg      = it_duein-zvtweg
        IMPORTING
          cmr_knmt       = wa_knmt
        EXCEPTIONS
          knmt_not_found = 1
          OTHERS         = 2.
      IF sy-subrc = 0.
        it_duein-matnr  = wa_knmt-matnr.
        MODIFY it_duein.
      ENDIF.
    ENDLOOP.

*  SELECT a~matnr  a~vbelv a~posnv a~vbeln a~posnn
*         b~mjahr  b~bwart b~erfmg b~zbudat
*    INTO CORRESPONDING FIELDS OF TABLE it_gi_qty
*  FROM vbfa AS a  INNER JOIN mseg AS b
*            ON a~vbeln  = b~mblnr
**           AND a~posnn  = b~zeile
*           AND a~mjahr  = b~mjahr
*    FOR ALL ENTRIES IN it_order_qty
*  WHERE vbelv = it_order_qty-vbeln
*    AND posnv = it_order_qty-posnr
*    AND a~vbtyp_n = 'R'
*    AND NOT EXISTS ( SELECT * FROM mseg AS c
*                     WHERE smbln  = b~mblnr
*                       AND smblp  = b~zeile
*                       AND sjahr  = b~mjahr ).

    SORT it_duein BY matnr.
    SORT it_order_qty BY matnr edatu.
    SORT it_gi_qty    BY matnr zbudat.
    LOOP AT it_order_qty.
      MOVE-CORRESPONDING it_order_qty TO it_cal_order.
      COLLECT it_cal_order.
    ENDLOOP.

    LOOP AT it_gi_qty.
      MOVE-CORRESPONDING it_gi_qty TO it_cal_gi.
      COLLECT it_cal_gi.
    ENDLOOP.

    LOOP AT it_cal_order.

      AT NEW matnr.
        CLEAR : l_rest_qty.
        READ TABLE it_cal_gi WITH KEY matnr = it_cal_order-matnr.
        IF sy-subrc = 0.
          l_rest_qty  = it_cal_gi-erfmg.
*        l_rest_qty = l_sum_qty.
        ELSE.
          it_cal_order-cal_qty  = it_cal_order-wmeng.
        ENDIF.
      ENDAT.

      IF it_cal_order-cal_qty = 0.
        it_cal_order-cal_qty = it_cal_order-wmeng - l_rest_qty.
        IF it_cal_order-cal_qty < 0.
          l_rest_qty = l_rest_qty - it_cal_order-wmeng.
        ELSE.
          l_rest_qty = 0.
        ENDIF.
      ENDIF.

      MODIFY it_cal_order.
    ENDLOOP.

*-<calculate holiday s/o qty -> apply qty to the latest date
    it_day_tmp[] = it_day[].
    SORT it_day_tmp BY datum DESCENDING.
    READ TABLE it_day_tmp INDEX 1.
    READ TABLE it_day INDEX 1.
    l_fdate_tmp = it_day-datum.  "Fisrt date
    l_ldate_tmp = it_day_tmp-datum.  "Last date

    LOOP AT it_cal_order.
      CHECK l_fdate_tmp <= it_cal_order-edatu
          AND l_ldate_tmp >= it_cal_order-edatu.

      READ TABLE it_day WITH KEY datum = it_cal_order-edatu.
      IF sy-subrc <> 0.
        LOOP AT it_day_tmp WHERE datum < it_cal_order-edatu.
          it_cal_order-seq  = it_day_tmp-seq.
          EXIT.
        ENDLOOP.

        it_cal_order-holiday_chk = 'X'.
        MODIFY it_cal_order.
      ENDIF.
    ENDLOOP.
*->

  ELSE.  "get only MP D-1 qty
    SELECT a~zbudat   a~matnr  a~erfmg
      INTO CORRESPONDING FIELDS OF TABLE it_gi_qty
      FROM mseg AS a
      FOR ALL ENTRIES IN it_material
     WHERE a~zbudat = l_yesterday
       AND a~matnr  = it_material-matnr
       AND a~bwart  = '551'
       AND a~werks  = w_werks
       AND NOT EXISTS ( SELECT * FROM mseg AS c
                                WHERE c~sjahr = a~mjahr
                                  AND c~smbln = a~mblnr
                                  AND c~smblp = a~zeile ).


  ENDIF.

*-Calculate Order Qty
  LOOP AT it_material.
    it_tab-matnr = it_material-matnr.
    it_tab-matnr_s = it_material-matnr.
*    it_tab-maktx   = it_material-maktx.
    it_tab-maktx   = it_material-matnr. "07.18.2014
    IF w_prdt = 'MP-PNL'.
      it_tab-desc = 'To be used'.
    ELSE.
      it_tab-desc = 'Order Qty'.
    ENDIF.
    APPEND it_tab.
  ENDLOOP.

  LOOP AT it_tab.
    LOOP AT it_day.
      READ TABLE it_cal_order WITH KEY matnr = it_tab-matnr
                                       edatu = it_day-datum.
      IF sy-subrc = 0.
        CONCATENATE 'IT_TAB-QTYD_' it_day-seq INTO l_text.
        ASSIGN (l_text) TO <fs01>.
        IF sy-subrc = 0.
          <fs01> = it_cal_order-cal_qty.
          it_tab-mtd = it_tab-mtd + it_cal_order-cal_qty. "MTD
        ENDIF.
      ENDIF.
    ENDLOOP.

*-apply holiday qty to the latest date
    LOOP AT it_cal_order WHERE matnr = it_tab-matnr
                           AND holiday_chk = 'X'.
      CONCATENATE 'IT_TAB-QTYD_' it_cal_order-seq INTO l_text.
      ASSIGN (l_text) TO <fs01>.
      IF sy-subrc = 0.
        <fs01> = <fs01> + it_cal_order-cal_qty.
        it_tab-mtd = it_tab-mtd + it_cal_order-cal_qty. "MTD
      ENDIF.
    ENDLOOP.

    LOOP AT it_week.
      CLEAR : l_week_qty.
      LOOP AT it_cal_order WHERE matnr = it_tab-matnr
                             AND edatu >= it_week-datum
                             AND edatu <= it_week-datum_to.
        l_week_qty  = l_week_qty + it_cal_order-cal_qty.

      ENDLOOP.

      IF sy-subrc = 0.
        CONCATENATE 'IT_TAB-QTYW_' it_week-seq INTO l_text.
        ASSIGN (l_text) TO <fs01>.
        IF sy-subrc = 0.
          <fs01> = l_week_qty.
          it_tab-mtd = it_tab-mtd + l_week_qty. "MTD
        ENDIF.
      ENDIF.
    ENDLOOP.

**-Yesterday GI
*    LOOP AT it_gi_qty WHERE matnr = it_tab-matnr
*                        AND zbudat = l_yesterday.
*      it_tab-d-1  = it_tab-d-1 + it_gi_qty-erfmg.
*    ENDLOOP.

*-Due in
    READ TABLE it_duein WITH KEY matnr = it_tab-matnr
                                 BINARY SEARCH.
    IF sy-subrc = 0.
      it_tab-due_in = it_duein-zwreqqty.
    ENDIF.

*-Past Due
    READ TABLE it_day INDEX 1.
    LOOP AT it_cal_order WHERE matnr = it_tab-matnr
                           AND edatu < it_day-datum .
      it_tab-past_due = it_tab-past_due +  it_cal_order-cal_qty.
    ENDLOOP.

    it_tab-mtd = it_tab-mtd + it_tab-past_due + it_tab-due_in.
    it_tab-seq = '1'.
    it_tab-if = 'C410'.  "Row Color
    MODIFY it_tab.
  ENDLOOP.
ENDFORM.                    " GET_QUANTITY
*&---------------------------------------------------------------------*
*&      Form  GET_STOCK_QUANTITY
*&---------------------------------------------------------------------*
FORM get_stock_quantity .

  DATA: it_asmp_pir LIKE TABLE OF ztpp_asmp_pir WITH HEADER LINE,
        lt_eng_stock LIKE TABLE OF ztpp_eng_stock WITH HEADER LINE.

  DATA : BEGIN OF it_mard_tmp OCCURS 0,
          matnr LIKE  mard-matnr,
          lgort LIKE  mard-lgort,
          labst LIKE  mard-labst,
         END OF it_mard_tmp.

  DATA : BEGIN OF it_mard OCCURS 0,
          matnr LIKE  mard-matnr,
          labst LIKE  mard-labst,
         END OF it_mard.

  DATA: l_matnr LIKE mara-matnr,
        l_cn(2) TYPE n,
        l_text(30).
  DATA: wa_tab1 LIKE it_tab,
        wa_tab3 LIKE it_tab.

  FIELD-SYMBOLS : <l_stock>, <l_plan>, <l_order>.

  CLEAR :  it_mard[],  it_mard.

  CHECK it_material[] IS NOT INITIAL.

  SELECT * INTO TABLE it_asmp_pir
   FROM ztpp_asmp_pir
   FOR ALL ENTRIES IN it_material
   WHERE matnr  = it_material-matnr
     AND wdatu = w_datum.

  SELECT matnr  lgort labst
        INTO CORRESPONDING FIELDS OF TABLE it_mard_tmp
  FROM mard
    FOR  ALL ENTRIES IN it_material
  WHERE matnr = it_material-idnrk.

  LOOP AT it_mard_tmp.
    MOVE-CORRESPONDING it_mard_tmp TO it_mard.
    COLLECT it_mard.
  ENDLOOP.

  SORT  it_mard  BY matnr.

  LOOP AT it_material.

    CLEAR: it_mard,
    wa_tab1, wa_tab3, it_tab, it_asmp_pir.
*    REFRESH lt_eng_stock.
*
    READ TABLE it_tab WITH KEY matnr = it_material-matnr.
    wa_tab1 = it_tab.
    CLEAR: it_tab.

    LOOP AT it_day.

**--Plan
      READ TABLE it_asmp_pir WITH KEY matnr = it_material-matnr
                                     pdatu = it_day-datum
                                     entlu = '1'.
      IF sy-subrc = 0.
        CONCATENATE 'IT_TAB-QTYD_' it_day-seq INTO l_text.
        ASSIGN (l_text) TO <fs01>.
        IF sy-subrc = 0.
          <fs01> = it_asmp_pir-plnmg.
          it_tab-mtd = it_tab-mtd + it_asmp_pir-plnmg. "MTD
        ENDIF.
      ENDIF.
    ENDLOOP.
    LOOP AT it_week.
      READ TABLE it_asmp_pir WITH KEY matnr = it_material-matnr
                                     pdatu = it_week-datum
                                     entlu = '2'.
      IF sy-subrc = 0.
        CONCATENATE 'IT_TAB-QTYW_' it_week-seq INTO l_text.
        ASSIGN (l_text) TO <fs01>.
        IF sy-subrc = 0.
          <fs01> = it_asmp_pir-plnmg.
          it_tab-mtd = it_tab-mtd + it_asmp_pir-plnmg. "MTD
        ENDIF.
      ENDIF.
    ENDLOOP.



    it_tab-matnr = it_material-matnr.
    IF it_material-bom_desc IS NOT INITIAL.
      it_tab-maktx = it_material-bom_desc.
    ENDIF.

*-Yesterday GI
    LOOP AT it_gi_qty WHERE matnr = it_tab-matnr
                        AND zbudat = l_yesterday.
      it_tab-d-1  = it_tab-d-1 + it_gi_qty-erfmg.
    ENDLOOP.

    it_tab-seq = '3'.

    it_tab-if = 'C210'.
*    ENDIF.
    it_tab-desc = 'Plan'.
    wa_tab3 = it_tab.
    APPEND it_tab.
    CLEAR: it_tab.

**--Stock
    CLEAR : lt_color[], lt_color, it_tab-cellcolor[].

    it_tab-seq = '2'.
    it_tab-matnr =  it_material-matnr.
    it_tab-maktx =  it_material-maktx.

*-First day stock
    READ TABLE it_mard WITH KEY matnr = it_material-idnrk
                                      BINARY SEARCH.
    IF sy-subrc = 0.
      it_tab-qtyd_01 = it_mard-labst.

      PERFORM fill_cell_color USING   'QTYD_01' it_tab-qtyd_01
                              CHANGING lt_color.
    ENDIF.

*-Second day stock
    it_tab-qtyd_02 =  it_tab-qtyd_01 + wa_tab3-qtyd_01
                      - wa_tab1-past_due - wa_tab1-qtyd_01.

    PERFORM fill_cell_color USING   'QTYD_02' it_tab-qtyd_02
                            CHANGING lt_color.

*-From Third
    LOOP AT it_day FROM 3 .
      l_cn = it_day-seq - 1.
      CONCATENATE 'WA_TAB1-QTYD_' l_cn INTO l_text.
      IF sy-subrc = 0.
        ASSIGN (l_text) TO <l_order>.
        CONCATENATE 'IT_TAB-QTYD_'  l_cn INTO l_text.
        ASSIGN (l_text) TO <l_stock>.
        CONCATENATE 'WA_TAB3-QTYD_' l_cn INTO l_text.
        ASSIGN (l_text) TO <l_plan>.

        CONCATENATE 'IT_TAB-QTYD_' it_day-seq INTO l_text.
        ASSIGN (l_text) TO <fs-qty>.
        IF sy-subrc = 0.
          <fs-qty> = <l_stock> + <l_plan> - <l_order>.

          PERFORM fill_cell_color USING   l_text+7(7) <fs-qty>
                                  CHANGING lt_color.
        ENDIF.
      ENDIF.
    ENDLOOP.

    CONCATENATE 'WA_TAB1-QTYD_' w_max_day_cn INTO l_text.
    ASSIGN (l_text) TO <l_order>.
    CONCATENATE 'IT_TAB-QTYD_'  w_max_day_cn INTO l_text.
    ASSIGN (l_text) TO <l_stock>.
    CONCATENATE 'WA_TAB3-QTYD_' w_max_day_cn INTO l_text.
    ASSIGN (l_text) TO <l_plan>.

    it_tab-qtyw_04 = <l_stock> + <l_plan> - <l_order>.

    PERFORM fill_cell_color USING   'QTYW_04' it_tab-qtyw_04
                            CHANGING lt_color.
    LOOP AT it_week FROM 2.
      l_cn = it_week-seq - 1.
      CONCATENATE 'WA_TAB1-QTYW_' l_cn INTO l_text.
      IF sy-subrc = 0.
        ASSIGN (l_text) TO <l_order>.
        CONCATENATE 'IT_TAB-QTYW_' l_cn INTO l_text.
        ASSIGN (l_text) TO <l_stock>.
        CONCATENATE 'WA_TAB3-QTYW_' l_cn INTO l_text.
        ASSIGN (l_text) TO <l_plan>.

        CONCATENATE 'IT_TAB-QTYW_' it_week-seq INTO l_text.
        ASSIGN (l_text) TO <fs-qty>.
        IF sy-subrc = 0.
          <fs-qty> = <l_stock> + <l_plan> - <l_order>.

          PERFORM fill_cell_color USING   l_text+7(7) <fs-qty>
                                  CHANGING lt_color.
        ENDIF.
      ENDIF.
    ENDLOOP.

    it_tab-mtd = it_tab-mtd + it_tab-past_due.
    it_tab-desc = 'Stock(MV)'.
    it_tab-cellcolor = lt_color.

    APPEND it_tab.
    CLEAR: it_tab.
  ENDLOOP.

*  IF W_PRDT = 'MP-PNL'.  "Delete Order Qty line
*    DELETE IT_TAB WHERE SEQ = '1'.
*  ENDIF.

  SORT it_tab BY matnr seq.
ENDFORM.                    " GET_STOCK_QUANTITY
*&---------------------------------------------------------------------*
*&      Form  FILL_CELL_COLOR
*&---------------------------------------------------------------------*
FORM fill_cell_color  USING    p_field "VALUE(P_5243)
                               p_qty
                      CHANGING pt_color TYPE slis_t_specialcol_alv.
  DATA: ls_color TYPE slis_specialcol_alv.

  CLEAR : ls_color.

  IF p_qty < 0.
    ls_color-color-col = 6.
    ls_color-fieldname = p_field.
    INSERT ls_color INTO TABLE pt_color.
  ENDIF.
ENDFORM.                    " FILL_CELL_COLOR
