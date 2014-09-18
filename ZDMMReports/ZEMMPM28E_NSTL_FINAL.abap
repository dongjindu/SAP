************************************************************************
* Program Name      : ZEMMPM28E_NSTL_FINAL
* Author            : Sung-Tae, Lim
* Creation Date     : 2004.05.19.
* Specifications By : Sung-Tae, Lim
* Development Request No : UD1K910554
* Addl Documentation:
* Description       : Daily Supply to Line (Non Supply to Line)
* Modification Logs
* Date            Developer        RequestNo      Description
* 2004.05.19.     Sung-Tae Lim     UD1K910554     Initial Coding
*
*
************************************************************************

REPORT zemmpm28e_nstl_final NO STANDARD PAGE HEADING
                            LINE-SIZE 255
                            LINE-COUNT 64(1)
                            MESSAGE-ID zmmm.

**---
INCLUDE : zrmmpmxxr_incl.


**--- Internal Tables
DATA : it_itab LIKE ztmm_nstl OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_temp OCCURS 0,
         datum LIKE ztmm_nstl-datum,
         rpsta LIKE ztmm_nstl-rpsta,
         kaptprog LIKE ztmm_nstl-kaptprog,
         matnr LIKE ztmm_nstl-matnr,
         bdmng LIKE ztmm_nstl-time01,
         meins LIKE ztmm_nstl-meins,
         ttime(03),
       END OF it_temp.

DATA : BEGIN OF it_order OCCURS 0,
         datum LIKE ztmm_dvrt-datum,
         matnr LIKE mara-matnr,
         uzeit LIKE ztmm_dvrt-uzeit,
         bdmng LIKE resb-bdmng,
         meins LIKE resb-meins,
         tprog LIKE ztmm_dvrt-tprog,
       END OF it_order.

DATA : BEGIN OF it_zspp_vin_info_for_stl OCCURS 0.
        INCLUDE STRUCTURE zspp_vin_info_for_nstl.
DATA : END OF it_zspp_vin_info_for_stl.

DATA : BEGIN OF it_ztmm_dvrt OCCURS 0.
        INCLUDE STRUCTURE ztmm_dvrt.
DATA : END OF it_ztmm_dvrt.

**--- Variables
DATA : w_atinn LIKE ausp-atinn,
       w_rp06  LIKE ztpp_dvrt1-rp06,
       w_subrc LIKE sy-subrc,
       w_datum LIKE sy-datum,
       w_uzeit LIKE sy-uzeit,
       w_curr_idx(2) TYPE n,
       w_kaptprog  LIKE ztmm_nstl-kaptprog, "First shift
       w_date_curr  LIKE sy-datum,          "Current shift date
       w_tprog_curr LIKE kapa-tprog,        "Current shift code
       w_date_next  LIKE sy-datum,          "Next shift date
       w_tprog_next LIKE kapa-tprog,        "Next shift code
       w_to_datum   LIKE sy-datum,          "T/O creation shift date
       w_to_tprog   LIKE kapa-tprog.        "T/O creation shift code


FIELD-SYMBOLS : <fsl>,
                <fsh>,
                <fsw>.

DATA : w_index(2) TYPE n VALUE '01',
       w_fnamel(9),
       w_fnameh(9),
       w_fnamew(14).

**--- Constants
CONSTANTS : c_werks LIKE t001w-werks VALUE 'P001',
            c_atwrt_06 LIKE ausp-atwrt VALUE '06',
            c_atwrt_01 LIKE ausp-atwrt VALUE '01',
            c_mtart LIKE mara-mtart VALUE 'ROH',
            c_time_plus TYPE t VALUE '000001',
            c_rp06(2) VALUE '06',
            c_rp01(2) VALUE '01',
            c_spptl LIKE ztmm_mast-spptl VALUE 'N',
            c_schgrup LIKE tc37a-schgrup VALUE 'HA'.

RANGES : r_matnr FOR mara-matnr.

*---

*--- insert by stlim (2004/07/28)
DATA : it_worktime LIKE zsmm_working_time_for_1t OCCURS 0
                                                 WITH HEADER LINE.
*--- end of insert

*--- blocked by stlim (2004/07/28)
*DATA : BEGIN OF it_worktime OCCURS 0.
*        INCLUDE STRUCTURE zsmm_nstl_worktime.
*DATA :   ttime(3),
*       END OF it_worktime.
*--- end of block

DATA : it_worktime_copy LIKE it_worktime OCCURS 0 WITH HEADER LINE.

DATA : it_dummy LIKE zsmm_working_time OCCURS 0 WITH HEADER LINE.

**---
CONSTANTS : c_nro_nr_00   VALUE '00' LIKE inri-nrrangenr, "Header Part
            c_nro_nr_01   VALUE '01' LIKE inri-nrrangenr, "Item Part
            c_nro_nr_09   VALUE '09' LIKE inri-nrrangenr. "App. Doc no.
* Number range object
DATA : w_nro_object  VALUE 'ZMMNRO0002' LIKE inri-object.

* Number_Get_Next
DATA : w_nro_number  TYPE num10.      " Same type of nro_object

DATA : w_zdocno TYPE num10.       "App. Doc. No.

DATA : it_bdcmsgcoll LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

DATA : wa_bdcmsgcoll LIKE LINE OF it_bdcmsgcoll.

**** Itab & WA for Non Supply to Line
DATA : BEGIN OF wa_matnr_date_time,
         matnr      LIKE ztmm_nstl-matnr,  "Material
         sdate      TYPE d,                "Start Date
         stime      TYPE t,                "Start Time
         edate      TYPE d,                "End Date
         etime      TYPE t,                "End Time
         qty        LIKE resb-bdmng,       "Quantity
         unit       LIKE resb-meins,       "Unit
         rdmng      LIKE mlgt-rdmng,       "Rounding Qunatity
         tqty       LIKE resb-bdmng,       "Target Quantity
         src_lgtyp  LIKE mlgt-lgtyp,       "Source Storage type
         src_lgpla  LIKE mlgt-lgpla,       "Source Storage bin
         des_lgtyp  LIKE pkhd-lgtyp,       "Destination Storage type
         des_lgpla  LIKE pkhd-lgpla,       "Destination Storage bin
         feedr      LIKE ztmm_mast-feedr,  "Feeder
         feed_cycle LIKE ztmm_mast-feed_cycle,"Feed Cycle
         ztime      LIKE ztmm_mast-ztime,  "Time from PBS out to W/S
         zline      LIKE ztmm_mast-zline,
         works      LIKE ztmm_mast-works,
         rh_lh      LIKE ztmm_mast-rh_lh,
         dispo      LIKE ztmm_mast-dispo,
         gesme      LIKE lqua-gesme,
         lpmin      LIKE ztmm_mast-lpmin,
         vsola      LIKE ztmm_nstl_log-vsola,
         stock_check LIKE ztmm_mast-stock_check,
         menge LIKE resb-bdmng,       " Requirement Quantity
       END OF wa_matnr_date_time.

DATA : it_matnr_date_time LIKE TABLE OF wa_matnr_date_time.

FIELD-SYMBOLS : <fs_matnr_date_time> LIKE LINE OF it_matnr_date_time.

DATA : it_data_for_to LIKE it_matnr_date_time WITH HEADER LINE.

DATA : wa_data_for_to LIKE LINE OF it_data_for_to.

FIELD-SYMBOLS : <fs_data_for_to> LIKE LINE OF it_data_for_to.

DATA : BEGIN OF wa_ztmm_nstl.
        INCLUDE STRUCTURE ztmm_nstl.
DATA :   feedr      LIKE ztmm_mast-feedr,      "Feeder
         feed_cycle LIKE ztmm_mast-feed_cycle, "Feed Cycle
         ztime      LIKE ztmm_mast-ztime,      "Time from PBS out to W/S
         zline      LIKE ztmm_mast-zline,
         works      LIKE ztmm_mast-works,
         rh_lh      LIKE ztmm_mast-rh_lh,
         dispo      LIKE ztmm_mast-dispo,
         lpmin      LIKE ztmm_mast-lpmin,
         stock_check LIKE ztmm_mast-stock_check,
         menge LIKE resb-bdmng,       " Requirement Quantity
       END OF wa_ztmm_nstl.

DATA : it_ztmm_nstl LIKE TABLE OF wa_ztmm_nstl.

FIELD-SYMBOLS : <fs_ztmm_nstl> LIKE LINE OF it_ztmm_nstl.

DATA : BEGIN OF it_write OCCURS 0.
        INCLUDE STRUCTURE wa_matnr_date_time.
DATA :   tanum LIKE ltap-tanum,     " TO number
         w_docno TYPE num10,
         linecolor(4),     " ALV Color
         messa(80),
         msgty LIKE ztmm_nstl_log-msgty,
         msgid LIKE ztmm_nstl_log-msgid,
         msgnr LIKE ztmm_nstl_log-msgnr,
         stats LIKE ztmm_nstl_log-stats,
       END OF it_write.

DATA : w_tot_lines TYPE i,     " full time
       w_lines TYPE i,     " time that delete overtime
       w_check_stock(1).

*--- current stock
DATA : BEGIN OF it_stock_temp OCCURS 0,
         matnr LIKE lqua-matnr,
         gesme LIKE lqua-gesme,
         lgtyp LIKE pkhd-lgtyp,
         lgpla LIKE pkhd-lgpla,
       END OF it_stock_temp.

DATA : it_stock LIKE it_stock_temp OCCURS 0 WITH HEADER LINE.

DATA : w_1stime TYPE t,
       w_1etime TYPE t,
       w_2stime TYPE t,
       w_2etime TYPE t.

*--- open TO quantity
DATA : BEGIN OF it_open_temp OCCURS 0,
         matnr LIKE ltap-matnr,
         vsola LIKE ltap-vsola,
       END OF it_open_temp.

DATA : it_open LIKE it_open_temp OCCURS 0 WITH HEADER LINE.

CONSTANTS : c_day TYPE i VALUE 10.


**--- Macro
DEFINE append_fieldcat.
  &1 = &1 + 1.
  w_fieldcat-col_pos    = &1.
  w_fieldcat-fieldname  = &2.
  w_fieldcat-outputlen  = &3.
  w_fieldcat-seltext_l  = &4.
  w_fieldcat-seltext_m  = &4.
  w_fieldcat-seltext_s  = &4.
  w_fieldcat-datatype   = &5.
  w_fieldcat-key        = &6.
  w_fieldcat-qfieldname = &7.
  w_fieldcat-cfieldname = &8.
  append w_fieldcat.
  clear : w_fieldcat.
END-OF-DEFINITION.

DEFINE append_sortcat.
  w_sortcat-spos      = &1.
  w_sortcat-fieldname = &2.
  w_sortcat-tabname   = &3.
  w_sortcat-up        = &4.
  w_sortcat-subtot    = &5.
  append w_sortcat.
  clear : w_sortcat.
END-OF-DEFINITION.


**---
SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME TITLE text-002.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT : 1(60) text-003.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT : 1(60) text-004.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK block2.

SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.

PARAMETERS :
*p_kaptpr LIKE tc37a-kaptprog OBLIGATORY,
             p_rp LIKE ausp-atwrt OBLIGATORY,
             p_arbpl LIKE crhd-arbpl.
*             p_datum TYPE d OBLIGATORY DEFAULT sy-datum.

SELECTION-SCREEN ULINE.

PARAMETERS : p_shift  AS CHECKBOX,
             p_update AS CHECKBOX DEFAULT 'X',
             p_tocrea AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN ULINE.

SELECT-OPTIONS : s_matnr FOR mara-matnr NO-EXTENSION.

SELECTION-SCREEN END OF BLOCK block1.


**---
INITIALIZATION.
  PERFORM event_build USING w_eventcat[].

**---
TOP-OF-PAGE.
  PERFORM top_of_page.


**---
AT SELECTION-SCREEN.
  PERFORM check_input_values.


**---
START-OF-SELECTION.
  PERFORM call_working_time_function.

  IF p_update NE space.
    PERFORM get_data.
  ENDIF.

  IF p_update NE space.
    PERFORM update_table.
  ENDIF.

*---
  PERFORM get_working_time.
  PERFORM check_input.

  IF p_tocrea NE space.
    PERFORM to_create_routine.
  ENDIF.


**---
END-OF-SELECTION.
  PERFORM log_table_update.
  PERFORM comment_build.     " USING w_top_of_page[].
  PERFORM make_alv_grid.




**---

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
*---
  CLEAR : it_itab, it_itab[], it_order, it_order[].

*--- Read Vehicle Master
  PERFORM read_vehicle_master USING w_subrc.

  CHECK w_subrc EQ 0.

  PERFORM make_mm_dvrt.

  PERFORM read_raw_data.
ENDFORM.                    " get_data

*&---------------------------------------------------------------------*
*&      Form  update_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_table.
  DATA: lw_datetime LIKE it_worktime-begzt.

  DATA: BEGIN OF lt_shift OCCURS 0,
          datum    LIKE sy-datum,
          rpsta    LIKE ztmm_nstl-rpsta,
          kaptprog LIKE ztmm_nstl-kaptprog,
        END   OF lt_shift.


  DATA: lt_ztmm_nstl LIKE ztmm_nstl OCCURS 0 WITH HEADER LINE.


  CONCATENATE: w_datum w_uzeit INTO lw_datetime.

*----- If currend shift is checked, keep current shift requirement.
  IF p_shift EQ 'X'.
    LOOP AT it_worktime WHERE datum =  w_datum
                          AND begzt <= lw_datetime
                          AND endzt >= lw_datetime.
    ENDLOOP.
    IF sy-subrc EQ 0.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_ztmm_nstl
        FROM ztmm_nstl
       WHERE datum    = it_worktime-datum
         AND rpsta    = p_rp
         AND kaptprog = it_worktime-tprog
         AND matnr    IN s_matnr.

      MOVE: it_worktime-index TO w_curr_idx.
    ENDIF.
  ELSE.
    LOOP AT it_worktime WHERE datum =  w_datum
                          AND begzt <= lw_datetime
                          AND endzt >= lw_datetime.
    ENDLOOP.
    IF sy-subrc EQ 0.
      DELETE it_itab WHERE datum    = it_worktime-datum
                       AND rpsta    = p_rp
                       AND kaptprog = it_worktime-tprog
                       AND matnr   IN s_matnr.
    ENDIF.
  ENDIF.

*----- Delete Requirement
  LOOP AT it_worktime.
    MOVE: it_worktime-datum TO lt_shift-datum,
          p_rp              TO lt_shift-rpsta,
          it_worktime-tprog TO lt_shift-kaptprog.

    COLLECT lt_shift.
  ENDLOOP.

  LOOP AT lt_shift.
    DELETE FROM ztmm_nstl WHERE datum    = lt_shift-datum
                            AND rpsta    = lt_shift-rpsta
                            AND kaptprog = lt_shift-kaptprog
                            AND matnr   IN s_matnr.
  ENDLOOP.

  READ TABLE lt_shift INDEX 1.
  IF sy-subrc EQ 0.
    MOVE: lt_shift-kaptprog TO w_kaptprog.
  ENDIF.

*----- Set Current Requirement
  FIELD-SYMBOLS: <1t_qty_old>,
                 <1t_qty_new>.

  DATA: lw_1t_qty_old(50),
        lw_1t_qty_new(50),
        lw_1t_idx(2) TYPE n.

  LOOP AT lt_ztmm_nstl.
    CLEAR: it_itab.
    READ TABLE it_itab WITH KEY datum    = lt_ztmm_nstl-datum
*                                rpsta    = lt_ztmm_nstl-rpsta
                                kaptprog = lt_ztmm_nstl-kaptprog
                                matnr    = lt_ztmm_nstl-matnr.
    IF sy-subrc NE 0.
      MOVE: lt_ztmm_nstl-datum    TO it_itab-datum,
            lt_ztmm_nstl-rpsta    TO it_itab-rpsta,
            lt_ztmm_nstl-kaptprog TO it_itab-kaptprog,
            lt_ztmm_nstl-matnr    TO it_itab-matnr.

      APPEND it_itab.
    ENDIF.

    DO w_curr_idx TIMES.
      MOVE sy-index TO lw_1t_idx.

      CONCATENATE: 'IT_ITAB-TIME'      lw_1t_idx INTO lw_1t_qty_new,
                   'LT_ZTMM_NSTL-TIME' lw_1t_idx INTO lw_1t_qty_old.

      ASSIGN: (lw_1t_qty_old) TO <1t_qty_old>,
              (lw_1t_qty_new) TO <1t_qty_new>.

      MOVE: <1t_qty_old>       TO <1t_qty_new>,
            lt_ztmm_nstl-rpsta TO it_itab-rpsta.

      MODIFY it_itab INDEX sy-tabix.
    ENDDO.
  ENDLOOP.

*---
  DATA : l_lines TYPE i.

  LOOP AT it_itab.
    MOVE-CORRESPONDING it_itab TO ztmm_nstl.
    MOVE : p_rp                TO ztmm_nstl-rpsta.
    ztmm_nstl-erdat = ztmm_nstl-aedat = sy-datum.
    ztmm_nstl-erzet = ztmm_nstl-aezet = sy-uzeit.
    ztmm_nstl-ernam = ztmm_nstl-aenam = sy-uname.
    MODIFY ztmm_nstl.
    CLEAR : ztmm_nstl.
  ENDLOOP.

  COMMIT WORK.

*---
  DESCRIBE TABLE it_itab LINES l_lines.

  WRITE : / '* Total', l_lines, 'Updated'.
ENDFORM.                    " update_table

*&---------------------------------------------------------------------*
*&      Form  to_create_routine
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM to_create_routine.
*---

  PERFORM get_it_ztmm_nstl.
  PERFORM get_it_matnr_date_time.
  PERFORM get_it_data_for_to.

  PERFORM process_it_data_for_to.
ENDFORM.                    " to_create_routine

*&---------------------------------------------------------------------*
*&      Form  read_vehicle_master
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_SUBRC  text
*----------------------------------------------------------------------*
FORM read_vehicle_master USING    p_subrc.
*---
  CLEAR : it_zspp_vin_info_for_stl, it_zspp_vin_info_for_stl[].

  CALL FUNCTION 'Z_FPP_GET_NON_SUPPLY_TO_LINE'
       EXPORTING
            i_werks                  = c_werks
            i_atwrt                  = p_rp
            i_date                   = w_datum
            i_time                   = w_uzeit
            i_arbpl                  = p_arbpl
       TABLES
            t_supply_info            = it_zspp_vin_info_for_stl
       EXCEPTIONS
            no_data_founded          = 1
            line_info_does_not_exist = 2
            etc_exception            = 3
            uph_info_does_not_exist  = 4
            OTHERS                   = 5.

  MOVE : sy-subrc TO w_subrc.

  IF sy-subrc NE 0.
    CASE sy-subrc.
      WHEN 1.
        MESSAGE e999 WITH text-m04.
      WHEN 2.
        MESSAGE e999 WITH text-m05.
      WHEN 3.
        MESSAGE e999 WITH text-m06.
      WHEN 4.
        MESSAGE e999 WITH text-m07.
      WHEN 5.
        MESSAGE e999 WITH text-m08.
    ENDCASE.
  ENDIF.

  DELETE it_zspp_vin_info_for_stl WHERE rsnum EQ space.
ENDFORM.                    " read_vehicle_master

*&---------------------------------------------------------------------*
*&      Form  make_mm_dvrt
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_mm_dvrt.
*--- delete DVRT Table
  DELETE FROM ztmm_dvrt WHERE plnum NE space.

  COMMIT WORK.

*--- insert DVRT Table
  LOOP AT it_zspp_vin_info_for_stl.
    MOVE-CORRESPONDING it_zspp_vin_info_for_stl TO it_ztmm_dvrt.
    MOVE : it_zspp_vin_info_for_stl-p_rp06(8)   TO it_ztmm_dvrt-datum,
           it_zspp_vin_info_for_stl-p_rp06+8(6) TO it_ztmm_dvrt-uzeit.

    it_ztmm_dvrt-uzeit = it_ztmm_dvrt-uzeit + c_time_plus.

    IF it_ztmm_dvrt-uzeit BETWEEN '000001' AND '040000'.
      it_ztmm_dvrt-datum = it_ztmm_dvrt-datum - 1.
    ENDIF.

    it_ztmm_dvrt-erdat = it_ztmm_dvrt-aedat = sy-datum.
    it_ztmm_dvrt-erzet = it_ztmm_dvrt-aezet = sy-uzeit.
    it_ztmm_dvrt-ernam = it_ztmm_dvrt-aenam = sy-uname.
    APPEND it_ztmm_dvrt.
    CLEAR : it_ztmm_dvrt, it_zspp_vin_info_for_stl.
  ENDLOOP.

  MODIFY ztmm_dvrt FROM TABLE it_ztmm_dvrt.

  COMMIT WORK.
ENDFORM.                    " make_mm_dvrt

*&---------------------------------------------------------------------*
*&      Form  read_raw_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_raw_data.
*---
  IF p_rp EQ '01'.
    PERFORM read_rp01.
  ELSEIF p_rp EQ '06'.
    PERFORM read_rp06.
  ENDIF.

  SORT it_order BY datum matnr uzeit.

*---
  FIELD-SYMBOLS : <fs>.

  DATA : l_bdmng(14),
         l_index(2) TYPE n,
         l_daytime LIKE it_worktime-begzt.

  CLEAR : it_temp, it_temp[], it_itab, it_itab[].

  LOOP AT it_order.
    CLEAR : it_worktime, l_bdmng, l_index, l_daytime.
    MOVE : it_order-datum TO it_itab-datum,
           it_order-tprog TO it_itab-kaptprog,
           it_order-matnr TO it_itab-matnr,
           it_order-meins TO it_itab-meins.
    CONCATENATE it_order-datum it_order-uzeit INTO l_daytime.
    LOOP AT it_worktime WHERE begzt <= l_daytime
                          AND endzt >= l_daytime.
*    LOOP AT it_worktime WHERE begzt+8(6) <= it_order-uzeit
*                          AND endzt+8(6) >= it_order-uzeit.
      MOVE : it_worktime-tprog TO it_itab-kaptprog.
      MOVE : it_worktime-index TO l_index.
      CONCATENATE 'IT_ITAB-TIME' l_index INTO l_bdmng.
*      CONCATENATE 'IT_ITAB-TIME' it_worktime-ttime+1(2) INTO l_bdmng.
    ENDLOOP.
*----- changed by bsbae. changed on 20040706
*    IF sy-subrc NE 0.
*      it_order-uzeit = it_order-uzeit - 1.
*      LOOP AT it_worktime WHERE begzt+8(6) <= it_order-uzeit
*                            AND endzt+8(6) >= it_order-uzeit.
*        MOVE : it_worktime-tprog TO it_itab-kaptprog.
*        CONCATENATE 'IT_ITAB-TIME' it_worktime-ttime+1(2) INTO l_bdmng
.
*      ENDLOOP.
*      IF sy-subrc NE 0.
*        MESSAGE e000(zz) WITH text-m09.
*      ENDIF.
*    ENDIF.
    CHECK sy-subrc EQ 0.
    ASSIGN : (l_bdmng) TO <fs>.
    MOVE : it_order-bdmng TO <fs>.
    COLLECT it_itab.
    CLEAR : it_itab.
*----- changed by bsbae. changed on 20040706
  ENDLOOP.
ENDFORM.                    " read_raw_data

*&---------------------------------------------------------------------*
*&      Form  APPEND_PLANNED_ORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_planned_order.
*---
  APPEND it_order.
  CLEAR : it_order.
ENDFORM.                    " APPEND_PLANNED_ORDER

*&---------------------------------------------------------------------*
*&      Form  check_input
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_input.
*---
*  CLEAR : it_worktime.
*
*  READ TABLE it_worktime WITH KEY tprog = p_kaptpr.
*
*  IF sy-subrc NE space.
*    SET CURSOR FIELD 'P_KAPTPR'.
*    MESSAGE e999 WITH text-m03.
*  ENDIF.
ENDFORM.                    " check_input

*&---------------------------------------------------------------------*
*&      Form  read_rp01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_rp01.
*--- read raw data
  EXEC SQL PERFORMING APPEND_PLANNED_ORDER.
    SELECT /*+ ORDERED*/
           Z.DATUM,  X.MATNR,  Z.UZEIT,  Y.BDMNG,  Y.MEINS,
           Z.TPROG
           INTO :IT_ORDER
           FROM ZTMM_DVRT Z, RESB Y, MARA X, ZTMM_MAST W
*- Z & Y
          WHERE Z.MANDT = :SY-MANDT
            AND Y.MANDT = Z.MANDT
            AND Y.RSNUM = Z.RSNUM
*- Y & X
            AND X.MANDT = Y.MANDT
            AND X.MATNR = Y.MATNR
            AND X.MTART = :C_MTART
*- X & W
            AND W.MANDT = X.MANDT
            AND W.MATNR = X.MATNR
            AND W.WERKS = Y.WERKS
            AND W.SPPTL = :C_SPPTL
            AND SUBSTR(W.ZLINE,1,1) = 'B'
  ENDEXEC.
ENDFORM.                                                    " read_rp01

*&---------------------------------------------------------------------*
*&      Form  read_rp06
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_rp06.
*--- read raw data
  EXEC SQL PERFORMING APPEND_PLANNED_ORDER.
    SELECT /*+ ORDERED*/
           Z.DATUM,  X.MATNR,  Z.UZEIT,  Y.BDMNG,  Y.MEINS,
           Z.TPROG
           INTO :IT_ORDER
           FROM ZTMM_DVRT Z, RESB Y, MARA X, ZTMM_MAST W
*- Z & Y
          WHERE Z.MANDT = :SY-MANDT
            AND Y.MANDT = Z.MANDT
            AND Y.RSNUM = Z.RSNUM
*- Y & X
            AND X.MANDT = Y.MANDT
            AND X.MATNR = Y.MATNR
            AND X.MTART = :C_MTART
*- X & W
            AND W.MANDT = X.MANDT
            AND W.MATNR = X.MATNR
            AND W.WERKS = Y.WERKS
            AND W.SPPTL = :C_SPPTL
            AND SUBSTR(W.ZLINE,1,1) != 'B'
  ENDEXEC.
ENDFORM.                                                    " read_rp06

*&---------------------------------------------------------------------*
*&      Form  log_table_update
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM log_table_update.
*---
  DATA : it_ztmm_nstl_log LIKE ztmm_nstl_log OCCURS 0 WITH HEADER LINE.

  CLEAR : it_ztmm_nstl_log, it_ztmm_nstl_log[].

  LOOP AT it_write.
    CLEAR : ztmm_nstl_log.
    MOVE-CORRESPONDING it_write TO it_ztmm_nstl_log.
    MOVE : it_write-w_docno     TO it_ztmm_nstl_log-logno_h,
           it_write-menge       TO it_ztmm_nstl_log-bdmng,
*           it_write-qty         TO it_ztmm_nstl_log-bdmng,
           it_write-unit        TO it_ztmm_nstl_log-meins,
           sy-tcode             TO it_ztmm_nstl_log-ztcode,
           sy-repid             TO it_ztmm_nstl_log-zprogramm.
    it_ztmm_nstl_log-ernam = it_ztmm_nstl_log-aenam = sy-uname.
    it_ztmm_nstl_log-erdat = it_ztmm_nstl_log-aedat = sy-datum.
    it_ztmm_nstl_log-erzet = it_ztmm_nstl_log-aezet = sy-uzeit.
    APPEND it_ztmm_nstl_log.
  ENDLOOP.

  MODIFY ztmm_nstl_log FROM TABLE it_ztmm_nstl_log.
ENDFORM.                    " log_table_update

*&---------------------------------------------------------------------*
*&      Form  comment_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM comment_build.
*---
  CLEAR : w_line.
  w_line-typ  = 'H'.
  w_line-info = text-006.
  APPEND w_line TO w_top_of_page.

  CLEAR : w_line.
  APPEND INITIAL LINE TO w_top_of_page.
ENDFORM.                    " comment_build

*&---------------------------------------------------------------------*
*&      Form  make_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_alv_grid.
*---
  MOVE : 'LINECOLOR' TO w_layout-info_fieldname.
  w_layout-colwidth_optimize = 'X'.

  PERFORM build_fieldcat.
  PERFORM build_sortcat.

  CLEAR : w_program.

  MOVE : sy-repid TO w_program.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
       EXPORTING
            i_callback_program = w_program
            is_layout          = w_layout
            it_fieldcat        = w_fieldcat[]
            it_events          = w_eventcat[]
            it_sort            = w_sortcat[]
            i_save             = 'A'
       TABLES
            t_outtab           = it_write
       EXCEPTIONS
            program_error      = 1
            OTHERS             = 2.
ENDFORM.                    " make_alv_grid

*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcat.
**--- &1 : position       &2 : field name       &3 : field length
**--- &4 : description    &5 : field type       &6 : key
**--- &7 : qty field      &8 : color
  append_fieldcat :
    w_col_pos 'MATNR'     18 'Material'       'CHAR' 'X' ''      '',
    w_col_pos 'SDATE'     10 'TO S/Date'      'DATS' ''  ''      '',
    w_col_pos 'STIME'     08 'TO S/Time'      'TIMS' ''  ''      '',
    w_col_pos 'EDATE'     10 'TO E/Date'      'DATS' ''  ''      '',
    w_col_pos 'ETIME'     08 'TO E/Time'      'TIMS' ''  ''      '',
    w_col_pos 'MENGE'     12 'Quantity'       'QUAN' ''  'MEINS' '',
*    w_col_pos 'QTY'       12 'Quantity'       'QUAN' ''  'MEINS' '',
    w_col_pos 'GESME'     12 'Current Stock'  'QUAN' ''  'MEINS' '',
    w_col_pos 'LPMIN'     12 'Safety Stock'   'QUAN' ''  'MEINS' '',
    w_col_pos 'VSOLA'     12 'Open TO Qty'    'QUAN' ''  'MEINS' '',
    w_col_pos 'UNIT'      03 'UoM'            'UNIT' ''  ''      '',
    w_col_pos 'WORKS'     05 'Workstation'    'CHAR' ''  ''      '',
    w_col_pos 'RH_LH'     02 'RH/LH'          'CHAR' ''  ''      '',
    w_col_pos 'ZLINE'     02 'Line'           'CHAR' ''  ''      '',
    w_col_pos 'DISPO'     03 'MRP Controller' 'CHAR' ''  ''      '',
    w_col_pos 'FEED_CYCLE'   04 'Feed Cycle'  'NUMC' ''  ''      '',
    w_col_pos 'ZTIME'     03 'Time for STL'   'NUMC' ''  ''      '',
    w_col_pos 'RDMNG'     12 'Rounding Qty'   'QUAN' ''  'MEINS' '',
    w_col_pos 'TQTY'      12 'TO Qty'         'QUAN' ''  'MEINS' '',
    w_col_pos 'FEEDR'     05 'Feeder'         'CHAR' ''  ''      '',
    w_col_pos 'TANUM'     10 'TO Number'      'CHAR' ''  ''      '',
    w_col_pos 'SRC_LGTYP' 03 'Src S/Type'     'CHAR' ''  ''      '',
    w_col_pos 'SRC_LGPLA' 10 'Src S/Bin'      'CHAR' ''  ''      '',
    w_col_pos 'DES_LGTYP' 03 'Des S/Type'     'CHAR' ''  ''      '',
    w_col_pos 'DES_LGPLA' 10 'Des S/Bin'      'CHAR' ''  ''      '',
    w_col_pos 'MESSA'     80 'Message'        'CHAR' ''  ''      '',
    w_col_pos 'MSGID'     06 'Message ID'     'CHAR' ''  ''      '',
    w_col_pos 'MSGNR'     06 'Message Number' 'CHAR' ''  ''      '',
    w_col_pos 'STOCK_CHECK'     06 'Stock Check' 'CHAR' ''  ''      ''.
ENDFORM.                    " build_fieldcat

*&---------------------------------------------------------------------*
*&      Form  build_sortcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_sortcat.
**--- &1 : position     &2 : field name     &3 : tab name
**--- &4 : up           &5 : sub total
  append_sortcat : '1' 'MATNR' 'IT_WRITE' 'X' ''.
ENDFORM.                    " build_sortcat

*&---------------------------------------------------------------------*
*&      Form  get_it_ztmm_nstl
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_it_ztmm_nstl.
*---
  IF p_rp EQ '01'.
    PERFORM get_rp01.
  ELSEIF p_rp EQ '06'.
    PERFORM get_rp06.
  ENDIF.
ENDFORM.                    " get_it_ztmm_nstl

*&---------------------------------------------------------------------*
*&      Form  get_rp01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_rp01.
*---
  SELECT a~matnr
         a~time01 a~time02
         a~time03 a~time04
         a~time05 a~time06
         a~time07 a~time08
         a~time09 a~time10
         a~time11 a~time12
         a~time13 a~time14
         a~time15 a~time16
         a~time17 a~time18
         a~time19 a~time20
         a~meins
         b~feedr
         b~feed_cycle
         b~ztime
         b~zline
         b~works
         b~rh_lh
         b~dispo
         b~lpmin
         b~stock_check
                   INTO CORRESPONDING FIELDS OF TABLE it_ztmm_nstl
                   FROM ztmm_nstl AS a INNER JOIN ztmm_mast AS b
                     ON a~matnr EQ b~matnr
                    AND b~spptl EQ 'N'      "Non Supply To Line
                  WHERE rpsta EQ p_rp
                    AND datum EQ w_to_datum
                    AND kaptprog EQ w_to_tprog
                    AND zline LIKE 'B%'
                    AND a~matnr IN s_matnr.
ENDFORM.                                                    " get_rp01

*&---------------------------------------------------------------------*
*&      Form  get_rp06
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_rp06.
*---
  SELECT a~matnr
         a~time01 a~time02
         a~time03 a~time04
         a~time05 a~time06
         a~time07 a~time08
         a~time09 a~time10
         a~time11 a~time12
         a~time13 a~time14
         a~time15 a~time16
         a~time17 a~time18
         a~time19 a~time20
         a~meins
         b~feedr
         b~feed_cycle
         b~ztime
         b~zline
         b~works
         b~rh_lh
         b~dispo
         b~lpmin
         b~stock_check
                   INTO CORRESPONDING FIELDS OF TABLE it_ztmm_nstl
                   FROM ztmm_nstl AS a INNER JOIN ztmm_mast AS b
                     ON a~matnr EQ b~matnr
                    AND b~spptl EQ 'N'      "Non Supply To Line
                  WHERE rpsta EQ p_rp
                    AND datum EQ w_to_datum
                    AND kaptprog EQ w_to_tprog
                    AND zline NOT LIKE 'B%'
                    AND a~matnr IN s_matnr.
ENDFORM.                                                    " get_rp06

*&---------------------------------------------------------------------*
*&      Form  get_it_matnr_date_time
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_it_matnr_date_time.
*---
  PERFORM get_current_stock.
  PERFORM get_open_to_quantity.

*---
  DATA : lv_sdate TYPE d,
         lv_stime TYPE t,
         lv_edate TYPE d,
         lv_etime TYPE t.

  DATA : lv_time_qty(19).

  FIELD-SYMBOLS : <fs_qty>.

  DATA: lv_time_idx(2) TYPE n.

*---
*  DELETE it_worktime WHERE TPROG NE p_kaptpr.

  CLEAR : w_tot_lines.

  DESCRIBE TABLE it_worktime LINES w_tot_lines.

*---
  LOOP AT it_ztmm_nstl INTO wa_ztmm_nstl.
    CLEAR : lv_time_idx.
    DO w_tot_lines TIMES.
      CLEAR : it_worktime.
      MOVE : sy-index TO lv_time_idx.

      READ TABLE it_worktime INDEX sy-index.
      MOVE : it_worktime-begzt(8)    TO lv_sdate,
             it_worktime-begzt+8(6)  TO lv_stime.

      IF lv_stime EQ '000000'.
        lv_sdate = lv_sdate + 1.
      ENDIF.

      MOVE : it_worktime-endzt(8)    TO lv_edate,
             it_worktime-endzt+8(6)  TO lv_etime.

*      IF lv_etime EQ '000000'.
*        lv_edate = lv_edate + 1.
*      ENDIF.

      CONCATENATE 'WA_ZTMM_NSTL-TIME' lv_time_idx INTO lv_time_qty.

      MOVE : wa_ztmm_nstl-matnr      TO wa_matnr_date_time-matnr,
             wa_ztmm_nstl-feedr      TO wa_matnr_date_time-feedr,"Feeder
             wa_ztmm_nstl-feed_cycle TO wa_matnr_date_time-feed_cycle,
             lv_sdate                TO wa_matnr_date_time-sdate,
             lv_stime                TO wa_matnr_date_time-stime,
             lv_edate                TO wa_matnr_date_time-edate,
             lv_etime                TO wa_matnr_date_time-etime.
      ASSIGN : (lv_time_qty)         TO <fs_qty>.
      MOVE : <fs_qty>                TO wa_matnr_date_time-qty,
             wa_ztmm_nstl-meins      TO wa_matnr_date_time-unit,
             wa_ztmm_nstl-feedr      TO wa_matnr_date_time-feedr,"Feeder
             wa_ztmm_nstl-feed_cycle TO wa_matnr_date_time-feed_cycle,
             wa_ztmm_nstl-ztime      TO wa_matnr_date_time-ztime,
             wa_ztmm_nstl-zline      TO wa_matnr_date_time-zline,
             wa_ztmm_nstl-works      TO wa_matnr_date_time-works,
             wa_ztmm_nstl-rh_lh      TO wa_matnr_date_time-rh_lh,
             wa_ztmm_nstl-dispo      TO wa_matnr_date_time-dispo,
             wa_ztmm_nstl-lpmin      TO wa_matnr_date_time-lpmin,
             wa_ztmm_nstl-stock_check
                                TO wa_matnr_date_time-stock_check.

      APPEND wa_matnr_date_time TO it_matnr_date_time.
    ENDDO.
  ENDLOOP.
ENDFORM.                    " get_it_matnr_date_time

*&---------------------------------------------------------------------*
*&      Form  get_current_stock
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_current_stock.
*---
  CLEAR : it_stock_temp, it_stock_temp[], it_stock, it_stock[].

  SELECT a~pknum
         b~lgnum
         b~lqnum
         a~matnr
         gesme     " quantity
         a~lgtyp
         a~lgpla
               INTO CORRESPONDING FIELDS OF TABLE it_stock_temp
               FROM pkhd AS a INNER JOIN lqua AS b
                 ON a~mandt EQ b~mandt
                AND a~matnr EQ b~matnr
                AND a~lgtyp EQ b~lgtyp
                AND a~lgpla EQ b~lgpla
                FOR ALL ENTRIES IN it_ztmm_nstl
              WHERE a~matnr EQ it_ztmm_nstl-matnr.

  LOOP AT it_stock_temp.
    MOVE : it_stock_temp-matnr TO it_stock-matnr,
           it_stock_temp-gesme TO it_stock-gesme.
    COLLECT it_stock.
    CLEAR : it_stock_temp, it_stock.
  ENDLOOP.
ENDFORM.                    " get_current_stock

*&---------------------------------------------------------------------*
*&      Form  get_open_to_quantity
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_open_to_quantity.
*---
  CLEAR : it_open_temp, it_open_temp[], it_open, it_open[].

  SELECT lgnum
         tanum
         tapos
         matnr
         vsola     "Source target quantity in alternate unit
               INTO CORRESPONDING FIELDS OF TABLE it_open_temp
               FROM ltap
                FOR ALL ENTRIES IN it_ztmm_nstl
              WHERE matnr EQ it_ztmm_nstl-matnr
                AND pquit EQ space "Open TO(Indicator: confirmation comp
                AND lgnum EQ 'P01'.

  LOOP AT it_open_temp.
    MOVE-CORRESPONDING it_open_temp TO it_open.
    COLLECT it_open.
    CLEAR : it_open_temp, it_open.
  ENDLOOP.
ENDFORM.                    " get_open_to_quantity

*&---------------------------------------------------------------------*
*&      Form  get_working_time
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_working_time.
  IF p_shift EQ 'X'.
    MOVE: w_date_curr  TO w_to_datum,
          w_tprog_curr TO w_to_tprog.
  ELSE.
    MOVE: w_date_next  TO w_to_datum,
          w_tprog_next TO w_to_tprog.
  ENDIF.


*--- check running date
  DELETE it_worktime WHERE datum NE w_to_datum.

*--- check shift
  DELETE it_worktime WHERE tprog NE w_to_tprog.

*--- sort worktime
  SORT it_worktime BY index.
*  SORT it_worktime BY begzt.

*--- blocked by stlim (2004/07/28)
*  DATA : l_tabix(02) TYPE n.
*
*  LOOP AT it_worktime.
*    MOVE : sy-tabix TO l_tabix.
*    CONCATENATE 'T' l_tabix INTO it_worktime-ttime.
*    MODIFY it_worktime.
*  ENDLOOP.
*--- end of block
ENDFORM.                    " get_working_time

*&---------------------------------------------------------------------*
*&      Form  get_it_data_for_to
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_it_data_for_to.
*---
  DATA : lv_sdate        TYPE d,
         lv_stime        TYPE t,
         lv_timeforcal   TYPE t,
         lv_hour_idx(2)  TYPE n,
         lv_hour_idx_tmp LIKE lv_hour_idx,
         lv_qty          LIKE wa_matnr_date_time-qty,
         l_check_last_time TYPE i,
         l_modify_tabix LIKE sy-tabix.

*---
  LOOP AT it_ztmm_nstl ASSIGNING <fs_ztmm_nstl>.
    PERFORM get_time_from_minutes USING    <fs_ztmm_nstl>-feed_cycle
                                  CHANGING lv_timeforcal.
    MOVE : 'X' TO w_check_stock.
*---
    CLEAR : lv_hour_idx.
    LOOP AT it_matnr_date_time ASSIGNING <fs_matnr_date_time>
                               WHERE matnr EQ <fs_ztmm_nstl>-matnr.

      lv_hour_idx     = lv_hour_idx + 1.   "Hour Increase
      lv_hour_idx_tmp = lv_hour_idx.       "Hour Increase

      IF lv_hour_idx LE lv_timeforcal(2).  "For Sum of During Hour Qty
        lv_qty = lv_qty + <fs_matnr_date_time>-qty.
      ENDIF.

      IF lv_hour_idx EQ 1.
        CLEAR : it_worktime.
        READ TABLE it_worktime INDEX lv_hour_idx.
        lv_sdate = <fs_matnr_date_time>-sdate.   "Start Date
        lv_stime = <fs_matnr_date_time>-stime.   "Start Time
*--- Adjusting sdate, stime by PBS OUT TIME(ZTIME)
        PERFORM sdate_stime_cal USING    <fs_matnr_date_time>-sdate
*                                         p_datum     "Begin Date
                                         <fs_matnr_date_time>-stime
                                                     "Begin Time
                                         <fs_matnr_date_time>-ztime
                                                     "PBS Out time
                                         lv_hour_idx
                                CHANGING lv_sdate
                                         lv_stime.
      ENDIF.

      IF lv_hour_idx EQ lv_timeforcal(2).
        PERFORM make_wa_data_for_to USING lv_qty
                                          lv_sdate
                                          lv_stime.
        MOVE-CORRESPONDING wa_data_for_to TO it_data_for_to.
        APPEND it_data_for_to.
        MOVE : sy-tabix TO l_modify_tabix.
        ASSIGN : wa_data_for_to TO <fs_data_for_to>.
        CLEAR : lv_hour_idx, lv_qty.
      ENDIF.

      AT END OF matnr.
        IF lv_hour_idx_tmp EQ lv_timeforcal(2).
          CONTINUE.
        ENDIF.
*---
        CLEAR : l_check_last_time.
        l_check_last_time = lv_hour_idx / lv_timeforcal(2) * 100.
        IF l_check_last_time LT 50.
          <fs_data_for_to>-qty = lv_qty + <fs_data_for_to>-qty.
          MOVE : <fs_data_for_to>-qty TO <fs_data_for_to>-menge.
          MODIFY it_data_for_to FROM <fs_data_for_to>
                                INDEX l_modify_tabix.
          CLEAR : lv_hour_idx, lv_qty.
          CONTINUE.
        ENDIF.
*---
        PERFORM make_wa_data_for_to USING lv_qty
                                          lv_sdate
                                          lv_stime.

        MOVE-CORRESPONDING wa_data_for_to TO it_data_for_to.
        APPEND it_data_for_to.
        MOVE : sy-tabix TO l_modify_tabix.
        ASSIGN : wa_data_for_to TO <fs_data_for_to>.
        CLEAR: lv_hour_idx_tmp, lv_qty.
      ENDAT.
    ENDLOOP.
  ENDLOOP.

*---
  DATA : lv_remainder TYPE p,      " Remainder
         lv_quotient  TYPE p,      " Quotient
         l_tabix LIKE sy-tabix.

  LOOP AT it_data_for_to ASSIGNING <fs_data_for_to>.
    MOVE : sy-tabix TO l_tabix.
*--- Begin of Rounding Quantity Check
    PERFORM get_rdmng USING    <fs_data_for_to>-matnr
                      CHANGING <fs_data_for_to>-rdmng.
*--- If there is Rounding Quantity,
*--- qty is to be least multiple of Rounding Quantity.
*--- A. Get Remainder  : mod
*--- B. Get quotient   : div
    CLEAR : lv_remainder, lv_quotient.

    IF NOT <fs_data_for_to>-rdmng IS INITIAL.
      lv_remainder = <fs_data_for_to>-qty MOD <fs_data_for_to>-rdmng.
      lv_quotient  = <fs_data_for_to>-qty DIV <fs_data_for_to>-rdmng.
    ENDIF.

    <fs_data_for_to>-tqty = <fs_data_for_to>-qty.

    IF NOT lv_remainder IS INITIAL.
      lv_quotient          = lv_quotient + 1.
      <fs_data_for_to>-tqty  = lv_quotient * <fs_data_for_to>-rdmng.
    ENDIF.
*--- End of Rounding Quantity Check
    MODIFY it_data_for_to FROM <fs_data_for_to> INDEX l_tabix.
  ENDLOOP.

*---
*  DATA : lv_tommorow TYPE d.
*  lv_tommorow = p_datum + 1.
*  DELETE it_data_for_to WHERE sdate EQ lv_tommorow
*                          AND stime GT '030000'.
ENDFORM.                    " get_it_data_for_to

*&---------------------------------------------------------------------*
*&      Form  get_time_from_minutes
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_ZTMM_NSTL>_FEED_CYCLE  text
*      <--P_LV_TIMEFORCAL  text
*----------------------------------------------------------------------*
FORM get_time_from_minutes USING    value(im_minutes)
                           CHANGING value(ex_time) TYPE t.
*---
  CLEAR : ex_time.

  DATA : BEGIN OF ls_time,
           hour(2) TYPE n,
           minute(2) TYPE n,
           second(2) TYPE n,
         END OF ls_time.

  ls_time-minute = im_minutes MOD 60.
  ls_time-hour   = im_minutes DIV 60.

  MOVE ls_time TO ex_time.
ENDFORM.                    " get_time_from_minutes

*&---------------------------------------------------------------------*
*&      Form  sdate_stime_cal
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_TOCRED  text
*      -->P_<FS_MATNR_DATE_TIME>_STIME  text
*      -->P_<FS_MATNR_DATE_TIME>_ZTIME  text
*      -->P_LV_HOUR_IDX  text
*      <--P_LV_SDATE  text
*      <--P_LV_STIME  text
*----------------------------------------------------------------------*
FORM sdate_stime_cal USING    value(im_date) TYPE d
                              value(im_time) TYPE t
                              value(im_minutes)
                              p_hour_idx
                     CHANGING value(ex_date) TYPE d
                              value(ex_time) TYPE t.
*---
  DATA : lv_time    TYPE t,
         lv_hoursum TYPE p.

  DATA : l_hour_idx TYPE i,
         l_tabix TYPE i.

  DATA : l_pbsout_adjust LIKE it_worktime-begzt.

  CLEAR : ex_date, ex_time, l_pbsout_adjust.

*. PBS Out Time Related
* 1. 60  -> Present time
* 2. 120 -> +60
* 3. 180 -> +120
* 4. 240 -> +180
* 5. 300 -> +240

  IF im_minutes LE 60.
    im_minutes = 60.
  ENDIF.

  im_minutes = im_minutes - 60.

  PERFORM get_time_from_minutes USING    im_minutes
                                CHANGING lv_time.

  CONCATENATE im_date im_time INTO l_pbsout_adjust.

  CLEAR : it_worktime_copy.
  READ TABLE it_worktime_copy WITH KEY begzt = l_pbsout_adjust.
  MOVE : sy-tabix TO l_tabix.

  l_tabix = l_tabix + lv_time(2).

  CLEAR : it_worktime_copy.
  READ TABLE it_worktime_copy INDEX l_tabix.

  MOVE : it_worktime_copy-begzt(8)   TO ex_date,
         it_worktime_copy-begzt+8(6) TO ex_time.

*  CLEAR : it_worktime.
*  READ TABLE it_worktime WITH KEY begzt+8(6) = im_time.
*  MOVE : sy-tabix TO l_tabix.
*
*  IF l_tabix EQ w_lines.
*    CLEAR : l_tabix.
*  ENDIF.
*
*  l_tabix = l_tabix + lv_time(2).
*
*  CLEAR : it_worktime.
*  READ TABLE it_worktime INDEX l_tabix.
*
*  MOVE : it_worktime-begzt+8(6) TO ex_time.
*
*  ex_date = im_date.
*
*  IF ex_time LE '040000'.
*    ex_date = ex_date + 1.
*  ELSE.
*    ex_date = ex_date.
*  ENDIF.
ENDFORM.                    " sdate_stime_cal

*&---------------------------------------------------------------------*
*&      Form  make_wa_data_for_to
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_QTY  text
*      -->P_LV_SDATE  text
*      -->P_LV_STIME  text
*----------------------------------------------------------------------*
FORM make_wa_data_for_to USING    value(im_qty)
                                  value(im_sdate)
                                  value(im_stime).
*---
  CONSTANTS : c_onehour   TYPE t VALUE '005959'.

*---
  MOVE : <fs_matnr_date_time> TO wa_data_for_to,
         im_sdate             TO wa_data_for_to-sdate,
         im_stime             TO wa_data_for_to-stime.

  MOVE : <fs_ztmm_nstl>-lpmin TO wa_data_for_to-lpmin.

  MOVE : im_qty               TO wa_data_for_to-menge.

*--- check stock ( X = quantity - current stock
*                               + safety stock
*                               - open TO quantity )
  CLEAR : it_stock, it_open.
  READ TABLE it_stock WITH KEY matnr = <fs_ztmm_nstl>-matnr.
  READ TABLE it_open WITH KEY matnr = <fs_ztmm_nstl>-matnr.

  IF w_check_stock NE space AND <fs_ztmm_nstl>-stock_check NE space.
    wa_data_for_to-qty = im_qty - it_stock-gesme + <fs_ztmm_nstl>-lpmin
                                - it_open-vsola.
    CLEAR : w_check_stock.
  ELSE.
    wa_data_for_to-qty = im_qty.
  ENDIF.

  MOVE : it_stock-gesme TO wa_data_for_to-gesme,
         it_open-vsola  TO wa_data_for_to-vsola.

*--- Get Source Storage type/bin
  PERFORM get_sorce_storage_type_bin USING    wa_data_for_to-matnr
                                     CHANGING wa_data_for_to-src_lgtyp
                                              wa_data_for_to-src_lgpla.

*--- Get Destination Storage type/bin
  PERFORM get_des_storage_type_bin USING    wa_data_for_to-matnr
                                   CHANGING wa_data_for_to-des_lgtyp
                                            wa_data_for_to-des_lgpla.

*--- 1.
  wa_data_for_to-etime = wa_data_for_to-stime + c_onehour.

  IF wa_data_for_to-etime = '000000'.
    wa_data_for_to-edate = wa_data_for_to-sdate + 1.
  ELSE.
    wa_data_for_to-edate = wa_data_for_to-sdate.
  ENDIF.
ENDFORM.                    " make_wa_data_for_to

*&---------------------------------------------------------------------*
*&      Form  get_sorce_storage_type_bin
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_DATA_FOR_TO_MATNR  text
*      <--P_WA_DATA_FOR_TO_SRC_LGTYP  text
*      <--P_WA_DATA_FOR_TO_SRC_LGPLA  text
*----------------------------------------------------------------------*
FORM get_sorce_storage_type_bin USING    value(im_matnr)
                                CHANGING value(ex_src_lgtyp)  "Storage T
                                         value(ex_src_lgpla). "Storage b
*---
  CLEAR : ex_src_lgtyp, ex_src_lgpla.

  SELECT SINGLE lgtyp lgpla INTO (ex_src_lgtyp, ex_src_lgpla)
                            FROM mlgt
                           WHERE matnr EQ im_matnr
                             AND lvorm EQ space.
ENDFORM.                    " get_sorce_storage_type_bin

*&---------------------------------------------------------------------*
*&      Form  get_des_storage_type_bin
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_DATA_FOR_TO_MATNR  text
*      <--P_WA_DATA_FOR_TO_DES_LGTYP  text
*      <--P_WA_DATA_FOR_TO_DES_LGPLA  text
*----------------------------------------------------------------------*
FORM get_des_storage_type_bin USING    value(im_matnr)
                              CHANGING value(ex_des_lgtyp)
                                       value(ex_des_lgpla).
*---
  CLEAR : ex_des_lgtyp, ex_des_lgpla.

  SELECT SINGLE lgtyp lgpla INTO (ex_des_lgtyp, ex_des_lgpla)
                            FROM pkhd
                           WHERE matnr EQ im_matnr.
ENDFORM.                    " get_des_storage_type_bin

*&---------------------------------------------------------------------*
*&      Form  get_rdmng
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_DATA_FOR_TO>_MATNR  text
*      <--P_<FS_DATA_FOR_TO>_RDMNG  text
*----------------------------------------------------------------------*
FORM get_rdmng USING    value(im_matnr)
               CHANGING value(ex_rdmng) TYPE mlgt-rdmng. "Rounding qty
*---
  CLEAR : ex_rdmng.

  SELECT SINGLE rdmng INTO ex_rdmng
                      FROM mlgt   "Material Data for Each Storage Type
                     WHERE matnr EQ im_matnr
                       AND lvorm EQ space.
  " Deletion flag for all material data of a storage type
ENDFORM.                    " get_rdmng

*&---------------------------------------------------------------------*
*&      Form  process_it_data_for_to
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_it_data_for_to.
*---
  DATA : l_messa(80),
         l_stats LIKE ztmm_nstl_log-stats.

  CLEAR : it_write, it_write[], l_stats.

  if p_shift eq 'X'.
    delete it_data_for_to where sdate <= sy-datum
                            and stime <  sy-uzeit.
  endif.

  LOOP AT it_data_for_to ASSIGNING <fs_data_for_to>.
    CLEAR : l_stats.
*--- App Doc No
    PERFORM number_get_next USING    c_nro_nr_09   "NRO Interval
                                     w_nro_object  "NRO Object
                            CHANGING w_zdocno.     "App Doc No
    COMMIT WORK.

    IF <fs_data_for_to>-tqty GE 1.
*--- Begin of Create TO (/nLT01)
*--- BDC Processing of /nLT01
      PERFORM bdc_processing_lt01 TABLES   it_bdcmsgcoll
                                  USING    w_zdocno
                                  CHANGING w_subrc.
*--- Begin of Change TO Header (/nLT1A)
      IF w_subrc EQ 0.
        CLEAR : wa_bdcmsgcoll.
        READ TABLE it_bdcmsgcoll INTO wa_bdcmsgcoll
                                 WITH KEY msgtyp = 'S'.
        CHECK sy-subrc EQ 0.
        PERFORM bdc_processing_lta1 TABLES   it_bdcmsgcoll
                                    USING    w_zdocno
                                             wa_bdcmsgcoll-msgv1
                                    CHANGING w_subrc.
        IF w_subrc EQ 0.
          MOVE : 'C' TO l_stats.
        ELSE.
          MOVE : 'H' TO l_stats.
        ENDIF.
      ENDIF.
*--- End of Change TO Header (/nLT1A)
    ENDIF.

    MOVE-CORRESPONDING <fs_data_for_to> TO it_write.
    MOVE : w_zdocno                     TO it_write-w_docno,
           l_stats                      TO it_write-stats.

    IF <fs_data_for_to>-tqty GE 1.
      IF w_subrc EQ 0.
        MOVE : c_green             TO it_write-linecolor,
               'S'                 TO it_write-msgty.
        CLEAR : it_bdcmsgcoll.
        READ TABLE it_bdcmsgcoll INDEX 1.
        MOVE : it_bdcmsgcoll-msgv2 TO it_write-tanum.
      ELSE.
        MOVE : c_red               TO it_write-linecolor,
               'E'                 TO it_write-msgty.
      ENDIF.
    ELSE.
      CLEAR : it_bdcmsgcoll, it_bdcmsgcoll[].
    ENDIF.

*--- message
    CLEAR : it_bdcmsgcoll, l_messa.
    READ TABLE it_bdcmsgcoll WITH KEY msgtyp = 'E'.
    IF sy-subrc EQ 0.
      PERFORM get_message USING    it_bdcmsgcoll-msgid
                                   it_bdcmsgcoll-msgnr
                                   it_bdcmsgcoll-msgv1
                                   it_bdcmsgcoll-msgv2
                                   it_bdcmsgcoll-msgv3
                                   it_bdcmsgcoll-msgv4
                          CHANGING l_messa.
    ELSE.
      READ TABLE it_bdcmsgcoll WITH KEY msgtyp = 'S'.
      IF sy-subrc EQ 0.
        PERFORM get_message USING    it_bdcmsgcoll-msgid
                                     it_bdcmsgcoll-msgnr
                                     it_bdcmsgcoll-msgv1
                                     it_bdcmsgcoll-msgv2
                                     it_bdcmsgcoll-msgv3
                                     it_bdcmsgcoll-msgv4
                            CHANGING l_messa.
      ENDIF.
    ENDIF.
    MOVE : l_messa             TO it_write-messa.
    MOVE : it_bdcmsgcoll-msgid TO it_write-msgid,
           it_bdcmsgcoll-msgnr TO it_write-msgnr.
    APPEND it_write.
    CLEAR : it_bdcmsgcoll, it_bdcmsgcoll[], it_write.
  ENDLOOP.
ENDFORM.                    " process_it_data_for_to

*&---------------------------------------------------------------------*
*&      Form  number_get_next
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_C_NRO_NR_09  text
*      -->P_W_NRO_OBJECT  text
*      <--P_W_ZDOCNO  text
*----------------------------------------------------------------------*
FORM number_get_next USING    value(p_nro_interval) LIKE inri-nrrangenr
                              value(p_nro_object)   LIKE inri-object
                     CHANGING value(p_nro_next).
*---
  CLEAR : p_nro_next.

  CALL FUNCTION 'NUMBER_GET_NEXT'
       EXPORTING
            nr_range_nr             = p_nro_interval
            object                  = p_nro_object
       IMPORTING
            number                  = p_nro_next
       EXCEPTIONS
            interval_not_found      = 1
            number_range_not_intern = 2
            object_not_found        = 3
            quantity_is_0           = 4
            quantity_is_not_1       = 5
            interval_overflow       = 6
            OTHERS                  = 7.
ENDFORM.                    " number_get_next

*&---------------------------------------------------------------------*
*&      Form  bdc_processing_lt01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_BDCMSGCOLL  text
*      -->P_W_ZDOCNO  text
*      <--P_W_SUBRC  text
*----------------------------------------------------------------------*
FORM bdc_processing_lt01 TABLES   ext_bdcmsgcoll STRUCTURE bdcmsgcoll
                         USING    value(p_zdocno)
                         CHANGING value(p_subrc).
*---
  CLEAR : ext_bdcmsgcoll, ext_bdcmsgcoll[], p_subrc.

  DATA : lv_bwlvs_002     TYPE bdcdata-fval,   "Movement type
         lv_matnr_003     TYPE bdcdata-fval,
         lv_anfme_004     TYPE bdcdata-fval,
         lv_anfme_007     TYPE bdcdata-fval,
         lv_altme_008     TYPE bdcdata-fval,
         lv_vltyp_009     TYPE bdcdata-fval,
         lv_vlpla_010     TYPE bdcdata-fval,
         lv_nltyp_011     TYPE bdcdata-fval,
         lv_nlpla_012     TYPE bdcdata-fval,
         lv_refnr_013     TYPE bdcdata-fval.   "Group(Feeder)

  lv_bwlvs_002 = '850'.
  lv_refnr_013 =  <fs_data_for_to>-feedr. "Group(Feeder)
  lv_matnr_003  = <fs_data_for_to>-matnr. "Material '327003K100'
  lv_anfme_004  = <fs_data_for_to>-tqty.
  lv_anfme_007  = <fs_data_for_to>-tqty.
  lv_altme_008  = <fs_data_for_to>-unit.
  lv_vltyp_009  = <fs_data_for_to>-src_lgtyp. "Src Storage Type
  lv_vlpla_010  = <fs_data_for_to>-src_lgpla. "Src Storage Bin
  lv_nltyp_011  = <fs_data_for_to>-des_lgtyp. "Des Storage Type
  lv_nlpla_012  = <fs_data_for_to>-des_lgpla. "Des Storage Bin

  CONDENSE : lv_bwlvs_002,  "Movement type
             lv_matnr_003,
             lv_anfme_004,
             lv_anfme_007,
             lv_altme_008,
             lv_vltyp_009,
             lv_vlpla_010,
             lv_nltyp_011,
             lv_nlpla_012,
             lv_refnr_013.

*--- BDC for LT01(Create TO)
  CALL FUNCTION 'Z_FMM_6012_01'
       EXPORTING
            lgnum_001 = 'P01'  "Warehouse number
            refnr_013 = lv_refnr_013  "Group(Feeder)
            bwlvs_002 = lv_bwlvs_002  "Movement type '999'
            matnr_003 = lv_matnr_003  "Material '327003K100'
            anfme_004 = lv_anfme_004
            werks_005 = 'P001'  "Plant
            lgort_006 = 'P400'  "Storage Location
            anfme_007 = lv_anfme_007
            altme_008 = lv_altme_008
            vltyp_009 = lv_vltyp_009  "Src Storage Type '434'
            vlpla_010 = lv_vlpla_010  "Src Storage Bin 'AA-01-11'
            nltyp_011 = lv_nltyp_011  "Des Storage Type '443'
            nlpla_012 = lv_nlpla_012  "Des Storage Bin 'TS-01'
       IMPORTING
            subrc     = p_subrc
       TABLES
            messtab   = ext_bdcmsgcoll[].
ENDFORM.                    " bdc_processing_lt01

*&---------------------------------------------------------------------*
*&      Form  bdc_processing_lta1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_BDCMSGCOLL  text
*      -->P_W_ZDOCNO  text
*      -->P_WA_BDCMSGCOLL_MSGV1  text
*      <--P_W_SUBRC  text
*----------------------------------------------------------------------*
FORM bdc_processing_lta1 TABLES   ext_bdcmsgcoll STRUCTURE bdcmsgcoll
                         USING    value(p_zdocno)
                                  value(p_msgv1)
                         CHANGING value(p_subrc).
*---
  CLEAR : ext_bdcmsgcoll, ext_bdcmsgcoll[], p_subrc.

  DATA : lv_tanum_001     TYPE bdcdata-fval,  "TO number
         lv_lgnum_002     TYPE bdcdata-fval,  "Warehouse number
         lv_stdat_003     TYPE bdcdata-fval,  "Start date
         lv_stuzt_004     TYPE bdcdata-fval,  "Start time
         lv_endat_005     TYPE bdcdata-fval,  "End date
         lv_enuzt_006     TYPE bdcdata-fval.  "End time

*--- (Begin) Adjust Date format in user by user
  DATA : lv_date(8).

  CLEAR: lv_date.

  PERFORM user_date_format USING    sy-uname
                                    <fs_data_for_to>-sdate
                           CHANGING lv_date.

  lv_stdat_003 = lv_date. "Start date

*---
  PERFORM user_date_format USING    sy-uname
                                    <fs_data_for_to>-edate
                           CHANGING lv_date.

  lv_endat_005 = lv_date.  "End date
*--- (End)Adjust Date format in user by user

  lv_tanum_001 = p_msgv1.                       "TO number  '813'
  lv_lgnum_002 = 'P01'.                         "Warehouse number
  lv_stuzt_004 = <fs_data_for_to>-stime.        "Start time
  lv_enuzt_006 = <fs_data_for_to>-etime.        "End time

  CONDENSE : lv_tanum_001,
             lv_lgnum_002,
             lv_stdat_003,
             lv_stuzt_004,
             lv_endat_005,
             lv_enuzt_006.

*--- BDC for LTA1(Change TO Header)
  CALL FUNCTION 'Z_FMM_6012_02'
       EXPORTING
            tanum_001 = lv_tanum_001
            lgnum_002 = lv_lgnum_002
            stdat_003 = lv_stdat_003
            stuzt_004 = lv_stuzt_004
            endat_005 = lv_endat_005
            enuzt_006 = lv_enuzt_006
       IMPORTING
            subrc     = p_subrc
       TABLES
            messtab   = ext_bdcmsgcoll[].

*--- (Begin)BDC Log to the table ZTLOG
  IF ext_bdcmsgcoll[] IS INITIAL.  "SUCCESS
    CLEAR : wa_bdcmsgcoll.
    wa_bdcmsgcoll-tcode   = 'LT1A'.
    wa_bdcmsgcoll-msgtyp  = 'S'.  "SUCCESS
    wa_bdcmsgcoll-msgspra = 'E'.
    wa_bdcmsgcoll-msgid   = 'ZMMM'.
    wa_bdcmsgcoll-msgnr   = '999'.
    wa_bdcmsgcoll-msgv1   = 'Transfer order'.
    wa_bdcmsgcoll-msgv2   = lv_tanum_001.
    wa_bdcmsgcoll-msgv3   = 'Start/End Date/Time'.
    wa_bdcmsgcoll-msgv4   = 'is changed.'.
    APPEND wa_bdcmsgcoll TO ext_bdcmsgcoll[].
  ENDIF.
ENDFORM.                    " bdc_processing_lta1

*&---------------------------------------------------------------------*
*&      Form  user_date_format
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_UNAME  text
*      -->P_<FS_DATA_FOR_TO>_SDATE  text
*      <--P_LV_DATE  text
*----------------------------------------------------------------------*
FORM user_date_format USING    value(p_user)     LIKE sy-uname
                               value(p_date)     LIKE sy-datum
                      CHANGING value(p_userdate) TYPE char8.
*---
  CLEAR: p_userdate.
  DATA: yyyy(4).  "year
  DATA: mm(2).    "day
  DATA: dd(2).    "month
  DATA: datfm LIKE usr01-datfm.  "date format

  SELECT SINGLE datfm INTO datfm
    FROM usr01
    WHERE bname = p_user.
** datfm
*1 DD.MM.YYYY
*2 MM/DD/YYYY
*3 MM-DD-YYYY
*4 YYYY.MM.DD
*5 YYYY/MM/DD
*6 YYYY-MM-DD
  yyyy = p_date+0(4).
  mm   = p_date+4(2).
  dd   = p_date+6(2).

  CASE datfm.
    WHEN 1.
      p_userdate+0(2) = dd.
      p_userdate+2(2) = mm.
      p_userdate+4(4) = yyyy.
    WHEN 2 OR 3.
      p_userdate+0(2) = mm.
      p_userdate+2(2) = dd.
      p_userdate+4(4) = yyyy.
    WHEN 4 OR 5 OR 6.
      p_userdate+0(4) = yyyy.
      p_userdate+4(2) = mm.
      p_userdate+6(2) = dd.
  ENDCASE.
ENDFORM.                    " user_date_format

*&---------------------------------------------------------------------*
*&      Form  get_message
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_BDCMSGCOLL_MSGID  text
*      -->P_IT_BDCMSGCOLL_MSGNR  text
*      -->P_IT_BDCMSGCOLL_MSGV1  text
*      -->P_IT_BDCMSGCOLL_MSGV2  text
*      -->P_IT_BDCMSGCOLL_MSGV3  text
*      -->P_IT_BDCMSGCOLL_MSGV4  text
*      <--P_L_MESSA  text
*----------------------------------------------------------------------*
FORM get_message USING    p_msgid
                          p_msgnr
                          p_msgv1
                          p_msgv2
                          p_msgv3
                          p_msgv4
                 CHANGING p_l_messa.
*---
  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
       EXPORTING
            msgid               = p_msgid
            msgnr               = p_msgnr
            msgv1               = p_msgv1
            msgv2               = p_msgv2
            msgv3               = p_msgv3
            msgv4               = p_msgv4
       IMPORTING
            message_text_output = p_l_messa.
ENDFORM.                    " get_message

*&---------------------------------------------------------------------*
*&      Form  check_input_values
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_input_values.
*---
  CLEAR : tc37a.

*  SELECT SINGLE kaptprog INTO tc37a-kaptprog
*                         FROM tc37a
*                        WHERE schgrup EQ c_schgrup
*                          AND kaptprog EQ p_kaptpr.
*
*  IF sy-subrc NE 0.
*    SET CURSOR FIELD 'P_KAPTPR'.
*    MESSAGE e999 WITH text-m10.
*  ENDIF.

*---
  IF NOT ( p_rp EQ '01' OR p_rp EQ '06' ).
    SET CURSOR FIELD 'P_RP'.
    MESSAGE e999 WITH text-m02.
  ENDIF.

  MOVE: sy-datum TO w_datum,
        sy-uzeit TO w_uzeit.
ENDFORM.                    " check_input_values

*&---------------------------------------------------------------------*
*&      Form  call_working_time_function
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_working_time_function.
*---
  CLEAR : it_worktime, it_worktime[], it_worktime_copy,
          it_worktime_copy[], it_dummy, it_dummy[].

*--- insert by stlim (2004/07/28)
  CALL FUNCTION 'Z_FMM_GET_WORKING_TIME'
       EXPORTING
            i_datum              = w_datum
            i_day                = c_day
            i_arbpl              = p_arbpl
       IMPORTING
            e_date_curr          = w_date_curr
            e_tprog_curr         = w_tprog_curr
            e_date_next          = w_date_next
            e_tprog_next         = w_tprog_next
       TABLES
            t_working_time       = it_dummy
            t_1t                 = it_worktime
       EXCEPTIONS
            cannot_read_dayname  = 1
            incorrect_shift_info = 2
            incorrect_capa_info  = 3
            OTHERS               = 4.

  IF sy-subrc NE 0.
    CASE sy-subrc.
      WHEN 1.
        MESSAGE e999 WITH text-m21.
      WHEN 2.
        MESSAGE e999 WITH text-m22.
      WHEN 3.
        MESSAGE e999 WITH text-m23.
      WHEN 4.
        MESSAGE e999 WITH text-m24.
    ENDCASE.
  ENDIF.
*--- end of insert

*--- blocked by stlim (2004/07/28)
*  CALL FUNCTION 'Z_FMM_NSTL_WORKTIME'
*       EXPORTING
*            i_datum         = p_datum
*            i_arbpl         = p_arbpl
*       TABLES
*            t_nstl_worktime = it_worktime.
*--- end of block

  it_worktime_copy[] = it_worktime[].
ENDFORM.                    " call_working_time_function
