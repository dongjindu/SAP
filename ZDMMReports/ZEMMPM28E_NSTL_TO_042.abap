************************************************************************
* Program Name      : ZEMMPM28E_NSTL_TO_042
* Author            : Hakchin Kim
* Creation Date     : 2004.05.07.
* Specifications By : Sung-Tae Lim
* Pattern           : Report 1-1
* Development Request No : UD1K910127
* Addl Documentation:
* Description       : Daily Supply to Line (Non Supply to Line)
*                     Transfer Order Creation & Header Change
* Modification Logs
* Date         Developer       RequestNo      Description
* 2004.05.07.  Sung-Tae Lim    UD1K910127     Initial Coding
*
************************************************************************

REPORT zemmpm28e_nstl_to_042 MESSAGE-ID zmmm
                             NO STANDARD PAGE HEADING
                             LINE-SIZE 400.

**---
INCLUDE : zrmmpmxxr_incl.

*--- Constnats for Time
CONSTANTS : c_begintime TYPE t VALUE '063000',
            c_onehour   TYPE t VALUE '010000'.

**** Constants&Vars for Number range object ****************************
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

* For Return code
DATA : w_subrc LIKE sy-subrc.

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
       END OF wa_matnr_date_time.

DATA : it_matnr_date_time LIKE TABLE OF wa_matnr_date_time.

FIELD-SYMBOLS : <fs_matnr_date_time> LIKE LINE OF it_matnr_date_time.

DATA : it_data_for_to LIKE it_matnr_date_time WITH HEADER LINE.
DATA : wa_data_for_to LIKE LINE OF it_data_for_to.

FIELD-SYMBOLS : <fs_data_for_to> LIKE LINE OF it_data_for_to.

DATA : BEGIN OF wa_ztmm_nstl,
         matnr      LIKE ztmm_nstl-matnr,
         datum      LIKE ztmm_nstl-datum,
         rpsta      LIKE ztmm_nstl-rpsta,
         time01     LIKE ztmm_nstl-time01,
         time02     LIKE ztmm_nstl-time02,
         time03     LIKE ztmm_nstl-time03,
         time04     LIKE ztmm_nstl-time04,
         time05     LIKE ztmm_nstl-time05,
         time06     LIKE ztmm_nstl-time06,
         time07     LIKE ztmm_nstl-time07,
         time08     LIKE ztmm_nstl-time08,
         time09     LIKE ztmm_nstl-time09,
         time10     LIKE ztmm_nstl-time10,
         meins      LIKE ztmm_nstl-meins,
*        INCLUDE STRUCTURE ztmm_nstl.
         feedr      LIKE ztmm_mast-feedr,      "Feeder
         feed_cycle LIKE ztmm_mast-feed_cycle, "Feed Cycle
         ztime      LIKE ztmm_mast-ztime,      "Time from PBS out to W/S
         zline      LIKE ztmm_mast-zline,
         works      LIKE ztmm_mast-works,
         rh_lh      LIKE ztmm_mast-rh_lh,
         dispo      LIKE ztmm_mast-dispo,
         lpmin      LIKE ztmm_mast-lpmin,
         stock_check LIKE ztmm_mast-stock_check,
       END OF wa_ztmm_nstl.

DATA : it_ztmm_nstl LIKE TABLE OF wa_ztmm_nstl.
FIELD-SYMBOLS : <fs_ztmm_nstl> LIKE LINE OF it_ztmm_nstl.

DATA : BEGIN OF it_itab OCCURS 0.
        INCLUDE STRUCTURE wa_matnr_date_time.
DATA :   tanum LIKE ltap-tanum,     " TO number
         w_docno TYPE num10,
         linecolor(4),     " ALV Color
         messa(80),
         msgty LIKE ztmm_nstl_log-msgty,
         msgid LIKE ztmm_nstl_log-msgid,
         msgnr LIKE ztmm_nstl_log-msgnr,
       END OF it_itab.

CONSTANTS : c_rp06(2) VALUE '06',
            c_rp04(2) VALUE '01'.

DATA : w_1shift_overtime(1),
       w_2shift_overtime(1),
       w_tot_lines TYPE i,     " full time
       w_lines TYPE i,     " time that delete overtime
       w_1shift_exec(1),
       w_2shift_exec(1),
       w_check_stock(1).

DATA : it_ztmm_nstl_time LIKE ztmm_nstl_time OCCURS 0 WITH HEADER LINE.

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

DATA : it_worktime LIKE zsmm_worktime OCCURS 0 WITH HEADER LINE.


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
**--- end of insert


**--- Selection Screen
SELECTION-SCREEN BEGIN OF BLOCK bl_1 WITH FRAME TITLE text-bl1.
PARAMETERS : p_tocred LIKE ztmm_nstl-datum OBLIGATORY,
             p_etime TYPE t.
SELECTION-SCREEN END OF BLOCK bl_1.

SELECT-OPTIONS : s_matnr FOR mara-matnr.

**---
INITIALIZATION.
  PERFORM event_build USING w_eventcat[].


**---
START-OF-SELECTION.
  PERFORM check_shift_info.

  READ TABLE it_worktime WITH KEY schnr = '2'.
  CHECK sy-subrc EQ 0.

  PERFORM get_it_ztmm_nstl.       "Raw Data
  PERFORM get_it_matnr_date_time. "Raw Data by time
  PERFORM get_it_data_for_to.     "Raw Data for T/O Creation

  IF it_data_for_to IS INITIAL.
    MESSAGE s999(zmmm) WITH 'There is no data!'(001).
  ELSE.
    PERFORM process_it_data_for_to.      "Process Data in Forground.
  ENDIF.


**---
END-OF-SELECTION.
  PERFORM update_table.
  PERFORM comment_build.     " USING w_top_of_page[].
  PERFORM make_alv_grid.


**---
TOP-OF-PAGE.
  PERFORM top_of_page.


**---
END-OF-PAGE.


**---
AT LINE-SELECTION.


**---
AT USER-COMMAND.


**---
TOP-OF-PAGE DURING LINE-SELECTION.




**---

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
  SELECT SINGLE zindx INTO ztmm_nstl_time-zindx
                      FROM ztmm_nstl_time.

  IF sy-subrc NE 0.
    MESSAGE e999 WITH text-m01.
  ENDIF.

*---
  DATA : lv_nstl_date TYPE sy-datum.

  lv_nstl_date = p_tocred.

*---
  SELECT a~matnr  AS matnr
         a~time11 AS time01
         a~time12 AS time02
         a~time13 AS time03
         a~time14 AS time04
         a~time15 AS time05
         a~time16 AS time06
         a~time17 AS time07
         a~time18 AS time08
         a~time19 AS time09
         a~time20 AS time10
*         a~time11 AS time11
*         a~time12 AS time12
*         a~time13 AS time13
*         a~time14 AS time14
*         a~time15 AS time15
*         a~time16 AS time16
*         a~time17 AS time17
*         a~time18 AS time18
*         a~time19 AS time19
*         a~time20 AS time20
         a~meins  AS meins
         b~feedr  AS feedr
         b~feed_cycle AS feed_cycle
         b~ztime  AS ztime
         b~zline  AS zline
         b~works  AS works
         b~rh_lh  AS rh_lh
         b~dispo  AS dispo
         b~lpmin  AS lpmin
         b~stock_check AS stock_check
                  INTO CORRESPONDING FIELDS OF TABLE it_ztmm_nstl
                  FROM ztmm_nstl AS a INNER JOIN ztmm_mast AS b
                    ON a~matnr EQ b~matnr
                   AND b~spptl EQ 'N'      "Non Supply To Line
                 WHERE rpsta EQ c_rp04
                   AND datum EQ lv_nstl_date
                   AND zline LIKE 'B%'
                   AND a~matnr IN s_matnr.
ENDFORM.                    " get_it_ztmm_nstl

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
  PERFORM check_time.
  PERFORM get_current_stock.
  PERFORM get_open_to_quantity.

*---
  DATA : lv_sdate TYPE d,
         lv_stime TYPE t,
         lv_edate TYPE d,
         lv_etime TYPE t.

  DATA : lv_time_qty(19). "Time Quantity Field (TIME01,TIME02,... )

  FIELD-SYMBOLS : <fs_qty>.

  DATA: lv_time_idx(2) TYPE n.

*---
  LOOP AT it_ztmm_nstl INTO wa_ztmm_nstl.
    CLEAR : lv_time_idx.
    DO 10 TIMES.
*    DO w_tot_lines TIMES.
      CLEAR : it_ztmm_nstl_time.
      lv_time_idx = 10 + sy-index.
*      MOVE : sy-index TO lv_time_idx.

      READ TABLE it_ztmm_nstl_time INDEX lv_time_idx.
      MOVE : p_tocred                TO lv_sdate,
             it_ztmm_nstl_time-stime TO lv_stime.

      IF lv_stime EQ '000000'.
        lv_sdate = lv_sdate + 1.
      ENDIF.

      MOVE : lv_sdate                TO lv_edate,
             it_ztmm_nstl_time-etime TO lv_etime.

      IF lv_etime EQ '000000'.
        lv_edate = lv_edate + 1.
      ENDIF.

      lv_time_idx = lv_time_idx - 10.

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

*      IF NOT wa_matnr_date_time-qty IS INITIAL.
      APPEND wa_matnr_date_time TO it_matnr_date_time.
*      ENDIF.
    ENDDO.
  ENDLOOP.
ENDFORM.                    " get_it_matnr_date_time

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
  PERFORM check_overtime.
*  PERFORM get_current_stock.

  SELECT SINGLE stime INTO w_1stime
                      FROM ztmm_nstl_time
                     WHERE zindx EQ '1'.
  SELECT MAX( stime ) AS w_1etime
         MAX( etime ) AS w_2stime
                                  INTO (w_1etime, w_2stime)
                                  FROM ztmm_nstl_time
                                 WHERE overt EQ '1'.
  SELECT MAX( stime ) INTO w_2etime
                      FROM ztmm_nstl_time
                     WHERE overt EQ '2'.

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

*      IF <fs_matnr_date_time>-stime EQ w_2stime.
*        CONTINUE.
*      ENDIF.

      lv_hour_idx     = lv_hour_idx + 1.   "Hour Increase
      lv_hour_idx_tmp = lv_hour_idx.       "Hour Increase

      IF lv_hour_idx LE lv_timeforcal(2).  "For Sum of During Hour Qty
        lv_qty = lv_qty + <fs_matnr_date_time>-qty.
      ENDIF.

      IF lv_hour_idx EQ 1.
        CLEAR : it_ztmm_nstl_time.
        READ TABLE it_ztmm_nstl_time INDEX lv_hour_idx.
        lv_sdate = <fs_matnr_date_time>-sdate.   "Start Date
        lv_stime = <fs_matnr_date_time>-stime.   "Start Time
*--- Adjusting sdate, stime by PBS OUT TIME(ZTIME)
        PERFORM sdate_stime_cal USING    p_tocred    "Begin Date
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
*        APPEND wa_data_for_to TO it_data_for_to.
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
          MODIFY it_data_for_to FROM <fs_data_for_to>
                                INDEX l_modify_tabix.
          CONTINUE.
        ENDIF.
*---
        PERFORM make_wa_data_for_to USING lv_qty
                                          lv_sdate
                                          lv_stime.

        MOVE-CORRESPONDING wa_data_for_to TO it_data_for_to.
        APPEND it_data_for_to.
        MOVE : sy-tabix TO l_modify_tabix.
*        APPEND wa_data_for_to TO it_data_for_to.
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
  DATA : lv_tommorow TYPE d.
  lv_tommorow = p_tocred + 1.
  DELETE it_data_for_to WHERE sdate EQ lv_tommorow
                          AND stime GT '030000'.
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
*      -->P_C_BEGINTIME  text
*      -->P_<FS_MATNR_DATE_TIME>_ZTIME  text
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

  CLEAR : ex_date, ex_time.

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

  CLEAR : it_ztmm_nstl_time.
  READ TABLE it_ztmm_nstl_time WITH KEY stime = im_time.
  MOVE : sy-tabix TO l_tabix.

  IF l_tabix EQ w_lines.
    CLEAR : l_tabix.
  ENDIF.

  l_tabix = l_tabix + lv_time(2).

  CLEAR : it_ztmm_nstl_time.
  READ TABLE it_ztmm_nstl_time INDEX l_tabix.

  MOVE : it_ztmm_nstl_time-stime TO ex_time.

  ex_date = im_date.

  IF ex_time LE '040000'.
    ex_date = ex_date + 1.
  ELSE.
    ex_date = ex_date.
  ENDIF.

*  lv_hoursum = im_time(2) + lv_time(2).
*  ex_date = im_date.
*  ex_time = im_time + lv_time.
*  IF lv_hoursum >= 24.
*    ex_date = ex_date + 1.
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
  CONSTANTS : c_onehour   TYPE t VALUE '010000'.

*  DATA : lv_remainder TYPE p,      " Remainder
*         lv_quotient  TYPE p.      " Quotient

*---
  MOVE : <fs_matnr_date_time> TO wa_data_for_to,
*         im_qty               TO wa_data_for_to-qty,
         im_sdate             TO wa_data_for_to-sdate,
         im_stime             TO wa_data_for_to-stime.

  MOVE : <fs_ztmm_nstl>-lpmin TO wa_data_for_to-lpmin.

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

**--- Begin of Rounding Quantity Check
*  PERFORM get_rdmng USING    wa_data_for_to-matnr
*                    CHANGING wa_data_for_to-rdmng.
*
**--- If there is Rounding Quantity,
**--- qty is to be least multiple of Rounding Quantity.
**--- A. Get Remainder  : mod
**--- B. Get quotient   : div
*  CLEAR : lv_remainder, lv_quotient.
*
*  IF NOT wa_data_for_to-rdmng IS INITIAL.
*    lv_remainder = wa_data_for_to-qty MOD wa_data_for_to-rdmng.
*    lv_quotient  = wa_data_for_to-qty DIV wa_data_for_to-rdmng.
*  ENDIF.
*
*  wa_data_for_to-tqty = wa_data_for_to-qty.
*
*  IF NOT lv_remainder IS INITIAL.
*    lv_quotient          = lv_quotient + 1.
*    wa_data_for_to-tqty  = lv_quotient * wa_data_for_to-rdmng.
*  ENDIF.
**--- End of Rounding Quantity Check

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
*&      Form  get_rdmng
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_DATA_FOR_TO_MATNR  text
*      <--P_WA_DATA_FOR_TO_RDMNG  text
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
*&      Form  process_it_data_for_to
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_it_data_for_to.
**---
  DATA : l_messa(80).

  CLEAR : it_itab, it_itab[].

  LOOP AT it_data_for_to ASSIGNING <fs_data_for_to>.
*--- time check
    IF w_1shift_exec NE space.
      CLEAR : it_worktime.
      READ TABLE it_worktime WITH KEY schnr = '1'.
      CHECK <fs_data_for_to>-stime BETWEEN it_worktime-begzt
                                       AND it_worktime-endzt.
*      CHECK <fs_data_for_to>-stime BETWEEN w_1stime AND w_1etime.
    ELSEIF w_2shift_exec NE space.
      CLEAR : it_worktime.
      READ TABLE it_worktime WITH KEY schnr = '2'.
      CHECK ( <fs_data_for_to>-stime BETWEEN it_worktime-begzt
                                         AND '230000' )
         OR ( <fs_data_for_to>-stime BETWEEN '000000'
                                         AND it_worktime-endzt ).
*      CHECK ( <fs_data_for_to>-stime BETWEEN w_2stime AND '230000' ) OR
*            ( <fs_data_for_to>-stime BETWEEN '000000' AND w_2etime ).
    ENDIF.
*--- App Doc No
    PERFORM number_get_next USING    c_nro_nr_09   "NRO Interval
                                     w_nro_object  "NRO Object
                            CHANGING w_zdocno.     "App Doc No
    COMMIT WORK.

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
    ENDIF.
*--- End of Change TO Header (/nLT1A)

    MOVE-CORRESPONDING <fs_data_for_to> TO it_itab.
    MOVE : w_zdocno                     TO it_itab-w_docno.

    IF w_subrc EQ 0.
      MOVE : c_green             TO it_itab-linecolor,
             'S'                 TO it_itab-msgty.
      CLEAR : it_bdcmsgcoll.
      READ TABLE it_bdcmsgcoll INDEX 1.
      MOVE : it_bdcmsgcoll-msgv2 TO it_itab-tanum.
    ELSE.
      MOVE : c_red               TO it_itab-linecolor,
             'E'                 TO it_itab-msgty.
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
    MOVE : l_messa             TO it_itab-messa.
    MOVE : it_bdcmsgcoll-msgid TO it_itab-msgid,
           it_bdcmsgcoll-msgnr TO it_itab-msgnr.
    APPEND it_itab.
    CLEAR : it_bdcmsgcoll, it_bdcmsgcoll[].
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
*&      Form  update_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_table.
*---
  DATA : it_ztmm_nstl_log LIKE ztmm_nstl_log OCCURS 0 WITH HEADER LINE.

  CLEAR : it_ztmm_nstl_log, it_ztmm_nstl_log[].

  LOOP AT it_itab.
    CLEAR : ztmm_nstl_log.
    MOVE-CORRESPONDING it_itab TO it_ztmm_nstl_log.
    MOVE : it_itab-w_docno     TO it_ztmm_nstl_log-logno_h,
           it_itab-qty         TO it_ztmm_nstl_log-bdmng,
           it_itab-unit        TO it_ztmm_nstl_log-meins,
           sy-tcode            TO it_ztmm_nstl_log-ztcode,
           sy-repid            TO it_ztmm_nstl_log-zprogramm.
    it_ztmm_nstl_log-ernam = it_ztmm_nstl_log-aenam = sy-uname.
    it_ztmm_nstl_log-erdat = it_ztmm_nstl_log-aedat = sy-datum.
    it_ztmm_nstl_log-erzet = it_ztmm_nstl_log-aezet = sy-uzeit.
    APPEND it_ztmm_nstl_log.
  ENDLOOP.

  MODIFY ztmm_nstl_log FROM TABLE it_ztmm_nstl_log.
ENDFORM.                    " update_table

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
            t_outtab           = it_itab
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
    w_col_pos 'QTY'       12 'Quantity'       'QUAN' ''  'MEINS' '',
    w_col_pos 'GESME'     12 'Current Stock'  'QUAN' ''  'MEINS' '',
    w_col_pos 'LPMIN'     12 'Safety Stock'   'QUAN' ''  'MEINS' '',
    w_col_pos 'VSOLA'     12 'Open TO Qty'    'QUAN' ''  'MEINS' '',
    w_col_pos 'UNIT'      03 'UoM'            'UNIT' ''  ''      '',
    w_col_pos 'WORKS'     05 'Workstation'    'CHAR' ''  ''      '',
    w_col_pos 'RH_LH'     02 'RH/LH'          'CHAR' ''  ''      '',
    w_col_pos 'ZLINE'     02 'Line'           'CHAR' ''  ''      '',
    w_col_pos 'DISPO'     03 'MRP Controller' 'CHAR' ''  ''      '',
    w_col_pos 'FEED_CYCLE'   03 'Feed Cycle'  'NUMC' ''  ''      '',
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
  append_sortcat : '1' 'MATNR' 'IT_ITAB' 'X' ''.
ENDFORM.                    " build_sortcat

*&---------------------------------------------------------------------*
*&      Form  check_time
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_time.
*---
  CLEAR : it_ztmm_nstl_time, it_ztmm_nstl_time[], w_tot_lines.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_ztmm_nstl_time
           FROM ztmm_nstl_time.

  DESCRIBE TABLE it_ztmm_nstl_time LINES w_tot_lines.

  SORT it_ztmm_nstl_time BY zindx.

*--- check execution time
  DATA : l_stime TYPE t,
         l_etime TYPE t.

  CLEAR : w_1shift_exec, w_2shift_exec, l_stime, l_etime.

  READ TABLE it_ztmm_nstl_time INDEX 1.
  MOVE : it_ztmm_nstl_time-stime TO l_stime.

  CLEAR : it_ztmm_nstl_time.
  READ TABLE it_ztmm_nstl_time INDEX w_tot_lines.
  MOVE : it_ztmm_nstl_time-etime TO l_etime.

  IF p_etime BETWEEN l_etime AND l_stime.
    MOVE : 'X' TO w_1shift_exec.
    CLEAR : w_2shift_exec.
  ELSE.
    MOVE : 'X' TO w_2shift_exec.
    CLEAR : w_1shift_exec.
  ENDIF.

  CLEAR : it_ztmm_nstl_time.

**--- insert by stlim (2004/05/18) - temp
  MOVE : 'X' TO w_2shift_exec.
  CLEAR : w_1shift_exec.
**--- end of insert
ENDFORM.                    " check_time

*&---------------------------------------------------------------------*
*&      Form  check_overtime
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_overtime.
*---
  CLEAR : w_lines, w_1shift_overtime, w_2shift_overtime.

  CALL FUNCTION 'Z_FMM_STL_OVERTIME'
       EXPORTING
            i_datum           = sy-datum
            i_uzeit           = sy-uzeit
       CHANGING
            e_1shift_overtime = w_1shift_overtime
            e_2shift_overtime = w_2shift_overtime.

  IF w_1shift_overtime EQ space.     " no overtime
    DELETE it_ztmm_nstl_time WHERE overt EQ '1'.
  ENDIF.

  IF w_2shift_overtime EQ space.     " no overtime
    DELETE it_ztmm_nstl_time WHERE overt EQ '2'.
  ENDIF.

  DESCRIBE TABLE it_ztmm_nstl_time LINES w_lines.

  SORT it_ztmm_nstl_time BY zindx.
ENDFORM.                    " check_overtime

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

  SELECT a~matnr
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

  SELECT matnr
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
*&      Form  check_shift_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_shift_info.
*---
  CLEAR : it_worktime, it_worktime[].

  CALL FUNCTION 'Z_FMM_STL_OVERTIME'
       EXPORTING
            i_datum    = sy-datum
            i_uzeit    = sy-uzeit
       TABLES
            t_worktime = it_worktime.
ENDFORM.                    " check_shift_info
