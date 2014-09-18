************************************************************************
* Program Name      : ZEMMPM10E_STL
* Author            : Hakchin Kim
* Creation Date     : 2003.11.11.
* Specifications By : Hakchin Kim
* Pattern           : Report 1-1
* Development Request No : EMMPM10
* Addl Documentation: F/S - EMMPM10 Pull List & Transfer Order
*                                   Creation(Supply to Line)
* Description       : This program is intended to meet the requirement
*                     of HMMA plant to supply the unique material
*                     (around 300 material) by hour on the production
*                     line.
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
************************************************************************
*/ Feeding Time = Start Time

REPORT  zemmpm10e_stl
                 MESSAGE-ID zmmm
                 NO STANDARD PAGE HEADING
                 LINE-SIZE 400.

INCLUDE zemmpm10e_stltop.       "Data Declaration
INCLUDE zemmpm10e_stlf01.       "Perform Library.

*--------- Report Transactin Execution --------------------------------*
INITIALIZATION.
  p_cdate = sy-datum.
  p_ctime = sy-uzeit.

*/ Begin of for test.
*  DATA: lv_year(4).
*  DATA: lv_month(2).
*  lv_year = sy-datum(4).
*  lv_month = sy-datum+4(2).
*  CONCATENATE lv_year lv_month '*' INTO p_rp06.
*/ End of for test.

*/ Begin of Added by Hakchin(20040127)
*  DATA: lv_year(4).
*  DATA: lv_month(2).
*  lv_year = sy-datum(4).
*  lv_month = sy-datum+4(2).
*  CONCATENATE lv_year lv_month '*' INTO p_rp06.
*/ End of Added by Hakchin(20040127)



*/ Begin of Commented by Hakchin(20040127)
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_rp06.
*  PERFORM get_it_rp06.
*  PERFORM f4_rp06                                           "RP06 Data
*                      TABLES it_rp06
*                             it_ddshretval
*                      USING  'RP06'   "Itab FIELD
*                             SY-REPID
*                             sy-dynnr "Screen number
*                            'P_RP06'  "Screen field
*                             0.       "Step loop line
*/ End of Commented by Hakchin(20040127)

AT SELECTION-SCREEN.
*/ Begin of Commented by Hakchin(20040127)
*  CLEAR: w_where_condition, it_where_condition.
*
*  CONCATENATE 'RP06 LIKE ''' p_rp06(10) '%' ''''
*    INTO w_where_condition.
*  APPEND w_where_condition TO it_where_condition.
*
*  DATA: lv_rp06 LIKE ztpp_dvrt1-rp06.
*  SELECT SINGLE rp06 INTO lv_rp06
*    FROM ztpp_dvrt1
*    WHERE (it_where_condition).
*
*  IF sy-subrc <> 0.
*    MESSAGE e999(zmmm) WITH 'Input Error with RP06'(002).
*  ENDIF.
*/ End of Commented by Hakchin(20040127)

START-OF-SELECTION.
*1. Prepare Data
*2. Create Transfer Order
*3. Change T/O Header Data

* Get Screen Date
  PERFORM get_w_screen_date.
* Get Screen Time
  PERFORM get_w_screen_time.

* Get Current Date & Time
  PERFORM get_w_current_date_time.

** Get Current Time
*  PERFORM get_w_current_time.
** Get Current Date
*  PERFORM get_w_current_date.


* Get data supply to line
  PERFORM get_data_from_table.            "get data

  IF it_toline IS INITIAL.
    MESSAGE s999(zmmm) WITH 'There is no data!'(001).
    EXIT.
  ENDIF.

  LOOP AT it_toline ASSIGNING <fs_toline>.

** Get Feeding Time
*    PERFORM get_time_from_minutes
*                     USING    <fs_toline>-feed_cycle
*                     CHANGING w_cal_time.

*/ Begin OF TO GET Start Date(Feeding Date) and Start Time
    PERFORM time_calculation USING    w_screen_date
                                      w_screen_time
                                      <fs_toline>-ztime
*                                      w_dummy_minutes
*                             CHANGING w_dummy_date
                             CHANGING <fs_toline>-sdate
                                      <fs_toline>-feeding_time.

    IF <fs_toline>-feed_cycle = '60' OR
       <fs_toline>-feed_cycle = '060'.
      "Ignore.
    ELSEIF <fs_toline>-feed_cycle = '120'.
      CLEAR: w_odd_even.
      w_odd_even = <fs_toline>-feeding_time(2) MOD 2.  "Start Time
      IF w_odd_even <> 1.  "Not Odd = Even
        <fs_toline>-feeding_time = <fs_toline>-feeding_time + c_onehour.
      ENDIF.
    ENDIF.
*/ End OF TO GET Start Date and Start Time

**** 1.5. For Stock Level Check
* Get Available stock(verme) & min. storage bin qty(lpmin).
    PERFORM get_verme_lpmin.

**** 1.6. For Open TO Check (LTAP-VSOLA)(/nLT23)
    PERFORM get_open_to.

**** 1.7. For Backflush Error Qty Check (/nCOGI)
    PERFORM get_bf_error_qty.

**** 1.8. Rounding Quantity Check
    PERFORM get_rdmng.

**** 1.9. Calculate Target Quantity from the aboves
    PERFORM get_tqty.

**** 2. Create TO
* App Doc No
    PERFORM number_get_next USING    c_nro_nr_09     "NRO Interval
                                     w_nro_object    "NRO Object
                            CHANGING w_zdocno.     "App Doc No
    COMMIT WORK.

**** Begin of Create TO (/nLT01)
* Get Source Storage type/bin
    PERFORM get_sorce_storage_type_bin.

* Get Destination Storage type/bin
    PERFORM get_des_storage_type_bin.

* BDC Processing of /nLT01
    PERFORM bdc_processing_lt01 TABLES   it_bdcmsgcoll
                                USING    w_zdocno
                                CHANGING w_subrc.

**** End of Create TO (/nLT01)
    IF w_subrc = 0.

* 3. Begin of Change TO Header (/nLT1A)
      CLEAR: wa_bdcmsgcoll.
      READ TABLE it_bdcmsgcoll INTO wa_bdcmsgcoll
                               WITH KEY msgtyp = 'S'.
      CHECK sy-subrc = 0.
      PERFORM bdc_processing_lta1
                       TABLES   it_bdcmsgcoll
                       USING    w_zdocno
                                wa_bdcmsgcoll-msgv1  "TO number
                       CHANGING w_subrc.
* End of Change TO Header (/nLT1A)
    ENDIF.

**** Write List
    PERFORM write_list.
  ENDLOOP.

*---------- List Procession Events ------------------------------------*
TOP-OF-PAGE.
  PERFORM make_col_heading.
