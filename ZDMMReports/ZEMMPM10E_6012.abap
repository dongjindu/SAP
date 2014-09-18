************************************************************************
* Program Name      : ZEMMPM10E_6012
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
REPORT  zemmpm10e_6012
                 MESSAGE-ID zmmm
                 NO STANDARD PAGE HEADING
                 LINE-SIZE 400.

INCLUDE zemmpm10e_6012top.       "Data Declaration
INCLUDE zemmpm10e_6012f01.       "Perform Library.

*--------- Report Transactin Execution --------------------------------*
START-OF-SELECTION.
*1. Prepare Data
*2. Create Transfer Order
*3. Change T/O Header Data

* Get data supply to line
  PERFORM get_data_from_table.            "get data

  IF it_toline IS INITIAL.
    MESSAGE s999(zmmm) WITH 'There is no data!'(001).
    EXIT.
  ENDIF.

* Get Present Date
  PERFORM get_w_present_date.
* Get Present Time
  PERFORM get_w_present_time.

  LOOP AT it_toline ASSIGNING <fs_toline>.
* Get Feeding Time
    PERFORM get_time_from_minutes
                     USING    <fs_toline>-feed_cycle
                     CHANGING w_cal_time.

* Get Feeding Date&Time
    PERFORM time_calculation USING    w_present_date
                                      w_present_time
                                      <fs_toline>-feed_cycle
                             CHANGING <fs_toline>-sdate
                                      <fs_toline>-feeding_time.


*    <fs_toline>-sdate        = w_present_date.
*    <fs_toline>-feeding_time = w_present_time + w_cal_time.

    IF <fs_toline>-feed_cycle <> '60' OR
       <fs_toline>-feed_cycle <> '060'.
      CLEAR: w_odd_even.
      w_odd_even = <fs_toline>-feeding_time(2) MOD 2.
      IF w_odd_even <> 1.  "Even
        <fs_toline>-feeding_time = <fs_toline>-feeding_time + c_onehour.
      ENDIF.
    ENDIF.

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
