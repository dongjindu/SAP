FUNCTION z_fpp_get_non_supply_to_line.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(I_WERKS) LIKE  T001W-WERKS
*"     REFERENCE(I_ATWRT) LIKE  AUSP-ATWRT
*"     REFERENCE(I_DATE) TYPE  D DEFAULT SY-DATUM
*"     REFERENCE(I_TIME) LIKE  SY-UZEIT DEFAULT '000000'
*"     REFERENCE(I_ARBPL) LIKE  CRHD-ARBPL OPTIONAL
*"     REFERENCE(I_TEST) TYPE  C DEFAULT 'X'
*"     REFERENCE(I_DAY) TYPE  I DEFAULT 3
*"  TABLES
*"      T_SUPPLY_INFO STRUCTURE  ZSPP_VIN_INFO_FOR_NSTL
*"      T_MITU STRUCTURE  ZSPP_VIN_INFO_FOR_NSTL OPTIONAL
*"  EXCEPTIONS
*"      NO_DATA_FOUNDED
*"      LINE_INFO_DOES_NOT_EXIST
*"      ETC_EXCEPTION
*"      UPH_INFO_DOES_NOT_EXIST
*"----------------------------------------------------------------------
*& Modification Logs
*"----------------------------------------------------------------------
*& Date            Developer      RequestNo       Description
*& 09/26/2005      Shiva          UD1K917724       select only test &
*&                                                 production car.
*"----------------------------------------------------------------------

  CLEAR: w_werks, w_rp_point, w_date, w_arbpl,w_vehicle_count.

  CLEAR: it_supply_info, it_supply_info[],
         rg_arbpl,         rg_arbpl[],
         rg_paplan,        rg_paplan[],
         it_day_work_time, it_day_work_time[],
         it_break_time,    it_break_time[],
         it_vin_master,    it_vin_master[],
         it_supply_info,   it_supply_info[],
         it_rsnum,         it_rsnum[],
         it_maxcapa,       it_maxcapa[],
         it_bt,            it_bt[].
  CLEAR: it_work_time, it_work_time[].

*----- Move parameter to global variable
  MOVE: i_werks    TO w_werks,
        i_atwrt    TO w_rp_point,               " Reporting Point
        i_date     TO w_date,
        i_time     TO w_time,
        i_arbpl    TO w_arbpl,
        i_test     TO w_test,
        i_day      TO w_day.

*----- Read work time
  PERFORM read_working_time.

*----- Read Line Capacity
  PERFORM read_line_capacity.

*----- Calculate Total Vehicle Count
  PERFORM calculate_vin_count.

*----- Calulate vehicle count of working time
  PERFORM calculate_vin_count_of_working.

*----- Read Vehicle Master
  PERFORM read_vehicle_master.

*----- Set RP Point Time
  PERFORM set_rp_point_time.


  t_supply_info[] = it_supply_info[].
  t_mitu[]        = it_mitu[].

  SORT t_supply_info BY p_rp06 p_model p_body_serial.
ENDFUNCTION.
