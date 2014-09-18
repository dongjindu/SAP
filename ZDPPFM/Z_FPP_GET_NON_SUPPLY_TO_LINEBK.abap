FUNCTION Z_FPP_GET_NON_SUPPLY_TO_LINEBK.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             REFERENCE(I_WERKS) LIKE  T001W-WERKS
*"             REFERENCE(I_ATWRT) LIKE  AUSP-ATWRT
*"             REFERENCE(I_DATE) TYPE  D DEFAULT SY-DATUM
*"             REFERENCE(I_ARBPL) LIKE  CRHD-ARBPL OPTIONAL
*"       TABLES
*"              T_SUPPLY_INFO STRUCTURE  ZSPP_VIN_INFO_FOR_NSTL
*"       EXCEPTIONS
*"              NO_DATA_FOUNDED
*"              LINE_INFO_DOES_NOT_EXIST
*"              ETC_EXCEPTION
*"              UPH_INFO_DOES_NOT_EXIST
*"----------------------------------------------------------------------

*  DATA:lt_condition LIKE zsca_vin_condition2 OCCURS 0 WITH HEADER LINE,
*       lt_value     LIKE zsca_char_value     OCCURS 0 WITH HEADER LINE,
*       lt_vin   LIKE zsca_vehicle_char_value OCCURS 0 WITH HEADER LINE,
*       lt_line_info LIKE zvpp_line_info      OCCURS 0 WITH HEADER LINE.
*
  CLEAR: w_vehicle_count.

  CLEAR: rg_arbpl,         rg_arbpl[],
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
  MOVE: i_werks TO w_werks,
        i_atwrt TO w_rp_point,               " Reporting Point
        i_date  TO w_date,
        i_arbpl TO w_arbpl.

*----- Read Line Capacity
  PERFORM read_line_capacity.

*----- Read rp Work Time
  PERFORM read_rp_work_time.

  READ TABLE it_day_work_time INDEX 1.
  CHECK sy-subrc EQ 0.

*----- Read Vehicle Master
  PERFORM read_vehicle_master.

*----- Set output table
*  PERFORM set_supply_info.
*
*----- Set RP06 time
  PERFORM set_rp06_time.

  t_supply_info[] = it_supply_info[].

  SORT t_supply_info BY p_rp06 p_model p_body_serial.
ENDFUNCTION.
