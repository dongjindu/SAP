FUNCTION z_fpp_get_supply_to_line_rp.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             REFERENCE(I_DATE) TYPE  D DEFAULT SY-DATUM
*"             REFERENCE(I_TIME) TYPE  T DEFAULT SY-UZEIT
*"             REFERENCE(I_RP_STATUS) TYPE  C OPTIONAL
*"       TABLES
*"              T_SUPPLY_INFO STRUCTURE  ZSPP_VIN_INFO_FOR_STL
*"       EXCEPTIONS
*"              NO_DATA_FOUNDED
*"              LINE_INFO_DOES_NOT_EXIST
*"              ETC_EXCEPTION
*"----------------------------------------------------------------------

  DATA: lt_condition LIKE zsca_vin_condition2 OCCURS 0 WITH HEADER LINE,
        lt_value     LIKE zsca_char_value     OCCURS 0 WITH HEADER LINE,
        lt_vin   LIKE zsca_vehicle_char_value OCCURS 0 WITH HEADER LINE,
        lt_line_info LIKE zvpp_line_info      OCCURS 0 WITH HEADER LINE.

  DATA: lw_date_f LIKE ausp-atwrt,
        lw_date_t LIKE ausp-atwrt.

*----- Set Vin Master Field
  MOVE: 'P_PLAN_ORDER' TO lt_value-atnam.
  APPEND lt_value.
  MOVE: 'P_RP06_ACTUAL_DATE' TO lt_value-atnam.
  APPEND lt_value.

*----- Set Condition
  DATA: lw_datef LIKE sy-datum,                "Date from
        lw_datet LIKE sy-datum,                "Date to
        lw_timfr LIKE sy-uzeit,                "To time
        lw_timto LIKE sy-uzeit.                "To time

  IF i_time < '010000'.
    lw_datef = i_date - 1.
    lw_datet = i_date.
    lw_timfr = '230001'.
    lw_timto = '000000'.
  ELSE.
    lw_datef = lw_datet = i_date.
    lw_timfr = i_time - 3600.
    CONCATENATE lw_timfr(2) '0001' INTO lw_timfr.
    lw_timto = lw_timfr + 3600 - 1.
  ENDIF.

  CONCATENATE: lw_datef lw_timfr INTO lw_date_f,
               lw_datet lw_timto INTO lw_date_t.

  MOVE: 'P_RP_STATUS' TO lt_condition-atnam,
        i_rp_status   TO lt_condition-atwrt_s.

  APPEND lt_condition.

*----- Get Vehicle Master Info
  CALL FUNCTION 'Z_FCA_GET_VEHICLE_MASTER'
       EXPORTING
            i_atnam                       = 'P_RP06_ACTUAL_DATE'
            i_atwrt_s                     = lw_date_f
            i_atwrt_e                     = lw_date_t
       TABLES
            t_condition                   = lt_condition
            t_value                       = lt_value
            t_vehicle                     = lt_vin
       EXCEPTIONS
            date_overflow                 = 1
            invalid_date                  = 2
            condition_does_not_exist      = 3
            characteristic_does_not_exist = 4
            OTHERS                        = 5.

  IF sy-subrc <> 0.
    RAISE no_data_founded.
  ENDIF.

*----- Set output table
  LOOP AT lt_vin.
    AT NEW objek.
      MOVE: lt_vin-objek(3) TO t_supply_info-p_model,
            lt_vin-objek+3  TO t_supply_info-p_body_serial.
      APPEND t_supply_info.
    ENDAT.

    READ TABLE t_supply_info WITH KEY p_model = lt_vin-objek(3)
                                      p_body_serial = lt_vin-objek+3.
    IF sy-subrc NE 0.
      RAISE etc_exception.
    ENDIF.

    CASE lt_vin-atnam.
      WHEN 'P_PLAN_ORDER'.
        MOVE: lt_vin-atwrt TO t_supply_info-plnum.
      WHEN 'P_RP06_ACTUAL_DATE'.
        MOVE: lt_vin-atwrt TO t_supply_info-p_rp06.
    ENDCASE.

    MODIFY t_supply_info INDEX sy-tabix.
  ENDLOOP.

*----- Get Reservation No
  LOOP AT t_supply_info.
    SELECT SINGLE rsnum INTO t_supply_info-rsnum
      FROM plaf
     WHERE plnum = t_supply_info-plnum.
    IF sy-subrc EQ 0.
      MODIFY t_supply_info.
    ENDIF.
  ENDLOOP.

*--- Sort by rsnum
  SORT t_supply_info BY rsnum.

**----- Sort by plnum
*  SORT t_supply_info BY plnum.
ENDFUNCTION.
