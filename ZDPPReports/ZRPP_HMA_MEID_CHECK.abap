************************************************************************
* Program Name      : RPP_HMA_MEID_CHECK
* Author            : Daniel
* Creation Date     : 08/31/2011
* Specifications By :
* Development Request No :
* Addl Documentation:
* Description       : Check MEID interface when vehicle passes T04
* Modification Logs
* Date       Developer    RequestNo    Description
*
*********************************************************************
REPORT zrpp_hma_meid_check MESSAGE-ID zmpp.
TABLES: ztppvr.

DATA: BEGIN OF it_veh OCCURS 0,
        p_model       LIKE ztppvr-p_model,
        p_body_serial LIKE ztppvr-p_body_serial,
        body          LIKE ausp-objek,
      END   OF it_veh.

DATA: BEGIN OF it_meid OCCURS 0,
        vin         LIKE ztppvr-p_vin,
        body        LIKE ausp-objek,
        pout_sdate  LIKE sy-datum,
        rp06_serial LIKE ztppvr-k04ser,
        meid        LIKE ztppvm-p_airbag_no10,
        meid_sent   LIKE ztpp_hma_meid-meid,
        tp_status   LIKE ausp-atwrt,
        worder      LIKE ausp-objek,
        219_108     LIKE ausp-atwrt,
      END   OF it_meid.

DATA: it_body LIKE TABLE OF solisti1 WITH HEADER LINE.

DATA: w_st_date       LIKE sy-datum,
      w_end_date      LIKE sy-datum,
      w_end_time      LIKE sy-uzeit,
      w_yesterday     LIKE sy-datum,
      w_rp_status     LIKE ztpp_vm-rp_cstatus,
      w_title         TYPE p15_text150,
      w_pout_atinn    LIKE cabn-atinn,
      w_serial_atinn  LIKE cabn-atinn,
      w_vin_atinn     LIKE cabn-atinn,
      w_meid_atinn    LIKE cabn-atinn,
      w_status_atinn  LIKE cabn-atinn,
      w_worder_atinn  LIKE cabn-atinn,
      w_219_108_atinn LIKE cabn-atinn,
      w_rp_atinn      LIKE cabn-atinn.

CONSTANTS: c_duration TYPE i VALUE -1,
           c_werks    TYPE werks_d VALUE 'P001',
           c_arbpl    TYPE arbpl   VALUE 'T'.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-h01.
PARAMETERS:     p_status LIKE ztppvr-p_status DEFAULT 'T04' OBLIGATORY.
SELECT-OPTIONS: s_model  FOR ztppvr-p_model.
SELECT-OPTIONS: s_dest   FOR ztppvr-p_dest_code.
PARAMETERS:     p_day(3) TYPE n DEFAULT 3 OBLIGATORY.
SELECTION-SCREEN SKIP.
PARAMETERS: p_rver  LIKE somlreci1-receiver OBLIGATORY
            DEFAULT 'PP_HMA_MEID'.
SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN.
  CHECK sy-ucomm EQ 'ONLI'.
  PERFORM check_user_input.


START-OF-SELECTION.
  PERFORM read_target_vehicles.

*&---------------------------------------------------------------------*
*&      Form  READ_TARGET_VEHICLES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_target_vehicles .

  PERFORM set_date_time.
  PERFORM get_target_vehicle.
  PERFORM read_vehicle_master.
  PERFORM read_meid_flag.
  PERFORM read_meid_sending_log.
  PERFORM check_meid.
  PERFORM send_email.
ENDFORM.                    " READ_TARGET_VEHICLES
*&---------------------------------------------------------------------*
*&      Form  GET_END_TIME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_date_time.
  DATA: l_curr_time LIKE sy-uzeit,
        l_wktime    TYPE i.

  CLEAR: w_st_date, w_end_date, w_end_time.

  l_wktime = p_day * 60 * 24.

  CALL FUNCTION 'Z_PP_GET_WKTIME'
    EXPORTING
      c_date                   = sy-datum
      opcode                   = '-'
      wktime                   = l_wktime
      werks                    = c_werks
      arbpl                    = c_arbpl
    IMPORTING
      t_date                   = w_st_date
    EXCEPTIONS
      error_operation          = 1
      cannot_read_working_time = 2
      OTHERS                   = 3.
  IF sy-subrc <> 0.
    MESSAGE e000 WITH 'Working time is not available.'.
  ENDIF.

  IF w_st_date IS INITIAL.
    w_st_date = sy-datum - p_day.
  ENDIF.

  CONCATENATE sy-uzeit+0(2) '59' '59' INTO l_curr_time.

  CALL FUNCTION 'END_TIME_DETERMINE'
    EXPORTING
      duration                   = c_duration
      unit                       = 'H'
*     FACTORY_CALENDAR           =
    IMPORTING
      end_date                   = w_end_date
      end_time                   = w_end_time
    CHANGING
      start_date                 = sy-datum
      start_time                 = l_curr_time
    EXCEPTIONS
      factory_calendar_not_found = 1
      date_out_of_calendar_range = 2
      date_not_valid             = 3
      unit_conversion_error      = 4
      si_unit_missing            = 5
      parameters_no_valid        = 6
      OTHERS                     = 7.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

  IF w_st_date < w_end_date.
    w_yesterday = w_end_date - 1.
  ELSE.
    w_yesterday = w_st_date.
  ENDIF.
ENDFORM.                    " GET_END_TIME
*&---------------------------------------------------------------------*
*&      Form  GET_TARGET_VEHICLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_target_vehicle .
  CLEAR: it_veh, it_veh[].

  IF w_yesterday NE w_end_date.
    SELECT p_model p_body_serial
      INTO TABLE it_veh
      FROM ztppvr AS a
     WHERE zbdat       BETWEEN w_st_date AND w_yesterday
       AND zresult     EQ 'S'
       AND p_model     IN s_model
       AND p_status    EQ p_status
       AND p_dest_code IN s_dest.
  ENDIF.

  SELECT p_model p_body_serial
    APPENDING TABLE it_veh
    FROM ztppvr AS a
   WHERE zbdat       EQ w_end_date
     AND zbtim       <= w_end_time
     AND zresult     EQ 'S'
     AND p_model     IN s_model
     AND p_status    EQ p_status
     AND p_dest_code IN s_dest.

  SELECT a~p_model a~p_body_serial
    APPENDING TABLE it_veh
    FROM ztppvm AS a INNER JOIN ztsd_um AS b
                        ON b~model_code = a~p_model
                       AND b~body_no    = a~p_body_serial
    WHERE a~zedat   BETWEEN w_st_date AND w_yesterday
      AND a~zresult EQ 'S'
      AND a~p_model IN s_model
      AND b~status  IN ('','F')
      AND b~wo_nation IN s_dest.

  SELECT a~p_model a~p_body_serial
    APPENDING TABLE it_veh
    FROM ztppvm AS a INNER JOIN ztsd_um AS b
                        ON b~model_code = a~p_model
                       AND b~body_no    = a~p_body_serial
    WHERE a~zedat   EQ w_end_date
      AND a~zresult EQ 'S'
      AND a~zetim   <= w_end_time
      AND a~p_model IN s_model
      AND b~status  IN ('','F')
      AND b~wo_nation IN s_dest.


  SORT it_veh BY p_model p_body_serial.

  DELETE ADJACENT DUPLICATES FROM it_veh
         COMPARING p_model p_body_serial.

  LOOP AT it_veh.
    CONCATENATE it_veh-p_model it_veh-p_body_serial
           INTO it_veh-body.
    MODIFY it_veh.
  ENDLOOP.
ENDFORM.                    " GET_TARGET_VEHICLE
*&---------------------------------------------------------------------*
*&      Form  READ_VEHICLE_MASTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_vehicle_master .
  DATA: BEGIN OF lt_ausp OCCURS 0,
          body        LIKE ausp-objek,
          pout_sdate  LIKE ausp-atflv,
          rp06_serial LIKE ausp-atwrt,
          vin         LIKE ausp-atwrt,
          meid        LIKE ausp-atwrt,
          tp_status   LIKE ausp-atwrt,
          worder      LIKE ausp-atwrt,
        END   OF lt_ausp.

  DATA: l_datum(8) TYPE n.

  CHECK it_veh[] IS NOT INITIAL.

  PERFORM get_characteristic.

  SELECT a~objek AS body        a~atflv AS pout_sdate
         b~atwrt AS rp06_serial c~atwrt AS vin
         d~atwrt AS meid        e~atwrt AS tp_status
         f~atwrt AS worder
    INTO CORRESPONDING FIELDS OF TABLE lt_ausp
    FROM ausp AS a LEFT OUTER JOIN ausp AS b
                     ON b~objek = a~objek
                    AND b~atinn = w_serial_atinn
                    AND b~mafid = a~mafid
                    AND b~klart = a~klart
                   LEFT OUTER JOIN ausp AS c
                     ON c~objek = a~objek
                    AND c~atinn = w_vin_atinn
                    AND c~mafid = a~mafid
                    AND c~klart = a~klart
                   LEFT OUTER JOIN ausp AS d
                     ON d~objek = a~objek
                    AND d~atinn = w_meid_atinn
                    AND d~mafid = a~mafid
                    AND d~klart = a~klart
                   LEFT OUTER JOIN ausp AS e
                     ON e~objek = a~objek
                    AND e~atinn = w_status_atinn
                    AND e~mafid = a~mafid
                    AND e~klart = a~klart
                   LEFT OUTER JOIN ausp AS f
                     ON f~objek = a~objek
                    AND f~atinn = w_worder_atinn
                    AND f~mafid = a~mafid
                    AND f~klart = a~klart
     FOR ALL ENTRIES IN it_veh
   WHERE a~objek = it_veh-body
     AND a~atinn = w_pout_atinn
     AND a~klart = '002'
     AND EXISTS ( SELECT * FROM ausp AS g
                         WHERE g~objek = a~objek
                           AND g~atinn = w_rp_atinn
                           AND g~mafid = a~mafid
                           AND g~klart = a~klart
                           AND g~atwrt >= w_rp_status ).

  LOOP AT lt_ausp.
    MOVE-CORRESPONDING lt_ausp TO it_meid.
    it_meid-pout_sdate = l_datum = lt_ausp-pout_sdate.
    APPEND it_meid.
  ENDLOOP.
ENDFORM.                    " READ_VEHICLE_MASTER
*&---------------------------------------------------------------------*
*&      Form  GET_CHARACTERISTIC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_characteristic .
  SELECT SINGLE atinn INTO w_pout_atinn
    FROM cabn WHERE atnam = 'P_RP06_SHOP_DATE'.

  SELECT SINGLE atinn INTO w_serial_atinn
    FROM cabn WHERE atnam = 'P_RP06_SERIAL'.

  SELECT SINGLE atinn INTO w_vin_atinn
    FROM cabn WHERE atnam = 'P_VIN'.

  SELECT SINGLE atinn INTO w_meid_atinn
    FROM cabn WHERE atnam = 'P_AIRBAG_NO11'.

  SELECT SINGLE atinn INTO w_status_atinn
    FROM cabn WHERE atnam = 'P_STATUS'.

  SELECT SINGLE atinn INTO w_worder_atinn
    FROM cabn WHERE atnam = 'P_WORK_ORDER'.

  SELECT SINGLE atinn INTO w_rp_atinn
    FROM cabn WHERE atnam = 'P_RP_STATUS'.
ENDFORM.                    " GET_CHARACTERISTIC
*&---------------------------------------------------------------------*
*&      Form  READ_MEID_FLAG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_meid_flag .
  DATA: lt_ausp LIKE ausp OCCURS 0 WITH HEADER LINE.

  CHECK it_meid[] IS NOT INITIAL.

  SELECT SINGLE atinn INTO w_219_108_atinn
    FROM cabn WHERE atnam = 'P_219_108'.

  SELECT objek atwrt
    INTO CORRESPONDING FIELDS OF TABLE lt_ausp
    FROM ausp
     FOR ALL ENTRIES IN it_meid
   WHERE objek = it_meid-worder
     AND atinn = w_219_108_atinn
     AND klart = '001'.

  SORT lt_ausp BY objek.

  LOOP AT it_meid.
    READ TABLE lt_ausp WITH KEY objek = it_meid-worder
                       BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_meid-219_108 = lt_ausp-atwrt.
      MODIFY it_meid.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " READ_MEID_FLAG
*&---------------------------------------------------------------------*
*&      Form  CHECK_MEID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_meid .
  LOOP AT it_meid.
    CASE it_meid-219_108.
      WHEN 'T'.         " MEID equipped.
        IF it_meid-meid IS INITIAL.
          PERFORM write_message USING text-m02.
          CONTINUE.
        ENDIF.

        IF it_meid-meid_sent IS INITIAL.
          PERFORM write_message USING text-m04.
          CONTINUE.
        ENDIF.
      WHEN OTHERS.
        IF it_meid-meid IS NOT INITIAL.
          PERFORM write_message USING text-m05.
          CONTINUE.
        ENDIF.

        IF it_meid-meid_sent IS NOT INITIAL.
          PERFORM write_message USING text-m06.
          CONTINUE.
        ENDIF.
    ENDCASE.

    IF it_meid-meid NE it_meid-meid_sent.
      PERFORM write_message USING text-m03.
      CONTINUE.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " CHECK_MEID
*&---------------------------------------------------------------------*
*&      Form  READ_MEID_SENDING_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_meid_sending_log .
  DATA: lt_meid_sent LIKE ztpp_hma_meid OCCURS 0 WITH HEADER LINE.

  check it_meid[] is not initial.

  SELECT vin meid
    INTO CORRESPONDING FIELDS OF TABLE lt_meid_sent
    FROM ztpp_hma_meid
     FOR ALL ENTRIES IN it_meid
   WHERE vin     = it_meid-vin
     AND zresult = 'S'.

  CHECK sy-subrc EQ 0.

  SORT lt_meid_sent BY vin.

  LOOP AT it_meid.
    READ TABLE lt_meid_sent WITH KEY vin = it_meid-vin
                            BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_meid-meid_sent = lt_meid_sent-meid.
      MODIFY it_meid.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " READ_MEID_SENDING_LOG
*&---------------------------------------------------------------------*
*&      Form  WRITE_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_message USING p_msg.
  CLEAR: it_body.

  WRITE: it_meid-vin         TO it_body(18),
         it_meid-pout_sdate  TO it_body+18(11),
         it_meid-rp06_serial TO it_body+29(7),
         it_meid-tp_status   TO it_body+36(8),
         it_meid-219_108     TO it_body+44(9),
         it_meid-meid        TO it_body+53(15),
         it_meid-meid_sent   TO it_body+68(15),
         p_msg               TO it_body+83(40).

  APPEND it_body.
ENDFORM.                    " WRITE_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  SEND_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_email .
  CHECK it_body[] IS NOT INITIAL.

  PERFORM make_header.

  CONCATENATE 'MEID Checking for Vehicle Passed' p_status
         INTO w_title SEPARATED BY space.

  CALL FUNCTION 'ZCAF_SEND_EMAIL'
    EXPORTING
      p_subject  = w_title
      p_rec_type = 'C'
      p_receiver = p_rver
    TABLES
      pt_body    = it_body.
ENDFORM.                    " SEND_EMAIL
*&---------------------------------------------------------------------*
*&      Form  MAKE_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_header .
  CLEAR: it_body.

  WRITE: 'VIN'               TO it_body(18),
         'PBS Out'           TO it_body+18(11),
         'Serial'            TO it_body+29(7),
         'Curr RP'           TO it_body+36(8),
         '219_108'           TO it_body+44(9),
         'V/M MEID'          TO it_body+53(15),
         'Sent MEID'         TO it_body+68(15),
         'Message'           TO it_body+83(40).

  INSERT it_body INDEX 1.

  CLEAR: it_body.

  WRITE: text-m99 TO it_body(123).

  INSERT it_body INDEX 2.
ENDFORM.                    " MAKE_HEADER
*&---------------------------------------------------------------------*
*&      Form  CHECK_USER_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_user_input .
  SELECT SINGLE rp_point INTO w_rp_status
    FROM ztpp_status
   WHERE id = p_status.
  IF sy-subrc NE 0.
    MESSAGE e000 WITH 'RP status is not maintained'.
  ENDIF.
ENDFORM.                    " CHECK_USER_INPUT
