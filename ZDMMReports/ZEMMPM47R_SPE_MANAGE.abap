************************************************************************
* Program Name      : ZEMMPM47R_SPE_MANAGE
* Author            : Byung-sung, Bae
* Creation Date     : 2004.04.26.
* Specifications By : Byung-sung, Bae
* Pattern           : Report 1-1
* Development Request No : UD1K909896
* Addl Documentation:
* Description       : Master Inspection Characteristic Uploading
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT zemmpm47r_spe_manage NO STANDARD PAGE HEADING
                            LINE-SIZE  126
                            LINE-COUNT  65.
INCLUDE: <icon>.
TYPE-POOLS : slis.
TABLES: mara,
        ztmm_analy,
        ztmm_spe,
        t024.

*----- Internal tables
DATA: BEGIN OF it_price OCCURS 0,
        check,                              "Check Field
        matnr      LIKE mara-matnr,         "Material
        meins      LIKE mara-meins,         "Master UoM
        profl      LIKE mara-profl,         "KD,LD Separator
        rduty      LIKE konp-kbetr,         "Import tax rate
        rfrgt      LIKE konp-kbetr,         "Import freight
        rcost      LIKE konp-kbetr,         "Import costs rate
        maktx      LIKE makt-maktx,         "Description
        werks      LIKE ztmm_analy-werks,   "Plant
        period     LIKE ztmm_analy-period,  "Period
        ekorg      LIKE ztmm_analy-ekorg,   "Purchase Org.
        ekgrp      LIKE t024-ekgrp,         "Purchase Grp.
        base_d     LIKE ztmm_analy-base_d,  "Base date
        datab      LIKE konh-datab,         "Valid date
        type(2),                            "KD, LP
        flag,                               "Status flag
        stprs      LIKE mbew-stprs,         "Std. Price
        zplp1      LIKE mbew-zplp1,         "Planned Price(quarter)
        zplp2      LIKE mbew-zplp2,         "Module Price
        zplp3      LIKE mbew-zplp3,         "Planned Price(year)
        peinh      LIKE mbew-peinh,         "Price Unit
        i_flag,                             "Info exist flag
        i_kzust    LIKE ztmm_analy-kzust,   "Info Reason code
        i_valid_d  LIKE ztmm_analy-valid_d, "Info Valid date
        i_wrbtr    LIKE ztmm_analy-wrbtr,   "Info Foreign Pice
        i_wduty    LIKE ztmm_analy-wrbtr,   "Info duty of F.Curr
        i_wfrgt    LIKE ztmm_analy-wrbtr,   "Info freight of F.Curr
        i_wcost    LIKE ztmm_analy-wrbtr,   "Info cost of F.Curr
        i_dmbtr    LIKE ztmm_analy-wrbtr,   "Info Local Price
        i_dduty    LIKE ztmm_analy-wrbtr,   "Info duty of L.Curr
        i_dfrgt    LIKE ztmm_analy-wrbtr,   "Info freight of L.Curr
        i_dcost    LIKE ztmm_analy-wrbtr,   "Info cost of L.Curr
        i_wramt    LIKE ztmm_analy-wrbtr,   "Info Foreign Amount
        i_dmamt    LIKE ztmm_analy-wrbtr,   "Info Local Amount
        i_peinh    LIKE ztmm_analy-peinh,   "Info price unit
        i_waers    LIKE ztmm_analy-waers,   "Info Currency
        i_wkurs    LIKE ztmm_analy-wkurs,   "Info exchange rate
        i_lifnr    LIKE ztmm_analy-lifnr,   "Info vendor
        i_name1    LIKE lfa1-name1,         "Info vendor name
        e_flag,                             "Error exist flag
        e_kzust    LIKE ztmm_analy-kzust,   "Error Reason code
        e_valid_d  LIKE ztmm_analy-valid_d, "Error Valid date
        e_wrbtr    LIKE ztmm_analy-wrbtr,   "Error Foreign Pice
        e_wduty    LIKE ztmm_analy-wrbtr,   "Error duty of F.Curr
        e_wfrgt    LIKE ztmm_analy-wrbtr,   "Error freight of F.Curr
        e_wcost    LIKE ztmm_analy-wrbtr,   "Error cost of F.Curr
        e_dmbtr    LIKE ztmm_analy-wrbtr,   "Error Local Price
        e_dduty    LIKE ztmm_analy-wrbtr,   "Error duty of L.Curr
        e_dfrgt    LIKE ztmm_analy-wrbtr,   "Error freight of L.Curr
        e_dcost    LIKE ztmm_analy-wrbtr,   "Error cost of L.Curr
        e_wramt    LIKE ztmm_analy-wrbtr,   "Error Foreign Amount
        e_dmamt    LIKE ztmm_analy-wrbtr,   "Error Local Amount
        e_peinh    LIKE ztmm_analy-peinh,   "Error price unit
        e_waers    LIKE ztmm_analy-waers,   "Error Currency
        e_wkurs    LIKE ztmm_analy-wkurs,   "Error exchange rate
        e_lifnr    LIKE ztmm_analy-lifnr,   "Error vendor
        e_name1    LIKE lfa1-name1,         "Error vendor name
        f_flag,                             "Finish exist flag
        f_kzust    LIKE ztmm_analy-kzust,   "Finish Reason code
        f_valid_d  LIKE ztmm_analy-valid_d, "Finish Valid date
        f_wrbtr    LIKE ztmm_analy-wrbtr,   "Finish Foreign Pice
        f_wduty    LIKE ztmm_analy-wrbtr,   "Finish duty of F.Curr
        f_wfrgt    LIKE ztmm_analy-wrbtr,   "Finish freight of F.Curr
        f_wcost    LIKE ztmm_analy-wrbtr,   "Finish cost of F.Curr
        f_dmbtr    LIKE ztmm_analy-wrbtr,   "Finish Local Price
        f_dduty    LIKE ztmm_analy-wrbtr,   "Finish duty of L.Curr
        f_dfrgt    LIKE ztmm_analy-wrbtr,   "Finish freight of L.Curr
        f_dcost    LIKE ztmm_analy-wrbtr,   "Finish cost of L.Curr
        f_wramt    LIKE ztmm_analy-wrbtr,   "Finish Foreign Amount
        f_dmamt    LIKE ztmm_analy-wrbtr,   "Finish Local Amount
        f_peinh    LIKE ztmm_analy-peinh,   "Finish price unit
        f_waers    LIKE ztmm_analy-waers,   "Finish Currency
        f_wkurs    LIKE ztmm_analy-wkurs,   "Finish exchange rate
        f_lifnr    LIKE ztmm_analy-lifnr,   "Finish vendor
        f_period(6)  TYPE n,
      END   OF it_price.

DATA: BEGIN OF it_knumh OCCURS 0,
        knumh LIKE konh-knumh,
        datab LIKE konh-datab,
        datbi LIKE konh-datbi,
      END   OF it_knumh.

DATA: BEGIN OF it_info OCCURS 0,                "Info Condition
          vakey   LIKE   konh-vakey,
          datab   LIKE   konh-datab,
          kzust   LIKE   konh-kzust,
          kpein   LIKE   konp-kpein,
          kumne   LIKE   konp-kumne,
          kumza   LIKE   konp-kumza,
          konwa   LIKE   konp-konwa,
          meins   LIKE   mara-meins,
          wkurs   LIKE   ztmm_analy-wkurs,
          kbetr   LIKE   konp-kbetr,
          rduty   LIKE   konp-kbetr,          "Duty rate
          rfrgt   LIKE   konp-kbetr,          "Freight rate
          rcost   LIKE   konp-kbetr,          "Other cost rate
          wrbtr   TYPE   ztmm_analy-wrbtr,    "Net price
          wduty   LIKE   ztmm_analy-wrbtr,    "duty of F.Curr
          wfrgt   LIKE   ztmm_analy-wrbtr,    "freight of F.Curr
          wcost   LIKE   ztmm_analy-wrbtr,    "cost of F.Curr
          dmbtr   LIKE   ztmm_analy-wrbtr,    "Local Price
          dduty   LIKE   ztmm_analy-wrbtr,    "duty of L.Curr
          dfrgt   LIKE   ztmm_analy-wrbtr,    "freight of L.Curr
          dcost   LIKE   ztmm_analy-wrbtr,    "cost of L.Curr
          wramt   LIKE   ztmm_analy-wrbtr,    "Foreign Amount
          dmamt   LIKE   ztmm_analy-wrbtr,    "Local Amount
      END   OF it_info.

DATA: it_zvmm_info_condi LIKE zvmm_info_condi OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF it_annual_matnr OCCURS 0,
        matnr   LIKE   mara-matnr,
      END   OF it_annual_matnr.

DATA: it_idnrk LIKE ztmm_analy_tmp OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF it_halb OCCURS 0,
        werks   LIKE   t001w-werks,
        idnrk   LIKE   mara-matnr,
      END   OF it_halb.

*----- Work areas
DATA: wa_price LIKE it_price.

DATA: BEGIN OF it_exchange_rate OCCURS 0,
        waers   LIKE   ekko-waers,         "Currency
        wkurs   TYPE   f,                  "Exchange rate
      END   OF it_exchange_rate.

DATA: wa_matnr_f       LIKE  mara-matnr,       "Material From
      wa_matnr_t       LIKE  mara-matnr,       "Material To
      wa_amount        TYPE  f,                "Amount of Base UoM
      wa_period        LIKE  ztmm_analy-period,"Period
      wa_vakey         LIKE  konh-vakey,       "Value Key
      wa_low_amt       TYPE  f,                "lower amount of Base UoM
      wa_base_d        LIKE  sy-datum,         "Base date
      wa_count         TYPE  i,                "Count of Material
      wa_cnt           TYPE  i,                "Total count
      wa_progress_idx  TYPE  i,                "Progress bar index
      wa_count_ready   TYPE  i,                "Count of ready
      wa_count_failed  TYPE  i,                "Count of failed
      wa_count_success TYPE  i,                "Count of success
      wa_count_others  TYPE  i,                "Count of others
      wa_count_finish  TYPE  i,                "Count of finished
      wa_count_t_price TYPE  i,                "Count of temp price
      wa_count_error   TYPE  i,                "Count of error
      wa_waers         LIKE  t001-waers.       "Currency

CONSTANTS: c_mark                   VALUE 'X',   "Marker
           c_ekorg LIKE ekko-ekorg  VALUE 'PU01',"Purchase Org.
           c_kschl LIKE konp-kschl  VALUE 'PB00',"Type of amount
           c_frght LIKE konp-kschl  VALUE 'FRA1',"Type of freight
           c_con01 LIKE konp-kschl  VALUE 'ZOTH',"Type of ETC rate
           c_con02 LIKE konp-kschl  VALUE 'ZOTI',"Type of ETC rate
*           c_price LIKE konp-kbetr  VALUE '0.01',"Default Price
           c_ready                  VALUE '1',   "Ready item
           c_failed                 VALUE '2',   "Update failed item
           c_success                VALUE '3',   "Update successful item
           c_finish                 VALUE '4',   "Update finished item
           c_t_price                VALUE '5',   "Temp price,can't input
           c_error                  VALUE '6'.   "Error item

*----- Selection screens
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-t01.
PARAMETERS:     p_werks LIKE t001w-werks DEFAULT 'P001' OBLIGATORY.
SELECT-OPTIONS: s_ekgrp FOR  t024-ekgrp NO-EXTENSION NO INTERVALS.
SELECT-OPTIONS: s_matnr FOR  mara-matnr NO-EXTENSION.
PARAMETERS:     p_spmon LIKE s001-spmon OBLIGATORY.
SELECTION-SCREEN END   OF BLOCK bl1.

SELECTION-SCREEN BEGIN OF BLOCK bl3 WITH FRAME TITLE text-t12.
PARAMETERS:     p_fprice DEFAULT 'X' AS CHECKBOX.
PARAMETERS:     p_iprice DEFAULT 'X' AS CHECKBOX.
SELECTION-SCREEN END   OF BLOCK bl3.


*----- Get Data
AT SELECTION-SCREEN.
  CHECK sy-ucomm = 'ONLI'.
  PERFORM check_rtn.
  PERFORM get_data.

TOP-OF-PAGE.
  PERFORM top_of_page.

TOP-OF-PAGE DURING LINE-SELECTION.
  PERFORM top_of_page.

START-OF-SELECTION.
  SET PF-STATUS 'BASE'.
  PERFORM write_data.

AT USER-COMMAND.
  CASE sy-ucomm.
    WHEN 'ENTR'.
      sy-lsind = sy-lsind - 1.
      PERFORM enter_rtn.
    WHEN 'SAVE'.
      sy-lsind = sy-lsind - 1.
      PERFORM save_rtn.
    WHEN 'S_ALL'.
      sy-lsind = sy-lsind - 1.
      PERFORM select_all_rtn.
    WHEN 'D_ALL'.
      sy-lsind = sy-lsind - 1.
      PERFORM deselect_all_rtn.
  ENDCASE.
*&---------------------------------------------------------------------*
*&      Form  check_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_rtn.
  PERFORM check_currency.
  PERFORM check_matnr.
  PERFORM check_period.
  PERFORM check_error_data.
ENDFORM.                    " check_rtn
*&---------------------------------------------------------------------*
*&      Form  check_matnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_matnr.
  IF     s_matnr-low EQ ' ' AND s_matnr-high EQ ' '.
    wa_matnr_t = 'ZZZZZZZZZZZZZZZZZZ'.
  ELSEIF s_matnr-low NE ' ' AND s_matnr-high EQ ' '.
    wa_matnr_f = wa_matnr_t = s_matnr-low.
  ELSEIF s_matnr-low EQ ' ' AND s_matnr-high NE ' '.
    wa_matnr_t = s_matnr-high.
  ELSEIF s_matnr-low NE ' ' AND s_matnr-high NE ' '.
    wa_matnr_f = s_matnr-low. wa_matnr_t = s_matnr-high.
  ENDIF.

  SELECT SINGLE * FROM mara WHERE matnr IN s_matnr.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m04.
  ENDIF.
ENDFORM.                    " check_matnr
*&---------------------------------------------------------------------*
*&      Form  check_period
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_period.
*  CASE c_mark.
*    WHEN r_year.
*      CONCATENATE p_year '05' INTO wa_period.
*    WHEN r_quar.
*      IF     r_quar_1 EQ 'X'.
*        CONCATENATE p_year '01' INTO wa_period.
*      ELSEIF r_quar_2 EQ 'X'.
*        CONCATENATE p_year '02' INTO wa_period.
*      ELSEIF r_quar_3 EQ 'X'.
*        CONCATENATE p_year '03' INTO wa_period.
*      ELSEIF r_quar_4 EQ 'X'.
*        CONCATENATE p_year '04' INTO wa_period.
*      ENDIF.
*  ENDCASE.
  wa_period = p_spmon.
ENDFORM.                    " check_period
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  PERFORM get_quater_plan.
ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  locking_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM locking_rtn.
  READ TABLE s_ekgrp INDEX 1.
  IF sy-subrc EQ 0.
    CALL FUNCTION 'ENQUEUE_EZ_ZSMM_ST_LOCK'
         EXPORTING
              mode_ztmm_spe  = 'E'
              mandt          = sy-mandt
              period         = wa_period
              ekgrp          = s_ekgrp-low
              submt          = 'X'
         EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ELSE.
    CALL FUNCTION 'ENQUEUE_EZ_ZSMM_ST_LOCK'
         EXPORTING
              mode_ztmm_spe  = 'E'
              mandt          = sy-mandt
              period         = wa_period
              submt          = 'X'
         EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.
ENDFORM.                    " locking_rtn
*&---------------------------------------------------------------------*
*&      Form  APPEND_ERROR_PRICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_error_price.
  DATA: lw_count TYPE i.

  PERFORM display_progress_bar.

*----- check purchasing group
  READ TABLE s_ekgrp INDEX 1.
  IF sy-subrc EQ 0.
    SELECT SINGLE b~ekgrp
      INTO wa_price-ekgrp
      FROM eina AS a INNER JOIN eine AS b
        ON a~infnr = b~infnr
     WHERE a~matnr = wa_price-matnr
       AND a~loekz = ' '
       AND b~ekgrp IN s_ekgrp
       AND b~loekz = ' '.
    IF sy-subrc NE 0.
      CLEAR: wa_price.
      CHECK 1 = 0.
    ENDIF.
  ENDIF.

*----- Select Vendor for Sub material
  SELECT COUNT(*) INTO lw_count
    FROM eina AS a INNER JOIN eine AS b
      ON a~infnr = b~infnr
   WHERE a~matnr = wa_price-matnr
     AND a~urzzt = 'SUB'
     AND a~loekz = ' '
     AND b~ekorg = c_ekorg
     AND b~loekz = ' '.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH wa_price-matnr text-m09.
  ENDIF.

  IF lw_count >= 2.
    MESSAGE e000(zz) WITH wa_price-matnr text-m10.
  ENDIF.

  SELECT SINGLE lifnr INTO wa_price-i_lifnr
    FROM eina
   WHERE matnr = wa_price-matnr
     AND urzzt = 'SUB'
     AND loekz = ' '.

*----- Read suitable Price
  CLEAR: it_knumh, it_knumh[].
  SELECT knumh datab
    INTO TABLE it_knumh
    FROM a018
   WHERE kappl =  'M'
     AND kschl =  'PB00'
     AND matnr =  wa_price-matnr
     AND lifnr =  wa_price-i_lifnr
     AND ekorg =  c_ekorg
     AND esokz =  '0'
     AND datab <= wa_base_d
     AND datbi >= wa_base_d.

  SORT it_knumh BY datab DESCENDING.

*----- Read the lowest price
  READ TABLE it_knumh INDEX 1.
  IF sy-subrc EQ 0.                 "Standard Price
    DELETE it_knumh WHERE datab < it_knumh-datab.
    PERFORM append_condition_price.
  ELSE.                             "Temp price, No price
    PERFORM read_future_price.
  ENDIF.

  PERFORM append_price_from_wa_price.
  CLEAR: wa_price.

  wa_cnt = wa_cnt + 1.
ENDFORM.                    " APPEND_ERROR_PRICE
*&---------------------------------------------------------------------*
*&      Form  CHECK_ERROR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_error_data.
  SELECT SINGLE base_d INTO wa_base_d
    FROM ztmm_spe
   WHERE werks  = p_werks
     AND period = wa_period
     AND submt  = 'X'.
  IF sy-subrc NE 0.
    MESSAGE w000(zz) WITH text-m05.

    SELECT SINGLE base_d INTO wa_base_d
      FROM ztmm_analy
     WHERE werks  = p_werks
       AND period = wa_period
       AND submt  = 'X'.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH text-m06.
    ENDIF.
  ENDIF.
ENDFORM.                    " CHECK_ERROR_DATA
*&---------------------------------------------------------------------*
*&      Form  append_condition_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_condition_price.
  DATA: lw_error_flg.

  PERFORM read_condition_per_vendor USING lw_error_flg.

  CHECK lw_error_flg IS INITIAL.

  PERFORM set_other_infomation.

  PERFORM delete_temp_price.

  PERFORM select_lowest_price.

  PERFORM append_price_to_it_price.
ENDFORM.                    " append_condition_price
*&---------------------------------------------------------------------*
*&      Form  read_future_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_future_price.
*----- Read suitable Price
  CLEAR: it_knumh, it_knumh[].
  SELECT knumh datab datbi
    INTO TABLE it_knumh
    FROM a018
   WHERE kappl =  'M'
     AND kschl =  'PB00'
     AND matnr =  wa_price-matnr
     AND lifnr =  wa_price-i_lifnr
     AND ekorg =  c_ekorg
     AND esokz =  '0'
     AND datab >  wa_base_d.

  SORT it_knumh BY datab.
*----- Read the lowest price
  READ TABLE it_knumh INDEX 1.
  IF sy-subrc EQ 0.                 "Standard Price
    DELETE it_knumh WHERE datab > it_knumh-datab.
    PERFORM append_condition_price.
  ELSE.
    PERFORM append_blank_price.
  ENDIF.
ENDFORM.                    " read_future_price
*&---------------------------------------------------------------------*
*&      Form  read_condition_per_vendor
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LW_ERROR_FLG  text
*----------------------------------------------------------------------*
FORM read_condition_per_vendor USING pw_error_flg.
*----- Read Info Record

  CLEAR: it_zvmm_info_condi, it_zvmm_info_condi[].
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_zvmm_info_condi
    FROM zvmm_info_condi
     FOR ALL ENTRIES IN it_knumh
   WHERE knumh = it_knumh-knumh
     AND kschl IN (c_kschl,c_frght,c_con01,c_con02)
     AND loevm_ko = ' '.
  IF sy-subrc NE 0.
    PERFORM append_blank_price.
    pw_error_flg = 'X'.
  ENDIF.

  LOOP AT it_zvmm_info_condi.
    READ TABLE it_knumh WITH KEY knumh = it_zvmm_info_condi-knumh.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH text-m02.
    ENDIF.

    MOVE: it_knumh-datab TO it_zvmm_info_condi-datab,
          it_knumh-datbi TO it_zvmm_info_condi-datbi.

    MODIFY it_zvmm_info_condi.
  ENDLOOP.
ENDFORM.                    " read_condition_per_vendor
*&---------------------------------------------------------------------*
*&      Form  append_blank_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_blank_price.
  CLEAR: it_price.
  MOVE: wa_price-matnr TO it_price-matnr,
        wa_price-maktx TO it_price-maktx.

ENDFORM.                    " append_blank_price
*&---------------------------------------------------------------------*
*&      Form  set_other_infomation
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_other_infomation.
*----- Set Import tax rate, freight rate, ETC rate to IT_INFO
  CLEAR: it_info, it_info[].
  SORT it_zvmm_info_condi BY vakey kopos.

  LOOP AT it_zvmm_info_condi.
    CLEAR: it_info.

    READ TABLE it_info WITH KEY vakey = it_zvmm_info_condi-vakey.
    IF sy-subrc NE 0.
      MOVE: it_zvmm_info_condi-vakey TO it_info-vakey,
            it_zvmm_info_condi-datab TO it_info-datab.

      IF it_zvmm_info_condi-kschl_konh EQ c_kschl.
        MOVE: it_zvmm_info_condi-kumne TO it_info-kumne,
              it_zvmm_info_condi-kumza TO it_info-kumza.
      ENDIF.

      IF wa_price-profl EQ 'K'.
        MOVE: wa_price-rduty TO it_info-rduty.
      ENDIF.

      PERFORM move_other_fields.

      APPEND it_info.
    ELSE.
      PERFORM move_other_fields.

      MODIFY it_info INDEX sy-tabix.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " set_other_infomation

*&---------------------------------------------------------------------*
*&      Form  delete_temp_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_temp_price.
*----- If normal price exist, delete 'X' Price
  LOOP AT it_info WHERE kzust(1) <> 'X'.
    EXIT.
  ENDLOOP.
  IF sy-subrc EQ 0.
    DELETE it_info WHERE kzust(1) = 'X'.
  ENDIF.
ENDFORM.                    " delete_temp_price
*&---------------------------------------------------------------------*
*&      Form  select_lowest_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_lowest_price.
*----- Read the lowest price

  CLEAR: wa_low_amt, wa_amount, wa_vakey.
  LOOP AT it_info.
    IF sy-tabix EQ 1.
      MOVE: it_info-vakey TO wa_vakey.
      PERFORM calculate_amount USING wa_low_amt.
      MODIFY it_info.
      CONTINUE.
    ENDIF.

    PERFORM calculate_amount USING wa_amount.

    IF wa_low_amt > wa_amount.
      wa_low_amt  = wa_amount.
      wa_vakey    = it_info-vakey.
    ENDIF.

    MODIFY it_info.
  ENDLOOP.
ENDFORM.                    " select_lowest_price
*&---------------------------------------------------------------------*
*&      Form  calculate_amount
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_LOW_AMT  text
*----------------------------------------------------------------------*
FORM calculate_amount USING pw_amount.
*----- Calculate amount
  DATA: lw_umrez LIKE eina-umrez,        "Numerator
        lw_umren LIKE eina-umren,        "Denomirator
        lw_crate TYPE i,                 "Conversion rate
        lw_wrbtr(16) TYPE p DECIMALS 10. "Amount

  PERFORM read_exchange_rate USING it_info-wkurs it_info-konwa.

  IF it_info-meins EQ wa_price-meins.    "Info UoM =  Master UoM
    pw_amount = ( ( it_info-kbetr / it_info-kpein )
                + ( it_info-kbetr / it_info-kpein ) *
                  ( it_info-rduty / 1000          )
                + ( it_info-kbetr / it_info-kpein ) *
                  ( it_info-wfrgt / 1000          )
                + ( it_info-kbetr / it_info-kpein ) *
                  ( it_info-wcost / 1000          ) )
                *   it_info-wkurs.

    lw_wrbtr = it_info-kbetr / it_info-kpein.

    PERFORM check_decimal_part USING lw_wrbtr lw_crate.

    it_info-wrbtr = ( it_info-kbetr / it_info-kpein ) *
                      lw_crate.

    it_info-wcost = it_info-wrbtr * it_info-rcost / 1000.
    it_info-wfrgt = it_info-wrbtr * it_info-rfrgt / 1000.
    it_info-wduty = it_info-wrbtr * it_info-rduty / 1000.
    it_info-wramt = it_info-wrbtr + it_info-wcost +
                    it_info-wfrgt + it_info-wduty.
    it_info-dmbtr = it_info-wrbtr * it_info-wkurs.
    it_info-dfrgt = it_info-wfrgt * it_info-wkurs.
    it_info-dduty = it_info-wduty * it_info-wkurs.
    it_info-dcost = it_info-wcost * it_info-wkurs.
    it_info-dmamt = it_info-dmbtr + it_info-dcost +
                    it_info-dfrgt + it_info-dduty.
    it_info-kpein =  lw_crate.

  ELSE.                                    "Info UoM <> Master UoM
    SELECT SINGLE umrez umren
      INTO (lw_umrez, lw_umren)
      FROM eina
     WHERE matnr = wa_price-matnr
       AND lifnr = wa_vakey(10).
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH text-m02.
    ENDIF.

*----- If condition's conversion rule does not exist,
*----- read material master conversion rule
    IF it_info-kumne IS INITIAL OR it_info-kumza IS INITIAL.
      SELECT SINGLE umrez umren
        INTO (it_info-kumza, it_info-kumne)
        FROM marm
       WHERE matnr = wa_price-matnr
         AND meinh = it_info-meins.
      IF sy-subrc NE 0.
        MESSAGE e000(zz) WITH wa_vakey(10) wa_price-matnr text-m07.
      ENDIF.
    ENDIF.

    pw_amount = ( ( it_info-kbetr / it_info-kpein ) /
                  ( it_info-kumne * it_info-kumza ) /
                  ( lw_umrez      * lw_umren      )
                + ( it_info-kbetr / it_info-kpein ) /
                  ( it_info-kumne * it_info-kumza ) /
                  ( lw_umrez      * lw_umren      ) *
                  ( it_info-rduty / 1000          )
                + ( it_info-kbetr / it_info-kpein ) /
                  ( it_info-kumne * it_info-kumza ) /
                  ( lw_umrez      * lw_umren      ) *
                  ( it_info-wfrgt / 1000          )
                + ( it_info-kbetr / it_info-kpein ) /
                  ( it_info-kumne * it_info-kumza ) /
                  ( lw_umrez      * lw_umren      ) *
                  ( it_info-wcost / 1000          ) )
                *   it_info-wkurs.

    lw_wrbtr = ( it_info-kbetr / it_info-kpein ) /
               ( it_info-kumne * it_info-kumza ) /
               ( lw_umrez      * lw_umren      ).

    PERFORM check_decimal_part USING lw_wrbtr lw_crate.

    it_info-wrbtr = ( it_info-kbetr / it_info-kpein ) /
                    ( it_info-kumne * it_info-kumza ) /
                    ( lw_umrez      * lw_umren      ) *
                      lw_crate.

    it_info-wcost = it_info-wrbtr * it_info-rcost / 1000.
    it_info-wduty = it_info-wrbtr * it_info-rduty / 1000.
    it_info-wfrgt = it_info-wrbtr * it_info-rfrgt / 1000.
    it_info-wramt = it_info-wrbtr + it_info-wcost +
                    it_info-wfrgt + it_info-wduty.
    it_info-dmbtr = it_info-wrbtr * it_info-wkurs.
    it_info-dduty = it_info-wduty * it_info-wkurs.
    it_info-dfrgt = it_info-wfrgt * it_info-wkurs.
    it_info-dcost = it_info-wcost * it_info-wkurs.
    it_info-dmamt = it_info-dmbtr + it_info-dcost +
                    it_info-dfrgt + it_info-dduty.
    it_info-kpein = lw_crate.
  ENDIF.
ENDFORM.                    " calculate_amount
*&---------------------------------------------------------------------*
*&      Form  append_price_to_it_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_price_to_it_price.
*----- Append selected price to IT_PRICE
  READ TABLE it_info WITH KEY vakey = wa_vakey.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m02.
  ENDIF.

  MOVE: it_info-rduty       TO wa_price-rduty,
        it_info-rcost       TO wa_price-rcost,
        it_info-kzust       TO wa_price-i_kzust,
        it_info-datab       TO wa_price-i_valid_d,
        it_info-konwa       TO wa_price-i_waers,
        it_info-wkurs       TO wa_price-i_wkurs,
        it_info-vakey(10)   TO wa_price-i_lifnr,
        it_info-kpein       TO wa_price-i_peinh,
        it_info-wrbtr       TO wa_price-i_wrbtr,
        it_info-wduty       TO wa_price-i_wduty,
        it_info-wcost       TO wa_price-i_wcost,
        it_info-wfrgt       TO wa_price-i_wfrgt,
        it_info-wramt       TO wa_price-i_wramt,
        it_info-dmbtr       TO wa_price-i_dmbtr,
        it_info-dduty       TO wa_price-i_dduty,
        it_info-dfrgt       TO wa_price-i_dfrgt,
        it_info-dcost       TO wa_price-i_dcost,
        it_info-dmamt       TO wa_price-i_dmamt,
        'X'                 TO wa_price-i_flag.

  SELECT SINGLE name1 INTO wa_price-i_name1
    FROM lfa1
   WHERE lifnr = wa_price-i_lifnr.
ENDFORM.                    " append_price_to_it_price
*&---------------------------------------------------------------------*
*&      Form  read_exchange_rate
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_PRICE_WKURS  text
*----------------------------------------------------------------------*
FORM read_exchange_rate USING p_wkurs p_waers.
  DATA: lw_wkurs TYPE f.


  READ TABLE it_exchange_rate WITH KEY waers = p_waers.
  IF sy-subrc NE 0.
    CALL FUNCTION 'Z_FCA_GET_EXCHANGE_RATE'
         EXPORTING
              client                  = sy-mandt
              date                    = wa_base_d
              source_currency         = p_waers
              target_currency         = wa_waers
              company_currency        = wa_waers
              type_of_rate            = 'P'
         IMPORTING
              exchange_rate           = lw_wkurs
         EXCEPTIONS
              target_local_rate_error = 1
              source_local_rate_error = 2
              OTHERS                  = 3.
    IF sy-subrc <> 0.
*      MESSAGE e000(zz) WITH text-m03 it_price-waers.
    ENDIF.

    MOVE: wa_price-i_waers TO it_exchange_rate-waers,
          lw_wkurs         TO it_exchange_rate-wkurs,
          lw_wkurs         TO p_wkurs.

    APPEND it_exchange_rate.
  ENDIF.

  MOVE it_exchange_rate-wkurs TO p_wkurs.
ENDFORM.                    " read_exchange_rate
*&---------------------------------------------------------------------*
*&      Form  APPEND_PRICE_FROM_WA_PRICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_price_from_wa_price.
  CLEAR: it_price.
  MOVE-CORRESPONDING wa_price TO it_price.

  it_price-f_wramt = it_price-f_wrbtr + it_price-f_wduty +
                     it_price-f_wfrgt + it_price-f_wcost.
  it_price-f_dmamt = it_price-f_dmbtr + it_price-f_dduty +
                     it_price-f_dfrgt + it_price-f_dcost.

  IF NOT it_price-e_flag  IS INITIAL AND
         it_price-e_wrbtr IS INITIAL.
    PERFORM set_error_price.
  ENDIF.

  it_price-e_wramt = it_price-e_wrbtr + it_price-e_wduty +
                     it_price-e_wfrgt + it_price-e_wcost.
  it_price-e_dmamt = it_price-e_dmbtr + it_price-e_dduty +
                     it_price-e_dfrgt + it_price-e_dcost.

  IF     wa_price-f_flag EQ 'X'.
    MOVE: c_finish TO it_price-flag.
    wa_count_finish = wa_count_finish + 1.
    wa_count_others = wa_count_others + 1.
  ELSEIF wa_price-e_flag EQ 'X'.
    MOVE: c_ready TO it_price-flag.
    wa_count_ready = wa_count_ready + 1.
  ELSEIF wa_price-i_flag EQ 'X'.
    MOVE: c_t_price TO it_price-flag.
    wa_count_t_price = wa_count_t_price + 1.
    wa_count_others = wa_count_others + 1.
  ELSE.
    MOVE: c_error TO it_price-flag.
    wa_count_error = wa_count_error + 1.
    wa_count_others = wa_count_others + 1.
  ENDIF.

  wa_count = wa_count + 1.


  IF     wa_price-profl EQ 'K'.
    it_price-type = 'KD'.
  ELSEIF wa_price-profl EQ 'V'.
    it_price-type = 'LP'.
  ENDIF.


  APPEND it_price.
ENDFORM.                    " APPEND_PRICE_FROM_WA_PRICE
*&---------------------------------------------------------------------*
*&      Form  top_of_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM top_of_page.
  RESERVE 3 LINES.

  WRITE:/01(130) text-h06 CENTERED.
  WRITE:/01(130) text-h07 CENTERED.

  NEW-LINE.

  WRITE:/09    text-h09,
         25(7) wa_cnt.

  WRITE:/02(4) icon_yellow_light AS ICON,
               text-h03,
         25(7) wa_count_ready.

  WRITE: 40(4) icon_green_light  AS ICON,
               text-h04,
         63(7) wa_count_success.

  WRITE: 102   text-h14, wa_period.

  WRITE:/02(4) icon_red_light    AS ICON,
               text-h05,
         25(7) wa_count_failed.

  WRITE: 40(4) icon_light_out    AS ICON,
               text-h08,
         63(7) wa_count_others.

  WRITE: 102   text-h15, wa_base_d.

  FORMAT COLOR COL_HEADING INTENSIFIED ON.

  ULINE.
  WRITE:/ text-h01.
  WRITE:/ text-h02.
  WRITE:/ text-h11.

  ULINE.
ENDFORM.                    " top_of_page
*&---------------------------------------------------------------------*
*&      Form  write_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_data.
  SORT it_price BY flag type matnr.

  LOOP AT it_price.
    PERFORM reserve_lines.

    PERFORM display_first_line.
    PERFORM display_second_line.
    PERFORM display_third_line.
    PERFORM display_finish_data.
    PERFORM display_info_data.
    ULINE.
  ENDLOOP.

  CLEAR: it_price.
ENDFORM.                    " write_data
*&---------------------------------------------------------------------*
*&      Form  set_format_color
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LW_COLOR_FLG  text
*----------------------------------------------------------------------*
FORM set_format_color.
  CASE it_price-flag.
    WHEN c_ready.
      FORMAT COLOR 1 INTENSIFIED OFF.
    WHEN c_t_price.
      FORMAT COLOR 7 INTENSIFIED ON.
    WHEN c_finish.
      FORMAT COLOR 5 INTENSIFIED ON.
    WHEN c_error.
      FORMAT COLOR 6 INTENSIFIED ON.
  ENDCASE.
ENDFORM.                    " set_format_color
*&---------------------------------------------------------------------*
*&      Form  display_detail_price_ready
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_detail_price_ready.

ENDFORM.                    " display_detail_price_ready
*&---------------------------------------------------------------------*
*&      Form  display_detail_price_finish
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_detail_price_finish.

ENDFORM.                    " display_detail_price_finish
*&---------------------------------------------------------------------*
*&      Form  display_detail_price_t_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_detail_price_t_price.

ENDFORM.                    " display_detail_price_t_price
*&---------------------------------------------------------------------*
*&      Form  display_detail_price_error
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_detail_price_error.

ENDFORM.                    " display_detail_price_error
*&---------------------------------------------------------------------*
*&      Form  ENTER_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM enter_rtn.
  DATA: lw_lsind LIKE sy-lsind,
        lw_lilli LIKE sy-lilli,
        lw_pagno LIKE sy-pagno.

  MOVE: sy-pagno TO lw_pagno.
  lw_lilli = lw_lilli - 1.
  lw_lsind = lw_lsind + 1.

  PERFORM read_screen_data_for_enter.

  PERFORM write_data.

*----- Set current line to top
  CALL FUNCTION 'LIST_SCROLL_LINE_TOPMOST'
       EXPORTING
            list_index          = lw_lsind
            list_line           = lw_lilli
            list_page           = lw_pagno
       EXCEPTIONS
            list_index_invalid  = 1
            list_line_not_found = 2
            no_list_active      = 3
            window_too_small    = 4
            OTHERS              = 5.

  CLEAR: it_price.
ENDFORM.                    " ENTER_RTN
*&---------------------------------------------------------------------*
*&      Form  display_progress_bar_running
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_progress_bar_running.

ENDFORM.                    " display_progress_bar_running
*&---------------------------------------------------------------------*
*&      Form  NUMERIC_CHECK_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM numeric_check_rtn USING pw_amount.
  CALL FUNCTION 'CATS_NUMERIC_INPUT_CHECK'
       EXPORTING
            input      = pw_amount
       IMPORTING
            output     = pw_amount
       EXCEPTIONS
            no_numeric = 1
            OTHERS     = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " NUMERIC_CHECK_RTN
*&---------------------------------------------------------------------*
*&      Form  read_screen_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_screen_data.
*----- read screen data and update ITAB
  DATA: lw_wramt(50),
        lw_wrbtr(50),
        lw_peinh(10),
        lw_crate TYPE i,
        lw_check,
        lw_line_idx TYPE i,
        lw_maktx(40).

  DO.
    CLEAR: it_price,lw_wramt,lw_wrbtr,lw_line_idx,lw_maktx,
           lw_check,lw_peinh,lw_crate.
    READ LINE sy-index FIELD VALUE it_price-check   INTO lw_check
                                   it_price-maktx   INTO lw_maktx
                                   it_price-e_wramt INTO lw_wramt
                                   it_price-e_wrbtr INTO lw_wrbtr
                                   it_price-e_peinh INTO lw_peinh.
    IF sy-subrc NE 0. EXIT. ENDIF.
    CHECK NOT it_price-matnr IS INITIAL AND lw_check EQ 'X'.

    READ TABLE it_price WITH KEY matnr  = it_price-matnr.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH text-m02.
    ENDIF.

    CHECK NOT lw_maktx IS INITIAL AND
            ( it_price-flag EQ c_ready  OR
              it_price-flag EQ c_failed OR
              it_price-flag EQ c_success ).

    MOVE: sy-tabix TO lw_line_idx.

    IF     it_price-type = 'KD'.
      PERFORM numeric_check_rtn USING lw_wrbtr.

*----- Changed by BSBAE. Changed on May 11,2004
*----- Requested by HSCho
      IF lw_wrbtr IS INITIAL.
*        lw_wrbtr = c_price.
        lw_crate = 1.
      ELSE.
        IF lw_peinh EQ 1.
          PERFORM check_decimal_part USING lw_wrbtr lw_crate.
          it_price-e_wrbtr = lw_wrbtr * lw_crate.
        ELSE.
          lw_crate = lw_peinh.
          it_price-e_wrbtr = lw_wrbtr.
        ENDIF.
      ENDIF.
*----- Changed by BSBAE. Changed on May 11,2004

      it_price-e_peinh = lw_crate.
      it_price-check   = lw_check.
      it_price-e_wrbtr = lw_wrbtr.
      it_price-e_wcost = lw_wrbtr * it_price-rcost / 1000.
      it_price-e_wduty = lw_wrbtr * it_price-rduty / 1000.
      it_price-e_wramt = it_price-e_wrbtr + it_price-e_wcost +
                         it_price-e_wduty.
      it_price-e_dmbtr = it_price-e_wrbtr * it_price-e_wkurs.
      it_price-e_dcost = it_price-e_wcost * it_price-e_wkurs.
      it_price-e_dduty = it_price-e_wduty * it_price-e_wkurs.
      it_price-e_dmamt = it_price-e_wramt * it_price-e_wkurs.
    ELSEIF it_price-type = 'LP'.
      PERFORM numeric_check_rtn USING lw_wramt.

*----- Changed by BSBAE. Changed on May 11,2004
*----- Requested by HSCho
      IF lw_wramt IS INITIAL.
*        lw_wramt = c_price.
        lw_crate = 1.
      ELSE.
        IF lw_peinh EQ 1.
          PERFORM check_decimal_part USING lw_wramt lw_crate.
          it_price-e_wramt = lw_wramt * lw_crate.
        ELSE.
          lw_crate = lw_peinh.
          it_price-e_wrbtr = lw_wrbtr.
        ENDIF.
      ENDIF.
*----- Changed by BSBAE. Changed on May 11,2004

      it_price-e_peinh = lw_crate.
      it_price-check   = lw_check.
      it_price-e_wrbtr = lw_wramt.
      it_price-e_wramt = lw_wrbtr.
      it_price-e_dmbtr = lw_wrbtr.
      it_price-e_dmamt = lw_wrbtr.
    ENDIF.

    MODIFY it_price INDEX lw_line_idx.
  ENDDO.
ENDFORM.                    " read_screen_data
*&---------------------------------------------------------------------*
*&      Form  reserve_lines
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reserve_lines.
  DATA: lw_line TYPE i.

  IF p_fprice EQ 'X'.
    lw_line = lw_line + 2.
  ENDIF.
  IF p_iprice EQ 'X'.
    lw_line = lw_line + 2.
  ENDIF.

  lw_line = lw_line + 4.

  RESERVE lw_line LINES.
ENDFORM.                    " reserve_lines
*&---------------------------------------------------------------------*
*&      Form  display_finish_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_finish_data.
  CHECK p_fprice EQ 'X'.
  FORMAT COLOR 2 INTENSIFIED ON.
  WRITE:/01(31)  space,
                '[Finished]'.

  FORMAT COLOR 5 INTENSIFIED OFF.
  WRITE: 44     it_price-f_kzust,
                it_price-f_waers,
                it_price-f_peinh NO-ZERO,
                it_price-f_wramt CURRENCY it_price-f_waers NO-ZERO,
                it_price-f_wrbtr CURRENCY it_price-f_waers NO-ZERO,
           (10) it_price-f_wduty CURRENCY it_price-f_waers NO-ZERO,
           (10) it_price-f_wfrgt CURRENCY it_price-f_waers NO-ZERO,
           (10) it_price-f_wcost CURRENCY it_price-f_waers NO-ZERO.
  HIDE it_price.

  WRITE:/1(42)  space COLOR COL_NORMAL INTENSIFIED ON,
                it_price-f_valid_d,
           (05) space,
                it_price-f_dmamt CURRENCY wa_waers NO-ZERO,
                it_price-f_dmbtr CURRENCY wa_waers NO-ZERO,
           (10) it_price-f_dduty CURRENCY wa_waers NO-ZERO,
           (10) it_price-f_dfrgt CURRENCY wa_waers NO-ZERO,
           (10) it_price-f_dcost CURRENCY wa_waers NO-ZERO.
  HIDE it_price.
ENDFORM.                    " display_finish_data
*&---------------------------------------------------------------------*
*&      Form  display_info_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_info_data.
  CHECK p_iprice EQ 'X'.
  FORMAT COLOR 2 INTENSIFIED ON.
  WRITE:/01(31)  space,
                '[Info    ]'.

  FORMAT COLOR 7 INTENSIFIED ON.

  WRITE: 44     it_price-i_kzust,
                it_price-i_waers,
                it_price-i_peinh NO-ZERO,
                it_price-i_wramt CURRENCY it_price-i_waers NO-ZERO,
                it_price-i_wrbtr CURRENCY it_price-i_waers NO-ZERO,
           (10) it_price-i_wduty CURRENCY it_price-i_waers NO-ZERO,
           (10) it_price-i_wfrgt CURRENCY it_price-i_waers NO-ZERO,
           (10) it_price-i_wcost CURRENCY it_price-i_waers NO-ZERO.
  HIDE it_price.
  WRITE:/1(42)  space COLOR COL_NORMAL INTENSIFIED ON,
                it_price-i_valid_d,
           (05) space,
                it_price-i_dmamt CURRENCY wa_waers NO-ZERO,
                it_price-i_dmbtr CURRENCY wa_waers NO-ZERO,
           (10) it_price-i_dduty CURRENCY wa_waers NO-ZERO,
           (10) it_price-i_dfrgt CURRENCY wa_waers NO-ZERO,
           (10) it_price-i_dcost CURRENCY wa_waers NO-ZERO.
  HIDE it_price.
ENDFORM.                    " display_info_data
*&---------------------------------------------------------------------*
*&      Form  display_standard_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_third_line.
  PERFORM set_format_color.
  WRITE:/3    it_price-e_lifnr,
         (29) it_price-e_name1.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  WRITE:        it_price-e_valid_d,
           (5)  space,
                it_price-e_dmamt CURRENCY wa_waers NO-ZERO,
                it_price-e_dmbtr CURRENCY wa_waers NO-ZERO,
           (10) it_price-e_dduty CURRENCY wa_waers NO-ZERO,
           (10) it_price-e_dfrgt CURRENCY wa_waers NO-ZERO,
           (10) it_price-e_dcost CURRENCY wa_waers NO-ZERO.

  HIDE it_price.
ENDFORM.                    " display_standard_price
*&---------------------------------------------------------------------*
*&      Form  display_first_line
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_first_line.
  PERFORM set_format_color.

  WRITE:/1      space,
          (18)  it_price-matnr,
                it_price-type,
                space,
                it_price-meins,
          (07)   space.

  CASE it_price-flag.
    WHEN c_ready.
      WRITE: (4) icon_yellow_light AS ICON.
    WHEN c_failed.
      WRITE: (4) icon_red_light    AS ICON HOTSPOT.
    WHEN c_success.
      WRITE: (4) icon_green_light  AS ICON.
    WHEN OTHERS.
      WRITE: (4) icon_light_out    AS ICON HOTSPOT.
  ENDCASE.

  FORMAT COLOR 3 INTENSIFIED OFF.
  WRITE: 54     it_price-peinh,
           (16) it_price-stprs CURRENCY wa_waers,
           (16) it_price-zplp1 CURRENCY wa_waers,
           (10) it_price-zplp2 CURRENCY wa_waers,
           (10) it_price-zplp3 CURRENCY wa_waers,
                '          '.

  HIDE it_price.
ENDFORM.                    " display_first_line
*&---------------------------------------------------------------------*
*&      Form  display_second_line
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_second_line.
  PERFORM set_format_color.

  CASE it_price-flag.
    WHEN c_ready OR c_failed OR c_success.
      WRITE:/1 it_price-check AS CHECKBOX.
    WHEN OTHERS.
      WRITE:/1 it_price-check AS CHECKBOX INPUT OFF.
  ENDCASE.


  WRITE: it_price-maktx.

  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  WRITE:        it_price-e_kzust,
                it_price-e_waers,
                it_price-e_peinh NO-ZERO.

  IF     it_price-flag = c_finish OR
         it_price-flag = c_t_price OR
         it_price-flag = c_error.
    WRITE:  it_price-e_wramt CURRENCY it_price-e_waers NO-ZERO,
            it_price-e_wrbtr CURRENCY it_price-e_waers NO-ZERO.
  ELSE.
    IF     it_price-type EQ 'KD'.
      WRITE:  it_price-e_wramt CURRENCY it_price-e_waers NO-ZERO,
              it_price-e_wrbtr CURRENCY it_price-e_waers INPUT NO-ZERO.
    ELSEIF it_price-type EQ 'LP'.
      WRITE:  it_price-e_wramt CURRENCY it_price-e_waers INPUT NO-ZERO,
              it_price-e_wrbtr CURRENCY it_price-e_waers NO-ZERO.
    ENDIF.
  ENDIF.

  WRITE: (10) it_price-e_wduty NO-ZERO,
         (10) it_price-e_wfrgt NO-ZERO,
         (10) it_price-e_wcost NO-ZERO.

  HIDE it_price.
ENDFORM.                    " display_second_line
*&---------------------------------------------------------------------*
*&      Form  save_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_rtn.
  DATA: lw_lsind LIKE sy-lsind,
        lw_lilli LIKE sy-lilli,
        lw_pagno LIKE sy-pagno.

  MOVE: sy-pagno TO lw_pagno.
  lw_lilli = lw_lilli - 1.
  lw_lsind = lw_lsind + 1.

  PERFORM read_screen_data.

  PERFORM update_table.

  PERFORM write_data.

*----- Set current line to top
  CALL FUNCTION 'LIST_SCROLL_LINE_TOPMOST'
       EXPORTING
            list_index          = lw_lsind
            list_line           = lw_lilli
            list_page           = lw_pagno
       EXCEPTIONS
            list_index_invalid  = 1
            list_line_not_found = 2
            no_list_active      = 3
            window_too_small    = 4
            OTHERS              = 5.

  CLEAR: it_price.
ENDFORM.                    " save_rtn
*&---------------------------------------------------------------------*
*&      Form  update_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_table.
*----- Update Temp Standard price

  LOOP AT it_price WHERE check = 'X'
                     AND ( flag = c_ready OR
                           flag = c_failed ).
    SELECT SINGLE * FROM ztmm_spe WHERE period = wa_period
                                    AND werks  = p_werks
                                    AND matnr  = it_price-matnr
                                    AND submt  = 'X'.
    IF sy-subrc NE 0.
      CASE it_price-flag.
        WHEN c_ready.
          wa_count_ready  = wa_count_ready  - 1.
          wa_count_failed = wa_count_failed + 1.
        WHEN c_success.
          wa_count_success = wa_count_success  - 1.
          wa_count_failed  = wa_count_failed + 1.
        WHEN c_failed.
      ENDCASE.

      it_price-flag = c_failed.
    ENDIF.

    MOVE: it_price-e_wrbtr TO ztmm_spe-wrbtr,
          it_price-e_wduty TO ztmm_spe-wduty,
          it_price-e_wfrgt TO ztmm_spe-wfrgt,
          it_price-e_wcost TO ztmm_spe-wcost,
          it_price-e_dmbtr TO ztmm_spe-dmbtr,
          it_price-e_dduty TO ztmm_spe-dduty,
          it_price-e_dfrgt TO ztmm_spe-dfrgt,
          it_price-e_dcost TO ztmm_spe-dcost,
          it_price-e_peinh TO ztmm_spe-peinh,
          sy-uname         TO ztmm_spe-aenam,
          sy-datum         TO ztmm_spe-aedat,
          sy-uzeit         TO ztmm_spe-aezet.

    UPDATE ztmm_spe.
    IF sy-subrc NE 0.
      CASE it_price-flag.
        WHEN c_ready.
          wa_count_ready  = wa_count_ready  - 1.
          wa_count_failed = wa_count_failed + 1.
        WHEN c_success.
          wa_count_success = wa_count_success  - 1.
          wa_count_failed  = wa_count_failed + 1.
        WHEN c_failed.
      ENDCASE.

      it_price-flag = c_failed.
    ELSE.
      CASE it_price-flag.
        WHEN c_ready.
          wa_count_ready   = wa_count_ready   - 1.
          wa_count_success = wa_count_success + 1.
        WHEN c_success.
        WHEN c_failed.
          wa_count_success = wa_count_success + 1.
          wa_count_failed  = wa_count_failed  - 1.
      ENDCASE.

      it_price-flag = c_success.
    ENDIF.
    MODIFY it_price.
  ENDLOOP.
ENDFORM.                    " update_table
*&---------------------------------------------------------------------*
*&      Form  set_error_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_error_price.
*----- Changed by BSBAE. Changed on May 11,2004
*----- Requested by HSCho
*  IF wa_price-i_flag IS INITIAL.
*    MOVE: '0.01' TO it_price-e_wrbtr,
*          '0.01' TO it_price-e_wramt,
*          '0.01' TO it_price-e_dmbtr,
*          '0.01' TO it_price-e_dmamt.
*  ELSE.
  MOVE: it_price-i_kzust   TO it_price-e_kzust,
        it_price-i_valid_d TO it_price-e_valid_d,
        it_price-i_wrbtr   TO it_price-e_wrbtr,
        it_price-i_wduty   TO it_price-e_wduty,
        it_price-i_wfrgt   TO it_price-e_wfrgt,
        it_price-i_wcost   TO it_price-e_wcost,
        it_price-i_wramt   TO it_price-e_wramt,
        it_price-i_dmbtr   TO it_price-e_dmbtr,
        it_price-i_dduty   TO it_price-e_dduty,
        it_price-i_dfrgt   TO it_price-e_dfrgt,
        it_price-i_dcost   TO it_price-e_dcost,
        it_price-i_dmamt   TO it_price-e_dmamt,
        it_price-i_peinh   TO it_price-e_peinh,
        it_price-i_waers   TO it_price-e_waers,
        it_price-i_wkurs   TO it_price-e_wkurs,
        it_price-i_lifnr   TO it_price-e_lifnr,
        it_price-i_name1   TO it_price-e_name1.
*  ENDIF.
*----- Changed by BSBAE. Changed on May 11,2004
ENDFORM.                    " set_error_price
*&---------------------------------------------------------------------*
*&      Form  display_progress_bar
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_progress_bar.
  DATA: lw_percentage(4) TYPE c,
        lw_mod TYPE i,
        lw_text(50).

  wa_progress_idx = wa_progress_idx + 1.

  lw_percentage = wa_progress_idx / wa_count * 100.

  CONCATENATE text-b01 lw_percentage '%' INTO lw_text.

  lw_mod = lw_percentage MOD 6.

  CHECK lw_mod EQ 5.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            percentage = lw_percentage
            text       = lw_text.
ENDFORM.                    " display_progress_bar
*&---------------------------------------------------------------------*
*&      Form  move_other_fields
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM move_other_fields.
  CASE it_zvmm_info_condi-kschl_konh.
    WHEN c_kschl.
      MOVE: it_zvmm_info_condi-kbetr TO it_info-kbetr,
            it_zvmm_info_condi-kzust TO it_info-kzust,
            it_zvmm_info_condi-kpein TO it_info-kpein,
            it_zvmm_info_condi-kmein TO it_info-meins,
            it_zvmm_info_condi-kumne TO it_info-kumne,
            it_zvmm_info_condi-kumza TO it_info-kumza,
            it_zvmm_info_condi-konwa TO it_info-konwa.
    WHEN c_frght.
      CHECK wa_price-profl EQ 'K'.
      MOVE: it_zvmm_info_condi-kbetr TO it_info-rfrgt.
    WHEN OTHERS.
      CHECK wa_price-profl EQ 'K'.
      it_info-rcost = it_info-rcost + it_zvmm_info_condi-kbetr.
  ENDCASE.
ENDFORM.                    " move_other_fields
*&---------------------------------------------------------------------*
*&      Form  read_total_count
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_total_count.
*----- Get total count of material
  SELECT COUNT( * ) INTO wa_count
    FROM mara
     WHERE matnr BETWEEN wa_matnr_f and wa_matnr_t
       and MTART in ('ROH', 'ROH1')
       AND profl IN ('K',   'V')
       AND lvorm <> 'X'.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m01.
  ENDIF.
ENDFORM.                    " read_total_count
*&---------------------------------------------------------------------*
*&      Form  check_currency
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_currency.
  SELECT SINGLE waers INTO wa_waers FROM t001 WHERE bukrs = 'H201'.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m07.
  ENDIF.
ENDFORM.                    " check_currency
*&---------------------------------------------------------------------*
*&      Form  get_quater_plan
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_quater_plan.
  CLEAR: wa_price.

  PERFORM locking_rtn.

  PERFORM read_total_count.

  CLEAR: wa_cnt.

  EXEC SQL PERFORMING APPEND_ERROR_PRICE.
    SELECT A.MATNR, A.MEINS,  A.PROFL,  C.MAKTX,
           F.KBETR, G.PERIOD, G.WERKS,  G.BASE_D,
           G.EKORG,
           D.STPRS, D.ZPLP1,  D.ZPLP2,  D.ZPLP3, D.PEINH,
           DECODE(NVL(G.PERIOD,'000000'),'000000',' ','X'),
           G.KZUST, G.WRBTR,  G.WDUTY,  G.WCOST,
           G.DMBTR, G.DDUTY,  G.DCOST,  G.WAERS,
           G.WKURS, G.LIFNR,  G.WFRGT,  G.DFRGT,
           G.PEINH,
           DECODE(NVL(H.PERIOD,'000000'), '000000',' ','X'),
           H.KZUST, H.WRBTR,  H.WDUTY,  H.WCOST,
           H.DMBTR, H.DDUTY,  H.DCOST,  H.WAERS,
           H.WKURS, H.LIFNR,  H.VALID_D,H.WFRGT,
           H.DFRGT, H.PEINH
      INTO :WA_PRICE-MATNR,     :WA_PRICE-MEINS,   :WA_PRICE-PROFL,
           :WA_PRICE-MAKTX,     :WA_PRICE-RDUTY,   :WA_PRICE-PERIOD,
           :WA_PRICE-WERKS,     :WA_PRICE-BASE_D,  :WA_PRICE-EKORG,
           :WA_PRICE-STPRS,     :WA_PRICE-ZPLP1,   :WA_PRICE-ZPLP2,
           :WA_PRICE-ZPLP3,     :WA_PRICE-PEINH,
           :WA_PRICE-E_FLAG,
           :WA_PRICE-E_KZUST,   :WA_PRICE-E_WRBTR, :WA_PRICE-E_WDUTY,
           :WA_PRICE-E_WCOST,   :WA_PRICE-E_DMBTR, :WA_PRICE-E_DDUTY,
           :WA_PRICE-E_DCOST,   :WA_PRICE-E_WAERS, :WA_PRICE-E_WKURS,
           :WA_PRICE-E_LIFNR,   :WA_PRICE-E_WFRGT, :WA_PRICE-E_DFRGT,
           :WA_PRICE-E_PEINH,
           :WA_PRICE-F_FLAG,
           :WA_PRICE-F_KZUST,   :WA_PRICE-F_WRBTR, :WA_PRICE-F_WDUTY,
           :WA_PRICE-F_WCOST,   :WA_PRICE-F_DMBTR, :WA_PRICE-F_DDUTY,
           :WA_PRICE-F_DCOST,   :WA_PRICE-F_WAERS, :WA_PRICE-F_WKURS,
           :WA_PRICE-F_LIFNR,   :WA_PRICE-F_VALID_D,:WA_PRICE-F_WFRGT,
           :WA_PRICE-F_DFRGT,   :WA_PRICE-F_PEINH
      FROM MARA A, MARC B, MAKT C, MBEW D, A902 E, KONP F, ZTMM_SPE G,
           ZTMM_ANALY H
     WHERE A.MANDT =   :SY-MANDT
       AND A.MATNR     BETWEEN :WA_MATNR_F AND :WA_MATNR_T
       AND A.MTART     IN ('ROH', 'ROH1')
       AND A.PROFL     IN ('K',   'V')
       AND A.LVORM     <> 'X'
       AND B.MANDT    =  A.MANDT
       AND B.MATNR    =  A.MATNR
       AND B.WERKS    =  :P_WERKS
       AND B.LVORM    =  ' '
       AND (B.DISPO     = 'M02' OR B.DISPO     = 'M03')
       AND C.MANDT     =  A.MANDT
       AND C.MATNR     =  A.MATNR
       AND C.SPRAS     =  :SY-LANGU
       AND D.MANDT(+)  =  B.MANDT
       AND D.MATNR(+)  =  B.MATNR
       AND D.BWKEY(+)  =  B.WERKS
       AND E.MANDT(+)  =  B.MANDT
       AND E.KAPPL(+)  =  'M'
       AND E.KSCHL(+)  =  'ZOA1'
       AND E.STAWN(+)  =  B.STAWN
       AND F.MANDT(+)  =  E.MANDT
       AND F.KNUMH(+)  =  E.KNUMH
       AND G.MANDT(+)  =  A.MANDT
       AND G.PERIOD(+) =  :WA_PERIOD
       AND G.WERKS(+)  =  :P_WERKS
       AND G.MATNR(+)  =  A.MATNR
       AND G.SUBMT(+)  =  'X'
       AND H.MANDT(+)  =  A.MANDT
       AND H.PERIOD(+) =  :WA_PERIOD
       AND H.WERKS(+) =  :P_WERKS
       AND H.MATNR(+)  =  A.MATNR
       AND H.SUBMT(+)  =  'X'
  ENDEXEC.

  READ TABLE it_price INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m01.
  ENDIF.
ENDFORM.                    " get_quater_plan
*&---------------------------------------------------------------------*
*&      Form  display_progress_bar_bom
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_progress_bar_bom USING pwa_matnr.
  DATA: lw_text(55).

  CONCATENATE text-b04 pwa_matnr INTO lw_text.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            percentage = 100
            text       = lw_text.
ENDFORM.                    " display_progress_bar_bom
*&---------------------------------------------------------------------*
*&      Form  check_decimal_part
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PW_AMOUNT  text
*      -->P_LW_CRATE  text
*----------------------------------------------------------------------*
FORM check_decimal_part USING pwa_amount pwa_crate.
*----- If decimal point 3, round.
*----- Maximum Price unit is 10.
  DATA: lw_amount TYPE p DECIMALS 10.

  lw_amount = frac( pwa_amount * 100 ).

  IF lw_amount > 0.
    pwa_crate = 10.
  ELSE.
    pwa_crate = 1.
  ENDIF.
ENDFORM.                    " check_decimal_part
*&---------------------------------------------------------------------*
*&      Form  select_all_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_all_rtn.
  MOVE: 'X' TO it_price-check.
  MODIFY it_price TRANSPORTING check WHERE flag EQ c_ready   OR
                                           flag EQ c_failed  OR
                                           flag EQ c_success.

  PERFORM write_data.
ENDFORM.                    " select_all_rtn
*&---------------------------------------------------------------------*
*&      Form  deselect_all_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM deselect_all_rtn.
  MOVE: '' TO it_price-check.
  MODIFY it_price TRANSPORTING check WHERE flag >= ' '.

  PERFORM write_data.
ENDFORM.                    " deselect_all_rtn
*&---------------------------------------------------------------------*
*&      Form  read_screen_data_for_enter
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_screen_data_for_enter.
*----- read screen data and update ITAB
  DATA: lw_wramt(50),
        lw_wrbtr(50),
        lw_peinh(10),
        lw_crate TYPE i,
        lw_line_idx TYPE i,
        lw_maktx(40).

  DO.
    CLEAR: it_price,lw_wramt,lw_wrbtr,lw_line_idx,lw_maktx,
           lw_peinh,lw_crate.
    READ LINE sy-index FIELD VALUE it_price-maktx   INTO lw_maktx
                                   it_price-e_wramt INTO lw_wramt
                                   it_price-e_wrbtr INTO lw_wrbtr
                                   it_price-e_peinh INTO lw_peinh.
    IF sy-subrc NE 0. EXIT. ENDIF.
    CHECK NOT it_price-matnr IS INITIAL.

    READ TABLE it_price WITH KEY matnr  = it_price-matnr.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH text-m02.
    ENDIF.

    CHECK NOT lw_maktx IS INITIAL AND
            ( it_price-flag EQ c_ready  OR
              it_price-flag EQ c_failed OR
              it_price-flag EQ c_success ).

    MOVE: sy-tabix TO lw_line_idx.

    IF     it_price-type = 'KD'.
      PERFORM numeric_check_rtn USING lw_wrbtr.

*----- Changed by BSBAE. Changed on May 11,2004
*----- Requested by HSCho
      IF lw_wrbtr IS INITIAL.
*        lw_wrbtr = c_price.
        lw_crate = 1.
      ELSE.
        IF lw_peinh EQ 1.
          PERFORM check_decimal_part USING lw_wrbtr lw_crate.
          it_price-e_wrbtr = lw_wrbtr * lw_crate.
        ELSEIF lw_peinh EQ 0.
          lw_crate = lw_peinh = 1.
          it_price-e_wrbtr = lw_wrbtr.
        ELSE.
          lw_crate = lw_peinh.
          it_price-e_wrbtr = lw_wrbtr.
        ENDIF.
      ENDIF.
*----- Changed by BSBAE. Changed on May 11,2004

      it_price-e_wrbtr = lw_wrbtr * lw_crate.
      it_price-e_peinh = lw_crate.
      it_price-e_wcost = lw_wrbtr * it_price-rcost / 1000.
      it_price-e_wduty = lw_wrbtr * it_price-rduty / 1000.
      it_price-e_wramt = it_price-e_wrbtr + it_price-e_wcost +
                         it_price-e_wduty.
      it_price-e_dmbtr = it_price-e_wrbtr * it_price-e_wkurs.
      it_price-e_dcost = it_price-e_wcost * it_price-e_wkurs.
      it_price-e_dduty = it_price-e_wduty * it_price-e_wkurs.
      it_price-e_dmamt = it_price-e_wramt * it_price-e_wkurs.
    ELSEIF it_price-type = 'LP'.
      PERFORM numeric_check_rtn USING lw_wramt.

*----- Changed by BSBAE. Changed on May 11,2004
*----- Requested by HSCho
      IF lw_wramt IS INITIAL.
*        lw_wramt = c_price.
        lw_crate = 1.
      ELSE.
        IF lw_peinh EQ 1.
          PERFORM check_decimal_part USING lw_wramt lw_crate.
          it_price-e_wramt = lw_wramt * lw_crate.
        ELSEIF lw_peinh EQ 0.
          lw_crate = lw_peinh = 1.
          it_price-e_wramt = lw_wramt.
        ELSE.
          lw_crate = lw_peinh.
          it_price-e_wramt = lw_wramt.
        ENDIF.
      ENDIF.
*----- Changed by BSBAE. Changed on May 11,2004

      it_price-e_wrbtr = lw_wramt * lw_crate.
      it_price-e_peinh = lw_crate.
      it_price-e_wrbtr = lw_wrbtr.
      it_price-e_wramt = lw_wrbtr.
      it_price-e_dmbtr = lw_wrbtr.
      it_price-e_dmamt = lw_wrbtr.
    ENDIF.

    MODIFY it_price INDEX lw_line_idx.
  ENDDO.
ENDFORM.                    " read_screen_data_for_enter
