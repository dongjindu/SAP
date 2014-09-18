************************************************************************
* Program Name      : ZEMMPM47R_MODULE_PRICE_ESTI
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
REPORT zemmpm47r_module_price_esti  NO STANDARD PAGE HEADING
                                    LINE-SIZE  122
                                    LINE-COUNT  65.
INCLUDE: <icon>.
TYPE-POOLS : slis.
TABLES: mara,
        ztmm_analy,
        ztmm_spe.

*----- Internal tables
DATA: BEGIN OF it_price OCCURS 0.
        INCLUDE STRUCTURE ztmm_analy.
DATA:   wramt    LIKE ztmm_analy-wrbtr,
        dmamt    LIKE ztmm_analy-wrbtr,
        peinh_mm LIKE mbew-peinh,       "Material Master Price Unit
        maktx    LIKE makt-maktx,
        profl    LIKE mara-profl,
        meins    LIKE mara-meins,
        type(2),
        check,
        flag,
        msg(100),
      END   OF it_price.

DATA: it_temp_price LIKE it_price OCCURS 0 WITH HEADER LINE.
DATA: it_error LIKE ztmm_spe      OCCURS 0 WITH HEADER LINE.

DATA: it_idnrk LIKE ztmm_analy_tmp OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF it_halb OCCURS 0,
        werks   LIKE   t001w-werks,
        idnrk   LIKE   mara-matnr,
      END   OF it_halb.

DATA: BEGIN OF it_annual_matnr OCCURS 0,
        matnr   LIKE   mara-matnr,
      END   OF it_annual_matnr.

DATA: BEGIN OF it_knumh OCCURS 0,
        knumh LIKE konh-knumh,
        datab LIKE konh-datab,
        datbi LIKE konh-datbi,
      END   OF it_knumh.

DATA: BEGIN OF it_info OCCURS 0,                "Info Condition
          vakey   LIKE   konh-vakey,
          datab   LIKE   konh-datab,
          kzust   LIKE   konh-kzust,
          konwa   LIKE   konp-konwa,
          kpein   LIKE   konp-kpein,
          meins   LIKE   mara-meins,
          kumne   LIKE   konp-kumne,
          kumza   LIKE   konp-kumza,
          wrbtr   TYPE   f,
          kbetr   LIKE   konp-kbetr,
          witax   LIKE   konp-kbetr,
          wfrgt   LIKE   konp-kbetr,
          wcost   LIKE   konp-kbetr,
      END   OF it_info.

DATA: BEGIN OF it_exchange_rate OCCURS 0,
        waers   LIKE   ekko-waers,         "Currency
        wkurs   TYPE   f,                  "Exchange rate
      END   OF it_exchange_rate.

DATA: BEGIN OF it_err_msg OCCURS 0,
        base_d   LIKE   sy-datum,
        matnr    LIKE   mara-matnr,
        id       LIKE   bapiret2-id,
        msgno    LIKE   bapiret2-number,
        v1       LIKE   bapiret2-message_v1,
        v2       LIKE   bapiret2-message_v2,
        v3       LIKE   bapiret2-message_v3,
        v4       LIKE   bapiret2-message_v4,
      END   OF it_err_msg.

DATA: it_zvmm_info_condi LIKE zvmm_info_condi OCCURS 0 WITH HEADER LINE.

DATA: it_bapiret2   LIKE bapiret2 OCCURS 0 WITH HEADER LINE.

*----- Work areas
DATA: BEGIN OF wa_price,
        matnr      LIKE mara-matnr,          "Material
        meins      LIKE mara-meins,          "Master UoM
        profl      LIKE mara-profl,          "KD,LD Separator
        witax      LIKE konp-kbetr,          "Import tax rate
        peinh_mm   LIKE mbew-peinh,          "Material Master Price Unit
        maktx      LIKE makt-maktx,          "Description
        period     LIKE ztmm_analy-period,   "Error Period
        werks      LIKE ztmm_analy-werks,    "Error Plant
        ekorg      LIKE ztmm_analy-ekorg,    "Error Purchase Org.
        kzust      LIKE ztmm_analy-kzust,    "Error Reason code
        valid_d    LIKE ztmm_analy-valid_d,  "Error Valid date
        wrbtr      LIKE ztmm_analy-wrbtr,    "Error Foreign Pice
        wduty      LIKE ztmm_analy-wrbtr,    "Error duty of F.Curr
        wfrgt      LIKE ztmm_analy-wfrgt,    "Error freight of F.Curr
        wcost      LIKE ztmm_analy-wrbtr,    "Error cost of F.Curr
        dmbtr      LIKE ztmm_analy-wrbtr,    "Error Local Price
        dduty      LIKE ztmm_analy-wrbtr,    "Error duty of L.Curr
        dfrgt      LIKE ztmm_analy-wfrgt,    "Error freight of L.Curr
        dcost      LIKE ztmm_analy-wrbtr,    "Error cost of L.Curr
        wramt      LIKE ztmm_analy-wrbtr,    "Error Foreign Amount
        dmamt      LIKE ztmm_analy-wrbtr,    "Error Local Amount
        peinh      LIKE ztmm_analy-peinh,    "Error Price unit
        waers      LIKE ztmm_analy-waers,    "Error Currency
        wkurs      LIKE ztmm_analy-wkurs,    "Error exchange rate
        lifnr      LIKE ztmm_analy-lifnr,    "Error vendor
        f_period   LIKE ztmm_analy-period,   "Finish Period
        f_werks    LIKE ztmm_analy-werks,    "Finish Plant
        f_ekorg    LIKE ztmm_analy-ekorg,    "Finish Purchase Org.
        f_kzust    LIKE ztmm_analy-kzust,    "Finish Reason code
        f_valid_d  LIKE ztmm_analy-valid_d,  "Finish Valid date
        f_wrbtr    LIKE ztmm_analy-wrbtr,    "Finish Foreign Pice
        f_wduty    LIKE ztmm_analy-wrbtr,    "Finish duty of F.Curr
        f_wfrgt    LIKE ztmm_analy-wfrgt,    "fINISH freight of F.Curr
        f_wcost    LIKE ztmm_analy-wrbtr,    "Finish cost of F.Curr
        f_dmbtr    LIKE ztmm_analy-wrbtr,    "Finish Local Price
        f_dduty    LIKE ztmm_analy-wrbtr,    "Finish duty of L.Curr
        f_dfrgt    LIKE ztmm_analy-wfrgt,    "fINISH freight of L.Curr
        f_dcost    LIKE ztmm_analy-wrbtr,    "Finish cost of L.Curr
        f_wramt    LIKE ztmm_analy-wrbtr,    "Finish Foreign Amount
        f_dmamt    LIKE ztmm_analy-wrbtr,    "Finish Local Amount
        f_peinh    LIKE ztmm_analy-peinh,    "Finish Price unit
        f_waers    LIKE ztmm_analy-waers,    "Finish Currency
        f_wkurs    LIKE ztmm_analy-wkurs,    "Finish exchange rate
        f_lifnr    LIKE ztmm_analy-lifnr,    "Finish vendor
        F_SOURCE   LIKE ZTMM_ANALY-SOURCE,   "Finish source
      END   OF wa_price.

DATA: wa_amount       TYPE   f,                "Amount of Base UoM
      wa_low_amt      TYPE   f,                "lower amount of Base UoM
      wa_matnr_f      LIKE   mara-matnr,       "Material From
      wa_matnr_t      LIKE   mara-matnr,       "Material To
      wa_period       LIKE   ztmm_analy-period,"Period
      wa_vakey        LIKE   konh-vakey,       "Value Key
      wa_progress_idx TYPE   i,                "Progress bar index
      wa_count        TYPE   i,                "Count of Material
      wa_count_ready  TYPE   i,                "Count of ready
      wa_count_finish TYPE   i,                "Count of finished
      wa_count_fail   TYPE   i,                "Count of failed
      wa_count_error  TYPE   i,                "Count of error
      wa_count_run    TYPE   i,                "Count of running item
      wa_pdate        TYPE   d,                "Plan Price date
      wa_base_date    TYPE   d.                "Base date

CONSTANTS: c_mark                   VALUE 'X',   "Marker
           c_ekorg LIKE ekko-ekorg  VALUE 'PU01',"Purchase Org.
           c_kschl LIKE konp-kschl  VALUE 'PB00',"Type of amount
           c_frght LIKE konp-kschl  VALUE 'FRA1',"Type of freight
           c_con01 LIKE konp-kschl  VALUE 'ZOTH',"Type of ETC rate
           c_con02 LIKE konp-kschl  VALUE 'ZOTI',"Type of ETC rate
           c_waers LIKE ekko-waers  VALUE 'USD', "Currency
           c_price LIKE konp-kbetr  VALUE '0.01',"Default Price
           c_ready                  VALUE '1',   "Ready item
           c_fail                   VALUE '2',   "Update failed item
           c_success                VALUE '3',   "Update successful item
           c_error                  VALUE '4'.   "Error item

*----- Selection screens
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-t01.
PARAMETERS:     p_werks LIKE t001w-werks DEFAULT 'P001' OBLIGATORY.
SELECT-OPTIONS: s_matnr FOR  mara-matnr NO-EXTENSION.
PARAMETERS:     p_spmon LIKE s001-spmon OBLIGATORY.
*PARAMETERS:     p_datum LIKE sy-datum DEFAULT sy-datum OBLIGATORY.
SELECTION-SCREEN END   OF BLOCK bl1.

*----- Get Data
AT SELECTION-SCREEN.
  CHECK sy-ucomm = 'ONLI'.
  PERFORM check_rtn.
  PERFORM get_data.

**---
TOP-OF-PAGE.
  PERFORM top_of_page.

TOP-OF-PAGE DURING LINE-SELECTION.
  PERFORM top_of_page.

START-OF-SELECTION.
  SET PF-STATUS 'BASE'.
  PERFORM write_data.

AT USER-COMMAND.
  CASE sy-ucomm.
    WHEN 'EXCUTE'.
      sy-lsind = sy-lsind - 1.
      PERFORM excute_rtn.
    WHEN 'SAVE'.
      PERFORM error_save_rtn.
    WHEN 'S_ALL'.
      sy-lsind = sy-lsind - 1.
      PERFORM select_all_rtn.
    WHEN 'D_ALL'.
      sy-lsind = sy-lsind - 1.
      PERFORM deselect_all_rtn.
  ENDCASE.

AT LINE-SELECTION.
  PERFORM double_click_rtn.

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  PERFORM get_quater_plan.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  APPEND_STANDARD_PRICE_LP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_standard_price.
  DATA: lw_count TYPE i.

  CLEAR: wa_vakey.

  PERFORM display_progress_bar.

*----- finished item
  IF NOT wa_price-f_period IS INITIAL.
    PERFORM append_finish_price.
    CLEAR: wa_price.
    CHECK 1 = 0.
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

  SELECT SINGLE lifnr INTO wa_price-lifnr
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
     AND lifnr =  wa_price-lifnr
     AND ekorg =  c_ekorg
     AND esokz =  '0'
     AND datab <= wa_base_date
     AND datbi >= wa_base_date.

  SORT it_knumh BY datab DESCENDING.

*----- Read the lowest price
  READ TABLE it_knumh INDEX 1.
  IF sy-subrc EQ 0.                 "Standard Price
    DELETE it_knumh WHERE datab < it_knumh-datab.
    PERFORM append_condition_price.
  ELSE.                             "Temp price, No price
    PERFORM read_future_price.
  ENDIF.

  CLEAR: wa_price.
ENDFORM.                    " APPEND_STANDARD_PRICE_LP
*&---------------------------------------------------------------------*
*&      Form  calculate_amount
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_PRICE  text
*----------------------------------------------------------------------*
FORM calculate_amount USING pwa_amount.
*----- Calculate amount
  DATA: lw_umrez LIKE eina-umrez,        "Numerator
        lw_umren LIKE eina-umren,        "Denomirator
        lw_crate TYPE i,                 "Conversion rate
        lw_wrbtr(16) TYPE p DECIMALS 10.                 "Amount

*----- Info UoM =  Master UoM
  IF it_info-meins EQ wa_price-meins.
    pwa_amount =   ( it_info-kbetr / it_info-kpein )
               + ( ( it_info-kbetr / it_info-kpein ) *
                   ( it_info-witax / 1000          ) )
               + ( ( it_info-kbetr / it_info-kpein ) *
                   ( it_info-wfrgt / 1000          ) )
               + ( ( it_info-kbetr / it_info-kpein ) *
                   ( it_info-wcost / 1000          ) ).

    lw_wrbtr = it_info-kbetr / it_info-kpein.

    PERFORM check_decimal_part USING lw_wrbtr lw_crate.

    it_info-wrbtr = ( it_info-kbetr / it_info-kpein ) *
                      lw_crate.

    it_info-kpein = lw_crate.
  ELSE.                                    "Info UoM <> Master UoM
*----- Read General Conversion rule
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

    pwa_amount = ( ( it_info-kbetr / it_info-kpein ) /
                   ( it_info-kumne * it_info-kumza ) /
                   ( lw_umrez      * lw_umren      ) )
               + ( ( it_info-kbetr / it_info-kpein ) /
                   ( it_info-kumne * it_info-kumza ) /
                   ( lw_umrez      * lw_umren      ) *
                   ( it_info-witax / 1000          ) )
               + ( ( it_info-kbetr / it_info-kpein ) /
                   ( it_info-kumne * it_info-kumza ) /
                   ( lw_umrez      * lw_umren      ) *
                   ( it_info-wfrgt / 1000          ) )
               + ( ( it_info-kbetr / it_info-kpein ) /
                   ( it_info-kumne * it_info-kumza ) /
                   ( lw_umrez      * lw_umren      ) *
                   ( it_info-wcost / 1000          ) ).

    lw_wrbtr = ( it_info-kbetr / it_info-kpein ) /
               ( it_info-kumne * it_info-kumza ) /
               ( lw_umrez      * lw_umren      ).

    PERFORM check_decimal_part USING lw_wrbtr lw_crate.

    it_info-wrbtr = ( it_info-kbetr / it_info-kpein ) /
                    ( it_info-kumne * it_info-kumza ) /
                    ( lw_umrez      * lw_umren      ) *
                    lw_crate.

    it_info-kpein = lw_crate.
  ENDIF.
ENDFORM.                    " calculate_amount

*&---------------------------------------------------------------------*
*&      Form  check_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_rtn.
  PERFORM check_matnr.
  PERFORM set_date.
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
*&      Form  append_blank_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_blank_price.
  CLEAR: it_price.


  IF wa_price-period IS INITIAL.
    " No info, No error item
    PERFORM append_error_price.
  ELSE.
    IF wa_price-wrbtr EQ 0.
      " No info, No error input item
      PERFORM append_error_zero_price.
    ELSE.
      " No info, Error input item
      PERFORM append_ready_price_for_error.
    ENDIF.
  ENDIF.
ENDFORM.                    " append_blank_price
*&---------------------------------------------------------------------*
*&      Form  read_exchange_rate
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_exchange_rate USING p_wkurs.
  DATA: lw_wkurs TYPE f.


  READ TABLE it_exchange_rate WITH KEY waers = it_price-waers.
  IF sy-subrc NE 0.
    CALL FUNCTION 'Z_FCA_GET_EXCHANGE_RATE'
         EXPORTING
              client                  = sy-mandt
              date                    = wa_base_date
              source_currency         = it_price-waers
              target_currency         = c_waers
              company_currency        = c_waers
              type_of_rate            = 'P'
         IMPORTING
              exchange_rate           = lw_wkurs
         EXCEPTIONS
              target_local_rate_error = 1
              source_local_rate_error = 2
              OTHERS                  = 3.
    IF sy-subrc <> 0.
      MESSAGE e000(zz) WITH text-m03 it_price-waers.
    ENDIF.

    MOVE: it_price-waers TO it_exchange_rate-waers,
          lw_wkurs       TO it_exchange_rate-wkurs,
          lw_wkurs       TO p_wkurs.

    APPEND it_exchange_rate.
  ENDIF.

  MOVE it_exchange_rate-wkurs TO p_wkurs.
ENDFORM.                    " read_exchange_rate

*&---------------------------------------------------------------------*
*&      Form  append_condition_price_for_KD
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
ENDFORM.                    " append_condition_price_for_KD
*&---------------------------------------------------------------------*
*&      Form  READ_CONDITION_BY_VENDOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
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
ENDFORM.                    " READ_CONDITION_BY_VENDOR
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

      IF it_zvmm_info_condi-kschl EQ c_kschl.
        MOVE: it_zvmm_info_condi-kumne TO it_info-kumne,
              it_zvmm_info_condi-kumza TO it_info-kumza.
      ENDIF.

      IF wa_price-profl EQ 'K'.
        MOVE: wa_price-witax TO it_info-witax.
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
*&      Form  SELECT_LOWEST_PRICE
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

    PERFORM  calculate_amount USING wa_amount.

    IF wa_low_amt > wa_amount.
      wa_low_amt  = wa_amount.
      wa_vakey    = it_info-vakey.
    ENDIF.

    MODIFY it_info.
  ENDLOOP.
ENDFORM.                    " SELECT_LOWEST_PRICE
*&---------------------------------------------------------------------*
*&      Form  APPEND_PRICE_TO_IT_PRICE
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

  IF it_info-kzust(1) EQ 'X'.         " X price
    IF wa_price-period IS INITIAL.
      "Info exist, error not exist
      PERFORM append_error_x_price.
    ELSE.
      IF wa_price-wrbtr IS INITIAL.
        "Info exist, error exist, price 0
        PERFORM append_error_x_price.
      ELSE.
        "Info exist, error exist
        PERFORM append_ready_price_for_error.
      ENDIF.
    ENDIF.
  ELSE.
    "Normal price exist
    PERFORM append_normal_price.
  ENDIF.
ENDFORM.                    " APPEND_PRICE_TO_IT_PRICE
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

  WRITE:/01(122) text-h06 CENTERED.
  WRITE:/01(122) text-h07 CENTERED.

  NEW-LINE.

  WRITE:/09    text-h09,
         25(7) wa_count.

  WRITE:/02(4) icon_yellow_light AS ICON,
               text-h03,
         25(7) wa_count_ready.

  WRITE: 40(4) icon_green_light  AS ICON,
               text-h04,
         63(7) wa_count_finish.

  WRITE: 100   text-h10, wa_period.

  WRITE:/02(4) icon_red_light    AS ICON,
               text-h05,
         25(7) wa_count_fail.

  WRITE: 40(4) icon_light_out    AS ICON,
               text-h08,
         63(7) wa_count_error.

  WRITE: 100   text-h11, wa_base_date.

  FORMAT COLOR COL_HEADING INTENSIFIED OFF.

  ULINE.
  WRITE:/ text-h01.
  WRITE:/ text-h02.
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
  DATA: lw_color_flg.

  SORT it_price BY flag type matnr.

  LOOP AT it_price.
    RESERVE 3 LINES.

    PERFORM set_format_color USING lw_color_flg.

    IF it_price-flag EQ c_error OR
       it_price-flag EQ c_success.
      WRITE:/ it_price-check AS CHECKBOX INPUT OFF.
    ELSE.
      WRITE:/ it_price-check AS CHECKBOX.
    ENDIF.

    CASE it_price-flag.
      WHEN c_ready.
        WRITE: (4) icon_yellow_light AS ICON.
      WHEN c_fail.
        WRITE: (4) icon_red_light    AS ICON HOTSPOT.
      WHEN c_success.
        WRITE: (4) icon_green_light  AS ICON.
      WHEN c_error.
        WRITE: (4) icon_light_out    AS ICON HOTSPOT.
    ENDCASE.
    WRITE:  (18) it_price-matnr,
                 it_price-type,
                 it_price-kzust,
                 it_price-valid_d,
                 it_price-meins,
            (02) space,
                 it_price-waers,
                 it_price-peinh,
            (12) it_price-wramt,
            (12) it_price-wrbtr,
            (10) it_price-wduty,
            (10) it_price-wfrgt,
            (10) it_price-wcost.

    HIDE: it_price, sy-tabix.


    WRITE:/08(56) it_price-maktx,
             (12) it_price-dmamt,
             (12) it_price-dmbtr,
             (10) it_price-dduty,
             (10) it_price-dfrgt,
             (10) it_price-dcost.

    ULINE.
  ENDLOOP.

  CLEAR: it_price.
ENDFORM.                    " write_data
*&---------------------------------------------------------------------*
*&      Form  set_format_color
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_format_color USING pw_color_flg.
  IF pw_color_flg EQ space.
    FORMAT INTENSIFIED OFF.
    MOVE : 'X' TO pw_color_flg.
  ELSE.
    FORMAT INTENSIFIED ON.
    CLEAR : pw_color_flg.
  ENDIF.

  CASE it_price-flag.
    WHEN c_ready.
      FORMAT COLOR 2 INTENSIFIED OFF.
    WHEN c_fail.
      FORMAT COLOR 2 INTENSIFIED ON.
    WHEN c_success.
      FORMAT COLOR 7 INTENSIFIED ON.
    WHEN c_error.
      FORMAT COLOR 6 INTENSIFIED OFF.
  ENDCASE.
ENDFORM.                    " set_format_color
*&---------------------------------------------------------------------*
*&      Form  SELECT_ALL_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_all_rtn.
  MOVE: 'X' TO it_price-check.
  MODIFY it_price TRANSPORTING check WHERE flag EQ c_ready
                                        OR flag EQ c_fail.

  PERFORM write_data.
ENDFORM.                    " SELECT_ALL_RTN
*&---------------------------------------------------------------------*
*&      Form  DESELECT_ALL_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM deselect_all_rtn.
  MOVE: ' ' TO it_price-check.
  MODIFY it_price TRANSPORTING check WHERE flag EQ c_ready
                                        OR flag EQ c_fail.

  PERFORM write_data.
ENDFORM.                    " DESELECT_ALL_RTN
*&---------------------------------------------------------------------*
*&      Form  EXCUTE_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM excute_rtn.
  DATA: lw_line_idx TYPE i.

  PERFORM get_count_of_checked_item.

  DO.
    CLEAR: it_price.
    READ LINE sy-index FIELD VALUE it_price-check
                                   it_price-matnr.
    IF sy-subrc NE 0. EXIT. ENDIF.
    CHECK NOT it_price-check IS INITIAL.

    READ TABLE it_price WITH KEY base_d = it_price-base_d
                                 matnr  = it_price-matnr.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH text-m02.
    ENDIF.

    PERFORM display_progress_bar_running.

    MOVE: sy-tabix TO lw_line_idx.

    PERFORM change_standard_price.

    MODIFY it_price INDEX lw_line_idx.
  ENDDO.

  PERFORM write_data.
ENDFORM.                    " EXCUTE_RTN
*&---------------------------------------------------------------------*
*&      Form  CHANGE_STANDARD_PRICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_standard_price.
  DATA: wa_head       LIKE bapimathead, "Header with control information
        wa_plant      LIKE bapi_marc  , "plant-specific material DATA
        wa_plantx     LIKE bapi_marcx ,
        wa_mbew       LIKE bapi_mbew  ,
        wa_mbewx      LIKE bapi_mbewx ,
        wa_amount     LIKE bseg-wrbtr,
        wa_peinh      LIKE ekpo-peinh.

  CLEAR: it_bapiret2, it_bapiret2[].

  MOVE: it_price-dmamt  TO wa_amount,
        it_price-peinh  TO wa_peinh.

  wa_head-material     = it_price-matnr.
  wa_head-cost_view    = 'X'.
  wa_plant-lot_size    = wa_peinh.
  wa_plantx-lot_size   = 'X'.
  wa_plant-plant       = it_price-werks.
  wa_plantx-plant      = it_price-werks.
  wa_mbew-val_area     = it_price-werks.
  wa_mbew-plndprice2   = wa_amount.
  wa_mbew-plndprdate2  = wa_pdate.
  wa_mbew-price_unit   = wa_peinh.
  wa_mbewx-val_area    = it_price-werks.
  wa_mbewx-plndprice2  = 'X'.
  wa_mbewx-plndprdate2 = 'X'.
  wa_mbewx-price_unit  = 'X'.

  IF it_price-dmamt = 0.
    PERFORM zero_price_error_rtn.
    PERFORM master_update_failed_rtn.
  ELSE.
    CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
         EXPORTING
              headdata       = wa_head
              valuationdata  = wa_mbew
              valuationdatax = wa_mbewx
              plantdata      = wa_plant
              plantdatax     = wa_plantx
         TABLES
              returnmessages = it_bapiret2.

    READ TABLE it_bapiret2 WITH KEY type = 'E'.
    IF sy-subrc EQ 0.
      PERFORM master_update_failed_rtn.
    ELSE.
      PERFORM master_update_success_rtn.
    ENDIF.
  ENDIF.
ENDFORM.                    " CHANGE_STANDARD_PRICE
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
    FROM mara AS a INNER JOIN marc AS b
      ON a~matnr = b~matnr
     WHERE a~matnr BETWEEN wa_matnr_f and wa_matnr_t
       and A~MTART in ('ROH', 'ROH1')
       AND a~profl IN ('K',   'V')
       AND a~lvorm <> 'X'
       AND b~werks = p_werks.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m01.
  ENDIF.
ENDFORM.                    " read_total_count
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
*&      Form  get_count_of_checked_item
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_count_of_checked_item.
*----- Read count of checked items
  CLEAR: wa_count_run, wa_progress_idx.

  DO.
    CLEAR: it_price.
    READ LINE sy-index FIELD VALUE it_price-check
                                   it_price-matnr.
    IF sy-subrc NE 0. EXIT. ENDIF.
    CHECK NOT it_price-check IS INITIAL.

    wa_count_run = wa_count_run + 1.
  ENDDO.
ENDFORM.                    " get_count_of_checked_item
*&---------------------------------------------------------------------*
*&      Form  display_progress_bar_running
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_progress_bar_running.
  DATA: lw_percentage(3) TYPE c,
        lw_mod TYPE i,
        lw_text(50).

  wa_progress_idx = wa_progress_idx + 1.

  lw_percentage = wa_progress_idx / wa_count_run * 100.

  CONCATENATE text-b02 lw_percentage '%' INTO lw_text.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            percentage = lw_percentage
            text       = lw_text.
ENDFORM.                    " display_progress_bar_running
*&---------------------------------------------------------------------*
*&      Form  double_click_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM double_click_rtn.
*----- Call Material Master Display
  DATA: lw_field(50).

  GET CURSOR FIELD lw_field.

  CASE lw_field.
    WHEN 'IT_PRICE-MATNR'.
      SET PARAMETER ID 'MAT' FIELD it_price-matnr.

      CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
    WHEN 'ICON_RED_LIGHT'.
      CLEAR: it_err_msg.
      READ TABLE it_err_msg WITH KEY base_d = it_price-base_d
                                     matnr  = it_price-matnr.
      IF sy-subrc EQ 0.
        MESSAGE ID it_err_msg-id TYPE 'S' NUMBER it_err_msg-msgno
                WITH it_err_msg-v1 it_err_msg-v2
                     it_err_msg-v3 it_err_msg-v4.
      ELSE.
        MESSAGE e000(zz) WITH text-m02.
      ENDIF.
  ENDCASE.

  CLEAR: it_price.
ENDFORM.                    " double_click_rtn

*&---------------------------------------------------------------------*
*&      Form  INPUT_PRICE_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM input_price_rtn.
  SET PF-STATUS 'INPUT'.
  PERFORM set_it_temp_price.
  PERFORM write_temp_date.
ENDFORM.                    " INPUT_PRICE_RTN
*&---------------------------------------------------------------------*
*&      Form  SET_IT_TEMP_PRICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_it_temp_price.
  DATA: lw_line_idx TYPE i.

  CLEAR: it_temp_price, it_temp_price[].

  DO.
    CLEAR: it_price.
    READ LINE sy-index FIELD VALUE it_price-check
                                   it_price-matnr.
    IF sy-subrc NE 0. EXIT. ENDIF.
    CHECK NOT it_price-check IS INITIAL.

    READ TABLE it_price WITH KEY base_d = it_price-base_d
                                 matnr  = it_price-matnr
                                 BINARY SEARCH.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH text-m02.
    ENDIF.

    CHECK it_price-profl EQ 'V'.

    MOVE: sy-tabix TO lw_line_idx.

    MOVE: it_price TO it_temp_price.

    APPEND it_temp_price.
  ENDDO.
ENDFORM.                    " SET_IT_TEMP_PRICE
*&---------------------------------------------------------------------*
*&      Form  write_temp_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_temp_date.
  DATA: lw_color_flg.

  SORT it_price BY base_d matnr.

  LOOP AT it_price.
    RESERVE 3 LINES.

    PERFORM set_format_color USING lw_color_flg.

    WRITE:/1(18) it_price-matnr,
                 it_price-valid_d,
                 it_price-kzust,
                 it_price-waers,
                 it_price-wramt,
                 it_price-wrbtr INPUT,
                 it_price-wduty,
                 it_price-wcost.

    HIDE: it_price, sy-tabix.

    WRITE:/1(39)  it_price-maktx,
                  it_price-dmamt,
                  it_price-dmbtr,
                  it_price-dduty,
                  it_price-dcost.

    ULINE.
  ENDLOOP.

  CLEAR: it_price.
ENDFORM.                    " write_temp_date
*&---------------------------------------------------------------------*
*&      Form  master_update_failed_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM master_update_failed_rtn.
  CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

  DELETE it_err_msg WHERE base_d = ztmm_spe-base_d
                      AND matnr  = ztmm_spe-matnr.

  CLEAR: it_err_msg.

  MOVE: it_price-base_d        TO it_err_msg-base_d,
        it_price-matnr         TO it_err_msg-matnr,
        it_bapiret2-id         TO it_err_msg-id,
        it_bapiret2-number     TO it_err_msg-msgno,
        it_bapiret2-message_v1 TO it_err_msg-v1,
        it_bapiret2-message_v2 TO it_err_msg-v2,
        it_bapiret2-message_v3 TO it_err_msg-v3,
        it_bapiret2-message_v4 TO it_err_msg-v4.

  APPEND it_err_msg.

  IF it_price-flag EQ c_ready.
    wa_count_ready = wa_count_ready - 1.
    wa_count_fail  = wa_count_fail + 1.
    it_price-flag  = c_fail.
  ENDIF.

  it_price-check = 'X'.
ENDFORM.                    " master_update_failed_rtn
*&---------------------------------------------------------------------*
*&      Form  master_update_success_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM master_update_success_rtn.
  PERFORM set_table_data_for_success.
  PERFORM update_table_for_insert.
ENDFORM.                    " master_update_success_rtn
*&---------------------------------------------------------------------*
*&      Form  update_table_for_insert
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_table_for_insert.
  INSERT ztmm_analy.

  IF sy-subrc NE 0.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    CLEAR: it_err_msg.

    MOVE: it_price-base_d TO it_err_msg-base_d,
          it_price-matnr  TO it_err_msg-matnr,
          'ZZ'            TO it_err_msg-id,
          '000'           TO it_err_msg-msgno,
          text-b03        TO it_err_msg-v1.

    APPEND it_err_msg.

    IF it_price-flag = c_ready.
      it_price-flag  = c_fail.
      wa_count_fail  = wa_count_fail + 1.
      wa_count_ready = wa_count_ready - 1.
    ENDIF.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
         EXPORTING
              wait = 'X'.

    IF it_price-flag = c_ready.
      wa_count_ready = wa_count_ready - 1.
    ELSEIF it_price-flag = c_fail.
      wa_count_fail  = wa_count_fail - 1.
    ENDIF.

    it_price-flag  = c_success.
    it_price-check = ''.
    wa_count_finish = wa_count_finish + 1.
  ENDIF.
ENDFORM.                    " update_table_for_insert
*&---------------------------------------------------------------------*
*&      Form  update_table_for_update
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_table_for_update.
  UPDATE ztmm_analy.
  IF sy-subrc NE 0.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    CLEAR: it_err_msg.

    MOVE: it_price-base_d TO it_err_msg-base_d,
          it_price-matnr  TO it_err_msg-matnr,
          'ZZ'            TO it_err_msg-id,
          '000'           TO it_err_msg-msgno,
          text-b03        TO it_err_msg-v1.

    APPEND it_err_msg.

    IF it_price-flag = c_ready.
      it_price-flag  = c_fail.
      wa_count_fail  = wa_count_fail + 1.
      wa_count_ready = wa_count_ready - 1.
    ENDIF.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
         EXPORTING
              wait = 'X'.

    IF it_price-flag = c_ready.
      wa_count_ready = wa_count_ready - 1.
    ELSEIF it_price-flag = c_fail.
      wa_count_fail  = wa_count_fail - 1.
    ENDIF.

    it_price-flag  = c_success.
    it_price-check = ''.
    wa_count_finish = wa_count_finish + 1.
  ENDIF.
ENDFORM.                    " update_table_for_update
*&---------------------------------------------------------------------*
*&      Form  append_finish_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_finish_price.
  CLEAR: it_price.

  MOVE: wa_period          TO it_price-period,
        wa_base_date       TO it_price-base_d,
        wa_price-matnr     TO it_price-matnr,
        wa_price-profl     TO it_price-profl,
        wa_price-peinh_mm  TO it_price-peinh_mm,
        wa_price-maktx     TO it_price-maktx,
        wa_price-f_werks   TO it_price-werks,
        wa_price-f_ekorg   TO it_price-ekorg,
        wa_price-f_kzust   TO it_price-kzust,
        wa_price-f_valid_d TO it_price-valid_d,
        wa_price-f_wrbtr   TO it_price-wrbtr,
        wa_price-f_wduty   TO it_price-wduty,
        wa_price-f_wfrgt   TO it_price-wfrgt,
        wa_price-f_wcost   TO it_price-wcost,
        wa_price-f_dmbtr   TO it_price-dmbtr,
        wa_price-f_dduty   TO it_price-dduty,
        wa_price-f_dfrgt   TO it_price-dfrgt,
        wa_price-f_dcost   TO it_price-dcost,
        wa_price-f_peinh   TO it_price-peinh,
        wa_price-meins     TO it_price-meins,
        wa_price-f_waers   TO it_price-waers,
        wa_price-f_wkurs   TO it_price-wkurs,
        WA_PRICE-F_SOURCE  TO it_price-source,
        wa_price-f_lifnr   TO it_price-lifnr,
        c_success          TO it_price-flag.

  it_price-wramt = it_price-wrbtr + it_price-wduty +
                   it_price-wfrgt + it_price-wcost.
  it_price-dmamt = it_price-dmbtr + it_price-dduty +
                   it_price-dfrgt + it_price-dcost.

  APPEND it_price.

  wa_count_finish = wa_count_finish + 1.
ENDFORM.                    " append_finish_price
*&---------------------------------------------------------------------*
*&      Form  append_ready_price_for_error
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_ready_price_for_error.
  CLEAR: it_price.

  MOVE: wa_period        TO it_price-period,
        wa_price-werks   TO it_price-werks,
        wa_base_date     TO it_price-base_d,
        wa_price-matnr   TO it_price-matnr,
        wa_price-profl   TO it_price-profl,
        wa_price-peinh_mm  TO it_price-peinh_mm,
        wa_price-maktx   TO it_price-maktx,
        wa_price-werks   TO it_price-werks,
        wa_price-ekorg   TO it_price-ekorg,
        wa_price-kzust   TO it_price-kzust,
        wa_price-valid_d TO it_price-valid_d,
        wa_price-wrbtr   TO it_price-wrbtr,
        wa_price-wduty   TO it_price-wduty,
        wa_price-wfrgt   TO it_price-wfrgt,
        wa_price-wcost   TO it_price-wcost,
        wa_price-dmbtr   TO it_price-dmbtr,
        wa_price-dduty   TO it_price-dduty,
        wa_price-dfrgt   TO it_price-dfrgt,
        wa_price-dcost   TO it_price-dcost,
        wa_price-peinh   TO it_price-peinh,
        wa_price-meins   TO it_price-meins,
        wa_price-waers   TO it_price-waers,
        wa_price-wkurs   TO it_price-wkurs,
        'M'              TO it_price-source,
        wa_price-lifnr   TO it_price-lifnr,
        c_ready          TO it_price-flag.

  it_price-wramt = it_price-wrbtr + it_price-wduty + it_price-wfrgt +
                   it_price-wcost.
  it_price-dmamt = it_price-dmbtr + it_price-dduty + it_price-dfrgt +
                   it_price-dcost.

  IF     wa_price-profl EQ 'K'.
    it_price-type = 'KD'.
  ELSEIF wa_price-profl EQ 'V'.
    it_price-type = 'LP'.
  ENDIF.

  APPEND it_price.

  wa_count_ready = wa_count_ready + 1.
ENDFORM.                    " append_ready_price_for_error
*&---------------------------------------------------------------------*
*&      Form  append_error_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_error_price.
  CLEAR: it_price.

  MOVE: wa_period      TO it_price-period,
        wa_price-werks TO it_price-werks,
        wa_base_date   TO it_price-base_d,
        wa_price-matnr TO it_price-matnr,
        wa_price-profl TO it_price-profl,
        wa_price-peinh_mm  TO it_price-peinh_mm,
        wa_price-maktx TO it_price-maktx,
        ' '            TO it_price-ekorg,
        ' '            TO it_price-kzust,
        wa_base_date   TO it_price-valid_d,
        1              TO it_price-peinh,
        wa_price-meins TO it_price-meins,
        c_waers        TO it_price-waers,
        1              TO it_price-wkurs,
        'M'            TO it_price-source,
        ''             TO it_price-lifnr,
        c_error        TO it_price-flag.

  IF     wa_price-profl EQ 'K'.
    it_price-type = 'KD'.
  ELSEIF wa_price-profl EQ 'V'.
    it_price-type = 'LP'.
  ENDIF.

  APPEND it_price.

  wa_count_error = wa_count_error + 1.
ENDFORM.                    " append_error_price
*&---------------------------------------------------------------------*
*&      Form  ERROR_SAVE_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM error_save_rtn.
*----- Save error items
  CLEAR: it_error, it_error[].

  LOOP AT it_price WHERE flag = c_error.
    MOVE: it_price-period TO it_error-period,
          'X'             TO it_error-submt,
          it_price-base_d TO it_error-base_d,
          it_price-matnr  TO it_error-matnr,
          it_price-werks  TO it_error-werks,
          it_price-ekorg  TO it_error-ekorg,
          it_price-kzust  TO it_error-kzust,
          it_price-waers  TO it_error-waers,
          it_price-wkurs  TO it_error-wkurs,
          it_price-peinh  TO it_error-peinh,
          it_price-lifnr  TO it_error-lifnr,
          sy-uname        TO it_error-ernam,
          sy-datum        TO it_error-erdat,
          sy-uzeit        TO it_error-erzet,
          sy-uname        TO it_error-aenam,
          sy-datum        TO it_error-aedat,
          sy-uzeit        TO it_error-aezet.
    APPEND it_error.
  ENDLOOP.

*----- Delete 0 price items
  DELETE FROM ztmm_spe WHERE period = it_price-period
                         AND werks  = p_werks
                         AND submt  = 'X'
                         AND wrbtr  = 0.

  INSERT ztmm_spe FROM TABLE it_error ACCEPTING DUPLICATE KEYS.
  IF sy-subrc NE 0.
    ROLLBACK WORK.
    MESSAGE e000(zz) WITH text-m12.
  ELSE.
    COMMIT WORK AND WAIT.
    MESSAGE s000(zz) WITH text-m13.
  ENDIF.
ENDFORM.                    " ERROR_SAVE_RTN
*&---------------------------------------------------------------------*
*&      Form  append_error_zero_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_error_zero_price.
  CLEAR: it_price.

  MOVE: wa_period        TO it_price-period,
        wa_price-werks   TO it_price-werks,
        wa_base_date     TO it_price-base_d,
        wa_price-matnr   TO it_price-matnr,
        wa_price-profl   TO it_price-profl,
        wa_price-peinh_mm  TO it_price-peinh_mm,
        wa_price-maktx   TO it_price-maktx,
        wa_price-werks   TO it_price-werks,
        wa_price-ekorg   TO it_price-ekorg,
        wa_price-kzust   TO it_price-kzust,
        wa_price-valid_d TO it_price-valid_d,
        wa_price-wrbtr   TO it_price-wrbtr,
        wa_price-wfrgt   TO it_price-wfrgt,
        wa_price-wduty   TO it_price-wduty,
        wa_price-wcost   TO it_price-wcost,
        wa_price-dmbtr   TO it_price-dmbtr,
        wa_price-dduty   TO it_price-dduty,
        wa_price-dfrgt   TO it_price-dfrgt,
        wa_price-dcost   TO it_price-dcost,
        wa_price-peinh   TO it_price-peinh,
        wa_price-waers   TO it_price-waers,
        wa_price-meins   TO it_price-meins,
        wa_price-wkurs   TO it_price-wkurs,
        'M'              TO it_price-source,
        wa_price-lifnr   TO it_price-lifnr,
        c_error          TO it_price-flag.

  it_price-wramt = it_price-wrbtr + it_price-wduty +
                   it_price-wfrgt + it_price-wcost.
  it_price-dmamt = it_price-dmbtr + it_price-dduty +
                   it_price-dfrgt + it_price-dcost.

  IF     wa_price-profl EQ 'K'.
    it_price-type = 'KD'.
  ELSEIF wa_price-profl EQ 'V'.
    it_price-type = 'LP'.
  ENDIF.

  APPEND it_price.

  wa_count_error = wa_count_error + 1.
ENDFORM.                    " append_error_zero_price
*&---------------------------------------------------------------------*
*&      Form  append_error_X_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_error_x_price.
  CLEAR: it_price.

  MOVE: wa_period           TO it_price-period,
        wa_price-werks      TO it_price-werks,
        wa_base_date        TO it_price-base_d,
        wa_price-matnr      TO it_price-matnr,
        wa_price-profl      TO it_price-profl,
        wa_price-werks      TO it_price-werks,
        wa_price-peinh_mm  TO it_price-peinh_mm,
        wa_price-maktx      TO it_price-maktx,
        it_info-kzust       TO it_price-kzust,
        it_info-datab       TO it_price-valid_d,
        it_info-wrbtr       TO it_price-wrbtr,
        wa_price-meins      TO it_price-meins,
        it_info-konwa       TO it_price-waers,
        'M'                 TO it_price-source,
        it_info-vakey(10)   TO it_price-lifnr,
        it_info-vakey+28(4) TO it_price-ekorg.

  IF it_info-kpein EQ 0.
    MOVE: 1                   TO it_price-peinh.
  ELSE.
    MOVE: it_info-kpein       TO it_price-peinh.
  ENDIF.

  it_price-wcost = it_price-wrbtr * it_info-wcost / 1000.
  it_price-wduty = it_price-wrbtr * it_info-witax / 1000.
  it_price-wfrgt = it_price-wrbtr * it_info-wfrgt / 1000.
  it_price-wramt = it_price-wrbtr + it_price-wcost + it_price-wfrgt +
                   it_price-wduty.

  PERFORM read_exchange_rate USING it_price-wkurs.

  it_price-dmbtr = it_price-wrbtr * it_price-wkurs.
  it_price-dduty = it_price-wduty * it_price-wkurs.
  it_price-dfrgt = it_price-wfrgt * it_price-wkurs.
  it_price-dcost = it_price-wcost * it_price-wkurs.
  it_price-dmamt = it_price-dmbtr + it_price-dcost + it_price-dfrgt +
                   it_price-dduty.

  MOVE: c_error TO it_price-flag.
  wa_count_error = wa_count_error + 1.

  IF     wa_price-profl EQ 'K'.
    it_price-type = 'KD'.
  ELSEIF wa_price-profl EQ 'V'.
    it_price-type = 'LP'.
  ENDIF.

  APPEND it_price.
ENDFORM.                    " append_error_X_price
*&---------------------------------------------------------------------*
*&      Form  append_normal_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_normal_price.
  CLEAR: it_price.

  MOVE: wa_period           TO it_price-period,
        wa_price-werks      TO it_price-werks,
        wa_base_date        TO it_price-base_d,
        wa_price-matnr      TO it_price-matnr,
        wa_price-profl      TO it_price-profl,
        wa_price-werks      TO it_price-werks,
        wa_price-peinh_mm   TO it_price-peinh_mm,
        wa_price-maktx      TO it_price-maktx,
        it_info-kzust       TO it_price-kzust,
        it_info-datab       TO it_price-valid_d,
        it_info-wrbtr       TO it_price-wrbtr,
        it_info-kpein       TO it_price-peinh,
        wa_price-meins      TO it_price-meins,
        it_info-konwa       TO it_price-waers,
        'I'                 TO it_price-source,
        it_info-vakey(10)   TO it_price-lifnr,
        it_info-vakey+28(4) TO it_price-ekorg.

  it_price-wcost = it_price-wrbtr * it_info-wcost / 1000.
  it_price-wfrgt = it_price-wrbtr * it_info-wfrgt / 1000.
  it_price-wduty = it_price-wrbtr * it_info-witax / 1000.
  it_price-wramt = it_price-wrbtr + it_price-wcost + it_price-wfrgt +
                   it_price-wduty.

  PERFORM read_exchange_rate USING it_price-wkurs.

  it_price-dmbtr = it_price-wrbtr * it_price-wkurs.
  it_price-dduty = it_price-wduty * it_price-wkurs.
  it_price-dfrgt = it_price-wrbtr * it_info-wfrgt / 1000.
  it_price-dcost = it_price-wcost * it_price-wkurs.
  it_price-dmamt = it_price-dmbtr + it_price-dcost + it_price-dfrgt +
                   it_price-dduty.

  MOVE: c_ready             TO it_price-flag.
  wa_count_ready = wa_count_ready + 1.

  IF     wa_price-profl EQ 'K'.
    it_price-type = 'KD'.
  ELSEIF wa_price-profl EQ 'V'.
    it_price-type = 'LP'.
  ENDIF.

  APPEND it_price.
ENDFORM.                    " append_normal_price
*&---------------------------------------------------------------------*
*&      Form  locking_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM locking_rtn.
  CALL FUNCTION 'ENQUEUE_EZ_ZSMM_ST_LOCK'
       EXPORTING
            mode_ztmm_spe  = 'E'
            mandt          = sy-mandt
            werks          = p_werks
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
ENDFORM.                    " locking_rtn
*&---------------------------------------------------------------------*
*&      Form  get_quater_plan
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_quater_plan.
  CLEAR: it_exchange_rate, it_exchange_rate[].
  CLEAR: wa_price.

  PERFORM read_total_count.

  PERFORM locking_rtn.

  EXEC SQL PERFORMING APPEND_STANDARD_PRICE.
    SELECT A.MATNR, A.MEINS, A.PROFL, D.KBETR, E.MAKTX,
           B.WERKS, F.EKORG, F.KZUST, F.WRBTR, F.WDUTY,
           F.WCOST, F.DMBTR, F.DDUTY, F.DCOST, F.WAERS,
           F.WKURS, F.LIFNR, G.WERKS, G.EKORG, G.KZUST,
           G.WRBTR, G.WDUTY, G.WCOST, G.DMBTR, G.DDUTY,
           G.DCOST, G.WAERS, G.WKURS, G.LIFNR, F.PERIOD,
           G.PERIOD,F.PEINH, G.PEINH, F.WFRGT, F.DFRGT,
           G.WFRGT, G.DFRGT, H.PEINH, G.SOURCE
      INTO :WA_PRICE-MATNR,   :WA_PRICE-MEINS,   :WA_PRICE-PROFL,
           :WA_PRICE-WITAX,   :WA_PRICE-MAKTX,   :WA_PRICE-WERKS,
           :WA_PRICE-EKORG,   :WA_PRICE-KZUST,   :WA_PRICE-WRBTR,
           :WA_PRICE-WDUTY,   :WA_PRICE-WCOST,   :WA_PRICE-DMBTR,
           :WA_PRICE-DDUTY,   :WA_PRICE-DCOST,   :WA_PRICE-WAERS,
           :WA_PRICE-WKURS,   :WA_PRICE-LIFNR,   :WA_PRICE-F_WERKS,
           :WA_PRICE-F_EKORG, :WA_PRICE-F_KZUST, :WA_PRICE-F_WRBTR,
           :WA_PRICE-F_WDUTY, :WA_PRICE-F_WCOST, :WA_PRICE-F_DMBTR,
           :WA_PRICE-F_DDUTY, :WA_PRICE-F_DCOST, :WA_PRICE-F_WAERS,
           :WA_PRICE-F_WKURS, :WA_PRICE-F_LIFNR, :WA_PRICE-PERIOD,
           :WA_PRICE-F_PERIOD,:WA_PRICE-PEINH,   :WA_PRICE-F_PEINH,
           :WA_PRICE-WFRGT,   :WA_PRICE-DFRGT,   :WA_PRICE-F_WFRGT,
           :WA_PRICE-F_DFRGT, :WA_PRICE-PEINH,   :WA_PRICE-F_SOURCE
      FROM MARA A, MARC B, MBEW H, MAKT E, A902 C, KONP D, ZTMM_SPE F,
           ZTMM_ANALY G
     WHERE A.MANDT = :SY-MANDT
       AND A.MATNR     BETWEEN :WA_MATNR_F AND :WA_MATNR_T
       AND A.MTART     IN ('ROH', 'ROH1')
       AND A.PROFL     IN ('K',   'V')
       AND A.LVORM     <> 'X'
       AND B.MANDT  =  A.MANDT
       AND B.MATNR  =  A.MATNR
       AND B.WERKS  =  :P_WERKS
       AND B.LVORM  =  ' '
       AND (B.DISPO  =  'M02' OR B.DISPO = 'M03')
       AND H.MANDT(+)  =  B.MANDT
       AND H.MATNR(+)  =  B.MATNR
       AND H.BWKEY(+)  =  B.WERKS
       AND H.LVORM(+)  =  ' '
       AND C.MANDT(+)  =  B.MANDT
       AND C.KAPPL(+)  =  'M'
       AND C.KSCHL(+)  =  'ZOA1'
       AND C.STAWN(+)  =  B.STAWN
       AND D.MANDT(+)  =  C.MANDT
       AND D.KNUMH(+)  =  C.KNUMH
       AND E.MANDT     =  A.MANDT
       AND E.MATNR     =  A.MATNR
       AND E.SPRAS     =  :SY-LANGU
       AND F.MANDT(+)  =  A.MANDT
       AND F.PERIOD(+) =  :WA_PERIOD
       AND F.WERKS(+) =  :P_WERKS
       AND F.MATNR(+)  =  A.MATNR
       AND F.SUBMT(+)  =  'X'
       AND G.MANDT(+)  =  A.MANDT
       AND G.PERIOD(+) =  :WA_PERIOD
       AND G.WERKS(+)  =  :P_WERKS
       AND G.MATNR(+)  =  A.MATNR
       AND G.SUBMT(+)  =  'X'
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
*&      Form  set_table_data_for_success
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_table_data_for_success.
  DELETE it_err_msg WHERE base_d = ztmm_spe-base_d
                      AND matnr  = ztmm_spe-matnr.


  CLEAR: ztmm_analy.

  MOVE: it_price-period  TO ztmm_analy-period,
        it_price-base_d  TO ztmm_analy-base_d,
        it_price-matnr   TO ztmm_analy-matnr,
        it_price-werks   TO ztmm_analy-werks,
        it_price-ekorg   TO ztmm_analy-ekorg,
        it_price-kzust   TO ztmm_analy-kzust,
        'X'              TO ztmm_analy-submt,
        it_price-valid_d TO ztmm_analy-valid_d,
        it_price-wrbtr   TO ztmm_analy-wrbtr,
        it_price-wduty   TO ztmm_analy-wduty,
        it_price-wfrgt   TO ztmm_analy-wfrgt,
        it_price-wcost   TO ztmm_analy-wcost,
        it_price-dmbtr   TO ztmm_analy-dmbtr,
        it_price-dduty   TO ztmm_analy-dduty,
        it_price-dfrgt   TO ztmm_analy-dfrgt,
        it_price-dcost   TO ztmm_analy-dcost,
        it_price-peinh   TO ztmm_analy-peinh,
        it_price-waers   TO ztmm_analy-waers,
        it_price-wkurs   TO ztmm_analy-wkurs,
        it_price-source  TO ztmm_analy-source,
        it_price-lifnr   TO ztmm_analy-lifnr,
        sy-uname         TO ztmm_analy-aenam,
        sy-datum         TO ztmm_analy-aedat,
        sy-uzeit         TO ztmm_analy-aezet,
        sy-uname         TO ztmm_analy-ernam,
        sy-datum         TO ztmm_analy-erdat,
        sy-uzeit         TO ztmm_analy-erzet.
ENDFORM.                    " set_table_data_for_success
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
  SELECT knumh datab
    INTO TABLE it_knumh
    FROM a018
   WHERE kappl =  'M'
     AND kschl =  'PB00'
     AND matnr =  wa_price-matnr
     AND lifnr =  wa_price-lifnr
     AND ekorg =  c_ekorg
     AND esokz =  '0'
     AND datab > wa_base_date.

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
*&      Form  move_other_fields
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM move_other_fields.
  CASE it_zvmm_info_condi-kschl.
    WHEN c_kschl.
      MOVE: it_zvmm_info_condi-kbetr TO it_info-kbetr,
            it_zvmm_info_condi-kzust TO it_info-kzust,
            it_zvmm_info_condi-kpein TO it_info-kpein,
            it_zvmm_info_condi-kmein TO it_info-meins,
            it_zvmm_info_condi-konwa TO it_info-konwa,
            it_zvmm_info_condi-kumne TO it_info-kumne,
            it_zvmm_info_condi-kumza TO it_info-kumza.
    WHEN c_frght.
      CHECK wa_price-profl EQ 'K'.
      MOVE: it_zvmm_info_condi-kbetr TO it_info-wfrgt.
    WHEN OTHERS.
      CHECK wa_price-profl EQ 'K'.
      it_info-wcost = it_info-wcost + it_zvmm_info_condi-kbetr.
  ENDCASE.
ENDFORM.                    " move_other_fields
*&---------------------------------------------------------------------*
*&      Form  check_decimal_part
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
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
*&      Form  set_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_date.
  CONCATENATE p_spmon '01' INTO wa_pdate.

  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
       EXPORTING
            day_in            = wa_pdate
       IMPORTING
            last_day_of_month = wa_base_date
       EXCEPTIONS
            day_in_no_date    = 1
            OTHERS            = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  wa_period = p_spmon.
ENDFORM.                    " set_date
*&---------------------------------------------------------------------*
*&      Form  zero_price_error_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form zero_price_error_rtn.
  DELETE it_err_msg WHERE base_d = ztmm_spe-base_d
                      AND matnr  = ztmm_spe-matnr.

  CLEAR: it_err_msg.

  MOVE: it_price-base_d        TO it_err_msg-base_d,
        it_price-matnr         TO it_err_msg-matnr,
        'ZZ'                   TO it_err_msg-id,
        '000'                  TO it_err_msg-msgno,
        text-m14               TO it_err_msg-v1.

  APPEND it_err_msg.

  IF it_price-flag EQ c_ready.
    wa_count_ready = wa_count_ready - 1.
    wa_count_fail  = wa_count_fail + 1.
    it_price-flag  = c_fail.
  ENDIF.

  it_price-check = 'X'.

endform.                    " zero_price_error_rtn
