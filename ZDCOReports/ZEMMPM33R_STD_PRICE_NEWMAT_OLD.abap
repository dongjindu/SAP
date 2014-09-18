************************************************************************
* Program Name      : ZEMMPM33R_STD_PRICE_NEWMAT
* Author            : Byung-sung, Bae
* Creation Date     : 2003.10.16.
* Specifications By : Byung-sung, Bae
* Pattern           : Report 1-1
* Development Request No : UD1K910009
* Addl Documentation:
* Description       : Standard Price Update for New Material
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 3/28/2005  Chris        UD1K915193  Material gross price (type PB00)
*                                     has been changed to the sum of
*                                     type PB00 and type ZTIR because
*                                     Tire and steel wheel module part
*                                     has been split into two part:
*                                     steel wheel and tire.
*
*
*
************************************************************************
REPORT zemmpm33r_std_price_newmat NO STANDARD PAGE HEADING
                                  LINE-SIZE  120
                                  LINE-COUNT  65.
INCLUDE: <icon>.
TYPE-POOLS : slis.
TABLES: mara,
        keko,
        a018,
        ztmm_analy,
        ztmm_spe,
        ztmm_analy_new,
        zvmm_info_condi,
        ztmm_spe_new.

*----- Internal tables
DATA: BEGIN OF it_price OCCURS 0.
        INCLUDE STRUCTURE ztmm_analy_new.
DATA:   wramt LIKE ztmm_analy-wrbtr,
        dmamt LIKE ztmm_analy-wrbtr,
        maktx LIKE makt-maktx,
        profl LIKE mara-profl,
        meins LIKE mara-meins,
        type(2),
        flag,
      END   OF it_price.

DATA: it_ztmm_analy_new LIKE it_price OCCURS 0 WITH HEADER LINE.
DATA: it_temp_price LIKE it_price OCCURS 0 WITH HEADER LINE.
DATA: it_error LIKE ztmm_spe_new  OCCURS 0 WITH HEADER LINE.

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
        lifnr LIKE lfa1-lifnr,
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

DATA: BEGIN OF bdc_tab OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF bdc_tab.

DATA: it_zvmm_info_condi LIKE zvmm_info_condi OCCURS 0 WITH HEADER LINE.

DATA: it_bapiret2   LIKE bapiret2 OCCURS 0 WITH HEADER LINE.

*----- Work areas
DATA: BEGIN OF wa_price,
        matnr      LIKE mara-matnr,          "Material
        meins      LIKE mara-meins,          "Master UoM
        profl      LIKE mara-profl,          "KD,LD Separator
        witax      LIKE konp-kbetr,          "Import tax rate
        maktx      LIKE makt-maktx,          "Description
        stprs      LIKE mbew-stprs,          "Standard price
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
        eflag      LIKE ztmm_analy_new-eflag,"Error Flag
        f_period   LIKE ztmm_analy-period,   "Finish Period
        f_werks    LIKE ztmm_analy-werks,    "Finish Plant
        f_ekorg    LIKE ztmm_analy-ekorg,    "Finish Purchase Org.
        f_kzust    LIKE ztmm_analy-kzust,    "Finish Reason code
        f_valid_d  LIKE ztmm_analy-valid_d,  "Finish Valid date
        f_wrbtr    LIKE ztmm_analy-wrbtr,    "Finish Foreign Pice
        f_wduty    LIKE ztmm_analy-wrbtr,    "Finish duty of F.Curr
        f_wfrgt    LIKE ztmm_analy-wfrgt,    "Finish freight of F.Curr
        f_wcost    LIKE ztmm_analy-wrbtr,    "Finish cost of F.Curr
        f_dmbtr    LIKE ztmm_analy-wrbtr,    "Finish Local Price
        f_dduty    LIKE ztmm_analy-wrbtr,    "Finish duty of L.Curr
        f_dfrgt    LIKE ztmm_analy-wfrgt,    "Finish freight of L.Curr
        f_dcost    LIKE ztmm_analy-wrbtr,    "Finish cost of L.Curr
        f_wramt    LIKE ztmm_analy-wrbtr,    "Finish Foreign Amount
        f_dmamt    LIKE ztmm_analy-wrbtr,    "Finish Local Amount
        f_peinh    LIKE ztmm_analy-peinh,    "Finish Price unit
        f_waers    LIKE ztmm_analy-waers,    "Finish Currency
        f_wkurs    LIKE ztmm_analy-wkurs,    "Finish exchange rate
        f_lifnr    LIKE ztmm_analy-lifnr,    "Finish vendor
        f_source   LIKE ztmm_analy-source,   "Finish source
      END   OF wa_price.

DATA: BEGIN OF wa_errsts,
        errsts01,                      "Material Master incollect
        errsts02,                      "Standard Price is not updated
        errsts03,                      "Standard Price is diffrent
        errsts04,                      "Price Unit is different
        errsts05,                      "Table consistence is not collect
      END   OF wa_errsts.

DATA: wa_amount       TYPE   f,                "Amount of Base UoM
      wa_low_amt      TYPE   f,                "lower amount of Base UoM
      wa_matnr_f      LIKE   mara-matnr,       "Material From
      wa_matnr_t      LIKE   mara-matnr,       "Material To
      wa_period       LIKE   ztmm_analy-period,"Period
      wa_vakey        LIKE   konh-vakey,       "Value Key
      wa_progress_idx TYPE   i,                "Progress bar index
      wa_count        TYPE   i,                "Count of Material
      wa_count_finish TYPE   i,                "Count of finished
      wa_count_fail   TYPE   i,                "Count of failed
      wa_count_error  TYPE   i,                "Count of error
      wa_count_cost   TYPE   i,                "Count of cost run failed
      wa_pdate        TYPE   d,                "Plan Price date
      wa_base_date    LIKE   sy-datum,         "Base date
      wa_error_flg.                            "Error flag

CONSTANTS: c_mark                   VALUE 'X',   "Marker
           c_ekorg LIKE ekko-ekorg  VALUE 'PU01',"Purchase Org.
           c_kschl LIKE konp-kschl  VALUE 'PB00',"Type of amount
           c_frght LIKE konp-kschl  VALUE 'FRA1',"Type of freight
           c_con01 LIKE konp-kschl  VALUE 'ZOTH',"Type of ETC rate
           c_con02 LIKE konp-kschl  VALUE 'ZOTI',"Type of ETC rate
           c_waers LIKE ekko-waers  VALUE 'USD', "Default Currency
           c_bukrs LIKE t001-bukrs  VALUE 'H201',"Company code
           c_ready                  VALUE '1',   "Ready item
           c_success                VALUE '2',   "Update successful item
           c_fail                   VALUE '3',   "Update failed item
           c_error                  VALUE '5',   "Error item
           c_cost_fail              VALUE '4',   "Cost run failed
           c_step1(5)               VALUE 'STEP1',          "Step 1
           c_step2(5)               VALUE 'STEP2',          "Step 2
           c_step3(5)               VALUE 'STEP3',          "Step 3
*          steel wheel and tire module price has been split into two
*          parts: one price for tire(ztir) and one price for steel
*          wheel(pb00)
           C_TIRE_PRICE_TYPE LIKE EKKO-EKORG VALUE 'ZTIR'. "TIRE PRICE

*----- Selection screens
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-t01.
PARAMETERS:     p_werks LIKE t001w-werks DEFAULT 'P001' OBLIGATORY.
SELECT-OPTIONS: s_matnr FOR  mara-matnr NO-EXTENSION.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20) uline.
SELECTION-SCREEN END   OF LINE.
PARAMETERS:     p_repro AS CHECKBOX.
SELECTION-SCREEN END   OF BLOCK bl1.

*----- Get Data
AT SELECTION-SCREEN.
*  CHECK sy-ucomm = 'ONLI'.

START-OF-SELECTION.
  IF p_repro EQ 'X'.
    PERFORM reprocessing_rtn.
  ELSE.
    PERFORM normal_processing_rtn.
  ENDIF.

**---
TOP-OF-PAGE.
  PERFORM top_of_page.

TOP-OF-PAGE DURING LINE-SELECTION.
  PERFORM top_of_page.

START-OF-SELECTION.
  SET PF-STATUS 'BASE'.
  PERFORM write_data.

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
  CLEAR: it_exchange_rate, it_exchange_rate[].
  CLEAR: wa_price.

  PERFORM locking_rtn.

  CHECK wa_error_flg IS INITIAL.

  EXEC SQL PERFORMING APPEND_STANDARD_PRICE.
    SELECT A.MATNR, A.MEINS, A.PROFL, D.KBETR, E.MAKTX,
           B.WERKS, F.EKORG, F.KZUST, F.WRBTR, F.WDUTY,
           F.WCOST, F.DMBTR, F.DDUTY, F.DCOST, F.WAERS,
           F.WKURS, F.LIFNR, G.WERKS, G.EKORG, G.KZUST,
           G.WRBTR, G.WDUTY, G.WCOST, G.DMBTR, G.DDUTY,
           G.DCOST, G.WAERS, G.WKURS, G.LIFNR, F.PERIOD,
           G.PERIOD,F.PEINH, G.PEINH, F.WFRGT, F.DFRGT,
           G.WFRGT, G.DFRGT, H.STPRS, G.EFLAG
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
           :WA_PRICE-F_DFRGT  :WA_PRICE-STPRS,   :WA_PRICE-EFLAG
      FROM MARA A, MARC B, MBEW H, MAKT E, A902 C, KONP D,
           ZTMM_SPE_NEW F, ZTMM_ANALY_NEW G
     WHERE A.MANDT = :SY-MANDT
       AND A.MATNR     BETWEEN :WA_MATNR_F AND :WA_MATNR_T
       AND A.MTART     IN ('ROH', 'ROH1')
       AND A.PROFL     IN ('K',   'V')
       AND A.LVORM     <> 'X'
       AND B.MANDT     =  A.MANDT
       AND B.MATNR     =  A.MATNR
       AND B.WERKS     =  :P_WERKS
       AND B.LVORM     =  ' '
       AND B.DISPO     <> 'M02'
       AND H.MANDT     =  B.MANDT
       AND H.MATNR     =  B.MATNR
       AND H.BWKEY     =  B.WERKS
       AND H.STPRS     =  0
       AND H.LVORM     =  ' '
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
       AND F.WERKS(+)  =  :P_WERKS
       AND F.MATNR(+)  =  A.MATNR
       AND G.MANDT(+)  =  A.MANDT
       AND G.PERIOD(+) =  :WA_PERIOD
       AND G.WERKS(+)  =  :P_WERKS
       AND G.MATNR(+)  =  A.MATNR
  ENDEXEC.

  READ TABLE it_price INDEX 1.
  IF sy-subrc NE 0.
    WRITE:/ text-m01.
    wa_error_flg = 'X'.
*    MESSAGE e000(zz) WITH text-m01.
  ENDIF.
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
  CLEAR: wa_vakey.

  CHECK wa_error_flg IS INITIAL.

  CHECK wa_price-eflag NE 'E'.

  SELECT SINGLE * FROM ztmm_spe WHERE period = wa_period
                                  AND werks  = wa_price-werks
                                  AND matnr  = wa_price-matnr.
  IF sy-subrc EQ 0 and ztmm_spe-WRBTR eq 0.
    EXIT.
  ENDIF.

  SELECT SINGLE * FROM ztmm_analy WHERE period = wa_period
                                    AND werks  = wa_price-werks
                                    AND matnr  = wa_price-matnr.
  IF sy-subrc EQ 0.
    EXIT.
  ENDIF.

*----- finished item
  IF NOT wa_price-f_period IS INITIAL.
    PERFORM append_finish_price.
    CLEAR: wa_price.
    CHECK 1 = 0.
  ENDIF.

*----- Read suitable Price
  CLEAR: it_knumh, it_knumh[].
  SELECT knumh datab lifnr
    INTO CORRESPONDING FIELDS OF TABLE it_knumh
    FROM a018
   WHERE kappl =  'M'
     AND kschl =  'PB00'
     AND matnr =  wa_price-matnr
     AND ekorg =  c_ekorg
     AND esokz =  '0'
     AND datab <= sy-datum
     AND datbi >= sy-datum.

*----- Check Info Record Deletion Mark
  DATA: lw_matnr LIKE mara-matnr.
  LOOP AT it_knumh.
    SELECT SINGLE matnr
      INTO lw_matnr
      FROM eina AS a INNER JOIN eine AS b
        ON a~infnr = b~infnr
     WHERE a~matnr = wa_price-matnr
       AND a~lifnr = it_knumh-lifnr
       AND a~loekz = ' '
       AND b~werks = ' '
       AND b~ekorg = c_ekorg
       AND b~loekz = ' '.
    IF sy-subrc NE 0.
      DELETE it_knumh.
    ENDIF.
  ENDLOOP.

  SORT it_knumh BY datab DESCENDING.

*----- Read the lowest price
  READ TABLE it_knumh INDEX 1.
  IF sy-subrc EQ 0.                 "Standard Price
    DELETE it_knumh WHERE datab < it_knumh-datab.
    PERFORM check_future_price.
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
      WRITE:/ text-m02.
      wa_error_flg = 'X'.
      EXIT.
*      MESSAGE e000(zz) WITH text-m02.
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
        WRITE:/ wa_vakey(10),
                wa_price-matnr,
                text-m07.
        wa_error_flg = 'X'.
        EXIT.
*        MESSAGE e000(zz) WITH wa_vakey(10) wa_price-matnr text-m07.
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
  PERFORM check_period.
  PERFORM check_base_date.
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
    WRITE: text-m04.
    wa_error_flg = 'X'.
    EXIT.
*    MESSAGE e000(zz) WITH text-m04.
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
              date                    = sy-datum
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
      WRITE:/ text-m03,
              it_price-waers.
      wa_error_flg = 'X'.
      EXIT.
*      MESSAGE e000(zz) WITH text-m03 it_price-waers.
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
*    Requested by chandra changed by chris
*    the gross price include type PB00 and ZTIR
*    AND kschl IN (c_kschl,c_frght,c_con01,c_con02)
     AND kschl IN (c_kschl,c_frght,c_con01,c_con02,C_TIRE_PRICE_TYPE)

*    end of change ON 03/28/2005
     AND loevm_ko = ' '.

  IF sy-subrc NE 0.
    PERFORM append_blank_price.
    pw_error_flg = 'X'.
  ENDIF.

*    Requested by chandra changed by chris
*    the gross price include type PB00 and ZTIR
*    COMBINE TYPE PB00 AND ZTIR PRICE TOGETHER
  DATA: WA_CONDI LIKE IT_ZVMM_INFO_CONDI.
  LOOP AT IT_ZVMM_INFO_CONDI.
     IF IT_ZVMM_INFO_CONDI-KSCHL = C_TIRE_PRICE_TYPE.
       CLEAR: WA_CONDI.
       READ TABLE IT_ZVMM_INFO_CONDI INTO WA_CONDI
            WITH KEY KNUMH = IT_ZVMM_INFO_CONDI-KNUMH
                     KSCHL = C_KSCHL.
       WA_CONDI-KBETR = WA_CONDI-KBETR + IT_ZVMM_INFO_CONDI-KBETR.
       MODIFY TABLE IT_ZVMM_INFO_CONDI FROM WA_CONDI
            TRANSPORTING KBETR.
       DELETE IT_ZVMM_INFO_CONDI.
     ENDIF.
  ENDLOOP.

*    end of change ON 03/28/2005


  LOOP AT it_zvmm_info_condi.
    READ TABLE it_knumh WITH KEY knumh = it_zvmm_info_condi-knumh.
    IF sy-subrc NE 0.
      WRITE:/ text-m02.
      wa_error_flg = 'X'.
      EXIT.
*      MESSAGE e000(zz) WITH text-m02.
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
    WRITE:/ text-m02.
    wa_error_flg = 'X'.
    EXIT.
*    MESSAGE e000(zz) WITH text-m02.
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

  WRITE:/02(4) icon_green_light  AS ICON,
               text-h04,
         25(7) wa_count_finish.

  WRITE: 40(4) icon_red_light    AS ICON,
               text-h05,
         63(7) wa_count_fail.


  WRITE: 98   text-h10, wa_period.


  WRITE:/02(4) icon_yellow_light  AS ICON,
               text-h12,
         25(7) wa_count_cost.

  WRITE: 40(4) icon_light_out    AS ICON,
               text-h08,
         63(7) wa_count_error.

  WRITE: 98   text-h11, wa_base_date.

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
    IF it_price-msg IS INITIAL.
      RESERVE 3 LINES.
    ELSE.
      RESERVE 4 LINES.
    ENDIF.
    PERFORM set_format_color USING lw_color_flg.

    CASE it_price-flag.
      WHEN c_cost_fail.
        WRITE: (4) icon_yellow_light AS ICON.
      WHEN c_fail.
        WRITE: (4) icon_red_light    AS ICON.
      WHEN c_success.
        WRITE: (4) icon_green_light  AS ICON.
      WHEN c_error.
        WRITE: (4) icon_light_out    AS ICON.
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


    WRITE:/06(56) it_price-maktx,
             (12) it_price-dmamt,
             (12) it_price-dmbtr,
             (10) it_price-dduty,
             (10) it_price-dfrgt,
             (10) it_price-dcost.

    IF it_price-msg NE ' '.
      WRITE:/25(85) it_price-msg.
    ENDIF.

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
    WHEN c_cost_fail.
      FORMAT COLOR 7 INTENSIFIED ON.
    WHEN c_fail.
      FORMAT COLOR 2 INTENSIFIED ON.
    WHEN c_success.
      FORMAT COLOR 2 INTENSIFIED OFF.
    WHEN c_error.
      FORMAT COLOR 6 INTENSIFIED OFF.
  ENDCASE.
ENDFORM.                    " set_format_color
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

  CHECK wa_error_flg IS INITIAL.

  LOOP AT it_price WHERE flag = c_ready.

    PERFORM change_standard_price.

    MODIFY it_price.
  ENDLOOP.
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
  PERFORM change_costing_lot_size.
  PERFORM change_excute_mr21.
  PERFORM set_table_data.
  PERFORM update_table.
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
    WRITE:/ text-m01.
    wa_error_flg = 'X'.
    EXIT.
*    MESSAGE e000(zz) WITH text-m01.
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
*&      Form  double_click_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM double_click_rtn.
*----- Call Material Master Display

  SET PARAMETER ID 'MAT' FIELD it_price-matnr.

  CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.

  CLEAR: it_price.
ENDFORM.                    " double_click_rtn

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

  CONCATENATE c_step1 it_bapiret2-message INTO it_price-msg
              SEPARATED BY space.

  it_price-flag  = c_fail.
  it_price-eflag = 'E'.
ENDFORM.                    " master_update_failed_rtn
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

    MOVE: text-b03 TO it_price-msg.

    IF it_price-flag = c_ready.
      it_price-flag  = c_fail.
      wa_count_fail  = wa_count_fail + 1.
    ENDIF.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
         EXPORTING
              wait = 'X'.

    it_price-flag  = c_success.
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
        'M'                TO it_price-source,
        wa_price-f_lifnr   TO it_price-lifnr.

  IF wa_price-stprs EQ 0.
    MOVE: c_cost_fail        TO it_price-flag.
    wa_count_cost = wa_count_cost + 1.
  ELSE.
    MOVE: c_success          TO it_price-flag.
    wa_count_finish = wa_count_finish + 1.
  ENDIF.

  it_price-wramt = it_price-wrbtr + it_price-wduty +
                   it_price-wfrgt + it_price-wcost.
  it_price-dmamt = it_price-dmbtr + it_price-dduty +
                   it_price-dfrgt + it_price-dcost.

  APPEND it_price.

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
ENDFORM.                    " append_ready_price_for_error
*&---------------------------------------------------------------------*
*&      Form  check_period
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_period.
*  CASE sy-datum+4(2).
*    WHEN '01' OR '02' OR '03'.
*      CONCATENATE sy-datum(4) '01' INTO wa_period.
*    WHEN '04' OR '05' OR '06'.
*      CONCATENATE sy-datum(4) '02' INTO wa_period.
*    WHEN '07' OR '08' OR '09'.
*      CONCATENATE sy-datum(4) '03' INTO wa_period.
*    WHEN '10' OR '11' OR '12'.
*      CONCATENATE sy-datum(4) '04' INTO wa_period.
*  ENDCASE.
      move sy-datum(6) TO wa_period.
ENDFORM.                    " check_period
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

*  CHECK wa_error_flg IS INITIAL.

  LOOP AT it_price WHERE flag = c_error.
    MOVE: it_price-period TO it_error-period,
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
  DELETE FROM ztmm_spe_new WHERE period = it_price-period
                             AND werks  = p_werks
                             AND wrbtr  = 0.

  INSERT ztmm_spe_new FROM TABLE it_error ACCEPTING DUPLICATE KEYS.
  IF sy-subrc NE 0.
    ROLLBACK WORK.
    MESSAGE s000(zz) WITH text-m12.
  ELSE.
    COMMIT WORK AND WAIT.
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
            submt          = ' '
       EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.
  CASE sy-subrc.
    WHEN 1.
      WRITE:/ text-m08,
              sy-msgv1.
      wa_error_flg = 'X'.
      EXIT.
    WHEN 2 OR 3.
      WRITE:/ text-m09.
      wa_error_flg = 'X'.
      EXIT.
  ENDCASE.
ENDFORM.                    " locking_rtn
*&---------------------------------------------------------------------*
*&      Form  SET_IT_PRICE_WITH_COMPONENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_IDNRK_IDNRK  text
*----------------------------------------------------------------------*
FORM set_it_price_with_component.
  EXEC SQL PERFORMING APPEND_STANDARD_PRICE.
    SELECT A.MATNR, A.MEINS, A.PROFL, D.KBETR, E.MAKTX,
           B.WERKS, F.EKORG, F.KZUST, F.WRBTR, F.WDUTY,
           F.WCOST, F.DMBTR, F.DDUTY, F.DCOST, F.WAERS,
           F.WKURS, F.LIFNR, G.WERKS, G.EKORG, G.KZUST,
           G.WRBTR, G.WDUTY, G.WCOST, G.DMBTR, G.DDUTY,
           G.DCOST, G.WAERS, G.WKURS, G.LIFNR, F.PERIOD,
           G.PERIOD,F.PEINH, G.PEINH, F.WFRGT, F.DFRGT,
           G.WFRGT, G.DFRGT
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
           :WA_PRICE-F_DFRGT
      FROM MARA A, ZTMM_ANALY_TMP H, MARC B, MAKT E, A902 C, KONP D,
           ZTMM_SPE F, ZTMM_ANALY G
     WHERE A.MANDT =  :SY-MANDT
       AND A.MATNR IN (SELECT IDNRK
                         FROM ZTMM_ANALY_TMP
                        WHERE MANDT  = :SY-MANDT
                          AND PERIOD = :WA_PERIOD
                          AND WERKS  = :P_WERKS
                          AND SUBMT  = ' ')
*       AND A.MTART     IN ('ROH', 'ROH1')
       AND A.PROFL     IN ('K',   'V')
       AND A.LVORM     <> 'X'
       AND H.MANDT  =  A.MANDT
       AND H.PERIOD =  :WA_PERIOD
       AND H.WERKS  =  :P_WERKS
       AND H.IDNRK  =  A.MATNR
       AND B.MANDT  =  A.MANDT
       AND B.MATNR  =  A.MATNR
       AND B.WERKS  =  H.WERKS_ACT
       AND B.LVORM  =  ' '
       AND C.MANDT(+)  =  B.MANDT
       AND C.KAPPL(+)  =  'M'
       AND C.KSCHL(+)  =  'ZOA1'
       AND C.STAWN(+)  =  B.STAWN
       AND D.MANDT(+)  =  C.MANDT
       AND D.KNUMH(+)  =  C.KNUMH
       AND E.MANDT     =  A.MANDT
       AND E.MATNR     =  A.MATNR
       AND E.SPRAS     =  :SY-LANGU
       AND F.MANDT(+)  =  B.MANDT
       AND F.PERIOD(+) =  :WA_PERIOD
       AND F.WERKS(+)  =  B.WERKS
       AND F.MATNR(+)  =  B.MATNR
       AND F.SUBMT(+)  =  ' '
       AND G.MANDT(+)  =  B.MANDT
       AND G.PERIOD(+) =  :WA_PERIOD
       AND G.WERKS(+)  =  B.WERKS
       AND G.MATNR(+)  =  B.MATNR
       AND G.SUBMT(+)  =  ' '
  ENDEXEC.

  READ TABLE it_price INDEX 1.
  IF sy-subrc NE 0.
    WRITE:/ text-m01.
    wa_error_flg = 'X'.
    EXIT.
*    MESSAGE e000(zz) WITH text-m01.
  ENDIF.

ENDFORM.                    " SET_IT_PRICE_WITH_COMPONENT
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
    INTO CORRESPONDING FIELDS OF TABLE it_knumh
    FROM a018
   WHERE kappl =  'M'
     AND kschl =  'PB00'
     AND matnr =  wa_price-matnr
     AND ekorg =  c_ekorg
     AND esokz =  '0'
     AND datab > sy-datum.

*----- Check Info Record Deletion Mark
  DATA: lw_matnr LIKE mara-matnr.
  LOOP AT it_knumh.
    SELECT SINGLE matnr
      INTO lw_matnr
      FROM eina AS a INNER JOIN eine AS b
        ON a~infnr = b~infnr
     WHERE a~matnr = wa_price-matnr
       AND a~lifnr = it_knumh-lifnr
       AND a~loekz = ' '
       AND b~werks = ' '
       AND b~ekorg = c_ekorg
       AND b~loekz = ' '.
    IF sy-subrc NE 0.
      DELETE it_knumh.
    ENDIF.
  ENDLOOP.

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
*&      Form  check_base_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_base_date.
  wa_base_date = sy-datum.
ENDFORM.                    " check_base_date
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
*------------------------------------------------*
*       FORM DYNPRO
*------------------------------------------------*
*  -->  DYNBEGIN
*  -->  NAME
*  -->  VALUE
*------------------------------------------------*
FORM dynpro USING dynbegin name value.
  IF dynbegin = 'X'.
    CLEAR:  bdc_tab.
    MOVE: name  TO bdc_tab-program,
          value TO bdc_tab-dynpro,
          'X'   TO bdc_tab-dynbegin.
    APPEND bdc_tab.
  ELSE.
    CLEAR:  bdc_tab.
    MOVE: name  TO bdc_tab-fnam,
          value TO bdc_tab-fval.
    APPEND bdc_tab.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  change_costing_lot_size
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_costing_lot_size.
  DATA: wa_head       LIKE bapimathead, "Header with control information
        wa_plant      LIKE bapi_marc  , "plant-specific material DATA
        wa_plantx     LIKE bapi_marcx ,
        wa_mbew       LIKE bapi_mbew  ,
        wa_mbewx      LIKE bapi_mbewx ,
        wa_amount     LIKE bseg-wrbtr,
        wa_peinh      LIKE ekpo-peinh.

  CHECK it_price-peinh > 1.

  CLEAR: it_bapiret2, it_bapiret2[].

  MOVE: it_price-dmamt  TO wa_amount,
        it_price-peinh  TO wa_peinh.

  wa_head-material     = it_price-matnr.
  wa_head-cost_view    = 'X'.
  wa_plant-lot_size    = wa_peinh.
  wa_plant-plant       = it_price-werks.
*  wa_mbew-val_area     = it_price-werks.
  wa_plantx-plant      = it_price-werks.
  wa_plantx-lot_size   = 'X'.
*  wa_mbewx-val_area    = it_price-werks.

  CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
       EXPORTING
            headdata       = wa_head
*            valuationdata  = wa_mbew
*            valuationdatax = wa_mbewx
            plantdata      = wa_plant
            plantdatax     = wa_plantx
       TABLES
            returnmessages = it_bapiret2.

  LOOP AT it_bapiret2 WHERE type = 'E'
                        AND type = 'A'.
  ENDLOOP.
  IF sy-subrc EQ 0.
    PERFORM master_update_failed_rtn.
  ELSE.
    PERFORM master_update_success.
  ENDIF.
ENDFORM.                    " change_costing_lot_size
*&---------------------------------------------------------------------*
*&      Form  change_excute_mr21
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_excute_mr21.
  PERFORM generate_bdc_table USING ' '.

  CALL TRANSACTION 'MR21' USING  bdc_tab
                          MODE   'N'
                          UPDATE 'S'.
  IF sy-subrc NE 0 OR sy-msgno NE '019'.
    IF sy-msgno EQ '811'.
      PERFORM generate_bdc_table USING c_mark.

      CALL TRANSACTION 'MR21' USING  bdc_tab
                              MODE   'N'
                              UPDATE 'S'.
      IF sy-subrc NE 0 OR sy-msgno NE '019'.
        CALL FUNCTION 'RKC_MSG_STRING'
             EXPORTING
                  id      = sy-msgid
                  mtype   = sy-msgty
                  number  = sy-msgno
                  par1    = sy-msgv1
                  par2    = sy-msgv2
                  par3    = sy-msgv3
                  par4    = sy-msgv4
             IMPORTING
                  msg_lin = it_price-msg.

        CONCATENATE c_step2 it_price-msg INTO it_price-msg
                    SEPARATED BY space.

        it_price-flag  = c_fail.
        it_price-eflag = 'E'.
      ELSE.
        CLEAR: it_price-msg, it_price-eflag.
        it_price-flag  = c_success.
        it_price-eflag = 'S'.
      ENDIF.
    ELSE.
      CALL FUNCTION 'RKC_MSG_STRING'
           EXPORTING
                id      = sy-msgid
                mtype   = sy-msgty
                number  = sy-msgno
                par1    = sy-msgv1
                par2    = sy-msgv2
                par3    = sy-msgv3
                par4    = sy-msgv4
           IMPORTING
                msg_lin = it_price-msg.

      CONCATENATE c_step2 it_price-msg INTO it_price-msg
                  SEPARATED BY space.

      it_price-flag  = c_fail.
      it_price-eflag = 'E'.
    ENDIF.
  ELSE.
    CLEAR: it_price-msg, it_price-eflag.
    it_price-flag  = c_success.
    it_price-eflag = 'S'.
  ENDIF.
ENDFORM.                    " change_excute_mr21
*&---------------------------------------------------------------------*
*&      Form  master_update_success
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM master_update_success.
  CLEAR: it_bapiret2.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
       EXPORTING
            wait   = 'X'
       IMPORTING
            return = it_bapiret2.

  IF it_bapiret2-type EQ 'E'.
    CONCATENATE c_step1 it_bapiret2-message INTO it_price-msg
                SEPARATED BY space.

    it_price-flag  = c_fail.
    it_price-eflag = 'E'.
  ELSE.
    CLEAR: it_price-msg.
    it_price-flag  = c_success.
    it_price-eflag = 'S'.
  ENDIF.
ENDFORM.                    " master_update_success
*&---------------------------------------------------------------------*
*&      Form  set_table_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_table_data.
  PERFORM set_ztmm_analy.
  PERFORM set_ztmm_spe.
ENDFORM.                    " set_table_data
*&---------------------------------------------------------------------*
*&      Form  set_ztmm_analy_new
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_ztmm_analy_new.
  CLEAR: ztmm_analy_new.

  MOVE: it_price-period  TO ztmm_analy_new-period,
        it_price-base_d  TO ztmm_analy_new-base_d,
        it_price-matnr   TO ztmm_analy_new-matnr,
        it_price-werks   TO ztmm_analy_new-werks,
        it_price-ekorg   TO ztmm_analy_new-ekorg,
        it_price-kzust   TO ztmm_analy_new-kzust,
        it_price-valid_d TO ztmm_analy_new-valid_d,
        it_price-wrbtr   TO ztmm_analy_new-wrbtr,
        it_price-wduty   TO ztmm_analy_new-wduty,
        it_price-wfrgt   TO ztmm_analy_new-wfrgt,
        it_price-wcost   TO ztmm_analy_new-wcost,
        it_price-dmbtr   TO ztmm_analy_new-dmbtr,
        it_price-dduty   TO ztmm_analy_new-dduty,
        it_price-dfrgt   TO ztmm_analy_new-dfrgt,
        it_price-dcost   TO ztmm_analy_new-dcost,
        it_price-peinh   TO ztmm_analy_new-peinh,
        it_price-waers   TO ztmm_analy_new-waers,
        it_price-wkurs   TO ztmm_analy_new-wkurs,
        it_price-source  TO ztmm_analy_new-source,
        it_price-lifnr   TO ztmm_analy_new-lifnr,
        it_price-eflag   TO ztmm_analy_new-eflag,
        it_price-msg     TO ztmm_analy_new-msg,
        sy-uname         TO ztmm_analy_new-aenam,
        sy-datum         TO ztmm_analy_new-aedat,
        sy-uzeit         TO ztmm_analy_new-aezet,
        sy-uname         TO ztmm_analy_new-ernam,
        sy-datum         TO ztmm_analy_new-erdat,
        sy-uzeit         TO ztmm_analy_new-erzet.
ENDFORM.                    " set_ztmm_analy_new
*&---------------------------------------------------------------------*
*&      Form  set_ztmm_analy
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_ztmm_analy.
  CLEAR: ztmm_analy.

  CHECK it_price-eflag EQ 'S'.

  MOVE: it_price-period  TO ztmm_analy-period,
        it_price-base_d  TO ztmm_analy-base_d,
        it_price-matnr   TO ztmm_analy-matnr,
        it_price-werks   TO ztmm_analy-werks,
        'X'              TO ztmm_analy-newmt,
        it_price-ekorg   TO ztmm_analy-ekorg,
        it_price-kzust   TO ztmm_analy-kzust,
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
ENDFORM.                    " set_ztmm_analy
*&---------------------------------------------------------------------*
*&      Form  set_ztmm_spe_new
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_ztmm_spe.
  CLEAR: ztmm_spe, ztmm_spe_new.

  CHECK it_price-eflag EQ 'S'.

  SELECT SINGLE * FROM ztmm_spe_new WHERE period = it_price-period
                                      AND werks  = it_price-werks
                                      AND matnr  = it_price-matnr.
  IF sy-subrc EQ 0.
    MOVE-CORRESPONDING ztmm_spe_new TO ztmm_spe.
    MOVE: 'X' TO ztmm_spe-newmt.
  ENDIF.
ENDFORM.                    " set_ztmm_spe_new
*&---------------------------------------------------------------------*
*&      Form  update_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_table.
  IF it_price-eflag EQ 'S'.
    INSERT ztmm_analy.
    IF sy-subrc NE 0.
      it_price-flag = c_fail.
      MOVE: 'E'      TO it_price-eflag.
      CONCATENATE c_step3 text-b08 INTO it_price-msg
                  SEPARATED BY space.
    ELSE.
      CLEAR: it_price-msg, it_price-eflag.
      it_price-flag  = c_success.
      it_price-eflag = 'S'.
    ENDIF.
  ENDIF.

  IF ztmm_spe-period NE '000000' AND
     it_price-eflag EQ 'S'.
    INSERT ztmm_spe.
    IF sy-subrc NE 0.
      ROLLBACK WORK.
      it_price-flag = c_fail.
      MOVE: 'E' TO it_price-eflag.
      CONCATENATE c_step3 text-b09 INTO it_price-msg
                  SEPARATED BY space.
    ELSE.
      CLEAR: it_price-msg, it_price-eflag.
      it_price-flag  = c_success.
      it_price-eflag = 'S'.
    ENDIF.
  ENDIF.

  PERFORM set_ztmm_analy_new.
  MODIFY ztmm_analy_new.
  IF sy-subrc NE 0.
    ROLLBACK WORK.
    it_price-flag = c_fail.
    MOVE: 'E'      TO it_price-eflag.
    CONCATENATE c_step3 text-b10 INTO it_price-msg
                  SEPARATED BY space.
    wa_count_fail  = wa_count_fail + 1.
    EXIT.
  ELSE.
    COMMIT WORK AND WAIT.
    IF it_price-eflag EQ 'S'.
      it_price-flag   = c_success.
      wa_count_finish = wa_count_finish + 1.
    ELSE.
      it_price-eflag  = 'E'.
      it_price-flag   = c_fail.
      wa_count_fail   = wa_count_fail + 1.
    ENDIF.
  ENDIF.
ENDFORM.                    " update_table
*&---------------------------------------------------------------------*
*&      Form  get_reprocessing_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_reprocessing_data.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_ztmm_analy_new
    FROM ztmm_analy_new AS a INNER JOIN mara AS b
      ON a~matnr = b~matnr
                             INNER JOIN makt AS c
      ON a~matnr = c~matnr
   WHERE a~matnr BETWEEN wa_matnr_f and wa_matnr_t
     and A~WERKS = P_WERKS
     AND a~eflag = 'E'
     AND c~spras = sy-langu.
  IF sy-subrc NE 0.
    MESSAGE s000(zz) WITH text-m01.
    EXIT.
  ENDIF.

  LOOP AT it_ztmm_analy_new.
    it_ztmm_analy_new-wramt = it_ztmm_analy_new-wrbtr +
                              it_ztmm_analy_new-wduty +
                              it_ztmm_analy_new-wfrgt +
                              it_ztmm_analy_new-wcost.

    it_ztmm_analy_new-dmamt = it_ztmm_analy_new-dmbtr +
                              it_ztmm_analy_new-dduty +
                              it_ztmm_analy_new-dfrgt +
                              it_ztmm_analy_new-dcost.

    CASE it_ztmm_analy_new-profl.
      WHEN 'K'.
        it_ztmm_analy_new-type = 'KD'.
      WHEN 'V'.
        it_ztmm_analy_new-type = 'LP'.
    ENDCASE.

    MODIFY it_ztmm_analy_new.
  ENDLOOP.

  it_price[] = it_ztmm_analy_new[].
ENDFORM.                    " get_reprocessing_data
*&---------------------------------------------------------------------*
*&      Form  APPEND_reprocess_PRICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_reprocess_price.
  CLEAR: it_price.

  MOVE: wa_period          TO it_price-period,
        wa_base_date       TO it_price-base_d,
        wa_price-matnr     TO it_price-matnr,
        wa_price-profl     TO it_price-profl,
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
        wa_price-f_source  TO it_price-source,
        wa_price-f_lifnr   TO it_price-lifnr.

  MOVE: c_ready TO it_price-flag.
  IF wa_price-stprs EQ 0.
    MOVE: c_cost_fail        TO it_price-flag.
    wa_count_cost = wa_count_cost + 1.
  ELSE.
    MOVE: c_success          TO it_price-flag.
    wa_count_finish = wa_count_finish + 1.
  ENDIF.

  it_price-wramt = it_price-wrbtr + it_price-wduty +
                   it_price-wfrgt + it_price-wcost.
  it_price-dmamt = it_price-dmbtr + it_price-dduty +
                   it_price-dfrgt + it_price-dcost.

  APPEND it_price.

  CLEAR: wa_price.
ENDFORM.                    " APPEND_reprocess_PRICE
*&---------------------------------------------------------------------*
*&      Form  NORMAL_PROCESSING_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM normal_processing_rtn.
  PERFORM check_rtn.
  CHECK wa_error_flg IS INITIAL.
  PERFORM get_data.

  CHECK wa_error_flg IS INITIAL.
  PERFORM excute_rtn.

  PERFORM error_save_rtn.
ENDFORM.                    " NORMAL_PROCESSING_RTN
*&---------------------------------------------------------------------*
*&      Form  REPROCESSING_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reprocessing_rtn.
  DATA: lw_step.                       "Processing Step

  PERFORM get_reprocessing_data.

  LOOP AT it_price.
    CLEAR: wa_errsts,it_price-eflag.


    CASE it_price-msg(5).
      WHEN c_step1.
        PERFORM excute_step1_processing.
      WHEN c_step2.
        PERFORM excute_step2_processing.
      WHEN c_step3.
        PERFORM excute_step3_processing.
    ENDCASE.

    MODIFY it_price.
  ENDLOOP.
ENDFORM.                    " REPROCESSING_RTN
*&---------------------------------------------------------------------*
*&      Form  excute_step2_processing
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM excute_step2_processing.
  PERFORM change_excute_mr21.
  PERFORM set_table_data.
  PERFORM update_table.
ENDFORM.                    " excute_step2_processing
*&---------------------------------------------------------------------*
*&      Form  excute_step1_processing
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM excute_step1_processing.
  PERFORM change_costing_lot_size.
  PERFORM change_excute_mr21.
  PERFORM set_table_data.
  PERFORM update_table.
ENDFORM.                    " excute_step1_processing
*&---------------------------------------------------------------------*
*&      Form  excute_step3_processing
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM excute_step3_processing.
  CLEAR: it_price-msg.
  it_price-eflag = 'S'.
  it_price-flag  = c_success.

  PERFORM set_table_data.
  PERFORM update_table.
ENDFORM.                    " excute_step3_processing
*&---------------------------------------------------------------------*
*&      Form  check_future_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_future_price.
*----- Check Undetermined Price
  CLEAR: zvmm_info_condi.
  SELECT SINGLE *
    FROM zvmm_info_condi
   WHERE knumh    = it_knumh-knumh
     AND kschl    = c_kschl
     AND loevm_ko = ' '.
  IF sy-subrc NE 0.
    PERFORM append_condition_price.
  ENDIF.

  SELECT SINGLE *
   FROM a018
  WHERE kappl =  'M'
    AND kschl =  'PB00'
    AND matnr =  wa_price-matnr
    AND lifnr =  it_knumh-lifnr
    AND ekorg =  c_ekorg
    AND esokz =  '0'
    AND datab > sy-datum.
  IF sy-subrc EQ 0.
    PERFORM read_future_price.
  ELSE.
    PERFORM append_condition_price.
  ENDIF.
ENDFORM.                    " check_future_price
*&---------------------------------------------------------------------*
*&      Form  generate_bdc_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_4342   text
*----------------------------------------------------------------------*
FORM generate_bdc_table USING p_check.
  DATA: lw_budat(10),
        lw_dmamt(15),
        lw_peinh(5),
        lw_bidat LIKE sy-datum.

  CHECK it_price-eflag NE 'E'.

  WRITE: it_price-base_d TO lw_budat,
         it_price-dmamt CURRENCY c_waers TO lw_dmamt,
         it_price-peinh  TO lw_peinh.


  REFRESH bdc_tab.
  PERFORM dynpro USING:
        'X' 'SAPRCKM_MR21'              '0201',
        ' ' 'MR21HEAD-BUDAT'             lw_budat,
        ' ' 'MR21HEAD-BUKRS'             c_bukrs,
        ' ' 'MR21HEAD-WERKS'             it_price-werks,
        ' ' 'BDC_OKCODE'                 '=ENTR'.

  IF p_check EQ c_mark.
    PERFORM dynpro USING:
          'X' 'SAPRCKM_MR21'              '0201',
          ' ' 'BDC_OKCODE'                 '=LTPC'.
  ENDIF.

  PERFORM dynpro USING:
        'X' 'SAPRCKM_MR21'              '0201',
        ' ' 'CKI_MR21_0250-MATNR(01)'    it_price-matnr,
        ' ' 'CKI_MR21_0250-NEWVALPR(01)' lw_dmamt,
        ' ' 'CKI_MR21_0250-NEWPEINH(01)' lw_peinh,
        ' ' 'BDC_OKCODE'                 '=SAVE'.
ENDFORM.                    " generate_bdc_table
