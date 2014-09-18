************************************************************************
* Program Name      : ZEMMPM34R_ST_PRICE_MANAGE
* Author            : Byung-sung, Bae
* Creation Date     : 2003.10.16.
* Specifications By : Byung-sung, Bae
* Pattern           : Report 1-1
* Development Request No : UD1K906309
* Addl Documentation:
* Description       : Master Inspection Characteristic Uploading
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT zemmpm34r_st_price_manage NO STANDARD PAGE HEADING
                                 LINE-SIZE  130
                                 LINE-COUNT  65.
INCLUDE: <icon>.
TYPE-POOLS : slis.
TABLES: mara,
        ztmm_analy,
        ztmm_spe.

*----- Internal tables
DATA: BEGIN OF it_mara OCCURS 0,               "Target Material
        matnr   LIKE   mara-matnr,
        profl   LIKE   mara-profl,
      END   OF it_mara.

DATA: BEGIN OF it_price OCCURS 0.
        INCLUDE STRUCTURE ztmm_analy.
DATA:   wramt LIKE ztmm_analy-wrbtr,
        dmamt LIKE ztmm_analy-wrbtr,
        maktx LIKE makt-maktx,
        profl LIKE mara-profl,
        check,
        flag,
        msg(100),
      END   OF it_price.

DATA: it_temp_price LIKE it_price OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF it_vakey OCCURS 0,
        vakey LIKE konh-vakey,
        datab LIKE konh-datab,
      END   OF it_vakey.

DATA: BEGIN OF it_info OCCURS 0,                "Info Condition
          kbetr   LIKE   konp-kbetr,
          wrbtr   TYPE   f,
          datab   LIKE   konh-datab,
          kpein   LIKE   konp-kpein,
          kumne   LIKE   konp-kumne,
          kumza   LIKE   konp-kumza,
          konwa   LIKE   konp-konwa,
          kzust   LIKE   konh-kzust,
          vakey   LIKE   konh-vakey,
          witax   LIKE   konp-kbetr,
          wcost   LIKE   konp-kbetr,
          meins   LIKE   mara-meins,
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
        maktx      LIKE makt-maktx,          "Description
        werks      LIKE ztmm_analy-werks,    "Error Plant
        ekorg      LIKE ztmm_analy-ekorg,    "Error Purchase Org.
        kzust      LIKE ztmm_analy-kzust,    "Error Reason code
        valid_d    LIKE ztmm_analy-valid_d,  "Error Valid date
        wrbtr      LIKE ztmm_analy-wrbtr,    "Error Foreign Pice
        wduty      LIKE ztmm_analy-wrbtr,    "Error duty of F.Curr
        wcost      LIKE ztmm_analy-wrbtr,    "Error cost of F.Curr
        dmbtr      LIKE ztmm_analy-wrbtr,    "Error Local Price
        dduty      LIKE ztmm_analy-wrbtr,    "Error duty of L.Curr
        dcost      LIKE ztmm_analy-wrbtr,    "Error cost of L.Curr
        wramt      LIKE ztmm_analy-wrbtr,    "Error Foreign Amount
        dmamt      LIKE ztmm_analy-wrbtr,    "Error Local Amount
        waers      LIKE ztmm_analy-waers,    "Error Currency
        wkurs      LIKE ztmm_analy-wkurs,    "Error exchange rate
        lifnr      LIKE ztmm_analy-lifnr,    "Error vendor
        f_werks    LIKE ztmm_analy-werks,    "Finish Plant
        f_ekorg    LIKE ztmm_analy-ekorg,    "Finish Purchase Org.
        f_kzust    LIKE ztmm_analy-kzust,    "Finish Reason code
        f_valid_d  LIKE ztmm_analy-valid_d,  "Finish Valid date
        f_wrbtr    LIKE ztmm_analy-wrbtr,    "Finish Foreign Pice
        f_wduty    LIKE ztmm_analy-wrbtr,    "Finish duty of F.Curr
        f_wcost    LIKE ztmm_analy-wrbtr,    "Finish cost of F.Curr
        f_dmbtr    LIKE ztmm_analy-wrbtr,    "Finish Local Price
        f_dduty    LIKE ztmm_analy-wrbtr,    "Finish duty of L.Curr
        f_dcost    LIKE ztmm_analy-wrbtr,    "Finish cost of L.Curr
        f_wramt    LIKE ztmm_analy-wrbtr,    "Finish Foreign Amount
        f_dmamt    LIKE ztmm_analy-wrbtr,    "Finish Local Amount
        f_waers    LIKE ztmm_analy-waers,    "Finish Currency
        f_wkurs    LIKE ztmm_analy-wkurs,    "Finish exchange rate
        f_lifnr    LIKE ztmm_analy-lifnr,    "Finish vendor
      END   OF wa_price.

DATA: wa_amount       TYPE   f,                "Amount of Base UoM
      wa_low_amt      TYPE   f,                "lower amount of Base UoM
      wa_matnr_f      LIKE   mara-matnr,       "Material From
      wa_matnr_t      LIKE   mara-matnr,       "Material To
*      wa_datum        LIKE   sy-datum,         "Base date
      wa_vakey        LIKE   konh-vakey,       "Value Key
      wa_progress_idx TYPE   i,                "Progress bar index
      wa_count        TYPE   i,                "Count of Material
      wa_count_ready  TYPE   i,                "Count of ready
      wa_count_finish TYPE   i,                "Count of finished
      wa_count_fail   TYPE   i,                "Count of failed
      wa_count_error  TYPE   i,                "Count of error
      wa_count_run    TYPE   i.                "Count of running item

CONSTANTS: c_mark                   VALUE 'X',   "Marker
           c_ekorg LIKE ekko-ekorg  VALUE 'PU01',"Purchase Org.
           c_kschl LIKE konp-kschl  VALUE 'PB00',"Type of amount
           c_con01 LIKE konp-kschl  VALUE 'FRA1',"Type of freight
           c_con02 LIKE konp-kschl  VALUE 'ZOTH',"Type of ETC rate
           c_con03 LIKE konp-kschl  VALUE 'ZOTI',"Type of ETC rate
           c_waers LIKE ekko-waers  VALUE 'USD', "Currency
           c_price LIKE konp-kbetr  VALUE '0.01',"Default Price
           c_ready                  VALUE '1',   "Ready item
           c_fail                   VALUE '2',   "Update failed item
           c_success                VALUE '3',   "Update successful item
           c_error                  VALUE '4'.   "Error item

*----- Selection screens
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-t01.
PARAMETERS:     p_werks LIKE t001w-werks DEFAULT 'P001' OBLIGATORY.
*PARAMETERS:     p_year  LIKE sy-datum(4) OBLIGATORY.
SELECT-OPTIONS: s_matnr FOR  mara-matnr NO-EXTENSION.
PARAMETERS:     p_datum LIKE sy-datum OBLIGATORY.
SELECTION-SCREEN END   OF BLOCK bl1.

SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE text-t02.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS       r_year RADIOBUTTON GROUP rd1 DEFAULT 'X'
                                              USER-COMMAND rd1.
SELECTION-SCREEN COMMENT  3(25) text-t03 FOR FIELD r_year.
SELECTION-SCREEN POSITION 40.
PARAMETERS       r_quar RADIOBUTTON GROUP rd1.
SELECTION-SCREEN COMMENT 42(25) text-t04 FOR FIELD r_quar.
SELECTION-SCREEN END OF LINE.

*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN COMMENT 1(79) text-t10.
*SELECTION-SCREEN END   OF LINE.

*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN POSITION 1.
*PARAMETERS       r_quar_1 RADIOBUTTON GROUP rd2.
*SELECTION-SCREEN COMMENT 3(15) text-t06 FOR FIELD r_quar_1.
*SELECTION-SCREEN POSITION 20.
*PARAMETERS       r_quar_2 RADIOBUTTON GROUP rd2.
*SELECTION-SCREEN COMMENT 22(15) text-t07 FOR FIELD r_quar_2.
*SELECTION-SCREEN POSITION 40.
*PARAMETERS       r_quar_3 RADIOBUTTON GROUP rd2.
*SELECTION-SCREEN COMMENT 42(15) text-t08 FOR FIELD r_quar_3.
*SELECTION-SCREEN POSITION 60.
*PARAMETERS       r_quar_4 RADIOBUTTON GROUP rd2.
*SELECTION-SCREEN COMMENT 62(15) text-t09 FOR FIELD r_quar_4.
*SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END   OF BLOCK bl2.

*SELECTION-SCREEN BEGIN OF BLOCK bl3 WITH FRAME TITLE text-t11.
*PARAMETERS:     p_nodisp AS CHECKBOX.
*PARAMETERS:     p_newmat AS CHECKBOX.
*SELECTION-SCREEN END   OF BLOCK bl3.

*----- Screen Attribute Control
*AT SELECTION-SCREEN OUTPUT.
*  PERFORM set_screen_attribute.
*  PERFORM set_quarter.                                   " Set Quarter

*----- Initial value
INITIALIZATION.
  PERFORM set_initial_value.

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
*&      Form  SET_INITIAL_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_initial_value.
  PERFORM set_spmon.        " Set Period
ENDFORM.                    " SET_INITIAL_VALUE
*&---------------------------------------------------------------------*
*&      Form  SET_SPMON
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_spmon.
*  p_year = sy-datum(4).
ENDFORM.                    " SET_SPMON
*&---------------------------------------------------------------------*
*&      Form  set_auarter
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_quarter.
*  CHECK NOT ( r_quar_1 EQ 'X' OR r_quar_2 EQ 'X' OR
*              r_quar_3 EQ 'X' OR r_quar_4 EQ 'X' ).
*
*  IF r_year = 'X'.
*    CLEAR: r_quar_1, r_quar_2, r_quar_3, r_quar_4.
*  ELSEIF r_quar = 'X'.
*    CASE sy-datum+4(2).
*      WHEN '01' OR '02' OR '03'.
*        r_quar_1 = 'X'.
*      WHEN '04' OR '05' OR '06'.
*        r_quar_2 = 'X'.
*      WHEN '07' OR '08' OR '09'.
*        r_quar_3 = 'X'.
*      WHEN '10' OR '12' OR '12'.
*        r_quar_4 = 'X'.
*    ENDCASE.
*  ENDIF.
ENDFORM.                    " set_auarter
*&---------------------------------------------------------------------*
*&      Form  set_screen_attribute
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_screen_attribute.
*  LOOP AT SCREEN.
*    CASE c_mark.
*      WHEN r_year.
*        IF screen-name = 'R_QUAR_1' OR screen-name = 'R_QUAR_2' OR
*           screen-name = 'R_QUAR_3' OR screen-name = 'R_QUAR_4'.
*          screen-input = '0'.
*        ENDIF.
*      WHEN r_quar.
*        IF screen-name = 'R_QUAR_1' OR screen-name = 'R_QUAR_2' OR
*           screen-name = 'R_QUAR_3' OR screen-name = 'R_QUAR_4'.
*          screen-input = '1'.
*        ENDIF.
*    ENDCASE.
*    MODIFY SCREEN.
*  ENDLOOP.
ENDFORM.                    " set_screen_attribute
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

  PERFORM read_total_count.

  EXEC SQL PERFORMING APPEND_STANDARD_PRICE.
    SELECT A.MATNR, A.MEINS, A.PROFL, D.KBETR, E.MAKTX,
           F.WERKS, F.EKORG, F.KZUST, F.WRBTR, F.WDUTY,
           F.WCOST, F.DMBTR, F.DDUTY, F.DCOST, F.WAERS,
           F.WKURS, F.LIFNR, G.WERKS, G.EKORG, G.KZUST,
           G.WRBTR, G.WDUTY, G.WCOST, G.DMBTR, G.DDUTY,
           G.DCOST, G.WAERS, G.WKURS, G.LIFNR
      INTO :WA_PRICE-MATNR,   :WA_PRICE-MEINS,   :WA_PRICE-PROFL,
           :WA_PRICE-WITAX,   :WA_PRICE-MAKTX,   :WA_PRICE-WERKS,
           :WA_PRICE-EKORG,   :WA_PRICE-KZUST,   :WA_PRICE-WRBTR,
           :WA_PRICE-WDUTY,   :WA_PRICE-WCOST,   :WA_PRICE-DMBTR,
           :WA_PRICE-DDUTY,   :WA_PRICE-DCOST,   :WA_PRICE-WAERS,
           :WA_PRICE-WKURS,   :WA_PRICE-LIFNR,   :WA_PRICE-F_WERKS,
           :WA_PRICE-F_EKORG, :WA_PRICE-F_KZUST, :WA_PRICE-F_WRBTR,
           :WA_PRICE-F_WDUTY, :WA_PRICE-F_WCOST, :WA_PRICE-F_DMBTR,
           :WA_PRICE-F_DDUTY, :WA_PRICE-F_DCOST, :WA_PRICE-F_WAERS,
           :WA_PRICE-F_WKURS, :WA_PRICE-F_LIFNR
      FROM MARA A, MARC B, MAKT E, A902 C, KONP D, ZTMM_SPE F,
           ZTMM_ANALY G
     WHERE A.MANDT = :SY-MANDT
       AND A.MATNR     BETWEEN :WA_MATNR_F AND :WA_MATNR_T
       AND A.MTART     IN ('ROH', 'ROH1')
       AND A.PROFL     IN ('K',   'V')
       AND A.LVORM     <> 'X'
       AND B.MANDT(+)  =  A.MANDT
       AND B.MATNR(+)  =  A.MATNR
       AND B.WERKS(+)  =  :P_WERKS
       AND B.LVORM(+)  =  ' '
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
       AND F.BASE_D(+) =  :P_DATUM
       AND F.MATNR(+)  =  A.MATNR
       AND G.MANDT(+)  =  A.MANDT
       AND G.BASE_D(+) =  :P_DATUM
       AND G.MATNR(+)  =  A.MATNR
  ENDEXEC.

  READ TABLE it_price INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m01.
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

  PERFORM display_progress_bar.

  IF wa_price-f_wrbtr NE 0.
    PERFORM append_finish_price.
    CLEAR: wa_price.
    CHECK 1 = 0.
  ENDIF.

  IF wa_price-wrbtr NE 0.
    PERFORM append_ready_price_for_error.
    CLEAR: wa_price.
    CHECK 1 = 0.
  ENDIF.

  MOVE: '%'            TO wa_vakey,
        wa_price-matnr TO wa_vakey+1,
        c_ekorg        TO wa_vakey+19,
        '0'            TO wa_vakey+23.

*----- Read the lastest Value key per vendor
  CLEAR: it_vakey, it_vakey[].
  SELECT vakey MAX( datab ) AS datab
    INTO CORRESPONDING FIELDS OF TABLE it_vakey
    FROM konh
   WHERE vakey LIKE wa_vakey
     AND datab <=   p_datum
     AND kschl =    'PB00'
     AND ( kzust BETWEEN '000' and 'WZZ' or
           KZUST between 'Y00' AND 'ZZZ' OR
           kzust LIKE    'X%' )
   GROUP BY vakey.

*----- Read the lowest price
  READ TABLE it_vakey INDEX 1.
  IF sy-subrc EQ 0.                 "Standard Price
    PERFORM append_condition_price.
  ELSE.                             "Temp price, No price
    PERFORM append_blank_price.
*    CLEAR: it_vakey, it_vakey[].
*    SELECT vakey MAX( datab ) AS datab
*      INTO CORRESPONDING FIELDS OF TABLE it_vakey
*      FROM konh
*     WHERE vakey LIKE wa_vakey
*       AND datab <= wa_datum
*       AND kschl =  c_kschl
*       AND kzust LIKE 'X%'
*     GROUP by vakey.
*
*    READ TABLE it_vakey INDEX 1.
*    IF sy-subrc EQ 0.                 "Temp price
*      PERFORM append_condition_price.
*    ELSE.                             "Price = '0.01'
*      PERFORM append_blank_price.
*    ENDIF.
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
        lw_umren LIKE eina-umren.        "Denomirator

  IF it_info-meins EQ wa_price-meins.    "Info UoM =  Master UoM
    pwa_amount = ( it_info-kbetr / it_info-kpein /
                   it_info-kumne * it_info-kumza )
               + ( it_info-kbetr / it_info-kpein /
                   it_info-kumne * it_info-kumza ) *
                   it_info-witax / 1000
               + ( it_info-kbetr / it_info-kpein /
                   it_info-kumne * it_info-kumza ) *
                   it_info-wcost / 1000.
    it_info-wrbtr = ( it_info-kbetr / it_info-kpein /
                      it_info-kumne * it_info-kumza ).
  ELSE.                                    "Info UoM <> Master UoM
    SELECT SINGLE umrez umren
      INTO (lw_umrez, lw_umren)
      FROM eina
     WHERE matnr = wa_price-matnr
       AND lifnr = wa_vakey(10).
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH text-m02.
    ENDIF.

    pwa_amount = ( it_info-kbetr / it_info-kpein /
                   it_info-kumne * it_info-kumza /
                   lw_umrez      * lw_umren      )
               + ( it_info-kbetr / it_info-kpein /
                   it_info-kumne * it_info-kumza /
                   lw_umrez      * lw_umren      ) *
                   it_info-witax / 1000
               + ( it_info-kbetr / it_info-kpein /
                   it_info-kumne * it_info-kumza /
                   lw_umrez      * lw_umren      ) *
                   it_info-wcost / 1000.
    it_info-wrbtr = ( it_info-kbetr / it_info-kpein /
                      it_info-kumne * it_info-kumza /
                      lw_umrez      * lw_umren      ).
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
  PERFORM check_date.
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
ENDFORM.                    " check_matnr
*&---------------------------------------------------------------------*
*&      Form  check_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_date.
*  CASE c_mark.
*    WHEN r_year.
*      CONCATENATE p_year '0101' INTO wa_datum.
*    WHEN r_quar.
*      IF     r_quar_1 EQ 'X'.
*        CONCATENATE p_year '0101' INTO wa_datum.
*      ELSEIF r_quar_2 EQ 'X'.
*        CONCATENATE p_year '0401' INTO wa_datum.
*      ELSEIF r_quar_3 EQ 'X'.
*        CONCATENATE p_year '0701' INTO wa_datum.
*      ELSEIF r_quar_4 EQ 'X'.
*        CONCATENATE p_year '1001' INTO wa_datum.
*      ENDIF.
*  ENDCASE.
ENDFORM.                    " check_date
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

  IF wa_price-wrbtr IS INITIAL.
    MOVE: p_datum        TO it_price-base_d,
          wa_price-matnr TO it_price-matnr,
          wa_price-profl TO it_price-profl,
          wa_price-maktx TO it_price-maktx,
          ' '            TO it_price-ekorg,
          ' '            TO it_price-kzust,
          p_datum        TO it_price-valid_d,
          '0.01'         TO it_price-wramt,
          '0.01'         TO it_price-wrbtr,
          0              TO it_price-wcost,
          '0.01'         TO it_price-dmamt,
          '0.01'         TO it_price-dmbtr,
          0              TO it_price-dcost,
          c_waers        TO it_price-waers,
          1              TO it_price-wkurs,
          'M'            TO it_price-source,
          ''             TO it_price-lifnr,
          c_error        TO it_price-flag.

    APPEND it_price.

    wa_count_error = wa_count_error + 1.
  ELSE.
    MOVE: p_datum          TO it_price-base_d,
          wa_price-matnr   TO it_price-matnr,
          wa_price-profl   TO it_price-profl,
          wa_price-maktx   TO it_price-maktx,
          wa_price-werks   TO it_price-werks,
          wa_price-ekorg   TO it_price-ekorg,
          wa_price-kzust   TO it_price-kzust,
          wa_price-valid_d TO it_price-valid_d,
          wa_price-wrbtr   TO it_price-wrbtr,
          wa_price-wcost   TO it_price-wcost,
          wa_price-dmbtr   TO it_price-dmbtr,
          wa_price-dcost   TO it_price-dcost,
          wa_price-waers   TO it_price-waers,
          wa_price-wkurs   TO it_price-wkurs,
          'M'              TO it_price-source,
          wa_price-lifnr   TO it_price-lifnr,
          c_ready          TO it_price-flag.

    it_price-wramt = it_price-wrbtr + it_price-wduty + it_price-wcost.
    it_price-dmamt = it_price-dmbtr + it_price-dduty + it_price-dcost.

    APPEND it_price.
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
FORM read_exchange_rate.
  DATA: lw_wkurs TYPE f.


  READ TABLE it_exchange_rate WITH KEY waers = it_price-waers.
  IF sy-subrc NE 0.
    CALL FUNCTION 'Z_FCA_GET_EXCHANGE_RATE'
         EXPORTING
              client                  = sy-mandt
              date                    = p_datum
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
          lw_wkurs       TO it_exchange_rate-wkurs.

    APPEND it_exchange_rate.
  ENDIF.
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
*----- Read the lowest price per vendor

  CLEAR: it_zvmm_info_condi, it_zvmm_info_condi[].
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_zvmm_info_condi
    FROM zvmm_info_condi
     FOR ALL ENTRIES IN it_vakey
   WHERE vakey = it_vakey-vakey
     AND datab = it_vakey-datab
     AND kschl IN (c_kschl,c_con01,c_con02,c_con03)
     AND loevm_ko = ' '.
  IF sy-subrc NE 0.
    PERFORM append_blank_price.
    pw_error_flg = 'X'.
  ENDIF.
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

    ON CHANGE OF it_zvmm_info_condi-vakey.
      MOVE-CORRESPONDING it_zvmm_info_condi TO it_info.
      IF wa_price-profl EQ 'K'.
        MOVE: wa_price-witax TO it_info-witax.
      ENDIF.
      APPEND it_info.
      CONTINUE.
    ENDON.

    READ TABLE it_info WITH KEY vakey = it_zvmm_info_condi-vakey.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH text-m02.
    ENDIF.

    IF wa_price-profl EQ 'K'.
      it_info-wcost = it_info-wcost + it_zvmm_info_condi-kbetr.
    ENDIF.

    MODIFY it_info INDEX sy-tabix.
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

  CLEAR: it_price.

  MOVE: p_datum             TO it_price-base_d,
        wa_price-matnr      TO it_price-matnr,
        wa_price-profl      TO it_price-profl,
        p_werks             TO it_price-werks,
        wa_price-maktx      TO it_price-maktx,
        it_info-kzust       TO it_price-kzust,
        it_info-datab       TO it_price-valid_d,
        it_info-wrbtr       TO it_price-wrbtr,
        it_info-konwa       TO it_price-waers,
        1                   TO it_price-wkurs,
        'I'                 TO it_price-source,
        it_info-vakey(10)   TO it_price-lifnr,
        it_info-vakey+28(4) TO it_price-ekorg.

  it_price-wcost = it_price-wrbtr * it_info-wcost / 1000.
  it_price-wduty = it_price-wrbtr * it_info-witax / 1000.
  it_price-wramt = it_price-wrbtr + it_price-wcost +
                   it_price-wduty.

  PERFORM read_exchange_rate.

  it_price-dmbtr = it_price-wrbtr * it_exchange_rate-wkurs.
  it_price-dduty = it_price-wduty * it_exchange_rate-wkurs.
  it_price-dcost = it_price-wcost * it_exchange_rate-wkurs.
  it_price-dmamt = it_price-dmbtr + it_price-dcost +
                   it_price-dduty.


  CASE wa_price-kzust(1).
    WHEN 'X'.
      MOVE: c_error             TO it_price-flag.
      wa_count_error = wa_count_error + 1.
    WHEN OTHERS.
      MOVE: c_ready             TO it_price-flag.
      wa_count_ready = wa_count_ready + 1.
  ENDCASE.

  APPEND it_price.

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

  WRITE:/01(130) text-h06 CENTERED.
  WRITE:/01(130) text-h07 CENTERED.

  NEW-LINE.

  WRITE:/09    text-h09,
         25(7) wa_count.

  WRITE:/02(4) icon_yellow_light AS ICON,
               text-h03,
         25(7) wa_count_ready.

  WRITE: 40(4) icon_green_light  AS ICON,
               text-h04,
         63(7) wa_count_finish.

  WRITE:/02(4) icon_red_light    AS ICON,
               text-h05,
         25(7) wa_count_fail.

  WRITE: 40(4) icon_light_out    AS ICON,
               text-h08,
         63(7) wa_count_error.

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

  SORT it_price BY flag base_d matnr.

  LOOP AT it_price.
    RESERVE 3 LINES.

    PERFORM set_format_color USING lw_color_flg.

    IF it_price-flag EQ c_error OR
       it_price-flag EQ c_success.
      WRITE:/ it_price-check AS CHECKBOX INPUT OFF.
    ELSE.
      WRITE:/ it_price-check AS CHECKBOX.
    ENDIF.

    WRITE:       it_price-base_d,
            (18) it_price-matnr,
                 it_price-valid_d,
                 it_price-kzust,
                 it_price-waers,
                 it_price-wramt,
                 it_price-wrbtr,
                 it_price-wduty,
                 it_price-wcost.

    HIDE: it_price, sy-tabix.

    CASE it_price-flag.
      WHEN c_ready.
        WRITE: /10(4) icon_yellow_light AS ICON.
      WHEN c_fail.
        WRITE: /10(4) icon_red_light    AS ICON HOTSPOT.
      WHEN c_success.
        WRITE: /10(4) icon_green_light  AS ICON.
      WHEN c_error.
        WRITE: /10(4) icon_light_out    AS ICON HOTSPOT.
    ENDCASE.

    WRITE: 14(39) it_price-maktx,
                  it_price-dmamt,
                  it_price-dmbtr,
                  it_price-dduty,
                  it_price-dcost.

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

*  CLEAR: wa_count_fail, wa_count_ready, wa_count_finish.

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
        wa_amount     LIKE bseg-wrbtr.

  CLEAR: it_bapiret2, it_bapiret2[].

  MOVE: it_price-dmamt TO wa_amount.

  CASE c_mark.
    WHEN r_year.
      wa_head-material     = it_price-matnr.
      wa_head-cost_view    = 'X'.
      wa_mbew-val_area     = p_werks.
      wa_mbew-plndprice3   = wa_amount.
      wa_mbew-plndprdate3  = sy-datum.
      wa_mbewx-val_area    = p_werks.
      wa_mbewx-plndprice3  = 'X'.
      wa_mbewx-plndprdate3 = 'X'.
    WHEN r_quar.
      wa_head-material     = it_price-matnr.
      wa_head-cost_view    = 'X'.
      wa_mbew-val_area     = p_werks.
      wa_mbew-plndprice1   = wa_amount.
      wa_mbew-plndprdate1  = sy-datum.
      wa_mbewx-val_area    = p_werks.
      wa_mbewx-plndprice1  = 'X'.
      wa_mbewx-plndprdate1 = 'X'.
  ENDCASE.

  CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
       EXPORTING
            headdata       = wa_head
            valuationdata  = wa_mbew
            valuationdatax = wa_mbewx
       TABLES
            returnmessages = it_bapiret2.

  READ TABLE it_bapiret2 WITH KEY type = 'E'.
  IF sy-subrc EQ 0.
    PERFORM master_update_failed_rtn.
  ELSE.
    PERFORM master_update_success_rtn.
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
*&      Form  display_progress_bar
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_progress_bar.
  DATA: lw_percentage(3) TYPE c,
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
  DATA: lw_field(50).

  GET CURSOR FIELD lw_field.

  CASE lw_field.
    WHEN 'IT_PRICE-MATNR'.
      SET PARAMETER ID 'MAT' FIELD it_price-matnr.

      CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
    WHEN 'ICON_RED_LIGHT'.
      MESSAGE ID it_err_msg-id TYPE 'S' NUMBER it_err_msg-msgno
              WITH it_err_msg-v1 it_err_msg-v2
                   it_err_msg-v3 it_err_msg-v4.
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
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

  DELETE it_err_msg WHERE base_d = ztmm_spe-base_d
                      AND matnr  = ztmm_spe-matnr.

  CLEAR: ztmm_analy.

  SELECT SINGLE * FROM ztmm_analy WHERE base_d = it_price-base_d
                                    AND matnr  = it_price-matnr.
  IF sy-subrc EQ 0.
    MOVE: it_price-werks   TO ztmm_analy-werks,
          it_price-ekorg   TO ztmm_analy-ekorg,
          it_price-kzust   TO ztmm_analy-kzust,
          it_price-valid_d TO ztmm_analy-valid_d,
          it_price-wrbtr   TO ztmm_analy-wrbtr,
          it_price-wduty   TO ztmm_analy-wduty,
          it_price-wcost   TO ztmm_analy-wcost,
          it_price-dmbtr   TO ztmm_analy-dmbtr,
          it_price-dduty   TO ztmm_analy-dduty,
          it_price-dcost   TO ztmm_analy-dcost,
          it_price-waers   TO ztmm_analy-waers,
          it_price-wkurs   TO ztmm_analy-wkurs,
          it_price-source  TO ztmm_analy-source,
          it_price-lifnr   TO ztmm_analy-lifnr,
          sy-uname         TO ztmm_analy-aenam,
          sy-datum         TO ztmm_analy-aedat,
          sy-uzeit         TO ztmm_analy-aezet.

    PERFORM update_table_for_update.
  ELSE.
    MOVE: it_price-base_d  TO ztmm_analy-base_d,
          it_price-matnr   TO ztmm_analy-matnr,
          it_price-werks   TO ztmm_analy-werks,
          it_price-ekorg   TO ztmm_analy-ekorg,
          it_price-kzust   TO ztmm_analy-kzust,
          it_price-valid_d TO ztmm_analy-valid_d,
          it_price-wrbtr   TO ztmm_analy-wrbtr,
          it_price-wduty   TO ztmm_analy-wduty,
          it_price-wcost   TO ztmm_analy-wcost,
          it_price-dmbtr   TO ztmm_analy-dmbtr,
          it_price-dduty   TO ztmm_analy-dduty,
          it_price-dcost   TO ztmm_analy-dcost,
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

    PERFORM update_table_for_insert.
  ENDIF.
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

  MOVE: p_datum            TO it_price-base_d,
        wa_price-matnr     TO it_price-matnr,
        wa_price-profl     TO it_price-profl,
        wa_price-maktx     TO it_price-maktx,
        wa_price-f_werks   TO it_price-werks,
        wa_price-f_ekorg   TO it_price-ekorg,
        wa_price-f_kzust   TO it_price-kzust,
        wa_price-f_valid_d TO it_price-valid_d,
        wa_price-f_wrbtr   TO it_price-wrbtr,
        wa_price-f_wcost   TO it_price-wcost,
        wa_price-f_dmbtr   TO it_price-dmbtr,
        wa_price-f_dcost   TO it_price-dcost,
        wa_price-f_waers   TO it_price-waers,
        wa_price-f_wkurs   TO it_price-wkurs,
        'M'                TO it_price-source,
        wa_price-f_lifnr   TO it_price-lifnr,
        c_success          TO it_price-flag.

  it_price-wramt = it_price-wrbtr + it_price-wduty + it_price-wcost.
  it_price-dmamt = it_price-dmbtr + it_price-dduty + it_price-dcost.

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

  MOVE: p_datum          TO it_price-base_d,
        wa_price-matnr   TO it_price-matnr,
        wa_price-profl   TO it_price-profl,
        wa_price-maktx   TO it_price-maktx,
        wa_price-werks   TO it_price-werks,
        wa_price-ekorg   TO it_price-ekorg,
        wa_price-kzust   TO it_price-kzust,
        wa_price-valid_d TO it_price-valid_d,
        wa_price-wrbtr   TO it_price-wrbtr,
        wa_price-wcost   TO it_price-wcost,
        wa_price-dmbtr   TO it_price-dmbtr,
        wa_price-dcost   TO it_price-dcost,
        wa_price-waers   TO it_price-waers,
        wa_price-wkurs   TO it_price-wkurs,
        'M'              TO it_price-source,
        wa_price-lifnr   TO it_price-lifnr,
        c_ready          TO it_price-flag.

  it_price-wramt = it_price-wrbtr + it_price-wduty + it_price-wcost.
  it_price-dmamt = it_price-dmbtr + it_price-dduty + it_price-dcost.

  APPEND it_price.

  wa_count_ready = wa_count_ready + 1.
ENDFORM.                    " append_ready_price_for_error
