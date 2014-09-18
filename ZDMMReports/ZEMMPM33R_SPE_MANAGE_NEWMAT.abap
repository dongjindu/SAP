************************************************************************
* Program Name      : ZEMMPM33R_SPE_MANAGE_NEWMAT
* Author            : Byung-sung, Bae
* Creation Date     : 2003.10.16.
* Specifications By : Byung-sung, Bae
* Pattern           : Report 1-1
* Development Request No : UD1K910837
* Addl Documentation:
* Description       : Error Price Update for New Material
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
report zemmpm33r_spe_manage_newmat no standard page heading
                                   line-size  126
                                   line-count  65.
include: <icon>.
type-pools : slis.
tables: mara,
        a018,
        ztmm_analy_new,
        ztmm_spe_new,
        zvmm_info_condi,
        t024,
        t001k.

*----- Internal tables
data: begin of it_price occurs 0,
        check,                              "Check Field
        matnr      like mara-matnr,         "Material
        meins      like mara-meins,         "Master UoM
        profl      like mara-profl,         "KD,LD Separator
        rduty      like konp-kbetr,         "Import tax rate
        rfrgt      like konp-kbetr,         "Import freight
        rcost      like konp-kbetr,         "Import costs rate
        maktx      like makt-maktx,         "Description
        werks      like ztmm_analy-werks,   "Plant
        period     like ztmm_analy-period,  "Period
        ekorg      like ztmm_analy-ekorg,   "Purchase Org.
        ekgrp      like t024-ekgrp,         "Purchase Grp.
        base_d     like ztmm_analy-base_d,  "Base date
        datab      like konh-datab,         "Valid date
        type(2),                            "KD, LP
        flag,                               "Status flag
        stprs      like mbew-stprs,         "Std. Price
        zplp1      like mbew-zplp1,         "Planned Price(quarter)
        zplp3      like mbew-zplp1,         "Planned Price(year)
        peinh      like mbew-peinh,         "Price Unit
        i_flag,                             "Info exist flag
        i_kzust    like ztmm_analy-kzust,   "Info Reason code
        i_valid_d  like ztmm_analy-valid_d, "Info Valid date
        i_wrbtr    like ztmm_analy-wrbtr,   "Info Foreign Pice
        i_wduty    like ztmm_analy-wrbtr,   "Info duty of F.Curr
        i_wfrgt    like ztmm_analy-wrbtr,   "Info freight of F.Curr
        i_wcost    like ztmm_analy-wrbtr,   "Info cost of F.Curr
        i_dmbtr    like ztmm_analy-wrbtr,   "Info Local Price
        i_dduty    like ztmm_analy-wrbtr,   "Info duty of L.Curr
        i_dfrgt    like ztmm_analy-wrbtr,   "Info freight of L.Curr
        i_dcost    like ztmm_analy-wrbtr,   "Info cost of L.Curr
        i_wramt    like ztmm_analy-wrbtr,   "Info Foreign Amount
        i_dmamt    like ztmm_analy-wrbtr,   "Info Local Amount
        i_peinh    like ztmm_analy-peinh,   "Info price unit
        i_waers    like ztmm_analy-waers,   "Info Currency
        i_wkurs    like ztmm_analy-wkurs,   "Info exchange rate
        i_lifnr    like ztmm_analy-lifnr,   "Info vendor
        i_name1    like lfa1-name1,         "Info vendor name
        e_flag,                             "Error exist flag
        e_kzust    like ztmm_analy-kzust,   "Error Reason code
        e_valid_d  like ztmm_analy-valid_d, "Error Valid date
        e_wrbtr    like ztmm_analy-wrbtr,   "Error Foreign Pice
        e_wduty    like ztmm_analy-wrbtr,   "Error duty of F.Curr
        e_wfrgt    like ztmm_analy-wrbtr,   "Error freight of F.Curr
        e_wcost    like ztmm_analy-wrbtr,   "Error cost of F.Curr
        e_dmbtr    like ztmm_analy-wrbtr,   "Error Local Price
        e_dduty    like ztmm_analy-wrbtr,   "Error duty of L.Curr
        e_dfrgt    like ztmm_analy-wrbtr,   "Error freight of L.Curr
        e_dcost    like ztmm_analy-wrbtr,   "Error cost of L.Curr
        e_wramt    like ztmm_analy-wrbtr,   "Error Foreign Amount
        e_dmamt    like ztmm_analy-wrbtr,   "Error Local Amount
        e_peinh    like ztmm_analy-peinh,   "Error price unit
        e_waers    like ztmm_analy-waers,   "Error Currency
        e_wkurs    like ztmm_analy-wkurs,   "Error exchange rate
        e_lifnr    like ztmm_analy-lifnr,   "Error vendor
        e_name1    like lfa1-name1,         "Error vendor name
        f_flag,                             "Finish exist flag
        f_kzust    like ztmm_analy-kzust,   "Finish Reason code
        f_valid_d  like ztmm_analy-valid_d, "Finish Valid date
        f_wrbtr    like ztmm_analy-wrbtr,   "Finish Foreign Pice
        f_wduty    like ztmm_analy-wrbtr,   "Finish duty of F.Curr
        f_wfrgt    like ztmm_analy-wrbtr,   "Finish freight of F.Curr
        f_wcost    like ztmm_analy-wrbtr,   "Finish cost of F.Curr
        f_dmbtr    like ztmm_analy-wrbtr,   "Finish Local Price
        f_dduty    like ztmm_analy-wrbtr,   "Finish duty of L.Curr
        f_dfrgt    like ztmm_analy-wrbtr,   "Finish freight of L.Curr
        f_dcost    like ztmm_analy-wrbtr,   "Finish cost of L.Curr
        f_wramt    like ztmm_analy-wrbtr,   "Finish Foreign Amount
        f_dmamt    like ztmm_analy-wrbtr,   "Finish Local Amount
        f_peinh    like ztmm_analy-peinh,   "Finish price unit
        f_waers    like ztmm_analy-waers,   "Finish Currency
        f_wkurs    like ztmm_analy-wkurs,   "Finish exchange rate
        f_lifnr    like ztmm_analy-lifnr,   "Finish vendor
        f_period(6)  type n,
      end   of it_price.

data: begin of it_knumh occurs 0,
        knumh like konh-knumh,
        datab like konh-datab,
        datbi like konh-datbi,
        lifnr like lfa1-lifnr,
      end   of it_knumh.

data: begin of it_info occurs 0,                "Info Condition
          vakey   like   konh-vakey,
          datab   like   konh-datab,
          kzust   like   konh-kzust,
          kpein   like   konp-kpein,
          kumne   like   konp-kumne,
          kumza   like   konp-kumza,
          konwa   like   konp-konwa,
          meins   like   mara-meins,
          wkurs   like   ztmm_analy-wkurs,
          kbetr   like   konp-kbetr,
          rduty   like   konp-kbetr,          "Duty rate
          rfrgt   like   konp-kbetr,          "Freight rate
          rcost   like   konp-kbetr,          "Other cost rate
          wrbtr   type   ztmm_analy-wrbtr,    "Net price
          wduty   like   ztmm_analy-wrbtr,    "duty of F.Curr
          wfrgt   like   ztmm_analy-wrbtr,    "freight of F.Curr
          wcost   like   ztmm_analy-wrbtr,    "cost of F.Curr
          dmbtr   like   ztmm_analy-wrbtr,    "Local Price
          dduty   like   ztmm_analy-wrbtr,    "duty of L.Curr
          dfrgt   like   ztmm_analy-wrbtr,    "freight of L.Curr
          dcost   like   ztmm_analy-wrbtr,    "cost of L.Curr
          wramt   like   ztmm_analy-wrbtr,    "Foreign Amount
          dmamt   like   ztmm_analy-wrbtr,    "Local Amount
      end   of it_info.

data: it_zvmm_info_condi like zvmm_info_condi occurs 0 with header line.

data: begin of it_annual_matnr occurs 0,
        matnr   like   mara-matnr,
      end   of it_annual_matnr.

data: it_idnrk like ztmm_analy_tmp occurs 0 with header line.

data: begin of it_halb occurs 0,
        werks   like   t001w-werks,
        idnrk   like   mara-matnr,
      end   of it_halb.

*----- Work areas
data: wa_price like it_price.

data: begin of it_exchange_rate occurs 0,
        waers   like   ekko-waers,         "Currency
        wkurs   type   f,                  "Exchange rate
      end   of it_exchange_rate.

data: wa_matnr_f       like  mara-matnr,       "Material From
      wa_matnr_t       like  mara-matnr,       "Material To
      wa_amount        type  f,                "Amount of Base UoM
      wa_period        like  ztmm_analy-period,"Period
      wa_vakey         like  konh-vakey,       "Value Key
      wa_low_amt       type  f,                "lower amount of Base UoM
      wa_base_d        like  sy-datum,         "Base date
      wa_count         type  i,                "Count of Material
      wa_cnt           type  i,                "Total count
      wa_progress_idx  type  i,                "Progress bar index
      wa_count_ready   type  i,                "Count of ready
      wa_count_failed  type  i,                "Count of failed
      wa_count_success type  i,                "Count of success
      wa_count_others  type  i,                "Count of others
      wa_count_finish  type  i,                "Count of finished
      wa_count_t_price type  i,                "Count of temp price
      wa_count_error   type  i,                "Count of error
      wa_waers         like  t001-waers.       "Currency

constants: c_mark                   value 'X',   "Marker
           c_ekorg like ekko-ekorg  value 'PU01',"Purchase Org.
           c_kschl like konp-kschl  value 'PB00',"Type of amount
           c_frght like konp-kschl  value 'FRA1',"Type of freight
           c_con01 like konp-kschl  value 'ZOTH',"Type of ETC rate
           c_con02 like konp-kschl  value 'ZOTI',"Type of ETC rate
*           c_price LIKE konp-kbetr  VALUE '0.01',"Default Price
           c_ready                  value '1',   "Ready item
           c_failed                 value '2',   "Update failed item
           c_success                value '3',   "Update successful item
           c_finish                 value '4',   "Update finished item
           c_t_price                value '5',   "Temp price,can't input
           c_error                  value '6',   "Error item
*          steel wheel and tire moduel price has been split into two
*          parts: one price for tire(ztir) and one price for steel
*          wheel(pb00)
           c_tire_price_type like ekko-ekorg value 'ZTIR'. "TIRE PRICE

*----- Selection screens
selection-screen begin of block bl1 with frame title text-t01.
parameters:     p_werks like t001w-werks default 'P001' obligatory.
*SELECT-OPTIONS: s_ekgrp FOR  t024-ekgrp NO-EXTENSION NO INTERVALS.
select-options: s_matnr for  mara-matnr no-extension.
selection-screen end   of block bl1.

*SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE text-t02.
*PARAMETERS:      p_year  LIKE sy-datum(4) DEFAULT sy-datum(4)
*OBLIGATORY
*.
*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN POSITION 1.
*PARAMETERS       r_year RADIOBUTTON GROUP rd1 DEFAULT 'X'
*                                              USER-COMMAND rd1.
*SELECTION-SCREEN COMMENT  3(25) text-t03 FOR FIELD r_year.
*SELECTION-SCREEN POSITION 40.
*PARAMETERS       r_quar RADIOBUTTON GROUP rd1.
*SELECTION-SCREEN COMMENT 42(25) text-t04 FOR FIELD r_quar.
*SELECTION-SCREEN END OF LINE.
*
*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN COMMENT 1(79) text-t10.
*SELECTION-SCREEN END   OF LINE.
*
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
*SELECTION-SCREEN END   OF BLOCK bl2.

selection-screen begin of block bl3 with frame title text-t12.
parameters:     p_fprice default 'X' as checkbox.
parameters:     p_iprice default 'X' as checkbox.
selection-screen end   of block bl3.


*----- Screen Attribute Control
at selection-screen output.
  perform set_screen_attribute.
  perform set_quarter.                                   " Set Quarter

*----- Get Data
at selection-screen.
  check sy-ucomm = 'ONLI'.
  perform check_rtn.
  perform get_data.

top-of-page.
  perform top_of_page.

top-of-page during line-selection.
  perform top_of_page.

start-of-selection.
  set pf-status 'BASE'.
  perform write_data.

at user-command.
  case sy-ucomm.
    when 'ENTR'.
      sy-lsind = sy-lsind - 1.
      perform enter_rtn.
    when 'SAVE'.
      sy-lsind = sy-lsind - 1.
      perform save_rtn.
    when 'S_ALL'.
      sy-lsind = sy-lsind - 1.
      perform select_all_rtn.
    when 'D_ALL'.
      sy-lsind = sy-lsind - 1.
      perform deselect_all_rtn.
  endcase.
*&---------------------------------------------------------------------*
*&      Form  set_screen_attribute
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_screen_attribute.
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
endform.                    " set_screen_attribute
*&---------------------------------------------------------------------*
*&      Form  set_quarter
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_quarter.
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
endform.                    " set_quarter
*&---------------------------------------------------------------------*
*&      Form  check_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_rtn.
  perform check_currency.
  perform check_matnr.
  perform check_period.
  perform check_error_data.
endform.                    " check_rtn
*&---------------------------------------------------------------------*
*&      Form  check_matnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_matnr.
  if     s_matnr-low eq ' ' and s_matnr-high eq ' '.
    wa_matnr_t = 'ZZZZZZZZZZZZZZZZZZ'.
  elseif s_matnr-low ne ' ' and s_matnr-high eq ' '.
    wa_matnr_f = wa_matnr_t = s_matnr-low.
  elseif s_matnr-low eq ' ' and s_matnr-high ne ' '.
    wa_matnr_t = s_matnr-high.
  elseif s_matnr-low ne ' ' and s_matnr-high ne ' '.
    wa_matnr_f = s_matnr-low. wa_matnr_t = s_matnr-high.
  endif.

  select single * from mara where matnr in s_matnr.
  if sy-subrc ne 0.
    message e000(zz) with text-m04.
  endif.
endform.                    " check_matnr
*&---------------------------------------------------------------------*
*&      Form  check_period
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_period.
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
  move: sy-datum(6) to wa_period.
endform.                    " check_period
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_data.
  clear: wa_price.

  perform locking_rtn.

  perform read_total_count.

  clear: wa_cnt.

  EXEC SQL PERFORMING APPEND_ERROR_PRICE.
    SELECT A.MATNR, A.MEINS,  A.PROFL,  C.MAKTX,
           F.KBETR, G.PERIOD, G.WERKS,  G.BASE_D,
           G.EKORG,
           D.STPRS, D.ZPLP1, D.ZPLP3, D.PEINH,
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
           :WA_PRICE-STPRS,     :WA_PRICE-ZPLP1,   :WA_PRICE-ZPLP3,
           :WA_PRICE-PEINH,
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
      FROM MARA A, MARC B, MAKT C, MBEW D, A902 E, KONP F,
           ZTMM_SPE_NEW G, ZTMM_ANALY_NEW H
     WHERE A.MANDT =   :SY-MANDT
       AND A.MATNR     BETWEEN :WA_MATNR_F AND :WA_MATNR_T
       AND A.MTART     IN ('ROH', 'ROH1')
       AND A.PROFL     IN ('K',   'V')
       AND A.LVORM     <> 'X'
       AND B.MANDT     =  A.MANDT
       AND B.MATNR     =  A.MATNR
       AND B.WERKS     =  :P_WERKS
       AND B.LVORM     =  ' '
       AND B.DISPO     <> 'M02'
       AND C.MANDT     =  A.MANDT
       AND C.MATNR     =  A.MATNR
       AND C.SPRAS     =  :SY-LANGU
       AND D.MANDT     =  B.MANDT
       AND D.MATNR     =  B.MATNR
       AND D.BWKEY     =  B.WERKS
       AND D.STPRS     =  0
       AND D.LVORM     =  ' '
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
       AND H.MANDT(+)  =  A.MANDT
       AND H.PERIOD(+) =  :WA_PERIOD
       AND H.WERKS(+) =  :P_WERKS
       AND H.MATNR(+)  =  A.MATNR
  ENDEXEC.

  read table it_price index 1.
  if sy-subrc ne 0.
    message e000(zz) with text-m01.
  endif.
endform.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  locking_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form locking_rtn.
  call function 'ENQUEUE_EZ_ZSMM_ST_LOCK'
       exporting
            mode_ztmm_spe  = 'E'
            mandt          = sy-mandt
            period         = wa_period
            submt          = ' '
       exceptions
            foreign_lock   = 1
            system_failure = 2
            others         = 3.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.
endform.                    " locking_rtn
*&---------------------------------------------------------------------*
*&      Form  APPEND_ERROR_PRICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form append_error_price.
  perform display_progress_bar.

*----- Read suitable Price
  clear: it_knumh, it_knumh[].
  select knumh datab lifnr
    into corresponding fields of table it_knumh
    from a018
   where kappl =  'M'
     and kschl =  'PB00'
     and matnr =  wa_price-matnr
     and ekorg =  c_ekorg
     and esokz =  '0'
     and datab <= wa_base_d
     and datbi >= wa_base_d.

  sort it_knumh by datab descending.

*----- Check Info Record Deletion Mark
  data: lw_matnr like mara-matnr.
  loop at it_knumh.
    select single matnr
      into lw_matnr
      from eina as a inner join eine as b
        on a~infnr = b~infnr
     where a~matnr = wa_price-matnr
       and a~lifnr = it_knumh-lifnr
       and a~loekz = ' '
       and b~werks = ' '
       and b~ekorg = c_ekorg
       and b~loekz = ' '.
    if sy-subrc ne 0.
      delete it_knumh.
    endif.
  endloop.

*----- Read the lowest price
  read table it_knumh index 1.
  if sy-subrc eq 0.                 "Standard Price
    delete it_knumh where datab < it_knumh-datab.
    perform check_future_price.
  else.                             "Temp price, No price
    perform read_future_price.
  endif.

  perform append_price_from_wa_price.
  clear: wa_price.

  wa_cnt = wa_cnt + 1.
endform.                    " APPEND_ERROR_PRICE
*&---------------------------------------------------------------------*
*&      Form  CHECK_ERROR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_error_data.
  select single base_d into wa_base_d
    from ztmm_spe_new
   where werks  = p_werks
     and period = wa_period.
  if sy-subrc ne 0.
    message w000(zz) with text-m05.

    select single base_d into wa_base_d
      from ztmm_analy_new
     where werks  = p_werks
       and period = wa_period.
    if sy-subrc ne 0.
      message e000(zz) with text-m06.
    endif.
  endif.
endform.                    " CHECK_ERROR_DATA
*&---------------------------------------------------------------------*
*&      Form  append_condition_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form append_condition_price.
  data: lw_error_flg.

  perform read_condition_per_vendor using lw_error_flg.

  check lw_error_flg is initial.

  perform set_other_infomation.

  perform delete_temp_price.

  perform select_lowest_price.

  perform append_price_to_it_price.
endform.                    " append_condition_price
*&---------------------------------------------------------------------*
*&      Form  read_future_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_future_price.
*----- Read suitable Price
  clear: it_knumh, it_knumh[].
  select knumh datab datbi lifnr
    into corresponding fields of table it_knumh
    from a018
   where kappl =  'M'
     and kschl =  'PB00'
     and matnr =  wa_price-matnr
     and ekorg =  c_ekorg
     and esokz =  '0'
     and datab >  wa_base_d.

*----- Check Info Record Deletion Mark
  data: lw_matnr like mara-matnr.
  loop at it_knumh.
    select single matnr
      into lw_matnr
      from eina as a inner join eine as b
        on a~infnr = b~infnr
     where a~matnr = wa_price-matnr
       and a~lifnr = it_knumh-lifnr
       and a~loekz = ' '
       and b~werks = ' '
       and b~ekorg = c_ekorg
       and b~loekz = ' '.
    if sy-subrc ne 0.
      delete it_knumh.
    endif.
  endloop.

  sort it_knumh by datab.
*----- Read the lowest price
  read table it_knumh index 1.
  if sy-subrc eq 0.                 "Standard Price
    delete it_knumh where datab > it_knumh-datab.
    perform append_condition_price.
  else.
    perform append_blank_price.
  endif.
endform.                    " read_future_price
*&---------------------------------------------------------------------*
*&      Form  read_condition_per_vendor
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LW_ERROR_FLG  text
*----------------------------------------------------------------------*
form read_condition_per_vendor using pw_error_flg.
*----- Read Info Record

  clear: it_zvmm_info_condi, it_zvmm_info_condi[].
  select *
    into corresponding fields of table it_zvmm_info_condi
    from zvmm_info_condi
     for all entries in it_knumh
   where knumh = it_knumh-knumh
*    Requested by chandra changed by chris
*    the gross price include type PB00 and ZTIR
*    AND kschl IN (c_kschl,c_frght,c_con01,c_con02)
     and kschl in (c_kschl,c_frght,c_con01,c_con02,c_tire_price_type)

*    end of change ON 03/28/2005
     and loevm_ko = ' '.
  if sy-subrc ne 0.
    perform append_blank_price.
    pw_error_flg = 'X'.
  endif.

*    Requested by chandra changed by chris
*    the gross price include type PB00 and ZTIR
*    COMBINE TYPE PB00 AND ZTIR PRICE TOGETHER
  data: wa_condi like it_zvmm_info_condi.
  loop at it_zvmm_info_condi.
     if it_zvmm_info_condi-kschl = c_tire_price_type.
       clear: wa_condi.
       read table it_zvmm_info_condi into wa_condi
            with key knumh = it_zvmm_info_condi-knumh
                     kschl = c_kschl.
       wa_condi-kbetr = wa_condi-kbetr + it_zvmm_info_condi-kbetr.
       modify table it_zvmm_info_condi from wa_condi
            transporting kbetr.
       delete it_zvmm_info_condi.
     endif.
  endloop.

*    end of change ON 03/28/2005


  loop at it_zvmm_info_condi.
    read table it_knumh with key knumh = it_zvmm_info_condi-knumh.
    if sy-subrc ne 0.
      message e000(zz) with text-m02.
    endif.

    move: it_knumh-datab to it_zvmm_info_condi-datab,
          it_knumh-datbi to it_zvmm_info_condi-datbi.

    modify it_zvmm_info_condi.
  endloop.
endform.                    " read_condition_per_vendor
*&---------------------------------------------------------------------*
*&      Form  append_blank_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form append_blank_price.
  clear: it_price.
  move: wa_price-matnr to it_price-matnr,
        wa_price-maktx to it_price-maktx.

endform.                    " append_blank_price
*&---------------------------------------------------------------------*
*&      Form  set_other_infomation
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_other_infomation.
*----- Set Import tax rate, freight rate, ETC rate to IT_INFO
  clear: it_info, it_info[].
  sort it_zvmm_info_condi by vakey kopos.

  loop at it_zvmm_info_condi.
    clear: it_info.

    read table it_info with key vakey = it_zvmm_info_condi-vakey.
    if sy-subrc ne 0.
      move: it_zvmm_info_condi-vakey to it_info-vakey,
            it_zvmm_info_condi-datab to it_info-datab.

      if it_zvmm_info_condi-kschl eq c_kschl.
        move: it_zvmm_info_condi-kumne to it_info-kumne,
              it_zvmm_info_condi-kumza to it_info-kumza.
      endif.

      if wa_price-profl eq 'K'.
        move: wa_price-rduty to it_info-rduty.
      endif.

      perform move_other_fields.

      append it_info.
    else.
      perform move_other_fields.

      modify it_info index sy-tabix.
    endif.
  endloop.
endform.                    " set_other_infomation

*&---------------------------------------------------------------------*
*&      Form  delete_temp_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form delete_temp_price.
*----- If normal price exist, delete 'X' Price
  loop at it_info where kzust(1) <> 'X'.
    exit.
  endloop.
  if sy-subrc eq 0.
    delete it_info where kzust(1) = 'X'.
  endif.
endform.                    " delete_temp_price
*&---------------------------------------------------------------------*
*&      Form  select_lowest_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form select_lowest_price.
*----- Read the lowest price

  clear: wa_low_amt, wa_amount, wa_vakey.
  loop at it_info.
    if sy-tabix eq 1.
      move: it_info-vakey to wa_vakey.
      perform calculate_amount using wa_low_amt.
      modify it_info.
      continue.
    endif.

    perform calculate_amount using wa_amount.

    if wa_low_amt > wa_amount.
      wa_low_amt  = wa_amount.
      wa_vakey    = it_info-vakey.
    endif.

    modify it_info.
  endloop.
endform.                    " select_lowest_price
*&---------------------------------------------------------------------*
*&      Form  calculate_amount
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_LOW_AMT  text
*----------------------------------------------------------------------*
form calculate_amount using pw_amount.
*----- Calculate amount
  data: lw_umrez like eina-umrez,        "Numerator
        lw_umren like eina-umren,        "Denomirator
        lw_crate type i,                 "Conversion rate
        lw_wrbtr(16) type p decimals 10. "Amount

  perform read_exchange_rate using it_info-wkurs it_info-konwa.

  if it_info-meins eq wa_price-meins.    "Info UoM =  Master UoM
    pw_amount = ( ( it_info-kbetr / it_info-kpein )
                + ( it_info-kbetr / it_info-kpein ) *
                  ( it_info-rduty / 1000          )
                + ( it_info-kbetr / it_info-kpein ) *
                  ( it_info-rfrgt / 1000          )
                + ( it_info-kbetr / it_info-kpein ) *
                  ( it_info-rcost / 1000          ) )
                *   it_info-wkurs.

    lw_wrbtr = it_info-kbetr / it_info-kpein.

    perform check_decimal_part using lw_wrbtr lw_crate.

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

  else.                                    "Info UoM <> Master UoM
    select single umrez umren
      into (lw_umrez, lw_umren)
      from eina
     where matnr = wa_price-matnr
       and lifnr = wa_vakey(10).
    if sy-subrc ne 0.
      message e000(zz) with text-m02.
    endif.

*----- If condition's conversion rule does not exist,
*----- read material master conversion rule
    if it_info-kumne is initial or it_info-kumza is initial.
      select single umrez umren
        into (it_info-kumza, it_info-kumne)
        from marm
       where matnr = wa_price-matnr
         and meinh = it_info-meins.
      if sy-subrc ne 0.
        message e000(zz) with wa_vakey(10) wa_price-matnr text-m07.
      endif.
    endif.

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

    perform check_decimal_part using lw_wrbtr lw_crate.

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
  endif.
endform.                    " calculate_amount
*&---------------------------------------------------------------------*
*&      Form  append_price_to_it_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form append_price_to_it_price.
*----- Append selected price to IT_PRICE
  read table it_info with key vakey = wa_vakey.
  if sy-subrc ne 0.
    message e000(zz) with text-m02.
  endif.

  move: it_info-rduty       to wa_price-rduty,
        it_info-rfrgt       to wa_price-rfrgt,
        it_info-rcost       to wa_price-rcost,
        it_info-kzust       to wa_price-i_kzust,
        it_info-datab       to wa_price-i_valid_d,
        it_info-konwa       to wa_price-i_waers,
        it_info-wkurs       to wa_price-i_wkurs,
        it_info-vakey(10)   to wa_price-i_lifnr,
        it_info-kpein       to wa_price-i_peinh,
        it_info-wrbtr       to wa_price-i_wrbtr,
        it_info-wduty       to wa_price-i_wduty,
        it_info-wcost       to wa_price-i_wcost,
        it_info-wfrgt       to wa_price-i_wfrgt,
        it_info-wramt       to wa_price-i_wramt,
        it_info-dmbtr       to wa_price-i_dmbtr,
        it_info-dduty       to wa_price-i_dduty,
        it_info-dfrgt       to wa_price-i_dfrgt,
        it_info-dcost       to wa_price-i_dcost,
        it_info-dmamt       to wa_price-i_dmamt,
        'X'                 to wa_price-i_flag.

  select single name1 into wa_price-i_name1
    from lfa1
   where lifnr = wa_price-i_lifnr.
endform.                    " append_price_to_it_price
*&---------------------------------------------------------------------*
*&      Form  read_exchange_rate
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_PRICE_WKURS  text
*----------------------------------------------------------------------*
form read_exchange_rate using p_wkurs p_waers.
  data: lw_wkurs type f.


  read table it_exchange_rate with key waers = p_waers.
  if sy-subrc ne 0.
    call function 'Z_FCA_GET_EXCHANGE_RATE'
         exporting
              client                  = sy-mandt
              date                    = wa_base_d
              source_currency         = p_waers
              target_currency         = wa_waers
              company_currency        = wa_waers
              type_of_rate            = 'P'
         importing
              exchange_rate           = lw_wkurs
         exceptions
              target_local_rate_error = 1
              source_local_rate_error = 2
              others                  = 3.
    if sy-subrc <> 0.
*      MESSAGE e000(zz) WITH text-m03 it_price-waers.
    endif.

    move: wa_price-i_waers to it_exchange_rate-waers,
          lw_wkurs         to it_exchange_rate-wkurs,
          lw_wkurs         to p_wkurs.

    append it_exchange_rate.
  endif.

  move it_exchange_rate-wkurs to p_wkurs.
endform.                    " read_exchange_rate
*&---------------------------------------------------------------------*
*&      Form  APPEND_PRICE_FROM_WA_PRICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form append_price_from_wa_price.
  clear: it_price.
  move-corresponding wa_price to it_price.

  it_price-f_wramt = it_price-f_wrbtr + it_price-f_wduty +
                     it_price-f_wfrgt + it_price-f_wcost.
  it_price-f_dmamt = it_price-f_dmbtr + it_price-f_dduty +
                     it_price-f_dfrgt + it_price-f_dcost.

  if not it_price-e_flag  is initial and
         it_price-e_wrbtr is initial.
    perform set_error_price.
  endif.

  it_price-e_wramt = it_price-e_wrbtr + it_price-e_wduty +
                     it_price-e_wfrgt + it_price-e_wcost.
  it_price-e_dmamt = it_price-e_dmbtr + it_price-e_dduty +
                     it_price-e_dfrgt + it_price-e_dcost.

  if     wa_price-f_flag eq 'X'.
    move: c_finish to it_price-flag.
    wa_count_finish = wa_count_finish + 1.
    wa_count_others = wa_count_others + 1.
  elseif wa_price-e_flag eq 'X'.
    move: c_ready to it_price-flag.
    wa_count_ready = wa_count_ready + 1.
  elseif wa_price-i_flag eq 'X'.
    move: c_t_price to it_price-flag.
    wa_count_t_price = wa_count_t_price + 1.
    wa_count_others = wa_count_others + 1.
  else.
    move: c_error to it_price-flag.
    wa_count_error = wa_count_error + 1.
    wa_count_others = wa_count_others + 1.
  endif.

  wa_count = wa_count + 1.


  if     wa_price-profl eq 'K'.
    it_price-type = 'KD'.
  elseif wa_price-profl eq 'V'.
    it_price-type = 'LP'.
  endif.


  append it_price.
endform.                    " APPEND_PRICE_FROM_WA_PRICE
*&---------------------------------------------------------------------*
*&      Form  top_of_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form top_of_page.
  reserve 3 lines.

  write:/01(130) text-h06 centered.
  write:/01(130) text-h07 centered.

  new-line.

  write:/09    text-h09,
         25(7) wa_cnt.

  write:/02(4) icon_yellow_light as icon,
               text-h03,
         25(7) wa_count_ready.

  write: 40(4) icon_green_light  as icon,
               text-h04,
         63(7) wa_count_success.

  write: 102   text-h14, wa_period.

  write:/02(4) icon_red_light    as icon,
               text-h05,
         25(7) wa_count_failed.

  write: 40(4) icon_light_out    as icon,
               text-h08,
         63(7) wa_count_others.

  write: 102   text-h15, wa_base_d.

  format color col_heading intensified on.

  uline.
  write:/ text-h01.
  write:/ text-h02.
  write:/ text-h11.

  uline.
endform.                    " top_of_page
*&---------------------------------------------------------------------*
*&      Form  write_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form write_data.
  sort it_price by flag type matnr.

  loop at it_price.
    perform reserve_lines.

    perform display_first_line.
    perform display_second_line.
    perform display_third_line.
    perform display_finish_data.
    perform display_info_data.
    uline.
  endloop.

  clear: it_price.
endform.                    " write_data
*&---------------------------------------------------------------------*
*&      Form  set_format_color
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LW_COLOR_FLG  text
*----------------------------------------------------------------------*
form set_format_color.
  case it_price-flag.
    when c_ready.
      format color 1 intensified off.
    when c_t_price.
      format color 7 intensified on.
    when c_success.
      format color 5 intensified on.
    when c_error.
      format color 6 intensified on.
  endcase.
endform.                    " set_format_color
*&---------------------------------------------------------------------*
*&      Form  display_detail_price_ready
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form display_detail_price_ready.

endform.                    " display_detail_price_ready
*&---------------------------------------------------------------------*
*&      Form  display_detail_price_finish
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form display_detail_price_finish.

endform.                    " display_detail_price_finish
*&---------------------------------------------------------------------*
*&      Form  display_detail_price_t_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form display_detail_price_t_price.

endform.                    " display_detail_price_t_price
*&---------------------------------------------------------------------*
*&      Form  display_detail_price_error
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form display_detail_price_error.

endform.                    " display_detail_price_error
*&---------------------------------------------------------------------*
*&      Form  ENTER_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form enter_rtn.
  data: lw_lsind like sy-lsind,
        lw_lilli like sy-lilli,
        lw_pagno like sy-pagno.

  move: sy-pagno to lw_pagno.
  lw_lilli = lw_lilli - 1.
  lw_lsind = lw_lsind + 1.

  perform read_screen_data_for_enter.

  perform write_data.

*----- Set current line to top
  call function 'LIST_SCROLL_LINE_TOPMOST'
       exporting
            list_index          = lw_lsind
            list_line           = lw_lilli
            list_page           = lw_pagno
       exceptions
            list_index_invalid  = 1
            list_line_not_found = 2
            no_list_active      = 3
            window_too_small    = 4
            others              = 5.

  clear: it_price.
endform.                    " ENTER_RTN
*&---------------------------------------------------------------------*
*&      Form  display_progress_bar_running
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form display_progress_bar_running.

endform.                    " display_progress_bar_running
*&---------------------------------------------------------------------*
*&      Form  NUMERIC_CHECK_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form numeric_check_rtn using pw_amount.
  call function 'CATS_NUMERIC_INPUT_CHECK'
       exporting
            input      = pw_amount
       importing
            output     = pw_amount
       exceptions
            no_numeric = 1
            others     = 2.

  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.
endform.                    " NUMERIC_CHECK_RTN
*&---------------------------------------------------------------------*
*&      Form  read_screen_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_screen_data.
*----- read screen data and update ITAB
  data: lw_wramt(50),
        lw_wrbtr(50),
        lw_peinh(10),
        lw_crate type i,
        lw_check,
        lw_line_idx type i,
        lw_maktx(40).

  do.
    clear: it_price,lw_wramt,lw_wrbtr,lw_line_idx,lw_maktx,
           lw_check,lw_peinh,lw_crate.
    read line sy-index field value it_price-check   into lw_check
                                   it_price-maktx   into lw_maktx
                                   it_price-e_wramt into lw_wramt
                                   it_price-e_wrbtr into lw_wrbtr
                                   it_price-e_peinh into lw_peinh.
    if sy-subrc ne 0. exit. endif.
    check not it_price-matnr is initial and lw_check eq 'X'.

    read table it_price with key matnr  = it_price-matnr.
    if sy-subrc ne 0.
      message e000(zz) with text-m02.
    endif.

    check not lw_maktx is initial and
            ( it_price-flag eq c_ready  or
              it_price-flag eq c_failed or
              it_price-flag eq c_success ).

    move: sy-tabix to lw_line_idx.

    perform price_unit_check  using lw_peinh.

    if     it_price-type = 'KD'.
      perform numeric_check_rtn using lw_wrbtr.

      it_price-check   = lw_check.
      it_price-e_peinh = lw_peinh.
      it_price-e_wrbtr = lw_wrbtr.

      it_price-e_wcost = lw_wrbtr * it_price-rcost / 1000.
      it_price-e_wfrgt = lw_wrbtr * it_price-rfrgt / 1000.
      it_price-e_wduty = lw_wrbtr * it_price-rduty / 1000.
      it_price-e_wramt = it_price-e_wrbtr + it_price-e_wcost +
                         it_price-e_wfrgt + it_price-e_wduty.
      it_price-e_dmbtr = it_price-e_wrbtr * it_price-e_wkurs.
      it_price-e_dcost = it_price-e_wcost * it_price-e_wkurs.
      it_price-e_dfrgt = it_price-e_wfrgt * it_price-e_wkurs.
      it_price-e_dduty = it_price-e_wduty * it_price-e_wkurs.
      it_price-e_dmamt = it_price-e_wramt * it_price-e_wkurs.
    elseif it_price-type = 'LP'.
      perform numeric_check_rtn using lw_wramt.

      it_price-check   = lw_check.
      it_price-e_peinh = lw_peinh.
      it_price-e_wrbtr = lw_wramt.
      it_price-e_wramt = lw_wramt.
      it_price-e_dmbtr = lw_wramt.
      it_price-e_dmamt = lw_wramt.
    endif.

    modify it_price index lw_line_idx.
  enddo.
endform.                    " read_screen_data
*&---------------------------------------------------------------------*
*&      Form  reserve_lines
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form reserve_lines.
  data: lw_line type i.

  if p_fprice eq 'X'.
    lw_line = lw_line + 2.
  endif.
  if p_iprice eq 'X'.
    lw_line = lw_line + 2.
  endif.

  lw_line = lw_line + 4.

  reserve lw_line lines.
endform.                    " reserve_lines
*&---------------------------------------------------------------------*
*&      Form  display_finish_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form display_finish_data.
  check p_fprice eq 'X'.
  format color 2 intensified on.
  write:/01(31)  space,
                '[Finished]'.

  format color col_normal intensified off.
  write: 44     it_price-f_kzust,
                it_price-f_waers,
                it_price-f_peinh no-zero,
                it_price-f_wramt currency it_price-f_waers no-zero,
                it_price-f_wrbtr currency it_price-f_waers no-zero,
           (10) it_price-f_wduty currency it_price-f_waers no-zero,
           (10) it_price-f_wfrgt currency it_price-f_waers no-zero,
           (10) it_price-f_wcost currency it_price-f_waers no-zero.
  hide it_price.

  write:/1(42)  space color col_normal intensified on,
                it_price-f_valid_d,
           (05) space,
                it_price-f_dmamt currency wa_waers no-zero,
                it_price-f_dmbtr currency wa_waers no-zero,
           (10) it_price-f_dduty currency wa_waers no-zero,
           (10) it_price-f_dfrgt currency wa_waers no-zero,
           (10) it_price-f_dcost currency wa_waers no-zero.
  hide it_price.
endform.                    " display_finish_data
*&---------------------------------------------------------------------*
*&      Form  display_info_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form display_info_data.
  check p_iprice eq 'X'.
  format color 2 intensified on.
  write:/01(31)  space,
                '[Info    ]'.

  format color 7 intensified on.

  write: 44     it_price-i_kzust,
                it_price-i_waers,
                it_price-i_peinh no-zero,
                it_price-i_wramt currency it_price-i_waers no-zero,
                it_price-i_wrbtr currency it_price-i_waers no-zero,
           (10) it_price-i_wduty currency it_price-i_waers no-zero,
           (10) it_price-i_wfrgt currency it_price-i_waers no-zero,
           (10) it_price-i_wcost currency it_price-i_waers no-zero.
  hide it_price.
  write:/1(42)  space color col_normal intensified on,
                it_price-i_valid_d,
           (05) space,
                it_price-i_dmamt currency wa_waers no-zero,
                it_price-i_dmbtr currency wa_waers no-zero,
           (10) it_price-i_dduty currency wa_waers no-zero,
           (10) it_price-i_dfrgt currency wa_waers no-zero,
           (10) it_price-i_dcost currency wa_waers no-zero.
  hide it_price.
endform.                    " display_info_data
*&---------------------------------------------------------------------*
*&      Form  display_standard_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form display_third_line.
  perform set_format_color.
  write:/3    it_price-e_lifnr,
         (29) it_price-e_name1.
  format color 5 intensified off.
  write:        it_price-e_valid_d,
           (5)  space,
                it_price-e_dmamt currency wa_waers no-zero,
                it_price-e_dmbtr currency wa_waers no-zero,
           (10) it_price-e_dduty currency wa_waers no-zero,
           (10) it_price-e_dfrgt currency wa_waers no-zero,
           (10) it_price-e_dcost currency wa_waers no-zero.

  hide it_price.
endform.                    " display_standard_price
*&---------------------------------------------------------------------*
*&      Form  display_first_line
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form display_first_line.
  perform set_format_color.

  write:/1      space,
          (18)  it_price-matnr,
                it_price-type,
                space,
                it_price-meins,
          (07)   space.

  case it_price-flag.
    when c_ready.
      write: (4) icon_yellow_light as icon.
    when c_failed.
      write: (4) icon_red_light    as icon hotspot.
    when c_success.
      write: (4) icon_green_light  as icon.
    when others.
      write: (4) icon_light_out    as icon hotspot.
  endcase.

  format color 3 intensified off.
  write: 54     it_price-peinh,
           (16) it_price-stprs currency wa_waers,
           (16) it_price-zplp3 currency wa_waers,
           (10) it_price-zplp1 currency wa_waers,
           (21) space.

  hide it_price.
endform.                    " display_first_line
*&---------------------------------------------------------------------*
*&      Form  display_second_line
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form display_second_line.
  perform set_format_color.

  case it_price-flag.
    when c_ready or c_failed or c_success.
      write:/1 it_price-check as checkbox.
    when others.
      write:/1 it_price-check as checkbox input off.
  endcase.


  write: it_price-maktx.

  format color 5 intensified off.
  write:        it_price-e_kzust,
                it_price-e_waers.

  if     it_price-flag = c_finish or
         it_price-flag = c_t_price or
         it_price-flag = c_error.
    write:  it_price-e_peinh no-zero,
            it_price-e_wramt currency it_price-e_waers no-zero,
            it_price-e_wrbtr currency it_price-e_waers no-zero.
  else.
    if     it_price-type eq 'KD'.
      write:  it_price-e_peinh input no-zero,
              it_price-e_wramt currency it_price-e_waers no-zero,
              it_price-e_wrbtr currency it_price-e_waers input no-zero.
    elseif it_price-type eq 'LP'.
      write:  it_price-e_peinh input no-zero,
              it_price-e_wramt currency it_price-e_waers input no-zero,
              it_price-e_wrbtr currency it_price-e_waers no-zero.
    endif.
  endif.

  write: (10) it_price-e_wduty no-zero,
         (10) it_price-e_wfrgt no-zero,
         (10) it_price-e_wcost no-zero.

  hide it_price.
endform.                    " display_second_line
*&---------------------------------------------------------------------*
*&      Form  save_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form save_rtn.
  data: lw_lsind like sy-lsind,
        lw_lilli like sy-lilli,
        lw_pagno like sy-pagno.

  move: sy-pagno to lw_pagno.
  lw_lilli = lw_lilli - 1.
  lw_lsind = lw_lsind + 1.

  perform read_screen_data.

  perform update_table.

  perform write_data.

*----- Set current line to top
  call function 'LIST_SCROLL_LINE_TOPMOST'
       exporting
            list_index          = lw_lsind
            list_line           = lw_lilli
            list_page           = lw_pagno
       exceptions
            list_index_invalid  = 1
            list_line_not_found = 2
            no_list_active      = 3
            window_too_small    = 4
            others              = 5.

  clear: it_price.
endform.                    " save_rtn
*&---------------------------------------------------------------------*
*&      Form  update_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form update_table.
*----- Update Temp Standard price

  loop at it_price where check = 'X'
                     and ( flag = c_ready or
                           flag = c_failed ).
    select single * from ztmm_spe_new where period = wa_period
                                        and werks  = p_werks
                                        and matnr  = it_price-matnr.
    if sy-subrc ne 0.
      case it_price-flag.
        when c_ready.
          wa_count_ready  = wa_count_ready  - 1.
          wa_count_failed = wa_count_failed + 1.
        when c_success.
          wa_count_success = wa_count_success  - 1.
          wa_count_failed  = wa_count_failed + 1.
        when c_failed.
      endcase.

      it_price-flag = c_failed.
    endif.

    move: it_price-e_wrbtr to ztmm_spe_new-wrbtr,
          it_price-e_wduty to ztmm_spe_new-wduty,
          it_price-e_wfrgt to ztmm_spe_new-wfrgt,
          it_price-e_wcost to ztmm_spe_new-wcost,
          it_price-e_dmbtr to ztmm_spe_new-dmbtr,
          it_price-e_dduty to ztmm_spe_new-dduty,
          it_price-e_dfrgt to ztmm_spe_new-dfrgt,
          it_price-e_dcost to ztmm_spe_new-dcost,
          it_price-e_peinh to ztmm_spe_new-peinh,
          sy-uname         to ztmm_spe_new-aenam,
          sy-datum         to ztmm_spe_new-aedat,
          sy-uzeit         to ztmm_spe_new-aezet.

    update ztmm_spe_new.
    if sy-subrc ne 0.
      case it_price-flag.
        when c_ready.
          wa_count_ready  = wa_count_ready  - 1.
          wa_count_failed = wa_count_failed + 1.
        when c_success.
          wa_count_success = wa_count_success  - 1.
          wa_count_failed  = wa_count_failed + 1.
        when c_failed.
      endcase.

      it_price-flag = c_failed.
    else.
      case it_price-flag.
        when c_ready.
          wa_count_ready   = wa_count_ready   - 1.
          wa_count_success = wa_count_success + 1.
        when c_success.
        when c_failed.
          wa_count_success = wa_count_success + 1.
          wa_count_failed  = wa_count_failed  - 1.
      endcase.

      it_price-flag = c_success.
    endif.
    modify it_price.
  endloop.
endform.                    " update_table
*&---------------------------------------------------------------------*
*&      Form  set_error_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_error_price.
*----- Changed by BSBAE. Changed on May 11,2004
*----- Requested by HSCho
*  IF wa_price-i_flag IS INITIAL.
*    MOVE: '0.01' TO it_price-e_wrbtr,
*          '0.01' TO it_price-e_wramt,
*          '0.01' TO it_price-e_dmbtr,
*          '0.01' TO it_price-e_dmamt.
*  ELSE.
  if not it_price-i_waers is initial.
    move: it_price-i_waers   to it_price-e_waers.
  endif.

  if not it_price-i_wkurs is initial.
    move: it_price-i_wkurs   to it_price-e_wkurs.
  endif.

  if not it_price-i_peinh is initial.
    move: it_price-i_peinh   to it_price-e_peinh.
  endif.

  move: it_price-i_kzust   to it_price-e_kzust,
        it_price-i_valid_d to it_price-e_valid_d,
        it_price-i_wrbtr   to it_price-e_wrbtr,
        it_price-i_wduty   to it_price-e_wduty,
        it_price-i_wfrgt   to it_price-e_wfrgt,
        it_price-i_wcost   to it_price-e_wcost,
        it_price-i_wramt   to it_price-e_wramt,
        it_price-i_dmbtr   to it_price-e_dmbtr,
        it_price-i_dduty   to it_price-e_dduty,
        it_price-i_dfrgt   to it_price-e_dfrgt,
        it_price-i_dcost   to it_price-e_dcost,
        it_price-i_dmamt   to it_price-e_dmamt,
        it_price-i_lifnr   to it_price-e_lifnr,
        it_price-i_name1   to it_price-e_name1.
*  ENDIF.
*----- Changed by BSBAE. Changed on May 11,2004
endform.                    " set_error_price
*&---------------------------------------------------------------------*
*&      Form  display_progress_bar
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form display_progress_bar.
  data: lw_percentage(4) type c,
        lw_mod type i,
        lw_text(50).

  wa_progress_idx = wa_progress_idx + 1.

  lw_percentage = wa_progress_idx / wa_count * 100.

  concatenate text-b01 lw_percentage '%' into lw_text.

  lw_mod = lw_percentage mod 6.

  check lw_mod eq 5.

  call function 'SAPGUI_PROGRESS_INDICATOR'
       exporting
            percentage = lw_percentage
            text       = lw_text.
endform.                    " display_progress_bar
*&---------------------------------------------------------------------*
*&      Form  move_other_fields
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form move_other_fields.
  case it_zvmm_info_condi-kschl.
    when c_kschl.
      move: it_zvmm_info_condi-kbetr to it_info-kbetr,
            it_zvmm_info_condi-kzust to it_info-kzust,
            it_zvmm_info_condi-kpein to it_info-kpein,
            it_zvmm_info_condi-kmein to it_info-meins,
            it_zvmm_info_condi-kumne to it_info-kumne,
            it_zvmm_info_condi-kumza to it_info-kumza,
            it_zvmm_info_condi-konwa to it_info-konwa.
    when c_frght.
      check wa_price-profl eq 'K'.
      move: it_zvmm_info_condi-kbetr to it_info-rfrgt.
    when others.
      check wa_price-profl eq 'K'.
      it_info-rcost = it_info-rcost + it_zvmm_info_condi-kbetr.
  endcase.
endform.                    " move_other_fields
*&---------------------------------------------------------------------*
*&      Form  read_total_count
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_total_count.
*----- Get total count of material
  select count( * ) into wa_count
    from mara
     where matnr between wa_matnr_f and wa_matnr_t
       and mtart in ('ROH', 'ROH1')
       and profl in ('K',   'V')
       and lvorm <> 'X'.
  if sy-subrc ne 0.
    message e000(zz) with text-m01.
  endif.
endform.                    " read_total_count
*&---------------------------------------------------------------------*
*&      Form  check_currency
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_currency.
  select single bukrs into t001k-bukrs
    from t001k
   where bwkey = p_werks.
  if sy-subrc ne 0.
    message e000(zz) with text-m07.
  endif.

  select single waers into wa_waers from t001 where bukrs = t001k-bukrs.
  if sy-subrc ne 0.
    message e000(zz) with text-m07.
  endif.
endform.                    " check_currency
*&---------------------------------------------------------------------*
*&      Form  get_quater_plan
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*form get_quater_plan.
*  clear: wa_price.
*
*  perform locking_rtn.
*
*  perform read_total_count.
*
*  clear: wa_cnt.
*
*  EXEC SQL PERFORMING APPEND_ERROR_PRICE.
*    SELECT A.MATNR, A.MEINS,  A.PROFL,  C.MAKTX,
*           F.KBETR, G.PERIOD, G.WERKS,  G.BASE_D,
*           G.EKORG,
*           D.STPRS, D.ZPLP1, D.ZPLP3, D.PEINH,
*           DECODE(NVL(G.PERIOD,'000000'),'000000',' ','X'),
*           G.KZUST, G.WRBTR,  G.WDUTY,  G.WCOST,
*           G.DMBTR, G.DDUTY,  G.DCOST,  G.WAERS,
*           G.WKURS, G.LIFNR,  G.WFRGT,  G.DFRGT,
*           G.PEINH,
*           DECODE(NVL(H.PERIOD,'000000'), '000000',' ','X'),
*           H.KZUST, H.WRBTR,  H.WDUTY,  H.WCOST,
*           H.DMBTR, H.DDUTY,  H.DCOST,  H.WAERS,
*           H.WKURS, H.LIFNR,  H.VALID_D,H.WFRGT,
*           H.DFRGT, H.PEINH
*      INTO :WA_PRICE-MATNR,     :WA_PRICE-MEINS,   :WA_PRICE-PROFL,
*           :WA_PRICE-MAKTX,     :WA_PRICE-RDUTY,   :WA_PRICE-PERIOD,
*           :WA_PRICE-WERKS,     :WA_PRICE-BASE_D,  :WA_PRICE-EKORG,
*           :WA_PRICE-STPRS,     :WA_PRICE-ZPLP1,   :WA_PRICE-ZPLP3,
*           :WA_PRICE-PEINH,
*           :WA_PRICE-E_FLAG,
*           :WA_PRICE-E_KZUST,   :WA_PRICE-E_WRBTR, :WA_PRICE-E_WDUTY,
*           :WA_PRICE-E_WCOST,   :WA_PRICE-E_DMBTR, :WA_PRICE-E_DDUTY,
*           :WA_PRICE-E_DCOST,   :WA_PRICE-E_WAERS, :WA_PRICE-E_WKURS,
*           :WA_PRICE-E_LIFNR,   :WA_PRICE-E_WFRGT, :WA_PRICE-E_DFRGT,
*           :WA_PRICE-E_PEINH,
*           :WA_PRICE-F_FLAG,
*           :WA_PRICE-F_KZUST,   :WA_PRICE-F_WRBTR, :WA_PRICE-F_WDUTY,
*           :WA_PRICE-F_WCOST,   :WA_PRICE-F_DMBTR, :WA_PRICE-F_DDUTY,
*           :WA_PRICE-F_DCOST,   :WA_PRICE-F_WAERS, :WA_PRICE-F_WKURS,
*           :WA_PRICE-F_LIFNR,   :WA_PRICE-F_VALID_D,:WA_PRICE-F_WFRGT,
*           :WA_PRICE-F_DFRGT,   :WA_PRICE-F_PEINH
*      FROM MARA A, MARC B, MAKT C, MBEW D, A902 E, KONP F, ZTMM_SPE G,
*           ZTMM_ANALY H
*     WHERE A.MANDT =   :SY-MANDT
*       AND A.MATNR     BETWEEN :WA_MATNR_F AND :WA_MATNR_T
*       AND A.MTART     IN ('ROH', 'ROH1')
*       AND A.PROFL     IN ('K',   'V')
*       AND A.LVORM     <> 'X'
*       AND B.MANDT     =  A.MANDT
*       AND B.MATNR     =  A.MATNR
*       AND B.WERKS     =  :P_WERKS
*       AND B.LVORM     =  ' '
**----- 2004.06.18 Requested by HSCHO, Changed by BSBAE
**----- Except submaterial
*       AND B.DISPO     <> 'M02'
**----- 2004.06.18 Requested by HSCHO, Changed by BSBAE
*       AND C.MANDT     =  A.MANDT
*       AND C.MATNR     =  A.MATNR
*       AND C.SPRAS     =  :SY-LANGU
*       AND D.MANDT(+)  =  B.MANDT
*       AND D.MATNR(+)  =  B.MATNR
*       AND D.BWKEY(+)  =  B.WERKS
*       AND E.MANDT(+)  =  B.MANDT
*       AND E.KAPPL(+)  =  'M'
*       AND E.KSCHL(+)  =  'ZOA1'
*       AND E.STAWN(+)  =  B.STAWN
*       AND F.MANDT(+)  =  E.MANDT
*       AND F.KNUMH(+)  =  E.KNUMH
*       AND G.MANDT(+)  =  A.MANDT
*       AND G.PERIOD(+) =  :WA_PERIOD
*       AND G.WERKS(+)  =  :P_WERKS
*       AND G.MATNR(+)  =  A.MATNR
*       AND G.SUBMT(+)  =  ' '
*       AND H.MANDT(+)  =  A.MANDT
*       AND H.PERIOD(+) =  :WA_PERIOD
*       AND H.WERKS(+) =  :P_WERKS
*       AND H.MATNR(+)  =  A.MATNR
*       AND H.SUBMT(+)  =  ' '
*  ENDEXEC.
*
*  read table it_price index 1.
*  if sy-subrc ne 0.
*    message e000(zz) with text-m01.
*  endif.
*endform.                    " get_quater_plan
*&---------------------------------------------------------------------*
*&      Form  get_annual_plan
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*form get_annual_plan.
*  clear: wa_price.
*
*  perform locking_rtn.
*
*  clear: it_annual_matnr, it_annual_matnr[],
*         it_idnrk,        it_idnrk[],
*         it_halb,         it_halb[].
*
**----- Read PIR(Target Fullspec code)
*  select matnr
*    into table it_annual_matnr
*    from pbim
*   where werks =  p_werks
*     and bedae in ('VSE','VSF')
*     and versb =  'Y1'
*     and pbdnr =  'Y1'
*     and loevr = space.
*
**----- Read BOM Component of fullspec code
*  loop at it_annual_matnr.
*    perform display_progress_bar_bom using it_annual_matnr-matnr.
*
*    perform get_bom_component using it_annual_matnr-matnr p_werks.
*  endloop.
*
*  sort it_idnrk by period werks idnrk.
*
**----- Read Engine BOM Component
*  perform get_component_of_engine.
*
**----- Delete duplicate items
*  delete adjacent duplicates from it_idnrk comparing all fields.
*
**----- Because of performance, save items to temp table
**----- And then, Read the other infomation by join
*  delete from ztmm_analy_tmp where period = wa_period
*                               and werks  = p_werks
*                               and submt  = ' '.
*
*  insert ztmm_analy_tmp from table it_idnrk accepting duplicate keys.
*
*  commit work and wait.
*
**  DESCRIBE TABLE it_idnrk LINES wa_count.
*
*  perform set_it_price_with_component.
*
*  delete from ztmm_analy_tmp where period = wa_period
*                               and werks  = p_werks
*                               and submt  = ' '.
*endform.                    " get_annual_plan
*&---------------------------------------------------------------------*
*&      Form  display_progress_bar_bom
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form display_progress_bar_bom using pwa_matnr.
  data: lw_text(55).

  concatenate text-b04 pwa_matnr into lw_text.

  call function 'SAPGUI_PROGRESS_INDICATOR'
       exporting
            percentage = 100
            text       = lw_text.
endform.                    " display_progress_bar_bom
*&---------------------------------------------------------------------*
*&      Form  get_bom_component
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ANNUAL_MATNR_MATNR  text
*----------------------------------------------------------------------*
*form get_bom_component using pw_matnr pw_werks.
*  data: lt_stb like stpox occurs 0 with header line.
*
*  call function 'CS_BOM_EXPL_MAT_V2'
*       exporting
*            capid                 = 'PC01'
*            datuv                 = wa_base_d
*            ehndl                 = '1'
*            mktls                 = 'X'
*            mehrs                 = 'X'
*            mmory                 = '1'
*            mtnrv                 = pw_matnr
*            stpst                 = 0
*            svwvo                 = 'X'
*            werks                 = pw_werks
*            vrsvo                 = 'X'
*       tables
*            stb                   = lt_stb
*       exceptions
*            alt_not_found         = 1
*            call_invalid          = 2
*            material_not_found    = 3
*            missing_authorization = 4
*            no_bom_found          = 5
*            no_plant_data         = 6
*            no_suitable_bom_found = 7
*            conversion_error      = 8
*            others                = 9.
*  if sy-subrc <> 0.
*    message e000(zz) with pw_werks pw_matnr text-m08.
*  endif.
*
*  loop at lt_stb where idnrk in s_matnr
*                   and ( mtart eq 'ROH'  or
*                         mtart eq 'ROH1' or
*                         mtart eq 'HALB' ).
*    case lt_stb-mtart.
*      when 'ROH' or 'ROH1'.
*        move: wa_period    to it_idnrk-period,
*              p_werks      to it_idnrk-werks,
*              pw_werks     to it_idnrk-werks_act,
*              lt_stb-idnrk to it_idnrk-idnrk.
*
*        collect it_idnrk.
*      when 'HALB'.
*        move: pw_werks     to it_halb-werks,
*              lt_stb-idnrk to it_halb-idnrk.
*
*        collect it_halb.
*    endcase.
*  endloop.
*endform.                    " get_bom_component
*&---------------------------------------------------------------------*
*&      Form  set_it_price_with_component
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*form set_it_price_with_component.
*  clear: wa_cnt.
*
*  EXEC SQL PERFORMING APPEND_ERROR_PRICE.
*    SELECT A.MATNR, A.MEINS,  A.PROFL,  C.MAKTX,
*           F.KBETR, G.PERIOD, G.WERKS,  G.BASE_D,
*           G.EKORG,
*           D.STPRS, D.ZPLP1, D.ZPLP3, D.PEINH,
*           DECODE(NVL(G.PERIOD,'000000'),'000000',' ','X'),
*           G.KZUST, G.WRBTR,  G.WDUTY,  G.WCOST,
*           G.DMBTR, G.DDUTY,  G.DCOST,  G.WAERS,
*           G.WKURS, G.LIFNR,  G.WFRGT,  G.DFRGT,
*           G.PEINH,
*           DECODE(NVL(H.PERIOD,'000000'), '000000',' ','X'),
*           H.KZUST, H.WRBTR,  H.WDUTY,  H.WCOST,
*           H.DMBTR, H.DDUTY,  H.DCOST,  H.WAERS,
*           H.WKURS, H.LIFNR,  H.VALID_D,H.WFRGT,
*           H.DFRGT, H.PEINH
*      INTO :WA_PRICE-MATNR,     :WA_PRICE-MEINS,   :WA_PRICE-PROFL,
*           :WA_PRICE-MAKTX,     :WA_PRICE-RDUTY,   :WA_PRICE-PERIOD,
*           :WA_PRICE-WERKS,     :WA_PRICE-BASE_D,  :WA_PRICE-EKORG,
*           :WA_PRICE-STPRS,     :WA_PRICE-ZPLP1,   :WA_PRICE-ZPLP3,
*           :WA_PRICE-PEINH,
*           :WA_PRICE-E_FLAG,
*           :WA_PRICE-E_KZUST,   :WA_PRICE-E_WRBTR, :WA_PRICE-E_WDUTY,
*           :WA_PRICE-E_WCOST,   :WA_PRICE-E_DMBTR, :WA_PRICE-E_DDUTY,
*           :WA_PRICE-E_DCOST,   :WA_PRICE-E_WAERS, :WA_PRICE-E_WKURS,
*           :WA_PRICE-E_LIFNR,   :WA_PRICE-E_WFRGT, :WA_PRICE-E_DFRGT,
*           :WA_PRICE-E_PEINH,
*           :WA_PRICE-F_FLAG,
*           :WA_PRICE-F_KZUST,   :WA_PRICE-F_WRBTR, :WA_PRICE-F_WDUTY,
*           :WA_PRICE-F_WCOST,   :WA_PRICE-F_DMBTR, :WA_PRICE-F_DDUTY,
*           :WA_PRICE-F_DCOST,   :WA_PRICE-F_WAERS, :WA_PRICE-F_WKURS,
*           :WA_PRICE-F_LIFNR,   :WA_PRICE-F_VALID_D,:WA_PRICE-F_WFRGT,
*           :WA_PRICE-F_DFRGT,   :WA_PRICE-F_PEINH
*      FROM MARA A, MARC B, MAKT C, MBEW D, A902 E, KONP F, ZTMM_SPE G,
*           ZTMM_ANALY H
*     WHERE A.MANDT =   :SY-MANDT
*       AND A.MATNR IN (SELECT IDNRK
*                         FROM ZTMM_ANALY_TMP
*                        WHERE MANDT  = :SY-MANDT
*                          AND PERIOD = :WA_PERIOD
*                          AND WERKS  = :P_WERKS
*                          AND SUBMT  = ' '.)
*       AND A.MTART     IN ('ROH', 'ROH1')
*       AND A.PROFL     IN ('K',   'V')
*       AND A.LVORM     <> 'X'
*       AND B.MANDT     =  A.MANDT
*       AND B.MATNR     =  A.MATNR
*       AND B.WERKS     =  :P_WERKS
*       AND B.LVORM     =  ' '
**----- 2004.06.18 Requested by HSCHO, Changed by BSBAE
**----- Except submaterial
*       AND B.DISPO  <> 'M02'
**----- 2004.06.18 Requested by HSCHO, Changed by BSBAE
*       AND C.MANDT     =  A.MANDT
*       AND C.MATNR     =  A.MATNR
*       AND C.SPRAS     =  :SY-LANGU
*       AND D.MANDT(+)  =  B.MANDT
*       AND D.MATNR(+)  =  B.MATNR
*       AND D.BWKEY(+)  =  B.WERKS
*       AND E.MANDT(+)  =  B.MANDT
*       AND E.KAPPL(+)  =  'M'
*       AND E.KSCHL(+)  =  'ZOA1'
*       AND E.STAWN(+)  =  B.STAWN
*       AND F.MANDT(+)  =  E.MANDT
*       AND F.KNUMH(+)  =  E.KNUMH
*       AND G.MANDT(+)  =  A.MANDT
*       AND G.PERIOD(+) =  :WA_PERIOD
*       AND G.WERKS(+)  =  :P_WERKS
*       AND G.MATNR(+)  =  A.MATNR
*       AND G.SUBMT(+)  =  ' '
*       AND H.MANDT(+)  =  A.MANDT
*       AND H.PERIOD(+) =  :WA_PERIOD
*       AND H.WERKS(+) =  :P_WERKS
*       AND H.MATNR(+)  =  A.MATNR
*       AND H.SUBMT(+)  =  ' '
*  ENDEXEC.
*
*  read table it_price index 1.
*  if sy-subrc ne 0.
*    message e000(zz) with text-m01.
*  endif.
*endform.                    " set_it_price_with_component
*&---------------------------------------------------------------------*
*&      Form  check_decimal_part
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PW_AMOUNT  text
*      -->P_LW_CRATE  text
*----------------------------------------------------------------------*
form check_decimal_part using pwa_amount pwa_crate.
*----- If decimal point 3, round.
*----- Maximum Price unit is 10.
  data: lw_amount type p decimals 10.

  lw_amount = frac( pwa_amount * 100 ).

  if lw_amount > 0.
    pwa_crate = 10.
  else.
    pwa_crate = 1.
  endif.
endform.                    " check_decimal_part
*&---------------------------------------------------------------------*
*&      Form  get_component_of_engine
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*form get_component_of_engine.
*  data: begin of lt_component occurs 0,
*          matnr   like   marc-matnr,           "Material
*          beskz   like   marc-beskz,           "Procurement type
*          sobsl   like   marc-sobsl,           "Special procurement
*        end   of lt_component.
*
*  check p_werks eq 'P001'.
*
*  select matnr into table lt_component
*    from marc
*     for all entries in it_halb
*   where werks = it_halb-werks
*     and matnr = it_halb-idnrk
*     and beskz = 'F'
*     and sobsl = '40'.
*
*  loop at lt_component.
*    perform display_progress_bar_bom using lt_component-matnr.
*
*    perform get_bom_component using lt_component-matnr 'E001'.
*  endloop.
*endform.                    " get_component_of_engine
*&---------------------------------------------------------------------*
*&      Form  select_all_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form select_all_rtn.
  move: 'X' to it_price-check.
  modify it_price transporting check where flag eq c_ready   or
                                           flag eq c_failed  or
                                           flag eq c_success.

  perform write_data.
endform.                    " select_all_rtn
*&---------------------------------------------------------------------*
*&      Form  deselect_all_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form deselect_all_rtn.
  move: '' to it_price-check.
  modify it_price transporting check where flag >= ' '.

  perform write_data.
endform.                    " deselect_all_rtn
*&---------------------------------------------------------------------*
*&      Form  read_screen_data_for_enter
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_screen_data_for_enter.
*----- read screen data and update ITAB
  data: lw_wramt(50),
        lw_wrbtr(50),
        lw_peinh(10),
        lw_crate type i,
        lw_line_idx type i,
        lw_check,
        lw_maktx(40).

  do.
    clear: it_price,lw_wramt,lw_wrbtr,lw_line_idx,lw_maktx,
           lw_peinh,lw_crate.
    read line sy-index field value it_price-check   into lw_check
                                   it_price-maktx   into lw_maktx
                                   it_price-e_wramt into lw_wramt
                                   it_price-e_wrbtr into lw_wrbtr
                                   it_price-e_peinh into lw_peinh.
    if sy-subrc ne 0. exit. endif.
    check not it_price-matnr is initial.

    read table it_price with key matnr  = it_price-matnr.
    if sy-subrc ne 0.
      message e000(zz) with text-m02.
    endif.

    check not lw_maktx is initial and
            ( it_price-flag eq c_ready  or
              it_price-flag eq c_failed or
              it_price-flag eq c_success ).

    move: sy-tabix to lw_line_idx.

    perform price_unit_check  using lw_peinh.

    if     it_price-type = 'KD'.
      perform numeric_check_rtn using lw_wrbtr.

      it_price-e_peinh = lw_peinh.
      it_price-e_wrbtr = lw_wrbtr.

      it_price-check   = lw_check.
      it_price-e_wcost = lw_wrbtr * it_price-rcost / 1000.
      it_price-e_wfrgt = lw_wrbtr * it_price-rfrgt / 1000.
      it_price-e_wduty = lw_wrbtr * it_price-rduty / 1000.
      it_price-e_wramt = it_price-e_wrbtr + it_price-e_wcost +
                         it_price-e_wfrgt + it_price-e_wduty.
      it_price-e_dmbtr = it_price-e_wrbtr * it_price-e_wkurs.
      it_price-e_dcost = it_price-e_wcost * it_price-e_wkurs.
      it_price-e_dfrgt = it_price-e_wfrgt * it_price-e_wkurs.
      it_price-e_dduty = it_price-e_wduty * it_price-e_wkurs.
      it_price-e_dmamt = it_price-e_wramt * it_price-e_wkurs.
    elseif it_price-type = 'LP'.
      perform numeric_check_rtn using lw_wramt.

      it_price-check   = lw_check.
      it_price-e_peinh = lw_peinh.
      it_price-e_wrbtr = lw_wramt.
      it_price-e_wramt = lw_wramt.
      it_price-e_dmbtr = lw_wramt.
      it_price-e_dmamt = lw_wramt.
    endif.

    modify it_price index lw_line_idx.
  enddo.
endform.                    " read_screen_data_for_enter
*&---------------------------------------------------------------------*
*&      Form  price_unit_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LW_PEINH  text
*----------------------------------------------------------------------*
form price_unit_check using pw_peinh.
  perform numeric_check_rtn using pw_peinh.

  case pw_peinh.
    when 0.
      message e000(zz) with text-m09 it_price-matnr.
    when 1 or 10.
    when others.
      message e000(zz) with text-m10 it_price-matnr.
  endcase.
endform.                    " PRICE_UNIT_CHECK
*&---------------------------------------------------------------------*
*&      Form  check_future_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_future_price.
*----- Check Undetermined Price
  clear: zvmm_info_condi.
  select single *
    from zvmm_info_condi
   where knumh    = it_knumh-knumh
     and kschl    = c_kschl
     and loevm_ko = ' '.
  if sy-subrc ne 0.
    perform append_condition_price.
  endif.

  select single *
   from a018
  where kappl =  'M'
    and kschl =  'PB00'
    and matnr =  wa_price-matnr
    and lifnr =  it_knumh-lifnr
    and ekorg =  c_ekorg
    and esokz =  '0'
    and datab > wa_base_d.
  if sy-subrc eq 0.
    perform read_future_price.
  else.
    perform append_condition_price.
  endif.
endform.                    " check_future_price
