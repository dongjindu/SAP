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
* 3/28/2005  Chris        UD1K915193  Material gross price (type PB00)
*                                     has been changed to the sum of
*                                     type PB00 and type ZTIR because
*                                     Tire and steel wheel module part
*                                     has been split into two part:
*                                     steel wheel and tire.
*01/03/06   Manjunath    UD1K918798   Program changes to display error
*                                     message/For KD materials.
*
************************************************************************
report zemmpm34r_st_price_manage no standard page heading
                                 line-size  122
                                 line-count  65.

include: <icon>.
type-pools : slis.
tables: mara,
        a018,
        ztmm_analy,
        ztmm_spe,
        ztmm_analy_new,
        ztmm_spe_new,
        zvmm_info_condi.

*----- Internal tables
data: begin of it_price occurs 0.
        include structure ztmm_analy.
data:   wramt    like ztmm_analy-wrbtr,
        dmamt    like ztmm_analy-wrbtr,
        profl    like mara-profl,
        maktx    like makt-maktx,
        vtext    like t686d-vtext,  "reason code text
        meins    like mara-meins,
        peinh_mm like mbew-peinh,
        stawn    like a902-stawn,          "HS Code
        type(2),
        check,
        flag,
        msg(100),
      end   of it_price.

data: it_temp_price like it_price occurs 0 with header line.
data: it_error like ztmm_spe      occurs 0 with header line.

data: it_idnrk like ztmm_analy_tmp occurs 0 with header line.

data: begin of it_halb occurs 0,
        werks   like   t001w-werks,
        idnrk   like   mara-matnr,
      end   of it_halb.

data: begin of it_annual_matnr occurs 0,
        matnr   like   mara-matnr,
        plnmg   like   pbhi-plnmg,
        dbmng   like   pbhi-dbmng,
      end   of it_annual_matnr.

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
          konwa   like   konp-konwa,
          kpein   like   konp-kpein,
          meins   like   mara-meins,
          kumne   like   konp-kumne,
          kumza   like   konp-kumza,
          wrbtr   type   f,
          kbetr   like   konp-kbetr,
          witax   like   konp-kbetr,
          wfrgt   like   konp-kbetr,
          wcost   like   konp-kbetr,
      end   of it_info.

data: begin of it_exchange_rate occurs 0,
        waers   like   ekko-waers,         "Currency
        wkurs   type   f,                  "Exchange rate
      end   of it_exchange_rate.

data: begin of it_err_msg occurs 0,
        base_d   like   sy-datum,
        matnr    like   mara-matnr,
        id       like   bapiret2-id,
        msgno    like   bapiret2-number,
        v1       like   bapiret2-message_v1,
        v2       like   bapiret2-message_v2,
        v3       like   bapiret2-message_v3,
        v4       like   bapiret2-message_v4,
      end   of it_err_msg.

data: it_zvmm_info_condi like zvmm_info_condi occurs 0 with header line.

data: it_bapiret2   like bapiret2 occurs 0 with header line.

*----- Work areas
data: begin of wa_price,
        matnr      like mara-matnr,          "Material
        meins      like mara-meins,          "Master UoM
        profl      like mara-profl,          "KD,LD Separator
        witax      like konp-kbetr,          "Import tax rate
        peinh_mm   like mbew-peinh,          "Material Master Price Unit
        maktx      like makt-maktx,          "Description
        stawn      like a902-stawn,          "HS Code
        period     like ztmm_analy-period,   "Error Period
        werks      like ztmm_analy-werks,    "Error Plant
        ekorg      like ztmm_analy-ekorg,    "Error Purchase Org.
        kzust      like ztmm_analy-kzust,    "Error Reason code
        valid_d    like ztmm_analy-valid_d,  "Error Valid date
        wrbtr      like ztmm_analy-wrbtr,    "Error Foreign Pice
        wduty      like ztmm_analy-wrbtr,    "Error duty of F.Curr
        wfrgt      like ztmm_analy-wfrgt,    "Error freight of F.Curr
        wcost      like ztmm_analy-wrbtr,    "Error cost of F.Curr
        dmbtr      like ztmm_analy-wrbtr,    "Error Local Price
        dduty      like ztmm_analy-wrbtr,    "Error duty of L.Curr
        dfrgt      like ztmm_analy-wfrgt,    "Error freight of L.Curr
        dcost      like ztmm_analy-wrbtr,    "Error cost of L.Curr
        wramt      like ztmm_analy-wrbtr,    "Error Foreign Amount
        dmamt      like ztmm_analy-wrbtr,    "Error Local Amount
        peinh      like ztmm_analy-peinh,    "Error Price unit
        waers      like ztmm_analy-waers,    "Error Currency
        wkurs      like ztmm_analy-wkurs,    "Error exchange rate
        lifnr      like ztmm_analy-lifnr,    "Error vendor
        f_period   like ztmm_analy-period,   "Finish Period
        f_werks    like ztmm_analy-werks,    "Finish Plant
        f_ekorg    like ztmm_analy-ekorg,    "Finish Purchase Org.
        f_kzust    like ztmm_analy-kzust,    "Finish Reason code
        f_valid_d  like ztmm_analy-valid_d,  "Finish Valid date
        f_wrbtr    like ztmm_analy-wrbtr,    "Finish Foreign Pice
        f_wduty    like ztmm_analy-wrbtr,    "Finish duty of F.Curr
        f_wfrgt    like ztmm_analy-wfrgt,    "fINISH freight of F.Curr
        f_wcost    like ztmm_analy-wrbtr,    "Finish cost of F.Curr
        f_dmbtr    like ztmm_analy-wrbtr,    "Finish Local Price
        f_dduty    like ztmm_analy-wrbtr,    "Finish duty of L.Curr
        f_dfrgt    like ztmm_analy-wfrgt,    "fINISH freight of L.Curr
        f_dcost    like ztmm_analy-wrbtr,    "Finish cost of L.Curr
        f_wramt    like ztmm_analy-wrbtr,    "Finish Foreign Amount
        f_dmamt    like ztmm_analy-wrbtr,    "Finish Local Amount
        f_peinh    like ztmm_analy-peinh,    "Finish Price unit
        f_waers    like ztmm_analy-waers,    "Finish Currency
        f_wkurs    like ztmm_analy-wkurs,    "Finish exchange rate
        f_lifnr    like ztmm_analy-lifnr,    "Finish vendor
        f_source   like ztmm_analy-source,   "Finish source
      end   of wa_price.

data: wa_amount       type   f,                "Amount of Base UoM
      wa_low_amt      type   f,                "lower amount of Base UoM
      wa_matnr_f      like   mara-matnr,       "Material From
      wa_matnr_t      like   mara-matnr,       "Material To
      wa_period       like   ztmm_analy-period,"Period
      wa_vakey        like   konh-vakey,       "Value Key
      wa_progress_idx type   i,                "Progress bar index
      wa_count        type   i,                "Count of Material
      wa_cnt           type  i,                "Total count
      wa_count_ready  type   i,                "Count of ready
      wa_count_finish type   i,                "Count of finished
      wa_count_fail   type   i,                "Count of failed
      wa_count_error  type   i,                "Count of error
      wa_count_run    type   i,                "Count of running item
      wa_pdate        type   d,                "Plan Price date
      wa_pdatu_f      type   d,
      wa_pdatu_t      type   d.

* Begin of changes - UD1K918798
data : flag_KDLP type c,
       wa_LAND1 like lfa1-land1.
* End of changes - UD1K918798
constants: c_mark                   value 'X',   "Marker
           c_ekorg like ekko-ekorg  value 'PU01',"Purchase Org.
           c_kschl like konp-kschl  value 'PB00',"Type of amount
           c_frght like konp-kschl  value 'FRA1',"Type of freight
           c_con01 like konp-kschl  value 'ZOTH',"Type of ETC rate
           c_con02 like konp-kschl  value 'ZOTI',"Type of ETC rate
           c_waers like ekko-waers  value 'USD', "Currency
           c_price like konp-kbetr  value '0.01',"Default Price
           c_ready                  value '1',   "Ready item
           c_fail                   value '2',   "Update failed item
           c_success                value '3',   "Update successful item
           c_error                  value '4',   "Error item
           c_hs_error               value '5',   "HS Code Error
*          steel wheel and tire moduel price has been split into two
*          parts: one price for tire(ztir) and one price for steel
*          wheel(pb00)
           c_tire_price_type like ekko-ekorg value 'ZTIR'. "TIRE PRICE
*----- Selection screens
selection-screen begin of block bl1 with frame title text-t01.
parameters:     p_werks like t001w-werks default 'P001' obligatory.

selection-screen begin of line.
selection-screen comment  1(31) text-t12 for field r_year.
parameters:     p_year  like bseg-gjahr default sy-datum(4) obligatory,
                p_month like t54c6-smont default sy-datum+4(2).
selection-screen end of line.

select-options: s_matnr for  mara-matnr no-extension.
parameters:     p_datum like sy-datum default sy-datum obligatory.
selection-screen end   of block bl1.

selection-screen begin of block bl2 with frame title text-t02.
selection-screen begin of line.
selection-screen position 1.
parameters       r_year radiobutton group rd1 user-command rd1.
selection-screen comment  3(20) text-t03 for field r_year.
selection-screen position 33.
parameters       r_quar radiobutton group rd1 default 'X'.  "UD1K918798
selection-screen comment 35(30) text-t04 for field r_quar.
selection-screen end of line.

parameters: p_stlan like stzu-stlan default '6'.
*selection-screen skip.


*SELECTION-SCREEN BEGIN OF LINE.
**SELECTION-SCREEN uline.
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
selection-screen end   of block bl2.


*----- Screen Attribute Control
at selection-screen output.
  perform set_screen_attribute.
  perform set_quarter.                                   " Set Quarter

*----- Get Data
at selection-screen.
  check sy-ucomm = 'ONLI'.
  perform check_rtn.
  perform get_data.

**---
top-of-page.
  perform top_of_page.

top-of-page during line-selection.
  perform top_of_page.

start-of-selection.
  set pf-status 'BASE'.
  perform write_data.

at user-command.
  case sy-ucomm.
    when 'EXCUTE'.
      sy-lsind = sy-lsind - 1.
      perform excute_rtn.
    when 'SAVE'.
      perform error_save_rtn.
    when 'S_ALL'.
      sy-lsind = sy-lsind - 1.
      perform select_all_rtn.
    when 'D_ALL'.
      sy-lsind = sy-lsind - 1.
      perform deselect_all_rtn.
    when 'DELETE'.
      perform delete_rtn.
    when 'DOWNLOAD'.
      perform download_rtn.
  endcase.

at line-selection.
  perform double_click_rtn.

*&---------------------------------------------------------------------*
*&      Form  set_auarter
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
endform.                    " set_auarter
*&---------------------------------------------------------------------*
*&      Form  set_screen_attribute
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_screen_attribute.
  loop at screen.
    case c_mark.
      when r_year.
        if screen-name = 'R_QUAR_1' or screen-name = 'R_QUAR_2' or
           screen-name = 'R_QUAR_3' or screen-name = 'R_QUAR_4' or
           screen-name = 'P_MONTH'.
          screen-input = '0'.
        endif.
      when r_quar.
        if screen-name = 'R_QUAR_1' or screen-name = 'R_QUAR_2' or
           screen-name = 'R_QUAR_3' or screen-name = 'R_QUAR_4' or
           screen-name = 'P_MONTH'.
          screen-input = '1'.
        endif.
    endcase.
    modify screen.
  endloop.
endform.                    " set_screen_attribute
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_data.
  case c_mark.
    when r_year.
      perform get_annual_plan.
    when r_quar.
      perform get_quater_plan.
  endcase.
endform.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  APPEND_STANDARD_PRICE_LP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form append_standard_price.
  clear: wa_vakey.

  if r_quar eq 'X'.
    select single * from ztmm_spe_new where period = wa_period
                                        and werks  = wa_price-werks
                                        and matnr  = wa_price-matnr.
    if sy-subrc eq 0.
      exit.
    endif.

    select single * from ztmm_analy_new where period = wa_period
                                          and werks  = wa_price-werks
                                          and matnr  = wa_price-matnr.
    if sy-subrc eq 0.
      exit.
    endif.
  endif.

  perform display_progress_bar.

*----- finished item
  if not wa_price-f_period is initial.
    perform append_finish_price.
    clear: wa_price.
    wa_cnt = wa_cnt + 1.
    check 1 = 0.
  endif.

*----- Read suitable Price
  clear: it_knumh, it_knumh[].
  select knumh datab matnr lifnr
    into corresponding fields of table it_knumh
    from a018
   where kappl =  'M'
     and kschl =  'PB00'
     and matnr =  wa_price-matnr
     and ekorg =  c_ekorg
     and esokz =  '0'
     and datab <= p_datum
     and datbi >= p_datum.

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

  sort it_knumh by datab descending.

*----- Read the lowest price
  read table it_knumh index 1.
  if sy-subrc eq 0.                 "Standard Price
    delete it_knumh where datab < it_knumh-datab.
* Begin of changes - UD1K918798
    if WA_PRICE-PROFL eq 'K' .
      select single LAND1 into wa_LAND1 from LFA1
       where lifnr eq it_knumh-lifnr.
      if sy-subrc eq 0 and wa_land1 eq 'US'.
        exit.
      endif.
    endif.
* End of changes - UD1K918798

    perform check_future_price.

  else.                             "Temp price, No price
    perform read_future_price.
  endif.

  clear: wa_price.
  wa_cnt = wa_cnt + 1.
endform.                    " APPEND_STANDARD_PRICE_LP
*&---------------------------------------------------------------------*
*&      Form  calculate_amount
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_PRICE  text
*----------------------------------------------------------------------*
form calculate_amount using pwa_amount.
*----- Calculate amount
  data: lw_umrez like eina-umrez,        "Numerator
        lw_umren like eina-umren,        "Denomirator
        lw_crate type i,                 "Conversion rate
        lw_wrbtr(16) type p decimals 10.                 "Amount

*----- Info UoM =  Master UoM
  if it_info-meins eq wa_price-meins.
    pwa_amount =   ( it_info-kbetr / it_info-kpein )
               + ( ( it_info-kbetr / it_info-kpein ) *
                   ( it_info-witax / 1000          ) )
               + ( ( it_info-kbetr / it_info-kpein ) *
                   ( it_info-wfrgt / 1000          ) )
               + ( ( it_info-kbetr / it_info-kpein ) *
                   ( it_info-wcost / 1000          ) ).

    lw_wrbtr = it_info-kbetr / it_info-kpein.

    perform check_decimal_part using lw_wrbtr lw_crate.

    it_info-wrbtr = ( it_info-kbetr / it_info-kpein ) *
                      lw_crate.

    it_info-kpein = lw_crate.
  else.                                    "Info UoM <> Master UoM
*----- Read General Conversion rule
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

    perform check_decimal_part using lw_wrbtr lw_crate.

    it_info-wrbtr = ( it_info-kbetr / it_info-kpein ) /
                    ( it_info-kumne * it_info-kumza ) /
                    ( lw_umrez      * lw_umren      ) *
                    lw_crate.

    it_info-kpein = lw_crate.
  endif.
endform.                    " calculate_amount

*&---------------------------------------------------------------------*
*&      Form  check_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_rtn.
  perform check_matnr.
  perform check_period.
  perform check_base_date.
  perform check_year.
  perform locking_rtn.
endform.                    " check_rtn
*&---------------------------------------------------------------------*
*&      Form  check_matnr
*&---------------------------------------------------------------------*
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
*&      Form  append_blank_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form append_blank_price.
  clear: it_price.


  if wa_price-period is initial.
    " No info, No error item
    perform append_error_price.
  else.
    if wa_price-wrbtr eq 0.
      " No info, No error input item
      perform append_error_zero_price.
    else.
      " No info, Error input item
      perform append_ready_price_for_error.
    endif.
  endif.
endform.                    " append_blank_price
*&---------------------------------------------------------------------*
*&      Form  read_exchange_rate
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_exchange_rate using p_wkurs.
  data: lw_wkurs type f.


  read table it_exchange_rate with key waers = it_price-waers.
  if sy-subrc ne 0.
    call function 'Z_FCA_GET_EXCHANGE_RATE'
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
              others                  = 3.
    if sy-subrc <> 0.
      message e000(zz) with text-m03 it_price-waers.
    endif.

    move: it_price-waers to it_exchange_rate-waers,
          lw_wkurs       to it_exchange_rate-wkurs,
          lw_wkurs       to p_wkurs.

    append it_exchange_rate.
  endif.

  move it_exchange_rate-wkurs to p_wkurs.
endform.                    " read_exchange_rate

*&---------------------------------------------------------------------*
*&      Form  append_condition_price_for_KD
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
endform.                    " append_condition_price_for_KD
*&---------------------------------------------------------------------*
*&      Form  READ_CONDITION_BY_VENDOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
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
endform.                    " READ_CONDITION_BY_VENDOR
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
        move: wa_price-witax to it_info-witax.
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
*&      Form  SELECT_LOWEST_PRICE
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

    perform  calculate_amount using wa_amount.

    if wa_low_amt > wa_amount.
      wa_low_amt  = wa_amount.
      wa_vakey    = it_info-vakey.
    endif.

    modify it_info.
  endloop.
endform.                    " SELECT_LOWEST_PRICE
*&---------------------------------------------------------------------*
*&      Form  APPEND_PRICE_TO_IT_PRICE
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

  if it_info-kzust(1) eq 'X'.         " X price
    if wa_price-period is initial.
      "Info exist, error not exist

      perform append_error_x_price.
    else.
      if wa_price-wrbtr is initial.
        "Info exist, error exist, price 0
        perform append_error_x_price.
      else.
        "Info exist, error exist
        perform append_ready_price_for_error.
      endif.
    endif.
  else.
    "Normal price exist
    perform append_normal_price.
  endif.
endform.                    " APPEND_PRICE_TO_IT_PRICE
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

  write:/01(122) text-h06 centered.
  write:/01(122) text-h07 centered.

  new-line.

  write:/09    text-h09,
         25(7) wa_cnt.

  write:/02(4) icon_yellow_light as icon,
               text-h03,
         25(7) wa_count_ready.

  write: 40(4) icon_green_light  as icon,
               text-h04,
         63(7) wa_count_finish.

  write: 100   text-h10, wa_period.

  write:/02(4) icon_red_light    as icon,
               text-h05,
         25(7) wa_count_fail.

  write: 40(4) icon_light_out    as icon,
               text-h08,
         63(7) wa_count_error.

  write: 100   text-h11, p_datum.

  format color col_heading intensified off.

  uline.
  write:/ text-h01.
  write:/ text-h02.
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
  data: lw_color_flg.

  sort it_price by flag type matnr.

  loop at it_price.
    reserve 3 lines.

    perform set_format_color using lw_color_flg.

    if it_price-flag eq c_error   or
       it_price-flag eq c_success or
       it_price-flag eq c_hs_error.
      write:/ it_price-check as checkbox input off.
    else.
      write:/ it_price-check as checkbox.
    endif.

    case it_price-flag.
      when c_ready.
        write: (4) icon_yellow_light as icon.
      when c_fail.
        write: (4) icon_red_light    as icon hotspot.
      when c_success.
        write: (4) icon_green_light  as icon.
      when c_error or c_hs_error.
        write: (4) icon_light_out    as icon hotspot.
    endcase.
    write:  (18) it_price-matnr,
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

    hide: it_price, sy-tabix.


    write:/08(56) it_price-maktx,
             (12) it_price-dmamt,
             (12) it_price-dmbtr,
             (10) it_price-dduty,
             (10) it_price-dfrgt,
             (10) it_price-dcost.
* Begin of changes - UD1K918798
    if it_price-msg ne ''.
      write :/25(85) it_price-msg.
    endif.
* End  of changes - UD1K918798

    uline.
  endloop.

  clear: it_price.
endform.                    " write_data
*&---------------------------------------------------------------------*
*&      Form  set_format_color
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_format_color using pw_color_flg.
  if pw_color_flg eq space.
    format intensified off.
    move : 'X' to pw_color_flg.
  else.
    format intensified on.
    clear : pw_color_flg.
  endif.

  case it_price-flag.
    when c_ready.
      format color 2 intensified off.
    when c_fail.
      format color 2 intensified on.
    when c_success.
      format color 7 intensified on.
    when c_error.
      format color 6 intensified off.
    when c_hs_error.
      format color 6 intensified on.
  endcase.
endform.                    " set_format_color
*&---------------------------------------------------------------------*
*&      Form  SELECT_ALL_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form select_all_rtn.
  move: 'X' to it_price-check.
  modify it_price transporting check where flag eq c_ready
                                        or flag eq c_fail.

  perform write_data.
endform.                    " SELECT_ALL_RTN
*&---------------------------------------------------------------------*
*&      Form  DESELECT_ALL_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form deselect_all_rtn.
  move: ' ' to it_price-check.
  modify it_price transporting check where flag eq c_ready
                                        or flag eq c_fail.

  perform write_data.
endform.                    " DESELECT_ALL_RTN
*&---------------------------------------------------------------------*
*&      Form  EXCUTE_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form excute_rtn.
  data: lw_line_idx type i.

  perform get_count_of_checked_item.

  do.
    clear: it_price.
    read line sy-index field value it_price-check
                                   it_price-matnr.
    if sy-subrc ne 0. exit. endif.
    check not it_price-check is initial.

    read table it_price with key base_d = it_price-base_d
                                 matnr  = it_price-matnr.
    if sy-subrc ne 0.
      message e000(zz) with text-m02.
    endif.

    perform display_progress_bar_running.

    move: sy-tabix to lw_line_idx.

    perform change_standard_price.

    modify it_price index lw_line_idx.
  enddo.

  perform write_data.
endform.                    " EXCUTE_RTN
*&---------------------------------------------------------------------*
*&      Form  CHANGE_STANDARD_PRICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form change_standard_price.
  data: wa_head       like bapimathead, "Header with control information
        wa_plant      like bapi_marc  , "plant-specific material DATA
        wa_plantx     like bapi_marcx ,
        wa_mbew       like bapi_mbew  ,
        wa_mbewx      like bapi_mbewx ,
        wa_amount     like bseg-wrbtr,
        wa_peinh      like ekpo-peinh.

  clear: it_bapiret2, it_bapiret2[].

  if it_price-peinh_mm eq 10.
    if it_price-peinh eq 10.
      move: it_price-dmamt  to wa_amount,
            it_price-peinh  to wa_peinh.
    else.
      wa_amount = it_price-dmamt * 10.
      wa_peinh  = 10.
    endif.
  else.
    move: it_price-dmamt  to wa_amount,
          it_price-peinh  to wa_peinh.
  endif.

  case c_mark.
    when r_year.                             "Annual Plan
      wa_head-material     = it_price-matnr.
      wa_head-cost_view    = 'X'.
      wa_plant-lot_size    = wa_peinh.
      wa_plantx-lot_size   = 'X'.
      wa_plant-plant       = it_price-werks.
      wa_plantx-plant      = it_price-werks.
      wa_mbew-val_area     = it_price-werks.
      wa_mbew-plndprice3   = wa_amount.
      wa_mbew-plndprdate3  = wa_pdate.
      wa_mbew-price_unit   = wa_peinh.
      wa_mbewx-val_area    = it_price-werks.
      wa_mbewx-plndprice3  = 'X'.
      wa_mbewx-plndprdate3 = 'X'.
      wa_mbewx-price_unit  = 'X'.
    when r_quar.                              "Quater Plan
      wa_head-material     = it_price-matnr.
      wa_head-cost_view    = 'X'.
      wa_plant-lot_size    = wa_peinh.
      wa_plantx-lot_size   = 'X'.
      wa_plant-plant       = it_price-werks.
      wa_plantx-plant      = it_price-werks.
      wa_mbew-val_area     = p_werks.
      wa_mbew-plndprice1   = wa_amount.
      wa_mbew-plndprdate1  = wa_pdate.
      wa_mbew-price_unit   = wa_peinh.
      wa_mbewx-val_area    = it_price-werks.
      wa_mbewx-plndprice1  = 'X'.
      wa_mbewx-plndprdate1 = 'X'.
      wa_mbewx-price_unit  = 'X'.
  endcase.

  if it_price-dmamt = 0.
    perform zero_price_error_rtn.
  else.
    call function 'BAPI_MATERIAL_SAVEDATA'
         EXPORTING
              headdata       = wa_head
              valuationdata  = wa_mbew
              valuationdatax = wa_mbewx
              plantdata      = wa_plant
              plantdatax     = wa_plantx
         TABLES
              returnmessages = it_bapiret2.

    loop at it_bapiret2 where type = 'E'
                           or type = 'A'.
    endloop.
    if sy-subrc eq 0.
      perform master_update_failed_rtn.
    else.
      perform master_update_success_rtn.
    endif.
  endif.
endform.                    " CHANGE_STANDARD_PRICE
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
    from mara as a inner join marc as b
      on a~matnr = b~matnr
     where a~matnr between wa_matnr_f and wa_matnr_t
       and a~mtart in ('ROH', 'ROH1')
       and a~profl in ('K',   'V')
       and a~lvorm <> 'X'
       and b~werks = p_werks.
  if sy-subrc ne 0.
* Begin of changes - UD1K918798
* Per Andy sometimes MIP/LP/KD Indicator in Basic data will be Blank
* So check against valuation class 3000 - KD (3001 - 3005) - LP
    clear flag_KDLP.
    select count( * ) into wa_count
      from mara as a inner join mbew as b
        on a~matnr = b~matnr
       where a~matnr between wa_matnr_f and wa_matnr_t
         and a~mtart in ('ROH', 'ROH1')
         and a~lvorm <> 'X'
         and b~BWKEY = p_werks
         and b~BKLAS in ('3000','3001','3002','3003','3004','3005').
    flag_KDLP = 'X'.
    if sy-subrc ne 0.
      clear flag_KDLP.
* End of changes - UD1K918798
      message e000(zz) with text-m01.
    endif.
  endif.

endform.                    " read_total_count
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
       EXPORTING
            percentage = lw_percentage
            text       = lw_text.
endform.                    " display_progress_bar
*&---------------------------------------------------------------------*
*&      Form  get_count_of_checked_item
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_count_of_checked_item.
*----- Read count of checked items
  clear: wa_count_run, wa_progress_idx.

  do.
    clear: it_price.
    read line sy-index field value it_price-check
                                   it_price-matnr.
    if sy-subrc ne 0. exit. endif.
    check not it_price-check is initial.

    wa_count_run = wa_count_run + 1.
  enddo.
endform.                    " get_count_of_checked_item
*&---------------------------------------------------------------------*
*&      Form  display_progress_bar_running
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form display_progress_bar_running.
  data: lw_percentage(3) type c,
        lw_mod type i,
        lw_text(50).

  wa_progress_idx = wa_progress_idx + 1.

  lw_percentage = wa_progress_idx / wa_count_run * 100.

  concatenate text-b02 lw_percentage '%' into lw_text.

  call function 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            percentage = lw_percentage
            text       = lw_text.
endform.                    " display_progress_bar_running
*&---------------------------------------------------------------------*
*&      Form  double_click_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form double_click_rtn.
*----- Call Material Master Display
  data: lw_field(50).

  get cursor field lw_field.

  case lw_field.
    when 'IT_PRICE-MATNR'.
      set parameter id 'MAT' field it_price-matnr.

      call transaction 'MM03' and skip first screen.
    when 'ICON_RED_LIGHT'.
      clear: it_err_msg.
      read table it_err_msg with key base_d = it_price-base_d
                                     matnr  = it_price-matnr.
      if sy-subrc eq 0.
        message id it_err_msg-id type 'S' number it_err_msg-msgno
                with it_err_msg-v1 it_err_msg-v2
                     it_err_msg-v3 it_err_msg-v4.
      else.
        message e000(zz) with text-m02.
      endif.
  endcase.

  clear: it_price.
endform.                    " double_click_rtn

*&---------------------------------------------------------------------*
*&      Form  INPUT_PRICE_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form input_price_rtn.
  set pf-status 'INPUT'.
  perform set_it_temp_price.
  perform write_temp_date.
endform.                    " INPUT_PRICE_RTN
*&---------------------------------------------------------------------*
*&      Form  SET_IT_TEMP_PRICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_it_temp_price.
  data: lw_line_idx type i.

  clear: it_temp_price, it_temp_price[].

  do.
    clear: it_price.
    read line sy-index field value it_price-check
                                   it_price-matnr.
    if sy-subrc ne 0. exit. endif.
    check not it_price-check is initial.

    read table it_price with key base_d = it_price-base_d
                                 matnr  = it_price-matnr
                                 binary search.
    if sy-subrc ne 0.
      message e000(zz) with text-m02.
    endif.

    check it_price-profl eq 'V'.

    move: sy-tabix to lw_line_idx.

    move: it_price to it_temp_price.

    append it_temp_price.
  enddo.
endform.                    " SET_IT_TEMP_PRICE
*&---------------------------------------------------------------------*
*&      Form  write_temp_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form write_temp_date.
  data: lw_color_flg.

  sort it_price by base_d matnr.

  loop at it_price.
    reserve 3 lines.

    perform set_format_color using lw_color_flg.

    write:/1(18) it_price-matnr,
                 it_price-valid_d,
                 it_price-kzust,
                 it_price-waers,
                 it_price-wramt,
                 it_price-wrbtr input,
                 it_price-wduty,
                 it_price-wcost.

    hide: it_price, sy-tabix.

    write:/1(39)  it_price-maktx,
                  it_price-dmamt,
                  it_price-dmbtr,
                  it_price-dduty,
                  it_price-dcost.

    uline.
  endloop.

  clear: it_price.
endform.                    " write_temp_date
*&---------------------------------------------------------------------*
*&      Form  master_update_failed_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form master_update_failed_rtn.
  call function 'BAPI_TRANSACTION_ROLLBACK'.

  delete it_err_msg where base_d = ztmm_spe-base_d
                      and matnr  = ztmm_spe-matnr.

  clear: it_err_msg.

  move: it_price-base_d        to it_err_msg-base_d,
        it_price-matnr         to it_err_msg-matnr,
        it_bapiret2-id         to it_err_msg-id,
        it_bapiret2-number     to it_err_msg-msgno,
        it_bapiret2-message_v1 to it_err_msg-v1,
        it_bapiret2-message_v2 to it_err_msg-v2,
        it_bapiret2-message_v3 to it_err_msg-v3,
        it_bapiret2-message_v4 to it_err_msg-v4.

  append it_err_msg.

  if it_price-flag eq c_ready.
    wa_count_ready = wa_count_ready - 1.
    wa_count_fail  = wa_count_fail + 1.
    it_price-flag  = c_fail.
  endif.

  it_price-check = 'X'.
endform.                    " master_update_failed_rtn
*&---------------------------------------------------------------------*
*&      Form  master_update_success_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form master_update_success_rtn.
  perform set_table_data_for_success.
  perform update_table_for_insert.
endform.                    " master_update_success_rtn
*&---------------------------------------------------------------------*
*&      Form  update_table_for_insert
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form update_table_for_insert.
  insert ztmm_analy.

  if sy-subrc ne 0.
    call function 'BAPI_TRANSACTION_ROLLBACK'.

    clear: it_err_msg.

    move: it_price-base_d to it_err_msg-base_d,
          it_price-matnr  to it_err_msg-matnr,
          'ZZ'            to it_err_msg-id,
          '000'           to it_err_msg-msgno,
          text-b03        to it_err_msg-v1.

    append it_err_msg.

    if it_price-flag = c_ready.
      it_price-flag  = c_fail.
      wa_count_fail  = wa_count_fail + 1.
      wa_count_ready = wa_count_ready - 1.
    endif.
  else.
    call function 'BAPI_TRANSACTION_COMMIT'
         EXPORTING
              wait = 'X'.

    if it_price-flag = c_ready.
      wa_count_ready = wa_count_ready - 1.
    elseif it_price-flag = c_fail.
      wa_count_fail  = wa_count_fail - 1.
    endif.

    it_price-flag  = c_success.
    it_price-check = ''.
    wa_count_finish = wa_count_finish + 1.
  endif.
endform.                    " update_table_for_insert
*&---------------------------------------------------------------------*
*&      Form  update_table_for_update
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form update_table_for_update.
  update ztmm_analy.
  if sy-subrc ne 0.
    call function 'BAPI_TRANSACTION_ROLLBACK'.

    clear: it_err_msg.

    move: it_price-base_d to it_err_msg-base_d,
          it_price-matnr  to it_err_msg-matnr,
          'ZZ'            to it_err_msg-id,
          '000'           to it_err_msg-msgno,
          text-b03        to it_err_msg-v1.

    append it_err_msg.

    if it_price-flag = c_ready.
      it_price-flag  = c_fail.
      wa_count_fail  = wa_count_fail + 1.
      wa_count_ready = wa_count_ready - 1.
    endif.
  else.
    call function 'BAPI_TRANSACTION_COMMIT'
         EXPORTING
              wait = 'X'.

    if it_price-flag = c_ready.
      wa_count_ready = wa_count_ready - 1.
    elseif it_price-flag = c_fail.
      wa_count_fail  = wa_count_fail - 1.
    endif.

    it_price-flag  = c_success.
    it_price-check = ''.
    wa_count_finish = wa_count_finish + 1.
  endif.
endform.                    " update_table_for_update
*&---------------------------------------------------------------------*
*&      Form  append_finish_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form append_finish_price.
  clear: it_price.

  move: wa_period          to it_price-period,
        p_datum            to it_price-base_d,
        wa_price-matnr     to it_price-matnr,
        wa_price-profl     to it_price-profl,
        wa_price-maktx     to it_price-maktx,
        wa_price-stawn     to it_price-stawn,
        wa_price-peinh_mm  to it_price-peinh_mm,
        wa_price-f_werks   to it_price-werks,
        wa_price-f_ekorg   to it_price-ekorg,
        wa_price-f_kzust   to it_price-kzust,
        wa_price-f_valid_d to it_price-valid_d,
        wa_price-f_wrbtr   to it_price-wrbtr,
        wa_price-f_wduty   to it_price-wduty,
        wa_price-f_wfrgt   to it_price-wfrgt,
        wa_price-f_wcost   to it_price-wcost,
        wa_price-f_dmbtr   to it_price-dmbtr,
        wa_price-f_dduty   to it_price-dduty,
        wa_price-f_dfrgt   to it_price-dfrgt,
        wa_price-f_dcost   to it_price-dcost,
        wa_price-f_peinh   to it_price-peinh,
        wa_price-meins     to it_price-meins,
        wa_price-f_waers   to it_price-waers,
        wa_price-f_wkurs   to it_price-wkurs,
        wa_price-f_source  to it_price-source,
        wa_price-f_lifnr   to it_price-lifnr,
        c_success          to it_price-flag.

  it_price-wramt = it_price-wrbtr + it_price-wduty +
                   it_price-wfrgt + it_price-wcost.
  it_price-dmamt = it_price-dmbtr + it_price-dduty +
                   it_price-dfrgt + it_price-dcost.

  append it_price.

  wa_count_finish = wa_count_finish + 1.
endform.                    " append_finish_price
*&---------------------------------------------------------------------*
*&      Form  append_ready_price_for_error
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form append_ready_price_for_error.
  clear: it_price.

  move: wa_period        to it_price-period,
        wa_price-werks   to it_price-werks,
        p_datum          to it_price-base_d,
        wa_price-matnr   to it_price-matnr,
        wa_price-profl   to it_price-profl,
        wa_price-maktx   to it_price-maktx,
        wa_price-stawn   to it_price-stawn,
        wa_price-peinh_mm   to it_price-peinh_mm,
        wa_price-werks   to it_price-werks,
        wa_price-ekorg   to it_price-ekorg,
        wa_price-kzust   to it_price-kzust,
        wa_price-valid_d to it_price-valid_d,
        wa_price-wrbtr   to it_price-wrbtr,
        wa_price-wduty   to it_price-wduty,
        wa_price-wfrgt   to it_price-wfrgt,
        wa_price-wcost   to it_price-wcost,
        wa_price-dmbtr   to it_price-dmbtr,
        wa_price-dduty   to it_price-dduty,
        wa_price-dfrgt   to it_price-dfrgt,
        wa_price-dcost   to it_price-dcost,
        wa_price-peinh   to it_price-peinh,
        wa_price-meins   to it_price-meins,
        wa_price-waers   to it_price-waers,
        wa_price-wkurs   to it_price-wkurs,
        'M'              to it_price-source,
        wa_price-lifnr   to it_price-lifnr,
        c_ready          to it_price-flag.

  it_price-wramt = it_price-wrbtr + it_price-wduty + it_price-wfrgt +
                   it_price-wcost.
  it_price-dmamt = it_price-dmbtr + it_price-dduty + it_price-dfrgt +
                   it_price-dcost.

  if     wa_price-profl eq 'K'.
    it_price-type = 'KD'.
  elseif wa_price-profl eq 'V'.
    it_price-type = 'LP'.
  endif.

  append it_price.

  wa_count_ready = wa_count_ready + 1.
endform.                    " append_ready_price_for_error
*&---------------------------------------------------------------------*
*&      Form  check_period
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_period.
  case c_mark.
    when r_year.
      concatenate p_year '13' into wa_period.
      move: p_datum           to   wa_pdate.
    when r_quar.
*      IF     r_quar_1 EQ 'X'.
*        CONCATENATE p_year '01' INTO wa_period.
*        CONCATENATE p_year '0101' INTO wa_pdate.
*      ELSEIF r_quar_2 EQ 'X'.
*        CONCATENATE p_year '02' INTO wa_period.
*        CONCATENATE p_year '0401' INTO wa_pdate.
*      ELSEIF r_quar_3 EQ 'X'.
*        CONCATENATE p_year '03' INTO wa_period.
*        CONCATENATE p_year '0701' INTO wa_pdate.
*      ELSEIF r_quar_4 EQ 'X'.
*        CONCATENATE p_year '04' INTO wa_period.
*        CONCATENATE p_year '1001' INTO wa_pdate.
*      ENDIF.
      concatenate p_year p_month      into wa_period.
      concatenate p_year p_month '01' into wa_pdate.
  endcase.
endform.                    " check_period
*&---------------------------------------------------------------------*
*&      Form  append_error_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form append_error_price.
  clear: it_price.

  move: wa_period      to it_price-period,
        wa_price-werks to it_price-werks,
        p_datum        to it_price-base_d,
        wa_price-matnr to it_price-matnr,
        wa_price-profl to it_price-profl,
        wa_price-maktx to it_price-maktx,
        wa_price-stawn to it_price-stawn,
        wa_price-peinh_mm   to it_price-peinh_mm,
        ' '            to it_price-ekorg,
        ' '            to it_price-kzust,
        p_datum        to it_price-valid_d,
        1              to it_price-peinh,
        wa_price-meins to it_price-meins,
        c_waers        to it_price-waers,
        1              to it_price-wkurs,
        'M'            to it_price-source,
        ''             to it_price-lifnr,
        c_error        to it_price-flag.

  if     wa_price-profl eq 'K'.
    it_price-type = 'KD'.
  elseif wa_price-profl eq 'V'.
    it_price-type = 'LP'.
  endif.

  append it_price.

  wa_count_error = wa_count_error + 1.
endform.                    " append_error_price
*&---------------------------------------------------------------------*
*&      Form  ERROR_SAVE_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form error_save_rtn.
*----- Save error items
  clear: it_error, it_error[].

  loop at it_price where flag = c_error.
    move: it_price-period to it_error-period,
          it_price-base_d to it_error-base_d,
          it_price-matnr  to it_error-matnr,
          it_price-werks  to it_error-werks,
          it_price-ekorg  to it_error-ekorg,
          it_price-kzust  to it_error-kzust,
          it_price-waers  to it_error-waers,
          it_price-wkurs  to it_error-wkurs,
          it_price-peinh  to it_error-peinh,
          it_price-lifnr  to it_error-lifnr,
          sy-uname        to it_error-ernam,
          sy-datum        to it_error-erdat,
          sy-uzeit        to it_error-erzet,
          sy-uname        to it_error-aenam,
          sy-datum        to it_error-aedat,
          sy-uzeit        to it_error-aezet.
    append it_error.
  endloop.

*----- Delete 0 price items
  delete from ztmm_spe where period = it_price-period
                         and werks  = p_werks
                         and submt  = ' '
                         and wrbtr  = 0.

  insert ztmm_spe from table it_error accepting duplicate keys.
  if sy-subrc ne 0.
    rollback work.
    message e000(zz) with text-m12.
  else.
    commit work and wait.
    message s000(zz) with text-m13.
  endif.
endform.                    " ERROR_SAVE_RTN
*&---------------------------------------------------------------------*
*&      Form  append_error_zero_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form append_error_zero_price.
  clear: it_price.

  move: wa_period        to it_price-period,
        wa_price-werks to it_price-werks,
        p_datum          to it_price-base_d,
        wa_price-matnr   to it_price-matnr,
        wa_price-profl   to it_price-profl,
        wa_price-maktx   to it_price-maktx,
        wa_price-stawn   to it_price-stawn,
        wa_price-peinh_mm   to it_price-peinh_mm,
        wa_price-werks   to it_price-werks,
        wa_price-ekorg   to it_price-ekorg,
        wa_price-kzust   to it_price-kzust,
        wa_price-valid_d to it_price-valid_d,
        wa_price-wrbtr   to it_price-wrbtr,
        wa_price-wfrgt   to it_price-wfrgt,
        wa_price-wduty   to it_price-wduty,
        wa_price-wcost   to it_price-wcost,
        wa_price-dmbtr   to it_price-dmbtr,
        wa_price-dduty   to it_price-dduty,
        wa_price-dfrgt   to it_price-dfrgt,
        wa_price-dcost   to it_price-dcost,
        wa_price-peinh   to it_price-peinh,
        wa_price-waers   to it_price-waers,
        wa_price-meins   to it_price-meins,
        wa_price-wkurs   to it_price-wkurs,
        'M'              to it_price-source,
        wa_price-lifnr   to it_price-lifnr,
        c_error          to it_price-flag.

  it_price-wramt = it_price-wrbtr + it_price-wduty +
                   it_price-wfrgt + it_price-wcost.
  it_price-dmamt = it_price-dmbtr + it_price-dduty +
                   it_price-dfrgt + it_price-dcost.

  if     wa_price-profl eq 'K'.
    it_price-type = 'KD'.
  elseif wa_price-profl eq 'V'.
    it_price-type = 'LP'.
  endif.

* Begin of changes - UD1K918798
  it_price-msg = 'No Info record exists -  Zero price'.
* End of changes - UD1K918798


  append it_price.

  wa_count_error = wa_count_error + 1.
endform.                    " append_error_zero_price
*&---------------------------------------------------------------------*
*&      Form  append_error_X_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form append_error_x_price.
  clear: it_price.

  move: wa_period           to it_price-period,
        wa_price-werks      to it_price-werks,
        p_datum             to it_price-base_d,
        wa_price-matnr      to it_price-matnr,
        wa_price-profl      to it_price-profl,
        wa_price-werks      to it_price-werks,
        wa_price-maktx      to it_price-maktx,
        wa_price-stawn      to it_price-stawn,
        wa_price-peinh_mm   to it_price-peinh_mm,
        it_info-kzust       to it_price-kzust,
        it_info-datab       to it_price-valid_d,
        it_info-wrbtr       to it_price-wrbtr,
        wa_price-meins      to it_price-meins,
        it_info-konwa       to it_price-waers,
        'M'                 to it_price-source,
        it_info-vakey(10)   to it_price-lifnr,
        it_info-vakey+28(4) to it_price-ekorg.

  if it_info-kpein eq 0.
    move: 1                   to it_price-peinh.
  else.
    move: it_info-kpein       to it_price-peinh.
  endif.

  it_price-wcost = it_price-wrbtr * it_info-wcost / 1000.
  it_price-wduty = it_price-wrbtr * it_info-witax / 1000.
  it_price-wfrgt = it_price-wrbtr * it_info-wfrgt / 1000.
  it_price-wramt = it_price-wrbtr + it_price-wcost + it_price-wfrgt +
                   it_price-wduty.

  perform read_exchange_rate using it_price-wkurs.

  it_price-dmbtr = it_price-wrbtr * it_price-wkurs.
  it_price-dduty = it_price-wduty * it_price-wkurs.
  it_price-dfrgt = it_price-wfrgt * it_price-wkurs.
  it_price-dcost = it_price-wcost * it_price-wkurs.
  it_price-dmamt = it_price-dmbtr + it_price-dcost + it_price-dfrgt +
                   it_price-dduty.

  move: c_error to it_price-flag.
  wa_count_error = wa_count_error + 1.

  if     wa_price-profl eq 'K'.
    it_price-type = 'KD'.
  elseif wa_price-profl eq 'V'.
    it_price-type = 'LP'.
  endif.
* Begin of changes - UD1K918798
  it_price-msg = 'Reason Code has X Indicator'.
* End of changes - UD1K918798
  append it_price.
endform.                    " append_error_X_price
*&---------------------------------------------------------------------*
*&      Form  append_normal_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form append_normal_price.
  clear: it_price.

  move: wa_period           to it_price-period,
        wa_price-werks      to it_price-werks,
        p_datum             to it_price-base_d,
        wa_price-matnr      to it_price-matnr,
        wa_price-profl      to it_price-profl,
        wa_price-werks      to it_price-werks,
        wa_price-maktx      to it_price-maktx,
        wa_price-stawn      to it_price-stawn,
        wa_price-peinh_mm   to it_price-peinh_mm,
        it_info-kzust       to it_price-kzust,
        it_info-datab       to it_price-valid_d,
        it_info-wrbtr       to it_price-wrbtr,
        it_info-kpein       to it_price-peinh,
        wa_price-meins      to it_price-meins,
        it_info-konwa       to it_price-waers,
        'I'                 to it_price-source,
        it_info-vakey(10)   to it_price-lifnr,
        it_info-vakey+28(4) to it_price-ekorg.

  it_price-wcost = it_price-wrbtr * it_info-wcost / 1000.
  it_price-wfrgt = it_price-wrbtr * it_info-wfrgt / 1000.
  it_price-wduty = it_price-wrbtr * it_info-witax / 1000.
  it_price-wramt = it_price-wrbtr + it_price-wcost + it_price-wfrgt +
                   it_price-wduty.

  perform read_exchange_rate using it_price-wkurs.

  it_price-dmbtr = it_price-wrbtr * it_price-wkurs.
  it_price-dduty = it_price-wduty * it_price-wkurs.
  it_price-dfrgt = it_price-wrbtr * it_info-wfrgt / 1000.
  it_price-dcost = it_price-wcost * it_price-wkurs.
  it_price-dmamt = it_price-dmbtr + it_price-dcost + it_price-dfrgt +
                   it_price-dduty.

  move: c_ready             to it_price-flag.
  wa_count_ready = wa_count_ready + 1.

  if     wa_price-profl eq 'K'.
    it_price-type = 'KD'.
  elseif wa_price-profl eq 'V'.
    it_price-type = 'LP'.
  endif.

  append it_price.
endform.                    " append_normal_price
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
       EXPORTING
            mode_ztmm_spe  = 'E'
            mandt          = sy-mandt
            werks          = p_werks
            period         = wa_period
            submt          = ' '
       EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            others         = 3.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.
endform.                    " locking_rtn
*&---------------------------------------------------------------------*
*&      Form  get_annual_plan
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_annual_plan.
  clear: it_exchange_rate, it_exchange_rate[].
  clear: wa_price.

  perform locking_rtn.


  clear: it_annual_matnr, it_annual_matnr[],
         it_idnrk,        it_idnrk[],
         it_halb,         it_halb[].

*----- Read PIR(Target Fullspec code)
  select matnr sum( plnmg ) sum( dbmng )
    into table it_annual_matnr
    from pbim as a inner join pbhi as b
      on a~bdzei = b~bdzei
     and b~pdatu between wa_pdatu_f and wa_pdatu_t
   where werks =  p_werks
     and matnr between wa_matnr_f and wa_matnr_t
     and bedae in ('VSE','VSF')
     and versb =  'Y1'
* by Andy Choi
*    and pbdnr =  'Y1'
     and loevr = space
   group by matnr.

*----- Read BOM Component of fullspec code
  loop at it_annual_matnr.
    check it_annual_matnr-plnmg ne it_annual_matnr-dbmng.

    perform display_progress_bar_bom using it_annual_matnr-matnr.

    perform get_bom_component using it_annual_matnr-matnr p_werks.
  endloop.

  sort it_idnrk by period werks idnrk.

*----- Read Engine BOM Component
  perform get_component_of_engine.

*----- Delete duplicate items
  delete adjacent duplicates from it_idnrk comparing all fields.

*----- Because of performance, save items to temp table
*----- And then, Read the other infomation by join
  delete from ztmm_analy_tmp where period = wa_period
                               and werks  = p_werks
                               and submt  = ' '.

  insert ztmm_analy_tmp from table it_idnrk accepting duplicate keys.

  commit work and wait.

  describe table it_idnrk lines wa_count.

  perform set_it_price_with_component.

  delete from ztmm_analy_tmp where period = wa_period
                               and werks  = p_werks
                               and submt  = ' '.

  perform check_hs_code.
endform.                    " get_annual_plan
*&---------------------------------------------------------------------*
*&      Form  get_quater_plan
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_quater_plan.
  clear: it_exchange_rate, it_exchange_rate[].
  clear: wa_price.

  perform read_total_count.

  clear: wa_cnt.
  if flag_KDLP is initial.                                  "UD1K918798
    EXEC SQL PERFORMING APPEND_STANDARD_PRICE.
      SELECT A.MATNR, A.MEINS, A.PROFL, D.KBETR, E.MAKTX,
             B.WERKS, F.EKORG, F.KZUST, F.WRBTR, F.WDUTY,
             F.WCOST, F.DMBTR, F.DDUTY, F.DCOST, F.WAERS,
             F.WKURS, F.LIFNR, G.WERKS, G.EKORG, G.KZUST,
             G.WRBTR, G.WDUTY, G.WCOST, G.DMBTR, G.DDUTY,
             G.DCOST, G.WAERS, G.WKURS, G.LIFNR, F.PERIOD,
             G.PERIOD,F.PEINH, G.PEINH, F.WFRGT, F.DFRGT,
             G.WFRGT, G.DFRGT, H.PEINH, G.SOURCE,C.STAWN
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
             :WA_PRICE-F_DFRGT, :WA_PRICE-PEINH_MM,:WA_PRICE-F_SOURCE,
             :WA_PRICE-STAWN
        FROM MARA A, MARC B, MBEW H, MAKT E, A902 C, KONP D, ZTMM_SPE F,
             ZTMM_ANALY G
       WHERE A.MANDT     = :SY-MANDT
         AND A.MATNR     BETWEEN :WA_MATNR_F AND :WA_MATNR_T
         AND A.MTART     IN ('ROH', 'ROH1')
         AND A.PROFL     IN ('K',   'V')
         AND A.LVORM     <> 'X'
         AND B.MANDT     =  A.MANDT
         AND B.MATNR     =  A.MATNR
         AND B.WERKS     =  :P_WERKS
         AND B.LVORM     =  ' '
*----- 2004.06.18 Requested by HSCHO, Changed by BSBAE
*----- Except submaterial
         AND B.DISPO     <> 'M02'
*----- 2004.06.18 Requested by HSCHO, Changed by BSBAE
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
         AND F.WERKS(+)  =  :P_WERKS
         AND F.MATNR(+)  =  A.MATNR
         AND F.SUBMT(+)  =  ' '
         AND G.MANDT(+)  =  A.MANDT
         AND G.PERIOD(+) =  :WA_PERIOD
         AND G.WERKS(+)  =  :P_WERKS
         AND G.MATNR(+)  =  A.MATNR
         AND G.SUBMT(+)  =  ' '
    ENDEXEC.
  else.                                                     "UD1K918798
    EXEC SQL PERFORMING APPEND_STANDARD_PRICE.
      SELECT A.MATNR, A.MEINS, A.PROFL, D.KBETR, E.MAKTX,
             B.WERKS, F.EKORG, F.KZUST, F.WRBTR, F.WDUTY,
             F.WCOST, F.DMBTR, F.DDUTY, F.DCOST, F.WAERS,
             F.WKURS, F.LIFNR, G.WERKS, G.EKORG, G.KZUST,
             G.WRBTR, G.WDUTY, G.WCOST, G.DMBTR, G.DDUTY,
             G.DCOST, G.WAERS, G.WKURS, G.LIFNR, F.PERIOD,
             G.PERIOD,F.PEINH, G.PEINH, F.WFRGT, F.DFRGT,
             G.WFRGT, G.DFRGT, H.PEINH, G.SOURCE,C.STAWN
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
             :WA_PRICE-F_DFRGT, :WA_PRICE-PEINH_MM,:WA_PRICE-F_SOURCE,
             :WA_PRICE-STAWN
        FROM MARA A, MARC B, MBEW H, MAKT E, A902 C, KONP D, ZTMM_SPE F,
             ZTMM_ANALY G
       WHERE A.MANDT     = :SY-MANDT
         AND A.MATNR     BETWEEN :WA_MATNR_F AND :WA_MATNR_T
         AND A.MTART     IN ('ROH', 'ROH1')
*        AND A.PROFL     IN ('K',   'V')
         AND A.LVORM     <> 'X'
         AND B.MANDT     =  A.MANDT
         AND B.MATNR     =  A.MATNR
         AND B.WERKS     =  :P_WERKS
         AND B.LVORM     =  ' '
*----- 2004.06.18 Requested by HSCHO, Changed by BSBAE
*----- Except submaterial
         AND B.DISPO     <> 'M02'
*----- 2004.06.18 Requested by HSCHO, Changed by BSBAE
         AND H.MANDT(+)  =  B.MANDT
         AND H.MATNR(+)  =  B.MATNR
         AND H.BWKEY(+)  =  B.WERKS
         and h.bklas      IN ('3000','3001','3002','3003','3004','3005')
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
         AND F.WERKS(+)  =  :P_WERKS
         AND F.MATNR(+)  =  A.MATNR
         AND F.SUBMT(+)  =  ' '
         AND G.MANDT(+)  =  A.MANDT
         AND G.PERIOD(+) =  :WA_PERIOD
         AND G.WERKS(+)  =  :P_WERKS
         AND G.MATNR(+)  =  A.MATNR
         AND G.SUBMT(+)  =  ' '
    ENDEXEC.

  endif.                                                    "UD1K918798
  read table it_price index 1.
  if sy-subrc ne 0.
    message e000(zz) with text-m01.
  endif.

  perform check_hs_code.
endform.                    " get_quater_plan
*&---------------------------------------------------------------------*
*&      Form  GET_BOM_COMPONENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_bom_component using pw_matnr pw_werks.
  data: lt_stb like stpox occurs 0 with header line.

* refer : LCKSAF0B.

  call function 'CS_BOM_EXPL_MAT_V2'
       EXPORTING
            capid                 = 'PC01'
            datuv                 = wa_pdatu_f
            ehndl                 = '1'
            mktls                 = 'X'
            mehrs                 = 'X'
            mmory                 = '1'
            mtnrv                 = pw_matnr
            stpst                 = 0
            svwvo                 = 'X'
            werks                 = pw_werks
            vrsvo                 = 'X'
            stlan                 = p_stlan
       TABLES
            stb                   = lt_stb
       EXCEPTIONS
            alt_not_found         = 1
            call_invalid          = 2
            material_not_found    = 3
            missing_authorization = 4
            no_bom_found          = 5
            no_plant_data         = 6
            no_suitable_bom_found = 7
            conversion_error      = 8
            others                = 9.
  if sy-subrc <> 0.
*    MESSAGE e000(zz) WITH pw_werks pw_matnr text-m06.
  endif.

  loop at lt_stb where idnrk in s_matnr
                   and ( mtart eq 'ROH'  or
                         mtart eq 'ROH1' or
                         mtart eq 'HALB' ).
    case lt_stb-mtart.
      when 'ROH' or 'ROH1'.
        move: wa_period    to it_idnrk-period,
              p_werks      to it_idnrk-werks,
              pw_werks     to it_idnrk-werks_act,
              lt_stb-idnrk to it_idnrk-idnrk.

        collect it_idnrk.
      when 'HALB'.
        move: pw_werks     to it_halb-werks,
              lt_stb-idnrk to it_halb-idnrk.

        collect it_halb.
    endcase.
  endloop.
endform.                    " GET_BOM_COMPONENT
*&---------------------------------------------------------------------*
*&      Form  SET_IT_PRICE_WITH_COMPONENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_IDNRK_IDNRK  text
*----------------------------------------------------------------------*
form set_it_price_with_component.

  EXEC SQL PERFORMING APPEND_STANDARD_PRICE.
    SELECT A.MATNR, A.MEINS, A.PROFL, D.KBETR, E.MAKTX,
           B.WERKS, F.EKORG, F.KZUST, F.WRBTR, F.WDUTY,
           F.WCOST, F.DMBTR, F.DDUTY, F.DCOST, F.WAERS,
           F.WKURS, F.LIFNR, G.WERKS, G.EKORG, G.KZUST,
           G.WRBTR, G.WDUTY, G.WCOST, G.DMBTR, G.DDUTY,
           G.DCOST, G.WAERS, G.WKURS, G.LIFNR, F.PERIOD,
           G.PERIOD,F.PEINH, G.PEINH, F.WFRGT, F.DFRGT,
           G.WFRGT, G.DFRGT, I.PEINH, G.SOURCE
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
           :WA_PRICE-F_DFRGT, :WA_PRICE-PEINH_MM, :WA_PRICE-F_SOURCE
      FROM MARA A, ZTMM_ANALY_TMP H, MARC B, MBEW I, MAKT E, A902 C,
           KONP D, ZTMM_SPE F, ZTMM_ANALY G
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
*----- 2004.06.18 Requested by HSCHO, Changed by BSBAE
*----- Except submaterial
       AND B.DISPO  <> 'M02'
*----- 2004.06.18 Requested by HSCHO, Changed by BSBAE
       AND I.MANDT(+)  =  B.MANDT
       AND I.MATNR(+)  =  B.MATNR
       AND I.BWKEY(+)  =  B.WERKS
       AND I.LVORM(+)  =  ' '
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

  read table it_price index 1.
  if sy-subrc ne 0.
    message e000(zz) with text-m01.
  endif.

endform.                    " SET_IT_PRICE_WITH_COMPONENT
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
       EXPORTING
            percentage = 100
            text       = lw_text.

endform.                    " display_progress_bar_bom
*&---------------------------------------------------------------------*
*&      Form  set_table_data_for_success
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_table_data_for_success.
  delete it_err_msg where base_d = ztmm_spe-base_d
                      and matnr  = ztmm_spe-matnr.

  clear: ztmm_analy.

  move: it_price-period  to ztmm_analy-period,
        it_price-base_d  to ztmm_analy-base_d,
        it_price-matnr   to ztmm_analy-matnr,
        it_price-werks   to ztmm_analy-werks,
        it_price-ekorg   to ztmm_analy-ekorg,
        it_price-kzust   to ztmm_analy-kzust,
        it_price-valid_d to ztmm_analy-valid_d,
        it_price-wrbtr   to ztmm_analy-wrbtr,
        it_price-wduty   to ztmm_analy-wduty,
        it_price-wfrgt   to ztmm_analy-wfrgt,
        it_price-wcost   to ztmm_analy-wcost,
        it_price-dmbtr   to ztmm_analy-dmbtr,
        it_price-dduty   to ztmm_analy-dduty,
        it_price-dfrgt   to ztmm_analy-dfrgt,
        it_price-dcost   to ztmm_analy-dcost,
        it_price-peinh   to ztmm_analy-peinh,
        it_price-waers   to ztmm_analy-waers,
        it_price-wkurs   to ztmm_analy-wkurs,
        it_price-source  to ztmm_analy-source,
        it_price-lifnr   to ztmm_analy-lifnr,
        sy-uname         to ztmm_analy-aenam,
        sy-datum         to ztmm_analy-aedat,
        sy-uzeit         to ztmm_analy-aezet,
        sy-uname         to ztmm_analy-ernam,
        sy-datum         to ztmm_analy-erdat,
        sy-uzeit         to ztmm_analy-erzet.
endform.                    " set_table_data_for_success
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
  select knumh datab lifnr
    into corresponding fields of table it_knumh
    from a018
   where kappl =  'M'
     and kschl =  'PB00'
     and matnr =  wa_price-matnr
     and ekorg =  c_ekorg
     and esokz =  '0'
     and datab > p_datum.

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
* Begin of changes - UD1K918798
    if WA_PRICE-PROFL eq 'K' .
      select single LAND1 into wa_LAND1 from LFA1
       where lifnr eq it_knumh-lifnr.
      if sy-subrc eq 0 and wa_land1 eq 'US'.
        exit.
      endif.
    endif.
* End of changes - UD1K918798

    perform append_condition_price.
  else.
    perform append_blank_price.
  endif.
endform.                    " read_future_price
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
            it_zvmm_info_condi-konwa to it_info-konwa,
            it_zvmm_info_condi-kumne to it_info-kumne,
            it_zvmm_info_condi-kumza to it_info-kumza.
    when c_frght.
      check wa_price-profl eq 'K'.
      move: it_zvmm_info_condi-kbetr to it_info-wfrgt.
    when others.
      check wa_price-profl eq 'K'.
      it_info-wcost = it_info-wcost + it_zvmm_info_condi-kbetr.
  endcase.
endform.                    " move_other_fields
*&---------------------------------------------------------------------*
*&      Form  check_base_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_base_date.
*----- Data exist already in ZTMM_ANALY, ZTMM_SPE
*----- Base date must be equal.
  select single * from ztmm_analy where werks  = p_werks
                                    and period = wa_period
                                    and newmt  = ' '
                                    and submt  = ' '.
  if sy-subrc eq 0.
    if ztmm_analy-base_d ne p_datum.
      message e000(zz) with text-m05 ztmm_analy-base_d.
    endif.
  endif.

  select single * from ztmm_spe where werks  = p_werks
                                  and period = wa_period
                                  and newmt  = ' '
                                  and submt  = ' '.
  if sy-subrc eq 0.
    if ztmm_spe-base_d ne p_datum.
      message e000(zz) with text-m05 ztmm_spe-base_d.
    endif.
  endif.
endform.                    " check_base_date
*&---------------------------------------------------------------------*
*&      Form  check_decimal_part
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
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
form get_component_of_engine.
  data: begin of lt_component occurs 0,
          matnr   like   marc-matnr,           "Material
          beskz   like   marc-beskz,           "Procurement type
          sobsl   like   marc-sobsl,           "Special procurement
        end   of lt_component.

  check p_werks eq 'P001'.

  read table it_halb index 1.
  if sy-subrc ne 0.
    move: '??????' to it_halb-idnrk,
          '????'   to it_halb-werks.
    append it_halb.
  endif.
  select a~matnr into table lt_component
    from marc as a inner join mara as b
      on a~matnr = b~matnr
     for all entries in it_halb
   where a~werks = it_halb-werks
     and a~matnr = it_halb-idnrk
     and b~mtart = 'HALB'
     and a~beskz = 'F'
     and a~sobsl = '40'.

  loop at lt_component.
    perform display_progress_bar_bom using lt_component-matnr.

** Changed on 12/19/11 for E002
*    perform get_bom_component using lt_component-matnr 'E001'.
     perform get_bom_component using lt_component-matnr it_halb-werks.
** End on 12/19/11
  endloop.
endform.                    " get_component_of_engine
*&---------------------------------------------------------------------*
*&      Form  zero_price_error_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form zero_price_error_rtn.
  delete it_err_msg where base_d = ztmm_spe-base_d
                      and matnr  = ztmm_spe-matnr.

  clear: it_err_msg.

  move: it_price-base_d        to it_err_msg-base_d,
        it_price-matnr         to it_err_msg-matnr,
        'ZZ'                   to it_err_msg-id,
        '000'                  to it_err_msg-msgno,
        text-m10               to it_err_msg-v1.

  append it_err_msg.

  if it_price-flag eq c_ready.
    wa_count_ready = wa_count_ready - 1.
    wa_count_fail  = wa_count_fail + 1.
    it_price-flag  = c_fail.
  endif.

  it_price-check = 'X'.
endform.                    " zero_price_error_rtn
*&---------------------------------------------------------------------*
*&      Form  DELETE_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form delete_rtn.
  data: lw_answer.

  call function 'POPUP_TO_CONFIRM_WITH_MESSAGE'
       EXPORTING
            defaultoption  = 'N'
            diagnosetext1  = text-b05
            textline1      = text-b06
            titel          = text-h06
            start_column   = 25
            start_row      = 6
            cancel_display = 'X'
       IMPORTING
            answer         = lw_answer.

  check lw_answer = 'J'.

  loop at it_price.
    delete from ztmm_analy where period = it_price-period
                             and werks  = it_price-werks
                             and matnr  = it_price-matnr
                             and submt  = ' '.

    delete from ztmm_spe   where period = it_price-period
                             and werks  = it_price-werks
                             and matnr  = it_price-matnr
                             and submt  = ' '.
  endloop.

  commit work and wait.

  message s000(zz) with text-m14.
  leave to screen 0.
endform.                    " DELETE_RTN
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
    exit.
  endif.

  if zvmm_info_condi-kzust(1) eq 'X'.
    " Check deletion or not for future price
    select single *
     from a018
    where kappl =  'M'
      and kschl =  'PB00'
      and matnr =  wa_price-matnr
      and lifnr =  it_knumh-lifnr
      and ekorg =  c_ekorg
      and esokz =  '0'
      and datab > p_datum.
    if sy-subrc ne 0.
      perform append_condition_price.
      exit.
    endif.

    select single *
      from zvmm_info_condi
     where knumh    = a018-knumh
       and kschl    = c_kschl
       and loevm_ko = ' '.
    if sy-subrc ne 0.
      perform append_condition_price.
      exit.
    endif.

    perform read_future_price.
  else.
    perform append_condition_price.
  endif.

endform.                    " check_future_price
*&---------------------------------------------------------------------*
*&      Form  check_hs_code
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_hs_code.
*----- IF Import Material doesn't have HS Code and has Info Record,
*----- Cannot Update Standard Price.
  loop at it_price where profl = 'K'.
    if it_price-stawn is initial and it_price-lifnr ne ' '.

      case it_price-flag.
        when c_ready.
          wa_count_ready = wa_count_ready - 1.
          wa_count_error = wa_count_error + 1.
        when c_fail.
          wa_count_fail  = wa_count_fail  - 1.
          wa_count_error = wa_count_error + 1.
        when c_success.
          wa_count_finish = wa_count_finish - 1.
          wa_count_error  = wa_count_error  + 1.
        when c_error.
      endcase.

      it_price-flag = c_hs_error.
* Begin of changes - UD1K918798
  it_price-msg = 'Import Material do not have HS Code'.
* End of changes - UD1K918798

      modify it_price.
    endif.
  endloop.
endform.                    " check_hs_code
*&---------------------------------------------------------------------*
*&      Form  CHECK_YEAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_year.
  concatenate p_year '0101' into wa_pdatu_f.
  concatenate p_year '1231' into wa_pdatu_t.
endform.                    " CHECK_YEAR
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form download_rtn.
  data: i_t686d like t686d occurs 0 with header line.
  select * into table i_t686d from t686d where spras = sy-langu.
  loop at it_price.
    read table i_t686d with key kzust = it_price-kzust.
    it_price-vtext = i_t686d-vtext.
    modify it_price.
  endloop.

  call function 'DOWNLOAD'
       EXPORTING
            filename                = 'C:\TEMP\DOWNLOAD.TXT'
            filetype                = 'DAT'
       TABLES
            data_tab                = it_price
       EXCEPTIONS
            invalid_filesize        = 1
            invalid_table_width     = 2
            invalid_type            = 3
            no_batch                = 4
            unknown_error           = 5
            gui_refuse_filetransfer = 6
            customer_error          = 7
            others                  = 8.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.
endform.                    " DOWNLOAD_RTN
