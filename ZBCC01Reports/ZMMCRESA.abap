REPORT ZMMCRESA .
*&--------------------------------------------------------------------&*
*&   Report: ZMMCRESA
*&   Author: Shiva.
*&   Specification: Upload Scheduling Agreement.
*&
*&--------------------------------------------------------------------&*
*&                        IMPORTANT
*& It is assumed that the program has to create Scheduling Agreement   *
*& only with one line item and the material master values will default.*
*&--------------------------------------------------------------------&*
*& Date        User    Transport                  Description
*& 12/14/04    Shiva    UD1K913517               initial program.
*&--------------------------------------------------------------------&*
data: begin of wa_excel_data,
        lifnr(10) type c,
        bsart(4)  type c,
        agrdt(10) type c,
        ekgrp(3)  type c,
        werks(4)  type c,
        lgort(4)  type c,
        valdb(10) type c,
        valde(10) type c,
        matnr(18) type c,
        trqty(17) type c,
        fzone(3)  type c,
        cprof(4) type c,
        unpoint(10) type c,
        jit_ind     type c,
      end of wa_excel_data.
data: wa_bdcmsg like bdcmsgcoll.
data: begin of wa_ekpo,
        lifnr like ekko-lifnr,
        matnr like ekpo-matnr,
      end of wa_ekpo.
data: it_bdcmsg like table of wa_bdcmsg,
      it_ekpo like table of wa_ekpo,
      it_err_log like table of wa_ekpo,
      it_excel_data like table of wa_excel_data.
data: w_holddate like sy-datum,
      w_eof type c,
      w_lines type i,
      w_sflag type c.
field-symbols: <fs_excel> like line of it_excel_data.

parameters: p_fname like rlgrap-filename
                         default '/usr/sap/EDI_SAP/conv/upload/'.

open dataset p_fname in text mode.
if sy-subrc <> 0.
  message id 'ZMM_UP' type 'E' number '000'.
  exit.
endif.
while w_eof ne 'X'.
  read dataset p_fname into wa_excel_data.
  if sy-subrc ne 0.
    w_eof = 'X'.
  else.
    append wa_excel_data to it_excel_data.
  endif.
endwhile.

describe table it_excel_data lines w_lines.
if w_lines eq 0.
  message id 'ZMM_UP' type 'E' number '001'.
  exit.
endif.

select lifnr matnr into table it_ekpo
                      from ekko as t1
                      inner join ekpo as t2
                      on t2~ebeln = t1~ebeln
                      and t2~loekz = t1~loekz
                      where bsart = 'JIT' and t1~loekz = ''
                      or    bsart = 'JIS' and t1~loekz = ''.

sort it_ekpo by lifnr matnr.
delete adjacent duplicates from it_ekpo comparing lifnr matnr.

perform open_group using 'ZSCHDAGR' sy-uname 'X' w_holddate space.

loop at it_excel_data assigning <fs_excel>.
  read table it_ekpo with key lifnr = <fs_excel>-lifnr
                              matnr = <fs_excel>-matnr
                                       binary search
                                       transporting no fields.
  if sy-subrc eq 0.
    wa_ekpo-matnr = <fs_excel>-matnr.
    wa_ekpo-lifnr = <fs_excel>-lifnr.
    collect wa_ekpo into it_err_log.
  else.
    w_sflag = 'X'.
    perform bdc_dynpro using 'SAPMM06E' '0200'.
    perform bdc_field  using 'EKKO-LIFNR' <fs_excel>-lifnr.
    perform bdc_field  using 'RM06E-EVART' <fs_excel>-bsart.
    perform bdc_field  using 'RM06E-VEDAT' <fs_excel>-agrdt.
    perform bdc_field  using 'EKKO-EKORG' 'PU01'.
    perform bdc_field  using 'EKKO-EKGRP' <fs_excel>-ekgrp.
    perform bdc_field  using 'BDC_OKCODE' '/0'.
    perform bdc_dynpro using 'SAPMM06E' '0201'.
    perform bdc_field  using 'EKKO-KDATB' <fs_excel>-valdb.
    perform bdc_field  using 'EKKO-KDATE' <fs_excel>-valde.
*perform bdc_field  using 'EKKO-ZTERM' 'P030'.
    perform bdc_field  using 'BDC_OKCODE' '=AB'.
    perform bdc_dynpro using 'SAPMM06E' '0220'.
    perform bdc_field  using 'EKPO-EMATN(1)' <fs_excel>-matnr.
    perform bdc_field  using 'EKPO-KTMNG(1)' <fs_excel>-trqty.
    perform bdc_field  using 'EKPO-WERKS(1)' <fs_excel>-werks.
    perform bdc_field  using 'RM06E-TCSELFLAG(1)' 'X'.
    perform bdc_field  using 'BDC_OKCODE' '=DETA'.
    perform bdc_dynpro using 'SAPMM06E' '0211'.
    perform bdc_field  using 'EKPO-LGORT' <fs_excel>-lgort.
*perform bdc_field  using 'EKPO-BSTAE' ''.
    perform bdc_field  using 'BDC_OKCODE' '=DETZ'.
    perform bdc_dynpro using 'SAPMM06E' '0212'.
*perform bdc_field  using 'EKPO-PLIFZ' '0'.
    perform bdc_field  using 'EKPO-ETFZ1' <fs_excel>-fzone.
    perform bdc_field  using 'EKPO-ABUEB' <fs_excel>-cprof.
    perform bdc_field  using 'EKPO-FABKZ' <fs_excel>-jit_ind.
    perform bdc_field  using 'EKPO-LGBZO' <fs_excel>-unpoint.
    perform bdc_field  using 'BDC_OKCODE' '=BU'.
    perform bdc_dynpro using 'SAPLSPO1' '0300'.
    perform bdc_field  using 'BDC_CURSOR' 'SPOP-OPTION1'.
    perform bdc_field  using 'BDC_OKCODE' '=YES'.

    perform bdc_transaction tables it_bdcmsg
                            using  'ME31L' space space space.
  endif.
endloop.
perform close_group using space.
if w_sflag is initial.
else.
  message id 'ZMM_UP' type 'I' number '002'.
endif.
loop at it_err_log into wa_ekpo.
  write: / text-001, wa_ekpo-matnr, text-002, wa_ekpo-lifnr.
endloop.
include bdcrecxy.
