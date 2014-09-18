REPORT ZMMNW866.
*&--------------------------------------------------------------------&*
*&    Program: ZMMNW866.                                              &*
*&    Author : Shiva.                                                 &*
*&    Specification: Send EDI - 866 using "Z" table.                  &*
*&--------------------------------------------------------------------&*
*& Date        User     Transport       Description
*& 04/01/2005  Shiva    UD1K915500      initial program.
*&--------------------------------------------------------------------&*
data: wa_parts_3days like ztmm_parts_3days.
data: begin of wa_jis_vendor,
        lifnr like ekko-lifnr,
        matnr like ekpo-matnr,
      end of wa_jis_vendor.
data: begin of wa_time_qty,
       lifnr like lfa1-lifnr,
       matnr like mara-matnr,
       d1t01(14) type c,
       d1mng01(6) type c,
       d1t02(14) type c,
       d1mng02(6) type c,
       d1t03(14) type c,
       d1mng03(6) type c,
       d1t04(14) type c,
       d1mng04(6) type c,
       d1t05(14) type c,
       d1mng05(6) type c,
       d1t06(14) type c,
       d1mng06(6) type c,
       d1t07(14) type c,
       d1mng07(6) type c,
       d1t08(14) type c,
       d1mng08(6) type c,
       d1t09(14) type c,
       d1mng09(6) type c,
       d1t10(14) type c,
       d1mng10(6) type c,
       d2t01(14) type c,
       d2mng01(6) type c,
       d2t02(14) type c,
       d2mng02(6) type c,
       d2t03(14) type c,
       d2mng03(6) type c,
       d2t04(14) type c,
       d2mng04(6) type c,
       d2t05(14) type c,
       d2mng05(6) type c,
       d2t06(14) type c,
       d2mng06(6) type c,
       d2t07(14) type c,
       d2mng07(6) type c,
       d2t08(14) type c,
       d2mng08(6) type c,
       d2t09(14) type c,
       d2mng09(6) type c,
       d2t10(14) type c,
       d2mng10(6) type c,
       d3t01(14) type c,
       d3mng01(6) type c,
       d3t02(14) type c,
       d3mng02(6) type c,
       d3t03(14) type c,
       d3mng03(6) type c,
       d3t04(14) type c,
       d3mng04(6) type c,
       d3t05(14) type c,
       d3mng05(6) type c,
       d3t06(14) type c,
       d3mng06(6) type c,
       d3t07(14) type c,
       d3mng07(6) type c,
       d3t08(14) type c,
       d3mng08(6) type c,
       d3t09(14) type c,
       d3mng09(6) type c,
       d3t10(14) type c,
       d3mng10(6) type c,
      end of wa_time_qty.

data: it_parts_3days like table of wa_parts_3days,
      *it_parts_3days like table of wa_parts_3days,
      it_jis_vendor like table of wa_jis_vendor,
      it_time_qty like table of wa_time_qty.

data: w_pack_qty1(6) type p decimals 0,
      w_pack_qty2(6) type p,
      w_pack_qty3(6) type p,
      w_pack_qty4(6) type p,
      w_pack_qty5(6) type p,
      w_fname like rlgrap-filename,
      w_flag type c.

parameters: p_fname like rlgrap-filename
                    default '/usr/sap/EDI_SAP/HMMA866/hmma866'.

select * from ztmm_parts_3days
         into table it_parts_3days
         where ptype = 'S'
         and   lifnr ne space.
if sy-subrc ne 0.
  MESSAGE e999(zmmm) WITH 'No Data in table ZTMM_PARTS_3DAYS'(001).
  exit.
endif.
select lifnr matnr into table it_jis_vendor
                   from ekko
                   inner join ekpo
                   on ekpo~ebeln = ekko~ebeln
                   and ekpo~loekz = ekko~loekz
                  where ekko~bstyp = 'L'
                  and   bsart = 'JIS'.
if sy-subrc ne 0.
  MESSAGE e999(zmmm) WITH 'No Scheduling Agreement exists'(001).
  exit.
endif.

sort: it_jis_vendor by lifnr matnr,
      it_parts_3days by lifnr matnr.

delete adjacent duplicates from it_jis_vendor comparing lifnr matnr.
*&----Open file.
concatenate p_fname sy-datum sy-uzeit into w_fname.
open dataset w_fname for output in text mode.
if sy-subrc ne 0.
  MESSAGE e999(zmmm) WITH 'File could not be open to write!'(001).
  exit.
endif.
*&--Process data
loop at it_parts_3days into wa_parts_3days.
  read table it_jis_vendor with key lifnr = wa_parts_3days-lifnr
                                    matnr = wa_parts_3days-matnr
                                     transporting no fields.
  if sy-subrc ne 0.
    continue.
  endif.
  wa_time_qty-lifnr   = wa_parts_3days-lifnr.
  wa_time_qty-matnr   = wa_parts_3days-matnr.
*&-----------Day 1.
  perform calculate_time using    wa_parts_3days-dat01
                                  wa_parts_3days-zet11
                         changing wa_time_qty-d1t01
                                  wa_time_qty-d1t02
                                  wa_time_qty-d1t03
                                  wa_time_qty-d1t04
                                  wa_time_qty-d1t05.
  w_pack_qty1 = wa_parts_3days-qty02.
  w_pack_qty2 = wa_parts_3days-qty04.
  w_pack_qty3 = wa_parts_3days-qty06.
  w_pack_qty4 = wa_parts_3days-qty08.
  w_pack_qty5 = wa_parts_3days-qty10.
  unpack: w_pack_qty1 to wa_time_qty-d1mng01,
          w_pack_qty2 to wa_time_qty-d1mng02,
          w_pack_qty3 to wa_time_qty-d1mng03,
          w_pack_qty4 to wa_time_qty-d1mng04,
          w_pack_qty5 to wa_time_qty-d1mng05.
  if not wa_parts_3days-zet12 is initial.
    clear: w_pack_qty1, w_pack_qty2, w_pack_qty3,
           w_pack_qty4, w_pack_qty5.
    perform calculate_time using    wa_parts_3days-dat01
                                    wa_parts_3days-zet12
                           changing wa_time_qty-d1t06
                                    wa_time_qty-d1t07
                                    wa_time_qty-d1t08
                                    wa_time_qty-d1t09
                                    wa_time_qty-d1t10.
    w_pack_qty1 = wa_parts_3days-qty12.
    w_pack_qty2 = wa_parts_3days-qty14.
    w_pack_qty3 = wa_parts_3days-qty16.
    w_pack_qty4 = wa_parts_3days-qty18.
    w_pack_qty5 = wa_parts_3days-qty20.
    unpack: w_pack_qty1 to wa_time_qty-d1mng06,
            w_pack_qty2 to wa_time_qty-d1mng07,
            w_pack_qty3 to wa_time_qty-d1mng08,
            w_pack_qty4 to wa_time_qty-d1mng09,
            w_pack_qty5 to wa_time_qty-d1mng10.
  endif.
*&-------Day 2
  clear: w_pack_qty1, w_pack_qty2, w_pack_qty3,
         w_pack_qty4, w_pack_qty5.
  perform calculate_time using    wa_parts_3days-dat02
                                  wa_parts_3days-zet21
                         changing wa_time_qty-d2t01
                                  wa_time_qty-d2t02
                                  wa_time_qty-d2t03
                                  wa_time_qty-d2t04
                                  wa_time_qty-d2t05.
  w_pack_qty1 = wa_parts_3days-qty22.
  w_pack_qty2 = wa_parts_3days-qty24.
  w_pack_qty3 = wa_parts_3days-qty26.
  w_pack_qty4 = wa_parts_3days-qty28.
  w_pack_qty5 = wa_parts_3days-qty30.
  unpack: w_pack_qty1 to wa_time_qty-d2mng01,
          w_pack_qty2 to wa_time_qty-d2mng02,
          w_pack_qty3 to wa_time_qty-d2mng03,
          w_pack_qty4 to wa_time_qty-d2mng04,
          w_pack_qty5 to wa_time_qty-d2mng05.
  if not wa_parts_3days-zet22 is initial.
    clear: w_pack_qty1, w_pack_qty2, w_pack_qty3,
           w_pack_qty4, w_pack_qty5.
    perform calculate_time using    wa_parts_3days-dat02
                                    wa_parts_3days-zet22
                           changing wa_time_qty-d2t06
                                    wa_time_qty-d2t07
                                    wa_time_qty-d2t08
                                    wa_time_qty-d2t09
                                    wa_time_qty-d2t10.
    w_pack_qty1 = wa_parts_3days-qty32.
    w_pack_qty2 = wa_parts_3days-qty34.
    w_pack_qty3 = wa_parts_3days-qty36.
    w_pack_qty4 = wa_parts_3days-qty38.
    w_pack_qty5 = wa_parts_3days-qty40.
    unpack: w_pack_qty1 to wa_time_qty-d2mng06,
            w_pack_qty2 to wa_time_qty-d2mng07,
            w_pack_qty3 to wa_time_qty-d2mng08,
            w_pack_qty4 to wa_time_qty-d2mng09,
            w_pack_qty5 to wa_time_qty-d2mng10.
  endif.
*&---- day 3.
  clear: w_pack_qty1, w_pack_qty2, w_pack_qty3,
         w_pack_qty4, w_pack_qty5.
  perform calculate_time using    wa_parts_3days-dat03
                                  wa_parts_3days-zet31
                         changing wa_time_qty-d3t01
                                  wa_time_qty-d3t02
                                  wa_time_qty-d3t03
                                  wa_time_qty-d3t04
                                  wa_time_qty-d3t05.
  w_pack_qty1 = wa_parts_3days-qty42.
  w_pack_qty2 = wa_parts_3days-qty44.
  w_pack_qty3 = wa_parts_3days-qty46.
  w_pack_qty4 = wa_parts_3days-qty48.
  w_pack_qty5 = wa_parts_3days-qty50.
  unpack: w_pack_qty1 to wa_time_qty-d3mng01,
          w_pack_qty2 to wa_time_qty-d3mng02,
          w_pack_qty3 to wa_time_qty-d3mng03,
          w_pack_qty4 to wa_time_qty-d3mng04,
          w_pack_qty5 to wa_time_qty-d3mng05.
  if not wa_parts_3days-zet32 is initial.
    clear: w_pack_qty1, w_pack_qty2, w_pack_qty3,
           w_pack_qty4, w_pack_qty5.
    perform calculate_time using    wa_parts_3days-dat03
                                    wa_parts_3days-zet32
                           changing wa_time_qty-d3t06
                                    wa_time_qty-d3t07
                                    wa_time_qty-d3t08
                                    wa_time_qty-d3t09
                                    wa_time_qty-d3t10.
    w_pack_qty1 = wa_parts_3days-qty52.
    w_pack_qty2 = wa_parts_3days-qty54.
    w_pack_qty3 = wa_parts_3days-qty56.
    w_pack_qty4 = wa_parts_3days-qty58.
    w_pack_qty5 = wa_parts_3days-qty60.
    unpack: w_pack_qty1 to wa_time_qty-d3mng06,
            w_pack_qty2 to wa_time_qty-d3mng07,
            w_pack_qty3 to wa_time_qty-d3mng08,
            w_pack_qty4 to wa_time_qty-d3mng09,
            w_pack_qty5 to wa_time_qty-d3mng10.
  endif.
  transfer wa_time_qty to w_fname.
  if sy-subrc ne 0.
    clear w_flag.
  endif.
  w_flag = 'X'.
*  append wa_time_qty to it_time_qty.
  clear: wa_time_qty, w_pack_qty1, w_pack_qty2, w_pack_qty3,
         w_pack_qty4, w_pack_qty5.
endloop.
close dataset w_fname.

if w_flag = space.
  message i999(zmmm) with 'EDI866 File not transfered!'.
else.
  message i999(zmmm) with 'EDI866 File transfered !'.
endif.

*&---------------------------------------------------------------------*
*&      Form  calculate_time
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_TIME1  text
*      <--P_W_TIME2  text
*      <--P_W_TIME3  text
*      <--P_W_TIME4  text
*      <--P_W_TIME5  text
*----------------------------------------------------------------------*
FORM calculate_time using    p_date
                             p_time
                    changing p_dat01
                             p_dat02
                             p_dat03
                             p_dat04
                             p_dat05.

  data: w_date  like sy-datum,
        w_time1 like sy-uzeit,
        w_time2 like sy-uzeit,
        w_time3 like sy-uzeit,
        w_time4 like sy-uzeit,
        w_time5 like sy-uzeit.

  w_date  = p_date.
  w_time1 = p_time.
  concatenate w_date w_time1 into p_dat01.
  if w_time1 > '220000'.
    w_date = w_date + 1.
  endif.
  w_time2 = w_time1 + 7200.
  concatenate w_date w_time2 into p_dat02.
  if w_time2 > '220000'.
    w_date = w_date + 1.
  endif.
  w_time3 = w_time2 + 7200.
  concatenate w_date w_time3 into p_dat03.
  if w_time3 > '220000'.
    w_date = w_date + 1.
  endif.
  w_time4 = w_time3 + 7200.
  concatenate w_date w_time4 into p_dat04.
  if w_time4 > '220000'.
    w_date = w_date + 1.
  endif.
  w_time5 = w_time4 + 7200.
  concatenate w_date w_time5 into p_dat05.

  clear: w_date, w_time1, w_time2, w_time3, w_time4, w_time5.

ENDFORM.                    " calculate_time
