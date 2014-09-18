REPORT ZMM21866 .
*&--------------------------------------------------------------------&*
*&    Program: ZMMNW866.                                              &*
*&    Author : Shiva.                                                 &*
*&    Specification: Send EDI - 866 using "Z" table.                  &*
*&--------------------------------------------------------------------&*
*& Date        User     Transport       Description
*& 04/28/2005  Shiva    UD1K915783      initial program.
*& 07/14/2005  Shiva    UD1K916846      check for sch.agr. complete
*&                                      indicator.
*&--------------------------------------------------------------------&*
data: wa_parts_21day like ztmm_parts_21day.
data: begin of wa_jis_vendor,
        lifnr like ekko-lifnr,
        matnr like ekpo-matnr,
      end of wa_jis_vendor.
data: begin of wa_time_qty,
       lifnr like lfa1-lifnr,
       matnr like mara-matnr,
       date1 like sy-datum,
       d01mng(6) type c,
       date2 like sy-datum,
       d02mng(6) type c,
       date3 like sy-datum,
       d03mng(6) type c,
       date4 like sy-datum,
       d04mng(6) type c,
       date5 like sy-datum,
       d05mng(6) type c,
       date6 like sy-datum,
       d06mng(6) type c,
       date7 like sy-datum,
       d07mng(6) type c,
       date8 like sy-datum,
       d08mng(6) type c,
       date9 like sy-datum,
       d09mng(6) type c,
       date10 like sy-datum,
       d10mng(6) type c,
       date11 like sy-datum,
       d11mng(6) type c,
       date12 like sy-datum,
       d12mng(6) type c,
       date13 like sy-datum,
       d13mng(6) type c,
       date14 like sy-datum,
       d14mng(6) type c,
       date15 like sy-datum,
       d15mng(6) type c,
       date16 like sy-datum,
       d16mng(6) type c,
       date17 like sy-datum,
       d17mng(6) type c,
       date18 like sy-datum,
       d18mng(6) type c,
       date19 like sy-datum,
       d19mng(6) type c,
       date20 like sy-datum,
       d20mng(6) type c,
       date21 like sy-datum,
       d21mng(6) type c,
      end of wa_time_qty.

data: it_parts_21day like table of wa_parts_21day,
      *it_parts_21day like table of wa_parts_21day,
      it_jis_vendor like table of wa_jis_vendor,
      it_time_qty like table of wa_time_qty.

data: w_pack_qty(6) type p decimals 0,
      w_fname like rlgrap-filename,
      w_flag type c.
data: w_date01 like sy-datum,
      w_date02 like sy-datum,
      w_date03 like sy-datum,
      w_date04 like sy-datum,
      w_date05 like sy-datum,
      w_date06 like sy-datum,
      w_date07 like sy-datum,
      w_date08 like sy-datum,
      w_date09 like sy-datum,
      w_date10 like sy-datum,
      w_date11 like sy-datum,
      w_date12 like sy-datum,
      w_date13 like sy-datum,
      w_date14 like sy-datum,
      w_date15 like sy-datum,
      w_date16 like sy-datum,
      w_date17 like sy-datum,
      w_date18 like sy-datum,
      w_date19 like sy-datum,
      w_date20 like sy-datum,
      w_date21 like sy-datum.

parameters: p_fname like rlgrap-filename
                    default '/usr/sap/EDI_SAP/HMMA866/hmma866'.

select * from ztmm_parts_21day
         into table it_parts_21day
         where lifnr ne space.
if sy-subrc ne 0.
  MESSAGE e999(zmmm) WITH 'No Data in table ZTMM_PARTS_21DAY'(001).
  exit.
endif.
select lifnr matnr into table it_jis_vendor
                   from ekko
                   inner join ekpo
                   on ekpo~ebeln = ekko~ebeln
                   and ekpo~loekz = ekko~loekz
                  where ekko~bstyp = 'L'
                  and   bsart = 'JIS'
                  and   ekko~loekz = space
                  and   elikz = space .
if sy-subrc ne 0.
  MESSAGE e999(zmmm) WITH 'No Scheduling Agreement exists'(001).
  exit.
endif.

sort: it_jis_vendor by lifnr matnr,
      it_parts_21day by lifnr matnr.

delete adjacent duplicates from it_jis_vendor comparing lifnr matnr.
*&----Open file.
concatenate p_fname sy-datum sy-uzeit into w_fname.
open dataset w_fname for output in text mode.
if sy-subrc ne 0.
  MESSAGE e999(zmmm) WITH 'File could not be open to write!'(001).
  exit.
endif.
*&--Process data
loop at it_parts_21day into wa_parts_21day.
  read table it_jis_vendor with key lifnr = wa_parts_21day-lifnr
                                    matnr = wa_parts_21day-matnr
                                     transporting no fields.
  if sy-subrc ne 0.
    continue.
  endif.
  wa_time_qty-lifnr   = wa_parts_21day-lifnr.
  wa_time_qty-matnr   = wa_parts_21day-matnr.
  if w_date01 ne wa_parts_21day-datum.
    w_date01 =  wa_parts_21day-datum.
    perform get_21dates using w_date01.
  endif.
  clear w_pack_qty.
  wa_time_qty-date1   = w_date01.
  w_pack_qty  = wa_parts_21day-d01.
  unpack w_pack_qty to wa_time_qty-d01mng.
  clear w_pack_qty.
  wa_time_qty-date2   = w_date02.
  w_pack_qty  = wa_parts_21day-d02.
  unpack w_pack_qty to wa_time_qty-d02mng.
  clear w_pack_qty.
  wa_time_qty-date3   = w_date03.
  w_pack_qty  = wa_parts_21day-d03.
  unpack w_pack_qty to wa_time_qty-d03mng.
  clear w_pack_qty.
  wa_time_qty-date4   = w_date04.
  w_pack_qty  = wa_parts_21day-d04.
  unpack w_pack_qty to wa_time_qty-d04mng.
  clear w_pack_qty.
  wa_time_qty-date5   = w_date05.
  w_pack_qty  = wa_parts_21day-d05.
  unpack w_pack_qty to wa_time_qty-d05mng.
  clear w_pack_qty.
  wa_time_qty-date6   = w_date06.
  w_pack_qty  = wa_parts_21day-d06.
  unpack w_pack_qty to wa_time_qty-d06mng.
  clear w_pack_qty.
  wa_time_qty-date7   = w_date07.
  w_pack_qty  = wa_parts_21day-d07.
  unpack w_pack_qty to wa_time_qty-d07mng.
  clear w_pack_qty.
  wa_time_qty-date8   = w_date08.
  w_pack_qty  = wa_parts_21day-d08.
  unpack w_pack_qty to wa_time_qty-d08mng.
  clear w_pack_qty.
  wa_time_qty-date9   = w_date09.
  w_pack_qty  = wa_parts_21day-d09.
  unpack w_pack_qty to wa_time_qty-d09mng.
  clear w_pack_qty.
  wa_time_qty-date10   = w_date10.
  w_pack_qty  = wa_parts_21day-d10.
  unpack w_pack_qty to wa_time_qty-d10mng.
  clear w_pack_qty.
  wa_time_qty-date11   = w_date11.
  w_pack_qty  = wa_parts_21day-d11.
  unpack w_pack_qty to wa_time_qty-d11mng.
  clear w_pack_qty.
  wa_time_qty-date12   = w_date12.
  w_pack_qty  = wa_parts_21day-d12.
  unpack w_pack_qty to wa_time_qty-d12mng.
  clear w_pack_qty.
  wa_time_qty-date13   = w_date13.
  w_pack_qty  = wa_parts_21day-d13.
  unpack w_pack_qty to wa_time_qty-d13mng.
  clear w_pack_qty.
  wa_time_qty-date14   = w_date14.
  w_pack_qty  = wa_parts_21day-d14.
  unpack w_pack_qty to wa_time_qty-d14mng.
  clear w_pack_qty.
  wa_time_qty-date15   = w_date15.
  w_pack_qty  = wa_parts_21day-d15.
  unpack w_pack_qty to wa_time_qty-d15mng.
  clear w_pack_qty.
  wa_time_qty-date16   = w_date16.
  w_pack_qty  = wa_parts_21day-d16.
  unpack w_pack_qty to wa_time_qty-d16mng.
  clear w_pack_qty.
  wa_time_qty-date17   = w_date17.
  w_pack_qty  = wa_parts_21day-d17.
  unpack w_pack_qty to wa_time_qty-d17mng.
  clear w_pack_qty.
  wa_time_qty-date18   = w_date18.
  w_pack_qty  = wa_parts_21day-d18.
  unpack w_pack_qty to wa_time_qty-d18mng.
  clear w_pack_qty.
  wa_time_qty-date19   = w_date19.
  w_pack_qty  = wa_parts_21day-d19.
  unpack w_pack_qty to wa_time_qty-d19mng.
  clear w_pack_qty.
  wa_time_qty-date20   = w_date20.
  w_pack_qty  = wa_parts_21day-d20.
  unpack w_pack_qty to wa_time_qty-d20mng.
  clear w_pack_qty.
  wa_time_qty-date21   = w_date21.
  w_pack_qty  = wa_parts_21day-d21.
  unpack w_pack_qty to wa_time_qty-d21mng.
  transfer wa_time_qty to w_fname.
  clear: wa_time_qty, w_pack_qty.
endloop.
close dataset w_fname.
message i999(zmmm) with 'EDI866 File transfered !'.
*&---------------------------------------------------------------------*
*&      Form  get_21dates
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_PARTS_21DAY_DATUM  text
*----------------------------------------------------------------------*
FORM get_21dates USING p_date.

  data: w_cnt(2) type n value '02',
        w_date   like sy-datum,
        w_dat(8) type c.
  field-symbols: <fs>.

  w_date = p_date.
  while w_cnt <= 21.
    w_date = w_date + 1.
    CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
      EXPORTING
*   CORRECT_OPTION                     = '+'
        DATE                               = w_date
        FACTORY_CALENDAR_ID                ='HM'
     IMPORTING
       DATE                               = w_date
*   FACTORYDATE                        =
*   WORKINGDAY_INDICATOR               =
 EXCEPTIONS
   CALENDAR_BUFFER_NOT_LOADABLE       = 1
   CORRECT_OPTION_INVALID             = 2
   DATE_AFTER_RANGE                   = 3
   DATE_BEFORE_RANGE                  = 4
   DATE_INVALID                       = 5
   FACTORY_CALENDAR_NOT_FOUND         = 6
   OTHERS                             = 7
              .
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    concatenate 'w_date' w_cnt into w_dat.
    assign (w_dat) to <fs>.
    <fs> = w_date.
    w_cnt = w_cnt + 1.
  endwhile.

ENDFORM.                    " get_21dates
