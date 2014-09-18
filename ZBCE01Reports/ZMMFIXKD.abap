REPORT ZMMFIXKD .
*&--------------------------------------------------------------------&*
*&    Program: ZMMFIXKD.                                              &*
*&    Author : Shiva.                                                 &*
*&    Specification: To fix Inbound deliveries realted to  KD PO.     &*
*&--------------------------------------------------------------------&*
*& Date        User     Transport       Description
*& 05/12/2005  Shiva    UD1K916014      To fix issue raised by GLOVIS
*&                             with respect to KD PO inbound deliveries.
*&--------------------------------------------------------------------&*

data: wa_kdasn like ztmm_kd_asn_main.

data: it_kdasn like table of wa_kdasn.

field-symbols: <fs_kdasn> like line of it_kdasn.

select-options: s_traid for wa_kdasn-traid obligatory.
parameters: p_date1 like sy-datum obligatory,
            p_char  type c obligatory,
            p_date2 like sy-datum obligatory.

select * from ztmm_kd_asn_main
         into table it_kdasn
         where traid in s_traid
         and   zedat eq p_date1.
if sy-subrc ne 0.
  write: / 'No records available for selection'.
  exit.
endif.
loop at it_kdasn assigning <fs_kdasn>.
  concatenate <fs_kdasn>-traid p_char into <fs_kdasn>-traid.
  <fs_kdasn>-zedat = p_date2.
  <fs_kdasn>-zresult = space.
  <fs_kdasn>-zmsg    = space.
endloop.

insert ztmm_kd_asn_main from table it_kdasn.
if sy-subrc ne 0.
  write: / 'Error in INSERT to ztmm_kd_asn_main'.
else.
  write: / 'Sucessfully Inserted to ztmm_kd_asn_main'.
endif.
