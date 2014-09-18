REPORT ZRSDTMHS .

parameters: p_date like sy-datum default sy-datum,
            p_uzeit like sy-uzeit default sy-uzeit.

data: begin of wa_equi,
        equnr like equi-equnr,
      end of wa_equi.
data: begin of wa_ausp,
       objek like ausp-objek,
       atnam like cabn-atnam,
       atwrt like ausp-atwrt,
      end of wa_ausp.
data: begin of wa_trimin_status,
        objek like equi-equnr,
        tidat like sy-datum,
        tiuze like sy-uzeit,
        datim(14) type c,
        rpsta(2) type c,
        natcd(3) type c,
        discd(2) type c,
        vinno(17) type c,
        wonum(10) type c,
        extcl(3)  type c,
        intcl(3)  type c,
      end of wa_trimin_status.
data: begin of wa_header,
        rec_type(3) type c,
        trans_date like sy-datum,
        filler(144) type c,
      end of wa_header.
data: begin of wa_detail,
        rec_type(3) type c,
        vinno(17)   type c,
        wonum(10)   type c,
        extcl(3)    type c,
        intcl(3)    type c,
        datim(14)   type c,
        filler(80)  type c,
      end of wa_detail.

data: it_equi like table of wa_equi,
      it_ausp like table of wa_ausp,
      it_trimin_status like table of wa_trimin_status.

field-symbols: <fs_ausp> like line of it_ausp,
               <fs_tmis> like line of it_trimin_status.

data: w_date_time(14) type n,
      w_uzeit like sy-uzeit,
      w_flag type c.

constants: c_o1h(3) type c value 'O1H',
           c_o1d(3) type c value 'O1D',
           c_07(2)  type n value '07',
           c_b28(3) type c value 'B28',
           c_aa(2)  type c value 'AA',
           c_ab(2)  type c value 'AB',
      c_fname  like rlgrap-filename value '/usr/sap/EDI_SAP/TRIMIN.txt'.

select objek atnam atwrt into table it_ausp
                         from ausp
                         inner join equi
                         on ausp~objek = equi~equnr
                         inner join cabn
                         on ausp~atinn = cabn~atinn
                         and ausp~adzhl = cabn~adzhl
            where ( eqtyp = 'V' and cabn~atnam = 'P_RP07_ACTUAL_DATE' )
              OR    ( eqtyp = 'V' and cabn~atnam = 'P_VIN' )
              OR    ( eqtyp = 'V' and cabn~atnam = 'P_WORK_ORDER' )
              OR    ( eqtyp = 'V' and cabn~atnam = 'P_EXT_COLOR' )
              OR    ( eqtyp = 'V' and cabn~atnam = 'P_INT_COLOR' )
              OR    ( eqtyp = 'V' and cabn~atnam = 'P_RP_STATUS' )
              OR    ( eqtyp = 'V' and cabn~atnam = 'P_NATN_CODE' )
              OR    ( eqtyp = 'V' and cabn~atnam = 'P_DIST_CODE' ).

if sy-subrc ne 0.
  message id 'ZMSD' type 'I' number '000' with text-001.
  exit.
endif.
sort it_ausp by objek atnam.

loop at it_ausp assigning <fs_ausp>.
  case <fs_ausp>-atnam.
    when 'P_DIST_CODE'.
      wa_trimin_status-discd = <fs_ausp>-atwrt.
    when 'P_EXT_COLOR'.
      wa_trimin_status-extcl = <fs_ausp>-atwrt.
    when 'P_INT_COLOR'.
      wa_trimin_status-intcl = <fs_ausp>-atwrt.
    when 'P_NATN_CODE'.
      wa_trimin_status-natcd = <fs_ausp>-atwrt.
    when 'P_RP07_ACTUAL_DATE'.
      wa_trimin_status-datim = <fs_ausp>-atwrt.
      wa_trimin_status-tidat = wa_trimin_status-datim.
      wa_trimin_status-tiuze = wa_trimin_status-datim+8(6).
    when 'P_RP_STATUS'.
      wa_trimin_status-rpsta = <fs_ausp>-atwrt.
    when 'P_VIN'.
      wa_trimin_status-vinno = <fs_ausp>-atwrt.
    when 'P_WORK_ORDER'.
      wa_trimin_status-wonum = <fs_ausp>-atwrt.
  endcase.
  at end of <fs_ausp>-objek.
    wa_trimin_status-objek = <fs_ausp>-objek.
    append wa_trimin_status to it_trimin_status.
    clear wa_trimin_status.
  endat.
endloop.
free: it_ausp, wa_ausp.
if <fs_ausp> is assigned.
  unassign <fs_ausp>.
endif.

sort it_trimin_status by objek tidat tiuze.
open dataset c_fname for output in text mode.
if sy-subrc ne 0.
  message id 'ZMSD' type 'I' number '000' with text-002.
  exit.
endif.
w_uzeit = p_uzeit + 3600.
*&-----Transfer Data.
loop at it_trimin_status assigning <fs_tmis>
                  where ( tidat eq p_date and
                          tiuze between p_uzeit and w_uzeit and
                          rpsta = c_07 and
                          natcd = c_b28 and
                          discd = c_aa )
                  OR     ( tidat eq p_date and
                          tiuze between p_uzeit and w_uzeit and
                          rpsta = c_07 and
                          natcd = c_b28 and
                          discd = c_ab ).
  if w_flag is initial.
*&-----Header.
    wa_header-rec_type = c_o1h.
    wa_header-trans_date = sy-datum.
    transfer wa_header to c_fname.
    w_flag = 'X'.
  endif.
*&-----Detail.
  wa_detail-rec_type = c_o1d.
  wa_detail-vinno    = <fs_tmis>-vinno.
  wa_detail-wonum    = <fs_tmis>-wonum.
  wa_detail-extcl    = <fs_tmis>-extcl.
  wa_detail-intcl    = <fs_tmis>-intcl.
  wa_detail-datim    = <fs_tmis>-datim.
  transfer wa_detail to c_fname.
  clear wa_detail.
endloop.

close dataset c_fname.
if w_flag ne 'X'.
  message id 'ZMSD' type 'I' number '000' with text-001.
endif.
