report ZCMMINFR
       no standard page heading line-size 255.
*&--------------------------------------------------------------------&*
*&  Program: ZCMMINFR.
*&  Author : Shiva.
*&  Specification: Update new condition type(s) to Purchase info record.
*&--------------------------------------------------------------------&*
*& Date         User    Transport             Description
*& 05/09/2005   Shiva   UD1K915926          initial program.
*& 05/10/2005   Shiva   UD1K915966           Choose current validity
*&                               period instead of choosing 1st period.
*&--------------------------------------------------------------------&*
data: begin of wa_excel_rec,
         matnr like mara-matnr,
         kbetr1(13) type c,
         kbetr2(13) type c,
         kbetr3(13) type c,
      end of wa_excel_rec.
data: begin of wa_a018,
        matnr like a018-matnr,
        datbi like a018-datbi,
        datab like a018-datab,
        knumh like a018-knumh,
      end of wa_a018.
data: begin of wa_konp,
        knumh like konp-knumh,
        kschl like konp-kschl,
      end of wa_konp.
data: begin of wa_datab_cnt,
        matnr like a018-matnr,
        count type i,
      end of wa_datab_cnt.
data: begin of wa_kschl_cnt,
        knumh like konp-knumh,
        count type i,
       end of wa_kschl_cnt.
data: wa_messtab like bdcmsgcoll.
data: it_excel_rec like table of wa_excel_rec,
      it_a018 like table of wa_a018,
      it_konp like table of wa_konp,
      it_datab_cnt like table of wa_datab_cnt,
      it_kschl_cnt like table of wa_kschl_cnt,
      it_messtab like table of wa_messtab.
data: w_field1(14) type c,
      w_field2(14) type c,
      w_field3(14) type c,
      w_lineid(2) type n,
      w_valprd_id(2) type n,
      w_hdate like sy-datum,
      w_eof type c,
      w_flag type c.
field-symbols: <fs_a018> like line of it_a018,
               <fs_konp> like line of it_konp,
               <fs_excel> like line of it_excel_rec.

parameters: p_fname like rlgrap-filename obligatory
                     default '/usr/sap/EDI_SAP/conv/upload/'.
open dataset p_fname for input in text mode.
if sy-subrc <> 0.
  message id 'ZMMM' type 'E' number '999' with text-e00.
  leave program.
endif.
while w_eof = space.
  read dataset p_fname into wa_excel_rec.
  if sy-subrc ne 0.
    w_eof = 'X'.
    exit.
  endif.
  append wa_excel_rec to it_excel_rec.
endwhile.

start-of-selection.
  select matnr datbi datab knumh
                          into table it_a018
                          from a018
                          where kappl = 'M'
                          and   kschl = 'PB00'
                          and   lifnr = 'SEF9'
                          and   ekorg = 'PU01'.
  if sy-subrc ne 0.
    exit.
  endif.
  select konp~knumh konp~kschl into table it_konp
                               from konp
                               for all entries in it_a018
                               where knumh = it_a018-knumh.
  if sy-subrc ne 0.
    exit.
  endif.
*&----Get Validity period count for each material.
  loop at it_a018 assigning <fs_a018>.
    wa_datab_cnt-matnr = <fs_a018>-matnr.
    wa_datab_cnt-count = 1.
    collect wa_datab_cnt into it_datab_cnt.
  endloop.
*&----Get condition count for each material.
  loop at it_konp assigning <fs_konp>.
    wa_kschl_cnt-knumh = <fs_konp>-knumh.
    wa_kschl_cnt-count = 1.
    collect wa_kschl_cnt into it_kschl_cnt.
  endloop.

  sort: it_a018 by matnr datab descending.

  perform open_group using 'ZINFOREC' sy-uname 'X' w_hdate space.

  loop at it_excel_rec assigning <fs_excel>.
    perform bdc_dynpro using 'SAPMM06I' '0100'.
    perform bdc_field  using 'BDC_CURSOR' 'EINA-MATNR'.
    perform bdc_field  using 'BDC_OKCODE' '/00'.
    perform bdc_field  using 'EINA-LIFNR' 'SEF9'.
    perform bdc_field  using 'EINA-MATNR' <fs_excel>-matnr.
    perform bdc_field  using 'EINE-EKORG' 'PU01'.
    perform bdc_field  using 'RM06I-NORMB' 'X'.
    perform bdc_dynpro using 'SAPMM06I' '0101'.
    perform bdc_field  using 'BDC_CURSOR' 'EINA-MAHN1'.
    perform bdc_field  using 'BDC_OKCODE' '=KO'.
    clear: w_valprd_id, w_field1.
    read table it_datab_cnt into wa_datab_cnt
                             with key matnr = <fs_excel>-matnr
                             transporting count.
    w_valprd_id = wa_datab_cnt-count.
    concatenate 'VAKE-DATAB(' w_valprd_id ')' into w_field1.
    perform bdc_dynpro using 'SAPLV14A' '0102'.
    perform bdc_field  using 'BDC_CURSOR' w_field1.
    perform bdc_field  using 'BDC_OKCODE' '=PICK'.
    read table it_a018 into wa_a018 with key matnr = <fs_excel>-matnr.
    read table it_kschl_cnt into wa_kschl_cnt
                            with key knumh = wa_a018-knumh
                            transporting count.
*&------First condition type
    if not <fs_excel>-kbetr1 is initial.
      clear: w_field2, w_field3.
      w_lineid = wa_kschl_cnt-count + 1.
      concatenate 'KONP-KSCHL(' w_lineid ')' into w_field2.
      concatenate 'KONP-KBETR(' w_lineid ')' into w_field3.
      perform fill_condn_table using w_field2 w_field3
                                     'ZP16' <fs_excel>-kbetr1.
    endif.
*&-----Second condition.
    clear: w_field2, w_field3.
    if not <fs_excel>-kbetr2 is initial.
      if w_lineid is initial.
        w_lineid = wa_kschl_cnt-count + 1.
      else.
        w_lineid = w_lineid + 1.
      endif.
      concatenate 'KONP-KSCHL(' w_lineid ')' into w_field2.
      concatenate 'KONP-KBETR(' w_lineid ')' into w_field3.
      perform fill_condn_table using w_field2 w_field3
                                     'ZP17' <fs_excel>-kbetr2.
    endif.
*&-----Third Condition.
    clear: w_field2, w_field3.
    if not <fs_excel>-kbetr3 is initial.
      if w_lineid is initial.
        w_lineid = wa_kschl_cnt-count + 1.
      else.
        w_lineid = w_lineid + 1.
      endif.
      concatenate 'KONP-KSCHL(' w_lineid ')' into w_field2.
      concatenate 'KONP-KBETR(' w_lineid ')' into w_field3.
      perform fill_condn_table using w_field2 w_field3
                                     'ZP18' <fs_excel>-kbetr3.
    endif.
*&----End
    perform bdc_dynpro      using 'SAPMV13A' '0201'.
    perform bdc_field       using 'BDC_OKCODE'  '=SICH'.
    perform bdc_transaction tables it_messtab
                            using 'ME12' space space space.
    w_flag = 'X'.
    clear: w_field1, w_field2, w_field3, w_lineid,w_valprd_id.
  endloop.
  perform close_group using space.
  if w_flag ne 'X'.
  else.
    message id 'ZMMM' type 'I' number '999' with text-001.
  endif.
  include bdcrecxy.
*&---------------------------------------------------------------------*
*&      Form  fill_condn_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_FIELD  text
*      -->P_0313   text
*----------------------------------------------------------------------*
FORM fill_condn_table using p_fnam1
                            p_fnam2
                            p_kschl
                            p_kbetr.

  perform bdc_dynpro      using 'SAPMV13A' '0201'.
  perform bdc_field       using 'BDC_CURSOR' p_fnam1.
  perform bdc_field       using 'BDC_OKCODE' '/00'.
  perform bdc_field       using p_fnam1 p_kschl.           "KSCHL.
  perform bdc_field       using p_fnam2 p_kbetr.           "KBETR.

ENDFORM.                    " fill_condn_table
