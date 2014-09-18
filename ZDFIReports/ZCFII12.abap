*&---------------------------------------------------------------------*
*& Report  ZCFII91                                                     *
*& Author                 :  WSKIM
*& Creation Date          : 10/08/2004
*& Specification By       : YC, YOON, Andy Choi
*& Pattern                : Report 1-1
*& Development Request No :
*& Addl documentation     :
*& Description  :
*&
*& Modification Log
*& Date     Developer      Request ID      Description
*&--------------------------------------------------------------------

report zcfii12   no standard page heading
                 line-size 132
                 line-count 65
                 message-id zmfi.
*Tables
tables : anla,ania,ankb,aufk,anlc.

*Internal tables
data: begin of it_anla occurs 0,
        bukrs like anla-bukrs,
        anln1 like anla-anln1,
        anln2 like anla-anln2,
        answl like anlc-answl, "value
        anlkl like anla-anlkl,
        eaufn like anla-eaufn,
        aktiv like anla-aktiv, "Asset capitalization date
        zugdt like anla-zugdt, "Asset value date of the first posting
       end of it_anla.

data : begin of it_data occurs 0,
        anln1 like anla-anln1,
        anln2 like anla-anln2,
        afasl like anlb-afasl, " dep key
        ndjar like anlb-ndjar, " usfl life
        afabg like anlb-afabg, " o.dep.start
        check,
        answl like anlc-answl, "value
       end of it_data.

data: it_msg                 like table of bdcmsgcoll  with header line,
      it_bdcdata             like table of bdcdata     with header line.

data : z_eaufn like ania-objnr,
       z_ktogr like ania-ktogr,
       w_int type i.
data: external_date(10),
      internal_date type d,
      original_date type d.

data: noheader type c value 'X'.
data: g_rc like sy-subrc.

selection-screen begin of block b1 with frame title text-001.
parameters: p_afabe like ankb-afabe default '20'.
parameters: p_reset as checkbox default 'X'.
selection-screen end of block b1.

parameters :  p_mode  type c default 'E'.
parameters :  p_test  as checkbox   default 'X'.

selection-screen begin of block b2 with frame title text-002.
select-options : s_anlkl for anla-anlkl default '4500',
                 s_anln1 for anla-anln1,
                 s_eaufn for anla-eaufn,
                 s_aktiv for anla-aktiv,  "Cap. date
                 s_zugdt for anla-zugdt.  "First posting

selection-screen end of block b2.
parameters :  p_value  as checkbox   default 'X'.

*AT SELECTION-SCREEN ON RADIOBUTTON GROUP rd1.

at selection-screen output.

start-of-selection.
  perform get_data.
  if p_test = 'X'.
  else.
    perform update_process.
  endif.

end-of-selection.
  perform write.
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_data.
  refresh: it_anla,it_data.

  select * into corresponding fields of table it_anla
  from anla as a
  inner join anlb as b
     on b~bukrs = a~bukrs
    and b~anln1 = a~anln1
    and b~anln2 = a~anln2
    and b~afabe = p_afabe
  where a~anln1 in s_anln1
    and a~anlkl in s_anlkl
    and a~eaufn in s_eaufn
    and a~aktiv in s_aktiv  "Cap. date
    and a~zugdt in s_zugdt.  "First posting

  if p_value = 'X'.
    loop at it_anla.
*check asset value > 0.
      select sum( answl ) into it_anla-answl
         from anlc
         where bukrs = it_anla-bukrs
           and anln1 = it_anla-anln1
           and afabe = '01'.
      if it_anla-answl = 0.
        delete it_anla index sy-tabix.
      else.
        modify it_anla.
      endif.
    endloop.
  endif.

  describe table it_anla lines w_int.
  if w_int = 0.
    message w001 with 'Not found Asset master'.
    exit.
  else.
    if p_reset = 'X'.
      loop at it_anla.
        it_data-anln1 = it_anla-anln1.
        it_data-anln2 = it_anla-anln2.
        it_data-afasl = '0000'.
        it_data-ndjar = 0.
        it_data-afabg = it_anla-aktiv.
        it_data-check = 'X'.
        append  it_data.clear it_data.
      endloop.
    else.
      loop at it_anla.
        it_data-answl = it_anla-answl. "value

*get order information
        clear : z_eaufn, z_ktogr.
        concatenate 'OR' it_anla-eaufn into z_eaufn.
       select single ktogr aktiv into (z_ktogr,it_data-afabg) from ania
                  where objnr eq z_eaufn .

        if sy-subrc = 0.
          select single afasl ndjar
           into (it_data-afasl,it_data-ndjar) from ankb
               where anlkl eq  z_ktogr
                 and afapl eq 'US'
                 and afabe eq p_afabe.
          if sy-subrc = 0.
            it_data-anln1 = it_anla-anln1.
            it_data-anln2 = it_anla-anln2.
            it_data-check = 'X'.     "RESET!!!
            append  it_data.clear it_data.
          else.
            clear it_anla.
          endif.
        else.
          it_data-anln1 = it_anla-anln1.
          it_data-anln2 = it_anla-anln2.
          it_data-check = ' '.
          append  it_data.  clear it_data.
          clear it_anla.
        endif.
      endloop.
    endif.
  endif.
endform.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  update_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form update_process.
  refresh it_bdcdata.
  sort it_data ascending by check anln1.
  loop at it_data where check eq 'X'.
    refresh it_bdcdata.
    perform bdc_dynpro_processing using :
                           'X'  'SAPLAIST'             '0100',
                           ' '  'BDC_OKCODE'           '/00' ,
                           ' '  'ANLA-ANLN1'           it_data-anln1,
                           ' '  'ANLA-ANLN2'           '0',
                           ' '  'ANLA-BUKRS'           'H201',

                           'X'  'SAPLAIST'             '1000',
                           ' '  'BDC_OKCODE'           '=TAB08'.

    perform date_conversion using  it_data-afabg
                            changing external_date.

    if p_afabe = '01'.
      perform bdc_dynpro_processing using :
                             'X'  'SAPLAIST'             '1000',
                             ' '  'BDC_OKCODE'           '=BUCH' ,
                             ' '  'ANLB-AFASL(01)'       it_data-afasl,
                             ' '  'ANLB-NDJAR(01)'       it_data-ndjar.

      perform bdc_dynpro_processing using :
                            ' '  'ANLB-AFABG(01)'      external_date.
    elseif p_afabe = '20'.
      perform bdc_dynpro_processing using :
                             'X'  'SAPLAIST'             '1000',
                             ' '  'BDC_OKCODE'           '=BUCH' ,
                             ' '  'ANLB-AFASL(03)'       it_data-afasl,
                             ' '  'ANLB-NDJAR(03)'       it_data-ndjar.

      perform bdc_dynpro_processing using :
                            ' '  'ANLB-AFABG(03)'      external_date.
    endif.

    call transaction 'AS02'  using it_bdcdata           mode p_mode
                                messages into           it_msg .

  endloop.

endform.                    " update_process
*&---------------------------------------------------------------------*
*&      Form  bdc_dynpro_processing
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0195   text
*      -->P_0196   text
*      -->P_0197   text
*----------------------------------------------------------------------*
form bdc_dynpro_processing  using    dy_begin  pg_name   sc_no.
  if dy_begin = 'X'.
    clear it_bdcdata.
    move  pg_name  to it_bdcdata-program.
    move  sc_no    to it_bdcdata-dynpro.
    move  'X'      to it_bdcdata-dynbegin.
    append it_bdcdata.
  else.
    clear it_bdcdata.
    move  pg_name  to it_bdcdata-fnam.
    move  sc_no    to it_bdcdata-fval.
    append it_bdcdata.
  endif.
endform.                    " bdc_dynpro_processing
*&---------------------------------------------------------------------*
*&      Form  WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form write.
  clear w_int.
  describe table it_data lines w_int.
  write :/ 'Count', w_int.
  loop at it_data.
    write : / it_data-anln1,it_data-afasl,it_data-ndjar, it_data-afabg,
                      it_data-check, it_data-answl.
  endloop.

  loop at it_msg where msgtyp eq 'E'.
    write : / it_msg-msgtyp,it_msg-msgid,it_msg-msgnr, it_msg-msgv1.
  endloop.
endform.                    " WRITE
*&---------------------------------------------------------------------*
*&      Form  date_conversion
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_DATA_AFABG  text
*      <--P_EXTERNAL_DATE  text
*----------------------------------------------------------------------*
form date_conversion using    p_it_data_afabg
                     changing p_external_date.

  original_date = p_it_data_afabg.

  call 'DATE_CONV_INT_TO_EXT'
  id 'DATINT' field original_date
  id 'DATEXT' field p_external_date.

endform.                    " date_conversion
