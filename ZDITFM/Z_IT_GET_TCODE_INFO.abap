function z_it_get_tcode_info.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_TCODE) TYPE  TCODE
*"  EXPORTING
*"     VALUE(O_DEVCLASS) TYPE  DEVCLASS
*"     VALUE(O_CTEXT) TYPE  AS4TEXTOLD
*"     VALUE(O_PGMNA) TYPE  PROGRAM_ID
*"     VALUE(O_TTEXT) TYPE  TTEXT_STCT
*"     VALUE(O_CALL_TCODE) TYPE  TCODE
*"     VALUE(O_TCTYP) TYPE  ZITTCTYP
*"     VALUE(O_TASKV) TYPE  ZITBPLV
*"----------------------------------------------------------------------
*LSEUKTOP
  data: hex_tra type x value '00',               " Transaktion         T
        hex_men type x value '01',               " Area menu           -
        hex_par type x value '02',               " Parametertrans.     P
        hex_rep type x value '80',               " Report              R
        hex_rpv type x value '10',               " Report  w Variante  V
        hex_obj type x value '08',               " Objekttransaktionen
        hex_chk type x value '04',               " mit Prüfobjekt
        hex_enq type x value '20'.               " Gesperrt über SM01

  data: l_tcode type tcode,
        param_beg type i,
        l_cinfo type syhex01.

  tables: tstcp, *tstc.

  l_tcode = i_tcode.
  condense l_tcode no-gaps.

  select single devclass into o_devclass from tadir
     where pgmid = 'R3TR'
       and object = 'TRAN'
       and obj_name = l_tcode.

  select single ctext into o_ctext from tdevct
     where spras    = sy-langu
       and devclass = o_devclass.

  select single pgmna cinfo into (o_pgmna, l_cinfo)
     from tstc
     where tcode = l_tcode.

  select single ttext into o_ttext
     from tstct
     where sprsl = sy-langu
       and tcode = l_tcode.

  if l_cinfo o hex_rep.             " Report
    o_tctyp = 'R'.
  elseif l_cinfo o hex_par.
    o_tctyp = 'P'.                    " Trans w. param inc. view maintenance
  elseif l_cinfo o hex_men.         " Menü
    o_tctyp = ' '.
  else.                                " Transaktion
    o_tctyp = 'T'.
  endif.


  if o_pgmna is initial.
*-----  LSEUKF01
    select single * from tstcp where tcode = l_tcode.

    if tstcp-param(1) = '/'.
      if tstcp-param ca ' '. endif.
      param_beg = sy-fdpos + 1.
      subtract 2 from sy-fdpos.
      if sy-fdpos gt 0.
        o_call_tcode = tstcp-param+2(sy-fdpos).
      endif.
    endif.

    select single pgmna into o_pgmna
       from tstc
       where tcode = o_call_tcode.


        shift tstcp-param by param_beg places.
        data: param type rsparam occurs 0 with header line.
        field-symbols <f>.
        do 254 times.
          if tstcp-param = space. exit. endif.
          clear param.
          condense tstcp-param no-gaps.
          if tstcp-param ca '='.
            check sy-fdpos ne 0.
            assign tstcp-param(sy-fdpos) to <f>.
            param-field = <f>.
            if param-field(1) = space. shift  param-field. endif.
            sy-fdpos = sy-fdpos + 1.
            shift tstcp-param by sy-fdpos places.
            if tstcp-param ca ';'.
              if sy-fdpos ne 0.
                assign tstcp-param(sy-fdpos) to <f>.
                param-value = <f>.
                if param-value(1) = space. shift  param-value. endif.
              endif.
            endif.
            if param-field = 'VIEWNAME'.
              o_TASKV = tstcp-param.
            endif.

          endif.
        enddo.

  endif.


endfunction.
