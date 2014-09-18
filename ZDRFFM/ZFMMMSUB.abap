FUNCTION ZFMMMSUB.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_LENUM) LIKE  LEIN-LENUM OPTIONAL
*"     VALUE(I_NLTYP) LIKE  LTAP-NLTYP OPTIONAL
*"     VALUE(I_NLPLA) LIKE  LTAP-NLPLA OPTIONAL
*"     VALUE(I_UNAME) LIKE  SY-UNAME
*"  TABLES
*"      T_LEINM STRUCTURE  LEINM
*"----------------------------------------------------------------------
*&--------------------------------------------------------------------&*
*&    Program: ZFMMMSUB.                                              &*
*&    Author : Shiva.                                                 &*
*&    Specification: Move Storage Unit using RF                       &*
*&--------------------------------------------------------------------&*
*& Date        User     Transport       Description
*& 09/19/2005  Shiva    UD1K917646      initial program.
*&--------------------------------------------------------------------&*

  data: begin of wa_lein,
          lenum like lein-lenum,
        end of wa_lein.
  data: it_lein like table of wa_lein.

  data: w_lines type i.

  constants: c_lgnum like t301-lgnum value 'P01'.

  if i_uname is initial.
    wa_leinm-text = text-W22.
    if t_leinm[] is initial.
      insert wa_leinm into table t_leinm.
    else.
      modify t_leinm from wa_leinm transporting text
                                   where text is initial.
    endif.
    exit.
  else.
    w_uname = i_uname.
  endif.
  if i_lenum is initial.
    if ( i_nltyp is initial or i_nlpla is initial ).
      wa_leinm-text = text-W16.
      if t_leinm[] is initial.
        insert wa_leinm into table t_leinm.
      else.
        modify t_leinm from wa_leinm transporting text
                                     where text is initial.
      endif.
      exit.
    endif.
    describe table t_leinm lines w_lines.
    if w_lines = 0.
      wa_leinm-text = text-W17.
      insert wa_leinm into table t_leinm.
      exit.
    endif.
    select single lgtyp from t301 into w_nltyp
                        where lgnum = c_lgnum
                        and   lgtyp = i_nltyp.
    if sy-subrc ne 0.
      wa_leinm-text = text-W18.
      modify t_leinm from wa_leinm transporting text
                                   where text is initial.
      exit.
    endif.
    select single lgpla from lagp into w_nlpla
                        where lgnum = c_lgnum
                        and   lgtyp = i_nltyp
                        and   lgpla = i_nlpla.
    if sy-subrc ne 0.
      wa_leinm-text = text-W19.
      modify t_leinm from wa_leinm transporting text
                                   where text is initial.
      exit.
    endif.
    it_leinm[] = t_leinm[].
    select lenum from lein
                 into table it_lein
                 for all entries in it_leinm
                 where lenum = it_leinm-lenum.
    if sy-subrc ne 0.
      wa_leinm-text = text-W20.
      modify t_leinm from wa_leinm transporting text
                                   where text is initial.
      exit.
    endif.
    sort it_lein by lenum.
    loop at it_leinm assigning <fs_leinm>.
      read table it_lein transporting no fields
                         with key lenum = <fs_leinm>-lenum
                                      binary search.
      if sy-subrc ne 0.
        <fs_leinm>-text = text-W20.
        <fs_leinm>-processed = space.
      endif.
    endloop.
  else.
    if ( not i_nltyp is initial ) or ( not i_nlpla is initial ).
      wa_leinm-text = text-W21.
      insert wa_leinm into table t_leinm.
      exit.
    endif.
    select single lenum from lein into w_lenum
                        where lenum = i_lenum.
    if sy-subrc ne 0.
      wa_leinm-text = text-W20.
      insert wa_leinm into table t_leinm.
      exit.
    endif.
  endif.

  if w_lenum is initial.
    perform move_su.
  else.
    select lgnum matnr werks meins gesme lgort
                             into table it_lqua
                             from lqua
                             where lenum = w_lenum.
    if sy-subrc ne 0.
    else.
      wa_ltak-bwlvs = c_bwlvs.
      wa_ltak-lgnum = wa_lqua-lgnum.
      wa_ltak-kquit = 'X'.
      append wa_ltak to it_ltak.
      loop at it_lqua into wa_lqua.
        wa_ltap_creat-matnr = wa_lqua-matnr.
        wa_ltap_creat-werks = wa_lqua-werks.
        wa_ltap_creat-lgort = wa_lqua-lgort.
        wa_ltap_creat-vlenr = w_lenum.
        wa_ltap_creat-altme = wa_lqua-meins.
        wa_ltap_creat-anfme = wa_lqua-menge.
        append wa_ltap_creat to it_ltap_creat.
      endloop.
      perform break_down.
    endif.
  endif.

  t_leinm[] = it_leinm[].

ENDFUNCTION.
