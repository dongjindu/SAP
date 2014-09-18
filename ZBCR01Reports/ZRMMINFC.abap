REPORT ZRMMINFC line-size 134.
*&--------------------------------------------------------------------&*
*&  Program id   : ZRMMINFC.
*&  Author       : Shiva.
*&  Specification: Display materials with and without Info record.
*&
*&--------------------------------------------------------------------&*
*& Date        User   Transport        Description
*& 01/14/2005  Shiva  UD1K913849       initial program.
*&--------------------------------------------------------------------&*
data: begin of wa_mara,
        matnr like mara-matnr,
        mtart like mara-mtart,
        profl like mara-profl,
        ekgrp like marc-ekgrp,
        dispo like marc-dispo,
        maktx like makt-maktx,
      end of wa_mara.
data: begin of wa_eina,
         matnr like eina-matnr,
         lifnr like eina-lifnr,
         ekgrp like eine-ekgrp,
         name1 like lfa1-name1,
         loekz like eina-loekz,
         prdat like eine-prdat,
       end of wa_eina.
data: begin of wa_info_rec,
        matnr like mara-matnr,
        maktx like makt-maktx,
        mtart like mara-mtart,
        profl like mara-profl,
        ekgrp_m like eine-ekgrp,
        ekgrp_i like eine-ekgrp,
        dispo like marc-dispo,
        lifnr like eina-lifnr,
        name1 like lfa1-name1,
        prdat like eine-prdat,
        loekz like eina-loekz,
      end of wa_info_rec.
data: it_mara like table of wa_mara,
      it_eina like table of wa_eina,
      it_info_rec like table of wa_info_rec.

data: w_nlines type i.

select-options: s_matnr for wa_mara-matnr,
                s_mtart for wa_mara-mtart,
                s_profl for wa_mara-profl,
                s_ekgrp for wa_mara-ekgrp,
                s_dispo for wa_mara-dispo,
                s_loekz for wa_eina-loekz.
parameters: p_info     radiobutton group rgrp,
            p_noinfo  radiobutton group rgrp,
            p_all      radiobutton group rgrp default 'X'.

at selection-screen.
  if s_matnr is initial.
    if s_mtart is initial.
      if s_profl is initial.
        if s_dispo is initial.
          if s_ekgrp is initial.
            message id 'ZMMM' type 'E' number '009' with text-001.
            exit.
          endif.
        endif.
      endif.
    endif.
  endif.

start-of-selection.
  select t1~matnr mtart profl ekgrp dispo maktx
                into table it_mara
                from mara as t1
                inner join marc as t2
                on t2~matnr = t1~matnr
                inner join makt as t3
                on t3~matnr = t1~matnr
                where t1~matnr in s_matnr
                and   mtart in s_mtart
                and   dispo in s_dispo
                and   profl in s_profl
                and   ekgrp in s_ekgrp
                and   spras eq sy-langu.
  describe table it_mara lines w_nlines.
  if w_nlines = 0 .
    message id 'ZMMM' type 'E' number '009' with text-002.
    exit.
  endif.
  select matnr t4~lifnr ekgrp name1 t5~loekz prdat
                   into table it_eina
                   from eina as t4
                   inner join eine as t5
                   on t5~infnr = t4~infnr
                   and t5~loekz = t4~loekz
                   inner join lfa1 as t6
                   on t6~lifnr = t4~lifnr
                   where t4~loekz in s_loekz.
  describe table it_eina lines w_nlines.
  if w_nlines = 0 .
    message id 'ZMMM' type 'E' number '009' with text-003.
    exit.
  endif.

  sort: it_mara by matnr,
        it_eina by matnr.

  loop at it_mara into wa_mara.
    loop at it_eina into wa_eina
       where matnr = wa_mara-matnr.
      move-corresponding: wa_mara to wa_info_rec,
                          wa_eina to wa_info_rec.
      wa_info_rec-ekgrp_m = wa_mara-ekgrp.
      wa_info_rec-ekgrp_i = wa_eina-ekgrp.
      append wa_info_rec to it_info_rec.
    endloop.
  endloop.
  if p_all = 'X' or p_info = 'X'.
    write: / 'Materials with Info record' centered.
    skip.
    format color col_heading.
    write: /(18) 'Material', (25) 'Description', (4) 'Type', (3) 'Sce',
              (7) text-004, (3) 'MRP Cont',
              (10) 'Vendor', (37) 'Name', (10) 'Valid','Del Flag'.
    write : / text-005 under text-004.
    format color off.
    loop at it_info_rec into wa_info_rec.
      write: /(18) wa_info_rec-matnr, (25) wa_info_rec-maktx,
               wa_info_rec-mtart,wa_info_rec-profl, wa_info_rec-ekgrp_m,
        wa_info_rec-ekgrp_i,wa_info_rec-dispo, space, wa_info_rec-lifnr,
               wa_info_rec-name1,  wa_info_rec-prdat, wa_info_rec-loekz.
    endloop.
    skip 2.
  endif.
  if p_all = 'X' or p_noinfo = 'X'.
    write: / 'Materials without Info record' centered.
    format color col_heading.
    write: /(18) 'Material', (25) 'Description', (4) 'Type', (3) 'Sce',
              (3) 'PGRPM', (3) 'MRP Cont'.
    format color off.
    loop at it_mara into wa_mara.
      read table it_eina with key matnr = wa_mara-matnr
                                        transporting no fields.
      if sy-subrc ne 0.
        write: /(18) wa_mara-matnr, (25) wa_mara-maktx, wa_mara-mtart,
                 wa_mara-profl, wa_info_rec-ekgrp_m, wa_mara-dispo.
      endif.
    endloop.
  endif.
