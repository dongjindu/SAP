*----------------------------------------------------------------------*
*   INCLUDE ZIPP308L_OBJ_DEP_BDC_F01                                   *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
form initialization.
  refresh: it_bdc, it_mess.
  clear:   it_bdc, it_mess.
* BDC TYPE
  wa_opt-defsize = 'X'.
  wa_opt-dismode = 'N'.
  wa_opt-updmode = 'S'.
endform.                               " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  SCREEN_MODIFY
*&---------------------------------------------------------------------*
form screen_modify.
endform.                    " SCREEN_MODIFY
*&---------------------------------------------------------------------*
*&      Form  AT_SEL_SCREEN_ON_VALUE_REQUEST
*&---------------------------------------------------------------------*
form at_sel_screen_on_value_request using def_path like rlgrap-filename
                                          mode     type c.
endform.                               " AT_SEL_SCREEN_ON_VALUE_REQUEST
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_PROCESS
*&---------------------------------------------------------------------*
form upload_process.
endform.                               " UPLOAD_PROCESS
*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
form read_process.
  select *
       from ztbm_abyodpdt
       into table it_aodp
       where zedat eq p_zedat
       and   zbtim eq p_zbtim
       and  ( zresult eq space or zresult eq 'E' ).
  if sy-subrc ne 0.
    write: / text-007.
  endif.
endform.                    " READ_PROCESS
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
form data_process.
  data: l_tabix type sy-tabix.
  data: lt_aodp like it_aodp occurs 0 with header line.
* DATA TOTAL LINES
  describe table it_aodp lines wa_line_idx.

  describe table lt_aodp lines wa_erro_idx.

  format color col_negative intensified off.
  write: / text-008, wa_line_idx.
  write: / text-009, wa_erro_idx.
  format color off.
  if not lt_aodp[] is initial.
    write: / text-010.
    perform write_error tables    lt_aodp
                        using     'L'.

  endif.
endform.                    " DATA_PROCESS
*&---------------------------------------------------------------------*
*&      Form  BDC_PROCESS
*&---------------------------------------------------------------------*
form bdc_process.
  data: l_nchr(10),
        l_tabix type sy-tabix,
        l_msg  like cfgnl-msglin,
        l_msgty type sy-msgty.
  data: l_knnam type cukb-knnam,
        c_comma type x  value 09,
        l_index type i,
        l_s type i.

*/////////////////////////////////////////*
  data  : $string(180).

* by IG.MOON 8/24/2007 {
  data : begin of it_atbez occurs 0,
*           CLID     LIKE KLAH-CLASS,
           atnam     like cabn-atnam,
           atbez     like cabnt-atbez,
*           VALUE(2),
         end   of  it_atbez.

  select " A~CLASS
           c~atnam
           d~atbez into table it_atbez
  from klah as a
  inner join ksml as b
  on b~clint eq a~clint
  inner join cabn as c
  on c~atinn eq b~imerk
  inner join cabnt as d
  on d~atinn eq c~atinn
  where a~klagr eq 'COLOR'
    and c~atnam >= 'COL_'.

  sort it_atbez by atbez.
  delete adjacent duplicates from it_atbez comparing atbez.

* }
*/////////////////////////////////////////*


  loop at it_aodp where zresult ne 'P'.
    l_tabix = sy-tabix.

    clear :  l_index, l_s .

*    CLEAR :  L_0093 ,L_8811, L_INDEX, L_S .

    clear: ln_cnt,last_line,last_fline.

* Initial Screen
    perform dynpro using:
       'X' 'SAPLCUKD'    '0100',
       ' ' 'RCUKD-KNNAM' it_aodp-dpid,    "DEPENDENCY
       ' ' 'RCUKD-AENNR' it_aodp-eono,    "ECM NO
       ' ' 'BDC_OKCODE'  '/00',

       'X' 'SAPLCUKD'    '0110',
       ' ' 'RCUKD-KNKTX' it_aodp-ddes,    "DEPENDENCY DESCRIPTION
       ' ' 'RCUKD-KNGRP' 'COLOR',    "DEPENDENCY GROUP
       ' ' 'RCUKD-KNABD' 'X',    "Selection condition
       ' ' 'RCUKD-KNSTA' '2'  , "IT_AODP-STAT,    "STATUS
       ' ' 'BDC_OKCODE'  '=KNED'.

    ln_cnt = 10.
    last_line = 10.

* CAR SPEC
    if not  it_aodp-cspec  is initial.
     concatenate '$ROOT.COl_CAR_SPEC = ''' it_aodp-cspec ''''    ' AND'
                                     into  ln_c.
      perform dynpro using:
*       'X' 'SAPLEDITOR_START'    '2210',
       'X' 'SAPLEDITOR_START'    '3210',
       ' ' 'BDC_CURSOR'          'RSTXP-TDLINECOM(01)',
        ' ' 'RSTXP-TDLINE(01)'    ln_c, "EDIC: Program
        ' ' 'BDC_OKCODE'  '=ACIL'.
      last_line  = last_line + 4.
    endif.

* COLOR
    if not it_aodp-eicol is initial .
      if it_aodp-eicol eq 'I'.    " Internal Color
        concatenate '$ROOT.COL_INT = ''' it_aodp-kclr ''''    ' AND'
        into  ln_c.
        perform enter_code.
      elseif it_aodp-eicol eq 'E' .  " External Color
        concatenate '$ROOT.COL_EXT = ''' it_aodp-kclr ''''    ' AND'
       into  ln_c.
        perform enter_code.
      endif.
    endif.
*
* Drive TYpe
    if not ( it_aodp-drty is initial or it_aodp-drty eq '-' ).
      concatenate '$ROOT.COL_DT = ''' it_aodp-drty ''''    ' AND'
     into  ln_c.
      perform enter_code.
    endif.
*
** Weather TYpe
    if not ( it_aodp-wety is initial or it_aodp-wety eq '-' ).
      concatenate '$ROOT.COL_WT = ''' it_aodp-wety ''''    ' AND'
     into  ln_c.
      perform enter_code.
    endif.
*
** Body TYpe
    if not ( it_aodp-boty is initial or it_aodp-boty eq '-' ) .
      concatenate '$ROOT.COL_BT = ''' it_aodp-boty ''''    ' AND'
     into  ln_c.
      perform enter_code.
    endif.
*
*
** Engine Capacity
    if not ( it_aodp-encapa is initial or it_aodp-encapa eq '-' ).
      concatenate '$ROOT.COL_EC = ''' it_aodp-encapa ''''    ' AND'
     into  ln_c.
      perform enter_code.
    endif.
*
*
** Engine TYpe
    if not ( it_aodp-enty is initial or it_aodp-enty eq '-' ) .
      concatenate '$ROOT.COL_ET = ''' it_aodp-enty ''''    ' AND'
     into  ln_c.
      perform enter_code.
    endif.
*
** FUEL TYPE
    if not ( it_aodp-futy is initial or it_aodp-futy eq '-' ).
      concatenate '$ROOT.COL_FT = ''' it_aodp-futy ''''    ' AND'
     into  ln_c.
      perform enter_code.
    endif.
*
**Transmission code
    if not ( it_aodp-tmcd is initial  or it_aodp-tmcd eq '-' ).
      concatenate '$ROOT.COL_TM = ''' it_aodp-tmcd ''''    ' AND'
     into  ln_c.
      perform enter_code.
    endif.
*
** Special CAR
    if not ( it_aodp-spcar is initial or it_aodp-spcar eq '-' ).
      concatenate '$ROOT.COL_SP = ''' it_aodp-spcar ''''    ' AND'
     into  ln_c.
      perform enter_code.
    endif.
*
** Nation
    if not ( it_aodp-nat is initial or it_aodp-nat eq '-' ) .
      concatenate '$ROOT.COL_NAT = ''' it_aodp-nat ''''    ' AND'
     into  ln_c.
      perform enter_code.
    endif.
*
** Grade
    if not ( it_aodp-grd is initial or it_aodp-grd eq '-'   ) .
      concatenate '$ROOT.COL_GR = ''' it_aodp-grd ''''    ' AND'
     into  ln_c.
      perform enter_code.
    endif.
*
** Combi Option
* Each of the below charactertics can repeat multiple times...if
*repeated  more than once we need to construct in clause
* like ('A1','A2','A3')

    if not  it_aodp-copt is initial.

      loop at it_atbez.
        l_s = 1.
        sy-subrc = 1.
        clear $string.

* by IG.MOON 9/23/2007 {
        __cls i_string.
* }

        while  l_s <= 60.
          search it_aodp-copt for it_atbez-atbez
                            and mark.
          if sy-subrc eq 0 and i_string[] is initial.
            l_index = sy-fdpos + 4.
            i_string-$code = it_aodp-copt+l_index(2).
            append i_string.
* replace found code {
            write '^^^^^^' to it_aodp-copt+sy-fdpos(6).
* }
            l_s = l_index + 2.
          elseif   sy-subrc eq 0 and not i_string[] is initial.
            l_index = sy-fdpos  + 4.
            i_string-$code = it_aodp-copt+l_index(2).
            append i_string.
* replace found code {
            write '^^^^^^' to it_aodp-copt+sy-fdpos(6).
* }
            l_s = l_index + 2.
          else.
            l_s = l_s + 6.
          endif.
        endwhile.

        if not i_string[] is initial.

* by IG.MOON 9/23/2007 {
          perform create_string tables i_string
                              changing $string.

          concatenate '$ROOT.' it_atbez-atnam
                      ' IN (  '  $string  ')'  ' AND'
                      into  ln_c.

* }

          perform enter_code.
        endif.
      endloop.
    endif.

** Exclusive Option
    if not  it_aodp-eopt is initial.
      loop at it_atbez.
        l_s = 1.
        sy-subrc = 1.
        clear $string.

* by IG.MOON 9/23/2007 {
        __cls i_string.
* }
        while  l_s <= 60.
          search it_aodp-eopt for it_atbez-atbez
                              and mark.
          if sy-subrc eq 0 and i_string[] is initial.
            l_index = sy-fdpos + 4.
            i_string-$code = it_aodp-eopt+l_index(2).
            append i_string.

* replace found code {
            write '^^^^^^' to it_aodp-eopt+sy-fdpos(6).
* }
            l_s = l_index + 2.

          elseif sy-subrc eq 0 and not i_string[] is initial.
            l_index = sy-fdpos  + 4.
            i_string-$code = it_aodp-eopt+l_index(2).
            append i_string.
* replace found code {
            write '^^^^^^' to it_aodp-eopt+sy-fdpos(6).
* }

            l_s = l_index + 2.

          else.
            l_s = l_s + 6.
          endif.
        endwhile.

        if not i_string[] is initial.

* by IG.MOON 9/23/2007 {
          perform create_string tables i_string
                              changing $string.

          concatenate 'NOT ($ROOT.' it_atbez-atnam
                      ' IN ( '  $string  '))'  ' AND'
                      into  ln_c.
* }
          perform enter_code.

        endif.
      endloop.
    endif.

* To Remove AND from Last statement.
    last_fline = last_line.
    last_line  = last_line - 1.
    loop at it_bdc from last_line.
      replace 'AND' with space into it_bdc-fval .
      modify it_bdc transporting fval.
      exit.
    endloop.
    loop at it_bdc from last_fline.
      replace '=ACIL' with '/00' into it_bdc-fval .
      modify it_bdc transporting fval.
      exit.
    endloop.

    perform dynpro using:
          'X' 'SAPLEDITOR_START'    '3210',
          ' ' 'BDC_CURSOR'          'RSTXP-TDLINECOM(02)',
          ' ' 'BDC_OKCODE'  '=VAPR',
          'X' 'SAPLEDITOR_START'    '3210',
          ' ' 'BDC_OKCODE'  '=VASV',
          'X' 'SAPLCUKD'    '0110',
          ' ' 'BDC_OKCODE'  '=SICH'.

    call transaction 'CU01'  using it_bdc
                             options from wa_opt
                             messages into it_mess.

    read table it_mess with key msgid = '28'
                                msgnr = '000'.
    if sy-subrc eq 0.
      l_msgty = 'S'.
      refresh: it_bdc, it_mess.
      perform dynpro using:
         'X' 'SAPLCUKD'    '0100',
         ' ' 'RCUKD-KNNAM' it_aodp-dpid,    "DEPENDENCY
         ' ' 'RCUKD-AENNR' it_aodp-eono,    "ECM NO
         ' ' 'BDC_OKCODE'  '/00',

         'X' 'SAPLCUKD'    '0110',
         ' ' 'RCUKD-KNSTA' it_aodp-stat,    "STATUS
         ' ' 'BDC_OKCODE'  '=SICH'.

    else.
      refresh: it_bdc, it_mess.
      l_msgty = 'E'.

      perform dynpro using:
         'X' 'SAPLCUKD'    '0100',
         ' ' 'RCUKD-KNNAM' it_aodp-dpid,    "DEPENDENCY
         ' ' 'RCUKD-AENNR' it_aodp-eono,    "ECM NO
         ' ' 'BDC_OKCODE'  '=DELB',

         'X' 'SAPLCUKD'    '0220',
         ' ' 'BDC_OKCODE'  '=DELD'.
    endif.
    call transaction 'CU02'  using it_bdc
                             options from wa_opt
                             messages into it_mess.

    perform rkc_msg_string changing l_msg.

*   MODIFY IT_AODP
    perform it_aodp_modify using l_msg
                                 l_msgty
                                 l_tabix.
    refresh: it_bdc, it_mess.
  endloop.

endform.                    " BDC_PROCESS
*&---------------------------------------------------------------------*
*&      Form  BDC_PROCESS_01
*&---------------------------------------------------------------------*
form bdc_process_01.
  data: l_chk(1),
        l_tabix type sy-tabix,
        l_msgty type sy-msgty,
        l_msg  like cfgnl-msglin.
* DATA TOTAL LINES
  describe table it_excl lines wa_line_idx.
  loop at it_excl.
    l_tabix = sy-tabix.
    perform dynpro using:
       'X' 'SAPLCUKD'    '0100',
       ' ' 'RCUKD-KNNAM' it_excl-dpid,    "DEPENDENCY
       ' ' 'RCUKD-AENNR' it_excl-eono,    "ECM NO
       ' ' 'BDC_OKCODE'  '/00',

       'X' 'SAPLCUKD'    '0110',
       ' ' 'RCUKD-KNKTX' it_excl-ddes,    "DEPENDENCY DESCRIPTION
       ' ' 'RCUKD-KNGRP' 'COLOR',    "DEPENDENCY GROUP
       ' ' 'RCUKD-KNABD' 'X',    "Selection condition
*         ' ' 'RCUKD-KNSTA' IT_EXCL-STAT,    "STATUS
       ' ' 'BDC_OKCODE'  '=KNED',
*{ 09/30/11 Paul Change 2210 -> 3210
       'X' 'SAPLEDITOR_START'    '3210',
       ' ' 'BDC_CURSOR'          'RSTXP-TDLINECOM(01)',
       ' ' 'RSTXP-TDLINECOM(01)' it_excl-lin1, "EDIC: Line command
       ' ' 'RSTXP-TDLINE(01)'    it_excl-dpc1, "EDIC: Program editorline
       ' ' 'BDC_OKCODE'  '=ACIL',

       'X' 'SAPLEDITOR_START'    '3210',
       ' ' 'BDC_CURSOR'          'RSTXP-TDLINECOM(02)',
       ' ' 'RSTXP-TDLINECOM(02)' it_excl-lin2, "EDIC: Line command
       ' ' 'RSTXP-TDLINE(02)'    it_excl-dpc2, "EDIC: Program editorline
       ' ' 'BDC_OKCODE'  '=ACIL',

       'X' 'SAPLEDITOR_START'    '3210',
       ' ' 'BDC_CURSOR'          'RSTXP-TDLINECOM(02)',
       ' ' 'RSTXP-TDLINECOM(02)' it_excl-lin3, "EDIC: Line command
       ' ' 'RSTXP-TDLINE(02)'    it_excl-dpc3, "EDIC: Program editorline
       ' ' 'BDC_OKCODE'  '=ACIL',

       'X' 'SAPLEDITOR_START'    '3210',
       ' ' 'BDC_CURSOR'          'RSTXP-TDLINECOM(02)',
       ' ' 'RSTXP-TDLINECOM(02)' it_excl-lin4, "EDIC: Line command
       ' ' 'RSTXP-TDLINE(02)'    it_excl-dpc4, "EDIC: Program editorline
       ' ' 'BDC_OKCODE'  '=ACIL',

       'X' 'SAPLEDITOR_START'    '3210',
       ' ' 'BDC_CURSOR'          'RSTXP-TDLINECOM(02)',
       ' ' 'RSTXP-TDLINECOM(02)' it_excl-lin5, "EDIC: Line command
       ' ' 'RSTXP-TDLINE(02)'    it_excl-dpc5, "EDIC: Program editorline
       ' ' 'BDC_OKCODE'  '=ACIL',

       'X' 'SAPLEDITOR_START'    '3210',
       ' ' 'BDC_CURSOR'          'RSTXP-TDLINECOM(02)',
       ' ' 'RSTXP-TDLINECOM(02)' it_excl-lin6, "EDIC: Line command
       ' ' 'RSTXP-TDLINE(02)'    it_excl-dpc6, "EDIC: Program editorline
       ' ' 'BDC_OKCODE'  '=VAPR',

       'X' 'SAPLEDITOR_START'    '3210',
       ' ' 'BDC_OKCODE'  '=VASV',

       'X' 'SAPLCUKD'    '0110',
       ' ' 'BDC_OKCODE'  '=SICH'.

    call transaction 'CU01'  using it_bdc
                             options from wa_opt
                             messages into it_mess.
    read table it_mess with key msgid = '28'
                                msgnr = '000'.
    if sy-subrc eq 0.
      refresh: it_bdc, it_mess.
      perform rkc_msg_string changing it_excl-zmsg.
      it_excl-zresult = 'S'.
      perform dynpro using:
         'X' 'SAPLCUKD'    '0100',
         ' ' 'RCUKD-KNNAM' it_excl-dpid,    "DEPENDENCY
         ' ' 'RCUKD-AENNR' it_excl-eono,    "ECM NO
         ' ' 'BDC_OKCODE'  '/00',

         'X' 'SAPLCUKD'    '0110',
         ' ' 'RCUKD-KNSTA' it_excl-stat,    "STATUS
         ' ' 'BDC_OKCODE'  '=SICH'.

    else.
      refresh: it_bdc, it_mess.
      it_excl-zresult = 'E'.
      perform rkc_msg_string changing it_excl-zmsg.
      perform dynpro using:
          'X' 'SAPLCUKD'    '0100',
          ' ' 'RCUKD-KNNAM' it_excl-dpid,    "DEPENDENCY
          ' ' 'RCUKD-AENNR' it_excl-eono,    "ECM NO
          ' ' 'BDC_OKCODE'  '=DELB',

          'X' 'SAPLCUKD'    '0220',
          ' ' 'BDC_OKCODE'  '=DELD'.

    endif.
    call transaction 'CU02'  using it_bdc
                             options from wa_opt
                             messages into it_mess.
*   MODIFY IT_EXCL
    modify it_excl index l_tabix transporting zresult
                                              zmsg.

    clear: l_chk.
    refresh: it_bdc, it_mess.
  endloop.
endform.                    " BDC_PROCESS_01
*---------------------------------------------------------------------*
*       FORM DYNPRO                                                   *
*---------------------------------------------------------------------*
form dynpro using dynbegin name value.
  if dynbegin = 'X'.
    clear it_bdc.
    move: name to it_bdc-program,
          value to it_bdc-dynpro,
          dynbegin to it_bdc-dynbegin.
    append it_bdc.
  else.
    clear it_bdc.
    move: name to it_bdc-fnam,
          value to it_bdc-fval.
    append it_bdc.
  endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  IT_AODP_MODIFY
*&---------------------------------------------------------------------*
form it_aodp_modify using p_msg
                          p_msgty
                          p_tabix.

  it_aodp-zmsg    = p_msg.
  it_aodp-zresult = p_msgty.
  case p_msgty.
    when 'S'.
      it_aodp-zbdat = sy-datum.  "SAP BDC EXECUTED DATE
      it_aodp-zbtim = sy-uzeit.  "SAP BDC EXECUTED TIME
      it_aodp-zbnam = sy-uname.  "BDC User ID
*     C: Create U: Update D: Delete
      it_aodp-zmode = 'C'.       "C:CREATE
      concatenate 'Create ' '/' p_msg into p_msg.
    when 'E'.
      it_aodp-zbdat = sy-datum.  "SAP BDC EXECUTED DATE
*      IT_AODP-ZBTIM = SY-UZEIT.  "SAP BDC EXECUTED TIME
      it_aodp-zbnam = sy-uname.  "BDC User ID
      it_aodp-zmode = 'C'.       "C:CREATE
    when others.
      it_aodp-zbdat = sy-datum.  "SAP BDC EXECUTED DATE
      it_aodp-zbtim = sy-uzeit.  "SAP BDC EXECUTED TIME
      it_aodp-zbnam = sy-uname.  "BDC User ID
      it_aodp-zmode = 'C'.       "C:CREATE
      concatenate 'Create ' '/' p_msg into p_msg.
  endcase.
* IT_AODP MODIFY
  modify it_aodp index p_tabix transporting zbdat
                                            zbtim
                                            zbnam
                                            zmode
                                            zresult
                                            zmsg.
endform.                    " IT_AODP_MODIFY
*&---------------------------------------------------------------------*
*&      Form  UPDATE_PROCESS
*&---------------------------------------------------------------------*
form update_process.
* ZTBM_ABXODPDT TABLE UPDATE
  perform update_ztbm_abxodpdt.
endform.                    " UPDATE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  UPDATE_ZTBM_ABXODPDT
*&---------------------------------------------------------------------*
form update_ztbm_abxodpdt.
*  update ztbm_abyodpdt from table it_aodp.

  loop at it_aodp.

    update ztbm_abyodpdt set zresult = it_aodp-zresult
                             zmsg = it_aodp-zmsg
                             ZBDAT = sy-datum
                             ZBTIM = sy-uzeit
         where dpid eq it_aodp-dpid
         and zedat eq p_zedat
         and zbtim eq p_zbtim
         and ( zresult eq space or zresult eq 'E' ).

    if sy-subrc ne 0.
      exit.
    endif.

  endloop.

  if sy-subrc eq 0.
    commit work.
  else.
    rollback work.
  endif.

endform.                    " UPDATE_ZTBM_ABXODPDT
*&---------------------------------------------------------------------*
*&      Form  RKC_MSG_STRING
*&---------------------------------------------------------------------*
form rkc_msg_string changing p_msg.
  call function 'RKC_MSG_STRING'
       exporting
            id      = sy-msgid
            mtype   = sy-msgty
            number  = sy-msgno
            par1    = sy-msgv1
            par2    = sy-msgv2
            par3    = sy-msgv3
            par4    = sy-msgv4
       importing
            msg_lin = p_msg
       exceptions
            others  = 1.
endform.                    " RKC_MSG_STRING
*&---------------------------------------------------------------------*
*&      Form  WRITE_PROCESS
*&---------------------------------------------------------------------*
form write_process.
  data l_index type sy-index.
  clear: wa_erro_idx, wa_line_idx.
*  DESCRIBE TABLE IT_AODP LINES WA_LINE_IDX.
  loop at it_aodp.
    if it_aodp-zresult eq 'E' or it_aodp-zresult eq 'P'.
      wa_erro_idx = wa_erro_idx + 1.
    else.
      wa_line_idx = wa_line_idx + 1.
    endif.
  endloop.

  format color col_negative intensified off.
  write: / text-031, wa_line_idx.
  write: / text-032, wa_erro_idx.
  format color off.
  if wa_erro_idx ge 1.
    format color col_heading intensified off.
*    write: / text-032.
    format color off.
    perform write_error tables    it_aodp
                        using     'E'.
  endif.
endform.                    " WRITE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  WRITE_PROCESS_01
*&---------------------------------------------------------------------*
form write_process_01.
  data l_index type sy-index.

  clear: wa_erro_idx, wa_line_idx.
*  DESCRIBE TABLE IT_EXCL LINES WA_LINE_IDX.
  loop at it_excl.
    if it_excl-zresult eq 'E'.
      wa_erro_idx = wa_erro_idx + 1.
    else.
      wa_line_idx = wa_line_idx + 1.
    endif.
  endloop.

  format color col_negative intensified off.
  write: / text-031, wa_line_idx.
  write: / text-032, wa_erro_idx.
  format color off.
  if wa_erro_idx ge 1.
    format color col_heading intensified off.
    write: / text-032.
    format color off.
    perform write_error1 tables   it_excl
                         using    'E'.
  endif.
endform.                    " WRITE_PROCESS_01
*&---------------------------------------------------------------------*
*&      Form  WRITE_ERROR
*&---------------------------------------------------------------------*
form write_error tables   pt_aodp structure it_aodp
                 using    p_zresult.

  loop at pt_aodp where zresult eq p_zresult.
    write: / pt_aodp-dpid,
            (12) pt_aodp-eono,
            (30) pt_aodp-ddes,
            (06) pt_aodp-stat,
            (10) pt_aodp-zresult,
            (100) pt_aodp-zmsg.
  endloop.
endform.                    " WRITE_ERROR
*&---------------------------------------------------------------------*
*&      Form  WRITE_ERROR1
*&---------------------------------------------------------------------*
form write_error1 tables   pt_excl structure it_excl
                  using    p_zresult.

  write: /(18)  text-011,
          (12)  text-012,
          (30)  text-013,
          (10)  text-014,
          (06)  text-015,
          (18)  text-016,
          (72)  text-017,
          (18)  text-018,
          (72)  text-019,
          (18)  text-020,
          (72)  text-021,
          (18)  text-022,
          (72)  text-023,
          (18)  text-024,
          (72)  text-025,
          (18)  text-026,
          (72)  text-027,
          (12)  text-028,
          (10)  text-029,
          (100)  text-030.
  loop at pt_excl where zresult eq p_zresult.
    write: /(18) pt_excl-dpid,
            (12) pt_excl-eono,
            (30) pt_excl-ddes,
            (10) pt_excl-vald,
            (06) pt_excl-stat,
            (18) pt_excl-lin1,
            (72) pt_excl-dpc1,
            (18) pt_excl-lin2,
            (72) pt_excl-dpc2,
            (18) pt_excl-lin3,
            (72) pt_excl-dpc3,
            (18) pt_excl-lin4,
            (72) pt_excl-dpc4,
            (18) pt_excl-lin5,
            (72) pt_excl-dpc5,
            (18) pt_excl-lin6,
            (72) pt_excl-dpc6,
            (12) pt_excl-synt,
            (10) pt_excl-zresult,
            (100) pt_excl-zmsg.
  endloop.
endform.                    " WRITE_ERROR1
