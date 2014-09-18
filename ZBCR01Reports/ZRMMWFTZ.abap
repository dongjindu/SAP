REPORT ZRMMWFTZ .
INCLUDE ZRWFZTOP.
*&--------------------------------------------------------------------&*
*&    Program: ZRMMWFTZ.                                              &*
*&    Author : Shiva.                                                 &*
*&    Specification: FTZ Estimated Duty - Weekly report.              &*
*&--------------------------------------------------------------------&*
*& Date        User     Transport       Description
*& 05/11/2005  Shiva    UD1K915972     initial program.
*&--------------------------------------------------------------------&*

selection-screen begin of block b1 with frame.
parameters: p_cdate like sy-datum obligatory
                                     default sy-datum.
selection-screen begin of line.
parameters: p_kdpart radiobutton group rb1.
selection-screen comment (15) text-001 for field p_kdpart.
parameters: p_lclpar radiobutton group rb1.
selection-screen comment (18) text-002 for field p_lclpar.
parameters: p_both radiobutton group rb1.
selection-screen comment (5) text-003 for field p_both.
selection-screen end of line.
selection-screen end of block b1.

CALL FUNCTION 'GET_WEEK_INFO_BASED_ON_DATE'
     EXPORTING
          DATE = p_cdate
     IMPORTING
          WEEK = w_cur_week.

CALL FUNCTION 'NEXT_WEEK'
     EXPORTING
          CURRENT_WEEK = w_cur_week
     IMPORTING
          NEXT_WEEK    = w_nxt_week
          MONDAY       = w_fdate
          SUNDAY       = w_tdate.

*&-----Get all planned orders for the following week.
select matnr plwrk sum( gsmng ) paltr from plaf
                         into table it_plaf
                         where paart = 'VP'
                         and   pedtr between w_fdate and w_tdate
                  group by matnr plwrk paltr.
if sy-subrc ne 0.
  message id 'ZMMM' type 'I' number '999' with text-004.
  exit.
endif.
*&-----Get Material information.
if p_kdpart = 'X'.
  select mara~matnr profl kordb maktx
                     into table it_mat_info
                     from mara
                     inner join marc
                     on marc~matnr = mara~matnr
                     and marc~lvorm = mara~lvorm
                     inner join makt
                     on makt~matnr = mara~matnr
                     where ( mtart = 'ROH1' and profl = 'K' and
                             mara~lvorm = space  and spras = sy-langu )
                      OR    ( mtart = 'ROH' and profl = 'K' and
                              mara~lvorm = space and spras = sy-langu ).

elseif p_lclpar = 'X'.
  select mara~matnr profl kordb maktx
                          into table it_mat_info
                          from mara
                          inner join marc
                          on marc~matnr = mara~matnr
                          and marc~lvorm = mara~lvorm
                          inner join makt
                          on makt~matnr = mara~matnr
                    where ( mtart = 'ROH1' and profl = 'V' and
                            mara~lvorm = space  and spras = sy-langu )
                    OR    ( mtart = 'ROH' and profl = 'V' and
                            mara~lvorm = space and spras = sy-langu ).
elseif p_both = 'X'.
  select mara~matnr profl kordb maktx
                    into table it_mat_info
                    from mara
                    inner join marc
                    on marc~matnr = mara~matnr
                    and marc~lvorm = mara~lvorm
                    inner join makt
                    on makt~matnr = mara~matnr
                     where ( mtart = 'ROH1' and profl = 'K' and
                             mara~lvorm = space  and spras = sy-langu )
                      OR   ( mtart = 'ROH1' and profl = 'V' and
                             mara~lvorm = space  and spras = sy-langu )
                      OR    ( mtart = 'ROH' and profl = 'K' and
                              mara~lvorm = space  and spras = sy-langu )
                      OR    ( mtart = 'ROH' and profl = 'V' and
                             mara~lvorm = space  and spras = sy-langu ).

endif.

sort it_mat_info by matnr.

*&------Get HTS information.
select t1~stawn text1 kbetr into table it_hts_info
                         from t604 as t1
                         inner join konh as t2
                         on vakey = t1~stawn
                         and kschl = 'ZOA1'
                         inner join konp as t3
                         on t3~knumh = t2~knumh
                         inner join t604t as t4
                         on t4~stawn = t1~stawn
                         and t4~land1 = t1~land1
                         where spras = sy-langu
                         and   t1~land1 = 'US'.
if sy-subrc ne 0.
  message id 'ZMMM' type 'I' number '999' with text-005.
  exit.
endif.
*&------Get Info record information.
perform get_material_price.

r_mtart-option = 'EQ'.
r_mtart-sign    = 'E'.
r_mtart-low = 'ROH'.
append r_mtart.
r_mtart-low = 'ROH1'.
append r_mtart.

loop at it_plaf assigning <fs_plaf>.
  call function 'Z_FFTZ_EXP_BOM'
       starting new task w_taskname
       destination in group 'PG_FTZ'
       performing get_bom_info on end of task
       exporting p_capid = 'PP01'
                 p_datuv = <fs_plaf>-paltr
                 p_emeng = <fs_plaf>-gsmng
                 p_mehrs = 'X'
                 p_mmory = '1'
                 p_mtnrv = <fs_plaf>-matnr
                 p_stlan = '1'
                p_werks = <fs_plaf>-plwrk
       tables
               p_stpox = it_stpox
       exceptions
          communication_failure = 1
          system_failure        = 2
          resource_failure      = 3.
  case sy-subrc.
    when 0.
      w_taskname = w_taskname + 1.
      w_snd_jobs = w_snd_jobs + 1.
    when 1 or 2.
      w_exc_flag = 'X'.
    when 3.
      if w_exc_flag = space.
        w_exc_flag = 'X'.
        wait until w_rcv_jobs >= w_snd_jobs up to '0.01' seconds.
      else.
        wait until w_rcv_jobs >= w_snd_jobs up to '0.01' seconds.
      endif.
      if sy-subrc eq 0.
        clear w_exc_flag.
      else.
        exit.
      endif.
  endcase.
endloop.
wait until w_rcv_jobs >= w_snd_jobs.

loop at it_stpox assigning <fs_stpox>.
  wa_comp_info-matnr = <fs_stpox>-idnrk.
  wa_comp_info-stawn = <fs_stpox>-stawn.
  wa_comp_info-menge = <fs_stpox>-mngko.
  collect wa_comp_info into it_comp_info.
endloop.
free: it_stpox.
loop at it_comp_info into wa_comp_info.
  wa_result-week = w_nxt_week+4(2).
  wa_result-matnr = wa_comp_info-matnr.
  wa_result-stawn = wa_comp_info-stawn.
  wa_result-menge = wa_comp_info-menge.
  read table it_mat_info into wa_mat_info
                         with key matnr = wa_comp_info-matnr.
  if sy-subrc ne 0.
    clear wa_result.
    continue.
  else.
    wa_result-profl = wa_mat_info-profl.
    wa_result-maktx = wa_mat_info-maktx.
  endif.
  read table it_hts_info into wa_hts_info
                         with key stawn = wa_comp_info-stawn.
  if sy-subrc ne 0.
    wa_result-text1 = space.
    wa_result-kbetr2 = 0.
  else.
    wa_result-text1 = wa_hts_info-text1.
    wa_result-kbetr2 = wa_hts_info-kbetr / 1000.
  endif.
  read table it_mat_price into wa_mat_price
                          with key matnr = wa_comp_info-matnr.
  if sy-subrc ne 0.
    wa_result-kbetr1 = 0.
  else.
    wa_result-kbetr1 = wa_mat_price-kbetr.
  endif.
  wa_result-salk31 = wa_result-menge * wa_result-kbetr1.
  wa_result-salk32 = wa_result-menge * wa_result-kbetr2.
  wa_result-salk33 = wa_result-salk31 + wa_result-salk32.
  append wa_result to it_result.
  clear wa_result.
endloop.

sort it_result by profl stawn matnr .
describe table it_result lines w_lines.
if w_lines eq 0.
else.
  call screen '9000'.
endif.
*&---------------------------------------------------------------------*
*&      Form  get_bom_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM get_bom_info using w_taskname.

  data: it_stpox1 like table of wa_stpox.

  receive results from function 'Z_FFTZ_EXP_BOM'
                       tables p_stpox = it_stpox1
                       exceptions
                        communication_failure = 1
                        system_failure        = 2.
  if sy-subrc ne 0.
    w_exc_flag = 'X'.
    exit.
  endif.
  w_rcv_jobs = w_rcv_jobs + 1.

  delete it_stpox1 where mtart in r_mtart.
  append lines of it_stpox1 to it_stpox.
*  wa_stpox-pmatnr = <fs_plaf>-matnr.
*  modify it_stpox from wa_stpox transporting pmatnr
*                  where pmatnr is initial.
ENDFORM.                    " get_bom_info
*&---------------------------------------------------------------------*
*&      Form  get_material_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_material_price.
  data: begin of wa_a018,
          matnr like a018-matnr,
          lifnr like a018-lifnr,
          knumh like a018-knumh,
        end of wa_a018.
  data: begin of wa_konp,
          knumh like konp-knumh,
          kbetr like konp-kbetr,
          kpein like konp-kpein,
       end of wa_konp.
  data: begin of wa_eord,
          matnr like eord-matnr,
          lifnr like eord-lifnr,
        end of wa_eord.
  data: it_a018 like table of wa_a018,
        it_a0181 like table of wa_a018,
        it_konp like table of wa_konp,
        it_eord like table of wa_eord.

  data: w_idx type i,
        w_kbetr type p decimals 3.

  field-symbols: <fs_a018> like line of it_a018.

  select matnr lifnr from eord
               into table it_eord
               for all entries in it_mat_info
               where matnr = it_mat_info-matnr
               and   flifn = 'X'
               and   vdatu <= sy-datum
               and   bdatu >= sy-datum.
  if sy-subrc ne 0.
    message id 'ZMMM' type 'I' number '999' with text-006.
    exit.
  endif.
  select matnr lifnr knumh from a018
                     into table it_a018
                     for all entries in it_mat_info
                     where matnr = it_mat_info-matnr
                     and   kappl = 'M'
                     and   kschl = 'PB00'
                     and datab <= sy-datum
                     and datbi >= sy-datum.
  if sy-subrc ne 0.
    message id 'ZMMM' type 'I' number '999' with text-007.
    exit.
  endif.
  loop at it_a018 assigning <fs_a018>.
    read table it_a0181 with key matnr = <fs_a018>-matnr
                        transporting no fields.
    if sy-subrc eq 0.
      continue.
    endif.
    read table it_eord with key matnr = <fs_a018>-matnr
                                lifnr = <fs_a018>-lifnr
                                transporting no fields.
    if sy-subrc ne 0.
      read table it_mat_info into wa_mat_info
                             with key matnr = <fs_a018>-matnr
                             transporting kordb.
      if wa_mat_info-kordb eq 'X'.
        continue.
      else.
        append <fs_a018> to it_a0181.
      endif.
    else.
      append <fs_a018> to it_a0181.
    endif.
  endloop.

  refresh it_a018.
  it_a018[] = it_a0181.
  free it_a0181.

  select knumh kbetr kpein from konp
                           into table it_konp
                           for all entries in it_a018
                           where knumh = it_a018-knumh
                           and   kschl = 'PB00'
                           and   loevm_ko = space.
  if sy-subrc ne 0.
    message id 'ZMMM' type 'I' number '999' with text-008.
    exit.
  endif.
  sort: it_a018 by knumh,
        it_konp by knumh.

  w_idx = 1.
  loop at it_a018 into wa_a018.
    read table it_konp into wa_konp index w_idx.
    if sy-subrc ne 0.
      exit.
    endif.
    if wa_a018-knumh = wa_konp-knumh.
      if wa_konp-kpein = 0.
        w_kbetr = wa_konp-kbetr.
      else.
        w_kbetr = wa_konp-kbetr / wa_konp-kpein.
      endif.
      wa_mat_price-matnr = wa_a018-matnr.
      wa_mat_price-kbetr = w_kbetr.
      append wa_mat_price to it_mat_price.
      w_idx = w_idx + 1.
    endif.
    clear w_kbetr.
  endloop.

ENDFORM.                    " get_material_price
*&---------------------------------------------------------------------*
*&      Module  STATUS_0900  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0900 OUTPUT.

  SET PF-STATUS 'PF-101'.
  SET TITLEBAR '100'.

  if w_custom_container is initial.
    create object w_custom_container
      exporting
        container_name = 'ZCUSTCON'.

    create object w_grid
      exporting
        i_parent = w_custom_container.

    perform mask_columns tables it_fldclog.

    call method w_grid->set_table_for_first_display
         exporting
            i_structure_name = 'WA_RESULT'
            i_save           = 'A'
            is_layout        = w_layo
          changing
            it_outtab        = it_result
            it_fieldcatalog  = it_fldclog[]
            it_sort          = it_srt[].
    call method w_grid->set_toolbar_interactive.
  endif.
ENDMODULE.                 " STATUS_0900  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  mask_columns
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FLDCLOG  text
*----------------------------------------------------------------------*
FORM mask_columns tables p_fldclog type lvc_t_fcat.

  data: wa_fldclog like lvc_s_fcat,
        wa_sort    like lvc_s_sort.

*&------------Field catalog.
  wa_fldclog-fieldname = 'WEEK'.
  wa_fldclog-inttype   = 'N'.
  wa_fldclog-outputlen = '2'.
  wa_fldclog-coltext   = 'Week'.
  wa_fldclog-seltext   = 'Week'.
  wa_fldclog-lzero     = 'X'.
  append wa_fldclog to p_fldclog.

  clear wa_fldclog.
  wa_fldclog-fieldname = 'PROFL'.
  wa_fldclog-inttype = 'C'.
  wa_fldclog-outputlen = '1'.
  wa_fldclog-coltext   = 'Source'.
  wa_fldclog-seltext   = 'Source'.
  append wa_fldclog to p_fldclog.

  clear wa_fldclog.
  wa_fldclog-fieldname = 'MATNR'.
  wa_fldclog-inttype = 'C'.
  wa_fldclog-outputlen = '18'.
  wa_fldclog-coltext   = 'Component'.
  wa_fldclog-seltext   = 'Comp'.
  append wa_fldclog to p_fldclog.

  clear wa_fldclog.
  wa_fldclog-fieldname = 'MAKTX'.
  wa_fldclog-inttype = 'C'.
  wa_fldclog-outputlen = '40'.
  wa_fldclog-coltext   = 'Component Description'.
  wa_fldclog-seltext   = 'Comp. Desc.'.
  append wa_fldclog to p_fldclog.

  clear wa_fldclog.
  wa_fldclog-fieldname = 'STAWN'.
  wa_fldclog-inttype   = 'C'.
  wa_fldclog-outputlen = '17'.
  wa_fldclog-coltext   = 'HTS'.
  wa_fldclog-seltext   = 'HTS'.
  append wa_fldclog to p_fldclog.

  clear wa_fldclog.
  wa_fldclog-fieldname = 'TEXT1'.
  wa_fldclog-inttype = 'C'.
  wa_fldclog-outputlen = '40'.
  wa_fldclog-coltext   = 'HTS Description'.
  wa_fldclog-seltext   = 'HTS. Desc.'.
  append wa_fldclog to p_fldclog.

  clear wa_fldclog.
  wa_fldclog-fieldname = 'MENGE'.
  wa_fldclog-inttype   = 'F'.
  wa_fldclog-outputlen = '13'.
  wa_fldclog-coltext   = 'Comp.Quantity'.
  wa_fldclog-seltext   = 'Comp. Qty.'.
  wa_fldclog-do_sum     = 'X'.
  wa_fldclog-decimals_o = 2.
  append wa_fldclog to p_fldclog.

  clear wa_fldclog.
  wa_fldclog-fieldname = 'KBETR1'.
  wa_fldclog-inttype   = 'F'.
  wa_fldclog-outputlen = '13'.
  wa_fldclog-coltext   = 'Comp. Price'.
  wa_fldclog-seltext   = 'Comp. Price.'.
  wa_fldclog-decimals_o = 3.
  append wa_fldclog to p_fldclog.

  clear wa_fldclog.
  wa_fldclog-fieldname = 'KBETR2'.
  wa_fldclog-inttype   = 'F'.
  wa_fldclog-outputlen = '13'.
  wa_fldclog-coltext   = 'Duty Rate'.
  wa_fldclog-seltext   = 'Duty Rate'.
  wa_fldclog-decimals_o = 3.
  append wa_fldclog to p_fldclog.

  clear wa_fldclog.
  wa_fldclog-fieldname = 'SALK31'.
  wa_fldclog-inttype   = 'F'.
  wa_fldclog-outputlen = '13'.
  wa_fldclog-coltext   = 'Comp. Value'.
  wa_fldclog-seltext   = 'Comp. Value.'.
  wa_fldclog-do_sum     = 'X'.
  append wa_fldclog to p_fldclog.

  clear wa_fldclog.
  wa_fldclog-fieldname = 'SALK32'.
  wa_fldclog-inttype   = 'F'.
  wa_fldclog-outputlen = '13'.
  wa_fldclog-coltext   = 'Duty Value'.
  wa_fldclog-seltext   = 'Duty Value.'.
  wa_fldclog-do_sum     = 'X'.
  append wa_fldclog to p_fldclog.

  clear wa_fldclog.
  wa_fldclog-fieldname = 'SALK33'.
  wa_fldclog-inttype   = 'F'.
  wa_fldclog-outputlen = '13'.
  wa_fldclog-coltext   = 'Total Value'.
  wa_fldclog-seltext   = 'Total Value.'.
  wa_fldclog-do_sum    = 'X'.
  append wa_fldclog to p_fldclog.
*&-----Layout.
  w_layo-cwidth_opt = 'X'.
*&-----Sort.
  wa_sort-spos      = '01'.
  wa_sort-fieldname = 'PROFL'.
  wa_sort-up        = 'X'.
  wa_sort-group     = 'UL'.
  append wa_sort to it_srt.

  clear wa_sort.
  wa_sort-spos      = '02'.
  wa_sort-fieldname = 'STAWN'.
  wa_sort-up        = 'X'.
  wa_sort-subtot    = 'X'.
  append wa_sort to it_srt.

  clear wa_sort.
  wa_sort-spos      = '03'.
  wa_sort-fieldname = 'MATNR'.
  wa_sort-up        = 'X'.
  append wa_sort to it_srt.

ENDFORM.                    " mask_columns
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0900  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0900 INPUT.
  data: w_okcode(20) type c.

  w_okcode = ok_code.
  clear ok_code.

  case w_okcode.
    when 'BACK' or 'EXIT'.
      set screen 0.
    when 'CANCEL'.
      leave program.
  endcase.
ENDMODULE.                 " USER_COMMAND_0900  INPUT
