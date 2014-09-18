function z_fpp_bf_pre_parallel.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(LP_BFST) LIKE  ZTPP_BFST STRUCTURE  ZTPP_BFST
*"     VALUE(LP_NUM) LIKE  ZTPP_BFST-BFP01_FLG
*"     VALUE(LP_BUDAT) LIKE  SY-DATUM
*"  TABLES
*"      RE_BFST STRUCTURE  ZTPP_BFST
*"      RE_BFST_R STRUCTURE  ZTPP_BFST
*"  EXCEPTIONS
*"      COMMUNICATION_FAILURE
*"      SYSTEM_FAILURE
*"      RESOURCE_FAILURE
*"----------------------------------------------------------------------
  clear it_bfst[].
  it_bfst = lp_bfst.
  p_budat = lp_budat.
  append it_bfst.
  perform get_bf_objects using wt_bfst
                               lp_num.

  perform write_list tables  re_bfst re_bfst_r
                      using   lp_num
                              wt_bfst.
endfunction.
*&---------------------------------------------------------------------*
*&      Form  get_bf_objects
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_bf_objects using  wt_bfst structure ztpp_bfst
                           ls_num.
*Get data : BackFlush status table ZTPP_BFST
  describe table it_bfst lines w_int.
  if w_int <> 0.
    perform gathering_components tables it_bfst
                                 using wt_bfst
                                       ls_num.
  else.
    message i005. "WITH text-002.
    exit.
  endif.
endform.                    " get_bf_objects
*&---------------------------------------------------------------------*
*&      Form  write_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form write_list tables  re_bfst structure ztpp_bfst
                        re_bfst_r structure ztpp_bfst
                using   lp_num
                        wt_bfst structure ztpp_bfst.
  data : w_int2 type i.

  if lp_num eq '09' and wt_bfst-plant ne space.
    move-corresponding wt_bfst to re_bfst_r.
    read table re_bfst_r with key plant = wt_bfst-plant
                                  model = wt_bfst-model
                                  body_ser = wt_bfst-body_ser
                                  plan_ord = wt_bfst-plan_ord.
    if sy-subrc <> 0.
      append re_bfst_r.
    else.
      modify table re_bfst_r from re_bfst_r.
    endif.
  elseif lp_num eq '18' ."or lp_num EQ space.
    read table it_bfst index 1.
    move-corresponding it_bfst to re_bfst.
    read table re_bfst with key plant = it_bfst-plant
                                model = it_bfst-model
                                body_ser = it_bfst-body_ser
                                plan_ord = it_bfst-plan_ord.
    if sy-subrc = 0.
      modify table re_bfst from re_bfst.
    else.
      append re_bfst.
    endif.
  elseif lp_num eq space.
    refresh re_bfst.
    read table it_bfst index 1.
    move-corresponding it_bfst to re_bfst.
    append re_bfst.

  endif.

*Parallel processing : Trigger SD GOOD Issue program :
*by swkim 02.14.2004
  if  not p_d is initial.
    perform sd_trigger_event.
  endif.
endform.                    " write_list
*&---------------------------------------------------------------------*
*&      Form  gathering_components
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_BFST  text
*----------------------------------------------------------------------*
form gathering_components tables lp_bfst structure it_bfst
                           using  wp_bfst structure it_bfst
                                 lp_num.
  data :w_int2 type i,
         tin(2) type n,
        wa_bfst like lp_bfst,
        z_field(17),z_field1(17),z_field2(9).


  field-symbols : <field> type any,
                  <field1> type any,
                  <field2> type any,
                  <date> type any,
                  <time> type any.

  clear : it_plpo[],wa_bfst.

  loop at lp_bfst.
*Accorting to valus of each field.
    if lp_num = space.
      perform collect_bfst using lp_bfst  wp_bfst lp_num.
      modify lp_bfst from lp_bfst.
      modify ztpp_bfst from lp_bfst.

    elseif lp_num = '09'.
      move lp_bfst to wa_bfst.
      perform collect_bfst using lp_bfst  wp_bfst lp_num.
      if r_flg = 'Y'.
        move lp_bfst  to wp_bfst.
        tin = 1.
        do 18 times.
          concatenate 'WA_BFST' '-' 'BFP' tin '_FLG' into z_field.
          assign (z_field) to <field>.
          concatenate 'WP_BFST' '-' 'BFP' tin '_FLG' into z_field1.
          assign (z_field1) to <field1>.

         if <field> = '09' and  ( <field1> = '01' or <field1> = '00' or
                                                  <field1> = 'ER' ).
*            MOVE  <field1> TO  <field>.
            perform update_ztpp_bfst using tin wp_bfst.
          endif.
          tin = tin + 1.
        enddo.
      endif.

    elseif lp_num = '18'.
      perform collect_bfst using lp_bfst  wp_bfst lp_num.
      modify lp_bfst from lp_bfst.

      update ztpp_bfst set bfp18_flg  = lp_bfst-bfp18_flg
                           bfp18_dat  = lp_bfst-bfp18_dat
                           bfp18_tim  = lp_bfst-bfp18_tim
                           fin_bf_flg = lp_bfst-fin_bf_flg
**Issue Num : PP-20040714-112 Request by bhkim  : 2004.07.08
**Start
*                           ernam      = lp_bfst-ernam
**end
                   where   plant      = lp_bfst-plant
                       and model      = lp_bfst-model
                       and body_ser   = lp_bfst-body_ser
                       and plan_ord   = lp_bfst-plan_ord.

    endif.
  endloop.

*Update at Vehicle master table : request by hur(01/27/2004)
  data : l_dtime(14).clear l_dtime.
  loop at lp_bfst where bfp18_flg = 'Y'
                    and fin_bf_flg = 'Y'.
    concatenate  lp_bfst-bfp18_dat lp_bfst-bfp18_tim
                    into l_dtime.
    perform update_vehicle_master using 'P_FINAL_BF_DATE'
                                        lp_bfst-vin_num
                                        l_dtime.
    clear : lp_bfst,l_dtime.
  endloop.

*Create log file
*  PERFORM create_interface_log TABLES lp_bfst.

endform.                    " gathering_components
*&---------------------------------------------------------------------*
*&      Form  input_di_backflush
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_BFST  text
*----------------------------------------------------------------------*
** Changed by Furong on 06/08/2006, Requested by Mr. Hur
*FORM input_di_backflush TABLES lt_act STRUCTURE zppc_act_conf
*                        USING ls_ppc_comp_conf TYPE
*                                      zppc_comp_conf
*                              ls_mode TYPE c
*                              del_flag TYPE c
*                        CHANGING l_plag.

form input_di_backflush tables lt_act structure zppc_act_conf
                        using ls_ppc_comp_conf type
                                      zppc_comp_conf
                              ls_mode type c
                              del_flag type c
                              pp_bfst  structure it_bfst
                        changing l_plag.
** end of change
  tables: ztpp_bfst_log.
  data: ls_bfst_log like ztpp_bfst_log.

  if ls_mode = 'W'.        "Components and Activites
    call function 'ZPPC1TP_COMP_CONF_DATA_WRITE'
         exporting
              is_ppc_comp_conf  = ls_ppc_comp_conf
              if_skipsync       = ' '
              if_compsync       = ' '
         tables
*              it_conf_mats      = lt_mat
              it_conf_acts      = lt_act
         exceptions
              order_error       = 1
              line_error        = 2
              bapi_error        = 3
              duplicate_posting = 4
              others            = 5.

    if sy-subrc is initial.
      message s008(ppc1pr).
      l_plag = 'Y'.
    else.
      message id sy-msgid type 'I' number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      l_plag = 'EB'.

** Changed by Furong on 06/08/2006, requested by Mr. Hur
      ls_bfst_log-plant = pp_bfst-plant.
      ls_bfst_log-model = pp_bfst-model.
      ls_bfst_log-body_ser = pp_bfst-body_ser.
      ls_bfst_log-plan_ord = pp_bfst-plan_ord.
      ls_bfst_log-bfp_flg = l_plag.
      ls_bfst_log-cr_date = sy-datum.
      ls_bfst_log-cr_time = sy-uzeit.

      call function 'MESSAGE_TEXT_BUILD'
           exporting
                msgid               = sy-msgid
                msgnr               = sy-msgno
                msgv1               = sy-msgv1
                msgv2               = sy-msgv2
                msgv3               = sy-msgv3
                msgv4               = sy-msgv4
           importing
                message_text_output = ls_bfst_log-message.
      insert into ztpp_bfst_log values ls_bfst_log.
      if sy-subrc = 0.
        commit work.
      else.
        rollback work.
      endif.
** end of change
    endif.

  elseif ls_mode eq 'R'.             "Reverse

    call function 'ZPPC1TP_COMP_CONF_DATA_REVERSE'
         exporting
              is_ppc_comp_conf = ls_ppc_comp_conf
              if_skipsync      = ' '
         tables
*              it_conf_mats     = lt_mat
              it_conf_acts     = lt_act
         exceptions
              order_error      = 1
              line_error       = 2
              bapi_error       = 3
              run_ppcgo        = 4
              others           = 5.
    if sy-subrc is initial.
      message s008(ppc1pr).
      l_plag = '01'.
*In Case that Plan order is Deleted
      if not del_flag is initial.
        l_plag = '00'.
      endif.
    else.
      message id sy-msgid type 'I' number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      l_plag = 'ER'.
** Changed by Furong on 06/08/2006, requested by Mr. Hur
      ls_bfst_log-plant = pp_bfst-plant.
      ls_bfst_log-model = pp_bfst-model.
      ls_bfst_log-body_ser = pp_bfst-body_ser.
      ls_bfst_log-plan_ord = pp_bfst-plan_ord.
      ls_bfst_log-bfp_flg = l_plag.
      ls_bfst_log-cr_date = sy-datum.
      ls_bfst_log-cr_time = sy-uzeit.

      call function 'MESSAGE_TEXT_BUILD'
           exporting
                msgid               = sy-msgid
                msgnr               = sy-msgno
                msgv1               = sy-msgv1
                msgv2               = sy-msgv2
                msgv3               = sy-msgv3
                msgv4               = sy-msgv4
           importing
                message_text_output = ls_bfst_log-message.
      insert into ztpp_bfst_log values ls_bfst_log.
      if sy-subrc = 0.
        commit work.
      else.
        rollback work.
      endif.
** end of change
    endif.
  endif.
endform.                    " input_di_backflush
*&---------------------------------------------------------------------*
*&      Form  collect_bfst
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LP_BFST  text
*----------------------------------------------------------------------*
form collect_bfst using lpp_bfst structure it_bfst
                        wpp_bfst structure it_bfst
                        lp18.
*Check
  data : z_field(18),z_dat(18),z_tim(18).
  field-symbols : <field> type any,
                  <date> type any,
                  <time> type any.
*Issue Num : PP-20040714-001 Request by bhkim  : 2004.07.08
*Reverse backflush locking Problem
*Step : Reversal(Sequential) -> Normal case(Parallel processing)
*First step
*Check backflush status : In case of reversal
  if lp18 eq '09'.
    if lpp_bfst-bfp18_flg eq '09'.
*DI Backflush
      concatenate 'LPP_BFST' '-' 'BFP' '18' '_FLG' into z_field.
      assign (z_field) to <field>.
*Conversion characteristic
      perform conversion_characteristic using '18'
                                         changing p_char.

      perform collect_di using lpp_bfst p_char '18' 'R'
                           changing p_flg.

      assign (z_field) to <field>.
      move p_flg to <field>.

      concatenate 'LPP_BFST' '-' 'BFP' '18' '_DAT' into z_dat.
      assign (z_dat) to <date>.
      move sy-datum to  <date>.

      concatenate 'LPP_BFST' '-' 'BFP' '18' '_TIM' into z_tim.
      assign (z_tim) to <time>.
      move sy-timlo  to <time>.
      if p_flg = '01' or  p_flg eq 'ER'  or p_flg eq '00'.
        r_flg = 'Y'.
      else.
        clear r_flg.
      endif.
    endif.
    clear num.
    num = '01'.
    do 17 times.
      concatenate 'LPP_BFST' '-' 'BFP' num '_FLG' into z_field.
      assign (z_field) to <field>.
      if <field> eq '09'.  " Reversal
*Conversion characteristic
        perform conversion_characteristic using num
                                          changing p_char.
*DI Backflush
        perform collect_di using lpp_bfst p_char num 'R'
                             changing p_flg.
        move p_flg to <field>.
        concatenate 'LPP_BFST' '-' 'BFP' num '_DAT' into z_dat.
        assign (z_dat) to <date>.
        move sy-datum to  <date>.

        concatenate 'LPP_BFST' '-' 'BFP' num '_TIM' into z_tim.
        assign (z_tim) to <time>.
        move sy-timlo  to <time>.
        if p_flg = '01' or  p_flg eq 'ER'  or p_flg eq '00'.
          r_flg = 'Y'.
        else.
          clear r_flg.
        endif.
      endif.
      num = num + 1.
    enddo.
  endif.
*Second step
*CHECK : status of Each filed
  if lp18 eq '18'.
    concatenate 'LPP_BFST' '-' 'BFP' lp18 '_FLG' into z_field.
    assign (z_field) to <field>.
*Conversion characteristic
    perform conversion_characteristic using lp18
                                      changing p_char.
*check status
    perform check_components_status using p_char lpp_bfst lp18.

    if not ausp-atflv is initial.
*DI Backflush
      perform collect_di using lpp_bfst p_char lp18 'W'
                           changing p_flg.
      move p_flg to <field>.
      if p_flg eq 'Y'.
        move 'Y'   to lpp_bfst-fin_bf_flg.
      endif.

      concatenate 'LPP_BFST' '-' 'BFP' lp18 '_DAT' into z_dat.
      assign (z_dat) to <date>.
      move sy-datum to  <date>.

      concatenate 'LPP_BFST' '-' 'BFP' lp18 '_TIM' into z_tim.
      assign (z_tim) to <time>.
      move sy-timlo  to <time>.

    endif.
  endif.
*Parallel Processing : except sign off point and reversal case
  if lp18 eq space.
    clear num.
    num = '01'.
    do 17 times.
      concatenate 'LPP_BFST' '-' 'BFP' num '_FLG' into z_field.
      assign (z_field) to <field>.
      if num ne '18' and <field> eq '01'.
*Issue # 20050131-005 BIP/BIW Backflush point logic Modification
*----Start
*BIW : 'XX'- For point 2-read sign-off(point18) shopdate from vm
*BIP : 'XY'- For point 6-read sign-off(point18) shopdate from vm
        data : bi_num like num.
*Conversion char : BIP BIW
        perform conversion_biwbip using num lpp_bfst-vin_num
                                  changing bi_num.
        if bi_num ne 0.
*Conversion characteristic :change rp point->18
          perform conversion_characteristic using bi_num
                                             changing p_char.
*check status
          perform check_components_status using p_char lpp_bfst bi_num.
*Conversion characteristic :original rp point
          perform conversion_characteristic using num
                                             changing p_char.
        else.
*----End
*Conversion characteristic
          perform conversion_characteristic using num
                                             changing p_char.
*check status
          perform check_components_status using p_char lpp_bfst num.
*-----Start
        endif.
*-----End
        if not ausp-atflv is initial.
*DI Backflush
          perform collect_di using lpp_bfst p_char num 'W'
                               changing p_flg.
          assign (z_field) to <field>.
          move p_flg to <field>.

          concatenate 'LPP_BFST' '-' 'BFP' num '_DAT' into z_dat.
          assign (z_dat) to <date>.
          move sy-datum to  <date>.

          concatenate 'LPP_BFST' '-' 'BFP' num '_TIM' into z_tim.
          assign (z_tim) to <time>.
          move sy-timlo  to <time>.
        endif.
      endif.
      if num = 17.
        if lpp_bfst-ernam eq zval1.
          move zval2 to lpp_bfst-ernam.
        endif.
      endif.
      num = num + 1.
*      ELSEIF <field> EQ '09'.  " Reversal
**Conversion characteristic
*        PERFORM conversion_characteristic USING num
*                                          CHANGING p_char.
**DI Backflush
*        PERFORM collect_di USING lpp_bfst p_char num 'R'
*                             CHANGING p_flg.
*        MOVE p_flg TO <field>.
*        CONCATENATE 'LPP_BFST' '-' 'BFP' num '_DAT' INTO z_dat.
*        ASSIGN (z_dat) TO <date>.
*        MOVE sy-datum TO  <date>.
*
*        CONCATENATE 'LPP_BFST' '-' 'BFP' num '_TIM' INTO z_tim.
*        ASSIGN (z_tim) TO <time>.
*        MOVE sy-timlo  TO <time>.
*        num = num + 1.
*      ELSE.
*        EXIT.
**        num = num + 1.
*      ENDIF.
    enddo.
  endif.
*DELETE CHECK
  if not lpp_bfst-plan_del_flg is initial.
    clear z_num.
    z_num = '01'.
    do 18 times.
      concatenate 'LPP_BFST' '-' 'BFP' num '_FLG' into z_field.
      assign (z_field) to <field>.
      if  <field> eq '00'.
        z_num = z_num + 1.
      endif.
      if z_num = 18.
*        MOVE 'Y'   TO lpp_bfst-fin_bf_flg.
        concatenate 'LPP_BFST' '-' 'BFP' z_num '_DAT' into z_dat.
        assign (z_dat) to <date>.
        move sy-datum to  <date>.
        concatenate 'LPP_BFST' '-' 'BFP' z_num '_TIM' into z_tim.
        assign (z_tim) to <time>.
        move sy-timlo  to <time>.
      endif.
    enddo.
  endif.
endform.                    " collect_bfst
*&---------------------------------------------------------------------*
*&      Form  collect_resb
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LPP_BFST  text
*      -->P_0198   text
*      -->P_0199   text
*      <--P_P_FLG  text
*----------------------------------------------------------------------*
form collect_di using    pp_bfst  structure it_bfst
                           pp_char  pp_num p_mode
                  changing  p_flag.

*Collect components
  perform collect_components_move using pp_char pp_bfst
                                        pp_num p_mode
                             changing lt_ppc_comp_conf
                                      g_sortb.
*Collect Activities & IPPE
  perform collect_activites tables lt_act
                            using pp_bfst pp_num g_sortb.

*Call input DI Backflush bapi function
**changed by Furong on 06/08/2006, Requested by Mr. Hur
*  PERFORM input_di_backflush  TABLES lt_act
*                              USING lt_ppc_comp_conf p_mode
*                                    pp_bfst-plan_del_flg
*                              CHANGING p_flag.

  perform input_di_backflush  tables lt_act
                              using lt_ppc_comp_conf p_mode
                                    pp_bfst-plan_del_flg
                                    pp_bfst
                              changing p_flag.
** end of change

endform.                    " collect_resb
*&---------------------------------------------------------------------*
*&      Form  conversion_characteristic
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NUM  text
*----------------------------------------------------------------------*
form conversion_characteristic using p_num
                               changing p_char.
  concatenate 'P_RP' p_num '_SHOP_DATE' into p_char.

endform.                    " conversion_characteristic
*&---------------------------------------------------------------------*
*&      Form  collect_components
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PP_CHAR  text
*      -->P_PP_BFST  text
*----------------------------------------------------------------------*
form check_components_status using lp_char
                              lp_bfst structure it_bfst
                              lp_num.
  clear l_atinn.
*Read vehicle master status
  perform get_vehicle_number using lp_char
                                   lp_bfst-vin_num.

endform.                    " collect_components
*&---------------------------------------------------------------------*
*&      Form  collect_activites
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PP_NUM  text
*----------------------------------------------------------------------*
form collect_activites   tables ls_act structure it_act
                         using  lp_bfst structure it_bfst
                              ls_num f_sortb.

*read routing : work center & value
  perform routing_workcenter tables ls_act
                             using lp_bfst
                                   ls_num f_sortb
                             changing p_flg.

endform.                    " collect_activites
*&---------------------------------------------------------------------*
*&      Form  routing_workcenter
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LP_BFST  text
*      -->P_LS_NUM  text
*      <--P_P_FLG  text
*----------------------------------------------------------------------*
form routing_workcenter tables ls_act structure it_act
                       using    lpp_bfst structure it_bfst
                                 cc g_sortb
                        changing pp_flg type c.

  data : l_plnnr like mapl-plnnr,
         l_plnal like mapl-plnal.

  clear : it_routing[],l_matnr,l_verid,l_dispo,l_plnnr,l_plnal.

*FIXME ; posting date issue - Andy

  select single  matnr verid dispo into (l_matnr,l_verid,l_dispo)
       from plaf
        where plnum eq lpp_bfst-plan_ord.

  select single  plnng alnag into (l_plnnr,l_plnal)
       from mkal
        where matnr eq l_matnr
          and verid eq l_verid
          and werks eq p_plant.

  select   ma~matnr  mr~dispo pk~plnnr
           pk~plnal  pp~vornr pp~lar02 pp~vgw02 pp~vge02
           pp~lar03  pp~vgw03 pp~vge03 ch~arbpl ch~sortb ch~zgr03
    into corresponding fields of table it_routing
      from ( ( ( ( ( ( mara as ma
        inner join marc as mr on ma~matnr = mr~matnr )
        inner join mapl as mp on mr~matnr = mp~matnr )
        inner join plko as pk on mp~plnnr = pk~plnnr and
                                 mp~plnal = pk~plnal )
        inner join plas as pa on mp~plnty = pa~plnty and
                                 mp~plnnr = pa~plnnr and
                                 mp~plnal = pa~plnal )
        inner join plpo as pp on pk~plnnr = pp~plnnr and
                                 pa~plnty = pp~plnty and
                                 pa~plnnr = pp~plnnr and
                                 pa~plnkn = pp~plnkn and
                                 pa~zaehl = pp~zaehl )
        inner join crhd as ch on pp~arbid = ch~objid )

      where mp~plnty = 'R' and
            mp~loekz = ' ' and
            pa~loekz = ' ' and
            ma~matnr eq l_matnr  and
            mr~werks eq p_plant  and
            mr~dispo eq l_dispo  and
            pk~plnnr eq l_plnnr  and
            pk~plnal eq l_plnal  and
            pa~plnfl eq '000000' and
            ch~objty eq 'A' and
            ch~sortb eq g_sortb.

  sort it_routing by matnr arbpl sortb
                      ascending.

  delete adjacent duplicates from it_routing
           comparing matnr arbpl sortb vornr.

  clear : w_int,ls_act[].
  describe table it_routing lines w_int.
  if w_int <> 0.
    clear : it_routing.
*modify it_avt
    loop at it_routing.
*HMMA_MAN
*Cost center
      concatenate it_routing-arbpl '_MAN' into g_arbpl.
*IPPE
      perform get_activities_ippe using g_mname g_arbpl
                                  changing mod_id
                                           re_id.
      if it_routing-vgw03 <> 0.
**change, requirement from Andy - start
        if it_routing-zgr03 <> space.
          tables: tc31a.
          data: l_rate like tc31a-zgkal.
          clear l_rate.
          select * from tc31a
            where zgrad = it_routing-zgr03
              and datub >= lt_ppc_comp_conf-budat
              order by datub.
            l_rate = tc31a-ZGKAL.
            exit.
          endselect.
          if sy-subrc = 0 and l_rate > 0.
            it_routing-vgw03 = it_routing-vgw03 * 100 / l_rate.
          endif.
        endif.
**change, requirement from Andy - end

        move it_routing-vgw03 to ls_act-duration_var. "man value
        move it_routing-vge03 to ls_act-durunit. "man unit
        move mod_id  to ls_act-mode_guid.
        move re_id   to ls_act-resource_guid.
        append ls_act. clear ls_act.
      endif.
*HMMA_MCH
*Cost center
      concatenate it_routing-arbpl '_MCH' into g_arbpl.
*IPPE
      perform get_activities_ippe using g_cname g_arbpl
                                  changing mod_id
                                           re_id.
      if it_routing-vgw02 <> 0.
        move it_routing-vgw02 to ls_act-duration_var. "man value
        move it_routing-vge02 to ls_act-durunit. "man unit
        move mod_id  to ls_act-mode_guid.
        move re_id   to ls_act-resource_guid.
        append ls_act.clear ls_act.
      endif.
    endloop.
  endif.
endform.                    " routing_workcenter
*&---------------------------------------------------------------------*
*&      Form  get_activities_ippe
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_F_SORTB  text
*----------------------------------------------------------------------*
form get_activities_ippe using  f_pname f_arbpl
                         changing f_mode_id f_resource_id.
  clear : f_mode_id,f_resource_id.

  call function 'ZPPC1TP_GET_ACT_GUID_1'
    exporting
      activity          = f_pname
      resource          = f_arbpl
   importing
*     ACTIVITY_ID       =
      mode_id           = f_mode_id
      resource_id       = f_resource_id.


endform.                    " get_activities_ippe
*&---------------------------------------------------------------------*
*&      Form  CREATE_INTERFACE_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form create_interface_log tables lp_bfst.
  clear w_int.
  describe table lp_bfst lines w_int.
  check w_int <> 0.
*  I_ZTCA_IF_LOG-TCODE    = 'ZPPI501'.
*  I_ZTCA_IF_LOG-ZSLNO    = WA_JOB-SLNO.
*  I_ZTCA_IF_LOG-JOBCOUNT = WA_JOB-INT.
  i_ztca_if_log-total    = w_int.
*  I_ZTCA_IF_LOG-ZSUCC    = Z_SUCC.
*  I_ZTCA_IF_LOG-ERROR    = Z_TOTAL - Z_SUCC.
  i_ztca_if_log-erdat    = sy-datum. "Created on.
  i_ztca_if_log-erzet    = sy-uzeit. "Created time.
  i_ztca_if_log-ernam    = sy-uname. "Created by.

  call function 'Z_FCA_EAI_INTERFACE_LOG'
    exporting
      i_ztca_if_log              = i_ztca_if_log
*   IMPORTING
*     E_ZTCA_IF_LOG              =
   exceptions
     update_failed              = 1
     number_range_error         = 2
     tcode_does_not_exist       = 3
     others                     = 4
            .
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

endform.                    " CREATE_INTERFACE_LOG
*&---------------------------------------------------------------------*
*&      Form  GET_VEHICLE_NUMBER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LP_BFST_PLAN_ORD  text
*----------------------------------------------------------------------*
form get_vehicle_number using ls_char  ls_vin_num.
  clear ausp.

  call function 'CONVERSION_EXIT_ATINN_INPUT'
       exporting
            input  = ls_char
       importing
            output = l_atinn.

*Compare with Vehicle master table
  select single * from ausp
        where objek  eq ls_vin_num
          and klart  eq '002'
          and atinn  eq l_atinn.


endform.                    " GET_VEHICLE_NUMBER
*&---------------------------------------------------------------------*
*&      Form  collect_components_move
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PP_CHAR  text
*      -->P_PP_BFST  text
*      -->P_PP_NUM  text
*      <--P_LT_PPC_COMP_CONF  text
*      <--P_G_SORTB  text
*----------------------------------------------------------------------*
form collect_components_move using lp_char
                              lp_bfst structure it_bfst
                              lp_num lp_mode type c
                        changing lt_ppc_comp_conf
                                 structure zppc_comp_conf
                              lp_usr01.

  data : ls_plnkn like plpo-plnkn,
         ls_sortb(10),
         f_num(2) type n,
         w_int2 type i.
  clear : l_steus,l_usr01,ls_plnkn,lt_ppc_comp_conf,ls_sortb.
*check ? node in sequence
*Reference Rate Routing
  ls_plnkn = lp_num.
  describe table it_plpo lines w_int2.
  if w_int2 = 0.
    select plnkn into corresponding fields of table it_plpo
            from plpo
             where  plnty eq 'M'
                and plnnr eq p_rp
                and werks eq p_plant
                and nvadd ne 'X'.
  endif.

  sort it_plpo by plnkn ascending.
  clear : it_plpo,f_num.
  f_num = 01.
  loop at it_plpo.
    move f_num to it_plpo-num.
    modify it_plpo from it_plpo.
    f_num = f_num + 1.
  endloop.

  read table it_plpo with  key num = ls_plnkn.

  select  single ch~sortb into ls_sortb
     from plpo as pp inner join crhd as ch
                    on pp~arbid = ch~objid
                     where pp~plnty eq 'M'
                       and pp~plnnr eq p_rp
                       and pp~nvadd ne 'X'
                       and pp~werks eq p_plant
                       and ch~objty eq 'A'
                       and pp~plnkn eq it_plpo-plnkn.

  clear : g_date, lt_ppc_comp_conf.
*conversion from fltp type to char
  perform convsersion_flat using ausp-atflv
                           changing f_date.

  lt_ppc_comp_conf-plnum  = lp_bfst-plan_ord.
  lt_ppc_comp_conf-stsor  = ls_sortb.

  if lp_mode  = 'W'.
    if p_budat is initial.
*Issue Num : PP-20040714-112 Request by bhkim  : 2004.07.08
*Start
*In case of Spec change
      if lp_bfst-ernam eq zval1.
        lt_ppc_comp_conf-budat  = sy-datum.
        lt_ppc_comp_conf-bldat  = sy-datum.
      else.
*end
*if value of posting in initial screen is not exist, use shop date
        lt_ppc_comp_conf-budat  = f_date.
        lt_ppc_comp_conf-bldat  = f_date.
      endif.
    else.
*if value of posting in initial screen is exist, use date
*of parameters
      lt_ppc_comp_conf-budat  = p_budat.
      lt_ppc_comp_conf-bldat  = p_budat.
    endif.
*In case of Reversal for reporting point : use system date instead of
*shop date
  elseif lp_mode eq 'R'.
    if p_budat is initial.
      lt_ppc_comp_conf-budat  = sy-datum.
      lt_ppc_comp_conf-bldat  = sy-datum.
    else.
      lt_ppc_comp_conf-budat  =  p_budat.
      lt_ppc_comp_conf-bldat  =  p_budat.
    endif.
  endif.

  if lp_num = '18'.
    lt_ppc_comp_conf-gr_ind = 'X'.
    lt_ppc_comp_conf-gi_ind = 'X'.
    lt_ppc_comp_conf-erfmg  = '1'.
    lt_ppc_comp_conf-erfme  = 'EA'.
  else.
    lt_ppc_comp_conf-erfmg  = '1'.
    lt_ppc_comp_conf-erfme  = 'EA'.
    lt_ppc_comp_conf-gr_ind = ' '.
    lt_ppc_comp_conf-gi_ind = 'X'.
  endif.
  lp_usr01 = ls_sortb.

endform.                    " collect_components_move
*&---------------------------------------------------------------------*
*&      Form  convsersion_flat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_AUSP_ATWRT  text
*----------------------------------------------------------------------*
form convsersion_flat using    p_atflv
                      changing l_text2.

  data : l_text3(9).
  clear : l_text,l_text1,l_text2,l_text3.
  move p_atflv to l_text.

  call function 'CHAR_FLTP_CONVERSION'
       exporting
            string = l_text
       importing
            flstr  = l_text1.

  move l_text1(9) to l_text3.
  replace '.' with ' ' into l_text3.
  condense l_text3 no-gaps.
  move l_text3 to l_text2.

endform.                    " convsersion_flat
*&---------------------------------------------------------------------*
*&      Form  UPDATE_vehicle_MASTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0216   text
*      -->P_LP_BFST_VIN_NUM  text
*----------------------------------------------------------------------*
form update_vehicle_master using    p_char
                                    p_vin_num
                                    p_dtime.

  clear : val_table[],val_table.
  val_table-atnam = p_char.
  val_table-atwrt = p_dtime.
  append val_table.

  call function 'Z_FPP_HANDLING_MASTER'
       exporting
          object             = p_vin_num
          mode               = 'W'
          ctype              = '002'
*           DISPLAY            = 'D'
       tables
          val_table          = val_table
*         EXCEPTIONS
*           NO_DATA            = 1
*           ERROR_MODE         = 2
*           ERROR_OBJECT       = 3
*           OTHERS             = 4
       .

endform.                    " UPDATE_vehicle_MASTER
*&---------------------------------------------------------------------*
*&      Form  sd_trigger_event
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form sd_trigger_event.

  call function 'BP_EVENT_RAISE'
           exporting
                eventid                = 'ZESD02_01'
*                eventparm              = 'SD_DEL01'
           exceptions
                bad_eventid            = 1
                eventid_does_not_exist = 2
                eventid_missing        = 3
                raise_failed           = 4
                others                 = 99.

  case sy-subrc.
    when 0.
      message s001 with 'SD Call' 'ZESD02_01'.
    when others.
      message e001 with 'SD Call' 'ZESD02_01'.
  endcase.

endform.                    " sd_trigger_event
