*----------------------------------------------------------------------*
***INCLUDE ZISD05L_CMR_CREATE_F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
form read_data.
  refresh : g_tc_9000_itab.
  clear   : g_tc_9000_itab.

  select *
         into corresponding fields of table g_tc_9000_itab
         from ztsd_acl_l
        where zacln in s_zacln
        and   zcdst eq p_zcdst
        and   zrmfg eq p_zrmfg.
endform.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  CALL_SCREEN
*&---------------------------------------------------------------------*
form call_screen.
  describe table g_tc_9000_itab lines w_cnt.
  if w_cnt = 0.
    message i000 with text-m01.
    stop.
  endif.

  call screen 9000.
endform.                    " CALL_SCREEN
*&---------------------------------------------------------------------*
*&      Form  PROCESS_CMR
*&---------------------------------------------------------------------*
form process_cmr.
  data : w_index like sy-tabix,
         w_zacdt like sy-datum,
         w_fkdat like sy-datum.
  DATA : W_COUNT(02) TYPE N,
         W_MAT_FIELD(20),
         W_QTY_FIELD(20),
         W_CT(2) TYPE N.
  DATA : W_KSCHL_FIELD(20),
         W_KBTER_FIELD(20),
         W_KOEIN_FIELD(20),
         W_SELKZ_FIELD(20).

  clear w_cnt.
  loop at   g_tc_9000_itab
       into g_tc_9000_wa
       where flag = 'X'
       and   ( zrmfg = ' ' or zrmfg = '7' ).
    w_cnt = w_cnt + 1.
    exit.
  endloop.
  if w_cnt <> 1.
    message i000 with text-m09 '(STATUS : _,7)'.
    exit.
  endif.

*  loop at   g_tc_9000_itab
*       into g_tc_9000_wa
*       where flag = 'X'
*       and   ( zrmfg = ' ' or zrmfg = '7' )
*       and   not zacdt is initial.
    perform bdc_item.
*  endloop.

  loop at   g_tc_9000_itab
       into g_tc_9000_wa
       where flag = 'X'
       and   ( zrmfg = ' ' or zrmfg = '7' )
       and   not zacdt is initial.
    refresh : bdc_tab, mess_tab.
    clear   : bdc_tab, mess_tab.
    CLEAR   : W_COUNT .
    w_index = sy-tabix.

    select single *
           from usr01
          where bname = sy-uname.
    case usr01-datfm.
      when '1'. "DD.MM.YYYY
        w_zacdt+4(4) = g_tc_9000_wa-zacdt+0(4).
        w_zacdt+2(2) = g_tc_9000_wa-zacdt+4(2).
        w_zacdt+0(2) = g_tc_9000_wa-zacdt+6(2).
        w_fkdat+4(4) = sy-datum+0(4).
        w_fkdat+2(2) = sy-datum+4(2).
        w_fkdat+0(2) = sy-datum+6(2).
      when '2' or '3'. "MM/DD/YYYY "MM-DD-YYYY
        w_zacdt+4(4) = g_tc_9000_wa-zacdt+0(4).
        w_zacdt+0(2) = g_tc_9000_wa-zacdt+4(2).
        w_zacdt+2(2) = g_tc_9000_wa-zacdt+6(2).
        w_fkdat+4(4) = sy-datum+0(4).
        w_fkdat+0(2) = sy-datum+4(2).
        w_fkdat+2(2) = sy-datum+6(2).
    endcase.

    select single *
           from knvv
          where kunnr = g_tc_9000_wa-zcdst
          and   spart = '99'
          and   loevm = ''. "DELETION FLAG

    read table bdc_item with key zacln = g_tc_9000_wa-zacln
                                 zcdst = g_tc_9000_wa-zcdst.
    perform bdc_fill using :
            'X' 'SAPMV45A'             '0101',
            ' ' 'VBAK-AUART'           'ZWCR',
            ' ' 'VBAK-VKORG'           knvv-vkorg,
            ' ' 'VBAK-VTWEG'           '40',
            ' ' 'VBAK-SPART'           '99',
            ' ' 'BDC_OKCODE'           '/00'.

   loop at it_head  where zacln eq g_tc_9000_wa-zacln
                     and   zcdst eq g_tc_9000_wa-zcdst.

     W_COUNT = W_COUNT + 1.
     PERFORM GET_FIELD_NAME USING W_MAT_FIELD
                                  W_QTY_FIELD
                                  W_SELKZ_FIELD
                                  W_COUNT.
     perform bdc_fill using :
            'X' 'SAPMV45A'             '4001',
            ' ' 'VBKD-FKDAT'           w_fkdat,
            ' ' 'VBKD-BSTKD'           g_tc_9000_wa-zacln,
            ' ' 'VBKD-BSTDK'           w_zacdt,
            ' ' 'KUAGV-KUNNR'          g_tc_9000_wa-zcdst,
            ' ' W_MAT_FIELD            IT_HEAD-matnr, "'CLAIM ITEM',
            ' ' W_QTY_FIELD            '1',
            ' ' 'BDC_OKCODE'           '/00',
            'X' 'SAPMV45A'             '4001',
            ' ' 'BDC_OKCODE'           '=KKAU',
            'X' 'SAPMV45A'             '4002',
            ' ' 'VBAK-WAERK'           bdc_item-zpycr,
            ' ' 'BDC_OKCODE'           '/EBACK',
            'X' 'SAPMV45A'             '4001',

            ' ' W_SELKZ_FIELD          'X',
            ' ' 'BDC_OKCODE'           '=PKO1'.

    W_CT = '03'.
    loop at bdc_item where zacln eq g_tc_9000_wa-zacln
                     and   zcdst eq g_tc_9000_wa-zcdst
                     and   zmodl eq it_head-zmodl.
      PERFORM GET_FIELD_NAME_1 USING W_KSCHL_FIELD
                                     W_KBTER_FIELD
                                     W_KOEIN_FIELD
                                     W_CT.
      perform bdc_fill using :
            'X' 'SAPMV45A'             '5003',
*            ' ' 'BDC_OKCODE'           '=V69A_KOAN',
*            'X' 'SAPMV45A'             '5003',
            ' ' W_KSCHL_FIELD          bdc_item-zcon,
            ' ' W_KBTER_FIELD          bdc_item-zrmc,
*            ' ' W_KOEIN_FIELD          bdc_item-zpycr,
            ' ' 'BDC_OKCODE'           '/00'.
      W_CT = W_CT + 1.
    endloop.

    perform bdc_fill using :
            'X' 'SAPMV45A'             '5003',
            ' ' 'BDC_OKCODE'           '/EBACK'.

   endloop.


    perform bdc_fill using :
            'X' 'SAPMV45A'             '4001',
            ' ' 'BDC_OKCODE'           '=SICH'.

    data : www(1). www = 'N'.
    call transaction 'VA01' using bdc_tab mode www "'N'
                                  update 'S'
                                  messages into mess_tab.
    perform message_adjust_cmr.
    if w_result = 'S'.
      g_tc_9000_wa-zcmrn = mess_tab-msgv2.
      g_tc_9000_wa-zcmrd = sy-datum.
      g_tc_9000_wa-zrmfg = '9'.

      modify g_tc_9000_itab from g_tc_9000_wa index w_index.

      update ztsd_acl_l set : zcmrn = g_tc_9000_wa-zcmrn
                              zcmrd = g_tc_9000_wa-zcmrd
                              zrmfg = g_tc_9000_wa-zrmfg
                        where zacln = g_tc_9000_wa-zacln
                        and   zcdst = g_tc_9000_wa-zcdst.
    else. " 'E'.
      exit.
    endif.
  endloop.
endform.                    " PROCESS_CMR
*&---------------------------------------------------------------------*
*&      Form  PROCESS_CMR_CANC
*&---------------------------------------------------------------------*
form process_cmr_canc.
  data : w_index like sy-tabix.

  read table g_tc_9000_itab into g_tc_9000_wa
             with key flag = 'X'
                      zrmfg = '9'.
  if sy-subrc <> 0.
    message i000 with text-m09 '(STATUS : 9)'.
    exit.
  endif.

  loop at   g_tc_9000_itab
       into g_tc_9000_wa
       where flag = 'X'
       and   zrmfg = '9'.
    w_index = sy-tabix.

    perform conversion_exit_alpha_input using g_tc_9000_wa-zcmrn.

    clear   : order_header_inx.
    refresh : return.
    clear   : return.

    order_header_inx-updateflag = 'D'.

    call function 'BAPI_SALESORDER_CHANGE'
      exporting
        salesdocument               = g_tc_9000_wa-zcmrn
*       ORDER_HEADER_IN             =
        order_header_inx            = order_header_inx
*       SIMULATION                  = ' '
*       BEHAVE_WHEN_ERROR           = ' '
*       INT_NUMBER_ASSIGNMENT       = ' '
*       LOGIC_SWITCH                =
      tables
        return                      = return
*       ORDER_ITEM_IN               =
*       ORDER_ITEM_INX              =
*       PARTNERS                    =
*       PARTNERCHANGES              =
*       PARTNERADDRESSES            =
*       ORDER_CFGS_REF              =
*       ORDER_CFGS_INST             =
*       ORDER_CFGS_PART_OF          =
*       ORDER_CFGS_VALUE            =
*       ORDER_CFGS_BLOB             =
*       ORDER_CFGS_VK               =
*       ORDER_CFGS_REFINST          =
*       SCHEDULE_LINES              =
*       SCHEDULE_LINESX             =
*       ORDER_TEXT                  =
*       ORDER_KEYS                  =
*       CONDITIONS_IN               =
*       CONDITIONS_INX              =
*       EXTENSIONIN                 =
          .
    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait          = 'X'
      importing
        return        = return.

    perform message_adjust_cmr_canc.
    if w_result = 'S'.
      g_tc_9000_wa-zcmrn = '          '.
      g_tc_9000_wa-zcmrd = '        '.
      g_tc_9000_wa-zrmfg = ' '.

      modify g_tc_9000_itab from g_tc_9000_wa index w_index.

      update ztsd_acl_l set : zcmrn = g_tc_9000_wa-zcmrn
                              zcmrd = g_tc_9000_wa-zcmrd
                              zrmfg = g_tc_9000_wa-zrmfg
                        where zacln = g_tc_9000_wa-zacln
                        and   zcdst = g_tc_9000_wa-zcdst.
    else. " 'E'.
      exit.
    endif.
  endloop.
endform.                    " PROCESS_CMR_CANC
*&---------------------------------------------------------------------*
*&      Form  PROCESS_CM
*&---------------------------------------------------------------------*
form process_cm.
  data : w_index like sy-tabix.

  refresh : bdc_tab, mess_tab.
  clear   : bdc_tab, mess_tab.

  clear w_cnt.
  loop at   g_tc_9000_itab
       into g_tc_9000_wa
       where flag = 'X'
       and   zrmfg = '9'.
    w_cnt = w_cnt + 1.
    exit.
  endloop.
  if w_cnt <> 1.
    message i000 with text-m09 '(STATUS : 9)'.
    exit.
  endif.

  perform bdc_fill using :
          'X' 'SDBILLDL'             '1000',
          ' ' 'P_FKART-LOW'          'ZWG2',
          ' ' 'P_ALLEA'              'X',
          ' ' 'BDC_OKCODE'           '=%009'.

  loop at   g_tc_9000_itab
       into g_tc_9000_wa
       where flag = 'X'
       and   zrmfg = '9'.
    perform bdc_fill using :
            'X' 'SAPLALDB'             '3000',
            ' ' 'RSCSEL_255-SLOW_I(02)'    g_tc_9000_wa-zcmrn,
            ' ' 'BDC_OKCODE'           '=P+'.
  endloop.

** : BDC Logic modify(12/07/2011 BY KDM)
*  perform bdc_fill using :
*          'X' 'SAPLALDB'             '3000',
*          ' ' 'BDC_OKCODE'           '=ACPT',
*          'X' 'SDBILLDL'             '1000',
*          ' ' 'BDC_OKCODE'           '=ONLI',
*          'X' 'SAPLSLVC_FULLSCREEN'  '0500',
*          ' ' 'BDC_OKCODE'           '=&ALL',
*          'X' 'SAPLSLVC_FULLSCREEN'  '0500',
*          ' ' 'BDC_OKCODE'           '=SAMH',
*          'X' 'SAPMV60A'             '0104',
*          ' ' 'BDC_OKCODE'           '=SICH',
*          'X' 'SAPLSLVC_FULLSCREEN'  '0500',
*          ' ' 'BDC_OKCODE'           '=&F03',
*          'X' 'SDBILLDL'             '1000',
*          ' ' 'BDC_OKCODE'           '/EERW'.

  PERFORM bdc_fill USING :
          'X' 'SAPLALDB'             '3000',
          ' ' 'BDC_OKCODE'           '=ACPT',
          'X' 'SDBILLDL'             '1000',
          ' ' 'P_ALLEL'              'X',
          ' ' 'BDC_OKCODE'           '=ONLI',
          'X' 'SAPMSSY0'             '0120',
          ' ' 'BDC_OKCODE'           '=SAMH',
          'X' 'SAPMV60A'             '0104',
          ' ' 'BDC_OKCODE'           '=SICH',
          'X' 'SAPMSSY0'             '0120',
          ' ' 'BDC_OKCODE'           '=&F03',
          'X' 'SDBILLDL'             '1000',
          ' ' 'BDC_OKCODE'           '/EERW'.
** : BDC Logic modify(12/07/2011 BY KDM) - End

  data : www(1). www = 'N'.
  call transaction 'VF04' using bdc_tab mode www "'N'
                                update 'S'
                                messages into mess_tab.
  perform message_adjust_cm.
  if w_result = 'S'.
    loop at   g_tc_9000_itab
         into g_tc_9000_wa
         where flag = 'X'
         and   zrmfg = '9'.
      w_index = sy-tabix.

      g_tc_9000_wa-zcmno = mess_tab-msgv1.
      g_tc_9000_wa-zrmfg = 'Q'.

      modify g_tc_9000_itab from g_tc_9000_wa index w_index.

      update ztsd_acl_l set : zcmno = g_tc_9000_wa-zcmno
                              zrmfg = g_tc_9000_wa-zrmfg
                        where zacln = g_tc_9000_wa-zacln
                        and   zcdst = g_tc_9000_wa-zcdst.
    endloop.
  else. " 'E'.
    exit.
  endif.
endform.                    " PROCESS_CM
*&---------------------------------------------------------------------*
*&      Form  PROCESS_CM_ADJ
*&---------------------------------------------------------------------*
form process_cm_adj.
  data : w_index like sy-tabix,
         w_vbeln like vbak-vbeln.

  read table g_tc_9000_itab into g_tc_9000_wa
             with key flag = 'X'
                      zrmfg = 'Q'.
  if sy-subrc <> 0.
    message i000 with text-m09 '(STATUS : Q)'.
    exit.
  endif.

" BY ZCMNO
  refresh bdc_list. clear bdc_list.
  loop at   g_tc_9000_itab
       into g_tc_9000_wa
       where flag = 'X'
       and   zrmfg = 'Q'.
    bdc_list-zcmno = g_tc_9000_wa-zcmno.
    collect bdc_list. clear bdc_list.
  endloop.

  loop at bdc_list.
  refresh : bdc_tab, mess_tab.
  clear   : bdc_tab, mess_tab.

  perform bdc_fill using :
          'X' 'SAPMV45A'             '0101',
          ' ' 'VBAK-AUART'           'ZWCA',
          ' ' 'VBAK-VKORG'           '',
          ' ' 'VBAK-VTWEG'           '',
          ' ' 'VBAK-SPART'           '',
          ' ' 'BDC_OKCODE'           '=COPY',
          'X' 'SAPLV45C'             '0100',
          ' ' 'VBRK-VBELN'           bdc_list-zcmno,
          ' ' 'BDC_OKCODE'           '=RUEF',
          'X' 'SAPLV60P'             '4413',
          ' ' 'BDC_OKCODE'           '=MKLO',
          'X' 'SAPLV60P'             '4413',
          ' ' 'BDC_OKCODE'           '=POPO'.

  loop at   g_tc_9000_itab
       into g_tc_9000_wa
       where flag = 'X'
       and   zrmfg = 'Q'
       and   zcmno = bdc_list-zcmno.

    perform conversion_exit_alpha_input using g_tc_9000_wa-zcmno.
    perform conversion_exit_alpha_input using g_tc_9000_wa-zcmrn.
    clear vbrp.
    select single
           *
           from vbrp
          where vbeln = g_tc_9000_wa-zcmno
          and   aubel = g_tc_9000_wa-zcmrn.
    perform bdc_fill using :
            'X' 'SAPLV60P'             '0251',
            ' ' 'RV45A-POSNR'          vbrp-posnr,
            ' ' 'BDC_OKCODE'           '=POSI'.
  endloop.

  perform bdc_fill using :
          'X' 'SAPLV60P'             '4413',
          ' ' 'BDC_OKCODE'           '=MARK',
          'X' 'SAPLV60P'             '4413',
          ' ' 'BDC_OKCODE'           '=RUEB',
          'X' 'SAPMV45A'             '4001',
          ' ' 'BDC_OKCODE'           '=SICH'.


  data : www(1). www = 'N'.
  call transaction 'VA01' using bdc_tab mode www "'N'
                                update 'S'
                                messages into mess_tab.
  perform message_adjust_cmr.
  if w_result = 'S'.
    w_vbeln = mess_tab-msgv2.
*****************************
    refresh : bdc_tab, mess_tab.
    clear   : bdc_tab, mess_tab.

    perform bdc_fill using :
            'X' 'SDBILLDL'             '1000',
            ' ' 'P_FKART-LOW'          'ZWCA',
            ' ' 'P_ALLEA'              'X',
            ' ' 'BDC_OKCODE'           '=%009'.

    perform bdc_fill using :
            'X' 'SAPLALDB'             '3000',
            ' ' 'RSCSEL_255-SLOW_I(02)'    w_vbeln,
            ' ' 'BDC_OKCODE'           '=P+'.

** : BDC Logic modify(12/07/2011 BY KDM) - Start
*    perform bdc_fill using :
*            'X' 'SAPLALDB'             '3000',
*            ' ' 'BDC_OKCODE'           '=ACPT',
*            'X' 'SDBILLDL'             '1000',
*            ' ' 'BDC_OKCODE'           '=ONLI',
*            'X' 'SAPLSLVC_FULLSCREEN'  '0500',
*            ' ' 'BDC_OKCODE'           '=&ALL',
*            'X' 'SAPLSLVC_FULLSCREEN'  '0500',
*            ' ' 'BDC_OKCODE'           '=SAMH',
*            'X' 'SAPMV60A'             '0104',
*            ' ' 'BDC_OKCODE'           '=SICH',
*            'X' 'SAPLSLVC_FULLSCREEN'  '0500',
*            ' ' 'BDC_OKCODE'           '=&F03',
*            'X' 'SDBILLDL'             '1000',
*            ' ' 'BDC_OKCODE'           '/EERW'.

  PERFORM bdc_fill USING :
          'X' 'SAPLALDB'             '3000',
          ' ' 'BDC_OKCODE'           '=ACPT',
          'X' 'SDBILLDL'             '1000',
          ' ' 'P_ALLEL'              'X',
          ' ' 'BDC_OKCODE'           '=ONLI',
          'X' 'SAPMSSY0'             '0120',
          ' ' 'BDC_OKCODE'           '=SAMH',
          'X' 'SAPMV60A'             '0104',
          ' ' 'BDC_OKCODE'           '=SICH',
          'X' 'SAPMSSY0'             '0120',
          ' ' 'BDC_OKCODE'           '=&F03',
          'X' 'SDBILLDL'             '1000',
          ' ' 'BDC_OKCODE'           '/EERW'.
** : BDC Logic modify(12/07/2011 BY KDM) - End

    call transaction 'VF04' using bdc_tab mode www "'N'
                                  update 'S'
                                  messages into mess_tab.
    perform message_adjust_cm.
    if w_result = 'S'.
*****************************
    loop at   g_tc_9000_itab
         into g_tc_9000_wa
         where flag = 'X'
         and   zrmfg = 'Q'.
      w_index = sy-tabix.

      g_tc_9000_wa-zcmrn = '          '.
      g_tc_9000_wa-zcmrd = '        '.
      g_tc_9000_wa-zcmno = '          '.
      g_tc_9000_wa-zrmfg = '7'.

      modify g_tc_9000_itab from g_tc_9000_wa index w_index.

      update ztsd_acl_l set : zcmrn = g_tc_9000_wa-zcmrn
                              zcmrd = g_tc_9000_wa-zcmrd
                              zcmno = g_tc_9000_wa-zcmno
                              zrmfg = g_tc_9000_wa-zrmfg
                        where zacln = g_tc_9000_wa-zacln
                        and   zcdst = g_tc_9000_wa-zcdst.
    endloop.
    else. " 'E'.
      exit.
    endif.
  else. " 'E'.
    exit.
  endif.
  endloop.
endform.                    " PROCESS_CM_ADJ
*&---------------------------------------------------------------------*
*&      Form  BDC_ITEM
*&---------------------------------------------------------------------*
form bdc_item.
  data : bdc_item_idx like sy-tabix.

  refresh : bdc_item, IT_HEAD.
  clear   : bdc_item, IT_HEAD.

  loop at   g_tc_9000_itab
       into g_tc_9000_wa
       where flag = 'X'
       and   ( zrmfg = ' ' or zrmfg = '7' )
       and   not zacdt is initial.
    select *
           from ztsd_acm_h
          where zacln eq g_tc_9000_wa-zacln
          and   zcdst eq g_tc_9000_wa-zcdst
          and   ( zcsts eq 'AA' or
                  zcsts eq 'JJ' ).
      bdc_item-zacln = ztsd_acm_h-zacln.
      bdc_item-zcdst = ztsd_acm_h-zcdst.
      bdc_item-zmodl = ztsd_acm_h-zmodl.
      bdc_item-zscod = ztsd_acm_h-zscod.
      bdc_item-zpycr = ztsd_acm_h-zpycr.
      if ztsd_acm_h-zctyp = 'C'.
        bdc_item-zctyp = 'C'.
      else.
        bdc_item-zctyp = 'W'.
      endif.
      bdc_item-zrm   = bdc_item-zrm +
                 ztsd_acm_h-zrmss + ztsd_acm_h-zrmpp + ztsd_acm_h-zrmll.

      collect bdc_item. clear bdc_item.

    endselect.
  endloop.

  loop at bdc_item.
    bdc_item_idx = sy-tabix.
    if bdc_item-zctyp = 'C'.
      select single zconc matnr
                    into (bdc_item-zcon, bdc_item-matnr)
                    from ztsd_vin_conv
                   where zscod eq bdc_item-zscod
                   and   zmodl eq bdc_item-zmodl.
    else.
      select single zconw matnr
                    into (bdc_item-zcon, bdc_item-matnr)
                    from ztsd_vin_conv
                   where zscod eq bdc_item-zscod
                   and   zmodl eq bdc_item-zmodl.
    endif.
    if sy-subrc = 0.
      write bdc_item-zrm to bdc_item-zrmc currency bdc_item-zpycr.
      modify bdc_item index bdc_item_idx.

      it_head-zacln = bdc_item-zacln.
      it_head-zcdst = bdc_item-zcdst.
      it_head-zmodl = bdc_item-zmodl.
      it_head-matnr = bdc_item-matnr.
      collect it_head. clear it_head.

    endif.
  endloop.
endform.                    " BDC_ITEM
*&---------------------------------------------------------------------*
*&      Form  BDC_FILL
*&---------------------------------------------------------------------*
form bdc_fill using    p1 p2 p3.
  clear bdc_tab.
  if p1 = 'X'.
     bdc_tab-dynbegin = p1.
     bdc_tab-program  = p2.
     bdc_tab-dynpro   = p3.
  else.
     bdc_tab-dynbegin = p1.
     bdc_tab-fnam     = p2.
     bdc_tab-fval     = p3.
  endif.
  append bdc_tab.
endform.                    " BDC_FILL
*&---------------------------------------------------------------------*
*&      Form  MESSAGE_ADJUST_CMR
*&---------------------------------------------------------------------*
form message_adjust_cmr.
  clear : w_result, w_result_msg.

  read table mess_tab with key msgtyp = 'E'.
  if sy-subrc = 0.
    call function 'MESSAGE_TEXT_BUILD'
         exporting
           msgid               = mess_tab-msgid
           msgnr               = mess_tab-msgnr
           msgv1               = mess_tab-msgv1
           msgv2               = mess_tab-msgv2
           msgv3               = mess_tab-msgv3
           msgv4               = mess_tab-msgv4
         importing
           message_text_output = w_result_msg.
    w_result = 'E'.
    message i000 with w_result_msg.
  else.
    read table mess_tab with key msgtyp = 'S'
                                 msgid  = 'V1'
                                 msgnr  = '311'.
    if sy-subrc = 0.
      call function 'MESSAGE_TEXT_BUILD'
           exporting
             msgid               = mess_tab-msgid
             msgnr               = mess_tab-msgnr
             msgv1               = mess_tab-msgv1
             msgv2               = mess_tab-msgv2
             msgv3               = mess_tab-msgv3
             msgv4               = mess_tab-msgv4
           importing
             message_text_output = w_result_msg.
      w_result = 'S'.
      message s000 with w_result_msg.
    endif.
  endif.
endform.                    " MESSAGE_ADJUST_CMR
*&---------------------------------------------------------------------*
*&      Form  MESSAGE_ADJUST_CMR_CANC
*&---------------------------------------------------------------------*
form message_adjust_cmr_canc.
  clear : w_result, w_result_msg.

  read table return with key type = 'E'.
  if sy-subrc = 0.
    w_result_msg = return-message.
    w_result = 'E'.
    message i000 with w_result_msg.
  else.
    read table return with key type    = 'S'
                               id      = 'V1'
                               number  = '008'.
    if sy-subrc = 0.
      w_result_msg = return-message.
      w_result = 'S'.
      message s000 with w_result_msg.
    endif.
  endif.
endform.                    " MESSAGE_ADJUST_CMR_CANC
*&---------------------------------------------------------------------*
*&      Form  MESSAGE_ADJUST_CM
*&---------------------------------------------------------------------*
form message_adjust_cm.
  clear : w_result, w_result_msg.

  read table mess_tab with key msgtyp = 'E'.
  if sy-subrc = 0.
    call function 'MESSAGE_TEXT_BUILD'
         exporting
           msgid               = mess_tab-msgid
           msgnr               = mess_tab-msgnr
           msgv1               = mess_tab-msgv1
           msgv2               = mess_tab-msgv2
           msgv3               = mess_tab-msgv3
           msgv4               = mess_tab-msgv4
         importing
           message_text_output = w_result_msg.
    w_result = 'E'.
    message i000 with w_result_msg.
  else.
    read table mess_tab with key msgtyp = 'S'
                                 msgid  = 'VF'.
                             """ MSGNR  = '051' '050' '311'
    if sy-subrc = 0.
      call function 'MESSAGE_TEXT_BUILD'
           exporting
             msgid               = mess_tab-msgid
             msgnr               = mess_tab-msgnr
             msgv1               = mess_tab-msgv1
             msgv2               = mess_tab-msgv2
             msgv3               = mess_tab-msgv3
             msgv4               = mess_tab-msgv4
           importing
             message_text_output = w_result_msg.
      w_result = 'S'.
      message s000 with w_result_msg.
    endif.
  endif.
endform.                    " MESSAGE_ADJUST_CM
*&---------------------------------------------------------------------*
*&      Form  PROCESS_EDIT
*&---------------------------------------------------------------------*
form process_edit.
  clear w_cnt.

  loop at   g_tc_9000_itab
       into g_tc_9000_wa
       where flag = 'X'.
    w_cnt = w_cnt + 1.
  endloop.

  if w_cnt = 1.
    move-corresponding g_tc_9000_wa to ztsd_acl_l.
    call screen 9100.
  else.
    message i000 with text-m02.
  endif.
endform.                    " PROCESS_EDIT
*&---------------------------------------------------------------------*
*&      Form  PROCESS_PRIN
*&---------------------------------------------------------------------*
form process_prin.
  clear w_cnt.

  loop at   g_tc_9000_itab
       into g_tc_9000_wa
       where flag = 'X'.
    w_cnt = w_cnt + 1.
  endloop.

  if w_cnt = 1.
    move-corresponding g_tc_9000_wa to ztsd_acl_l.
    if ztsd_acl_l-zrmfg <> 'Q'.
      message i000 with text-m08
                        '( STATUS :'
                        ztsd_acl_l-zrmfg
                        ')'.
      exit.
    endif.

    perform get_acm_fax.
    perform write_acm_fax.
  else.
    message i000 with text-m02.
  endif.
endform.                    " PROCESS_PRIN
*&---------------------------------------------------------------------*
*&      Form  PROCESS_REMI
*&---------------------------------------------------------------------*
form process_remi. " CHECK PAYMENT
  clear w_cnt.

  loop at   g_tc_9000_itab
       into g_tc_9000_wa
       where flag = 'X'.
    w_cnt = w_cnt + 1.
  endloop.

  if w_cnt = 1.
    move-corresponding g_tc_9000_wa to ztsd_acl_l.
    if ztsd_acl_l-zrmfg <> 'Q'.
      message i000 with text-m08
                        '( STATUS :'
                        ztsd_acl_l-zrmfg
                        ')'.
      exit.
    endif.
    if ztsd_acl_l-zrtdt is initial.
      message i000 with text-m10.
      exit.

    endif.
    "IF "CHECK ZRTDT
    "ENDIF.

    perform get_acm_remi.
    perform write_acm_remi.
  else.
    message i000 with text-m02.
  endif.
endform.                    " PROCESS_REMI
*&---------------------------------------------------------------------*
*&      Form  GET_ACM_FAX
*&---------------------------------------------------------------------*
form get_acm_fax.
  refresh it_acm_fax. clear it_acm_fax.

  loop at   g_tc_9000_itab
       into g_tc_9000_wa
       where zcmno = ztsd_acl_l-zcmno.
    it_acm_fax-zacln = g_tc_9000_wa-zacln.
    it_acm_fax-zacdt = g_tc_9000_wa-zacdt.
    it_acm_fax-zscaa = g_tc_9000_wa-zscaa.
    it_acm_fax-zcmrn = g_tc_9000_wa-zcmrn.
    it_acm_fax-zpycr = ztsd_acl_l-zpycr.

    perform conversion_exit_alpha_input using g_tc_9000_wa-zcmrn.
    clear vbak.
    select single
           *
           from vbak
          where vbeln = g_tc_9000_wa-zcmrn.

    select *
           from konv
          where knumv = vbak-knumv.
      if konv-kschl+0(3) = 'ZWA' or konv-kschl+0(3) = 'ZCA'.
        it_acm_fax-zacaa = it_acm_fax-zacaa + konv-kbetr.
      elseif konv-kschl+0(3) = 'ZWC' or konv-kschl+0(3) = 'ZCC'.
        it_acm_fax-zcbaa = it_acm_fax-zcbaa + ( konv-kbetr * -1 ).
      endif.
    endselect.

    append it_acm_fax. clear it_acm_fax.
  endloop.
endform.                    " GET_ACM_FAX
*&---------------------------------------------------------------------*
*&      Form  GET_ACM_REMI
*&---------------------------------------------------------------------*
form get_acm_remi.
  refresh it_acm_remi. clear it_acm_remi.

  loop at   g_tc_9000_itab
       into g_tc_9000_wa
       where zrtdt = ztsd_acl_l-zrtdt.
    it_acm_remi-zacln = g_tc_9000_wa-zacln.
    it_acm_remi-zacdt = g_tc_9000_wa-zacdt.
    it_acm_remi-zcmrn = g_tc_9000_wa-zcmrn.
    it_acm_remi-zpycr = ztsd_acl_l-zpycr.

    perform conversion_exit_alpha_input using g_tc_9000_wa-zcmrn.
    clear vbak.
    select single
           *
           from vbak
          where vbeln = g_tc_9000_wa-zcmrn.

    select *
           from konv
          where knumv = vbak-knumv.
      if konv-kschl+0(3) = 'ZWA' or konv-kschl+0(3) = 'ZCA'.
        it_acm_remi-zacaa = it_acm_remi-zacaa + konv-kbetr.
      elseif konv-kschl+0(3) = 'ZWC' or konv-kschl+0(3) = 'ZCC'.
        it_acm_remi-zcbaa = it_acm_remi-zcbaa + ( konv-kbetr * -1 ).
      endif.
      it_acm_remi-zremi = it_acm_remi-zremi + konv-kbetr.
    endselect.

    append it_acm_remi. clear it_acm_remi.
  endloop.
endform.                    " GET_ACM_REMI
*&---------------------------------------------------------------------*
*&      Form  WRITE_ACM_FAX
*&---------------------------------------------------------------------*
form write_acm_fax.
  leave to list-processing.
  set pf-status 'EMAIL'.
  w_gubun = '1'. "FAX

  perform get_email_address using ztsd_acl_l-zcdst.

  clear : w_attn, w_cc.
  read table mail_addr index 1.
  if sy-subrc = 0.
    concatenate mail_addr-namev mail_addr-name1
                into w_attn separated by space.
  endif.
  read table mail_addr index 2.
  if sy-subrc = 0.
    concatenate mail_addr-namev mail_addr-name1
                into w_cc   separated by space.
  endif.

*  SELECT SINGLE * FROM KNVK WHERE PARNR = '0000000001'
*                            AND   KUNNR = ZTSD_ACL_L-ZCDST.
*  CONCATENATE KNVK-NAMEV KNVK-NAME1 INTO W_ATTN SEPARATED BY SPACE.
*
*  SELECT SINGLE * FROM KNVK WHERE PARNR = '0000000002'
*                            AND   KUNNR = ZTSD_ACL_L-ZCDST.
*  CONCATENATE KNVK-NAMEV KNVK-NAME1 INTO W_CC   SEPARATED BY SPACE.

  if ztsd_acl_l-zfxdt is initial.
    read table g_tc_9000_itab into g_tc_9000_wa
               with key zacln = ztsd_acl_l-zacln
                        zcdst = ztsd_acl_l-zcdst.
    if sy-subrc = 0.
      g_tc_9000_wa-zfxdt = sy-datum.

      modify g_tc_9000_itab from g_tc_9000_wa index sy-tabix.
    endif.

    ztsd_acl_l-zfxdt = sy-datum.

    update ztsd_acl_l set : zfxdt = ztsd_acl_l-zfxdt
                      where zacln = ztsd_acl_l-zacln
                      and   zcdst = ztsd_acl_l-zcdst.
  endif.

  skip 5.
  write:/     'Approved Credit Memo'.
  write:/     'Distributor Code :', p_zcdst,
          49  'ACM Date :', ztsd_acl_l-zfxdt.
  skip 3.
  write:/ sy-uline.
  skip 4.

  write:/(120) 'Hyundai Motor Manufacturing Alabama' centered.
  write:/(120) 'FAX : '      centered.
  write:/(120) 'OUTWARD FAX' centered,
          109  'Page 1 of 1'.

  clear kna1.
  select single
         *
         from kna1
        where kunnr = ztsd_acl_l-zcdst.

  format color col_normal.
  write:/ sy-uline.
  write:/     'REF  :',   (52) ztsd_acl_l-zcmno,
          60  'FAX NO :', (52) kna1-telfx.
  write:/     'TO   :',   (52) kna1-name1,
          60  'DATE   :', (52) ztsd_acl_l-zfxdt.
  write:/     'ATTN :',   (52) w_attn,
          60(61) ''.
  write:/     'CC   :',   (52) w_cc,
          60(61) ''.
  write:/     'FROM :',   (52) 'Hyundai Motor Manufacturing Alabama',
          60(61) ''.
  write:/ sy-uline.
  format color col_normal off.

  skip 1.

  format color col_normal.
  write:/ sy-uline.
  write:/(120) 'SUBJECT : Approved Credit Memo' centered.
  write:/ sy-uline.
  format color col_normal off.

  write:/     'Please be informed of Approved Credit Memo against your',
              'submitted claims as follows;'.
  write:/99   'Unit :', ztsd_acl_l-zpycr.

  write:/12(98) sy-uline.
  write:/1(11) '' no-gap.
  format color col_normal.
  write:   (07) 'ACL NO',
         27(12) 'Receipt Date',
         46(17) 'Submission Amount' right-justified,
         69(17) 'Approved Amount'   right-justified,
         93(17) 'Charge Back'       right-justified.
  format color col_normal off.
  write:/12(98) sy-uline.

  clear : w_zscaa, w_zacaa, w_zcbaa.
  loop at it_acm_fax.
    write:/12(07) it_acm_fax-zacln,
           27(12) it_acm_fax-zacdt,
           46(17) it_acm_fax-zscaa,
           69(17) it_acm_fax-zacaa,
           93(17) it_acm_fax-zcbaa.

    w_zscaa = w_zscaa + it_acm_fax-zscaa.
    w_zacaa = w_zacaa + it_acm_fax-zacaa.
    w_zcbaa = w_zcbaa + it_acm_fax-zcbaa.
    skip 1.
  endloop.
  describe table it_acm_fax lines w_cnt.

  if w_cnt < 20.
    w_cnt = ( 20 - w_cnt ) * 2.
    skip w_cnt.
  endif.

  write:/12(98) sy-uline.
  write:/1(11) '' no-gap.
  format color col_normal.
  write:   (07) 'Total',
         46(17) w_zscaa,
         69(17) w_zacaa,
         93(17) w_zcbaa.
  format color col_normal off.
  write:/12(98) sy-uline.

  skip 1.
  write:/'Note'.
*         123456789012345678901234567890123456789012345678901234567890
  write:/'     1. Receipt date means the date when claim data or ACL',
         'printout was received by HMMA, whichever was later, both of'.
  write:/'        which are fully proven satisfactory in all respects',
         'of the requirments of Hyundai Warranty Policy and procedures'.
  write:/'        Manual prepared by HMMA'.
  write:/'     2. If any ACL remains outstanding in HMMA though',
         'submitted 40 days ago, please contact HMMA for corrective',
         'action'.
  write:/'        within a week from today'.
  write:/'     3. Reimbursement amount will be approved amount minus',
         'charge back'.

  skip 1.
  write:/ sy-uline.

  write:/(120)
          'Copyright(c) 2001 Hyundai Motor Campany. All Rights Reserved'
          centered.


  refresh it_acm_fax_h. clear it_acm_fax_h.
  it_acm_fax_h-zcdst = ztsd_acl_l-zcdst.
  it_acm_fax_h-zcmno = ztsd_acl_l-zcmno.
  it_acm_fax_h-zfxdt = ztsd_acl_l-zfxdt.
  append it_acm_fax_h. clear it_acm_fax_h.
endform.                    " WRITE_ACM_FAX
*&---------------------------------------------------------------------*
*&      Form  WRITE_ACM_REMI
*&---------------------------------------------------------------------*
form write_acm_remi.
  leave to list-processing.
  set pf-status 'EMAIL'.
  w_gubun = '2'. "REMI

  perform get_email_address using ztsd_acl_l-zcdst.

  clear : w_attn, w_cc.
  read table mail_addr index 1.
  if sy-subrc = 0.
    concatenate mail_addr-namev mail_addr-name1
                into w_attn separated by space.
  endif.
  read table mail_addr index 2.
  if sy-subrc = 0.
    concatenate mail_addr-namev mail_addr-name1
                into w_cc   separated by space.
  endif.

*  SELECT SINGLE * FROM KNVK WHERE PARNR = '0000000001'
*                            AND   KUNNR = ZTSD_ACL_L-ZCDST.
*  CONCATENATE KNVK-NAMEV KNVK-NAME1 INTO W_ATTN SEPARATED BY SPACE.
*
*  SELECT SINGLE * FROM KNVK WHERE PARNR = '0000000002'
*                            AND   KUNNR = ZTSD_ACL_L-ZCDST.
*  CONCATENATE KNVK-NAMEV KNVK-NAME1 INTO W_CC   SEPARATED BY SPACE.

  skip 1. "5
  write:/     'Notification of Remittance'.
  write:/     'Distributor Code :', p_zcdst,
          49  'Remittance Date :', ztsd_acl_l-zrtdt.
  skip 0. "3
  write:/ sy-uline.
  skip 1. "4

  write:/(120) 'Hyundai Motor Manufacturing Alabama' centered.
  write:/(120) 'FAX : '      centered.
  write:/(120) 'OUTWARD FAX' centered,
          109  'Page 1 of 1'.

  clear kna1.
  select single
         *
         from kna1
        where kunnr = ztsd_acl_l-zcdst.

  format color col_normal.
  write:/ sy-uline.
  write:/     'REF  :',   (52) '', "ZTSD_ACL_L-ZCMNO,
          60  'FAX NO :', (52) kna1-telfx.
  write:/     'TO   :',   (52) kna1-name1,
          60  'DATE   :', (52) ztsd_acl_l-zrtdt.
  write:/     'ATTN :',   (52) w_attn,
          60(61) ''.
  write:/     'CC   :',   (52) w_cc,
          60(61) ''.
  write:/     'FROM :',   (52) 'Hyundai Motor Manufacturing Alabama',
          60(61) ''.
  write:/ sy-uline.
  format color col_normal off.

  skip 1.

  format color col_normal.
  write:/ sy-uline.
  write:/(120) 'SUBJECT : Notification of Remittance' centered.
  write:/ sy-uline.
  format color col_normal off.

  write:/  '1. Remittance'.
  write:/  '   Please note that warranty reimbursement amount was',
           'transfered to you as follows;'.
  write:/99 'Unit :', ztsd_acl_l-zpycr.

  write:/12(98) sy-uline.
  write:/1(11) '' no-gap.
  format color col_normal.
  write:   (07) 'ACL NO',
         27(12) 'Receipt Date',
         46(17) 'Approved Amount' right-justified,
         69(17) 'Charge Back'     right-justified,
         93(17) 'Remitted Amount' right-justified.
  format color col_normal off.
  write:/12(98) sy-uline.

  clear : w_zacaa, w_zcbaa, w_zremi.
  loop at it_acm_remi.
    write:/12(07) it_acm_remi-zacln,
           27(12) it_acm_remi-zacdt,
           46(17) it_acm_remi-zacaa,
           69(17) it_acm_remi-zcbaa,
           93(17) it_acm_remi-zremi.

    w_zacaa = w_zacaa + it_acm_remi-zacaa.
    w_zcbaa = w_zcbaa + it_acm_remi-zcbaa.
    w_zremi = w_zremi + it_acm_remi-zremi.
    skip 1.
  endloop.
  describe table it_acm_remi lines w_cnt.

  if w_cnt < 20.
    w_cnt = ( 20 - w_cnt ) * 2.
    skip w_cnt.
  endif.

  write:/12(98) sy-uline.
  write:/1(11) '' no-gap.
  format color col_normal.
  write:   (07) 'Total',
         46(17) w_zacaa,
         69(17) w_zcbaa,
         93(17) w_zremi.
  format color col_normal off.
  write:/12(98) sy-uline.

  skip 1.
  write:/12(17) 'Bank Account    :', ''.
  write:/12(17) 'Account Name    :', ''.
  write:/12(17) 'Account No      :', ''.
  write:/12(17) 'Remittance Date :', ztsd_acl_l-zrtdt.

  skip 1.
  write:/'Note'.
*                    12345678901234567890123456789012345678901234567
  write:/'           1. Receipt date means the date when claim data',
         'or ACL printout was received by HMMA, whichever was'.
  write:/'              later, both of which are fully proven',
         'satisfactory in all respects of the requirments of'.
  write:/'              Hyundai Warranty Policy and procedures',
         'Manual prepared by HMMA'.
  write:/'           2. If any ACL remains outstanding in HMMA though',
         'submitted 40 days ago, please contact HMMA for'.
  write:/'              corrective action within a week from today'.

  skip 1.
  write:/  '2. Receipt acknowledgement by Distributor'.
  write:/  '   Please confirm your receipt by return with the',
           'following information withen a week.'.

  skip 1.
  write:/'           Amount received   :', ''.
  write:/'           Receipt date      :', ''.
  write:/'           Receipt signature :', ''.

  skip 1.
  write:/ sy-uline.
  write:/(120)
          'Copyright(c) 2001 Hyundai Motor Campany. All Rights Reserved'
          centered.


  refresh it_acm_remi_h. clear it_acm_remi_h.
  it_acm_remi_h-zcdst = ztsd_acl_l-zcdst.
  it_acm_remi_h-zcmno = ztsd_acl_l-zcmno.
  it_acm_remi_h-zrtdt = ztsd_acl_l-zrtdt.
  append it_acm_remi_h. clear it_acm_remi_h.
endform.                    " WRITE_ACM_REMI
*&---------------------------------------------------------------------*
*&      Form  GET_EMAIL_ADDRESS
*&---------------------------------------------------------------------*
form get_email_address using p_kunnr.
  refresh mail_addr. clear mail_addr.
  call function 'Z_FSD_MAIL_ADDR'
    exporting
      kunnr                = p_kunnr
    tables
      mail_addr            = mail_addr
    exceptions
      not_found_kna1       = 1
      not_found_knvk       = 2
      others               = 3.
endform.                    " GET_EMAIL_ADDRESS
*&---------------------------------------------------------------------*
*&      Form  PROCESS_SAVE
*&---------------------------------------------------------------------*
form process_save.
* CHECK CURRENCT KEY
  if not ztsd_acl_l-zscaa is initial and
     ztsd_acl_l-zsmcr is initial.
    ztsd_acl_l-zsmcr = ztsd_acl_l-zpycr.
  endif.

  read table g_tc_9000_itab into g_tc_9000_wa
             with key flag = 'X'.
  if sy-subrc = 0.
    g_tc_9000_wa-zacdt = ztsd_acl_l-zacdt.
    g_tc_9000_wa-zscqt = ztsd_acl_l-zscqt.
    g_tc_9000_wa-zscaa = ztsd_acl_l-zscaa.
    g_tc_9000_wa-zsmcr = ztsd_acl_l-zsmcr.
    g_tc_9000_wa-zrtdt = ztsd_acl_l-zrtdt.

    modify g_tc_9000_itab from g_tc_9000_wa index sy-tabix.

    update ztsd_acl_l set : zacdt = g_tc_9000_wa-zacdt
                            zscqt = g_tc_9000_wa-zscqt
                            zscaa = g_tc_9000_wa-zscaa
                            zsmcr = g_tc_9000_wa-zsmcr
                            zrtdt = g_tc_9000_wa-zrtdt
                      where zacln = g_tc_9000_wa-zacln
                      and   zcdst = g_tc_9000_wa-zcdst.

    message i000 with text-m03.
  endif.
endform.                    " PROCESS_SAVE
*&---------------------------------------------------------------------*
*&      Form  PROCESS_DEL
*&---------------------------------------------------------------------*
form process_del.
  data : w_zacln like ztsd_acl_l-zacln,
         w_zcdst like ztsd_acl_l-zcdst.

  w_zacln = ztsd_acl_l-zacln.
  w_zcdst = ztsd_acl_l-zcdst.

  if ztsd_acl_l-zrmfg <> ' ' and
     ztsd_acl_l-zrmfg <> '7'.
    message i000 with text-m08
                      '( STATUS :'
                      ztsd_acl_l-zrmfg
                      ')'.
    exit.
  endif.

  call function 'POPUP_TO_CONFIRM_STEP'
    exporting
      defaultoption     = 'N'
      textline1         = text-m04
      titel             = text-m05
    importing
      answer            = w_answer.
  if w_answer eq 'J'.
    delete from ztsd_acm_h
          where zacln eq ztsd_acl_l-zacln
          and   zcdst eq ztsd_acl_l-zcdst.
    if sy-subrc = 0.
      if ztsd_acl_l-zctyp <> 'C'.
        delete from ztsd_acm_i
              where zacln eq ztsd_acl_l-zacln
              and   zcdst eq ztsd_acl_l-zcdst.
      endif.
      if sy-subrc = 0.
        delete from ztsd_acl_l
              where zacln eq ztsd_acl_l-zacln
              and   zcdst eq ztsd_acl_l-zcdst.
        if sy-subrc = 0.
          message i000 with text-m06.
          delete g_tc_9000_itab where zacln eq w_zacln
                                and   zcdst eq w_zcdst.
          refresh control 'TC_9000' from screen '9000'.

          set screen 0.
        else.
          message i000 with text-m07.
          rollback work.
        endif.
      else.
        message i000 with text-m07.
        rollback work.
      endif.
    else.
      message i000 with text-m07.
      rollback work.
    endif.
  endif.
endform.                    " PROCESS_DEL
*&---------------------------------------------------------------------*
*&      Form  SHOW_CLICKED_CMR_DOC
*&---------------------------------------------------------------------*
form show_clicked_cmr_doc.
  data : w_field(20),
         w_vbeln like vbak-vbeln.
  get cursor field w_field value w_vbeln.
  check not w_vbeln is initial.
  set parameter id 'AUN' field w_vbeln.
  call transaction 'VA03' and skip first screen.
endform.                    " SHOW_CLICKED_CMR_DOC
*&---------------------------------------------------------------------*
*&      Form  SHOW_CLICKED_CM_DOC
*&---------------------------------------------------------------------*
form show_clicked_cm_doc.
  data : w_field(20),
         w_vbeln like vbak-vbeln.
  get cursor field w_field value w_vbeln.
  check not w_vbeln is initial.
  set parameter id 'VF' field w_vbeln.
  call transaction 'VF03' and skip first screen.
endform.                    " SHOW_CLICKED_CM_DOC
*&---------------------------------------------------------------------*
*&      Form  CONVERSION_EXIT_ALPHA_INPUT
*&---------------------------------------------------------------------*
form conversion_exit_alpha_input using ppppp.
  call function 'CONVERSION_EXIT_ALPHA_INPUT'
       exporting
            input  = ppppp
       importing
            output = ppppp.
endform.                    " CONVERSION_EXIT_ALPHA_INPUT
*&---------------------------------------------------------------------*
*&      Form  CONVERSION_EXIT_ALPHA_OUTPUT
*&---------------------------------------------------------------------*
form conversion_exit_alpha_output using ppppp.
  call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
       exporting
            input  = ppppp
       importing
            output = ppppp.
endform.                    " CONVERSION_EXIT_ALPHA_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  SEND_WORD
*&---------------------------------------------------------------------*
form send_word.
data : begin of attafile occurs 0.
       include structure zssd_mail_atta.
data : end of attafile.
data : begin of smtp_addr occurs 0.
       include structure zssd_mail_addr.
data : end of smtp_addr.

  call function 'Z_FSD_MAKE_WORD'
    exporting
      gubun               = w_gubun
    importing
      return              = w_return
    tables
      fax_h               = it_acm_fax_h
      fax_i               = it_acm_fax
      remi_h              = it_acm_remi_h
      remi_i              = it_acm_remi
    exceptions
      upload_fail         = 1
      download_fail       = 2
      others              = 3.

  if w_return <> 0.
    message i000 with 'MAKE WORD FILE FAIL'.
    exit.
  endif.

  refresh attafile. clear attafile.
  case w_gubun.
  when '1'.
    concatenate 'c:\acm_' ztsd_acl_l-zcmno '.rtf'
                into attafile-attafile.
  when '2'.
    concatenate 'c:\nor_' ztsd_acl_l-zcmno '.rtf'
                into attafile-attafile.
  endcase.
  append attafile.

  call function 'Z_FSD_SEND_MAIL'
    exporting
      gubun           = w_gubun
      kunnr           = ztsd_acl_l-zcdst
    importing
      return          = w_return
    tables
      attafile        = attafile
      smtp_addr       = smtp_addr.

  if w_return <> 0.
    message i000 with 'Unable to send'.
    exit.
  endif.

  message i000 with 'Sending Mail'.
endform.                    " SEND_WORD















************************************************************************
*----------------------------------------------------------------------*
*   INCLUDE TABLECONTROL_FORMS                                         *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  USER_OK_TC                                               *
*&---------------------------------------------------------------------*
 form user_ok_tc using    p_tc_name type dynfnam
                          p_table_name
                          p_mark_name
                 changing p_ok      like sy-ucomm.

*-BEGIN OF LOCAL DATA--------------------------------------------------*
   data: l_ok              type sy-ucomm,
         l_offset          type i.
*-END OF LOCAL DATA----------------------------------------------------*

* Table control specific operations                                    *
*   evaluate TC name and operations                                    *
   search p_ok for p_tc_name.
   if sy-subrc <> 0.
     exit.
   endif.
   l_offset = strlen( p_tc_name ) + 1.
   l_ok = p_ok+l_offset.
* execute general and TC specific operations                           *
   case l_ok.
     when 'P--' or                     "top of list
          'P-'  or                     "previous page
          'P+'  or                     "next page
          'P++'.                       "bottom of list
       perform compute_scrolling_in_tc using p_tc_name
                                             l_ok.
       clear p_ok.
*     WHEN 'L--'.                       "total left
*       PERFORM FCODE_TOTAL_LEFT USING P_TC_NAME.
*
*     WHEN 'L-'.                        "column left
*       PERFORM FCODE_COLUMN_LEFT USING P_TC_NAME.
*
*     WHEN 'R+'.                        "column right
*       PERFORM FCODE_COLUMN_RIGHT USING P_TC_NAME.
*
*     WHEN 'R++'.                       "total right
*       PERFORM FCODE_TOTAL_RIGHT USING P_TC_NAME.
*
     when 'MARK'.                      "mark all filled lines
       perform fcode_tc_mark_lines using p_tc_name
                                         p_table_name
                                         p_mark_name   .
       clear p_ok.

     when 'DMRK'.                      "demark all filled lines
       perform fcode_tc_demark_lines using p_tc_name
                                           p_table_name
                                           p_mark_name .
       clear p_ok.

*     WHEN 'SASCEND'   OR
*          'SDESCEND'.                  "sort column
*       PERFORM FCODE_SORT_TC USING P_TC_NAME
*                                   l_ok.

   endcase.

 endform.                              " USER_OK_TC

*&---------------------------------------------------------------------*
*&      Form  COMPUTE_SCROLLING_IN_TC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*      -->P_OK       ok code
*----------------------------------------------------------------------*
 form compute_scrolling_in_tc using    p_tc_name
                                       p_ok.
*-BEGIN OF LOCAL DATA--------------------------------------------------*
   data l_tc_new_top_line     type i.
   data l_tc_name             like feld-name.
   data l_tc_lines_name       like feld-name.
   data l_tc_field_name       like feld-name.

   field-symbols <tc>         type cxtab_control.
   field-symbols <lines>      type i.
*-END OF LOCAL DATA----------------------------------------------------*

   assign (p_tc_name) to <tc>.
* get looplines of TableControl
   concatenate 'G_' p_tc_name '_LINES' into l_tc_lines_name.
   assign (l_tc_lines_name) to <lines>.


* is no line filled?                                                   *
   if <tc>-lines = 0.
*   yes, ...                                                           *
     l_tc_new_top_line = 1.
   else.
*   no, ...                                                            *
     call function 'SCROLLING_IN_TABLE'
          exporting
               entry_act             = <tc>-top_line
               entry_from            = 1
               entry_to              = <tc>-lines
               last_page_full        = 'X'
               loops                 = <lines>
               ok_code               = p_ok
               overlapping           = 'X'
          importing
               entry_new             = l_tc_new_top_line
          exceptions
*              NO_ENTRY_OR_PAGE_ACT  = 01
*              NO_ENTRY_TO           = 02
*              NO_OK_CODE_OR_PAGE_GO = 03
               others                = 0.
   endif.

* get actual tc and column                                             *
   get cursor field l_tc_field_name
              area  l_tc_name.

   if syst-subrc = 0.
     if l_tc_name = p_tc_name.
*     set actual column                                                *
       set cursor field l_tc_field_name line 1.
     endif.
   endif.

* set the new top line                                                 *
   <tc>-top_line = l_tc_new_top_line.


 endform.                              " COMPUTE_SCROLLING_IN_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_MARK_LINES
*&---------------------------------------------------------------------*
*       marks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
form fcode_tc_mark_lines using p_tc_name
                               p_table_name
                               p_mark_name.
*-BEGIN OF LOCAL DATA--------------------------------------------------*
  data l_table_name       like feld-name.

  field-symbols <tc>         type cxtab_control.
  field-symbols <table>      type standard table.
  field-symbols <wa>.
  field-symbols <mark_field>.
*-END OF LOCAL DATA----------------------------------------------------*

  assign (p_tc_name) to <tc>.

* get the table, which belongs to the tc                               *
   concatenate p_table_name '[]' into l_table_name. "table body
   assign (l_table_name) to <table>.                "not headerline

* mark all filled lines                                                *
  loop at <table> assigning <wa>.

*   access to the component 'FLAG' of the table header                 *
     assign component p_mark_name of structure <wa> to <mark_field>.

     <mark_field> = 'X'.
  endloop.
endform.                                          "fcode_tc_mark_lines

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_DEMARK_LINES
*&---------------------------------------------------------------------*
*       demarks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
form fcode_tc_demark_lines using p_tc_name
                                 p_table_name
                                 p_mark_name .
*-BEGIN OF LOCAL DATA--------------------------------------------------*
  data l_table_name       like feld-name.

  field-symbols <tc>         type cxtab_control.
  field-symbols <table>      type standard table.
  field-symbols <wa>.
  field-symbols <mark_field>.
*-END OF LOCAL DATA----------------------------------------------------*

  assign (p_tc_name) to <tc>.

* get the table, which belongs to the tc                               *
   concatenate p_table_name '[]' into l_table_name. "table body
   assign (l_table_name) to <table>.                "not headerline

* demark all filled lines                                              *
  loop at <table> assigning <wa>.

*   access to the component 'FLAG' of the table header                 *
     assign component p_mark_name of structure <wa> to <mark_field>.

     <mark_field> = space.
  endloop.
endform.                                          "fcode_tc_mark_lines
*&---------------------------------------------------------------------*
*&      Form  GET_FIELD_NAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_MAT_FIELD  text
*      -->P_W_QTY_FIELD  text
*      -->P_W_COUNT  text
*----------------------------------------------------------------------*
form GET_FIELD_NAME using    p_mat_field
                             p_qty_field
                             P_SELKZ_FIELD
                             p_count.

   CONCATENATE 'RV45A-MABNR(' P_COUNT ')' INTO P_MAT_FIELD.

   CONCATENATE 'VBAP-ZMENG(' P_COUNT ')' INTO P_QTY_FIELD.

   CONCATENATE 'RV45A-VBAP_SELKZ(' P_COUNT ')' INTO P_SELKZ_FIELD.

endform.                    " GET_FIELD_NAME
*&---------------------------------------------------------------------*
*&      Form  GET_FIELD_NAME_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_KSCHL_FIELD  text
*      -->P_W_KBTER_FIELD  text
*      -->P_W_KOEIN_FIELD  text
*      -->P_W_CT  text
*----------------------------------------------------------------------*
form GET_FIELD_NAME_1 using    p_kschl
                               p_kbter
                               p_koein
                               p_ct.
   CONCATENATE 'KOMV-KSCHL(' P_CT ')'  INTO P_KSCHL.

   CONCATENATE 'KOMV-KBETR(' P_CT ')'  INTO P_KBTER.

   CONCATENATE 'RV61A-KOEIN(' P_CT ')' INTO P_KOEIN.


endform.                    " GET_FIELD_NAME_1
