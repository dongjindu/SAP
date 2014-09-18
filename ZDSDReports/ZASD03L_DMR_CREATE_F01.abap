*----------------------------------------------------------------------*
***INCLUDE ZASD03L_DMR_CREATE_F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
form read_data.
  select *
         into corresponding fields of table g_tc_9000_itab
         from ztsd_rec_l
        where zissn in s_zissn
        and   zdmfg eq p_zdmfg
        and   zrctt ne 0.
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
*&      Form  PROCESS_DMR
*&---------------------------------------------------------------------*
form process_dmr.
  data : w_index like sy-tabix,
         w_zacdt like sy-datum,
         w_fkdat like sy-datum.
  data : l_kschl like konv-kschl,
         l_amount like it_recitem-zrcpp.
  data : cnt(2) type n.
  data: l_text(20).
  DATA: W_MAT_FIELD(20),
        W_QTY_FIELD(20),
        W_COUNT(02)  TYPE N.

  clear w_cnt.
  loop at   g_tc_9000_itab
       into g_tc_9000_wa
       where flag = 'X'
       and   ( zdmfg = ' ' or zdmfg = '7' ).
    w_cnt = w_cnt + 1.
    exit.
  endloop.
  if w_cnt <> 1.
    message i000 with text-m09 '(STATUS : _,7)'.
    exit.
  endif.

  loop at   g_tc_9000_itab
       into g_tc_9000_wa
       where flag = 'X'
       and   ( zdmfg = ' ' or zdmfg = '7' )
       and   not zvend is initial
       and   not zissn is initial.
    refresh : bdc_tab, mess_tab.
    clear   : bdc_tab, mess_tab.

    w_index = sy-tabix.
*-->requested by MR. KO, changed by Chris
*   get the item model and ocndition type and amount
    perform get_recliam_items using g_tc_9000_wa.

*-->end of change on 05/03/2005

    select single *
           from usr01
          where bname = sy-uname.
    case usr01-datfm.
      when '1'. "DD.MM.YYYY
*        W_ZACDT+4(4) = G_TC_9000_WA-ZACDT+0(4).
*        W_ZACDT+2(2) = G_TC_9000_WA-ZACDT+4(2).
*        W_ZACDT+0(2) = G_TC_9000_WA-ZACDT+6(2).
        w_fkdat+4(4) = sy-datum+0(4).
        w_fkdat+2(2) = sy-datum+4(2).
        w_fkdat+0(2) = sy-datum+6(2).
      when '2' or '3'. "MM/DD/YYYY "MM-DD-YYYY
*        W_ZACDT+4(4) = G_TC_9000_WA-ZACDT+0(4).
*        W_ZACDT+0(2) = G_TC_9000_WA-ZACDT+4(2).
*        W_ZACDT+2(2) = G_TC_9000_WA-ZACDT+6(2).
        w_fkdat+4(4) = sy-datum+0(4).
        w_fkdat+0(2) = sy-datum+4(2).
        w_fkdat+2(2) = sy-datum+6(2).
    endcase.

    select single *
           from knvv
          where kunnr = g_tc_9000_wa-zvend
          and   spart = '99'
          and   loevm = ''. "DELETION FLAG

    perform bdc_fill using :
            'X' 'SAPMV45A'             '0101',
            ' ' 'BDC_OKCODE'           '/00',
            ' ' 'VBAK-AUART'           'ZWDR',
            ' ' 'VBAK-VKORG'           knvv-vkorg,
            ' ' 'VBAK-VTWEG'           '40',
            ' ' 'VBAK-SPART'           '99'.

    CLEAR: W_COUNT.
    loop at it_head.
            W_COUNT = W_COUNT + 1.
            cnt = '03'.
            PERFORM GET_FIELD_NAME USING W_MAT_FIELD
                                         W_QTY_FIELD
                                         W_COUNT.
    perform bdc_fill using :
            'X' 'SAPMV45A'             '4001',
            ' ' 'BDC_OKCODE'           '/00',
            ' ' 'VBKD-FKDAT'           w_fkdat,
            ' ' 'VBKD-BSTKD'           g_tc_9000_wa-zissn,
*           ' ' 'VBKD-BSTDK'           W_ZACDT,
            ' ' 'KUAGV-KUNNR'          g_tc_9000_wa-zvend,
*            ' ' 'RV45A-MABNR(01)'     it_recitem-matnr, "'RECLAIM ITEM'
            ' '  W_MAT_FIELD           it_head-matnr,
*            ' ' 'VBAP-ZMENG(01)'       '1',
            ' '  W_QTY_FIELD           '1',
            'X' 'SAPMV45A'             '4001',
            ' ' 'BDC_OKCODE'           '/00',
            'X' 'SAPMV45A'             '4001',
            ' ' 'BDC_OKCODE'           '/00',

            'X' 'SAPMV45A'             '4001',

            ' ' 'BDC_OKCODE'           '=PKO1',
            ' ' 'KUWEV-KUNNR'          g_tc_9000_wa-zvend,
*            ' ' 'BDC_CURSOR'           'RV45A-MABNR(01)'.
            ' ' 'BDC_CURSOR'           W_MAT_FIELD.

*            'X' 'SAPMV45A'             '4001',
*            ' ' 'BDC_OKCODE'           '=KKAU',
*            'X' 'SAPMV45A'             '4002',
*            ' ' 'VBAK-WAERK'           g_tc_9000_wa-zpycr,
*            ' ' 'BDC_OKCODE'           '/EBACK',
*            'X' 'SAPMV45A'             '4001',
*            ' ' 'RV45A-VBAP_SELKZ(01)' 'X'.
*

   perform bdc_fill using :
            'X' 'SAPMV45A'             '5003',
            ' ' 'BDC_OKCODE'           '/00'.

* fill the items by modl and condition type.


    loop at it_recitem where matnr = it_head-matnr
                         and zissn = it_head-zissn
                         and zvend = it_head-zvend
                         .
      " conditon type
      if it_recitem-zctyp = 'C'.
        l_kschl = it_recitem-zconc.
      else.
        l_kschl = it_recitem-zconw.
      endif.
      " total amount
      l_amount = it_recitem-zrcpp + it_recitem-zrcll + it_recitem-zrcss.
      write l_amount to w_char_11 currency g_tc_9000_wa-zpycr.


      concatenate 'KOMV-KSCHL(' cnt ')' into l_text.
      perform bdc_fill using:
            ' ' l_text                 l_kschl  .

      concatenate 'KOMV-KBETR(' cnt ')' into l_text.
      perform bdc_fill using:
            ' ' l_text                 w_char_11.

      concatenate 'RV61A-KOEIN(' cnt ')' into l_text.
      perform bdc_fill using:
            ' ' l_text                 g_tc_9000_wa-zpycr.
      cnt = cnt + 1.
    endloop.
    perform bdc_fill using :
            'X' 'SAPMV45A'             '5003',
            ' ' 'BDC_OKCODE'           '/EBACK'.



    endloop.
    perform bdc_fill using :
            'X' 'SAPMV45A'             '4001',
            ' ' 'BDC_OKCODE'           '=SICH'.
    data : www(1). www = 'N'.
    call transaction 'VA01' using bdc_tab mode www
                                  update 'S'
                                  messages into mess_tab.
    perform message_adjust_dmr.
    if w_result = 'S'.
      g_tc_9000_wa-zdmrn = mess_tab-msgv2.
      g_tc_9000_wa-zdmrd = sy-datum.
      g_tc_9000_wa-zdmfg = '9'.

      modify g_tc_9000_itab from g_tc_9000_wa index w_index.

      update ztsd_rec_l set : zdmrn = g_tc_9000_wa-zdmrn
                              zdmrd = g_tc_9000_wa-zdmrd
                              zdmfg = g_tc_9000_wa-zdmfg
                        where zvend = g_tc_9000_wa-zvend
                        and   zissn = g_tc_9000_wa-zissn.
    else. " 'E'.
      exit.
    endif.
  endloop.
endform.                    " PROCESS_DMR
*&---------------------------------------------------------------------*
*&      Form  PROCESS_DMR_CANC
*&---------------------------------------------------------------------*
form process_dmr_canc.
  data : w_index like sy-tabix.

  read table g_tc_9000_itab into g_tc_9000_wa
             with key flag = 'X'
                      zdmfg = '9'.
  if sy-subrc <> 0.
    message i000 with text-m09 '(STATUS : 9)'.
    exit.
  endif.

  loop at   g_tc_9000_itab
       into g_tc_9000_wa
       where flag = 'X'
       and   zdmfg = '9'.
    w_index = sy-tabix.

    perform conversion_exit_alpha_input using g_tc_9000_wa-zdmrn.

    clear   : order_header_inx.
    refresh : return.
    clear   : return.

    order_header_inx-updateflag = 'D'.

    call function 'BAPI_SALESORDER_CHANGE'
      exporting
        salesdocument               = g_tc_9000_wa-zdmrn
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
              wait   = 'X'
         importing
              return = return.

    perform message_adjust_dmr_canc.
    if w_result = 'S'.
      g_tc_9000_wa-zdmrn = '          '.
      g_tc_9000_wa-zdmrd = '        '.
      g_tc_9000_wa-zdmfg = ' '.

      modify g_tc_9000_itab from g_tc_9000_wa index w_index.

      update ztsd_rec_l set : zdmrn = g_tc_9000_wa-zdmrn
                              zdmrd = g_tc_9000_wa-zdmrd
                              zdmfg = g_tc_9000_wa-zdmfg
                        where zvend = g_tc_9000_wa-zvend
                        and   zissn = g_tc_9000_wa-zissn.
    else. " 'E'.
      exit.
    endif.
  endloop.
endform.                    " PROCESS_DMR_CANC
*&---------------------------------------------------------------------*
*&      Form  PROCESS_DM
*&---------------------------------------------------------------------*
form process_dm.
  data : begin of w_vend occurs 0,
         zvend like ztsd_rec_l-zvend,
         end of w_vend.

  data : w_index like sy-tabix.

  refresh : bdc_tab, mess_tab, w_vend.
  clear   : bdc_tab, mess_tab, w_vend.

  clear w_cnt.
  loop at   g_tc_9000_itab
       into g_tc_9000_wa
       where flag = 'X'
       and   zdmfg = '9'.
    w_cnt = w_cnt + 1.
    exit.
  endloop.
  if w_cnt <> 1.
    message i000 with text-m09 '(STATUS : 9)'.
    exit.
  endif.

  perform bdc_fill using :
          'X' 'SDBILLDL'             '1000',
          ' ' 'P_FKART-LOW'          'ZWL2',
          ' ' 'P_ALLEA'              'X',
          ' ' 'BDC_OKCODE'           '=%009'.

** : BDC Logic modify(11/28/2011 BY KDM) - UD1K953451
  loop at   g_tc_9000_itab
       into g_tc_9000_wa
       where flag = 'X'
       and   zdmfg = '9'.
    perform bdc_fill using :
            'X' 'SAPLALDB'             '3000',
*            ' ' 'RSCSEL-SLOW_I(02)'    g_tc_9000_wa-zdmrn,
            ' ' 'RSCSEL_255-SLOW_I(02)'    g_tc_9000_wa-zdmrn,
            ' ' 'BDC_OKCODE'           '=P+'.

    w_vend-zvend = g_tc_9000_wa-zvend.
    collect w_vend.
  endloop.

*  perform bdc_fill using :
*          'X' 'SAPLALDB'             '3000',
*          ' ' 'BDC_OKCODE'           '=ACPT',
*          'X' 'SDBILLDL'             '1000',
*          ' ' 'BDC_OKCODE'           '=ONLI',
*          'X' 'SAPLSLVC_FULLSCREEN'  '0500',
*          ' ' 'BDC_OKCODE'           '=&ALL',
*          'X' 'SAPLSLVC_FULLSCREEN'  '0500',
*          ' ' 'BDC_OKCODE'           '=SAMH'.
*  describe table w_vend lines w_cnt.
*  if w_cnt = 1.
*    perform bdc_fill using :
*          'X' 'SAPMV60A'             '0104',
*          ' ' 'BDC_OKCODE'           '=SICH'.
*  else.
*    perform bdc_fill using :
*          'X' 'SAPMV60A'             '0103', "SPRIT
*          ' ' 'BDC_OKCODE'           '=SICH'.
*  endif.
*  perform bdc_fill using :
*          'X' 'SAPLSLVC_FULLSCREEN'  '0500',
*          ' ' 'BDC_OKCODE'           '=&F03',
*          'X' 'SDBILLDL'             '1000',
*          ' ' 'BDC_OKCODE'           '/EERW'.

  perform bdc_fill using :
          'X' 'SAPLALDB'             '3000',
          ' ' 'BDC_OKCODE'           '=ACPT',
          'X' 'SDBILLDL'             '1000',
          ' ' 'P_ALLEL'              'X',
          ' ' 'BDC_OKCODE'           '=ONLI',
          'X' 'SAPMSSY0'             '0120',
          ' ' 'BDC_OKCODE'           '=SAMH'.
  describe table w_vend lines w_cnt.
  if w_cnt = 1.
    perform bdc_fill using :
          'X' 'SAPMV60A'             '0104',
          ' ' 'BDC_OKCODE'           '=SICH'.
  else.
    perform bdc_fill using :
          'X' 'SAPMV60A'             '0103', "SPRIT
          ' ' 'BDC_OKCODE'           '=SICH'.
  endif.
  perform bdc_fill using :
          'X' 'SAPMSSY0'             '0120',
          ' ' 'BDC_OKCODE'           '=&F03',
          'X' 'SDBILLDL'             '1000',
          ' ' 'BDC_OKCODE'           '/EERW'.
** : BDC Logic modify(11/28/2011 BY KDM) - UD1K953451

  data : www(1). www = 'N'.
  call transaction 'VF04' using bdc_tab mode www "'N'
                                update 'S'
                                messages into mess_tab.
  perform message_adjust_dm.
  perform message_get_multi.
  if w_result = 'S'.
    loop at   g_tc_9000_itab
         into g_tc_9000_wa
         where flag = 'X'
         and   zdmfg = '9'.
      w_index = sy-tabix.

      perform conversion_exit_alpha_input using g_tc_9000_wa-zdmrn.
      select * from vbfa where vbelv eq g_tc_9000_wa-zdmrn
                         and   vbtyp_n eq 'P'.
        perform conversion_exit_alpha_output using vbfa-vbeln.
        read table mess_tab_multi with key msgv1 = vbfa-vbeln.
        if sy-subrc = 0.
          g_tc_9000_wa-zdmno = mess_tab_multi-msgv1.
          g_tc_9000_wa-zdmfg = 'Q'.
          exit.
        endif.
      endselect.

      modify g_tc_9000_itab from g_tc_9000_wa index w_index.

      update ztsd_rec_l set : zdmno = g_tc_9000_wa-zdmno
                              zdmfg = g_tc_9000_wa-zdmfg
                        where zvend = g_tc_9000_wa-zvend
                        and   zissn = g_tc_9000_wa-zissn.
    endloop.
  else. " 'E'.
    exit.
  endif.
endform.                    " PROCESS_DM
*&---------------------------------------------------------------------*
*&      Form  PROCESS_DM_ADJ
*&---------------------------------------------------------------------*
form process_dm_adj.
  data : w_index like sy-tabix,
         w_vbeln like vbak-vbeln.

  read table g_tc_9000_itab into g_tc_9000_wa
             with key flag = 'X'
                      zdmfg = 'Q'.
  if sy-subrc <> 0.
    message i000 with text-m09 '(STATUS : Q)'.
    exit.
  endif.

  " BY ZDMNO
  refresh bdc_list. clear bdc_list.
  loop at   g_tc_9000_itab
       into g_tc_9000_wa
       where flag = 'X'
       and   zdmfg = 'Q'.
    bdc_list-zdmno = g_tc_9000_wa-zdmno.
    collect bdc_list. clear bdc_list.
  endloop.

  loop at bdc_list.
    refresh : bdc_tab, mess_tab.
    clear   : bdc_tab, mess_tab.

    perform bdc_fill using :
            'X' 'SAPMV45A'             '0101',
            ' ' 'VBAK-AUART'           'ZWRA',
            ' ' 'VBAK-VKORG'           '',
            ' ' 'VBAK-VTWEG'           '',
            ' ' 'VBAK-SPART'           '',
            ' ' 'BDC_OKCODE'           '=COPY',
            'X' 'SAPLV45C'             '0100',
            ' ' 'VBRK-VBELN'           bdc_list-zdmno,
            ' ' 'BDC_OKCODE'           '=RUEF',
            'X' 'SAPLV60P'             '4413',
            ' ' 'BDC_OKCODE'           '=MKLO',
            'X' 'SAPLV60P'             '4413',
            ' ' 'BDC_OKCODE'           '=POPO'.

    loop at   g_tc_9000_itab
         into g_tc_9000_wa
         where flag = 'X'
         and   zdmfg = 'Q'
         and   zdmno = bdc_list-zdmno.

      perform conversion_exit_alpha_input using g_tc_9000_wa-zdmno.
      perform conversion_exit_alpha_input using g_tc_9000_wa-zdmrn.
      clear vbrp.
      select single
             *
             from vbrp
            where vbeln = g_tc_9000_wa-zdmno
            and   aubel = g_tc_9000_wa-zdmrn.
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
    perform message_adjust_dmr.
    if w_result = 'S'.
      w_vbeln = mess_tab-msgv2.
*****************************
      refresh : bdc_tab, mess_tab.
      clear   : bdc_tab, mess_tab.

      perform bdc_fill using :
              'X' 'SDBILLDL'             '1000',
              ' ' 'P_FKART-LOW'          'ZWRA',
              ' ' 'P_ALLEA'              'X',
              ' ' 'BDC_OKCODE'           '=%009'.

** : BDC Logic modify(12/07/2011 BY KDM) - Start UD1K953451
      perform bdc_fill using :
              'X' 'SAPLALDB'             '3000',
*            ' ' 'RSCSEL-SLOW_I(02)'    w_vbeln,
            ' ' 'RSCSEL_255-SLOW_I(02)'  w_vbeln,
              ' ' 'BDC_OKCODE'           '=P+'.

*      perform bdc_fill using :
*              'X' 'SAPLALDB'             '3000',
*              ' ' 'BDC_OKCODE'           '=ACPT',
*              'X' 'SDBILLDL'             '1000',
*              ' ' 'BDC_OKCODE'           '=ONLI',
*              'X' 'SAPLSLVC_FULLSCREEN'  '0500',
*              ' ' 'BDC_OKCODE'           '=&ALL',
*              'X' 'SAPLSLVC_FULLSCREEN'  '0500',
*              ' ' 'BDC_OKCODE'           '=SAMH',
*              'X' 'SAPMV60A'             '0104',
*              ' ' 'BDC_OKCODE'           '=SICH',
*              'X' 'SAPLSLVC_FULLSCREEN'  '0500',
*              ' ' 'BDC_OKCODE'           '=&F03',
*              'X' 'SDBILLDL'             '1000',
*              ' ' 'BDC_OKCODE'           '/EERW'.

      perform bdc_fill using :
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
** : BDC Logic modify(12/07/2011 BY KDM) - End UD1K953451

      call transaction 'VF04' using bdc_tab mode www "'N'
                                    update 'S'
                                    messages into mess_tab.
      perform message_adjust_dm.
      if w_result = 'S'.
*****************************
        loop at   g_tc_9000_itab
             into g_tc_9000_wa
             where flag = 'X'
             and   zdmfg = 'Q'.
          w_index = sy-tabix.

          g_tc_9000_wa-zdmrn = '          '.
          g_tc_9000_wa-zdmrd = '        '.
          g_tc_9000_wa-zdmno = '          '.
          g_tc_9000_wa-zdmfg = '7'.

          modify g_tc_9000_itab from g_tc_9000_wa index w_index.

          update ztsd_rec_l set : zdmrn = g_tc_9000_wa-zdmrn
                                  zdmrd = g_tc_9000_wa-zdmrd
                                  zdmno = g_tc_9000_wa-zdmno
                                  zdmfg = g_tc_9000_wa-zdmfg
                            where zvend = g_tc_9000_wa-zvend
                            and   zissn = g_tc_9000_wa-zissn.
        endloop.
      else. " 'E'.
        exit.
      endif.
    else. " 'E'.
      exit.
    endif.
  endloop.
endform.                    " PROCESS_DM_ADJ
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
*&      Form  MESSAGE_ADJUST_DMR
*&---------------------------------------------------------------------*
form message_adjust_dmr.
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
endform.                    " MESSAGE_ADJUST_DMR
*&---------------------------------------------------------------------*
*&      Form  MESSAGE_ADJUST_DMR_CANC
*&---------------------------------------------------------------------*
form message_adjust_dmr_canc.
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
endform.                    " MESSAGE_ADJUST_DMR_CANC
*&---------------------------------------------------------------------*
*&      Form  MESSAGE_ADJUST_DM
*&---------------------------------------------------------------------*
form message_adjust_dm.
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
endform.                    " MESSAGE_ADJUST_DM
*&---------------------------------------------------------------------*
*&      Form  MESSAGE_GET_MULTI
*&---------------------------------------------------------------------*
form message_get_multi.
  refresh mess_tab_multi. clear mess_tab_multi.
  loop at mess_tab where msgtyp = 'S'
                   and   msgid  = 'VF'
                   and ( msgnr  = '051' or
                         msgnr  = '050' or
                         msgnr  = '311' ).
    mess_tab_multi-msgv1 = mess_tab-msgv1.
    append mess_tab_multi. clear mess_tab_multi.
  endloop.
endform.                    " MESSAGE_GET_MULTI
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
    move-corresponding g_tc_9000_wa to ztsd_rec_l.
    if ztsd_rec_l-zdmfg <> 'Q'.
      message i000 with text-m08
                        '( STATUS :'
                        ztsd_rec_l-zdmfg
                        ')'.
      exit.
    endif.

    perform get_rec_noti.
    perform write_rec_noti.
  else.
    message i000 with text-m02.
  endif.
endform.                    " PROCESS_PRIN
*&---------------------------------------------------------------------*
*&      Form  GET_REC_NOTI
*&---------------------------------------------------------------------*
form get_rec_noti.
  refresh it_rec_noti. clear it_rec_noti.

  clear w_numc_5.
  loop at   g_tc_9000_itab
       into g_tc_9000_wa
       where zvend = ztsd_rec_l-zvend.
    w_numc_5 = w_numc_5 + 1.

    it_rec_noti-zseq  = w_numc_5.
    it_rec_noti-zissn = g_tc_9000_wa-zissn.
    it_rec_noti-zrcqt = g_tc_9000_wa-zrcqt.
    it_rec_noti-zrctt = g_tc_9000_wa-zrctt.
    it_rec_noti-zpycr = g_tc_9000_wa-zpycr.

    append it_rec_noti. clear it_rec_noti.
  endloop.
endform.                    " GET_REC_NOTI
*&---------------------------------------------------------------------*
*&      Form  WRITE_REC_NOTI
*&---------------------------------------------------------------------*
form write_rec_noti.
  leave to list-processing.
  set pf-status 'EMAIL'.
  w_gubun = '3'. "WCI

  perform get_email_address using ztsd_rec_l-zvend.

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
*                            AND   KUNNR = ZTSD_REC_L-ZVEND.
*  CONCATENATE KNVK-NAMEV KNVK-NAME1 INTO W_ATTN SEPARATED BY SPACE.
*
*  SELECT SINGLE * FROM KNVK WHERE PARNR = '0000000002'
*                            AND   KUNNR = ZTSD_REC_L-ZVEND.
*  CONCATENATE KNVK-NAMEV KNVK-NAME1 INTO W_CC   SEPARATED BY SPACE.

  skip 4. "5
  write:/     'Warranty Claim Invoice'.
  write:/     'Vendor Code :', ztsd_rec_l-zvend,
          49  'Invoice Date :', sy-datum.
  skip 0. "3
  write:/ sy-uline.
  skip 1. "4

  write:/(120) 'Hyundai Motor Munufacturing Alabama' centered.
  write:/(120) 'FAX : '      centered.
  write:/(120) 'OUTWARD FAX' centered,
          109  'Page 1 of 1'.

  clear kna1.
  select single
         *
         from kna1
        where kunnr = ztsd_rec_l-zvend.

  format color col_normal.
  write:/ sy-uline.
  write:/     'REF  :',   (52) ztsd_rec_l-zdmno, ""
          60  'FAX NO :', (52) kna1-telfx.
  write:/     'TO   :',   (52) kna1-name1,
          60  'DATE   :', (52) sy-datum.
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
  write:/(120) 'SUBJECT : Warranty Claim Invoice' centered.
  write:/ sy-uline.
  format color col_normal off.

*         123456789012345678901234567890123456789012345678901234567890
  write:/'1. We submit the claims and request to reimburse the',
         'related amount as follows for your products.'.
  skip 1.
  write:/99 'Unit :', ztsd_rec_l-zpycr.

  write:/12(98) sy-uline.
  write:/1(11) '' no-gap.
  format color col_normal.
  write:   (05) 'SeqNo',
         22(10) 'Issue No.',
         37(15) 'Claim qty.'   right-justified,
         58(30) 'Claim amount' right-justified,
         93(17) 'Remarks'.
  format color col_normal off.
  write:/12(98) sy-uline.

  clear : w_zrcqt, w_zrctt.
  loop at it_rec_noti.
    write:/12(05) it_rec_noti-zseq no-zero,
           22(10) it_rec_noti-zissn,
           37(15) it_rec_noti-zrcqt no-zero right-justified,
           58(30) it_rec_noti-zrctt currency it_rec_noti-zpycr,
           93(17) ''.

    w_zrcqt = w_zrcqt + it_rec_noti-zseq.
    w_zrctt = w_zrctt + it_rec_noti-zrctt.
    skip 1.
  endloop.
  describe table it_rec_noti lines w_cnt.

  if w_cnt < 20.
    w_cnt = ( 20 - w_cnt ) * 2.
    skip w_cnt.
  endif.

  write:/12(98) sy-uline.
  write:/1(11) '' no-gap.
  format color col_normal.
  write:   (05) 'Total',
         37(15) w_zrcqt no-zero right-justified,
         58(30) w_zrctt currency it_rec_noti-zpycr,
         93(17) ''.
  format color col_normal off.
  write:/12(98) sy-uline.

  skip 1.
  write:/'2. For your reference, we inform you of our bank information'.
  write:/12(17) 'Bank Account    :', ''.
  write:/12(17) 'Account Name    :', ''.
  write:/12(17) 'Account No      :', ''.
  write:/12(17) 'Beneficiary     :',
                'Hyundai Motor Manufacturing Alabama'.

  skip 1.
  write:/' Note'.
*         123456789012345678901234567890123456789012345678901234567890
  write:/' If you need more information for these claims,',
         'please do not hesitate to contact us.'.
  skip 1.
  write:/' Yours sincerely,'.
  skip 1.
  write:/' H. J. Kim'.
  write:/' General Manager'.
  write:/' Quality Information Team'.

  skip 1.
  write:/' Encl : Claim Defect Report & Claim Invoice.'.
  write:/ sy-uline.
  write:/(120)
          'Copyright(c) 2001 Hyundai Motor Campany. All Rights Reserved'
          centered.


  refresh it_rec_noti_h. clear it_rec_noti_h.
  it_rec_noti_h-zvend = ztsd_rec_l-zvend.
  it_rec_noti_h-zdmno = ztsd_rec_l-zdmno.
  it_rec_noti_h-zdate = sy-datum.
  append it_rec_noti_h. clear it_rec_noti_h.
endform.                    " WRITE_REC_NOTI
*&---------------------------------------------------------------------*
*&      Form  PROCESS_INFM
*&---------------------------------------------------------------------*
form process_infm.
  clear w_cnt.

  loop at   g_tc_9000_itab
       into g_tc_9000_wa
       where flag = 'X'
       and   ( zdmfg = ' ' or zdmfg = '7' ).
    w_cnt = w_cnt + 1.
  endloop.

  if w_cnt = 1.
    move-corresponding g_tc_9000_wa to ztsd_rec_l.
    if ztsd_rec_l-zdmfg ne ' ' and
       ztsd_rec_l-zdmfg ne '7'.
      message i000 with text-m08
                        '( STATUS :'
                        ztsd_rec_l-zdmfg
                        ')'.
      exit.
    endif.

    refresh attafile. clear attafile.
*
    perform make_txt using 'INFM'.

    concatenate 'c:\wci_' ztsd_rec_l-zvend '_'
                ztsd_rec_l-zissn '_dat.txt'
                into attafile-attafile.
    append attafile.
    concatenate 'c:\wci_' ztsd_rec_l-zvend '_'
                ztsd_rec_l-zissn '_asc.txt'
                into attafile-attafile.
    append attafile.
*

    w_gubun = '4'.
    call function 'Z_FSD_SEND_MAIL'
         exporting
              gubun     = w_gubun
              kunnr     = ztsd_rec_l-zvend
         importing
              return    = w_return
         tables
              attafile  = attafile
              smtp_addr = smtp_addr.

    if w_return <> 0.
      message i000 with 'Unable to send'.
      exit.
    endif.

    message i000 with 'Sending Mail'.
  else.
    message i000 with text-m02.
  endif.
endform.                    " PROCESS_INFM
*&---------------------------------------------------------------------*
*&      Form  GET_EMAIL_ADDRESS
*&---------------------------------------------------------------------*
form get_email_address using p_kunnr.
  refresh mail_addr. clear mail_addr.
  call function 'Z_FSD_MAIL_ADDR'
       exporting
            kunnr          = p_kunnr
       tables
            mail_addr      = mail_addr
       exceptions
            not_found_kna1 = 1
            not_found_knvk = 2
            others         = 3.
endform.                    " GET_EMAIL_ADDRESS
*&---------------------------------------------------------------------*
*&      Form  SHOW_CLICKED_DMR_DOC
*&---------------------------------------------------------------------*
form show_clicked_dmr_doc.
  data : w_field(20),
         w_vbeln like vbak-vbeln.
  get cursor field w_field value w_vbeln.
  check not w_vbeln is initial.
  set parameter id 'AUN' field w_vbeln.
  call transaction 'VA03' and skip first screen.
endform.                    " SHOW_CLICKED_DMR_DOC
*&---------------------------------------------------------------------*
*&      Form  SHOW_CLICKED_DM_DOC
*&---------------------------------------------------------------------*
form show_clicked_dm_doc.
  data : w_field(20),
         w_vbeln like vbak-vbeln.
  get cursor field w_field value w_vbeln.
  check not w_vbeln is initial.
  set parameter id 'VF' field w_vbeln.
  call transaction 'VF03' and skip first screen.
endform.                    " SHOW_CLICKED_DM_DOC
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
  call function 'Z_FSD_MAKE_WORD'
       exporting
            gubun         = w_gubun
       importing
            return        = w_return
       tables
            wci_h         = it_rec_noti_h
            wci_i         = it_rec_noti
       exceptions
            upload_fail   = 1
            download_fail = 2
            others        = 3.

  if w_return <> 0.
    message i000 with 'MAKE WORD FILE FAIL'.
    exit.
  endif.

  refresh attafile. clear attafile.
  case w_gubun.
    when '3'.
      concatenate 'c:\wci_' ztsd_rec_l-zdmno '.rtf'
                  into attafile-attafile.
  endcase.
  append attafile.

*
  perform make_txt using 'PRIN'.
  case w_gubun.
    when '3'.
      concatenate 'c:\wci_' ztsd_rec_l-zdmno '_dat.txt'
                  into attafile-attafile.
  endcase.
  append attafile.
  case w_gubun.
    when '3'.
      concatenate 'c:\wci_' ztsd_rec_l-zdmno '_asc.txt'
                  into attafile-attafile.
  endcase.
  append attafile.
*

  call function 'Z_FSD_SEND_MAIL'
       exporting
            gubun     = w_gubun
            kunnr     = ztsd_rec_l-zvend
       importing
            return    = w_return
       tables
            attafile  = attafile
            smtp_addr = smtp_addr.

  if w_return <> 0.
    message i000 with 'Unable to send'.
    exit.
  endif.

  message i000 with 'Sending Mail'.
endform.                    " SEND_WORD
*&---------------------------------------------------------------------*
*&      Form  MAKE_TXT
*&---------------------------------------------------------------------*
form make_txt using gubun.
  select *
         into table it_rec_h
         from ztsd_rec_h
        where zissn eq ztsd_rec_l-zissn
        and   zvend eq ztsd_rec_l-zvend.

  select *
         into table it_rec_i
         from ztsd_rec_i
              for all entries in it_rec_h
        where zissn eq it_rec_h-zissn
        and   zvend eq it_rec_h-zvend
        and   zacln eq it_rec_h-zacln
        and   zcdst eq it_rec_h-zcdst
        and   zcdlr eq it_rec_h-zcdlr
        and   zcser eq it_rec_h-zcser.

  refresh : rec_txt_dat, rec_txt_asc.
  clear   : rec_txt_dat, rec_txt_asc.

  perform txt_dat_title.

  loop at it_rec_h.
    rec_txt_dat-zvend = it_rec_h-zvend+0(04).
    rec_txt_asc-zvend = it_rec_h-zvend+0(04).
    rec_txt_dat-zissn = it_rec_h-zissn+0(07).
    rec_txt_asc-zissn = it_rec_h-zissn+0(07).
    rec_txt_dat-zacln = it_rec_h-zacln+0(07).
    rec_txt_asc-zacln = it_rec_h-zacln+0(07).
    rec_txt_dat-zcdst = it_rec_h-zcdst+0(05).
    rec_txt_asc-zcdst = it_rec_h-zcdst+0(05).
    rec_txt_dat-zcdlr = it_rec_h-zcdlr+0(05).
    rec_txt_asc-zcdlr = it_rec_h-zcdlr+0(05).
    rec_txt_dat-zcser = it_rec_h-zcser+0(06).
    rec_txt_asc-zcser = it_rec_h-zcser+0(06).
    rec_txt_asc-zcseq = it_rec_h-zcseq+0(02).               "UD1K913539
    rec_txt_dat-zcseq = it_rec_h-zcseq+0(02).               "UD1K913539


    write it_rec_h-zrcpp to rec_txt_dat-zrcpp
                            using edit mask 'RR_________'.
    write it_rec_h-zrcpp to rec_txt_asc-zrcpp
                            using edit mask 'RR_________'.
    write it_rec_h-zrcll to rec_txt_dat-zrcll
                            using edit mask 'RR_________'.
    write it_rec_h-zrcll to rec_txt_asc-zrcll
                            using edit mask 'RR_________'.
    write it_rec_h-zrcss to rec_txt_dat-zrcss
                            using edit mask 'RR_________'.
    write it_rec_h-zrcss to rec_txt_asc-zrcss
                            using edit mask 'RR_________'.
    write it_rec_h-zsprt to rec_txt_dat-zsprt
                            using edit mask 'RR_____'.
    write it_rec_h-zsprt to rec_txt_asc-zsprt
                            using edit mask 'RR_____'.
    write it_rec_h-zshar to rec_txt_dat-zshar
                            using edit mask 'RR_____'.
    write it_rec_h-zshar to rec_txt_asc-zshar
                            using edit mask 'RR_____'.

    select single * from ztsd_acm_h
           where zacln eq it_rec_h-zacln
           and   zcdst eq it_rec_h-zcdst
           and   zcdlr eq it_rec_h-zcdlr
           and   zcser eq it_rec_h-zcser
           and   zcseq eq it_rec_h-zcseq.
    if sy-subrc = 0.
      rec_txt_dat-zcpis = ztsd_acm_h-zcpis+0(08).
      rec_txt_asc-zcpis = ztsd_acm_h-zcpis+0(08).
      rec_txt_dat-zctyp = ztsd_acm_h-zctyp+0(01).
      rec_txt_asc-zctyp = ztsd_acm_h-zctyp+0(01).
      rec_txt_dat-zvin  = ztsd_acm_h-zvin+0(17).
      rec_txt_asc-zvin  = ztsd_acm_h-zvin+0(17).
      rec_txt_dat-zscod = ztsd_acm_h-zscod+0(03).
      rec_txt_asc-zscod = ztsd_acm_h-zscod+0(03).

      select single * from ztsd_vin_conv
             where zscod eq ztsd_acm_h-zscod.
      if sy-subrc = 0.
        rec_txt_dat-zspar = ztsd_vin_conv-zspar+0(15).
        rec_txt_asc-zspar = ztsd_vin_conv-zspar+0(15).
      else.
        rec_txt_dat-zspar = ''.
        rec_txt_asc-zspar = ''.
      endif.

      rec_txt_dat-zprdt = ztsd_acm_h-zprdt.
      rec_txt_asc-zprdt = ztsd_acm_h-zprdt.
      rec_txt_dat-zdlvy = ztsd_acm_h-zdlvy.
      rec_txt_asc-zdlvy = ztsd_acm_h-zdlvy.
      rec_txt_dat-zrpdt = ztsd_acm_h-zrpdt.
      rec_txt_asc-zrpdt = ztsd_acm_h-zrpdt.
      rec_txt_dat-zodrd = ztsd_acm_h-zodrd+0(06).
      rec_txt_asc-zodrd = ztsd_acm_h-zodrd+0(06).
      rec_txt_dat-zronm = ztsd_acm_h-zronm+0(10).
      rec_txt_asc-zronm = ztsd_acm_h-zronm+0(10).
      rec_txt_dat-zpidt = ztsd_acm_h-zpidt.
      rec_txt_asc-zpidt = ztsd_acm_h-zpidt.
      rec_txt_dat-zpodr = ztsd_acm_h-zpodr+0(06).
      rec_txt_asc-zpodr = ztsd_acm_h-zpodr+0(06).
      rec_txt_dat-zpron = ztsd_acm_h-zpron+0(10).
      rec_txt_asc-zpron = ztsd_acm_h-zpron+0(10).
      rec_txt_dat-zcptn = ztsd_acm_h-zcptn+0(15).
      rec_txt_asc-zcptn = ztsd_acm_h-zcptn+0(15).

      select single * from ztsd_part_inf
             where zcptn eq ztsd_acm_h-zcptn.
      if sy-subrc = 0.
        rec_txt_dat-zptna = ztsd_part_inf-zptna+0(30).
        rec_txt_asc-zptna = ztsd_part_inf-zptna+0(30).
      else.
        rec_txt_dat-zptna = ''.
        rec_txt_asc-zptna = ''.
      endif.

      rec_txt_dat-zmnop = ztsd_acm_h-zmnop+0(08).
      rec_txt_asc-zmnop = ztsd_acm_h-zmnop+0(08).
      rec_txt_dat-znatr = ztsd_acm_h-znatr+0(03).
      rec_txt_asc-znatr = ztsd_acm_h-znatr+0(03).
      rec_txt_dat-zcaus = ztsd_acm_h-zcaus+0(03).
      rec_txt_asc-zcaus = ztsd_acm_h-zcaus+0(03).
      rec_txt_dat-zsbla = ztsd_acm_h-zsbla+0(01).
      rec_txt_asc-zsbla = ztsd_acm_h-zsbla+0(01).
      rec_txt_dat-zsblb = ztsd_acm_h-zsblb+0(01).
      rec_txt_asc-zsblb = ztsd_acm_h-zsblb+0(01).
*  REQUESTED BY Mr. Ko CHANGED BY CHRIS    "UD1K913539
      if ztsd_acm_h-zctyp+0(01) = 'C'.
        append rec_txt_dat. "CLEAR REC_TXT_DAT.
        append rec_txt_asc. "CLEAR REC_TXT_ASC.
        continue.
      endif.
*  END OF CHANGE ON 12/16/2004
    endif.


    loop at it_rec_i where zissn eq it_rec_h-zissn
                     and   zvend eq it_rec_h-zvend
                     and   zacln eq it_rec_h-zacln
                     and   zcdst eq it_rec_h-zcdst
                     and   zcdlr eq it_rec_h-zcdlr
                     and   zcser eq it_rec_h-zcser.

      rec_txt_dat-zline = it_rec_i-zline+0(02).
      rec_txt_asc-zline = it_rec_i-zline+0(02).

      write it_rec_i-zvprc to rec_txt_dat-zvprc
                              using edit mask 'RR_________'.
      write it_rec_i-zvprc to rec_txt_asc-zvprc
                              using edit mask 'RR_________'.

      rec_txt_dat-zvmup = it_rec_i-zvmup+0(03).
      rec_txt_asc-zvmup = it_rec_i-zvmup+0(03).
      write it_rec_i-zrcpp to rec_txt_dat-zrcp2
                              using edit mask 'RR_________'.
      write it_rec_i-zrcpp to rec_txt_asc-zrcp2
                              using edit mask 'RR_________'.

      write it_rec_i-zrcll to rec_txt_dat-zrcl2
                              using edit mask 'RR_________'.
      write it_rec_i-zrcll to rec_txt_asc-zrcl2
                              using edit mask 'RR_________'.

      select single * from ztsd_acm_i
             where zacln eq it_rec_i-zacln
             and   zcdst eq it_rec_i-zcdst
             and   zcdlr eq it_rec_i-zcdlr
             and   zcser eq it_rec_i-zcser
             and   zline eq it_rec_i-zline.
      if sy-subrc = 0.
        rec_txt_dat-zrppn = ztsd_acm_i-zrppn+0(15).
        rec_txt_asc-zrppn = ztsd_acm_i-zrppn+0(15).

        select single * from ztsd_part_inf
               where zcptn eq ztsd_acm_i-zrppn.
        if sy-subrc = 0.
          rec_txt_dat-zptn2 = ztsd_part_inf-zptna(30).
          rec_txt_asc-zptn2 = ztsd_part_inf-zptna(30).
        else.
          rec_txt_dat-zptn2 = ''.
          rec_txt_asc-zptn2 = ''.
        endif.

        rec_txt_dat-zrmpq = ztsd_acm_i-zrmpq+0(02).
        rec_txt_asc-zrmpq = ztsd_acm_i-zrmpq+0(02).

        rec_txt_dat-zoper = ztsd_acm_i-zoper+0(08).
        rec_txt_asc-zoper = ztsd_acm_i-zoper+0(08).
        rec_txt_dat-zrmlq = ztsd_acm_i-zrmlq+0(01).
        rec_txt_asc-zrmlq = ztsd_acm_i-zrmlq+0(01).
        rec_txt_dat-zrmlt = ztsd_acm_i-zrmlt+1(04).
        rec_txt_asc-zrmlt = ztsd_acm_i-zrmlt+2(03).
        rec_txt_dat-zlrat = ztsd_acm_i-zlrat+0(05).
        rec_txt_asc-zlrat = ztsd_acm_i-zlrat+0(05).
        append rec_txt_dat. "CLEAR REC_TXT_DAT.
        append rec_txt_asc. "CLEAR REC_TXT_ASC.
      endif.
    endloop.

  endloop.

  case gubun.
    when 'INFM'.
 concatenate 'c:\wci_' ztsd_rec_l-zvend '_' ztsd_rec_l-zissn '_dat.txt'
                  into w_file.
    when 'PRIN'.
      concatenate 'c:\wci_' ztsd_rec_l-zdmno '_dat.txt'
                  into w_file.
  endcase.

  call function 'WS_DOWNLOAD'
    exporting
*     BIN_FILESIZE                  = ' '
*     CODEPAGE                      = ' '
      filename                      = w_file
      filetype                      = 'DAT'
*     MODE                          = ' '
*     WK1_N_FORMAT                  = ' '
*     WK1_N_SIZE                    = ' '
*     WK1_T_FORMAT                  = ' '
*     WK1_T_SIZE                    = ' '
*     COL_SELECT                    = ' '
*     COL_SELECTMASK                = ' '
*     NO_AUTH_CHECK                 = ' '
*   IMPORTING
*     FILELENGTH                    =
    tables
      data_tab                      = rec_txt_dat
*     FIELDNAMES                    =
    exceptions
      file_open_error               = 1
      file_write_error              = 2
      invalid_filesize              = 3
      invalid_type                  = 4
      no_batch                      = 5
      unknown_error                 = 6
      invalid_table_width           = 7
      gui_refuse_filetransfer       = 8
      customer_error                = 9
      others                        = 10.

  case gubun.
    when 'INFM'.
 concatenate 'c:\wci_' ztsd_rec_l-zvend '_' ztsd_rec_l-zissn '_asc.txt'
                  into w_file.
    when 'PRIN'.
      concatenate 'c:\wci_' ztsd_rec_l-zdmno '_asc.txt'
                  into w_file.
  endcase.

  call function 'WS_DOWNLOAD'
    exporting
*     BIN_FILESIZE                  = ' '
*     CODEPAGE                      = ' '
      filename                      = w_file
      filetype                      = 'ASC'
*     MODE                          = ' '
*     WK1_N_FORMAT                  = ' '
*     WK1_N_SIZE                    = ' '
*     WK1_T_FORMAT                  = ' '
*     WK1_T_SIZE                    = ' '
*     COL_SELECT                    = ' '
*     COL_SELECTMASK                = ' '
*     NO_AUTH_CHECK                 = ' '
*   IMPORTING
*     FILELENGTH                    =
    tables
      data_tab                      = rec_txt_asc
*     FIELDNAMES                    =
    exceptions
      file_open_error               = 1
      file_write_error              = 2
      invalid_filesize              = 3
      invalid_type                  = 4
      no_batch                      = 5
      unknown_error                 = 6
      invalid_table_width           = 7
      gui_refuse_filetransfer       = 8
      customer_error                = 9
      others                        = 10.
endform.                    " MAKE_TXT
*&---------------------------------------------------------------------*
*&      Form  TXT_DAT_TITLE
*&---------------------------------------------------------------------*
form txt_dat_title.
  rec_txt_dat-zvend = 'VEND'.
  rec_txt_dat-zissn = 'ISSN'.
  rec_txt_dat-zacln = 'ACLN'.
  rec_txt_dat-zcdst = 'CDST'.
  rec_txt_dat-zcdlr = 'CDLR'.
  rec_txt_dat-zcser = 'CSER'.
  rec_txt_dat-zcseq = 'CSEQ'.                               "UD1K913539
  rec_txt_dat-zcpis = 'CPIS'.
  rec_txt_dat-zctyp = 'CTYP'.
  rec_txt_dat-zvin  = 'VIN'.
  rec_txt_dat-zscod = 'SCOD'.
  rec_txt_dat-zspar = 'SPAR'.
  rec_txt_dat-zprdt = 'PRDT'.
  rec_txt_dat-zdlvy = 'DLVY'.
  rec_txt_dat-zrpdt = 'RPDT'.
  rec_txt_dat-zodrd = 'ODRD'.
  rec_txt_dat-zronm = 'RONM'.
  rec_txt_dat-zpidt = 'PIDT'.
  rec_txt_dat-zpodr = 'PODR'.
  rec_txt_dat-zpron = 'PRON'.
  rec_txt_dat-zcptn = 'CPTN'.
  rec_txt_dat-zptna = 'PTNA'.
  rec_txt_dat-zmnop = 'MNOP'.
  rec_txt_dat-znatr = 'NATR'.
  rec_txt_dat-zcaus = 'CAUS'.
  rec_txt_dat-zsbla = 'SBLA'.
  rec_txt_dat-zsblb = 'SBLB'.
  rec_txt_dat-zrcpp = 'RCPP'.
  rec_txt_dat-zrcll = 'RCLL'.
  rec_txt_dat-zrcss = 'RCSS'.
  rec_txt_dat-zsprt = 'SPRT'.
  rec_txt_dat-zshar = 'SHAR'.
  rec_txt_dat-zline = 'LINE'.
  rec_txt_dat-zrppn = 'RPPN'.
  rec_txt_dat-zptn2 = 'PTNA'.
  rec_txt_dat-zrmpq = 'RMPQ'.
  rec_txt_dat-zvprc = 'VPRC'.
  rec_txt_dat-zvmup = 'VMUP'.
  rec_txt_dat-zrcp2 = 'RCPP'.
  rec_txt_dat-zoper = 'OPER'.
  rec_txt_dat-zrmlq = 'RMLQ'.
  rec_txt_dat-zrmlt = 'RMLT'.
  rec_txt_dat-zlrat = 'LRAT'.
  rec_txt_dat-zrcl2 = 'RCLL'.

  append rec_txt_dat. clear rec_txt_dat.

  append rec_txt_dat. clear rec_txt_dat. "SPACE LINE
endform.                    " TXT_DAT_TITLE












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
*&      Form  get_recliam_items
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_TC_9000_WA  text
*----------------------------------------------------------------------*
form get_recliam_items using p_wa like g_tc_9000_wa.
  data: lt_item  like it_recitem occurs 0 with header line.

  CLEAR: IT_RECITEM, IT_RECITEM[],
         IT_HEAD, IT_HEAD[].

*   reading the items for this reclaim
  select * into table it_rec
    from ztsd_rec_h
    where zissn  = p_wa-zissn
     and  zvend  = p_wa-zvend
     and  zcsts  = 'AA'.
  if sy-subrc ne 0.
    message e000 with 'Reclaim data is wrong, Please check'.
  endif.
*   read the condition
  clear: it_recitem, it_recitem[].
  loop at it_rec.
    move-corresponding it_rec to it_recitem.
*     get the condtions
    select single zconw zconc matnr
      into (it_recitem-zconw, it_recitem-zconc, it_recitem-matnr)
      from ztsd_vin_conv
      where zmodl = it_rec-zmodl.
    if sy-subrc ne 0.
      message e000 with 'No condition type for model:' it_rec-zmodl.
    endif.

    append it_recitem.

  endloop.
* SUMMARIZE THE ITEMS
  lt_item[] = it_recitem[].
  clear: it_recitem[].
  loop at lt_item.
    if lt_item-zctyp ne 'C'.
      lt_item-zctyp = 'W'.
    endif.
    collect lt_item into it_recitem.
    it_head-zissn = lt_item-zissn.
    it_head-zvend = lt_item-zvend.
    it_head-zmodl = lt_item-zmodl.
    it_head-matnr = lt_item-matnr.
    collect it_head. clear it_head.
  endloop.
endform.                    " get_recliam_items
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
                             p_count.

   CONCATENATE 'RV45A-MABNR(' P_COUNT ')' INTO P_MAT_FIELD.

   CONCATENATE 'VBAP-ZMENG(' P_COUNT ')' INTO P_QTY_FIELD.


endform.                    " GET_FIELD_NAME
