*----------------------------------------------------------------------*
*   INCLUDE ZACO08L_F001                                               *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  CHK_INPUT_VALUE
*&---------------------------------------------------------------------*
*       Check Input Values
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form chk_input_value.
* Check Input Value (Period)
  if p_frper > p_toper.
    message e003(zmco) with p_frper p_toper.
  endif.

  if p_frper < 0 or p_frper > 12.
    message e007(zmco) with p_frper .
  endif.

  if p_toper < 0 or p_toper > 12.
    message e007(zmco) with p_toper.
  endif.

* Check TEST-RUN  Flag
  if p_trun na 'X '.
    message e008(zmco).
  endif.

* Check Currency IND.
  if p_currt na 'CTO'.
    message e000(zmco) with p_currt ' is not a posible value' .
  endif.

endform.                    " CHK_INPUT_VALUE

*&---------------------------------------------------------------------*
*&      Form  CAL_PER_COUNT
*&---------------------------------------------------------------------*
*       Calculation STD. - period Counter
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form cal_per_count.
* Cal. the Counter
  gv_percount = p_toper - p_frper + 1.

endform.                    " CAL_PER_COUNT

*&---------------------------------------------------------------------*
*&      Form  READ_VARIABLE_MASTER
*&---------------------------------------------------------------------*
*       Read data from ZTCO_VRRATIO and ZTCO_VRCCATCE
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_variable_master.

  clear : it_ztco_vrratio ,  it_ztco_vrratio[],
          it_costcenterlist, it_costcenterlist[],
          it_return,         it_return[].

* Get Valid CCtr Only FOR GV_CCGR_SETID - 'HMMA1'
  data : l_datum like sy-datum.
  concatenate p_gjahr p_frper+1(2) '01' into l_datum.
  call function 'BAPI_COSTCENTER_GETLIST1'
       exporting
            controllingarea = p_kokrs
            date_from       = l_datum
            costcentergroup = gv_ccgr_setid
       tables
            costcenterlist  = it_costcenterlist
            return          = it_return.
* Message
  perform dis_bapi_message.

* Selection data from ZTCO_VRCCATCE
* Only for data in table ZTCO_VRCCATCE
  select *  into  corresponding fields of table it_ztco_vrratio
            from  ztco_vrccatce
            for all entries in it_costcenterlist
           where  kokrs = p_kokrs
             and  kostl = it_costcenterlist-costcenter
             and  datbi_kostl >= sy-datum
             and  datbi_kstar >= sy-datum
             and  datbi_lstar >= sy-datum.
  if it_ztco_vrratio[] is initial.
    message e004(zmco) with 'ZTCO_VRCCATCE'.
  endif.

  sort it_ztco_vrratio by kostl kstar.
  clear it_ztco_vrratio .

* Selection data from ZTCO_VRRATIO
* The number of data in table 'ZTCO_VRRATIO' is much smaller than that
* in table 'ZTCO_VRCCATCE' - using "select single" query
* Do NOT Join 'ZTCO_VRCCATCE' and 'ZTCO_VRRATIO' because CCtr can be
* grouped together to the mother CCtr group.
  loop at it_ztco_vrratio.
    select  single
                    gjahr  name_coall  vrratio
              into  (it_ztco_vrratio-gjahr, it_ztco_vrratio-name_coall,
                     it_ztco_vrratio-vrratio)
              from  ztco_vrratio
             where  gjahr = p_gjahr
               and  kostl = it_ztco_vrratio-kostl
               and  kstar = it_ztco_vrratio-kstar.
* No data found ->
* Variable rate from Read CCtr group + SPACE(CCtr) + CE(cost element)
    if sy-subrc <> 0.
* Finding Parents CCtr group.
      perform read_cctrgrp_fr_cctr using it_ztco_vrratio-kostl
                                         it_ztco_vrratio-name_coall.

      select  single
                      gjahr  name_coall  vrratio
               into  (it_ztco_vrratio-gjahr, it_ztco_vrratio-name_coall,
                      it_ztco_vrratio-vrratio)
                from  ztco_vrratio
               where  gjahr      = p_gjahr
                 and  name_coall = it_ztco_vrratio-name_coall
                 and  kostl      = space
                 and  kstar      = it_ztco_vrratio-kstar.
** No data found Again? -> remove the record
*      IF SY-SUBRC <> 0.
*        DELETE IT_ZTCO_VRRATIO.
*        CONTINUE.
*      ENDIF.
    endif.
** If variable rate = 0.00, remove the record off
*    IF IT_ZTCO_VRRATIO-VRRATIO IS INITIAL.
*      DELETE IT_ZTCO_VRRATIO.
*      CONTINUE.
*    ENDIF.
* Modify DATA
    modify it_ztco_vrratio.
    clear it_ztco_vrratio.
  endloop.

  sort it_ztco_vrratio by name_coall kostl lstar kstar.
  clear it_ztco_vrratio.

* Check relationship between CCtr and CCtr grp
  data : lv_name_coall like it_ztco_vrratio-name_coall.
  loop at it_ztco_vrratio.
* Finding Parents CCtr group.
    clear lv_name_coall.
    perform read_cctrgrp_fr_cctr using it_ztco_vrratio-kostl
                                       lv_name_coall.
    if it_ztco_vrratio-name_coall ne lv_name_coall.
      message e005(zmco) with it_ztco_vrratio-kostl
                              it_ztco_vrratio-name_coall
                              'ZTCO_VRCCATCE'
                              lv_name_coall.
    endif.

* Get Object Key (without Activity Type)
    call function 'K_KOSTL_OBJECT_KEY_GET'
         exporting
              kokrs = p_kokrs
              kostl = it_ztco_vrratio-kostl
         importing
              objnr = it_ztco_vrratio-objnr.

    modify it_ztco_vrratio.
    clear it_ztco_vrratio.
  endloop.

  clear it_ztco_vrratio.

endform.                    " READ_VARIABLE_MASTER

*&---------------------------------------------------------------------*
*&      Form  DIS_BAPI_MESSAGE
*&---------------------------------------------------------------------*
*       Display BAPI Message
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form dis_bapi_message.
  if not it_return[] is initial.
    loop at   it_return.
      message id     it_return-id
              type   it_return-type
              number it_return-number
              with   it_return-message_v1
                     it_return-message_v2
                     it_return-message_v3
                     it_return-message_v4.
    endloop.
  endif.
endform.                    " DIS_BAPI_MESSAGE

*&---------------------------------------------------------------------*
*&      Form  READ_CCTRGRP_FR_CCTR
*&---------------------------------------------------------------------*
*       Read Cost center group name - parents group
*----------------------------------------------------------------------*
*      -->P_KOSTL      : CCtr
*      <--P_NAME_COALL : CCtr Group Name
*----------------------------------------------------------------------*
form read_cctrgrp_fr_cctr using    p_kostl
                                   p_name_coall.
* Only One Record
  loop at it_values where vfrom =< p_kostl
                      and vto   => p_kostl.
  endloop.

* GET CCtr group Name
  if sy-subrc = 0.
    call function 'G_SET_DECRYPT_SETID'
      exporting
         setid            = it_values-setid
      importing
*        SETCLASS         =
         shortname        = p_name_coall
*        KOKRS            =
*        KTOPL            =
*        SEARCHFLD        =
*        LIB              =
*        RNAME            =
*        ECCS_DIMEN       =
*        ECCS_ITCLG       =
*        ECCS_SITYP       =
              .
  endif.

endform.                    " READ_CCTRGRP_FR_CCTR

*&---------------------------------------------------------------------*
*&      Form  READ_DATA_FROM_COSP
*&---------------------------------------------------------------------*
*       read data (without Activity type) from COSP
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_data_from_cosp.

* Read Dynamic Fields Name
  perform read_field_name_from_dd.

* Read DATA from COSP (Posted DATA Directly)
  perform select_fr_cosp_wo_at.


endform.                    " READ_DATA_FROM_COSP

*&---------------------------------------------------------------------*
*&      Form  read_field_name_from_DD
*&---------------------------------------------------------------------*
*       Read Technical FieldName
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_field_name_from_dd.

  clear : it_et_fieldlist, it_et_fieldlist[].
  call function 'RECP_DD_TABL_FIELDNAMES_GET'
       exporting
            ic_tabname   = c_ic_tabname
       tables
            et_fieldlist = it_et_fieldlist
       exceptions
            not_found    = 1
            others       = 2.

  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

endform.                    " read_field_name_from_DD

*&---------------------------------------------------------------------*
*&      Form  SELECT_FR_COSP_WO_AT
*&---------------------------------------------------------------------*
*       Select DATA (Amount) w/o AT
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form select_fr_cosp_wo_at.

  clear it_ztco_vrratio.

* Value Type = '01' NOT '10'.
* Ledger : standard ledger '00'.
* Data Restriction : Only from The Objnr in ITAB 'IT_ZTCO_VRRATIO'
* Index  : Table Key Index
  clear : it_cosp, it_cosp[].
  clear cosp.
  select (it_et_fieldlist)
         into corresponding fields of it_cosp
         from cosp
          for all entries in it_ztco_vrratio
        where lednr = '00'
          and objnr = it_ztco_vrratio-objnr
          and gjahr = p_gjahr
          and wrttp = '01'
          and versn = p_versn
          and kstar = it_ztco_vrratio-kstar.

    clear: it_cosp-beknz, it_cosp-vrgng.
    collect it_cosp.
  endselect.

  clear it_cosp.


  loop at it_cosp.
    if  it_cosp-wkf001 = 0
    and it_cosp-wkf002 = 0
    and it_cosp-wkf003 = 0
    and it_cosp-wkf004 = 0
    and it_cosp-wkf005 = 0
    and it_cosp-wkf006 = 0
    and it_cosp-wkf007 = 0
    and it_cosp-wkf008 = 0
    and it_cosp-wkf009 = 0
    and it_cosp-wkf010 = 0
    and it_cosp-wkf011 = 0
    and it_cosp-wkf012 = 0.

      delete it_cosp.
    endif.

  endloop.



endform.                    " SELECT_FR_COSP_WO_AT

*&---------------------------------------------------------------------*
*&      Form  BUILD_POST_DATA
*&---------------------------------------------------------------------*
*       To post Variable Amount (POST data)
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form build_post_data.

  sort it_cosp           by objnr kostl lstar kstar .
  sort it_ztco_vrratio   by objnr kostl lstar kstar .

* FOR Report (ITAB)
  clear : it_list, it_list[].

* Can NOT be assigned different Ativity Type in
* CCtr + CE Key Combination
  field-symbols: <fs1> type any, <fs2> type any.
  data : lv_cosp_wkf(30).  " VALUE 'IT_COSP-'.
  data : lv_cosp_var(30).  " VALUE 'IT_COSP-'.
  data : lv_cnt  like  cosp-perbl.

  loop at it_cosp .
    clear it_ztco_vrratio.
    read table it_ztco_vrratio with key objnr = it_cosp-objnr
                                        kstar = it_cosp-kstar.
* Check Ratio DATA
    if sy-subrc <> 0.
      message e006(zmco) with it_cosp-objnr it_cosp-kstar.
    endif.

* For report (Key)
    it_list-kostl      =  it_cosp-kostl      =  it_ztco_vrratio-kostl.
    it_list-lstar      =  it_cosp-lstar      =  it_ztco_vrratio-lstar.
    it_list-vrratio    =  it_cosp-vrratio    =  it_ztco_vrratio-vrratio.
    it_list-name_coall =  it_cosp-name_coall
                       =  it_ztco_vrratio-name_coall.
    it_list-kstar      =  it_cosp-kstar.

* Period Counter : Set From-Period .
    clear lv_cnt.
    lv_cnt = p_frper .

* Calculating Variable Amount
    do gv_percount times.

      clear lv_cosp_wkf.
      concatenate 'IT_COSP-'  gv_fieldgroup_wkf  lv_cnt
             into lv_cosp_wkf.
      assign (lv_cosp_wkf) to <fs1>.

      clear lv_cosp_var.
      concatenate 'IT_COSP-'  gv_fieldgroup_var  lv_cnt
             into lv_cosp_var.
      assign (lv_cosp_var) to <fs2>.

* For report (Original Fixed Cost - SUM)
      it_list-org_val = it_list-org_val + <fs1>.

* Main CAL. Logic (with considering fractions from decimal point)
      <fs2> = <fs1> * it_ztco_vrratio-vrratio.
      <fs1> = <fs1> - <fs2>.

* For report (Changed Fixed-Cost - SUM)
      it_list-fix_val = it_list-fix_val + <fs1>.
* For report (Changed Variable Cost - SUM)
      it_list-var_val = it_list-var_val + <fs2>.

* Period Counter
      lv_cnt = lv_cnt + 1.
    enddo.

    modify it_cosp.
    clear  it_cosp.

* For report (Diff. Amount - SUM)
    it_list-dif_val = it_list-org_val
                    - it_list-fix_val - it_list-var_val .

* For report
    append it_list.
    clear  it_list.

  endloop.

  clear  it_cosp.
  clear  it_list.

endform.                    " BUILD_POST_DATA

*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_INIT
*&---------------------------------------------------------------------*
*       Building Field Cat.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form fieldcat_init.
  clear : gv_col_pos, it_fieldcat, it_fieldcat[].

  bulid_fieldcat  'IT_LIST'     'NAME_COALL'
                  'X'           'X'      space      space      '15'
                  'Group Name'           space.

  bulid_fieldcat  'IT_LIST'     'KOSTL'
                  'X'           'X'      space      space      '10'
                  'Cost Center'          space.

  bulid_fieldcat  'IT_LIST'     'LSTAR'
                  'X'           'X'      space      space      '6'
                  'Activity Type'        space.

  bulid_fieldcat  'IT_LIST'     'KSTAR'
                  'X'           'X'      space      space      '10'
                  'Cost element'         space.


  bulid_fieldcat  'IT_LIST'     'VRRATIO'
                  space         space    space      space      '6'
                  'Variable Ratio'       space.

  bulid_fieldcat  'IT_LIST'     'ORG_VAL'
                  space         space    'WAERS'  'IT_LIST'    '20'
                  'Org. Fixed Amt.'      'CURR'.

  bulid_fieldcat  'IT_LIST'     'FIX_VAL'
                  space         space    'WAERS'  'IT_LIST'    '20'
                  'Changed Fixed Amt.'   'CURR'.

  bulid_fieldcat  'IT_LIST'     'VAR_VAL'
                  space         space    'WAERS'  'IT_LIST'    '20'
                  'Changed Var. Amt.'    'CURR'.

  bulid_fieldcat  'IT_LIST'     'DIF_VAL'
                  space         space    'WAERS'  'IT_LIST'    '20'
                  'Diff. Amount.'        'CURR'.

  bulid_fieldcat  'IT_LIST'     'WAERS'
                  space         space    space     space       '4'
                  'CURR'                 'CUKY'.

endform.                    " FIELDCAT_INIT

*&---------------------------------------------------------------------*
*&      Form  PRE_LIST_DATA
*&---------------------------------------------------------------------*
*       The Preparation for ALV List
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form pre_list_data.
* Read CO area Currency
  clear it_list.
  select single waers into it_list-waers
                     from tka01
                    where kokrs = p_kokrs.
* ALL Curr data are base on Controlling Area Currency.
* Change Currency fields with Controlling Area Currency.
  modify it_list  transporting waers where waers eq space.

* Sort IT_LIST.
  sort it_list by name_coall kostl lstar  kstar .
  clear it_list.

* Set Event
  data : wa_l_event  type slis_alv_event.
  wa_l_event-name = slis_ev_top_of_page.
  wa_l_event-form = 'BASIC_TOP_OF_PAGE'.
  append wa_l_event to it_events.
endform.                    " PRE_LIST_DATA

*-----------------------------------------------------------------------
*    FORM PF_STATUS_VAR
*-----------------------------------------------------------------------
form pf_status_var using  extab type slis_t_extab.
  set pf-status 'BALVLIST' excluding extab.
endform.

*---------------------------------------------------------------------*
*       FORM USER_COMMAND                                             *
*---------------------------------------------------------------------*
*       For User_command - AT User Command                            *
*---------------------------------------------------------------------*
*  -->  UCOMM                                                         *
*  -->  SELFIELD                                                      *
*---------------------------------------------------------------------*
form user_command using ucomm    like sy-ucomm
                        selfield type slis_selfield.
  case ucomm.
* Important part !
* POST PLAN data (CCtr+AT+CE)
    when 'POST' or 'REVS'.
      perform post_pl_cctr_at_ce  using ucomm.
  endcase.

endform.

*---------------------------------------------------------------------*
*       FORM BASIC_TOP_OF_PAGE                                        *
*---------------------------------------------------------------------*
*       Display Title                                                 *
*---------------------------------------------------------------------*
form basic_top_of_page.

  write : / 'Controlling Area ', p_kokrs.
  write : / 'Fiscal Year      ', p_gjahr, '   Period ', p_frper,
            ' ~ ', p_toper.
  write : / 'Version          ', p_versn.
  write : / 'Test Run         ', p_trun.
  skip 1.

endform.

*&---------------------------------------------------------------------*
*&      Form  CALL_ALV_LIST
*&---------------------------------------------------------------------*
*       CALL ALV LIST
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form call_alv_list.
  call function 'REUSE_ALV_LIST_DISPLAY'
    exporting
*     I_INTERFACE_CHECK              = ' '
*     I_BYPASSING_BUFFER             =
*     I_BUFFER_ACTIVE                = ' '
      i_callback_program             = gv_repid
      i_callback_pf_status_set       = gv_status
      i_callback_user_command        = gv_user_command
*     I_STRUCTURE_NAME               =
*     IS_LAYOUT                      =
      it_fieldcat                    = it_fieldcat[]
*     IT_EXCLUDING                   =
*     IT_SPECIAL_GROUPS              =
*     IT_SORT                        =
*     IT_FILTER                      =
*     IS_SEL_HIDE                    =
*     I_DEFAULT                      = 'X'
      i_save                         = 'A'
*     IS_VARIANT                     =
      it_events                      = it_events
      it_event_exit                  = it_event_exit
*     IS_PRINT                       =
*     IS_REPREP_ID                   =
*     I_SCREEN_START_COLUMN          = 0
*     I_SCREEN_START_LINE            = 0
*     I_SCREEN_END_COLUMN            = 0
*     I_SCREEN_END_LINE              = 0
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER        =
*     ES_EXIT_CAUSED_BY_USER         =
    tables
      t_outtab                       = it_list
    exceptions
      program_error                  = 1
      others                         = 2.

  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

endform.                    " CALL_ALV_LIST

*&---------------------------------------------------------------------*
*&      Form  POST_PL_CCTR_AT_CE
*&---------------------------------------------------------------------*
*       POST PLAN DATA using BAPI FM
*----------------------------------------------------------------------*
*  -->  P_Ucomm        Post/Reverse
*----------------------------------------------------------------------*
form post_pl_cctr_at_ce using p_ucomm.

* Init. Message TAB
  clear : it_return, it_return[].

* Fill Header DATA
  clear wa_headerinfo.
  wa_headerinfo-co_area        = p_kokrs.
  wa_headerinfo-fisc_year      = p_gjahr.
  wa_headerinfo-period_from	 = p_frper.
  wa_headerinfo-period_to	 = p_toper.
  wa_headerinfo-version        = p_versn.
* WA_HEADERINFO-DOC_HDR_TX	 =
  wa_headerinfo-plan_currtype	 = p_currt.

* Fill Object List and Plan Values per Period
  clear : it_indexstructure, it_indexstructure[].
  clear : it_coobject,       it_coobject[].
  clear : it_pervalue,       it_pervalue[].
  clear : it_totvalue,       it_totvalue[].

* Sort to post data.
  sort it_cosp by kostl lstar kstar.

* by ig.moon 12/04 {
  data $flag(1).
* }

  loop at it_cosp.

*by ig.moon 12/04 {
    at new lstar.
      $flag = 'X'.
    endat.

    if $flag eq 'X'.
      clear $flag.
      it_indexstructure-object_index
           = it_indexstructure-object_index + 1 .

      clear it_coobject.
      it_coobject-object_index = it_indexstructure-object_index.

      it_coobject-costcenter   = it_cosp-kostl.
      it_coobject-acttype      = it_cosp-lstar.
      append it_coobject.
      clear  it_coobject.
    endif.

*** Obj
**    ON CHANGE OF  IT_COSP-KOSTL
**              OR  IT_COSP-LSTAR .
*** Index of Object Key
**      IT_INDEXSTRUCTURE-OBJECT_INDEX
**           = IT_INDEXSTRUCTURE-OBJECT_INDEX + 1 .
**
**      CLEAR IT_COOBJECT.
**      IT_COOBJECT-OBJECT_INDEX = IT_INDEXSTRUCTURE-OBJECT_INDEX.
**
**      IT_COOBJECT-COSTCENTER   = IT_COSP-KOSTL.
**      IT_COOBJECT-ACTTYPE      = IT_COSP-LSTAR.
**      APPEND IT_COOBJECT.
**      CLEAR  IT_COOBJECT.
**    ENDON.
* }

* Value.
* Index of Value
    it_indexstructure-value_index
         = it_indexstructure-value_index + 1.

    clear it_pervalue.
    it_pervalue-value_index = it_indexstructure-value_index.
    it_pervalue-cost_elem   = it_cosp-kstar.
* Set Value

* Post
    perform set_value_amt.

    append it_pervalue.
    clear  it_pervalue.

* append Index
    append it_indexstructure.
    clear it_cosp.
  endloop.

* by ig.moon 12/04 {
  clear it_indexstructure.
* }

* Call BAPI FM
  perform call_post_fm.

* The data without Activity Type should be removed
  perform call_fm_for_clearing.


* Commit
  if p_trun = 'X'.
  else.
    commit work.
    message s009(zmco) with p_ucomm.
  endif.

endform.                    " POST_PL_CCTR_AT_CE

*&---------------------------------------------------------------------*
*&      Form  SET_VALUE_AMT
*&---------------------------------------------------------------------*
*       Fill value
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_value_amt.
* Fixed Cost
* Variable Cost
  trans_value 01. trans_value 02. trans_value 03. trans_value 04.
  trans_value 05. trans_value 06. trans_value 07. trans_value 08.
  trans_value 09. trans_value 10. trans_value 11. trans_value 12.
  trans_value 13. trans_value 14. trans_value 15. trans_value 16.

endform.                    " SET_VALUE_AMT

*&---------------------------------------------------------------------*
*&      Form  CALL_POST_FM
*&---------------------------------------------------------------------*
*       POSTING
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form call_post_fm.

  call function 'BAPI_COSTACTPLN_POSTPRIMCOST'
    exporting
      headerinfo           = wa_headerinfo
      delta                = 'X'
    tables
      indexstructure       = it_indexstructure
      coobject             = it_coobject
      pervalue             = it_pervalue
*     TOTVALUE             =
*     CONTRL               =
      return               = it_return.

* Check error
  clear  it_return.
  loop at it_return  where type ca 'AE'.
    message id     it_return-id
            type   it_return-type
            number it_return-number
            with   it_return-message_v1
                   it_return-message_v2
                   it_return-message_v3
                   it_return-message_v4.
    clear it_return.
  endloop.

endform.                    " CALL_POST_FM

*&---------------------------------------------------------------------*
*&      Form  CALL_FM_FOR_CLEARING
*&---------------------------------------------------------------------*
*       Clearing data with AT
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form call_fm_for_clearing.
* The data without Activity Type should be removed
* - (Request by Dong Whan Kim 2003.11.05)

* Init. Message TAB
  clear : it_return, it_return[].

* Local Data Definition
  data : it_l_cl_onj like standard table of  it_coobject
                     with header line .
  data : it_l_cl_per like standard table of  it_pervalue
                     with header line .

  define trans_value_nega.
    it_l_cl_per-fix_val_per&1  =  ( it_pervalue-fix_val_per&1 +
                                    it_pervalue-var_val_per&1 ) * -1.
  end-of-definition.


* Remove AT
  it_l_cl_onj[] = it_coobject[].
  clear it_l_cl_onj-acttype .
  modify it_l_cl_onj transporting acttype where acttype ne space.
  clear it_l_cl_onj.

* Remove Values
  clear : it_l_cl_per, it_l_cl_per[].
  loop at it_pervalue.
    it_l_cl_per-value_index  =    it_pervalue-value_index .
    it_l_cl_per-cost_elem    =    it_pervalue-cost_elem   .

    trans_value_nega:01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16.

    append it_l_cl_per.
    clear  it_l_cl_per.
    clear  it_pervalue.
  endloop.

  call function 'BAPI_COSTACTPLN_POSTPRIMCOST'
    exporting
      headerinfo           = wa_headerinfo
      delta                = 'X'
    tables
      indexstructure       = it_indexstructure
      coobject             = it_l_cl_onj    "IT_COOBJECT
      pervalue             = it_l_cl_per    "IT_PERVALUE
*     TOTVALUE             =
*     CONTRL               =
      return               = it_return.

* Check error
  clear  it_return.
  loop at it_return  where type ca 'AE'.
    message id     it_return-id
            type   it_return-type
            number it_return-number
            with   it_return-message_v1
                   it_return-message_v2
                   it_return-message_v3
                   it_return-message_v4.
    clear it_return.
  endloop.

endform.                    " CALL_FM_FOR_CLEARING
