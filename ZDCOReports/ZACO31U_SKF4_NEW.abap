************************************************************************
* Program Name      : ZACO31U_SKF4
* Author            : Eun Hwa , Jung
* Creation Date     : 2003.10.17.
* Specifications By : Dong-Hwan Kim
* Pattern           : Report 1-1
* Development Request No : UD1K903011
* Addl Documentation:
* Description       : Create No of Persons by Department(Actual)

* the BDC structures for BATCH INPUT processing
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

report  zaco31u_skf4 message-id zmco.

** type-pools
type-pools: slis.

tables : csks, pa0001, pa0000.   ", ZTCO_SKF_MANAGE.

* Constants
constants: c_skf1 type stagr value 'CS001',
           c_skf2 type stagr value 'CS010',
           c_skf3 type stagr value 'CS011'.



* Main internal table
*data: begin of it_pa0001 occurs 0,
*        kostl like pa0001-kostl,
*        count type i,
**-- tEST
*        pernr like pa0001-pernr,
**        DATE  LIKE COHEADER-BUDAT,
**        STAGR LIKE TKA03-STAGR,
**        PERNR LIKE PA0001-PERNR,
*      end of it_pa0001.

**   end date setting
data : first_day type sy-datum,
       last_day type sy-datum.


data: begin of it_post occurs 0,
        kostl   type kostl,
        count   type i,
        anzhl   like ztco_mha-anzhl,
        rate    type p decimals 3,
      end of it_post.

data: begin of it_cs001 occurs 0,
        cgroup(15),
        count type p decimals 4,
      end of it_cs001.

data : it_cs010 like it_cs001 occurs 0 with header line.
data : it_cs011 like it_cs001 occurs 0 with header line.


data: begin of it_mha occurs 0,
        kostl like ztco_mha-kostl,
        anzhl like ztco_mha-anzhl,
      end of  it_mha.

data: begin of it_admin occurs 0,
        kostl like ztco_mha-kostl,
        anzhl like ztco_mha-anzhl,
      end of it_admin.

data : g_total  like ztco_mha-anzhl,
       g_non_mh like ztco_mha-anzhl,
       g_rate   type p decimals 4,
       p_cc_grp(2)  value 'MH'.

ranges : r_admin   for  ztco_mha-kostl,
         r_kostl   for  ztco_mha-kostl.

** For BAPI
*DATA : it_costcenterlist LIKE STANDARD TABLE OF bapi1112_values
*                         WITH HEADER LINE.
data : it_costcenterlist like standard table of bapi0012_cclist
                         with header line.
data : it_nodes like standard table of bapiset_hier
                         with header line.
data : it_return         like standard table of bapiret2
                         with header line.
data : wa_doc_header like bapidochdrp .
data : it_doc_items  like standard table of bapiskfitm
                     with header line.

data: begin of it_low_cost occurs 0,
        cgroup(15),
        kostl(15),
      end of it_low_cost.

** For ALV
data : gv_repid like sy-repid.
data : gv_status       type slis_formname value 'PF_STATUS'.
data : gv_user_command type slis_formname value 'USER_COMMAND'.
data : it_sort         type slis_t_sortinfo_alv with header line .
data : gv_col_pos type i.
data : it_fieldcat          type slis_t_fieldcat_alv,
       wa_fieldcat          like line of it_fieldcat,
       it_eventcat          type slis_t_event,
       wa_eventcat          like line of it_eventcat.
data : it_events	          type slis_t_event,
       it_event_exit	    type slis_t_event_exit.

* Global Variant
data : gv_post_date like coheader-budat.

*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
selection-screen begin of block bl1 with frame title text-001.
parameters :
               p_kokrs like csks-kokrs memory id cac obligatory,
               p_gjahr like cobk-gjahr memory id gjr obligatory,
               p_perid like cosp-perbl memory id bpe obligatory,
               p_versn like cobk-versn memory id kvt obligatory,
               p_trun(1).

selection-screen skip 1.

selection-screen begin of block bl3 with frame title text-003.
select-options : s_kostl  for csks-kostl.
parameters:      p_ncoal like grpdynp-name_coall
                                   default 'HMMA1'.
selection-screen end of block bl3.
selection-screen end of block bl1.

*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
initialization.
* Set Global ALV Parameter
  gv_repid = sy-repid.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
at selection-screen.
  perform chk_input_value.

* Searching for Cost Center group
at selection-screen on value-request for p_ncoal.
  perform read_cegrp_group using '0101'
                                 p_ncoal.


*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
start-of-selection.
* preparation posting date.
  perform cal_post_date.
* Read Cost Center Group - > low level -> Cost Center
  perform read_cegrp.
* Read HR master
  perform read_personnel_count.

* Read MHA data
  perform read_mh.
* Preparation of Posting
  perform pre_posting.
* Preparation of ALV
  perform pre_report_adj.

*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
end-of-selection.
* Call ALV LIST
  perform call_alv_list.

*&---------------------------------------------------------------------*
*&      Form  READ_CEGRP
*&---------------------------------------------------------------------*
*       read cost center
*----------------------------------------------------------------------*
form read_cegrp.
  data : lv_datum like sy-datum.

* Making an internal table for CCtr to select data
* Selected Group on screen
  clear : it_costcenterlist, it_costcenterlist[].
  clear : it_nodes , it_nodes[].
  clear : it_return, it_return[].

  concatenate p_gjahr p_perid+1(2) '01' into lv_datum.

* From CCtr Group
  if not p_ncoal is initial.
    call function 'BAPI_COSTCENTER_GETLIST1'
         exporting
              controllingarea = p_kokrs
              date_from       = lv_datum
              costcentergroup = p_ncoal
         tables
              costcenterlist  = it_costcenterlist
              return          = it_return.

* Message
    perform dis_bapi_message.
  endif.
* From CCtrs on selection screen
* From Select-options.
  data : it_l_cctr like standard table of it_costcenterlist
                 with header line.

  if not s_kostl[] is initial.
    loop at s_kostl.
      clear : it_l_cctr, it_l_cctr[].
      call function 'BAPI_COSTCENTER_GETLIST1'
           exporting
                controllingarea = p_kokrs
                date_from       = lv_datum
                costcenter_from = s_kostl-low
                costcenter_to   = s_kostl-high
           tables
                costcenterlist  = it_l_cctr
                return          = it_return.
* Message
      perform dis_bapi_message.
* Appending CE list
      append lines of it_l_cctr  to it_costcenterlist.
      clear it_costcenterlist.
      clear s_kostl.
    endloop.
  endif.


* Sorting
  sort it_costcenterlist by co_area costcenter.
  delete adjacent duplicates from it_costcenterlist.

* Check CCtr
  if it_costcenterlist[] is initial .
    message e018(zmco) with 'Cost Center'.
  endif.

  loop at it_costcenterlist.
    it_low_cost-kostl = it_costcenterlist-costcenter.
    append it_low_cost.
  endloop.
*
*  DATA : COUNT TYPE I.
*  CLEAR : IT_LOW_COST, IT_LOW_COST[].
*
*  LOOP AT IT_NODES.
*    CLEAR COUNT.
*    COUNT = IT_NODES-VALCOUNT.
*    DO COUNT TIMES.
*      IT_LOW_COST-CGROUP = IT_NODES-GROUPNAME.
*      APPEND IT_LOW_COST.
*    ENDDO.
*  ENDLOOP.
*
*  CLEAR IT_LOW_COST.
*  LOOP AT IT_LOW_COST.
*    CLEAR IT_COSTCENTERLIST.
*    READ TABLE IT_COSTCENTERLIST INDEX SY-TABIX.
*
*    IT_LOW_COST-KOSTL = IT_COSTCENTERLIST-VALFROM.
*    MODIFY IT_LOW_COST.
*
*  ENDLOOP.



endform.                    " READ_CEGRP
*&---------------------------------------------------------------------*
*&      Form  DIS_BAPI_MESSAGE
*&---------------------------------------------------------------------*
*        Display BAPI Message
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
*&      Form  READ_CEGRP_GROUP
*&---------------------------------------------------------------------*
*       Search Help for Cost element Group / CCTr Group
*----------------------------------------------------------------------*
*  -->  p_class      Class Name
*  <--  P_SET_NAME   Result Group Name
*----------------------------------------------------------------------*
form read_cegrp_group using   p_class
                               p_set_name.

  call function 'K_GROUP_SELECT'
    exporting
*     BUTTONS                  = 'X'
      class                    = p_class
*     CRUSER                   = '*'
      field_name               = space
*     SEARCHFLD                = '    '
*     SEARCHFLD_INPUT          = 'X'
      searchfld_required       = 'X'
*     SET                      = '*'
*     START_COLUMN             = 10
*     START_ROW                = 5
*     TABLE                    = 'CCSS'
*     TYPELIST                 = 'BS'
*     UPDUSER                  = '*'
*     KOKRS                    =
*     KTOPL                    =
    importing
*     CLASS_NAME               =
      set_name                 = p_set_name
*     SET_TITLE                =
*     TABLE_NAME               =
*     SETID                    =
    exceptions
      no_set_picked            = 1
      others                   = 2.

* No error check for F4  SH
  if sy-subrc <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.


endform.                    " READ_CEGRP_GROUP
*&---------------------------------------------------------------------*
*&      Form  READ_PERSONNEL_COUNT
*&---------------------------------------------------------------------*
*       READ_HR_MASTER
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_personnel_count.

* Sorting
*  SORT it_costcenterlist BY valfrom valto.
  sort it_costcenterlist by costcenter.

  clear :  first_day, last_day.

  concatenate p_gjahr p_perid+1(2) '01' into first_day.

  call function 'LAST_DAY_OF_MONTHS'
       exporting
            day_in            = first_day
       importing
            last_day_of_month = last_day.

  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
        with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.


** Obsoleted 2004.07.14
***// Begin of Mod. By Hyung Jin Youn 2004.07.09
*** Only for EE group = '1' or '9'
*  CLEAR : IT_PA0001, IT_PA0001[].
*  CLEAR   PA0001.
*  LOOP AT IT_COSTCENTERLIST.
*    SELECT KOSTL COUNT( DISTINCT PERNR )
*         APPENDING TABLE IT_PA0001
*         FROM PA0001
*          WHERE  ENDDA >= LAST_DAY             " '99991231'
*           AND   KOSTL = IT_COSTCENTERLIST-VALFROM
*           AND   KOKRS = P_KOKRS
*           AND   PERSG IN ('1', '9')
*             GROUP BY KOSTL.
*  ENDLOOP.
*  CLEAR IT_PA0001.
*
*** End of Mod.

**// Begin of Mod. By Hyung Jin Youn 2004.07.14
*    Change of condition(where clause)
*  clear : it_pa0001, it_pa0001[].
*  clear   pa0001.
*  data: begin of it_per occurs 0,
*          pernr type pernr,
*          kostl type kostl,
*        end of it_per.
*  loop at it_costcenterlist.
*    select pernr into table it_per
*        from pa0000
*         where  ( begda <= last_day and endda >= last_day )
*           and  stat2 = '3'.    "'Active
*
*
*    select kostl count( distinct pernr )
*         appending table it_pa0001
*        from pa0001
*         for all entries in it_per
*         where  pernr = it_per
*          and   kostl = it_costcenterlist-costcenter
*          and   kokrs = p_kokrs
*          and   stat2 = '3' "'Active
*          and  ( begda <= last_day and endda >= last_day )
*         group by kostl.
*
*  endloop.
*
*  clear it_pa0001.

** End of Mod.

  select kostl sum( emp_cnt )
     into table it_post
     from ztco_mha
     where kokrs       =  p_kokrs
       and gjahr       =  p_gjahr
       and perid       =  p_perid
     group by kostl.
  if sy-subrc <> 0.
    message e000 with 'Run MH collection program first!'.
  endif.
endform.                    " READ_PERSONNEL_COUNT

*-----------------------------------------------------------------------
*    FORM PF_STATUS
*-----------------------------------------------------------------------
form pf_status using  extab type slis_t_extab.
  set pf-status 'LIST' excluding extab .
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
* POST PLAN data to STD
    when 'POST'.    " OR 'REVS'.
*      PERFORM post_std  USING ucomm.

*-- check previous posting
      data: l_belnr type co_belnr.
      select single a~belnr into l_belnr
           from cobk as a
              inner join coepr as b
                 on a~kokrs = b~kokrs
                and a~belnr = b~belnr
           where a~gjahr = p_gjahr
             and a~kokrs = p_kokrs
             and a~perab = p_perid
             and a~vrgng = 'RKS'
             and a~stokz = space
             and a~stflg = space
             and b~stagr = c_skf1.

      if sy-subrc = 0.
       message s000 with 'Already posted. Please reverse first(KB34N) '
                          l_belnr.
      else.
        perform posting_fm  using ucomm.
      endif.

  endcase.

endform.
*&---------------------------------------------------------------------*
*&      Form  POST_STD
*&---------------------------------------------------------------------*
*       Preparation posting
*----------------------------------------------------------------------*
*      -->P_UCOMM  text
*----------------------------------------------------------------------*
*form post_std using    p_ucomm.
*  data : lv_conf_text(50).
*
*  delete it_post where count eq '0'.
*
*  data  : line  type i.
*  clear : line.
*  describe table it_post   lines line.
*
*  if line = 0.
*    message e000(zmco) with
*    ' Enter Value not equal to 0 '.
*  endif.
*
*
** Init. Message TAB
*  clear : it_return, it_return[].
*
*** <  Posting , hard coding > **
** doc date / posting date   =  ' YEAR/MONTH/25 '
** SKF                        =  'CS001'
** TEXT
*  clear lv_conf_text.
*  concatenate sy-uname  sy-datum  sy-repid
*         into lv_conf_text
*         separated by '/'.
*
** Fill Header DATA
*  clear wa_doc_header.
*  wa_doc_header-co_area           = p_kokrs.
*  wa_doc_header-docdate           = gv_post_date.
*  wa_doc_header-postgdate         = gv_post_date.
*  wa_doc_header-version           = p_versn.
*  wa_doc_header-variant           = 'SAP01'.
*  wa_doc_header-doc_hdr_tx        = lv_conf_text.
*  wa_doc_header-username          = sy-uname.
*
** Fill Object List
*  clear : it_doc_items, it_doc_items[].
*  loop at it_post.
*    it_doc_items-statkeyfig = c_skf1.                       " 'CS001'
*    it_doc_items-stat_qty   = it_post-count.
***// Mod. By Hyung Jin Youn
** -> Alpha Numeric Conversion
**    IT_DOC_ITEMS-REC_CCTR   = IT_POST-CGROUP.
*    it_doc_items-rec_cctr   = it_post-kostl.
*    call function 'CONVERSION_EXIT_ALPHA_INPUT'
*         exporting
*              input  = it_doc_items-rec_cctr
*         importing
*              output = it_doc_items-rec_cctr.
***// End of Mod.
*    append it_doc_items.
*  endloop.
*
** Call BAPI FM
*  perform call_post_fm.
*
** Commit
*  if p_trun = 'X'.
*    read table it_return  index 1.
*    message s000(zmco) with it_return-message.
*  else.
*    commit work.
*    read table it_return  index 1.
*    message s000(zmco) with it_return-message.
**   MESSAGE S009(ZMCO) WITH P_UCOMM.
*
**       Consolidation management for the SKF
*    call function 'Z_FCO_MANAGEMENT_SKF'
*      exporting
*        im_pgmno         =   sy-tcode
*        im_kokrs         =   p_kokrs
*        im_gjahr         =   p_gjahr
*        im_perbl         =   p_perid
**           IM_PERBL_T       =
*        im_versn         =   p_versn
**           IM_KOSTL_F        =    S_KOSTL-LOW
**           IM_KOSTL_T        =    S_KOSTL-HIGH
*       im_gname          =    p_ncoal.
**           IM_PRZNR_F       =
**           IM_PRZNR_T       =
**         IMPORTING
**           SUBRC            =
*
*  endif.
*
*
*endform.                    " CALL_BDC
*&---------------------------------------------------------------------*
*&      Form  PRE_REPORT_ADJ
*&---------------------------------------------------------------------*
*       The Preparation for ALV List
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form pre_report_adj.

* Building Field Cat.
  perform fieldcat_init .

endform.                    " PRE_REPORT_ADJ
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_INIT
*&---------------------------------------------------------------------*
*       Building Field Cat
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form fieldcat_init.

  clear : gv_col_pos, it_fieldcat, it_fieldcat[].

*  PERFORM BUILD_FIELDCAT USING
*    'IT_PA0001'  'DATE'       'X'            SPACE    SPACE
*    SPACE        '15'      'Posting Date'   SPACE    SPACE      SPACE.

  perform build_fieldcat using
    'IT_POST' 'KOSTL'  'X'            space    space
    space        '15'      'Receiver CCtr'   space    space      space.

*  PERFORM BUILD_FIELDCAT USING
*    'IT_PA0001' 'STAGR'  SPACE            SPACE    SPACE
*    SPACE   '15'      'SKF'    SPACE    SPACE SPACE.

  perform build_fieldcat using
    'IT_POST' 'COUNT'  space            'X'      space
    space    '15'      'Headcount'      space    space space.

  perform build_fieldcat using
    'IT_POST' 'ANZHL'  space            'X'      space
    space    '15'      'Man hours'      space    space space.

  perform build_fieldcat using
    'IT_POST' 'RATE'  space            space    space
    space    '15'      'MH Excl%'      space    space space.


* Set Event
  data : wa_l_event  type slis_alv_event.
  wa_l_event-name = slis_ev_top_of_page.
  wa_l_event-form = 'BASIC_TOP_OF_PAGE'.
  append wa_l_event to it_events.


endform.                    " FIELDCAT_INIT
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0675   text
*      -->P_0676   text
*      -->P_0677   text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_0681   text
*      -->P_0682   text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*----------------------------------------------------------------------*
form build_fieldcat using   value(p_0100)
                             value(p_0101)
                             value(p_0102)
                             value(p_0103)
                             value(p_0104)
                             value(p_0105)
                             value(p_0106)
                             value(p_0107)
                             value(p_0108)
                             value(p_0109)
                             value(p_0110).

  add 1 to gv_col_pos.
  wa_fieldcat-tabname     = p_0100.
  wa_fieldcat-fieldname   = p_0101.
  wa_fieldcat-key         = p_0102.
  wa_fieldcat-do_sum      = p_0103.
  wa_fieldcat-cfieldname  = p_0104.
  wa_fieldcat-ctabname    = p_0105.
  wa_fieldcat-outputlen   = p_0106.
  wa_fieldcat-seltext_l   = p_0107.
  wa_fieldcat-datatype    = p_0108.
  wa_fieldcat-qfieldname  = p_0109.
  wa_fieldcat-qtabname    = p_0110.
  wa_fieldcat-col_pos     = gv_col_pos.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

endform.                    " BUILD_FIELDCAT
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
*       IT_SORT                        = IT_SORT[]        "
*     IT_FILTER                      =
*     IS_SEL_HIDE                    =
*     I_DEFAULT                      = 'X'
       i_save                         = 'A'
*     IS_VARIANT                     =
       it_events                      = it_events     "
       it_event_exit                  = it_event_exit   "
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
       t_outtab                       = it_post
       exceptions
       program_error                  = 1
       others                         = 2.

  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

endform.                    " CALL_ALV_LIST

*---------------------------------------------------------------------*
*       FORM BASIC_TOP_OF_PAGE                                        *
*---------------------------------------------------------------------*
*       Display Title                                                 *
*---------------------------------------------------------------------*
form basic_top_of_page.

  write : / 'Controlling Area      : ', p_kokrs .
  write : / 'Fiscal Year/Period    : '
            , p_gjahr, '/', p_perid.
  write : / 'Version               : ', p_versn .
  write : / 'Posting Date          : ', gv_post_date.
  write : / 'SKF                   : ', 'CS001-No.of Persons by Team.' .

* CCTR
  if p_ncoal ne space.
    write : / 'Cost Center Group     : ', p_ncoal.
  endif.

*  IF NOT S_KOSTL[] IS INITIAL.
*    LOOP AT S_KOSTL.
*      AT FIRST.
*        WRITE : / 'Cost Center           :     '.
*      ENDAT.
*      WRITE : / '                        ', S_KOSTL-LOW, '~',
*S_KOSTL-HIGH.
*    ENDLOOP.
*  ENDIF.

  write : / 'Test Run              : ', p_trun.
  skip 1.

endform.
*&---------------------------------------------------------------------*
*&      Form  CAL_POST_DATE
*&---------------------------------------------------------------------*
*       calculate posting date
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form cal_post_date.

  data  : perid like cosp-perbl.
  data  : gjahr like cobk-gjahr.
  clear : perid, gjahr, gv_post_date.

**// Begin of Mod. By Hyung Jin Youn  2004.07.09
* Change posting date to 25th of P_PERID and P_GJAHR.

*  IF P_PERID = '1'.
*    GJAHR = P_GJAHR - 1.
*    CONCATENATE  GJAHR  '12' '25'  INTO GV_POST_DATE.
*  ELSE.
*    PERID = P_PERID - 1.
*    CONCATENATE  P_GJAHR  PERID+1(2) '25'  INTO GV_POST_DATE.
*  ENDIF.

  concatenate  p_gjahr  p_perid+1(2) '25'  into gv_post_date.

**// End of Mod.

endform.                    " CAL_POST_DATE
*&---------------------------------------------------------------------*
*&      Form  CHK_INPUT_VALUE
*&---------------------------------------------------------------------*
*       CHK_INPUT_VALUE
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form chk_input_value.

  if p_perid < 0 or p_perid > 12.
    message e007(zmco) with p_perid .
  endif.

* Check TEST-RUN  Flag
  if p_trun na 'X '.
    message e008(zmco).
  endif.

* Check Cost Center/Cost Center Group
  if    " S_KOSTL[] IS INITIAL
     "AND
     p_ncoal   is initial .
    message e016(zmco).
*  ELSEIF
*         NOT S_KOSTL[] IS INITIAL
*     AND NOT P_NCOAL   IS INITIAL .
*    MESSAGE E017(ZMCO).
  endif.


endform.                    " CHK_INPUT_VALUE
*&---------------------------------------------------------------------*
*&      Form  CALL_POST_FM
*&---------------------------------------------------------------------*
*       CALL_POST_FM
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form call_post_fm.

  call function 'BAPI_ACC_STAT_KEY_FIG_POST'
       exporting
            doc_header = wa_doc_header
       tables
            doc_items  = it_doc_items
            return     = it_return.


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
*&      Form  PRE_POSTING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form pre_posting.

  sort it_post by kostl.

* For admin : add MH & MH exclusion%
  loop at it_post.
    clear it_admin.
    read table it_admin with key kostl = it_post-kostl.

    check sy-subrc = 0 .
    it_post-rate  = g_rate * 100.
    modify it_post. clear it_post.
  endloop.

endform.                    " PRE_POSTING
*&---------------------------------------------------------------------*
*&      Form  read_mh
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_mh.

  perform get_all_cc_group tables r_kostl
                           using  'MH'.

  perform get_all_cc_group tables r_admin
                           using  'MH_ADM'.

*  PERFORM get_cc_group(zaco92a_mh_roll_up) TABLES r_admin
*                                           USING  'ADM'.


* M/H by Cost Center
  select kostl sum( anzhl )
     into table it_mha
     from ztco_mha
     where kokrs       =  p_kokrs
       and gjahr       =  p_gjahr
       and perid       =  p_perid
       and lgart       in ('1', '2', '3' )
     group by kostl.

  data: l_idx type i.
  clear : g_total, g_non_mh, g_rate.
  loop at it_mha.
    read table it_post with key kostl = it_mha-kostl.
    l_idx = sy-tabix.
    if sy-subrc = 0.
      it_post-anzhl = it_mha-anzhl.
      modify it_post index l_idx transporting anzhl.
    endif.

    if it_mha-kostl in r_admin.
      it_admin-kostl = it_mha-kostl.
      it_admin-anzhl = it_mha-anzhl.
      collect it_admin. clear it_admin.
    elseif it_mha-kostl in r_kostl.
      g_total  = it_mha-anzhl + g_total.
    else.
      g_total     = it_mha-anzhl + g_total.
      g_non_mh    = it_mha-anzhl + g_non_mh.
    endif.
  endloop.

  g_rate = g_non_mh / g_total.

endform.                    " read_mh
*&---------------------------------------------------------------------*
*&      Form  get_all_cc_group
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_R_KOSTL  text
*----------------------------------------------------------------------*
form get_all_cc_group tables p_cc structure r_admin
                       using p_type.

  data:wa_return1 like bapiret2,
       wa_hnodes  like bapiset_hier,
       wa_hvalues  like bapi1112_values.

  data: it_hnodes like table of wa_hnodes,
        it_hvalues like table of wa_hvalues with header line.

*&----------Get cost center group information.
  call function 'BAPI_COSTCENTERGROUP_GETDETAIL'
       exporting
            controllingarea = p_kokrs
            groupname       = p_type
       importing
            return          = wa_return1
       tables
            hierarchynodes  = it_hnodes
            hierarchyvalues = it_hvalues.
  if wa_return1-type = 'E'.
    message id 'ZFI' type 'E' number '999' with text-005.
    exit.
  endif.

  data: w_hvalues like it_hvalues.
  refresh p_cc.
  p_cc-option = 'BT'. p_cc-sign = 'I'.
  loop at it_hvalues into w_hvalues.
    p_cc-low  = w_hvalues-valfrom.
    p_cc-high = w_hvalues-valto.
    append p_cc.
  endloop.

endform.                    " get_all_cc_group
*&---------------------------------------------------------------------*
*&      Form  postING_FM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_UCOMM  text
*----------------------------------------------------------------------*
form posting_fm using    p_ucomm.
  clear : it_cs001, it_cs001[],
          it_cs010, it_cs010[],
          it_cs011, it_cs011[].

  loop at it_post.
*    Header count
    if it_post-count <> 0.
      it_cs001-cgroup = it_post-kostl.
      it_cs001-count  = it_post-count.
      append it_cs001. clear it_cs001.
    endif.
*    Adming MH
    if it_post-anzhl <> 0 .
      it_cs010-cgroup = it_post-kostl.
      it_cs010-count  = it_post-anzhl.
      append it_cs010. clear it_cs010.
    endif.
*    Exclusion %
    if  it_post-rate <> 0 .
      it_cs011-cgroup = it_post-kostl.
      it_cs011-count  = it_post-rate .
      append it_cs011. clear it_cs011.
    endif.
  endloop.

* Header count
  perform posting_std tables it_cs001
                      using  c_skf1 .

* Admin MH
  perform posting_std tables it_cs010
                      using  c_skf2 .

* Exclusion %
  perform posting_std tables it_cs011
                      using  c_skf3 .


endform.                    " postING_FM

*&---------------------------------------------------------------------*
*&      Form  POSTING_STD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_CS001  text
*      -->P_1567   text
*----------------------------------------------------------------------*
form posting_std tables   p_post structure it_cs001
                 using    f_skf.

  data : lv_conf_text(50).

* Init. Message TAB
  clear : it_return, it_return[].

** <  Posting , hard coding > **
* doc date / posting date   =  LAST DAY OF THE MONTH
* TEXT
  clear lv_conf_text.
  concatenate sy-uname  sy-datum  sy-repid
         into lv_conf_text separated by '/'.

* Fill Header DATA
  clear wa_doc_header.
  wa_doc_header-co_area       = p_kokrs.
  wa_doc_header-docdate       = last_day.
  wa_doc_header-postgdate     = last_day.
  wa_doc_header-version       = p_versn.
  wa_doc_header-variant       = 'SAP01'.
  wa_doc_header-doc_hdr_tx    = lv_conf_text.
  wa_doc_header-username      = sy-uname.

* Fill Object List
  clear : it_doc_items, it_doc_items[].
  loop at p_post.
    it_doc_items-statkeyfig = f_skf.
    it_doc_items-stat_qty   = p_post-count.
* -> Alpha Numeric Conversion
    it_doc_items-rec_cctr   = p_post-cgroup.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
         exporting
              input  = it_doc_items-rec_cctr
         importing
              output = it_doc_items-rec_cctr.
**// End of Mod.
    append it_doc_items.
  endloop.

* Call BAPI FM
  perform call_post_fm.

* Commit
  if p_trun = 'X'.
    read table it_return  index 1.
    message s000(zmco) with it_return-message.
  else.
    commit work.
    read table it_return  index 1.
    message s000(zmco) with it_return-message.
*       Consolidation management for the SKF
    call function 'Z_FCO_MANAGEMENT_SKF'
      exporting
        im_pgmno         =   sy-tcode
        im_kokrs         =   p_kokrs
        im_gjahr         =   p_gjahr
        im_perbl         =   p_perid
*           IM_PERBL_T       =
        im_versn         =   p_versn
*           IM_KOSTL_F        =    S_KOSTL-LOW
*           IM_KOSTL_T        =    S_KOSTL-HIGH
       im_gname          =    p_ncoal.
*           IM_PRZNR_F       =
*           IM_PRZNR_T       =
*         IMPORTING
*           SUBRC            =

  endif.



endform.                    " POSTING_STD
