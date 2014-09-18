************************************************************************
* Program Name      : ZACO23U_SKF1
* Author            : Eun Hwa , Jung
* Creation Date     : 2003.10.07.
* Specifications By : Dong-Hwan Kim
* Pattern           : Report 1-1
* Development Request No : UD1K902678
* Addl Documentation:
* Description       : This program allocates
*                      the Activity Type Quantity of Semi-direct Cost
*                      Centers to Receiver Cost Centers by Man_Hr
*                      quantity ratio
* the BDC structures for BATCH INPUT processing
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************


report zaco23u_skf1 message-id zmco.

** type-pools
type-pools: slis.

** Table
tables : csks , cosp, cssl, csla , cosl, coss.

** For OBJECT KEY
data : begin of wa_obj ,
        objnr  like  coss-objnr,
        kostl  like  csks-kostl,
        lstar  like  csla-lstar,
       end of wa_obj.

data : it_obj_cctr_at   like standard table of wa_obj
                        with header line .


** Internal Table
data : begin of it_cosl occurs 500.
        include structure zsco_cosl_key01.
        include structure zsco_cosl_lst01.
data : end of   it_cosl.

data: begin of it_tmp_cosl occurs 0,
        gjahr  like  cosl-gjahr,
        perid  like  coejl-perbl,
        objnr  like  coss-objnr,
        kostl  like  csks-kostl,
        lstar  like  csla-lstar,
        curqty like  cosl-lst001,
        unit   like  cosl-meinh,
      end of it_tmp_cosl.


data : begin of it_cosl2 occurs 500.
        include structure zsco_cosl_key01.
        include structure zsco_cosl_lst01.
        include structure zsco_coss_meg01.
data : end of   it_cosl2.

data: begin of it_tmp_cosl2 occurs 0,
        gjahr  like  cosl-gjahr,
        perid  like  coejl-perbl,
        objnr  like  coss-objnr,
        kostl  like  csks-kostl,
        lstar  like  csla-lstar,
        curqty like  cosl-lst001,
        unit   like  cosl-meinh,
      end of it_tmp_cosl2.

data: begin of it_coss occurs 0,
        lstar  like  cssl-lstar,
        kostl  like  cssl-kostl.
        include structure zsco_coss_key01.
data : end of   it_coss.

* by ig.moon 3/30/2010 {
data: begin of it_coss_meg occurs 0,
        lstar  like  cssl-lstar,
        kostl  like  cssl-kostl.
        include structure zsco_coss_key01.
        include structure zsco_coss_meg01.
data : end of   it_coss_meg.
* }

data : begin of it_sum occurs 0,
           gjahr  like cssl-gjahr,
           perid  like coejl-perbl,
*           KOSTL LIKE CSSL-KOSTL,
*           LSTAR LIKE ZTCO_MHHRTRANS-LSTAR,
           objnr  like cssl-objnr,
           actqty like cosl-lst001,
       end of it_sum.

* For DD data
data : gv_ci_tabname     type ddobjname .
data : it_et_fieldlist   like table of rfvicp_ddic_tabl_fieldname
                         with header line.

* for reporting
data : begin of it_report occurs 0,
        gjahr   like  cosl-gjahr,
        perid   like  coejl-perbl,
        kostl   like  csks-kostl,
        objnr   like  cssl-objnr,
        lstar   like  csla-lstar,
        r_kostl like  cssl-kostl,
        r_objnr like  cssl-objnr,
        r_lstar like  cssl-lstar,
        actqty  like  cosl-lst001,
        curqty  like  cosl-lst001,
        vaeqty  like  cosl-lst001,
        unit    like  cosl-meinh,
      end of it_report.

* for posting
data : begin of it_post occurs 0,
        gjahr   like  cosl-gjahr,
        r_kostl like  csks-kostl,
        r_lstar like  cssl-lstar,
        kostl   like  csks-kostl,
        lstar   like  csla-lstar.
        include structure zsco_cosp_amt02.
data : end of  it_post.


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

*
data : gv_percount       like cosp-perbl. "Period Counter

** For BAPI
data : it_costcenterlist like standard table of bapi0012_cclist
                         with header line.
data : it_return         like standard table of bapiret2
                         with header line.
data : wa_headerinfo     like bapiplnhdr.
data : it_indexstructure like standard table of bapiacistru
                         with header line.
data : it_coobject       like standard table of bapiaciobj
                         with header line.
data : it_pervalue       like standard table of bapiacival
                         with header line.
data : it_totvalue       like standard table of bapiacitot
                         with header line.

* Macro For Transferring value in BAPI
define trans_value.
  it_pervalue-quantity_var_per&1  =  it_post-var0&1.
end-of-definition.
*

*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
selection-screen begin of block bl1 with frame title text-001.
parameters :
             p_kokrs like csks-kokrs memory id cac obligatory,
             p_gjahr like cobk-gjahr memory id gjr obligatory,
             p_frper like cosp-perbl memory id bpe obligatory,
             p_toper like cosp-perbl memory id bpe obligatory,
             p_versn like cobk-versn memory id kvt obligatory,
             p_currt like plnhdr-plnct default 'C' obligatory,
             p_trun(1).

selection-screen skip 1.

selection-screen begin of block bl3 with frame title text-003.
select-options : s_kostl  for csks-kostl.
parameters:      p_ncoal like grpdynp-name_coall
                                     default 'SEMIDIRECT'.
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
* Calculating Period Count
  perform cal_per_count.
* Read Cost Center Group - > Cost Center
  perform read_cegrp.
* Read OBJ Key Combination
  perform set_obj_key.
* Read Dynamic Fields Name
  perform read_field_name_from_dd_cosl.
* Read Sender_Quantity Information from COSL
  perform read_cosl_qty.
* Read Receiver.
  perform read_coss_parob.
* Read Receiver_Quantity Information from COSL
  perform read_cosl2_qty.
* Calculating Plan_Qty and preparation of reporting data.
  perform cal_plan_qty.
* Preparation of ALV
  perform pre_report_adj.
* Preparation of posting data.
  perform posting_data.


*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
end-of-selection.
* Call ALV LIST
  perform call_alv_list.


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
*&      Form  READ_CEGRP
*&---------------------------------------------------------------------*
*       read cost center
*----------------------------------------------------------------------*
form read_cegrp.

* Making an internal table for CCtr to select data
* Selected Group on screen
  clear : it_costcenterlist, it_costcenterlist[].
  clear : it_return, it_return[].

* Set Validity Date (Start)
  data : lv_datum like sy-datum.
  concatenate p_gjahr p_frper+1(2) '01' into lv_datum.

* From CCtr Group
  if not p_ncoal is initial.
    call function 'BAPI_COSTCENTER_GETLIST1'
         EXPORTING
              controllingarea = p_kokrs
              date_from       = lv_datum
              costcentergroup = p_ncoal
         TABLES
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
           EXPORTING
                controllingarea = p_kokrs
                date_from       = lv_datum
                costcenter_from = s_kostl-low
                costcenter_to   = s_kostl-high
           TABLES
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

  clear it_costcenterlist.
* Except costcenter = '55103'.
  delete it_costcenterlist where costcenter = '0000055103'.

* Sorting
  sort it_costcenterlist by co_area costcenter.
  delete adjacent duplicates from it_costcenterlist.

* Check CCtr
  if it_costcenterlist[] is initial .
    message e018(zmco) with 'Cost Center'.
  endif.


endform.                    " READ_CEGRP
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
*&      Form  SET_OBJ_KEY
*&---------------------------------------------------------------------*
*       Read OBJ Key Combination
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_obj_key.

  clear : it_obj_cctr_at, it_obj_cctr_at[].

* CCtr  - > objnr , kostl , lstar
  clear cssl.
  select  objnr kostl lstar
                      into corresponding fields of table it_obj_cctr_at
                      from cssl
                     for all entries in it_costcenterlist
                     where kokrs = p_kokrs
                       and kostl = it_costcenterlist-costcenter
                       and gjahr = p_gjahr.

  clear : it_obj_cctr_at.


endform.                    " SET_OBJ_KEY
*&---------------------------------------------------------------------*
*&      Form  READ_COSL_QTY
*&---------------------------------------------------------------------*
*       Read Quantity data from COSL
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_cosl_qty.

  clear : it_cosl, it_cosl[].
  clear cosl.
  select (it_et_fieldlist)
         into corresponding fields of table it_cosl
         from cosl
          for all entries in it_obj_cctr_at
        where lednr = '00'
          and objnr = it_obj_cctr_at-objnr
          and gjahr = p_gjahr
          and wrttp = '01'             " plan : '01'
          and versn = p_versn.
  clear : it_cosl.

* Local Data definition
  field-symbols: <fs1> type any.
  data : lv_lst_nam(30).
  data : lv_cnt  like  cosp-perbl.

  clear   it_cosl.
  clear : it_tmp_cosl, it_tmp_cosl[].

  loop at it_cosl.
* Period Counter : Set From-Period .
    clear lv_cnt.
    lv_cnt = p_frper .
* Key Part
    it_tmp_cosl-gjahr = p_gjahr.
    clear it_obj_cctr_at.
    read table    it_obj_cctr_at
         with key objnr = it_cosl-objnr.
    it_tmp_cosl-kostl = it_obj_cctr_at-kostl.
    it_tmp_cosl-lstar = it_obj_cctr_at-lstar.
    it_tmp_cosl-objnr = it_obj_cctr_at-objnr.

    it_tmp_cosl-unit = it_cosl-meinh.     " insert unit

    do gv_percount times.
* Period
      clear it_tmp_cosl-perid.
      it_tmp_cosl-perid = lv_cnt.
* Value Transferring
      clear lv_lst_nam.
      concatenate 'IT_COSL-'  'LST'  lv_cnt
             into lv_lst_nam.
      assign (lv_lst_nam) to <fs1>.
      clear it_tmp_cosl-curqty.
      it_tmp_cosl-curqty = <fs1>.

*   Unit Conversion
      if it_tmp_cosl-unit <> 'STD'.
        perform unit_conv using it_tmp_cosl-unit
                                it_tmp_cosl-curqty.
      endif.

* Collect
      collect it_tmp_cosl.
* Period Counter
      lv_cnt = lv_cnt + 1.
    enddo.
    clear it_tmp_cosl.
    clear it_cosl.
  endloop.
  clear it_tmp_cosl.


endform.                    " READ_COSL_QTY
*&---------------------------------------------------------------------*
*&      Form  READ_COSS_PAROB
*&---------------------------------------------------------------------*
*       To read COSS table
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_coss_parob.

* search for Partner object
  clear : it_coss, it_coss[].
  clear coss.
  select *
         into corresponding fields of table it_coss
         from coss
          for all entries in it_obj_cctr_at
        where lednr = '00'
          and objnr = it_obj_cctr_at-objnr
          and gjahr = p_gjahr
          and wrttp = '01'
          and versn = p_versn.
*Issue number
*Requested by dhkim,20041020,changed by wskim
*---Start
*          and meg009 ne 0.
*---End

  clear it_coss.

  delete it_coss where parob = ' '.

*  DATA : LV_LSTAR LIKE CSSL-LSTAR,
*         LV_KOSTL LIKE CSSL-KOSTL.
*  CLEAR : LV_LSTAR, LV_KOSTL.

  loop at it_coss.
*    CLEAR : LV_LSTAR, LV_KOSTL.
*    CALL FUNCTION 'OBJECT_KEY_GET_KL'
*      EXPORTING
*        OBJNR             = IT_COSS-PAROB
*      IMPORTING
**     KOKRS             =
*       KOSTL             =  LV_KOSTL
*       LSTAR             =  LV_LSTAR      .
*
*       IF SY-SUBRC <> 0.
*         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*       ENDIF.

    it_coss-kostl = it_coss-parob+6(10).
    it_coss-lstar = it_coss-parob+16(6).
    modify it_coss.
    clear it_coss.
  endloop.

  delete it_coss where lstar ne 'MAN_HR'.

* by ig.moon 3/30/2010 {

  clear : it_coss_meg, it_coss_meg[].
  select *
         into corresponding fields of table it_coss_meg
         from coss
          for all entries in it_obj_cctr_at
        where lednr = '00'
          and objnr = it_obj_cctr_at-objnr
          and gjahr = p_gjahr
          and wrttp = '01'
          and versn = p_versn.

  delete it_coss_meg where parob = ' '.

  loop at it_coss_meg.
    it_coss_meg-kostl = it_coss_meg-parob+6(10).
    it_coss_meg-lstar = it_coss_meg-parob+16(6).
    modify it_coss_meg.
    clear it_coss_meg.
  endloop.


  delete it_coss_meg where lstar ne 'MAN_HR'.

* }


endform.                    " READ_COSS_PAROB
*&---------------------------------------------------------------------*
*&      Form  CAL_PER_COUNT
*&---------------------------------------------------------------------*
*       Calculation STD. - period Counter
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form cal_per_count.

  gv_percount = p_toper - p_frper + 1.

endform.                    " CAL_PER_COUNT
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

* Sort
  sort it_report by gjahr perid kostl objnr lstar r_kostl r_objnr
                    r_lstar.
  clear it_report.

  it_sort-fieldname = 'GJAHR'.
  it_sort-up        = 'X'.
  it_sort-expa      = 'X'.
  it_sort-subtot    = 'X'.
  append it_sort.
  it_sort-fieldname = 'PERID'.
  it_sort-up        = 'X'.
  it_sort-expa      = 'X'.
  it_sort-subtot    = 'X'.
  append it_sort.
  it_sort-fieldname = 'KOSTL'.
  it_sort-up        = 'X'.
  it_sort-expa      = 'X'.
  it_sort-subtot    = 'X'.
  append it_sort.
  it_sort-fieldname = 'LSTAR'.
  it_sort-up        = 'X'.
  it_sort-expa      = 'X'.
  it_sort-subtot    = 'X'.
  append it_sort.


* Set Event
  data : wa_l_event  type slis_alv_event.
  wa_l_event-name = slis_ev_top_of_page.
  wa_l_event-form = 'BASIC_TOP_OF_PAGE'.
  append wa_l_event to it_events.


endform.                    " PRE_REPORT_ADJ
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


* Key
  perform build_fieldcat using
    'IT_REPORT' 'GJAHR'  'X'            space    space
    space        '4'      'Year'           space    space    space.

  perform build_fieldcat using
    'IT_REPORT' 'PERID'  'X'            space    space
    space        '6'      'Period'           space    space    space.

  perform build_fieldcat using
    'IT_REPORT' 'KOSTL'  'X'            space    space
    space        '10'      'Sender'           space    space    space.

*  PERFORM BUILD_FIELDCAT USING
*    'IT_REPORT' 'OBJNR'  'X'            SPACE    SPACE
*    SPACE        '22'      'OBJNR'           SPACE    SPACE    SPACE.

  perform build_fieldcat using
    'IT_REPORT' 'LSTAR'  'X'            space    space
    space        '6'      'Sen.AT'           space    space    space.

  perform build_fieldcat using
    'IT_REPORT' 'R_KOSTL' 'X'            space    space
    space        '10'      'Receiver'       space    space    space.

*  PERFORM BUILD_FIELDCAT USING
*    'IT_REPORT' 'R_OBJNR'  'X'            SPACE    SPACE
*    SPACE        '22'      'R_OBJNR'           SPACE    SPACE    SPACE.

  perform build_fieldcat using
    'IT_REPORT' 'R_LSTAR'  'X'            space    space
    space        '6'      'Rec.AT'           space    space    space.

** Value

  perform build_fieldcat using
    'IT_REPORT' 'UNIT'  space            space    space
    space   '4'      'Unit'      'UNIT'   space     space.

  perform build_fieldcat using
    'IT_REPORT' 'ACTQTY'  space            space    space
    space   '15'      'Rec.Plan.Qty'  space    space space.

  perform build_fieldcat using
    'IT_REPORT' 'CURQTY'  space            space    space
    space '15'      'Sen.Plan.Qty'    space    space space.

*  PERFORM BUILD_FIELDCAT USING
*    'IT_REPORT' 'VAEQTY'  SPACE            'X'    SPACE
*    'IT_REPORT'   '15'      'VAEQTY'      SPACE   'UNIT' 'IT_REPORT'.

  perform build_fieldcat using
    'IT_REPORT' 'VAEQTY'  space            'X'    space
    'IT_REPORT'   '15'      'Plan Qty'      space   space  space .


endform.                    " FIELDCAT_INIT
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1197   text
*      -->P_1198   text
*      -->P_1199   text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_1203   text
*      -->P_1204   text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*----------------------------------------------------------------------*
form build_fieldcat using    value(p_0100)
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
*       text
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
       it_sort                        = it_sort[]        "
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
       t_outtab                       = it_report
       exceptions
       program_error                  = 1
       others                         = 2.

  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

endform.                    " CALL_ALV_LIST

*-----------------------------------------------------------------------
*    FORM PF_STATUS
*-----------------------------------------------------------------------
form pf_status using  extab type slis_t_extab.
  set pf-status 'BALVLIST' excluding extab .
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
    when 'POST'.     " OR 'REVS'.
      perform post_std_cctr_at_ce  using ucomm.
  endcase.

endform.

*---------------------------------------------------------------------*
*       FORM BASIC_TOP_OF_PAGE                                        *
*---------------------------------------------------------------------*
*       Display Title                                                 *
*---------------------------------------------------------------------*
form basic_top_of_page.
  write : / 'Controlling Area/          : '
            , p_kokrs .
  write : / 'Fiscal Year/Period/Version : '
            , p_gjahr, '/', p_frper, '~', p_toper , '/', p_versn.
* CCTR
  if p_ncoal ne space.
    write : / 'Cost Center Grp.           : ', p_ncoal.
  endif.

  if not s_kostl[] is initial.
    loop at s_kostl.
      at first.
        write : / 'Cost Center                : '.
      endat.
      write : / '                            ', s_kostl-low, '~',
    s_kostl-high.
    endloop.
  endif.


  write : / 'Test Run                     ', p_trun.
  skip 1.


endform.
*&---------------------------------------------------------------------*
*&      Form  POST_STD_CCTR_AT_CE
*&---------------------------------------------------------------------*
*       PREPARATION POSTING
*----------------------------------------------------------------------*
*      -->P_UCOMM  text
*----------------------------------------------------------------------*
form post_std_cctr_at_ce using    p_ucomm.

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
  sort it_post by r_kostl r_lstar.

  loop at it_post.
* Obj
    on change of  it_post-r_kostl
              or  it_post-r_lstar .
* Index of Object Key
      it_indexstructure-object_index
           = it_indexstructure-object_index + 1 .

      clear it_coobject.
      it_coobject-object_index = it_indexstructure-object_index.

      it_coobject-costcenter   = it_post-r_kostl.
      it_coobject-acttype      = it_post-r_lstar.
      append it_coobject.
      clear  it_coobject.
    endon.

* Value.
* Index of Value
    it_indexstructure-value_index
         = it_indexstructure-value_index + 1.

    clear it_pervalue.
    it_pervalue-value_index = it_indexstructure-value_index.

    it_pervalue-send_cctr     = it_post-kostl.
    it_pervalue-send_activity = it_post-lstar.

* Set Value
* Post/Reverse
    if  p_ucomm = 'POST'.
      perform set_value_amt.
    endif.

    append it_pervalue.
    clear  it_pervalue.

* append Index
    append it_indexstructure.
    clear it_post.
  endloop.

* Call BAPI FM
  perform call_post_fm.

* Commit
  if p_trun = 'X'.
  else.
    commit work.
    message s009(zmco) with p_ucomm.

*       Consolidation management for the SKF
    call function 'Z_FCO_MANAGEMENT_SKF'
         EXPORTING
              im_pgmno   = sy-tcode
              im_kokrs   = p_kokrs
              im_gjahr   = p_gjahr
              im_perbl   = p_frper
              im_perbl_t = p_toper
              im_versn   = p_versn
              im_kostl_f = s_kostl-low
              im_kostl_t = s_kostl-high
              im_gname   = p_ncoal.
*           IM_PRZNR_F       =
*           IM_PRZNR_T       =
*         IMPORTING
*           SUBRC            =

  endif.


endform.                    " POST_STD_CCTR_AT_CE
*&---------------------------------------------------------------------*
*&      Form  SET_VALUE_AMT
*&---------------------------------------------------------------------*
*       text
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
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form call_post_fm.

  call function 'BAPI_COSTACTPLN_POSTACTINPUT'
    exporting
      headerinfo           = wa_headerinfo
*   DELTA                = ' '
    tables
      indexstructure       = it_indexstructure
      coobject             = it_coobject
      pervalue              = it_pervalue
*   TOTVALUE             =
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
*&      Form  CHK_INPUT_VALUE
*&---------------------------------------------------------------------*
*       text
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

* Check Cost Center/Cost Center Group
  if     s_kostl[] is initial
     and p_ncoal   is initial .
    message e016(zmco).
  elseif
         not s_kostl[] is initial
     and not p_ncoal   is initial .
    message e017(zmco).
  endif.

** Check TEST-RUN  Flag
  if p_trun na 'X '.
    message e008(zmco).
  endif.

* Check Currency IND.
  if p_currt na 'CTO'.
    message e000(zmco) with p_currt ' is not a posible value' .
  endif.

endform.                    " CHK_INPUT_VALUE
*&---------------------------------------------------------------------*
*&      Form  POSTING_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form posting_data.

  clear : it_post, it_post[].
  clear : it_report.
  loop at it_report.
    move-corresponding it_report to it_post.

    if it_report-perid = '001'.
      it_post-var001 = it_report-vaeqty.
    elseif it_report-perid = '002'.
      it_post-var002 = it_report-vaeqty.
    elseif it_report-perid = '003'.
      it_post-var003 = it_report-vaeqty.
    elseif it_report-perid = '004'.
      it_post-var004 = it_report-vaeqty.
    elseif it_report-perid = '005'.
      it_post-var005 = it_report-vaeqty.
    elseif it_report-perid = '006'.
      it_post-var006 = it_report-vaeqty.
    elseif it_report-perid = '007'.
      it_post-var007 = it_report-vaeqty.
    elseif it_report-perid = '008'.
      it_post-var008 = it_report-vaeqty.
    elseif it_report-perid = '009'.
      it_post-var009 = it_report-vaeqty.
    elseif it_report-perid = '010'.
      it_post-var010 = it_report-vaeqty.
    elseif it_report-perid = '011'.
      it_post-var011 = it_report-vaeqty.
    elseif it_report-perid = '012'.
      it_post-var012 = it_report-vaeqty.
    elseif it_report-perid = '013'.
      it_post-var013 = it_report-vaeqty.
    elseif it_report-perid = '014'.
      it_post-var014 = it_report-vaeqty.
    elseif it_report-perid = '015'.
      it_post-var015 = it_report-vaeqty.
    elseif it_report-perid = '016'.
      it_post-var016 = it_report-vaeqty.
    endif.

    collect it_post.
    clear it_post.
  endloop.
  clear it_post.

  sort it_post by gjahr r_kostl r_lstar.


endform.                    " POSTING_DATA
*&---------------------------------------------------------------------*
*&      Form  READ_COSL2_QTY
*&---------------------------------------------------------------------*
*       Read Quantity data from COSL
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_cosl2_qty.

  clear : it_cosl2, it_cosl2[].
  clear cosl.
  select (it_et_fieldlist)
         into corresponding fields of table it_cosl2
         from cosl
          for all entries in it_coss
        where lednr = '00'
          and objnr = it_coss-parob
          and gjahr = p_gjahr
          and wrttp = '01'
          and versn = p_versn.
  clear : it_cosl2.


* Local Data definition
  field-symbols: <fs2> type any.
  data : lv_lst_nam2(30).
  data : lv_cnt2  like  cosp-perbl.

  clear   it_cosl2.
  clear : it_tmp_cosl2, it_tmp_cosl2[].

  loop at it_cosl2.
* Period Counter : Set From-Period .
    clear lv_cnt2.
    lv_cnt2 = p_frper .
* Key Part
    it_tmp_cosl2-gjahr = p_gjahr.
    it_tmp_cosl2-kostl = it_cosl2-objnr+6(6).
*    IT_TMP_COSL2-LSTAR = IT_COSL2-OBJNR+16(6).
    it_tmp_cosl2-objnr = it_cosl2-objnr.

    it_tmp_cosl2-unit = it_cosl2-meinh.     " insert unit.

    do gv_percount times.
* Period
      clear it_tmp_cosl2-perid.            "!
      it_tmp_cosl2-perid = lv_cnt2.
* Value Transferring
      clear lv_lst_nam2.
      concatenate 'IT_COSL2-'  'LST'  lv_cnt2
             into lv_lst_nam2.
      assign (lv_lst_nam2) to <fs2>.
      clear it_tmp_cosl2-curqty.
      it_tmp_cosl2-curqty = <fs2>.

*   Unit Conversion
      if it_tmp_cosl2-unit <> 'STD'.
        perform unit_conv using it_tmp_cosl2-unit
                                it_tmp_cosl2-curqty.
      endif.

* Collect
      collect it_tmp_cosl2.
* Period Counter
      lv_cnt2 = lv_cnt2 + 1.
    enddo.
    clear it_tmp_cosl2.
    clear it_cosl2.
  endloop.
  clear it_tmp_cosl2.

endform.                    " READ_COSL2_QTY
*&---------------------------------------------------------------------*
*&      Form  CAL_PLAN_QTY
*&---------------------------------------------------------------------*
*       Calculating Ratio
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form cal_plan_qty.
*Making an it_report for reporting
  clear : it_report, it_report[].
  clear : it_tmp_cosl, it_coss.
  loop at it_tmp_cosl.
    loop at it_coss where objnr = it_tmp_cosl-objnr.

      move-corresponding it_tmp_cosl to it_report.
      it_report-r_kostl = it_coss-kostl.
      it_report-r_objnr = it_coss-parob.
      it_report-r_lstar = it_coss-lstar.

      append it_report.
      clear it_report.

    endloop.
  endloop.

* by ig.moon 3/30/2010 {
  field-symbols: <fs3> type any.
  data : lv_meg_nam2(30).

  sort it_coss_meg by gjahr kostl objnr.

* }

  data $ix type i.

  clear it_report.

  loop at it_report.

    $ix = sy-tabix.

    clear it_tmp_cosl2.
    read table    it_tmp_cosl2
       with key gjahr = it_report-gjahr
                perid = it_report-perid
                objnr = it_report-r_objnr.
*                KOSTL = IT_REPORT-R_KOSTL.


* by ig.moon 3/30/2010 {
    read table it_coss_meg with key gjahr = it_report-gjahr
                                    kostl = it_report-r_kostl
                                    objnr = it_report-objnr
                                    binary search.
    if sy-subrc eq 0.

      clear lv_meg_nam2.
      concatenate 'IT_COSS_MEG'  '-MEG'  it_report-perid
             into lv_meg_nam2.
      assign (lv_meg_nam2) to <fs3>.

      if <fs3> eq '0.000'.
        clear it_tmp_cosl2-curqty.
      endif.

    endif.

* }

    it_report-actqty = it_tmp_cosl2-curqty.
    modify it_report index $ix.

  endloop.

  clear : it_sum, it_sum[].
  loop at it_report.
    move-corresponding it_report to it_sum.
    collect it_sum.
  endloop.
  clear it_sum.


* Calculate PLAN QTY.

  clear it_report.
  loop at it_report.
    clear it_sum.
    read table  it_sum
         with key gjahr = it_report-gjahr
                  perid = it_report-perid
                  objnr = it_report-objnr.
*                  KOSTL = IT_REPORT-KOSTL
*                  LSTAR = IT_REPORT-LSTAR.

    it_report-vaeqty = it_report-curqty *
                        ( it_report-actqty / it_sum-actqty ).
    modify it_report.
  endloop.
  clear it_report.


endform.                    " CAL_PLAN_QTY
*&---------------------------------------------------------------------*
*&      Form  READ_FIELD_NAME_FROM_DD_COSL
*&---------------------------------------------------------------------*
*       Read Technical FieldName for COSL
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_field_name_from_dd_cosl.

  clear : it_et_fieldlist, it_et_fieldlist[].

* read DD infor. COSL Key Part
  perform read_dd_info  tables it_et_fieldlist
                        using  'ZSCO_COSL_KEY01'.

* read DD infor. COSL Value Part (Total Quantity)
  perform read_dd_info  tables it_et_fieldlist
                        using  'ZSCO_COSL_LST01'.

endform.                    " READ_FIELD_NAME_FROM_DD_COSL
*&---------------------------------------------------------------------*
*&      Form  READ_DD_INFO
*&---------------------------------------------------------------------*
*        Read DD information
*----------------------------------------------------------------------*
*     -->IT_l_ET_FIELDLIST  Field-List Table
*      -->P_CI_TABNAME       DD name
*----------------------------------------------------------------------*
form read_dd_info tables   it_l_et_fieldlist structure it_et_fieldlist
                  using    p_ci_tabname      like gv_ci_tabname.
* Local DATA definition
  data : it_l_fdlist like standard table of it_et_fieldlist
                     with header line.
* Making FDlist
  clear : it_l_fdlist,     it_l_fdlist[].
  clear gv_ci_tabname.
  gv_ci_tabname = p_ci_tabname.
  call function 'RECP_DD_TABL_FIELDNAMES_GET'
       EXPORTING
            ic_tabname   = gv_ci_tabname
       TABLES
            et_fieldlist = it_l_fdlist
       EXCEPTIONS
            not_found    = 1
            others       = 2.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.
  append lines of  it_l_fdlist       to it_l_et_fieldlist.

endform.                    " READ_DD_INFO
*&---------------------------------------------------------------------*
*&      Form  UNIT_CONV
*&---------------------------------------------------------------------*
*       Unit Conversion
*----------------------------------------------------------------------*
*      -->P_UNIT  UNIT
*      -->P_QTY   Quantity
*----------------------------------------------------------------------*
form unit_conv using    p_unit
                        p_qty.
  call function 'UNIT_CONVERSION_SIMPLE'
   exporting
     input                      = p_qty
*    NO_TYPE_CHECK              = 'X'
     round_sign                 = 'X'
     unit_in                    = p_unit
     unit_out                   = 'STD'
   importing
*    ADD_CONST                  =
*    DECIMALS                   =
*    DENOMINATOR                =
*    NUMERATOR                  =
     output                     = p_qty
   exceptions
     conversion_not_found       = 1
     division_by_zero           = 2
     input_invalid              = 3
     output_invalid             = 4
     overflow                   = 5
     type_invalid               = 6
     units_missing              = 7
     unit_in_not_found          = 8.

  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

  p_unit = 'STD'.

endform.                    " UNIT_CONV
