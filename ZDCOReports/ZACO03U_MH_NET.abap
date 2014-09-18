************************************************************************
* Program Name      : ZACO59U_NET_MAN_HOUR
* Author            : Byung Sung Bae
* Creation Date     : 06/10/2003
* Specifications By : Dong-Hwan Kim
* Pattern           : Report 1-1
* Development Request No: UD1K915915
* Add documentation :
* Description       : This program is copied from ZACO03U_MHAM
* the BDC structures for BATCH INPUT processing
*
* Modifications Log
* Date   Developer   Request ID    Description
*
************************************************************************
report zaco59u_net_man_hour message-id zmco.


*----------------------------------------------------------------------*
*   Include Program
*----------------------------------------------------------------------*
* For Global Value in CO
include zlzgco_global_formto1.


*----------------------------------------------------------------------*
*   Data Definition
*----------------------------------------------------------------------*
** type-pools
type-pools: slis.

** Tables
tables: ztco_nmhhrtrans, catsdb, ztco_mhaatype, tka01, cosl, bwom_ifact,
         zsco_cosl_key01, zsco_cosl_lst01.

ranges : pnpkostl for pernr-kostl.

** Internal table
*DATA : IT_ZTCO_NMHHRTRANS LIKE Hashed TABLE OF ZTCO_NMHHRTRANS
*                         WITH UNIQUE KEY GJAHR PERID KOSTL LSTAR
*                         WITH HEADER LINE .
data : it_ztco_nmhhrtrans  like standard table of ztco_nmhhrtrans
                          with header line .
data : it_post            like standard table of ztco_nmhhrtrans
                          with header line .
* For TimeSheet
data : it_tmp_catsdb     like standard table of ztco_nmhhrtrans
                         with header line .
* For B/F Quan.
data : it_tmp_cosl       like standard table of ztco_nmhhrtrans
                         with header line .
* For A/A Type
data : it_ztco_mhaatype  like standard table of ztco_mhaatype
                         with header line .
* For DATA retrieval
data : begin of it_catsdb occurs 5000,
          workdate  like catsdb-workdate,
          skostl    like catsdb-skostl  , "Sender CCtr
          lstar     like catsdb-lstar   ,
          rkostl    like catsdb-rkostl  , "Rec. CCtr
          awart     like catsdb-awart   ,
          unit      like catsdb-unit    ,
          status    like catsdb-status  ,
          catshours like catsdb-catshours.
data : end of   it_catsdb.
data : begin of it_cosl occurs 500.
        include structure zsco_cosl_key01.
        include structure zsco_cosl_lst01.
data : end of   it_cosl.

** Range
ranges : r_workdate for catsdb-workdate,
         r_perid    for ztco_nmhhrtrans-perid.

** For BAPI
data : it_costcenterlist like standard table of bapi0012_cclist
                         with header line.
data : it_return         like standard table of bapiret2
                         with header line.
* For DD data
data : gv_percount       like cosp-perbl. "Period Counter
data : gv_ci_tabname     type ddobjname .
data : it_et_fieldlist   like table of rfvicp_ddic_tabl_fieldname
                         with header line.
data : begin of wa_obj ,
        objnr  like  coss-objnr,
        kostl  like  csks-kostl,
        lstar  like  csla-lstar,
       end of wa_obj.
data : it_obj_cctr_at   like standard table of wa_obj
                        with header line .


data: begin of lt_tmp_catsdb occurs 0.
        include structure it_tmp_catsdb.
data:   pernr   like   pernr-pernr,
        datum   like   sy-datum,
      end   of lt_tmp_catsdb.


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
* Globale Daten
include rptbal01.

*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
initialization.
* Set Global ALV Parameter
  gv_repid = sy-repid.
* Set default Wage Type
  perform set_def_wage_type.


*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
  selection-screen begin of block bl1 with frame title text-001.
  parameters : p_kokrs like csks-kokrs   memory   id cac obligatory,
               p_gjahr like anlp-gjahr   memory   id gjr obligatory.

  selection-screen begin of line.
  selection-screen comment 1(31) text-002.
*   From Period.
  parameters: p_frper like rku01g-perab obligatory.
  selection-screen comment 52(05) text-003.
*   To Period.
  parameters: p_toper like rku01g-perbi no-display. " OBLIGATORY.
  selection-screen end of line.
  parameters : p_lstar like csla-lstar            default 'MAN_HR'
                                                         obligatory,
               p_ncoal like grpdynp-name_coall    default 'DIRECT'
                                                         obligatory.
  select-options : s_lgart for t512w-lgart.

  selection-screen end of block bl1.
* For HR Time Sheet
  selection-screen begin of block bl3 with frame title text-006.
  select-options : s_status for catsdb-status     default '30'
                                                         no-display.
  "OBLIGATORY.
  selection-screen end of block bl3.
* For B/F Quantity
  selection-screen begin of block bl2 with frame title text-005.
  parameters : p_versn like cosl-versn            default '000'
                                                  obligatory,
               p_wrttp like cosl-wrttp            default '4'
                                                  obligatory.
  select-options : s_vrgng for cosl-vrgng        .
  selection-screen end of block bl2.


*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
at selection-screen.
* Check period range
  perform check_period_range.
* Searching for CCtr group
at selection-screen on value-request for p_ncoal.
  perform read_cctr_group.


*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
start-of-selection.
* Calculating Period Count
  perform cal_per_count.
* Controlling Area Information
  perform read_tka01.
* Read CCtrs
  perform read_cctr.
* Preparation to select data
  perform pre_to_select.
* Enqueue ZTCO_NMHHRTRANS
  perform enqueue_ztco_nmhhrtrans.
* Delete DATA in Table ZTCO_NMHHRTRANS.
  perform delete_data.

**// Mod. By Hyung Jin Youn 2004.07.16.
** Change of Source data for Timesheet
* Select DATA from CATSDB
* Read TimeSheet DATA
* PERFORM READ_FR_CATSDB.

*ANDY FIX
* perform read_fr_catsdb2.
  perform read_co_mh.

**// End of Mod.

**// Mod. By Hyung Jin Youn 2004.08.23
* Change the logics about reading HR timesheet data
  perform read_fr_catsdb3.
**// End of Mod.

* Read B/F Quantity
  perform read_bf_quan.
* Aggregate data from B/F and TimeSheet
  perform add_up_data.
* Get Net hour
*  perform get_net_hour.
* Preparation of ALV
  perform pre_report_adj.
* Do not Commit Work or Dequeue explicitly
* LUW will do

*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
end-of-selection.
* Call ALV LIST
  perform call_alv_list.

*----------------------------------------------------------------------*
* Sub-Routine
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  CHECK_PERIOD_RANGE
*&---------------------------------------------------------------------*
*       Check Period range
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_period_range.
*NO-DISPLAY / Only One period
  p_toper = p_frper.
*  IF P_FRPER > P_TOPER.
*    MESSAGE E031.
*  ENDIF.
endform.                    " CHECK_PERIOD_RANGE

*&---------------------------------------------------------------------*
*&      Form  READ_CCTR_GROUP
*&---------------------------------------------------------------------*
*       Read CCtr Group (Search Help)
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_cctr_group.
  call function 'K_GROUP_SELECT'
    exporting
*     BUTTONS                  = 'X'
      class                    = '0101'
*     CRUSER                   = '*'
      field_name               = space
*     SEARCHFLD                = '    '
*     SEARCHFLD_INPUT          = 'X'
      searchfld_required       = ' '
*     SET                      = GV_CCGR_SETID
*     START_COLUMN             = 10
*     START_ROW                = 5
*     TABLE                    = 'CCSS'
*     TYPELIST                 = 'BS'
*     UPDUSER                  = '*'
*     KOKRS                    =
*     KTOPL                    =
    importing
*     CLASS_NAME               =
      set_name                 = p_ncoal
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

endform.                    " READ_CCTR_GROUP

*&---------------------------------------------------------------------*
*&      Form  Read_TKA01
*&---------------------------------------------------------------------*
*       Controlling Area Information
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_tka01.
  clear tka01.
  select single * from tka01
                 where kokrs = p_kokrs.
  if sy-subrc <> 0.
    message e038 with p_kokrs.
  endif.
endform.                    " Read_TKA01

*&---------------------------------------------------------------------*
*&      Form  PRE_TO_SELECT
*&---------------------------------------------------------------------*
*       Preparation to select data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form pre_to_select.
* Read A/A type
  clear : it_ztco_mhaatype, it_ztco_mhaatype.
  select * into corresponding fields of table it_ztco_mhaatype
           from ztco_mhaatype
          where selind = 'X'.

  if it_ztco_mhaatype[] is initial .
    message e042.
  endif.

* Convert Periods to date Range
  clear : r_workdate, r_workdate[].
  call function 'FIRST_DAY_IN_PERIOD_GET'
       exporting
            i_gjahr = p_gjahr
            i_periv = tka01-lmona
            i_poper = p_frper
       importing
            e_date  = r_workdate-low.

  call function 'LAST_DAY_IN_PERIOD_GET'
       exporting
            i_gjahr = p_gjahr
            i_periv = tka01-lmona
            i_poper = p_toper
       importing
            e_date  = r_workdate-high.
  r_workdate-sign   = 'I'.
  r_workdate-option = 'BT'.
  append r_workdate.

endform.                    " PRE_TO_SELECT

*&---------------------------------------------------------------------*
*&      Form  READ_FR_CATSDB
*&---------------------------------------------------------------------*
*       Read TIMESHEET data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_fr_catsdb.

** Caution !!!
* CATSDB has MASS-Volume data.
* There is no appropriate Index with key - 'STATUS , AWART, Workdate'
* It seems any of fields in the key combination dose not deserve to be
* used as an index field
* Recommend to run this program in background mode if possible

* BUT, This program is made to run on On-Line mode to check report
* (request By functional Member - 2003.10.07)

* Set AWART
  ranges : r_l_awart for catsdb-awart.
  clear  : r_l_awart, r_l_awart[].
  loop at it_ztco_mhaatype.
    r_l_awart-low  = it_ztco_mhaatype-awfrom.
    r_l_awart-high = it_ztco_mhaatype-awto.
    if    r_l_awart-low  ne space
      and r_l_awart-high ne space .
      r_l_awart-sign = 'I'.
      r_l_awart-option = 'BT'.
    else.
      r_l_awart-sign = 'I'.
      r_l_awart-option = 'EQ'.
    endif.
    append r_l_awart.
    clear  r_l_awart.
  endloop.

* Index - ZD1
*          WORKDATE  LIKE CATSDB-WORKDATE,
*          SKOSTL    LIKE CATSDB-SKOSTL  , "Sender CCtr
*          LSTAR     LIKE CATSDB-LSTAR   ,
*          RKOSTL    LIKE CATSDB-RKOSTL  , "Rec. CCtr
*          AWART     LIKE CATSDB-AWART   ,
*          UNIT      LIKE CATSDB-UNIT    ,
*          STATUS    LIKE CATSDB-STATUS  ,
*          CATSHOURS LIKE CATSDB-CATSHOURS.

  clear : it_catsdb, it_catsdb[].
  select workdate  skostl  lstar   rkostl
         awart     unit    status  catshours
         into corresponding fields of table it_catsdb
         from catsdb
        where awart    in r_l_awart
          and status   in s_status
          and workdate in r_workdate.

  if  it_catsdb[] is initial.
    message e026.
  endif.

  clear it_catsdb.

* Re-org.
  clear : it_tmp_catsdb, it_tmp_catsdb[].
  loop at it_catsdb.
    it_tmp_catsdb-gjahr = p_gjahr.
* AT => Set as MAN_HR
* Always MAN_HR is used as AT no matter what the AT in timesheet is
* (request by Functional Member)
    it_tmp_catsdb-lstar = p_lstar.
* Unit
    it_tmp_catsdb-unit  = it_catsdb-unit.
* CCtr
* Sender, Receiver = O, X -> Sender
    if    it_catsdb-skostl ne space
      and it_catsdb-rkostl eq space.
      it_tmp_catsdb-kostl = it_catsdb-skostl.
    elseif
* Sender, Receiver = O, O -> Receiver /  Support Case
          it_catsdb-skostl ne space
      and it_catsdb-rkostl ne space.
      it_tmp_catsdb-kostl = it_catsdb-rkostl.
    else.
* Sender, Receiver = X, O -> Sender / No Kostl
* Sender, Receiver = X, X -> Sender / No Kostl
      it_tmp_catsdb-kostl = it_catsdb-skostl.
    endif.
* Check Cost Center Range (User input -> CCtr Group)
* Ex.> Direct
    clear it_costcenterlist.
    read table it_costcenterlist
         with key costcenter = it_tmp_catsdb-kostl.
    if sy-subrc <> 0.
      continue.
    endif.
* Period
    call function 'DATE_TO_PERIOD_CONVERT'
      exporting
        i_date               = it_catsdb-workdate
*       I_MONMIT             = 00
        i_periv              = tka01-lmona
      importing
        e_buper              = it_tmp_catsdb-perid
*       E_GJAHR              =
      exceptions
        input_false          = 1
        t009_notfound        = 2
        t009b_notfound       = 3
        others               = 4.
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.
* Actual Quantity
    it_tmp_catsdb-actqty = it_catsdb-catshours.
* Collect data
    collect it_tmp_catsdb.
    clear it_tmp_catsdb.
    clear it_catsdb.
  endloop.

  clear it_tmp_catsdb.

endform.                    " READ_FR_CATSDB

*&---------------------------------------------------------------------*
*&      Form  ENQUEUE_ZTCO_NMHHRTRANS
*&---------------------------------------------------------------------*
*       Enqueue
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form enqueue_ztco_nmhhrtrans.

  data : lv_perid like ztco_nmhhrtrans-perid.

  lv_perid = p_frper.

  do 16 times .
    if lv_perid =< p_toper.
      call function 'ENQUEUE_EZ_ZTCO_NMHHRTR'
        exporting
          mode_ztco_nmhhrtrans       = 'E'
          mandt                     = sy-mandt
          gjahr                     = p_gjahr
          perid                     = lv_perid
*         KOSTL                     =
*         LSTAR                     =
*         X_GJAHR                   = ' '
*         X_PERID                   = ' '
*         X_KOSTL                   = ' '
*         X_LSTAR                   = ' '
*         _SCOPE                    = '2'
*         _WAIT                     = ' '
*         _COLLECT                  = ' '
        exceptions
          foreign_lock              = 1
          system_failure            = 2
          others                    = 3.

      if sy-subrc <> 0.
        message id sy-msgid type sy-msgty number sy-msgno
                with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      endif.
    endif.
* Period Counting
    lv_perid = lv_perid  + 1.

  enddo.
endform.                    " ENQUEUE_ZTCO_NMHHRTRANS

*&---------------------------------------------------------------------*
*&      Form  DELETE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form delete_data.
* local Data definition
  data : lv_answer .
  data : lv_title(80).
* Period Range
  clear : r_perid, r_perid[].
* Building Period Range
  r_perid-low    = p_frper.
  r_perid-high   = p_toper.
  r_perid-sign   = 'I'.
  r_perid-option = 'BT'.
  append r_perid.
  clear  r_perid.

* message
  clear : lv_answer,  lv_title.
  concatenate 'All data will be lost ' p_gjahr '/' p_frper '/' p_toper
         into lv_title.

  call function 'POPUP_TO_CONFIRM_STEP'
    exporting
*     DEFAULTOPTION        = 'Y'
      textline1            = lv_title
      textline2            = 'In Table - ZTCO_NMHHRTRANS'
      titel                = 'Delete DATA in Table'
*     START_COLUMN         = 25
*     START_ROW            = 6
*     CANCEL_DISPLAY       = 'X'
    importing
      answer               = lv_answer.

  if  lv_answer <> 'J'.
    message e043.
  endif.

* whenever running this program
* All data - Period relative - should be deleted and replaced
* with new records
  delete from ztco_nmhhrtrans
         where gjahr = p_gjahr
           and perid in r_perid.

* No Check Subrc

endform.                    " DELETE_DATA

*&---------------------------------------------------------------------*
*&      Form  READ_CCTR
*&---------------------------------------------------------------------*
*       Read CCtrs for retrieval.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_cctr.
* Making an internal table for CCtr to select data
  data : lv_datum like sy-datum.
  concatenate p_gjahr p_frper+1(2) '01' into lv_datum.

  clear : it_costcenterlist, it_costcenterlist[],
          it_return,         it_return[].

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

* Cost center
  clear : pnpkostl, pnpkostl[].
  loop at it_costcenterlist.
    pnpkostl-low    = it_costcenterlist-costcenter.
    pnpkostl-sign   = 'I'.
    pnpkostl-option = 'EQ'.
    append pnpkostl.
    clear  pnpkostl.
  endloop.

endform.                    " READ_CCTR

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
*&      Form  READ_BF_QUAN
*&---------------------------------------------------------------------*
*       Read BackFlush Quantity
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_bf_quan.
* Using COSL
* It is NOT sure that ALL data in COSL were summurized ONLY from B/F.
* An inspection is On-going now.  (2003.10.07)
* Some part of bellow query can be changed
* in case that the conclusion is not made or changed

* IN HMMA Business Flow , Only B/F quantity will be summed up at COSL
* (From Consultant) (2003.10.08)

* Quantity Fields -> Activity qty    ( LST001~ LST016)

* Read Dynamic Fields Name
  perform read_field_name_from_dd_cosl.

* Set Object KEY
  perform set_obj_key.

* Read AT Quantity DATA from COSS
  perform read_at_quan_fr_cosl.

* Re-org
  perform re_org_cosl_data.

endform.                    " READ_BF_QUAN

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

* read DD infor. COSS Key Part
  perform read_dd_info  tables it_et_fieldlist
                        using  'ZSCO_COSL_KEY01'.

* read DD infor. COSS Value Part (Total Quantity)
  perform read_dd_info  tables it_et_fieldlist
                        using  'ZSCO_COSL_LST01'.

endform.                    " READ_FIELD_NAME_FROM_DD_COSL

*&---------------------------------------------------------------------*
*&      Form  READ_DD_INFO
*&---------------------------------------------------------------------*
*       Read DD information
*----------------------------------------------------------------------*
*      -->IT_l_ET_FIELDLIST  Field-List Table
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
       exporting
            ic_tabname   = gv_ci_tabname
       tables
            et_fieldlist = it_l_fdlist
       exceptions
            not_found    = 1
            others       = 2.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.
  append lines of  it_l_fdlist       to it_l_et_fieldlist.

endform.                    " READ_DD_INFO

*&---------------------------------------------------------------------*
*&      Form  READ_AT_QUAN_FR_COSL
*&---------------------------------------------------------------------*
*       Read AT Quantity Total DATA from COSL
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_at_quan_fr_cosl.
  clear : it_cosl, it_cosl[].
  clear cosl.
  select (it_et_fieldlist)
         into corresponding fields of table it_cosl
         from cosl
          for all entries in it_obj_cctr_at
        where lednr = '00'
          and objnr = it_obj_cctr_at-objnr
          and gjahr = p_gjahr
          and wrttp = p_wrttp
          and versn = p_versn
          and vrgng in s_vrgng.
  clear : it_cosl.
endform.                    " READ_AT_QUAN_FR_COSL

*&---------------------------------------------------------------------*
*&      Form  SET_OBJ_KEY
*&---------------------------------------------------------------------*
*       Set Object Key
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_obj_key.
  clear : it_obj_cctr_at, it_obj_cctr_at[].
  loop at it_costcenterlist.
    call function 'K_LSTAR_OBJECT_KEY_GET'
         exporting
              kokrs = p_kokrs
              kostl = it_costcenterlist-costcenter
              lstar = p_lstar
         importing
              objnr = it_obj_cctr_at-objnr.
    it_obj_cctr_at-kostl = it_costcenterlist-costcenter.
    it_obj_cctr_at-lstar = p_lstar.
    append it_obj_cctr_at. clear it_obj_cctr_at.
    clear it_costcenterlist.
  endloop.
  clear : it_obj_cctr_at.
endform.                    " SET_OBJ_KEY

*&---------------------------------------------------------------------*
*&      Form  RE_ORG_COSL_DATA
*&---------------------------------------------------------------------*
*       reorganization - period split
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form re_org_cosl_data.

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
* Unit
    it_tmp_cosl-unit  = it_cosl-meinh.
*
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
* Collect
      collect it_tmp_cosl.
* Period Counter
      lv_cnt = lv_cnt + 1.
    enddo.
    clear it_tmp_cosl.
    clear it_cosl.
  endloop.
  clear it_tmp_cosl.
endform.                    " RE_ORG_COSL_DATA

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
*&      Form  ADD_UP_DATA
*&---------------------------------------------------------------------*
*       Aggregate data from B/F and TimeSheet
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form add_up_data.
* TimeSheet  (Summed)
  clear it_tmp_catsdb.  sort it_tmp_catsdb by gjahr perid kostl lstar.
* B/F - COSL (Summed)
  clear it_tmp_cosl.    sort it_tmp_cosl   by gjahr perid kostl lstar.
* Cntr -> IT_COSTCENTERLIST
  clear it_costcenterlist. sort it_costcenterlist by costcenter.

* Unit Conversion
  loop at it_tmp_catsdb.
    if it_tmp_catsdb-unit <> 'STD'.
      perform unit_conv using it_tmp_catsdb-unit
                              it_tmp_catsdb-actqty.
      modify it_tmp_catsdb.
    endif.
    clear  it_tmp_catsdb.
  endloop.

  loop at it_tmp_cosl.
    if it_tmp_cosl-unit <> 'STD'.
      perform unit_conv using it_tmp_cosl-unit
                              it_tmp_cosl-actqty.
      modify it_tmp_cosl.
    endif.
    clear  it_tmp_cosl.
  endloop.

* Agrregating
  clear : it_ztco_nmhhrtrans, it_ztco_nmhhrtrans[].

  append lines of it_tmp_catsdb to it_ztco_nmhhrtrans.
  clear it_ztco_nmhhrtrans.
  append lines of it_tmp_cosl   to it_ztco_nmhhrtrans.
  clear it_ztco_nmhhrtrans.

* Collecting
  clear : it_post, it_post[].
  loop at it_ztco_nmhhrtrans.
    move-corresponding it_ztco_nmhhrtrans to it_post.
* Variance
    it_post-vaeqty = it_post-actqty - it_post-curqty.
    collect it_post.
    clear   it_post.
    clear it_ztco_nmhhrtrans.
  endloop.
  clear   it_post.

endform.                    " ADD_UP_DATA

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
*    ROUND_SIGN                 = ' '
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
     unit_in_not_found          = 8
     unit_out_not_found         = 9
     others                     = 10.

  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

  p_unit = 'STD'.

endform.                    " UNIT_CONV

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
*     I_STRUCTURE_NAME               = 'ZTCO_NMHHRTRANS'
*     IS_LAYOUT                      =
      it_fieldcat                    = it_fieldcat[]
*     IT_EXCLUDING                   =
*     IT_SPECIAL_GROUPS              =
      it_sort                        = it_sort[]
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
*       FORM PF_STATUS                                                *
*---------------------------------------------------------------------*
*       PF_STATUS                                                     *
*---------------------------------------------------------------------*
form pf_status using  extab type slis_t_extab.
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
* For POST - DB Update
    when 'UPDA' .
      perform update.
  endcase.

endform.

*---------------------------------------------------------------------*
*       FORM BASIC_TOP_OF_PAGE                                        *
*---------------------------------------------------------------------*
*       Display Title                                                 *
*---------------------------------------------------------------------*
form basic_top_of_page.
  write : / 'Controlling Area                         : '
            , p_kokrs.
  write : / 'Fiscal Year/Period/Version/Activity Type : '
            , p_gjahr, '/', p_frper, '~', p_toper, '/', p_versn
            , '/', p_lstar.
  write : / 'Value Type/CO business transaction       : '
            , p_wrttp, '/',  s_vrgng-low, '~',  s_vrgng-high.

endform.

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

* Sort IT_POST.
  sort it_post  by gjahr perid kostl lstar.
  clear it_post.

  it_sort-fieldname = 'PERID'.
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
  perform build_fieldcat  using
    'IT_POST'    'PERID'  'X'            space    space
    space        '3'      'Period'       space    space    space.

  perform build_fieldcat  using
    'IT_POST'    'KOSTL'  'X'            space    space
    space        '10'     'Cost Center'  space    space    space.

  perform build_fieldcat  using
    'IT_POST'    'LSTAR'  'X'            space    space
    space        '6'      'AT'           space    space    space.
* Value
* Quantity
  perform build_fieldcat using
    'IT_POST'    'ACTQTY'  space         'X'      space
    space        '18'  'Actual M/H'      'QUAN'   'UNIT'  'IT_POST'.

  perform build_fieldcat using
    'IT_POST'    'CURQTY'  space         'X'      space
    space        '18'  'Current M/H'     'QUAN'   'UNIT'  'IT_POST'.

  perform build_fieldcat using
    'IT_POST'    'VAEQTY'  space         'X'      space
    space        '18'  'Variance M/H'   'QUAN'   'UNIT'  'IT_POST'.
* Unit
  perform build_fieldcat using
    'IT_POST'    'UNIT'   space          space    space
    space        '4'      'UNIT'         'UNIT'   space    space.

endform.                    " FIELDCAT_INIT

*&---------------------------------------------------------------------*
*&      Form  Build_FieldCAT
*&---------------------------------------------------------------------*
*       Field_CAT
*----------------------------------------------------------------------*
*      -->P_0065   text
*      -->P_0066   text
*      -->P_0067   text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_0071   text
*      -->P_0072   text
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

endform.                    " Build_FieldCAT

*&---------------------------------------------------------------------*
*&      Form  UPDATE
*&---------------------------------------------------------------------*
*       Update.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form update.
  clear ztco_nmhhrtrans.
  loop at it_post.
* LOG
    it_post-erdat = sy-datum.
    it_post-erzet = sy-uzeit.
    it_post-ernam = sy-uname.
    clear ztco_nmhhrtrans.
    move-corresponding  it_post to ztco_nmhhrtrans.
    insert ztco_nmhhrtrans .
    if sy-subrc <> 0.
      message e044.
    endif.
    clear it_post.
  endloop.
* Success
  message s009 with 'Data Creation'.
endform.                    " UPDATE

*&---------------------------------------------------------------------*
*&      Form  READ_FR_CATSDB2
*&---------------------------------------------------------------------*
*       Read TIMESHEET data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_fr_catsdb2.

* Ingnore A/A type
* HR source has no indicator to check if supportive hour or
* non-supportive hour

  data : pnpbegda like qppnp-begda,
         pnpendda like qppnp-endda,
         pnptimr6 like qppnp-timr6.
  data : sw_zl    like rptxxxxx-kr_feld3.
  ranges : lgart for t512w-lgart.
  data : it_l_rsparams like standard table of rsparams
                       with header line .
*
  call function 'RS_REFRESH_FROM_SELECTOPTIONS'
    exporting
      curr_report           = 'ZACO03U_HRMH'
*   IMPORTING
*     SP                    =
    tables
      selection_table       = it_l_rsparams
    exceptions
      not_found             = 1
      no_report             = 2
      others                = 3.

  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

* Put data
  pnptimr6 = 'X'.
  sw_zl    = 'X'.

  clear : lgart, lgart[].
  lgart[] = s_lgart[].

* Cost center
  clear r_workdate.
  read table r_workdate index 1.
  pnpbegda = r_workdate-low.
  pnpendda = r_workdate-high.

  submit zaco03u_hrmh
*    VIA SELECTION-SCREEN
    and return
    with selection-table it_l_rsparams
    with pnptimr6 = pnptimr6
    with pnpbegda = pnpbegda
    with pnpendda = pnpendda
    with sw_zl = sw_zl
    with lgart in lgart
    with pnpkostl in pnpkostl.

*<data_tab>
  import time_data_zes   = time_data_zes
         time_data_saldo = time_data_saldo
         time_data_zl    = time_data_zl
         data_tab        = data_tab
         from memory id 'HRM'.


** Re-org.
  data : lv_tabcol(30).
  data : lv_kostl(5) value 'KOSTL',
         lv_anzhl(5) value 'ANZHL',
         lv_pernr(5) value 'PERNR',
         lv_datum(5) value 'DATUM',
         lv_persg_txt(9) value 'PERSG_TXT'.

  field-symbols : <fstab> type table,
                  <fswa>  type any,
                  <fsfn>  type any,
                  <fsval> type any.

  concatenate data_tab '[' ']' into lv_tabcol.
  assign (lv_tabcol) to <fstab>.


  assign local copy of initial line of <fstab> to <fswa>.
* If no data found in HR tables
  if sy-subrc <> 0.
    message e000 with text-101.
  endif .

  loop at <fstab> assigning <fswa>.
    assign lv_persg_txt to <fsfn>.
    assign component <fsfn> of structure <fswa> to <fsval>.

*    check <fsval> eq 'Active Team Member' or
*          <fsval> eq 'Inpatriate'.

    move <fsval> to lt_tmp_catsdb-pernr.
    clear lt_tmp_catsdb.
    lt_tmp_catsdb-gjahr = p_gjahr.
    lt_tmp_catsdb-perid = p_frper.
    lt_tmp_catsdb-lstar = p_lstar.
    lt_tmp_catsdb-unit = 'STD'.
* CCTR -KOSTL
    assign lv_kostl to <fsfn>.
    assign component <fsfn> of structure <fswa> to <fsval>.
    lt_tmp_catsdb-kostl = <fsval> .
* MAN_HR -ANZHL
    assign lv_anzhl to <fsfn>.
    assign component <fsfn> of structure <fswa> to <fsval>.
*    IT_TMP_CATSDB-ACTQTY = <FSVAL> .
    move <fsval> to lt_tmp_catsdb-actqty.
* Date - DATUM
    assign lv_datum to <fsfn>.
    assign component <fsfn> of structure <fswa> to <fsval>.
    move <fsval> to lt_tmp_catsdb-datum.
* PERSONAL NO - DATUM
    assign lv_pernr to <fsfn>.
    assign component <fsfn> of structure <fswa> to <fsval>.
    move <fsval> to lt_tmp_catsdb-pernr.

* Collect data
    collect lt_tmp_catsdb.
    clear lt_tmp_catsdb.
    clear <fswa>.
  endloop.

* Get Net Hour
  data: lw_hour_gap type i,
        lw_rest     like it_tmp_catsdb-actqty.

  clear : it_tmp_catsdb, it_tmp_catsdb[].
  loop at lt_tmp_catsdb.
    clear : it_tmp_catsdb.
    move: lt_tmp_catsdb-gjahr to it_tmp_catsdb-gjahr,
          lt_tmp_catsdb-perid to it_tmp_catsdb-perid,
          lt_tmp_catsdb-lstar to it_tmp_catsdb-lstar,
          lt_tmp_catsdb-unit  to it_tmp_catsdb-unit,
          lt_tmp_catsdb-kostl to it_tmp_catsdb-kostl.
    if lt_tmp_catsdb-kostl(2) ne 'MX'.
      move: lt_tmp_catsdb-actqty to it_tmp_catsdb-actqty.
    else.
      if     lt_tmp_catsdb-actqty <= 4.
        it_tmp_catsdb-actqty = lt_tmp_catsdb-actqty - '0.25'.
      elseif lt_tmp_catsdb-actqty > 4 and
             lt_tmp_catsdb-actqty <= 8.
        it_tmp_catsdb-actqty = lt_tmp_catsdb-actqty - '0.42'.
      elseif lt_tmp_catsdb-actqty > 8.
        lw_hour_gap = lt_tmp_catsdb-actqty - 8.
        lw_rest = lw_hour_gap * 5 / 60.
        it_tmp_catsdb-actqty = lt_tmp_catsdb-actqty - '0.42' - lw_rest.
      endif.
    endif.

    collect it_tmp_catsdb.
  endloop.
* Remark
* An error was found in HR program .
*  -> rescan cctr.
  loop at it_tmp_catsdb.
    clear it_costcenterlist.
    read table it_costcenterlist
      with key costcenter = it_tmp_catsdb-kostl.
    if sy-subrc <> 0.
      delete it_tmp_catsdb.
    endif.
    clear it_tmp_catsdb.
  endloop.

endform.                    " READ_FR_CATSDB2

*&---------------------------------------------------------------------*
*&      Form  SET_DEF_WAGE_TYPE
*&---------------------------------------------------------------------*
*       Set Default Wage Type
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_def_wage_type.

  clear : s_lgart, s_lgart[].
* Regular
  s_lgart-low = '0200'.
  s_lgart-option = 'EQ'.
  s_lgart-sign   = 'I'.
  append s_lgart.
  clear  s_lgart.
* Over Time
  s_lgart-low = '0900'.
  s_lgart-option = 'EQ'.
  s_lgart-sign   = 'I'.
  append s_lgart.
  clear  s_lgart.

  s_lgart-low = '0901'.
  s_lgart-option = 'EQ'.
  s_lgart-sign   = 'I'.
  append s_lgart.
  clear  s_lgart.

  s_lgart-low = '0902'.
  s_lgart-option = 'EQ'.
  s_lgart-sign   = 'I'.
  append s_lgart.
  clear  s_lgart.

endform.                    " SET_DEF_WAGE_TYPE

*&---------------------------------------------------------------------*
*&      Form  READ_FR_CATSDB3
*&---------------------------------------------------------------------*
*       Cal. Supportive and Not-Supportive Working Hour
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_fr_catsdb3.

* TimeSheet  (Summed)
  clear it_tmp_catsdb.  sort it_tmp_catsdb by gjahr perid kostl lstar.

* Local Data definition
  data : it_l_catsdb3 like standard table of it_catsdb
                      with header line .

* Select data
  clear : it_l_catsdb3, it_l_catsdb3[].
  select workdate  skostl  lstar   rkostl
         awart     unit    status  catshours
         into corresponding fields of table it_l_catsdb3
         from catsdb
        where
*             AWART    IN R_L_AWART
              status   in s_status
          and workdate in r_workdate.

  if  it_l_catsdb3[] is initial.
*    MESSAGE E026.
  endif.

* All information for sender cctr and receiver cctr must be
* in each record
  delete it_l_catsdb3
   where skostl    eq space
      or rkostl    eq space
      or catshours eq space.


  data : begin of it_l_calwsig occurs 0,
            kostl     like catsdb-skostl  , "CCtr
            catshours like catsdb-catshours.
  data : end of   it_l_calwsig.

  clear : it_l_calwsig, it_l_calwsig[].
  loop at it_l_catsdb3.
* Sender
    it_l_calwsig-kostl     = it_l_catsdb3-skostl.
    it_l_calwsig-catshours = it_l_catsdb3-catshours * ( -1 ).
    collect it_l_calwsig.
    clear   it_l_calwsig.
* Receiver
    it_l_calwsig-kostl     = it_l_catsdb3-rkostl.
    it_l_calwsig-catshours = it_l_catsdb3-catshours .
    collect it_l_calwsig.
    clear   it_l_calwsig.
    clear it_l_catsdb3.
  endloop.

* Cal.
  data : it_l_catadd like standard table of it_tmp_catsdb
                     with header line .
  loop at it_l_calwsig.
    loop at it_tmp_catsdb where  kostl  = it_l_calwsig-kostl.
      it_tmp_catsdb-actqty = it_tmp_catsdb-actqty
                           + it_l_calwsig-catshours.
      modify it_tmp_catsdb.
      clear  it_tmp_catsdb.
    endloop.
    if sy-subrc <> 0.
      move-corresponding it_l_calwsig to it_l_catadd.
      it_l_catadd-gjahr = p_gjahr.
      it_l_catadd-perid = p_frper.
      it_l_catadd-lstar = p_lstar.
      it_l_catadd-unit = 'STD'.
      it_l_catadd-actqty = it_l_calwsig-catshours.
      collect it_l_catadd.
    endif.
    clear it_l_catadd.
    clear it_l_calwsig.
  endloop.

  loop at it_l_catadd.
    clear  it_tmp_catsdb.
    move-corresponding it_l_catadd to it_tmp_catsdb.
    collect it_tmp_catsdb.
    clear   it_tmp_catsdb.
  endloop.

** Sender '-'
*  SORT IT_L_CATSDB3 BY SKOSTL .
*  LOOP AT IT_TMP_CATSDB.
*    LOOP AT IT_L_CATSDB3 WHERE  SKOSTL  = IT_TMP_CATSDB-KOSTL.
*      IT_TMP_CATSDB-ACTQTY = IT_TMP_CATSDB-ACTQTY
*                           - IT_L_CATSDB3-CATSHOURS.
*    ENDLOOP.
*    MODIFY IT_TMP_CATSDB.
*    CLEAR  IT_TMP_CATSDB.
*  ENDLOOP.
*
** Receiver '-'
*  SORT IT_L_CATSDB3 BY RKOSTL .
*  LOOP AT IT_TMP_CATSDB.
*    LOOP AT IT_L_CATSDB3 WHERE  RKOSTL  = IT_TMP_CATSDB-KOSTL.
*      IT_TMP_CATSDB-ACTQTY = IT_TMP_CATSDB-ACTQTY
*                           + IT_L_CATSDB3-CATSHOURS.
*    ENDLOOP.
*    MODIFY IT_TMP_CATSDB.
*    CLEAR  IT_TMP_CATSDB.
*  ENDLOOP.

  clear  it_tmp_catsdb.

endform.                    " READ_FR_CATSDB3
*&---------------------------------------------------------------------*
*&      Form  read_co_mh
*&---------------------------------------------------------------------*
form read_co_mh.
  tables: ztco_mhat.
  data: i_mhat like ztco_mhat occurs 0 with header line.

  select * into table i_mhat
    from ztco_mhat
    where gjahr = p_gjahr
      and perid = p_frper
      and kostl in pnpkostl
      and lgart between '1' and '2'.  "regular, ot

  clear : it_tmp_catsdb, it_tmp_catsdb[].
  it_tmp_catsdb-gjahr = p_gjahr.
  it_tmp_catsdb-perid = p_frper.
  it_tmp_catsdb-lstar = p_lstar.
  it_tmp_catsdb-unit = 'STD'.


  loop at i_mhat.
    it_tmp_catsdb-kostl  = i_mhat-kostl.
    it_tmp_catsdb-actqty = i_mhat-netmh.
    collect it_tmp_catsdb.
  endloop.

endform.                    " read_co_mh
