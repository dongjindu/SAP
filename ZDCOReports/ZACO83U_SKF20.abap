************************************************************************
* Program Name      : ZACO83U_SKF20
* Author            : Eun Hwa , Jung
* Creation Date     : 2004.04.20
* Specifications By : Dong-Hwan Kim
* Pattern           : Report 1-1
* Development Request No :  UD1K909742
* Addl Documentation:
* Description       :
*                   Create/Allocate maintenance activity qty(actual)
* the BDC structures for BATCH INPUT processing
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

report  zaco83u_skf20 message-id zmco.

** type-pools
type-pools: slis.

tables : csks, catsdb , cssl, cosl, coss, cosr.

** For OBJECT KEY
data : begin of wa_obj ,
        objnr  like  coss-objnr,
        kostl  like  csks-kostl,
        lstar  like  csla-lstar,
       end of wa_obj.

data : it_obj_cctr_at   like standard table of wa_obj
                        with header line .

* For DD data
data : gv_ci_tabname     type ddobjname .
data : it_et_fieldlist   like table of rfvicp_ddic_tabl_fieldname
                         with header line.

** Internal Table

data: begin of it_cosr occurs 0,
        objnr type cosr-objnr,
        gjahr type cosr-gjahr,
        wrttp type cosr-wrttp,
        versn type cosr-versn,
        stagr type cosr-stagr,
        meinh type cosr-meinh,
        sme001 type cosr-sme001,
        sme002 type cosr-sme002,
        sme003 type cosr-sme003,
        sme004 type cosr-sme004,
        sme005 type cosr-sme005,
        sme006 type cosr-sme006,
        sme007 type cosr-sme007,
        sme008 type cosr-sme008,
        sme009 type cosr-sme009,
        sme010 type cosr-sme010,
        sme011 type cosr-sme011,
        sme012 type cosr-sme012,
        sme013 type cosr-sme013,
        sme014 type cosr-sme014,
        sme015 type cosr-sme015,
        sme016 type cosr-sme016,
      end of it_cosr.

data: begin of it_tmp_cosr occurs 0,
*        OBJNR TYPE COSR-OBJNR,
*        PRZNR TYPE CBPR-PRZNR,
        gjahr type cosr-gjahr,
        perid  like coejl-perbl,
        kostl  like  cssl-kostl,
        lstar  like  cssl-lstar,
        stagr type cosr-stagr,
        meinh type cosr-meinh,
        smexxx type cosr-sme001,
        a_qty   type cosr-sme001,
      end of it_tmp_cosr.

data: begin of it_tmp_cosr_sum occurs 0,
        gjahr type cosr-gjahr,
        perid  like coejl-perbl,
        stagr type cosr-stagr,
        smexxx type cosr-sme001,
      end of it_tmp_cosr_sum.

** Main internal table
data: begin of it_catsdb occurs 0,
        pernr type catsdb-pernr,
        workdate type catsdb-workdate,
        skostl type catsdb-skostl,
        lstar type catsdb-lstar,
        rkostl type catsdb-rkostl,
        catshours(9) type p decimals 2,
      end of it_catsdb.

data: begin of it_catsdb_sum occurs 0,
*        PERNR TYPE CATSDB-PERNR,
*        WORKDATE TYPE CATSDB-WORKDATE,
        skostl type catsdb-skostl,
        lstar type catsdb-lstar,
*        RKOSTL TYPE CATSDB-RKOSTL,
        catshours(9) type p decimals 2,
      end of it_catsdb_sum.

data: begin of it_coss occurs 0,
        kostl  like  cssl-kostl,
        lstar  like  cssl-lstar,
        r_kostl  like  cssl-kostl,
        r_lstar  like  cssl-lstar,
        parob_b  like  coss-objnr.
        include structure zsco_coss_key01.
data : end of   it_coss.

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

data : begin of it_sum occurs 0,
           gjahr  like cssl-gjahr,
           perid  like coejl-perbl,
*           KOSTL LIKE CSSL-KOSTL,
*           LSTAR LIKE ZTCO_MHHRTRANS-LSTAR,
           objnr  like cssl-objnr,
           actqty like cosl-lst001,
       end of it_sum.

data : begin of it_objnr occurs 0,
            objnr  like  cosr-objnr,
       end of it_objnr.

data : begin of it_objnr2 occurs 0,
            objnr  like  cosr-objnr,
       end of it_objnr2.

* for reporting
data : begin of it_report2 occurs 0,
        gjahr   like  cosl-gjahr,
        perid   like  coejl-perbl,
        kostl   like  csks-kostl,
        objnr   like  cssl-objnr,
        lstar   like  csla-lstar,
        qty     like  cosl-lst001,
        a_qty   like  cosl-lst001,
        p_kostl   like  csks-kostl,
        p_lstar   like  csla-lstar,
        r_kostl like  cssl-kostl,
        r_objnr like  cssl-objnr,
        r_lstar like  cssl-lstar,
        actqty  like  cosl-lst001,
        curqty  like  cosl-lst001,
        vaeqty  like  cosl-lst001,
        unit    like  cosl-meinh,
        bldat   like  coheader-bldat,          " doc date
      end of it_report2.

data : begin of it_report occurs 0,
        gjahr   like  cosl-gjahr,
        perid   like  coejl-perbl,
        kostl   like  csks-kostl,
        objnr   like  cssl-objnr,
        lstar   like  csla-lstar,
        qty     like  cosl-lst001,
        a_qty   like  cosl-lst001,
        p_kostl   like  csks-kostl,
        p_lstar   like  csla-lstar,
        r_kostl like  cssl-kostl,
        r_objnr like  cssl-objnr,
        r_lstar like  cssl-lstar,
        actqty  like  cosl-lst001,
        curqty  like  cosl-lst001,
        vaeqty  like  cosl-lst001,
        unit    like  cosl-meinh,
        bldat   like  coheader-bldat,          " doc date
      end of it_report.

* by ig.moon 4/21/2010 {
data: begin of it_coss_meg occurs 0,
        lstar  like  cssl-lstar,
        kostl  like  cssl-kostl.
        include structure zsco_coss_key01.
        include structure zsco_coss_meg01.
data : end of   it_coss_meg.
* }


** For BAPI
data : it_costcenterlist like standard table of bapi0012_cclist
                         with header line.
data : it_return         like standard table of bapiret2
                         with header line.

data : wa_doc_header like bapidochdrp .
data : it_doc_items  like standard table of bapiaaitm
                      with header line.

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
data : ac_qty_tot(13) type p decimals 3.

ranges : rs_date for catsdb-workdate.
* Globale Daten
include rptbal01.


*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
selection-screen begin of block bl1 with frame title text-001.
parameters :
               p_kokrs like csks-kokrs memory id cac obligatory,
               p_gjahr like cobk-gjahr memory id gjr obligatory,
               p_perid like cosp-perbl memory id bpe obligatory,
               p_versn like cobk-versn memory id kvt obligatory,
              p_kostl like csks-kostl default '0000055103' no-display,
               p_stagr like cosr-stagr default 'CS301' no-display,
               p_trun(1).
parameters : p_lstar like csla-lstar            default 'MAN_HR'
                                                     no-display.
selection-screen skip 1.
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

*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
start-of-selection.
* preparation posting date.
  perform cal_post_date.

**// Mod. By Hyung Jin Youn 2004.07.16.
** Change of Source data for Timesheet
** Read HR 'CATSDB'.
*  PERFORM READ_CATSDB.
*  perform read_fr_catsdb2.
**// End of Mod.
  perform read_co_mh.

* Read Sender_Quantity Information from COSR
  perform read_cosr.
* Calculating allocation Qty
  perform cal_allocated_qty.
* Read Receiver information.
  perform read_coss_parob.
* Read Dynamic Fields Name
  perform read_field_name_from_dd_cosl.
* Read Receiver_Quantity Information from COSL
  perform read_cosl_qty.
* Calculating Qty and preparation of reporting data.
  perform cal_plan_qty.
* Preparation of ALV
  perform pre_report_adj.

*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
end-of-selection.
* Call ALV LIST
  perform call_alv_list.

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
**&---------------------------------------------------------------------
**
**&      Form  READ_CATSDB
**&---------------------------------------------------------------------
**
**       read ' HR time sheet' information
**----------------------------------------------------------------------
**
*form read_catsdb.
*
** Read time sheet data
*  clear : it_catsdb, it_catsdb[].
*  clear catsdb.
*  select *
*         into corresponding fields of table it_catsdb
*         from catsdb
*          where workdate in rs_date
*           and skostl = p_kostl
**           AND LSTAR  = 'MNT_HR'
*           and ( rkostl = it_tmp_cosl-kostl or rkostl = ' ' )
*           and kokrs = p_kokrs
*           and status = '30'.    " Processing status = Approved'
*  clear  it_catsdb.
*
*  sort it_catsdb by skostl.
*  loop at it_catsdb.
*    if it_catsdb-rkostl = ' '.
*      move-corresponding it_catsdb to it_catsdb_sum.
*    else.
*      move it_catsdb-rkostl    to it_catsdb_sum-skostl.
*      move it_catsdb-catshours to it_catsdb_sum-catshours.
*    endif.
*    collect it_catsdb_sum.
*    clear it_catsdb_sum.
*  endloop.
*  clear it_catsdb.
*
*
*endform.                    " READ_CATSDB
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
    when 'POST'.
      perform post_std  using ucomm.
  endcase.

endform.
*&---------------------------------------------------------------------*
*&      Form  PRE_REPORT_ADJ
*&---------------------------------------------------------------------*
*       The Preparation for ALV List
*----------------------------------------------------------------------*
form pre_report_adj.

* Building Field Cat.
  perform fieldcat_init .

* Sort
  sort it_report by gjahr perid kostl lstar r_kostl r_lstar.
  clear it_report.

  it_sort-fieldname = 'KOSTL'.
  it_sort-up        = 'X'.
  it_sort-expa      = 'X'.
  it_sort-subtot    = 'X'.
  append it_sort.
*  IT_SORT-FIELDNAME = 'LSTAR'.
*  IT_SORT-UP        = 'X'.
*  IT_SORT-EXPA      = 'X'.
*  IT_SORT-SUBTOT    = 'X'.
*  APPEND IT_SORT.
*


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

* Key

  perform build_fieldcat using
   'IT_REPORT' 'KOSTL'  'X'            space    space
   space        '10'      'Section'      space    space    space.

*  PERFORM BUILD_FIELDCAT USING
*   'IT_REPORT' 'LSTAR'  'X'            SPACE    SPACE
*   SPACE        '6'      'Sen.AT'           SPACE    SPACE    SPACE.

  perform build_fieldcat using
    'IT_REPORT' 'P_KOSTL'  'X'            space    space
    space        '10'      'Sender CC'   space    space    space.

  perform build_fieldcat using
   'IT_REPORT' 'P_LSTAR'  'X'            space    space
   space        '7'      'Sen.AT'        space    space    space.

  perform build_fieldcat using
    'IT_REPORT' 'R_KOSTL' 'X'            space    space
    space        '10'      'Rec. C/C'       space    space    space.

  perform build_fieldcat using
    'IT_REPORT' 'R_LSTAR'  'X'            space    space
    space        '6'      'Rec.AT'           space    space    space.


** Value

*  PERFORM BUILD_FIELDCAT USING
*    'IT_REPORT' 'UNIT'  SPACE            SPACE    SPACE
*    SPACE   '4'      'Unit'      'UNIT'   SPACE     SPACE.

  perform build_fieldcat using
    'IT_REPORT' 'QTY'  space             space    space
    'IT_REPORT'   '15'      'Section.Qty'       space    space    space.

  perform build_fieldcat using
    'IT_REPORT' 'A_QTY'  space            space    space
    'IT_REPORT'   '15'      'Alloc.Qty'         space    space    space.

  perform build_fieldcat using
    'IT_REPORT' 'ACTQTY'  space             'X'     space
    'IT_REPORT'   '15'      'Rec.Qty'  space    space space.

*  PERFORM BUILD_FIELDCAT USING
*    'IT_REPORT' 'CURQTY'  SPACE            SPACE    SPACE
*    SPACE '15'      'Sen.Actual Qty'    SPACE    SPACE SPACE.

  perform build_fieldcat using
    'IT_REPORT' 'VAEQTY'  space            'X'    space
    'IT_REPORT'   '15'      'Rec.Input Qty'      space   space  space .


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
       it_sort                        = it_sort[]
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
  write : / 'Test Run                     ', p_trun.
  write : / 'C/C:' , p_kostl, 'AT: MNT_HR' , '     Acty qty:' ,
ac_qty_tot.
  skip 1.

endform.
*&---------------------------------------------------------------------*
*&      Form  CAL_POST_DATE
*&---------------------------------------------------------------------*
*       calculate posting date
*----------------------------------------------------------------------*
form cal_post_date.

*** <  Posting , hard coding > **
* doc date / posting date   =  ' year/month/25 '
  clear gv_post_date.
  concatenate  p_gjahr  p_perid+1(2) '25'  into gv_post_date.


* CREATE RANGES FOR WORKDATE.
  data :  dat1 like sy-datum,
          dat2 like sy-datum.
  clear : dat1, dat2.

  concatenate p_gjahr p_perid+1(2) '01' into dat1.
  call function 'RP_LAST_DAY_OF_MONTHS'
       EXPORTING
            day_in            = dat1
       IMPORTING
            last_day_of_month = dat2.

  rs_date-sign = 'I'.
  rs_date-option = 'BT'.
  rs_date-low = dat1.
  rs_date-high = dat2.
  append rs_date.   clear rs_date.


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


endform.                    " CHK_INPUT_VALUE
*&---------------------------------------------------------------------*
*&      Form  READ_COSL_QTY
*&---------------------------------------------------------------------*
*      Read Receiver_Quantity Information from COSL
*----------------------------------------------------------------------*
form read_cosl_qty.

  clear : it_cosl, it_cosl[].
  clear cosl.
  select (it_et_fieldlist)
         into corresponding fields of table it_cosl
         from cosl
          for all entries in it_coss
        where lednr = '00'
          and objnr = it_coss-parob_b
          and gjahr = p_gjahr
          and wrttp = '04'             " Actual
          and versn = p_versn.
  clear : it_cosl.

  delete it_cosl where objnr+16(6) ne 'MCH_HR'.


* Local Data definition
  field-symbols: <fs> type any.
  data : lv_lst_nam(30).
  data : lv_cnt  like  cosp-perbl.
*
  clear   it_cosl.
  clear : it_tmp_cosl, it_tmp_cosl[].

  clear : it_tmp_cosl, it_tmp_cosl[].
  loop at it_cosl.
    it_tmp_cosl-gjahr = p_gjahr.
    it_tmp_cosl-perid = p_perid.

    it_tmp_cosl-kostl = it_cosl-objnr+6(6).
    it_tmp_cosl-lstar = it_cosl-objnr+16(6).
    it_tmp_cosl-objnr = it_cosl-objnr.
    it_tmp_cosl-unit = it_cosl-meinh.

* Value Transferring
    clear lv_lst_nam.
    concatenate 'IT_COSL-'  'LST'  p_perid
          into lv_lst_nam.
    assign (lv_lst_nam) to <fs>.
    clear it_tmp_cosl-curqty.
    it_tmp_cosl-curqty = <fs>.

*   Unit Conversion
    if it_tmp_cosl-unit <> 'STD'.
      perform unit_conv using it_tmp_cosl-unit
                              it_tmp_cosl-curqty.
    endif.

* Collect
    collect it_tmp_cosl.
    clear   it_tmp_cosl.
    clear   it_cosl.
  endloop.
  clear it_tmp_cosl.
  clear it_cosl.




endform.                    " READ_COSL_QTY
*&---------------------------------------------------------------------*
*&      Form  READ_COSS_PAROB
*&---------------------------------------------------------------------*
*       Read Receiver information.
*----------------------------------------------------------------------*
form read_coss_parob.

  perform create_coss_object.

* by ig.moon 4/21/2010 {
  clear : it_coss_meg, it_coss_meg[].
  select *
         into corresponding fields of table it_coss_meg
         from coss
          for all entries in it_objnr2
        where lednr = '00'
          and objnr = it_objnr2-objnr      "IT_COSR-OBJNR
          and gjahr = p_gjahr
          and wrttp = '01'         " plan
          and versn = p_versn
          and vrgng = 'RKP7'.

  delete it_coss_meg where parob = ' '.

  loop at it_coss_meg.
    it_coss_meg-kostl = it_coss_meg-parob+6(10).
    it_coss_meg-lstar = it_coss_meg-parob+16(6).
    modify it_coss_meg.
    clear it_coss_meg.
  endloop.

  delete it_coss_meg where lstar ne 'MAN_HR'.
* }

* search for Partner object
  clear : it_coss, it_coss[].
  clear it_cosr.
  clear coss.
  select *
         into corresponding fields of table it_coss
         from coss
          for all entries in it_objnr2
        where lednr = '00'
          and objnr = it_objnr2-objnr      "IT_COSR-OBJNR
          and gjahr = p_gjahr
          and wrttp = '01'         " plan
          and versn = p_versn
          and vrgng = 'RKP7'.

*Issue number
*Requested by dhkim,20041007,changed by wskim
*---Start
*         and meg009 ne 0.
*---End

  clear it_coss.

  delete it_coss where parob = ' '.
  delete it_coss where parob+16(6) ne 'MAN_HR'.


  loop at it_coss.
    concatenate it_coss-parob(12) '    MCH_HR' into it_coss-parob_b.
    it_coss-kostl = it_coss-objnr+6(10).
    it_coss-lstar = it_coss-objnr+16(6).
    it_coss-r_kostl = it_coss-parob+6(10).
    it_coss-r_lstar = it_coss-parob+16(6).
    modify it_coss.
    clear  it_coss.
  endloop.


endform.                    " READ_COSS_PAROB

*&---------------------------------------------------------------------*
*&      Form  CAL_PLAN_QTY
*&---------------------------------------------------------------------*
*       caculating Qty and preparation of reporting data.
*----------------------------------------------------------------------*
form cal_plan_qty.

  sort it_coss by kostl.
*Making an it_report for reporting
  clear : it_report2, it_report2[].
  clear : it_tmp_cosr, it_coss.
  loop at it_coss.
    loop at it_tmp_cosr where kostl = it_coss-kostl.

      it_report2-gjahr   = p_gjahr.
      it_report2-perid   = p_perid.
      it_report2-kostl   = it_tmp_cosr-kostl.
      it_report2-objnr   = it_coss-objnr.
      it_report2-lstar   = it_coss-lstar.
      it_report2-qty     = it_tmp_cosr-smexxx.
      it_report2-a_qty     = it_tmp_cosr-a_qty.

      case it_report2-kostl.
        when '0000055001' or '0000055002' or '0000055003'
        or '0000055004'.
          it_report2-p_kostl   = '0000055103'.
        when '0000055005' or '0000055015'.
          it_report2-p_kostl   = '0000055107'.
      endcase.

*      it_report2-p_kostl   = p_kostl.                       " '55103'.
      it_report2-p_lstar   = 'MNT_HR'.
      it_report2-r_kostl = it_coss-r_kostl.
      it_report2-r_objnr = it_coss-parob_b.
      it_report2-r_lstar = it_coss-parob_b+16(6).    "R_LSTAR.
      it_report2-curqty  = it_tmp_cosr-a_qty.
      concatenate p_gjahr p_perid+1(2) '25'
                into it_report2-bldat.
      append it_report2.
      clear  it_report2.
    endloop.
  endloop.


  clear it_report2.
  loop at it_report2.
    clear it_tmp_cosl.
    read table    it_tmp_cosl
       with key gjahr = it_report2-gjahr
                perid = it_report2-perid
                objnr = it_report2-r_objnr.

    it_report2-actqty = it_tmp_cosl-curqty.
    modify it_report2.
    clear  it_report2.
  endloop.


  clear : it_sum, it_sum[].
  loop at it_report2.
    move-corresponding it_report2 to it_sum.
    collect it_sum.
    clear   it_sum.
  endloop.
  clear it_sum.


* Calculate PLAN QTY.
  data   tot_qty(9) type p decimals 3.
  data   flag(1).
  clear  tot_qty.

* by ig.moon 3/30/2010 {
  field-symbols: <fs3> type any.
  data : lv_meg_nam2(30).
  sort it_coss_meg by gjahr kostl objnr.
* }


  clear : it_report, it_report[].
  clear it_report2.
  loop at it_report2.

    at new objnr.
      clear : tot_qty, flag, it_sum.
      read table  it_sum
           with key gjahr = it_report2-gjahr
                    perid = it_report2-perid
                    objnr = it_report2-objnr.
    endat.

    if it_sum-actqty <> 0.
      it_report2-vaeqty = it_report2-curqty *
                         ( it_report2-actqty / it_sum-actqty ).
      tot_qty = tot_qty +  it_report2-vaeqty.
    endif.

    move-corresponding it_report2 to it_report.

    at end of objnr.
      flag = 'Y'.
    endat.

    if flag = 'Y'.
      it_report-vaeqty = ( it_report-curqty - tot_qty )
                               + it_report-vaeqty.
    endif.

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
        clear it_report-vaeqty.
      else.
        append it_report.
        clear  it_report.
      endif.
    endif.
* }
  endloop.

endform.                    " CAL_PLAN_QTY
*&---------------------------------------------------------------------*
*&      Form  READ_FIELD_NAME_FROM_DD_COSL
*&---------------------------------------------------------------------*
*       text
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
*&      Form  POST_STD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_UCOMM  text
*----------------------------------------------------------------------*
form post_std using    p_ucomm.

  delete it_report where vaeqty eq '0'.

  data  : line  type i.
  clear : line.
  describe table it_report lines line.

  if line = 0.
    message e000(zmco) with
    ' Enter Value not equal to 0 '.
  endif.


* Init. Message TAB
  clear : it_return, it_return[].

  data : lv_conf_text(50).
* TEXT
  clear lv_conf_text.
  concatenate sy-uname  sy-datum  sy-repid
         into lv_conf_text
         separated by '/'.

* Fill Header DATA _ kb21np
  clear wa_doc_header.
  wa_doc_header-co_area           = p_kokrs.
  wa_doc_header-docdate           = it_report-bldat.
  wa_doc_header-postgdate         = it_report-bldat.
  wa_doc_header-version           = p_versn.
  wa_doc_header-variant           = 'SAP01'.
  wa_doc_header-doc_hdr_tx        = lv_conf_text.
  wa_doc_header-username          = sy-uname.

* Fill Object List
  clear : it_doc_items, it_doc_items[].

  loop at it_report.
    it_doc_items-send_cctr  = it_report-p_kostl.
    it_doc_items-acttype    = it_report-p_lstar.
    it_doc_items-rec_cctr   = it_report-r_kostl.
    it_doc_items-actvty_qty = it_report-vaeqty.
    it_doc_items-activityun = 'STD'.
    append it_doc_items.
  endloop.

* Call BAPI FM
  perform call_post_fm.                                     " KB21NP

* Commit
  if p_trun = 'X'.
    read table it_return  index 1.
    message s000(zmco) with it_return-message.
  else.
    commit work.
    read table it_return  index 1.
    message s000(zmco) with it_return-message.
*    MESSAGE S009(ZMCO) WITH P_UCOMM.

*       Consolidation management for the SKF
    call function 'Z_FCO_MANAGEMENT_SKF'
      exporting
        im_pgmno         =   sy-tcode
        im_kokrs         =   p_kokrs
        im_gjahr         =   p_gjahr
        im_perbl         =   p_perid
*           IM_PERBL_T       =
        im_versn         =   p_versn.
*           IM_GNAME          =
*           IM_PRZNR_F       =
*           IM_PRZNR_T       =
*         IMPORTING
*           SUBRC            =

  endif.


endform.                    " POST_STD
*&---------------------------------------------------------------------*
*&      Form  CALL_POST_FM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form call_post_fm.

  call function 'BAPI_ACC_ACTIVITY_ALLOC_POST'
      exporting
        doc_header            = wa_doc_header
*   IGNORE_WARNINGS       = ' '
* IMPORTING
*   DOC_NO                =
      tables
        doc_items             = it_doc_items
        return                = it_return.
*   CRITERIA              =

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
*&      Form  READ_COSR
*&---------------------------------------------------------------------*
*       Read Sender_Quantity Information from COSR
*----------------------------------------------------------------------*
form read_cosr.

  perform create_objnr.

  clear : it_cosr, it_cosr[].
  clear cosr.
  select *
         into corresponding fields of table it_cosr
         from cosr
         for all entries in it_objnr
        where lednr = '00'
          and objnr = it_objnr-objnr
          and gjahr = p_gjahr
          and wrttp = '01'                 " Plan
          and versn = p_versn
          and stagr = p_stagr.                              " 'CS301'.
  clear : it_cosr.


* Local Data definition
  field-symbols: <fs1> type any.
  data : lv_lst_nam(30).

  clear   it_cosr.
  clear : it_tmp_cosr, it_tmp_cosr[].
  loop at it_cosr.
* Key Part
    it_tmp_cosr-gjahr = p_gjahr.
    it_tmp_cosr-perid = p_perid.

    it_tmp_cosr-kostl = it_cosr-objnr+6(10).
    it_tmp_cosr-lstar = it_cosr-objnr+16(6).
    it_tmp_cosr-stagr = it_cosr-stagr.
    it_tmp_cosr-meinh = it_cosr-meinh.

* Value Transferring
    clear lv_lst_nam.
    concatenate 'IT_COSR-'  'SME'  p_perid
           into lv_lst_nam.
    assign (lv_lst_nam) to <fs1>.
    clear it_tmp_cosr-smexxx.
    it_tmp_cosr-smexxx = <fs1>.

*   Unit Conversion
*    IF IT_TMP_COSR-MEINH <> 'STD'.
*      PERFORM UNIT_CONV USING IT_TMP_COSR-MEINH
*                              IT_TMP_COSR-SMEXXX.
*    ENDIF.
*
* Collect
    collect it_tmp_cosr.

    clear it_tmp_cosr.
    clear it_cosr.
  endloop.
  clear it_tmp_cosr.


endform.                    " READ_COSR
*&---------------------------------------------------------------------*
*&      Form  CAL_ALLOCATED_QTY
*&---------------------------------------------------------------------*
*       Calculating allocation Qty
*----------------------------------------------------------------------*
form cal_allocated_qty.

  data : p_tot type p decimals 2,
         tot   type p decimals 2.
  clear : p_tot, tot.

* Preparation
  clear : it_tmp_cosr_sum, it_tmp_cosr_sum[].
  loop at it_tmp_cosr.
    move-corresponding it_tmp_cosr to it_tmp_cosr_sum.
    collect it_tmp_cosr_sum.
    clear   it_tmp_cosr_sum.
  endloop.
  clear it_tmp_cosr_sum.

  read table it_tmp_cosr_sum index 1.
  if sy-subrc = 0.
    p_tot = it_tmp_cosr_sum-smexxx.
  endif.

  read table it_catsdb_sum index 1.
  if sy-subrc = 0.
    tot = it_catsdb_sum-catshours.
    ac_qty_tot = tot.
  endif.

* allocation
  data  : line  type i.
  clear : line.
  describe table it_tmp_cosr lines line.

  data  temp(9) type p decimals 3.
  clear temp.
  loop at it_tmp_cosr.

    it_tmp_cosr-a_qty = tot * ( it_tmp_cosr-smexxx  /  p_tot ).
    temp = temp + it_tmp_cosr-a_qty.

    if sy-tabix = line.
      it_tmp_cosr-a_qty = ( tot - temp ) + it_tmp_cosr-a_qty.
    endif.

    modify it_tmp_cosr.
    clear  it_tmp_cosr.
  endloop.
  clear  it_tmp_cosr.


endform.                    " CAL_ALLOCATED_QTY
*&---------------------------------------------------------------------*
*&      Form  CREATE_OBJNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form create_objnr.

  clear  it_objnr.

  it_objnr-objnr = 'KSH2010000055001      '.
  append it_objnr.
  clear  it_objnr.
  it_objnr-objnr = 'KSH2010000055002      '.
  append it_objnr.
  clear  it_objnr.
  it_objnr-objnr = 'KSH2010000055003      '.
  append it_objnr.
  clear  it_objnr.
  it_objnr-objnr = 'KSH2010000055004      '.
  append it_objnr.
  clear  it_objnr.

* 55107
  it_objnr-objnr = 'KSH2010000055005      '.
  append it_objnr.
  clear  it_objnr.

  it_objnr-objnr = 'KSH2010000055015      '.
  append it_objnr.
  clear  it_objnr.

endform.                    " CREATE_OBJNR
*&---------------------------------------------------------------------*
*&      Form  CREATE_COSS_OBJECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form create_coss_object.

  clear it_objnr2.

  it_objnr2-objnr = 'KLH2010000055001PRS_HR'.
  append it_objnr2.
  clear  it_objnr2.
  it_objnr2-objnr = 'KLH2010000055002PRS_HR'.
  append it_objnr2.
  clear  it_objnr2.
  it_objnr2-objnr = 'KLH2010000055003PRS_HR'.
  append it_objnr2.
  clear  it_objnr2.
  it_objnr2-objnr = 'KLH2010000055004PRS_HR'.
  append it_objnr2.
  clear  it_objnr2.

* 55107

  it_objnr2-objnr = 'KLH2010000055005PRS_HR'.
  append it_objnr2.
  clear  it_objnr2.

  it_objnr2-objnr = 'KLH2010000055015PRS_HR'.
  append it_objnr2.
  clear  it_objnr2.

endform.                    " CREATE_COSS_OBJECT
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
  ranges : pnpkostl for pernr-kostl.

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
* Regular
  clear : lgart, lgart[].
  lgart-low = '0200'.
  lgart-option = 'EQ'.
  lgart-sign   = 'I'.
  append lgart.
  clear  lgart.
* Over Time
  lgart-low = '0900'.
  lgart-option = 'EQ'.
  lgart-sign   = 'I'.
  append lgart.
  clear  lgart.

  lgart-low = '0901'.
  lgart-option = 'EQ'.
  lgart-sign   = 'I'.
  append lgart.
  clear  lgart.

  lgart-low = '0902'.
  lgart-option = 'EQ'.
  lgart-sign   = 'I'.
  append lgart.
  clear  lgart.

* Cost center
  clear : pnpkostl, pnpkostl[].
*  LOOP AT IT_COSTCENTERLIST.
*    PNPKOSTL-LOW    = IT_COSTCENTERLIST-COSTCENTER.
*    PNPKOSTL-SIGN   = 'I'.
*    PNPKOSTL-OPTION = 'EQ'.
*    APPEND PNPKOSTL.
*    CLEAR  PNPKOSTL.
*  ENDLOOP.

  clear rs_date.
  read table rs_date index 1.
  pnpbegda = rs_date-low.
  pnpendda = rs_date-high.

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
  data : lv_kostl(5) value 'KOSTL', lv_anzhl(5) value 'ANZHL'.

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


  clear : it_catsdb, it_catsdb[].

  loop at <fstab> assigning <fswa>.
*    IT_TMP_CATSDB-GJAHR = P_GJAHR.
*    IT_TMP_CATSDB-PERID = P_PERID.
*    IT_CATSDB-LSTAR = P_LSTAR.
*    IT_CATSDB-UNIT  = 'STD'.
* CCTR -KOSTL
    assign lv_kostl to <fsfn>.
    assign component <fsfn> of structure <fswa> to <fsval>.
    it_catsdb-skostl = <fsval> .
* MAN_HR -ANZHL
    assign lv_anzhl to <fsfn>.
    assign component <fsfn> of structure <fswa> to <fsval>.
    move <fsval> to it_catsdb-catshours.
* Collect data
    collect it_catsdb.
    clear it_catsdb.
    clear <fswa>.
  endloop.

** Remark
** An error was found in HR program .
*  -> Rescan CCtr.

* by ig.moon 7/24/2009 {
*  loop at it_catsdb where skostl ne p_kostl.
*    delete it_catsdb.
*    clear it_catsdb.
*  endloop.
* }

  sort it_catsdb by skostl.
  loop at it_catsdb.
    if it_catsdb-rkostl = ' '.
      move-corresponding it_catsdb to it_catsdb_sum.
    else.
      move it_catsdb-rkostl    to it_catsdb_sum-skostl.
      move it_catsdb-catshours to it_catsdb_sum-catshours.
    endif.
    collect it_catsdb_sum.
    clear it_catsdb_sum.
  endloop.
  clear it_catsdb.

endform.                    " READ_FR_CATSDB2
*&---------------------------------------------------------------------*
*&      Form  read_co_mh
*&---------------------------------------------------------------------*
form read_co_mh.
  tables: ztco_mhat.
  data: i_mhat like ztco_mhat occurs 0 with header line.

  select * into table i_mhat
    from ztco_mhat
    where gjahr = p_gjahr
      and perid = p_perid
      and ( kostl = '0000055103' or kostl = '0000055107' ) "p_kostl
      and lgart between '1' and '2'.  "regular, ot

  it_catsdb-lstar = p_lstar.

  clear : it_catsdb, it_catsdb[].
  it_catsdb-lstar = p_lstar.  "MAN_HR

  loop at i_mhat.
    it_catsdb-rkostl    = i_mhat-kostl.
    it_catsdb-catshours = i_mhat-ANZHL.
    collect it_catsdb.
  endloop.

  clear: it_catsdb_sum.
  loop at it_catsdb.
    it_catsdb_sum-skostl    = it_catsdb-rkostl.
    it_catsdb_sum-catshours = it_catsdb-catshours.
    collect it_catsdb_sum.
  endloop.

endform.                    " read_co_mh
