************************************************************************
* Program Name      : ZACO56U_SKF18
* Author            : Eun Hwa , Jung
* Creation Date     : 2003.03.23
* Specifications By : Dong-Hwan Kim
* Pattern           : Report 1-1
* Development Request No : UD1K908286
* Addl Documentation:
* Description       : Transfer Plan Labor Cost HR->CCA

* the BDC structures for BATCH INPUT processing
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************


REPORT zaco56u_skf18 MESSAGE-ID zmco.

** type-pools
TYPE-POOLS: slis.

** Table
TABLES : zthr_pcp00 , csks.


** Internal Table
DATA : BEGIN OF it_zthr_pcp00 OCCURS 0.
        INCLUDE STRUCTURE zthr_pcp00.
DATA : END OF   it_zthr_pcp00.

DATA: BEGIN OF it_cal_zthr_pcp00 OCCURS 0,
        zyear TYPE zthr_pcp00-zyear,
        zmons TYPE zthr_pcp00-zmons,
        zvers TYPE zthr_pcp00-zvers,
        zcost TYPE zthr_pcp00-zcost,
        act01 TYPE zthr_pcp00-act01,
        act02 TYPE zthr_pcp00-act02,
        act03 TYPE zthr_pcp00-act03,
        act04 TYPE zthr_pcp00-act04,
        act05 TYPE zthr_pcp00-act05,
        act06 TYPE zthr_pcp00-act06,
        act07 TYPE zthr_pcp00-act07,
        act08 TYPE zthr_pcp00-act08,
        act09 TYPE zthr_pcp00-act09,
        act10 TYPE zthr_pcp00-act10,
        act11 TYPE zthr_pcp00-act11,
        act12 TYPE zthr_pcp00-act12,
        act13 TYPE zthr_pcp00-act13,
        act14 TYPE zthr_pcp00-act14,
        act15 TYPE zthr_pcp00-act15,
        act16 TYPE zthr_pcp00-act16,
        act17 TYPE zthr_pcp00-act17,
        act18 TYPE zthr_pcp00-act18,
        act19 TYPE zthr_pcp00-act19,
        act20 TYPE zthr_pcp00-act20,
        act21 TYPE zthr_pcp00-act21,
        act22 TYPE zthr_pcp00-act22,
      END OF it_cal_zthr_pcp00.

**// Mod. By Hyung Jin Youn  2004.08.02
* Data have to be collected by period
* for reporting
*DATA : BEGIN OF IT_REPORT OCCURS 0,
*        KOSTL   LIKE  CSKS-KOSTL,
*        KSTAR   LIKE  COSS-KSTAR,
*        QTY  LIKE  COSL-LST001,
*      END OF IT_REPORT.

DATA : BEGIN OF it_report OCCURS 0,
        zyear TYPE zthr_pcp00-zyear,
        zmons TYPE zthr_pcp00-zmons,
        kostl   LIKE  csks-kostl,
        kstar   LIKE  coss-kstar,
        qty  LIKE  cosl-lst001,
      END OF it_report.

* For Posting
DATA : BEGIN OF it_post OCCURS 1000,
        kostl       LIKE  csks-kostl,
*       LSTAR       LIKE  CSLA-LSTAR,
        kstar       LIKE  cosp-kstar.
*       OBJNR       LIKE  COSP-OBJNR.
        INCLUDE STRUCTURE bapiacpval. "Fix
DATA : END OF   it_post.

**// End of Mod.

** For ALV
DATA : gv_repid LIKE sy-repid.
DATA : gv_status       TYPE slis_formname VALUE 'PF_STATUS'.
DATA : gv_user_command TYPE slis_formname VALUE 'USER_COMMAND'.
DATA : it_sort         TYPE slis_t_sortinfo_alv WITH HEADER LINE .
DATA : gv_col_pos TYPE i.
DATA : it_fieldcat          TYPE slis_t_fieldcat_alv,
       wa_fieldcat          LIKE LINE OF it_fieldcat,
       it_eventcat          TYPE slis_t_event,
       wa_eventcat          LIKE LINE OF it_eventcat.
DATA : it_events	          TYPE slis_t_event,
       it_event_exit	    TYPE slis_t_event_exit.


** For BAPI
DATA : it_costcenterlist LIKE STANDARD TABLE OF bapi0012_cclist
                         WITH HEADER LINE.
DATA : it_return         LIKE STANDARD TABLE OF bapiret2
                         WITH HEADER LINE.
DATA : wa_headerinfo     LIKE bapiplnhdr.
DATA : it_indexstructure LIKE STANDARD TABLE OF bapiacpstru
                         WITH HEADER LINE.
DATA : it_coobject       LIKE STANDARD TABLE OF bapipcpobj
                         WITH HEADER LINE.
DATA : it_pervalue       LIKE STANDARD TABLE OF bapipcpval
                         WITH HEADER LINE.
DATA : it_totvalue       LIKE STANDARD TABLE OF bapipcptot
                         WITH HEADER LINE.
* DATA
DATA : gv_percount       LIKE cosp-perbl. "Period Counter
RANGES : rs_date FOR zthr_pcp00-zmons.


*----------------------------------------------------------------------*
*  Macro
*----------------------------------------------------------------------*
* Macro For Transferring value in BAPI
DEFINE trans_value.
  it_pervalue-fix_val_per&1  =  it_post-price_fix_per&1 .
END-OF-DEFINITION.

*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.
PARAMETERS :
             p_kokrs LIKE csks-kokrs MEMORY ID cac OBLIGATORY,
             p_gjahr LIKE cobk-gjahr MEMORY ID gjr OBLIGATORY,
             p_frper LIKE cosp-perbl MEMORY ID bpe OBLIGATORY,
             p_toper LIKE cosp-perbl MEMORY ID bpe OBLIGATORY,
             p_verhr LIKE cobk-versn MEMORY ID kvt OBLIGATORY,
             p_versn LIKE cobk-versn MEMORY ID kvt OBLIGATORY,
             p_currt LIKE plnhdr-plnct DEFAULT 'C' OBLIGATORY,
             p_trun(1).

SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK bl3 WITH FRAME TITLE text-003.
SELECT-OPTIONS : s_kostl  FOR csks-kostl.
PARAMETERS:      p_ncoal LIKE grpdynp-name_coall
                                     DEFAULT 'SEMIDIRECT'.
SELECTION-SCREEN END OF BLOCK bl3.
SELECTION-SCREEN END OF BLOCK bl1.


*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
INITIALIZATION.
* Set Global ALV Parameter
  gv_repid = sy-repid.


*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  PERFORM chk_input_value.

* Searching for Cost Center group
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_ncoal.
  PERFORM read_cegrp_group USING '0101'
                                 p_ncoal.


*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Read Cost Center Group - > Cost Center
  PERFORM read_cegrp.
* Read Quantity Information from ZTHR_PCP00
  PERFORM read_zthr_pcp00.
* Preparation of ALV
  PERFORM pre_report_adj.


*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.
* Call ALV LIST
  PERFORM call_alv_list.


*&---------------------------------------------------------------------*
*&      Form  READ_CEGRP_GROUP
*&---------------------------------------------------------------------*
*       Search Help for Cost element Group / CCTr Group
*----------------------------------------------------------------------*
*  -->  p_class      Class Name
*  <--  P_SET_NAME   Result Group Name
*----------------------------------------------------------------------*
FORM read_cegrp_group USING   p_class
                               p_set_name.

  CALL FUNCTION 'K_GROUP_SELECT'
    EXPORTING
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
    IMPORTING
*     CLASS_NAME               =
      set_name                 = p_set_name
*     SET_TITLE                =
*     TABLE_NAME               =
*     SETID                    =
    EXCEPTIONS
      no_set_picked            = 1
      OTHERS                   = 2.

* No error check for F4  SH
  IF sy-subrc <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    " READ_CEGRP_GROUP
*&---------------------------------------------------------------------*
*&      Form  READ_CEGRP
*&---------------------------------------------------------------------*
*       read cost center
*----------------------------------------------------------------------*
FORM read_cegrp.

* Making an internal table for CCtr to select data
* Selected Group on screen
  CLEAR : it_costcenterlist, it_costcenterlist[].
  CLEAR : it_return, it_return[].

* Set Validity Date (Start)
  DATA : lv_datum LIKE sy-datum.
  CONCATENATE p_gjahr p_frper+1(2) '01' INTO lv_datum.
*   CONCATENATE P_GJAHR P_PERID+1(2) '01' INTO LV_DATUM.

* From CCtr Group
  IF NOT p_ncoal IS INITIAL.
    CALL FUNCTION 'BAPI_COSTCENTER_GETLIST1'
         EXPORTING
              controllingarea = p_kokrs
              date_from       = lv_datum
              costcentergroup = p_ncoal
         TABLES
              costcenterlist  = it_costcenterlist
              return          = it_return.

* Message
    PERFORM dis_bapi_message.
  ENDIF.

* From CCtrs on selection screen
* From Select-options.
  DATA : it_l_cctr LIKE STANDARD TABLE OF it_costcenterlist
                 WITH HEADER LINE.

  IF NOT s_kostl[] IS INITIAL.
    LOOP AT s_kostl.
      CLEAR : it_l_cctr, it_l_cctr[].
      CALL FUNCTION 'BAPI_COSTCENTER_GETLIST1'
           EXPORTING
                controllingarea = p_kokrs
                date_from       = lv_datum
                costcenter_from = s_kostl-low
                costcenter_to   = s_kostl-high
           TABLES
                costcenterlist  = it_l_cctr
                return          = it_return.
* Message
      PERFORM dis_bapi_message.
* Appending CE list
      APPEND LINES OF it_l_cctr  TO it_costcenterlist.
      CLEAR it_costcenterlist.
      CLEAR s_kostl.
    ENDLOOP.
  ENDIF.

* Sorting
  SORT it_costcenterlist BY co_area costcenter.
  DELETE ADJACENT DUPLICATES FROM it_costcenterlist.

* Check CCtr
  IF it_costcenterlist[] IS INITIAL .
    MESSAGE e018(zmco) WITH 'Cost Center'.
  ENDIF.


ENDFORM.                    " READ_CEGRP
*&---------------------------------------------------------------------*
*&      Form  DIS_BAPI_MESSAGE
*&---------------------------------------------------------------------*
*       Display BAPI Message
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM dis_bapi_message.
  IF NOT it_return[] IS INITIAL.
    LOOP AT   it_return.
      MESSAGE ID     it_return-id
              TYPE   it_return-type
              NUMBER it_return-number
              WITH   it_return-message_v1
                     it_return-message_v2
                     it_return-message_v3
                     it_return-message_v4.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " DIS_BAPI_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  READ_ZTHR_PCP00
*&---------------------------------------------------------------------*
*       Read Quantity data from ZTHR_PCP00
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_zthr_pcp00.

*  DATA : MONTH(2) TYPE N.
*  MONTH = P_PERID.

**--- CREATE RANGES FOR PERIOD.
  rs_date-sign = 'I'.
  rs_date-option = 'BT'.
  rs_date-low = p_frper.
  rs_date-high = p_toper.
  APPEND rs_date.   CLEAR rs_date.


  CLEAR : it_zthr_pcp00, it_zthr_pcp00[].
  CLEAR zthr_pcp00.
  SELECT *
         INTO CORRESPONDING FIELDS OF TABLE it_zthr_pcp00
         FROM zthr_pcp00
         FOR ALL ENTRIES IN it_costcenterlist
        WHERE zyear = p_gjahr
          AND zmons IN rs_date    " = MONTH
*          AND ZVERS = P_VERSN
          AND zvers = p_verhr
          AND zcost = it_costcenterlist-costcenter.
  CLEAR : it_zthr_pcp00.

**// Mod. By Hyung Jin Youn  2004.08.03
* Multiply. Head Count
*  LOOP AT IT_ZTHR_PCP00.
*    MOVE-CORRESPONDING IT_ZTHR_PCP00 TO IT_CAL_ZTHR_PCP00.
*    COLLECT IT_CAL_ZTHR_PCP00.
*  ENDLOOP.

  DATA : lv_ind(2) TYPE n.
  DATA : lv_ds_act(30).
  FIELD-SYMBOLS : <fsact> TYPE ANY.

  LOOP AT it_zthr_pcp00.
    MOVE-CORRESPONDING it_zthr_pcp00 TO it_cal_zthr_pcp00.

*    IT_ZTHR_PCP00-zhedc
    lv_ind = 1.
    DO 22 TIMES.
      CONCATENATE 'IT_CAL_ZTHR_PCP00-ACT' lv_ind INTO lv_ds_act.
      ASSIGN (lv_ds_act) TO <fsact>.
      IF lv_ind = 6 OR lv_ind = 7 OR lv_ind = 10 OR lv_ind = 21.
      ELSE.
*      IF LV_IND <> 10 AND LV_IND <> 21.
        <fsact> = <fsact> *  it_zthr_pcp00-zhedc.
      ENDIF.
      lv_ind = lv_ind + 1.
    ENDDO.

    COLLECT it_cal_zthr_pcp00.
    CLEAR   it_cal_zthr_pcp00.
  ENDLOOP.

**// End of Mod.


  LOOP AT it_cal_zthr_pcp00.

*// Mod. By Hyung Jin Youn  2004.08.03
* Adding Period Fields
    PERFORM split_val_by_ce USING '601100'
                                  it_cal_zthr_pcp00-act01.
    PERFORM split_val_by_ce USING '601110'
                                  it_cal_zthr_pcp00-act02.
    PERFORM split_val_by_ce USING '601120'
                                  it_cal_zthr_pcp00-act03.
    PERFORM split_val_by_ce USING '601180'
                                  it_cal_zthr_pcp00-act04.
    PERFORM split_val_by_ce USING '601200'
                                  it_cal_zthr_pcp00-act05.
    PERFORM split_val_by_ce USING '601220'
                                  it_cal_zthr_pcp00-act06.
    PERFORM split_val_by_ce USING '601225'
                                  it_cal_zthr_pcp00-act07.
*Requested by hscho, changed by wskim,on 2004.11.04
*changed GL : 601230 -> 601240
    PERFORM split_val_by_ce USING '601240'                  "601230'
                                  it_cal_zthr_pcp00-act08.
    PERFORM split_val_by_ce USING '601300'
                                  it_cal_zthr_pcp00-act09.
    PERFORM split_val_by_ce USING '601310'
                                  it_cal_zthr_pcp00-act10.


    PERFORM split_val_by_ce USING '601320'
                                  it_cal_zthr_pcp00-act11.
    PERFORM split_val_by_ce USING '601330'
                                  it_cal_zthr_pcp00-act12.
    PERFORM split_val_by_ce USING '601340'
                                  it_cal_zthr_pcp00-act13.
    PERFORM split_val_by_ce USING '601350'
                                  it_cal_zthr_pcp00-act14.
    PERFORM split_val_by_ce USING '601360'
                                  it_cal_zthr_pcp00-act15.
    PERFORM split_val_by_ce USING '601380'
                                  it_cal_zthr_pcp00-act16.
    PERFORM split_val_by_ce USING '601390'
                                  it_cal_zthr_pcp00-act17.
    PERFORM split_val_by_ce USING '601400'
                                  it_cal_zthr_pcp00-act18.
    PERFORM split_val_by_ce USING '601410'
                                  it_cal_zthr_pcp00-act19.
    PERFORM split_val_by_ce USING '601430'
                                  it_cal_zthr_pcp00-act20.

    PERFORM split_val_by_ce USING '601500'
                                  it_cal_zthr_pcp00-act21.
    PERFORM split_val_by_ce USING '601490'
                                  it_cal_zthr_pcp00-act22.


*    IT_REPORT-KOSTL = IT_CAL_ZTHR_PCP00-ZCOST.
*    IT_REPORT-KSTAR = '601100'.
*    IT_REPORT-QTY = IT_CAL_ZTHR_PCP00-ACT01.
*    APPEND IT_REPORT.
*    CLEAR  IT_REPORT.
*
*    IT_REPORT-KOSTL = IT_CAL_ZTHR_PCP00-ZCOST.
*    IT_REPORT-KSTAR = '601110'.
*    IT_REPORT-QTY = IT_CAL_ZTHR_PCP00-ACT02.
*    APPEND IT_REPORT.
*    CLEAR  IT_REPORT.
*
*    IT_REPORT-KOSTL = IT_CAL_ZTHR_PCP00-ZCOST.
*    IT_REPORT-KSTAR = '601120'.
*    IT_REPORT-QTY = IT_CAL_ZTHR_PCP00-ACT03.
*    APPEND IT_REPORT.
*    CLEAR  IT_REPORT.
*
*    IT_REPORT-KOSTL = IT_CAL_ZTHR_PCP00-ZCOST.
*    IT_REPORT-KSTAR = '601180'.
*    IT_REPORT-QTY = IT_CAL_ZTHR_PCP00-ACT04.
*    APPEND IT_REPORT.
*    CLEAR  IT_REPORT.
*
*    IT_REPORT-KOSTL = IT_CAL_ZTHR_PCP00-ZCOST.
*    IT_REPORT-KSTAR = '601200'.
*    IT_REPORT-QTY = IT_CAL_ZTHR_PCP00-ACT05.
*    APPEND IT_REPORT.
*    CLEAR  IT_REPORT.
*
*    IT_REPORT-KOSTL = IT_CAL_ZTHR_PCP00-ZCOST.
*    IT_REPORT-KSTAR = '601220'.
*    IT_REPORT-QTY = IT_CAL_ZTHR_PCP00-ACT06.
*    APPEND IT_REPORT.
*    CLEAR  IT_REPORT.
*
*    IT_REPORT-KOSTL = IT_CAL_ZTHR_PCP00-ZCOST.
*    IT_REPORT-KSTAR = '601225'.
*    IT_REPORT-QTY = IT_CAL_ZTHR_PCP00-ACT07.
*    APPEND IT_REPORT.
*    CLEAR  IT_REPORT.
*
*    IT_REPORT-KOSTL = IT_CAL_ZTHR_PCP00-ZCOST.
*    IT_REPORT-KSTAR = '601230'.
*    IT_REPORT-QTY = IT_CAL_ZTHR_PCP00-ACT08.
*    APPEND IT_REPORT.
*    CLEAR  IT_REPORT.
*
*    IT_REPORT-KOSTL = IT_CAL_ZTHR_PCP00-ZCOST.
*    IT_REPORT-KSTAR = '601300'.
*    IT_REPORT-QTY = IT_CAL_ZTHR_PCP00-ACT09.
*    APPEND IT_REPORT.
*    CLEAR  IT_REPORT.
*
*    IT_REPORT-KOSTL = IT_CAL_ZTHR_PCP00-ZCOST.
*    IT_REPORT-KSTAR = '601310'.
*    IT_REPORT-QTY = IT_CAL_ZTHR_PCP00-ACT10.
*    APPEND IT_REPORT.
*    CLEAR  IT_REPORT.
*
*    IT_REPORT-KOSTL = IT_CAL_ZTHR_PCP00-ZCOST.
*    IT_REPORT-KSTAR = '601320'.
*    IT_REPORT-QTY = IT_CAL_ZTHR_PCP00-ACT11.
*    APPEND IT_REPORT.
*    CLEAR  IT_REPORT.
*
*    IT_REPORT-KOSTL = IT_CAL_ZTHR_PCP00-ZCOST.
*    IT_REPORT-KSTAR = '601330'.
*    IT_REPORT-QTY = IT_CAL_ZTHR_PCP00-ACT12.
*    APPEND IT_REPORT.
*    CLEAR  IT_REPORT.
*
*    IT_REPORT-KOSTL = IT_CAL_ZTHR_PCP00-ZCOST.
*    IT_REPORT-KSTAR = '601340'.
*    IT_REPORT-QTY = IT_CAL_ZTHR_PCP00-ACT13.
*    APPEND IT_REPORT.
*    CLEAR  IT_REPORT.
*
*    IT_REPORT-KOSTL = IT_CAL_ZTHR_PCP00-ZCOST.
*    IT_REPORT-KSTAR = '601350'.
*    IT_REPORT-QTY = IT_CAL_ZTHR_PCP00-ACT14.
*    APPEND IT_REPORT.
*    CLEAR  IT_REPORT.
*
*    IT_REPORT-KOSTL = IT_CAL_ZTHR_PCP00-ZCOST.
*    IT_REPORT-KSTAR = '601360'.
*    IT_REPORT-QTY = IT_CAL_ZTHR_PCP00-ACT15.
*    APPEND IT_REPORT.
*    CLEAR  IT_REPORT.
*
*    IT_REPORT-KOSTL = IT_CAL_ZTHR_PCP00-ZCOST.
*    IT_REPORT-KSTAR = '601380'.
*    IT_REPORT-QTY = IT_CAL_ZTHR_PCP00-ACT16.
*    APPEND IT_REPORT.
*    CLEAR  IT_REPORT.
*
*    IT_REPORT-KOSTL = IT_CAL_ZTHR_PCP00-ZCOST.
*    IT_REPORT-KSTAR = '601390'.
*    IT_REPORT-QTY = IT_CAL_ZTHR_PCP00-ACT17.
*    APPEND IT_REPORT.
*    CLEAR  IT_REPORT.
*
*    IT_REPORT-KOSTL = IT_CAL_ZTHR_PCP00-ZCOST.
*    IT_REPORT-KSTAR = '601400'.
*    IT_REPORT-QTY = IT_CAL_ZTHR_PCP00-ACT18.
*    APPEND IT_REPORT.
*    CLEAR  IT_REPORT.
*
*    IT_REPORT-KOSTL = IT_CAL_ZTHR_PCP00-ZCOST.
*    IT_REPORT-KSTAR = '601410'.
*    IT_REPORT-QTY = IT_CAL_ZTHR_PCP00-ACT19.
*    APPEND IT_REPORT.
*    CLEAR  IT_REPORT.
*
*    IT_REPORT-KOSTL = IT_CAL_ZTHR_PCP00-ZCOST.
*    IT_REPORT-KSTAR = '601430'.
*    IT_REPORT-QTY = IT_CAL_ZTHR_PCP00-ACT20.
*    APPEND IT_REPORT.
*    CLEAR  IT_REPORT.
*
*    IT_REPORT-KOSTL = IT_CAL_ZTHR_PCP00-ZCOST.
*    IT_REPORT-KSTAR = '601500'.
*    IT_REPORT-QTY = IT_CAL_ZTHR_PCP00-ACT21.
*    APPEND IT_REPORT.
*    CLEAR  IT_REPORT.
*
*    IT_REPORT-KOSTL = IT_CAL_ZTHR_PCP00-ZCOST.
*    IT_REPORT-KSTAR = '601490'.
*    IT_REPORT-QTY = IT_CAL_ZTHR_PCP00-ACT22.
*    APPEND IT_REPORT.
*    CLEAR  IT_REPORT.

  ENDLOOP.

* delete record with Init. Value
  DELETE it_report WHERE qty IS initial.

**// End og Mod.

ENDFORM.                    " READ_ZTHR_PCP00
*&---------------------------------------------------------------------*
*&      Form  PRE_REPORT_ADJ
*&---------------------------------------------------------------------*
*       The Preparation for ALV List
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pre_report_adj.

* Building Field Cat.
  PERFORM fieldcat_init .

* Sort
  SORT it_report BY zyear zmons kostl kstar.
  CLEAR it_report.

  it_sort-fieldname = 'ZYEAR'.
  it_sort-up        = 'X'.
  it_sort-expa      = 'X'.
  it_sort-subtot    = 'X'.
  APPEND it_sort.
  it_sort-fieldname = 'ZMONS'.
  it_sort-up        = 'X'.
  it_sort-expa      = 'X'.
  it_sort-subtot    = 'X'.
  APPEND it_sort.

  it_sort-fieldname = 'KOSTL'.
  it_sort-up        = 'X'.
  it_sort-expa      = 'X'.
  it_sort-subtot    = 'X'.
  APPEND it_sort.
*  IT_SORT-FIELDNAME = 'LSTAR'.
*  IT_SORT-UP        = 'X'.
*  IT_SORT-EXPA      = 'X'.
*  IT_SORT-SUBTOT    = 'X'.
*  APPEND IT_SORT.
*

* Set Event
  DATA : wa_l_event  TYPE slis_alv_event.
  wa_l_event-name = slis_ev_top_of_page.
  wa_l_event-form = 'BASIC_TOP_OF_PAGE'.
  APPEND wa_l_event TO it_events.


ENDFORM.                    " PRE_REPORT_ADJ
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_INIT
*&---------------------------------------------------------------------*
*       Building Field Cat.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fieldcat_init.

  CLEAR : gv_col_pos, it_fieldcat, it_fieldcat[].


* Key
  PERFORM build_fieldcat USING
    'IT_REPORT' 'ZYEAR'  'X'            space    space
    space        '4'      'Year'           space    space    space.

  PERFORM build_fieldcat USING
    'IT_REPORT' 'ZMONS'  'X'            space    space
    space        '6'      'Period'           space    space    space.

  PERFORM build_fieldcat USING
    'IT_REPORT' 'KOSTL'  'X'            space    space
    space        '11'      'Cost Center'      space    space    space.

  PERFORM build_fieldcat USING
    'IT_REPORT' 'KSTAR'  'X'            space    space
    space        '12'      'Cost element'           space    space
space.

** Value

*
  PERFORM build_fieldcat USING
    'IT_REPORT' 'QTY'  space            space    space
    space '15'      'Amount'    space    space space.

*
ENDFORM.                    " FIELDCAT_INIT
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
FORM build_fieldcat USING    value(p_0100)
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

  ADD 1 TO gv_col_pos.
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
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

ENDFORM.                    " BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  CALL_ALV_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_alv_list.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
     EXPORTING
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
     TABLES
       t_outtab                       = it_report
       EXCEPTIONS
       program_error                  = 1
       OTHERS                         = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " CALL_ALV_LIST

*-----------------------------------------------------------------------
*    FORM PF_STATUS
*-----------------------------------------------------------------------
FORM pf_status USING  extab TYPE slis_t_extab.
  SET PF-STATUS 'BALVLIST' EXCLUDING extab .
ENDFORM.

*---------------------------------------------------------------------*
*       FORM USER_COMMAND                                             *
*---------------------------------------------------------------------*
*       For User_command - AT User Command                            *
*---------------------------------------------------------------------*
*  -->  UCOMM                                                         *
*  -->  SELFIELD                                                      *
*---------------------------------------------------------------------*
FORM user_command USING ucomm    LIKE sy-ucomm
                        selfield TYPE slis_selfield.
  CASE ucomm.
* Important part !
* POST PLAN data to STD
    WHEN 'POST'.
      PERFORM post_std  USING ucomm.
  ENDCASE.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM BASIC_TOP_OF_PAGE                                        *
*---------------------------------------------------------------------*
*       Display Title                                                 *
*---------------------------------------------------------------------*
FORM basic_top_of_page.
  WRITE : / 'Controlling Area/          : '
            , p_kokrs .
  WRITE : / 'Fiscal Year/Period/Version : '
            , p_gjahr, '/', p_frper, '~', p_toper , '/', p_versn.
*  WRITE : / 'Fiscal Year/Period/Version : '
*            , P_GJAHR, '/', P_PERID , '/', P_VERSN.

* CCTR
  IF p_ncoal NE space.
    WRITE : / 'Cost Center Grp.           : ', p_ncoal.
  ENDIF.

  IF NOT s_kostl[] IS INITIAL.
    LOOP AT s_kostl.
      AT FIRST.
        WRITE : / 'Cost Center                : '.
      ENDAT.
      WRITE : / '                            ', s_kostl-low, '~',
    s_kostl-high.
    ENDLOOP.
  ENDIF.


  WRITE : / 'Test Run                     ', p_trun.
  SKIP 1.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  POST_STD
*&---------------------------------------------------------------------*
*       PREPARATION POSTING
*----------------------------------------------------------------------*
*      -->P_UCOMM  text
*----------------------------------------------------------------------*
FORM post_std USING    p_ucomm.

  DELETE it_report WHERE qty EQ space.

  DATA  : line  TYPE i.
  CLEAR : line.
  DESCRIBE TABLE it_report   LINES line.

  IF line = 0.
    MESSAGE e000(zmco) WITH
    ' No planning data has been changed '.
  ENDIF.

* Init. Message TAB
  CLEAR : it_return, it_return[].

* Fill Object List and Plan Values per Period
  CLEAR : it_indexstructure, it_indexstructure[].
  CLEAR : it_coobject,       it_coobject[].
  CLEAR : it_pervalue,       it_pervalue[].
  CLEAR : it_totvalue,       it_totvalue[].



**// Mod. By Hyung Jin Youn 2004.08.03
* Post by period

* Fill Header DATA
  CLEAR wa_headerinfo.
  wa_headerinfo-co_area        = p_kokrs.
  wa_headerinfo-fisc_year      = p_gjahr.
  wa_headerinfo-period_from    = p_frper.
  wa_headerinfo-period_to      = p_toper.
  wa_headerinfo-version        = p_versn.
* WA_HEADERINFO-DOC_HDR_TX	 =
  wa_headerinfo-plan_currtype  = p_currt.


  SORT it_report BY kostl kstar zmons .

  CLEAR : it_post, it_post[].

  LOOP AT it_report.
    MOVE-CORRESPONDING it_report TO it_post.

    PERFORM set_value_amt_bapi.
    COLLECT it_post.
    CLEAR   it_post.
    CLEAR   it_report.
  ENDLOOP.

  CLEAR   it_post.

* Sort to post data.
  SORT it_post BY kostl kstar.

  LOOP AT it_post.
* Obj
    ON CHANGE OF  it_post-kostl.
* Index of Object Key
      it_indexstructure-object_index
           = it_indexstructure-object_index + 1 .

      CLEAR it_coobject.
      it_coobject-object_index = it_indexstructure-object_index.

      it_coobject-costcenter   = it_post-kostl.
* No activity type
*     IT_COOBJECT-ACTTYPE      = IT_POST-LSTAR.
      APPEND it_coobject.
      CLEAR  it_coobject.
    ENDON.

* Value.
* Index of Value
    it_indexstructure-value_index
         = it_indexstructure-value_index + 1.

    CLEAR it_pervalue.
    it_pervalue-value_index = it_indexstructure-value_index.
    it_pervalue-cost_elem   = it_post-kstar.
* Set Value
* Post
    IF  p_ucomm = 'POST'.
      PERFORM set_value_amt_bapi_xx.
    ENDIF.

    APPEND it_pervalue.
    CLEAR  it_pervalue.

* append Index
    APPEND it_indexstructure.
    CLEAR it_post.
  ENDLOOP.



*
*  LOOP AT IT_REPORT.
*** Obj
*    ON CHANGE OF  IT_REPORT-KOSTL.
** Index of Object Key
*      IT_INDEXSTRUCTURE-OBJECT_INDEX
*           = IT_INDEXSTRUCTURE-OBJECT_INDEX + 1 .
*
*      CLEAR IT_COOBJECT.
*      IT_COOBJECT-OBJECT_INDEX = IT_INDEXSTRUCTURE-OBJECT_INDEX.
*      IT_COOBJECT-COSTCENTER   = IT_REPORT-KOSTL.
*      APPEND IT_COOBJECT.
*      CLEAR  IT_COOBJECT.
*    ENDON.
*
** Value.
*    ON CHANGE OF IT_REPORT-KSTAR.
** Index of Value
*      IT_INDEXSTRUCTURE-VALUE_INDEX
*           = IT_INDEXSTRUCTURE-VALUE_INDEX + 1.
*    ENDON.
*
**    CLEAR IT_TOTVALUE.
**    IT_TOTVALUE-VALUE_INDEX = IT_INDEXSTRUCTURE-VALUE_INDEX.
**    IT_TOTVALUE-COST_ELEM    = IT_REPORT-KSTAR.
**    IT_TOTVALUE-FIX_VALUE    = IT_REPORT-QTY.
**    APPEND IT_TOTVALUE.
**    CLEAR  IT_TOTVALUE.
*
*    CLEAR IT_PERVALUE.
*    IT_PERVALUE-VALUE_INDEX = IT_INDEXSTRUCTURE-VALUE_INDEX.
**    RESOURCE
**    TRANS_CURRENCY_ISO
**    TRANS_CURR
**    UNIT_ISO
**    UNIT_OF_MEASURE
*    IT_PERVALUE-COST_ELEM   = IT_REPORT-KSTAR.
** Set Value
** Post/Reverse
*    IF  P_UCOMM = 'POST'.
**
*    ENDIF.
*
*    APPEND IT_INDEXSTRUCTURE.
*    CLEAR IT_REPORT.
*  ENDLOOP.


**// End of Mod.

* Call BAPI FM
  PERFORM call_post_fm.

* Commit
  IF p_trun = 'X'.
  ELSE.
    COMMIT WORK.
    MESSAGE s009(zmco) WITH p_ucomm.

*       Consolidation management for the SKF
*        CALL FUNCTION 'Z_FCO_MANAGEMENT_SKF'
*          EXPORTING
*            IM_PGMNO         =   SY-TCODE
*            IM_KOKRS         =   P_KOKRS
*            IM_GJAHR         =   P_GJAHR
*            IM_PERBL         =   P_FRPER
*            IM_PERBL_T       =   P_TOPER
*            IM_VERSN         =   P_VERSN
*           IM_KOSTL_F        =    S_KOSTL-LOW
*           IM_KOSTL_T        =    S_KOSTL-HIGH
*           IM_GNAME          =    P_NCOAL.
**           IM_PRZNR_F       =
**           IM_PRZNR_T       =
**         IMPORTING
**           SUBRC            =

  ENDIF.


ENDFORM.                    " POST_STD
*&---------------------------------------------------------------------*
*&      Form  CALL_POST_FM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_post_fm.


  CALL FUNCTION 'BAPI_COSTACTPLN_POSTPRIMCOST'
    EXPORTING
      headerinfo           = wa_headerinfo
*     DELTA                = ' '
    TABLES
      indexstructure       = it_indexstructure
      coobject             = it_coobject
      pervalue             = it_pervalue
*     TOTVALUE             = IT_TOTVALUE
      return               = it_return.


*  CALL FUNCTION 'BAPI_COSTACTPLN_POSTACTINPUT'
*    EXPORTING
*      HEADERINFO           = WA_HEADERINFO
**   DELTA                = ' '
*    TABLES
**      INDEXSTRUCTURE       = IT_INDEXSTRUCTURE
*      COOBJECT             = IT_COOBJECT
**      PERVALUE              = IT_PERVALUE
*       TOTVALUE             = IT_TOTVALUE
*      RETURN               = IT_RETURN.
*



* Check error
  CLEAR  it_return.
  LOOP AT it_return  WHERE type CA 'AE'.
    MESSAGE ID     it_return-id
            TYPE   it_return-type
            NUMBER it_return-number
            WITH   it_return-message_v1
                   it_return-message_v2
                   it_return-message_v3
                   it_return-message_v4.
    CLEAR it_return.
  ENDLOOP.

ENDFORM.                    " CALL_POST_FM
*&---------------------------------------------------------------------*
*&      Form  CHK_INPUT_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM chk_input_value.

* Check Input Value (Period)
  IF p_frper > p_toper.
    MESSAGE e003(zmco) WITH p_frper p_toper.
  ENDIF.

  IF p_frper < 0 OR p_frper > 12.
    MESSAGE e007(zmco) WITH p_frper .
  ENDIF.

  IF p_toper < 0 OR p_toper > 12.
    MESSAGE e007(zmco) WITH p_toper.
  ENDIF.
*
* Check Cost Center/Cost Center Group
  IF     s_kostl[] IS INITIAL
     AND p_ncoal   IS INITIAL .
    MESSAGE e016(zmco).
  ELSEIF
         NOT s_kostl[] IS INITIAL
     AND NOT p_ncoal   IS INITIAL .
    MESSAGE e017(zmco).
  ENDIF.

** Check TEST-RUN  Flag
  IF p_trun NA 'X '.
    MESSAGE e008(zmco).
  ENDIF.

* Check Currency IND.
  IF p_currt NA 'CTO'.
    MESSAGE e000(zmco) WITH p_currt ' is not a posible value' .
  ENDIF.

ENDFORM.                    " CHK_INPUT_VALUE
*&---------------------------------------------------------------------*
*&      Form  UNIT_CONV
*&---------------------------------------------------------------------*
*       Unit Conversion
*----------------------------------------------------------------------*
*      -->P_UNIT  UNIT
*      -->P_QTY   Quantity
*----------------------------------------------------------------------*
FORM unit_conv USING    p_unit
                        p_qty.
  CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
   EXPORTING
     input                      = p_qty
*    NO_TYPE_CHECK              = 'X'
     round_sign                 = 'X'
     unit_in                    = p_unit
     unit_out                   = 'STD'
   IMPORTING
*    ADD_CONST                  =
*    DECIMALS                   =
*    DENOMINATOR                =
*    NUMERATOR                  =
     output                     = p_qty
   EXCEPTIONS
     conversion_not_found       = 1
     division_by_zero           = 2
     input_invalid              = 3
     output_invalid             = 4
     overflow                   = 5
     type_invalid               = 6
     units_missing              = 7
     unit_in_not_found          = 8.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  p_unit = 'STD'.

ENDFORM.                    " UNIT_CONV

*&---------------------------------------------------------------------*
*&      Form  SPLIT_VAL_BY_CE
*&---------------------------------------------------------------------*
*       Split Val. By CE
*----------------------------------------------------------------------*
*      -->P_0682 CE
*      -->P_qty  Qty.
*----------------------------------------------------------------------*
FORM split_val_by_ce USING    value(p_0682)
                              p_qty.

  it_report-zyear = it_cal_zthr_pcp00-zyear.
  it_report-zmons = it_cal_zthr_pcp00-zmons.

  it_report-kostl = it_cal_zthr_pcp00-zcost.
  it_report-kstar = p_0682.
  it_report-qty   = p_qty.
  APPEND it_report.
  CLEAR  it_report.

ENDFORM.                    " SPLIT_VAL_BY_CE

*&---------------------------------------------------------------------*
*&      Form  SET_VALUE_AMT_BAPI
*&---------------------------------------------------------------------*
*       For Values
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_value_amt_bapi.

  FIELD-SYMBOLS: <fs1> TYPE ANY.
  DATA : lv_fix(40).
  DATA : lv_cnt(2)  TYPE n.

* Period Counter : Set From-Period .
  CLEAR lv_cnt.
  lv_cnt = it_report-zmons.

  CLEAR lv_fix.
  CONCATENATE 'IT_POST-PRICE_FIX_PER' lv_cnt
         INTO lv_fix.
  ASSIGN (lv_fix) TO <fs1>.
  <fs1> = it_report-qty.

ENDFORM.                    " SET_VALUE_AMT_BAPI

*&---------------------------------------------------------------------*
*&      Form  SET_VALUE_AMT_BAPI_XX
*&---------------------------------------------------------------------*
*       Fill value
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_value_amt_bapi_xx.
* Fixed Cost
  trans_value 01. trans_value 02. trans_value 03. trans_value 04.
  trans_value 05. trans_value 06. trans_value 07. trans_value 08.
  trans_value 09. trans_value 10. trans_value 11. trans_value 12.
  trans_value 13. trans_value 14. trans_value 15. trans_value 16.
ENDFORM.                    " SET_VALUE_AMT_BAPI_XX
