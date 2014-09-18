************************************************************************
* Program Name      : ZACO84U_SKF21
* Author            : Eun Hwa , Jung
* Creation Date     : 2004.04.20
* Specifications By : Dong-Hwan Kim
* Pattern           : Report 1-1
* Development Request No :  UD1K909742
* Addl Documentation:
* Description       :
*                   Allocate maintenance activity qty(plan)
* the BDC structures for BATCH INPUT processing
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 03292010   Valerian     UD1K948707   Allocate CC: 55005 & 55015 to
*            HIS20094                  CC: 55107 (Engine Maintenance)
* 03302010   HIS20094     UD1K948727   Hard coding Rec.CC MXTX12 to
*                         UD1K948729   sect.CC 55004 and Send.CC 55103
* 03302010   HIS20094     UD1K948731   Remove the hard coding above
************************************************************************

REPORT  zaco84u_skf21 MESSAGE-ID zmco.

** type-pools
TYPE-POOLS: slis.

TABLES : csks, cssl, cosl, coss, cosr.

** For OBJECT KEY
DATA : BEGIN OF wa_obj ,
        objnr  LIKE  coss-objnr,
        kostl  LIKE  csks-kostl,
        lstar  LIKE  csla-lstar,
       END OF wa_obj.

DATA : it_obj_cctr_at   LIKE STANDARD TABLE OF wa_obj
                        WITH HEADER LINE .

* For DD data
DATA : gv_ci_tabname     TYPE ddobjname .
DATA : it_et_fieldlist   LIKE TABLE OF rfvicp_ddic_tabl_fieldname
                         WITH HEADER LINE.

* by ig.moon 4/21/2010 {
DATA: BEGIN OF it_coss_meg OCCURS 0,
        lstar  LIKE  cssl-lstar,
        kostl  LIKE  cssl-kostl.
        INCLUDE STRUCTURE zsco_coss_key01.
        INCLUDE STRUCTURE zsco_coss_meg01.
DATA : END OF   it_coss_meg.
* }

** Internal Table

DATA: BEGIN OF it_cosr OCCURS 0,
        objnr TYPE cosr-objnr,
        gjahr TYPE cosr-gjahr,
        wrttp TYPE cosr-wrttp,
        versn TYPE cosr-versn,
        stagr TYPE cosr-stagr,
        meinh TYPE cosr-meinh,
        sme001 TYPE cosr-sme001,
        sme002 TYPE cosr-sme002,
        sme003 TYPE cosr-sme003,
        sme004 TYPE cosr-sme004,
        sme005 TYPE cosr-sme005,
        sme006 TYPE cosr-sme006,
        sme007 TYPE cosr-sme007,
        sme008 TYPE cosr-sme008,
        sme009 TYPE cosr-sme009,
        sme010 TYPE cosr-sme010,
        sme011 TYPE cosr-sme011,
        sme012 TYPE cosr-sme012,
        sme013 TYPE cosr-sme013,
        sme014 TYPE cosr-sme014,
        sme015 TYPE cosr-sme015,
        sme016 TYPE cosr-sme016,
      END OF it_cosr.

DATA: BEGIN OF it_tmp_cosr OCCURS 0,
*        OBJNR TYPE COSR-OBJNR,
*        PRZNR TYPE CBPR-PRZNR,
        gjahr TYPE cosr-gjahr,
*        perid  LIKE coejl-perbl,
        kostl  LIKE  cssl-kostl,
         perid  LIKE coejl-perbl,
        lstar  LIKE  cssl-lstar,
        stagr TYPE cosr-stagr,
        meinh TYPE cosr-meinh,
        smexxx TYPE cosr-sme001,
        a_qty   TYPE cosr-sme001,
      END OF it_tmp_cosr.

DATA: BEGIN OF it_tmp_cosr_sum OCCURS 0,
        gjahr TYPE cosr-gjahr,
        perid  LIKE coejl-perbl,
        stagr TYPE cosr-stagr,
        smexxx TYPE cosr-sme001,
      END OF it_tmp_cosr_sum.

DATA: BEGIN OF it_coss OCCURS 0,
        kostl  LIKE  cssl-kostl,
        lstar  LIKE  cssl-lstar,
        r_kostl  LIKE  cssl-kostl,
        r_lstar  LIKE  cssl-lstar,
        parob_b  LIKE  coss-objnr.
        INCLUDE STRUCTURE zsco_coss_key01.
DATA : END OF   it_coss.

DATA : BEGIN OF it_cosl OCCURS 500.
        INCLUDE STRUCTURE zsco_cosl_key01.
        INCLUDE STRUCTURE zsco_cosl_lst01.
DATA : END OF   it_cosl.

DATA: BEGIN OF it_tmp_cosl OCCURS 0,
        gjahr  LIKE  cosl-gjahr,
        perid  LIKE  coejl-perbl,
        objnr  LIKE  coss-objnr,
        kostl  LIKE  csks-kostl,
        lstar  LIKE  csla-lstar,
        curqty LIKE  cosl-lst001,
        unit   LIKE  cosl-meinh,
      END OF it_tmp_cosl.

DATA : BEGIN OF it_cosl2 OCCURS 500.
        INCLUDE STRUCTURE zsco_cosl_key01.
        INCLUDE STRUCTURE zsco_cosl_lst01.
DATA : END OF   it_cosl2.

DATA: BEGIN OF it_tmp_cosl2 OCCURS 0,
        gjahr  LIKE  cosl-gjahr,
        perid  LIKE  coejl-perbl,
        objnr  LIKE  coss-objnr,
        kostl  LIKE  csks-kostl,
        lstar  LIKE  csla-lstar,
        curqty LIKE  cosl-lst001,
        unit   LIKE  cosl-meinh,
      END OF it_tmp_cosl2.

DATA : BEGIN OF it_sum OCCURS 0,
           gjahr  LIKE cssl-gjahr,
           perid  LIKE coejl-perbl,
*           KOSTL LIKE CSSL-KOSTL,
*           LSTAR LIKE ZTCO_MHHRTRANS-LSTAR,
           objnr  LIKE cssl-objnr,
           actqty LIKE cosl-lst001,
       END OF it_sum.

DATA : BEGIN OF it_objnr OCCURS 0,
            objnr  LIKE  cosr-objnr,
       END OF it_objnr.

DATA : BEGIN OF it_objnr2 OCCURS 0,
            objnr  LIKE  cosr-objnr,
       END OF it_objnr2.

* for reporting
DATA : BEGIN OF it_report2 OCCURS 0,
        gjahr   LIKE  cosl-gjahr,
        perid   LIKE  coejl-perbl,
        kostl   LIKE  csks-kostl,
        objnr   LIKE  cssl-objnr,
        lstar   LIKE  csla-lstar,
        qty     LIKE  cosl-lst001,
        a_qty   LIKE  cosl-lst001,
        p_kostl   LIKE  csks-kostl,
        p_lstar   LIKE  csla-lstar,
        r_kostl LIKE  cssl-kostl,
        r_objnr LIKE  cssl-objnr,
        r_lstar LIKE  cssl-lstar,
        actqty  LIKE  cosl-lst001,
        curqty  LIKE  cosl-lst001,
        vaeqty  LIKE  cosl-lst001,
        unit    LIKE  cosl-meinh,
      END OF it_report2.

* for posting
DATA : BEGIN OF it_post OCCURS 0,
        gjahr   LIKE  cosl-gjahr,
*Requested by DHKim,20041021,changed by wskim
*-----Start
*        perid   LIKE  coejl-perbl,
*-----End
        r_kostl LIKE  csks-kostl,
        r_lstar LIKE  cssl-lstar,
        p_kostl   LIKE  csks-kostl,
        p_lstar   LIKE  csla-lstar.
        INCLUDE STRUCTURE zsco_cosp_amt02.
DATA : END OF  it_post.

DATA : BEGIN OF it_report OCCURS 0,
        gjahr   LIKE  cosl-gjahr,
        perid   LIKE  coejl-perbl,
        kostl   LIKE  csks-kostl,
        objnr   LIKE  cssl-objnr,
        lstar   LIKE  csla-lstar,
        qty     LIKE  cosl-lst001,
        a_qty   LIKE  cosl-lst001,
        p_kostl   LIKE  csks-kostl,
        p_lstar   LIKE  csla-lstar,
        r_kostl LIKE  cssl-kostl,
        r_objnr LIKE  cssl-objnr,
        r_lstar LIKE  cssl-lstar,
        actqty  LIKE  cosl-lst001,
        curqty  LIKE  cosl-lst001,
        vaeqty  LIKE  cosl-lst001,
        unit    LIKE  cosl-meinh,
      END OF it_report.
*Requested by DHKim,20041207,changed by wskim
*-----Start
DATA :  p_vaeqty LIKE it_report-vaeqty.
*-----End

** For BAPI
DATA : it_costcenterlist LIKE STANDARD TABLE OF bapi0012_cclist
                         WITH HEADER LINE.
DATA : it_return         LIKE STANDARD TABLE OF bapiret2
                         WITH HEADER LINE.

DATA : wa_headerinfo     LIKE bapiplnhdr.
DATA : it_indexstructure LIKE STANDARD TABLE OF bapiacistru
                         WITH HEADER LINE.
DATA : it_coobject       LIKE STANDARD TABLE OF bapiaciobj
                         WITH HEADER LINE.
DATA : it_pervalue       LIKE STANDARD TABLE OF bapiacival
                         WITH HEADER LINE.
DATA : it_totvalue       LIKE STANDARD TABLE OF bapiacitot
                         WITH HEADER LINE.


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

* Global Variant
DATA : gv_post_date LIKE coheader-budat.
DATA : ac_qty_tot(13) TYPE p DECIMALS 3.
DATA : gv_percount       LIKE cosp-perbl. "Period Counter

DATA : BEGIN OF it_sender OCCURS 0,                         "HIS20094
         kostl TYPE csks-kostl,                             "HIS20094
       END OF it_sender.                                    "HIS20094

RANGES : rs_date FOR catsdb-workdate.

* Macro For Transferring value in BAPI
DEFINE trans_value.
*  IT_PERVALUE-QUANTITY_FIX_PER&1  =  IT_POST-VAR0&1.
  it_pervalue-quantity_var_per&1  =  it_post-var0&1.
END-OF-DEFINITION.

*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.
PARAMETERS :
               p_kokrs LIKE csks-kokrs MEMORY ID cac OBLIGATORY,
               p_gjahr LIKE cobk-gjahr MEMORY ID gjr OBLIGATORY,
               p_perid LIKE cosp-perbl MEMORY ID bpe OBLIGATORY,
               p_toper LIKE cosp-perbl MEMORY ID bpe OBLIGATORY,
               p_versn LIKE cobk-versn MEMORY ID kvt OBLIGATORY,
               p_kostl LIKE csks-kostl DEFAULT '0000055103' NO-DISPLAY,
               p_stagr LIKE cosr-stagr DEFAULT 'CS301' NO-DISPLAY,
               p_currt LIKE plnhdr-plnct DEFAULT 'C' OBLIGATORY,
               p_trun(1).

SELECTION-SCREEN SKIP 1.
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

*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
*Issue number
*Requested by dhkim,20041020,changed by wskim
*---Start
* Calculating Period Count
  PERFORM cal_per_count.
*---End

  PERFORM populate_sender.                                  "HIS20094

  LOOP AT it_sender.                                        "HIS20094
* OBJ Key Combination
    PERFORM set_obj_key.
* Read Dynamic Fields Name
    PERFORM read_field_name_from_dd_cosl.
* Read Sender_Quantity Information from COSL
    PERFORM read_cosl_qty.
* Read table 'COSR' SKF information
    PERFORM read_cosr.
* Calculating Qty
    PERFORM cal_allocated_qty.
* Read Receiver.
    PERFORM read_coss_parob.
* Read Receiver_Quantity Information from COSL
    PERFORM read_cosl_qty_2.
* Calculating Qty and preparation of reporting data.
    PERFORM cal_plan_qty.
  ENDLOOP.                                                  "HIS20094

* Preparation of ALV
  PERFORM pre_report_adj.
* Preparation of posting data.
  PERFORM posting_data.
*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.
* Call ALV LIST
  PERFORM call_alv_list.
*&---------------------------------------------------------------------*
*&      Form  DIS_BAPI_MESSAGE
*&---------------------------------------------------------------------*
*        Display BAPI Message
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
*&      Form  READ_COSL_QTY
*&---------------------------------------------------------------------*
*       Read Sender_Quantity Information from COSL
*----------------------------------------------------------------------*

FORM read_cosl_qty.

  CLEAR : it_cosl, it_cosl[].
  CLEAR cosl.
  SELECT (it_et_fieldlist)
         INTO CORRESPONDING FIELDS OF TABLE it_cosl
         FROM cosl
          FOR ALL ENTRIES IN it_obj_cctr_at
          WHERE lednr = '00'
          AND objnr = it_obj_cctr_at-objnr
          AND gjahr = p_gjahr
          AND wrttp = '01'             " plan
          AND versn = p_versn.
  CLEAR : it_cosl.


* Local Data definition
  FIELD-SYMBOLS: <fs> TYPE ANY.
  DATA : lv_lst_nam(30).
  DATA : lv_cnt  LIKE  cosp-perbl.
*
  CLEAR   it_cosl.
  CLEAR : it_tmp_cosl, it_tmp_cosl[].

  CLEAR : it_tmp_cosl, it_tmp_cosl[].
  LOOP AT it_cosl.
    it_tmp_cosl-gjahr = p_gjahr.
*Requested by dhkim,20041020,changed by wskim
*-----Start
* Period Counter : Set From-Period .
    CLEAR lv_cnt.
*    it_tmp_cosl-perid = p_perid.
    lv_cnt = p_perid .
*-----End

    CLEAR it_obj_cctr_at.
    READ TABLE    it_obj_cctr_at
         WITH KEY objnr = it_cosl-objnr.
    it_tmp_cosl-kostl = it_obj_cctr_at-kostl.
    it_tmp_cosl-lstar = it_obj_cctr_at-lstar.
    it_tmp_cosl-objnr = it_obj_cctr_at-objnr.
*    it_tmp_cosl-unit = it_cosl-meinh.
*Requested by dhkim,20041020,changed by wskim
*-----Start
    DO gv_percount TIMES.
* Period
      CLEAR it_tmp_cosl-perid.
      it_tmp_cosl-unit = it_cosl-meinh.
      it_tmp_cosl-perid = lv_cnt.
*-----End
* Value Transferring
      CLEAR lv_lst_nam.
*Requested by dhkim,20041020,changed by wskim
*-----Start
*    CONCATENATE 'IT_COSL-'  'LST'  p_perid
*          INTO lv_lst_nam.
      CONCATENATE 'IT_COSL-'  'LST'  lv_cnt
            INTO lv_lst_nam.
*-----End
      ASSIGN (lv_lst_nam) TO <fs>.
      CLEAR it_tmp_cosl-curqty.
      it_tmp_cosl-curqty = <fs>.

*   Unit Conversion
      IF it_tmp_cosl-unit <> 'STD'.
        PERFORM unit_conv USING it_tmp_cosl-unit
                                it_tmp_cosl-curqty.
      ENDIF.

* Collect
      COLLECT it_tmp_cosl.
*Requested by dhkim,20041020,changed by wskim
*-----Start
* Period Counter
      lv_cnt = lv_cnt + 1.
    ENDDO.
*-----End
    CLEAR   it_tmp_cosl.
    CLEAR   it_cosl.
  ENDLOOP.
  CLEAR it_tmp_cosl.
  CLEAR it_cosl.

ENDFORM.                    " READ_COSL_QTY
*-----------------------------------------------------------------------
*    FORM PF_STATUS
*-----------------------------------------------------------------------
FORM pf_status USING  extab TYPE slis_t_extab.
  SET PF-STATUS 'LIST' EXCLUDING extab .
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
    WHEN 'POST'.
      PERFORM post_std  USING ucomm.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PRE_REPORT_ADJ
*&---------------------------------------------------------------------*
*       The Preparation for ALV List
*----------------------------------------------------------------------*
FORM pre_report_adj.

* Building Field Cat.
  PERFORM fieldcat_init .

* Sort
  SORT  it_report BY gjahr perid kostl lstar r_kostl r_lstar.
  CLEAR it_report.
  it_sort-fieldname = 'GJAHR'.
  it_sort-up        = 'X'.
  it_sort-expa      = 'X'.
  it_sort-subtot    = 'X'.
  APPEND it_sort.
  it_sort-fieldname = 'PERID'.
  it_sort-up        = 'X'.
  it_sort-expa      = 'X'.
  it_sort-subtot    = 'X'.
  APPEND it_sort.
  it_sort-fieldname = 'KOSTL'.
  it_sort-up        = 'X'.
  it_sort-expa      = 'X'.
  it_sort-subtot    = 'X'.
  APPEND it_sort.
  it_sort-fieldname = 'LSTAR'.
  it_sort-up        = 'X'.
  it_sort-expa      = 'X'.
  it_sort-subtot    = 'X'.
  APPEND it_sort.


* Set Event
  DATA : wa_l_event  TYPE slis_alv_event.
  wa_l_event-name = slis_ev_top_of_page.
  wa_l_event-form = 'BASIC_TOP_OF_PAGE'.
  APPEND wa_l_event TO it_events.


ENDFORM.                    " PRE_REPORT_ADJ
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_INIT
*&---------------------------------------------------------------------*
*       Building Field Cat
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fieldcat_init.

  CLEAR : gv_col_pos, it_fieldcat, it_fieldcat[].

* Key
  PERFORM build_fieldcat USING
    'IT_REPORT' 'GJAHR'  'X'            space    space
    space        '4'      'Year'           space    space    space.

  PERFORM build_fieldcat USING
    'IT_REPORT' 'PERID'  'X'            space    space
    space        '3'      'Period'           space    space    space.

  PERFORM build_fieldcat USING
   'IT_REPORT' 'KOSTL'  'X'            space    space
   space        '8'      'Section'      space    space    space.

*  PERFORM BUILD_FIELDCAT USING
*   'IT_REPORT' 'LSTAR'  'X'            SPACE    SPACE
*   SPACE        '6'      'Sen.AT'           SPACE    SPACE    SPACE.

  PERFORM build_fieldcat USING
    'IT_REPORT' 'P_KOSTL'  'X'            space    space
    space        '10'      'Sender CC'      space    space    space.

  PERFORM build_fieldcat USING
    'IT_REPORT' 'P_LSTAR'  'X'            space    space
    space        '7'      'Sen.AT'      space    space    space.

  PERFORM build_fieldcat USING
    'IT_REPORT' 'R_KOSTL' 'X'            space    space
    space        '10'      'Rec. C/C'       space    space    space.

  PERFORM build_fieldcat USING
    'IT_REPORT' 'R_LSTAR'  'X'            space    space
    space        '6'      'Rec.AT'           space    space    space.

** Value

*  PERFORM BUILD_FIELDCAT USING
*    'IT_REPORT' 'UNIT'  SPACE            SPACE    SPACE
*    SPACE   '4'      'Unit'      'UNIT'   SPACE     SPACE.

  PERFORM build_fieldcat USING
    'IT_REPORT' 'QTY'  space             space    space
    'IT_REPORT'   '15'      'Section.Qty'       space    space    space.

  PERFORM build_fieldcat USING
    'IT_REPORT' 'A_QTY'  space            space    space
    'IT_REPORT'   '15'      'Alloc.Qty'         space    space    space.

  PERFORM build_fieldcat USING
    'IT_REPORT' 'ACTQTY'  space            'X'     space
    space   '15'      'Rec.Qty'  space    space space.

*  PERFORM BUILD_FIELDCAT USING
*    'IT_REPORT' 'CURQTY'  SPACE            SPACE    SPACE
*    SPACE '15'      'Sen.Plan Qty'    SPACE    SPACE SPACE.

  PERFORM build_fieldcat USING
    'IT_REPORT' 'VAEQTY'  space            'X'    space
    'IT_REPORT'   '15'      'Rec.Input Qty'      space   space  space .

*
* Set Event
  DATA : wa_l_event  TYPE slis_alv_event.
  wa_l_event-name = slis_ev_top_of_page.
  wa_l_event-form = 'BASIC_TOP_OF_PAGE'.
  APPEND wa_l_event TO it_events.


ENDFORM.                    " FIELDCAT_INIT
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
FORM build_fieldcat USING   value(p_0100)
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
*       CALL ALV LIST
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

*---------------------------------------------------------------------*
*       FORM BASIC_TOP_OF_PAGE                                        *
*---------------------------------------------------------------------*
*       Display Title                                                 *
*---------------------------------------------------------------------*
FORM basic_top_of_page.

  WRITE : / 'Controlling Area      : ', p_kokrs .
  WRITE : / 'Fiscal Year/Period    : '
            , p_gjahr, '/', p_perid,'~', p_toper.
  WRITE : / 'Version               : ', p_versn .
  WRITE : / 'Test Run                     ', p_trun.

* Begin of HIS20094
*  write : / 'C/C:' , p_kostl, 'AT: MNT_HR' , '    Rec Input Qty:'
**Requested by DHKim,20041021,changed by wskim
**-----Start
**  ,ac_qty_tot.
*  ,p_vaeqty.
**-----End

  WRITE : / 'C/C:' .

  LOOP AT it_sender.
    WRITE it_sender-kostl.
  ENDLOOP.

  WRITE : 'AT: MNT_HR' ,
           '    Rec Input Qty:',
           p_vaeqty.
* End of HIS20094

  SKIP 1.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHK_INPUT_VALUE
*&---------------------------------------------------------------------*
*       CHK_INPUT_VALUE
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM chk_input_value.

  IF p_perid < 0 OR p_perid > 12.
    MESSAGE e007(zmco) WITH p_perid .
  ENDIF.


ENDFORM.                    " CHK_INPUT_VALUE
*&---------------------------------------------------------------------*
*&      Form  SET_OBJ_KEY
*&---------------------------------------------------------------------*
* OBJ Key Combination
*----------------------------------------------------------------------*
FORM set_obj_key.

  CLEAR : it_obj_cctr_at, it_obj_cctr_at[].

* Begin of HIS20094
*  call function 'K_LSTAR_OBJECT_KEY_GET'
*       exporting
*            kokrs = p_kokrs
*            kostl = '0000055103'  "P_KOSTL
*            lstar = 'MNT_HR'
*       importing
*            objnr = it_obj_cctr_at-objnr.

  CALL FUNCTION 'K_LSTAR_OBJECT_KEY_GET'
       EXPORTING
            kokrs = p_kokrs
            kostl = it_sender-kostl  "P_KOSTL
            lstar = 'MNT_HR'
       IMPORTING
            objnr = it_obj_cctr_at-objnr.
* End of HIS20094

  it_obj_cctr_at-kostl = it_obj_cctr_at-objnr+6(10).
  it_obj_cctr_at-lstar = it_obj_cctr_at-objnr+16(6).
  APPEND it_obj_cctr_at. CLEAR it_obj_cctr_at.

  CLEAR : it_obj_cctr_at.

ENDFORM.                    " SET_OBJ_KEY
*&---------------------------------------------------------------------*
*&      Form  READ_COSL_QTY_2
*&---------------------------------------------------------------------*
*       Read Receiver_Quantity Information from COSL
*----------------------------------------------------------------------*
FORM read_cosl_qty_2.

  CLEAR : it_cosl2, it_cosl2[].
  CLEAR cosl.
  SELECT (it_et_fieldlist)
       INTO CORRESPONDING FIELDS OF TABLE it_cosl2
         FROM cosl
          FOR ALL ENTRIES IN it_coss
        WHERE lednr = '00'
          AND objnr = it_coss-parob_b
          AND gjahr = p_gjahr
          AND wrttp = '01'             " Plan
          AND versn = p_versn.
  CLEAR : it_cosl2.

  DELETE it_cosl2 WHERE objnr+16(6) NE 'MCH_HR'.


* Local Data definition
  FIELD-SYMBOLS: <fs> TYPE ANY.
  DATA : lv_lst_nam(30).
  DATA : lv_cnt  LIKE  cosp-perbl.
*
  CLEAR   it_cosl2.
  CLEAR : it_tmp_cosl2, it_tmp_cosl2[].

  CLEAR : it_tmp_cosl2, it_tmp_cosl2[].
  LOOP AT it_cosl2.
    it_tmp_cosl2-gjahr = p_gjahr.
*Requested by dhkim,20041020,changed by wskim
*-----Start
* Period Counter : Set From-Period .
    CLEAR lv_cnt.
*    it_tmp_cosl2-perid = p_perid.
    lv_cnt = p_perid .
*-----End
    it_tmp_cosl2-kostl = it_cosl2-objnr+6(6).
    it_tmp_cosl2-lstar = it_cosl2-objnr+16(6).
    it_tmp_cosl2-objnr = it_cosl2-objnr.
*    it_tmp_cosl2-unit  = it_cosl2-meinh.
*Requested by dhkim,20041020,changed by wskim
*-----Start
    DO gv_percount TIMES.
* Period
      CLEAR it_tmp_cosl2-perid.
      it_tmp_cosl2-unit  = it_cosl2-meinh.
      it_tmp_cosl2-perid = lv_cnt.
*-----End
* Value Transferring
      CLEAR lv_lst_nam.
*Requested by dhkim,20041020,changed by wskim
*-----Start
*    CONCATENATE 'IT_COSL2-'  'LST'  p_perid
*          INTO lv_lst_nam.
      CONCATENATE 'IT_COSL2-'  'LST'  lv_cnt
            INTO lv_lst_nam.
*-----End
      ASSIGN (lv_lst_nam) TO <fs>.
      CLEAR it_tmp_cosl2-curqty.
      it_tmp_cosl2-curqty = <fs>.

*   Unit Conversion
      IF it_tmp_cosl2-unit <> 'STD'.
        PERFORM unit_conv USING it_tmp_cosl2-unit
                                it_tmp_cosl2-curqty.
      ENDIF.
* Collect
      COLLECT it_tmp_cosl2.
* Period Counter
      lv_cnt = lv_cnt + 1.
    ENDDO.
    CLEAR   it_tmp_cosl2.
    CLEAR   it_cosl2.
  ENDLOOP.
  CLEAR it_tmp_cosl2.
  CLEAR it_cosl2.




ENDFORM.                    " READ_COSL_QTY_2
*&---------------------------------------------------------------------*
*&      Form  READ_COSS_PAROB
*&---------------------------------------------------------------------*
*      Read Receiver.
*----------------------------------------------------------------------*
FORM read_coss_parob.

  PERFORM create_coss_objnr.

* by ig.moon 4/21/2010 {
  CLEAR : it_coss_meg, it_coss_meg[].
  SELECT *
         INTO CORRESPONDING FIELDS OF TABLE it_coss_meg
         FROM coss
          FOR ALL ENTRIES IN it_objnr2
        WHERE lednr = '00'
          AND objnr = it_objnr2-objnr      "IT_COSR-OBJNR
          AND gjahr = p_gjahr
          AND wrttp = '01'         " plan
          AND versn = p_versn
          AND vrgng = 'RKP7'.

  DELETE it_coss_meg WHERE parob = ' '.

  LOOP AT it_coss_meg.
    it_coss_meg-kostl = it_coss_meg-parob+6(10).
    it_coss_meg-lstar = it_coss_meg-parob+16(6).
    MODIFY it_coss_meg.
    CLEAR it_coss_meg.
  ENDLOOP.

  DELETE it_coss_meg WHERE lstar NE 'MAN_HR'.
* }


* search for Partner object
  CLEAR : it_coss, it_coss[].
  CLEAR it_cosr.
  CLEAR coss.
  SELECT *
         INTO CORRESPONDING FIELDS OF TABLE it_coss
         FROM coss
          FOR ALL ENTRIES IN it_objnr2
        WHERE lednr = '00'
          AND objnr = it_objnr2-objnr
          AND gjahr = p_gjahr
          AND wrttp = '01'        " plan
* }
*          and meg001 <> 0
* }
          AND versn = p_versn.
*Issue number
*Requested by dhkim,20041021,changed by wskim
*---Start
*          and meg009 ne 0.
*---End
  CLEAR it_coss.

  DELETE it_coss WHERE parob = ' '.
  DELETE it_coss WHERE parob+16(6) NE 'MAN_HR'.


  LOOP AT it_coss.
    CONCATENATE it_coss-parob(12) '    MCH_HR' INTO it_coss-parob_b.
    it_coss-kostl   = it_coss-objnr+6(10).
    it_coss-lstar   = it_coss-objnr+16(6).
    it_coss-r_kostl = it_coss-parob+6(10).
    it_coss-r_lstar = it_coss-parob+16(6).
    MODIFY it_coss.
    CLEAR  it_coss.
  ENDLOOP.


ENDFORM.                    " READ_COSS_PAROB

*&---------------------------------------------------------------------*
*&      Form  CAL_PLAN_QTY
*&---------------------------------------------------------------------*
*       Calculating Qty and preparation of reporting data.
*----------------------------------------------------------------------*
FORM cal_plan_qty.

  SORT it_coss BY kostl.
*Making an it_report for reporting
  CLEAR : it_report2, it_report2[].
  CLEAR : it_tmp_cosr, it_coss.
*Issue number
*Requested by dhkim,20041021,changed by wskim
*---Start
  DATA :  r_perid LIKE p_perid.
  r_perid = p_perid .
  SORT it_tmp_cosr BY perid  kostl.
  DO  .
    IF r_perid  >  p_toper.
      EXIT.
    ENDIF.
*---End

    LOOP AT it_coss.

      LOOP AT it_tmp_cosr WHERE kostl = it_coss-kostl
*---Start
                               AND perid = r_perid.
*---End
*                         AND LSTAR = IT_COSS-LSTAR.

        it_report2-gjahr   = p_gjahr.
*Issue number
*Requested by dhkim,20041021,changed by wskim
*---Start
*      it_report2-perid   = p_perid.
        it_report2-perid   = it_tmp_cosr-perid.
*---End
        it_report2-kostl   = it_tmp_cosr-kostl.
        it_report2-objnr   = it_coss-objnr.
        it_report2-lstar   = it_coss-lstar.
        it_report2-qty     = it_tmp_cosr-smexxx.
        it_report2-a_qty     = it_tmp_cosr-a_qty.

        p_kostl = it_sender-kostl.                          "HIS20094

        it_report2-p_kostl   = p_kostl.                     " '55103'.
        it_report2-p_lstar   = 'MNT_HR'.

        it_report2-r_kostl = it_coss-r_kostl.
        it_report2-r_objnr = it_coss-parob_b.
        it_report2-r_lstar = 'MAN_HR'.     "IT_COSS-PAROB_B+16(6).
        it_report2-curqty  = it_tmp_cosr-a_qty.
        APPEND it_report2.
        CLEAR  it_report2.
      ENDLOOP.

    ENDLOOP.

*---Start
    r_perid =  r_perid  + 1.
  ENDDO.

*---End

  CLEAR it_report2.
  LOOP AT it_report2.
    CLEAR it_tmp_cosl2.
    READ TABLE    it_tmp_cosl2
       WITH KEY gjahr = it_report2-gjahr
                perid = it_report2-perid
                objnr = it_report2-r_objnr.

    it_report2-actqty = it_tmp_cosl2-curqty.
    MODIFY it_report2.
    CLEAR  it_report2.
  ENDLOOP.

  SORT it_report2 BY perid kostl.
  CLEAR : it_sum, it_sum[].
  LOOP AT it_report2.
    MOVE-CORRESPONDING it_report2 TO it_sum.
    COLLECT it_sum.
    CLEAR   it_sum.
  ENDLOOP.
  CLEAR it_sum.


* Calculate PLAN QTY.
  DATA   tot_qty(9) TYPE p DECIMALS 3.
  DATA   flag(1).
  CLEAR  tot_qty.

* by ig.moon 3/30/2010 {
  FIELD-SYMBOLS: <fs3> TYPE ANY.
  DATA : lv_meg_nam2(30).
  SORT it_coss_meg BY gjahr kostl objnr.
* }

*  clear : it_report, it_report[].                         "HIS20094
  CLEAR it_report2.
  SORT it_report2 BY perid objnr.
  LOOP AT it_report2.

    AT NEW objnr.
      CLEAR : tot_qty, flag, it_sum.
      READ TABLE  it_sum
           WITH KEY gjahr = it_report2-gjahr
                    perid = it_report2-perid
                    objnr = it_report2-objnr.
    ENDAT.

    IF it_sum-actqty <> 0.
      it_report2-vaeqty = it_report2-curqty *
                         ( it_report2-actqty / it_sum-actqty ).
      tot_qty = tot_qty +  it_report2-vaeqty.
    ENDIF.

    MOVE-CORRESPONDING it_report2 TO it_report.

    AT END OF objnr.
      flag = 'Y'.
    ENDAT.

    IF flag = 'Y'.
      it_report-vaeqty = ( it_report-curqty - tot_qty )
                               + it_report-vaeqty.
    ENDIF.


* by ig.moon 3/30/2010 {
    READ TABLE it_coss_meg WITH KEY gjahr = it_report-gjahr
                                    kostl = it_report-r_kostl
                                    objnr = it_report-objnr
                                    BINARY SEARCH.
    IF sy-subrc EQ 0.
      CLEAR lv_meg_nam2.
      CONCATENATE 'IT_COSS_MEG'  '-MEG'  it_report-perid
             INTO lv_meg_nam2.
      ASSIGN (lv_meg_nam2) TO <fs3>.

      IF <fs3> EQ '0.000'.
        CLEAR it_report-vaeqty.
      ELSE.
        APPEND it_report.
        CLEAR  it_report.
      ENDIF.
    ENDIF.
* }


*    append it_report.
*    clear  it_report.
  ENDLOOP.



  CLEAR it_report.
*Issue number
*Requested by dhkim,20041021,changed by wskim
*---Start
  LOOP AT it_report.
    AT LAST.
      SUM.
      MOVE it_report-vaeqty TO p_vaeqty.
    ENDAT.
  ENDLOOP.
*---End

ENDFORM.                    " CAL_PLAN_QTY
*&---------------------------------------------------------------------*
*&      Form  READ_FIELD_NAME_FROM_DD_COSL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_field_name_from_dd_cosl.

  CLEAR : it_et_fieldlist, it_et_fieldlist[].

* read DD infor. COSL Key Part
  PERFORM read_dd_info  TABLES it_et_fieldlist
                        USING  'ZSCO_COSL_KEY01'.

* read DD infor. COSL Value Part (Total Quantity)
  PERFORM read_dd_info  TABLES it_et_fieldlist
                        USING  'ZSCO_COSL_LST01'.

ENDFORM.                    " READ_FIELD_NAME_FROM_DD_COSL
*&---------------------------------------------------------------------*
*&      Form  READ_DD_INFO
*&---------------------------------------------------------------------*
*        Read DD information
*----------------------------------------------------------------------*
*     -->IT_l_ET_FIELDLIST  Field-List Table
*      -->P_CI_TABNAME       DD name
*----------------------------------------------------------------------*
FORM read_dd_info TABLES   it_l_et_fieldlist STRUCTURE it_et_fieldlist
                  USING    p_ci_tabname      LIKE gv_ci_tabname.
* Local DATA definition
  DATA : it_l_fdlist LIKE STANDARD TABLE OF it_et_fieldlist
                     WITH HEADER LINE.
* Making FDlist
  CLEAR : it_l_fdlist,     it_l_fdlist[].
  CLEAR gv_ci_tabname.
  gv_ci_tabname = p_ci_tabname.
  CALL FUNCTION 'RECP_DD_TABL_FIELDNAMES_GET'
       EXPORTING
            ic_tabname   = gv_ci_tabname
       TABLES
            et_fieldlist = it_l_fdlist
       EXCEPTIONS
            not_found    = 1
            OTHERS       = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  APPEND LINES OF  it_l_fdlist       TO it_l_et_fieldlist.

ENDFORM.                    " READ_DD_INFO
*&---------------------------------------------------------------------*
*&      Form  POST_STD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_UCOMM  text
*----------------------------------------------------------------------*
FORM post_std USING    p_ucomm.

  DELETE it_report WHERE vaeqty EQ '0'.

  DATA  : line  TYPE i.
  CLEAR : line.
  DESCRIBE TABLE it_report LINES line.

  IF line = 0.
    MESSAGE e000(zmco) WITH
    ' Enter Value not equal to 0 '.
  ENDIF.

* Init. Message TAB
  CLEAR : it_return, it_return[].

* Fill Header DATA
  CLEAR wa_headerinfo.
  wa_headerinfo-co_area        = p_kokrs.
  wa_headerinfo-fisc_year      = p_gjahr.
  wa_headerinfo-period_from	 = p_perid.
*  WA_HEADERINFO-PERIOD_TO	 = P_PERID.
  wa_headerinfo-period_to	 = p_toper.

  wa_headerinfo-version        = p_versn.
* WA_HEADERINFO-DOC_HDR_TX	 =
  wa_headerinfo-plan_currtype	 = p_currt.

* Fill Object List and Plan Values per Period
  CLEAR : it_indexstructure, it_indexstructure[].
  CLEAR : it_coobject,       it_coobject[].
  CLEAR : it_pervalue,       it_pervalue[].
  CLEAR : it_totvalue,       it_totvalue[].

*Requested by Hur,20041020,changed by wskim
*-----Start* Sort to post data.
  SORT it_post BY r_kostl r_lstar.
*-----End
  LOOP AT it_post.
*Requested by DHKim,20041021,changed by wskim
*-----Start
** Index of Object Key
*    it_indexstructure-object_index
*         = it_indexstructure-object_index + 1 .
*
*    CLEAR it_coobject.
*    it_coobject-object_index = it_indexstructure-object_index.
*
*    it_coobject-costcenter   = it_post-r_kostl.
*    it_coobject-acttype      = it_post-r_lstar.
*    APPEND it_coobject.
*    CLEAR  it_coobject.
**    ENDON.
** Obj
************************
    ON CHANGE OF  it_post-r_kostl
              OR  it_post-r_lstar .
* Index of Object Key
      it_indexstructure-object_index
           = it_indexstructure-object_index + 1 .

      CLEAR it_coobject.
      it_coobject-object_index = it_indexstructure-object_index.

      it_coobject-costcenter   = it_post-r_kostl.
      it_coobject-acttype      = it_post-r_lstar.
      APPEND it_coobject.
      CLEAR  it_coobject.
    ENDON.
*-----End


* Value.
* Index of Value
    it_indexstructure-value_index
         = it_indexstructure-value_index + 1.

    CLEAR it_pervalue.
    it_pervalue-value_index = it_indexstructure-value_index.

    it_pervalue-send_cctr     = it_post-p_kostl.
    it_pervalue-send_activity = it_post-p_lstar.

** Set Value
** Post/Reverse

*Requested by Hur,20041020,changed by wskim
*-----Start
    IF  p_ucomm = 'POST'.
*-----End
      PERFORM set_value_amt.
*-----Start
    ENDIF.
*-----End

    APPEND it_pervalue.
    CLEAR  it_pervalue.

* append Index
    APPEND it_indexstructure.
    CLEAR it_post.
  ENDLOOP.

* Call BAPI FM
  PERFORM call_post_fm.

* Commit
  IF p_trun = 'X'.
  ELSE.
    COMMIT WORK.
    MESSAGE s009(zmco) WITH p_ucomm.

*       Consolidation management for the SKF
    CALL FUNCTION 'Z_FCO_MANAGEMENT_SKF'
         EXPORTING
              im_pgmno   = sy-tcode
              im_kokrs   = p_kokrs
              im_gjahr   = p_gjahr
              im_perbl   = p_perid
*              im_perbl_t = p_perid
              im_perbl_t = p_toper

              im_versn   = p_versn.
*           IM_KOSTL_F        =    P_KOSTL
*           IM_KOSTL_T        =    P_KOSTL.
*           IM_GNAME          =    P_NCOAL.
*           IM_PRZNR_F       =
*           IM_PRZNR_T       =
*         IMPORTING
*           SUBRC            =

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

  CALL FUNCTION 'BAPI_COSTACTPLN_POSTACTINPUT'
     EXPORTING
       headerinfo           = wa_headerinfo
*   DELTA                = ' '
     TABLES
       indexstructure       = it_indexstructure
       coobject             = it_coobject
       pervalue              = it_pervalue
*   TOTVALUE             =
       return               = it_return.

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
*&      Form  READ_COSR
*&---------------------------------------------------------------------*
*      Read table 'COSR' SKF information
*----------------------------------------------------------------------*
FORM read_cosr.

  PERFORM create_objnr.

  CLEAR : it_cosr, it_cosr[].
  CLEAR cosr.
  SELECT *
         INTO CORRESPONDING FIELDS OF TABLE it_cosr
         FROM cosr
          FOR ALL ENTRIES IN it_objnr
          WHERE lednr = '00'
          AND objnr = it_objnr-objnr
          AND gjahr = p_gjahr
          AND wrttp = '01'                 " Plan
          AND versn = p_versn
          AND stagr = p_stagr.                              " 'CS301'.
  CLEAR : it_cosr.

* Local Data definition
  FIELD-SYMBOLS: <fs1> TYPE ANY.
  DATA : lv_lst_nam(30).
  DATA : lv_cnt  LIKE  cosp-perbl.

  CLEAR   it_cosr.
  CLEAR : it_tmp_cosr, it_tmp_cosr[].


  LOOP AT it_cosr.
* Key Part
    it_tmp_cosr-gjahr = p_gjahr.
*Requested by dhkim,20041020,changed by wskim
*-----Start
* Period Counter : Set From-Period .
    CLEAR lv_cnt.
*    it_tmp_cosr-perid = p_perid.
    lv_cnt = p_perid .
*-----End
    it_tmp_cosr-kostl = it_cosr-objnr+6(10).
    it_tmp_cosr-lstar = it_cosr-objnr+16(6).
    it_tmp_cosr-stagr = it_cosr-stagr.
    it_tmp_cosr-meinh = it_cosr-meinh.
*Requested by dhkim,20041020,changed by wskim
*-----Start
    DO gv_percount TIMES.
* Period
      CLEAR it_tmp_cosl-perid.
      it_tmp_cosr-perid = lv_cnt.
*-----End
* Value Transferring
      CLEAR lv_lst_nam.
*Requested by dhkim,20041020,changed by wskim
*-----Start
*    CONCATENATE 'IT_COSR-'  'SME'  p_perid
*           INTO lv_lst_nam.
      CONCATENATE 'IT_COSR-'  'SME' lv_cnt
             INTO lv_lst_nam.
*-----End
      ASSIGN (lv_lst_nam) TO <fs1>.
      CLEAR it_tmp_cosr-smexxx.
      it_tmp_cosr-smexxx = <fs1>.

*   Unit Conversion
*    IF IT_TMP_COSR-MEINH <> 'STD'.
*      PERFORM UNIT_CONV USING IT_TMP_COSR-MEINH
*                              IT_TMP_COSR-SMEXXX.
*    ENDIF.

* Collect
      COLLECT it_tmp_cosr.
*Requested by dhkim,20041020,changed by wskim
*-----Start
* Period Counter
      lv_cnt = lv_cnt + 1.
    ENDDO.
*-----End
    CLEAR it_tmp_cosr.
    CLEAR it_cosr.
  ENDLOOP.
  CLEAR it_tmp_cosr.


ENDFORM.                    " READ_COSR
*&---------------------------------------------------------------------*
*&      Form  CAL_ALLOCATED_QTY
*&---------------------------------------------------------------------*
*       Calculating Qty
*----------------------------------------------------------------------*
FORM cal_allocated_qty.

  DATA : p_tot TYPE p DECIMALS 2,
         tot   TYPE p DECIMALS 2.
  CLEAR : p_tot, tot.
*Requested by dhkim,20041020,changed by wskim
*---Start
  SORT it_tmp_cosr BY perid kostl.
*---End
* Preparation
  CLEAR : it_tmp_cosr_sum, it_tmp_cosr_sum[].
  LOOP AT it_tmp_cosr.
    MOVE-CORRESPONDING it_tmp_cosr TO it_tmp_cosr_sum.
    COLLECT it_tmp_cosr_sum.
    CLEAR   it_tmp_cosr_sum.
  ENDLOOP.
  CLEAR it_tmp_cosr_sum.
*Requested by dhkim,20041020,changed by wskim
*---Start

*  READ TABLE it_tmp_cosr_sum INDEX 1.
*  IF sy-subrc = 0.
*    p_tot = it_tmp_cosr_sum-smexxx.
*  ENDIF.
*
*  READ TABLE it_tmp_cosl INDEX 1.
*  IF sy-subrc = 0.
*    tot = it_tmp_cosl-curqty.
*    ac_qty_tot = tot.
*  ENDIF.
*
*
*
** allocation
*  DATA  : line  TYPE i.
*  CLEAR : line.
*  DESCRIBE TABLE it_tmp_cosr LINES line.
*
*  DATA  temp(9) TYPE p DECIMALS 3.
*  CLEAR temp.
*  LOOP AT it_tmp_cosr.
*
*    it_tmp_cosr-a_qty = tot * ( it_tmp_cosr-smexxx  /  p_tot ).
*    temp = temp + it_tmp_cosr-a_qty.
*
*    IF sy-tabix = line.
*      it_tmp_cosr-a_qty = ( tot - temp ) + it_tmp_cosr-a_qty.
*    ENDIF.
*
*    MODIFY it_tmp_cosr.
*    CLEAR  it_tmp_cosr.
*  ENDLOOP.
*  CLEAR  it_tmp_cosr.
**************************
* allocation
  DATA  : line  TYPE i.
  CLEAR : line.
  DESCRIBE TABLE it_tmp_cosr LINES line.

  DATA  temp(9) TYPE p DECIMALS 3.
  CLEAR temp.
  LOOP AT it_tmp_cosr.
    READ TABLE it_tmp_cosr_sum WITH KEY perid = it_tmp_cosr-perid.
    IF sy-subrc = 0.
      p_tot = it_tmp_cosr_sum-smexxx.
    ENDIF.

    READ TABLE it_tmp_cosl  WITH KEY perid =  it_tmp_cosr-perid .
    IF sy-subrc = 0.
      tot = it_tmp_cosl-curqty.
      ac_qty_tot = tot.
    ENDIF.

    it_tmp_cosr-a_qty = tot * ( it_tmp_cosr-smexxx  /  p_tot ).
    temp = temp + it_tmp_cosr-a_qty.

    IF sy-tabix = line.
      it_tmp_cosr-a_qty = ( tot - temp ) + it_tmp_cosr-a_qty.
    ENDIF.

    MODIFY it_tmp_cosr.
    CLEAR  it_tmp_cosr.
  ENDLOOP.
  CLEAR  it_tmp_cosr.



*---End

ENDFORM.                    " CAL_ALLOCATED_QTY
*&---------------------------------------------------------------------*
*&      Form  CREATE_OBJNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_objnr.
  REFRESH it_objnr.
  CLEAR  it_objnr.

  IF it_sender-kostl = '0000055103'.                        "HIS20094
    it_objnr-objnr = 'KSH2010000055001      '.
    APPEND it_objnr.
    CLEAR  it_objnr.
    it_objnr-objnr = 'KSH2010000055002      '.
    APPEND it_objnr.
    CLEAR  it_objnr.
    it_objnr-objnr = 'KSH2010000055003      '.
    APPEND it_objnr.
    CLEAR  it_objnr.
    it_objnr-objnr = 'KSH2010000055004      '.
    APPEND it_objnr.
    CLEAR  it_objnr.
*  it_objnr-objnr = 'KSH2010000055005      '.              "HIS20094
*  append it_objnr.                                        "HIS20094
*  clear  it_objnr.                                        "HIS20094

  ELSEIF it_sender-kostl = '0000055107'.                    "HIS20094
    it_objnr-objnr = 'KSH2010000055005      '.              "HIS20094
    APPEND it_objnr.                                        "HIS20094
    CLEAR  it_objnr.                                        "HIS20094

    it_objnr-objnr = 'KSH2010000055015      '.
    APPEND it_objnr.
    CLEAR  it_objnr.
  ENDIF.                                                    "HIS20094

ENDFORM.                    " CREATE_OBJNR
*&---------------------------------------------------------------------*
*&      Form  CREATE_COSS_OBJNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_coss_objnr.

  CLEAR it_objnr2.
  CLEAR it_objnr2[].                                        "HIS20094

  IF it_sender-kostl = '0000055103'.                        "HIS20094
    it_objnr2-objnr = 'KLH2010000055001PRS_HR'.
    APPEND it_objnr2.
    CLEAR  it_objnr2.
    it_objnr2-objnr = 'KLH2010000055002PRS_HR'.
    APPEND it_objnr2.
    CLEAR  it_objnr2.
    it_objnr2-objnr = 'KLH2010000055003PRS_HR'.
    APPEND it_objnr2.
    CLEAR  it_objnr2.
    it_objnr2-objnr = 'KLH2010000055004PRS_HR'.
    APPEND it_objnr2.
    CLEAR  it_objnr2.
*  it_objnr2-objnr = 'KLH2010000055005PRS_HR'.             "HIS20094
*  append it_objnr2.                                       "HIS20094
*  clear  it_objnr2.                                       "HIS20094

  ELSEIF it_sender-kostl = '0000055107'.                    "HIS20094
    it_objnr2-objnr = 'KLH2010000055005PRS_HR'.             "HIS20094
    APPEND it_objnr2.                                       "HIS20094
    CLEAR  it_objnr2.                                       "HIS20094

    it_objnr2-objnr = 'KLH2010000055015PRS_HR'.             "HIS20094
    APPEND it_objnr2.                                       "HIS20094
    CLEAR  it_objnr2.                                       "HIS20094
  ENDIF.                                                    "HIS20094

ENDFORM.                    " CREATE_COSS_OBJNR
*&---------------------------------------------------------------------*
*&      Form  SET_VALUE_AMT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_value_amt.

* Fixed Cost
* Variable Cost
  trans_value 01. trans_value 02. trans_value 03. trans_value 04.
  trans_value 05. trans_value 06. trans_value 07. trans_value 08.
  trans_value 09. trans_value 10. trans_value 11. trans_value 12.
  trans_value 13. trans_value 14. trans_value 15. trans_value 16.


ENDFORM.                    " SET_VALUE_AMT
*&---------------------------------------------------------------------*
*&      Form  POSTING_DATA
*&---------------------------------------------------------------------*
*       Preparation of posting data.
*----------------------------------------------------------------------*
FORM posting_data.
* Begin of HIS20094 - Hard Coding removed
* Begin of HIS20094 - Hard Coding as requested by Michael Yoon
*    loop at it_report where r_kostl = 'MXTX12'.
*        it_report-kostl   = '0000055004'.
*        it_report-objnr   = 'KLH2010000055004PRS_HR'.
*        it_report-p_kostl = '0000055103'.
*        modify it_report transporting kostl objnr p_kostl.
*    endloop.
* End of HIS20094
* End of HIS20094

  CLEAR : it_post, it_post[].
  CLEAR : it_report.
  LOOP AT it_report.
    MOVE-CORRESPONDING it_report TO it_post.

    IF it_report-perid = '001'.
      it_post-var001 = it_report-vaeqty.
    ELSEIF it_report-perid = '002'.
      it_post-var002 = it_report-vaeqty.
    ELSEIF it_report-perid = '003'.
      it_post-var003 = it_report-vaeqty.
    ELSEIF it_report-perid = '004'.
      it_post-var004 = it_report-vaeqty.
    ELSEIF it_report-perid = '005'.
      it_post-var005 = it_report-vaeqty.
    ELSEIF it_report-perid = '006'.
      it_post-var006 = it_report-vaeqty.
    ELSEIF it_report-perid = '007'.
      it_post-var007 = it_report-vaeqty.
    ELSEIF it_report-perid = '008'.
      it_post-var008 = it_report-vaeqty.
    ELSEIF it_report-perid = '009'.
      it_post-var009 = it_report-vaeqty.
    ELSEIF it_report-perid = '010'.
      it_post-var010 = it_report-vaeqty.
    ELSEIF it_report-perid = '011'.
      it_post-var011 = it_report-vaeqty.
    ELSEIF it_report-perid = '012'.
      it_post-var012 = it_report-vaeqty.
    ELSEIF it_report-perid = '013'.
      it_post-var013 = it_report-vaeqty.
    ELSEIF it_report-perid = '014'.
      it_post-var014 = it_report-vaeqty.
    ELSEIF it_report-perid = '015'.
      it_post-var015 = it_report-vaeqty.
    ELSEIF it_report-perid = '016'.
      it_post-var016 = it_report-vaeqty.
    ENDIF.

    COLLECT it_post.
    CLEAR it_post.
  ENDLOOP.
  CLEAR it_post.

ENDFORM.                    " POSTING_DATA
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
*&      Form  CAL_PER_COUNT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cal_per_count.
  gv_percount = p_toper - p_perid + 1.

ENDFORM.                    " CAL_PER_COUNT
*&---------------------------------------------------------------------*
*&      Form  populate_sender
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM populate_sender.
  CLEAR: it_sender, it_sender[].

  it_sender-kostl = '0000055103'.
  APPEND it_sender.
  it_sender-kostl = '0000055107'.
  APPEND it_sender.

ENDFORM.                    " populate_sender
