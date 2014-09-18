************************************************************************
* Program Name      : ZACO21U_SKF16
* Author            : Eun Hwa , Jung
* Creation Date     : 2004.01.28.
* Specifications By : Dong-Hwan Kim
* Pattern           : Report 1-1
* Development Request No : UD1K905445
* Addl Documentation:
* Description       : Create Semi-Direct Cost Center Activity Output
* the BDC structures for BATCH INPUT processing
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************


REPORT zaco21u_skf16 MESSAGE-ID zmco.

** type-pools
TYPE-POOLS: slis.

** Table
TABLES : csks , cssl, cosl,  zthr_pcp05.

** Internal Table
DATA: BEGIN OF it_zthr_pcp05 OCCURS 0,
        zscst TYPE zthr_pcp05-zscst,
        zyear TYPE zthr_pcp05-zyear,
        zmons TYPE zthr_pcp05-zmons,
        zvers TYPE zthr_pcp05-zvers,
***Issue Number : CO-20041115-010, Requested by DHKIM
***Changed on 2004/11/30, by WSKIM
***---Start
**        zgrup TYPE zthr_pcp05-zgrup,
**        zseqn TYPE zthr_pcp05-zseqn,
**        zplnd TYPE zthr_pcp05-zplnd,
**        zhedc TYPE zthr_pcp05-zhedc,
**        zprat TYPE zthr_pcp05-zprat,
***---End
        ztotm TYPE zthr_pcp05-ztotm,
      END OF it_zthr_pcp05.


DATA: BEGIN OF it_sum OCCURS 0,
        zscst TYPE zthr_pcp05-zscst,
        zyear TYPE zthr_pcp05-zyear,
        zmons TYPE zthr_pcp05-zmons,
        zvers TYPE zthr_pcp05-zvers,
        ztotm TYPE zthr_pcp05-ztotm,
      END OF it_sum.

* for reporting
DATA : BEGIN OF it_report OCCURS 0,
        gjahr   LIKE  cosl-gjahr,
        perid   TYPE zthr_hcp01-zmons,
        kostl   LIKE  csks-kostl,
        lstar   LIKE  csla-lstar,
        count   TYPE zthr_pcp05-ztotm,
      END OF it_report.

* for posting
DATA : BEGIN OF it_post OCCURS 0,
        gjahr   LIKE  cosl-gjahr,
        kostl   LIKE  csks-kostl,
        lstar   LIKE  csla-lstar.
        INCLUDE STRUCTURE zsco_cosp_amt02.
DATA : END OF  it_post.


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

*
DATA : gv_percount       LIKE cosp-perbl. "Period Counter

* For BAPI
DATA : it_costcenterlist LIKE STANDARD TABLE OF bapi0012_cclist
                         WITH HEADER LINE.
DATA : it_return         LIKE STANDARD TABLE OF bapiret2
                         WITH HEADER LINE.
DATA : wa_headerinfo     LIKE bapiplnhdr.
DATA : it_indexstructure LIKE STANDARD TABLE OF bapiacpstru
                         WITH HEADER LINE.
DATA : it_coobject       LIKE STANDARD TABLE OF bapiacpobj
                         WITH HEADER LINE.
DATA : it_pervalue       LIKE STANDARD TABLE OF bapiacpval
                         WITH HEADER LINE.
DATA : it_totvalue       LIKE STANDARD TABLE OF bapiacptot
                         WITH HEADER LINE.

** For OBJECT KEY
DATA : BEGIN OF wa_obj ,
        objnr  LIKE  coss-objnr,
        kostl  LIKE  csks-kostl,
        lstar  LIKE  csla-lstar,
       END OF wa_obj.

DATA : it_obj_cctr_at   LIKE STANDARD TABLE OF wa_obj
                        WITH HEADER LINE .

DATA : BEGIN OF it_cosl OCCURS 0,
         objnr  LIKE  cosl-objnr,
       END OF   it_cosl.

DATA : BEGIN OF it_lstar OCCURS 0,
        kostl  LIKE  csks-kostl,
        lstar  LIKE  csla-lstar,
       END OF   it_lstar.


RANGES : rs_perid FOR zthr_pcp05-zmons.

* Macro For Transferring value in BAPI
DEFINE trans_value.
  it_pervalue-actvty_qty_per&1  =  it_post-var0&1.
*  QUANTITY_PER&1
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
* Calculating Period Count
  PERFORM cal_per_count.
* Read Cost Center Group  -> Cost Center
  PERFORM read_cegrp.
* Read OBJ Key Combination
  PERFORM set_obj_key.
* Read Quantity Information from COSL
  PERFORM read_cosl.
* Read ' Plan Working Hour ' data.
  PERFORM read_workhour_plan.
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
*&      Form  CAL_PER_COUNT
*&---------------------------------------------------------------------*
*       Calculation STD. - period Counter
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cal_per_count.

  gv_percount = p_toper - p_frper + 1.

ENDFORM.                    " CAL_PER_COUNT
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
  SORT it_report BY gjahr perid kostl.
  CLEAR it_report.

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
    'IT_REPORT' 'GJAHR'  'X'            space    space
    space        '4'      'Year'           space    space    space.

  PERFORM build_fieldcat USING
    'IT_REPORT' 'PERID'  'X'            space    space
    space        '6'      'Period'           space    space    space.

  PERFORM build_fieldcat USING
    'IT_REPORT' 'KOSTL'  'X'            space    space
    space        '11'      'Cost Center'        space    space    space.

  PERFORM build_fieldcat USING
    'IT_REPORT' 'LSTAR'  'X'            space    space
    space        '8'      'Activity'    space    space    space.

** Value
  PERFORM build_fieldcat USING
    'IT_REPORT' 'COUNT'  space            'X'    space
    'IT_REPORT'   '15'      'Quantity'      space   space  space .


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
      PERFORM post_std USING ucomm.
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
*&      Form  CALL_POST_FM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_post_fm.


  CALL FUNCTION 'BAPI_COSTACTPLN_POSTACTOUTPUT'
    EXPORTING
      headerinfo           = wa_headerinfo
*    DELTA                = ' '
    TABLES
      indexstructure       = it_indexstructure
      coobject             = it_coobject
      pervalue             = it_pervalue
*    TOTVALUE             =
*    CONTRL               =
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

* Check Cost Center/Cost Center Group
  IF s_kostl[] IS INITIAL AND
     p_ncoal   IS INITIAL .
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


ENDFORM.                    " CHK_INPUT_VALUE
*&---------------------------------------------------------------------*
*&      Form  POSTING_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM posting_data.

  CLEAR : it_post, it_post[].
  CLEAR : it_report.
  LOOP AT it_report.
    MOVE-CORRESPONDING it_report TO it_post.

    IF it_report-perid = '01'.
      it_post-var001 = it_report-count.
    ELSEIF it_report-perid = '02'.
      it_post-var002 = it_report-count.
    ELSEIF it_report-perid = '03'.
      it_post-var003 = it_report-count.
    ELSEIF it_report-perid = '04'.
      it_post-var004 = it_report-count.
    ELSEIF it_report-perid = '05'.
      it_post-var005 = it_report-count.
    ELSEIF it_report-perid = '06'.
      it_post-var006 = it_report-count.
    ELSEIF it_report-perid = '07'.
      it_post-var007 = it_report-count.
    ELSEIF it_report-perid = '08'.
      it_post-var008 = it_report-count.
    ELSEIF it_report-perid = '09'.
      it_post-var009 = it_report-count.
    ELSEIF it_report-perid = '10'.
      it_post-var010 = it_report-count.
    ELSEIF it_report-perid = '11'.
      it_post-var011 = it_report-count.
    ELSEIF it_report-perid = '12'.
      it_post-var012 = it_report-count.
    ELSEIF it_report-perid = '13'.
      it_post-var013 = it_report-count.
    ELSEIF it_report-perid = '14'.
      it_post-var014 = it_report-count.
    ELSEIF it_report-perid = '15'.
      it_post-var015 = it_report-count.
    ELSEIF it_report-perid = '16'.
      it_post-var016 = it_report-count.
    ENDIF.

    COLLECT it_post.
    CLEAR it_post.
  ENDLOOP.
  CLEAR it_post.

  SORT it_post BY gjahr kostl.


ENDFORM.                    " POSTING_DATA
*&---------------------------------------------------------------------*
*&      Form  READ_WORKHOUR_PLAN
*&---------------------------------------------------------------------*
*       Read Working Hour Plan data
*----------------------------------------------------------------------*
FORM read_workhour_plan.

  rs_perid-sign = 'I'.
  rs_perid-option = 'BT'.
  rs_perid-low = p_frper+1(2).
  rs_perid-high = p_toper+1(2).
  APPEND rs_perid.   CLEAR rs_perid.


** Plan Working hour data
  CLEAR : it_zthr_pcp05, it_zthr_pcp05[].
  CLEAR zthr_pcp05.
  SELECT *
    APPENDING CORRESPONDING FIELDS OF TABLE it_zthr_pcp05
      FROM zthr_pcp05
        FOR ALL ENTRIES IN it_costcenterlist
        WHERE
* ZCOST = IT_COSTCENTERLIST-COSTCENTER
               zscst = it_costcenterlist-costcenter
         AND   zyear = p_gjahr
         AND   zmons IN rs_perid
         AND   zvers = p_verhr.
  CLEAR it_zthr_pcp05.

*  DATA : LINE TYPE I.
*  CLEAR  LINE.
*  DESCRIBE TABLE IT_ZTHR_PCP05 LINES LINE.
*  IF LINE = 0.
*    MESSAGE E000 WITH
*    'Please check, Table:ZTHR_PCP05 data not found '.
*  ENDIF.
***Issue Number : CO-20041115-010, Requested by DHKIM
***Changed on 2004/11/17, by WSKIM
***---Start
**  LOOP AT it_zthr_pcp05 WHERE zseqn EQ 1.
**
**    it_zthr_pcp05-ztotm =  it_zthr_pcp05-ztotm +
**          ( it_zthr_pcp05-zplnd * it_zthr_pcp05-zhedc * 8
**            *  it_zthr_pcp05-zprat / 100 ).
**    MODIFY it_zthr_pcp05 FROM it_zthr_pcp05.
**
**  ENDLOOP.
**
***---End

  CLEAR : it_sum, it_sum[].
  CLEAR   it_zthr_pcp05.
  LOOP AT it_zthr_pcp05.
    MOVE-CORRESPONDING it_zthr_pcp05 TO it_sum.
    COLLECT it_sum.
    CLEAR   it_sum.
  ENDLOOP.


** Preparation reporting
  CLEAR : it_report, it_report[].
  CLEAR it_sum.
  LOOP AT it_sum.
    it_report-gjahr  = it_sum-zyear.
    it_report-perid  = it_sum-zmons.
    it_report-kostl  = it_sum-zscst.
    CLEAR it_lstar.
    READ TABLE it_lstar WITH KEY kostl = it_sum-zscst.
    IF sy-subrc = 0.
      it_report-lstar  = it_lstar-lstar.
    ELSE.
      MESSAGE s000 WITH
    ' Activity Output data not found - check Tcode KP26'.
    ENDIF.
    it_report-count  = it_sum-ztotm.
    COLLECT it_report.
    CLEAR   it_report.
  ENDLOOP.
  CLEAR it_sum.

  SORT it_report BY gjahr perid kostl.

ENDFORM.                    " READ_WORKHOUR_PLAN
*&---------------------------------------------------------------------*
*&      Form  POST_STD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_UCOMM  text
*----------------------------------------------------------------------*
FORM post_std USING    p_ucomm.

* Init. Message TAB
  CLEAR : it_return, it_return[].

* Fill Header DATA
  CLEAR wa_headerinfo.
  wa_headerinfo-co_area        = p_kokrs.
  wa_headerinfo-fisc_year      = p_gjahr.
  wa_headerinfo-period_from	 = p_frper.
  wa_headerinfo-period_to	 = p_toper.
  wa_headerinfo-version        = p_versn.
  wa_headerinfo-plan_currtype	 = p_currt.

* Fill Object List and Plan Values per Period
  CLEAR : it_indexstructure, it_indexstructure[].
  CLEAR : it_coobject,       it_coobject[].
  CLEAR : it_pervalue,       it_pervalue[].
  CLEAR : it_totvalue,       it_totvalue[].

* Sort to post data.
  SORT it_post BY kostl lstar.

  LOOP AT it_post.
* Obj
    ON CHANGE OF  it_post-kostl.

* Index of Object Key
      it_indexstructure-object_index
           = it_indexstructure-object_index + 1 .

      CLEAR it_coobject.
      it_coobject-object_index = it_indexstructure-object_index.

      it_coobject-costcenter   = it_post-kostl.
      it_coobject-acttype      = it_post-lstar.
      APPEND it_coobject.
      CLEAR  it_coobject.
    ENDON.

* Value.
* Index of Value
    it_indexstructure-value_index
         = it_indexstructure-value_index + 1.

    CLEAR it_pervalue.
    it_pervalue-value_index = it_indexstructure-value_index.

* Set Value
* Post/Reverse
    IF  p_ucomm = 'POST'.
      PERFORM set_value_amt.
    ENDIF.

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

  ENDIF.


ENDFORM.                    " POST_STD
*&---------------------------------------------------------------------*
*&      Form  SET_OBJ_KEY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_obj_key.

  CLEAR : it_obj_cctr_at, it_obj_cctr_at[].

* CCtr  - > objnr , kostl , lstar
  CLEAR cssl.
  SELECT  objnr kostl lstar
                      INTO CORRESPONDING FIELDS OF TABLE it_obj_cctr_at
                      FROM cssl
                     FOR ALL ENTRIES IN it_costcenterlist
                     WHERE kokrs = p_kokrs
                       AND kostl = it_costcenterlist-costcenter
                       AND gjahr = p_gjahr.

  CLEAR : it_obj_cctr_at.

  DATA : line TYPE i.
  CLEAR  line.
  DESCRIBE TABLE it_obj_cctr_at LINES line.
  IF line = 0.
    MESSAGE e000 WITH
    ' Cost Center/Activity Type, data not found - check Table CSSL'.
  ENDIF.



ENDFORM.                    " SET_OBJ_KEY
*&---------------------------------------------------------------------*
*&      Form  READ_COSL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_cosl.

  CLEAR : it_cosl, it_cosl[].
  CLEAR cosl.
  SELECT *
         INTO CORRESPONDING FIELDS OF TABLE it_cosl
         FROM cosl
          FOR ALL ENTRIES IN it_obj_cctr_at
        WHERE lednr = '00'
          AND objnr = it_obj_cctr_at-objnr
          AND gjahr = p_gjahr
          AND wrttp = '01'             " plan : '01'
          AND versn = p_versn.
  CLEAR : it_cosl.

  DATA : line TYPE i.
  CLEAR  line.
  DESCRIBE TABLE it_cosl LINES line.
  IF line = 0.
    MESSAGE e000 WITH
    ' Activity Output data not found - check Tcode KP26'.
  ENDIF.


  CLEAR it_lstar.
  DATA : lv_lstar LIKE cssl-lstar,
         lv_kostl LIKE cssl-kostl.
  CLEAR : lv_lstar, lv_kostl.

  LOOP AT it_cosl.
    CLEAR : lv_lstar, lv_kostl.
    CALL FUNCTION 'OBJECT_KEY_GET_KL'
      EXPORTING
        objnr             = it_cosl-objnr
      IMPORTING
*     KOKRS             =
       kostl             =  lv_kostl
       lstar             =  lv_lstar      .

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    it_lstar-kostl = lv_kostl.
    it_lstar-lstar = lv_lstar.
    COLLECT it_lstar.
    CLEAR   it_lstar.

  ENDLOOP.


ENDFORM.                    " READ_COSL
