************************************************************************
* Program Name      : ZACO51U_TCCA
* Author            : Eun Hwa , Jung
* Creation Date     : 2004.02.14.
* Specifications By : Dong-Hwan Kim, Andy Choi
* Pattern           : Report 1-1
* Development Request No : UD1K907356
* Addl Documentation:
* Description       : Transfer Remain CCA Variance


* the BDC structures for BATCH INPUT processing
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************


REPORT zaco51u_tcca MESSAGE-ID zmco.

** type-pools
TYPE-POOLS: slis.

** Table
TABLES : coss, cosp, csks, cosl, afpo.
TABLES: t001.


** For OBJECT KEY
DATA : BEGIN OF wa_obj ,
        objnr  LIKE  coss-objnr,
        kostl  LIKE  csks-kostl,
        lstar  LIKE  csla-lstar,
       END OF wa_obj.
DATA : it_obj_cctr_at   LIKE STANDARD TABLE OF wa_obj
                        WITH HEADER LINE .

DATA : it_obj_cctr_at_2   LIKE STANDARD TABLE OF wa_obj
                        WITH HEADER LINE .

** Internal Table
DATA : BEGIN OF it_cosp OCCURS 500,
                 objnr TYPE cosp-objnr,
                 gjahr TYPE cosp-gjahr,
                 wrttp TYPE cosp-wrttp,
                 versn TYPE cosp-versn,
                 meinh TYPE cosp-meinh,
                 wtg001 TYPE cosl-lst001,
                 wtg002 TYPE cosp-wtg002,
                 wtg003 TYPE cosp-wtg003,
                 wtg004 TYPE cosp-wtg004,
                 wtg005 TYPE cosp-wtg005,
                 wtg006 TYPE cosp-wtg006,
                 wtg007 TYPE cosp-wtg007,
                 wtg008 TYPE cosp-wtg008,
                 wtg009 TYPE cosp-wtg009,
                 wtg010 TYPE cosp-wtg010,
                 wtg011 TYPE cosp-wtg011,
                 wtg012 TYPE cosp-wtg012,
                 wtg013 TYPE cosp-wtg013,
                 wtg014 TYPE cosp-wtg014,
                 wtg015 TYPE cosp-wtg015,
                 wtg016 TYPE cosp-wtg016,
               END OF it_cosp.


DATA: BEGIN OF it_tmp_cosp OCCURS 0,
        gjahr  LIKE coss-gjahr,
        perid  LIKE coejl-perbl,
*        OBJNR  LIKE COSS-OBJNR,
        kostl  LIKE csks-kostl,
*        LSTAR  LIKE CSLA-LSTAR,
        balamt LIKE cosl-lst001,
      END OF it_tmp_cosp.


DATA: BEGIN OF it_cosl OCCURS 0,
        lednr TYPE cosl-lednr,
        objnr TYPE cosl-objnr,
        gjahr TYPE cosl-gjahr,
        wrttp TYPE cosl-wrttp,
        versn TYPE cosl-versn,
        lst001 TYPE cosl-lst001,
        lst002 TYPE cosl-lst002,
        lst003 TYPE cosl-lst003,
        lst004 TYPE cosl-lst004,
        lst005 TYPE cosl-lst005,
        lst006 TYPE cosl-lst006,
        lst007 TYPE cosl-lst007,
        lst008 TYPE cosl-lst008,
        lst009 TYPE cosl-lst009,
        lst010 TYPE cosl-lst010,
        lst011 TYPE cosl-lst011,
        lst012 TYPE cosl-lst012,
      END OF it_cosl.

DATA: BEGIN OF it_tmp_cosl OCCURS 0,
        gjahr  LIKE  cosl-gjahr,
        perid  LIKE  coejl-perbl,
        objnr  LIKE  coss-objnr,
        balamt LIKE  cosl-lst001,
      END OF it_tmp_cosl.


DATA: BEGIN OF it_coss OCCURS 0,
        lednr TYPE coss-lednr,
        objnr TYPE coss-objnr,
        gjahr TYPE coss-gjahr,
        wrttp TYPE coss-wrttp,
        versn TYPE coss-versn,
        kstar TYPE coss-kstar,
        vrgng TYPE coss-vrgng,
        parob TYPE coss-parob,
        uspob TYPE coss-uspob,
      END OF it_coss.


DATA: BEGIN OF it_post OCCURS 0,
        hkont(10),
        shkzg TYPE bseg-shkzg,
        wrbtr(15), " TYPE BSEG-WRBTR,
        kostl(10),
        zuonr LIKE bseg-zuonr,
      END OF it_post.

* for reporting
DATA : BEGIN OF it_report OCCURS 0,
        gjahr LIKE  cosl-gjahr,
        perid LIKE  coejl-perbl,
        kostl LIKE  csks-kostl,
        lstar LIKE  csla-lstar,
        bldat   LIKE coheader-bldat,          " doc date
        budat  LIKE coheader-budat,          " Posting date
        eaufnr LIKE  rk40c_kbxxn-eaufnr,     " receiver order
        megxxx LIKE  rk40c_kbxxn-mbgbtr,     " Total qty
        balamt LIKE cosl-lst001,             " Total price
      END OF it_report.

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

*** For BAPI
DATA : it_costcenterlist LIKE STANDARD TABLE OF bapi0012_cclist
                         WITH HEADER LINE.
DATA : it_return         LIKE STANDARD TABLE OF bapiret2
                         WITH HEADER LINE.

*  (kb21n)
DATA : wa_doc_header LIKE bapidochdrp .
DATA : it_doc_items  LIKE STANDARD TABLE OF bapiaaitm
                     WITH HEADER LINE.

RANGES : rs_date FOR mara-ersda.


*** Global variance
DATA : gv_postdate(8),
       gv_actype LIKE cssl-lstar,
       gv_totqty(1).


DATA:
      obj_type LIKE bapiache02-obj_type,
      obj_key LIKE bapiache02-obj_key,
      obj_sys LIKE bapiache02-obj_sys,

      documentheader  LIKE bapiache08,
      accountgl       LIKE bapiacgl08  OCCURS 0 WITH HEADER LINE,
      currencyamount  LIKE bapiaccr08  OCCURS 0 WITH HEADER LINE,
      return          LIKE bapiret2    OCCURS 0 WITH HEADER LINE,
      extension1      LIKE bapiextc    OCCURS 0 WITH HEADER LINE,

      t_edidd         LIKE edidd       OCCURS 0 WITH HEADER LINE,
      bapi_retn_info  LIKE bapiret2    OCCURS 0 WITH HEADER LINE.

*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.
PARAMETERS :
             p_kokrs LIKE csks-kokrs MEMORY ID cac OBLIGATORY,
             p_gjahr LIKE cobk-gjahr MEMORY ID gjr OBLIGATORY,
             p_perid LIKE cosp-perbl MEMORY ID bpe OBLIGATORY,
             p_versn LIKE cobk-versn MEMORY ID kvt OBLIGATORY.
*             P_TRUN(1).
SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK bl3 WITH FRAME TITLE text-003.
SELECT-OPTIONS : s_kostl  FOR csks-kostl. " DEFAULT 'MXBXB1'.
PARAMETERS:      p_ncoal LIKE grpdynp-name_coall
                                     DEFAULT 'DIRECT'.
select-options : s_KSTAR  for cosp-KSTAR.
SELECTION-SCREEN END OF BLOCK bl3.
SELECTION-SCREEN END OF BLOCK bl1.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
PARAMETERS: p_run    AS CHECKBOX DEFAULT ' ',
            p_blart  LIKE bapiache08-doc_type DEFAULT 'SA'.
PARAMETERS:
      p_fo    LIKE ska1-saknr DEFAULT '0000590090' NO-DISPLAY,
      p_cogs  LIKE ska1-saknr DEFAULT '0000530990' NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK b3.


constants: c_nstar like GRPDYNP-NAME_COALL value 'CKCC'.

*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
INITIALIZATION.
* Set Global ALV Parameter
  gv_repid = sy-repid.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*

* Searching for Cost Center group
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_ncoal.
  PERFORM read_cegrp_group USING '0101'
                                 p_ncoal.

*AT SELECTION-SCREEN.
*  PERFORM CHK_INPUT_VALUE.


*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Check input value
  PERFORM chk_input_value.
*FIXIT
  SELECT SINGLE * FROM t001 WHERE bukrs = p_kokrs.

* preparation data
  PERFORM rre_data.
* Read Cost Center Group - > Cost Center
  PERFORM read_ccgrp.
  perform read_cegrp.
* Read OBJ Key Combination
  PERFORM set_obj_key.
* Read Remain Variance Information from COSP and COSS
  PERFORM read_qty.
* Read Order Info.
  PERFORM read_order.
* Preparation of reporting data.
  PERFORM pre_report_data.
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
*&      Form  READ_CCGRP
*&---------------------------------------------------------------------*
*       read cost center
*----------------------------------------------------------------------*
FORM read_ccgrp.

* Making an internal table for CCtr to select data
* Selected Group on screen
  CLEAR : it_costcenterlist, it_costcenterlist[].
  CLEAR : it_return, it_return[].

* Set Validity Date (Start)
  DATA : lv_datum LIKE sy-datum.
  CONCATENATE p_gjahr p_perid+1(2) '01' INTO lv_datum.

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


ENDFORM.                    " READ_CCGRP
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
*&      Form  SET_OBJ_KEY
*&---------------------------------------------------------------------*
*       Read OBJ Key Combination
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_obj_key.

  CLEAR : it_obj_cctr_at, it_obj_cctr_at[].
  CLEAR : it_obj_cctr_at_2, it_obj_cctr_at_2[].

  LOOP AT it_costcenterlist.
    CALL FUNCTION 'K_LSTAR_OBJECT_KEY_GET'
         EXPORTING
              kokrs = p_kokrs
              kostl = it_costcenterlist-costcenter
              lstar = ' '
         IMPORTING
              objnr = it_obj_cctr_at-objnr.
    it_obj_cctr_at-kostl = it_costcenterlist-costcenter.
    APPEND it_obj_cctr_at. CLEAR it_obj_cctr_at.


    CALL FUNCTION 'K_KOSTL_OBJECT_KEY_GET'
         EXPORTING
              kokrs = p_kokrs
              kostl = it_costcenterlist-costcenter
         IMPORTING
              objnr = it_obj_cctr_at-objnr.
    it_obj_cctr_at-kostl = it_costcenterlist-costcenter.
    APPEND it_obj_cctr_at. CLEAR it_obj_cctr_at.



    CALL FUNCTION 'K_LSTAR_OBJECT_KEY_GET'
         EXPORTING
              kokrs = p_kokrs
              kostl = it_costcenterlist-costcenter
              lstar = 'MAN_HR'
         IMPORTING
              objnr = it_obj_cctr_at_2-objnr.
    it_obj_cctr_at_2-kostl = it_costcenterlist-costcenter.
    APPEND it_obj_cctr_at_2. CLEAR it_obj_cctr_at_2.



    CLEAR it_costcenterlist.
  ENDLOOP.
  CLEAR : it_obj_cctr_at.
  CLEAR : it_obj_cctr_at_2.


ENDFORM.                    " SET_OBJ_KEY
*&---------------------------------------------------------------------*
*&      Form  READ_QTY
*&---------------------------------------------------------------------*
*       Read Quantity data from COSP and COSS
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_qty.

  DATA $objnr LIKE wa_obj-objnr.

  CLEAR : it_cosp, it_cosp[].
  CLEAR cosp.
  CLEAR it_obj_cctr_at.
  LOOP AT it_obj_cctr_at.
    CLEAR $objnr.
    CONCATENATE '%' it_obj_cctr_at-objnr '%' INTO $objnr.
    SELECT *
       APPENDING CORRESPONDING FIELDS OF TABLE it_cosp
       FROM cosp
*          FOR ALL ENTRIES IN IT_OBJ_CCTR_AT
      WHERE lednr = '00'
        AND objnr LIKE $objnr        " IT_OBJ_CCTR_AT-OBJNR
        AND gjahr = p_gjahr
        AND wrttp = '04'             " Actual
        AND versn = p_versn
        AND KSTAR in s_KSTAR.
  ENDLOOP.
  CLEAR : it_cosp, it_obj_cctr_at.




  CLEAR coss.
  LOOP AT it_obj_cctr_at.
    CLEAR $objnr.
    CONCATENATE '%' it_obj_cctr_at-objnr '%' INTO $objnr.
    SELECT *
       APPENDING CORRESPONDING FIELDS OF TABLE it_cosp
       FROM coss
*          FOR ALL ENTRIES IN IT_OBJ_CCTR_AT
      WHERE lednr = '00'
        AND objnr LIKE $objnr       " IT_OBJ_CCTR_AT-OBJNR
        AND gjahr = p_gjahr
        AND wrttp = '04'             " Actual
        AND versn = p_versn
        AND KSTAR in s_KSTAR.
  ENDLOOP.
  CLEAR : it_cosp, it_obj_cctr_at.


* Local Data definition
  FIELD-SYMBOLS: <fs1> TYPE ANY.
  DATA : lv_lst_nam(30).

  CLEAR   it_cosp.
  CLEAR : it_tmp_cosp, it_tmp_cosp[].

  LOOP AT it_cosp.
* Key Part
    it_tmp_cosp-gjahr = p_gjahr.
    it_tmp_cosp-perid = p_perid.

    it_tmp_cosp-kostl = it_cosp-objnr+6(10).

* Value Transferring
    CLEAR lv_lst_nam.
    CONCATENATE 'IT_COSP-'  'WTG'  p_perid
           INTO lv_lst_nam.
    ASSIGN (lv_lst_nam) TO <fs1>.
    CLEAR it_tmp_cosp-balamt.
    it_tmp_cosp-balamt = <fs1>.
* Collect
    COLLECT it_tmp_cosp.

    CLEAR it_tmp_cosp.
    CLEAR it_cosp.
  ENDLOOP.
  CLEAR it_tmp_cosp.


ENDFORM.                    " READ_QTY
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

  it_sort-fieldname = 'KOSTL'.
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
*       Building Field Cat.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fieldcat_init.

  CLEAR : gv_col_pos, it_fieldcat, it_fieldcat[].


* Key

*  PERFORM BUILD_FIELDCAT USING
*   'IT_REPORT' 'BUDAT'  'X'            SPACE    SPACE
*   SPACE '10'      'Post Date'    SPACE    SPACE SPACE.
*
  PERFORM build_fieldcat USING
    'IT_REPORT' 'KOSTL'  'X'            space    space
    space        '11'      'Cost Center'       space    space    space.

*///start modi 2004.05.25
*  PERFORM BUILD_FIELDCAT USING
*    'IT_REPORT' 'LSTAR'  'X'            SPACE    SPACE
*    SPACE        '10'      'Acty type'    SPACE    SPACE    SPACE.
*
*** Value
*  PERFORM BUILD_FIELDCAT USING
*   'IT_REPORT' 'EAUFNR'  SPACE            SPACE    SPACE
*   SPACE '10'      'Rec.Order'    SPACE    SPACE SPACE.
*
*
*
*  PERFORM BUILD_FIELDCAT USING
*   'IT_REPORT' 'MEGXXX'  SPACE            SPACE    SPACE
*   SPACE '10'      'Total qty'    SPACE    SPACE SPACE.
*///end  modi 2004.05.25

  PERFORM build_fieldcat USING
   'IT_REPORT' 'BALAMT'  space       'X'      space
   space '15'      'Over amt'    space    space space.

*  PERFORM BUILD_FIELDCAT USING
*    'IT_REPORT' 'UNIT'  SPACE            SPACE    SPACE
*    SPACE   '4'      'Unit'      'UNIT'   SPACE     SPACE.


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
*       call ALV function
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_alv_list.

  DELETE it_report WHERE balamt = 0.


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
*       IT_SORT                        = IT_SORT[]
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
    WHEN 'POST'.
*      PERFORM POST_STD  USING UCOMM.
      PERFORM post_bdc USING ucomm.


  ENDCASE.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM BASIC_TOP_OF_PAGE                                        *
*---------------------------------------------------------------------*
*       Display Title                                                 *
*---------------------------------------------------------------------*
FORM basic_top_of_page.

  WRITE : / 'Controlling Area           : '
            , p_kokrs .
  WRITE : / 'Fiscal Year/Period/Version : '
            , p_gjahr, '/', p_perid , '/', p_versn.
  WRITE : / 'Posting date               : '
            , gv_postdate.


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

*  WRITE : / 'Test Run                     ', P_TRUN.
  SKIP 1.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  POST_STD
*&---------------------------------------------------------------------*
*        The Preparation for posting
*----------------------------------------------------------------------*
*      -->P_UCOMM  text
*----------------------------------------------------------------------*
FORM post_std USING    p_ucomm.

*  DELETE IT_REPORT WHERE balamt EQ '0'.
*
*  DATA  : LINE  TYPE I.
*  CLEAR : LINE.
*  DESCRIBE TABLE IT_REPORT   LINES LINE.
*
*  IF LINE = 0.
*    MESSAGE E000(ZMCO) WITH
*    ' Enter Value not equal to 0 '.
*  ENDIF.
*
*
** Init. Message TAB
*  CLEAR : IT_RETURN, IT_RETURN[].
*
*  DATA : LV_CONF_TEXT(50).
** TEXT
*  CLEAR LV_CONF_TEXT.
*  CONCATENATE SY-UNAME  SY-DATUM  SY-REPID
*         INTO LV_CONF_TEXT
*         SEPARATED BY '/'.
*
** Fill Header DATA _ kn21n
*  CLEAR WA_DOC_HEADER.
*  WA_DOC_HEADER-CO_AREA           = P_KOKRS.
*  WA_DOC_HEADER-DOCDATE           = IT_REPORT-BLDAT.
*  WA_DOC_HEADER-POSTGDATE         = IT_REPORT-BUDAT.
*  WA_DOC_HEADER-VERSION           = P_VERSN.
*  WA_DOC_HEADER-VARIANT           = 'SAP10'.
*  WA_DOC_HEADER-DOC_HDR_TX        = LV_CONF_TEXT.
*  WA_DOC_HEADER-USERNAME          = SY-UNAME.
*
*
** Fill Object List
*  CLEAR : IT_DOC_ITEMS, IT_DOC_ITEMS[].
*
*  LOOP AT IT_REPORT.
*
*    IT_DOC_ITEMS-SEND_CCTR = IT_REPORT-KOSTL.
*    IT_DOC_ITEMS-ACTTYPE = IT_REPORT-LSTAR.
*    IT_DOC_ITEMS-ACTVTY_QTY = IT_REPORT-MEGXXX.
*    IT_DOC_ITEMS-PRICE = IT_REPORT-balamt.
*    IT_DOC_ITEMS-CURRENCY = 'USD'.
*    IT_DOC_ITEMS-REC_ORDER = IT_REPORT-EAUFNR.
*    APPEND IT_DOC_ITEMS.
*
*  ENDLOOP.
*
** Call BAPI FM
*  PERFORM CALL_POST_FM.   " KB21N-  activity allocation
*
*
** Commit
*  IF P_TRUN = 'X'.
*    READ TABLE IT_RETURN  INDEX 1.
*    MESSAGE S000(ZMCO) WITH IT_RETURN-MESSAGE.
*  ELSE.
*    COMMIT WORK.
*    READ TABLE IT_RETURN  INDEX 1.
*    MESSAGE S000(ZMCO) WITH IT_RETURN-MESSAGE.
**    MESSAGE S009(ZMCO) WITH P_UCOMM.
*  ENDIF.
*
ENDFORM.                    " POST_STD_CCTR_AT_CE
*&---------------------------------------------------------------------*
*&      Form  CHK_INPUT_VALUE
*&---------------------------------------------------------------------*
*       check input value
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM chk_input_value.

* Check Input Value (Period)
  IF p_perid < 0 OR p_perid > 12.
    MESSAGE e007(zmco) WITH p_perid .
  ENDIF.

* Check Cost Center/Cost Center Group
  IF     s_kostl[] IS INITIAL
     AND p_ncoal   IS INITIAL .
    MESSAGE e016(zmco).
  ELSEIF
         NOT s_kostl[] IS INITIAL
     AND NOT p_ncoal   IS INITIAL .
    MESSAGE e017(zmco).
  ENDIF.

* Check TEST-RUN  Flag
*  IF P_TRUN NA 'X '.
*    MESSAGE E008(ZMCO).
*  ENDIF.


ENDFORM.                    " CHK_INPUT_VALUE
*&---------------------------------------------------------------------*
*&      Form  PRE_REPORT_DATA
*&---------------------------------------------------------------------*
*        The preparation for reporting
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pre_report_data.
*** <  Posting , hard coding > **
** doc date / posting date   =  ' YEAR/MONTH/28 '
** Activity type =  ' MAN_HR '
** Total Qty     =  ' 1 '
*
*
*  CLEAR : IT_REPORT, IT_REPORT[].
*  CLEAR IT_TMP_COSP.
*  LOOP AT IT_TMP_COSP.
*    MOVE-CORRESPONDING IT_TMP_COSP TO IT_REPORT.
*    IT_REPORT-LSTAR = GV_ACTYPE.
*    IT_REPORT-BLDAT = GV_POSTDATE.
*    IT_REPORT-BUDAT = GV_POSTDATE.
*
*    CLEAR IT_OBJ_CCTR_AT_2.
*    READ TABLE IT_OBJ_CCTR_AT_2 WITH KEY KOSTL = IT_REPORT-KOSTL.
*    IF SY-SUBRC = 0.
*      CLEAR IT_COSS.
*      READ TABLE IT_COSS WITH KEY PAROB = IT_OBJ_CCTR_AT_2-OBJNR
*                               USPOB = IT_OBJ_CCTR_AT_2-OBJNR.
*      IF SY-SUBRC = 0.
*        IT_REPORT-EAUFNR = IT_COSS-OBJNR+2(12).
**      ELSE.
**        IT_REPORT-EAUFNR = ' '.
*      ENDIF.
*    ENDIF.
*
*    IT_REPORT-MEGXXX = GV_TOTQTY.
*    APPEND IT_REPORT.
*  ENDLOOP.



  CLEAR : it_report, it_report[].
  CLEAR it_tmp_cosp.
  LOOP AT it_tmp_cosp.
    MOVE-CORRESPONDING it_tmp_cosp TO it_report.
    it_report-bldat = gv_postdate.
    it_report-budat = gv_postdate.
    APPEND it_report.
  ENDLOOP.

ENDFORM.                    " PRE_REPORT_DATA
*&---------------------------------------------------------------------*
*&      Form  CALL_POST_FM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_post_fm.

  CALL FUNCTION 'BAPI_ACC_ACTIVITY_ALLOC_POST'
    EXPORTING
      doc_header            = wa_doc_header
*   IGNORE_WARNINGS       = ' '
* IMPORTING
*   DOC_NO                =
    TABLES
      doc_items             = it_doc_items
      return                = it_return.
*   CRITERIA              =

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
*&      Form  READ_ORDER
*&---------------------------------------------------------------------*
*       Read Order Info.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_order.

*/////start  Modi.  2004.05.25
*  DATA $OBJNR LIKE WA_OBJ-OBJNR.
*  CLEAR $OBJNR.
*  CONCATENATE '%' 'OR' '%' INTO $OBJNR.
*
*  CLEAR : IT_COSL, IT_COSL[].
*  CLEAR COSL.
*  SELECT *
*         INTO CORRESPONDING FIELDS OF TABLE IT_COSL
*         FROM COSL
*        WHERE LEDNR = '00'
*          AND OBJNR LIKE $OBJNR
*          AND GJAHR = P_GJAHR
*          AND WRTTP = '04'             " Actual
*          AND VERSN = P_VERSN.
*  CLEAR : IT_COSL.
*
** Local Data definition
*  FIELD-SYMBOLS: <FS1> TYPE ANY.
*  DATA : LV_LST_NAM(30).
*  DATA : LV_CNT  LIKE  COSP-PERBL.
*
*  CLEAR   IT_COSL.
*  CLEAR : IT_TMP_COSL, IT_TMP_COSL[].
*
*  LOOP AT IT_COSL.
*    CLEAR LV_CNT.
*    LV_CNT = P_PERID .
** Key Part
*    IT_TMP_COSL-GJAHR = P_GJAHR.
*    IT_TMP_COSL-OBJNR = IT_COSL-OBJNR.
*
** Period
*    CLEAR IT_TMP_COSL-PERID.
*    IT_TMP_COSL-PERID = LV_CNT.
** Value Transferring
*    CLEAR LV_LST_NAM.
*    CONCATENATE 'IT_COSL-'  'LST'  LV_CNT
*           INTO LV_LST_NAM.
*    ASSIGN (LV_LST_NAM) TO <FS1>.
*    CLEAR IT_TMP_COSL-balamt.
*    IT_TMP_COSL-balamt = <FS1>.
*
** Collect
*    COLLECT IT_TMP_COSL.
*    CLEAR IT_TMP_COSL.
*    CLEAR IT_COSL.
*  ENDLOOP.
*  CLEAR IT_TMP_COSL.
*
*  DELETE IT_TMP_COSL WHERE balamt < '10'.
*
*
*  DATA $PAROB LIKE COSS-PAROB.
*  CLEAR $PAROB.
*  CONCATENATE '%' 'MAN_HR' '%' INTO $PAROB.
*
*  CLEAR : IT_COSS, IT_COSS[].
*  CLEAR COSS.
*  SELECT *
*         INTO CORRESPONDING FIELDS OF TABLE IT_COSS
*         FROM COSS
*          FOR ALL ENTRIES IN IT_TMP_COSL
*        WHERE LEDNR = '00'
*          AND OBJNR = IT_TMP_COSL-OBJNR
*          AND GJAHR = P_GJAHR
*          AND WRTTP = '04'
*          AND VERSN = P_VERSN
*          AND PAROB LIKE $PAROB
*          AND USPOB LIKE $PAROB.
*  CLEAR : IT_COSS.
*
*/////end  Modi.  2004.05.25
ENDFORM.                    " READ_ORDER
*&---------------------------------------------------------------------*
*&      Form  RRE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM rre_data.
**** Hard coding setting
* activity type
  gv_actype = 'MAN_HR'.
* total qty
  gv_totqty = '1'.

* posting date
  CONCATENATE p_gjahr p_perid+1(2) '25' INTO gv_postdate.

ENDFORM.                    " RRE_DATA
*&---------------------------------------------------------------------*
*&      Form  POST_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_UCOMM  text
*----------------------------------------------------------------------*
FORM post_bdc USING    p_ucomm.
  DATA: l_cnt TYPE i,
        l_rc  LIKE sy-subrc.
  DATA: l_budat  LIKE sy-datum.

  SET PF-STATUS '0001'.

  CONCATENATE  p_gjahr p_perid+1(2) '01' INTO l_budat.

  CALL FUNCTION 'MM_ARRANG_GET_END_OF_MONTH'
       EXPORTING
            i_datum = l_budat
       IMPORTING
            e_datum = l_budat.


*  sort it_report by balamt descending.

  LOOP AT it_report WHERE balamt <> 0.
    AT FIRST.
      PERFORM fill_bapi_header  USING l_budat.
    ENDAT.

    PERFORM fill_bapi_line USING l_cnt.

    AT LAST.
      PERFORM call_bapi_post USING l_rc.
    ENDAT.
  ENDLOOP.

  WRITE:/ '*** Posting Log ***'.

  LOOP AT return.
    WRITE:/ return-id, return-number, return-message(80).
  ENDLOOP.

ENDFORM.                    " POST_BDC
*&---------------------------------------------------------------------*
*&      Form  fill_bapi_header
*&---------------------------------------------------------------------*
FORM fill_bapi_header  USING p_budat.
  REFRESH : accountgl, currencyamount, return, extension1.
  CLEAR : documentheader.

  documentheader-username       = sy-uname.
  documentheader-comp_code      = t001-bukrs.
  documentheader-doc_type       = p_blart.
  documentheader-fisc_year      = p_budat(4).
  documentheader-fis_period     = p_budat+4(2).
  documentheader-doc_date       = p_budat.
  documentheader-pstng_date     = p_budat.
  documentheader-trans_date     = p_budat.

*lineitem currency
  currencyamount-currency    = t001-waers.

ENDFORM.                    " fill_bapi_header
*&---------------------------------------------------------------------*
*&      Form  call_bapi_post
*&---------------------------------------------------------------------*
FORM call_bapi_post USING p_rc.
  CLEAR p_rc.

  IF p_run = space.
    CALL FUNCTION 'BAPI_ACC_GL_POSTING_CHECK'
         EXPORTING
              documentheader = documentheader
         TABLES
              accountgl      = accountgl
              currencyamount = currencyamount
              return         = return
              extension1     = extension1.
  ELSE.
    CALL FUNCTION 'BAPI_ACC_GL_POSTING_POST'
         EXPORTING
              documentheader = documentheader
         IMPORTING
              obj_type       = obj_type
              obj_key        = obj_key
              obj_sys        = obj_sys
         TABLES
              accountgl      = accountgl
              currencyamount = currencyamount
              return         = return
              extension1     = extension1.
  ENDIF.

  READ TABLE return WITH KEY type = 'E'.
  IF sy-subrc = 0.
    p_rc = 1.
  ENDIF.

  CHECK p_run = 'X'.
  IF p_rc <> 0.
    ROLLBACK WORK.
  ELSE.
    COMMIT WORK.
  ENDIF.

ENDFORM.                    " call_bapi_post
*&---------------------------------------------------------------------*
*&      Form  fill_bapi_line
*&---------------------------------------------------------------------*
FORM fill_bapi_line USING    p_cnt.
  DATA: l_amt LIKE bapiaccr08-amt_doccur.

  accountgl-item_text  = ''.
  accountgl-alloc_nmbr = it_report-kostl.

  l_amt = - it_report-balamt.

* 1st lineitem
  p_cnt = p_cnt + 1.
  MOVE: p_cnt        TO accountgl-itemno_acc,
        p_cnt        TO currencyamount-itemno_acc,
        p_fo         TO accountgl-gl_account,
        it_report-kostl TO accountgl-costcenter,
        l_amt        TO currencyamount-amt_doccur.
  APPEND: accountgl, currencyamount.

  CLEAR accountgl-costcenter.

* 2nd lineitem
  p_cnt = p_cnt + 1.
  l_amt = - l_amt.
  MOVE: p_cnt        TO accountgl-itemno_acc,
        p_cnt        TO currencyamount-itemno_acc,
        p_cogs       TO accountgl-gl_account,
        l_amt        TO currencyamount-amt_doccur.
  APPEND: accountgl, currencyamount.

ENDFORM.                    " fill_bapi_line
*&---------------------------------------------------------------------*
*&      Form  read_cegrp
*&---------------------------------------------------------------------*
FORM read_cegrp.
  DATA: t_setlist LIKE setlist OCCURS 0 WITH HEADER LINE.
  DATA: t_sethier LIKE sethier OCCURS 0 WITH HEADER LINE.
  DATA: t_setvalues LIKE setvalues OCCURS 0 WITH HEADER LINE.
  DATA: l_indh LIKE sy-tabix,
        l_indv LIKE sy-tabix.


  describe table s_kstar lines l_indh.
  check l_indh = 0.

  CALL FUNCTION 'G_SET_LIST_SELECT'
       EXPORTING
            setclass      = '0102'
            shortname     = c_nstar
            kokrs         = p_kokrs
            ktopl         = t001-ktopl
       TABLES
            matching_sets = t_setlist.
  IF t_setlist[] IS INITIAL.
    MESSAGE e002(sy) WITH 'Cost element group does not exist'.
    EXIT.
  ELSE.
    READ TABLE t_setlist INDEX 1.
  ENDIF.

  CALL FUNCTION 'G_SET_TREE_IMPORT'
       EXPORTING
            setid                     = t_setlist-setname
       TABLES
            set_hierarchy             = t_sethier
            set_values                = t_setvalues
       EXCEPTIONS
            set_not_found             = 1
            illegal_field_replacement = 2
            illegal_table_replacement = 3
            OTHERS                    = 4.

  IF sy-subrc <> 0.
    MESSAGE e002(sy) WITH 'Cost element group does not exist'.
    EXIT.
  ENDIF.
* TRANSFER THE VALUE TO CE RANGE.
  refresh s_kstar. clear s_kstar.
  s_KSTAR-sign = 'I'.
  s_KSTAR-option = 'BT'.
  LOOP AT t_setvalues.
    s_KSTAR-low  = t_setvalues-from.
    s_KSTAR-high = t_setvalues-to.
    APPEND s_KSTAR.
  ENDLOOP.

ENDFORM.                    " read_cegrp
