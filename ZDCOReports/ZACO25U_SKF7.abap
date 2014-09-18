************************************************************************
* Program Name      : ZACO25U_SKF7
* Author            : Eun Hwa , Jung
* Creation Date     : 2003.11.03.
* Specifications By : Dong-Hwan Kim
* Pattern           : Report 1-1
* Development Request No : UD1K903551
* Addl Documentation:
* Description       : Create Man hour for Process Driver


* the BDC structures for BATCH INPUT processing
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 10/19/2006  Manju       UD1K922643   Display detailed log
*
*
************************************************************************


REPORT zaco25u_skf_rd2 MESSAGE-ID zmco.

** type-pools
TYPE-POOLS: slis.
INCLUDE:  <icon>.

** Table
TABLES : csks ,  cosl.

** For OBJECT KEY
DATA : BEGIN OF wa_obj ,
        objnr  LIKE  coss-objnr,
        kostl  LIKE  csks-kostl,
        lstar  LIKE  csla-lstar,
       END OF wa_obj.
DATA : it_obj_cctr_at   LIKE STANDARD TABLE OF wa_obj
                        WITH HEADER LINE .

** Internal Table
DATA : BEGIN OF it_cosl OCCURS 500.
        INCLUDE STRUCTURE zsco_cosl_key01.
        INCLUDE STRUCTURE zsco_cosl_lst01.
DATA : END OF   it_cosl.

DATA: BEGIN OF it_tmp_cosl OCCURS 0,
        gjahr  LIKE coss-gjahr,
        perid  LIKE coejl-perbl,
        objnr  LIKE coss-objnr,
        kostl  LIKE csks-kostl,
        lstar  LIKE csla-lstar,
        curqty LIKE cosl-lst001,
        unit   LIKE cosl-meinh,
      END OF it_tmp_cosl.

DATA : BEGIN OF it_ztco_cc_pro_qty OCCURS 0.
        INCLUDE STRUCTURE ztco_cc_pro_qty.
DATA : END OF   it_ztco_cc_pro_qty.

DATA: BEGIN OF it_sum OCCURS 0,
        kostl  TYPE ztco_cc_pro_qty-kostl,
        megxxx TYPE ztco_cc_pro_qty-megxxx,
      END OF it_sum.

* for reporting
DATA : BEGIN OF it_report OCCURS 0,
        gjahr   LIKE cosl-gjahr,
        perid   LIKE coejl-perbl,
        kostl   LIKE csks-kostl,
        lstar   LIKE csla-lstar,

        bldat   LIKE coheader-bldat,         " doc date
        budat   LIKE coheader-budat,         " Posting date
        eprznr  LIKE rk40c_kbxxn-eprznr,     " Receiver business process
        stagr   LIKE rk40c_kbxxn-stagr,       " SKF
        curqty  LIKE cosl-lst001,             " Total qty
        unit    LIKE cosl-meinh,

        tqty    LIKE cosl-lst001,
        qty     LIKE cosl-lst001,

        senbusproc LIKE bapiaaitm-senbusproc, "Sender business process
        recbusproc LIKE bapiaaitm-recbusproc, "Receiver business process
        chkbox  TYPE c,                                     "UD1K922643
        icon  TYPE icon_d,                                  "UD1K922643
      END OF it_report.

* For DD data
DATA : gv_ci_tabname     TYPE ddobjname .
DATA : it_et_fieldlist   LIKE TABLE OF rfvicp_ddic_tabl_fieldname
                         WITH HEADER LINE.

* for posting
DATA : BEGIN OF it_post OCCURS 0,
        gjahr   LIKE  cosl-gjahr,
        r_kostl LIKE  cssl-kostl,
        r_lstar LIKE  cssl-lstar,
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

*** For BAPI
DATA : it_costcenterlist LIKE STANDARD TABLE OF bapi0012_cclist
                         WITH HEADER LINE.
DATA : it_return         LIKE STANDARD TABLE OF bapiret2
                         WITH HEADER LINE.
DATA : it2_return         LIKE STANDARD TABLE OF bapiret2
                         WITH HEADER LINE.
DATA : it3_return         LIKE STANDARD TABLE OF bapiret2
                         WITH HEADER LINE.

*  (kb31np)
DATA : wa_doc_header LIKE bapidochdrp .
DATA : it_doc_items  LIKE STANDARD TABLE OF bapiskfitm
                     WITH HEADER LINE.
*  (kb21np)
DATA : wa2_doc_header LIKE bapidochdrp .
DATA : it2_doc_items  LIKE STANDARD TABLE OF bapiaaitm
                     WITH HEADER LINE.

DATA:  BEGIN OF it_belnr OCCURS 0,
         belnr  LIKE cobk-belnr,
       END OF   it_belnr.

DATA  gv_p_versn(3).

*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.
PARAMETERS :
             p_kokrs LIKE csks-kokrs MEMORY ID cac OBLIGATORY,
             p_gjahr LIKE cobk-gjahr MEMORY ID gjr OBLIGATORY,
             p_perid LIKE cosp-perbl MEMORY ID bpe OBLIGATORY,
             p_versn LIKE cobk-versn MEMORY ID kvt OBLIGATORY,
             p_trun(1).
SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK bl4 WITH FRAME TITLE text-003.
PARAMETERS : p_lstar LIKE csla-lstar            DEFAULT 'MAN_HR'
                                                         OBLIGATORY.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN END OF BLOCK bl4.

SELECTION-SCREEN BEGIN OF BLOCK bl3 WITH FRAME TITLE text-003.
SELECT-OPTIONS : s_kostl  FOR csks-kostl DEFAULT 'MXTX11'.
PARAMETERS:      p_ncoal LIKE grpdynp-name_coall.
*                                     DEFAULT 'SEMIDIRECT'.
SELECTION-SCREEN END OF BLOCK bl3.
SELECTION-SCREEN END OF BLOCK bl1.


*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
INITIALIZATION.
* Set Global ALV Parameter
  gv_repid = sy-repid.
*  KB21NP- BUSINESS PROCESS ALLOCATION - Posting, version.
  gv_p_versn = '500'.


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
* Read OBJ Key Combination
  PERFORM set_obj_key.
* Read Dynamic Fields Name
  PERFORM read_field_name_from_dd_cosl.
* Read Sender_Quantity Information from COSL
  PERFORM read_cosl_qty.
* Read Procee.Qty Information from ZTCO_CC_PRO_QTY
  PERFORM read_ztco_cc_pro_qty.
* Calculate qty
  PERFORM cal_qty.
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
*&      Form  SET_OBJ_KEY
*&---------------------------------------------------------------------*
*       Read OBJ Key Combination
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_obj_key.

  CLEAR : it_obj_cctr_at, it_obj_cctr_at[].

  LOOP AT it_costcenterlist.
    CALL FUNCTION 'K_LSTAR_OBJECT_KEY_GET'
         EXPORTING
              kokrs = p_kokrs
              kostl = it_costcenterlist-costcenter
              lstar = p_lstar
         IMPORTING
              objnr = it_obj_cctr_at-objnr.
    it_obj_cctr_at-kostl = it_costcenterlist-costcenter.
    it_obj_cctr_at-lstar = p_lstar.
    APPEND it_obj_cctr_at.
    CLEAR  it_obj_cctr_at.
    CLEAR  it_costcenterlist.
  ENDLOOP.
  CLEAR : it_obj_cctr_at.


ENDFORM.                    " SET_OBJ_KEY
*&---------------------------------------------------------------------*
*&      Form  READ_COSL_QTY
*&---------------------------------------------------------------------*
*       Read Quantity data from COSL
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
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
          AND wrttp = '04'             " Actual
          AND versn = p_versn.
  CLEAR : it_cosl.


* Local Data definition
  FIELD-SYMBOLS: <fs1> TYPE ANY.
  DATA : lv_lst_nam(30).

  CLEAR   it_cosl.
  CLEAR : it_tmp_cosl, it_tmp_cosl[].

  LOOP AT it_cosl.
* Key Part
    it_tmp_cosl-gjahr = p_gjahr.
    it_tmp_cosl-perid = p_perid.

    CLEAR it_obj_cctr_at.
    READ TABLE    it_obj_cctr_at
         WITH KEY objnr = it_cosl-objnr.
    it_tmp_cosl-kostl = it_obj_cctr_at-kostl.
    it_tmp_cosl-lstar = it_obj_cctr_at-lstar.
    it_tmp_cosl-objnr = it_obj_cctr_at-objnr.

    it_tmp_cosl-unit = it_cosl-meinh.     " unit

* Value Transferring
    CLEAR lv_lst_nam.
    CONCATENATE 'IT_COSL-'  'LST'  p_perid
           INTO lv_lst_nam.
    ASSIGN (lv_lst_nam) TO <fs1>.
    CLEAR it_tmp_cosl-curqty.
    it_tmp_cosl-curqty = <fs1>.

*   Unit Conversion
    IF it_tmp_cosl-unit <> 'STD'.
      PERFORM unit_conv USING it_tmp_cosl-unit
                              it_tmp_cosl-curqty.
    ENDIF.

* Collect
    COLLECT it_tmp_cosl.

    CLEAR it_tmp_cosl.
    CLEAR it_cosl.
  ENDLOOP.
  CLEAR it_tmp_cosl.


ENDFORM.                    " READ_COSL_QTY
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
  SORT it_report BY gjahr perid kostl lstar.
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
*  PERFORM BUILD_FIELDCAT USING
*    'IT_REPORT' 'GJAHR'  'X'            SPACE    SPACE
*    SPACE        '4'      'Year'           SPACE    SPACE    SPACE.
*
*  PERFORM BUILD_FIELDCAT USING
*    'IT_REPORT' 'PERID'  'X'            SPACE    SPACE
*    SPACE        '6'      'Period'           SPACE    SPACE    SPACE.


  PERFORM build_fieldcat USING
    'IT_REPORT' 'KOSTL'  'X'            space    space
    space        '10'      'Sender CC'   space    space    space.

  PERFORM build_fieldcat USING
    'IT_REPORT' 'LSTAR'  'X'            space    space
    space        '6'      'Sen.AT'           space    space    space.

  PERFORM build_fieldcat USING
   'IT_REPORT' 'TQTY'  'X'            space    space
   space '15'      'Total Qty'    space    space space.

  PERFORM build_fieldcat USING
  'IT_REPORT' 'EPRZNR'  'X'            space    space
  space '12'      'Biz.Process'    space    space space.

  PERFORM build_fieldcat USING
  'IT_REPORT' 'QTY'  'X'            space    space
  space '15'      'Quantity'    space    space space.


** Value

  PERFORM build_fieldcat USING
  'IT_REPORT' 'BUDAT'  space           space    space
  space '10'      'Post Date'    space    space space.

  PERFORM build_fieldcat USING
  'IT_REPORT' 'STAGR'  space            space    space
  space '6'      'SKF'    space    space space.

  PERFORM build_fieldcat USING
  'IT_REPORT' 'RECBUSPROC'  space            space    space
  space '12'      'Rec. Process'    space    space space.

  PERFORM build_fieldcat USING
  'IT_REPORT' 'CURQTY'  space            space    space
  space '15'      'Total Qty'    space    space space.

  PERFORM build_fieldcat USING
    'IT_REPORT' 'UNIT'  space            space    space
    space   '4'      'Unit'      'UNIT'   space     space.

  PERFORM build_fieldcat USING
    'IT_REPORT' 'ICON'  space            space    space
    space   '4'      'STS'      'STS'   space     space .




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

  DATA : cs_layo TYPE slis_layout_alv.

  cs_layo-box_fieldname          = 'CHKBOX'.


  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
     EXPORTING
*     I_INTERFACE_CHECK              = ' '
*     I_BYPASSING_BUFFER             =
*     I_BUFFER_ACTIVE                = ' '
       i_callback_program             = gv_repid
       i_callback_pf_status_set       = gv_status
       i_callback_user_command        = gv_user_command
*     I_STRUCTURE_NAME               =
      is_layout                      =  cs_layo
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

  IF sy-ucomm EQ '&F03'.
    sy-lsind = 0.
*   LEAVE PROGRAM.
    LEAVE TO SCREEN 0.
    EXIT.
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
      PERFORM post_std  USING ucomm.
    WHEN 'LOG'.
      PERFORM dis_log_for_post_doc.

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
            , p_gjahr, '/', p_perid , '/', p_versn.
  WRITE : / 'Business Process PostVersion:'
            , gv_p_versn.
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
*        The Preparation for posting
*----------------------------------------------------------------------*
*      -->P_UCOMM  text
*----------------------------------------------------------------------*
FORM post_std USING    p_ucomm.

* delete value '0'
  DELETE it_report WHERE curqty EQ '0'.

  DATA  : line  TYPE i.
  CLEAR : line.
  DESCRIBE TABLE it_report   LINES line.

  IF line = 0.
    MESSAGE e000(zmco) WITH
    ' Enter Value not equal to 0 '.
  ENDIF.

* Init. Message TAB
  CLEAR : it_return, it_return[].
  CLEAR : it2_return, it2_return[].

  DATA : lv_conf_text(50).
* TEXT
  CLEAR lv_conf_text.
  CONCATENATE sy-uname  sy-datum  sy-repid
         INTO lv_conf_text
         SEPARATED BY '/'.

* Fill Header DATA _ kn31np
  CLEAR wa_doc_header.
  wa_doc_header-co_area           = p_kokrs.
  wa_doc_header-docdate           = it_report-bldat.
  wa_doc_header-postgdate         = it_report-budat.
  wa_doc_header-version           = p_versn.
  wa_doc_header-variant           = 'SAP06'.
  wa_doc_header-doc_hdr_tx        = lv_conf_text.
  wa_doc_header-username          = sy-uname.

* Fill Header DATA _ kb21np
  CLEAR wa2_doc_header.
  wa2_doc_header-co_area           = p_kokrs.
  wa2_doc_header-docdate           = it_report-bldat.
  wa2_doc_header-postgdate         = it_report-budat.
  wa2_doc_header-version           = gv_p_versn.  " '500'.   " P_VERSN.
  wa2_doc_header-variant           = 'SAP08'.
  wa2_doc_header-doc_hdr_tx        = lv_conf_text.
  wa2_doc_header-username          = sy-uname.

* Fill Object List
  CLEAR : it_doc_items, it_doc_items[].
  CLEAR : it2_doc_items, it2_doc_items[].

  LOOP AT it_report WHERE chkbox EQ 'X'.                    "UD1K92264

    it_doc_items-statkeyfig = 'AS008'.   "IT_REPORT-STAGR.
    it_doc_items-stat_qty   = it_report-curqty.
    it_doc_items-recbusproc = it_report-eprznr.
    APPEND it_doc_items.

    it2_doc_items-senbusproc = it_report-eprznr.
    it2_doc_items-recbusproc = 'DUMMY-PROC'.
    it2_doc_items-actvty_qty = it_report-curqty.
    it2_doc_items-activityun = 'STD'.
    APPEND it2_doc_items.

  ENDLOOP.

* Call BAPI FM
  PERFORM call_post_fm.     " KB31NP- SKF
  PERFORM call_post_fm_2.   " KB21NP- BUSINESS PROCESS ALLOCATION


* Commit
  IF p_trun = 'X'.
*   READ TABLE IT_RETURN  INDEX 1.
*   MESSAGE S000(ZMCO) WITH IT_RETURN-MESSAGE.
  ELSE.
    COMMIT WORK.
*    READ TABLE IT_RETURN  INDEX 1.
*    MESSAGE S000(ZMCO) WITH IT_RETURN-MESSAGE.
    MESSAGE s009(zmco) WITH p_ucomm.

*       Consolidation management for the SKF
    CALL FUNCTION 'Z_FCO_MANAGEMENT_SKF'
      EXPORTING
        im_pgmno         =   sy-tcode
        im_kokrs         =   p_kokrs
        im_gjahr         =   p_gjahr
        im_perbl         =   p_perid
*           IM_PERBL_T       =
        im_versn         =   p_versn
       im_kostl_f        =    s_kostl-low
       im_kostl_t        =    s_kostl-high
       im_gname          =    p_ncoal.
*           IM_PRZNR_F       =
*           IM_PRZNR_T       =
*         IMPORTING
*           SUBRC            =


  ENDIF.

ENDFORM.                    " POST_STD_CCTR_AT_CE
*&---------------------------------------------------------------------*
*&      Form  CALL_POST_FM
*&---------------------------------------------------------------------*
*       Call bapi function
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_post_fm.

  CALL FUNCTION 'BAPI_ACC_STAT_KEY_FIG_POST'
    EXPORTING
      doc_header            = wa_doc_header
*   IGNORE_WARNINGS       = ' '
* IMPORTING
*   DOC_NO                =
    TABLES
      doc_items             = it_doc_items
      return                = it_return.


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
  IF p_trun NA 'X '.
    MESSAGE e008(zmco).
  ENDIF.

* Check Activity type
  IF p_lstar NE 'MAN_HR'.
    MESSAGE e018(zmco) WITH p_lstar.
  ENDIF.


ENDFORM.                    " CHK_INPUT_VALUE
*&---------------------------------------------------------------------*
*&      Form  READ_FIELD_NAME_FROM_DD_COSL
*&---------------------------------------------------------------------*
*       Read Technical FieldName for COSL
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
*&      Form  PRE_REPORT_DATA
*&---------------------------------------------------------------------*
*        The preparation for reporting
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pre_report_data.
** <  Posting , hard coding > **
* doc date / posting date   =  ' YEAR/MONTH/28 '
* SKF                       =  ' AS008'
* receiver.business process =  ' DUMMY-PROC'

  SORT it_tmp_cosl BY gjahr perid objnr kostl.
  SORT it_ztco_cc_pro_qty BY gjahr kostl prznr.
  SORT it_sum BY kostl.

  CLEAR : it_report, it_report[].

  CLEAR it_ztco_cc_pro_qty.
  LOOP AT it_ztco_cc_pro_qty.
    CLEAR it_tmp_cosl.
    READ TABLE it_tmp_cosl WITH KEY kostl = it_ztco_cc_pro_qty-kostl.
    MOVE-CORRESPONDING it_tmp_cosl TO it_report.
    CONCATENATE it_report-gjahr it_report-perid+1(2) '28'
                INTO it_report-bldat.
    CONCATENATE it_report-gjahr it_report-perid+1(2) '28'
                INTO it_report-budat.
    it_report-eprznr = it_ztco_cc_pro_qty-prznr.
    it_report-stagr = 'AS008'.
    it_report-senbusproc = it_ztco_cc_pro_qty-prznr.
    it_report-recbusproc = 'DUMMY-PROC'.

    it_report-tqty = it_tmp_cosl-curqty.   " Total qty

    CLEAR it_sum.
    READ TABLE it_sum WITH KEY kostl = it_ztco_cc_pro_qty-kostl.
    it_report-qty = it_ztco_cc_pro_qty-megxxx.     " Quantity
*    calculate quantity
    it_report-curqty = it_report-curqty *
                      ( it_ztco_cc_pro_qty-megxxx / it_sum-megxxx ).

    APPEND it_report.
  ENDLOOP.
  CLEAR it_report.


ENDFORM.                    " PRE_REPORT_DATA
*&---------------------------------------------------------------------*
*&      Form  CALL_POST_FM_2
*&---------------------------------------------------------------------*
*       call BAPI function
*----------------------------------------------------------------------*
FORM call_post_fm_2.

  DATA: it_xmsg   LIKE TABLE OF ztismessage WITH HEADER LINE,
        flag .


  CALL FUNCTION 'BAPI_ACC_ACTIVITY_ALLOC_POST'
    EXPORTING
      doc_header            = wa2_doc_header
*   IGNORE_WARNINGS       = ' '
* IMPORTING
*   DOC_NO                =
    TABLES
      doc_items             = it2_doc_items
      return                = it2_return.
*   CRITERIA              =


  LOOP AT it2_return WHERE type EQ 'S'.
    it_report-icon = icon_green_light.
    MODIFY it_report TRANSPORTING icon WHERE chkbox = 'X'.
  ENDLOOP.


* Check error
  CLEAR  it2_return.
  LOOP AT it2_return  WHERE type CA 'WAE'.
*  Begin of changes - UD1K922643
*    MESSAGE ID     IT2_RETURN-ID
*            TYPE   IT2_RETURN-TYPE
*            NUMBER IT2_RETURN-NUMBER
*            WITH   IT2_RETURN-MESSAGE_V1
*                   IT2_RETURN-MESSAGE_V2
*                   IT2_RETURN-MESSAGE_V3
*                   IT2_RETURN-MESSAGE_V4.
*    CLEAR IT2_RETURN.

    MOVE-CORRESPONDING it2_return TO it_xmsg.
    it_xmsg-msgid = it2_return-id.
    it_xmsg-msgty = it2_return-type.
    it_xmsg-msgno = it2_return-number.
    it_xmsg-msgv1 = it2_return-message_v1.
    it_xmsg-msgv2 = it2_return-message_v2.
    it_xmsg-msgv3 = it2_return-message_v3.
    it_xmsg-msgv4 = it2_return-message_v4.
    it_xmsg-msgtx = it2_return-message.
    APPEND it_xmsg.
    IF it_xmsg-msgty = 'E' OR
       it_xmsg-msgty = 'W' .
      flag = 'X'.
    ENDIF.

  ENDLOOP.

  IF flag = 'X' .
    CALL FUNCTION 'ZMM_IF_POPUP_TO_ERROR_MESSAGE'
     EXPORTING
*     XLOGNO            =
       xdocno_show       = 'X'
     TABLES
       xmsg              =  it_xmsg.
  ENDIF.


  LOOP AT it2_return  WHERE type CA 'AE'.
    MESSAGE ID     it2_return-id
            TYPE   it2_return-type
            NUMBER it2_return-number
            WITH   it2_return-message_v1
                   it2_return-message_v2
                   it2_return-message_v3
                   it2_return-message_v4.
    CLEAR it2_return.
  ENDLOOP.

  sy-lsind = 0.
  IF sy-ucomm EQ 'POST'.
    LOOP AT it_report WHERE NOT icon IS initial.
      it_report-chkbox = ''.
      MODIFY it_report TRANSPORTING chkbox WHERE NOT icon IS initial.
      EXIT.
    ENDLOOP.
    PERFORM call_alv_list.
  ENDIF.

*  END of changes - UD1K922643
ENDFORM.                    " CALL_POST_FM_2
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
*     UNIT_OUT_NOT_FOUND         = 9
*     OTHERS                     = 10.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  p_unit = 'STD'.

ENDFORM.                    " UNIT_CONV
*&---------------------------------------------------------------------*
*&      Form  DIS_LOG_FOR_POST_DOC
*&---------------------------------------------------------------------*
*        Display posted documents
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM dis_log_for_post_doc.

  CLEAR :it3_return[], it3_return.

  LOOP AT it_return WHERE type  = 'S'
                        AND id = 'BK'
                        AND number = '003'.
    MOVE-CORRESPONDING it_return TO it3_return.
    APPEND it3_return.
    CLEAR it3_return.
    CLEAR it_return.
  ENDLOOP.

  LOOP AT it2_return WHERE type  = 'S'
                        AND id = 'BK'
                        AND number = '003'.
    MOVE-CORRESPONDING it2_return TO it3_return.
    APPEND it3_return.
    CLEAR it3_return.
    CLEAR it2_return.
  ENDLOOP.


* Saving Document numbers which were posted successfully.
  CLEAR : it_belnr, it_belnr[].
  LOOP AT it3_return WHERE type  = 'S'
                       AND id = 'BK'
                       AND number = '003'.
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*         EXPORTING
*              INPUT  = IT3_RETURN-MESSAGE
*         IMPORTING
*              OUTPUT = IT_BELNR-BELNR.
    it_belnr-belnr = it3_return-message_v1.

    APPEND it_belnr.
    CLEAR  it_belnr.
    CLEAR it3_return.
  ENDLOOP.

* Check if at least one document was created.
  IF it_belnr[] IS INITIAL .
    MESSAGE e025.
  ENDIF.

* Read Informations from Document Header
  DATA : it_l_cobk LIKE STANDARD TABLE OF cobk
                   WITH HEADER LINE .
  CLEAR : it_l_cobk, it_l_cobk[].

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_l_cobk
           FROM cobk
           FOR ALL ENTRIES IN it_belnr
          WHERE kokrs = p_kokrs
            AND belnr = it_belnr-belnr.

  IF it_l_cobk[] IS INITIAL .
    MESSAGE e025.
  ENDIF.

  SORT it_l_cobk BY belnr.

  CALL FUNCTION 'STC1_POPUP_WITH_TABLE_CONTROL'
    EXPORTING
      header                  = 'CO Document Information'
      tabname                 = 'COBK'
      display_only            = 'X'
      no_button               = space
*     X_START                 = 5
*     Y_START                 = 5
*     X_END                   = 80
*     Y_END                   = 25
    TABLES
      table                   = it_l_cobk
    EXCEPTIONS
      no_more_tables          = 1
      too_many_fields         = 2
      nametab_not_valid       = 3
      handle_not_valid        = 4
      OTHERS                  = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.                    " DIS_LOG_FOR_POST_DOC
*&---------------------------------------------------------------------*
*&      Form  READ_ZTCO_CC_PRO_QTY
*&---------------------------------------------------------------------*
*     Read table 'ZTCO_CC_PRO_QTY'
*----------------------------------------------------------------------*
FORM read_ztco_cc_pro_qty.

  CLEAR : it_ztco_cc_pro_qty, it_ztco_cc_pro_qty[].

  CLEAR it_tmp_cosl.
  SELECT *
        INTO CORRESPONDING FIELDS OF TABLE it_ztco_cc_pro_qty
        FROM ztco_cc_pro_qty
        FOR ALL ENTRIES IN it_tmp_cosl
         WHERE gjahr = p_gjahr
           AND kostl = it_tmp_cosl-kostl.

  IF sy-subrc <> 0.
    MESSAGE s000 WITH 'Please maintain Mapping table:ZTCO_CC_PRO_QTY!'.
  ENDIF.

  CLEAR it_ztco_cc_pro_qty.

ENDFORM.                    " READ_ZTCO_CC_PRO_QTY
*&---------------------------------------------------------------------*
*&      Form  CAL_QTY
*&---------------------------------------------------------------------*
*       calculate qty
*----------------------------------------------------------------------*
FORM cal_qty.

  CLEAR : it_sum, it_sum[].

  CLEAR it_ztco_cc_pro_qty.
  LOOP AT it_ztco_cc_pro_qty.
    MOVE-CORRESPONDING it_ztco_cc_pro_qty TO it_sum.
    COLLECT it_sum.
    CLEAR it_sum.
    CLEAR it_ztco_cc_pro_qty.
  ENDLOOP.

ENDFORM.                    " CAL_QTY
