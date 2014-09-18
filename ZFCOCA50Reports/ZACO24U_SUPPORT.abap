************************************************************************
* Program Name      : ZACO24U_SUPPORT
* Author            : HS, Jung
* Creation Date     : 2006.10.21.
* Specifications By : ANDY CHOI
* Pattern           : Report 1-1
* Addl Documentation:
* Description       : Create M/H FOR Supporting/Supperted
************************************************************************

REPORT zaco24u_support MESSAGE-ID zmco.


INCLUDE zaco24u_support_top.

*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.
PARAMETERS :
             p_kokrs LIKE ztco_mha-kokrs MEMORY ID cac OBLIGATORY,
             p_gjahr LIKE ztco_mha-gjahr MEMORY ID gjr OBLIGATORY,
             p_perid LIKE ztco_mha-perid MEMORY ID bpe OBLIGATORY,
             p_versn LIKE cobk-versn MEMORY ID kvt OBLIGATORY
                          default '0'.
SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK blk2 WITH FRAME TITLE text-002.
PARAMETERS: p_ccgrp LIKE bapico_group-groupname.
SELECT-OPTIONS s_kostl FOR ztco_mha-kostl.
PARAMETERS: p_cegrp LIKE bapico_group-groupname DEFAULT 'H201_HR'.
SELECT-OPTIONS s_kstar FOR cosp-kstar.
SELECTION-SCREEN END OF BLOCK blk2.
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
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_ccgrp.
  PERFORM read_cegrp_group USING '0101'
                                 p_ccgrp.


*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.

* Read Cost Center Group - > Cost Center
  PERFORM read_ccgrp.

* Read Cost Element Group - > Cost Element
  PERFORM read_cegrp.

* Read Supporting/Supported Data
  PERFORM select_ztco_mha.

* Read cost elemet allocate amt
  PERFORM select_cosp.

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
  IF NOT p_ccgrp IS INITIAL.
    CALL FUNCTION 'BAPI_COSTCENTER_GETLIST1'
         EXPORTING
              controllingarea = p_kokrs
              date_from       = lv_datum
              costcentergroup = p_ccgrp
         TABLES
              costcenterlist  = it_costcenterlist
              return          = it_return.

* Message
    PERFORM dis_bapi_message.


    LOOP AT it_costcenterlist.
      r_kostl-sign    =  'I' .
      r_kostl-option  =  'EQ' .
      r_kostl-low     = it_costcenterlist-costcenter.
      APPEND r_kostl. CLEAR r_kostl.
    ENDLOOP.

  ENDIF.

  IF NOT s_kostl IS INITIAL.
    r_kostl[] = s_kostl[].
  ENDIF.



ENDFORM.                    " READ_CCGRP
*&---------------------------------------------------------------------*
*&      Form  READ_CEGRP
*&---------------------------------------------------------------------*
*       read cost element
*----------------------------------------------------------------------*
FORM read_cegrp.

  REFRESH: it_hnodes, it_hvalues.
  CLEAR: wa_hnodes,wa_hvalues,wa_return1.

  CALL FUNCTION 'BAPI_COSTELEMENTGRP_GETDETAIL'
       EXPORTING
            chartofaccounts = 'HNA1'
            groupname       = p_cegrp
       IMPORTING
            return          = wa_return1
       TABLES
            hierarchynodes  = it_hnodes
            hierarchyvalues = it_hvalues.
  IF wa_return1-type = 'E'.
    MESSAGE e000 WITH 'Not a Valid cost element group'.
    EXIT.
  ENDIF.

  w_idx = 1.
  LOOP AT it_hnodes INTO wa_hnodes.
    IF wa_hnodes-valcount NE 0.
      w_cnt1 = wa_hnodes-valcount.
    ELSE.
      CONTINUE.
    ENDIF.
    LOOP AT it_hvalues INTO wa_hvalues FROM w_idx.
      w_cnt2 = w_cnt2 + 1.
      IF w_cnt2 > w_cnt1.
        w_idx  = sy-tabix.
        CLEAR w_cnt2.
        EXIT.
      ENDIF.
*      IF wa_hnodes-groupname = p_cegrp.
      wa_group_detail-gname = wa_hnodes-groupname.
      wa_group_detail-valfrom = wa_hvalues-valfrom.
      wa_group_detail-valto = wa_hvalues-valto.
      APPEND wa_group_detail TO it_ceg_detail.
*      ENDIF.
    ENDLOOP.
  ENDLOOP.
  CLEAR : w_idx,wa_hnodes, wa_hvalues.

  LOOP AT it_ceg_detail INTO wa_group_detail.
    r_kstar-sign    = 'I'.
    r_kstar-option  = 'BT'.
    r_kstar-low     = wa_group_detail-valfrom.
    r_kstar-high    = wa_group_detail-valto.
    APPEND r_kstar. CLEAR r_kstar.
  ENDLOOP.


  IF NOT s_kstar[] IS INITIAL.
    r_kstar[] = s_kstar[].
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
  SORT it_report BY gjahr perid kostl srkostl kstar.
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


  PERFORM build_fieldcat USING
    'IT_REPORT' 'KOSTL'   'X'        space space space '10'
                          'Sender'   space space space.

  PERFORM build_fieldcat USING
    'IT_REPORT' 'SRKOSTL' 'X'        space space space '10'
                          'Receiver' space space space.

  PERFORM build_fieldcat USING
    'IT_REPORT' 'KSTAR'   'X'        space space space '10'
                          'C.Elemt'  space space space.

** Value
  PERFORM build_fieldcat USING
   'IT_REPORT' 'SUR_MH'  space       space space space '15'
                        'Support MH'    space space space .

  PERFORM build_fieldcat USING
   'IT_REPORT' 'AMT'     space       space space space '15'
                        'Alloc.Amt'    space space space .

  PERFORM build_fieldcat USING
   'IT_REPORT' 'ICON'    space       space space space '6'
                        'Status'     space space 'X'.


*  PERFORM build_fieldcat USING
*   'IT_REPORT' 'TOT_MH'  space       space space space '15'
*                        'Tot.MH'     space space space .
*
*  PERFORM build_fieldcat USING
*   'IT_REPORT' 'RATE'    space       space space space '10'
*                        'Ratio'      space space space .
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
*  wa_fieldcat-qtabname    = p_0110.
  wa_fieldcat-icon       = p_0110.
  wa_fieldcat-col_pos     = gv_col_pos.
*  WA_FIELDCAT-checkbox    = P_0111.
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
*CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
     EXPORTING
*     I_INTERFACE_CHECK              = ' '
*     I_BYPASSING_BUFFER             =
*     I_BUFFER_ACTIVE                = ' '
       i_callback_program             = gv_repid
       i_callback_pf_status_set       = gv_status
       i_callback_user_command        = gv_user_command
*     I_STRUCTURE_NAME               =
      is_layout                      = cs_layo
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

  clear it_report.
  READ TABLE IT_REport WITH KEY CHKbox = 'X'.
  IF SY-SUBRC <> 0 .
     MESSAGE e000 WITH 'Please select at least 1 Item!!!'.
  endif.

  CASE ucomm.
* Important part !
    WHEN 'POST' OR 'CHK'.
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
*  WRITE : / 'Business Process PostVersion:'
*            , gv_p_versn.

* CCTR
  IF p_ccgrp NE space.
    WRITE : / 'Cost Center Grp.           : ', p_ccgrp.
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

  IF p_cegrp NE space.
    WRITE : / 'Cost Element Grp.           : ', p_cegrp.
  ENDIF.

  IF NOT s_kstar[] IS INITIAL.
    LOOP AT s_KSTAR.
      AT FIRST.
        WRITE : / 'Cost Element                : '.
      ENDAT.
      WRITE : / '                            ', s_KSTAR-low, '~',
    s_KSTAR-high.
    ENDLOOP.
  ENDIF.



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

*  DELETE it_report WHERE amt EQ '0'.

  DATA  : line  TYPE i.
  CLEAR : line.
  DESCRIBE TABLE it_report   LINES line.

  IF line = 0.
    MESSAGE e000(zmco) WITH
    ' Enter Value not equal to 0 '.
  ENDIF.


* Init. Message TAB
  CLEAR : it_return, it_return[].

  DATA : lv_conf_text(50).
  DATA : l_date TYPE datum,
         l_last_date TYPE datum.

* TEXT
  CLEAR lv_conf_text.
  CONCATENATE sy-uname  sy-datum  sy-repid
         INTO lv_conf_text
         SEPARATED BY '/'.

* Get last date
  CONCATENATE p_gjahr p_perid+1(2) '01' INTO l_date.
  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
       EXPORTING
            day_in            = l_date
       IMPORTING
            last_day_of_month = l_last_date.

* Fill Header DATA _ kn51n
  CLEAR wa_doc_header.
  wa_doc_header-co_area           = p_kokrs.
  wa_doc_header-docdate           = l_last_date.
  wa_doc_header-postgdate         = l_last_date.
*  wa_doc_header-version           = p_versn.
  wa_doc_header-variant           = '01SAP'.        "Cost center

*Ref.Doc Number (cobk-refbn)
  wa_doc_header-OBJ_KEY           = 'SUPPORT'.
  wa_doc_header-doc_hdr_tx        = lv_conf_text.
  wa_doc_header-username          = sy-uname.
  wa_doc_header-trans_curr  = 'USD'.

* Fill Object List
  CLEAR : it_doc_items, it_doc_items[].

  LOOP AT it_report WHERE chkbox EQ 'X'.
    it_doc_items-seg_text    = lv_conf_text.
    it_doc_items-send_cctr   = it_report-kostl.
    it_doc_items-rec_cctr    = it_report-srkostl.
    it_doc_items-cost_elem   = it_report-kstar.
    it_doc_items-value_tcur  = it_report-amt.
    it_doc_items-quantity    = it_report-sur_mh.
    it_doc_items-postquun    = 'STD'.
    APPEND it_doc_items. CLEAR it_doc_items.
  ENDLOOP.

* Call BAPI FM
* KB51NP- BUSINESS PROCESS ALLOCATION

  PERFORM check_post_test_kb51n USING p_ucomm.
  PERFORM make_log USING p_ucomm.
  MESSAGE s009(zmco) WITH p_ucomm.

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

** Check Input Value (Period)
*  IF p_perid < 0 OR p_perid > 12.
*    MESSAGE e007(zmco) WITH p_perid .
*  ENDIF.
*
** Check Cost Center/Cost Center Group
*  IF     s_kostl[] IS INITIAL
*     AND p_ncoal   IS INITIAL .
*    MESSAGE e016(zmco).
*  ELSEIF
*         NOT s_kostl[] IS INITIAL
*     AND NOT p_ncoal   IS INITIAL .
*    MESSAGE e017(zmco).
*  ENDIF.
*
** Check TEST-RUN  Flag
*  IF p_trun NA 'X '.
*    MESSAGE e008(zmco).
*  ENDIF.
*
** Check Activity type
*  IF p_lstar NE 'MCH_HR'.
*    MESSAGE e018(zmco) WITH p_lstar.
*  ENDIF.
*
*** Check Currency IND.
**  IF P_CURRT NA 'CTO'.
**    MESSAGE E000(ZMCO) WITH P_CURRT ' is not a posible value' .
**  ENDIF.

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
  DATA : l_rate TYPE p DECIMALS 5.
  SORT it_mha.
  CLEAR : it_report, it_report[].
  LOOP AT it_mha.
    MOVE-CORRESPONDING it_mha TO it_report.
* Surpporting rate
    CLEAR l_rate .
    l_rate =  it_mha-sur_mh  / it_mha-tot_mh .
* Cost element all. amt
    LOOP AT it_cosp WHERE kostl = it_mha-kostl.
      CLEAR : it_report-amt, it_report-kstar, it_report-rate.
      IF l_rate <>  0 .
        it_report-amt = it_cosp-amt * l_rate .
      ELSE.
        it_report-amt = it_cosp-amt .
      ENDIF.
      it_report-kstar = it_cosp-kstar.
      it_report-rate  = l_rate.
      CONCATENATE 'KS' p_kokrs it_report-kostl INTO it_report-objnr.
      CONCATENATE 'KS' p_kokrs it_report-srkostl INTO it_report-parob.
      COLLECT it_report.
    ENDLOOP.
    CLEAR it_report.
  ENDLOOP.

* Check if the items are posted or not
  CHECK NOT it_report[] IS INITIAL.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_coep_temp
    FROM coep AS a
   INNER JOIN cobk AS b
      ON a~kokrs = b~kokrs
     AND a~belnr = b~belnr
     FOR ALL ENTRIES IN it_report
   WHERE a~objnr = it_report-objnr
     AND a~gjahr = p_gjahr
     and a~versn = p_versn
     AND a~wrttp = '04'
     AND a~perio = p_perid
     AND a~kstar = it_report-kstar
     AND a~parob = it_report-parob
     AND b~stokz = ''
     and b~STFLG = ''.

*MANDT/OBJNR/KSTAR/GJAHR/PERIO/PAROB1
*MANDT/TIMESTMP/OBJNR, Index for Delt
*OBJNR/KSTAR/GJAHR/PERIO/PAROB1
*OBJNR_HK/OBJNR/BEKNZ

* Delete posted item.
  LOOP AT it_report.
    CLEAR it_coep_temp.
    READ TABLE it_coep_temp WITH KEY objnr = it_report-objnr
                                     parob = it_report-parob
                                     kstar = it_report-kstar.
    IF sy-subrc = 0 .
      DELETE it_report.
      CONTINUE.
    ENDIF.
  ENDLOOP.

  DELETE it_report WHERE amt EQ '0'.
  sort it_report.



ENDFORM.                    " PRE_REPORT_DATA
*&---------------------------------------------------------------------*
*&      Form  CALL_POST_KB51N
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_post_kb51n.

  CALL FUNCTION 'BAPI_ACC_MANUAL_ALLOC_POST'
       EXPORTING
            doc_header = wa_doc_header
       TABLES
            doc_items  = it_doc_items
            return     = it_return.




ENDFORM.                    " CALL_POST_KB51N
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
*&      Form  DIS_LOG_FOR_POST_DOC
*&---------------------------------------------------------------------*
*        Display posted documents
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM dis_log_for_post_doc.

* Saving Document numbers which were posted successfully.
  CLEAR : it_belnr, it_belnr[].
  LOOP AT it_return WHERE type  = 'S'
                      AND id = 'BK'
                      AND number = '003'.
    it_belnr-belnr = it_return-message_v1.

    APPEND it_belnr.
    CLEAR  it_belnr.
    CLEAR it_return.
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
*&      Form  select_ztco_mha
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_ztco_mha.

  SELECT *
         INTO CORRESPONDING FIELDS OF TABLE it_mha_temp
         FROM ztco_mha
        WHERE KOKRS = p_KOKRS
          and gjahr = p_gjahr
          AND perid = p_perid
          AND kostl IN r_kostl
          AND lgart IN ('1', '2', '3') .

  LOOP AT it_mha_temp.
    IF it_mha_temp-lgart = '1' OR it_mha_temp-lgart = '2' .
      MOVE-CORRESPONDING it_mha_temp TO it_mha_tot.
      it_mha_tot-mh = it_mha_temp-anzhl.
      COLLECT it_mha_tot. CLEAR it_mha_tot.
    ELSE.
      MOVE-CORRESPONDING it_mha_temp TO it_mha_sur.
      IF it_mha_temp-anzhl < 0 .
        it_mha_sur-mh = abs( it_mha_temp-anzhl ) .
        COLLECT it_mha_sur. CLEAR it_mha_sur.
      ENDIF.
    ENDIF.
  ENDLOOP.

  LOOP AT it_mha_tot.
    LOOP AT it_mha_sur WHERE kostl = it_mha_tot-kostl.
      MOVE-CORRESPONDING it_mha_sur TO it_mha.
      CONCATENATE 'KS' p_kokrs it_mha_sur-kostl INTO it_mha-objnr.
      it_mha-tot_mh = it_mha_tot-mh.
      it_mha-sur_mh = it_mha_sur-mh.
      COLLECT it_mha. CLEAR it_mha.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    " select_ztco_mha
*&---------------------------------------------------------------------*
*&      Form  select_cosp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_cosp.
  DATA : l_field(30).

  CHECK NOT it_mha[] IS INITIAL .
  SELECT *
         INTO CORRESPONDING FIELDS OF TABLE it_cosp_temp
         FROM cosp
         FOR ALL ENTRIES IN it_mha
        WHERE gjahr = p_gjahr
          AND objnr = it_mha-objnr
          AND wrttp = '04'
          AND kstar IN r_kstar.

  LOOP AT it_cosp_temp.
    CLEAR : l_field.
    CONCATENATE 'IT_COSP_TEMP-WKG' p_perid INTO l_field.
    ASSIGN  (l_field)    TO   <f_field> .
    it_cosp-amt = <f_field> .
    MOVE-CORRESPONDING it_cosp_temp TO it_cosp.
    it_cosp-kostl = it_cosp_temp-objnr+6(10).
    COLLECT it_cosp. CLEAR it_cosp.
  ENDLOOP.

ENDFORM.                    " select_cosp
*&---------------------------------------------------------------------*
*&      Form  CHECK_BEFORE_POST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_before_post.

ENDFORM.                    " CHECK_BEFORE_POST
*&---------------------------------------------------------------------*
*&      Form  MAKE_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_log USING pp_ucomm.

  DATA: it_xmsg   LIKE TABLE OF ztismessage WITH HEADER LINE,
        flag .


  LOOP AT it_return WHERE type EQ 'S'.
    it_report-icon = ICON_INCLUDE_IN_SELECTION.
    MODIFY it_report TRANSPORTING icon WHERE chkbox = 'X'.
  ENDLOOP.


* Check error
  CLEAR:it_return,flag. CLEAR it_xmsg[].
  LOOP AT it_return  WHERE type CA 'WAE'.

    MOVE-CORRESPONDING it_return TO it_xmsg.
    it_xmsg-msgid = it_return-id.
    it_xmsg-msgty = it_return-type.
    it_xmsg-msgno = it_return-number.
    it_xmsg-msgv1 = it_return-message_v1.
    it_xmsg-msgv2 = it_return-message_v2.
    it_xmsg-msgv3 = it_return-message_v3.
    it_xmsg-msgv4 = it_return-message_v4.
    it_xmsg-msgtx = it_return-message.
    APPEND it_xmsg.
    IF it_xmsg-msgty = 'E' OR
       it_xmsg-msgty = 'W' .
      flag = 'X'.
    ENDIF.
  ENDLOOP.

  IF flag = 'X' .
    CALL FUNCTION 'ZMM_IF_POPUP_TO_ERROR_MESSAGE'
         EXPORTING
              xdocno_show = 'X'
         TABLES
              xmsg        = it_xmsg.
  ENDIF.

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

* sy-lsind = 0.
  IF pp_ucomm EQ 'POST'.
    COMMIT WORK.
    LOOP AT it_report WHERE NOT icon IS initial.
      it_report-chkbox = ''.
      MODIFY it_report TRANSPORTING chkbox WHERE NOT icon IS initial.
      EXIT.
    ENDLOOP.
  ENDIF.

  sort it_Report.
  PERFORM call_alv_list.
ENDFORM.                    " MAKE_LOG
*&---------------------------------------------------------------------*
*&      Form  check_post_test_KB51N
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_post_test_kb51n USING pp_ucomm.

* Post
  IF pp_ucomm EQ 'POST'.
    CALL FUNCTION 'BAPI_ACC_MANUAL_ALLOC_POST'
         EXPORTING
              doc_header = wa_doc_header
         TABLES
              doc_items  = it_doc_items
              return     = it_return.
* Error Check : Test run
  ELSE.
    CALL FUNCTION 'BAPI_ACC_MANUAL_ALLOC_CHECK'
         EXPORTING
              doc_header = wa_doc_header
         TABLES
              doc_items  = it_doc_items
              return     = it_return.
  ENDIF.
ENDFORM.                    " check_post_test_KB51N
