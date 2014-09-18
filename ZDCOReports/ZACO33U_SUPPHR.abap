************************************************************************
* Program Name      : ZACO33U_SKF8
* Author            : Eun Hwa , Jung
* Creation Date     : 2003.11.13
* Specifications By : Dong-Hwan Kim, Andy Choi
* Pattern           : Report 1-1
* Development Request No :  UD1K903815
* Addl Documentation:
* Description       : Create Support Working Hour for Reposting

* the BDC structures for BATCH INPUT processing
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

REPORT  zaco33u_skf8 MESSAGE-ID zmco.

** type-pools
TYPE-POOLS: slis.

TABLES : csks, catsdb , ztco_mhaatype .

**// Mod . By Hyung Jin Youn 2004.02.18
* The field "CATSHOURS" is too small to store numbers more than
* 9999.99.
** Main internal table
DATA: BEGIN OF it_catsdb OCCURS 0,
        pernr TYPE catsdb-pernr,
        workdate TYPE catsdb-workdate,
        skostl TYPE catsdb-skostl,
        lstar  TYPE catsdb-lstar,
        rkostl TYPE catsdb-rkostl,
        awart  TYPE catsdb-awart,
*        CATSHOURS TYPE CATSDB-CATSHOURS,
        catshours(9) TYPE p DECIMALS 2,
      END OF it_catsdb.

* sender + hours
DATA: BEGIN OF i_skostl_sum OCCURS 0,
        skostl TYPE catsdb-skostl,
*        LSTAR TYPE CATSDB-LSTAR,
*        CATSHOURS TYPE CATSDB-CATSHOURS,
        catshours(9) TYPE p DECIMALS 2,

      END OF i_skostl_sum.

* sender + receiver + hours
DATA: BEGIN OF i_rkostl_sum OCCURS 0,
        skostl TYPE catsdb-skostl,
*        LSTAR TYPE CATSDB-LSTAR,
        rkostl TYPE catsdb-rkostl,
*        AWART TYPE CATSDB-AWART,
*        CATSHOURS TYPE CATSDB-CATSHOURS,
        catshours(9) TYPE p DECIMALS 2,

      END OF i_rkostl_sum.

* posting
DATA: BEGIN OF it_post OCCURS 0,
*        PERNR TYPE CATSDB-PERNR,
*        WORKDATE TYPE CATSDB-WORKDATE,
        skostl TYPE catsdb-skostl,
        lstar TYPE catsdb-lstar,
        rkostl TYPE catsdb-rkostl,
*        AWART TYPE CATSDB-AWART,
*        CATSHOURS TYPE CATSDB-CATSHOURS,
        catshours(9) TYPE p DECIMALS 2,

      END OF it_post.

* using posting
DATA: BEGIN OF it_post_r_sum OCCURS 0,
*        SKOSTL TYPE CATSDB-SKOSTL,
*        LSTAR TYPE CATSDB-LSTAR,
        rkostl TYPE catsdb-rkostl,
*        AWART TYPE CATSDB-AWART,
*        CATSHOURS TYPE CATSDB-CATSHOURS,
        catshours(9) TYPE p DECIMALS 2,

        stagr TYPE ztco_cc_skf-stagr,
      END OF it_post_r_sum.

DATA: BEGIN OF it_posting OCCURS 0,
        kostl TYPE catsdb-rkostl,
*        CATSHOURS TYPE CATSDB-CATSHOURS,
        catshours(9) TYPE p DECIMALS 2,

        stagr TYPE ztco_cc_skf-stagr,
      END OF it_posting.

* for reporting
DATA: BEGIN OF it_report OCCURS 0,
        skostl TYPE catsdb-skostl,
*        LSTAR TYPE CATSDB-LSTAR,
        rkostl TYPE catsdb-rkostl,
*        T_HOURS TYPE CATSDB-CATSHOURS,
        t_hours(9) TYPE p DECIMALS 2,
*        CATSHOURS TYPE CATSDB-CATSHOURS,
        catshours(9) TYPE p DECIMALS 2,
*        C_HOURS TYPE CATSDB-CATSHOURS,
        c_hours(9) TYPE p DECIMALS 2,

      END OF it_report.
**// End Of Mod.

RANGES : pnpkostl FOR pernr-kostl.

DATA: BEGIN OF it_ztco_cc_skf OCCURS 0,
        kostl TYPE ztco_cc_skf-kostl,
        stagr TYPE ztco_cc_skf-stagr,
      END OF it_ztco_cc_skf.

DATA: BEGIN OF it_ztco_mhaatype OCCURS 0,
        awfrom TYPE ztco_mhaatype-awfrom,
        awto TYPE ztco_mhaatype-awto,
      END OF it_ztco_mhaatype.

** For BAPI
DATA : it_costcenterlist LIKE STANDARD TABLE OF bapi0012_cclist
                         WITH HEADER LINE.
DATA : it_return         LIKE STANDARD TABLE OF bapiret2
                         WITH HEADER LINE.
DATA : wa_doc_header LIKE bapidochdrp .
DATA : it_doc_items  LIKE STANDARD TABLE OF bapiskfitm
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
RANGES : rs_date FOR catsdb-workdate.
RANGES : rs_aw FOR it_ztco_mhaatype-awfrom.
* Globale Daten
INCLUDE rptbal01.


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

SELECTION-SCREEN BEGIN OF BLOCK bl3 WITH FRAME TITLE text-003.
SELECT-OPTIONS : s_kostl  FOR csks-kostl DEFAULT 'MXTX11'.

PARAMETERS:      p_ncoal LIKE grpdynp-name_coall.
*                                   DEFAULT 'HEADCOUNT'.
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
* preparation posting date.
  PERFORM cal_post_date.
* Read Cost Center Group - > Cost Center
  PERFORM read_cegrp.
**// Mod. By Hyung Jin Youn 2004.08.23
** Change of Source data for Timesheet
* Read HR 'CATSDB'.

*replaced by co_mh
*   perform read_fr_catsdb2.
  PERFORM read_co_mh.

  PERFORM read_fr_catsdb3.
**// End of Mod.

* Calculate Working_hour.
  PERFORM cal_workinghour.
* Preparation of ALV
  PERFORM pre_report_adj.

*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.
* Call ALV LIST
  PERFORM call_alv_list.

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

* Cost center
  CLEAR : pnpkostl, pnpkostl[].
  LOOP AT it_costcenterlist.
    pnpkostl-low    = it_costcenterlist-costcenter.
    pnpkostl-sign   = 'I'.
    pnpkostl-option = 'EQ'.
    APPEND pnpkostl.
    CLEAR  pnpkostl.
  ENDLOOP.

ENDFORM.                    " READ_CEGRP
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
*&      Form  READ_CATSDB
*&---------------------------------------------------------------------*
*       READ_CATSDB
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_catsdb.

  PERFORM read_rs_aw.

* Read 'CATSDB'
  CLEAR : it_catsdb, it_catsdb[].
  CLEAR catsdb.
  SELECT *
         INTO CORRESPONDING FIELDS OF TABLE it_catsdb
         FROM catsdb
          FOR ALL ENTRIES IN it_costcenterlist
          WHERE workdate IN rs_date
           AND skostl = it_costcenterlist-costcenter
           AND awart IN rs_aw     "  = '1000'   " A/A type
           AND kokrs = p_kokrs.
  CLEAR  it_catsdb.

ENDFORM.                    " READ_CATSDB
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
* POST PLAN data to STD
    WHEN 'POST'.
      PERFORM pre_post_data.
      PERFORM post_std  USING ucomm.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  POST_STD
*&---------------------------------------------------------------------*
*       Preparation posting
*----------------------------------------------------------------------*
*      -->P_UCOMM  text
*----------------------------------------------------------------------*
FORM post_std USING    p_ucomm.

  DELETE it_posting WHERE catshours EQ '0'.

  DATA  : line  TYPE i.
  CLEAR : line.
  DESCRIBE TABLE it_posting   LINES line.

  IF line = 0.
    MESSAGE e000(zmco) WITH
    ' Enter Value not equal to 0 '.
  ENDIF.


* Init. Message TAB
  CLEAR : it_return, it_return[].

* Fill Header DATA
  CLEAR wa_doc_header.
  wa_doc_header-co_area           = p_kokrs.
  wa_doc_header-docdate           = gv_post_date.
  wa_doc_header-postgdate         = gv_post_date.
  wa_doc_header-version           = p_versn.
  wa_doc_header-variant           = 'SAP01'.
  wa_doc_header-username          = sy-uname.

* Fill Object List
  CLEAR : it_doc_items, it_doc_items[].
  LOOP AT it_posting.
    it_doc_items-statkeyfig = it_posting-stagr.
    it_doc_items-stat_qty   = it_posting-catshours.
    it_doc_items-rec_cctr   = it_posting-kostl.

    APPEND it_doc_items.
  ENDLOOP.

* Call BAPI FM
  PERFORM call_post_fm.

* Commit
  IF p_trun = 'X'.
    READ TABLE it_return  INDEX 1.
    MESSAGE s000(zmco) WITH it_return-message.
  ELSE.
    COMMIT WORK.
    READ TABLE it_return  INDEX 1.
    MESSAGE s000(zmco) WITH it_return-message.
*   MESSAGE S009(ZMCO) WITH P_UCOMM.

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


ENDFORM.                    " CALL_BDC
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

  PERFORM build_fieldcat USING
    'IT_REPORT' 'SKOSTL'  space            space    space
    space   '10'      'CCtr'    space    space space.

  PERFORM build_fieldcat USING
    'IT_REPORT' 'T_HOURS'  space            space    space
    space    '10'      'Org M/H'    space    space space.


  PERFORM build_fieldcat USING
   'IT_REPORT' 'CATSHOURS'  space            space    space
   space    '10'      'Sen M/H'    space    space space.


  PERFORM build_fieldcat USING
   'IT_REPORT' 'RKOSTL'  space            space    space
   space    '15'      'Rec. CC'    space    space space.


  PERFORM build_fieldcat USING
  'IT_REPORT' 'C_HOURS'  space            space    space
  space    '10'      'Total M/H'    space    space space.

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

*---------------------------------------------------------------------*
*       FORM BASIC_TOP_OF_PAGE                                        *
*---------------------------------------------------------------------*
*       Display Title                                                 *
*---------------------------------------------------------------------*
FORM basic_top_of_page.

  WRITE : / 'Controlling Area      : ', p_kokrs .
  WRITE : / 'Fiscal Year/Period    : '
            , p_gjahr, '/', p_perid.
  WRITE : / 'Version               : ', p_versn .
  WRITE : / 'Posting Date          : ', gv_post_date.

* CCTR
  IF p_ncoal NE space.
    WRITE : / 'Cost Center Group     : ', p_ncoal.
  ENDIF.

  IF NOT s_kostl[] IS INITIAL.
    LOOP AT s_kostl.
      AT FIRST.
        WRITE : / 'Cost Center           :     '.
      ENDAT.
      WRITE : / '                        ', s_kostl-low, '~',
s_kostl-high.
    ENDLOOP.
  ENDIF.

  WRITE : / 'Test Run              : ', p_trun.
  SKIP 1.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CAL_POST_DATE
*&---------------------------------------------------------------------*
*       calculate posting date
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cal_post_date.

** <  Posting , hard coding > **
* doc date / posting date   =  ' YEAR/MONTH/25 '
  CLEAR gv_post_date.
  CONCATENATE  p_gjahr  p_perid+1(2) '25'  INTO gv_post_date.


**--- CREATE RANGES FOR WORKDATE.
  DATA :  dat1 LIKE sy-datum,
          dat2 LIKE sy-datum.

  CLEAR : dat1, dat2.

  CONCATENATE p_gjahr p_perid+1(2) '01' INTO dat1.
  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
       EXPORTING
            day_in            = dat1
       IMPORTING
            last_day_of_month = dat2.

  rs_date-sign = 'I'.
  rs_date-option = 'BT'.
  rs_date-low = dat1.
  rs_date-high = dat2.
  APPEND rs_date.   CLEAR rs_date.


ENDFORM.                    " CAL_POST_DATE
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

* Check TEST-RUN  Flag
  IF p_trun NA 'X '.
    MESSAGE e008(zmco).
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


ENDFORM.                    " CHK_INPUT_VALUE
*&---------------------------------------------------------------------*
*&      Form  CALL_POST_FM
*&---------------------------------------------------------------------*
*       CALL_POST_FM
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
*&      Form  CAL_WORKINGHOUR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cal_workinghour.

  SORT it_catsdb BY skostl.

* create 'internal table' using reporting
  CLEAR : it_post,      it_post[],
          i_rkostl_sum, i_rkostl_sum[],
          i_skostl_sum, i_skostl_sum[].
  CLEAR it_catsdb.
  LOOP AT it_catsdb.
    IF it_catsdb-rkostl <> ' '.
      MOVE-CORRESPONDING it_catsdb TO it_post.
      COLLECT it_post.
      CLEAR it_post.
    ENDIF.

    MOVE-CORRESPONDING it_catsdb TO i_rkostl_sum.
    COLLECT i_rkostl_sum.
    CLEAR i_rkostl_sum.

    MOVE-CORRESPONDING it_catsdb TO i_skostl_sum.
    COLLECT i_skostl_sum.
    CLEAR i_skostl_sum.
  ENDLOOP.
  CLEAR it_catsdb.


* preparation report.
  CLEAR : it_report, it_report[].
  CLEAR it_post.
  LOOP AT it_post.
    CLEAR i_skostl_sum.
    READ TABLE i_skostl_sum WITH KEY skostl = it_post-skostl.
    MOVE i_skostl_sum-catshours TO it_report-t_hours.
    MOVE-CORRESPONDING it_post TO it_report.
    CLEAR i_rkostl_sum.
    READ TABLE i_rkostl_sum WITH KEY skostl = it_post-skostl
                                      rkostl = space.
    IF sy-subrc = 0.
      MOVE i_rkostl_sum-catshours TO it_report-c_hours.
    ELSE.
      it_report-c_hours = 0.
    ENDIF.

    APPEND it_report.
    CLEAR it_report.

  ENDLOOP.
  CLEAR it_post.

ENDFORM.                    " CAL_WORKINGHOUR
*&---------------------------------------------------------------------*
*&      Form  READ_RS_AW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_rs_aw.

  CLEAR : it_ztco_mhaatype, it_ztco_mhaatype[].
  SELECT awfrom awto
         INTO  TABLE it_ztco_mhaatype
            FROM ztco_mhaatype
          WHERE selind = 'X'.

  LOOP AT it_ztco_mhaatype.
    IF it_ztco_mhaatype-awto = ' '.
      rs_aw-sign = 'I'.
      rs_aw-option = 'EQ'.
      rs_aw-low = it_ztco_mhaatype-awfrom.
    ELSE.
      rs_aw-sign = 'I'.
      rs_aw-option = 'BT'.
      rs_aw-low = it_ztco_mhaatype-awfrom.
      rs_aw-high = it_ztco_mhaatype-awto.
    ENDIF.
    APPEND rs_aw.   CLEAR rs_aw.
  ENDLOOP.

ENDFORM.                    " READ_RS_AW
*&---------------------------------------------------------------------*
*&      Form  PRE_POST_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pre_post_data.

* Set Validity Date (Start)
  DATA  lv_datum LIKE sy-datum.
  CLEAR lv_datum.
  CONCATENATE p_gjahr p_perid+1(2) '01' INTO lv_datum.

  CLEAR : it_ztco_cc_skf , it_ztco_cc_skf[].
  SELECT *
       INTO CORRESPONDING FIELDS OF TABLE it_ztco_cc_skf
       FROM ztco_cc_skf
          WHERE datab <= lv_datum.       "Validity Date (Start)


  CLEAR : it_posting, it_posting[].
  CLEAR it_report.
  LOOP AT it_report.
    CLEAR it_ztco_cc_skf.
    READ TABLE it_ztco_cc_skf WITH KEY kostl = it_report-skostl.
    IF sy-subrc = 0.
      it_posting-kostl = it_report-rkostl.
      it_posting-stagr = it_ztco_cc_skf-stagr.
      it_posting-catshours = it_report-catshours.
      APPEND it_posting.
      CLEAR  it_posting.
    ELSE.
      MESSAGE e000(zmco) WITH 'There is no data-' it_report-skostl
                               ',Check (T-CODE:ZACO38)-ZTCO_CC_SKF'.
    ENDIF.
  ENDLOOP.


  CLEAR it_report.
  LOOP AT it_report WHERE c_hours <> 0.
    CLEAR it_ztco_cc_skf.
    READ TABLE it_ztco_cc_skf WITH KEY kostl = it_report-skostl.
    IF sy-subrc = 0.
      it_posting-kostl = it_report-skostl.
      it_posting-stagr = it_ztco_cc_skf-stagr.
      it_posting-catshours = it_report-c_hours.
      APPEND it_posting.
      CLEAR  it_posting.
    ELSE.
      MESSAGE e000(zmco) WITH 'There is no data-' it_report-skostl
                               ',Check (T-CODE:ZACO38)-ZTCO_CC_SKF'.
    ENDIF.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM it_posting.


ENDFORM.                    " PRE_POST_DATA

*&---------------------------------------------------------------------*
*&      Form  READ_FR_CATSDB3
*&---------------------------------------------------------------------*
*       Cal. Supportive and Not-Supportive Working Hour
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_fr_catsdb3.

* TimeSheet  (Summed)
  CLEAR it_catsdb.  SORT it_catsdb BY skostl lstar rkostl .

* Local Data definition
  DATA : it_l_catsdb3 LIKE STANDARD TABLE OF it_catsdb
                      WITH HEADER LINE .

* Select data
  CLEAR : it_l_catsdb3, it_l_catsdb3[].
  SELECT workdate  skostl  lstar   rkostl
         awart     unit    status  catshours
         INTO CORRESPONDING FIELDS OF TABLE it_l_catsdb3
         FROM catsdb
        WHERE
*             AWART    IN R_L_AWART
              status   = '30'
          AND workdate IN rs_date.

  IF  it_l_catsdb3[] IS INITIAL.
*    MESSAGE E026.
  ENDIF.

* All information for sender cctr and receiver cctr must be
* in each record
  DELETE it_l_catsdb3
   WHERE skostl    EQ space
      OR rkostl    EQ space
      OR catshours EQ space.

  LOOP AT it_l_catsdb3.
    CLEAR it_catsdb.
    MOVE-CORRESPONDING it_l_catsdb3 TO it_catsdb.
    COLLECT it_catsdb.
    CLEAR   it_catsdb.
    CLEAR it_l_catsdb3.
  ENDLOOP.

ENDFORM.                    " READ_FR_CATSDB3
*&---------------------------------------------------------------------*
*&      Form  read_co_mh
*&---------------------------------------------------------------------*
FORM read_co_mh.
  TABLES: ztco_mhat.
  DATA: i_mhat LIKE ztco_mhat OCCURS 0 WITH HEADER LINE.

* regular + OT
  SELECT * INTO TABLE i_mhat
    FROM ztco_mhat
    WHERE gjahr = p_gjahr
      AND perid = p_perid
      AND kostl IN pnpkostl
      AND lgart BETWEEN '1' and '2'.  "regular, ot,

  CLEAR : it_catsdb, it_catsdb[].

  LOOP AT i_mhat.
    it_catsdb-skostl     = i_mhat-kostl.
    it_catsdb-catshours  = i_mhat-anzhl.
    COLLECT it_catsdb.
  ENDLOOP.

ENDFORM.                    " read_co_mh
