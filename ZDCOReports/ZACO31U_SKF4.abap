************************************************************************
* Program Name      : ZACO31U_SKF4
* Author            : Eun Hwa , Jung
* Creation Date     : 2003.10.17.
* Specifications By : Dong-Hwan Kim
* Pattern           : Report 1-1
* Development Request No : UD1K903011
* Addl Documentation:
* Description       : Create No of Persons by Department(Actual)

* the BDC structures for BATCH INPUT processing
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

REPORT  zaco31u_skf4 MESSAGE-ID zmco.

** type-pools
TYPE-POOLS: slis.

TABLES : csks, pa0001, pa0000.   ", ZTCO_SKF_MANAGE.

* Constants
CONSTANTS: c_skf TYPE stagr VALUE 'CS001'.


* Main internal table
DATA: BEGIN OF it_pa0001 OCCURS 0,
        kostl LIKE pa0001-kostl,
        count TYPE i,
*-- tEST
        pernr LIKE pa0001-pernr,
*        DATE  LIKE COHEADER-BUDAT,
*        STAGR LIKE TKA03-STAGR,
*        PERNR LIKE PA0001-PERNR,
      END OF it_pa0001.

DATA: BEGIN OF it_post OCCURS 0,
        cgroup(15),
        count TYPE i,
      END OF it_post.

** For BAPI
*DATA : it_costcenterlist LIKE STANDARD TABLE OF bapi1112_values
*                         WITH HEADER LINE.
DATA : IT_COSTCENTERLIST LIKE STANDARD TABLE OF BAPI0012_CCLIST
                         WITH HEADER LINE.
DATA : it_nodes LIKE STANDARD TABLE OF bapiset_hier
                         WITH HEADER LINE.
DATA : it_return         LIKE STANDARD TABLE OF bapiret2
                         WITH HEADER LINE.
DATA : wa_doc_header LIKE bapidochdrp .
DATA : it_doc_items  LIKE STANDARD TABLE OF bapiskfitm
                     WITH HEADER LINE.

DATA: BEGIN OF it_low_cost OCCURS 0,
        cgroup(15),
        kostl(15),
      END OF it_low_cost.

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
SELECT-OPTIONS : S_KOSTL  FOR CSKS-KOSTL.
PARAMETERS:      p_ncoal LIKE grpdynp-name_coall
                                   DEFAULT 'HEADCOUNT'.
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
* Read Cost Center Group - > low level -> Cost Center
  PERFORM read_cegrp.
* Read HR master
  PERFORM read_personnel_count.
* Preparation of Posting
  PERFORM pre_posting.
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
  DATA : lv_datum LIKE sy-datum.

* Making an internal table for CCtr to select data
* Selected Group on screen
  CLEAR : it_costcenterlist, it_costcenterlist[].
  CLEAR : it_nodes , it_nodes[].
  CLEAR : it_return, it_return[].

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

  LOOP AT it_costcenterlist.
    it_low_cost-kostl = it_costcenterlist-COSTCENTER.
    APPEND it_low_cost.
  ENDLOOP.
*
*  DATA : COUNT TYPE I.
*  CLEAR : IT_LOW_COST, IT_LOW_COST[].
*
*  LOOP AT IT_NODES.
*    CLEAR COUNT.
*    COUNT = IT_NODES-VALCOUNT.
*    DO COUNT TIMES.
*      IT_LOW_COST-CGROUP = IT_NODES-GROUPNAME.
*      APPEND IT_LOW_COST.
*    ENDDO.
*  ENDLOOP.
*
*  CLEAR IT_LOW_COST.
*  LOOP AT IT_LOW_COST.
*    CLEAR IT_COSTCENTERLIST.
*    READ TABLE IT_COSTCENTERLIST INDEX SY-TABIX.
*
*    IT_LOW_COST-KOSTL = IT_COSTCENTERLIST-VALFROM.
*    MODIFY IT_LOW_COST.
*
*  ENDLOOP.



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
*&      Form  READ_PERSONNEL_COUNT
*&---------------------------------------------------------------------*
*       READ_HR_MASTER
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_personnel_count.

* Sorting
*  SORT it_costcenterlist BY valfrom valto.
  SORT it_costcenterlist BY COSTCENTER.

**   end date setting
  DATA : first_day TYPE sy-datum,
         last_day TYPE sy-datum.

  CLEAR :  first_day, last_day.

  CONCATENATE p_gjahr p_perid+1(2) '01' INTO first_day.

  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
       EXPORTING
            day_in            = first_day
       IMPORTING
            last_day_of_month = last_day.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


** Obsoleted 2004.07.14
***// Begin of Mod. By Hyung Jin Youn 2004.07.09
*** Only for EE group = '1' or '9'
*  CLEAR : IT_PA0001, IT_PA0001[].
*  CLEAR   PA0001.
*  LOOP AT IT_COSTCENTERLIST.
*    SELECT KOSTL COUNT( DISTINCT PERNR )
*         APPENDING TABLE IT_PA0001
*         FROM PA0001
*          WHERE  ENDDA >= LAST_DAY             " '99991231'
*           AND   KOSTL = IT_COSTCENTERLIST-VALFROM
*           AND   KOKRS = P_KOKRS
*           AND   PERSG IN ('1', '9')
*             GROUP BY KOSTL.
*  ENDLOOP.
*  CLEAR IT_PA0001.
*
*** End of Mod.

**// Begin of Mod. By Hyung Jin Youn 2004.07.14
*    Change of condition(where clause)
  CLEAR : it_pa0001, it_pa0001[].
  CLEAR   pa0001.
  LOOP AT it_costcenterlist.
    SELECT kostl COUNT( DISTINCT a~pernr )
         APPENDING TABLE it_pa0001
        FROM pa0001 AS a  INNER JOIN pa0000 AS b
           ON a~pernr = b~pernr
          AND a~subty = b~subty
          AND a~objps = b~objps
          AND a~sprps = b~sprps
          AND a~endda = b~endda
*         AND A~BEGDA = B~BEGDA
          AND a~seqnr = b~seqnr
         WHERE
*                a~kostl   BETWEEN it_costcenterlist-valfrom
*                              AND it_costcenterlist-valto
                a~kostl = it_costcenterlist-costcenter
          and   a~kokrs = p_kokrs
          AND   b~stat2 = '3' "'Active
          AND  (      a~begda <= last_day
                AND   a~endda >= last_day )
            GROUP by kostl.
  ENDLOOP.

  CLEAR it_pa0001.

** End of Mod.

ENDFORM.                    " READ_PERSONNEL_COUNT

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
    WHEN 'POST'.    " OR 'REVS'.
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
  DATA : lv_conf_text(50).

  DELETE it_post WHERE count EQ '0'.

  DATA  : line  TYPE i.
  CLEAR : line.
  DESCRIBE TABLE it_post   LINES line.

  IF line = 0.
    MESSAGE e000(zmco) WITH
    ' Enter Value not equal to 0 '.
  ENDIF.

* Init. Message TAB
  CLEAR : it_return, it_return[].

** <  Posting , hard coding > **
* doc date / posting date   =  ' YEAR/MONTH/25 '
* SKF                        =  'CS001'
* TEXT
  CLEAR lv_conf_text.
  CONCATENATE sy-uname  sy-datum  sy-repid
         INTO lv_conf_text
         SEPARATED BY '/'.

* Fill Header DATA
  CLEAR wa_doc_header.
  wa_doc_header-co_area           = p_kokrs.
  wa_doc_header-docdate           = gv_post_date.
  wa_doc_header-postgdate         = gv_post_date.
  wa_doc_header-version           = p_versn.
  wa_doc_header-variant           = 'SAP01'.
  wa_doc_header-doc_hdr_tx        = lv_conf_text.
  wa_doc_header-username          = sy-uname.

* Fill Object List
  CLEAR : it_doc_items, it_doc_items[].
  LOOP AT it_post.
    it_doc_items-statkeyfig = c_skf.                        " 'CS001'
    it_doc_items-stat_qty   = it_post-count.
**// Mod. By Hyung Jin Youn
* -> Alpha Numeric Conversion
*    IT_DOC_ITEMS-REC_CCTR   = IT_POST-CGROUP.
    it_doc_items-rec_cctr   = it_post-cgroup.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
         EXPORTING
              input  = it_doc_items-rec_cctr
         IMPORTING
              output = it_doc_items-rec_cctr.
**// End of Mod.
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
*           IM_KOSTL_F        =    S_KOSTL-LOW
*           IM_KOSTL_T        =    S_KOSTL-HIGH
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

*  PERFORM BUILD_FIELDCAT USING
*    'IT_PA0001'  'DATE'       'X'            SPACE    SPACE
*    SPACE        '15'      'Posting Date'   SPACE    SPACE      SPACE.

  PERFORM build_fieldcat USING
    'IT_POST' 'CGROUP'  'X'            space    space
    space        '15'      'Receiver CCtr'   space    space      space.

*  PERFORM BUILD_FIELDCAT USING
*    'IT_PA0001' 'STAGR'  SPACE            SPACE    SPACE
*    SPACE   '15'      'SKF'    SPACE    SPACE SPACE.

  PERFORM build_fieldcat USING
    'IT_POST' 'COUNT'  space            space    space
    space    '15'      'Quantity'    space    space space.

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
       t_outtab                       = it_post
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
  WRITE : / 'SKF                   : ', 'CS001-No.of Persons by Team.' .

* CCTR
  IF p_ncoal NE space.
    WRITE : / 'Cost Center Group     : ', p_ncoal.
  ENDIF.

*  IF NOT S_KOSTL[] IS INITIAL.
*    LOOP AT S_KOSTL.
*      AT FIRST.
*        WRITE : / 'Cost Center           :     '.
*      ENDAT.
*      WRITE : / '                        ', S_KOSTL-LOW, '~',
*S_KOSTL-HIGH.
*    ENDLOOP.
*  ENDIF.

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

  DATA  : perid LIKE cosp-perbl.
  DATA  : gjahr LIKE cobk-gjahr.
  CLEAR : perid, gjahr, gv_post_date.

**// Begin of Mod. By Hyung Jin Youn  2004.07.09
* Change posting date to 25th of P_PERID and P_GJAHR.

*  IF P_PERID = '1'.
*    GJAHR = P_GJAHR - 1.
*    CONCATENATE  GJAHR  '12' '25'  INTO GV_POST_DATE.
*  ELSE.
*    PERID = P_PERID - 1.
*    CONCATENATE  P_GJAHR  PERID+1(2) '25'  INTO GV_POST_DATE.
*  ENDIF.

  CONCATENATE  p_gjahr  p_perid+1(2) '25'  INTO gv_post_date.

**// End of Mod.

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
  IF    " S_KOSTL[] IS INITIAL
     "AND
     p_ncoal   IS INITIAL .
    MESSAGE e016(zmco).
*  ELSEIF
*         NOT S_KOSTL[] IS INITIAL
*     AND NOT P_NCOAL   IS INITIAL .
*    MESSAGE E017(ZMCO).
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
*&      Form  PRE_POSTING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pre_posting.

  CLEAR : it_post, it_post[].
  LOOP AT it_pa0001.
    CLEAR it_low_cost.
    READ TABLE it_low_cost WITH KEY kostl = it_pa0001-kostl.
    it_post-cgroup = it_low_cost-kostl.
    it_post-count  = it_pa0001-count.
    COLLECT it_post.
    CLEAR   it_post.
  ENDLOOP.
  CLEAR it_pa0001.

  SORT it_post BY cgroup.

ENDFORM.                    " PRE_POSTING
