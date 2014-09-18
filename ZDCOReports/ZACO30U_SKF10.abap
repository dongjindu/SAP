************************************************************************
* Program Name      : ZACO30U_SKF10
* Author            : Eun Hwa , Jung
* Creation Date     : 2003.11.21.
* Specifications By : Dong-Hwan Kim
* Pattern           : Report 1-1
* Development Request No : UD1K904312
* Addl Documentation:
* Description       : Create NO of persons by department(Plan)
* the BDC structures for BATCH INPUT processing
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************


REPORT ZACO30U_SKF10 MESSAGE-ID ZMCO.

** type-pools
TYPE-POOLS: SLIS.

** Table
TABLES : CSKS , ZTHR_AHC01.  " ZTHR_HCP01.

** Internal Table
*DATA: BEGIN OF IT_ZTHR_HCP01 OCCURS 0,
*        ZYEAR TYPE ZTHR_HCP01-ZYEAR,
*        ZMONS TYPE ZTHR_HCP01-ZMONS,
*        ZCOST TYPE ZTHR_HCP01-ZCOST,
*        ZHEDC TYPE ZTHR_HCP01-ZHEDC,
*      END OF IT_ZTHR_HCP01.

DATA: BEGIN OF IT_ZTHR_AHC01 OCCURS 0,
        ZVERS TYPE ZTHR_AHC01-ZVERS,
        ZYEAR TYPE ZTHR_AHC01-ZYEAR,
        ZMONS TYPE ZTHR_AHC01-ZMONS,
*Issue Number : CO-20041115-010, Requested by DHKIM
*Changed on 2004/11/17, by WSKIM
*---Start
*        ZSCST TYPE ZTHR_AHC01-ZSCST,
        ZCOST TYPE ZTHR_AHC01-ZCOST,
*---End
        ZHEDC TYPE ZTHR_AHC01-ZHEDC,
        ZNEWC TYPE ZTHR_AHC01-ZNEWC,
      END OF IT_ZTHR_AHC01.

DATA: BEGIN OF IT_SUM OCCURS 0,
        ZVERS TYPE ZTHR_AHC01-ZVERS,
        ZYEAR TYPE ZTHR_AHC01-ZYEAR,
        ZMONS TYPE ZTHR_AHC01-ZMONS,
*Issue Number : CO-20041115-010, Requested by DHKIM
*Changed on 2004/11/17, by WSKIM
*---Start
*        ZSCST TYPE ZTHR_AHC01-ZSCST,
        ZCOST TYPE ZTHR_AHC01-ZCOST,
*---End
        ZHEDC TYPE ZTHR_AHC01-ZHEDC,
        ZNEWC TYPE ZTHR_AHC01-ZNEWC,
      END OF IT_SUM.


*DATA: BEGIN OF IT_SUM OCCURS 0,
*        ZYEAR TYPE ZTHR_HCP01-ZYEAR,
*        ZMONS TYPE ZTHR_HCP01-ZMONS,
*        ZCOST TYPE ZTHR_HCP01-ZCOST,
*        ZHEDC TYPE ZTHR_HCP01-ZHEDC,
*      END OF IT_SUM.

* for reporting
DATA : BEGIN OF IT_REPORT OCCURS 0,
        VERS(3),                    "TYPE  ZTHR_AHC01-ZVERS,
        GJAHR   LIKE  COSL-GJAHR,
        PERID   TYPE  ZTHR_AHC01-ZMONS,
        KOSTL   LIKE  CSKS-KOSTL,
        COUNT   TYPE  ZTHR_AHC01-ZHEDC,
      END OF IT_REPORT.

* for posting
DATA : BEGIN OF IT_POST OCCURS 0,
        GJAHR   LIKE  COSL-GJAHR,
        KOSTL   LIKE  CSKS-KOSTL.
        INCLUDE STRUCTURE ZSCO_COSP_AMT02.
DATA : END OF  IT_POST.


** For ALV
DATA : GV_REPID LIKE SY-REPID.
DATA : GV_STATUS       TYPE SLIS_FORMNAME VALUE 'PF_STATUS'.
DATA : GV_USER_COMMAND TYPE SLIS_FORMNAME VALUE 'USER_COMMAND'.
DATA : IT_SORT         TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE .
DATA : GV_COL_POS TYPE I.
DATA : IT_FIELDCAT          TYPE SLIS_T_FIELDCAT_ALV,
       WA_FIELDCAT          LIKE LINE OF IT_FIELDCAT,
       IT_EVENTCAT          TYPE SLIS_T_EVENT,
       WA_EVENTCAT          LIKE LINE OF IT_EVENTCAT.
DATA : IT_EVENTS	          TYPE SLIS_T_EVENT,
       IT_EVENT_EXIT	    TYPE SLIS_T_EVENT_EXIT.

*
DATA : GV_PERCOUNT       LIKE COSP-PERBL. "Period Counter

** For BAPI
DATA : IT_COSTCENTERLIST LIKE STANDARD TABLE OF BAPI1112_VALUES
                         WITH HEADER LINE.
DATA : IT_NODES LIKE STANDARD TABLE OF BAPISET_HIER
                         WITH HEADER LINE.
DATA : IT_RETURN         LIKE STANDARD TABLE OF BAPIRET2
                         WITH HEADER LINE.
DATA : WA_HEADERINFO     LIKE BAPIPLNHDR_SKF.
DATA : IT_INDEXSTRUCTURE LIKE STANDARD TABLE OF BAPIACISTRU
                         WITH HEADER LINE.
DATA : IT_COOBJECT       LIKE STANDARD TABLE OF BAPISKFOBJ
                         WITH HEADER LINE.
DATA : IT_PERVALUE       LIKE STANDARD TABLE OF BAPISKFVAL
                         WITH HEADER LINE.
DATA : IT_TOTVALUE       LIKE STANDARD TABLE OF BAPISKFTOT
                         WITH HEADER LINE.

DATA: BEGIN OF IT_LOW_COST OCCURS 0,
        CGROUP(15),
        KOSTL(15),
      END OF IT_LOW_COST.

RANGES : RS_PERID FOR ZTHR_HCP01-ZMONS.

* Macro For Transferring value in BAPI
DEFINE TRANS_VALUE.
  IT_PERVALUE-QUANTITY_PER&1  =  IT_POST-VAR0&1.
END-OF-DEFINITION.


*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-001.
PARAMETERS :
             P_KOKRS LIKE CSKS-KOKRS MEMORY ID CAC OBLIGATORY,
             P_GJAHR LIKE COBK-GJAHR MEMORY ID GJR OBLIGATORY,
             P_FRPER LIKE COSP-PERBL MEMORY ID BPE OBLIGATORY,
             P_TOPER LIKE COSP-PERBL MEMORY ID BPE OBLIGATORY,
             P_VERHR LIKE COBK-VERSN MEMORY ID KVT OBLIGATORY,
             P_VERSN LIKE COBK-VERSN MEMORY ID KVT OBLIGATORY,
             P_TRUN(1).

SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK BL3 WITH FRAME TITLE TEXT-003.
*SELECT-OPTIONS : S_KOSTL  FOR CSKS-KOSTL.
PARAMETERS:      P_NCOAL LIKE GRPDYNP-NAME_COALL
                                     DEFAULT 'HEADCOUNT'.
SELECTION-SCREEN END OF BLOCK BL3.
SELECTION-SCREEN END OF BLOCK BL1.


*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
INITIALIZATION.
* Set Global ALV Parameter
  GV_REPID = SY-REPID.


*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  PERFORM CHK_INPUT_VALUE.

* Searching for Cost Center group
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_NCOAL.
  PERFORM READ_CEGRP_GROUP USING '0101'
                                 P_NCOAL.


*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Calculating Period Count
  PERFORM CAL_PER_COUNT.
* Read Cost Center Group - > low level -> Cost Center
  PERFORM READ_CEGRP.
* Read 'Head Count Plan' data.
  PERFORM READ_HEADCOUNT_PLAN.
* Preparation of ALV
  PERFORM PRE_REPORT_ADJ.
* Preparation of posting data.
  PERFORM POSTING_DATA.

*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.
* Call ALV LIST
  PERFORM CALL_ALV_LIST.

*&---------------------------------------------------------------------*
*&      Form  READ_CEGRP_GROUP
*&---------------------------------------------------------------------*
*       Search Help for Cost element Group / CCTr Group
*----------------------------------------------------------------------*
*  -->  p_class      Class Name
*  <--  P_SET_NAME   Result Group Name
*----------------------------------------------------------------------*
FORM READ_CEGRP_GROUP USING   P_CLASS
                               P_SET_NAME.

  CALL FUNCTION 'K_GROUP_SELECT'
    EXPORTING
*     BUTTONS                  = 'X'
      CLASS                    = P_CLASS
*     CRUSER                   = '*'
      FIELD_NAME               = SPACE
*     SEARCHFLD                = '    '
*     SEARCHFLD_INPUT          = 'X'
      SEARCHFLD_REQUIRED       = 'X'
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
      SET_NAME                 = P_SET_NAME
*     SET_TITLE                =
*     TABLE_NAME               =
*     SETID                    =
    EXCEPTIONS
      NO_SET_PICKED            = 1
      OTHERS                   = 2.

* No error check for F4  SH
  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    " READ_CEGRP_GROUP
*&---------------------------------------------------------------------*
*&      Form  READ_CEGRP
*&---------------------------------------------------------------------*
*       read cost center
*----------------------------------------------------------------------*
FORM READ_CEGRP.

* Making an internal table for CCtr to select data
* Selected Group on screen
  CLEAR : IT_COSTCENTERLIST, IT_COSTCENTERLIST[].
  CLEAR : IT_NODES , IT_NODES[].
  CLEAR : IT_RETURN, IT_RETURN[].

  CALL FUNCTION 'BAPI_COSTCENTERGROUP_GETDETAIL'
    EXPORTING
      CONTROLLINGAREA       = P_KOKRS
      GROUPNAME             = P_NCOAL
*   IMPORTING
     RETURN                = IT_RETURN
    TABLES
      HIERARCHYNODES        = IT_NODES
      HIERARCHYVALUES       = IT_COSTCENTERLIST.

** Message
  PERFORM DIS_BAPI_MESSAGE.


  DELETE ADJACENT DUPLICATES FROM IT_COSTCENTERLIST.

* Check CCtr
  IF IT_COSTCENTERLIST[] IS INITIAL .
    MESSAGE E018(ZMCO) WITH 'Cost Center'.
  ENDIF.

*
  DATA : COUNT TYPE I.
  CLEAR : IT_LOW_COST, IT_LOW_COST[].

  LOOP AT IT_NODES.
    CLEAR COUNT.
    COUNT = IT_NODES-VALCOUNT.
    DO COUNT TIMES.
      IT_LOW_COST-CGROUP = IT_NODES-GROUPNAME.
      APPEND IT_LOW_COST.
    ENDDO.
  ENDLOOP.

  CLEAR IT_LOW_COST.
  LOOP AT IT_LOW_COST.
    CLEAR IT_COSTCENTERLIST.
    READ TABLE IT_COSTCENTERLIST INDEX SY-TABIX.

    IT_LOW_COST-KOSTL = IT_COSTCENTERLIST-VALFROM.
    MODIFY IT_LOW_COST.

  ENDLOOP.



ENDFORM.                    " READ_CEGRP
*&---------------------------------------------------------------------*
*&      Form  DIS_BAPI_MESSAGE
*&---------------------------------------------------------------------*
*       Display BAPI Message
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DIS_BAPI_MESSAGE.
  IF NOT IT_RETURN[] IS INITIAL.
    LOOP AT   IT_RETURN.
      MESSAGE ID     IT_RETURN-ID
              TYPE   IT_RETURN-TYPE
              NUMBER IT_RETURN-NUMBER
              WITH   IT_RETURN-MESSAGE_V1
                     IT_RETURN-MESSAGE_V2
                     IT_RETURN-MESSAGE_V3
                     IT_RETURN-MESSAGE_V4.
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
FORM CAL_PER_COUNT.

  GV_PERCOUNT = P_TOPER - P_FRPER + 1.

ENDFORM.                    " CAL_PER_COUNT
*&---------------------------------------------------------------------*
*&      Form  PRE_REPORT_ADJ
*&---------------------------------------------------------------------*
*       The Preparation for ALV List
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRE_REPORT_ADJ.

* Building Field Cat.
  PERFORM FIELDCAT_INIT .

* Sort
  SORT IT_REPORT BY VERS GJAHR PERID KOSTL .
  CLEAR IT_REPORT.

*  IT_SORT-FIELDNAME = 'GJAHR'.
*  IT_SORT-UP        = 'X'.
*  IT_SORT-EXPA      = 'X'.
*  IT_SORT-SUBTOT    = 'X'.
*  APPEND IT_SORT.
*  IT_SORT-FIELDNAME = 'PERID'.
*  IT_SORT-UP        = 'X'.
*  IT_SORT-EXPA      = 'X'.
*  IT_SORT-SUBTOT    = 'X'.
*  APPEND IT_SORT.
*  IT_SORT-FIELDNAME = 'KOSTL'.
*  IT_SORT-UP        = 'X'.
*  IT_SORT-EXPA      = 'X'.
*  IT_SORT-SUBTOT    = 'X'.
*  APPEND IT_SORT.
*  IT_SORT-FIELDNAME = 'LSTAR'.
*  IT_SORT-UP        = 'X'.
*  IT_SORT-EXPA      = 'X'.
*  IT_SORT-SUBTOT    = 'X'.
*  APPEND IT_SORT.


* Set Event
  DATA : WA_L_EVENT  TYPE SLIS_ALV_EVENT.
  WA_L_EVENT-NAME = SLIS_EV_TOP_OF_PAGE.
  WA_L_EVENT-FORM = 'BASIC_TOP_OF_PAGE'.
  APPEND WA_L_EVENT TO IT_EVENTS.


ENDFORM.                    " PRE_REPORT_ADJ
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_INIT
*&---------------------------------------------------------------------*
*       Building Field Cat.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FIELDCAT_INIT.

  CLEAR : GV_COL_POS, IT_FIELDCAT, IT_FIELDCAT[].


* Key

  PERFORM BUILD_FIELDCAT USING
   'IT_REPORT' 'VERS'  'X'            SPACE    SPACE
   SPACE        '7'      'Version'           SPACE    SPACE    SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_REPORT' 'GJAHR'  'X'            SPACE    SPACE
    SPACE        '4'      'Year'           SPACE    SPACE    SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_REPORT' 'PERID'  'X'            SPACE    SPACE
    SPACE        '6'      'Period'           SPACE    SPACE    SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_REPORT' 'KOSTL'  'X'            SPACE    SPACE
    SPACE        '11'      'Cost Center'        SPACE    SPACE    SPACE.


** Value
  PERFORM BUILD_FIELDCAT USING
    'IT_REPORT' 'COUNT'  SPACE            'X'    SPACE
    'IT_REPORT'   '15'      'Quantity'      SPACE   SPACE  SPACE .


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
FORM BUILD_FIELDCAT USING    VALUE(P_0100)
                             VALUE(P_0101)
                             VALUE(P_0102)
                             VALUE(P_0103)
                             VALUE(P_0104)
                             VALUE(P_0105)
                             VALUE(P_0106)
                             VALUE(P_0107)
                             VALUE(P_0108)
                             VALUE(P_0109)
                             VALUE(P_0110).

  ADD 1 TO GV_COL_POS.
  WA_FIELDCAT-TABNAME     = P_0100.
  WA_FIELDCAT-FIELDNAME   = P_0101.
  WA_FIELDCAT-KEY         = P_0102.
  WA_FIELDCAT-DO_SUM      = P_0103.
  WA_FIELDCAT-CFIELDNAME  = P_0104.
  WA_FIELDCAT-CTABNAME    = P_0105.
  WA_FIELDCAT-OUTPUTLEN   = P_0106.
  WA_FIELDCAT-SELTEXT_L   = P_0107.
  WA_FIELDCAT-DATATYPE    = P_0108.
  WA_FIELDCAT-QFIELDNAME  = P_0109.
  WA_FIELDCAT-QTABNAME    = P_0110.
  WA_FIELDCAT-COL_POS     = GV_COL_POS.
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

ENDFORM.                    " BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  CALL_ALV_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_ALV_LIST.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
     EXPORTING
*     I_INTERFACE_CHECK              = ' '
*     I_BYPASSING_BUFFER             =
*     I_BUFFER_ACTIVE                = ' '
       I_CALLBACK_PROGRAM             = GV_REPID
       I_CALLBACK_PF_STATUS_SET       = GV_STATUS
       I_CALLBACK_USER_COMMAND        = GV_USER_COMMAND
*     I_STRUCTURE_NAME               =
*     IS_LAYOUT                      =
       IT_FIELDCAT                    = IT_FIELDCAT[]
*     IT_EXCLUDING                   =
*     IT_SPECIAL_GROUPS              =
*       IT_SORT                        = IT_SORT[]        "
*     IT_FILTER                      =
*     IS_SEL_HIDE                    =
*     I_DEFAULT                      = 'X'
       I_SAVE                         = 'A'
*     IS_VARIANT                     =
       IT_EVENTS                      = IT_EVENTS     "
       IT_EVENT_EXIT                  = IT_EVENT_EXIT   "
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
       T_OUTTAB                       = IT_REPORT
       EXCEPTIONS
       PROGRAM_ERROR                  = 1
       OTHERS                         = 2.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " CALL_ALV_LIST

*-----------------------------------------------------------------------
*    FORM PF_STATUS
*-----------------------------------------------------------------------
FORM PF_STATUS USING  EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'BALVLIST' EXCLUDING EXTAB .
ENDFORM.

*---------------------------------------------------------------------*
*       FORM USER_COMMAND                                             *
*---------------------------------------------------------------------*
*       For User_command - AT User Command                            *
*---------------------------------------------------------------------*
*  -->  UCOMM                                                         *
*  -->  SELFIELD                                                      *
*---------------------------------------------------------------------*
FORM USER_COMMAND USING UCOMM    LIKE SY-UCOMM
                        SELFIELD TYPE SLIS_SELFIELD.
  CASE UCOMM.
* Important part !
* POST PLAN data to STD
    WHEN 'POST'.
      PERFORM POST_STD USING UCOMM.
  ENDCASE.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM BASIC_TOP_OF_PAGE                                        *
*---------------------------------------------------------------------*
*       Display Title                                                 *
*---------------------------------------------------------------------*
FORM BASIC_TOP_OF_PAGE.
  WRITE : / 'Controlling Area/          : '
            , P_KOKRS .
  WRITE : / 'Fiscal Year/Period/Version : '
            , P_GJAHR, '/', P_FRPER, '~', P_TOPER , '/', P_VERSN.
  WRITE : / 'SKF                        : ',
                   'CS001-No.of Persons by Team.' .

* CCTR
  IF P_NCOAL NE SPACE.
    WRITE : / 'Cost Center Grp.           : ', P_NCOAL.
  ENDIF.

*  IF NOT S_KOSTL[] IS INITIAL.
*    LOOP AT S_KOSTL.
*      AT FIRST.
*        WRITE : / 'Cost Center                : '.
*      ENDAT.
*      WRITE : / '                            ', S_KOSTL-LOW, '~',
*    S_KOSTL-HIGH.
*    ENDLOOP.
*  ENDIF.


  WRITE : / 'Test Run                     ', P_TRUN.
  SKIP 1.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  POST_STD
*&---------------------------------------------------------------------*
*       PREPARATION POSTING
*----------------------------------------------------------------------*
*      -->P_UCOMM  text
*----------------------------------------------------------------------*
FORM POST_STD USING    P_UCOMM.

* Init. Message TAB
  CLEAR : IT_RETURN, IT_RETURN[].

* Fill Header DATA
  CLEAR WA_HEADERINFO.
  WA_HEADERINFO-CO_AREA        = P_KOKRS.
  WA_HEADERINFO-FISC_YEAR      = P_GJAHR.
  WA_HEADERINFO-PERIOD_FROM	 = P_FRPER.
  WA_HEADERINFO-PERIOD_TO	 = P_TOPER.
  WA_HEADERINFO-VERSION        = P_VERSN.
* WA_HEADERINFO-DOC_HDR_TX	 =    .

* Fill Object List and Plan Values per Period
  CLEAR : IT_INDEXSTRUCTURE, IT_INDEXSTRUCTURE[].
  CLEAR : IT_COOBJECT,       IT_COOBJECT[].
  CLEAR : IT_PERVALUE,       IT_PERVALUE[].
  CLEAR : IT_TOTVALUE,       IT_TOTVALUE[].

* Sort to post data.
  SORT IT_POST BY KOSTL.

  LOOP AT IT_POST.
* Obj
    ON CHANGE OF  IT_POST-KOSTL.
* Index of Object Key
      IT_INDEXSTRUCTURE-OBJECT_INDEX
           = IT_INDEXSTRUCTURE-OBJECT_INDEX + 1 .

      CLEAR IT_COOBJECT.
      IT_COOBJECT-OBJECT_INDEX = IT_INDEXSTRUCTURE-OBJECT_INDEX.

      IT_COOBJECT-COSTCENTER   = IT_POST-KOSTL.

      APPEND IT_COOBJECT.
      CLEAR  IT_COOBJECT.
    ENDON.

* Value.
* Index of Value
    IT_INDEXSTRUCTURE-VALUE_INDEX
         = IT_INDEXSTRUCTURE-VALUE_INDEX + 1.

    CLEAR IT_PERVALUE.
    IT_PERVALUE-VALUE_INDEX = IT_INDEXSTRUCTURE-VALUE_INDEX.
** <  Posting , hard coding > **
* SKF   =  ' CS001 '
    IT_PERVALUE-STATKEYFIG     = 'CS001'.

* Set Value
* Post/Reverse
    IF  P_UCOMM = 'POST'.
      PERFORM SET_VALUE_AMT.
    ENDIF.

    APPEND IT_PERVALUE.
    CLEAR  IT_PERVALUE.

* append Index
    APPEND IT_INDEXSTRUCTURE.
    CLEAR IT_POST.
  ENDLOOP.

* Call BAPI FM
  PERFORM CALL_POST_FM.

* Commit
  IF P_TRUN = 'X'.
  ELSE.
    COMMIT WORK.
    MESSAGE S009(ZMCO) WITH P_UCOMM.

*       Consolidation management for the SKF
    CALL FUNCTION 'Z_FCO_MANAGEMENT_SKF'
      EXPORTING
        IM_PGMNO         =   SY-TCODE
        IM_KOKRS         =   P_KOKRS
        IM_GJAHR         =   P_GJAHR
        IM_PERBL         =   P_FRPER
        IM_PERBL_T       =   P_TOPER
        IM_VERSN         =   P_VERSN
*           IM_KOSTL_F        =    S_KOSTL-LOW
*           IM_KOSTL_T        =    S_KOSTL-HIGH
       IM_GNAME          =    P_NCOAL.
*           IM_PRZNR_F       =
*           IM_PRZNR_T       =
*         IMPORTING
*           SUBRC            =

  ENDIF.


ENDFORM.                    " POST_STD
*&---------------------------------------------------------------------*
*&      Form  SET_VALUE_AMT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_VALUE_AMT.

* Fixed Cost
* Variable Cost
  TRANS_VALUE 01. TRANS_VALUE 02. TRANS_VALUE 03. TRANS_VALUE 04.
  TRANS_VALUE 05. TRANS_VALUE 06. TRANS_VALUE 07. TRANS_VALUE 08.
  TRANS_VALUE 09. TRANS_VALUE 10. TRANS_VALUE 11. TRANS_VALUE 12.
  TRANS_VALUE 13. TRANS_VALUE 14. TRANS_VALUE 15. TRANS_VALUE 16.

ENDFORM.                    " SET_VALUE_AMT
*&---------------------------------------------------------------------*
*&      Form  CALL_POST_FM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_POST_FM.

  CALL FUNCTION 'BAPI_COSTACTPLN_POSTKEYFIGURE'
    EXPORTING
      HEADERINFO           = WA_HEADERINFO
*   DELTA                = ' '
    TABLES
      INDEXSTRUCTURE       = IT_INDEXSTRUCTURE
      COOBJECT             = IT_COOBJECT
     PERVALUE             = IT_PERVALUE
*   TOTVALUE             =
      RETURN               = IT_RETURN.

* Check error
  CLEAR  IT_RETURN.
  LOOP AT IT_RETURN  WHERE TYPE CA 'AE'.
    MESSAGE ID     IT_RETURN-ID
            TYPE   IT_RETURN-TYPE
            NUMBER IT_RETURN-NUMBER
            WITH   IT_RETURN-MESSAGE_V1
                   IT_RETURN-MESSAGE_V2
                   IT_RETURN-MESSAGE_V3
                   IT_RETURN-MESSAGE_V4.
    CLEAR IT_RETURN.
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
FORM CHK_INPUT_VALUE.

* Check Input Value (Period)
  IF P_FRPER > P_TOPER.
    MESSAGE E003(ZMCO) WITH P_FRPER P_TOPER.
  ENDIF.

  IF P_FRPER < 0 OR P_FRPER > 12.
    MESSAGE E007(ZMCO) WITH P_FRPER .
  ENDIF.

  IF P_TOPER < 0 OR P_TOPER > 12.
    MESSAGE E007(ZMCO) WITH P_TOPER.
  ENDIF.

* Check Cost Center Group
  IF P_NCOAL   IS INITIAL .
    MESSAGE E016(ZMCO).
  ENDIF.

** Check TEST-RUN  Flag
  IF P_TRUN NA 'X '.
    MESSAGE E008(ZMCO).
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
FORM POSTING_DATA.

  CLEAR : IT_POST, IT_POST[].
  CLEAR : IT_REPORT.
  LOOP AT IT_REPORT.
    MOVE-CORRESPONDING IT_REPORT TO IT_POST.

    IF IT_REPORT-PERID = '01'.
      IT_POST-VAR001 = IT_REPORT-COUNT.
    ELSEIF IT_REPORT-PERID = '02'.
      IT_POST-VAR002 = IT_REPORT-COUNT.
    ELSEIF IT_REPORT-PERID = '03'.
      IT_POST-VAR003 = IT_REPORT-COUNT.
    ELSEIF IT_REPORT-PERID = '04'.
      IT_POST-VAR004 = IT_REPORT-COUNT.
    ELSEIF IT_REPORT-PERID = '05'.
      IT_POST-VAR005 = IT_REPORT-COUNT.
    ELSEIF IT_REPORT-PERID = '06'.
      IT_POST-VAR006 = IT_REPORT-COUNT.
    ELSEIF IT_REPORT-PERID = '07'.
      IT_POST-VAR007 = IT_REPORT-COUNT.
    ELSEIF IT_REPORT-PERID = '08'.
      IT_POST-VAR008 = IT_REPORT-COUNT.
    ELSEIF IT_REPORT-PERID = '09'.
      IT_POST-VAR009 = IT_REPORT-COUNT.
    ELSEIF IT_REPORT-PERID = '10'.
      IT_POST-VAR010 = IT_REPORT-COUNT.
    ELSEIF IT_REPORT-PERID = '11'.
      IT_POST-VAR011 = IT_REPORT-COUNT.
    ELSEIF IT_REPORT-PERID = '12'.
      IT_POST-VAR012 = IT_REPORT-COUNT.
    ELSEIF IT_REPORT-PERID = '13'.
      IT_POST-VAR013 = IT_REPORT-COUNT.
    ELSEIF IT_REPORT-PERID = '14'.
      IT_POST-VAR014 = IT_REPORT-COUNT.
    ELSEIF IT_REPORT-PERID = '15'.
      IT_POST-VAR015 = IT_REPORT-COUNT.
    ELSEIF IT_REPORT-PERID = '16'.
      IT_POST-VAR016 = IT_REPORT-COUNT.
    ENDIF.

    COLLECT IT_POST.
    CLEAR IT_POST.
  ENDLOOP.
  CLEAR IT_POST.

  SORT IT_POST BY GJAHR KOSTL.


ENDFORM.                    " POSTING_DATA
*&---------------------------------------------------------------------*
*&      Form  READ_HEADCOUNT_PLAN
*&---------------------------------------------------------------------*
*       Read Head Count Plan data
*----------------------------------------------------------------------*
FORM READ_HEADCOUNT_PLAN.

  RS_PERID-SIGN = 'I'.
  RS_PERID-OPTION = 'BT'.
  RS_PERID-LOW = P_FRPER+1(2).
  RS_PERID-HIGH = P_TOPER+1(2).
  APPEND RS_PERID.   CLEAR RS_PERID.

*Issue Number : CO-20041115-010, Requested by DHKIM
*Changed on 2004/11/17, by WSKIM
*---Start
** Read Head Count Plan data
*  CLEAR : IT_ZTHR_AHC01, IT_ZTHR_AHC01[].
*  CLEAR ZTHR_AHC01.
*  SELECT *
*    APPENDING CORRESPONDING FIELDS OF TABLE IT_ZTHR_AHC01
*      FROM ZTHR_AHC01
*        FOR ALL ENTRIES IN IT_COSTCENTERLIST
*        WHERE  ZVERS = P_VERHR
*         AND   ZYEAR = P_GJAHR
*         AND   ZMONS IN RS_PERID
*         AND   ZCOST = IT_COSTCENTERLIST-VALFROM.
*  CLEAR IT_ZTHR_AHC01.
*
*
*  CLEAR : IT_SUM, IT_SUM[].
*  CLEAR   IT_ZTHR_AHC01.
*  LOOP AT IT_ZTHR_AHC01.
*    MOVE-CORRESPONDING IT_ZTHR_AHC01 TO IT_SUM.
*    COLLECT IT_SUM.
*    CLEAR   IT_SUM.
*  ENDLOOP.
*
*
*** Preparation reporting
*  CLEAR : IT_REPORT, IT_REPORT[].
*  LOOP AT IT_SUM.
*    CLEAR IT_LOW_COST.
*    READ TABLE IT_LOW_COST WITH KEY KOSTL = IT_SUM-ZSCST.
**    IT_REPORT-VERS   = IT_SUM-ZVERS.
*    IT_REPORT-VERS   = P_VERSN.
*    IT_REPORT-GJAHR  = IT_SUM-ZYEAR.
*    IT_REPORT-PERID  = IT_SUM-ZMONS.
*    IT_REPORT-KOSTL  = IT_LOW_COST-CGROUP.
*    IT_REPORT-COUNT  = IT_SUM-ZHEDC + IT_SUM-ZNEWC.
*    COLLECT IT_REPORT.
*    CLEAR   IT_REPORT.
*  ENDLOOP.
*  CLEAR IT_SUM.
*
*  SORT IT_REPORT BY VERS GJAHR PERID KOSTL .

** Read Head Count Plan data
  CLEAR : IT_ZTHR_AHC01, IT_ZTHR_AHC01[].
  SELECT *
    APPENDING CORRESPONDING FIELDS OF TABLE IT_ZTHR_AHC01
      FROM ZTHR_PCP00
        FOR ALL ENTRIES IN IT_COSTCENTERLIST
        WHERE  ZVERS = P_VERHR
         AND   ZYEAR = P_GJAHR
         AND   ZMONS IN RS_PERID
         AND   ZCOST = IT_COSTCENTERLIST-VALFROM.
  CLEAR IT_ZTHR_AHC01.

  CLEAR : IT_SUM, IT_SUM[].
  CLEAR   IT_ZTHR_AHC01.
  LOOP AT IT_ZTHR_AHC01.
    MOVE-CORRESPONDING IT_ZTHR_AHC01 TO IT_SUM.
    COLLECT IT_SUM.
    CLEAR   IT_SUM.
  ENDLOOP.

** Preparation reporting
  CLEAR : IT_REPORT, IT_REPORT[].
  LOOP AT IT_SUM.
    CLEAR IT_LOW_COST.
    READ TABLE IT_LOW_COST WITH KEY KOSTL = IT_SUM-ZCOST.
*    IT_REPORT-VERS   = IT_SUM-ZVERS.
    IT_REPORT-VERS   = P_VERSN.
    IT_REPORT-GJAHR  = IT_SUM-ZYEAR.
    IT_REPORT-PERID  = IT_SUM-ZMONS.
    IT_REPORT-KOSTL  = IT_LOW_COST-CGROUP.
    IT_REPORT-COUNT  = IT_SUM-ZHEDC + IT_SUM-ZNEWC.
    COLLECT IT_REPORT.
    CLEAR   IT_REPORT.
  ENDLOOP.
  CLEAR IT_SUM.

  SORT IT_REPORT BY VERS GJAHR PERID KOSTL .
*---End

ENDFORM.                    " READ_HEADCOUNT_PLAN
