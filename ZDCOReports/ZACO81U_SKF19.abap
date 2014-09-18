************************************************************************
* Program Name      : ZACO81U_SKF19
* Author            : Eun Hwa , Jung
* Creation Date     : 2004.04.07
* Specifications By : Dong-Hwan Kim
* Pattern           : Report 1-1
* Development Request No :  UD
* Addl Documentation:
* Description       : Create/Allocate Semi-direct activity qty(actual)

* the BDC structures for BATCH INPUT processing
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

REPORT  ZACO81U_SKF19 MESSAGE-ID ZMCO.

** type-pools
TYPE-POOLS: SLIS.

TABLES : CSKS, CATSDB , CSSL, COSL, COSS.

** For OBJECT KEY
DATA : BEGIN OF WA_OBJ ,
        OBJNR  LIKE  COSS-OBJNR,
        KOSTL  LIKE  CSKS-KOSTL,
        LSTAR  LIKE  CSLA-LSTAR,
       END OF WA_OBJ.

DATA : IT_OBJ_CCTR_AT   LIKE STANDARD TABLE OF WA_OBJ
                        WITH HEADER LINE .

* For DD data
DATA : GV_CI_TABNAME     TYPE DDOBJNAME .
DATA : IT_ET_FIELDLIST   LIKE TABLE OF RFVICP_DDIC_TABL_FIELDNAME
                         WITH HEADER LINE.

** Internal Table

DATA: BEGIN OF IT_COSL OCCURS 0,
        OBJNR TYPE COSL-OBJNR,
        GJAHR TYPE COSL-GJAHR,
        WRTTP TYPE COSL-WRTTP,
        VERSN TYPE COSL-VERSN,
      END OF IT_COSL.


DATA: BEGIN OF IT_TMP_COSL OCCURS 0,
        OBJNR  LIKE  COSS-OBJNR,
        KOSTL  LIKE  CSKS-KOSTL,
        LSTAR  LIKE  CSLA-LSTAR,
      END OF IT_TMP_COSL.

** Main internal table
DATA: BEGIN OF IT_CATSDB OCCURS 0,
*        pernr type catsdb-pernr,
*        workdate type catsdb-workdate,
        SKOSTL TYPE CATSDB-SKOSTL,
        LSTAR TYPE CATSDB-LSTAR,
        RKOSTL TYPE CATSDB-RKOSTL,
        CATSHOURS(9) TYPE P DECIMALS 2,
      END OF IT_CATSDB.
* sender + hours
DATA: BEGIN OF I_SKOSTL_SUM OCCURS 0,
        SKOSTL TYPE CATSDB-SKOSTL,
*        LSTAR TYPE CATSDB-LSTAR,
        CATSHOURS(9) TYPE P DECIMALS 2,
      END OF I_SKOSTL_SUM.

* for reporting
DATA: BEGIN OF IT_REPORT OCCURS 0,
        SKOSTL LIKE RK23B-SKOST,
        LSTAR  LIKE RK23B-LSTAR,
        OBJNR  LIKE  COSS-OBJNR,
        LVBSU LIKE RK23B-LVBSU,
      END OF IT_REPORT.


DATA: BEGIN OF IT_COSS OCCURS 0,
        LSTAR  LIKE  CSSL-LSTAR,
        KOSTL  LIKE  CSSL-KOSTL.
        INCLUDE STRUCTURE ZSCO_COSS_KEY01.
DATA : END OF   IT_COSS.

DATA : BEGIN OF IT_COSL2 OCCURS 500.
        INCLUDE STRUCTURE ZSCO_COSL_KEY01.
        INCLUDE STRUCTURE ZSCO_COSL_LST01.
DATA : END OF   IT_COSL2.

DATA: BEGIN OF IT_TMP_COSL2 OCCURS 0,
        GJAHR  LIKE  COSL-GJAHR,
        PERID  LIKE  COEJL-PERBL,
        OBJNR  LIKE  COSS-OBJNR,
        KOSTL  LIKE  CSKS-KOSTL,
        LSTAR  LIKE  CSLA-LSTAR,
        CURQTY LIKE  COSL-LST001,
        UNIT   LIKE  COSL-MEINH,
      END OF IT_TMP_COSL2.

DATA : BEGIN OF IT_SUM OCCURS 0,
           GJAHR  LIKE CSSL-GJAHR,
           PERID  LIKE COEJL-PERBL,
*           KOSTL LIKE CSSL-KOSTL,
*           LSTAR LIKE ZTCO_MHHRTRANS-LSTAR,
           OBJNR  LIKE CSSL-OBJNR,
           ACTQTY LIKE COSL-LST001,
       END OF IT_SUM.


* for reporting
DATA : BEGIN OF IT_REPORT2 OCCURS 0,
        GJAHR   LIKE  COSL-GJAHR,
        PERID   LIKE  COEJL-PERBL,
        KOSTL   LIKE  CSKS-KOSTL,
        OBJNR   LIKE  CSSL-OBJNR,
        LSTAR   LIKE  CSLA-LSTAR,
        R_KOSTL LIKE  CSSL-KOSTL,
        R_OBJNR LIKE  CSSL-OBJNR,
        R_LSTAR LIKE  CSSL-LSTAR,
        ACTQTY  LIKE  COSL-LST001,
        CURQTY  LIKE  COSL-LST001,
        VAEQTY  LIKE  COSL-LST001,
        UNIT    LIKE  COSL-MEINH,
        BLDAT   LIKE COHEADER-BLDAT,          " doc date
      END OF IT_REPORT2.

DATA : BEGIN OF IT_REPORT3 OCCURS 0,
        GJAHR   LIKE  COSL-GJAHR,
        PERID   LIKE  COEJL-PERBL,
        KOSTL   LIKE  CSKS-KOSTL,
        OBJNR   LIKE  CSSL-OBJNR,
        LSTAR   LIKE  CSLA-LSTAR,
        R_KOSTL LIKE  CSSL-KOSTL,
        R_OBJNR LIKE  CSSL-OBJNR,
        R_LSTAR LIKE  CSSL-LSTAR,
        ACTQTY  LIKE  COSL-LST001,
        CURQTY  LIKE  COSL-LST001,
        VAEQTY  LIKE  COSL-LST001,
        UNIT    LIKE  COSL-MEINH,
        BLDAT   LIKE COHEADER-BLDAT,          " doc date
      END OF IT_REPORT3.

* by ig.moon 4/21/2010 {
data: begin of it_coss_meg occurs 0,
        lstar  like  cssl-lstar,
        kostl  like  cssl-kostl.
        include structure zsco_coss_key01.
        include structure zsco_coss_meg01.
data : end of   it_coss_meg.
* }

** For BAPI
DATA : IT_COSTCENTERLIST LIKE STANDARD TABLE OF BAPI0012_CCLIST
                         WITH HEADER LINE.
DATA : IT_RETURN         LIKE STANDARD TABLE OF BAPIRET2
                         WITH HEADER LINE.

DATA : WA_DOC_HEADER LIKE BAPIDOCHDRP .
DATA : IT_DOC_ITEMS  LIKE STANDARD TABLE OF BAPIAAITM
                      WITH HEADER LINE.

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

* Global Variant
DATA : GV_POST_DATE LIKE COHEADER-BUDAT.
RANGES : RS_DATE FOR CATSDB-WORKDATE.
* Globale Daten
INCLUDE RPTBAL01.


*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-001.
PARAMETERS :
               P_KOKRS LIKE CSKS-KOKRS MEMORY ID CAC OBLIGATORY,
               P_GJAHR LIKE COBK-GJAHR MEMORY ID GJR OBLIGATORY,
               P_PERID LIKE COSP-PERBL MEMORY ID BPE OBLIGATORY,
               P_VERSN LIKE COBK-VERSN MEMORY ID KVT OBLIGATORY.
PARAMETERS : P_LSTAR LIKE CSLA-LSTAR            DEFAULT 'MAN_HR'
                                                     NO-DISPLAY,
             P_TRUN(1).
SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK BL3 WITH FRAME TITLE TEXT-003.
SELECT-OPTIONS : S_KOSTL  FOR CSKS-KOSTL. " DEFAULT 'MXTX11'.
PARAMETERS:      P_NCOAL LIKE GRPDYNP-NAME_COALL
                                   DEFAULT 'SEMIDIRECT'.
SELECTION-SCREEN END OF BLOCK BL3.
SELECTION-SCREEN END OF BLOCK BL1.

SELECTION-SCREEN BEGIN OF BLOCK BL3D WITH FRAME TITLE TEXT-0D3.
SELECT-OPTIONS : S_KOSTD FOR CSKS-KOSTL NO INTERVALS.
SELECTION-SCREEN END OF BLOCK BL3D.

*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
INITIALIZATION.
* Set Global ALV Parameter
  GV_REPID = SY-REPID.
  CLEAR : S_KOSTD[], S_KOSTD.

  S_KOSTD-SIGN = 'I'.
  S_KOSTD-OPTION = 'EQ'.
  S_KOSTD-LOW = '0000055103'.
  APPEND S_KOSTD.

  S_KOSTD-SIGN = 'I'.
  S_KOSTD-OPTION = 'EQ'.
  S_KOSTD-LOW = '0000055107'.
  APPEND S_KOSTD.


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
* preparation posting date.
  PERFORM CAL_POST_DATE.
* Read Cost Center Group - > Cost Center
  PERFORM READ_CEGRP.
* Read OBJ Key Combination
  PERFORM SET_OBJ_KEY.
* Read Sender CC_activity Information from COSL
  PERFORM READ_COSL_QTY.

**// Mod. By Hyung Jin Youn 2004.07.16.
** Change of Source data for Timesheet
* Read HR 'CATSDB'.
*  PERFORM READ_CATSDB.

* perform read_fr_catsdb2.
**// End of Mod.
  PERFORM READ_CO_MH.

* Calculate Working_hour.
  PERFORM CAL_WORKINGHOUR.


* Read Receiver.
  PERFORM READ_COSS_PAROB.
* Read Dynamic Fields Name
  PERFORM READ_FIELD_NAME_FROM_DD_COSL.
* Read Receiver_Quantity Information from COSL
  PERFORM READ_COSL2_QTY.
* Calculating Qty and preparation of reporting data.
  PERFORM CAL_PLAN_QTY.
* Preparation of ALV
  PERFORM PRE_REPORT_ADJ.

*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.
* Call ALV LIST
  PERFORM CALL_ALV_LIST.

*&---------------------------------------------------------------------*
*&      Form  READ_CEGRP
*&---------------------------------------------------------------------*
*       read cost center
*----------------------------------------------------------------------*
FORM READ_CEGRP.

* Making an internal table for CCtr to select data
* Selected Group on screen
  CLEAR : IT_COSTCENTERLIST, IT_COSTCENTERLIST[].
  CLEAR : IT_RETURN, IT_RETURN[].

* Set Validity Date (Start)
  DATA : LV_DATUM LIKE SY-DATUM.
  CONCATENATE P_GJAHR P_PERID+1(2) '01' INTO LV_DATUM.

* From CCtr Group
  IF NOT P_NCOAL IS INITIAL.
    CALL FUNCTION 'BAPI_COSTCENTER_GETLIST1'
         EXPORTING
              CONTROLLINGAREA = P_KOKRS
              DATE_FROM       = LV_DATUM
              COSTCENTERGROUP = P_NCOAL
         TABLES
              COSTCENTERLIST  = IT_COSTCENTERLIST
              RETURN          = IT_RETURN.

* Message
    PERFORM DIS_BAPI_MESSAGE.
  ENDIF.

* From CCtrs on selection screen
* From Select-options.
  DATA : IT_L_CCTR LIKE STANDARD TABLE OF IT_COSTCENTERLIST
                 WITH HEADER LINE.

  IF NOT S_KOSTL[] IS INITIAL.
    LOOP AT S_KOSTL.
      CLEAR : IT_L_CCTR, IT_L_CCTR[].
      CALL FUNCTION 'BAPI_COSTCENTER_GETLIST1'
           EXPORTING
                CONTROLLINGAREA = P_KOKRS
                DATE_FROM       = LV_DATUM
                COSTCENTER_FROM = S_KOSTL-LOW
                COSTCENTER_TO   = S_KOSTL-HIGH
           TABLES
                COSTCENTERLIST  = IT_L_CCTR
                RETURN          = IT_RETURN.
* Message
      PERFORM DIS_BAPI_MESSAGE.
* Appending CE list
      APPEND LINES OF IT_L_CCTR  TO IT_COSTCENTERLIST.
      CLEAR IT_COSTCENTERLIST.
      CLEAR S_KOSTL.
    ENDLOOP.
  ENDIF.

  CLEAR IT_COSTCENTERLIST.
* Except costcenter = '55103'.
* by ig.moon 8/4/2009 {
*  delete it_costcenterlist where costcenter = '0000055103'.

  LOOP AT S_KOSTD.
    DELETE IT_COSTCENTERLIST WHERE COSTCENTER = S_KOSTD-LOW.
  ENDLOOP.

*}

* Sorting
  SORT IT_COSTCENTERLIST BY CO_AREA COSTCENTER.
  DELETE ADJACENT DUPLICATES FROM IT_COSTCENTERLIST.

* Check CCtr
  IF IT_COSTCENTERLIST[] IS INITIAL .
    MESSAGE E018(ZMCO) WITH 'Cost Center'.
  ENDIF.


ENDFORM.                    " READ_CEGRP
*&---------------------------------------------------------------------*
*&      Form  DIS_BAPI_MESSAGE
*&---------------------------------------------------------------------*
*        Display BAPI Message
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
*&      Form  READ_CATSDB
*&---------------------------------------------------------------------*
*       read ' HR time sheet' information
*----------------------------------------------------------------------*
FORM READ_CATSDB.

* Read time sheet data
  CLEAR : IT_CATSDB, IT_CATSDB[].
  CLEAR CATSDB.
  SELECT *
         INTO CORRESPONDING FIELDS OF TABLE IT_CATSDB
         FROM CATSDB
          FOR ALL ENTRIES IN IT_TMP_COSL
          WHERE WORKDATE IN RS_DATE
           AND SKOSTL = IT_TMP_COSL-KOSTL
           AND ( RKOSTL = IT_TMP_COSL-KOSTL OR RKOSTL = ' ' )
           AND KOKRS = P_KOKRS
           AND STATUS = '30'.    " Processing status = 30 :'Approved'
  "
  CLEAR  IT_CATSDB.

ENDFORM.                    " READ_CATSDB
*-----------------------------------------------------------------------
*    FORM PF_STATUS
*-----------------------------------------------------------------------
FORM PF_STATUS USING  EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'LIST' EXCLUDING EXTAB .
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
    WHEN 'POST'.
      PERFORM POST_STD  USING UCOMM.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PRE_REPORT_ADJ
*&---------------------------------------------------------------------*
*       The Preparation for ALV List
*----------------------------------------------------------------------*
FORM PRE_REPORT_ADJ.

* Building Field Cat.
  PERFORM FIELDCAT_INIT .

* Sort
  SORT IT_REPORT3 BY GJAHR PERID KOSTL LSTAR R_KOSTL R_LSTAR.
  CLEAR IT_REPORT3.

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
  IT_SORT-FIELDNAME = 'KOSTL'.
  IT_SORT-UP        = 'X'.
  IT_SORT-EXPA      = 'X'.
  IT_SORT-SUBTOT    = 'X'.
  APPEND IT_SORT.
  IT_SORT-FIELDNAME = 'LSTAR'.
  IT_SORT-UP        = 'X'.
  IT_SORT-EXPA      = 'X'.
  IT_SORT-SUBTOT    = 'X'.
  APPEND IT_SORT.



ENDFORM.                    " PRE_REPORT_ADJ
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_INIT
*&---------------------------------------------------------------------*
*       Building Field Cat
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FIELDCAT_INIT.

  CLEAR : GV_COL_POS, IT_FIELDCAT, IT_FIELDCAT[].

* Key
*  PERFORM BUILD_FIELDCAT USING
*    'IT_REPORT2' 'GJAHR'  'X'            SPACE    SPACE
*    SPACE        '4'      'Year'           SPACE    SPACE    SPACE.
*
*  PERFORM BUILD_FIELDCAT USING
*    'IT_REPORT2' 'PERID'  'X'            SPACE    SPACE
*    SPACE        '6'      'Period'           SPACE    SPACE    SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_REPORT3' 'KOSTL'  'X'            SPACE    SPACE
    SPACE        '10'      'Sen. C/C'      SPACE    SPACE    SPACE.

*  PERFORM BUILD_FIELDCAT USING
*    'IT_REPORT' 'OBJNR'  'X'            SPACE    SPACE
*    SPACE        '22'      'OBJNR'           SPACE    SPACE    SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_REPORT3' 'LSTAR'  'X'            SPACE    SPACE
    SPACE        '6'      'Sen.AT'           SPACE    SPACE    SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_REPORT3' 'R_KOSTL' 'X'            SPACE    SPACE
    SPACE        '10'      'Rec. C/C'       SPACE    SPACE    SPACE.

*  PERFORM BUILD_FIELDCAT USING
*    'IT_REPORT' 'R_OBJNR'  'X'            SPACE    SPACE
*    SPACE        '22'      'R_OBJNR'           SPACE    SPACE    SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_REPORT3' 'R_LSTAR'  'X'            SPACE    SPACE
    SPACE        '6'      'Rec.AT'           SPACE    SPACE    SPACE.

** Value

*  PERFORM BUILD_FIELDCAT USING
*    'IT_REPORT2' 'UNIT'  SPACE            SPACE    SPACE
*    SPACE   '4'      'Unit'      'UNIT'   SPACE     SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_REPORT3' 'ACTQTY'  SPACE            SPACE    SPACE
    SPACE   '15'      'Rec.Actual Qty'  SPACE    SPACE SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_REPORT3' 'CURQTY'  SPACE            SPACE    SPACE
    SPACE '15'      'Sen.Actual Qty'    SPACE    SPACE SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_REPORT3' 'VAEQTY'  SPACE            'X'    SPACE
    'IT_REPORT'   '15'      'Rec.Input Qty'      SPACE   SPACE  SPACE .


* Set Event
  DATA : WA_L_EVENT  TYPE SLIS_ALV_EVENT.
  WA_L_EVENT-NAME = SLIS_EV_TOP_OF_PAGE.
  WA_L_EVENT-FORM = 'BASIC_TOP_OF_PAGE'.
  APPEND WA_L_EVENT TO IT_EVENTS.


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
FORM BUILD_FIELDCAT USING   VALUE(P_0100)
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
*       CALL ALV LIST
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
       IT_SORT                        = IT_SORT[]
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
       T_OUTTAB                       = IT_REPORT3
       EXCEPTIONS
       PROGRAM_ERROR                  = 1
       OTHERS                         = 2.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.




ENDFORM.                    " CALL_ALV_LIST

*---------------------------------------------------------------------*
*       FORM BASIC_TOP_OF_PAGE                                        *
*---------------------------------------------------------------------*
*       Display Title                                                 *
*---------------------------------------------------------------------*
FORM BASIC_TOP_OF_PAGE.

  WRITE : / 'Controlling Area      : ', P_KOKRS .
  WRITE : / 'Fiscal Year/Period    : '
            , P_GJAHR, '/', P_PERID.
  WRITE : / 'Version               : ', P_VERSN .
  WRITE : / 'Posting Date          : ', GV_POST_DATE.


* CCTR
  IF P_NCOAL NE SPACE.
    WRITE : / 'Cost Center Group     : ', P_NCOAL.
  ENDIF.

  IF NOT S_KOSTL[] IS INITIAL.
    LOOP AT S_KOSTL.
      AT FIRST.
        WRITE : / 'Cost Center           :     '.
      ENDAT.
      WRITE : / '                        ', S_KOSTL-LOW, '~',
S_KOSTL-HIGH.
    ENDLOOP.
  ENDIF.
  WRITE : / 'Test Run                     ', P_TRUN.
  SKIP 1.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CAL_POST_DATE
*&---------------------------------------------------------------------*
*       calculate posting date
*----------------------------------------------------------------------*
FORM CAL_POST_DATE.

*** <  Posting , hard coding > **
* doc date / posting date   =  ' year/month/25 '
  CLEAR GV_POST_DATE.
  CONCATENATE  P_GJAHR  P_PERID+1(2) '25'  INTO GV_POST_DATE.


**--- CREATE RANGES FOR WORKDATE.
  DATA :  DAT1 LIKE SY-DATUM,
          DAT2 LIKE SY-DATUM.

  CLEAR : DAT1, DAT2.

  CONCATENATE P_GJAHR P_PERID+1(2) '01' INTO DAT1.
  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
       EXPORTING
            DAY_IN            = DAT1
       IMPORTING
            LAST_DAY_OF_MONTH = DAT2.

  RS_DATE-SIGN = 'I'.
  RS_DATE-OPTION = 'BT'.
  RS_DATE-LOW = DAT1.
  RS_DATE-HIGH = DAT2.
  APPEND RS_DATE.   CLEAR RS_DATE.


ENDFORM.                    " CAL_POST_DATE
*&---------------------------------------------------------------------*
*&      Form  CHK_INPUT_VALUE
*&---------------------------------------------------------------------*
*       CHK_INPUT_VALUE
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHK_INPUT_VALUE.

  IF P_PERID < 0 OR P_PERID > 12.
    MESSAGE E007(ZMCO) WITH P_PERID .
  ENDIF.


* Check Cost Center/Cost Center Group
  IF     S_KOSTL[] IS INITIAL
     AND P_NCOAL   IS INITIAL .
    MESSAGE E016(ZMCO).
  ELSEIF
         NOT S_KOSTL[] IS INITIAL
     AND NOT P_NCOAL   IS INITIAL .
    MESSAGE E017(ZMCO).
  ENDIF.


ENDFORM.                    " CHK_INPUT_VALUE
*&---------------------------------------------------------------------*
*&      Form  CAL_WORKINGHOUR
*&---------------------------------------------------------------------*
*       Calculate Working_hour.
*----------------------------------------------------------------------*
FORM CAL_WORKINGHOUR.

  SORT IT_CATSDB BY SKOSTL.

  LOOP AT IT_CATSDB.
    IF IT_CATSDB-RKOSTL = ' '.
      MOVE-CORRESPONDING IT_CATSDB TO I_SKOSTL_SUM.
    ELSE.
      MOVE IT_CATSDB-RKOSTL    TO I_SKOSTL_SUM-SKOSTL.
      MOVE IT_CATSDB-CATSHOURS TO I_SKOSTL_SUM-CATSHOURS.
    ENDIF.
    COLLECT I_SKOSTL_SUM.
    CLEAR I_SKOSTL_SUM.
  ENDLOOP.
  CLEAR IT_CATSDB.


* preparation report.
  LOOP AT I_SKOSTL_SUM.
    CLEAR IT_TMP_COSL.
    READ TABLE IT_TMP_COSL WITH KEY KOSTL = I_SKOSTL_SUM-SKOSTL.
    MOVE IT_TMP_COSL-OBJNR          TO IT_REPORT-OBJNR.
    MOVE IT_TMP_COSL-LSTAR          TO IT_REPORT-LSTAR.
    MOVE I_SKOSTL_SUM-CATSHOURS     TO IT_REPORT-LVBSU.
    MOVE-CORRESPONDING I_SKOSTL_SUM TO IT_REPORT.

    APPEND  IT_REPORT.
    CLEAR   IT_REPORT.

  ENDLOOP.

  CLEAR I_SKOSTL_SUM.
  CLEAR IT_REPORT.


ENDFORM.                    " CAL_WORKINGHOUR
*&---------------------------------------------------------------------*
*&      Form  SET_OBJ_KEY
*&---------------------------------------------------------------------*
*      Read OBJ Key Combination
*----------------------------------------------------------------------*
FORM SET_OBJ_KEY.

  CLEAR : IT_OBJ_CCTR_AT, IT_OBJ_CCTR_AT[].

* CCtr  - > objnr , kostl , lstar
  CLEAR CSSL.
  SELECT  OBJNR KOSTL LSTAR
                      INTO CORRESPONDING FIELDS OF TABLE IT_OBJ_CCTR_AT
                      FROM CSSL
                     FOR ALL ENTRIES IN IT_COSTCENTERLIST
                     WHERE KOKRS = P_KOKRS
                       AND KOSTL = IT_COSTCENTERLIST-COSTCENTER
                       AND GJAHR = P_GJAHR.

  CLEAR : IT_OBJ_CCTR_AT.

ENDFORM.                    " SET_OBJ_KEY
*&---------------------------------------------------------------------*
*&      Form  READ_COSL_QTY
*&---------------------------------------------------------------------*
*       Read Sender CC_activity Information from COSL
*----------------------------------------------------------------------*
FORM READ_COSL_QTY.

  CLEAR : IT_COSL, IT_COSL[].
  CLEAR COSL.
  SELECT *
         INTO CORRESPONDING FIELDS OF TABLE IT_COSL
         FROM COSL
          FOR ALL ENTRIES IN IT_OBJ_CCTR_AT
        WHERE LEDNR = '00'
          AND OBJNR = IT_OBJ_CCTR_AT-OBJNR
          AND GJAHR = P_GJAHR
          AND WRTTP = '01'             " plan : '01'
          AND VERSN = P_VERSN.
  CLEAR : IT_COSL.


  CLEAR : IT_TMP_COSL, IT_TMP_COSL[].
  LOOP AT IT_COSL.
    CLEAR IT_OBJ_CCTR_AT.
    READ TABLE    IT_OBJ_CCTR_AT
         WITH KEY OBJNR = IT_COSL-OBJNR.
    IT_TMP_COSL-KOSTL = IT_OBJ_CCTR_AT-KOSTL.
    IT_TMP_COSL-LSTAR = IT_OBJ_CCTR_AT-LSTAR.
    IT_TMP_COSL-OBJNR = IT_OBJ_CCTR_AT-OBJNR.
* Collect
    COLLECT IT_TMP_COSL.
    CLEAR   IT_TMP_COSL.
  ENDLOOP.
  CLEAR IT_TMP_COSL.


ENDFORM.                    " READ_COSL_QTY
*&---------------------------------------------------------------------*
*&      Form  READ_COSS_PAROB
*&---------------------------------------------------------------------*
*      Read Receiver.
*----------------------------------------------------------------------*
FORM READ_COSS_PAROB.

* by ig.moon 4/21/2010 {
  clear : it_coss_meg, it_coss_meg[].
  select *
         into corresponding fields of table it_coss_meg
         from coss
          for all entries in IT_REPORT
        where lednr = '00'
          and objnr = IT_REPORT-objnr      "IT_COSR-OBJNR
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
  CLEAR : IT_COSS, IT_COSS[].
  CLEAR IT_REPORT.
  CLEAR COSS.
  SELECT *
         INTO CORRESPONDING FIELDS OF TABLE IT_COSS
         FROM COSS
          FOR ALL ENTRIES IN IT_REPORT
        WHERE LEDNR = '00'
          AND OBJNR = IT_REPORT-OBJNR
          AND GJAHR = P_GJAHR
          AND WRTTP = '01'        " plan
          AND VERSN = P_VERSN
          AND VRGNG = 'RKP7'.
*Issue number
*Requested by dhkim,20041007,changed by wskim
*---Start
*         and meg009 ne 0.
*---End
  CLEAR IT_COSS.

  DELETE IT_COSS WHERE PAROB = ' '.

*  DATA : LV_LSTAR LIKE CSSL-LSTAR,
*         LV_KOSTL LIKE CSSL-KOSTL.
*  CLEAR : LV_LSTAR, LV_KOSTL.

  LOOP AT IT_COSS.
*    CLEAR : LV_LSTAR, LV_KOSTL.
*    CALL FUNCTION 'OBJECT_KEY_GET_KL'
*      EXPORTING
*        OBJNR             = IT_COSS-PAROB
*      IMPORTING
**     KOKRS             =
*       KOSTL             =  LV_KOSTL
*       LSTAR             =  LV_LSTAR      .
*
*       IF SY-SUBRC <> 0.
*         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*       ENDIF.

    IT_COSS-KOSTL = IT_COSS-PAROB+6(10).
    IT_COSS-LSTAR = IT_COSS-PAROB+16(6).
    MODIFY IT_COSS.
    CLEAR  IT_COSS.
  ENDLOOP.

  DELETE IT_COSS WHERE LSTAR NE 'MAN_HR'.


ENDFORM.                    " READ_COSS_PAROB
*&---------------------------------------------------------------------*
*&      Form  READ_COSL2_QTY
*&---------------------------------------------------------------------*
*      Read Receiver_Quantity Information from COSL
*----------------------------------------------------------------------*
FORM READ_COSL2_QTY.

  CLEAR : IT_COSL2, IT_COSL2[].
  CLEAR IT_COSS.
  CLEAR COSL.
  SELECT (IT_ET_FIELDLIST)
         INTO CORRESPONDING FIELDS OF TABLE IT_COSL2
         FROM COSL
          FOR ALL ENTRIES IN IT_COSS
        WHERE LEDNR = '00'
          AND OBJNR = IT_COSS-PAROB
          AND GJAHR = P_GJAHR
          AND WRTTP = '04'
          AND VERSN = P_VERSN.
  CLEAR : IT_COSL2.


* Local Data definition
  FIELD-SYMBOLS: <FS2> TYPE ANY.
  DATA : LV_LST_NAM2(30).
  DATA : LV_CNT2  LIKE  COSP-PERBL.
*
  CLEAR   IT_COSL2.
  CLEAR : IT_TMP_COSL2, IT_TMP_COSL2[].

  LOOP AT IT_COSL2.
* Key Part
    IT_TMP_COSL2-GJAHR = P_GJAHR.
    IT_TMP_COSL2-PERID = P_PERID.

    IT_TMP_COSL2-KOSTL = IT_COSL2-OBJNR+6(6).
*    IT_TMP_COSL2-LSTAR = IT_COSL2-OBJNR+16(6).
    IT_TMP_COSL2-OBJNR = IT_COSL2-OBJNR.
    IT_TMP_COSL2-UNIT = IT_COSL2-MEINH.

* Value Transferring
    CLEAR LV_LST_NAM2.
    CONCATENATE 'IT_COSL2-'  'LST'  P_PERID
          INTO LV_LST_NAM2.
    ASSIGN (LV_LST_NAM2) TO <FS2>.
    CLEAR IT_TMP_COSL2-CURQTY.
    IT_TMP_COSL2-CURQTY = <FS2>.

*   Unit Conversion
    IF IT_TMP_COSL2-UNIT <> 'STD'.
      PERFORM UNIT_CONV USING IT_TMP_COSL2-UNIT
                              IT_TMP_COSL2-CURQTY.
    ENDIF.

* Collect
    COLLECT IT_TMP_COSL2.
    CLEAR IT_TMP_COSL2.
    CLEAR IT_COSL2.
  ENDLOOP.
  CLEAR IT_TMP_COSL2.
  CLEAR IT_COSL2.


ENDFORM.                    " READ_COSL2_QTY
*&---------------------------------------------------------------------*
*&      Form  CAL_PLAN_QTY
*&---------------------------------------------------------------------*
*      Calculating Qty and preparation of reporting data.
*----------------------------------------------------------------------*
FORM CAL_PLAN_QTY.

* by ig.moon 3/30/2010 {
  field-symbols: <fs3> type any.
  data : lv_meg_nam2(30).
  sort it_coss_meg by gjahr kostl objnr.
* }

*Making an it_report for reporting
  CLEAR : IT_REPORT2, IT_REPORT2[].
  CLEAR : IT_REPORT, IT_COSS.
  LOOP AT IT_COSS.
    LOOP AT IT_REPORT WHERE OBJNR = IT_COSS-OBJNR.

      IT_REPORT2-GJAHR   = P_GJAHR.
      IT_REPORT2-PERID   = P_PERID.
      IT_REPORT2-KOSTL   = IT_REPORT-SKOSTL.
      IT_REPORT2-OBJNR   = IT_REPORT-OBJNR.
      IT_REPORT2-LSTAR   = IT_REPORT-LSTAR.
      IT_REPORT2-R_KOSTL = IT_COSS-KOSTL.
      IT_REPORT2-R_OBJNR = IT_COSS-PAROB.
      IT_REPORT2-R_LSTAR = IT_COSS-LSTAR.
      IT_REPORT2-CURQTY  = IT_REPORT-LVBSU.
      CONCATENATE P_GJAHR P_PERID+1(2) '25'
                INTO IT_REPORT2-BLDAT.


* by ig.moon 3/30/2010 {
      read table it_coss_meg with key gjahr = it_report2-gjahr
                                      kostl = IT_COSS-kostl
                                      objnr = it_report2-objnr
                                      binary search.
      if sy-subrc eq 0.
        clear lv_meg_nam2.
        concatenate 'IT_COSS_MEG'  '-MEG'  it_report2-perid
               into lv_meg_nam2.
        assign (lv_meg_nam2) to <fs3>.

        if <fs3> eq '0.000'.
          clear it_report2-vaeqty.
        else.
          append it_report2.
          clear  it_report2.
        endif.
      endif.
* }

*      APPEND IT_REPORT2.
*      CLEAR IT_REPORT2.

    ENDLOOP.
  ENDLOOP.


  CLEAR IT_REPORT2.
  LOOP AT IT_REPORT2.
    CLEAR IT_TMP_COSL2.
    READ TABLE    IT_TMP_COSL2
       WITH KEY GJAHR = IT_REPORT2-GJAHR
                PERID = IT_REPORT2-PERID
                OBJNR = IT_REPORT2-R_OBJNR.
*                KOSTL = IT_REPORT2-R_KOSTL.

    IT_REPORT2-ACTQTY = IT_TMP_COSL2-CURQTY.
    MODIFY IT_REPORT2.
    CLEAR  IT_REPORT2.
  ENDLOOP.
  SORT  IT_REPORT2 BY GJAHR PERID KOSTL OBJNR.

  CLEAR : IT_SUM, IT_SUM[].
  LOOP AT IT_REPORT2.
    MOVE-CORRESPONDING IT_REPORT2 TO IT_SUM.
    COLLECT IT_SUM.
    CLEAR   IT_SUM.
  ENDLOOP.
  CLEAR IT_SUM.

* Calculate PLAN QTY.
  DATA   TOT_QTY(9) TYPE P DECIMALS 3.
  DATA   FLAG(1).
  CLEAR  TOT_QTY.

  CLEAR : IT_REPORT3, IT_REPORT3[].
  CLEAR   IT_REPORT2.
  LOOP AT IT_REPORT2.

    AT NEW OBJNR.
      CLEAR : TOT_QTY, FLAG, IT_SUM.
      READ TABLE  IT_SUM
           WITH KEY GJAHR = IT_REPORT2-GJAHR
                    PERID = IT_REPORT2-PERID
                    OBJNR = IT_REPORT2-OBJNR.
*                  KOSTL = IT_REPORT2-KOSTL
*                  LSTAR = IT_REPORT2-LSTAR.
    ENDAT.

    IF IT_SUM-ACTQTY <> 0.
      IT_REPORT2-VAEQTY = IT_REPORT2-CURQTY *
                          ( IT_REPORT2-ACTQTY / IT_SUM-ACTQTY ).
      TOT_QTY = TOT_QTY +  IT_REPORT2-VAEQTY.
    ENDIF.

    MOVE-CORRESPONDING IT_REPORT2 TO IT_REPORT3.

    AT END OF OBJNR.
      FLAG = 'Y'.
    ENDAT.

    IF FLAG = 'Y'.
      IT_REPORT3-VAEQTY = ( IT_REPORT3-CURQTY - TOT_QTY )
                               + IT_REPORT3-VAEQTY.
    ENDIF.


    APPEND IT_REPORT3.
    CLEAR  IT_REPORT3.
  ENDLOOP.
  CLEAR IT_REPORT3.


ENDFORM.                    " CAL_PLAN_QTY
*&---------------------------------------------------------------------*
*&      Form  READ_FIELD_NAME_FROM_DD_COSL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_FIELD_NAME_FROM_DD_COSL.

  CLEAR : IT_ET_FIELDLIST, IT_ET_FIELDLIST[].

* read DD infor. COSL Key Part
  PERFORM READ_DD_INFO  TABLES IT_ET_FIELDLIST
                        USING  'ZSCO_COSL_KEY01'.

* read DD infor. COSL Value Part (Total Quantity)
  PERFORM READ_DD_INFO  TABLES IT_ET_FIELDLIST
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
FORM READ_DD_INFO TABLES   IT_L_ET_FIELDLIST STRUCTURE IT_ET_FIELDLIST
                  USING    P_CI_TABNAME      LIKE GV_CI_TABNAME.
* Local DATA definition
  DATA : IT_L_FDLIST LIKE STANDARD TABLE OF IT_ET_FIELDLIST
                     WITH HEADER LINE.
* Making FDlist
  CLEAR : IT_L_FDLIST,     IT_L_FDLIST[].
  CLEAR GV_CI_TABNAME.
  GV_CI_TABNAME = P_CI_TABNAME.
  CALL FUNCTION 'RECP_DD_TABL_FIELDNAMES_GET'
       EXPORTING
            IC_TABNAME   = GV_CI_TABNAME
       TABLES
            ET_FIELDLIST = IT_L_FDLIST
       EXCEPTIONS
            NOT_FOUND    = 1
            OTHERS       = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  APPEND LINES OF  IT_L_FDLIST       TO IT_L_ET_FIELDLIST.

ENDFORM.                    " READ_DD_INFO
*&---------------------------------------------------------------------*
*&      Form  POST_STD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_UCOMM  text
*----------------------------------------------------------------------*
FORM POST_STD USING    P_UCOMM.

  DELETE IT_REPORT3 WHERE VAEQTY EQ '0'.

  DATA  : LINE  TYPE I.
  CLEAR : LINE.
  DESCRIBE TABLE IT_REPORT3 LINES LINE.

  IF LINE = 0.
    MESSAGE E000(ZMCO) WITH
    ' Enter Value not equal to 0 '.
  ENDIF.


* Init. Message TAB
  CLEAR : IT_RETURN, IT_RETURN[].

  DATA : LV_CONF_TEXT(50).
* TEXT
  CLEAR LV_CONF_TEXT.
  CONCATENATE SY-UNAME  SY-DATUM  SY-REPID
         INTO LV_CONF_TEXT
         SEPARATED BY '/'.

* Fill Header DATA _ kb21np
  CLEAR WA_DOC_HEADER.
  WA_DOC_HEADER-CO_AREA           = P_KOKRS.
  WA_DOC_HEADER-DOCDATE           = IT_REPORT3-BLDAT.
  WA_DOC_HEADER-POSTGDATE         = IT_REPORT3-BLDAT.
  WA_DOC_HEADER-VERSION           = P_VERSN.
  WA_DOC_HEADER-VARIANT           = 'SAP01'.
  WA_DOC_HEADER-DOC_HDR_TX        = LV_CONF_TEXT.
  WA_DOC_HEADER-USERNAME          = SY-UNAME.

* Fill Object List
  CLEAR : IT_DOC_ITEMS, IT_DOC_ITEMS[].

  LOOP AT IT_REPORT3.
    IT_DOC_ITEMS-SEND_CCTR  = IT_REPORT3-KOSTL.
    IT_DOC_ITEMS-ACTTYPE    = IT_REPORT3-LSTAR.
    IT_DOC_ITEMS-REC_CCTR   = IT_REPORT3-R_KOSTL.
    IT_DOC_ITEMS-ACTVTY_QTY = IT_REPORT3-VAEQTY.
    IT_DOC_ITEMS-ACTIVITYUN = 'STD'.
    APPEND IT_DOC_ITEMS.
  ENDLOOP.

* Call BAPI FM
  PERFORM CALL_POST_FM.                                     " KB21NP

* Commit
  IF P_TRUN = 'X'.
    READ TABLE IT_RETURN  INDEX 1.
    MESSAGE S000(ZMCO) WITH IT_RETURN-MESSAGE.
  ELSE.
    COMMIT WORK.
    READ TABLE IT_RETURN  INDEX 1.
    MESSAGE S000(ZMCO) WITH IT_RETURN-MESSAGE.
*    MESSAGE S009(ZMCO) WITH P_UCOMM.

*       Consolidation management for the SKF
    CALL FUNCTION 'Z_FCO_MANAGEMENT_SKF'
      EXPORTING
        IM_PGMNO         =   SY-TCODE
        IM_KOKRS         =   P_KOKRS
        IM_GJAHR         =   P_GJAHR
        IM_PERBL         =   P_PERID
*           IM_PERBL_T       =
        IM_VERSN         =   P_VERSN
       IM_KOSTL_F        =   S_KOSTL-LOW
       IM_KOSTL_T        =   S_KOSTL-HIGH
       IM_GNAME          =    P_NCOAL.
*           IM_PRZNR_F       =   S_PRZNR-LOW
*           IM_PRZNR_T       =   S_PRZNR-HIGH.
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
FORM CALL_POST_FM.

  CALL FUNCTION 'BAPI_ACC_ACTIVITY_ALLOC_POST'
      EXPORTING
        DOC_HEADER            = WA_DOC_HEADER
*   IGNORE_WARNINGS       = ' '
* IMPORTING
*   DOC_NO                =
      TABLES
        DOC_ITEMS             = IT_DOC_ITEMS
        RETURN                = IT_RETURN.
*   CRITERIA              =

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
*&      Form  UNIT_CONV
*&---------------------------------------------------------------------*
*       Unit Conversion
*----------------------------------------------------------------------*
*      -->P_UNIT  UNIT
*      -->P_QTY   Quantity
*----------------------------------------------------------------------*
FORM UNIT_CONV USING    P_UNIT
                        P_QTY.
  CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
   EXPORTING
     INPUT                      = P_QTY
*    NO_TYPE_CHECK              = 'X'
     ROUND_SIGN                 = 'X'
     UNIT_IN                    = P_UNIT
     UNIT_OUT                   = 'STD'
   IMPORTING
*    ADD_CONST                  =
*    DECIMALS                   =
*    DENOMINATOR                =
*    NUMERATOR                  =
     OUTPUT                     = P_QTY
   EXCEPTIONS
     CONVERSION_NOT_FOUND       = 1
     DIVISION_BY_ZERO           = 2
     INPUT_INVALID              = 3
     OUTPUT_INVALID             = 4
     OVERFLOW                   = 5
     TYPE_INVALID               = 6
     UNITS_MISSING              = 7
     UNIT_IN_NOT_FOUND          = 8.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  P_UNIT = 'STD'.

ENDFORM.                    " UNIT_CONV

*&---------------------------------------------------------------------*
*&      Form  READ_FR_CATSDB2
*&---------------------------------------------------------------------*
*       Read TIMESHEET data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_FR_CATSDB2.

* Ingnore A/A type
* HR source has no indicator to check if supportive hour or
* non-supportive hour

  DATA : PNPBEGDA LIKE QPPNP-BEGDA,
         PNPENDDA LIKE QPPNP-ENDDA,
         PNPTIMR6 LIKE QPPNP-TIMR6.
  DATA : SW_ZL    LIKE RPTXXXXX-KR_FELD3.
  RANGES : LGART FOR T512W-LGART.
  RANGES : PNPKOSTL FOR PERNR-KOSTL.

  DATA : IT_L_RSPARAMS LIKE STANDARD TABLE OF RSPARAMS
                       WITH HEADER LINE .
*
  CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
    EXPORTING
      CURR_REPORT           = 'ZACO03U_HRMH'
*   IMPORTING
*     SP                    =
    TABLES
      SELECTION_TABLE       = IT_L_RSPARAMS
    EXCEPTIONS
      NOT_FOUND             = 1
      NO_REPORT             = 2
      OTHERS                = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

* Put data
  PNPTIMR6 = 'X'.
  SW_ZL    = 'X'.
* Regular
  CLEAR : LGART, LGART[].
  LGART-LOW = '0200'.
  LGART-OPTION = 'EQ'.
  LGART-SIGN   = 'I'.
  APPEND LGART.
  CLEAR  LGART.
* Over Time
  LGART-LOW = '0900'.
  LGART-OPTION = 'EQ'.
  LGART-SIGN   = 'I'.
  APPEND LGART.
  CLEAR  LGART.

  LGART-LOW = '0901'.
  LGART-OPTION = 'EQ'.
  LGART-SIGN   = 'I'.
  APPEND LGART.
  CLEAR  LGART.

  LGART-LOW = '0902'.
  LGART-OPTION = 'EQ'.
  LGART-SIGN   = 'I'.
  APPEND LGART.
  CLEAR  LGART.

* Cost center
  CLEAR : PNPKOSTL, PNPKOSTL[].
*  LOOP AT IT_COSTCENTERLIST.
*    PNPKOSTL-LOW    = IT_COSTCENTERLIST-COSTCENTER.
*    PNPKOSTL-SIGN   = 'I'.
*    PNPKOSTL-OPTION = 'EQ'.
*    APPEND PNPKOSTL.
*    CLEAR  PNPKOSTL.
*  ENDLOOP.

  CLEAR RS_DATE.
  READ TABLE RS_DATE INDEX 1.
  PNPBEGDA = RS_DATE-LOW.
  PNPENDDA = RS_DATE-HIGH.

  SUBMIT ZACO03U_HRMH
*    VIA SELECTION-SCREEN
    AND RETURN
    WITH SELECTION-TABLE IT_L_RSPARAMS
    WITH PNPTIMR6 = PNPTIMR6
    WITH PNPBEGDA = PNPBEGDA
    WITH PNPENDDA = PNPENDDA
    WITH SW_ZL = SW_ZL
    WITH LGART IN LGART
    WITH PNPKOSTL IN PNPKOSTL.

*<data_tab>
  IMPORT TIME_DATA_ZES   = TIME_DATA_ZES
         TIME_DATA_SALDO = TIME_DATA_SALDO
         TIME_DATA_ZL    = TIME_DATA_ZL
         DATA_TAB        = DATA_TAB
         FROM MEMORY ID 'HRM'.


** Re-org.
  DATA : LV_TABCOL(30).
  DATA : LV_KOSTL(5) VALUE 'KOSTL', LV_ANZHL(5) VALUE 'ANZHL'.

  FIELD-SYMBOLS : <FSTAB> TYPE TABLE,
                  <FSWA>  TYPE ANY,
                  <FSFN>  TYPE ANY,
                  <FSVAL> TYPE ANY.

  CONCATENATE DATA_TAB '[' ']' INTO LV_TABCOL.
  ASSIGN (LV_TABCOL) TO <FSTAB>.

  ASSIGN LOCAL COPY OF INITIAL LINE OF <FSTAB> TO <FSWA>.
* If no data found in HR tables
  IF SY-SUBRC <> 0.
    MESSAGE E000 WITH TEXT-101.
  ENDIF .


  CLEAR : IT_CATSDB, IT_CATSDB[].

  LOOP AT <FSTAB> ASSIGNING <FSWA>.
*    IT_TMP_CATSDB-GJAHR = P_GJAHR.
*    IT_TMP_CATSDB-PERID = P_PERID.
    IT_CATSDB-LSTAR = P_LSTAR.
*   IT_CATSDB-UNIT  = 'STD'.
* CCTR -KOSTL
    ASSIGN LV_KOSTL TO <FSFN>.
    ASSIGN COMPONENT <FSFN> OF STRUCTURE <FSWA> TO <FSVAL>.
    IT_CATSDB-SKOSTL = <FSVAL> .
* MAN_HR -ANZHL
    ASSIGN LV_ANZHL TO <FSFN>.
    ASSIGN COMPONENT <FSFN> OF STRUCTURE <FSWA> TO <FSVAL>.
    MOVE <FSVAL> TO IT_CATSDB-CATSHOURS.
* Collect data
    COLLECT IT_CATSDB.
    CLEAR IT_CATSDB.
    CLEAR <FSWA>.
  ENDLOOP.

** Remark
** An error was found in HR program .
*  -> Rescan CCtr.
  LOOP AT IT_CATSDB.
    CLEAR IT_COSTCENTERLIST.
    READ TABLE IT_COSTCENTERLIST
      WITH KEY COSTCENTER = IT_CATSDB-SKOSTL.
    IF SY-SUBRC <> 0.
      DELETE IT_CATSDB.
    ENDIF.
    CLEAR IT_CATSDB.
  ENDLOOP.

ENDFORM.                    " READ_FR_CATSDB2
*&---------------------------------------------------------------------*
*&      Form  read_co_mh
*&---------------------------------------------------------------------*
FORM READ_CO_MH.
  TABLES: ZTCO_MHAT.
  DATA: I_MHAT LIKE ZTCO_MHAT OCCURS 0 WITH HEADER LINE.

  SELECT * INTO TABLE I_MHAT
    FROM ZTCO_MHAT
    WHERE GJAHR = P_GJAHR
      AND PERID = P_PERID
      AND KOSTL IN S_KOSTL
      AND LGART BETWEEN '1' AND '2'.  "regular, ot

  IT_CATSDB-LSTAR = P_LSTAR.

  CLEAR : IT_CATSDB, IT_CATSDB[].
  IT_CATSDB-LSTAR = P_LSTAR.  "MAN_HR

  LOOP AT I_MHAT.
    IT_CATSDB-RKOSTL    = I_MHAT-KOSTL.
    IT_CATSDB-CATSHOURS = I_MHAT-ANZHL.
    COLLECT IT_CATSDB.
  ENDLOOP.

*  it_catsdb_sum[] = it_catsdb[].

ENDFORM.                    " read_co_mh
