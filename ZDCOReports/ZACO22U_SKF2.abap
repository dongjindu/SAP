************************************************************************
* Program Name      : ZACO22U_SKF2
* Author            : Eun Hwa , Jung
* Creation Date     : 2003.10.14.
* Specifications By : Dong-Hwan Kim
* Pattern           : Report 1-1
* Development Request No : UD1K902779
* Addl Documentation:
* Description       :
*                   Create Sender Activity for Semi-direct CCtr(Actual)
*
*
* the BDC structures for BATCH INPUT processing
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

REPORT  ZACO22U_SKF2 MESSAGE-ID ZMCO.

** type-pools
TYPE-POOLS: SLIS.

** Table
TABLES : CSKS, CSSL, COSL.


** Internal Table
DATA : BEGIN OF IT_COSL OCCURS 500.
        INCLUDE STRUCTURE ZSCO_COSL_KEY01.
DATA :  DIS001 TYPE COSL-DIS001,
        DIS002 TYPE COSL-DIS002,
        DIS003 TYPE COSL-DIS003,
        DIS004 TYPE COSL-DIS004,
        DIS005 TYPE COSL-DIS005,
        DIS006 TYPE COSL-DIS006,
        DIS007 TYPE COSL-DIS007,
        DIS008 TYPE COSL-DIS008,
        DIS009 TYPE COSL-DIS009,
        DIS010 TYPE COSL-DIS010,
        DIS011 TYPE COSL-DIS011,
        DIS012 TYPE COSL-DIS012,
        DIS013 TYPE COSL-DIS013,
        DIS014 TYPE COSL-DIS014,
        DIS015 TYPE COSL-DIS015,
        DIS016 TYPE COSL-DIS016,
        MEINH  TYPE COSL-MEINH.
DATA : END OF   IT_COSL.

* for reporting
DATA: BEGIN OF IT_REPORT OCCURS 0,
        VERSN LIKE COBK-VERSN,
        KOKRS LIKE CSKS-KOKRS,
        GJAHR LIKE COSL-GJAHR,
        PERID LIKE COEJL-PERBL,
        SKOST LIKE RK23B-SKOST,
        LSTAR LIKE RK23B-LSTAR,
        OBJNR LIKE  COSS-OBJNR,
        LVBSU LIKE RK23B-LVBSU,
        UNIT  LIKE COSL-MEINH,
      END OF IT_REPORT.


*- BDC TABLE
DATA  BEGIN OF bdc_tab OCCURS 0.     " BDCDATA TABLE.
        INCLUDE STRUCTURE bdcdata.
DATA  END OF bdc_tab.

DATA  BEGIN OF messtab OCCURS 0.     " BDC MESSAGE TABLE.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA  END OF messtab.

DATA : message(100),
           bdc_mode VALUE 'N'.     "'A'.

DATA : BEGIN OF it_mess OCCURS 0,
        message(100),
       END OF it_mess.

** For BAPI
DATA : IT_COSTCENTERLIST LIKE STANDARD TABLE OF BAPI0012_CCLIST
                         WITH HEADER LINE.
DATA : IT_RETURN         LIKE STANDARD TABLE OF BAPIRET2
                         WITH HEADER LINE.


** For OBJECT KEY
DATA : BEGIN OF WA_OBJ ,
        OBJNR  LIKE  COSS-OBJNR,
        KOSTL  LIKE  CSKS-KOSTL,
        LSTAR  LIKE  CSLA-LSTAR,
       END OF WA_OBJ.
DATA : IT_OBJ_CCTR_AT   LIKE STANDARD TABLE OF WA_OBJ
                        WITH HEADER LINE .

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

*  For  posting date.
DATA : DATE(10).

*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-001.
PARAMETERS :
             P_KOKRS LIKE CSKS-KOKRS MEMORY ID CAC OBLIGATORY,
             P_GJAHR LIKE COBK-GJAHR MEMORY ID GJR OBLIGATORY,
             P_PERID LIKE COSP-PERBL MEMORY ID BPE OBLIGATORY,
             P_VERSN LIKE COBK-VERSN MEMORY ID KVT OBLIGATORY.
SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK BL3 WITH FRAME TITLE TEXT-003.
SELECT-OPTIONS : S_KOSTL  FOR CSKS-KOSTL.
PARAMETERS:      P_NCOAL LIKE GRPDYNP-NAME_COALL DEFAULT 'SEMIDIRECT'.
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

  PERFORM PRE_POST_DATE.

* Read Cost Center Group - > Cost Center
  PERFORM READ_CEGRP.

* Read OBJ Key Combination
  PERFORM SET_OBJ_KEY.

* Read Sender_Quantity Information from COSL
  PERFORM READ_COSL_QTY.

* Preparation of ALV
  PERFORM PRE_REPORT_ADJ.

*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.
* Call ALV LIST
  PERFORM CALL_ALV_LIST.

*&---------------------------------------------------------------------*
*&      Form  dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0014   text
*      -->P_0015   text
*      -->P_0016   text
*----------------------------------------------------------------------*
FORM dynpro USING   dynbegin name value.

  IF dynbegin  = 'X'.
    CLEAR bdc_tab.
    MOVE : name  TO bdc_tab-program,
           value TO bdc_tab-dynpro,
           'X'   TO bdc_tab-dynbegin.
    APPEND bdc_tab.
  ELSE.
    CLEAR bdc_tab.
    MOVE : name  TO bdc_tab-fnam,
           value TO bdc_tab-fval.
    APPEND bdc_tab.
  ENDIF.

ENDFORM.                    " dynpro
*&---------------------------------------------------------------------*
*&      Form  CALL_BDC
*&---------------------------------------------------------------------*
*       Preparation bdc data
*----------------------------------------------------------------------*
*      -->P_UCOMM  text
*----------------------------------------------------------------------*
FORM CALL_BDC USING    P_UCOMM.

 CLEAR : bdc_tab, bdc_tab[].

* Controlling Area setting
  CALL FUNCTION 'K_KOKRS_SET'
       IMPORTING
            E_KOKRS   = P_KOKRS
       EXCEPTIONS
            NOT_FOUND = 1
            OTHERS    = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


  PERFORM dynpro USING :
          'X' 'SAPMK23B'      '1001',
          ' ' 'COBK-BLDAT'     DATE,
          ' ' 'COBK-BUDAT'     DATE,      " Posting date : 10/05/2003
          ' ' 'RK23A-VERSN'    P_VERSN,
          ' ' 'COBK-KOKRS'     P_KOKRS,
          ' ' 'BDC_OKCODE'    '/00'.


  DATA : INDEX(3) TYPE N.
  DATA : LV_NAME1(30).
  DATA : LV_NAME2(30).
  DATA : LV_NAME3(30).
  DATA : LV_NAME4(30).

  DATA : VAL(15),
         VAL2 TYPE I.

  CLEAR INDEX.
  loop at it_report.
    CLEAR : LV_NAME1, LV_NAME2, LV_NAME3, LV_NAME4.
    INDEX = INDEX + 1.       "SY-TABIX.
    CONCATENATE 'RK23B-SKOST(' INDEX ')' INTO LV_NAME1.
    CONCATENATE 'RK23B-LSTAR(' INDEX ')' INTO LV_NAME2.
    CONCATENATE 'RK23B-LVBSU(' INDEX ')' INTO LV_NAME3.
    CONCATENATE 'RK23B-LEINH(' INDEX ')' INTO LV_NAME4.

    CLEAR : VAL, VAL2.
    IF  P_UCOMM = 'POST'.
      WRITE IT_REPORT-LVBSU TO VAL.
*      reverse
    ELSEIF  P_UCOMM = 'REVS'.
      VAL2 = IT_REPORT-LVBSU * -1 .
      WRITE VAL2 TO VAL.
    ENDIF.

    PERFORM dynpro USING :
        'X' 'SAPMK23B'      '5111',
        ' ' LV_NAME1     IT_REPORT-SKOST,
        ' ' LV_NAME2     IT_REPORT-LSTAR,
        ' ' LV_NAME3     VAL,
        ' ' LV_NAME4     'HR',    "IT_REPORT-UNIT,
        ' ' 'BDC_OKCODE'    '/00'.

  endloop.
  clear it_report.

  PERFORM dynpro USING :
        'X' 'SAPMK23B'      '5111',
        ' ' 'BDC_OKCODE'    '=BU'.

* Call transaction.
  PERFORM CALL_TRANSACTION.


ENDFORM.                    " CALL_BDC
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
*&      Form  SET_OBJ_KEY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
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
                       AND GJAHR = P_GJAHR
                       AND LATYPI = '5'.

  CLEAR IT_OBJ_CCTR_AT.

ENDFORM.                    " SET_OBJ_KEY
*&---------------------------------------------------------------------*
*&      Form  READ_COSL_QTY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
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
          AND WRTTP = '04'                   " ACTUAL : '04'
          AND VERSN = P_VERSN.
  CLEAR IT_COSL.

* Local Data definition
  FIELD-SYMBOLS: <FS1> TYPE ANY.
  DATA : LV_LST_NAM(30).
  CLEAR   IT_COSL.
  CLEAR : IT_REPORT, IT_REPORT[].

  LOOP AT IT_COSL.
** Key Part
    IT_REPORT-VERSN = P_VERSN.
    IT_REPORT-KOKRS = P_KOKRS.
    IT_REPORT-GJAHR = P_GJAHR.
    IT_REPORT-PERID = P_PERID.

    CLEAR IT_OBJ_CCTR_AT.
    READ TABLE    IT_OBJ_CCTR_AT
         WITH KEY OBJNR = IT_COSL-OBJNR.
    IT_REPORT-SKOST = IT_OBJ_CCTR_AT-KOSTL.
    IT_REPORT-LSTAR = IT_OBJ_CCTR_AT-LSTAR.
    IT_REPORT-OBJNR = IT_OBJ_CCTR_AT-OBJNR.

    IT_REPORT-UNIT = IT_COSL-MEINH.

* Value Transferring
    CLEAR LV_LST_NAM.
    CONCATENATE 'IT_COSL-'  'DIS'  P_PERID
           INTO LV_LST_NAM.
    ASSIGN (LV_LST_NAM) TO <FS1>.

    CLEAR IT_REPORT-LVBSU.
    IT_REPORT-LVBSU = <FS1>.

*   Unit Conversion
     IF IT_REPORT-UNIT <> 'STD'.
      PERFORM UNIT_CONV USING IT_REPORT-UNIT
                              IT_REPORT-LVBSU.
     ENDIF.

* Collect
    COLLECT IT_REPORT.

    CLEAR IT_REPORT.
    CLEAR IT_COSL.
  ENDLOOP.
  CLEAR IT_REPORT.


ENDFORM.                    " READ_COSL_QTY
*&---------------------------------------------------------------------*
*&      Form  PRE_REPORT_ADJ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRE_REPORT_ADJ.

 sort it_report by SKOST LSTAR.

* Building Field Cat.
  PERFORM FIELDCAT_INIT .

* Set Event
  DATA : WA_L_EVENT  TYPE SLIS_ALV_EVENT.
  WA_L_EVENT-NAME = SLIS_EV_TOP_OF_PAGE.
  WA_L_EVENT-FORM = 'BASIC_TOP_OF_PAGE'.
  APPEND WA_L_EVENT TO IT_EVENTS.


ENDFORM.                    " PRE_REPORT_ADJ
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FIELDCAT_INIT.

  CLEAR : GV_COL_POS, IT_FIELDCAT, IT_FIELDCAT[].

* Key

  PERFORM BUILD_FIELDCAT USING
    'IT_REPORT' 'VERSN'  'X'            SPACE    SPACE
    SPACE        '7'      'Version'           SPACE    SPACE    SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_REPORT' 'KOKRS'  'X'            SPACE    SPACE
    SPACE        '7'      'Co-Area'           SPACE    SPACE    SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_REPORT' 'GJAHR'  'X'            SPACE    SPACE
    SPACE        '4'      'Year'           SPACE    SPACE    SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_REPORT' 'PERID'  'X'            SPACE    SPACE
    SPACE        '6'      'Period'           SPACE    SPACE    SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_REPORT' 'SKOST'  'X'            SPACE    SPACE
    SPACE        '10'      'Sender'           SPACE    SPACE    SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_REPORT' 'LSTAR'  'X'            SPACE    SPACE
    SPACE        '6'      'Sen.AT'           SPACE    SPACE    SPACE.

** Value

  PERFORM BUILD_FIELDCAT USING
    'IT_REPORT' 'UNIT'  SPACE            SPACE    SPACE
    SPACE   '4'      'Unit'      'UNIT'   SPACE     SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_REPORT' 'LVBSU'  SPACE            SPACE    SPACE
    SPACE '15'      'Allo.Act.Qty'    SPACE    SPACE SPACE.



ENDFORM.                    " FIELDCAT_INIT
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0995   text
*      -->P_0996   text
*      -->P_0997   text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_1001   text
*      -->P_1002   text
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
  SET PF-STATUS 'LIST' EXCLUDING EXTAB .
ENDFORM.

*---------------------------------------------------------------------*
*       FORM BASIC_TOP_OF_PAGE                                        *
*---------------------------------------------------------------------*
*       Display Title                                                 *
*---------------------------------------------------------------------*
FORM BASIC_TOP_OF_PAGE.

  WRITE : / 'Controlling Area           : '
            , P_KOKRS .
  WRITE : / 'Fiscal Year/Period/Version : '
            , P_GJAHR, '/', P_PERID, '/', P_VERSN.
  WRITE : / 'Posting Date               : ' , DATE.


* CCTR
  IF P_NCOAL NE SPACE.
  WRITE : / 'Cost Center Grp.           : ', P_NCOAL.
  ENDIF.

  IF NOT S_KOSTL[] IS INITIAL.
    LOOP AT S_KOSTL.
      AT FIRST.
  WRITE : / 'Cost Center                : '.
      ENDAT.
  WRITE : / '                            ', S_KOSTL-LOW, '~',
S_KOSTL-HIGH.
    ENDLOOP.
  ENDIF.

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
* POST PLAN data.
    WHEN 'POST' OR 'REVS'.
      PERFORM CALL_BDC  USING UCOMM.
  ENDCASE.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHK_INPUT_VALUE
*&---------------------------------------------------------------------*
*       text
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
*&      Form  CALL_TRANSACTION
*&---------------------------------------------------------------------*
*       CALL_TRANSACTION
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_TRANSACTION.

  CLEAR : message.

  CALL TRANSACTION 'KB51'  USING bdc_tab
                           MODE   bdc_mode
                           UPDATE 'S'
                           MESSAGES INTO messtab.

  if sy-subrc eq 0.
*       Consolidation management for the SKF
        CALL FUNCTION 'Z_FCO_MANAGEMENT_SKF'
          EXPORTING
            IM_PGMNO         =   SY-TCODE
            IM_KOKRS         =   P_KOKRS
            IM_GJAHR         =   P_GJAHR
            IM_PERBL         =   P_PERID
*           IM_PERBL_T       =
            IM_VERSN         =   P_VERSN
           IM_KOSTL_F        =    S_KOSTL-LOW
           IM_KOSTL_T        =    S_KOSTL-HIGH
           IM_GNAME          =    P_NCOAL.
*           IM_PRZNR_F       =
*           IM_PRZNR_T       =
*         IMPORTING
*           SUBRC            =

  else.
    read table messtab index 1.
    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
         EXPORTING
              msgid               = messtab-msgid
              msgnr               = messtab-msgnr
              msgv1               = messtab-msgv1
              msgv2               = messtab-msgv2
              msgv3               = messtab-msgv3
              msgv4               = messtab-msgv4
         IMPORTING
              message_text_output = message.
    message E000(ZMCO) with MESSAGE.

  ENDIF.


ENDFORM.                    " CALL_TRANSACTION
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
*&      Form  PRE_POST_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRE_POST_DATE.

* preparation posting date.
*  DATA : DATE(10).
   DATA : PERID(2) TYPE N.
  clear : perid , date.
  PERID = P_PERID+1(2).
  CONCATENATE PERID '/05/' P_GJAHR  INTO DATE.

ENDFORM.                    " PRE_POST_DATE
