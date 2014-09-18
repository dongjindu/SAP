*----------------------------------------------------------------------*
***INCLUDE ZACO18L_F002 .
*----------------------------------------------------------------------*
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

  BULID_FIELDCAT  'IT_REPORT'     'PERIOD'
                  'X'           SPACE    SPACE      SPACE      '3'
                  'Period'               SPACE.

  BULID_FIELDCAT  'IT_REPORT'     'KOSTL'
                  'X'           SPACE    SPACE      SPACE      '10'
                  'Cost Center'          SPACE.

  BULID_FIELDCAT  'IT_REPORT'     'LSTAR'
                  'X'           SPACE    SPACE      SPACE      '6'
                  'Activity Type'        SPACE.

  BULID_FIELDCAT  'IT_REPORT'     'FIX_AMT'
                  SPACE         'X'      'WAERS'  'IT_REPORT'  '20'
                  'Org. Fixed Amt.'      'CURR'.

  BULID_FIELDCAT  'IT_REPORT'     'VAR_AMT'
                  SPACE         'X'      'WAERS'  'IT_REPORT'  '20'
                  'Org. Var. Amt.'       'CURR'.

  BULID_FIELDCAT  'IT_REPORT'     'RATE'
                  SPACE         SPACE    SPACE      SPACE      '10'
                  'Quan. Rat.'           SPACE.

  BULID_FIELDCAT  'IT_REPORT'     'CH_VAR_AMT'
                  SPACE         'X'      'WAERS'  'IT_REPORT'  '20'
                  'Changed Var. Amt.'    'CURR'.

  BULID_FIELDCAT  'IT_REPORT'     'WAERS'
                  SPACE         SPACE    SPACE     SPACE       '4'
                  'CURR'                 'CUKY'.

ENDFORM.                    " FIELDCAT_INIT

*&---------------------------------------------------------------------*
*&      Form  PRE_REPORT_ADJ
*&---------------------------------------------------------------------*
*       The Preparation for ALV List
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRE_REPORT_ADJ.
* Read CO area Currency
  CLEAR IT_REPORT.
  SELECT SINGLE WAERS INTO IT_REPORT-WAERS
                     FROM TKA01
                    WHERE KOKRS = P_KOKRS.
* ALL Curr data are base on Controlling Area Currency.
* Change Currency fields with Controlling Area Currency.
  MODIFY IT_REPORT  TRANSPORTING WAERS WHERE WAERS EQ SPACE.

* Sort IT_REPORT.
  SORT IT_REPORT BY PERIOD KOSTL LSTAR .
  CLEAR IT_REPORT.
  IT_SORT-FIELDNAME = 'PERIOD'.
  IT_SORT-UP        = 'X'.
  IT_SORT-EXPA      = 'X'.
  IT_SORT-SUBTOT    = 'X'.
  APPEND IT_SORT.

* Set Event
  DATA : WA_L_EVENT  TYPE SLIS_ALV_EVENT.
  WA_L_EVENT-NAME = SLIS_EV_TOP_OF_PAGE.
  WA_L_EVENT-FORM = 'BASIC_TOP_OF_PAGE'.
  APPEND WA_L_EVENT TO IT_EVENTS.

ENDFORM.                    " PRE_REPORT_ADJ

*&---------------------------------------------------------------------*
*&      Form  CALL_ALV_LIST
*&---------------------------------------------------------------------*
*       CALL ALV LIST
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
      IT_SORT                        = IT_SORT[]
*     IT_FILTER                      =
*     IS_SEL_HIDE                    =
*     I_DEFAULT                      = 'X'
      I_SAVE                         = 'A'
*     IS_VARIANT                     =
      IT_EVENTS                      = IT_EVENTS
      IT_EVENT_EXIT                  = IT_EVENT_EXIT
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
*    FORM PF_STATUS_VAR
*-----------------------------------------------------------------------
FORM PF_STATUS_VAR USING  EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'BALVLIST' EXCLUDING EXTAB OF PROGRAM 'ZACO08U_VRVA'.
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
    WHEN 'POST' OR 'REVS'.
      PERFORM POST_STD_CCTR_AT_CE  USING UCOMM.
  ENDCASE.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM BASIC_TOP_OF_PAGE                                        *
*---------------------------------------------------------------------*
*       Display Title                                                 *
*---------------------------------------------------------------------*
FORM BASIC_TOP_OF_PAGE.

  WRITE : / 'Controlling Area ', P_KOKRS.
  WRITE : / 'Fiscal Year      ', P_GJAHR, '   Period ', P_FRPER,
            ' ~ ', P_TOPER.
  WRITE : / 'Plan Version     ', P_FRVER.
  WRITE : / 'Standard  Version', P_TOVER.


* CCTR
  IF P_NCOAL NE SPACE.
    WRITE : / 'Cost Center Grp.', P_NCOAL.
  ENDIF.

  IF NOT S_KOSTL[] IS INITIAL.
    LOOP AT S_KOSTL.
      AT FIRST.
        WRITE : / 'Cost Center:     '.
      ENDAT.
      WRITE : / '                 ', S_KOSTL-LOW, '~', S_KOSTL-HIGH.
    ENDLOOP.
  ENDIF.

* CE
  IF P_CEGRP NE SPACE.
    WRITE : / 'Cost Element Grp.', P_CEGRP.
  ENDIF.
  IF NOT S_KSTAR[] IS INITIAL.
    LOOP AT S_KSTAR.
      AT FIRST.
        WRITE : / 'Cost Element:    '.
      ENDAT.
      WRITE : / '                 ', S_KSTAR-LOW, '~', S_KSTAR-HIGH.
    ENDLOOP.
  ENDIF.

  WRITE : / 'Test Run         ', P_TRUN.
  SKIP 1.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  POST_STD_CCTR_AT_CE
*&---------------------------------------------------------------------*
*       POST PLAN DATA using BAPI FM
*----------------------------------------------------------------------*
*      -->P_UCOMM  Post/Reverse
*----------------------------------------------------------------------*
FORM POST_STD_CCTR_AT_CE USING    P_UCOMM.

* Calculating data to post (refer to Rate ).
  PERFORM CAL_POST_DATA.

* Posting Part ( Using BAPI FM )
  PERFORM POST_USING_FM USING P_UCOMM.

* Init POST Itab
  PERFORM INIT_IT_POST.

ENDFORM.                    " POST_STD_CCTR_AT_CE

*&---------------------------------------------------------------------*
*&      Form  CAL_POST_DATA
*&---------------------------------------------------------------------*
*       Calculating data to post (refer to Rate ).
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CAL_POST_DATA.

* Init ITAB (to POST)
  CLEAR : IT_POST, IT_POST[].
* Moving Data to IT_POST
  LOOP AT IT_COSP.
    MOVE-CORRESPONDING IT_COSP TO  IT_POST.
    COLLECT IT_POST.
    CLEAR IT_POST.
    CLEAR IT_COSP.
  ENDLOOP.

  CLEAR IT_POST.

* adjusting amounts with the rate in ratio table
  FIELD-SYMBOLS: <FS1> TYPE ANY, <FS2> TYPE ANY.
  DATA : LV_COSP_RAT(30).
  DATA : LV_COSP_VAR(30).
  DATA : LV_CNT  LIKE  COSP-PERBL.

  SORT IT_RATE BY OBJNR.

  SORT IT_POST BY OBJNR.


  LOOP AT IT_POST.
* read Rate Table
    ON CHANGE OF IT_POST-OBJNR.
      CLEAR IT_RATE.
      READ TABLE IT_RATE  WITH KEY OBJNR = IT_POST-OBJNR
                          BINARY SEARCH.
    ENDON.
* Period Counter : Set From-Period .
    CLEAR LV_CNT.
    LV_CNT = P_FRPER .

    DO GV_PERCOUNT TIMES.
* Rate
      CLEAR LV_COSP_RAT.
      CONCATENATE 'IT_RATE-'  GV_FIELDGROUP_RAT  LV_CNT
             INTO LV_COSP_RAT.
      ASSIGN (LV_COSP_RAT) TO <FS1>.
* values (Variable AMT)
      CLEAR LV_COSP_VAR.
      CONCATENATE 'IT_POST-'  GV_FIELDGROUP_VAR  LV_CNT
             INTO LV_COSP_VAR.
      ASSIGN (LV_COSP_VAR) TO <FS2>.
* Adjusting    Var' = Var * rate (Original Var. Amt.)
      <FS2> = <FS2> * <FS1>.

* Period Counter
      LV_CNT = LV_CNT + 1.
    ENDDO.

* Modifying
    MODIFY IT_POST.
    CLEAR IT_POST.
  ENDLOOP.

  CLEAR IT_POST.

ENDFORM.                    " CAL_POST_DATA

*&---------------------------------------------------------------------*
*&      Form  POST_USING_FM
*&---------------------------------------------------------------------*
*       POST PLAN DATA using BAPI FM (STD. Version)
*----------------------------------------------------------------------*
*  -->  p_ucomm   Post/Reverse
*----------------------------------------------------------------------*
FORM POST_USING_FM  USING P_UCOMM.
* Init. Message TAB
  CLEAR : IT_RETURN, IT_RETURN[].

* Fill Header DATA
  CLEAR WA_HEADERINFO.
  WA_HEADERINFO-CO_AREA        = P_KOKRS.
  WA_HEADERINFO-FISC_YEAR      = P_GJAHR.
  WA_HEADERINFO-PERIOD_FROM	 = P_FRPER.
  WA_HEADERINFO-PERIOD_TO	 = P_TOPER.
  WA_HEADERINFO-VERSION        = P_TOVER. "STD. Version
* WA_HEADERINFO-DOC_HDR_TX	 =
  WA_HEADERINFO-PLAN_CURRTYPE	 = P_CURRT.

* Fill Object List and Plan Values per Period
  CLEAR : IT_INDEXSTRUCTURE, IT_INDEXSTRUCTURE[].
  CLEAR : IT_COOBJECT,       IT_COOBJECT[].
  CLEAR : IT_PERVALUE,       IT_PERVALUE[].
  CLEAR : IT_TOTVALUE,       IT_TOTVALUE[].

* Sort to post data.
  SORT IT_POST BY KOSTL LSTAR KSTAR.

  LOOP AT IT_POST.
* Obj
    ON CHANGE OF  IT_POST-KOSTL
              OR  IT_POST-LSTAR .
* Index of Object Key
      IT_INDEXSTRUCTURE-OBJECT_INDEX
           = IT_INDEXSTRUCTURE-OBJECT_INDEX + 1 .

      CLEAR IT_COOBJECT.
      IT_COOBJECT-OBJECT_INDEX = IT_INDEXSTRUCTURE-OBJECT_INDEX.

      IT_COOBJECT-COSTCENTER   = IT_POST-KOSTL.
      IT_COOBJECT-ACTTYPE      = IT_POST-LSTAR.
      APPEND IT_COOBJECT.
      CLEAR  IT_COOBJECT.
    ENDON.

* Value.
* Index of Value
    IT_INDEXSTRUCTURE-VALUE_INDEX
         = IT_INDEXSTRUCTURE-VALUE_INDEX + 1.

    CLEAR IT_PERVALUE.
    IT_PERVALUE-VALUE_INDEX = IT_INDEXSTRUCTURE-VALUE_INDEX.
    IT_PERVALUE-COST_ELEM   = IT_POST-KSTAR.
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
  ENDIF.

ENDFORM.                    " POST_USING_FM

*&---------------------------------------------------------------------*
*&      Form  SET_VALUE_AMT
*&---------------------------------------------------------------------*
*       Fill value
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
*       POSTING
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_POST_FM.

  CALL FUNCTION 'BAPI_COSTACTPLN_POSTPRIMCOST'
    EXPORTING
      HEADERINFO           = WA_HEADERINFO
      DELTA                = ' '
    TABLES
      INDEXSTRUCTURE       = IT_INDEXSTRUCTURE
      COOBJECT             = IT_COOBJECT
      PERVALUE             = IT_PERVALUE
*     TOTVALUE             =
*     CONTRL               =
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
*&      Form  INIT_IT_POST
*&---------------------------------------------------------------------*
*       Initialization the post ITAB
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_IT_POST.
  CLEAR : IT_POST, IT_POST[].
  FREE  : IT_POST, IT_POST[].
ENDFORM.                    " INIT_IT_POST
