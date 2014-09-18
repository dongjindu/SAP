*----------------------------------------------------------------------*
***INCLUDE ZACO38L_F002 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  CALL_ALV_LIST
*&---------------------------------------------------------------------*
*       ALV LIST
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
      I_CALLBACK_PROGRAM             = G_REPID
      I_CALLBACK_PF_STATUS_SET       = G_STATUS_SET
      I_CALLBACK_USER_COMMAND        = G_USER_COMMAND
*      I_STRUCTURE_NAME               = 'ZTCO_PLANDEP'
*     IS_LAYOUT                      =
      IT_FIELDCAT                    = IT_FIELDCAT[]
*     IT_EXCLUDING                   = IT_EXCLUDING[]
*     IT_SPECIAL_GROUPS              =
      IT_SORT                        = IT_SORT[]
*     IT_FILTER                      =
*     IS_SEL_HIDE                    =
*     I_DEFAULT                      = 'X'
*     I_SAVE                         = ' '
*     IS_VARIANT                     =
      IT_EVENTS                      = IT_EVENTS
*     IT_EVENT_EXIT                  =
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
      T_OUTTAB                       = IT_DISP_PLANDEP
    EXCEPTIONS
      PROGRAM_ERROR                  = 1
      OTHERS                         = 2.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " CALL_ALV_LIST

*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_INIT
*&---------------------------------------------------------------------*
*        Building Field Cat.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FIELDCAT_INIT.

  CLEAR : GV_COL_POS, IT_FIELDCAT, IT_FIELDCAT[].
* CCtr (Key)
  PERFORM BULID_FIELDCAT USING  'IT_DISP_PLANDEP'
                                'KOSTL'
                                'X'
                                SPACE
                                SPACE
                                SPACE
                                '10'
                                'Cost Center'
                                SPACE.
* Asset (Key)
  PERFORM BULID_FIELDCAT USING  'IT_DISP_PLANDEP'
                                'ANLN1'
                                'X'
                                SPACE
                                SPACE
                                SPACE
                                '10'
                                'Asset No.'
                                SPACE.
* Sub-Asset (Key)
  PERFORM BULID_FIELDCAT USING  'IT_DISP_PLANDEP'
                                'ANLN2'
                                'X'
                                SPACE
                                SPACE
                                SPACE
                                '4'
                                'SubA'
                                SPACE.
* Account(CE) (Key)
  PERFORM BULID_FIELDCAT USING  'IT_DISP_PLANDEP'
                                'KOART'
                                'X'
                                SPACE
                                SPACE
                                SPACE
                                '10'
                                'Cost Elem.'
                                SPACE.
* AMT
  PERFORM FIELD_CAT_FOR_AMT.

* Currency Key
  PERFORM BULID_FIELDCAT USING  'IT_DISP_PLANDEP'
                                'WAERS'
                                SPACE
                                SPACE
                                SPACE
                                SPACE
                                '4'
                                'CURR'
                                'CUKY'.

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
* Sort IT_DISP_PLANDEP.
  SORT IT_DISP_PLANDEP BY KOSTL  ANLN1  ANLN2  KOART.
  CLEAR IT_DISP_PLANDEP.
  IT_SORT-FIELDNAME = 'KOSTL'.
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

*-----------------------------------------------------------------------
*    FORM SET_STATUS
*-----------------------------------------------------------------------
FORM SET_STATUS USING  EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'BALVLIST' EXCLUDING EXTAB.
ENDFORM.

*-----------------------------------------------------------------------
*    FORM SET_STATUS_CHG
*-----------------------------------------------------------------------
FORM SET_STATUS_CHG USING  EXTAB TYPE SLIS_T_EXTAB.
*  DATA : IT_L_EXTAB TYPE SLIS_T_EXTAB WITH HEADER LINE.
*  IT_L_EXTAB-FCODE = 'POST'.
*  APPEND IT_L_EXTAB.
*  CLEAR  IT_L_EXTAB.
*  SET PF-STATUS 'BALVLIST' EXCLUDING IT_L_EXTAB.
  SET PF-STATUS 'BALVLIST2' EXCLUDING EXTAB. " IMMEDIATELY.
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

  DATA : LV_ANSWER.

  CASE UCOMM.
* Important part ! - No reverse Function
    WHEN 'POST' .
* Confirmation Message
      PERFORM CONFIRM_POPUP     USING LV_ANSWER.
      IF LV_ANSWER = 'J'.
* POSTING !!
        PERFORM POST_PL_DEP_COST  USING UCOMM.
** Blocking
*  CLEAR : IT_EXCLUDING, IT_EXCLUDING[].
*  IT_EXCLUDING-FCODE = 'POST'.
*  APPEND IT_EXCLUDING.
*  CLEAR  IT_EXCLUDING.
*       G_STATUS_SET = 'SET_STATUS_CHG'.
        SELFIELD-EXIT = 'X'.
      ENDIF.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  BULID_FIELDCAT
*&---------------------------------------------------------------------*
*       FIELDCAT
*----------------------------------------------------------------------*
*      -->P_0050
*      -->P_0051
*      -->P_0052
*      -->P_0053
*      -->P_0054
*      -->P_0055
*      -->P_0056
*      -->P_0057
*      -->P_0058
*----------------------------------------------------------------------*
FORM BULID_FIELDCAT USING    VALUE(P_0050)
                             VALUE(P_0051)
                             VALUE(P_0052)
                             VALUE(P_0053)
                             VALUE(P_0054)
                             VALUE(P_0055)
                             VALUE(P_0056)
                             VALUE(P_0057)
                             VALUE(P_0058).

  ADD 1 TO GV_COL_POS.
  WA_FIELDCAT-TABNAME     = P_0050.
  WA_FIELDCAT-FIELDNAME   = P_0051.
  WA_FIELDCAT-KEY         = P_0052.
  WA_FIELDCAT-DO_SUM      = P_0053.
  WA_FIELDCAT-CFIELDNAME  = P_0054.
  WA_FIELDCAT-CTABNAME    = P_0055.
  WA_FIELDCAT-OUTPUTLEN   = P_0056.
  WA_FIELDCAT-SELTEXT_L   = P_0057.
  WA_FIELDCAT-DATATYPE    = P_0058.
  WA_FIELDCAT-COL_POS     = GV_COL_POS.
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

ENDFORM.                    " BULID_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  FIELD_CAT_FOR_AMT
*&---------------------------------------------------------------------*
*       AMT
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FIELD_CAT_FOR_AMT.
* Only display  from-period : P_VONPE
*               to-period   : P_BISPE

* AMT
  DATA : LV_MAX(2) TYPE N,
         LV_CNT(2) TYPE N.
  DATA : LV_FNAME(30), LV_TEXT(20).

  LV_CNT = P_VONPE.
  LV_MAX = P_BISPE.
  IF LV_MAX > 26. "Maximum: 26.
    LV_MAX = 26.
  ENDIF.

  WHILE  LV_CNT =< LV_MAX.
    CLEAR : LV_FNAME, LV_TEXT.
    CONCATENATE 'VAL'  LV_CNT INTO  LV_FNAME.
    CONCATENATE 'Per.' LV_CNT INTO  LV_TEXT.

    PERFORM BULID_FIELDCAT USING  'IT_DISP_PLANDEP'
                                  LV_FNAME
                                  SPACE
                                  'X'
                                  'WAERS'
                                  'IT_DISP_PLANDEP'
                                  '20'
                                  LV_TEXT
                                  'CURR'.
    LV_CNT = LV_CNT + 1.
  ENDWHILE.

ENDFORM.                    " FIELD_CAT_FOR_AMT

*---------------------------------------------------------------------*
*       FORM BASIC_TOP_OF_PAGE                                        *
*---------------------------------------------------------------------*
*       Display Title                                                 *
*---------------------------------------------------------------------*
FORM BASIC_TOP_OF_PAGE.
  WRITE : / 'Fiscal Year      ', P_GJAHR, '   Period ', P_VONPE ,
            ' ~ ', P_BISPE.
  WRITE : / 'Controlling Area/CC code/Plant/Plan Version   : ',
             P_KOKRS, '/', P_BUKRS, '/', P_WERKS, '/', P_VERSN.
  WRITE : / 'Cost Center Grp./Dep.Area                     : ',
             P_NCOAL, '/', P_AFABER.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  POST_PL_DEP_COST
*&---------------------------------------------------------------------*
*       Planned Dep. Cost (Subtraction)
*----------------------------------------------------------------------*
*      -->P_UCOMM  text
*----------------------------------------------------------------------*
FORM POST_PL_DEP_COST USING    P_UCOMM.
* Reorginize DATA to POST.
  PERFORM REORG_POST_DATA.
* Posting Part ( Using BAPI FM )
  PERFORM POST_USING_FM USING P_UCOMM.
* Init POST Itab
  PERFORM INIT_IT_POST.

ENDFORM.                    " POST_PL_DEP_COST

*&---------------------------------------------------------------------*
*&      Form  REORG_POST_DATA
*&---------------------------------------------------------------------*
*       Reorginize DATA to POST.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REORG_POST_DATA.
  CLEAR : IT_POST, IT_POST[].
  LOOP AT IT_DISP_PLANDEP.
    IT_POST-KOSTL = IT_DISP_PLANDEP-KOSTL.
    IT_POST-KSTAR = IT_DISP_PLANDEP-KOART.
* Activity Type is SPACE
    PERFORM SET_VALUE_AMT.
    COLLECT IT_POST.
    CLEAR   IT_POST.
    CLEAR IT_DISP_PLANDEP.
  ENDLOOP.
  CLEAR IT_POST.
ENDFORM.                    " REORG_POST_DATA

*&---------------------------------------------------------------------*
*&      Form  SET_VALUE_AMT
*&---------------------------------------------------------------------*
*       Fill value
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_VALUE_AMT.
  TRANS_VALUE_LC 01. TRANS_VALUE_LC 02. TRANS_VALUE_LC 03.
  TRANS_VALUE_LC 04. TRANS_VALUE_LC 05. TRANS_VALUE_LC 06.
  TRANS_VALUE_LC 07. TRANS_VALUE_LC 08. TRANS_VALUE_LC 09.
  TRANS_VALUE_LC 10. TRANS_VALUE_LC 11. TRANS_VALUE_LC 12.
  TRANS_VALUE_LC 13. TRANS_VALUE_LC 14. TRANS_VALUE_LC 15.
  TRANS_VALUE_LC 16.
ENDFORM.                    " SET_VALUE_AMT

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

*&---------------------------------------------------------------------*
*&      Form  POST_USING_FM
*&---------------------------------------------------------------------*
*       POST PLAN DATA using BAPI FM
*----------------------------------------------------------------------*
*  -->  p_ucomm   Post/Reverse
*----------------------------------------------------------------------*
FORM POST_USING_FM USING    P_UCOMM.
* Init. Message TAB
  CLEAR : IT_RETURN, IT_RETURN[].

* Fill Header DATA
  CLEAR WA_HEADERINFO.
  WA_HEADERINFO-CO_AREA        = P_KOKRS.
  WA_HEADERINFO-FISC_YEAR      = P_GJAHR.
  WA_HEADERINFO-PERIOD_FROM	 = P_VONPE.
  WA_HEADERINFO-PERIOD_TO	 = P_BISPE.
  WA_HEADERINFO-VERSION        = P_VERSN. "Plan. Version
* WA_HEADERINFO-DOC_HDR_TX	 =
  WA_HEADERINFO-PLAN_CURRTYPE	 = P_CURRT.

* Fill Object List and Plan Values per Period
  CLEAR : IT_INDEXSTRUCTURE, IT_INDEXSTRUCTURE[].
  CLEAR : IT_COOBJECT,       IT_COOBJECT[].
  CLEAR : IT_PERVALUE,       IT_PERVALUE[].
  CLEAR : IT_TOTVALUE,       IT_TOTVALUE[].

* Sort to post data.
  SORT IT_POST BY KOSTL KSTAR.

  LOOP AT IT_POST.
* Obj
    ON CHANGE OF  IT_POST-KOSTL.
* Index of Object Key
      IT_INDEXSTRUCTURE-OBJECT_INDEX
           = IT_INDEXSTRUCTURE-OBJECT_INDEX + 1 .

      CLEAR IT_COOBJECT.
      IT_COOBJECT-OBJECT_INDEX = IT_INDEXSTRUCTURE-OBJECT_INDEX.

      IT_COOBJECT-COSTCENTER   = IT_POST-KOSTL.
* No activity type
*     IT_COOBJECT-ACTTYPE      = IT_POST-LSTAR.
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
* Post
    IF  P_UCOMM = 'POST'.
      PERFORM SET_VALUE_AMT_BAPI.
    ENDIF.

    APPEND IT_PERVALUE.
    CLEAR  IT_PERVALUE.

* append Index
    APPEND IT_INDEXSTRUCTURE.
    CLEAR IT_POST.
  ENDLOOP.

* Call BAPI FM
  PERFORM CALL_POST_FM.

** Commit
  COMMIT WORK.
  MESSAGE S009(ZMCO) WITH P_UCOMM.

ENDFORM.                    " POST_USING_FM

*&---------------------------------------------------------------------*
*&      Form  SET_VALUE_AMT_BAPI
*&---------------------------------------------------------------------*
*       Fill value
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_VALUE_AMT_BAPI.
* Fixed Cost
  TRANS_VALUE 01. TRANS_VALUE 02. TRANS_VALUE 03. TRANS_VALUE 04.
  TRANS_VALUE 05. TRANS_VALUE 06. TRANS_VALUE 07. TRANS_VALUE 08.
  TRANS_VALUE 09. TRANS_VALUE 10. TRANS_VALUE 11. TRANS_VALUE 12.
  TRANS_VALUE 13. TRANS_VALUE 14. TRANS_VALUE 15. TRANS_VALUE 16.
ENDFORM.                    " SET_VALUE_AMT_BAPI

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
* Important!
* Must use delta indicator (cumulate mode)
      DELTA                = 'X'
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
*&      Form  CONFIRM_POPUP
*&---------------------------------------------------------------------*
*       Conf. Mes.
*----------------------------------------------------------------------*
*      -->P_ANSWER  J/N
*----------------------------------------------------------------------*
FORM CONFIRM_POPUP USING    P_ANSWER.

  CALL FUNCTION 'POPUP_CONTINUE_YES_NO'
    EXPORTING
      DEFAULTOPTION       = 'N'
      TEXTLINE1           = TEXT-022
*   TEXTLINE2           = ' '
      TITEL               = TEXT-023
*   START_COLUMN        = 25
*   START_ROW           = 6
    IMPORTING
      ANSWER              =  P_ANSWER.

ENDFORM.                    " CONFIRM_POPUP
