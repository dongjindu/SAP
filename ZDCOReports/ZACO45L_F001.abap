*----------------------------------------------------------------------*
*   INCLUDE ZACO45L_F001                                               *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  BLOCK_INPUT
*&---------------------------------------------------------------------*
*       Bloking Input
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BLOCK_INPUT.
  LOOP AT SCREEN.
    CHECK SCREEN-GROUP1 = 'VRG'.
    SCREEN-INPUT        = '0'.
    SCREEN-INTENSIFIED  = '1'.
    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.                    " BLOCK_INPUT

*&---------------------------------------------------------------------*
*&      Form  DR_LIST_FOR_ACC
*&---------------------------------------------------------------------*
*       Drop Down List For Account Number
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DR_LIST_FOR_ACC.

  GV_DRLIST_NAME = 'P_SAKN1'.

  refresh: IT_DRLIST_LIST.
  CLEAR : IT_DRLIST_LIST, IT_DRLIST_LIST[].

  CLEAR WA_DRLIST_VALUE.
  WA_DRLIST_VALUE-KEY  = '0000620100'.
  WA_DRLIST_VALUE-TEXT = 'Warranty'.
  APPEND WA_DRLIST_VALUE TO IT_DRLIST_LIST.

  CLEAR WA_DRLIST_VALUE.
  WA_DRLIST_VALUE-KEY  = '0000620105'.
  WA_DRLIST_VALUE-TEXT = 'Warranty Reclaim'.
  APPEND WA_DRLIST_VALUE TO IT_DRLIST_LIST.

  CLEAR WA_DRLIST_VALUE.
  WA_DRLIST_VALUE-KEY  = '0000620110'.
  WA_DRLIST_VALUE-TEXT = 'Campaign'.
  APPEND WA_DRLIST_VALUE TO IT_DRLIST_LIST.

  CLEAR WA_DRLIST_VALUE.
  WA_DRLIST_VALUE-KEY  = '0000620115'.
  WA_DRLIST_VALUE-TEXT = 'Campaign Reclaim'.
  APPEND WA_DRLIST_VALUE TO IT_DRLIST_LIST.

  CLEAR WA_DRLIST_VALUE.

  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            ID              = GV_DRLIST_NAME
            VALUES          = IT_DRLIST_LIST
       EXCEPTIONS
            ID_ILLEGAL_NAME = 1
            OTHERS          = 2.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " DR_LIST_FOR_ACC

*&---------------------------------------------------------------------*
*&      Form  READ_ZTCO_WARR
*&---------------------------------------------------------------------*
*       Read data from ZTCO_WARR
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_ZTCO_WARR.

  CLEAR : IT_ZTCO_WARR, IT_ZTCO_WARR[].

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTCO_WARR
           FROM ZTCO_WARR
          WHERE SAKN1 = P_SAKN1.

  IF IT_ZTCO_WARR[] IS INITIAL .
    MESSAGE E072.
  ENDIF.
* Check Values
* THERE CAN BE COMMON TYPE AS VEHICLE MODEL : PAPH2 = blank
  LOOP AT IT_ZTCO_WARR WHERE  COPA_VALUES EQ SPACE.
  ENDLOOP.

  IF SY-SUBRC = 0.
    MESSAGE E072.
  ENDIF.

ENDFORM.                    " READ_ZTCO_WARR

*&---------------------------------------------------------------------*
*&      Form  READ_BILLING_DOC
*&---------------------------------------------------------------------*
*       Read Billing Documents
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_BILLING_DOC.

  CLEAR COBK.

  CLEAR : IT_COBK, IT_COBK[].

** Obsolete !!
** read Account Docs/Billing Docs.
** KONV is not transferent table, it can not be used in subquery or join
** Reading BSEG is not recommanded - Performance problem
*  SELECT   BK~BUKRS  BK~BELNR  BK~GJAHR  BK~MONAT  BK~AWTYP  BK~AWKEY
*           VB~VBELN  VB~KNUMV
*           INTO CORRESPONDING FIELDS OF TABLE IT_BKPF
*           FROM BKPF AS BK INNER JOIN VBRK AS VB
*             ON BK~AWKEY = VB~VBELN
*          WHERE BK~BUKRS = P_BUKRS
*            AND BK~GJAHR = P_GJAHR
*            AND BK~MONAT = P_PERDE
*            AND BK~AWTYP = 'VBRK'.

* Read Cost Documents / Billing Documents
* Only billing documents for warrenty and campaing can generate
* Cost documents, so to search the billing documents from cost
* documents is not so harmful to the performance - Query.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_COBK
           FROM COBK
          WHERE KOKRS = P_KOKRS
            AND GJAHR = P_GJAHR
            AND PERAB = P_PERDE
            AND PERBI = P_PERDE
            AND AWTYP = 'VBRK'
            AND VERSN = '000'.
* Index ZD1
  IF IT_COBK[] IS INITIAL .
    MESSAGE E000 WITH TEXT-090 P_GJAHR P_PERDE.
  ENDIF.

* Read VBRK / KONV
* KONV is not transferent table, it can not be used in subquery or join
  CLEAR : IT_KNUMV, IT_KNUMV[].

  SELECT   VBELN KNUMV FKSTO WAERK KURRF
            INTO CORRESPONDING FIELDS OF TABLE IT_KNUMV
            FROM VBRK
             FOR ALL ENTRIES IN IT_COBK
           WHERE VBELN = IT_COBK-REFBN.

  CLEAR IT_KNUMV.
  SORT  IT_KNUMV  BY VBELN.

  DELETE ADJACENT DUPLICATES FROM IT_KNUMV.

ENDFORM.                    " READ_BILLING_DOC

*&---------------------------------------------------------------------*
*&      Form  READ_CON_AMT
*&---------------------------------------------------------------------*
*       Read Condition and Amt.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_CON_AMT.
  CLEAR IT_ZTCO_WARR.
  CLEAR IT_COBK.
  CLEAR IT_KNUMV.

  CLEAR : IT_VBRK_KONV, IT_VBRK_KONV[].
* It is not necessary to check condition types because the account no.
* restricts the condition types which are assigned
* to the account number.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_VBRK_KONV
           FROM KONV
           FOR ALL ENTRIES IN  IT_KNUMV
          WHERE KNUMV = IT_KNUMV-KNUMV
            AND SAKN1 = P_SAKN1.

  CLEAR IT_VBRK_KONV.

  IF IT_VBRK_KONV[] IS INITIAL.
    MESSAGE E000 WITH TEXT-091 P_SAKN1 P_GJAHR P_PERDE.
  ENDIF.

*FKSTO         X -> -1
* Check The cancellation -> -1
*(The Amount in condtion table is not changed by the cancellation)
* Convert trs. Currency to CO Area Currency
  SORT IT_KNUMV BY KNUMV.
  LOOP AT IT_VBRK_KONV.
* Check The cancellation
    CLEAR IT_KNUMV.
    READ TABLE IT_KNUMV WITH KEY KNUMV = IT_VBRK_KONV-KNUMV.
    IF IT_KNUMV-FKSTO = 'X'.
      IT_VBRK_KONV-KWERT = IT_VBRK_KONV-KWERT * ( -1 ).
      IT_VBRK_KONV-KBETR = IT_VBRK_KONV-KBETR * ( -1 ).
    ENDIF.
* Local/Doc Currency
    IT_VBRK_KONV-WAERS = IT_KNUMV-WAERK.
* Convert trs. Currency to CO Area Currency
    IT_VBRK_KONV-COWAER = TKA01-WAERS.
    IF IT_VBRK_KONV-WAERS <>  IT_VBRK_KONV-COWAER.
* Convertion (Curr)
      PERFORM CONV_CURR_WITH_RATE
         USING IT_VBRK_KONV-KWERT
               IT_VBRK_KONV-WAERS
               '1'
               IT_KNUMV-KURRF
*              IT_VBRK_KONV-KKURS
               IT_VBRK_KONV-COWAER
               IT_VBRK_KONV-WKGBTR.
    ELSE.
      IT_VBRK_KONV-WKGBTR =  IT_VBRK_KONV-KWERT.
    ENDIF.
* Modify
    MODIFY IT_VBRK_KONV.
    CLEAR IT_VBRK_KONV.
  ENDLOOP.

ENDFORM.                    " READ_CON_AMT

*&---------------------------------------------------------------------*
*&      Form  Read_TKA01
*&---------------------------------------------------------------------*
*       Controlling Area Information
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_TKA01.
  CLEAR TKA01.
  SELECT SINGLE * FROM TKA01
                 WHERE KOKRS = P_KOKRS.
  IF SY-SUBRC <> 0.
    MESSAGE E038 WITH P_KOKRS.
  ENDIF.
ENDFORM.                    " Read_TKA01
*&---------------------------------------------------------------------*
*&      Form  CONV_CURR_WITH_RATE
*&---------------------------------------------------------------------*
*       Currency Convertion
*----------------------------------------------------------------------*
*      -->P_KWERT
*      -->P_WAERS
*      -->P_FACTOR
*      -->P_KKURS
*      -->P_COWAER
*      -->P_WKGBTR
*----------------------------------------------------------------------*
FORM CONV_CURR_WITH_RATE USING    P_KWERT   LIKE BSEG-DMBTR
                                  P_WAERS   LIKE BKPF-WAERS
                                  P_FACTOR  LIKE TCURR-FFACT
                                  P_KKURS   LIKE TCURR-UKURS
                                  P_COWAER  LIKE BKPF-WAERS
                                  P_WKGBTR  .

  DATA : LV_RATE        LIKE TCURR-UKURS ,
         LV_WKGBTR      LIKE BSEG-WRBTR.

  CLEAR LV_RATE.
  LV_RATE = ABS( P_KKURS ).

  CALL FUNCTION 'CONVERT_CURRENCY_BY_RATE'
       EXPORTING
            FROM_AMOUNT   = P_KWERT
            FROM_CURRENCY = P_WAERS
            FROM_FACTOR   = P_FACTOR
            RATE          = LV_RATE
            TO_CURRENCY   = P_COWAER
            TO_FACTOR     = P_FACTOR
       IMPORTING
            TO_AMOUNT     = LV_WKGBTR
       EXCEPTIONS
            NO_RATE_FOUND = 1
            OTHERS        = 2.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  P_WKGBTR = LV_WKGBTR.

ENDFORM.                    " CONV_CURR_WITH_RATE

*&---------------------------------------------------------------------*
*&      Form  COLLECT_DATA
*&---------------------------------------------------------------------*
*       Collect data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM COLLECT_DATA.

  CLEAR IT_VBRK_KONV.

  CLEAR : IT_COL_DATA, IT_COL_DATA[].

  LOOP AT IT_VBRK_KONV.
* Trasfer Key part
    CLEAR IT_ZTCO_WARR.
    READ TABLE  IT_ZTCO_WARR
                    WITH KEY  KSCHL = IT_VBRK_KONV-KSCHL
                              SAKN1 = IT_VBRK_KONV-SAKN1.
    IT_COL_DATA-PAPH2       = IT_ZTCO_WARR-PAPH2.
    IT_COL_DATA-COPA_VALUES = IT_ZTCO_WARR-COPA_VALUES.
* Value Part
    MOVE-CORRESPONDING IT_VBRK_KONV TO IT_COL_DATA.
* Collect
    COLLECT IT_COL_DATA.
    CLEAR   IT_COL_DATA.
    CLEAR IT_VBRK_KONV.
  ENDLOOP.

  CLEAR   IT_COL_DATA.

ENDFORM.                    " COLLECT_DATA

*&---------------------------------------------------------------------*
*&      Form  CAL_COMMON_VEH_TYPE
*&---------------------------------------------------------------------*
*       Calulate data for Vehicle Type 'COMMON'
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CAL_COMMON_VEH_TYPE.

* Only when there are data about Vehicle Mode 'COMMON'.
* COMMON Vehicle Mode : the value of field 'PAPH2' is INITIAL value
  CHECK
       NOT IT_COMMON[] IS INITIAL.
* Cal. Sum of data in IT_COMMON.
  PERFORM SUM_OF_COMMON.

  IF NOT IT_POST[] IS INITIAL.

* Case 1.
* Other Vehicle Models have data
* And 'COMMON' Vehicle Model also has data
    PERFORM CAL_COMMON_CASE_1.

  ELSE.

* Case2.
* Only 'COMMON' Vehicle Model has data
    PERFORM CAL_COMMON_CASE_2.

  ENDIF.

ENDFORM.                    " CAL_COMMON_VEH_TYPE

*&---------------------------------------------------------------------*
*&      Form  SUM_OF_COMMON
*&---------------------------------------------------------------------*
*       Cal. Sum of data in IT_COMMON.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SUM_OF_COMMON.
  CLEAR GV_COM_SUM.
  LOOP AT IT_COMMON.
    GV_COM_SUM = GV_COM_SUM + IT_COMMON-WKGBTR  .
  ENDLOOP.
ENDFORM.                    " SUM_OF_COMMON

*&---------------------------------------------------------------------*
*&      Form  CAL_COMMON_CASE_1
*&---------------------------------------------------------------------*
* Case 1.
* Other Vehicle Models have data
* And 'COMMON' Vehicle Model also has data
*
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CAL_COMMON_CASE_1.

  DATA : LV_SUM     LIKE COEP-WKGBTR.

  CLEAR LV_SUM.
* Cal. SUM (But VM 'COMMON')
  LOOP AT IT_POST.
    LV_SUM = LV_SUM + IT_POST-WKGBTR.
    CLEAR IT_POST.
  ENDLOOP.
* Cal. %   (But VM 'COMMON')
  LOOP AT IT_POST.
    IT_POST-VM_SUM    = LV_SUM.
    IT_POST-%_OF_SUM  = IT_POST-WKGBTR / IT_POST-VM_SUM.
    IT_POST-COM_X_%   = GV_COM_SUM * IT_POST-%_OF_SUM.
    MODIFY IT_POST.
    CLEAR  IT_POST.
  ENDLOOP.

ENDFORM.                    " CAL_COMMON_CASE_1

*&---------------------------------------------------------------------*
*&      Form  BUILDING_TAB_FOR_POSTING
*&---------------------------------------------------------------------*
*       Building Tab for posting
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILDING_TAB_FOR_POSTING.

  CLEAR : IT_POST,    IT_POST[].
  CLEAR : IT_COMMON,  IT_COMMON[].

* Only VM - Vehicle Model - 'COMMON'
  LOOP AT  IT_COL_DATA
                       WHERE  PAPH2 EQ SPACE
                          OR  PAPH2 IS INITIAL.
    MOVE-CORRESPONDING IT_COL_DATA TO IT_COMMON.
    COLLECT  IT_COMMON.
    CLEAR    IT_COMMON.
    CLEAR    IT_COL_DATA .
  ENDLOOP.
*   Zero Value
  DELETE IT_COMMON WHERE WKGBTR IS INITIAL.

* Not VM - Vehicle Model - 'COMMON'
  LOOP AT  IT_COL_DATA
                       WHERE       PAPH2 NE SPACE
                          AND  NOT PAPH2 IS INITIAL.
    MOVE-CORRESPONDING IT_COL_DATA TO IT_POST.
    COLLECT  IT_POST.
    CLEAR    IT_POST.
    CLEAR    IT_COL_DATA .
  ENDLOOP.
*   Zero Value
  DELETE IT_POST WHERE WKGBTR IS INITIAL.

ENDFORM.                    " BUILDING_TAB_FOR_POSTING

*&---------------------------------------------------------------------*
*&      Form  CAL_COMMON_CASE_2
*&---------------------------------------------------------------------*
* Case2.
* Only 'COMMON' Vehicle Model has data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CAL_COMMON_CASE_2.

* Count of Vehicle Kind
  DATA : LV_VM_COUNT LIKE SY-TABIX.
  CLEAR LV_VM_COUNT.

  DATA : IT_L_WARR LIKE STANDARD TABLE OF IT_ZTCO_WARR
                   WITH HEADER LINE .

  CLEAR : IT_L_WARR, IT_L_WARR[].
  SELECT  DISTINCT PAPH2 COPA_VALUES
           INTO CORRESPONDING FIELDS OF TABLE IT_L_WARR
           FROM ZTCO_WARR
          WHERE SAKN1 = P_SAKN1
            AND PAPH2 NE SPACE.

  DESCRIBE TABLE IT_L_WARR LINES LV_VM_COUNT.

* / Vehicle Kind
  LOOP AT IT_L_WARR.
    CLEAR IT_POST.
    MOVE-CORRESPONDING IT_L_WARR TO IT_POST.
* Currency
    CLEAR IT_COMMON.
    READ TABLE  IT_COMMON
    WITH KEY    PAPH2       = SPACE
                COPA_VALUES = IT_POST-COPA_VALUES.
    IT_POST-COWAER = IT_COMMON-COWAER.
* value / rate
    IT_POST-%_OF_SUM  = 1 / LV_VM_COUNT.
    IT_POST-COM_X_%   = GV_COM_SUM / LV_VM_COUNT.
    APPEND IT_POST.
    CLEAR  IT_POST.
    CLEAR  IT_L_WARR.
  ENDLOOP.
ENDFORM.                    " CAL_COMMON_CASE_2

*&---------------------------------------------------------------------*
*&      Form  CALL_ALV_LIST
*&---------------------------------------------------------------------*
*       Call ALV LIST
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
*     IT_SORT                        =
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
      T_OUTTAB                       = IT_POST
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
*       Building Field Cat.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FIELDCAT_INIT.
  CLEAR : GV_COL_POS, IT_FIELDCAT, IT_FIELDCAT[].


*         PAPH2    LIKE ZTCO_WARR-PAPH2,
*         COPA_VALUES
*                  LIKE ZTCO_WARR-COPA_VALUES,
*         WKGBTR   LIKE COEP-WKGBTR,
*         COWAER   LIKE COEP-OWAER,
*         VM_SUM   LIKE COEP-WKGBTR,
*         %_OF_SUM(6) TYPE P DECIMALS 2,
*         COM_X_%  LIKE COEP-WKGBTR,
*
* key part
  BULID_FIELDCAT  'IT_POST'     'PAPH2'
                  'X'           'X'      SPACE      SPACE      '15'
                  'Vehicle Model'        SPACE.

  BULID_FIELDCAT  'IT_POST'     'COPA_VALUES'
                  'X'           'X'      SPACE      SPACE       '8'
                  'Value Fd'             SPACE.

* Value Part
  BULID_FIELDCAT  'IT_POST'     'WKGBTR'
                  SPACE         SPACE    'COWAER'  'IT_POST'    '20'
                  'Con. Amt.'   'CURR'.

  BULID_FIELDCAT  'IT_POST'     '%_OF_SUM'
                  SPACE         SPACE    SPACE      SPACE       '10'
                  'COM. Rate'   'CURR'.

  BULID_FIELDCAT  'IT_POST'     'COM_X_%'
                  SPACE         SPACE    'COWAER'  'IT_POST'    '20'
                  'COM. Amt.'   'CURR'.

  BULID_FIELDCAT  'IT_POST'     'VM_SUM'
                  SPACE         SPACE    'COWAER'  'IT_POST'    '20'
                  'SUM. VM.'    'CURR'.

  BULID_FIELDCAT  'IT_POST'     'COWAER'
                  SPACE         SPACE    SPACE      SPACE       '4'
                  'CURR'        'CUKY'.

ENDFORM.                    " FIELDCAT_INIT

*&---------------------------------------------------------------------*
*&      Form  PRE_LIST_DATA
*&---------------------------------------------------------------------*
*       The Preparation for ALV List
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRE_LIST_DATA.
* Sort IT_LIST.
  SORT IT_POST BY  PAPH2  COPA_VALUES.
  CLEAR IT_POST.

* Set Event
  DATA : WA_L_EVENT  TYPE SLIS_ALV_EVENT.
  WA_L_EVENT-NAME = SLIS_EV_TOP_OF_PAGE.
  WA_L_EVENT-FORM = 'BASIC_TOP_OF_PAGE'.
  APPEND WA_L_EVENT TO IT_EVENTS.
ENDFORM.                    " PRE_LIST_DATA

*-----------------------------------------------------------------------
*    FORM PF_STATUS_VAR
*-----------------------------------------------------------------------
FORM PF_STATUS_VAR USING  EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'BALVLIST' EXCLUDING EXTAB.
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
* POST PA data
    WHEN 'POST' OR 'REVS'.
      PERFORM POST_CO_PA  USING UCOMM.
  ENDCASE.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM BASIC_TOP_OF_PAGE                                        *
*---------------------------------------------------------------------*
*       Display Title                                                 *
*---------------------------------------------------------------------*
FORM BASIC_TOP_OF_PAGE.

  WRITE : / 'Controlling Area ', P_KOKRS.
  WRITE : / 'Company Code     ', P_BUKRS.
  WRITE : / 'Record Type      ', P_VRGAR.
  WRITE : / 'Fiscal Year      ', P_GJAHR, '   Period ', P_PERDE.
  WRITE : / 'Date as Posting Date ', P_BUDAT.
  WRITE : / 'Test Run         ', P_TRUN.
  SKIP 1.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  POST_CO_PA
*&---------------------------------------------------------------------*
*       POST PA data
*----------------------------------------------------------------------*
*      -->P_UCOMM  SY-UCOMM
*----------------------------------------------------------------------*
FORM POST_CO_PA USING    P_UCOMM.

* Data definition
  DATA : LV_RECORD_ID LIKE IT_INPUTDATA-RECORD_ID.

  CLEAR IT_POST.

* Init. Message TAB
  CLEAR : IT_RETURN, IT_RETURN[].

  CLEAR : GV_OPERATINGCONCERN, GV_TESTRUN.
  CLEAR : IT_INPUTDATA, IT_INPUTDATA[].
  CLEAR : IT_FIELDLIST, IT_FIELDLIST[].

* Operation Concern
  GV_OPERATINGCONCERN = TKA01-ERKRS.
* Test Run
  GV_TESTRUN = P_TRUN.
* Field List
  PERFORM SET_FD_LIST.
* Input Value
  LOOP AT IT_POST.
* Record ID
    LV_RECORD_ID = LV_RECORD_ID + 1.
* Input Data
    PERFORM SET_INPUT_DATA USING LV_RECORD_ID
                                 P_UCOMM.
    CLEAR IT_POST.
  ENDLOOP.

* posting -> Important!!
* Call BAPI FM
  PERFORM CALL_POST_FM.

* Commit
  IF P_TRUN = 'X'.
  ELSE.
    COMMIT WORK.
    MESSAGE S009(ZMCO) WITH P_UCOMM.
  ENDIF.

ENDFORM.                    " POST_CO_PA

*&---------------------------------------------------------------------*
*&      Form  SET_INPUT_DATA
*&---------------------------------------------------------------------*
*       Input Data
*----------------------------------------------------------------------*
*  -->  P_RECORD_ID        Record ID
*  -->  P_UCOMM            Posting/Reversing
*----------------------------------------------------------------------*
FORM SET_INPUT_DATA USING P_RECORD_ID
                          P_UCOMM.
* Record Type
  CLEAR IT_INPUTDATA.
  IT_INPUTDATA-RECORD_ID = P_RECORD_ID.
  IT_INPUTDATA-FIELDNAME = 'VRGAR'.
  IT_INPUTDATA-VALUE     = P_VRGAR.
  APPEND  IT_INPUTDATA.
* Fs. Year
  CLEAR IT_INPUTDATA.
  IT_INPUTDATA-RECORD_ID = P_RECORD_ID.
  IT_INPUTDATA-FIELDNAME = 'GJAHR'.
  IT_INPUTDATA-VALUE     = P_GJAHR.
  APPEND  IT_INPUTDATA.
* Period
  CLEAR IT_INPUTDATA.
  IT_INPUTDATA-RECORD_ID = P_RECORD_ID.
  IT_INPUTDATA-FIELDNAME = 'PERDE'.
  IT_INPUTDATA-VALUE     = P_PERDE.
  APPEND  IT_INPUTDATA.
* Posting Date
  CLEAR IT_INPUTDATA.
  IT_INPUTDATA-RECORD_ID = P_RECORD_ID.
  IT_INPUTDATA-FIELDNAME = 'BUDAT'.
  IT_INPUTDATA-VALUE     = P_BUDAT.
  APPEND  IT_INPUTDATA.
* Company Code
  CLEAR IT_INPUTDATA.
  IT_INPUTDATA-RECORD_ID = P_RECORD_ID.
  IT_INPUTDATA-FIELDNAME = 'BUKRS'.
  IT_INPUTDATA-VALUE     = P_BUKRS.
  APPEND  IT_INPUTDATA.
* Vehicle Model
  CLEAR IT_INPUTDATA.
  IT_INPUTDATA-RECORD_ID = P_RECORD_ID.
  IT_INPUTDATA-FIELDNAME = 'PAPH2'.
  IT_INPUTDATA-VALUE     = IT_POST-PAPH2.
  APPEND  IT_INPUTDATA.
* Value : Warranty/Campaign
  CLEAR IT_INPUTDATA.
  IT_INPUTDATA-RECORD_ID = P_RECORD_ID.
  IT_INPUTDATA-FIELDNAME = IT_POST-COPA_VALUES.
  IT_INPUTDATA-VALUE     =
                           IT_POST-WKGBTR
                         + IT_POST-COM_X_%.
*& -> Reverse
  IF P_UCOMM = 'REVS'.
    IT_INPUTDATA-VALUE = IT_INPUTDATA-VALUE * ( -1 ).
  ENDIF.

  IT_INPUTDATA-CURRENCY  = IT_POST-COWAER.
  APPEND  IT_INPUTDATA.

ENDFORM.                    " SET_INPUT_DATA

*&---------------------------------------------------------------------*
*&      Form  SET_FD_LIST
*&---------------------------------------------------------------------*
*       Field List
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_FD_LIST.
  IT_FIELDLIST-FIELDNAME = 'VRGAR'.
  COLLECT IT_FIELDLIST.
  CLEAR  IT_FIELDLIST.
  IT_FIELDLIST-FIELDNAME = 'GJAHR'.
  COLLECT IT_FIELDLIST.
  CLEAR  IT_FIELDLIST.
  IT_FIELDLIST-FIELDNAME = 'PERDE'.
  COLLECT IT_FIELDLIST.
  CLEAR  IT_FIELDLIST.
  IT_FIELDLIST-FIELDNAME = 'BUDAT'.
  COLLECT IT_FIELDLIST.
  CLEAR  IT_FIELDLIST.
  IT_FIELDLIST-FIELDNAME = 'BUKRS'.
  COLLECT IT_FIELDLIST.
  CLEAR  IT_FIELDLIST.
  IT_FIELDLIST-FIELDNAME = 'PAPH2'.
  COLLECT IT_FIELDLIST.
  CLEAR  IT_FIELDLIST.

  LOOP AT IT_POST.
    IT_FIELDLIST-FIELDNAME = IT_POST-COPA_VALUES.
    COLLECT IT_FIELDLIST.
    CLEAR  IT_FIELDLIST.
    CLEAR IT_POST.
  ENDLOOP.

ENDFORM.                    " SET_FD_LIST

*&---------------------------------------------------------------------*
*&      Form  CALL_POST_FM
*&---------------------------------------------------------------------*
*       Call BAPI FM
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_POST_FM.

  CALL FUNCTION 'BAPI_COPAACTUALS_POSTCOSTDATA'
       EXPORTING
            OPERATINGCONCERN = GV_OPERATINGCONCERN
            TESTRUN          = GV_TESTRUN
       TABLES
            INPUTDATA        = IT_INPUTDATA
            FIELDLIST        = IT_FIELDLIST
            RETURN           = IT_RETURN.

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
*&      Form  CAL_BUDAT
*&---------------------------------------------------------------------*
*       Posting Date -> Last Date of Month
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CAL_BUDAT.
  CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
       EXPORTING
            I_GJAHR = P_GJAHR
            I_PERIV = TKA01-LMONA
            I_POPER = P_PERDE
       IMPORTING
            E_DATE  = P_BUDAT.
ENDFORM.                    " CAL_BUDAT
