*----------------------------------------------------------------------*
***INCLUDE ZACO12L_F002 .
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

  BULID_FIELDCAT  'IT_POST'     'VEHTP'
                  'X'           SPACE    SPACE      SPACE      '3'
                  'Vty'                  SPACE.

  BULID_FIELDCAT  'IT_POST'     'KOSTL'
                  'X'           SPACE    SPACE      SPACE      '10'
                  'Cost Center'          SPACE.

  BULID_FIELDCAT  'IT_POST'     'KTNAFG'
                  'X'           SPACE    SPACE      SPACE      '10'
                  'Account'              SPACE.

  BULID_FIELDCAT  'IT_POST'     'AUFNR'
                  'X'           SPACE    SPACE      SPACE      '10'
                  'PCC Order'            SPACE.

  BULID_FIELDCAT  'IT_POST'     'DEPAMT'
                  SPACE         'X'      'WAERS'  'IT_POST'    '20'
                  'Org. Dep. Amt.'       'CURR'.

  BULID_FIELDCAT  'IT_POST'     'CHG_NAFAZ'
                  SPACE         'X'      'WAERS'  'IT_POST'    '20'
                  'Chg. Dep. Amt.'       'CURR'.

  BULID_FIELDCAT  'IT_POST'     'RATE'
                  SPACE         SPACE    SPACE      SPACE      '10'
                  'Quan. Rat.'           SPACE.

  ADD 1 TO GV_COL_POS.
  WA_FIELDCAT-TABNAME     = 'IT_POST'.
  WA_FIELDCAT-FIELDNAME   = 'MEGXX'.
  WA_FIELDCAT-OUTPUTLEN   = '20'.
  WA_FIELDCAT-SELTEXT_L   = 'Quan.'.
  WA_FIELDCAT-JUST        = 'R'.
  WA_FIELDCAT-COL_POS     = GV_COL_POS.
*         quantity(3)    type c,
*         qfieldname     type slis_fieldname, " field with quantity unit
*         qtabname       type slis_tabname,   " and table
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

  BULID_FIELDCAT  'IT_POST'     'WAERS'
                  SPACE         SPACE    SPACE     SPACE       '4'
                  'CURR'                 'CUKY'.

ENDFORM.                    " FIELDCAT_INIT

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
      T_OUTTAB                       = IT_POST
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
* For POST (Internal Order)
    WHEN 'POST'.
      PERFORM POST_INTERNAL_ORDER.
* For Reverse (Internal Order)
    WHEN 'LOG'.
      PERFORM DIS_LOG_FOR_POST_DOC.
  ENDCASE.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM BASIC_TOP_OF_PAGE                                        *
*---------------------------------------------------------------------*
*       Display Title                                                 *
*---------------------------------------------------------------------*
FORM BASIC_TOP_OF_PAGE.
  WRITE : / 'Controlling Area '     , P_KOKRS.
  WRITE : / 'Company Code     '     , P_BUKRS,
            40(10) 'Plant'          , P_BUKRS.
  WRITE : / 'Fiscal Year      '     , P_GJAHR,
            40(10) 'Period'         , P_PERAF.
  WRITE : / 'Cost center Group'     , P_NCOAL,
            40(10) 'Act. Type'      , P_LSTAR.
  WRITE : / 'Version          '     , P_VERSN,
            40(10) 'Dep. area'      , P_AFABER.
  SKIP 1.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PRE_REPORT_ADJ
*&---------------------------------------------------------------------*
*       The Preparation for ALV report
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRE_REPORT_ADJ.
* Put Original Dep. Amounts
  LOOP AT IT_POST.
    ON CHANGE OF IT_POST-VEHTP
              OR IT_POST-KOSTL
              OR IT_POST-KTNAFG .
      IT_POST-DEPAMT = IT_POST-NAFAZ  .
      MODIFY IT_POST.
    ENDON.
    CLEAR IT_POST.
  ENDLOOP.

* Sort IT_POST.
  SORT IT_POST BY VEHTP KOSTL AUFNR KTNAFG  .
  CLEAR IT_POST.
  IT_SORT-FIELDNAME = 'VEHTP'.
  IT_SORT-UP        = 'X'.
  IT_SORT-EXPA      = 'X'.
  IT_SORT-SUBTOT    = 'X'.
  APPEND IT_SORT.
  IT_SORT-FIELDNAME = 'KOSTL'.
  IT_SORT-UP        = 'X'.
  IT_SORT-EXPA      = 'X'.
  IT_SORT-SUBTOT    = 'X'.
  APPEND IT_SORT.
  SORT IT_POST BY VEHTP KOSTL KTNAFG.

* Set Event
  DATA : WA_L_EVENT  TYPE SLIS_ALV_EVENT.
  WA_L_EVENT-NAME = SLIS_EV_TOP_OF_PAGE.
  WA_L_EVENT-FORM = 'BASIC_TOP_OF_PAGE'.
  APPEND WA_L_EVENT TO IT_EVENTS.

ENDFORM.                    " PRE_REPORT_ADJ

*&---------------------------------------------------------------------*
*&      Form  POST_INTERNAL_ORDER
*&---------------------------------------------------------------------*
*       Posting Internal Order
*----------------------------------------------------------------------*
FORM POST_INTERNAL_ORDER .

* The number of Item Records should not be greater than 250(200)
  DATA : IT_POST_IDX LIKE SY-TABIX.
  DATA : LV_MOD      LIKE SY-TABIX.
  DATA : LV_DIV      LIKE SY-TABIX.
  DATA : LV_FROM_IDX LIKE SY-TABIX,
         LV_TO_IDX   LIKE SY-TABIX,
         LV_SKIP.

* Check overlapping POSTING!
  DATA LV_ANSWER.
  IF NOT IT_BELNR[] IS  INITIAL.
    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
      EXPORTING
        DEFAULTOPTION        = 'N'
        TEXTLINE1            = TEXT-002
        TEXTLINE2            = TEXT-003
        TITEL                = 'Posting Confirmation'
*       START_COLUMN         = 25
*       START_ROW            = 6
*       CANCEL_DISPLAY       = 'X'
      IMPORTING
        ANSWER               = LV_ANSWER.
  ELSE.
    LV_ANSWER = 'J'.
  ENDIF.
  CHECK LV_ANSWER = 'J'.

* Clear Message TAB
  CLEAR : IT_RESUTAB, IT_RESUTAB[].

* Read the number of Index of "IT_POST"
  DESCRIBE TABLE IT_POST LINES IT_POST_IDX.
  LV_DIV = IT_POST_IDX  DIV  200.  LV_MOD = IT_POST_IDX  MOD  200.
  LV_DIV = LV_DIV + 1.
* CALL BDC
  DO LV_DIV TIMES.
* Cal. MOD. DIV.
    LV_TO_IDX   =  LV_DIV * 200 .
    LV_FROM_IDX =  LV_TO_IDX - 199.
* Check Index of IT_POST
* From
    CLEAR LV_SKIP.
    CLEAR IT_POST. READ TABLE IT_POST INDEX LV_FROM_IDX.
    IF SY-SUBRC <> 0. LV_SKIP = 'X'. ENDIF.
* TO
    CLEAR IT_POST. READ TABLE IT_POST INDEX LV_TO_IDX.
    IF SY-SUBRC <> 0. LV_TO_IDX = LV_FROM_IDX + LV_MOD - 1 . ENDIF.

    IF LV_SKIP <> 'X'.
      PERFORM CALL_TRA_BDC USING  LV_FROM_IDX LV_TO_IDX  .
    ENDIF.
  ENDDO.

* Sucess Message
  MESSAGE I009 WITH 'Please, Check the log.'.

* Saving Document numbers which were posted successfully.
  CLEAR : IT_BELNR, IT_BELNR[].
  LOOP AT IT_RESUTAB WHERE MSGTYP = 'S'
                       AND MSGID  = 'BK'
                       AND MSGNR  = '003'.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
         EXPORTING
              INPUT  = IT_RESUTAB-MSGV1
         IMPORTING
              OUTPUT = IT_BELNR-BELNR.
    APPEND IT_BELNR.
    CLEAR  IT_BELNR.
    CLEAR IT_RESUTAB.
  ENDLOOP.
ENDFORM.                    " POST_INTERNAL_ORDER

*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM BDC_DYNPRO USING PROGRAM DYNPRO.
  CLEAR IT_BDCDATA.
  IT_BDCDATA-PROGRAM  = PROGRAM.
  IT_BDCDATA-DYNPRO   = DYNPRO.
  IT_BDCDATA-DYNBEGIN = 'X'.
  APPEND IT_BDCDATA.
ENDFORM.

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM BDC_FIELD USING FNAM FVAL.
  CLEAR IT_BDCDATA.
  IT_BDCDATA-FNAM = FNAM.
  IT_BDCDATA-FVAL = FVAL.
  APPEND IT_BDCDATA.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CALL_TRA_BDC
*&---------------------------------------------------------------------*
*       BDC BODY for "Internal Order"
*----------------------------------------------------------------------*
*      -->P_LV_FR  From-Index
*      -->P_LV_TO  To-Index
*----------------------------------------------------------------------*
FORM CALL_TRA_BDC USING    P_LV_FR
                           P_LV_TO.
* Conv. Date Format(Document/Posting Date)
  DATA LV_DATUM(10).
  DATA LV_MEG(16).

  CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
       EXPORTING
            INPUT  = P_BLDAT
       IMPORTING
            OUTPUT = LV_DATUM.

* BDC
  CLEAR : IT_BDCDATA, IT_BDCDATA[].
  CLEAR : IT_MESSTAB, IT_MESSTAB[].

* Header Information
  PERFORM BDC_DYNPRO      USING 'SAPMK23B' '1001'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'    '/00'.
  PERFORM BDC_FIELD       USING 'COBK-BLDAT'    LV_DATUM.
  PERFORM BDC_FIELD       USING 'COBK-BUDAT'    LV_DATUM.
  PERFORM BDC_FIELD       USING 'COBK-PERAB'    P_BLDAT+4(2).
  PERFORM BDC_FIELD       USING 'COBK-KOKRS'    P_KOKRS.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'    '/00'.
  PERFORM BDC_FIELD       USING 'COBK-VARNR'    'SAP09'.

* Transaction Currency will be set.
* Currency Key can not be changed manually
*  PERFORM BDC_DYNPRO      USING 'SAPMK23B' '1334'.
*  PERFORM BDC_FIELD       USING 'RK23A-TWAER'        SPACE.

* Items
  LOOP AT IT_POST  FROM P_LV_FR TO  P_LV_TO.
    PERFORM BDC_DYNPRO      USING 'SAPMK23B' '1334'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'    '=NEUE'.
    PERFORM BDC_FIELD       USING 'RK23B-KSTAR(02)'  IT_POST-KTNAFG.
    CLEAR LV_MEG.
    LV_MEG = IT_POST-CHG_NAFAZ.
    PERFORM BDC_FIELD       USING 'RK23B-IKPSU(02)'  LV_MEG.
    PERFORM BDC_FIELD       USING 'RK23B-SKOST(02)'  IT_POST-KOSTL.
    PERFORM BDC_FIELD       USING 'RK23B-EAUFN(02)'  IT_POST-AUFNR.
  ENDLOOP.

* SAVING
  PERFORM BDC_DYNPRO      USING 'SAPMK23B' '1334'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'    '=BU'.

* Call Transaction
  REFRESH IT_MESSTAB.
  CALL TRANSACTION 'KB11'
                   USING IT_BDCDATA
                   MODE   P_MODE
                   UPDATE 'S'
                   MESSAGES INTO IT_MESSTAB.

* Error? -> No futher Processing
  LOOP AT IT_MESSTAB WHERE MSGTYP CA 'AE'.
    MESSAGE     ID IT_MESSTAB-MSGID
              TYPE IT_MESSTAB-MSGTYP
            NUMBER IT_MESSTAB-MSGNR
              WITH IT_MESSTAB-MSGV1
                   IT_MESSTAB-MSGV2
                   IT_MESSTAB-MSGV3
                   IT_MESSTAB-MSGV4.
  ENDLOOP.

* Storing Messages to Message Container
  LOOP AT IT_MESSTAB.
    MOVE-CORRESPONDING IT_MESSTAB TO IT_RESUTAB.
    APPEND IT_RESUTAB.
    CLEAR IT_RESUTAB.
    CLEAR IT_MESSTAB.
  ENDLOOP.

ENDFORM.                    " CALL_TRA_BDC

*&---------------------------------------------------------------------*
*&      Form  DIS_LOG_FOR_POST_DOC
*&---------------------------------------------------------------------*
*       Display posted documents
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DIS_LOG_FOR_POST_DOC.
* Check if at least one document was created.
  IF IT_BELNR[] IS INITIAL .
    MESSAGE E025.
  ENDIF.

* Read Informations from Document Header
  DATA : IT_L_COBK LIKE STANDARD TABLE OF COBK
                   WITH HEADER LINE .
  CLEAR : IT_L_COBK, IT_L_COBK[].

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_L_COBK
           FROM COBK
           FOR ALL ENTRIES IN IT_BELNR
          WHERE KOKRS = P_KOKRS
            AND BELNR = IT_BELNR-BELNR.

  CALL FUNCTION 'STC1_POPUP_WITH_TABLE_CONTROL'
    EXPORTING
      HEADER                  = 'CO Document Information'
      TABNAME                 = 'COBK'
      DISPLAY_ONLY            = 'X'
      NO_BUTTON               = SPACE
*     X_START                 = 5
*     Y_START                 = 5
*     X_END                   = 80
*     Y_END                   = 25
    TABLES
      TABLE                   = IT_L_COBK
    EXCEPTIONS
      NO_MORE_TABLES          = 1
      TOO_MANY_FIELDS         = 2
      NAMETAB_NOT_VALID       = 3
      HANDLE_NOT_VALID        = 4
      OTHERS                  = 5.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " DIS_LOG_FOR_POST_DOC
