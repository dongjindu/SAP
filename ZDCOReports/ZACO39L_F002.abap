*&---------------------------------------------------------------------*
*   INCLUDE ZACO39L_F002.
*&---------------------------------------------------------------------*

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
      T_OUTTAB                       = IT_COL_TAB
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
* Key
  PERFORM BUILD_FIELDCAT USING
    'IT_COL_TAB' 'PERIOD' 'X'            SPACE    SPACE
    SPACE        '3'      'Period'       SPACE    SPACE    SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_COL_TAB' 'VEHTP'  'X'            SPACE    SPACE
    SPACE        '2'      'VM'           SPACE    SPACE    SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_COL_TAB' 'MATNR'  'X'            SPACE    SPACE
    SPACE        '18'     'Material'     SPACE    SPACE    SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_COL_TAB' 'KOART'  'X'            SPACE    SPACE
    SPACE        '10'     'Account'      SPACE    SPACE    SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_COL_TAB' 'KOSTL'  'X'            SPACE    SPACE
    SPACE        '10'     'CCtr'         SPACE    SPACE    SPACE.

* Value
* Planned Dep. Cost
  PERFORM BUILD_FIELDCAT USING
    'IT_COL_TAB' 'VALXX'  SPACE          SPACE   'WAERS'
    'IT_COL_TAB' '20'            'Plan. DEP. Cost'
    'CURR'       SPACE           SPACE.
* KSPP
  PERFORM BUILD_FIELDCAT USING
    'IT_COL_TAB' 'MEGBTR' SPACE          SPACE    SPACE
    SPACE        '18'     'AT quantity'  'QUAN'   'MEINH' 'IT_COL_TAB'.

  PERFORM BUILD_FIELDCAT USING
    'IT_COL_TAB' 'MEINH'  SPACE          SPACE    SPACE
    SPACE        '4'      'MH'           'UNIT'   SPACE    SPACE.
* Quantity (LTP/Period)
  PERFORM BUILD_FIELDCAT USING
    'IT_COL_TAB' 'PLNMG'  SPACE          SPACE    SPACE
    SPACE        '18'  'Plan.LTP Quan.'  'QUAN'   'MEINS' 'IT_COL_TAB'.

  PERFORM BUILD_FIELDCAT USING
    'IT_COL_TAB' 'MEINS'  SPACE          SPACE    SPACE
    SPACE        '4'      'UNIT'         'UNIT'   SPACE    SPACE.
* result
  PERFORM BUILD_FIELDCAT USING
    'IT_COL_TAB' 'DEP_AT_%_PLM'  SPACE   'X'      'WAERS'
    'IT_COL_TAB' '20'            'Plan. Mat. Unit.Cost'
    'CURR'       SPACE           SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_COL_TAB' 'WAERS'  SPACE          SPACE    SPACE
    SPACE        '4'      'CURR'         'CUKY'   SPACE    SPACE.
* others
  PERFORM BUILD_FIELDCAT USING
    'IT_COL_TAB' 'AT_SUM' SPACE          SPACE    SPACE
    SPACE        '18'  'AT quan. SUM'    'QUAN'   'MEINH' 'IT_COL_TAB'.

  PERFORM BUILD_FIELDCAT USING
    'IT_COL_TAB' 'AT_%'   SPACE          'X'      SPACE
    SPACE        '10'  'AT %'            SPACE    SPACE    SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_COL_TAB' 'DEP_AT_%'      SPACE   'X'      'WAERS'
    'IT_COL_TAB' '20'            'DEP.X AT.%'
    'CURR'       SPACE           SPACE.

** For Main ITAB
*DATA : BEGIN OF IT_COL_TAB OCCURS 0,
*         WERKS  LIKE CKI64A-WERKS, "Plant
*         LSTAR  LIKE CSSL-LSTAR  , "AT
*         PERIOD LIKE RKU01G-PERBI, "Period
*         VEHTP  LIKE ZTCO_VEHI_TYPE-VEHTP,
*         MATNR  LIKE MARA-MATNR,
*         KOSTL  LIKE CSKS-KOSTL,
*         KOART  LIKE ZTCO_PLANDEP-KOART,
** Planned Dep. Cost
*         WAERS  LIKE ZTCO_PLANDEP-WAERS,
*         VALXX  LIKE ZTCO_PLANDEP-VAL01,
** KSPP
*         MEINH     LIKE COSSA-MEINH,
*         MEG_NTRAN LIKE COSSA-MEG001,      "Nicht übernommene Mengen
*         MEGBTR    LIKE COSSA-MEG001,
** MDPB.
*         MEINS     LIKE MARA-MEINS,
*         PLNMG     LIKE MDPB-PLNMG,
** ATq_SUM
*         AT_SUM    LIKE COSSA-MEG001,
*         AT_%(10)  TYPE P DECIMALS 6,
*         DEP_AT_%  LIKE ZTCO_PLANDEP-VAL01,
*         DEP_AT_%_PLM
*                   LIKE ZTCO_PLANDEP-VAL01.

ENDFORM.                    " FIELDCAT_INIT
*&---------------------------------------------------------------------*
*&      Form  Build_FieldCAT
*&---------------------------------------------------------------------*
*       Field_CAT
*----------------------------------------------------------------------*
*      -->P_0065   text
*      -->P_0066   text
*      -->P_0067   text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_0071   text
*      -->P_0072   text
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

ENDFORM.                    " Build_FieldCAT

*---------------------------------------------------------------------*
*       FORM PF_STATUS                                                *
*---------------------------------------------------------------------*
*       PF_STATUS                                                     *
*---------------------------------------------------------------------*
FORM PF_STATUS USING  EXTAB TYPE SLIS_T_EXTAB.
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
*// Mod. by Hyung Jin Youn 2004.04.08
* Deactivate Posting/reversing Function
*  -> See detail/Change Control in Technical Document
* Important part !
* For POST (Internal Order)
    WHEN 'POST' OR 'REVS'.
*      PERFORM POST_ADDICTIVE_COST USING UCOMM.
  ENDCASE.
*// End Of Mod.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM BASIC_TOP_OF_PAGE                                        *
*---------------------------------------------------------------------*
*       Display Title                                                 *
*---------------------------------------------------------------------*
FORM BASIC_TOP_OF_PAGE.
  WRITE : / 'Controlling Area/Company Code/Plant      : '
            , P_KOKRS, '/', P_BUKRS, '/', P_WERKS.
  WRITE : / 'Fiscal Year/Period/Version/Activity Type : '
            , P_GJAHR, '/', P_VONPE, '~', P_BISPE, '/', P_VERSN
            , '/', P_LSTAR.
  WRITE : / 'Version number/Per.Adj.: '
            , P_VERSB, '/', P_PCORR.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  POST_ADDICTIVE_COST
*&---------------------------------------------------------------------*
*       ADDICTIVE_COST
*----------------------------------------------------------------------*
*  -->  p_UCOMM   Sy-UCOMM
*----------------------------------------------------------------------*
FORM POST_ADDICTIVE_COST USING P_UCOMM.

  CLEAR : IT_POST.
  CLEAR : IT_TRANSFER_DATA, IT_TRANSFER_DATA[].
  DATA  : LV_POSNR  LIKE RK70L-POSNR.
  DATA  : IT_L_PS   LIKE IT_TRANSFER_DATA-POSITION
                    WITH HEADER LINE .
  DATA  : LV_TVERS LIKE CKI64A-TVERS.

  SORT IT_POST BY PERIOD MATNR .

  LOOP AT IT_POST.
    ON CHANGE OF IT_POST-PERIOD
              OR IT_POST-MATNR.
      CLEAR  IT_TRANSFER_DATA.
* Header DATA
      IT_TRANSFER_DATA-KLVAR = P_KLVAR.
      IT_TRANSFER_DATA-MATNR = IT_POST-MATNR.
      IT_TRANSFER_DATA-WERKS = P_WERKS.
* DELETED : Version is generated by period -> User Input
      IT_TRANSFER_DATA-TVERS = P_TVERS.
      CLEAR LV_TVERS.
*      IT_TRANSFER_DATA-TVERS = LV_TVERS = IT_POST-PERIOD.
* Period
* SKIP Valuation DATE
      CALL FUNCTION 'FIRST_DAY_IN_PERIOD_GET'
           EXPORTING
                I_GJAHR = P_GJAHR
                I_PERIV = TKA01-LMONA
                I_POPER = IT_POST-PERIOD
           IMPORTING
                E_DATE  = IT_TRANSFER_DATA-KADAT.
      CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
           EXPORTING
                I_GJAHR = P_GJAHR
                I_PERIV = TKA01-LMONA
                I_POPER = IT_POST-PERIOD
           IMPORTING
                E_DATE  = IT_TRANSFER_DATA-BIDAT.
* Clear ITEM DATA
      CLEAR : IT_TRANSFER_DATA-POSITION[],
              IT_TRANSFER_DATA-POSITION  .
      CLEAR : IT_L_PS, IT_L_PS[].
* Clear Item Counter
      CLEAR LV_POSNR.
    ENDON.
* Post Or Reverse
* Reverse Function make only header data so it will delete
* whole item data at once. In other word, old data will be removed
* whatever they are -> Only this program is supposed to use Addictive
* Costing.
    IF P_UCOMM = 'POST'. "POST
      LV_POSNR = LV_POSNR + 1.
      IT_L_PS-POSNR = LV_POSNR.
      IT_L_PS-TYPPS = 'V'.
      IT_L_PS-KSTAR = IT_POST-KOART.
      IT_L_PS-MENGE = '1'.
      CONCATENATE TEXT-010 SY-REPID
                  SY-UNAME SY-DATUM SY-UZEIT
             INTO IT_L_PS-LTEXT
             SEPARATED BY SPACE.
*// Mod. by Hyung Jin Youn 2004.02.09
* Not Gross & Fixed Price -> Fixed Value
      IT_L_PS-LPREIS   = IT_POST-DEP_AT_%_PLM.
      IT_L_PS-LPREIFX  = IT_POST-DEP_AT_%_PLM.
*      IT_L_PS-LWERTFX  = IT_POST-DEP_AT_%_PLM.
*// End. Of MOD .
      IT_L_PS-LPEINH  = '1'.
      APPEND IT_L_PS.
      CLEAR IT_L_PS.
    ELSE.                "reverse
    ENDIF.

    AT END OF MATNR.
      IT_TRANSFER_DATA-POSITION[] = IT_L_PS[].
      APPEND IT_TRANSFER_DATA.
      CLEAR  IT_TRANSFER_DATA.
    ENDAT.

    CLEAR IT_POST.
  ENDLOOP.

  CLEAR  IT_TRANSFER_DATA.

* posting
  PERFORM CALL_POST_FM.

ENDFORM.                    " POST_ADDICTIVE_COST

*&---------------------------------------------------------------------*
*&      Form  CALL_POST_FM
*&---------------------------------------------------------------------*
*       POST (Addictive Costing)
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_POST_FM.


  CALL FUNCTION 'KKPI_BDC_ADD_ON_COSTS_CREATE'
       EXPORTING
            CLIENT                 = SY-MANDT
            USER                   = SY-UNAME
            MODE                   = P_MODE
            UPDATE                 = 'A'
            IT_TRANSFER_DATA       = IT_TRANSFER_DATA[]
       EXCEPTIONS
            CALL_TRANSACTION_ERROR = 1
            NO_TRANSFER_DATA       = 2
            WRONG_MODE_PARAMETER   = 3
            OTHERS                 = 4.

  IF SY-SUBRC <> 0.
    ROLLBACK WORK.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " CALL_POST_FM
