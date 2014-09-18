************************************************************************
* Program Name      : ZEPM01_ASSET
* Author            : Myoungho, Park
* Creation Date     : 2003.10.28.
* Specifications By :
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       :   This Functional Specification defines the
* connections between FI's Asset Master and PM¡¯s Equipment Master and
* the Interface Field
*
* Modification Logs
* Date       Developer        RequestNo     Description
*
*
************************************************************************
REPORT ZEPM01_ASSET  NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID ZMPM.

TYPE-POOLS: SLIS.

TABLES: ANLA,   "//Asset Master Record Segment
        ANLC,   "//Asset Value Fields
        EQUI,   "//Equipment master data
        ITOB.   "//PM technical objects (EQUI, funcational location)

*** temp tbale
DATA: BEGIN OF IT_TEMP OCCURS 0,
        BUKRS LIKE ITOB-BUKRS,
        ANLNR LIKE ITOB-ANLNR,
        EQUNR LIKE ITOB-EQUNR,
      END OF IT_TEMP.

*** asset tbale
DATA: BEGIN OF IT_ASSET OCCURS 0,
        ANLNR LIKE ITOB-ANLNR,         "//Main asset number
        EQUNR LIKE ITOB-EQUNR,         "//Equipment number
        ZUGDT LIKE V_ANLSUM_1-ZUGDT,   "//Asset value date
                                       "//of the first posting
        ANSWL LIKE V_ANLSUM_1-ANSWL,   "//Transactions for
                                  "//the year affecting asset values
        WAERS LIKE T093B-WAERS,       "//Currency Key
        ANLN2 TYPE ANLN2,
      END OF IT_ASSET.

*** message list tbale
DATA: BEGIN OF IT_MESSAGE OCCURS 0,
        TYPE    LIKE BAPIRET2-TYPE,
        ID      LIKE BAPIRET2-ID,
        NUMBER  LIKE BAPIRET2-NUMBER,
        MESSAGE LIKE BAPIRET2-MESSAGE,
      END OF IT_MESSAGE.

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

********** SELECTION-SCREEN ********************************
************************************************************
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS : S_BUKRS  FOR ANLA-BUKRS  NO INTERVALS
                                          NO-EXTENSION,
                 S_EQART  FOR EQUI-EQART ,
                 S_EQUNR  FOR EQUI-EQUNR .

SELECTION-SCREEN END OF BLOCK BLOCK1.


INITIALIZATION.
  S_BUKRS-LOW = 'H201'.
  APPEND S_BUKRS.

  GV_REPID = SY-REPID.

START-OF-SELECTION.


END-OF-SELECTION.

  PERFORM GET_ASSET_VALUE.

  CHECK SY-SUBRC EQ 0.

  PERFORM UPDATE_EQUIP_VLAUE.

* Preparation of ALV
  PERFORM PRE_REPORT_ADJ.

* Call ALV LIST
  PERFORM CALL_ALV_LIST.

*&---------------------------------------------------------------------*
*&      Form  GET_ASSET_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_ASSET_VALUE.

* Acquisition date : Asset value date of the first posting (ANLA-ZUGDT)
* to ITOB-ANSDT.

* Asset Value: Transactions for the year affecting asset values
* (ANLC-ANSWL) to ITOB-ANSWT .
* Sum values only Deprecation area (ANLB-AFABE) = 01.


  SELECT DISTINCT BUKRS ANLNR EQUNR
         INTO CORRESPONDING FIELDS OF TABLE IT_TEMP
         FROM ITOB
         WHERE BUKRS IN S_BUKRS
         AND   EQART IN S_EQART
         AND   EQUNR IN S_EQUNR.

  IF SY-SUBRC EQ 0.
    LOOP AT IT_TEMP.
      SELECT ANLN1 AS ANLNR ANLN2
             ZUGDT
             SUM( ANSWL ) AS ANSWL
             APPENDING  CORRESPONDING FIELDS OF TABLE IT_ASSET
             FROM  V_ANLSUM_1
             WHERE BUKRS = IT_TEMP-BUKRS
             AND   ANLN1 = IT_TEMP-ANLNR
             AND   AFABE = '01'
             GROUP BY ANLN1 ANLN2 ZUGDT. " zugdt. " answl.
      IF SY-SUBRC EQ 0.
        MOVE IT_TEMP-EQUNR TO IT_ASSET-EQUNR.
        MODIFY IT_ASSET TRANSPORTING EQUNR WHERE EQUNR EQ SPACE.
      ENDIF.

    ENDLOOP.

    SELECT  SINGLE WAERS INTO IT_ASSET-WAERS
             FROM  T093B
             WHERE BUKRS IN S_BUKRS
             AND   AFABE = '01'.
    MODIFY IT_ASSET TRANSPORTING WAERS WHERE WAERS EQ SPACE.
  ENDIF.

ENDFORM.                    " GET_ASSET_VALUE
*&---------------------------------------------------------------------*
*&      Form  UPDATE_EQUIP_VLAUE
*&---------------------------------------------------------------------*
*       Update Euipment master using BAPI..
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_EQUIP_VLAUE.
  DATA: WA_DATA_GENERAL      LIKE BAPI_ITOB,
        WA_DATA_GENERALX     LIKE BAPI_ITOBX,
        WA_DATA_SPECIFIC     LIKE BAPI_ITOB_EQ_ONLY,
        WA_DATA_SPECIFICX    LIKE BAPI_ITOB_EQ_ONLYX,
        WA_DATA_GENERAL_EXP  LIKE BAPI_ITOB,
        WA_DATA_SPECIFIC_EXP LIKE BAPI_ITOB_EQ_ONLY,
        RETURN               LIKE BAPIRET2,
        WA_MESSAGE           LIKE BAPIRET2.

  DATA: WA_MESSAGE1(30),
        WA_MESSAGE2(30),
        WA_MESSAGE3(30),
        WA_ANSWL(18) TYPE N.

  LOOP AT IT_ASSET.
    CLEAR: WA_DATA_GENERAL,     WA_DATA_GENERALX,
           WA_DATA_SPECIFIC,    WA_DATA_SPECIFICX,
           WA_DATA_GENERAL_EXP ,WA_DATA_SPECIFIC_EXP,
           RETURN .

    MOVE: IT_ASSET-ZUGDT TO WA_DATA_GENERAL-ACQDATE,
          IT_ASSET-ANSWL TO WA_DATA_GENERAL-ACQUISVAL,
          IT_ASSET-WAERS TO WA_DATA_GENERAL-CURRENCY.

    MOVE: 'X'            TO WA_DATA_GENERALX-ACQDATE,
          'X'            TO WA_DATA_GENERALX-ACQUISVAL,
          'X'            TO WA_DATA_GENERALX-CURRENCY.

**** PM BAPI: Change Equipment
    CALL FUNCTION 'BAPI_EQUI_CHANGE'
         EXPORTING
              EQUIPMENT         = IT_ASSET-EQUNR
              DATA_GENERAL      = WA_DATA_GENERAL
              DATA_GENERALX     = WA_DATA_GENERALX
              DATA_SPECIFIC     = WA_DATA_SPECIFIC
              DATA_SPECIFICX    = WA_DATA_SPECIFICX
         IMPORTING
              DATA_GENERAL_EXP  = WA_DATA_GENERAL_EXP
              DATA_SPECIFIC_EXP = WA_DATA_SPECIFIC_EXP
              RETURN            = RETURN.
**** error check
    IF RETURN-TYPE  = 'E'.
      ROLLBACK WORK.
      CALL FUNCTION 'BALW_BAPIRETURN_GET2'
           EXPORTING
                TYPE   = RETURN-TYPE
                CL     = RETURN-ID
                NUMBER = RETURN-NUMBER
                PAR1   = RETURN-MESSAGE_V1
                PAR2   = RETURN-MESSAGE_V2
                PAR3   = RETURN-MESSAGE_V3
                PAR4   = RETURN-MESSAGE_V4
           IMPORTING
                RETURN = WA_MESSAGE.

      CLEAR:  IT_MESSAGE.
      MOVE-CORRESPONDING WA_MESSAGE TO IT_MESSAGE.
      APPEND IT_MESSAGE.
    ELSE.
      COMMIT WORK.
      MOVE : 'S'  TO IT_MESSAGE-TYPE.
      CONCATENATE TEXT-001 IT_ASSET-EQUNR INTO WA_MESSAGE1.
      CONCATENATE TEXT-002 IT_ASSET-ZUGDT INTO WA_MESSAGE2.
      WRITE IT_ASSET-ANSWL TO WA_ANSWL CURRENCY IT_ASSET-WAERS.
      CONCATENATE TEXT-003 WA_ANSWL IT_ASSET-WAERS INTO WA_MESSAGE3.
      CONCATENATE WA_MESSAGE1 WA_MESSAGE2 WA_MESSAGE3
                  INTO IT_MESSAGE-MESSAGE.
      APPEND IT_MESSAGE.
      CLEAR : IT_MESSAGE, WA_MESSAGE1, WA_MESSAGE2,
              WA_MESSAGE3, WA_ANSWL.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " UPDATE_EQUIP_VLAUE
*&---------------------------------------------------------------------*
*&      Form  PRE_REPORT_ADJ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRE_REPORT_ADJ.
* Building Field Cat.
  CLEAR : GV_COL_POS, IT_FIELDCAT, IT_FIELDCAT[].

* Key
  PERFORM BUILD_FIELDCAT USING
    'IT_MESSAGE' 'TYPE'  'X'     SPACE SPACE
     SPACE    '1'     'Type'  SPACE SPACE SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_MESSAGE' 'ID'  'X'     SPACE SPACE
     SPACE    '20'     'Message ID'  SPACE SPACE SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_MESSAGE' 'NUMBER'  'X'     SPACE SPACE
     SPACE    '3'     'Message Number'  SPACE SPACE SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_MESSAGE' 'MESSAGE'  'X'     SPACE SPACE
     SPACE    '100'     'Message'  SPACE SPACE SPACE.

*** Sort
  SORT IT_MESSAGE BY TYPE ID NUMBER MESSAGE.
  CLEAR: IT_MESSAGE.

  IT_SORT-FIELDNAME = 'TYPE'.
  IT_SORT-UP        = 'X'.
  IT_SORT-EXPA      = 'X'.
  IT_SORT-SUBTOT    = 'X'.
  APPEND IT_SORT.

  IT_SORT-FIELDNAME = 'ID'.
  IT_SORT-UP        = 'X'.
  IT_SORT-EXPA      = 'X'.
  IT_SORT-SUBTOT    = 'X'.
  APPEND IT_SORT.

  IT_SORT-FIELDNAME = 'NUMBER'.
  IT_SORT-UP        = 'X'.
  IT_SORT-EXPA      = 'X'.
  IT_SORT-SUBTOT    = 'X'.
  APPEND IT_SORT.

  IT_SORT-FIELDNAME = 'MESSAGE'.
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
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_ALV_LIST.
  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
       EXPORTING
            I_CALLBACK_PROGRAM       = GV_REPID
            I_CALLBACK_PF_STATUS_SET = GV_STATUS
            I_CALLBACK_USER_COMMAND  = GV_USER_COMMAND
            IT_FIELDCAT              = IT_FIELDCAT[]
            IT_SORT                  = IT_SORT[]
            I_SAVE                   = 'A'
            IT_EVENTS                = IT_EVENTS
            IT_EVENT_EXIT            = IT_EVENT_EXIT  "
       TABLES
            T_OUTTAB                 = IT_MESSAGE
       EXCEPTIONS
            PROGRAM_ERROR            = 1
            OTHERS                   = 2.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " CALL_ALV_LIST
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
*---------------------------------------------------------------------*
*       FORM BASIC_TOP_OF_PAGE                                        *
*---------------------------------------------------------------------*
*       Display Title                                                 *
*---------------------------------------------------------------------*
FORM BASIC_TOP_OF_PAGE.

  WRITE  AT 20 'Resault of Equipment & asset Master synchronization'
         INVERSE COLOR 3.
  SKIP.
  WRITE : / 'Company code        : ', S_BUKRS-LOW.
  WRITE : / 'Technical obj. type : ', S_EQART-LOW, ' ~ ', S_EQART-HIGH.
  WRITE : / 'Equipment           : ', S_EQUNR-LOW, ' ~ ', S_EQUNR-HIGH.
  WRITE : / 'Date                : ', SY-DATUM.
  SKIP.
ENDFORM.
*-----------------------------------------------------------------------
*    FORM PF_STATUS_VAR
*-----------------------------------------------------------------------
FORM PF_STATUS USING  EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'BALVLIST'  EXCLUDING EXTAB.
ENDFORM.
