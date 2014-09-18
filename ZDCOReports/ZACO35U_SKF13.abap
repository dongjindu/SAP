************************************************************************
* Program Name      : ZACO35U_SKF13
* Author            : Eun Hwa , Jung
* Creation Date     : 2003.12.16.
* Specifications By : Jin won, Hong
* Pattern           : Report 1-1
* Development Request No : UD
* Addl Documentation:
* Description       : Create No of Products for Trim Shop


* the BDC structures for BATCH INPUT processing
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************


REPORT ZACO35U_SKF13 MESSAGE-ID ZMCO.

** type-pools
TYPE-POOLS: SLIS.

** Table
TABLES :  MARA, MSEG.


** Internal Table

DATA: BEGIN OF IT_MARA OCCURS 0,
         MATNR TYPE MARA-MATNR,
         MTART TYPE MARA-MTART,
*         WERKS TYPE MARC-WERKS,
*         FEVOR TYPE MARC-FEVOR,
      END OF IT_MARA.

DATA : BEGIN OF IT_MSEG OCCURS 0,
       MJAHR TYPE MKPF-MJAHR,
       BUDAT TYPE MKPF-BUDAT,
       BWART TYPE MSEG-BWART,
       MATNR TYPE MSEG-MATNR,
       WERKS TYPE MSEG-WERKS,
       END OF IT_MSEG.

DATA : BEGIN OF IT_IPGO OCCURS 0,
       MBLNR TYPE MSEG-MBLNR,
       BWART TYPE MSEG-BWART,
       MATNR TYPE MSEG-MATNR,
       WERKS TYPE MSEG-WERKS,
       END OF IT_IPGO.

DATA : BEGIN OF IT_CHUISO OCCURS 0,
       MBLNR TYPE MSEG-MBLNR,
       BWART TYPE MSEG-BWART,
       MATNR TYPE MSEG-MATNR,
       WERKS TYPE MSEG-WERKS,
       END OF IT_CHUISO.


* for reporting
DATA : BEGIN OF IT_REPORT OCCURS 0,
        BLDAT   LIKE COHEADER-BLDAT,          " doc date
        BUDAT   LIKE COHEADER-BUDAT,         " Posting date
        SENBUSPROC LIKE BAPIAAITM-SENBUSPROC, "Sender business process
        RECBUSPROC LIKE BAPIAAITM-RECBUSPROC, "Receiver business process
        TQTY    TYPE I,
      END OF IT_REPORT.



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

*** For BAPI
DATA : IT_RETURN         LIKE STANDARD TABLE OF BAPIRET2
                         WITH HEADER LINE.
DATA : WA_DOC_HEADER LIKE BAPIDOCHDRP .
DATA : IT_DOC_ITEMS  LIKE STANDARD TABLE OF BAPIAAITM
                      WITH HEADER LINE.


DATA : QTY TYPE I.

RANGES : RS_DATE FOR MARA-ERSDA.
*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-001.
PARAMETERS :
             P_KOKRS LIKE CSKS-KOKRS MEMORY ID CAC OBLIGATORY,
             P_GJAHR LIKE COBK-GJAHR MEMORY ID GJR OBLIGATORY,
             P_PERID LIKE COSP-PERBL MEMORY ID BPE OBLIGATORY,
             P_VERSN LIKE COBK-VERSN MEMORY ID KVT OBLIGATORY,
             P_TRUN(1).
SELECTION-SCREEN SKIP 1.
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

*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Read Material Info.
  PERFORM READ_MSEG.
* Preparation of reporting data.
  PERFORM PRE_REPORT_DATA.
* Preparation of ALV
  PERFORM PRE_REPORT_ADJ.

*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.
* Call ALV LIST
  PERFORM CALL_ALV_LIST.

*&---------------------------------------------------------------------*
*&      Form  READ_MSEG
*&---------------------------------------------------------------------*
*       Read Quality Info.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_MSEG.

  DATA :  DAT1 LIKE SY-DATUM,
          DAT2 LIKE SY-DATUM.
  CLEAR : DAT1, DAT2.

  CONCATENATE P_GJAHR P_PERID+1(2) '01' INTO DAT1.

  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
       EXPORTING
            DAY_IN            = DAT1
       IMPORTING
            LAST_DAY_OF_MONTH = DAT2.
  IF SY-SUBRC <> 0.
     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

**--- CREATE RANGES FOR PERIOD.
  RS_DATE-SIGN = 'I'.
  RS_DATE-OPTION = 'BT'.
  RS_DATE-LOW = DAT1.
  RS_DATE-HIGH = DAT2.
  APPEND RS_DATE.   CLEAR RS_DATE.


***--  Read Quality Notification  info.

  CLEAR : IT_MARA, IT_MARA[].
  CLEAR MARA.
  SELECT MATNR MTART
         INTO CORRESPONDING FIELDS OF TABLE IT_MARA
         FROM MARA
           WHERE MTART EQ 'FERT'.
  CLEAR  IT_MARA.

  SORT IT_MARA BY MATNR.


  CLEAR : IT_MSEG, IT_MSEG[].
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE IT_MSEG
      FROM ZMKPF_MSEG_V CLIENT SPECIFIED
        FOR ALL ENTRIES IN IT_MARA
         WHERE  MANDT = SY-MANDT
         AND   MJAHR = P_GJAHR
         AND   BUDAT IN RS_DATE
         AND   BWART IN ('131', '132')
         AND   MATNR = IT_MARA-MATNR
         AND   WERKS = 'P001'.
  CLEAR IT_MSEG.


** QTY = ( BWART : '131' ) - ( BWART : '132' )
  CLEAR : IT_IPGO, IT_IPGO[], IT_CHUISO, IT_CHUISO[].
  CLEAR IT_MSEG.
  LOOP AT IT_MSEG.
    IF IT_MSEG-BWART = '131'.
      MOVE-CORRESPONDING IT_MSEG TO IT_IPGO.
      APPEND IT_IPGO.
      CLEAR  IT_IPGO.
    ELSEIF IT_MSEG-BWART = '132'.
      MOVE-CORRESPONDING IT_MSEG TO IT_CHUISO.
      APPEND IT_CHUISO.
      CLEAR  IT_CHUISO.
    ENDIF.
  ENDLOOP.


  DATA  : LINE  TYPE I.
  DATA  : LINE2 TYPE I.
  CLEAR : LINE, LINE2.
  DESCRIBE TABLE IT_IPGO   LINES LINE.
  DESCRIBE TABLE IT_CHUISO LINES LINE2.

  CLEAR : QTY.
  QTY = LINE - LINE2 .


ENDFORM.                    " READ_MSEG
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
    'IT_REPORT' 'BUDAT'  'X'          SPACE    SPACE
    SPACE '10'      'Post Date'    SPACE    SPACE SPACE.


** Value
  PERFORM BUILD_FIELDCAT USING
  'IT_REPORT' 'SENBUSPROC'  SPACE            SPACE    SPACE
  SPACE '12'      'Send.Process'    SPACE    SPACE SPACE.

  PERFORM BUILD_FIELDCAT USING
  'IT_REPORT' 'RECBUSPROC'  SPACE            SPACE    SPACE
  SPACE '12'      'Rec. Process'    SPACE    SPACE SPACE.

*  PERFORM BUILD_FIELDCAT USING
*    'IT_REPORT' 'UNIT'  SPACE            SPACE    SPACE
*    SPACE   '4'      'Unit'      'UNIT'   SPACE     SPACE.

  PERFORM BUILD_FIELDCAT USING
   'IT_REPORT' 'TQTY'  SPACE            SPACE    SPACE
   SPACE '15'      'Total Qty'    SPACE    SPACE SPACE.



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
*       call ALV function
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
    WHEN 'POST'.
      PERFORM POST_STD  USING UCOMM.
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
            , P_GJAHR, '/', P_PERID , '/', P_VERSN.

  WRITE : / 'Test Run                     ', P_TRUN.
  SKIP 1.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  POST_STD
*&---------------------------------------------------------------------*
*        The Preparation for posting
*----------------------------------------------------------------------*
*      -->P_UCOMM  text
*----------------------------------------------------------------------*
FORM POST_STD USING    P_UCOMM.

  DELETE IT_REPORT WHERE TQTY EQ '0'.

  DATA  : LINE  TYPE I.
  CLEAR : LINE.
  DESCRIBE TABLE IT_REPORT LINES LINE.

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
  WA_DOC_HEADER-DOCDATE           = IT_REPORT-BLDAT.
  WA_DOC_HEADER-POSTGDATE         = IT_REPORT-BUDAT.
  WA_DOC_HEADER-VERSION           = P_VERSN.
  WA_DOC_HEADER-VARIANT           = 'SAP08'.
  WA_DOC_HEADER-DOC_HDR_TX        = LV_CONF_TEXT.
  WA_DOC_HEADER-USERNAME          = SY-UNAME.

* Fill Object List
  CLEAR : IT_DOC_ITEMS, IT_DOC_ITEMS[].

  LOOP AT IT_REPORT.

    IT_DOC_ITEMS-SENBUSPROC = IT_REPORT-SENBUSPROC.
    IT_DOC_ITEMS-RECBUSPROC = IT_REPORT-RECBUSPROC.
    IT_DOC_ITEMS-ACTVTY_QTY = IT_REPORT-TQTY.
*    IT_DOC_ITEMS-ACTIVITYUN = 'EA'.
    APPEND IT_DOC_ITEMS.
  ENDLOOP.

* Call BAPI FM
  PERFORM CALL_POST_FM.     " KB21NP- BUSINESS PROCESS ALLOCATION

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
            IM_VERSN         =   P_VERSN.
*           IM_KOSTL_F        =
*           IM_KOSTL_T        =
*           IM_GNAME          =
*           IM_PRZNR_F
*           IM_PRZNR_T       =
*         IMPORTING
*           SUBRC            =

  ENDIF.

ENDFORM.                    " POST_STD_CCTR_AT_CE
*&---------------------------------------------------------------------*
*&      Form  CALL_POST_FM
*&---------------------------------------------------------------------*
*       Call bapi function
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
*&      Form  CHK_INPUT_VALUE
*&---------------------------------------------------------------------*
*       check input value
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHK_INPUT_VALUE.

* Check Input Value (Period)
  IF P_PERID < 0 OR P_PERID > 12.
    MESSAGE E007(ZMCO) WITH P_PERID .
  ENDIF.


* Check TEST-RUN  Flag
  IF P_TRUN NA 'X '.
    MESSAGE E008(ZMCO).
  ENDIF.


ENDFORM.                    " CHK_INPUT_VALUE
*&---------------------------------------------------------------------*
*&      Form  PRE_REPORT_DATA
*&---------------------------------------------------------------------*
*        The preparation for reporting
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRE_REPORT_DATA.

  CLEAR : IT_REPORT, IT_REPORT[].
** <  Posting , hard coding > **
* doc date / posting date   =  ' YEAR/MONTH/28 '
* send.business process     =  'P4101-55101'
* receiver.business process =  ' DUMMY-PROC'
  CONCATENATE P_GJAHR P_PERID+1(2) '28'
              INTO IT_REPORT-BLDAT.
  CONCATENATE P_GJAHR P_PERID+1(2) '28'
              INTO IT_REPORT-BUDAT.
  IT_REPORT-SENBUSPROC = 'P4101-55101'.
  IT_REPORT-RECBUSPROC = 'DUMMY-PROC'.
  IT_REPORT-TQTY = QTY.
  APPEND IT_REPORT.

* doc date / posting date   =  ' YEAR/MONTH/28 '
* send.business process     =  'P6207-55102'
* receiver.business process =  ' DUMMY-PROC'
  CONCATENATE P_GJAHR P_PERID+1(2) '28'
              INTO IT_REPORT-BLDAT.
  CONCATENATE P_GJAHR P_PERID+1(2) '28'
              INTO IT_REPORT-BUDAT.
  IT_REPORT-SENBUSPROC = 'P6207-55102'.
  IT_REPORT-RECBUSPROC = 'DUMMY-PROC'.
  IT_REPORT-TQTY = QTY.
  APPEND IT_REPORT.

  CLEAR IT_REPORT.


ENDFORM.                    " PRE_REPORT_DATA
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
  SORT IT_REPORT BY SENBUSPROC.
  CLEAR IT_REPORT.


* Set Event
  DATA : WA_L_EVENT  TYPE SLIS_ALV_EVENT.
  WA_L_EVENT-NAME = SLIS_EV_TOP_OF_PAGE.
  WA_L_EVENT-FORM = 'BASIC_TOP_OF_PAGE'.
  APPEND WA_L_EVENT TO IT_EVENTS.


ENDFORM.                    " PRE_REPORT_ADJ
