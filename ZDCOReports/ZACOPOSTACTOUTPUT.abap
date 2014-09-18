************************************************************************
* Program Name      : ZACOPOSTACTOUTPUT
* Author            : Michelle Jeong
* Creation Date     : 12/01/2006
* Specifications By : Andy Choi
* Description       : Upload Post Activity Planning
************************************************************************
REPORT ZACOPOSTACTOUTPUT MESSAGE-ID ZMCO.

* type-pools
TYPE-POOLS: SLIS.

* Main internal table
DATA : BEGIN OF IT_UPLOAD OCCURS 0,
        SKF(10),
        VERID(3),
        KOSTL   LIKE  CSKS-KOSTL.
        INCLUDE STRUCTURE ZSCO_COSS_MEG01.                  "1~12 QTY
DATA : END OF  IT_UPLOAD.

DATA : IT_POST_PLN LIKE IT_UPLOAD OCCURS 0 WITH HEADER LINE.

* For BAPI
DATA: WA_HEADERINFO     LIKE BAPIPLNHDR,
      IT_INDEXSTRUCTURE TYPE TABLE OF BAPIACPSTRU WITH HEADER LINE,
      IT_COOBJECT       TYPE TABLE OF BAPIACPOBJ WITH HEADER LINE,
      IT_PERVALUE       TYPE TABLE OF BAPIACPVAL WITH HEADER LINE,
      IT_RETURN         TYPE TABLE OF BAPIRET2   WITH HEADER LINE.

* For ALV
DATA : GV_REPID        LIKE SY-REPID,
       GV_STATUS       TYPE SLIS_FORMNAME VALUE 'PF_STATUS',
       GV_USER_COMMAND TYPE SLIS_FORMNAME VALUE 'USER_COMMAND',
       IT_FIELDCAT     TYPE SLIS_T_FIELDCAT_ALV,
       WA_FIELDCAT     LIKE LINE OF IT_FIELDCAT,
       IT_EVENTCAT     TYPE SLIS_T_EVENT,
       WA_EVENTCAT     LIKE LINE OF IT_EVENTCAT,
       IT_EVENTS	  TYPE SLIS_T_EVENT,
       IT_EVENT_EXIT	  TYPE SLIS_T_EVENT_EXIT.

* Global Variant
DATA : GV_COL_POS TYPE I,
       GV_POST_DATE LIKE COHEADER-BUDAT.

* Macro For Transferring value in BAPI
DEFINE TRANS_VALUE.
  IT_PERVALUE-ACTVTY_QTY_PER&1  =  IT_POST_PLN-MEG0&1.
END-OF-DEFINITION.

DEFINE TRANS_VALUE_CAP.
  IT_PERVALUE-ACTVTY_CAP_PER&1  =  IT_POST_PLN-MEG0&1.
END-OF-DEFINITION.

*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-100.
SELECTION-SCREEN BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-001.
PARAMETERS: P_KOKRS LIKE CSKS-KOKRS MEMORY ID CAC OBLIGATORY,
            P_GJAHR LIKE COBK-GJAHR MEMORY ID GJR OBLIGATORY,
            P_PERID LIKE COSP-PERBL MEMORY ID BPE OBLIGATORY,
            P_TRUN(1).
SELECTION-SCREEN END   OF BLOCK BL1.

SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK BL3 WITH FRAME TITLE TEXT-022.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  1(12)  TEXT-020.
SELECTION-SCREEN POSITION 13.
PARAMETERS: P_QTY RADIOBUTTON GROUP RA01
            USER-COMMAND  CTY.
SELECTION-SCREEN COMMENT  25(10) TEXT-021.
SELECTION-SCREEN POSITION 35.
PARAMETER P_CAP  RADIOBUTTON GROUP RA01.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BL3.
SELECTION-SCREEN SKIP 1.
PARAMETER P_FILE  LIKE RLGRAP-FILENAME OBLIGATORY.
SELECTION-SCREEN END OF BLOCK B1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  PERFORM AT_SEL_SCREEN_ON_VALUE_REQUEST USING P_FILE 'O'.

*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Set Global ALV Parameter
  GV_REPID = SY-REPID.
  PERFORM UPLOAD_FILE.

*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.
* Call ALV LIST
  PERFORM CALL_ALV_LIST.

*&---------------------------------------------------------------------*
*&      Form  DIS_BAPI_MESSAGE
*&---------------------------------------------------------------------*
*        Display BAPI Message
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
FORM USER_COMMAND USING UCOMM    LIKE SY-UCOMM
                        SELFIELD TYPE SLIS_SELFIELD.
  IF UCOMM = 'POST'.
    SORT IT_UPLOAD.

    LOOP AT IT_UPLOAD.
      MOVE-CORRESPONDING IT_UPLOAD TO IT_POST_PLN.
      APPEND IT_POST_PLN.
      CLEAR IT_POST_PLN.

      AT END OF VERID.
        PERFORM CALL_BAPI.

        CLEAR IT_POST_PLN.
        REFRESH  IT_POST_PLN.
      ENDAT.
    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_INIT
*&---------------------------------------------------------------------*
*       Building Field Cat
*----------------------------------------------------------------------*
FORM FIELDCAT_INIT.
  DATA : L_CNT(3) TYPE N,
         L_FIELD(10),
         L_TEXT(10).
  CLEAR : GV_COL_POS, IT_FIELDCAT, IT_FIELDCAT[].

  PERFORM BUILD_FIELDCAT USING 'IT_UPLOAD' 'SKF'    'X' '' '' ''
                               '5'         'SKF'        '' '' ''.

  PERFORM BUILD_FIELDCAT USING 'IT_UPLOAD' 'VERID'  'X' '' '' ''
                               '6'         'Verison'    '' '' ''.

  PERFORM BUILD_FIELDCAT USING 'IT_UPLOAD' 'KOSTL'  'X' '' '' ''
                               '10'        'CostCenter' '' '' ''.

  CLEAR L_CNT.
  DO 12 TIMES.
    L_CNT  = L_CNT + 1.
    CONCATENATE 'MEG' L_CNT INTO L_FIELD.
    CONCATENATE 'Qty_' L_CNT+1(2) INTO L_TEXT.

    PERFORM BUILD_FIELDCAT USING 'IT_UPLOAD' L_FIELD '' 'X' '' ''
                                 '15'        L_TEXT  '' '' ''.
  ENDDO.

* Set Event
  DATA : WA_L_EVENT  TYPE SLIS_ALV_EVENT.
  WA_L_EVENT-NAME = SLIS_EV_TOP_OF_PAGE.
  WA_L_EVENT-FORM = 'BASIC_TOP_OF_PAGE'.
  APPEND WA_L_EVENT TO IT_EVENTS.


ENDFORM.                    " FIELDCAT_INIT
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT
*&---------------------------------------------------------------------*
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
  PERFORM FIELDCAT_INIT .

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
       EXPORTING
            I_CALLBACK_PROGRAM       = GV_REPID
            I_CALLBACK_PF_STATUS_SET = GV_STATUS
            I_CALLBACK_USER_COMMAND  = GV_USER_COMMAND
            IT_FIELDCAT              = IT_FIELDCAT[]
            I_SAVE                   = 'A'
            IT_EVENTS                = IT_EVENTS
            IT_EVENT_EXIT            = IT_EVENT_EXIT
       TABLES
            T_OUTTAB                 = IT_UPLOAD
       EXCEPTIONS
            PROGRAM_ERROR            = 1
            OTHERS                   = 2.

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
  WRITE : / 'Test Run              : ', P_TRUN.
  SKIP 1.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CALL_POST_FM
*&---------------------------------------------------------------------*
*&      Form  upload_file
*&---------------------------------------------------------------------*
FORM UPLOAD_FILE.
  CALL FUNCTION 'WS_UPLOAD'
       EXPORTING
            CODEPAGE                = ' '
            FILENAME                = P_FILE
            FILETYPE                = 'DAT'
       TABLES
            DATA_TAB                = IT_UPLOAD
       EXCEPTIONS
            CONVERSION_ERROR        = 1
            FILE_OPEN_ERROR         = 2
            FILE_READ_ERROR         = 3
            INVALID_TABLE_WIDTH     = 4
            INVALID_TYPE            = 5
            NO_BATCH                = 6
            UNKNOWN_ERROR           = 7
            GUI_REFUSE_FILETRANSFER = 8
            CUSTOMER_ERROR          = 9
            OTHERS                  = 10.

  CASE SY-SUBRC.
    WHEN 0.
      MESSAGE S000 WITH P_FILE ' is loaded'.
    WHEN OTHERS.
      MESSAGE E000 WITH 'Error during file upload'.
  ENDCASE.

ENDFORM.                    " upload_file
*&---------------------------------------------------------------------*
*&      Form  AT_SEL_SCREEN_ON_VALUE_REQUEST
*&---------------------------------------------------------------------*
FORM AT_SEL_SCREEN_ON_VALUE_REQUEST USING DEF_PATH LIKE RLGRAP-FILENAME
                                          MODE     TYPE C.

  DATA: TMP_FILENAME LIKE RLGRAP-FILENAME,
        TMP_MASK(80),
        FIELDLN TYPE I.

  FIELD-SYMBOLS <TMP_SYM>.

  TMP_MASK = ',*.*,*.*.'.
  FIELDLN = STRLEN( DEF_PATH ) - 1.
  ASSIGN DEF_PATH+FIELDLN(1) TO <TMP_SYM>.

  IF <TMP_SYM> = '/' OR <TMP_SYM> = '\'.
    CLEAR <TMP_SYM>.
  ENDIF.

  CALL FUNCTION 'WS_FILENAME_GET'
       EXPORTING
            DEF_FILENAME     = P_FILE
            DEF_PATH         = DEF_PATH
            MASK             = TMP_MASK
            MODE             = MODE
       IMPORTING
            FILENAME         = TMP_FILENAME
       EXCEPTIONS
            INV_WINSYS       = 01
            NO_BATCH         = 02
            SELECTION_CANCEL = 03
            SELECTION_ERROR  = 04.

  IF SY-SUBRC = 0.
    P_FILE = TMP_FILENAME.
  ENDIF.

ENDFORM.                    " AT_SEL_SCREEN_ON_VALUE_REQUEST
*&---------------------------------------------------------------------*
*&      Form  SET_VALUE_AMT
*&---------------------------------------------------------------------*
FORM SET_VALUE_AMT.
* Fixed Cost
* Variable Cost
  TRANS_VALUE 01. TRANS_VALUE 02. TRANS_VALUE 03. TRANS_VALUE 04.
  TRANS_VALUE 05. TRANS_VALUE 06. TRANS_VALUE 07. TRANS_VALUE 08.
  TRANS_VALUE 09. TRANS_VALUE 10. TRANS_VALUE 11. TRANS_VALUE 12.
  TRANS_VALUE 13. TRANS_VALUE 14. TRANS_VALUE 15. TRANS_VALUE 16.

ENDFORM.                    " SET_VALUE_AMT
*&---------------------------------------------------------------------*
*&      Form  CALL_BAPI
*&---------------------------------------------------------------------*
FORM CALL_BAPI.
  CLEAR: WA_HEADERINFO, IT_INDEXSTRUCTURE, IT_COOBJECT,
         IT_PERVALUE, IT_RETURN.
  REFRESH: IT_INDEXSTRUCTURE, IT_COOBJECT,
           IT_PERVALUE, IT_RETURN.

* Sort to post data.
  SORT IT_POST_PLN BY KOSTL.

  WA_HEADERINFO-CO_AREA       = P_KOKRS.
  WA_HEADERINFO-FISC_YEAR     = P_GJAHR.
  WA_HEADERINFO-PERIOD_FROM   = '001'.
  WA_HEADERINFO-PERIOD_TO	   = '012'.
  WA_HEADERINFO-VERSION       = IT_UPLOAD-VERID.
  WA_HEADERINFO-PLAN_CURRTYPE = 'C'.

  LOOP AT IT_POST_PLN.
*   Obj
    ON CHANGE OF  IT_POST_PLN-KOSTL.
*     Index of Object Key
      IT_INDEXSTRUCTURE-OBJECT_INDEX
           = IT_INDEXSTRUCTURE-OBJECT_INDEX + 1 .

      CLEAR IT_COOBJECT.
      IT_COOBJECT-OBJECT_INDEX = IT_INDEXSTRUCTURE-OBJECT_INDEX.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
           EXPORTING
                INPUT  = IT_POST_PLN-KOSTL
           IMPORTING
                OUTPUT = IT_COOBJECT-COSTCENTER.

      IT_COOBJECT-ACTTYPE     = IT_POST_PLN-SKF.

      APPEND IT_COOBJECT.
      CLEAR  IT_COOBJECT.
    ENDON.

*   Value.
*   Index of Value
    IT_INDEXSTRUCTURE-VALUE_INDEX
         = IT_INDEXSTRUCTURE-VALUE_INDEX + 1.

    CLEAR IT_PERVALUE.
    IT_PERVALUE-VALUE_INDEX = IT_INDEXSTRUCTURE-VALUE_INDEX.

    IF P_QTY = 'X'.
      PERFORM SET_VALUE_AMT.
    ELSEIF P_CAP = 'X'.
      PERFORM SET_VALUE_AMT_CAP.
    ENDIF.

    APPEND IT_PERVALUE.
    CLEAR  IT_PERVALUE.

    APPEND IT_INDEXSTRUCTURE.
    CLEAR IT_POST_PLN.
  ENDLOOP.
*
  CALL FUNCTION 'BAPI_COSTACTPLN_POSTACTOUTPUT'
       EXPORTING
            HEADERINFO     = WA_HEADERINFO
       TABLES
            INDEXSTRUCTURE = IT_INDEXSTRUCTURE
            COOBJECT       = IT_COOBJECT
            PERVALUE       = IT_PERVALUE
            RETURN         = IT_RETURN.

* Commit
  DATA LT_RETURN LIKE BAPIRET2.
  CLEAR LT_RETURN.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
       EXPORTING
            WAIT   = 'X'
       IMPORTING
            RETURN = LT_RETURN.

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

ENDFORM.                    " CALL_BAPI
*&---------------------------------------------------------------------*
*&      Form  SET_VALUE_AMT_CAP
*&---------------------------------------------------------------------*
FORM SET_VALUE_AMT_CAP.
  TRANS_VALUE_CAP 01. TRANS_VALUE_CAP 02. TRANS_VALUE_CAP 03.
  TRANS_VALUE_CAP 04. TRANS_VALUE_CAP 05. TRANS_VALUE_CAP 06.
  TRANS_VALUE_CAP 07. TRANS_VALUE_CAP 08. TRANS_VALUE_CAP 09.
  TRANS_VALUE_CAP 10. TRANS_VALUE_CAP 11. TRANS_VALUE_CAP 12.
  TRANS_VALUE_CAP 13. TRANS_VALUE_CAP 14. TRANS_VALUE_CAP 15.
  TRANS_VALUE_CAP 16.

ENDFORM.                    " SET_VALUE_AMT_CAP
