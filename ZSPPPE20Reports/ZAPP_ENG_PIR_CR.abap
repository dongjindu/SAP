************************************************************************
* Program Name      : ZIPP_ENG_PIR
* Author            : Furong Wang
* Creation Date     : 08/03/07
* Specifications By :
* Development Request No :
* Addl Documentation:
* Description       : Short/Long Term PIR Management (BAPI)
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
************************************************************************
REPORT ZIPP_ENG_PIR NO STANDARD PAGE HEADING
                          LINE-SIZE 1023
                          MESSAGE-ID ZMPP.

TYPE-POOLS M60VT .
TYPES BEGIN OF TY_TOTAL.
        INCLUDE STRUCTURE RM60PLVP.
TYPES:  STATUS TYPE C,
      END OF TY_TOTAL.
*----------------------------------------------------------------------*
*  EXTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
TABLES : T001W.              "Plants/Branches
TABLES : BAPISITEMR, "Communication fields:indep. reqmts item data table
         CM60R,      "Common work area for planned indep. req functions
         T371F,     "IB: Object Types for User (Owner/Observer)
         IBINOWN.   "IB: Owner of an IBase instance

DATA : BEGIN OF IT_HEADMATNR OCCURS 0,
         WERKS   LIKE  ZTPP_ENG_PIR-WERKS,   "PLANT
         VERSB   LIKE  PBIM-VERSB,             "VERSION
*         pbdnr   LIKE  pbim--pbdnr,   "REQUIREMENT PLAN No
         MATNR   LIKE  PBIM-MATNR.  "MATERIAL No
*         pver    LIKE  pbim-pver.  "PROD VERSION
DATA : END OF IT_HEADMATNR.

DATA: IT_ENG_PIR LIKE TABLE OF ZTPP_ENG_PIR WITH HEADER LINE.

DATA : BEGIN OF IT_ERROR OCCURS 0,
         PBDNR   LIKE  ZTPP_PMT07JB_C-PBDNR,   "REQUIREMENT PLAN No
         MATNR   LIKE  ZTPP_PMT07JB_C-MATNR,   "FSC
         MSGTY   LIKE  SY-MSGTY,               "STATUS
         MSG     LIKE  CFGNL-MSGLIN.           "MESSAGE
DATA : END OF IT_ERROR.


DATA : BEGIN OF IT_SUCCESS OCCURS 0,
         PBDNR   LIKE  ZTPP_PMT07JB_C-PBDNR.  "REQUIREMENT PLAN No
DATA : END OF IT_SUCCESS.

*----->

DATA:   IT_PBIM  LIKE TABLE OF PBIM    WITH HEADER LINE,

       IT_PBED  LIKE TABLE OF PBED    WITH HEADER LINE.

DATA : WA_VERSB_NB       LIKE  PBIM-VERSB,  "PIR VERSION
       WA_LINE_IX        LIKE  SY-TABIX,
       WA_SUCCESS_IX     LIKE  SY-TABIX,
       WA_ERROR_IX       LIKE  SY-TABIX,    "COUNT OF ERROR FOR LINE
*       WA_PERCENTAGE_PD  TYPE  P       ,
*       WA_FABKL          LIKE  T001W-FABKL,
       WA_MEINS          LIKE  MARA-MEINS,
       W_TYPE(1).


DATA : IT_BAPISSHDIN   LIKE TABLE OF BAPISSHDIN  WITH HEADER LINE,
       IT_BAPIRETURN   LIKE TABLE OF BAPIRETURN1 WITH HEADER LINE.

DATA: WA_DATUM             LIKE SY-DATUM,
      WA_FLAG              TYPE C       ,
      WA_ACTIVE(1).                     "ACTIVE CHECKBOX (Y:'X',N:'')

*----------------------------------------------------------------------*
*  CONSTANTS DECLARATION
*----------------------------------------------------------------------*
CONSTANTS: C_MARK   VALUE 'X',
           C_DATE   VALUE 'D',         "DATE TYPE
           C_WEEK   VALUE 'W',         "DATE TYPE
           C_REQTY  LIKE   T459U-BEDAE  VALUE 'BSF'.    "REQ TYPE

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.
PARAMETERS : P_WERKS      LIKE   T001W-WERKS DEFAULT 'E001'
                                             OBLIGATORY MEMORY ID WRK.

SELECTION-SCREEN SKIP 1.
PARAMETERS : RA_SHORT   RADIOBUTTON GROUP RA  DEFAULT 'X',
             RA_LONG    RADIOBUTTON GROUP RA.
SELECTION-SCREEN END OF BLOCK B1.


************************************************************************
* START-OF-SELECTION
************************************************************************
START-OF-SELECTION.
  PERFORM GET_DATA.
  IF IT_ENG_PIR[] IS INITIAL.
    MESSAGE I000 WITH 'No data'.
  ELSE.
    PERFORM EXCUTE_PROCESS.
  ENDIF.


************************************************************************
* END-OF-SELECTION
************************************************************************
END-OF-SELECTION.
  PERFORM LIST_PROCESS.

*&---------------------------------------------------------------------*
*&      Form  DATA_ARRANGE
*&---------------------------------------------------------------------*
FORM GET_DATA.
  DATA: LT_ENG_PIR LIKE TABLE OF ZTPP_ENG_PIR WITH HEADER LINE,
        L_MONDAY LIKE SY-DATUM,
         L_MONDAY_N LIKE SY-DATUM,
         L_FIRST LIKE SY-DATUM,
         L_FACID LIKE TFACS-IDENT.

  CASE C_MARK.
    WHEN RA_SHORT.
      WA_VERSB_NB = '00'.      "Short Term Plan
      WA_ACTIVE   = 'X'.
    WHEN RA_LONG.
      WA_VERSB_NB = '99'.      "Long Term Plan
      CLEAR WA_ACTIVE.
  ENDCASE.

  SELECT MAX( WDATU ) INTO WA_DATUM
    FROM ZTPP_ENG_PIR.

  IF RA_SHORT = 'X'.
    W_TYPE = '1'.

    SELECT * INTO TABLE IT_ENG_PIR
    FROM ZTPP_ENG_PIR
    WHERE ENTLU = W_TYPE
     AND WDATU = WA_DATUM
     AND PDATU >= SY-DATUM
     AND PLNMG > 0.
  ELSE.
    W_TYPE = '2'.
    SELECT * INTO TABLE IT_ENG_PIR
   FROM ZTPP_ENG_PIR
   WHERE ENTLU = W_TYPE
    AND WDATU = WA_DATUM
    AND PLNMG > 0.

    SELECT * INTO TABLE LT_ENG_PIR
    FROM ZTPP_ENG_PIR
    WHERE ENTLU = '1'
     AND WDATU = WA_DATUM
     AND PDATU >= SY-DATUM
     AND PLNMG > 0.

    SORT LT_ENG_PIR BY PDATU.
    READ TABLE LT_ENG_PIR INDEX 1.
    L_FIRST = LT_ENG_PIR-PDATU.

    L_MONDAY = L_FIRST + 7.
    L_MONDAY_N = L_MONDAY + 7.

    CALL FUNCTION 'GET_WEEK_INFO_BASED_ON_DATE'
     EXPORTING
       DATE          = L_MONDAY
     IMPORTING
*         WEEK          =
       MONDAY        = L_MONDAY
*         SUNDAY        =
              .
    L_MONDAY_N = L_MONDAY + 7.

    SELECT SINGLE FABKL INTO L_FACID
      FROM T001W
      WHERE WERKS = 'P001'.

    CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
         EXPORTING
              CORRECT_OPTION               = '+'
              DATE                         = L_MONDAY
              FACTORY_CALENDAR_ID          = L_FACID
         IMPORTING
              DATE                         = L_MONDAY
         EXCEPTIONS
              CALENDAR_BUFFER_NOT_LOADABLE = 1
              CORRECT_OPTION_INVALID       = 2
              DATE_AFTER_RANGE             = 3
              DATE_BEFORE_RANGE            = 4
              DATE_INVALID                 = 5
              FACTORY_CALENDAR_NOT_FOUND   = 6
              OTHERS                       = 7.

    CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
         EXPORTING
              CORRECT_OPTION               = '+'
              DATE                         = L_MONDAY_N
              FACTORY_CALENDAR_ID          = L_FACID
         IMPORTING
              DATE                         = L_MONDAY_N
         EXCEPTIONS
              CALENDAR_BUFFER_NOT_LOADABLE = 1
              CORRECT_OPTION_INVALID       = 2
              DATE_AFTER_RANGE             = 3
              DATE_BEFORE_RANGE            = 4
              DATE_INVALID                 = 5
              FACTORY_CALENDAR_NOT_FOUND   = 6
              OTHERS                       = 7.

    LOOP AT LT_ENG_PIR.
      IT_ENG_PIR = LT_ENG_PIR.
      IF LT_ENG_PIR-PDATU < L_MONDAY.
        IT_ENG_PIR-PDATU = L_FIRST.
      ELSEIF  LT_ENG_PIR-PDATU >= L_MONDAY_N.
        IT_ENG_PIR-PDATU = L_MONDAY_N.
      ELSE.
        IT_ENG_PIR-PDATU = L_MONDAY.
      ENDIF.
      COLLECT IT_ENG_PIR.
      CLEAR: IT_ENG_PIR, LT_ENG_PIR.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " DATA_ARRANGE

*&---------------------------------------------------------------------*
*&      Form  PROGRESS_INDICATOR
*&---------------------------------------------------------------------*
*FORM PROGRESS_INDICATOR USING  P_TEXT.
*  IF WA_LINE_IX <> 0.
*    WA_PERCENTAGE_PD = ( SY-TABIX / WA_LINE_IX ) * 100.
*  ENDIF.
*  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
*       EXPORTING
*            PERCENTAGE = WA_PERCENTAGE_PD
*            TEXT       = P_TEXT.
*
*ENDFORM.                    " PROGRESS_INDICATOR


*&---------------------------------------------------------------------*
*&      Form  EXCUTE_PROCESS
*&---------------------------------------------------------------------*
FORM EXCUTE_PROCESS.
  PERFORM DELETE_PIR.
  PERFORM GATHERING_DATA.
  PERFORM BAPI_EXECUTION.
ENDFORM.                    " EXCUTE_PROCESS

*&---------------------------------------------------------------------*
*&      Form  LIST_PROCESS
*&---------------------------------------------------------------------*
FORM LIST_PROCESS.
  DATA : L_LINE_IX   LIKE   SY-TABIX,
         L_SUCCESS_IX    LIKE   SY-TABIX,
         L_LINE          LIKE   SY-TABIX.

  DESCRIBE TABLE IT_ENG_PIR  LINES L_LINE_IX.
  IF L_LINE_IX = 0.
    WRITE :/ '*********** No Data found **************'.
  ELSE.
    SKIP 1.
    WRITE :/ TEXT-313 ,
             WA_SUCCESS_IX COLOR COL_POSITIVE.
    SKIP 1.
    LOOP AT IT_ERROR.
      AT FIRST.
        WRITE :/ '********** BEGIN OF ERROR Detail List ***********'.
      ENDAT.
      L_LINE = SY-TABIX MOD 2.
      IF L_LINE EQ 1.
        FORMAT COLOR COL_NORMAL INTENSIFIED ON.
      ELSE.
        FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
      ENDIF.
      WRITE :/ IT_ERROR-PBDNR COLOR COL_KEY,
               IT_ERROR-MATNR COLOR COL_KEY,
               IT_ERROR-MSGTY COLOR COL_NEGATIVE,
               IT_ERROR-MSG   COLOR COL_NORMAL.
      AT LAST.
        FORMAT RESET INTENSIFIED ON.
        WRITE :/ '********** END OF ERROR Detail List ***********'.
      ENDAT.
    ENDLOOP.

    SKIP 1.
    WRITE :/ '********** END OF PROCESS ***********'.
  ENDIF.
  WRITE AT: /001(030) 'End of processing ...(End)' ,
           031(010) SY-DATUM                    ,
           042(010) SY-UZEIT                    .

*  WRITE :/ '********** END OF PROCESS ***********'.
ENDFORM.                    " LIST_PROCESS

*&---------------------------------------------------------------------*
*&      Form  GATHERING_DATA
*&---------------------------------------------------------------------*
FORM GATHERING_DATA.
  DATA L_TABIX_IX   LIKE   SY-TABIX.

  DESCRIBE TABLE IT_ENG_PIR LINES WA_LINE_IX.
  LOOP AT IT_ENG_PIR.
    IT_HEADMATNR-WERKS = IT_ENG_PIR-WERKS.
    IT_HEADMATNR-MATNR = IT_ENG_PIR-MATNR.
    MOVE : WA_VERSB_NB  TO  IT_HEADMATNR-VERSB.
    COLLECT IT_HEADMATNR.
    CLEAR : IT_HEADMATNR.
  ENDLOOP.
  SORT IT_ENG_PIR BY WERKS MATNR.
ENDFORM.                    " GATHERING_DATA

*&---------------------------------------------------------------------*
*&      Form  BAPI_EXECUTION
*&---------------------------------------------------------------------*
FORM BAPI_EXECUTION.
  CLEAR : IT_SUCCESS, IT_SUCCESS[],
          IT_ERROR,   IT_ERROR[].
  CLEAR : WA_SUCCESS_IX, WA_ERROR_IX.

  LOOP AT IT_HEADMATNR.
    PERFORM GENERATE_BAPI_DATA.
  ENDLOOP.
ENDFORM.                    " BAPI_EXECUTION
*&---------------------------------------------------------------------*
*&      Form  GENERATE_BAPI_DATA
*&---------------------------------------------------------------------*
FORM GENERATE_BAPI_DATA.
  DATA L_BDZEI LIKE  PBIM-BDZEI.
*  DATA : L_INDICATOR     LIKE  SCAL-INDICATOR.
*  DATA : L_MSG           LIKE  CFGNL-MSGLIN.

  CLEAR : BAPISITEMR, CM60R, WA_MEINS,
          IT_BAPISSHDIN, IT_BAPISSHDIN[],
          IT_BAPIRETURN, IT_BAPIRETURN[].

  BAPISITEMR-MATERIAL   = IT_HEADMATNR-MATNR. "FSC
  BAPISITEMR-PLANT      = IT_HEADMATNR-WERKS. "PLANT
  BAPISITEMR-REQU_TYPE  = C_REQTY.            "VSE
  BAPISITEMR-VERSION    = IT_HEADMATNR-VERSB. "VERSION
  BAPISITEMR-VERS_ACTIV = WA_ACTIVE.          "ACTIVE Yes/No
  BAPISITEMR-REQ_NUMBER = 'ENG'.              "Req plan No

  SELECT SINGLE MEINS
                INTO WA_MEINS
                FROM MARA
                WHERE MATNR EQ IT_HEADMATNR-MATNR .

  LOOP AT IT_ENG_PIR WHERE WERKS EQ IT_HEADMATNR-WERKS
                         AND MATNR EQ IT_HEADMATNR-MATNR.

    IT_BAPISSHDIN-DATE_TYPE  = W_TYPE  .   "DATE TYPE
    IT_BAPISSHDIN-REQ_DATE   = IT_ENG_PIR-PDATU.   "DATE
    IT_BAPISSHDIN-REQ_QTY    = IT_ENG_PIR-PLNMG.   "QTY
    IT_BAPISSHDIN-UNIT       = WA_MEINS.            "UNIT
    IT_BAPISSHDIN-PROD_VES   = '01'.    "PROD VERSION
    APPEND IT_BAPISSHDIN.
    CLEAR IT_BAPISSHDIN.
  ENDLOOP.

  SORT IT_BAPISSHDIN BY DATE_TYPE REQ_DATE.

  SELECT SINGLE A~BDZEI
                 INTO L_BDZEI
                 FROM PBIM AS A INNER JOIN PBED AS B
                   ON A~BDZEI EQ B~BDZEI
                 WHERE A~WERKS EQ IT_HEADMATNR-WERKS  "PLANT
                   AND A~MATNR EQ IT_HEADMATNR-MATNR  "Material
                   AND A~BEDAE EQ C_REQTY           "REQUIREMENT TYPE
                   AND A~VERSB EQ IT_HEADMATNR-VERSB. "VERSION
*                     AND a~pbdnr EQ it_headmatnr-pbdnr. "REQ. Plan No

  IF SY-SUBRC EQ 0.
    CALL FUNCTION 'BAPI_REQUIREMENTS_CHANGE'
         EXPORTING
              MATERIAL                 = BAPISITEMR-MATERIAL
              PLANT                    = BAPISITEMR-PLANT
              REQUIREMENTSTYPE         = BAPISITEMR-REQU_TYPE
              VERSION                  = BAPISITEMR-VERSION
              REQMTSPLANNUMBER         = BAPISITEMR-REQ_NUMBER
              VERS_ACTIV               = BAPISITEMR-VERS_ACTIV
         TABLES
              REQUIREMENTS_SCHEDULE_IN = IT_BAPISSHDIN
              RETURN                   = IT_BAPIRETURN.

  ELSE.
    CALL FUNCTION 'BAPI_REQUIREMENTS_CREATE'
         EXPORTING
              REQUIREMENTS_ITEM        = BAPISITEMR
         TABLES
              REQUIREMENTS_SCHEDULE_IN = IT_BAPISSHDIN
              RETURN                   = IT_BAPIRETURN.

  ENDIF.

  IF IT_BAPIRETURN[] IS INITIAL.
    WA_SUCCESS_IX = WA_SUCCESS_IX + 1.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
  ELSE.
    LOOP AT IT_BAPIRETURN WHERE TYPE NE 'S'.
      WA_ERROR_IX = WA_ERROR_IX + 1.

      IT_ERROR-MATNR = IT_HEADMATNR-MATNR.
      MOVE IT_BAPIRETURN-TYPE        TO IT_ERROR-MSGTY.
      MOVE IT_BAPIRETURN-MESSAGE     TO IT_ERROR-MSG.
      APPEND IT_ERROR.
      CLEAR IT_ERROR.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " GENERATE_BAPI_DATA
*&---------------------------------------------------------------------*
*&      Form  DELETE_PIR
*&---------------------------------------------------------------------*
FORM DELETE_PIR.
  DATA : L_OBJECT     TYPE  IBINOWN-OBJKEY.

  SELECT SINGLE *
               FROM T371F
               WHERE OBJTYP EQ 'PBKO'.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_PBIM
    FROM PBIM
   WHERE VERSB = WA_VERSB_NB    "version
     AND WERKS = P_WERKS.       "Plant

  LOOP AT IT_PBIM.
*
    CLEAR L_OBJECT.
    L_OBJECT+0(18)  = IT_PBIM-MATNR.
    L_OBJECT+18(4)  = IT_PBIM-WERKS.
    L_OBJECT+22(4)  = IT_PBIM-BEDAE.
    L_OBJECT+26(2)  = IT_PBIM-VERSB.
    L_OBJECT+28(10) = IT_PBIM-PBDNR.

    CLEAR : IT_PBED, IT_PBED[].
    SELECT *
      INTO TABLE IT_PBED
      FROM PBED
     WHERE BDZEI = IT_PBIM-BDZEI.

    IF SY-SUBRC = 0.
      LOOP AT IT_PBED.
        DELETE FROM PBED WHERE BDZEI = IT_PBED-BDZEI
                           AND PDATU = IT_PBED-PDATU.
        L_OBJECT+38(10) = IT_PBED-PDATU.
        SELECT SINGLE *
                     FROM IBINOWN
                     WHERE INTTYP EQ T371F-INTTYP
                       AND OBJKEY EQ L_OBJECT.
        IF SY-SUBRC EQ 0.
          UPDATE IBINOWN SET DELFLAG = 'X'
                     WHERE INTTYP  EQ T371F-INTTYP
                       AND OBJKEY EQ L_OBJECT.
        ENDIF.
      ENDLOOP.
    ENDIF.
    DELETE FROM PBIM WHERE MATNR = IT_PBIM-MATNR
                       AND WERKS = IT_PBIM-WERKS
                       AND BEDAE = IT_PBIM-BEDAE
                       AND VERSB = IT_PBIM-VERSB
                       AND PBDNR = IT_PBIM-PBDNR .
  ENDLOOP.
ENDFORM.                    " DELETE_PIR

*&---------------------------------------------------------------------*
*&      Form  WRITE_ERROR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXT_010  text
*----------------------------------------------------------------------*
FORM WRITE_ERROR USING    PA_TEXT  PA_MATNR.
  IF WA_FLAG = 'X'.
    " Write Header's Format...
    ULINE AT (85) .
    WRITE AT: /001(85) TEXT-900.
    ULINE AT (85) .
  ENDIF.

  WRITE AT: /001(46) PA_TEXT ,
             048(03) ' : '   ,
             051(18) PA_MATNR,
             069(16) TEXT-019.
  ULINE AT (85) .
ENDFORM.                    " WRITE_ERROR
