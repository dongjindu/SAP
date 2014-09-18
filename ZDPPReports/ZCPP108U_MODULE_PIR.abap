************************************************************************
* Program Name      : ZCPP108U_MODULE_PIR
* Author            : JongOh, Kim
* Creation Date     : 2004.05.13
* Specifications By : JongOh, Kim
* Pattern           : 5.1.4.3
* Development Request No : UD1K910309
* Addl Documentation:
* Description       : PIR Management for Module BOM
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************

REPORT ZCPP108U_MODULE_PIR  NO STANDARD PAGE HEADING
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
TABLES : T001W,              " Plants/Branches
         MARA ,              " General Material Data
         MARC ,              " Plant Data for Material
         PLAF ,              " Planned order
         PBIM ,              " Independent requirements for material
         PBED .              " Independent requirements data

TABLES : BAPISITEMR, "Communication fields:indep. reqmts item data table
         CM60R.      "Common work area for planned indep. req functions

TABLES : T371F,     "IB: Object Types for User (Owner/Observer)
         IBINOWN.   "IB: Owner of an IBase instance
*----------------------------------------------------------------------*
*  INTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
*-----> Planned Order List
DATA : BEGIN OF IT_PLAF OCCURS 0,
          PWWRK   LIKE   PLAF-PWWRK ,            " Plant
          MATNR   LIKE   PLAF-MATNR ,            " Module code
          PSTTR   LIKE   PLAF-PSTTR ,            " Planned date
          GSMNG   LIKE   PLAF-GSMNG .            " Planned Qty
DATA : END OF IT_PLAF .

*-----> HEADER
DATA : BEGIN OF IT_HEAD OCCURS 0,
         WERKS   LIKE  PBIM-WERKS,               " PLANT
         VERSB   LIKE  PBIM-VERSB,               " VERSION
         MATNR   LIKE  PBIM-MATNR.               " MATERIAL No
DATA : END OF IT_HEAD.

*-----> DAILY ITEM
DATA : BEGIN OF IT_ITEM OCCURS 0,
         WERKS   LIKE  PBIM-WERKS,               "PLANT
         VERSB   LIKE  PBIM-VERSB,               "VERSION
*         PBDNR   LIKE  PBIM-PBDNR,               "REQUIREMENT PLAN No
         MATNR   LIKE  PBIM-MATNR,               "MODULE CODE
         PDATU   LIKE  PBED-PDATU,               "DATE
         PRGRS   LIKE  BAPISCHEDULE-DATE_TYPE,   "Date type
         PLNMG   LIKE  PBED-PLNMG.               "QTY
DATA : END OF IT_ITEM.


*-----> ERROR TABLE FOR PIR
DATA : BEGIN OF IT_ERROR OCCURS 0,
         MATNR   LIKE  PBIM-MATNR,               "MODULE CODE
         MSGTY   LIKE  SY-MSGTY,                 "STATUS
         MSG     LIKE  CFGNL-MSGLIN.             "MESSAGE
DATA : END OF IT_ERROR.

*-----> SUCCESS TABLE
DATA : BEGIN OF IT_SUCCESS OCCURS 0,
         MATNR   LIKE  PBIM-MATNR.               "MODULE CODE
DATA : END OF IT_SUCCESS.

*-----> ERROR TABLE FOR BDC
DATA : IT_ERR_BDC  LIKE TABLE OF IT_ERROR  WITH HEADER LINE .

*----->
DATA : IT_PBIM  LIKE TABLE OF PBIM    WITH HEADER LINE,
       IT_PBED  LIKE TABLE OF PBED    WITH HEADER LINE.

*----------------------------------------------------------------------*
*  GLOBAL VARIABLE DECLARATION
*----------------------------------------------------------------------*
DATA : WA_TOT_BDC_IX     LIKE  SY-TABIX,    "Total Count of BDC
       WA_SUC_BDC_IX     LIKE  SY-TABIX,    "Count of Success for BDC
       WA_ERR_BDC_IX     LIKE  SY-TABIX,    "Count of Error for BDC
       WA_TOTAL_IX       LIKE  SY-TABIX,    "Total Count of PIR
       WA_SUCCESS_IX     LIKE  SY-TABIX,    "COUNT OF SUCCESS for PIR
       WA_ERROR_IX       LIKE  SY-TABIX.    "COUNT OF ERROR for PIR
DATA : WA_BDC_FLG        .
DATA : WA_MEINS          LIKE  MARA-MEINS.

RANGES : R_PDATU  FOR  ZTPP_PMT07JB_C-PDATU.

*----------------------------------------------------------------------*
*  INTERNAL TABLE DECLARATION (BAPI)
*----------------------------------------------------------------------*
*-----> BAPI INPUT/OUTPUT TABLE
DATA : IT_BAPISSHDIN   LIKE TABLE OF BAPISSHDIN  WITH HEADER LINE,
       IT_BAPISCHARR   LIKE TABLE OF BAPISCHARR  WITH HEADER LINE,
       IT_BAPIRETURN   LIKE TABLE OF BAPIRETURN1 WITH HEADER LINE.

*----------------------------------------------------------------------*
*  GLOBAL VARIABLE DECLARATION(BAPI)
*----------------------------------------------------------------------*
*------> BDC MODE ('A' DISPLAY SCREEN, 'N' NO DISPLAY)
DATA: WA_DATUM             LIKE SY-DATUM,
      WA_ACTIVE(1).                     " ACTIVE CHECKBOX (Y:'X',N:'')

*----------------------------------------------------------------------*
*  CONSTANTS DECLARATION
*----------------------------------------------------------------------*
CONSTANTS: C_MARK   VALUE 'X',         " MARK
           C_DATE   VALUE '1',         " DATE TYPE
           C_MODE   VALUE 'N',         " BDC MODE
           C_VERSB  LIKE   PBIM-VERSB   VALUE 'M0' ,    "Version
           C_REQTY  LIKE   T459U-BEDAE  VALUE 'VSF'.    "REQ TYPE

*----------------------------------------------------------------------*
*  INTERNAL TABLE DECLARATION (BDC DATA)
*----------------------------------------------------------------------*
DATA : IT_BDCDATA     LIKE TABLE OF BDCDATA  WITH HEADER LINE.
DATA : WA_OPTION_DS   LIKE CTU_PARAMS.   "BDC OPTION

*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.
PARAMETERS : P_WERKS      LIKE   T001W-WERKS DEFAULT 'P001'
                                             OBLIGATORY MEMORY ID WRK.
SELECT-OPTIONS  S_MATNR   FOR  MARA-MATNR .
SELECTION-SCREEN END OF BLOCK B1.

************************************************************************
* INITIALIZAION
************************************************************************
INITIALIZATION.

************************************************************************
* AT SELECTION-SCREEN
************************************************************************
AT SELECTION-SCREEN.
  PERFORM AT_SELECTION-SCREEN.

************************************************************************
* START-OF-SELECTION
************************************************************************
START-OF-SELECTION.
  PERFORM SET_PARAMETERS.
  PERFORM EXCUTE_PROCESS.

************************************************************************
* END-OF-SELECTION
************************************************************************
END-OF-SELECTION.
  PERFORM LIST_PROCESS.

*&---------------------------------------------------------------------*
*&      Form  BDC_DYNPRO
*&---------------------------------------------------------------------*
FORM BDC_DYNPRO USING  P_PROGRAM
                       P_DYNPRO.
  CLEAR IT_BDCDATA.
  IT_BDCDATA-PROGRAM  = P_PROGRAM.
  IT_BDCDATA-DYNPRO   = P_DYNPRO.
  IT_BDCDATA-DYNBEGIN = 'X'.
  APPEND IT_BDCDATA.

ENDFORM.                    " BDC_DYNPRO

*&---------------------------------------------------------------------*
*&      Form  BDC_FIELD
*&---------------------------------------------------------------------*
FORM BDC_FIELD USING    P_FNAM
                        P_FVAL.
*  IF P_FVAL <> Nodata.
  CLEAR IT_BDCDATA.
  IT_BDCDATA-FNAM = P_FNAM.
  IT_BDCDATA-FVAL = P_FVAL.
  APPEND IT_BDCDATA.
*  ENDIF.

ENDFORM.                    " BDC_FIELD
*&---------------------------------------------------------------------*
*&      Form  AT_SELECTION-SCREEN
*&---------------------------------------------------------------------*
FORM AT_SELECTION-SCREEN.
  SELECT SINGLE *
             FROM T001W
             WHERE WERKS EQ P_WERKS .
  IF SY-SUBRC NE 0.
    MESSAGE E001 WITH TEXT-301.
  ENDIF.
ENDFORM.                    " AT_SELECTION-SCREEN
*&---------------------------------------------------------------------*
*&      Form  SET_PARAMETERS
*&---------------------------------------------------------------------*
FORM SET_PARAMETERS.
*----> SET BDC MODE OPTION
  PERFORM SET_MODE    .

ENDFORM.                    " SET_PARAMETERS
*&---------------------------------------------------------------------*
*&      Form  EXCUTE_PROCESS
*&---------------------------------------------------------------------*
FORM EXCUTE_PROCESS.

*----> WRITE START
  PERFORM WRITE_START.

*----> SOURCE DATA
  PERFORM SELECT_PLAF .

*----> Generate BAPI format
  PERFORM GATHERING_DATA.

*-----> Delete Past PIR
  PERFORM DELETE_PIR.

*----> BAPI LOGIC for PIR Management
  PERFORM BAPI_EXECUTION.

ENDFORM.                    " EXCUTE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  WRITE_START
*&---------------------------------------------------------------------*
FORM WRITE_START .
  WRITE :/ '********** BEGIN OF PROCESS ***********'.
  WRITE AT: /001(030) 'Processing Time...(Start)' ,
             031(010) SY-DATUM                    ,
             042(010) SY-UZEIT                    .
  WRITE :/ '********** BEGIN OF PROCESS ***********'.
  SKIP 2.
ENDFORM.                    " WRITE_START
*&---------------------------------------------------------------------*
*&      Form  SET_MODE
*&---------------------------------------------------------------------*
FORM SET_MODE.
  CLEAR : WA_OPTION_DS.
  WA_OPTION_DS-DISMODE = C_MODE.
  WA_OPTION_DS-DEFSIZE = 'X'.
  WA_OPTION_DS-UPDMODE = 'S'.
ENDFORM.                    " SET_MODE
*&---------------------------------------------------------------------*
*&      Form  SELECT_PLAF
*&---------------------------------------------------------------------*
FORM SELECT_PLAF.

  CLEAR : IT_PLAF , IT_PLAF[] .
  SELECT PWWRK
         MATNR
         PSTTR
         GSMNG
         INTO TABLE IT_PLAF
         FROM PLAF
         WHERE PWWRK EQ P_WERKS
           AND DISPO EQ 'M01'       " MRP Controller
*           AND PLSCN NE '801'       " Planning scenario
           AND MATNR IN S_MATNR .   " Module Code

ENDFORM.                    " SELECT_PLAF
*&---------------------------------------------------------------------*
*&      Form  GATHERING_DATA
*&---------------------------------------------------------------------*
FORM GATHERING_DATA.
  CLEAR : IT_ITEM    ,  IT_ITEM[] ,
          IT_HEAD    ,  IT_HEAD[] ,
          IT_BDCDATA ,  IT_BDCDATA[] .
  CLEAR : WA_TOT_BDC_IX , WA_SUC_BDC_IX , WA_ERR_BDC_IX ,
          WA_TOTAL_IX   , WA_SUCCESS_IX , WA_ERROR_IX   .

  SORT IT_PLAF BY PWWRK MATNR PSTTR .
  LOOP AT IT_PLAF .
    AT NEW MATNR .
      CLEAR : WA_BDC_FLG .
      MOVE: IT_PLAF-PWWRK  TO   IT_HEAD-WERKS ,
            IT_PLAF-MATNR  TO   IT_HEAD-MATNR ,
            C_VERSB        TO   IT_HEAD-VERSB .
      APPEND IT_HEAD .

*---> Check production strateqy '40' and create or change strategy
      CLEAR MARC .
      SELECT SINGLE *
                 FROM MARC
                 WHERE WERKS EQ P_WERKS
                   AND MATNR EQ IT_PLAF-MATNR .
      IF MARC-STRGR NE '40'.
*---> BDC Process for Changing strategy of Module Code
        PERFORM GENERATE_BDCDATA .
        PERFORM CALL_TRANSACTION .
      ENDIF.
    ENDAT.

    IF WA_BDC_FLG NE 'X' .
      MOVE : IT_PLAF-PWWRK   TO   IT_ITEM-WERKS ,
             IT_PLAF-MATNR   TO   IT_ITEM-MATNR ,
             IT_PLAF-PSTTR   TO   IT_ITEM-PDATU ,
             IT_PLAF-GSMNG   TO   IT_ITEM-PLNMG ,
             C_DATE          TO   IT_ITEM-PRGRS ,
             C_VERSB         TO   IT_ITEM-VERSB .
      COLLECT IT_ITEM .  CLEAR IT_ITEM .
    ENDIF.
  ENDLOOP .

ENDFORM.                    " GATHERING_DATA
*&---------------------------------------------------------------------*
*&      Form  GENERATE_BDCDATA
*&---------------------------------------------------------------------*
FORM GENERATE_BDCDATA.
*---> Total BDC count
  WA_TOT_BDC_IX = WA_TOT_BDC_IX + 1     .

  PERFORM BDC_DYNPRO  USING 'SAPLMGMM'	'0060'              .
  PERFORM BDC_FIELD   USING :
       'BDC_CURSOR'	'RMMG1-MATNR' ,
	 'BDC_OKCODE'	'/00'         ,
	 'RMMG1-MATNR'	IT_HEAD-MATNR .

  PERFORM BDC_DYNPRO  USING 'SAPLMGMM'	'0070'              .
  PERFORM BDC_FIELD   USING :
       'BDC_CURSOR'	'MSICHTAUSW-DYTXT(01)' ,
	 'BDC_OKCODE'	'=ENTR'                ,
	 'MSICHTAUSW-KZSEL(01)'	C_MARK           .

  PERFORM BDC_DYNPRO  USING 'SAPLMGMM'	'5004'              .
  PERFORM BDC_FIELD   USING :
       'BDC_OKCODE'	'/00'                  .

  PERFORM BDC_DYNPRO  USING 'SAPLMGMM'	'5004'              .
  PERFORM BDC_FIELD   USING :
       'BDC_OKCODE'	'=SP14'                .

  PERFORM BDC_DYNPRO  USING 'SAPLMGMM'	'0081'              .
  PERFORM BDC_FIELD   USING :
       'BDC_CURSOR'	'RMMG1-WERKS'          ,
	 'BDC_OKCODE'	'=ENTR'                ,
	 'RMMG1-WERKS'	P_WERKS                .


  PERFORM BDC_DYNPRO  USING 'SAPLMGMM'	'5000'              .
  PERFORM BDC_FIELD   USING :
       'BDC_OKCODE'	'=BU'                  ,
       'MARC-STRGR'	'40'                   .
ENDFORM.                    " GENERATE_BDCDATA
*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION
*&---------------------------------------------------------------------*
FORM CALL_TRANSACTION.
  CALL TRANSACTION 'MM02' USING IT_BDCDATA
                      OPTIONS FROM WA_OPTION_DS.
  PERFORM ERROR_TEXT .
ENDFORM.                    " CALL_TRANSACTION
*&---------------------------------------------------------------------*
*&      Form  ERROR_TEXT
*&---------------------------------------------------------------------*
FORM ERROR_TEXT.
  DATA L_MSG  LIKE CFGNL-MSGLIN.
  CLEAR IT_BDCDATA.
  REFRESH IT_BDCDATA.
  CALL FUNCTION 'RKC_MSG_STRING'
       EXPORTING
            ID      = SY-MSGID
            MTYPE   = SY-MSGTY
            NUMBER  = SY-MSGNO
            PAR1    = SY-MSGV1
            PAR2    = SY-MSGV2
            PAR3    = SY-MSGV3
            PAR4    = SY-MSGV4
       IMPORTING
            MSG_LIN = L_MSG
       EXCEPTIONS
            OTHERS  = 1.

  CASE SY-MSGTY.
    WHEN 'E' OR 'A' OR 'X' OR 'W'.
      WA_ERR_BDC_IX = WA_ERR_BDC_IX + 1.
      IT_ERR_BDC-MSGTY = 'E'.
      IT_ERR_BDC-MSG   = L_MSG.
      APPEND IT_ERR_BDC. CLEAR IT_ERR_BDC.
    WHEN OTHERS.   " 'I', 'S' :SUCCESS
      WA_SUC_BDC_IX = WA_SUC_BDC_IX + 1.
  ENDCASE .
ENDFORM.                    " ERROR_TEXT
*&---------------------------------------------------------------------*
*&      Form  DELETE_PIR
*&---------------------------------------------------------------------*
FORM DELETE_PIR.
  CLEAR : IT_PBIM, IT_PBIM[] .
  SELECT *
    INTO  TABLE IT_PBIM
    FROM PBIM
   WHERE VERSB = C_VERSB        "version
     AND WERKS = P_WERKS        "Plant
     AND MATNR IN S_MATNR .

  LOOP AT IT_PBIM.
    CLEAR : IT_PBED, IT_PBED[].
    SELECT *
      INTO TABLE IT_PBED
      FROM PBED
     WHERE BDZEI = IT_PBIM-BDZEI.

    IF SY-SUBRC = 0.
      LOOP AT IT_PBED.
        DELETE FROM PBED WHERE BDZEI = IT_PBED-BDZEI
                           AND PDATU = IT_PBED-PDATU.
      ENDLOOP.
    ENDIF.
    DELETE FROM PBIM WHERE MATNR = IT_PBIM-MATNR
                       AND WERKS = IT_PBIM-WERKS
                       AND VERSB = IT_PBIM-VERSB.
  ENDLOOP.

ENDFORM.                    " DELETE_PIR
*&---------------------------------------------------------------------*
*&      Form  BAPI_EXECUTION
*&---------------------------------------------------------------------*
FORM BAPI_EXECUTION.
  CLEAR : IT_SUCCESS, IT_SUCCESS[],
          IT_ERROR,   IT_ERROR[].
  CLEAR : WA_SUCCESS_IX, WA_ERROR_IX.

  LOOP AT IT_HEAD.
    PERFORM GENERATE_BAPI_DATA.
  ENDLOOP.
ENDFORM.                    " BAPI_EXECUTION
*&---------------------------------------------------------------------*
*&      Form  GENERATE_BAPI_DATA
*&---------------------------------------------------------------------*
FORM GENERATE_BAPI_DATA.
  CLEAR : BAPISITEMR   , WA_MEINS,
          IT_BAPISSHDIN, IT_BAPISSHDIN[],
          IT_BAPIRETURN, IT_BAPIRETURN[].

*----> GENERATE BAPISITEMR
  BAPISITEMR-MATERIAL   = IT_HEAD-MATNR.  " MODULE CODE
  BAPISITEMR-PLANT      = IT_HEAD-WERKS.  " PLANT
  BAPISITEMR-VERSION    = IT_HEAD-VERSB.  " VERSION
  BAPISITEMR-REQU_TYPE  = C_REQTY.        " VSF
  BAPISITEMR-VERS_ACTIV = WA_ACTIVE.      " ACTIVE Yes/No

*-----> UNIT & Check Material Master
  SELECT SINGLE MEINS
                INTO WA_MEINS
                FROM MARA
                WHERE MATNR EQ IT_HEAD-MATNR.

*----> GENERATE SCHEDULE LINE & CHARACTERISTICS
  PERFORM GENERATE_SCHEDULE_CHAR.

*-----> Delete Inputed Material Number at past
  SORT IT_BAPISSHDIN BY DATE_TYPE REQ_DATE.
  DATA L_BDZEI LIKE  PBIM-BDZEI.
  CLEAR L_BDZEI.
  SELECT SINGLE A~BDZEI
               INTO L_BDZEI
               FROM PBIM AS A INNER JOIN PBED AS B
                 ON A~BDZEI EQ B~BDZEI
               WHERE A~WERKS EQ IT_HEAD-WERKS     "PLANT
                 AND A~MATNR EQ IT_HEAD-MATNR     "Material
                 AND A~BEDAE EQ C_REQTY           "REQUIREMENT TYPE
                 AND A~VERSB EQ IT_HEAD-VERSB .   "VERSION WA_VERSB

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
*              REQUIREMENTS_CHAR_IN     = IT_BAPISCHARR
              RETURN                   = IT_BAPIRETURN.

  ELSE.
    CALL FUNCTION 'BAPI_REQUIREMENTS_CREATE'
         EXPORTING
              REQUIREMENTS_ITEM        = BAPISITEMR
         TABLES
              REQUIREMENTS_SCHEDULE_IN = IT_BAPISSHDIN
*              REQUIREMENTS_CHAR_IN     = IT_BAPISCHARR
              RETURN                   = IT_BAPIRETURN.

  ENDIF.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

  IF IT_BAPIRETURN[] IS INITIAL.
    WA_SUCCESS_IX = WA_SUCCESS_IX + 1.
  ELSE.
    LOOP AT IT_BAPIRETURN WHERE TYPE NE 'S'.
      WA_ERROR_IX = WA_ERROR_IX + 1.
      MOVE-CORRESPONDING IT_ITEM     TO IT_ERROR.
      MOVE IT_BAPIRETURN-TYPE        TO IT_ERROR-MSGTY.
      MOVE IT_BAPIRETURN-MESSAGE     TO IT_ERROR-MSG.
      APPEND IT_ERROR.
      CLEAR IT_ERROR.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " GENERATE_BAPI_DATA
*&---------------------------------------------------------------------*
*&      Form  GENERATE_SCHEDULE_CHAR
*&---------------------------------------------------------------------*
FORM GENERATE_SCHEDULE_CHAR.

  LOOP AT IT_ITEM WHERE WERKS EQ IT_HEAD-WERKS
                    AND VERSB EQ IT_HEAD-VERSB
                    AND MATNR EQ IT_HEAD-MATNR .
    IT_BAPISSHDIN-DATE_TYPE  = IT_ITEM-PRGRS.   "DATE TYPE
    IT_BAPISSHDIN-REQ_DATE   = IT_ITEM-PDATU.   "DATE
    IT_BAPISSHDIN-REQ_QTY    = IT_ITEM-PLNMG.   "QTY
    IT_BAPISSHDIN-UNIT       = WA_MEINS.            "UNIT
    APPEND IT_BAPISSHDIN.
    CLEAR IT_BAPISSHDIN.
  ENDLOOP .
ENDFORM.                    " GENERATE_SCHEDULE_CHAR
*&---------------------------------------------------------------------*
*&      Form  LIST_PROCESS
*&---------------------------------------------------------------------*
FORM LIST_PROCESS.

  DESCRIBE TABLE IT_HEAD LINES WA_TOTAL_IX .

* Total count of uploaded Module code
  WRITE :/ TEXT-101 , WA_TOTAL_IX COLOR COL_HEADING .
  IF WA_TOT_BDC_IX GT 0 .
* Total count of changed strategy
    WRITE :/ TEXT-102 , WA_TOT_BDC_IX  .
* Success count of changed strategy
    WRITE :/ TEXT-103 , WA_SUC_BDC_IX  .
* Error count of changed strategy
    WRITE :/ TEXT-104 , WA_ERR_BDC_IX  COLOR COL_NEGATIVE.
  ENDIF.
* Success count of PIR for module code
  WRITE :/ TEXT-105 , WA_SUCCESS_IX  .
* Error count of PIR for module code
  WRITE :/ TEXT-106 , WA_ERROR_IX    COLOR COL_NEGATIVE .

  DATA : L_FLG .
*-----> Detail List for  Error Changed strategy
  LOOP AT IT_ERR_BDC .
    AT FIRST .
      SKIP 2 .
      FORMAT INTENSIFIED ON .
      WRITE :/ TEXT-107 .
    ENDAT .
    IF L_FLG EQ 'X'.
      CLEAR L_FLG .
      FORMAT INTENSIFIED ON .
    ELSE.
      L_FLG = 'X' .
      FORMAT INTENSIFIED OFF .
    ENDIF.
    WRITE :/ IT_ERR_BDC-MATNR COLOR COL_KEY,
             IT_ERR_BDC-MSGTY COLOR COL_NEGATIVE,
             IT_ERR_BDC-MSG   COLOR COL_NORMAL .
    AT LAST .
      FORMAT INTENSIFIED ON .
      WRITE :/ TEXT-109 .
    ENDAT.
  ENDLOOP.

*-----> Detail List for PIR Error
  LOOP AT IT_ERROR .
    AT FIRST .
      SKIP 2 .
      FORMAT INTENSIFIED ON .
      WRITE :/ TEXT-108 .
    ENDAT.
    IF L_FLG EQ 'X'.
      CLEAR L_FLG .
      FORMAT INTENSIFIED ON .
    ELSE.
      L_FLG = 'X' .
      FORMAT INTENSIFIED OFF .
    ENDIF.
    WRITE :/ IT_ERROR-MATNR COLOR COL_KEY,
             IT_ERROR-MSGTY COLOR COL_NEGATIVE,
             IT_ERROR-MSG   COLOR COL_NORMAL .
    AT LAST .
      FORMAT INTENSIFIED ON .
      WRITE :/ TEXT-109 .

    ENDAT .
  ENDLOOP.

*----> WRITE END
  PERFORM WRITE_END.
ENDFORM.                    " LIST_PROCESS
*&---------------------------------------------------------------------*
*&      Form  WRITE_END
*&---------------------------------------------------------------------*
FORM WRITE_END.
  SKIP 2.
  FORMAT INTENSIFIED ON .
  WRITE :/ '********** END OF PROCESS ***********'.
  WRITE AT: /001(030) 'Processing Time...(Start)' ,
             031(010) SY-DATUM                    ,
             042(010) SY-UZEIT                    .
  WRITE :/ '********** END OF PROCESS ***********'.
  SKIP 2.
ENDFORM.                    " WRITE_END
