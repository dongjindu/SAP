************************************************************************
* Program Name      : ZEPP320U_PLANNING_TABLE
* Author            : JongOh, Kim
* Creation Date     : 2003.10.21.
* Specifications By : JongOh, Kim
* Pattern           : 2.3
* Development Request No : UD1K901977
* Addl Documentation:
* Description       : Maintenance of Planning Table (BDC T-Code: MDP1)
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

REPORT ZEPP320U_PLANNING_TABLE  NO STANDARD PAGE HEADING
                              LINE-SIZE 500
                              MESSAGE-ID ZMPP.

*----------------------------------------------------------------------*
*  EXTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
TABLES:  CUVTAB,          "Variant table basic data
         CUVTAB_VALC,     "CHAR format values for variant table
         CUVTLN,          "Line object of variant table
         ZTBM_ABXCLSDT.   "Class

*----------------------------------------------------------------------*
*  INTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
DATA: IT_ABXCLSDT   LIKE TABLE OF ZTBM_ABXCLSDT WITH HEADER LINE.
DATA: IT_KSML       LIKE TABLE OF KSML          WITH HEADER LINE.

DATA: IT_CUVTAB_OLD LIKE TABLE OF CUVTAB_VALC   WITH HEADER LINE,
      IT_CUVTLN_OLD LIKE TABLE OF CUVTLN        WITH HEADER LINE,
      IT_CUVTAB_NEW LIKE TABLE OF CUVTAB_VALC   WITH HEADER LINE,
      IT_CUVTLN_NEW LIKE TABLE OF CUVTLN        WITH HEADER LINE,
      IT_CUVTAB_VALN LIKE TABLE OF CUVTAB_VALN  WITH HEADER LINE.

*----------------------------------------------------------------------*
*  GLOBAL VARIABLE DECLARATION
*----------------------------------------------------------------------*
DATA : WA_KLAH_FLG ,   "EXISTENCE IN KLAH ('X':TRUE, '':FALSE)
       WA_CABN_FLG ,   "EXISTENCE IN CABN ('X':TRUE, '':FALSE)
       WA_KSML_FLG ,   "EXISTENCE IN KSML ('X':TRUE, '':FALSE)
       WA_CUVTAB_FLG . "EXISTENCE CUVTAB ('X':TRUE, '':FALSE)

DATA : WA_CLINT    LIKE KLAH-CLINT.    "Internal Class Number
DATA : WA_ATINN    LIKE CABN-ATINN.    "Internal characteristic
DATA : WA_VTINT    LIKE CUVTAB-VTINT.  "Internal number of variant table

DATA : WA_CLTY     TYPE ZTBM_ABXCLSDT-CLTY,   "CLASS TYPE
       WA_FLAL     TYPE ZTBM_ABXCLSDT-FLAL.   "VALID FROM

DATA : WA_CUVTAB_MSG   LIKE CFGNL-MSGLIN. "Msg of Planning Table Created

DATA : WA_ABXCLSDT_IX  LIKE SY-TABIX.

DATA : WA_CUVTAB_PD    TYPE P,    "Count of Planning table Created
       WA_VALC_PD      TYPE P,    "Count of Planning table value created
       WA_ERROR_PD     TYPE P,    "Count of Error
       WA_TOTAL_PD     TYPE P.    "Total count Uploaded
*----------------------------------------------------------------------*
*  INTERNAL TABLE DECLARATION (BDC DATA)
*----------------------------------------------------------------------*
DATA : IT_BDCDATA     LIKE TABLE OF BDCDATA  WITH HEADER LINE.

*----------------------------------------------------------------------*
*  GLOBAL VARIABLE DECLARATION(BDC)
*----------------------------------------------------------------------*
*------> BDC MODE ('A' DISPLAY SCREEN, 'N' NO DISPLAY)
DATA: WA_MODE,                          "BDC MODE
      WA_ACTIVE(1).                     "ACTIVE CHECKBOX (Y:'X',N:'')
DATA: WA_OPTION_DS   LIKE CTU_PARAMS.   "BDC OPTION

*----------------------------------------------------------------------*
*  CONSTANTS DECLARATION
*----------------------------------------------------------------------*
CONSTANTS: C_MARK    VALUE  'X',
           C_MODE    VALUE  'A'.

*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.
SELECT-OPTIONS: S_DATUM FOR SY-DATUM.
SELECTION-SCREEN END OF BLOCK B1.

************************************************************************
* INITIALIZAION
************************************************************************
INITIALIZATION.
  PERFORM INITIALIZATION.

************************************************************************
* AT SELECTION-SCREEN
************************************************************************
AT SELECTION-SCREEN.
*  PERFORM AT_SELECTION-SCREEN.

************************************************************************
* START-OF-SELECTION
************************************************************************
START-OF-SELECTION.
  PERFORM EXECUTE_PROCESS.

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
*&      Form  EXECUTE_PROCESS
*&---------------------------------------------------------------------*
FORM EXECUTE_PROCESS.

*----> SELECT ZTBM_ABXCLSDT
  PERFORM SELECT_ABXCLSDT.

*----> SET BDC MODE
  PERFORM SET_MODE.

*----> (T-CODE: MDP1 , MDP4)
  LOOP AT IT_ABXCLSDT.
    WA_ABXCLSDT_IX = SY-TABIX.
    MOVE : IT_ABXCLSDT-CLTY  TO  WA_CLTY,  "CLASS TYPE
           IT_ABXCLSDT-FLAL  TO  WA_FLAL.  "VALID FROM

    AT NEW CLID.     "CLASS
*----> CHECK CLASS EXISTENCE
      PERFORM CHECK_CLASS_EXISTENCE.
    ENDAT.

*----> CHECK CHARACTERISTIC EXISTENCE & PLANNING TABLE EXISTENCE
*      & CREATE PLANNING TABLE OF CHARACTERISTIC(T-CODE: MDP1)
    IF WA_KLAH_FLG EQ C_MARK.       "Class Existed
      PERFORM CREATE_PLANNING_TABLE.
*----> CREATE VALUE OF PLANNING TABLE FIELD (T-CODE: MDP4)
      PERFORM CREATE_PLANNING_TABLE_VALUE.
    ELSE.
      WA_ERROR_PD = WA_ERROR_PD + 1.
      MOVE  'E'       TO IT_ABXCLSDT-ZRESULT.
      MOVE  TEXT-304  TO IT_ABXCLSDT-ZMSG.
      MODIFY IT_ABXCLSDT  INDEX  WA_ABXCLSDT_IX.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " EXECUTE_PROCESS

*&---------------------------------------------------------------------*
*&      Form  SELECT_ABXCLSDT
*&---------------------------------------------------------------------*
FORM SELECT_ABXCLSDT.
  DATA : L_ZBTIM   LIKE   SY-UZEIT.
  CLEAR L_ZBTIM.
  CLEAR : IT_ABXCLSDT,  IT_ABXCLSDT[].
*          IT_HEAD,      IT_HEAD[].
  SELECT *
         INTO TABLE IT_ABXCLSDT
         FROM ZTBM_ABXCLSDT
          WHERE ZBDAT IN S_DATUM
            AND ZBTIM NE L_ZBTIM.
  IF SY-SUBRC EQ 0.
*----> SORT BY CLASS, CHARACTERISTIC, CHARACTERISTIC VALUE
    SORT IT_ABXCLSDT BY CLID CHID FLAL CHVL.
    CLEAR : IT_ABXCLSDT-ZRESULT,
            IT_ABXCLSDT-ZMSG.
    MODIFY IT_ABXCLSDT TRANSPORTING ZRESULT ZMSG
           WHERE MANDT EQ SY-MANDT.
  ENDIF.
ENDFORM.                    " SELECT_ABXCLSDT

*&---------------------------------------------------------------------*
*&      Form  SET_MODE
*&---------------------------------------------------------------------*
FORM SET_MODE.
*----> SET BDC MODE OPTION
  CLEAR : WA_OPTION_DS.
  WA_OPTION_DS-DISMODE = C_MODE.
  WA_OPTION_DS-DEFSIZE = 'X'.
  WA_OPTION_DS-UPDMODE = 'S'.
ENDFORM.                    " SET_MODE

*&---------------------------------------------------------------------*
*&      Form  CHECK_CLASS_EXISTENCE
*&---------------------------------------------------------------------*
FORM CHECK_CLASS_EXISTENCE.
*----> CHECK CLASS & CHARACTERISTIC CREATION.
  CLEAR : WA_CLINT, IT_KSML, IT_KSML[].
  SELECT SINGLE CLINT
               INTO WA_CLINT
               FROM KLAH
               WHERE CLASS EQ IT_ABXCLSDT-CLID  "CLASS ID
                 AND KLART EQ WA_CLTY  "CLASS TYPE
                 AND VONDT LE WA_FLAL. "VALID FROM
  IF SY-SUBRC EQ 0.
    WA_KLAH_FLG = 'X'.       "KLAH TRUE
    SELECT *
          INTO TABLE IT_KSML
          FROM KSML
          WHERE CLINT EQ WA_CLINT.
    SORT IT_KSML BY IMERK.

  ELSE.
    CLEAR WA_KLAH_FLG.       "KLAH FALSE
  ENDIF.

ENDFORM.                    " CHECK_CLASS_EXISTENCE

*&---------------------------------------------------------------------*
*&      Form  CREATE_PLANNING_TABLE
*&---------------------------------------------------------------------*
FORM CREATE_PLANNING_TABLE.
*----> CHECK CHARACTERISTIC EXISTENCE
  CLEAR : WA_CABN_FLG, WA_KSML_FLG, WA_CUVTAB_FLG,
          WA_ATINN,    WA_VTINT.
  SELECT SINGLE ATINN
             INTO WA_ATINN    "Characteristic Internal No
             FROM CABN
             WHERE ATNAM EQ IT_ABXCLSDT-CHID. "Characteristic Name
  IF SY-SUBRC EQ 0.
    WA_CABN_FLG = C_MARK.     "Characteristic Existed
    READ TABLE IT_KSML WITH KEY IMERK = WA_ATINN.
    IF SY-SUBRC EQ 0.
*----> CHECK PLANNING TABLE CREATED
      WA_KSML_FLG = C_MARK.   "Characteristics of a Class
      SELECT SINGLE VTINT
                 INTO WA_VTINT
                 FROM CUVTAB
                 WHERE VTNAM EQ IT_ABXCLSDT-CHID.
      IF SY-SUBRC EQ 0.
        CLEAR WA_CUVTAB_FLG.  "Planning Table Existed
      ELSE.
*-----> CREATE PLANNING TABLE (T-CODE: MDP1)
        WA_CUVTAB_FLG = C_MARK.  "Planning Table Not Existed
        PERFORM CREATE_PLANNING_TABLE_BDC.
      ENDIF.
    ELSE.
      WA_ERROR_PD = WA_ERROR_PD + 1.
      CLEAR WA_KSML_FLG.  "Not Characteristics of a Class
      MOVE  'E'        TO IT_ABXCLSDT-ZRESULT.
      MOVE  TEXT-301   TO IT_ABXCLSDT-ZMSG.
      MODIFY IT_ABXCLSDT  INDEX  WA_ABXCLSDT_IX.
    ENDIF.
  ELSE.
    WA_ERROR_PD = WA_ERROR_PD + 1.
    CLEAR WA_CABN_FLG.   "Characteristic Not Existed
    MOVE  'E'       TO IT_ABXCLSDT-ZRESULT.
    MOVE  TEXT-302  TO IT_ABXCLSDT-ZMSG.
    MODIFY IT_ABXCLSDT  INDEX  WA_ABXCLSDT_IX.
  ENDIF.

ENDFORM.                    " CREATE_PLANNING_TABLE

*&---------------------------------------------------------------------*
*&      Form  CREATE_PLANNING_TABLE_BDC
*&---------------------------------------------------------------------*
FORM CREATE_PLANNING_TABLE_BDC.
  PERFORM GENERATE_BDCDATA_MDP1.
  PERFORM CALL_TRANSACTION_MDP1.
ENDFORM.                    " CREATE_PLANNING_TABLE_BDC

*&---------------------------------------------------------------------*
*&      Form  GENERATE_BDCDATA_MDP1
*&---------------------------------------------------------------------*
FORM GENERATE_BDCDATA_MDP1.
  DATA : L_DATUM(10).
  WRITE : IT_ABXCLSDT-FLAL TO L_DATUM.
  PERFORM BDC_DYNPRO USING : 'SAPLCUTU'	'1100'.
  PERFORM BDC_FIELD USING :
      'BDC_CURSOR'	'RCUTU-VTNAM',
      'BDC_OKCODE'	'=BASD',
      'RCUTU-VTNAM'	IT_ABXCLSDT-CHID,  "Planning table
      'RCUTU-DATUM'	L_DATUM.           "Valid from Date

  PERFORM BDC_DYNPRO USING : 'SAPLCUTU'	'1200'.
  PERFORM BDC_FIELD USING :
      'BDC_CURSOR'	'RCUTU-VTSTA',
      'BDC_OKCODE'	'=PARM',
      'RCUTU-VTTXT'	IT_ABXCLSDT-CHID,   "PLANNING TABLE TEXT
      'RCUTU-VTSTA'	IT_ABXCLSDT-STAT,   "STATUS (IT_ABXCLSDT-STAT)
      'RCUTU-VTGRU'     'COLOR'.            "GROUP

  PERFORM BDC_DYNPRO USING : 'SAPLCUTU'	'1300'.
  PERFORM BDC_FIELD USING :
      'BDC_CURSOR'	'RCUTU-ATNAM(01)',
      'BDC_OKCODE'	'/00',
      'RCUTU-ATNAM(01)'	IT_ABXCLSDT-CHID.

  PERFORM BDC_DYNPRO USING : 'SAPLCUTU'	'1300'.
  PERFORM BDC_FIELD USING : 'BDC_OKCODE'	'=SAVE'.

ENDFORM.                    " GENERATE_BDCDATA_MDP1
*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION_MDP1
*&---------------------------------------------------------------------*
FORM CALL_TRANSACTION_MDP1.
  CALL TRANSACTION 'MDP1' USING IT_BDCDATA
                         OPTIONS FROM WA_OPTION_DS.
  PERFORM ERROR_TEXT.
  CLEAR : IT_BDCDATA.
  REFRESH : IT_BDCDATA.

ENDFORM.                    " CALL_TRANSACTION_MDP1
*&---------------------------------------------------------------------*
*&      Form  ERROR_TEXT
*&---------------------------------------------------------------------*
FORM ERROR_TEXT.
  CLEAR : WA_CUVTAB_MSG.
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
            MSG_LIN = WA_CUVTAB_MSG
       EXCEPTIONS
            OTHERS  = 1.

  CASE SY-MSGTY.
    WHEN 'E' OR 'A' OR 'X' OR 'W'.
      WA_ERROR_PD = WA_ERROR_PD + 1.
      MOVE  'E'            TO IT_ABXCLSDT-ZRESULT.
      MOVE  WA_CUVTAB_MSG  TO IT_ABXCLSDT-ZMSG.
      MODIFY IT_ABXCLSDT  INDEX  WA_ABXCLSDT_IX.
    WHEN OTHERS.                       " 'I', 'S' :SUCCESS
      WA_CUVTAB_PD = WA_CUVTAB_PD + 1.
  ENDCASE.

ENDFORM.                    " ERROR_TEXT
*&---------------------------------------------------------------------*
*&      Form  CREATE_PLANNING_TABLE_VALUE
*&---------------------------------------------------------------------*
FORM CREATE_PLANNING_TABLE_VALUE.
  CLEAR : IT_CUVTAB_OLD, IT_CUVTAB_OLD[],
          IT_CUVTLN_OLD, IT_CUVTLN_OLD[],
          IT_CUVTAB_NEW, IT_CUVTAB_NEW[],
          IT_CUVTLN_NEW, IT_CUVTLN_NEW[].

*----> CREATE CHARACTERISTIC VALUE OF PLANNING TABLE (T-CODE: MDP4)
  IF WA_KLAH_FLG EQ C_MARK       "Class Existed
     AND WA_CABN_FLG EQ C_MARK   "Characteristic Existed
     AND WA_KSML_FLG EQ C_MARK.  "Characteristic of Class Existed
    CLEAR CUVTAB.
*---> Check Planning Table Existed
    SELECT SINGLE *
               FROM CUVTAB
               WHERE VTNAM EQ IT_ABXCLSDT-CHID.  "PLANNIG TABLE
    IF SY-SUBRC EQ 0.
*---> Check CHAR format values for variant table
      SELECT SINGLE *
                 FROM CUVTAB_VALC
                 WHERE VTINT EQ CUVTAB-VTINT      "Table Internal No
                   AND VALC  EQ IT_ABXCLSDT-CHVL. "Field Value
      IF SY-SUBRC NE 0.
*---> Check CHAR format values for variant table
        SELECT *
               INTO TABLE IT_CUVTAB_OLD
               FROM CUVTAB_VALC
               WHERE VTINT EQ CUVTAB-VTINT.  "Table Internal No
        IF SY-SUBRC EQ 0.
          SORT IT_CUVTAB_OLD BY SLNID DESCENDING.
          READ TABLE IT_CUVTAB_OLD INDEX 1.
*---> Check Line object of variant table
          SELECT *
                 INTO TABLE IT_CUVTLN_OLD
                 FROM CUVTLN
                 WHERE VTINT EQ CUVTAB-VTINT. "Table Internal No
          SORT IT_CUVTLN_OLD BY SLNID DESCENDING.
          READ TABLE IT_CUVTLN_OLD INDEX 1.
          MOVE-CORRESPONDING : IT_CUVTAB_OLD TO IT_CUVTAB_NEW,
                               IT_CUVTLN_OLD TO IT_CUVTLN_NEW.
          CONDENSE IT_ABXCLSDT-CHVL.
          IT_CUVTAB_NEW-VALC  = IT_ABXCLSDT-CHVL. "Field Value
          IT_CUVTAB_NEW-SLNID = IT_CUVTAB_NEW-SLNID + 1.
          IT_CUVTLN_NEW-SLNID = IT_CUVTLN_NEW-SLNID + 1.
          IT_CUVTLN_NEW-VTLIN = IT_CUVTLN_NEW-VTLIN + 1.
        ELSE.
          CONDENSE IT_ABXCLSDT-CHVL.
          IT_CUVTAB_NEW-MANDT = SY-MANDT.
          IT_CUVTAB_NEW-VTINT = CUVTAB-VTINT. "Table Internal No
          IT_CUVTAB_NEW-SLNID = '00001'.
          IT_CUVTAB_NEW-ATINN = WA_ATINN.
          IT_CUVTAB_NEW-VLCNT = '001'.
          IT_CUVTAB_NEW-VALC  = IT_ABXCLSDT-CHVL. "Field Value

          IT_CUVTLN_NEW-MANDT = SY-MANDT.
          IT_CUVTLN_NEW-VTINT = CUVTAB-VTINT. "Table Internal No
          IT_CUVTLN_NEW-VTLIN = '00001'.
          CLEAR IT_CUVTLN_NEW-ECM_CNT.
          IT_CUVTLN_NEW-SLNID = '00001'.
          CLEAR IT_CUVTLN_NEW-ECM_NO.
          CLEAR IT_CUVTLN_NEW-ECM_DL.
        ENDIF.
        APPEND : IT_CUVTAB_NEW,
                 IT_CUVTLN_NEW.
        CLEAR : IT_CUVTLN_OLD, IT_CUVTLN_OLD[],
                IT_CUVTAB_OLD, IT_CUVTAB_OLD[].

*---> CREATE VALUE OF PLANNIG TABLE FIELD
        CALL FUNCTION 'CUVT_UPDATE_TABLE_CONTENT'
          EXPORTING
            TABLE_HEADER       = CUVTAB
*             ECM_NUMBER         =
          TABLES
            LINES_OLD          = IT_CUVTLN_OLD[]
            LINES_NEW          = IT_CUVTLN_NEW[]
            VALUES_C_OLD       = IT_CUVTAB_OLD[]
            VALUES_C_NEW       = IT_CUVTAB_NEW[]
            VALUES_N_OLD       = IT_CUVTAB_VALN[]
            VALUES_N_NEW       = IT_CUVTAB_VALN[].

        IF SY-SUBRC EQ 0.
          WA_VALC_PD = WA_VALC_PD + 1.
        ENDIF.
      ENDIF.
    ELSE.
*----> Exist value of Planning Table
      WA_ERROR_PD = WA_ERROR_PD + 1.
      MOVE  'E'      TO  IT_ABXCLSDT-ZRESULT.
      MOVE  TEXT-303 TO IT_ABXCLSDT-ZMSG.
      MODIFY IT_ABXCLSDT  INDEX  WA_ABXCLSDT_IX.
    ENDIF.

  ENDIF.
ENDFORM.                    " CREATE_PLANNING_TABLE_VALUE
*&---------------------------------------------------------------------*
*&      Form  LIST_PROCESS
*&---------------------------------------------------------------------*
FORM LIST_PROCESS.
  DATA : L_TABIX     TYPE   SY-TABIX,
         L_INDEX     TYPE   SY-TABIX.

  DESCRIBE TABLE IT_ABXCLSDT LINES WA_TOTAL_PD.
  WRITE:/ TEXT-306 , WA_TOTAL_PD.
  WRITE:/ TEXT-307 , WA_CUVTAB_PD.
  WRITE:/ TEXT-308 , WA_VALC_PD.
  WRITE:/ TEXT-309 , WA_ERROR_PD.
  SKIP 2.
  LOOP AT IT_ABXCLSDT  WHERE  ZRESULT EQ 'E'.
    L_TABIX = L_TABIX  + 1.
    L_INDEX = L_TABIX MOD 2.
    AT FIRST.
     WRITE:/ '************** Begin of Error Detail List **************'.
    ENDAT.
    IF L_INDEX EQ 0.
      FORMAT INTENSIFIED ON.
    ELSE.
      FORMAT INTENSIFIED OFF.
    ENDIF.
    WRITE:/  L_TABIX COLOR COL_TOTAL,
            IT_ABXCLSDT-CLID   COLOR COL_KEY,     "Class No
            IT_ABXCLSDT-CLTY   COLOR COL_KEY,     "Class Type
            IT_ABXCLSDT-CHID   COLOR COL_KEY,     "Characteristic name
            IT_ABXCLSDT-CHVL   COLOR COL_NORMAL,  "Characteristic value
            IT_ABXCLSDT-EONO   COLOR COL_NORMAL,  "Change No
            IT_ABXCLSDT-FLAL   COLOR COL_NORMAL,  "Valid from date
            IT_ABXCLSDT-CDES   COLOR COL_NORMAL,  "Class description
            IT_ABXCLSDT-CHDS   COLOR COL_NORMAL,  "Characteristic value
            IT_ABXCLSDT-STAT   COLOR COL_NORMAL,  "Class status
            IT_ABXCLSDT-TVAL   COLOR COL_NORMAL,  "Valid to date
            IT_ABXCLSDT-GEFT   COLOR COL_NORMAL,  "Processing division
            IT_ABXCLSDT-ZUSER  COLOR COL_NORMAL,  "
            IT_ABXCLSDT-ZSTIM  COLOR COL_NORMAL,  "
            IT_ABXCLSDT-ZEDAT  COLOR COL_NORMAL,  "
            IT_ABXCLSDT-ZETIM  COLOR COL_NORMAL,  "
            IT_ABXCLSDT-ZBDAT  COLOR COL_NORMAL,  "
            IT_ABXCLSDT-ZBNAM  COLOR COL_NORMAL,  "
            IT_ABXCLSDT-ZMODE  COLOR COL_NORMAL,  "
            IT_ABXCLSDT-ZRESULT COLOR COL_NEGATIVE, "
            IT_ABXCLSDT-ZMSG   COLOR COL_POSITIVE.  "DETAIL OF ABXCLSDT
    AT LAST.
      FORMAT INTENSIFIED ON.
      WRITE:/ '************** End of Error Detail List **************'.
    ENDAT.
  ENDLOOP.

ENDFORM.                    " LIST_PROCESS
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
FORM INITIALIZATION.
  CLEAR : S_DATUM , S_DATUM[] .
  S_DATUM-SIGN   = 'I' .
  S_DATUM-OPTION = 'EQ' .
  S_DATUM-LOW    = SY-DATUM .
  APPEND S_DATUM .
ENDFORM.                    " INITIALIZATION
