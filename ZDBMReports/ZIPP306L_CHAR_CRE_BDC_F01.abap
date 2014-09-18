*----------------------------------------------------------------------*
*   INCLUDE ZIPP306L_CHAR_CRE_BDC_F01                                  *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
FORM INITIALIZATION.
  REFRESH: IT_BDC, IT_MESS.
  CLEAR:   IT_BDC, IT_MESS.
ENDFORM.                               " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  SCREEN_MODIFY
*&---------------------------------------------------------------------*
FORM SCREEN_MODIFY.
  LOOP AT SCREEN.
*    IF SCREEN-NAME  EQ 'P_RDO1'.
    IF P_RDO1 EQ 'X'.
      CASE SCREEN-NAME.
        WHEN 'P_FILETY' OR 'P_TCODE' OR 'P_FILE' OR
        '%B002004_BLOCK_1000' OR '%_P_FILE_%_APP_%-TEXT' OR
        '%_P_FILETY_%_APP_%-TEXT' OR '%_P_TCODE_%_APP_%-TEXT'.
          SCREEN-ACTIVE = 0.
      ENDCASE.
*    ELSEIF SCREEN-NAME  EQ 'P_RDO2'.  "EXCEL DATA
    ELSEIF P_RDO2 EQ 'X'.  "EXCEL DATA
      CASE SCREEN-NAME.
        WHEN 'P_FILETY' OR 'P_TCODE'.
          SCREEN-INPUT = 0.
      ENDCASE.
    ENDIF.
    MODIFY SCREEN.
    CLEAR SCREEN.
  ENDLOOP.
ENDFORM.                    " SCREEN_MODIFY
*&---------------------------------------------------------------------*
*&      Form  AT_SEL_SCREEN_ON_VALUE_REQUEST
*&---------------------------------------------------------------------*
FORM AT_SEL_SCREEN_ON_VALUE_REQUEST USING DEF_PATH LIKE RLGRAP-FILENAME
                                          MODE     TYPE C.

  DATA: TMP_FILENAME LIKE RLGRAP-FILENAME.
  DATA: TMP_MASK(80).                  " LIKE GLOBAL_FILEMASK_ALL.
  DATA: FIELDLN TYPE I.
  FIELD-SYMBOLS: <TMP_SYM>.

* Build Filter for Fileselektor

*  IF GLOBAL_FILEMASK_MASK IS INITIAL.
  TMP_MASK = ',*.*,*.*.'.
*  ELSE.
*    TMP_MASK = ','.
*    WRITE GLOBAL_FILEMASK_TEXT TO TMP_MASK+1.
*    WRITE ',' TO TMP_MASK+21.
*    WRITE GLOBAL_FILEMASK_MASK TO TMP_MASK+22.
*    WRITE '.' TO TMP_MASK+42.
*    CONDENSE TMP_MASK NO-GAPS.
*  ENDIF.

*  IF NOT GLOBAL_FILEMASK_ALL IS INITIAL.
*    TMP_MASK = GLOBAL_FILEMASK_ALL.
*  ENDIF.
*
  FIELDLN = STRLEN( DEF_PATH ) - 1.
  ASSIGN DEF_PATH+FIELDLN(1) TO <TMP_SYM>.
  IF <TMP_SYM> = '/' OR <TMP_SYM> = '\'.
    CLEAR <TMP_SYM>.
  ENDIF.

  CALL FUNCTION 'WS_FILENAME_GET'
       EXPORTING
            DEF_FILENAME     = P_FILE
            DEF_PATH         = DEF_PATH
*           MASK             = ',*.*,*.*.'
            MASK             = TMP_MASK
            MODE             = MODE
*           TITLE            = ' '
       IMPORTING
            FILENAME         = TMP_FILENAME
*         RC               =
       EXCEPTIONS
            INV_WINSYS       = 01
            NO_BATCH         = 02
            SELECTION_CANCEL = 03
            SELECTION_ERROR  = 04.

  IF SY-SUBRC = 0.
    P_FILE = TMP_FILENAME.
  ELSE.
* IF SY-SUBRC = 01.    "// Does not work, why ???
*   MESSAGELINE = 'Not supported'.
* ENDIF.
  ENDIF.

ENDFORM.                               " AT_SEL_SCREEN_ON_VALUE_REQUEST
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_PROCESS
*&---------------------------------------------------------------------*
FORM UPLOAD_PROCESS.

  CALL FUNCTION 'WS_UPLOAD'
       EXPORTING
            CODEPAGE                = ' '
            FILENAME                = P_FILE
            FILETYPE                = P_FILETY
*           HEADLEN                 = ' '
*           LINE_EXIT               = ' '
*           TRUNCLEN                = ' '
*           USER_FORM               = ' '
*           USER_PROG               = ' '
*      IMPORTING
*           FILELENGTH              =
       TABLES
            DATA_TAB                = IT_EXCL
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
           OTHERS                  = 10
            .
  CASE SY-SUBRC.
    WHEN 0.
      DATA L_TEXT(132).
      CONCATENATE P_FILE ' DATA UPLOAD SUCCESS!!'
                  INTO L_TEXT.
      WRITE: / L_TEXT.
      SKIP.
    WHEN 2.
      MESSAGE E000 WITH 'FILE OPEN ERROR, FILE NO FOUND!'.
    WHEN 3.
      MESSAGE E000 WITH 'FILE READ ERROR'.
    WHEN OTHERS.
      MESSAGE E000 WITH 'FILE UPLOAD ERROR, CHECK YOUR FILE!.'.
  ENDCASE.

ENDFORM.                               " UPLOAD_PROCESS
*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
FORM READ_PROCESS.
  DATA L_DATUM TYPE SY-DATUM.
  L_DATUM = SY-DATUM.
  SELECT *
       FROM ZTBM_ABXCHRDT
       INTO TABLE IT_ACHR
       WHERE ZEDAT EQ L_DATUM.
  IF SY-SUBRC NE 0.
    WRITE: / 'NO DATA'.
  ENDIF.
ENDFORM.                    " READ_PROCESS
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
FORM DATA_PROCESS.
  DATA: L_TABIX TYPE SY-TABIX.
  DATA: LT_ACHR LIKE IT_ACHR OCCURS 0 WITH HEADER LINE.
* DATA TOTAL LINES
  DESCRIBE TABLE IT_ACHR LINES WA_LINE_IDX.

  LOOP AT IT_ACHR WHERE GEFT NE 'I'.
    L_TABIX = SY-TABIX.
    LT_ACHR = IT_ACHR.
    APPEND LT_ACHR.
    DELETE IT_ACHR INDEX L_TABIX.
    CLEAR: LT_ACHR, IT_ACHR.
  ENDLOOP.
* DATA INTERFACE ERROR LINES
  DESCRIBE TABLE LT_ACHR LINES WA_ERRO_IDX.

  FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
  WRITE: / 'Characteristic Upload Total : ', WA_LINE_IDX.
  WRITE: / 'Interface Input Data Error : ', WA_ERRO_IDX.
  FORMAT COLOR OFF.
  IF NOT LT_ACHR[] IS INITIAL.
    WRITE: / 'PROCESSING DIVISION ERROR'.
    WRITE: /(30) 'CHARACTERISTIC NAME',
            (30) 'CHARACTERISTIC VALUE',
            (18) 'CHANGE NUMBER',
            (06) 'STATUS',
            (18) 'DATA TYPE',
            (06) 'NUMBER',
            (30) 'CHARCTERISTIC VALUE DES',
            (10) 'VALID-FROM',
            (10) 'PRO DIVI'.
    LOOP AT LT_ACHR.
      WRITE: /(30) LT_ACHR-CHID,
              (30) LT_ACHR-CHVL,
              (18) LT_ACHR-EONO,
              (06) LT_ACHR-STAT,
              (18) LT_ACHR-DATA,
              (06) LT_ACHR-NCHR,
              (30) LT_ACHR-CDES,
              (08) LT_ACHR-VALD,
              (10) LT_ACHR-GEFT.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " DATA_PROCESS
*&---------------------------------------------------------------------*
*&      Form  BDC_PROCESS
*&---------------------------------------------------------------------*
FORM BDC_PROCESS.
  DATA: L_NCHR(10),
        L_TABIX TYPE SY-TABIX,
        L_MSG  LIKE CFGNL-MSGLIN.
  DATA: L_ATNAM TYPE CABN-ATNAM.
  DATA: BEGIN OF LA_OPT OCCURS 0.
          INCLUDE STRUCTURE CTU_PARAMS.
  DATA: END OF LA_OPT.
  LA_OPT-DEFSIZE = 'X'.
  LA_OPT-DISMODE = 'N'.
  LA_OPT-UPDMODE = 'S'.

  LOOP AT IT_ACHR.
    L_TABIX = SY-TABIX.
*    READ TABLE LT_ACHR WITH KEY ATNAM = IT_ACHR-CHID
*                       BINARY SEARCH.
    SELECT SINGLE ATNAM
                FROM CABN
                INTO L_ATNAM
                WHERE ATNAM EQ IT_ACHR-CHID.
    IF SY-SUBRC EQ 0.
      PERFORM DYNPRO USING:
         'X' 'SAPMCTAV'    '0100',
         ' ' 'CABN-ATNAM' IT_ACHR-CHID,    "CHARACTERISTIC
         ' ' 'CCIN-AENNR' IT_ACHR-EONO,    "
         ' ' 'BDC_OKCODE'  '=WERT',

         'X' 'SAPLCTMV'    '0200',
         ' ' 'BDC_OKCODE'  '=P+',

         'X' 'SAPLCTMV'    '0200',
         ' ' 'CAWNT-ATWTB(02)' IT_ACHR-CHVL,    "CHARACTERISTIC
         ' ' 'CAWN-ATWRT(02)' IT_ACHR-CDES,    "
         ' ' 'BDC_OKCODE'  '=SICH'.
      CALL TRANSACTION 'CT02'  USING IT_BDC
                               OPTIONS FROM LA_OPT
                               MESSAGES INTO IT_MESS.
    ELSE.
      WRITE: IT_ACHR-NCHR TO L_NCHR.
      SHIFT L_NCHR LEFT  DELETING LEADING SPACE.
      PERFORM DYNPRO USING:
         'X' 'SAPMCTAV'    '0100',
         ' ' 'CABN-ATNAM' IT_ACHR-CHID,    "CHARACTERISTIC
         ' ' 'CCIN-AENNR' IT_ACHR-EONO,    "
         ' ' 'BDC_OKCODE'  '/00'.
      IF IT_ACHR-CHID EQ 'COLOREXTERNAL'.
        PERFORM DYNPRO USING:
           'X' 'SAPLCTMV'    '0110',
         ' ' 'CABNT-ATBEZ'  'VEHICLE EXTERNAL COLOR'.    "CHARACTERISTIC
      ELSE.
        PERFORM DYNPRO USING:
          'X' 'SAPLCTMV'    '0110',
         ' ' 'CABNT-ATBEZ'  'VEHICLE INTERNAL COLOR'.    "CHARACTERISTIC
      ENDIF.

      PERFORM DYNPRO USING:
       ' ' 'CABN-ATKLA'  'COLOR',    "
       ' ' 'CABN-ATMST' IT_ACHR-STAT,    "
       ' ' 'RCTMV-ATEIN' 'X',    "
       ' ' 'RCTMV-ATERF' ' ',    "
       ' ' 'RCTMV-ATERF' ' ',    "
       ' ' 'RCTMV-FORMAT' IT_ACHR-DATA,    "
       ' ' 'BDC_OKCODE'  '=FORMAT',

       'X' 'SAPLCTMV'    '0155',
       ' ' 'RCTMV-ATCST' L_NCHR,    "CHARACTERISTIC
*       ' ' 'RCTMV-ATCST' IT_ACHR-NCHR,    "CHARACTERISTIC
      ' ' 'RCTMV-ATEIN' 'X',    "
       ' ' 'BDC_OKCODE'  '=RETU',

       'X' 'SAPLCTMV'    '0110',
       ' ' 'RCTMV-ATERF' ' ',    "
       ' ' 'BDC_OKCODE'  '=SICH',

       'X' 'SAPLCTMV'    '0200',
       ' ' 'CAWNT-ATWTB(01)' IT_ACHR-CHVL,    "CHARACTERISTIC
       ' ' 'CAWN-ATWRT(01)' IT_ACHR-CDES,    "
       ' ' 'BDC_OKCODE'  '=SICH'.

      CALL TRANSACTION 'CT01'  USING IT_BDC
                               OPTIONS FROM LA_OPT
                               MESSAGES INTO IT_MESS.

    ENDIF.


    PERFORM RKC_MSG_STRING CHANGING L_MSG.

*   MODIFY IT_AMMC
    PERFORM IT_ACHR_MODIFY USING L_MSG
                                 SY-MSGTY
                                 L_TABIX.
    REFRESH: IT_BDC, IT_MESS.
  ENDLOOP.

ENDFORM.                    " BDC_PROCESS
*&---------------------------------------------------------------------*
*&      Form  BDC_PROCESS_01
*&---------------------------------------------------------------------*
FORM BDC_PROCESS_01.
  DATA: L_TABIX TYPE SY-TABIX,
        L_ATNAM TYPE CABN-ATNAM.
  DATA: BEGIN OF LA_OPT OCCURS 0.
          INCLUDE STRUCTURE CTU_PARAMS.
  DATA: END OF LA_OPT.
  LA_OPT-DEFSIZE = 'X'.
  LA_OPT-DISMODE = 'N'.
  LA_OPT-UPDMODE = 'S'.
* DATA TOTAL LINES
  DESCRIBE TABLE IT_EXCL LINES WA_LINE_IDX.

  LOOP AT IT_EXCL.
    L_TABIX = SY-TABIX.
    SELECT SINGLE ATNAM
                FROM CABN
                INTO L_ATNAM
                WHERE ATNAM EQ IT_EXCL-CHID.
    IF SY-SUBRC EQ 0.
      PERFORM DYNPRO USING:
         'X' 'SAPMCTAV'    '0100',
         ' ' 'CABN-ATNAM' IT_EXCL-CHID,    "CHARACTERISTIC
         ' ' 'CCIN-AENNR' IT_EXCL-EONO,    "
         ' ' 'BDC_OKCODE'  '=WERT',

         'X' 'SAPLCTMV'    '0200',
         ' ' 'BDC_OKCODE'  '=P+',

         'X' 'SAPLCTMV'    '0200',
         ' ' 'CAWNT-ATWTB(02)' IT_EXCL-CHVL,    "CHARACTERISTIC
         ' ' 'CAWN-ATWRT(02)' IT_EXCL-CDES,    "
         ' ' 'BDC_OKCODE'  '=SICH'.
      CALL TRANSACTION 'CT02'  USING IT_BDC
                               OPTIONS FROM LA_OPT
                               MESSAGES INTO IT_MESS.
    ELSE.
      PERFORM DYNPRO USING:
         'X' 'SAPMCTAV'    '0100',
         ' ' 'CABN-ATNAM' IT_EXCL-CHID,    "CHARACTERISTIC
         ' ' 'CCIN-AENNR' IT_EXCL-EONO,    "
         ' ' 'BDC_OKCODE'  '/00'.
      IF IT_EXCL-CHID EQ 'COLOREXTERNAL'.
        PERFORM DYNPRO USING:
           'X' 'SAPLCTMV'    '0110',
         ' ' 'CABNT-ATBEZ'  'VEHICLE EXTERNAL COLOR'.    "
      ELSE.
        PERFORM DYNPRO USING:
          'X' 'SAPLCTMV'    '0110',
         ' ' 'CABNT-ATBEZ'  'VEHICLE INTERNAL COLOR'.    "
      ENDIF.

      PERFORM DYNPRO USING:
       ' ' 'CABN-ATKLA'  'COLOR',    "
       ' ' 'CABN-ATMST' IT_EXCL-STAT,    "
       ' ' 'RCTMV-ATEIN' 'X',    "
       ' ' 'RCTMV-ATERF' ' ',    "
       ' ' 'RCTMV-ATERF' ' ',    "
       ' ' 'RCTMV-FORMAT' IT_EXCL-DATA,    "
       ' ' 'BDC_OKCODE'  '=FORMAT',

       'X' 'SAPLCTMV'    '0155',
*       ' ' 'RCTMV-ATCST' L_NCHR,    "CHARACTERISTIC
       ' ' 'RCTMV-ATCST' IT_EXCL-NCHR,    "
      ' ' 'RCTMV-ATEIN' 'X',    "
       ' ' 'BDC_OKCODE'  '=RETU',

       'X' 'SAPLCTMV'    '0110',
       ' ' 'RCTMV-ATERF' ' ',    "
       ' ' 'BDC_OKCODE'  '=SICH',

       'X' 'SAPLCTMV'    '0200',
       ' ' 'CAWNT-ATWTB(01)' IT_EXCL-CHVL,    "
       ' ' 'CAWN-ATWRT(01)' IT_EXCL-CDES,    "
       ' ' 'BDC_OKCODE'  '=SICH'.

      CALL TRANSACTION 'CT01'  USING IT_BDC
                               OPTIONS FROM LA_OPT
                               MESSAGES INTO IT_MESS.

    ENDIF.
    IT_EXCL-MSGTY = SY-MSGTY.
    PERFORM RKC_MSG_STRING CHANGING IT_EXCL-MESSG.
*   MODIFY IT_EXCL
    MODIFY IT_EXCL INDEX L_TABIX TRANSPORTING MSGTY
                                              MESSG.
    REFRESH: IT_BDC, IT_MESS.
  ENDLOOP.

ENDFORM.                    " BDC_PROCESS_01
*---------------------------------------------------------------------*
*       FORM DYNPRO                                                   *
*---------------------------------------------------------------------*
FORM DYNPRO USING DYNBEGIN NAME VALUE.
  IF DYNBEGIN = 'X'.
    CLEAR IT_BDC.
    MOVE: NAME TO IT_BDC-PROGRAM,
          VALUE TO IT_BDC-DYNPRO,
          DYNBEGIN TO IT_BDC-DYNBEGIN.
    APPEND IT_BDC.
  ELSE.
    CLEAR IT_BDC.
    MOVE: NAME TO IT_BDC-FNAM,
          VALUE TO IT_BDC-FVAL.
    APPEND IT_BDC.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  UPDATE_CABN
*&---------------------------------------------------------------------*
FORM UPDATE_CABN.
  DATA LT_CABN TYPE CABN OCCURS 0 WITH HEADER LINE.
  DATA L_TABIX TYPE SY-TABIX.
  IF NOT IT_ACHR[] IS INITIAL.
    SELECT *
         FROM CABN
         INTO TABLE LT_CABN
         FOR ALL ENTRIES IN IT_ACHR
         WHERE ATNAM EQ IT_ACHR-CHID.
  ENDIF.
  LOOP AT LT_CABN.
    L_TABIX = SY-TABIX.
    LT_CABN-ATWRD = ' '.
    MODIFY LT_CABN INDEX L_TABIX TRANSPORTING ATWRD.
    CLEAR LT_CABN.
  ENDLOOP.

  UPDATE CABN FROM TABLE LT_CABN.
  IF SY-SUBRC EQ 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
ENDFORM.                    " UPDATE_CABN
*&---------------------------------------------------------------------*
*&      Form  IT_ACHR_MODIFY
*&---------------------------------------------------------------------*
FORM IT_ACHR_MODIFY USING P_MSG
                          P_MSGTY
                          P_TABIX.

  IT_ACHR-ZMSG    = P_MSG.
  IT_ACHR-ZRESULT = P_MSGTY.
  CASE P_MSGTY.
    WHEN 'S'.
      IT_ACHR-ZBDAT = SY-DATUM.  "SAP BDC EXECUTED DATE
      IT_ACHR-ZBTIM = SY-UZEIT.  "SAP BDC EXECUTED TIME
      IT_ACHR-ZBNAM = SY-UNAME.  "BDC User ID
*     C: Create U: Update D: Delete
      IT_ACHR-ZMODE = 'C'.       "C:CREATE
    WHEN 'E'.
      IT_ACHR-ZBDAT = SY-DATUM.  "SAP BDC EXECUTED DATE
*      IT_ACHR-ZBTIM = SY-UZEIT.  "SAP BDC EXECUTED TIME
      IT_ACHR-ZBNAM = SY-UNAME.  "BDC User ID
      IT_ACHR-ZMODE = 'C'.       "C:CREATE
    WHEN OTHERS.
      IT_ACHR-ZBDAT = SY-DATUM.  "SAP BDC EXECUTED DATE
      IT_ACHR-ZBTIM = SY-UZEIT.  "SAP BDC EXECUTED TIME
      IT_ACHR-ZBNAM = SY-UNAME.  "BDC User ID
      IT_ACHR-ZMODE = 'C'.       "C:CREATE
  ENDCASE.
* IT_ACHR MODIFY
  MODIFY IT_ACHR INDEX P_TABIX TRANSPORTING ZBDAT
                                            ZBTIM
                                            ZBNAM
                                            ZMODE
                                            ZRESULT
                                            ZMSG.
ENDFORM.                    " IT_ACHR_MODIFY
*&---------------------------------------------------------------------*
*&      Form  UPDATE_PROCESS
*&---------------------------------------------------------------------*
FORM UPDATE_PROCESS.
* ZTBM_ABXCHRDT TABLE UPDATE
  PERFORM UPDATE_ZTBM_ABXCHRDT.
* CABN TABLE UPDATE
  PERFORM UPDATE_CABN.
ENDFORM.                    " UPDATE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  UPDATE_ZTBM_ABXCHRDT
*&---------------------------------------------------------------------*
FORM UPDATE_ZTBM_ABXCHRDT.
  UPDATE ZTBM_ABXCHRDT FROM TABLE IT_ACHR.
  IF SY-SUBRC EQ 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
ENDFORM.                    " UPDATE_ZTBM_ABXCHRDT
*&---------------------------------------------------------------------*
*&      Form  RKC_MSG_STRING
*&---------------------------------------------------------------------*
FORM RKC_MSG_STRING CHANGING P_MSG.
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
            MSG_LIN = P_MSG
       EXCEPTIONS
            OTHERS  = 1.
ENDFORM.                    " RKC_MSG_STRING
*&---------------------------------------------------------------------*
*&      Form  WRITE_PROCESS
*&---------------------------------------------------------------------*
FORM WRITE_PROCESS.
  DATA L_INDEX TYPE SY-INDEX.
  DESCRIBE TABLE IT_ACHR LINES WA_LINE_IDX.
  LOOP AT IT_ACHR WHERE ZRESULT EQ 'E'.
    WA_ERRO_IDX = WA_ERRO_IDX + 1.
  ENDLOOP.

  FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
  WRITE: / 'Characteristic CREATE SUCCESS :  ', WA_LINE_IDX.
  WRITE: / 'Characteristic ERROR :  ', WA_ERRO_IDX.
  FORMAT COLOR OFF.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE: / 'Characteristic Create ERROR'.
  FORMAT COLOR OFF.
  WRITE: /(30) 'Characteristic',
          (30) 'Characteristic Value',
          (30) 'Char Value description',
          (18) 'Data Type',
          (12) 'ECM NUMBER',
          (99) 'ERROR MESSAGE'.
  LOOP AT IT_ACHR WHERE ZRESULT EQ 'E'.
    WRITE: /(30) IT_ACHR-CHID,
            (30) IT_ACHR-CHVL,
            (30) IT_ACHR-CDES,
            (18) IT_ACHR-DATA,
            (12) IT_ACHR-EONO,
            (99) IT_ACHR-ZMSG.
  ENDLOOP.
ENDFORM.                    " WRITE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  WRITE_PROCESS_01
*&---------------------------------------------------------------------*
FORM WRITE_PROCESS_01.
  DATA L_INDEX TYPE SY-INDEX.
  DESCRIBE TABLE IT_EXCL LINES WA_LINE_IDX.
  LOOP AT IT_EXCL WHERE MSGTY EQ 'E'.
    WA_ERRO_IDX = WA_ERRO_IDX + 1.
  ENDLOOP.

  FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
  WRITE: / 'Characteristic CREATE SUCCESS :  ', WA_LINE_IDX.
  WRITE: / 'Characteristic ERROR :  ', WA_ERRO_IDX.
  FORMAT COLOR OFF.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE: / 'Characteristic Create ERROR'.
  FORMAT COLOR OFF.
  WRITE: /(30) 'Characteristic',
          (30) 'Characteristic Value',
          (30) 'Char Value description',
          (18) 'Data Type',
          (12) 'ECM NUMBER',
          (99) 'ERROR MESSAGE'.
  LOOP AT IT_EXCL WHERE MSGTY EQ 'E'.
    WRITE: /(30) IT_EXCL-CHID,
            (30) IT_EXCL-CHVL,
            (30) IT_EXCL-CDES,
            (18) IT_EXCL-DATA,
            (12) IT_EXCL-EONO,
            (99) IT_EXCL-MESSG.
  ENDLOOP.
ENDFORM.                    " WRITE_PROCESS_01
