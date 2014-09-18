*----------------------------------------------------------------------*
*   INCLUDE ZIPP307L_CLASS_BDC_F01                                  *
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
        WHEN 'P_FILETY' OR '%_P_FILE_%_APP_%-TEXT'
          OR 'P_FILE'   OR '%_P_FILETY_%_APP_%-TEXT'.
          SCREEN-ACTIVE = 0.
        WHEN 'P_TCODE'.
          SCREEN-INPUT = 0.
      ENDCASE.
*    ELSEIF SCREEN-NAME  EQ 'P_RDO2'.  "EXCEL DATA
    ELSEIF P_RDO2 EQ 'X'.  "EXCEL DATA
      CASE SCREEN-NAME.
        WHEN 'P_ZEDAT' OR '%_P_ZEDAT_%_APP_%-TEXT'
          OR 'P_ZBTIM' OR '%_P_ZBTIM_%_APP_%-TEXT'.
          SCREEN-ACTIVE = 0.
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
      CONCATENATE P_FILE TEXT-003
                  INTO L_TEXT.
      WRITE: / L_TEXT.
      SKIP.
    WHEN 2.
      MESSAGE E000 WITH TEXT-004.
    WHEN 3.
      MESSAGE E000 WITH TEXT-005.
    WHEN OTHERS.
      MESSAGE E000 WITH TEXT-006.
  ENDCASE.

ENDFORM.                               " UPLOAD_PROCESS
*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
FORM READ_PROCESS.
  SELECT *
       FROM ZTBM_ABXCLSDT
       INTO TABLE IT_ACLS
       WHERE ZEDAT EQ P_ZEDAT
       AND   ZBTIM EQ P_ZBTIM.
  IF SY-SUBRC NE 0.
    WRITE: / TEXT-007.
  ENDIF.
ENDFORM.                    " READ_PROCESS
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
FORM DATA_PROCESS.
  DATA: L_TABIX TYPE SY-TABIX.
  DATA: LT_ACLS LIKE IT_ACLS OCCURS 0 WITH HEADER LINE.
* DATA TOTAL LINES
  DESCRIBE TABLE IT_ACLS LINES WA_LINE_IDX.

  LOOP AT IT_ACLS WHERE GEFT NE 'I'.
    L_TABIX = SY-TABIX.
    LT_ACLS = IT_ACLS.
    LT_ACLS-ZMODE = 'C'.
    LT_ACLS-ZRESULT = 'L'.
    LT_ACLS-ZMSG = 'INTERFACE INPUT DATA ERROR'.
    APPEND LT_ACLS.
    DELETE IT_ACLS INDEX L_TABIX.
    CLEAR: LT_ACLS, IT_ACLS.
  ENDLOOP.
* DATA INTERFACE ERROR LINES
  DESCRIBE TABLE LT_ACLS LINES WA_ERRO_IDX.

  FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
  WRITE: / TEXT-008, WA_LINE_IDX.
  WRITE: / TEXT-009, WA_ERRO_IDX.
  FORMAT COLOR OFF.
  IF NOT LT_ACLS[] IS INITIAL.
    WRITE: / TEXT-010.
    PERFORM WRITE_ERROR_ACLS TABLES LT_ACLS
                             USING  'L'.
  ENDIF.
ENDFORM.                    " DATA_PROCESS
*&---------------------------------------------------------------------*
*&      Form  BDC_PROCESS
*&---------------------------------------------------------------------*
FORM BDC_PROCESS.
  DATA: L_NCHR(10),
        L_TABIX TYPE SY-TABIX,
        L_MSG  LIKE CFGNL-MSGLIN,
        L_MODE.
  DATA: L_POSNR TYPE KSML-POSNR,
        L_PAGAC TYPE RCTMV-PAGAC,
        L_FLAL(10),
        L_TVAL(10).
* BDC OPTIONS
  WA_OPT-DEFSIZE = 'X'.
  WA_OPT-DISMODE = 'N'.
  WA_OPT-UPDMODE = 'S'.

  LOOP AT IT_ACLS.
    L_TABIX = SY-TABIX.
    WRITE: IT_ACLS-FLAL TO L_FLAL,
           IT_ACLS-TVAL TO L_TVAL.
    CLEAR : L_POSNR, L_PAGAC.
    PERFORM READ_KLAH_CABN_KSML USING    IT_ACLS-CLID
                                         IT_ACLS-CLTY
                                         IT_ACLS-CHID
                                CHANGING L_POSNR
                                         L_PAGAC
                                         L_MODE.
    CASE L_MODE.
      WHEN '1'.
*       CLASS CREATE
        PERFORM CLASS_CREATED USING IT_ACLS-CLID
                                    IT_ACLS-CLTY
                                    IT_ACLS-EONO
                                    IT_ACLS-CDES
                                    IT_ACLS-STAT
                                    L_FLAL
                                    L_TVAL
                                    IT_ACLS-CHID
                                    IT_ACLS-CHVL
                                    IT_ACLS-CHDS.
      WHEN '2'.
*       CLASS CHANGE(CHARACTERISTIC CREATE)
        PERFORM CLASS_CHANGING_CHARA_ID USING IT_ACLS-CLID
                                              IT_ACLS-CLTY
                                              IT_ACLS-EONO
                                              IT_ACLS-CHID
                                              IT_ACLS-CHDS
                                              IT_ACLS-CHVL.


      WHEN '3'.
*       CLASS CHANGE(CHARACTERISTIC VALUE CREATE)
        PERFORM CLASS_CHANGING_CHARA_VALUE USING IT_ACLS-CLID
                                                 IT_ACLS-CLTY
                                                 IT_ACLS-EONO
                                                 IT_ACLS-CHVL
                                                 IT_ACLS-CHDS
                                                 L_POSNR
                                                 L_PAGAC.
    ENDCASE.

    IT_ACLS-ZRESULT = SY-MSGTY.

    PERFORM RKC_MSG_STRING CHANGING L_MSG.

    IT_ACLS-ZMSG    = L_MSG.
*   IT_ACLS MODIFY
    MODIFY IT_ACLS INDEX L_TABIX TRANSPORTING ZRESULT
                                              ZMSG.
    REFRESH: IT_BDC, IT_MESS.
  ENDLOOP.

ENDFORM.                    " BDC_PROCESS
*&---------------------------------------------------------------------*
*&      Form  BDC_PROCESS_01
*&---------------------------------------------------------------------*
FORM BDC_PROCESS_01.
  DATA: L_TABIX TYPE SY-TABIX,
        L_CLASS TYPE KLAH-CLASS,
       L_MODE.
  DATA: L_POSNR TYPE KSML-POSNR,
        L_PAGAC TYPE RCTMV-PAGAC.
* DATA TOTAL LINES
  DESCRIBE TABLE IT_EXCL LINES WA_LINE_IDX.

  LOOP AT IT_EXCL.
    L_TABIX = SY-TABIX.
    PERFORM READ_KLAH_CABN_KSML USING    IT_EXCL-CLID
                                         IT_EXCL-CLTY
                                         IT_EXCL-CHID
                                CHANGING L_POSNR
                                         L_PAGAC
                                         L_MODE.
    CASE L_MODE.
      WHEN '1'.
*       CLASS CREATE
        PERFORM CLASS_CREATED USING IT_EXCL-CLID
                                    IT_EXCL-CLTY
                                    IT_EXCL-EONO
                                    IT_EXCL-CDES
                                    IT_EXCL-STAT
                                    IT_EXCL-FLAL
                                    IT_EXCL-TVAL
                                    IT_EXCL-CHID
                                    IT_EXCL-CHVL
                                    IT_EXCL-CHDS.
      WHEN '2'.
*       CLASS CHANGE(CHARACTERISTIC CREATE)
        PERFORM CLASS_CHANGING_CHARA_ID USING IT_EXCL-CLID
                                              IT_EXCL-CLTY
                                              IT_EXCL-EONO
                                              IT_EXCL-CHID
                                              IT_EXCL-CHDS
                                              IT_EXCL-CHVL.


      WHEN '3'.
*       CLASS CHANGE(CHARACTERISTIC VALUE CREATE)
        PERFORM CLASS_CHANGING_CHARA_VALUE USING IT_EXCL-CLID
                                                 IT_EXCL-CLTY
                                                 IT_EXCL-EONO
                                                 IT_EXCL-CHVL
                                                 IT_EXCL-CHDS
                                                 L_POSNR
                                                 L_PAGAC.
    ENDCASE.
    IT_EXCL-ZRESULT = SY-MSGTY.
    PERFORM RKC_MSG_STRING CHANGING IT_EXCL-ZMSG.
*   MODIFY IT_EXCL
    MODIFY IT_EXCL INDEX L_TABIX TRANSPORTING ZRESULT
                                              ZMSG.
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
*&      Form  UPDATE_PROCESS
*&---------------------------------------------------------------------*
FORM UPDATE_PROCESS.
* ZTBM_ABXCLSDT TABLE UPDATE
  PERFORM UPDATE_ZTBM_ABXCLSDT.
ENDFORM.                    " UPDATE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  UPDATE_ZTBM_ABXCLSDT
*&---------------------------------------------------------------------*
FORM UPDATE_ZTBM_ABXCLSDT.
  UPDATE ZTBM_ABXCLSDT FROM TABLE IT_ACLS.
  IF SY-SUBRC EQ 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
ENDFORM.                    " UPDATE_ZTBM_ABXCLSDT
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
  DESCRIBE TABLE IT_ACLS LINES WA_LINE_IDX.
  LOOP AT IT_ACLS WHERE ZRESULT EQ 'E'.
    WA_ERRO_IDX = WA_ERRO_IDX + 1.
  ENDLOOP.
  WA_LINE_IDX = WA_LINE_IDX - WA_ERRO_IDX.
  FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
  WRITE: / TEXT-022, WA_LINE_IDX.
  WRITE: / TEXT-023, WA_ERRO_IDX.
  FORMAT COLOR OFF.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE: / TEXT-024.
  FORMAT COLOR OFF.
  PERFORM WRITE_ERROR_ACLS TABLES IT_ACLS
                           USING  'E'.
ENDFORM.                    " WRITE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  WRITE_PROCESS_01
*&---------------------------------------------------------------------*
FORM WRITE_PROCESS_01.
  DATA L_INDEX TYPE SY-INDEX.
  DESCRIBE TABLE IT_EXCL LINES WA_LINE_IDX.
  LOOP AT IT_EXCL WHERE ZRESULT EQ 'E'.
    WA_ERRO_IDX = WA_ERRO_IDX + 1.
    MOVE-CORRESPONDING IT_EXCL TO IT_ACLS.
    APPEND IT_ACLS.
    CLEAR: IT_EXCL, IT_ACLS.
  ENDLOOP.

  FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
  WRITE: / TEXT-022, WA_LINE_IDX.
  WRITE: / TEXT-023, WA_ERRO_IDX.
  FORMAT COLOR OFF.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE: / TEXT-024.
  FORMAT COLOR OFF.
  PERFORM WRITE_ERROR_ACLS TABLES IT_ACLS
                           USING  'E'.
ENDFORM.                    " WRITE_PROCESS_01
*&---------------------------------------------------------------------*
*&      Form  WRITE_ERROR_ACLS
*&---------------------------------------------------------------------*
FORM WRITE_ERROR_ACLS TABLES   PT_ACLS STRUCTURE IT_ACLS
                      USING    L_ZRESULT.
  WRITE: / TEXT-010.
  WRITE: /(18)  TEXT-011,
          (04)  TEXT-012,
          (30)  TEXT-013,
          (30)  TEXT-014,
          (18)  TEXT-015,
          (10)  TEXT-016,
          (40)  TEXT-017,
          (30)  TEXT-018,
          (06)  TEXT-019,
          (10)  TEXT-020,
          (10)  TEXT-021.
  LOOP AT PT_ACLS WHERE ZRESULT EQ L_ZRESULT.
    WRITE: /(18) PT_ACLS-CLID,
            (04) PT_ACLS-CLTY,
            (30) PT_ACLS-CHID,
            (30) PT_ACLS-CHVL,
            (18) PT_ACLS-EONO,
            (10) PT_ACLS-FLAL,
            (40) PT_ACLS-CDES,
            (30) PT_ACLS-CHDS,
            (06) PT_ACLS-STAT,
            (10) PT_ACLS-TVAL,
            (10) PT_ACLS-GEFT,
            (100) PT_ACLS-ZMSG.
  ENDLOOP.
ENDFORM.                    " WRITE_ERROR_ACLS
*&---------------------------------------------------------------------*
*&      Form  CLASS_CHANGING_CHARA_ID
*&---------------------------------------------------------------------*
FORM CLASS_CHANGING_CHARA_ID USING P_CLID
                                   P_CLTY
                                   P_EONO
                                   P_CHID
                                   P_CHDS
                                   P_CHVL.
  PERFORM DYNPRO USING:
     'X' 'SAPLCLMO'    '0100',
     ' ' 'RMCLM-CLASS' P_CLID,    "CLASS
     ' ' 'RMCLM-KLART' P_CLTY,    "
     ' ' 'RMCLM-AENNR' P_EONO,    "
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCLMO'    '0100',
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCLMO'    '7777',
     ' ' 'BDC_OKCODE'  '=MERK',

     'X' 'SAPLCLMO'    '7777',
     ' ' 'RMCLM-EINTRAG' '100',           "
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCLMO'    '7777',
     ' ' 'RMCLM-MARK(02)'   'X',             "
     ' ' 'RMCLM-MERKMA(02)' P_CHID,    "
     ' ' 'BDC_CURSOR'    'RMCLM-MERKMA(02)',
     ' ' 'BDC_OKCODE'  '=MWRT',

     'X' 'SAPLCTMV'  '0200',    "
     ' ' 'BDC_OKCODE'  '=P+',

     'X' 'SAPLCTMV'  '0200',    "
     ' ' 'CAWN-ATWRT(02)'   P_CHVL,  "
     ' ' 'CAWNT-ATWTB(02)'    P_CHDS, "
     ' ' 'BDC_OKCODE'  '=SICH'.

  CALL TRANSACTION 'CL02'  USING IT_BDC
                           OPTIONS FROM WA_OPT
                           MESSAGES INTO IT_MESS.
ENDFORM.                    " CLASS_CHANGING_CHARA_ID
*&---------------------------------------------------------------------*
*&      Form  CLASS_CREATED
*&---------------------------------------------------------------------*
FORM CLASS_CREATED USING P_CLID
                         P_CLTY
                         P_EONO
                         P_CDES
                         P_STAT
                         P_FLAL
                         P_TVAL
                         P_CHID
                         P_CHVL
                         P_CHDS.
  PERFORM DYNPRO USING:
     'X' 'SAPLCLMO'    '0100',
     ' ' 'RMCLM-CLASS' P_CLID,    "CLASS
     ' ' 'RMCLM-KLART' P_CLTY,    "
     ' ' 'RMCLM-AENNR' P_EONO,    "
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCLMO'    '0100',
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCLMO'  '7777',    "
     ' ' 'RMCLM-KLBEZ' P_CDES,    "CLASS DESCRIPTION
     ' ' 'RMCLM-STATU' P_STAT,    "STATUS
     ' ' 'RMCLM-KLAGR' 'COLOR',
     ' ' 'RMCLM-VONDT' P_FLAL,
     ' ' 'RMCLM-BISDT' P_TVAL,    "
     ' ' 'BDC_OKCODE'  '=MERK',

     'X' 'SAPLCLMO'  '7777',    "
     ' ' 'RMCLM-MERKMA(01)' P_CHID,    "
     ' ' 'RMCLM-MARK(01)'   'X',           "
     ' ' 'BDC_OKCODE'  '=MWRT',

     'X' 'SAPLCTMV'  '0200',    "
     ' ' 'BDC_OKCODE'  '=P+',

     'X' 'SAPLCTMV'  '0200',    "CAWN-ATWRT
     ' ' 'CAWN-ATWRT(02)'   P_CHVL,  "
     ' ' 'CAWNT-ATWTB(02)'    P_CHDS, "
     ' ' 'BDC_OKCODE'  '=SICH'.

  CALL TRANSACTION 'CL01'  USING IT_BDC
                           OPTIONS FROM WA_OPT
                           MESSAGES INTO IT_MESS.

ENDFORM.                    " CLASS_CREATED
*&---------------------------------------------------------------------*
*&      Form  READ_KLAH_CABN_KSML
*&---------------------------------------------------------------------*
FORM READ_KLAH_CABN_KSML USING    P_CLASS
                                  P_KLART
                                  P_ATNAM
                         CHANGING P_POSNR
                                  P_PAGAC
                                  P_MODE.
  DATA: L_CLINT TYPE KLAH-CLINT,
        L_ATINN TYPE CABN-ATINN,
        L_OMERK TYPE KSML-OMERK.
  DATA: LT_CAWN TYPE CAWN OCCURS 0 WITH HEADER LINE.

* CLASS Creation existence and nonexistence .
  SELECT SINGLE CLINT
              FROM KLAH
              INTO L_CLINT
              WHERE CLASS EQ P_CLASS
              AND   KLART EQ P_KLART.
  IF SY-SUBRC EQ 0.
*   CHARACTERISTIC Creation existence and nonexistence .
    SELECT SINGLE ATINN
                FROM CABN
                INTO L_ATINN
                WHERE ATNAM EQ P_ATNAM.
    IF SY-SUBRC EQ 0.
      SELECT SINGLE POSNR
                    OMERK
                  FROM KSML
                  INTO (P_POSNR, L_OMERK )
                  WHERE CLINT EQ L_CLINT
                  AND   IMERK EQ L_ATINN.
      IF SY-SUBRC EQ 0.
        P_MODE = '3'.
        SELECT *
             FROM CAWN
             INTO TABLE LT_CAWN
             WHERE ATINN EQ L_OMERK.
        DESCRIBE TABLE LT_CAWN LINES P_PAGAC.
      ELSE.
*       CHARACTERISTIC CREATE
        P_MODE = '2'.
        P_POSNR = '100'.
      ENDIF.
    ELSE.
*     CHARACTERISTIC CREATE
      P_MODE = '2'.
      P_POSNR = '100'.
    ENDIF.

  ELSE.
*   CLASS CREATE
    P_MODE = '1'.
  ENDIF.
ENDFORM.                    " READ_KLAH_CABN_KSML
*&---------------------------------------------------------------------*
*&      Form  CLASS_CHANGING_CHARA_VALUE
*&---------------------------------------------------------------------*
FORM CLASS_CHANGING_CHARA_VALUE USING P_CLID
                                      P_CLTY
                                      P_EONO
                                      P_CHVL
                                      P_CHDS
                                      P_POSNR
                                      P_PAGAC.
  PERFORM DYNPRO USING:
     'X' 'SAPLCLMO'    '0100',
     ' ' 'RMCLM-CLASS' P_CLID,    "CLASS
     ' ' 'RMCLM-KLART' P_CLTY,    "
     ' ' 'RMCLM-AENNR' P_EONO,    "
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCLMO'    '0100',
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCLMO'    '7777',
     ' ' 'BDC_OKCODE'  '=MERK',

     'X' 'SAPLCLMO'    '7777',
     ' ' 'RMCLM-EINTRAG' P_POSNR,         "
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCLMO'    '7777',
     ' ' 'RMCLM-MARK(01)'   'X',             "
*     ' ' 'RMCLM-MERKMA(02)' P_CHID,    "
     ' ' 'BDC_OKCODE'  '=MWRT'.
  IF P_PAGAC GT '0000'.
    PERFORM DYNPRO USING:
       'X' 'SAPLCTMV'  '0200',    "
       ' ' 'RCTMV-PAGAC'   P_PAGAC,             "
       ' ' 'BDC_OKCODE'  '/00'.
  ENDIF.
  PERFORM DYNPRO USING:
     'X' 'SAPLCTMV'  '0200',    "
     ' ' 'CAWN-ATWRT(02)'   P_CHVL,  "
     ' ' 'CAWNT-ATWTB(02)'    P_CHDS, "
     ' ' 'BDC_OKCODE'  '=SICH'.

  CALL TRANSACTION 'CL02'  USING IT_BDC
                           OPTIONS FROM WA_OPT
                           MESSAGES INTO IT_MESS.
ENDFORM.                    " CLASS_CHANGING_CHARA_VALUE
*&---------------------------------------------------------------------*
*&      Form  ERROR_CHECK
*&---------------------------------------------------------------------*
FORM ERROR_CHECK.
  DATA: L_CLINT TYPE KLAH-CLINT,
        L_ATINN TYPE CABN-ATINN.
  DATA: L_TABIX TYPE SY-TABIX.
  LOOP AT IT_ACLS.
    L_TABIX = SY-TABIX.
    IF IT_ACLS-ZRESULT NE 'E'.
*     CLASS Creation existence and nonexistence .
      SELECT SINGLE CLINT
                  FROM KLAH
                  INTO L_CLINT
                  WHERE CLASS EQ IT_ACLS-CLID
                  AND   KLART EQ IT_ACLS-CLTY.
      IF SY-SUBRC EQ 0.
*       CHARACTERISTIC Creation existence and nonexistence .
        SELECT SINGLE ATINN
                    FROM CABN
                    INTO L_ATINN
                    WHERE ATNAM EQ IT_ACLS-CHID.
        IF SY-SUBRC EQ 0.
          SELECT SINGLE *
                      FROM KSML
                      WHERE CLINT EQ L_CLINT
                      AND   IMERK EQ L_ATINN.
          IF SY-SUBRC EQ 0.
            IT_ACLS-ZRESULT = 'S'.
            IT_ACLS-ZBDAT = SY-DATUM.  "SAP BDC EXECUTED DATE
            IT_ACLS-ZBTIM = SY-UZEIT.  "SAP BDC EXECUTED TIME
            IT_ACLS-ZBNAM = SY-UNAME.  "BDC User ID
*           C: Create U: Update D: Delete
            IT_ACLS-ZMODE = 'C'.       "C:CREATE
          ELSE.
            IT_ACLS-ZRESULT = 'E'.
            IT_ACLS-ZBDAT = SY-DATUM.  "SAP BDC EXECUTED DATE
            IT_ACLS-ZBNAM = SY-UNAME.  "BDC User ID
*           C: CREATE U: UPDATE D: DELETE
            IT_ACLS-ZMODE = 'C'.       "C:CREATE
          ENDIF.
        ELSE.
          IT_ACLS-ZRESULT = 'E'.
          IT_ACLS-ZBDAT = SY-DATUM.  "SAP BDC EXECUTED DATE
          IT_ACLS-ZBNAM = SY-UNAME.  "BDC User ID
*         C: Create U: Update D: Delete
          IT_ACLS-ZMODE = 'C'.       "C:CREATE
        ENDIF.

      ELSE.
        IT_ACLS-ZRESULT = 'E'.
        IT_ACLS-ZBDAT = SY-DATUM.  "SAP BDC EXECUTED DATE
        IT_ACLS-ZBNAM = SY-UNAME.  "BDC User ID
*       C: Create U: Update D: Delete
        IT_ACLS-ZMODE = 'C'.       "C:CREATE
      ENDIF.
    ELSE.
      IT_ACLS-ZRESULT = 'E'.
      IT_ACLS-ZBDAT = SY-DATUM.  "SAP BDC EXECUTED DATE
      IT_ACLS-ZBNAM = SY-UNAME.  "BDC User ID
*     C: Create U: Update D: Delete
      IT_ACLS-ZMODE = 'C'.       "C:CREATE
    ENDIF.
    MODIFY IT_ACLS INDEX L_TABIX TRANSPORTING ZRESULT
                                              ZBDAT
                                              ZBTIM
                                              ZBNAM
                                              ZMODE.
    CLEAR: IT_ACLS, L_CLINT, L_ATINN.
  ENDLOOP.
ENDFORM.                    " ERROR_CHECK
*&---------------------------------------------------------------------*
*&      Form  ERROR_CHECK1
*&---------------------------------------------------------------------*
FORM ERROR_CHECK1.
  DATA: L_CLINT TYPE KLAH-CLINT,
        L_ATINN TYPE CABN-ATINN.
  DATA: L_TABIX TYPE SY-TABIX.
  LOOP AT IT_EXCL.
    L_TABIX = SY-TABIX.
    IF IT_EXCL-ZRESULT NE 'E'.
*     CLASS Creation existence and nonexistence .
      SELECT SINGLE CLINT
                  FROM KLAH
                  INTO L_CLINT
                  WHERE CLASS EQ IT_EXCL-CLID
                  AND   KLART EQ IT_EXCL-CLTY.
      IF SY-SUBRC EQ 0.
*       CHARACTERISTIC Creation existence and nonexistence .
        SELECT SINGLE ATINN
                    FROM CABN
                    INTO L_ATINN
                    WHERE ATNAM EQ IT_EXCL-CHID.
        IF SY-SUBRC EQ 0.
          SELECT SINGLE *
                      FROM KSML
                      WHERE CLINT EQ L_CLINT
                      AND   IMERK EQ L_ATINN.
          IF SY-SUBRC EQ 0.
            IT_EXCL-ZRESULT = 'S'.
          ELSE.
            IT_EXCL-ZRESULT = 'E'.
          ENDIF.
        ELSE.
          IT_EXCL-ZRESULT = 'E'.
        ENDIF.

      ELSE.
        IT_EXCL-ZRESULT = 'E'.
      ENDIF.
    ELSE.
      IT_EXCL-ZRESULT = 'E'.
    ENDIF.
    MODIFY IT_EXCL INDEX L_TABIX TRANSPORTING ZRESULT.
    CLEAR: IT_EXCL, L_CLINT, L_ATINN.
  ENDLOOP.
ENDFORM.                    " ERROR_CHECK1
