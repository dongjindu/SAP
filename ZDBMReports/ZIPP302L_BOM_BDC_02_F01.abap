*----------------------------------------------------------------------*
*   INCLUDE ZIPP302L_BOM_BDC_02_F01                                    *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
FORM INITIALIZATION.
* BDC MODE, DEFAULT SIZE, UPDATE MODE
  WA_OPT-DEFSIZE = 'X'.
  WA_OPT-DISMODE = 'N'.
  WA_OPT-UPDMODE = 'S'.
ENDFORM.                    " INITIALIZATION
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
      CONCATENATE P_FILE TEXT-002
                  INTO L_TEXT.
      WRITE: / L_TEXT.
      SKIP.
    WHEN 2.
      MESSAGE E000 WITH TEXT-003.
    WHEN 3.
      MESSAGE E000 WITH TEXT-004.
    WHEN OTHERS.
      MESSAGE E000 WITH TEXT-005.
  ENDCASE.

ENDFORM.                               " UPLOAD_PROCESS
*&---------------------------------------------------------------------*
*&      Form  BDC_PROCESS
*&---------------------------------------------------------------------*
FORM BDC_PROCESS.
* NON COLOR PART BDC
  PERFORM NON_COLOR_PART_BDC.
* COLOR PART BDC
  PERFORM COLOR_PART_BDC.
ENDFORM.                    " BDC_PROCESS
*&---------------------------------------------------------------------*
*&      Form  NON_COL_END_ITEM_CREATE
*&---------------------------------------------------------------------*
FORM NON_COL_END_ITEM_CREATE.
  PERFORM DYNPRO USING:
     'X' 'SAPLCSDI'    '0140',
     ' ' 'BDC_OKCODE'  '=FCBU'.
* CALL TRANSACTION
  CALL TRANSACTION 'CS02'  USING IT_BDC
                           OPTIONS FROM WA_OPT
                           MESSAGES INTO IT_MESS.
ENDFORM.                    " NON_COL_END_ITEM_CREATE
*&---------------------------------------------------------------------*
*&      Form  NON_COL_CHANGE_HEADER_BOM
*&---------------------------------------------------------------------*
FORM NON_COL_CHANGE_HEADER_BOM USING  PA_NCOL STRUCTURE WA_NCOL.
  PERFORM DYNPRO USING:
     'X' 'SAPLCSDI'    '0100',
     ' ' 'RC29N-MATNR' PA_NCOL-MATNR,    "NEXT MATERIAL
     ' ' 'RC29N-WERKS' PA_NCOL-WERKS,   "PLANT
     ' ' 'RC29N-STLAN' PA_NCOL-STLAN,   "BOM usage
     ' ' 'RC29N-STLAL' PA_NCOL-STLAL,   "ALT BOM
     ' ' 'RC29N-AENNR' PA_NCOL-AENNR,   "Change number
     ' ' 'BDC_OKCODE'  '=FCPU',

     'X' 'SAPLCSDI'    '0150',
     ' ' 'BDC_OKCODE'  '=FCNP'.

ENDFORM.                    " NON_COL_CHANGE_HEADER_BOM
*&---------------------------------------------------------------------*
*&      Form  NON_COL_BADY_ITEM_CREATE1
*&---------------------------------------------------------------------*
FORM   NON_COL_BADY_ITEM_CREATE1 USING PA_NCOL STRUCTURE WA_NCOL.
  DATA: L_DBCNT TYPE SY-DBCNT.
  PERFORM DYNPRO USING:
     'X' 'SAPLCSDI'    '0140',
     ' ' 'RC29P-AUSKZ(02)' 'X'   ,    "CHECK
     ' ' 'RC29P-POSNR(02)' PA_NCOL-POSNR,   "BOM item number
     ' ' 'RC29P-IDNRK(02)' PA_NCOL-IDNRK,   "BOM compenent
     ' ' 'RC29P-MENGE(02)' PA_NCOL-MENGE,   "Compenent quantity
     ' ' 'RC29P-POSTP(02)' PA_NCOL-POSTP,   "Item category
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCSDI'    '0130',
     ' ' 'RC29P-ITSOB' PA_NCOL-ITSOB,   "procurement type
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCSDI'    '0131',
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCSDI'    '0138',
     ' ' 'ZEITM'       PA_NCOL-ZEITM,    "END ITEM TYPE
     ' ' 'ZSTGB'       PA_NCOL-ZSTGB,    "STRUCTURE TYPE
     ' ' 'ZSUFF'       PA_NCOL-ZSUFF,    "SUFFIX NO
     ' ' 'ZSEQU'       PA_NCOL-ZSEQU,    "SEQUENCE NO
     ' ' 'ZUPGN'       PA_NCOL-ZUPGN,    "UPG
     ' ' 'ZINFO'       PA_NCOL-ZSEQC,    "COLOR SEQUENCE
     ' ' 'BDC_OKCODE'  '/00'.

  IF PA_NCOL-CLPT EQ 'C'.
    SELECT COUNT( * )
         FROM MAST AS A INNER JOIN STPO AS B
                        ON A~STLNR EQ B~STLNR
         INTO L_DBCNT
         WHERE A~MATNR EQ PA_NCOL-MATNR
         AND   A~WERKS EQ PA_NCOL-WERKS
         AND   A~STLAN EQ PA_NCOL-STLAN
         AND   A~STLAL EQ PA_NCOL-STLAL.
    IF L_DBCNT GE '1'.
      PERFORM DYNPRO USING:
         'X' 'SAPLCSDI'    '0140',
         ' ' 'RC29P-AUSKZ(02)' 'X',                "
         ' ' 'BDC_OKCODE'  '=WIZU'.
    ELSE.
      PERFORM DYNPRO USING:
         'X' 'SAPLCSDI'    '0140',
         ' ' 'RC29P-AUSKZ(01)' 'X',                "
         ' ' 'BDC_OKCODE'  '=WIZU'.
    ENDIF.
    PERFORM DYNPRO USING:
     'X' 'SAPLCUKD'    '0130',
      ' ' 'RCUKD-KNNAM(01)' PA_NCOL-DPID,      " O/J DEPENDENCY
      ' ' 'BDC_OKCODE'  '/00',

      'X' 'SAPLCUKD'    '0130',
      ' ' 'BDC_OKCODE'  '=BACK'.
  ENDIF.
  PERFORM DYNPRO USING:
       'X' 'SAPLCSDI'    '0140',
       ' ' 'BDC_OKCODE'  '=FCNP'.

ENDFORM.                    " NON_COL_BADY_ITEM_CREATE1
*&---------------------------------------------------------------------*
*&      Form  NON_COL_READ_MAST_STPO_STLKN
*&---------------------------------------------------------------------*
FORM NON_COL_READ_MAST_STPO_STLKN USING    PA_NCOL STRUCTURE WA_NCOL
                                  CHANGING P_MODE
                                           P_STLKN.
  DATA: L_ZSEQU(4) TYPE N,
        L_STLNR TYPE MAST-STLNR,
        L_CNT TYPE I.
  L_ZSEQU = PA_NCOL-ZSEQU.
  L_ZSEQU = L_ZSEQU - 1.

  SELECT SINGLE A~STLNR
                B~STLKN
       FROM MAST AS A INNER JOIN STPO AS B
                      ON    B~STLTY EQ 'M'
                      AND   A~STLNR EQ B~STLNR
       INTO  (L_STLNR, P_STLKN)
       WHERE A~MATNR EQ PA_NCOL-MATNR
       AND   A~WERKS EQ PA_NCOL-WERKS
       AND   A~STLAN EQ PA_NCOL-STLAN
       AND   A~STLAL EQ PA_NCOL-STLAL
       AND   B~POSNR EQ PA_NCOL-POSNR
       AND   B~IDNRK EQ PA_NCOL-IDNRK
       AND   B~SUFF  EQ PA_NCOL-ZSUFF
       AND   B~SEQU  EQ L_ZSEQU.
  IF SY-SUBRC EQ 0.
    SELECT COUNT( DISTINCT STASZ )
         FROM STAS
         INTO L_CNT
         WHERE STLTY EQ 'M'
         AND   STLNR EQ L_STLNR
         AND   STLAL EQ PA_NCOL-STLAL
         AND   STLKN EQ P_STLKN.
    IF L_CNT GT 1.
      P_MODE = '4'.
    ELSE.
      P_MODE = '3'.
    ENDIF.
  ELSE.
    P_MODE = '4'.
  ENDIF.
ENDFORM.                    " NON_COL_READ_MAST_STPO_STLKN
*&---------------------------------------------------------------------*
*&      Form  NON_COL_MAST_STPO_READ
*&---------------------------------------------------------------------*
FORM NON_COL_MAST_STPO_READ USING    PA_BMNC STRUCTURE WA_BMNC
                                  CHANGING P_MODE
                                           P_STLKN.
  DATA: L_ZSEQU(4) TYPE N,
        L_STLNR TYPE MAST-STLNR,
        L_CNT TYPE I.
  L_ZSEQU = PA_BMNC-SEQU.
  L_ZSEQU = L_ZSEQU - 1.

  SELECT SINGLE A~STLNR
                B~STLKN
       FROM MAST AS A INNER JOIN STPO AS B
                      ON    B~STLTY EQ 'M'
                      AND   A~STLNR EQ B~STLNR
       INTO  (L_STLNR, P_STLKN)
       WHERE A~MATNR EQ PA_BMNC-MTNO
       AND   A~WERKS EQ PA_BMNC-PLNT
       AND   A~STLAN EQ PA_BMNC-USAG
       AND   A~STLAL EQ PA_BMNC-ALTN
       AND   B~POSNR EQ PA_BMNC-PREF
       AND   B~IDNRK EQ PA_BMNC-COMP
       AND   B~SUFF  EQ PA_BMNC-SUFF
       AND   B~SEQU  EQ L_ZSEQU.
  IF SY-SUBRC EQ 0.
    SELECT COUNT( DISTINCT STASZ )
         FROM STAS
         INTO L_CNT
         WHERE STLTY EQ 'M'
         AND   STLNR EQ L_STLNR
         AND   STLAL EQ PA_BMNC-ALTN
         AND   STLKN EQ P_STLKN.
    IF L_CNT GT 1.
      P_MODE = '4'.
    ELSE.
      P_MODE = '3'.
    ENDIF.
  ELSE.
    P_MODE = '4'.
  ENDIF.
ENDFORM.                    " NON_COL_MAST_STPO_READ
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
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
FORM DATA_PROCESS CHANGING PA_CHECK.
  PERFORM DATA_CHECK_MARC CHANGING PA_CHECK.
ENDFORM.                    " DATA_PROCESS
*&---------------------------------------------------------------------*
*&      Form  DATA_CHECK_MARC
*&---------------------------------------------------------------------*
FORM DATA_CHECK_MARC CHANGING PA_CHECK.
  DATA L_TABIX TYPE SY-TABIX.
  DATA: BEGIN OF LT_CHCK OCCURS 0,
          MATNR TYPE MARC-MATNR,
          WERKS TYPE MARC-WERKS,
        END OF LT_CHCK.
  DATA: BEGIN OF LT_MARC OCCURS 0,
          MATNR TYPE MARC-MATNR,
          WERKS TYPE MARC-WERKS,
        END OF LT_MARC.
  LOOP AT IT_EXCL.
    LT_CHCK-MATNR = IT_EXCL-MATNR.
    LT_CHCK-WERKS = IT_EXCL-WERKS.
    COLLECT LT_CHCK.
    LT_CHCK-MATNR = IT_EXCL-IDNRK.
    LT_CHCK-WERKS = IT_EXCL-WERKS.
    COLLECT LT_CHCK.
    CLEAR: IT_EXCL, LT_CHCK.
  ENDLOOP.

  IF NOT LT_CHCK[] IS INITIAL.
    SELECT MATNR
           WERKS
         FROM MARC
         INTO TABLE LT_MARC
         FOR ALL ENTRIES IN LT_CHCK
         WHERE MATNR EQ LT_CHCK-MATNR
         AND   WERKS EQ LT_CHCK-WERKS.
    IF SY-SUBRC EQ 0.
      SORT LT_MARC BY MATNR WERKS.
      LOOP AT IT_EXCL.
        L_TABIX = SY-TABIX.
        READ TABLE LT_MARC WITH KEY MATNR = IT_EXCL-MATNR
                                    WERKS = IT_EXCL-WERKS
                           BINARY SEARCH.
        IF SY-SUBRC NE 0.
          PA_CHECK = 'X'.
          IT_EXCL-ZRESULT = 'L'.
          CONCATENATE 'NENT MATERIAL : ' IT_EXCL-MATNR
                      '  Material number does not exist'
                 INTO IT_EXCL-ZMSG.
          MODIFY IT_EXCL INDEX L_TABIX TRANSPORTING ZRESULT
                                                    ZMSG.
        ENDIF.
        READ TABLE LT_MARC WITH KEY MATNR = IT_EXCL-IDNRK
                                    WERKS = IT_EXCL-WERKS
                           BINARY SEARCH.
        IF SY-SUBRC NE 0.
          PA_CHECK = 'X'.
          IT_EXCL-ZRESULT = 'L'.
          IF IT_EXCL-ZMSG IS INITIAL.
            CONCATENATE  'COMPORNENT : ' IT_EXCL-IDNRK
                         '  Material number does not exist'
                    INTO IT_EXCL-ZMSG.
          ELSE.
            CONCATENATE  IT_EXCL-ZMSG '  COMPORNENT : ' IT_EXCL-IDNRK
                         '  Material number does not exist'
                    INTO IT_EXCL-ZMSG.
          ENDIF.

          MODIFY IT_EXCL INDEX L_TABIX TRANSPORTING ZRESULT
                                                    ZMSG.
        ENDIF.
      ENDLOOP.
    ELSE.
      PA_CHECK = 'X'.
      IT_EXCL-ZRESULT = 'L'.
      IT_EXCL-ZMSG = ' Material number does not exist'.
      MODIFY IT_EXCL TRANSPORTING ZMSG
                                  ZRESULT
                               WHERE MATNR GE SPACE
                               AND   WERKS GE SPACE.
    ENDIF.
  ENDIF.
  IF PA_CHECK EQ 'X'.
    CLEAR WA_ERRO_IDX.
    LOOP AT IT_EXCL WHERE ZRESULT EQ 'L'.
      WA_ERRO_IDX = WA_ERRO_IDX + 1.
    ENDLOOP.
    FORMAT COLOR COL_HEADING INTENSIFIED OFF.
    WRITE: / TEXT-006, WA_ERRO_IDX.
    FORMAT COLOR OFF.
    PERFORM ERROR_WRITE TABLES IT_EXCL
                        USING  'L'.
  ENDIF.
ENDFORM.                    " DATA_CHECK_MARC
*&---------------------------------------------------------------------*
*&      Form  BOM_HEADER
*&---------------------------------------------------------------------*
FORM BOM_HEADER USING P_MATNR
                      P_WERKS
                      P_STLAN
                      P_STLAL
                      P_BMENG
                      P_STLST.
  DATA: L_MSG   LIKE CFGNL-MSGLIN,
        L_MSGTY TYPE SY-MSGTY.
  PERFORM DYNPRO USING:
     'X' 'SAPLCSDI'    '0100',
     ' ' 'RC29N-MATNR' P_MATNR,    "NEXT MATERIAL
     ' ' 'RC29N-WERKS' P_WERKS,    "PLANT
     ' ' 'RC29N-STLAN' P_STLAN,    "BOM STLANe
     ' ' 'RC29N-STLAL' P_STLAL,    "ALT BOM
     ' ' 'RC29N-AENNR' '19000101-001',    "Change number
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCSDI'    '0110',
     ' ' 'RC29K-BMENG' P_BMENG,    "Confirmed quantity
     ' ' 'RC29K-STLST' P_STLST,    "BOM STATUS
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCSDI'    '0111',
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCSDI'    '0140',
     ' ' 'BDC_OKCODE'  '=FCBU'.
  CALL TRANSACTION 'CS01'  USING IT_BDC
                           OPTIONS FROM WA_OPT
                           MESSAGES INTO IT_MESS.
  L_MSGTY = SY-MSGTY.
  PERFORM RKC_MSG_STRING CHANGING L_MSG.

  REFRESH: IT_BDC, IT_MESS.
ENDFORM.         " BOM_HEADER
*&---------------------------------------------------------------------*
*&      Form  ERROR_WRITE
*&---------------------------------------------------------------------*
FORM ERROR_WRITE TABLES   PT_EXCL STRUCTURE IT_EXCL
                 USING    P_ZRESULT.
  WRITE: /(18)  TEXT-007,
          (05)  TEXT-008,
          (05)  TEXT-009,
          (06)  TEXT-010,
          (12)  TEXT-011,
          (07)  TEXT-012,
          (08)  TEXT-013,
          (06)  TEXT-014,
          (07)  TEXT-015,
          (03)  TEXT-016,
          (18)  TEXT-017,
          (10)  TEXT-018,
          (08)  TEXT-019,
          (08)  TEXT-020,
          (08)  TEXT-021,
          (08)  TEXT-022,
          (10)  TEXT-023,
          (30)  TEXT-024,
          (12)  TEXT-025,
          (08)  TEXT-026,
                TEXT-027.
  LOOP AT PT_EXCL WHERE ZRESULT EQ P_ZRESULT.
    WRITE: /(18) PT_EXCL-MATNR,
            (05) PT_EXCL-WERKS,
            (05) PT_EXCL-STLAN,
            (06) PT_EXCL-STLAL,
            (12) PT_EXCL-AENNR,
            (07) PT_EXCL-BMENG,
            (08) PT_EXCL-BMEIN,
            (06) PT_EXCL-STLST,
            (07) PT_EXCL-POSNR,
            (03) PT_EXCL-POSTP,
            (18) PT_EXCL-IDNRK,
            (10) PT_EXCL-MENGE,
            (08) PT_EXCL-MEINS,
            (08) PT_EXCL-ITSOB,
            (08) PT_EXCL-ZEITM,
            (08) PT_EXCL-ZSUFF,
            (10) PT_EXCL-CLPT,
            (30) PT_EXCL-DPID,
            (12) PT_EXCL-UPCT,
            (08) PT_EXCL-ZRESULT,
                 PT_EXCL-ZMSG.
  ENDLOOP.
ENDFORM.                    " ERROR_WRITE
*&---------------------------------------------------------------------*
*&      Form  BOM_EXPLODED
*&---------------------------------------------------------------------*
FORM BOM_EXPLODED TABLES   P_BOM_EXPLODED STRUCTURE IT_BOM_EXPLODED.
  DATA: BEGIN OF LT_MARC OCCURS 0,
          MATNR LIKE MARC-MATNR,  "MATERIAL
          WERKS LIKE MARC-WERKS,  "PLANT
          MTART LIKE MARA-MTART,  "MATERIAL TYPE
        END   OF LT_MARC.
  DATA L_TABIX TYPE SY-TABIX.
* MATERIAL & COMPENENT COLLECT
  LOOP AT IT_COLO WHERE UPCT NE '2'
                  AND   ZRESULT NE 'E'.
    LT_MARC-MATNR = IT_COLO-MATNR.
    LT_MARC-WERKS = IT_COLO-WERKS.
    COLLECT LT_MARC.
    CLEAR: LT_MARC.
    LT_MARC-MATNR = IT_COLO-IDNRK.
    LT_MARC-WERKS = IT_COLO-WERKS.
    COLLECT LT_MARC.
    CLEAR: LT_MARC, IT_COLO.
  ENDLOOP.
  IF NOT LT_MARC[] IS INITIAL.
*   SELECTION MARA--->MODIFY LT_MARC-MTART(MATERIAL TYPE)
*                            LT_MARC-KZKFG(Configurable Material)
    PERFORM READ_MARA TABLES LT_MARC.

    LOOP AT LT_MARC WHERE MTART EQ 'FERT'.
      L_TABIX = SY-TABIX.
      P_BOM_EXPLODED-MATNR = LT_MARC-MATNR.
      P_BOM_EXPLODED-WERKS = LT_MARC-WERKS.
      APPEND P_BOM_EXPLODED.
      DELETE LT_MARC INDEX L_TABIX.
      CLEAR: P_BOM_EXPLODED, LT_MARC.
    ENDLOOP.

    LOOP AT LT_MARC.
      SORT P_BOM_EXPLODED.
      READ TABLE P_BOM_EXPLODED WITH KEY MATNR = LT_MARC-MATNR
                                         WERKS = LT_MARC-WERKS
                                BINARY SEARCH.
      IF SY-SUBRC NE 0.
*       BOM EXPLODED.
        PERFORM READ_BOM TABLES P_BOM_EXPLODED
                        USING LT_MARC-MATNR
                              LT_MARC-WERKS
                              SY-DATUM   "IDNRK WERKS DATUV
                        CHANGING WA_LAST.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " BOM_EXPLODED
*&---------------------------------------------------------------------*
*&      Form  READ_MARA
*&---------------------------------------------------------------------*
FORM READ_MARA TABLES PT_MARC.
  DATA: BEGIN OF LT_MARC OCCURS 0,
          MATNR LIKE MARC-MATNR,  "MATERIAL
          WERKS LIKE MARC-WERKS,  "PLANT
          MTART LIKE MARA-MTART,  "MATERIAL TYPE
        END   OF LT_MARC.
  DATA: BEGIN OF LT_MARA OCCURS 0,
          MATNR LIKE MARA-MATNR, "MATERIAL
          MTART LIKE MARA-MTART, "MATERIAL TYPE
        END   OF LT_MARA.
  DATA: L_TABIX TYPE SY-TABIX.
* INTERNAL TABLE MOVE
  LT_MARC[] = PT_MARC[].
* REFRESH
  REFRESH PT_MARC.
  SELECT MATNR
         MTART
       FROM MARA
       INTO TABLE LT_MARA
       FOR ALL ENTRIES IN LT_MARC
       WHERE MATNR EQ LT_MARC-MATNR.
  IF SY-SUBRC EQ 0.
*   SORTING
    SORT LT_MARA BY MATNR MTART.
*   MODIFY MTART(Material type)
    LOOP AT LT_MARC.
      L_TABIX = SY-TABIX.
      READ TABLE LT_MARA WITH KEY MATNR = LT_MARC-MATNR
                         BINARY SEARCH TRANSPORTING MTART.
      IF SY-SUBRC EQ 0.
        LT_MARC-MTART = LT_MARA-MTART.
        MODIFY LT_MARC INDEX L_TABIX TRANSPORTING MTART.
      ENDIF.
      CLEAR: LT_MARC, LT_MARA.
    ENDLOOP.
  ENDIF.
* INTERNAL TABLE MOVE
  PT_MARC[] = LT_MARC[].
ENDFORM.                    " READ_MARA
*&---------------------------------------------------------------------*
*&      Form  READ_BOM
*&---------------------------------------------------------------------*
FORM READ_BOM TABLES P_BOM_EXPLODED STRUCTURE IT_BOM_EXPLODED
              USING P_IDNRK
                    P_WERKS
                    P_DATUV
              CHANGING P_LAST.

  DATA : TEMP_ISTPOV LIKE STPOV OCCURS 0 WITH HEADER LINE.
* REFRESH: IMC29S, ISTPOV, ICSCEQUI, ICSCKND, ICSCMAT, ICSCSTD, ICSCTPL.
* CLEAR : IMC29S, ISTPOV, ICSCEQUI, ICSCKND, ICSCMAT, ICSCSTD, ICSCTPL.

  CALL FUNCTION 'CS_WHERE_USED_MAT'
       EXPORTING
            DATUB                      = P_DATUV
            DATUV                      = P_DATUV
            MATNR                      = P_IDNRK
            WERKS                      = P_WERKS
       IMPORTING
            TOPMAT                     = IT_MC29S
       TABLES
            WULTB                      = IT_STPOV
            EQUICAT                    = IT_CSCEQUI
            KNDCAT                     = IT_CSCKND
            MATCAT                     = IT_CSCMAT
            STDCAT                     = IT_CSCSTD
            TPLCAT                     = IT_CSCTPL
       EXCEPTIONS
            CALL_INVALID               = 1
            MATERIAL_NOT_FOUND         = 2
            NO_WHERE_USED_REC_FOUND    = 3
            NO_WHERE_USED_REC_SELECTED = 4
            NO_WHERE_USED_REC_VALID    = 5
            OTHERS                     = 6.

  TEMP_ISTPOV[] = IT_STPOV[].
  IF SY-SUBRC <> 0.
    P_LAST = 'X'.
  ELSE.
    LOOP AT TEMP_ISTPOV.
      SORT P_BOM_EXPLODED.
      READ TABLE P_BOM_EXPLODED WITH KEY MATNR = TEMP_ISTPOV-MATNR
                                         WERKS = TEMP_ISTPOV-WERKS
                                BINARY SEARCH.
      IF SY-SUBRC NE 0.
        PERFORM READ_BOM TABLES P_BOM_EXPLODED
                         USING TEMP_ISTPOV-MATNR
                               TEMP_ISTPOV-WERKS
                               P_DATUV
                         CHANGING P_LAST.
        READ TABLE P_BOM_EXPLODED WITH KEY MATNR = TEMP_ISTPOV-MATNR
                                           WERKS = TEMP_ISTPOV-WERKS
                                  BINARY SEARCH.
        IF SY-SUBRC NE 0.
          P_BOM_EXPLODED-MATNR = TEMP_ISTPOV-MATNR.
          P_BOM_EXPLODED-WERKS = TEMP_ISTPOV-WERKS.
          APPEND P_BOM_EXPLODED. CLEAR P_BOM_EXPLODED.
          CLEAR P_LAST.
        ENDIF.
      ENDIF.
      CLEAR TEMP_ISTPOV.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " READ_BOM
*&---------------------------------------------------------------------*
*&      Form  MARA_CONFIGURABLE_MATERIAL
*&---------------------------------------------------------------------*
FORM MARA_CONFIGURABLE_MATERIAL TABLES   P_BOM_EXPLODED
                                              STRUCTURE IT_BOM_EXPLODED.
  DATA: BEGIN OF LT_MARA OCCURS 0,
          MATNR TYPE MARA-MATNR,
          KZKFG TYPE MARA-KZKFG,  "Configurable Material
        END   OF LT_MARA.
  DATA: L_TABIX TYPE SY-TABIX.
  SELECT MATNR
         KZKFG
       FROM MARA
       INTO TABLE LT_MARA
       FOR ALL ENTRIES IN P_BOM_EXPLODED
       WHERE MATNR EQ P_BOM_EXPLODED-MATNR.
  IF SY-SUBRC EQ 0.
*   SORTING
    SORT LT_MARA BY MATNR.
    LOOP AT P_BOM_EXPLODED.
      L_TABIX = SY-TABIX.
      READ TABLE LT_MARA WITH KEY MATNR = P_BOM_EXPLODED-MATNR
                         BINARY SEARCH TRANSPORTING KZKFG.
      IF SY-SUBRC EQ 0.
        P_BOM_EXPLODED-KZKFG = LT_MARA-KZKFG.
        MODIFY P_BOM_EXPLODED INDEX L_TABIX TRANSPORTING KZKFG.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " MARA_CONFIGURABLE_MATERIAL
*&---------------------------------------------------------------------*
*&      Form  MM02_CONFIGURABLE_MATERIAL_BDC
*&---------------------------------------------------------------------*
FORM MM02_CONFIGURABLE_MATERIAL_BDC.

  DATA: L_TABIX TYPE SY-TABIX.
  REFRESH: IT_BDC, IT_MESS.
  CLEAR  : IT_BDC, IT_MESS.
  LOOP AT IT_BOM_EXPLODED WHERE KZKFG NE 'X'.
    L_TABIX = SY-TABIX.
    PERFORM DYNPRO USING:
       'X' 'SAPLMGMM'    '0060',
       ' ' 'RMMG1-MATNR' IT_BOM_EXPLODED-MATNR,   "
       ' ' 'BDC_OKCODE'  '=AUSW',

       'X' 'SAPLMGMM'    '0070',
       ' ' 'MSICHTAUSW-KZSEL(02)' 'X',   "
       ' ' 'BDC_OKCODE'  '=BILD',
*       2003/12/21 SCREEN NUMBER CHANGE
*       'X' 'SAPLMGMM'    '4004',
       'X' 'SAPLMGMM'    '5004',
       ' ' 'MARA-KZKFG'  'X',   "
       ' ' 'BDC_OKCODE'  '=BU'.

    CALL TRANSACTION 'MM02'  USING IT_BDC
             OPTIONS FROM WA_OPT
             MESSAGES INTO IT_MESS.
*    IT_BOM_EXPLODED-ZRESULT = SY-MSGTY.
    PERFORM RKC_MSG_STRING CHANGING IT_EXP_MESS-MSG.
*    IT_BOM_EXPLODED-ZMSG = L_MSG.
*    MODIFY IT_BOM_EXPLODED INDEX L_TABIX TRANSPORTING ZRESULT
*                                                      ZMSG.
    IT_EXP_MESS = IT_BOM_EXPLODED.
    APPEND IT_EXP_MESS.
    CLEAR IT_EXP_MESS.
    REFRESH: IT_BDC, IT_MESS.
    CLEAR  : IT_BDC, IT_MESS.
  ENDLOOP.
ENDFORM.                    " MM02_CONFIGURABLE_MATERIAL_BDC
*&---------------------------------------------------------------------*
*&      Form  NON_COL_BOM_ITEM_DELETE
*&---------------------------------------------------------------------*
FORM NON_COL_BOM_ITEM_DELETE USING    PA_NCOL STRUCTURE WA_NCOL
                              P_STLKN.
  PERFORM DYNPRO USING:
     'X' 'SAPLCSDI'    '0100',
     ' ' 'RC29N-MATNR' PA_NCOL-MATNR,   "NEXT MATERIAL
     ' ' 'RC29N-WERKS' PA_NCOL-WERKS,   "PLANT
     ' ' 'RC29N-STLAN' PA_NCOL-STLAN,   "BOM usage
     ' ' 'RC29N-STLAL' PA_NCOL-STLAL,   "ALT BOM
     ' ' 'RC29N-AENNR' PA_NCOL-AENNR,   "Change number
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCSDI'    '0150',
     ' ' 'BDC_OKCODE'  '=SETP',

     'X' 'SAPLCSDI'    '0708',
     ' ' 'RC29P-SELPI' P_STLKN,                "
     ' ' 'BDC_OKCODE'  '=CLWI',

     'X' 'SAPLCSDI'    '0150',
     ' ' 'RC29P-AUSKZ(01)' 'X',                "
     ' ' 'BDC_OKCODE'  '=FCDL',

     'X' 'SAPLCSDI'    '0150',
     ' ' 'BDC_OKCODE'  '=FCBU'.

* CALL TRANSACTION
  CALL TRANSACTION 'CS02'  USING IT_BDC
                           OPTIONS FROM WA_OPT
                           MESSAGES INTO IT_MESS.
ENDFORM.                    " NON_COL_BOM_ITEM_DELETE
*&---------------------------------------------------------------------*
*&      Form  TABLE_NON_COL_BOM_DELETE
*&---------------------------------------------------------------------*
FORM TABLE_NON_COL_BOM_DELETE USING    PA_BMNC STRUCTURE WA_BMNC
                                       P_STLKN.
  PERFORM DYNPRO USING:
     'X' 'SAPLCSDI'    '0100',
     ' ' 'RC29N-MATNR' PA_BMNC-MTNO,    "NEXT MATERIAL
     ' ' 'RC29N-WERKS' PA_BMNC-PLNT,    "PLANT
     ' ' 'RC29N-STLAN' PA_BMNC-USAG,    "BOM usage
     ' ' 'RC29N-STLAL' PA_BMNC-ALTN,    "ALT BOM
     ' ' 'RC29N-AENNR' PA_BMNC-EONO,    "Change number
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCSDI'    '0150',
     ' ' 'BDC_OKCODE'  '=SETP',

     'X' 'SAPLCSDI'    '0708',
     ' ' 'RC29P-SELPI' P_STLKN,                "
     ' ' 'BDC_OKCODE'  '=CLWI',

     'X' 'SAPLCSDI'    '0150',
     ' ' 'RC29P-AUSKZ(01)' 'X',                "
     ' ' 'BDC_OKCODE'  '=FCDL',

     'X' 'SAPLCSDI'    '0150',
     ' ' 'BDC_OKCODE'  '=FCBU'.

* CALL TRANSACTION
  CALL TRANSACTION 'CS02'  USING IT_BDC
                           OPTIONS FROM WA_OPT
                           MESSAGES INTO IT_MESS.
ENDFORM.                    " TABLE_NON_COL_BOM_DELETE
*&---------------------------------------------------------------------*
*&      Form  DEPENDENCY_ECM_CHECK
*&---------------------------------------------------------------------*
FORM DEPENDENCY_ECM_CHECK CHANGING PA_CHECK.
  DATA: LT_EXCL LIKE IT_EXCL OCCURS 0 WITH HEADER LINE,
        L_KNNAM LIKE CUKB-KNNAM.
  DATA: BEGIN OF LT_AENR OCCURS 0,
          AENNR TYPE AENR-AENNR,
        END OF LT_AENR.
  DATA: BEGIN OF MT_AENR OCCURS 0,
          AENNR TYPE AENR-AENNR,
        END OF MT_AENR.
  CLEAR: WA_ERRO_IDX, WA_LINE_IDX.
* DEPENDENCY CHECK
  LOOP AT IT_EXCL WHERE CLPT EQ 'C'
                  AND   UPCT EQ '1'.
    WA_LINE_IDX = WA_LINE_IDX + 1.
    SELECT SINGLE KNNAM
                FROM CUKB
                INTO L_KNNAM
                WHERE KNNAM EQ IT_EXCL-DPID.
    IF SY-SUBRC NE 0.
      LT_EXCL = IT_EXCL.
      LT_EXCL-ZRESULT = 'L'.
      LT_EXCL-ZMSG = 'Dependency does not exist'.
      APPEND LT_EXCL.
      WA_ERRO_IDX = WA_ERRO_IDX + 1.
      PA_CHECK = 'X'.
    ENDIF.
    CLEAR: IT_EXCL, LT_EXCL.
  ENDLOOP.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE: / TEXT-028, WA_LINE_IDX.
  WRITE: / TEXT-029,
            WA_ERRO_IDX.
  FORMAT COLOR OFF.
  IF WA_ERRO_IDX GE '1'.
    PERFORM ERROR_WRITE TABLES LT_EXCL
                        USING  'L'.
  ENDIF.
  REFRESH LT_EXCL. CLEAR LT_EXCL.
* CHANGE NUMBER CHECK
  LOOP AT IT_EXCL.
    IT_SUBM-STLAN = IT_EXCL-STLAN.
    IT_SUBM-STLAL = IT_EXCL-STLAL.
    IT_SUBM-MATNR = IT_EXCL-MATNR.
    IT_SUBM-WERKS = IT_EXCL-WERKS.
    COLLECT IT_SUBM.
    LT_AENR-AENNR = IT_EXCL-AENNR.
    COLLECT LT_AENR.
    CLEAR: IT_EXCL, LT_AENR, IT_SUBM.
  ENDLOOP.
  IF NOT LT_AENR[] IS INITIAL.
    SELECT AENNR
         FROM AENR
         INTO TABLE MT_AENR
         FOR ALL ENTRIES IN LT_AENR
         WHERE AENNR EQ LT_AENR-AENNR.
    IF SY-SUBRC EQ 0.
      SORT MT_AENR.
      CLEAR: WA_ERRO_IDX, WA_LINE_IDX.
      LOOP AT IT_EXCL.
        WA_LINE_IDX = WA_LINE_IDX + 1.
        READ TABLE MT_AENR WITH KEY AENNR = IT_EXCL-AENNR
                           BINARY SEARCH.
        IF SY-SUBRC NE 0.
          WA_ERRO_IDX = WA_ERRO_IDX + 1.
          LT_EXCL = IT_EXCL.
          LT_EXCL-ZRESULT = 'L'.
          LT_EXCL-ZMSG = 'Change No does not exist'.
          APPEND LT_EXCL.
          PA_CHECK = 'X'.
        ENDIF.
      ENDLOOP.
      DESCRIBE TABLE IT_EXCL LINES WA_LINE_IDX.
      DESCRIBE TABLE LT_EXCL LINES WA_ERRO_IDX.
      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
      WRITE: / TEXT-030, WA_LINE_IDX.
      WRITE: / TEXT-031,
                WA_ERRO_IDX.
      FORMAT COLOR OFF.
      IF WA_ERRO_IDX GE '1'.
        PERFORM ERROR_WRITE TABLES LT_EXCL
                            USING  'L'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " DEPENDENCY_ECM_CHECK
*&---------------------------------------------------------------------*
*&      Form  COLOR_PART_PARTITION
*&---------------------------------------------------------------------*
FORM COLOR_PART_PARTITION.
  LOOP AT IT_EXCL.
    IF IT_EXCL-CLPT EQ 'C'.
      MOVE-CORRESPONDING IT_EXCL TO IT_COLO.
      APPEND IT_COLO. CLEAR IT_COLO.
    ELSE.
      MOVE-CORRESPONDING IT_EXCL TO IT_NCOL.
      APPEND IT_NCOL. CLEAR IT_NCOL.
    ENDIF.
    CLEAR IT_EXCL.
  ENDLOOP.
  REFRESH IT_EXCL. CLEAR IT_EXCL.
ENDFORM.                    " COLOR_PART_PARTITION
*&---------------------------------------------------------------------*
*&      Form  NON_COLOR_PART_BDC
*&---------------------------------------------------------------------*
FORM NON_COLOR_PART_BDC.
  DATA: L_CHK(1),
        L_TABIX TYPE SY-TABIX,
        L_TANEW TYPE SY-TABIX,
        L_TAEND TYPE SY-TABIX,
        L_MODE,
        L_STLKN TYPE STPO-STLKN,
        L_ZRESULT LIKE IT_EXCL-ZRESULT,
        L_ZMSG LIKE IT_EXCL-ZMSG.
* SORTING
  SORT IT_NCOL BY MATNR WERKS STLAN STLAL POSNR
                  IDNRK ZSUFF ZSEQU UPCT  AENNR.
* BDC PROCESS
  LOOP AT IT_NCOL.
    L_TABIX = SY-TABIX.
    WA_NCOL = IT_NCOL.
    CLEAR: L_STLKN.
*   UPDATE CONTROL TYPE
    IF WA_NCOL-UPCT EQ '2'.
*     BOM ITEM DELETE
      PERFORM NON_COL_READ_MAST_STPO_STLKN USING    WA_NCOL
                                           CHANGING L_MODE
                                                    L_STLKN.
      IF L_MODE EQ '3'. "DELETE(ITEM DELETE)
        IF NOT L_STLKN IS INITIAL.
          PERFORM NON_COL_BOM_ITEM_DELETE USING    WA_NCOL
                                        L_STLKN.      "Item node number

        ENDIF.
        PERFORM RKC_MSG_STRING CHANGING L_ZMSG.
        L_ZRESULT = SY-MSGTY.
*       MODIFY IT_NCOL
        IT_NCOL-ZRESULT = L_ZRESULT.
        IT_NCOL-ZMSG    = L_ZMSG.
        MODIFY IT_NCOL INDEX L_TABIX TRANSPORTING ZRESULT
                                                  ZMSG.
      ELSEIF L_MODE EQ '4'.
*       MODIFY IT_NCOL
        IT_NCOL-ZRESULT = 'E'.
        IT_NCOL-ZMSG    = 'BOM ITEM to delete does not exist.'.
        MODIFY IT_NCOL INDEX L_TABIX TRANSPORTING ZRESULT
                                                  ZMSG.
      ENDIF.
      CLEAR IT_NCOL.
      REFRESH: IT_BDC, IT_MESS.
      CLEAR: IT_BDC, IT_MESS.

*   BOM ITEM CREATE
    ELSEIF WA_NCOL-UPCT EQ '1'.
*     AT NEW STLAL
      AT NEW STLAL.
        L_TANEW = L_TABIX.
        CLEAR L_MODE.
        CLEAR IT_STPO. REFRESH IT_STPO.
*       BOM CHECK & BOM HEADER CREATED
        PERFORM MAST_STPO_CHECK USING    WA_NCOL
                                CHANGING L_MODE.
        IF L_MODE EQ '1' OR  L_MODE EQ '2'.
          PERFORM NON_COL_CHANGE_HEADER_BOM USING WA_NCOL.
          L_MODE = '1'.
        ENDIF.
      ENDAT.

*     'UPDATE CONTROL MODE' different BDC
      IF L_MODE EQ '1'.
*       BOM ITEM DATA already CHECK
        CLEAR L_CHK.
        PERFORM CHECK_IT_STPO USING    WA_NCOL
                              CHANGING L_CHK.
        IF L_CHK EQ 'X'.
          PERFORM NON_COL_BADY_ITEM_CREATE1 USING WA_NCOL.
        ELSE.
          IT_NCOL-ZRESULT = 'E'.
          IT_NCOL-ZMSG = 'BOM ITEM DATA already.'.
          MODIFY IT_NCOL INDEX L_TABIX TRANSPORTING ZRESULT
                                                    ZMSG.
          CLEAR IT_NCOL.
        ENDIF.
      ENDIF.

*     AT END OF STLAL
      AT END OF STLAL.
        L_TAEND = L_TABIX.
        IF L_MODE EQ '1'.
          PERFORM NON_COL_END_ITEM_CREATE.
          L_ZRESULT = SY-MSGTY.
          PERFORM RKC_MSG_STRING CHANGING L_ZMSG.
*         MODIFY IT_NCOL
          LOOP AT IT_NCOL FROM L_TANEW TO L_TAEND
                          WHERE ZRESULT NE 'E'.
            IT_NCOL-ZRESULT = L_ZRESULT.
            IT_NCOL-ZMSG    = L_ZMSG.
            MODIFY IT_NCOL INDEX SY-TABIX TRANSPORTING ZRESULT
                                                       ZMSG.
            CLEAR IT_NCOL.
          ENDLOOP.
        ENDIF.

        REFRESH: IT_BDC, IT_MESS.
        CLEAR: IT_BDC, IT_MESS.
      ENDAT.
    ENDIF.
    CLEAR: IT_NCOL, WA_NCOL.
  ENDLOOP.

* BOM ERROR WRITE.
  CLEAR: WA_LINE_IDX, WA_ERRO_IDX.
  REFRESH IT_EXCL. CLEAR IT_EXCL.
* TOTAL LINE COUNT
  DESCRIBE TABLE IT_NCOL LINES WA_LINE_IDX.
* ERROR LINE COUNT
  LOOP AT IT_NCOL.
    IF IT_NCOL-ZRESULT EQ'E'.
      WA_ERRO_IDX = WA_ERRO_IDX + 1.
      MOVE-CORRESPONDING IT_NCOL TO IT_EXCL.
      APPEND IT_EXCL.
    ENDIF.
    CLEAR: IT_NCOL, IT_EXCL.
  ENDLOOP.
  REFRESH IT_NCOL. CLEAR IT_NCOL.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE: / TEXT-032, WA_LINE_IDX.
  WRITE: / TEXT-033, WA_ERRO_IDX.
  FORMAT COLOR OFF.

* Error appears if is more than one case.
  IF WA_ERRO_IDX GE '1'.
    PERFORM ERROR_WRITE TABLES IT_EXCL
                        USING  'E'.
    REFRESH IT_EXCL. CLEAR IT_EXCL.
  ENDIF.
ENDFORM.                    " NON_COLOR_PART_BDC
*&---------------------------------------------------------------------*
*&      Form  COLOR_PART_BDC
*&---------------------------------------------------------------------*
FORM COLOR_PART_BDC.
  DATA: L_TABIX TYPE SY-TABIX,
        L_TANEW TYPE SY-TABIX,
        L_TAEND TYPE SY-TABIX,
        L_MODE,
        L_STLKN TYPE STPO-STLKN,
        L_ZRESULT LIKE IT_EXCL-ZRESULT,
        L_ZMSG LIKE IT_EXCL-ZMSG.

* SORTING
  SORT IT_COLO BY MATNR WERKS STLAN STLAL POSNR
                  IDNRK ZSUFF ZSEQU UPCT AENNR ZSEQC.
  LOOP AT IT_COLO.
    L_TABIX = SY-TABIX.
    WA_COLO = IT_COLO.
    CASE WA_COLO-UPCT.
*     BOM ITEM ADDITION & OBJECT DEPENDENCY CREATE
      WHEN '1'.
*       AT NEW ZSEQU
        AT NEW ZSEQU.
          L_TANEW = L_TABIX.
          CLEAR L_MODE.
          CLEAR IT_STPO. REFRESH IT_STPO.
          PERFORM COLOR_PART_HEADER USING    WA_COLO
                                    CHANGING L_MODE
                                             L_STLKN.
          IF L_MODE EQ '1'. "CHANGE(ITEM CREATE)
            PERFORM CHANGE_HEADER_BOM USING     WA_COLO.
            PERFORM CHANGE_BADY_ITEM USING     WA_COLO.
*          ELSEIF L_MODE EQ '2'.
*            PERFORM APPEND_OBJECT_DEPENDENCY USING     WA_COLO
*                                                       L_STLKN.
          ENDIF.

        ENDAT.
*       AT END OF ZSEQU
        AT END OF ZSEQU.
          L_TAEND = L_TABIX.
          IF L_MODE EQ '1'. "CHANGE(ITEM CREATE)
            CLEAR L_MODE.
            PERFORM COLOR_PART_CALL_TRANSACTION USING    L_TANEW
                                                           L_TAEND
                                                  CHANGING L_ZRESULT
                                                           L_ZMSG
                                                           L_MODE.
          ENDIF.
        ENDAT.
*       'UPDATE CONTROL MODE' different BDC
*        PERFORM COLOR_PART_BADY_1 USING    WA_COLO
*                                  CHANGING L_MODE.
*        IF L_MODE EQ '1'.
*          PERFORM CHANGE_BADY_ITEM_CREATE1 USING     WA_COLO.
*          L_MODE = '2'.
*        ELSEIF L_MODE EQ '2'.
*          PERFORM BADY_DEPENDENCY USING WA_COLO-DPID.
*        ENDIF.
**       AT END OF ZSEQU
*        AT END OF ZSEQU.
*          L_TAEND = L_TABIX.
*          PERFORM COLOR_PART_CALL_TRANSACTION USING    L_TANEW
*                                                       L_TAEND
*                                              CHANGING L_ZRESULT
*                                                       L_ZMSG
*                                                       L_MODE.
*        ENDAT.

*     BOM ITEM DELETE
      WHEN '2'.
        PERFORM COLOR_PART_ITEM_DELETE USING    WA_COLO
                                                L_TABIX
                                       CHANGING L_MODE
                                                L_STLKN
                                                L_ZRESULT
                                                L_ZMSG.
    ENDCASE.

  ENDLOOP.
* BOM ERROR WRITE.
  CLEAR: WA_LINE_IDX, WA_ERRO_IDX.
  REFRESH IT_EXCL. CLEAR IT_EXCL.
* TOTAL LINE COUNT
  DESCRIBE TABLE IT_COLO LINES WA_LINE_IDX.
* ERROR LINE COUNT
  LOOP AT IT_COLO.
    L_TABIX = SY-TABIX.
    IF IT_COLO-ZRESULT EQ'E'.

      WA_ERRO_IDX = WA_ERRO_IDX + 1.
      MOVE-CORRESPONDING IT_COLO TO IT_EXCL.
      APPEND IT_EXCL.
      DELETE IT_COLO INDEX L_TABIX.
    ENDIF.
    CLEAR: IT_COLO, IT_EXCL.
  ENDLOOP.
*  REFRESH IT_COLO. CLEAR IT_COLO.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE: / TEXT-034, WA_LINE_IDX.
  WRITE: / TEXT-035, WA_ERRO_IDX.
  FORMAT COLOR OFF.

* Error appears if is more than one case.
  IF WA_ERRO_IDX GE '1'.
    PERFORM ERROR_WRITE TABLES IT_EXCL
                        USING  'E'.
    REFRESH IT_EXCL. CLEAR IT_EXCL.
  ENDIF.
ENDFORM.                    " COLOR_PART_BDC
*&---------------------------------------------------------------------*
*&      Form  READ_MAST_STPO_STLKN
*&---------------------------------------------------------------------*
FORM READ_MAST_STPO_STLKN USING    P_MATNR
                                   P_WERKS
                                   P_STLAN
                                   P_STLAL
                                   P_POSNR
                                   P_IDNRK
                                   P_ZSUFF
                                   P_ZSEQU
                          CHANGING P_MODE
                                   P_STLKN.
  DATA: L_ZSEQU(4) TYPE N,
        L_STLNR TYPE MAST-STLNR,
        L_CNT TYPE I.
  L_ZSEQU = P_ZSEQU.
  L_ZSEQU = L_ZSEQU - 1.

  SELECT SINGLE A~STLNR
                B~STLKN
       FROM MAST AS A INNER JOIN STPO AS B
                      ON    B~STLTY EQ 'M'
                      AND   A~STLNR EQ B~STLNR
       INTO  (L_STLNR, P_STLKN)
       WHERE A~MATNR EQ P_MATNR
       AND   A~WERKS EQ P_WERKS
       AND   A~STLAN EQ P_STLAN
       AND   A~STLAL EQ P_STLAL
       AND   B~POSNR EQ P_POSNR
       AND   B~IDNRK EQ P_IDNRK
       AND   B~SUFF  EQ P_ZSUFF
       AND   B~SEQU  EQ L_ZSEQU.
  IF SY-SUBRC EQ 0.
    SELECT COUNT( DISTINCT STASZ )
         FROM STAS
         INTO L_CNT
         WHERE STLTY EQ 'M'
         AND   STLNR EQ L_STLNR
         AND   STLAL EQ P_STLAL
         AND   STLKN EQ P_STLKN.
    IF L_CNT GT 1.
      P_MODE = '4'.
    ELSE.
      P_MODE = '3'.
    ENDIF.
  ELSE.
    P_MODE = '4'.
  ENDIF.
ENDFORM.                    " READ_MAST_STPO_STLKN
*&---------------------------------------------------------------------*
*&      Form  BOM_ITEM_DELETE
*&---------------------------------------------------------------------*
FORM BOM_ITEM_DELETE USING    P_MTNO
                              P_PLNT
                              P_USAG
                              P_ALTN
                              P_EONO
                              P_STLKN.
  PERFORM DYNPRO USING:
     'X' 'SAPLCSDI'    '0100',
     ' ' 'RC29N-MATNR' P_MTNO,    "NEXT MATERIAL
     ' ' 'RC29N-WERKS' P_PLNT,    "PLANT
     ' ' 'RC29N-STLAN' P_USAG,    "BOM usage
     ' ' 'RC29N-STLAL' P_ALTN,    "ALT BOM
     ' ' 'RC29N-AENNR' P_EONO,    "Change number
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCSDI'    '0150',
     ' ' 'BDC_OKCODE'  '=SETP',

     'X' 'SAPLCSDI'    '0708',
     ' ' 'RC29P-SELPI' P_STLKN,                "
     ' ' 'BDC_OKCODE'  '=CLWI',

     'X' 'SAPLCSDI'    '0150',
     ' ' 'RC29P-AUSKZ(01)' 'X',                "
     ' ' 'BDC_OKCODE'  '=FCDL',

     'X' 'SAPLCSDI'    '0150',
     ' ' 'BDC_OKCODE'  '=FCBU'.

* CALL TRANSACTION
  CALL TRANSACTION 'CS02'  USING IT_BDC
                           OPTIONS FROM WA_OPT
                           MESSAGES INTO IT_MESS.
ENDFORM.                    " BOM_ITEM_DELETE
*&---------------------------------------------------------------------*
*&      Form  CHANGE_HEADER_BOM
*&---------------------------------------------------------------------*
FORM CHANGE_HEADER_BOM USING   PA_COLO STRUCTURE WA_COLO.

  PERFORM DYNPRO USING:
     'X' 'SAPLCSDI'    '0100',
     ' ' 'RC29N-MATNR' PA_COLO-MATNR,   "NEXT MATERIAL
     ' ' 'RC29N-WERKS' PA_COLO-WERKS,   "PLANT
     ' ' 'RC29N-STLAN' PA_COLO-STLAN,   "BOM usage
     ' ' 'RC29N-STLAL' PA_COLO-STLAL,   "ALT BOM
     ' ' 'RC29N-AENNR' PA_COLO-AENNR,   "Change number
     ' ' 'BDC_OKCODE'  '=FCPU',

     'X' 'SAPLCSDI'    '0150',
     ' ' 'BDC_OKCODE'  '=FCNP'.

ENDFORM.                    " CHANGE_HEADER_BOM
*&---------------------------------------------------------------------*
*&      Form  APPEND_OBJECT_DEPENDENCY
*&---------------------------------------------------------------------*
FORM APPEND_OBJECT_DEPENDENCY USING   PA_COLO STRUCTURE WA_COLO
                                      P_STLKN.

  PERFORM DYNPRO USING:
     'X' 'SAPLCSDI'    '0100',
     ' ' 'RC29N-MATNR' PA_COLO-MATNR,   "NEXT MATERIAL
     ' ' 'RC29N-WERKS' PA_COLO-WERKS,   "PLANT
     ' ' 'RC29N-STLAN' PA_COLO-STLAN,   "BOM usage
     ' ' 'RC29N-STLAL' PA_COLO-STLAL,   "ALT BOM
     ' ' 'RC29N-AENNR' PA_COLO-AENNR,   "Change number
     ' ' 'BDC_OKCODE'  '=FCPU',

     'X' 'SAPLCSDI'    '0150',
     ' ' 'BDC_OKCODE'  '=SETP',

     'X' 'SAPLCSDI'    '0708',
     ' ' 'RC29P-SELPI' P_STLKN,    "
     ' ' 'BDC_OKCODE'  '=CLWI'.
ENDFORM.                    " APPEND_OBJECT_DEPENDENCY
*&---------------------------------------------------------------------*
*&      Form  CHANGE_BADY_ITEM
*&---------------------------------------------------------------------*
FORM   CHANGE_BADY_ITEM USING    PA_COLO STRUCTURE WA_COLO.
  PERFORM DYNPRO USING:
     'X' 'SAPLCSDI'    '0140',
     ' ' 'RC29P-AUSKZ(02)' 'X'   ,    "CHECK
     ' ' 'RC29P-POSNR(02)' PA_COLO-POSNR,   "BOM item number
     ' ' 'RC29P-IDNRK(02)' PA_COLO-IDNRK,   "BOM compenent
     ' ' 'RC29P-MENGE(02)' PA_COLO-MENGE,   "Compenent quantity
     ' ' 'RC29P-POSTP(02)' PA_COLO-POSTP,   "Item category
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCSDI'    '0130',
     ' ' 'RC29P-ITSOB' PA_COLO-ITSOB,   "procurement type
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCSDI'    '0131',
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCSDI'    '0138',
     ' ' 'ZEITM'       PA_COLO-ZEITM,    "END ITEM TYPE
     ' ' 'ZSTGB'       PA_COLO-ZSTGB,    "STRUCTURE TYPE
     ' ' 'ZSUFF'       PA_COLO-ZSUFF,    "SUFFIX NO
     ' ' 'ZSEQU'       PA_COLO-ZSEQU,    "SEQUENCE NO
     ' ' 'ZUPGN'       PA_COLO-ZUPGN,    "UPG
     ' ' 'ZINFO'       PA_COLO-ZSEQC,    "COLOR SEQUENCE
     ' ' 'BDC_OKCODE'  '/00'.

  PERFORM DYNPRO USING:
       'X' 'SAPLCSDI'    '0140',
       ' ' 'BDC_OKCODE'  '=FCNP'.

ENDFORM.                    " CHANGE_BADY_ITEM
*&---------------------------------------------------------------------*
*&      Form  BADY_DEPENDENCY
*&---------------------------------------------------------------------*
FORM BADY_DEPENDENCY USING P_DPID.

  PERFORM DYNPRO USING:
     'X' 'SAPLCSDI'    '0150',
     ' ' 'RC29P-AUSKZ(01)' 'X',    "
     ' ' 'BDC_OKCODE'  '=WIZU',

     'X' 'SAPLCUKD'    '0130',
     ' ' 'BDC_OKCODE'  '=NEWZ',

     'X' 'SAPLCUKD'    '0130',
     ' ' 'RCUKD-KNNAM(02)' P_DPID, "
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCUKD'    '0130',
     ' ' 'BDC_OKCODE'  '=BACK'.

ENDFORM.                    " BADY_DEPENDENCY
*&---------------------------------------------------------------------*
*&      Form  CHANGE_END_ITEM_CREATE
*&---------------------------------------------------------------------*
FORM CHANGE_END_ITEM_CREATE.
  PERFORM DYNPRO USING:
     'X' 'SAPLCSDI'    '0140',
     ' ' 'BDC_OKCODE'  '=FCBU'.
* CALL TRANSACTION
  CALL TRANSACTION 'CS02'  USING IT_BDC
                           OPTIONS FROM WA_OPT
                           MESSAGES INTO IT_MESS.
ENDFORM.                    " CHANGE_END_ITEM_CREATE
*&---------------------------------------------------------------------*
*&      Form  CHANGE_END_ITEM_CREATE1
*&---------------------------------------------------------------------*
FORM CHANGE_END_ITEM_CREATE1.
  PERFORM DYNPRO USING:
     'X' 'SAPLCSDI'    '0150',
     ' ' 'BDC_OKCODE'  '=FCBU'.
* CALL TRANSACTION
  CALL TRANSACTION 'CS02'  USING IT_BDC
                           OPTIONS FROM WA_OPT
                           MESSAGES INTO IT_MESS.
ENDFORM.                    " CHANGE_END_ITEM_CREATE1
*&---------------------------------------------------------------------*
*&      Form  COLOR_PART_CALL_TRANSACTION
*&---------------------------------------------------------------------*
FORM COLOR_PART_CALL_TRANSACTION USING    P_TANEW
                                          P_TAEND
                                 CHANGING P_ZRESULT
                                          P_ZMSG
                                          P_MODE.
  IF P_MODE EQ '1' OR  P_MODE EQ '2'.
    PERFORM CHANGE_END_ITEM_CREATE.
    P_ZRESULT = SY-MSGTY.
    PERFORM RKC_MSG_STRING CHANGING P_ZMSG.
*   MODIFY IT_COLO
    LOOP AT IT_COLO FROM P_TANEW TO P_TAEND.
      IT_COLO-ZRESULT = P_ZRESULT.
      IT_COLO-ZMSG    = P_ZMSG.
      MODIFY IT_COLO INDEX SY-TABIX TRANSPORTING ZRESULT
                                                 ZMSG.
      CLEAR IT_COLO.
    ENDLOOP.
  ENDIF.
  REFRESH: IT_BDC, IT_MESS.
  CLEAR: IT_BDC, IT_MESS.
ENDFORM.                    " COLOR_PART_CALL_TRANSACTION
*&---------------------------------------------------------------------*
*&      Form  COLOR_PART_CALL_TRANSACTION1
*&---------------------------------------------------------------------*
FORM COLOR_PART_CALL_TRANSACTION1 USING    P_TANEW
                                           P_TAEND
                                  CHANGING P_ZRESULT
                                           P_ZMSG
                                           P_MODE.
  IF P_MODE EQ '1' OR  P_MODE EQ '2'.
    PERFORM CHANGE_END_ITEM_CREATE1.
    P_ZRESULT = SY-MSGTY.
    PERFORM RKC_MSG_STRING CHANGING P_ZMSG.
*   MODIFY IT_COLO
    LOOP AT IT_COLO FROM P_TANEW TO P_TAEND.
      IT_COLO-ZRESULT = P_ZRESULT.
      IT_COLO-ZMSG    = P_ZMSG.
      MODIFY IT_COLO INDEX SY-TABIX TRANSPORTING ZRESULT
                                                 ZMSG.
      CLEAR IT_COLO.
    ENDLOOP.
  ENDIF.
  REFRESH: IT_BDC, IT_MESS.
  CLEAR: IT_BDC, IT_MESS.
ENDFORM.                    " COLOR_PART_CALL_TRANSACTION1
*&---------------------------------------------------------------------*
*&      Form  COLOR_PART_ITEM_DELETE
*&---------------------------------------------------------------------*
FORM COLOR_PART_ITEM_DELETE USING    PA_COLO STRUCTURE WA_COLO
                                     P_TABIX
                            CHANGING P_MODE
                                     P_STLKN
                                     P_ZRESULT
                                     P_ZMSG.
  PERFORM READ_MAST_STPO_STLKN USING PA_COLO-MATNR "Material
                                     PA_COLO-WERKS "Plant
                                     PA_COLO-STLAN "BOM usage
                                     PA_COLO-STLAL "Alternative
                                     PA_COLO-POSNR "BOM item
                                     PA_COLO-IDNRK "BOM
                                     PA_COLO-ZSUFF "SUFFIX
                                     PA_COLO-ZSEQU
                            CHANGING P_MODE
                                     P_STLKN.
  IF P_MODE EQ '3'. "DELETE(ITEM DELETE)
    IF NOT P_STLKN IS INITIAL.
      PERFORM BOM_ITEM_DELETE USING PA_COLO-MATNR "Material number
                                    PA_COLO-WERKS "Plant
                                    PA_COLO-STLAN "BOM usage
                                    PA_COLO-STLAL "Alternative BOM
                                    PA_COLO-AENNR "Change number
                                    P_STLKN.      "Item node number

      PERFORM RKC_MSG_STRING CHANGING P_ZMSG.
      P_ZRESULT = SY-MSGTY.
*     MODIFY IT_COLO
      IT_COLO-ZRESULT = P_ZRESULT.
      IT_COLO-ZMSG    = P_ZMSG.
      MODIFY IT_COLO INDEX P_TABIX TRANSPORTING ZRESULT
                                                ZMSG.
    ELSE.
*     MODIFY IT_COLO
      IT_COLO-ZRESULT = 'E'.
      IT_COLO-ZMSG    = 'BOM ITEM to delete does not exist.'.
      MODIFY IT_COLO INDEX P_TABIX TRANSPORTING ZRESULT
                                                ZMSG.
    ENDIF.
  ELSEIF P_MODE EQ '4'.
*   MODIFY IT_COLO
    IT_COLO-ZRESULT = 'E'.
    IT_COLO-ZMSG    = 'BOM ITEM to delete does not exist.'.
    MODIFY IT_COLO INDEX P_TABIX TRANSPORTING ZRESULT
                                              ZMSG.
  ENDIF.

  CLEAR IT_COLO.

  REFRESH: IT_BDC, IT_MESS.
  CLEAR: IT_BDC, IT_MESS.
ENDFORM.                    " COLOR_PART_ITEM_DELETE
*&---------------------------------------------------------------------*
*&      Form  MAST_STPO_CHECK
*&---------------------------------------------------------------------*
FORM MAST_STPO_CHECK USING    PA_NCOL STRUCTURE WA_NCOL
                     CHANGING P_MODE.
  DATA: L_STLNR LIKE MAST-STLNR,
        L_SEQU(04) TYPE N.

  L_SEQU = PA_NCOL-ZSEQU.

  SELECT SINGLE STLNR
       FROM MAST
       INTO L_STLNR
       WHERE MATNR EQ PA_NCOL-MATNR
       AND   WERKS EQ PA_NCOL-WERKS
       AND   STLAN EQ PA_NCOL-STLAN
       AND   STLAL EQ PA_NCOL-STLAL.
  IF SY-SUBRC EQ 0.
    P_MODE = '1'.
    SELECT POSNR
           IDNRK
           SUFF
           SEQU
           STLKN
         FROM STPO
         INTO TABLE IT_STPO
         WHERE STLTY EQ 'M'
         AND   STLNR EQ L_STLNR.
    IF SY-SUBRC EQ 0.
      SORT IT_STPO BY POSNR IDNRK ZSUFF ZSEQU.
      READ TABLE IT_STPO WITH KEY POSNR  = PA_NCOL-POSNR
                                  IDNRK  = PA_NCOL-IDNRK
                                  ZSUFF  = PA_NCOL-ZSUFF
*                                  ZSEQU  = PA_NCOL-ZSEQU
                                  ZSEQU  = L_SEQU
                         BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        P_MODE = '2'.
      ELSE.
        P_MODE = '1'.
      ENDIF.
    ELSE.
      P_MODE = '1'.
    ENDIF.
  ELSE.
*   BOM HEADER CREATED
    P_MODE = '1'.
    PERFORM BOM_HEADER USING PA_NCOL-MATNR  "Material
                             PA_NCOL-WERKS  "Plant
                             PA_NCOL-STLAN  "BOM usage
                             PA_NCOL-STLAL  "Alternat BOM
                             PA_NCOL-BMENG  "Confirmed qua
                             PA_NCOL-STLST. "BOM Status
  ENDIF.
ENDFORM.                    " MAST_STPO_CHECK
*&---------------------------------------------------------------------*
*&      Form  CHECK_IT_STPO
*&---------------------------------------------------------------------*
FORM CHECK_IT_STPO USING    PA_NCOL STRUCTURE WA_NCOL
                   CHANGING P_CHK.
  DATA L_SEQU(04) TYPE N.
  L_SEQU = PA_NCOL-ZSEQU.
  READ TABLE IT_STPO WITH KEY POSNR  = PA_NCOL-POSNR
                              IDNRK  = PA_NCOL-IDNRK
                              ZSUFF  = PA_NCOL-ZSUFF
                              ZSEQU  = L_SEQU
*                              ZSEQU  = PA_NCOL-ZSEQU
                     BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    P_CHK = ' '.
  ELSE.
    P_CHK = 'X'.
  ENDIF.
ENDFORM.                    " CHECK_IT_STPO
*&---------------------------------------------------------------------*
*&      Form  COLOR_PART_HEADER
*&---------------------------------------------------------------------*
FORM COLOR_PART_HEADER USING    PA_COLO STRUCTURE WA_COLO
                       CHANGING P_MODE
                                P_STLKN.

  DATA: L_STLNR LIKE MAST-STLNR,
        L_SEQU(04) TYPE N.

  L_SEQU = PA_COLO-ZSEQU.

  SELECT SINGLE STLNR
       FROM MAST
       INTO L_STLNR
       WHERE MATNR EQ PA_COLO-MATNR
       AND   WERKS EQ PA_COLO-WERKS
       AND   STLAN EQ PA_COLO-STLAN
       AND   STLAL EQ PA_COLO-STLAL.
  IF SY-SUBRC EQ 0.
    P_MODE = '1'.
    SELECT POSNR
           IDNRK
           SUFF
           SEQU
           STLKN
         FROM STPO
         INTO TABLE IT_STPO
         WHERE STLTY EQ 'M'
         AND   STLNR EQ L_STLNR.
    IF SY-SUBRC EQ 0.
      SORT IT_STPO BY POSNR IDNRK ZSUFF ZSEQU.
      READ TABLE IT_STPO WITH KEY POSNR  = PA_COLO-POSNR
                                  IDNRK  = PA_COLO-IDNRK
                                  ZSUFF  = PA_COLO-ZSUFF
                                  ZSEQU  = L_SEQU
*                                  ZSEQU  = PA_COLO-ZSEQU
                         BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        P_MODE = '2'.
        P_STLKN = IT_STPO-STLKN.
      ELSE.
        P_MODE = '1'.
      ENDIF.
    ELSE.
      P_MODE = '1'.
    ENDIF.
  ELSE.
*   BOM HEADER CREATED
    P_MODE = '1'.
    PERFORM BOM_HEADER USING PA_COLO-MATNR  "Material
                             PA_COLO-WERKS  "Plant
                             PA_COLO-STLAN  "BOM usage
                             PA_COLO-STLAL  "Alternat BOM
                             PA_COLO-BMENG  "Confirmed qua
                             PA_COLO-STLST. "BOM Status
  ENDIF.
ENDFORM.                    " COLOR_PART_HEADER
*&---------------------------------------------------------------------*
*&      Form  OBJECT_DEPENDENCY_APPENDING
*&---------------------------------------------------------------------*
FORM OBJECT_DEPENDENCY_APPENDING.
  DATA: L_STLKN TYPE STPO-STLKN,
        L_TABIX TYPE SY-TABIX,
        L_TANEW TYPE SY-TABIX,
        L_TAEND TYPE SY-TABIX,
        L_ZRESULT LIKE IT_EXCL-ZRESULT,
        L_ZMSG LIKE IT_EXCL-ZMSG,
        L_MODE.
  CLEAR: WA_LINE_IDX, WA_ERRO_IDX.
* MATERIAL & OBJECT DEPENDENCY APPENDING
  LOOP AT IT_COLO WHERE UPCT NE '2'
                  AND   ZRESULT NE 'E'.
    L_TABIX = SY-TABIX.
    WA_LINE_IDX = WA_LINE_IDX + 1.

    WA_COLO = IT_COLO.
*     BOM ITEM ADDITION & OBJECT DEPENDENCY CREATE
*       AT NEW ZSEQU
    AT NEW ZSEQC.
      L_MODE = '1'.
      L_TANEW = L_TABIX.
      PERFORM MAST_STPO_STLKN USING WA_COLO
                              CHANGING L_STLKN.
      PERFORM APPEND_OBJECT_DEPENDENCY USING     WA_COLO
                                                 L_STLKN.

    ENDAT.
*   'UPDATE CONTROL MODE' different BDC
    PERFORM BADY_DEPENDENCY USING WA_COLO-DPID.
*       AT END OF ZSEQU
    AT END OF ZSEQC.
      L_TAEND = L_TABIX.
      PERFORM COLOR_PART_CALL_TRANSACTION1 USING    L_TANEW
                                                   L_TAEND
                                          CHANGING L_ZRESULT
                                                   L_ZMSG
                                                   L_MODE.

    ENDAT.
    CLEAR:  IT_COLO.
  ENDLOOP.
* BOM ERROR WRITE.
  REFRESH IT_EXCL. CLEAR IT_EXCL.
* ERROR LINE COUNT
  LOOP AT IT_COLO.
    WA_LINE_IDX = WA_LINE_IDX + 1.
    IF IT_COLO-ZRESULT EQ 'E'.
      WA_ERRO_IDX = WA_ERRO_IDX + 1.
      MOVE-CORRESPONDING IT_COLO TO IT_EXCL.
      APPEND IT_EXCL.
    ENDIF.
    CLEAR: IT_COLO, IT_EXCL.
  ENDLOOP.
*  REFRESH IT_COLO. CLEAR IT_COLO.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE: / TEXT-036,
            WA_LINE_IDX.
  WRITE: / TEXT-037,
            WA_ERRO_IDX.
  FORMAT COLOR OFF.

* Error appears if is more than one case.
  IF WA_ERRO_IDX GE '1'.
    PERFORM ERROR_WRITE TABLES IT_EXCL
                        USING  'E'.
    REFRESH IT_EXCL. CLEAR IT_EXCL.
  ENDIF.
ENDFORM.                    " OBJECT_DEPENDENCY_APPENDING
*&---------------------------------------------------------------------*
*&      Form  MAST_STPO_STLKN
*&---------------------------------------------------------------------*
FORM MAST_STPO_STLKN USING    PA_COLO STRUCTURE WA_COLO
                     CHANGING P_STLKN.
  DATA: L_SEQU TYPE STPO-SEQU.
  L_SEQU = PA_COLO-ZSEQU.
  SELECT SINGLE B~STLKN
       FROM MAST AS A INNER JOIN STPO AS B
                      ON    B~STLTY EQ 'M'
                      AND   A~STLNR EQ B~STLNR
       INTO   P_STLKN
       WHERE A~MATNR EQ PA_COLO-MATNR
       AND   A~WERKS EQ PA_COLO-WERKS
       AND   A~STLAN EQ PA_COLO-STLAN
       AND   A~STLAL EQ PA_COLO-STLAL
       AND   B~POSNR EQ PA_COLO-POSNR
       AND   B~IDNRK EQ PA_COLO-IDNRK
       AND   B~SUFF  EQ PA_COLO-ZSUFF
       AND   B~SEQU  EQ L_SEQU.
ENDFORM.                    " MAST_STPO_STLKN
*&---------------------------------------------------------------------*
*&      Form  MAST_STPO_STLKN_1
*&---------------------------------------------------------------------*
FORM MAST_STPO_STLKN_1 USING    PA_BMCO STRUCTURE WA_BMCO
                       CHANGING P_STLKN.
  DATA: L_SEQU TYPE STPO-SEQU.
*  L_SEQU = PA_BMCO-SEQC.
  L_SEQU = PA_BMCO-SEQU.
  SELECT SINGLE B~STLKN
       FROM MAST AS A INNER JOIN STPO AS B
                      ON    B~STLTY EQ 'M'
                      AND   A~STLNR EQ B~STLNR
       INTO   P_STLKN
       WHERE A~MATNR EQ PA_BMCO-MTNO
       AND   A~WERKS EQ PA_BMCO-PLNT
       AND   A~STLAN EQ PA_BMCO-USAG
       AND   A~STLAL EQ PA_BMCO-ALTN
       AND   B~POSNR EQ PA_BMCO-PREF
       AND   B~IDNRK EQ PA_BMCO-COMP
       AND   B~SUFF  EQ PA_BMCO-SUFF
       AND   B~SEQU  EQ L_SEQU.
ENDFORM.                    " MAST_STPO_STLKN_1
*&---------------------------------------------------------------------*
*&      Form  EXCEL_DATA_BDC_PROCESS
*&---------------------------------------------------------------------*
FORM EXCEL_DATA_BDC_PROCESS.
  PERFORM UPLOAD_PROCESS.
  PERFORM DEPENDENCY_ECM_CHECK CHANGING WA_CHECK.
  IF WA_CHECK NE 'X'.
    PERFORM DATA_PROCESS CHANGING WA_CHECK.
    IF WA_CHECK NE 'X'.
*     COLOR PART & NON COLOR PART PARTITION
      PERFORM COLOR_PART_PARTITION.
*     BOM ITEM CREATE
      PERFORM BDC_PROCESS.
*     BOM EXPLODED.
      REFRESH IT_BOM_EXPLODED. CLEAR IT_BOM_EXPLODED.
      PERFORM BOM_EXPLODED TABLES IT_BOM_EXPLODED.
*     MARA CONFIGURABLE MATERIAL CHECK
      IF NOT IT_BOM_EXPLODED[] IS INITIAL.
        PERFORM MARA_CONFIGURABLE_MATERIAL TABLES IT_BOM_EXPLODED.
        PERFORM MM02_CONFIGURABLE_MATERIAL_BDC.
      ENDIF.
      PERFORM OBJECT_DEPENDENCY_APPENDING.
*      PERFORM JOB_PROCESS.
    ENDIF.
  ENDIF.
ENDFORM.                    " EXCEL_DATA_BDC_PROCESS
*&---------------------------------------------------------------------*
*&      Form  TABLE_SELECTION_DATA_BDC
*&---------------------------------------------------------------------*
FORM TABLE_SELECTION_DATA_BDC.
  CLEAR WA_CHECK.
  PERFORM READ_PROCESS.
  PERFORM DEPENDENCY_CHANGE_NO_CHECK CHANGING WA_CHECK.
  IF WA_CHECK NE 'X'.
*   COLOR PART & NON COLOR PART PARTITION
    PERFORM TABLE_COLOR_PART_PARTITION.
*   BOM ITEM CREATE
    PERFORM TABLE_SELECTION_BDC_PROCESS.
*   BOM EXPLODED.
    REFRESH IT_BOM_EXPLODED. CLEAR IT_BOM_EXPLODED.
    PERFORM TABLE_BOM_EXPLODED TABLES IT_BOM_EXPLODED.
*   MARA CONFIGURABLE MATERIAL CHECK
    IF NOT IT_BOM_EXPLODED[] IS INITIAL.
      PERFORM MARA_CONFIGURABLE_MATERIAL TABLES IT_BOM_EXPLODED.
      PERFORM MM02_CONFIGURABLE_MATERIAL_BDC.
    ENDIF.
*    PERFORM OBJECT_DEPENDENCY_APPENDING.
    PERFORM TABLE_OBJECT_DEPENDENCY.
    PERFORM JOB_PROCESS.
  ENDIF.
  PERFORM BOM_EXPLODED_WRITE.
ENDFORM.                    " TABLE_SELECTION_DATA_BDC
*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
FORM READ_PROCESS.
  SELECT *
       FROM ZTBM_ABXEBMDT
       INTO TABLE IT_BMDT
       WHERE ZEDAT EQ P_ZEDAT
       AND   ZBTIM EQ P_ZBTIM.
*       AND   COMP  EQ '3711238200'.
  IF SY-SUBRC EQ 0.
    DESCRIBE TABLE IT_BMDT LINES WA_LINE_IDX.
    WRITE: / 'BOM SELECTION TOTAL LINES : ' COLOR 4, WA_LINE_IDX.
    IT_BMDT-ZRESULT = ' '.
    IT_BMDT-ZMSG = ' '.
*    LT_BMDT-ZBTIM = SY-UZEIT.
    MODIFY IT_BMDT TRANSPORTING ZRESULT
                                ZMSG
                             WHERE MTNO GE SPACE
                             AND   PLNT GE SPACE
                             AND   USAG GE SPACE
                             AND   ALTN GE SPACE
                             AND   PREF GE SPACE
                             AND   COMP GE SPACE
                             AND   SUFF GE SPACE
                             AND   SEQU GE SPACE
                             AND   SEQC GE SPACE.
  ELSE.
    WRITE: / TEXT-001 COLOR 4.
  ENDIF.
ENDFORM.                    " READ_PROCESS
*&---------------------------------------------------------------------*
*&      Form  DEPENDENCY_CHANGE_NO_CHECK
*&---------------------------------------------------------------------*
FORM DEPENDENCY_CHANGE_NO_CHECK CHANGING PA_CHECK.
  DATA L_TABIX TYPE SY-TABIX.
  DATA: LT_BMDT LIKE IT_BMDT OCCURS 0 WITH HEADER LINE,
        L_ERRO_DEP LIKE WA_ERRO_IDX,
        L_ERRO_ECM LIKE WA_ERRO_IDX,
        L_ERRO_MAT LIKE WA_ERRO_IDX,
        L_KNNAM LIKE CUKB-KNNAM.
  DATA: BEGIN OF LT_AENR OCCURS 0,
          AENNR TYPE AENR-AENNR,
        END OF LT_AENR.
  DATA: BEGIN OF MT_AENR OCCURS 0,
          AENNR TYPE AENR-AENNR,
        END OF MT_AENR.
  DATA: BEGIN OF LT_CHCK OCCURS 0,
          MATNR TYPE MARC-MATNR,
          WERKS TYPE MARC-WERKS,
        END OF LT_CHCK.
  DATA: BEGIN OF LT_MARC OCCURS 0,
          MATNR TYPE MARC-MATNR,
          WERKS TYPE MARC-WERKS,
        END OF LT_MARC.
  CLEAR: WA_ERRO_IDX, WA_LINE_IDX.

* DEPENDENCY CHECK
  LOOP AT IT_BMDT WHERE CLPT EQ 'C'
                  AND   UPCT EQ '1'.
    WA_LINE_IDX = WA_LINE_IDX + 1.
    SELECT SINGLE KNNAM
                FROM CUKB
                INTO L_KNNAM
                WHERE KNNAM EQ IT_BMDT-DPID.
    IF SY-SUBRC NE 0.
      LT_BMDT = IT_BMDT.
      LT_BMDT-ZRESULT = 'L'.
      LT_BMDT-ZMSG    = 'Dependency does not exist'.
      APPEND LT_BMDT.
      L_ERRO_DEP = L_ERRO_DEP + 1.
    ENDIF.
    CLEAR: IT_BMDT, LT_BMDT.
  ENDLOOP.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE: / TEXT-028, WA_LINE_IDX.
  WRITE: / TEXT-029,
            L_ERRO_DEP.
  FORMAT COLOR OFF.

* CHANGE NUMBER CHECK
  DESCRIBE TABLE IT_BMDT LINES WA_LINE_IDX.

  LOOP AT IT_BMDT.
    IT_SUBM-STLAN = IT_BMDT-USAG.
    IT_SUBM-STLAL = IT_BMDT-ALTN.
    IT_SUBM-MATNR = LT_CHCK-MATNR = IT_BMDT-MTNO.
    IT_SUBM-WERKS = LT_CHCK-WERKS = IT_BMDT-PLNT.

    COLLECT IT_SUBM.
    COLLECT LT_CHCK.
    LT_CHCK-MATNR = IT_BMDT-COMP.
    LT_CHCK-WERKS = IT_BMDT-PLNT.
    COLLECT LT_CHCK.

    LT_AENR-AENNR = IT_BMDT-EONO.
    COLLECT LT_AENR.
    CLEAR: IT_BMDT, LT_CHCK, LT_AENR, IT_SUBM.
  ENDLOOP.
  DESCRIBE TABLE LT_AENR LINES WA_LINE_IDX.

  IF NOT LT_AENR[] IS INITIAL.
    SELECT AENNR
         FROM AENR
         INTO TABLE MT_AENR
         FOR ALL ENTRIES IN LT_AENR
         WHERE AENNR EQ LT_AENR-AENNR.
    IF SY-SUBRC EQ 0.
      SORT MT_AENR.
      LOOP AT IT_BMDT.
        READ TABLE MT_AENR WITH KEY AENNR = IT_BMDT-EONO
                           BINARY SEARCH.
        IF SY-SUBRC NE 0.
          L_ERRO_ECM = L_ERRO_ECM + 1.
          LT_BMDT = IT_BMDT.
          LT_BMDT-ZRESULT = 'L'.
          LT_BMDT-ZMSG = 'Change No does not exist'.
          APPEND LT_BMDT.
        ENDIF.
      ENDLOOP.

      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
      WRITE: / TEXT-030, WA_LINE_IDX.
      WRITE: / TEXT-031,
                L_ERRO_ECM.
      FORMAT COLOR OFF.
    ENDIF.
  ENDIF.

* MARC CHECK
  DESCRIBE TABLE LT_CHCK LINES WA_LINE_IDX.
  IF NOT LT_CHCK[] IS INITIAL.
    SELECT MATNR
           WERKS
         FROM MARC
         INTO TABLE LT_MARC
         FOR ALL ENTRIES IN LT_CHCK
         WHERE MATNR EQ LT_CHCK-MATNR
         AND   WERKS EQ LT_CHCK-WERKS.
    IF SY-SUBRC EQ 0.
      SORT LT_MARC BY MATNR WERKS.
      LOOP AT IT_BMDT.
        L_TABIX = SY-TABIX.
*       NEXT MATERIAL CHECK
        READ TABLE LT_MARC WITH KEY MATNR = IT_BMDT-MTNO
                                    WERKS = IT_BMDT-PLNT
                           BINARY SEARCH.
        IF SY-SUBRC NE 0.
          IT_BMDT-ZRESULT = 'L'.
          CONCATENATE 'NENT MATERIAL : ' IT_BMDT-MTNO
                      '  Material number does not exist'
                 INTO IT_BMDT-ZMSG.
          MODIFY IT_BMDT INDEX L_TABIX TRANSPORTING ZRESULT
                                                    ZMSG.
        ENDIF.
*       COMPONENT CHECK
        READ TABLE LT_MARC WITH KEY MATNR = IT_BMDT-COMP
                                    WERKS = IT_BMDT-PLNT
                           BINARY SEARCH.
        IF SY-SUBRC NE 0.
          IT_BMDT-ZRESULT = 'L'.
          IF IT_BMDT-ZMSG IS INITIAL.
            CONCATENATE  'COMPORNENT : ' IT_BMDT-COMP
                         '  Material number does not exist'
                    INTO IT_BMDT-ZMSG.
          ELSE.
            CONCATENATE  IT_BMDT-ZMSG '  COMPORNENT : ' IT_BMDT-COMP
                         '  Material number does not exist'
                    INTO IT_BMDT-ZMSG.
          ENDIF.

          MODIFY IT_BMDT INDEX L_TABIX TRANSPORTING ZRESULT
                                                    ZMSG.
        ENDIF.
      ENDLOOP.
    ELSE.
      IT_BMDT-ZRESULT = 'L'.
      IT_BMDT-ZMSG = ' Material number does not exist'.
      MODIFY IT_BMDT TRANSPORTING ZMSG
                                  ZRESULT
                               WHERE MTNO GE SPACE
                               AND   PLNT GE SPACE.
    ENDIF.
  ENDIF.
  LOOP AT IT_BMDT WHERE ZRESULT EQ 'L'.
    LT_BMDT = IT_BMDT.
    L_ERRO_MAT = L_ERRO_MAT + 1.
    APPEND LT_BMDT.
    CLEAR: LT_BMDT, IT_BMDT.
  ENDLOOP.

  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE: / TEXT-038, WA_LINE_IDX.
  WRITE: / TEXT-006, L_ERRO_MAT.
  FORMAT COLOR OFF.

  IF NOT LT_BMDT[] IS INITIAL.
*   ERROR LIST WRITE
    PA_CHECK = 'X'.
    LT_BMDT-ZBDAT = SY-DATUM.
    LT_BMDT-ZBNAM = SY-UNAME.
*    LT_BMDT-ZBTIM = SY-UZEIT.
    MODIFY LT_BMDT TRANSPORTING ZBDAT
                                ZBTIM
                                ZBNAM
                             WHERE MTNO GE SPACE
                             AND   PLNT GE SPACE
                             AND   USAG GE SPACE
                             AND   ALTN GE SPACE
                             AND   PREF GE SPACE
                             AND   COMP GE SPACE
                             AND   SUFF GE SPACE
                             AND   SEQU GE SPACE
                             AND   SEQC GE SPACE.
    PERFORM ERROR_DATA_WRITE TABLES    LT_BMDT
                             USING     'L'.
    DELETE ADJACENT DUPLICATES FROM LT_BMDT COMPARING
                                    MTNO PLNT USAG ALTN PREF
                                    COMP SUFF SEQU SEQC.
    PERFORM UPDATE_ZTBM_ABXEBMDT TABLES    LT_BMDT.
  ENDIF.
ENDFORM.                    " DEPENDENCY_CHANGE_NO_CHECK
*&---------------------------------------------------------------------*
*&      Form  ERROR_DATA_WRITE
*&---------------------------------------------------------------------*
FORM ERROR_DATA_WRITE TABLES   PT_BMDT STRUCTURE IT_BMDT
                      USING    P_ZRESULT.
  WRITE: /(20)  TEXT-039,
          (10)  TEXT-040,
          (10)  TEXT-041,
          (10)  TEXT-042,
          (10)  TEXT-043,
          (20)  TEXT-044,
          (10)  TEXT-045,
          (10)  TEXT-046,
          (15)  TEXT-047,
          (15)  TEXT-048,
          (20)  TEXT-049,
          (10)  TEXT-050,
          (10)  TEXT-051,
          (10)  TEXT-052,
          (20)  TEXT-053,
          (15)  TEXT-054,
          (10)  TEXT-055,
          (15)  TEXT-056,
          (10)  TEXT-057,
          (10)  TEXT-058,
          (30)  TEXT-059,
          (15)  TEXT-060,
          (20)  TEXT-061,
          (10)  TEXT-062,
          (15)  TEXT-063,
         (220)  TEXT-064.
  LOOP AT PT_BMDT WHERE ZRESULT EQ P_ZRESULT.
    WRITE: /(20) PT_BMDT-MTNO,
            (10) PT_BMDT-PLNT,
            (10) PT_BMDT-USAG,
            (10) PT_BMDT-ALTN,
            (10) PT_BMDT-PREF,
            (20) PT_BMDT-COMP,
            (10) PT_BMDT-SUFF,
            (10) PT_BMDT-SEQU,
            (15) PT_BMDT-SEQC,
            (15) PT_BMDT-EONO,
            (20) PT_BMDT-BQTY UNIT PT_BMDT-HUNT,
            (10) PT_BMDT-HUNT,
            (10) PT_BMDT-STAT,
            (10) PT_BMDT-ITCA,
            (20) PT_BMDT-QNTY UNIT PT_BMDT-UNIT,
            (15) PT_BMDT-STGB,
            (10) PT_BMDT-UNIT,
            (15) PT_BMDT-SPPR,
            (10) PT_BMDT-EITM,
            (10) PT_BMDT-CLPT,
            (30) PT_BMDT-DPID,
            (15) PT_BMDT-UPCT,
            (20) PT_BMDT-UPGN,
            (10) PT_BMDT-ZMODE,
            (15) PT_BMDT-ZRESULT,
            (220) PT_BMDT-ZMSG.
  ENDLOOP.

ENDFORM.                    " ERROR_DATA_WRITE
*&---------------------------------------------------------------------*
*&      Form  UPDATE_ZTBM_ABXEBMDT
*&---------------------------------------------------------------------*
FORM UPDATE_ZTBM_ABXEBMDT TABLES   PT_BMDT STRUCTURE IT_BMDT.
  UPDATE ZTBM_ABXEBMDT FROM TABLE PT_BMDT.
  IF SY-SUBRC EQ 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
ENDFORM.                    " UPDATE_ZTBM_ABXEBMDT
*&---------------------------------------------------------------------*
*&      Form  TABLE_COLOR_PART_PARTITION
*&---------------------------------------------------------------------*
FORM TABLE_COLOR_PART_PARTITION.
  LOOP AT IT_BMDT.
    IF IT_BMDT-CLPT EQ 'C'.
      MOVE-CORRESPONDING IT_BMDT TO IT_BMCO.
      APPEND IT_BMCO. CLEAR IT_BMCO.
    ELSE.
      MOVE-CORRESPONDING IT_BMDT TO IT_BMNC.
      APPEND IT_BMNC. CLEAR IT_BMNC.
    ENDIF.
    CLEAR IT_BMDT.
  ENDLOOP.
  REFRESH IT_BMDT. CLEAR IT_BMDT.
ENDFORM.                    " TABLE_COLOR_PART_PARTITION
*&---------------------------------------------------------------------*
*&      Form  TABLE_SELECTION_BDC_PROCESS
*&---------------------------------------------------------------------*
FORM TABLE_SELECTION_BDC_PROCESS.
* NON COLOR PART BDC
  PERFORM TABLE_NON_COLOR_PART_BDC.
* COLOR PART BDC
  PERFORM TABLE_COLOR_PART_BDC.
ENDFORM.                    " TABLE_SELECTION_BDC_PROCESS
*&---------------------------------------------------------------------*
*&      Form  TABLE_NON_COLOR_PART_BDC
*&---------------------------------------------------------------------*
FORM TABLE_NON_COLOR_PART_BDC.
  DATA: L_CHK(1),
        L_TABIX TYPE SY-TABIX,
        L_TANEW TYPE SY-TABIX,
        L_TAEND TYPE SY-TABIX,
        L_MODE,
        L_STLKN TYPE STPO-STLKN,
        L_ZRESULT LIKE IT_EXCL-ZRESULT,
        L_ZMSG LIKE IT_EXCL-ZMSG.
* SORTING
  SORT IT_BMNC BY MTNO PLNT USAG ALTN PREF
                  COMP SUFF SEQU UPCT EONO.
* BDC PROCESS
  LOOP AT IT_BMNC.
    L_TABIX = SY-TABIX.
    WA_BMNC = IT_BMNC.
    CLEAR: L_STLKN.
*   UPDATE CONTROL TYPE
    IF WA_BMNC-UPCT EQ '2'.
*     BOM ITEM DELETE
      PERFORM NON_COL_MAST_STPO_READ USING    WA_BMNC
                                     CHANGING L_MODE
                                              L_STLKN.
      IF L_MODE EQ '3'. "DELETE(ITEM DELETE)
        IF NOT L_STLKN IS INITIAL.
          PERFORM TABLE_NON_COL_BOM_DELETE USING   WA_BMNC
                                                   L_STLKN. "Item nodeNO

        ENDIF.
        PERFORM RKC_MSG_STRING CHANGING L_ZMSG.
        L_ZRESULT = SY-MSGTY.
*       MODIFY IT_BMNC
        IT_BMNC-ZMODE = 'D'.
        IT_BMNC-ZRESULT = L_ZRESULT.
        IT_BMNC-ZMSG    = L_ZMSG.
        MODIFY IT_BMNC INDEX L_TABIX TRANSPORTING ZMODE
                                                  ZRESULT
                                                  ZMSG.
      ELSEIF L_MODE EQ '4'.
*       MODIFY IT_BMNC
        IT_BMNC-ZMODE = 'D'.
        IT_BMNC-ZRESULT = 'E'.
        IT_BMNC-ZMSG    = 'BOM ITEM to delete does not exist.'.
        MODIFY IT_BMNC INDEX L_TABIX TRANSPORTING ZMODE
                                                  ZRESULT
                                                  ZMSG.
      ENDIF.
      CLEAR IT_BMNC.
      REFRESH: IT_BDC, IT_MESS.
      CLEAR: IT_BDC, IT_MESS.

*   BOM ITEM CREATE
    ELSEIF WA_BMNC-UPCT EQ '1'.
*     AT NEW ALTN
      AT NEW ALTN.
        L_TANEW = L_TABIX.
        CLEAR L_MODE.
        CLEAR IT_STPO. REFRESH IT_STPO.
*       BOM CHECK & BOM HEADER CREATED
        PERFORM TALBE_MAST_STPO_CHECK USING    WA_BMNC
                                      CHANGING L_MODE.
        IF L_MODE EQ '1' OR  L_MODE EQ '2'.
          PERFORM TABLE_NON_COL_CHANGE_BOM USING WA_BMNC.
          L_MODE = '1'.
        ENDIF.
      ENDAT.

*     'UPDATE CONTROL MODE' different BDC
      IF L_MODE EQ '1'.
*       BOM ITEM DATA already CHECK
        CLEAR L_CHK.
        PERFORM TABLE_CHECK_IT_STPO USING    WA_BMNC
                                    CHANGING L_CHK.
        IF L_CHK EQ 'X'.
          PERFORM TABLE_NON_COL_BADY USING WA_BMNC.
        ELSE.
          IT_BMNC-ZMODE   = 'C'.
          IT_BMNC-ZRESULT = 'E'.
          IT_BMNC-ZMSG = 'BOM ITEM DATA already.'.
          MODIFY IT_BMNC INDEX L_TABIX TRANSPORTING ZMODE
                                                    ZRESULT
                                                    ZMSG.
          CLEAR IT_BMNC.
        ENDIF.
      ENDIF.

*     AT END OF ALTN
      AT END OF ALTN.
        L_TAEND = L_TABIX.
        IF L_MODE EQ '1'.
          PERFORM NON_COL_END_ITEM_CREATE.
          L_ZRESULT = SY-MSGTY.
          PERFORM RKC_MSG_STRING CHANGING L_ZMSG.
*         MODIFY IT_BMNC
          LOOP AT IT_BMNC FROM L_TANEW TO L_TAEND.
            IF L_ZRESULT IS INITIAL.
              IT_BMNC-ZRESULT = 'E'.
            ELSE.
              IT_BMNC-ZRESULT = L_ZRESULT.
            ENDIF.
            IT_BMNC-ZMODE   = 'C'.
            IT_BMNC-ZMSG    = L_ZMSG.
            MODIFY IT_BMNC INDEX SY-TABIX TRANSPORTING ZMODE
                                                       ZRESULT
                                                       ZMSG.
            CLEAR IT_BMNC.
          ENDLOOP.
          CLEAR L_ZRESULT.
        ENDIF.

        REFRESH: IT_BDC, IT_MESS.
        CLEAR: IT_BDC, IT_MESS.
      ENDAT.
    ENDIF.
    CLEAR: IT_BMNC, WA_BMNC.
  ENDLOOP.

* BOM ERROR WRITE.
  CLEAR: WA_LINE_IDX, WA_ERRO_IDX.
  REFRESH IT_BMDT. CLEAR IT_BMDT.
* TOTAL LINE COUNT
  DESCRIBE TABLE IT_BMNC LINES WA_LINE_IDX.
* ERROR LINE COUNT
  LOOP AT IT_BMNC.
    MOVE-CORRESPONDING IT_BMNC TO IT_BMDT.
    IT_BMDT-ZBDAT = SY-DATUM.
    IT_BMDT-ZBNAM = SY-UNAME.
    IF IT_BMNC-ZRESULT EQ'E'.
      WA_ERRO_IDX = WA_ERRO_IDX + 1.
    ELSE.
      IT_BMDT-ZBTIM = SY-UZEIT.
    ENDIF.
    APPEND IT_BMDT.
    CLEAR: IT_BMNC, IT_BMDT.
  ENDLOOP.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE: / TEXT-032, WA_LINE_IDX.
  WRITE: / TEXT-033, WA_ERRO_IDX.
  FORMAT COLOR OFF.
  REFRESH IT_BMNC. CLEAR IT_BMNC.

* Error appears if is more than one case.
  IF WA_ERRO_IDX GE '1'.
    PERFORM ERROR_DATA_WRITE TABLES IT_BMDT
                             USING  'E'.
  ENDIF.

  PERFORM UPDATE_ZTBM_ABXEBMDT TABLES   IT_BMDT.

ENDFORM.                    " TABLE_NON_COLOR_PART_BDC
*&---------------------------------------------------------------------*
*&      Form  TABLE_COLOR_PART_BDC
*&---------------------------------------------------------------------*
FORM TABLE_COLOR_PART_BDC.
  DATA: L_TABIX TYPE SY-TABIX,
        L_TANEW TYPE SY-TABIX,
        L_TAEND TYPE SY-TABIX,
        L_MODE,
        L_CHK,
        L_STLKN TYPE STPO-STLKN,
        L_ZRESULT LIKE IT_EXCL-ZRESULT,
        L_ZMSG LIKE IT_EXCL-ZMSG.

* SORTING
  SORT IT_BMCO BY MTNO PLNT USAG ALTN PREF
                  COMP SUFF SEQU UPCT EONO SEQC.
  LOOP AT IT_BMCO.
    L_TABIX = SY-TABIX.
    WA_BMCO = IT_BMCO.
    CASE WA_BMCO-UPCT.
*     BOM ITEM ADDITION & OBJECT DEPENDENCY CREATE
      WHEN '1'.
*       AT NEW SEQU
        AT NEW SEQU.
          L_TANEW = L_TABIX.
          CLEAR L_MODE.
          CLEAR IT_STPO. REFRESH IT_STPO.
          PERFORM TABLE_COLOR_PART_HEADER USING    WA_BMCO
                                          CHANGING L_MODE
                                                   L_STLKN.
          IF L_MODE EQ '1'. "CHANGE(ITEM CREATE)
            PERFORM TABLE_CHANGE_BOM USING    WA_BMCO.
            PERFORM TABLE_CHANGE_BADY USING   WA_BMCO.
          ENDIF.
        ENDAT.

*        IF L_MODE EQ '1'.
*          PERFORM TABLE_COL_CHECK_IT_STPO USING    WA_BMCO
*                                          CHANGING L_CHK.
*          IF L_CHK EQ 'X'.
*            PERFORM TABLE_CHANGE_BADY USING WA_BMCO.
*          ENDIF.
*        ENDIF.

*       AT END OF SEQU
        AT END OF SEQU.

          L_TAEND = L_TABIX.
*          PERFORM TABLE_CHANGE_BADY USING WA_BMCO.
          IF L_MODE EQ '1'.

            PERFORM COLOR_PART_TRANSACTION USING    L_TANEW
                                                    L_TAEND
                                           CHANGING L_ZRESULT
                                                    L_ZMSG
                                                    L_MODE.
          ENDIF.
          REFRESH: IT_BDC, IT_MESS.
          CLEAR: IT_BDC, IT_MESS.
        ENDAT.

      WHEN '2'.
        PERFORM TABLE_COLOR_PART_DELETE USING    WA_BMCO
                                                 L_TABIX
                                        CHANGING L_MODE
                                                 L_STLKN
                                                 L_ZRESULT
                                                 L_ZMSG.
    ENDCASE.
    CLEAR L_CHK.
  ENDLOOP.
* BOM ERROR WRITE.
  CLEAR: WA_LINE_IDX, WA_ERRO_IDX.
* TOTAL LINE COUNT
  DESCRIBE TABLE IT_BMCO LINES WA_LINE_IDX.
* ERROR LINE COUNT
  REFRESH: IT_BMDT. CLEAR: IT_BMDT.
  LOOP AT IT_BMCO.
    L_TABIX = SY-TABIX.
    MOVE-CORRESPONDING IT_BMCO TO IT_BMDT.
    IT_BMDT-ZBDAT = SY-DATUM.
    IT_BMDT-ZBNAM = SY-UNAME.
    IF IT_BMCO-ZRESULT EQ'E'.
      WA_ERRO_IDX = WA_ERRO_IDX + 1.
      DELETE IT_BMCO INDEX L_TABIX.
    ELSE.
      IT_BMDT-ZBTIM = SY-UZEIT.
    ENDIF.
    APPEND IT_BMDT.
    CLEAR: IT_BMCO, IT_BMDT.
  ENDLOOP.
*  REFRESH IT_BMCO. CLEAR IT_BMCO.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE: / TEXT-034, WA_LINE_IDX.
  WRITE: / TEXT-035, WA_ERRO_IDX.
  FORMAT COLOR OFF.

ENDFORM.                    " TABLE_COLOR_PART_BDC
*&---------------------------------------------------------------------*
*&      Form  TALBE_MAST_STPO_CHECK
*&---------------------------------------------------------------------*
FORM TALBE_MAST_STPO_CHECK USING    PA_BMNC STRUCTURE WA_BMNC
                           CHANGING P_MODE.
  DATA: L_STLNR LIKE MAST-STLNR,
        L_BQTY(20),
        L_SEQU  LIKE WA_BMNC-SEQU.
  L_SEQU = PA_BMNC-SEQU.
  SELECT SINGLE STLNR
       FROM MAST
       INTO L_STLNR
       WHERE MATNR EQ PA_BMNC-MTNO
       AND   WERKS EQ PA_BMNC-PLNT
       AND   STLAN EQ PA_BMNC-USAG
       AND   STLAL EQ PA_BMNC-ALTN.
  IF SY-SUBRC EQ 0.
    P_MODE = '1'.
    SELECT POSNR
           IDNRK
           SUFF
           SEQU
           STLKN
         FROM STPO
         INTO TABLE IT_STPO
         WHERE STLTY EQ 'M'
         AND   STLNR EQ L_STLNR.
    IF SY-SUBRC EQ 0.
      SORT IT_STPO BY POSNR IDNRK ZSUFF ZSEQU.
      READ TABLE IT_STPO WITH KEY POSNR  = PA_BMNC-PREF
                                  IDNRK  = PA_BMNC-COMP
                                  ZSUFF  = PA_BMNC-SUFF
                                  ZSEQU  = L_SEQU
                         BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        P_MODE = '2'.
      ELSE.
        P_MODE = '1'.
      ENDIF.
    ELSE.
      P_MODE = '1'.
    ENDIF.
  ELSE.
*   BOM HEADER CREATED
    WRITE: PA_BMNC-BQTY TO L_BQTY UNIT PA_BMNC-HUNT LEFT-JUSTIFIED.
    P_MODE = '1'.
    PERFORM BOM_HEADER USING PA_BMNC-MTNO   "Material
                             PA_BMNC-PLNT   "Plant
                             PA_BMNC-USAG   "BOM usage
                             PA_BMNC-ALTN   "Alternat BOM
                             L_BQTY         "Confirmed qua
                             PA_BMNC-STAT . "BOM Status
  ENDIF.
ENDFORM.                    " TALBE_MAST_STPO_CHECK
*&---------------------------------------------------------------------*
*&      Form  TABLE_NON_COL_CHANGE_BOM
*&---------------------------------------------------------------------*
FORM TABLE_NON_COL_CHANGE_BOM USING    PA_BMNC STRUCTURE WA_BMNC.
  PERFORM DYNPRO USING:
     'X' 'SAPLCSDI'    '0100',
     ' ' 'RC29N-MATNR' PA_BMNC-MTNO,     "NEXT MATERIAL
     ' ' 'RC29N-WERKS' PA_BMNC-PLNT,    "PLANT
     ' ' 'RC29N-STLAN' PA_BMNC-USAG,    "BOM usage
     ' ' 'RC29N-STLAL' PA_BMNC-ALTN,    "ALT BOM
     ' ' 'RC29N-AENNR' PA_BMNC-EONO,    "Change number
     ' ' 'BDC_OKCODE'  '=FCPU',

     'X' 'SAPLCSDI'    '0150',
     ' ' 'BDC_OKCODE'  '=FCNP'.
ENDFORM.                    " TABLE_NON_COL_CHANGE_BOM
*&---------------------------------------------------------------------*
*&      Form  TABLE_CHECK_IT_STPO
*&---------------------------------------------------------------------*
FORM TABLE_CHECK_IT_STPO USING    PA_BMNC STRUCTURE WA_BMNC
                         CHANGING P_CHK.
  READ TABLE IT_STPO WITH KEY POSNR  = PA_BMNC-PREF
                              IDNRK  = PA_BMNC-COMP
                              ZSUFF  = PA_BMNC-SUFF
                              ZSEQU  = PA_BMNC-SEQU
                     BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    P_CHK = ' '.
  ELSE.
    P_CHK = 'X'.
  ENDIF.
ENDFORM.                    " TABLE_CHECK_IT_STPO
*&---------------------------------------------------------------------*
*&      Form  TABLE_NON_COL_BADY
*&---------------------------------------------------------------------*
FORM TABLE_NON_COL_BADY USING    PA_BMNC STRUCTURE WA_BMNC.
  DATA: L_DBCNT TYPE SY-DBCNT,
        L_QNTY(20).
  WRITE: PA_BMNC-QNTY TO L_QNTY UNIT PA_BMNC-UNIT LEFT-JUSTIFIED.
  PERFORM DYNPRO USING:
     'X' 'SAPLCSDI'    '0140',
     ' ' 'RC29P-AUSKZ(02)' 'X'   ,    "CHECK
     ' ' 'RC29P-POSNR(02)' PA_BMNC-PREF,    "BOM item number
     ' ' 'RC29P-IDNRK(02)' PA_BMNC-COMP,    "BOM compenent
     ' ' 'RC29P-MENGE(02)' L_QNTY,          "Compenent quantity
     ' ' 'RC29P-POSTP(02)' PA_BMNC-ITCA,    "Item category
*     ' ' 'RC29P-SORTF(02)' PA_BMNC-USR01,   "Sortstring
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCSDI'    '0130',
     ' ' 'RC29P-ITSOB' PA_BMNC-SPPR,    "procurement type
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCSDI'    '0131',
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCSDI'    '0138',
     ' ' 'ZEITM'       PA_BMNC-EITM,    "END ITEM TYPE
     ' ' 'ZSTGB'       PA_BMNC-STGB,    "STRUCTURE TYPE
     ' ' 'ZSUFF'       PA_BMNC-SUFF,    "SUFFIX NO
     ' ' 'ZSEQU'       PA_BMNC-SEQU,    "SEQUENCE NO
     ' ' 'ZUPGN'       PA_BMNC-UPGN,    "UPG
*     ' ' 'ZINFO'       PA_BMNC-SEQC,    "COLOR SEQUENCE
     ' ' 'BDC_OKCODE'  '/00',
     'X' 'SAPLCSDI'    '0140',
     ' ' 'BDC_OKCODE'  '=FCNP'.

ENDFORM.                    " TABLE_NON_COL_BADY
*&---------------------------------------------------------------------*
*&      Form  TABLE_COLOR_PART_HEADER
*&---------------------------------------------------------------------*
FORM TABLE_COLOR_PART_HEADER USING    PA_BMCO STRUCTURE WA_BMCO
                             CHANGING P_MODE
                                      P_STLKN.
  DATA: L_STLNR LIKE MAST-STLNR,
        L_BQTY(20),
        L_SEQU(04) TYPE N.
  L_SEQU = PA_BMCO-SEQU.

  SELECT SINGLE STLNR
       FROM MAST
       INTO L_STLNR
       WHERE MATNR EQ PA_BMCO-MTNO
       AND   WERKS EQ PA_BMCO-PLNT
       AND   STLAN EQ PA_BMCO-USAG
       AND   STLAL EQ PA_BMCO-ALTN.
  IF SY-SUBRC EQ 0.
    P_MODE = '1'.
    SELECT POSNR
           IDNRK
           SUFF
           SEQU
           STLKN
*           ZINFO
         FROM STPO
         INTO TABLE IT_STPO
         WHERE STLTY EQ 'M'
         AND   STLNR EQ L_STLNR.
    IF SY-SUBRC EQ 0.
      SORT IT_STPO BY POSNR IDNRK ZSUFF ZSEQU.
      READ TABLE IT_STPO WITH KEY POSNR  = PA_BMCO-PREF
                                  IDNRK  = PA_BMCO-COMP
                                  ZSUFF  = PA_BMCO-SUFF
                                  ZSEQU  = L_SEQU
*                                  ZINFO  = PA_BMCO-ZINFO
                         BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        P_MODE = '2'.
        P_STLKN = IT_STPO-STLKN.
      ELSE.
        P_MODE = '1'.
      ENDIF.
    ELSE.
      P_MODE = '1'.
    ENDIF.
  ELSE.
*   BOM HEADER CREATED
    WRITE: PA_BMCO-BQTY TO L_BQTY UNIT PA_BMCO-HUNT LEFT-JUSTIFIED.
    P_MODE = '1'.
    PERFORM BOM_HEADER USING PA_BMCO-MTNO  "Material
                             PA_BMCO-PLNT  "Plant
                             PA_BMCO-USAG  "BOM usage
                             PA_BMCO-ALTN  "Alternat BOM
                             L_BQTY        "Confirmed qua
                             PA_BMCO-STAT. "BOM Status
  ENDIF.
ENDFORM.                    " TABLE_COLOR_PART_HEADER
*&---------------------------------------------------------------------*
*&      Form  TABLE_CHANGE_BOM
*&---------------------------------------------------------------------*
FORM TABLE_CHANGE_BOM USING    PA_BMCO STRUCTURE WA_BMCO.

  PERFORM DYNPRO USING:
     'X' 'SAPLCSDI'    '0100',
     ' ' 'RC29N-MATNR' PA_BMCO-MTNO,    "NEXT MATERIAL
     ' ' 'RC29N-WERKS' PA_BMCO-PLNT,    "PLANT
     ' ' 'RC29N-STLAN' PA_BMCO-USAG,    "BOM usage
     ' ' 'RC29N-STLAL' PA_BMCO-ALTN,    "ALT BOM
     ' ' 'RC29N-AENNR' PA_BMCO-EONO,    "Change number
     ' ' 'BDC_OKCODE'  '=FCPU',

     'X' 'SAPLCSDI'    '0150',
     ' ' 'BDC_OKCODE'  '=FCNP'.
ENDFORM.                    " TABLE_CHANGE_BOM
*&---------------------------------------------------------------------*
*&      Form  TABLE_CHANGE_BADY
*&---------------------------------------------------------------------*
FORM TABLE_CHANGE_BADY USING    PA_BMCO STRUCTURE WA_BMCO.
  DATA L_QNTY(20).
  WRITE: PA_BMCO-QNTY TO L_QNTY UNIT PA_BMCO-UNIT LEFT-JUSTIFIED.
  PERFORM DYNPRO USING:
     'X' 'SAPLCSDI'    '0140',
     ' ' 'RC29P-AUSKZ(02)' 'X'   ,    "CHECK
     ' ' 'RC29P-POSNR(02)' PA_BMCO-PREF,    "BOM item number
     ' ' 'RC29P-IDNRK(02)' PA_BMCO-COMP,    "BOM compenent
     ' ' 'RC29P-MENGE(02)' L_QNTY,          "Compenent quantity
     ' ' 'RC29P-POSTP(02)' PA_BMCO-ITCA,    "Item category
*     ' ' 'RC29P-SORTF(02)' PA_BMCO-USR01,   "Sortstring
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCSDI'    '0130',
     ' ' 'RC29P-ITSOB' PA_BMCO-SPPR,    "procurement type
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCSDI'    '0131',
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCSDI'    '0138',
     ' ' 'ZEITM'       PA_BMCO-EITM,    "END ITEM TYPE
     ' ' 'ZSTGB'       PA_BMCO-STGB,    "STRUCTURE TYPE
     ' ' 'ZSUFF'       PA_BMCO-SUFF,    "SUFFIX NO
     ' ' 'ZSEQU'       PA_BMCO-SEQU,    "SEQUENCE NO
     ' ' 'ZUPGN'       PA_BMCO-UPGN,    "UPG
     ' ' 'ZINFO'       PA_BMCO-SEQC,    "COLOR SEQUENCE
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCSDI'    '0140',
     ' ' 'BDC_OKCODE'  '=FCNP'.
ENDFORM.                    " TABLE_CHANGE_BADY
*&---------------------------------------------------------------------*
*&      Form  TABLE_COL_CHECK_IT_STPO
*&---------------------------------------------------------------------*
FORM TABLE_COL_CHECK_IT_STPO USING    PA_BMCO STRUCTURE WA_BMCO
                             CHANGING P_CHK.
  DATA L_SEQU(04) TYPE N.
  L_SEQU = PA_BMCO-SEQU.
  READ TABLE IT_STPO WITH KEY POSNR  = PA_BMCO-PREF
                              IDNRK  = PA_BMCO-COMP
                              ZSUFF  = PA_BMCO-SUFF
*                              ZSEQU  = PA_BMCO-SEQU
                              ZSEQU  = L_SEQU
                     BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    P_CHK = ' '.
  ELSE.
    P_CHK = 'X'.
  ENDIF.
ENDFORM.                    " TABLE_COL_CHECK_IT_STPO
*&---------------------------------------------------------------------*
*&      Form  COLOR_PART_TRANSACTION
*&---------------------------------------------------------------------*
FORM COLOR_PART_TRANSACTION USING    P_TANEW
                                     P_TAEND
                            CHANGING P_ZRESULT
                                     P_ZMSG
                                     P_MODE.
  DATA L_TABIX TYPE SY-TABIX.
  IF P_MODE EQ '1' OR  P_MODE EQ '2'.
    PERFORM CHANGE_END_ITEM_CREATE.
    P_ZRESULT = SY-MSGTY.
    PERFORM RKC_MSG_STRING CHANGING P_ZMSG.
*   MODIFY IT_BMCO
    LOOP AT IT_BMCO FROM P_TANEW TO P_TAEND.
      L_TABIX = SY-TABIX.
      IF P_ZRESULT IS INITIAL.
        IT_BMCO-ZRESULT = 'E'.
        IT_BMCO-ZMODE   = 'C'.
        IT_BMCO-ZMSG    = P_ZMSG.
      ELSE.
        IT_BMCO-ZRESULT = P_ZRESULT.
        IT_BMCO-ZMODE   = 'C'.
        IT_BMCO-ZMSG    = P_ZMSG.
      ENDIF.
      MODIFY IT_BMCO INDEX L_TABIX TRANSPORTING ZMODE
                                                ZRESULT
                                                ZMSG.
      CLEAR IT_BMCO.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " COLOR_PART_TRANSACTION
*&---------------------------------------------------------------------*
*&      Form  COLOR_PART_TRANSACTION1
*&---------------------------------------------------------------------*
FORM COLOR_PART_TRANSACTION1 USING    P_TANEW
                                      P_TAEND
                             CHANGING P_ZRESULT
                                      P_ZMSG
                                      P_MODE.
  DATA L_TABIX TYPE SY-TABIX.
  IF P_MODE EQ '1' OR  P_MODE EQ '2'.
    PERFORM CHANGE_END_ITEM_CREATE1.
    P_ZRESULT = SY-MSGTY.
    PERFORM RKC_MSG_STRING CHANGING P_ZMSG.
*   MODIFY IT_BMCO
    LOOP AT IT_BMCO FROM P_TANEW TO P_TAEND.
      L_TABIX = SY-TABIX.
      IF P_ZRESULT IS INITIAL.
        IT_BMCO-ZRESULT = 'E'.
        IT_BMCO-ZMODE   = 'C'.
        IT_BMCO-ZMSG    = P_ZMSG.
      ELSE.
        IT_BMCO-ZRESULT = P_ZRESULT.
        IT_BMCO-ZMODE   = 'C'.
        IT_BMCO-ZMSG    = P_ZMSG.
      ENDIF.
      MODIFY IT_BMCO INDEX L_TABIX TRANSPORTING ZMODE
                                                ZRESULT
                                                ZMSG.
      CLEAR IT_BMCO.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " COLOR_PART_TRANSACTION1
*&---------------------------------------------------------------------*
*&      Form  TABLE_COLOR_PART_DELETE
*&---------------------------------------------------------------------*
FORM TABLE_COLOR_PART_DELETE USING    PA_BMCO STRUCTURE WA_BMCO
                                      P_TABIX
                             CHANGING P_MODE
                                      P_STLKN
                                      P_ZRESULT
                                      P_ZMSG.
  PERFORM READ_MAST_STPO_STLKN USING PA_BMCO-MTNO  "Material
                                     PA_BMCO-PLNT  "Plant
                                     PA_BMCO-USAG  "BOM usage
                                     PA_BMCO-ALTN  "Alternative
                                     PA_BMCO-PREF  "BOM item
                                     PA_BMCO-COMP  "BOM
                                     PA_BMCO-SUFF  "SUFFIX
                                     PA_BMCO-SEQU
                            CHANGING P_MODE
                                     P_STLKN.
  IF P_MODE EQ '3'. "DELETE(ITEM DELETE)
    IF NOT P_STLKN IS INITIAL.
      PERFORM BOM_ITEM_DELETE USING PA_BMCO-MTNO  "Material number
                                    PA_BMCO-PLNT  "Plant
                                    PA_BMCO-USAG  "BOM usage
                                    PA_BMCO-ALTN  "Alternative BOM
                                    PA_BMCO-EONO  "Change number
                                    P_STLKN.      "Item node number

      PERFORM RKC_MSG_STRING CHANGING P_ZMSG.
      P_ZRESULT = SY-MSGTY.
*     MODIFY IT_COLO
      IT_BMCO-ZMODE   = 'D'.
      IT_BMCO-ZBDAT   = SY-DATUM.
      IT_BMCO-ZBTIM   = SY-UZEIT.
      IT_BMCO-ZBNAM   = SY-UNAME.
      IT_BMCO-ZRESULT = P_ZRESULT.
      IT_BMCO-ZMSG    = P_ZMSG.
      MODIFY IT_BMCO INDEX P_TABIX TRANSPORTING ZMODE
                                                ZRESULT
                                                ZMSG
                                                ZBDAT
                                                ZBTIM
                                                ZBNAM.
    ELSE.
*     MODIFY IT_BMCO
      IT_BMCO-ZMODE   = 'D'.
      IT_BMCO-ZBDAT   = SY-DATUM.
      IT_BMCO-ZBNAM   = SY-UNAME.
      IT_BMCO-ZRESULT = 'E'.
      IT_BMCO-ZMSG    = 'BOM ITEM to delete does not exist.'.
      MODIFY IT_BMCO INDEX P_TABIX TRANSPORTING ZMODE
                                                ZRESULT
                                                ZMSG
                                                ZBDAT
                                                ZBNAM.
    ENDIF.
  ELSEIF P_MODE EQ '4'.
*   MODIFY IT_BMCO
    IT_BMCO-ZMODE   = 'D'.
    IT_BMCO-ZBDAT   = SY-DATUM.
    IT_BMCO-ZBNAM   = SY-UNAME.
    IT_BMCO-ZRESULT = 'E'.
    IT_BMCO-ZMSG    = 'BOM ITEM to delete does not exist.'.
    MODIFY IT_BMCO INDEX P_TABIX TRANSPORTING ZMODE
                                              ZRESULT
                                              ZMSG
                                              ZBDAT
                                              ZBNAM.
  ENDIF.

  CLEAR IT_BMCO.

  REFRESH: IT_BDC, IT_MESS.
  CLEAR: IT_BDC, IT_MESS.
ENDFORM.                    " TABLE_COLOR_PART_DELETE
*&---------------------------------------------------------------------*
*&      Form  TABLE_BOM_EXPLODED
*&---------------------------------------------------------------------*
FORM TABLE_BOM_EXPLODED TABLES P_BOM_EXPLODED STRUCTURE IT_BOM_EXPLODED.
  DATA: BEGIN OF LT_MARC OCCURS 0,
          MATNR LIKE MARC-MATNR,  "MATERIAL
          WERKS LIKE MARC-WERKS,  "PLANT
          MTART LIKE MARA-MTART,  "MATERIAL TYPE
        END   OF LT_MARC.
  DATA L_TABIX TYPE SY-TABIX.
* MATERIAL & COMPENENT COLLECT
  LOOP AT IT_BMCO WHERE UPCT NE '2'
                  AND   ZRESULT NE 'E'.
    LT_MARC-MATNR = IT_BMCO-MTNO.
    LT_MARC-WERKS = IT_BMCO-PLNT.
    COLLECT LT_MARC.
    CLEAR: LT_MARC.
    LT_MARC-MATNR = IT_BMCO-COMP.
    LT_MARC-WERKS = IT_BMCO-PLNT.
    COLLECT LT_MARC.
    CLEAR: LT_MARC, IT_BMCO.
  ENDLOOP.
  IF NOT LT_MARC[] IS INITIAL.
*   SELECTION MARA--->MODIFY LT_MARC-MTART(MATERIAL TYPE)
*                            LT_MARC-KZKFG(Configurable Material)
    PERFORM READ_MARA TABLES LT_MARC.

    LOOP AT LT_MARC WHERE MTART EQ 'FERT'.
      L_TABIX = SY-TABIX.
      P_BOM_EXPLODED-MATNR = LT_MARC-MATNR.
      P_BOM_EXPLODED-WERKS = LT_MARC-WERKS.
      APPEND P_BOM_EXPLODED.
      DELETE LT_MARC INDEX L_TABIX.
      CLEAR: P_BOM_EXPLODED, LT_MARC.
    ENDLOOP.

    LOOP AT LT_MARC.
      SORT P_BOM_EXPLODED.
      READ TABLE P_BOM_EXPLODED WITH KEY MATNR = LT_MARC-MATNR
                                         WERKS = LT_MARC-WERKS
                                BINARY SEARCH.
      IF SY-SUBRC NE 0.
*       BOM EXPLODED.
        PERFORM READ_BOM TABLES P_BOM_EXPLODED
                        USING LT_MARC-MATNR
                              LT_MARC-WERKS
                              SY-DATUM   "IDNRK WERKS DATUV
                        CHANGING WA_LAST.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " TABLE_BOM_EXPLODED
*&---------------------------------------------------------------------*
*&      Form  TABLE_OBJECT_DEPENDENCY
*&---------------------------------------------------------------------*
FORM TABLE_OBJECT_DEPENDENCY.
  DATA: L_STLKN TYPE STPO-STLKN,
        L_TABIX TYPE SY-TABIX,
        L_TANEW TYPE SY-TABIX,
        L_TAEND TYPE SY-TABIX,
        L_ZRESULT LIKE IT_EXCL-ZRESULT,
        L_ZMSG LIKE IT_EXCL-ZMSG,
        L_MODE.
  CLEAR: WA_LINE_IDX, WA_ERRO_IDX, WA_BMCO.
* MATERIAL & OBJECT DEPENDENCY APPENDING
  LOOP AT IT_BMCO WHERE UPCT EQ '2'
                  OR   ZRESULT EQ 'E'.
    L_TABIX = SY-TABIX.
    MOVE-CORRESPONDING IT_BMCO TO IT_BMDT.
    APPEND IT_BMDT.
    DELETE IT_BMCO INDEX L_TABIX.
    CLEAR: IT_BMDT, IT_BMCO.
  ENDLOOP.
  CLEAR: L_TABIX.
  LOOP AT IT_BMCO.
    L_TABIX = SY-TABIX.
    WA_LINE_IDX = WA_LINE_IDX + 1.

    WA_BMCO = IT_BMCO.
*     BOM ITEM ADDITION & OBJECT DEPENDENCY CREATE
*   AT NEW SEQU
    AT NEW SEQU.
      L_MODE = '1'.
      L_TANEW = L_TABIX.

      PERFORM MAST_STPO_STLKN_1 USING WA_BMCO
                                CHANGING L_STLKN.
      PERFORM TABLE_APPEND_DEPENDENCY USING     WA_BMCO
                                                L_STLKN.

    ENDAT.
*   'UPDATE CONTROL MODE' different BDC
    PERFORM BADY_DEPENDENCY USING WA_BMCO-DPID.
*   AT END OF ZSEQU
    AT END OF SEQU.
      L_TAEND = L_TABIX.
      PERFORM COLOR_PART_TRANSACTION1 USING    L_TANEW
                                               L_TAEND
                                      CHANGING L_ZRESULT
                                               L_ZMSG
                                               L_MODE.

      REFRESH: IT_BDC, IT_MESS.
      CLEAR: IT_BDC, IT_MESS.
    ENDAT.
    CLEAR:  IT_BMCO.
  ENDLOOP.
* ERROR LINE COUNT
  DESCRIBE TABLE IT_BMCO LINES WA_LINE_IDX.
* ERROR LINE COUNT
  LOOP AT IT_BMCO.
    L_TABIX = SY-TABIX.
    MOVE-CORRESPONDING IT_BMCO TO IT_BMDT.
    IT_BMDT-ZBDAT = SY-DATUM.
    IT_BMDT-ZBNAM   = SY-UNAME.

    IF IT_BMCO-ZRESULT EQ'E'.
      WA_ERRO_IDX = WA_ERRO_IDX + 1.
*      DELETE IT_BMCO INDEX L_TABIX.
    ELSE.
      IT_BMDT-ZBTIM = SY-UZEIT.
    ENDIF.

    APPEND IT_BMDT.
    CLEAR: IT_BMCO, IT_BMDT.
  ENDLOOP.

*  REFRESH IT_BMCO. CLEAR IT_BMCO.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE: / TEXT-036,
            WA_LINE_IDX.
  WRITE: / TEXT-037,
            WA_ERRO_IDX.
  FORMAT COLOR OFF.

* Error appears if is more than one case.
  IF WA_ERRO_IDX GE '1'.
    PERFORM ERROR_DATA_WRITE TABLES IT_BMDT
                             USING  'E'.

  ENDIF.

  PERFORM UPDATE_ZTBM_ABXEBMDT TABLES   IT_BMDT.

ENDFORM.                    " TABLE_OBJECT_DEPENDENCY
*&---------------------------------------------------------------------*
*&      Form  TABLE_APPEND_DEPENDENCY
*&---------------------------------------------------------------------*
FORM TABLE_APPEND_DEPENDENCY USING    PA_BMCO STRUCTURE WA_BMCO
                                      P_STLKN.
  PERFORM DYNPRO USING:
     'X' 'SAPLCSDI'    '0100',
     ' ' 'RC29N-MATNR' PA_BMCO-MTNO,    "NEXT MATERIAL
     ' ' 'RC29N-WERKS' PA_BMCO-PLNT,    "PLANT
     ' ' 'RC29N-STLAN' PA_BMCO-USAG,    "BOM usage
     ' ' 'RC29N-STLAL' PA_BMCO-ALTN,    "ALT BOM
     ' ' 'RC29N-AENNR' PA_BMCO-EONO,    "Change number
     ' ' 'BDC_OKCODE'  '=FCPU',

     'X' 'SAPLCSDI'    '0150',
     ' ' 'BDC_OKCODE'  '=SETP',

     'X' 'SAPLCSDI'    '0708',
     ' ' 'RC29P-SELPI' P_STLKN,    "
     ' ' 'BDC_OKCODE'  '=CLWI'.
ENDFORM.                    " TABLE_APPEND_DEPENDENCY
*&---------------------------------------------------------------------*
*&      Form  BOM_EXPLODED_WRITE
*&---------------------------------------------------------------------*
FORM BOM_EXPLODED_WRITE.
  IF NOT IT_EXP_MESS[] IS INITIAL.
    CLEAR: WA_LINE_IDX, WA_ERRO_IDX.
    DESCRIBE TABLE IT_EXP_MESS LINES WA_LINE_IDX.
    FORMAT COLOR COL_HEADING INTENSIFIED OFF.
    WRITE: / TEXT-065,  WA_LINE_IDX.
    FORMAT COLOR OFF.
    WRITE: /(20) TEXT-039,
            (10) TEXT-040,
            (100) TEXT-064.
    LOOP AT IT_EXP_MESS.
      WRITE: /(20) IT_EXP_MESS-MATNR,
              (10) IT_EXP_MESS-WERKS,
              (100) IT_EXP_MESS-MSG.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " BOM_EXPLODED_WRITE
*&---------------------------------------------------------------------*
*&      Form  JOB_PROCESS
*&---------------------------------------------------------------------*
FORM JOB_PROCESS.
*  DATA L_SUBM VALUE 'X'.
*  EXPORT: IT_SUBM TO MEMORY ID 'IT_SUBM'.
*  SUBMIT ZEPP308U_SUB_BOM_FSC_ASSIG
*                WITH P_SUBM EQ L_SUBM
*                AND RETURN  .
  PERFORM JOB_EXECUTION USING TEXT-066
                              TEXT-067.
  PERFORM JOB_EXECUTION USING TEXT-068
                              TEXT-069.
*  PERFORM JOB_EXECUTION USING 'ZEPP320U_PLANNING_TABLE'
*                              '320'.
*  PERFORM JOB_EXECUTION USING 'ZEPP323U_PLANNING_PROFILE'
*                              '322'.
*  PERFORM JOB_EXECUTION USING 'ZEPP323U_PLANNING_PROFILE'
*                              '323'.
ENDFORM.                    " JOB_PROCESS
*&---------------------------------------------------------------------*
*&      Form  CALL_JOB_OPEN
*&---------------------------------------------------------------------*
FORM CALL_JOB_OPEN USING P_JOBNAME P_JOBCOUNT.
  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
*      DELANFREP              = ' '
*      JOBGROUP               = ' '
      JOBNAME                = P_JOBNAME
*      SDLSTRTDT              = NO_DATE
*      SDLSTRTTM              = NO_TIME
    IMPORTING
      JOBCOUNT               = P_JOBCOUNT
    EXCEPTIONS
      CANT_CREATE_JOB        = 1
      INVALID_JOB_DATA       = 2
      JOBNAME_MISSING        = 3
      OTHERS                 = 4.
*  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
ENDFORM.                    " CALL_JOB_OPEN
*&---------------------------------------------------------------------*
*&      Form  CALL_JOB_SUBMIT
*&---------------------------------------------------------------------*
FORM CALL_JOB_SUBMIT USING P_JOBNAME
                           P_REPORT
                           P_JOBCOUNT.

  CALL FUNCTION 'JOB_SUBMIT'
    EXPORTING
*      ARCPARAMS                         =
      AUTHCKNAM                         = SY-UNAME
*      COMMANDNAME                       = ' '
*      OPERATINGSYSTEM                   = ' '
*      EXTPGM_NAME                       = ' '
*      EXTPGM_PARAM                      = ' '
*      EXTPGM_SET_TRACE_ON               = ' '
*      EXTPGM_STDERR_IN_JOBLOG           = 'X'
*      EXTPGM_STDOUT_IN_JOBLOG           = 'X'
*      EXTPGM_SYSTEM                     = ' '
*      EXTPGM_RFCDEST                    = ' '
*      EXTPGM_WAIT_FOR_TERMINATION       = 'X'
      JOBCOUNT                          = P_JOBCOUNT
      JOBNAME                           = P_JOBNAME
*      LANGUAGE                          = SY-LANGU
*      PRIPARAMS                         = ' '
      REPORT                            = P_REPORT
*      VARIANT                           = ' '
*    IMPORTING
*      STEP_NUMBER                       =
      EXCEPTIONS
      BAD_PRIPARAMS                     = 1
      BAD_XPGFLAGS                      = 2
      INVALID_JOBDATA                   = 3
      JOBNAME_MISSING                   = 4
      JOB_NOTEX                         = 5
      JOB_SUBMIT_FAILED                 = 6
      LOCK_FAILED                       = 7
      PROGRAM_MISSING                   = 8
      PROG_ABAP_AND_EXTPG_SET           = 9
      OTHERS                            = 10.

*  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
ENDFORM.                    " CALL_JOB_SUBMIT
*&---------------------------------------------------------------------*
*&      Form  CALL_JOB_CLOSE
*&---------------------------------------------------------------------*
FORM CALL_JOB_CLOSE USING P_JOBNAME P_JOBCOUNT.
  CALL FUNCTION 'JOB_CLOSE'
    EXPORTING
*   AT_OPMODE                         = ' '
*   AT_OPMODE_PERIODIC                = ' '
*   CALENDAR_ID                       = ' '
*   EVENT_ID                          = ' '
*   EVENT_PARAM                       = ' '
*   EVENT_PERIODIC                    = ' '
      JOBCOUNT                          = P_JOBCOUNT
      JOBNAME                           = P_JOBNAME
*   LASTSTRTDT                        = NO_DATE
*   LASTSTRTTM                        = NO_TIME
*   PRDDAYS                           = 0
*   PRDHOURS                          = 0
*   PRDMINS                           = 0
*   PRDMONTHS                         = 0
*   PRDWEEKS                          = 0
*   PREDJOB_CHECKSTAT                 = ' '
*   PRED_JOBCOUNT                     = ' '
*   PRED_JOBNAME                      = ' '
*   SDLSTRTDT                         = NO_DATE
*   SDLSTRTTM                         = NO_TIME
*   STARTDATE_RESTRICTION             = BTC_PROCESS_ALWAYS
     STRTIMMED                         = 'X'  "IMMEDIATE
*   TARGETSYSTEM                      = ' '
*   START_ON_WORKDAY_NOT_BEFORE       = SY-DATUM
*   START_ON_WORKDAY_NR               = 0
*   WORKDAY_COUNT_DIRECTION           = 0
*   RECIPIENT_OBJ                     =
*   TARGETSERVER                      = ' '
*   DONT_RELEASE                      = ' '
* IMPORTING
*   JOB_WAS_RELEASED                  =
   EXCEPTIONS
     CANT_START_IMMEDIATE              = 1
     INVALID_STARTDATE                 = 2
     JOBNAME_MISSING                   = 3
     JOB_CLOSE_FAILED                  = 4
     JOB_NOSTEPS                       = 5
     JOB_NOTEX                         = 6
     LOCK_FAILED                       = 7
     OTHERS                            = 8.

  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " CALL_JOB_CLOSE
*&---------------------------------------------------------------------*
*&      Form  JOB_EXECUTION
*&---------------------------------------------------------------------*
FORM JOB_EXECUTION USING    P_JOBNAME P_IND.
  REFRESH R_DATUM. CLEAR R_DATUM.

  R_DATUM-LOW = P_ZEDAT.
  R_DATUM-SIGN = 'I'.
  R_DATUM-OPTION = 'EQ'.
  APPEND R_DATUM.

  WA_REPORT = WA_JOBNAME = P_JOBNAME.
  CONCATENATE WA_JOBNAME P_IND INTO WA_JOBNAME.
*-----> JOB OPEN
  PERFORM CALL_JOB_OPEN USING WA_JOBNAME WA_JOBCOUNT.

*-----> JOB SUBMIT
*  CASE P_IND.
*    WHEN '315' OR '308'.
  SUBMIT (WA_REPORT) WITH P_ZEDAT EQ P_ZEDAT
                     WITH P_ZBTIM EQ P_ZBTIM
                   USER SY-UNAME
                   VIA JOB WA_JOBNAME
                   NUMBER WA_JOBCOUNT
                   AND RETURN.
*    WHEN '320'.
*      SUBMIT (WA_REPORT) WITH S_DATUM IN R_DATUM
*                       USER SY-UNAME
*                       VIA JOB WA_JOBNAME
*                       NUMBER WA_JOBCOUNT
*                       AND RETURN.
*    WHEN '322'.
*      SUBMIT (WA_REPORT) WITH S_DATUM IN R_DATUM
*                         WITH R1 EQ 'X'
*                         WITH R2 EQ SPACE
*                       USER SY-UNAME
*                       VIA JOB WA_JOBNAME
*                       NUMBER WA_JOBCOUNT
*                       AND RETURN.
*    WHEN '323'.
*      SUBMIT (WA_REPORT) WITH R1 EQ SPACE
*                         WITH R2 EQ 'X'
*                       USER SY-UNAME
*                       VIA JOB WA_JOBNAME
*                       NUMBER WA_JOBCOUNT
*                       AND RETURN.
*  ENDCASE.

*    PERFORM CALL_JOB_SUBMIT USING WA_JOBNAME
*                                  WA_REPORT
*                                  WA_JOBCOUNT.

*-----> JOB CLOSE
  PERFORM CALL_JOB_CLOSE USING WA_JOBNAME
                               WA_JOBCOUNT.

ENDFORM.                    " JOB_EXECUTION
*&---------------------------------------------------------------------*
*&      Form  READ_PLP0
*&---------------------------------------------------------------------*
FORM READ_PLP0 USING    P_VSPVB
                        P_PLNT
               CHANGING P_USR01.
  SELECT SINGLE USR01
              FROM PLPO
              INTO P_USR01
              WHERE PLNTY EQ 'M'
              AND   PLNNR EQ 'RP'
              AND   WERKS EQ P_PLNT
              AND   USR00 EQ P_VSPVB.
ENDFORM.                                                    " READ_PLP0
