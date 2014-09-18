*----------------------------------------------------------------------*
*   INCLUDE ZEPP316L_BOM_INIT_BDC_05_F01                               *
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
    CASE SCREEN-NAME.
      WHEN 'P_FILETY' OR 'P_TCODE'.
        SCREEN-INPUT = 0.
    ENDCASE.
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
*     ' ' 'ZINFO'       PA_NCOL-ZSUFF,    "INFO
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
*                                  USING    P_MATNR
*                                           P_WERKS
*                                           P_STLAN
*                                           P_STLAL
*                                           P_POSNR
*                                           P_IDNRK
*                                           P_ZSUFF
*                                           P_ZSEQU
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
*  PERFORM SAME_DATA_EXISTS_CHECK.
  PERFORM DATA_CHECK_MARC CHANGING PA_CHECK.
*  IF PA_CHECK NE 'X'.
*    PERFORM READ_MAST_STKO_STPO_CHECK CHANGING PA_CHECK.
*  ENDIF.
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
    WRITE: / 'Material number does not exist ERROR : ', WA_ERRO_IDX.
    FORMAT COLOR OFF.
    PERFORM ERROR_WRITE TABLES IT_EXCL
                        USING  'L'.
  ENDIF.
ENDFORM.                    " DATA_CHECK_MARC
*&---------------------------------------------------------------------*
*&      Form  SAME_DATA_EXISTS_CHECK
*&---------------------------------------------------------------------*
FORM SAME_DATA_EXISTS_CHECK.
  DATA: LT_EXCL LIKE IT_EXCL OCCURS 0 WITH HEADER LINE.
  DATA: MT_EXCL LIKE IT_EXCL OCCURS 0 WITH HEADER LINE.
  DATA: L_TABIX TYPE SY-TABIX,
        M_TABIX TYPE SY-TABIX,
        L_INDEX TYPE SY-INDEX.
  DESCRIBE TABLE IT_EXCL LINES WA_LINE_IDX.
  LT_EXCL[] = IT_EXCL[].
  LOOP AT LT_EXCL.
    L_TABIX = SY-TABIX.
    LOOP AT IT_EXCL WHERE MATNR EQ LT_EXCL-MATNR
                    AND   WERKS EQ LT_EXCL-WERKS
                    AND   STLAN EQ LT_EXCL-STLAN
                    AND   STLAL EQ LT_EXCL-STLAL
                    AND   POSNR EQ LT_EXCL-POSNR
                    AND   IDNRK EQ LT_EXCL-IDNRK
                    AND   ZSUFF EQ LT_EXCL-ZSUFF
                    AND   ZSEQU EQ LT_EXCL-ZSEQU.
      M_TABIX = SY-TABIX.
      L_INDEX = L_INDEX + 1.
      IF L_INDEX GE 2.
        MT_EXCL = IT_EXCL.
        MT_EXCL-ZRESULT = 'L'.
        MT_EXCL-ZMSG = 'Same data exists more than 2 items to Excel.'.
        APPEND MT_EXCL.
        DELETE IT_EXCL INDEX M_TABIX.
        CLEAR: IT_EXCL, MT_EXCL.
      ENDIF.
    ENDLOOP.
    CLEAR: LT_EXCL, L_INDEX.
  ENDLOOP.

  DESCRIBE TABLE MT_EXCL LINES WA_ERRO_IDX.

  FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
  WRITE: / 'BOM EXCLE Upload Total : ', WA_LINE_IDX.
  WRITE: / 'BOM EXCLE Interface Input Data Error : ', WA_ERRO_IDX.
  FORMAT COLOR OFF.
  IF NOT MT_EXCL[] IS INITIAL.
    WRITE: / 'Same data exists more than 2 items to Excel.'.
    SORT MT_EXCL BY MATNR WERKS STLAN STLAL POSNR IDNRK ZSUFF ZSEQU.

    PERFORM ERROR_WRITE TABLES MT_EXCL
                      USING  'L'.
  ENDIF.
ENDFORM.                    " SAME_DATA_EXISTS_CHECK
*&---------------------------------------------------------------------*
*&      Form  READ_MAST_STKO_STPO_CHECK
*&---------------------------------------------------------------------*
FORM READ_MAST_STKO_STPO_CHECK CHANGING PA_CHECK.
  DATA: BEGIN OF LT_MAST OCCURS 0,
          MATNR TYPE MAST-MATNR,  "MATNRIAL NO
          WERKS TYPE MAST-WERKS,  "PLANT
          STLAN TYPE MAST-STLAN,  "BOM USAGE
          STLAL TYPE MAST-STLAL,  "ALTERNATIVE BOM
          BMENG(17),  " TYPE STKO-BMENG,
          STLST(02),  " TYPE STKO-STLST,
        END OF LT_MAST.

  LOOP AT IT_EXCL WHERE UPCT EQ '1'.
    LT_MAST-MATNR = IT_EXCL-MATNR.  "MATNRIAL NO
    LT_MAST-WERKS = IT_EXCL-WERKS.  "PLANT
    LT_MAST-STLAN = IT_EXCL-STLAN.  "BOM USAGE
    LT_MAST-STLAL = IT_EXCL-STLAL.  "ALTERNATIVE BOM
    WRITE: IT_EXCL-BMENG TO LT_MAST-BMENG LEFT-JUSTIFIED,
           IT_EXCL-STLST TO LT_MAST-STLST.
    COLLECT LT_MAST.
    CLEAR: IT_EXCL, LT_MAST.
  ENDLOOP.

  IF NOT LT_MAST[] IS INITIAL.
    SELECT A~MATNR
           A~WERKS
           A~STLAN
           A~STLAL
           B~POSNR
           B~IDNRK
           B~SUFF
           B~SEQU
           A~STLNR
           B~STLKN
         FROM MAST AS A INNER JOIN STPO AS B
                        ON    B~STLTY EQ 'M'
                        AND   A~STLNR EQ B~STLNR
         INTO TABLE IT_MAST
         FOR ALL ENTRIES IN LT_MAST
         WHERE A~MATNR EQ LT_MAST-MATNR
         AND   A~WERKS EQ LT_MAST-WERKS
         AND   A~STLAN EQ LT_MAST-STLAN
         AND   A~STLAL EQ LT_MAST-STLAL.
    IF SY-SUBRC EQ 0.
      PERFORM CHECK_IT_EXCL_FROM_IT_MAST CHANGING PA_CHECK.
*     BOM HEADER BDC PROCESS
      IF PA_CHECK NE 'X'.
        LOOP AT LT_MAST.
          PERFORM BOM_HEADER_CREATE USING LT_MAST-MATNR  "Material
                                          LT_MAST-WERKS  "Plant
                                          LT_MAST-STLAN  "BOM usage
                                          LT_MAST-STLAL  "Alternat BOM
                                          LT_MAST-BMENG  "Confirmed qua
                                          LT_MAST-STLST  "BOM Status
                                    CHANGING PA_CHECK.
          CLEAR: LT_MAST.
        ENDLOOP.
        IF PA_CHECK EQ 'X'.
          CLEAR WA_ERRO_IDX.
          LOOP AT IT_EXCL WHERE ZRESULT EQ 'E'.
            WA_ERRO_IDX = WA_ERRO_IDX + 1.
          ENDLOOP.
          FORMAT COLOR COL_HEADING INTENSIFIED OFF.
          WRITE: / 'BOM HEADER BDC ERROR : ', WA_ERRO_IDX.
          FORMAT COLOR OFF.
          PERFORM ERROR_WRITE TABLES IT_EXCL
                              USING  'E'.
        ENDIF.
      ENDIF.
    ELSE.
      LOOP AT LT_MAST.
        PERFORM BOM_HEADER_CREATE USING LT_MAST-MATNR  "Material
                                        LT_MAST-WERKS  "Plant
                                        LT_MAST-STLAN  "BOM usage
                                        LT_MAST-STLAL  "Alternat BOM
                                        LT_MAST-BMENG  "Confirmed qua
                                        LT_MAST-STLST  "BOM Status
                                  CHANGING PA_CHECK.
        CLEAR: LT_MAST.
      ENDLOOP.
    ENDIF.

  ELSE.
    PA_CHECK = 'X'.
  ENDIF.

  IF PA_CHECK EQ 'X'.
    CLEAR WA_ERRO_IDX.
    LOOP AT IT_EXCL WHERE ZRESULT EQ 'L'.
      WA_ERRO_IDX = WA_ERRO_IDX + 1.
    ENDLOOP.
    FORMAT COLOR COL_HEADING INTENSIFIED OFF.
    WRITE: / 'BOM does exist & BOM does not exist ERROR : ',
             WA_ERRO_IDX.
    FORMAT COLOR OFF.
    PERFORM ERROR_WRITE TABLES IT_EXCL
                        USING  'L'.
  ENDIF.
ENDFORM.                    " READ_MAST_STKO_STPO_CHECK
*&---------------------------------------------------------------------*
*&      Form  CHECK_IT_EXCL_FROM_IT_MAST
*&---------------------------------------------------------------------*
FORM CHECK_IT_EXCL_FROM_IT_MAST CHANGING PA_CHECK.
  DATA: L_TABIX TYPE SY-TABIX.
* SORTING
  SORT IT_MAST BY MATNR WERKS STLAN STLAL POSNR IDNRK
                        ZSUFF ZSEQU STLNR STLKN.
* UPDATE CONTROL TYPE must not be '1' two faces 'BOM HEADER'.
* If is, work can not achieve since.
  LOOP AT IT_EXCL WHERE UPCT EQ '1'.
    L_TABIX = SY-TABIX.
    READ TABLE IT_MAST WITH KEY MATNR = IT_EXCL-MATNR
                                WERKS = IT_EXCL-WERKS
                                STLAN = IT_EXCL-STLAN
                                STLAL = IT_EXCL-STLAL
                                POSNR = IT_EXCL-POSNR
                                IDNRK = IT_EXCL-IDNRK
                                ZSUFF = IT_EXCL-ZSUFF
                                ZSEQU = IT_EXCL-ZSEQU
                       BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      IF IT_EXCL-UPCT EQ '1'.
*       L : LEGACY DATA ERROR
        IT_EXCL-ZRESULT = 'L'.
*       MESSAGE TEXT
        IT_EXCL-ZMSG = 'Update Control type 1 : BOM does exist'.
*       BOM does not exist
        PA_CHECK = 'X'.
      ENDIF.
    ENDIF.
    MODIFY IT_EXCL INDEX L_TABIX TRANSPORTING ZRESULT
                                              ZMSG.
    CLEAR: IT_EXCL, IT_MAST.
  ENDLOOP.
ENDFORM.                    " CHECK_IT_EXCL_FROM_IT_MAST
*&---------------------------------------------------------------------*
*&      Form  BOM_HEADER_CREATE
*&---------------------------------------------------------------------*
FORM BOM_HEADER_CREATE USING P_MATNR
                             P_WERKS
                             P_STLAN
                             P_STLAL
                             P_BMENG
                             P_STLST
                       CHANGING PA_CHECK.
  DATA: L_TABIX TYPE SY-TABIX,
        L_MSG   LIKE CFGNL-MSGLIN,
        L_MSGTY TYPE SY-MSGTY.
  PERFORM DYNPRO USING:
     'X' 'SAPLCSDI'    '0100',
     ' ' 'RC29N-MATNR' P_MATNR,    "NEXT MATERIAL
     ' ' 'RC29N-WERKS' P_WERKS,    "PLANT
     ' ' 'RC29N-STLAN' P_STLAN,    "BOM STLANe
     ' ' 'RC29N-STLAL' P_STLAL,    "ALT BOM
     ' ' 'RC29N-AENNR' '19590101-001',    "Change number
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

  LOOP AT IT_EXCL WHERE MATNR EQ P_MATNR
                  AND   WERKS EQ P_WERKS
                  AND   STLAN EQ P_STLAN
                  AND   STLAL EQ P_STLAL.

    L_TABIX = SY-TABIX.

    IT_EXCL-ZRESULT = L_MSGTY.
    IT_EXCL-ZMSG = L_MSG.
    MODIFY IT_EXCL INDEX L_TABIX TRANSPORTING ZRESULT
                                              ZMSG .
  ENDLOOP.
  IF L_MSGTY EQ 'E'.
    PA_CHECK = 'X'.
  ENDIF.
  REFRESH: IT_BDC, IT_MESS.
ENDFORM.         " BOM_HEADER_CREATE
*&---------------------------------------------------------------------*
*&      Form  BOM_HEADER
*&---------------------------------------------------------------------*
FORM BOM_HEADER USING P_MATNR
                      P_WERKS
                      P_STLAN
                      P_STLAL
                      P_BMENG
                      P_STLST.
  DATA: L_TABIX TYPE SY-TABIX,
        L_MSG   LIKE CFGNL-MSGLIN,
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

*  IF L_MSGTY EQ 'E'.
*    PA_CHECK = 'X'.
*  ENDIF.
  REFRESH: IT_BDC, IT_MESS.
ENDFORM.         " BOM_HEADER
*&---------------------------------------------------------------------*
*&      Form  ERROR_WRITE
*&---------------------------------------------------------------------*
FORM ERROR_WRITE TABLES   PT_EXCL STRUCTURE IT_EXCL
                 USING    P_ZRESULT.
  WRITE: /(18) 'MATERIAL(NEXT)',
          (05) 'PLANT',
          (05) 'USAGE',
          (06) 'AltBOM',
          (12) 'EO NOMBER',
          (07) 'BASEQTY',
          (08) 'NEXTUNIT',
          (06) 'STATUS',
          (07) 'PREFIXN',
          (03) 'ICT',
          (18) 'COMPONENT',
          (10) 'QTY',
          (08) 'COMPU/M',
          (08) 'PHANTOM',
          (08) 'END ITEM',
          (08) 'SUFFIX  ',
          (10) 'COLORPART',
          (30) 'OBJECT DEPENDENCY',
          (12) 'CONTROL TYPE',
          (08) 'MES TYPE',
               'MESSAGE'.
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

  DATA: L_MSG   LIKE CFGNL-MSGLIN,
        L_TABIX TYPE SY-TABIX.
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

       'X' 'SAPLMGMM'    '4004',
       ' ' 'MARA-KZKFG'  'X',   "
       ' ' 'BDC_OKCODE'  '=BU'.

    CALL TRANSACTION 'MM02'  USING IT_BDC
             OPTIONS FROM WA_OPT
             MESSAGES INTO IT_MESS.
*    IT_BOM_EXPLODED-ZRESULT = SY-MSGTY.
    PERFORM RKC_MSG_STRING CHANGING L_MSG.
*    IT_BOM_EXPLODED-ZMSG = L_MSG.
*    MODIFY IT_BOM_EXPLODED INDEX L_TABIX TRANSPORTING ZRESULT
*                                                      ZMSG.
    WRITE: / IT_BOM_EXPLODED, L_MSG.
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
*&      Form  DEPENDENCY_ECM_CHECK
*&---------------------------------------------------------------------*
FORM DEPENDENCY_ECM_CHECK CHANGING PA_CHECK.
  DATA: LT_EXCL LIKE IT_EXCL OCCURS 0 WITH HEADER LINE,
        L_TABIX LIKE SY-TABIX,
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
  WRITE: / 'Dependency Check TOTAL LINES : ', WA_LINE_IDX.
  WRITE: / 'Dependency does not exist ERROR TOTAL LINES : ',
            WA_ERRO_IDX.
  FORMAT COLOR OFF.
  IF WA_ERRO_IDX GE '1'.
    PERFORM ERROR_WRITE TABLES LT_EXCL
                        USING  'L'.
  ENDIF.
  REFRESH LT_EXCL. CLEAR LT_EXCL.
* CHANGE NUMBER CHECK
  LOOP AT IT_EXCL.
    LT_AENR-AENNR = IT_EXCL-AENNR.
    COLLECT LT_AENR.
    CLEAR: IT_EXCL, LT_AENR.
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
      WRITE: / 'Change No Check TOTAL LINES : ', WA_LINE_IDX.
      WRITE: / 'Change No does not exist ERROR TOTAL LINES : ',
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
  DATA: MT_EXCL LIKE IT_EXCL OCCURS 0 WITH HEADER LINE,
        L_NCHR(10),
        L_CHK(1),
        L_TABIX TYPE SY-TABIX,
        L_TANEW TYPE SY-TABIX,
        L_TAEND TYPE SY-TABIX,
        L_MODE,
        L_MSG  LIKE CFGNL-MSGLIN,
        L_STLKN TYPE STPO-STLKN,
        L_ATNAM TYPE CABN-ATNAM,
        L_ZRESULT LIKE IT_EXCL-ZRESULT,
        L_ZMSG LIKE IT_EXCL-ZMSG.
  DATA: LA_NCOL LIKE IT_NCOL.
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
  WRITE: / 'BOM NON COLOR PART BDC TOTAL LINES : ', WA_LINE_IDX.
  WRITE: / 'BOM NON COLOR PART BDC ERROR TOTAL LINES : ', WA_ERRO_IDX.
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
  DATA: MT_EXCL LIKE IT_EXCL OCCURS 0 WITH HEADER LINE,
        L_NCHR(10),
        L_CHK(1),
        L_TABIX TYPE SY-TABIX,
        L_TANEW TYPE SY-TABIX,
        L_TAEND TYPE SY-TABIX,
        L_MODE,
        L_MSG  LIKE CFGNL-MSGLIN,
        L_STLKN TYPE STPO-STLKN,
        L_ATNAM TYPE CABN-ATNAM,
        L_ZRESULT LIKE IT_EXCL-ZRESULT,
        L_ZMSG LIKE IT_EXCL-ZMSG.

* SORTING
  SORT IT_COLO BY MATNR WERKS STLAN STLAL POSNR
                  IDNRK ZSUFF ZSEQC UPCT ZSEQU.
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
          PERFORM COLOR_PART_CALL_TRANSACTION USING    L_TANEW
                                                        L_TAEND
                                               CHANGING L_ZRESULT
                                                        L_ZMSG
                                                        L_MODE.
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
  WRITE: / 'BOM COLOR PART BDC TOTAL LINES : ', WA_LINE_IDX.
  WRITE: / 'BOM COLOR PART BDC ERROR TOTAL LINES : ', WA_ERRO_IDX.
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
*                      INNER JOIN STAS AS C
*                      ON    C~STLTY EQ 'M'
*                      AND   A~STLNR EQ C~STLNR
*                      AND   A~STLAL EQ C~STLAL
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
*&      Form  READ_MAST_STPO_STLKN1
*&---------------------------------------------------------------------*
FORM READ_MAST_STPO_STLKN1 USING    P_MATNR
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
    P_MODE = '2'.
  ELSE.
    P_MODE = '1'.
  ENDIF.
ENDFORM.                    " READ_MAST_STPO_STLKN1
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
*&      Form  CHANGE_BADY_ITEM_CREATE1
*&---------------------------------------------------------------------*
FORM   CHANGE_BADY_ITEM_CREATE1 USING    PA_COLO STRUCTURE WA_COLO.
  DATA: L_DBCNT TYPE SY-DBCNT.
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
     ' ' 'ZSEQU'       PA_COLO-ZSEQC,    "SEQUENCE NO
     ' ' 'ZUPGN'       PA_COLO-ZUPGN,    "UPG
*     ' ' 'ZINFO'       PA_COLO-ZINFO,    "INFO
     ' ' 'BDC_OKCODE'  '/00'.

  IF PA_COLO-CLPT EQ 'C'.

    SELECT COUNT( * )
         FROM MAST AS A INNER JOIN STPO AS B
                        ON A~STLNR EQ B~STLNR
         INTO L_DBCNT
         WHERE A~MATNR EQ PA_COLO-MATNR
         AND   A~WERKS EQ PA_COLO-WERKS
         AND   A~STLAN EQ PA_COLO-STLAN
         AND   A~STLAL EQ PA_COLO-STLAL.
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
      ' ' 'RCUKD-KNNAM(01)' PA_COLO-DPID,      " O/J DEPENDENCY
      ' ' 'BDC_OKCODE'  '/00',

      'X' 'SAPLCUKD'    '0130',
      ' ' 'BDC_OKCODE'  '=BACK',

        'X' 'SAPLCSDI'    '0140',
        ' ' 'BDC_OKCODE'  '=FCNP'.
  ELSE.
    PERFORM DYNPRO USING:
         'X' 'SAPLCSDI'    '0140',
         ' ' 'BDC_OKCODE'  '=FCNP'.
  ENDIF.

ENDFORM.                    " CHANGE_BADY_ITEM_CREATE1
*&---------------------------------------------------------------------*
*&      Form  CHANGE_BADY_ITEM
*&---------------------------------------------------------------------*
FORM   CHANGE_BADY_ITEM USING    PA_COLO STRUCTURE WA_COLO.
  DATA: L_DBCNT TYPE SY-DBCNT.
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
     ' ' 'ZSEQU'       PA_COLO-ZSEQC,    "SEQUENCE NO
     ' ' 'ZUPGN'       PA_COLO-ZUPGN,    "UPG
*     ' ' 'ZINFO'       PA_COLO-ZINFO,    "INFO
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

*     'X' 'SAPLCSDI'    '0150',
*     ' ' 'BDC_OKCODE'  '=FCNP'.
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
*&      Form  COLOR_PART_CONTROL_1
*&---------------------------------------------------------------------*
FORM COLOR_PART_CONTROL_1 USING    PA_COLO STRUCTURE WA_COLO
                          CHANGING P_MODE
                                   P_STLKN.
  PERFORM READ_MAST_STPO_STLKN1 USING PA_COLO-MATNR "Material
                                      PA_COLO-WERKS "Plant
                                      PA_COLO-STLAN "BOM usage
                                      PA_COLO-STLAL "Alternative
                                      PA_COLO-POSNR "Item number
                                      PA_COLO-IDNRK "component
                                      PA_COLO-ZSUFF "SUFFIX
                                      PA_COLO-ZSEQU
                              CHANGING P_MODE
                                       P_STLKN.
  IF P_MODE EQ '1'. "CHANGE(ITEM CREATE)
    PERFORM CHANGE_HEADER_BOM USING     WA_COLO.

  ELSEIF P_MODE EQ '2'.
    PERFORM APPEND_OBJECT_DEPENDENCY USING     WA_COLO
                                               P_STLKN.
  ENDIF.
ENDFORM.                    " COLOR_PART_CONTROL_1
*&---------------------------------------------------------------------*
*&      Form  COLOR_PART_BADY_1
*&---------------------------------------------------------------------*
FORM COLOR_PART_BADY_1 USING    PA_COLO STRUCTURE WA_COLO
                       CHANGING P_MODE.
  IF P_MODE EQ '1'.
    PERFORM CHANGE_BADY_ITEM_CREATE1 USING     WA_COLO.
    P_MODE = '2'.
  ELSEIF P_MODE EQ '2'.
    PERFORM BADY_DEPENDENCY USING PA_COLO-DPID.
  ENDIF.
ENDFORM.                    " COLOR_PART_BADY_1
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
*&      Form  OBJECT_DEPENDENCY_DELETE
*&---------------------------------------------------------------------*
FORM OBJECT_DEPENDENCY_DELETE USING    P_TANEW
                                       P_TAEND
                              CHANGING P_ZRESULT
                                       P_ZMSG
                                       P_MODE.

ENDFORM.                    " OBJECT_DEPENDENCY_DELETE
*&---------------------------------------------------------------------*
*&      Form  MAST_STPO_CHECK
*&---------------------------------------------------------------------*
FORM MAST_STPO_CHECK USING    PA_NCOL STRUCTURE WA_NCOL
                     CHANGING P_MODE.
  DATA: L_STLNR LIKE MAST-STLNR.

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
                                  ZSEQU  = PA_NCOL-ZSEQU
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
*  SELECT A~MATNR
*         A~WERKS
*         A~STLAN
*         A~STLAL
*         B~POSNR
*         B~IDNRK
*         B~SUFF
*         B~SEQU
*         A~STLNR
*         B~STLKN
*       FROM MAST AS A INNER JOIN STPO AS B
*                      ON    B~STLTY EQ 'M'
*                      AND   A~STLNR EQ B~STLNR
*    INTO TABLE IT_MAST
*       WHERE A~MATNR EQ LA_NCOL-MATNR
*       AND   A~WERKS EQ LA_NCOL-WERKS
*       AND   A~STLAN EQ LA_NCOL-STLAN
*       AND   A~STLAL EQ LA_NCOL-STLAL.
ENDFORM.                    " MAST_STPO_CHECK
*&---------------------------------------------------------------------*
*&      Form  CHECK_IT_STPO
*&---------------------------------------------------------------------*
FORM CHECK_IT_STPO USING    PA_NCOL STRUCTURE WA_NCOL
                   CHANGING P_CHK.
  READ TABLE IT_STPO WITH KEY POSNR  = PA_NCOL-POSNR
                              IDNRK  = PA_NCOL-IDNRK
                              ZSUFF  = PA_NCOL-ZSUFF
                              ZSEQU  = PA_NCOL-ZSEQU
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

  DATA: L_STLNR LIKE MAST-STLNR.

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
                                  ZSEQU  = PA_COLO-ZSEQC
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
    IF IT_COLO-ZRESULT EQ 'E'.
      WA_ERRO_IDX = WA_ERRO_IDX + 1.
      MOVE-CORRESPONDING IT_COLO TO IT_EXCL.
      APPEND IT_EXCL.
    ENDIF.
    CLEAR: IT_COLO, IT_EXCL.
  ENDLOOP.
*  REFRESH IT_COLO. CLEAR IT_COLO.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE: / 'BOM COLOR PART OBJECT DEPENDENCY BDC TOTAL LINES : ',
            WA_LINE_IDX.
  WRITE: / 'BOM COLOR PART OBJECT DEPENDENCY BDC ERROR TOTAL LINES : ',
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
  L_SEQU = PA_COLO-ZSEQC.
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
