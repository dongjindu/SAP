*----------------------------------------------------------------------*
*   INCLUDE ZIPP304U_FSC_PRODU_BDC_F01                                 *
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
      CONCATENATE P_FILE TEXT-021 "' DATA UPLOAD SUCCESS!!'
                  INTO L_TEXT.
      WRITE: / L_TEXT.
      SKIP.
    WHEN 2.
      MESSAGE E000 WITH TEXT-022. "'FILE OPEN ERROR, FILE NO FOUND!'.
    WHEN 3.
      MESSAGE E000 WITH TEXT-023. "'FILE READ ERROR'.
    WHEN OTHERS.
    MESSAGE E000 WITH TEXT-024. "'FILE UPLOAD ERROR, CHECK YOUR FILE!.'.
  ENDCASE.

ENDFORM.                               " UPLOAD_PROCESS
*&---------------------------------------------------------------------*
*&      Form  BDC_PROCESS
*&---------------------------------------------------------------------*
FORM BDC_PROCESS.
  DATA: L_TABIX TYPE SY-TABIX,
        L_MSG  LIKE CFGNL-MSGLIN,
        L_PLNNR TYPE MAPL-PLNNR,
        L_FVLD(10),
        L_TVLD(10).
  REFRESH: IT_BDC, IT_MESS. CLEAR: IT_BDC, IT_MESS.
* DATA TOTAL LINES
  DESCRIBE TABLE IT_ALFP LINES WA_LINE_IDX.
  LOOP AT IT_ALFP.
    L_TABIX = SY-TABIX.
    WRITE: IT_ALFP-FVLD TO L_FVLD,
           IT_ALFP-TVLD TO L_TVLD.
**   2004.01.07 RATE ROUTING ADDTIONAL
*    PERFORM RATE_ROUTING USING    IT_ALFP-MTNO+6(7)
*                         CHANGING L_PLNNR.

    PERFORM DYNPRO USING:
       'X' 'SAPLMGMM'    '0060',
       ' ' 'RMMG1-MATNR' IT_ALFP-MTNO,   "
       ' ' 'BDC_OKCODE'  '=AUSW',

       'X' 'SAPLMGMM'    '0070',
*       2003/12/21 SCREEN NUMBER CHANGE
       ' ' 'MSICHTAUSW-KZSEL(01)' 'X',   "
*       ' ' 'MSICHTAUSW-KZSEL(12)' 'X',   "
       ' ' 'BDC_OKCODE'  '=ENTR',

       'X' 'SAPLMGMM'    '5004',
       ' ' 'BDC_OKCODE'  '=SP15',

       'X' 'SAPLMGMM'    '0081',
       ' ' 'RMMG1-WERKS' IT_ALFP-PLNT,   "
       ' ' 'BDC_OKCODE'  '=ENTR',
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
       'X' 'SAPLMGMM'    '5004',
       ' ' 'BDC_OKCODE'  '=PB03',

       'X' 'SAPLMDIA'    '0100',
       ' ' 'BDC_OKCODE'  '=NEWM',

       'X' 'SAPLMDIA'    '0100',
       ' ' 'BDC_CURSOR' 'MKAL-VERID(02)',
       ' ' 'MKAL-VERID(02)' IT_ALFP-VERS,   "
       ' ' 'MKAL-TEXT1(02)' IT_ALFP-TEXT,   "
       ' ' 'MKAL-ADATU(02)' L_FVLD,   "
       ' ' 'MKAL-BDATU(02)' L_TVLD,   "
       ' ' 'BDC_OKCODE'  '=DETA',

       'X' 'SAPLMDIA'    '0200',
       ' ' 'MKAL-STLAL' IT_ALFP-ALTN,   "
       ' ' 'MKAL-STLAN' IT_ALFP-USAG,   "
**      2004.01.07
**      RATE ROUTING ADDTIONAL
*       ' ' 'MKAL-PLTYG'  'R',   "
*       ' ' 'MKAL-PLNNG'  L_PLNNR,   "
*       ' ' 'MKAL-ALNAG'  '1',   "
*       ' ' 'MKAL-SERKZ'  'X',   "
*       ' ' 'MKAL-MDV01'  '1',   "
       ' ' 'BDC_OKCODE'  '=ENTR',

       'X' 'SAPLMDIA'    '0100',
       ' ' 'BDC_OKCODE'  '=ENTR',

*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
       'X' 'SAPLMGMM'    '5004',
       ' ' 'BDC_OKCODE'  '=BU'.

    CALL TRANSACTION 'MM02'  USING         IT_BDC
                             OPTIONS  FROM WA_OPT
                             MESSAGES INTO IT_MESS.
    IT_ALFP-ZRESULT = SY-MSGTY.
    IT_ALFP-ZMODE = TEXT-020.
    PERFORM RKC_MSG_STRING CHANGING L_MSG.
    IT_ALFP-ZMSG = L_MSG.
*   MODIFY IT_ALFP
    PERFORM IT_ALFP_MODIFY USING L_MSG
                                 SY-MSGTY
                                 L_TABIX.
    MOVE-CORRESPONDING IT_ALFP TO IT_ROUT.
    IT_ROUT-PLNNR = L_PLNNR.
    APPEND IT_ROUT.
    CLEAR IT_ROUT.
    REFRESH: IT_BDC, IT_MESS.
    CLEAR: IT_BDC, IT_MESS, L_PLNNR.
  ENDLOOP.
ENDFORM.                    " BDC_PROCESS
*&---------------------------------------------------------------------*
*&      Form  IT_ALFP_MODIFY
*&---------------------------------------------------------------------*
FORM IT_ALFP_MODIFY USING P_MSG
                          P_MSGTY
                          P_TABIX.

  CASE P_MSGTY.
    WHEN 'S'.
      IT_ALFP-ZBDAT = SY-DATUM.  "SAP BDC EXECUTED DATE
      IT_ALFP-ZBTIM = SY-UZEIT.  "SAP BDC EXECUTED TIME
      IT_ALFP-ZBNAM = SY-UNAME.  "BDC User ID
*     C: Create U: Update D: Delete
      IT_ALFP-ZMODE = TEXT-020.       "C:CREATE
    WHEN 'E'.
      IT_ALFP-ZBDAT = SY-DATUM.  "SAP BDC EXECUTED DATE
*      IT_ALFP-ZBTIM = SY-UZEIT.  "SAP BDC EXECUTED TIME
      IT_ALFP-ZBNAM = SY-UNAME.  "BDC User ID
      IT_ALFP-ZMODE = TEXT-020.       "C:CREATE
    WHEN OTHERS.
      IT_ALFP-ZBDAT = SY-DATUM.  "SAP BDC EXECUTED DATE
      IT_ALFP-ZBTIM = SY-UZEIT.  "SAP BDC EXECUTED TIME
      IT_ALFP-ZBNAM = SY-UNAME.  "BDC User ID
      IT_ALFP-ZMODE = TEXT-020.       "C:CREATE
  ENDCASE.
* IT_ACHR MODIFY
  MODIFY IT_ALFP INDEX P_TABIX TRANSPORTING ZBDAT
                                            ZBTIM
                                            ZBNAM
                                            ZMODE
                                            ZRESULT
                                            ZMSG.
ENDFORM.                    " IT_ALFP_MODIFY
*&---------------------------------------------------------------------*
*&      Form  BDC_PROCESS1
*&---------------------------------------------------------------------*
FORM BDC_PROCESS1.
  DATA: L_TABIX TYPE SY-TABIX,
        L_PLNNR TYPE MAPL-PLNNR.
* DATA TOTAL LINES
  DESCRIBE TABLE IT_EXCL LINES WA_LINE_IDX.
  FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
  WRITE: / TEXT-025, "'PROD VERSION Upload Total : ',
           WA_LINE_IDX.
  FORMAT COLOR OFF.
  LOOP AT IT_EXCL.
    L_TABIX = SY-TABIX.

**   2004.01.07 RATE ROUTING ADDTIONAL
*    PERFORM RATE_ROUTING1 USING    IT_EXCL-MATNR+6(7)
*                          CHANGING L_PLNNR.

    PERFORM DYNPRO USING:
       'X' 'SAPLMGMM'    '0060',
       ' ' 'RMMG1-MATNR' IT_EXCL-MATNR,   "
       ' ' 'BDC_OKCODE'  '=AUSW',

       'X' 'SAPLMGMM'    '0070',
*       2003/12/21 SCREEN NUMBER CHANGE
       ' ' 'MSICHTAUSW-KZSEL(01)' 'X',   "
*       ' ' 'MSICHTAUSW-KZSEL(12)' 'X',   "
       ' ' 'BDC_OKCODE'  '=ENTR',

       'X' 'SAPLMGMM'    '5004',
       ' ' 'BDC_OKCODE'  '=SP15',

       'X' 'SAPLMGMM'    '0081',
       ' ' 'RMMG1-WERKS' IT_EXCL-WERKS,  "
       ' ' 'BDC_OKCODE'  '=ENTR',
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
       'X' 'SAPLMGMM'    '5004',
       ' ' 'BDC_OKCODE'  '=PB03',

       'X' 'SAPLMDIA'    '0100',
       ' ' 'BDC_OKCODE'  '=NEWM',

       'X' 'SAPLMDIA'    '0100',
       ' ' 'MKAL-VERID(02)' IT_EXCL-VERID,   "
       ' ' 'MKAL-TEXT1(02)' IT_EXCL-TEXT1,   "
       ' ' 'MKAL-ADATU(02)' IT_EXCL-ADATU,   "
       ' ' 'MKAL-BDATU(02)' IT_EXCL-BDATU,   "
       ' ' 'BDC_OKCODE'  '=DETA',

       'X' 'SAPLMDIA'    '0200',
       ' ' 'MKAL-STLAL' IT_EXCL-STLAL,   "
       ' ' 'MKAL-STLAN' IT_EXCL-STLAN,   "
**      2004.01.07
**      RATE ROUTING ADDTIONAL
*       ' ' 'MKAL-PLTYG'  'R',   "
*       ' ' 'MKAL-PLNNG'  L_PLNNR,   "
*       ' ' 'MKAL-ALNAG'  '1',   "
*       ' ' 'MKAL-SERKZ'  'X',   "
       ' ' 'BDC_OKCODE'  '=ENTR',

       'X' 'SAPLMDIA'    '0100',
       ' ' 'BDC_OKCODE'  '=ENTR',

*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
       'X' 'SAPLMGMM'    '5004',
       ' ' 'BDC_OKCODE'  '=BU'.

    CALL TRANSACTION 'MM02'  USING IT_BDC
                    OPTIONS FROM WA_OPT
                    MESSAGES INTO IT_MESS.
    IT_EXCL-MSGTY = SY-MSGTY.
    PERFORM RKC_MSG_STRING CHANGING IT_EXCL-MESSG.
*     MODIFY IT_EXCL
    MODIFY IT_EXCL INDEX L_TABIX TRANSPORTING MSGTY
                                              MESSG.
    REFRESH: IT_BDC, IT_MESS.
  ENDLOOP.
ENDFORM.                    " BDC_PROCESS1
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
ENDFORM.                    " DYNPRO
*&---------------------------------------------------------------------*
*&      Form  WRITE_PROCESS
*&---------------------------------------------------------------------*
FORM WRITE_PROCESS.
  DATA L_INDEX TYPE SY-INDEX.

  CLEAR: WA_ERRO_IDX, WA_LINE_IDX.
  DESCRIBE TABLE IT_ALFP LINES WA_LINE_IDX.
  LOOP AT IT_ALFP WHERE ZRESULT EQ 'E'.
    WA_ERRO_IDX = WA_ERRO_IDX + 1.
  ENDLOOP.
  WA_LINE_IDX = WA_LINE_IDX - WA_ERRO_IDX.
  FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
  WRITE: / TEXT-026, "'PRODUCTION VERSION BDC SUCCESS :  ',
           WA_LINE_IDX.
  WRITE: / TEXT-027, "'PRODUCTION VERSION BDC ERROR :  ',
           WA_ERRO_IDX.
  FORMAT COLOR OFF.
  IF WA_ERRO_IDX GE 1.
    FORMAT COLOR COL_HEADING INTENSIFIED OFF.
    WRITE: / TEXT-027. "'PRODUCTION VERSION BDC ERROR'.
    FORMAT COLOR OFF.
    WRITE: /(18)  TEXT-006,
            (05)  TEXT-007,
            (07)  TEXT-008,
            (40)  TEXT-009,
            (10)  TEXT-010,
            (10)  TEXT-011,
            (10)  TEXT-012,
            (10)  TEXT-013,
            (10)  TEXT-014,
                  TEXT-015.
    LOOP AT IT_ALFP WHERE ZRESULT EQ 'E'.
      WRITE: /(18) IT_ALFP-MTNO,
              (05) IT_ALFP-PLNT,
              (07) IT_ALFP-VERS,
              (40) IT_ALFP-TEXT,
              (10) IT_ALFP-FVLD,
              (10) IT_ALFP-TVLD,
              (10) IT_ALFP-ALTN,
              (10) IT_ALFP-USAG,
              (10) IT_ALFP-ZRESULT,
                   IT_ALFP-ZMSG.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " WRITE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  WRITE_ROUTING_PROCESS
*&---------------------------------------------------------------------*
FORM WRITE_ROUTING_PROCESS.
  DATA L_INDEX TYPE SY-INDEX.

  FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
  WRITE: / TEXT-028,
           WA_LINE_IDX.
  WRITE: / TEXT-029,
           WA_ERRO_IDX.
  FORMAT COLOR OFF.
  IF WA_ERRO_IDX GE 1.
    FORMAT COLOR COL_HEADING INTENSIFIED OFF.
    WRITE: / TEXT-029.
    FORMAT COLOR OFF.
    WRITE: /(18)  TEXT-006,
            (05)  TEXT-007,
            (07)  TEXT-008,
            (40)  TEXT-009,
            (10)  TEXT-010,
            (10)  TEXT-011,
            (10)  TEXT-012,
            (10)  TEXT-013,
            (10)  TEXT-014,
            (18)  TEXT-030,
                  TEXT-015.
    LOOP AT IT_ROUT WHERE ZRESULT EQ 'E'.
      WRITE: /(18) IT_ROUT-MTNO,
              (05) IT_ROUT-PLNT,
              (07) IT_ROUT-VERS,
              (40) IT_ROUT-TEXT,
              (10) IT_ROUT-FVLD,
              (10) IT_ROUT-TVLD,
              (10) IT_ROUT-ALTN,
              (10) IT_ROUT-USAG,
              (10) IT_ROUT-ZRESULT,
              (18) IT_ROUT-PLNNR,
                   IT_ROUT-ZMSG.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " WRITE_ROUTING_PROCESS
*&---------------------------------------------------------------------*
*&      Form  WRITE_PROCESS1
*&---------------------------------------------------------------------*
FORM WRITE_PROCESS1.
  DATA L_INDEX TYPE SY-INDEX.

  CLEAR: WA_ERRO_IDX, WA_LINE_IDX.
  DESCRIBE TABLE IT_EXCL LINES WA_LINE_IDX.
  LOOP AT IT_EXCL WHERE MSGTY EQ 'E'.
    WA_ERRO_IDX = WA_ERRO_IDX + 1.
  ENDLOOP.
  WA_LINE_IDX = WA_LINE_IDX - WA_ERRO_IDX.
  FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
  WRITE: / TEXT-026, "'PRODUCTION VERSION BDC SUCCESS :  ',
           WA_LINE_IDX.
  WRITE: / TEXT-027, "'PRODUCTION VERSION BDC ERROR :  ',
           WA_ERRO_IDX.
  FORMAT COLOR OFF.
  IF WA_ERRO_IDX GE 1.
    FORMAT COLOR COL_HEADING INTENSIFIED OFF.
    WRITE: / TEXT-027. "'PRODUCTION VERSION BDC ERROR'.
    FORMAT COLOR OFF.
    WRITE: /(18)  TEXT-006,
            (05)  TEXT-007,
            (07)  TEXT-008,
            (40)  TEXT-009,
            (10)  TEXT-010,
            (10)  TEXT-011,
            (10)  TEXT-012,
            (10)  TEXT-013,
            (10)  TEXT-014,
                  TEXT-015.
    LOOP AT IT_EXCL WHERE MSGTY EQ 'E'.
      WRITE: /(18) IT_EXCL-MATNR,
              (05) IT_EXCL-WERKS,
              (07) IT_EXCL-VERID,
              (40) IT_EXCL-TEXT1,
              (10) IT_EXCL-ADATU,
              (10) IT_EXCL-BDATU,
              (10) IT_EXCL-STLAL,
              (10) IT_EXCL-STLAN,
              (10) IT_EXCL-MSGTY,
                   IT_EXCL-MESSG.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " WRITE_PROCESS1
*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
FORM READ_PROCESS.
  SELECT *
       FROM ZTBM_ABXLFPDT
       INTO TABLE IT_ALFP
       WHERE ZEDAT EQ P_ZEDAT
       AND   ZBTIM EQ P_ZBTIM.
  IF SY-SUBRC NE 0.
    WRITE: / TEXT-001.
  ENDIF.
ENDFORM.                    " READ_PROCESS
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
FORM DATA_PROCESS.
  DATA: BEGIN OF LT_MARC OCCURS 0,
          MATNR TYPE MARC-MATNR,
          WERKS TYPE MARC-WERKS,
        END OF LT_MARC,
        MT_MARC LIKE LT_MARC OCCURS 0 WITH HEADER LINE,
        LT_ALFP LIKE IT_ALFP OCCURS 0 WITH HEADER LINE,
        L_TABIX TYPE SY-TABIX.
  DESCRIBE TABLE IT_ALFP LINES WA_LINE_IDX.

  LOOP AT IT_ALFP.
    LT_MARC-MATNR = IT_ALFP-MTNO.
    LT_MARC-WERKS = IT_ALFP-PLNT.
    COLLECT LT_MARC.
    CLEAR: IT_ALFP, LT_MARC.
  ENDLOOP.
  IF NOT LT_MARC[] IS INITIAL.
    SELECT MATNR
           WERKS
         FROM MARC
         INTO TABLE MT_MARC
         FOR ALL ENTRIES IN LT_MARC
         WHERE MATNR EQ LT_MARC-MATNR
         AND   WERKS EQ LT_MARC-WERKS.
    IF SY-SUBRC EQ 0.
*     SORTING
      SORT MT_MARC BY MATNR WERKS.
      LOOP AT IT_ALFP.
        L_TABIX = SY-TABIX.
        READ TABLE MT_MARC WITH KEY MATNR = IT_ALFP-MTNO
                                    WERKS = IT_ALFP-PLNT
                           BINARY SEARCH.
        IF SY-SUBRC NE 0.
          LT_ALFP = IT_ALFP.
          LT_ALFP-ZBDAT = SY-DATUM.
          LT_ALFP-ZBNAM = SY-UNAME.
          LT_ALFP-ZRESULT = TEXT-002.
          LT_ALFP-ZMSG = TEXT-003.
          DELETE IT_ALFP INDEX L_TABIX.
          APPEND LT_ALFP.
        ENDIF.
        CLEAR: IT_ALFP, LT_ALFP, MT_MARC.
      ENDLOOP.
    ELSE.
*     ALL ERROR
      LOOP AT IT_ALFP.
        L_TABIX = SY-TABIX.
        LT_ALFP = IT_ALFP.
        LT_ALFP-ZBDAT = SY-DATUM.
        LT_ALFP-ZBNAM = SY-UNAME.
        LT_ALFP-ZRESULT = TEXT-002.
        LT_ALFP-ZMSG = TEXT-003.
        DELETE IT_ALFP INDEX L_TABIX.
        APPEND LT_ALFP.
        CLEAR: LT_ALFP, IT_ALFP.
      ENDLOOP.
    ENDIF.
  ENDIF.
  DESCRIBE TABLE LT_ALFP LINES WA_ERRO_IDX.

  FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
  WRITE: / TEXT-004, WA_LINE_IDX.
  WRITE: / TEXT-005, WA_ERRO_IDX.
  FORMAT COLOR OFF.
* Material number does not exist WRITE
  IF NOT LT_ALFP[] IS INITIAL.
    WRITE: / TEXT-003.
    PERFORM NOT_EXIST_MATNRIAL_WRITE TABLES LT_ALFP.
    UPDATE ZTBM_ABXLFPDT FROM TABLE LT_ALFP.
    IF SY-SUBRC EQ 0.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
  ENDIF.
ENDFORM.                    " DATA_PROCESS
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS1
*&---------------------------------------------------------------------*
FORM DATA_PROCESS1.
  DATA: BEGIN OF LT_MARC OCCURS 0,
          MATNR TYPE MARC-MATNR,
          WERKS TYPE MARC-WERKS,
        END OF LT_MARC,
        MT_MARC LIKE LT_MARC OCCURS 0 WITH HEADER LINE,
        LT_EXCL LIKE IT_EXCL OCCURS 0 WITH HEADER LINE,
        L_TABIX TYPE SY-TABIX.
  DESCRIBE TABLE IT_EXCL LINES WA_LINE_IDX.

  LOOP AT IT_EXCL.
    LT_MARC-MATNR = IT_EXCL-MATNR.
    LT_MARC-WERKS = IT_EXCL-WERKS.
    COLLECT LT_MARC.
    CLEAR: IT_EXCL, LT_MARC.
  ENDLOOP.
  IF NOT LT_MARC[] IS INITIAL.
    SELECT MATNR
           WERKS
         FROM MARC
         INTO TABLE MT_MARC
         FOR ALL ENTRIES IN LT_MARC
         WHERE MATNR EQ LT_MARC-MATNR
         AND   WERKS EQ LT_MARC-WERKS.
    IF SY-SUBRC EQ 0.
*     SORTING
      SORT MT_MARC BY MATNR WERKS.
      LOOP AT IT_EXCL.
        L_TABIX = SY-TABIX.
        READ TABLE MT_MARC WITH KEY MATNR = IT_EXCL-MATNR
                                    WERKS = IT_EXCL-WERKS
                           BINARY SEARCH.
        IF SY-SUBRC NE 0.
          LT_EXCL = IT_EXCL.
          LT_EXCL-MSGTY = TEXT-002. "'L'.
          LT_EXCL-MESSG = TEXT-003. "'Material number does not exist'.
          DELETE IT_EXCL INDEX L_TABIX.
          APPEND LT_EXCL.
        ENDIF.
        CLEAR: IT_EXCL, LT_EXCL, MT_MARC.
      ENDLOOP.
    ELSE.
*     ALL ERROR
      LOOP AT IT_EXCL.
        L_TABIX = SY-TABIX.
        LT_EXCL = IT_EXCL.
        LT_EXCL-MSGTY = TEXT-002. "'L'.
        LT_EXCL-MESSG = TEXT-003. "'Material number does not exist'.
        DELETE IT_EXCL INDEX L_TABIX.
        APPEND LT_EXCL.
      ENDLOOP.
      REFRESH IT_EXCL. CLEAR IT_EXCL.
    ENDIF.

  ENDIF.
  DESCRIBE TABLE LT_EXCL LINES WA_ERRO_IDX.

  FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
  WRITE: / TEXT-004, "'PRODUCTION VERSION EXCEL Upload Total : ',
           WA_LINE_IDX.
  WRITE: / TEXT-005, "'Material number does not exist Error : ',
           WA_ERRO_IDX.
  FORMAT COLOR OFF.
* Material number does not exist WRITE
  IF NOT LT_EXCL[] IS INITIAL.

    WRITE: / TEXT-003. "'Material number does not exist'.
    PERFORM NOT_EXIST_MATNRIAL_WRITE1 TABLES LT_EXCL.
  ENDIF.
ENDFORM.                    " DATA_PROCESS1
*&---------------------------------------------------------------------*
*&      Form  NOT_EXIST_MATNRIAL_WRITE
*&---------------------------------------------------------------------*
FORM NOT_EXIST_MATNRIAL_WRITE TABLES PT_ALFP STRUCTURE IT_ALFP.
  WRITE: /(18)  TEXT-006,
          (05)  TEXT-007,
          (07)  TEXT-008,
          (40)  TEXT-009,
          (10)  TEXT-010,
          (10)  TEXT-011,
          (10)  TEXT-012,
          (10)  TEXT-013,
          (10)  TEXT-014,
                TEXT-015.
  LOOP AT PT_ALFP.
    WRITE: /(18) PT_ALFP-MTNO,
            (05) PT_ALFP-PLNT,
            (07) PT_ALFP-VERS,
            (40) PT_ALFP-TEXT,
            (10) PT_ALFP-FVLD,
            (10) PT_ALFP-TVLD,
            (10) PT_ALFP-ALTN,
            (10) PT_ALFP-USAG,
            (10) PT_ALFP-ZRESULT,
                 PT_ALFP-ZMSG.
  ENDLOOP.
ENDFORM.                    " NOT_EXIST_MATNRIAL_WRITE
*&---------------------------------------------------------------------*
*&      Form  NOT_EXIST_MATNRIAL_WRITE1
*&---------------------------------------------------------------------*
FORM NOT_EXIST_MATNRIAL_WRITE1 TABLES PT_EXCL STRUCTURE IT_EXCL.
*  WRITE: /(18) 'MATERIAL NUMBER',
*          (05) 'PLANT',
*          (07) 'VERSION',
*          (40) 'TEXT',
*          (10) 'FORM DATE',
*          (10) 'TO DATE',
*          (10) 'ALTER BOM',
*          (10) 'BOM USAGE',
*          (10) 'ERROR TYPE',
*               'MESSAGE TEXT'.
  WRITE: /(18)  TEXT-006,
          (05)  TEXT-007,
          (07)  TEXT-008,
          (40)  TEXT-009,
          (10)  TEXT-010,
          (10)  TEXT-011,
          (10)  TEXT-012,
          (10)  TEXT-013,
          (10)  TEXT-014,
                TEXT-015.
  LOOP AT PT_EXCL.
    WRITE: /(18) PT_EXCL-MATNR,
            (05) PT_EXCL-WERKS,
            (07) PT_EXCL-VERID,
            (40) PT_EXCL-TEXT1,
            (10) PT_EXCL-ADATU,
            (10) PT_EXCL-BDATU,
            (10) PT_EXCL-STLAL,
            (10) PT_EXCL-STLAN,
            (10) PT_EXCL-MSGTY,
                 PT_EXCL-MESSG.
  ENDLOOP.
ENDFORM.                    " NOT_EXIST_MATNRIAL_WRITE1
*&---------------------------------------------------------------------*
*&      Form  UPDATE_PROCESS
*&---------------------------------------------------------------------*
FORM UPDATE_PROCESS.
  UPDATE ZTBM_ABXLFPDT FROM TABLE IT_ALFP.
  IF SY-SUBRC EQ 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
ENDFORM.                    " UPDATE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  TITLE
*&---------------------------------------------------------------------*
FORM TITLE.
* TITLE
  WRITE: /1(130) TEXT-016 CENTERED COLOR 4 .
* BDC OPTIONS
  WA_OPT-DEFSIZE = TEXT-017.
  WA_OPT-DISMODE = TEXT-018.
  WA_OPT-UPDMODE = TEXT-019.
ENDFORM.                    " TITLE
*&---------------------------------------------------------------------*
*&      Form  RATE_ROUTING
*&---------------------------------------------------------------------*
FORM RATE_ROUTING USING    P_PLNNR
                  CHANGING Q_PLNNR.
  DATA: L_PLNNR TYPE PLKO-PLNNR,
        L_PLNTY TYPE MAPL-PLNTY,
        M_PLNNR TYPE MAPL-PLNNR,
        N_PLNNR TYPE MAPL-PLNNR,
        L_PLNAL TYPE MAPL-PLNAL,
        L_LINES TYPE SY-TABIX,
        L_TABIX TYPE SY-TABIX.
  DATA: BEGIN OF LT_PLKO OCCURS 0,
          PLNNR TYPE PLKO-PLNNR,
        END   OF LT_PLKO.
  DATA: BEGIN OF LT_MAPL OCCURS 0,
          PLNTY TYPE MAPL-PLNTY,
          PLNNR TYPE MAPL-PLNNR,
          PLNAL TYPE MAPL-PLNAL,
        END   OF LT_MAPL.
  REFRESH: IT_BDC, IT_MESS. CLEAR: IT_BDC, IT_MESS.
* READ PLKO
  SELECT SINGLE PLNNR
              FROM PLKO
              INTO L_PLNNR
              WHERE PLNTY EQ 'M'
              AND   PLNNR EQ P_PLNNR.
  IF SY-SUBRC EQ 0.
*   READ MAPL
    SELECT  PLNTY PLNNR PLNAL
         FROM MAPL
         INTO TABLE LT_MAPL
         WHERE PLNTY EQ 'R'
         AND   PLNNR LIKE '10%'.
    IF SY-SUBRC EQ 0.
      SORT LT_MAPL BY PLNNR DESCENDING.
      READ TABLE LT_MAPL INDEX 1.
      Q_PLNNR = LT_MAPL-PLNNR + 1.

    ENDIF.
*   FSC RATE ROUTION
    PERFORM FSC_RATE_ROUTION USING    IT_ALFP-MTNO
                                      IT_ALFP-PLNT
                                      Q_PLNNR
                                      L_PLNNR.
  ELSE.
*   READ PLKO
    CONCATENATE P_PLNNR '%' INTO N_PLNNR.
    SELECT PLNNR
         FROM PLKO
         INTO TABLE LT_PLKO
         WHERE PLNTY EQ 'M'.
*         AND   PLNNR LIKE N_PLNNR.
    IF SY-SUBRC EQ 0.
      LT_PLKO-PLNNR = P_PLNNR.
      APPEND LT_PLKO.
      CLEAR LT_PLKO.
      SORT LT_PLKO BY PLNNR.
      DESCRIBE TABLE LT_PLKO LINES L_LINES.
      READ TABLE LT_PLKO WITH KEY PLNNR = P_PLNNR
                         BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        L_TABIX = SY-TABIX.
        IF L_TABIX EQ L_LINES.
          L_TABIX = L_TABIX - 1.
          READ TABLE LT_PLKO INDEX L_TABIX.
          L_PLNNR = LT_PLKO-PLNNR.
        ELSE.
          L_TABIX = L_TABIX + 1.
          READ TABLE LT_PLKO INDEX L_TABIX.
          L_PLNNR = LT_PLKO-PLNNR.
        ENDIF.
*       REFERENCE RATE ROUTION CREATE
        PERFORM REFERENCE_RATE_ROUTION_CREATE USING    P_PLNNR
                                                       L_PLNNR.

*       READ MAPL
        SELECT  PLNTY PLNNR PLNAL
             FROM MAPL
             INTO TABLE LT_MAPL
             WHERE PLNTY EQ 'R'
             AND   PLNNR LIKE '10%'.
        IF SY-SUBRC EQ 0.
          SORT LT_MAPL BY PLNNR DESCENDING.
          READ TABLE LT_MAPL INDEX 1.
          Q_PLNNR = LT_MAPL-PLNNR + 1.
        ENDIF.
*       FSC RATE ROUTION
        PERFORM FSC_RATE_ROUTION USING    IT_ALFP-MTNO
                                          IT_ALFP-PLNT
                                          Q_PLNNR
                                          P_PLNNR.
      ENDIF.

    ENDIF.

  ENDIF.
ENDFORM.                    " RATE_ROUTING
*&---------------------------------------------------------------------*
*&      Form  RATE_ROUTING1
*&---------------------------------------------------------------------*
FORM RATE_ROUTING1 USING    P_PLNNR
                   CHANGING Q_PLNNR.
  DATA: L_PLNNR TYPE PLKO-PLNNR,
        L_PLNTY TYPE MAPL-PLNTY,
        M_PLNNR TYPE MAPL-PLNNR,
        N_PLNNR TYPE MAPL-PLNNR,
        L_PLNAL TYPE MAPL-PLNAL,
        L_LINES TYPE SY-TABIX,
        L_TABIX TYPE SY-TABIX.
  DATA: BEGIN OF LT_PLKO OCCURS 0,
          PLNNR TYPE PLKO-PLNNR,
        END   OF LT_PLKO.
  DATA: BEGIN OF LT_MAPL OCCURS 0,
          PLNTY TYPE MAPL-PLNTY,
          PLNNR TYPE MAPL-PLNNR,
          PLNAL TYPE MAPL-PLNAL,
        END   OF LT_MAPL.
* READ PLKO
  SELECT SINGLE PLNNR
              FROM PLKO
              INTO L_PLNNR
              WHERE PLNTY EQ 'M'
              AND   PLNNR EQ P_PLNNR.
  IF SY-SUBRC EQ 0.
*   READ MAPL
    SELECT  PLNTY PLNNR PLNAL
         FROM MAPL
         INTO TABLE LT_MAPL
         WHERE PLNTY EQ 'R'
         AND   PLNNR LIKE '10%'.
    IF SY-SUBRC EQ 0.
      SORT LT_MAPL BY PLNNR DESCENDING.
      READ TABLE LT_MAPL INDEX 1.
      Q_PLNNR = LT_MAPL-PLNNR + 1.

    ENDIF.
*   FSC RATE ROUTION
    PERFORM FSC_RATE_ROUTION1 USING    IT_EXCL-MATNR
                                       IT_EXCL-WERKS
                                       Q_PLNNR
                                       L_PLNNR.
  ELSE.
*   READ PLKO
    CONCATENATE P_PLNNR '%' INTO N_PLNNR.
    SELECT PLNNR
         FROM PLKO
         INTO TABLE LT_PLKO
         WHERE PLNTY EQ 'M'
         AND   PLNNR LIKE N_PLNNR.
    IF SY-SUBRC EQ 0.
      LT_PLKO-PLNNR = P_PLNNR.
      APPEND LT_PLKO.
      CLEAR LT_PLKO.
      SORT LT_PLKO BY PLNNR.
      DESCRIBE TABLE LT_PLKO LINES L_LINES.
      READ TABLE LT_PLKO WITH KEY PLNNR = P_PLNNR
                         BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        L_TABIX = SY-TABIX.
        IF L_TABIX EQ L_LINES.
          L_TABIX = L_TABIX - 1.
          READ TABLE LT_PLKO INDEX L_TABIX.
          L_PLNNR = LT_PLKO-PLNNR.
        ELSE.
          L_TABIX = L_TABIX + 1.
          READ TABLE LT_PLKO INDEX L_TABIX.
          L_PLNNR = LT_PLKO-PLNNR.
        ENDIF.
*       REFERENCE RATE ROUTION CREATE
        PERFORM REFERENCE_RATE_ROUTION_CREATE USING    P_PLNNR
                                                       L_PLNNR.

*       READ MAPL
        SELECT  PLNTY PLNNR PLNAL
             FROM MAPL
             INTO TABLE LT_MAPL
             WHERE PLNTY EQ 'R'
             AND   PLNNR LIKE '10%'.
        IF SY-SUBRC EQ 0.
          SORT LT_MAPL BY PLNNR DESCENDING.
          READ TABLE LT_MAPL INDEX 1.
          Q_PLNNR = LT_MAPL-PLNNR + 1.
        ENDIF.
*       FSC RATE ROUTION
        PERFORM FSC_RATE_ROUTION1 USING    IT_EXCL-MATNR
                                           IT_EXCL-WERKS
                                           Q_PLNNR
                                           P_PLNNR.
      ENDIF.

    ENDIF.

  ENDIF.
ENDFORM.                    " RATE_ROUTING1
*&---------------------------------------------------------------------*
*&      Form  REFERENCE_RATE_ROUTION_CREATE
*&---------------------------------------------------------------------*
FORM REFERENCE_RATE_ROUTION_CREATE USING    P_PLNNR
                                            Q_PLNNR.
  DATA: L_DATUM(10).
  REFRESH: IT_BDC, IT_MESS. CLEAR: IT_BDC, IT_MESS.
  WRITE: SY-DATUM TO L_DATUM.
  PERFORM DYNPRO USING:
     'X' 'SAPLCPDI'    '1001',
     ' ' 'RC271-PLNNR' P_PLNNR,   "
     ' ' 'RC271-STTAG' L_DATUM,  "
     ' ' 'BDC_OKCODE'  '=COPY',

     'X' 'SAPLCPCO'    '0101',
     ' ' 'TYP(01)'     'X',   "
     ' ' 'BDC_OKCODE'  '=CONT',

     'X' 'SAPLCPCO'    '1020',
     ' ' 'RC271-PLNNR' Q_PLNNR,   "
     ' ' 'BDC_OKCODE'  '=CONT',

     'X' 'SAPLCPDA'    '1200',
     ' ' 'PLKOD-STATU' '4',   "
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCPDI'    '5400',
     ' ' 'BDC_OKCODE'  '=MAAL',

     'X' 'SAPLCPDI'    '5400',
     ' ' 'BDC_OKCODE'  '=BU'.
  CALL TRANSACTION 'CA31'  USING IT_BDC
                  OPTIONS FROM WA_OPT
                  MESSAGES INTO IT_MESS.
  REFRESH: IT_BDC, IT_MESS. CLEAR: IT_BDC, IT_MESS.
ENDFORM.                    " REFERENCE_RATE_ROUTION_CREATE
*&---------------------------------------------------------------------*
*&      Form  FSC_RATE_ROUTION
*&---------------------------------------------------------------------*
FORM FSC_RATE_ROUTION USING    P_MATNR
                               P_WERKS
                               P_PLNNR
                               Q_PLNNR.
  DATA: L_DATUM(10).
  REFRESH: IT_BDC, IT_MESS. CLEAR: IT_BDC, IT_MESS.
  WRITE: SY-DATUM TO L_DATUM.
  PERFORM DYNPRO USING:
     'X' 'SAPLCPDI'    '1010',
     ' ' 'RC27M-MATNR' P_MATNR,   "
     ' ' 'RC27M-WERKS' P_WERKS,   "
     ' ' 'RC271-PLNNR' P_PLNNR,  "
     ' ' 'RC271-STTAG' L_DATUM, "
     ' ' 'BDC_OKCODE'  '=COPY',

     'X' 'SAPLCPCO'    '0101',
     ' ' 'TYP(02)'     'X',   "
     ' ' 'BDC_OKCODE'  '=CONT',

     'X' 'SAPLCPCO'    '1020',
     ' ' 'RC271-PLNNR' Q_PLNNR,   "
     ' ' 'BDC_OKCODE'  '=CONT',

     'X' 'SAPLCPDA'    '1200',
     ' ' 'PLKOD-STATU' '4',   "
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCPDI'    '5400',
     ' ' 'BDC_OKCODE'  '=MAAL',

     'X' 'SAPLCPDI'    '5400',
     ' ' 'BDC_OKCODE'  '=BU'.
  CALL TRANSACTION 'CA21'  USING IT_BDC
                  OPTIONS FROM WA_OPT
                  MESSAGES INTO IT_MESS.
  REFRESH: IT_BDC, IT_MESS. CLEAR: IT_BDC, IT_MESS.
ENDFORM.                    " FSC_RATE_ROUTION
*&---------------------------------------------------------------------*
*&      Form  FSC_RATE_ROUTION1
*&---------------------------------------------------------------------*
FORM FSC_RATE_ROUTION1 USING    P_MATNR
                                P_WERKS
                                P_PLNNR
                                Q_PLNNR.
  DATA: L_DATUM(10).
  REFRESH: IT_BDC, IT_MESS. CLEAR: IT_BDC, IT_MESS.
  WRITE: SY-DATUM TO L_DATUM.
  PERFORM DYNPRO USING:
     'X' 'SAPLCPDI'    '1010',
     ' ' 'RC27M-MATNR' P_MATNR,   "
     ' ' 'RC27M-WERKS' P_WERKS,   "
     ' ' 'RC271-PLNNR' P_PLNNR,  "
     ' ' 'RC271-STTAG' L_DATUM, "
     ' ' 'BDC_OKCODE'  '=COPY',

     'X' 'SAPLCPCO'    '0101',
     ' ' 'TYP(02)'     'X',   "
     ' ' 'BDC_OKCODE'  '=CONT',

     'X' 'SAPLCPCO'    '1020',
     ' ' 'RC271-PLNNR' Q_PLNNR,   "
     ' ' 'BDC_OKCODE'  '=CONT',

     'X' 'SAPLCPDA'    '1200',
     ' ' 'PLKOD-STATU' '4',   "
     ' ' 'BDC_OKCODE'  '/00',

     'X' 'SAPLCPDI'    '5400',
     ' ' 'BDC_OKCODE'  '=MAAL',

     'X' 'SAPLCPDI'    '5400',
     ' ' 'BDC_OKCODE'  '=BU'.
  CALL TRANSACTION 'CA21'  USING IT_BDC
                  OPTIONS FROM WA_OPT
                  MESSAGES INTO IT_MESS.
ENDFORM.                    " FSC_RATE_ROUTION1
