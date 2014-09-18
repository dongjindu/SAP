*----------------------------------------------------------------------*
*   INCLUDE ZIPP308L_OBJ_DEP_BDC_F01                                   *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
FORM INITIALIZATION.
  REFRESH: IT_BDC, IT_MESS.
  CLEAR:   IT_BDC, IT_MESS.
* BDC TYPE
  WA_OPT-DEFSIZE = 'X'.
  WA_OPT-DISMODE = 'N'.
  WA_OPT-UPDMODE = 'S'.
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
       FROM ZTBM_ABXODPDT
       INTO TABLE IT_AODP
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
  DATA: LT_AODP LIKE IT_AODP OCCURS 0 WITH HEADER LINE.
* DATA TOTAL LINES
  DESCRIBE TABLE IT_AODP LINES WA_LINE_IDX.

  DESCRIBE TABLE LT_AODP LINES WA_ERRO_IDX.

  FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
  WRITE: / TEXT-008, WA_LINE_IDX.
  WRITE: / TEXT-009, WA_ERRO_IDX.
  FORMAT COLOR OFF.
  IF NOT LT_AODP[] IS INITIAL.
    WRITE: / TEXT-010.
    PERFORM WRITE_ERROR TABLES    LT_AODP
                        USING     'L'.

  ENDIF.
ENDFORM.                    " DATA_PROCESS
*&---------------------------------------------------------------------*
*&      Form  BDC_PROCESS
*&---------------------------------------------------------------------*
FORM BDC_PROCESS.
  DATA: L_NCHR(10),
        L_TABIX TYPE SY-TABIX,
        L_MSG  LIKE CFGNL-MSGLIN,
        L_MSGTY TYPE SY-MSGTY.
  DATA: L_KNNAM TYPE CUKB-KNNAM.

  LOOP AT IT_AODP.
    L_TABIX = SY-TABIX.
    PERFORM DYNPRO USING:
       'X' 'SAPLCUKD'    '0100',
       ' ' 'RCUKD-KNNAM' IT_AODP-DPID,    "DEPENDENCY
       ' ' 'RCUKD-AENNR' IT_AODP-EONO,    "ECM NO
       ' ' 'BDC_OKCODE'  '/00',

       'X' 'SAPLCUKD'    '0110',
       ' ' 'RCUKD-KNKTX' IT_AODP-DDES,    "DEPENDENCY DESCRIPTION
       ' ' 'RCUKD-KNGRP' 'COLOR',    "DEPENDENCY GROUP
       ' ' 'RCUKD-KNABD' 'X',    "Selection condition
*         ' ' 'RCUKD-KNSTA' IT_AODP-STAT,    "STATUS
       ' ' 'BDC_OKCODE'  '=KNED',

       'X' 'SAPLEDITOR_START'    '2210',
       ' ' 'BDC_CURSOR'          'RSTXP-TDLINECOM(01)',
       ' ' 'RSTXP-TDLINECOM(01)' IT_AODP-LIN1, "EDIC: Line command
       ' ' 'RSTXP-TDLINE(01)'    IT_AODP-DPC1, "EDIC: Program editorline
       ' ' 'BDC_OKCODE'  '=ACIL',

       'X' 'SAPLEDITOR_START'    '2210',
       ' ' 'BDC_CURSOR'          'RSTXP-TDLINECOM(02)',
       ' ' 'RSTXP-TDLINECOM(02)' IT_AODP-LIN2, "EDIC: Line command
       ' ' 'RSTXP-TDLINE(02)'    IT_AODP-DPC2, "EDIC: Program editorline
       ' ' 'BDC_OKCODE'  '=ACIL',

       'X' 'SAPLEDITOR_START'    '2210',
       ' ' 'BDC_CURSOR'          'RSTXP-TDLINECOM(02)',
       ' ' 'RSTXP-TDLINECOM(02)' IT_AODP-LIN3, "EDIC: Line command
       ' ' 'RSTXP-TDLINE(02)'    IT_AODP-DPC3, "EDIC: Program editorline
       ' ' 'BDC_OKCODE'  '=ACIL',

       'X' 'SAPLEDITOR_START'    '2210',
       ' ' 'BDC_CURSOR'          'RSTXP-TDLINECOM(02)',
       ' ' 'RSTXP-TDLINECOM(02)' IT_AODP-LIN4, "EDIC: Line command
       ' ' 'RSTXP-TDLINE(02)'    IT_AODP-DPC4, "EDIC: Program editorline
       ' ' 'BDC_OKCODE'  '=ACIL',

       'X' 'SAPLEDITOR_START'    '2210',
       ' ' 'BDC_CURSOR'          'RSTXP-TDLINECOM(02)',
       ' ' 'RSTXP-TDLINECOM(02)' IT_AODP-LIN5, "EDIC: Line command
       ' ' 'RSTXP-TDLINE(02)'    IT_AODP-DPC5, "EDIC: Program editorline
       ' ' 'BDC_OKCODE'  '=ACIL',

       'X' 'SAPLEDITOR_START'    '2210',
       ' ' 'BDC_CURSOR'          'RSTXP-TDLINECOM(02)',
       ' ' 'RSTXP-TDLINECOM(02)' IT_AODP-LIN6, "EDIC: Line command
       ' ' 'RSTXP-TDLINE(02)'    IT_AODP-DPC6, "EDIC: Program editorline
       ' ' 'BDC_OKCODE'  '=VAPR',

       'X' 'SAPLEDITOR_START'    '2210',
       ' ' 'BDC_OKCODE'  '=VASV',

       'X' 'SAPLCUKD'    '0110',
       ' ' 'BDC_OKCODE'  '=SICH'.

    CALL TRANSACTION 'CU01'  USING IT_BDC
                             OPTIONS FROM WA_OPT
                             MESSAGES INTO IT_MESS.

    READ TABLE IT_MESS WITH KEY MSGID = '28'
                                MSGNR = '000'.
    IF SY-SUBRC EQ 0.
      L_MSGTY = 'S'.
      REFRESH: IT_BDC, IT_MESS.
      PERFORM DYNPRO USING:
         'X' 'SAPLCUKD'    '0100',
         ' ' 'RCUKD-KNNAM' IT_AODP-DPID,    "DEPENDENCY
         ' ' 'RCUKD-AENNR' IT_AODP-EONO,    "ECM NO
         ' ' 'BDC_OKCODE'  '/00',

         'X' 'SAPLCUKD'    '0110',
         ' ' 'RCUKD-KNSTA' IT_AODP-STAT,    "STATUS
         ' ' 'BDC_OKCODE'  '=SICH'.

    ELSE.
      REFRESH: IT_BDC, IT_MESS.
      L_MSGTY = 'E'.

      PERFORM DYNPRO USING:
         'X' 'SAPLCUKD'    '0100',
         ' ' 'RCUKD-KNNAM' IT_EXCL-DPID,    "DEPENDENCY
         ' ' 'RCUKD-AENNR' IT_EXCL-EONO,    "ECM NO
         ' ' 'BDC_OKCODE'  '=DELB',

         'X' 'SAPLCUKD'    '0220',
         ' ' 'BDC_OKCODE'  '=DELD'.
    ENDIF.
    CALL TRANSACTION 'CU02'  USING IT_BDC
                             OPTIONS FROM WA_OPT
                             MESSAGES INTO IT_MESS.

    PERFORM RKC_MSG_STRING CHANGING L_MSG.

*   MODIFY IT_AODP
    PERFORM IT_AODP_MODIFY USING L_MSG
                                 L_MSGTY
                                 L_TABIX.
    REFRESH: IT_BDC, IT_MESS.
  ENDLOOP.

ENDFORM.                    " BDC_PROCESS
*&---------------------------------------------------------------------*
*&      Form  BDC_PROCESS_01
*&---------------------------------------------------------------------*
FORM BDC_PROCESS_01.
  DATA: L_CHK(1),
        L_TABIX TYPE SY-TABIX,
        L_MSGTY TYPE SY-MSGTY,
        L_MSG  LIKE CFGNL-MSGLIN.
* DATA TOTAL LINES
  DESCRIBE TABLE IT_EXCL LINES WA_LINE_IDX.
  LOOP AT IT_EXCL.
    L_TABIX = SY-TABIX.
    PERFORM DYNPRO USING:
       'X' 'SAPLCUKD'    '0100',
       ' ' 'RCUKD-KNNAM' IT_EXCL-DPID,    "DEPENDENCY
       ' ' 'RCUKD-AENNR' IT_EXCL-EONO,    "ECM NO
       ' ' 'BDC_OKCODE'  '/00',

       'X' 'SAPLCUKD'    '0110',
       ' ' 'RCUKD-KNKTX' IT_EXCL-DDES,    "DEPENDENCY DESCRIPTION
       ' ' 'RCUKD-KNGRP' 'COLOR',    "DEPENDENCY GROUP
       ' ' 'RCUKD-KNABD' 'X',    "Selection condition
*         ' ' 'RCUKD-KNSTA' IT_EXCL-STAT,    "STATUS
       ' ' 'BDC_OKCODE'  '=KNED',

       'X' 'SAPLEDITOR_START'    '2210',
       ' ' 'BDC_CURSOR'          'RSTXP-TDLINECOM(01)',
       ' ' 'RSTXP-TDLINECOM(01)' IT_EXCL-LIN1, "EDIC: Line command
       ' ' 'RSTXP-TDLINE(01)'    IT_EXCL-DPC1, "EDIC: Program editorline
       ' ' 'BDC_OKCODE'  '=ACIL',

       'X' 'SAPLEDITOR_START'    '2210',
       ' ' 'BDC_CURSOR'          'RSTXP-TDLINECOM(02)',
       ' ' 'RSTXP-TDLINECOM(02)' IT_EXCL-LIN2, "EDIC: Line command
       ' ' 'RSTXP-TDLINE(02)'    IT_EXCL-DPC2, "EDIC: Program editorline
       ' ' 'BDC_OKCODE'  '=ACIL',

       'X' 'SAPLEDITOR_START'    '2210',
       ' ' 'BDC_CURSOR'          'RSTXP-TDLINECOM(02)',
       ' ' 'RSTXP-TDLINECOM(02)' IT_EXCL-LIN3, "EDIC: Line command
       ' ' 'RSTXP-TDLINE(02)'    IT_EXCL-DPC3, "EDIC: Program editorline
       ' ' 'BDC_OKCODE'  '=ACIL',

       'X' 'SAPLEDITOR_START'    '2210',
       ' ' 'BDC_CURSOR'          'RSTXP-TDLINECOM(02)',
       ' ' 'RSTXP-TDLINECOM(02)' IT_EXCL-LIN4, "EDIC: Line command
       ' ' 'RSTXP-TDLINE(02)'    IT_EXCL-DPC4, "EDIC: Program editorline
       ' ' 'BDC_OKCODE'  '=ACIL',

       'X' 'SAPLEDITOR_START'    '2210',
       ' ' 'BDC_CURSOR'          'RSTXP-TDLINECOM(02)',
       ' ' 'RSTXP-TDLINECOM(02)' IT_EXCL-LIN5, "EDIC: Line command
       ' ' 'RSTXP-TDLINE(02)'    IT_EXCL-DPC5, "EDIC: Program editorline
       ' ' 'BDC_OKCODE'  '=ACIL',

       'X' 'SAPLEDITOR_START'    '2210',
       ' ' 'BDC_CURSOR'          'RSTXP-TDLINECOM(02)',
       ' ' 'RSTXP-TDLINECOM(02)' IT_EXCL-LIN6, "EDIC: Line command
       ' ' 'RSTXP-TDLINE(02)'    IT_EXCL-DPC6, "EDIC: Program editorline
       ' ' 'BDC_OKCODE'  '=VAPR',

       'X' 'SAPLEDITOR_START'    '2210',
       ' ' 'BDC_OKCODE'  '=VASV',

       'X' 'SAPLCUKD'    '0110',
       ' ' 'BDC_OKCODE'  '=SICH'.

    CALL TRANSACTION 'CU01'  USING IT_BDC
                             OPTIONS FROM WA_OPT
                             MESSAGES INTO IT_MESS.
    READ TABLE IT_MESS WITH KEY MSGID = '28'
                                MSGNR = '000'.
    IF SY-SUBRC EQ 0.
      REFRESH: IT_BDC, IT_MESS.
      PERFORM RKC_MSG_STRING CHANGING IT_EXCL-ZMSG.
      IT_EXCL-ZRESULT = 'S'.
      PERFORM DYNPRO USING:
         'X' 'SAPLCUKD'    '0100',
         ' ' 'RCUKD-KNNAM' IT_EXCL-DPID,    "DEPENDENCY
         ' ' 'RCUKD-AENNR' IT_EXCL-EONO,    "ECM NO
         ' ' 'BDC_OKCODE'  '/00',

         'X' 'SAPLCUKD'    '0110',
         ' ' 'RCUKD-KNSTA' IT_EXCL-STAT,    "STATUS
         ' ' 'BDC_OKCODE'  '=SICH'.

    ELSE.
      REFRESH: IT_BDC, IT_MESS.
      IT_EXCL-ZRESULT = 'E'.
      PERFORM RKC_MSG_STRING CHANGING IT_EXCL-ZMSG.
      PERFORM DYNPRO USING:
          'X' 'SAPLCUKD'    '0100',
          ' ' 'RCUKD-KNNAM' IT_EXCL-DPID,    "DEPENDENCY
          ' ' 'RCUKD-AENNR' IT_EXCL-EONO,    "ECM NO
          ' ' 'BDC_OKCODE'  '=DELB',

          'X' 'SAPLCUKD'    '0220',
          ' ' 'BDC_OKCODE'  '=DELD'.

    ENDIF.
    CALL TRANSACTION 'CU02'  USING IT_BDC
                             OPTIONS FROM WA_OPT
                             MESSAGES INTO IT_MESS.
*   MODIFY IT_EXCL
    MODIFY IT_EXCL INDEX L_TABIX TRANSPORTING ZRESULT
                                              ZMSG.

    CLEAR: L_CHK.
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
*&      Form  IT_AODP_MODIFY
*&---------------------------------------------------------------------*
FORM IT_AODP_MODIFY USING P_MSG
                          P_MSGTY
                          P_TABIX.

  IT_AODP-ZMSG    = P_MSG.
  IT_AODP-ZRESULT = P_MSGTY.
  CASE P_MSGTY.
    WHEN 'S'.
      IT_AODP-ZBDAT = SY-DATUM.  "SAP BDC EXECUTED DATE
      IT_AODP-ZBTIM = SY-UZEIT.  "SAP BDC EXECUTED TIME
      IT_AODP-ZBNAM = SY-UNAME.  "BDC User ID
*     C: Create U: Update D: Delete
      IT_AODP-ZMODE = 'C'.       "C:CREATE
    WHEN 'E'.
      IT_AODP-ZBDAT = SY-DATUM.  "SAP BDC EXECUTED DATE
*      IT_AODP-ZBTIM = SY-UZEIT.  "SAP BDC EXECUTED TIME
      IT_AODP-ZBNAM = SY-UNAME.  "BDC User ID
      IT_AODP-ZMODE = 'C'.       "C:CREATE
    WHEN OTHERS.
      IT_AODP-ZBDAT = SY-DATUM.  "SAP BDC EXECUTED DATE
      IT_AODP-ZBTIM = SY-UZEIT.  "SAP BDC EXECUTED TIME
      IT_AODP-ZBNAM = SY-UNAME.  "BDC User ID
      IT_AODP-ZMODE = 'C'.       "C:CREATE
  ENDCASE.
* IT_AODP MODIFY
  MODIFY IT_AODP INDEX P_TABIX TRANSPORTING ZBDAT
                                            ZBTIM
                                            ZBNAM
                                            ZMODE
                                            ZRESULT
                                            ZMSG.
ENDFORM.                    " IT_AODP_MODIFY
*&---------------------------------------------------------------------*
*&      Form  UPDATE_PROCESS
*&---------------------------------------------------------------------*
FORM UPDATE_PROCESS.
* ZTBM_ABXODPDT TABLE UPDATE
  PERFORM UPDATE_ZTBM_ABXODPDT.
ENDFORM.                    " UPDATE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  UPDATE_ZTBM_ABXODPDT
*&---------------------------------------------------------------------*
FORM UPDATE_ZTBM_ABXODPDT.
  UPDATE ZTBM_ABXODPDT FROM TABLE IT_AODP.
  IF SY-SUBRC EQ 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
ENDFORM.                    " UPDATE_ZTBM_ABXODPDT
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
  CLEAR: WA_ERRO_IDX, WA_LINE_IDX.
*  DESCRIBE TABLE IT_AODP LINES WA_LINE_IDX.
  LOOP AT IT_AODP.
    IF IT_AODP-ZRESULT EQ 'E'.
      WA_ERRO_IDX = WA_ERRO_IDX + 1.
    ELSE.
      WA_LINE_IDX = WA_LINE_IDX + 1.
    ENDIF.
  ENDLOOP.

  FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
  WRITE: / TEXT-031, WA_LINE_IDX.
  WRITE: / TEXT-032, WA_ERRO_IDX.
  FORMAT COLOR OFF.
  IF WA_ERRO_IDX GE 1.
    FORMAT COLOR COL_HEADING INTENSIFIED OFF.
    WRITE: / TEXT-032.
    FORMAT COLOR OFF.
    PERFORM WRITE_ERROR TABLES    IT_AODP
                        USING     'E'.
  ENDIF.
ENDFORM.                    " WRITE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  WRITE_PROCESS_01
*&---------------------------------------------------------------------*
FORM WRITE_PROCESS_01.
  DATA L_INDEX TYPE SY-INDEX.

  CLEAR: WA_ERRO_IDX, WA_LINE_IDX.
*  DESCRIBE TABLE IT_EXCL LINES WA_LINE_IDX.
  LOOP AT IT_EXCL.
    IF IT_EXCL-ZRESULT EQ 'E'.
      WA_ERRO_IDX = WA_ERRO_IDX + 1.
    ELSE.
      WA_LINE_IDX = WA_LINE_IDX + 1.
    ENDIF.
  ENDLOOP.

  FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
  WRITE: / TEXT-031, WA_LINE_IDX.
  WRITE: / TEXT-032, WA_ERRO_IDX.
  FORMAT COLOR OFF.
  IF WA_ERRO_IDX GE 1.
    FORMAT COLOR COL_HEADING INTENSIFIED OFF.
    WRITE: / TEXT-032.
    FORMAT COLOR OFF.
    PERFORM WRITE_ERROR1 TABLES   IT_EXCL
                         USING    'E'.
  ENDIF.
ENDFORM.                    " WRITE_PROCESS_01
*&---------------------------------------------------------------------*
*&      Form  WRITE_ERROR
*&---------------------------------------------------------------------*
FORM WRITE_ERROR TABLES   PT_AODP STRUCTURE IT_AODP
                 USING    P_ZRESULT.

  WRITE: /(18)  TEXT-011,
          (12)  TEXT-012,
          (30)  TEXT-013,
          (10)  TEXT-014,
          (06)  TEXT-015,
          (18)  TEXT-016,
          (72)  TEXT-017,
          (18)  TEXT-018,
          (72)  TEXT-019,
          (18)  TEXT-020,
          (72)  TEXT-021,
          (18)  TEXT-022,
          (72)  TEXT-023,
          (18)  TEXT-024,
          (72)  TEXT-025,
          (18)  TEXT-026,
          (72)  TEXT-027,
          (12)  TEXT-028,
          (10)  TEXT-029,
          (100)  TEXT-030.
  LOOP AT PT_AODP WHERE ZRESULT EQ P_ZRESULT.
    WRITE: /(18) PT_AODP-DPID,
            (12) PT_AODP-EONO,
            (30) PT_AODP-DDES,
            (10) PT_AODP-VALD,
            (06) PT_AODP-STAT,
            (18) PT_AODP-LIN1,
            (72) PT_AODP-DPC1,
            (18) PT_AODP-LIN2,
            (72) PT_AODP-DPC2,
            (18) PT_AODP-LIN3,
            (72) PT_AODP-DPC3,
            (18) PT_AODP-LIN4,
            (72) PT_AODP-DPC4,
            (18) PT_AODP-LIN5,
            (72) PT_AODP-DPC5,
            (18) PT_AODP-LIN6,
            (72) PT_AODP-DPC6,
            (12) PT_AODP-SYNT,
            (10) PT_AODP-ZRESULT,
            (100) PT_AODP-ZMSG.
  ENDLOOP.
ENDFORM.                    " WRITE_ERROR
*&---------------------------------------------------------------------*
*&      Form  WRITE_ERROR1
*&---------------------------------------------------------------------*
FORM WRITE_ERROR1 TABLES   PT_EXCL STRUCTURE IT_EXCL
                  USING    P_ZRESULT.

  WRITE: /(18)  TEXT-011,
          (12)  TEXT-012,
          (30)  TEXT-013,
          (10)  TEXT-014,
          (06)  TEXT-015,
          (18)  TEXT-016,
          (72)  TEXT-017,
          (18)  TEXT-018,
          (72)  TEXT-019,
          (18)  TEXT-020,
          (72)  TEXT-021,
          (18)  TEXT-022,
          (72)  TEXT-023,
          (18)  TEXT-024,
          (72)  TEXT-025,
          (18)  TEXT-026,
          (72)  TEXT-027,
          (12)  TEXT-028,
          (10)  TEXT-029,
          (100)  TEXT-030.
  LOOP AT PT_EXCL WHERE ZRESULT EQ P_ZRESULT.
    WRITE: /(18) PT_EXCL-DPID,
            (12) PT_EXCL-EONO,
            (30) PT_EXCL-DDES,
            (10) PT_EXCL-VALD,
            (06) PT_EXCL-STAT,
            (18) PT_EXCL-LIN1,
            (72) PT_EXCL-DPC1,
            (18) PT_EXCL-LIN2,
            (72) PT_EXCL-DPC2,
            (18) PT_EXCL-LIN3,
            (72) PT_EXCL-DPC3,
            (18) PT_EXCL-LIN4,
            (72) PT_EXCL-DPC4,
            (18) PT_EXCL-LIN5,
            (72) PT_EXCL-DPC5,
            (18) PT_EXCL-LIN6,
            (72) PT_EXCL-DPC6,
            (12) PT_EXCL-SYNT,
            (10) PT_EXCL-ZRESULT,
            (100) PT_EXCL-ZMSG.
  ENDLOOP.
ENDFORM.                    " WRITE_ERROR1
