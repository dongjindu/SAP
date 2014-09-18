*----------------------------------------------------------------------*
*   INCLUDE ZIPP309L_CONFI_PROF_BDC_F01                                *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
FORM INITIALIZATION.
  REFRESH: IT_BDC, IT_MESS.
  CLEAR:   IT_BDC, IT_MESS.
  WA_OPT-DEFSIZE = 'X'.
  WA_OPT-DISMODE = 'N'.
  WA_OPT-UPDMODE = 'S'.

ENDFORM.                               " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  AT_SEL_SCREEN_ON_VALUE_REQUEST
*&---------------------------------------------------------------------*
FORM AT_SEL_SCREEN_ON_VALUE_REQUEST USING DEF_PATH LIKE RLGRAP-FILENAME
                                          MODE     TYPE C.

ENDFORM.                               " AT_SEL_SCREEN_ON_VALUE_REQUEST
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_PROCESS
*&---------------------------------------------------------------------*
FORM UPLOAD_PROCESS.
ENDFORM.                               " UPLOAD_PROCESS
*&---------------------------------------------------------------------*
*&      Form  BDC_PROCESS
*&---------------------------------------------------------------------*
FORM BDC_PROCESS.
  DATA: L_TABIX TYPE SY-TABIX.
  PERFORM GET_PARAMETER_ID.
* DATA TOTAL LINES
  DESCRIBE TABLE IT_ACFI LINES WA_LINE_IDX.
* T_CODE : CU41 BDC
* by IG.MOON 8/24/2007 {
  PERFORM CU41_BDC_PROCESS_NEW.
*
* T_CODE : MM02-MRP4 VIEW CONFIGURATION  BDC
*  PERFORM MM02_BDC_PROCESS.

  PERFORM MM02_BDC_COLOR_EXT_INT.
ENDFORM.                    " BDC_PROCESS
*&---------------------------------------------------------------------*
*&      Form  BDC_PROCESS1
*&---------------------------------------------------------------------*
FORM BDC_PROCESS1.
  DATA: L_TABIX TYPE SY-TABIX.
* DATA TOTAL LINES
  DESCRIBE TABLE IT_EXCL LINES WA_LINE_IDX.
* T_CODE : CU41 BDC
  PERFORM CU41_EXCEL_BDC_PROCESS.
* T_CODE : MM02-MRP3 VIEW CONFIGURATION  BDC
  PERFORM MM02_EXCEL_BDC_PROCESS.

  PERFORM MM02_EXCEL_BDC_COLOR_EXT_INT.
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
  DESCRIBE TABLE IT_ACFI LINES WA_LINE_IDX.
  LOOP AT IT_ACFI WHERE ZRESULT EQ 'E'.
    WA_ERRO_IDX = WA_ERRO_IDX + 1.
  ENDLOOP.
  WA_LINE_IDX = WA_LINE_IDX - WA_ERRO_IDX.
  FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
  WRITE: / TEXT-005, WA_LINE_IDX.
  WRITE: / TEXT-006, WA_ERRO_IDX.
  FORMAT COLOR OFF.
  IF WA_ERRO_IDX GE 1.
    FORMAT COLOR COL_HEADING INTENSIFIED OFF.
    WRITE: / TEXT-007.
    FORMAT COLOR OFF.
    WRITE: /(18)  TEXT-008,
            (06)  TEXT-009,
            (18)  TEXT-010,
            (12)  TEXT-011,
            (10)  TEXT-012,
            (30)  TEXT-013,
            (15)  TEXT-014,
            (06)  TEXT-015,
            (10)  TEXT-016,
            (10)  TEXT-017,
                  TEXT-018.
    LOOP AT IT_ACFI WHERE ZRESULT EQ 'E'.
      WRITE: /(18) IT_ACFI-MTNO,
              (06) IT_ACFI-PLNT,
              (18) IT_ACFI-CLID,
              (12) IT_ACFI-EONO,
              (10) IT_ACFI-PRIT,
              (30) IT_ACFI-PROF,
              (15) IT_ACFI-CLTY,
              (06) IT_ACFI-STAT,
              (10) IT_ACFI-GEFT,
              (10) IT_ACFI-ZRESULT,
                   IT_ACFI-ZMSG.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " WRITE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  WRITE_PROCESS1
*&---------------------------------------------------------------------*
FORM WRITE_PROCESS1.
  DATA L_INDEX TYPE SY-INDEX.

  CLEAR: WA_ERRO_IDX, WA_LINE_IDX.
  DESCRIBE TABLE IT_EXCL LINES WA_LINE_IDX.
  LOOP AT IT_EXCL WHERE ZRESULT EQ 'E'.
    WA_ERRO_IDX = WA_ERRO_IDX + 1.
  ENDLOOP.
  WA_LINE_IDX = WA_LINE_IDX - WA_ERRO_IDX.
  FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
  WRITE: / TEXT-005, WA_LINE_IDX.
  WRITE: / TEXT-006, WA_ERRO_IDX.
  FORMAT COLOR OFF.
  IF WA_ERRO_IDX GE 1.
    FORMAT COLOR COL_HEADING INTENSIFIED OFF.
    WRITE: / TEXT-007.
    FORMAT COLOR OFF.
    WRITE: /(18)  TEXT-008,
            (06)  TEXT-009,
            (18)  TEXT-010,
            (12)  TEXT-011,
            (10)  TEXT-012,
            (30)  TEXT-013,
            (15)  TEXT-014,
            (06)  TEXT-015,
            (10)  TEXT-016,
            (10)  TEXT-017,
                  TEXT-018.
    LOOP AT IT_EXCL WHERE ZRESULT EQ 'E'.
      WRITE: /(18) IT_EXCL-MTNO,
              (06) IT_EXCL-PLNT,
              (18) IT_EXCL-CLID,
              (12) IT_EXCL-EONO,
              (10) IT_EXCL-PRIT,
              (30) IT_EXCL-PROF,
              (15) IT_EXCL-CLTY,
              (06) IT_EXCL-STAT,
              (10) IT_EXCL-GEFT,
              (10) IT_EXCL-ZRESULT,
                   IT_EXCL-ZMSG.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " WRITE_PROCESS1
*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
FORM READ_PROCESS.
  SELECT *
       FROM ZTBM_ABYCFIDT
       INTO TABLE IT_ACFI
       WHERE ZEDAT EQ P_ZEDAT
       AND   ZBTIM EQ P_ZBTIM
       AND  ( ZRESULT EQ SPACE OR ZRESULT EQ 'E' ).

  IF SY-SUBRC NE 0.
    WRITE: / TEXT-019.
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
        LT_ACFI LIKE IT_ACFI OCCURS 0 WITH HEADER LINE,
        L_TABIX TYPE SY-TABIX.
  DESCRIBE TABLE IT_ACFI LINES WA_LINE_IDX.
*  SY-TFILL.
  LOOP AT IT_ACFI.
    LT_MARC-MATNR = IT_ACFI-MTNO.
    LT_MARC-WERKS = IT_ACFI-PLNT.
    COLLECT LT_MARC.
    CLEAR: IT_ACFI, LT_MARC.
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
      LOOP AT IT_ACFI.
        L_TABIX = SY-TABIX.
        READ TABLE MT_MARC WITH KEY MATNR = IT_ACFI-MTNO
                                    WERKS = IT_ACFI-PLNT
                           BINARY SEARCH.
        IF SY-SUBRC NE 0.
          LT_ACFI = IT_ACFI.
          LT_ACFI-ZRESULT = 'L'.
*         C: Create U: Update D: Delete
          LT_ACFI-ZMODE = 'C'.       "C:CREATE
          LT_ACFI-ZBDAT = SY-DATUM.  "SAP BDC EXECUTED DATE
          LT_ACFI-ZBNAM = SY-UNAME.  "BDC User ID
          LT_ACFI-ZMSG = TEXT-022.
          DELETE IT_ACFI INDEX L_TABIX.
          APPEND LT_ACFI.
        ENDIF.
        CLEAR: IT_ACFI, LT_ACFI, MT_MARC.
      ENDLOOP.
    ELSE.
      LOOP AT IT_ACFI.
        L_TABIX = SY-TABIX.
        LT_ACFI = IT_ACFI.
        LT_ACFI-ZRESULT = 'L'.
*         C: Create U: Update D: Delete
        LT_ACFI-ZMODE = 'C'.       "C:CREATE
        LT_ACFI-ZBDAT = SY-DATUM.  "SAP BDC EXECUTED DATE
        LT_ACFI-ZBNAM = SY-UNAME.  "BDC User ID
        LT_ACFI-ZMSG = TEXT-022.
        DELETE IT_ACFI INDEX L_TABIX.
        APPEND LT_ACFI.
        CLEAR: IT_ACFI, LT_ACFI.
      ENDLOOP.
    ENDIF.

  ENDIF.
  DESCRIBE TABLE LT_ACFI LINES WA_ERRO_IDX.

  FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
  WRITE: / TEXT-020, WA_LINE_IDX.
  WRITE: / TEXT-021, WA_ERRO_IDX.
  FORMAT COLOR OFF.
* Material number does not exist WRITE
  IF NOT LT_ACFI[] IS INITIAL.

    WRITE: / TEXT-022.
    PERFORM NOT_EXIST_MATNRIAL_WRITE TABLES LT_ACFI.
*   UPDATE
    PERFORM UPDATE_ZTBM_ABYCFIDTT TABLES LT_ACFI.
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
    LT_MARC-MATNR = IT_EXCL-MTNO.
    LT_MARC-WERKS = IT_EXCL-PLNT.
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
        READ TABLE MT_MARC WITH KEY MATNR = IT_EXCL-MTNO
                                    WERKS = IT_EXCL-PLNT
                           BINARY SEARCH.
        IF SY-SUBRC NE 0.
          LT_EXCL = IT_EXCL.
          LT_EXCL-ZRESULT = 'L'.
          LT_EXCL-ZMSG = TEXT-022.
          DELETE IT_EXCL INDEX L_TABIX.
          APPEND LT_EXCL.
        ENDIF.
        CLEAR: IT_EXCL, LT_EXCL, MT_MARC.
      ENDLOOP.
    ELSE.
      LOOP AT IT_EXCL.
        L_TABIX = SY-TABIX.
        LT_EXCL = IT_EXCL.
        LT_EXCL-ZRESULT = 'L'.
        LT_EXCL-ZMSG = TEXT-022.
        DELETE IT_EXCL INDEX L_TABIX.
        APPEND LT_EXCL.
        CLEAR: IT_EXCL, LT_EXCL.
      ENDLOOP.
    ENDIF.

  ENDIF.
  DESCRIBE TABLE LT_EXCL LINES WA_ERRO_IDX.

  FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
  WRITE: / TEXT-023, WA_LINE_IDX.
  WRITE: / TEXT-021, WA_ERRO_IDX.
  FORMAT COLOR OFF.
* Material number does not exist WRITE
  IF NOT LT_EXCL[] IS INITIAL.

    WRITE: / TEXT-022.
    PERFORM NOT_EXIST_MATNRIAL_WRITE1 TABLES LT_EXCL.
  ENDIF.
ENDFORM.                    " DATA_PROCESS1
*&---------------------------------------------------------------------*
*&      Form  NOT_EXIST_MATNRIAL_WRITE
*&---------------------------------------------------------------------*
FORM NOT_EXIST_MATNRIAL_WRITE TABLES PT_ACFI STRUCTURE IT_ACFI.
  WRITE: /(18)  TEXT-008,
          (06)  TEXT-009,
          (18)  TEXT-010,
          (12)  TEXT-011,
          (10)  TEXT-012,
          (30)  TEXT-013,
          (15)  TEXT-014,
          (06)  TEXT-015,
          (10)  TEXT-016,
          (10)  TEXT-017,
                TEXT-018.
  LOOP AT PT_ACFI WHERE ZRESULT EQ 'E'.
    WRITE: /(18) PT_ACFI-MTNO,
            (06) PT_ACFI-PLNT,
            (18) PT_ACFI-CLID,
            (12) PT_ACFI-EONO,
            (10) PT_ACFI-PRIT,
            (30) PT_ACFI-PROF,
            (15) PT_ACFI-CLTY,
            (06) PT_ACFI-STAT,
            (10) PT_ACFI-GEFT,
            (10) PT_ACFI-ZRESULT,
                 PT_ACFI-ZMSG.
  ENDLOOP.
ENDFORM.                    " NOT_EXIST_MATNRIAL_WRITE
*&---------------------------------------------------------------------*
*&      Form  NOT_EXIST_MATNRIAL_WRITE1
*&---------------------------------------------------------------------*
FORM NOT_EXIST_MATNRIAL_WRITE1 TABLES PT_EXCL STRUCTURE IT_EXCL.
  WRITE: /(18)  TEXT-008,
          (06)  TEXT-009,
          (18)  TEXT-010,
          (12)  TEXT-011,
          (10)  TEXT-012,
          (30)  TEXT-013,
          (15)  TEXT-014,
          (06)  TEXT-015,
          (10)  TEXT-016,
          (10)  TEXT-017,
                TEXT-018.
  LOOP AT PT_EXCL.
    WRITE: /(18) PT_EXCL-MTNO,
            (06) PT_EXCL-PLNT,
            (18) PT_EXCL-CLID,
            (12) PT_EXCL-EONO,
            (10) PT_EXCL-PRIT,
            (30) PT_EXCL-PROF,
            (15) PT_EXCL-CLTY,
            (06) PT_EXCL-STAT,
            (10) PT_EXCL-GEFT,
            (10) PT_EXCL-ZRESULT,
                 PT_EXCL-ZMSG.
  ENDLOOP.
ENDFORM.                    " NOT_EXIST_MATNRIAL_WRITE1
*&---------------------------------------------------------------------*
*&      Form  UPDATE_PROCESS
*&---------------------------------------------------------------------*
FORM UPDATE_PROCESS.
  UPDATE ZTBM_ABYCFIDT FROM TABLE IT_ACFI.
  IF SY-SUBRC EQ 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
ENDFORM.                    " UPDATE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  UPDATE_ZTBM_ABYCFIDTT
*&---------------------------------------------------------------------*
FORM UPDATE_ZTBM_ABYCFIDTT TABLES   PT_ACFI STRUCTURE IT_ACFI .
  UPDATE ZTBM_ABYCFIDT FROM TABLE PT_ACFI.
  IF SY-SUBRC EQ 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
ENDFORM.                    " UPDATE_ZTBM_ABYCFIDTT
*&---------------------------------------------------------------------*
*&      Form  IT_ACFI_MODIFY
*&---------------------------------------------------------------------*
FORM IT_ACFI_MODIFY USING    P_MSG
                             P_MSGTY
                             P_TABIX.
  IT_ACFI-ZRESULT = P_MSGTY.
  CASE P_MSGTY.
    WHEN 'S'.
      IT_ACFI-ZMSG    = 'CONFIGURATION PROFILE CREATE'.
      IT_ACFI-ZBDAT = SY-DATUM.  "SAP BDC EXECUTED DATE
      IT_ACFI-ZBTIM = SY-UZEIT.  "SAP BDC EXECUTED TIME
      IT_ACFI-ZBNAM = SY-UNAME.  "BDC User ID
      IT_ACFI-ZRESULT = 'S'.
*     C: Create U: Update D: Delete
      IT_ACFI-ZMODE = 'C'.       "C:CREATE
    WHEN 'E'.
      IT_ACFI-ZMSG    = P_MSG.
      IT_ACFI-ZBDAT = SY-DATUM.  "SAP BDC EXECUTED DATE
*      IT_ACFI-ZBTIM = SY-UZEIT.  "SAP BDC EXECUTED TIME
      IT_ACFI-ZBNAM = SY-UNAME.  "BDC User ID
      IT_ACFI-ZMODE = 'C'.       "C:CREATE
    WHEN OTHERS.
      IT_ACFI-ZMSG    = 'CONFIGURATION PROFILE CREATE'.
      IT_ACFI-ZBDAT = SY-DATUM.  "SAP BDC EXECUTED DATE
      IT_ACFI-ZBTIM = SY-UZEIT.  "SAP BDC EXECUTED TIME
      IT_ACFI-ZBNAM = SY-UNAME.  "BDC User ID
      IT_ACFI-ZMODE = 'C'.       "C:CREATE
      IT_ACFI-ZRESULT = 'S'.
  ENDCASE.
* IT_ACFI MODIFY
  MODIFY IT_ACFI INDEX P_TABIX TRANSPORTING ZBDAT
                                            ZBTIM
                                            ZBNAM
                                            ZMODE
                                            ZRESULT
                                            ZMSG.

ENDFORM.                    " IT_ACFI_MODIFY
*&---------------------------------------------------------------------*
*&      Form  CU41_EXCEL_BDC_PROCESS
*&---------------------------------------------------------------------*
FORM CU41_EXCEL_BDC_PROCESS.
  DATA: L_TABIX TYPE SY-TABIX,
        L_MSG  LIKE CFGNL-MSGLIN.
  LOOP AT IT_EXCL.
    L_TABIX = SY-TABIX.
    PERFORM DYNPRO USING:
       'X' 'SAPLCUCO'    '0110',
       ' ' 'RCUCO-MATNR' IT_EXCL-MTNO,   "
       ' ' 'RMCLF-AENNR1' IT_EXCL-EONO,   "
       ' ' 'BDC_OKCODE'  '/00',

       'X' 'SAPLCUCO'    '0200',
       ' ' 'BDC_OKCODE'  '=P+',

       'X' 'SAPLCUCO'    '0200',
       ' ' 'RCUCO-PRIO(02)' IT_EXCL-PRIT,   "
       ' ' 'RCUCO-PRFID(02)' IT_EXCL-PROF,   "
       ' ' 'RCUCO-KLART(02)' IT_EXCL-CLTY,   "
       ' ' 'BDC_OKCODE'  '/00',

       'X' 'SAPLCUCO'    '0200',
       ' ' 'RCUCO-CLSEL(02)' 'X',   "
       ' ' 'BDC_OKCODE'  '=DCLA',

       'X' 'SAPLCUCO'    '0350',
       ' ' 'BDC_OKCODE'  '=OCLA',

       'X' 'SAPLSPO1'    '0100',
       ' ' 'BDC_OKCODE'  '=NO',

       'X' 'SAPLCLFM'    '0500',
       ' ' 'BDC_OKCODE'  '=NEUZ',

       'X' 'SAPLCLFM'    '0500',
       ' ' 'RMCLF-CLASS(02)' IT_EXCL-CLID,   "
       ' ' 'BDC_OKCODE'  '/00',

       'X' 'SAPLCTMS'    '0109',
       ' ' 'RCTMS-MNAME(01)' 'COLOREXT',  "
       ' ' 'RCTMS-MNAME(02)' 'COLORINT',  "
       ' ' 'RCTMS-MNAME(03)' 'COLOR_MI',  "
       ' ' 'RCTMS-MNAME(04)' 'COLOR_OPT1',  "
       ' ' 'RCTMS-MNAME(05)' 'COLOR_OPT2',  "
       ' ' 'RCTMS-MNAME(06)' 'COLOR_OPT3',  "
       ' ' 'RCTMS-MNAME(07)' 'COLOR_OPT4',  "
       ' ' 'RCTMS-MWERT(03)' IT_EXCL-CLMI,  "
       ' ' 'RCTMS-MWERT(04)' IT_EXCL-CLO1,  "
       ' ' 'RCTMS-MWERT(05)' IT_EXCL-CLO2,  "
       ' ' 'RCTMS-MWERT(06)' IT_EXCL-CLO3,  "
       ' ' 'RCTMS-MWERT(07)' IT_EXCL-CLO4,  "
       ' ' 'BDC_OKCODE'  '=BACK',

       'X' 'SAPLCLFM'    '0500',
       ' ' 'BDC_OKCODE'  '=SAVE'.
    CALL TRANSACTION 'CU41'  USING IT_BDC
                             OPTIONS FROM WA_OPT
                             MESSAGES INTO IT_MESS.
    IT_EXCL-ZRESULT = SY-MSGTY.
    PERFORM RKC_MSG_STRING CHANGING L_MSG.
    IT_EXCL-ZMSG = L_MSG.
*   MODIFY IT_EXCL
    MODIFY IT_EXCL INDEX L_TABIX TRANSPORTING ZRESULT
                                              ZMSG.
    REFRESH: IT_BDC, IT_MESS.
    CLEAR: IT_BDC, IT_MESS, IT_EXCL.
  ENDLOOP.
ENDFORM.                    " CU41_EXCEL_BDC_PROCESS
*&---------------------------------------------------------------------*
*&      Form  MM02_BDC_PROCESS
*&---------------------------------------------------------------------*
FORM MM02_BDC_PROCESS.
  DATA: L_TABIX TYPE SY-TABIX,
        L_MSG  LIKE CFGNL-MSGLIN.
  LOOP AT IT_ACFI WHERE ZRESULT NE 'E'.
    L_TABIX = SY-TABIX.
    PERFORM DYNPRO USING:
       'X' 'SAPLMGMM'    '0060',
       ' ' 'RMMG1-MATNR' IT_ACFI-MTNO,   "
       ' ' 'BDC_OKCODE'  '/00',

       'X' 'SAPLMGMM'    '0070',
       ' ' 'MSICHTAUSW-KZSEL(01)' 'X',   "
       ' ' 'BDC_OKCODE'  '=ENTR',
*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4004',
       'X' 'SAPLMGMM'    '5004',
       ' ' 'BDC_OKCODE'  '=SP14',

       'X' 'SAPLMGMM'    '0081',
       ' ' 'RMMG1-WERKS' IT_ACFI-PLNT,   "
       ' ' 'BDC_OKCODE'  '=ENTR',

*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
       'X' 'SAPLMGMM'    '5000',
       ' ' 'MARC-STDPD' IT_ACFI-PROF,   "
       ' ' 'BDC_OKCODE'  '=PB19',

       'X' 'SAPLCEI0'    '0109',
       ' ' 'BDC_OKCODE'  '=BACK',

*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
       'X' 'SAPLMGMM'    '5000',
       ' ' 'BDC_OKCODE'  '=BU'.

    CALL TRANSACTION 'MM02'  USING IT_BDC
                    OPTIONS FROM WA_OPT
                    MESSAGES INTO IT_MESS.
    IT_ACFI-ZRESULT = SY-MSGTY.
    IT_ACFI-ZMODE = 'C'.
    IF IT_ACFI-ZRESULT EQ 'E'.
      PERFORM RKC_MSG_STRING CHANGING L_MSG.
*     MODIFY IT_ACFI
      PERFORM IT_ACFI_MODIFY USING L_MSG
                                   SY-MSGTY
                                   L_TABIX.
    ENDIF.
    REFRESH: IT_BDC, IT_MESS.
    CLEAR: IT_BDC, IT_MESS, IT_ACFI.
  ENDLOOP.
ENDFORM.                    " MM02_BDC_PROCESS
*&---------------------------------------------------------------------*
*&      Form  MM02_EXCEL_BDC_PROCESS
*&---------------------------------------------------------------------*
FORM MM02_EXCEL_BDC_PROCESS.
  DATA: L_TABIX TYPE SY-TABIX,
        L_MSG  LIKE CFGNL-MSGLIN.
  LOOP AT IT_EXCL WHERE ZRESULT NE 'E'.
    L_TABIX = SY-TABIX.
    PERFORM DYNPRO USING:
       'X' 'SAPLMGMM'    '0060',
       ' ' 'RMMG1-MATNR' IT_EXCL-MTNO,   "
       ' ' 'BDC_OKCODE'  '/00',

       'X' 'SAPLMGMM'    '0070',
       ' ' 'MSICHTAUSW-KZSEL(11)' 'X',   "
       ' ' 'BDC_OKCODE'  '=ENTR',

       'X' 'SAPLMGMM'    '0080',
       ' ' 'RMMG1-WERKS' IT_EXCL-PLNT,   "
       ' ' 'BDC_OKCODE'  '=ENTR',

*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
       ' ' 'MARC-STDPD' IT_EXCL-PROF,   "
       ' ' 'BDC_OKCODE'  '=PB19',

       'X' 'SAPLCEI0'    '0109',
       ' ' 'BDC_OKCODE'  '=BACK',

*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
     'X' 'SAPLMGMM'    '5000',
       ' ' 'BDC_OKCODE'  '=BU'.

    CALL TRANSACTION 'MM02'  USING IT_BDC
                    OPTIONS FROM WA_OPT
                    MESSAGES INTO IT_MESS.
    IT_EXCL-ZRESULT = SY-MSGTY.
    IF IT_EXCL-ZRESULT EQ 'E'.
      PERFORM RKC_MSG_STRING CHANGING L_MSG.
      IT_EXCL-ZMSG = L_MSG.
*     MODIFY IT_EXCL
      MODIFY IT_EXCL INDEX L_TABIX TRANSPORTING ZRESULT
                                                ZMSG.
    ENDIF.
    REFRESH: IT_BDC, IT_MESS.
    CLEAR: IT_BDC, IT_MESS, IT_EXCL.
  ENDLOOP.
ENDFORM.                    " MM02_EXCEL_BDC_PROCESS
*&---------------------------------------------------------------------*
*&      Form  DATA_MAST_CHECK
*&---------------------------------------------------------------------*
FORM DATA_MAST_CHECK.
  DATA: BEGIN OF LT_COLL OCCURS 0,
          MATNR TYPE MAST-MATNR,
          WERKS TYPE MAST-WERKS,
        END OF LT_COLL.
  DATA: LT_MAST LIKE LT_COLL OCCURS 0 WITH HEADER LINE,
        LT_ACFI LIKE IT_ACFI OCCURS 0 WITH HEADER LINE,
        L_TABIX TYPE SY-TABIX.
  LOOP AT IT_ACFI.
    LT_COLL-MATNR = IT_ACFI-MTNO.
    LT_COLL-WERKS = IT_ACFI-PLNT.
    COLLECT LT_COLL.
    CLEAR: LT_COLL, IT_ACFI.
  ENDLOOP.

  IF NOT LT_COLL[] IS INITIAL.
    SELECT MATNR
           WERKS
         FROM MAST
         INTO TABLE LT_MAST
         FOR ALL ENTRIES IN LT_COLL
         WHERE MATNR EQ LT_COLL-MATNR
         AND   WERKS EQ LT_COLL-WERKS
         AND   STLAN EQ '1'.

    DESCRIBE TABLE IT_ACFI LINES WA_LINE_IDX.
*   SORTING
    SORT LT_MAST BY MATNR WERKS.
    LOOP AT IT_ACFI.
      L_TABIX = SY-TABIX.
      READ TABLE LT_MAST WITH KEY MATNR = IT_ACFI-MTNO
                                  WERKS = IT_ACFI-PLNT
                         BINARY SEARCH.
      IF SY-SUBRC NE 0.
        LT_ACFI = IT_ACFI.
        LT_ACFI-ZRESULT = 'E'.
        LT_ACFI-ZMSG = 'BOM was not defined'.
        LT_ACFI-ZMODE = 'C'.       "C:CREATE
        LT_ACFI-ZBDAT = SY-DATUM.  "SAP BDC EXECUTED DATE
        LT_ACFI-ZBNAM = SY-UNAME.  "BDC User ID
        APPEND LT_ACFI.
        DELETE IT_ACFI INDEX L_TABIX.
      ENDIF.
      CLEAR: IT_ACFI, LT_ACFI, LT_MAST.
    ENDLOOP.
  ENDIF.

  DESCRIBE TABLE LT_ACFI LINES WA_ERRO_IDX.

  FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
  WRITE: / TEXT-020, WA_LINE_IDX.
  WRITE: / 'BOM was not defined Error : ', WA_ERRO_IDX.
  FORMAT COLOR OFF.
* Material number does not exist WRITE
  IF NOT LT_ACFI[] IS INITIAL.

    WRITE: /'BOM was not defined'.
    PERFORM NOT_EXIST_MATNRIAL_WRITE TABLES LT_ACFI.
*   UPDATE
    PERFORM UPDATE_ZTBM_ABYCFIDTT TABLES LT_ACFI.
  ENDIF.
ENDFORM.                    " DATA_MAST_CHECK
*&---------------------------------------------------------------------*
*&      Form  GET_PARAMETER_ID
*&---------------------------------------------------------------------*
FORM GET_PARAMETER_ID.
  DATA : L_C(2) TYPE C.
  RCUCO-CUQOB = '001'.
  SET PARAMETER ID PARAM_CUQ FIELD RCUCO-CUQOB.
  SET PARAMETER ID 'KLA' FIELD L_C.
ENDFORM.                    " GET_PARAMETER_ID
*&---------------------------------------------------------------------*
*&      Form  MM02_BDC_COLOR_EXT_INT
*&---------------------------------------------------------------------*
FORM MM02_BDC_COLOR_EXT_INT.
  DATA: L_TABIX TYPE SY-TABIX,
        L_MSG  LIKE CFGNL-MSGLIN.
  DATA: L_COLOREXT LIKE RCTMS-MWERT,
        L_COLORINT LIKE RCTMS-MWERT.
  LOOP AT IT_ACFI WHERE ZRESULT NE 'E'.
    L_TABIX = SY-TABIX.
    PERFORM READ_MARA_COLOR_EXT_INT USING    IT_ACFI-MTNO+5(2)
                                             IT_ACFI-MTNO(1)
                                    CHANGING L_COLOREXT
                                             L_COLORINT.
    PERFORM DYNPRO USING:
       'X' 'SAPLMGMM'    '0060',
       ' ' 'RMMG1-MATNR' IT_ACFI-MTNO,   "
       ' ' 'BDC_OKCODE'  '/00',

       'X' 'SAPLMGMM'    '0070',
       ' ' 'MSICHTAUSW-KZSEL(01)' 'X',   "
       ' ' 'BDC_OKCODE'  '=ENTR',

*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4004',
       'X' 'SAPLMGMM'    '5004',
       ' ' 'BDC_OKCODE'  '=SP14',

       'X' 'SAPLMGMM'    '0081',
       ' ' 'RMMG1-WERKS' IT_ACFI-PLNT,   "
       ' ' 'BDC_OKCODE'  '=ENTR',

*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
       'X' 'SAPLMGMM'    '5000',
       ''  'MARC-STDPD'  IT_ACFI-MTNO,
       ' ' 'BDC_OKCODE'  '=PB19',

       'X' 'SAPLCEI0'    '0109',
       ' ' 'RCTMS-MNAME(01)' L_NAMEEXT,   "
       ' ' 'RCTMS-MNAME(02)' L_NAMEINT,   "
       ' ' 'RCTMS-MWERT(01)' L_COLOREXT,   "
       ' ' 'RCTMS-MWERT(02)' L_COLORINT,   "
       ' ' 'BDC_OKCODE'  '=BACK',

*       2003/12/21 SCREEN NUMBER CHANGE
*     'X' 'SAPLMGMM'    '4000',
       'X' 'SAPLMGMM'    '5000',
       ' ' 'BDC_OKCODE'  '=BU'.

    CALL TRANSACTION 'MM02'  USING IT_BDC
                    OPTIONS FROM WA_OPT
                    MESSAGES INTO IT_MESS.

***    IT_ACFI-ZRESULT = SY-MSGTY.
***    IT_ACFI-ZMODE = 'C'.
***    IF IT_ACFI-ZRESULT EQ 'E'.
***      PERFORM RKC_MSG_STRING CHANGING L_MSG.
****     MODIFY IT_ACFI
***      PERFORM IT_ACFI_MODIFY USING L_MSG
***                                   SY-MSGTY
***                                   L_TABIX.
***    ENDIF.

    REFRESH: IT_BDC, IT_MESS.
    CLEAR: IT_BDC, IT_MESS, IT_ACFI.
  ENDLOOP.
ENDFORM.                    " MM02_BDC_COLOR_EXT_INT
*&---------------------------------------------------------------------*
*&      Form  READ_MARA_COLOR_EXT_INT
*&---------------------------------------------------------------------*
FORM READ_MARA_COLOR_EXT_INT USING    P_MATNR P_YEAR
                             CHANGING P_COLOREXT
                                      P_COLORINT.
  DATA: L_FERTH LIKE MARA-FERTH,
        L_NORMT LIKE MARA-NORMT.

  DATA $MATNR LIKE MARA-MATNR.

  CONCATENATE P_MATNR P_YEAR INTO $MATNR.

  SELECT SINGLE FERTH
                NORMT
              FROM MARA
              INTO (L_FERTH, L_NORMT)
              WHERE MATNR EQ $MATNR.
  IF SY-SUBRC EQ 0.
    P_COLOREXT = L_FERTH.
    P_COLORINT = L_NORMT.
  ELSE.
    CLEAR : P_COLOREXT, P_COLORINT.
  ENDIF.
ENDFORM.                    " READ_MARA_COLOR_EXT_INT
*&---------------------------------------------------------------------*
*&      Form  MM02_EXCEL_BDC_COLOR_EXT_INT
*&---------------------------------------------------------------------*
FORM MM02_EXCEL_BDC_COLOR_EXT_INT.
  DATA: L_TABIX TYPE SY-TABIX,
        L_MSG  LIKE CFGNL-MSGLIN.
  DATA: L_COLOREXT LIKE RCTMS-MWERT,
        L_COLORINT LIKE RCTMS-MWERT.
  LOOP AT IT_EXCL WHERE ZRESULT NE 'E'.
    L_TABIX = SY-TABIX.
    PERFORM READ_MARA_COLOR_EXT_INT USING    IT_EXCL-MTNO+6(2)
                                             IT_EXCL-MTNO(1)
                                    CHANGING L_COLOREXT
                                             L_COLORINT.
    PERFORM DYNPRO USING:
       'X' 'SAPLMGMM'    '0060',
       ' ' 'RMMG1-MATNR' IT_EXCL-MTNO,   "
       ' ' 'BDC_OKCODE'  '/00',
       'X' 'SAPLMGMM'    '0070',
       ' ' 'MSICHTAUSW-KZSEL(01)' 'X',   "
       ' ' 'BDC_OKCODE'  '=ENTR',
     'X' 'SAPLMGMM'    '5004',
       ' ' 'BDC_OKCODE'  '=SP14',
       'X' 'SAPLMGMM'    '0081',
       ' ' 'RMMG1-WERKS' IT_EXCL-PLNT,   "
       ' ' 'BDC_OKCODE'  '=ENTR',
     'X' 'SAPLMGMM'    '5000',
       ' ' 'BDC_OKCODE'  '=PB19',
       'X' 'SAPLCEI0'    '0109',
       ' ' 'RCTMS-MNAME(01)' L_NAMEEXT,   "
       ' ' 'RCTMS-MNAME(02)' L_NAMEINT,   "
       ' ' 'RCTMS-MWERT(01)' L_COLOREXT,   "
       ' ' 'RCTMS-MWERT(02)' L_COLORINT,   "
       ' ' 'BDC_OKCODE'  '=BACK',
     'X' 'SAPLMGMM'    '5000',
       ' ' 'BDC_OKCODE'  '=BU'.

    CALL TRANSACTION 'MM02'  USING IT_BDC
                    OPTIONS FROM WA_OPT
                    MESSAGES INTO IT_MESS.
*    IT_EXCL-ZRESULT = SY-MSGTY.
*    IF IT_EXCL-ZRESULT EQ 'E'.
*      PERFORM RKC_MSG_STRING CHANGING L_MSG.
*      IT_EXCL-ZMSG = L_MSG.
**     MODIFY IT_EXCL
*      MODIFY IT_EXCL INDEX L_TABIX TRANSPORTING ZRESULT
*                                                ZMSG.
*    ENDIF.
  ENDLOOP.
ENDFORM.                    " MM02_EXCEL_BDC_COLOR_EXT_INT
