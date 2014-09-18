*----------------------------------------------------------------------*
*   INCLUDE ZIPP301L_ECM_BDC_F01                                      *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
FORM INITIALIZATION.

ENDFORM.                               " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  SCREEN_MODIFY
*&---------------------------------------------------------------------*
FORM SCREEN_MODIFY.
*  LOOP AT SCREEN.
*    CASE SCREEN-NAME.
*      WHEN 'P_FILETY' OR 'P_TCODE'.
*        SCREEN-INPUT = 0.
*    ENDCASE.
*
*    MODIFY SCREEN.
*    CLEAR SCREEN.
*  ENDLOOP.
ENDFORM.                    " SCREEN_MODIFY
*&---------------------------------------------------------------------*
*&      Form  AT_SEL_SCREEN_ON_VALUE_REQUEST
*&---------------------------------------------------------------------*
FORM AT_SEL_SCREEN_ON_VALUE_REQUEST USING DEF_PATH LIKE RLGRAP-FILENAME
                                          MODE     TYPE C.

*  DATA: TMP_FILENAME LIKE RLGRAP-FILENAME.
*  DATA: TMP_MASK(80).                  " LIKE GLOBAL_FILEMASK_ALL.
*  DATA: FIELDLN TYPE I.
*  FIELD-SYMBOLS: <TMP_SYM>.
*
** Build Filter for Fileselektor
*
**  IF GLOBAL_FILEMASK_MASK IS INITIAL.
*  TMP_MASK = ',*.*,*.*.'.
**  ELSE.
**    TMP_MASK = ','.
**    WRITE GLOBAL_FILEMASK_TEXT TO TMP_MASK+1.
**    WRITE ',' TO TMP_MASK+21.
**    WRITE GLOBAL_FILEMASK_MASK TO TMP_MASK+22.
**    WRITE '.' TO TMP_MASK+42.
**    CONDENSE TMP_MASK NO-GAPS.
**  ENDIF.
*
**  IF NOT GLOBAL_FILEMASK_ALL IS INITIAL.
**    TMP_MASK = GLOBAL_FILEMASK_ALL.
**  ENDIF.
**
*  FIELDLN = STRLEN( DEF_PATH ) - 1.
*  ASSIGN DEF_PATH+FIELDLN(1) TO <TMP_SYM>.
*  IF <TMP_SYM> = '/' OR <TMP_SYM> = '\'.
*    CLEAR <TMP_SYM>.
*  ENDIF.
*
*  CALL FUNCTION 'WS_FILENAME_GET'
*       EXPORTING
*            DEF_FILENAME     = P_FILE
*            DEF_PATH         = DEF_PATH
**           MASK             = ',*.*,*.*.'
*            MASK             = TMP_MASK
*            MODE             = MODE
**           TITLE            = ' '
*       IMPORTING
*            FILENAME         = TMP_FILENAME
**         RC               =
*       EXCEPTIONS
*            INV_WINSYS       = 01
*            NO_BATCH         = 02
*            SELECTION_CANCEL = 03
*            SELECTION_ERROR  = 04.
*
*  IF SY-SUBRC = 0.
*    P_FILE = TMP_FILENAME.
*  ELSE.
** IF SY-SUBRC = 01.    "// Does not work, why ???
**   MESSAGELINE = 'Not supported'.
** ENDIF.
*  ENDIF.

ENDFORM.                               " AT_SEL_SCREEN_ON_VALUE_REQUEST
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_PROCESS
*&---------------------------------------------------------------------*
FORM UPLOAD_PROCESS.

*  CALL FUNCTION 'WS_UPLOAD'
*       EXPORTING
*            CODEPAGE                = ' '
*            FILENAME                = P_FILE
*            FILETYPE                = P_FILETY
**           HEADLEN                 = ' '
**           LINE_EXIT               = ' '
**           TRUNCLEN                = ' '
**           USER_FORM               = ' '
**           USER_PROG               = ' '
**      IMPORTING
**           FILELENGTH              =
*       TABLES
*            DATA_TAB                = IT_ECM
*      EXCEPTIONS
*           CONVERSION_ERROR        = 1
*           FILE_OPEN_ERROR         = 2
*           FILE_READ_ERROR         = 3
*           INVALID_TABLE_WIDTH     = 4
*           INVALID_TYPE            = 5
*           NO_BATCH                = 6
*           UNKNOWN_ERROR           = 7
*           GUI_REFUSE_FILETRANSFER = 8
*           CUSTOMER_ERROR          = 9
*           OTHERS                  = 10
*            .
*  CASE SY-SUBRC.
*    WHEN 0.
*      DATA L_TEXT(132).
*      CONCATENATE P_FILE ' DATA UPLOAD SUCCESS!!'
*                  INTO L_TEXT.
*      WRITE: / L_TEXT.
*      SKIP.
*    WHEN 2.
*      MESSAGE E000 WITH 'FILE OPEN ERROR, FILE NO FOUND!'.
*    WHEN 3.
*      MESSAGE E000 WITH 'FILE READ ERROR'.
*    WHEN OTHERS.
*      MESSAGE E000 WITH 'FILE UPLOAD ERROR, CHECK YOUR FILE!.'.
*  ENDCASE.

ENDFORM.                               " UPLOAD_PROCESS
*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
FORM READ_PROCESS.
  PERFORM READ_ECM.
ENDFORM.                    " READ_PROCESS
*&---------------------------------------------------------------------*
*&      Form  READ_ECM
*&---------------------------------------------------------------------*
FORM READ_ECM.
*
  SELECT *
       FROM ZTBM_ABXEHDDT
       INTO TABLE IT_ECM
       WHERE ZEDAT EQ P_ZEDAT
       AND   ZBTIM EQ P_ZBTIM.
  IF SY-SUBRC NE 0.
    WRITE: TEXT-001.
  ENDIF.

ENDFORM.                    " READ_ECM
*&---------------------------------------------------------------------*
*&      Form  BDC_PROCESS
*&---------------------------------------------------------------------*
FORM BDC_PROCESS.
  DATA: L_TABIX TYPE SY-TABIX,
        L_MSG  LIKE CFGNL-MSGLIN,
        L_MSGTY TYPE SY-MSGTY,
        L_DATUM(10) . "TYPE SY-DATUM.

  LOOP AT IT_ECM.
    L_TABIX = SY-TABIX.
    WRITE: IT_ECM-VALD TO L_DATUM.
    PERFORM DYNPRO USING:
       'X' 'SAPMC29C'    '0100',
       ' ' 'RC29A-AENNR' IT_ECM-EONO,    "Change number
       ' ' 'RAD_BUT_ECNTYP-NORM' 'X',    "Change Master
       ' ' 'BDC_OKCODE'  '/00',

       'X' 'SAPMC29C'   '0010',
       ' ' 'RC29A-AETXT' IT_ECM-EODS,
       ' ' 'RC29A-DATUV' L_DATUM,
       ' ' 'RC29A-AENST' IT_ECM-STAT,
       ' ' 'BDC_OKCODE'  '/00',

       'X' 'SAPMC29C'   '0020',
       ' ' 'RC29A-AEERL(01)' 'X',
       ' ' 'RC29A-INDFL(01)' 'X',
       ' ' 'RC29A-OIGEN(01)' 'X',
       ' ' 'BDC_OKCODE'  '=FCBU'.
    CALL TRANSACTION 'CC01'
                    USING IT_BDC
                    MODE 'N'
                    MESSAGES INTO IT_MSG.
    L_MSGTY = SY-MSGTY.
    PERFORM RKC_MSG_STRING CHANGING L_MSG.
    PERFORM MODIFY_IT_ECM_DATE USING L_TABIX
                                     L_MSGTY
                                     L_MSG.
    REFRESH IT_BDC. CLEAR IT_BDC.
    REFRESH IT_MSG. CLEAR IT_MSG.
    CLEAR IT_ECM.
  ENDLOOP.
ENDFORM.                               " BDC_PROCESS
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
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  WRITE_PROCESS
*&---------------------------------------------------------------------*
FORM WRITE_PROCESS.
  DATA: L_LINES TYPE SY-TABIX,
        M_LINES TYPE SY-TABIX.

  LOOP AT IT_ECM.
    IF IT_ECM-ZRESULT EQ 'E'.
      L_LINES = L_LINES + 1.
    ELSE.
      M_LINES = M_LINES + 1.
    ENDIF.
  ENDLOOP.

  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  WRITE: / TEXT-012, M_LINES.
  WRITE: / TEXT-013, L_LINES.
  FORMAT COLOR OFF.
  IF L_LINES GE 1.
    FORMAT COLOR COL_HEADING INTENSIFIED OFF.
    WRITE: / TEXT-014.
    FORMAT COLOR OFF.

    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
*    WRITE: /(12) 'CHANGE/NO',     " TYPE RC29A-AENNR,
*            (40) 'C/NO TEXT',     "TYPE RC29A-AETXT,
*            (10) 'Valid from',    "TYPE RC29A-DATUV,
*            (10) 'EO TYPE',       "
*            (10) 'STATUS',        "TYPE RC29A-AENST,
*            (15) 'Message number' ,
*            (78) 'Message text' .
    WRITE: /(12)  TEXT-015,     " TYPE RC29A-AENNR,
            (40)  TEXT-016,     "TYPE RC29A-AETXT,
            (10)  TEXT-017,    "TYPE RC29A-DATUV,
            (10)  TEXT-018,       "
            (10)  TEXT-019,        "TYPE RC29A-AENST,
            (15)  TEXT-020 ,
            (78) TEXT-021 .
    FORMAT COLOR OFF.

    LOOP AT IT_ECM WHERE ZRESULT EQ 'E'.
      WRITE: /(12) IT_ECM-EONO,
              (40) IT_ECM-EODS,
              (10) IT_ECM-EOKD,
              (10) IT_ECM-STAT,
              (10) IT_ECM-VALD,
              (15) IT_ECM-ZRESULT,
              (78) IT_ECM-ZMSG.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " WRITE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
FORM DATA_PROCESS.
  DATA: L_TABIX TYPE SY-TABIX,
        L_STRLEN TYPE I,
        L_LINES TYPE SY-TABIX.
  DATA LT_ECM_ER LIKE IT_ECM OCCURS 0 WITH HEADER LINE.
  DATA: BEGIN OF LT_AENR OCCURS 0,
          AENNR TYPE AENR-AENNR,
          AENST TYPE AENR-AENST,
        END OF LT_AENR.
* DATA TOTAL LINES
  DESCRIBE TABLE IT_ECM LINES WA_LINE_IDX.
* TABLE 'AENR' SELECTION.
  PERFORM READ_AENR TABLES LT_AENR.
* SORTING
  SORT LT_AENR BY AENNR.
* ERROR CHECK
  LOOP AT IT_ECM.
    L_TABIX = SY-TABIX.
    READ TABLE LT_AENR WITH KEY AENNR = IT_ECM-EONO
                       BINARY SEARCH
                       TRANSPORTING NO FIELDS.
    IF SY-SUBRC EQ 0.
      LT_ECM_ER = IT_ECM.
      LT_ECM_ER-ZRESULT = 'L'.
      LT_ECM_ER-ZMSG = TEXT-002.
      PERFORM ZSBM_IF_TIME_CHANGE USING     'E'
                                            IT_ECM-ZEDAT
                                            IT_ECM-ZETIM
                                   CHANGING LT_ECM_ER-ZBDAT
                                            LT_ECM_ER-ZBTIM
                                            LT_ECM_ER-ZBNAM.
      APPEND LT_ECM_ER.
      DELETE IT_ECM INDEX L_TABIX.
      CLEAR: LT_ECM_ER, IT_ECM.
      CONTINUE.
    ENDIF.
    L_STRLEN = STRLEN( IT_ECM-EONO ).
*   Change number inputed wrongly.
    IF L_STRLEN NE '12'.
      LT_ECM_ER = IT_ECM.
      PERFORM ZSBM_IF_TIME_CHANGE USING     'E'
                                            IT_ECM-ZEDAT
                                            IT_ECM-ZETIM
                                   CHANGING LT_ECM_ER-ZBDAT
                                            LT_ECM_ER-ZBTIM
                                            LT_ECM_ER-ZBNAM.
      LT_ECM_ER-ZRESULT = 'L'.
      LT_ECM_ER-ZMSG = TEXT-003.
      APPEND LT_ECM_ER.
      DELETE IT_ECM INDEX L_TABIX.
      CLEAR: LT_ECM_ER, IT_ECM.
      CONTINUE.
    ENDIF.
    IF IT_ECM-STAT NE '01'.
      LT_ECM_ER = IT_ECM.
      PERFORM ZSBM_IF_TIME_CHANGE USING     'E'
                                            IT_ECM-ZEDAT
                                            IT_ECM-ZETIM
                                   CHANGING LT_ECM_ER-ZBDAT
                                            LT_ECM_ER-ZBTIM
                                            LT_ECM_ER-ZBNAM.
      LT_ECM_ER-ZRESULT = 'L'.
      LT_ECM_ER-ZMSG = TEXT-004.
      APPEND LT_ECM_ER.
      DELETE IT_ECM INDEX L_TABIX.
      CLEAR: LT_ECM_ER, IT_ECM.
      CONTINUE.
    ENDIF.

    IF IT_ECM-EOKD NE '1'.
      LT_ECM_ER = IT_ECM.
      PERFORM ZSBM_IF_TIME_CHANGE USING     'E'
                                            IT_ECM-ZEDAT
                                            IT_ECM-ZETIM
                                   CHANGING LT_ECM_ER-ZBDAT
                                            LT_ECM_ER-ZBTIM
                                            LT_ECM_ER-ZBNAM.
      LT_ECM_ER-ZRESULT = 'L'.
      LT_ECM_ER-ZMSG = TEXT-004.
      APPEND LT_ECM_ER.
      DELETE IT_ECM INDEX L_TABIX.
      CLEAR: LT_ECM_ER, IT_ECM.
      CONTINUE.
    ENDIF.
  ENDLOOP.
*
  DESCRIBE TABLE LT_ECM_ER LINES WA_ERRO_IDX.

  FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
  WRITE: / TEXT-005,          WA_LINE_IDX.
  WRITE: / TEXT-004, WA_ERRO_IDX.
  FORMAT COLOR OFF.
  IF WA_ERRO_IDX GE 1.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
    WRITE: / TEXT-004.
    FORMAT COLOR OFF.
    FORMAT COLOR COL_HEADING INTENSIFIED OFF.
*    WRITE: /(12) 'CHANGE/NO',     " TYPE RC29A-AENNR,
*            (40) 'C/NO TEXT',     "TYPE RC29A-AETXT,
*            (10) 'Valid from',    "TYPE RC29A-DATUV,
*            (10) 'EO TYPE',       "
*            (10) 'STATUS',        "TYPE RC29A-AENST,
*            (100) 'MESSAGE'.
    WRITE: /(12)  TEXT-006,     " TYPE RC29A-AENNR,
            (40)  TEXT-007,     "TYPE RC29A-AETXT,
            (10)  TEXT-008,    "TYPE RC29A-DATUV,
            (10)  TEXT-009,       "
            (10)  TEXT-010,        "TYPE RC29A-AENST,
            (100)  TEXT-011.
    FORMAT COLOR OFF.
    LOOP AT LT_ECM_ER.
      WRITE: /(12) LT_ECM_ER-EONO,     " TYPE RC29A-AENNR,
              (40) LT_ECM_ER-EODS,     "TYPE RC29A-AETXT,
              (10) LT_ECM_ER-VALD,     "TYPE RC29A-DATUV,
              (10) LT_ECM_ER-EOKD,      "TYPE AENR-AENST,
              (10) LT_ECM_ER-STAT,     "TYPE RC29A-AENST,
              (100) LT_ECM_ER-ZMSG.
    ENDLOOP.
    UPDATE ZTBM_ABXEHDDT FROM TABLE LT_ECM_ER.
    IF SY-SUBRC EQ 0.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
  ENDIF.
ENDFORM.                    " DATA_PROCESS
*&---------------------------------------------------------------------*
*&      Form  READ_AENR
*&---------------------------------------------------------------------*
FORM READ_AENR TABLES PT_AENR.
  IF NOT IT_ECM[] IS INITIAL.
    SELECT AENNR
           AENST
         FROM AENR
         INTO TABLE PT_AENR
         FOR ALL ENTRIES IN IT_ECM
         WHERE AENNR EQ IT_ECM-EONO.
  ENDIF.
ENDFORM.                    " READ_AENR
*&---------------------------------------------------------------------*
*&      Form  ZSBM_IF_TIME_CHANGE
*&---------------------------------------------------------------------*
FORM ZSBM_IF_TIME_CHANGE USING     P_CHK
                                   P_ZEDAT
                                   P_ZETIM
                          CHANGING P_ZBDAT
                                   P_ZBTIM
                                   P_ZBNAM.
* BDC EXECUTE DATE
  P_ZBDAT = SY-DATUM.
  P_ZBNAM = SY-UNAME.
* If error becomes, do not input time.
  IF P_CHK EQ 'S'.
    P_ZBTIM = SY-UZEIT.
  ENDIF.
ENDFORM.                    " ZSBM_IF_TIME_CHANGE
*&---------------------------------------------------------------------*
*&      Form  MODIFY_IT_ECM_DATE
*&---------------------------------------------------------------------*
FORM MODIFY_IT_ECM_DATE USING P_TABIX
                              P_MSGTY
                              P_MSG.
*     C: Create U: Update D: Delete
  IT_ECM-ZMODE = 'C'.       "C:CREATE
  IT_ECM-ZMSG    = P_MSG.
  CASE P_MSGTY.
    WHEN 'S'.
      IT_ECM-ZBDAT = SY-DATUM.  "SAP BDC EXECUTED DATE
      IT_ECM-ZBTIM = SY-UZEIT.  "SAP BDC EXECUTED TIME
      IT_ECM-ZBNAM = SY-UNAME.  "BDC User ID
      IT_ECM-ZRESULT = 'S'.
    WHEN 'E'.
      IT_ECM-ZBDAT = SY-DATUM.  "SAP BDC EXECUTED DATE
*      IT_ECM-ZBTIM = SY-UZEIT.  "SAP BDC EXECUTED TIME
      IT_ECM-ZBNAM = SY-UNAME.  "BDC User ID
      IT_ECM-ZRESULT = 'E'.
    WHEN OTHERS.
      IT_ECM-ZBDAT = SY-DATUM.  "SAP BDC EXECUTED DATE
      IT_ECM-ZBTIM = SY-UZEIT.  "SAP BDC EXECUTED TIME
      IT_ECM-ZBNAM = SY-UNAME.  "BDC User ID
      IT_ECM-ZRESULT = 'S'.
  ENDCASE.
* IT_ACHR MODIFY
  MODIFY IT_ECM INDEX P_TABIX TRANSPORTING ZBDAT
                                           ZBTIM
                                           ZBNAM
                                           ZMODE
                                           ZRESULT
                                           ZMSG.

ENDFORM.                    " MODIFY_IT_ECM_DATE
*&---------------------------------------------------------------------*
*&      Form  TABLE_UPDATE
*&---------------------------------------------------------------------*
FORM TABLE_UPDATE.
  UPDATE ZTBM_ABXEHDDT FROM TABLE IT_ECM.
  IF SY-SUBRC EQ 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
ENDFORM.                    " TABLE_UPDATE
