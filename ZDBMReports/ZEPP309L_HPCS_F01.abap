*----------------------------------------------------------------------*
*   INCLUDE ZEPP309L_HPCS_F01                                          *
*----------------------------------------------------------------------*
*&--------------------------------------------------------------------&*
*& Date         User   Transport   Description                        &*
*& 11/23/2004   Shiva  UD1K913152  Pass material number instead of    &*
*&                                 work order number for func. module &*
*&                                 Z_FPP_HANDLING_MASTER              &*
*& 12/01/2004   Shiva  UD1K913281  Added logic for charac."P_WO_HPC_B"&*
*&--------------------------------------------------------------------&*

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
      %_P_TCODE_%_APP_%-TEXT = 'Transaction code'.
      P_TCODE = 'MM02 Classification'.
*    ELSEIF SCREEN-NAME  EQ 'P_RDO2'.  "EXCEL DATA
    ELSEIF P_RDO2 EQ 'X'.  "EXCEL DATA
      CASE SCREEN-NAME.
        WHEN 'P_ZEDAT' OR '%_P_ZEDAT_%_APP_%-TEXT'
          OR 'P_ZBTIM' OR '%_P_ZBTIM_%_APP_%-TEXT'.
          SCREEN-ACTIVE = 0.
        WHEN 'P_FILETY' OR 'P_TCODE'.
          SCREEN-INPUT = 0.
      ENDCASE.
      %_P_TCODE_%_APP_%-TEXT = 'TABLE INSERT'.
      P_TCODE = 'ZTBM_ABXHPCDT'.
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
            DATA_TAB                = IT_HPCS
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
  PERFORM READ_ZTBM_ABXHPCDT.
  PERFORM READ_ZTBM_ABXHPCDT01.
ENDFORM.                    " READ_PROCESS
*&---------------------------------------------------------------------*
*&      Form  READ_ZTBM_ABXHPCDT
*&---------------------------------------------------------------------*
FORM READ_ZTBM_ABXHPCDT.
  SELECT *
       FROM ZTBM_ABXHPCDT
       INTO TABLE IT_HPCDT
       WHERE ZEDAT EQ P_ZEDAT
       AND   ZBTIM EQ P_ZBTIM.
  IF SY-SUBRC NE 0.
    MESSAGE S001 WITH TEXT-017.
  ENDIF.
ENDFORM.                    " READ_ZTBM_ABXHPCDT
*&---------------------------------------------------------------------*
*&      Form  READ_ZTBM_ABXHPCDT01
*&---------------------------------------------------------------------*
FORM READ_ZTBM_ABXHPCDT01.
  SELECT *
       FROM ZTBM_ABXHPCDT01
       INTO TABLE IT_HPCDT01
       WHERE ZEDAT EQ P_ZEDAT
       AND   ZBTIM EQ P_ZBTIM.
  IF SY-SUBRC NE 0.
    MESSAGE S001 WITH TEXT-018.
  ENDIF.
ENDFORM.                    " READ_ZTBM_ABXHPCDT01
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
FORM DATA_PROCESS.
************* 2003.11.06*********************
* IT_HPCDT IT_HPCDT01 TO IT_HPCS
  PERFORM DATA_GATHER_IT_HPCS.
************* 2003.11.06*********************
************* 2004.02.20*********************
* CALL FUNCTION Z_FPP_HANDLING_MASTER
*  PERFORM FUNCTION_DATA.

  PERFORM FUNCTION_DATA_01.
*
ENDFORM.                    " DATA_PROCESS
*&---------------------------------------------------------------------*
*&      Form  DATA_GATHER_IT_HPCS
*&---------------------------------------------------------------------*
FORM DATA_GATHER_IT_HPCS.
  DATA: L_TABIX TYPE SY-TABIX.

  REFRESH: IT_HPCS.  CLEAR: IT_HPCS.

  LOOP AT IT_HPCDT.
    MOVE-CORRESPONDING: IT_HPCDT TO IT_HPCS.
    APPEND IT_HPCS.
    CLEAR: IT_HPCDT, IT_HPCS.
  ENDLOOP.
*   2004.02.02 TABLE KEY CHAGNE
*  SORT IT_HPCS BY FSCC PQBG CEXT CINT SEQU.
  SORT IT_HPCS BY PQBG ZWORK CEXT CINT SEQU.
*  ZYEAR NATN MDIX PQBG OCNO VERS     CEXT CINT. " STAT

  LOOP AT IT_HPCDT01.
*   2004.02.02 TABLE KEY CHAGNE
    READ TABLE IT_HPCS WITH KEY PQBG   = IT_HPCDT01-PQBG
*    FSCC  = IT_HPCDT01-FSCC
                                ZWORK = IT_HPCDT01-ZWORK
                                CEXT   = IT_HPCDT01-CEXT
                                CINT   = IT_HPCDT01-CINT
                                SEQU   = IT_HPCDT01-SEQU
                      BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      L_TABIX = SY-TABIX.
      MOVE-CORRESPONDING IT_HPCDT01 TO IT_HPCS.
      MODIFY IT_HPCS INDEX L_TABIX .
    ELSE.

    ENDIF.
    CLEAR: IT_HPCDT01, IT_HPCS.
  ENDLOOP.
ENDFORM.                    " DATA_GATHER_IT_HPCS
*&---------------------------------------------------------------------*
*&      Form  FUNCTION_DATA
*&---------------------------------------------------------------------*
FORM FUNCTION_DATA.
  DATA: BEGIN OF LT_HPCS OCCURS 0.
          INCLUDE STRUCTURE IT_HPCS.
  DATA: MATNR TYPE MATNR.
  DATA: END OF LT_HPCS.
  DATA: BEGIN OF LT_MARA OCCURS 0,
          MATNR TYPE MATNR,
        END   OF LT_MARA.
  DATA: L_MATNR TYPE MATNR,
        L_TABIX TYPE SY-TABIX,
        L_ZMODE TYPE ZTBM_ABXHPCDT-ZMODE,
        LA_LINE_IDX TYPE I,
        LA_ERRO_IDX TYPE I.
  LT_HPCS[] = IT_HPCS[].
  REFRESH IT_HPCS. CLEAR IT_HPCS.
  LOOP AT LT_HPCS.
    L_TABIX = SY-TABIX.
    REFRESH LT_MARA.  CLEAR LT_MARA.
    PERFORM READ_MARA_MAKT TABLES   LT_MARA
                           USING    LT_HPCS-FSCC
                                    LT_HPCS-PQBG
                                    LT_HPCS-CEXT
                                    LT_HPCS-CINT
                                    LT_HPCS-SEQU
                           CHANGING LT_HPCS-MATNR
                                    LT_HPCS-ZBDAT
                                    LT_HPCS-ZBTIM
                                    LT_HPCS-ZBNAM
                                    LT_HPCS-ZMODE
                                    LT_HPCS-ZRESULT
                                    LT_HPCS-ZMSG.
    LOOP AT LT_MARA.
      REFRESH IT_VALUE. CLEAR IT_VALUE.
      PERFORM FUNCTION_DATA_APPEND TABLES   IT_VALUE
                                   USING    LT_HPCS
                                            LT_MARA-MATNR
                                   CHANGING L_ZMODE.
      CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
           EXPORTING
                OBJECT       = LT_MARA-MATNR
                MODE         = 'W'
                CTYPE        = '001'
           TABLES
                VAL_TABLE    = IT_VALUE
           EXCEPTIONS
                NO_DATA      = 1
                ERROR_MODE   = 2
                ERROR_OBJECT = 3
                ERROR_VALUE  = 4
                OTHERS       = 5.

      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
      CLEAR LT_MARA.
      LOOP AT IT_VALUE WHERE NOT ZFLAG IS INITIAL.
        LT_HPCS-ZRESULT = 'E'.
        CONCATENATE LT_HPCS-ZMSG IT_VALUE-ATNAM ' & ' IT_VALUE-ATWRT
                    'ERROR'      INTO LT_HPCS-ZMSG.
      ENDLOOP.
    ENDLOOP.
    IF LT_HPCS-ZRESULT EQ 'E'.
      LA_ERRO_IDX = LA_ERRO_IDX + 1.
      LT_HPCS-ZBDAT = SY-DATUM.
*      LT_HPCS-ZBDAT = SY-UZEIT.
      LT_HPCS-ZBNAM = SY-UNAME.
    ELSE.
      LT_HPCS-ZBDAT = SY-DATUM.
      LT_HPCS-ZBTIM = SY-UZEIT.
      LT_HPCS-ZBNAM = SY-UNAME.
      LT_HPCS-ZRESULT = 'S'.
      LT_HPCS-ZMSG = 'HPCS UPLOAD SUCCESS'.
    ENDIF.
    LT_HPCS-ZMODE = L_ZMODE.
    MODIFY LT_HPCS INDEX L_TABIX TRANSPORTING MATNR
                                              ZBDAT
                                              ZBTIM
                                              ZBNAM
                                              ZMODE
                                              ZRESULT
                                              ZMSG.
    CLEAR: LT_HPCS.
  ENDLOOP.
* WRITE ( TOTAL LINE & ERROR LINE & ERROR MESSAGE )
  DESCRIBE TABLE LT_HPCS LINES LA_LINE_IDX.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE: / TEXT-019,
           LA_LINE_IDX.
  WRITE: / TEXT-020,
            LA_ERRO_IDX.
  FORMAT COLOR OFF.
*  IF LA_ERRO_IDX GE '1'.
  SORT LT_HPCS BY ZRESULT FSCC MATNR.
  PERFORM ERROR_HEAD_WRITE.
  LOOP AT LT_HPCS.
    IF LT_HPCS-ZRESULT EQ 'E'.
      FORMAT COLOR COL_KEY INTENSIFIED OFF.
      PERFORM ERROR_WRITE USING    LT_HPCS.
      FORMAT COLOR OFF.
    ELSE.
      FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
      PERFORM ERROR_WRITE USING    LT_HPCS.
      FORMAT COLOR OFF.
    ENDIF.

  ENDLOOP.
*  ENDIF.

* MOVE-CORRESPONDING
  IT_HPCS[] = LT_HPCS[].

ENDFORM.                    " FUNCTION_DATA
*&---------------------------------------------------------------------*
*&      Form  DATA_DIVIDING
*&---------------------------------------------------------------------*
FORM DATA_DIVIDING.
  REFRESH: IT_HPCDT, IT_HPCDT01.  CLEAR: IT_HPCDT, IT_HPCDT01.
  LOOP AT IT_HPCS.
    IT_HPCDT-MANDT = IT_HPCDT01-MANDT = SY-MANDT.
    MOVE-CORRESPONDING: IT_HPCS TO IT_HPCDT,
                        IT_HPCS TO IT_HPCDT01.
    APPEND IT_HPCDT.
    APPEND IT_HPCDT01.
    CLEAR: IT_HPCDT, IT_HPCDT01, IT_HPCS.
  ENDLOOP.
ENDFORM.                    " DATA_DIVIDING
*&---------------------------------------------------------------------*
*&      Form  TABLE_ABXHPCDT_INSERT
*&---------------------------------------------------------------------*
FORM TABLE_ABXHPCDT_INSERT.
  WRITE: / TEXT-023.
  MODIFY ZTBM_ABXHPCDT FROM TABLE IT_HPCDT.
  IF SY-SUBRC EQ 0.
    MODIFY ZTBM_ABXHPCDT01 FROM TABLE IT_HPCDT01.
    IF SY-SUBRC EQ 0.
      COMMIT WORK.
      WRITE: / TEXT-021 COLOR 4.
    ELSE.
      ROLLBACK WORK.
      WRITE: / TEXT-022 COLOR 4.
    ENDIF.
  ELSE.
    ROLLBACK WORK.
    WRITE: / TEXT-022 COLOR 4.
  ENDIF.

ENDFORM.                    " TABLE_ABXHPCDT_INSERT
*&---------------------------------------------------------------------*
*&      Form  READ_MARA_MAKT
*&---------------------------------------------------------------------*
FORM READ_MARA_MAKT TABLES   PT_MARA
                    USING    P_FSCC
                             P_PQBG
                             P_CEXT
                             P_CINT
                             P_SEQU
                    CHANGING P_MATNR
                             P_ZBDAT
                             P_ZBTIM
                             P_ZBNAM
                             P_ZMODE
                             P_ZRESULT
                             P_ZMSG.

  DATA: BEGIN OF LA_MATNR,
          MATNR TYPE MARA-MATNR,
        END OF LA_MATNR.

  DATA: BEGIN OF LT_MARA OCCURS 0,
          MATNR TYPE MARA-MATNR,
          MTART TYPE MARA-MTART,
        END OF LT_MARA.

  DATA: L_MAKTX TYPE MAKTX,
        L_MATNR TYPE MATNR.
  L_MAKTX = P_FSCC.

  SELECT A~MATNR
         A~MTART
       FROM MARA AS A INNER JOIN MAKT AS B
                      ON A~MATNR EQ B~MATNR
       INTO TABLE LT_MARA
       WHERE B~SPRAS EQ SY-LANGU
       AND   B~MAKTX EQ L_MAKTX.
  IF SY-SUBRC EQ 0.
    SORT LT_MARA BY MATNR MTART.
    LOOP AT LT_MARA WHERE MTART EQ 'WOHD'.
      L_MATNR = LT_MARA-MATNR.
      CASE P_PQBG.
        WHEN 'P'.
          LA_MATNR-MATNR = P_MATNR = L_MATNR.
          APPEND LA_MATNR TO PT_MARA.
        WHEN 'Q'.
          CONCATENATE L_MATNR P_CEXT P_CINT INTO L_MATNR.

          READ TABLE LT_MARA WITH KEY MATNR = L_MATNR
                             BINARY SEARCH.
          IF SY-SUBRC EQ 0.
            LA_MATNR-MATNR = P_MATNR = LT_MARA-MATNR.
            APPEND LA_MATNR TO PT_MARA.
          ENDIF.
      ENDCASE.
    ENDLOOP.
  ELSE.
* ERROR MESSAGE WORK ORDER does not exist.
    P_ZBDAT = SY-DATUM.
    P_ZBNAM = SY-UNAME.
    P_ZMODE = 'C'.
    P_ZRESULT = 'E'.
    P_ZMSG = 'ERROR MESSAGE  WORK ORDER does not exist.'.
  ENDIF.
ENDFORM.                    " READ_MARA_MAKT
*&---------------------------------------------------------------------*
*&      Form  FUNCTION_DATA_APPEND
*&---------------------------------------------------------------------*
FORM FUNCTION_DATA_APPEND TABLES   PT_VALUE STRUCTURE IT_VALUE
                          USING    PT_HPCS
                                   P_MATNR
                          CHANGING P_ZMODE.
  DATA: BEGIN OF WA_HPCS.
          INCLUDE STRUCTURE IT_HPCS.
  DATA: MATNR TYPE MATNR.
  DATA: END OF WA_HPCS.
  DATA: L_ATFLV LIKE AUSP-ATFLV,
        L_ATINN LIKE AUSP-ATINN,
        L_INDEX(03) TYPE N,
        L_FIELD1(20),
        L_FIELD2(20).
  FIELD-SYMBOLS <FS>.
  WA_HPCS = PT_HPCS.
  SELECT SINGLE ATINN
            FROM CABN
            INTO L_ATINN
            WHERE ATNAM EQ 'P_H_GEN_DATE'.

  SELECT SINGLE ATFLV
              FROM AUSP
              INTO L_ATFLV
*              WHERE OBJEK EQ WA_HPCS-MATNR
              WHERE OBJEK EQ P_MATNR
              AND   ATINN EQ L_ATINN.
  IF SY-SUBRC EQ 0.
    IF L_ATFLV IS INITIAL.
      P_ZMODE = 'C'.
      PT_VALUE-ATNAM = 'P_H_GEN_DATE'.
      PT_VALUE-ATWRT = WA_HPCS-ZEDAT.
      APPEND PT_VALUE. CLEAR: PT_VALUE.
    ELSE.
      P_ZMODE = 'U'.
      PT_VALUE-ATNAM = 'P_H_MOD_DATE'.
      PT_VALUE-ATWRT = WA_HPCS-ZEDAT.
      APPEND PT_VALUE. CLEAR: PT_VALUE.
    ENDIF.
  ELSE.
    P_ZMODE = 'C'.
    PT_VALUE-ATNAM = 'P_H_GEN_DATE'.
    PT_VALUE-ATWRT = WA_HPCS-ZEDAT.
    APPEND PT_VALUE. CLEAR: PT_VALUE.
  ENDIF.

  PT_VALUE-ATNAM = 'P_HPC_STATUS'.
  PT_VALUE-ATWRT = 'H'.
  APPEND PT_VALUE. CLEAR: PT_VALUE.
*&--------------------------------------------------------------------&*
*&         Note by: Shiva & Catherine  - 12/02/2004.
*& At present we assumed that there will be only 20 characteristic
*& values for the characteristic 'P_WO_HPC_B'.
*&--------------------------------------------------------------------&*
  CASE WA_HPCS-PQBG.
    WHEN 'B'.
      DO 20 TIMES.
        L_INDEX = L_INDEX + 1.
        CONCATENATE 'P_WO_HPC_B' L_INDEX INTO L_FIELD1.
        CONCATENATE 'WA_HPCS-C' L_INDEX INTO L_FIELD2.
        ASSIGN (L_FIELD2) TO <FS>.
        PT_VALUE-ATNAM = L_FIELD1.
        PT_VALUE-ATWRT = <FS>.
        APPEND PT_VALUE. CLEAR: PT_VALUE.
        CLEAR: L_FIELD1, L_FIELD2, <FS>.
      ENDDO.
    WHEN 'P'.
      DO 400 TIMES.
        L_INDEX = L_INDEX + 1.
        CONCATENATE 'P_WO_HPC_P' L_INDEX INTO L_FIELD1.
        CONCATENATE 'WA_HPCS-C' L_INDEX INTO L_FIELD2.
        ASSIGN (L_FIELD2) TO <FS>.
        PT_VALUE-ATNAM = L_FIELD1.
        PT_VALUE-ATWRT = <FS>.
        APPEND PT_VALUE. CLEAR: PT_VALUE.
        CLEAR: L_FIELD1, L_FIELD2, <FS>.
      ENDDO.
    WHEN 'Q'.
      DO 100 TIMES.
        L_INDEX = L_INDEX + 1.
        CONCATENATE 'P_WO_HPC_Q' L_INDEX INTO L_FIELD1.
        CONCATENATE 'WA_HPCS-C' L_INDEX INTO L_FIELD2.
        ASSIGN (L_FIELD2) TO <FS>.
        PT_VALUE-ATNAM = L_FIELD1.
        PT_VALUE-ATWRT = <FS>.
        APPEND PT_VALUE. CLEAR: PT_VALUE.
        CLEAR: L_FIELD1, L_FIELD2, <FS>.
      ENDDO.
  ENDCASE.

ENDFORM.                    " FUNCTION_DATA_APPEND
*&---------------------------------------------------------------------*
*&      Form  ERROR_HEAD_WRITE
*&---------------------------------------------------------------------*
FORM ERROR_HEAD_WRITE.
*  WRITE: /(04) 'TYPE',
*          (15) 'Work order',
*          (15) 'EXTERNAL COLOR',
*          (15) 'INTERNAL COLOR',
*          (10) 'SEQUENCE',
*          (23) 'FSC',
*          (20) 'MATERIAL STATUS',
*          (20) 'SAP INTERFACE DATE',
*          (20) 'SAP INTERFACE TIME',
*          (20) 'SAP BDC EXECUTED DATE',
*          (20) 'SAP BDC EXECUTED TIME',
*          (05) 'MESSAGE TYEP',
*          (30) 'Create/Update/Delete',
*               'MESSAGE'.
  WRITE: /(04)  TEXT-005,
          (15)  TEXT-003,
          (15)  TEXT-006,
          (15)  TEXT-007,
          (10)  TEXT-008,
          (23)  TEXT-004,
          (20)  TEXT-009,
          (20)  TEXT-010,
          (20)  TEXT-011,
          (20)  TEXT-012,
          (20)  TEXT-013,
          (05)  TEXT-014,
          (30)  TEXT-015,
                TEXT-016.
ENDFORM.                    " ERROR_HEAD_WRITE
*&---------------------------------------------------------------------*
*&      Form  ERROR_WRITE
*&---------------------------------------------------------------------*
FORM ERROR_WRITE USING    PA_HPCS.
  DATA: BEGIN OF WA_HPCS.
          INCLUDE STRUCTURE IT_HPCS.
  DATA: MATNR TYPE MATNR.
  DATA: END OF WA_HPCS.

  WA_HPCS = PA_HPCS.

  WRITE: /(04) WA_HPCS-PQBG,   "TYPE
          (15) WA_HPCS-ZWORK,  "Work order
          (15) WA_HPCS-CEXT,   "EXTERNAL COLOR
          (15) WA_HPCS-CINT,   "INTERNAL COLOR
          (10) WA_HPCS-SEQU,   "SEQUENCE
          (23) WA_HPCS-FSCC,   "FULL SPEC
          (20) WA_HPCS-STAT,   "MATERIAL STATUS
          (20) WA_HPCS-ZEDAT,  "SAP BDC EXECUTED DATE
          (20) WA_HPCS-ZETIM,  "SAP BDC EXECUTED TIME
          (20) WA_HPCS-ZBDAT,  "SAP BDC EXECUTED DATE
          (20) WA_HPCS-ZBTIM,  "SAP BDC EXECUTED TIME
          (05) WA_HPCS-ZRESULT,
          (30) WA_HPCS-ZMODE,
               WA_HPCS-ZMSG.
ENDFORM.                    " ERROR_WRITE
*&---------------------------------------------------------------------*
*&      Form  FUNCTION_DATA_01
*&---------------------------------------------------------------------*
FORM FUNCTION_DATA_01.
  DATA: BEGIN OF LT_HPCS OCCURS 0.
          INCLUDE STRUCTURE IT_HPCS.
  DATA: MATNR TYPE MATNR.
  DATA: END OF LT_HPCS.
  DATA: L_MATNR TYPE MATNR,
        L_TABIX TYPE SY-TABIX,
        L_ZMODE TYPE ZTBM_ABXHPCDT-ZMODE,
        LA_LINE_IDX TYPE I,
        LA_ERRO_IDX TYPE I.
  LT_HPCS[] = IT_HPCS[].
  REFRESH IT_HPCS. CLEAR IT_HPCS.
  LOOP AT LT_HPCS.
    L_TABIX = SY-TABIX.
    REFRESH IT_VALUE. CLEAR IT_VALUE.

*   REQUESTED BY CATHERINE S. CHANGED BY CHRIS
*   NOTE: THE 'Q' TYPE HPC CODE SHOULD BE
*        UPLOADED TO WOCL MATERIALS
    DATA: L_OBJECT  LIKE MARA-MATNR.
    IF LT_HPCS-PQBG = 'Q'.
      CONCATENATE LT_HPCS-ZWORK
                   LT_HPCS-CEXT
                   LT_HPCS-CINT
             INTO  L_OBJECT.
    ELSE.
      L_OBJECT = LT_HPCS-ZWORK.
    ENDIF.
*   END OF CHANGE ON 03/07/2005


    PERFORM FUNCTION_DATA_APPEND TABLES   IT_VALUE
                                 USING    LT_HPCS
                                          L_OBJECT "LT_HPCS-ZWORK
                                 CHANGING L_ZMODE.


    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              OBJECT       = L_OBJECT  "LT_HPCS-ZWORK
              MODE         = 'W'
              CTYPE        = '001'
         TABLES
              VAL_TABLE    = IT_VALUE
         EXCEPTIONS
              NO_DATA      = 1
              ERROR_MODE   = 2
              ERROR_OBJECT = 3
              ERROR_VALUE  = 4
              OTHERS       = 5.

    IF SY-SUBRC <> 0.
      if sy-subrc eq 3.
        IF LT_HPCS-PQBG = 'Q'.
          MESSAGE I001 WITH 'Work order color doesn''t exists!'.
        ELSE.
          message i001 with 'Work order doesn''t exists !'.
        ENDIF.
      elseif sy-subrc eq 2.
        message i001 with 'Exception ERROR_MODE raised !'.
      endif.
    ENDIF.
    LOOP AT IT_VALUE WHERE NOT ZFLAG IS INITIAL.
      LT_HPCS-ZRESULT = 'E'.
      CONCATENATE LT_HPCS-ZMSG IT_VALUE-ATNAM ' & ' IT_VALUE-ATWRT
                  'ERROR'      INTO LT_HPCS-ZMSG.
    ENDLOOP.
    IF LT_HPCS-ZRESULT EQ 'E'.
      LA_ERRO_IDX = LA_ERRO_IDX + 1.
      LT_HPCS-ZBDAT = SY-DATUM.
*      LT_HPCS-ZBDAT = SY-UZEIT.
      LT_HPCS-ZBNAM = SY-UNAME.
    ELSE.
      LT_HPCS-ZBDAT = SY-DATUM.
      LT_HPCS-ZBTIM = SY-UZEIT.
      LT_HPCS-ZBNAM = SY-UNAME.
      LT_HPCS-ZRESULT = 'S'.
      LT_HPCS-ZMSG = 'HPCS UPLOAD SUCCESS'.
    ENDIF.
    LT_HPCS-ZMODE = L_ZMODE.
    MODIFY LT_HPCS INDEX L_TABIX TRANSPORTING MATNR
                                              ZBDAT
                                              ZBTIM
                                              ZBNAM
                                              ZMODE
                                              ZRESULT
                                              ZMSG.
    CLEAR: LT_HPCS.
  ENDLOOP.
* WRITE ( TOTAL LINE & ERROR LINE & ERROR MESSAGE )
  DESCRIBE TABLE LT_HPCS LINES LA_LINE_IDX.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE: / TEXT-019,
           LA_LINE_IDX.
  WRITE: / TEXT-020,
            LA_ERRO_IDX.
  FORMAT COLOR OFF.
*  IF LA_ERRO_IDX GE '1'.
  SORT LT_HPCS BY ZRESULT ZWORK MATNR.
  PERFORM ERROR_HEAD_WRITE.
  LOOP AT LT_HPCS.
    IF LT_HPCS-ZRESULT EQ 'E'.
      FORMAT COLOR COL_KEY INTENSIFIED OFF.
      PERFORM ERROR_WRITE USING    LT_HPCS.
      FORMAT COLOR OFF.
    ELSE.
      FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
      PERFORM ERROR_WRITE USING    LT_HPCS.
      FORMAT COLOR OFF.
    ENDIF.

  ENDLOOP.
*  ENDIF.

* MOVE-CORRESPONDING
  IT_HPCS[] = LT_HPCS[].

ENDFORM.                    " FUNCTION_DATA_01
