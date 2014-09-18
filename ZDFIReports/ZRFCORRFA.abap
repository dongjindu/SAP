*$*$--------------------------------------------------------------$*$*
*$ Correction Inst.         0120061532 0000607695                     $*
*$------------------------------------------------------------------$*
*$ Valid for       :                                                  $*
*$ Software Component   SAP_APPL   SAP Application                    $*
*$  Release 46C          All Support Package Levels                   $*
*$*$--------------------------------------------------------------$*$*
*&-------------------------------------------------------------------*
*& Object          REPS ZRFCORRFA
*& Object Header   PROG ZRFCORRFA
*&-------------------------------------------------------------------*
*& REPORT ZRFCORRFA
*&-------------------------------------------------------------------*
*>>>> START OF INSERTION <<<<
*&---------------------------------------------------------------------*
*& Report  RFCORRFA                                                    *
*&---------------------------------------------------------------------*
*& This report corrects the functional area in the BSEG table
*& (Field BSEG-FKBER):
*& The derivation of the functional area is processed anew and
*& the new functional area is then stored in the FI documents (in the
*& BSEG-table).
*&
*& The report works in the following way:
*& 1) The FI documents are selected from the BSEG and BKPF tables.
*& 2) The selected entries are transformed into ACCIT and ACCHD
*&    structures temporarily in the memory.
*& 3) The logic for deriving the functional area is processed
*&    (this logic for deriving the functional area works based on
*&    the ACCIT and ACCHD structures).
*&
*& This reports works well for all FI documents which were posted
*& directly in FI, e.g. using the FB01 transaction, since all
*& information of the posting is available in the BSEG and BKPF tables.
*&
*& But a wrong functional area might be derived for FI documents which
*& were originally posted in other modules, e.g. in MM:
*& For such FI documents the complete information of the original
*& MM posting is NOT available: The original ACCIT and ACCHD entries
*& cannot be recreated any more by using only the information which
*& is stored in the BSEG/BKPF tables.
*& Hence please use this report only for FI documents which were
*& posted in FI and not for those which were posted in other modules
*& like FI.
*& Please note: In most cases this report will work fine also for
*& FI documents from MM etc., since the most common fields like
*& account, cost center category etc. will be filled for all documents.
*& If the report does not fullfill your needs, maybe because
*& the derivation of functional areas for documents from MM
*& does not work as desired, please proceed as described in
*& OSS note nr. 115840.
*&---------------------------------------------------------------------*

REPORT ZFCORRFA .
*=======================================================================
* global data:
*=======================================================================

TYPE-POOLS: SLIS.

TABLES: BKPF, BSEG.

TYPES:
* same fields as below in GT_FINAL_LIST (->ALV):
BEGIN OF COS_BSEG_FKBER_CHANGE,
BUKRS       TYPE  BUKRS,
BELNR       TYPE  BELNR_D,
BUZEI       LIKE  BSEG-BUZEI,
GJAHR       TYPE  GJAHR,
FKBER_NEW   TYPE  FKBER,
FKBER_OLD   TYPE  FKBER,
FLAG_MSG    TYPE  FLAG,
FLAG_ERROR  TYPE  FLAG,
END OF COS_BSEG_FKBER_CHANGE,

COS_BSEG_FKBER_CHANGE_T  TYPE  TABLE OF  COS_BSEG_FKBER_CHANGE.

TYPES:
BEGIN OF TY_S_DOCTAB,
MANDT   TYPE  SY-MANDT,
BUKRS   TYPE  BSEG-BUKRS,
GJAHR   TYPE  BSEG-GJAHR,
BELNR   TYPE  BSEG-BELNR,
BUZEI   TYPE  BSEG-BUZEI,
END OF TY_S_DOCTAB,
TY_T_DOCTAB  TYPE  TABLE OF  TY_S_DOCTAB.

DATA:
BEGIN OF GS_COS_BSEG_FKBER_CHANGE,
BUKRS       LIKE  BSEG-BUKRS,
BELNR       LIKE  BSEG-BELNR,
BUZEI       LIKE  BSEG-BUZEI,
GJAHR       LIKE  BSEG-GJAHR,
FKBER_NEW   LIKE  BSEG-FKBER,
FKBER_OLD   LIKE  BSEG-FKBER,
FLAG_MSG    LIKE  BSEG-XBILK,
FLAG_ERROR  LIKE  BSEG-XBILK,
END OF GS_COS_BSEG_FKBER_CHANGE.

DATA:
GS_DOCTAB       TYPE  TY_S_DOCTAB,
GT_DOCTAB       TYPE  TY_T_DOCTAB,
GT_BKPF         LIKE  BKPF  OCCURS 0,
GT_BSEG         LIKE  BSEG  OCCURS 0,
GS_BKPF         LIKE  BKPF,
GS_BSEG         LIKE  BSEG,
GT_ACCHD_TEMP   LIKE  ACCHD OCCURS 0 WITH HEADER LINE,
GT_ACCIT_TEMP   LIKE  ACCIT OCCURS 0 WITH HEADER LINE,
GT_ACCCR_TEMP   LIKE  ACCCR OCCURS 0 WITH HEADER LINE.

DATA:
GD_ZEILE(22),
GD_SUBRC              TYPE  SY-SUBRC,
GD_CURSOR_BKPF        TYPE  CURSOR,      "Cursor for SELECT BSEG
GD_OLD_FAREA          TYPE  BSEG-FKBER,
GD_NEW_FAREA          TYPE  BSEG-FKBER,
GD_FLAG_ERROR_RECORD  TYPE  BOOLE-BOOLE,
GD_COUNTER_PROCESSED  TYPE  I,        "number of processed BSEGs
GD_COUNTER_CHANGED    TYPE  I,        "number of BSEGs with chaged Farea
GD_COUNTER_MSG_ALL    TYPE  I,        "number of all appeared messages
GD_MAX_SEVERITY_REC   LIKE  SY-SUBRC, "severity of mesgs of record
GD_REC_CORRECTED      LIKE  SY-TFILL. "number of BSEG rec. corrected

* structure of final list:
DATA:
GT_FINAL_LIST   TYPE  COS_BSEG_FKBER_CHANGE_T WITH HEADER LINE.

*=======================================================================
* selection screen:
*=======================================================================

* key fields of BSEG/BKPF-table:
SELECT-OPTIONS:
  COMPCODE  FOR  BKPF-BUKRS MEMORY ID BUK,
  FISCYEAR  FOR  BKPF-GJAHR MEMORY ID GJR,
  DOCNR     FOR  BKPF-BELNR MEMORY ID BLN,
  LINEITEM  FOR  BSEG-BUZEI MEMORY ID BUZ.
* select options for header:
SELECTION-SCREEN BEGIN OF BLOCK A WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:
  DOCTYPE   FOR BKPF-BLART MEMORY ID BAR,
  PERIOD    FOR BKPF-MONAT MEMORY ID PER,   "memory id = ?
  ACITIVTY  FOR BKPF-GLVOR MEMORY ID ACT,   "memory id = ?
  REFPROC   FOR BKPF-AWTYP MEMORY ID RPR,   "memory id = ?
  REFKEY    FOR BKPF-AWKEY MEMORY ID RKY.   "memory id = ?
SELECTION-SCREEN END OF BLOCK A.

* control parameters:
SELECTION-SCREEN BEGIN OF BLOCK C WITH FRAME  TITLE TEXT-003.
PARAMETERS:
  TEST_RUN    LIKE  COFI_SCR-FLG_TEST  DEFAULT 'X',
  SHOW_LST    LIKE  COFI_SCR-FLG_LIST  DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK C.

*=======================================================================
START-OF-SELECTION.
*=======================================================================

  CALL FUNCTION 'MESSAGES_INITIALIZE'.

  OPEN CURSOR WITH HOLD GD_CURSOR_BKPF
  FOR SELECT * FROM BKPF
         WHERE  BUKRS       IN  COMPCODE
         AND    GJAHR       IN  FISCYEAR
         AND    BELNR       IN  DOCNR
         AND    BLART IN  DOCTYPE
         AND    MONAT IN  PERIOD
         AND    GLVOR IN  ACITIVTY

          AND    AWTYP IN  REFPROC
          AND    AWKEY IN  REFKEY
          ORDER BY PRIMARY KEY.

* start processing:
  DO.
    REFRESH: GT_BKPF,  GT_BSEG.
    CLEAR:   GT_BKPF,  GT_BSEG.

    FETCH NEXT CURSOR GD_CURSOR_BKPF  INTO TABLE  GT_BKPF

       PACKAGE SIZE '20'.

    IF ( SY-SUBRC <> 0 ).
      CLOSE CURSOR GD_CURSOR_BKPF.
      EXIT.
    ENDIF.

*   select corresponding headers:
    SELECT * FROM BSEG
             INTO  TABLE GT_BSEG
             FOR ALL ENTRIES IN GT_BKPF
             WHERE  BUKRS = GT_BKPF-BUKRS
             AND    GJAHR = GT_BKPF-GJAHR
             AND    BELNR = GT_BKPF-BELNR
             AND    BUZEI IN  LINEITEM.

*   keep only those BSEG entries which match with the entered
*   selection criteria:
    LOOP AT GT_BSEG  INTO  GS_BSEG.

*     init some data:
      CLEAR: GD_FLAG_ERROR_RECORD.

      GD_OLD_FAREA = GS_BSEG-FKBER.
*     default: no change:

      GD_NEW_FAREA = GS_BSEG-FKBER.

      PERFORM FILL_MSG_IDENTIFIER
              USING     GS_BSEG-BUKRS
                        GS_BSEG-BELNR
                        GS_BSEG-GJAHR
              CHANGING  GD_ZEILE.

*     set header for messages:
      CALL FUNCTION 'MESSAGE_LINE_SET'
           EXPORTING
                ZEILE = GD_ZEILE.

      IF ( GS_BKPF-BUKRS <> GS_BSEG-BUKRS ) OR
         ( GS_BKPF-BELNR <> GS_BSEG-BELNR ) OR
         ( GS_BKPF-GJAHR <> GS_BSEG-GJAHR ).

*       read header:
        READ  TABLE GT_BKPF  INTO  GS_BKPF
                             WITH KEY  BUKRS = GS_BSEG-BUKRS
                                       BELNR = GS_BSEG-BELNR
                                       GJAHR = GS_BSEG-GJAHR
                                 BINARY SEARCH.
        IF ( SY-SUBRC <> 0 ).
*         no header found; error!
          CALL FUNCTION 'MESSAGE_STORE'
               EXPORTING
                    MSGTY = 'E'
                    TXTNR = '011'
                    ARBGB = 'K5'
                    MSGV1 =
                       'No Header for comcode/docnr/year:'  "#EC NOTEXT
                    MSGV2 = GS_BSEG-BUKRS
                    MSGV3 = GS_BSEG-BELNR
                    MSGV4 = GS_BSEG-GJAHR.
*         do not process this record:
          CONTINUE.
        ENDIF.
      ENDIF.
*     gt_bkpf contains now the header which corresponds to the
*     gt_bseg entry:
*     process it (derive new functional area):

*     functional area will only be derived anew, if it is
*     initial:
      GD_OLD_FAREA = GS_BSEG-FKBER.
      CLEAR GS_BSEG-FKBER.
*     transform  BSEG/BKPF  to  ACCIT/ACCHD:
      PERFORM BSEG_TO_ACCIT_TRANSFORM
              TABLES    GT_ACCHD_TEMP
                        GT_ACCIT_TEMP
                        GT_ACCCR_TEMP
              USING     GS_BKPF
                        GS_BSEG
              CHANGING  GD_SUBRC.

      IF ( GD_SUBRC <> 0 ).
*       error during transforming BSEG/BKPF into  ACCIT/ACCHD:
*       do not process this record:
        CONTINUE.
      ENDIF.
*     BSEG/BKPF successfully transformed into  ACCIT/ACCHD;
*     let's derive new functional area:
      CALL FUNCTION 'AC_DOCUMENT_FAREA_SET'
           TABLES
                T_ACCHD       = GT_ACCHD_TEMP
                T_ACCIT       = GT_ACCIT_TEMP
           EXCEPTIONS
                ERROR_MESSAGE = 1
                OTHERS        = 2.
      IF ( SY-SUBRC <> 0 ).
        CALL FUNCTION 'MESSAGE_STORE'
             EXPORTING
                  MSGTY = SYST-MSGTY
                  TXTNR = SYST-MSGNO
                  ARBGB = SYST-MSGID
                  MSGV1 = SYST-MSGV1
                  MSGV2 = SYST-MSGV2
                  MSGV3 = SYST-MSGV3
                  MSGV4 = SYST-MSGV4.
      ENDIF.      "SY-SUBRC <> 0
*     check if error occurred:
      CALL FUNCTION 'MESSAGES_COUNT'
           EXPORTING
                LINE_FROM     = GD_ZEILE
                LINE_TO       = GD_ZEILE
           IMPORTING
                MAX_SEVERITY  = GD_MAX_SEVERITY_REC
           EXCEPTIONS
                ERROR_MESSAGE = 0
                OTHERS        = 0.
      IF ( GD_MAX_SEVERITY_REC > 8 ).
        GD_FLAG_ERROR_RECORD  = 'X'.
      ELSE.
        CLEAR GD_FLAG_ERROR_RECORD.
      ENDIF.
      READ TABLE  GT_ACCIT_TEMP  INDEX 1.
      IF ( SY-SUBRC = 0 ).
*       entry found in ACCIT (a must!):
        GD_NEW_FAREA = GT_ACCIT_TEMP-FKBER.
        IF ( GD_NEW_FAREA <> GD_OLD_FAREA ).
*         functional area changed;
          IF ( TEST_RUN             = ' ' ) AND
             ( GD_FLAG_ERROR_RECORD = ' ' ).      "no errors so far
*           no testrun; update database:
            UPDATE  BSEG SET  FKBER = GD_NEW_FAREA
                    WHERE  BUKRS = GS_BSEG-BUKRS
                    AND    BELNR = GS_BSEG-BELNR
                    AND    GJAHR = GS_BSEG-GJAHR
                    AND    BUZEI = GS_BSEG-BUZEI.

            IF ( SY-SUBRC <> 0 ).
*             error during update:
              GD_FLAG_ERROR_RECORD = 'X'.
              CALL FUNCTION 'MESSAGE_STORE'
                   EXPORTING
                        MSGTY = 'E'
                        TXTNR = '011'
                        ARBGB = 'K5'
                        MSGV1 = 'DATABASE UPDATE'           "#EC NOTEXT
                        MSGV2 = 'FAILED FOR DOCUMENT'       "#EC NOTEXT
                        MSGV3 = GD_ZEILE
                        MSGV4 = ''.
            ENDIF.      "error during update

            UPDATE  BSIS SET  FKBER = GD_NEW_FAREA
                    WHERE  BUKRS  = GS_BSEG-BUKRS
                    AND    HKONT  = GS_BSEG-HKONT
                    AND    AUGDT  = GS_BSEG-AUGDT
                    AND    AUGBL  = GS_BSEG-AUGBL
                    AND    ZUONR  = GS_BSEG-ZUONR
                    AND    GJAHR  = GS_BSEG-GJAHR
                    AND    BELNR  = GS_BSEG-BELNR
                    AND    BUZEI  = GS_BSEG-BUZEI.

            UPDATE  BSAS SET  FKBER = GD_NEW_FAREA
                    WHERE  BUKRS  = GS_BSEG-BUKRS
                    AND    HKONT  = GS_BSEG-HKONT
                    AND    AUGDT  = GS_BSEG-AUGDT
                    AND    AUGBL  = GS_BSEG-AUGBL
                    AND    ZUONR  = GS_BSEG-ZUONR
                    AND    GJAHR  = GS_BSEG-GJAHR
                    AND    BELNR  = GS_BSEG-BELNR
                    AND    BUZEI  = GS_BSEG-BUZEI.

            UPDATE  BSIK SET  FKBER = GD_NEW_FAREA
                    WHERE  BUKRS   = GS_BSEG-BUKRS
                    AND    LIFNR   = GS_BSEG-HKONT
                    AND    UMSKS   = GS_BSEG-UMSKS
             AND    UMSKZ   = GS_BSEG-UMSKZ
             AND    AUGDT   = GS_BSEG-AUGDT
             AND    AUGBL   = GS_BSEG-AUGBL
             AND    ZUONR   = GS_BSEG-ZUONR
             AND    GJAHR   = GS_BSEG-GJAHR
             AND    BELNR   = GS_BSEG-BELNR
             AND    BUZEI   = GS_BSEG-BUZEI.

            UPDATE  BSAK SET  FKBER = GD_NEW_FAREA
                    WHERE  BUKRS   = GS_BSEG-BUKRS
                    AND    LIFNR   = GS_BSEG-HKONT
                    AND    UMSKS   = GS_BSEG-UMSKS
                    AND    UMSKZ   = GS_BSEG-UMSKZ
                    AND    AUGDT   = GS_BSEG-AUGDT
                    AND    AUGBL   = GS_BSEG-AUGBL
                    AND    ZUONR   = GS_BSEG-ZUONR
                    AND    GJAHR   = GS_BSEG-GJAHR
                    AND    BELNR   = GS_BSEG-BELNR
                    AND    BUZEI   = GS_BSEG-BUZEI.

            UPDATE  BSID SET  FKBER = GD_NEW_FAREA
                    WHERE  BUKRS   = GS_BSEG-BUKRS
                    AND    KUNNR   = GS_BSEG-KUNNR
                    AND    UMSKS   = GS_BSEG-UMSKS
                    AND    UMSKZ   = GS_BSEG-UMSKZ
                    AND    AUGDT   = GS_BSEG-AUGDT
                    AND    AUGBL   = GS_BSEG-AUGBL
                    AND    ZUONR   = GS_BSEG-ZUONR
                    AND    GJAHR   = GS_BSEG-GJAHR
                    AND    BELNR   = GS_BSEG-BELNR
                    AND    BUZEI   = GS_BSEG-BUZEI.

            UPDATE  BSAD SET  FKBER = GD_NEW_FAREA
                    WHERE  BUKRS   = GS_BSEG-BUKRS
                    AND    KUNNR   = GS_BSEG-KUNNR
                    AND    UMSKS   = GS_BSEG-UMSKS
                    AND    UMSKZ   = GS_BSEG-UMSKZ
                    AND    AUGDT   = GS_BSEG-AUGDT
                    AND    AUGBL   = GS_BSEG-AUGBL
                    AND    ZUONR   = GS_BSEG-ZUONR
                    AND    GJAHR   = GS_BSEG-GJAHR
                    AND    BELNR   = GS_BSEG-BELNR
                    AND    BUZEI   = GS_BSEG-BUZEI.

          ENDIF.        "no test run
          ADD 1 TO  GD_COUNTER_CHANGED.
        ENDIF.          "farea changed
      else.
*      entry not found in accit_temp:
        CALL FUNCTION 'MESSAGE_STORE'
             EXPORTING
                  MSGTY = 'E'
                  TXTNR = '011'
                  ARBGB = 'K5'
                  MSGV1 = 'NO ENTRY FOUND'                  "#EC NOTEXT
                  MSGV2 = 'IN ACCIT FOR DOCUMENT'           "#EC NOTEXT
                  MSGV3 = GD_ZEILE
                  MSGV4 = ''.
      ENDIF.            "entry found in accit_temp
      ADD 1  TO  GD_COUNTER_PROCESSED.
*     fill final list for display:
      IF ( SHOW_LST = 'X' ).
*       final list wanted:
        PERFORM FILL_FINAL_LIST
                TABLES  GT_FINAL_LIST
                USING   GS_BSEG
                        GD_OLD_FAREA          "old functional area
                        GD_NEW_FAREA          "new functional area
                        GD_FLAG_ERROR_RECORD
                        GD_ZEILE.
      ENDIF.      "final list wanted
    ENDLOOP.
*   execute database commit every 1000 entries:
    CALL FUNCTION 'DB_COMMIT'.
  ENDDO.

*=======================================================================
END-OF-SELECTION.
*=======================================================================

  IF ( SY-BATCH = 'X' ).
*   make sure that messages are written into job log:
    CALL FUNCTION 'MESSAGES_SHOW'
         EXCEPTIONS
              NO_MESSAGES = 0.
  ENDIF.

* display final list:
  IF ( SHOW_LST = 'X' ).
*   display final list:
    PERFORM DISPLAY_FINAL_LIST
            TABLES  GT_FINAL_LIST
            USING   TEST_RUN.
  ELSE.
*   write only a header:
    PERFORM ALV_TOP_OF_LIST.
  ENDIF.


*=======================================================================
*&      Form  BSEG_TO_ACCIT_TRANSFORM
*&                                                                     *
FORM BSEG_TO_ACCIT_TRANSFORM
     TABLES   ET_ACCHD_TEMP  STRUCTURE  ACCHD
              ET_ACCIT_TEMP  STRUCTURE  ACCIT
              ET_ACCCR_TEMP  STRUCTURE  ACCCR
     USING    IS_BKPF        LIKE       BKPF
              IS_BSEG        LIKE       BSEG
     CHANGING ED_SUBRC       LIKE       SY-SUBRC.

  FIELD-SYMBOLS:
  <LS_ACCIT>  TYPE  ACCIT.

  DATA:
  LT_BKPF_TEMP    LIKE  BKPF  OCCURS 0 WITH HEADER LINE,
  LT_BSEG_TEMP    LIKE  BSEG  OCCURS 0 WITH HEADER LINE,
  LS_AUFKV        LIKE  AUFKV.

  REFRESH: ET_ACCHD_TEMP, ET_ACCIT_TEMP.
  CLEAR:   ET_ACCHD_TEMP, ET_ACCIT_TEMP.

  CLEAR ED_SUBRC.

  APPEND:  IS_BKPF  TO  LT_BKPF_TEMP,
           IS_BSEG  TO  LT_BSEG_TEMP.

  CALL FUNCTION 'FI_DOC_TO_ACC_TRANSFORM'
       TABLES
            T_BKPF        = LT_BKPF_TEMP
            T_BSEG        = LT_BSEG_TEMP
            T_ACCHD       = ET_ACCHD_TEMP
            T_ACCIT       = ET_ACCIT_TEMP
            T_ACCCR       = ET_ACCCR_TEMP
       EXCEPTIONS
            ERROR_MESSAGE = 1
            OTHERS        = 2.

  IF ( SY-SUBRC <> 0 ).
*   error during transforming BSEG/BKPF into  ACCIT/ACCHD:
    CALL FUNCTION 'MESSAGE_STORE'
         EXPORTING
              MSGTY = SYST-MSGTY
              TXTNR = SYST-MSGNO
              ARBGB = SYST-MSGID
              MSGV1 = SYST-MSGV1
              MSGV2 = SYST-MSGV2
              MSGV3 = SYST-MSGV3
              MSGV4 = SYST-MSGV4.
    ED_SUBRC = 1.
    EXIT.
  ENDIF.      "sy-subrc <> 0

  IF ( NOT  IS_BSEG-AUFNR  IS  INITIAL ).

*   order assigned: fill also field ACCIT-AUTYP which is not
*   filled by the above function module:
    CALL FUNCTION 'K_ORDER_READ'
         EXPORTING
              AUFNR         = IS_BSEG-AUFNR
         IMPORTING
              I_AUFKV       = LS_AUFKV
         EXCEPTIONS
              ERROR_MESSAGE = 1
                                                                 Page 12

              OTHERS        = 2.
    IF ( SY-SUBRC <> 0 ).
      CALL FUNCTION 'MESSAGE_STORE'
           EXPORTING
                MSGTY = SYST-MSGTY
                TXTNR = SYST-MSGNO
                ARBGB = SYST-MSGID
                MSGV1 = SYST-MSGV1
                MSGV2 = SYST-MSGV2
                MSGV3 = SYST-MSGV3
                MSGV4 = SYST-MSGV4.
      ED_SUBRC = 1.
    ELSE.
*     fill autyp:
      READ TABLE ET_ACCIT_TEMP  INDEX 1  ASSIGNING  <LS_ACCIT>.
      IF ( SY-SUBRC = 0 ).
        <LS_ACCIT>-AUTYP = LS_AUFKV-AUTYP.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    "BSEG_TO_ACCIT_TRANSFORM
*&                                                                     *
*&      Form  FILL_MSG_IDENTIFIER
*&                                                                     *

FORM FILL_MSG_IDENTIFIER
     USING     ID_COMP_CODE  LIKE  BSEG-BUKRS
               ID_DOCNR      LIKE  BSEG-BELNR
               ID_YEAR       LIKE  BSEG-GJAHR
     CHANGING  ED_MSG_IDENT  TYPE  CHAR22.

  CLEAR ED_MSG_IDENT.

  ED_MSG_IDENT    = ID_COMP_CODE.
  ED_MSG_IDENT+4  = '/'.
  ED_MSG_IDENT+5  = ID_DOCNR.
  ED_MSG_IDENT+15 = '/'.
  ED_MSG_IDENT+16 = ID_YEAR.

ENDFORM.                    "FILL_MSG_IDENTIFIER
*&                                                                     *
*&      Form  FILL_FINAL_LIST
*&                                                                     *
FORM FILL_FINAL_LIST
     TABLES   CT_FINAL_LIST  TYPE       COS_BSEG_FKBER_CHANGE_T

     USING    IS_BSEG        LIKE       BSEG
              ID_OLD_FAREA   LIKE       BSEG-FKBER
              ID_NEW_FAREA   LIKE       BSEG-FKBER
              ID_FLAG_ERROR  LIKE       BOOLE-BOOLE
              ID_MSG_IDENT   TYPE       CHAR22.

  DATA:
  LS_FINAL_LIST  TYPE  COS_BSEG_FKBER_CHANGE,
  LD_COUNT       LIKE  SY-TABIX.

  MOVE-CORRESPONDING  IS_BSEG  TO  LS_FINAL_LIST.
  LS_FINAL_LIST-FKBER_OLD  = ID_OLD_FAREA.
  LS_FINAL_LIST-FKBER_NEW  = ID_NEW_FAREA.
  LS_FINAL_LIST-FLAG_ERROR = ID_FLAG_ERROR.

* check if there are messages:
  CALL FUNCTION 'MESSAGES_COUNT'
       EXPORTING
            LINE_FROM     = ID_MSG_IDENT
            LINE_TO       = ID_MSG_IDENT
       IMPORTING
            COUNT         = LD_COUNT
       EXCEPTIONS
            ERROR_MESSAGE = 0
            OTHERS        = 0.

  IF ( LD_COUNT > 0 ).
    LS_FINAL_LIST-FLAG_MSG = 'X'.
  ENDIF.

  APPEND  LS_FINAL_LIST  TO  CT_FINAL_LIST.

ENDFORM.                    "FILL_FINAL_LIST
*&                                                                     *
*&      Form  DISPLAY_FINAL_LIST
*&                                                                     *
FORM DISPLAY_FINAL_LIST
     TABLES   IT_FINAL_LIST   TYPE  COS_BSEG_FKBER_CHANGE_T
     USING    ID_TEST_RUN     LIKE  BOOLE-BOOLE.

  DATA:
  LT_FIELDCAT     TYPE SLIS_FIELDCAT_ALV OCCURS 0 WITH HEADER LINE,
  LS_ALV_COMMANDS TYPE SLIS_EVENT_EXIT,
  LT_ALV_COMMANDS TYPE SLIS_T_EVENT_EXIT,
  LS_ALV_EVENTS   TYPE SLIS_ALV_EVENT,
  LT_ALV_EVENTS   TYPE SLIS_T_EVENT,
  LS_LAYOUT       TYPE SLIS_LAYOUT_ALV,
  LS_VARIANT      LIKE DISVARIANT,
  LD_REPID        LIKE SY-REPID.

  LD_REPID        = SY-REPID.
* Fill table for events, that will be processed in this FM:
* The header of the alv list is writen in the form 'ALV_TOP_OF_LIST'
  LS_ALV_EVENTS-NAME = 'TOP_OF_LIST'.
  LS_ALV_EVENTS-FORM = 'ALV_TOP_OF_LIST'.
  APPEND LS_ALV_EVENTS TO LT_ALV_EVENTS.

  LS_LAYOUT-F2CODE = 'CHOOSE'.

* Fill fieldcatalog from ddic strucure cofi_iccf_alv
* This catalog (itab) contains information on the fields to write
* on the list.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            I_PROGRAM_NAME     = LD_REPID
            I_INCLNAME         = LD_REPID
            I_INTERNAL_TABNAME = 'GS_COS_BSEG_FKBER_CHANGE'
       CHANGING
            CT_FIELDCAT        = LT_FIELDCAT[].

  SORT  GT_FINAL_LIST  BY  BUKRS  GJAHR BELNR  BUZEI.
  PERFORM MODIFY_FIELDCAT_FINAL
          TABLES LT_FIELDCAT
                 GT_FINAL_LIST.

* now show the final list with posted records
  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
       EXPORTING
            I_CALLBACK_PROGRAM      = LD_REPID
            I_CALLBACK_USER_COMMAND = 'ALV_COMMANDS_LIST'
            IT_FIELDCAT             = LT_FIELDCAT[]
            IT_EVENTS               = LT_ALV_EVENTS
            IS_LAYOUT               = LS_LAYOUT
            IS_VARIANT              = LS_VARIANT
            I_SAVE                  = 'A'  "user def. list var.
            IT_EVENT_EXIT           = LT_ALV_COMMANDS
       TABLES
            T_OUTTAB                = GT_FINAL_LIST.

ENDFORM.                    "DISPLAY_FINAL_LIST
*&                                                                     *
*&      Form  MODIFY_FIELDCAT_FINAL
*&                                                                     *
FORM MODIFY_FIELDCAT_FINAL
     TABLES CT_FIELDCAT       TYPE  SLIS_T_FIELDCAT_ALV
            IT_FINAL_LIST     TYPE  COS_BSEG_FKBER_CHANGE_T.

  FIELD-SYMBOLS:
  <LS_FIELDCAT>  TYPE  LINE OF SLIS_T_FIELDCAT_ALV.

  LOOP AT  CT_FIELDCAT  ASSIGNING  <LS_FIELDCAT>.

    IF ( <LS_FIELDCAT>-FIELDNAME = 'FLAG_MSG' ).
      <LS_FIELDCAT>-OUTPUTLEN = 10.
      <LS_FIELDCAT>-HOTSPOT = 'X'.
      <LS_FIELDCAT>-SELTEXT_L = 'Messages exist'.
      <LS_FIELDCAT>-SELTEXT_M = 'Messages exist'.
      <LS_FIELDCAT>-SELTEXT_S = 'Messages'.
      CLEAR: <LS_FIELDCAT>-REF_FIELDNAME, <LS_FIELDCAT>-REF_TABNAME,
             <LS_FIELDCAT>-REPTEXT_DDIC, <LS_FIELDCAT>-DDIC_OUTPUTLEN.
    ELSEIF ( <LS_FIELDCAT>-FIELDNAME = 'FLAG_ERROR' ).
      <LS_FIELDCAT>-OUTPUTLEN = 10.
      <LS_FIELDCAT>-SELTEXT_L = 'Errors occurred'.
      <LS_FIELDCAT>-SELTEXT_M = 'Errors occurred'.
      <LS_FIELDCAT>-SELTEXT_S = 'Errors'.
      CLEAR: <LS_FIELDCAT>-REF_FIELDNAME, <LS_FIELDCAT>-REF_TABNAME,
             <LS_FIELDCAT>-REPTEXT_DDIC, <LS_FIELDCAT>-DDIC_OUTPUTLEN.
    ELSEIF ( <LS_FIELDCAT>-FIELDNAME = 'FKBER_NEW' ).
      <LS_FIELDCAT>-OUTPUTLEN = 16.
      <LS_FIELDCAT>-SELTEXT_L = 'New FuncArea'.
      <LS_FIELDCAT>-SELTEXT_M = 'New FuncArea'.
      <LS_FIELDCAT>-SELTEXT_S = 'New FArea'.
      CLEAR: <LS_FIELDCAT>-REF_FIELDNAME, <LS_FIELDCAT>-REF_TABNAME,
             <LS_FIELDCAT>-REPTEXT_DDIC, <LS_FIELDCAT>-DDIC_OUTPUTLEN.
    ELSEIF ( <LS_FIELDCAT>-FIELDNAME = 'FKBER_OLD' ).
      <LS_FIELDCAT>-OUTPUTLEN = 16.
      <LS_FIELDCAT>-SELTEXT_L = 'Old FuncArea'.
      <LS_FIELDCAT>-SELTEXT_M = 'Old FuncArea'.
      <LS_FIELDCAT>-SELTEXT_S = 'Old FArea'.
      CLEAR: <LS_FIELDCAT>-REF_FIELDNAME, <LS_FIELDCAT>-REF_TABNAME,
             <LS_FIELDCAT>-REPTEXT_DDIC, <LS_FIELDCAT>-DDIC_OUTPUTLEN.
    ELSEIF ( <LS_FIELDCAT>-FIELDNAME = 'BELNR' ).
      <LS_FIELDCAT>-HOTSPOT = 'X'.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " MODIFY_FIELDCAT_FINAL

*&                                                                     *
*&    Form  ALV_COMMANDS_LIST
*&                                                                     *
FORM ALV_COMMANDS_LIST                                      "#EC CALLED
     USING ID_UCOMM     LIKE  SY-UCOMM
           IS_SELFIELD  TYPE  SLIS_SELFIELD.

  CASE ID_UCOMM.
    WHEN 'CHOOSE'.
*     F2 on any field:
      CASE  IS_SELFIELD-SEL_TAB_FIELD.
        WHEN 'GT_FINAL_LIST-FLAG_MSG'.
*         user clicked on a record of list:
          READ TABLE GT_FINAL_LIST  INDEX  IS_SELFIELD-TABINDEX.
          IF ( SY-SUBRC = 0 ).
            PERFORM FILL_MSG_IDENTIFIER
                    USING     GT_FINAL_LIST-BUKRS
                              GT_FINAL_LIST-BELNR
                              GT_FINAL_LIST-GJAHR
                    CHANGING  GD_ZEILE.
*           display messages for this record:
            CALL FUNCTION 'MESSAGES_SHOW'
                 EXPORTING
                      LINE_FROM = GD_ZEILE
                      LINE_TO   = GD_ZEILE.
          ENDIF.
        WHEN 'GD_COUNTER_MSG_ALL'.
*         display messages:
          CALL FUNCTION 'MESSAGES_SHOW'.
        WHEN 'GT_FINAL_LIST-BELNR'.
*         display FI document:
          READ TABLE GT_FINAL_LIST  INDEX  IS_SELFIELD-TABINDEX.
          IF ( SY-SUBRC = 0 ).
            SET PARAMETER ID 'BLN' FIELD GT_FINAL_LIST-BELNR.
            SET PARAMETER ID 'BUK' FIELD GT_FINAL_LIST-BUKRS.
            SET PARAMETER ID 'GJR' FIELD GT_FINAL_LIST-GJAHR.
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          ENDIF.
      ENDCASE.
    WHEN OTHERS.

  ENDCASE.

ENDFORM.                    "ALV_COMMANDS_LIST
*&
*&    Form  ALV_TOP_OF_LIST
*&
FORM ALV_TOP_OF_LIST.                                       "#EC CALLED

  WRITE: /.
  WRITE: / 'Verarbeitungsart'(100).
  IF ( TEST_RUN IS INITIAL ).
    WRITE AT 40  'Echtlauf'(101).
  ELSE.
    WRITE AT 40  'Testlauf'(102).
  ENDIF.

* check if there are messages:
  CALL FUNCTION 'MESSAGES_COUNT'
       IMPORTING
            COUNT         = GD_COUNTER_MSG_ALL
       EXCEPTIONS
            ERROR_MESSAGE = 0
            OTHERS        = 0.

  WRITE: / 'Anzahl Meldungen'(110).

  FORMAT HOTSPOT ON.
  WRITE AT 40  GD_COUNTER_MSG_ALL LEFT-JUSTIFIED.
  FORMAT HOTSPOT OFF.

  WRITE: / 'Anzahl bearbeiteter Belegpos.'(120).
  WRITE AT 40  GD_COUNTER_PROCESSED LEFT-JUSTIFIED.

  WRITE: / 'Anzahl geanderter Belegpos.'(130).
  WRITE AT 40  GD_COUNTER_CHANGED LEFT-JUSTIFIED.


  WRITE: /.

ENDFORM.                    "ALV_TOP_OF_LIST

*>>>> END OF INSERTION <<<<<<
