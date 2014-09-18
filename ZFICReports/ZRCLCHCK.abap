*&---------------------------------------------------------------------*
*& Report  ZRCLCHCK                                                    *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  ZRCLCHCK LINE-SIZE 250.
* changing history:
* -----------------
* SCT 02052000 note 202357 corrected
* ----------------------------------

*=======================================================================
* global data:
*=======================================================================
TABLES:
COFIS, BKPF, BSEG.

DATA:
GS_TKA01 LIKE  TKA01,
GT_BKPF  LIKE  BKPF OCCURS 0 WITH HEADER LINE,
GT_BSEG  LIKE  BSEG OCCURS 0 WITH HEADER LINE,
GT_COFIS LIKE COFIS OCCURS 0 WITH HEADER LINE,
GD_STRING(20),
GD_MESS_COUNT  LIKE SY-TFILL,
GD_HIDDEN_FIELD(120)  TYPE C,

BEGIN OF GT_OUTPUT_LIST OCCURS 0,
   KOKRS       LIKE  COFIS-KOKRS,
   BUKRS       LIKE  COFIS-RBUKRS,
   RCL_DOCNR   LIKE  COFIS-DOCNR,
   RCL_HSL_SUM LIKE  COFIS-HSL,
   RCL_TSL_SUM LIKE  COFIS-TSL,
   RCL_KSL_SUM LIKE  COFIS-KSL,
   RCL_YEAR    LIKE  COFIS-RYEAR,
   RCL_PERIOD  LIKE  COFIS-POPER,
   FI_DOCNR    LIKE  BKPF-BELNR,
   FI_SUM      LIKE  BSEG-DMBTR,
   FI_YEAR     LIKE  BKPF-GJAHR,
   FI_PERIOD   LIKE  BKPF-MONAT,
   RCL_FI_DIFF LIKE  BSEG-DMBTR,
   ACCOUNT     LIKE  BSEG-HKONT,                              "02052000
   STFLG       LIKE  COFIS-STFLG,
   STOKZ       LIKE  COFIS-STOKZ,
   COMMENT(50),
   RCL_LOGSYS  LIKE  COFIS-LOGSYS,
   RCL_LOGSYSO LIKE  COFIS-LOGSYS,
   RCL_LOGSYSP LIKE  COFIS-LOGSYS,
   FI_LOGSYS   LIKE  BKPF-AWSYS,
END OF GT_OUTPUT_LIST,

BEGIN OF GS_SEL_BELNR,
   SIGN(1),
   OPTION(2),
   LOW    LIKE BKPF-BELNR,
   HIGH   LIKE BKPF-BELNR,
END OF GS_SEL_BELNR,

  GD_FIELDPOS_1  LIKE  SY-TABIX,
  GD_FIELDPOS_2  LIKE  SY-TABIX,
  GD_FIELDPOS_3  LIKE  SY-TABIX,
  GD_FIELDPOS_4  LIKE  SY-TABIX,
  GD_FIELDPOS_5  LIKE  SY-TABIX,
  GD_FIELDPOS_6  LIKE  SY-TABIX,
  GD_FIELDPOS_7  LIKE  SY-TABIX,
  GD_FIELDPOS_8  LIKE  SY-TABIX,
  GD_FIELDPOS_9  LIKE  SY-TABIX,
  GD_FIELDPOS_10 LIKE  SY-TABIX,
  GD_FIELDPOS_11 LIKE  SY-TABIX,
  GD_FIELDPOS_12 LIKE  SY-TABIX,
  GD_FIELDPOS_13 LIKE  SY-TABIX,
  GD_FIELDPOS_14 LIKE  SY-TABIX,
  GD_FIELDPOS_15 LIKE  SY-TABIX,
  GD_FIELDPOS_16 LIKE  SY-TABIX,
  GD_FIELDPOS_17 LIKE  SY-TABIX,
  GD_FIELDPOS_18 LIKE  SY-TABIX,
  GD_FIELDPOS_19 LIKE  SY-TABIX,
  GD_FIELDPOS_20 LIKE  SY-TABIX,
  GD_FIELDPOS_21 LIKE  SY-TABIX,
  GD_FIELDPOS_LAST LIKE  SY-TABIX.

*=======================================================================
* selection screen:
*=======================================================================

* FI document number:
SELECTION-SCREEN BEGIN OF BLOCK A WITH FRAME.
PARAMETERS:
  COAREA LIKE  COFIS-KOKRS MEMORY ID CAC   OBLIGATORY,
  PERIOD LIKE  COFIS-POPER MEMORY ID PER   OBLIGATORY,        "02052000
  YEAR   LIKE  COFIS-RYEAR MEMORY ID GJR   OBLIGATORY.
SELECTION-SCREEN END OF BLOCK A.

SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK B WITH FRAME.
SELECT-OPTIONS:
  REVERSAL FOR COFIS-STFLG,
  REVERSED FOR COFIS-STOKZ.
SELECTION-SCREEN END OF BLOCK B.

SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK C WITH FRAME.
PARAMETERS:
  DSP_LSYS   LIKE  BOOLE-BOOLE.
SELECTION-SCREEN END OF BLOCK C.

SELECTION-SCREEN SKIP 2.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (30) GD_ADDF.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP 2.
SELECTION-SCREEN PUSHBUTTON 2(30) GD_OKKP USER-COMMAND OKKP.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN PUSHBUTTON 2(30) GD_KALA USER-COMMAND KALA.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN PUSHBUTTON 2(30) GD_KAL1 USER-COMMAND KAL1.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN PUSHBUTTON 2(30) GD_OK17 USER-COMMAND OK17.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN PUSHBUTTON 2(30) GD_OBYB USER-COMMAND OBYB.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN PUSHBUTTON 2(30) GD_OBYA USER-COMMAND OBYA.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN PUSHBUTTON 2(30) GD_OBXM USER-COMMAND OBXM.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN PUSHBUTTON 2(30) GD_KAL7 USER-COMMAND KAL7.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN PUSHBUTTON 2(30) GD_GR55 USER-COMMAND GR55.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN PUSHBUTTON 2(30) GD_KALC USER-COMMAND KALC.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN PUSHBUTTON 2(30) GD_KALS USER-COMMAND KALS.

*=======================================================================
INITIALIZATION.
*=======================================================================
  GD_ADDF = 'Additional Functions'.

  GD_OKKP = 'Controlling Area Master Data'.
  GD_KALA = 'RCL Activation'.
  GD_KAL1 = 'RCL Follow Up Posting'.
  GD_OK17 = 'Enhanced Account Determination'.
  GD_OBYB = 'Standard Account Determination'.
  GD_OBYA = 'Company Code Clearing Account'.
  GD_OBXM = 'Business Area Clearing Account'.
  GD_KAL7 = 'Overview Cost Flow'.
  GD_GR55 = 'Start Report Writer'.
  GD_KALC = 'Execute Reconciliation Posting CO-FI'.
  GD_KALS = 'Reverse Reconciliation Posting CO-FI'.

*=======================================================================
AT SELECTION-SCREEN.
*=======================================================================
  PERFORM EXECUTE_USER_COMMAND
          USING  SY-UCOMM.

*=======================================================================
AT USER-COMMAND.
*=======================================================================

  PERFORM EXECUTE_USER_COMMAND
          USING  SY-UCOMM.

*=======================================================================
START-OF-SELECTION.
*=======================================================================

  CALL FUNCTION 'MESSAGES_INITIALIZE'.

  PERFORM INITIAL_CHECKS
          USING     COAREA
                    PERIOD
                    YEAR
          CHANGING  GS_TKA01.

  PERFORM FIND_RCL_DOCUMENTS
          TABLES  GT_COFIS
                  GT_OUTPUT_LIST
          USING   COAREA
                  PERIOD
                  YEAR.

  IF ( SY-SAPRL(1) < '4' ).                                   "02052000
    PERFORM ACCOUNT_DETERMINATION                             "02052000
            TABLES  GT_COFIS                                  "02052000
                    GT_OUTPUT_LIST                            "02052000
            USING   GS_TKA01.                                 "02052000
  ENDIF.                                                      "02052000

  PERFORM FIND_FI_DOCUMENTS
          TABLES  GT_OUTPUT_LIST.

  PERFORM CHECK_OUTPUT_LIST
          TABLES  GT_OUTPUT_LIST.

  PERFORM LIST_OUTPUT
          TABLES  GT_OUTPUT_LIST.

*=======================================================================
AT LINE-SELECTION.
*=======================================================================

* User interaction:
  GET CURSOR FIELD  GD_HIDDEN_FIELD.
  CASE GD_HIDDEN_FIELD.
    WHEN 'GD_STRING'.
      CALL FUNCTION 'MESSAGES_SHOW'
           EXPORTING
                SHOW_LINNO  = ' '
           EXCEPTIONS
                NO_MESSAGES = 00.
  ENDCASE.

*=======================================================================
TOP-OF-PAGE.
*=======================================================================

  CALL FUNCTION 'MESSAGES_COUNT'
       IMPORTING
            COUNT = GD_MESS_COUNT.

  IF ( GD_MESS_COUNT <> 0 ).
*   messages appeared: number of messages:
    FORMAT: HOTSPOT ON.
    WRITE  'Messages' TO GD_STRING LEFT-JUSTIFIED.
    WRITE: /1(40) GD_STRING    COLOR COL_NORMAL.
    FORMAT: HOTSPOT OFF.
    WRITE AT 52 GD_MESS_COUNT.
  ENDIF.

  WRITE: /, / 'Reconciliation Ledger:'.
  IF ( GS_TKA01-RCLAC = ' ' ).
    WRITE: 'Not Active' COLOR COL_NEGATIVE.
  ELSE.
    WRITE: 'Active' COLOR COL_POSITIVE.
  ENDIF.

  IF ( GT_COFIS[] IS INITIAL ).
    WRITE: /, / 'No RCL Documents found (COFIS)'.
  ENDIF.

  WRITE: /, /.
  WRITE / SY-ULINE.

  PERFORM FILL_FIELDPOSNS
          CHANGING GD_FIELDPOS_1.      "and other fields

  PERFORM WRITE_HEADER_LINE
          USING GD_FIELDPOS_1.         "and other fields

*=======================================================================
* Forms:
*=======================================================================

*&---------------------------------------------------------------------*
*&      Form  find_rcl_documents
*&---------------------------------------------------------------------*
FORM FIND_RCL_DOCUMENTS
     TABLES  ET_COFIS         STRUCTURE  GT_COFIS
             CT_OUTPUT_LIST   STRUCTURE  GT_OUTPUT_LIST
     USING   ID_COAREA        LIKE       COFIS-KOKRS
             ID_PERIOD        LIKE       COFIS-POPER
             ID_YEAR          LIKE       COFIS-RYEAR.

  RANGES:
  LT_STFLG  FOR COFIS-STFLG,
  LT_STOKZ  FOR COFIS-STOKZ.

  REFRESH ET_COFIS.
  CLEAR   ET_COFIS.

  LOOP AT REVERSAL.
    MOVE-CORRESPONDING REVERSAL TO LT_STFLG.
    APPEND LT_STFLG.
  ENDLOOP.

  LOOP AT REVERSED.
    MOVE-CORRESPONDING REVERSED TO LT_STOKZ.
    APPEND LT_STOKZ.
  ENDLOOP.

  SELECT * FROM COFIS
           INTO TABLE ET_COFIS
             WHERE KOKRS  = ID_COAREA
             AND   RYEAR  = ID_YEAR
             AND   POPER  = ID_PERIOD
             AND   STFLG IN LT_STFLG
             AND   STOKZ IN LT_STOKZ.

  IF ( SY-SUBRC <> 0 ).
    CALL FUNCTION 'MESSAGE_STORE'
         EXPORTING
              ARBGB                   = 'K5'
              EXCEPTION_IF_NOT_ACTIVE = ' '
              MSGTY                   = 'I'
              MSGV1                   = 'No RCL documents found'
              MSGV2                   = 'in Period'
              MSGV3                   = ID_PERIOD
              MSGV4                   = ID_YEAR
              TXTNR                   = '001'.
  ENDIF.

  SORT ET_COFIS BY KOKRS RYEAR POPER DOCNR DOCLN.

  LOOP AT ET_COFIS.
    MOVE-CORRESPONDING ET_COFIS TO CT_OUTPUT_LIST.
    CT_OUTPUT_LIST-BUKRS       = ET_COFIS-RBUKRS.
    CT_OUTPUT_LIST-RCL_DOCNR   = ET_COFIS-DOCNR.
    CT_OUTPUT_LIST-RCL_HSL_SUM = ET_COFIS-HSL.
    CT_OUTPUT_LIST-RCL_TSL_SUM = ET_COFIS-TSL.
    CT_OUTPUT_LIST-RCL_KSL_SUM = ET_COFIS-KSL.
    CT_OUTPUT_LIST-RCL_YEAR    = ET_COFIS-RYEAR.
    CT_OUTPUT_LIST-RCL_PERIOD  = ET_COFIS-POPER.
    CT_OUTPUT_LIST-RCL_LOGSYS  = ET_COFIS-LOGSYS.
    CT_OUTPUT_LIST-RCL_LOGSYSO = ET_COFIS-RLOGSYS.
    CT_OUTPUT_LIST-RCL_LOGSYSP = ET_COFIS-SLOGSYS.
    COLLECT CT_OUTPUT_LIST.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  find_fi_documents
*&---------------------------------------------------------------------*
FORM FIND_FI_DOCUMENTS
     TABLES   CT_OUTPUT_LIST   STRUCTURE  GT_OUTPUT_LIST.

  DATA:
  LD_TABIX   LIKE  SY-TABIX,
  BEGIN OF LS_AWKEY,
    AWREF LIKE ACCHD-AWREF,
    AWORG LIKE ACCHD-AWORG,
  END OF LS_AWKEY.

  LOOP AT CT_OUTPUT_LIST.
    LD_TABIX = SY-TABIX.

    CLEAR CT_OUTPUT_LIST-FI_SUM.

    LS_AWKEY-AWORG = CT_OUTPUT_LIST-KOKRS.
    LS_AWKEY-AWREF = CT_OUTPUT_LIST-RCL_DOCNR.

    SELECT * FROM   BKPF
             WHERE  AWTYP = 'COFIS'
             AND    AWKEY = LS_AWKEY.
      CT_OUTPUT_LIST-FI_YEAR   = BKPF-GJAHR.
      CT_OUTPUT_LIST-FI_DOCNR  = BKPF-BELNR.
      CT_OUTPUT_LIST-FI_PERIOD = BKPF-MONAT.
      CT_OUTPUT_LIST-FI_LOGSYS = BKPF-AWSYS.
      SELECT * FROM  BSEG
               WHERE  BUKRS       = BKPF-BUKRS
               AND    BELNR       = BKPF-BELNR
               AND    GJAHR       = BKPF-GJAHR
               AND    HKONT       = CT_OUTPUT_LIST-ACCOUNT.
        IF BSEG-SHKZG = 'H'.
          CT_OUTPUT_LIST-FI_SUM = CT_OUTPUT_LIST-FI_SUM - BSEG-DMBTR.
        ELSE.
          CT_OUTPUT_LIST-FI_SUM = CT_OUTPUT_LIST-FI_SUM + BSEG-DMBTR.
        ENDIF.
      ENDSELECT.
    ENDSELECT.

    IF ( SY-SUBRC <> 0 ).
      CALL FUNCTION 'MESSAGE_STORE'
           EXPORTING
                ARBGB                   = 'K5'
                EXCEPTION_IF_NOT_ACTIVE = ' '
                MSGTY                   = 'I'
                MSGV1                   = 'No FI document found'
                MSGV2                   = 'For RCL document'
                MSGV3                   = CT_OUTPUT_LIST-RCL_DOCNR
                MSGV4                   = CT_OUTPUT_LIST-KOKRS
                TXTNR                   = '001'.
      CT_OUTPUT_LIST-COMMENT = 'FI doc not found'.
    ELSEIF ( SYST-DBCNT > 1 ).
      CALL FUNCTION 'MESSAGE_STORE'
           EXPORTING
                ARBGB                   = 'K5'
                EXCEPTION_IF_NOT_ACTIVE = ' '
                MSGTY                   = 'I'
                MSGV1                   = 'More than 1 FI doc found'
                MSGV2                   = 'For RCL document'
                MSGV3                   = CT_OUTPUT_LIST-RCL_DOCNR
                MSGV4                   = CT_OUTPUT_LIST-KOKRS
                TXTNR                   = '001'.
      CT_OUTPUT_LIST-COMMENT = 'Multiple FI docs found'.
    ENDIF.
    CT_OUTPUT_LIST-RCL_FI_DIFF = CT_OUTPUT_LIST-RCL_HSL_SUM +
                                 CT_OUTPUT_LIST-FI_SUM.
    MODIFY CT_OUTPUT_LIST INDEX LD_TABIX.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  execute_user_command
*&---------------------------------------------------------------------*
FORM EXECUTE_USER_COMMAND
     USING    ID_UCOMM  LIKE  SY-UCOMM.

  IF ( ID_UCOMM = 'OKKP' ).
    CALL TRANSACTION 'OKKP'.
  ELSEIF ( ID_UCOMM = 'KALA' ).
    CALL TRANSACTION 'KALA'.
  ELSEIF ( ID_UCOMM = 'KAL1' ).
    CALL TRANSACTION 'KAL1'.
  ELSEIF ( ID_UCOMM = 'OK17' ).
    CALL TRANSACTION 'OK17'.
  ELSEIF ( ID_UCOMM = 'OBYB' ).
    CALL TRANSACTION 'OBYB'.
  ELSEIF ( ID_UCOMM = 'OBYA' ).
    CALL TRANSACTION 'OBYA'.
  ELSEIF ( ID_UCOMM = 'OBXN' ).
    CALL TRANSACTION 'OBXN'.
  ELSEIF ( ID_UCOMM = 'OBXM' ).
    CALL TRANSACTION 'OBXM'.
  ELSEIF ( ID_UCOMM = 'KAL7' ).
    CALL TRANSACTION 'KAL7'.
  ELSEIF ( ID_UCOMM = 'GR55' ).
    CALL TRANSACTION 'GR55'.
  ELSEIF ( ID_UCOMM = 'KALC' ).
    CALL TRANSACTION 'KALC'.
  ELSEIF ( ID_UCOMM = 'KALS' ).
    CALL TRANSACTION 'KALS'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  initial_checks
*&---------------------------------------------------------------------*
FORM INITIAL_CHECKS
     USING    ID_COAREA    LIKE  COFIS-KOKRS
              ID_PERIOD    LIKE  COFIS-POPER
              ID_YEAR      LIKE  COFIS-RYEAR
     CHANGING CS_TKA01     LIKE  TKA01.

  CALL FUNCTION 'K_KOKRS_READ'
       EXPORTING
            KOKRS   = ID_COAREA
       IMPORTING
            E_TKA01 = CS_TKA01.

  IF ( CS_TKA01-RCLAC = ' ' ).
    CALL FUNCTION 'MESSAGE_STORE'
         EXPORTING
              ARBGB                   = 'K5'
              EXCEPTION_IF_NOT_ACTIVE = ' '
              MSGTY                   = 'W'
              MSGV1                   = 'Reconciliation ledger'
              MSGV2                   = 'is not active in '
              MSGV3                   = 'controlling area'
              MSGV4                   = ID_COAREA
              TXTNR                   = '001'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  check_output_list
*&---------------------------------------------------------------------*
FORM CHECK_OUTPUT_LIST
     TABLES   IT_OUTPUT_LIST  STRUCTURE  GT_OUTPUT_LIST.

  DATA:
  LD_OWN_LOGSYS  LIKE  T000-LOGSYS.
  RANGES:
  LT_RANGES_LOGSYS FOR T000-LOGSYS.
  CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
       IMPORTING
            OWN_LOGICAL_SYSTEM             = LD_OWN_LOGSYS
       EXCEPTIONS
            OWN_LOGICAL_SYSTEM_NOT_DEFINED = 0.

  LT_RANGES_LOGSYS-SIGN   = 'I'.
  LT_RANGES_LOGSYS-OPTION = 'EQ'.
  LT_RANGES_LOGSYS-LOW    = LD_OWN_LOGSYS.
  APPEND LT_RANGES_LOGSYS.
  CLEAR LT_RANGES_LOGSYS-LOW.
  APPEND LT_RANGES_LOGSYS.

  LOOP AT IT_OUTPUT_LIST.

    IF ( NOT IT_OUTPUT_LIST-RCL_LOGSYSO IN LT_RANGES_LOGSYS ) OR
       ( NOT IT_OUTPUT_LIST-RCL_LOGSYSP IN LT_RANGES_LOGSYS ) OR
       ( NOT IT_OUTPUT_LIST-FI_LOGSYS   IN LT_RANGES_LOGSYS ).
      CALL FUNCTION 'MESSAGE_STORE'
           EXPORTING
                ARBGB                   = 'K5'
                EXCEPTION_IF_NOT_ACTIVE = ' '
                MSGTY                   = 'W'
                MSGV1                   = 'Different logical systems'
                MSGV2                   = 'in RCL document'
                MSGV3                   = IT_OUTPUT_LIST-RCL_DOCNR
                TXTNR                   = '001'.
      IF ( IT_OUTPUT_LIST-FI_LOGSYS <> LD_OWN_LOGSYS ) AND
         ( IT_OUTPUT_LIST-FI_LOGSYS <> SPACE ).
        CALL FUNCTION 'MESSAGE_STORE'
             EXPORTING
                  ARBGB                   = 'K5'
                  EXCEPTION_IF_NOT_ACTIVE = ' '
                  MSGTY                   = 'W'
                  MSGV1                   = 'T000-LOGSYS:'
                  MSGV2                   = LD_OWN_LOGSYS
                  MSGV3                   = 'FI_LOGSYS'
                  MSGV4                   = IT_OUTPUT_LIST-FI_LOGSYS
                  TXTNR                   = '001'.
      ENDIF.
      IF ( IT_OUTPUT_LIST-RCL_LOGSYS <> LD_OWN_LOGSYS ) AND
         ( IT_OUTPUT_LIST-RCL_LOGSYS <> SPACE ).
        CALL FUNCTION 'MESSAGE_STORE'
             EXPORTING
                  ARBGB                   = 'K5'
                  EXCEPTION_IF_NOT_ACTIVE = ' '
                  MSGTY                   = 'W'
                  MSGV1                   = 'T000-LOGSYS:'
                  MSGV2                   = LD_OWN_LOGSYS
                  MSGV3                   = 'RCL_LOGSYS'
                  MSGV4                   = IT_OUTPUT_LIST-RCL_LOGSYS
                  TXTNR                   = '001'.
      ENDIF.
      IF ( IT_OUTPUT_LIST-RCL_LOGSYSO <> LD_OWN_LOGSYS ) AND
         ( IT_OUTPUT_LIST-RCL_LOGSYSO <> SPACE ).
        CALL FUNCTION 'MESSAGE_STORE'
             EXPORTING
                  ARBGB                   = 'K5'
                  EXCEPTION_IF_NOT_ACTIVE = ' '
                  MSGTY                   = 'W'
                  MSGV1                   = 'T000-LOGSYS:'
                  MSGV2                   = LD_OWN_LOGSYS
                  MSGV3                   = 'RCL_LOGSYS'
                  MSGV4                   = IT_OUTPUT_LIST-RCL_LOGSYSO
                  TXTNR                   = '001'.
      ENDIF.
      IF ( IT_OUTPUT_LIST-RCL_LOGSYSP <> LD_OWN_LOGSYS ) AND
         ( IT_OUTPUT_LIST-RCL_LOGSYSP <> SPACE ).
        CALL FUNCTION 'MESSAGE_STORE'
             EXPORTING
                  ARBGB                   = 'K5'
                  EXCEPTION_IF_NOT_ACTIVE = ' '
                  MSGTY                   = 'W'
                  MSGV1                   = 'T000-LOGSYS:'
                  MSGV2                   = LD_OWN_LOGSYS
                  MSGV3                   = 'RCL_LOGSYS'
                  MSGV4                   = IT_OUTPUT_LIST-RCL_LOGSYSP
                  TXTNR                   = '001'.
      ENDIF.
      IT_OUTPUT_LIST-COMMENT = 'Different logical systems'.
      MODIFY IT_OUTPUT_LIST.
    ENDIF.

    IF ( IT_OUTPUT_LIST-RCL_FI_DIFF <> SPACE ).
      CALL FUNCTION 'MESSAGE_STORE'
           EXPORTING
                ARBGB                   = 'K5'
                EXCEPTION_IF_NOT_ACTIVE = ' '
                MSGTY                   = 'W'
                MSGV1                   = 'Difference between RCL/FI'
                MSGV2                   = 'in RCL document'
                MSGV3                   = IT_OUTPUT_LIST-RCL_DOCNR
                TXTNR                   = '001'.
      IT_OUTPUT_LIST-COMMENT = 'Difference between FI and RCL'.
      MODIFY IT_OUTPUT_LIST.
    ENDIF.

    IF ( IT_OUTPUT_LIST-RCL_HSL_SUM = 0  ) AND
       ( IT_OUTPUT_LIST-RCL_TSL_SUM = 0  ) AND
       ( IT_OUTPUT_LIST-RCL_KSL_SUM <> 0 ).
      CALL FUNCTION 'MESSAGE_STORE'
           EXPORTING
                ARBGB                   = 'K5'
                EXCEPTION_IF_NOT_ACTIVE = ' '
                MSGTY                   = 'W'
                MSGV1                   = 'Only group curr. <> 0'
                MSGV2                   = 'in RCL document'
                MSGV3                   = IT_OUTPUT_LIST-RCL_DOCNR
                TXTNR                   = '001'.
      IT_OUTPUT_LIST-COMMENT = 'Only group curr. <> 0 in RCL'.
      MODIFY IT_OUTPUT_LIST.
    ENDIF.

    IF ( IT_OUTPUT_LIST-RCL_PERIOD <> IT_OUTPUT_LIST-FI_PERIOD ) OR
       ( IT_OUTPUT_LIST-RCL_YEAR   <> IT_OUTPUT_LIST-FI_YEAR   ).
*     1. check different periods RCL / FI:
      CALL FUNCTION 'MESSAGE_STORE'
           EXPORTING
                ARBGB                 = 'K5'
                EXCEPTION_IF_NOT_ACTIVE = ' '
                MSGTY                 = 'W'
            MSGV1                   = 'Different Periods in RCL and FI'
                MSGV2                 = 'for FI document '
                MSGV3                 = IT_OUTPUT_LIST-BUKRS
                MSGV4                 = IT_OUTPUT_LIST-FI_DOCNR
                TXTNR                 = '001'.
      CALL FUNCTION 'MESSAGE_STORE'
           EXPORTING
               ARBGB                = 'K5'
               EXCEPTION_IF_NOT_ACTIVE = ' '
               MSGTY                = 'W'
              MSGV1                = 'This FI document has period/year'
               MSGV2                = IT_OUTPUT_LIST-FI_PERIOD
               MSGV3                = IT_OUTPUT_LIST-FI_YEAR
               TXTNR                = '001'.
      IT_OUTPUT_LIST-COMMENT = 'Different periods'.
      MODIFY IT_OUTPUT_LIST.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  LIST_OUTPUT
*&---------------------------------------------------------------------*
FORM LIST_OUTPUT
     TABLES   IT_OUTPUT_LIST  STRUCTURE  GT_OUTPUT_LIST.

  PERFORM FILL_FIELDPOSNS
          CHANGING GD_FIELDPOS_1.

  LOOP AT IT_OUTPUT_LIST.
    WRITE: /                                     SY-VLINE,
           AT GD_FIELDPOS_1(4)   IT_OUTPUT_LIST-KOKRS,       SY-VLINE,
           AT GD_FIELDPOS_2(5)   IT_OUTPUT_LIST-BUKRS,       SY-VLINE,
                                                              "02052000
           AT GD_FIELDPOS_3(10)  IT_OUTPUT_LIST-RCL_DOCNR,   SY-VLINE.
    IF ( DSP_LSYS = ' ' ).
      WRITE:
            AT GD_FIELDPOS_4(17)  IT_OUTPUT_LIST-RCL_HSL_SUM, SY-VLINE,
            AT GD_FIELDPOS_5(17)  IT_OUTPUT_LIST-RCL_TSL_SUM, SY-VLINE,
            AT GD_FIELDPOS_6(17)  IT_OUTPUT_LIST-RCL_KSL_SUM, SY-VLINE,
            AT GD_FIELDPOS_7(4)   IT_OUTPUT_LIST-RCL_YEAR,    SY-VLINE,
            AT GD_FIELDPOS_8(4)   IT_OUTPUT_LIST-RCL_PERIOD,  SY-VLINE,
                                                              "02052000
            AT GD_FIELDPOS_9(10)  IT_OUTPUT_LIST-FI_DOCNR,    SY-VLINE,
            AT GD_FIELDPOS_10(17) IT_OUTPUT_LIST-FI_SUM,      SY-VLINE,
            AT GD_FIELDPOS_11(4)  IT_OUTPUT_LIST-FI_YEAR,     SY-VLINE,
            AT GD_FIELDPOS_12(3)  IT_OUTPUT_LIST-FI_PERIOD,   SY-VLINE,
            AT GD_FIELDPOS_13(17) IT_OUTPUT_LIST-RCL_FI_DIFF, SY-VLINE,
            AT GD_FIELDPOS_14(12) IT_OUTPUT_LIST-ACCOUNT,     SY-VLINE,
            AT GD_FIELDPOS_15(5)  IT_OUTPUT_LIST-STFLG,       SY-VLINE,
            AT GD_FIELDPOS_16(5)  IT_OUTPUT_LIST-STOKZ,       SY-VLINE,
            AT GD_FIELDPOS_17(30) IT_OUTPUT_LIST-COMMENT,     SY-VLINE.
    ELSE.
      WRITE:
      AT GD_FIELDPOS_18(10) IT_OUTPUT_LIST-RCL_LOGSYS,  SY-VLINE,
      AT GD_FIELDPOS_19(10) IT_OUTPUT_LIST-RCL_LOGSYSO, SY-VLINE,
      AT GD_FIELDPOS_20(10) IT_OUTPUT_LIST-RCL_LOGSYSP, SY-VLINE,
      AT GD_FIELDPOS_21(10) IT_OUTPUT_LIST-FI_LOGSYS,   SY-VLINE.
    ENDIF.
  ENDLOOP.

  WRITE / SY-ULINE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  fill_fieldposns
*&---------------------------------------------------------------------*
FORM FILL_FIELDPOSNS
     CHANGING GD_FIELDPOS1  LIKE  GD_FIELDPOS_1.            "to 20

  GD_FIELDPOS_1 = 2.                   "kokrs
  GD_FIELDPOS_2 = GD_FIELDPOS_1 + 6.   "bukrs
  GD_FIELDPOS_3 = GD_FIELDPOS_2 + 7.   "rcl_docnr

  IF ( DSP_LSYS = ' ' ).
    GD_FIELDPOS_4 = GD_FIELDPOS_3 + 12."RCL_HSL_SUM
    GD_FIELDPOS_5 = GD_FIELDPOS_4 + 19."RCL_HSL_SUM
    GD_FIELDPOS_6 = GD_FIELDPOS_5 + 19."RCL_HSL_SUM
    GD_FIELDPOS_7 = GD_FIELDPOS_6 + 19."rcl_year
    GD_FIELDPOS_8 = GD_FIELDPOS_7 + 6. "rcl_period
    GD_FIELDPOS_9 = GD_FIELDPOS_8 + 7. "FI docnr
    GD_FIELDPOS_10 = GD_FIELDPOS_9 + 12. "FI HSL sum
    GD_FIELDPOS_11 = GD_FIELDPOS_10 + 19."FI year
    GD_FIELDPOS_12 = GD_FIELDPOS_11 + 7. "FI period
    GD_FIELDPOS_13 = GD_FIELDPOS_12 + 5. "RCL_FI_DIFF
    GD_FIELDPOS_14 = GD_FIELDPOS_13 + 19."Account
    GD_FIELDPOS_15 = GD_FIELDPOS_14 + 15."STFLG
    GD_FIELDPOS_16 = GD_FIELDPOS_15 + 7. "STOKZ
    GD_FIELDPOS_17 = GD_FIELDPOS_16 + 7. "Comment
    GD_FIELDPOS_LAST = GD_FIELDPOS_17 + 31.
  ELSE.
    GD_FIELDPOS_18 = GD_FIELDPOS_3  + 12."RCL_LOGSYS
    GD_FIELDPOS_19 = GD_FIELDPOS_18 + 12."RCL_LOGSYSO
    GD_FIELDPOS_20 = GD_FIELDPOS_19 + 12."RCL_LOGSYSP
    GD_FIELDPOS_21 = GD_FIELDPOS_20 + 12."FI_LOGSYSP
    GD_FIELDPOS_LAST = GD_FIELDPOS_21 + 11.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  write_header_line
*&---------------------------------------------------------------------*
FORM WRITE_HEADER_LINE
     USING    GD_FIELDPOS_1  LIKE  GD_FIELDPOS_1.

  WRITE: /                                     SY-VLINE,
         AT GD_FIELDPOS_1(4)   'KOKR',        SY-VLINE,
         AT GD_FIELDPOS_2(5)   'BUKRS',       SY-VLINE,
         AT GD_FIELDPOS_3(10)  'RCL_DOCNR',   SY-VLINE.
  IF ( DSP_LSYS = ' ' ).
    WRITE:
    AT GD_FIELDPOS_4(17)  'RCL_HSL_SUM', SY-VLINE,
    AT GD_FIELDPOS_5(17)  'RCL_TSL_SUM', SY-VLINE,
    AT GD_FIELDPOS_6(17)  'RCL_KSL_SUM', SY-VLINE,
    AT GD_FIELDPOS_7(4)   'RCLY',        SY-VLINE,
    AT GD_FIELDPOS_8(4)   'RCLP',        SY-VLINE,
    AT GD_FIELDPOS_9(10)  'FI DOCNR',    SY-VLINE,
    AT GD_FIELDPOS_10(17) 'FI_HSL_SUM',  SY-VLINE,
    AT GD_FIELDPOS_11(4)  'FIY',         SY-VLINE,
    AT GD_FIELDPOS_12(3)  'FIP',         SY-VLINE,
    AT GD_FIELDPOS_13(17) 'RCL_FI_DIFF', SY-VLINE,
    AT GD_FIELDPOS_14(12) 'ACCOUNT',     SY-VLINE,
    AT GD_FIELDPOS_15(5)  'RVSAL',       SY-VLINE,
    AT GD_FIELDPOS_16(5)  'RVSED',       SY-VLINE,
    AT GD_FIELDPOS_17(30) 'COMMENT',     SY-VLINE.
  ELSE.
    WRITE:
    AT GD_FIELDPOS_18(10) 'RCL_LOGSYS',  SY-VLINE,
    AT GD_FIELDPOS_19(10) 'RCL_LOGSYSO', SY-VLINE,
    AT GD_FIELDPOS_20(10) 'RCL_LOGSYSP', SY-VLINE,
    AT GD_FIELDPOS_21(10) 'FI LOGSYS',   SY-VLINE.
  ENDIF.

  WRITE: / SY-ULINE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ACCOUNT_DETERMINATION
*&---------------------------------------------------------------------*
FORM ACCOUNT_DETERMINATION                                    "02052000
     TABLES   IT_COFIS        STRUCTURE  COFIS                "02052000
              CT_OUTPUT_LIST  STRUCTURE  GT_OUTPUT_LIST       "02052000
     USING    IS_TKA01        LIKE       TKA01.               "02052000
                                                              "02052000
  DATA:                                                       "02052000
  LD_IORG_ACCT  LIKE  BOOLE-BOOLE,                            "02052000
  LT_COFI_ICCF  LIKE  COFI_ICCF  OCCURS 0 WITH HEADER LINE.   "02052000
                                                              "02052000
  IF ( IS_TKA01-RCL_PRIMAC = 'X' ).                           "02052000
    LD_IORG_ACCT = ' '.                                       "02052000
  ELSE.                                                       "02052000
    LD_IORG_ACCT = 'X'.                                       "02052000
  ENDIF.                                                      "02052000
                                                              "02052000
  LOOP AT IT_COFIS.                                           "02052000
    REFRESH LT_COFI_ICCF.                                     "02052000
    CLEAR   LT_COFI_ICCF.                                     "02052000
                                                              "02052000
    MOVE-CORRESPONDING IT_COFIS TO LT_COFI_ICCF.              "02052000
    APPEND  LT_COFI_ICCF.                                     "02052000
                                                              "02052000
    CALL FUNCTION 'K_ICCF_ACCOUNT_DETERMINE'                  "02052000
         EXPORTING                                            "02052000
              I_BUDAT     = IT_COFIS-BUDAT                    "02052000
              I_KOKRS     = IS_TKA01-KOKRS                    "02052000
              I_ORG_ACCT  = LD_IORG_ACCT                      "02052000
         TABLES                                               "02052000
              T_COFI_ICCF = LT_COFI_ICCF.                     "02052000
                                                              "02052000
    READ TABLE LT_COFI_ICCF INDEX 1.                          "02052000
    IF ( SY-SUBRC <> 0 ).                                     "02052000
      CALL FUNCTION 'MESSAGE_STORE'                           "02052000
           EXPORTING                                          "02052000
                ARBGB                   = 'K5'                "02052000
                EXCEPTION_IF_NOT_ACTIVE = ' '                 "02052000
                MSGTY                   = 'E'                 "02052000
                MSGV1                   = 'Internal error!'   "02052000
                MSGV2                   = 'Please call SAP'   "02052000
                TXTNR                   = '001'.              "02052000
    ENDIF.                                                    "02052000
    READ TABLE CT_OUTPUT_LIST WITH                            "02052000
                          KEY KOKRS     = IT_COFIS-KOKRS      "02052000
                          RCL_DOCNR = IT_COFIS-DOCNR.         "02052000
    CT_OUTPUT_LIST-ACCOUNT = LT_COFI_ICCF-ACCOUNT.            "02052000
    MODIFY CT_OUTPUT_LIST INDEX SY-TABIX.                     "02052000
                                                              "02052000
  ENDLOOP.                                                    "02052000
                                                              "02052000
ENDFORM.                                                      "02052000
