*&---------------------------------------------------------------------*
*& Report  RKAKALX3                                                    *
*&                                                                     *
*&---------------------------------------------------------------------*
* changing history:
* -----------------
* Mercury
* SCT P9CK103047 121100: note 0365936: tto much errornous records
* 4.6C
* SCT P00K016482 260100: new FM K_COEP_FAREA_SET_SINGLE
* 4.6B
* SCT P99K052200 230899 note 168766: revenues on cost centers
* 4.5B
* SCT ALRK136242 211298 needed memory reduced: list output optional
* SCT ALRK136242 170998 note 116590: farea for plan data
* 4.5A
* SCT ALRK100431 010498 New logic for fkber for business objects
* SCT ALRK087269 030398 form farea_for_pa_object moved.
*-----------------------------------------------------------------------

* A SHORT DOCUMENTATION FOR THIS REPORT:
*
* THIS REPORT READS ALL CO LINE ITEMS
* FROM THE DATABASE TABLE COEP THAT MEET THE SELECTION
* CRITERIA YOU SPECIFIED ON THE SELECTION SCREEN.
* THE NAME OF THESE PARAMETERS CORRESPONDS TO THE FIELD NAMES OF
* TABLE COEP (WITH 'p_' AS PREFIX).
*  -> P_KOKRS  =  CONTROLLING AREA
*  -> P_PERIO  =  PERIOD
*  -> P_BELNR  =  CO DOCUMENT NUMBER
*  -> P_BUZEI  =  CO LINE ITEM
*  -> ...
* THEN IT CALLS THE SUBSTITUTION ACTIVATED FOR COMPONENT
* "FI", CALL UP POINT "5".
* FUNCTION AREA (FKBER) UND PARTNER FUNCTION AREA (PFKBER)
* ARE NEWLY DERIVED FOR ALL THESE CO LINE ITEMS.
* IN THE DISPLAYED LIST YOU CAN COMPARE
* THE OLD FUNCTION AREAS (FKBER_OLD AND PFKBER_OLD)
* WITH THE NEW ONES.
*
*- IF YOU START THE REPORT IN TEST RUN (TEST_RUN = "X"),
*  THAN NO CHANGES ARE MADE ON THE DATABASE.
*
*- IF YOU DO NOT START THE REPORT IN TEST RUN'.
*  (TEST_RUN = " "), than the NEW derived function areas
*  ARE WRITTEN TO THE CO LINE ITEMS ON THE DATABASE.
*  THAT MEANS, THE OLD FUNCTION AREA AND PARTNER FUNCTION
*  AREA ARE EXCHANGED IN THESE LINE ITEMS AGAINST THE
*  NEW ONES.
*  BUT: LINE ITEMS, WHERE ERRORS OCCURRED, ARE NOT
*       CHANGED ON THE DATABASE.
*       ALSO CO LINE ITEMS WHERE COEP-VRGNG = 'COIN' OR 'COIE',
*       THAT MEANS CO LINE ITEMS WHICH WERE CAUSED BY POSTINGS IN OTHER
*       COMPONENTS THAN CO ARE NOT CHANGED, BECAUSE THIS WOULD BE NOT
*       REASONABLE SINCE THE ORIGINAL DOCUMENT IS NOT LOCATED
*       IN CO.
*  REMARK: CO LINE ITEMS WITH COEP-VRGNG = 'COIN' ARE NOT
*          RELEVANT FOR RECONCILATION POSTINGS.
*  ERRORS ARE INDICATED WITH AN "X" IN THE FIRST
*  COLUMN OF THE DISPLAYED LIST.

REPORT  RKAKALX3 MESSAGE-ID K5 LINE-SIZE 97.

TABLES:
        COBK,
        COEP,
        COIOB,
        TKA01.

*=======================================================================
* Selection screen

SELECTION-SCREEN BEGIN OF BLOCK C WITH FRAME.

PARAMETERS:
  P_KOKRS LIKE COFIS-KOKRS.

SELECTION-SCREEN END OF BLOCK C.
SELECTION-SCREEN BEGIN OF BLOCK D WITH FRAME.
SELECT-OPTIONS:
  P_GJAHR    FOR COEP-GJAHR    NO INTERVALS NO-EXTENSION,
  P_PERIO    FOR COEP-PERIO    NO-EXTENSION,
  P_BELNR    FOR COEP-BELNR,
  P_BUZEI    FOR COEP-BUZEI.
SELECTION-SCREEN END OF BLOCK D.
SELECTION-SCREEN BEGIN OF BLOCK E WITH FRAME.
SELECT-OPTIONS:
P_VRGNG    FOR COEP-VRGNG,
P_WRTTP    FOR COEP-WRTTP,
P_VERSN    FOR COEP-VERSN,
P_OBJNR    FOR COEP-OBJNR,
P_PAROB1   FOR COEP-PAROB1.
SELECTION-SCREEN END OF BLOCK E.
SELECTION-SCREEN BEGIN OF BLOCK F WITH FRAME.
SELECT-OPTIONS:
P_BUKRS    FOR COEP-BUKRS,
P_GSBER    FOR COEP-GSBER,
P_FKBER    FOR COEP-FKBER,
P_SCOPE    FOR COEP-SCOPE,
P_LOGSYO   FOR COEP-LOGSYSO,
P_KSTAR    FOR COEP-KSTAR.
SELECTION-SCREEN END OF BLOCK F.
SELECTION-SCREEN BEGIN OF BLOCK G WITH FRAME.
SELECT-OPTIONS:
P_PBUKRS    FOR COEP-PBUKRS,
P_PARGB     FOR COEP-PARGB,
P_PFKBER    FOR COEP-PFKBER,
P_PSCOPE    FOR COEP-PSCOPE,
P_LOGSYP    FOR COEP-LOGSYSP,
P_PKSTAR    FOR COEP-PKSTAR.
SELECTION-SCREEN END OF BLOCK G.

SELECTION-SCREEN BEGIN OF BLOCK B WITH FRAME.

PARAMETERS:
  TEST_RUN    LIKE  COFI_SCR-FLG_TEST  DEFAULT 'X',
  SHOW_LST    LIKE  COFI_SCR-FLG_LIST  DEFAULT 'X'.

SELECTION-SCREEN END OF BLOCK B.
*=======================================================================
* global data

DATA:
    LT_COEP  LIKE COEP  OCCURS 0 WITH HEADER LINE,
    LT_COIOB LIKE COIOB OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF LT_COEP_LIST OCCURS 0,
                    KOKRS      LIKE COEP-KOKRS,
                    BELNR      LIKE COEP-BELNR,
                    BUZEI      LIKE COEP-BUZEI,
                    FKBER_OLD  LIKE COEP-FKBER,
                    PFKBER_OLD LIKE COEP-PFKBER,
                    FLG_ERROR  LIKE COFI_SCR-FLG_TEST,
                    FKBER      LIKE COEP-FKBER,
                    PFKBER     LIKE COEP-PFKBER,
                    WTGBTR     LIKE COEP-WTGBTR,
                    TWAER      LIKE COEP-TWAER,
      END OF LT_COEP_LIST.

CONSTANTS:
  BEGIN OF GC_OBJ,                     "Flag: Objekt/Partnerobjekt
    O(1) TYPE C  VALUE 'O',
    P(1) TYPE C  VALUE 'P',
  END OF GC_OBJ.

DATA:
    GD_MESS_COUNT   LIKE  SY-TFILL,
    GD_STRING       TYPE  CHAR40,
    GD_HIDDEN_FIELD(120)  TYPE C,
    WA_COEP         LIKE  COEP,
    GD_TFILL        LIKE  SY-TFILL,
    GD_CURSOR       TYPE  CURSOR,      "Cursor für SELECT COEP
    GD_SAVE_TABIX   LIKE  SY-TABIX,
    GD_COUNT        LIKE  SY-TABIX,
    GD_MAX_SEV      LIKE  SY-SUBRC,
    GD_REC_CORRECTED LIKE SY-TFILL,    "Anzahl gelesene Sätze
    GD_NUM_REC_TOT  LIKE SY-TFILL,     "Anzahl gelesene Sätze gesamt
    GD_SUBRC        LIKE SY-SUBRC,
    GD_FKBER_ACTIV  LIKE BOOLE-BOOLE,  "Flag: 'Funktionsbereich' aktiv
    LD_ZEILE        LIKE MESG-ZEILE,
    LD_BUDAT        LIKE COBK-BUDAT.   "lokaler Zwischenspeicher für

INCLUDE RKAKALCN.

*=======================================================================
INITIALIZATION.
*=======================================================================

  GET PARAMETER ID 'CAC' FIELD P_KOKRS.

*=======================================================================
AT SELECTION-SCREEN OUTPUT.
*=======================================================================

*=======================================================================
AT SELECTION-SCREEN.
*=======================================================================

  SET PARAMETER ID 'CAC'  FIELD P_KOKRS.
  SET PARAMETER ID 'BUK'  FIELD P_BUKRS.

*=======================================================================
START-OF-SELECTION.
*=======================================================================

  CALL FUNCTION 'MESSAGES_INITIALIZE'.

*  OPEN CURSOR WITH HOLD GD_CURSOR
*  FOR SELECT * FROM COEP
*         WHERE  KOKRS       =   P_KOKRS
*         AND    PERIO       IN  P_PERIO
*         AND    GJAHR       IN  P_GJAHR
*         AND    BELNR       IN  P_BELNR
*         AND    BUZEI       IN  P_BUZEI
*         AND    VRGNG       IN  P_VRGNG
*         AND    VRGNG       <>  'COIN'
*         AND    VRGNG       <>  'COIE'
*         AND    WRTTP       IN  P_WRTTP
*         AND    VERSN       IN  P_VERSN
*         AND    OBJNR       IN  P_OBJNR
*         AND    PAROB1      IN  P_PAROB1
*         AND    BUKRS       IN  P_BUKRS
*         AND    GSBER       IN  P_GSBER
*         AND    FKBER       IN  P_FKBER
*         AND    SCOPE       IN  P_SCOPE
*         AND    LOGSYSO     IN  P_LOGSYO
*         AND    KSTAR       IN  P_KSTAR
*         AND    PBUKRS      IN  P_PBUKRS
*         AND    PARGB       IN  P_PARGB
*         AND    PFKBER      IN  P_PFKBER
*         AND    PSCOPE      IN  P_PSCOPE
*         AND    LOGSYSP     IN  P_LOGSYP
*         AND    PKSTAR      IN  P_PKSTAR
*         ORDER  BY PRIMARY KEY.
  OPEN CURSOR WITH HOLD GD_CURSOR
  FOR SELECT * FROM COEP
         WHERE  KOKRS       =   P_KOKRS
         AND    PERIO       IN  P_PERIO
         AND    GJAHR       IN  P_GJAHR
         AND    BELNR       IN  P_BELNR
         AND    BUZEI       IN  P_BUZEI
         AND  ( VRGNG       =  'COIN' OR VRGNG = 'COIE' )
         AND    WRTTP       IN  P_WRTTP
         AND    VERSN       IN  P_VERSN
         AND    OBJNR       IN  P_OBJNR
         AND    PAROB1      IN  P_PAROB1
         AND    BUKRS       IN  P_BUKRS
         AND    GSBER       IN  P_GSBER
         AND    FKBER       IN  P_FKBER
         AND    SCOPE       IN  P_SCOPE
         AND    LOGSYSO     IN  P_LOGSYO
         AND    KSTAR       IN  P_KSTAR
         AND    PBUKRS      IN  P_PBUKRS
         AND    PARGB       IN  P_PARGB
         AND    PFKBER      IN  P_PFKBER
         AND    PSCOPE      IN  P_PSCOPE
         AND    LOGSYSP     IN  P_LOGSYP
         AND    PKSTAR      IN  P_PKSTAR
         ORDER  BY PRIMARY KEY.
  DO.
    REFRESH LT_COEP.
    REFRESH LT_COEP_LIST.

    FETCH NEXT CURSOR GD_CURSOR INTO TABLE LT_COEP
          PACKAGE SIZE '5000'.

    IF ( SY-SUBRC <> 0 ).
      CLOSE CURSOR GD_CURSOR.
      EXIT.
    ENDIF.

*   complete first CO document:
*   FM 'K_SUBSTITUTION_FKBER' needs complete CO documents
*   to determine wether revenues on cost centers are posted:
    READ TABLE LT_COEP INDEX 1 INTO WA_COEP.
    IF ( WA_COEP-BUZEI NE '001' ).
      SELECT * FROM COEP APPENDING TABLE LT_COEP
                         WHERE KOKRS = WA_COEP-KOKRS
                         AND   BELNR = WA_COEP-BELNR
                         AND   BUZEI < WA_COEP-BUZEI. "ordered!

    ENDIF.

*   to make sure that the above select does not place line items
*   of the first belnr at the end:
    SORT LT_COEP BY KOKRS BELNR BUZEI.

*   complete last CO document:
    DESCRIBE TABLE LT_COEP LINES GD_TFILL.
    READ TABLE LT_COEP INDEX GD_TFILL INTO WA_COEP.

    SELECT * FROM COEP APPENDING TABLE LT_COEP
                       WHERE KOKRS = WA_COEP-KOKRS
                       AND   BELNR = WA_COEP-BELNR
                       AND   BUZEI > WA_COEP-BUZEI  "ordered!
                       AND   PERIO     IN  P_PERIO
                       AND   GJAHR     IN  P_GJAHR
                       AND   BELNR     IN  P_BELNR
                       AND   BUZEI     IN  P_BUZEI
*                       AND   VRGNG     IN  P_VRGNG
*                       AND   VRGNG     <>  'COIN'
*                       AND   VRGNG     <>  'COIE'
                       AND  ( VRGNG    =  'COIN' OR VRGNG = 'COIE' )
                       AND   WRTTP     IN  P_WRTTP
                       AND   VERSN     IN  P_VERSN
                       AND   OBJNR     IN  P_OBJNR
                       AND   PAROB1    IN  P_PAROB1
                       AND   BUKRS     IN  P_BUKRS
                       AND   GSBER     IN  P_GSBER
                       AND   FKBER     IN  P_FKBER
                       AND   SCOPE     IN  P_SCOPE
                       AND   LOGSYSO   IN  P_LOGSYO
                       AND   KSTAR     IN  P_KSTAR
                       AND   PBUKRS    IN  P_PBUKRS
                       AND   PARGB     IN  P_PARGB
                       AND   PFKBER    IN  P_PFKBER
                       AND   PSCOPE    IN  P_PSCOPE
                       AND   LOGSYSP   IN  P_LOGSYP
                       AND   PKSTAR    IN  P_PKSTAR.

    SORT LT_COEP BY KOKRS BELNR BUZEI.

*   make sure that all document are complete in which       "P99K052200
*   revenues on cost centers might be posted:               "P99K052200
    PERFORM COMPLETE_DOCS                                   "P99K052200
            TABLES    LT_COEP.                              "P99K052200

*   start processing:
    LOOP AT LT_COEP.
      GD_SAVE_TABIX = SY-TABIX.

*ANDY
*      CHECK NOT ( GC_NO_PARTNER_VRGNG CS LT_COEP-VRGNG ).
  CHECK ( GC_NO_PARTNER_VRGNG CS LT_COEP-VRGNG ).


      ADD 1 TO GD_NUM_REC_TOT.

      CLEAR LT_COEP_LIST.

      MOVE  LT_COEP-FKBER  TO  LT_COEP_LIST-FKBER_OLD.
      MOVE  LT_COEP-PFKBER TO  LT_COEP_LIST-PFKBER_OLD.
      CLEAR LT_COEP-FKBER.
      CLEAR LT_COEP-PFKBER.

      IF ( COBK-KOKRS NE LT_COEP-KOKRS ) OR
         ( COBK-BELNR NE LT_COEP-BELNR ).
*        Belegkopf lesen
        SELECT SINGLE * FROM COBK
               WHERE  KOKRS       = LT_COEP-KOKRS
               AND    BELNR       = LT_COEP-BELNR.
      ENDIF.

      ADD 1 TO LD_ZEILE.                                    "P9CK103047

      CALL FUNCTION 'MESSAGE_LINE_SET'
           EXPORTING
                ZEILE = LD_ZEILE.

      PERFORM SET_NEW_FKBER_PFKBER
              TABLES   LT_COIOB
                       LT_COEP
                       LT_COEP_LIST
              USING    COBK
                       LD_ZEILE
              CHANGING LT_COEP
                       GD_SUBRC.

      IF ( GD_SUBRC = 0 ).
        MOVE-CORRESPONDING LT_COEP TO LT_COEP_LIST.
        APPEND LT_COEP_LIST.
        IF ( LT_COEP-FKBER  <> LT_COEP_LIST-FKBER_OLD  ) OR
           ( LT_COEP-PFKBER <> LT_COEP_LIST-PFKBER_OLD ).
          MODIFY LT_COEP INDEX GD_SAVE_TABIX.
          ADD 1 TO GD_REC_CORRECTED.
        ENDIF.
      ELSE.
        MOVE-CORRESPONDING LT_COEP TO LT_COEP_LIST.
        MOVE 'X' TO LT_COEP_LIST-FLG_ERROR.
        APPEND LT_COEP_LIST.
      ENDIF.

    ENDLOOP.

    IF ( TEST_RUN = ' ' ).
*      DB update!

*      LOOP AT LT_COEP.
*        CHECK ( GC_NO_PARTNER_VRGNG CS LT_COEP-VRGNG ).
*        DELETE LT_COEP INDEX SY-TABIX.
*      ENDLOOP.

      LOOP AT LT_COEP_LIST.

        CHECK ( LT_COEP_LIST-FLG_ERROR = 'X' ) OR
              ( LT_COEP_LIST-FKBER  = LT_COEP_LIST-FKBER_OLD  AND
                LT_COEP_LIST-PFKBER = LT_COEP_LIST-PFKBER_OLD ).

        DELETE LT_COEP WHERE KOKRS = LT_COEP_LIST-KOKRS
                       AND   BELNR = LT_COEP_LIST-BELNR
                       AND   BUZEI = LT_COEP_LIST-BUZEI.
      ENDLOOP.

      IF NOT ( LT_COEP[] IS INITIAL ).
        UPDATE COEP FROM TABLE LT_COEP.
        CALL FUNCTION 'DB_COMMIT'.
      ENDIF.
    ENDIF.

    IF ( SHOW_LST = 'X' ).
      PERFORM SHOW_RECORDS
              TABLES  LT_COEP_LIST.
    ENDIF.

  ENDDO.

  CALL FUNCTION 'MESSAGES_COUNT'
       IMPORTING
            COUNT        = GD_COUNT
            MAX_SEVERITY = GD_MAX_SEV.

  IF ( GD_COUNT > 0 ).
    CALL FUNCTION 'MESSAGES_SHOW'.
  ENDIF.

* to show list dynpro even if list is empty:
  IF ( SHOW_LST = 'X' ).
    WRITE SY-ULINE.
    WRITE: /.
  ENDIF.

*=======================================================================
TOP-OF-PAGE.
*=======================================================================
  WRITE:  /1(50) 'Number of selected  line items :'(100),
                 GD_NUM_REC_TOT.

  WRITE:  /1(50) 'Number of changed   line items :'(200),
                 GD_REC_CORRECTED.

  CALL FUNCTION 'MESSAGES_COUNT'
       IMPORTING
            COUNT = GD_MESS_COUNT.

  IF ( GD_MESS_COUNT <> 0 ).
*   messages appeared: number of messages:
    FORMAT: HOTSPOT ON.
    WRITE  TEXT-500  TO  GD_STRING LEFT-JUSTIFIED.
    WRITE: /1(40) GD_STRING    COLOR COL_NORMAL.
    FORMAT: HOTSPOT OFF.
    WRITE AT 52 GD_MESS_COUNT.
  ENDIF.

  WRITE /.

  IF ( SHOW_LST = 'X' ).
    WRITE /: 'The following list contains only'(300),
             'the changed or errornous line items:'(400).
    WRITE /.

    WRITE SY-ULINE.

    WRITE:  /1    '|',
              2(6)    'ERROR'(001),      '|',
             10(5)    'FAREA'(002),      '|',
             17(9)    'FAREA_OLD'(003),  '|',
             28(6)    'PFAREA'(004),     '|',
             36(10)   'PFAREA_OLD'(005), '|',
             51(9)    'DOC. NR. '(006),  '|',
             63(9)    'LINE ITEM'(007),  '|',
             77(19)   'TR. AMOUNT         '(008), '|'.
    WRITE SY-ULINE.
  ENDIF.

*======================================================================*
AT LINE-SELECTION.
*======================================================================*

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
* form routines:
*=======================================================================

*&---------------------------------------------------------------------*
*&      Form  PROCESS_COEP_LINE
*&---------------------------------------------------------------------*
FORM SET_NEW_FKBER_PFKBER
         TABLES   T_COIOB     STRUCTURE  COIOB
                  T_COEP      STRUCTURE  COEP
                  T_COEP_LIST STRUCTURE  LT_COEP_LIST
         USING    R_COBK       LIKE COBK
                  R_ZEILE      LIKE MESG-ZEILE
         CHANGING C_COEP       LIKE COEP
                  C_SUBRC      LIKE SY-SUBRC.

  DATA:
  LD_MAX_SEVERITY    LIKE SY-SUBRC.

  CALL FUNCTION 'K_COEP_FAREA_SET_SINGLE'                   "P00K016482
       EXPORTING
           IS_COBK              = R_COBK
           ID_MFLAG             = 'X'  "read master data
       TABLES
           IT_COEP              = T_COEP
           IT_COIOB             = T_COIOB
       CHANGING
           CS_COEP              = C_COEP.

  CALL FUNCTION 'MESSAGES_COUNT'
       EXPORTING
            LINE_FROM    = LD_ZEILE
            LINE_TO      = LD_ZEILE
       IMPORTING
            MAX_SEVERITY = LD_MAX_SEVERITY.

  IF ( LD_MAX_SEVERITY GE  12 ).
    C_SUBRC = '1'.
  ELSE.
    CLEAR C_SUBRC.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SHOW_RECORDS
*&---------------------------------------------------------------------*
FORM SHOW_RECORDS
           TABLES  LT_COEP_LIST  STRUCTURE  LT_COEP_LIST.

  LOOP AT LT_COEP_LIST.

    CHECK ( LT_COEP_LIST-FLG_ERROR = 'X' ) OR
          ( LT_COEP_LIST-FKBER  <> LT_COEP_LIST-FKBER_OLD ) OR
          ( LT_COEP_LIST-PFKBER <> LT_COEP_LIST-PFKBER_OLD ).

    WRITE:   /1    '|',
              2     LT_COEP_LIST-FLG_ERROR,
              9     '|',
             11     LT_COEP_LIST-FKBER,
             16     '|',
             21     LT_COEP_LIST-FKBER_OLD,
             27     '|',
             31     LT_COEP_LIST-PFKBER,
             35     '|',
             41     LT_COEP_LIST-PFKBER_OLD,
             47     '|',
             50     LT_COEP_LIST-BELNR,
             61     '|',
             65     LT_COEP_LIST-BUZEI,
             73     '|',
             76     LT_COEP_LIST-WTGBTR CURRENCY LT_COEP_LIST-TWAER,
             97     '|'.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  COMPLETE_DOCS
*&---------------------------------------------------------------------*
FORM COMPLETE_DOCS                                          "P99K052200
     TABLES   CT_COEP  STRUCTURE  COEP.

  DATA:
  LT_COEP_SUPPLEMENT  LIKE  COEP  OCCURS 0 WITH HEADER LINE.

* check whether incomplete CO documents might have been selected:
* check ( not  P_KOKRS[]  is initial ) or
*       ( not  P_GJAHR[]  is initial ) or
*       ( not  P_PERIO[]  is initial ) or
*       ( not  P_BELNR[]  is initial ) or
  CHECK ( NOT  P_BUZEI[]  IS INITIAL ) OR
*       ( not  P_VRGNG[]  is initial ) or
        ( NOT  P_WRTTP[]  IS INITIAL ) OR
        ( NOT  P_VERSN[]  IS INITIAL ) OR
        ( NOT  P_OBJNR[]  IS INITIAL ) OR
        ( NOT  P_PAROB1[] IS INITIAL ) OR
        ( NOT  P_BUKRS[]  IS INITIAL ) OR
        ( NOT  P_GSBER[]  IS INITIAL ) OR
        ( NOT  P_FKBER[]  IS INITIAL ) OR
        ( NOT  P_SCOPE[]  IS INITIAL ) OR
        ( NOT  P_LOGSYO[] IS INITIAL ) OR
*       ( not  P_KSTAR[]  is initial ) or
        ( NOT  P_PBUKRS[] IS INITIAL ) OR
        ( NOT  P_PARGB[]  IS INITIAL ) OR
        ( NOT  P_PFKBER[] IS INITIAL ) OR
        ( NOT  P_PSCOPE[] IS INITIAL ) OR
        ( NOT  P_LOGSYP[] IS INITIAL ).                     "or
*       ( not  P_PKSTAR[] is initial ) or

* maybe there are revenues on cost centers;
* function module K_COEP_FAREA_SET needs posting and its
* cross entry in this case (c.f. note 168766)

* coep has been sorted before by kokrs belnr buzei.
  LOOP AT CT_COEP WHERE PAROB1(2) = 'AO'.

*   get this complete document from DB
    SELECT * FROM COEP APPENDING TABLE  LT_COEP_SUPPLEMENT
                       WHERE  KOKRS = CT_COEP-KOKRS
                       AND    BELNR = CT_COEP-BELNR.

    DELETE CT_COEP WHERE KOKRS = CT_COEP-KOKRS
                   AND   BELNR = CT_COEP-BELNR.
  ENDLOOP.

* append completed documents to ct_coep:
  LOOP AT LT_COEP_SUPPLEMENT.
    APPEND LT_COEP_SUPPLEMENT  TO CT_COEP.
  ENDLOOP.

  IF ( SY-SUBRC = 0 ).
*   order has changed:
    SORT CT_COEP BY KOKRS BELNR BUZEI.
    IF ( 1 = 2 ). MESSAGE I300(K5). ENDIF.      "cross reference
    CALL FUNCTION 'MESSAGE_STORE'
         EXPORTING
              ARBGB = 'K5'
              MSGTY = 'I'
              TXTNR = '300'.
  ENDIF.

ENDFORM.
