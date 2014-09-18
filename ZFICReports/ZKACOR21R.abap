*&-------------------------------------------------------------------*
*$*$--------------------------------------------------------------$*$*
*$ Correction Inst.         0120061532 0000464435                     $*
*$------------------------------------------------------------------$*
*$ Valid for       :                                                  $*
*$ Software Component   SAP_APPL   SAP Application                    $*
*$  Release 45A          All Support Package Levels                   $*
*$  Release 45B          All Support Package Levels                   $*
*$  Release 46A          To SAPKH46A34                                $*
*$  Release 46B          All Support Package Levels                   $*
*$  Release 46C          All Support Package Levels                   $*
*$  Release 470          All Support Package Levels                   $*
*$------------------------------------------------------------------$*
*$ Changes/Objects Not Contained in Standard SAP System               $*
*$*$--------------------------------------------------------------$*$*
*&-------------------------------------------------------------------*
*& Object          REPS RKACOR21R
*& Object Header   PROG RKACOR21R
*&-------------------------------------------------------------------*
*& PROGRAM RKACOR21R
*&-------------------------------------------------------------------*
*>>>> START OF INSERTION <<<<
*&---------------------------------------------------------------------*
*& Report  RKACOR21R
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&  - reverse CO document and update CO totols w/o posting             *
*&    to other components (P_RWIN  = SPACE)                            *
*&  - updates document text for both the reversal and the reversed doc.*
*&  - creates application log for documents having been processed      *
*&  - does not check whether posting period is blocked                 *
*&  - does not check whether document has been reversed already        *
*&    ( in case of doubt check document text of doc. to be reveresed,  *
*&      if reversed already text will be 'RKACOR21R - reversed by xxx' *
*&      xxx being the document number of the reversal document )       *
*&                                                                     *
*   Use very carefully!                                                *
*----------------------------------------------------------------------*
*  selection options:                                                  *
*    - p_kokrs  controlling area                                       *
*    - s_belnr  number of  CO document                                 *
*    - p_budat  posting date (reversal)                                *
*    - p_perio  posting period if posting into special period          *
*----------------------------------------------------------------------*
REPORT RKACOR21R MESSAGE-ID KI.

TABLES: MESG,                          "for appl. log
        COBK,
        COEP,
        COEPD,
        COEPL,
        COEPR,
        TKA01.

PARAMETERS:
  P_KOKRS  LIKE CCSS-KOKRS MEMORY ID CAC OBLIGATORY.
SELECT-OPTIONS:
  S_BELNR FOR COEP-BELNR OBLIGATORY
          MEMORY ID BLN.
SELECTION-SCREEN SKIP 1.
PARAMETERS:
  P_BUDAT TYPE BUDAT OBLIGATORY DEFAULT SY-DATLO,
  P_PERIO LIKE COEP-PERIO.

SELECTION-SCREEN SKIP 1.

PARAMETERS:
  P_RWIN  LIKE BOOLE-BOOLE       DEFAULT ' ' NO-DISPLAY,
  "Invisible parameter: Call FM 'K_COEP_COMPLETE'
  P_COMPL LIKE BOOLE-BOOLE DEFAULT 'X' NO-DISPLAY,
  P_PROT  LIKE COFI_SCR-FLG_LIST DEFAULT 'X' MODIF ID NIP,  "ALRK294908
  P_TEST  LIKE COFI_SCR-FLG_TEST DEFAULT 'X'.

DATA:
  TRUE  LIKE BOOLE-BOOLE VALUE 'X',
  FALSE LIKE TRUE VALUE ' ',
  I TYPE I,
  WKGBTR LIKE COEP-WKGBTR,
  BEGIN OF GD_NEWDOC,
    BUDAT  TYPE BUDAT,
    GJAHR  TYPE GJAHR,
    PERIO  LIKE COEP-PERIO,
  END OF GD_NEWDOC.

DATA: BEGIN OF ITSEL1 OCCURS 0,
        REFBK LIKE COBK-REFBK,
        REFBN LIKE COBK-REFBN,
        REFGJ LIKE COBK-REFGJ,
      END OF ITSEL1.

DATA: BEGIN OF ITSEL2 OCCURS 0,
        REFBN LIKE COBK-REFBN,
        AWORG LIKE COBK-AWORG,
        AWTYP LIKE COBK-AWTYP,
      END OF ITSEL2.

DATA: CON_BALHDR_OBJECT LIKE BALHDR-OBJECT VALUE 'CORS',
      CON_BALHDR_SUBOBJECT LIKE BALHDR-SUBOBJECT VALUE 'DOCUMENT',
      CON_MSGID LIKE SY-MSGID VALUE 'KI'.

DATA: BEGIN OF COBK_DEL OCCURS 0,
        KOKRS LIKE COBK-KOKRS,
        BELNR LIKE COBK-BELNR,
        VRGNG LIKE COBK-VRGNG,
        REFBT LIKE COBK-REFBT,
        REFBN LIKE COBK-REFBN,
        REFBK LIKE COBK-REFBK,
        AWORG LIKE COBK-AWORG,
      END OF COBK_DEL.
DATA: BEGIN OF ITCOBK OCCURS 1.
        INCLUDE STRUCTURE COBK.
DATA: END OF ITCOBK.
DATA: BEGIN OF ITCOEP OCCURS 0.
        INCLUDE STRUCTURE COEP.
DATA: END OF ITCOEP.
DATA: BEGIN OF ITCOEPL OCCURS 0.
        INCLUDE STRUCTURE COEPL.
DATA: END OF ITCOEPL.
DATA: BEGIN OF ITCOEPR OCCURS 0.
        INCLUDE STRUCTURE COEPR.
DATA: END OF ITCOEPR.
DATA:
  GT_PERIODS LIKE PERIODS OCCURS 0 WITH HEADER LINE,
  GT_COIOB   LIKE COIOB   OCCURS 0 WITH HEADER LINE.

* tabelle TKOMP.
INCLUDE IS01AD00.

INITIALIZATION.
  AUTHORITY-CHECK OBJECT 'S_ADMI_FCD'
    ID 'S_ADMI_FCD' FIELD 'RSET'.
  IF SY-SUBRC NE 0.
    MESSAGE I107.
    LEAVE TO TRANSACTION SPACE.
  ENDIF.

START-OF-SELECTION.
  FORMAT INTENSIFIED OFF.                                   "ALRK294908
  PERFORM CHECKS.
  PERFORM DATE_PERIOD_CHECK.
  PERFORM LOG_SEL.
  PERFORM COBK_KEYS_COLLECT.
  PERFORM LOG_DOC.
  PERFORM DOCUMENTS.

TOP-OF-PAGE.

  IF I > 0.
    IF P_TEST = SPACE.
      WRITE: / 'Incorrect documents will be canceled'.      "#EC NOTEXT
    ELSE.
   WRITE: / 'Test mode: incorrect documents are only listed'."#EC NOTEXT
    ENDIF.
    SKIP 1.

    WRITE: /1 'Clnt',                                       "#EC NOTEXT
            7 'CoAra',                                      "#EC NOTEXT
           13 'CO doc',                                     "#EC NOTEXT
           24 'Year',                                       "#EC NOTEXT
           29 'Perio',                                      "#EC NOTEXT
           35 'AWTyp',                                      "#EC NOTEXT
           42 'Refbn',                                      "#EC NOTEXT
           53 'Aworg',                                      "#EC NOTEXT
           69 'WKGBTR'.
    ULINE.
  ENDIF.

AT SELECTION-SCREEN OUTPUT.                                 "ALRK294908
  LOOP AT SCREEN.
    IF SCREEN-GROUP1 = 'NIP'.
      SCREEN-INPUT = '0'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.


*----------------------------------------------------------------------*
*       Form  CHECKS
*----------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CHECKS.

  DESCRIBE TABLE S_BELNR LINES I.
  IF I = 0.
    MESSAGE E016(GP) WITH 'Error:'
            'no document specified'.                        "#EC NOTEXT
    STOP.
  ENDIF.
* Check CO-area
  CALL FUNCTION 'K_KOKRS_READ'
       EXPORTING
            KOKRS   = P_KOKRS
       IMPORTING
            E_TKA01 = TKA01
       EXCEPTIONS
            OTHERS  = 1.
  IF NOT SY-SUBRC IS INITIAL.
    MESSAGE ID SY-MSGID TYPE 'W' NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
*       Form  COBK_KEYS_COLLECT
*----------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM COBK_KEYS_COLLECT.

  SELECT * FROM COBK
         WHERE KOKRS = P_KOKRS
         AND   BELNR IN S_BELNR.
    MOVE-CORRESPONDING COBK TO COBK_DEL.
    APPEND COBK_DEL.
  ENDSELECT.
  DESCRIBE TABLE COBK_DEL LINES I.
  IF I = 0.
    MESSAGE E016(GP) WITH 'No CO-document found'.           "#EC NOTEXT
    STOP.
  ENDIF.
ENDFORM.                               " COBK_KEYS_COLLECT

*----------------------------------------------------------------------*
*       Form  DOCUMENTS
*----------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM DOCUMENTS.
  LOOP AT COBK_DEL.
    REFRESH: ITCOBK,
             ITCOEP,
             ITCOEPL,
             ITCOEPR.
*   COBK
    SELECT SINGLE * FROM COBK WHERE KOKRS = COBK_DEL-KOKRS
                                AND BELNR = COBK_DEL-BELNR.
    CHECK SY-SUBRC = 0.
    ITCOBK = COBK.
    APPEND ITCOBK.
*   COEP
    SELECT * FROM COEP WHERE KOKRS = COBK-KOKRS
                         AND BELNR = COBK-BELNR.
      WKGBTR      = COEP-WKGBTR.
      COEP-WTGBTR = - COEP-WTGBTR.
      COEP-WOGBTR = - COEP-WOGBTR.
      COEP-WKGBTR = - COEP-WKGBTR.
      COEP-WKFBTR = - COEP-WKFBTR.
      COEP-PAGBTR = - COEP-PAGBTR.
      COEP-PAFBTR = - COEP-PAFBTR.
      COEP-MEGBTR = - COEP-MEGBTR.
      COEP-MEFBTR = - COEP-MEFBTR.
      COEP-MBGBTR = - COEP-MBGBTR.
      COEP-MBFBTR = - COEP-MBFBTR.
      ITCOEP = COEP.
      APPEND ITCOEP.
      IF P_PROT = 'X'.
        WRITE: /  ITCOBK-MANDT,
                7 ITCOBK-KOKRS,
               13 ITCOBK-BELNR,
               24 ITCOBK-GJAHR,
               29 ITCOBK-PERAB,
               35 ITCOBK-AWTYP,
               42 ITCOBK-REFBN,
               53 ITCOBK-AWORG,
               69 WKGBTR CURRENCY ITCOBK-KWAER.
      ENDIF.
    ENDSELECT.
*   COEPL
    SELECT * FROM COEPL WHERE KOKRS = COBK-KOKRS
                          AND BELNR = COBK-BELNR.
      COEPL-LSTBTR = - COEPL-LSTBTR.
      COEPL-LSBBTR = - COEPL-LSBBTR.
      COEPL-KAPBTR = - COEPL-KAPBTR.
      COEPL-AUSBTR = - COEPL-AUSBTR.
      COEPL-AUBBTR = - COEPL-AUBBTR.
      COEPL-DISBTR = - COEPL-DISBTR.
      ITCOEPL = COEPL.
      APPEND ITCOEPL.
    ENDSELECT.
*   COEPR
    SELECT * FROM COEPR WHERE KOKRS = COBK-KOKRS
                        AND   BELNR = COBK-BELNR.
      IF COEPR-GRTYP EQ '2'.
        COEPR-SMEBTR = - COEPR-SMEBTR.
        COEPR-SMABTR = - COEPR-SMABTR.
      ELSE.
        COEPR-SMEBTR = 0.
        COEPR-SMABTR = 0.
      ENDIF.
      ITCOEPR = COEPR.
      APPEND ITCOEPR.
      IF P_PROT = 'X'.
        WRITE: /  ITCOBK-MANDT,
                7 ITCOBK-KOKRS,
               13 ITCOBK-BELNR,
               24 ITCOBK-GJAHR,
               29 ITCOBK-PERAB,
               35 ITCOBK-AWTYP,
               42 ITCOBK-REFBN,
               53 ITCOBK-AWORG.
*              69 wkgbtr CURRENCY itcobk-kwaer.
      ENDIF.
    ENDSELECT.

    PERFORM RWIN_CHECK
            TABLES ITCOEP
                   ITCOEPL
                   GT_COIOB
                   GT_PERIODS
            USING  ITCOBK.

    IF P_TEST = SPACE.
      SET UPDATE TASK LOCAL.
      PERFORM RWIN_POST
              TABLES ITCOEP
                     ITCOEPL
                     GT_COIOB
                     GT_PERIODS
              USING  ITCOBK.
      PERFORM POST_INVERTED_DOCUMENT ON COMMIT.
      COMMIT WORK.
    ENDIF.
  ENDLOOP.                             "LOOP AT COBK_DEL
ENDFORM.                               "DOCUMENTS

*----------------------------------------------------------------------*
*       Form  LOG_SEL
*----------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM LOG_SEL.

  CHECK P_TEST IS INITIAL.

  MESG-MSGTY = 'I'.
  MESG-ARBGB = 'KI'.
  MESG-TXTNR = 108.
  IF P_TEST IS INITIAL.
    CALL FUNCTION 'K_APPL_LOG_WRITE_REPSELS'
         EXPORTING
              I_SUBOBJECT = CON_BALHDR_SUBOBJECT
              I_REPORT    = SY-CPROG
              I_MESG      = MESG.
  ENDIF.
  IF 1 = 2.
    MESSAGE I108.
  ENDIF.
ENDFORM.                               " LOG_SEL

*----------------------------------------------------------------------*
*       Form  LOG_DOC
*----------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM LOG_DOC.

  DATA: BEGIN OF LD_BALMI.             " Nachricht f?s Anwendungs-Log
          INCLUDE STRUCTURE BALMI.
  DATA: END OF LD_BALMI.
  DATA: BEGIN OF LT_SPAR OCCURS 1.     " f? Selektionen
          INCLUDE STRUCTURE SPAR.
  DATA: END OF LT_SPAR.
  DATA: BEGIN OF LT_BALNRI OCCURS 0.   " Dummy f? Aufruf von
          INCLUDE STRUCTURE BALNRI.    " APPL_LOG_WRITE_DB
  DATA: END OF LT_BALNRI.
  DATA: BEGIN OF LD_DOC,
          BELNR LIKE COBK-BELNR,
          FILL1 TYPE C VALUE '/',
          VRGNG LIKE COBK-VRGNG,
          FILL2 TYPE C VALUE '/',
          REFBT LIKE COBK-REFBT,
          FILL3 TYPE C VALUE '/',
          REFBN LIKE COBK-REFBN,
          FILL4 TYPE C VALUE '/',
          REFBK LIKE COBK-REFBK,
          FILL5 TYPE C VALUE '/',
          AWORG LIKE COBK-AWORG,
        END OF LD_DOC.


  CHECK P_TEST IS INITIAL.

* Message ins Application-Log schreiben
  CLEAR LD_BALMI.
  READ TABLE COBK_DEL INDEX 1.
  LD_BALMI-MSGTY  = 'I'.
  LD_BALMI-MSGID  = CON_MSGID.
  LD_BALMI-MSGNO  = '109'.
  LD_BALMI-MSGV1  = COBK_DEL-KOKRS.
  LD_BALMI-USEREXITP = SY-REPID.       "Programm der FGruppe
  LD_BALMI-USEREXITF = 'K_APPL_LOG_SHOW_DOCUMENTS'.

  LT_SPAR-PARAM = 'DOCUMENT'.
  LOOP AT COBK_DEL.
    MOVE-CORRESPONDING COBK_DEL TO LD_DOC.
    LT_SPAR-VALUE = LD_DOC.
    APPEND LT_SPAR.
  ENDLOOP.

  CALL FUNCTION 'APPL_LOG_WRITE_MESSAGE_PARAMS'
       EXPORTING
            OBJECT           = CON_BALHDR_OBJECT
            SUBOBJECT        = CON_BALHDR_SUBOBJECT
            MESSAGE          = LD_BALMI
            UPDATE_OR_INSERT = 'I'
       TABLES
            PARAMETERS       = LT_SPAR.

* Protokoll sichern
  CALL FUNCTION 'APPL_LOG_WRITE_DB'
       EXPORTING
            OBJECT                = CON_BALHDR_OBJECT
            SUBOBJECT             = CON_BALHDR_SUBOBJECT
       TABLES
            OBJECT_WITH_LOGNUMBER = LT_BALNRI.

ENDFORM.                               " LOG_DOC

*----------------------------------------------------------------------*
*       Form  K_APPL_LOG_SHOW_DOCUMENTS
*----------------------------------------------------------------------*
*       Userexit f? Anzeige Appliction-Log                            *
*----------------------------------------------------------------------*
FORM K_APPL_LOG_SHOW_DOCUMENTS TABLES IT_SPAR STRUCTURE SPAR.

  DATA: LD_LENGTH TYPE I,
        LD_MOD    TYPE I,
        LD_I      TYPE I.

  ULINE 1(2). WRITE 'Documents' COLOR 4.                    "#EC NOTEXT
  LD_LENGTH = 14.
  LD_I = 123 - LD_LENGTH + 1.
  POSITION LD_LENGTH. WRITE AT (LD_I) SY-ULINE.
  LD_I = 0.
  LOOP AT IT_SPAR.
    CHECK IT_SPAR-PARAM = 'DOCUMENT'.
    ADD 1 TO LD_I.
    LD_MOD = LD_I MOD 3.
    IF LD_MOD = 1.
      WRITE /1 SY-VLINE.
    ENDIF.
    WRITE (39) IT_SPAR-VALUE.
    IF LD_MOD = 0.
      WRITE 123 SY-VLINE.
    ENDIF.
  ENDLOOP.
  IF LD_MOD <> 0.
    WRITE 123 SY-VLINE.
  ENDIF.
  ULINE AT (123).
ENDFORM.                               " K_APPL_LOG_SHOW_DOCUMENTS

*----------------------------------------------------------------------*
*       Form  DELETE_DOCUMENT
*----------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM DELETE_DOCUMENT.

* Summens?ze berichtigen
  CALL FUNCTION 'K_DOCUMENT_UPDATE'
       EXPORTING
            I_TOTALS_UPDATE = 'X'
            I_ITEMS_INSERT  = SPACE
            I_RCL_UPDATE    = 'X'
       TABLES
            T_COBK          = ITCOBK
            T_COEP          = ITCOEP
            T_COEPR         = ITCOEPR
            T_COEPL         = ITCOEPL.
* Alte Einzelposten + Belegkopf l?chen
  DELETE COBK  FROM TABLE ITCOBK.
  DELETE COEP  FROM TABLE ITCOEP.
  DELETE COEPR FROM TABLE ITCOEPR.
  DELETE COEPL FROM TABLE ITCOEPL.

ENDFORM.                               " DELETE_DOCUMENT

*----------------------------------------------------------------------*
*       Form  POST_INVERTED_DOCUMENT
*----------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM POST_INVERTED_DOCUMENT.

  DATA:
    LT_COBKUPD LIKE COBK OCCURS 0 WITH HEADER LINE,         "ALRK294908
    LD_BELNR LIKE COBK-BELNR.
  DATA:
    LD_VRGNG LIKE COBK-VRGNG.

* Nummernvergabe
  LOOP AT ITCOBK.
    LD_VRGNG = ITCOBK-VRGNG.
    IF LD_VRGNG = 'KAZV' OR
       LD_VRGNG = 'KAZM' OR
       LD_VRGNG = 'KAZN'.
      LD_VRGNG = 'KAZO'.
    ENDIF.
    CALL FUNCTION 'K_NUMBER_GET_CO_DOCUMENT'
         EXPORTING
              KOKRS = ITCOBK-KOKRS
              VRGNG = LD_VRGNG
         IMPORTING
              BELNR = LD_BELNR.
    LT_COBKUPD = ITCOBK.                                    "ALRK294908
    CLEAR LT_COBKUPD-BLTXT.
    CONCATENATE 'RKACOR21R - reversed by' LD_BELNR
       INTO LT_COBKUPD-BLTXT SEPARATED BY ' '.
    APPEND LT_COBKUPD.                                      "ALRK294908
    CLEAR ITCOBK-REFBN.                                     "ALRK294908
    LOOP AT ITCOEP WHERE KOKRS = ITCOBK-KOKRS
                     AND BELNR = ITCOBK-BELNR.
      ITCOEP-BELNR = LD_BELNR.
      ITCOEP-PERIO = GD_NEWDOC-PERIO.
      ITCOEP-GJAHR = GD_NEWDOC-GJAHR.
      MODIFY ITCOEP.
    ENDLOOP.
    LOOP AT ITCOEPL WHERE KOKRS = ITCOBK-KOKRS
                      AND BELNR = ITCOBK-BELNR.
      ITCOEPL-BELNR = LD_BELNR.
      ITCOEP-PERIO = GD_NEWDOC-PERIO.
      ITCOEP-GJAHR = GD_NEWDOC-GJAHR.
      MODIFY ITCOEPL.
    ENDLOOP.
    CLEAR ITCOBK-AWTYP.                                     "ALRK294908
    CLEAR ITCOBK-AWORG.                                     "ALRK294908
    CLEAR ITCOBK-AWREF_REV.                                 "ALRK294908
    CLEAR ITCOBK-AWORG_REV.                                 "ALRK294908
    ITCOBK-BLTXT = 'RKACOR21R correction document for'.     "#EC NOTEXT
    CONCATENATE ITCOBK-BLTXT ITCOBK-BELNR                   "ALRK294908
                INTO ITCOBK-BLTXT SEPARATED BY ' '.         "ALRK294908
    ITCOBK-REFBT = 'K'.                                     "ALRK294908
    ITCOBK-REFBN = ITCOBK-BELNR.                            "ALRK294908
    ITCOBK-USNAM = SY-UNAME.                                "ALRK294908
    ITCOBK-CPUDT = SY-DATLO.                                "ALRK294908
    ITCOBK-CPUTM = SY-UZEIT.
    ITCOBK-BELNR = LD_BELNR.
    ITCOBK-GJAHR = GD_NEWDOC-GJAHR.
    ITCOBK-PERAB = GD_NEWDOC-PERIO.
    ITCOBK-PERBI = GD_NEWDOC-PERIO.
    ITCOBK-BUDAT = GD_NEWDOC-BUDAT.
    MODIFY ITCOBK.
  ENDLOOP.

  UPDATE COBK FROM LT_COBKUPD.                              "ALRK294908

* Neue EPs + Summens?ze buchen
  CALL FUNCTION 'K_DOCUMENT_UPDATE'
       EXPORTING
            I_TOTALS_UPDATE = 'X'
            I_ITEMS_INSERT  = 'X'
            I_RCL_UPDATE    = 'X'
       TABLES
            T_COBK          = ITCOBK
            T_COEP          = ITCOEP
            T_COEPL         = ITCOEPL.

ENDFORM.                               " POST_INVERTED_DOCUMENT

*---------------------------------------------------------------------*
*       FORM rwin_check                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM RWIN_CHECK
     TABLES T_COEP    STRUCTURE COEP
            T_COEPL   STRUCTURE COEPL
            T_COIOB   STRUCTURE COIOB
            T_PERIODS STRUCTURE PERIODS
     USING  I_COBK    STRUCTURE COBK.

  CHECK P_RWIN = TRUE.

  DATA:
    LD_PROCESS  LIKE TRWPR-PROCESS,
    LD_EVENT    LIKE TRWPR-EVENT,
    LT_COBK     LIKE COBK OCCURS 1,
    LD_NO_RWIN  LIKE BOOLE-BOOLE.

  PERFORM RWIN_PROCESS_EVENT_GET
          USING     'CHECK'
                    I_COBK-VRGNG
          CHANGING  LD_PROCESS
                    LD_EVENT
                    LD_NO_RWIN.
  CHECK LD_NO_RWIN IS INITIAL.

* CALL FUNCTION 'K_PERIODS_GET_FOR_GJAHR'   "not available in Rel. 3.1
*      EXPORTING
*           I_KOKRS   = I_COBK-KOKRS
*           I_GJAHR   = I_COBK-GJAHR
*      TABLES
*           T_PERIODS = T_PERIODS.
  CALL FUNCTION 'G_PERIODS_OF_YEAR_GET'
       EXPORTING
            VARIANT   = TKA01-LMONA
            YEAR      = I_COBK-GJAHR
       TABLES
            I_PERIODS = T_PERIODS.
  LOOP AT T_COEP
       WHERE KOKRS = I_COBK-KOKRS
       AND   BELNR = I_COBK-BELNR.
    PERFORM COIOB_READ_AND_FILL
            TABLES   T_COIOB
            USING    T_COEP-KOKRS
                     T_COEP-OBJNR
                     I_COBK-BUDAT
            CHANGING T_COIOB
                     SY-SUBRC
                     SY-TABIX.
    PERFORM COIOB_READ_AND_FILL                             "ALRK239987
            TABLES   T_COIOB                                "ALRK239987
            USING    T_COEP-KOKRS                           "ALRK239987
                     T_COEP-PAROB1                          "ALRK239987
                     I_COBK-BUDAT                           "ALRK239987
            CHANGING T_COIOB                                "ALRK239987
                     SY-SUBRC                               "ALRK239987
                     SY-TABIX.                              "ALRK239987
  ENDLOOP.

* Zeitpunkt Belegkopf fertig
  CALL FUNCTION 'RWIN_CHECK'
       EXPORTING
            GJAHR   = I_COBK-GJAHR
            PROCESS = 'K-BELEG '
            EVENT   = 'KOPF    '
       TABLES
            TKOMP   = TKOMP.
  LOOP AT TKOMP.
    CALL FUNCTION TKOMP-FUNCTION
         EXPORTING
              I_COBK  = I_COBK
              I_KWAER = I_COBK-KWAER
              I_TWAER = T_COEP-TWAER.
  ENDLOOP.
* Process document check                                               *
  APPEND I_COBK TO LT_COBK.
  CALL FUNCTION 'RWIN_CHECK'
       EXPORTING
            GJAHR   = I_COBK-GJAHR
            PROCESS = LD_PROCESS
            EVENT   = LD_EVENT
       TABLES
            TKOMP   = TKOMP.
  LOOP AT TKOMP.
    IF P_COMPL = FALSE.
      CHECK TKOMP-FUNCTION NE 'K_COEP_COMPLETE'.
    ENDIF.
    CALL FUNCTION TKOMP-FUNCTION
         TABLES
              I_COBK  = LT_COBK
              I_COEP  = T_COEP
              I_COEPL = T_COEPL
              I_COIOB = T_COIOB.
  ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM rwin_process_event_get                                   *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM RWIN_PROCESS_EVENT_GET
          USING     I_ACTION           "CHECK/POST
                    I_VRGNG    LIKE COBK-VRGNG
          CHANGING  E_PROCESS  LIKE TRWPR-PROCESS
                    E_EVENT    LIKE TRWPR-EVENT
                    E_NO_RWIN  LIKE BOOLE-BOOLE.

  DATA:
    LD_RKACT_CL LIKE RKACT_CL.

  CLEAR E_PROCESS.
  CLEAR E_EVENT.

  IF I_VRGNG IS INITIAL.
    MESSAGE X011(K5) WITH 'ERROR IN COBK' '' '' ''.
  ENDIF.

  CALL FUNCTION 'K_VRGNG_CLASSIFY'
       EXPORTING
            I_VRGNG    = I_VRGNG
       IMPORTING
            E_RKACT_CL = LD_RKACT_CL.

  IF LD_RKACT_CL-SIGN_AP <> 'A'.       "actual.
*   This program is not designed to handle activities other than
*   actual activities
    MESSAGE X011(K5) WITH 'PROGRAM ERROR' '' '' ''.
  ENDIF.
  IF 'COIE/COIN/KZRI/RKS ' CS I_VRGNG.
    E_NO_RWIN = 'X'.
    EXIT.
* ELSEIF 'KSII/RKIL/RKL/RKN/' CS I_VRGNG.                   "ALRK239987
  ELSEIF 'KSII/RKIL/RKL/RKN/RKLN/KOLI' CS I_VRGNG.          "ALRK239987
    E_PROCESS = 'K-BELEG2'.
  ELSEIF 'RKS/' CS I_VRGNG.
    E_PROCESS = 'K-BELEG3'.
  ELSEIF 'KOAO/KOAE/' CS I_VRGNG.
    E_PROCESS = 'K-ABRECH'.
  ELSE.
    E_PROCESS = 'K-BELEG1'.
  ENDIF.

* Set E_EVENT
  IF I_ACTION = 'CHECK'.
    E_EVENT = 'PRUEFEN'.
  ELSEIF I_ACTION = 'POST'.
    E_EVENT = 'FERTIG'.
  ELSE.
    MESSAGE X011(K5) WITH 'PROGRAM ERROR'
                          'ACTION NOT ALLOWED' I_ACTION ''.
  ENDIF.

  IF SY-SAPRL(2) >= '45'                                  "#EC PORTABLE
  AND ( E_PROCESS = 'K-BELEG1' OR
        E_PROCESS = 'K-BELEG2' ).
    E_PROCESS = 'K-BELEG '.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM rwin_post                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM RWIN_POST
     TABLES   T_COEP  STRUCTURE COEP
              T_COEPL STRUCTURE COEPL
              T_COIOB STRUCTURE COIOB
              T_PERIODS STRUCTURE PERIODS
     USING    I_COBK    STRUCTURE COBK.

  CHECK P_RWIN = TRUE.

  DATA:
    LD_PROCESS LIKE TRWPR-PROCESS,
    LD_EVENT   LIKE TRWPR-EVENT,
    LT_COBK  LIKE COBK  OCCURS 1 WITH HEADER LINE,
    LT_COEP  LIKE COEP  OCCURS 0 WITH HEADER LINE,
    LT_COEPL LIKE COEPL OCCURS 0 WITH HEADER LINE,
    LD_NO_RWIN  LIKE BOOLE-BOOLE.

  PERFORM RWIN_PROCESS_EVENT_GET
          USING     'POST'
                    I_COBK-VRGNG
          CHANGING  LD_PROCESS
                    LD_EVENT
                    LD_NO_RWIN.
  CHECK LD_NO_RWIN IS INITIAL.

* Zeitpunkt Beleg buchen
  CALL FUNCTION 'RWIN_CHECK'
       EXPORTING
            GJAHR   = I_COBK-GJAHR
            PROCESS = LD_PROCESS
            EVENT   = LD_EVENT
       TABLES
            TKOMP   = TKOMP.

  APPEND I_COBK  TO LT_COBK.
  LT_COEP[] = T_COEP[].
  LT_COEPL[] = LT_COEPL[].
* Replace first character of doc.no. to avoid duplicate records in RWIN
  LOOP AT LT_COBK.
    LT_COBK-BELNR(1) = 'X'.
    MODIFY LT_COBK.
  ENDLOOP.
  LOOP AT LT_COEP.
    LT_COEP-BELNR(1) = 'X'.
    MODIFY LT_COEP.
  ENDLOOP.
  LOOP AT LT_COEPL.
    LT_COEPL-BELNR(1) = 'X'.
    MODIFY LT_COEPL.
  ENDLOOP.

  LOOP AT TKOMP.
    CALL FUNCTION TKOMP-FUNCTION
         TABLES
              I_COBK  = LT_COBK
              I_COEP  = LT_COEP
              I_COEPL = LT_COEPL
              I_COIOB = T_COIOB.
  ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM COIOB_READ                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM COIOB_READ
     TABLES   T_COIOB   STRUCTURE  COIOB
     USING    R_OBJNR   LIKE       COIOB-OBJNR
              R_DATBI   LIKE       COIOB-DATBI
     CHANGING C_COIOB   STRUCTURE  COIOB
              C_SUBRC   LIKE       SY-SUBRC
              C_TABIX   LIKE       SY-TABIX.

* Test read COIOB.
* Read with binary search not practicable because of missing Key
* field DATBI.
  READ TABLE T_COIOB WITH KEY R_OBJNR BINARY SEARCH.

  C_SUBRC = SY-SUBRC.
  C_TABIX = SY-TABIX.

  CHECK ( C_SUBRC = 0 ).

  C_COIOB = T_COIOB.

* Should datbi be used to read coiob?
  CHECK ( NOT R_DATBI IS INITIAL ).
  CHECK ( NOT T_COIOB-DATBI IS INITIAL ).
  CHECK ( T_COIOB-DATBI LT R_DATBI ).

* read coiob using datbi:
  LOOP AT T_COIOB FROM C_TABIX
          WHERE OBJNR = R_OBJNR
          AND   DATBI GE R_DATBI.
  ENDLOOP.

  C_SUBRC = SY-SUBRC.

  IF ( C_SUBRC = 0 ).
    C_COIOB = T_COIOB.
    C_TABIX = SY-TABIX.
  ELSE.
    C_TABIX = SY-TABIX + 1.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM COIOB_read_AND_FILL                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  T_COIOB                                                       *
*  -->  I_KOKRS                                                       *
*  -->  I_OBJNR                                                       *
*  -->  I_BUDAT                                                       *
*  -->  E_COIOB                                                       *
*  -->  E_SUBRC                                                       *
*  -->  E_TABIX                                                       *
*---------------------------------------------------------------------*
FORM COIOB_READ_AND_FILL
     TABLES   T_COIOB STRUCTURE COIOB
     USING    I_KOKRS LIKE CCSS-KOKRS
              I_OBJNR LIKE IONRA-OBJNR
              I_BUDAT LIKE COBK-BUDAT
     CHANGING E_COIOB LIKE COIOB
              E_SUBRC LIKE SY-SUBRC
              E_TABIX LIKE SY-TABIX.

  STATICS:
    LDS_GJAHR LIKE CCSS-GJAHR,
    LDS_BUDAT LIKE COBK-BUDAT,
    LDS_BUPER LIKE CCSS-BUPER.

  IF TKA01-KOKRS NE I_KOKRS
  OR TKA01-LMONA IS INITIAL.
    CALL FUNCTION 'K_KOKRS_READ'
         EXPORTING
              KOKRS   = I_KOKRS
         IMPORTING
              E_TKA01 = TKA01.
  ENDIF.
  IF  I_BUDAT <> LDS_BUDAT
  OR  I_KOKRS <> TKA01-KOKRS.
    CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'
         EXPORTING
              I_DATE  = I_BUDAT
              I_PERIV = TKA01-LMONA
         IMPORTING
              E_BUPER = LDS_BUPER
              E_GJAHR = LDS_GJAHR
         EXCEPTIONS
              OTHERS  = 4.
    IF SY-SUBRC <> 0.
      MESSAGE ID   SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.
  PERFORM READ_MASTER_DATA
      TABLES   T_COIOB
      USING    I_OBJNR
               I_BUDAT
               LDS_BUPER
               LDS_GJAHR
               I_KOKRS
      CHANGING E_SUBRC
               E_TABIX.
  IF E_SUBRC = 0.
    PERFORM COIOB_READ
            TABLES   T_COIOB
            USING    I_OBJNR
                     I_BUDAT
            CHANGING E_COIOB
                     E_SUBRC
                     E_TABIX.
  ENDIF.
  LDS_BUDAT = I_BUDAT.
ENDFORM.                               "COIOB_FILL

*---------------------------------------------------------------------*
*       FORM COIOB_FILL                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  T_COIOB                                                       *
*  -->  I_KOKRS                                                       *
*  -->  I_OBJNR                                                       *
*  -->  I_BUDAT                                                       *
*  -->  E_COIOB                                                       *
*  -->  E_SUBRC                                                       *
*  -->  E_TABIX                                                       *
*---------------------------------------------------------------------*
FORM COIOB_FILL
     TABLES   T_COIOB STRUCTURE COIOB
     USING    I_KOKRS LIKE CCSS-KOKRS
                                                                 Page 42

              I_OBJNR LIKE IONRA-OBJNR
              I_BUDAT LIKE COBK-BUDAT
     CHANGING E_COIOB LIKE COIOB
              E_SUBRC LIKE SY-SUBRC
              E_TABIX LIKE SY-TABIX.

  STATICS:
    LDS_GJAHR LIKE CCSS-GJAHR,
    LDS_BUDAT LIKE COBK-BUDAT,
    LDS_BUPER LIKE CCSS-BUPER.

  IF TKA01-KOKRS NE I_KOKRS
  OR TKA01-LMONA IS INITIAL.
    CALL FUNCTION 'K_KOKRS_READ'
         EXPORTING
              KOKRS   = I_KOKRS
         IMPORTING
              E_TKA01 = TKA01.
  ENDIF.
  IF  I_BUDAT <> LDS_BUDAT
  OR  I_KOKRS <> TKA01-KOKRS.
    CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'
         EXPORTING
              I_DATE  = I_BUDAT
              I_PERIV = TKA01-LMONA
         IMPORTING
              E_BUPER = LDS_BUPER
              E_GJAHR = LDS_GJAHR
         EXCEPTIONS
              OTHERS  = 4.
    IF SY-SUBRC <> 0.
      MESSAGE ID   SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.
  PERFORM READ_MASTER_DATA
      TABLES   T_COIOB
      USING    I_OBJNR
               I_BUDAT
               LDS_BUPER
               LDS_GJAHR
               I_KOKRS
      CHANGING E_SUBRC
               E_TABIX.
  IF E_SUBRC = 0.
    PERFORM COIOB_READ
            TABLES   T_COIOB
            USING    I_OBJNR
                     I_BUDAT
            CHANGING E_COIOB
                     E_SUBRC
                     E_TABIX.
  ENDIF.
  LDS_BUDAT = I_BUDAT.
ENDFORM.                               "COIOB_FILL

*---------------------------------------------------------------------*
*       FORM READ_MASTER_DATA                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  T_COIOB                                                       *
*  -->  R_OBJNR                                                       *
*  -->  R_MASTER_DATE                                                 *
*  -->  R_PERIO                                                       *
*  -->  R_GJAHR                                                       *
*  -->  R_KOKRS                                                       *
*  -->  C_SUBRC                                                       *
*  -->  C_SAVE_TABIX                                                  *
*---------------------------------------------------------------------*
FORM READ_MASTER_DATA
          TABLES   T_COIOB         STRUCTURE COIOB
          USING    R_OBJNR         LIKE      COIOB-OBJNR
                   R_MASTER_DATE   LIKE      COBK-BUDAT
                   R_PERIO         LIKE      COEP-PERIO
                   R_GJAHR         LIKE      COEP-GJAHR
                   R_KOKRS         LIKE      COBK-KOKRS
          CHANGING C_SUBRC         LIKE      SY-SUBRC
                   C_SAVE_TABIX    LIKE      SY-TABIX.

  STATICS:
  LD_ENDYEAR    LIKE SY-DATUM,
  LT_COIOB      LIKE COIOB OCCURS 0 WITH HEADER LINE,
  LT_PERIODS    LIKE PERIODS OCCURS 0 WITH HEADER LINE.

* get last day of year:
  PERFORM LAST_DATE_OF_YEAR
           USING    R_KOKRS
                    R_GJAHR
           CHANGING LD_ENDYEAR.

* try two times to read master data
  DO 2 TIMES.

    REFRESH LT_COIOB.
    CLEAR   LT_COIOB.
    LT_COIOB-OBJNR = R_OBJNR.
    APPEND LT_COIOB.

    REFRESH LT_PERIODS.
    CLEAR   LT_PERIODS.
    LT_PERIODS-BUPER = R_PERIO.
    LT_PERIODS-DATAB = R_MASTER_DATE.
    IF SY-INDEX = 1.
*     first try: last day of fiscal year
      LT_PERIODS-DATBI = LD_ENDYEAR.
    ELSE.
*     sec. try: actual posting date
      LT_PERIODS-DATBI = R_MASTER_DATE.
    ENDIF.
    APPEND LT_PERIODS.

*   Stammdaten nachlesen
    CALL FUNCTION 'K_OBJECTS_MASTER_READ'
         TABLES
              COIOB_TAB = LT_COIOB
              I_PERIODS = LT_PERIODS
         EXCEPTIONS
              OTHERS    = 01.

    C_SUBRC = SY-SUBRC.
    READ TABLE LT_COIOB INDEX 1.                            "P40K007171

*   master data complete?
    IF ( C_SUBRC = 0 ) AND
       ( NOT LT_COIOB-KOKRS IS INITIAL ).
*      yes: exit loop:
      EXIT.
    ELSE.
      C_SUBRC = 4.
    ENDIF.

  ENDDO.

  IF ( C_SUBRC = 0 ).
*    Insert COIOB
    LOOP AT LT_COIOB.
      PERFORM COIOB_INSERT                                  "P40K007171
              TABLES   T_COIOB                              "P40K007171
              USING    LT_COIOB                             "P40K007171
              CHANGING C_SAVE_TABIX.                        "P40K007171
    ENDLOOP.

  ELSE.

    IF ( 1 = 2 ).                      "x-ref.
      MESSAGE E103(KI) WITH R_OBJNR LT_COIOB-OBART.
    ENDIF.

    MESSAGE ID 'KI' TYPE 'E' NUMBER '103'
            WITH  R_OBJNR  LT_COIOB-OBART.
*   no raise because tr. KALX calls also 'k_substitution_fkber'
*   In this FM no raise may occur; else all following coeps will get
*   no farea!
*   raise object_not_found.

  ENDIF.                               "IF c_subrc = 0.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM COIOB_INSERT                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  T_COIOB                                                       *
*  -->  R_COIOB                                                       *
*  -->  C_TABIX                                                       *
*---------------------------------------------------------------------*
FORM COIOB_INSERT                      "new P40K007171
     TABLES   T_COIOB STRUCTURE COIOB
     USING    R_COIOB STRUCTURE COIOB
     CHANGING C_TABIX LIKE SY-TABIX.
  READ TABLE T_COIOB WITH KEY
             OBJNR = R_COIOB-OBJNR
             DATBI = R_COIOB-DATBI
             BINARY SEARCH.

  C_TABIX = SY-TABIX.
  T_COIOB = R_COIOB.
  INSERT T_COIOB INDEX C_TABIX.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM LAST_DATE_OF_YEAR                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  R_KOKRS                                                       *
*  -->  R_GJAHR                                                       *
*  -->  C_ENDYEAR                                                     *
*---------------------------------------------------------------------*
FORM LAST_DATE_OF_YEAR
     USING    R_KOKRS   LIKE COEP-KOKRS
              R_GJAHR   LIKE COEP-GJAHR
     CHANGING C_ENDYEAR LIKE SY-DATUM.

  STATICS:
    LD_SAVE_GJAHR LIKE COEP-GJAHR,
    LD_SAVE_ENDYEAR LIKE SY-DATUM.

  IF  LD_SAVE_GJAHR = R_GJAHR
  AND TKA01-KOKRS   = R_KOKRS
  AND NOT LD_SAVE_ENDYEAR IS INITIAL.
    C_ENDYEAR = LD_SAVE_ENDYEAR.
    EXIT.
  ENDIF.

  IF R_KOKRS NE TKA01-KOKRS.
    CALL FUNCTION 'K_KOKRS_READ'
         EXPORTING
              KOKRS   = R_KOKRS
         IMPORTING
              E_TKA01 = TKA01.
  ENDIF.

* Bestimmung des letzten Tages des Gesch?tsjahres
  CALL FUNCTION 'G_POSTING_DATE_OF_YEAR_GET'
       EXPORTING
            VARIANT = TKA01-LMONA
            YEAR    = R_GJAHR
       IMPORTING
            TO_DATE = C_ENDYEAR.

  IF C_ENDYEAR IS INITIAL.
*   Perioden wie Kalenderjahr
    C_ENDYEAR(4) = R_GJAHR.
    C_ENDYEAR+4  = '1231'.
  ENDIF.
* Gesch?tsjahr merken
  LD_SAVE_GJAHR = R_GJAHR.
  LD_SAVE_ENDYEAR = C_ENDYEAR.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM DATE_PERIOD_CHECK                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM DATE_PERIOD_CHECK.

  DATA:
    LD_MONAT LIKE ACCIT-MONAT.

  LD_MONAT = P_PERIO.

  CALL FUNCTION 'FI_PERIOD_DETERMINE'
       EXPORTING
            I_BUDAT       = P_BUDAT
            I_PERIV       = TKA01-LMONA
*           I_GJAHR       = gd_newdoc-GJAHR
            I_MONAT       = LD_MONAT
       IMPORTING
            E_GJAHR       = GD_NEWDOC-GJAHR
            E_MONAT       = LD_MONAT.

  GD_NEWDOC-PERIO = LD_MONAT.
  GD_NEWDOC-BUDAT = P_BUDAT.

ENDFORM.                    " date_period_check
