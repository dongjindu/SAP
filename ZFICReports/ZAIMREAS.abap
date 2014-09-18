*$*$--------------------------------------------------------------$*$*
*$ Correction Inst.         0120024545 0000130763                     $*
*$------------------------------------------------------------------$*
*$ Valid for       :                                                  $*
*$ Software Component   SAP_APPL   SAP Application                    $*
*$  Release 40B          All Support Package Levels                   $*
*$  Release 45B          All Support Package Levels                   $*
*$  Release 46B          All Support Package Levels                   $*
*$  Release 46C          All Support Package Levels                   $*
*$  Release 470          w/o Support Packages                         $*
*$------------------------------------------------------------------$*
*$ Changes/Objects Not Contained in Standard SAP System               $*
*$*$--------------------------------------------------------------$*$*
*&-------------------------------------------------------------------*
*& Object          REPS ZAIMREAS
*& Object Header   PROG ZAIMREAS
*&-------------------------------------------------------------------*
*& REPORT ZAIMREAS
*&-------------------------------------------------------------------*
*>>>> START OF INSERTION <<<<
REPORT ZAIMREAS MESSAGE-ID 00
                LINE-SIZE 148.

TYPE-POOLS: SSCR.

TABLES: AUFK, PRPS, IMAK.

DATA: RESTRICT        TYPE SSCR_RESTRICT.
DATA: BEGIN OF        T_SELOP_NAME OCCURS 1,
        NAME(30)      TYPE C,
      END   OF        T_SELOP_NAME.

DATA: TAB_IMPR        LIKE IMPR     OCCURS 10 WITH HEADER LINE.

DATA: MSGPOS(40)      TYPE C,
      CON_DELETE(1)   TYPE C VALUE 'D',
      CON_INSERT(1)   TYPE C VALUE 'I',
      IM_OBARTS(10)   TYPE C,
      FLG_ERROR(1)    TYPE C,
      OBART           LIKE IMPS-OBART,
      SUBRC           LIKE SY-SUBRC,
      SUBRC_FROM      LIKE SY-SUBRC,
      SUBRC_TO        LIKE SY-SUBRC,
      TABIX           LIKE SY-TABIX.

DATA: XIMTP           LIKE IMTP     OCCURS 0 WITH HEADER LINE,
      XIMPR_FROM      LIKE IMPR     OCCURS 0 WITH HEADER LINE,
      XIMPR_TO        LIKE IMPR     OCCURS 0 WITH HEADER LINE,
      XIMZO_FROM      LIKE IMZO     OCCURS 0 WITH HEADER LINE,
      XIMZO_TO        LIKE IMZO     OCCURS 0 WITH HEADER LINE,
      XIMPS_TO        LIKE IMPS     OCCURS 0 WITH HEADER LINE,
      XOBJ            LIKE RAIP_OBJ OCCURS 0 WITH HEADER LINE,
      UPD_IMZO        LIKE RIMZO    OCCURS 0 WITH HEADER LINE.
PARAMETERS: PROGRAM  LIKE IMTP-PRNAM MEMORY ID IMT.
SELECTION-SCREEN BEGIN OF BLOCK FT.
PARAMETERS: FROM_POS LIKE IMPR-POSID MEMORY ID IMP,
            TO_POS   LIKE IMPR-POSID MEMORY ID IMP.
SELECTION-SCREEN END   OF BLOCK FT.
SELECTION-SCREEN SKIP 1.
PARAMETERS: X_ORDER  AS CHECKBOX DEFAULT 'X'.
SELECT-OPTIONS: ORDER FOR AUFK-AUFNR.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK PRO.
PARAMETERS: X_WBSEL AS CHECKBOX DEFAULT 'X'.
SELECT-OPTIONS: PROJECT FOR PRPS-PSPHI NO INTERVALS.
SELECT-OPTIONS: WBSEL   FOR PRPS-PSPNR NO INTERVALS.
SELECTION-SCREEN END   OF BLOCK PRO.
SELECTION-SCREEN SKIP 1.
PARAMETERS: X_APPREQ AS CHECKBOX DEFAULT 'X'.
SELECT-OPTIONS: APPREQ FOR IMAK-POSNR.
RANGES: OBJECT FOR IMZO-OBJNR.
SELECTION-SCREEN SKIP 1.
PARAMETERS: TEST_RUN AS CHECKBOX DEFAULT 'X'.


INITIALIZATION.

CLEAR T_SELOP_NAME.
T_SELOP_NAME-NAME = 'WBSEL'.
APPEND T_SELOP_NAME.
CLEAR T_SELOP_NAME.
T_SELOP_NAME-NAME = 'PROJECT'.
APPEND T_SELOP_NAME.
PERFORM AIPP_GET_SELOP_RESTRICTIONS
  TABLES
    T_SELOP_NAME
  CHANGING
    RESTRICT.
CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
     EXPORTING
          RESTRICTION   = RESTRICT
     EXCEPTIONS
          OTHERS        = 1.


AT SELECTION-SCREEN ON PROGRAM.

IF PROGRAM IS INITIAL.
   MESSAGE E208 WITH
     'Indicate prgogram definition'. "#EC NOTEXT
ENDIF.
SELECT COUNT(*) FROM IMTP
  WHERE PRNAM = PROGRAM.
IF SY-DBCNT = 0.
   MESSAGE E208 WITH
     'Prgogram definition not found'. "#EC NOTEXT
ENDIF.


AT SELECTION-SCREEN ON FROM_POS.
IF FROM_POS IS INITIAL.
   MESSAGE E208 WITH
     'Indicate from-position'. "#EC NOTEXT
ENDIF.
*
SELECT COUNT(*) FROM IMPR
  WHERE PRNAM = PROGRAM
  AND   POSID = FROM_POS.
IF SY-DBCNT = 0.
   MESSAGE E208 WITH
     'From-position not found'. "#EC NOTEXT
ENDIF.
*
REFRESH TAB_IMPR.
SELECT * FROM IMPR
  INTO TABLE TAB_IMPR
  WHERE PRNAM = PROGRAM
  AND   POSID = FROM_POS.
*
LOOP AT TAB_IMPR.
*
  PERFORM GET_POSITION USING    TAB_IMPR
                       CHANGING MSGPOS.
*
  SELECT COUNT(*) FROM IMPR
    WHERE PARNR = TAB_IMPR-POSNR.
  IF SY-DBCNT > 0.
     MESSAGE I162 WITH
       'The following error occured for' MSGPOS. "#EC NOTEXT
     MESSAGE E208 WITH
       'From-position is not a leaf'. "#EC NOTEXT
*
     EXIT.
  ENDIF.
*
  IF TAB_IMPR-XAKTB <>  SPACE.
     MESSAGE I162 WITH
       'The following error occured for' MSGPOS. "#EC NOTEXT
     MESSAGE E208 WITH
       'budget distribution indicator is active'. "#EC NOTEXT
*
     EXIT.
  ENDIF.
*
ENDLOOP.


AT SELECTION-SCREEN ON TO_POS.

IF TO_POS IS INITIAL.
   MESSAGE E208 WITH
     'Indicate to-position'. "#EC NOTEXT
ENDIF.
*
SELECT COUNT(*) FROM IMPR
  WHERE PRNAM = PROGRAM
  AND   POSID = TO_POS.
IF SY-DBCNT = 0.
   MESSAGE E208 WITH
     'To-position not found'. "#EC NOTEXT
ENDIF.
*
REFRESH TAB_IMPR.
SELECT * FROM IMPR
  INTO TABLE TAB_IMPR
  WHERE PRNAM = PROGRAM
  AND   POSID = TO_POS.
*
LOOP AT TAB_IMPR.
*
  PERFORM GET_POSITION USING    TAB_IMPR
                       CHANGING MSGPOS.
*
  SELECT COUNT(*) FROM IMPR
    WHERE PARNR = TAB_IMPR-POSNR.
  IF SY-DBCNT > 0.
     MESSAGE I162 WITH
       'The following error occured for' MSGPOS. "#EC NOTEXT
     MESSAGE E208 WITH
       'To-position is not a leaf'. "#EC NOTEXT
*
     EXIT.
  ENDIF.
*
  IF TAB_IMPR-XAKTB <>  SPACE.
     MESSAGE I162 WITH
       'The following error occured for' MSGPOS. "#EC NOTEXT
     MESSAGE E208 WITH
       'Budget distribution indicator is active'. "#EC NOTEXT
*
     EXIT.
  ENDIF.
*
ENDLOOP.


AT SELECTION-SCREEN ON BLOCK FT.

IF FROM_POS = TO_POS.
   MESSAGE E208 WITH
     'From-position = to-position'. "#EC NOTEXT
ENDIF.


AT SELECTION-SCREEN ON BLOCK PRO.

IF     X_WBSEL   <> SPACE   AND
   NOT WBSEL[]   IS INITIAL AND
   NOT PROJECT[] IS INITIAL .
   MESSAGE E208 WITH
     'Select either projects or WBS-elements'. "#EC NOTEXT
ENDIF.


START-OF-SELECTION.
* Nothing to do?
IF X_ORDER  IS INITIAL AND
   X_WBSEL  IS INITIAL AND
   X_APPREQ IS INITIAL .
   MESSAGE I208 WITH
     'Please select measures/app.requests'. "#EC NOTEXT
   EXIT.
ENDIF.

* Select all IMTP-, IMPR-records needed.
SELECT * FROM IMTP INTO TABLE XIMTP
  WHERE PRNAM = PROGRAM.
SELECT * FROM IMPR INTO TABLE XIMPR_FROM
  WHERE PRNAM = PROGRAM
  AND   POSID = FROM_POS.
SELECT * FROM IMPR INTO TABLE XIMPR_TO
  WHERE PRNAM = PROGRAM
  AND   POSID = TO_POS.

* production run ...
IF TEST_RUN = SPACE.
*  ... enqueue all CIPs involved.
   LOOP AT XIMTP.
     CALL FUNCTION 'AIPE_ENQUEUE_INVPROG'
          EXPORTING
               I_PRNAM        = XIMTP-PRNAM
          "    I_POSID        =
               I_GJAHR        = XIMTP-GJAHR
               I_FLG_SHARED   = ' '
               I_MSG_TYPE     = 'E'
          EXCEPTIONS
               FOREIGN_LOCK   = 1
               SYSTEM_FAILURE = 2
               OTHERS         = 3.
     IF SY-SUBRC <> 0.
       MESSAGE ID SY-MSGID TYPE 'E' NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
     ENDIF.
   ENDLOOP.
ENDIF.

* What object types are requested?
CLEAR IM_OBARTS.
IF X_ORDER <> SPACE.
   MOVE 'OR' TO IM_OBARTS+0(2).
ENDIF.
IF X_WBSEL <> SPACE.
   MOVE 'PR' TO IM_OBARTS+2(2).
ENDIF.
IF X_APPREQ <> SPACE.
   MOVE 'IQ' TO IM_OBARTS+4(2).
ENDIF.

* Transform ranges into object number range.
PERFORM AIP1_CREATE_SEL_OBJNR
         TABLES ORDER
                PROJECT
                WBSEL
                APPREQ
                OBJECT
         USING IM_OBARTS.

* Get all IMZO records to from-positions.
SELECT * FROM IMZO INTO TABLE XIMZO_FROM
  FOR ALL ENTRIES IN XIMPR_FROM
  WHERE         POSNR =  XIMPR_FROM-POSNR
  AND           OBJNR IN OBJECT.
* AND   (       LOGDL IS NULL      OR
*         ( NOT LOGDL IS NULL AND
*               LOGDL <> 'A'  AND
*               LOGDL <> 'R'  )    ).
* Determine all CO objects to be reassigned.
LOOP AT XIMZO_FROM.
  CLEAR XOBJ.
  XOBJ-OBJNR = XIMZO_FROM-OBJNR.
  COLLECT XOBJ.
ENDLOOP.

* Get all IMZO records to to-positions.
SELECT * FROM IMZO INTO TABLE XIMZO_TO
  FOR ALL ENTRIES IN XIMPR_TO
  WHERE         POSNR =  XIMPR_TO-POSNR
  AND           OBJNR IN OBJECT.
* AND   (       LOGDL IS NULL      OR
*         ( NOT LOGDL IS NULL AND
*               LOGDL <> 'A'  AND
*               LOGDL <> 'R'  )    ).

* Get all IMPS records to to-positions.
SELECT * FROM IMPS INTO TABLE XIMPS_TO
  FOR ALL ENTRIES IN XIMPR_TO
  WHERE         POSNR =  XIMPR_TO-POSNR.

* Convenient sorts for binary search.
SORT XOBJ       BY OBJNR.
SORT XIMTP      BY PRNAM GJAHR.
SORT XIMPR_FROM BY PRNAM GJAHR.
SORT XIMPR_TO   BY PRNAM GJAHR.
SORT XIMZO_FROM BY OBJNR POSNR IPPOS.
SORT XIMZO_TO   BY OBJNR POSNR IPPOS.
SORT XIMPS_TO   BY POSNR OBART.
*
REFRESH UPD_IMZO.

* Process each object.
LOOP AT XOBJ.

* Assumption: Reassignment is possible.
  FLG_ERROR = SPACE.
* Determine object type.
  CALL FUNCTION 'OBJECT_NUMBER_TYPE_GET'
       EXPORTING
            OBJNR     = XOBJ-OBJNR
       IMPORTING
            OBART     = OBART.
* Process each approval year.
  LOOP AT XIMTP.
*   Determine IMPR record of from-position.
    READ TABLE XIMPR_FROM
         WITH KEY PRNAM = XIMTP-PRNAM
                  GJAHR = XIMTP-GJAHR
         BINARY SEARCH.
    SUBRC_FROM = SY-SUBRC.
*   Determine IMPR record of to-position.
    READ TABLE XIMPR_TO
         WITH KEY PRNAM = XIMTP-PRNAM
                  GJAHR = XIMTP-GJAHR
         BINARY SEARCH.
    SUBRC_TO = SY-SUBRC.
*   Error: from-position w/o to-position.
    IF SUBRC_FROM =  0 AND
       SUBRC_TO   <> 0 .
       READ TABLE XIMZO_FROM
            WITH KEY OBJNR = XOBJ-OBJNR
                     POSNR = XIMPR_FROM-POSNR
            BINARY SEARCH.
       IF SY-SUBRC = 0.
          XIMPR_TO = XIMPR_FROM.
          XIMPR_TO-POSID = TO_POS.
          PERFORM PROT USING 'E' 2
                             XIMPR_FROM
                             XIMPR_TO
                             XOBJ-OBJNR.
          FLG_ERROR = 'X'.
       ENDIF.
    ENDIF.
*   Error: to-position w/o from-position.
    IF SUBRC_FROM <> 0 AND
       SUBRC_TO   =  0 .
       READ TABLE XIMZO_TO
            WITH KEY OBJNR = XOBJ-OBJNR
                     POSNR = XIMPR_TO-POSNR
            BINARY SEARCH.
       IF SY-SUBRC = 0.
          XIMPR_FROM = XIMPR_TO.
          XIMPR_FROM-POSID = FROM_POS.
          PERFORM PROT USING 'E' 3
                             XIMPR_FROM
                             XIMPR_TO
                             XOBJ-OBJNR.
          FLG_ERROR = 'X'.
       ENDIF.
    ENDIF.
*   Further processing only if from-/to-position both exist.
    CHECK SUBRC_FROM = 0 AND
          SUBRC_TO   = 0 .
*   Warning: recieving position does not allow
*   assignments of the current object type.
    READ TABLE XIMPS_TO
         WITH KEY POSNR = XIMPR_TO-POSNR
                  OBART = OBART
         BINARY SEARCH.
    IF SY-SUBRC <> 0.
       PERFORM PROT USING 'W' 4
                          XIMPR_FROM
                          XIMPR_TO
                          XOBJ-OBJNR.
       FLG_ERROR = 'X'.
    ENDIF.
*   Error: measure/request already assigned to to-position
*   (only possible if assignments are weighted by
*   percentage rates).
    READ TABLE XIMZO_FROM
         WITH KEY OBJNR = XOBJ-OBJNR
                  POSNR = XIMPR_FROM-POSNR
         BINARY SEARCH.
    SUBRC = SY-SUBRC.
    TABIX = SY-TABIX.
    IF SUBRC = 0.
       LOOP AT XIMZO_FROM
         FROM TABIX.
         IF XIMZO_FROM-OBJNR <> XOBJ-OBJNR       OR
            XIMZO_FROM-POSNR <> XIMPR_FROM-POSNR .
            EXIT.
         ENDIF.
         READ TABLE XIMZO_TO
              WITH KEY OBJNR = XIMZO_FROM-OBJNR
                       POSNR = XIMPR_TO-POSNR
                       IPPOS = XIMZO_FROM-IPPOS
              BINARY SEARCH.
         IF SY-SUBRC = 0.
            PERFORM PROT USING 'E' 1
                               XIMPR_FROM
                               XIMPR_TO
                               XOBJ-OBJNR.
            FLG_ERROR = 'X'.
         ENDIF.
       ENDLOOP.
    ENDIF.
  ENDLOOP.

* Reassignment possible?
  CHECK FLG_ERROR = SPACE.

  LOOP AT XIMTP.
*   Determine IMPR record of from-position.
    READ TABLE XIMPR_FROM
         WITH KEY PRNAM = XIMTP-PRNAM
                  GJAHR = XIMTP-GJAHR
         BINARY SEARCH.
    SUBRC_FROM = SY-SUBRC.
*   Determine IMPR record of to-position.
    READ TABLE XIMPR_TO
         WITH KEY PRNAM = XIMTP-PRNAM
                  GJAHR = XIMTP-GJAHR
         BINARY SEARCH.
    SUBRC_TO   = SY-SUBRC.
*   Further processing only if from-/to-position both exist.
    CHECK SUBRC_FROM = 0 AND
          SUBRC_TO   = 0 .
*
    READ TABLE XIMZO_FROM
         WITH KEY OBJNR = XOBJ-OBJNR
                  POSNR = XIMPR_FROM-POSNR
         BINARY SEARCH.
    SUBRC = SY-SUBRC.
    TABIX = SY-TABIX.
    IF SUBRC = 0.
       LOOP AT XIMZO_FROM
         FROM TABIX.
         IF XIMZO_FROM-OBJNR <> XOBJ-OBJNR       OR
            XIMZO_FROM-POSNR <> XIMPR_FROM-POSNR .
            EXIT.
         ENDIF.
*        IMZO record to be deleted.
         CLEAR UPD_IMZO.
         MOVE-CORRESPONDING XIMZO_FROM TO UPD_IMZO.
         UPD_IMZO-UPDKZ = CON_DELETE.
         APPEND UPD_IMZO.
*        IMZO record to be inserted.
         CLEAR UPD_IMZO.
         MOVE-CORRESPONDING XIMZO_FROM TO UPD_IMZO.
         UPD_IMZO-POSNR = XIMPR_TO-POSNR.
         UPD_IMZO-UPDKZ = CON_INSERT.
         APPEND UPD_IMZO.
       ENDLOOP.
       PERFORM PROT USING 'S' 0
                          XIMPR_FROM
                          XIMPR_TO
                          XOBJ-OBJNR.
    ENDIF.
  ENDLOOP.

ENDLOOP.

* Update database.
IF NOT UPD_IMZO[] IS INITIAL AND
       TEST_RUN   =  SPACE   .
   SORT UPD_IMZO BY UPDKZ OBJNR POSNR IPPOS.
   CALL FUNCTION 'AIPU_POSITION_PREPARE_UPDATE'
        TABLES
             T_UPD_IMZO   = UPD_IMZO.
   CALL FUNCTION 'AIPU_POSITION_UPDATE'
        TABLES
             T_UPD_IMZO   = UPD_IMZO.
   COMMIT WORK.
ENDIF.

END-OF-SELECTION.


*---------------------------------------------------------------------*
FORM AIPP_GET_SELOP_RESTRICTIONS
  TABLES
    IT_SELOP_NAME
  CHANGING
    E_RESTRICT TYPE SSCR_RESTRICT.

  DATA: L_OPT_LIST       TYPE SSCR_OPT_LIST,
        L_ASS            TYPE SSCR_ASS.

* Dem Funktionsbaustein wird der Name von einem oder mehreren
* Select-Options mitgegeben. Der Funktionsbaustein erzeugt
* das Datenobjekt E_RESTRICT, das dann hinter der aufrufenden
* Stelle dem Funktionsbaustein SELECT_OPTIONS_RESTRICT
* mitgegeben wird, um f? die Select-Options aus T_SELOP_NAME
* nur Einzelwerte zuzulassen.
* Zum Aufbau des Ausgabeparameters E_RESTRICT:
* Siehe Doku des Funktionsbausteins SELECT_OPTIONS_RESTRICT.

* Optionenliste 'Nur Einzelwerte'.
  CLEAR L_OPT_LIST.
  L_OPT_LIST-NAME         = 'NOINTERVLS'.
  L_OPT_LIST-OPTIONS-EQ   = 'X'.
  APPEND L_OPT_LIST TO E_RESTRICT-OPT_LIST_TAB.

* Festlegungen f? Select-Options.
  LOOP AT IT_SELOP_NAME.
    CLEAR L_ASS.
*   Festlegung gilt f? einzelne Select-Option.
    L_ASS-KIND    = 'S'.
    L_ASS-NAME    = IT_SELOP_NAME.
*   Erlaubtes Sign im Startbild.
    L_ASS-SG_MAIN = 'I'.
*   Zus?zlich erlaubtes Sign im Bild
*   f? Mehrfachselektion: Kein zus?zliches
*   Sign erlaubt.
    L_ASS-SG_ADDY = ' '.
*   Erlaubte Optionen Startbild.
    L_ASS-OP_MAIN = 'NOINTERVLS'.
*   Erlaubte Optionen Mehrfachselektion.
    L_ASS-OP_ADDY = 'NOINTERVLS'.
*
    APPEND L_ASS TO E_RESTRICT-ASS_TAB.
  ENDLOOP.

ENDFORM.


FORM GET_POSITION USING VALUE(I_IMPR) LIKE IMPR
                  CHANGING E_MSGPOS   LIKE MSGPOS.

  CLEAR E_MSGPOS.
  WRITE: I_IMPR-PRNAM TO E_MSGPOS+00(08),
         '/'          TO E_MSGPOS+09(01),
         I_IMPR-GJAHR TO E_MSGPOS+11(04).
  CONDENSE E_MSGPOS NO-GAPS.
  WRITE: '-'          TO E_MSGPOS+16(01),
         I_IMPR-POSID TO E_MSGPOS+18(24).
  CONDENSE E_MSGPOS.

ENDFORM.

FORM AIP1_CREATE_SEL_OBJNR
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
         TABLES
                IT_SEL_AUFNR STRUCTURE ORDER
                IT_SEL_PSPHI STRUCTURE PROJECT
                IT_SEL_PSPNR STRUCTURE WBSEL
                IT_SEL_POSNR STRUCTURE APPREQ
                ET_SEL_OBJNR STRUCTURE OBJECT
         USING
               VALUE(I_OBARTS) TYPE  C.
*"----------------------------------------------------------------------

  RANGES: T_SEL_OBJNR FOR IMZO-OBJNR,
          T_SEL_AUFNR FOR AUFK-AUFNR,
          T_SEL_PSPHI FOR PRPS-PSPHI,
          T_SEL_PSPNR FOR PRPS-PSPNR,
          T_SEL_POSNR FOR IMAK-POSNR.

  DATA: BEGIN OF T_PSP OCCURS 100,
          PSPNR       LIKE PRPS-PSPNR,
          OBJNR       LIKE PRPS-OBJNR,
          XLOW(1)     TYPE C,
          XHIGH(1)    TYPE C,
        END   OF T_PSP.

  DATA: T_PSP_TMP     LIKE T_PSP OCCURS 100 WITH HEADER LINE.

  DATA: L_PSPHI       LIKE PRPS-PSPHI,
        L_PSPNR       LIKE PRPS-PSPNR,
        L_OBARTS(10)  TYPE C,
        L_TABIX       LIKE SY-TABIX,
        L_TABIX_PSP   LIKE SY-TABIX,
        L_TFILL       LIKE SY-TFILL.

*       L_SUBRC_1     LIKE SY-SUBRC,
*       L_SUBRC_2     LIKE SY-SUBRC.

* ?ergabe unstrukturierte Daten --> Strukturierte Daten.
  L_OBARTS      = I_OBARTS.
  T_SEL_AUFNR[] = IT_SEL_AUFNR[].
  T_SEL_PSPHI[] = IT_SEL_PSPHI[].
  T_SEL_PSPNR[] = IT_SEL_PSPNR[].
  T_SEL_POSNR[] = IT_SEL_POSNR[].

* Default: Alle Ma?ahmen/Manfen zum Programm selektieren.
  REFRESH: T_SEL_OBJNR.

* Auftr?e sollen selektiert werden.
  IF L_OBARTS+0(2) = 'OR' OR
     L_OBARTS+2(2) = 'OR' OR
     L_OBARTS+4(2) = 'OR' OR
     L_OBARTS+6(2) = 'OR' OR
     L_OBARTS+8(2) = 'OR' .
*    Bestimmte Auftr?e selektieren.
     IF NOT T_SEL_AUFNR[] IS INITIAL.
        LOOP AT T_SEL_AUFNR.
          CLEAR: T_SEL_OBJNR.
          T_SEL_OBJNR-SIGN       = T_SEL_AUFNR-SIGN.
          T_SEL_OBJNR-OPTION     = T_SEL_AUFNR-OPTION.
          IF NOT T_SEL_AUFNR-LOW  IS INITIAL.
             T_SEL_OBJNR-LOW(2)     = 'OR'.
             T_SEL_OBJNR-LOW+2(12)  = T_SEL_AUFNR-LOW.
          ENDIF.
          IF NOT T_SEL_AUFNR-HIGH IS INITIAL.
             T_SEL_OBJNR-HIGH(2)    = 'OR'.
             T_SEL_OBJNR-HIGH+2(12) = T_SEL_AUFNR-HIGH.
          ENDIF.
          APPEND T_SEL_OBJNR.
        ENDLOOP.
*    Alle Auftr?e selektieren.
     ELSE.
        CLEAR T_SEL_OBJNR.
        T_SEL_OBJNR-LOW    = 'OR*'.
        T_SEL_OBJNR-SIGN   = 'I'.    " Include.
        T_SEL_OBJNR-OPTION = 'CP'.   " Compare Pattern.
        APPEND T_SEL_OBJNR.
     ENDIF.
* Auftr?e sollen nicht selektiert werden.
  ELSE.
     CLEAR T_SEL_OBJNR.
     T_SEL_OBJNR-LOW    = 'OR*'.
     T_SEL_OBJNR-SIGN   = 'E'.       " Exclude.
     T_SEL_OBJNR-OPTION = 'CP'.      " Compare Pattern.
     APPEND T_SEL_OBJNR.
  ENDIF.

* PSP-Elemente sollen selektiert werden.
  IF L_OBARTS+0(2) = 'PR' OR
     L_OBARTS+2(2) = 'PR' OR
     L_OBARTS+4(2) = 'PR' OR
     L_OBARTS+6(2) = 'PR' OR
     L_OBARTS+8(2) = 'PR' .
*    Bestimmte PSP-Elemente selektieren.
     IF NOT T_SEL_PSPHI[] IS INITIAL OR
        NOT T_SEL_PSPNR[] IS INITIAL .
*       PSP-Elemente lesen
*       o  entweder gem? Abgrenzung Projekte
*       o  oder     gem? Abgrenzung PSP-Elemente.
        REFRESH T_PSP.
        IF     NOT T_SEL_PSPHI[] IS INITIAL.
               SELECT DISTINCT PSPNR OBJNR FROM PRPS
                 INTO CORRESPONDING FIELDS OF TABLE T_PSP
                 WHERE PSPHI IN T_SEL_PSPHI.
        ELSEIF NOT T_SEL_PSPNR[] IS INITIAL.
               SELECT DISTINCT PSPNR OBJNR FROM PRPS
                 INTO CORRESPONDING FIELDS OF TABLE T_PSP
                 WHERE PSPNR IN T_SEL_PSPNR.
        ENDIF.
*       PSP-Elemente im Selektionsbereich.
        IF NOT T_PSP[] IS INITIAL.
*          Aus Liste T_PSP der PSP-Elemente optimal
*          kleinen Range basteln.
*          Grundlage dieses Spiels: Innerhal eines PSP
*          kommen die internen PSP-Nummern meist
*          hintereinander weg!
           SORT T_PSP BY PSPNR.
*          Feststellen, wo Nummernintervalle
*          innerhalb von T_PSP beginnen/enden.
*
*          Erste Nummer ist auf jeden Fall ein
*          Intervall-Beginn.
           READ TABLE T_PSP INDEX 1.
           T_PSP-XLOW = 'X'.
           MODIFY T_PSP INDEX 1.
*          Letzte Nummer ist auf jeden Fall ein
*          Intervall-Ende.
           DESCRIBE TABLE T_PSP LINES L_TFILL.
           READ TABLE T_PSP INDEX L_TFILL.
           T_PSP-XHIGH = 'X'.
           MODIFY T_PSP INDEX L_TFILL.
*
           REFRESH T_PSP_TMP.
           T_PSP_TMP[] = T_PSP[].
           LOOP AT T_PSP.
             L_TABIX_PSP = SY-TABIX.
*            Weitere Intervall-Beginne feststellen.
             IF L_TABIX_PSP > 1.
*               Lies Nummer der vorigen Zeile von T_PSP
                L_TABIX = L_TABIX_PSP - 1.
                READ TABLE T_PSP_TMP INDEX L_TABIX.
*               Abstand der Nummern ...
                L_PSPNR = T_PSP-PSPNR - T_PSP_TMP-PSPNR.
*               ... mehr als 1 ...
                IF L_PSPNR > 1.
*                  ... dann f?gt hier
*                  ein neues Intervall an.
                   T_PSP-XLOW = 'X'.
                   MODIFY T_PSP.
                ENDIF.
             ENDIF.
*            Weitere Intervall-Enden feststellen.
             IF L_TABIX_PSP < L_TFILL.
*               Lies Nummer der n?hsten Zeile von T_PSP
                L_TABIX = L_TABIX_PSP + 1.
                READ TABLE T_PSP_TMP INDEX L_TABIX.
*               Abstand der Nummern ...
                L_PSPNR = T_PSP_TMP-PSPNR - T_PSP-PSPNR.
*               ... mehr als 1 ...
                IF L_PSPNR > 1.
*                  ... dann endet hier
*                  ein Intervall.
                   T_PSP-XHIGH = 'X'.
                   MODIFY T_PSP.
                ENDIF.
             ENDIF.
           ENDLOOP.
*          Range bilden.
           LOOP AT T_PSP
             WHERE ( XLOW  <> SPACE OR
                     XHIGH <> SPACE ).
*            Intervallbeginn ...
             IF T_PSP-XLOW <> SPACE.
*               ... dann auf jeden Fall neue Zeile
*               in Objektnummernrange.
                CLEAR T_SEL_OBJNR.
                T_SEL_OBJNR-SIGN = 'I'.
                T_SEL_OBJNR-LOW  = T_PSP-OBJNR.
             ENDIF.
*            Intervallbeginn = Intervallende ==>
*            Einzelwert in Range aufnehmen.
             IF     T_PSP-XLOW  <> SPACE AND
                    T_PSP-XHIGH <> SPACE .
                    T_SEL_OBJNR-OPTION = 'EQ'.
                    APPEND T_SEL_OBJNR.
*            Intervallbeginn <> Intervallende ==>
*            Intervall in Range aufnehmen.
             ELSEIF T_PSP-XLOW  =  SPACE AND
                    T_PSP-XHIGH <> SPACE .
                    T_SEL_OBJNR-HIGH   = T_PSP-OBJNR.
                    T_SEL_OBJNR-OPTION = 'BT'.
                    APPEND T_SEL_OBJNR.
             ENDIF.
           ENDLOOP.
*       Keine PSP-Elemente im Selektionsbereich.
        ELSE.
           CLEAR T_SEL_OBJNR.
           T_SEL_OBJNR-LOW    = 'PR*'.
           T_SEL_OBJNR-SIGN   = 'E'.    " Exclude.
           T_SEL_OBJNR-OPTION = 'CP'.   " Compare Pattern.
           APPEND T_SEL_OBJNR.
        ENDIF.
*    Alle PSP-Elemente selektieren.
     ELSE.
        CLEAR T_SEL_OBJNR.
        T_SEL_OBJNR-LOW    = 'PR*'.
        T_SEL_OBJNR-SIGN   = 'I'.    " Include.
        T_SEL_OBJNR-OPTION = 'CP'.   " Compare Pattern.
        APPEND T_SEL_OBJNR.
     ENDIF.
* PSP-Elemente sollen nicht selektiert werden.
  ELSE.
     CLEAR T_SEL_OBJNR.
     T_SEL_OBJNR-LOW    = 'PR*'.
     T_SEL_OBJNR-SIGN   = 'E'.       " Exclude.
     T_SEL_OBJNR-OPTION = 'CP'.      " Compare Pattern.
     APPEND T_SEL_OBJNR.
  ENDIF.

* Manfen sollen selektiert werden.
  IF L_OBARTS+0(2) = 'IQ' OR
     L_OBARTS+2(2) = 'IQ' OR
     L_OBARTS+4(2) = 'IQ' OR
     L_OBARTS+6(2) = 'IQ' OR
     L_OBARTS+8(2) = 'IQ' .
*    Bestimmte Manfen selektieren.
     IF NOT T_SEL_POSNR[] IS INITIAL.
        LOOP AT T_SEL_POSNR.
          CLEAR: T_SEL_OBJNR.
          T_SEL_OBJNR-SIGN       = T_SEL_POSNR-SIGN.
          T_SEL_OBJNR-OPTION     = T_SEL_POSNR-OPTION.
          IF NOT T_SEL_POSNR-LOW  IS INITIAL.
             T_SEL_OBJNR-LOW(2)     = 'IQ'.
             T_SEL_OBJNR-LOW+2(12)  = T_SEL_POSNR-LOW.
          ENDIF.
          IF NOT T_SEL_POSNR-HIGH IS INITIAL.
             T_SEL_OBJNR-HIGH(2)    = 'IQ'.
             T_SEL_OBJNR-HIGH+2(12) = T_SEL_POSNR-HIGH.
          ENDIF.
          APPEND T_SEL_OBJNR.
        ENDLOOP.
*    Alle Manfen selektieren.
     ELSE.
        CLEAR T_SEL_OBJNR.
        T_SEL_OBJNR-LOW    = 'IQ*'.
        T_SEL_OBJNR-SIGN   = 'I'.    " Include.
        T_SEL_OBJNR-OPTION = 'CP'.   " Compare Pattern.
        APPEND T_SEL_OBJNR.
     ENDIF.
* Manfen sollen nicht selektiert werden.
  ELSE.
     CLEAR T_SEL_OBJNR.
     T_SEL_OBJNR-LOW    = 'IQ*'.
     T_SEL_OBJNR-SIGN   = 'E'.       " Exclude.
     T_SEL_OBJNR-OPTION = 'CP'.      " Compare Pattern.
     APPEND T_SEL_OBJNR.
  ENDIF.



* ?ergabe strukturierte Tabelle --> unstrukturierte Tabelle.
  ET_SEL_OBJNR[] = T_SEL_OBJNR[].

ENDFORM.


FORM PROT USING
  VALUE(I_MSG_TYPE)  LIKE SY-MSGTY
  VALUE(I_MSG_CODE)  TYPE I
  VALUE(I_IMPR_FROM) LIKE IMPR
  VALUE(I_IMPR_TO)   LIKE IMPR
  VALUE(I_OBJNR)     LIKE IMZO-OBJNR.

  DATA: BEGIN OF L_OBJECT,
          TYPE(12)     TYPE C,
          SEPARATOR(1) TYPE C,
          ID(24)       TYPE C,
        END   OF L_OBJECT.

  DATA: L_OBART LIKE IMPS-OBART,
        L_AUFNR LIKE AUFKV-AUFNR,
        L_PSPNR LIKE PRPS-PSPNR,
        L_POSNR LIKE IMAK-POSNR.

  DATA: L_TEXT1(120) TYPE C VALUE                         "#EC NOTEXT
        'Reassignment from &V1& to &V2& not possible :',  "#EC NOTEXT
        L_TEXT2(120) TYPE C.

  CASE I_MSG_TYPE.
  WHEN 'S'.
    CLEAR L_TEXT1.
    L_TEXT1 =                           "#EC NOTEXT
    'Reassignment from &V1& to &V2& :'. "#EC NOTEXT
    FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
  WHEN 'W'.
    FORMAT COLOR COL_TOTAL    INTENSIFIED OFF.
  WHEN 'E'.
    FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
  ENDCASE.

  CALL FUNCTION 'OBJECT_NUMBER_TYPE_GET'
       EXPORTING
            OBJNR     = I_OBJNR
       IMPORTING
            OBART     = L_OBART.

  CASE L_OBART.
  WHEN 'OR'.
    CALL FUNCTION 'OBJECT_KEY_GET_OR'
         EXPORTING
              OBJNR       = I_OBJNR
         IMPORTING
              AUFNR       = L_AUFNR.
    WRITE : 'order'       TO L_OBJECT-TYPE, "#EC NOTEXT
            L_AUFNR       TO L_OBJECT-ID.
  WHEN 'PR'.
    CALL FUNCTION 'OBJECT_KEY_GET_PR'
         EXPORTING
              OBJNR       = I_OBJNR
         IMPORTING
              PSPNR       = L_PSPNR.
    WRITE : 'WBS-element' TO L_OBJECT-TYPE, "#EC NOTEXT
            L_PSPNR       TO L_OBJECT-ID.
  WHEN 'IQ'.
    CALL FUNCTION 'OBJECT_KEY_GET_IQ'
         EXPORTING
              OBJNR       = I_OBJNR
         IMPORTING
              IMA_POSNR   = L_POSNR.
    WRITE : 'app.request' TO L_OBJECT-TYPE, "#EC NOTEXT
            L_POSNR       TO L_OBJECT-ID.
  ENDCASE.

  PERFORM GET_POSITION USING    I_IMPR_FROM
                       CHANGING MSGPOS.
  REPLACE '&V1&' WITH MSGPOS INTO L_TEXT1.
  PERFORM GET_POSITION USING    I_IMPR_TO
                       CHANGING MSGPOS.
  REPLACE '&V2&' WITH MSGPOS INTO L_TEXT1.
  CONDENSE L_TEXT1.

  CASE I_MSG_CODE.
  WHEN 0.
    IF TEST_RUN <> SPACE.
       L_TEXT2 =
       'Reassignment is possible.'. "#EC NOTEXT
    ELSE.
       L_TEXT2 =
       'Reassignment successfully completed.'. "#EC NOTEXT
    ENDIF.
  WHEN 1.
    L_TEXT2 =
    'Assignment to to-position already exists.'. "#EC NOTEXT
  WHEN 2.
    L_TEXT2 =
    'From-position exists but to-position does not exist.'. "#EC NOTEXT
  WHEN 3.
    L_TEXT2 =
    'To-position exists but from-position does not exist.'. "#EC NOTEXT
  WHEN 4.
    L_TEXT2 =
    'To-position does not allow assignments of that type'. "#EC NOTEXT
  ENDCASE.

  WRITE: / L_OBJECT, (110) L_TEXT1,
         /39(110) L_TEXT2.

  FORMAT RESET.

ENDFORM.
*>>>> END OF INSERTION <<<<<<
