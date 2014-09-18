************************************************************************
* Program Name      : ZEPP323U_PLANNING_PROFILE
* Author            : JongOh, Kim
* Creation Date     : 2003.10.21.
* Specifications By : JongOh, Kim
* Pattern           : 2.3
* Development Request No : UD1K901977
* Addl Documentation:
* Description       : Maintenance of FSC Planning Profile
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

REPORT ZEPP323U_PLANNING_PROFILE  NO STANDARD PAGE HEADING
                      LINE-SIZE 470
                      MESSAGE-ID ZMPP.
*----------------------------------------------------------------------*
*  EXTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
TABLES:  CUVTAB,          "Variant table basic data
         CUVTAB_VALC,     "CHAR format values for variant table
         CUVTLN,          "Line object of variant table
         ZTBM_ABXCFIDT,   "Configuration Profile
         ZTBM_ABXCLSDT,   "CLASS
         TPHVP,           "Header: Planning Profile
         TPLVP,           "Planning Profile
         TPSVP,           "Planning profile lines
         AUSP.            "Characteristic Values
TABLES:  RM60DRL,         "Create Relevance for Planning directly
         MAVA1,           "Material Master Fields for KMAT
         MTCOM.           "Material Master Communication

*----------------------------------------------------------------------*
*  INTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
DATA: IT_ABXCFIDT   LIKE TABLE OF ZTBM_ABXCFIDT WITH HEADER LINE.

DATA: IT_KSML       LIKE TABLE OF KSML          WITH HEADER LINE.
DATA: IT_TPLVP      LIKE TABLE OF TPLVP         WITH HEADER LINE.
DATA: IT_TPHVP      LIKE TABLE OF TPHVP         WITH HEADER LINE.

DATA: IT_VALC       LIKE TABLE OF CUVTAB_VALC   WITH HEADER LINE.

DATA: BEGIN OF IT_CAWN  OCCURS 0,
        ATINN      LIKE  CAWN-ATINN,  "INTERNAL No
        ATNAM      LIKE  CABN-ATNAM,  "NAME
        ATWRT      LIKE  CAWN-ATWRT.  "VALUE
DATA: END OF IT_CAWN.

DATA: IT_CAWN_TEMP LIKE TABLE OF IT_CAWN       WITH HEADER LINE.
DATA: IT_RM60DRL   LIKE TABLE OF RM60DRL       WITH HEADER LINE.

*----------------------------------------------------------------------*
*  GLOBAL VARIABLE DECLARATION
*----------------------------------------------------------------------*
DATA : WA_ABXCFIDT_IX    TYPE  SY-TABIX.
DATA : WA_ABXCFIDT_PD    TYPE  P,          "TOTAL COUNT
       WA_SUCCESS_PD     TYPE  P,          "SUCCESS COUNT
       WA_ERROR_PD       TYPE  P.          "ERROR COUNT
DATA : WA_CLINT          TYPE  KLAH-CLINT. "Internal Class Number

RANGES : R_VALC FOR CUVTAB-VTINT.

*----------------------------------------------------------------------*
*  CONSTANTS DECLARATION
*----------------------------------------------------------------------*
CONSTANTS: C_MARK    VALUE  'X'.

*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.
*PARAMETERS: P_WERKS     TYPE   T001W-WERKS  OBLIGATORY.
SELECT-OPTIONS: S_DATUM FOR SY-DATUM.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  (20) TEXT-100.
PARAMETERS: R1 RADIOBUTTON GROUP RA .
SELECTION-SCREEN COMMENT  (20) TEXT-101 FOR FIELD R1.   "Create
PARAMETERS: R2 RADIOBUTTON GROUP RA .
SELECTION-SCREEN COMMENT  (20) TEXT-102 FOR FIELD R2.   "Maintain
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B1.

************************************************************************
* INITIALIZAION
************************************************************************
INITIALIZATION.
  PERFORM INITIALIZATION.

************************************************************************
* AT SELECTION-SCREEN
************************************************************************
AT SELECTION-SCREEN.
*  PERFORM AT_SELECTION-SCREEN.

************************************************************************
* START-OF-SELECTION
************************************************************************
START-OF-SELECTION.
  PERFORM EXECUTE_PROCESS.

************************************************************************
* END-OF-SELECTION
************************************************************************
END-OF-SELECTION.
  PERFORM LIST_PROCESS.

*&---------------------------------------------------------------------*
*&      Form  EXECUTE_PROCESS
*&---------------------------------------------------------------------*
FORM EXECUTE_PROCESS.

  CLEAR WA_ERROR_PD.
  CASE C_MARK.
    WHEN R1.   "CREATE
      PERFORM CREATE_PROCESS.
    WHEN R2.   "MAINTAIN
      PERFORM MAINTAIN_PROCESS.
  ENDCASE.
ENDFORM.                    " EXECUTE_PROCESS

*&---------------------------------------------------------------------*
*&      Form  CREATE_PROCESS
*&---------------------------------------------------------------------*
FORM CREATE_PROCESS.
*------> SELECT ZTBM_ABXCFIDT
  PERFORM SELECT_ZTBM_ABXCFIDT.

*------> To create Planning profile from BDC & To check error of BDC
  PERFORM CREATE_PROFILE_OF_ABXCFIDT.

ENDFORM.                    " CREATE_PROCESS

*&---------------------------------------------------------------------*
*&      Form  SELECT_ZTBM_ABXCFIDT
*&---------------------------------------------------------------------*
FORM SELECT_ZTBM_ABXCFIDT.
  DATA : L_ZBTIM   LIKE   SY-UZEIT.
  CLEAR : L_ZBTIM, IT_ABXCFIDT, IT_ABXCFIDT[].
  SELECT *
         INTO TABLE IT_ABXCFIDT
         FROM ZTBM_ABXCFIDT
         WHERE ZBDAT IN S_DATUM
           AND ZBTIM NE L_ZBTIM.

  IF SY-SUBRC EQ 0.
    CLEAR : IT_ABXCFIDT-ZRESULT,
            IT_ABXCFIDT-ZMSG.
    MODIFY IT_ABXCFIDT TRANSPORTING ZRESULT ZMSG
           WHERE MANDT EQ SY-MANDT.
  ENDIF.
ENDFORM.                    " SELECT_ZTBM_ABXCFIDT

*&---------------------------------------------------------------------*
*&      Form  CREATE_PROFILE_OF_ABXCFIDT
*&---------------------------------------------------------------------*
FORM CREATE_PROFILE_OF_ABXCFIDT.
  DESCRIBE TABLE IT_ABXCFIDT LINES  WA_ABXCFIDT_PD.
  LOOP AT IT_ABXCFIDT.
    CLEAR WA_CLINT.
    WA_ABXCFIDT_IX = SY-TABIX.
* Check of Class Header Data
    SELECT SINGLE CLINT
                 INTO WA_CLINT
                 FROM KLAH
                 WHERE CLASS EQ IT_ABXCFIDT-CLID  "CLASS ID
                   AND KLART EQ IT_ABXCFIDT-CLTY  "CLASS TYPE
                   AND KLAGR EQ 'COLOR'.          "Class Group
    IF SY-SUBRC EQ 0.
      PERFORM CHECK_KSML .
    ELSE.
      WA_ERROR_PD = WA_ERROR_PD + 1.
      MOVE 'E'         TO IT_ABXCFIDT-ZRESULT.
      MOVE TEXT-301    TO IT_ABXCFIDT-ZMSG.
      MODIFY IT_ABXCFIDT  INDEX WA_ABXCFIDT_IX.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " CREATE_PROFILE_OF_ABXCFIDT

*&---------------------------------------------------------------------*
*&      Form  SELECT_CUVTAB_VALC
*&---------------------------------------------------------------------*
FORM SELECT_CUVTAB_VALC.
  SELECT *
         INTO TABLE IT_VALC
         FROM CUVTAB_VALC
         WHERE VTINT IN R_VALC.
  SORT IT_VALC BY VTINT SLNID.
ENDFORM.                    " SELECT_CUVTAB_VALC
*&---------------------------------------------------------------------*
*&      Form  SELECT_CAWN
*&---------------------------------------------------------------------*
FORM SELECT_CAWN USING P_OMERK P_IND P_ATNAM.

  CLEAR : IT_CAWN_TEMP, IT_CAWN_TEMP[].
  CASE P_IND.
    WHEN 'CAWN'.
      SELECT ATINN
             ATWRT
             INTO CORRESPONDING FIELDS OF TABLE IT_CAWN_TEMP
             FROM CAWN
             WHERE ATINN EQ P_OMERK.
      SORT IT_CAWN_TEMP.
      LOOP AT IT_CAWN_TEMP.
        MOVE-CORRESPONDING IT_CAWN_TEMP TO IT_CAWN.
        IT_CAWN-ATINN = IT_KSML-IMERK.
        IT_CAWN-ATNAM = P_ATNAM.
        APPEND IT_CAWN.
      ENDLOOP.
    WHEN 'AUSP'.
      IT_CAWN_TEMP-ATINN = IT_KSML-IMERK.
      IT_CAWN_TEMP-ATWRT = AUSP-ATWRT.
      MOVE-CORRESPONDING IT_CAWN_TEMP TO IT_CAWN.
      IT_CAWN-ATNAM = P_ATNAM.
      APPEND IT_CAWN.
  ENDCASE.

ENDFORM.                    " SELECT_CAWN
*&---------------------------------------------------------------------*
*&      Form  CHECK_KSML
*&---------------------------------------------------------------------*
FORM CHECK_KSML.
  CLEAR : IT_KSML, IT_KSML[].
* Check of Characteristics of a Class
  SELECT *
         INTO TABLE IT_KSML
         FROM KSML
         WHERE CLINT EQ WA_CLINT.
  IF SY-SUBRC EQ 0.
    SORT IT_KSML BY IMERK.
    PERFORM GENERATE_PROFILE.
  ELSE.
    WA_ERROR_PD  =  WA_ERROR_PD + 1.
    MOVE 'E'        TO  IT_ABXCFIDT-ZRESULT.
    MOVE TEXT-302   TO  IT_ABXCFIDT-ZMSG.
    MODIFY IT_ABXCFIDT   INDEX  WA_ABXCFIDT_IX.
  ENDIF.
ENDFORM.                    " CHECK_KSML
*&---------------------------------------------------------------------*
*&      Form  CHECK_CUVTAB_CAWN
*&---------------------------------------------------------------------*
FORM CHECK_CUVTAB_CAWN.
  DATA : L_ATNAM         TYPE  CABN-ATNAM.
  DATA : L_VTINT_CUVTAB  TYPE  CUVTAB-VTINT.
  DATA : L_CUOBJ         TYPE  INOB-CUOBJ.

  CLEAR : IT_CAWN, IT_CAWN[], IT_CAWN_TEMP, IT_CAWN_TEMP[].
* Link between Internal Number and Object
  CLEAR L_CUOBJ.
  SELECT SINGLE CUOBJ
          INTO L_CUOBJ
          FROM INOB
          WHERE KLART EQ IT_ABXCFIDT-CLTY  "Class type
            AND OBTAB EQ 'MARA'  "Name of database table for object
            AND OBJEK EQ IT_ABXCFIDT-MTNO. "Key of Object of Classified

  LOOP AT IT_KSML.
    CLEAR : L_ATNAM, L_VTINT_CUVTAB.
    SELECT SINGLE ATNAM
            INTO L_ATNAM
            FROM CABN
            WHERE ATINN EQ IT_KSML-IMERK.  "Characteristic
    IF SY-SUBRC EQ 0.
*----> Check Planning Table of Characteristics in Class
      SELECT SINGLE VTINT
                   INTO L_VTINT_CUVTAB
                   FROM CUVTAB
                   WHERE VTNAM EQ L_ATNAM    "Name of variant table
                     AND VTGRU EQ 'COLOR'    "Variant table group
                     AND VTSTA EQ '1'.       "Variant table status
      IF SY-SUBRC EQ 0.
        CLEAR R_VALC.
        R_VALC-SIGN   = 'I'.
        R_VALC-OPTION = 'EQ'.
        R_VALC-LOW    = L_VTINT_CUVTAB.
        APPEND R_VALC.

        CLEAR AUSP.
        SELECT SINGLE *
                FROM AUSP
                WHERE OBJEK  EQ  L_CUOBJ
                  AND ATINN  EQ  IT_KSML-IMERK
                  AND MAFID  EQ  'O'
                  AND KLART  EQ  IT_ABXCFIDT-CLTY.

*-----> Select All Valid Values of Characteristics for Class
        IF SY-SUBRC NE 0.
          PERFORM SELECT_CAWN USING IT_KSML-OMERK 'CAWN' L_ATNAM.
        ELSE.
          PERFORM SELECT_CAWN USING IT_KSML-OMERK 'AUSP' L_ATNAM.
        ENDIF.

      ENDIF.
    ENDIF.
  ENDLOOP.

*-----> Select All Values of Planning Table fields
  PERFORM SELECT_CUVTAB_VALC.


ENDFORM.                    " CHECK_CUVTAB_CAWN

*&---------------------------------------------------------------------*
*&      Form  MAINTAIN_PROCESS
*&---------------------------------------------------------------------*
FORM MAINTAIN_PROCESS.
  DATA : L_UZEIT   TYPE  SY-UZEIT.
  SELECT SINGLE *
        FROM ZTBM_ABXCLSDT
        WHERE ZBDAT IN S_DATUM
          AND ZBTIM NE L_UZEIT.
  IF SY-SUBRC EQ 0.
*------> Select Planning profile
    PERFORM SELECT_TPHVP.

*------> To maintain Planning profile from BDC & To check error of BDC
    PERFORM MAINTAIN_PROFILE_OF_MARA.
  ENDIF.
ENDFORM.                    " MAINTAIN_PROCESS
*&---------------------------------------------------------------------*
*&      Form  SELECT_TPHVP
*&---------------------------------------------------------------------*
FORM SELECT_TPHVP.
  SELECT *
         INTO TABLE IT_TPHVP
         FROM TPHVP.

*---> APPEND IT_ABXCFIDT
  PERFORM APPEND_ABXCFIDT.
ENDFORM.                    " SELECT_TPHVP
*&---------------------------------------------------------------------*
*&      Form  MAINTAIN_PROFILE_OF_MARA
*&---------------------------------------------------------------------*
FORM MAINTAIN_PROFILE_OF_MARA.
  DESCRIBE TABLE IT_ABXCFIDT LINES  WA_ABXCFIDT_PD.
  LOOP AT IT_ABXCFIDT.
    CLEAR WA_CLINT.
    WA_ABXCFIDT_IX = SY-TABIX.

* Check of Planning Profile
    READ TABLE IT_TPHVP WITH KEY OBJEKT = IT_ABXCFIDT-MTNO.
    SELECT SINGLE *
               FROM TPLVP
               WHERE PROFILID EQ IT_TPHVP-PROFILID.

* Check of Class Description
    SELECT SINGLE CLINT
                INTO WA_CLINT
                FROM SWOR
                WHERE KSCHL EQ IT_ABXCFIDT-MTNO+6(2)
                  AND SPRAS EQ SY-LANGU.

* Check of Class Header Data
    SELECT SINGLE CLASS
                 INTO IT_ABXCFIDT-CLID
                 FROM KLAH
                 WHERE CLINT EQ WA_CLINT        "Internal Class Number
                   AND KLART EQ IT_ABXCFIDT-CLTY  "CLASS TYPE
                   AND KLAGR EQ 'COLOR'.          "Class Group
    IF SY-SUBRC EQ 0.
      PERFORM CHECK_KSML.
    ELSE.
      WA_ERROR_PD = WA_ERROR_PD + 1.
      MOVE 'E'         TO IT_ABXCFIDT-ZRESULT.
      MOVE TEXT-301    TO IT_ABXCFIDT-ZMSG.
      MODIFY IT_ABXCFIDT  INDEX WA_ABXCFIDT_IX.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " MAINTAIN_PROFILE_OF_MARA
*&---------------------------------------------------------------------*
*&      Form  APPEND_ABXCFIDT
*&---------------------------------------------------------------------*
FORM APPEND_ABXCFIDT.
  DATA : L_CUOBJ     LIKE   INOB-CUOBJ.
  DATA : L_ATNAM     LIKE   CABN-ATNAM.
  DATA : L_CLINT         TYPE  KLAH-CLINT.

  LOOP AT IT_TPHVP.
    CLEAR IT_ABXCFIDT.
    MOVE : IT_TPHVP-OBJEKT  TO  IT_ABXCFIDT-MTNO.  "FSC
*           IT_TPHVP-KLART   TO  IT_ABXCFIDT-CLTY.  "Class type

* Plant
    SELECT SINGLE WERKS
                INTO IT_ABXCFIDT-PLNT
                FROM MARC
                WHERE MATNR EQ IT_TPHVP-OBJEKT.
*                  AND WERKS EQ P_WERKS.

* Class
    CLEAR L_CUOBJ.
    SELECT SINGLE CUOBJ
                 INTO L_CUOBJ
                 FROM INOB
*                 WHERE KLART EQ IT_TPHVP-KLART
                 WHERE OBTAB EQ 'MARA'
                   AND OBJEK EQ IT_TPHVP-OBJEKT.
    CLEAR L_CLINT.
    SELECT SINGLE CLINT
                INTO L_CLINT
                FROM KSSK
                WHERE OBJEK EQ L_CUOBJ
                  AND MAFID EQ 'O'.
*                  AND KLART EQ IT_TPHVP-KLART.

    SELECT SINGLE CLASS
                  KLART
                  INTO (IT_ABXCFIDT-CLID,IT_ABXCFIDT-CLTY)
                  FROM KLAH
                  WHERE CLINT EQ L_CLINT.

    CLEAR : IT_KSML, IT_KSML[].
    SELECT *
           INTO TABLE IT_KSML
           FROM KSML
           WHERE CLINT EQ L_CLINT.

    LOOP AT IT_KSML.
      SELECT SINGLE ATNAM
                 INTO L_ATNAM
                 FROM CABN
                 WHERE ATINN EQ IT_KSML-IMERK.
      CASE L_ATNAM.
        WHEN 'COLOR_MI'.
          PERFORM SELECT_AUSP USING L_CUOBJ IT_ABXCFIDT-CLMI.
        WHEN 'COLOR_OPT1'.
          PERFORM SELECT_AUSP USING L_CUOBJ IT_ABXCFIDT-CLO1.
        WHEN 'COLOR_OPT2'.
          PERFORM SELECT_AUSP USING L_CUOBJ IT_ABXCFIDT-CLO2.
        WHEN 'COLOR_OPT3'.
          PERFORM SELECT_AUSP USING L_CUOBJ IT_ABXCFIDT-CLO3.
        WHEN 'COLOR_OPT4'.
          PERFORM SELECT_AUSP USING L_CUOBJ IT_ABXCFIDT-CLO4.
      ENDCASE.
    ENDLOOP.
    APPEND IT_ABXCFIDT.
    CLEAR IT_ABXCFIDT.
  ENDLOOP.

ENDFORM.                    " APPEND_ABXCFIDT

*&---------------------------------------------------------------------*
*&      Form  SELECT_AUSP
*&---------------------------------------------------------------------*
FORM SELECT_AUSP USING P_CUOBJ  P_FIELD.
  SELECT SINGLE ATWRT
          INTO P_FIELD
          FROM AUSP
          WHERE OBJEK  EQ  P_CUOBJ
            AND ATINN  EQ  IT_KSML-IMERK
            AND MAFID  EQ  'O'
            AND KLART  EQ  IT_TPHVP-KLART.
ENDFORM.                    " SELECT_AUSP
*&---------------------------------------------------------------------*
*&      Form  LIST_PROCESS
*&---------------------------------------------------------------------*
FORM LIST_PROCESS.
  DATA : L_TABIX     TYPE   SY-TABIX,
         L_INDEX     TYPE   SY-TABIX.

  WRITE:/ TEXT-311 , WA_ABXCFIDT_PD.
  CASE C_MARK.
    WHEN R1.
      WRITE:/ TEXT-312 , WA_SUCCESS_PD.
    WHEN R2.
      WRITE:/ TEXT-313 , WA_SUCCESS_PD.
  ENDCASE.
  WRITE:/ TEXT-314 , WA_ERROR_PD.

  SKIP 2.
  LOOP AT IT_ABXCFIDT WHERE ZRESULT EQ 'E'.
    L_TABIX = L_TABIX  + 1.
    L_INDEX = L_TABIX MOD 2.
    AT FIRST.
      WRITE: '************** Begin of Error Detail List **************'.
    ENDAT.
    IF L_INDEX EQ 0.
      FORMAT INTENSIFIED ON.
    ELSE.
      FORMAT INTENSIFIED OFF.
    ENDIF.
    WRITE:/  L_TABIX COLOR COL_TOTAL,
            IT_ABXCFIDT-MTNO   COLOR COL_KEY,     "FSC
            IT_ABXCFIDT-PLNT   COLOR COL_KEY,     "PLANT
            IT_ABXCFIDT-CLID   COLOR COL_KEY,     "CLASS
            IT_ABXCFIDT-EONO   COLOR COL_NORMAL,  "CHANGE NUMBER
            IT_ABXCFIDT-PRIT   COLOR COL_NORMAL,  "Conf profile priority
            IT_ABXCFIDT-PROF   COLOR COL_NORMAL,  "Conf profile name
            IT_ABXCFIDT-CLTY   COLOR COL_NORMAL,  "CLASS TYPE
            IT_ABXCFIDT-STAT   COLOR COL_NORMAL,  "Status of Conf profil
            IT_ABXCFIDT-CLMI   COLOR COL_NORMAL,  "COLOR MI
            IT_ABXCFIDT-CLO1   COLOR COL_NORMAL,  "COLOR OPT1#
            IT_ABXCFIDT-CLO2   COLOR COL_NORMAL,  "COLOR OPT2#
            IT_ABXCFIDT-CLO3   COLOR COL_NORMAL,  "COLOR OPT3#
            IT_ABXCFIDT-CLO4   COLOR COL_NORMAL,  "COLOR OPT4#
            IT_ABXCFIDT-GEFT   COLOR COL_NORMAL,  "Processing division
            IT_ABXCFIDT-ZUSER  COLOR COL_NORMAL,  "
            IT_ABXCFIDT-ZSTIM  COLOR COL_NORMAL,  "
            IT_ABXCFIDT-ZEDAT  COLOR COL_NORMAL,  "
            IT_ABXCFIDT-ZETIM  COLOR COL_NORMAL,  "
            IT_ABXCFIDT-ZBDAT  COLOR COL_NORMAL,  "
            IT_ABXCFIDT-ZBNAM  COLOR COL_NORMAL,  "
            IT_ABXCFIDT-ZMODE  COLOR COL_NORMAL,  "
            IT_ABXCFIDT-ZRESULT COLOR COL_NEGATIVE, "
            IT_ABXCFIDT-ZMSG   COLOR COL_POSITIVE.  "DETAIL OF ABXCFIDT
    AT LAST.
      FORMAT INTENSIFIED ON.
      WRITE: / '************** End of Error Detail List **************'.
    ENDAT.
  ENDLOOP.
ENDFORM.                    " LIST_PROCESS
*&---------------------------------------------------------------------*
*&      Form  CREATE_PROFILE
*&---------------------------------------------------------------------*
FORM CREATE_PROFILE USING P_CLINT.
  DATA : L_MATNR    TYPE RM60B-MATNR,
         L_PROFTXT  TYPE TPHVP-PROFTXT.
  CLEAR: IT_RM60DRL, IT_RM60DRL[].
  LOOP AT IT_VALC.
    CLEAR : IT_RM60DRL.
    MOVE : IT_VALC-VTINT  TO  IT_RM60DRL-CLINT,
           IT_VALC-SLNID  TO  IT_RM60DRL-LNPOS,
           'X'            TO  IT_RM60DRL-ATERF.
    READ TABLE IT_CAWN WITH KEY ATINN = IT_VALC-ATINN
                                ATWRT = IT_VALC-VALC.
    IF SY-SUBRC EQ 0.
      MOVE   'X'          TO  IT_RM60DRL-PL_REL.
    ENDIF.
    APPEND IT_RM60DRL.
  ENDLOOP.
  L_MATNR   = IT_ABXCFIDT-MTNO.
  L_PROFTXT = IT_ABXCFIDT-MTNO.
  CALL FUNCTION 'REQUIREMENTS_MAINT_RELE_BACK'
       EXPORTING
            I_MATNR                   = L_MATNR
            I_PROFTXT                 = L_PROFTXT
            I_PLNTP                   = ' '
            I_VISUD                   = 'X'
            I_PROFILID                = TPHVP-PROFILID
            I_CLINT                   = P_CLINT  "CLASS
       TABLES
            I_VALUES                  = IT_RM60DRL
       EXCEPTIONS
            MATERIAL_NOT_FOUND        = 1
            MATERIAL_NOT_CONFIGURABLE = 2
            NO_RELEV_CHAR_VAL         = 3
            MATERIAL_BLOCKED          = 4
            SYSTEM_ERROR              = 5
            PROFILE_NOT_FOUND         = 6
            NO_UPDATE_POSSIBLE        = 7
            DOUBLE_CHARACTERISTIC     = 8
            OTHERS                    = 9.

  IF SY-SUBRC EQ 0.
    WA_SUCCESS_PD = WA_SUCCESS_PD + 1.
  ELSE.
    PERFORM MESSAGE_TXT.
  ENDIF.

ENDFORM.                    " CREATE_PROFILE
*&---------------------------------------------------------------------*
*&      Form  MESSAGE_TXT
*&---------------------------------------------------------------------*
FORM MESSAGE_TXT.
  DATA L_MSG  LIKE CFGNL-MSGLIN.
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
            MSG_LIN = L_MSG
       EXCEPTIONS
            OTHERS  = 1.
  CASE SY-MSGTY.
    WHEN 'E' OR 'A' OR 'X' OR 'W'.
      WA_ERROR_PD = WA_ERROR_PD + 1.
      MOVE L_MSG      TO  IT_ABXCFIDT-ZMSG.
      MOVE SY-MSGTY   TO  IT_ABXCFIDT-ZRESULT.
      MODIFY IT_ABXCFIDT  INDEX  WA_ABXCFIDT_IX.
    WHEN OTHERS.     " 'I', 'S' :SUCCESS
  ENDCASE.
ENDFORM.                    " MESSAGE_TXT
*&---------------------------------------------------------------------*
*&      Form  GENERATE_PROFILE
*&---------------------------------------------------------------------*
FORM GENERATE_PROFILE.
* Search valid value of characteristics in class assigned to FSC and
* Search all value of planning table assigned to characteristics
  PERFORM CHECK_CUVTAB_CAWN.

* Check Header: Planning Profile
  CLEAR TPHVP.
  SELECT SINGLE *
             FROM TPHVP
             WHERE OBJEKT EQ IT_ABXCFIDT-MTNO.   "FSC Material Code
  IF SY-SUBRC EQ 0.
*Check Planning Profile Line
    CLEAR : IT_TPLVP, IT_TPLVP[].
    SELECT *
           INTO TABLE IT_TPLVP
           FROM TPLVP
           WHERE PROFILID EQ TPHVP-PROFILID. "Name of planning profile
    IF SY-SUBRC NE 0.
      CASE C_MARK.
        WHEN R1.  "CREATE
          PERFORM CREATE_PROFILE USING WA_CLINT.
        WHEN R2.  "MAINTAIN
          PERFORM CREATE_PROFILE USING WA_CLINT.
      ENDCASE.
    ELSE.
      CASE C_MARK.
        WHEN R1.  "CREATE
          WA_ERROR_PD = WA_ERROR_PD + 1.
          MOVE TEXT-303  TO  IT_ABXCFIDT-ZMSG.
          MOVE 'E'       TO  IT_ABXCFIDT-ZRESULT.
          MODIFY IT_ABXCFIDT  INDEX  WA_ABXCFIDT_IX.
        WHEN R2.  "MAINTAIN
          PERFORM CREATE_PROFILE USING WA_CLINT.
      ENDCASE.
    ENDIF.
  ELSE.
    CASE C_MARK.
      WHEN R1.  "CREATE
        PERFORM CREATE_PROFILE USING WA_CLINT.
      WHEN R2.  "MAINTAIN
        WA_ERROR_PD = WA_ERROR_PD + 1.
        MOVE TEXT-303   TO  IT_ABXCFIDT-ZMSG.
        MOVE 'E'        TO  IT_ABXCFIDT-ZRESULT.
        MODIFY IT_ABXCFIDT  INDEX  WA_ABXCFIDT_IX.
    ENDCASE.
  ENDIF.

ENDFORM.                    " GENERATE_PROFILE
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
FORM INITIALIZATION.
  CLEAR : S_DATUM , S_DATUM[] .
  S_DATUM-SIGN   = 'I' .
  S_DATUM-OPTION = 'EQ' .
  S_DATUM-LOW    = SY-DATUM .
  APPEND S_DATUM .
ENDFORM.                    " INITIALIZATION
