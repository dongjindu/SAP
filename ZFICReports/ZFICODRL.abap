*&---------------------------------------------------------------------*
*& Report  ZFICODRL
*&                                                                     *
*&---------------------------------------------------------------------*
* changing history:
* -----------------
* SCT 02052000 note 202357 changed
* --------------------------------

REPORT  ZFICODRL LINE-SIZE 250.

*=======================================================================
* global data:
*=======================================================================
TABLES:
COFIS, BKPF, BSEG.

DATA:
GS_TKA01 LIKE  TKA01,
GT_BKPF  LIKE  BKPF  OCCURS 0 WITH HEADER LINE,
GT_BSEG  LIKE  BSEG  OCCURS 0 WITH HEADER LINE,
GT_COFIS LIKE  COFIS OCCURS 0 WITH HEADER LINE,
GT_COVP  LIKE  COVP  OCCURS 0 WITH HEADER LINE,
GD_STRING(20),
GD_REV         LIKE  BOOLE-BOOLE,
GD_MESS_COUNT  LIKE SY-TFILL,
GD_HIDDEN_FIELD(120)  TYPE C,

BEGIN OF GS_SEL_BELNR,
   SIGN(1),
   OPTION(2),
   LOW    LIKE BKPF-BELNR,
   HIGH   LIKE BKPF-BELNR,
END OF GS_SEL_BELNR.

*=======================================================================
* selection screen:
*=======================================================================

* FI document number:
SELECTION-SCREEN BEGIN OF BLOCK A WITH FRAME.
PARAMETERS:
  COMPCODE LIKE  BKPF-BUKRS MEMORY ID BUK   OBLIGATORY,
  DOCNR    LIKE  BKPF-BELNR MEMORY ID BLN   OBLIGATORY,
  YEAR     LIKE  BKPF-GJAHR MEMORY ID GJR   OBLIGATORY.
SELECTION-SCREEN END OF BLOCK A.

SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK B WITH FRAME.
PARAMETERS:
CODOCS  LIKE  BOOLE-BOOLE DEFAULT 'X'.

SELECT-OPTIONS:
  REVERSAL FOR COFIS-STFLG NO-DISPLAY,
  REVERSED FOR COFIS-STOKZ NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK B.

SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK C WITH FRAME.
PARAMETERS:
  P_TECH   LIKE  BOOLE-BOOLE NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK C.

*=======================================================================
INITIALIZATION.
*=======================================================================

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

  REFRESH GT_BKPF.
  CLEAR   GT_BKPF.

  REFRESH GT_BSEG.
  CLEAR   GT_BSEG.

  REFRESH GT_COFIS.
  CLEAR   GT_COFIS.

  PERFORM READ_FI_DOC
          TABLES  GT_BKPF
                  GT_BSEG
          USING   COMPCODE
                  DOCNR
                  YEAR.

  CHECK ( NOT GT_BKPF[] IS INITIAL ).

  PERFORM FIND_RCL_DOCUMENTS
          TABLES  GT_BKPF
                  GT_COFIS.

  CHECK ( NOT GT_COFIS[] IS INITIAL ).

  PERFORM FIND_CO_DOCS_FOR_RCL_DOC
          TABLES  GT_COFIS
                  GT_COVP
          USING   CODOCS.

  PERFORM LIST_OUTPUT
          TABLES  GT_BKPF
                  GT_BSEG
                  GT_COFIS
                  GT_COVP
          USING   CODOCS.


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

*=======================================================================
END-OF-SELECTION.
*=======================================================================

*=======================================================================
* Forms:
*=======================================================================

*&---------------------------------------------------------------------*
*&      Form  find_rcl_documents
*&---------------------------------------------------------------------*
FORM FIND_RCL_DOCUMENTS
     TABLES  IT_BKPF          STRUCTURE  BKPF
             ET_COFIS         STRUCTURE  GT_COFIS.

  RANGES:
  LT_STFLG  FOR COFIS-STFLG,
  LT_STOKZ  FOR COFIS-STOKZ.

  DATA:
  BEGIN OF LS_AWKEY,
    AWREF LIKE ACCHD-AWREF,
    AWORG LIKE ACCHD-AWORG,
  END OF LS_AWKEY.

  LOOP AT IT_BKPF.
    LS_AWKEY = IT_BKPF-AWKEY.
    SELECT * FROM COFIS
             APPENDING TABLE ET_COFIS
               WHERE KOKRS  = LS_AWKEY-AWORG
               AND   DOCNR  = LS_AWKEY-AWREF.
    IF ( SY-SUBRC <> 0 ).
      CALL FUNCTION 'MESSAGE_STORE'
           EXPORTING
                ARBGB                   = 'K5'
                EXCEPTION_IF_NOT_ACTIVE = ' '
                MSGTY                   = 'I'
                MSGV1 =
                   'No RCL doc found for FI doc'            "#EC NOTEXT
                MSGV2                   = IT_BKPF-BUKRS
                MSGV3                   = IT_BKPF-BELNR
                MSGV4                   = IT_BKPF-GJAHR
                TXTNR                   = '001'.
    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  find_fi_documents
*&---------------------------------------------------------------------*
FORM FIND_CO_DOCS_FOR_RCL_DOC
     TABLES   IT_COFIS   STRUCTURE COFIS
              ET_COVP    STRUCTURE COVP
     USING    ID_CODOCS  LIKE      BOOLE-BOOLE.

  CHECK ( ID_CODOCS = 'X' ).

  CHECK ( NOT IT_COFIS[] IS INITIAL ).                        "02052000

  IF ( SY-SAPRL(1) >= '4' ).                                  "02052000
*   release >= 4.0A                                           "02052000
    CALL FUNCTION 'K_RCL_COEPS_FOR_COFIS'
         EXPORTING
              I_FLG_DISPLAY = ' '
         TABLES
              T_COFIS       = IT_COFIS
              T_COVP        = ET_COVP.
  ELSE.                                                      "02052000
*  release < 4.0A                                            "02052000
   DATA:  LT_COFIT  LIKE  COFIT  OCCURS 0 WITH HEADER LINE.  "02052000
   LOOP AT IT_COFIS.                                         "02052000
      MOVE-CORRESPONDING IT_COFIS TO LT_COFIT.               "02052000
      APPEND LT_COFIT.                                       "02052000
   ENDLOOP.                                                  "02052000
   CALL FUNCTION 'K_RCL_COEPS_FOR_COFIT'                     "02052000
        EXPORTING                                            "02052000
             I_PER_FROM    = IT_COFIS-POPER                  "02052000
             I_PER_TO      = IT_COFIS-POPER                  "02052000
             I_FLG_DISPLAY = ' '                             "02052000
        TABLES                                               "02052000
             T_COFIT       = LT_COFIT                        "02052000
             T_COVP        = ET_COVP                         "02052000
        EXCEPTIONS                                           "02052000
             NOT_FOUND     = 0.                              "02052000
  ENDIF.                                                     "02052000


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  execute_user_command
*&---------------------------------------------------------------------*
FORM EXECUTE_USER_COMMAND
     USING    ID_UCOMM  LIKE  SY-UCOMM.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  read_fi_doc
*&---------------------------------------------------------------------*
FORM READ_FI_DOC
     TABLES   ET_BKPF     STRUCTURE BKPF
              ET_BSEG     STRUCTURE BSEG
     USING    ID_BUKRS    LIKE      BKPF-BUKRS
              ID_BELNR    LIKE      BKPF-BELNR
              ID_GJAHR    LIKE      BKPF-GJAHR.

  SELECT SINGLE * FROM BKPF INTO ET_BKPF
                  WHERE BUKRS = ID_BUKRS
                  AND   BELNR = ID_BELNR
                  AND   GJAHR = ID_GJAHR.
  IF ( SY-SUBRC = 0 ).
    IF ( ET_BKPF-AWTYP = 'COFIS' ).
      APPEND ET_BKPF.
    ELSE.
      WRITE : /, /,
              AT 2  'FI document',
              AT 30 ID_BUKRS, ID_BELNR, ID_GJAHR,
              AT 2  'was not created by reconciliation posting'.
    ENDIF.
  ELSE.
    WRITE: /, /,
           AT 2 'FI document not found',
           AT 30 ID_BUKRS, ID_BELNR, ID_GJAHR,
           /,  /.

    CALL FUNCTION 'MESSAGE_STORE'
         EXPORTING
              ARBGB                   = 'K5'
              EXCEPTION_IF_NOT_ACTIVE = ' '
              MSGTY                   = 'I'
              MSGV1                   = 'FI document not found'
              MSGV2                   = ID_BUKRS
              MSGV3                   = ID_BELNR
              MSGV4                   = ID_GJAHR
              TXTNR                   = '001'.
  ENDIF.

  SELECT * FROM  BSEG INTO TABLE ET_BSEG
           WHERE BUKRS = ID_BUKRS
           AND   BELNR = ID_BELNR
           AND   GJAHR = ID_GJAHR.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  LIST_OUTPUT
*&---------------------------------------------------------------------*
FORM LIST_OUTPUT
     TABLES   IT_BKPF   STRUCTURE  BKPF
              IT_BSEG   STRUCTURE  BSEG
              IT_COFIS  STRUCTURE  COFIS
              IT_COVP   STRUCTURE  COVP
     USING    ID_CODOCS LIKE       BOOLE-BOOLE.

  DATA:
  LD_TSL_SUM_COFIS  LIKE  COFIS-TSL,
  LD_TFILL  LIKE  SY-TFILL.

  PERFORM DISPLAY_FI_DOCUMENT
          TABLES IT_BKPF
                 IT_BSEG.

  DESCRIBE TABLE IT_BKPF[] LINES LD_TFILL.
  IF ( LD_TFILL > 1 ).
    MESSAGE E001(K5) WITH 'Multiple FI documents found'     "#EC NOTEXT
                     IT_BKPF-BUKRS  IT_BKPF-BELNR IT_BKPF-GJAHR.
  ELSEIF ( LD_TFILL = 0 ).
    EXIT.
  ENDIF.

  SORT IT_COFIS BY KOKRS DOCNR DOCLN.

  PERFORM CHECK_RCL_REVERSAL
          TABLES IT_BKPF
                 IT_COFIS.

  PERFORM DISPLAY_RCL_DOCUMENT
          TABLES    IT_BKPF
                    IT_COFIS
          CHANGING  LD_TSL_SUM_COFIS.

  PERFORM CHECK_ADDITIONAL_RCL_DOCS
          TABLES IT_COFIS
          USING  LD_TSL_SUM_COFIS.

  CHECK ( ID_CODOCS = 'X' ).

  PERFORM DISPLAY_COVP
          TABLES   IT_COVP.

  PERFORM DISPLAY_COVP_FOR_COFIS_DOCLN
          TABLES  IT_COFIS
                  IT_COVP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  display_covp
*&---------------------------------------------------------------------*
FORM DISPLAY_COVP
     TABLES  IT_COVP  STRUCTURE  COVP.

  DATA:
  LD_TSL_SUM  LIKE  COFIS-TSL.

* display CO line items:
  SORT IT_COVP BY KOKRS BELNR BUZEI.

  IF ( IT_COVP[] IS INITIAL ).
    WRITE: / 'No CO line items found'.
    EXIT.
  ELSE.
    WRITE: /, /2 'CO document line items that meet the',
              /2 'criteria of the RCL document:', /.
  ENDIF.

  CLEAR LD_TSL_SUM.

  LOOP AT IT_COVP.
    IF ( SY-TABIX = 1 ).
      WRITE: / SY-ULINE(123),
             /1(1)   '|',
              2(10)  'Docnr',
              13(1)  '|',
              14(5)  'Docln',
              20(1)  '|',
              21(6)  'CoCde',
              28(1)  '|',
              29(6)  'BusAr',
              36(1)  '|',
              37(6)  'FuncAr',
              44(1)  '|',
              45(6)  'PCoCde',
              52(1)  '|',
              53(6)  'PBusAr',
              60(1)  '|',
              61(7)  'PFuncAr',
              68(1)  '|',
              69(12) 'Cost element',
              82(1)  '|',
              83(5)  'BusTr',
              89(1)  '|',
              90(6)  'ObjTy',
              97(1)  '|',
              98(6)   'PObjTy',
              106(1)  '|',
              107(15) 'Amount',
              123(1)  '|',
            / SY-ULINE(123).
    ENDIF.
    WRITE:   /1(1)   '|',
              2(10)  IT_COVP-BELNR,
              13(1)  '|',
              14(5)  IT_COVP-BUZEI,
              20(1)  '|',
              21(6)  IT_COVP-BUKRS,
              28(1)  '|',
              29(6)  IT_COVP-GSBER,
              36(1)  '|',
              37(6)  IT_COVP-FKBER,
              44(1)  '|',
              45(6)  IT_COVP-PBUKRS,
              52(1)  '|',
              53(6)  IT_COVP-PARGB,
              60(1)  '|',
              61(6)  IT_COVP-PFKBER,
              68(1)  '|',
              69(12) IT_COVP-KSTAR,
              82(1)  '|',
              83(5)  IT_COVP-VRGNG,
              89(1)  '|',
              90(6)  IT_COVP-OBJNR(2),
              97(1)  '|',
              98(6)   IT_COVP-PAROB1(2),
              106(1)  '|',
              107(15) IT_COVP-WKGBTR,
              123(1)  '|',
            / SY-ULINE(123).
    LD_TSL_SUM = LD_TSL_SUM + IT_COVP-WKGBTR.
  ENDLOOP.
  WRITE: /90(10)   'Summary',
          106(1)   '|',
          107(15)  LD_TSL_SUM,
          123(1)   '|',
         /106 SY-ULINE(18).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  check_rcl_reversal
*&---------------------------------------------------------------------*
FORM CHECK_RCL_REVERSAL
     TABLES   IT_BKPF   STRUCTURE  BKPF
              IT_COFIS  STRUCTURE  COFIS.

  DATA:
  BEGIN OF LS_AWKEY,
    AWREF LIKE ACCHD-AWREF,
    AWORG LIKE ACCHD-AWORG,
  END OF LS_AWKEY.

  LOOP AT IT_BKPF.
    LS_AWKEY = IT_BKPF-AWKEY.
  ENDLOOP.

  LOOP AT IT_COFIS
       WHERE KOKRS = LS_AWKEY-AWORG
       AND   DOCNR = LS_AWKEY-AWREF.

    IF ( SY-TABIX = 1 ) AND
       ( IT_COFIS-STFLG = 'X' ).
      WRITE: /, /2 'This is a reversal RCL document.'.
      WRITE: /2 'It reverses RCL document:',
              30 IT_COFIS-REFDOCNR.
      SELECT * FROM COFIS UP TO 1 ROWS
                          WHERE KOKRS = IT_COFIS-KOKRS
                          AND   DOCNR = IT_COFIS-REFDOCNR.
      ENDSELECT.
      IF ( SY-SUBRC = 0 ).
        WRITE: /2 'This reversed RCL document ',
               /2 'was posted in period /',
               /2 'fiscal year:',
                30 IT_COFIS-POPER, IT_COFIS-RYEAR.
      ELSE.
        WRITE: /2 'This RCL document was not found on DB!'.
      ENDIF.
      LS_AWKEY-AWREF = IT_COFIS-REFDOCNR.
      SELECT SINGLE * FROM BKPF
                      WHERE  AWTYP = 'COFIS'
                      AND    AWKEY = LS_AWKEY.
      IF ( SY-SUBRC = 0 ).
        WRITE: /2  'The reversed FI document:',
                20  BKPF-BELNR,
               /2  'in period / fiscal year:',
                20  BKPF-MONAT, BKPF-GJAHR,
               /2   'in company code:',
                20  BKPF-BUKRS.
      ELSE.
        WRITE: /2 'No reversed FI document found'.
      ENDIF.
    ELSEIF ( SY-TABIX = 1 ) AND
           ( IT_COFIS-STOKZ = 'X' ).
      WRITE: /, /2 'This document has been reversed.'.
      SELECT  * FROM COFIS UP TO 1 ROWS
                           WHERE  KOKRS    = IT_COFIS-KOKRS
                           AND    REFDOCNR = IT_COFIS-DOCNR.
      ENDSELECT.
      IF ( SY-SUBRC = 0 ).
        WRITE: /2 'The reversal RCL document:',
                30 COFIS-DOCNR,
               /2 'in period / fiscal year:',
                30 COFIS-POPER, COFIS-RYEAR.
        LS_AWKEY-AWREF = COFIS-DOCNR.
        SELECT * FROM BKPF UP TO 1 ROWS
                           WHERE AWTYP = 'COFIS'
                           AND   AWKEY = LS_AWKEY.
        ENDSELECT.
        IF ( SY-SUBRC = 0 ).
          WRITE: /2 'The reversal FI document:',
                  30 BKPF-BELNR,
                 /2 'in period / fiscal year:',
                  30 BKPF-MONAT, BKPF-GJAHR.
        ELSE.
          WRITE: /2 'No reversal FI document found'.
        ENDIF.
      ELSE.
        WRITE: /2 'No reversal RCL document found!'.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  display_rcl_document
*&---------------------------------------------------------------------*
FORM DISPLAY_RCL_DOCUMENT
     TABLES    IT_BKPF           STRUCTURE  BKPF
               IT_COFIS          STRUCTURE  COFIS
     CHANGING  CD_TSL_SUM_COFIS  LIKE       COFIS-TSL.

  DATA:
  BEGIN OF LS_AWKEY,
    AWREF LIKE ACCHD-AWREF,
    AWORG LIKE ACCHD-AWORG,
  END OF LS_AWKEY,
  LD_HSL_SUM  LIKE  COFIS-HSL,
  LD_TSL_SUM  LIKE  COFIS-TSL,
  LD_KSL_SUM  LIKE  COFIS-KSL.

* write RCL document:
  WRITE: /,
         /, AT 2 'RCL document:'.

  LOOP AT IT_BKPF.
    LS_AWKEY = IT_BKPF-AWKEY.
  ENDLOOP.

  LOOP AT IT_COFIS
       WHERE KOKRS = LS_AWKEY-AWORG
       AND   DOCNR = LS_AWKEY-AWREF.

*   write header:
    IF ( SY-TABIX = 1 ).
      WRITE: /, SY-ULINE(103),
            /1(1)   '|',
            2(10)  'RCL docnr',
            13(1)  '|',
            14(9)  'RCL docln',
            24(1)  '|',
            26(9)  'Comp code',
            36(1)  '|',
            38(6)  'Period',
            45(1)  '|',
            47(4)  'Year',
            52(1)  '|',
            53(12) 'Cost element',
            66(1)  '|',
            67(10) 'CoArea curr.',
            78(1)  '|',
            80(10) 'Comp. c. curr',
            91(1)  '|',
            92(10) 'Group curr',
            103(1) '|'.
      WRITE / SY-ULINE(103).
    ENDIF.
*   write rcl document:
    WRITE: /1(1)   '|',
            2(10)  IT_COFIS-DOCNR,
            13(1)  '|',
            14(9)  IT_COFIS-DOCLN,
            24(1)  '|',
            26(9)  IT_COFIS-RBUKRS,
            36(1)  '|',
            38(6)  IT_COFIS-POPER,
            45(1)  '|',
            47(4)  IT_COFIS-RYEAR,
            52(1)  '|',
            53(12) IT_COFIS-RACCT,
            66(1)  '|',
            67(10) IT_COFIS-TSL,
            78(1)  '|',
            80(10) IT_COFIS-HSL,
            91(1)  '|',
            92(10) IT_COFIS-KSL,
            103(1) '|'.
    LD_HSL_SUM = LD_HSL_SUM + IT_COFIS-HSL.
    LD_TSL_SUM = LD_TSL_SUM + IT_COFIS-TSL.
    LD_KSL_SUM = LD_KSL_SUM + IT_COFIS-KSL.

  ENDLOOP.
  WRITE: / SY-ULINE(103).

  WRITE: /53(10) 'Summary',
          66(1)  '|',
          67(10) LD_TSL_SUM,
          78(1)  '|',
          80(10) LD_HSL_SUM,
          91(1)  '|',
          92(10) LD_KSL_SUM,
          103(1) '|'.
  WRITE: /66 SY-ULINE(38).

* store amount of this RCL document:
  CD_TSL_SUM_COFIS = LD_TSL_SUM.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  display_fi_document
*&---------------------------------------------------------------------*
FORM DISPLAY_FI_DOCUMENT
     TABLES  IT_BKPF   STRUCTURE  BKPF
             IT_BSEG   STRUCTURE  BSEG.

* write FI document:
  WRITE: /,
         /, AT 2   'FI document:'.

* write FI header:
  LOOP AT IT_BKPF.
    WRITE: /2   'FI document number:',
            30  IT_BKPF-BELNR,
           /2   'company code:',
            30  IT_BKPF-BUKRS,
           /2   'period:',
            30  IT_BKPF-MONAT,
           /2   'fiscal year:',
            30  IT_BKPF-GJAHR,
           /2   'text:',
            30  IT_BKPF-BKTXT.
  ENDLOOP.

* write FI items:
  LOOP AT IT_BSEG.
    IF ( SY-TABIX = 1 ).
*     write header line:
      WRITE: / SY-ULINE(40),
             /1(1)    '|',
              2(3)    'D/C',
              6(1)    '|',
              7(10)   'Account',
              18(1)   '|',
              19(20)  'Amount local curr.',
              40(1)   '|',
              / SY-ULINE(40).
    ENDIF.
    WRITE: /1(1)    '|',
            2(3)    IT_BSEG-BSCHL,
            6(1)    '|',
            7(10)   IT_BSEG-HKONT,
            18(1)   '|',
            19(20)  IT_BSEG-DMBTR,
            40(1)   '|'.

  ENDLOOP.
  WRITE: / SY-ULINE(40).

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  check_additional_rcl_docs
*&---------------------------------------------------------------------*
FORM CHECK_ADDITIONAL_RCL_DOCS
     TABLES   IT_COFIS          STRUCTURE  COFIS
     USING    ID_TSL_SUM_COFIS  LIKE       COFIS-TSL.

  DATA:
  LD_TSL_SUM     LIKE  COFIS-TSL,
  LT_COFIS_HELP  LIKE  COFIS OCCURS 0 WITH HEADER LINE.

* check whether there were multiple reconciliation postings
* or reversals, so that the drill down to the CO line items
* might fail.
  CLEAR LD_TSL_SUM.
  LOOP AT IT_COFIS.

    REFRESH LT_COFIS_HELP.
    CLEAR   LT_COFIS_HELP.

    SELECT * FROM   COFIS UP TO 1 ROWS
          INTO  TABLE LT_COFIS_HELP
          WHERE KOKRS   = IT_COFIS-KOKRS
          AND   POPER   = IT_COFIS-POPER
          AND   RYEAR   = IT_COFIS-RYEAR
          AND   WRTTP   = IT_COFIS-WRTTP
          AND   RVERS   = IT_COFIS-RVERS
          AND   RACCT   = IT_COFIS-RACCT
          AND   ROBART  = IT_COFIS-ROBART
          AND   RHRKFT  = IT_COFIS-RHRKFT
          AND   VRGNG   = IT_COFIS-VRGNG
          AND   SOBART  = IT_COFIS-SOBART
          AND   SBUSA   = IT_COFIS-SBUSA
          AND   DRCRK   = IT_COFIS-DRCRK
          AND   RUNIT   = IT_COFIS-RUNIT
          AND   RBUKRS  = IT_COFIS-RBUKRS
          AND   RBUSA   = IT_COFIS-RBUSA
          AND   RFAREA  = IT_COFIS-RFAREA
          AND   RSCOPE  = IT_COFIS-RSCOPE
          AND   RLOGSYS = IT_COFIS-RLOGSYS
          AND   SBUKRS  = IT_COFIS-SBUKRS
          AND   SFAREA  = IT_COFIS-SFAREA
          AND   SSCOPE  = IT_COFIS-SSCOPE
          AND   SLOGSYS = IT_COFIS-SLOGSYS
          AND   STFLG   = IT_COFIS-STFLG
          AND   STOKZ   = IT_COFIS-STOKZ
          AND   DOCNR  <> IT_COFIS-DOCNR
          AND ( CPUDT  <> IT_COFIS-CPUDT OR
                CPUTM  <> IT_COFIS-CPUTM    ).

    IF ( SY-SUBRC = 0 ).
      WRITE: /2 'There are RCL documents that correspond',
             /2 'to the same cost flow as RCL document / line item',
              IT_COFIS-DOCNR, IT_COFIS-DOCLN, ':'.
      LOOP AT LT_COFIS_HELP.
        WRITE: /2 'RCL document number / line item:',
                  LT_COFIS_HELP-DOCNR, LT_COFIS_HELP-DOCLN.
        WRITE: /2 'The amount in controlling area currency is',
               /2 LT_COFIS_HELP-TSL.
        LD_TSL_SUM = LD_TSL_SUM + LT_COFIS_HELP-TSL.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

  IF ( LD_TSL_SUM > 0 ).
    WRITE: /, /2 'The total amount of the additional RCL',
           'documents is', LD_TSL_SUM.
    LD_TSL_SUM = LD_TSL_SUM + ID_TSL_SUM_COFIS.
    WRITE: /2 'Hence the sum of the CO line items should give',
              LD_TSL_SUM, 'and not', ID_TSL_SUM_COFIS.
  ENDIF.

  WRITE: /, /.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  display_covp_for_cofis_docln
*&---------------------------------------------------------------------*
FORM DISPLAY_COVP_FOR_COFIS_DOCLN
     TABLES   IT_COFIS  STRUCTURE  COFIS
              IT_COVP   STRUCTURE  COVP.

  DATA:
  LD_TFILL      LIKE  SY-TFILL,
  LT_COVP_HELP  LIKE  COVP  OCCURS 0 WITH HEADER LINE.

  RANGES:
  LT_BEKNZ  FOR COVP-BEKNZ,
  LT_ROBART FOR COFIS-ROBART,
  LT_SOBART FOR COFIS-SOBART.

  DESCRIBE TABLE IT_COFIS LINES LD_TFILL.
  CHECK ( LD_TFILL > 1 ).

  WRITE: /, /,
         /2 'In the following list the CO line items are',
            'displayed again, but are assigned to a specific',
            'line item of the RCL document'.

  LOOP AT IT_COFIS.
    WRITE: /, /2  'RCL document:',
               30 IT_COFIS-DOCNR,
              /2  'RCL line item',
               30 IT_COFIS-DOCLN,
              /2  'RCL amount',
               30 IT_COFIS-TSL.

    REFRESH LT_BEKNZ.
    CLEAR   LT_BEKNZ.

    LT_BEKNZ-SIGN = 'I'.
    LT_BEKNZ-OPTION = 'EQ'.

    IF ( IT_COFIS-DRCRK = 'S' ).
      LT_BEKNZ-LOW = 'S'.
      APPEND LT_BEKNZ.
    ELSEIF ( IT_COFIS-DRCRK = 'H' ).
      LT_BEKNZ-LOW = 'H'.
      APPEND LT_BEKNZ.
      LT_BEKNZ-LOW = 'A'.
      APPEND LT_BEKNZ.
      LT_BEKNZ-LOW = 'L'.
      APPEND LT_BEKNZ.
    ELSE.
*     if customer has modified FM K_COST_LEDR_BEKNZ_CONVERT:
      LT_BEKNZ-LOW = IT_COFIS-DRCRK.
      APPEND LT_BEKNZ.
    ENDIF.

    REFRESH LT_ROBART.
    CLEAR   LT_ROBART.

    LT_ROBART-SIGN = 'I'.
    LT_ROBART-OPTION = 'EQ'.

    IF ( IT_COFIS-ROBART = 'KL' ) OR
       ( IT_COFIS-ROBART = 'KS' ).
      LT_ROBART-LOW = 'KS'.
      APPEND LT_ROBART.
      LT_ROBART-LOW = 'KL'.
      APPEND LT_ROBART.
    ELSE.
      LT_ROBART-LOW = IT_COFIS-ROBART.
      APPEND LT_ROBART.
    ENDIF.

    REFRESH LT_SOBART.
    CLEAR   LT_SOBART.

    LT_SOBART-SIGN = 'I'.
    LT_SOBART-OPTION = 'EQ'.

    IF ( IT_COFIS-SOBART = 'KL' ) OR
       ( IT_COFIS-SOBART = 'KS' ).
      LT_SOBART-LOW = 'KS'.
      APPEND LT_SOBART.
      LT_SOBART-LOW = 'KL'.
      APPEND LT_SOBART.
    ELSE.
      LT_SOBART-LOW = IT_COFIS-SOBART.
      APPEND LT_SOBART.
    ENDIF.

    REFRESH LT_COVP_HELP.
    CLEAR   LT_COVP_HELP.

    LOOP AT IT_COVP
          WHERE KOKRS     = IT_COFIS-KOKRS
          AND   PERIO     = IT_COFIS-POPER
          AND   GJAHR     = IT_COFIS-RYEAR
          AND   WRTTP     = IT_COFIS-WRTTP
          AND   VERSN     = IT_COFIS-RVERS
          AND   KSTAR     = IT_COFIS-RACCT
          AND   OBJNR(2) IN LT_ROBART
          AND   HRKFT(4)  = IT_COFIS-RHRKFT
          AND   VRGNG     = IT_COFIS-VRGNG
          AND   PAROB1(2) IN LT_SOBART
          AND   PARGB     = IT_COFIS-SBUSA
          AND   BEKNZ    IN LT_BEKNZ
          AND   MEINH     = IT_COFIS-RUNIT
          AND   BUKRS     = IT_COFIS-RBUKRS
          AND   GSBER     = IT_COFIS-RBUSA
          AND   FKBER     = IT_COFIS-RFAREA
          AND   SCOPE     = IT_COFIS-RSCOPE
          AND   LOGSYSO   = IT_COFIS-RLOGSYS
          AND   PBUKRS    = IT_COFIS-SBUKRS
          AND   PFKBER    = IT_COFIS-SFAREA
          AND   PSCOPE    = IT_COFIS-SSCOPE
          AND   LOGSYSP   = IT_COFIS-SLOGSYS.
      APPEND IT_COVP TO LT_COVP_HELP.
    ENDLOOP.
    PERFORM DISPLAY_COVP
            TABLES LT_COVP_HELP.
  ENDLOOP.

ENDFORM.
