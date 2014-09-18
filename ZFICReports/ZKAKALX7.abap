*&---------------------------------------------------------------------*
*& Report  ZKAKALX7
*&---------------------------------------------------------------------*
* A short documentation for this report:
* This report helps you to find all CO line items where
* business area and partner business area     or
* functional area and partner functional area or
* company code and partner company code
* are inconsistent.
*
* This report reads all CO line items that meet the
* selection criteria you specified on the selection screen
* and checks wether there are CO documents where business areas
* and partner business areas do not match.
*
* For example, the line items of a CO internal reposting
* of costs can be as follows:
*
* Document no.   Line item   business area   Partner business area
* 1000000101     001            1111               2222
* 1000000101     002            2222               2222(instead of 1111)
*
* Such CO documents are detected by this report and shown in a
* final list.
* CO line items where this report had problems to find the cross entry,
* are marked with an 'X' in the first row.
*
* Only CO line items with coep-wrttp = '04' are read for technical
* reasons.
* Please check with help of report 'RKABSHOW' or transaction 'SE16'
* if there are statistical line items belonging to the listed
* CO documents where the business area or partner business area
* is wrong as in the line items with wrttp = '04'.
*
* If the text-elements were not translated:
* The name of the parameters corresponds to the field names of
* table coep.
*  -> COAREA   =  Controlling area
*  -> YEAR     =  fiscal year
*  -> PERIOD   =  period
*  -> DOCNR    =  CO document number

* If the parameter RECDOCS is switched off, then the system selects and
* checks all CO line items (what should be the default).
* If the parameter is switched on, then only those line items will
* be selected which are cross-company code or coss-business area
* or cross-functional area.
* Both values of the parameter should yield the same result.

* if the flag ARCHVD is switched on, then also documents are checked,
* where some line items might have been archived.

REPORT  ZKAKALX7.

TABLES:
    COEP, COBK.

*------------- global data ---------------------------------------------
DATA:
      LT_COEP  LIKE  COEP OCCURS 0 WITH HEADER LINE.

DATA:
  LD_KOKRS_DELBZ_OLD  LIKE  COEP-KOKRS,
  LD_BELNR_DELBZ_OLD  LIKE  COEP-BELNR,
  LD_DELBZ            LIKE  COBK-DELBZ.

DATA: BEGIN OF LT_COEP_ERROR_COCODE OCCURS 0,
        KOKRS  LIKE COEP-KOKRS,
        BELNR  LIKE COEP-BELNR,
      END OF LT_COEP_ERROR_COCODE,

      BEGIN OF LT_COEP_ERROR_BUS_AREA OCCURS 0,
        KOKRS  LIKE COEP-KOKRS,
        BELNR  LIKE COEP-BELNR,
      END OF LT_COEP_ERROR_BUS_AREA,

      BEGIN OF LT_COEP_ERROR_FAREA OCCURS 0,
        KOKRS  LIKE COEP-KOKRS,
        BELNR  LIKE COEP-BELNR,
      END OF LT_COEP_ERROR_FAREA,

      BEGIN OF LT_COEP_ERROR_BALANCE OCCURS 0,
        KOKRS  LIKE COEP-KOKRS,
        BELNR  LIKE COEP-BELNR,
      END OF LT_COEP_ERROR_BALANCE,

      BEGIN OF LT_COEP_ERROR_OCURRBAL OCCURS 0,
        KOKRS  LIKE COEP-KOKRS,
        BELNR  LIKE COEP-BELNR,
      END OF LT_COEP_ERROR_OCURRBAL,

      BEGIN OF LT_COEP_ERROR_BALANCE2 OCCURS 0,
        KOKRS  LIKE COEP-KOKRS,
        BELNR  LIKE COEP-BELNR,
      END OF LT_COEP_ERROR_BALANCE2,

      BEGIN OF LT_BALANCE OCCURS 0,
        WKGBTR LIKE  COEP-WKGBTR,
      END OF LT_BALANCE,

      BEGIN OF LT_BALANCE2 OCCURS 0,
        BUKRS  LIKE  COEP-BUKRS,
        PBUKRS LIKE  COEP-BUKRS,
        WKGBTR LIKE  COEP-WKGBTR,
      END OF LT_BALANCE2,

      BEGIN OF LT_OCURRBALANCE OCCURS 0,
        WOGBTR LIKE  COEP-WKGBTR,
        OWAER  LIKE  COEP-OWAER,
      END OF LT_OCURRBALANCE,

      BEGIN OF LT_COCODE_BALANCE OCCURS 0,
        BUKRS  LIKE  COEP-BUKRS,
        WKGBTR LIKE  COEP-WKGBTR,
      END OF LT_COCODE_BALANCE,

      BEGIN OF LT_BUS_AREA_BALANCE OCCURS 0,
        GSBER  LIKE  COEP-GSBER,
        WKGBTR LIKE  COEP-WKGBTR,
      END OF LT_BUS_AREA_BALANCE,

      BEGIN OF LT_FAREA_BALANCE OCCURS 0,
        FKBER  LIKE  COEP-FKBER,
        WKGBTR LIKE  COEP-WKGBTR,
      END OF LT_FAREA_BALANCE.

DATA:
      LS_TKA01               LIKE  TKA01,
      LD_SINGLE_OWAER_IN_CC  LIKE  BOOLE-BOOLE,
      LD_MULT_OWAER_IN_DOC   LIKE  BOOLE-BOOLE,
      LS_T001O               LIKE  T001,
      LS_T001P               LIKE  T001,
      LD_OWAER               LIKE  COEP-OWAER,
      LD_CURSOR              TYPE  CURSOR,
      LD_OLD_KOKRS           LIKE  COEP-KOKRS,
      LD_OLD_BELNR           LIKE  COEP-BELNR.

*------------- selection screen ----------------------------------------
SELECTION-SCREEN SKIP 2.
SELECTION-SCREEN BEGIN OF BLOCK A.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 3(31) GD_CTXT FOR FIELD COAREA.
PARAMETERS:
  COAREA LIKE COEP-KOKRS  MEMORY ID CAC OBLIGATORY.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 3(31) GD_YTXT FOR FIELD YEAR.
PARAMETERS:
  YEAR   LIKE COEP-GJAHR  MEMORY ID GJR OBLIGATORY.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 3(31) GD_PTXT FOR FIELD PERIOD.
PARAMETERS:
  PERIOD LIKE COEP-PERIO  MEMORY ID VPE OBLIGATORY.
SELECTION-SCREEN END OF LINE.
* selection-screen comment 3(27) gd_ntxt for field docnr.
DATA:
GD_NTXT  TYPE C.
SELECT-OPTIONS:
  DOCNR  FOR  COEP-BELNR  MEMORY ID BLN.

SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 3(32) GD_RTXT FOR FIELD RECDOCS.
PARAMETERS:
* RECDOCS  TYPE  RCL_CHECK_ALL_CODOCS  DEFAULT ' '.
  RECDOCS  LIKE  BOOLE-BOOLE  DEFAULT ' '.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 3(32) GD_DTXT FOR FIELD ARCHVD.
PARAMETERS:
*   ARCHVD   TYPE  RCL_CHECK_DELBZ  DEFAULT 'X'.
  ARCHVD   LIKE  BOOLE-BOOLE  DEFAULT 'X'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK A.

*=======================================================================
INITIALIZATION.
*=======================================================================

  GET PARAMETER ID 'CAC' FIELD COAREA.
  GET PARAMETER ID 'GJR' FIELD YEAR.
  GET PARAMETER ID 'VPE' FIELD PERIOD.

* selct option texts:
  GD_NTXT = 'CO document number'(500).
  GD_CTXT = 'Controlling area'(600).
  GD_YTXT = 'Year'(700).
  GD_PTXT = 'Period'(800).

* flags on selection screen:
  GD_RTXT = 'Only reconcil. relev. docs'(300).
  GD_DTXT = 'Also partly archived docs'(400).

*=======================================================================
AT SELECTION-SCREEN.
*=======================================================================

  SET PARAMETER ID 'CAC' FIELD COAREA.
  SET PARAMETER ID 'GJR' FIELD YEAR.
  SET PARAMETER ID 'VPE' FIELD PERIOD.

*=======================================================================
START-OF-SELECTION.
*=======================================================================

* first check if different CO object currencies are allowed:
  CALL FUNCTION 'K_KOKRS_READ'
       EXPORTING
            KOKRS   = COAREA
       IMPORTING
            E_TKA01 = LS_TKA01.

* check consistency of co object currencies:
  IF ( LS_TKA01-XWBUK = SPACE ).
*   all company codes of a controlling area must have the same
*   currency;
*   no unique object currency exists:
    CLEAR LD_SINGLE_OWAER_IN_CC.
  ELSE.
*   all CO objects of a cotrolling area must have the same
*   currency:
    LD_SINGLE_OWAER_IN_CC = 'X'.
  ENDIF.

  IF ( RECDOCS = ' ' ).
    OPEN CURSOR WITH HOLD LD_CURSOR FOR
    SELECT * FROM COEP
              WHERE  KOKRS    = COAREA
              AND    GJAHR    = YEAR
              AND    PERIO    = PERIOD
              AND    VERSN    = '000'
              AND    WRTTP    = '04'
              AND    BELNR   IN DOCNR
              AND    PAROB1  <> SPACE
              ORDER BY PRIMARY KEY.
  ELSE.
*   select only CO documents which are  relevant for reconciliation
*   posting:
    OPEN CURSOR WITH HOLD LD_CURSOR FOR
    SELECT * FROM COEP
              WHERE  KOKRS    = COAREA
              AND    GJAHR    = YEAR
              AND    PERIO    = PERIOD
              AND    VERSN    = '000'
              AND    WRTTP    = '04'
              AND    PAROB1   <> SPACE
              AND    VRGNG    <> 'COIN'
              AND    VRGNG    <> 'COIE'
              AND    ( COEP~BUKRS <> COEP~PBUKRS OR
                       COEP~GSBER <> COEP~PARGB  OR
                       COEP~FKBER <> COEP~PFKBER     )
              AND    BELNR   IN DOCNR
              AND    PAROB1  <> SPACE
              ORDER BY PRIMARY KEY.
  ENDIF.

  DO.
    REFRESH LT_COEP.
    FETCH NEXT CURSOR LD_CURSOR INTO TABLE LT_COEP
          PACKAGE SIZE '1000'.

    IF ( SY-SUBRC <> 0 ).
      CLOSE CURSOR LD_CURSOR.
      EXIT.
    ENDIF.

    PERFORM COMPLETE_FIRST_DOCUMENT
            TABLES LT_COEP.

    PERFORM COMPLETE_LAST_DOCUMENT
            TABLES LT_COEP.

    SORT LT_COEP BY KOKRS BELNR BUZEI .

    REFRESH  LT_COCODE_BALANCE.
    REFRESH  LT_BUS_AREA_BALANCE.
    REFRESH  LT_FAREA_BALANCE.
    REFRESH  LT_BALANCE.
    REFRESH  LT_BALANCE2.
    REFRESH LT_OCURRBALANCE.
    CLEAR LD_MULT_OWAER_IN_DOC.

    LOOP AT  LT_COEP.

      IF ( LT_COEP-KOKRS <> LD_KOKRS_DELBZ_OLD ) OR
         ( LT_COEP-BELNR <> LD_BELNR_DELBZ_OLD ) AND
         ( ARCHVD  = ' ' ).   "do not check partly archived docum.

*       check whether this is an partly archived CO document:
        SELECT SINGLE DELBZ FROM COBK INTO LD_DELBZ
               WHERE KOKRS = LT_COEP-KOKRS
               AND   BELNR = LT_COEP-BELNR.

        LD_KOKRS_DELBZ_OLD = LT_COEP-KOKRS.
        LD_BELNR_DELBZ_OLD = LT_COEP-BELNR.

*       check only documents where no line items were archived:
        CHECK ( LD_DELBZ = 0 ).
      ENDIF.

      PERFORM READ_COMP_CODE_MASTER_DATA
              USING     LT_COEP-BUKRS
                        LT_COEP-PBUKRS
                        LS_TKA01-XWBUK
              CHANGING  LS_T001O
                        LS_T001P.

      IF ( LS_T001O-WAERS <> LS_T001P-WAERS ) AND
         ( LS_T001P-BUKRS <> SPACE          ).
*       multiple company codes exist in this document with
*       different currencies:
        LD_MULT_OWAER_IN_DOC = 'X'.
      ENDIF.

      IF ( LT_COEP-KOKRS NE LD_OLD_KOKRS ) OR
         ( LT_COEP-BELNR NE LD_OLD_BELNR ).
*       new document, so check if old one is consistent:
        PERFORM CHECK_COCODE
                TABLES  LT_COCODE_BALANCE
                        LT_COEP_ERROR_COCODE
                USING   LD_OLD_KOKRS
                        LD_OLD_BELNR.
        PERFORM CHECK_BUSA
                TABLES  LT_BUS_AREA_BALANCE
                        LT_COEP_ERROR_BUS_AREA
                USING   LD_OLD_KOKRS
                        LD_OLD_BELNR.
        PERFORM CHECK_FAREA
                TABLES  LT_FAREA_BALANCE
                        LT_COEP_ERROR_FAREA
                USING   LD_OLD_KOKRS
                        LD_OLD_BELNR.
        PERFORM CHECK_BLCE
                TABLES  LT_BALANCE
                        LT_COEP_ERROR_BALANCE
                USING   LD_OLD_KOKRS
                        LD_OLD_BELNR.
        PERFORM CHECK_BLC2
                TABLES  LT_BALANCE2
                        LT_COEP_ERROR_BALANCE2
                USING   LD_OLD_KOKRS
                        LD_OLD_BELNR.
        PERFORM CHECK_OCURR_BALANCE
                TABLES  LT_OCURRBALANCE
                        LT_COEP_ERROR_OCURRBAL
                USING   LD_OLD_KOKRS
                        LD_OLD_BELNR
                        LD_SINGLE_OWAER_IN_CC
                        LD_MULT_OWAER_IN_DOC.

*       delete old documents:
        REFRESH LT_COCODE_BALANCE.
        REFRESH LT_BUS_AREA_BALANCE.
        REFRESH LT_FAREA_BALANCE.
        REFRESH LT_BALANCE.
        REFRESH LT_BALANCE2.
        REFRESH LT_OCURRBALANCE.
        CLEAR LD_MULT_OWAER_IN_DOC.

*       add the new document:
        PERFORM ADD_DOC_BUKRS
                TABLES   LT_COEP
                         LT_COCODE_BALANCE.
        PERFORM ADD_DOC_BUSA
                TABLES   LT_COEP
                         LT_BUS_AREA_BALANCE.
        PERFORM ADD_DOC_FAREA
                TABLES   LT_COEP
                         LT_FAREA_BALANCE.
        PERFORM ADD_DOC_BALANCE
                TABLES   LT_COEP
                         LT_BALANCE.
        PERFORM ADD_DOC_BALANCE2
                TABLES   LT_COEP
                         LT_BALANCE2.
        PERFORM ADD_DOC_OCURRBALANCE
                TABLES   LT_COEP
                         LT_OCURRBALANCE.

        LD_OLD_KOKRS = LT_COEP-KOKRS.
        LD_OLD_BELNR = LT_COEP-BELNR.
      ELSE.
        PERFORM ADD_DOC_BUKRS
                TABLES   LT_COEP
                         LT_COCODE_BALANCE.
        PERFORM ADD_DOC_BUSA
                TABLES   LT_COEP
                         LT_BUS_AREA_BALANCE.
        PERFORM ADD_DOC_FAREA
                TABLES   LT_COEP
                         LT_FAREA_BALANCE.
        PERFORM ADD_DOC_BALANCE
                TABLES   LT_COEP
                         LT_BALANCE.
        PERFORM ADD_DOC_BALANCE2
                TABLES   LT_COEP
                         LT_BALANCE2.
        PERFORM ADD_DOC_OCURRBALANCE
                TABLES   LT_COEP
                         LT_OCURRBALANCE.
      ENDIF.
    ENDLOOP.

*   check last document:
    PERFORM CHECK_COCODE
            TABLES  LT_COCODE_BALANCE
                    LT_COEP_ERROR_COCODE
            USING   LD_OLD_KOKRS
                    LD_OLD_BELNR.
    PERFORM CHECK_BUSA
            TABLES  LT_BUS_AREA_BALANCE
                    LT_COEP_ERROR_BUS_AREA
            USING   LD_OLD_KOKRS
                    LD_OLD_BELNR.
    PERFORM CHECK_FAREA
            TABLES  LT_FAREA_BALANCE
                    LT_COEP_ERROR_FAREA
            USING   LD_OLD_KOKRS
                    LD_OLD_BELNR.
    PERFORM CHECK_BLCE
            TABLES  LT_BALANCE
                    LT_COEP_ERROR_BALANCE
            USING   LD_OLD_KOKRS
                    LD_OLD_BELNR.
    PERFORM CHECK_BLC2
            TABLES  LT_BALANCE2
                    LT_COEP_ERROR_BALANCE2
            USING   LD_OLD_KOKRS
                    LD_OLD_BELNR.
    PERFORM CHECK_OCURR_BALANCE
            TABLES  LT_OCURRBALANCE
                    LT_COEP_ERROR_OCURRBAL
            USING   LD_OLD_KOKRS
                    LD_OLD_BELNR
                    LD_SINGLE_OWAER_IN_CC
                    LD_MULT_OWAER_IN_DOC.
  ENDDO.

*=======================================================================
END-OF-SELECTION.
*=======================================================================

* avoid that a document is displayed in different lists:
  LOOP AT LT_COEP_ERROR_BALANCE.

    DELETE  LT_COEP_ERROR_BALANCE2
            WHERE KOKRS = LT_COEP_ERROR_BALANCE-KOKRS
            AND   BELNR = LT_COEP_ERROR_BALANCE-BELNR.

    DELETE  LT_COEP_ERROR_OCURRBAL
            WHERE KOKRS = LT_COEP_ERROR_BALANCE-KOKRS
            AND   BELNR = LT_COEP_ERROR_BALANCE-BELNR.

    DELETE  LT_COEP_ERROR_COCODE
            WHERE KOKRS = LT_COEP_ERROR_BALANCE-KOKRS
            AND   BELNR = LT_COEP_ERROR_BALANCE-BELNR.

    DELETE  LT_COEP_ERROR_BUS_AREA
            WHERE KOKRS = LT_COEP_ERROR_BALANCE-KOKRS
            AND   BELNR = LT_COEP_ERROR_BALANCE-BELNR.

    DELETE  LT_COEP_ERROR_FAREA
            WHERE KOKRS = LT_COEP_ERROR_BALANCE-KOKRS
            AND   BELNR = LT_COEP_ERROR_BALANCE-BELNR.

  ENDLOOP.

*-----------------------------------------------------------------------
  WRITE: / 'The following Lists contain CO documents'(011),
           'which are inconsistent'(012),
           'If no line items of these documents have been '(013),
           'archived, then they must be corrected by SAP'(014).

  WRITE: /.

  WRITE: / 'Documents with balance <> zero'(020).

  WRITE: /.

  PERFORM DISPLAY_LIST
          TABLES   LT_COEP_ERROR_BALANCE.

*-----------------------------------------------------------------------

  WRITE: /.

  WRITE: / 'Documents with inconsistent object currencies'(025).

  WRITE: /.

  PERFORM DISPLAY_LIST
          TABLES   LT_COEP_ERROR_OCURRBAL.

*-----------------------------------------------------------------------

  WRITE: / 'Documents with company code balance <> zero'(030).

  WRITE: /.

  LOOP AT LT_COEP_ERROR_BALANCE2.
    MOVE-CORRESPONDING LT_COEP_ERROR_BALANCE2 TO
                       LT_COEP_ERROR_COCODE.
    APPEND LT_COEP_ERROR_COCODE.
  ENDLOOP.

  SORT LT_COEP_ERROR_BALANCE.
  DELETE ADJACENT DUPLICATES FROM LT_COEP_ERROR_BALANCE.

  PERFORM DISPLAY_LIST
          TABLES   LT_COEP_ERROR_COCODE.

*-----------------------------------------------------------------------
  WRITE: / 'Documents with business area balance <> zero'(040).

  WRITE: /.

  PERFORM DISPLAY_LIST
          TABLES   LT_COEP_ERROR_BUS_AREA.

*-----------------------------------------------------------------------
  WRITE: / 'Documents with functional area balance <> zero'(050).

  WRITE: /.

  PERFORM DISPLAY_LIST
          TABLES   LT_COEP_ERROR_FAREA.


*=======================================================================
* Forms
*=======================================================================

*&---------------------------------------------------------------------*
*&      Form  complete_first_document
*&---------------------------------------------------------------------*
FORM COMPLETE_FIRST_DOCUMENT                                "P00K001821
     TABLES   CT_COEP STRUCTURE COEP.

  DATA:
  LS_COEP             LIKE  COEP,
  LT_COEP_SUPPLEMENT  LIKE  COEP OCCURS 0 WITH HEADER LINE.

  READ TABLE CT_COEP INDEX 1 INTO LS_COEP.

* check if some records had been selected:
  CHECK ( SY-SUBRC = 0 ).

  SELECT * FROM COEP INTO TABLE LT_COEP_SUPPLEMENT
                     WHERE KOKRS = LS_COEP-KOKRS
                     AND   BELNR = LS_COEP-BELNR
                     AND   GJAHR    = YEAR
                     AND   PERIO    = PERIOD
                     AND   VERSN    = '000'
                     AND   WRTTP    = '04'
                     AND   PAROB1   <> SPACE
                     AND   VRGNG    <> 'COIN'
                     AND   VRGNG    <> 'COIE'
                     AND   ( COEP~BUKRS <> COEP~PBUKRS OR
                             COEP~GSBER <> COEP~PARGB  OR
                             COEP~FKBER <> COEP~PFKBER     )
                     AND   PAROB1  <> SPACE
                     ORDER BY PRIMARY KEY.

  DELETE CT_COEP WHERE KOKRS = LS_COEP-KOKRS
                 AND   BELNR = LS_COEP-BELNR.

  LOOP AT LT_COEP_SUPPLEMENT.
*   to avoid sort before last document is completed later
    INSERT LT_COEP_SUPPLEMENT INTO CT_COEP INDEX 1.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  complete_last_document
*&---------------------------------------------------------------------*
FORM COMPLETE_LAST_DOCUMENT                                 "P00K001821
     TABLES   CT_COEP STRUCTURE COEP.

  DATA:
  LS_COEP             LIKE  COEP,
  LD_COEP_TFILL       LIKE  SY-TFILL,
  LT_COEP_SUPPLEMENT  LIKE  COEP OCCURS 0 WITH HEADER LINE.

  DESCRIBE TABLE CT_COEP LINES LD_COEP_TFILL.
  READ TABLE CT_COEP INDEX LD_COEP_TFILL INTO LS_COEP.

* check if some records had been selected:
  CHECK ( SY-SUBRC = 0 ).

  SELECT * FROM COEP INTO TABLE LT_COEP_SUPPLEMENT
                     WHERE KOKRS = LS_COEP-KOKRS
                     AND   BELNR = LS_COEP-BELNR
                     AND   GJAHR    = YEAR
                     AND   PERIO    = PERIOD
                     AND   VERSN    = '000'
                     AND   WRTTP    = '04'
                     AND   PAROB1   <> SPACE
                     AND   VRGNG    <> 'COIN'
                     AND   VRGNG    <> 'COIE'
                     AND   ( COEP~BUKRS <> COEP~PBUKRS OR
                             COEP~GSBER <> COEP~PARGB  OR
                             COEP~FKBER <> COEP~PFKBER     )
                     AND   PAROB1  <> SPACE
                     ORDER BY PRIMARY KEY.

  DELETE CT_COEP WHERE KOKRS = LS_COEP-KOKRS
                 AND   BELNR = LS_COEP-BELNR.

  LOOP AT LT_COEP_SUPPLEMENT.
    APPEND LT_COEP_SUPPLEMENT TO CT_COEP.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  display_list
*&---------------------------------------------------------------------*
FORM DISPLAY_LIST
     TABLES   IT_CODOC   STRUCTURE LT_COEP_ERROR_BUS_AREA.

  WRITE: / SY-ULINE(22).

  WRITE: /1       '|',
          2(7)    'CO AREA'(009),
          10       '|',
          11(10)   'DOCNR'(001),
          22      '|'.

  WRITE: / SY-ULINE(22).

  LOOP AT IT_CODOC.
    WRITE: /1      '|',
            2(7)  IT_CODOC-KOKRS,
                   '|',
            11(10) IT_CODOC-BELNR,
                   '|'.
  ENDLOOP.

  IF ( IT_CODOC[] IS INITIAL ).
    WRITE: / 'List contains no data'(100).
  ENDIF.

  WRITE: / SY-ULINE(22).
  WRITE: /, /, /, /.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_BUSA
*&---------------------------------------------------------------------*
FORM CHECK_BUSA
     TABLES   CT_BUS_AREA_BALANCE    STRUCTURE LT_BUS_AREA_BALANCE
              CT_COEP_ERROR_BUS_AREA STRUCTURE LT_COEP_ERROR_BUS_AREA
     USING    ID_OLD_KOKRS           LIKE      COEP-KOKRS
              ID_OLD_BELNR           LIKE      COEP-BELNR.

  DELETE CT_BUS_AREA_BALANCE WHERE WKGBTR = 0.

  IF ( NOT CT_BUS_AREA_BALANCE[] IS INITIAL ).
    CT_COEP_ERROR_BUS_AREA-KOKRS = ID_OLD_KOKRS.
    CT_COEP_ERROR_BUS_AREA-BELNR = ID_OLD_BELNR.
    APPEND CT_COEP_ERROR_BUS_AREA.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM CHECK_CODODE
*---------------------------------------------------------------------*
FORM CHECK_COCODE
     TABLES   CT_COCODE_BALANCE      STRUCTURE LT_COCODE_BALANCE
              CT_COEP_ERROR_COCODE   STRUCTURE LT_COEP_ERROR_COCODE
     USING    ID_OLD_KOKRS           LIKE      COEP-KOKRS
              ID_OLD_BELNR           LIKE      COEP-BELNR.

  DELETE CT_COCODE_BALANCE WHERE WKGBTR = 0.

  IF ( NOT CT_COCODE_BALANCE[] IS INITIAL ).
    CT_COEP_ERROR_COCODE-KOKRS = ID_OLD_KOKRS.
    CT_COEP_ERROR_COCODE-BELNR = ID_OLD_BELNR.
    APPEND CT_COEP_ERROR_COCODE.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM CHECK_FAREA                                              *
*---------------------------------------------------------------------*
FORM CHECK_FAREA
     TABLES   CT_FAREA_BALANCE       STRUCTURE LT_FAREA_BALANCE
              CT_COEP_ERROR_FAREA    STRUCTURE LT_COEP_ERROR_FAREA
     USING    ID_OLD_KOKRS           LIKE      COEP-KOKRS
              ID_OLD_BELNR           LIKE      COEP-BELNR.

  DELETE CT_FAREA_BALANCE WHERE WKGBTR = 0.

  IF ( NOT CT_FAREA_BALANCE[] IS INITIAL ).
    CT_COEP_ERROR_FAREA-KOKRS = ID_OLD_KOKRS.
    CT_COEP_ERROR_FAREA-BELNR = ID_OLD_BELNR.
    APPEND CT_COEP_ERROR_FAREA.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM CHECK_BLCE                                               *
*---------------------------------------------------------------------*
FORM CHECK_BLCE
     TABLES   CT_BALANCE             STRUCTURE LT_BALANCE
              CT_COEP_ERROR_BALANCE  STRUCTURE LT_COEP_ERROR_BALANCE
     USING    ID_OLD_KOKRS           LIKE      COEP-KOKRS
              ID_OLD_BELNR           LIKE      COEP-BELNR.

  DELETE CT_BALANCE WHERE WKGBTR = 0.

  IF ( NOT CT_BALANCE[] IS INITIAL ).
    CT_COEP_ERROR_BALANCE-KOKRS = ID_OLD_KOKRS.
    CT_COEP_ERROR_BALANCE-BELNR = ID_OLD_BELNR.
    APPEND CT_COEP_ERROR_BALANCE.
  ENDIF.

ENDFORM.


*---------------------------------------------------------------------*
*       FORM add_doc_BUSA                                             *
*---------------------------------------------------------------------*
FORM ADD_DOC_BUSA
     TABLES   IT_COEP                STRUCTURE LT_COEP
              CT_BUS_AREA_BALANCE    STRUCTURE LT_BUS_AREA_BALANCE.

* object:
  MOVE-CORRESPONDING IT_COEP TO CT_BUS_AREA_BALANCE.
  COLLECT CT_BUS_AREA_BALANCE.
* partner:
  CT_BUS_AREA_BALANCE-GSBER  = IT_COEP-PARGB.
  CT_BUS_AREA_BALANCE-WKGBTR = IT_COEP-WKGBTR.
  COLLECT CT_BUS_AREA_BALANCE.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM ADD_DOC_BUKRS                                            *
*---------------------------------------------------------------------*
FORM ADD_DOC_BUKRS
     TABLES   IT_COEP                STRUCTURE COEP
              CT_COCODE_BALANCE      STRUCTURE LT_COCODE_BALANCE.

* object:
  MOVE-CORRESPONDING IT_COEP TO CT_COCODE_BALANCE.
  COLLECT CT_COCODE_BALANCE.

* partner:
  CT_COCODE_BALANCE-BUKRS   = IT_COEP-PBUKRS.
  CT_COCODE_BALANCE-WKGBTR  = IT_COEP-WKGBTR.
  COLLECT CT_COCODE_BALANCE.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM ADD_DOC_FAREA                                            *
*---------------------------------------------------------------------*
FORM ADD_DOC_FAREA
     TABLES   IT_COEP             STRUCTURE LT_COEP
              CT_FAREA_BALANCE    STRUCTURE LT_FAREA_BALANCE.

* object:
  MOVE-CORRESPONDING IT_COEP TO CT_FAREA_BALANCE.
  COLLECT CT_FAREA_BALANCE.
* partner:
  CT_FAREA_BALANCE-FKBER  = IT_COEP-PFKBER.
  CT_FAREA_BALANCE-WKGBTR = IT_COEP-WKGBTR.
  COLLECT CT_FAREA_BALANCE.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM ADD_DOC_balance                                          *
*---------------------------------------------------------------------*
FORM ADD_DOC_BALANCE
     TABLES   IT_COEP        STRUCTURE LT_COEP
              CT_BALANCE     STRUCTURE LT_BALANCE.

* object:
  MOVE-CORRESPONDING IT_COEP TO CT_BALANCE.
  COLLECT CT_BALANCE.
* partner:
  CT_BALANCE-WKGBTR = IT_COEP-WKGBTR.
  COLLECT CT_BALANCE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CHECK_BLC2
*&---------------------------------------------------------------------*
FORM CHECK_BLC2
     TABLES   CT_BALANCE2            STRUCTURE LT_BALANCE2
              CT_COEP_ERROR_BALANCE  STRUCTURE LT_COEP_ERROR_BALANCE
     USING    ID_OLD_KOKRS           LIKE      COEP-KOKRS
              ID_OLD_BELNR           LIKE      COEP-BELNR.

  DATA:
  LD_WKGBTR  LIKE  COEP-WKGBTR.

  CLEAR: LD_WKGBTR.

  LOOP AT CT_BALANCE2.
    LD_WKGBTR = LD_WKGBTR + LT_BALANCE2-WKGBTR.
  ENDLOOP.

  IF ( NOT LD_WKGBTR IS INITIAL ).
    CT_COEP_ERROR_BALANCE-KOKRS = ID_OLD_KOKRS.
    CT_COEP_ERROR_BALANCE-BELNR = ID_OLD_BELNR.
    APPEND CT_COEP_ERROR_BALANCE.
*   refresh only after an error occurred:
    REFRESH LT_BALANCE2.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ADD_DOC_BALANCE2
*&---------------------------------------------------------------------*
FORM ADD_DOC_BALANCE2
     TABLES   IT_COEP        STRUCTURE LT_COEP
              CT_BALANCE2    STRUCTURE LT_BALANCE2.

* object:
  MOVE-CORRESPONDING IT_COEP TO CT_BALANCE2.
* partner:
  CT_BALANCE2-PBUKRS = IT_COEP-PBUKRS.
  CT_BALANCE2-WKGBTR = IT_COEP-WKGBTR.
  COLLECT CT_BALANCE2.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  READ_COMP_CODE_MASTER_DATA
*&---------------------------------------------------------------------*
FORM READ_COMP_CODE_MASTER_DATA
     USING    ID_BUKRS   LIKE  COEP-BUKRS
              ID_PBUKRS  LIKE  COEP-PBUKRS
              ID_XWBUK   LIKE  TKA01-XWBUK
     CHANGING CS_T001O   LIKE  T001
              CS_T001P   LIKE  T001.

* determine master data of company codes:
  IF ( ID_BUKRS <> CS_T001O-BUKRS ).
*   new company code:
    CALL FUNCTION 'FI_COMPANY_CODE_DATA'
         EXPORTING
              I_BUKRS = ID_BUKRS
         IMPORTING
              E_T001  = CS_T001O.
  ENDIF.

  IF ( ID_PBUKRS  <> CS_T001P  ) AND
     ( ID_PBUKRS  <> SPACE     ).      "allocation!
*   new partner company code:
    CALL FUNCTION 'FI_COMPANY_CODE_DATA'
         EXPORTING
              I_BUKRS = ID_PBUKRS
         IMPORTING
              E_T001  = CS_T001P.
  ENDIF.

* check consistency of co object currencies:
  IF ( ID_XWBUK = SPACE ).
*   all company codes of a controlling area must have the same
*   currency:
    IF ( CS_T001O-WAERS <> CS_T001P-WAERS ) AND
       ( CS_T001P-BUKRS <> SPACE          ).
*     error: company codes must have same currency:
      MESSAGE E001(K5) WITH 'CO-Document:' LT_COEP-BELNR
                            'Different company code currencies'
                            'in table T001!'.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CHECK_LT_COEP_ERROR_OCURRBAL
*&---------------------------------------------------------------------*
FORM CHECK_OCURR_BALANCE
     TABLES   CT_OCURRBALANCE         STRUCTURE  LT_OCURRBALANCE
              CT_COEP_ERROR_OCURRBAL  STRUCTURE  LT_COEP_ERROR_OCURRBAL
     USING    ID_OLD_KOKRS            LIKE       COEP-KOKRS
              ID_OLD_BELNR            LIKE       COEP-BELNR
              ID_SINGLE_OWAER_IN_CC   LIKE       BOOLE-BOOLE
              ID_MULT_OWAER_IN_DOC    LIKE       BOOLE-BOOLE.

  DATA:
  LD_TFILL   LIKE  SY-TFILL,
  LD_WOGBTR  LIKE  COEP-WOGBTR.

  IF ( ID_MULT_OWAER_IN_DOC = SPACE ).
*   only one object currency should be in document:
    DESCRIBE TABLE  CT_OCURRBALANCE  LINES  LD_TFILL.
    IF ( LD_TFILL > 1 ).
*     only one ocurr. allowed, but more than one object
*     currency found in document:
      MESSAGE S001(K5) WITH 'Multiple object currencies in'
                            'CO document nr.' ID_OLD_BELNR.
      CT_COEP_ERROR_OCURRBAL-KOKRS = ID_OLD_KOKRS.
      CT_COEP_ERROR_OCURRBAL-BELNR = ID_OLD_BELNR.
      APPEND CT_COEP_ERROR_OCURRBAL.
    ELSE.
*     only one ocurr. allowed and only one was found in document;
*     check if the value balances to zero:
      DELETE CT_OCURRBALANCE WHERE WOGBTR = 0.
      IF ( NOT CT_OCURRBALANCE[] IS INITIAL ).
        CT_COEP_ERROR_OCURRBAL-KOKRS = ID_OLD_KOKRS.
        CT_COEP_ERROR_OCURRBAL-BELNR = ID_OLD_BELNR.
        APPEND CT_COEP_ERROR_OCURRBAL.
      ENDIF.
    ENDIF.
  ELSE.
*   commented out, because there can be cross-company allocations and
*   the company codes have different local currencies;
*   in this case, the balances in CT_OCURRBALANCE are not zero,
*   since there are no company code clearing line items in the
*   CO document:
*   multiple object currencies in document found:
*   IF ( ID_SINGLE_OWAER_IN_CC <> SPACE ).
*     multiple o curr. found, but there may be only one object currency:
*     MESSAGE S001(K5) WITH 'Multiple object currencies in'
*                           'CO document nr.' ID_OLD_BELNR.
*     CT_COEP_ERROR_OCURRBAL-KOKRS = ID_OLD_KOKRS.
*     CT_COEP_ERROR_OCURRBAL-BELNR = ID_OLD_BELNR.
*     APPEND CT_COEP_ERROR_OCURRBAL.
*   ELSE.
*     multiple obj. curr. found, which is allowed due to cust.:
*     do not check this!
*   ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  add_doc_ocurrbalance
*&---------------------------------------------------------------------*
FORM ADD_DOC_OCURRBALANCE
     TABLES   IT_COEP           STRUCTURE  LT_COEP
              CT_OCURRBALANCE   STRUCTURE  LT_OCURRBALANCE.

  MOVE-CORRESPONDING IT_COEP TO CT_OCURRBALANCE.
  COLLECT CT_OCURRBALANCE.

ENDFORM.
