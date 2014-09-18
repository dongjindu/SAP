*&---------------------------------------------------------------------*
*& Report  ZKAKALX9                                                    *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  ZKAKALX9.

* documentation:
* this report selects some CO line items and sums them up.
* this sum can be compared e.g. with reconciliation ledger summary
* records (table COFIT) or CO summary records (tables COSP and COSS).
* if the names of the selection parameters were not translated,
* please find hear the description of the paramters:

* COAREA     controlling area
* YEAR       fiscal year
* PERIOD     period

* DOCNR      CO document number
* LINE       CO line item
* COSTELEM   cost element
* TRANSACT   business transaction
* VERSION    version

* RCOMPCDE   receiver company code
* RBUSA      receiver business area
* RFAREA     receiver functional area
* ROBART     receiver object type
* RSCOPE     receiver object class
* OBJECT     object number of receiver CO object
* RLOGSYS    logical system of receiver

* SCOMPCDE   sender company code
* SBUSA      sender business area
* SFAREA     sender functional area
* SOBART     sender object type
* SSCOPE     sender object class
* POBJECT    object number of sender CO object
* SLOGSYS    logical system of sender

* DISPLIST   flag: display no only sums but also line items

* ======================================================================
* global data.
* ======================================================================
TABLES:
COEP, COFIT.

DATA:
 LD_CURSOR       TYPE  CURSOR,
 LD_COEP_COUNTER LIKE  SY-TABIX,
 LS_TKA01        LIKE  TKA01,
 LT_COEP         LIKE  COEP OCCURS 0 WITH HEADER LINE,
 LT_COEP_DISP    LIKE  COEP OCCURS 0 WITH HEADER LINE.

DATA:
* summary in controlling area currency:
BEGIN OF LT_COAREA_SUM OCCURS 0,
   WKGBTR  LIKE  COEP-WKGBTR,
   CURR    LIKE  TKA01-WAERS,
END OF LT_COAREA_SUM,

* sum in object currency:
BEGIN OF LT_OBJECT_SUM  OCCURS 0,
      WOGBTR  LIKE  COEP-WOGBTR,
      CURR    LIKE  COEP-OWAER,
END OF LT_OBJECT_SUM,

* sum in transaction currency:
BEGIN OF LT_TRANSACTION_SUM  OCCURS 0,
       WTGBTR  LIKE  COEP-WTGBTR,
       CURR    LIKE  COEP-TWAER,
END OF LT_TRANSACTION_SUM.

* ======================================================================
* selection screen.
* ======================================================================

SELECTION-SCREEN BEGIN OF BLOCK A WITH FRAME.
* basic paramters:
* controllimg area:
PARAMETER:
  COAREA LIKE  COEP-KOKRS MEMORY ID CAC OBLIGATORY.

* year and period:
PARAMETERS:
  YEAR   LIKE COEP-GJAHR  MEMORY ID GJR OBLIGATORY,
  PERIOD LIKE COEP-PERIO  MEMORY ID VPE OBLIGATORY.

SELECTION-SCREEN END OF BLOCK A.

SELECTION-SCREEN BEGIN OF BLOCK B WITH FRAME.

* cost element
SELECT-OPTIONS:
  DOCNR     FOR COEP-BELNR  MEMORY ID BLN,
  LINE      FOR COEP-BUZEI,
  COSTELEM  FOR COEP-KSTAR  MEMORY ID KAT,
  TRANSACT  FOR COEP-VRGNG,
  VERSION   FOR COEP-VERSN.

SELECTION-SCREEN END OF BLOCK B.

SELECTION-SCREEN BEGIN OF BLOCK C WITH FRAME.

* object (receiver) fields:
SELECT-OPTIONS:
  RCOMPCDE FOR COEP-BUKRS  MEMORY ID BUK,
  RBUSA    FOR COEP-GSBER  MEMORY ID GSB,
  RFAREA   FOR COEP-FKBER  MEMORY ID FBE,
  ROBJTYPE FOR COFIT-ROBART,
  RSCOPE   FOR COFIT-RSCOPE,
  OBJECT   FOR COEP-OBJNR,
  RLOGSYS  FOR COEP-LOGSYSO.

SELECTION-SCREEN END OF BLOCK C.

SELECTION-SCREEN BEGIN OF BLOCK D WITH FRAME.

* object (receiver) fields:
SELECT-OPTIONS:
  SCOMPCDE FOR COEP-PBUKRS  MEMORY ID BUK,
  SBUSA    FOR COEP-PARGB   MEMORY ID GSB,
  SFAREA   FOR COEP-PFKBER  MEMORY ID FBE,
  SOBJTYPE FOR COFIT-SOBART,
  SSCOPE   FOR COFIT-SSCOPE,
  POBJECT  FOR COEP-PAROB1,
  SLOGSYS  FOR COEP-LOGSYSP.

SELECTION-SCREEN END OF BLOCK D.

SELECTION-SCREEN BEGIN OF BLOCK E.

PARAMETERS:
  DISPLIST  LIKE  BOOLE-BOOLE DEFAULT 'X'.

SELECTION-SCREEN END OF BLOCK E.

*=======================================================================
INITIALIZATION.
*=======================================================================

*=======================================================================
AT SELECTION-SCREEN.
*=======================================================================

*=======================================================================
START-OF-SELECTION.
*=======================================================================

* get controlling area currency:
  CALL FUNCTION 'K_KOKRS_READ'
       EXPORTING
            KOKRS   = COAREA
       IMPORTING
            E_TKA01 = LS_TKA01.

* controlling area is the same for all documents:
  LT_COAREA_SUM-CURR = LS_TKA01-WAERS.

* select co documents:
  OPEN CURSOR WITH HOLD LD_CURSOR FOR
  SELECT * FROM COEP
            WHERE  KOKRS    =  COAREA
            AND    BELNR    IN DOCNR
            AND    BUZEI    IN LINE
            AND    GJAHR    =  YEAR
            AND    PERIO    =  PERIOD
            AND    WRTTP    =  '04'
            AND    VERSN    IN VERSION
            AND    VRGNG    IN TRANSACT
            AND    BUKRS    IN RCOMPCDE
            AND    PBUKRS   IN SCOMPCDE
            AND    GSBER    IN RBUSA
            AND    PARGB    IN SBUSA
            AND    FKBER    IN RFAREA
            AND    PFKBER   IN SFAREA
            AND    LOGSYSO  IN RLOGSYS
            AND    LOGSYSP  IN SLOGSYS
            AND    OBJNR    IN OBJECT
            AND    PAROB1   IN POBJECT
            AND    KSTAR    IN COSTELEM
            ORDER BY PRIMARY KEY.

  DO.
    REFRESH LT_COEP.
    FETCH NEXT CURSOR LD_CURSOR INTO TABLE LT_COEP
          PACKAGE SIZE '1000'.

    IF ( SY-SUBRC <> 0 ).
      CLOSE CURSOR LD_CURSOR.
      EXIT.
    ENDIF.

    LOOP AT LT_COEP.
      ADD 1 TO LD_COEP_COUNTER.

*     controlling area currency:
      LT_COAREA_SUM-WKGBTR = LT_COEP-WKGBTR.
      COLLECT LT_COAREA_SUM.

*     object currency:
*     PERFORM COLLECT_OBJECT_SUM
*             USING     LT_COEP
*                       LS_TKA01-LMONA
*             CHANGING  LT_OBJECT_SUM.
      LT_OBJECT_SUM-CURR = LT_COEP-OWAER.
      LT_OBJECT_SUM-WOGBTR = LT_COEP-WOGBTR.
      COLLECT LT_OBJECT_SUM.

*     transaction currency:
      LT_TRANSACTION_SUM-CURR   = LT_COEP-TWAER.
      LT_TRANSACTION_SUM-WTGBTR = LT_COEP-WTGBTR.
      COLLECT LT_TRANSACTION_SUM.

    ENDLOOP.
    IF ( DISPLIST <> SPACE ).
*     store line items for later list display:
      APPEND LINES OF  LT_COEP[]  TO  LT_COEP_DISP[].
    ENDIF.
  ENDDO.

*=======================================================================
END-OF-SELECTION.
*=======================================================================

  WRITE: /, /, /.

  WRITE: 'Number of selected line items:'(100).
  WRITE AT 55: LD_COEP_COUNTER.

  WRITE: /, /, /.

* display final list:
  PERFORM DISPLAY_LISTS
          TABLES   LT_COAREA_SUM
                   LT_OBJECT_SUM
                   LT_TRANSACTION_SUM
                   LT_COEP_DISP
          USING    DISPLIST
                   LS_TKA01.


*=======================================================================
* forms:
*=======================================================================

*&---------------------------------------------------------------------*
*&      Form  COLLECT_OBJECT_SUM
*&---------------------------------------------------------------------*
FORM COLLECT_OBJECT_SUM                                     "#EC CALLED
     USING     IS_COEP         LIKE  COEP
               ID_FY_VARIANT   LIKE  T001-PERIV
     CHANGING  CS_OBJECT_SUM   LIKE  LT_OBJECT_SUM.

  DATA:
  LT_PERIODS  LIKE  PERIODS OCCURS 0 WITH HEADER LINE,
  LT_COIOB    LIKE  COIOB   OCCURS 0 WITH HEADER LINE.

  LT_PERIODS-BUPER  = IS_COEP-PERIO.

  CALL FUNCTION 'FIRST_DAY_IN_PERIOD_GET'
       EXPORTING
            I_GJAHR = IS_COEP-GJAHR
            I_PERIV = ID_FY_VARIANT
            I_POPER = LT_PERIODS-BUPER
       IMPORTING
            E_DATE  = LT_PERIODS-DATAB.

  CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
       EXPORTING
            I_GJAHR = IS_COEP-GJAHR
            I_PERIV = ID_FY_VARIANT
            I_POPER = LT_PERIODS-BUPER
       IMPORTING
            E_DATE  = LT_PERIODS-DATBI.

  APPEND  LT_PERIODS.

  LT_COIOB-OBJNR = IS_COEP-OBJNR.
  APPEND LT_COIOB.

* object currency:
  CALL FUNCTION 'K_OBJECTS_MASTER_READ'
       TABLES
            COIOB_TAB = LT_COIOB
            I_PERIODS = LT_PERIODS.

  READ TABLE  LT_COIOB   INDEX 1.

  IF ( LT_COIOB-OWAER IS INITIAL ).
    MESSAGE E001(K5) WITH 'No object currency found for'    "#EC NOTEXT
                          'object'                          "#EC NOTEXT
                          LT_COIOB-OBJNR ''.
  ENDIF.

  CS_OBJECT_SUM-CURR   = LT_COIOB-OWAER.
  CS_OBJECT_SUM-WOGBTR = IS_COEP-WOGBTR.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_LISTS
*&---------------------------------------------------------------------*
FORM DISPLAY_LISTS
     TABLES  IT_COAREA_SUM       STRUCTURE  LT_COAREA_SUM
             IT_OBJECT_SUM       STRUCTURE  LT_OBJECT_SUM
             IT_TRANSACTION_SUM  STRUCTURE  LT_TRANSACTION_SUM
             IT_COEP_LIST        STRUCTURE  COEP
    USING    ID_DISPLAY_LIST     LIKE       BOOLE-BOOLE
             IS_TKA01            LIKE       TKA01.


  WRITE: 'Controlling area currency values'(110).

  WRITE: /, /.

  WRITE: / SY-ULINE(46).

  WRITE: /1(1)  '|',
          3(10) 'CURRENCY'(001),
          13(1)  '|',
          15(30) 'VALUE'(002),
          46(1)  '|'.

  WRITE: / SY-ULINE(46).


  LOOP AT IT_COAREA_SUM.

    WRITE:  /1(1)   '|',
             2(10)  IT_COAREA_SUM-CURR,
             13(1)  '|',
             15(30) IT_COAREA_SUM-WKGBTR
                    CURRENCY IT_COAREA_SUM-CURR,
             46(1)  '|'.

  ENDLOOP.

  WRITE: / SY-ULINE(46).

  WRITE: /, /.

  WRITE: 'CO-Object currency values'(120).

  WRITE: /, /.

  WRITE: / SY-ULINE(46).

  WRITE: /1(1)  '|',
          2(10) 'CURRENCY'(001),
         13(1)  '|',
         14(30) 'VALUE'(002),
         46(1)  '|'.

  WRITE: / SY-ULINE(46).

  LOOP AT IT_OBJECT_SUM.

    WRITE: /1(1)   '|',
            2(10)  IT_OBJECT_SUM-CURR,
            13(1)  '|',
            15(30) IT_OBJECT_SUM-WOGBTR
                   CURRENCY  IT_OBJECT_SUM-CURR,
            46(1)  '|'.

  ENDLOOP.

  WRITE: / SY-ULINE(46).

  WRITE: /, /.

  WRITE: 'Transaction currency values'(130).

  WRITE: /, /.

  WRITE: / SY-ULINE(46).

  WRITE: /1(1)   '|',
          2(10)  'CURRENCY'(001),
          13(1)  '|',
          15(30) 'VALUE'(002),
          46(1)  '|'.

  WRITE: / SY-ULINE(46).

  LOOP AT IT_TRANSACTION_SUM.

    WRITE: /1(1)  '|',
            2(10) IT_TRANSACTION_SUM-CURR,
           13(1)  '|',
           15(30) IT_TRANSACTION_SUM-WTGBTR
                  CURRENCY  IT_TRANSACTION_SUM-CURR,
           46(1)  '|'.

  ENDLOOP.

  WRITE: / SY-ULINE(46).

  CHECK ( ID_DISPLAY_LIST <> SPACE ).

* display line items:
  WRITE: /, /, /.
  WRITE: 'Line items:'.
  WRITE: /, /.

  WRITE: / SY-ULINE(77).

  WRITE: /1(1)   '|',
          2(10)  'DOCNR'(003),
          13(1)  '|',
          15(10) 'LINE'(004),
          26(1)  '|',
          28(12) 'COST ELEMENT'(005),
          41(1)  '|',
          43(20) 'CO AREA VALUE'(006),
          64(1)  '|',
          66(10)  'CO AREA CURR'(007),
          77(1)  '|'.

  WRITE: / SY-ULINE(77).

  LOOP AT IT_COEP_LIST.

    WRITE: /1(1)   '|',
            2(10)  IT_COEP_LIST-BELNR,
            13(1)  '|',
            14(10) IT_COEP_LIST-BUZEI,
            26(1)  '|',
            28(12) IT_COEP_LIST-KSTAR,
            41(1)  '|',
            43(20) IT_COEP_LIST-WKGBTR  CURRENCY  IS_TKA01-WAERS,
            64(1)  '|',
            66(10) IS_TKA01-WAERS,
            77(1)  '|'.
  ENDLOOP.

  WRITE: / SY-ULINE(77).

  WRITE: /26(1)   '|',
          28(12)  'Summary'(008),
          41(1)   '|',
          43(20) IT_COAREA_SUM-WKGBTR CURRENCY IS_TKA01-WAERS,
          64(1)  '|',
          66(10) IS_TKA01-WAERS,
          77(1)  '|'.

  WRITE: /26 SY-ULINE(52).

ENDFORM.
