************************************************************************
*                                                                      *
*  Report:          ZZ_SL_ANALYZE   V3.0                               *
*                                                                      *
************************************************************************
*                                                                      *
* Created by     :  OBERDORF                                           *
* Date/Time      :  10/04/2004                                         *
* Last Changed   :  27/01/2005                                         *
* Valid releases :  SAP_APPL  46b, 46c, 470                            *
*                                                                      *
************************************************************************
*                                                                      *
* Purpose:    Analyzing of Special Ledger Tables                       *
*                                                                      *
************************************************************************
*                                                                      *
*  Using this report ZZ_SL_ANALYSE, you can locate the causes for      *
*  differences between FI and FI-SL.                                   *
*  Moreover, this report helps you to detect technical inconsistencies *
*  in your ledger-tables or your ledger customizing.                   *
*                                                                      *
*  On the selection screen you should enter at least:                  *
*  the ledger, the record type, the Version, the fiscal year and the   *
*  company/company code.                                               *
*  The extended selection area is only valid for the "missing document *
*  check" and the "Additional NON-FI document" check.                  *
*                                                                      *
*  This report contains the following checks:                          *
*                                                                      *
*  1) Object Table Inconsistency Check:                                *
*     Only when the Object Numbers in the Obj. Tables and the Summary  *
*     tables are consistent, the system can update the summary table   *
*     correctly. If the program finds inconsistencies there then there *
*     is a high probability that the summary table is not correct.     *
*     Therefore, the summary table should be deleted using transaction *
*     GCDE and re-builded from the line item table using report        *
*     RGUREP01 (only possible if all line items exist)                 *
*     The procedure is described in note 45747.                        *
*                                                                      *
*  2) Inconsistency between Summary Table and Line Item Table:         *
*     In case your ledger writes line items this check helps you to    *
*     analyze inconsistencies between the summary and line item table. *
*     The report compares the totals records with the totals of the    *
*     line items. In case the report indicates errors here, the summary*
*     table should be deleted using transaction GCDE and re-builded    *
*     from the line item table using report RGUREP01.                  *
*     The procedure is described in note 45747.                        *
*                                                                      *
*                                                                      *
*  3) Double Key / NULL values:                                        *
*     This error can occur when Special Ledger Table Groups were       *
*     manually and incorrectly changed, extended or converted.         *
*     NULL values will only affect the previous periods (the periods   *
*     before the  table group extension).                              *
*     Whereas a "double key error" indicates a wrong table group       *
*     customizing where the consistency check of transaction GCIN      *
*     had been ignored.                                                *
*     Please note, that the first index of the object table has to be  *
*     unique, so double keys can only occur when this index was        *
*     temporarily deactivated.                                         *
*                                                                      *
*  4) Customizing                                                      *
*     This check offers an overview of important special ledger        *
*     customizing settings which influence posting from FI:            *
*                                                                      *
*     - Activity settings                                              *
*     - Fixed Field Movement                                           *
*     - Variable Field Movement                                        *
*     - Chart of accounts, etc.                                        *
*                                                                      *
*     Furthermore, some consitency checks can indicate some errors.    *
*                                                                      *
*                                                                      *
*  5) Missing Documents                                                *
*     This check can only be used when line items writing is activated *
*     for the affected ledger/company code.                            *
*     The check compares the number of documents in FI with the number *
*     of documents in Special Ledger. A document which exists in FI    *
*     but not in Special Ledger will be printed on the output list.    *
*     As this check may take a while you can use the extended          *
*     Selection Area.                                                  *
*                                                                      *
*     Special Selection Buttons:                                       *
*                                                                      *
*     - Option: Fast Search:                                           *
*       Activating this button will have an effect on the processing   *
*       time of the report.                                            *
*                                                                      *
*       When you deactivate the flag the program checks in the FI      *
*       table BSEG, if the FI document contains also line items.       *
*       Documents without line items in BSEG are skipped. Processing   *
*       time will be stretched then.                                   *
*       This flag should not be deactivated if you analyze a splitt    *
*       ledger.                                                        *
*                                                                      *
*     - Option: Converted Pool Docs exist:                             *
*       In case you have converted old pool tables (for example GLT1)  *
*       into transparent tables, then the program might not recognize  *
*       the converted documents. Then please activate this button.     *
*                                                                      *
*  6) Additional NON FI checks.                                        *
*     This check can only be used when line items writing is activated *
*     for the affected ledger.                                         *
*     If the summary table of FI does not match eith the summary table *
*     of FI-SL then some documents are missing or there are documents  *
*     in Special Ledger which do not exist in FI.                      *
*     This check lists all documents such as CO documents or local     *
*     document which have no equivalent in FI.                         *
*                                                                      *
************************************************************************

REPORT zz_sl_analyze01 MESSAGE-ID gu LINE-SIZE 110.

CONSTANTS zpglen TYPE i VALUE 95.

*Types
TYPE-POOLS: gusl,                      "Selection processor
            kkblo,
            icon.

TYPES:  BEGIN OF tab_struc,
               table(10)   TYPE c,
               name(10)    TYPE c,
               keyflag(1)  TYPE c,
               notempty(1) TYPE c,
               content LIKE rsdsselopt-low,
        END OF tab_struc.

TYPES:
 BEGIN OF tt_err4 ,
   gjahr  LIKE bkpf-gjahr,
   bukrs  LIKE bkpf-bukrs,
   belnr  LIKE bkpf-belnr,
   awtyp  LIKE bkpf-awtyp,
   glvor  LIKE bkpf-glvor,
 END OF tt_err4.


TYPES:
 BEGIN OF tt_err5 ,
   RLDNR  LIKE GLFUNCA-RLDNR,
   RRCTY  LIKE GLFUNCA-RRCTY,
   RVERS  LIKE GLFUNCA-RVERS,
   RYEAR  LIKE GLFUNCA-RYEAR,
   POPER  LIKE GLFUNCA-POPER,
   RBUKRS LIKE GLFUNCA-RBUKRS,
   RCOMP  LIKE GLT2-RCOMP,
   DOCNR  LIKE GLFUNCA-DOCNR,
   ACTIV  LIKE GLFUNCA-ACTIV,
 END OF tt_err5.

TYPES:
 BEGIN OF tt_err6 ,
   RLDNR  LIKE GLFUNCA-RLDNR,
   RRCTY  LIKE GLFUNCA-RRCTY,
   RVERS  LIKE GLFUNCA-RVERS,
   RYEAR  LIKE GLFUNCA-RYEAR,
   RBUKRS LIKE GLFUNCA-RBUKRS,
   RCOMP  LIKE GLT2-RCOMP,
   ACTIV  LIKE GLFUNCA-ACTIV,
   count1 LIKE SY-TABIX,
 END OF tt_err6.


*Tables
TABLES: glu1, t800a, t881, t001, t022t, ttypt, bkpf.

* Field Symbols
FIELD-SYMBOLS <slfield> TYPE ANY.

* Fields
DATA:   h_org_name(6).
DATA:   Z_LI_EXIST(1) VALUE ' '.
DATA: wa_t001 LIKE T001.

RANGES  r_rldnr FOR glu1-rldnr.
RANGES  r_rvers FOR glu1-rvers.
RANGES  r_rrcty FOR glu1-rrcty.

RANGES  p_racct  FOR glu1-racct.
RANGES  p_rfarea FOR glu1-rfarea.
RANGES  p_rbusa  FOR glu1-rbusa.
RANGES  p_rcntr  FOR glu1-rcntr.
RANGES  p_rprctr FOR glu1-rprctr.
RANGES  r_rbukrs FOR glu1-rbukrs.
RANGES  r_rcomp FOR glu1-rcomp.
RANGES  r_ryear FOR glu1-ryear.

DATA: err_chck1 TYPE boolean VALUE 'T'.
DATA: err_chck2 TYPE boolean VALUE 'T'.
DATA: err_chck3 TYPE boolean VALUE 'T'.
DATA: err_chck4 TYPE boolean VALUE 'T'.
DATA: err_chck5 TYPE boolean VALUE 'T'.
DATA: err_chck6 TYPE boolean VALUE 'T'.

* Fields for Check1
DATA: t_sumfields   LIKE dfies OCCURS 0 WITH HEADER LINE,
      t_h_sumfields LIKE dfies OCCURS 0 WITH HEADER LINE,
      t_ob1fields   LIKE dfies OCCURS 0 WITH HEADER LINE,
      t_ob2fields   LIKE dfies OCCURS 0 WITH HEADER LINE,
      t_obj_fields_sum LIKE dfies OCCURS 0 WITH HEADER LINE,
      t_obj_fields_sum_exc LIKE dfies OCCURS 0 WITH HEADER LINE,
      t_obj_fields_obj LIKE dfies OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF t_err1 OCCURS 0,
        rldnr  LIKE glu1-rldnr ,
        rrcty  LIKE glu1-rrcty ,
        rvers  LIKE glu1-rvers ,
        ryear  LIKE glu1-ryear ,
        rbukrs LIKE glu1-rbukrs ,
      END OF t_err1.

DATA: h_fieldname LIKE dfies-fieldname.
DATA: zmsg01(120).

FIELD-SYMBOLS: <table>,
               <field>.

DATA: work(200).                       "allg. Arbeitsfeld
DATA: BEGIN OF code OCCURS 250,
        line(72),                      "Select-ABAP Code-Tabelle
      END OF code,
      offset_code TYPE i.              "Linker Rand im Coding

* for Check 2
DATA: it_err2(zpglen) TYPE c OCCURS 0 WITH HEADER LINE.
DATA: zlines LIKE sy-tabix.
DATA: z_prog_name LIKE sy-xprog.
DATA: zmsg02(120).

* for Check 3
DATA: BEGIN OF cust_out OCCURS 100,
        line(90),
      END OF cust_out.
DATA: cust_err(1) Type c.
DATA: zctext00(50),
      zctext01(50),
      zctext02(50),
      zctext03(50),
      zctext04(50),
      zctext05(50),
      zctext06(50),
      zctext07(50),
      zctext08(50),
      zctext09(50),
      zctext10(50),
      zctext11(50),
      zctext12(50).

* for Check 4
DATA: it_missdoc TYPE tt_err4 OCCURS 10.
DATA: it_nondoc1 TYPE tt_err5 OCCURS 10.
DATA: it_nondoc2 TYPE tt_err6 OCCURS 10.

DATA: ZTEXT2(90) TYPE c.
DATA: wa_missdoc TYPE tt_err4.


* for Check 5
DATA: Z_MSG1 LIKE WORK.
DATA: LD_MSG LIKE WORK.
DATA: obj_table LIKE T800A-OBJTABLE.


DATA: BEGIN OF IT_CLNT0 OCCURS 5,
        ISSUE(6) TYPE C,
        CLIENT LIKE T001-MANDT,
        COUNT1 LIKE SY-TABIX,
     END OF IT_CLNT0.



*--------------------------------------------------


* Selection screen definition
SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE t0.

* Test Run
SELECTION-SCREEN BEGIN OF BLOCK b WITH FRAME TITLE t1.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20) zatext01 FOR FIELD p_chck01.
SELECTION-SCREEN POSITION POS_LOW.
PARAMETERS p_chck01 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20) zatext02 FOR FIELD p_chck02.
SELECTION-SCREEN POSITION POS_LOW.
PARAMETERS p_chck02 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20) zatext05 FOR FIELD p_chck05.
SELECTION-SCREEN POSITION POS_LOW.
PARAMETERS p_chck05 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF LINE.


SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20) zatext03 FOR FIELD p_chck03.
SELECTION-SCREEN POSITION POS_LOW.
PARAMETERS p_chck03 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20) zatext04 FOR FIELD p_chck04.
SELECTION-SCREEN POSITION POS_LOW.
PARAMETERS p_chck04 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20) zatext06 FOR FIELD p_chck06.
SELECTION-SCREEN POSITION POS_LOW.
PARAMETERS p_chck06 AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN END OF LINE.


SELECTION-SCREEN END OF BLOCK b.

SELECTION-SCREEN BEGIN OF BLOCK d WITH FRAME TITLE t3.
*Ledger
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20) zbtext01 FOR FIELD p_rldnr.
SELECTION-SCREEN POSITION POS_LOW.
PARAMETERS p_rldnr LIKE glu1-rldnr OBLIGATORY MEMORY ID gln.
SELECTION-SCREEN END OF LINE.

* Record Type
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20) zbtext02 FOR FIELD p_rrcty.
SELECTION-SCREEN POSITION POS_LOW.
PARAMETERS p_rrcty LIKE glu1-rrcty OBLIGATORY default '0'.
SELECTION-SCREEN END OF LINE.

* Version
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20) zbtext03 FOR FIELD p_rvers.
SELECTION-SCREEN POSITION POS_LOW.
PARAMETERS p_rvers LIKE glu1-rvers OBLIGATORY default '001'.
SELECTION-SCREEN END OF LINE.

* Fiscal Year
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20) zbtext06 FOR FIELD p_ryear.
SELECTION-SCREEN POSITION POS_LOW.
PARAMETERS p_ryear LIKE glu1-ryear default '2004'.
SELECTION-SCREEN END OF LINE.

* Company Code
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20) zbtext04 FOR FIELD p_rbukrs.
SELECTION-SCREEN POSITION POS_LOW.
PARAMETERS p_rbukrs LIKE glu1-rbukrs MEMORY ID buk.
SELECTION-SCREEN END OF LINE.

* Company
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20) zbtext05 FOR FIELD p_rcomp.
SELECTION-SCREEN POSITION POS_LOW.
PARAMETERS p_rcomp LIKE glu1-rcomp MEMORY ID gcc.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK d.


SELECTION-SCREEN BEGIN OF BLOCK e WITH FRAME TITLE t4.

* Period
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20) zbtext07 FOR FIELD r_perid.
SELECTION-SCREEN POSITION POS_LOW.
SELECT-OPTIONS r_perid FOR glu1-poper.
SELECTION-SCREEN END OF LINE.

* Doc No.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20) zbtext08 FOR FIELD r_belnr.
SELECTION-SCREEN POSITION POS_LOW.
SELECT-OPTIONS r_belnr FOR bkpf-belnr.
SELECTION-SCREEN END OF LINE.

* Document Type
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20) zbtext09 FOR FIELD r_doctt.
SELECTION-SCREEN POSITION POS_LOW.
SELECT-OPTIONS r_doctt FOR bkpf-blart.
SELECTION-SCREEN END OF LINE.


*  AWTYP
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20) zbtext10 FOR FIELD r_awtyp.
SELECTION-SCREEN POSITION POS_LOW.
SELECT-OPTIONS r_awtyp FOR ttypt-awtyp.
SELECTION-SCREEN END OF LINE.


* Activity
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20) zbtext11 FOR FIELD r_activ.
SELECTION-SCREEN POSITION POS_LOW.
SELECT-OPTIONS r_activ FOR t022t-activity.
SELECTION-SCREEN END OF LINE.

* BSTAT
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20) zbtext12 FOR FIELD r_bstat.
SELECTION-SCREEN POSITION POS_LOW.
SELECT-OPTIONS r_bstat FOR bkpf-bstat.
SELECTION-SCREEN END OF LINE.

* Fast Search
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20) zbtext13 FOR FIELD z_quick.
SELECTION-SCREEN POSITION POS_LOW.
PARAMETERS z_quick  AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF LINE.

* Including old documents from converted pool tables
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(30) zbtext14 FOR FIELD z_poolcv.
SELECTION-SCREEN POSITION POS_LOW.
PARAMETERS z_poolcv  AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK e.

SELECTION-SCREEN END OF BLOCK a.


******************* INITIALIZATION ***********************

INITIALIZATION.

* Selection Texts
  t0 = 'Analyzing Inconsistencies between FI and FI-SL'.
  t1 = 'Processing Check'.
  t3 = 'Selection'.
  t4 = 'Extended Selection for Missing Doc check'.

  zatext01 = 'Object Table Inconsist.'(a01).
  zatext02 = 'Inconsist. Sum and Line It.Table'(a02).
  zatext03 = 'Customizing'(a03).
  zatext04 = 'Missing Documents'(a04).
  zatext05 = 'Double Key/NULL Values'(A05).
  zatext06 = 'Additional Non-FI Documents'(A06).

  zbtext01 = 'Ledger'(b01).
  zbtext02 = 'Record Type'(b02).
  zbtext03 = 'Version'(b03).
  zbtext04 = 'Company Code'(b04).
  zbtext05 = 'Company'(b05).
  zbtext06 = 'Fiscal Year'(b06).
  zbtext07 = 'Fiscal period'(b07).
  zbtext08 = 'Accounting document number'(b08).
  zbtext09 = 'Document type'(b09).
  zbtext10 = 'Reference procedure'(b10).
  zbtext11 = 'FI-SL business activity'(b11).
  zbtext12 = 'Document Status'(b12).
  zbtext13 = 'Fast Search'(b13).
  zbtext14 = 'Converted pool docs exist '(b14).

  zctext00 = 'Cust.Settings which may cause inconsistencies'(c00).
  zctext01 = 'No Line Item Writing for all GL activities'(c01).
  zctext02 = 'No Standard fixed Field Movement'(c02).
  zctext03 = 'User Exits in fixed Field Movement'(c03).
  zctext04 = 'A ledger selection is possibly activ'(c04).
  zctext05 = 'Field Movement: User Exit exist for <RACCT>'(c05).
  zctext06 = 'Field Movement: HKONT <> RACCT'(c06).
  zctext07 = 'At least one GL-activity is missing'(c07).
  zctext08 = 'At least one non-GL-activity assigned'(c08).
  zctext09 = 'Local documents (GB01/GB11) in line item table'(c09).
  zctext10 = 'Details:'(c10).
  zctext11 = 'Fixed Field Movement (Transaction GCI4):'(c11).
  zctext12 = 'Variable Field Movement (Transaction GCF3):'(c12).


******************* Check Selection Screen ***********************
AT SELECTION-SCREEN.
*Check ledger
  SELECT SINGLE * FROM t881 WHERE rldnr = p_rldnr.
  IF sy-subrc NE 0.
    SET CURSOR FIELD 'P_RLDNR'.
    MESSAGE e781 WITH p_rldnr.
  ELSE.
    SELECT SINGLE * FROM t800a WHERE tab = t881-tab.
    IF sy-subrc NE 0 OR t800a-ntable IS INITIAL.
      SET CURSOR FIELD 'P_RLDNR'.
      MESSAGE e085.
    ENDIF.
    IF   ( t800a-objtable IS INITIAL )
     AND ( p_chck01 = 'X' ).
      p_chck01 = ' '.
      MESSAGE e016 WITH 'Check of Object table not necessery for table'
                         t800a-tab.
    ENDIF.

    IF   ( t800a-objtable IS INITIAL )
     AND ( p_chck05 = 'X' ).
      p_chck05 = ' '.
      MESSAGE e016 WITH 'Check of NULL values not necessery for table'
                         t800a-tab.
    ENDIF.

    IF ( sy-subrc = 0 ) AND ( t800a-inactive NE space ) .
      MESSAGE e171(gu) WITH t800a-tab.
    ENDIF.
*   Check if ledger is fix
    IF t881-fix = 'X'.
      IF  p_chck01 IS INITIAL AND
          p_chck02 IS INITIAL AND
          p_chck03 IS INITIAL AND
          p_chck04 IS INITIAL AND
          p_chck05 = 'X'      AND
          T800a-TAB NE 'V_GLFLEXT'.
        SET CURSOR FIELD 'P_RLDNR'.
        MESSAGE w602 WITH P_RLDNR.
      ELSE.
        SET CURSOR FIELD 'P_RLDNR'.
        MESSAGE e602 WITH P_RLDNR.
      ENDIF.
    ENDIF.
  ENDIF.
  IF t800a-comptab IS INITIAL.
    IF t800a-objtable IS INITIAL.
      h_org_name = 'BUKRS'.
    ELSE.
      h_org_name = 'RBUKRS'.
    ENDIF.
  ELSE.
    h_org_name = 'RCOMP'.
  ENDIF.

* CoCode Year required
  IF p_chck01 = 'X' OR
     p_chck02 = 'X' OR
     p_chck03 = 'X' OR
     p_chck04 = 'X' OR
     p_chck05 = 'X' OR
     p_chck06 = 'X'.

    IF ( p_rbukrs IS INITIAL ) AND ( p_rcomp IS INITIAL ) .
      MESSAGE e166 WITH p_rldnr.
    ENDIF.
    IF ( p_ryear IS INITIAL ).
      MESSAGE e255 WITH p_rldnr.
    ENDIF.
  ENDIF.


*Check Company code
  IF NOT ( p_rcomp  IS INITIAL )  AND
     NOT ( p_rbukrs IS INITIAL ).
    MESSAGE e919.
  ENDIF.

  IF t800a-comptab = 'X'.
    IF NOT ( p_rbukrs IS INITIAL ).
      MESSAGE e608 WITH p_rldnr.
    ENDIF.
  ELSE.
    IF NOT ( p_rcomp IS INITIAL ).
      MESSAGE e606 WITH p_rldnr.
    ENDIF.
  ENDIF.

* Find Company Code in global ledger / company
  REFRESH r_rbukrs.
  IF ( h_org_name = 'RCOMP' ).
    SELECT * FROM T001 INTO wa_t001
           WHERE RCOMP = p_rcomp.
      MOVE: 'I'      TO r_rbukrs-sign,
            'EQ'     TO r_rbukrs-option,
             wa_t001-bukrs TO r_rbukrs-low.
      APPEND r_rbukrs.
    ENDSELECT.
    IF sy-subrc ne 0.
      MESSAGE e296 WITH p_rcomp.
    ENDIF.
  ELSE.
    MOVE: 'I'      TO r_rbukrs-sign,
          'EQ'     TO r_rbukrs-option,
           p_rbukrs TO r_rbukrs-low.
    APPEND r_rbukrs.
  ENDIF.


********************Begin of processing*********************************
START-OF-SELECTION.


* Check if Line Items Exist
  PERFORM check_lineItm changing Z_LI_EXIST.

* START Check 1
  IF p_chck01 = 'X'.
    PERFORM check_obj.
    DESCRIBE TABLE t_err1 LINES zlines.
    IF zlines > 0.
      err_chck1 = 'F'.
    ELSE.
      err_chck1 = 'T'.
    ENDIF.
    IF NOT zmsg01 is INITIAL.
      err_chck1 = 'F'.
    ENDIF.

    SKIP 2.
  ENDIF.


* START Check 5
  IF P_CHCK05 = 'X'.
    CLEAR Z_MSG1.
    REFRESH IT_CLNT0.
    PERFORM DUB_CREATE USING    T800A
                       CHANGING IT_CLNT0[]
                                Z_MSG1.
    DESCRIBE TABLE IT_CLNT0 LINES ZLINES.

    IF  ZLINES > 0
     OR NOT ( Z_MSG1 IS INITIAL ).
      ERR_CHCK5 = 'F'.
    ELSE.
      ERR_CHCK5 = 'T'.
    ENDIF.

    IF NOT zmsg02 is INITIAL.
      err_chck5 = 'F'.
    ENDIF.

  ENDIF.


* START Check 2
  IF p_chck02 = 'X'.
    PERFORM comp_sum USING h_org_name
                           p_rldnr
                           p_rrcty
                           p_rvers
                           p_rbukrs
                           p_rcomp
                           p_ryear
                     CHANGING it_err2[] .

    DESCRIBE TABLE it_err2 LINES zlines.
    READ TABLE it_err2 INDEX 2.
    IF it_err2(10) = '----------'.
      err_chck2 = 'F'.
    ELSE.
      err_chck2 = 'T'.
    ENDIF.

*    LOOP AT it_err2.
*      WRITE:/ it_err2.
*    ENDLOOP.
  ENDIF.

* START Check 3
  IF p_chck03 = 'X'.
    PERFORM CHECK_CUST1 USING p_rldnr
                              p_rrcty
                              p_rvers
                              p_ryear
                              p_rbukrs
                              p_rcomp
                              h_org_name
                              T800a
                              T881
                     changing cust_out[]
                              cust_err.


    IF cust_err = 'X'.
      err_chck3 = 'F'.
    ELSE.
      err_chck3 = 'T'.
    ENDIF.
    SKIP 2.
  ENDIF.


* START Check 4: Missing Documents
  IF ( p_chck04 = 'X' ) AND ( Z_LI_EXIST = 'X' ).
    PERFORM check_doc_exist USING    h_org_name
                                     z_poolcv
                            CHANGING it_missdoc[].
    DESCRIBE TABLE it_missdoc LINES zlines.
    IF zlines > 0.
      err_chck4 = 'F'.
    ELSE.
      err_chck4 = 'T'.
    ENDIF.
  ENDIF.

* START Check 6: Non FI Documents
  IF ( p_chck06 = 'X' ) AND ( Z_LI_EXIST = 'X' ).

    PERFORM check_non_fi USING    h_org_name
                        CHANGING it_nondoc1[]
                                 it_nondoc2[].

    DESCRIBE TABLE it_nondoc1 LINES zlines.

    IF zlines > 0.
      err_chck6 = 'F'.
    ELSE.
      err_chck6 = 'T'.
    ENDIF.

  ENDIF.

  PERFORM err_output1.
  PERFORM err_output2.


*---------------------------------------------------------------------*
*  FORM check_obj     * *
*---------------------------------------------------------------------*
*  Checks Inconsistencies in Object Tables                            *
*---------------------------------------------------------------------*

FORM check_obj.
  DATA: zmsgx1 LIKE zmsg01.
* Check object table consistency---------------------------------------

  CLEAR: zmsg01, zmsgx1.
*Get the fields of all tables
  PERFORM get_fields.

*Generate the coding in ZZUREP12
  PERFORM generate_data_declaration.

*Generate call of e01_check_objnrs  *not in use*
* PERFORM generate_call_e01_check_objnrs.

*Generate e01_check_objnrs
  PERFORM generate_e01_check_objnrs.

*Generate list output
  PERFORM generate_list.

*Generate subroutine print_list.
  PERFORM generate_print_list.


*Insert the generated report
  GENERATE SUBROUTINE POOL code NAME z_prog_name MESSAGE zmsg01 .
  IF sy-subrc NE 0.
    CONCATENATE 'Check_' zatext01 '_will be skipped' INTO zmsgx1.
    MESSAGE I016 WITH zmsgx1.
    EXIT.
  ENDIF.
  COMMIT WORK.

*External call of check routine in generated RGUREP12
  MOVE: 'I'      TO r_rldnr-sign,
        'EQ'     TO r_rldnr-option,
         p_rldnr TO r_rldnr-low.
  APPEND r_rldnr.

  MOVE: 'I'      TO r_rvers-sign,
        'EQ'     TO r_rvers-option,
         p_rvers TO r_rvers-low.
  APPEND r_rvers.

  MOVE: 'I'      TO r_rrcty-sign,
        'EQ'     TO r_rrcty-option,
         p_rrcty TO r_rrcty-low.
  APPEND r_rrcty.

  IF NOT ( p_ryear IS INITIAL ).
    MOVE: 'I'      TO r_ryear-sign,
          'EQ'     TO r_ryear-option,
           p_ryear TO r_ryear-low.
    APPEND r_ryear.
  ENDIF.

  MOVE: 'I'    TO r_rcomp-sign,
        'EQ'   TO r_rcomp-option,
         p_rcomp TO r_rcomp-low.
  APPEND r_rcomp.



  PERFORM e01_check_objnrs IN PROGRAM (z_prog_name)
                                      TABLES r_rldnr
                                            r_rrcty
                                            r_rvers
                                            r_ryear
                                            r_rcomp
                                            r_rbukrs
                                            p_racct
                                            p_rbusa
                                            p_rfarea
                                            p_rcntr
                                            p_rprctr
                                            t_err1.


* End of check: object table consistency  -----------------------------
ENDFORM.                    "check_obj




* Start of Subroutines -------------------------------------------------

*---------------------------------------------------------------------*
*  FORM get_fields
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
FORM get_fields.
*Get key fields (with object numbers) of totals table
  PERFORM get_table_fields TABLES t_sumfields
                            USING t800a-tab
                                  'K'
                                  'X'.

*Get key fields (without object numbers) of totals table
  PERFORM get_table_fields TABLES t_h_sumfields
                            USING t800a-tab
                                  'K'
                                  ' '.

*Append logical key fields at the end of t_sumtab
  LOOP AT t_h_sumfields.
    READ TABLE t_sumfields WITH KEY fieldname = t_h_sumfields-fieldname.
    IF sy-subrc NE 0.
      APPEND t_h_sumfields TO t_sumfields.
    ENDIF.
  ENDLOOP.
  DELETE t_sumfields WHERE fieldname = 'RCLNT'.
*Get fields of object table 1
  IF t800a-objtable NE space.
    PERFORM get_table_fields TABLES t_ob1fields
                            USING t800a-objtable
                                  'A'
                                  'X'.


    DELETE t_ob1fields WHERE fieldname = 'MANDT'
                          OR fieldname = 'DATIV'
                          OR fieldname = 'DATIB'
                          OR fieldname = 'DATPV'
                          OR fieldname = 'DATPB'.
  ENDIF.
*Get fields of object table 2
  IF t800a-objtable2 NE space.
    PERFORM get_table_fields TABLES t_ob2fields
                            USING t800a-objtable2
                                  'A'
                                  'X'.


    DELETE t_ob2fields WHERE fieldname = 'MANDT'
                          OR fieldname = 'DATIV'
                          OR fieldname = 'DATIB'
                          OR fieldname = 'DATPV'
                          OR fieldname = 'DATPB'.
  ENDIF.
ENDFORM.                    "get_fields

*---------------------------------------------------------------------*
*       FORM get_table_fields                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  GTF_T_FIELDS                                                  *
*  -->  GTF_TABLE                                                     *
*  -->  GTF_FTYPE                                                     *
*  -->  GTF_NO_GLX_OBJ_PROCESSING                                     *
*---------------------------------------------------------------------*
FORM get_table_fields TABLES gtf_t_fields STRUCTURE dfies
                      USING  gtf_table LIKE t800a-tab
                             gtf_ftype
                             gtf_no_glx_obj_processing.
  DATA: l_subrc LIKE sy-subrc.

  CALL FUNCTION 'G_FIELD_SET'
       EXPORTING
            ftype                 = gtf_ftype
            no_glx_obj_processing = gtf_no_glx_obj_processing
            table                 = gtf_table
       EXCEPTIONS
            not_found             = 1.
  IF sy-subrc EQ 1.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  REFRESH gtf_t_fields.
  DO.
    CALL FUNCTION 'G_FIELD_GET'
         IMPORTING
              field_attr = gtf_t_fields
              subrc      = l_subrc.
    IF l_subrc NE 0.
      EXIT.
    ELSE.
      APPEND gtf_t_fields.
    ENDIF.
  ENDDO.
  CLEAR gtf_t_fields.
ENDFORM.                    "get_table_fields


*&---------------------------------------------------------------------*
*&      Form  generate_data_declaration
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM generate_data_declaration .

  PERFORM append_code USING  0 0 'report rgurep12.' space.

  PERFORM append_code USING  0 0 space space.

  MOVE 'TABLES GLU1.' TO work.
  REPLACE '$' INTO work WITH ''.
  PERFORM append_code USING  0 0 work space.

  MOVE 'TABLES: $.' TO work.
  REPLACE '$' INTO work WITH t800a-tab.
  PERFORM append_code USING  0 0 work space.

  IF t800a-objtable NE space.
    MOVE 'TABLES: $.' TO work.
    REPLACE '$' INTO work WITH t800a-objtable.
    PERFORM append_code USING  0 0 work space.
  ENDIF.

  IF t800a-objtable2 NE space.
    MOVE 'TABLES: $.' TO work.
    REPLACE '$' INTO work WITH t800a-objtable2.
    PERFORM append_code USING  0 0 work space.
  ENDIF.

  PERFORM append_code USING  0 0 space space.

  PERFORM append_code USING  0 0:
    'RANGES:' space,
    'p_rldnr for glu1-rldnr ,'    space,
    'p_rrcty for glu1-rrcty ,'    space,
    'p_rvers for glu1-rvers ,'    space,
    'p_ryear for glu1-ryear ,'    space,
    'p_rcomp for glu1-rcomp , '   space,
    'p_rbukrs for glu1-rbukrs ,'  space,
    'p_racct for glu1-racct ,'    space,
    'p_rbusa for glu1-rbusa ,'    space,
    'p_rfarea for glu1-rfarea ,'  space,
    'p_rcntr for glu1-rcntr , '   space,
    'p_rprctr for glu1-rprctr .'  space.
  .

  PERFORM append_code USING  0 0 space space.

  PERFORM append_code USING  0 8:
   'data: begin of t_sumtab occurs 0,' space.

  LOOP AT t_sumfields.
    sy-fdpos = strlen( t800a-tab ).
    ASSIGN t800a-tab(sy-fdpos) TO <table>.
    MOVE '$ like $-$,' TO work.
    REPLACE '$' INTO work WITH: t_sumfields-fieldname,
                                <table>,
                                t_sumfields-fieldname.
    PERFORM append_code USING 0 0 work space.
  ENDLOOP.

  PERFORM append_code USING -8 0:
   '      end of t_sumtab.' space.

  PERFORM append_code USING  0 0 space space.

  PERFORM append_code USING:
    0 8 'data: begin of t_errtab occurs 0.' space,
    0 0 'include structure t_sumtab.' space,
   -8 0 'data:   err_flag(3).' space,
    0 0 'data: end of t_errtab.' space.

  PERFORM append_code USING  0 0 space space.

  PERFORM append_code USING  0 8:
   'data: begin of t_ob1tab occurs 0,' space.

  LOOP AT t_ob1fields.
    sy-fdpos = strlen( t800a-objtable ).
    ASSIGN t800a-objtable(sy-fdpos) TO <table>.
    MOVE '$ like $-$,' TO work.
    REPLACE '$' INTO work WITH: t_ob1fields-fieldname,
                                <table>,
                                t_ob1fields-fieldname.
    PERFORM append_code USING 0 0 work space.
  ENDLOOP.

  PERFORM append_code USING -8 0:
   '      end of t_ob1tab.' space.

  PERFORM append_code USING  0 0 space space.

  IF t800a-objtable2 NE space.
    PERFORM append_code USING  0 8:
     'data: begin of t_ob2tab occurs 0,' space.

    LOOP AT t_ob2fields.
      sy-fdpos = strlen( t800a-objtable2 ).
      ASSIGN t800a-objtable2(sy-fdpos) TO <table>.
      MOVE '$ like $-$,' TO work.
      REPLACE '$' INTO work WITH: t_ob2fields-fieldname,
                                  <table>,
                                  t_ob2fields-fieldname.
      PERFORM append_code USING 0 0 work space.
    ENDLOOP.

    PERFORM append_code USING -8 0:
     '      end of t_ob2tab.' space.
  ENDIF.

  PERFORM append_code USING  0 0 space space.

  MOVE 'data: begin of t_err1 occurs 0,' TO work.
  PERFORM append_code USING 0 0 work space.
  MOVE '        RLDNR  like GLU1-RLDNR ,' TO work.
  PERFORM append_code USING 0 0 work space.
  MOVE '        RRCTY  like GLU1-RRCTY ,' TO work.
  PERFORM append_code USING 0 0 work space.
  MOVE '        RVERS  like GLU1-RVERS ,' TO work.
  PERFORM append_code USING 0 0 work space.
  MOVE '        RYEAR  like GLU1-RYEAR ,' TO work.
  PERFORM append_code USING 0 0 work space.
  MOVE '        RBUKRS like GLU1-RBUKRS ,' TO work.
  PERFORM append_code USING 0 0 work space.
  MOVE '      end of t_err1.' TO work.
  PERFORM append_code USING 0 0 work space.

  PERFORM append_code USING  0 0 space space.


ENDFORM.                    " generate_data_declaration
*&---------------------------------------------------------------------*
*&      Form  generate_call_e01_check_objnrs
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM generate_call_e01_check_objnrs .
* not in use
  PERFORM append_code USING:
   0 2 'perform e01_check_objnrs tables' space,
   0 0 'p_rldnr'  space,
   0 0 'p_rrcty'  space,
   0 0 'p_rvers'  space,
   0 0 'p_ryear'  space,
   0 0 'p_rcomp'  space,
   0 0 'p_rbukrs' space,
   0 0 'p_racct'  space,
   0 0 'p_rbusa'  space,
   0 0 'p_rfarea' space,
   0 0 'p_rcntr'  space,
   0 0 'p_rprctr' space,
   0 0 't_err1.'  space,
  -2 0 space space.

ENDFORM.                    " generate_call_e01_check_objnrs
*&---------------------------------------------------------------------*
*&      Form  generate_e01_check_objnrs
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM generate_e01_check_objnrs .

  PERFORM append_code USING:
    0 2 'form e01_check_objnrs tables' space,
    0 0 't_p_rldnr structure p_rldnr' space,
    0 0 't_p_rrcty structure p_rrcty' space,
    0 0 't_p_rvers structure p_rvers' space,
    0 0 't_p_ryear structure p_ryear' space,
    0 0 't_p_rcomp structure p_rcomp' space,
    0 0 't_p_rbukrs structure p_rbukrs' space,
    0 0 't_p_racct structure p_racct' space,
    0 0 't_p_rbusa structure p_rbusa' space,
    0 0 't_p_rfarea structure p_rfarea' space,
    0 0 't_p_rcntr structure p_rcntr' space,
    0 0 't_p_rprctr structure p_rprctr' space,
    0 0 't_err1 structure t_err1.' space,

    0 0 space space.

*Generate select-statement
  work = 'select * from $ into corresponding fields of table t_sumtab'.
  REPLACE '$' INTO work WITH t800a-tab.
  PERFORM append_code USING  0 0 work space.

  PERFORM append_code
    USING  0 0 'where rldnr in t_p_rldnr' space.

  PERFORM create_in_statement USING 'RRCTY'.
  PERFORM create_in_statement USING 'RVERS'.
  PERFORM create_in_statement USING 'RYEAR'.
  PERFORM create_in_statement USING 'RCOMP'.
  PERFORM create_in_statement USING 'RBUKRS'.
  PERFORM append_code USING  0 0 '.' space.
  PERFORM append_code USING  0 0 space space.

*Generate loop on t_sumtab
  PERFORM append_code USING
    0 2 'loop at t_sumtab.' space.

  IF NOT t800a-objtable IS INITIAL.
*Generate check 1) Receiver <-> object table 1
    CLEAR t_obj_fields_sum. REFRESH t_obj_fields_sum.
    CLEAR t_obj_fields_sum_exc. REFRESH t_obj_fields_sum_exc.
    CLEAR t_obj_fields_obj. REFRESH t_obj_fields_obj.
    LOOP AT t_ob1fields WHERE fieldname NE 'OBJNR'.
      h_fieldname = 'R'.
      h_fieldname+1 = t_ob1fields-fieldname.
      CALL FUNCTION 'G_FIELD_READ'
           EXPORTING
                table     = t800a-tab
                fieldname = h_fieldname
           EXCEPTIONS
                not_found = 1
                OTHERS    = 2.
      IF sy-subrc = 0.
        t_obj_fields_sum = t_ob1fields.
        t_obj_fields_sum-fieldname = h_fieldname.
        APPEND t_obj_fields_sum.
        t_obj_fields_obj = t_ob1fields.
        APPEND t_obj_fields_obj.
      ENDIF.
    ENDLOOP.
    PERFORM generate_check_objnr USING 'ROBJNR'
                                       t800a-objtable
                                       't_ob1tab'
                                       '1'.

*Generate check 2) Sender <-> object table 1
    CLEAR t_obj_fields_sum. REFRESH t_obj_fields_sum.
    CLEAR t_obj_fields_sum_exc. REFRESH t_obj_fields_sum_exc.
    CLEAR t_obj_fields_obj. REFRESH t_obj_fields_obj.
    LOOP AT t_ob1fields WHERE fieldname NE 'OBJNR'.
      h_fieldname = 'S'.
      h_fieldname+1 = t_ob1fields-fieldname.
      CALL FUNCTION 'G_FIELD_READ'
           EXPORTING
                table     = t800a-tab
                fieldname = h_fieldname
           EXCEPTIONS
                not_found = 1
                OTHERS    = 2.
      IF sy-subrc = 0.
        t_obj_fields_sum = t_ob1fields.
        t_obj_fields_sum-fieldname = h_fieldname.
        APPEND t_obj_fields_sum.
        t_obj_fields_obj = t_ob1fields.
        APPEND t_obj_fields_obj.
      ELSE.
        t_obj_fields_sum_exc = t_ob1fields.
        APPEND t_obj_fields_sum_exc.
      ENDIF.
    ENDLOOP.
    IF NOT t_obj_fields_sum[] IS INITIAL.
      PERFORM generate_check_objnr USING 'SOBJNR'
                                         t800a-objtable
                                         't_ob1tab'
                                         '2'.
    ENDIF.
  ENDIF.

  IF NOT t800a-objtable2 IS INITIAL.
*Generate check 3) C-Fields <-> object table 2
    CLEAR t_obj_fields_sum. REFRESH t_obj_fields_sum.
    CLEAR t_obj_fields_sum_exc. REFRESH t_obj_fields_sum_exc.
    CLEAR t_obj_fields_obj. REFRESH t_obj_fields_obj.
    LOOP AT t_ob2fields WHERE fieldname NE 'OBJNR'.
      h_fieldname = t_ob2fields-fieldname.
      CALL FUNCTION 'G_FIELD_READ'
           EXPORTING
                table     = t800a-tab
                fieldname = h_fieldname
           EXCEPTIONS
                not_found = 1
                OTHERS    = 2.
      IF sy-subrc = 0.
        t_obj_fields_sum = t_ob2fields.
        t_obj_fields_sum-fieldname = h_fieldname.
        APPEND t_obj_fields_sum.
        t_obj_fields_obj = t_ob2fields.
        APPEND t_obj_fields_obj.
      ENDIF.
    ENDLOOP.
    PERFORM generate_check_objnr USING 'COBJNR'
                                       t800a-objtable2
                                       't_ob2tab'
                                       '3'.
  ENDIF.

  PERFORM append_code USING -2 0 'ENDLOOP.' space.

  PERFORM append_code USING  0 0 space space.

ENDFORM.                    " generate_e01_check_objnrs
*&---------------------------------------------------------------------*
*&      Form  create_in_statement
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1053   text
*----------------------------------------------------------------------*
FORM create_in_statement  USING  cis_field LIKE dfies-fieldname.
  CALL FUNCTION 'G_FIELD_READ'
       EXPORTING
            table     = t800a-tab
            fieldname = cis_field
       EXCEPTIONS
            not_found = 1
            OTHERS    = 2.
  IF sy-subrc = 0.
    work = ' and $ in t_p_$'.
    REPLACE '$' INTO work WITH: cis_field,
                                cis_field.
    PERFORM append_code USING  0 0 work space.
  ENDIF.

ENDFORM.                    " create_in_statement

*---------------------------------------------------------------------*
*  FORM generate_check_objnr
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
*  -->  GCO_OBJNR
*  -->  GCO_OBJTABLE
*  -->  GCO_OBXTAB
*  -->  GCO_NUMBER
*---------------------------------------------------------------------*
FORM generate_check_objnr USING gco_objnr
                                gco_objtable
                                gco_obxtab
                                gco_number.

*A-Check
  CASE gco_number.
    WHEN '1'.
      PERFORM append_code USING:
      0 0 space '1) Receiver <-> object table 1',
      0 0 space 'a) Receiver OBJECT NUMBER with Object table 1'.
    WHEN '2'.
      PERFORM append_code USING:
      0 0 space '2) Sender <-> object table 1',
      0 0 space 'a) Sender OBJECT NUMBER with Object table 1'.
    WHEN '3'.
      PERFORM append_code USING:
      0 0 space '3) Constant fields <-> object table 2',
      0 0 space 'a) C OBJECT NUMBER with Object table 2'.
  ENDCASE.

  work = 'clear $. refresh $.'.
  REPLACE '$' INTO work WITH: gco_obxtab, gco_obxtab.
  PERFORM append_code USING  0 0 work space.

  work = 'if t_sumtab-$ ne ''000000000000000000''.'.
  REPLACE '$' INTO work WITH: gco_objnr.
  PERFORM append_code USING  0 2 work space.

  work =
  'select * from $ into corresponding fields of table $'.
  REPLACE '$' INTO work WITH: gco_objtable,
                              gco_obxtab.
  PERFORM append_code USING  0 0 work space.

  work = 'where objnr = t_sumtab-$.'.
  REPLACE '$' INTO work WITH: gco_objnr.
  PERFORM append_code USING  0 0 work space.

  work = 'describe table $ lines sy-tfill.'.
  REPLACE '$' INTO work WITH: gco_obxtab.
  PERFORM append_code USING  0 0 work space.

  PERFORM create_fill_t_errtab_tfill USING gco_number
                                           'A1'
                                           'A2'.

  work = 'read table $ index 1.'.
  REPLACE '$' INTO work WITH: gco_obxtab.
  PERFORM append_code USING  0 0 work space.

  work = 'IF'.
  LOOP AT t_obj_fields_sum.
    READ TABLE t_obj_fields_obj INDEX sy-tabix.

    work+3 = 't_sumtab-$ ne $-$'.
    REPLACE '$' INTO work WITH: t_obj_fields_sum-fieldname,
                                gco_obxtab,
                                t_obj_fields_obj-fieldname.
    PERFORM append_code USING  0 0 work space.

    work = 'OR'.
  ENDLOOP.
  PERFORM append_code USING  0 2 '.' space.

  PERFORM create_fill_t_errtab_simple USING gco_number
                                            'A3'.

  PERFORM append_code USING -2 0 'ENDIF.' space.
  PERFORM append_code USING -2 0 'ENDIF.' space.
  PERFORM append_code USING -2 2 'ELSE.' space.

  work = 'IF NOT'.
  LOOP AT t_obj_fields_sum.

    work+8 = 't_sumtab-$ is initial'.
    REPLACE '$' INTO work WITH: t_obj_fields_sum-fieldname.
    PERFORM append_code USING  0 0 work space.

    work = 'OR NOT'.
  ENDLOOP.
  PERFORM append_code USING  0 2 '.' space.

  PERFORM create_fill_t_errtab_simple USING gco_number
                                            'A4'.

  PERFORM append_code USING -2 0 'ENDIF.' space.
  PERFORM append_code USING -2 0 'ENDIF.' space.

*B-Check
  CASE gco_number.
    WHEN '1'.
      PERFORM append_code USING:
      0 0 space 'b) Receiver FIELDS with Object table 1'.
    WHEN '2'.
      PERFORM append_code USING:
      0 0 space 'b) Sender FIELDS with Object table 1'.
    WHEN '3'.
      PERFORM append_code USING:
      0 0 space 'b) C-FIELDS with Object table 2'.
  ENDCASE.

  work = 'clear $. refresh $.'.
  REPLACE '$' INTO work WITH: gco_obxtab, gco_obxtab.
  PERFORM append_code USING  0 0 work space.

  work = 'IF NOT'.
  LOOP AT t_obj_fields_sum.

    work+8 = 't_sumtab-$ is initial'.
    REPLACE '$' INTO work WITH: t_obj_fields_sum-fieldname.
    PERFORM append_code USING  0 0 work space.

    work = 'OR NOT'.
  ENDLOOP.
  PERFORM append_code USING  0 2 '.' space.

  work =
  'select * from $ into corresponding fields of table $'.
  REPLACE '$' INTO work WITH: gco_objtable,
                              gco_obxtab.
  PERFORM append_code USING  0 0 work space.

  work = 'where'.
  LOOP AT t_obj_fields_sum.
    READ TABLE t_obj_fields_obj INDEX sy-tabix.

    work+6 = '$ = t_sumtab-$'.
    REPLACE '$' INTO work WITH: t_obj_fields_obj-fieldname,
                                t_obj_fields_sum-fieldname.
    PERFORM append_code USING  0 0 work space.

    work = 'AND'.
  ENDLOOP.
  LOOP AT t_obj_fields_sum_exc.
    work+6 = '$ = space'.
    REPLACE '$' INTO work WITH: t_obj_fields_sum_exc-fieldname.
    PERFORM append_code USING  0 0 work space.

    work = 'AND'.
  ENDLOOP.

  PERFORM append_code USING  0 0 '.' space.

  work = 'describe table $ lines sy-tfill.'.
  REPLACE '$' INTO work WITH: gco_obxtab.
  PERFORM append_code USING  0 0 work space.

  PERFORM create_fill_t_errtab_tfill USING gco_number
                                           'B1'
                                           'B2'.

  work = 'read table $ index 1.'.
  REPLACE '$' INTO work WITH: gco_obxtab.
  PERFORM append_code USING  0 0 work space.


  work = 'if t_sumtab-$ ne $-objnr.'.
  REPLACE '$' INTO work WITH: gco_objnr,
                              gco_obxtab.
  PERFORM append_code USING  0 2 work space.

  PERFORM create_fill_t_errtab_simple USING gco_number
                                            'B3'.

  PERFORM append_code USING -2 0 'ENDIF.' space.

  PERFORM append_code USING -2 0 'ENDIF.' space.

  PERFORM append_code USING -2 2 'ELSE.' space.


  work = 'if t_sumtab-$ ne ''000000000000000000''.'.
  REPLACE '$' INTO work WITH: gco_objnr.
  PERFORM append_code USING  0 2 work space.

  PERFORM create_fill_t_errtab_simple USING gco_number
                                            'B4'.

  PERFORM append_code USING -2 0 'ENDIF.' space.

  PERFORM append_code USING -2 0 'ENDIF.' space.

ENDFORM.                    "generate_check_objnr

*---------------------------------------------------------------------*
*  FORM create_fill_t_errtab_tfill
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
*  -->  CFT_NUMBER
*  -->  CFT_VAR1
*  -->  CFT_VAR2
*---------------------------------------------------------------------*
FORM create_fill_t_errtab_tfill USING cft_number
                                      cft_var1
                                      cft_var2.

  PERFORM append_code USING:
    0 2 'if sy-tfill ne 1.' space,
    0 0 't_errtab = t_sumtab.' space,
    0 2 'if sy-tfill = 0.' space.

  work = 't_errtab-err_flag = ''$$''.'.
  REPLACE '$' INTO work WITH: cft_number,
                              cft_var1.
  PERFORM append_code USING  0 0 work space.

  PERFORM append_code USING:
    -2 2 'else.' space.

  work = 't_errtab-err_flag = ''$$''.'.
  REPLACE '$' INTO work WITH: cft_number,
                              cft_var2.
  PERFORM append_code USING  0 0 work space.

  PERFORM append_code USING:
    -2 0 'endif.' space,
     0 0 'append t_errtab.' space,
    -2 2 'else.' space.
ENDFORM.                    "create_fill_t_errtab_tfill

*---------------------------------------------------------------------*
*  FORM create_fill_t_errtab_simple
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
*  -->  CFT_NUMBER
*  -->  CFT_VAR1
*  -->  CFT_VAR2
*---------------------------------------------------------------------*
FORM create_fill_t_errtab_simple USING cft_number
                                      cft_var.

  PERFORM append_code USING:
    0 0 't_errtab = t_sumtab.' space.

  work = 't_errtab-err_flag = ''$$''.'.
  REPLACE '$' INTO work WITH: cft_number,
                              cft_var.
  PERFORM append_code USING  0 0 work space.

  PERFORM append_code USING:
    0 0 'append t_errtab.' space.
ENDFORM.                    "create_fill_t_errtab_simple

*&---------------------------------------------------------------------*
*&      Form  generate_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM generate_list .

  PERFORM append_code USING  0 0 space 'Print result list'.

  PERFORM append_code USING:
    0 2 'LOOP AT t_errtab.' space,
    0 0 'CLEAR t_err1.' space,
    0 0 'MOVE t_errtab-RLDNR  TO t_err1-RLDNR.' space,
    0 0 'MOVE t_errtab-RRCTY  TO t_err1-RRCTY.' space,
    0 0 'MOVE t_errtab-RVERS  TO t_err1-RVERS.' space,
    0 0 'MOVE t_errtab-RYEAR  TO t_err1-RYEAR.' space.

  IF h_org_name = 'RCOMP'.
    work = 'MOVE t_errtab-RCOMP TO t_err1-RBUKRS.'.
  ENDIF.
  IF h_org_name = 'RBUKRS'.
    work = 'MOVE t_errtab-RBUKRS TO t_err1-RBUKRS.'.
  ENDIF.
  IF h_org_name = 'BUKRS'.
    work = 'MOVE t_errtab-BUKRS TO t_err1-BUKRS.'.
  ENDIF.
  PERFORM append_code USING  0 0 work space.

  work = 'COLLECT t_err1.'.
  PERFORM append_code USING  0 0 work space.
  work = 'ENDLOOP.'.
  PERFORM append_code USING  0 0 work space.
  PERFORM append_code USING  0 0 space space.


  PERFORM append_code USING:
    0 0 'sort t_errtab by err_flag.' space,
    0 0 'read table t_errtab index 1.' space,
    0 2 'if sy-subrc = 0.' space.

*  work = 'message I016(GU) with ''Inconsistencies found''.'.
*  PERFORM APPEND_CODE USING 0 0 work space.

  PERFORM append_code USING:
    0 0 'perform print_list using ''1A1'' text-1A1.' space,
    0 0 'perform print_list using ''1A2'' text-1A2.' space,
    0 0 'perform print_list using ''1A3'' text-1A3.' space,
    0 0 'perform print_list using ''1A4'' text-1A4.' space,
    0 0 'perform print_list using ''1B1'' text-1B1.' space,
    0 0 'perform print_list using ''1B2'' text-1B2.' space,
    0 0 'perform print_list using ''1B3'' text-1B3.' space,
    0 0 'perform print_list using ''1B4'' text-1B4.' space,
    0 0 'perform print_list using ''2A1'' text-2A1.' space,
    0 0 'perform print_list using ''2A2'' text-2A2.' space,
    0 0 'perform print_list using ''2A3'' text-2A3.' space,
    0 0 'perform print_list using ''2A4'' text-2A4.' space,
    0 0 'perform print_list using ''2B1'' text-2B1.' space,
    0 0 'perform print_list using ''2B2'' text-2B2.' space,
    0 0 'perform print_list using ''2B3'' text-2B3.' space,
    0 0 'perform print_list using ''2B4'' text-2B4.' space,
    0 0 'perform print_list using ''3A1'' text-3A1.' space,
    0 0 'perform print_list using ''3A2'' text-3A2.' space,
    0 0 'perform print_list using ''3A3'' text-3A3.' space,
    0 0 'perform print_list using ''3A4'' text-3A4.' space,
    0 0 'perform print_list using ''3B1'' text-3B1.' space,
    0 0 'perform print_list using ''3B2'' text-3B2.' space,
    0 0 'perform print_list using ''3B3'' text-3B3.' space,
    0 0 'perform print_list using ''3B4'' text-3B4.' space,
   -2 2 'else.' space.

*  work = 'message I016(GU) with ''No inconsistencies found''.'.
*  PERFORM APPEND_CODE USING 0 0 work space.

  PERFORM append_code USING:
   -2 0 'endif.' space,
   -2 0 'endform.' space.

  PERFORM append_code USING  0 0 space space.

ENDFORM.                    " generate_list

*&---------------------------------------------------------------------*
*&      Form  generate_print_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM generate_print_list .
  PERFORM append_code USING:
    0 2 'FORM print_list USING pl_err_flag pl_text.' space,
    0 2 'loop at t_errtab where err_flag = pl_err_flag.' space,
    0 0 'exit.' space,
   -2 0 'endloop.' space,
    0 2 'if sy-subrc = 0.' space,
    0 0 'write / pl_text color col_negative.' space,
   -2 0 'endif.' space.
*    0 2 'loop at t_errtab where err_flag = pl_err_flag.' space,
*    0 0 'write: /' space.

*  LOOP AT t_sumfields.
*    work = 't_errtab-$, ''!'','.
*    REPLACE '$' INTO work WITH: t_sumfields-fieldname.
*    PERFORM append_code USING  0 0 work space.
*  ENDLOOP.
*  PERFORM append_code USING  0 0 ''' ''.' space.
*  PERFORM append_code USING -2 0 'endloop.' space.
  PERFORM append_code USING -2 0 'endform.' space.
ENDFORM.                    " generate_print_list

*---------------------------------------------------------------------*
*       FORM APPEND_CODE                                              *
*---------------------------------------------------------------------*
*       ABAP/4-Coding-Zeilen in Tabelle CODE anh?gen                 *
*---------------------------------------------------------------------*
*  -->  APP_OFF_BEF  Differenzabstand zum linken Rand vor APPEND      *
*  -->  APP_OFF_AFT  Differenzabstand zum linken Rand nach APPEND     *
*  -->  APP_CODE     Code-Zeile                                       *
*  -->  APP_COMMENT  Kommentar                                        *
*---------------------------------------------------------------------*
FORM append_code USING app_off_bef app_off_aft app_code app_comment.
  DATA: BEGIN OF splitted_code OCCURS 1,
          line(72),
        END OF splitted_code.
  DATA: unsplitted_line(200).

  CLEAR unsplitted_line.
  CLEAR splitted_code. REFRESH splitted_code.
  CLEAR code-line.

  offset_code = offset_code + app_off_bef.
*Kein Coding
  IF app_code = space.
*Kommentar
    IF app_comment NE space.
      code = '*'.
      code+1 = app_comment.
      IF code+1(2) = space OR
         code+1(2) = '--'.
        code+70 = '* '.
      ENDIF.
    ENDIF.
    APPEND code.
  ELSE.
*Evtl. Zeilensplit
    WRITE app_code TO unsplitted_line+offset_code.
    CALL FUNCTION 'G_SPLIT_LINE'
         EXPORTING
              input_line   = unsplitted_line
         TABLES
              export_lines = splitted_code
         EXCEPTIONS
              OTHERS       = 1.
*Interne Tabelle Splitted_code abarbeiten
    LOOP AT splitted_code.
      code-line = splitted_code-line.
      IF app_comment NE space AND
      code+39 = space.          "Coding nie mit Kommentar ?erschreiben
        code+39(1) = '"'.
        code+40 = app_comment.
      ENDIF.
      APPEND code.                     "Zeile anh?gen
    ENDLOOP.
  ENDIF.
  offset_code = offset_code + app_off_aft.
ENDFORM.                    "APPEND_CODE



*---------------------------------------------------------------------*
*  FORM check_obj     * *
*---------------------------------------------------------------------*
*  Checks Inconsistencies in Object Tables                            *
*---------------------------------------------------------------------*

FORM comp_sum USING value(org_name) LIKE h_org_name
                    value(p_rldnr)  LIKE p_rldnr
                    value(p_rrcty)  LIKE p_rrcty
                    value(p_rvers)  LIKE p_rvers
                    value(p_rbukrs) LIKE p_rbukrs
                    value(p_rcomp)  LIKE p_rcomp
                    value(p_ryear)  LIKE p_ryear
           CHANGING it_vasci LIKE it_err2[].

  DATA: mem_tab LIKE abaplist OCCURS 100 WITH HEADER LINE.

  IF org_name = 'RBUKRS'.
    IF ( p_rbukrs IS INITIAL ).
      SUBMIT rguslsep EXPORTING LIST TO MEMORY AND RETURN
                  WITH ledger  = p_rldnr
                  WITH satzart = p_rrcty
                  WITH version = p_rvers
                  WITH jahr    = p_ryear.
    ELSE.
      SUBMIT rguslsep EXPORTING LIST TO MEMORY AND RETURN
                  WITH ledger  = p_rldnr
                  WITH satzart = p_rrcty
                  WITH version = p_rvers
                  WITH bukreis = p_rbukrs
                  WITH jahr    = p_ryear.
    ENDIF.
  ENDIF.

  IF org_name = 'RCOMP'.
    IF ( p_rcomp IS INITIAL ).
      SUBMIT rguslsep EXPORTING LIST TO MEMORY AND RETURN
                  WITH ledger  = p_rldnr
                  WITH satzart = p_rrcty
                  WITH version = p_rvers
                  WITH jahr    = p_ryear.
    ELSE.
      SUBMIT rguslsep EXPORTING LIST TO MEMORY AND RETURN
                 WITH ledger  = p_rldnr
                 WITH satzart = p_rrcty
                 WITH version = p_rvers
                 WITH company = p_rcomp
                 WITH jahr    = p_ryear.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'LIST_FROM_MEMORY'
       TABLES
            listobject = mem_tab
       EXCEPTIONS
            not_found  = 1
            OTHERS     = 2.
  IF sy-subrc <> 0.
  ELSE.
  ENDIF.


  IF NOT mem_tab[] IS INITIAL.
    CALL FUNCTION 'LIST_TO_ASCI'
         TABLES
              listasci           = it_vasci
              listobject         = mem_tab
         EXCEPTIONS
              empty_list         = 1
              list_index_invalid = 2
              OTHERS             = 3.
  ENDIF.


  CALL FUNCTION 'LIST_FREE_MEMORY'.
  FREE mem_tab.

ENDFORM.                    "comp_sum


*---------------------------------------------------------------------*
*       FORM check_doc_exist                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM check_doc_exist  USING value(l_org_name) LIKE h_org_name
                            value(l_convdocs) LIKE z_poolcv
                      CHANGING lt_err_tab LIKE it_missdoc[].

  DATA: lt_bkpf LIKE bkpf,
        ld_exist   TYPE i,
        li_exist   TYPE i,
        ls_err     TYPE tt_err4,
        lt_mul_tab TYPE tt_err4 OCCURS 10.
*        lt_err_tab TYPE tt_err4 OCCURS 10.

  DATA: mem_awkey like BKPF-awkey.
  DATA: mem_doublcheck Type i.

  FIELD-SYMBOLS:  <fs_bkpf>  TYPE bkpf.

*Exclude FI Statistical Postings / Noted items
  MOVE: 'E'      TO r_activ-sign,
        'EQ'     TO r_activ-option,
        'RFST'   TO r_activ-low.
  APPEND  r_activ.

  MOVE: 'E'      TO r_activ-sign,
        'EQ'     TO r_activ-option,
        'RFIG'   TO r_activ-low.
  APPEND  r_activ.

  LOOP at  r_rbukrs.
    p_rbukrs = r_rbukrs-low.
    mem_awkey = SPACE.
    mem_doublcheck = 1.
    SELECT * FROM bkpf INTO CORRESPONDING FIELDS OF lt_bkpf
            WHERE bukrs = p_rbukrs
            AND   gjahr = p_ryear
            AND   belnr IN r_belnr
            AND   blart IN r_doctt
            AND   awtyp IN r_awtyp
            AND   glvor IN r_activ
            AND   monat IN r_perid
            AND   bstat IN r_bstat
            AND   bstat <> 'V'
            ORDER by GJAHR BUKRS AWKEY .

      IF mem_awkey = lt_bkpf-awkey.
        mem_doublcheck = mem_doublcheck + 1.
      ELSE.
        mem_doublcheck = 0.
      ENDIF.

      mem_awkey = lt_bkpf-awkey.

      ASSIGN lt_bkpf TO <fs_bkpf>.
      PERFORM spl_exist USING    <fs_bkpf>
                                 l_org_name
                                 l_convdocs
                        CHANGING ld_exist.

      IF ( ld_exist > 0 ) and ( ld_exist < mem_doublcheck ).
        IF NOT ( lt_bkpf-bstat CA 'ABZ' ).
          MOVE-CORRESPONDING <fs_bkpf> TO ls_err.
          APPEND ls_err TO lt_err_tab.
        ENDIF.
      ENDIF.


      IF ld_exist < 1.
        PERFORM check_bsegli USING  <fs_bkpf>
                                     z_quick
                            CHANGING li_exist.
        IF li_exist > 0.
          MOVE-CORRESPONDING <fs_bkpf> TO ls_err.
          APPEND ls_err TO lt_err_tab.
        ENDIF.
      ELSEIF ld_exist > 1.
        MOVE-CORRESPONDING <fs_bkpf> TO ls_err.
        APPEND ls_err TO lt_mul_tab.
      ENDIF.

    ENDSELECT.
  ENDLOOP.

  sort lt_err_tab by gjahr bukrs belnr.

ENDFORM.                    "check_doc_exist

*---------------------------------------------------------------------*
*       FORM check_bsegli                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  IS_BKPF                                                       *
*  -->  Z_CHKBSEG                                                     *
*  -->  LI_EXIST                                                      *
*---------------------------------------------------------------------*
FORM check_bsegli USING    is_bkpf   TYPE bkpf
                           z_chkbseg LIKE rkec1-test
                  CHANGING li_exist TYPE i.

  li_exist = 0.
  IF z_chkbseg IS INITIAL.
    SELECT COUNT( * ) FROM  bseg
                      INTO  li_exist
                      WHERE bukrs = is_bkpf-bukrs
                       AND  belnr = is_bkpf-belnr
                       AND  gjahr = is_bkpf-gjahr.
  ELSE.
    IF is_bkpf-bstat CA 'ABZ'.
      li_exist = 0.
    ELSE.
      li_exist = 1.
    ENDIF.
  ENDIF.

ENDFORM.                    "check_bsegli


*---------------------------------------------------------------------*
*       FORM spl_exist                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  IS_BKPF                                                       *
*  -->  ED_EXIST                                                      *
*---------------------------------------------------------------------*
FORM spl_exist USING    is_bkpf  TYPE bkpf
                        value(l_org_name) LIKE h_org_name
                        value(l_convdocs)
               CHANGING ed_exist TYPE i.

  DATA: ld_cnt   TYPE i,
        ld_awref LIKE acchd-awref,
        ld_aworg LIKE acchd-aworg,
        ld_awtyp LIKE acchd-awtyp.

  IF is_bkpf-awtyp IS INITIAL.
    ld_awtyp = 'BKPF'.
  ELSE.
    ld_awtyp = is_bkpf-awtyp.
  ENDIF.
  IF is_bkpf-awkey IS INITIAL. " ???
    ld_awref      = is_bkpf-belnr.
    ld_aworg+0(4) = is_bkpf-bukrs.
    ld_aworg+4(4) = is_bkpf-gjahr.
  ELSE.
    ld_awref = is_bkpf-awkey+0(10).
    ld_aworg = is_bkpf-awkey+10(10).
  ENDIF.

  IF ( h_org_name = 'RBUKRS' ) AND ( l_convdocs IS INITIAL ).
    SELECT COUNT(*) FROM glidxa INTO ld_cnt
                              WHERE awref = ld_awref
                              AND   awtyp = ld_awtyp
                              AND   aworg = ld_aworg
                              AND   rldnr = p_rldnr
                              AND   budat = is_bkpf-budat.
  ELSE.
    SELECT COUNT(*) FROM glidxa INTO ld_cnt
                           WHERE awref = ld_awref
                           AND   awtyp = ld_awtyp
                           AND   aworg = ld_aworg
                           AND   rldnr = p_rldnr.
  ENDIF.
  ed_exist = ld_cnt.



ENDFORM.                    "spl_exist


*---------------------------------------------------------------------*
*       FORM ERR_OUTPUT1                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM err_output1.

  WRITE /(90) '1) Selection:' COLOR COL_KEY LEFT-JUSTIFIED .
  SKIP 1.

  WRITE:/ '    Ledger            :', p_rldnr, '--> '.
  WRITE: t881-tab.
  WRITE:/ '    Version           :', p_rvers.
  WRITE:/ '    Record Type       :', p_rrcty.
  IF h_org_name = 'RBUKRS'.
    WRITE:/ '    Company Code      :', p_rbukrs.
  ELSE.
    WRITE:/ '    Company           :', p_rcomp.
  ENDIF.
  WRITE:/ '    Fiscal Year       :', p_ryear.
  WRITE:/ '    More Selections   :'.
  IF NOT ( r_perid IS INITIAL ).
    WRITE 'Period '.
  ENDIF.
  IF NOT ( r_belnr IS INITIAL ).
    WRITE 'DocNumber'.
  ENDIF.
  IF NOT ( r_doctt IS INITIAL ).
    WRITE 'DocType '.
  ENDIF.
  IF NOT ( r_awtyp IS INITIAL ).
    WRITE 'DocType '.
  ENDIF.
  IF NOT ( r_activ IS INITIAL ).
    WRITE 'Activity '.
  ENDIF.
  IF NOT ( r_bstat IS INITIAL ).
    WRITE 'DocStatus '.
  ENDIF.
  IF NOT ( z_quick IS INITIAL ).
    WRITE 'Fast Search'.
  ENDIF.


  SKIP 1.
  WRITE:/ ' - The following checks have been processed:'.
  IF p_chck01 = 'X'.
    WRITE:/ '    - Object/Summary Table Inconsistency'.
  ENDIF.
  IF p_chck02 = 'X'.
    WRITE:/ '    - Inconsistency between Summary- and Line Item Table'.
  ENDIF.
  IF p_chck03 = 'X'.
    WRITE:/ '    - Cust. settings which could lead to differences'.
  ENDIF.
  IF p_chck04 = 'X'.
    WRITE:/ '    - Missing FI documents (FI -> FI-SL)'.
  ENDIF.
  IF p_chck06 = 'X'.
    WRITE:/ '    - Non FI documents (local SL-docs, CO documents)'.
  ENDIF.
  IF P_CHCK05 = 'X'.
    WRITE:/ '    - Double Keys in Object- OR Null Values in Sum.Table:'.
  ENDIF.

  SKIP 2.

  WRITE /(90) '2) Summarization of technical analyses :'
         COLOR COL_KEY LEFT-JUSTIFIED .

  SKIP 1.

  IF p_chck01 = 'X'.
    WRITE:/ '    Check 1: Object/Summary Table Inconsistency:'.
    PERFORM val_error USING err_chck1.
  ENDIF.
  IF p_chck02 = 'X'.
    WRITE:/ '    Check 2: Inc. betw Sum- and Line Item Table:'.
    PERFORM val_error USING err_chck2.
  ENDIF.
  IF p_chck03 = 'X'.
    WRITE:/ '    Check 3: Customizing settings              :'.
    PERFORM val_error USING err_chck3.
  ENDIF.
  IF p_chck04 = 'X'.
    WRITE:/ '    Check 4: Missing documents (FI -> FI-SL)   :'.
    IF Z_LI_EXIST = 'X'.
      PERFORM val_error USING err_chck4.
    else.
      WRITE 'Check not processed: No Line items in SL table'.
    ENDIF.
  ENDIF.
  IF P_CHCK05 = 'X'.
    WRITE:/ '    Check 5: Double Keys/Null Values Obj/SumTab:'.
    PERFORM VAL_ERROR USING ERR_CHCK5.
  ENDIF.
  IF p_chck06 = 'X'.
    WRITE:/ '    Check 6: Non FI documents (local/CO-docs.) :'.
    IF Z_LI_EXIST = 'X'.
      PERFORM val_error USING err_chck6.
    else.
      WRITE 'Check not processed: No Line items in SL table'.
    ENDIF.
  ENDIF.
ENDFORM.                    "err_output1


*---------------------------------------------------------------------*
*       FORM err_output2                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM err_output2.

  DATA:  wa_nondoc1 TYPE tt_err5.
  DATA:  wa_nondoc2 TYPE tt_err6.

  SKIP 2.
  WRITE /(90) '3)  Details of technical analyses :'
         COLOR COL_KEY LEFT-JUSTIFIED .
  WRITE:/ .

* Output Check 1
  IF ( p_chck01 = 'X' ) AND ( err_chck1 = 'F' ).
    WRITE:/ '  '.
    FORMAT INTENSIFIED COLOR = 3.
    WRITE 'Check 1:'.
    FORMAT COLOR INTENSIFIED OFF.
    WRITE: 'Object/Summary Table Inconsistency'.
    IF NOT zmsg01 IS INITIAL.
      SKIP 1.
     WRITE / '- Please note: A program error has occured in this check'.
      WRITE /3 'Error Text: ' COLOR COL_NEGATIVE .
      WRITE 16 zmsg01 INTENSIFIED ON LEFT-JUSTIFIED .
      WRITE:/.
      WRITE /3 'Please skip this check. Alternatively, you can use' .
      WRITE:/3 'report RGUREP11 for this check (see note 434734)'.
      SKIP 2.
    ENDIF.

    IF zmsg01 IS INITIAL.
      SKIP 1.
      WRITE:/ '       Ldr RC Ver Year CoCde'.
      WRITE:/ ' '.
      ULINE AT 7(23).

      LOOP AT t_err1.
        WRITE:/ '        ', t_err1-rldnr, t_err1-rrcty, t_err1-rvers,
                t_err1-ryear, t_err1-rbukrs .
        IF SY-TABIX > 50.
          SKIP 2.
          WRITE 'Selection restricted to 50 lines'.
          SKIP 1.
          EXIT.
        ENDIF.
      ENDLOOP.
      SKIP 2.
      WRITE:/3 'Detailed List: See also note 434734'.
      SKIP 3.
    ENDIF.
  ENDIF.

* Output Check 2
  IF ( p_chck02 = 'X' ) AND ( err_chck2 = 'F' ).
    SKIP 2.
    WRITE:/ '  '.
    FORMAT INTENSIFIED COLOR = 3.
    WRITE 'Check 2:'.
    FORMAT COLOR INTENSIFIED OFF.
    WRITE: 'Inconst. between Sumary and Line Item Table'.
    SKIP 1.
    LOOP AT it_err2.
      WRITE AT /5 it_err2.
    ENDLOOP.
  ENDIF.

* Output Check 3
  IF ( p_chck03 = 'X' ) AND ( err_chck3 = 'F' ).
    SKIP 2.
    WRITE:/ '  '.
    FORMAT INTENSIFIED COLOR = 3.
    WRITE 'Check 3:'.
    FORMAT COLOR INTENSIFIED OFF.
    WRITE: 'Customizing settings which may cause inconsistencies'.
    SKIP 1.
    LOOP AT cust_out into ZTEXT2.
      IF ZTEXT2 is INITIAL.
        SKIP 1.
      ELSE.
        Write:/ ZTEXT2.
      ENDIF.
    ENDLOOP.
    SKIP 1.
  ENDIF.

* Output Check 4
  IF p_chck04 = 'X' AND ( err_chck4 = 'F' ).
    SKIP 2.
    WRITE:/ '  '.
    FORMAT INTENSIFIED COLOR = 3.
    WRITE 'Check 4:'.
    FORMAT COLOR INTENSIFIED OFF.
    WRITE: 'Missing documents (FI -> FI-SL)'.
    SKIP 1.
    WRITE AT /5 'Year CoCd DocNo      AWTYP GLVOR'.
    WRITE AT /1(90) sy-uline.
    LOOP AT it_missdoc INTO wa_missdoc.
      WRITE AT /5 wa_missdoc-GJAHR.
      WRITE wa_missdoc-BUKRS.
      WRITE wa_missdoc-belnr.
      WRITE wa_missdoc-awtyp.
      WRITE wa_missdoc-glvor.
    ENDLOOP.
  ENDIF.

* Output Check 6
  IF p_chck06 = 'X' AND ( err_chck6 = 'F' ).
    SKIP 2.
    WRITE:/ '  '.
    FORMAT INTENSIFIED COLOR = 3.
    WRITE 'Check 6:'.
    FORMAT COLOR INTENSIFIED OFF.
    WRITE 'Non FI documents'.
    SKIP 1.
    DESCRIBE TABLE it_nondoc1 LINES zlines.
    if zlines < 500.
      IF ( h_org_name = 'RCOMP' ).
        WRITE AT /5 'Activity Ldgr  Comp.  Year  Doc.Number'.
      ELSE.
        WRITE AT /5 'Activity Ldgr  CoCde  Year  Doc.Number'.
      ENDIF.
      WRITE AT /1(90) sy-uline.
      LOOP AT it_nondoc1 INTO wa_nondoc1.
        WRITE AT /6  wa_nondoc1-ACTIV.
        WRITE AT 15 wa_nondoc1-RLDNR.
        WRITE ' '.
        IF ( h_org_name = 'RCOMP' ).
          WRITE wa_nondoc1-RCOMP LEFT-JUSTIFIED .
        ELSE.
          WRITE wa_nondoc1-RBUKRS.
          WRITE ' '.
        ENDIF.
        WRITE wa_nondoc1-RYEAR.
        WRITE ' '.
        WRITE wa_nondoc1-DOCNR.
      ENDLOOP.
    else.
      IF ( h_org_name = 'RCOMP' ).
        WRITE AT /5 'Activity Ldgr Comp.  Year Number of documents'.
      ELSE.
        WRITE AT /5 'Activity Ldgr CoCde  Year Number of documents'.
      ENDIF.
      WRITE AT /1(90) sy-uline.
      LOOP AT it_nondoc2 INTO wa_nondoc2.
        WRITE AT /6  wa_nondoc2-ACTIV.
        WRITE AT 15 wa_nondoc2-RLDNR.
        IF ( h_org_name = 'RCOMP' ).
          WRITE wa_nondoc2-RCOMP.
        ELSE.
          WRITE wa_nondoc2-RBUKRS.
        ENDIF.
        WRITE wa_nondoc2-RYEAR.
        WRITE wa_nondoc2-count1.
      ENDLOOP.
    endif.
  ENDIF.

* Output Check 5
  IF ( P_CHCK05 = 'X' ) AND ( ERR_CHCK5 = 'F' ).
    SKIP 2.
    WRITE:/ '  '.
    FORMAT INTENSIFIED COLOR = 3.
    WRITE 'Check 5:'.
    FORMAT COLOR INTENSIFIED OFF.
    WRITE: 'NULL Values in Summary Table OR Double Key in Obj. Table1'.
    SKIP 1.
    IF NOT Z_MSG1 IS INITIAL.
      FORMAT INTENSIFIED COLOR = 6.
      WRITE /7 Z_MSG1 .
      FORMAT COLOR INTENSIFIED OFF.
      SKIP 1.
    ENDIF.

    IF NOT zmsg02 IS INITIAL.
      SKIP 1.
     WRITE / '- Please note: A program error has occured in this check'.
      WRITE /3 'Error Text: ' COLOR COL_NEGATIVE .
      WRITE 16 zmsg02 INTENSIFIED ON LEFT-JUSTIFIED .
      WRITE:/.
      WRITE /3 'Please skip this check and open an OSS message.'.
      SKIP 2.
    ENDIF.

    DESCRIBE TABLE IT_CLNT0 LINES zlines.
    IF zlines > 0.
      SKIP 1.
      WRITE:/7 'Problem                 '.
      WRITE 'Client   Number defect of entries'.
      WRITE:/ ' '.
      ULINE AT 7(60).
      LOOP AT IT_CLNT0.
        IF IT_CLNT0-ISSUE = 'DUPKY1'.
          WRITE:/7 'Double Key ObjTable 1   :'.
        ENDIF.
      IF IT_CLNT0-ISSUE = 'DUPKY2'.
          WRITE:/7 'Double Key ObjTable 2   :'.
       ENDIF.
       IF IT_CLNT0-ISSUE = 'NULL'.
          WRITE:/7 'NULL Values in Sum.Table:'.
        ENDIF.
        WRITE IT_CLNT0-CLIENT.
        WRITE (14) IT_CLNT0-COUNT1 .
      ENDLOOP.
    ENDIF.

  ENDIF.


ENDFORM.                    "err_output2


*---------------------------------------------------------------------*
*       FORM VAL_ERROR                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE(VAL_ERROR)                                              *
*---------------------------------------------------------------------*
FORM val_error USING value(val_error) TYPE boolean.
  IF val_error = 'T'.
    WRITE ICON_GREEN_LIGHT AS ICON .
    WRITE: 'OK'.
  ELSE.
    WRITE ICON_YELLOW_LIGHT AS ICON .
    WRITE 'ERROR. See detail list'.
  ENDIF.

ENDFORM.                    "val_error


*---------------------------------------------------------------------*
*       FORM CHECK_CUST1                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE(P_RLDNR)                                                *
*  -->  VALUE(P_RRCTY)                                                *
*  -->  VALUE(P_RVERS)                                                *
*  -->  VALUE(P_RYEAR)                                                *
*  -->  VALUE(P_RBUKRS)                                               *
*  -->  VALUE(P_RCOMP)                                                *
*  -->  VALUE(H_ORG_NAME)                                             *
*  -->  VALUE(T800A)                                                  *
*  -->  VALUE(T881)                                                   *
*  -->  CUST_OUT                                                      *
*  -->  CUST_ERR                                                      *
*---------------------------------------------------------------------*
FORM CHECK_CUST1 USING VALUE(p_rldnr)
                       VALUE(p_rrcty)
                       VALUE(p_rvers)
                       VALUE(p_ryear)
                       VALUE(p_rbukrs)
                       VALUE(p_rcomp)
                       VALUE(h_org_name)
                       VALUE(T800a) like T800a
                       VALUE(T881)  like T881
                    changing cust_out LIKE cust_out[]
                             cust_err.

  TABLES: t882, t886a, t886b, t882c,
          t022, t800m, t886c, t888m,
          GLS1.

  DATA: zline1 LIKE sy-tabix.
  DATA: zline2 LIKE sy-tabix.
  DATA: zline3 LIKE sy-tabix.
  DATA: ZCLNT LIKE GLU1-RCLNT.
  DATA: ZTEXT(90) TYPE c.
  DATA: FI_fiscva LIKE t882-periv.
  DATA: SL_fiscva LIKE t882-periv.

  DATA: BEGIN OF it_ledger  OCCURS 100,
          rldnr      LIKE glu1-rldnr,
          ttable     LIKE t800a-tab,
          atable     LIKE t800a-tab,
          otable     LIKE t800a-tab,
          bukrs      LIKE glu1-rbukrs,
          comp       LIKE glu1-rcomp,
          bukz1      LIKE t882-bukz,
          bukz2      LIKE T886B-BUKZ,
          slperiv    LIKE t882-periv,
          glperiv    LIKE t882-periv,
          mbuch      LIKE t886b-mbuch,
          glsip1     LIKE t886b-glsip,
          glsip2     LIKE t886b-glsip,
          glsip3     LIKE t886b-glsip,
          activ      LIKE t886b-activity,
          glactiv(1) TYPE c,
          feldmodif  LIKE t888-feldmodif,
          exit       LIKE t888m-exit,
          fixmodi(1) TYPE c,
        END OF it_ledger.

  DATA: BEGIN OF wa_cust,
       NOLINE(1) TYPE c,  " 'X' = No L. ITEM Writing for all GL activit.
          fixmd1(1) TYPE c,  " 'X' = No Standard fixed Field Movement
          fixmd2(1) TYPE c,  " 'X' = User Exits in fixed Field Movement
          Select(1) TYPE c,  " 'X' = Ledger Selection Could be activ
          URACCT(1) TYPE c,  " 'X' = User Exit in RACCT
       UHKONT(1) TYPE c,  " 'X' = HKONT <> RACCT (ACCOUNT not in CoCode)
          GACTIV(1) TYPE c,  " 'X' = GL activity missing
          NACTIV(1) TYPE c,  " 'X' = NON GL activity assigned
          LOCDOC(1) TYPE c,  " 'X' = Local Postings in Line Item Table
         END of wa_cust.

  DATA it_t800m LIKE t800m OCCURS 0 WITH HEADER LINE.
  DATA it_t888m LIKE t888m OCCURS 0 WITH HEADER LINE.

  RANGES  rl_activ FOR t022t-activity.

  MOVE SPACE TO wa_cust.


*FILL GL activities selection

  SELECT * FROM t022 WHERE tab_1 = 'ACCHD'
                     AND   notused = ' '.
    MOVE: 'I'      TO rl_activ-sign,
          'EQ'     TO rl_activ-option,
           t022-activity TO rl_activ-low.
    APPEND rl_activ.
  ENDSELECT.


*Check GL activities
  IF h_org_name = 'RBUKRS' OR h_org_name = 'BUKRS'.
    SELECT SINGLE * FROM t882 WHERE rldnr = p_rldnr
                             AND   bukrs = p_rbukrs.

    SELECT SINGLE * FROM t001 WHERE bukrs = t882-bukrs.

* SELECT GL activities assigned to company code
    SELECT  * FROM t886b
              WHERE  rldnr    = p_rldnr
                AND  bukrs    = p_rbukrs
                AND  activity IN rl_activ.

      it_ledger-rldnr   = t881-rldnr.
      it_ledger-ttable  = t800a-tab.
      it_ledger-atable  = t800a-ntable.
      it_ledger-otable  = t800a-objtable.
      it_ledger-bukrs   = t882-bukrs.
      it_ledger-slperiv = t882-periv.
      it_ledger-glperiv = t001-periv.
      it_ledger-comp    = space.
      it_ledger-bukz1   = t882-bukz.
      it_ledger-bukz2   = T886B-BUKZ.
      it_ledger-mbuch   = t886b-mbuch.
      it_ledger-glsip1  = t881-glsip.
      it_ledger-glsip2  = t882-glsip.
      it_ledger-glsip3  = t886b-glsip.
      it_ledger-activ   = t886b-activity.
      it_ledger-glactiv = 'X'.
      it_ledger-feldmodif = t886b-feldmodif.
      APPEND it_ledger.
    ENDSELECT.

* SELECT GL activities assigned to ledger
    SELECT  * FROM t886a
              WHERE  rldnr    = p_rldnr
              AND    activity IN rl_activ.

      READ TABLE it_ledger WITH KEY
               rldnr  = t881-rldnr
               bukrs  = t882-bukrs
               activ  = t886a-activity.

      IF sy-subrc > 0.
        it_ledger-rldnr   = t881-rldnr.
        it_ledger-ttable  = t800a-tab.
        it_ledger-atable  = t800a-ntable.
        it_ledger-otable  = t800a-objtable.
        it_ledger-bukrs   = t882-bukrs.
        it_ledger-slperiv = t882-periv.
        it_ledger-glperiv = t001-periv.
        it_ledger-comp    = space.
        it_ledger-bukz1   = t882-bukz.
        it_ledger-bukz2   = T886a-BUKZ.
        it_ledger-mbuch   = t886a-mbuch.
        it_ledger-glsip1  = t881-glsip.
        it_ledger-glsip2  = t882-glsip.
        it_ledger-glsip3  = t886a-glsip.
        it_ledger-activ   = t886a-activity.
        it_ledger-glactiv = 'X'.
        it_ledger-feldmodif = t886a-feldmodif.
        APPEND it_ledger.
      ENDIF.

    ENDSELECT.

* Check missing Activity
    DESCRIBE TABLE rl_activ   LINES zline1.
    DESCRIBE TABLE it_ledger LINES zline2.
    IF ( zline1 > zline2 ).
      wa_cust-GACTIV = 'X'.
    ENDIF.


* NON GL activities assigned to company code
    SELECT  * FROM t886b
              WHERE  rldnr          = p_rldnr
                     AND   bukrs    = p_rbukrs
                     AND   activity NOT IN rl_activ.
      it_ledger-rldnr   = t881-rldnr.
      it_ledger-ttable  = t800a-tab.
      it_ledger-atable  = t800a-ntable.
      it_ledger-otable  = t800a-objtable.
      it_ledger-bukrs   = t882-bukrs.
      it_ledger-slperiv = t882-periv.
      it_ledger-glperiv = t001-periv.
      it_ledger-comp    = space.
      it_ledger-bukz1   = t882-bukz.
      it_ledger-bukz2   = T886B-BUKZ.
      it_ledger-mbuch   = t886b-mbuch.
      it_ledger-glsip1  = t881-glsip.
      it_ledger-glsip2  = t882-glsip.
      it_ledger-glsip3  = t886b-glsip.
      it_ledger-activ   = t886b-activity.
      it_ledger-glactiv = ' '.
      it_ledger-feldmodif = t886b-feldmodif.
      APPEND it_ledger.
      wa_cust-NACTIV = 'X'.
    ENDSELECT.

* NON GL activities assigned to ledger
    SELECT  * FROM t886a
              WHERE  rldnr    = p_rldnr
                     AND   activity NOT IN rl_activ.

      READ TABLE it_ledger WITH KEY
               rldnr  = t881-rldnr
               bukrs  = t882-bukrs
               activ  = t886a-activity.

      IF sy-subrc > 0.
        it_ledger-rldnr   = t881-rldnr.
        it_ledger-ttable  = t800a-tab.
        it_ledger-atable  = t800a-ntable.
        it_ledger-otable  = t800a-objtable.
        it_ledger-bukrs   = t882-bukrs.
        it_ledger-slperiv = t882-periv.
        it_ledger-glperiv = t001-periv.
        it_ledger-comp    = space.
        it_ledger-bukz1   = t882-bukz.
        it_ledger-bukz2   = T886a-BUKZ.
        it_ledger-mbuch   = t886a-mbuch.
        it_ledger-glsip1  = t881-glsip.
        it_ledger-glsip2  = t882-glsip.
        it_ledger-glsip3  = t886a-glsip.
        it_ledger-activ   = t886a-activity.
        it_ledger-glactiv = ' '.
        it_ledger-feldmodif = t886a-feldmodif.
        APPEND it_ledger.
        wa_cust-NACTIV = 'X'.
      ENDIF.
    ENDSELECT.
  ENDIF.

  IF h_org_name = 'RCOMP'.
    SELECT SINGLE * FROM t882c WHERE rldnr = p_rldnr
                               AND   rcomp = p_rcomp .
* SELECT GL activities
    SELECT  * FROM t886c
                    WHERE      rldnr = p_rldnr
                      AND      rcomp = p_rcomp
                      AND   activity IN rl_activ.
      it_ledger-rldnr   = t881-rldnr.
      it_ledger-ttable  = t800a-tab.
      it_ledger-atable  = t800a-ntable.
      it_ledger-otable  = t800a-objtable.
      it_ledger-bukrs   = space.
      it_ledger-slperiv = t882c-periv.
      it_ledger-glperiv = space.
      it_ledger-comp    = t882c-rcomp.
      it_ledger-bukz1   = t882c-bukz.
      it_ledger-bukz2   = T886c-BUKZ.
      it_ledger-mbuch   = t886c-mbuch.
      it_ledger-glsip1  = t881-glsip.
      it_ledger-glsip2  = t882c-glsip.
      it_ledger-glsip3  = t886c-glsip.
      it_ledger-activ   = t886c-activity.
      it_ledger-glactiv = 'X'.
      it_ledger-feldmodif = t886c-feldmodif.
      APPEND it_ledger.
    ENDSELECT.

* SELECT GL activities assigned to ledger
    SELECT  * FROM t886a
              WHERE  rldnr    = p_rldnr
              AND    activity IN rl_activ.

      READ TABLE it_ledger WITH KEY
               rldnr  = t881-rldnr
               comp  = p_rcomp
               activ  = t886a-activity.

      IF sy-subrc > 0.
        it_ledger-rldnr   = t881-rldnr.
        it_ledger-ttable  = t800a-tab.
        it_ledger-atable  = t800a-ntable.
        it_ledger-otable  = t800a-objtable.
        it_ledger-bukrs   = SPACE.
        it_ledger-comp    = t882c-rcomp.
        it_ledger-slperiv = t882-periv.
        it_ledger-glperiv = t001-periv.
        it_ledger-bukz1   = t882-bukz.
        it_ledger-bukz2   = T886a-BUKZ.
        it_ledger-mbuch   = t886a-mbuch.
        it_ledger-glsip1  = t881-glsip.
        it_ledger-glsip2  = t882-glsip.
        it_ledger-glsip3  = t886a-glsip.
        it_ledger-activ   = t886a-activity.
        it_ledger-glactiv = 'X'.
        it_ledger-feldmodif = t886a-feldmodif.
        APPEND it_ledger.
      ENDIF.

    ENDSELECT.


* SELECT NON-GL-activities assigned to company
    SELECT  * FROM t886c
                    WHERE      rldnr = p_rldnr
                      AND      rcomp = p_rcomp
                      AND   activity NOT IN rl_activ.
      it_ledger-rldnr   = t881-rldnr.
      it_ledger-ttable  = t800a-tab.
      it_ledger-atable  = t800a-ntable.
      it_ledger-otable  = t800a-objtable.
      it_ledger-bukrs   = space.
      it_ledger-slperiv = t882c-periv.
      it_ledger-glperiv = space.
      it_ledger-comp    = t882c-rcomp.
      it_ledger-bukz1   = t882c-bukz.
      it_ledger-bukz2   = T886c-BUKZ.
      it_ledger-mbuch   = t886c-mbuch.
      it_ledger-glsip1  = t881-glsip.
      it_ledger-glsip2  = t882c-glsip.
      it_ledger-glsip3  = t886c-glsip.
      it_ledger-activ   = t886c-activity.
      it_ledger-glactiv = ' '.
      it_ledger-feldmodif = t886c-feldmodif.
      APPEND it_ledger.
    ENDSELECT.

* SELECT NON-GL activities assigned to ledger
    SELECT  * FROM t886a
              WHERE  rldnr    = p_rldnr
              AND   activity NOT IN rl_activ.

      READ TABLE it_ledger WITH KEY
               rldnr  = t881-rldnr
               comp  = p_rcomp
               activ  = t886a-activity.

      IF sy-subrc > 0.
        it_ledger-rldnr   = t881-rldnr.
        it_ledger-ttable  = t800a-tab.
        it_ledger-atable  = t800a-ntable.
        it_ledger-otable  = t800a-objtable.
        it_ledger-bukrs   = SPACE.
        it_ledger-comp    = t882c-rcomp.
        it_ledger-slperiv = t882-periv.
        it_ledger-glperiv = t001-periv.
        it_ledger-bukz1   = t882-bukz.
        it_ledger-bukz2   = T886a-BUKZ.
        it_ledger-mbuch   = t886a-mbuch.
        it_ledger-glsip1  = t881-glsip.
        it_ledger-glsip2  = t882-glsip.
        it_ledger-glsip3  = t886a-glsip.
        it_ledger-activ   = t886a-activity.
        it_ledger-glactiv = ' '.
        it_ledger-feldmodif = t886a-feldmodif.
        APPEND it_ledger.
      ENDIF.

    ENDSELECT.
  ENDIF.

*Check Fixed Field Movement (Note: 7626) and User Exits
  wa_cust-fixmd1 = space.
  wa_cust-fixmd1 = space.

  SELECT * FROM t800m
           WHERE    totable   = t800a-ntable
             AND    fromtable = 'ACCIT_GLX'.

    IF t800m-tofield = 'TSL'.
      IF t800m-fromfield NE 'PSWBT'.
        wa_cust-fixmd1 = 'X'.
      ENDIF.
    ENDIF.

    IF t800m-tofield = 'RTCUR'.
      IF t800m-fromfield NE 'PSWSL'.
        wa_cust-fixmd1 = 'X'.
      ENDIF.
    ENDIF.

    IF t800m-tofield = 'TSL'
    OR t800m-tofield = 'MSL'
    OR t800m-tofield = 'RTCUR'.
      IF t800m-exit(1) = 'U'.
        wa_cust-fixmd2 = 'X'.
      ENDIF.
    ENDIF.

    MOVE-CORRESPONDING t800m TO it_t800m.
    APPEND it_t800m.
  ENDSELECT.
* END: Check Fixed Field Movement (Note: 7626)


*Check variable Field Movement (Note: 7626) and User Exits
  LOOP AT it_ledger.
    SELECT * FROM t888m
             WHERE  FELDMODIF = it_ledger-feldmodif.

      IF  t888m-tofield = 'RACCT'
      AND t888m-fromtable = 'ACCIT_GLX'.
        IF NOT ( t888M-EXIT IS INITIAL ).
          wa_cust-URACCT = 'X'.
        ENDIF.
        IF t888m-fromfield ne 'HKONT'.
          wa_cust-UHKONT = 'X'.
        ENDIF.
      ENDIF.
      IF  t888m-fromtable = 'ACCIT_GLX'.
        MOVE-CORRESPONDING t888m TO it_t888m.
        COLLECT it_t888m.
      ENDIF.

    ENDSELECT.

  ENDLOOP.

* Final Cust. Analyses
  LOOP AT it_ledger.
* GL activities
    IF it_ledger-glactiv = 'X'.
* Ledger Selection ?
      IF  it_ledger-bukz1 ne '0'
      AND it_ledger-mbuch ne 'X'.
        wa_cust-Select = 'X'.
      ENDIF.
* Line Item Writing
      IF  it_ledger-glsip1 = ' '
       AND it_ledger-glsip2 = ' '
       AND it_ledger-glsip2 = ' '.
        wa_cust-NOLINE = 'X'.
      ENDIF.
    ENDIF.
  ENDLOOP.

* Check local Postings
  CLEAR   rl_activ.
  REFRESH rl_activ.
  MOVE: 'I'      TO rl_activ-sign,
        'EQ'     TO rl_activ-option,
        'RGL0' TO rl_activ-low.
  APPEND rl_activ.
  MOVE: 'I'      TO rl_activ-sign,
        'EQ'     TO rl_activ-option,
        'RGG0' TO rl_activ-low.
  APPEND rl_activ.
  MOVE: 'I'      TO rl_activ-sign,
        'EQ'     TO rl_activ-option,
        'GLU1' TO rl_activ-low.
  APPEND rl_activ.
  MOVE: 'I'      TO rl_activ-sign,
        'CP'     TO rl_activ-option,
        'Z*' TO rl_activ-low.
  APPEND rl_activ.

  IF h_org_name = 'RBUKRS'.
    SELECT  RCLNT FROM (T800a-NTABLE) UP TO 1 ROWS
             INTO ZCLNT
             WHERE RLDNR = p_rldnr
             AND   RVERS = p_rvers
             AND   RRCTY = p_rrcty
             AND   RBUKRS = p_rbukrs
             AND   RYEAR  = p_ryear
             AND   ACTIV IN rl_activ.
      IF SY-SUBRC = 0.
        wa_cust-LOCDOC = 'X'.
      ENDIF.
    ENDSELECT.
  ENDIF.

  IF T800a-ntable = 'GLS1'.
    SELECT * FROM GLS1 UP TO 1 ROWS
             WHERE RLDNR = p_rldnr
             AND   BUKRS = p_rbukrs
             AND   RVERS = p_rvers
             AND   RRCTY = p_rrcty
             AND   RYEAR  = p_ryear
             AND   ACTIV IN rl_activ.
      IF SY-SUBRC = 0.
        wa_cust-LOCDOC = 'X'.
      ENDIF.
    ENDSELECT.
  ENDIF.

  IF h_org_name = 'RCOMP'.
    SELECT  RCLNT FROM (T800a-NTABLE) UP TO 1 ROWS
             INTO ZCLNT
             WHERE RLDNR = p_rldnr
             AND   RVERS = p_rvers
             AND   RRCTY = p_rrcty
             AND   RCOMP = p_rcomp
             AND   RYEAR  = p_ryear
             AND   ACTIV IN rl_activ.
      IF SY-SUBRC = 0.
        wa_cust-LOCDOC = 'X'.
      ENDIF.
    ENDSELECT.
  ENDIF.

* Summary Status

  IF wa_cust IS INITIAL.
    CUST_ERR = ' '.
  ELSE.
    CUST_ERR = 'X'.
  ENDIF.

* Prepare Report
  REFRESH cust_out.
  CLEAR ZTEXT.

  IF wa_cust-NOLINE = 'X'.
    CLEAR ZTEXT.
    WRITE '- ' TO ZTEXT+3(3).
    WRITE ZCTEXT01 TO ZTEXT+3(47).
    append ZTEXT TO cust_out.
  ENDIF.

  IF wa_cust-fixmd1 = 'X'.
    CLEAR ZTEXT.
    WRITE '- ' TO ZTEXT+3(3).
    WRITE ZCTEXT02 TO ZTEXT+6(47).
    append ZTEXT TO cust_out.
  ENDIF.

  IF wa_cust-fixmd2 = 'X'.
    CLEAR ZTEXT.
    WRITE '- ' TO ZTEXT+3(3).
    WRITE ZCTEXT03 TO ZTEXT+6(47).
    append ZTEXT TO cust_out.
  ENDIF.

  IF wa_cust-Select = 'X'.
    CLEAR ZTEXT.
    WRITE '- ' TO ZTEXT+3(3).
    WRITE ZCTEXT04 TO ZTEXT+6(47).
    append ZTEXT TO cust_out.
  ENDIF.

  IF wa_cust-URACCT = 'X'.
    CLEAR ZTEXT.
    WRITE '- ' TO ZTEXT+3(3).
    WRITE ZCTEXT05 TO ZTEXT+6(47).
    append ZTEXT TO cust_out.
  ENDIF.

  IF wa_cust-UHKONT = 'X'.
    CLEAR ZTEXT.
    WRITE '- ' TO ZTEXT+3(3).
    WRITE ZCTEXT06 TO ZTEXT+6(47).
    append ZTEXT TO cust_out.
  ENDIF.

  IF wa_cust-GACTIV = 'X'.
    CLEAR ZTEXT.
    WRITE '- ' TO ZTEXT+3(3).
    WRITE ZCTEXT07 TO ZTEXT+6(47).
    append ZTEXT TO cust_out.
  ENDIF.

  IF wa_cust-NACTIV = 'X'.
    CLEAR ZTEXT.
    WRITE '- ' TO ZTEXT+3(3).
    WRITE ZCTEXT08 TO ZTEXT+6(47).
    append ZTEXT TO cust_out.
  ENDIF.

  IF wa_cust-LOCDOC = 'X'.
    CLEAR ZTEXT.
    WRITE '- ' TO ZTEXT+3(3).
    WRITE ZCTEXT09 TO ZTEXT+6(47).
    append ZTEXT TO cust_out.
  ENDIF.

  CLEAR ZTEXT.
  append ZTEXT TO cust_out.
  append ZTEXT TO cust_out.
  WRITE ZCTEXT10 TO ZTEXT.
  append ZTEXT TO cust_out.
  CLEAR ZTEXT.
  append ZTEXT TO cust_out.
  LOOP AT it_ledger.
    FI_fiscva = it_ledger-glperiv.
    SL_fiscva = it_ledger-slperiv.
    AT NEW rldnr.
      CLEAR ZTEXT.
      WRITE 'Ledger:' TO ZTEXT+1(30).
      WRITE p_rldnr TO ZTEXT+31(2).
      append ZTEXT TO cust_out.

      CLEAR ZTEXT.
      WRITE 'Comp/CCode:' TO ZTEXT+1(30).
      IF p_rcomp is INITIAL.
        WRITE  p_rbukrs TO ZTEXT+31(10).
      ELSE.
        WRITE  p_rcomp TO ZTEXT+31(10).
      ENDIF.
      append ZTEXT TO cust_out.

      CLEAR ZTEXT.
      WRITE  'Chart of Accnt in FI / FI-SL:' TO ZTEXT+1(30).
      WRITE  SL_fiscva TO ZTEXT+31(3) DECIMALS 0.
      WRITE  ' / ' TO ZTEXT+34(3).
      WRITE  FI_fiscva TO ZTEXT+37(3).
      append ZTEXT TO cust_out.

      CLEAR  ZTEXT.
      append ZTEXT TO cust_out.
      append ZTEXT TO cust_out.
      WRITE 'Blck   Line Item at Level   Acti  Post  FI-GL'  TO ZTEXT+3.
      WRITE 'Field   Ledger'   TO ZTEXT+52.
      append ZTEXT TO cust_out.
      WRITE 'Ind.  -RLDNR-CoCde-Activ-  vity  Ind   Actvty' TO ZTEXT+3.
      WRITE 'Movem  Selection'  TO ZTEXT+52.
      append ZTEXT TO cust_out.
      MOVE sy-uline to  ZTEXT.
      append ZTEXT TO cust_out.
    ENDAT.

    CLEAR ZTEXT.
    WRITE it_ledger-bukz1 TO ZTEXT+4(1).
    WRITE it_ledger-glsip1 TO ZTEXT+12(1).
    WRITE it_ledger-glsip2 TO ZTEXT+18(1).
    WRITE it_ledger-glsip3 TO ZTEXT+24(1).
    WRITE it_ledger-activ TO ZTEXT+31(4).
    WRITE it_ledger-bukz2 TO ZTEXT+38(4).
    WRITE it_ledger-glactiv TO ZTEXT+45(1).
    WRITE it_ledger-feldmodif TO ZTEXT+52(4).
    IF it_ledger-MBUCH IS INITIAL.
        WRITE 'Selection active' TO ZTEXT+59(16).
    ELSE.
      WRITE 'No Selection' TO ZTEXT+59(12).
    ENDIF.
    append ZTEXT TO cust_out.
  ENDLOOP.
  CLEAR ZTEXT.
  append ZTEXT TO cust_out.
  append ZTEXT TO cust_out.

  WRITE ZCTEXT11 TO ZTEXT.
  append ZTEXT TO cust_out.
  CLEAR ZTEXT.
  append ZTEXT TO cust_out.
  WRITE 'Table-From Table-To    Field-From   Field-To'  TO ZTEXT+3.
  WRITE 'User-Exit'  TO ZTEXT+49.
  append ZTEXT TO cust_out.
  MOVE sy-uline to ZTEXT.
  append ZTEXT TO cust_out.

  LOOP AT it_t800m.
    CLEAR ZTEXT.
    WRITE it_t800m-FROMTABLE TO ZTEXT+4(10).
    WRITE it_t800m-TOTABLE TO ZTEXT+15(10).
    WRITE it_t800m-FROMFIELD TO ZTEXT+26(10).
    WRITE it_t800m-TOFIELD TO ZTEXT+40(10).
    WRITE it_t800m-EXIT TO ZTEXT+52(10).
    append ZTEXT TO cust_out.
  ENDLOOP.


  CLEAR ZTEXT.
  append ZTEXT TO cust_out.
  append ZTEXT TO cust_out.

  WRITE ZCTEXT12 TO ZTEXT.
  append ZTEXT TO cust_out.
  CLEAR ZTEXT.
  append ZTEXT TO cust_out.
  WRITE 'Fld Movmt   Table-From Field-From   Field-To'  TO ZTEXT+3.
  WRITE 'User-Exit'  TO ZTEXT+49.
  append ZTEXT TO cust_out.
  MOVE sy-uline to ZTEXT.
  append ZTEXT TO cust_out.
  CLEAR ZTEXT.

  LOOP AT it_t888m.
    CLEAR ZTEXT.
    WRITE it_t888m-FELDMODIF TO ZTEXT+4(10).
    WRITE it_t888m-FROMTABLE TO ZTEXT+15(10).
    WRITE it_t888m-FROMFIELD TO ZTEXT+26(10).
    WRITE it_t888m-TOFIELD TO ZTEXT+40(10).
    WRITE it_t888m-EXIT TO ZTEXT+52(10).
    append ZTEXT TO cust_out.
  ENDLOOP.

ENDFORM.                    "CHECK_CUST1

*---------------------------------------------------------------------*
*       FORM err_output3                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM err_output3.
ENDFORM.                    "err_output3

*---------------------------------------------------------------------*
*       FORM DUB_CREATE                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  ZT800A                                                        *
*  -->  IT_CLNT1                                                      *
*  -->  Z_MSG                                                         *
*---------------------------------------------------------------------*
FORM DUB_CREATE USING ZT800A LIKE T800A
                CHANGING IT_CLNT1 LIKE IT_CLNT0[]
                         Z_MSG LIKE Z_MSG1.

  DATA: DB_FLDS LIKE DBFIELD OCCURS 0 WITH HEADER LINE.

  REFRESH CODE.
  CLEAR CODE.

* DATA DECLARATION
* Create Coding

  PERFORM APPEND_CODE USING  0 0 'report ZZSLDUBKEY.' SPACE.
  PERFORM APPEND_CODE USING  0 0 SPACE SPACE.

PERFORM APPEND_CODE USING  0 0 'data: begin of it_clnt occurs 5,' SPACE.
  PERFORM APPEND_CODE USING  0 0 '        issue(6) type c,' SPACE.
 PERFORM APPEND_CODE USING  0 0 '        client like t001-mandt,' SPACE.
  PERFORM APPEND_CODE USING  0 0 '        count1 like sy-tabix,' SPACE.
  PERFORM APPEND_CODE USING  0 0 '     end of it_clnt.' SPACE.
  PERFORM APPEND_CODE USING  0 0 SPACE SPACE.
  WORK = 'form dupkeychk changing it_client like  it_clnt[].'.
  PERFORM APPEND_CODE USING  0 0 WORK SPACE.

  MOVE 'TABLES:  $, $.'   TO WORK.
  REPLACE '$' INTO WORK WITH ZT800A-OBJTABLE.
  REPLACE '$' INTO WORK WITH ZT800A-TAB.
  CONDENSE WORK.
  PERFORM APPEND_CODE USING  0 0 WORK SPACE.
  IF NOT ZT800A-OBJTABLE2 IS INITIAL.
    MOVE 'TABLES: $.' TO WORK.
    REPLACE '$' INTO WORK WITH ZT800A-OBJTABLE2.
    CONDENSE WORK.
    PERFORM APPEND_CODE USING  0 0 WORK SPACE.
  ENDIF.
 PERFORM APPEND_CODE USING  0 0 'data: wa_clnt like it_clnt.' SPACE.
 PERFORM APPEND_CODE USING  0 0 'data: help_tabix like sy-tabix.' SPACE.


* Coding for object table 1
obj_table = ZT800A-OBJTABLE.
PERFORM CRT_OBJ_CODE USING obj_table
                           ZT800A
                           '1'
               CHANGING    Z_MSG.

* Coding for object table 2
IF NOT ZT800A-OBJTABLE2 IS INITIAL.
  obj_table = ZT800A-OBJTABLE2.
  PERFORM CRT_OBJ_CODE USING obj_table
                         ZT800A
                         '2'
             CHANGING    Z_MSG.
ENDIF.


* Search NULL Values in Summary table

  REFRESH DB_FLDS.
  CLEAR DB_FLDS.

  CALL FUNCTION 'DB_GET_TABLE_FIELDS'
       EXPORTING
            FIELD_INFO = 'A'
            TABNAME    = ZT800A-TAB
       TABLES
            DBFIELDS   = DB_FLDS
       EXCEPTIONS
            OTHERS     = 1.

  LOOP AT DB_FLDS.
    IF  DB_FLDS-TYPE     EQ 'NUMBER'
     OR DB_FLDS-KEYFLAG  EQ 'X'
     OR DB_FLDS-NAME  EQ 'CSPRED'
     OR DB_FLDS-NAME  EQ 'QSPRED'.
      DELETE DB_FLDS.
    ENDIF.
  ENDLOOP.

  PERFORM APPEND_CODE USING  0 0 SPACE SPACE.

  WORK = 'select * from $ client specified where'.
  REPLACE '$' INTO WORK WITH ZT800A-TAB.
  CONDENSE WORK.
  PERFORM APPEND_CODE USING  0 4 WORK SPACE.
  DESCRIBE TABLE DB_FLDS LINES zlines.
  LOOP AT DB_FLDS.
    WORK = '$  is null $'.
    REPLACE '$' INTO WORK WITH DB_FLDS-NAME.
    IF SY-TABIX < zlines..
      REPLACE '$' INTO WORK WITH 'OR'.
    ELSE.
      REPLACE '$' INTO WORK WITH '.'.
    ENDIF.
    CONDENSE WORK.
    PERFORM APPEND_CODE USING  0 0 WORK SPACE.
  ENDLOOP.
  PERFORM APPEND_CODE USING -2 0 SPACE SPACE.

  PERFORM APPEND_CODE USING  0 0 'wa_clnt-issue = ''NULL''.' SPACE.
  WORK = 'wa_clnt-client &=& $-RCLNT.'.
  REPLACE '$' INTO WORK WITH ZT800A-TAB.
  CONDENSE WORK NO-GAPS.
  REPLACE '&' INTO WORK WITH ' '.
  REPLACE '&' INTO WORK WITH ' '.
  PERFORM APPEND_CODE USING  0 0 WORK SPACE.

  PERFORM APPEND_CODE USING  0 0 'wa_clnt-count1 = 1.' SPACE.
 PERFORM APPEND_CODE USING  0 0 'collect wa_clnt into it_client.' SPACE.
  PERFORM APPEND_CODE USING -2 0 'endselect.' SPACE.
  PERFORM APPEND_CODE USING  0 0 'endform.' SPACE.

*Insert the generated report
  GENERATE SUBROUTINE POOL CODE NAME Z_PROG_NAME MESSAGE ZMSG02 .
  IF SY-SUBRC NE 0.
    MESSAGE I016 WITH 'Check DubKey will be skipped'.
    EXIT.
  ENDIF.
  COMMIT WORK.
  PERFORM DUPKEYCHK  IN PROGRAM (Z_PROG_NAME) CHANGING IT_CLNT1[].

ENDFORM.                    "DUB_CREATE




*---------------------------------------------------------------------*
*       FORM CRT_OBJ_CODE                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  OBJ_TABLE                                                     *
*  -->  ZT800A                                                        *
*  -->  Z_MSG                                                         *
*---------------------------------------------------------------------*
FORM CRT_OBJ_CODE USING  obj_table
                         ZT800A LIKE T800A
                         obj_nr LIKE Z_LI_EXIST
               CHANGING  Z_MSG LIKE Z_MSG1.

  DATA: DB_FLDS LIKE DBFIELD OCCURS 0 WITH HEADER LINE.
  DATA: IDX_FLDS LIKE DBFLDNAM OCCURS 0 WITH HEADER LINE.
  DATA: z_indtab LIKE DD12L-DBINDEX.
  DATA: zlines LIKE sy-tabix.

  WORK = 'data: begin of good_$ occurs 0.'.
  REPLACE '$' INTO WORK WITH OBJ_TABLE.
  CONDENSE WORK.
  PERFORM APPEND_CODE USING  0 6 WORK SPACE.
  WORK = 'include structure $.'.
  REPLACE '$' INTO WORK WITH OBJ_TABLE.
  CONDENSE WORK.
  PERFORM APPEND_CODE USING  2 0 WORK SPACE.

  PERFORM APPEND_CODE USING  0 0 'data: processed_r type c,' SPACE.
  PERFORM APPEND_CODE USING  0 0 'processed_s type c,' SPACE.
  WORK =  'end of good_$.'.
  REPLACE '$' INTO WORK WITH OBJ_TABLE.
  CONDENSE WORK.
  PERFORM APPEND_CODE USING  -2 0 WORK SPACE.
  PERFORM APPEND_CODE USING  -6 0 SPACE SPACE.

  WORK = 'data: delete_$ like $ occurs 0 with header line.'.
  REPLACE '$' INTO WORK WITH OBJ_TABLE.
  REPLACE '$' INTO WORK WITH OBJ_TABLE.
  CONDENSE WORK.
  PERFORM APPEND_CODE USING  0 0 WORK SPACE.
  PERFORM APPEND_CODE USING  0 0 SPACE SPACE.

* Find Unique Fields
  CALL FUNCTION 'DB_GET_TABLE_FIELDS'
       EXPORTING
            FIELD_INFO = 'A'
            TABNAME    = OBJ_TABLE
       TABLES
            DBFIELDS   = DB_FLDS
       EXCEPTIONS
            OTHERS     = 1.

  LOOP AT DB_FLDS.
    IF  DB_FLDS-NAME    = 'DATIV'
     OR DB_FLDS-NAME    = 'DATIB'
     OR DB_FLDS-NAME    = 'DATPV'
     OR DB_FLDS-NAME    = 'DATPB'
     OR DB_FLDS-NAME    = 'OBJNR'.
      DELETE DB_FLDS.
    ENDIF.
  ENDLOOP.
  SORT DB_FLDS BY NAME.

* Check Fromat of Index table

CALL FUNCTION 'DD_INDEX_NAME'
  EXPORTING
    INDEXNAME       = '1  '
    TABNAME         = OBJ_TABLE
  IMPORTING
    DBINDEX         = z_indtab.


* Get Unique Key From DB
* Get field of DB Index Table
  CALL FUNCTION 'DB_GET_INDEX_FIELDS'
       EXPORTING
            DBINDEX    = z_indtab
       TABLES
            DBFLDNAMES = IDX_FLDS
       EXCEPTIONS
            OTHERS     = 1.

  SORT IDX_FLDS BY NAME.
* Check Index table
  CLEAR LD_MSG.
  LOOP AT DB_FLDS.
    READ TABLE IDX_FLDS INDEX SY-TABIX.
    IF DB_FLDS-NAME NE IDX_FLDS-NAME.
      Z_MSG = 'Error: Please check unique Index of table $'.
      REPLACE '$' INTO Z_MSG WITH z_indtab.
      CONDENSE Z_MSG.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF NOT LD_MSG IS INITIAL.
    EXIT.
  ENDIF.

  SORT DB_FLDS BY COLUMN ASCENDING.

* Begin of Processing

  WORK = 'select * from $ client specified order by mandt objnr.'.
  REPLACE '$' INTO WORK WITH OBJ_TABLE.
  PERFORM APPEND_CODE USING  0 0 WORK SPACE.
  WORK = 'read table good_$ with key'.
  REPLACE '$' INTO WORK WITH OBJ_TABLE.
  CONDENSE WORK.
  PERFORM APPEND_CODE USING  1 2 WORK SPACE.

  LOOP AT DB_FLDS.
    WORK = '$ &=&   $-$'.
    REPLACE '$' INTO WORK WITH DB_FLDS-NAME.
    REPLACE '$' INTO WORK WITH OBJ_TABLE.
    REPLACE '$' INTO WORK WITH DB_FLDS-NAME.
    CONDENSE WORK NO-GAPS.
    REPLACE '&' INTO WORK WITH ' '.
    REPLACE '&' INTO WORK WITH ' '.
    PERFORM APPEND_CODE USING  0 0 WORK SPACE.
  ENDLOOP.
  PERFORM APPEND_CODE USING  0 0 'binary search .' SPACE.
  PERFORM APPEND_CODE USING -2 0 SPACE SPACE.
  PERFORM APPEND_CODE USING  0 2 'if sy-subrc > 0.' SPACE.

  WORK = 'insert $ into good_$ index sy-tabix.'.
  REPLACE '$' INTO WORK WITH OBJ_TABLE.
  REPLACE '$' INTO WORK WITH OBJ_TABLE.
  CONDENSE WORK.
  PERFORM APPEND_CODE USING  0 -2 WORK SPACE.
  PERFORM APPEND_CODE USING  0 2 'else.' SPACE.
  PERFORM APPEND_CODE USING  0 0 'help_tabix = sy-tabix.' SPACE.
  WORK = 'append $ to delete_$.'.
  REPLACE '$' INTO WORK WITH OBJ_TABLE.
  REPLACE '$' INTO WORK WITH OBJ_TABLE.
  CONDENSE WORK.
  PERFORM APPEND_CODE USING  0 0 WORK SPACE.

  IF obj_nr = '1'.
    PERFORM APPEND_CODE USING  0 0 'wa_clnt-issue = ''DUPKY1''.' SPACE.
  ENDIF.

  IF obj_nr = '2'.
    PERFORM APPEND_CODE USING  0 0 'wa_clnt-issue = ''DUPKY2''.' SPACE.
  ENDIF.


  WORK = 'wa_clnt-client &=& $-mandt.'.
  REPLACE '$' INTO WORK WITH OBJ_TABLE.
  CONDENSE WORK NO-GAPS.
  REPLACE '&' INTO WORK WITH ' '.
  REPLACE '&' INTO WORK WITH ' '.
  PERFORM APPEND_CODE USING  0 0 WORK SPACE.
  PERFORM APPEND_CODE USING  0 0 'wa_clnt-count1 = 1.' SPACE.
 PERFORM APPEND_CODE USING  0 0 'collect wa_clnt into it_client.' SPACE.
  PERFORM APPEND_CODE USING -2 0 'endif.' SPACE.
  PERFORM APPEND_CODE USING -1 0 'endselect.' SPACE.

  WORK = 'FREE GOOD_$ .'.
  REPLACE '$' INTO WORK WITH OBJ_TABLE.
  CONDENSE WORK.
  PERFORM APPEND_CODE USING  0 0 WORK SPACE.

  PERFORM APPEND_CODE USING  0 0 SPACE SPACE.

ENDFORM.





*---------------------------------------------------------------------*
*       FORM check_non_fi                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE(L_ORG_NAME)                                             *
*  -->  LT_ERR_TAB                                                    *
*---------------------------------------------------------------------*
FORM check_non_fi     USING    value(l_org_name) LIKE h_org_name
                      CHANGING lt_err_tab1 LIKE it_nondoc1[]
                               lt_err_tab2 LIKE it_nondoc2[].


  DATA: wa_nondoc1 TYPE tt_err5.
  DATA: wa_nondoc2 TYPE tt_err6.

  RANGES  rl_activ FOR t022t-activity.

*FILL GL activities selection

  SELECT * FROM t022 WHERE tab_1 = 'ACCHD'
                     AND   notused = ' '.
    MOVE: 'E'      TO rl_activ-sign,
          'EQ'     TO rl_activ-option,
           t022-activity TO rl_activ-low.
    APPEND rl_activ.
  ENDSELECT.

  IF h_org_name = 'RBUKRS'.
    SELECT *  FROM (T800a-NTABLE)
      INTO corresponding fields of wa_nondoc1
             WHERE RLDNR = p_rldnr
             AND   RVERS = p_rvers
             AND   RRCTY = p_rrcty
             AND   RBUKRS = p_rbukrs
             AND   RYEAR  = p_ryear
             AND   ACTIV IN rl_activ.

      IF SY-SUBRC = 0.
        COLLECT wa_nondoc1 INTO lt_err_tab1.
      ENDIF.
    ENDSELECT.
  ENDIF.

  IF h_org_name = 'BUKRS'.
    SELECT *  FROM (T800a-NTABLE)
      INTO corresponding fields of wa_nondoc1
             WHERE RLDNR = p_rldnr
             AND   RVERS = p_rvers
             AND   RRCTY = p_rrcty
             AND   BUKRS = p_rbukrs
             AND   RYEAR  = p_ryear
             AND   ACTIV IN rl_activ.

      IF SY-SUBRC = 0.
        COLLECT wa_nondoc1 INTO lt_err_tab1.
      ENDIF.
    ENDSELECT.
  ENDIF.



* global ledger
  IF h_org_name = 'RCOMP'.
    SELECT *  FROM (T800a-NTABLE)
      INTO corresponding fields of wa_nondoc1
             WHERE RLDNR = p_rldnr
             AND   RVERS = p_rvers
             AND   RRCTY = p_rrcty
             AND   RCOMP = p_rcomp
             AND   RYEAR  = p_ryear
             AND   ACTIV IN rl_activ.

      IF SY-SUBRC = 0.
        COLLECT wa_nondoc1 INTO lt_err_tab1.
      ENDIF.
    ENDSELECT.
  ENDIF.

  SORT lt_err_tab1 BY ACTIV DOCNR.

  LOOP AT lt_err_tab1 INTO wa_nondoc1.
    MOVE-CORRESPONDING wa_nondoc1 TO wa_nondoc2.
    wa_nondoc2-COUNT1 = 1.
    COLLECT wa_nondoc2 INTO lt_err_tab2.
  ENDLOOP.

ENDFORM.                    "check_non_fi


*---------------------------------------------------------------------*
*       FORM check_lineItm                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  LI_EXIST                                                      *
*---------------------------------------------------------------------*
FORM check_lineItm changing LI_EXIST LIKE Z_LI_EXIST.
  DATA: ZCLNT LIKE GLU1-RCLNT.

  CLEAR LI_EXIST.
  IF h_org_name = 'RBUKRS'.
    SELECT  RCLNT FROM (T800a-NTABLE) UP TO 1 ROWS
             INTO ZCLNT
             WHERE RLDNR = p_rldnr
             AND   RVERS = p_rvers
             AND   RRCTY = p_rrcty
             AND   RBUKRS = p_rbukrs
             AND   RYEAR  = p_ryear.

      IF SY-SUBRC = 0.
        LI_EXIST = 'X'.
      ENDIF.
    ENDSELECT.
  ENDIF.

  IF T800a-ntable = 'GLS1'.
    SELECT * FROM GLS1 UP TO 1 ROWS
             WHERE RLDNR = p_rldnr
             AND   BUKRS = p_rbukrs
             AND   RVERS = p_rvers
             AND   RRCTY = p_rrcty
             AND   RYEAR  = p_ryear.
      IF SY-SUBRC = 0.
        LI_EXIST = 'X'.
      ENDIF.
    ENDSELECT.
  ENDIF.

  IF h_org_name = 'RCOMP'.
    SELECT  RCLNT FROM (T800a-NTABLE) UP TO 1 ROWS
             INTO ZCLNT
             WHERE RLDNR = p_rldnr
             AND   RVERS = p_rvers
             AND   RRCTY = p_rrcty
             AND   RCOMP = p_rcomp
             AND   RYEAR  = p_ryear.
      IF SY-SUBRC = 0.
        LI_EXIST = 'X'.
      ENDIF.
    ENDSELECT.
  ENDIF.

ENDFORM.                    "check_lineItm
