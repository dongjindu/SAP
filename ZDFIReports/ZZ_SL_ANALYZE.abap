************************************************************************
*                                                                      *
*  Report:          ZZ_SL_ANALYZE   V5.0                               *
*                                                                      *
************************************************************************
*                                                                      *
* Created by     :  OBERDORF                                           *
* Date/Time      :  10/04/2004  (dd/mm/yy)                             *
* Last Changed   :  19/07/2006                                         *
* Valid releases :  SAP_APPL  46b, 46c, 470, 50(ERP2004), 60(ERP2005)  *
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
*   7) Check consistency of GLIDXA                                     *
*      Only necessery when line item writing is activated              *
*                                                                      *
*                                                                      *
*  8) Number range object (GL_RECID/GL_BUCHEN).                        *
*     This check compares the number range object (GL_RECID /          *
*     GL_BUCHEN)with the largest record number (field GL_SIRID)        *
*     and the largest document number in the FI-SL line item table.    *
*     An inconsistency causes SAPSQL_ARRAY_INSERT_DUPREC dumps.        *
*     For detailed information please have a look at note 62150.       *
*                                                                      *
*                                                                      *
*  9) Check SL Table Directory (T800A)                                 *
*     The FI-SL Table direcory contains the basic table assignments    *
*     in Special Purpose Ledger.                                       *
*     Usually those customizing settings are correct because they are  *
*     done automatically while creating and installing the table group *
*     using transaction GCIN.                                          *
*                                                                      *
*     However, in transaction GCI3 it is possible to change the        *
*     settings, which is not recommended.                              *
*     Inconsistent entries in the FI-SL Table Directory lead to        *
*     unpredictable system behaviour.                                  *
*                                                                      *
* 10) Master Data Validation Check                                     *
*     This check tests all entries in table T800D. The customizing     *
*     transaction is GCS1 and checks:                                  *
*                                                                      *
*     a) If the entries exist (yellow rating)                          *
*        In case the field is not used for allocation or planning      *
*        then missing entries are not relevant and do usually          *
*        not cause problems.                                           *
*                                                                      *
*     b) If master data can be read                                    *
*        In case entries exist but no master data can be read then     *
*        this might have 2 reasons:                                    *
*                                                                      *
*        - Master Data Table contains no data                          *
*        - the user exit which is assigned in table T800D is           *
*          not correct installed. Then please check the user exit.     *
*                                                                      *
*     If you activate the flag "Check Cmp/Code from Selection" then    *
*     the master-data-check verifies if master data exist in the       *
*     specific company code or global company which is specified       *
*     in the selection block. Else the program searches for master     *
*     data in all assigned company codes or global companies.          *
*                                                                      *
* 11) Additional Field and table Check                                 *
*     This check analyzes the customizing of the variable field        *
*     movement and the entries the master data validation.             *
*     It checks if the customized tables and fields really exist       *
*     in the Repository.                                               *
*                                                                      *
*                                                                      *
************************************************************************

REPORT zz_sl_analyze01 MESSAGE-ID gu LINE-SIZE 120.

CONSTANTS: zpglen TYPE i VALUE 95,
           c_orglen TYPE DDLENG VALUE 6.

*Types
TYPE-POOLS: gusl,                      "Selection processor
            kkblo.

INCLUDE <ICON>.    " Type Pool in as 46c "ICON".

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
DATA:   pgm_vers(12) TYPE c.
DATA:   h_org_name(6).
DATA:   Z_LI_EXIST(1) VALUE ' '.
DATA:   Z_T8AMSG(200) TYPE c.
DATA:   wa_t001 LIKE T001.
DATA:   G_LENGTH_GLU1 TYPE i.
DATA:   G_LENGTH_STAB TYPE i.
DATA:   g_fmod_name TYPE RS38L_FNAM.
DATA:   gs_tfdir LIKE TFDIR.

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

DATA: err_chck1  TYPE boolean VALUE 'T'.
DATA: err_chck2  TYPE boolean VALUE 'T'.
DATA: err_chck3  TYPE boolean VALUE 'T'.
DATA: err_chck4  TYPE boolean VALUE 'T'.
DATA: err_chck5  TYPE boolean VALUE 'T'.
DATA: err_chck6  TYPE boolean VALUE 'T'.
DATA: err_chck7  TYPE boolean VALUE 'T'.
DATA: err_chck8  TYPE boolean VALUE 'T'.
DATA: err_chck9  TYPE boolean VALUE 'T'.
DATA: err_chck10 TYPE boolean VALUE 'T'.
DATA: err_chck11 TYPE boolean VALUE 'T'.


DATA: BEGIN OF g_chkerr OCCURS 0,
        checkno(03) TYPE c,
        pos         TYPE i,
        rating(1)   TYPE c,
        KEY1(10)    TYPE c,
        MSGID       LIKE SY-MSGTY,
        NUMBER      LIKE SY-MSGNO,
        TEXT(80)   TYPE c,
      END OF g_chkerr.


DATA: gd_unicode(1) TYPE C.

* Runtime ANALYSES
DATA: BEGIN OF GD_RUNTIME,
  EVENT      TYPE FIELDNAME,
  START_TIME LIKE SY-TIMLO,
  START_DATE LIKE SY-DATLO,
  END_TIME   LIKE SY-TIMLO,
  END_DATE   LIKE SY-DATLO,
  DURATION   LIKE SY-TABIX,
 END OF GD_RUNTIME.


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
      zctext12(50),
      zctext13(50).

* for Check 4
DATA: it_missdoc TYPE tt_err4 OCCURS 10.
DATA: it_nondoc1 TYPE tt_err5 OCCURS 10.
DATA: it_nondoc2 TYPE tt_err6 OCCURS 10.

DATA: ZTEXT2(90) TYPE c.
DATA: wa_missdoc TYPE tt_err4.

* for Check 11
DATA: BEGIN OF it_glidxa OCCURS 10.
DATA:   notin_glidxa(1)  TYPE c,
        notin_bkpf(1)    TYPE c,
        notin_litable(1) TYPE c.
        INCLUDE STRUCTURE GLIDXA.
DATA: END OF it_glidxa.

DATA: wa_glidxa LIKE it_glidxa.


* for Check 5
DATA: Z_MSG1 LIKE WORK.
DATA: LD_MSG LIKE WORK.
DATA: obj_table LIKE T800A-OBJTABLE.


DATA: BEGIN OF IT_CLNT0 OCCURS 5,
        ISSUE(6) TYPE C,
        CLIENT LIKE T001-MANDT,
        COUNT1 LIKE SY-TABIX,
     END OF IT_CLNT0.


* for Check 7
DATA: begin of wa_norange,
        error(1)  TYPE c,
        docnr_dbs LIKE GLU1-DOCNR,
        docnr_obj LIKE GLU1-DOCNR,
        recid_dbs LIKE GLFUNCA-GL_SIRID,
        recid_obj LIKE GLFUNCA-GL_SIRID,
      end of wa_norange.


* for Check 9
data: gs_800a like t800a,
      gs_881 like t881,
      gt_800D like t800d occurs 0 with header line,
      gt_fieldtab like dfies occurs 0 with header line,
      gt_org_info like glx_org_info occurs 0 with header line.

data: begin of gt_t800d_msg occurs 0,
        line(72),
        type,
      end of gt_t800d_msg.

data: gf_orgunit_min(c_orglen),
      gf_orgunit_max(c_orglen),
      gf_value_from like rgumd-value,
      gf_value_to like rgumd-value,
      gf_work(72),
      gf_record_key like rgumd-record_key,
      gf_stext(30),
      gf_ltext(3).

field-symbols: <gf_tab>, <gf_field>, <tabfield>.

data g_date like sy-datum.


* for Check 10
DATA: begin of gt_fldchk_msg occurs 0,
        type(1),
        issue1(4)  TYPE c,
        issue2(20) TYPE c,
        custab     LIKE T800a-tab,
        client     TYPE t000-mandt,
        sltab      LIKE T800a-tab,
        field      LIKE DD03D-FIELDNAME,
        FELDMODIF  LIKE T888M-FELDMODIF,
        tcode(4)      TYPE c,
        line(100)  TYPE c,
        component(6) TYPE c,
      end of gt_fldchk_msg.

* Message Texts
DATA: zttext00(zpglen) TYPE c.
DATA: zttext01(zpglen) TYPE c.
DATA: zttext02(zpglen) TYPE c.
DATA: zttext03(zpglen) TYPE c.
DATA: zttext04(zpglen) TYPE c.
DATA: zttext05(zpglen) TYPE c.
DATA: zttext06(zpglen) TYPE c.
DATA: zttext07(zpglen) TYPE c.
DATA: zttext08(zpglen) TYPE c.
DATA: zttext09(zpglen) TYPE c.
DATA: zttext10(zpglen) TYPE c.
DATA: zttext11(zpglen) TYPE c.
DATA: zttext12(zpglen) TYPE c.


*--------------------------------------------------


* Selection screen definition
SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE t0.

* Test Run
SELECTION-SCREEN BEGIN OF BLOCK b WITH FRAME TITLE t1.

* Check 08: Inconsistency of FI-SL Table Directory
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(30) zatext08 FOR FIELD p_chck08.
SELECTION-SCREEN POSITION 40.
PARAMETERS p_chck08 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF LINE.

* Check 01: Object Table Inconsist
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(30) zatext01 FOR FIELD p_chck01.
SELECTION-SCREEN POSITION 40.
PARAMETERS p_chck01 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF LINE.

* Check 02: Inconsist. Sum and Line It.Table
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(30) zatext02 FOR FIELD p_chck02.
SELECTION-SCREEN POSITION 40.
PARAMETERS p_chck02 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF LINE.

* Check 05: Double Key/NULL Values
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(30) zatext05 FOR FIELD p_chck05.
SELECTION-SCREEN POSITION 40.
PARAMETERS p_chck05 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF LINE.

* Check 03: Customizing
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(30) zatext03 FOR FIELD p_chck03.
SELECTION-SCREEN POSITION 40.
PARAMETERS p_chck03 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF LINE.

* Check 04: Missing Documents
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(30) zatext04 FOR FIELD p_chck04.
SELECTION-SCREEN POSITION 40.
PARAMETERS p_chck04 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF LINE.

* Check 06: Additional Non-FI Documents
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(30) zatext06 FOR FIELD p_chck06.
SELECTION-SCREEN POSITION 40.
PARAMETERS p_chck06 AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN END OF LINE.

* Check 11: Consistency of Index GLIDXA
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(30) zatext11 FOR FIELD p_chck11.
SELECTION-SCREEN POSITION 40.
PARAMETERS p_chck11 AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN END OF LINE.


* Check 07: No.range object (GL_RECID/GL_BUCHEN)
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(30) zatext07 FOR FIELD p_chck07.
SELECTION-SCREEN POSITION 40.
PARAMETERS p_chck07 AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN END OF LINE.

* Check 09: Master Data Validation Check
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(30) zatext09 FOR FIELD p_chck09.
SELECTION-SCREEN POSITION 40.
PARAMETERS p_chck09 AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN END OF LINE.

* Check 10: Fields and tables Check
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(30) zatext10 FOR FIELD p_chck10.
SELECTION-SCREEN POSITION 40.
PARAMETERS p_chck10 AS CHECKBOX DEFAULT ' '.
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
PARAMETERS p_ryear LIKE glu1-ryear MEMORY ID GJR.
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


* Block f: extended Master Data Validation
SELECTION-SCREEN BEGIN OF BLOCK f WITH FRAME TITLE t5.

* Period / Date for check
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20) zbtext15 FOR FIELD p_mdate.
SELECTION-SCREEN POSITION POS_LOW.
PARAMETERS p_mdate LIKE sy-datum.
SELECTION-SCREEN END OF LINE.

* Check company code from Selection Screen
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(20) zbtext16 FOR FIELD p_T8dbuk.
SELECTION-SCREEN POSITION POS_LOW.
PARAMETERS p_T8dbuk as CHECKBOX default ' '.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK f.

SELECTION-SCREEN END OF BLOCK a.


******************* INITIALIZATION ***********************

INITIALIZATION.

* Version
  pgm_vers = 'Version 5.0'.

* Date for Selection Screen: Extended Master Data Check
  p_mdate = SYST-DATUM.
* Selection Texts
  t0 = 'Analyzing Special Ledger Settings and Data'.
  t1 = 'Processing Check'.
  t3 = 'Selection'.
  t4 = 'Extended Selection for Missing Doc check'.
  t5 = 'Extended Selection for Master Data check'.

  CONCATENATE t0 '-' pgm_vers INTO t0 SEPARATED BY ' '.

  zatext01 = 'Object Table Inconsist.'(a01).
  zatext02 = 'Inconsist. Sum and Line It.Table'(a02).
  zatext03 = 'Customizing'(a03).
  zatext04 = 'Missing Documents'(a04).
  zatext05 = 'Double Key/NULL Values'(A05).
  zatext06 = 'Additional Non-FI Documents'(A06).
  zatext07 = 'No.range object (GL_RECID/GL_BUCHEN)'(A07).
  zatext08 = 'Inconsistency of FI-SL Table Directory'(A08).
  zatext09 = 'Master Data Validation Check'(A09).
  zatext10 = 'Additional Field and table Check'(A10).
  zatext11 = 'Consistency of Index GLIDXA'(A11).

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
  zbtext15 = 'Valid for Date'(b16).
  zbtext16 = 'Check Cmp/Code from Selection'(b16).


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
  zctext13 = 'Missing Index for Online Split (note 720909):'(c13).


* Message Texts
  zttext00 = 'Field &: No entry in table T800D'(t00).
  zttext01 = 'Field &: Only BUKRS,RCOMP,KOKRS as & can be checked'(t01).
  zttext02 = 'Field &: RFIELD3 to RFIELD5 cannot be checked'(t02).
  zttext03 = 'Field &: G_READ_TEXT does not work correct'(t03).
  zttext04 = 'Field &: G_READ_VALUE does not work correct'(t04).
  zttext05 = 'Field &: G_READ_AREA does not work correct'(t05).
  zttext06 = 'Ledger: '(t06).
  zttext07 = 'Table: '(t07).
  zttext08 = 'Validated Fields:'(t08).
  zttext09 = 'Error Messages:'(t09).
  zttext10 = 'Field &: All Master Data Exits work correct'(t10).
  zttext11 = 'No Master Data found'(t11).
  zttext12 = 'Performance: Long proc.time. Check exit for Field &'(t12).

* Check if Unicode System
  g_fmod_name = 'SLDAG_CHECK_FOR_UNICODE'.
  select single * from tfdir INTO gs_tfdir
                  where funcname = g_fmod_name.
  IF SY-SUBRC = 0.
    CALL FUNCTION g_fmod_name
      IMPORTING
        UNICODE = gd_unicode.
    IF gd_unicode > 0.
      gd_unicode = 'X'.
    ELSE.
      CLEAR gd_unicode.
    ENDIF.
  ELSE.
    gd_unicode = '*'.
  ENDIF.

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

    IF   ( t800a-objtable IS INITIAL )
     AND ( p_chck07 = 'X' ).
      p_chck07 = ' '.
      MESSAGE e016 WITH 'No Range check not possible for table'
      t800a-NTABLE.
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
  IF p_chck09 = 'X' AND
     p_T8dbuk = 'X'.
    IF ( p_rbukrs IS INITIAL ) AND ( p_rcomp IS INITIAL ) .
      MESSAGE e166 WITH p_rldnr.
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

* Prerequesites:

* a) Check if Line Items Exist
  PERFORM check_lineItm changing Z_LI_EXIST.

* b) Check Length of Table
  PERFORM GET_TABLE_LENGHTH USING     'GLU1'
                            CHANGING  G_LENGTH_GLU1.

  PERFORM GET_TABLE_LENGHTH USING     T800A-TAB
                            CHANGING  G_LENGTH_STAB.


* START Check 1
  IF p_chck01 = 'X'.
    PERFORM SHOW_PROGRESS USING ZATEXT01.
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
    PERFORM SHOW_PROGRESS USING zatext05.
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
    PERFORM SHOW_PROGRESS USING ZATEXT02.
    PERFORM comp_sum USING h_org_name
                           p_rldnr
                           p_rrcty
                           p_rvers
                           p_rbukrs
                           p_rcomp
                           p_ryear
                     CHANGING it_err2[].

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
    PERFORM SHOW_PROGRESS USING ZATEXT03.
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
    PERFORM SHOW_PROGRESS USING ZATEXT04.
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


* START Check 11: Check Consistency of GLIDXA
  IF ( p_chck11 = 'X' ) AND ( Z_LI_EXIST = 'X' ).
    PERFORM SHOW_PROGRESS USING ZATEXT11.
    PERFORM check_glidxa USING       h_org_name
                                     z_poolcv
                            CHANGING it_glidxa[].
    DESCRIBE TABLE it_glidxa LINES zlines.
    IF zlines > 0.
      err_chck11 = 'F'.
    ELSE.
      err_chck11 = 'T'.
    ENDIF.
  ENDIF.



* START Check 6: Non FI Documents
  IF ( p_chck06 = 'X' ) AND ( Z_LI_EXIST = 'X' ).
    PERFORM SHOW_PROGRESS USING ZATEXT06.
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


* START Check 7: Check Number Range Object GL_RECID/GL_BUCHEN

  IF ( p_chck07 = 'X' ).
    PERFORM SHOW_PROGRESS USING ZATEXT07.
    CLEAR wa_norange.
    perform check_nrange  USING T800a
                       CHANGING wa_norange.

    IF NOT wa_norange-error IS INITIAL.
      err_chck7 = 'F'.
    ELSE.
      err_chck7 = 'T'.
    ENDIF.

  ENDIF.

* START Check 8
  IF ( p_chck08 = 'X' ).
    PERFORM SHOW_PROGRESS USING ZATEXT08.
    perform check_T800a USING t800a.
    DESCRIBE TABLE g_chkerr LINES zlines.
    IF zlines > 0.
      err_chck8 = 'F'.
    ELSE.
      err_chck8 = 'T'.
    ENDIF.
  ENDIF.


* START Check 9: Master Data Check
  IF ( p_chck09 = 'X' ).
    PERFORM SHOW_PROGRESS USING ZATEXT09.
    PERFORM check_T800d USING p_rldnr
                              p_ryear
                              r_perid.
    CLEAR zlines.
    LOOP AT gt_t800d_msg  TRANSPORTING NO FIELDS
            WHERE TYPE = 'E' OR TYPE = 'W'.
      zlines = zlines + 1.
    ENDLOOP.
    IF zlines > 0.
      err_chck9 = 'F'.
    ELSE.
      err_chck9 = 'T'.
    ENDIF.
  ENDIF.


* START Check 10: Fields and Tables Check
  IF ( p_chck10 = 'X' ).
    PERFORM SHOW_PROGRESS USING ZATEXT10.
    PERFORM check_fldtab.
    CLEAR zlines.
    LOOP AT gt_fldchk_msg TRANSPORTING NO FIELDS
            WHERE TYPE = 'E' OR TYPE = 'W'.
      zlines = zlines + 1.
    ENDLOOP.
    IF zlines > 0.
      err_chck10 = 'F'.
    ELSE.
      err_chck10 = 'T'.
    ENDIF.
  ENDIF.



* Output
  PERFORM SHOW_PROGRESS USING 'End of Analysis '.
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
  WRITE:/ '    Program Version   :', pgm_vers.
  WRITE:/ '    Job Date          :', syst-datum.
  WRITE:/ '    System            :', syst-sysid.
  IF gd_unicode IS INITIAL.
    WRITE '(NON-UNICODE)'.
  ELSE.
    IF gd_unicode = 'X'.
      WRITE '(UNICODE)'.
    ELSE.
      WRITE '(UNICODE/NON-UNICODE: ???)'.
    ENDIF.
  ENDIF.
  WRITE:/ '    SAP Basis Release :', syst-saprl.
  WRITE:/ '    Client            :', syst-mandt.

  WRITE:/.
  WRITE:/ '    Ledger            :', p_rldnr.
  WRITE:/ '    Summary Table     :', t881-tab.
  WRITE:/ '    Length of Sum.tab :', G_LENGTH_STAB.
  WRITE:/ '    Length of GLU1    :', G_LENGTH_GLU1.
  WRITE:/ .
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
  IF p_chck07 = 'X'.
    WRITE:/ '    - Check Number Range Objects: GL_RECID / GL_BUCHEN'.
  ENDIF.
  IF p_chck08 = 'X'.
    WRITE:/ '    - Check consistency of SL Directionary (T800A)'.
  ENDIF.
  IF p_chck09 = 'X'.
    WRITE:/ '    - Check Master Data Validation (T800D)'.
  ENDIF.
  IF p_chck10 = 'X'.
    WRITE:/ '    - Check Tables and Fields in T888M and T800D'.
  ENDIF.
  IF p_chck11 = 'X'.
    WRITE:/ '    - Check Consistency of Index Table GLIDXA'.
  ENDIF.




  SKIP 2.

  WRITE /(90) '2) Summarization of technical analyses :'
         COLOR COL_KEY LEFT-JUSTIFIED .

  SKIP 1.
  IF p_chck01 = 'X'.
    WRITE:/ '    Check 01: Object/Summary Table Inconsistency:'.
    PERFORM val_error USING err_chck1.
  ENDIF.
  IF p_chck02 = 'X'.
    WRITE:/ '    Check 02: Inc. betw Sum- and Line Item Table:'.
    PERFORM val_error USING err_chck2.
  ENDIF.
  IF p_chck03 = 'X'.
    WRITE:/ '    Check 03: Customizing settings              :'.
    PERFORM val_error USING err_chck3.
  ENDIF.
  IF p_chck04 = 'X'.
    WRITE:/ '    Check 04: Missing documents (FI -> FI-SL)   :'.
    IF Z_LI_EXIST = 'X'.
      PERFORM val_error USING err_chck4.
    else.
      WRITE 'Check not processed: No Line items in SL table'.
    ENDIF.
  ENDIF.
  IF P_CHCK05 = 'X'.
    WRITE:/ '    Check 05: Double Keys/Null Values Obj/SumTab:'.
    PERFORM VAL_ERROR USING ERR_CHCK5.
  ENDIF.
  IF p_chck06 = 'X'.
    WRITE:/ '    Check 06: Non FI documents (local/CO-docs.) :'.
    IF Z_LI_EXIST = 'X'.
      PERFORM val_error USING err_chck6.
    else.
      WRITE 'Check not processed: No Line items in SL table'.
    ENDIF.
  ENDIF.
  IF P_CHCK07 = 'X'.
    WRITE:/ '    Check 07: Check No.Range:GL_RECID/GL_BUCHEN :'.
    PERFORM VAL_ERROR USING ERR_CHCK7.
  ENDIF.
  IF P_CHCK08 = 'X'.
    WRITE:/ '    Check 08: Check SL Table Directory (T800a)  :'.
    PERFORM VAL_ERROR USING ERR_CHCK8.
  ENDIF.
  IF P_CHCK09 = 'X'.
    WRITE:/ '    Check 09: Master Data Validation (T800D)    :'.
    PERFORM VAL_ERROR USING ERR_CHCK9.
  ENDIF.
  IF P_CHCK10 = 'X'.
    WRITE:/ '    Check 10: Tables in Fields in Customizing   :'.
    PERFORM VAL_ERROR USING ERR_CHCK10.
  ENDIF.
  IF P_CHCK11 = 'X'.
    WRITE:/ '    Check 11: Consistency of Index GLIDXA       :'.
    IF Z_LI_EXIST = 'X'.
      PERFORM val_error USING err_chck11.
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
      WRITE / '- Please note: A program error has occured in this check'
.
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
      WRITE / '- Please note: A program error has occured in this check'
.
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


* Output Check 7
  IF ( P_CHCK07 = 'X' ) AND ( ERR_CHCK7 = 'F' ).
    SKIP 2.
    WRITE:/ '  '.
    FORMAT INTENSIFIED COLOR = 3.
    WRITE 'Check 7:'.
    FORMAT COLOR INTENSIFIED OFF.
    WRITE 'Check Number Range Object: GL_BUCHEN/GL_RECID '.
    SKIP 1.

    IF  wa_norange-error = 'T'.
      WRITE:/'   '.
      FORMAT INTENSIFIED COLOR = 6.
      WRITE 'SL Line Item Table has no standard structure.'.
      WRITE 'Check cannot be processed...'.
      FORMAT COLOR INTENSIFIED OFF.
    ENDIF.

    IF  wa_norange-error = 'R'.
      WRITE:/'   '.
      FORMAT INTENSIFIED COLOR = 6.
      WRITE 'Number Range Object Inconsistency: See note 62150 !!!'.
      FORMAT COLOR INTENSIFIED OFF.

      SKIP 2.
      WRITE:/7 'Number Range Object: GL_BUCHEN'.
      WRITE:/9 'Current Number in Object  : ',wa_norange-docnr_obj.
      IF wa_norange-docnr_obj <= wa_norange-docnr_dbs.
        WRITE '  <--- Error, see note 62150 '.
      ENDIF.
      WRITE:/9 'Highest Doc.No in LI-table: ',wa_norange-docnr_dbs.
      SKIP 1.
      WRITE:/7 'Number Range Object: GL_RECID'.
      WRITE:/9 'Current Number in Object  : ',wa_norange-recid_obj.
      IF ( wa_norange-recid_obj IS INITIAL ) AND
         ( wa_norange-recid_dbs IS INITIAL ).
        WRITE '--> Field GL_RECID not in table'.
      ELSE.
        IF wa_norange-recid_obj <= wa_norange-recid_dbs.
          WRITE '  <--- Error, see note 62150'.
        ENDIF.
      ENDIF.

      WRITE:/9 'Highest Rec-Id in LI-table: ',wa_norange-recid_dbs.
    ENDIF.
  ENDIF.

* Output Check 8
  IF ( P_CHCK08 = 'X' ) AND ( ERR_CHCK8 = 'F' ).
    SKIP 2.
    WRITE:/ '  '.
    FORMAT INTENSIFIED COLOR = 3.
    WRITE 'Check 8:'.
    FORMAT COLOR INTENSIFIED OFF.
    WRITE 'Check FI-SL Table Directory T800A (transaction GCI3)'.
    SKIP 1.
    LOOP AT g_chkerr WHERE checkno = '008'.
      WRITE:/4 '- ', g_chkerr-TEXT.
    ENDLOOP.
  ENDIF.


* Output Check 9
  IF ( P_CHCK09 = 'X' ) AND ( ERR_CHCK9 = 'F' ).
    SKIP 2.
    WRITE:/ '  '.
    FORMAT INTENSIFIED COLOR = 3.
    WRITE 'Check 9:'.
    FORMAT COLOR INTENSIFIED OFF.
    WRITE 'Check Customizing of Master Data Validation (T800D) '.
    SKIP 1.
    write:/5  'Table         : ', gs_881-tab.
    WRITE:/5  'Valid at      : ', G_DATE.
    IF NOT P_T8DBUK IS INITIAL.
      WRITE:/5  'Company Code  : ', p_rbukrs.
    ELSE.
      WRITE:/5  'Company Code  :  *Any*'.
    ENDIF.
    WRITE:/5 '(Check only relev. for Allocation or Planning problems)'.
    skip.
    write: /5 zttext08.
    loop at gt_t800d_msg.
      case gt_t800d_msg-type.
        when 'I'.
          WRITE: /7 'I' COLOR COL_NORMAL .
          write: gt_t800d_msg-line.
        when 'W'.
          WRITE: /7 'W' COLOR COL_TOTAL .
          write  gt_t800d_msg-line.
        when 'E'.
          WRITE: /7 'E' COLOR COL_NEGATIVE .
          write  gt_t800d_msg-line.
        WHEN OTHERS.
          write:/7 gt_t800d_msg-line.
      endcase.
    endloop.
    SKIP 2.
    WRITE:/5 '- G_READ* Error means: System cannot read Master Data. '.
    WRITE:/5 '  If error: please check entries using transaction GCS1'.
  ENDIF.

* Output Check 10
  IF ( P_CHCK10 = 'X' ) AND ( ERR_CHCK10 = 'F' ).
    SKIP 2.
    WRITE:/ '  '.
    FORMAT INTENSIFIED COLOR = 3.
    WRITE 'Check 10:'.
    FORMAT COLOR INTENSIFIED OFF.
    WRITE 'Check Fields in Field Movement and M.D.Validation '.
    SKIP 2.
    WRITE:/5  '-----Legend--------------------------------------------'.
    WRITE: /5 'W' COLOR COL_TOTAL .
    WRITE ' = Warning. Entry not correct but currently no problem'.

    WRITE: /5 'E' COLOR COL_NEGATIVE .
    WRITE ' = Error. May cause system dump. Lines should be corrected'.

    SKIP 2.
    write:/5 'Client ' .
    WRITE 'FieldMvmnt '.
    WRITE 'Transaction '.
    WRITE 'Cust.Table  '.
    WRITE 'Ledger Table         '.
    WRITE 'Field               '.
    WRITE 'Component  '.
    ULINE.
    skip.
    SORT gt_fldchk_msg BY issue1 type client.
* Output variable field Movement
    loop at gt_fldchk_msg WHERE ( type = 'E' OR type = 'W' ) AND
                                  issue1 = 'VAR'.
      WRITE: /6 gt_fldchk_msg-client.
      WRITE 14 gt_fldchk_msg-FELDMODIF .
      WRITE 30 gt_fldchk_msg-tcode .
      WRITE 40(20) gt_fldchk_msg-custab .
      WRITE 52(20) gt_fldchk_msg-sltab .
      WRITE (20) gt_fldchk_msg-field.
      WRITE  gt_fldchk_msg-component .
      IF gt_fldchk_msg-type eq 'E'.
        WRITE: /5 'E' COLOR COL_NEGATIVE .
      ENDIF.
      IF gt_fldchk_msg-type eq 'W'.
        WRITE: /5 'W' COLOR COL_TOTAL .
      ENDIF.
      WRITE: 8 gt_fldchk_msg-line .
      ULINE.
    endloop.
    IF SY-SUBRC <> 0.
      WRITE:/5 'No critical field movement entries found'.
    ENDIF.
    SKIP 3.
* Master Data Validation
    write:/5 'Client ' .
    WRITE 'Transaction '.
    WRITE 'Cust.Table  '.
    WRITE 'Ledger Table         '.
    WRITE 'Field               '.
    WRITE 'Component  '.
    ULINE.
    skip.
    loop at gt_fldchk_msg WHERE ( type = 'E' OR type = 'W' ) AND
                                  issue1 = 'MAST'.
      WRITE: /6 gt_fldchk_msg-client.
      WRITE 14 gt_fldchk_msg-tcode .
      WRITE 28(20) gt_fldchk_msg-custab .
      WRITE 40(20) gt_fldchk_msg-sltab .
      WRITE (20) gt_fldchk_msg-field.
      WRITE  gt_fldchk_msg-component .
      IF gt_fldchk_msg-type eq 'E'.
        WRITE: /5 'E' COLOR COL_NEGATIVE .
      ENDIF.
      IF gt_fldchk_msg-type eq 'W'.
        WRITE: /5 'W' COLOR COL_TOTAL .
      ENDIF.
      WRITE: 8 gt_fldchk_msg-line .
      ULINE.
    ENDLOOP.
    IF SY-SUBRC <> 0.
      WRITE:/5 'No critical entries in Master Data Validation found'.
    ENDIF.


* Client Check
    READ TABLE gt_fldchk_msg WITH KEY issue1 = 'CLNT'.
    IF sy-subrc = 0.
      SKIP 3.
      WRITE:/5 '- Inconsistent table entries found:'.
      SKIP 1.
      LOOP AT gt_fldchk_msg WHERE issue1 = 'CLNT'.
        WRITE: /5 'E' COLOR COL_NEGATIVE .
        WRITE: 8 gt_fldchk_msg-line .
      ENDLOOP.
    ENDIF.

  ENDIF.


* Output Check 11
  IF p_chck11 = 'X' AND ( err_chck11 = 'F' ).
    SKIP 2.
    WRITE:/ '  '.
    FORMAT INTENSIFIED COLOR = 3.
    WRITE 'Check 11:'.
    FORMAT COLOR INTENSIFIED OFF.
    WRITE: 'Consistency of Index Table GLIDXA'.
    SKIP 1.
    WRITE AT /47 '------Entry missing in------' .
    WRITE AT /5 'Year CoCd SL-DocNo   AWTYP  AWREF        '.
    WRITE 'GLIDXA   LI-Table   FI-BKPF' .
    WRITE AT /1(90) sy-uline.
    LOOP AT it_glidxa INTO wa_glidxa.
      WRITE AT /5 wa_glidxa-RYEAR.
      WRITE wa_glidxa-BUKRS.
      WRITE wa_glidxa-DOCNR.
      WRITE wa_glidxa-awtyp.
      WRITE wa_glidxa-awref.
      IF NOT wa_glidxa-notin_glidxa IS INITIAL.
        WRITE 48 'IDX'.
      ENDIF.
      IF NOT wa_glidxa-notin_litable IS INITIAL.
        WRITE 58 'SL'.
      ENDIF.
      IF NOT wa_glidxa-notin_bkpf IS INITIAL.
        WRITE 68 'FI'.
      ENDIF.
    ENDLOOP.
  ENDIF.

*----------------------------------------------------------------



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
    WRITE 'Warning! See detail list'.
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
          splitidx(1) TYPE c,"  X =  missing Index for Splitt Ledger
         END of wa_cust.

  DATA it_t800m LIKE t800m OCCURS 0 WITH HEADER LINE.
  DATA it_t888m LIKE t888m OCCURS 0 WITH HEADER LINE.
  DATA lt_dd17v LIKE dd17v occurs 0 with header LINE.
  DATA lf_real_table like t800a-ntable.


  RANGES  rl_activ FOR t022t-activity.

  MOVE SPACE TO wa_cust.

* Check Missing Index for Splitt Ledger

  IF NOT T881-SPLITMETHD IS INITIAL.
    lf_real_table = T800a-ntable.
    CALL FUNCTION 'G_FIELD_READ'
      EXPORTING
        TABLE     = lf_real_table
        FIELDNAME = 'BELNR'
      EXCEPTIONS
        NOT_FOUND = 1
        OTHERS    = 2.
    IF sy-subrc eq 0.
      CALL FUNCTION 'DD_TABL_GET'
        EXPORTING
          TABL_NAME   = lf_real_table
        TABLES
          DD17V_TAB_N = lt_DD17V.
      loop at lt_dd17v where fieldname = 'BELNR'.
        exit.
      endloop.
      if sy-subrc ne 0.
        wa_cust-splitidx = 'X'.
      endif.
    ENDIF.
  ENDIF.

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

  IF wa_cust-splitidx = 'X'.
    CLEAR ZTEXT.
    WRITE '- ' TO ZTEXT+3(3).
    WRITE ZCTEXT13 TO ZTEXT+6(47).
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

  PERFORM APPEND_CODE USING  0 0 'data: begin of it_clnt occurs 5,'
SPACE.
  PERFORM APPEND_CODE USING  0 0 '        issue(6) type c,' SPACE.
  PERFORM APPEND_CODE USING  0 0 '        client like t001-mandt,' SPACE
.
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
  PERFORM APPEND_CODE USING  0 0 'data: help_tabix like sy-tabix.' SPACE
.


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
  PERFORM APPEND_CODE USING  0 0 'collect wa_clnt into it_client.' SPACE
.
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
      INDEXNAME = '1  '
      TABNAME   = OBJ_TABLE
    IMPORTING
      DBINDEX   = z_indtab.


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
  PERFORM APPEND_CODE USING  0 0 'collect wa_clnt into it_client.' SPACE
.
  PERFORM APPEND_CODE USING -2 0 'endif.' SPACE.
  PERFORM APPEND_CODE USING -1 0 'endselect.' SPACE.

  WORK = 'FREE GOOD_$ .'.
  REPLACE '$' INTO WORK WITH OBJ_TABLE.
  CONDENSE WORK.
  PERFORM APPEND_CODE USING  0 0 WORK SPACE.

  PERFORM APPEND_CODE USING  0 0 SPACE SPACE.

ENDFORM.                    "CRT_OBJ_CODE





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


*---------------------------------------------------------------------*
*       FORM check_T800a                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE(LD_T800A)                                               *
*---------------------------------------------------------------------*
FORM check_T800a USING VALUE(ld_t800a) LIKE T800a.

  DATA: tmp_t800a    LIKE T800a.
  DATA: ld_sumtab    LIKE T800a-tab.
  DATA: ld_ntable    LIKE T800a-NTABLE.
  DATA: ld_psitable  LIKE T800a-psitable.
  DATA: ld_objtable1  LIKE T800a-objtable.
  DATA: ld_objtable2 LIKE T800a-objtable2.
  DATA: ld_error(1)  TYPE c.
  DATA: ld_subrc like SY-SUBRC.
  CLEAR ld_error.

  ld_sumtab     = ld_t800a-tab.
  ld_ntable     = ld_t800a-NTABLE.
  ld_psitable   = ld_T800a-psitable.
  ld_objtable1  = ld_T800a-objtable.
  ld_objtable2  = ld_T800a-objtable2.

  ld_error = ld_t800a-inactive.

* Check if table activ in Dictionary

* Summary Table
  IF NOT ld_sumtab IS INITIAL.
    CALL FUNCTION 'G_TABLE_READ_INACTIVE'
      EXPORTING
        TABLE               = ld_sumtab
      EXCEPTIONS
        ACTIVE_AND_INACTIVE = 1
        NOT_FOUND           = 2
        ONLY_ACTIVE         = 3
        ONLY_INACTIVE       = 4
        OTHERS              = 5.

    IF ( SY-SUBRC <> 1 ) AND ( SY-SUBRC <> 3 ).
      CLEAR g_chkerr.
      g_chkerr-rating  = "r".
      g_chkerr-checkno = '008'.
      g_chkerr-pos     = 1.
      g_chkerr-MSGID = SY-MSGID.
      g_chkerr-number = SY-MSGNO.
      g_chkerr-text = 'Please check table $ in Dictionary SE11'.
      REPLACE '$' INTO g_chkerr-text WITH ld_sumtab.
      CONDENSE g_chkerr-text.
      APPEND g_chkerr.
    ENDIF.
  ENDIF.

* LI Table
  IF NOT ld_ntable IS INITIAL.
    CALL FUNCTION 'G_TABLE_READ_INACTIVE'
      EXPORTING
        TABLE               = ld_ntable
      EXCEPTIONS
        ACTIVE_AND_INACTIVE = 1
        NOT_FOUND           = 2
        ONLY_ACTIVE         = 3
        ONLY_INACTIVE       = 4
        OTHERS              = 5.

    IF ( SY-SUBRC <> 1 ) AND ( SY-SUBRC <> 3 ).
      CLEAR g_chkerr.
      g_chkerr-rating  = "r".
      g_chkerr-checkno = '008'.
      g_chkerr-pos     = 2.
      g_chkerr-MSGID = SY-MSGID.
      g_chkerr-number = SY-MSGNO.
      g_chkerr-text = 'Please check table $ in Dictionary SE11'.
      REPLACE '$' INTO g_chkerr-text WITH ld_ntable.
      CONDENSE g_chkerr-text.
      APPEND g_chkerr.
    ENDIF.
  ENDIF.


* Plan Item Table
  IF NOT ld_psitable IS INITIAL.
    CALL FUNCTION 'G_TABLE_READ_INACTIVE'
      EXPORTING
        TABLE               = ld_psitable
      EXCEPTIONS
        ACTIVE_AND_INACTIVE = 1
        NOT_FOUND           = 2
        ONLY_ACTIVE         = 3
        ONLY_INACTIVE       = 4
        OTHERS              = 5.

    IF ( SY-SUBRC <> 1 ) AND ( SY-SUBRC <> 3 ).
      CLEAR g_chkerr.
      g_chkerr-rating  = "r".
      g_chkerr-checkno = '008'.
      g_chkerr-pos     = 3.
      g_chkerr-MSGID = SY-MSGID.
      g_chkerr-number = SY-MSGNO.
      g_chkerr-text = 'Please check table $ in Dictionary SE11'.
      REPLACE '$' INTO g_chkerr-text WITH ld_psitable.
      CONDENSE g_chkerr-text.
      APPEND g_chkerr.
    ENDIF.
  ENDIF.

* Object Table 1
  IF NOT ld_objtable1 IS INITIAL.
    CALL FUNCTION 'G_TABLE_READ_INACTIVE'
      EXPORTING
        TABLE               = ld_objtable1
      EXCEPTIONS
        ACTIVE_AND_INACTIVE = 1
        NOT_FOUND           = 2
        ONLY_ACTIVE         = 3
        ONLY_INACTIVE       = 4
        OTHERS              = 5.

    IF ( SY-SUBRC <> 1 ) AND ( SY-SUBRC <> 3 ).
      CLEAR g_chkerr.
      g_chkerr-rating  = "r".
      g_chkerr-checkno = '008'.
      g_chkerr-pos     = 4.
      g_chkerr-MSGID = SY-MSGID.
      g_chkerr-number = SY-MSGNO.
      g_chkerr-text = 'Please check table $ in Dictionary SE11'.
      REPLACE '$' INTO g_chkerr-text WITH ld_objtable1.
      CONDENSE g_chkerr-text.
      APPEND g_chkerr.
    ENDIF.
  ENDIF.

* Object Table 2
  IF NOT ld_objtable2 IS INITIAL.
    CALL FUNCTION 'G_TABLE_READ_INACTIVE'
      EXPORTING
        TABLE               = ld_objtable2
      EXCEPTIONS
        ACTIVE_AND_INACTIVE = 1
        NOT_FOUND           = 2
        ONLY_ACTIVE         = 3
        ONLY_INACTIVE       = 4
        OTHERS              = 5.

    IF ( SY-SUBRC <> 1 ) AND ( SY-SUBRC <> 3 ).
      CLEAR g_chkerr.
      g_chkerr-rating  = "r".
      g_chkerr-checkno = '008'.
      g_chkerr-pos     = 5.
      g_chkerr-MSGID = SY-MSGID.
      g_chkerr-number = SY-MSGNO.
      g_chkerr-text = 'Please check table $ in dictionary SE11'.
      REPLACE '$' INTO g_chkerr-text WITH ld_objtable2.
      CONDENSE g_chkerr-text.
      APPEND g_chkerr.
    ENDIF.
  ENDIF.

*------------- End Dictionary Check of table group ------------------

* Check if table activ in T800a

* Check Summary Table
  IF ld_t800a-inactive = 'X'.
    CLEAR g_chkerr.
    g_chkerr-rating  = "r".
    g_chkerr-checkno = '008'.
    g_chkerr-pos     = 6.
    g_chkerr-text = 'Table $ not activ in SL table directory T800a'.
    REPLACE '$' INTO g_chkerr-text WITH ld_sumtab.
    CONDENSE g_chkerr-text.
    APPEND g_chkerr.
  ENDIF.

* Check LI Table
  IF NOT ld_ntable IS INITIAL.
    SELECT SINGLE * FROM T800A INTO ld_t800a
        WHERE TAB = ld_ntable.
    ld_subrc = SY-SUBRC.
    IF ( ld_subrc <> 0 ) OR  ( ld_t800a-inactive = 'X' ).
      CLEAR g_chkerr.
      g_chkerr-rating  = "r".
      g_chkerr-checkno = '008'.
      g_chkerr-pos     = 7.
      g_chkerr-text = 'Table $ not activ in SL table directory T800a'.
      REPLACE '$' INTO g_chkerr-text WITH ld_ntable.
      CONDENSE g_chkerr-text.
      APPEND g_chkerr.
    ENDIF.
    IF ld_subrc = 0.
      IF ld_sumtab    ne ld_t800a-NTABLE.
        CLEAR g_chkerr.
        g_chkerr-rating  = "r".
        g_chkerr-checkno = '008'.
        g_chkerr-pos     = 8.
        g_chkerr-text = 'Error in T800a: corresponding Tables $/ $'.
        REPLACE '$' INTO g_chkerr-text WITH ld_ntable.
        REPLACE '$' INTO g_chkerr-text WITH ld_sumtab.
        CONDENSE g_chkerr-text.
        APPEND g_chkerr.
      ENDIF.
      IF  ( ld_psitable  ne ld_T800a-psitable ) AND
      NOT ld_T800a-objtable IS INITIAL.   " -> Pool table
        CLEAR g_chkerr.
        g_chkerr-rating  = "r".
        g_chkerr-checkno = '008'.
        g_chkerr-pos     = 9.
        g_chkerr-text = 'Error in T800a: corresponding Tables $/ $'.
        REPLACE '$' INTO g_chkerr-text WITH ld_ntable.
        REPLACE '$' INTO g_chkerr-text WITH ld_psitable.
        APPEND g_chkerr.
        CONDENSE g_chkerr-text.
      ENDIF.
      IF ld_objtable1 ne ld_T800a-objtable.
        CLEAR g_chkerr.
        g_chkerr-rating  = "r".
        g_chkerr-checkno = '008'.
        g_chkerr-pos     = 10.
        g_chkerr-text = 'Error in T800a: corresponding Tables $/ $'.
        REPLACE '$' INTO g_chkerr-text WITH ld_ntable.
        REPLACE '$' INTO g_chkerr-text WITH ld_objtable1.
        CONDENSE g_chkerr-text.
        APPEND g_chkerr.
      ENDIF.
      IF ld_objtable2 ne ld_T800a-objtable2.
        CLEAR g_chkerr.
        g_chkerr-rating  = "r".
        g_chkerr-checkno = '008'.
        g_chkerr-pos     = 11.
        g_chkerr-text = 'Error in T800a: corresponding Tables $/ $'.
        REPLACE '$' INTO g_chkerr-text WITH ld_ntable.
        REPLACE '$' INTO g_chkerr-text WITH ld_objtable2.
        CONDENSE g_chkerr-text.
        APPEND g_chkerr.
      ENDIF.
    ENDIF.
  ENDIF.


* Check Plan Line Item Table
  IF NOT ld_psitable IS INITIAL.
    SELECT SINGLE * FROM T800A INTO ld_t800a
        WHERE TAB = ld_psitable.
    ld_subrc = SY-SUBRC.
    IF ( ld_subrc <> 0 ) OR  ( ld_t800a-inactive = 'X' ).
      CLEAR g_chkerr.
      g_chkerr-rating  = "r".
      g_chkerr-checkno = '008'.
      g_chkerr-pos     = 7.
      g_chkerr-text = 'Table $ not activ in SL table directory T800a'.
      REPLACE '$' INTO g_chkerr-text WITH ld_psitable.
      CONDENSE g_chkerr-text.
      APPEND g_chkerr.
    ENDIF.
    IF ld_subrc = 0.
      IF ld_sumtab ne ld_t800a-NTABLE.
        CLEAR g_chkerr.
        g_chkerr-rating  = "r".
        g_chkerr-checkno = '008'.
        g_chkerr-pos     = 8.
        g_chkerr-text = 'Error in T800a: corresponding Tables $/ $'.
        REPLACE '$' INTO g_chkerr-text WITH ld_psitable.
        REPLACE '$' INTO g_chkerr-text WITH ld_sumtab.
        CONDENSE g_chkerr-text.
        APPEND g_chkerr.
      ENDIF.
      IF ld_objtable1 ne ld_T800a-objtable.
        CLEAR g_chkerr.
        g_chkerr-rating  = "r".
        g_chkerr-checkno = '008'.
        g_chkerr-pos     = 9.
        g_chkerr-text = 'Error in T800a: corresponding Tables $/ $'.
        REPLACE '$' INTO g_chkerr-text WITH ld_psitable.
        REPLACE '$' INTO g_chkerr-text WITH ld_objtable1.
        CONDENSE g_chkerr-text.
        APPEND g_chkerr.
      ENDIF.
      IF ld_objtable2 ne ld_T800a-objtable2.
        CLEAR g_chkerr.
        g_chkerr-rating  = "r".
        g_chkerr-checkno = '008'.
        g_chkerr-pos     = 10.
        g_chkerr-text = 'Error in T800a: corresponding Tables $/ $'.
        REPLACE '$' INTO g_chkerr-text WITH ld_psitable.
        REPLACE '$' INTO g_chkerr-text WITH ld_objtable2.
        CONDENSE g_chkerr-text.
        APPEND g_chkerr.
      ENDIF.
    ENDIF.
  ENDIF.


ENDFORM.                    "check_T800a



*&--------------------------------------------------------------------*
*&      Form  check_nrange
*&--------------------------------------------------------------------*
*       Check Number Range Object GL_RECID/GL_BUCHEN                  *
*---------------------------------------------------------------------*
*      -->VALUE(T800Atext
*      -->WA_NORANGE text
*---------------------------------------------------------------------*
FORM check_nrange  USING  value(ZT800a) LIKE T800A
                   CHANGING no_range LIKE wa_norange.

  DATA: BEGIN OF ls_INFO.
          INCLUDE STRUCTURE NRIV.
  DATA: END OF ls_INFO.

  DATA: ld_nr LIKE nriv-nrlevel.
  DATA: ld_litab LIKE T800a-TAB.

  DATA: dref_tab TYPE REF TO data.

  field-symbols: <wa_tab>   TYPE ANY,
                 <anyfield> TYPE ANY.


  CLEAR ld_nr.
  ld_litab = T800a-NTABLE.

* Check fields in line item table
  CATCH SYSTEM-EXCEPTIONS CREATE_DATA_UNKNOWN_TYPE = 5.
    create DATA dref_tab TYPE (ld_litab).
    ASSIGN dref_tab->* TO <wa_tab>.
  ENDCATCH.
  IF sy-subrc = 5.
    no_range-error = 'T'. "Table Structure not SL standard
    EXIT.
  ENDIF.

* Check if field DOCNR in line item table
  ASSIGN COMPONENT 'DOCNR' OF STRUCTURE <wa_tab> TO <anyfield>.
  IF sy-subrc ne 0.
    no_range-error = 'T'. "Table Structure not SL standard
    EXIT.
  ENDIF.


* Get highest document number in client
  SELECT MAX( DOCNR ) FROM (ld_litab) into no_range-docnr_dbs.

* Get highest recid number in LI table
  ASSIGN COMPONENT 'GL_SIRID' OF STRUCTURE <wa_tab> TO <anyfield>.
  IF sy-subrc eq 0.
    SELECT MAX( GL_SIRID ) FROM (ld_litab) into no_range-recid_dbs.
    IF sy-subrc ne 0.
      CLEAR no_range-recid_dbs.
    ENDIF.
* Get GL_RECID number range status
    CLEAR LS_INFO.
    CALL FUNCTION 'NUMBER_GET_INFO'
      EXPORTING
        OBJECT             = 'GL_RECID'
        NR_RANGE_NR        = '01'
      IMPORTING
        INTERVAL           = LS_INFO
      EXCEPTIONS
        INTERVAL_NOT_FOUND = 1
        OBJECT_NOT_FOUND   = 2
        OTHERS             = 3.

    IF sy-subrc ne 0.
      CLEAR ld_nr.
    ELSE.
      ld_nr = LS_INFO-NRLEVEL.
    ENDIF.
    MOVE ld_nr+2 TO no_range-recid_obj.
  ELSE.
    CLEAR no_range-recid_dbs.
  ENDIF.

* Get GL_BUCHEN number range status
  CLEAR LS_INFO.
  CALL FUNCTION 'NUMBER_GET_INFO'
    EXPORTING
      OBJECT             = 'GL_BUCHEN'
      NR_RANGE_NR        = '01'
    IMPORTING
      INTERVAL           = LS_INFO
    EXCEPTIONS
      INTERVAL_NOT_FOUND = 1
      OBJECT_NOT_FOUND   = 2
      OTHERS             = 3.

  IF sy-subrc ne 0.
    CLEAR ld_nr.
  ELSE.
    ld_nr = LS_INFO-NRLEVEL.
  ENDIF.

  MOVE ld_nr+10 TO no_range-docnr_obj.

  no_range-error = ' '.

  IF no_range-docnr_dbs >= no_range-docnr_obj.
    no_range-error = 'R'.    "Number Range Error
  ENDIF.

  IF no_range-recid_dbs >= no_range-recid_obj.
    no_range-error = 'R'.    "Number Range Error
  ENDIF.


ENDFORM.                    "check_nrange

*&---------------------------------------------------------------------*
*&      Form  check_T800d
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_RLDNR  text
*      -->P_P_RYEAR  text
*      -->P_R_PERID  text
*      <--P_IT_ERR9[]  text
*----------------------------------------------------------------------*
FORM check_T800d USING    e01_rldnr TYPE T881-RLDNR
                          ld_RYEAR TYPE GJAHR
                          r_PERID.

  CALL FUNCTION 'MESSAGES_INITIALIZE'
    EXPORTING
      COLLECT_AND_SEND = ' '
      RESET            = 'X'.

* FILL G_DATE from Selection Screen
  G_DATE = p_mdate.
  IF G_DATE IS INITIAL.
    G_DATE = SYST-DATUM.
  ENDIF.

  select single * from t881 into gs_881 where rldnr = e01_rldnr.
  if sy-subrc ne 0.
    message e203(GU).
  endif.
  select single * from t800a into gs_800a where tab = gs_881-tab.
*fill minimum and maximum values
  perform FILL_MAX_AND_MIN_VALUES USING   'CHAR'
                                           c_orglen
                                  CHANGING gf_orgunit_min
                                           gf_orgunit_max.

*Get all relevant company codes and companies
  CALL FUNCTION 'G_GET_ORGANIZATIONAL_DATA_INT'
    EXPORTING
      I_RLDNR_FR            = gs_881-rldnr
      I_RLDNR_TO            = gs_881-rldnr
      I_ORGUNIT_FR          = gf_orgunit_min
      I_ORGUNIT_TO          = gf_orgunit_max
    TABLES
      T_ORGANIZATIONAL_INFO = gt_org_info
    EXCEPTIONS
      others                = 1.
  if sy-subrc ne 0.
    message e610(GU) with gs_881-rldnr.
  endif.
*Get fields from DDIC
  CALL FUNCTION 'G_FIELDTAB_GET'
    EXPORTING
      TABLE      = gs_800a-tab
    TABLES
      T_FIELDTAB = gt_fieldtab.
*Get all relevant exit fields
  perform get_exit_fields.
*Check whether t800d entry exists
  perform check_t800d_entries.
ENDFORM.                    " check_master_data

*&---------------------------------------------------------------------*
*&      Form  get_exit_fields
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_exit_fields.
  data: lt_fieldtab like gt_fieldtab occurs 0 with header line.
  lt_fieldtab[] = gt_fieldtab[].
  refresh gt_fieldtab.
* standard fields with no correct exit
  delete lt_fieldtab where fieldname = 'RRCTY'
                        or fieldname = 'RYEAR'
                        or fieldname = 'RPMAX'
                        or fieldname = 'RCLNT'
                        or fieldname = 'OCLNT'
                        or fieldname = 'LOGSYS'
                        or fieldname = 'DRCRK'.
*Sender fields need no entry in t800d
  loop at lt_fieldtab.
    if lt_fieldtab-fieldname(1) ne 'S'.
      gt_fieldtab = lt_fieldtab.
      append gt_fieldtab.
    else.
      lt_fieldtab-fieldname(1) = 'R'.
      read table lt_fieldtab with key fieldname = lt_fieldtab-fieldname.
      check sy-subrc ne 0.
      lt_fieldtab-fieldname(1) = 'S'.
      gt_fieldtab = lt_fieldtab.
      append gt_fieldtab.
    endif.
  endloop.
endform.                    " get_exit_fields
*&---------------------------------------------------------------------*
*&      Form  check_t800d_entries
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_t800d_entries.

  sy-fdpos = strlen( gs_800a-tab ).
  assign gs_800a-tab(sy-fdpos) to <gf_tab>.

  loop at gt_fieldtab.
    sy-fdpos = strlen( gt_fieldtab-fieldname ).
    assign gt_fieldtab-fieldname(sy-fdpos) to <gf_field>.
    select single * from t800d into gt_800d
           where tab = gs_800a-tab
           and feld = gt_fieldtab-fieldname.
    if sy-subrc = 0.
      perform call_user_exit using gt_800d.
    else.
      gt_t800d_msg-line = zttext00.
      replace '&' into gt_t800d_msg-line with:
      <gf_field>.
      gt_t800d_msg-type = 'W'.
      append gt_t800d_msg.
    endif.
  endloop.
ENDFORM.                    " check_t800d_entries

*---------------------------------------------------------------------*
*       FORM FILL_MAX_AND_MIN_VALUES                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  U_FILL_MIN                                                    *
*  -->  U_FILL_MAX                                                    *
*  -->  U_FILLED                                                      *
*---------------------------------------------------------------------*
FORM FILL_MAX_AND_MIN_VALUES USING    U_DATA_TYPE TYPE DFIES-DATATYPE
                                      U_LENGTH TYPE DDLENG
                             CHANGING U_FILL_MIN
                                      U_FILL_MAX.

  "Dynamic selects for value tables do not work with HEX format!
  DATA: BEGIN OF U_FILL_HEXFF_NONUCDE,
*         X1(1) TYPE X VALUE 'FF',
          X1(1) TYPE c VALUE 'Z',
        END OF U_FILL_HEXFF_NONUCDE.

  DATA: BEGIN OF U_FILL_HEX99_NONUCDE,
*         X1(1) TYPE X VALUE 'FF',
          X1(1) TYPE c VALUE '9',
        END OF U_FILL_HEX99_NONUCDE.


  DATA: BEGIN OF U_FILL_HEX00_NONUCDE,
*         X1(1) TYPE X VALUE '00',
          X1(1) TYPE C VALUE '0',
        END OF U_FILL_HEX00_NONUCDE.

  DATA: U_FILL_MAX_LENGTH TYPE I,
        U_FILL_OFFSET TYPE I.

* DESCRIBE FIELD U_FILL_MIN LENGTH U_FILL_MAX_LENGTH IN CHARACTER MODE .

  U_FILL_MAX_LENGTH = U_LENGTH.

  U_FILL_OFFSET = 0.
  WHILE U_FILL_OFFSET < U_FILL_MAX_LENGTH.
    U_FILL_MIN+U_FILL_OFFSET(1) = U_FILL_HEX00_NONUCDE.
    U_FILL_OFFSET = U_FILL_OFFSET + 1.
  ENDWHILE.

* DESCRIBE FIELD U_FILL_MAX LENGTH U_FILL_MAX_LENGTH IN CHARACTER MODE .

  U_FILL_MAX_LENGTH = U_LENGTH.

  U_FILL_OFFSET = 0.
  IF U_DATA_TYPE = 'NUMC'.
    WHILE U_FILL_OFFSET < U_FILL_MAX_LENGTH.
      U_FILL_MAX+U_FILL_OFFSET(1) = U_FILL_HEX99_NONUCDE.
      U_FILL_OFFSET = U_FILL_OFFSET + 1.
    ENDWHILE.
  ELSE.
    WHILE U_FILL_OFFSET < U_FILL_MAX_LENGTH.
      U_FILL_MAX+U_FILL_OFFSET(1) = U_FILL_HEXFF_NONUCDE.
      U_FILL_OFFSET = U_FILL_OFFSET + 1.
    ENDWHILE.
  ENDIF.

ENDFORM.                    "FILL_MAX_AND_MIN_VALUES
*&---------------------------------------------------------------------*
*&      Form  call_user_exits
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_800D  text
*----------------------------------------------------------------------*
FORM call_user_exit USING  cue_800d like t800d.
  data: ld_area_exit_works.
  data: gt_igmd00 like gmd00 occurs 0 with header line,
        gt_igmdtt like gmdtt occurs 0 with header line,
        gt_periods like periods occurs 0 with header line.

*Only BUKRS, RCOMP and KOKRS can be checked as superior fields
*Attention: GLPCT has several fields with RFIELD1 = KOKRS
*and RFIELD2 = BUKRS
  if cue_800d-rfield1  ne space
  and cue_800d-rfield1 ne 'RLDNR'
  and cue_800d-rfield1 ne 'BUKRS'
  and cue_800d-rfield1 ne 'RBUKRS'
  and cue_800d-rfield1 ne 'RCOMP'
  and cue_800d-rfield1 ne 'KOKRS'
  and cue_800d-rfield1 ne 'RKOKRS'.
    gt_t800d_msg-line = zttext01.
    replace '&' into gt_t800d_msg-line with:
    <gf_field>, 'RFIELD1'.
    gt_t800d_msg-type = 'W'.
    append gt_t800d_msg.
    exit.
  endif.
  if cue_800d-rfield2  ne space
  and cue_800d-rfield2 ne 'RLDNR'
  and cue_800d-rfield2 ne 'BUKRS'
  and cue_800d-rfield2 ne 'RBUKRS'
  and cue_800d-rfield2 ne 'RCOMP'
  and cue_800d-rfield2 ne 'KOKRS'
  and cue_800d-rfield2 ne 'RKOKRS'
  and cue_800d-rfield2 ne 'RBUSA'.     "for cost center
    gt_t800d_msg-line = zttext01.
    replace '&' into gt_t800d_msg-line with:
    <gf_field>, 'RFIELD2'.
    gt_t800d_msg-type = 'W'.
    append gt_t800d_msg.
    exit.
  endif.

*RFIELD3-5 can not be checked
  if cue_800d-rfield3  ne space
  or cue_800d-rfield4  ne space
  or cue_800d-rfield5  ne space.
    gt_t800d_msg-line = zttext02 .
    replace '&' into gt_t800d_msg-line with:
    <gf_field>.
    gt_t800d_msg-type = 'W'.
    append gt_t800d_msg.
    exit.
  endif.
*Fill GT_PERIODS
  gt_periods-datab = gt_periods-datbi = g_date.
  gt_periods-buper = g_date+4(2).
  append gt_periods.

*fill minimum and maximum values
  perform FILL_MAX_AND_MIN_VALUES USING    gt_fieldtab-datatype
                                           gt_fieldtab-LENG
                                  CHANGING gf_value_from
                                           gf_value_to.
*Now call the specific exit.
  loop at gt_org_info.
    IF NOT p_T8dbuk IS INITIAL.
      IF gt_org_info-bukrs NE p_rbukrs.
        CONTINUE.
      ENDIF.
    ENDIF.
    if ld_area_exit_works = 'X'.
      exit.
    endif.
*Fill superior field
    if not cue_800d-rfield1 is initial.
      gf_work =  'GLU1-$'.
      REPLACE '$' INTO gf_WORK WITH cue_800d-rfield1.
      condense gf_work.
      assign (gf_work) to <tabfield>.
      if gs_800a-comptab is initial.
        <tabfield> = gt_org_info-bukrs.
      else.
        <tabfield> = gt_org_info-rcomp.
      endif.
      glu1-rldnr = gs_881-rldnr.

*     gf_record_key = glu1."Remove this this line if unicode check activ
      field-symbols: <f_rec_key1> LIKE gf_record_key.
      field-symbols: <f_rec_key2> TYPE c.

      ASSIGN GLU1 TO <f_rec_key2> CASTING.
      ASSIGN gf_record_key TO <f_rec_key1> CASTING.
      <f_rec_key1> = <f_rec_key2>.
**
    endif.

*  Start Runtime Analyzes
    PERFORM RUNTIME_START USING cue_800d-feld.
* Depending on Release different CALL
    IF syst-saprl(2) < '47'.
*1) G_READ_AREA
      CALL FUNCTION 'G_READ_AREA'
        EXPORTING
          CHECK_TYP   = '1'
          FIELD       = cue_800d-feld
          RECORD_KEY  = gf_record_key
          T800D_TABLE = cue_800d-tab
          TABLE       = 'GLU1'
          TEXT_FLAG   = ' '
          VALUE_FROM  = gf_value_from
          VALUE_TO    = gf_value_to
        TABLES
          IGMD00      = gt_igmd00
          IGMDTT      = gt_igmdtt
          PERIOD      = gt_periods
        EXCEPTIONS
          NOT_FOUND   = 1
          OTHERS      = 2.
    ELSE.
      CALL FUNCTION 'G_READ_AREA'
        EXPORTING
          CHECK_TYP   = '1'
          FIELD       = cue_800d-feld
          RECORD_KEY  = GLU1
          T800D_TABLE = cue_800d-tab
          TABLE       = 'GLU1'
          TEXT_FLAG   = ' '
          VALUE_FROM  = gf_value_from
          VALUE_TO    = gf_value_to
        TABLES
          IGMD00      = gt_igmd00
          IGMDTT      = gt_igmdtt
          PERIOD      = gt_periods
        EXCEPTIONS
          NOT_FOUND   = 1
          OTHERS      = 2.
    ENDIF.
    IF SY-SUBRC = 0.
      ld_area_exit_works = 'X'.
      read table gt_igmd00 index 1.
      IF syst-saprl(2) < '47'.
        CALL FUNCTION 'G_READ_VALUE'
          EXPORTING
            CHECK_TYP   = '1'
            FIELD       = cue_800d-feld
            RECORD_KEY  = gf_record_key
            T800D_TABLE = cue_800d-tab
            TABLE       = 'GLU1'
            TEXT_FLAG   = ' '
            VALUE       = gt_igmd00-value
          TABLES
            PERIOD      = gt_periods
          EXCEPTIONS
            NOT_FOUND   = 1
            OTHERS      = 2.
      ELSE.
        CALL FUNCTION 'G_READ_VALUE'
          EXPORTING
            CHECK_TYP   = '1'
            FIELD       = cue_800d-feld
            RECORD_KEY  = GLU1
            T800D_TABLE = cue_800d-tab
            TABLE       = 'GLU1'
            TEXT_FLAG   = ' '
            VALUE       = gt_igmd00-value
          TABLES
            PERIOD      = gt_periods
          EXCEPTIONS
            NOT_FOUND   = 1
            OTHERS      = 2.

      ENDIF.
      IF SY-SUBRC = 0.
        IF syst-saprl(2) < '47'.
*3) G_READ_TEXT
          CALL FUNCTION 'G_READ_TEXT'
            EXPORTING
              FIELD       = cue_800d-feld
              RECORD_KEY  = gf_record_key
              T800D_TABLE = cue_800d-tab
              TABLE       = 'GLU1'
              VALUE       = gt_igmd00-value
            IMPORTING
              LTEXT       = gf_ltext
              STEXT       = gf_stext
            EXCEPTIONS
              NOT_FOUND   = 1
              OTHERS      = 2.
        ELSE.
          CALL FUNCTION 'G_READ_TEXT'
            EXPORTING
              FIELD       = cue_800d-feld
              RECORD_KEY  = GLU1
              T800D_TABLE = cue_800d-tab
              TABLE       = 'GLU1'
              VALUE       = gt_igmd00-value
            IMPORTING
              LTEXT       = gf_ltext
              STEXT       = gf_stext
            EXCEPTIONS
              NOT_FOUND   = 1
              OTHERS      = 2.

        ENDIF.

        IF SY-SUBRC = 0.
          PERFORM RUNTIME_STOP USING cue_800d-feld.
          IF GD_RUNTIME-DURATION > 120.
* Performance low
            gt_t800d_msg-line = zttext12.
            replace '&' into gt_t800d_msg-line with: <gf_field>.
            gt_t800d_msg-type = 'W'.
            append gt_t800d_msg.
          ENDIF.
*Sucess message: Exits work correctly
          gt_t800d_msg-line = zttext10.
          replace '&' into gt_t800d_msg-line with:
          <gf_field>.
          gt_t800d_msg-type = 'I'.
          append gt_t800d_msg.
          exit.
        else.
*g_read_text not ok
          gt_t800d_msg-line = zttext03.
          replace '&' into gt_t800d_msg-line with:
          <gf_field>.
          gt_t800d_msg-type = 'E'.
          append gt_t800d_msg.
          exit.
        ENDIF.
      else.
*g_read_value not ok
        gt_t800d_msg-line = zttext04.
        replace '&' into gt_t800d_msg-line with:
        <gf_field>.
        gt_t800d_msg-type = 'E'.
        append gt_t800d_msg.
        exit.
      ENDIF.
    endif.
  endloop.
*g_read_area not ok
  if ld_area_exit_works is initial.
    gt_t800d_msg-line = zttext05.
    replace '&' into gt_t800d_msg-line with:
    <gf_field>.
    gt_t800d_msg-type = 'E'.
    append gt_t800d_msg.
    exit.
  ENDIF.


ENDFORM.                    " call_user_exits


*-----------------------------------------------------------------
*&---------------------------------------------------------------------*
*&      Form  check_fldtab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_fldtab.
  DATA: lt_t888M TYPE TABLE of T888M.
  DATA: ls_t888M TYPE T888M.
*
  DATA: lt_t888 TYPE TABLE of T888.
  DATA: ls_t888 TYPE T888.
*
  DATA: lt_t800d TYPE TABLE of T800d.
  DATA: ls_t800d TYPE T800d.
*
  DATA lt_t800a LIKE T800a occurs 0.
  DATA ls_t800a LIKE T800a.
*
  DATA lt_t000 LIKE T000 occurs 0.
  DATA ls_t000 LIKE T000.
*
  DATA: BEGIN OF lt_client occurs 10,
          client LIKE T000-MANDT,
          custab(5) TYPE c,
          exist(1) TYPE c,
        END OF lt_client.
  DATA  LS_CLIENT LIKE  lt_client.
*
  DATA: ld_exist(1) TYPE c.
  DATA: ld_entry_err(1) TYPE c.
  DATA: ld_next(1) TYPE c.
*
  DATA: ld_delet(3) TYPE c.

  CLEAR ld_delet.

* Fill client table
  SELECT * FROM T000 INTO TABLE lt_t000.

* Fill Table Directory
  SELECT * FROM T800A INTO TABLE lt_t800a
      WHERE TTYPE = 'TT'.

* Fill Header variable field movement
  SELECT * FROM T888 CLIENT SPECIFIED INTO TABLE lt_t888
           WHERE CLASS = 'A'.

*Check variable field movement
  LOOP AT lt_t888 INTO LS_T888.
    SELECT * FROM T888M CLIENT SPECIFIED APPENDING TABLE lt_t888M
             WHERE FELDMODIF = LS_T888-FELDMODIF.
  ENDLOOP.

  LOOP AT lt_t888M INTO ls_t888m.
    CHECK NOT ls_t888m-fromtable IS INITIAL.

    CLEAR: gt_fldchk_msg.

* CHECK FROM-Fields
    CALL FUNCTION 'G_FIELD_READ'
      EXPORTING
        TABLE     = ls_t888m-fromtable
        FIELDNAME = ls_t888m-fromfield
      EXCEPTIONS
        NOT_FOUND = 1
        OTHERS    = 2.

    IF sy-subrc ne 0.
      gt_fldchk_msg-type    = 'W'.
      gt_fldchk_msg-client    = ls_t888m-mandt.
      gt_fldchk_msg-feldmodif = ls_t888m-feldmodif.
      gt_fldchk_msg-tcode     = 'GCF2'.
      gt_fldchk_msg-custab    = 'T888M'.
      gt_fldchk_msg-issue1    = 'VAR'.
      gt_fldchk_msg-line   = 'Field $ does not exist in table $'.
      REPLACE '$' INTO gt_fldchk_msg-line WITH ls_t888m-fromfield.
      CONDENSE gt_fldchk_msg-line.
      REPLACE '$' INTO gt_fldchk_msg-line WITH ls_t888m-fromtable.
      CONDENSE gt_fldchk_msg-line.
      gt_fldchk_msg-sltab = ls_t888m-fromtable.
      gt_fldchk_msg-field  = ls_t888m-fromfield.
*     Check if acitiv
      READ TABLE lt_t800a INTO ls_t800a
            WITH KEY TAB = ls_t888-totable.
      IF sy-subrc = 0.
        IF ls_t800a-inactive IS INITIAL.
          APPEND gt_fldchk_msg.
        ENDIF.
      ENDIF.
      CLEAR: gt_fldchk_msg.
    ENDIF.

* CHECK TO-Fields
    READ TABLE lt_t888 INTO ls_t888
          WITH KEY mandt     = ls_t888m-mandt
                   feldmodif = ls_t888m-feldmodif.
    IF sy-subrc <> 0.
      gt_fldchk_msg-type    = 'W'.
      gt_fldchk_msg-client    = ls_t888m-mandt.
      gt_fldchk_msg-feldmodif = ls_t888m-feldmodif.
      gt_fldchk_msg-tcode     = 'GCF2'.
      gt_fldchk_msg-custab    = 'T888M'.
      gt_fldchk_msg-issue1    = 'VAR'.
      gt_fldchk_msg-line   = 'Unexpected Error in T888/T888M'.
      gt_fldchk_msg-sltab = ls_t888m-fromtable.
      gt_fldchk_msg-field  = ls_t888m-fromfield.
      APPEND gt_fldchk_msg.
      CLEAR: gt_fldchk_msg.
      EXIT.
    ENDIF.

    CLEAR ls_t800a.
    CLEAR ld_exist.
    READ TABLE lt_t800a INTO ls_t800a
      WITH KEY TAB = ls_t888-totable.

* Check summary table
    IF NOT ls_t800a-tab IS INITIAL AND
       ls_t888-totable ne 'GLU1'.

      CALL FUNCTION 'G_FIELD_READ'
        EXPORTING
          TABLE     = ls_t800a-tab
          FIELDNAME = ls_t888m-tofield
        EXCEPTIONS
          NOT_FOUND = 1
          OTHERS    = 2.

      IF sy-subrc = 0.
        ld_exist = 'X'.
      ENDIF.
    ENDIF.

* Check line item table
    IF NOT ls_t800a-ntable IS INITIAL AND
           ls_t888-totable ne 'GLU1'.

      CALL FUNCTION 'G_FIELD_READ'
        EXPORTING
          TABLE     = ls_t800a-ntable
          FIELDNAME = ls_t888m-tofield
        EXCEPTIONS
          NOT_FOUND = 1
          OTHERS    = 2.

      IF sy-subrc = 0.
        ld_exist = 'X'.
      ENDIF.
    ENDIF.


    IF ld_exist IS INITIAL AND
        ls_t888-totable ne 'GLU1'.
      gt_fldchk_msg-type    = 'I'.
      gt_fldchk_msg-client    = ls_t888m-mandt.
      gt_fldchk_msg-feldmodif = ls_t888m-feldmodif.
      gt_fldchk_msg-tcode     = 'GCF2'.
      gt_fldchk_msg-custab    = 'T888M'.
      gt_fldchk_msg-issue1    = 'VAR'.
      gt_fldchk_msg-line   = 'Field $ exists not in table $ or $'.
      REPLACE '$' INTO gt_fldchk_msg-line WITH ls_t888m-tofield.
      CONDENSE gt_fldchk_msg-line.
      REPLACE '$' INTO gt_fldchk_msg-line WITH ls_t800a-tab.
      CONDENSE gt_fldchk_msg-line.
      REPLACE '$' INTO gt_fldchk_msg-line WITH ls_t800a-ntable.
      CONDENSE gt_fldchk_msg-line.
      gt_fldchk_msg-sltab = ls_t888-TOTABLE.
      gt_fldchk_msg-field  = ls_t888m-tofield.
*     Find Component for TOTABLE
      READ TABLE lt_t800a INTO ls_t800a
            WITH KEY TAB = ls_t888-totable.
      IF sy-subrc = 0.
        CONCATENATE ls_t800a-appl '-' ls_t800a-subappl
               INTO gt_fldchk_msg-component.
        IF ls_t800a-inactive IS INITIAL.
          APPEND gt_fldchk_msg.
          CLEAR: gt_fldchk_msg.
        ENDIF.
      ELSE.
        gt_fldchk_msg-component = '? ? ?'.
      ENDIF.
      CLEAR: gt_fldchk_msg.
    ENDIF.

* CHECK GLU1-Fields
    IF ls_t888-TOTABLE <> 'GLU1'.
      CALL FUNCTION 'G_FIELD_READ'
        EXPORTING
          TABLE     = 'GLU1'
          FIELDNAME = ls_t888m-tofield
        EXCEPTIONS
          NOT_FOUND = 1
          OTHERS    = 2.
      IF sy-subrc ne 0.
        gt_fldchk_msg-type    = 'W'.
        gt_fldchk_msg-client    = ls_t888m-mandt.
        gt_fldchk_msg-feldmodif = ls_t888m-feldmodif.
        gt_fldchk_msg-tcode     = 'GCF2'.
        gt_fldchk_msg-custab    = 'T888M'.
        gt_fldchk_msg-issue1    = 'VAR'.
        gt_fldchk_msg-line   = 'Field $ does not exist in GLU1'.
        REPLACE '$' INTO gt_fldchk_msg-line WITH ls_t888m-tofield.
        CONDENSE gt_fldchk_msg-line.
        gt_fldchk_msg-sltab = ls_t888-totable.
        gt_fldchk_msg-field  = ls_t888m-tofield.
*     Find Component for TOTABLE
        READ TABLE lt_t800a INTO ls_t800a
              WITH KEY TAB = ls_t888-totable.
        IF sy-subrc = 0.
          CONCATENATE ls_t800a-appl '-' ls_t800a-subappl
                 INTO gt_fldchk_msg-component.
          IF ls_t800a-inactive IS INITIAL.
            APPEND gt_fldchk_msg.
            CLEAR: gt_fldchk_msg.
          ENDIF.
        ELSE.
          gt_fldchk_msg-component = '? ? ?'.
        ENDIF.
        CLEAR: gt_fldchk_msg.
      ENDIF.
    ENDIF.
* Fill table for client check
    CLEAR lt_client.
    lt_client-client = ls_t888m-mandt.
    lt_client-custab = 'T888M'.
    COLLECT lt_client.
  ENDLOOP.
  FREE: lt_t888M, lt_t888.

***********************************
*Check master data entries T800D  *
***********************************
  SELECT * FROM T800D CLIENT SPECIFIED INTO TABLE lt_t800D.
  LOOP AT lt_t800D INTO ls_t800D.

    CLEAR ld_next.

* Fill table for client check
    CLEAR lt_client.
    lt_client-client = ls_t800D-mandt.
    lt_client-custab = 'T800D'.
    COLLECT lt_client.

    CLEAR ls_t800a.
    READ TABLE lt_t800a INTO ls_t800a
      WITH KEY TAB = ls_t800d-tab.

    IF sy-subrc = 0.
      IF NOT ls_t800a-inactive IS INITIAL.
        ld_next = 'X'.
      ENDIF.
    ELSE.
      ld_next = 'X'.
    ENDIF.


    IF ( NOT ls_t800D-VALUE_TAB IS INITIAL ) OR
       ( NOT ls_t800D-TEXT_TAB  IS INITIAL ).
      CLEAR ld_next.
    ENDIF.

* Next when Valtabs not filled
    IF NOT ld_next IS INITIAL.
      CONTINUE.
    ENDIF.


    CLEAR gt_fldchk_msg.
    CALL FUNCTION 'G_FIELD_READ'
      EXPORTING
        TABLE     = ls_t800d-tab
        FIELDNAME = ls_t800d-feld
      EXCEPTIONS
        NOT_FOUND = 1
        OTHERS    = 2.

    IF sy-subrc ne 0.
      gt_fldchk_msg-type    = 'I'.
      gt_fldchk_msg-client    = ls_t800d-mandt.
      gt_fldchk_msg-tcode     = 'GCS1'.
      gt_fldchk_msg-custab    = 'T800D'.
      gt_fldchk_msg-issue1    = 'MAST'.
      gt_fldchk_msg-issue2    = 'T800D-FELD'.

      gt_fldchk_msg-line   = 'Field $ does not exist in table $'.
      REPLACE '$' INTO gt_fldchk_msg-line WITH ls_t800d-feld.
      CONDENSE gt_fldchk_msg-line.
      REPLACE '$' INTO gt_fldchk_msg-line WITH ls_t800d-tab.
      CONDENSE gt_fldchk_msg-line.
      gt_fldchk_msg-sltab = ls_t800d-tab.
      gt_fldchk_msg-field  = ls_t800d-feld.
*     Find Component for T800D-TAB
      READ TABLE lt_t800a INTO ls_t800a
            WITH KEY TAB = ls_t800d-tab.
      IF sy-subrc = 0.
        CONCATENATE ls_t800a-appl '-' ls_t800a-subappl
               INTO gt_fldchk_msg-component.
      ELSE.
        gt_fldchk_msg-component = '? ? ?'.
        gt_fldchk_msg-type    = 'W'.
      ENDIF.
      APPEND gt_fldchk_msg.
      CLEAR: gt_fldchk_msg.
    ENDIF.

* Check Superior field 1
    IF NOT ls_t800d-rfield1 IS INITIAL.
      CALL FUNCTION 'G_FIELD_READ'
        EXPORTING
          TABLE     = ls_t800d-tab
          FIELDNAME = ls_t800d-rfield1
        EXCEPTIONS
          NOT_FOUND = 1
          OTHERS    = 2.
      IF sy-subrc ne 0.
        gt_fldchk_msg-type    = 'I'.
        gt_fldchk_msg-client    = ls_t800d-mandt.
        gt_fldchk_msg-tcode     = 'GCS1'.
        gt_fldchk_msg-custab    = 'T800D'.
        gt_fldchk_msg-issue1    = 'MAST'.
        gt_fldchk_msg-issue2    = 'T800D-FELD'.

        gt_fldchk_msg-line   = 'Superior Field1 $ exists not in table $'
.
        REPLACE '$' INTO gt_fldchk_msg-line WITH ls_t800d-rfield1.
        CONDENSE gt_fldchk_msg-line.
        REPLACE '$' INTO gt_fldchk_msg-line WITH ls_t800d-tab.
        CONDENSE gt_fldchk_msg-line.
        gt_fldchk_msg-sltab  = ls_t800d-tab.
        gt_fldchk_msg-field  = ls_t800d-feld.
*     Find Component for T800D-TAB
        READ TABLE lt_t800a INTO ls_t800a
              WITH KEY TAB = ls_t800d-tab.
        IF sy-subrc = 0.
          CONCATENATE ls_t800a-appl '-' ls_t800a-subappl
                 INTO gt_fldchk_msg-component.
        ELSE.
          gt_fldchk_msg-component = '? ? ?'.
          gt_fldchk_msg-type    = 'W'.
        ENDIF.
        APPEND gt_fldchk_msg.
        CLEAR: gt_fldchk_msg.
      ENDIF.
    ENDIF.
* Check Superior field 2
    IF NOT ls_t800d-rfield2 IS INITIAL.
      CALL FUNCTION 'G_FIELD_READ'
        EXPORTING
          TABLE     = ls_t800d-tab
          FIELDNAME = ls_t800d-rfield2
        EXCEPTIONS
          NOT_FOUND = 1
          OTHERS    = 2.
      IF sy-subrc ne 0.
        gt_fldchk_msg-type    = 'I'.
        gt_fldchk_msg-client    = ls_t800d-mandt.
        gt_fldchk_msg-tcode     = 'GCS1'.
        gt_fldchk_msg-custab    = 'T800D'.
        gt_fldchk_msg-issue1    = 'MAST'.
        gt_fldchk_msg-issue2    = 'T800D-FELD'.

        gt_fldchk_msg-line   = 'Superior Field2 $ exists not in table $'
.
        REPLACE '$' INTO gt_fldchk_msg-line WITH ls_t800d-rfield2.
        CONDENSE gt_fldchk_msg-line.
        REPLACE '$' INTO gt_fldchk_msg-line WITH ls_t800d-tab.
        CONDENSE gt_fldchk_msg-line.
        gt_fldchk_msg-sltab  = ls_t800d-tab.
        gt_fldchk_msg-field  = ls_t800d-feld.
*     Find Component for T800D-TAB
        READ TABLE lt_t800a INTO ls_t800a
              WITH KEY TAB = ls_t800d-tab.
        IF sy-subrc = 0.
          CONCATENATE ls_t800a-appl '-' ls_t800a-subappl
                 INTO gt_fldchk_msg-component.
        ELSE.
          gt_fldchk_msg-component = '? ? ?'.
          gt_fldchk_msg-type    = 'W'.
        ENDIF.
        APPEND gt_fldchk_msg.
        CLEAR: gt_fldchk_msg.
      ENDIF.
    ENDIF.

* Check Superior field 3
    IF NOT ls_t800d-rfield3 IS INITIAL.
      CALL FUNCTION 'G_FIELD_READ'
        EXPORTING
          TABLE     = ls_t800d-tab
          FIELDNAME = ls_t800d-rfield3
        EXCEPTIONS
          NOT_FOUND = 1
          OTHERS    = 2.
      IF sy-subrc ne 0.
        gt_fldchk_msg-type    = 'I'.
        gt_fldchk_msg-client    = ls_t800d-mandt.
        gt_fldchk_msg-tcode     = 'GCS1'.
        gt_fldchk_msg-custab    = 'T800D'.
        gt_fldchk_msg-issue1    = 'MAST'.
        gt_fldchk_msg-issue2    = 'T800D-FELD'.

        gt_fldchk_msg-line   = 'Superior Field3 $ exists not in table $'
.
        REPLACE '$' INTO gt_fldchk_msg-line WITH ls_t800d-rfield3.
        CONDENSE gt_fldchk_msg-line.
        REPLACE '$' INTO gt_fldchk_msg-line WITH ls_t800d-tab.
        CONDENSE gt_fldchk_msg-line.
        gt_fldchk_msg-sltab  = ls_t800d-tab.
        gt_fldchk_msg-field  = ls_t800d-feld.
*     Find Component for T800D-TAB
        READ TABLE lt_t800a INTO ls_t800a
              WITH KEY TAB = ls_t800d-tab.
        IF sy-subrc = 0.
          CONCATENATE ls_t800a-appl '-' ls_t800a-subappl
                 INTO gt_fldchk_msg-component.
        ELSE.
          gt_fldchk_msg-component = '? ? ?'.
          gt_fldchk_msg-type    = 'W'.
        ENDIF.
        APPEND gt_fldchk_msg.
        CLEAR: gt_fldchk_msg.
      ENDIF.
    ENDIF.

* Check Superior field 4
    IF NOT ls_t800d-rfield4 IS INITIAL.
      CALL FUNCTION 'G_FIELD_READ'
        EXPORTING
          TABLE     = ls_t800d-tab
          FIELDNAME = ls_t800d-rfield4
        EXCEPTIONS
          NOT_FOUND = 1
          OTHERS    = 2.
      IF sy-subrc ne 0.
        gt_fldchk_msg-type    = 'I'.
        gt_fldchk_msg-client    = ls_t800d-mandt.
        gt_fldchk_msg-tcode     = 'GCS1'.
        gt_fldchk_msg-custab    = 'T800D'.
        gt_fldchk_msg-issue1    = 'MAST'.
        gt_fldchk_msg-issue2    = 'T800D-FELD'.

        gt_fldchk_msg-line   = 'Superior Field4 $ exists not in table $'
.
        REPLACE '$' INTO gt_fldchk_msg-line WITH ls_t800d-rfield4.
        CONDENSE gt_fldchk_msg-line.
        REPLACE '$' INTO gt_fldchk_msg-line WITH ls_t800d-tab.
        CONDENSE gt_fldchk_msg-line.
        gt_fldchk_msg-sltab  = ls_t800d-tab.
        gt_fldchk_msg-field  = ls_t800d-feld.
*     Find Component for T800D-TAB
        READ TABLE lt_t800a INTO ls_t800a
              WITH KEY TAB = ls_t800d-tab.
        IF sy-subrc = 0.
          CONCATENATE ls_t800a-appl '-' ls_t800a-subappl
                 INTO gt_fldchk_msg-component.
        ELSE.
          gt_fldchk_msg-component = '? ? ?'.
          gt_fldchk_msg-type    = 'W'.
        ENDIF.
        APPEND gt_fldchk_msg.
        CLEAR: gt_fldchk_msg.
      ENDIF.
    ENDIF.
* Check Value Tables

* Value Table
    IF NOT ls_t800d-VALUE_TAB IS INITIAL.

      CALL FUNCTION 'INTERN_DD_TABL_TYPE'
        EXPORTING
          OBJNAME              = ls_t800d-VALUE_TAB
        EXCEPTIONS
          OBJECT_NOT_FOUND     = 1
          OBJECT_NOT_SPECIFIED = 2
          OTHERS               = 3.
      IF SY-SUBRC <> 0.
        gt_fldchk_msg-type    = 'E'.
        gt_fldchk_msg-client    = ls_t800d-mandt.
        gt_fldchk_msg-tcode     = 'GCS1'.
        gt_fldchk_msg-custab    = 'T800D'.
        gt_fldchk_msg-issue1    = 'MAST'.
        gt_fldchk_msg-issue2    = 'T800D-VALUE_TAB'.

        gt_fldchk_msg-line   = 'Table $ does not exist(SE11)'.
        REPLACE '$' INTO gt_fldchk_msg-line WITH ls_t800d-VALUE_TAB.
        CONDENSE gt_fldchk_msg-line.
        gt_fldchk_msg-sltab  = ls_t800d-tab.
        gt_fldchk_msg-field  = ls_t800d-feld.
*     Find Component for T800D-TAB
        READ TABLE lt_t800a INTO ls_t800a
              WITH KEY TAB = ls_t800d-tab.
        IF sy-subrc = 0.
          CONCATENATE ls_t800a-appl '-' ls_t800a-subappl
                 INTO gt_fldchk_msg-component.
        ELSE.
          gt_fldchk_msg-component = '? ? ?'.
          gt_fldchk_msg-type    = 'W'.
        ENDIF.
        APPEND gt_fldchk_msg.
      ELSE.
        CALL FUNCTION 'G_FIELD_READ'
          EXPORTING
            TABLE     = ls_t800d-VALUE_TAB
            FIELDNAME = ls_t800d-VALUE_FIELD
          EXCEPTIONS
            NOT_FOUND = 1
            OTHERS    = 2.
        IF sy-subrc ne 0.
          gt_fldchk_msg-type    = 'E'.
          gt_fldchk_msg-client    = ls_t800d-mandt.
          gt_fldchk_msg-tcode     = 'GCS1'.
          gt_fldchk_msg-custab    = 'T800D'.
          gt_fldchk_msg-issue1    = 'MAST'.
          gt_fldchk_msg-issue2    = 'T800D-VALUE_FIELD'.

          gt_fldchk_msg-line   = 'Field $ exists not in table $'.
          REPLACE '$' INTO gt_fldchk_msg-line WITH ls_t800d-VALUE_FIELD.
          CONDENSE gt_fldchk_msg-line.
          REPLACE '$' INTO gt_fldchk_msg-line WITH ls_t800d-VALUE_TAB.
          CONDENSE gt_fldchk_msg-line.
          gt_fldchk_msg-sltab  = ls_t800d-tab.
          gt_fldchk_msg-field  = ls_t800d-feld.
*     Find Component for T800D-TAB
          READ TABLE lt_t800a INTO ls_t800a
                WITH KEY TAB = ls_t800d-tab.
          IF sy-subrc = 0.
            CONCATENATE ls_t800a-appl '-' ls_t800a-subappl
                   INTO gt_fldchk_msg-component.
          ELSE.
            gt_fldchk_msg-component = '? ? ?'.
            gt_fldchk_msg-type    = 'W'.
          ENDIF.
          APPEND gt_fldchk_msg.
        ENDIF.
      ENDIF.
    ENDIF.

* Text Table
    IF NOT ls_t800d-TEXT_TAB IS INITIAL.
      CALL FUNCTION 'INTERN_DD_TABL_TYPE'
        EXPORTING
          OBJNAME              = ls_t800d-TEXT_TAB
        EXCEPTIONS
          OBJECT_NOT_FOUND     = 1
          OBJECT_NOT_SPECIFIED = 2
          OTHERS               = 3.
      IF SY-SUBRC <> 0.
        gt_fldchk_msg-type    = 'E'.
        gt_fldchk_msg-client    = ls_t800d-mandt.
        gt_fldchk_msg-tcode     = 'GCS1'.
        gt_fldchk_msg-custab    = 'T800D'.
        gt_fldchk_msg-issue1    = 'MAST'.
        gt_fldchk_msg-issue2    = 'T800D-TEXT_TAB'.

        gt_fldchk_msg-line   = 'Text-Table $ does not exist(SE11)'.
        REPLACE '$' INTO gt_fldchk_msg-line WITH ls_t800d-TEXT_TAB.
        CONDENSE gt_fldchk_msg-line.
        gt_fldchk_msg-sltab  = ls_t800d-tab.
        gt_fldchk_msg-field  = ls_t800d-feld.
*     Find Component for T800D-TAB
        READ TABLE lt_t800a INTO ls_t800a
              WITH KEY TAB = ls_t800d-tab.
        IF sy-subrc = 0.
          CONCATENATE ls_t800a-appl '-' ls_t800a-subappl
                 INTO gt_fldchk_msg-component.
        ELSE.
          gt_fldchk_msg-component = '? ? ?'.
          gt_fldchk_msg-type    = 'W'.
        ENDIF.
        APPEND gt_fldchk_msg.
      ELSE.
* Check Text field short
        IF NOT ls_t800d-TEXT_FIELD_S IS INITIAL.
          CALL FUNCTION 'G_FIELD_READ'
            EXPORTING
              TABLE     = ls_t800d-TEXT_TAB
              FIELDNAME = ls_t800d-TEXT_FIELD_S
            EXCEPTIONS
              NOT_FOUND = 1
              OTHERS    = 2.
          IF sy-subrc ne 0.
            gt_fldchk_msg-type    = 'E'.
            gt_fldchk_msg-client    = ls_t800d-mandt.
            gt_fldchk_msg-tcode     = 'GCS1'.
            gt_fldchk_msg-custab    = 'T800D'.
            gt_fldchk_msg-issue1    = 'MAST'.
            gt_fldchk_msg-issue2    = 'T800D-TEXT_FIELD_S'.

            gt_fldchk_msg-line   = 'Field $ exists not in table $'.
            REPLACE '$' INTO gt_fldchk_msg-line WITH
ls_t800d-TEXT_FIELD_S.
            CONDENSE gt_fldchk_msg-line.
            REPLACE '$' INTO gt_fldchk_msg-line WITH ls_t800d-TEXT_TAB.
            CONDENSE gt_fldchk_msg-line.
            gt_fldchk_msg-sltab  = ls_t800d-tab.
            gt_fldchk_msg-field  = ls_t800d-feld.
*     Find Component for T800D-TAB
            READ TABLE lt_t800a INTO ls_t800a
                  WITH KEY TAB = ls_t800d-tab.
            IF sy-subrc = 0.
              CONCATENATE ls_t800a-appl '-' ls_t800a-subappl
                     INTO gt_fldchk_msg-component.
            ELSE.
              gt_fldchk_msg-component = '? ? ?'.
            ENDIF.
            APPEND gt_fldchk_msg.
          ENDIF.
        ENDIF.
* Check Text field long
        IF NOT ls_t800d-TEXT_FIELD_L IS INITIAL.
          CALL FUNCTION 'G_FIELD_READ'
            EXPORTING
              TABLE     = ls_t800d-TEXT_TAB
              FIELDNAME = ls_t800d-TEXT_FIELD_L
            EXCEPTIONS
              NOT_FOUND = 1
              OTHERS    = 2.
          IF sy-subrc ne 0.
            gt_fldchk_msg-type    = 'E'.
            gt_fldchk_msg-client    = ls_t800d-mandt.
            gt_fldchk_msg-tcode     = 'GCS1'.
            gt_fldchk_msg-custab    = 'T800D'.
            gt_fldchk_msg-issue1    = 'MAST'.
            gt_fldchk_msg-issue2    = 'T800D-TEXT_FIELD_L'.

            gt_fldchk_msg-line   = 'Field $ exists not in table $'.
            REPLACE '$' INTO gt_fldchk_msg-line WITH
ls_t800d-TEXT_FIELD_L.
            CONDENSE gt_fldchk_msg-line.
            REPLACE '$' INTO gt_fldchk_msg-line WITH ls_t800d-TEXT_TAB.
            CONDENSE gt_fldchk_msg-line.
            gt_fldchk_msg-sltab  = ls_t800d-tab.
            gt_fldchk_msg-field  = ls_t800d-feld.
*     Find Component for T800D-TAB
            READ TABLE lt_t800a INTO ls_t800a
                  WITH KEY TAB = ls_t800d-tab.
            IF sy-subrc = 0.
              CONCATENATE ls_t800a-appl '-' ls_t800a-subappl
                     INTO gt_fldchk_msg-component.
            ELSE.
              gt_fldchk_msg-component = '? ? ?'.
            ENDIF.
            APPEND gt_fldchk_msg.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

* Client Check
  LOOP AT lt_client INTO ls_client.
* Client exist ?
    READ TABLE lt_t000 INTO ls_t000
      WITH KEY mandt = ls_client-client.
    IF sy-subrc = 0.
      ls_client-exist = 'X'.
    ELSE.
      CLEAR ls_client-exist.
    ENDIF.
    MODIFY lt_client FROM ls_client.
  ENDLOOP.
  SORT lt_client BY custab client.

  LOOP AT lt_client INTO ls_client.
    IF ls_client-exist IS INITIAL.
      CLEAR gt_fldchk_msg.
      gt_fldchk_msg-type    = 'E'.
      gt_fldchk_msg-client    = ls_client-client.
      gt_fldchk_msg-issue1    = 'CLNT'.
      gt_fldchk_msg-custab    = ls_client-custab.
      gt_fldchk_msg-line = 'For Client $ entries in table $ exist.'.
      CONCATENATE gt_fldchk_msg-line 'Client $ does not exist!'
                  INTO gt_fldchk_msg-line.
      REPLACE '$' INTO gt_fldchk_msg-line WITH ls_client-client.
      CONDENSE gt_fldchk_msg-line.
      REPLACE '$' INTO gt_fldchk_msg-line WITH ls_client-custab.
      CONDENSE gt_fldchk_msg-line.
      REPLACE '$' INTO gt_fldchk_msg-line WITH ls_client-client.
      APPEND gt_fldchk_msg.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " check_fldtab

*---------------------------------------------------------------------*
*       FORM RUNTIME_START                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  U_EVENT                                                       *
*---------------------------------------------------------------------*
FORM RUNTIME_START using u_event TYPE FIELDNAME.
  CLEAR GD_RUNTIME.
  get time.
  GD_RUNTIME-EVENT = u_event.
  GD_RUNTIME-START_TIME = SY-TIMLO.
  GD_RUNTIME-START_DATE = SY-DATLO.
ENDFORM.                    "RUNTIME_START

*---------------------------------------------------------------------*
*       FORM RUNTIME_END                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  U_EVENT                                                       *
*---------------------------------------------------------------------*
FORM RUNTIME_STOP using u_event TYPE FIELDNAME.
  IF u_event ne GD_RUNTIME-EVENT OR
     u_event IS INITIAL.
    MESSAGE I016 WITH 'Wrong Event: Run Time ANALYZES'.
    EXIT.
  ENDIF.
  get time.
  GD_RUNTIME-EVENT = u_event.
  GD_RUNTIME-END_TIME = SY-TIMLO.
  GD_RUNTIME-END_DATE = SY-DATLO.
  CALL FUNCTION 'SWI_DURATION_DETERMINE'
    EXPORTING
      START_DATE = GD_RUNTIME-START_DATE
      END_DATE   = GD_RUNTIME-END_DATE
      START_TIME = GD_RUNTIME-START_TIME
      END_TIME   = GD_RUNTIME-END_TIME
    IMPORTING
      DURATION   = GD_RUNTIME-DURATION.

ENDFORM.                    "RUNTIME_STOP

*&---------------------------------------------------------------------*
*&      Form  SHOW_PROGRESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PROCESS_TXT  text
*----------------------------------------------------------------------*
FORM SHOW_PROGRESS USING process_txt LIKE zatext01 .

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      TEXT = process_txt.

  MESSAGE S016 WITH process_txt '' '' '' .

ENDFORM.                    "SHOW_PRGRESS



*&---------------------------------------------------------------------*
*&      Form  GET_TABLE_LENGHTH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TABLE    text
*      -->P_LENGTH   text
*----------------------------------------------------------------------*
FORM GET_TABLE_LENGHTH USING    p_table TYPE T800A-TAB
                       CHANGING p_length TYPE i.
  DATA LT_DFIES_TAB LIKE DFIES OCCURS 0 .
  DATA LS_DFIES_TAB LIKE DFIES.
  DATA ld_len1 TYPE i.

  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      TABNAME        = p_table
      LANGU          = SY-LANGU
    TABLES
      DFIES_TAB      = LT_DFIES_TAB
    EXCEPTIONS
      NOT_FOUND      = 1
      INTERNAL_ERROR = 2
      OTHERS         = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CLEAR ld_len1.
  LOOP AT LT_DFIES_TAB INTO LS_DFIES_TAB.
    ld_len1 = ld_len1 + LS_DFIES_TAB-LENG.
  ENDLOOP.
  p_length = ld_len1.

ENDFORM.                    "GET_TABLE_LENGHTH


*&---------------------------------------------------------------------*
*&      Form  check_glidxa
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_H_ORG_NAME  text
*      -->P_Z_POOLCV  text
*      <--P_IT_GLIDXA[]  text
*----------------------------------------------------------------------*
FORM check_glidxa  USING    P_H_ORG_NAME
                            P_Z_POOLCV
                   CHANGING P_IT_GLIDXA LIKE it_glidxa[].

  DATA: lt_glidxa LIKE GLIDXA OCCURS 10.
  DATA: ls_glidxa LIKE GLIDXA.
  DATA: ld_clnt LIKE GLFUNCA-RCLNT.

  DATA: lt_litab  LIKE glidxa OCCURS 10.
  DATA: ls_litab1 LIKE glidxa.

  DATA: BEGIN OF lt_bkpfref OCCURS 10,
*        AWSYS LIKE  ACCHD-AWSYS,
         AWTYP LIKE  ACCHD-AWTYP,
         AWORG LIKE  ACCHD-AWORG,
         AWREF LIKE  ACCHD-AWREF.
  DATA: END Of lt_bkpfref.

  DATA: ls_bkpfref LIKE lt_bkpfref.

  DATA: ls_bkpf LIKE bkpf.

  DATA: ls_glidxadumy  LIKE it_glidxa.

DATA: BEGIN OF ls_litab2.
DATA:   REFDOCNR TYPE REFBELNR.
DATA:   RBUKRS   TYPE BUKRS.
DATA:   ROBUKRS  TYPE BUKRS.
DATA:   REFDOCCT TYPE REFDOCCT.
        INCLUDE STRUCTURE GLIDXA.
DATA: END OF ls_litab2.





  IF P_H_ORG_NAME = 'RBUKRS'.
* Read GLIDXA
    SELECT * FROM GLIDXA INTO TABLE lt_glidxa
      WHERE RLDNR = p_rldnr  AND
           RYEAR =  p_ryear  AND
           RRCTY =  p_rrcty  AND
           RVERS =  p_rvers  AND
           BUKRS =  p_rbukrs.

* Read Line Item Table
    SELECT *  FROM (T800a-NTABLE)
        INTO CORRESPONDING FIELDS OF  ls_litab2
             WHERE RLDNR  = p_rldnr  AND
                   RVERS  = p_rvers  AND
                   RRCTY  = p_rrcty  AND
                   RBUKRS = p_rbukrs AND
                   RYEAR  = p_ryear  AND
                   AWTYP  ne SPACE.

      MOVE-CORRESPONDING ls_litab2 TO ls_litab1.
      ls_litab1-awref = ls_litab2-refdocnr.
      ls_litab1-bukrs = ls_litab2-rbukrs.
      ls_litab1-docct = ls_litab2-refdocct.
      COLLECT ls_litab1 INTO lt_litab.
    ENDSELECT.

* Read BKPF
    SELECT * FROM BKPF INTO ls_bkpf
      WHERE GJAHR =  p_ryear  AND
            BUKRS =  p_rbukrs.
      ls_bkpfref-awtyp = ls_bkpf-awtyp.
      ls_bkpfref-awref = ls_bkpf-awkey+00(10).
      ls_bkpfref-aworg = ls_bkpf-awkey+10(10).
      COLLECT ls_bkpfref INTO lt_bkpfref.
    ENDSELECT.

  ENDIF.

  IF P_H_ORG_NAME = 'RCOMP'.
* Read GLIDXA
    SELECT * FROM GLIDXA INTO TABLE lt_glidxa
      WHERE RLDNR = p_rldnr  AND
           RYEAR =  p_ryear  AND
           RRCTY =  p_rrcty  AND
           RVERS =  p_rvers  AND
           RCOMP =  p_rcomp.
* Read Line Item Table
    SELECT *  FROM (T800a-NTABLE)  INTO CORRESPONDING FIELDS OF
ls_litab2
             WHERE RLDNR  = p_rldnr  AND
                   RVERS  = p_rvers  AND
                   RRCTY  = p_rrcty  AND
                   RCOMP  = p_rcomp  AND
                   RYEAR  = p_ryear  AND
                   AWTYP  ne SPACE.

      MOVE-CORRESPONDING ls_litab2 TO ls_litab1.
      ls_litab1-awref = ls_litab2-refdocnr.
      ls_litab1-rcomp = ls_litab2-rcomp.
      ls_litab1-bukrs = ls_litab2-robukrs.
      ls_litab1-docct = ls_litab2-refdocct.
      COLLECT ls_litab1 INTO lt_litab.
    ENDSELECT.
* Read BKPF
    SELECT * FROM BKPF INTO ls_bkpf
      WHERE GJAHR =  p_ryear  AND
            BUKRS IN  r_rbukrs.
      ls_bkpfref-awtyp = ls_bkpf-awtyp.
      ls_bkpfref-awref = ls_bkpf-awkey+00(10).
      ls_bkpfref-aworg = ls_bkpf-awkey+10(10).
      COLLECT ls_bkpfref INTO lt_bkpfref.
    ENDSELECT.
  ENDIF.


* Sort internal Tables
  SORT lt_litab.
  SORT lt_glidxa.
  SORT lt_bkpfref.

* Check; Entry in GLIDXA but not in Line Item Table
  LOOP AT lt_glidxa INTO ls_glidxa.
    READ TABLE lt_litab FROM ls_glidxa INTO ls_litab1.
    IF sy-subrc <> 0.
      CLEAR ls_glidxadumy.
      MOVE-CORRESPONDING ls_glidxa TO ls_glidxadumy.
      ls_glidxadumy-notin_litable = 'X'.
* Check if entry is in BKPF
      READ TABLE lt_bkpfref WITh KEY awtyp = ls_glidxadumy-awtyp
                                     awref = ls_glidxadumy-awref
                                     aworg = ls_glidxadumy-aworg
        INTO ls_bkpfref.
      IF sy-subrc <> 0.
        ls_glidxadumy-notin_bkpf = 'X'.
      ENDIF.
      APPEND ls_glidxadumy TO P_IT_GLIDXA.
    ENDIF.
  ENDLOOP.


* Check; Entry in Line Item Table but not in GLIDXA
  LOOP AT lt_litab INTO ls_litab1.
    READ TABLE lt_glidxa FROM ls_litab1 INTO ls_glidxa.
    IF sy-subrc <> 0.
      CLEAR ls_glidxadumy.
      MOVE-CORRESPONDING ls_litab1 TO ls_glidxadumy.
      ls_glidxadumy-notin_glidxa = 'X'.
* Check if entry is in BKPF
      READ TABLE lt_bkpfref WITh KEY awtyp = ls_glidxadumy-awtyp
                                     awref = ls_glidxadumy-awref
                                     aworg = ls_glidxadumy-aworg
        INTO ls_bkpfref.
      IF sy-subrc <> 0.
        ls_glidxadumy-notin_bkpf = 'X'.
      ENDIF.
      APPEND ls_glidxadumy TO P_IT_GLIDXA.
    ENDIF.
  ENDLOOP.

  FREE: lt_glidxa, lt_litab, lt_bkpfref.

ENDFORM.                    " check_glidxa
