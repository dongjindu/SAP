*----------------------------------------------------------------------*
*   INCLUDE ZACO38L_1TOP                                               *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*   Include Program
*----------------------------------------------------------------------*
* For Global Value in CO
INCLUDE ZLZGCO_GLOBAL_FORMTO1.


*----------------------------------------------------------------------*
*   Data Definition
*----------------------------------------------------------------------*
** type-pools
TYPE-POOLS: SLIS.
TYPE-POOLS: ICON.
TYPE-POOLS: KSPP.

** Tables
TABLES : ANLP, ANLA, ZTCO_VEHI_TYPE, T095B, T093B, MARA, COSS, AUFK,
         ANLAV, ZTCO_PLANDEP, T459A, T459V, V_T442C, *ZVCO_V_T442C,
         MDPB, KSPP, RM60X.

** Internal Table
* For ANLA - Asset Master Record Segment
DATA : BEGIN OF IT_ANLA_AUFK   OCCURS 0,
          BUKRS  LIKE ANLA-BUKRS,
          ANLN1  LIKE ANLA-ANLN1,
          ANLN2  LIKE ANLA-ANLN2,
          IZWEK  LIKE AUFK-IZWEK,
       END OF IT_ANLA_AUFK  .
* For CCtr group - P_NCOAL
DATA : IT_NODES_P_NCOAL  TYPE GSETH_NODE_TAB
                         WITH HEADER LINE ,
       IT_VALUES_P_NCOAL TYPE GSETH_VAL_TAB
                         WITH HEADER LINE .
* For CCtr group - 'HMMA2' engin CCtrs
DATA : IT_NODES_HMMA2  TYPE GSETH_NODE_TAB
                       WITH HEADER LINE ,
       IT_VALUES_HMMA2 TYPE GSETH_VAL_TAB
                       WITH HEADER LINE .
* For Posting
DATA : BEGIN OF IT_POST OCCURS 1000,
        KOSTL       LIKE  CSKS-KOSTL,
*       LSTAR       LIKE  CSLA-LSTAR,
        KSTAR       LIKE  COSP-KSTAR.
*       OBJNR       LIKE  COSP-OBJNR.
        INCLUDE STRUCTURE ZSCO_COSP_AMT04. "Fix
DATA : END OF   IT_POST.
* For Vehicle Model
DATA : IT_ZTCO_VEHI_TYPE LIKE STANDARD TABLE OF ZTCO_VEHI_TYPE
                         WITH HEADER LINE
                         INITIAL SIZE 50.

** Global Field
DATA : GV_KTOPL LIKE T095B-KTOPL.

** For BAPI
DATA : IT_COSTCENTERLIST LIKE STANDARD TABLE OF BAPI0012_CCLIST
                         WITH HEADER LINE.
DATA : IT_RETURN         LIKE STANDARD TABLE OF BAPIRET2
                         WITH HEADER LINE.
DATA : WA_HEADERINFO     LIKE BAPIPLNHDR.
DATA : IT_INDEXSTRUCTURE LIKE STANDARD TABLE OF BAPIACPSTRU
                         WITH HEADER LINE.
DATA : IT_COOBJECT       LIKE STANDARD TABLE OF BAPIPCPOBJ
                         WITH HEADER LINE.
DATA : IT_PERVALUE       LIKE STANDARD TABLE OF BAPIPCPVAL
                         WITH HEADER LINE.
DATA : IT_TOTVALUE       LIKE STANDARD TABLE OF BAPIPCPTOT
                         WITH HEADER LINE.

** For calling other programs/ALV
* Naming Convention can be different because a standard program
* is included
*     Waehrungsschluessel.
DATA : SAV_WAERS       LIKE T093B-WAERS.
* Data's und Forms für ALV-Anbindung                             "alv
INCLUDE RAKOPL02_ALV_DATA.                                       "alv
DATA : IT_GT_OUTTAB    LIKE STANDARD TABLE OF GT_OUTTAB
                       WITH HEADER LINE .
* ALV (w/o data definition of INCLUDE RAKOPL02_ALV_DATA.)
DATA : GV_COL_POS TYPE I.
DATA : IT_FIELDCAT          TYPE SLIS_T_FIELDCAT_ALV,
       WA_FIELDCAT           LIKE LINE OF IT_FIELDCAT,
       IT_EVENTCAT          TYPE SLIS_T_EVENT,
       WA_EVENTCAT           LIKE LINE OF IT_EVENTCAT.
DATA : IT_EVENTS	          TYPE SLIS_T_EVENT,
       IT_EVENT_EXIT	    TYPE SLIS_T_EVENT_EXIT.
DATA : IT_SORT         TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE .
DATA : IT_EXCLUDING    TYPE SLIS_T_EXTAB        WITH HEADER LINE .
* For Updating CBO Table
DATA : IT_ZTCO_PLANDEP LIKE STANDARD TABLE OF ZTCO_PLANDEP
                       WITH HEADER LINE .
DATA : IT_DISP_PLANDEP LIKE STANDARD TABLE OF ZTCO_PLANDEP
                       WITH HEADER LINE .
* For LTP AT(Quantity)
DATA : R_RSLSTAR         LIKE TABLE OF RSLSTAR
                         WITH HEADER LINE.
* Neue Anzeigeliste                                         "P30K090742
DATA: GT_DISPLAYLIST TYPE KSPP_T_RESULTLIST                 "P30K090742
            OCCURS 0 WITH HEADER LINE.                      "P30K090742

*----------------------------------------------------------------------*
*  Macro
*----------------------------------------------------------------------*
* Macro For Transferring value in BAPI
DEFINE TRANS_VALUE.
* Minus Posting
  IT_PERVALUE-FIX_VAL_PER&1  =  IT_POST-WKF0&1 * ( -1 ).
* IT_PERVALUE-VAR_VAL_PER&1  =  IT_POST-VAR0&1.
END-OF-DEFINITION.
* Macro For Transferring value
DEFINE TRANS_VALUE_LC.
  IT_POST-WKF0&1 = IT_DISP_PLANDEP-VAL&1.
END-OF-DEFINITION.


*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-001.
PARAMETERS : P_KOKRS LIKE CSKS-KOKRS   MEMORY   ID CAC OBLIGATORY,
             P_BUKRS LIKE ANLP-BUKRS   MEMORY   ID BUK OBLIGATORY,
             P_WERKS LIKE CKI64A-WERKS                 OBLIGATORY,
             P_GJAHR LIKE ANLP-GJAHR   MEMORY   ID GJR OBLIGATORY,
             P_VERSN LIKE COSP-VERSN                   OBLIGATORY,
*            P_PERAF LIKE ANLP-PERAF   MEMORY   ID BPE OBLIGATORY,
             P_CURRT LIKE PLNHDR-PLNCT DEFAULT 'C'     OBLIGATORY.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) TEXT-002.
*   Planung-von-Periode.
PARAMETERS: P_VONPE LIKE RKU01G-PERAB OBLIGATORY.
SELECTION-SCREEN COMMENT 52(05) TEXT-003.
*   Planung-bis-Periode.
PARAMETERS: P_BISPE LIKE RKU01G-PERBI OBLIGATORY.
SELECTION-SCREEN END OF LINE.
PARAMETERS :
             P_LSTAR LIKE CSLA-LSTAR            DEFAULT 'MCH_HR'
                                                       OBLIGATORY,
             P_PCORR LIKE KSPP-PCORR   AS CHECKBOX,
             P_NCOAL LIKE GRPDYNP-NAME_COALL    DEFAULT GV_CCGR_SETID
                                                       OBLIGATORY,
             P_AFABER LIKE ANLP-AFABER  DEFAULT '01'   OBLIGATORY.
SELECT-OPTIONS :
*            S_IZWEK  FOR  AUFK-IZWEK   DEFAULT 'A'    OBLIGATORY.
             S_VEHTP  FOR  ZTCO_VEHI_TYPE-VEHTP        MATCHCODE OBJECT
                                                       ZSH_CO_VEH_MODEL
                                                       OBLIGATORY.
SELECTION-SCREEN END OF BLOCK BL1.


* For PP Long-Term Planning data
SELECTION-SCREEN BEGIN OF BLOCK BL5 WITH FRAME TITLE TEXT-009.

SELECT-OPTIONS:  S_BEDAE FOR  RM60X-BEDAE OBLIGATORY.

PARAMETERS :     P_VERSB LIKE RM60X-VERSB
*                         FOR TABLE 'T459V'
                               OBLIGATORY.
SELECTION-SCREEN END OF BLOCK BL5.


* For FI Planned Dep. Cost
SELECTION-SCREEN BEGIN OF BLOCK BL2 WITH FRAME TITLE TEXT-005.
SELECTION-SCREEN BEGIN OF BLOCK BL3 WITH FRAME TITLE TEXT-006.
PARAMETERS:        P_PRNAM1  LIKE IMPR-PRNAM
                             MEMORY ID IMT             OBLIGATORY,
                   P_POSI1   LIKE IMPR-POSID
                             MEMORY ID IMP.
SELECT-OPTIONS:    S_GNJHR1  FOR ANLAV-PRGJR
                             MEMORY ID GJR             OBLIGATORY.
PARAMETERS :       P_PGSEL1  AS CHECKBOX  DEFAULT 'X'            ,
                   P_XAUFT1  AS CHECKBOX  DEFAULT 'X'            .
SELECT-OPTIONS:    S_EAUFN1  FOR    ANLAV-EAUFN                  ,
                   S_ANLKL1  FOR    ANLAV-ANLKL                  .
SELECTION-SCREEN END OF BLOCK BL3.

**// Mod. by hyung Jin Youn 2004.01.13
* FI runs the process of "Depreciation Cost Plan" Only one time

*SELECTION-SCREEN BEGIN OF BLOCK BL4 WITH FRAME TITLE TEXT-007.
*PARAMETERS:        P_PRNAM2  LIKE IMPR-PRNAM
*                             MEMORY ID IMT             OBLIGATORY,
*                   P_POSI2   LIKE IMPR-POSID
*                             MEMORY ID IMP.
*SELECT-OPTIONS:    S_GNJHR2  FOR ANLAV-PRGJR
*                             MEMORY ID GJR             OBLIGATORY.
*PARAMETERS :       P_PGSEL2  AS CHECKBOX  DEFAULT 'X'            ,
*                   P_XAUFT2  AS CHECKBOX  DEFAULT 'X'            .
*SELECT-OPTIONS:    S_EAUFN2  FOR    ANLAV-EAUFN                  ,
*                   S_ANLKL2  FOR    ANLAV-ANLKL                  .
*SELECTION-SCREEN END OF BLOCK BL4.

**// End. of Mod.

SELECTION-SCREEN END OF BLOCK BL2.


*
