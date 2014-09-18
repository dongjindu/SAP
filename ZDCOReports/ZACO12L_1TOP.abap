*----------------------------------------------------------------------*
*   INCLUDE ZACO12L_1TOP                                               *
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

** Tables
TABLES : ANLP, ANLA, ZTCO_VEHI_TYPE, T095B, AUFK, T093B, MARA, COSS.

** Internal Table
* Main Internal table
DATA : BEGIN OF IT_ANLP OCCURS 1000,
        BUKRS   LIKE ANLP-BUKRS,
        GJAHR   LIKE ANLP-GJAHR,
        PERAF   LIKE ANLP-PERAF,
        AFBNR   LIKE ANLP-AFBNR,
        ANLN1   LIKE ANLP-ANLN1,
        ANLN2   LIKE ANLP-ANLN2,
        AFABER  LIKE ANLP-AFABER,
        KOSTL   LIKE ANLP-KOSTL,
        KTOGR   LIKE ANLP-KTOGR,
        NAFAZ   LIKE ANLP-NAFAZ,
*        IZWEK   LIKE AUFK-IZWEK,
        VEHTP   LIKE ZTCO_VEHI_TYPE-VEHTP,
        KTNAFG  LIKE T095B-KTNAFG,
        WAERS   LIKE T093B-WAERS,
       END OF   IT_ANLP.
DATA : BEGIN OF IT_RES_ANLP OCCURS 0,
        BUKRS   LIKE ANLP-BUKRS,
        GJAHR   LIKE ANLP-GJAHR,
        PERAF   LIKE ANLP-PERAF,
        KOSTL   LIKE ANLP-KOSTL,
*        IZWEK   LIKE AUFK-IZWEK,
        VEHTP   LIKE ZTCO_VEHI_TYPE-VEHTP,
        KTNAFG  LIKE T095B-KTNAFG,
        NAFAZ   LIKE ANLP-NAFAZ,
        WAERS   LIKE T093B-WAERS,
       END OF   IT_RES_ANLP.
DATA : BEGIN OF IT_RS_MARA OCCURS 0,
        MATNR   LIKE MARA-MATNR,
        MTART   LIKE MARA-MTART,
*        IZWEK   LIKE ZTCO_VEHI_TYPE-IZWEK,
        VEHTP   LIKE ZTCO_VEHI_TYPE-VEHTP,
       END OF   IT_RS_MARA.
DATA : BEGIN OF IT_ORDER_DATA OCCURS 0.
        INCLUDE STRUCTURE  IT_RS_MARA.
DATA :  AUFNR   LIKE AFPO-AUFNR,
        KOSTL   LIKE ANLP-KOSTL,
        LSTAR   LIKE CSLA-LSTAR,
        OBJNR   LIKE COSS-OBJNR,
        PAROB   LIKE COSS-PAROB,
        USPOB   LIKE COSS-USPOB.
DATA : END OF   IT_ORDER_DATA.
DATA : BEGIN OF IT_COSS OCCURS 0.
DATA :
*        IZWEK   LIKE ZTCO_VEHI_TYPE-IZWEK,
        VEHTP   LIKE ZTCO_VEHI_TYPE-VEHTP,
        KOSTL   LIKE ANLP-KOSTL,
        AUFNR   LIKE AFPO-AUFNR,
        KTNAFG  LIKE T095B-KTNAFG.
        INCLUDE STRUCTURE ZSCO_COSS_KEY01.
        INCLUDE STRUCTURE ZSCO_COSS_MEG01.
DATA : END OF  IT_COSS.
DATA : BEGIN OF IT_RATE OCCURS 0,
*        IZWEK   LIKE AUFK-IZWEK,
        VEHTP   LIKE ZTCO_VEHI_TYPE-VEHTP,
        KOSTL   LIKE ANLP-KOSTL,
        AUFNR   LIKE AFPO-AUFNR,
        MEGXX   LIKE COSS-MEG001,
*       LSTAR   LIKE CSLA-LSTAR,
        RATE(11) TYPE P  DECIMALS 7,
*        RATE    LIKE ZSCO_COSS_RAT01-RAT001,
       END OF  IT_RATE.
DATA : BEGIN OF IT_POST OCCURS 0.
        INCLUDE STRUCTURE IT_RATE.
DATA :  KTNAFG    LIKE T095B-KTNAFG,
        NAFAZ     LIKE ANLP-NAFAZ,
        CHG_NAFAZ LIKE ANLP-NAFAZ,
        DEPAMT    LIKE ANLP-NAFAZ,
        WAERS     LIKE T093B-WAERS.
DATA : END OF  IT_POST.
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
* For ANLA - Asset Master Record Segment
DATA : BEGIN OF IT_ANLA_AUFK   OCCURS 0,
          BUKRS  LIKE ANLA-BUKRS,
          ANLN1  LIKE ANLA-ANLN1,
          ANLN2  LIKE ANLA-ANLN2,
          IZWEK  LIKE AUFK-IZWEK,
          VEHTP  LIKE ZTCO_VEHI_TYPE-VEHTP,
       END OF IT_ANLA_AUFK  .
* For Vehicle Model
DATA : IT_ZTCO_VEHI_TYPE LIKE STANDARD TABLE OF ZTCO_VEHI_TYPE
                         WITH HEADER LINE
                         INITIAL SIZE 50.

** Global Field
DATA : GV_KTOPL LIKE T095B-KTOPL.
* For DATA retrieval
DATA : GV_CI_TABNAME TYPE  DDOBJNAME .
* For DD data
DATA : IT_ET_FIELDLIST LIKE TABLE OF RFVICP_DDIC_TABL_FIELDNAME
                       WITH HEADER LINE.

** For BAPI
DATA : IT_COSTCENTERLIST LIKE STANDARD TABLE OF BAPI0012_CCLIST
                         WITH HEADER LINE.
DATA : IT_RETURN         LIKE STANDARD TABLE OF BAPIRET2
                         WITH HEADER LINE.

** For ALV
DATA : GV_REPID LIKE SY-REPID.
DATA : GV_STATUS       TYPE SLIS_FORMNAME VALUE 'PF_STATUS_VAR'.
DATA : GV_USER_COMMAND TYPE SLIS_FORMNAME VALUE 'USER_COMMAND'.
DATA : IT_SORT         TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE .
DATA : GV_COL_POS TYPE I.
DATA : IT_FIELDCAT          TYPE SLIS_T_FIELDCAT_ALV,
       WA_FIELDCAT           LIKE LINE OF IT_FIELDCAT,
       IT_EVENTCAT          TYPE SLIS_T_EVENT,
       WA_EVENTCAT           LIKE LINE OF IT_EVENTCAT.
DATA : IT_EVENTS	          TYPE SLIS_T_EVENT,
       IT_EVENT_EXIT	    TYPE SLIS_T_EVENT_EXIT.

** For BDC
DATA : IT_BDCDATA LIKE STANDARD TABLE OF BDCDATA
                  WITH HEADER LINE .
DATA:  IT_MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.
DATA:  IT_RESUTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.
DATA:  BEGIN OF IT_BELNR OCCURS 0,
         BELNR  LIKE COBK-BELNR,
       END OF   IT_BELNR.


*----------------------------------------------------------------------*
*  Macro
*----------------------------------------------------------------------*
* Macro for defining FIELDCATs
DEFINE BULID_FIELDCAT.
  ADD 1 TO GV_COL_POS.
  WA_FIELDCAT-TABNAME     = &1.
  WA_FIELDCAT-FIELDNAME   = &2.
  WA_FIELDCAT-KEY         = &3.
  WA_FIELDCAT-DO_SUM      = &4.
  WA_FIELDCAT-CFIELDNAME  = &5.
  WA_FIELDCAT-CTABNAME    = &6.
  WA_FIELDCAT-OUTPUTLEN   = &7.
  WA_FIELDCAT-SELTEXT_L   = &8.
  WA_FIELDCAT-DATATYPE    = &9.
  WA_FIELDCAT-COL_POS     = GV_COL_POS.
*  WA_FIELDCAT-REF_FIELDNAME  = &5.
*  WA_FIELDCAT-REF_TABNAME = &6.
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.
END-OF-DEFINITION.

*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-001.
PARAMETERS : P_KOKRS LIKE CSKS-KOKRS   MEMORY   ID CAC OBLIGATORY,
             P_BUKRS LIKE ANLP-BUKRS   MEMORY   ID BUK OBLIGATORY,
             P_WERKS LIKE CKI64A-WERKS MEMORY   ID WRK OBLIGATORY,
             P_GJAHR LIKE ANLP-GJAHR   MEMORY   ID GJR OBLIGATORY,
             P_PERAF LIKE ANLP-PERAF   MEMORY   ID BPE OBLIGATORY,
             P_LSTAR LIKE CSLA-LSTAR   MEMORY   ID LAR OBLIGATORY,
             P_NCOAL LIKE GRPDYNP-NAME_COALL    DEFAULT 'DIRECT'
                                                       OBLIGATORY,
             P_VERSN  LIKE COSS-VERSN   DEFAULT '000'  OBLIGATORY,
             P_AFABER LIKE ANLP-AFABER  DEFAULT '01'   OBLIGATORY,
             P_BLDAT  LIKE COBK-BLDAT   DEFAULT SY-DATUM
                                                       OBLIGATORY,
             P_MODE(1)                  DEFAULT 'N'    OBLIGATORY.
SELECT-OPTIONS :
*             S_IZWEK  FOR  AUFK-IZWEK   DEFAULT 'A'    OBLIGATORY.
             S_VEHTP  FOR  ZTCO_VEHI_TYPE-VEHTP        MATCHCODE OBJECT
                                                       ZSH_CO_VEH_MODEL
                                                       OBLIGATORY.
**-- TEST  Temporary CODE For Unit test
*PARAMETERS : P_TEST(1)    .
**-- TEST

SELECTION-SCREEN END OF BLOCK BL1.

*
