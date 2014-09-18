*----------------------------------------------------------------------*
*   INCLUDE ZACO39L_1TOP                                               *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*   Data Definition
*----------------------------------------------------------------------*
** type-pools
TYPE-POOLS: SLIS.
TYPE-POOLS: ICON.
TYPE-POOLS: KSPP, KKCK.                                     "P30K090742
TYPE-POOLS: KKPI.

** Tables
TABLES : ZTCO_VEHI_TYPE, *ZVCO_V_T442C, T459A, T459V, ZTCO_PLANDEP,
         MDPB, TKA01, KSPP, RM60X, MARA.

** Internal Table
* For ANLA - Asset Master Record Segment
DATA : BEGIN OF IT_ANLA_AUFK   OCCURS 0,
          BUKRS  LIKE ANLA-BUKRS,
          ANLN1  LIKE ANLA-ANLN1,
          ANLN2  LIKE ANLA-ANLN2,
          IZWEK  LIKE AUFK-IZWEK,
       END OF IT_ANLA_AUFK  .
* For source DATA
DATA : IT_ZTCO_PLANDEP   LIKE STANDARD TABLE OF ZTCO_PLANDEP
                         WITH HEADER LINE .
* For Vehicle Model
DATA : IT_ZTCO_VEHI_TYPE LIKE STANDARD TABLE OF ZTCO_VEHI_TYPE
                         WITH HEADER LINE
                         INITIAL SIZE 50.
* For LTP AT(Quantity)
DATA : IT_ZSCO_LTP_ATQ   LIKE STANDARD TABLE OF ZSCO_LTP_ATQ
                         WITH HEADER LINE
                         INITIAL SIZE 1000.
DATA : R_RSLSTAR         LIKE TABLE OF RSLSTAR
                         WITH HEADER LINE.
* Neue Anzeigeliste                                         "P30K090742
DATA: GT_DISPLAYLIST TYPE KSPP_T_RESULTLIST                 "P30K090742
            OCCURS 0 WITH HEADER LINE.                      "P30K090742

* For LTP Vehicle Quantity
DATA : IT_MDPB           LIKE STANDARD TABLE OF MDPB
                         WITH HEADER LINE.
* For MATNR
DATA : BEGIN OF IT_MARA OCCURS 0,
         VEHTP LIKE ZTCO_VEHI_TYPE-VEHTP,
         MTART LIKE MARA-MTART,
         MATNR LIKE MARA-MATNR,
         MEINS LIKE MARA-MEINS,
       END OF   IT_MARA .
* For Planned DEP. Cost
DATA : BEGIN OF IT_PLANDEP OCCURS 0,
         PERIOD LIKE RKU01G-PERBI, "Period
         VEHTP  LIKE ZTCO_VEHI_TYPE-VEHTP,
         KOSTL  LIKE CSKS-KOSTL,
         KOART  LIKE ZTCO_PLANDEP-KOART,
         VALXX  LIKE ZTCO_PLANDEP-VAL01,
         WAERS  LIKE ZTCO_PLANDEP-WAERS,
       END OF   IT_PLANDEP.
* For LTP_KSPP
DATA : BEGIN OF IT_LTP_KSPP OCCURS 0,
         WERKS  LIKE CKI64A-WERKS, "Plant
         LSTAR  LIKE CSSL-LSTAR  , "AT
         PERIOD LIKE RKU01G-PERBI, "Period
         VEHTP  LIKE ZTCO_VEHI_TYPE-VEHTP,
         MATNR  LIKE MARA-MATNR,
         KOSTL  LIKE CSKS-KOSTL,
         MEINH     LIKE COSSA-MEINH,
         MEG_NTRAN LIKE COSSA-MEG001,      "Nicht übernommene Mengen
         MEGBTR    LIKE COSSA-MEG001,
       END OF  IT_LTP_KSPP.
* For MDPB
DATA : BEGIN OF IT_LTP_MDPB OCCURS 0,
         WERKS  LIKE CKI64A-WERKS, "Plant
         PERIOD LIKE RKU01G-PERBI, "Period
         VEHTP  LIKE ZTCO_VEHI_TYPE-VEHTP,
         MATNR  LIKE MARA-MATNR,
         MEINS  LIKE MARA-MEINS,
         PLNMG  LIKE MDPB-PLNMG,
       END OF   IT_LTP_MDPB.
* For Main ITAB
DATA : BEGIN OF IT_COL_TAB OCCURS 0,
         WERKS  LIKE CKI64A-WERKS, "Plant
         LSTAR  LIKE CSSL-LSTAR  , "AT
         PERIOD LIKE RKU01G-PERBI, "Period
         VEHTP  LIKE ZTCO_VEHI_TYPE-VEHTP,
         MATNR  LIKE MARA-MATNR,
         KOSTL  LIKE CSKS-KOSTL,
         KOART  LIKE ZTCO_PLANDEP-KOART,
* Planned Dep. Cost
         WAERS  LIKE ZTCO_PLANDEP-WAERS,
         VALXX  LIKE ZTCO_PLANDEP-VAL01,
* KSPP
         MEINH     LIKE COSSA-MEINH,
         MEG_NTRAN LIKE COSSA-MEG001,      "Nicht übernommene Mengen
         MEGBTR    LIKE COSSA-MEG001,
* MDPB.
         MEINS     LIKE MARA-MEINS,
         PLNMG     LIKE MDPB-PLNMG,
* ATq_SUM
         AT_SUM    LIKE COSSA-MEG001,
         AT_%(10)  TYPE P DECIMALS 6,
         DEP_AT_%  LIKE ZTCO_PLANDEP-VAL01,
         DEP_AT_%_PLM
                   LIKE ZTCO_PLANDEP-VAL01.
DATA : END OF IT_COL_TAB.

** Global variables
DATA : GV_PERCOUNT LIKE COSP-PERBL. "Period Counter

** For ALV
DATA : GV_REPID LIKE SY-REPID.
DATA : GV_STATUS       TYPE SLIS_FORMNAME VALUE 'PF_STATUS'.
DATA : GV_USER_COMMAND TYPE SLIS_FORMNAME VALUE 'USER_COMMAND'.
DATA : IT_SORT         TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE .
DATA : GV_COL_POS TYPE I.
DATA : IT_FIELDCAT          TYPE SLIS_T_FIELDCAT_ALV,
       WA_FIELDCAT          LIKE LINE OF IT_FIELDCAT,
       IT_EVENTCAT          TYPE SLIS_T_EVENT,
       WA_EVENTCAT          LIKE LINE OF IT_EVENTCAT.
DATA : IT_EVENTS	          TYPE SLIS_T_EVENT,
       IT_EVENT_EXIT	    TYPE SLIS_T_EVENT_EXIT.

** For posting FM (Addictive cost)
DATA : BEGIN OF IT_POST OCCURS 100,
         PERIOD LIKE RKU01G-PERBI, "Period
         MATNR  LIKE MARA-MATNR,
         KOART  LIKE ZTCO_PLANDEP-KOART,
         DEP_AT_%_PLM
                   LIKE ZTCO_PLANDEP-VAL01.
DATA : END OF IT_POST.
DATA : IT_TRANSFER_DATA TYPE KKPI_TRANSFER_DATA
                        WITH HEADER LINE.


*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-001.
PARAMETERS : P_KOKRS LIKE CSKS-KOKRS   MEMORY   ID CAC OBLIGATORY,
             P_BUKRS LIKE ANLP-BUKRS   MEMORY   ID BUK OBLIGATORY,
             P_GJAHR LIKE ANLP-GJAHR   MEMORY   ID GJR OBLIGATORY,
             P_VERSN LIKE COSP-VERSN                   OBLIGATORY,

             P_WERKS LIKE CKI64A-WERKS DEFAULT  'P001'
                                       MODIF ID ZPA
                                                       OBLIGATORY,
             P_LSTAR LIKE CSSL-LSTAR   DEFAULT  'MCH_HR'
                                       MODIF ID ZPA
                                                       OBLIGATORY,
             P_KLVAR LIKE CKI64A-KLVAR  DEFAULT 'ZPC1' OBLIGATORY,
             P_TVERS LIKE CKI64A-TVERS  DEFAULT '01'   OBLIGATORY.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) TEXT-002.
*   Planung-von-Periode.
PARAMETERS: P_VONPE LIKE RKU01G-PERAB OBLIGATORY.
SELECTION-SCREEN COMMENT 52(05) TEXT-003.
*   Planung-bis-Periode.
PARAMETERS: P_BISPE LIKE RKU01G-PERBI OBLIGATORY.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BL1.

* For PP Long-Term Planning data
SELECTION-SCREEN BEGIN OF BLOCK BL5 WITH FRAME TITLE TEXT-009.
SELECT-OPTIONS :
             S_BEDAE FOR  RM60X-BEDAE
                                       OBLIGATORY.
*                                      FOR TABLE 'T459A'

PARAMETERS : P_VERSB LIKE RM60X-VERSB
*                                      FOR TABLE 'T459V'
                                       OBLIGATORY.
PARAMETERS :
             P_PCORR LIKE KSPP-PCORR   AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK BL5.

* Change Plant Code and AT
PARAMETERS : P_CHG_C AS CHECKBOX USER-COMMAND PCHKBOX,
             P_MODE  DEFAULT 'N' NO-DISPLAY.
