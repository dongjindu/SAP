*----------------------------------------------------------------------*
*   INCLUDE ZACO45L_1TOP                                               *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*   Data Definition
*----------------------------------------------------------------------*
** type-pools
TYPE-POOLS: SLIS, VRM.

** Tables
TABLES : ZTCO_WARR, CE0H201, TKA01.
TABLES : VBRK, VBRP, KONV, COBK, COEP.

** Internal tables
* For Warranty & Campaign Linkage
DATA : BEGIN OF IT_ZTCO_WARR OCCURS 0.
        INCLUDE STRUCTURE  ZTCO_WARR.
DATA : END OF  IT_ZTCO_WARR .
* For COBK
DATA : BEGIN OF IT_COBK OCCURS 0.
        INCLUDE STRUCTURE COBK.
DATA : END OF   IT_COBK .
* For VBRK KNOV
DATA : BEGIN OF IT_VBRK_KONV  OCCURS 100,
         VBELN   LIKE VBRK-VBELN,
         KNUMV   LIKE VBRK-KNUMV,
         SAKN1   LIKE KONV-SAKN1,
         KSCHL   LIKE KONV-KSCHL,
         KWERT   LIKE KONV-KWERT,
         WAERS   LIKE KONV-WAERS,
         KKURS   LIKE KONV-KKURS,
         KBETR   LIKE KONV-KBETR,
         WKGBTR  LIKE COEP-WKGBTR,
         COWAER  LIKE COEP-OWAER,
       END OF   IT_VBRK_KONV.
* For Condition Numbers
DATA : BEGIN OF IT_KNUMV OCCURS 0,
        VBELN   LIKE VBRK-VBELN,
        KNUMV   LIKE VBRK-KNUMV,
        FKSTO LIKE VBRK-FKSTO  ,
        WAERK   LIKE VBRK-WAERK,
        KURRF   like VBRK-KURRF,
       END OF IT_KNUMV.
* For Collected Data
DATA : BEGIN OF IT_COL_DATA  OCCURS 100,
         PAPH2    LIKE ZTCO_WARR-PAPH2,
         COPA_VALUES
                  LIKE ZTCO_WARR-COPA_VALUES,
         WKGBTR   LIKE COEP-WKGBTR,
         COWAER   LIKE COEP-OWAER,
         VM_SUM   LIKE COEP-WKGBTR,
         %_OF_SUM(6) TYPE P DECIMALS 2,
         COM_X_%  LIKE COEP-WKGBTR,
       END OF   IT_COL_DATA.
* For VM 'COMMON'
DATA : BEGIN OF IT_COMMON  OCCURS 0.
        INCLUDE STRUCTURE IT_COL_DATA.
DATA : END OF   IT_COMMON.
* For Posting
DATA : BEGIN OF IT_POST  OCCURS 100.
        INCLUDE STRUCTURE IT_COL_DATA.
DATA : END OF   IT_POST.

** Global Values
* Data for drop_down_list
DATA: GV_DRLIST_NAME  TYPE VRM_ID,
      IT_DRLIST_LIST  TYPE VRM_VALUES,
      WA_DRLIST_VALUE LIKE LINE OF IT_DRLIST_LIST.
* Summed value of Amt. in COMMON Tab
DATA : GV_COM_SUM LIKE COEP-WKGBTR.

** For ALV
DATA : GV_REPID LIKE SY-REPID.
DATA : GV_STATUS TYPE SLIS_FORMNAME VALUE 'PF_STATUS_VAR'.
DATA : GV_USER_COMMAND TYPE SLIS_FORMNAME VALUE 'USER_COMMAND'.
DATA : GV_COL_POS TYPE I.
DATA : IT_FIELDCAT          TYPE SLIS_T_FIELDCAT_ALV,
       WA_FIELDCAT           LIKE LINE OF IT_FIELDCAT,
       IT_EVENTCAT          TYPE SLIS_T_EVENT,
       WA_EVENTCAT           LIKE LINE OF IT_EVENTCAT.
DATA : IT_EVENTS	          TYPE SLIS_T_EVENT,
       IT_EVENT_EXIT	    TYPE SLIS_T_EVENT_EXIT.

* For BAPI
DATA : IT_RETURN         LIKE STANDARD TABLE OF BAPIRET2
                         WITH HEADER LINE.
DATA : GV_OPERATINGCONCERN  LIKE  BAPI0017-OP_CONCERN,
       GV_TESTRUN           LIKE  BAPI0017-TESTRUN.
DATA : IT_INPUTDATA      LIKE STANDARD TABLE OF   BAPI_COPA_DATA
                         WITH HEADER LINE,
       IT_FIELDLIST      LIKE STANDARD TABLE OF   BAPI_COPA_FIELD
                         WITH HEADER LINE.


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
PARAMETERS : P_KOKRS LIKE CE0H201-KOKRS MEMORY   ID CAC
                                        OBLIGATORY,
             P_BUKRS LIKE CE0H201-BUKRS MEMORY   ID BUK
                                        OBLIGATORY,
             P_GJAHR LIKE CE0H201-GJAHR OBLIGATORY,
             P_PERDE LIKE CE0H201-PERDE OBLIGATORY,
             P_BUDAT LIKE CE0H201-BUDAT NO-DISPLAY,
             P_VRGAR LIKE CE0H201-VRGAR DEFAULT 'X'
                                        MODIF ID VRG
                                        OBLIGATORY,
             P_TRUN as checkbox  default 'X'.
SELECTION-SCREEN END OF BLOCK BL1.

SELECTION-SCREEN BEGIN OF BLOCK BL2 WITH FRAME TITLE TEXT-002.
PARAMETERS : P_SAKN1 LIKE ZTCO_WARR-SAKN1
                     AS LISTBOX VISIBLE LENGTH 20
                     OBLIGATORY.
SELECTION-SCREEN END OF BLOCK BL2.
