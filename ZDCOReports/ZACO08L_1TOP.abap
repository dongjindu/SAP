*----------------------------------------------------------------------*
*   INCLUDE ZACO08L_1TOP                                               *
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
TABLES : COSP, ZTCO_VRRATIO, ZTCO_VRCCATCE.

** Internal Table
* For ZTCO_VRRATIO
DATA : BEGIN OF IT_ZTCO_VRRATIO OCCURS 0,
        GJAHR       LIKE  ZTCO_VRRATIO-GJAHR,
        NAME_COALL  LIKE  ZTCO_VRRATIO-NAME_COALL,
        KOSTL       LIKE  ZTCO_VRRATIO-KOSTL,
        LSTAR       LIKE  ZTCO_VRCCATCE-LSTAR,
        KSTAR       LIKE  ZTCO_VRRATIO-KSTAR,
        VRRATIO     LIKE  ZTCO_VRRATIO-VRRATIO,
        OBJNR       LIKE  COSP-OBJNR,
       END OF  IT_ZTCO_VRRATIO.
* For Main ITAB
DATA : BEGIN OF WA_COSP.
DATA :  NAME_COALL  LIKE  ZTCO_VRRATIO-NAME_COALL,
        KOSTL       LIKE  ZTCO_VRRATIO-KOSTL,
        LSTAR       LIKE  ZTCO_VRCCATCE-LSTAR,
*       KSTAR       LIKE  ZTCO_VRRATIO-KSTAR,
        VRRATIO     LIKE  ZTCO_VRRATIO-VRRATIO.
        INCLUDE STRUCTURE ZSCO_COSP_AMT01.
DATA : END OF   WA_COSP.
DATA : BEGIN OF IT_COSP OCCURS 0.
        INCLUDE STRUCTURE WA_COSP.
        INCLUDE STRUCTURE ZSCO_COSP_AMT02.
DATA : END OF  IT_COSP.
DATA : BEGIN OF IT_LIST OCCURS 0,
        NAME_COALL  LIKE  ZTCO_VRRATIO-NAME_COALL,
        KOSTL       LIKE  ZTCO_VRRATIO-KOSTL,
        LSTAR       LIKE  ZTCO_VRCCATCE-LSTAR,
        KSTAR       LIKE  ZTCO_VRRATIO-KSTAR,
        VRRATIO     LIKE  ZTCO_VRRATIO-VRRATIO,
        ORG_VAL     LIKE  COSP-WKF001,  "Original Value
        FIX_VAL     LIKE  COSP-WKF001,  "Changed Fix-Cost
        VAR_VAL     LIKE  COSP-WKF001,  "Changed Variable-Cost
        DIF_VAL     LIKE  COSP-WKF001,  "Diff. Amount
        WAERS       LIKE  TKA01-WAERS,  "CO area Currency
      END OF IT_LIST.

** Work Area / Global Fields
* For Fields Group
DATA : GV_FIELDGROUP_WKF(3) VALUE 'WKF'.
DATA : GV_FIELDGROUP_WKG(3) VALUE 'WKG'.
DATA : GV_FIELDGROUP_VAR(3) VALUE 'VAR'.
* For Read DATA
DATA : GV_PERCOUNT LIKE COSP-PERBL. "Period Counter
* For DD data
DATA : IT_ET_FIELDLIST LIKE TABLE OF RFVICP_DDIC_TABL_FIELDNAME.

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

** Costants
CONSTANTS : C_IC_TABNAME TYPE  DDOBJNAME VALUE 'ZSCO_COSP_AMT01'.

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

* Macro For Transferring value in BAPI
DEFINE TRANS_VALUE.
  IT_PERVALUE-FIX_VAL_PER&1  =  IT_COSP-WKF0&1.
  IT_PERVALUE-VAR_VAL_PER&1  =  IT_COSP-VAR0&1.
END-OF-DEFINITION.


*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-001.
PARAMETERS : P_KOKRS LIKE CSKS-KOKRS MEMORY ID CAC OBLIGATORY,
             P_GJAHR LIKE COBK-GJAHR MEMORY ID GJR OBLIGATORY,
             P_FRPER LIKE COSP-PERBL MEMORY ID BPE OBLIGATORY,
             P_TOPER LIKE COSP-PERBL MEMORY ID BPE OBLIGATORY,
             P_VERSN LIKE COBK-VERSN MEMORY ID KVT OBLIGATORY,
             P_CURRT LIKE PLNHDR-PLNCT DEFAULT 'C' OBLIGATORY,
             P_TRUN(1).
SELECTION-SCREEN END OF BLOCK BL1.



*
