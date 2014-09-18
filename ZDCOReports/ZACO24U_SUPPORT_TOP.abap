*----------------------------------------------------------------------*
*   INCLUDE ZACO24U_SUPPORT_TOP                                        *
*----------------------------------------------------------------------*

** type-pools
TYPE-POOLS: SLIS.
include:  <icon>.

** Table
TABLES : ztco_mha, cosp.

ranges : r_kostl for ztco_mha-kostl,
         r_kstar for cosp-kstar.


** Internal Table

DATA: BEGIN OF IT_mha_temp OCCURS 0,
       KOKRS   like ztco_mha-kokrs,
       GJAHR   like ztco_mha-gjahr,
       PERID   like ztco_mha-perid,
       kOSTL   like ztco_mha-kostl,
       SRKOSTL LIKE ZTCO_MHA-SRKOSTL,
       LGART   like ztco_mha-lgart,
       ANZHL   like ztco_mha-anzhl,
     END OF IT_mha_temp.

DATA: BEGIN OF IT_mha OCCURS 0,
       KOKRS   like ztco_mha-kokrs,
       GJAHR   like ztco_mha-gjahr,
       PERID   like ztco_mha-perid,
       kOSTL   like ztco_mha-kostl,
       SRKOSTL LIKE ZTCO_MHA-SRKOSTL,
       tot_mh  like ztco_mha-anzhl,
       SUR_MH  LIKE ZTCO_MHA-ANZHL,
       objnr   like cosp-objnr,
     END OF IT_mha.

DATA: BEGIN OF IT_mha_TOT OCCURS 0,
       KOKRS   like ztco_mha-kokrs,
       GJAHR   like ztco_mha-gjahr,
       PERID   like ztco_mha-perid,
       kOSTL   like ztco_mha-kostl,
       SRKOSTL LIKE ZTCO_MHA-SRKOSTL,
       mh      like ztco_mha-anzhl,
     END OF IT_mha_TOT.

DATA: BEGIN OF IT_mha_SUR OCCURS 0,
       KOKRS   like ztco_mha-kokrs,
       GJAHR   like ztco_mha-gjahr,
       PERID   like ztco_mha-perid,
       kOSTL   like ztco_mha-kostl,
       SRKOSTL LIKE ZTCO_MHA-SRKOSTL,
       mh      like ztco_mha-anzhl,
     END OF IT_mha_SUR.




data : it_cosp_temp like cosp occurs 0 with header line.

DATA: BEGIN OF IT_cosp OCCURS 0,
       GJAHR   like cosp-gjahr,
       kOSTL   like ztco_mha-kostl,
       kstar   like cosp-kstar,
       amt     like cosp-wkg001,
     END OF IT_cosp.


* for reporting
DATA : BEGIN OF IT_REPORT OCCURS 0,
        GJAHR   LIKE  ZTCO_MHA-GJAHR,
        PERID   LIKE  ZTCO_MHA-PERID,
        KOSTL   LIKE  ZTCO_MHA-KOSTL,
        SrKOSTL LIKE  ZTCO_MHA-srKOSTL,
        KSTAR   LIKE  cosp-KSTAR,
        TOT_MH  LIKE  ZTCO_MHA-ANZHL,
        SUR_MH  LIKE  ZTCO_MHA-ANZHL,
        AMT     LIKE  COSP-WKG001,
        RATE    TYPE  P DECIMALS 2,
        OBJNR   like coep-OBJNR,
        PAROB   like coep-PAROB,
        chkBOX  type c,
        icon    type icon_d,
      END OF IT_REPORT.

DATA : IT_COEP_TEMP LIKE COEP OCCURS 0 WITH HEADER LINE.
* for posting
DATA : BEGIN OF IT_POST OCCURS 0,
        GJAHR   LIKE  COSL-GJAHR,
        R_KOSTL LIKE  CSSL-KOSTL,
        R_LSTAR LIKE  CSSL-LSTAR,
        KOSTL   LIKE  CSKS-KOSTL,
        LSTAR   LIKE  CSLA-LSTAR.
        INCLUDE STRUCTURE ZSCO_COSP_AMT02.
DATA : END OF  IT_POST.

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
DATA : IT_EVENTS            TYPE SLIS_T_EVENT,
       IT_EVENT_EXIT      TYPE SLIS_T_EVENT_EXIT.

*** For Cost center & Cost element
DATA : IT_COSTCENTERLIST LIKE STANDARD TABLE OF BAPI0012_CCLIST
                         WITH HEADER LINE.
DATA : IT_RETURN         LIKE STANDARD TABLE OF BAPIRET2
                         WITH HEADER LINE.

DATA:  BEGIN OF IT_BELNR OCCURS 0,
         BELNR  LIKE COBK-BELNR,
       END OF   IT_BELNR.


DATA:  wa_return1 LIKE bapiret2,
       wa_hnodes  LIKE bapiset_hier,
       wa_hvalues  LIKE bapi1112_values.

data : it_hnodes LIKE TABLE OF wa_hnodes,
       it_hvalues LIKE TABLE OF wa_hvalues WITH HEADER LINE.

DATA : BEGIN OF wa_group_detail,
         gname   LIKE wa_hnodes-groupname,
         valfrom LIKE wa_hvalues-valfrom,
         valto   LIKE wa_hvalues-valto,
       END OF wa_group_detail.
data : it_ceg_detail LIKE TABLE OF wa_group_detail.

FIELD-SYMBOLS: <f_field>.

DATA : w_cnt1 TYPE i,
       w_cnt2 TYPE i,
       w_idx TYPE i.

* Posting (manual cost allocation)
DATA : WA_DOC_HEADER LIKE BAPIDOCHDRU12P .
DATA : IT_DOC_ITEMS  LIKE STANDARD TABLE OF BAPIMAITM
                     WITH HEADER LINE.
