* 4.6C
* QNZAHRK057327 include HUSPAYDATA - not marked linewise
* TIPPA1K000001 New tables UTIPS for Tips processing
* 4.0C
* YLHAHRK000132 01091998 ACCR definition moved to RPC2RX29
* 3.0D
* QFLP30K065957 12.04.94 Additional field in GRDO1
* QFLP30K060057 28.03.96 Additional garnishment tables, not stored in
*                        Cluster, only for RPCEDTU0
* 3.0B
* XJIP30K019868 231095 DFRT structure change
* 3.0A
* XJIS11K151799 290895 New Tables for TAX retro (DFRT)
* VNIS11K151799 180895 New Tables for Garnishment.
* 2.2A
*    CRTK900460 25051995 New tax calc.
*---------------------------------------------------------------------*
* Data definitions Cluster RU database PCL2 Payroll results USA       *
*---------------------------------------------------------------------*
INCLUDE huspaydata.                                       "QNZAHRK057327

* country   dependent
DATA: BEGIN OF  ru-version.
*      INCLUDE STRUCTURE PC201.       "version value 0    "XTWPH9K008656
       INCLUDE STRUCTURE pc201_pay.                       "XTWPH9K008656
DATA: END OF  ru-version.

DATA: BEGIN OF oru-version.
*       INCLUDE STRUCTURE PC201.                          "XTWPH9K008656
        INCLUDE STRUCTURE pc201_pay.                      "XTWPH9K008656
DATA: END OF oru-version.

*DATA: BEGIN OF NAME.
*        INCLUDE STRUCTURE PC22Q.
*DATA: END OF NAME.
*
*DATA: BEGIN OF PERM.
*        INCLUDE STRUCTURE PC22R.
*DATA: END OF PERM.
*
*DATA: BEGIN OF ADR.
*        INCLUDE STRUCTURE PC22S.
*DATA: END OF ADR.
*
** Tax table
*DATA: BEGIN OF TAX OCCURS 5.
*        INCLUDE STRUCTURE PC22T.
*DATA: END OF   TAX.
*
** US work state/locality taxing authorities prorates
*DATA: BEGIN OF TAXPR OCCURS 10.
*        INCLUDE STRUCTURE PC22X.
*DATA: END OF TAXPR.
*
** US residence authorities / UI authorities
*DATA: BEGIN OF TAXR OCCURS 5.
*        INCLUDE STRUCTURE PC23C.
*DATA: END OF TAXR.
*
** TCRTA cumulation table Tax wage types
*DATA: BEGIN OF TCRTA OCCURS 20.
*        INCLUDE STRUCTURE PC245.                              "QJC
*DATA: END OF TCRTA.
*
** TCRTB cumulation table Tax Company/Authorities
*DATA: BEGIN OF TCRTB OCCURS 5.
*        INCLUDE STRUCTURE PC23F.                              "QJC
*DATA: END OF TCRTB.

* TCRT is not a cluster table -------------------------------
*DATA: BEGIN OF TCRT OCCURS 20.                                "QJC
*        INCLUDE STRUCTURE PC23H.
*DATA: END OF TCRT.
*------------------------------------------------------------

* Difference RT for retrocalculation                      "XJIS11K151799
*DATA:  BEGIN OF DFRT OCCURS 20.
**      INCLUDE STRUCTURE PC207.                          "XJIP30K019868
*        INCLUDE STRUCTURE PC23R.                                 "
*DATA:  END   OF DFRT.

* Accruals
*ata: begin of accr occurs 100.                           "YLHAHRK000132
*       include structure pc23g.                          "YLHAHRK000132
*ata: end of accr.                                        "YLHAHRK000132

DATA: rp-imp-ru-subrc LIKE sy-subrc.

* tables for Garnishment
*DATA: BEGIN OF GRDOC OCCURS 5.    "Document            "VNIS11K151799
*        INCLUDE STRUCTURE PC290.
*DATA: END OF GRDOC.
*
*DATA: BEGIN OF GRORD OCCURS 5.    "Order               "VNIS11K151799
*        INCLUDE STRUCTURE PC291.
*DATA: END OF GRORD.
*
*DATA: BEGIN OF GRREC OCCURS 5.    "Result              "VNIS11K151799
*        INCLUDE STRUCTURE PC292.
*DATA: END OF GRREC.

DATA: BEGIN OF grdo1 OCCURS 5,    "Additional Document "QFLP30K060057
        abbeg LIKE pc290-chkdt,   "Begin of payroll period
        abend LIKE pc290-chkdt,   "End of payroll period
        tdeve LIKE pc291-groku,   "Total deduction to vendor "P30K065957
        totsc LIKE pc291-groku,   "Total service charge      "P30K065957
      END OF grdo1.

DATA: BEGIN OF gror1 OCCURS 5,      "Additional Order    "QFLP30K060057
        taxku LIKE pc291-groku,     "Taxes cumulativ
        fedex LIKE pc291-fedmi,     "Federal Minimum Exempt
        dereg LIKE pc291-deven,     "Regular deduction
        deadd LIKE pc291-deven,     "Additional deduction
        plisu(3) TYPE p DECIMALS 2, "Percent of LIMIT Support garnish.
        pdesu(3) TYPE p DECIMALS 2, "Percent of Deduc Support garnish.
        plirt(3) TYPE p DECIMALS 2, "Percent of Remainder Support garn.
        limsp LIKE pc291-limku,     "Limit without Federal Minimum
                                    "but with Remainder Support if appli
        plisp(3) TYPE p DECIMALS 2, "Percent of LIMSP
      END OF gror1.

INCLUDE rpcxruu0.                                        "XTWALRK016400

* UTIPS cumulation table for tip processing wage types    "TIPPA1K000001
*DATA: BEGIN OF UTIPS OCCURS  0.                          "TIPPA1K000001
*     INCLUDE STRUCTURE PC2A0.                            "TIPPA1K000001
*DATA: END OF UTIPS.                                      "TIPPA1K000001
