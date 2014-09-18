*----------------------------------------------------------------------*
*   INCLUDE ZLZGCO_GLOBAL_FORMTO1                                      *
*----------------------------------------------------------------------*
* type Pools for Variable Ratio Table
TYPE-POOLS GSETH .
* CCGroup For Indirect Direct SemiDirect
DATA: GV_CCGR_SETID(15) TYPE C VALUE 'HMMA1'.

* CCGroup For Engin CCtrs
DATA: GV_CCGR_SETID2(15) TYPE C VALUE 'HMMA2'.

* CCGroup For ALL
DATA: GV_CCGR_SETID_ALL(15) TYPE C VALUE 'HMMA'.

DATA : IT_NODES  TYPE GSETH_NODE_TAB
                 WITH HEADER LINE ,
       IT_VALUES TYPE GSETH_VAL_TAB
                 WITH HEADER LINE .
