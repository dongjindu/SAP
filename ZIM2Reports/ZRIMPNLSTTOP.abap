*----------------------------------------------------------------------*
*   INCLUDE ZRIMPNLSTTOP                                               *
*----------------------------------------------------------------------*
TABLES : ZTPMTHD,
         LFA1,
         EKKO,
         T024.

TYPE-POOLS : SLIS.

DATA: W_ERR_CHK    TYPE C VALUE 'N',   " Error Check.
      W_LINE       TYPE I,               " IT_TAB Line Count.
      W_MOD        LIKE SY-TABIX,          " Odd or Even.
      W_TABIX      LIKE SY-TABIX.
DATA: GT_FIELDCAT  TYPE SLIS_T_FIELDCAT_ALV.
DATA: G_REPID      LIKE SY-REPID.
DATA: G_LAYOUT     TYPE SLIS_LAYOUT_ALV.
DATA: LS_FIELDCAT  TYPE SLIS_FIELDCAT_ALV.
DATA: POS          TYPE I.

* Internal Table Declaration.
DATA: BEGIN OF     IT_TAB OCCURS 0.
        INCLUDE    STRUCTURE ZTPMTHD.
  DATA: NAME1      LIKE LFA1-NAME1,
        EKTEL      LIKE T024-EKTEL,
        ZFPNAMK    LIKE ZTPMTHD-ZFPNAM,
      END OF       IT_TAB.
