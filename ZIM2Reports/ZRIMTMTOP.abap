*----------------------------------------------------------------------*
*   INCLUDE ZRIMTMTOP                                                  *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*   TABLE Declare                                                      *
*----------------------------------------------------------------------*
TABLES : ZTBKPF,             " Import Cost Header.
         ZTBDIV,             " Import Cost Distribution of P/O
         ZTREQHD,            " Import Request Header Table
         ZTREQIT,            " Import Request Item Table
         ZTBL,               " B/L Header Table
         ZTBLIT,             " B/L Item Table
         ZTSETAC,            " Settled Account Management Table
         EKPO,               " P/O Item Table
         EKBE,               " G/R History Table
         EKBZ,               " G/R History Item Table.
         T685T,              " Conditions: Types: Texts.
         MBEW,               " Material Valuation
         USR01,              " User master record (runtime data)
         T001,               " Company Code
         T001W,              " Plants/Branches
         T001K,              " Valuation area
         T030,               " Standard Accounts Table
         ZTIMIMG11,          " Account No. Management Table
         ZTIMIMG08,          " Code Management
         ZTIMIMG00.          " Basic Config.

DATA : W_TABIX         LIKE  SY-TABIX,
       W_DATE          LIKE  SY-DATUM,
       W_YM(6)         TYPE  C,
       W_IMDNO         LIKE  ZTBDIV-ZFIMDNO,
       W_BELNR         LIKE  EKBZ-BELNR,
       W_CHK           TYPE  C,
       W_LINE          TYPE  I,
       W_MOD           TYPE  I,
       L_MENGE(17)     TYPE  C,
       L_MEINS(5)      TYPE  C,
       W_ERR_CHK(1)    TYPE  C,
       W_SET_QTY       LIKE  EKPO-MENGE,
       W_TO_QTY        LIKE  EKPO-MENGE,
       P_BUKRS         LIKE  ZTBKPF-BUKRS,
       W_TEMP_AMT      LIKE  EKBZ-DMBTR,
       W_MINUS_AMOUNT  LIKE  EKBZ-DMBTR,
       W_PRICE_AMOUNT  LIKE  EKBZ-DMBTR,
       W_COUNT         TYPE  I,
       W_ACT_ACC       LIKE  ZTBDIV-AKONT,
       W_PLD_ACC       LIKE  ZTBDIV-AKONT,
       W_CO_ACC        LIKE  ZTBDIV-AKONT,
       W_PRI_ACC       LIKE  ZTBDIV-AKONT,
       W_TEXT(18)      TYPE  C,
       W_CHAR_DATE(8)  TYPE  C,
       W_BUDAT         LIKE  SY-DATUM,
       W_FIRST_DATE    LIKE  SY-DATUM,
       W_LAST_DATE     LIKE  SY-DATUM,
       W_BEFORE_FIRST  LIKE  SY-DATUM,
       W_BEFORE_END    LIKE  SY-DATUM,
       W_END_DATE      LIKE  SY-DATUM,
       W_INIT_DATE     LIKE  SY-DATUM,
       W_EBELN         LIKE  EKPO-EBELN,
       W_EBELP         LIKE  EKPO-EBELP,
       W_DAY(2)        TYPE  C,
       W_LCNO(20)      TYPE  C,
       W_BLNO(20)      TYPE  C,
       TEMP_WRBTR(16)  TYPE  C,
       W_MAX_SEQ       LIKE  ZTSETAC-ZFSEQ,
       W_GJAHR         LIKE  BKPF-GJAHR,
       W_MODE          TYPE  C VALUE 'E',
       INCLUDE(8)      TYPE  C.

* Total Line Variable.
DATA : W_TOT_ACTUAL    LIKE  EKBZ-DMBTR,
       W_TOT_PLAND     LIKE  EKBZ-DMBTR,
       W_TOT_CHA       LIKE  EKBZ-DMBTR,
       W_SET_ACTUAL    LIKE  EKBZ-DMBTR,
       W_SET_PLAND     LIKE  EKBZ-DMBTR,
       W_TSET_ACTUAL   LIKE  EKBZ-DMBTR,
       W_TSET_PLAND    LIKE  EKBZ-DMBTR,
       W_NSET          LIKE  EKBZ-DMBTR.

*-----------------------------------------------------------------------
* Internal Table Define
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_PO OCCURS 0,
   EBELN    LIKE     ZTREQIT-EBELN,
   EBELP    LIKE     ZTREQIT-EBELP,
END OF IT_PO.

DATA: BEGIN OF IT_BELNR OCCURS 0,
   GJAHR    LIKE     BSEG-GJAHR,
   BELNR    LIKE     BSEG-BELNR,
   BUZEI    LIKE     BSEG-BUZEI,
   BUDAT    LIKE     BKPF-BUDAT,
END OF IT_BELNR.

DATA : BEGIN OF IT_TEMP OCCURS 0,
       EBELN           LIKE   EKBZ-EBELN,
       EBELP           LIKE   EKBZ-EBELP,
       BEWTP           LIKE   EKBE-BEWTP,
       VGABE           LIKE   EKBE-VGABE,
       SHKZG           LIKE   EKBE-SHKZG,
       BUDAT           LIKE   EKBZ-BUDAT,
       MENGE           LIKE   EKBE-MENGE,
       DMBTR           LIKE   EKBE-DMBTR,
       LC_ACT          LIKE   EKBE-DMBTR,
       BL_ACT          LIKE   EKBE-DMBTR,
       LC_PLD          LIKE   EKBE-DMBTR,
       BL_PLD          LIKE   EKBE-DMBTR.
DATA : END   OF  IT_TEMP.

DATA : BEGIN OF IT_TAB OCCURS 0,
       EBELN           LIKE   EKBZ-EBELN,
       EBELP           LIKE   EKBZ-EBELP,
       MATNR           LIKE   EKPO-MATNR,
       MAKTX           LIKE   EKPO-TXZ01,
       PO_QTY          LIKE   EKPO-MENGE,
       GR_QTY          LIKE   EKPO-MENGE,
       MEINS           LIKE   EKPO-MEINS,
       WAERS           LIKE   ZTBDIV-WAERS,
       IV_LAST         LIKE   EKBZ-DMBTR,
       IV_THIS         LIKE   EKBZ-DMBTR,
       GR_LAST         LIKE   EKBZ-DMBTR,
       GR_THIS         LIKE   EKBZ-DMBTR,
       SETTLED         LIKE   EKBZ-DMBTR,
       BALANCE         LIKE   EKBZ-DMBTR.
DATA : END   OF  IT_TAB.

*>> P/O Sum Internal Table
DATA : BEGIN OF IT_TAB_PO OCCURS 0,
       EBELN           LIKE   EKBZ-EBELN,
       EBELP           LIKE   EKBZ-EBELP,
       MATNR           LIKE   EKPO-MATNR,
       MAKTX           LIKE   MAKT-MAKTX,
       PO_QTY          LIKE   EKPO-MENGE,
       GR_QTY          LIKE   EKPO-MENGE,
       MEINS           LIKE   EKPO-MEINS,
       WAERS           LIKE   ZTBDIV-WAERS,
       IV_LAST         LIKE   EKBZ-DMBTR,
       IV_THIS         LIKE   EKBZ-DMBTR,
       GR_LAST         LIKE   EKBZ-DMBTR,
       GR_THIS         LIKE   EKBZ-DMBTR,
       SETTLED         LIKE   EKBZ-DMBTR,
       BALANCE         LIKE   EKBZ-DMBTR.
DATA : END   OF  IT_TAB_PO.

*>> P/O Sum Internal Table
DATA : BEGIN OF IT_TAB_TOTAL OCCURS 0,
       EBELN           LIKE   EKBZ-EBELN,
       EBELP           LIKE   EKBZ-EBELP,
       MATNR           LIKE   EKPO-MATNR,
       MAKTX           LIKE   MAKT-MAKTX,
       PO_QTY          LIKE   EKPO-MENGE,
       GR_QTY          LIKE   EKPO-MENGE,
       MEINS           LIKE   EKPO-MEINS,
       WAERS           LIKE   ZTBDIV-WAERS,
       IV_LAST         LIKE   EKBZ-DMBTR,
       IV_THIS         LIKE   EKBZ-DMBTR,
       GR_LAST         LIKE   EKBZ-DMBTR,
       GR_THIS         LIKE   EKBZ-DMBTR,
       SETTLED         LIKE   EKBZ-DMBTR,
       BALANCE         LIKE   EKBZ-DMBTR.
DATA : END   OF  IT_TAB_TOTAL.

* Message Table.
DATA:   BEGIN OF RETURN OCCURS 0.
       INCLUDE  STRUCTURE  BDCMSGCOLL.
       DATA : ICON       LIKE BAL_S_DMSG-%_ICON,
              MESSTXT(255) TYPE C.
DATA:   END   OF RETURN.

RANGES : R_ACCOUNT FOR  BSAS-HKONT  OCCURS 2.
