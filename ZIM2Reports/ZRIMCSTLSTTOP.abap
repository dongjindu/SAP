*----------------------------------------------------------------------*
*   INCLUDE ZRIMCSTLSTTOP                                              *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*   TABLE Declare                                                      *
*----------------------------------------------------------------------*
TABLES : ZTBKPF,               " Charge Document Header
         ZTBSEG,               " Charge Document Item
         ZTBDIV,               " Charge Document Distribute
         ZTBHIS,               " Charge Document History
         T001,                 " Company Codes
         T163B ,               " TEXT
         T163C ,               " TEXT
         T685T ,               " TEXT
         EKBZ  ,               " History per Purchasing Document: Delive
         EKBE  ,               " History per Purchasing Document
         KONV  ,               " Conditions (Transaction Data)
         EKKO  ,               " Purchasing Header
         EKPO  ,               " Purchasing Item
         ZTIMIMG01 ,           " Payment Term Configuration
         ZTIMIMG00 ,           " Import Module Basic Configuration
         ZTIMIMG08 ,           " Code Management
         ZTREQHD   ,           " Import Request Header
         ZTREQIT   ,           " Import Request Item
         RV61A.                " Input/Output Fields for SAPMV61A

*----------------------------------------------------------------------*
*   INTERNAL TABLE Declare                                             *
*----------------------------------------------------------------------*
DATA : BEGIN OF IT_REQ OCCURS 0,
       EBELN           LIKE   ZTREQIT-EBELN,    " PO Header
       EBELP           LIKE   ZTREQIT-EBELP.    " PO Item
DATA : END   OF IT_REQ.

DATA : BEGIN OF IT_PO OCCURS 0,
       EBELN           LIKE   EKPO-EBELN,       " PO Header
       EBELP           LIKE   EKPO-EBELP,       " PO Item
       MATNR           LIKE   EKPO-MATNR,       " Material
       TXZ01           LIKE   EKPO-TXZ01,       " Material Text
       WERKS           LIKE   EKPO-WERKS,       " Plant
       MENGE           LIKE   EKPO-MENGE,       " P/O Quantity
       MEINS           LIKE   EKPO-MEINS,       " Unit of measure
       BUKRS           LIKE   EKKO-BUKRS.       " Company Code
DATA : END   OF IT_PO.

DATA : BEGIN OF IT_PO_TMP OCCURS 0,
       EBELN           LIKE   EKPO-EBELN.       " PO Header
DATA : END   OF IT_PO_TMP.

DATA : BEGIN OF IT_DIV OCCURS 0,
       EBELN           LIKE   ZTBDIV-EBELN,      " PO Header.
       EBELP           LIKE   ZTBDIV-EBELP,      " PO Item.
       ZFRVSX          LIKE   ZTBKPF-ZFRVSX,     " Reverse Yes/No.
       BUKRS           LIKE   ZTBDIV-BUKRS,      " Company Code
       ZFACDO          LIKE   ZTBKPF-ZFACDO,     " FI Document No
       ZFFIYR          LIKE   ZTBKPF-ZFFIYR,     " FI Document Year
       ZFDCSTX         LIKE   ZTBDIV-ZFDCSTX,    " Delivery Cost Yes/No
       ZFCSTGRP        LIKE   ZTBDIV-ZFCSTGRP,   " Expense Group
       ZFCD            LIKE   ZTBDIV-ZFCD,       " Expense Code
       WRBTR           LIKE   ZTBDIV-WRBTR,      " Amount
       DMBTR           LIKE   ZTBDIV-DMBTR,      " Local Amount
       HWAER           LIKE   ZTBDIV-HWAER,      " Currency
       MENGE           LIKE   ZTBDIV-MENGE,      " Quantity
       MEINS           LIKE   ZTBDIV-MEINS.      " Unit of measure
DATA : END   OF IT_DIV.

DATA : BEGIN OF IT_EKBZ OCCURS 0,
       EBELN           LIKE   EKBZ-EBELN,        " PO Header
       EBELP           LIKE   EKBZ-EBELP,        " PO Item
       VGABE           LIKE   EKBZ-VGABE,        " Transaction Type
       GJAHR           LIKE   EKBZ-GJAHR,        " MM Document Year
       BELNR           LIKE   EKBZ-BELNR,        " MM Document No
       BEWTP           LIKE   EKBZ-BEWTP,        " PO History Type
       DMBTR           LIKE   EKBZ-DMBTR,        " Local Amount
       HSWAE           LIKE   EKBZ-HSWAE,        " Currency
       MENGE           LIKE   EKBZ-MENGE,        " Quantity
       MEINS           LIKE   EKPO-MEINS,        " Unit of measure
       SHKZG           LIKE   EKBZ-SHKZG,        " Credit/Debit
       KSCHL           LIKE   EKBZ-KSCHL.        " Condition
DATA : END   OF IT_EKBZ.

DATA : BEGIN OF IT_EKBE  OCCURS 0,
       EBELN           LIKE   EKBE-EBELN,        " PO Header
       EBELP           LIKE   EKBE-EBELP,        " PO Item
       VGABE           LIKE   EKBE-VGABE,        " Transaction Type
       GJAHR           LIKE   EKBE-GJAHR,        " MM Document Year
       BELNR           LIKE   EKBE-BELNR,        " MM Document No
       BEWTP           LIKE   EKBE-BEWTP,        " PO History Type
       DMBTR           LIKE   EKBE-DMBTR,        " Local Amount
       HSWAE           LIKE   EKBE-HSWAE,        " Currency
       WRBTR           LIKE   EKBE-WRBTR,        " Amount
       WAERS           LIKE   EKBE-WAERS,        " Document Currency
       MENGE           LIKE   EKBE-MENGE,        " Quantity
       MEINS           LIKE   EKPO-MEINS,        " Unit of measure
       SHKZG           LIKE   EKBE-SHKZG.        " Credit/Debit
DATA : END   OF IT_EKBE.

*----------------------------------------------------------------------*
*   VARIABLE Declare                                                   *
*----------------------------------------------------------------------*
DATA : P_BUKRS         LIKE   ZTBKPF-BUKRS,
       W_ERR_CHK       TYPE   C,
       W_COUNT         TYPE   I,
       W_LINE          TYPE   I,
       W_MOD           TYPE   I,
       INCLUDE(8)      TYPE   C,
       W_TOT_ACT       LIKE   EKBZ-DMBTR,
       W_TOT_PLAN      LIKE   EKBZ-DMBTR,
       W_TOT_BALA      LIKE   EKBZ-DMBTR,
       W_CURRENCY      LIKE   T001-WAERS,
       W_GR_QTY        LIKE   EKPO-MENGE,
       W_PO            LIKE   EKPO-EBELN,
       W_EBELP         LIKE   EKPO-EBELP,
       W_GUBUN(6)      TYPE   C,
       W_CHECK         TYPE   C.
