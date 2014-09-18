*----------------------------------------------------------------------*
*   INCLUDE ZRIMCSTCHATOP                                              *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*   TABLE 선언                                                         *
*----------------------------------------------------------------------*
TABLES : ZTBKPF,               " 비용문서 HEADER
         ZTBSEG,               " 비용문서 ITEM
         ZTBDIV,               " 비용 배부 내역.
         ZTBHIS,               " 비용 HISTORY
         T163B ,               " TEXT
         T163C ,               " TEXT
         T685T ,               " TEXT
         EKBZ  ,               " 비용 회계 전표
         EKBE  ,               " 물대 회계 전표
         KONV  ,
         EKKO  ,
         EKPO  ,
         ZTIMIMG01 ,
         ZTIMIMG00 ,
         ZTIMIMG08 ,
         ZTREQHD   ,
         ZTREQIT   ,
         RV61A.

*----------------------------------------------------------------------*
*   INTERNAL TABLE 선언                                                *
*----------------------------------------------------------------------*
DATA : BEGIN OF IT_REQ OCCURS 0,
       EBELN           LIKE   ZTREQIT-EBELN,
       EBELP           LIKE   ZTREQIT-EBELP.
DATA : END   OF IT_REQ.

DATA : BEGIN OF IT_PO OCCURS 0,
       EBELN           LIKE   EKPO-EBELN,
       EBELP           LIKE   EKPO-EBELP,
       WERKS           LIKE   EKPO-WERKS,
       MEINS           LIKE   EKPO-MEINS,
       BUKRS           LIKE   EKKO-BUKRS.
DATA : END   OF IT_PO.

DATA : BEGIN OF IT_DIV OCCURS 0,
       EBELN           LIKE   ZTBDIV-EBELN,      " PO Header.
       EBELP           LIKE   ZTBDIV-EBELP,      " PO Item.
       ZFRVSX          LIKE   ZTBKPF-ZFRVSX,     " 역기표.
       BUKRS           LIKE   ZTBDIV-BUKRS,      " 회사코드.
       ZFACDO          LIKE   ZTBKPF-ZFACDO,     " 전표번호.
       ZFFIYR          LIKE   ZTBKPF-ZFFIYR,     " 전표년도.
       ZFDCSTX         LIKE   ZTBDIV-ZFDCSTX,    " Delivery Cost 여부.
       ZFCSTGRP        LIKE   ZTBDIV-ZFCSTGRP,   " 비용그룹.
       ZFCD            LIKE   ZTBDIV-ZFCD,       " 비용코드.
       WRBTR           LIKE   ZTBDIV-WRBTR,      " 금액.
       DMBTR           LIKE   ZTBDIV-DMBTR,      " 원화비용.
       HWAER           LIKE   ZTBDIV-HWAER,      " 통화.
       MENGE           LIKE   ZTBDIV-MENGE,      " 수량.
       MEINS           LIKE   ZTBDIV-MEINS.      " 단위.
DATA : END   OF IT_DIV.

DATA : BEGIN OF IT_EKBZ OCCURS 0,
       EBELN           LIKE   EKBZ-EBELN,        " PO Header
       EBELP           LIKE   EKBZ-EBELP,        " PO Item
       VGABE           LIKE   EKBZ-VGABE,        " Transaction 유형.
       GJAHR           LIKE   EKBZ-GJAHR,        " 전표년도.
       BELNR           LIKE   EKBZ-BELNR,        " 전표번호.
       BEWTP           LIKE   EKBZ-BEWTP,        " PO 이력종류.
       DMBTR           LIKE   EKBZ-DMBTR,        " 현지통화금액.
       HSWAE           LIKE   EKBZ-HSWAE,        " 통화.
       MENGE           LIKE   EKBZ-MENGE,        " 수량.
       MEINS           LIKE   EKPO-MEINS,        " 단위.
       SHKZG           LIKE   EKBZ-SHKZG,        " 대변/차변 지시자.
       KSCHL           LIKE   EKBZ-KSCHL.
DATA : END   OF IT_EKBZ.

DATA : BEGIN OF IT_EKBE  OCCURS 0,
       EBELN           LIKE   EKBE-EBELN,        " PO Header
       EBELP           LIKE   EKBE-EBELP,        " PO Item
       VGABE           LIKE   EKBE-VGABE,        " Transaction 유형.
       GJAHR           LIKE   EKBE-GJAHR,        " 전표년도.
       BELNR           LIKE   EKBE-BELNR,        " 전표번호.
       BEWTP           LIKE   EKBE-BEWTP,        " PO 이력종류.
       DMBTR           LIKE   EKBE-DMBTR,        " 현지통화금액.
       HSWAE           LIKE   EKBE-HSWAE,        " 통화.
       MENGE           LIKE   EKBE-MENGE,        " 수량.
       MEINS           LIKE   EKPO-MEINS,        " 단위.
       SHKZG           LIKE   EKBE-SHKZG.        " 대변/차변 지시자.
DATA : END   OF IT_EKBE.

*----------------------------------------------------------------------*
*   VARIABLE 선언                                                      *
*----------------------------------------------------------------------*
DATA : W_ERR_CHK         TYPE C,
       W_COUNT           TYPE I,
       W_LINE            TYPE I,
       SV_SORT           TYPE C,
       SV_CHK            TYPE C,
       INCLUDE(8)        TYPE C,
       SV_TEXT(20)       TYPE C,
       SV_TOT1           LIKE EKBZ-DMBTR,
       SV_TOT2           LIKE EKBZ-DMBTR,
       SV_TOT3           LIKE EKBZ-DMBTR,
       SV_PER1(5)        TYPE P  DECIMALS 5,
       SV_PER2(5)        TYPE P  DECIMALS 5,
       SV_PER3(5)        TYPE P  DECIMALS 5,
       SV_KSCHL          LIKE EKBZ-KSCHL,
       SV_MENGE          LIKE EKPO-MENGE,
       SV_DMBTR          LIKE EKBZ-DMBTR,
       SV_NAME           LIKE T001W-NAME1,
       SV_KEY            LIKE KONV-WAERS,
       SV_SUM            LIKE EKBZ-DMBTR,
       SV_SUM1           LIKE EKBZ-DMBTR,
       SV_SUM2           LIKE EKBZ-DMBTR,
       SV_SUM3           LIKE EKBZ-DMBTR,
       SV_SUM4           LIKE EKBZ-DMBTR,
       SV_CHA            LIKE EKBZ-DMBTR,
       SV_EBELN          LIKE EKPO-EBELN,
       SV_EBELP          LIKE EKPO-EBELP,
       SV_MEINS          LIKE EKPO-MEINS,
       SV_WAERS          LIKE EKBZ-WAERS,
       SV_KNUMV          LIKE EKKO-KNUMV.
