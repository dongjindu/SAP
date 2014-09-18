*----------------------------------------------------------------------*
*   INCLUDE ZRIMINTOP                                                  *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*   TABLE 선언                                                         *
*----------------------------------------------------------------------*
TABLES : ZTBKPF,               " 비용문서 HEADER
         ZTBSEG,               " 비용문서 ITEM
         ZTBDIV,               " 비용 배부 내역.
         ZTBHIS,               " 비용 HISTORY
         ZTIMIMG00 ,
         ZTIMIMG08 ,
         ZTREQHD   ,
         ZTREQIT   ,
         EKKO,
         MAKT,
         ZTIV,
         ZTIVHST,
         ZTIVIT,
         ZTIVHSTIT,
         RV61A.

*----------------------------------------------------------------------*
*   INTERNAL TABLE 선언                                                *
*----------------------------------------------------------------------*
DATA : BEGIN OF IT_REQ OCCURS 0,
       EBELN           LIKE   ZTREQIT-EBELN,
       EBELP           LIKE   ZTREQIT-EBELP,
       MENGE           LIKE   ZTREQIT-MENGE,
       MEINS           LIKE   ZTREQIT-MEINS,
       GRMENGE         LIKE   ZTREQIT-MENGE.
DATA : END   OF IT_REQ.

DATA : BEGIN OF   IT_TAB_AMT OCCURS 0,
       EBELN      LIKE EKBZ-EBELN,           " PO.
       EBELP      LIKE EKBZ-EBELP,           " PO ITEM.
       ZFCSTGRP   LIKE ZTBDIV-ZFCSTGRP,      " 비용그룹.
       ZFCD       LIKE ZTBDIV-ZFCD,          " 비용코드.
       ZFCDNM     LIKE ZTIMIMG08-ZFCDNM,     " 비용명.
       MATNR      LIKE ZTBDIV-MATNR,         " 자재.
       MAKTX      LIKE MAKT-MAKTX,           " 자재명.
       MENGE      LIKE ZTBDIV-MENGE,         " 비용수량.
       GRMENGE    LIKE ZTIVHSTIT-GRMENGE,    " 입고수량.
       MEINS      LIKE ZTBDIV-MEINS,         " 수량단위.
       ZFAMT      LIKE ZTBDIV-ZFAMT,         " 발생비용금액.
       WAERS      LIKE ZTBDIV-WAERS,         " 통화.
       INAMT      LIKE ZTBDIV-ZFAMT.         " 미착금액.
DATA : END OF IT_TAB_AMT.

DATA : BEGIN OF   IT_TAB_SUM OCCURS 0,
       MATNR      LIKE ZTBDIV-MATNR,         " 자재.
       MAKTX      LIKE MAKT-MAKTX,           " 자재명.
       ZFCSTGRP   LIKE ZTBDIV-ZFCSTGRP,      " 비용그룹.
       ZFCD       LIKE ZTBDIV-ZFCD,          " 비용코드.
       ZFCDNM     LIKE ZTIMIMG08-ZFCDNM,     " 비용명.
       WAERS      LIKE ZTBDIV-WAERS,         " 통화.
       INAMT      LIKE ZTBDIV-ZFAMT.         " 미착금액.
DATA : END OF IT_TAB_SUM.

DATA : BEGIN OF   IT_TAB_TOTAL OCCURS 0,
       MATNR      LIKE ZTBDIV-MATNR,         " 자재.
       MAKTX      LIKE MAKT-MAKTX,           " 자재명.
       ZFCSTGRP   LIKE ZTBDIV-ZFCSTGRP,      " 비용그룹.
       ZFCD       LIKE ZTBDIV-ZFCD,          " 비용코드.
       ZFCDNM     LIKE ZTIMIMG08-ZFCDNM,     " 비용명.
       WAERS      LIKE ZTBDIV-WAERS,         " 통화.
       INAMT      LIKE ZTBDIV-ZFAMT.         " 미착금액.
DATA : END OF IT_TAB_TOTAL.

*----------------------------------------------------------------------*
*   VARIABLE 선언                                                      *
*----------------------------------------------------------------------*
DATA : W_ERR_CHK         TYPE C,
       W_TABIX           LIKE SY-TABIX,
       W_LINE            TYPE I,
       SV_CHK            TYPE C,
       SV_EBELN          LIKE EKPO-EBELN,
       SV_EBELP          LIKE EKPO-EBELP,
       W_MENGE           LIKE ZTIVHSTIT-GRMENGE,
       SV_AMT            LIKE ZTBDIV-ZFAMT,
       TOT_AMT           LIKE ZTBDIV-ZFAMT,
       SV_WAERS          LIKE ZTBDIV-WAERS,
       W_COUNT           TYPE I,
       SV_MATNR          LIKE ZTBDIV-MATNR,
       SV_MAKTX          LIKE MAKT-MAKTX,
       SV_CDNM           LIKE ZTIMIMG08-ZFCDNM,
       SV_CSTGRP         LIKE ZTBDIV-ZFCSTGRP,
       SV_ZFCD           LIKE ZTBDIV-ZFCD,
       W_CNT             TYPE I,
       W_MOD             TYPE I,
       SV_TOT            LIKE ZTBDIV-ZFAMT.
