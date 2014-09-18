*----------------------------------------------------------------------*
*   INCLUDE ZISD10U_MOBIS_INV_TOP                                      *
*----------------------------------------------------------------------*
TABLES : VBRK, VBRP, LIKP, LIPS, VBFA, VBAK, KONV,
         NAST, VBAP.


DATA : BEGIN OF IT_HEADER OCCURS 0,
       RECORD(26),
       END OF IT_HEADER.

DATA : BEGIN OF IT_DOWNFILE OCCURS 0,
       RECORD(149),
       END OF IT_DOWNFILE.

DATA : W_NUMC_GI(7) TYPE N,
       W_NUMC_IV(7) TYPE N,
       W_NUMC_UP(9) TYPE N,
       W_NUMC_TX(9) TYPE N,
       W_NUMC_FC(9) TYPE N,
       W_NUMC_OA(9) TYPE N,
       W_NUMC_TA(11) TYPE N,
       W_KWERT LIKE KONV-KWERT,
       W_CHAR_9(9),
       W_DSN(90). " VALUE '/usr/sap/EDI_SAP/'.

data: begin of it_file occurs 0,
        VBELN     LIKE VBRK-VBELN,    "BILLING DOC NO
        ERDAT     LIKE VBRK-ERDAT,    "BILLING DATE
        BSTNK     LIKE VBAK-BSTNK,    "ORDER NUMBER
        BSTKD     LIKE VBKD-BSTKD,    "P.O ITEM NO--ONLY FOR MOBIS
        AUPOS     LIKE VBRP-AUPOS,    "ITEM NUMBER
        MATNR     LIKE VBAP-KDMAT,    "MATERIAL
        LFIMG(7)  TYPE P DECIMALS 0, "LIKE LIPS-LFIMG,   "DELIVIED QTY
        FKIMG(7)  TYPE P DECIMALS 0, "LIKE VBRP-FKIMG,   "INVOICE QTY
        WAERK     LIKE VBRK-WAERK,    "CURRENCY
        INCST(9)  TYPE P DECIMALS 2, "LIKE VBRK-MWSBK,   "INSURANCE COST
        FRCST(9)  TYPE P DECIMALS 2, "LIKE VBRK-MWSBK,   "FREIGHT COST
        MWSBK(9)  TYPE P DECIMALS 2, "LIKE VBRK-MWSBK,   "TAX AMOUNT
        NETPR(9)  TYPE P DECIMALS 2, "LIKE VBRP-CMPRE,   "unit price
        NETWR(11) TYPE P DECIMALS 2, "LIKE VBRK-NETWR,   "TOTAL AMOUNT
        NTGEW(7)  TYPE P DECIMALS 3, "LIKE VBRP-NTGEW,   "NET WEIGHT
        AUBEL     LIKE VBRP-AUBEL,    "SALES ORDER
      end of it_file.
data: wa_file like it_file,
      wa_filet like it_file,
      L_TABIX  LIKE SY-TABIX.
data: begin of it_out occurs 0,
        VBELN(10)              ,    "BILLING DOC NO
        ERDAT(08)              ,    "BILLING DATE
        BSTNK(10)              ,    "ORDER NUMBER
        BSTKD(04)              ,    "ITEM NUMBER
        MATNR(18)              ,    "MATERIAL
        LFIMG(07)              ,    "DELIVIED QTY
        FKIMG(07)              ,    "INVOICE QTY
        WAERK(03)              ,    "CURRENCY
        INCST(09)              ,    "INSURANCE COST
        FRCST(09)              ,    "FREIGHT COST
        MWSBK(09)              ,    "TAX AMOUNT
        NETPR(09)              ,    "UNIT PRICE
        NETWR(11)              ,    "TOTAL AMOUNT
        NTGEW(07)              ,    "NET WEIGHT
      end of it_out.

 DATA: BEGIN OF LT_DEL OCCURS 0,
          VBELN   LIKE VBRP-VBELN,
          VBELV   LIKE VBFA-VBELV,
          POSNV   LIKE VBFA-POSNV,
          RFMNG   LIKE VBFA-RFMNG,
        END OF LT_DEL.
  DATA: L_DELQTY  LIKE VBFA-RFMNG.
