*&---------------------------------------------------------------------*
*& Report  ZRIMLDCST                                                   *
*&---------------------------------------------------------------------*
*&  Program : LDC Rate Management Status.
*&     Name : SH, Na INFOLINK Ltd.                                     *
*&     Date : 2004.02.13                                               *
*&---------------------------------------------------------------------*
*&    Desc. :                                                          *
*&---------------------------------------------------------------------*
REPORT  ZRIMTEMP    MESSAGE-ID ZIM
                    LINE-SIZE 124
                    NO STANDARD PAGE HEADING.
*-----------------------------------------------------------------------
* Tables & Variable Define
*-----------------------------------------------------------------------
INCLUDE: <ICON>.
TABLES : ZTBKPF, ZTBSEG, ZTBDIV, ZTBHIS,
         T163B, T163C, T685T, LFA1,
         EKBZ, EKBE, KONV, EKKO, EKPO,
         ZTIMIMG01, ZTIMIMG00, ZTIMIMG08,
         ZTREQHD, ZTREQIT, RV61A.

TYPE-POOLS : SLIS.

*SELECT SINGLE *
*         FROM EKKO
*        WHERE EBELN = '4200000108'.
*  MOVE 'N005' TO EKKO-LIFNR.
*  UPDATE EKKO.

SELECT SINGLE *
         FROM ZTREQHD
        WHERE EBELN = '4200000090'.
  MOVE 'A' TO ZTREQHD-ZFTRANS.
  UPDATE ZTREQHD.
