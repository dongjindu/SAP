*&---------------------------------------------------------------------*
*& Module pool       SAPMZIM10                                         *
*&---------------------------------------------------------------------*
*& Program Description  : Customs Clearance Module Pool Program
*& Created by           : Na Hyun Joo INFOLINK Ltd.
*& Created on           : 2003.10.08
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
PROGRAM  SAPMZIM10     MESSAGE-ID    ZIM
                       NO STANDARD PAGE HEADING.

INCLUDE <ICON>.
*-----------------------------------------------------------------------
* BDC
*-----------------------------------------------------------------------
INCLUDE ZRIMBDCCOM.  " BDC Common INCLUDE

*-----------------------------------------------------------------------
* DATA DEFINE
*-----------------------------------------------------------------------
INCLUDE ZRIM00TOP.    " Import System Main Data Define Include
INCLUDE ZRIM10T01.    " Import Customs Clearance Scrren Define

*-----------------------------------------------------------------------
* Process Before Output
*-----------------------------------------------------------------------
INCLUDE ZRIM10O01.    " PBO Module Include

*-----------------------------------------------------------------------
* Process After Input
*-----------------------------------------------------------------------
INCLUDE ZRIM10I01.    " PAI Module Include

*-----------------------------------------------------------------------
* Sub MODULE Include
*-----------------------------------------------------------------------
INCLUDE ZRIM10F01.    " Sub Module Include

INCLUDE ZRIM10E01.    " Event Module Include          NCW Insert
