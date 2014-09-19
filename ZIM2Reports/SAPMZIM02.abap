*&---------------------------------------------------------------------*
*& Module pool       SAPMZIM02                                         *
*&---------------------------------------------------------------------*
*&  Program Name : Import Expense Management Main Module Pool Program  *
*&  Created by   : Kang Suk Bong INFOLINK Ltd.                         *
*&  Created on   : 2001.05.14                                          *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
PROGRAM  SAPMZIM02     MESSAGE-ID    ZIM
                       NO STANDARD PAGE HEADING.

*-----------------------------------------------------------------------
* Data Define Include
*-----------------------------------------------------------------------
INCLUDE <ICON>.
INCLUDE <SYMBOL>.
INCLUDE ZRIM00TOP.
INCLUDE ZRIM02TOP.
INCLUDE ZRIMBDCCOM.

*-----------------------------------------------------------------------
* PBO Module
*-----------------------------------------------------------------------
INCLUDE ZRIM02O01.

*-----------------------------------------------------------------------
* PAI MODULE
*-----------------------------------------------------------------------
INCLUDE ZRIM02I01.

INCLUDE ZRIM02F01.