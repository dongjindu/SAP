*&---------------------------------------------------------------------*
*& Module pool       SAPMZIM03                                         *
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입관세/부가세 Main Module Pool Program              *
*&      작성자 : 이 채 경 INFOLINK Ltd.                                *
*&      작성일 : 2001.06.09                                            *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
PROGRAM  SAPMZIM03     MESSAGE-ID    ZIM
                       NO STANDARD PAGE HEADING.

*-----------------------------------------------------------------------
* Data Define Include
*-----------------------------------------------------------------------
INCLUDE <ICON>.
INCLUDE <SYMBOL>.
INCLUDE ZRIM00TOP.    " 수입 System Main Data Define Include
INCLUDE ZRIM03TOP.
INCLUDE ZRIMBDCCOM.   " 수입의뢰 BDC 공통 Include
INCLUDE ZRIM03O01.
INCLUDE ZRIM03F01.
INCLUDE ZRIM03I01.
