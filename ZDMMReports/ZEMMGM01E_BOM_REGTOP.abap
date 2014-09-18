************************************************************************
* Program name : ZEMMGM01E_BOM_REG                                     *
* Created by   : Min-su Park                                           *
* Created on   : 2003.11.10.                                           *
* Pattern      :                                                       *
* Description  : BOM Registration Request Program                      *
* Modification Log                                                     *
* Date            Developer        Request No.    Description          *
* 2003.11.10.     Min-su Park      UD1K901873     Initial Coding       *
*                                                                      *
************************************************************************
*&---------------------------------------------------------------------*
*& Include ZEMMGM01E_BOM_REGTOP                                        *
*&                                                                     *
*&---------------------------------------------------------------------*

PROGRAM  ZEMMGM01E_BOM_REG MESSAGE-ID ZMMM .
TABLES : ZTMM_END_PART.

*Screen
CONTROLS TC_END_PART TYPE TABLEVIEW USING SCREEN 100.
DATA : OK_CODE LIKE SY-UCOMM  ,
       W_FCODE LIKE SY-UCOMM  ,
       W_LOOPC LIKE SY-LOOPC  .

DATA:  W_NUMBER(4) TYPE n VALUE '1100'.

*Internal Table
DATA : BEGIN OF IT_END_PART OCCURS 0.
        INCLUDE STRUCTURE ZTMM_END_PART.
DATA :  SUB_PART(18)                 ,
        STEEL                        ,
        MARK                         ,
       END OF IT_END_PART            .

*General Variable
DATA : W_SEL TYPE C VALUE 'X',
       WA_END_PART LIKE ZTMM_END_PART.

*Selection Screen
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-001.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 5.
PARAMETERS : R1 RADIOBUTTON GROUP RADI DEFAULT 'X'.
SELECTION-SCREEN POSITION 7.
SELECTION-SCREEN COMMENT 8(20) text-002 FOR FIELD R1.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 5.
PARAMETERS : R2 RADIOBUTTON GROUP RADI .
SELECTION-SCREEN POSITION 7.
SELECTION-SCREEN COMMENT 8(20) text-003 FOR FIELD R2.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 5.
PARAMETERS : R3 RADIOBUTTON GROUP RADI .
SELECTION-SCREEN POSITION 7.
SELECTION-SCREEN COMMENT 8(20) text-004 FOR FIELD R3.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b2.

*Sub Selection Screen.
SELECTION-SCREEN BEGIN OF SCREEN 1100 AS SUBSCREEN.
 SELECT-OPTIONS :
      S_LIFNR FOR ZTMM_END_PART-LIFNR MODIF ID AA       ,
      S_DATE FOR ZTMM_END_PART-REQUEST_DATE MODIF ID AA,
      S_CODE FOR ZTMM_END_PART-RETURN_CODE
                    MODIF ID AA1 NO-EXTENSION             .
SELECTION-SCREEN END OF SCREEN 1100.
