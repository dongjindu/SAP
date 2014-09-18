************************************************************************
* Program Name      : ZSSD05F_HMA_COMMERCIAL
* Author            : jun ho choi
* Creation Date     : 2004.01.20.
* Specifications By : jun ho choi
* Pattern           :
* Development Request No : UD1K906130
* Addl Documentation:
* Description       : HMA COMMERCIAL INVOICE
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT ZSSD05F_HMA_COMMERCIAL NO STANDARD PAGE HEADING
                                 MESSAGE-ID ZMSD
                                 LINE-SIZE 135.


*
TABLES : ZSSD_BILLING, VBRK, VBRP.


*
TYPE-POOLS OLE2.        "EXCEL


*
DATA: EXCEL      TYPE OLE2_OBJECT,
      BOOKS      TYPE OLE2_OBJECT,
"      BOOK       TYPE OLE2_OBJECT,
"      RANGE      TYPE OLE2_OBJECT,
"      BORDERS    TYPE OLE2_OBJECT,
      CELL       TYPE OLE2_OBJECT,
      SHEET      TYPE OLE2_OBJECT,
"      C1(3)      TYPE N,
"      C2(3)      TYPE N,
      GV_UNDER(30) VALUE '______________________________',
      GV_UNDER1(16) VALUE '________________',
      GV_FILENAME LIKE RLGRAP-FILENAME VALUE
        'C:\temp\Commercial_Invoice_HMA.xls'.

DATA : BEGIN OF IT_LIST OCCURS 0,
"       VBELN LIKE VBRK-VBELN,
"       FKDAT LIKE VBRK-FKDAT,
"       POSNR LIKE VBRP-POSNR,
       MATNR LIKE VBRP-MATNR,
       ARKTX LIKE VBRP-ARKTX,
       FKIMG LIKE VBRP-FKIMG,
       VRKME LIKE VBRP-VRKME,
       NETWR LIKE VBRP-NETWR,
       WAERK LIKE VBRK-WAERK,
       END OF IT_LIST.

DATA : SAVE_OK_CODE(4). "+

DATA : W_CNT TYPE I, "+
       W_SAVE(1), "-
       W_ANSWER(1), "-
       W_TOT_FKIMG LIKE VBRP-FKIMG,
       W_TOT_NETWR LIKE VBRP-NETWR,
       W_UNDER1(35) VALUE '___________________________________',
       W_UNDER2(25) VALUE '_________________________',
       W_UNDER3(17) VALUE '_________________',
       W_UNDER4(09) VALUE '_________',
       W_UNDER5(15) VALUE '_______________',
       W_UNDER6(18) VALUE '__________________'.


*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS : S_FKDAT FOR VBRK-FKDAT OBLIGATORY NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK B1.


*
START-OF-SELECTION.
  PERFORM GET_DATA.


*
END-OF-SELECTION.
  PERFORM CALL_SCREEN.





************************************************************************
* TYPE FOR THE DATA OF TABLECONTROL 'TC_9000'
TYPES: BEGIN OF T_TC_9000.
         INCLUDE STRUCTURE ZSSD_BILLING.
TYPES:   FLAG,       "flag for mark column
       END OF T_TC_9000.

* INTERNAL TABLE FOR TABLECONTROL 'TC_9000'
DATA:     G_TC_9000_ITAB   TYPE T_TC_9000 OCCURS 0,
          G_TC_9000_WA     TYPE T_TC_9000, "work area
          G_TC_9000_COPIED.           "copy flag

* DECLARATION OF TABLECONTROL 'TC_9000' ITSELF
CONTROLS: TC_9000 TYPE TABLEVIEW USING SCREEN 9000.

* LINES OF TABLECONTROL 'TC_9000'
DATA:     G_TC_9000_LINES  LIKE SY-LOOPC.

DATA:     OK_CODE LIKE SY-UCOMM.

* Includes inserted by Screen Painter Wizard. DO NOT CHANGE THIS LINE!
INCLUDE ZSSD05L_HMA_COMMERCIAL_F01.
INCLUDE ZSSD05L_HMA_COMMERCIAL_PBO.
INCLUDE ZSSD05L_HMA_COMMERCIAL_PAI.
