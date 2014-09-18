************************************************************************
* Program name : ZEMMGM08E_INBOUND_ASN_IV                              *
* Created by   : Min-su Park                                           *
* Created on   : 2003.10.07                                            *
* Pattern      : Report 1-1                                            *
* Description  : Create Inbound Delivery From ASN & IV                 *
*                                                                      *
* Modification Log                                                     *
* Date            Developer        Request No.    Description          *
* 2003.10.07.     Min-su Park      UD1K901873     Initial Coding       *
*                                                                      *
************************************************************************
*----------------------------------------------------------------------*
*   INCLUDE ZEMMGM08E_TOP                                              *
*----------------------------------------------------------------------*
TABLES : ZTMM_ASN_IV.
*BDC_DATA
DATA : BEGIN OF IT_BDC OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA : END OF IT_BDC.
DATA : BEGIN OF IT_MESSAGE OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA : END OF IT_MESSAGE.

*Internal Table
DATA : IT_ASN_IV LIKE ZTMM_ASN_IV OCCURS 0 WITH HEADER LINE.
DATA : BEGIN OF IT_BDC_FIELD OCCURS 0,
             PO_NO(20)               , "Purchase Order
             LINE_NO(03)             , "Item
             DEPEN_PLACE(10)         , "Vendor
             REQUEST_DATE(08)        , "Delivery Date
             REQUEST_QTY(08)         , "Delivery Quantity
             DEPEN_NO(20)            , "Vendor Batch
       END OF IT_BDC_FIELD .

*SELECT SCREEN
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_ZSLNO FOR ZTMM_ASN_IV-ZSLNO NO INTERVALS NO-EXTENSION.
SELECT-OPTIONS: S_ZSDAT FOR ZTMM_ASN_IV-ZSDAT.
SELECTION-SCREEN END OF BLOCK B1.
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-005.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 5.
PARAMETERS       R1 RADIOBUTTON GROUP RADI DEFAULT 'X'.
SELECTION-SCREEN POSITION 7.
SELECTION-SCREEN COMMENT 8(10) TEXT-002 FOR FIELD R1.
SELECTION-SCREEN POSITION 25.
PARAMETERS       R2 RADIOBUTTON GROUP RADI.
SELECTION-SCREEN POSITION 26.
SELECTION-SCREEN COMMENT 27(25) TEXT-003 FOR FIELD R2.
SELECTION-SCREEN POSITION 55.
PARAMETERS       R3 RADIOBUTTON GROUP RADI.
SELECTION-SCREEN POSITION 56.
SELECTION-SCREEN COMMENT 57(10) TEXT-004 FOR FIELD R3.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B2.
PARAMETERS : P_MODE LIKE CTU_PARAMS-DISMODE DEFAULT 'N'.

*ALV
TYPE-POOLS: SLIS.

DATA:   WA_EVENTS      TYPE SLIS_T_EVENT,
        W_REPID LIKE SY-REPID           ,
        IT_FIELDCAT TYPE slis_t_fieldcat_alv WITH HEADER LINE,
        W_FORMNAME_TOP_OF_PAGE TYPE SLIS_FORMNAME VALUE 'TOP_OF_PAGE',
        WA_LIST_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.

*Data
DATA : BEGIN OF IT_EKPO OCCURS 0.
        INCLUDE STRUCTURE EKPO.
DATA : FLAG           ,
       END OF IT_EKPO.
DATA : W_MARK VALUE 'X'                ,
       W_TOTAL   LIKE ZTCA_IF_LOG-TOTAL,
       W_ZSUCC   LIKE ZTCA_IF_LOG-ZSUCC,
       W_POSNR   LIKE EKPO-EBELP       .
