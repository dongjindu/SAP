****************************************************************
* Program Name  : ZIMMGM19I_SUBPART_REQUEST
* Created by    : Min-su Park
* Created on    : 2003.11.06.
* Pattern       :
* Description   : SUB PART REQUEST(Out bound)
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2003.11.06.     Min-su Park    UD1K901873     Initial Coding
***************************************************************
*&---------------------------------------------------------------------*
*& Include ZIMMGM19I_SUBPART_REQUESTTOP                                *
*&                                                                     *
*&---------------------------------------------------------------------*

PROGRAM  ZIMMGM19I_SUBPART_REQUEST     .

*Internal Table
DATA : BEGIN OF IT_END_PART OCCURS 0.
        INCLUDE STRUCTURE ZTMM_END_PART.
DATA : END OF IT_END_PART.

*ALV Definition.
TYPE-POOLS: SLIS.
DATA:   WA_EVENTS   TYPE SLIS_T_EVENT       ,
        W_REPID LIKE SY-REPID               ,
        WA_SORT     TYPE SLIS_T_SORTINFO_ALV,
        IT_FIELDCAT TYPE slis_t_fieldcat_alv WITH HEADER LINE,
        W_FORMNAME_TOP_OF_PAGE TYPE SLIS_FORMNAME VALUE 'TOP_OF_PAGE',
        WA_LIST_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.

*General Data
DATA : C_DEST(10) VALUE 'WMRM01'.
DATA : W_TOTAL      LIKE ZTCA_IF_LOG-TOTAL,  "total cnt in interface
       W_SUCC       LIKE ZTCA_IF_LOG-ZSUCC,  "success cnt in interface
       WA_ZTCA_IF_LOG LIKE ZTCA_IF_LOG.


*Selection Screen
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 5.
PARAMETERS       r1 RADIOBUTTON GROUP radi DEFAULT 'X'.
SELECTION-SCREEN POSITION 7.
SELECTION-SCREEN COMMENT 8(10) text-002 FOR FIELD r1.
SELECTION-SCREEN POSITION 40.
PARAMETERS       r2 RADIOBUTTON GROUP radi.
SELECTION-SCREEN POSITION 41.
SELECTION-SCREEN COMMENT 42(25) text-003 FOR FIELD r1.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b2.
