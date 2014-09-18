************************************************************************
* Program Name      : ZEMMPM35E_SPE_MANAGE
* Created by        : Min-su Park
* Created on        : 2003.10.21.
* Pattern           :
* Description       :  Manage Error Standard Price for Purchase Material
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2003.10.22.     Min-su Park    UD1K901873     Initial Coding
************************************************************************

*&---------------------------------------------------------------------*
*& Include ZMINSUTOP                                                   *
*&                                                                     *
*&---------------------------------------------------------------------*

PROGRAM  ZMINSU MESSAGE-ID ZMMM  .
TABLES : ZTMM_SPE, RSDXX.
*Screen.
CONTROLS TC_SPE TYPE TABLEVIEW USING SCREEN 100.
DATA : OK_CODE LIKE SY-UCOMM,
       W_FCODE   LIKE SY-UCOMM,
       W_LOOPC   LIKE SY-LOOPC.

*BDC_DATA
DATA : BEGIN OF IT_BDC OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA : END OF IT_BDC.
DATA : BEGIN OF IT_MESSAGE OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA : END OF IT_MESSAGE.

*Internal Table.
DATA : BEGIN OF IT_ZTMM_SPE OCCURS 0,
        MARK                        .
        INCLUDE STRUCTURE ZTMM_SPE.
DATA : END OF IT_ZTMM_SPE.

*Finding Variable
DATA : W_LOOP_FIRST TYPE I,
       W_POSITION   TYPE I,
       W_FIND_POS   TYPE I,
       W_FOUND(01)        .


*Select Options.
SELECT-OPTIONS : S_EKORG FOR ZTMM_SPE-EKORG,
                 S_WERKS FOR ZTMM_SPE-WERKS.
