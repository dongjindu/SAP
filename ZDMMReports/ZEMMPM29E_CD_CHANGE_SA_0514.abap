************************************************************************
* Program Name  : ZEMMPM29E_CD_CHANGE_SA
* Created  by   : Min-su Park
* Creation on   : 2003.10.27.
* Pattern       :
* Description   : Condition change in Schedule Agreement
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2003.10.19.     Min-su Park    UD1K901873     Initial Coding
*&---------------------------------------------------------------------*
* Date            Developer        RequestNo      Description
* 2004.02.04.     Jaesung Lee    UD1K906915     Changed condition logic
* Condition logic changed: plant leavel =>  Purchasing group
*&---------------------------------------------------------------------*

************************************************************************

*&---------------------------------------------------------------------*
*& Module pool       ZEMMPM29E_CD_CHANGE_SA                            *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*


INCLUDE zemmpm29e_cd_change_satop.
INCLUDE zemmpm29e_cd_change_saevt.
*INCLUDE ZEMMPM29E_CD_CHANGE_SAF01.
INCLUDE zemmpm29e_cd_change_saf02.

************************************************************************
