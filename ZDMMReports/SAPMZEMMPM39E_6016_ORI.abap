************************************************************************
* Program Name      : SAPMZEMMPM39E_6016
* Author            : Hakchin Kim
* Creation Date     : 2003.08.25.
* Specifications By : Hakchin Kim
* Development Request No : EMMPM39
* Addl Documentation: F/S - EMMPM39
* Description       : Transfer Order List for confirmation
*                     - For RF with Feeding Order List
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
************************************************************************
*&---------------------------------------------------------------------*
*& Module pool       SAPMZEMMPM39E_6016                                *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*
PROGRAM  sapmzemmpm39e_6016
             NO STANDARD PAGE HEADING
             LINE-SIZE  120                                 "until 1023
             LINE-COUNT 90.             "(7)
*             MESSAGE-ID zmmm.         "Message Class Declaration

INCLUDE MZEMMPM39E_6016_ORITOP.
*INCLUDE mzemmpm39e_6016top.
INCLUDE MZEMMPM39E_6016_ORICLA.
*INCLUDE mzemmpm39e_6016cla.      " Global Class
INCLUDE MZEMMPM39E_6016_ORIO01.
*INCLUDE mzemmpm39e_6016o01.
INCLUDE MZEMMPM39E_6016_ORII01.
*INCLUDE mzemmpm39e_6016i01.
INCLUDE MZEMMPM39E_6016_ORIF01.
*INCLUDE mzemmpm39e_6016f01.
