************************************************************************
* Program Name      : SAPMZPP_APPLICATION
* Author            : Bobby
* Creation Date     : 2003.08.26.
* Specifications By : Bobby
* Pattern           : 2.1
* Development Request No :
* Addl Documentation:
* Description       : [PP] APPLICATION SET
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 01/27/2004 Tonkey       UD1K906384   Changed Table Relationship
* 09/29/2004 chris        UD1K912350   Added planned order number
*                                      on screen 2117
* 06/29/2005 chris                     exclude scrapped and disposal car
*
************************************************************************
*&---------------------------------------------------------------------*
*& Module pool       SAPMZPP_APPLICATION                               *
*&                                                                     *
*&---------------------------------------------------------------------*
PROGRAM sapmzpp_application MESSAGE-ID zmpp .
INCLUDE mzpp_applicationtop.
INCLUDE mzpp_applicationtop_alv.
* INCLUDE MZPP_APPLICATIONCLASS.
INCLUDE mzpp_applicationo01.
INCLUDE mzpp_applicationi01.
* Modified by HASEEB MOHAMMAD on 01-17-2006 to Make the Excel file in
* screen 2205, i.e the SCRAP LIST HISTORY #31.
INCLUDE mzpp_applicationf01.
*INCLUDE z_has_mzpp_applicationf01.

* above is commented by me

INCLUDE mzpp_applicationf02.
INCLUDE mzpp_applicationf03.

LOAD-OF-PROGRAM.
  CLEAR: ok_code.
  SET PARAMETER ID 'ZMENU' FIELD ok_code.

INCLUDE MZPP_APPLICATIONF04.

INCLUDE MZPP_APPLICATIONF05.

INCLUDE MZPP_APPLICATIONF06.
