************************************************************************
* Program Name      : ZAPP246R_PROD_INL_BY_PROGRESS
* Author            : KIM GIL-HYUN (Tonkey)
* Creation Date     : 2004.04.06.
* Specifications By : B. Choi
* Development Request No : UD1K909191
* Addl Documentation:
* Description       : Production Results In Line Status By Progress .
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 10/29/13   Furong       Block from running
*
************************************************************************

INCLUDE zapp246l_top.
INCLUDE zapp246l_o01.
INCLUDE zapp246l_i01.
INCLUDE zapp246l_f01.

*-----------------------------------------------------------
START-OF-SELECTION.
*-----------------------------------------------------------
* 10/29/13   Furong       Block from running
  MESSAGE e009 with 'The program has been blocked'.
*
  CALL SCREEN 100.
