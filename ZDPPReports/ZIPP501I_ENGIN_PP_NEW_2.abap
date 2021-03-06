************************************************************************
* Program Name      : ZIPP501I_ENGIN_PP
* Author            : HyungYul, Kim
* Creation Date     : 2003.09.03.
* Specifications By : HyungYul, Kim
* Pattern           : 5.2.2
* Development Request No : UD1K902114
* Addl Documentation:
* Description       : Transfer of Engine Production Planning
*                     From SAP to MES
* Modification Logs
* Date       Developer    RequestNo    Description
*02/27/2006  Furong                    Add process for no planned order
*                                      case
************************************************************************
REPORT ZIPP501I_ENGIN_PP NO STANDARD PAGE HEADING  MESSAGE-ID ZMPP.

************************************************************************
* INCLUDE
************************************************************************
INCLUDE ZIPP501L_ENGIN_PP_NEW_2_TOP.
*INCLUDE ZIPP501L_ENGIN_PP_NEW_TOP.
*INCLUDE ZIPP501L_ENGIN_PP_TOP.
INCLUDE ZIPP501L_ENGIN_PP_NEW_2_F.
*INCLUDE ZIPP501L_ENGIN_PP_NEW_F.
*INCLUDE ZIPP501L_ENGIN_PP_F.

************************************************************************
* INITIALIZAION
************************************************************************
INITIALIZATION.
  PERFORM INITIALIZATION.

************************************************************************
* AT SELECTION-SCREEN
************************************************************************
AT SELECTION-SCREEN.
  PERFORM AT_SELECTION-SCREEN.

************************************************************************
* START-OF-SELECTION
************************************************************************
START-OF-SELECTION.
  PERFORM READ_PROCESS.
  PERFORM DATA_PROCESS.

************************************************************************
* END-OF-SELECTION
************************************************************************
END-OF-SELECTION.
  PERFORM LIST_PROCESS.
