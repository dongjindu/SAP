************************************************************************
* Program Name      : ZIPP502I_ENGIN_PS_2
* Author            : HyungYul, Kim
* Creation Date     : 2003.09.03.
* Specifications By : HyungYul, Kim
* Pattern           : 5.2.2
* Development Request UD1K943542
* Addl Documentation:
* Description       : Transfer of Engine Production Spec
*                     From SAP to MES
* Modification Logs
* Date       Developer    RequestNo    Description
* 05/07/08    Furong Wang UD1K943542   Copy from ZIPP502I_ENGIN_PS for
*                                      new MES interface

*
************************************************************************
REPORT ZIPP502I_ENGIN_PS MESSAGE-ID ZMPP.

************************************************************************
* INCLUDE
************************************************************************
INCLUDE ZIPP502L_ENGIN_PS_2_TOP.
*INCLUDE ZIPP502L_ENGIN_PS_TOP.
INCLUDE ZIPP502L_ENGIN_PS_2_F.
*INCLUDE ZIPP502L_ENGIN_PS_F.

************************************************************************
* INITIALIZAION
************************************************************************
INITIALIZATION.
*  PERFORM INITIALIZATION.

************************************************************************
* AT SELECTION-SCREEN
************************************************************************
AT SELECTION-SCREEN.
*  PERFORM AT_SELECTION-SCREEN.

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
