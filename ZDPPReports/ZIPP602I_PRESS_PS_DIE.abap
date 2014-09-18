************************************************************************
* Program Name      : ZIPP602I_PRESS_PS_DIE
* Author            : HyungYul, Kim
* Creation Date     : 2003.09.03.
* Specifications By : HyungYul, Kim
* Pattern           : 5.2.2
* Development Request No : UD1K903245
* Addl Documentation:
* Description       : Transfer of Engine Production Planning
*                     From SAP to MES
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT ZIPP602I_PRESS_PS_DIE NO STANDARD PAGE HEADING
*                         LINE-SIZE 120
                         MESSAGE-ID ZMPP.

************************************************************************
* INCLUDE
************************************************************************
INCLUDE ZIPP602L_PRESS_PS_DIE_TOP.
INCLUDE ZIPP602L_PRESS_PS_DIE_F.

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
