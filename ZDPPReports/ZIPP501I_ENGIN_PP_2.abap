************************************************************************
* Program Name      : ZIPP501I_ENGIN_PP_2
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
* 2004.01.13  JOKIM       UD1K905816   Addition Selected Cond. of 'PLAF'
* 05/07/08    Furong Wang UD1K943542   Copy from ZIPP501I_ENGIN_PP for
*                                      new MES interface. Send to 2
*                                      interface ZTPPER & ZTPPER2
*                                      by ENG_SPC15 = ENG1 or ENG2
************************************************************************
REPORT ZIPP501I_ENGIN_PP_2 NO STANDARD PAGE HEADING  MESSAGE-ID ZMPP.

************************************************************************
* INCLUDE
************************************************************************
INCLUDE ZIPP501L_ENGIN_PP_2_TOP.
*INCLUDE ZIPP501L_ENGIN_PP_TOP.
INCLUDE ZIPP501L_ENGIN_PP_2_F.
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
