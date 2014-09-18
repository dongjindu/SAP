************************************************************************
* Program Name      : ZRSD01R_WO_STATUS_GENERAL
* Author            : HONG KI KIM
* Creation Date     : 2003.09.30.
* Specifications By : HONG KI KIM
* Pattern           : Report 1-2
* Development Request No : UD1K904835
* Addl Documentation:
* Description       : ZRSD01R_WO_STATUS_GENERAL
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT  ZRSD01R_WO_STATUS_GENERAL NO STANDARD PAGE HEADING
                                  MESSAGE-ID ZMSD.

*----------------------------------------------------------------------*
* INCLUDE
*----------------------------------------------------------------------*
INCLUDE ZRSD01R_WO_STATUS_GENERAL_T01.
*INCLUDE ZRSD01R_WO_STATUS_GENERAL_CLS.
INCLUDE ZRSD01R_WO_STATUS_GENERAL_F01.
INCLUDE ZRSD01R_WO_STATUS_GENERAL_PBO.
INCLUDE ZRSD01R_WO_STATUS_GENERAL_PAI.
*----------------------------------------------------------------------*
* EVENT
*----------------------------------------------------------------------*
INITIALIZATION.
*  PERFORM INITIALIZATION.

START-OF-SELECTION.
  PERFORM READ_DATA.
*  PERFORM MODIFY_DATA.

END-OF-SELECTION.
  PERFORM CALL_SCREEN.
