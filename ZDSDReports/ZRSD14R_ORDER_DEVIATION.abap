************************************************************************
* Program Name      : ZRSD14R_ORDER_DEVIATION
* Author            : HONG KI KIM
* Creation Date     : 2003.08.20.
* Specifications By : HONG KI KIM
* Pattern           : Report 1-2
* Development Request No : UD1K904910
* Addl Documentation:
* Description       : ORDER DEVIATION LIST
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT ZRSD14R_ORDER_DEVIATION NO STANDARD PAGE HEADING
                               MESSAGE-ID ZMSD.

INCLUDE ZRSD14R_ORDER_DEVIATION_T01.
INCLUDE ZRSD14R_ORDER_DEVIATION_F01.
INCLUDE ZRSD14R_ORDER_DEVIATION_PBO.
INCLUDE ZRSD14R_ORDER_DEVIATION_PAI.

START-OF-SELECTION.
  PERFORM READ_DATA.
  PERFORM MODIFY_DATA.

END-OF-SELECTION.
  PERFORM CALL_SCREEN.
