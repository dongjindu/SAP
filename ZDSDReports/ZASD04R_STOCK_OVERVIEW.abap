************************************************************************
* Program Name      : ZASD04R_STOCK_OVERVIEW
* Author            : HONG KI KIM
* Creation Date     : 2003.09.29.
* Specifications By : HONG KI KIM
* Pattern           : 1-2
* Development Request No : UD1K904910
* Addl Documentation:
* Description       : ZASD04R_STOCK_OVERVIEW
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT  ZASD04R_STOCK_OVERVIEW   NO STANDARD PAGE HEADING
                                 MESSAGE-ID ZMSD.

*----------------------------------------------------------------------*
* INCLUDE
*----------------------------------------------------------------------*
INCLUDE ZASD04R_STOCK_OVERVIEW_T01.
INCLUDE ZASD04R_STOCK_OVERVIEW_CLS.
INCLUDE ZASD04R_STOCK_OVERVIEW_F01.
INCLUDE ZASD04R_STOCK_OVERVIEW_PBO.
INCLUDE ZASD04R_STOCK_OVERVIEW_PAI.
*----------------------------------------------------------------------*
* EVENT
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM INITIALIZATION.

START-OF-SELECTION.
  PERFORM READ_DATA.
  PERFORM MODIFY_DATA.

END-OF-SELECTION.
  PERFORM CALL_SCREEN.
