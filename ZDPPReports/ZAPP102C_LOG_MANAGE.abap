************************************************************************
* Author                 : HONG KI KIM
* Creation Date          : 2003-09-29
* Specifications By      :
* Pattern                : 2.1
* Development Request No : UD1K902107
* Addl documentation     :
* Description            : ZAS
* HMMA ABAP Programming Guide : 1.2 Screen Call
* Modification Log
* Date       Developer    Request ID Description
************************************************************************
REPORT  ZASD04R_STOCK_OVERVIEW   NO STANDARD PAGE HEADING
                                 MESSAGE-ID ZMSD.

*----------------------------------------------------------------------*
* INCLUDE
*----------------------------------------------------------------------*
INCLUDE ZAPP102C_LOG_MAN_T01.
INCLUDE ZAPP102C_LOG_MAN_CLS.
INCLUDE ZAPP102C_LOG_MAN_F01.
INCLUDE ZAPP102C_LOG_MAN_PBO.
INCLUDE ZAPP102C_LOG_MAN_PAI.
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
