************************************************************************
* Program Name      : ZRSD01R_WO_DETAIL_BACKGROUND
* Author            : HONG KI KIM
* Creation Date     : 2003.10.05.
* Specifications By : HONG KI KIM
* Pattern           : Report 1-2
* Development Request No : UD1K904835
* Addl Documentation:
* Description       : ZRSD01R_WO_DETAIL_BACKGROUND
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT  ZRSD01R_WO_DETAIL_BACKGROUND  NO STANDARD PAGE HEADING
                                      MESSAGE-ID ZMSD.

*----------------------------------------------------------------------*
* INCLUDE
*----------------------------------------------------------------------*
INCLUDE ZRSD01R_WO_DETAIL_BACKGROUND_T.
INCLUDE ZRSD01R_WO_DETAIL_BACKGROUND_F.
INCLUDE ZRSD01R_WO_DETAIL_BACKGROUND_P.
INCLUDE ZRSD01R_WO_DETAIL_BACKGROUND_I.
*----------------------------------------------------------------------*
* EVENT
*----------------------------------------------------------------------*
INITIALIZATION.
*  PERFORM INITIALIZATION.

START-OF-SELECTION.
  PERFORM READ_DATA.
  PERFORM PROCESS_DATA.

END-OF-SELECTION.
  PERFORM CALL_SCREEN.
