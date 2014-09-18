************************************************************************
* Author                 : HONG KI KIM
* Creation Date          : 2003-09-23
* Specifications By      :
* Development Request No : UD1K902107
* Addl documentation     :
* Description            : VIN INFORMATION DOWNLOAD
* Modification Log
* Date       Developer    Request ID Description
************************************************************************

REPORT  ZISD14U_VIN_INF     NO STANDARD PAGE HEADING
                            MESSAGE-ID ZMSD.
*----------------------------------------------------------------------*
* INCLUDE
*----------------------------------------------------------------------*
INCLUDE ZISD14U_VIN_INF_BG_T01.
INCLUDE ZISD14U_VIN_INF_BG_F01.
*----------------------------------------------------------------------*
* EVENT
*----------------------------------------------------------------------*
START-OF-SELECTION.
    PERFORM READ_DATA.

END-OF-SELECTION.
    PERFORM DOWNLOAD_DATA.
