************************************************************************
* Program Name      : ZIPP109I_APS_7CW.
* Author            : DongYeop Han
* Creation Date     : 2003.08.25.
* Specifications By : DongYeop Han
* Pattern           : 1.1
* Development Request No :UD1K901928
* Addl Documentation:
* Description       : MITU Order (7CW).
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT ZIPP109I_APS_7CW
               NO STANDARD PAGE HEADING
               LINE-SIZE  1023
               MESSAGE-ID ZMPP.

************************************************************************
*              INCLUDE                                                 *
************************************************************************
TABLES: AUSP,
        ZTPP_PMT07CW.


*INTERNAL TABLE
DATA: BEGIN OF IT_OBJEK OCCURS 0,
      OBJEK LIKE AUSP-OBJEK,
      END OF IT_OBJEK.

DATA: IT_T07CW LIKE ZSPP_PMT07CW OCCURS 0 WITH HEADER LINE,
      IT_PMT07CW_RFC LIKE ZSPP_PMT07CW_RFC OCCURS 0 WITH HEADER LINE.

*WORK_AREA
DATA: P_ATINN LIKE   AUSP-ATINN,       "USING CONVERSION FUNTION
      P_ATWRT TYPE   AUSP-ATWRT,
      P_ATFLV TYPE   AUSP-ATFLV,
      P_ATFOR TYPE   CABN-ATFOR,
      WA_NUMERIC(15) TYPE N,
      WA_INTEGER     TYPE I.

*----------------------------------------------------------------------*
*  CONSTANS DECLARATION
*----------------------------------------------------------------------*
DATA : C_DEST(10) VALUE 'WMPP01'.   "Outbound Interface Destination


*SELECTION-SCREEN
SELECTION-SCREEN BEGIN OF BLOCK block WITH FRAME.
PARAMETERS:P_CHECK AS CHECKBOX.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 5(30) TEXT.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK block.
INCLUDE ZIPP109I_APS_7CW_FO1.
************************************************************************

************************************************************************
*              INITIALIZATION                                          *
************************************************************************
INITIALIZATION.
TEXT = 'CHECK ----> REPROCESSING'.
************************************************************************
*              START-OF-SELECTION PROCESSING                           *
************************************************************************
START-OF-SELECTION.
IF P_CHECK IS INITIAL.
PERFORM BASIC_DATA_SELECT.
PERFORM DATA_JOIN.
ELSE.                               "RE PROCESSING
PERFORM ERROR_DATA_SELECT.
ENDIF.
PERFORM TO_LEGACY.
PERFORM TABLE_UPDATE.
END-OF-SELECTION.
PERFORM DATA_WRITE.
