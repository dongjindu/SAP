************************************************************************
* Program Name      : ZAPP918R_VEHICLE_SHIPMENT
* Author            : JongOh, Kim
* Creation Date     : 2003.09.03.
* Specifications By : JongOh, Kim
* Pattern           : 5.1.4.1
* Development Request No : UD1K901950
* Addl Documentation:
* Description       : Call the SD Function
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT ZAPP918R_VEHICLE_SHIPMENT NO STANDARD PAGE HEADING
                                 LINE-SIZE 200
                                 MESSAGE-ID zmpp.

*----------------------------------------------------------------------*
*  EXTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
TABLES : ztpp_pmt07jb,      "Sequencing Result (D~D+3)
         ztpp_pmt07jb_a,    "SUMMARIZED PMT07JB
         ztpp_common_vals.  "PP: Common Values

*----------------------------------------------------------------------*
*  INTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*  GLOBAL VARIABLE DECLARATION
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*  CONSTANTS DECLARATION
*----------------------------------------------------------------------*
CONSTANTS : c_mark   VALUE 'X'.

*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS : p_equnr  LIKE  equi-equnr .
SELECTION-SCREEN END OF BLOCK b1.

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
  PERFORM call_sd_function.

************************************************************************
* END-OF-SELECTION
************************************************************************
END-OF-SELECTION.
*  PERFORM list_process.

*&---------------------------------------------------------------------*
*&      Form  CALL_SD_FUNCTION
*&---------------------------------------------------------------------*
FORM call_sd_function.
  CALL FUNCTION 'Z_FSD_VEHICLE_GOODISSUE'
       EXPORTING
            equno               = p_equnr .
ENDFORM.                    " CALL_SD_FUNCTION

*&---------------------------------------------------------------------*
*&      Form  LIST_PROCESS
*&---------------------------------------------------------------------*
FORM list_process.

ENDFORM.                    " LIST_PROCESS

*&---------------------------------------------------------------------*
*&      Form  AT_SELECTION-SCREEN
*&---------------------------------------------------------------------*
FORM AT_SELECTION-SCREEN.

ENDFORM.                    " AT_SELECTION-SCREEN

*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
FORM INITIALIZATION.

ENDFORM.                    " INITIALIZATION
