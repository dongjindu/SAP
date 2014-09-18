************************************************************************
* Program Name      : ZAPP916R_VEHICLE_DELIVERY
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
REPORT zapp916r_vehicle_delivery NO STANDARD PAGE HEADING
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
DATA : it_pmt07jb         LIKE TABLE OF ztpp_pmt07jb   WITH HEADER LINE,
       it_pmt07jb_a       LIKE TABLE OF ztpp_pmt07jb_a WITH HEADER LINE,
       it_7jb             LIKE TABLE OF ztpp_pmt07jb_b WITH HEADER LINE,
       it_cfile           LIKE TABLE OF ztpp_pmt07jb_c WITH HEADER LINE,
       it_cfile2          LIKE TABLE OF ztpp_pmt07jb_c WITH HEADER LINE,
       it_item            LIKE TABLE OF ztpp_pmt07jb_c WITH HEADER LINE,
       it_dfile           LIKE TABLE OF ztpp_pmt07jb_d WITH HEADER LINE,
       it_dfile2          LIKE TABLE OF ztpp_pmt07jb_d WITH HEADER LINE,
       it_wohd            LIKE TABLE OF zspp_vin_value WITH HEADER LINE,
       it_log             LIKE TABLE OF ztca_if_log    WITH HEADER LINE.

*----------------------------------------------------------------------*
*  GLOBAL VARIABLE DECLARATION
*----------------------------------------------------------------------*
DATA : wa_pmt07jb_a_ix    LIKE  sy-tabix,
       wa_pmt07jb_ix      LIKE  sy-tabix,
       wa_error_ix        LIKE  sy-tabix,
       wa_maxsqdt_dt      LIKE  sy-datum.
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
  CALL FUNCTION 'Z_FSD_VEHICLE_DELIVERY'
       EXPORTING
            equno               = p_equnr
       EXCEPTIONS
            not_found_vin       = 1
            not_found_sales     = 2
            not_found_vstel     = 3
            not_create_delivery = 4
            not_update_delivery = 5
            OTHERS              = 6.
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
