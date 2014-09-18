************************************************************************
* Program Name      : ZISD14U_VIN_INF2
* Author            : JUN HO CHOI
* Creation Date     : 2004.04.12.
* Specifications By : JUN HO CHOI
* Pattern           : 1-2
* Development Request No : UD1K909304
* Addl Documentation:
* Description       : VIN INFORMATION DOWNLOAD
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 01/26/2006 Furong Wang               Multi-selection for
*                                      HMC and HMA
*
*
************************************************************************
REPORT  zisd14u_vin_inf2    NO STANDARD PAGE HEADING
                            MESSAGE-ID zmsd.
*----------------------------------------------------------------------*
* TABLE
*----------------------------------------------------------------------*
TABLES: ausp, cabn.
*----------------------------------------------------------------------
* TYPES
*----------------------------------------------------------------------
TYPE-POOLS: slis, kkblo, vrm.
*----------------------------------------------------------------------*
* INTERNAL TABLE
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* GLOBAL VARIABLE
*----------------------------------------------------------------------*

DATA : variant LIKE indx-srtfd VALUE 'ISD14_02'.
DATA : var_des LIKE indx-srtfd VALUE 'ISD14_02_DES'.
DATA : var_NEW LIKE indx-srtfd VALUE 'ISD14_02_NEW'.
DATA : w_error(1).

DATA : eventid LIKE tbtcjob-eventid.


*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS : s_date  FOR sy-datum NO-EXTENSION OBLIGATORY. " ATFLV
PARAMETERS: p_des(1) DEFAULT 'C'.
SELECTION-SCREEN COMMENT 40(35) TEXT-M01.
PARAMETERS: p_NEW(1) DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b1 .


START-OF-SELECTION.
  PERFORM check_data.
  CHECK w_error IS INITIAL.
  PERFORM download_file.


*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_FILE
*&---------------------------------------------------------------------*
FORM download_file.
  EXPORT s_date TO   DATABASE indx(zs) ID variant.
  EXPORT p_des TO DATABASE indx(zs) ID var_des.
  EXPORT p_NEW TO DATABASE indx(zs) ID var_NEW.

  eventid = 'ZISD14_02'.

  CALL FUNCTION 'BP_EVENT_RAISE'
    EXPORTING
      eventid                      = eventid
*     EVENTPARM                    = ' '
*     TARGET_INSTANCE              = ' '
   EXCEPTIONS
     bad_eventid                  = 1
     eventid_does_not_exist       = 2
     eventid_missing              = 3
     raise_failed                 = 4
     OTHERS                       = 5.
  IF sy-subrc EQ 0.
    MESSAGE i000 WITH 'STARTING BATCH JOB'.
  ENDIF.
ENDFORM.                    " DOWNLOAD_FILE
*&---------------------------------------------------------------------*
*&      Form  check_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_data.
  CLEAR: w_error.
  IF p_des IS INITIAL.
    MESSAGE i000 WITH text-m02.
    w_error = 'X'.
    EXIT.
  ENDIF.

  IF NOT p_des CA 'CA'.
    MESSAGE i000 WITH text-m03.
    w_error = 'X'.
  ENDIF.
ENDFORM.                    " check_data
