************************************************************************
* Created by  : Myoungho Park
* Created on  : 2003.08.08.
* Description : Upload Excel Document format
*
*
* Modification Log
* Date       Developer    RequestNo    Description
*
* This Program is Uploading file that prints doucment
* using predefined excel form
*
************************************************************************

REPORT ZRGA_UPLOAD_FORMAT NO STANDARD PAGE HEADING
                                    LINE-SIZE 120
                                    LINE-COUNT 90.


FIELD-SYMBOLS : <FS>.

*** For File Select
DATA: "WA_FNAME LIKE RLGRAP-FILENAME,
      WA_RC LIKE SY-SUBRC.

****For Error Message
DATA: WA_RETURN LIKE BAPIRET2 .



SELECTION-SCREEN BEGIN OF BLOCK BLOCK1.
  PARAMETER: WA_FNAME LIKE RLGRAP-FILENAME.
  SELECTION-SCREEN PUSHBUTTON /70(10) PH_FNAME USER-COMMAND SCH.
  PARAMETER: WA_SPATH LIKE RLGRAP-FILENAME.
SELECTION-SCREEN END OF BLOCK BLOCK1.



INITIALIZATION.
  WA_SPATH = '/usr/sap/trans/tmp/'.
  MOVE 'Search' TO PH_FNAME.

AT SELECTION-SCREEN.
  IF SY-UCOMM = 'SCH'.
    PERFORM FILENAME_GET.
  ENDIF.


******** START-OF-SELECTION *****************
*********************************************
START-OF-SELECTION.

END-OF-SELECTION.
******** END-OF-SELECTION *******************
*********************************************


  PERFORM UPLOAD_FILE_FRNT_TO_SERVER.


*&---------------------------------------------------------------------*
*&      Form  FILENAME_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILENAME_GET.
*** Call file selector
  CALL FUNCTION 'WS_FILENAME_GET'
       EXPORTING
            MASK             = ',*.*,*.*.'
                             "// ',*.XLS,*.XLS.'
            MODE             = 'O'
            TITLE            = 'Select File format'
       IMPORTING
            FILENAME         = WA_FNAME
            RC               = WA_RC
       EXCEPTIONS
            INV_WINSYS       = 1
            NO_BATCH         = 2
            SELECTION_CANCEL = 3
            SELECTION_ERROR  = 4
            OTHERS           = 5.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZMPM) WITH TEXT-M01.
  ENDIF.

ENDFORM.                    " FILENAME_GET
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_FILE_FRNT_TO_SERVER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPLOAD_FILE_FRNT_TO_SERVER.

  DATA : WA_FILE_NAME  LIKE RLGRAP-FILENAME.

*-- saperate local pure file name
  PERFORM SPLIT_PATH_FILENAME USING WA_FNAME        "full file path
                                    WA_FILE_NAME.   "file name
*--- make target full file name
  CONCATENATE WA_SPATH  WA_FILE_NAME INTO WA_SPATH.

  CALL FUNCTION 'Z_FCA_UPLOAD_FILE_FRNT_TO_APPL'
       EXPORTING
            I_LOCAL_FILE_PATH = WA_FNAME
            I_APPL_FILE_PATH  = WA_SPATH
            I_OVERWRITE       = 'X'
       IMPORTING
            RETURN            = WA_RETURN.

  CALL FUNCTION 'BALW_BAPIRETURN_GET2'
       EXPORTING
            TYPE   = WA_RETURN-TYPE
            CL     = WA_RETURN-ID
            NUMBER = WA_RETURN-NUMBER
            PAR1   = WA_RETURN-MESSAGE_V1
            PAR2   = WA_RETURN-MESSAGE_V2
            PAR3   = WA_RETURN-MESSAGE_V3
            PAR4   = WA_RETURN-MESSAGE_V4
       IMPORTING
            RETURN = WA_RETURN.
  IF SY-SUBRC NE 0.
    MESSAGE E000(Zmpm) WITH WA_RETURN-MESSAGE.
  else.
    message s000(zmpm) with text-m02.
  ENDIF.
ENDFORM.                    " UPLOAD_FILE_FRNT_TO_SERVER
*&---------------------------------------------------------------------*
*&      Form  SPLIT_PATH_FILENAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_SERVER_PATH  text
*      -->P_WA_FILE_NAME  text
*----------------------------------------------------------------------*
FORM SPLIT_PATH_FILENAME USING    P_FULL_FILE_PATH
                                  P_FILE_NAME.

*  DATA : WA_FULL_PATH LIKE RLGRAP-FILENAME.
  DATA : WA_PATH      LIKE RLGRAP-FILENAME.

*  WA_FULL_PATH = P_FULL_FILE_PATH.

  CALL FUNCTION 'SO_SPLIT_FILE_AND_PATH'
       EXPORTING
            FULL_NAME     = P_FULL_FILE_PATH  "WA_FULL_PATH
       IMPORTING
            STRIPPED_NAME = P_FILE_NAME
            FILE_PATH     = WA_PATH
       EXCEPTIONS
            X_ERROR       = 1
            OTHERS        = 2.

  IF SY-SUBRC <> 0.

  ENDIF.

ENDFORM.                    " SPLIT_PATH_FILENAME
