************************************************************************
* Program Name      : ZISD06U_PART_MOBIS
* Author            : jun ho choi
* Creation Date     : 2003.11.13.
* Specifications By : jun ho choi
* Pattern           : 5-1
* Development Request No : UD1K904910
* Addl Documentation:
* Description       : Uploading Part files and storage within SAP
*                          Costum Tables.
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 01/12/2005 wskim  UD1K913797  Issue # Parts info uploading - change
* directory
*
*
************************************************************************
REPORT ZISD06U_PART_MOBIS NO STANDARD PAGE HEADING
                          MESSAGE-ID ZMSD.


*
TABLES : ZTSD_PART_INF,
         ZTSD_PART_HST,
         ZTSD_PART_PRC,
         ZTSD_PART_SUP.


*
DATA : BEGIN OF IT_INF OCCURS 0,
       RECORD(61),
       END OF IT_INF.

DATA : BEGIN OF IT_HST OCCURS 0,
       RECORD(37),
       END OF IT_HST.

DATA : BEGIN OF IT_PRC OCCURS 0,
       RECORD(58),
       END OF IT_PRC.

DATA : BEGIN OF IT_SUP OCCURS 0,
       RECORD(33),
       END OF IT_SUP.

DATA : W_CNT TYPE I.

DATA : SELECTFIELD   LIKE  HELP_INFO-FIELDNAME,
       X_FIELDS      LIKE  HELP_VALUE OCCURS 0 WITH HEADER LINE,
       SELECT_VALUE  LIKE  HELP_INFO-FLDVALUE,
       ID_TABIX      LIKE  SY-TABIX.
DATA : BEGIN OF X_FILE OCCURS 0.
       INCLUDE STRUCTURE EPSFILI.
DATA : END OF X_FILE.


*
DATA : VARIANT LIKE INDX-SRTFD VALUE 'ISD06_01'.

DATA : BEGIN OF IT_LIST OCCURS 0,
       FILE1 LIKE RLGRAP-FILENAME,
       FILE2 LIKE RLGRAP-FILENAME,
       FILE3 LIKE RLGRAP-FILENAME,
       FILE4 LIKE RLGRAP-FILENAME,
       END OF IT_LIST.

DATA: EVENTID LIKE TBTCJOB-EVENTID.
*Issue #  requested by YKKO
*Changed by wskim,on 01122005
*-----Start
data : file_path LIKE EPSF-EPSDIRNAM value '/usr/sap/EDI_SAP/'.
*-----End

*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS : P_FILE1 LIKE RLGRAP-FILENAME,
             P_FILE2 LIKE RLGRAP-FILENAME,
             P_FILE3 LIKE RLGRAP-FILENAME,
             P_FILE4 LIKE RLGRAP-FILENAME.
SELECTION-SCREEN END OF BLOCK B1.


*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE1.
  PERFORM GET_FILE USING P_FILE1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE2.
  PERFORM GET_FILE USING P_FILE2.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE3.
  PERFORM GET_FILE USING P_FILE3.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE4.
  PERFORM GET_FILE USING P_FILE4.


*
START-OF-SELECTION.
  PERFORM UPLOAD_FILE.


*
END-OF-SELECTION.


*&---------------------------------------------------------------------*
*&      Form  GET_FILE
*&---------------------------------------------------------------------*
FORM GET_FILE USING P_FILE.
  CLEAR   : SELECTFIELD, X_FIELDS, SELECT_VALUE, ID_TABIX, X_FILE.
  REFRESH : X_FIELDS, X_FILE.

  CALL FUNCTION 'EPS_GET_DIRECTORY_LISTING'
    EXPORTING
*Issue # 20050107-003 requested by YKKO
*Changed by wskim,on 01112005
*-----Start
*      DIR_NAME                     = '/sapmnt/UP2/EDI/'
       DIR_NAME                     = file_path
*-----End
      FILE_MASK                    = '*.*'
*   IMPORTING
*     DIR_NAME                     =
*     FILE_COUNTER                 =
*     ERROR_COUNTER                =
    TABLES
      DIR_LIST                     = X_FILE
    EXCEPTIONS
      INVALID_EPS_SUBDIR           = 1
      SAPGPARAM_FAILED             = 2
      BUILD_DIRECTORY_FAILED       = 3
      NO_AUTHORIZATION             = 4
      READ_DIRECTORY_FAILED        = 5
      TOO_MANY_READ_ERRORS         = 6
      EMPTY_DIRECTORY_LIST         = 7
      OTHERS                       = 8.

  X_FIELDS-TABNAME = 'EPSFILI'.
  X_FIELDS-FIELDNAME = 'NAME'.
  X_FIELDS-SELECTFLAG = 'X'.
  APPEND X_FIELDS.

  X_FIELDS-TABNAME = 'EPSFILI'.
  X_FIELDS-FIELDNAME = 'SIZE'.
  X_FIELDS-SELECTFLAG = ''.
  APPEND X_FIELDS.

  DESCRIBE TABLE X_FILE LINES W_CNT.
  IF W_CNT = 1.
  READ TABLE X_FILE INDEX 1.
  CONCATENATE file_path X_FILE-NAME INTO P_FILE.
  ELSE.
  CALL FUNCTION 'HELP_VALUES_GET_NO_DD_NAME'
       EXPORTING
            CUCOL                        = 20
            CUROW                        = 05
            SELECTFIELD                  = SELECTFIELD
            TITEL                        = 'File in /usr/sap/EDI_SAP/'
       IMPORTING
            IND                          = ID_TABIX
            SELECT_VALUE                 = SELECT_VALUE
       TABLES
            FIELDS                       = X_FIELDS
            FULL_TABLE                   = X_FILE
       EXCEPTIONS
            FULL_TABLE_EMPTY             = 1
            NO_TABLESTRUCTURE_GIVEN      = 2
            NO_TABLEFIELDS_IN_DICTIONARY = 3
            MORE_THEN_ONE_SELECTFIELD    = 4
            NO_SELECTFIELD               = 5
            OTHERS                       = 6.

  CHECK NOT ID_TABIX IS INITIAL.
  READ TABLE X_FILE INDEX ID_TABIX.
  CONCATENATE file_path X_FILE-NAME INTO P_FILE.
  ENDIF.
ENDFORM.                    " GET_FILE
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_FILE
*&---------------------------------------------------------------------*
FORM UPLOAD_FILE.
  REFRESH IT_LIST. CLEAR IT_LIST.
  IT_LIST-FILE1 = P_FILE1.
  IT_LIST-FILE2 = P_FILE2.
  IT_LIST-FILE3 = P_FILE3.
  IT_LIST-FILE4 = P_FILE4.
  APPEND IT_LIST. CLEAR IT_LIST.

  EXPORT IT_LIST TO   DATABASE INDX(ZS) ID VARIANT.

  MESSAGE I000 WITH 'STARTING BATCH JOB'.

  EVENTID = 'ZISD06_01'.

  CALL FUNCTION 'BP_EVENT_RAISE'
    EXPORTING
      EVENTID                      = EVENTID
*     EVENTPARM                    = ' '
*     TARGET_INSTANCE              = ' '
   EXCEPTIONS
     BAD_EVENTID                  = 1
     EVENTID_DOES_NOT_EXIST       = 2
     EVENTID_MISSING              = 3
     RAISE_FAILED                 = 4
     OTHERS                       = 5.
ENDFORM.                    " UPLOAD_FILE
