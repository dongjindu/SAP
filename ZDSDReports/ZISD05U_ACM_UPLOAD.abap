************************************************************************
* Program Name      : ZISD05U_ACM_UPLOAD
* Author            : jun ho choi
* Creation Date     : 2003.07.08.
* Specifications By : jun ho choi
* Pattern           : 5-1
* Development Request No : UD1K904910
* Addl Documentation:
* Description       : Uploading ACM files and storage within SAP
*                          Costum Tables.
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT ZISD05U_ACM_UPLOAD NO STANDARD PAGE HEADING
                          MESSAGE-ID ZMSD.


*
TABLES : ZTSD_ACM_H,
         ZTSD_ACM_I,
         ZTSD_ACL_L,
         ZTSD_VIN_CONV,
         CABN,
         AUSP,
         KNVV.


*
DATA : BEGIN OF IT_UPFILE OCCURS 0,
       RECORD(431),
       END OF IT_UPFILE.

DATA : BEGIN OF IT_ACM_H OCCURS 0.
       INCLUDE STRUCTURE ZTSD_ACM_H.
DATA : END OF IT_ACM_H.

DATA : BEGIN OF IT_ACM_I OCCURS 0.
       INCLUDE STRUCTURE ZTSD_ACM_I.
DATA : END OF IT_ACM_I.

DATA : BEGIN OF IT_ACL_L OCCURS 0.
       INCLUDE STRUCTURE ZTSD_ACL_L.
DATA : END OF IT_ACL_L.

DATA : BEGIN OF IT_VM OCCURS 0,
       ZVIN  LIKE IT_ACM_H-ZVIN,
       ZSCOD LIKE IT_ACM_H-ZSCOD,
       END OF IT_VM.

DATA : W_ACLN LIKE ZTSD_ACM_H-ZACLN,
       W_CDST LIKE ZTSD_aCM_H-ZCDST,
       W_CDLR LIKE ZTSD_aCM_H-ZCDLR,
       W_CSER LIKE ZTSD_aCM_H-ZCSER.

DATA : W_CNT TYPE I.

DATA : SELECTFIELD   LIKE  HELP_INFO-FIELDNAME,
       X_FIELDS      LIKE  HELP_VALUE OCCURS 0 WITH HEADER LINE,
       SELECT_VALUE  LIKE  HELP_INFO-FLDVALUE,
       ID_TABIX      LIKE  SY-TABIX.
DATA : BEGIN OF X_FILE OCCURS 0.
       INCLUDE STRUCTURE EPSFILI.
DATA : END OF X_FILE.


*
DATA : VARIANT LIKE INDX-SRTFD VALUE 'ISD05_01'.

DATA : BEGIN OF IT_LIST OCCURS 0,
       FILE LIKE RLGRAP-FILENAME,
       END OF IT_LIST.

DATA: EVENTID LIKE TBTCJOB-EVENTID.


*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS : P_FILE LIKE RLGRAP-FILENAME OBLIGATORY.
SELECTION-SCREEN END OF BLOCK B1.


*
INITIALIZATION.
**  CONCATENATE '/sapmnt' SY-SYSID 'EDI/' INTO P_FILE
**                                        SEPARATED BY '/'.
  P_FILE = '/usr/sap/EDI_SAP/'.


*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  PERFORM GET_FILE.


*
START-OF-SELECTION.
  PERFORM UPLOAD_FILE.


*
END-OF-SELECTION.


*&---------------------------------------------------------------------*
*&      Form  GET_FILE
*&---------------------------------------------------------------------*
FORM GET_FILE.
  CLEAR   : SELECTFIELD, X_FIELDS, SELECT_VALUE, ID_TABIX, X_FILE.
  REFRESH : X_FIELDS, X_FILE.

  CALL FUNCTION 'EPS_GET_DIRECTORY_LISTING'
    EXPORTING
      DIR_NAME                     = '/usr/sap/EDI_SAP/'
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
  CONCATENATE '/usr/sap/EDI_SAP/' X_FILE-NAME INTO P_FILE.
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
  CONCATENATE '/usr/sap/EDI_SAP/' X_FILE-NAME INTO P_FILE.
  ENDIF.
ENDFORM.                    " GET_FILE
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_FILE
*&---------------------------------------------------------------------*
FORM UPLOAD_FILE.
  REFRESH IT_LIST. CLEAR IT_LIST.
  IT_LIST-FILE = P_FILE.
  APPEND IT_LIST. CLEAR IT_LIST.

  EXPORT IT_LIST TO   DATABASE INDX(ZS) ID VARIANT.

  MESSAGE I000 WITH 'STARTING BATCH JOB'.

  EVENTID = 'ZISD05_01'.

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
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_RESULT
*&---------------------------------------------------------------------*
FORM DISPLAY_RESULT.
  LOOP AT IT_ACL_L.
    WRITE:/ SY-VLINE, (07) IT_ACL_L-ZACLN,
            SY-VLINE, (10) IT_ACL_L-ZCDST,
            SY-VLINE, (07) IT_ACL_L-ZACQT NO-ZERO,
            SY-VLINE, (14) IT_ACL_L-ZACAA CURRENCY IT_ACL_L-ZPYCR,
            SY-VLINE, (05) IT_ACL_L-ZPYCR,
            SY-VLINE.
    WRITE:/(59) SY-ULINE.
  ENDLOOP.
ENDFORM.                    " DISPLAY_RESULT
*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM TOP_OF_PAGE.
  FORMAT COLOR COL_HEADING.
  WRITE:/(59) SY-ULINE.
  WRITE:/ SY-VLINE, (07) 'ACL No',
          SY-VLINE, (10) 'Dist. Code',
          SY-VLINE, (07) 'ACL Quan',
          SY-VLINE, (14) 'Appr. ACL Amt',
          SY-VLINE, (05) 'CurK',
          SY-VLINE.
  WRITE:/(59) SY-ULINE.
  FORMAT COLOR COL_HEADING OFF.
ENDFORM.                    " TOP_OF_PAGE
