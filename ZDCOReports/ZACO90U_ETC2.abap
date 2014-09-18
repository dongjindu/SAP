************************************************************************
* Program Name      : ZACO90U_ETC2
* Author            : Hyung Jin Youn
* Creation Date     : 06/01/2004
* Specifications By : Hae-Sung Cho
* Pattern           : Report 1-1
* Development Request No: UD1K905482
* Add documentation :
* Description       : To make easy to upload additive costs.
*                     (CATT cannot support fully the needs of HMMA)
*
* the BDC structures for BATCH INPUT processing
*
* Modifications Log
* Date   Developer   Request ID    Description
*
************************************************************************
REPORT ZACO90U_ETC2 MESSAGE-ID ZMCO.


*----------------------------------------------------------------------*
*   Data Definition
*----------------------------------------------------------------------*
** Type-Pools
TYPE-POOLS : KKPI , KCDE .


** Tables
TABLES : ZSCO_ADD_COST.

** Internal table
* For Excel Data
DATA : IT_ZSCO_ADD_COST
                  LIKE STANDARD TABLE OF  ZSCO_ADD_COST
                  WITH HEADER LINE .
DATA : IT_INTERN  TYPE KCDE_INTERN WITH HEADER LINE .
* For DDIF
DATA : IT_DFTAB   LIKE STANDARD TABLE OF  DFIES
                  WITH HEADER LINE .
* For Posting
DATA : IT_TRS_DATA TYPE KKPI_TRANSFER_DATA
                   WITH HEADER LINE .


*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-001.
PARAMETERS : P_FNAME LIKE RLGRAP-FILENAME
                          OBLIGATORY
                          DEFAULT 'C:\*.XLS',
             P_EROW  TYPE I OBLIGATORY.
PARAMETERS : P_MODE       OBLIGATORY
                          DEFAULT 'N',
             P_UPDATE     OBLIGATORY
                          DEFAULT 'A'.

SELECTION-SCREEN END OF BLOCK BL1.


*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
INITIALIZATION.


*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FNAME.
* Browsing file path
  PERFORM SELECT_INPUT_FILE_NAME.


*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Get Field info.
  PERFORM READ_FIELD_LIST_DDIF.
* Upload
  PERFORM UPLOAD_FR_EXCEL.
* Making DATA tab
  PERFORM MAKE_DATA_TAB.
* Preparation for posting
  PERFORM PRE_FOR_POSTING.
* Log 1
  PERFORM SHOW_LOG_BF.
* POSTING
  PERFORM POST_ADD_COST.
* No error -> Completed
  PERFORM SHOW_LOG_AF.


*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.


*
*----------------------------------------------------------------------*
* SUB-ROUTINE
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  SELECT_INPUT_FILE_NAME
*&---------------------------------------------------------------------*
*       Browsing File path
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECT_INPUT_FILE_NAME.

  CALL FUNCTION 'F4_FILENAME'
       EXPORTING
            PROGRAM_NAME  = SYST-CPROG
            DYNPRO_NUMBER = SYST-DYNNR
            FIELD_NAME    = ' '
       IMPORTING
            FILE_NAME     = P_FNAME.

ENDFORM.                    " SELECT_INPUT_FILE_NAME

*&---------------------------------------------------------------------*
*&      Form  UPLOAD_FR_EXCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPLOAD_FR_EXCEL.

  CLEAR : IT_INTERN, IT_INTERN[].

* Cal. The No. of End.COL.
  DATA : LV_END_COL LIKE SY-TFILL.
  DESCRIBE TABLE   IT_DFTAB LINES LV_END_COL.

* Uploading
  CALL FUNCTION 'KCD_EXCEL_OLE_TO_INT_CONVERT'
       EXPORTING
            FILENAME                = P_FNAME
            I_BEGIN_COL             = '1'
            I_BEGIN_ROW             = '1'
            I_END_COL               = LV_END_COL
            I_END_ROW               = P_EROW
       TABLES
            INTERN                  = IT_INTERN
       EXCEPTIONS
            INCONSISTENT_PARAMETERS = 1
            UPLOAD_OLE              = 2
            OTHERS                  = 3.

  IF SY-SUBRC <> 0.
    MESSAGE E000 WITH 'Check your file'.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " UPLOAD_FR_EXCEL

*&---------------------------------------------------------------------*
*&      Form  MAKE_DATA_TAB
*&---------------------------------------------------------------------*
*       Making Data Tab
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_DATA_TAB.

  CLEAR : IT_ZSCO_ADD_COST, IT_ZSCO_ADD_COST[].

  FIELD-SYMBOLS : <FS> TYPE ANY.
  DATA : LV_FNAME(60).
  SORT IT_INTERN BY ROW COL.

  LOOP AT IT_INTERN.
* New line
    AT NEW ROW.
      CLEAR IT_ZSCO_ADD_COST.
    ENDAT.

* Set Value
    CLEAR IT_DFTAB.
    READ TABLE IT_DFTAB WITH KEY POSITION = IT_INTERN-COL.
    IF SY-SUBRC = 0.
      CLEAR LV_FNAME.
      CONCATENATE 'IT_ZSCO_ADD_COST'
                  '-'
                  IT_DFTAB-FIELDNAME
             INTO LV_FNAME.

      ASSIGN (LV_FNAME) TO <FS>.
* IT_ZSCO_ADD_COST-(IT_DFTAB-FIELDNAME) = IT_INTERN-VALUE.
      <FS> = IT_INTERN-VALUE.
    ENDIF.

* Append
    AT END OF ROW.
      APPEND IT_ZSCO_ADD_COST.
      CLEAR  IT_ZSCO_ADD_COST.
    ENDAT.

    CLEAR IT_INTERN.
  ENDLOOP.

  CLEAR  IT_ZSCO_ADD_COST.

  SORT IT_ZSCO_ADD_COST BY KEY.

ENDFORM.                    " MAKE_DATA_TAB

*&---------------------------------------------------------------------*
*&      Form  READ_FIELD_LIST_DDIF
*&---------------------------------------------------------------------*
*       Read Field info.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_FIELD_LIST_DDIF.

  CLEAR : IT_DFTAB, IT_DFTAB[].

  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      TABNAME              = 'ZSCO_ADD_COST'
*     FIELDNAME            = ' '
*     LANGU                = SY-LANGU
*     LFIELDNAME           = ' '
*     ALL_TYPES            = ' '
*   IMPORTING
*     X030L_WA             =
*     DDOBJTYPE            =
*     DFIES_WA             =
*     LINES_DESCR          =
    TABLES
      DFIES_TAB            = IT_DFTAB
*     FIXED_VALUES         =
   EXCEPTIONS
     NOT_FOUND            = 1
     INTERNAL_ERROR       = 2
     OTHERS               = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " READ_FIELD_LIST_DDIF

*&---------------------------------------------------------------------*
*&      Form  PRE_FOR_POSTING
*&---------------------------------------------------------------------*
*       Preparation for posting
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRE_FOR_POSTING.
*KKPI_BDC_ADD_ON_COSTS_CREATE
*
  DATA : IT_L_POS LIKE IT_TRS_DATA-POSITION
                  WITH HEADER LINE .

  CLEAR : IT_TRS_DATA, IT_TRS_DATA[].

  SORT IT_ZSCO_ADD_COST BY KEY.

  LOOP AT IT_ZSCO_ADD_COST.
    AT NEW KEY.
      CLEAR : IT_L_POS, IT_L_POS[].
      CLEAR IT_TRS_DATA.
      MOVE-CORRESPONDING IT_ZSCO_ADD_COST TO IT_TRS_DATA.
    ENDAT.

    MOVE-CORRESPONDING IT_ZSCO_ADD_COST TO IT_L_POS.
    APPEND IT_L_POS.
    CLEAR  IT_L_POS.

    AT END OF KEY.
      IT_TRS_DATA-POSITION[] = IT_L_POS[].
      CLEAR : IT_L_POS, IT_L_POS[].
      APPEND IT_TRS_DATA.
      CLEAR  IT_TRS_DATA.
    ENDAT.
  ENDLOOP.

  CLEAR  IT_TRS_DATA.

ENDFORM.                    " PRE_FOR_POSTING

*&---------------------------------------------------------------------*
*&      Form  SHOW_LOG_BF
*&---------------------------------------------------------------------*
*       Log
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SHOW_LOG_BF.
  ULINE .
  WRITE : / 'Before posting additive costs'.
  ULINE .
  WRITE : / 'The restricted record count in Execl file :', P_EROW.
  DESCRIBE TABLE IT_ZSCO_ADD_COST LINES SY-TFILL.
  WRITE : / 'The number of data record for posting     :', SY-TFILL.
  ULINE .
  SKIP 4.
ENDFORM.                    " SHOW_LOG_BF

*&---------------------------------------------------------------------*
*&      Form  POST_ADD_COST
*&---------------------------------------------------------------------*
*       Post
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM POST_ADD_COST.
*IT_TRS_DATA

  CALL FUNCTION 'KKPI_BDC_ADD_ON_COSTS_CREATE'
    EXPORTING
*   CLIENT                       = SY-MANDT
      USER                         = SY-UNAME
      MODE                         = P_MODE
      UPDATE                       = P_UPDATE
      IT_TRANSFER_DATA             = IT_TRS_DATA[]
    EXCEPTIONS
      CALL_TRANSACTION_ERROR       = 1
      NO_TRANSFER_DATA             = 2
      WRONG_MODE_PARAMETER         = 3
      OTHERS                       = 4.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " POST_ADD_COST

*&---------------------------------------------------------------------*
*&      Form  SHOW_LOG_AF
*&---------------------------------------------------------------------*
*       Log 2
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SHOW_LOG_AF.
  ULINE .
  WRITE : / 'Completed'.
  WRITE : / 'End of Process'.

ENDFORM.                    " SHOW_LOG_AF
