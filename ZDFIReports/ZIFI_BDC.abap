***INCLUDE Zifi_bdc.
*&---------------------------------------------------------------------*
*& Report      : ZINC_BDC                                              *
*& Description : BDC program                                           *
*& Create date : 2002.4.25                                             *
*& Create by   : HSJEONG                                               *
*&---------------------------------------------------------------------*

************************************************************************
* DATA ??                                                            *
************************************************************************
DATA : FLENGTH    TYPE I.                   " Upload File Length

DATA : BEGIN OF TIME_WA,
         START    LIKE SY-UZEIT,            " Start Time
         END      LIKE SY-UZEIT,            " End Time
         RUN      LIKE SY-UZEIT,            " ????
       END OF TIME_WA.
*........................................... BDC Structure Define .....*
DATA : BEGIN OF BDCDATA OCCURS 0.
INCLUDE  STRUCTURE BDCDATA.
DATA : END OF BDCDATA.

*............. Call Transaction ??? System Return Message ?? .....*
DATA : BEGIN OF MESSTAB OCCURS 0.
INCLUDE  STRUCTURE BDCMSGCOLL.
DATA : END OF MESSTAB.

*............. LUW ??
DATA: BEGIN OF IS_OPT ,
       DISMODE  LIKE CTU_PARAMS-DISMODE,
       UPDMODE  LIKE CTU_PARAMS-UPDMODE,
       CATTMODE LIKE CTU_PARAMS-CATTMODE VALUE   ' ',
       DEFSIZE  LIKE CTU_PARAMS-DEFSIZE  VALUE   ' ',
       RACOMMIT LIKE CTU_PARAMS-RACOMMIT VALUE   'X',
       NOBINPT  LIKE CTU_PARAMS-NOBINPT  VALUE   'X',
       NOBIEND  LIKE CTU_PARAMS-NOBIEND  VALUE   'X',
      END OF IS_OPT.

************************************************************************
* Select-options & Parameters                                          *
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK BL0 WITH FRAME.
PARAMETERS : P_AS   AS CHECKBOX MODIF ID BL0.
SELECTION-SCREEN END OF BLOCK BL0.

*SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-001.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS : P_CT   RADIOBUTTON GROUP R1 MODIF ID BL1." Call Transaction
SELECTION-SCREEN COMMENT 5(20) TEXT-004 MODIF ID BL1.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 7(20) TEXT-005 MODIF ID BL1.
PARAMETERS : P_MODE LIKE RFPDO-ALLGAZMD DEFAULT 'N'   " BDC mode
                    MODIF ID BL1.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 7(20) TEXT-006 MODIF ID BL1.
PARAMETERS : P_UP   LIKE RFPDO-ALLGVBMD DEFAULT 'S'   " BDC update mode.
                    MODIF ID BL1.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 7.
PARAMETERS : P_ES   AS CHECKBOX DEFAULT 'X'           " Error? ????
                    MODIF ID BL1.
SELECTION-SCREEN COMMENT 10(70) TEXT-003 MODIF ID BL1.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS : P_CS   RADIOBUTTON GROUP R1              " Create Session
                    MODIF ID BL1.
SELECTION-SCREEN COMMENT 5(20) TEXT-007 MODIF ID BL1.
SELECTION-SCREEN END OF LINE.

*SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK BL11 WITH FRAME.
PARAMETERS : P_GROUP(12) TYPE C DEFAULT SY-REPID      " BDC Session name
                         MODIF ID BL3,
             P_USER(12)  TYPE C DEFAULT SY-UNAME      " User Name
                         MODIF ID BL3,
             P_KEEP      AS CHECKBOX   "' ' = delete session if finished
                         MODIF ID BL3, "'X' = keep   session if finished
             P_HOLDDT    LIKE SY-DATUM DEFAULT SY-DATUM  " Create Date
                         MODIF ID BL3.
SELECTION-SCREEN END OF BLOCK BL11.
SELECTION-SCREEN END OF BLOCK BL1.

*SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK BL2 WITH FRAME.
PARAMETERS : P_TEST AS CHECKBOX MODIF ID BL2.        " Test Run
SELECTION-SCREEN END OF BLOCK BL2.

************************************************************************
* AT SELECTION-SCREEN                                                  *
************************************************************************
AT SELECTION-SCREEN.
  IF P_CT EQ 'X'.                      "???? = CALL TRANSACTION
    IF P_MODE IS INITIAL OR P_UP IS INITIAL.
      MESSAGE E001(ZEHR_01) WITH 'BDC MODE? BDC Update MODE?'
                                 '?????'.

    ELSEIF ( P_MODE NE 'A' AND P_MODE NE 'E' AND P_MODE NE 'N' ).
      MESSAGE E001(ZEHR_01) WITH 'BDC MODE? A, E, N? ??'
                                 '???????.'.
    ELSEIF ( P_UP NE 'S' AND P_UP NE 'A' ).
      MESSAGE E001(ZEHR_01) WITH 'BDC Update MODE? A, S? ??'
                                 '???????.'.
    ENDIF.
  ENDIF.
  IF ( P_ES EQ 'X' OR P_CS EQ 'X' ).    "SESSION ??
    IF P_GROUP IS INITIAL OR P_USER IS INITIAL.
      MESSAGE E001(ZEHR_01) WITH 'BDC Session Name? User Name?'
                                 '?????'.
    ENDIF.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  BDC_OPEN_GROUP
*&---------------------------------------------------------------------*
*       Creating the session header                                    *
*----------------------------------------------------------------------*
FORM BDC_OPEN_GROUP.
  CALL FUNCTION 'BDC_OPEN_GROUP'
       EXPORTING
            CLIENT = SY-MANDT
            GROUP  = P_GROUP
            USER   = P_USER
            KEEP   = P_KEEP. "Do not delete session after processing
  IF SY-SUBRC NE 0.
    MESSAGE A001(ZEHR_01) WITH 'BDC Session' P_GROUP
                          '? ??? ?? ?????!(open error)'.
*   BDC Session (&1)? ??? ?? ????? !!! [&2]
  ENDIF.
  PERFORM WRITE_TIME_PROC USING 'S'.
ENDFORM.                               " BDC_OPEN_GROUP_PROC

*&---------------------------------------------------------------------*
*&      Form  BDC_INSERT
*&---------------------------------------------------------------------*
*       Insert BDC table into session                                  *
*----------------------------------------------------------------------*
*  -->  fp_tcode : BDC ?? ? Transaction Code
*----------------------------------------------------------------------*
FORM BDC_INSERT USING FP_TCODE.
  CALL FUNCTION 'BDC_INSERT'
       EXPORTING
            TCODE     = FP_TCODE
       TABLES
            DYNPROTAB = BDCDATA.

  IF SY-SUBRC NE 0.
    WRITE : 'ERROR ??? :  BDC_INSERT !!!',
            'Rec.No.      : ', SY-TABIX.
    EXIT.
  ENDIF.
ENDFORM.                               " BDC_INSERT_PROC

*&---------------------------------------------------------------------*
*&      Form  BDC_CLOSE_GROUP
*&---------------------------------------------------------------------*
*       Closing the completed session                                  *
*----------------------------------------------------------------------*
FORM BDC_CLOSE_GROUP.
  CALL FUNCTION  'BDC_CLOSE_GROUP'.

  IF SY-SUBRC NE 0.
    MESSAGE A001(ZEHR_01) WITH 'BDC Session' P_GROUP
                               '? ??? ???????!(close error)'.
*   BDC Session (&1)? ??? ?? ????? !!! [&2]
  ELSE.
    MESSAGE I001(ZEHR_01) WITH 'BDC Session' P_GROUP
                               '? ??? ???????!'.
*   BDC Session (&1)? ??? ?? ????? !!!
  ENDIF.
  PERFORM WRITE_TIME_PROC USING 'E'.
ENDFORM.                               " BDC_CLOSE_GROUP_PROC

*&---------------------------------------------------------------------*
*&      Form  BDC_DYNPRO
*&---------------------------------------------------------------------*
*       ?? itab? bdcdata? fill                                    *
*----------------------------------------------------------------------*
*  -->  fp_dynbegin : screen start ??
*  -->  fp_name     : dynbegin = 'X' -> Program Name
*                     dynbegin = ' ' -> Field Name
*  -->  fp_value    : dynbegin = 'X' -> Screen Number
*                     dynbegin = ' ' -> Field Value
*----------------------------------------------------------------------*
FORM BDC_DYNPRO USING FP_DYNBEGIN
                      FP_NAME
                      FP_VALUE.
  CLEAR BDCDATA.

  IF FP_DYNBEGIN = 'X'.
    MOVE: FP_NAME  TO BDCDATA-PROGRAM,
          FP_VALUE TO BDCDATA-DYNPRO,
          'X'      TO BDCDATA-DYNBEGIN.
  ELSE.
    MOVE: FP_NAME  TO BDCDATA-FNAM,
          FP_VALUE TO BDCDATA-FVAL.
  ENDIF.

  APPEND BDCDATA.
ENDFORM.                               " DYNPRO_PROC

*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION_OPT
*&---------------------------------------------------------------------*
*       Transaction ??                                               *
*       CTU_PARAMS STRUCTURE? ????,                               *
*       Transaction ??? AUTO COMMIT? ????.                     *
*----------------------------------------------------------------------*
*  -->  fp_tcode  : Transaction Code
*  -->  fp_mode   : Display mode
*                   .A :Display all
*                   .E : Display only if there are errors
*                   .N : Display nothing
*  -->  fp_update : Update mode
*                   .S : Do not continue processing until update
*                         has finished(Synchronous update)
*                   .A : Continue processing immediately(Asynchronous)
*----------------------------------------------------------------------*
FORM CALL_TRANSACTION_OPT USING FP_TCODE
                                FP_MODE
                                FP_UPDATE
                                FP_RCODE.

  DATA : CTU_PARAMS LIKE CTU_PARAMS.

  CLEAR CTU_PARAMS.

  CTU_PARAMS-DISMODE  = FP_MODE.
  CTU_PARAMS-UPDMODE  = FP_UPDATE.
  CTU_PARAMS-CATTMODE = ' '.
  CTU_PARAMS-DEFSIZE  = ' '.
  CTU_PARAMS-RACOMMIT = 'X'.
  CTU_PARAMS-NOBINPT  = 'X'.
  CTU_PARAMS-NOBIEND  = 'X'.

  CALL TRANSACTION FP_TCODE USING   BDCDATA
                            OPTIONS FROM CTU_PARAMS
                            MESSAGES INTO MESSTAB.

*  FP_RCODE = SY-SUBRC.
ENDFORM.                               " CALL_TRANSACTION_PROC

*&---------------------------------------------------------------------*
*&      Form  WRITE_TIME_PROC
*&---------------------------------------------------------------------*
*       ??? ???? ? ????? Write                            *
*----------------------------------------------------------------------*
*  -->  fp_status : ?? ?? ?? ( S - Start, E - End )
*----------------------------------------------------------------------*
FORM WRITE_TIME_PROC USING FP_STATUS.
*  DATA: WA_STATUS.
*
*  WA_STATUS = FP_STATUS.
*  TRANSLATE WA_STATUS TO UPPER CASE.
*
*  CASE WA_STATUS.
*    WHEN 'S'.
*      TIME_WA-START = SY-UZEIT.
*      WRITE: / 'Start Time :',  TIME_WA-START.
*
*    WHEN 'E'.
*      TIME_WA-END = SY-UZEIT.
*      TIME_WA-RUN = TIME_WA-END - TIME_WA-START.
*      WRITE: / 'End   Time :', TIME_WA-END, '(', TIME_WA-RUN, ')'.
*  ENDCASE.
ENDFORM.                               " WRITE_TIME_PROC

*&---------------------------------------------------------------------*
*&      Form  UPLOAD
*&---------------------------------------------------------------------*
*       local file ? ??? internal table? ??                     *
*----------------------------------------------------------------------*
*  -->  FILENAME : file name
*----------------------------------------------------------------------*
FORM UPLOAD TABLES ITAB.
  DATA : FILENAME   LIKE RLGRAP-FILENAME.

  CALL FUNCTION 'UPLOAD'
       EXPORTING
            CODEPAGE                = 'ASC'
            FILETYPE                = 'DAT'
       IMPORTING
            FILESIZE                = FLENGTH
            ACT_FILENAME            = FILENAME  "File Location
       TABLES
            DATA_TAB                = ITAB
       EXCEPTIONS
            CONVERSION_ERROR        = 1
            INVALID_TABLE_WIDTH     = 2
            INVALID_TYPE            = 3
            NO_BATCH                = 4
            UNKNOWN_ERROR           = 5
            GUI_REFUSE_FILETRANSFER = 6.

*  WRITE :/1 'Source File Name   : ', FILENAME.
*  WRITE :/1 'Source File Length : ', FLENGTH.
ENDFORM.                               "from read_excel_file

*----------------------------------------------------------------------*
*    FORM READ_MSG_TXT
*----------------------------------------------------------------------*
FORM READ_MSG_TXT TABLES MESSTAB
                  CHANGING MSG_TXT.

  DATA : L_CNT    LIKE SY-TABIX,
         MSG      LIKE BDCMSGCOLL.

  DESCRIBE TABLE MESSTAB LINES L_CNT.
  CLEAR MSG.
  READ TABLE MESSTAB INDEX L_CNT INTO MSG.

  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
       EXPORTING
            MSGID               = MSG-MSGID
            MSGNR               = MSG-MSGNR
            MSGV1               = MSG-MSGV1
            MSGV2               = MSG-MSGV2
            MSGV3               = MSG-MSGV3
            MSGV4               = MSG-MSGV4
       IMPORTING
            MESSAGE_TEXT_OUTPUT = MSG_TXT.
ENDFORM.                               " READ_MSG_TXT

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_RESULT
*&---------------------------------------------------------------------*
*       BDC result ? ??? ??
*----------------------------------------------------------------------*
*      -->FP_T_CNT  : total count
*      -->FP_S_CNT  : success count
*      -->FP_E_CNT  : error count
*----------------------------------------------------------------------*
FORM DISPLAY_RESULT USING FP_T_CNT
                          FP_S_CNT
                          FP_E_CNT.
  WRITE : /1 '========================',
          /1 '       BDC RESULT       ',
          /1 '========================'.
  SKIP.
  WRITE : /1 ' Total   : ', 12(12) FP_T_CNT RIGHT-JUSTIFIED,
          /1 ' Success : ', 12(12) FP_S_CNT RIGHT-JUSTIFIED,
          /1 ' Error   : ', 12(12) FP_E_CNT RIGHT-JUSTIFIED.
ENDFORM.                    " display_result

*&---------------------------------------------------------------------*
*&      Form  COMMIT_WORK
*&---------------------------------------------------------------------*
*       COMMIT WORK
*----------------------------------------------------------------------*
FORM COMMIT_WORK.
  COMMIT WORK.
ENDFORM.                    " FORM COMMIT_WORK

*&---------------------------------------------------------------------*
*&      Form  ROLLBACK_WORK.
*&---------------------------------------------------------------------*
*       ROLLBACK WORK.
*----------------------------------------------------------------------*
FORM ROLLBACK_WORK.
  ROLLBACK WORK.
ENDFORM.                    " FORM ROLLBACK_WORK.

*&-------------------------------------------------------------------
*&      Form  F4_FILE_NAME
*&-------------------------------------------------------------------
*       FILE SEARCH??
*--------------------------------------------------------------------
*       <--FP_FILE : ????
*       <--FP_RCODE : RETURN CODE
*--------------------------------------------------------------------
FORM F4_FILE_NAME CHANGING FP_FILE
                           FP_RCODE.
  DATA : L_REPID           LIKE SY-REPID.
  MOVE SY-REPID  TO  L_REPID.
*.P_AS ?? ???? ????.
  CALL FUNCTION 'GET_DYNP_VALUE'
    EXPORTING
      I_FIELD             = 'P_AS'
      I_REPID             = L_REPID
      I_DYNNR             = '1000'
    CHANGING
      O_VALUE             = P_AS.
*.PC?? ?? SEARCH.
  IF P_AS EQ SPACE.
    CALL FUNCTION 'WS_FILENAME_GET'
         EXPORTING
              DEF_FILENAME     = FP_FILE  " ???
              DEF_PATH         = 'C:\'  " ?? ??? PATH
              MASK             = ',*.*,*.*.'  " ????? ???
              MODE             = SPACE  " OPEN('')/SAVE('S') ??
              TITLE            = '?? ??'  " Dialog Title
         IMPORTING
              FILENAME         = FP_FILE  " Return ???
         EXCEPTIONS
              INV_WINSYS       = 01
              NO_BATCH         = 02
              SELECTION_CANCEL = 03
              SELECTION_ERROR  = 04.
    FP_RCODE = SY-SUBRC.
*.APPLICATION SERVER?? ?? SEARCH.
  ELSE.
    CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
         EXPORTING
              DIRECTORY        = ' '
              FILEMASK         = ' '
         IMPORTING
              SERVERFILE       = FP_FILE
         EXCEPTIONS
              CANCELED_BY_USER = 1
              OTHERS           = 2.
  ENDIF.
ENDFORM.                    " F4_FILE_NAME

*&-------------------------------------------------------------------
*&      Form  CALL_TRANSACTION
*&-------------------------------------------------------------------
*       Transaction ?? : OPTION ??
*&-------------------------------------------------------------------
FORM CALL_TRANSACTION USING FP_TCODE
                            FP_MODE
                            FP_UPDATE
                            FP_RCODE.
  IS_OPT-DISMODE  = FP_MODE.
  IS_OPT-UPDMODE  = FP_UPDATE.

  CALL TRANSACTION FP_TCODE
       USING       BDCDATA
       OPTIONS FROM IS_OPT
       MESSAGES    INTO MESSTAB.

  FP_RCODE = SY-SUBRC.
ENDFORM.                    " CALL_TRANSACTION

*&---------------------------------------------------------------------*
*       internal table ? ??? local file? ??
*----------------------------------------------------------------------*
*  -->  FILENAME : file name
*----------------------------------------------------------------------*
FORM WS_DOWNLOAD TABLES ITAB
                 USING  FILENAME LIKE RLGRAP-FILENAME.

  CALL FUNCTION 'WS_DOWNLOAD'
       EXPORTING
            CODEPAGE                   = 'ASC'
            FILENAME                   = FILENAME  "File Location
            FILETYPE                   = 'DAT'
       TABLES
            DATA_TAB                   = ITAB
       EXCEPTIONS
            FILE_OPEN_ERROR            = 1
            FILE_WRITE_ERROR           = 2
            INVALID_FILESIZE           = 3
            INVALID_TYPE               = 4
            NO_BATCH                   = 5
            UNKNOWN_ERROR              = 6
            INVALID_TABLE_WIDTH        = 7
            GUI_REFUSE_FILETRANSFER    = 8
            CUSTOMER_ERROR             = 9.
  CHECK SY-SUBRC = 0.
  SKIP 4.
  WRITE :/1 'Error File Download   : ', FILENAME.
  CALL FUNCTION 'WS_EXECUTE'
       EXPORTING
            PROGRAM            = FILENAME
       EXCEPTIONS
            FRONTEND_ERROR     = 1
            NO_BATCH           = 2
            PROG_NOT_FOUND     = 3
            ILLEGAL_OPTION     = 4
            GUI_REFUSE_EXECUTE = 5
            OTHERS             = 6.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                               "FORM WS_UPLOAD

*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION_PROC2
*&---------------------------------------------------------------------*
*       Transaction ??                                               *
*----------------------------------------------------------------------*
*  -->  fp_tcode  : Transaction Code
*  -->  fp_mode   : Display mode
*                   .A :Display all
*                   .E : Display only if there are errors
*                   .N : Display nothing
*  -->  fp_update : Update mode
*                   .S : Do not continue processing until update
*                         has finished(Synchronous update)
*                   .A : Continue processing immediately(Asynchronous)
*----------------------------------------------------------------------*
FORM CALL_TRANSACTION_PROC2 USING FP_TCODE
                                  FP_MODE
                                  FP_UPDATE
                                  FP_RCODE.
  CALL TRANSACTION FP_TCODE
       USING       BDCDATA
       MODE        FP_MODE
       UPDATE      FP_UPDATE
       MESSAGES    INTO MESSTAB.
  FP_RCODE = SY-SUBRC.
ENDFORM.                    " CALL_TRANSACTION_PROC1

*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION_PROC
*&---------------------------------------------------------------------*
*       Transaction ??                                               *
*----------------------------------------------------------------------*
*  -->  fp_tcode  : Transaction Code
*  -->  fp_mode   : Display mode
*                   .A :Display all
*                   .E : Display only if there are errors
*                   .N : Display nothing
*  -->  fp_update : Update mode
*                   .S : Do not continue processing until update
*                         has finished(Synchronous update)
*                   .A : Continue processing immediately(Asynchronous)
*----------------------------------------------------------------------*
FORM CALL_TRANSACTION_PROC USING FP_TCODE
                                 FP_MODE
                                 FP_UPDATE
                                 FP_RCODE.
  CALL TRANSACTION FP_TCODE
       USING       BDCDATA
       MODE        FP_MODE
       UPDATE      FP_UPDATE
       MESSAGES    INTO MESSTAB.

  FP_RCODE = SY-SUBRC.
ENDFORM.                    " CALL_TRANSACTION_PROC

*&---------------------------------------------------------------------*
*&      Form  OPEN_DATASET                                             *
*&---------------------------------------------------------------------*
*       OPEN DATASET                                                   *
*----------------------------------------------------------------------*
*       -->FP_DSN : ???? & ???                                  *
*       -->FP_IOA : 'INPUT', 'OUTPUT', 'APPENDING'                     *
*       -->FP_MODE : 'BINARY', 'TEXT'                                  *
*       -->FP_FILTER : '' W/O filter, OTHERS with filter               *
*----------------------------------------------------------------------*
FORM OPEN_DATASET USING FP_DSN
                        FP_IOA
                        FP_MODE
                        FP_FILTER
                        FP_RCODE.

  DATA : L_TEMP(200) TYPE C.
  FIELD-SYMBOLS : <FS>.

  IF FP_DSN NE SPACE.
    CONCATENATE '''' FP_DSN '''' INTO L_TEMP.
  ENDIF.

  IF FP_IOA NE SPACE.
    CONCATENATE L_TEMP 'FOR' FP_IOA INTO L_TEMP SEPARATED BY SPACE.
  ENDIF.

  IF FP_MODE NE SPACE.
    CONCATENATE L_TEMP 'IN' FP_MODE 'MODE'
                        INTO L_TEMP SEPARATED BY SPACE.
  ENDIF.

  IF FP_FILTER NE SPACE.
    CONCATENATE L_TEMP 'FILTER ''' FP_FILTER ''''
                       INTO L_TEMP SEPARATED BY SPACE.
  ENDIF.

  ASSIGN L_TEMP TO <FS>.
  OPEN DATASET <FS>.
  FP_RCODE = SY-SUBRC.
ENDFORM.                    " OPEN_DATASET

*&---------------------------------------------------------------------*
*&      Form  CLOSE_DATASET                                            *
*&---------------------------------------------------------------------*
*       CLOSE DATASET                                                  *
*----------------------------------------------------------------------*
*       -->FP_DSN : ???? & ???                                  *
*----------------------------------------------------------------------*
FORM CLOSE_DATASET USING FP_DSN.
  CLOSE DATASET FP_DSN.
  IF sy-subrc NE 0.
    WRITE :/1 '??!', FP_DSN, '? Close Dataset ?? ?????.'.
  ELSE.
    WRITE :/1 FP_DSN, '????? ?? ?????.'.
  ENDIF.
ENDFORM.                    " CLOSE_DATASET

*&---------------------------------------------------------------------*
*&      Form  READ_DATASET
*&---------------------------------------------------------------------*
*       READ DATASET
*----------------------------------------------------------------------*
FORM READ_DATASET USING    FP_FILENAME
                           FP_ITAB
                           FP_RCODE.
  READ DATASET FP_FILENAME INTO FP_ITAB.
  FP_RCODE = SY-SUBRC.
ENDFORM.                    " READ_DATASET
