************************************************************************
* Program Name      : ZCPP103U_0010
* Author            : JongOh, Kim
* Creation Date     : 2003.08.16.
* Specifications By : JongOh, Kim
* Development Request No : UD1K901950
* Addl Documentation:
* Description       : VARIANT TABLE CREATION (BDC T-Code: CU61)
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT  ZCPP103_0010 NO STANDARD PAGE HEADING
                        LINE-SIZE 120
                        MESSAGE-ID ZMPP.
*----------------------------------------------------------------------*
*  INTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
*-----> UPLOAD TABLE
DATA : BEGIN OF IT_UPLOAD OCCURS 0,
       VTNAM   LIKE  RCUTU-VTNAM,    "TABLE NAME
       VTTXT   LIKE  RCUTU-VTTXT,    "TABLE TEXT
*        VTSTA   LIKE  RCUTU-VTSTA,    "STATUS
       VTGRU   LIKE  RCUTU-VTGRU,    "GROUP
       ATNAM   LIKE  RCUTU-ATNAM,    "CHARACTERISTICS
       ATKEY   LIKE  RCUTU-ATKEY.    "KEY FIELD
DATA : END OF IT_UPLOAD.

*-----> HEADER TABLE
DATA : BEGIN OF IT_HEADER OCCURS 0,
       VTNAM   LIKE  RCUTU-VTNAM.    "TABLE NAME
*       VTTXT   LIKE  RCUTU-VTTXT,    "TABLE TEXT
*       VTGRU   LIKE  RCUTU-VTGRU.    "GROUP
DATA : END OF IT_HEADER.

*-----> ERROR TABLE
DATA : BEGIN OF IT_ERROR OCCURS 0,
       VTNAM   LIKE  RCUTU-VTNAM,    "TABLE NAME
       MSGTY   LIKE  SY-MSGTY,       "STATUS
       MSG     LIKE  CFGNL-MSGLIN.   "MESSAGE
DATA : END OF IT_ERROR.

*----------------------------------------------------------------------*
*  INTERNAL TABLE DECLARATION (BDC DATA)
*----------------------------------------------------------------------*
DATA : IT_BDCDATA  LIKE  TABLE OF BDCDATA  WITH HEADER LINE.

*----------------------------------------------------------------------*
*  GLOBAL VARIABLE DECLARATION
*----------------------------------------------------------------------*
*------> BDC MODE ('A' DISPLAY SCREEN, 'N' NO DISPLAY)
DATA: WA_MODE.        "BDC MODE

*----------------------------------------------------------------------*
*  GLOBAL VARIABLE DECLARATION
*----------------------------------------------------------------------*
CONSTANTS: C_MARK   VALUE 'X'.

*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.
PARAMETERS : P_FILES LIKE RLGRAP-FILENAME OBLIGATORY.
SELECTION-SCREEN END OF BLOCK B1.

PARAMETERS : RA_1  RADIOBUTTON GROUP RA DEFAULT 'X',
             RA_2  RADIOBUTTON GROUP RA.


*------> AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILES
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILES.
  PERFORM VALUE_REQUEST.

*------> AT SELECTION-SCREEN.
AT SELECTION-SCREEN.
  PERFORM CHECK_PROCESS.

*------> START-OF-SELECTION.
START-OF-SELECTION.
  PERFORM EXECUTE_PROCESS.

*------> END-OF-SELECTION.
END-OF-SELECTION.
  PERFORM LIST_PROCESS.

*&---------------------------------------------------------------------*
*&      Form  VALUE_REQUEST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM VALUE_REQUEST.
  CALL FUNCTION 'WS_FILENAME_GET'
       EXPORTING
            DEF_PATH         = 'C:\TEMP\'
            MASK             = ',*.TXT,*.TXT.'
            MODE             = 'O'
       IMPORTING
            FILENAME         = P_FILES
       EXCEPTIONS
            INV_WINSYS       = 01
            NO_BATCH         = 02
            SELECTION_CANCEL = 03
            SELECTION_ERROR  = 04.
ENDFORM.                    " VALUE_REQUEST

*&---------------------------------------------------------------------*
*&      Form  CHECK_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_PROCESS.
  IF SY-UCOMM EQ 'ONLI'.
    CLEAR : IT_UPLOAD[], IT_HEADER[].
    CALL FUNCTION 'WS_UPLOAD'
         EXPORTING
              FILENAME                = P_FILES
              FILETYPE                = 'DAT'
         TABLES
              DATA_TAB                = IT_UPLOAD
         EXCEPTIONS
              CONVERSION_ERROR        = 1
              FILE_OPEN_ERROR         = 2
              FILE_READ_ERROR         = 3
              INVALID_TYPE            = 4
              NO_BATCH                = 5
              UNKNOWN_ERROR           = 6
              INVALID_TABLE_WIDTH     = 7
              GUI_REFUSE_FILETRANSFER = 8
              CUSTOMER_ERROR          = 9
              OTHERS                  = 10.

    IF SY-SUBRC <> 0.
      MESSAGE E000 WITH 'UPLOAD ÆÄÀÏ ERROR!!'.
    ELSE.
      LOOP AT IT_UPLOAD.
        CLEAR IT_HEADER.
        MOVE-CORRESPONDING IT_UPLOAD TO IT_HEADER.
        APPEND IT_HEADER.
      ENDLOOP.
      DELETE ADJACENT DUPLICATES FROM IT_HEADER.
    ENDIF.
  ENDIF.
ENDFORM.                    " CHECK_PROCESS

*&---------------------------------------------------------------------*
*&      Form  EXECUTE_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EXECUTE_PROCESS.
  LOOP AT IT_HEADER.
    PERFORM GENERATE_BDC_DATA.
    PERFORM CALL_TRANSACTION.
  ENDLOOP.
ENDFORM.                    " EXECUTE_PROCESS

*&---------------------------------------------------------------------*
*&      Form  GENERATE_BDC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0016   text
*----------------------------------------------------------------------*
FORM GENERATE_BDC_DATA.
  DATA : L_TABIX    LIKE   SY-TABIX,
         L_DATE(10).

  WRITE SY-DATUM  TO  L_DATE.
  CLEAR L_TABIX.
  L_TABIX = 1.
  LOOP AT IT_UPLOAD WHERE  VTNAM  EQ IT_HEADER-VTNAM.
*                      AND  VTTXT  EQ IT_HEADER-VTTXT
*                      AND  VTGRU  EQ IT_HEADER-VTGRU.
    PERFORM BDC_DYNPRO  USING 'SAPLCUTU' '0100'.
    PERFORM BDC_FIELD   USING : 'BDC_CURSOR' 'RCUTU-VTNAM',
                                'BDC_OKCODE' '=BASD',
                                'RCUTU-VTNAM' IT_UPLOAD-VTNAM,
                                'RCUTU-DATUM' L_DATE.

    PERFORM BDC_DYNPRO  USING 'SAPLCUTU' '0200'.
    PERFORM BDC_FIELD   USING : 'BDC_CURSOR' 'RCUTU-VTDCT',
                                'BDC_OKCODE' '=PARM',
                                'RCUTU-VTTXT' IT_UPLOAD-VTTXT,
                                'RCUTU-VTSTA' '1', "<--DEFAULT
                                'RCUTU-VTGRU' IT_UPLOAD-VTGRU,
                                'RCUTU-VTDCT' 'X'. "<--DEFAULT
*-----> 1ST LINE
    IF L_TABIX EQ 1.
      PERFORM BDC_DYNPRO   USING 'SAPLCUTU' '0300'.
      PERFORM BDC_FIELD    USING : 'BDC_CURSOR' 'RCUTU-ATNAM(01)',
                                   'BDC_OKCODE' '/00',
                                   'RCUTU-ATNAM(01)' IT_UPLOAD-ATNAM.
      PERFORM BDC_FIELD    USING : 'BDC_CURSOR' 'RCUTU-ATKEY(01)',
                                   'BDC_OKCODE' '/00',
                                   'RCUTU-ATKEY(01)' IT_UPLOAD-ATKEY.
      L_TABIX = L_TABIX + 1.
    ELSE.
*-----> 2ND LINE ~
      PERFORM BDC_FIELD    USING 'BDC_OKCODE' '=NEWF'.
      PERFORM BDC_DYNPRO   USING 'SAPLCUTU' '0300'.
      PERFORM BDC_FIELD    USING : 'BDC_CURSOR' 'RCUTU-ATNAM(02)',
                                   'BDC_OKCODE' '/00',
                                   'RCUTU-ATNAM(02)' IT_UPLOAD-ATNAM.
      PERFORM BDC_FIELD    USING : 'BDC_CURSOR' 'RCUTU-ATKEY(02)',
                                   'BDC_OKCODE' '/00',
                                   'RCUTU-ATKEY(02)' IT_UPLOAD-ATKEY.
    ENDIF.
  ENDLOOP.
*-----> SAVE
  PERFORM BDC_FIELD   USING 'BDC_OKCODE' '=SAVE'.
ENDFORM.                    " GENERATE_BDC_DATA

*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_TRANSACTION.
  CASE  C_MARK.
    WHEN  RA_1.
      MOVE  'A'  TO  WA_MODE.
    WHEN  RA_2.
      MOVE  'N'  TO  WA_MODE.
  ENDCASE.
  CALL TRANSACTION 'CU61' USING IT_BDCDATA
                          MODE WA_MODE
                          UPDATE 'S'.
  PERFORM ERROR_TEXT.
  REFRESH IT_BDCDATA.
ENDFORM.                    " CALL_TRANSACTION
*&---------------------------------------------------------------------*
*&      Form  BDC_DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2475   text
*      -->P_2476   text
*----------------------------------------------------------------------*
FORM BDC_DYNPRO USING  P_PROGRAM
                       P_DYNPRO.
  CLEAR IT_BDCDATA.
  IT_BDCDATA-PROGRAM  = P_PROGRAM.
  IT_BDCDATA-DYNPRO   = P_DYNPRO.
  IT_BDCDATA-DYNBEGIN = 'X'.
  APPEND IT_BDCDATA.

ENDFORM.                    " BDC_DYNPRO

*&---------------------------------------------------------------------*
*&      Form  BDC_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2480   text
*      -->P_2481   text
*----------------------------------------------------------------------*
FORM BDC_FIELD USING    P_FNAM
                        P_FVAL.
  IF P_FVAL <> SPACE.       "Nodata.
    CLEAR IT_BDCDATA.
    IT_BDCDATA-FNAM = P_FNAM.
    IT_BDCDATA-FVAL = P_FVAL.
    APPEND IT_BDCDATA.
  ENDIF.

ENDFORM.                    " BDC_FIELD
*&---------------------------------------------------------------------*
*&      Form  ERROR_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ERROR_TEXT.
  DATA L_MSG  LIKE CFGNL-MSGLIN.
  CALL FUNCTION 'RKC_MSG_STRING'
       EXPORTING
            ID      = SY-MSGID
            MTYPE   = SY-MSGTY
            NUMBER  = SY-MSGNO
            PAR1    = SY-MSGV1
            PAR2    = SY-MSGV2
            PAR3    = SY-MSGV3
            PAR4    = SY-MSGV4
       IMPORTING
            MSG_LIN = L_MSG
       EXCEPTIONS
            OTHERS  = 1.

  MOVE : IT_HEADER-VTNAM  TO  IT_ERROR-VTNAM,
         SY-MSGTY         TO  IT_ERROR-MSGTY.
  CASE SY-MSGTY.
    WHEN 'E' OR 'A' OR 'X' OR 'W'.
      MOVE L_MSG            TO  IT_ERROR-MSG.
    WHEN OTHERS.                       " 'I', 'S' :SUCCESS
      MOVE 'CREATE SUCCESS' TO  IT_ERROR-MSG.
  ENDCASE.
  APPEND IT_ERROR.
ENDFORM.                    " ERROR_TEXT
*&---------------------------------------------------------------------*
*&      Form  LIST_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LIST_PROCESS.
  DATA L_LINE  LIKE  SY-INDEX.
  LOOP AT IT_ERROR.
    L_LINE = SY-TABIX MOD 2.
    IF L_LINE EQ 0.
      FORMAT INTENSIFIED ON.
    ELSE.
      FORMAT INTENSIFIED OFF.
    ENDIF.
    WRITE: IT_ERROR-VTNAM COLOR COL_NORMAL,
*           IT_ERROR-MSGTY COLOR COL_NORMAL,
           IT_ERROR-MSG COLOR COL_NORMAL.
  ENDLOOP.
ENDFORM.                    " LIST_PROCESS
