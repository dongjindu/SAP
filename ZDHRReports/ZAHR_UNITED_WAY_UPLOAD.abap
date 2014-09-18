REPORT ZAHR_UNITED_WAY_UPLOAD MESSAGE-ID ZMPP
     NO STANDARD PAGE HEADING LINE-SIZE 105 .
***********************************************************************
* Program Name      : ZAHR_UNITED_WAY_UPLOAD
* Author            : Furong
* Creation Date     : 11/06/08
* Specifications By : JT Hu
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       : Upload United Way data by BDC
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*****************************************************************

DATA: BEGIN OF IT_PERSON OCCURS 0,
       PERNR LIKE PA0014-PERNR,
       SNAME LIKE PA0001-SNAME,
       BEGDA LIKE PA0014-BEGDA,
       ENDDA LIKE PA0014-ENDDA,
       LGART LIKE P0014-LGART,
       BETRG LIKE PA0014-BETRG,
       UPDAT(10),
       BEGDA_C(8),
       ENDDA_C(8),
      END OF IT_PERSON.

DATA: BEGIN OF IT_EXCL OCCURS 0,
      COL1(8),
      COL2(10),
      COL3(10),
      COL4(4),
      COL5(10),
      END OF IT_EXCL.

DATA: I_PROCESSED TYPE I.
DATA: W_FILETY LIKE RLGRAP-FILETYPE VALUE 'DAT'.

*FOR BDC
DATA: BEGIN OF BDCDATA OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDCDATA.

DATA: BEGIN OF IT_MESSAGE OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA: END OF IT_MESSAGE.

DATA: BEGIN OF IT_MESS OCCURS 0,
        MSGTY LIKE SY-MSGTY,
        MSGTX(120) TYPE C,
      END OF IT_MESS.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-101.
PARAMETERS:
  P_FILE  LIKE RLGRAP-FILENAME OBLIGATORY.
SELECTION-SCREEN END   OF BLOCK B1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  PERFORM AT_SEL_SCREEN_ON_VALUE_REQUEST USING P_FILE 'O'.

START-OF-SELECTION.
  PERFORM UPLOAD_PROCESS.

END-OF-SELECTION.
  PERFORM UPDATE_BDC.
  PERFORM WRITE_OUTPUT.

*&---------------------------------------------------------------------*
*&      Form  UPDATE_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_BDC.
  DATA: I_TOTAL TYPE I.
  DATA: I_PERCENT TYPE I.
  DESCRIBE TABLE IT_PERSON LINES I_TOTAL.
  IF I_TOTAL = 0.
  ELSE.
    I_PROCESSED = 0.
    LOOP AT IT_PERSON.
      CLEAR: BDCDATA, BDCDATA[].
      PERFORM DO_BDC USING IT_PERSON.
      I_PROCESSED = I_PROCESSED + 1.
      I_PERCENT = ( I_PROCESSED * 100 ) / I_TOTAL .
      PERFORM PROGRESS_INDICATOR USING I_PERCENT.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " UPDATE_BDC
*&---------------------------------------------------------------------*
*&      Form  DO_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DO_BDC USING P_PERSON LIKE IT_PERSON.
  DATA: L_UPDATE TYPE C.
  PERFORM FILL_BDCDATA USING P_PERSON .
  PERFORM CALL_TRANSACTION USING 'PA30' L_UPDATE.
  IF L_UPDATE = 'S'.
    P_PERSON-UPDAT = 'Success'.
    MODIFY IT_PERSON FROM P_PERSON TRANSPORTING UPDAT.
  ELSE.
    P_PERSON-UPDAT = 'Fail'.
    MODIFY IT_PERSON FROM P_PERSON TRANSPORTING UPDAT.
  ENDIF.

ENDFORM.                    " DO_BDC
*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0194   text
*----------------------------------------------------------------------*
FORM CALL_TRANSACTION USING P_TCODE P_UPDATE.
  DATA: L_MSGSTR(100) TYPE C,
        L_MODE(1).
  L_MODE = 'N'.
  CALL TRANSACTION P_TCODE
           USING BDCDATA
           MODE L_MODE
           UPDATE 'S'
           MESSAGES INTO IT_MESSAGE.
* ckeck the massge
  IF SY-SUBRC = 0.
    P_UPDATE = 'S'.
  ELSE.
    P_UPDATE = 'F'.
    PERFORM RKC_MSG_STRING USING L_MSGSTR.
    IT_MESS-MSGTY = SY-MSGTY.
    IT_MESS-MSGTX = L_MSGSTR.
    APPEND IT_MESS.
  ENDIF.
ENDFORM.                    " CALL_TRANSACTION
*&---------------------------------------------------------------------*
*&      Form  FILL_BDCDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_BDCDATA USING P_PERSON LIKE IT_PERSON.
  DATA: L_AMOUNT(13).

  PERFORM BDC_DYNPRO      USING 'SAPMP50A' '1000'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'RP50G-PERNR'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=VIEW'.
  PERFORM BDC_FIELD       USING 'RP50G-PERNR'
*                              record-PERNR_001.
                                 IT_PERSON-PERNR.
*perform bdc_field       using 'RP50G-TIMR6'
*                              record-TIMR6_002.
  PERFORM BDC_DYNPRO      USING 'SAPMP50A' '0100'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'T588T-DTEXT(02)'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=PICK'.
  PERFORM BDC_DYNPRO      USING 'SAPMP50A' '1000'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=INS'.
  PERFORM BDC_FIELD       USING 'RP50G-PERNR'
                                IT_PERSON-PERNR.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'T582S-ITEXT(03)'.
*perform bdc_field       using 'RP50G-SELEC(03)'
*                              record-SELEC_03_004.
*perform bdc_field       using 'RP50G-TIMR6'
*                              record-TIMR6_005.
  PERFORM BDC_DYNPRO      USING 'MP001400' '2010'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'Q0014-BETRG'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM BDC_FIELD       USING 'P0014-BEGDA'
                                  IT_PERSON-BEGDA_C.
  PERFORM BDC_FIELD       USING 'P0014-ENDDA'
                                  IT_PERSON-ENDDA_C.
  PERFORM BDC_FIELD       USING 'P0014-LGART'
                                IT_PERSON-LGART.
  L_AMOUNT = IT_PERSON-BETRG.
  PERFORM BDC_FIELD       USING 'Q0014-BETRG'
                                L_AMOUNT.
*perform bdc_field       using 'P0014-WAERS'
*                              record-WAERS_010.
  PERFORM BDC_DYNPRO      USING 'MP001400' '2010'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'P0014-BEGDA'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=UPD'.
  PERFORM BDC_FIELD       USING 'P0014-BEGDA'
                                IT_PERSON-BEGDA_C.
  PERFORM BDC_FIELD       USING 'P0014-ENDDA'
                                IT_PERSON-ENDDA_C.
  PERFORM BDC_FIELD       USING 'P0014-LGART'
                                IT_PERSON-LGART.
  PERFORM BDC_FIELD       USING 'Q0014-BETRG'
                                L_AMOUNT.
*perform bdc_field       using 'P0014-WAERS'
*                              record-WAERS_015.
ENDFORM.                    " FILL_BDCDATA

*&---------------------------------------------------------------------*
*&      Form  RKC_MSG_STRING
*&---------------------------------------------------------------------*
*       SERACH THE MESSAGE OF BDC
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM RKC_MSG_STRING CHANGING P_MSG.
  DATA: LW_MSG LIKE CFGNL-MSGLIN.

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
            MSG_LIN = LW_MSG
       EXCEPTIONS
            OTHERS  = 1.
*  CONCATENATE 'Update failed for' it_person-pernr
*    INTO lw_msg SEPARATED BY space.
  CONCATENATE IT_PERSON-PERNR LW_MSG INTO LW_MSG
     SEPARATED BY SPACE.
  MOVE: LW_MSG TO P_MSG.
ENDFORM.                    " RKC_MSG_STRING

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM BDC_FIELD USING FNAM FVAL.
  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.
ENDFORM.

*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM BDC_DYNPRO USING PROGRAM DYNPRO.
  CLEAR BDCDATA.
  BDCDATA-PROGRAM  = PROGRAM.
  BDCDATA-DYNPRO   = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  WRITE_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM WRITE_OUTPUT.
  PERFORM WRITE_HEADER.
*  IF s_no_exist = 'X' OR
*     s_no_exist = 'N'.
*    WRITE: / g_message.
*    EXIT.
*  ENDIF.
  PERFORM WRITE_DATA.

ENDFORM.                    " WRITE_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  WRITE_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM WRITE_HEADER.
  DATA: I_COUNT TYPE I.

  DESCRIBE  TABLE IT_PERSON LINES I_COUNT.
  WRITE: /1 'Total Number of Employees: ', I_COUNT.

  WRITE AT /5(98) SY-ULINE .

  WRITE :/5 SY-VLINE NO-GAP.
  FORMAT COLOR COL_HEADING.
  WRITE:    TEXT-010  NO-GAP,
           SY-VLINE NO-GAP, TEXT-070  NO-GAP,
           SY-VLINE NO-GAP, TEXT-020  NO-GAP,
           SY-VLINE NO-GAP, TEXT-030  NO-GAP,
           SY-VLINE NO-GAP, TEXT-040  NO-GAP,
           SY-VLINE NO-GAP, TEXT-050  NO-GAP,
           SY-VLINE NO-GAP, TEXT-060  NO-GAP.

  WRITE:   SY-VLINE NO-GAP.
  FORMAT COLOR OFF.
  WRITE AT /5(98) SY-ULINE .

ENDFORM.                    " WRITE_HEADER

*&---------------------------------------------------------------------*
*&      Form  write_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM WRITE_DATA.
  DATA: I_COUNT TYPE I.
  DATA: I_MOD TYPE I.

  SORT IT_PERSON BY PERNR.

  I_COUNT = 0.
  LOOP AT IT_PERSON.
    WRITE :/5 SY-VLINE NO-GAP, 6(10) IT_PERSON-PERNR NO-GAP,
            SY-VLINE NO-GAP, 17(30) IT_PERSON-SNAME NO-GAP,
            SY-VLINE NO-GAP, 48(10) IT_PERSON-BEGDA NO-GAP,
            SY-VLINE NO-GAP, 59(10) IT_PERSON-ENDDA NO-GAP,
            SY-VLINE NO-GAP, 70(10) IT_PERSON-LGART NO-GAP,
            SY-VLINE NO-GAP, 81(10) IT_PERSON-BETRG NO-GAP,
            SY-VLINE NO-GAP, 92(10) IT_PERSON-UPDAT NO-GAP,
            SY-VLINE NO-GAP.
    I_COUNT = I_COUNT + 1.
    I_MOD = I_COUNT MOD 5.
    IF I_MOD = 0.
      WRITE AT /5(98) SY-ULINE.
    ENDIF.
  ENDLOOP.
  IF I_MOD NE 0.
    WRITE AT /5(98) SY-ULINE.
  ENDIF.
* WRITE THE MESSAGE
*  DESCRIBE TABLE IT_MESS LINES I_COUNT.
*  IF I_COUNT NE 0.
*    SKIP.
*    ULINE.
*    WRITE: / 'The Following Error Occurs: '.
*    ULINE.
*    I_COUNT = 1.
*    LOOP AT IT_MESS.
*      WRITE: /1(4) I_COUNT,6(120) IT_MESS-MSGTX.
*      I_COUNT = I_COUNT + 1.
*    ENDLOOP.
*    ULINE.
*  ENDIF.

ENDFORM.                    " write_data
*&---------------------------------------------------------------------*
*&      Form  PROGRESS_INDICATOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_PERCENT  text
*----------------------------------------------------------------------*
FORM PROGRESS_INDICATOR USING    P_PERCENT.
  DATA: L_TEXT(40).
  DATA: I_MOD TYPE I.

  L_TEXT = P_PERCENT.
  CONDENSE L_TEXT.
  I_MOD = P_PERCENT MOD 5.
  IF I_MOD = 0.
    CONCATENATE L_TEXT '% PROCESSED' INTO L_TEXT.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
         EXPORTING
              TEXT = L_TEXT.
  ENDIF.
ENDFORM.                    " PROGRESS_INDICATOR
*&---------------------------------------------------------------------*
*&      Form  AT_SEL_SCREEN_ON_VALUE_REQUEST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_FILE  text
*      -->P_0126   text
*----------------------------------------------------------------------*
FORM AT_SEL_SCREEN_ON_VALUE_REQUEST USING DEF_PATH LIKE
RLGRAP-FILENAME
                                          MODE     TYPE C.

  DATA: TMP_FILENAME LIKE RLGRAP-FILENAME.
  DATA: TMP_MASK(80).                  " LIKE GLOBAL_FILEMASK_ALL.
  DATA: FIELDLN TYPE I.
  FIELD-SYMBOLS: <TMP_SYM>.

  TMP_MASK = ',*.*,*.*.'.
  FIELDLN = STRLEN( DEF_PATH ) - 1.
  ASSIGN DEF_PATH+FIELDLN(1) TO <TMP_SYM>.
  IF <TMP_SYM> = '/' OR <TMP_SYM> = '\'.
    CLEAR <TMP_SYM>.
  ENDIF.

  CALL FUNCTION 'WS_FILENAME_GET'
       EXPORTING
            DEF_FILENAME     = P_FILE
            DEF_PATH         = DEF_PATH
*           MASK             = ',*.*,*.*.'
            MASK             = TMP_MASK
            MODE             = MODE
*           TITLE            = ' '
       IMPORTING
            FILENAME         = TMP_FILENAME
*         RC               =
       EXCEPTIONS
            INV_WINSYS       = 01
            NO_BATCH         = 02
            SELECTION_CANCEL = 03
            SELECTION_ERROR  = 04.

  IF SY-SUBRC = 0.
    P_FILE = TMP_FILENAME.
  ELSE.
* IF SY-SUBRC = 01.    "// Does not work, why ???
*   MESSAGELINE = 'Not supported'.
* ENDIF.
  ENDIF.

ENDFORM.                    " AT_SEL_SCREEN_ON_VALUE_REQUEST
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPLOAD_PROCESS.
  DATA: L_CHAR8(8),
        L_DATE LIKE SY-DATUM.
  CALL FUNCTION 'UPLOAD'
    EXPORTING
*   CODEPAGE                      = ' '
      FILENAME                      = P_FILE
      FILETYPE                      = W_FILETY
*   ITEM                          = ' '
*   FILEMASK_MASK                 = ' '
*   FILEMASK_TEXT                 = ' '
*   FILETYPE_NO_CHANGE            = ' '
*   FILEMASK_ALL                  = ' '
*   FILETYPE_NO_SHOW              = ' '
*   LINE_EXIT                     = ' '
*   USER_FORM                     = ' '
*   USER_PROG                     = ' '
*   SILENT                        = 'S'
* IMPORTING
*   FILESIZE                      =
*   CANCEL                        =
*   ACT_FILENAME                  =
*   ACT_FILETYPE                  =
     TABLES
       DATA_TAB                      = IT_EXCL

* EXCEPTIONS
*   CONVERSION_ERROR              = 1
*   INVALID_TABLE_WIDTH           = 2
*   INVALID_TYPE                  = 3
*   NO_BATCH                      = 4
*   UNKNOWN_ERROR                 = 5
*   GUI_REFUSE_FILETRANSFER       = 6
*   OTHERS                        = 7
             .
  IF SY-SUBRC <> 0.

* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CASE SY-SUBRC.
    WHEN 0.
*      DATA L_TEXT(132).
*      CONCATENATE P_FILE ' successfully uploaded'
*                  INTO L_TEXT.
*      WRITE: / L_TEXT.
*      SKIP.
    WHEN 2.
      MESSAGE E000 WITH 'File open error'.
    WHEN 3.
      MESSAGE E000 WITH 'File read error'.
    WHEN OTHERS.
      MESSAGE E000 WITH 'Upload error'.
  ENDCASE.

  LOOP AT IT_EXCL.
    IF IT_EXCL-COL1 IS INITIAL.
    ELSE.
      IT_PERSON-PERNR = IT_EXCL-COL1.
      CONCATENATE IT_EXCL-COL2+6(4) IT_EXCL-COL2+0(2)
         IT_EXCL-COL2+3(2) INTO L_CHAR8.
      WRITE L_CHAR8 TO L_DATE.
      IT_PERSON-BEGDA = L_DATE.
      CONCATENATE  IT_EXCL-COL2+0(2) IT_EXCL-COL2+3(2)
         IT_EXCL-COL2+6(4) INTO IT_PERSON-BEGDA_C.

      CONCATENATE IT_EXCL-COL3+6(4) IT_EXCL-COL3+0(2)
          IT_EXCL-COL3+3(2) INTO L_CHAR8.
      WRITE L_CHAR8 TO L_DATE.
      IT_PERSON-ENDDA = L_DATE.
      CONCATENATE  IT_EXCL-COL3+0(2) IT_EXCL-COL3+3(2)
        IT_EXCL-COL3+6(4) INTO IT_PERSON-ENDDA_C.

*      IT_PERSON-BEGDA = IT_EXCL-COL2.
*      IT_PERSON-ENDDA = IT_EXCL-COL3.
      IT_PERSON-LGART = IT_EXCL-COL4.
      IT_PERSON-BETRG = IT_EXCL-COL5.
      SELECT SINGLE ENAME INTO IT_PERSON-SNAME
        FROM PA0001
        WHERE PERNR = IT_PERSON-PERNR.
      APPEND IT_PERSON.
      CLEAR: IT_PERSON.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " UPLOAD_PROCESS
