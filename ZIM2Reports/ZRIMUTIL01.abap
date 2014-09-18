*&---------------------------------------------------------------------*
*& INCLUDE ZRIMUTIL01 .
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입시스템 Utility function Include                   *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2000.02.03                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
TABLES : DD01T,
         DD07T.

DATA : OUT_TEXT(70)  TYPE     C.
DATA : W_DDTEXT      LIKE     DD07T-DDTEXT.
DATA : W_SY_SUBRC    LIKE     SY-SUBRC.

*&---------------------------------------------------------------------*
*&      Form  P2000_FILE_NAME_HELP
*&---------------------------------------------------------------------*
FORM P2000_FILE_NAME_HELP USING    DIR_PATH
                                   FILEMASK
                                   INIT_FILE_NAME
                          CHANGING FILE_NAME.

 DATA: TMP_FILENAME LIKE RLGRAP-FILENAME,
      TMP_MASK(80),
      FIELDLN TYPE I,
      TMP_PATH LIKE RLGRAP-FILENAME.

 FIELD-SYMBOLS: <TMP_SYM>.

  IF DIR_PATH NE SPACE.
     TMP_PATH = DIR_PATH.
  ENDIF.

  IF FILEMASK = SPACE.
     TMP_MASK = ',*.*,*.*.'.
  ELSE.
     TMP_MASK = FILEMASK.
     CONDENSE TMP_MASK NO-GAPS.
  ENDIF.

  FIELDLN = STRLEN( TMP_PATH ) - 1.
  ASSIGN TMP_PATH+FIELDLN(1) TO <TMP_SYM>.
  IF <TMP_SYM> = '/' OR <TMP_SYM> = '\'.
     CLEAR <TMP_SYM>.
  ENDIF.

  CALL FUNCTION 'WS_FILENAME_GET'
       EXPORTING
            DEF_FILENAME     = INIT_FILE_NAME
            DEF_PATH         = TMP_PATH
            MASK             = TMP_MASK
*           mode             = 'S'
       IMPORTING
            FILENAME         = TMP_FILENAME
       EXCEPTIONS
            INV_WINSYS       = 01
            NO_BATCH         = 02
            SELECTION_CANCEL = 03
            SELECTION_ERROR  = 04.

  IF  SY-SUBRC = 0.
       FILE_NAME = TMP_FILENAME.
  ELSE.
  ENDIF.

ENDFORM.                    " P2000_FILE_NAME_HELP

*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_BAR
*&---------------------------------------------------------------------*
*    작업수행에 대한 진척도를 계산한다.;'SAPGUI_PROGRESS_INDICATOR'
FORM P2000_SHOW_BAR USING TEXT PERC.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING   PERCENTAGE = PERC
                   TEXT       = TEXT.

ENDFORM.                    " P2000_SHOW_BAR

*&---------------------------------------------------------------------*
*&      Form  P2000_GET_NUMBER_NEXT
*&---------------------------------------------------------------------*
FORM P2000_GET_NUMBER_NEXT       USING    P_GUBUN     P_DOCNO.

     CALL FUNCTION 'ZIM_NUMBER_GET_NEXT'
         EXPORTING
               ZFREQTY         =    P_GUBUN
         IMPORTING
               ZFREQNO         =    P_DOCNO
         EXCEPTIONS
                NOT_INPUT       = 1
                NOT_TYPE        = 2
                NOT_RANGE       = 3
                NOT_FOUND       = 4
                LOCKED          = 6
                ERROR_DUPLICATE = 8.

      CASE SY-SUBRC.
         WHEN 1.     MESSAGE    E012.
         WHEN 2.     MESSAGE    E013      WITH  P_GUBUN.
         WHEN 3.     MESSAGE    E014      WITH  P_DOCNO.
         WHEN 4.     MESSAGE    E964.
         WHEN 6.     MESSAGE    E510      WITH
                                  SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
         WHEN 8.     MESSAGE    E015      WITH  P_DOCNO.
      ENDCASE.

ENDFORM.                    " P2000_GET_NUMBER_NEXT
