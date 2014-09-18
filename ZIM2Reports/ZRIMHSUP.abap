*&---------------------------------------------------------------------*
*& Report  ZRIMHSUP                                                    *
*&---------------------------------------------------------------------*
*&  프로그램명 : HS-CODE UPLOAD                                        *
*&      작성자 : 김은정 INFOLINK Ltd.                                  *
*&      작성일 : 2001.11.20                                            *
*&---------------------------------------------------------------------*
*&   DESC.     : HS-CODE를 FILE로 받아서 DB에 UPLOAD시켜주는 BDC PROG.
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMHSUP  MESSAGE-ID ZIM
                  LINE-SIZE  79
                  NO STANDARD PAGE HEADING.

*----------------------------------------------------------------------
* Internal Table & Data 선언
*----------------------------------------------------------------------
DATA : BEGIN OF IT_TAB  OCCURS 0,
       LAND1    LIKE    V_T604-LAND1,
       STAWN    LIKE    V_T604-STAWN,
       TEXT1    LIKE    V_T604-TEXT1,
       MINOL    LIKE    V_T604-MINOL.
DATA : END   OF IT_TAB.

*>>> Page & Record Count.
DATA : W_COUNT  TYPE    I,
       W_PAGE   TYPE    I,
       W_PCNT   TYPE    I.

*>>> BDC Table.
DATA:    BEGIN OF BDCDATA OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA END OF BDCDATA.

DATA:  MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.

DATA : W_SUBRC      TYPE  SY-SUBRC,
       DISP_MODE    TYPE  C  VALUE 'N',
       UMODE        TYPE  C  VALUE 'S'.

*----------------------------------------------------------------------
* Selection Screen.
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-B03.

PARAMETERS : P_FILE  LIKE RLGRAP-FILENAME.

SELECTION-SCREEN END   OF BLOCK B1.

*----------------------------------------------------------------------
* Initialization.
*----------------------------------------------------------------------
INITIALIZATION.
  SET TITLEBAR 'HSUP'.

*----------------------------------------------------------------------
* Top of Page
*----------------------------------------------------------------------
TOP-OF-PAGE.
  SET PF-STATUS 'HSUP'.
  SET TITLEBAR  'HSUP'.
  PERFORM P1000_WRITE_TITLE.

*----------------------------------------------------------------------
* Start of Selection
*----------------------------------------------------------------------
START-OF-SELECTION.
  PERFORM P1000_READIMPORTFILE.

*----------------------------------------------------------------------
* End of Selection
*----------------------------------------------------------------------
END-OF-SELECTION.
  PERFORM P1000_WRITE_DATA.

*----------------------------------------------------------------------
* At User Command
*----------------------------------------------------------------------
AT USER-COMMAND.
  CASE SY-UCOMM.
    WHEN 'UPLOAD'.
      PERFORM P1000_HSCODE_UPLOAD.
  ENDCASE.

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.

  SET  TITLEBAR 'HSUP'.          " TITLE BAR

ENDFORM.                    " P2000_SET_PARAMETER

*&---------------------------------------------------------------------*
*&      Form  P1000_WRITE_TITLE
*&---------------------------------------------------------------------*
FORM P1000_WRITE_TITLE.

  SKIP 2.
  FORMAT  COLOR   COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /30 '[ HS-CODE UPLOAD ]'
               COLOR COL_HEADING INTENSIFIED ON.
  SKIP 1.

ENDFORM.                    " P1000_WRITE_TITLE

*&---------------------------------------------------------------------*
*&      Form  P1000_READIMPORTFILE
*&---------------------------------------------------------------------*
*       Upload된 File에서 Data를 읽어온다.
*----------------------------------------------------------------------*
FORM P1000_READIMPORTFILE.

  PERFORM P2000_CALL_FUNCTION    TABLES IT_TAB.

ENDFORM.                    " P1000_READIMPORTFILE

*&---------------------------------------------------------------------*
*&      Form  P2000_CALL_FUNCTION
*&---------------------------------------------------------------------*
FORM P2000_CALL_FUNCTION TABLES   IT_TAB STRUCTURE IT_TAB.

  REFRESH IT_TAB.
  CALL FUNCTION 'WS_UPLOAD'
       EXPORTING
            FILENAME                = P_FILE
            FILETYPE                = 'DAT'
       TABLES
            DATA_TAB                = IT_TAB
       EXCEPTIONS
            CONVERSION_ERROR        = 1
            FILE_OPEN_ERROR         = 2
            FILE_READ_ERROR         = 3
            INVALID_TABLE_WIDTH     = 4
            INVALID_TYPE            = 5
            NO_BATCH                = 6
            UNKNOWN_ERROR           = 7
            GUI_REFUSE_FILETRANSFER = 8
            CUSTOMER_ERROR          = 9
            OTHERS                  = 10.

  IF SY-SUBRC NE 0.
    MESSAGE E637 WITH 'HS-CODE'.
  ENDIF.

ENDFORM.                    " P2000_CALL_FUNCTION

*&---------------------------------------------------------------------*
*&      Form  P1000_WRITE_DATA
*&---------------------------------------------------------------------*
FORM P1000_WRITE_DATA.

  W_COUNT = 0.
  W_PAGE  = 1.
  W_PCNT  = 0.

  PERFORM P2000_PAGE_HEADER.

  LOOP AT IT_TAB.
    PERFORM P2000_LINE_WRITE.

    AT LAST.
      PERFORM P2000_LAST_WRITE.
    ENDAT.
  ENDLOOP.

ENDFORM.                    " P1000_WRITE_DATA

*&---------------------------------------------------------------------*
*&      Form  P2000_PAGE_HEADER
*&---------------------------------------------------------------------*
FORM P2000_PAGE_HEADER.

  SKIP 1.
  FORMAT  COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /   'DATE : ', SY-DATUM, 60 'PAGE : ', W_PAGE.

  WRITE : /    SY-ULINE.
  FORMAT  COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : /    SY-VLINE                   NO-GAP,
          (05) '국 가'                    NO-GAP,  SY-VLINE NO-GAP,
          (17) '   HS CODE'               NO-GAP,  SY-VLINE NO-GAP,
          (40) '              HS CODE 명' NO-GAP,  SY-VLINE NO-GAP,
          (12) '수입승인유무'             NO-GAP,  SY-VLINE.
  WRITE : /    SY-ULINE.

ENDFORM.                    " P2000_PAGE_HEADER

*&---------------------------------------------------------------------*
*&      Form  P2000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P2000_LINE_WRITE.

  IF W_PCNT = 17.
    W_PCNT = 0.
    W_PAGE = W_PAGE + 1.
    PERFORM  P2000_PAGE_HEADER.
  ENDIF.

  FORMAT COLOR COL_NORMAL INTENSIFIED ON.

  WRITE : /    SY-VLINE             NO-GAP,
          (05) IT_TAB-LAND1         NO-GAP,  SY-VLINE  NO-GAP,
          (17) IT_TAB-STAWN         NO-GAP,  SY-VLINE  NO-GAP,
          (40) IT_TAB-TEXT1         NO-GAP,  SY-VLINE  NO-GAP,
          (12) IT_TAB-MINOL         NO-GAP,  SY-VLINE.
  HIDE : IT_TAB.
  WRITE : /    SY-ULINE.

  W_COUNT = W_COUNT + 1.
  W_PCNT  = W_PCNT  + 1.

ENDFORM.                    " P2000_LINE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P2000_LAST_WRITE
*&---------------------------------------------------------------------*
FORM P2000_LAST_WRITE.

  IF W_COUNT GT 0.
    WRITE : /5 '   총 ', W_COUNT, '건'.
  ENDIF.

ENDFORM.                    " P2000_LAST_WRITE

*&---------------------------------------------------------------------*
*&      Form  P1000_HSCODE_UPLOAD
*&---------------------------------------------------------------------*
*       BDC로 DATA를 DB에 INSERT 시킴.
*----------------------------------------------------------------------*
FORM P1000_HSCODE_UPLOAD.

  REFRESH : BDCDATA.
  LOOP AT IT_TAB.
    IF SY-TABIX EQ 1.
* 초기화면
*        PERFORM P2000_DYNPRO USING :
*                'X' 'SAPLS_IMG_TOOL_5'  '0100',
*                ' ' 'BDC_OKCODE'        'REF_IMG'.
    ENDIF.
    " BDCDATA에 APPEND시킴.
    PERFORM P2000_BDC_INSERT.
  ENDLOOP.
  PERFORM P2000_CALL_TRANSACTION  USING  'SPRO'
                                          W_SUBRC.

ENDFORM.                    " P1000_HSCODE_UPLOAD

*&---------------------------------------------------------------------*
*&      Form  P2000_BDC_INSERT
*&---------------------------------------------------------------------*
*       BDCDATA INTERNAL TABLE에 APPEND.
*----------------------------------------------------------------------*
FORM P2000_BDC_INSERT.

  DATA    : W_MINOL  LIKE V_T604-MINOL.

  IF IT_TAB-MINOL = 'X' OR IT_TAB-MINOL = 'Y'.
    W_MINOL = 'X'.
  ELSE.
    W_MINOL = SPACE.
  ENDIF.

* 두번째화면

* 3RD SCREEN
  PERFORM P2000_DYNPRO USING :
      'X' 'SAPL080E'            '0020',
      ' ' 'BDC_OKCODE'          'NEWL'.
* NEW ENTRY INSERT SCREEN
  PERFORM P2000_DYNPRO USING :
      'X' 'SAPL080E'            '0040',
      ' ' 'V_T604-LAND1'        IT_TAB-LAND1,
      ' ' 'V_T604-STAWN'        IT_TAB-STAWN,
      ' ' 'V_T604-TEXT1'        IT_TAB-TEXT1,
      ' ' 'V_T604-MINOL'        W_MINOL,
      ' ' 'BDC_OKCODE'          'SAVE'.

  PERFORM P2000_DYNPRO USING :
    'X' 'SAPL080E'            '0040',
    ' ' 'BDC_OKCODE'          '=UEBE'.

ENDFORM.                    " P2000_BDC_INSERT

*&---------------------------------------------------------------------*
*&      Form  P2000_DYNPRO
*&---------------------------------------------------------------------*
*       BDCDATA INSERT
*----------------------------------------------------------------------*
FORM P2000_DYNPRO USING    DYNBEGIN NAME VALUE.

  IF DYNBEGIN = 'X'.
    CLEAR  BDCDATA.
    MOVE : NAME      TO BDCDATA-PROGRAM,
           VALUE     TO BDCDATA-DYNPRO,
           'X'       TO BDCDATA-DYNBEGIN.
    APPEND BDCDATA.
  ELSE.
    CLEAR  BDCDATA.
    MOVE : NAME      TO BDCDATA-FNAM,
           VALUE     TO BDCDATA-FVAL.
    APPEND BDCDATA.
  ENDIF.

ENDFORM.                    " P2000_DYNPRO

*&---------------------------------------------------------------------*
*&      Form  P2000_CALL_TRANSACTION
*&---------------------------------------------------------------------*
FORM P2000_CALL_TRANSACTION USING    TCODE
                            CHANGING P_SUBRC.

  REFRESH MESSTAB.

  CALL TRANSACTION  TCODE  USING    BDCDATA
                           MODE     'A'
                           UPDATE   UMODE
                           MESSAGES INTO     MESSTAB.

  P_SUBRC = SY-SUBRC.

ENDFORM.                    " P2000_CALL_TRANSACTION
