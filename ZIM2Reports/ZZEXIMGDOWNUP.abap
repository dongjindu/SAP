*&---------------------------------------------------------------------*
*& Report  ZEXIMGDOWNUP                                                *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*
REPORT ZEXIMGDOWNUP NO STANDARD PAGE HEADING
                    LINE-SIZE 101
                    MESSAGE-ID ZIM.

*----------------------------------------------------------------------*
*  TABLE DEFINE*
*----------------------------------------------------------------------*
TABLES: ZTIMIMG00,       " 수입시스템 BASIC CONFIG.
        ZTIMIMG01,       " Payment Term Configuration.
        ZTIMIMG02,       " 세관코드.
        ZTIMIMG03,       " 보세구역코드.
        ZTIMIMG04,       " Planned Cost Rate 관리.
        ZTIMIMG05,       " 보세운송 운임단가 관리.
        ZTIMIMG06,       " 관세청 고시환율 관리.
        ZTIMIMG07,       " 통관수수료율 관리.
        ZTIMIMG08,       " 기타코드관리.
        ZTIMIMG09,       " HS CODE 별 관세율 관리.
        ZTIMIMG10,       " 관세사관리.
        ZTIMIMG11,       " G/R, I/V, 비용처리 CONFIGURATION.
        ZTIMIMG12,       " 운송수단 MATCH CODE 관리.
        ZTIMIMG17,       " 항공화물 해외운송 요율 관리.
        ZTIMIMG18,       " 수입시스템 자재관리.
        ZTIMIMG19,       " 사용자별 사업영역 정의.
        ZTIMIMG20,     " 수송요율 유지보수. JSY20020912
        ZTIMIMG21,     " 해외항공화물운송요율표 관리. NHJ 2002.09.18
        ZTIMIMG22,     " 해외해상화물운송요율표 관리. NHJ 2002.09.18
        ZTIMIMG23,     " 선적지&운임적용지역 MATCH. NHJ 2002.09.18
        ZTIMIMGTX,     " 수입시스템 EDI BASIC CONFIG.
        BAPICURR,
        ZTIMGTXT,        " 수입 IMG 기능설명 Text
        ZTIEPORT,        " 선적항, 도착항 관리.
        USR01.

*----------------------------------------------------------------------*
*  DATA                                                                *
*----------------------------------------------------------------------*

DATA: BEGIN OF IT_ZTIMIMG00  OCCURS 0.
        INCLUDE STRUCTURE ZTIMIMG00.
DATA: AMT1(16) TYPE  C.
DATA: END OF IT_ZTIMIMG00.

DATA: BEGIN OF IT_ZTIMIMG01  OCCURS 0.
        INCLUDE STRUCTURE ZTIMIMG01.
DATA: END OF IT_ZTIMIMG01.

DATA: BEGIN OF IT_ZTIMIMG02  OCCURS 0.
        INCLUDE STRUCTURE ZTIMIMG02.
DATA: END OF IT_ZTIMIMG02.

DATA: BEGIN OF IT_ZTIMIMG03  OCCURS 0.
        INCLUDE STRUCTURE ZTIMIMG03.
DATA: END OF IT_ZTIMIMG03.

DATA: BEGIN OF IT_ZTIMIMG04  OCCURS 0.
        INCLUDE STRUCTURE ZTIMIMG04.
DATA: END OF IT_ZTIMIMG04.

DATA: BEGIN OF IT_ZTIMIMG05  OCCURS 0.
        INCLUDE STRUCTURE ZTIMIMG05.
DATA: END OF IT_ZTIMIMG05.

DATA: BEGIN OF IT_ZTIMIMG06  OCCURS 0.
        INCLUDE STRUCTURE ZTIMIMG06.
DATA: END OF IT_ZTIMIMG06.

DATA: BEGIN OF IT_ZTIMIMG07  OCCURS 0.
        INCLUDE STRUCTURE ZTIMIMG07.
DATA: END OF IT_ZTIMIMG07.

DATA: BEGIN OF IT_ZTIMIMG08  OCCURS 0.
        INCLUDE STRUCTURE ZTIMIMG08.
DATA: END OF IT_ZTIMIMG08.

DATA: BEGIN OF IT_ZTIMIMG09  OCCURS 0.
        INCLUDE STRUCTURE ZTIMIMG09.
DATA: END OF IT_ZTIMIMG09.

DATA: BEGIN OF IT_ZTIMIMG10  OCCURS 0.
        INCLUDE STRUCTURE ZTIMIMG10.
DATA: END OF IT_ZTIMIMG10.

DATA: BEGIN OF IT_ZTIMIMG11  OCCURS 0.
        INCLUDE STRUCTURE ZTIMIMG11.
DATA: END OF IT_ZTIMIMG11.

DATA: BEGIN OF IT_ZTIMIMG12  OCCURS 0.
        INCLUDE STRUCTURE ZTIMIMG12.
DATA: END OF IT_ZTIMIMG12.

DATA: BEGIN OF IT_ZTIMIMG17  OCCURS 0.
        INCLUDE STRUCTURE ZTIMIMG17.
DATA: END OF IT_ZTIMIMG17.

DATA: BEGIN OF IT_ZTIMIMG18  OCCURS 0.
        INCLUDE STRUCTURE ZTIMIMG18.
DATA: END OF IT_ZTIMIMG18.

DATA: BEGIN OF IT_ZTIMIMG19  OCCURS 0.
        INCLUDE STRUCTURE ZTIMIMG19.
DATA: END OF IT_ZTIMIMG19.

DATA: BEGIN OF IT_ZTIMIMG20  OCCURS 0.
        INCLUDE STRUCTURE ZTIMIMG20.
DATA: END OF IT_ZTIMIMG20.

DATA: BEGIN OF IT_ZTIMIMG21  OCCURS 0.
        INCLUDE STRUCTURE ZTIMIMG21.
DATA: END OF IT_ZTIMIMG21.

DATA: BEGIN OF IT_ZTIMIMG22  OCCURS 0.
        INCLUDE STRUCTURE ZTIMIMG22.
DATA: END OF IT_ZTIMIMG22.

DATA: BEGIN OF IT_ZTIMIMG23 OCCURS 0.
        INCLUDE STRUCTURE ZTIMIMG23.
DATA: END OF IT_ZTIMIMG23.

DATA: BEGIN OF IT_ZTIMIMGTX  OCCURS 0.
        INCLUDE STRUCTURE ZTIMIMGTX.
DATA: END OF IT_ZTIMIMGTX.

DATA: BEGIN OF IT_ZTIMGTXT  OCCURS 0.
        INCLUDE STRUCTURE ZTIMGTXT.
DATA: END OF IT_ZTIMGTXT.

DATA: BEGIN OF IT_ZTIEPORT OCCURS 0.
        INCLUDE STRUCTURE ZTIEPORT.
DATA: END OF IT_ZTIEPORT.

DATA:  BEGIN OF IT_TAB  OCCURS 0,
       TABNAME    LIKE  DD02T-TABNAME,
       DDTEXT     LIKE  DD02T-DDTEXT.
DATA:  END OF IT_TAB.

DATA:  BEGIN OF IT_SELECTED OCCURS 0,
       TABNAME    LIKE  DD02T-TABNAME.
DATA: END OF IT_SELECTED.

DATA:  BEGIN OF IT_TXT  OCCURS 0,
       MANDT       LIKE ZTIMIMG09-MANDT,
       STAWN       LIKE ZTIMIMG09-STAWN,
       ZFAPLDT(8)  TYPE C,
       ZFTEXTE     LIKE ZTIMIMG09-ZFTEXTE,
       ZFTEXTK     LIKE ZTIMIMG09-ZFTEXTK,
       ZFTXCD      LIKE ZTIMIMG09-ZFTXCD,
       ZFBASIC(5)  TYPE C,"LIKE ZTIMIMG09-ZFBASIC ,
       ZFEXEC(5)   TYPE C,"LIKE ZTIMIMG09-ZFEXEC  ,
       ZFSPECR(5)  TYPE C,"LIKE ZTIMIMG09-ZFTXCD,
       ZFIRLW      LIKE ZTIMIMG09-ZFIRLW ,
       ERNAM       LIKE ZTIMIMG09-ERNAM,
       CDAT(8)     TYPE C,
       UNAM        LIKE ZTIMIMG09-UNAM,
       UDAT(8)     TYPE C.
DATA: END OF IT_TXT.

DATA: W_TABIX      LIKE SY-TABIX,
      DIGITS       TYPE I      VALUE  20,
      W_FILENAME   LIKE RLGRAP-FILENAME,
*      W_FILENAME   TYPE STRING,
      W_TABNAME    LIKE DD02T-TABNAME,
      MARKFIELD(1) TYPE C.

DATA: W_SELECTED_LINES  TYPE P.           " 선택 LINE COUNT

*----------------------------------------------------------------------*
*  Selection Screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-B02.

*   PARAMETERS: P_FILE LIKE RLGRAP-FILENAME
*               DEFAULT 'C:\IMGSET\IMG.TXT'.

SELECTION-SCREEN END OF BLOCK B2.

*----------------------------------------------------------------------*
* INITIALIZATION                                                       *
*----------------------------------------------------------------------*
INITIALIZATION.
  SET TITLEBAR 'ZEDU'.

*---------------------------------------------------------------------*
*  Start of Selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
*  PERFORM P1000_READIMPORTFILE.
  PERFORM P1000_READ_TABLE.

*----------------------------------------------------------------------*
*  End of Selection
*----------------------------------------------------------------------*
END-OF-SELECTION.
  PERFORM P1000_WRITE_TABLE.

*  PERFORM P3000_WRITE.

AT USER-COMMAND.
  CASE SY-UCOMM.
    WHEN 'DOWNLOAD'.
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES EQ 1.
        READ TABLE IT_SELECTED INDEX 1.
        PERFORM P2000_CREATE_DOWNFILE USING IT_SELECTED-TABNAME.
      ELSEIF W_SELECTED_LINES GT 1.
        MESSAGE E965.
      ELSEIF W_SELECTED_LINES EQ 0.
        MESSAGE S766.
        EXIT.
      ENDIF.

*          PERFORM P2000_UPDATETABLE.
    WHEN 'UPLOAD'.
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES EQ 1.
        READ TABLE IT_SELECTED INDEX 1.
        PERFORM P2000_CREATE_UPFILE USING IT_SELECTED-TABNAME.
      ELSEIF W_SELECTED_LINES GT 1.
        MESSAGE E965.
      ELSEIF W_SELECTED_LINES EQ 0.
        MESSAGE S766.
        EXIT.
      ENDIF.

    WHEN  OTHERS.
  ENDCASE.

*&---------------------------------------------------------------------*
*&      FORM  IMPORT_FILE
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM P1000_READIMPORTFILE.
  CALL FUNCTION 'WS_UPLOAD'
    EXPORTING
      FILENAME                = W_FILENAME
      FILETYPE                = 'ASC'
    TABLES
      DATA_TAB                = IT_TXT
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
  IF SY-SUBRC <> 0.
    MESSAGE E771 WITH W_FILENAME.
  ENDIF.

  LOOP AT IT_TXT.
    MOVE-CORRESPONDING   IT_TXT   TO   IT_TAB.
*      MOVE : IT_TXT-ZFAPLDT  TO  IT_TAB-ZFAPLDT,
*             IT_TXT-ZFBASIC  TO  IT_TAB-ZFBASIC,
*             IT_TXT-ZFEXEC   TO  IT_TAB-ZFEXEC,
*             IT_TXT-ZFSPECR  TO  IT_TAB-ZFSPECR,
*             IT_TXT-CDAT     TO  IT_TAB-CDAT,
*             IT_TXT-UDAT     TO  IT_TAB-UDAT.
*             APPEND IT_TAB.
  ENDLOOP.

ENDFORM.                               " IMPORT_FILE
*
*&---------------------------------------------------------------------*
*&      FORM  UPDATE_TABLE
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM P2000_UPDATETABLE.

*MODIFY   ZTIMIMG09  FROM TABLE IT_TAB.
  IF SY-SUBRC NE 0.
    ROLLBACK WORK.
    MESSAGE E952(ZIM).
  ELSE.
    COMMIT WORK.
    MESSAGE I953(ZIM).
  ENDIF.

ENDFORM.                    " UPDATE_TABLE
*&---------------------------------------------------------------------*
*&      FORM  P3000_WRITE
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM P3000_WRITE.





  LOOP AT IT_TAB.

*    WRITE:/ IT_TAB-MANDT,
*            IT_TAB-STAWN ,
*            IT_TAB-ZFAPLDT,
*            IT_TAB-ZFTEXTE,
*            IT_TAB-ZFTEXTK ,
*            IT_TAB-ZFTXCD,
*            IT_TAB-ZFBASIC ,
*            IT_TAB-ZFEXEC,
*            IT_TAB-ZFSPECR,
*            IT_TAB-ZFIRLW,
*            IT_TAB-ERNAM,
*            IT_TAB-CDAT,
*            IT_TAB-UNAM,
*            IT_TAB-UDAT.

  ENDLOOP.

ENDFORM.                    " P3000_WRITE.

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_TABLE
*&---------------------------------------------------------------------*
FORM P1000_READ_TABLE.

  REFRESH IT_TAB.
  CLEAR   IT_TAB.

* IMG 00~09
  SELECT DISTINCT *
    INTO CORRESPONDING FIELDS OF TABLE IT_TAB
    FROM DD02T
   WHERE TABNAME    GE 'ZTIMIMG00'
     AND TABNAME    LE 'ZTIMIMG13'
     AND DDLANGUAGE EQ '3'
     AND AS4LOCAL   EQ 'A'.

* Port Code
  SELECT    DISTINCT *
  APPENDING CORRESPONDING FIELDS OF TABLE IT_TAB
    FROM    DD02T
   WHERE    TABNAME    IN
            ('ZTIEPORT',  'ZTIMIMG17', 'ZTIMIMG18' ,
             'ZTIMIMG19', 'ZTIMIMG20', 'ZTIMIMG21 ',
             'ZTIMIMG22', 'ZTIMIMG23', 'ZTIMIMGTX', 'ZTIMGTXT')
     AND    DDLANGUAGE EQ '3'
     AND    AS4LOCAL   EQ 'A'.


* 쓰지 않는 테이블 빼기.
  LOOP AT IT_TAB WHERE TABNAME EQ 'ZTIMIMG04'
                    OR TABNAME EQ 'ZTIMIMG05'
                    OR TABNAME EQ 'ZTIMIMG06'
                    OR TABNAME EQ 'ZTIMIMG07'
                    OR TABNAME EQ 'ZTIMIMG10'
                    OR TABNAME EQ 'ZTIMIMG12'
                    OR TABNAME EQ 'ZTIMIMG17'
                    OR TABNAME EQ 'ZTIMIMG18'
                    OR TABNAME EQ 'ZTIMIMG19'.

    DELETE IT_TAB.
  ENDLOOP.

  SORT IT_TAB BY TABNAME.
ENDFORM.                    " P1000_READ_TABLE

*&---------------------------------------------------------------------*
*&      Form  P1000_WRITE_TABLE
*&---------------------------------------------------------------------*
FORM P1000_WRITE_TABLE.

  SET TITLEBAR 'ZEDU'.
  SET PF-STATUS 'ZEDU'.

  PERFORM P2000_TITLE_WRITE.

  SORT IT_TAB BY TABNAME.
  LOOP AT IT_TAB.
    PERFORM P2000_LINE_WRITE.
  ENDLOOP.
  WRITE :/ SY-ULINE.

ENDFORM.                    " P1000_WRITE_TABLE

*&---------------------------------------------------------------------*
*&      Form  P2000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P2000_TITLE_WRITE.

  SKIP 2.

  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE :/40 '<<IMG Data Upload & Download>>' CENTERED
                COLOR COL_NEGATIVE INTENSIFIED OFF.
  SKIP 2.

  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE :/ SY-ULINE.
  WRITE :/ SY-VLINE NO-GAP, (99) 'Table 선택' CENTERED NO-GAP,
           SY-VLINE.
  WRITE :/ SY-ULINE.

ENDFORM.                    " P2000_TITLE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P2000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P2000_LINE_WRITE.

  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  WRITE :/                                          SY-VLINE,
           MARKFIELD AS CHECKBOX COLOR COL_HEADING, SY-VLINE,
           IT_TAB-TABNAME COLOR COL_POSITIVE      , SY-VLINE,
           IT_TAB-DDTEXT                          , SY-VLINE.
  HIDE  :  IT_TAB.

ENDFORM.                    " P2000_LINE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
FORM P2000_MULTI_SELECTION.

  REFRESH IT_SELECTED.
  CLEAR : IT_SELECTED,
          W_SELECTED_LINES.

  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
      MOVE : IT_TAB-TABNAME     TO IT_SELECTED-TABNAME.
      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.

ENDFORM.                    " P2000_MULTI_SELECTION

*&---------------------------------------------------------------------*
*&      Form  P2000_CREATE_DOWNFILE
*&---------------------------------------------------------------------*
FORM P2000_CREATE_DOWNFILE USING    P_TABNAME.

  REFRESH : IT_ZTIMIMG00,  IT_ZTIMIMG01, IT_ZTIMIMG02, IT_ZTIMIMG03,
            IT_ZTIMIMG04,  IT_ZTIMIMG05, IT_ZTIMIMG06, IT_ZTIMIMG07,
            IT_ZTIMIMG08,  IT_ZTIMIMG09, IT_ZTIMIMG10, IT_ZTIMIMG11,
            IT_ZTIMIMG12,  IT_ZTIMIMG17, IT_ZTIMIMG18, IT_ZTIMIMG19,
            IT_ZTIMIMG20,  IT_ZTIMIMG21, IT_ZTIMIMG22, IT_ZTIMIMG23,
*            IT_ZTEIMG04,   IT_ZTEIMG05,  IT_ZTEIMG08,  IT_ZTEIMG12,
            IT_ZTIMIMGTX,  IT_ZTIMGTXT,  IT_ZTIEPORT.
  CLEAR   : IT_ZTIMIMG00,  IT_ZTIMIMG01, IT_ZTIMIMG02, IT_ZTIMIMG03,
            IT_ZTIMIMG04,  IT_ZTIMIMG05, IT_ZTIMIMG06, IT_ZTIMIMG07,
            IT_ZTIMIMG08,  IT_ZTIMIMG09, IT_ZTIMIMG10, IT_ZTIMIMG11,
            IT_ZTIMIMG12,  IT_ZTIMIMG17, IT_ZTIMIMG18, IT_ZTIMIMG19,
            IT_ZTIMIMG20,  IT_ZTIMIMG21, IT_ZTIMIMG22, IT_ZTIMIMG23,
*            IT_ZTEIMG04,   IT_ZTEIMG05,  IT_ZTEIMG08,  IT_ZTEIMG12,
            IT_ZTIMIMGTX,  IT_ZTIMGTXT,  IT_ZTIEPORT.

  CASE P_TABNAME.
    WHEN 'ZTIMIMG00'.
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE IT_ZTIMIMG00
        FROM (P_TABNAME).

      LOOP AT  IT_ZTIMIMG00.
        W_TABIX  = SY-TABIX.
        WRITE  IT_ZTIMIMG00-ZFUSDAM  TO       IT_ZTIMIMG00-AMT1
                                     CURRENCY IT_ZTIMIMG00-ZFUSD.
        PERFORM  P2000_WRITE_NO_MASK CHANGING IT_ZTIMIMG00-AMT1.
        MODIFY  IT_ZTIMIMG00  INDEX  W_TABIX.
      ENDLOOP.

      PERFORM P2000_DOWNLOAD  TABLES IT_ZTIMIMG00
                              USING  P_TABNAME.
    WHEN 'ZTIMIMG01'.
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE IT_ZTIMIMG01
        FROM ZTIMIMG01.
      PERFORM P2000_DOWNLOAD  TABLES IT_ZTIMIMG01
                              USING  P_TABNAME.
    WHEN 'ZTIMIMG02'.
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE IT_ZTIMIMG02
        FROM ZTIMIMG02.
      PERFORM P2000_DOWNLOAD  TABLES IT_ZTIMIMG02
                              USING  P_TABNAME.
    WHEN 'ZTIMIMG03'.
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE IT_ZTIMIMG03
        FROM ZTIMIMG03.
      PERFORM P2000_DOWNLOAD  TABLES IT_ZTIMIMG03
                              USING  P_TABNAME.
    WHEN 'ZTIMIMG04'.
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE IT_ZTIMIMG04
        FROM ZTIMIMG04.
      PERFORM P2000_DOWNLOAD  TABLES IT_ZTIMIMG04
                              USING  P_TABNAME.
    WHEN 'ZTIMIMG05'.
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE IT_ZTIMIMG05
        FROM ZTIMIMG05.
      PERFORM P2000_DOWNLOAD  TABLES IT_ZTIMIMG05
                              USING  P_TABNAME.
    WHEN 'ZTIMIMG06'.
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE IT_ZTIMIMG06
        FROM ZTIMIMG06.
      PERFORM P2000_DOWNLOAD  TABLES IT_ZTIMIMG06
                              USING  P_TABNAME.
    WHEN 'ZTIMIMG07'.
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE IT_ZTIMIMG07
        FROM ZTIMIMG07.
      PERFORM P2000_DOWNLOAD  TABLES IT_ZTIMIMG07
                              USING  P_TABNAME.
    WHEN 'ZTIMIMG08'.
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE IT_ZTIMIMG08
        FROM ZTIMIMG08.
      PERFORM P2000_DOWNLOAD  TABLES IT_ZTIMIMG08
                              USING  P_TABNAME.
    WHEN 'ZTIMIMG09'.
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE IT_ZTIMIMG09
        FROM ZTIMIMG09.
      PERFORM P2000_DOWNLOAD  TABLES IT_ZTIMIMG09
                              USING  P_TABNAME.
    WHEN 'ZTIMIMG10'.
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE IT_ZTIMIMG10
        FROM ZTIMIMG10.
      PERFORM P2000_DOWNLOAD  TABLES IT_ZTIMIMG10
                              USING  P_TABNAME.
    WHEN 'ZTIMIMG11'.
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE IT_ZTIMIMG11
        FROM ZTIMIMG11.
      PERFORM P2000_DOWNLOAD  TABLES IT_ZTIMIMG11
                              USING  P_TABNAME.
    WHEN 'ZTIMIMG12'.
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE IT_ZTIMIMG12
        FROM ZTIMIMG12.
      PERFORM P2000_DOWNLOAD  TABLES IT_ZTIMIMG12
                              USING  P_TABNAME.
    WHEN 'ZTIMIMG17'.
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE IT_ZTIMIMG17
        FROM ZTIMIMG17.
      PERFORM P2000_DOWNLOAD  TABLES IT_ZTIMIMG17
                              USING  P_TABNAME.
    WHEN 'ZTIMIMG18'.
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE IT_ZTIMIMG18
        FROM ZTIMIMG18.
      PERFORM P2000_DOWNLOAD  TABLES IT_ZTIMIMG18
                              USING  P_TABNAME.
    WHEN 'ZTIMIMG19'.
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE IT_ZTIMIMG19
        FROM ZTIMIMG19.
      PERFORM P2000_DOWNLOAD  TABLES IT_ZTIMIMG19
                              USING  P_TABNAME.
    WHEN 'ZTIMIMG20'.
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE IT_ZTIMIMG20
        FROM ZTIMIMG20.
      PERFORM P2000_DOWNLOAD  TABLES IT_ZTIMIMG20
                              USING  P_TABNAME.
    WHEN 'ZTIMIMG21'.
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE IT_ZTIMIMG21
        FROM ZTIMIMG21.
      PERFORM P2000_DOWNLOAD  TABLES IT_ZTIMIMG21
                              USING  P_TABNAME.
    WHEN 'ZTIMIMG22'.
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE IT_ZTIMIMG22
        FROM ZTIMIMG22.
      PERFORM P2000_DOWNLOAD  TABLES IT_ZTIMIMG23
                              USING  P_TABNAME.
    WHEN 'ZTIMIMG23'.
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE IT_ZTIMIMG23
        FROM ZTIMIMG23.
      PERFORM P2000_DOWNLOAD  TABLES IT_ZTIMIMG23
                              USING  P_TABNAME.
*    WHEN 'ZTEIMG04'.
*      SELECT *
*        INTO CORRESPONDING FIELDS OF TABLE IT_ZTEIMG04
*        FROM ZTEIMG04.
*      PERFORM P2000_DOWNLOAD  TABLES IT_ZTEIMG04
*                              USING  P_TABNAME.
*    WHEN 'ZTEIMG05'.
*      SELECT *
*        INTO CORRESPONDING FIELDS OF TABLE IT_ZTEIMG05
*        FROM ZTEIMG05.
*      PERFORM P2000_DOWNLOAD  TABLES IT_ZTEIMG05
*                              USING  P_TABNAME.
*    WHEN 'ZTEIMG08'.
*      SELECT *
*        INTO CORRESPONDING FIELDS OF TABLE IT_ZTEIMG08
*        FROM ZTEIMG08.
*      PERFORM P2000_DOWNLOAD  TABLES IT_ZTEIMG08
*                              USING  P_TABNAME.
*    WHEN 'ZTEIMG12'.
*      SELECT *
*        INTO CORRESPONDING FIELDS OF TABLE IT_ZTEIMG12
*        FROM ZTEIMG12.
*      PERFORM P2000_DOWNLOAD  TABLES IT_ZTEIMG12
*                              USING  P_TABNAME.
    WHEN 'ZTIEPORT'.
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE IT_ZTIEPORT
        FROM ZTIEPORT.
      PERFORM P2000_DOWNLOAD  TABLES IT_ZTIEPORT
                              USING  P_TABNAME.
    WHEN 'ZTIMIMGTX'.
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE IT_ZTIMIMGTX
        FROM ZTIMIMGTX.
      PERFORM P2000_DOWNLOAD  TABLES IT_ZTIMIMGTX
                              USING  P_TABNAME.
    WHEN 'ZTIMGTXT'.
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE IT_ZTIMGTXT
        FROM ZTIMGTXT.
      PERFORM P2000_DOWNLOAD  TABLES IT_ZTIMGTXT
                              USING  P_TABNAME.
    WHEN OTHERS.

  ENDCASE.

ENDFORM.                    " P2000_CREATE_DOWNFILE

*&---------------------------------------------------------------------*
*&      Form  P2000_DOWNLOAD
*&---------------------------------------------------------------------*
FORM P2000_DOWNLOAD TABLES   P_TAB
                    USING    P_TABNAME.

  CLEAR : W_FILENAME.

  CONCATENATE 'C:\' P_TABNAME '.TXT' INTO W_FILENAME.
  CALL FUNCTION 'WS_DOWNLOAD'
       EXPORTING
            FILENAME = W_FILENAME
            FILETYPE = 'DAT'
       TABLES
            DATA_TAB = P_TAB.

  IF SY-SUBRC EQ 0.
     MESSAGE I460(ZIM1) WITH W_FILENAME.
  ELSE.
     MESSAGE E461(ZIM1) WITH P_TABNAME.
  ENDIF.

ENDFORM.                    " P2000_DOWNLOAD

*&---------------------------------------------------------------------*
*&      Form  P2000_CREATE_UPFILE
*&---------------------------------------------------------------------*
FORM P2000_CREATE_UPFILE USING    P_TABNAME.

  REFRESH : IT_ZTIMIMG00,  IT_ZTIMIMG01, IT_ZTIMIMG02, IT_ZTIMIMG03,
            IT_ZTIMIMG04,  IT_ZTIMIMG05, IT_ZTIMIMG06, IT_ZTIMIMG07,
            IT_ZTIMIMG08,  IT_ZTIMIMG09, IT_ZTIMIMG10, IT_ZTIMIMG11,
            IT_ZTIMIMG12,  IT_ZTIMIMG17, IT_ZTIMIMG18, IT_ZTIMIMG19,
            IT_ZTIMIMG20,  IT_ZTIMIMG21, IT_ZTIMIMG22, IT_ZTIMIMG23,
*            IT_ZTEIMG04,   IT_ZTEIMG05,  IT_ZTEIMG08,  IT_ZTEIMG12,
            IT_ZTIMIMGTX,  IT_ZTIMGTXT,  IT_ZTIEPORT.
  CLEAR   : IT_ZTIMIMG00,  IT_ZTIMIMG01, IT_ZTIMIMG02, IT_ZTIMIMG03,
            IT_ZTIMIMG04,  IT_ZTIMIMG05, IT_ZTIMIMG06, IT_ZTIMIMG07,
            IT_ZTIMIMG08,  IT_ZTIMIMG09, IT_ZTIMIMG10, IT_ZTIMIMG11,
            IT_ZTIMIMG12,  IT_ZTIMIMG17, IT_ZTIMIMG18, IT_ZTIMIMG19,
            IT_ZTIMIMG20,  IT_ZTIMIMG21, IT_ZTIMIMG22, IT_ZTIMIMG23,
*            IT_ZTEIMG04,   IT_ZTEIMG05,  IT_ZTEIMG08,  IT_ZTEIMG12,
            IT_ZTIMIMGTX,  IT_ZTIMGTXT,  IT_ZTIEPORT.

  CASE P_TABNAME.
    WHEN 'ZTIMIMG00'.
      PERFORM P2000_UPLOAD    TABLES  IT_ZTIMIMG00
                              USING   P_TABNAME.
      LOOP  AT  IT_ZTIMIMG00.
        CLEAR : ZTIMIMG00.
        MOVE-CORRESPONDING IT_ZTIMIMG00 TO ZTIMIMG00.
        MOVE IT_ZTIMIMG00-AMT1  TO  ZTIMIMG00-ZFUSDAM.
        PERFORM SET_CURR_CONV_TO_INTERNAL CHANGING ZTIMIMG00-ZFUSDAM
                                                   ZTIMIMG00-ZFUSD.

        MOVE : SY-MANDT          TO  ZTIMIMG00-MANDT,
               SY-DATUM          TO  ZTIMIMG00-CDAT,
               SY-DATUM          TO  ZTIMIMG00-UDAT,
               SY-UNAME          TO  ZTIMIMG00-ERNAM,
               SY-UNAME          TO  ZTIMIMG00-UNAM.
        MODIFY  ZTIMIMG00.
      ENDLOOP.
*      MODIFY ZTIMIMG00   FROM TABLE   IT_ZTIMIMG00.
    WHEN 'ZTIMIMG01'.
      PERFORM P2000_UPLOAD    TABLES  IT_ZTIMIMG01
                              USING   P_TABNAME.
      MODIFY ZTIMIMG01   FROM TABLE   IT_ZTIMIMG01.
    WHEN 'ZTIMIMG02'.
      PERFORM P2000_UPLOAD    TABLES  IT_ZTIMIMG02
                              USING   P_TABNAME.
      MODIFY ZTIMIMG02   FROM TABLE   IT_ZTIMIMG02.
    WHEN 'ZTIMIMG03'.
      PERFORM P2000_UPLOAD    TABLES  IT_ZTIMIMG03
                              USING   P_TABNAME.
      MODIFY ZTIMIMG03   FROM TABLE   IT_ZTIMIMG03.
    WHEN 'ZTIMIMG04'.
      PERFORM P2000_UPLOAD    TABLES  IT_ZTIMIMG04
                              USING   P_TABNAME.
      MODIFY ZTIMIMG04   FROM TABLE   IT_ZTIMIMG04.
    WHEN 'ZTIMIMG05'.
      PERFORM P2000_UPLOAD    TABLES  IT_ZTIMIMG05
                              USING   P_TABNAME.
      MODIFY ZTIMIMG05   FROM TABLE   IT_ZTIMIMG05.
    WHEN 'ZTIMIMG06'.
      PERFORM P2000_UPLOAD    TABLES  IT_ZTIMIMG06
                              USING   P_TABNAME.
      MODIFY ZTIMIMG06   FROM TABLE   IT_ZTIMIMG06.
    WHEN 'ZTIMIMG07'.
      PERFORM P2000_UPLOAD    TABLES  IT_ZTIMIMG07
                              USING   P_TABNAME.
      MODIFY ZTIMIMG07   FROM TABLE   IT_ZTIMIMG07.
    WHEN 'ZTIMIMG08'.
      PERFORM P2000_UPLOAD    TABLES  IT_ZTIMIMG08
                              USING   P_TABNAME.
      MODIFY ZTIMIMG08   FROM TABLE   IT_ZTIMIMG08.
    WHEN 'ZTIMIMG09'.
      PERFORM P2000_UPLOAD    TABLES  IT_ZTIMIMG09
                              USING   P_TABNAME.
      MODIFY ZTIMIMG09   FROM TABLE   IT_ZTIMIMG09.
    WHEN 'ZTIMIMG10'.
      PERFORM P2000_UPLOAD    TABLES  IT_ZTIMIMG10
                              USING   P_TABNAME.
      MODIFY ZTIMIMG10   FROM TABLE   IT_ZTIMIMG10.
    WHEN 'ZTIMIMG11'.
      PERFORM P2000_UPLOAD    TABLES  IT_ZTIMIMG11
                              USING   P_TABNAME.
      MODIFY ZTIMIMG11   FROM TABLE   IT_ZTIMIMG11.
    WHEN 'ZTIMIMG12'.
      PERFORM P2000_UPLOAD    TABLES  IT_ZTIMIMG12
                              USING   P_TABNAME.
      MODIFY ZTIMIMG12   FROM TABLE   IT_ZTIMIMG12.
    WHEN 'ZTIMIMG17'.
      PERFORM P2000_UPLOAD    TABLES  IT_ZTIMIMG17
                              USING   P_TABNAME.
      MODIFY ZTIMIMG17   FROM TABLE   IT_ZTIMIMG17.
    WHEN 'ZTIMIMG18'.
      PERFORM P2000_UPLOAD    TABLES  IT_ZTIMIMG18
                              USING   P_TABNAME.
      MODIFY ZTIMIMG18   FROM TABLE   IT_ZTIMIMG18.
    WHEN 'ZTIMIMG19'.
      PERFORM P2000_UPLOAD    TABLES  IT_ZTIMIMG19
                              USING   P_TABNAME.
      MODIFY ZTIMIMG19   FROM TABLE   IT_ZTIMIMG19.
    WHEN 'ZTIMIMG20'.
      PERFORM P2000_UPLOAD    TABLES  IT_ZTIMIMG20
                              USING   P_TABNAME.
      MODIFY ZTIMIMG20   FROM TABLE   IT_ZTIMIMG20.
    WHEN 'ZTIMIMG21'.
      PERFORM P2000_UPLOAD    TABLES  IT_ZTIMIMG21
                              USING   P_TABNAME.
      MODIFY ZTIMIMG21   FROM TABLE   IT_ZTIMIMG21.
    WHEN 'ZTIMIMG22'.
      PERFORM P2000_UPLOAD    TABLES  IT_ZTIMIMG22
                              USING   P_TABNAME.
      MODIFY ZTIMIMG22   FROM TABLE   IT_ZTIMIMG22.
    WHEN 'ZTIMIMG23'.
      PERFORM P2000_UPLOAD    TABLES  IT_ZTIMIMG23
                              USING   P_TABNAME.
      MODIFY ZTIMIMG23   FROM TABLE   IT_ZTIMIMG23.
*    WHEN 'ZTEIMG04'.
*      PERFORM P2000_UPLOAD    TABLES  IT_ZTEIMG04
*                              USING   P_TABNAME.
*      MODIFY ZTEIMG04    FROM TABLE   IT_ZTEIMG04.
*    WHEN 'ZTEIMG05'.
*      PERFORM P2000_UPLOAD    TABLES  IT_ZTEIMG05
*                              USING   P_TABNAME.
*      MODIFY ZTEIMG05    FROM TABLE   IT_ZTEIMG05.
*    WHEN 'ZTEIMG08'.
*      PERFORM P2000_UPLOAD    TABLES  IT_ZTEIMG08
*                              USING   P_TABNAME.
*      MODIFY ZTEIMG08    FROM TABLE   IT_ZTEIMG08.
*    WHEN 'ZTEIMG12'.
*      PERFORM P2000_UPLOAD    TABLES  IT_ZTEIMG12
*                              USING   P_TABNAME.
*      MODIFY ZTEIMG12    FROM TABLE   IT_ZTEIMG12.
    WHEN 'ZTIEPORT'.
      PERFORM P2000_UPLOAD    TABLES  IT_ZTIEPORT
                              USING   P_TABNAME.
      MODIFY ZTIEPORT    FROM TABLE   IT_ZTIEPORT.
    WHEN 'ZTIMIMGTX'.
      PERFORM P2000_UPLOAD    TABLES  IT_ZTIMIMGTX
                              USING   P_TABNAME.
      MODIFY ZTIMIMGTX   FROM TABLE   IT_ZTIMIMGTX.
    WHEN 'ZTIMGTXT'.
      PERFORM P2000_UPLOAD    TABLES  IT_ZTIMGTXT
                              USING   P_TABNAME.
      MODIFY ZTIMGTXT    FROM TABLE   IT_ZTIMGTXT.
    WHEN OTHERS.
  ENDCASE.
  IF SY-SUBRC NE 0.
    ROLLBACK WORK.
    MESSAGE  E952(ZIM).
  ELSE.
    COMMIT WORK.
    MESSAGE  I953(ZIM).
  ENDIF.

ENDFORM.                    " P2000_CREATE_UPFILE

*&---------------------------------------------------------------------*
*&      Form  P2000_UPLOAD
*&---------------------------------------------------------------------*
FORM P2000_UPLOAD TABLES   P_TAB
                  USING    P_TABNAME.

  CLEAR : W_FILENAME.

  CONCATENATE 'C:\' P_TABNAME '.TXT' INTO W_FILENAME.
  CALL FUNCTION 'WS_UPLOAD'
    EXPORTING
      FILENAME = W_FILENAME
      FILETYPE = 'DAT'
      DAT_D_FORMAT = 'DD.MM.YYYY'
    TABLES
      DATA_TAB = P_TAB.

  IF SY-SUBRC NE 0.
     MESSAGE E463(ZIM1) WITH W_FILENAME.
  ENDIF.
ENDFORM.                    " P2000_UPLOAD
*&---------------------------------------------------------------------*
*&      Form  P2000_WRITE_NO_MASK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_IT_ZTIMIMG00_AMT1  text
*----------------------------------------------------------------------*
FORM P2000_WRITE_NO_MASK CHANGING P_TEXT_AMOUNT.

  SELECT SINGLE * FROM USR01 WHERE BNAME EQ SY-UNAME.

  CASE USR01-DCPFM.
    WHEN 'X'.    " Decimal point is period: N,NNN.NN
      PERFORM    P2000_CHANGE_SYMBOL    USING P_TEXT_AMOUNT ',' ' '.
      CONDENSE         P_TEXT_AMOUNT    NO-GAPS.
    WHEN 'Y'.    " Decimal point is N NNN NNN,NN
      PERFORM    P2000_CHANGE_SYMBOL    USING P_TEXT_AMOUNT  ',' '.'.
      CONDENSE         P_TEXT_AMOUNT    NO-GAPS.
    WHEN OTHERS. " Decimal point is comma: N.NNN,NN
      PERFORM    P2000_CHANGE_SYMBOL    USING P_TEXT_AMOUNT  '.' ' '.
      PERFORM    P2000_CHANGE_SYMBOL    USING P_TEXT_AMOUNT  ',' '.'.
      CONDENSE         P_TEXT_AMOUNT    NO-GAPS.
  ENDCASE.

ENDFORM.                    " P2000_WRITE_NO_MASK

*&---------------------------------------------------------------------*
*&      Form  P2000_CHANGE_SYMBOL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_TEXT_AMOUNT  text
*      -->P_1378   text
*      -->P_1379   text
*----------------------------------------------------------------------*
FORM P2000_CHANGE_SYMBOL USING   P_AMOUNT P_FROM P_TO.
  DO.
    REPLACE  P_FROM   WITH   P_TO  INTO    P_AMOUNT.
    IF  SY-SUBRC  <>    0.
      EXIT.
    ENDIF.
  ENDDO.
ENDFORM.                    " P2000_CHANGE_SYMBOL
*&---------------------------------------------------------------------*
*&      Form  SET_CURR_CONV_TO_INTERNAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_IT_ZTIMIMG00_AMT1  text
*      <--P_IT_ZTIMIMG00_ZFUSD  text
*----------------------------------------------------------------------*
FORM SET_CURR_CONV_TO_INTERNAL CHANGING P_AMOUNT
                                        P_WAERS.

  BAPICURR-BAPICURR = P_AMOUNT.

  CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_INTERNAL'
    EXPORTING
      CURRENCY             = P_WAERS
      AMOUNT_EXTERNAL      = BAPICURR-BAPICURR
      MAX_NUMBER_OF_DIGITS = DIGITS
    IMPORTING
      AMOUNT_INTERNAL      = P_AMOUNT
    EXCEPTIONS
      OTHERS               = 1.

ENDFORM.                    " SET_CURR_CONV_TO_INTERNAL
