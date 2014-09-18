*& Report  ZRIMEDIR03                                                  *
*&---------------------------------------------------------------------*
*&  프로그램명 : EDI RECEIPT(MATRIX 용)                                *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2001.07.13                                            *
*&---------------------------------------------------------------------*
*&   DESC.     : EDI RECEIPT(BATCH JOB 으로 EDI 수신)
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*& 나현주 : Matrix2B 와의 Interface로 인한 변경<2002.08.20>.
*&---------------------------------------------------------------------*
REPORT  ZRIMEDIR03  MESSAGE-ID ZIM
                     NO STANDARD PAGE HEADING.

INCLUDE : <ICON>.

*------ EDI
DATA  :  UPLOAD_PATH(300)     TYPE       C.     " loading datA
DATA  :  FILE_NAME            LIKE       ZTDHF1-FILENAME.

DATA : W_OK_CODE         LIKE   SY-UCOMM,
       W_READ_CNT        TYPE   I,
       W_ZFDHENO         LIKE   ZTDHF1-ZFDHENO,
       W_ZFCDDOC         LIKE   ZTCDF1-ZFCDDOC,
       W_ZFDHSRO         LIKE   ZTDHF1-ZFDHSRO,
       W_ZFDHREF         LIKE   ZTDHF1-ZFDHREF.

DATA  W_EDI_RECORD(65535).
DATA: BEGIN OF IT_TAB OCCURS 0,
      W_RECORD   LIKE     W_EDI_RECORD,
      END OF IT_TAB.

DATA: BEGIN OF IT_EDI OCCURS 0,
      RECORD   LIKE     W_EDI_RECORD,
      END OF IT_EDI.

DATA: BEGIN OF IT_TAB1 OCCURS 0,
      ZFDHENO     LIKE     ZTDHF1-ZFDHENO,    ">문서관리번호.
      ZFDHDOC     LIKE     ZTDHF1-ZFDHDOC,    ">문서 종류.
      ZFDOCNO     LIKE     ZTREQST-ZFDOCNO,   ">SEND 문서 번호.
      ZFDOCNOR    LIKE     ZTREQST-ZFDOCNOR,  ">RECEIPT 문서 번호.
      EBELN       LIKE     ZTREQHD-EBELN,     ">구매오더 번호.
      ZFREQNO     LIKE     ZTREQST-ZFREQNO,   ">수입의뢰번호.
      ZFAMDNO     LIKE     ZTREQST-ZFAMDNO,   ">AMEND SEQ.
      ZFOPNNO     LIKE     ZTREQST-ZFOPNNO,   ">승인번호.
      ZFOPNDT     LIKE     ZTREQST-ZFOPNDT,   ">개설일자.
      ZFFILE(100),
      END OF IT_TAB1.

DATA: BEGIN OF IT_SELECT OCCURS 0,
      ZFDHENO     LIKE     ZTDHF1-ZFDHENO,  " 문서관리번호.
      ZFDHDOC     LIKE     ZTDHF1-ZFDHDOC,  " 문서 종류.
      FILENAME(100),
      END OF IT_SELECT.

DATA: BEGIN OF IT_FLATFILE OCCURS 0,
      FILENAME(100),
      END OF IT_FLATFILE.

DATA: BEGIN OF IT_BACKUP OCCURS 0,
      FILENAME(100),
      END OF IT_BACKUP.

DATA: BEGIN OF IT_FILENAME OCCURS 0,
      FILENAME(300),
      END OF IT_FILENAME.

DATA: TEXT100(100),
      TEXT50(50).

DATA : L_COUNT    TYPE   I.
DATA: BEGIN OF MTAB_DATA OCCURS 0,
      LINE(132)   TYPE C,
END OF MTAB_DATA.

*>>> ERROR 처리용.
DATA : BEGIN OF IT_ERR_LIST OCCURS 0.
       INCLUDE  STRUCTURE  BDCMSGCOLL.
       DATA : ICON         LIKE BAL_S_DMSG-%_ICON,
              MESSTXT(266) TYPE C.
DATA : END OF IT_ERR_LIST.

DATA:
      MI_HANDLE       TYPE I,
      ANTWORT(1)      TYPE C,             " 공통 popup Screen에서 사?
      L_LEN           TYPE I,
      L_STRLEN        TYPE I,
      L_SIZE          TYPE I,
      W_MOD           TYPE I,
      L_DATE          TYPE SY-DATUM,
      INCLUDE(8)      TYPE C,             "
      L_TIME          TYPE SY-UZEIT,
      W_BUKRS         LIKE ZTIMIMGTX-BUKRS.

*-----------------------------------------------------------------------
* Tables 및 변수 Define
*-----------------------------------------------------------------------
INCLUDE   ZRIMPRELTOP.    " 구매 Released  Report Data Define용 Include
INCLUDE   ZRIMUTIL01.     " Utility function 모?

**-------------------------------------
** Selection Screen 절.
**----------------------------------
*SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
*SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
*    PARAMETERS: P_BUKRS LIKE ZTIMIMGTX-BUKRS OBLIGATORY
*                        DEFAULT 'KHNP'.
*SELECTION-SCREEN END OF BLOCK B1.

* PARAMETER 초기값 Setting
INITIALIZATION.                          " 초기값 SETTING
   PERFORM   P2000_SET_PARAMETER.

*-----------------------------------------------------------------------
* START OF SELECTION 절.
*-----------------------------------------------------------------------
START-OF-SELECTION.
* Import System Config Check

   PERFORM   P100_GET_EDI_IMG  USING W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
   PERFORM   P1000_GET_UPLOAD_FILE  USING W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.

  SET  TITLEBAR 'ZIME10'.          " TITLE BAR

ENDFORM.                    " P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_UPLOAD_FILE
*&---------------------------------------------------------------------*
*       UPLOAD용 FILENAME을 발췌하는 서브루틴.
*----------------------------------------------------------------------*
FORM P1000_GET_UPLOAD_FILE USING    W_ERR_CHK.

   FREE : IT_TAB, IT_TAB1.
   CLEAR : IT_TAB, IT_TAB1.

   MOVE 'N'        TO     W_ERR_CHK.

*> FTP CONNECT.
   PERFORM  P2000_FTP_CONNECT.
   CHECK : W_ERR_CHK EQ 'N'.

*> CURRENT DIR.
   PERFORM  P2000_GET_CURRENT_DIR USING UPLOAD_PATH
                                        MI_HANDLE.
   CHECK : W_ERR_CHK EQ 'N'.

*> FILE LIST GET.
   PERFORM  P2000_GET_FILE_LIST.
   CHECK : W_ERR_CHK EQ 'N'.

*> FTP DISCONNET.
   CALL FUNCTION 'FTP_DISCONNECT'
      EXPORTING
            HANDLE   =   MI_HANDLE
      EXCEPTIONS
            OTHERS   =   1.

  LOOP AT IT_FLATFILE.
*>>   file ---> internal table
      MOVE IT_FLATFILE-FILENAME TO FILE_NAME.
      PERFORM  P1000_FILE_TO_ITAB.
*>>   FILE COUNTER 증가.
      ADD    1    TO    L_COUNT.
  ENDLOOP.

*>> FTP CONNECT 하여서 성공한 DATA 는 파일명을 RENAME.
   PERFORM  P2000_FTP_CONNECT.
   CHECK : W_ERR_CHK EQ 'N'.

   " 파일 RENAME
   LOOP  AT  IT_BACKUP.
      MOVE  IT_BACKUP-FILENAME  TO  FILE_NAME.
      PERFORM  P2000_FILE_RENAME.
   ENDLOOP.

*> FTP DISCONNET.
   CALL FUNCTION 'FTP_DISCONNECT'
      EXPORTING
            HANDLE   =   MI_HANDLE
      EXCEPTIONS
            OTHERS   =   1.

ENDFORM.                    " P1000_GET_UPLOAD_FILE

*&---------------------------------------------------------------------*
*&      Form  P1000_FILE_TO_ITAB
*&---------------------------------------------------------------------*
*       SAM-FILE 내역을 Internal Table로 Append
*----------------------------------------------------------------------*
FORM P1000_FILE_TO_ITAB.

   CASE FILE_NAME(6).
      WHEN 'INF700'.    ">L/C 개설응답서.
         CALL FUNCTION 'ZIM_MAT_INF700_EDI_DOC'
              EXPORTING
                 W_FILENAME = FILE_NAME
                 BACK_PATH  = ZTIMIMGTX-ZFRBAK
                 W_DEL      = 'X'
             TABLES
                 RETURN     = RETURN
             EXCEPTIONS
                 OTHERS     = 4.
      WHEN 'INF707'.    ">L/C 변경응답서.
         CALL FUNCTION 'ZIM_MAT_INF707_EDI_DOC'
              EXPORTING
                 W_FILENAME = FILE_NAME
                 BACK_PATH  = ZTIMIMGTX-ZFRBAK
                 W_DEL      = 'X'
              TABLES
                 RETURN     = RETURN
              EXCEPTIONS
                 OTHERS     = 4.
         WHEN OTHERS.
   ENDCASE.

   IF SY-SUBRC EQ 0.
      CLEAR : IT_BACKUP.
      MOVE  FILE_NAME  TO  IT_BACKUP-FILENAME.
      APPEND  IT_BACKUP.
   ENDIF.

ENDFORM.                    " P1000_FILE_TO_ITAB
*&---------------------------------------------------------------------*
*&      Form  P2000_GET_CURRENT_DIR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_GET_CURRENT_DIR  USING UPLOAD_PATH   MI_HANDLE.
DATA: L_STRLEN           TYPE I,
      L_START            TYPE I,
      L_END              TYPE I,
      L_LEN              TYPE I,
      L_POSITION1        TYPE I,
      L_POSITION2        TYPE I,
      L_FIRST_CHK        TYPE C VALUE 'N',
      L_COMMAND(50)      TYPE C.

   CONCATENATE 'cd' 'edi_recv' INTO L_COMMAND SEPARATED BY SPACE.

   PERFORM P2000_SHOW_BAR
           USING 'Receipt된 Flat Files 정보를 읽는 중입니다...' 0.

*> INBOUND DIRECTORY로 이동.
   CALL FUNCTION 'FTP_COMMAND'
        EXPORTING
           HANDLE        = MI_HANDLE
           COMMAND       = L_COMMAND
        TABLES
           DATA          = MTAB_DATA
        EXCEPTIONS
           TCPIP_ERROR   = 1
           COMMAND_ERROR = 2
           DATA_ERROR    = 3
           OTHERS        = 4.

* do some error checking.
   CASE SY-SUBRC.
      WHEN 1.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S101(04) WITH ZTIMIMGTX-HOST.   EXIT.
      WHEN 2.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S200(04).      EXIT.
      WHEN 3.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S207(04).      EXIT.
      WHEN 4.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S208(04).      EXIT.
   ENDCASE.

*> DIRECTORY 정보 GET.
   REFRESH : MTAB_DATA.
   CALL FUNCTION 'FTP_COMMAND'
        EXPORTING
           HANDLE        = MI_HANDLE
           COMMAND       = 'pwd'
        TABLES
           DATA          = MTAB_DATA
        EXCEPTIONS
           TCPIP_ERROR   = 1
           COMMAND_ERROR = 2
           DATA_ERROR    = 3
           OTHERS        = 4.

* do some error checking.
   CASE SY-SUBRC.
      WHEN 1.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S101(04) WITH ZTIMIMGTX-HOST.   EXIT.
      WHEN 2.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S200(04).      EXIT.
      WHEN 3.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S207(04).      EXIT.
      WHEN 4.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S208(04).      EXIT.
   ENDCASE.

  READ TABLE MTAB_DATA INDEX 2.
  IF SY-SUBRC EQ 0.
     L_STRLEN   = STRLEN( MTAB_DATA ).
     L_POSITION1 = 0.
     L_POSITION2 = 0.
     DO.
        IF L_POSITION1 GT L_STRLEN.
           EXIT.
        ENDIF.
        IF MTAB_DATA+L_POSITION1(1) EQ '"'.
           IF L_FIRST_CHK EQ 'N'.
              L_START = L_POSITION1 + 1.
              L_FIRST_CHK = 'Y'.
           ELSE.
              L_END   = L_POSITION1.
           ENDIF.
        ENDIF.
        ADD 1  TO  L_POSITION1.
     ENDDO.
     L_LEN = L_END - L_START.
     UPLOAD_PATH = MTAB_DATA+L_START(L_LEN).
     CONCATENATE UPLOAD_PATH '/' INTO UPLOAD_PATH.
  ELSE.
     MOVE 'Y'        TO     W_ERR_CHK.
     MESSAGE S207(04).
  ENDIF.

ENDFORM.                    " P2000_GET_CURRENT_DIR
*&---------------------------------------------------------------------*
*&      Form  P2000_FTP_CONNECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_FTP_CONNECT.

   PERFORM P2000_SHOW_BAR USING 'File Server에 로그인 중입니다...' 0.

DATA: mc_password(20) TYPE c ,
      mc_userid(20)   TYPE c ,
      mi_key          TYPE i VALUE 26101957,
      mi_pwd_len      TYPE i.

   DESCRIBE FIELD MC_PASSWORD LENGTH MI_PWD_LEN.
**-- FTP_CONNECT requires an encrypted password to work
   MC_PASSWORD = 'edi_int'.
   CALL 'AB_RFC_X_SCRAMBLE_STRING'
      ID 'SOURCE' FIELD MC_PASSWORD ID 'KEY'         FIELD MI_KEY
      ID 'SCR'    FIELD 'X'         ID 'DESTINATION' FIELD MC_PASSWORD
      ID 'DSTLEN' FIELD MI_PWD_LEN.

  CALL FUNCTION 'FTP_CONNECT'
       EXPORTING
           USER            = 'edi_int' "MC_USERID
           PASSWORD        = MC_PASSWORD
           HOST            = '10.135.9.34'
           RFC_DESTINATION = 'SAPFTP'
      IMPORTING
           HANDLE          = MI_HANDLE
      EXCEPTIONS
           NOT_CONNECTED   = 1
           OTHERS          = 2.

   CASE SY-SUBRC.
      WHEN 1.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S201(04) WITH ZTIMIMGTX-HOST.
      WHEN 2.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S202(04) WITH ZTIMIMGTX-UNAME ZTIMIMGTX-HOST.
   ENDCASE.

ENDFORM.                    " P2000_FTP_CONNECT
*&---------------------------------------------------------------------*
*&      Form  P2000_GET_FILE_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_GET_FILE_LIST.

   REFRESH : MTAB_DATA.
   CALL FUNCTION 'FTP_COMMAND'
        EXPORTING
           HANDLE        = MI_HANDLE
           COMMAND       = 'dir *.itf -Nf'
        TABLES
           DATA          = MTAB_DATA
        EXCEPTIONS
           TCPIP_ERROR   = 1
           COMMAND_ERROR = 2
           DATA_ERROR    = 3
           OTHERS        = 4.

   W_SY_SUBRC = SY-SUBRC.

   IF W_SY_SUBRC EQ 0.
      REFRESH : IT_FLATFILE.
      DESCRIBE TABLE MTAB_DATA LINES W_LINE.
      IF W_LINE LT 4.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S920.
         EXIT.
      ENDIF.
      W_LINE = W_LINE - 1.
      LOOP AT MTAB_DATA FROM 4 TO W_LINE.
         CASE MTAB_DATA+39(6).
            WHEN 'INF700' OR 'INF707' OR 'LOGADV' .
            WHEN OTHERS.
               CONTINUE.
         ENDCASE.
         CLEAR : IT_FLATFILE.
         CONCATENATE ZTIMIMGTX-ZFPATH '/' MTAB_DATA+39 INTO IT_FLATFILE.
         " EDI SERVER 의 파일을 SAP SERVER로 FILE TRANSFER
         PERFORM  P3000_FILE_TRANSFER.
         APPEND IT_FLATFILE.
      ENDLOOP.
   ELSE.
* do some error checking.
      CASE SY-SUBRC.
         WHEN 1.
            MOVE 'Y'        TO     W_ERR_CHK.
            MESSAGE S101(04) WITH ZTIMIMGTX-HOST.   EXIT.
         WHEN 2.
            MOVE 'Y'        TO     W_ERR_CHK.
            MESSAGE S200(04).      EXIT.
         WHEN 3.
            MOVE 'Y'        TO     W_ERR_CHK.
            MESSAGE S207(04).      EXIT.
         WHEN 4.
            MOVE 'Y'        TO     W_ERR_CHK.
            MESSAGE S208(04).      EXIT.
      ENDCASE.
   ENDIF.

ENDFORM.                    " P2000_GET_FILE_LIST
*&---------------------------------------------------------------------*
*&      Form  P2000_MULTI_MSG_MAKE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_MULTI_MSG_MAKE.

   LOOP AT  RETURN.

      MOVE : RETURN-TYPE         TO     IT_ERR_LIST-MSGTYP,
             RETURN-ID           TO     IT_ERR_LIST-MSGID,
             RETURN-NUMBER       TO     IT_ERR_LIST-MSGNR,
             RETURN-MESSAGE_V1   TO     IT_ERR_LIST-MSGV1,
             RETURN-MESSAGE_V2   TO     IT_ERR_LIST-MSGV2,
             RETURN-MESSAGE_V3   TO     IT_ERR_LIST-MSGV3,
             RETURN-MESSAGE_V4   TO     IT_ERR_LIST-MSGV4,
             RETURN-MESSAGE      TO     IT_ERR_LIST-MESSTXT.
*             ZTIV-ZFIVNO         TO     IT_ERR_LIST-ZFIVNO.

      CASE IT_ERR_LIST-MSGTYP.
         WHEN 'E' OR 'A'.
            MOVE ICON_LED_RED             TO     IT_ERR_LIST-ICON.
         WHEN 'I'.
            MOVE ICON_LED_GREEN           TO     IT_ERR_LIST-ICON.
         WHEN 'S'.
            MOVE ICON_LED_GREEN           TO     IT_ERR_LIST-ICON.
         WHEN 'W'.
            MOVE ICON_LED_YELLOW          TO     IT_ERR_LIST-ICON.
      ENDCASE.
      APPEND  IT_ERR_LIST.
   ENDLOOP.

ENDFORM.                    " P2000_MULTI_MSG_MAKE

*&---------------------------------------------------------------------*
*&      Form  P100_GET_EDI_IMG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P100_GET_EDI_IMG USING P_ERR_CHK.

   SELECT SINGLE * FROM ZTIMIMGTX
                  WHERE BUKRS EQ 'KHNP'.

   IF SY-SUBRC NE 0.
       P_ERR_CHK = 'Y'.
   ENDIF.

ENDFORM.                    " P100_GET_EDI_IMG
*&---------------------------------------------------------------------*
*&      Form  P3000_FILE_TRANSFER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_FILE_TRANSFER .

   DATA : L_COMMAND(80)  TYPE  C.

   CLEAR : L_COMMAND.

   CONCATENATE 'get' IT_FLATFILE+26  IT_FLATFILE-FILENAME
                     INTO L_COMMAND SEPARATED BY SPACE.

*> INBOUND DIRECTORY로 이동.
   CALL FUNCTION 'FTP_COMMAND'
        EXPORTING
           HANDLE        = MI_HANDLE
           COMMAND       = L_COMMAND
        TABLES
           DATA          = MTAB_DATA
        EXCEPTIONS
           TCPIP_ERROR   = 1
           COMMAND_ERROR = 2
           DATA_ERROR    = 3
           OTHERS        = 4.

* do some error checking.
   CASE SY-SUBRC.
      WHEN 1.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S101(04) WITH ZTIMIMGTX-HOST.   EXIT.
      WHEN 2.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S200(04).      EXIT.
      WHEN 3.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S207(04).      EXIT.
      WHEN 4.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S208(04).      EXIT.
   ENDCASE.

ENDFORM.                    " P3000_FILE_TRANSFER
*&---------------------------------------------------------------------*
*&      Form  P2000_FILE_RENAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_FILE_RENAME .

   DATA : L_COMMAND(80)  TYPE  C.

   CLEAR : L_COMMAND.

   CONCATENATE 'rename' FILE_NAME 'O' INTO L_COMMAND SEPARATED BY SPACE.
   CONCATENATE L_COMMAND FILE_NAME INTO L_COMMAND.

*> INBOUND DIRECTORY로 이동.
   CALL FUNCTION 'FTP_COMMAND'
        EXPORTING
           HANDLE        = MI_HANDLE
           COMMAND       = L_COMMAND
        TABLES
           DATA          = MTAB_DATA
        EXCEPTIONS
           TCPIP_ERROR   = 1
           COMMAND_ERROR = 2
           DATA_ERROR    = 3
           OTHERS        = 4.

* do some error checking.
   CASE SY-SUBRC.
      WHEN 1.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S101(04) WITH ZTIMIMGTX-HOST.   EXIT.
      WHEN 2.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S200(04).      EXIT.
      WHEN 3.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S207(04).      EXIT.
      WHEN 4.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S208(04).      EXIT.
   ENDCASE.

ENDFORM.                    " P2000_FILE_RENAME
