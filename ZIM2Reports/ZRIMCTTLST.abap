*&---------------------------------------------------------------------
*& Report  ZRIMMCCLLST
*&---------------------------------------------------------------------
*&  프로그램명 : 컨테이너운송 현황
*&      작성자 : 이채경 INFOLINK Ltd.
*&      작성일 : 2001.09.10
*&---------------------------------------------------------------------
*&   DESC.     :
*&
*&---------------------------------------------------------------------
*& [변경내용]
*&
*&---------------------------------------------------------------------
REPORT  ZRIMCCTLST  MESSAGE-ID ZIM
                    LINE-SIZE 111
                    NO STANDARD PAGE HEADING.
TABLES: ZTBLINR, ZTBL,ZTIDS,ZTIMIMG08, T001W,LFA1.
*----------------------------------------------------------------------
*  리스트용 INTERNAL TABLE
*----------------------------------------------------------------------
DATA : BEGIN OF IT_BL OCCURS 0,
       ZFBLNO       LIKE     ZTBL-ZFBLNO,     " B/L 관리번
       NAME1        LIKE     T001W-NAME1,     " 플랜트명.
       ZFTRCK       LIKE     ZTBL-ZFTRCK,
       ZFBNDT       LIKE     ZTBL-ZFBNDT,     " 보세운송일.
       ZFRPTTY      LIKE     ZTBL-ZFRPTTY,    " 수입신고형태.
       ZF20FT       LIKE     ZTBL-ZF20FT,
       ZF40FT       LIKE     ZTBL-ZF20FT,
       ZF20FT1      TYPE I,     " 20
       ZF20FT2      TYPE I,
       ZF20FT3      TYPE I,
       ZF20FT4      TYPE I,
       ZF20FT5      TYPE I,
       ZF20FT6      TYPE I,
       ZFL20FT      TYPE I,     " 20 FEET TOTAL.
       ZF40FT1      TYPE I,     " 40
       ZF40FT2      TYPE I,
       ZF40FT3      TYPE I,
       ZF40FT4      TYPE I,
       ZF40FT5      TYPE I,
       ZFL40FT      TYPE I,     " 40 FEET TOTAL.
       ZFWERKS      LIKE     ZTBL-ZFWERKS.    " PLANT
DATA : END OF IT_BL.

DATA : BEGIN OF IT_TAB OCCURS 0,
       ZFWERKS      LIKE     ZTBL-ZFWERKS,    " PLANT
       ZFTRCK       LIKE     ZTBL-ZFTRCK,     " TRCK
       ZF20FT1      TYPE I,     " 20
       ZF20FT2      TYPE I,
       ZF20FT3      TYPE I,
       ZF20FT4      TYPE I,
       ZF20FT5      TYPE I,
       ZF20FT6      TYPE I,
       ZFL20FT      TYPE I,     " 20 FEET TOTAL.
       ZF40FT1      TYPE I,     " 40
       ZF40FT2      TYPE I,
       ZF40FT3      TYPE I,
       ZF40FT4      TYPE I,
       ZF40FT5      TYPE I,
       ZFL40FT      TYPE I.     " 40 FEET TOTAL.
DATA : END OF IT_TAB.

DATA :  W_ERR_CHK     TYPE C,
        W_LCOUNT      TYPE I,
        W_FIELD_NM    TYPE C,
        W_PAGE        TYPE I,
        W_TITLE(50),
        W_TITLE1(50),
        W_DOM_TEX1     LIKE DD07T-DDTEXT,
        W_FNAME        LIKE ZTIMIMG08-ZFCDNM,
        W_CHK_TITLE,
        W_PAGE_CHK,
        W_LINE        TYPE I,
        W_GUBUN(50),
        W_COUNT       TYPE I,
        W_SUBRC       LIKE SY-SUBRC,
        W_TABIX       LIKE SY-TABIX,
        W_ZFCLSEQ     LIKE ZTIDS-ZFCLSEQ,
        W_LIST_INDEX  LIKE SY-TABIX.

INCLUDE   ZRIMSORTCOM.    " REPORT Sort
INCLUDE   ZRIMUTIL01.     " Utility function 모?

*----------------------------------------------------------------------
* Selection Screen ?
*----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS: S_BUKRS   FOR ZTBL-BUKRS NO-EXTENSION NO INTERVALS,
                   S_TRQDT   FOR ZTBL-ZFTRQDT,      " 운송요청일.
                   S_BLSDP   FOR ZTBL-ZFBLSDP,       " 송부처.
                   S_WERKS   FOR ZTBL-ZFWERKS,      " PLANT,
                   S_TRCK    FOR ZTBL-ZFTRCK.       " 운송사.
SELECTION-SCREEN END OF BLOCK B1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_BLSDP-LOW.
   PERFORM   P1000_BL_SDP_HELP(ZRIMBWGILST)  USING  S_BLSDP-LOW.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_BLSDP-HIGH.
   PERFORM   P1000_BL_SDP_HELP(ZRIMBWGILST)  USING  S_BLSDP-HIGH.


INITIALIZATION.                          " 초기값 SETTING
   PERFORM   P2000_INIT.

*title Text Write
TOP-OF-PAGE.
 PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...

*----------------------------------------------------------------------
* START OF SELECTION ?
*----------------------------------------------------------------------
START-OF-SELECTION.
* 권한 검증 함?
   PERFORM   P2000_AUTHORITY_CHECK     USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
* 테이블 SELECT
   PERFORM   P1000_READ_DATA USING W_ERR_CHK.
   IF W_ERR_CHK = 'Y'.
      MESSAGE S738.  EXIT.
   ENDIF.

* 레포트 Write
   PERFORM  P3000_DATA_WRITE.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*----------------------------------------------------------------------
* User Command
*----------------------------------------------------------------------
AT USER-COMMAND.

   CASE SY-UCOMM.
*      WHEN 'STUP' OR 'STDN'.         " SORT 선택?
*         W_FIELD_NM = 'ZFWERKS'.
*         ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
*         PERFORM HANDLE_SORT TABLES  IT_TAB
*                             USING   SY-UCOMM.
*    WHEN 'REFR'.
*          PERFORM P1000_READ_DATA.
*          PERFORM RESET_LIST.
     WHEN 'DISP'.
        IF W_TABIX IS INITIAL.
           MESSAGE S962.    EXIT.
        ENDIF.
        PERFORM P2000_TO_DISP_DETAIL USING IT_TAB-ZFWERKS
                                           IT_TAB-ZFTRCK.

     WHEN 'DOWN'.          " FILE DOWNLOAD....
          PERFORM P3000_TO_PC_DOWNLOAD.
  ENDCASE.
  CLEAR: IT_TAB, W_TABIX.

*&---------------------------------------------------------------------
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------
FORM P3000_TITLE_WRITE.

  SKIP 1.
  IF W_PAGE_CHK EQ 'Y'.
     WRITE : /45 '[ 컨테이너운송 현황 ]'
     COLOR COL_HEADING INTENSIFIED OFF.
     IF NOT S_TRQDT[] IS INITIAL.
        WRITE : / 'Date   : ', S_TRQDT-LOW,'~',S_TRQDT-HIGH.
     ENDIF.
     W_PAGE_CHK = 'N'.
  ENDIF.
  CLEAR LFA1.
  SELECT SINGLE *
    FROM LFA1
   WHERE LIFNR = IT_TAB-ZFTRCK.
  IF SY-SUBRC EQ 0.
     WRITE : /(06) IT_TAB-ZFTRCK,':',LFA1-NAME1.
  ELSE.
     WRITE: /(06)'미입력'.
  ENDIF.
  WRITE : / SY-ULINE.
  FORMAT RESET.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE NO-GAP,(25) '     ' NO-GAP CENTERED
            COLOR COL_NORMAL INTENSIFIED OFF,
            SY-VLINE NO-GAP,(13) '입항전신고' NO-GAP CENTERED,
            SY-VLINE NO-GAP,(13) '부두통관'   NO-GAP CENTERED,
            SY-VLINE NO-GAP,(13) '보세운송'   NO-GAP CENTERED,
            SY-VLINE NO-GAP,(13) '자가입고'   NO-GAP CENTERED,
            SY-VLINE NO-GAP,(13) '영업창고'   NO-GAP CENTERED,
            SY-VLINE NO-GAP,(13) '계' NO-GAP CENTERED,
            SY-VLINE.
  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  WRITE :/ SY-VLINE,27 SY-ULINE.
  FORMAT RESET.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.

  WRITE : / SY-VLINE NO-GAP,(25) '   ' NO-GAP CENTERED
            COLOR COL_NORMAL INTENSIFIED OFF,
            SY-VLINE NO-GAP,(06) '20'  NO-GAP CENTERED, " 입항전.
            SY-VLINE NO-GAP,(06) '40'  NO-GAP CENTERED,
            SY-VLINE NO-GAP,(06) '20'  NO-GAP CENTERED, " 부두통관.
            SY-VLINE NO-GAP,(06) '40'  NO-GAP CENTERED,
            SY-VLINE NO-GAP,(06) '20'  NO-GAP CENTERED, " 보세운송.
            SY-VLINE NO-GAP,(06) '40'  NO-GAP CENTERED,
            SY-VLINE NO-GAP,(06) '20'  NO-GAP CENTERED, " 자가입고.
            SY-VLINE NO-GAP,(06) '40'  NO-GAP CENTERED,
            SY-VLINE NO-GAP,(06) '20'  NO-GAP CENTERED, " 영업창고.
            SY-VLINE NO-GAP,(06) '40'  NO-GAP CENTERED,
            SY-VLINE NO-GAP,(06) '20'  NO-GAP CENTERED, " 계.
            SY-VLINE NO-GAP,(06) '40'  NO-GAP CENTERED,
            SY-VLINE.
  WRITE : / SY-ULINE.

ENDFORM.                    " P3000_TITLE_WRITE

*&---------------------------------------------------------------------
*&      Form  P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------
FORM P2000_AUTHORITY_CHECK USING    W_ERR_CHK.

   W_ERR_CHK = 'N'.
*----------------------------------------------------------------------
*  해당 화면 AUTHORITY CHECK
*----------------------------------------------------------------------
*   AUTHORITY-CHECK OBJECT 'ZI_BL_MGT'
*           ID 'ACTVT' FIELD '*'.
*
*   IF SY-SUBRC NE 0.
*      MESSAGE S960 WITH SY-UNAME 'B/L 관리 트랜잭션'.
*      W_ERR_CHK = 'Y'.   EXIT.
*   ENDIF.

ENDFORM.                    " P2000_AUTHORITY_CHECK

*&---------------------------------------------------------------------
*&      Form  P1000_GET_IT_TAB
*&---------------------------------------------------------------------
FORM P1000_READ_DATA USING W_ERR_CHK.

  W_ERR_CHK = 'N'.

**  B/L
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_BL
      FROM ZTBL
     WHERE ZFRPTTY NE SPACE       " 미통관.
       AND BUKRS   IN S_BUKRS
       AND ZFTRQDT IN S_TRQDT     " 컨테이너운송요청일.
       AND ZFBLSDP IN S_BLSDP     " 송부처.
       AND ZFTRCK  IN S_TRCK      " 운송사.            " 변경되어서
       AND ZFSHTY  EQ 'F'         " FULL CONTAINER.
       AND ZFWERKS IN S_WERKS.    " PLANT,

  IF SY-SUBRC <> 0.  W_ERR_CHK = 'Y'.   EXIT.    ENDIF.

 LOOP AT IT_BL.
     W_TABIX = SY-TABIX.
     CASE IT_BL-ZFRPTTY.
        WHEN  'A'.	" 입항전 통관.
          MOVE: IT_BL-ZF20FT  TO IT_BL-ZF20FT1,
                IT_BL-ZF40FT  TO IT_BL-ZF40FT1.
        WHEN  'D'.	" 부두(내장) 통관.
          MOVE: IT_BL-ZF20FT  TO IT_BL-ZF20FT2,
                IT_BL-ZF40FT  TO IT_BL-ZF40FT2.
        WHEN  'B'.	" 보세운송 통관.
          MOVE: IT_BL-ZF20FT  TO IT_BL-ZF20FT3,
                IT_BL-ZF40FT  TO IT_BL-ZF40FT3.
        WHEN  'N'.	" 자가장치장 통관.
          MOVE: IT_BL-ZF20FT  TO IT_BL-ZF20FT4,
                IT_BL-ZF40FT  TO IT_BL-ZF40FT4.
        WHEN  'W'.	" 영업장치장 통관.
          MOVE: IT_BL-ZF20FT  TO IT_BL-ZF20FT5,
                IT_BL-ZF40FT  TO IT_BL-ZF40FT5.
        WHEN OTHERS.
     ENDCASE.
     IT_BL-ZFL20FT =
        IT_BL-ZF20FT1 + IT_BL-ZF20FT2 + IT_BL-ZF20FT3 +
        IT_BL-ZF20FT4 + IT_BL-ZF20FT5.
     IT_BL-ZFL40FT =
        IT_BL-ZF40FT1 + IT_BL-ZF40FT2 + IT_BL-ZF40FT3 +
        IT_BL-ZF40FT4 + IT_BL-ZF40FT5.
     MODIFY IT_BL INDEX W_TABIX.
     MOVE-CORRESPONDING IT_BL TO IT_TAB.
     COLLECT IT_TAB.
  ENDLOOP.

  DESCRIBE TABLE IT_TAB LINES W_LINE.
  IF W_LINE = 0.
     MESSAGE S738.  W_ERR_CHK = 'Y'.EXIT.
  ENDIF.

ENDFORM.                    " P1000_READ_DATA

*&---------------------------------------------------------------------
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------
FORM P3000_DATA_WRITE .

   SET TITLEBAR  'ZIMR64'.
   SET PF-STATUS 'ZIMR64'.
   SORT IT_TAB BY ZFTRCK.
   LOOP AT IT_TAB.
      W_TABIX = SY-TABIX.
      ON CHANGE OF IT_TAB-ZFTRCK.
        NEW-PAGE.
      ENDON.
      PERFORM   P3000_LINE_WRITE.
      AT LAST.
         PERFORM P3000_LINE_TOTAL.
      ENDAT.

   ENDLOOP.
   CLEAR: IT_TAB, W_TABIX.

ENDFORM.                    " P3000_DATA_WRITE
*&---------------------------------------------------------------------
*&      Form  P2000_INIT
*&---------------------------------------------------------------------
FORM P2000_INIT.

  SET TITLEBAR  'ZIMR64'.
  W_PAGE_CHK = 'Y'.
  MOVE :    'I'          TO  S_TRQDT-SIGN,
            'BT'         TO  S_TRQDT-OPTION,
            SY-DATUM     TO  S_TRQDT-HIGH.
  CONCATENATE SY-DATUM(6) '01' INTO S_TRQDT-LOW.
  APPEND S_TRQDT.

ENDFORM.                    " P2000_INIT
*&---------------------------------------------------------------------
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------
FORM P3000_LINE_WRITE.

  CLEAR  T001W.
  SELECT SINGLE *
   FROM  T001W
  WHERE  WERKS = IT_TAB-ZFWERKS.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.

  WRITE : / SY-VLINE NO-GAP,(04)IT_TAB-ZFWERKS,
            (20) T001W-NAME1     NO-GAP,
            SY-VLINE NO-GAP,(06) IT_TAB-ZF20FT1  NO-GAP,
            SY-VLINE NO-GAP,(06) IT_TAB-ZF40FT1  NO-GAP,
            SY-VLINE NO-GAP,(06) IT_TAB-ZF20FT2  NO-GAP,
            SY-VLINE NO-GAP,(06) IT_TAB-ZF40FT2  NO-GAP,
            SY-VLINE NO-GAP,(06) IT_TAB-ZF20FT3  NO-GAP,
            SY-VLINE NO-GAP,(06) IT_TAB-ZF40FT3  NO-GAP,
            SY-VLINE NO-GAP,(06) IT_TAB-ZF20FT4  NO-GAP,
            SY-VLINE NO-GAP,(06) IT_TAB-ZF40FT4  NO-GAP,
            SY-VLINE NO-GAP,(06) IT_TAB-ZF20FT5  NO-GAP,
            SY-VLINE NO-GAP,(06) IT_TAB-ZF40FT5  NO-GAP,
            SY-VLINE NO-GAP,(06) IT_TAB-ZFL20FT  NO-GAP,
            SY-VLINE NO-GAP,(06) IT_TAB-ZFL40FT  NO-GAP,
            SY-VLINE.
  HIDE : IT_TAB,W_TABIX.
  WRITE:/ SY-ULINE .

ENDFORM.

*&---------------------------------------------------------------------
*&      Form  P3000_LINE_TOTAL
*&---------------------------------------------------------------------
FORM P3000_LINE_TOTAL.

  WRITE:/ SY-ULINE.
  FORMAT RESET.
  FORMAT COLOR COL_TOTAL INTENSIFIED ON.
  SUM.
  WRITE :/ SY-VLINE NO-GAP,(25) '총         계' NO-GAP CENTERED,
           SY-VLINE NO-GAP,(06) IT_TAB-ZF20FT1  NO-GAP,
           SY-VLINE NO-GAP,(06) IT_TAB-ZF40FT1  NO-GAP,
           SY-VLINE NO-GAP,(06) IT_TAB-ZF20FT2  NO-GAP,
           SY-VLINE NO-GAP,(06) IT_TAB-ZF40FT2  NO-GAP,
           SY-VLINE NO-GAP,(06) IT_TAB-ZF20FT3  NO-GAP,
           SY-VLINE NO-GAP,(06) IT_TAB-ZF40FT3  NO-GAP,
           SY-VLINE NO-GAP,(06) IT_TAB-ZF20FT4  NO-GAP,
           SY-VLINE NO-GAP,(06) IT_TAB-ZF40FT4  NO-GAP,
           SY-VLINE NO-GAP,(06) IT_TAB-ZF20FT5  NO-GAP,
           SY-VLINE NO-GAP,(06) IT_TAB-ZF40FT5  NO-GAP,
           SY-VLINE NO-GAP,(06) IT_TAB-ZFL20FT  NO-GAP,
           SY-VLINE NO-GAP,(06) IT_TAB-ZFL40FT  NO-GAP,
           SY-VLINE.
  WRITE:/ SY-ULINE.

ENDFORM.                    " P3000_LINE_TOTAL
*&---------------------------------------------------------------------
*&      Form  GET_ZTIIMIMG08_SELECT
*&---------------------------------------------------------------------
FORM GET_ZTIIMIMG08_SELECT USING  P_KEY
                                  P_ZFCD
                           CHANGING P_NAME.
  CLEAR ZTIMIMG08.
  SELECT SINGLE *
         FROM ZTIMIMG08
         WHERE ZFCDTY = P_KEY
           AND ZFCD =  P_ZFCD.

  P_NAME = ZTIMIMG08-ZFCDNM.

ENDFORM.                    " GET_ZTIIMIMG08_SELECT
*&---------------------------------------------------------------------
*
*&      Form  RESET_LIST
*&---------------------------------------------------------------------
*
FORM RESET_LIST.

  W_CHK_TITLE = 1.
  MOVE 0 TO SY-LSIND.
  PERFORM   P3000_TITLE_WRITE.     " 해더 출력...
  PERFORM   P3000_DATA_WRITE.

ENDFORM.                    " RESET_LIST
*&---------------------------------------------------------------------
*
*&      Form  P2000_TO_DISP_DETAIL
*&---------------------------------------------------------------------
*
FORM P2000_TO_DISP_DETAIL USING P_ZFWERKS P_ZFTRCK.

  DATA: SELTAB     TYPE TABLE OF RSPARAMS,
         SELTAB_WA  LIKE LINE OF SELTAB.

  IF NOT P_ZFWERKS IS  INITIAL.
      MOVE: 'S_WERKS'  TO SELTAB_WA-SELNAME,
         'S'        TO SELTAB_WA-KIND,      " SELECT-OPTION
         'I'        TO SELTAB_WA-SIGN,
         'EQ'       TO SELTAB_WA-OPTION,
         P_ZFWERKS  TO SELTAB_WA-LOW,
         SPACE      TO SELTAB_WA-HIGH.
     APPEND SELTAB_WA TO SELTAB.
  ENDIF.
*  IF NOT P_ZFTRCK IS INITIAL.
       MOVE: 'S_TRCK'   TO SELTAB_WA-SELNAME,
         'S'        TO SELTAB_WA-KIND,      " SELECT-OPTION
         'I'        TO SELTAB_WA-SIGN,
         'EQ'       TO SELTAB_WA-OPTION,
         P_ZFTRCK   TO SELTAB_WA-LOW,
         SPACE      TO SELTAB_WA-HIGH.
       APPEND SELTAB_WA TO SELTAB.
*  ENDIF.

**> 면허일자.
*  IF NOT S_IDSDT[] IS INITIAL.
*      LOOP AT S_IDSDT.
*         MOVE: 'S_IDSDT'      TO SELTAB_WA-SELNAME,
*            'S'            TO SELTAB_WA-KIND,      " SELECT-OPTION
*            S_IDSDT-SIGN   TO SELTAB_WA-SIGN,
*            S_IDSDT-OPTION TO SELTAB_WA-OPTION,
*            S_IDSDT-LOW    TO SELTAB_WA-LOW,
*            S_IDSDT-HIGH   TO SELTAB_WA-HIGH.
*        APPEND SELTAB_WA TO SELTAB.
*     ENDLOOP.
*  ENDIF.
*> 회사코드.
  IF NOT S_BUKRS[] IS INITIAL.
      LOOP AT S_BUKRS.
         MOVE: 'S_BUKRS'         TO SELTAB_WA-SELNAME,
               'S'               TO SELTAB_WA-KIND,
               S_BUKRS-SIGN      TO SELTAB_WA-SIGN,
               S_BUKRS-OPTION    TO SELTAB_WA-OPTION,
               S_BUKRS-LOW       TO SELTAB_WA-LOW,
               S_BUKRS-HIGH      TO SELTAB_WA-HIGH.
        APPEND SELTAB_WA TO SELTAB.
      ENDLOOP.
  ENDIF.
*>> SELECTION 조건 넘겨주기.

  IF NOT S_WERKS[] IS INITIAL.
      LOOP AT S_WERKS.
         MOVE: 'S_WERKS'      TO SELTAB_WA-SELNAME,
               'S'            TO SELTAB_WA-KIND,      " SELECT-OPTION
               S_WERKS-SIGN   TO SELTAB_WA-SIGN,
               S_WERKS-OPTION TO SELTAB_WA-OPTION,
               S_WERKS-LOW    TO SELTAB_WA-LOW,
               S_WERKS-HIGH   TO SELTAB_WA-HIGH.
         APPEND SELTAB_WA TO SELTAB.
      ENDLOOP.
  ENDIF.
  IF NOT S_TRCK[] IS INITIAL.
      LOOP AT S_TRCK.
         MOVE: 'S_TRCK'      TO SELTAB_WA-SELNAME,
               'S'            TO SELTAB_WA-KIND,      " SELECT-OPTION
               S_TRCK-SIGN   TO SELTAB_WA-SIGN,
               S_TRCK-OPTION TO SELTAB_WA-OPTION,
               S_TRCK-LOW    TO SELTAB_WA-LOW,
               S_TRCK-HIGH   TO SELTAB_WA-HIGH.
         APPEND SELTAB_WA TO SELTAB.
      ENDLOOP.
  ENDIF.
  IF NOT S_BLSDP[] IS INITIAL.
      LOOP AT S_TRCK.
         MOVE: 'S_BLSDP'      TO SELTAB_WA-SELNAME,
               'S'            TO SELTAB_WA-KIND,      " SELECT-OPTION
               S_BLSDP-SIGN   TO SELTAB_WA-SIGN,
               S_BLSDP-OPTION TO SELTAB_WA-OPTION,
               S_BLSDP-LOW    TO SELTAB_WA-LOW,
               S_BLSDP-HIGH   TO SELTAB_WA-HIGH.
         APPEND SELTAB_WA TO SELTAB.
      ENDLOOP.
  ENDIF.
  IF NOT S_TRQDT[] IS INITIAL.
      LOOP AT S_TRQDT.
         MOVE: 'S_TRQDT'      TO SELTAB_WA-SELNAME,
               'S'            TO SELTAB_WA-KIND,      " SELECT-OPTION
               S_TRQDT-SIGN   TO SELTAB_WA-SIGN,
               S_TRQDT-OPTION TO SELTAB_WA-OPTION,
               S_TRQDT-LOW    TO SELTAB_WA-LOW,
               S_TRQDT-HIGH   TO SELTAB_WA-HIGH.
         APPEND SELTAB_WA TO SELTAB.
      ENDLOOP.
  ENDIF.


  SUBMIT ZRIMCTDTLST
          WITH  SELECTION-TABLE SELTAB
          AND RETURN.

ENDFORM.                    " P2000_TO_DISP_DETAIL
