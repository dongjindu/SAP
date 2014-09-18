*&---------------------------------------------------------------------
*& Report  ZRIMMCCLLST
*&---------------------------------------------------------------------
*&  프로그램명 : 통관형태별 물동량 현황.
*&      작성자 : 이채경 INFOLINK Ltd.
*&      작성일 : 2001.09.08
*&---------------------------------------------------------------------
*&   DESC.     :
*&
*&---------------------------------------------------------------------
*& [변경내용]
*&
*&---------------------------------------------------------------------
REPORT  ZRIMMCCLLST  MESSAGE-ID ZIM
                       LINE-SIZE 168
                     NO STANDARD PAGE HEADING.
TABLES: ZTBLINR, ZTBL,ZTIDS,ZTIMIMG08, T001W, ZTIDSUS.
*----------------------------------------------------------------------
*  리스트용 INTERNAL TABLE
*----------------------------------------------------------------------
DATA : BEGIN OF IT_BL OCCURS 0,
       ZFWERKS    LIKE     ZTBL-ZFWERKS,    " PLANT
       ZFBLNO     LIKE     ZTBL-ZFBLNO,     " B/L 관리번?
       ZFRPTTY    LIKE     ZTBL-ZFRPTTY,    " 수입신고형태.
       ZFSHNO     LIKE     ZTBL-ZFSHNO,
       ZFBNDT     LIKE     ZTBL-ZFBNDT,     " 보세운송일.
       ZFTOWT     LIKE     ZTIDS-ZFTOWT.
DATA : END OF IT_BL.

DATA : BEGIN OF IT_IDS OCCURS 0,
       ZFBLNO     LIKE     ZTBL-ZFBLNO,     " B/L 관리번
       ZFCLSEQ    LIKE     ZTIDS-ZFCLSEQ,
       ZFIDSDT    LIKE     ZTIDS-ZFIDSDT,
       ZFBNDT     LIKE     ZTBL-ZFBNDT,     " 보세운송일.
       ZFRPTTY    LIKE     ZTBL-ZFRPTTY,    " 수입신고형태.
       ZFTOWT     LIKE     ZTIDS-ZFTOWT,
       W_COUNT1   TYPE     I,               " Consumption Entry
       W_COUNT2   TYPE     I,               " In Bond Transit
       W_COUNT3   TYPE     I,               " Temporary Import
       W_COUNT4   TYPE     I,               " Foreign Trade Zone
       W_COUNT5   TYPE     I,               " Quota Entry
       W_COUNT6   TYPE     I,               " Warehouse Entry
       W_LCOUNT   TYPE     I,               " 회계단위토탈건수.
       ZFTOWT1    LIKE     ZTIDS-ZFTOWT,
       ZFTOWT2    LIKE     ZTIDS-ZFTOWT,
       ZFTOWT3    LIKE     ZTIDS-ZFTOWT,
       ZFTOWT4    LIKE     ZTIDS-ZFTOWT,
       ZFTOWT5    LIKE     ZTIDS-ZFTOWT,
       ZFTOWT6    LIKE     ZTIDS-ZFTOWT,
       ZFLTOWT    LIKE     ZTIDS-ZFTOWT,
       ZFWERKS    LIKE     ZTBL-ZFWERKS.    " PLANT
DATA : END OF IT_IDS.

DATA : BEGIN OF IT_TAB OCCURS 0,
       ZFWERKS    LIKE     ZTBL-ZFWERKS,    " PLANT
       W_COUNT1   TYPE I,                   " 입항전신고건수
       W_COUNT2   TYPE I,                   " 부두통관건수.
       W_COUNT3   TYPE I,                   " 보세운송건수
       W_COUNT4   TYPE I,                   " 자가입고건수.
       W_COUNT5   TYPE I,                   " 영업창고건수.
       W_COUNT6   TYPE I,                   " IN-BULK 건수.
       W_LCOUNT   TYPE I,                   " 회계단위토탈건수.
       ZFTOWT1    LIKE     ZTIDS-ZFTOWT,
       ZFTOWT2    LIKE     ZTIDS-ZFTOWT,
       ZFTOWT3    LIKE     ZTIDS-ZFTOWT,
       ZFTOWT4    LIKE     ZTIDS-ZFTOWT,
       ZFTOWT5    LIKE     ZTIDS-ZFTOWT,
       ZFTOWT6    LIKE     ZTIDS-ZFTOWT,
       ZFLTOWT    LIKE     ZTIDS-ZFTOWT.
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
        W_LINE        TYPE I,
        W_GUBUN(50),
        W_COUNT       TYPE I,
        W_SUBRC       LIKE SY-SUBRC,
        W_TABIX       LIKE SY-TABIX,
        W_ZFCLSEQ     LIKE ZTIDS-ZFCLSEQ,
        W_LIST_INDEX  LIKE SY-TABIX,
        CURSORFIELD(20).

INCLUDE   ZRIMSORTCOM.    " REPORT Sort
INCLUDE   ZRIMUTIL01.     " Utility function 모?

*----------------------------------------------------------------------
* Selection Screen ?
*----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS: S_BUKRS   FOR ZTBL-BUKRS NO-EXTENSION NO INTERVALS,
                   S_IDSDT   FOR ZTIDS-ZFIDSDT,     " 면허일.
                   S_BNDT    FOR ZTBL-ZFBNDT,       " 보세운송일.
                   S_BLSDP   FOR ZTBL-ZFBLSDP,      " 송부처.
                   S_WERKS   FOR ZTBL-ZFWERKS.      " PLANT,
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
   IF W_ERR_CHK = 'T'.
    MESSAGE S977 WITH '보세운송일과 면허일중 하나는 입력하셔야 합니다.'
.
    EXIT.
   ENDIF.
   IF W_ERR_CHK = 'S'.
      MESSAGE S977 WITH '보세운송일과 면허일중 택일하십시오.'.
      EXIT.
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

*     WHEN 'DISP'.
*        IF W_TABIX IS INITIAL.
*           MESSAGE S962.    EXIT.
*        ENDIF.
*        PERFORM P2000_TO_DISP_DETAIL USING IT_TAB-ZFWERKS.

     WHEN 'DOWN'.          " FILE DOWNLOAD....
          PERFORM P3000_TO_PC_DOWNLOAD.
  ENDCASE.
  CLEAR: IT_TAB, W_TABIX.

*----------------------------------------------------------------------
* At Line Selection.
*----------------------------------------------------------------------
AT LINE-SELECTION.
    IF W_TABIX IS INITIAL.
        MESSAGE S962.
    ELSE.
        GET CURSOR FIELD CURSORFIELD.
        IF SY-SUBRC EQ 0.
            CASE CURSORFIELD.
                WHEN 'IT_TAB-W_COUNT1' OR 'IT_TAB-ZFTOWT1'.
                    IF IT_TAB-W_COUNT1 IS INITIAL.
                        MESSAGE S962.EXIT.
                    ENDIF.
                    PERFORM P2000_TO_DISP_DETAIL USING IT_TAB-ZFWERKS
                                                      'A'.  "입항전 통관

                WHEN 'IT_TAB-W_COUNT2' OR 'IT_TAB-ZFTOWT2'.
                    IF IT_TAB-W_COUNT2 IS INITIAL.
                        MESSAGE S962.EXIT.
                    ENDIF.
                    PERFORM P2000_TO_DISP_DETAIL USING IT_TAB-ZFWERKS
                                                 'D'. "부두(내장) 통관

                WHEN 'IT_TAB-W_COUNT3' OR 'IT_TAB-ZFTOWT3'.
                    IF IT_TAB-W_COUNT3 IS INITIAL.
                        MESSAGE S962.EXIT.
                    ENDIF.
                    PERFORM P2000_TO_DISP_DETAIL USING IT_TAB-ZFWERKS
                                                 'B'. "보세운송 통관

                WHEN 'IT_TAB-W_COUNT4' OR 'IT_TAB-ZFTOWT4'.
                    IF IT_TAB-W_COUNT4 IS INITIAL.
                        MESSAGE S962.EXIT.
                    ENDIF.
                    PERFORM P2000_TO_DISP_DETAIL USING IT_TAB-ZFWERKS
                                                 'N'. "자가장치장 통관

                WHEN 'IT_TAB-W_COUNT5' OR 'IT_TAB-ZFTOWT5'.
                    IF IT_TAB-W_COUNT5 IS INITIAL.
                        MESSAGE S962.EXIT.
                    ENDIF.
                    PERFORM P2000_TO_DISP_DETAIL USING IT_TAB-ZFWERKS
                                                 'W'. "영업장치장 통관

                WHEN 'IT_TAB-W_COUNT6' OR 'IT_TAB-ZFTOWT6'.
                    IF IT_TAB-W_COUNT6 IS INITIAL.
                        MESSAGE S962.EXIT.
                    ENDIF.
                    PERFORM P2000_TO_DISP_DETAIL USING IT_TAB-ZFWERKS
                                                 'I'. "In-Bulk

                WHEN 'IT_TAB-W_LCOUNT' OR 'IT_TAB-ZFLTOWT'.
                    IF IT_TAB-W_LCOUNT IS INITIAL.
                        MESSAGE S962.EXIT.
                    ENDIF.
                    PERFORM P2000_TO_DISP_DETAIL USING IT_TAB-ZFWERKS
                                               SPACE.

                WHEN OTHERS.
                    MESSAGE S962.
            ENDCASE.
        ELSE.
            MESSAGE S962.
        ENDIF.
    ENDIF.

    CLEAR IT_TAB.
*&---------------------------------------------------------------------
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------
FORM P3000_TITLE_WRITE.

  SKIP 1.
  WRITE : /80 '[ 통관형태별 물동량현황 ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  IF NOT S_BNDT[] IS INITIAL.
     WRITE : / 'Date : ', S_BNDT-LOW,'~',S_BNDT-HIGH.
  ENDIF.
  IF NOT S_IDSDT[] IS INITIAL.
     WRITE : / 'Date : ', S_IDSDT-LOW,'~',S_IDSDT-HIGH.
  ENDIF.
  FORMAT RESET.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-ULINE.
  WRITE : / SY-VLINE NO-GAP,(05) '     ' NO-GAP CENTERED
            COLOR COL_NORMAL INTENSIFIED OFF,
            SY-VLINE NO-GAP,(22) '입항전신고' NO-GAP CENTERED,
            SY-VLINE NO-GAP,(22) '부두통관'   NO-GAP CENTERED,
            SY-VLINE NO-GAP,(22) '보세운송' NO-GAP CENTERED,
            SY-VLINE NO-GAP,(22) '자가입고' NO-GAP CENTERED,
            SY-VLINE NO-GAP,(22) '영업창고' NO-GAP CENTERED,
            SY-VLINE NO-GAP,(22) 'in-bulk' NO-GAP CENTERED,
            SY-VLINE NO-GAP,(22) '계' NO-GAP CENTERED,
            SY-VLINE.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  WRITE :/ SY-VLINE COLOR COL_NORMAL INTENSIFIED OFF,7 SY-ULINE.

  FORMAT RESET.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-VLINE NO-GAP,(05) '     ' NO-GAP CENTERED
            COLOR COL_NORMAL INTENSIFIED OFF,
            SY-VLINE NO-GAP,(04) '건수'  NO-GAP CENTERED,
            SY-VLINE NO-GAP,(17) '중량'  NO-GAP CENTERED,
            SY-VLINE NO-GAP,(04) '건수'  NO-GAP CENTERED,
            SY-VLINE NO-GAP,(17) '중량'  NO-GAP CENTERED,
            SY-VLINE NO-GAP,(04) '건수'  NO-GAP CENTERED,
            SY-VLINE NO-GAP,(17) '중량'  NO-GAP CENTERED,
            SY-VLINE NO-GAP,(04) '건수'  NO-GAP CENTERED,
            SY-VLINE NO-GAP,(17) '중량'  NO-GAP CENTERED,
            SY-VLINE NO-GAP,(04) '건수'  NO-GAP CENTERED,
            SY-VLINE NO-GAP,(17) '중량'  NO-GAP CENTERED,
            SY-VLINE NO-GAP,(04) '건수'  NO-GAP CENTERED,
            SY-VLINE NO-GAP,(17) '중량'  NO-GAP CENTERED,
            SY-VLINE NO-GAP,(04) '건수'  NO-GAP CENTERED,
            SY-VLINE NO-GAP,(17) '중량'  NO-GAP CENTERED,
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
  IF  S_BNDT[] IS INITIAL AND S_IDSDT[] IS INITIAL.
      W_ERR_CHK = 'T'.
      EXIT.
  ENDIF.
  IF NOT S_BNDT[] IS INITIAL AND NOT S_IDSDT[] IS INITIAL.
     W_ERR_CHK = 'S'.
     EXIT.
  ENDIF.

**  B/L
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_BL
      FROM ZTBL
     WHERE BUKRS   IN S_BUKRS
       AND ZFBNDT  IN S_BNDT      " 보세운송일.
       AND ZFRPTTY NE SPACE       " 미통관.
       AND ZFBLSDP IN S_BLSDP     " 송부처.
       AND ZFWERKS IN S_WERKS.    " PLANT,

  IF SY-SUBRC <> 0.  W_ERR_CHK = 'Y'.   EXIT.    ENDIF.
*>> 수입면허.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_IDS
           FROM ZTIDS
           FOR ALL ENTRIES IN IT_BL
           WHERE ZFBLNO  = IT_BL-ZFBLNO
             AND ZFIDSDT IN S_IDSDT.

 LOOP AT IT_BL.
*>> 보세운송이 있고 면허일이 없는경우 통관이 안된 경우 APPEND.
    IF NOT S_IDSDT[] IS INITIAL.
       CONTINUE.
    ENDIF.
    IF NOT S_BNDT[] IS INITIAL.
       CLEAR: W_LCOUNT.
       IF  IT_BL-ZFRPTTY = 'B' OR  IT_BL-ZFRPTTY = 'N'.
           READ TABLE IT_IDS WITH KEY ZFBLNO = IT_BL-ZFBLNO.
           IF SY-SUBRC NE 0.
              MOVE-CORRESPONDING IT_BL TO IT_IDS.
              APPEND IT_IDS.
           ENDIF.
       ENDIF.
    ENDIF.
  ENDLOOP.

  LOOP AT IT_IDS.
     W_TABIX = SY-TABIX.
*     SELECT  SUM( ZFTOWT )   AS ZFTOWT        " 총중량.
*               INTO (IT_IDS-ZFTOWT)
*           FROM ZTIDS
*           WHERE ZFBLNO   = IT_IDS-ZFBLNO.

     READ TABLE IT_BL WITH KEY ZFBLNO = IT_IDS-ZFBLNO.
     IF SY-SUBRC EQ 0.
        MOVE:  IT_BL-ZFWERKS  TO IT_IDS-ZFWERKS,
               IT_BL-ZFRPTTY  TO IT_IDS-ZFRPTTY.
     ENDIF.
     CASE IT_IDS-ZFRPTTY.
        WHEN  'A'.	" 입항전 통관.
          ADD 1 TO IT_IDS-W_COUNT1.
          MOVE   IT_IDS-ZFTOWT TO IT_IDS-ZFTOWT1.
        WHEN  'D'.	" 부두(내장) 통관.
          ADD 1 TO IT_IDS-W_COUNT2.
          MOVE   IT_IDS-ZFTOWT TO IT_IDS-ZFTOWT2.
        WHEN  'B'.	" 보세운송 통관.
          ADD 1 TO IT_IDS-W_COUNT3.
          MOVE   IT_IDS-ZFTOWT TO IT_IDS-ZFTOWT3.
        WHEN  'N'.	" 자가장치장 통관.
          ADD 1 TO IT_IDS-W_COUNT4.
          MOVE   IT_IDS-ZFTOWT TO IT_IDS-ZFTOWT4.
        WHEN  'W'.	" 영업장치장 통관.
          ADD 1 TO IT_IDS-W_COUNT5.
          MOVE   IT_IDS-ZFTOWT TO IT_IDS-ZFTOWT5.
        WHEN  'I'.	" In-Bulk(영업장치장)
          ADD 1 TO IT_IDS-W_COUNT6.
          MOVE   IT_IDS-ZFTOWT TO IT_IDS-ZFTOWT6.
        WHEN OTHERS.
     ENDCASE.
     IT_IDS-ZFLTOWT =
        IT_IDS-ZFTOWT1 + IT_IDS-ZFTOWT2 + IT_IDS-ZFTOWT3 +
        IT_IDS-ZFTOWT4 + IT_IDS-ZFTOWT5 + IT_IDS-ZFTOWT6.
     IT_IDS-W_LCOUNT =
        IT_IDS-W_COUNT1 + IT_IDS-W_COUNT2 + IT_IDS-W_COUNT3 +
        IT_IDS-W_COUNT4 + IT_IDS-W_COUNT5 + IT_IDS-W_COUNT6.
     MODIFY IT_IDS INDEX W_TABIX.
     MOVE-CORRESPONDING IT_IDS TO IT_TAB.
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

   SET TITLEBAR  'ZIMR63'.
   SET PF-STATUS 'ZIMR63'.
   LOOP AT IT_TAB.
      W_TABIX = SY-TABIX.
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

  SET TITLEBAR  'ZIMR63'.
  MOVE :    'I'          TO  S_IDSDT-SIGN,
            'BT'         TO  S_IDSDT-OPTION,
            SY-DATUM     TO  S_IDSDT-HIGH.
  CONCATENATE SY-DATUM(6) '01' INTO S_IDSDT-LOW.
  APPEND S_IDSDT.

ENDFORM.                    " P2000_INIT
*&---------------------------------------------------------------------
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------
FORM P3000_LINE_WRITE.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  WRITE : / SY-VLINE NO-GAP,(05) IT_TAB-ZFWERKS  NO-GAP,
            SY-VLINE NO-GAP,(04) IT_TAB-W_COUNT1 NO-GAP,
            SY-VLINE NO-GAP,(17) IT_TAB-ZFTOWT1  UNIT 'TON' NO-GAP,
            SY-VLINE NO-GAP,(04) IT_TAB-W_COUNT2 NO-GAP,
            SY-VLINE NO-GAP,(17) IT_TAB-ZFTOWT2  UNIT 'TON' NO-GAP,
            SY-VLINE NO-GAP,(04) IT_TAB-W_COUNT3 NO-GAP,
            SY-VLINE NO-GAP,(17) IT_TAB-ZFTOWT3  UNIT 'TON' NO-GAP,
            SY-VLINE NO-GAP,(04) IT_TAB-W_COUNT4 NO-GAP,
            SY-VLINE NO-GAP,(17) IT_TAB-ZFTOWT4  UNIT 'TON' NO-GAP,
            SY-VLINE NO-GAP,(04) IT_TAB-W_COUNT5 NO-GAP,
            SY-VLINE NO-GAP,(17) IT_TAB-ZFTOWT5 UNIT 'TON'  NO-GAP,
            SY-VLINE NO-GAP,(04) IT_TAB-W_COUNT6 NO-GAP,
            SY-VLINE NO-GAP,(17) IT_TAB-ZFTOWT6 UNIT 'TON' NO-GAP,
            SY-VLINE NO-GAP,(04) IT_TAB-W_LCOUNT NO-GAP,
            SY-VLINE NO-GAP,(17) IT_TAB-ZFLTOWT UNIT 'TON'  NO-GAP,
            SY-VLINE.
  HIDE : IT_TAB,W_TABIX.
  WRITE:/ SY-ULINE .

ENDFORM.

*&---------------------------------------------------------------------
*&      Form  P3000_LINE_TOTAL
*&---------------------------------------------------------------------
FORM P3000_LINE_TOTAL.

   FORMAT RESET.
   FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
   SUM.
   WRITE :/ SY-VLINE NO-GAP,(05) '계' NO-GAP CENTERED,
            SY-VLINE NO-GAP,(04) IT_TAB-W_COUNT1 NO-GAP ,
            SY-VLINE NO-GAP,(17) IT_TAB-ZFTOWT1  UNIT 'TON' NO-GAP ,
            SY-VLINE NO-GAP,(04) IT_TAB-W_COUNT2 UNIT 'TON' NO-GAP ,
            SY-VLINE NO-GAP,(17) IT_TAB-ZFTOWT2 UNIT 'TON' NO-GAP ,
            SY-VLINE NO-GAP,(04) IT_TAB-W_COUNT3 NO-GAP ,
            SY-VLINE NO-GAP,(17) IT_TAB-ZFTOWT3 UNIT 'TON'   NO-GAP ,
            SY-VLINE NO-GAP,(04) IT_TAB-W_COUNT4 NO-GAP,
            SY-VLINE NO-GAP,(17) IT_TAB-ZFTOWT4  UNIT 'TON' NO-GAP,
            SY-VLINE NO-GAP,(04) IT_TAB-W_COUNT5 UNIT 'TON' NO-GAP,
            SY-VLINE NO-GAP,(17) IT_TAB-ZFTOWT5  UNIT 'TON' NO-GAP,
            SY-VLINE NO-GAP,(04) IT_TAB-W_COUNT6 UNIT 'TON' NO-GAP,
            SY-VLINE NO-GAP,(17) IT_TAB-ZFTOWT6  UNIT 'TON' NO-GAP,
            SY-VLINE NO-GAP,(04) IT_TAB-W_LCOUNT NO-GAP,
            SY-VLINE NO-GAP,(17) IT_TAB-ZFLTOWT  UNIT 'TON' NO-GAP,
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
FORM P2000_TO_DISP_DETAIL USING P_ZFWERKS
                                P_TYPE.

   DATA: SELTAB     TYPE TABLE OF RSPARAMS,
         SELTAB_WA  LIKE LINE OF SELTAB.

   MOVE: 'S_WERKS'  TO SELTAB_WA-SELNAME,
         'S'        TO SELTAB_WA-KIND,      " SELECT-OPTION
         'I'        TO SELTAB_WA-SIGN,
         'EQ'       TO SELTAB_WA-OPTION,
         P_ZFWERKS  TO SELTAB_WA-LOW,
         SPACE      TO SELTAB_WA-HIGH.
   APPEND SELTAB_WA TO SELTAB.

*> 회사코드.
   LOOP AT S_BUKRS.
      MOVE: 'S_BUKRS'      TO SELTAB_WA-SELNAME,
            'S'            TO SELTAB_WA-KIND,      " SELECT-OPTION
            S_BUKRS-SIGN   TO SELTAB_WA-SIGN,
            S_BUKRS-OPTION TO SELTAB_WA-OPTION,
            S_BUKRS-LOW    TO SELTAB_WA-LOW,
            S_BUKRS-HIGH   TO SELTAB_WA-HIGH.
      APPEND SELTAB_WA TO SELTAB.
   ENDLOOP.

*> 면허일자.
   LOOP AT S_IDSDT.
      MOVE: 'S_IDSDT'      TO SELTAB_WA-SELNAME,
            'S'            TO SELTAB_WA-KIND,      " SELECT-OPTION
            S_IDSDT-SIGN   TO SELTAB_WA-SIGN,
            S_IDSDT-OPTION TO SELTAB_WA-OPTION,
            S_IDSDT-LOW    TO SELTAB_WA-LOW,
            S_IDSDT-HIGH   TO SELTAB_WA-HIGH.
      APPEND SELTAB_WA TO SELTAB.
   ENDLOOP.
*> 보세운송일.
   LOOP AT S_BNDT.
      MOVE: 'S_BNDT'         TO SELTAB_WA-SELNAME,
            'S'              TO SELTAB_WA-KIND,      " SELECT-OPTION
            S_BNDT-SIGN      TO SELTAB_WA-SIGN,
            S_BNDT-OPTION    TO SELTAB_WA-OPTION,
            S_BNDT-LOW       TO SELTAB_WA-LOW,
            S_BNDT-HIGH      TO SELTAB_WA-HIGH.
      APPEND SELTAB_WA TO SELTAB.
   ENDLOOP.
*> 플랜트 선택값.
   MOVE: 'S_WERKS'  TO SELTAB_WA-SELNAME,
         'S'        TO SELTAB_WA-KIND,      " SELECT-OPTION
         'I'        TO SELTAB_WA-SIGN,
         'EQ'       TO SELTAB_WA-OPTION,
         P_ZFWERKS  TO SELTAB_WA-LOW,
         SPACE      TO SELTAB_WA-HIGH.
   APPEND SELTAB_WA TO SELTAB.
**>> 송부처 선택값.
* IF NOT P_ZFBLSDP IS INITIAL.
*      MOVE: 'S_BLSDP'  TO SELTAB_WA-SELNAME,
*            'S'        TO SELTAB_WA-KIND,      " SELECT-OPTION
*            'I'        TO SELTAB_WA-SIGN,
*            'EQ'       TO SELTAB_WA-OPTION,
*            P_ZFBLSDP  TO SELTAB_WA-LOW,
*            SPACE      TO SELTAB_WA-HIGH.
*      APPEND SELTAB_WA TO SELTAB.
*   ENDIF.
*>> SELECTION 조건 넘겨주기.
*>> 플랜트.
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
*>> 송부처.
  IF NOT S_BLSDP[] IS INITIAL.
      LOOP AT S_BLSDP.
         MOVE: 'S_BLSDP'      TO SELTAB_WA-SELNAME,
               'S'            TO SELTAB_WA-KIND,      " SELECT-OPTION
               S_BLSDP-SIGN   TO SELTAB_WA-SIGN,
               S_BLSDP-OPTION TO SELTAB_WA-OPTION,
               S_BLSDP-LOW    TO SELTAB_WA-LOW,
               S_BLSDP-HIGH   TO SELTAB_WA-HIGH.
         APPEND SELTAB_WA TO SELTAB.
      ENDLOOP.
  ENDIF.

*>> 수입신고형태 (2001.12.03 김영광 추가)
  IF P_TYPE NE SPACE.
      MOVE: 'S_RPTTY'      TO SELTAB_WA-SELNAME,
            'S'            TO SELTAB_WA-KIND,      " SELECT-OPTION
            'I'            TO SELTAB_WA-SIGN,
            'EQ'           TO SELTAB_WA-OPTION,
            P_TYPE         TO SELTAB_WA-LOW,
            SPACE          TO SELTAB_WA-HIGH.
      APPEND SELTAB_WA TO SELTAB.
  ENDIF.


  SUBMIT ZRIMMDTCCLLST
          WITH  SELECTION-TABLE SELTAB
          AND RETURN.

ENDFORM.                    " P2000_TO_DISP_DETAIL
