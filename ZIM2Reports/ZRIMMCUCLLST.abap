*&---------------------------------------------------------------------
*& Report  ZRIMMCUCLLST
*&---------------------------------------------------------------------
*&  프로그램명 : 수입물동량 현황.
*&      작성자 : 이채경 INFOLINK Ltd.
*&      작성일 : 2001.09.03
*&---------------------------------------------------------------------
*&   DESC.     :
*&
*&---------------------------------------------------------------------
*& [변경내용]
*&
*&---------------------------------------------------------------------
REPORT  ZRIMMCUCLLST  MESSAGE-ID ZIM
                       LINE-SIZE 121
                     NO STANDARD PAGE HEADING.
TABLES: ZTBLINR, ZTBL,ZTIDS,ZTIMIMG08, T001W, ZTIDSUS, ZTIMIMG00.
*----------------------------------------------------------------------
*  리스트용 INTERNAL TABLE
*----------------------------------------------------------------------
DATA : BEGIN OF IT_BL OCCURS 0,
       ZFWERKS    LIKE     ZTBL-ZFWERKS,    " PLANT
       ZFBLNO     LIKE     ZTBL-ZFBLNO,     " B/L 관리번?
       ZFAPRTC    LIKE     ZTBL-ZFAPRTC,    " PORT.
       ZFPOYN     LIKE     ZTBL-ZFPOYN,     " 유환여부.
       ZFBLSDP    LIKE     ZTBL-ZFBLSDP,    " 송부처.
       ZFRPTTY    LIKE     ZTBL-ZFRPTTY,    " 수입신고형태.
       ZFSHNO     LIKE     ZTBL-ZFSHNO,
       ZFBNDT     LIKE     ZTBL-ZFBNDT,     " 보세운송일.
       ZF20FT     LIKE     ZTBL-ZF20FT,
       ZF40FT     LIKE     ZTBL-ZF40FT,
       ZF20FHQ    LIKE     ZTBL-ZF20FHQ,
       ZF40FHQ    LIKE     ZTBL-ZF40FHQ.
DATA : END OF IT_BL.

DATA : BEGIN OF IT_IDS OCCURS 0,
       ZFBLNO     LIKE     ZTBL-ZFBLNO,
       ZFCLSEQ    LIKE     ZTIDSUS-ZFCLSEQ,
       W_COUNT    TYPE     I,
       ZFHBLNO    LIKE     ZTBL-ZFHBLNO,
       ZFBNDT     LIKE     ZTBL-ZFBNDT,
       ZFAPRTC    LIKE     ZTBL-ZFAPRTC,
       ZF20FT     LIKE     ZTBL-ZF20FT,
       ZF40FT     LIKE     ZTBL-ZF40FT,
       ZF20FHQ    LIKE     ZTBL-ZF20FHQ,
       ZF40FHQ    LIKE     ZTBL-ZF40FHQ,
       ZFINRC     LIKE     ZTIDSUS-ZFINRC,
       ZFPOYN     LIKE     ZTBL-ZFPOYN,
       ZFBLSDP    LIKE     ZTBL-ZFBLSDP,
       ZFRPTTY    LIKE     ZTBL-ZFRPTTY,
       ZFIVAMK    LIKE     ZTIDSUS-ZFIVAMK,
       ZFKRW      LIKE     ZTIDSUS-ZFKRW,
       ZFIVAMT    LIKE     ZTIDSUS-ZFIVAMT,
       ZFIVAMC    LIKE     ZTIDSUS-ZFIVAMC,
       ZFDUTY     LIKE     ZTIDSUS-ZFDUTY,
       ZFOTFE     LIKE     ZTIDSUS-ZFOTFE,
       ZFTOCUR    LIKE     ZTIDSUS-ZFTOCUR,
       ZFTOFEE    LIKE     ZTIDSUS-ZFTOFEE,
       ZFWERKS    LIKE     ZTBL-ZFWERKS.
DATA :   END OF IT_IDS.

DATA : BEGIN OF IT_TAB OCCURS 0,
       ZFWERKS    LIKE     ZTBL-ZFWERKS,
       W_COUNT    TYPE     I,
       ZFAPRTC    LIKE     ZTBL-ZFAPRTC,
       ZFINRC     LIKE     ZTIDSUS-ZFINRC,
       ZFRPTTY    LIKE     ZTBL-ZFRPTTY,
       ZFBLSDP    LIKE     ZTBL-ZFBLSDP,
       ZF20FT     TYPE     I,
       ZF40FT     TYPE     I,
       ZF20FHQ    TYPE     I,
       ZF40FHQ    TYPE     I,
       ZFIVAMT    LIKE     ZTIDSUS-ZFIVAMT,
       ZFIVAMC    LIKE     ZTIDSUS-ZFIVAMC,
       ZFIVAMK    LIKE     ZTIDSUS-ZFIVAMK,
       ZFKRW      LIKE     ZTIDSUS-ZFKRW,
       ZFDUTY     LIKE     ZTIDSUS-ZFDUTY,
       ZFOTFE     LIKE     ZTIDSUS-ZFOTFE,
       ZFTOCUR    LIKE     ZTIDSUS-ZFTOCUR,
       ZFTOFEE    LIKE     ZTIDSUS-ZFTOFEE.
DATA : END OF IT_TAB.

DATA :  W_ERR_CHK     TYPE C,
        W_LCOUNT      TYPE I,
        W_FIELD_NM    TYPE C,
        W_PAGE        TYPE I,
        W_TITLE(50),
        W_TITLE1(50),
        W_DOM_TEX1     LIKE DD07T-DDTEXT,
        W_FNAME        LIKE ZTIMIMG08-ZFCDNM,
        P_BUKRS        LIKE ZTBL-BUKRS,
        W_CHK_TITLE,
        W_LINE        TYPE I,
        W_GUBUN(50),
        W_COUNT       TYPE I,
        W_SUBRC       LIKE SY-SUBRC,
        W_TABIX       LIKE SY-TABIX,
        W_ZFCLSEQ     LIKE ZTIDS-ZFCLSEQ,
        W_LIST_INDEX  LIKE SY-TABIX.
*>> SUTOTAL.
 DATA: S_COUNT     TYPE I,
       S_IVAMK     LIKE  ZTIDSUS-ZFIVAMK,
       S_DUTY      LIKE  ZTIDSUS-ZFDUTY,
       S_OTFE      LIKE  ZTIDSUS-ZFOTFE,
       S_TOTFEE    LIKE  ZTIDSUS-ZFTOFEE,
       S_ZF20FT    TYPE I,
       S_ZF40FT    TYPE I,
       S_ZF20FHQ   TYPE I,
       S_ZF40FHQ   TYPE I.

INCLUDE   ZRIMSORTCOM.    " REPORT Sort
INCLUDE   ZRIMUTIL01.     " Utility function 모?

*----------------------------------------------------------------------
* Selection Screen ?
*----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS: S_BUKRS   FOR ZTBL-BUKRS NO-EXTENSION NO INTERVALS,
                   S_IDSDT   FOR ZTIDSUS-ZFEDT,
                   S_BNDT    FOR ZTBL-ZFBNDT,
                   S_APRT    FOR ZTBL-ZFAPRTC,
                   S_RPTTY   FOR ZTBL-ZFRPTTY,
                   S_BLSDP   FOR ZTBL-ZFBLSDP,
                   S_INRC    FOR ZTIDSUS-ZFINRC,
                   S_WERKS   FOR ZTBL-ZFWERKS,
                   S_POYN    FOR ZTBL-ZFPOTY.
 SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.

* SORT 구분
  SELECTION-SCREEN : BEGIN OF LINE,POSITION 1.
     SELECTION-SCREEN : COMMENT 4(18) TEXT-021, POSITION 1.
     PARAMETERS : P_POT  RADIOBUTTON GROUP RDG.     " PORT
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN : BEGIN OF LINE,POSITION 1.
     SELECTION-SCREEN : COMMENT 4(18) TEXT-022, POSITION 1.
     PARAMETERS : P_CUT      RADIOBUTTON GROUP RDG.     " 세관별.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN : BEGIN OF LINE,  POSITION 1.
     SELECTION-SCREEN : COMMENT 4(18) TEXT-023, POSITION 1.
     PARAMETERS : P_YN   RADIOBUTTON GROUP RDG."무환 SHIPBACK 송부처
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN : BEGIN OF LINE,POSITION 1.
     SELECTION-SCREEN : COMMENT 4(18) TEXT-024, POSITION 1.
     PARAMETERS : P_TY      RADIOBUTTON GROUP RDG.     " 신고형태별.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B2.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_BLSDP-LOW.
   PERFORM   P1000_BL_SDP_HELP(ZRIMBWGILST)  USING  S_BLSDP-LOW.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_BLSDP-HIGH.
   PERFORM   P1000_BL_SDP_HELP(ZRIMBWGILST)  USING  S_BLSDP-HIGH.

INITIALIZATION.                          " 초기값 SETTING
   PERFORM   P2000_INIT.
   PERFORM   P1000_SET_BUKRS.

*title Text Write
TOP-OF-PAGE.
 IF SY-LANGU EQ '3'.
    PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...
 ELSE.
    PERFORM   P3000_TITLE_WRITE_EN.
 ENDIF.

*----------------------------------------------------------------------
* START OF SELECTION ?
*----------------------------------------------------------------------
START-OF-SELECTION.
* B/L 테이블 SELECT
   PERFORM   P1000_READ_DATA  USING   W_ERR_CHK.
   IF W_ERR_CHK = 'Y'.
      MESSAGE S738.  EXIT.
   ENDIF.
   IF W_ERR_CHK = 'T'.
    MESSAGE S977 WITH 'Input Entry date or Bonded transport date'.
    EXIT.
   ENDIF.
   IF W_ERR_CHK = 'S'.
      MESSAGE S977 WITH 'Select Entry date or Bonded transport date'.
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
     WHEN 'DISP'.
        IF W_TABIX IS INITIAL.
           MESSAGE S962.    EXIT.
        ENDIF.
        PERFORM P2000_TO_DISP_DETAIL USING   IT_TAB-ZFWERKS
                                             IT_TAB-ZFAPRTC
                                             IT_TAB-ZFINRC
                                             IT_TAB-ZFRPTTY
                                             IT_TAB-ZFBLSDP
                                             P_YN.

     WHEN 'DOWN'.          " FILE DOWNLOAD....
          PERFORM P3000_TO_PC_DOWNLOAD.
  ENDCASE.
  CLEAR: IT_TAB, W_TABIX.

*&---------------------------------------------------------------------
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------
FORM P3000_TITLE_WRITE.

  SKIP 1.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  IF P_POT = 'X'.
     SELECT SINGLE PORTT INTO W_FNAME
            FROM ZTIEPORT
            WHERE LAND1  EQ  'KR'
            AND   PORT   EQ  IT_TAB-ZFAPRTC.

     MOVE '[Port별 물량현황]' TO W_TITLE.
     CONCATENATE 'Port:' IT_TAB-ZFAPRTC '-' W_FNAME
                                       INTO W_GUBUN.
  ENDIF.
  IF P_CUT = 'X'.
     MOVE '[세관별 물량현황]' TO W_TITLE.
     PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDCOTM' IT_TAB-ZFINRC
                               CHANGING   W_DOM_TEX1.
     CONCATENATE '세관:' IT_TAB-ZFINRC '-'  W_DOM_TEX1
                                     INTO W_GUBUN.
  ENDIF.
  IF P_YN = 'X'.
     PERFORM  GET_ZTIIMIMG08_SELECT USING '012' IT_TAB-ZFBLSDP
                                  CHANGING   W_FNAME.
     MOVE '[무환 Ship-Back별 물량현황]' TO W_TITLE.
     CONCATENATE '송부처:' IT_TAB-ZFBLSDP '-' W_FNAME
                                           INTO W_GUBUN.
  ENDIF.
  IF P_TY = 'X'.
     PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDRPTTY'
                                          IT_TAB-ZFRPTTY
                                   CHANGING   W_DOM_TEX1.
     MOVE '[신고형태별 물량현황]' TO W_TITLE.
     CONCATENATE '신고형태:' IT_TAB-ZFRPTTY '-'  W_DOM_TEX1
                                            INTO W_GUBUN.
  ENDIF.
  IF W_CHK_TITLE = 1.
     WRITE : /65 W_TITLE.
     WRITE : /126 'Date : ', SY-DATUM.
     W_CHK_TITLE = 0.
  ENDIF.
  WRITE : W_GUBUN.
  WRITE : / SY-ULINE.
  FORMAT RESET.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-VLINE NO-GAP,(05) 'Plant' NO-GAP CENTERED,
            SY-VLINE NO-GAP,(05) '건수'  NO-GAP CENTERED,
            SY-VLINE NO-GAP,(17) '중량(M/T)' NO-GAP CENTERED,
            SY-VLINE NO-GAP,(19) 'CIF$' NO-GAP CENTERED,
            SY-VLINE NO-GAP,(19) '감정가격' NO-GAP CENTERED,
            SY-VLINE NO-GAP,(19) '관세외' NO-GAP CENTERED,
            SY-VLINE NO-GAP,(19) '부가세' NO-GAP CENTERED,
            SY-VLINE NO-GAP,(19) '납부세액계' NO-GAP CENTERED,
            SY-VLINE NO-GAP,(06) '20Cont' NO-GAP CENTERED,
            SY-VLINE NO-GAP,(06) '40Cont' NO-GAP CENTERED,
            SY-VLINE NO-GAP,(06) '20HQ'   NO-GAP CENTERED,
            SY-VLINE NO-GAP,(06) '40HQ'   NO-GAP CENTERED,
            SY-VLINE.
  WRITE : / SY-ULINE.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------
*&      Form  P1000_GET_IT_TAB
*&---------------------------------------------------------------------
FORM P1000_READ_DATA  USING   W_ERR_CHK.

  W_ERR_CHK = 'N'.
  RANGES: R_ZFPOYN FOR ZTBL-ZFPOYN OCCURS 5.

  MOVE :    'I'       TO  R_ZFPOYN-SIGN,
            'EQ'      TO  R_ZFPOYN-OPTION,
            'N'       TO  R_ZFPOYN-LOW,
            SPACE     TO  R_ZFPOYN-HIGH.
  APPEND  R_ZFPOYN.
  MOVE :    'I'       TO  R_ZFPOYN-SIGN,
            'EQ'      TO  R_ZFPOYN-OPTION,
            'M'       TO  R_ZFPOYN-LOW,
            SPACE     TO  R_ZFPOYN-HIGH.
  APPEND  R_ZFPOYN.

  IF P_YN NE 'X'.  " 무환 BACK.
     MOVE :    'I'       TO  R_ZFPOYN-SIGN,
               'EQ'      TO  R_ZFPOYN-OPTION,
               'Y'       TO  R_ZFPOYN-LOW,
               SPACE     TO  R_ZFPOYN-HIGH.
     APPEND  R_ZFPOYN.
  ENDIF.

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
       AND ZFAPRTC IN S_APRT      " 도착항.
       AND ZFRPTTY IN S_RPTTY     " 수입신고형태.
       AND ZFRPTTY NE SPACE       " 미통관.
       AND ZFBLSDP IN S_BLSDP     " 송부처.
       AND ZFWERKS IN S_WERKS     " PLANT,
       AND ZFPOTY  IN S_POYN      " 무환형태.
       AND ZFPOYN  IN R_ZFPOYN.   " 유무환구분.
  IF SY-SUBRC <> 0.  W_ERR_CHK = 'Y'. EXIT.  ENDIF.
*>> 수입면허.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_IDS
           FROM ZTIDSUS
           FOR ALL ENTRIES IN IT_BL
           WHERE ZFBLNO  = IT_BL-ZFBLNO
             AND ZFINRC  IN S_INRC   "세관.
             AND ZFEDT   IN S_IDSDT.

  LOOP AT IT_IDS.
    W_TABIX = SY-TABIX.

     READ TABLE IT_BL WITH KEY ZFBLNO = IT_IDS-ZFBLNO.
     IF SY-SUBRC EQ 0.
        MOVE-CORRESPONDING  IT_BL TO IT_IDS.
        MOVE  IT_BL-ZFRPTTY  TO IT_IDS-ZFRPTTY.
        MOVE  IT_BL-ZFWERKS  TO IT_IDS-ZFWERKS.
     ENDIF.
     MODIFY IT_IDS INDEX W_TABIX.
  ENDLOOP.

  LOOP AT IT_BL.
*>> 보세운송이 있고 면허일이 없는경우 통관이 안된 경우 APPEND.
    IF NOT S_IDSDT[] IS INITIAL.
       CONTINUE.
    ENDIF.
    IF NOT S_BNDT[] IS INITIAL.
       CLEAR: W_LCOUNT.
       IF  IT_BL-ZFRPTTY = 'B' OR  IT_BL-ZFRPTTY = 'W'.
           READ TABLE IT_IDS WITH KEY ZFBLNO = IT_BL-ZFBLNO.
           IF SY-SUBRC NE 0.
              MOVE-CORRESPONDING IT_BL TO IT_IDS.
              APPEND IT_IDS.
              W_LCOUNT = W_LCOUNT + 1.
           ENDIF.
       ENDIF.
    ENDIF.
  ENDLOOP.
*>> C0LLECT
  LOOP AT IT_IDS.
     MOVE-CORRESPONDING IT_IDS TO IT_TAB.

     IF P_POT NE 'X'. " 도착항별.
        CLEAR: IT_TAB-ZFAPRTC.
     ENDIF.
     IF P_CUT NE 'X'. " 세관별.
        CLEAR: IT_TAB-ZFINRC.
     ENDIF.
     IF P_YN NE 'X'.  " 무환
        CLEAR IT_TAB-ZFBLSDP.    " 송부처..
     ENDIF.
     IF P_TY NE 'X'.  " 신고형태.
        CLEAR IT_TAB-ZFRPTTY.
     ENDIF.
     MOVE 1 TO IT_TAB-W_COUNT.
     COLLECT IT_TAB.
   ENDLOOP.

   DESCRIBE TABLE IT_TAB LINES W_LINE.
   IF W_LINE = 0.
      MESSAGE S738.  EXIT.
   ENDIF.

ENDFORM.                    " P1000_READ_DATA
*&---------------------------------------------------------------------
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------
FORM P3000_DATA_WRITE .

   IF P_POT = 'X'. " 도착항별.
     SORT IT_TAB BY ZFAPRTC.
     MOVE 'The Quantity of material list by port' TO W_TITLE1.
   ENDIF.
   IF P_CUT = 'X'.       " 세관별.
     SORT IT_TAB BY ZFINRC.
     MOVE 'The Quantity of material list by customs' TO W_TITLE1.
   ENDIF.
   IF P_YN = 'X'.  " 무환.
      SORT IT_TAB BY  ZFBLSDP.    " 송.
      MOVE 'The Quantity of material list by Monetary Type' TO W_TITLE1.
   ENDIF.
   IF P_TY = 'X'.
     SORT IT_TAB BY ZFRPTTY.
     MOVE 'The Quantity of material list by Import Type' TO W_TITLE1.
   ENDIF.
   SET TITLEBAR  'ZIMR62' WITH W_TITLE1.
   SET PF-STATUS 'ZIMR62'.
   LOOP AT IT_TAB.
      W_TABIX = SY-TABIX.
      ON CHANGE OF IT_TAB-ZFRPTTY OR
                   IT_TAB-ZFBLSDP OR
                   IT_TAB-ZFINRC  OR
                   IT_TAB-ZFAPRTC.
         IF SY-TABIX NE 1.
            PERFORM   P3000_SUB_TOTOL_WRITE.
            NEW-PAGE.
         ENDIF.
      ENDON.
       PERFORM   P3000_LINE_WRITE.
      AT LAST.
         PERFORM   P3000_SUB_TOTOL_WRITE.
         PERFORM P3000_LINE_TOTAL.
      ENDAT.

   ENDLOOP.
   CLEAR: IT_TAB, W_TABIX.

ENDFORM.                    " P3000_DATA_WRITE
*&---------------------------------------------------------------------
*&      Form  P2000_INIT
*&---------------------------------------------------------------------
FORM P2000_INIT.

  P_POT = 'X'.                    " PORT 별.
  SET TITLEBAR  'ZIMR62' WITH 'The Quantity of Material List'.
  W_CHK_TITLE = 1.                " TITLE CHECK.

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
            SY-VLINE NO-GAP,(05) IT_TAB-W_COUNT  NO-GAP,
            SY-VLINE NO-GAP,(19) IT_TAB-ZFIVAMK
                                 CURRENCY IT_TAB-ZFKRW    NO-GAP,
            SY-VLINE NO-GAP,(19) IT_TAB-ZFDUTY
                                 CURRENCY IT_TAB-ZFTOCUR  NO-GAP,
            SY-VLINE NO-GAP,(19) IT_TAB-ZFOTFE
                                 CURRENCY IT_TAB-ZFTOCUR  NO-GAP,
            SY-VLINE NO-GAP,(19) IT_TAB-ZFTOFEE
                                 CURRENCY IT_TAB-ZFTOCUR  NO-GAP,
            SY-VLINE NO-GAP, (06) IT_TAB-ZF20FT NO-GAP,  " 20 CONT Qty.
            SY-VLINE NO-GAP, (06) IT_TAB-ZF40FT NO-GAP,  " 40 CONT Qty.
            SY-VLINE NO-GAP, (06) IT_TAB-ZF20FHQ NO-GAP, " 20 HQ Qty.
            SY-VLINE NO-GAP, (06) IT_TAB-ZF40FHQ NO-GAP, " 40 HQ Qty.
            SY-VLINE.
  HIDE : IT_TAB, W_TABIX.
  WRITE:/ SY-ULINE.

  ADD: IT_TAB-W_COUNT  TO S_COUNT,
       IT_TAB-ZFIVAMK  TO S_IVAMK,
       IT_TAB-ZFDUTY   TO S_DUTY,
       IT_TAB-ZFOTFE   TO S_OTFE,
       IT_TAB-ZFTOFEE  TO S_TOTFEE,
       IT_TAB-ZF20FT   TO S_ZF20FT,
       IT_TAB-ZF40FT   TO S_ZF40FT,
       IT_TAB-ZF20FHQ  TO S_ZF20FHQ,
       IT_TAB-ZF40FHQ  TO S_ZF40FHQ.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_TOTAL
*&---------------------------------------------------------------------*
FORM P3000_LINE_TOTAL.

   SKIP 1.
   FORMAT RESET.
   FORMAT COLOR COL_TOTAL INTENSIFIED ON.
   WRITE:/ SY-ULINE.
   SUM.
   WRITE : / SY-VLINE NO-GAP,
        (05)'Total' NO-GAP,                       SY-VLINE NO-GAP,
        (05)IT_TAB-W_COUNT NO-GAP,                SY-VLINE NO-GAP,
        (19)IT_TAB-ZFIVAMK CURRENCY 'USD' NO-GAP, SY-VLINE NO-GAP,
        (19)IT_TAB-ZFDUTY  CURRENCY 'USD' NO-GAP, SY-VLINE NO-GAP,
        (19)IT_TAB-ZFOTFE  CURRENCY 'USD' NO-GAP, SY-VLINE NO-GAP,
        (19)IT_TAB-ZFTOFEE CURRENCY 'USD' NO-GAP, SY-VLINE NO-GAP,
        (06)IT_TAB-ZF20FT  NO-GAP,                SY-VLINE NO-GAP,
        (06)IT_TAB-ZF40FT  NO-GAP,                SY-VLINE NO-GAP,
        (06)IT_TAB-ZF20FHQ NO-GAP,                SY-VLINE NO-GAP,
        (06)IT_TAB-ZF40FHQ NO-GAP,                SY-VLINE.

  WRITE:/ SY-ULINE.
  FORMAT COLOR OFF.

ENDFORM.                    " P3000_LINE_TOTAL
*&---------------------------------------------------------------------*
*&      Form  P3000_SUB_TOTOL_WRITE
*&---------------------------------------------------------------------*
FORM P3000_SUB_TOTOL_WRITE.

  FORMAT RESET.
  FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
  WRITE : / SY-VLINE NO-GAP,(05)'Sum'            NO-GAP,
            SY-VLINE NO-GAP,(05) S_COUNT         NO-GAP,
            SY-VLINE NO-GAP,(19) S_IVAMK
                                 CURRENCY 'USD'  NO-GAP,
            SY-VLINE NO-GAP,(19) S_DUTY
                                 CURRENCY 'USD'  NO-GAP,
            SY-VLINE NO-GAP,(19) S_OTFE
                                 CURRENCY 'USD'  NO-GAP,
            SY-VLINE NO-GAP,(19) S_TOTFEE
                                 CURRENCY 'USD' NO-GAP,
            SY-VLINE NO-GAP,(06) S_ZF20FT NO-GAP,
            SY-VLINE NO-GAP,(06) S_ZF40FT NO-GAP,
            SY-VLINE NO-GAP,(06) S_ZF20FHQ NO-GAP,
            SY-VLINE NO-GAP,(06) S_ZF40FHQ NO-GAP,
            SY-VLINE.
  WRITE:/ SY-ULINE.
  CLEAR: S_COUNT,S_IVAMK,S_DUTY, S_OTFE, S_TOTFEE,S_ZF20FT,S_ZF40FT.

 ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_ZTIIMIMG08_SELECT
*&---------------------------------------------------------------------*
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
*&---------------------------------------------------------------------*
*&      Form  RESET_LIST
*&---------------------------------------------------------------------*
FORM RESET_LIST.

  W_CHK_TITLE = 1.
  MOVE 0 TO SY-LSIND.
  PERFORM   P3000_TITLE_WRITE.     " 해더 출력...
  PERFORM   P3000_DATA_WRITE.

ENDFORM.                    " RESET_LIST
*&---------------------------------------------------------------------*
*&      Form  P2000_TO_DISP_DETAIL
*&---------------------------------------------------------------------*
FORM P2000_TO_DISP_DETAIL USING P_ZFWERKS  P_ZFAPRTC
                          P_ZFINRC P_ZFRPTTY P_ZFBLSDP P_YN.

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
      MOVE: 'S_BUKRS'         TO SELTAB_WA-SELNAME,
            'S'               TO SELTAB_WA-KIND,      " SELECT-OPTION
            S_BUKRS-SIGN      TO SELTAB_WA-SIGN,
            S_BUKRS-OPTION    TO SELTAB_WA-OPTION,
            S_BUKRS-LOW       TO SELTAB_WA-LOW,
            S_BUKRS-HIGH      TO SELTAB_WA-HIGH.
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

   MOVE: 'S_WERKS'  TO SELTAB_WA-SELNAME,
         'S'        TO SELTAB_WA-KIND,      " SELECT-OPTION
         'I'        TO SELTAB_WA-SIGN,
         'EQ'       TO SELTAB_WA-OPTION,
         P_ZFWERKS  TO SELTAB_WA-LOW,
         SPACE      TO SELTAB_WA-HIGH.
   APPEND SELTAB_WA TO SELTAB.

   IF NOT P_ZFAPRTC IS INITIAL.
      MOVE: 'S_APRT'   TO SELTAB_WA-SELNAME,
            'S'        TO SELTAB_WA-KIND,      " SELECT-OPTION
            'I'        TO SELTAB_WA-SIGN,
            'EQ'       TO SELTAB_WA-OPTION,
            P_ZFAPRTC  TO SELTAB_WA-LOW,
            SPACE      TO SELTAB_WA-HIGH.
      APPEND SELTAB_WA TO SELTAB.
   ENDIF.

   IF NOT P_ZFINRC IS INITIAL.
      MOVE: 'S_INRC'   TO SELTAB_WA-SELNAME,
            'S'        TO SELTAB_WA-KIND,      " SELECT-OPTION
            'I'        TO SELTAB_WA-SIGN,
            'EQ'       TO SELTAB_WA-OPTION,
            P_ZFINRC   TO SELTAB_WA-LOW,
            SPACE      TO SELTAB_WA-HIGH.
      APPEND SELTAB_WA TO SELTAB.
   ENDIF.

   IF NOT P_ZFRPTTY IS INITIAL.
      MOVE: 'S_RPTTY'  TO SELTAB_WA-SELNAME,
            'S'        TO SELTAB_WA-KIND,      " SELECT-OPTION
            'I'        TO SELTAB_WA-SIGN,
            'EQ'       TO SELTAB_WA-OPTION,
            P_ZFRPTTY  TO SELTAB_WA-LOW,
            SPACE      TO SELTAB_WA-HIGH.
      APPEND SELTAB_WA TO SELTAB.
   ENDIF.

   IF NOT P_ZFBLSDP IS INITIAL.
      MOVE: 'S_BLSDP'  TO SELTAB_WA-SELNAME,
            'S'        TO SELTAB_WA-KIND,      " SELECT-OPTION
            'I'        TO SELTAB_WA-SIGN,
            'EQ'       TO SELTAB_WA-OPTION,
            P_ZFBLSDP  TO SELTAB_WA-LOW,
            SPACE      TO SELTAB_WA-HIGH.
      APPEND SELTAB_WA TO SELTAB.
   ENDIF.
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
*>> 도착항.
  IF NOT S_APRT[] IS INITIAL.
      LOOP AT S_APRT .
         MOVE: 'S_APRT '      TO SELTAB_WA-SELNAME,
               'S'            TO SELTAB_WA-KIND,      " SELECT-OPTION
               S_APRT-SIGN   TO SELTAB_WA-SIGN,
               S_APRT-OPTION TO SELTAB_WA-OPTION,
               S_APRT-LOW    TO SELTAB_WA-LOW,
               S_APRT-HIGH   TO SELTAB_WA-HIGH.
         APPEND SELTAB_WA TO SELTAB.
      ENDLOOP.
   ENDIF.

*>>세관.
   IF NOT S_INRC[] IS INITIAL.
      LOOP AT S_APRT .
         MOVE: 'S_INRC'      TO SELTAB_WA-SELNAME,
               'S'           TO SELTAB_WA-KIND,      " SELECT-OPTION
               S_INRC-SIGN   TO SELTAB_WA-SIGN,
               S_INRC-OPTION TO SELTAB_WA-OPTION,
               S_INRC-LOW    TO SELTAB_WA-LOW,
               S_INRC-HIGH   TO SELTAB_WA-HIGH.
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
*>> 신고형태.
   IF NOT S_RPTTY[] IS INITIAL.
      LOOP AT S_RPTTY.
         MOVE: 'S_RPTTY'      TO SELTAB_WA-SELNAME,
               'S'            TO SELTAB_WA-KIND,      " SELECT-OPTION
               S_RPTTY-SIGN   TO SELTAB_WA-SIGN,
               S_RPTTY-OPTION TO SELTAB_WA-OPTION,
               S_RPTTY-LOW    TO SELTAB_WA-LOW,
               S_RPTTY-HIGH   TO SELTAB_WA-HIGH.
         APPEND SELTAB_WA TO SELTAB.
      ENDLOOP.
   ENDIF.
   IF NOT P_YN IS INITIAL.
      CLEAR SELTAB_WA.
      MOVE: 'P_POYN'   TO SELTAB_WA-SELNAME,
            'P'        TO SELTAB_WA-KIND,      " SELECT-OPTION
            P_YN  TO SELTAB_WA-LOW.
      APPEND SELTAB_WA TO SELTAB.
   ENDIF.

   SUBMIT ZRIMMDTCCLLST
          WITH  SELECTION-TABLE SELTAB
          AND RETURN.

ENDFORM.                    " P2000_TO_DISP_DETAIL
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE_EN
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE_EN.

  SKIP 1.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  IF P_POT = 'X'.
     SELECT SINGLE PORTT INTO W_FNAME
            FROM ZTIEPORT
            WHERE LAND1  EQ  'US'
            AND   PORT   EQ  IT_TAB-ZFAPRTC.

     MOVE '[The Quantity of Material List by Port]' TO W_TITLE.
     CONCATENATE 'Port:' IT_TAB-ZFAPRTC '-' W_FNAME
                                       INTO W_GUBUN.
  ENDIF.
  IF P_CUT = 'X'.
     MOVE '[The Quantity of Material List by Customs]' TO W_TITLE.
     PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDCOTM' IT_TAB-ZFINRC
                               CHANGING   W_DOM_TEX1.
     CONCATENATE 'Customs:' IT_TAB-ZFINRC '-'  W_DOM_TEX1
                                     INTO W_GUBUN.
  ENDIF.
  IF P_YN = 'X'.
     PERFORM  GET_ZTIIMIMG08_SELECT USING '012' IT_TAB-ZFBLSDP
                                  CHANGING   W_FNAME.
     MOVE '[The Quantity of Material List by Non-monetary,Shipback]'
                                           TO W_TITLE.
     CONCATENATE 'Non-Monetary Type :' IT_TAB-ZFBLSDP '-' W_FNAME
                                           INTO W_GUBUN.
  ENDIF.
  IF P_TY = 'X'.
     PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDRPTTY'
                                          IT_TAB-ZFRPTTY
                                   CHANGING   W_DOM_TEX1.
     MOVE '[The Quantity of Material List by Import Type]' TO W_TITLE.
     CONCATENATE 'Import Type:' IT_TAB-ZFRPTTY '-'  W_DOM_TEX1
                                            INTO W_GUBUN.
  ENDIF.
  IF W_CHK_TITLE = 1.
     WRITE : /30 W_TITLE.
     W_CHK_TITLE = 0.
  ENDIF.
  SKIP 2.
  WRITE : / W_GUBUN.
  WRITE : 85 'Date : ', SY-DATUM.
  WRITE : / SY-ULINE.
  FORMAT RESET.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-VLINE NO-GAP,(05) 'Plant'          NO-GAP CENTERED,
            SY-VLINE NO-GAP,(05) 'Count'          NO-GAP CENTERED,
            SY-VLINE NO-GAP,(19) 'Taxable Amount' NO-GAP CENTERED,
            SY-VLINE NO-GAP,(19) 'Duty'           NO-GAP CENTERED,
            SY-VLINE NO-GAP,(19) 'Other Fee'      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(19) 'Total Fee'      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(06) '20Cont'         NO-GAP CENTERED,
            SY-VLINE NO-GAP,(06) '40Cont'         NO-GAP CENTERED,
            SY-VLINE NO-GAP,(06) '20HQ'           NO-GAP CENTERED,
            SY-VLINE NO-GAP,(06) '40HQ'           NO-GAP CENTERED,
            SY-VLINE.
  WRITE : / SY-ULINE.

ENDFORM.                    " P3000_TITLE_WRITE_EN
*&---------------------------------------------------------------------*
*&      Form  P1000_SET_BUKRS
*&---------------------------------------------------------------------*
FORM P1000_SET_BUKRS.

   CLEAR : ZTIMIMG00, P_BUKRS.
   SELECT SINGLE * FROM ZTIMIMG00.
   IF NOT ZTIMIMG00-ZFBUFIX IS INITIAL.
      MOVE  ZTIMIMG00-ZFBUKRS   TO  P_BUKRS.
   ENDIF.

*>> 회사코드 SET.
    MOVE: 'I'          TO S_BUKRS-SIGN,
          'EQ'         TO S_BUKRS-OPTION,
          P_BUKRS      TO S_BUKRS-LOW.
    APPEND S_BUKRS.

ENDFORM.                    " P1000_SET_BUKRS
