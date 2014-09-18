*&--------------------------------------------------------------------
*& Report  ZRIMMDTCCLLST
*&--------------------------------------------------------------------
*&  프로그램명 : 수입물동량 상세내역.
*&      작성자 : 이채경 INFOLINK Ltd.
*&      작성일 : 2001.09.06
*&--------------------------------------------------------------------
*&   DESC.     :
*&
*&--------------------------------------------------------------------
*& [변경내용]
*&
*&--------------------------------------------------------------------
REPORT  ZRIMMDTCCLLST  MESSAGE-ID ZIM
                       LINE-SIZE 135
                     NO STANDARD PAGE HEADING.
TABLES:  ZTBL,ZTIDS,ZTIMIMG08, T001W,ZTIDR, ZTIDSUS, ZTIDRUS.
*---------------------------------------------------------------------
*  리스트용 INTERNAL TABLE
*---------------------------------------------------------------------
DATA : BEGIN OF IT_BL OCCURS 0,
       ZFBLNO     LIKE     ZTBL-ZFBLNO,
       ZFREBELN   LIKE     ZTBL-ZFREBELN,
       ZFHBLNO    LIKE     ZTBL-ZFHBLNO,
       ZFSHNO     LIKE     ZTBL-ZFSHNO,
       ZFBNDT     LIKE     ZTBL-ZFBNDT,
       ZFAPRTC    LIKE     ZTBL-ZFAPRTC,
       ZF20FT     LIKE     ZTBL-ZF20FT,
       ZF40FT     LIKE     ZTBL-ZF40FT,
       ZFPOYN     LIKE     ZTBL-ZFPOYN,
       ZFBLSDP    LIKE     ZTBL-ZFBLSDP,
       ZFRPTTY    LIKE     ZTBL-ZFRPTTY,
       ZFNEWT     LIKE     ZTBL-ZFNEWT,
       ZFNEWTM    LIKE     ZTBL-ZFNEWTM,
       ZFWERKS    LIKE     ZTBL-ZFWERKS.
DATA : END OF IT_BL.

DATA : BEGIN OF IT_TAB OCCURS 0,
       ZFBLNO     LIKE     ZTBL-ZFBLNO,
       ZFREBELN   LIKE     ZTBL-ZFREBELN,
       W_EBELN(12),
       ZFHBLNO    LIKE     ZTBL-ZFHBLNO,
       ZFSHNO     LIKE     ZTBL-ZFSHNO,
       ZFBNDT     LIKE     ZTBL-ZFBNDT,
       ZFAPRTC    LIKE     ZTBL-ZFAPRTC,
       ZFPOYN     LIKE     ZTBL-ZFPOYN,
       ZF20FT     TYPE     I,
       ZF40FT     TYPE     I,
       W_COUNT    TYPE     I,
       ZFINRC     LIKE     ZTIDSUS-ZFINRC,
       ZFCLSEQ    LIKE     ZTIDSUS-ZFCLSEQ,
       POYN       LIKE     DD07T-DDTEXT,
       ZFEDT      LIKE     ZTIDSUS-ZFEDT,
       ZFBLSDP    LIKE     ZTBL-ZFBLSDP,
       ZFRPTTY    LIKE     ZTBL-ZFRPTTY,
       RPTTY      LIKE     DD07T-DDTEXT,
       ZFNEWT     LIKE     ZTBL-ZFNEWT,
       ZFNEWTM    LIKE     ZTBL-ZFNEWTM,
       ZFENTNO    LIKE     ZTIDSUS-ZFENTNO,
       ZFIVAMT    LIKE     ZTIDSUS-ZFIVAMT,
       ZFIVAMC    LIKE     ZTIDSUS-ZFIVAMC,
       ZFIVAMK    LIKE     ZTIDSUS-ZFIVAMK,
       ZFKRW      LIKE     ZTIDSUS-ZFKRW,
       ZFDUTY     LIKE     ZTIDSUS-ZFDUTY,
       ZFTOFEE    LIKE     ZTIDSUS-ZFTOFEE,
       ZFOTFE     LIKE     ZTIDSUS-ZFOTFE,
       ZFTOCUR    LIKE     ZTIDSUS-ZFTOCUR,
       ZFWERKS    LIKE     ZTBL-ZFWERKS.
DATA :  END OF IT_TAB.

DATA :  W_ERR_CHK     TYPE C,
        W_FIELD_NM    TYPE C,
        W_PAGE        TYPE I,
        W_TITLE(50),
        W_TITLE1(50),
        W_DOM_TEX1     LIKE DD07T-DDTEXT,
        W_FNAME        LIKE ZTIMIMG08-ZFCDNM,
        W_CHK_TITLE,
        W_LINE        TYPE I,
        L_COUNT       TYPE I,
        W_GUBUN(50),
        W_COUNT       TYPE I,
        W_DESC(12),
        W_LCOUNT       TYPE I,
        W_SUBRC       LIKE SY-SUBRC,
        W_TABIX       LIKE SY-TABIX,
        W_ZFCLSEQ     LIKE ZTIDS-ZFCLSEQ,
        W_LIST_INDEX  LIKE SY-TABIX.
*>> SUTOTAL.
 DATA: S_COUNT     TYPE I,
       S_ZFNEWT    LIKE  ZTBL-ZFNEWT,
       S_ZFTBAU    LIKE  ZTIDS-ZFTBAU,
       S_ZFTBAK    LIKE  ZTIDS-ZFTBAK,
       S_ZFTOT     LIKE  ZTIDS-ZFVAAMTS,
       S_ZFVAAMTS  LIKE  ZTIDS-ZFVAAMTS,
       S_ZFTXAMTS  LIKE  ZTIDS-ZFTXAMTS,
       S_ZF20FT    TYPE I,
       S_ZF40FT    TYPE I.

INCLUDE   ZRIMSORTCOM.    " REPORT Sort
INCLUDE   ZRIMUTIL01.     " Utility function 모?

*---------------------------------------------------------------------
* Selection Screen ?
*---------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS: S_BUKRS     FOR ZTBL-BUKRS NO-EXTENSION
                                               NO INTERVALS,
                   S_IDSDT   FOR ZTIDS-ZFIDSDT,     " 면허일.
                   S_BNDT    FOR ZTBL-ZFBNDT,       " 보세운송일.
                   S_WERKS   FOR ZTBL-ZFWERKS,      " PLANT,
                   S_APRT    FOR ZTBL-ZFAPRTC,      " 도착항.
                   S_INRC    FOR ZTIDSUS-ZFINRC,    " 세관.
                   S_RPTTY   FOR ZTBL-ZFRPTTY,      " 수입신고형태.
                   S_BLSDP   FOR ZTBL-ZFBLSDP,      " 송부처.
                   S_TRQDT   FOR ZTBL-ZFTRQDT,      " 운송요청일.
                   S_TRCK    FOR ZTBL-ZFTRCK.
   PARAMETERS : P_POYN    AS CHECKBOX.              " 무환여부.

SELECTION-SCREEN END OF BLOCK B1.

INITIALIZATION.                          " 초기값 SETTING
   PERFORM   P2000_INIT.

*title Text Write
TOP-OF-PAGE.
 PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...

*---------------------------------------------------------------------
* START OF SELECTION ?
*---------------------------------------------------------------------
START-OF-SELECTION.
* B/L 테이블 SELECT
   PERFORM   P1000_READ_DATA USING W_ERR_CHK.
   IF W_ERR_CHK = 'Y'.
      MESSAGE S738. EXIT.
   ENDIF.
* 레포트 Write
   PERFORM  P3000_DATA_WRITE.

*---------------------------------------------------------------------
* User Command
*---------------------------------------------------------------------
AT USER-COMMAND.

   CASE SY-UCOMM.
       WHEN 'STUP' OR 'STDN'.         " SORT 선택?
          W_FIELD_NM = 'ZFWERKS'.
          ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
          PERFORM HANDLE_SORT TABLES  IT_TAB
                              USING   SY-UCOMM.
     WHEN 'REFR'.
           W_LCOUNT = 0.
           PERFORM   P1000_READ_DATA USING W_ERR_CHK.
           PERFORM RESET_LIST.
     WHEN 'DISP1'.                       " B/L 조회.
           IF W_TABIX EQ 0.
              MESSAGE S962.EXIT.
           ENDIF.
           PERFORM P2000_DISP_ZTBL(SAPMZIM09) USING IT_TAB-ZFBLNO.
     WHEN 'DISP2'.
          IF W_TABIX EQ 0.
              MESSAGE S962.EXIT.
          ENDIF.
          PERFORM P2000_DISP_ZTIV USING IT_TAB-ZFBLNO.
     WHEN 'DISP3'. " 수입신고.
           IF W_TABIX EQ 0.
              MESSAGE S962.EXIT.
           ENDIF.
           SELECT SINGLE *
              FROM ZTIDRUS
              WHERE ZFBLNO  = IT_TAB-ZFBLNO
                AND ZFCLSEQ = IT_TAB-ZFCLSEQ.
           IF SY-SUBRC NE 0.
              MESSAGE S753. EXIT.
           ENDIF.
           PERFORM P2000_DISP_ZTIDR  USING IT_TAB-ZFBLNO IT_TAB-ZFCLSEQ.
     WHEN 'DISP4'.                       " 수입면허.
           IF W_TABIX EQ 0.
              MESSAGE S962.EXIT.
           ENDIF.
           IF IT_TAB-ZFEDT IS INITIAL.
              MESSAGE S639.EXIT.
           ENDIF.
           PERFORM P2000_DISP_ZTIDS USING IT_TAB-ZFBLNO IT_TAB-ZFCLSEQ.
     WHEN 'DOWN'.          " FILE DOWNLOAD....
           PERFORM P3000_TO_PC_DOWNLOAD.
  ENDCASE.

*&--------------------------------------------------------------------
*&      Form  P3000_TITLE_WRITE
*&--------------------------------------------------------------------
FORM P3000_TITLE_WRITE.

   CLEAR  T001W.
   SELECT SINGLE *
          FROM  T001W
          WHERE  WERKS = S_WERKS-LOW.

  SKIP 1.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /60 '[The Quantity of material detail list]'.
  IF NOT S_APRT-LOW IS INITIAL.
     SELECT SINGLE PORTT INTO W_FNAME
            FROM  ZTIEPORT
            WHERE LAND1 EQ 'US'
            AND   PORT  EQ S_APRT-LOW.
     MOVE '[The Quantity of Material List by Port]' TO W_TITLE.
     CONCATENATE 'Port:' S_APRT-LOW  '-' W_FNAME
                                       INTO W_GUBUN.
  ENDIF.
  IF NOT P_POYN IS INITIAL.
     PERFORM  GET_ZTIIMIMG08_SELECT USING '012' S_BLSDP-LOW
                                   CHANGING   W_FNAME.
     CONCATENATE 'Shipping document sending department:'
               S_BLSDP-LOW '-' W_FNAME INTO W_GUBUN.
  ENDIF.
  IF NOT S_RPTTY-LOW IS INITIAL.
     PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDRPTTY'
                                          S_RPTTY-LOW
                                      CHANGING   W_DOM_TEX1.
     MOVE '[The Quantity of material detail list]' TO W_TITLE.
     CONCATENATE 'Declaration Type:' S_RPTTY-LOW  '-'  W_DOM_TEX1
                                            INTO W_GUBUN.

  ENDIF.
  IF NOT S_INRC-LOW IS INITIAL.
     PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDCOTM' S_INRC-LOW
                          CHANGING   W_DOM_TEX1.
     CONCATENATE 'Customs:' S_INRC-LOW '-'  W_DOM_TEX1
                                    INTO W_GUBUN.
  ENDIF.
  WRITE : / 'Plant:', S_WERKS-LOW,(20)T001W-NAME1,W_GUBUN,
             115 'Date : ',SY-DATUM.


  WRITE : / SY-ULINE.
  FORMAT RESET.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE,(12) 'Purchase Ord.'    CENTERED,
            SY-VLINE,(24) 'House B/L No'     CENTERED,
            SY-VLINE,(10) 'Declaration Type' CENTERED,
            SY-VLINE,(10) 'Transport'        CENTERED,
            SY-VLINE,(10) 'Entry Date'       CENTERED,
            SY-VLINE,(19) 'Taxable Amount'   CENTERED,
            SY-VLINE,(19) 'Total Fee'        CENTERED,
            SY-VLINE,(06) '20Cont'           CENTERED,
            SY-VLINE.
  FORMAT RESET.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE: /  SY-VLINE,(12) 'B/L No' CENTERED,
            SY-VLINE,(24) 'Entry No'         CENTERED,
            SY-VLINE,(10) 'Non/Mone.'        CENTERED,
            SY-VLINE,(23) 'Weight'           CENTERED,
            SY-VLINE,(19) 'Duty'             CENTERED,
            SY-VLINE,(19) 'Other Fee'        CENTERED,
            SY-VLINE,(06) '40Cont'           CENTERED,
            SY-VLINE.
  WRITE : / SY-ULINE.

ENDFORM.                    " P3000_TITLE_WRITE

*&--------------------------------------------------------------------
*&      Form  P1000_GET_IT_TAB
*&--------------------------------------------------------------------
FORM P1000_READ_DATA USING W_ERR_CHK.

  W_ERR_CHK = 'N'.

  RANGES: R_ZFPOYN FOR ZTBL-ZFPOYN OCCURS 5.

*  IF P_POYN EQ 'X'.  " 무환 BACK.
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

  IF P_POYN NE 'X'.  " 무환 BACK.
     MOVE :    'I'       TO  R_ZFPOYN-SIGN,
               'EQ'      TO  R_ZFPOYN-OPTION,
               'Y'       TO  R_ZFPOYN-LOW,
               SPACE     TO  R_ZFPOYN-HIGH.
     APPEND  R_ZFPOYN.

  ENDIF.
*>>  B/L
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_BL
      FROM ZTBL
     WHERE ZFWERKS IN S_WERKS     " PLANT,
       AND BUKRS   IN S_BUKRS
       AND ZFAPRTC IN S_APRT      " 도착항.
       AND ZFRPTTY IN S_RPTTY     " 수입신고형태.
       AND ZFRPTTY NE SPACE       " 수입신고형태.
       AND ZFBLSDP IN S_BLSDP     " 송부처.
       AND ZFBNDT  IN S_BNDT      " 보세운송일.
       AND ZFPOYN  IN R_ZFPOYN    " 유무환구분.
       AND ZFTRCK  IN S_TRCK
       AND ZFTRQDT IN S_TRQDT.
  IF SY-SUBRC <> 0.  W_ERR_CHK = 'Y'.   EXIT.    ENDIF.
*>> 수입면허.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TAB
           FROM ZTIDSUS
           FOR ALL ENTRIES IN IT_BL
           WHERE ZFBLNO  = IT_BL-ZFBLNO
             AND ZFINRC  IN S_INRC
             AND ZFEDT   IN S_IDSDT.

  LOOP AT IT_BL.
*>> 보세운송이 있고 면허일이 없는경우 통관이 안된 경우 APPEND.
    IF NOT S_IDSDT[] IS INITIAL.
       CONTINUE.
    ENDIF.
    IF NOT S_BNDT[] IS INITIAL.
       CLEAR: S_COUNT.
       IF  IT_BL-ZFRPTTY = 'B' OR  IT_BL-ZFRPTTY = 'W'.
           READ TABLE IT_TAB WITH KEY ZFBLNO = IT_BL-ZFBLNO.
           IF SY-SUBRC NE 0.
              MOVE-CORRESPONDING IT_BL TO IT_TAB.
              APPEND IT_TAB.
              S_COUNT = S_COUNT + 1.
           ENDIF.
       ENDIF.
    ENDIF.
  ENDLOOP.

  LOOP AT IT_TAB.

     W_TABIX = SY-TABIX.
     READ TABLE IT_BL WITH KEY ZFBLNO = IT_TAB-ZFBLNO.
     IF SY-SUBRC NE 0.
*        DELETE IT_TAB INDEX W_TABIX.
*        CONTINUE.
     ENDIF.
     MOVE-CORRESPONDING IT_BL TO IT_TAB.
     IT_TAB-ZFTOFEE =  IT_TAB-ZFDUTY  + IT_TAB-ZFOTFE .

     PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDRPTTY' IT_TAB-ZFRPTTY
                               CHANGING   IT_TAB-RPTTY.
     PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDPOYN'  IT_TAB-ZFPOYN
                               CHANGING   IT_TAB-POYN.
*>> 선적차수.
    IF NOT IT_TAB-ZFSHNO IS INITIAL.
          CONCATENATE IT_TAB-ZFREBELN '-' IT_TAB-ZFSHNO
                      INTO IT_TAB-W_EBELN.
    ELSE.
          MOVE  IT_TAB-ZFREBELN  TO  IT_TAB-W_EBELN.
    ENDIF.
*>> 분할 통관 체크.
    MODIFY IT_TAB INDEX W_TABIX.
    SELECT COUNT( * ) INTO W_LINE
       FROM ZTIDSUS
       WHERE ZFBLNO = IT_TAB-ZFBLNO
         AND ZFEDT   IN S_IDSDT
         AND ZFINRC  IN S_INRC.

    ON CHANGE OF IT_TAB-ZFBLNO.
       CLEAR L_COUNT.
    ENDON.
    IF W_LINE >= 2.
       ADD 1 TO L_COUNT.       " 분할 통관갯수.
       IF L_COUNT NE W_LINE.
          CONTINUE.
       ENDIF.
    ENDIF.
    W_LCOUNT = W_LCOUNT + 1.  " 물통량.

  ENDLOOP.

  DESCRIBE TABLE IT_TAB LINES W_LINE.
  IF W_LINE = 0.
     W_ERR_CHK = 'Y'. EXIT.
  ENDIF.

ENDFORM.                    " P1000_READ_DATA

*&--------------------------------------------------------------------
*&      Form  P3000_DATA_WRITE
*&--------------------------------------------------------------------
FORM P3000_DATA_WRITE .

   SET TITLEBAR  'DTCCL'.
   SET PF-STATUS 'DTCCL'.
   LOOP AT IT_TAB.
       W_TABIX = SY-TABIX.
       PERFORM   P3000_LINE_WRITE.
      AT LAST.
         PERFORM P3000_LINE_TOTAL.
      ENDAT.

   ENDLOOP.
   CLEAR: IT_TAB,W_TABIX.
ENDFORM.                    " P3000_DATA_WRITE
*&--------------------------------------------------------------------
*&      Form  P2000_INIT
*&--------------------------------------------------------------------
FORM P2000_INIT.

  SET TITLEBAR  'DTCCL'.

ENDFORM.                    " P2000_INIT
*&--------------------------------------------------------------------
*&      Form  P3000_LINE_WRITE
*&--------------------------------------------------------------------
FORM P3000_LINE_WRITE.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  WRITE : / SY-VLINE,(12) IT_TAB-W_EBELN,
            SY-VLINE,(24) IT_TAB-ZFHBLNO,
            SY-VLINE,(10) IT_TAB-RPTTY,   " 통관구분.
            SY-VLINE,(10) IT_TAB-ZFBNDT,  " 보세운송일.
            SY-VLINE,(10) IT_TAB-ZFEDT,   " 면허일.
            SY-VLINE,(19) IT_TAB-ZFIVAMK CURRENCY 'USD',
            SY-VLINE,(19) IT_TAB-ZFTOFEE CURRENCY 'USD',
            SY-VLINE,(06) IT_TAB-ZF20FT,
            SY-VLINE.
  HIDE : IT_TAB,W_TABIX.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  WRITE : / SY-VLINE,(12) IT_TAB-ZFBLNO,
            SY-VLINE,(24) IT_TAB-ZFENTNO,
            SY-VLINE,(10) IT_TAB-POYN,
            SY-VLINE,(23) IT_TAB-ZFNEWT UNIT IT_TAB-ZFNEWTM,
            SY-VLINE,(19) IT_TAB-ZFDUTY   CURRENCY 'USD',
            SY-VLINE,(19) IT_TAB-ZFOTFE   CURRENCY 'USD',
            SY-VLINE,(06) IT_TAB-ZF40FT,SY-VLINE.
  HIDE : IT_TAB, W_TABIX.
  WRITE : / SY-ULINE.
  IF NOT IT_TAB-ZFEDT IS INITIAL.  " 통관건수.
        W_COUNT = W_COUNT + 1.
  ENDIF.

 ENDFORM.

*&--------------------------------------------------------------------
*&      Form  P3000_LINE_TOTAL
*&--------------------------------------------------------------------
FORM P3000_LINE_TOTAL.

   FORMAT RESET.
   FORMAT COLOR COL_TOTAL INTENSIFIED ON.
   SUM.
   WRITE :/ SY-VLINE,(12) 'T O T A L ',
                     (26) ' ',
                     (12) ' ',
                     (12) ' ',
                     (12) ' ',
            SY-VLINE,(19) IT_TAB-ZFIVAMK CURRENCY 'USD',
            SY-VLINE,(19) IT_TAB-ZFTOFEE CURRENCY 'USD',
            SY-VLINE,(06) IT_TAB-ZF20FT,
            SY-VLINE.
  FORMAT RESET.
  FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
  WRITE: /  SY-VLINE,(12) 'B/L Count :',
                     (05) W_LCOUNT,(04)'Case',(02)'',
                     (12)'Clear. Cnt:',
                     (05) W_COUNT, (04)'Case',(01)'',
                     (25) IT_TAB-ZFNEWT  UNIT IT_TAB-ZFNEWTM,
            SY-VLINE,(19) IT_TAB-ZFDUTY    CURRENCY 'USD',
            SY-VLINE,(19) IT_TAB-ZFOTFE    CURRENCY 'USD',
            SY-VLINE,(06) IT_TAB-ZF40FT, SY-VLINE.
  WRITE : / SY-ULINE.

ENDFORM.                    " P3000_LINE_TOTAL
*&--------------------------------------------------------------------
*&      Form  RESET_LIST
*&--------------------------------------------------------------------
FORM RESET_LIST.

  W_CHK_TITLE = 1.
  W_COUNT = 0.
  MOVE 0 TO SY-LSIND.
  PERFORM   P3000_TITLE_WRITE.     " 해더 출력...
  PERFORM   P3000_DATA_WRITE.

ENDFORM.                    " RESET_LIST
*&---------------------------------------------------------------------
*
*&      Form  GET_ZTIIMIMG08_SELECT
*&---------------------------------------------------------------------
*
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
*&      Form  P2000_DISP_ZTIV
*&---------------------------------------------------------------------
*
FORM P2000_DISP_ZTIV USING    P_ZFBLNO.

   SELECT COUNT( * ) INTO W_COUNT
      FROM ZTIV
     WHERE ZFBLNO = P_ZFBLNO.

   IF W_COUNT = 0.
      MESSAGE S679 WITH  P_ZFBLNO.
      EXIT.
   ENDIF.

   SET PARAMETER ID 'ZPIVNO'  FIELD ''.
   SET PARAMETER ID 'ZPHBLNO' FIELD ''.
   SET PARAMETER ID 'ZPBLNO'  FIELD P_ZFBLNO.

   CALL TRANSACTION 'ZIM33'  AND SKIP  FIRST SCREEN.


ENDFORM.                    " P2000_DISP_ZTIV
*&---------------------------------------------------------------------*
*&      Form  P2000_DISP_ZTIDR
*&---------------------------------------------------------------------*
FORM P2000_DISP_ZTIDR USING    P_ZFBLNO
                               P_ZFCLSEQ.

   SET PARAMETER ID 'ZPHBLNO'  FIELD ''.
   SET PARAMETER ID 'ZPBLNO'   FIELD P_ZFBLNO.
   SET PARAMETER ID 'ZPCLSEQ'  FIELD P_ZFCLSEQ.

   CALL TRANSACTION 'ZIMCD3'  AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_DISP_ZTIDR

*&---------------------------------------------------------------------*
*&      Form  P2000_DISP_ZTIDS
*&---------------------------------------------------------------------*
FORM P2000_DISP_ZTIDS USING    P_ZFBLNO
                               P_ZFCLSEQ.

   SET PARAMETER ID 'ZPHBLNO'  FIELD ''.
   SET PARAMETER ID 'ZPBLNO'   FIELD P_ZFBLNO.
   SET PARAMETER ID 'ZPCLSEQ'  FIELD P_ZFCLSEQ.
   SET PARAMETER ID 'ZPENTNO'  FIELD ''.

   CALL TRANSACTION 'ZIMCC3'  AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_DISP_ZTIDS
