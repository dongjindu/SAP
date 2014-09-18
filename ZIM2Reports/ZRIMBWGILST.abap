*&---------------------------------------------------------------------*
*& Report  ZRIMBWGILST                                                 *
*&                                                                     *
*&---------------------------------------------------------------------*
*&  프로그램명 : 보세창고 출고의뢰 LIST                                *
*&      작성자 : 이승준 INFOLINK Ltd.                                  *
*&      작성일 : 2001.09.13                                            *
*&---------------------------------------------------------------------*
*&  DESC.      :  통관부서 수입신고 의뢰건 대상으로 보세창고에서
*&                LIST-UP하여  출고준비
*&---------------------------------------------------------------------*
*& [변경내용]
*& 2001.11.07 김영광(LG-EDS): 통관완료 건 출력, 플랜트 및 선적차수 포함.
*&                            Indentation.
*&---------------------------------------------------------------------*

REPORT  ZRIMBWGILST  MESSAGE-ID ZIM
                     LINE-SIZE 130
                     NO STANDARD PAGE HEADING.

*----------------------------------------------------------------------*
* TABLES 및 변수 DEFINE.                                               *
*----------------------------------------------------------------------*
*>> TABLE DEFINE.
TABLES : ZTBL,
         ZTBWHD,
         ZTCUCL,
         ZTIDR,
         ZTBLINR.

*>> 변수 DEFINE.
DATA : W_ERR_CHK TYPE C VALUE 'N',   "ERROR CHECK.
       MAX LIKE ZTBLINR-ZFBTSEQ,     "ZFBTSEQ의 최대값.
       WA_LINE TYPE I,               "IT_TAB LINE 수.
       ZFTOWT1 TYPE I,               "합계용.
       W_LINE  TYPE I,                "SUB LINE 수
       W_MOD LIKE SY-TABIX,          "MOD 용.
       W_TABIX LIKE SY-TABIX,        "HIDE 용.
       MARKFIELD TYPE C.             "CHECKBOX용,

*>>CONTROL LEVEL GROUP용.
FIELD-GROUPS: HEADER.

*>> IT_TAB DEFINE.
DATA: BEGIN OF IT_TAB OCCURS 0,
***> GROUP KEY
      ZFBLSDP  LIKE ZTBL-ZFBLSDP,   "지역-B/L송부처
      ZFBNARCD LIKE ZTBL-ZFBNARCD,  "장치장소-보세구역코드
      ZFCLSEQ  LIKE ZTIDR-ZFCLSEQ,
***> KEY WORD
      ZFIVNO   LIKE ZTBWHD-ZFIVNO,   "외자적송명세서용.
      ZFGISEQ  LIKE ZTBWHD-ZFGISEQ,  "외자적송명세서용.
      ZFBLNO   LIKE  ZTBWHD-ZFBLNO,  " B/L 관리NO
      ZFBTSEQ  LIKE  ZTBLINR-ZFBTSEQ, "보세운송 일련번호
***> WRITE FILED.
      ZFREBELN LIKE  ZTBWHD-ZFREBELN, "대표P/O NO.
      ZFSHNO   LIKE  ZTBWHD-ZFSHNO,   "선적차수.
*      ZFIDSDT  LIKE ZTBWHD-ZFIDSDT,  "면허일자-신고수리일
      ZFIDWDT  LIKE ZTBWHD-ZFIDWDT,  "희망일-신고희망일
      ZFNEWT   LIKE ZTBL-ZFNEWT ,    "순중량
      ZFNEWTM  LIKE ZTBL-ZFNEWTM,    "순중량단위
      ZFTOWT   LIKE ZTBWHD-ZFTOWT ,  "총중량
      ZFTOWTM  LIKE ZTBWHD-ZFTOWTM,  "단위
      ZFPKCN   LIKE ZTBWHD-ZFPKCN ,  "총포장개수
      ZFPKCN_I TYPE I,               "SUM용.
      ZFPKCNM  LIKE ZTBWHD-ZFPKCNM,  "포장 종류
      ZFCARNM  LIKE ZTBL-ZFCARNM,    "선기명
      ZFRGDSR  LIKE ZTBL-ZFRGDSR,    "대표품명
      ZFHBLNO  LIKE ZTBL-ZFHBLNO,    "HOUSE B/L NO
      ZFINRNO  LIKE ZTBLINR-ZFINRNO, "반입신고번호
      ZFLOC    LIKE ZTBLINR-ZFLOC,   "저장위치
      ZFWERKS    LIKE ZTBL-ZFWERKS.    "플랜트
DATA : END OF IT_TAB.

DATA : IT_T001W    LIKE    T001W       OCCURS 0 WITH HEADER LINE.
DATA : WA_TAB LIKE IT_TAB.

*----------------------------------------------------------------------*
*          SELECTION-SCREEN                                            *
*----------------------------------------------------------------------*

SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS:
    S_BUKRS     FOR ZTBL-BUKRS NO-EXTENSION
                               NO INTERVALS,
    S_BLSDP  FOR ZTBWHD-ZFBLSDP,  "지역-B/L송부처
    S_BNARCD FOR ZTBWHD-ZFBNARCD, "장치장소-보세구역코드
    S_REBELN FOR ZTBWHD-ZFREBELN, "대표P/O NO.
*   S_IDSDT  FOR ZTBWHD-ZFIDSDT,  "면허일자-신고수리일
    S_IDWDT  FOR ZTBWHD-ZFIDWDT.  "희망일-신고희망일
SELECTION-SCREEN END OF BLOCK B1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_BLSDP-LOW.
    PERFORM   P1000_BL_SDP_HELP  USING  S_BLSDP-LOW.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_BLSDP-HIGH.
    PERFORM   P1000_BL_SDP_HELP  USING  S_BLSDP-HIGH.

*----------------------------------------------------------------------*
*          INITIALIZATION.                                             *
*----------------------------------------------------------------------*
INITIALIZATION.                          " 초기값 SETTING
    PERFORM   P2000_SET_PARAMETER.

*----------------------------------------------------------------------*
*          START-OF-SELECTION.                                         *
*----------------------------------------------------------------------*
START-OF-SELECTION.

*>>INTERNAL TABLE CLEAR.
    REFRESH IT_TAB.
    CLEAR IT_TAB.

*>> READ TABLE ZTBWHD
    PERFORM   P1000_READ_ZTBWHD        USING   W_ERR_CHK.

    LOOP AT IT_TAB.
        W_TABIX = SY-TABIX.

*       SELECT SINGLE ZFBNARCD INTO IT_TAB-ZFBNARCD
*              FROM   ZTIDR
*              WHERE  ZFBLNO  EQ  IT_TAB-ZFBLNO
*              AND    ZFCLSEQ EQ ( SELECT MAX( ZFCLSEQ )
*                                         FROM ZTIDR
*                                       WHERE ZFBLNO EQ IT_TAB-ZFBLNO ).

*>>   GET ZTBLINR-ZFBTSEQD 의 MAX값
        PERFORM   P_1000_GET_ZFBTSEQD    USING   W_ERR_CHK.
        PERFORM   P_1000_READ_ZTBLINR    USING   W_ERR_CHK.
        PERFORM   P_1000_READ_ZTBL       USING   W_ERR_CHK.
        MOVE IT_TAB-ZFPKCN TO IT_TAB-ZFPKCN_I.
        MODIFY IT_TAB INDEX W_TABIX.
    ENDLOOP.

    CLEAR W_TABIX.
*>> WRITE IT_TAB.
    PERFORM   P_3000_WRITE_IT        USING W_ERR_CHK.

*-----------------------------------------------------------------------
* User Command
*         -ZRIMBWLST [Report] 보세창고출고현황 참조.
*-----------------------------------------------------------------------
AT USER-COMMAND.
    CASE SY-UCOMM.
        WHEN 'DISP'.                     " 보세창고출고.
            IF W_TABIX EQ 0.
                MESSAGE E962 .
            ELSE.
                PERFORM  P2000_DISP_ZTBWHD USING IT_TAB-ZFHBLNO
                                                 IT_TAB-ZFBLNO.
            ENDIF.

        WHEN 'DISP1'.                       " B/L 조회.
            IF W_TABIX NE 0.
                PERFORM P2000_DISP_ZTBL(SAPMZIM09) USING IT_TAB-ZFBLNO.
            ELSE.
                MESSAGE E962.
            ENDIF.

        WHEN 'DISP2'.                       " 통관요청.
            IF W_TABIX NE 0.
                PERFORM P2000_DISP_ZTIV           USING IT_TAB-ZFHBLNO
                                                 IT_TAB-ZFBLNO.
            ELSE.
                MESSAGE E962.
            ENDIF.

        WHEN 'DISP3'.                        " 수입신고.
            IF W_TABIX NE 0.
                PERFORM P2000_DISP_ZTIDR        USING IT_TAB-ZFBLNO
                                                      IT_TAB-ZFHBLNO.
            ELSE.
                MESSAGE E962.
            ENDIF.

        WHEN 'DISP4'.                        " 수입면허.
            IF W_TABIX NE 0.
                PERFORM P2000_DISP_ZTIDS            USING IT_TAB-ZFBLNO
                                                         IT_TAB-ZFHBLNO.
            ELSE.
                MESSAGE E962.
            ENDIF.

        WHEN 'BWPRT'.                       " 외자적송명세서.
            IF W_TABIX NE 0.
                PERFORM P2000_PRINT_ZTBW(SAPMZIM09) USING IT_TAB-ZFIVNO
                                                        IT_TAB-ZFGISEQ.
            ELSE.
                MESSAGE E962.
            ENDIF.
    ENDCASE.

    CLEAR: IT_TAB ,W_TABIX.

*----------------------------------------------------------------------*
*           TOP-OF-PAGE                                                *
*----------------------------------------------------------------------*
TOP-OF-PAGE.
    FORMAT RESET.

    WRITE : /55 '[ 보세창고 출고의뢰 LIST ]'
            COLOR COL_HEADING INTENSIFIED OFF.

*-----------------------FORM 시작--------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.
    SET TITLEBAR 'ZIMR65'.                     " TITLE BAR
    REFRESH IT_TAB.
    CLEAR IT_TAB.
ENDFORM.                                     " P2000_SET_PARAMETER

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_ZTBWHD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_ZTBWHD USING  W_ERR_CHK.
    W_ERR_CHK = 'N'.                " Error Bit Setting
*MODIFIED BY SEUNGYEON.(2002.08.30)
    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TAB
             FROM ( ZTBL AS H INNER JOIN ZTIDR AS S
                    ON   H~ZFBLNO    EQ S~ZFBLNO )
                     INNER JOIN ZTIV AS I
                     ON S~ZFIVNO   EQ   I~ZFIVNO
*                    INNER JOIN ZTCUCL AS I
*                    ON   S~ZFBLNO    EQ  I~ZFBLNO
*                    AND  S~ZFCLSEQ   EQ  I~ZFCLSEQ
            WHERE  H~ZFBLSDP  IN S_BLSDP
              AND  H~BUKRS    IN S_BUKRS
              AND  S~ZFBNARCD IN S_BNARCD
              AND  H~ZFREBELN IN S_REBELN
              AND  I~ZFCUST   IN ('3', 'Y')
*             AND  S~ZFDOCST  EQ 'R'
              AND  H~ZFRPTTY  IN ('B', 'N')
*             AND  S~ZFIDSDT  IN S_IDSDT.
              AND  S~ZFIDWDT  IN S_IDWDT.

*  SELECT * FROM ZTBWHD INTO CORRESPONDING FIELDS OF TABLE IT_TAB
*          WHERE  ZFBLSDP  IN S_BLSDP
*            AND  ZFBNARCD IN S_BNARCD
*            AND  ZFREBELN IN S_REBELN
*            AND  ZFIDSDT  IN S_IDSDT
*            AND  ZFIDWDT  IN S_IDWDT.

    IF SY-SUBRC NE 0.               " Not Found?
        W_ERR_CHK = 'Y'.  MESSAGE S738. EXIT.
    ENDIF.

    SELECT * INTO TABLE IT_T001W FROM T001W
             FOR ALL ENTRIES IN IT_TAB
             WHERE  WERKS EQ IT_TAB-ZFWERKS.

ENDFORM.                    " P1000_READ_ZTBWHD

*&---------------------------------------------------------------------*
*&      Form  P_1000_GET_ZFBTSEQD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ERR_CHK  text
*----------------------------------------------------------------------*
FORM P_1000_GET_ZFBTSEQD USING    P_W_ERR_CHK.
    W_ERR_CHK = 'N'.                " Error Bit Setting

    SELECT MAX( ZFBTSEQ ) INTO MAX FROM ZTBLINR
           WHERE  ZFBLNO = IT_TAB-ZFBLNO
           GROUP BY ZFBLNO.
    ENDSELECT.

    IF SY-SUBRC NE 0.               " Not Found?
        W_ERR_CHK = 'Y'.  MESSAGE S738. EXIT.
    ENDIF.

    MOVE MAX TO IT_TAB-ZFBTSEQ.
    CLEAR MAX.
ENDFORM.                    " P_1000_GET_ZFBTSEQD

*&---------------------------------------------------------------------*
*&      Form  P_1000_READ_ZTBLINR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ERR_CHK  text
*----------------------------------------------------------------------*
FORM P_1000_READ_ZTBLINR USING    P_W_ERR_CHK.
    W_ERR_CHK = 'N'.                " Error Bit Setting
    CLEAR ZTBLINR.

    SELECT SINGLE * FROM ZTBLINR
         WHERE ZFBTSEQ = IT_TAB-ZFBTSEQ
           AND ZFBLNO  = IT_TAB-ZFBLNO.

    IF SY-SUBRC EQ 0.
        MOVE  ZTBLINR-ZFINRNO TO IT_TAB-ZFINRNO.
        MOVE  ZTBLINR-ZFLOC   TO IT_TAB-ZFLOC.
    ENDIF.

    IF SY-SUBRC NE 0.               " Not Found?
        W_ERR_CHK = 'Y'.  MESSAGE S738. EXIT.
    ENDIF.
ENDFORM.                    " P_1000_READ_ZTBLINR

*&---------------------------------------------------------------------*
*&      Form  P_1000_READ_ZTBL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ERR_CHK  text
*----------------------------------------------------------------------*
FORM P_1000_READ_ZTBL USING    P_W_ERR_CHK.
    W_ERR_CHK = 'N'.                " Error Bit Setting
    CLEAR ZTBL.

    SELECT SINGLE * FROM ZTBL
           WHERE  ZFBLNO  = IT_TAB-ZFBLNO.

    IF SY-SUBRC EQ 0.
        SELECT SUM( BLMENGE ) INTO IT_TAB-ZFNEWT
               FROM ZTBLIT
               WHERE ZFBLNO EQ IT_TAB-ZFBLNO.

        SELECT MEINS INTO IT_TAB-ZFNEWTM
               FROM  ZTBLIT  UP TO 1 ROWS
               WHERE ZFBLNO EQ IT_TAB-ZFBLNO.

        ENDSELECT.

        MOVE   ZTBL-ZFBLSDP  TO IT_TAB-ZFBLSDP.
        MOVE   ZTBL-ZFBNARCD TO IT_TAB-ZFBNARCD.
        MOVE   ZTBL-ZFRGDSR  TO IT_TAB-ZFRGDSR.
*       MOVE   ZTBL-ZFNEWT   TO IT_TAB-ZFNEWT.
*       MOVE   ZTBL-ZFNEWTM  TO IT_TAB-ZFNEWTM.
        MOVE   ZTBL-ZFCARNM  TO IT_TAB-ZFCARNM.
        MOVE   ZTBL-ZFHBLNO  TO IT_TAB-ZFHBLNO.
    ENDIF.

    IF SY-SUBRC NE 0.               " Not Found?
        W_ERR_CHK = 'Y'.  MESSAGE S738. EXIT.
    ENDIF.
ENDFORM.                    " P_1000_READ_ZTBL

*&---------------------------------------------------------------------*
*&      Form  P_3000_WRITE_IT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ERR_CHK  text
*----------------------------------------------------------------------*
FORM P_3000_WRITE_IT USING    P_W_ERR_CHK.
    SET  PF-STATUS 'ZIMR65'.
    SET  TITLEBAR 'ZIMR65'.
    SORT IT_TAB BY ZFBLSDP ZFBNARCD.
    CLEAR WA_LINE.
    DESCRIBE TABLE IT_TAB LINES WA_LINE.
    PERFORM P3000_LINE_WRITE.
ENDFORM.                     " P_3000_WRITE_IT

*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_LINE_WRITE.
    DATA : W_TEXT1(20),
           W_TEXT2(20),
           W_PO(15).

    W_LINE = 0.

    LOOP AT IT_TAB.
        W_LINE = W_LINE + 1.
        W_TABIX = SY-TABIX.

        FORMAT RESET.
*>>     보세구역코드가 바뀔때만.

        AT NEW ZFBNARCD.
            SKIP.
            CLEAR : W_TEXT1, W_TEXT2.

            IF NOT IT_TAB-ZFBLSDP IS INITIAL.
                SELECT SINGLE ZFCDNM INTO W_TEXT1 FROM   ZTIMIMG08
                       WHERE  ZFCDTY   EQ   '012'
                       AND    ZFCD     EQ   IT_TAB-ZFBLSDP.
            ENDIF.

            IF NOT IT_TAB-ZFBNARCD IS INITIAL.
                SELECT ZFBNARM INTO W_TEXT2 FROM   ZTIMIMG03
                       WHERE  ZFBNARCD  EQ  IT_TAB-ZFBNARCD.

                ENDSELECT.
            ENDIF.

            WRITE :  /3 '송부처 : ', IT_TAB-ZFBLSDP, W_TEXT1,
                      33 '장치장소 : ', IT_TAB-ZFBNARCD, W_TEXT2,
                      110 'TODAY : ', SY-DATUM.

            PERFORM P3000_TITLE_WRITE.
        ENDAT.

        READ TABLE IT_T001W WITH KEY WERKS = IT_TAB-ZFWERKS.

        CLEAR W_PO.
        CONCATENATE IT_TAB-ZFREBELN '-' IT_TAB-ZFSHNO INTO W_PO.

        IF W_PO EQ '-'.
            W_PO = ''.
        ENDIF.

*>>     일반FIELD.
        FORMAT COLOR COL_NORMAL INTENSIFIED ON.
        WRITE : /
               SY-VLINE, (12) W_PO,                   "대표P/O NO.
*              SY-VLINE, (8) IT_TAB-ZFIDSDT NO-GAP, "면허일자-신고수리일
               SY-VLINE, (14) IT_TAB-ZFIDWDT,         "희망일-신고희망일
               SY-VLINE, (12) IT_TAB-ZFNEWT UNIT IT_TAB-ZFNEWTM
                                              NO-GAP,   "순중량
                         (4) IT_TAB-ZFNEWTM   NO-GAP,   "순중량단위
               SY-VLINE,(12) IT_TAB-ZFTOWT UNIT IT_TAB-ZFTOWTM
                                              NO-GAP,   "총중량
                         (4) IT_TAB-ZFTOWTM   NO-GAP,   "단위
               SY-VLINE,(12) IT_TAB-ZFPKCN_I  UNIT IT_TAB-ZFPKCNM
                                              NO-GAP,   "총포장개수
                         (4) IT_TAB-ZFPKCNM   NO-GAP,   "포장 종류
               SY-VLINE,(15) IT_TAB-ZFCARNM   NO-GAP,   "선기명
               SY-VLINE,(25) IT_T001W-NAME1   NO-GAP,   "플랜트명
               AT 130 SY-VLINE.
*>>>    HIDE
        HIDE: IT_TAB, W_TABIX.
        FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
        WRITE : /
               SY-VLINE,(30) IT_TAB-ZFRGDSR  NO-GAP,   "대표품명
               SY-VLINE,(34) IT_TAB-ZFHBLNO  NO-GAP,   "HOUSE B/L NO
               SY-VLINE,(16) IT_TAB-ZFINRNO  NO-GAP,   "반입신고번호
               SY-VLINE,(15) IT_TAB-ZFLOC    NO-GAP,   "저장위치
               SY-VLINE,(25) '     ',
               AT 130 SY-VLINE , SY-ULINE(130).
*>>>    HIDE
        HIDE: IT_TAB, W_TABIX.

*>>>    소계,총계,건수.
*>>     장치장소별
        INSERT  IT_TAB-ZFNEWT IT_TAB-ZFTOWT IT_TAB-ZFPKCN_I INTO HEADER.

        AT END OF ZFBNARCD.
            SUM.
            FORMAT COLOR 3 INTENSIFIED OFF.
            WRITE : /
                    SY-VLINE, (10) 'SUB TOTAL:'       NO-GAP,
                    SY-VLINE, (18) ''                 NO-GAP,
                    SY-VLINE, (12)  IT_TAB-ZFNEWT
                                    UNIT  IT_TAB-ZFNEWTM NO-GAP,
                               (4) ''                 NO-GAP,
                    SY-VLINE, (12) IT_TAB-ZFTOWT
                                    UNIT  IT_TAB-ZFTOWTM NO-GAP,
                               (4) ''                 NO-GAP,
                    SY-VLINE, (12) IT_TAB-ZFPKCN_I    NO-GAP,
                               (4) ''                 NO-GAP,
                    SY-VLINE, 100(4) W_LINE, '건',
                    AT 130 SY-VLINE , SY-ULINE(130).
            W_LINE = 0.
        ENDAT.

*>>     마지막에
        AT LAST.
            SUM.
            FORMAT COLOR 3 INTENSIFIED ON.
            WRITE :/ SY-ULINE(130),
                   / SY-VLINE, (10) '  TOTAL  :'       NO-GAP,
                     SY-VLINE, (18) ''                 NO-GAP,
                     SY-VLINE, (12)  IT_TAB-ZFNEWT
                                     UNIT  IT_TAB-ZFTOWTM NO-GAP,
                                (4) ''                 NO-GAP,
                     SY-VLINE, (12) IT_TAB-ZFTOWT
                                     UNIT  IT_TAB-ZFTOWTM NO-GAP,
                                (4) ''                 NO-GAP,
                     SY-VLINE, (12) IT_TAB-ZFPKCN_I    NO-GAP,
                                (4) ''                 NO-GAP,
                     SY-VLINE,
                     95 '총검색건수',(4) WA_LINE,
                     AT 130 SY-VLINE , SY-ULINE(130).
            FORMAT RESET.
        ENDAT.
  ENDLOOP.

  CLEAR: IT_TAB,W_TABIX.
ENDFORM.                    " P3000_LINE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.
    WRITE : / SY-ULINE(130).
    FORMAT COLOR COL_HEADING INTENSIFIED ON.
    WRITE : / SY-VLINE,(12)  '대표P/O NO'          CENTERED,
*             SY-VLINE,(10)  '면허일자-신고수리일' CENTERED NO-GAP,
              SY-VLINE,(14)   '신고희망일'         CENTERED,
              SY-VLINE,(12)  '순중량'              CENTERED NO-GAP,
              (4) '단위'                           CENTERED NO-GAP,
              SY-VLINE,(12)  '총중량'              CENTERED NO-GAP,
              (4) '단위'                           CENTERED NO-GAP,
              SY-VLINE,(12)  '총포장개수'          CENTERED NO-GAP,
              (4) '종류'                           CENTERED NO-GAP,
              SY-VLINE,(15)  '선 기 명'            CENTERED NO-GAP,
              SY-VLINE,(25)  '플 랜 트'            CENTERED NO-GAP,
              AT 130 SY-VLINE .
    FORMAT COLOR COL_HEADING INTENSIFIED OFF.
    WRITE : / SY-VLINE,(30)    '대표품명'          CENTERED NO-GAP,
              SY-VLINE,(34)    ' B/L NO '          CENTERED NO-GAP,
              SY-VLINE,(16)    '반입신고번호'      CENTERED NO-GAP,
              SY-VLINE,(15)    '저장위치'          CENTERED NO-GAP,
              SY-VLINE,(25)    '        '          CENTERED NO-GAP,
              AT 130 SY-VLINE, SY-ULINE(130).
ENDFORM.                    " P3000_TITLE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P2000_DISP_ZTBWHD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TAB_ZFHBLNO  text
*      -->P_IT_TAB_ZFBLNO  text
*----------------------------------------------------------------------*
FORM P2000_DISP_ZTBWHD USING    P_ZFHBLNO
                                P_ZFBLNO.
    SET PARAMETER ID 'ZPIVNO'  FIELD ''.
    SET PARAMETER ID 'ZPGISEQ' FIELD ''.
    SET PARAMETER ID 'ZPHBLNO' FIELD P_ZFHBLNO.
    SET PARAMETER ID 'ZPBLNO'  FIELD P_ZFBLNO.

   CALL TRANSACTION 'ZIMBG3'  AND SKIP  FIRST SCREEN.
ENDFORM.                    " P2000_DISP_ZTBWHD

*&---------------------------------------------------------------------*
*&      Form  P2000_DISP_ZTIV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TAB_ZFHBLNO  text
*      -->P_IT_TAB_ZFBLNO  text
*----------------------------------------------------------------------*
FORM P2000_DISP_ZTIV USING    P_ZFHBLNO
                              P_ZFBLNO.
    SET PARAMETER ID 'ZPIVNO'  FIELD ''.
    SET PARAMETER ID 'ZPHBLNO' FIELD P_ZFHBLNO.
    SET PARAMETER ID 'ZPBLNO'  FIELD P_ZFBLNO.

    CALL TRANSACTION 'ZIM33'  AND SKIP  FIRST SCREEN.
ENDFORM.                    " P2000_DISP_ZTIV

*&---------------------------------------------------------------------*
*&      Form  P2000_DISP_ZTIDR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TAB_ZFBLNO  text
*      -->P_IT_TAB_ZPHBLNO  text
*----------------------------------------------------------------------*
FORM P2000_DISP_ZTIDR USING    P_ZFBLNO
                               P_ZPHBLNO.
    SET PARAMETER ID 'ZPHBLNO' FIELD  P_ZPHBLNO.
    SET PARAMETER ID 'ZPBLNO'  FIELD  P_ZFBLNO.
    SET PARAMETER ID 'ZPCLSEQ' FIELD  ''.
    SET PARAMETER ID 'ZPIDRNO' FIELD  ''.

    CALL TRANSACTION 'ZIM63' AND SKIP  FIRST SCREEN.
ENDFORM.                    " P2000_DISP_ZTIDR

*&---------------------------------------------------------------------*
*&      Form  P2000_DISP_ZTIDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TAB_ZFBLNO  text
*      -->P_IT_TAB_ZPHBLNO  text
*----------------------------------------------------------------------*
FORM P2000_DISP_ZTIDS USING    P_ZFBLNO
                               P_ZPHBLNO.
    SET PARAMETER ID 'ZPHBLNO' FIELD  P_ZPHBLNO.
    SET PARAMETER ID 'ZPBLNO'  FIELD  P_ZFBLNO.
    SET PARAMETER ID 'ZPCLSEQ' FIELD ' '.
    SET PARAMETER ID 'ZPIDRNO' FIELD ' '.

    CALL TRANSACTION 'ZIM76' AND SKIP  FIRST SCREEN.
ENDFORM.                    " P2000_DISP_ZTIDS

*&---------------------------------------------------------------------*
*&      Form  P1000_BL_SDP_HELP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_S_BLSDP_LOW  text
*----------------------------------------------------------------------*
FORM P1000_BL_SDP_HELP USING    P_S_BLSDP.
    TABLES : ZTIMIMG08.

    DATA : DYNPROG            LIKE SY-REPID,
           DYNNR              LIKE SY-DYNNR,
           WINDOW_TITLE(30)   TYPE C,
           L_DISPLAY          TYPE C.

    DATA : BEGIN OF RETURN_TAB OCCURS 0.
           INCLUDE STRUCTURE DDSHRETVAL.

    DATA : END OF RETURN_TAB.

    DATA : BEGIN OF IT_BLSDP_HELP OCCURS 0,
           ZFBLSDP   LIKE ZTIMIMG08-ZFCD,
           ZFCDNM    LIKE ZTIMIMG08-ZFCDNM,
           END OF IT_BLSDP_HELP.

    REFRESH : IT_BLSDP_HELP.

    SELECT *
      FROM   ZTIMIMG08
     WHERE  ZFCDTY   EQ   '012'.

        MOVE : ZTIMIMG08-ZFCD   TO   IT_BLSDP_HELP-ZFBLSDP,
               ZTIMIMG08-ZFCDNM TO   IT_BLSDP_HELP-ZFCDNM.
        APPEND IT_BLSDP_HELP.
    ENDSELECT.

    IF SY-SUBRC NE 0.
        MESSAGE S406.
        EXIT.
    ENDIF.

    DYNPROG = SY-REPID.
    DYNNR   = SY-DYNNR.

    WINDOW_TITLE = 'B/L 송부처'.
    CONCATENATE WINDOW_TITLE '코드 HELP' INTO WINDOW_TITLE
                SEPARATED BY SPACE.

    CLEAR: L_DISPLAY.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
*                RETFIELD        = 'OTYPE'
                RETFIELD        = 'ZFBLSDP'
                DYNPPROG        = DYNPROG
                DYNPNR          = DYNNR
*                DYNPROFIELD     = 'ZFBLSDP'
                WINDOW_TITLE    = WINDOW_TITLE
                VALUE_ORG       = 'S'
                DISPLAY         = L_DISPLAY
        TABLES
                VALUE_TAB       = IT_BLSDP_HELP
                RETURN_TAB      = RETURN_TAB
        EXCEPTIONS
                PARAMETER_ERROR = 1
                NO_VALUES_FOUND = 2
                OTHERS          = 3.

    IF SY-SUBRC <> 0.
        EXIT.
    ENDIF.

    READ TABLE RETURN_TAB INDEX 1.
    P_S_BLSDP = RETURN_TAB-FIELDVAL.
ENDFORM.                    " P1000_BL_SDP_HELP
