*&---------------------------------------------------------------------*
*& Report          ZRIMTRORDER                                         *
*&---------------------------------------------------------------------*
*&  프로그램명 : 자재수송지시서(보세창고출고)                          *
*&      작성자 : 정승연 INFOLINK Ltd.                                  *
*&      작성일 : 2002.11.06                                            *
*&     적용회사: 한수원.
*&---------------------------------------------------------------------*
*&   DESC.     :외자 위탁차량 수송비 세부내역서 포함.
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*

PROGRAM  ZRIMTRORDER  MESSAGE-ID ZIM
                     LINE-SIZE 152
                     NO STANDARD PAGE HEADING.
*
**----------------------------------
**-------------------------------------
** Include
**----------------------------------
**-------------------------------------
*
*INCLUDE   ZRIMTRORDERTOP.
**INCLUDE   ZRIMSORTCOM.    " 수입의뢰 Report Sort를 위한 Include
*INCLUDE   ZRIMUTIL01.     " Utility function 모음.
*
**----------------------------------
**-------------------------------------
** Selection Screen .
**----------------------------------
**-------------------------------------
*SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
**>> 검색조건
*SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
*PARAMETERS :   P_ZFTRNO  LIKE  ZTTRHD-ZFTRNO
*                         OBLIGATORY MEMORY ID ZPTRNO.    " 출고번호.
*SELECTION-SCREEN END OF BLOCK B1.
*
**---------------------------------------------------------------------*
** EVENT INITIALIZATION.
**---------------------------------------------------------------------*
*INITIALIZATION.                                 " 초기값 SETTING
*  PERFORM   P2000_SET_PARAMETER.
*  SET TITLEBAR 'TRPL'.
*
*
**---------------------------------------------------------------------*
** EVENT START-OF-SELECTION.
**---------------------------------------------------------------------*
*START-OF-SELECTION.
*
*  PERFORM   P1000_READ_TEXT           USING   W_ERR_CHK.
*  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
*
** 레포트 Write
*  PERFORM   P3000_DATA_WRITE .
*  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
**  CLEAR : IT_TAB.
**----------------------------------
**-------------------------------------
** EVENT AT USER-COMMAND.
**----------------------------------
**-------------------------------------
*AT USER-COMMAND.
*
*  CASE SY-UCOMM.
**    WHEN 'STUP' OR 'STDN'.              " SORT 선택?
**      IF IT_TAB-ZFTRNO IS INITIAL.
**        MESSAGE S962.
**      ELSE.
**        W_FIELD_NM = 'ZFTRNO'.
**        ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
**        PERFORM HANDLE_SORT TABLES  IT_TAB
**                            USING   SY-UCOMM.
**      ENDIF.
**    WHEN 'MKAL' OR 'MKLO'.          " 전체 선택 및 선택해제.
**      PERFORM P2000_SELECT_RECORD   USING   SY-UCOMM.
**
*    WHEN 'REFR'.
*      PERFORM   P1000_READ_TEXT  USING W_ERR_CHK.
*      IF W_ERR_CHK EQ 'Y'.    LEAVE TO SCREEN 0.    ENDIF.
*      PERFORM   RESET_LIST.
*
*    WHEN 'AREQ'.
*      PERFORM P2000_RELEASE_REQ.
*
*    WHEN 'BAC1' OR 'EXIT' OR 'CANC'.    " 종료.
*      LEAVE TO SCREEN 0.
*    WHEN OTHERS.
*  ENDCASE.
**  CLEAR IT_TAB.
*
**&---------------------------------
**------------------------------------*
**&      Form  P2000_SET_PARAMETER
**&---------------------------------
**------------------------------------*
*FORM P2000_SET_PARAMETER.
*
*
*ENDFORM.                    " P2000_SET_PARAMETER
**&---------------------------------
**------------------------------------*
**&      Form  P1000_READ_TEXT
**&---------------------------------
**------------------------------------*
*FORM P1000_READ_TEXT USING    P_W_ERR_CHK.
*
**>> 헤더 읽기. (자재 수송지시서)
*  PERFORM P1000_READ_TRHD.
*  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
*
**>> 수송비 내역 읽기.
*  PERFORM P1000_READ_COST.
*
*ENDFORM.                    " P1000_READ_TEXT
*
**&---------------------------------
**------------------------------------*
**&      Form  P3000_DATA_WRITE
**&---------------------------------
**------------------------------------*
*FORM P3000_DATA_WRITE.
*
*  SET PF-STATUS 'TRPL'.
**>> 수송지시서 HEAD.
*  PERFORM P3000_HEAD_WRITE.
*
*  NEW-PAGE.
*
**>> 수송비 세부내역서 .
*  PERFORM P3000_COST_WRITE.
*
*ENDFORM.                    " P3000_DATA_WRITE
**&---------------------------------
**-------------------------------------
**&      Form  RESET_LIST
**&---------------------------------
**-------------------------------------
*FORM RESET_LIST.
*
*  MOVE 0 TO SY-LSIND.
*
*  W_PAGE = 1.
*  W_LINE = 1.
*  W_COUNT = 0.
** 레포트 Write
*  PERFORM   P3000_DATA_WRITE .
*
*ENDFORM.                    " RESET_LIST
**&---------------------------------
**------------------------------------*
**&      Form  P1000_READ_TRHD
**&---------------------------------
**------------------------------------*
**       text
**----------------------------------
**------------------------------------*
*FORM P1000_READ_TRHD .
*
**> 수송헤더내용.
*  SELECT SINGLE * INTO CORRESPONDING FIELDS OF ST_TAB_HD
*                  FROM ZTTRHD
*                 WHERE ZFTRNO = P_ZFTRNO.
*
*  IF SY-SUBRC NE 0.
*    W_ERR_CHK = 'Y'.
*    MESSAGE S738.
*    EXIT.
*  ENDIF.
*
**> 부가세, 전표총액.
*  SELECT SINGLE WMWST WRBTR WAERS
*           INTO (ST_TAB_HD-WMWST,
*                 ST_TAB_HD-WRBTR, ST_TAB_HD-WAERS)
*           FROM ZTBKPF
*          WHERE BUKRS = ST_TAB_HD-BUKRS
*            AND BELNR = ST_TAB_HD-BELNR
*            AND GJAHR = ST_TAB_HD-GJAHR .
*
**> 수송방법명.
*  IF NOT ST_TAB_HD-ZFDRMT IS INITIAL.
*    PERFORM   GET_DD07T_SELECT USING  'ZDDRMT'  ST_TAB_HD-ZFDRMT
*                            CHANGING   ST_TAB_HD-W_DRMT  W_SY_SUBRC.
*  ENDIF.
*
**> 운송업체명.
*  IF NOT ST_TAB_HD-ZFTRCO IS INITIAL.
*    PERFORM  P1000_GET_VENDOR   USING   ST_TAB_HD-ZFTRCO
*                             CHANGING   ST_TAB_HD-W_TRCO.
*  ENDIF.
*
**> 공급가액.
*  SELECT SUM( WRBTR ) INTO ST_TAB_HD-AMOUNT
*                      FROM ZTBSEG
*                     WHERE BUKRS = ST_TAB_HD-BUKRS
*                       AND BELNR = ST_TAB_HD-BELNR
*                       AND GJAHR = ST_TAB_HD-GJAHR .
**> 운반비.
*  SELECT SINGLE WRBTR INTO ST_TAB_HD-TRS_AMT
*                      FROM ZTBSEG
*                     WHERE BUKRS = ST_TAB_HD-BUKRS
*                       AND BELNR = ST_TAB_HD-BELNR
*                       AND GJAHR = ST_TAB_HD-GJAHR
*                       AND ZFCSTGRP = '009'
*                       AND ZFCD     = '001'.
**> 인건비.
*  SELECT SINGLE WRBTR INTO ST_TAB_HD-MAN_AMT
*                      FROM ZTBSEG
*                     WHERE BUKRS = ST_TAB_HD-BUKRS
*                       AND BELNR = ST_TAB_HD-BELNR
*                       AND GJAHR = ST_TAB_HD-GJAHR
*                       AND ZFCSTGRP = '009'
*                       AND ZFCD     = '002'.
**>기타비용.
*  ST_TAB_HD-ETC_AMT = ST_TAB_HD-AMOUNT
*                        - ( ST_TAB_HD-TRS_AMT + ST_TAB_HD-MAN_AMT ).
*
**> 대표품목.
*  SELECT SINGLE TXZ01 INTO ST_TAB_HD-TXZ01
*                      FROM ZTTRIT
*                     WHERE ZFTRNO = ST_TAB_HD-ZFTRNO.
**> 품목수.
*  SELECT COUNT( * ) INTO ST_TAB_HD-W_ITM_CN
*                    FROM ZTTRIT
*                   WHERE ZFTRNO = ST_TAB_HD-ZFTRNO.
*
*  ST_TAB_HD-W_ITM_CN = ST_TAB_HD-W_ITM_CN - 1.
*
**> 수송처.
*  SELECT DISTINCT WERKS
*             INTO CORRESPONDING FIELDS OF TABLE IT_TAB_WERKS
*             FROM ZTTRIT
*            WHERE ZFTRNO = ST_TAB_HD-ZFTRNO.
*
*  LOOP AT IT_TAB_WERKS.
*    W_TABIX  = SY-TABIX.
*    SELECT SINGLE NAME1 INTO IT_TAB_WERKS-W_WERKS
*                        FROM T001W
*                       WHERE WERKS = IT_TAB_WERKS-WERKS.
*    MODIFY IT_TAB_WERKS INDEX W_TABIX.
*  ENDLOOP.
*ENDFORM.                    " P1000_READ_TRHD
*
**&---------------------------------
**------------------------------------*
**&      Form  P1000_READ_COST
**&---------------------------------
**------------------------------------*
**       text
**----------------------------------
**------------------------------------*
*FORM P1000_READ_COST .
*
**> 세부내역.
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TAB_CST
*           FROM ZTTRCST
*          WHERE ZFTRNO = P_ZFTRNO.
*
**> 산출내역.
**>> 운반비, 인건비.
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TAB_DTL
*           FROM ZTTRCSTIT
*          WHERE ZFTRNO = P_ZFTRNO.
*
*  LOOP AT IT_TAB_DTL.
*    W_TABIX = SY-TABIX.
*    READ TABLE IT_TAB_CST WITH KEY ZFSEQ = IT_TAB_DTL-ZFSEQ.
*    IF SY-SUBRC EQ 0.
*      IT_TAB_DTL-ZFHBLNO = IT_TAB_CST-ZFHBLNO.
*      MODIFY IT_TAB_DTL INDEX W_TABIX.
*    ENDIF.
*  ENDLOOP.
*
**>> 기타비용.
*  SELECT M~ZFCDNM B~WRBTR
*          INTO CORRESPONDING FIELDS OF TABLE IT_TAB_BSEG
*          FROM ZTBSEG AS B INNER JOIN ZTIMIMG08 AS M
*            ON B~ZFCSTGRP = M~ZFCDTY
*           AND B~ZFCD     = M~ZFCD
*         WHERE B~BUKRS = ST_TAB_HD-BUKRS
*           AND B~BELNR = ST_TAB_HD-BELNR
*           AND B~GJAHR = ST_TAB_HD-GJAHR
*           AND B~ZFCSTGRP = '009'
*           AND B~ZFCD NOT IN ('001', '002').
*
*ENDFORM.                    " P1000_READ_COST
**&---------------------------------
**------------------------------------*
**&      Form  P3000_HEAD_WRITE
**&---------------------------------
**------------------------------------*
*FORM P3000_HEAD_WRITE.
*
*  SKIP 2.
*  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
*  WRITE : /70  '[ 자재수송지시서 ]'
*               COLOR COL_HEADING INTENSIFIED OFF.
*
*  SKIP 2.
*  WRITE : 102 SY-ULINE,
*         /102 SY-VLINE NO-GAP, (4) '결'     CENTERED NO-GAP,
*              SY-VLINE NO-GAP, (14) '직  원' CENTERED NO-GAP,
*              SY-VLINE NO-GAP, (14) '과  장' CENTERED NO-GAP,
*              SY-VLINE NO-GAP, (14) '부  장' CENTERED NO-GAP,
*              SY-VLINE NO-GAP,
*         /102 SY-VLINE NO-GAP, (4) '  ' CENTERED NO-GAP,
*          107 SY-ULINE NO-GAP,
*         /102 SY-VLINE NO-GAP, (4) '  ' CENTERED NO-GAP,
*              SY-VLINE NO-GAP, (14) '  ' CENTERED NO-GAP,
*              SY-VLINE NO-GAP, (14) '  ' CENTERED NO-GAP,
*              SY-VLINE NO-GAP, (14) '  ' CENTERED NO-GAP,
*              SY-VLINE NO-GAP,
*         /102 SY-VLINE NO-GAP, (4) '  ' CENTERED NO-GAP,
*              SY-VLINE NO-GAP, (14) '  ' CENTERED NO-GAP,
*              SY-VLINE NO-GAP, (14) '  ' CENTERED NO-GAP,
*              SY-VLINE NO-GAP, (14) '  ' CENTERED NO-GAP,
*              SY-VLINE NO-GAP,
*         /102 SY-VLINE NO-GAP, (4) '재' CENTERED NO-GAP,
*              SY-VLINE NO-GAP, (14) '  ' CENTERED NO-GAP,
*              SY-VLINE NO-GAP, (14) '  ' CENTERED NO-GAP,
*              SY-VLINE NO-GAP, (14) '  ' CENTERED NO-GAP,
*              SY-VLINE NO-GAP,
*          / ' 수송지시번호 : ' NO-GAP,
*              ST_TAB_HD-ZFTRNO NO-GAP,
*          102 SY-ULINE.
*  SKIP 2 .
*  WRITE AT (MAX-LINE) SY-ULINE.
**> 1.
*  WRITE : / SY-VLINE NO-GAP, (20) '지  시   내  용' CENTERED NO-GAP,
*            SY-VLINE NO-GAP,
*           ' 아래와 같이(붙임 증표상의) 자재 수송작업을 지시합니다.'.
*  WRITE AT MAX-LINE SY-VLINE NO-GAP.
*  WRITE AT (MAX-LINE) SY-ULINE.
**> 2.
*  DATA : W_LEN TYPE I.
*  W_LEN = STRLEN( ST_TAB_HD-TXZ01 ).
*  WRITE : / SY-VLINE NO-GAP, (20) '수  송   품  목' CENTERED NO-GAP,
*            SY-VLINE NO-GAP,
*        AT (W_LEN) ST_TAB_HD-TXZ01, ' 외 ' NO-GAP,
*       (05) ST_TAB_HD-W_ITM_CN NO-GAP, '종' NO-GAP.
*  WRITE AT MAX-LINE SY-VLINE NO-GAP.
*  WRITE AT (MAX-LINE) SY-ULINE.
**> 3.
*  WRITE : / SY-VLINE NO-GAP, (20) '중           량' CENTERED NO-GAP,
*            SY-VLINE NO-GAP,
*       (19) ST_TAB_HD-ZFTOWT UNIT ST_TAB_HD-ZFTOWTM
*                             RIGHT-JUSTIFIED NO-GAP NO-ZERO,
*       (03) ST_TAB_HD-ZFTOWTM NO-GAP, SY-VLINE NO-GAP,
*       (20) '수  송   구  간' CENTERED NO-GAP, SY-VLINE NO-GAP,
*            '고리본부 -> ' NO-GAP.
**-----< 수송구간 쓰기 >--------------------->>
*  LOOP AT IT_TAB_WERKS.
*    IF SY-TABIX NE '1'.
*      WRITE : ', ' NO-GAP.
*    ENDIF.
*    WRITE : IT_TAB_WERKS-W_WERKS NO-GAP.
*  ENDLOOP.
**------------------------------------------>>
*  WRITE AT MAX-LINE SY-VLINE NO-GAP.
*  WRITE AT (MAX-LINE) SY-ULINE.
**>4
*  WRITE : / SY-VLINE NO-GAP, (20) '수  송   기  간' CENTERED NO-GAP,
*            SY-VLINE NO-GAP,
*       (10) ST_TAB_HD-ZFGIDT NO-GAP, ' ~ ' NO-GAP,
*       (10) ST_TAB_HD-ZFDRDT NO-GAP,
*        66  SY-VLINE NO-GAP,
*       (20) '수  송   방  법' CENTERED NO-GAP, SY-VLINE NO-GAP,
*            ST_TAB_HD-W_DRMT NO-GAP.
*  WRITE AT MAX-LINE SY-VLINE NO-GAP.
*  WRITE AT (MAX-LINE) SY-ULINE.
**>5
*  WRITE : / SY-VLINE NO-GAP, (20) '수  송   업  체' CENTERED NO-GAP,
*            SY-VLINE NO-GAP,
*       (43) ST_TAB_HD-W_TRCO NO-GAP,
*        66  SY-VLINE NO-GAP,
*       (20) '작  업   조  건' CENTERED NO-GAP, SY-VLINE NO-GAP,
*       (50) ST_TAB_HD-ZFTRTERM NO-GAP.
*  WRITE AT MAX-LINE SY-VLINE NO-GAP.
*  WRITE AT (MAX-LINE) SY-ULINE.
**>6~7
*  WRITE : / SY-VLINE NO-GAP, (20) ' ' NO-GAP, SY-VLINE NO-GAP,
*       (20) '공급가액' CENTERED NO-GAP, SY-VLINE NO-GAP,
*       (17) ST_TAB_HD-AMOUNT CURRENCY ST_TAB_HD-WAERS
*                             RIGHT-JUSTIFIED NO-GAP,
*       (05) ST_TAB_HD-WAERS  NO-GAP, SY-VLINE NO-GAP,
*       (20) '부 가 세' CENTERED NO-GAP, SY-VLINE NO-GAP,
*       (17) ST_TAB_HD-WMWST  CURRENCY ST_TAB_HD-WAERS
*                             RIGHT-JUSTIFIED NO-GAP,
*       (05) ST_TAB_HD-WAERS  NO-GAP, SY-VLINE NO-GAP,
*       (19) '총    액' CENTERED NO-GAP, SY-VLINE NO-GAP,
*       (17) ST_TAB_HD-WRBTR  CURRENCY ST_TAB_HD-WAERS
*                             RIGHT-JUSTIFIED NO-GAP,
*       (05) ST_TAB_HD-WAERS  NO-GAP.
*  WRITE AT MAX-LINE SY-VLINE NO-GAP.
*  WRITE : / SY-VLINE NO-GAP, (20) '수    송    비' CENTERED NO-GAP,
*            SY-VLINE NO-GAP.
*  WRITE AT 22 SY-ULINE.
*  WRITE : / SY-VLINE NO-GAP, (20) ' ' NO-GAP, SY-VLINE NO-GAP,
*            '공급가 내역 (운반비: ' NO-GAP,
*        (16) ST_TAB_HD-TRS_AMT  CURRENCY ST_TAB_HD-WAERS
*                                  RIGHT-JUSTIFIED NO-GAP,
*            '   인건비: ' NO-GAP,
*        (16) ST_TAB_HD-MAN_AMT  CURRENCY ST_TAB_HD-WAERS
*                                  RIGHT-JUSTIFIED NO-GAP,
*            '   기타비용 : ' NO-GAP,
*        (16) ST_TAB_HD-ETC_AMT  CURRENCY ST_TAB_HD-WAERS
*                                  RIGHT-JUSTIFIED NO-GAP,
*            ' )' NO-GAP.
*  WRITE AT MAX-LINE SY-VLINE NO-GAP.
*  WRITE AT (MAX-LINE) SY-ULINE.
**>8~12
*  WRITE : / SY-VLINE NO-GAP, (20) '작          업' CENTERED NO-GAP,
*            SY-VLINE NO-GAP,  ST_TAB_HD-ZFRMK1 NO-GAP.
*  WRITE AT MAX-LINE SY-VLINE NO-GAP.
*  WRITE : / SY-VLINE NO-GAP, (20) '및'    CENTERED NO-GAP,
*            SY-VLINE NO-GAP,  ST_TAB_HD-ZFRMK2 NO-GAP.
*  WRITE AT MAX-LINE SY-VLINE NO-GAP.
*  WRITE : / SY-VLINE NO-GAP, (20) '운    송    시' CENTERED NO-GAP,
*            SY-VLINE NO-GAP,  ST_TAB_HD-ZFRMK3 NO-GAP.
*  WRITE AT MAX-LINE SY-VLINE NO-GAP.
*  WRITE : / SY-VLINE NO-GAP, (20) '지          시' CENTERED NO-GAP,
*            SY-VLINE NO-GAP,  ST_TAB_HD-ZFRMK4 NO-GAP.
*  WRITE AT MAX-LINE SY-VLINE NO-GAP.
*  WRITE : / SY-VLINE NO-GAP, (20) '사          항' CENTERED NO-GAP,
*            SY-VLINE NO-GAP,  ST_TAB_HD-ZFRMK5 NO-GAP.
*  WRITE AT MAX-LINE SY-VLINE NO-GAP.
*  WRITE AT (MAX-LINE) SY-ULINE.
**> 이하..
*  WRITE : / SY-VLINE NO-GAP.
*  WRITE AT MAX-LINE SY-VLINE NO-GAP.
*  WRITE : / SY-VLINE NO-GAP.
*  WRITE AT MAX-LINE SY-VLINE NO-GAP.
*  WRITE : / SY-VLINE NO-GAP,
*      (150) '이와같이 수송 작업을 지시함.'     CENTERED NO-GAP.
*  WRITE AT MAX-LINE SY-VLINE NO-GAP.
*  WRITE : / SY-VLINE NO-GAP.
*  WRITE AT MAX-LINE SY-VLINE NO-GAP.
*  WRITE : / SY-VLINE NO-GAP.
*  WRITE AT MAX-LINE SY-VLINE NO-GAP.
*  WRITE : / SY-VLINE NO-GAP,
*      (150) '2002년     월     일'             CENTERED NO-GAP.
*  WRITE AT MAX-LINE SY-VLINE NO-GAP.
*  WRITE : / SY-VLINE NO-GAP.
*  WRITE AT MAX-LINE SY-VLINE NO-GAP.
*  WRITE : / SY-VLINE NO-GAP.
*  WRITE AT MAX-LINE SY-VLINE NO-GAP.
*  WRITE : / SY-VLINE NO-GAP,
*      (150) '한 국 수 력 원 자 력 고 리 본 부' CENTERED NO-GAP.
*  WRITE AT MAX-LINE SY-VLINE NO-GAP.
*  WRITE : / SY-VLINE NO-GAP.
*  WRITE AT MAX-LINE SY-VLINE NO-GAP.
*  WRITE : / SY-VLINE NO-GAP.
*  WRITE AT MAX-LINE SY-VLINE NO-GAP.
*  WRITE : / SY-VLINE NO-GAP,
*      (150) '자재부장                  (인)'   CENTERED NO-GAP.
*  WRITE AT MAX-LINE SY-VLINE NO-GAP.
*  WRITE : / SY-VLINE NO-GAP.
*  WRITE AT MAX-LINE SY-VLINE NO-GAP.
*  WRITE : / SY-VLINE NO-GAP.
*  WRITE AT MAX-LINE SY-VLINE NO-GAP.
*  WRITE AT (MAX-LINE) SY-ULINE.
**> TAIL.
*  WRITE : / SY-VLINE NO-GAP,
*       (49) ' ' CENTERED NO-GAP, SY-VLINE NO-GAP,
*       (50) ' ' CENTERED NO-GAP, SY-VLINE NO-GAP,
*       (49) '자재부 외자인수과' CENTERED NO-GAP, SY-VLINE NO-GAP.
*  WRITE AT MAX-LINE SY-VLINE NO-GAP.
*  WRITE : / SY-VLINE NO-GAP,
*       (49) '첨부 : 발송증표      매' CENTERED NO-GAP,
*             SY-VLINE NO-GAP,
*       (50) '작업지시처' CENTERED NO-GAP, SY-VLINE NO-GAP,
*       (49) '          ' CENTERED NO-GAP, SY-VLINE NO-GAP.
*  WRITE AT MAX-LINE SY-VLINE NO-GAP.
*  WRITE : / SY-VLINE NO-GAP,
*       (49) ' ' CENTERED NO-GAP, SY-VLINE NO-GAP,
*       (50) ' ' CENTERED NO-GAP, SY-VLINE NO-GAP,
*       (49) '015-726-2836' CENTERED NO-GAP, SY-VLINE NO-GAP.
*  WRITE AT MAX-LINE SY-VLINE NO-GAP.
*  WRITE AT (MAX-LINE) SY-ULINE.
*
*ENDFORM.                    " P3000_HEAD_WRITE
**&---------------------------------
**------------------------------------*
**&      Form  P3000_COST_WRITE
**&---------------------------------
**------------------------------------*
*FORM P3000_COST_WRITE .
*
*  SKIP 2.
*  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
*  WRITE : /70  '[ 외자 위탁차량 수송비 세부내역서 ]'
*               COLOR COL_HEADING INTENSIFIED OFF.
*
*  SKIP 2.
**================< 산출 내역 >=============>>>>>
*  WRITE : / '1) 산출 내역 '.
*  ULINE.
*  WRITE : / SY-VLINE NO-GAP, (24) ' '             CENTERED NO-GAP,
*            SY-VLINE NO-GAP, (07) ' '             CENTERED NO-GAP,
*            SY-VLINE NO-GAP, (07) '적  용'        CENTERED NO-GAP,
*            SY-VLINE NO-GAP, (37) '운   반   비'  CENTERED NO-GAP,
*            SY-VLINE NO-GAP, (37) '인   건   비'  CENTERED NO-GAP,
*            SY-VLINE NO-GAP, (16) ' '             CENTERED NO-GAP,
*            SY-VLINE NO-GAP, (16) ' '             CENTERED NO-GAP,
*            SY-VLINE NO-GAP.
*  WRITE : / SY-VLINE NO-GAP, (24) 'House B/L No.' CENTERED NO-GAP,
*            SY-VLINE NO-GAP, (07) '내  역'        CENTERED NO-GAP,
*            SY-VLINE NO-GAP, (07) ' '             CENTERED NO-GAP,
*       (76) SY-ULINE NO-GAP,
*            SY-VLINE NO-GAP, (16) '기타금액'      CENTERED NO-GAP,
*            SY-VLINE NO-GAP, (16) '총 합계'       CENTERED NO-GAP,
*            SY-VLINE NO-GAP.
*  WRITE : / SY-VLINE NO-GAP, (24) ' '             CENTERED NO-GAP,
*            SY-VLINE NO-GAP, (07) ' '             CENTERED NO-GAP,
*            SY-VLINE NO-GAP, (07) '톤  수'        CENTERED NO-GAP,
*            SY-VLINE NO-GAP, (15) '단   가'       CENTERED NO-GAP,
*            SY-VLINE NO-GAP, (04) '할증'          CENTERED NO-GAP,
*            SY-VLINE NO-GAP, (16) '금   액'       CENTERED NO-GAP,
*            SY-VLINE NO-GAP, (15) '단   가'       CENTERED NO-GAP,
*            SY-VLINE NO-GAP, (04) '할증'          CENTERED NO-GAP,
*            SY-VLINE NO-GAP, (16) '금   액'        CENTERED NO-GAP,
*            SY-VLINE NO-GAP, (16) ' '             CENTERED NO-GAP,
*            SY-VLINE NO-GAP, (16) ' '             CENTERED NO-GAP,
*            SY-VLINE NO-GAP.
*  ULINE.
**> 산출내역 WRITE.
*  PERFORM P3000_CALC_WRITE.
*
**================< 세부 내역 >=============>>>>>
*  SKIP 2.
*  WRITE : / '2) 세부 내역 '.
*  ULINE.
*  WRITE : / SY-VLINE NO-GAP, (24) ' '             CENTERED NO-GAP,
*            SY-VLINE NO-GAP, (71) '규        격'  CENTERED NO-GAP,
*            SY-VLINE NO-GAP, (17) ' '             CENTERED NO-GAP,
*            SY-VLINE NO-GAP, (17) ' '             CENTERED NO-GAP,
*            SY-VLINE NO-GAP, (17) ' '             CENTERED NO-GAP,
*            SY-VLINE NO-GAP.
*  WRITE : / SY-VLINE NO-GAP, (24) 'House B/L No.' CENTERED NO-GAP,
*       (72) SY-ULINE NO-GAP,
*            SY-VLINE NO-GAP, (17) '용적톤'        CENTERED NO-GAP,
*            SY-VLINE NO-GAP, (17) '중량톤'        CENTERED NO-GAP,
*            SY-VLINE NO-GAP, (17) '계산톤'        CENTERED NO-GAP,
*            SY-VLINE NO-GAP.
*  WRITE : / SY-VLINE NO-GAP, (24) ' '             CENTERED NO-GAP,
*            SY-VLINE NO-GAP, (17) '가    로'      CENTERED NO-GAP,
*            SY-VLINE NO-GAP, (17) '세    로'      CENTERED NO-GAP,
*            SY-VLINE NO-GAP, (17) '높    이'      CENTERED NO-GAP,
*            SY-VLINE NO-GAP, (17) '실 용 적'      CENTERED NO-GAP,
*            SY-VLINE NO-GAP, (17) ' '             CENTERED NO-GAP,
*            SY-VLINE NO-GAP, (17) ' '             CENTERED NO-GAP,
*            SY-VLINE NO-GAP, (17) ' '             CENTERED NO-GAP,
*            SY-VLINE NO-GAP.
*  ULINE.
*
**> 산출내역 WRITE.
*  PERFORM P3000_DETAIL_WRITE.
*
*
*ENDFORM.                    " P3000_COST_WRITE
*
**&---------------------------------
**------------------------------------*
**&      Form  P3000_CALC_WRITE
**&---------------------------------
**------------------------------------*
**       text
**----------------------------------
**------------------------------------*
*FORM P3000_CALC_WRITE .
*
*  CLEAR: W_TRS_TOT, W_MAN_TOT, W_ROW_TOT, W_ETC_TOT,
*         W_GRD_TOT, W_WAERS.
*
**>> 운반비, 인건비.
*  LOOP AT IT_TAB_DTL.
*    W_WAERS = IT_TAB_DTL-WAERS.
*    WRITE : / SY-VLINE NO-GAP, (24) IT_TAB_DTL-ZFHBLNO    NO-GAP,
*              SY-VLINE NO-GAP,
*        (06) IT_TAB_DTL-ZFTRATE  RIGHT-JUSTIFIED NO-GAP, '%' NO-GAP,
*              SY-VLINE NO-GAP,
*        (07) IT_TAB_DTL-ZFDTON  RIGHT-JUSTIFIED NO-GAP,
*              SY-VLINE NO-GAP,
*        (15) IT_TAB_DTL-NETPR   CURRENCY IT_TAB_DTL-WAERS
*                                RIGHT-JUSTIFIED NO-GAP,
*              SY-VLINE NO-GAP,
*        (04) IT_TAB_DTL-ZFTADD  RIGHT-JUSTIFIED NO-GAP,
*              SY-VLINE NO-GAP,
*        (16) IT_TAB_DTL-ZFTRAMT CURRENCY IT_TAB_DTL-WAERS
*                                RIGHT-JUSTIFIED NO-GAP,
*              SY-VLINE NO-GAP,
*        (15) IT_TAB_DTL-NETPR   CURRENCY IT_TAB_DTL-WAERS
*                                RIGHT-JUSTIFIED NO-GAP,
*              SY-VLINE NO-GAP,
*        (04) IT_TAB_DTL-ZFMADD  RIGHT-JUSTIFIED NO-GAP,
*              SY-VLINE NO-GAP,
*        (16) IT_TAB_DTL-ZFMAMT CURRENCY IT_TAB_DTL-WAERS
*                                RIGHT-JUSTIFIED NO-GAP,
*              SY-VLINE NO-GAP, (16) ' '        NO-GAP.
**--> SUM.
*    W_TRS_TOT = W_TRS_TOT + IT_TAB_DTL-ZFTRAMT.  " 운반비 합계.
*    W_MAN_TOT = W_MAN_TOT + IT_TAB_DTL-ZFMAMT.   " 인건비 합계.
*    W_ROW_TOT = IT_TAB_DTL-ZFTRAMT + IT_TAB_DTL-ZFMAMT. "ROW SUM.
*
*    WRITE : SY-VLINE NO-GAP,
*        (16)  W_ROW_TOT        CURRENCY IT_TAB_DTL-WAERS
*                                RIGHT-JUSTIFIED NO-GAP,
*            SY-VLINE NO-GAP.
*    ULINE.
*    CLEAR : W_ROW_TOT.
*  ENDLOOP.
*
**>> 기타비용.
*  LOOP AT IT_TAB_BSEG.
*    WRITE : / SY-VLINE NO-GAP, (32) IT_TAB_BSEG-ZFCDNM NO-GAP,
*              SY-VLINE NO-GAP, (07) ' '                NO-GAP,
*              SY-VLINE NO-GAP, (15) ' '                NO-GAP,
*              SY-VLINE NO-GAP, (04) ' '                NO-GAP,
*              SY-VLINE NO-GAP, (16) ' '                NO-GAP,
*              SY-VLINE NO-GAP, (15) ' '                NO-GAP,
*              SY-VLINE NO-GAP, (04) ' '                NO-GAP,
*              SY-VLINE NO-GAP, (16) ' '                NO-GAP,
*              SY-VLINE NO-GAP,
*        (16) IT_TAB_BSEG-WRBTR  CURRENCY W_WAERS
*                                RIGHT-JUSTIFIED NO-GAP,
*              SY-VLINE NO-GAP,
*        (16) IT_TAB_BSEG-WRBTR  CURRENCY W_WAERS
*                                RIGHT-JUSTIFIED NO-GAP,
*              SY-VLINE NO-GAP.
**--> SUM.
*    W_ETC_TOT = W_ETC_TOT + IT_TAB_BSEG-WRBTR.
*
*    ULINE.
*  ENDLOOP.
*
**====>>> TOTOAL.
*  W_GRD_TOT = W_TRS_TOT + W_MAN_TOT + W_ETC_TOT.
*
*  FORMAT COLOR COL_TOTAL INTENSIFIED ON.
*  WRITE : / SY-VLINE NO-GAP, (40) '총      계'    CENTERED NO-GAP,
*            SY-VLINE NO-GAP,
*      (37) W_TRS_TOT CURRENCY W_WAERS  RIGHT-JUSTIFIED NO-GAP,
*            SY-VLINE NO-GAP,
*      (37) W_MAN_TOT CURRENCY W_WAERS  RIGHT-JUSTIFIED NO-GAP,
*            SY-VLINE NO-GAP,
*      (16) W_ETC_TOT CURRENCY W_WAERS  RIGHT-JUSTIFIED NO-GAP,
*            SY-VLINE NO-GAP,
*      (16) W_GRD_TOT CURRENCY W_WAERS  RIGHT-JUSTIFIED NO-GAP,
*            SY-VLINE NO-GAP.
*  ULINE.
*  FORMAT RESET.
*ENDFORM.                    " P3000_CALC_WRITE
**&---------------------------------
**------------------------------------*
**&      Form  P3000_DETAIL_WRITE
**&---------------------------------
**------------------------------------*
**       text
**----------------------------------
**------------------------------------*
*FORM P3000_DETAIL_WRITE .
*
*  LOOP AT IT_TAB_CST.
*    WRITE : / SY-VLINE NO-GAP,
*         (24) IT_TAB_CST-ZFHBLNO                  NO-GAP,
*              SY-VLINE NO-GAP,
*         (17) IT_TAB_CST-ZFGARO  RIGHT-JUSTIFIED  NO-GAP,
*              SY-VLINE NO-GAP,
*         (17) IT_TAB_CST-ZFSERO  RIGHT-JUSTIFIED  NO-GAP,
*              SY-VLINE NO-GAP,
*         (17) IT_TAB_CST-ZFNOPI  RIGHT-JUSTIFIED  NO-GAP,
*              SY-VLINE NO-GAP,
*         (17) IT_TAB_CST-ZFRWET  RIGHT-JUSTIFIED  NO-GAP,
*              SY-VLINE NO-GAP,
*         (17) IT_TAB_CST-ZFYTON  RIGHT-JUSTIFIED  NO-GAP,
*              SY-VLINE NO-GAP,
*         (17) IT_TAB_CST-ZFWTON  RIGHT-JUSTIFIED  NO-GAP,
*              SY-VLINE NO-GAP,
*         (17) IT_TAB_CST-ZFCTON  RIGHT-JUSTIFIED  NO-GAP,
*              SY-VLINE NO-GAP.
*    ULINE.
*  ENDLOOP.
*ENDFORM.                    " P3000_DETAIL_WRITE
*
**&---------------------------------
**------------------------------------*
**&      Form  P1000_GET_VENDOR
**&---------------------------------
**------------------------------------*
*FORM P1000_GET_VENDOR USING    P_LIFNR
*                      CHANGING P_NAME1.
*  DATA : L_TEXT(35).
*
*  CLEAR : P_NAME1, W_LFA1.
*  IF P_LIFNR IS INITIAL.
*    EXIT.
*  ENDIF.
*
** VENDOR MASTER SELECT( LFA1 )----------------------->
*  CALL FUNCTION 'READ_LFA1'
*    EXPORTING
*      XLIFNR         = P_LIFNR
*    IMPORTING
*      XLFA1          = W_LFA1
*    EXCEPTIONS
*      KEY_INCOMPLETE = 01
*      NOT_AUTHORIZED = 02
*      NOT_FOUND      = 03.
*
*  CASE SY-SUBRC.
*    WHEN 01.     MESSAGE I025.
*    WHEN 02.     MESSAGE E950.
*    WHEN 03.     MESSAGE E020   WITH    P_LIFNR.
*  ENDCASE.
**   TRANSLATE W_LFA1 TO UPPER CASE.
*  MOVE: W_LFA1-NAME1   TO   L_TEXT.
*  TRANSLATE L_TEXT TO UPPER CASE.
*  P_NAME1 = L_TEXT.
*
*ENDFORM.                    " P1000_GET_VENDOR
**&---------------------------------
**------------------------------------*
**&      Form  P2000_RELEASE_REQ
**&---------------------------------
**------------------------------------*
**       text
**----------------------------------
**------------------------------------*
*FORM P2000_RELEASE_REQ .
*
**  DATA: rel_indicator LIKE eban-frgkz.
** SELECT SINGLE frgkz INTO rel_indicator
**    FROM eban
**    WHERE banfn = io_docno AND
**          frgkz <> 'X'.    "결재지사자
*
**  DATA: WL_RELST LIKE ZTTRHD-ZFRELST.
**  CLEAR WL_RELST.
**  SELECT SINGLE ZFRELST INTO WL_RELST
**     FROM ZTTRHD
**     WHERE ZFTRNO = P_ZFTRNO.
**
**    CASE WL_RELST.
**     WHEN 'W'.
**      MESSAGE I999(ZIM1)
**         WITH '이 문서는 한번 이상 결재 요청된 문서입니다. '.
**     WHEN 'D'.
**      MESSAGE I999(ZIM1)
**         WITH '이 문서는 이미 결재 완료된 문서입니다. '.
**    ENDCASE.
*
**  DATA: APPTYPE LIKE ZWF00-APPTYPE, "전자결재업무구분 NCW막음APPTYPE
**  DATA: BUKRS   LIKE ZWF00-BUKRS,   "Company Code
**        GJAHR   LIKE ZWF00-GJAHR,
**        "관리번호: 전자결재를 요청한 원래문서의 일련번호
**        CTRLNO  LIKE ZWF00-CTRLNO,
**        "관리번호: 전자결재를 요청한 원래문서의 Lineitem No.
**        CTRLPG  TYPE ZZ_CTRLPG,
**        "관리번호: 전자결재를 요청한 원래문서의 Lineitem No.
**        KEY(10).
*
**  APPTYPE = 'MTR'.
**  CTRLNO = P_ZFTRNO.
*
**  CALL FUNCTION 'Z_WF_APPROVAL_REQUEST'       "NCW 막음
**    EXPORTING
**      APPTYPE            = APPTYPE          "결재문서 유형
**      CTRLNO             = CTRLNO           "일련번호
**    EXCEPTIONS
**      LOGIN_FAILED       = 1
**      APPROVER_NOT_FOUND = 2
**      DATA_NOT_FOUND     = 3
**      DATA_DUPLICATED    = 4
**      ERROR_FOUND        = 5
**      OTHERS             = 6.
*
*  IF SY-SUBRC <> 0.
*    CASE SY-SUBRC.
*      WHEN 1.      MESSAGE I999(ZMMM) WITH 'LOGIN_FAILED'.
*      WHEN 2.      MESSAGE I999(ZMMM) WITH 'APPROVER_NOT_FOUND'.
*      WHEN 3.      MESSAGE I999(ZMMM) WITH 'DATA_NOT_FOUND'.
*      WHEN 4.      MESSAGE I999(ZMMM) WITH 'DATA_DUPLICATED'.
*      WHEN 5.      MESSAGE I999(ZMMM) WITH 'ERROR_FOUND'.
*      WHEN OTHERS. MESSAGE I999(ZMMM) WITH 'OTHERS'.
*    ENDCASE.
*  ELSE.
*    UPDATE ZTTRHD SET ZFRELST = 'W' WHERE ZFTRNO = P_ZFTRNO.
*    MESSAGE S999(ZIM1) WITH '결재 요청 되었습니다.'.
*  ENDIF.
*
*ENDFORM.                    " P2000_RELEASE_REQ
