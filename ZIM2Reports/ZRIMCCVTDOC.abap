*&---------------------------------------------------------------------
*& Report  ZRIMCCVTDOC
*&---------------------------------------------------------------------
*&  프로그램명 : 수입 관세/부가세/통관료 일계표.
*&      작성자 : 이채경 INFOLINK Ltd.
*&      작성일 : 2001.10.06
*&---------------------------------------------------------------------
*&   DESC.     :
*&
*&---------------------------------------------------------------------
*& [변경내용]
*&
*&---------------------------------------------------------------------
REPORT  ZRIMCCVTDOC  MESSAGE-ID ZIM
                     LINE-SIZE 195
                     NO STANDARD PAGE HEADING.

TABLES: ZTBKPF,
        ZTBSEG,
        ZTBL,
        ZTIDS,
        ZTIMIMG10,
        LFA1,
        ZTIV,
        ZTCUCLIV,
        T001,
        ZTIMIMG02.

*----------------------------------------------------------------------
*  리스트용 INTERNAL TABLE
*----------------------------------------------------------------------

DATA : BEGIN OF IT_TAB OCCURS 0,
       BUKRS     LIKE    ZTBKPF-BUKRS,
       BELNR     LIKE    ZTBKPF-BELNR,
       GJAHR     LIKE    ZTBKPF-GJAHR,
       ZFIMDNO   LIKE    ZTBSEG-ZFIMDNO,    " 수입문서번호.
       ZFACDO    LIKE    ZTBKPF-ZFACDO,      " 회계전표번호.
       WAERS     LIKE    ZTBKPF-WAERS,
       HWAER     LIKE    ZTBKPF-HWAER,
       ZFREBELN  LIKE    ZTBL-ZFREBELN,      " P/O NO
       ZFSHNO    LIKE    ZTBL-ZFSHNO,        " 선적차수.
       ZFHBLNO   LIKE    ZTBL-ZFHBLNO,       " B/L NO
       GUBUN     TYPE C,
       ZFCD      LIKE    ZTBSEG-ZFCD,        " 관리코드.
       LIFNR     LIKE    ZTBKPF-LIFNR,       " 구매처.
       NAME1     LIKE    LFA1-NAME1,         " 거래선.
       BUDAT     LIKE    ZTBKPF-BUDAT,       " 전기일.
       ZFBDT     LIKE    ZTBKPF-ZFBDT,       " 지불일.
       ZFRVSX    LIKE    ZTBKPF-ZFRVSX,      " 역기표.
       ZFCSTGRP  LIKE    ZTBSEG-ZFCSTGRP,    " 비용그룹.
       WRBTR     LIKE    ZTBSEG-WRBTR,
       DMBTR     LIKE    ZTBSEG-DMBTR,
       WMWST     LIKE    ZTBSEG-WMWST,       ">부가세.
       FWBAS     LIKE    ZTBSEG-FWBAS,       " 부가세과표액.
       CSTGRP    LIKE    DD07T-DDTEXT,
       BUPLA     LIKE    ZTBKPF-BUPLA,       " 귀속사업장.
       CPUDT     LIKE    ZTBKPF-CPUDT,       " 전표입력일.
       CPUTM     LIKE    ZTBKPF-CPUTM,       " 전표입력시간.
       UTME      LIKE    ZTBKPF-UTME,        " 최종변경시간.
       ZFCNAME   LIKE    ZTBKPF-ZFCNAME,     " 담당자.
       ZFIDRNO   LIKE    ZTIDS-ZFIDRNO,
       ZFCUTAMT  LIKE    ZTIDS-ZFCUTAMT,     " 관세사 수수?
       ZFCUAMTS  LIKE    ZTIDS-ZFCUAMTS,     " 총관?
       ZFVAAMTS  LIKE    ZTIDS-ZFVAAMTS,     " 총부가?
       ZFIDSDT   LIKE    ZTIDS-ZFIDSDT,      " 신고일.
       ZFCOCD	   LIKE    ZTIDS-ZFCOCD.       " 관세징수형태.
DATA : END OF IT_TAB.

DATA : W_STCD2   LIKE    LFA1-STCD2,         " 사업자등록번호.
       W_ZFACTNO LIKE    ZTIMIMG02-ZFACTNO.  " 계좌번호.

DATA :  W_ERR_CHK     TYPE C,
        W_LCOUNT      TYPE I,
        W_FIELD_NM    TYPE C,
        W_PAGE        TYPE I,
        W_CHECK_PAGE(1) TYPE C,
        W_LINE        TYPE I,
        W_COUNT       TYPE I,
        W_TABIX       LIKE SY-TABIX.

*----------------------------------------------------------------------
* Selection Screen ?
*----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

   SELECT-OPTIONS: S_BUKRS   FOR  ZTBKPF-BUKRS NO-EXTENSION
                                               NO INTERVALS,
                   S_DAT     FOR  ZTBKPF-CPUDT NO-EXTENSION NO INTERVALS
                                           OBLIGATORY ,
                   S_TIME    FOR  ZTBKPF-CPUTM,
                   S_CNAME   FOR  ZTBKPF-ZFCNAME NO-EXTENSION
                                                 NO INTERVALS.
                                                 "OBLIGATORY.

 SELECTION-SCREEN END OF BLOCK B1.
 INITIALIZATION.                          " 초기값 SETTING
   PERFORM   P2000_INIT.

*title Text Write
W_CHECK_PAGE = 'X'.
TOP-OF-PAGE.
 PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...

*----------------------------------------------------------------------
* START OF SELECTION ?
*----------------------------------------------------------------------
START-OF-SELECTION.
* 권한 검증 함?
   PERFORM   P2000_AUTHORITY_CHECK     USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
* B/L 테이블 SELECT
   PERFORM   P1000_READ_DATA    USING  W_ERR_CHK.
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
     WHEN 'DISP'.
       IF W_TABIX IS INITIAL.
          MESSAGE S962.
       ELSE.
          IF NOT IT_TAB-BELNR IS INITIAL.
              PERFORM P2000_DISPLAY_COST_DOCUMENT USING  IT_TAB-BUKRS
                                                         IT_TAB-GJAHR
                                                         IT_TAB-BELNR.
          ELSE.
             MESSAGE S962.
          ENDIF.

       ENDIF.
   ENDCASE.
   CLEAR : W_TABIX, IT_TAB.

*&---------------------------------------------------------------------
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------
FORM P3000_TITLE_WRITE.

  SKIP 2.
  WRITE:/80 '수입관세/부가세/통관료 일계표'.
  SKIP 2.

*    ULINE AT 169(195).
*     WRITE:/169 SY-VLINE NO-GAP, 170 '결제'  CENTERED,
*            174 SY-VLINE, 175 '기  안'  CENTERED,
*            181 SY-VLINE, 182 '심  의'  CENTERED,
*            188 SY-VLINE, 189 '확  정' CENTERED,
*            195 SY-VLINE.
*     ULINE AT /169(195).
*     WRITE:/169 SY-VLINE NO-GAP, 170 '담'  CENTERED,
*            174 SY-VLINE, 175 ''  CENTERED,
*            181 SY-VLINE, 182 ''  CENTERED,
*            188 SY-VLINE, 189 '' CENTERED,
*            195 SY-VLINE.
*     WRITE:80 '수입관세/부가세/통관료 일계표'.
*
*     WRITE:/169 SY-VLINE NO-GAP, 170 '당'  CENTERED,
*            174 SY-VLINE, 175 ''  CENTERED,
*            181 SY-VLINE, 182 ''  CENTERED,
*            188 SY-VLINE, 189 '' CENTERED,
*            195 SY-VLINE.
*     ULINE AT /169(195).
*     WRITE:/169 SY-VLINE NO-GAP, 170 '재'  CENTERED,
*            174 SY-VLINE, 175 ''  CENTERED,
*            181 SY-VLINE, 182 ''  CENTERED,
*            188 SY-VLINE, 189 '' CENTERED,
*            195 SY-VLINE.
*     WRITE:/169 SY-VLINE NO-GAP, 170 '경'  CENTERED,
*            174 SY-VLINE, 175 ''  CENTERED,
*            181 SY-VLINE, 182 ''  CENTERED,
*            188 SY-VLINE, 189 '' CENTERED,
*            195 SY-VLINE.
*     ULINE AT /169(195).
*
** WRITE:/80 '수입관세/부가세/통관료 일계표'.
*  SKIP 2.
  WRITE:/ '담당자:',S_CNAME-LOW,
         140 '생성일:',S_DAT-LOW NO-GAP,'.' NO-GAP,
                       S_TIME-LOW,'~',
                       S_DAT-LOW NO-GAP,'.' NO-GAP,
                       S_TIME-HIGH.
  WRITE:/ SY-ULINE.

  WRITE:/(10) '회계번호'    CENTERED,
          (13) '구매문서'   CENTERED,
          (20) 'B/L NO'     CENTERED,
          (10) '면허번호'   CENTERED,
          (08) '징수형태'   CENTERED,
          (12) '신고일'     CENTERED,
          (12) '지불일'     CENTERED,
          (17) '과표액'     RIGHT-JUSTIFIED,
          (20) '거래선'     CENTERED,
          (17) '관세'       RIGHT-JUSTIFIED,
          (17) '부가세'     RIGHT-JUSTIFIED,
          (17) '통관수수료' RIGHT-JUSTIFIED,
          (9) '귀속사업장'  CENTERED.
*  WRITE:/ SY-ULINE.

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
*      MESSAGE S960 WITH SY-UNAME 'B/L Doc transaction'.
*      W_ERR_CHK = 'Y'.   EXIT.
*   ENDIF.

ENDFORM.                    " P2000_AUTHORITY_CHECK

*&---------------------------------------------------------------------
*&      Form  P1000_GET_IT_TAB
*&---------------------------------------------------------------------
FORM P1000_READ_DATA USING W_ERR_CHK.

  W_ERR_CHK = 'N'.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TAB
            FROM ZTBKPF AS R INNER JOIN ZTBSEG AS I
             ON R~BUKRS = I~BUKRS
            AND R~BELNR = I~BELNR
            AND R~GJAHR = I~GJAHR
         WHERE  R~ZFPOSYN  = 'Y'              " 전기여부.
           AND  R~BUKRS    IN  S_BUKRS
           AND  R~UDAT     IN  S_DAT          " 기준일.
           AND  R~ZFCNAME  IN  S_CNAME
           AND  R~UTME     IN  S_TIME
           AND  I~ZFCSTGRP = '006'.
  IF SY-SUBRC NE 0.  W_ERR_CHK = 'Y'. EXIT.  ENDIF.

  SORT IT_TAB BY ZFACDO ZFCNAME CPUDT.

  LOOP AT IT_TAB.
     W_TABIX = SY-TABIX.
     CLEAR ZTIV.
*     SELECT  SINGLE *
*        FROM  ZTIV
*        WHERE ZFIVNO = IT_TAB-ZFIMDNO.
*    CLEAR ZTCUCLIV.
*    SELECT SINGLE *
*        FROM ZTCUCLIV
*        WHERE ZFIVNO = ZTIV-ZFIVNO.
*>> 수입면허.
     CLEAR ZTIDS.
     SELECT SINGLE *
        FROM ZTIDS
       WHERE ZFIVNO   = IT_TAB-ZFIMDNO.
*         AND ZFCLSEQ = ZTCUCLIV-ZFCLSEQ.
     IF SY-SUBRC EQ 0.
        MOVE: ZTIDS-ZFIDRNO  TO IT_TAB-ZFIDRNO,
              ZTIDS-ZFIDSDT  TO IT_TAB-ZFIDSDT,      " 신고일.
              ZTIDS-ZFCOCD   TO IT_TAB-ZFCOCD.       " 관세징수형태.
     ENDIF.
*>> 관리 코드.
     CASE IT_TAB-ZFCD.
        WHEN '001'.  " 관세.
          IT_TAB-GUBUN = '1'.
*          MOVE: ZTIDS-ZFCUAMTS TO IT_TAB-ZFCUAMTS.     " 총관?
          MOVE IT_TAB-WRBTR     TO IT_TAB-ZFCUAMTS.     " 총관?
        WHEN '002'.  " 통관수수료.
          IT_TAB-GUBUN = '3'.
*          MOVE: ZTIDS-ZFCUTAMT TO IT_TAB-ZFCUTAMT.     " 관세사 수수?
          MOVE IT_TAB-WRBTR     TO IT_TAB-ZFCUTAMT.     " 총관?
        WHEN '003'.  " 부가세.
          IT_TAB-GUBUN = '2'.
*          MOVE: ZTIDS-ZFVAAMTS TO IT_TAB-ZFVAAMTS.     " 총부가?
          MOVE IT_TAB-WMWST     TO IT_TAB-ZFVAAMTS.     " 총관?
        WHEN OTHERS.
          DELETE IT_TAB INDEX W_TABIX.
          CONTINUE.
     ENDCASE.

*>> B/L DATA MOVE
     CLEAR ZTBL.
     SELECT SINGLE *
         FROM ZTBL
        WHERE ZFBLNO  = ZTIDS-ZFBLNO.
     IF SY-SUBRC EQ 0.
        MOVE: ZTBL-ZFREBELN  TO   IT_TAB-ZFREBELN,      " P/O NO
              ZTBL-ZFHBLNO   TO   IT_TAB-ZFHBLNO,       " B/L NO
              ZTBL-ZFSHNO    TO   IT_TAB-ZFSHNO.        " 선적차?
     ENDIF.
     CLEAR LFA1.
     SELECT SINGLE *
            FROM  LFA1
            WHERE  LIFNR = IT_TAB-LIFNR.
     IT_TAB-NAME1 = LFA1-NAME1.

*> 역기표.
     IF IT_TAB-ZFRVSX EQ 'X'.
        IT_TAB-DMBTR    = IT_TAB-DMBTR    * -1.
        IT_TAB-WRBTR    = IT_TAB-WRBTR    * -1.
        IT_TAB-ZFCUAMTS = IT_TAB-ZFCUAMTS * -1.
        IT_TAB-ZFCUTAMT = IT_TAB-ZFCUTAMT * -1.
        IT_TAB-ZFVAAMTS = IT_TAB-ZFVAAMTS * -1.
        IT_TAB-WMWST    = IT_TAB-WMWST    * -1.
        IT_TAB-FWBAS    = IT_TAB-FWBAS    * -1.
     ENDIF.

     MODIFY IT_TAB INDEX W_TABIX.

  ENDLOOP.

ENDFORM.                    " P1000_READ_DATA

*&---------------------------------------------------------------------
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------
FORM P3000_DATA_WRITE .

   SET TITLEBAR  'ZIMR82'.
   SET PF-STATUS 'ZIMR82'.

   SORT IT_TAB BY  ZFCNAME  UTME GUBUN.
   LOOP AT IT_TAB.
        ON CHANGE OF IT_TAB-ZFIMDNO .
            WRITE:/ SY-ULINE.
            PERFORM P3000_TOP_LINE_WRITE.
        ENDON.
        PERFORM   P3000_LINE_WRITE.
        AT LAST.
           PERFORM P3000_LAST_WRITE.
        ENDAT.
   ENDLOOP.

   CLEAR: IT_TAB, W_TABIX.

ENDFORM.                    " P3000_DATA_WRITE
*&---------------------------------------------------------------------
*&      Form  P2000_INIT
*&---------------------------------------------------------------------
FORM P2000_INIT.

  SET TITLEBAR  'ZIMR82'.
  MOVE :    'I'          TO  S_DAT-SIGN,
            'EQ'         TO  S_DAT-OPTION,
            SY-DATUM     TO  S_DAT-LOW.

  APPEND S_DAT.

ENDFORM.                    " P2000_INIT
*&---------------------------------------------------------------------
*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------
*
FORM P3000_LINE_WRITE.
   DATA: W_TEXT(13).

   IF IT_TAB-ZFSHNO IS INITIAL.
       MOVE IT_TAB-ZFREBELN TO W_TEXT.
   ELSE.
       CONCATENATE IT_TAB-ZFREBELN '-' IT_TAB-ZFSHNO INTO W_TEXT.
   ENDIF.

   WRITE:/(10) IT_TAB-ZFACDO CENTERED,"회계전표번호.
         (13) W_TEXT         CENTERED, " P/O NO
         (20) IT_TAB-ZFHBLNO CENTERED, " B/L NO
         (10) IT_TAB-ZFIDRNO CENTERED,
         (08) IT_TAB-ZFCOCD CENTERED, " 관세징수형태.
         (12) IT_TAB-ZFIDSDT CENTERED," 신고일.
         (12) IT_TAB-ZFBDT CENTERED,  " 지불일.
         (17) IT_TAB-FWBAS CURRENCY IT_TAB-WAERS,"과표액.
         (20) IT_TAB-NAME1 CENTERED,         " 거래선.
         (17) IT_TAB-ZFCUAMTS CURRENCY IT_TAB-WAERS ,"관?
         (17) IT_TAB-ZFVAAMTS CURRENCY IT_TAB-WAERS,"부가?
         (17) IT_TAB-ZFCUTAMT CURRENCY IT_TAB-WAERS, "수수?
         (9) IT_TAB-BUPLA CENTERED.  " 귀속사업장.
   HIDE: IT_TAB, W_TABIX.

ENDFORM.                    " P3000_LINE_WRITE
*&---------------------------------------------------------------------
*
*&      Form  P3000_TOP_LINE_WRITE
*&---------------------------------------------------------------------
*
FORM P3000_TOP_LINE_WRITE.



ENDFORM.                    " P3000_TOP_LINE_WRITE
*&---------------------------------------------------------------------
*
*&      Form  P3000_LAST_WRITE
*&---------------------------------------------------------------------
*
FORM P3000_LAST_WRITE.

   SUM.
   WRITE:/ SY-ULINE.
   WRITE:/(10) '',
         (13) '',
         (20) '',
         (10) '',
         (08) '',
         (12) '',
         (12) '계',
         (17) IT_TAB-FWBAS CURRENCY 'KRW',"과표액.
         (20) '',
         (17) IT_TAB-ZFCUAMTS CURRENCY 'KRW' ,"관?
         (17) IT_TAB-ZFVAAMTS CURRENCY 'KRW',"부가?
         (17) IT_TAB-ZFCUTAMT  CURRENCY 'KRW', "수수?
         (9) ''.
   WRITE:/ SY-ULINE.

ENDFORM.                    " P3000_LAST_WRITE
*&---------------------------------------------------------------------*
*&      Form  P2000_DISPLAY_COST_DOCUMENT
*&---------------------------------------------------------------------*
FORM P2000_DISPLAY_COST_DOCUMENT USING    P_BUKRS
                                          P_GJAHR
                                          P_BELNR.

 SET  PARAMETER ID  'BUK'       FIELD   P_BUKRS.
 SET  PARAMETER ID  'GJR'       FIELD   P_GJAHR.
 SET  PARAMETER ID  'ZPBENR'    FIELD   P_BELNR.
 CALL TRANSACTION 'ZIMY3'.

ENDFORM.                    " P2000_DISPLAY_COST_DOCUMENT
