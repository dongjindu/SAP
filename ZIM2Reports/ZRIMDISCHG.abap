*&---------------------------------------------------------------------*
*& Report  ZRIMDISCHG                                                  *
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입어음 Discount 내역통보서                          *
*&      작성자 : 이석철 INFOLINK Ltd.                                  *
*&      작성일 : 2001.07.20                                            *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
REPORT ZRIMDISCHG     MESSAGE-ID ZIM
                      LINE-SIZE 116
                      NO STANDARD PAGE HEADING.

*-----------------------------------------------------------------------
* Tables 및 변수 Define
*-----------------------------------------------------------------------
TABLES: ZTPMTHD, LFA1.

DATA : W_ERR_CHK,
       W_PNAM(20),
       W_BNAME(120),
       W_USIT(20),
       W_USITR(20),
       W_SUM(20),
       W_SUM2      LIKE   ZTPMTHD-ZFPNAM,
       W_BKCH(20),
       W_ADBK      LIKE   LFA1,
       W_BENI      LIKE   LFA1.

*-----------------------------------------------------------------------
* Selection Screen.
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

   PARAMETERS    :   P_PNNO   LIKE  ZTPMTHD-ZFPNNO
                                    OBLIGATORY.
                              "PAYMENT NOTICE 관리번호.
SELECTION-SCREEN END OF BLOCK B1.

* PARAMETER 초기값 Setting
INITIALIZATION.                          " 초기값 SETTING
   PERFORM   P2000_SET_PARAMETER.

* Title Text Write
TOP-OF-PAGE.
   PERFORM   P3000_TITLE_WRITE.          " 해더 출력...

*-----------------------------------------------------------------------
* START OF SELECTION 절.
*-----------------------------------------------------------------------
START-OF-SELECTION.

* Payment Notice Header 테이블 SELECT
   PERFORM   P1000_GET_DATA      USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.     ENDIF.

* 레포트 Write
   PERFORM   P3000_DATA_WRITE        USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.     ENDIF.

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.

*   CASE SY-UCOMM.
*      WHEN 'DISP'.
*         IF IT_TAB IS INITIAL.
*            MESSAGE S962.
*         ELSE.
*            CASE IT_TAB-ZFDHDOC.
*               WHEN 'APP700' OR 'LOCAPP' OR 'PAYORD' OR 'APPPUR' OR
*                    'APP707' OR 'LOCAMR'.
*                  SET PARAMETER ID 'BES'       FIELD ''.  "P/O.
*                  SET PARAMETER ID 'ZPOPNNO'   FIELD ''.  "L/C.
*                  SET PARAMETER ID 'ZPREQNO'   FIELD
*IT_TAB-ZFDHREF(10)*.
*                                               "수입의뢰관리번호.
*                  SET PARAMETER ID 'ZPAMDNO'
*                                FIELD IT_TAB-ZFDHREF+11(5). "AMEND
*회차*
*
*                  IF IT_TAB-ZFDHREF+11(5) EQ '00000'.
*                     CALL TRANSACTION 'ZIM03' AND SKIP  FIRST SCREEN.
*                  ELSE.
*                     CALL TRANSACTION 'ZIM13' AND SKIP  FIRST SCREEN.
*                  ENDIF.*
*
*               WHEN 'IMPREQ'.
*                  SET PARAMETER ID 'ZPHBLNO'    FIELD ''.  "HOUSE B/L.
*                  SET PARAMETER ID 'ZPBLNO'
*                                FIELD IT_TAB-ZFDHREF(10).  "L/C.
*                  SET PARAMETER ID 'ZPIDRNO'
*                                FIELD ''.          "수입의뢰관리번호.
*                  SET PARAMETER ID 'ZPCLSEQ'
*                                FIELD IT_TAB-ZFDHREF+11(5). "AMEND 회차
*                  CALL TRANSACTION 'ZIM63' AND SKIP  FIRST SCREEN.*
*
*               WHEN OTHERS.
*            ENDCASE.
*         ENDIF.
*      WHEN 'DOWN'.          " FILE DOWNLOAD....
*           PERFORM P3000_TO_PC_DOWNLOAD.
* *     WHEN 'REFR'.
*           PERFORM  P1000_RESET_LIST.
*      WHEN OTHERS.
*   ENDCASE.
*   CLEAR : IT_TAB.

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.
  SET TITLEBAR  'ZIM40'.          " TITLE BAR
ENDFORM.                    " P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  SKIP 2.
  WRITE : /51  '수입어음 Discount 내역통보서'.
  SKIP 1.

ENDFORM.                    " P3000_TITLE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P1000_GET_DATA
*&---------------------------------------------------------------------*
FORM P1000_GET_DATA   USING   W_ERR_CHK.

  W_ERR_CHK = 'N'.                " Error Bit Setting

  SELECT SINGLE * FROM ZTPMTHD WHERE ZFPNNO EQ P_PNNO.

  IF SY-SUBRC NE 0.            " Not Found?
     W_ERR_CHK = 'Y'.  MESSAGE S009.    EXIT.
  ENDIF.
*>> 어음금액 정렬.
  WRITE:  ZTPMTHD-ZFPNAM CURRENCY ZTPMTHD-ZFPNAMC
                         TO W_PNAM LEFT-JUSTIFIED.
  CONCATENATE ZTPMTHD-ZFPNAMC W_PNAM   INTO W_PNAM
                         SEPARATED BY SPACE.
*>> 할인수수료 정렬.
  WRITE:  ZTPMTHD-ZFUSIT CURRENCY ZTPMTHD-ZFUSITC
                         TO W_USIT LEFT-JUSTIFIED.
  CONCATENATE ZTPMTHD-ZFUSITC W_USIT   INTO W_USIT
                         SEPARATED BY SPACE.
*>> 어음할인율 정렬.
  WRITE : ZTPMTHD-ZFUSITR TO W_USITR
                          LEFT-JUSTIFIED.
  CONCATENATE W_USITR 'PERCENTAGE'     INTO W_USITR
                          SEPARATED BY SPACE.
*  CONDENSE  W_USITR NO-GAPS.

*>>기타수수료정렬.
  WRITE: ZTPMTHD-ZFBKCH CURRENCY ZTPMTHD-ZFBKCHC
                        TO W_BKCH LEFT-JUSTIFIED.
  CONCATENATE  ZTPMTHD-ZFBKCHC W_BKCH INTO W_BKCH
               SEPARATED BY SPACE.
  WRITE: W_BKCH TO W_BKCH LEFT-JUSTIFIED.

*>> 합계 정렬
  CLEAR : W_SUM2.
  W_SUM2 = ZTPMTHD-ZFPNAM + ZTPMTHD-ZFUSIT + ZTPMTHD-ZFBKCH.

  WRITE: W_SUM2 CURRENCY ZTPMTHD-ZFBKCHC TO W_SUM
                               LEFT-JUSTIFIED.
  CONCATENATE  ZTPMTHD-ZFBKCHC W_SUM INTO W_SUM
                               SEPARATED BY SPACE.
  WRITE: W_SUM TO W_SUM LEFT-JUSTIFIED.

  IF ZTPMTHD-ZFPNBN IS INITIAL.
     CLEAR : W_ADBK.
  ELSE.
     SELECT SINGLE * INTO W_ADBK
                     FROM LFA1
                     WHERE LIFNR EQ ZTPMTHD-ZFPNBN.
*>>통지은행명정렬
     WRITE: W_ADBK-NAME1 TO W_BNAME LEFT-JUSTIFIED.
     CONCATENATE  W_BNAME W_ADBK-NAME2 INTO W_BNAME
                  SEPARATED BY SPACE.
     WRITE: W_BNAME TO W_BNAME LEFT-JUSTIFIED.

  ENDIF.

  IF ZTPMTHD-ZFBENI IS INITIAL.
     CLEAR : W_BENI.
  ELSE.
     SELECT SINGLE * INTO W_BENI
                     FROM LFA1
                     WHERE LIFNR EQ ZTPMTHD-ZFBENI.
  ENDIF.

ENDFORM.            " P1000_GET_DATA
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING      W_ERR_CHK.

   SET PF-STATUS 'ZIM06'.           " GUI STATUS SETTING.
   SET TITLEBAR  'ZIM40'.           " GUI TITLE SETTING..

   WRITE: / '문서번호 ', ZTPMTHD-ZFPNNO,
         95 '통지일자', ZTPMTHD-ZFNTDT,
          / SY-ULINE,
          / '통지은행',                   50 W_BNAME,
          / '거래고객',                   50 W_BENI-NAME1.
    IF NOT ZTPMTHD-ZFOPNNO IS INITIAL.
        WRITE:/ '신용장(계약서)번호',     50 ZTPMTHD-ZFOPNNO.
    ENDIF.
    IF NOT ZTPMTHD-ZFHBLNO IS INITIAL.
        WRITE:/ '선하증권(운송장)번호',
                                          50 ZTPMTHD-ZFHBLNO.
    ENDIF.

        WRITE:/ '어음금액',              50 W_PNAM.

        WRITE:/ '할인수수료',             50 W_USIT,
              / '어음할인율',             50 W_USITR.

        WRITE:/ '기타수수료',             50 W_BKCH.

        WRITE:/ '합계',                   50 W_SUM.

    IF NOT ZTPMTHD-ZFDSDT IS INITIAL.
        WRITE:/ '할인일자',               50 ZTPMTHD-ZFDSDT.
    ENDIF.

    IF NOT ZTPMTHD-ZFPYDT IS INITIAL.
        WRITE:/ '최종결제일',             50 ZTPMTHD-ZFPYDT.
    ENDIF.
    IF NOT ( ZTPMTHD-ZFRMK1 IS INITIAL
             AND ZTPMTHD-ZFRMK2 IS INITIAL
             AND ZTPMTHD-ZFRMK3 IS INITIAL
             AND ZTPMTHD-ZFRMK4 IS INITIAL
             AND ZTPMTHD-ZFRMK5 IS INITIAL ).
        WRITE:/ '기타사항',               50 ZTPMTHD-ZFRMK1,
              /50 ZTPMTHD-ZFRMK2,
              /50 ZTPMTHD-ZFRMK3,
              /50 ZTPMTHD-ZFRMK4,
              /50 ZTPMTHD-ZFRMK5.
     ENDIF.
     SKIP 1.
     WRITE: / SY-ULINE+(50), '< 발신기관 전자서명 >', 74 SY-ULINE,
            / '발신기관 전자서명  ', W_ADBK-NAME1,
            / '                   ', W_ADBK-NAME2,
            / '                   ', W_ADBK-STCD2.
      SKIP 4.
      WRITE:/,/,/,/,/,/,
            /13 SY-ULINE+13(88),
            /13 SY-VLINE,
            '  이 전자문서는 무역업무자동화촉진에관한법률',
            '  제 2 조 제 7 호, 제 10 조 제 1 항 및 ',
            100 SY-VLINE,
            /13 SY-VLINE, '  동법시행령 제 12 조에 의거 발행된',
            ' 전자문서입니다.', 100 SY-VLINE,
            /13 SY-VLINE, 100 SY-VLINE, 13 SY-ULINE+13(88).

ENDFORM.                    " P3000_DATA_WRITE
