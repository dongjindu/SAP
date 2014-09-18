*&---------------------------------------------------------------------*
*& Report  ZRIMTTREQ                                                   *
*&---------------------------------------------------------------------*
*&  프로그램명 : 지급신청서                                            *
*&      작성자 : 이석철 INFOLINK Ltd.                                  *
*&      작성일 : 2001.07.16                                            *
*&---------------------------------------------------------------------*
*&   DESC.     :  지급신청서                                           *
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMTTREQ    MESSAGE-ID ZIM
                     LINE-SIZE 116
                     NO STANDARD PAGE HEADING.

*-----------------------------------------------------------------------
* 변수선언을 않고, 테이블만을 갖고 처리....
*-----------------------------------------------------------------------
DATA : W_ERR_CHK,
       W_DOM_SENDTY(20),
       W_DOM_CFRG(40),
       W_AMOUNT(30),
       W_OBNM(80),
       W_APPAD(80),
       W_APPNM(110),
       W_ZFOBNM(80).

*-----------------------------------------------------------------------
* Tables 및 변수 Define
*-----------------------------------------------------------------------
TABLES : ZTREQHD, ZTREQST, ZTCIVHD, ZTTTHD, ZTIMIMGTX.
      "수입의뢰 HEADER, 수입의뢰품목, 수입의뢰상태, 지급지시서 HEADER

*-----------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

   PARAMETERS    : P_CIVRN LIKE ZTCIVHD-ZFCIVRN. "물대관리번호.

SELECTION-SCREEN END OF BLOCK B1.


* PARAMETER 초기값 Setting
INITIALIZATION.                          " 초기값 SETTING
    PERFORM   P2000_SET_PARAMETER.

*-----------------------------------------------------------------------
* START OF SELECTION ?
*-----------------------------------------------------------------------
START-OF-SELECTION.
* 권한 검증 함?
    PERFORM   P2000_AUTHORITY_CHECK     USING   W_ERR_CHK.
    IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
*  테이블 SELECT
    PERFORM   P1000_GET_DATA           USING W_ERR_CHK.
    IF W_ERR_CHK EQ 'Y'.
      MESSAGE S966.  EXIT.
    ENDIF.
* 레포트 Write
    PERFORM   P3000_DATA_WRITE          USING   W_ERR_CHK.
    IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.

  SET  TITLEBAR 'TTREQ'.          " TITLE BAR

ENDFORM.                    " P2000_SET_PARAMETER

*&---------------------------------------------------------------------*
*&      Form  P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
FORM P2000_AUTHORITY_CHECK          USING   W_ERR_CHK.

*   W_ERR_CHK = 'N'.
*-----------------------------------------------------------------------
*  해당 화면 AUTHORITY CHECK
*-----------------------------------------------------------------------
*   AUTHORITY-CHECK OBJECT 'ZI_BL_MGT'
*           ID 'ACTVT' FIELD '*'.

*   IF SY-SUBRC NE 0.
*      MESSAGE S960 WITH SY-UNAME 'B/L 관리 트랜잭션'.
*      W_ERR_CHK = 'Y'.   EXIT.
*   ENDIF.

ENDFORM.                    " P2000_AUTHORITY_CHECK


*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING      W_ERR_CHK.

  SET PF-STATUS 'TTREQ'.           " GUI STATUS SETTING
  SET  TITLEBAR 'TTREQ'.           " GUI TITLE SETTING..
  PERFORM P3000_LINE_WRITE.

ENDFORM.                    " P3000_DATA_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

  PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDSENDTY' ZTTTHD-ZFSENDTY
                                    CHANGING   W_DOM_SENDTY.

  PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDCFRG1' ZTTTHD-ZFCFRG
                                    CHANGING   W_DOM_CFRG.

  WRITE: ZTTTHD-ZFAMT CURRENCY ZTTTHD-WAERS TO W_AMOUNT LEFT-JUSTIFIED.
  CONCATENATE ZTTTHD-WAERS W_AMOUNT INTO W_AMOUNT SEPARATED BY SPACE.

  WRITE: '신청인:' TO W_APPNM.
  CONCATENATE W_APPNM ZTIMIMGTX-ZFAPPNM INTO W_APPNM SEPARATED BY SPACE.
  CONCATENATE W_APPNM '(인 또는 서명)' INTO W_APPNM SEPARATED BY SPACE.
  WRITE: W_APPNM TO W_APPNM RIGHT-JUSTIFIED.

  WRITE: ZTTTHD-ZFOBNM TO W_OBNM.
  CONCATENATE W_OBNM '은행장' INTO W_OBNM SEPARATED BY SPACE.
  WRITE: W_OBNM TO W_OBNM RIGHT-JUSTIFIED.

  WRITE: ZTTTHD-ZFAPPAD1 TO W_APPAD LEFT-JUSTIFIED.       "주소
  CONCATENATE W_APPAD ZTTTHD-ZFAPPAD2 INTO W_APPAD SEPARATED BY SPACE.
  WRITE: W_APPAD TO W_APPAD LEFT-JUSTIFIED.
  CONCATENATE W_APPAD ZTTTHD-ZFAPPAD3 INTO W_APPAD SEPARATED BY SPACE.
  WRITE: W_APPAD TO W_APPAD LEFT-JUSTIFIED.

  SKIP 4.
*  WRITE:/ SY-ULINE+(30), 67 SY-ULINE,
*        / SY-VLINE,'FILE NO:', ZTREQHD-EBELN, 30 SY-VLINE,
*          67 SY-VLINE,
*          73 SY-VLINE, '  起  案',
*          88 SY-VLINE, '  審  議',
*          103 SY-VLINE,'  確  定', 116 SY-VLINE,
*        / SY-ULINE+(30), 67 SY-VLINE, '決', 73 SY-ULINE,
*        /67 SY-VLINE, 73 SY-VLINE, 88 SY-VLINE, 103 SY-VLINE,
*         116 SY-VLINE,
*        /67 SY-VLINE, '裁', 73 SY-VLINE, 88 SY-VLINE,
*         103 SY-VLINE, 116 SY-VLINE,
*        /67 SY-VLINE, 73 SY-VLINE, 88 SY-VLINE, 103 SY-VLINE,
*         116 SY-VLINE,
*        /67 SY-ULINE.

  WRITE:/ '<지침서식 제3-1호>',
        /, /(116) '지 급 신 청 서' CENTERED,
        /, /(116) '[거래외국환은행지정(변경)신청서 겸용]' CENTERED.

  WRITE:/ SY-ULINE,
        / SY-VLINE, 6 SY-VLINE, '상  호  또는  성  명', 30 SY-VLINE,
          '한   글  (KOREA)', 50 SY-VLINE, ZTIMIMGTX-ZFAPPNM,
          116 SY-VLINE,
        / SY-VLINE, '신', 6 SY-VLINE, 50 SY-VLINE,
          30 SY-ULINE, 116 SY-VLINE,
        / SY-VLINE, 6 SY-VLINE, '    (Applicant)',  30 SY-VLINE,
          '영   문  (ENGLISH)', 50 SY-VLINE, ZTIMIMGTX-ZFAPPNML,
          116 SY-VLINE,
        / SY-VLINE, 6 SY-VLINE, 30 SY-VLINE,  50 SY-VLINE,
          6 SY-ULINE, 116 SY-VLINE,
        / SY-VLINE, 6 SY-VLINE, '사        업        자',  30 SY-VLINE,
          '사업자  등록 번호', 50 SY-VLINE, ZTIMIMGTX-ZFELTXN,
          116 SY-VLINE,
        / SY-VLINE, '청', 6 SY-VLINE, 30 SY-VLINE,
          6 SY-ULINE, 116 SY-VLINE,
        / SY-VLINE, 6 SY-VLINE, '내        국        인', 30 SY-VLINE,
          '주 민 등 록 번 호', 50 SY-VLINE,
          116 SY-VLINE,
        / SY-VLINE, 6 SY-VLINE, 30 SY-VLINE,
          6 SY-ULINE, 116 SY-VLINE,
        / SY-VLINE, 6 SY-VLINE, '외 국 인 (해 외 교 포)', 30 SY-VLINE,
          '여   권   번   호', 50 SY-VLINE,
          116 SY-VLINE,
        / SY-VLINE, '인', 6 SY-VLINE, 30 SY-VLINE,
          6 SY-ULINE, 116 SY-VLINE,
        / SY-VLINE, 6 SY-VLINE, '주                  소', 30 SY-VLINE,
          W_APPAD, 116 SY-VLINE,
        / SY-VLINE, 6 SY-VLINE, 30 SY-VLINE,
          '                                                  ',
          ' (Tel)', ZTTTHD-ZFTELNO,
          116 SY-VLINE, SY-ULINE,
*-----------------------------------------------------------------------
        / SY-VLINE, 6 SY-VLINE, ' 송 금 방 법 (Send By)', 30 SY-VLINE,
          W_DOM_SENDTY, 116 SY-VLINE,
        / SY-VLINE, 6 SY-VLINE, 6 SY-ULINE,
        / SY-VLINE, 6 SY-VLINE, ' 송   금   액 (Amount)', 30 SY-VLINE,
          W_AMOUNT, 116 SY-VLINE,
        / SY-VLINE, 6 SY-VLINE, 6 SY-ULINE,
        / SY-VLINE, 6 SY-VLINE, 30 SY-VLINE, '성       명 (Name)',
          50 SY-VLINE, ZTTTHD-ZFBENI1, 85 SY-VLINE,
          '신청인과의 관계',105 SY-VLINE, 116 SY-VLINE,
        / SY-VLINE, 6 SY-VLINE, 30 SY-ULINE, 30 SY-VLINE,
        / SY-VLINE, '신', 6 SY-VLINE, '수        취        인',
          30 SY-VLINE, '주      소(Address)', 116 SY-VLINE,
        / SY-VLINE, 6 SY-VLINE, 30 SY-VLINE, ZTTTHD-ZFBENI2,
          116 SY-VLINE,
        / SY-VLINE, 6 SY-VLINE, 30 SY-VLINE, ZTTTHD-ZFBENI3,
          116 SY-VLINE,
        / SY-VLINE, 6 SY-VLINE, 30 SY-VLINE, 116 SY-VLINE,
          30 SY-ULINE,
        / SY-VLINE, 6 SY-VLINE, 30 SY-VLINE,
          '국             적', 50 SY-VLINE, ZTREQHD-ZFSHCU,
          116 SY-VLINE,
        / SY-VLINE, '청', 6 SY-VLINE, 30 SY-VLINE,
          116 SY-VLINE, 6 SY-ULINE,
*-----------------------------------------------------------------------
        / SY-VLINE, 6 SY-VLINE, 30 SY-VLINE,
          '은행명·주소(Bank Name & Address)', 116 SY-VLINE,
        / SY-VLINE, 6 SY-VLINE, '수 취 인 거 래 은 행', 30 SY-VLINE,
          ZTTTHD-ZFOBNM, 116 SY-VLINE,
        / SY-VLINE, 6 SY-VLINE, '  (BNF''S BANK)', 30 SY-VLINE,
          ZTTTHD-ZFOBBR, 116 SY-VLINE,
        / SY-VLINE, 6 SY-VLINE, 30 SY-VLINE, 30 SY-ULINE,
        / SY-VLINE, 6 SY-VLINE, 30 SY-VLINE,
          '수취인계좌번호(BNF''S A/C No)', 65 SY-VLINE,
          ZTTTHD-ZFOBAK1, 116 SY-VLINE,
        / SY-VLINE, '내', 6 SY-VLINE, 30 SY-VLINE, 60 SY-VLINE,
          6 SY-ULINE,
*-----------------------------------------------------------------------
        / SY-VLINE, 6 SY-VLINE, '지    급    사    유', 30 SY-VLINE,
          50 SY-VLINE, ' 국외수수료부담(Charge)', 75 SY-VLINE,
          W_DOM_CFRG, 116 SY-VLINE,
        / SY-VLINE, 6 SY-VLINE, 30 SY-VLINE,
*-----------------------------------------------------------------------
          50 SY-VLINE, 75 SY-VLINE, 116 SY-VLINE, 6 SY-ULINE,
        / SY-VLINE, 6 SY-VLINE, '수입대금(미화 2만불',
          30 SY-VLINE, ' 품목 (H.S. Code)', 50 SY-VLINE,
          '        L/C or 계약서 No.', 85 SY-VLINE,
          '      대응수입예정일', 116 SY-VLINE,
        / SY-VLINE, 6 SY-VLINE, 30 SY-VLINE, 50 SY-VLINE,
          85 SY-VLINE, 116 SY-VLINE, 30 SY-ULINE,
        / SY-VLINE, '용', 6 SY-VLINE, '초과)의 경우기재', 30 SY-VLINE,
          50 SY-VLINE, ZTREQHD-EBELN, 85 SY-VLINE, '당초:       ',
          '변경:      ', 116 SY-VLINE,
        / SY-VLINE, 6 SY-VLINE, 30 SY-VLINE, 50 SY-VLINE,
          85 SY-VLINE, 116 SY-VLINE, 6 SY-ULINE,
*-----------------------------------------------------------------------
        / SY-VLINE, 6 SY-VLINE, 30 SY-VLINE, '기 본 경 비:',
           '                    ', '체 재 비:', 116 SY-VLINE,
        / SY-VLINE, 6 SY-VLINE, '※ 해 외 여 행 경 비',
          30 SY-VLINE, 116 SY-VLINE,
        / SY-VLINE, 6 SY-VLINE, 30 SY-VLINE, '현지정착비:',
          '                     ', '기타경비:', 116 SY-VLINE,
        / SY-VLINE, 6 SY-VLINE,  '   동 의 경 우 기 재',
          30 SY-VLINE, 116 SY-VLINE,
        / SY-VLINE, 6 SY-VLINE, 30 SY-VLINE, '해외이주비:',
          116 SY-VLINE, SY-ULINE,
*        / SY-VLINE, 6 SY-VLINE, 30 SY-VLINE, 116 SY-VLINE, SY-ULINE,
*-----------------------------------------------------------------------
        / SY-VLINE, ' □ 귀행을 거주자의 건당 비화 5천불이하',
          '소액경상지급을 위한 거래외국환은행으로 지정하고자 합니다.',
          116 SY-VLINE,
        / SY-VLINE, '     (실명확인증표에 의하여 실명확인후 본 서식',
         '상단에 실명확인필을 날인할 것)', 116 SY-VLINE,
        / SY-VLINE, ' □ 본인은 귀행 영업점에 비치된 외환거래',
          '기본약관을 알람하고 그 내용에 따른 것을 확약하며 ',
          '위와같이', 116 SY-VLINE,
        / SY-VLINE, '     지급신청합니다.', 116 SY-VLINE,
        / SY-VLINE, 116 SY-VLINE,
        / SY-VLINE, W_APPNM, 116 SY-VLINE, SY-ULINE,
*-----------------------------------------------------------------------
        / SY-VLINE, '이 신청서는 외국환통계자료로 활용하며 ',
          '과세자료로 국세청에 통보될 수 있습니다.',
          116 SY-VLINE, SY-ULINE,
        / SY-VLINE, '위 사실을 확인함', 70 SY-VLINE,
          '지 정 확 인 번 호', 90 SY-VLINE, 116 SY-VLINE,
        / SY-VLINE, 70 SY-VLINE, 90 SY-VLINE, 116 SY-VLINE, 70 SY-ULINE,
        / SY-VLINE, 70 SY-VLINE, '지   정   일   자',
          90 SY-VLINE, 116 SY-VLINE,
        / SY-VLINE, 70 SY-VLINE, 90 SY-VLINE, 116 SY-VLINE, 70 SY-ULINE,
        / SY-VLINE, '                                    ',
          '                                             ',
          '년           월           일', 116 SY-VLINE,
        / SY-VLINE, 116 SY-VLINE,
        / SY-VLINE, '     ', W_OBNM,
          '                  (인)', 116 SY-VLINE,
        / SY-ULINE,
        / '※ 신용카드 또는 직불카드에 의하여 지급하고자',
          ' 하는 경우에는 구분 표시할 것'.

ENDFORM.                    " P3000_LINE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P1000_GET_DATA
*&---------------------------------------------------------------------*
FORM P1000_GET_DATA USING W_ERR_CHK.

   W_ERR_CHK = 'N'.
*>>HEADER 읽기
*   CLEAR ZTREQHD.
*   SELECT SINGLE *
*     FROM ZTREQHD
*    WHERE ZFREQNO =  P_REQNO.
*>> 송장처리시로 변경 020619
   SELECT SINGLE * FROM ZTCIVHD WHERE ZFCIVRN = P_CIVRN.

   IF SY-SUBRC NE 0.
      W_ERR_CHK = 'Y'.
      EXIT.
   ENDIF.

*   CLEAR ZTREQST.
*   SELECT SINGLE *
*     FROM ZTREQST
*    WHERE ZFREQNO =  P_REQNO
*      AND ZFAMDNO =  P_AMDNO.

   IF SY-SUBRC NE 0.
      W_ERR_CHK = 'Y'.
      EXIT.
   ENDIF.
*>> IMG 읽기
   CLEAR ZTIMIMGTX.
   SELECT SINGLE *
     FROM ZTIMIMGTX
    WHERE BUKRS =  ZTCIVHD-BUKRS.

   IF SY-SUBRC NE 0.
      W_ERR_CHK = 'Y'.
      EXIT.
   ENDIF.

*>>지급지시서 HEADER 읽기
   CLEAR ZTTTHD.
   SELECT SINGLE *
     FROM ZTTTHD
    WHERE ZFCIVRN =  P_CIVRN.
*>>송금방법, 수수료 부담
   "IF ZTTTHD-ZFSENDTY EQ

   IF SY-SUBRC NE 0.
      W_ERR_CHK = 'Y'.
      EXIT.
   ENDIF.

ENDFORM.                    " P1100_GET_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_DD07T_SELECT
*&---------------------------------------------------------------------*
*FORM GET_DD07T_SELECT USING    P_DOMNAME
*                               P_FIELD
*                      CHANGING P_W_NAME.
*  CLEAR : DD07T, P_W_NAME.
*  IF P_FIELD IS INITIAL.   EXIT.   ENDIF.
*
*  SELECT * FROM DD07T WHERE DOMNAME     EQ P_DOMNAME
*                      AND   DDLANGUAGE  EQ SY-LANGU
*                      AND   AS4LOCAL    EQ 'A'
*                      AND   DOMVALUE_L  EQ P_FIELD
*                      ORDER BY AS4VERS DESCENDING.
*    EXIT.
*  ENDSELECT.
*   TRANSLATE DD07T-DDTEXT TO UPPER CASE.
*  P_W_NAME   = DD07T-DDTEXT.
*  TRANSLATE P_W_NAME TO UPPER CASE.
*ENDFORM.                    " GET_DD07T_SELECT
