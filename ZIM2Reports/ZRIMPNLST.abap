*&---------------------------------------------------------------------*
*& Report  ZRIMPNLST                                                   *
*&---------------------------------------------------------------------*
*&  프로그램명 : BANKER'S USANCE 기채발행 실적.                        *
*&      작성자 : Lee Seung-Jun INFOLINK Ltd.                           *
*&      작성일 : 2001.09.20                                            *
*&---------------------------------------------------------------------*
*&  DESC.      : 조회기간동안 BANKER'S USANCE 결제조건의 개채발생내역을
*&               PO NO별로 조회할 수 있는 화면구성.
*&---------------------------------------------------------------------*
*& [변경내용]
*&---------------------------------------------------------------------*
REPORT  ZRIMPNLST  MESSAGE-ID ZIM
                   LINE-SIZE 133
                   NO STANDARD PAGE HEADING .

*----------------------------------------------------------------------*
*          TABLE,DATA, DEFINE                                          *
*----------------------------------------------------------------------*
TABLES : LFA1, ZTIMIMG00, ZTPMTHD.

DATA   : W_ERR_CHK TYPE C VALUE 'N', "> ERROR CHECK.
         W_LINE  TYPE I,             "> IT_TAB LINE 수.
         W_MOD LIKE SY-TABIX,        "> 홀짝.
         W_TABIX LIKE SY-TABIX.

DATA   : BEGIN OF IT_TAB_Y OCCURS 0,
*> Select-Option..
     BUKRS    LIKE ZTPMTHD-BUKRS,   "> 회사코드.
     BUDAT    LIKE ZTPMTHD-BUDAT,   "> 기간.
*> List.
     EBELN    LIKE ZTPMTHD-EBELN,   "> 구매문서번호.
     ZFPNBN   LIKE ZTPMTHD-ZFPNBN,  "> 통지은행 거래처코드.
     NAME1    LIKE LFA1-NAME1,      "> 통지은행명.
     ZFOPNNO  LIKE ZTPMTHD-ZFOPNNO, "> 신용장(L/C)-승인번호.
     ZFPNAM   LIKE ZTPMTHD-ZFPNAM,  "> Notice Amount(원금).
     ZFPNAMC  LIKE ZTPMTHD-ZFPNAMC, "> Notice Currency.
     ZFPYDT   LIKE ZTPMTHD-ZFPYDT,  "> 결제완료일(기산일).
     ZFPWDT   LIKE ZTPMTHD-ZFPWDT,  "> 만기일.
     ZFUSITR  LIKE ZTPMTHD-ZFUSITR, "> 이자율.
     ZFUSIT   LIKE ZTPMTHD-ZFUSIT,  "> 이자금액.
     ZFUSITC  LIKE ZTPMTHD-ZFUSITC, "> 이자 통화단위.
     ZFBKCH   LIKE ZTPMTHD-ZFBKCH,  "> 이자지급금액.
     ZFBKCHP  LIKE ZTPMTHD-ZFBKCHP, "> Banking Charge 선취여부.
     ZFBKCHC  LIKE ZTPMTHD-ZFBKCHC, ">
*> Call Header Lever Clause.
     ZFREQNO  LIKE ZTPMTHD-ZFREQNO, "> 수입의뢰 관리번호.
     ZFHBLNO  LIKE ZTPMTHD-ZFHBLNO, "> House B/L No.
     ZFBLNO   LIKE ZTPMTHD-ZFBLNO,  "> B/L 관리번호.
     ZFISNO   LIKE ZTPMTHD-ZFISNO.  "> 인수증 발급번호.
DATA : END OF IT_TAB_Y.

DATA : BEGIN OF IT_TAB_NL OCCURS 0,
     BUKRS    LIKE ZTPMTHD-BUKRS,
     BUDAT    LIKE ZTPMTHD-BUDAT,
     EBELN    LIKE ZTPMTHD-EBELN,
     ZFPNBN   LIKE ZTPMTHD-ZFPNBN,
     NAME1    LIKE LFA1-NAME1,
     ZFOPNNO  LIKE ZTPMTHD-ZFOPNNO,
     ZFPNAM   LIKE ZTPMTHD-ZFPNAM,
     ZFPNAMC  LIKE ZTPMTHD-ZFPNAMC,
     ZFPYDT   LIKE ZTPMTHD-ZFPYDT,
     ZFPWDT   LIKE ZTPMTHD-ZFPWDT,
     ZFUSITR  LIKE ZTPMTHD-ZFUSITR, "이자율.
     ZFUSIT   LIKE ZTPMTHD-ZFUSIT,  "이자금액.
     ZFUSITC  LIKE ZTPMTHD-ZFUSITC, "이자 통화단위.
     ZFBKCH   LIKE ZTPMTHD-ZFBKCH,
     ZFBKCHP  LIKE ZTPMTHD-ZFBKCHP,
     ZFBKCHC  LIKE ZTPMTHD-ZFBKCHC,
     ZFREQNO  LIKE ZTPMTHD-ZFREQNO,
     ZFHBLNO  LIKE ZTPMTHD-ZFHBLNO,
     ZFBLNO   LIKE ZTPMTHD-ZFBLNO,
     ZFISNO   LIKE ZTPMTHD-ZFISNO.
DATA : END OF IT_TAB_NL.

DATA : BEGIN OF IT_TAB_NS OCCURS 0,
     BUKRS    LIKE ZTPMTHD-BUKRS,
     BUDAT    LIKE ZTPMTHD-BUDAT,
     EBELN    LIKE ZTPMTHD-EBELN,
     ZFPNBN   LIKE ZTPMTHD-ZFPNBN,
     NAME1    LIKE LFA1-NAME1,
     ZFOPNNO  LIKE ZTPMTHD-ZFOPNNO,
     ZFPNAM   LIKE ZTPMTHD-ZFPNAM,
     ZFPNAMC  LIKE ZTPMTHD-ZFPNAMC,
     ZFPYDT   LIKE ZTPMTHD-ZFPYDT,
     ZFPWDT   LIKE ZTPMTHD-ZFPWDT,
     ZFUSITR  LIKE ZTPMTHD-ZFUSITR, "> 이자율..
     ZFUSIT   LIKE ZTPMTHD-ZFUSIT,  "> 이자금액..
     ZFUSITC  LIKE ZTPMTHD-ZFUSITC, "> 이자 통화단위..
     ZFBKCH   LIKE ZTPMTHD-ZFBKCH,
     ZFBKCHP  LIKE ZTPMTHD-ZFBKCHP,
     ZFBKCHC  LIKE ZTPMTHD-ZFBKCHC,
     ZFREQNO  LIKE ZTPMTHD-ZFREQNO,
     ZFHBLNO  LIKE ZTPMTHD-ZFHBLNO,
     ZFBLNO   LIKE ZTPMTHD-ZFBLNO,
     ZFISNO   LIKE ZTPMTHD-ZFISNO.
DATA : END OF IT_TAB_NS.

*----------------------------------------------------------------------*
*          SELECTION-SCREEN                                            *
*----------------------------------------------------------------------*
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS :
          S_BUKRS  FOR ZTPMTHD-BUKRS,  "회사코드
          S_BUDAT  FOR ZTPMTHD-BUDAT.  "기간 FROM~TO
SELECTION-SCREEN END OF BLOCK B1.

*----------------------------------------------------------------------*
*          INITIALIZATION.                                             *
*----------------------------------------------------------------------*
INITIALIZATION.                          " 초기값 SETTING
  IF SY-LANGU EQ '3'.
    SET TITLEBAR 'ZIMR80'
        WITH 'BANKER''''S USANCE 기채(起債)발행 실적'.
  ELSE.
    SET TITLEBAR 'ZIMR80'
        WITH 'BANKER''''S USANCE FLOATION OF LOAN RESULT'.
  ENDIF.
  CLEAR:   IT_TAB_Y, IT_TAB_NL, IT_TAB_NS.
  REFRESH: IT_TAB_Y, IT_TAB_NL, IT_TAB_NS.

*----------------------------------------------------------------------*
*          START-OF-SELECTION.                                         *
*----------------------------------------------------------------------*
START-OF-SELECTION.

*>>> READ TABLE
  PERFORM   P1000_READ_TABY.
  PERFORM   P1000_READ_TABNL.
  PERFORM   P1000_READ_TABNS.

*>>> SORT-GROUP:
  SORT IT_TAB_Y  BY EBELN NAME1 ZFREQNO ZFPNAM.
  SORT IT_TAB_NL BY EBELN NAME1 ZFREQNO ZFPNAM.
  SORT IT_TAB_NS BY EBELN NAME1 ZFREQNO ZFPNAM.
*>>> WRITE LIST
*  PERFORM   P3000_WRITE_TOP.
  SET TITLEBAR 'ZIMR80'.
  SET PF-STATUS 'ZIMR80'.
  PERFORM   P3000_WRITE_TABY.
  PERFORM   P3000_WRITE_TABNL.
  PERFORM   P3000_WRITE_TABNS.

*----------------------------------------------------------------------*
*           TOP-OF-PAGE                                                *
*----------------------------------------------------------------------*
TOP-OF-PAGE.
  FORMAT RESET.
  IF SY-LANGU EQ '3'.
    WRITE : /40 '[ BANKER''''S USANCE 기채발행 실적 ]'
                 COLOR COL_HEADING INTENSIFIED OFF,
            /.
  ELSE.
    WRITE : /40 '[ BANKER''''S USANCE FLOATATION OF LOAN RESULT ]'
                 COLOR COL_HEADING INTENSIFIED OFF,
            /.
  ENDIF.
  IF NOT S_BUDAT IS INITIAL.
    WRITE : 75 S_BUDAT-LOW, ' ~ ', S_BUDAT-HIGH.
  ENDIF.
  SKIP.

*----------------------------------------------------------------------*
* User Command
*----------------------------------------------------------------------*
AT USER-COMMAND.
  CASE SY-UCOMM.
    WHEN 'DSPO'.                    " 구매오더조회.
      IF W_TABIX EQ 0.
        MESSAGE E962 .
      ENDIF.
      IF NOT IT_TAB_Y-EBELN IS INITIAL.
        SET PARAMETER ID 'BES' FIELD IT_TAB_Y-EBELN.
        CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
      ENDIF.
      IF NOT IT_TAB_NL-EBELN IS INITIAL.
        SET PARAMETER ID 'BES' FIELD IT_TAB_NL-EBELN.
        CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
      ENDIF.
      IF NOT IT_TAB_NS-EBELN IS INITIAL.
        SET PARAMETER ID 'BES' FIELD IT_TAB_NS-EBELN.
        CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
      ENDIF.

    WHEN 'DSLC'.                    " L/C 조회.
      IF W_TABIX EQ 0.
        MESSAGE E962 .
      ENDIF.
      IF NOT IT_TAB_Y-ZFREQNO IS INITIAL.
        PERFORM P2000_SHOW_LC USING  IT_TAB_Y-ZFREQNO.
      ENDIF.
      IF NOT IT_TAB_NL-ZFREQNO IS INITIAL.
        PERFORM P2000_SHOW_LC USING  IT_TAB_NL-ZFREQNO.
      ENDIF.
      IF NOT IT_TAB_NS-ZFREQNO IS INITIAL.
        PERFORM P2000_SHOW_LC USING  IT_TAB_NS-ZFREQNO.
      ENDIF.

    WHEN 'DSBL'.                    " B/L 조회.
      IF W_TABIX EQ 0.
        MESSAGE E962 .
      ENDIF.
      IF NOT IT_TAB_Y-ZFBLNO IS INITIAL.
        PERFORM P2000_DISP_ZTBL(SAPMZIM09) USING IT_TAB_Y-ZFBLNO.
      ENDIF.
      IF NOT IT_TAB_NL-ZFBLNO IS INITIAL.
        PERFORM P2000_DISP_ZTBL(SAPMZIM09) USING IT_TAB_NL-ZFBLNO.
      ENDIF.
      IF NOT IT_TAB_NS-ZFBLNO IS INITIAL.
        PERFORM P2000_DISP_ZTBL(SAPMZIM09) USING IT_TAB_NS-ZFBLNO.
      ENDIF.

    WHEN 'DSRE'.                    " 인수증 조회.
      SELECT SINGLE * FROM ZTIMIMG00.
      IF ZTIMIMG00-ZFTAXYN EQ 'X'.
        IF W_TABIX EQ 0.
          MESSAGE E962 .
        ENDIF.
        IF NOT IT_TAB_Y-ZFISNO IS INITIAL.
          PERFORM P2000_DISP_ZTBL(SAPMZIM09) USING IT_TAB_Y-ZFISNO.
        ENDIF.
        IF NOT IT_TAB_NL-ZFISNO IS INITIAL.
          PERFORM P2000_DISP_ZTBL(SAPMZIM09) USING IT_TAB_NL-ZFISNO.
        ENDIF.
        IF NOT IT_TAB_NS-ZFISNO IS INITIAL.
          PERFORM P2000_DISP_ZTBL(SAPMZIM09) USING IT_TAB_NS-ZFISNO.
        ENDIF.
      ENDIF.
  ENDCASE.

  CLEAR: W_TABIX, IT_TAB_Y, IT_TAB_NL, IT_TAB_NS.
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_TABY
*&---------------------------------------------------------------------*
FORM P1000_READ_TABY.

  SELECT * FROM ZTPMTHD INTO CORRESPONDING FIELDS OF TABLE IT_TAB_Y
         WHERE BUKRS IN S_BUKRS
           AND BUDAT IN S_BUDAT
           AND ZFLCKN  EQ '2'
           AND ZFUSIRP EQ 'Y'
           AND ZFBKCHP EQ 'Y'.
  LOOP AT IT_TAB_Y.
    W_TABIX = SY-TABIX.
    SELECT SINGLE * FROM LFA1
        WHERE LIFNR EQ IT_TAB_Y-ZFPNBN.
    MOVE LFA1-NAME1 TO  IT_TAB_Y-NAME1.
    MODIFY IT_TAB_Y INDEX W_TABIX.
  ENDLOOP.

ENDFORM.                    " P1000_READ_TABY
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_TABNL
*&---------------------------------------------------------------------*
FORM P1000_READ_TABNL.

  SELECT * FROM ZTPMTHD INTO CORRESPONDING FIELDS OF TABLE IT_TAB_NL
         WHERE BUKRS IN S_BUKRS
           AND BUDAT IN S_BUDAT
           AND ZFLCKN  EQ '2'
           AND ZFUSIRP EQ 'N'
           AND ZFBKCHP EQ 'Y'.
  LOOP AT IT_TAB_NL.
    W_TABIX = SY-TABIX.
    SELECT SINGLE * FROM LFA1
        WHERE LIFNR EQ IT_TAB_NL-ZFPNBN.
    MOVE LFA1-NAME1 TO  IT_TAB_NL-NAME1.
    MODIFY IT_TAB_NL INDEX W_TABIX.
  ENDLOOP.

ENDFORM.                    " P1000_READ_TABNL
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_TABNS
*&---------------------------------------------------------------------*
FORM P1000_READ_TABNS.

  SELECT * FROM ZTPMTHD INTO CORRESPONDING FIELDS OF TABLE IT_TAB_NS
           WHERE BUKRS IN S_BUKRS
             AND BUDAT IN S_BUDAT
             AND ZFLCKN  EQ '2'
             AND ZFUSIRP EQ 'Y'
             AND ZFBKCHP EQ 'N'.

  LOOP AT IT_TAB_NS.
    W_TABIX = SY-TABIX.
    SELECT SINGLE * FROM LFA1
        WHERE LIFNR EQ IT_TAB_NS-ZFPNBN.
    MOVE LFA1-NAME1 TO  IT_TAB_NS-NAME1.
    MODIFY IT_TAB_NS INDEX W_TABIX.
  ENDLOOP.

ENDFORM.                    " P1000_READ_TABNS
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_TABY
*&---------------------------------------------------------------------*
FORM P3000_WRITE_TABY.

  IF SY-LANGU EQ '3'.
    WRITE :/ ' 1) 기채발행 & 이자지급 (선취)'.
  ELSE.
    WRITE :/ ' 1) Floatation of loan & Payment Interest (Preocupation)'.
  ENDIF.
  PERFORM   P3000_WRITE_TOP.
  WRITE :/ SY-ULINE(133).
*>>자료유무CHECK.
  IF IT_TAB_Y IS INITIAL.
    IF SY-LANGU EQ '3'.
      WRITE :/ SY-VLINE, 20 '해당하는 자료가 없습니다.'.
    ELSE.
      WRITE :/ SY-VLINE, 20 'There are no suitable data.'.
    ENDIF.
    WRITE : 133 SY-VLINE.
  ENDIF.
  IF NOT IT_TAB_Y IS INITIAL.
    LOOP AT IT_TAB_Y.
      W_TABIX = SY-TABIX.
******>>홀,짝
      W_MOD = SY-TABIX MOD 2.
      IF W_MOD = 1.
        FORMAT COLOR 2 INTENSIFIED ON.
      ELSEIF W_MOD = 0.
        FORMAT COLOR 2 INTENSIFIED OFF.
      ENDIF.
      WRITE :/ SY-VLINE NO-GAP,(10) IT_TAB_Y-EBELN        NO-GAP,
               SY-VLINE NO-GAP,(20) IT_TAB_Y-NAME1        NO-GAP,
               SY-VLINE NO-GAP,(20) IT_TAB_Y-ZFOPNNO      NO-GAP,
               SY-VLINE NO-GAP,(12) IT_TAB_Y-ZFPNAM
                               CURRENCY IT_TAB_Y-ZFPNAMC  NO-GAP,
                                (3) IT_TAB_Y-ZFPNAMC      NO-GAP,
               SY-VLINE NO-GAP, (8) IT_TAB_Y-ZFPYDT       NO-GAP,
               SY-VLINE NO-GAP, (8) IT_TAB_Y-ZFPWDT       NO-GAP,
               SY-VLINE NO-GAP,(16) '' NO-GAP,
               SY-VLINE NO-GAP,(10) " '' NO-GAP,
                                    IT_TAB_Y-ZFUSITR      NO-GAP,
               SY-VLINE NO-GAP,(12) IT_TAB_Y-ZFUSIT
                               CURRENCY IT_TAB_Y-ZFUSITC  NO-GAP,
                                (3) IT_TAB_Y-ZFUSITC      NO-GAP.
      WRITE : 133 SY-VLINE.
      HIDE: IT_TAB_Y, W_TABIX.
    ENDLOOP.
  ENDIF.
  WRITE :/ SY-ULINE(133). SKIP.
  FORMAT RESET.

ENDFORM.                    " P3000_WRITE_TABY
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_TABNL
*&---------------------------------------------------------------------*
FORM P3000_WRITE_TABNL.

  IF SY-LANGU EQ '3'.
    WRITE :/ ' 2) 기채발생 (후취)'.
  ELSE.
    WRITE :/ ' 2) Floatation of loan (Collect)'.
  ENDIF.
  PERFORM   P3000_WRITE_TOP.
  WRITE :/ SY-ULINE(133).
*>>자료유무CHECK.
  IF IT_TAB_NL IS INITIAL.
    IF SY-LANGU EQ '3'.
      WRITE :/ SY-VLINE, 20 '해당하는 자료가 없습니다.'.
    ELSE.
      WRITE :/ SY-VLINE, 20 'There are no suitable data'.
    ENDIF.
    WRITE : 133 SY-VLINE.
  ENDIF.
  IF NOT IT_TAB_NL IS INITIAL.
    LOOP AT IT_TAB_NL.
      W_TABIX = SY-TABIX.
      W_MOD = SY-TABIX MOD 2.
      IF W_MOD = 1.
        FORMAT COLOR 2 INTENSIFIED ON.
      ELSEIF W_MOD = 0.
        FORMAT COLOR 2 INTENSIFIED OFF.
      ENDIF.
      WRITE :/ SY-VLINE NO-GAP,(10) IT_TAB_NL-EBELN        NO-GAP,
               SY-VLINE NO-GAP,(20) IT_TAB_NL-NAME1        NO-GAP,
               SY-VLINE NO-GAP,(20) IT_TAB_NL-ZFOPNNO      NO-GAP,
               SY-VLINE NO-GAP,(12) IT_TAB_NL-ZFPNAM
                               CURRENCY IT_TAB_NL-ZFPNAMC  NO-GAP,
                                (3) IT_TAB_NL-ZFPNAMC      NO-GAP,
               SY-VLINE NO-GAP, (8) IT_TAB_NL-ZFPYDT       NO-GAP,
               SY-VLINE NO-GAP, (8) IT_TAB_NL-ZFPWDT       NO-GAP,
               SY-VLINE NO-GAP,(16) '' NO-GAP,
                                   "IT_TAB_NL-이자지급기간 NO-GAP,
               SY-VLINE NO-GAP,(10) " '' NO-GAP,
                                    IT_TAB_NL-ZFUSITR      NO-GAP,
               SY-VLINE NO-GAP,(12) IT_TAB_NL-ZFUSIT
                               CURRENCY IT_TAB_NL-ZFUSITC  NO-GAP,
                                (3) IT_TAB_NL-ZFUSITC      NO-GAP.
      WRITE : 133 SY-VLINE.
      HIDE: IT_TAB_NL, W_TABIX.
    ENDLOOP.
  ENDIF.
  WRITE :/ SY-ULINE(133). SKIP.
  FORMAT RESET.
ENDFORM.                    " P3000_WRITE_TABNL
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_TABNS
*&---------------------------------------------------------------------*
FORM P3000_WRITE_TABNS.
  IF SY-LANGU EQ '3'.
    WRITE :/ ' 3) 이자지급 (후취)'.
  ELSE.
    WRITE :/ ' 3) Interest Payment (Collect)'.
  ENDIF.
  PERFORM   P3000_WRITE_TOP.
  WRITE :/ SY-ULINE(133).
*>>자료유무CHECK.
  IF IT_TAB_NS IS INITIAL.
    IF SY-LANGU EQ '3'.
      WRITE :/ SY-VLINE, 20 '해당하는 자료가 없습니다.'.
    ELSE.
      WRITE :/ SY-VLINE, 20 'There are no suitalbe data.'.
    ENDIF.
    WRITE : 133 SY-VLINE.
  ENDIF.
  IF NOT IT_TAB_NS IS INITIAL.
    LOOP AT IT_TAB_NS.
      W_TABIX = SY-TABIX.
      W_MOD = SY-TABIX MOD 2.
      IF W_MOD = 1.
        FORMAT COLOR 2 INTENSIFIED ON.
      ELSEIF W_MOD = 0.
        FORMAT COLOR 2 INTENSIFIED OFF.
      ENDIF.
      WRITE :/ SY-VLINE NO-GAP,(10) IT_TAB_NS-EBELN        NO-GAP,
               SY-VLINE NO-GAP,(20) IT_TAB_NS-NAME1        NO-GAP,
               SY-VLINE NO-GAP,(20) IT_TAB_NS-ZFOPNNO      NO-GAP,
               SY-VLINE NO-GAP,(12) IT_TAB_NS-ZFPNAM
                               CURRENCY IT_TAB_NS-ZFPNAMC  NO-GAP,
                                (3) IT_TAB_NS-ZFPNAMC      NO-GAP,
               SY-VLINE NO-GAP, (8) IT_TAB_NS-ZFPYDT       NO-GAP,
               SY-VLINE NO-GAP, (8) IT_TAB_NS-ZFPWDT       NO-GAP,
               SY-VLINE NO-GAP,(16) '' NO-GAP,
                                "IT_TAB_NS-이자지급기간 NO-GAP,
               SY-VLINE NO-GAP,(10) "'' NO-GAP,
                                    IT_TAB_NS-ZFUSITR      NO-GAP,
               SY-VLINE NO-GAP,(12) IT_TAB_NS-ZFUSIT
                               CURRENCY IT_TAB_NS-ZFUSITC  NO-GAP,
                                (3) IT_TAB_NS-ZFUSITC      NO-GAP.
      WRITE : 133 SY-VLINE.
      HIDE: IT_TAB_NS, W_TABIX.
    ENDLOOP.
  ENDIF.
  WRITE :/ SY-ULINE(133).SKIP.
  FORMAT RESET.
ENDFORM.                    " P3000_WRITE_TABNS
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_TOP
*&---------------------------------------------------------------------*
FORM P3000_WRITE_TOP.

  FORMAT COLOR 1 INTENSIFIED ON.
  IF SY-LANGU EQ '3'.
    WRITE :/ SY-ULINE(133).
    WRITE :/ SY-VLINE NO-GAP,(10) 'P/O NO'           NO-GAP,
             SY-VLINE NO-GAP,(20) '은행명'           NO-GAP,
             SY-VLINE NO-GAP,(20) 'L/C NO'           NO-GAP,
             SY-VLINE NO-GAP,(15) '원  금'           NO-GAP,
             SY-VLINE NO-GAP, (8) '기산일'           NO-GAP,
             SY-VLINE NO-GAP, (8) '만기일'           NO-GAP,
             SY-VLINE NO-GAP,(16) '이자지급기간'     NO-GAP,
             SY-VLINE NO-GAP,(10) '이자율'           NO-GAP,
             SY-VLINE NO-GAP,(15) '이자지급금액'     NO-GAP.
    WRITE : 133 SY-VLINE.
  ELSE.
    WRITE :/ SY-ULINE(133).
    WRITE :/ SY-VLINE NO-GAP,(10) 'P/O No.'          NO-GAP,
             SY-VLINE NO-GAP,(20) 'Bank'             NO-GAP,
             SY-VLINE NO-GAP,(20) 'L/C No.'          NO-GAP,
             SY-VLINE NO-GAP,(15) 'Principal Sum'    NO-GAP,
             SY-VLINE NO-GAP, (8) 'Reckn.Dt'         NO-GAP,
             SY-VLINE NO-GAP, (8) 'Expir.Dt'         NO-GAP,
             SY-VLINE NO-GAP,(16) 'Period Intr.Pymt' NO-GAP,
             SY-VLINE NO-GAP,(10) 'Intr. Rate'       NO-GAP,
             SY-VLINE NO-GAP,(15) 'Intr.Pymt. Amt'   NO-GAP.
    WRITE : 133 SY-VLINE.
  ENDIF.
  FORMAT RESET.

ENDFORM.                    " P3000_WRITE_TOP
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_LC
*&---------------------------------------------------------------------*
FORM P2000_SHOW_LC USING    P_ZFREQNO.

  SET PARAMETER ID 'BES'       FIELD ''.
  SET PARAMETER ID 'ZPOPNNO'   FIELD ''.
  SET PARAMETER ID 'ZPREQNO'   FIELD P_ZFREQNO.
  SET PARAMETER ID 'ZPAMDNO'   FIELD ''.
  EXPORT 'BES'           TO MEMORY ID 'BES'.
  EXPORT 'ZPREQNO'       TO MEMORY ID 'ZPREQNO'.
  EXPORT 'ZPOPNNO'       TO MEMORY ID 'ZPOPNNO'.
  EXPORT 'ZPAMDNO'       TO MEMORY ID 'ZPAMDNO'.

  CALL TRANSACTION 'ZIM03' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_SHOW_LC
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_RED
*&---------------------------------------------------------------------*
FORM P2000_SHOW_RED USING    P_ZFISNO.
  DATA: L_MAX_REDNO LIKE ZTRED-ZFREDNO.
  SELECT MAX( ZFREDNO ) INTO L_MAX_REDNO
        FROM ZTRED
        WHERE ZFISNO = P_ZFISNO.

  SET PARAMETER ID 'ZPREDNO'  FIELD L_MAX_REDNO.
  CALL TRANSACTION 'ZIMA7' AND SKIP FIRST SCREEN.

ENDFORM.                    " P2000_SHOW_RED
