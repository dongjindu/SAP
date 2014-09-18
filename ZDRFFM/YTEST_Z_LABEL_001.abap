FUNCTION YTEST_Z_LABEL_001.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(P_HANGUL) TYPE  CHAR200
*"     REFERENCE(P_X) TYPE  NUM4
*"     REFERENCE(P_Y) TYPE  NUM4
*"     REFERENCE(P_X_SIZE) TYPE  NUM2 DEFAULT '1'
*"     REFERENCE(P_Y_SIZE) TYPE  NUM2 DEFAULT '1'
*"     REFERENCE(P_WIDTH) TYPE  NUM4 DEFAULT '15'
*"     REFERENCE(P_PRINT) TYPE  CHAR1 DEFAULT 'X'
*"  TABLES
*"      P_SCRIPT STRUCTURE  YTEST_ZLAB01S OPTIONAL
*"----------------------------------------------------------------------
  TABLES : YTEST_ZLAB01.

  DATA: STA TYPE I,          " HEXA로 읽을위치
        POS TYPE I,          " Space로 조정할 위치
        FLG,                 " 현재 2Byte Pair 상태
        LEN TYPE I,          " Input String 길이
        HAN(2).              " 한글 1 자

  FIELD-SYMBOLS: <F>,        " Input String의 Hexa Value
                 <S> TYPE X. " 비교할 1byte Charater의 Hexa값

  DATA : TP_X(4),   "찍을 위치 X
         TP_Y(4),   "찍을 위치 Y
         TP_X_SIZE TYPE NUM4, "영문 X SIZE
         TP_Y_SIZE TYPE NUM4. "영문 Y SIZE

* 변수 초기화
  CLEAR : STA, POS, FLG, LEN, HAN," <F>, <S>,
          TP_X, TP_Y.
  REFRESH : P_SCRIPT. CLEAR : P_SCRIPT.

* 찍을 내용의 전체 길이 계산
  LEN = STRLEN( P_HANGUL ).
  CHECK LEN > 0.

* 입력 받은 값을 헥사로 변환 (한글과 영문 구분을 위해)
  ASSIGN P_HANGUL TO <F> TYPE 'X'.

* LOOPING 시작
  DO LEN TIMES. "전체 길이 만큼 회전

*   읽을 포인트
    STA = SY-INDEX - 1.

*   1 BYTE 읽는다.
    ASSIGN <F>+STA(1) TO <S>.

*   읽은 값이 2 BYTE 인지 1 BYTE 인지를 구분하여
*   해당 로직별로 처리한다.
    IF <S> >= '80'.  " 2 BYTE 문자
*     2 BYTE 인 경우 : 2 BYTE 시작에서는 그냥 SKIP 하고
*                      2 BYTE가 끝나는 시점에서 읽고 쓴다.
      IF FLG = SPACE.  " 2 byte 시작 시점 (그냥 Return)
        FLG = 'X'.
      ELSE.            " 2 byte 끝 시점 (인쇄)
        FLG = SPACE.
*       읽을 위치 계산
        POS = STA - 1.
*       한글 1자(2byte)를 읽는다.
        HAN = P_HANGUL+POS(2).
*       해당 한글의 Image값을 읽어 온다.
        SELECT SINGLE * FROM YTEST_ZLAB01
          WHERE ZFONT = '1'
          AND   ZHANG = HAN.
*       읽은 한글 Image를 프린터로 보낸다.
        WRITE : / YTEST_ZLAB01-ZHANC(50) NO-GAP,
                / YTEST_ZLAB01-ZHANC+50(50) NO-GAP,
                / YTEST_ZLAB01-ZHANC+100(50) NO-GAP,
                / YTEST_ZLAB01-ZHANC+150(18) NO-GAP.
*       해당 한글 Image 를 라벨로 출력한다.
        TP_X = P_X + POS * P_WIDTH. "가로 위치
        TP_Y = P_Y.                 "세로 위치
        IF P_PRINT = 'X'.
          WRITE : / '^FO'         NO-GAP,
                     TP_X         NO-GAP,
                    ','           NO-GAP,
                     TP_Y         NO-GAP,
                  / '^XGR:'       NO-GAP,
                     YTEST_ZLAB01-ZINDX NO-GAP,
                    ','           NO-GAP,
                     P_X_SIZE     NO-GAP,
                    ','           NO-GAP,
                     P_Y_SIZE     NO-GAP,
                    '^FS'         NO-GAP.
        ELSE.
          CONCATENATE '^FO' TP_X ',' TP_Y INTO P_SCRIPT-SCRIPT.
          APPEND P_SCRIPT.
          CONCATENATE '^XGR:' YTEST_ZLAB01-ZINDX ',' P_X_SIZE ','
                      P_Y_SIZE '^FS' INTO P_SCRIPT-SCRIPT.
          APPEND P_SCRIPT.
        ENDIF.
      ENDIF.
    ELSE.            " 1 BYTE 문자
*     1 BYTE 인 경우 : 1 BYTE 시작에서 그냥 인쇄.
*     읽을 위치 계산
      POS = STA.
*     1자(1byte)를 읽는다.
      HAN = P_HANGUL+POS(1).
*     해당 글자를 라벨로 출력한다.
      TP_X = P_X + POS * P_WIDTH. "가로 위치
      TP_Y = P_Y + 2.             "세로 위치
      TP_X_SIZE = P_X_SIZE * '14'.
      TP_Y_SIZE = P_Y_SIZE * '28'.
      IF P_PRINT = 'X'.
        WRITE : / '^FO'         NO-GAP,
                   TP_X         NO-GAP,
                  ','           NO-GAP,
                   TP_Y         NO-GAP,
                / '^A0'         NO-GAP,
                   TP_X_SIZE    NO-GAP, "14 배수
                   ','          NO-GAP,
                   TP_Y_SIZE    NO-GAP,  "28 배수
                / '^FD'         NO-GAP,
                   HAN(1)       NO-GAP,
                  '^FS'         NO-GAP.
      ELSE.
        CONCATENATE '^FO' TP_X ',' TP_Y INTO P_SCRIPT-SCRIPT.
        APPEND P_SCRIPT.
        CONCATENATE '^A0' TP_X_SIZE ',' TP_Y_SIZE
                    INTO P_SCRIPT-SCRIPT.
        APPEND P_SCRIPT.
        CONCATENATE '^FD' HAN(1) '^FS' INTO P_SCRIPT-SCRIPT.
        APPEND P_SCRIPT.
      ENDIF.
    ENDIF.

  ENDDO.
ENDFUNCTION.
