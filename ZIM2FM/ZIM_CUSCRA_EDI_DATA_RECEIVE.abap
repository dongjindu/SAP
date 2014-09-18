FUNCTION ZIM_CUSCRA_EDI_DATA_RECEIVE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(W_ZFDHENO) LIKE  ZTDHF1-ZFDHENO
*"  EXCEPTIONS
*"      UPDATE_ERROR
*"      NOT_FOUND
*"      NO_REFERENCE
*"----------------------------------------------------------------------
DATA : C_ZFDDFDA1(3),
       WL_VIA(1)        TYPE C,
       WL_.

  SELECT SINGLE * FROM ZTDHF1 WHERE ZFDHENO EQ W_ZFDHENO.
  IF SY-SUBRC NE 0.
     RAISE   NOT_FOUND.
  ENDIF.

  REFRESH: IT_SAITIN_A, IT_SAITIN_S.
* 전체 SELECT
  SELECT *  FROM ZTDDF1
            APPENDING CORRESPONDING FIELDS OF TABLE IT_SAITIN_A
            WHERE ZFDDENO = W_ZFDHENO.
* 시작점 SELECT
  C_ZFDDFDA1 = '{%'.
  SELECT *  APPENDING CORRESPONDING FIELDS OF TABLE IT_SAITIN_S
            FROM ZTDDF1
            WHERE ZFDDENO EQ    W_ZFDHENO
            AND   ZFDDFDA LIKE  C_ZFDDFDA1.

*-----------------------------------------------------------------------
* DATA MOVE
*-----------------------------------------------------------------------
  CLEAR : ZTBLINOU.
  ZTBL-MANDT   = SY-MANDT.      " Client
  ZTBL-ZFDOCNO = W_ZFDHENO.     " 전자문서 번호

  LOOP AT IT_SAITIN_S.
    CASE IT_SAITIN_S-ZFDDFDA.
* BGM Seg. ===> 전자문서번호(신청인부호+연도+일련번호)
      WHEN '{10'.
        Z_ZFDDSEQ = IT_SAITIN_S-ZFDDSEQ + 2.
        READ TABLE IT_SAITIN_A WITH KEY ZFDDSEQ = Z_ZFDDSEQ.
*        ZTBLOUR-ZFBTRNO = IT_SAITIN_A-ZFDDFDA.
* ?????
      WHEN '{11'.
* 운송기한일
      WHEN '{12'.
        Z_ZFDDSEQ = IT_SAITIN_S-ZFDDSEQ + 1.
        READ TABLE IT_SAITIN_A WITH KEY ZFDDSEQ = Z_ZFDDSEQ.
        CASE IT_SAITIN_A-ZFDDFDA.
           WHEN '137'.   " ????
           WHEN '75'.    " 운송기한일
              PERFORM READ_TABLE_1   TABLES   IT_SAITIN_A
                                     CHANGING Z_ZFDDSEQ.
              ZTBLINOU-ZFTDDT     = IT_SAITIN_A-ZFDDFDA.
        ENDCASE.
* 보세구역 ( 발송지 / 최종목적지 )
      WHEN '{13'.
        Z_ZFDDSEQ = IT_SAITIN_S-ZFDDSEQ + 1.
        READ TABLE IT_SAITIN_A WITH KEY ZFDDSEQ = Z_ZFDDSEQ.
        CASE IT_SAITIN_A-ZFDDFDA.
           WHEN '5'.     " 발송지 보세구역 부호
              PERFORM READ_TABLE_1   TABLES   IT_SAITIN_A
                                     CHANGING Z_ZFDDSEQ.
              ZTBLINOU-ZFDBNAR    = IT_SAITIN_A-ZFDDFDA.
           WHEN '20'.    " 최종도착지 보세구역
              PERFORM READ_TABLE_1   TABLES   IT_SAITIN_A
                                     CHANGING Z_ZFDDSEQ.
              ZTBLINOU-ZFABNAR    = IT_SAITIN_A-ZFDDFDA.
        ENDCASE.
* REF Seg. ( 적하목록 관리번호, MSN, HSN )
      WHEN '{16'.
        Z_ZFDDSEQ = IT_SAITIN_S-ZFDDSEQ + 2.
        READ TABLE IT_SAITIN_A WITH KEY ZFDDSEQ = Z_ZFDDSEQ.
        ZTBLINOU-ZFGMNO     = IT_SAITIN_A-ZFDDFDA.
* MSN
        PERFORM READ_TABLE_1   TABLES   IT_SAITIN_A
                               CHANGING Z_ZFDDSEQ.
        ZTBLINOU-ZFMSN      = IT_SAITIN_A-ZFDDFDA.
* HSN
        PERFORM READ_TABLE_1   TABLES   IT_SAITIN_A
                               CHANGING Z_ZFDDSEQ.
        ZTBLINOU-ZFHSN      = IT_SAITIN_A-ZFDDFDA.
*-----------------------------------------------------------------------
* 상품명세 ( 5 라인 )  ===> DB 미반영
*     WHEN '{21'.
*-----------------------------------------------------------------------
* 포장단위
      WHEN '{22'.
        Z_ZFDDSEQ = IT_SAITIN_S-ZFDDSEQ + 1.
        READ TABLE IT_SAITIN_A WITH KEY ZFDDSEQ = Z_ZFDDSEQ.
        ZTBLINOU-ZFPKCN     = IT_SAITIN_A-ZFDDFDA.
        PERFORM READ_TABLE_1   TABLES   IT_SAITIN_A
                               CHANGING Z_ZFDDSEQ.
        ZTBLINOU-ZFPKCNM    = IT_SAITIN_A-ZFDDFDA.
*>>> UNIT ISO CODE ===> 내부코드로 변환( PKG    단위 )
        PERFORM    SET_UNIT_CONV_TO_INTERNAL CHANGING ZTBLINOU-ZFPKCNM.
* 중량 단위
      WHEN '{23'.
        Z_ZFDDSEQ = IT_SAITIN_S-ZFDDSEQ + 2.
        READ TABLE IT_SAITIN_A WITH KEY ZFDDSEQ = Z_ZFDDSEQ.
        ZTBLINOU-ZFWEINM    = IT_SAITIN_A-ZFDDFDA.
*>>> UNIT ISO CODE ===> 내부코드로 변환
        PERFORM    SET_UNIT_CONV_TO_INTERNAL CHANGING ZTBLINOU-ZFWEINM.
* 중량
        PERFORM READ_TABLE_1   TABLES   IT_SAITIN_A
                               CHANGING Z_ZFDDSEQ.
        ZTBLINOU-ZFWEIG     = IT_SAITIN_A-ZFDDFDA.
*-----------------------------------------------------------------------
* 발송지 보세구역 부호  ===> db 미반영
*-----------------------------------------------------------------------
*     WHEN '{24'.
*-----------------------------------------------------------------------

*-----------------------------------------------------------------------
* 컨터이너 번호          ===> db 미반영
*-----------------------------------------------------------------------
*     WHEN '{25'.
*-----------------------------------------------------------------------
*     WHEN '{30'.         ===> ??????? db 미반영

* B/L No. ( House B/L No. )
      WHEN '{26'.
         Z_ZFDDSEQ = IT_SAITIN_S-ZFDDSEQ + 2.
         READ TABLE IT_SAITIN_A WITH KEY ZFDDSEQ = Z_ZFDDSEQ.
* 중복시 최근에 생성한 B/L No.
         SELECT MAX( ZFBLNO ) INTO ZTBLINOU-ZFBLNO FROM ZTBL
                              WHERE ZFHBLNO  EQ IT_SAITIN_A-ZFDDFDA.
         IF ZTBLINOU-ZFBLNO IS INITIAL.
            RAISE   NO_REFERENCE.
         ENDIF.

         SELECT MAX( ZFBTSEQ ) INTO ZTBLINOU-ZFBTSEQ FROM ZTBLINOU
                               WHERE ZFBLNO  EQ ZTBLINOU-ZFBLNO.
         IF ZTBLINOU-ZFBTSEQ IS INITIAL.
            ZTBLINOU-ZFBTSEQ = '00001'.
         ELSE.
            ZTBLINOU-ZFBTSEQ = ZTBLINOU-ZFBTSEQ + 1.
         ENDIF.
      WHEN '{28'.
         Z_ZFDDSEQ = IT_SAITIN_S-ZFDDSEQ + 1.
         READ TABLE IT_SAITIN_A WITH KEY ZFDDSEQ = Z_ZFDDSEQ.
         CASE IT_SAITIN_A-ZFDDFDA.
           WHEN 'CZ'.        " 송하인
             PERFORM READ_TABLE_4   TABLES   IT_SAITIN_A
                                    CHANGING Z_ZFDDSEQ.
             ZTBLINOU-ZFGSNM   = IT_SAITIN_A-ZFDDFDA.
* DB 미반영
           WHEN 'CN'.        " 수하인
         ENDCASE.                             " * IT_SAITIN_A-ZFDDFDA

    ENDCASE.                           " *  IT_SAITIN_S-ZFDDFDA
  ENDLOOP.                             " ** IT_SAITIN_S
* 발송지 내부코드
  PERFORM   P2000_GET_ZSIMIMG03_INT_CODE   USING    ZTBLINOU-ZFDBNAR
                                           CHANGING ZTBLINOU-ZFDBNARC.
* 도착지 내부코드
  PERFORM   P2000_GET_ZSIMIMG03_INT_CODE   USING    ZTBLINOU-ZFABNAR
                                           CHANGING ZTBLINOU-ZFABNARC.

* 수신일
  MOVE : SY-DATUM    TO    ZTBLINOU-ZFRCDT,
         SY-UNAME    TO    ZTBLINOU-ERNAM,
         SY-DATUM    TO    ZTBLINOU-CDAT,
         SY-UNAME    TO    ZTBLINOU-UNAM,
         SY-DATUM    TO    ZTBLINOU-UDAT,
         W_ZFDHENO   TO    ZTBLINOU-ZFDOCNO.

  INSERT ZTBLINOU.

  IF SY-SUBRC NE  0.
     RAISE    UPDATE_ERROR.
  ELSE.
     MESSAGE  S042  WITH  ZTBLINOU-ZFBLNO ZTBLINOU-ZFBTSEQ.
  ENDIF.

  ZTDHF1-ZFDHAPP = 'Y'.
  UPDATE  ZTDHF1.

ENDFUNCTION.
