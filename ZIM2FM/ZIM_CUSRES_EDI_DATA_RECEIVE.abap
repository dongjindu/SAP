FUNCTION ZIM_CUSRES_EDI_DATA_RECEIVE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(W_ZFDHENO) LIKE  ZTDHF1-ZFDHENO
*"  EXCEPTIONS
*"      UPDATE_ERROR
*"      NOT_FOUND
*"      NO_REFERENCE
*"      NOT_TYPE
*"----------------------------------------------------------------------
DATA : C_ZFDDFDA1(3),
       WL_VIA(1)        TYPE C,
       WL_TYPE(3)       TYPE C,
       WL_DOC_NO(70)    TYPE C,
       WL_DOC_TY(3)     TYPE C.

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
* BGM Seg. ===> 보세운송 신고/신청번호
      WHEN '{10'.
        Z_ZFDDSEQ = IT_SAITIN_S-ZFDDSEQ + 1.
        READ TABLE IT_SAITIN_A WITH KEY ZFDDSEQ = Z_ZFDDSEQ.
* 5DF : 완료통보
* 5AF : 접수통보
        CASE IT_SAITIN_A-ZFDDFDA.
           WHEN '5DF'.      ">완료통보
           WHEN '5AF'.      ">접수통보
           WHEN OTHERS.     ">ERROR
              RAISE   NOT_TYPE.
        ENDCASE.
        WL_TYPE = IT_SAITIN_A-ZFDDFDA(3).
*>>> NEXT LINE( 문서번호 )
        PERFORM READ_TABLE_1   TABLES   IT_SAITIN_A
                               CHANGING Z_ZFDDSEQ.
        WL_DOC_NO = IT_SAITIN_A-ZFDDFDA.
* 세관 정보( SEND시 정보를 보냄 --> 담당자는 SKIP )
      WHEN '{11'.
* 통보세관 정보 --> SKIP
      WHEN '{12'.
* 관세청통보일시 및 접수(완료)일시 --> SKIP
      WHEN '{13'.
* 작업 종류 --> SKIP
      WHEN '{14'.
* 심사결과내역 --> SKIP
      WHEN '{15'.
* 세관이 신청서 접수시 부여한 접수번호 --> SKIP
      WHEN '{1A'.
* 문서종류(신청문서) -->
*>> 5DA:환급신청서,     5DB:기초원재료납세증명서
*>> 5DC:평균세액증명서  5DD:정산신고서
*>> 5DE:분할증명서
      WHEN '{1B'.
        Z_ZFDDSEQ = IT_SAITIN_S-ZFDDSEQ + 1.
        READ TABLE IT_SAITIN_A WITH KEY ZFDDSEQ = Z_ZFDDSEQ.
        WL_DOC_TY = IT_SAITIN_A-ZFDDFDA.
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.

*>> 해당 문서 SELECT
  CASE WL_DOC_TY.
     WHEN '5DA'.  "환급신청
     WHEN '5DB'.  "기초원재료납세증명서
     WHEN '5DC'.  "평균세액증명서
     WHEN '5DD'.  "정산신고서
     WHEN '5DE'.  "분할증명서
     WHEN OTHERS. "
  ENDCASE.








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
