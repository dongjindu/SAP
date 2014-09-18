FUNCTION ZIM_DOANTC_EDI_DATA_RECEIVE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(W_ZFDHENO) LIKE  ZTDHF1-ZFDHENO
*"  EXCEPTIONS
*"      UPDATE_ERROR
*"      NOT_FOUND
*"      NO_REFERENCE
*"      DATE_ERROR
*"----------------------------------------------------------------------
DATA : C_ZFDDFDA1(3),
       WL_DOC_NO        LIKE IT_SAITIN_A-ZFDDFDA,
       WL_DOC_TY(3)     TYPE C,
       W_ZFPNNO         LIKE ZTPMTHD-ZFPNNO.

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
  CLEAR : ZTPMTHD.
  ZTPMTHD-MANDT   = SY-MANDT.      " Client

  LOOP AT IT_SAITIN_S.
    CASE IT_SAITIN_S-ZFDDFDA.
* BGM Seg. ===> 전자문서번호 ( SKIP )
      WHEN '{10'.
* 수신업체, 통지은행 TEXT ( SKIP )
      WHEN '{11'.
* 신용장번호(AAC) 및 계약서번호(CT)
      WHEN '{12'.
        Z_ZFDDSEQ = IT_SAITIN_S-ZFDDSEQ + 1.
        READ TABLE IT_SAITIN_A WITH KEY ZFDDSEQ = Z_ZFDDSEQ.
        CASE IT_SAITIN_A-ZFDDFDA.
           WHEN 'AAC'.   " 신용장번호
              WL_DOC_TY = 'AAC'.
           WHEN 'CT'.    " 계약서번호
              WL_DOC_TY = 'CT'.
           WHEN 'AWB'.   " 항공화물운송장===>(SKIP)
              CONTINUE.
           WHEN 'BM'.    " 선하증권번호  ===>(SKIP)
              CONTINUE.
           WHEN OTHERS.
              CONTINUE.
        ENDCASE.
*>> 문서 번호
        PERFORM READ_TABLE_1   TABLES   IT_SAITIN_A
                               CHANGING Z_ZFDDSEQ.
        WL_DOC_NO           = IT_SAITIN_A-ZFDDFDA.
* 금액관련 정보
      WHEN '{13'.
        Z_ZFDDSEQ = IT_SAITIN_S-ZFDDSEQ + 1.
        READ TABLE IT_SAITIN_A WITH KEY ZFDDSEQ = Z_ZFDDSEQ.
        CASE IT_SAITIN_A-ZFDDFDA.
           WHEN '154'.     " 어음금액
              PERFORM READ_TABLE_1   TABLES   IT_SAITIN_A
                                     CHANGING Z_ZFDDSEQ.
              ZTPMTHD-ZFPNAM      = IT_SAITIN_A-ZFDDFDA.
*>>>>>> CURRENCY
              PERFORM READ_TABLE_1   TABLES   IT_SAITIN_A
                                     CHANGING Z_ZFDDSEQ.
              ZTPMTHD-ZFPNAMC     = IT_SAITIN_A-ZFDDFDA.
           WHEN '304'.     " 기타수수료
              PERFORM READ_TABLE_1   TABLES   IT_SAITIN_A
                                     CHANGING Z_ZFDDSEQ.
              ZTPMTHD-ZFBKCH      = IT_SAITIN_A-ZFDDFDA.
*>>>>>> CURRENCY
              PERFORM READ_TABLE_1   TABLES   IT_SAITIN_A
                                     CHANGING Z_ZFDDSEQ.
              ZTPMTHD-ZFBKCHC     = IT_SAITIN_A-ZFDDFDA.
        ENDCASE.
* DTM ( 통지일자, 최종결제일, 할인일자 )
      WHEN '{14'.
        Z_ZFDDSEQ = IT_SAITIN_S-ZFDDSEQ + 1.
        READ TABLE IT_SAITIN_A WITH KEY ZFDDSEQ = Z_ZFDDSEQ.
        CASE  IT_SAITIN_A-ZFDDFDA.
           WHEN '184'.   "통지일자
              PERFORM READ_TABLE_1   TABLES   IT_SAITIN_A
                                     CHANGING Z_ZFDDSEQ.
              ZTPMTHD-ZFNTDT      = IT_SAITIN_A-ZFDDFDA.
           WHEN '265'.   "최종결제일
              PERFORM READ_TABLE_1   TABLES   IT_SAITIN_A
                                     CHANGING Z_ZFDDSEQ.
              ZTPMTHD-ZFPYDT      = IT_SAITIN_A-ZFDDFDA.
        ENDCASE.
* 기타사항   ===> SKIP
      WHEN '{15'.
* 통지은행명 ===> SKIP
      WHEN '{16'.
    ENDCASE.                           " *  IT_SAITIN_S-ZFDDFDA
  ENDLOOP.                             " ** IT_SAITIN_S
*-----------------------------------------------------------------------
* 구매오더 및 수입의뢰 문서 SELECT 로직 추가할 부분
*   ----> SELECT 후 해당 필드 MOVE처리 추가...
* IF WL_DOC_TY EQ 'AAC'. --> L/C
* ELSEIF WL_DOC_TY EQ 'CT' --> P/O
* ENDIF.
* WL_DOC_NO
*
* IF SY-SUBRC NE 0.
*    RAISE  NO_REFERENCE.   " NOT FOUND시 오류 ?????
* ENDIF.
*-----------------------------------------------------------------------
*>>> 금액 내부 필드값으로 변경...
  IF NOT ZTPMTHD-ZFPNAM IS INITIAL.
     PERFORM    SET_CURR_CONV_TO_INTERNAL CHANGING ZTPMTHD-ZFPNAM
                                                   ZTPMTHD-ZFPNAMC.
  ENDIF.
  IF NOT ZTPMTHD-ZFUSIT IS INITIAL.
     PERFORM    SET_CURR_CONV_TO_INTERNAL CHANGING ZTPMTHD-ZFUSIT
                                                   ZTPMTHD-ZFUSITC.
  ENDIF.
*-----------------------------------------------------------------------
* DATE CONVERT
*-----------------------------------------------------------------------
  IF NOT ZTPMTHD-ZFPYDT IS INITIAL. "--> 결제완료일
     CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
          EXPORTING
                     DATE_EXTERNAL = ZTPMTHD-ZFPYDT
          IMPORTING
                     DATE_INTERNAL = ZTPMTHD-ZFPYDT.

     IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
               RAISING DATE_ERROR.
     ENDIF.
  ENDIF.

  IF NOT ZTPMTHD-ZFNTDT IS INITIAL. "--> 통지일자
     CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
          EXPORTING
                     DATE_EXTERNAL = ZTPMTHD-ZFNTDT
          IMPORTING
                     DATE_INTERNAL = ZTPMTHD-ZFNTDT.

     IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
               RAISING DATE_ERROR.
     ENDIF.
  ENDIF.

*>>> FIELD MOVE
  IF WL_DOC_TY EQ 'AAC'.   "--> L/C
     MOVE : WL_DOC_NO   TO    ZTPMTHD-ZFOPNNO.  " 문서번호
  ELSEIF WL_DOC_TY EQ 'CT'."--> P/O
     MOVE : WL_DOC_NO   TO    ZTPMTHD-EBELN.    " 문서번호
  ENDIF.

  MOVE : SY-UNAME    TO    ZTPMTHD-ERNAM,
         SY-DATUM    TO    ZTPMTHD-CDAT,
         SY-UNAME    TO    ZTPMTHD-UNAM,
         SY-DATUM    TO    ZTPMTHD-UDAT.
*>>> 전자문서 번호 관리하지 않음.
*        W_ZFDHENO   TO    ZTBLINOU-ZFDOCNO.

  SELECT MAX( ZFPNNO ) INTO W_ZFPNNO FROM ZTPMTHD.
  IF W_ZFPNNO IS INITIAL.
     W_ZFPNNO = '0000000001'.
  ELSE.
     W_ZFPNNO = W_ZFPNNO + 1.
  ENDIF.

  INSERT ZTPMTHD.

  IF SY-SUBRC NE  0.
     RAISE    UPDATE_ERROR.
  ELSE.
     MESSAGE  S261  WITH  ZTPMTHD-ZFPNNO.
  ENDIF.

ENDFUNCTION.
