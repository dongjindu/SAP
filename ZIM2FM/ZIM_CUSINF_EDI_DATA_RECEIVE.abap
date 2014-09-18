FUNCTION ZIM_CUSINF_EDI_DATA_RECEIVE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(W_ZFDHENO) LIKE  ZTDHF1-ZFDHENO
*"  EXCEPTIONS
*"      UPDATE_ERROR
*"      NOT_FOUND
*"      NO_REFERENCE
*"      REFLECT
*"      DATE_ERROR
*"----------------------------------------------------------------------
DATA : C_ZFDDFDA1(3),
       WL_VIA(1)        TYPE C,
       WL_ZTBLINOU      LIKE ZTBLINOU,
       WL_ZFHBLNO       LIKE ZTBL-ZFHBLNO.

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
*  ZTBL-ZFDOCNO = W_ZFDHENO.     " 전자문서 번호

  LOOP AT IT_SAITIN_S.
    CASE IT_SAITIN_S-ZFDDFDA.
* BGM Seg. ===> 보세운송 신고/신청번호
      WHEN '{10'.
        Z_ZFDDSEQ = IT_SAITIN_S-ZFDDSEQ + 2.
        READ TABLE IT_SAITIN_A WITH KEY ZFDDSEQ = Z_ZFDDSEQ.
        ZTBLINOU-ZFBTRNO = IT_SAITIN_A-ZFDDFDA.
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
              CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
                   EXPORTING
                        DATE_EXTERNAL = IT_SAITIN_A-ZFDDFDA
                   IMPORTING
                        DATE_INTERNAL = ZTBLINOU-ZFTDDT .
              IF SY-SUBRC <> 0.
                 MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
                       RAISING DATE_ERROR.
              ENDIF.
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
*       PERFORM    SET_UNIT_CONV_TO_INTERNAL CHANGING ZTBLINOU-ZFPKCNM.
* 중량 단위
      WHEN '{23'.
        Z_ZFDDSEQ = IT_SAITIN_S-ZFDDSEQ + 1.
        READ TABLE IT_SAITIN_A WITH KEY ZFDDSEQ = Z_ZFDDSEQ.
        IF IT_SAITIN_A-ZFDDFDA = 'WT'.
           PERFORM READ_TABLE_1   TABLES   IT_SAITIN_A
                                  CHANGING Z_ZFDDSEQ.
           ZTBLINOU-ZFWEINM    = IT_SAITIN_A-ZFDDFDA.
*>>> UNIT ISO CODE ===> 내부코드로 변환
*       PERFORM    SET_UNIT_CONV_TO_INTERNAL CHANGING ZTBLINOU-ZFWEINM.
* 중량
           PERFORM READ_TABLE_1   TABLES   IT_SAITIN_A
                                  CHANGING Z_ZFDDSEQ.
           ZTBLINOU-ZFWEIG     = IT_SAITIN_A-ZFDDFDA.
        ENDIF.

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
*-----------------------------------------------------------------------
*>>> KSB 2000/12/07 추가
         WL_ZFHBLNO = IT_SAITIN_A-ZFDDFDA.
*>>> KSB 2000/12/07 막음 ==> 마지막으로 이동
* 중복시 최근에 생성한 B/L No.
*        SELECT MAX( ZFBLNO ) INTO ZTBLINOU-ZFBLNO FROM ZTBL
*                             WHERE ZFHBLNO  EQ IT_SAITIN_A-ZFDDFDA.
*
*        IF ZTBLINOU-ZFBLNO IS INITIAL.
*           PERFORM   P3000_BL_CREATE  USING IT_SAITIN_A-ZFDDFDA
*                                            ZTBL.
*           ZTBLINOU-ZFBLNO = ZTBL-ZFBLNO.
*        ELSE.
*           SELECT SINGLE * FROM ZTBL WHERE ZFBLNO EQ ZTBLINOU-ZFBLNO.
*           IF SY-SUBRC NE 0.
*              RAISE   NO_REFERENCE.
*           ELSE.
*              IF ZTBL-ZFETA(4) NE SY-DATUM(4) AND
*                 ZTBL-ZFETA(4) NE '0000'.
*                 PERFORM   P3000_BL_CREATE  USING IT_SAITIN_A-ZFDDFDA
*                                                  ZTBL.
*                 ZTBLINOU-ZFBLNO = ZTBL-ZFBLNO.
*              ENDIF.
*           ENDIF.
*
*        ENDIF.
*
*        SELECT MAX( ZFBTSEQ ) INTO ZTBLINOU-ZFBTSEQ FROM ZTBLINOU
*                              WHERE ZFBLNO  EQ ZTBLINOU-ZFBLNO.
*        IF ZTBLINOU-ZFBTSEQ IS INITIAL.
*           ZTBLINOU-ZFBTSEQ = '00001'.
*        ELSE.
*----> 2000/10/12 안덕기 과장 요청으로 막음(반입예정정보가 존재하면)
*           SELECT SINGLE * INTO   WL_ZTBLINOU FROM ZTBLINOU
*                           WHERE  ZFBLNO  EQ ZTBLINOU-ZFBLNO
*                           AND    ZFBTSEQ EQ ZTBLINOU-ZFBTSEQ.
*
*           IF ZTBLINOU-ZFGMNO EQ WL_ZTBLINOU-ZFGMNO.
*              RAISE  REFLECT.
**           ELSE.
**              ZTBLINOU-ZFBTSEQ = ZTBLINOU-ZFBTSEQ + 1.
*           ENDIF.
*        ENDIF.
*-----------------------------------------------------------------------
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

*-----------------------------------------------------------------------
*>>>KSB 2000/12/07 이동 작업
* 중복시 최근에 생성한 B/L No.
  SELECT MAX( ZFBLNO ) INTO ZTBLINOU-ZFBLNO FROM ZTBL
                            WHERE ZFHBLNO  EQ WL_ZFHBLNO.

  IF ZTBLINOU-ZFBLNO IS INITIAL.
     PERFORM   P3000_BL_CREATE  USING WL_ZFHBLNO
                                      ZTBL.
     ZTBLINOU-ZFBLNO = ZTBL-ZFBLNO.
  ELSE.
     SELECT SINGLE * FROM ZTBL WHERE ZFBLNO EQ ZTBLINOU-ZFBLNO.
     IF SY-SUBRC NE 0.
        RAISE   NO_REFERENCE.
     ELSE.
        IF ZTBL-ZFETA(4) NE SY-DATUM(4) AND
           ZTBL-ZFETA(4) NE '0000'.
           PERFORM   P3000_BL_CREATE  USING IT_SAITIN_A-ZFDDFDA
                                            ZTBL.
           ZTBLINOU-ZFBLNO = ZTBL-ZFBLNO.
        ENDIF.
     ENDIF.

  ENDIF.

  SELECT MAX( ZFBTSEQ ) INTO ZTBLINOU-ZFBTSEQ FROM ZTBLINOU
                        WHERE ZFBLNO  EQ ZTBLINOU-ZFBLNO.
  IF ZTBLINOU-ZFBTSEQ IS INITIAL.
     ZTBLINOU-ZFBTSEQ = '00001'.
  ELSE.
*----> 2000/10/12 안덕기 과장 요청으로 막음(반입예정정보가 존재하면)
      SELECT SINGLE * INTO   WL_ZTBLINOU FROM ZTBLINOU
                      WHERE  ZFBLNO  EQ ZTBLINOU-ZFBLNO
                      AND    ZFBTSEQ EQ ZTBLINOU-ZFBTSEQ.

      IF ZTBLINOU-ZFGMNO EQ WL_ZTBLINOU-ZFGMNO.
         RAISE  REFLECT.
      ENDIF.
  ENDIF.
*-----------------------------------------------------------------------

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


FORM   P3000_BL_CREATE    USING    P_HBLNO
                                   ZTBL    STRUCTURE   ZTBL.

   CLEAR : ZTBL.

   CALL FUNCTION 'ZIM_NUMBER_GET_NEXT'
         EXPORTING
               ZFREQTY         =    'BL'
         IMPORTING
               ZFREQNO         =    ZTBL-ZFBLNO
         EXCEPTIONS
                NOT_INPUT       = 1
                NOT_TYPE        = 2
                NOT_RANGE       = 3
                NOT_FOUND       = 4
                ERROR_DUPLICATE = 8.

   CASE SY-SUBRC.
         WHEN 1.     MESSAGE    E012.
         WHEN 2.     MESSAGE    E013      WITH  'BL'.
         WHEN 3.     MESSAGE    E014      WITH  ZTBL-ZFBLNO.
         WHEN 4.     MESSAGE    E964.
         WHEN 8.     MESSAGE    E015      WITH  ZTBL-ZFBLNO.
   ENDCASE.

   MOVE : SY-MANDT      TO   ZTBL-MANDT,
          SY-DATUM      TO   ZTBL-CDAT,
          P_HBLNO       TO   ZTBL-ZFHBLNO,
         'KG'           TO  ZTBL-ZFNEWTM,          " KG
         'KG'           TO  ZTBL-ZFTOWTM,          " KG
         'M3'           TO  ZTBL-ZFTOVLM,          " 용적
          SY-DATUM      TO   ZTBL-UDAT,
          SY-UNAME      TO   ZTBL-ERNAM,
          SY-UNAME      TO   ZTBL-UNAM.

   INSERT     ZTBL.

ENDFORM.
