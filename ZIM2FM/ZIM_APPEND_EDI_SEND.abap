FUNCTION ZIM_APPEND_EDI_SEND.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(W_ZFREQNO) LIKE  ZTINS-ZFREQNO
*"     VALUE(W_ZFINSEQ) LIKE  ZTINS-ZFINSEQ
*"     VALUE(W_ZFAMDNO) LIKE  ZTINS-ZFAMDNO
*"     VALUE(W_ZFDHENO) LIKE  ZTDHF1-ZFDHENO
*"  EXCEPTIONS
*"      DB_ERROR
*"----------------------------------------------------------------------
DATA : TMP_ZFAMDNO   LIKE   ZTREQST-ZFAMDNO.

 CALL  FUNCTION 'ZIM_GET_INSURANCE_DOC'
    EXPORTING
       ZFREQNO             =          W_ZFREQNO
       ZFINSEQ             =          W_ZFINSEQ
       ZFAMDNO             =          W_ZFAMDNO
    IMPORTING
       W_ZTINS             =          ZTINS
       W_ZTINSRSP          =          ZTINSRSP
       W_ZTINSSG3          =          ZTINSSG3
    TABLES
        IT_ZSINSAGR        =          IT_ZSINSAGR
        IT_ZSINSAGR_ORG    =          IT_ZSINSAGR_ORG
        IT_ZSINSSG2        =          IT_ZSINSSG2
        IT_ZSINSSG2_ORG    =          IT_ZSINSSG2_ORG
        IT_ZSINSSG5        =          IT_ZSINSSG5
        IT_ZSINSSG5_ORG    =          IT_ZSINSSG5_ORG
     EXCEPTIONS
        NOT_FOUND         =    4
        NOT_INPUT         =    8.

 CASE SY-SUBRC.
    WHEN 4.
       MESSAGE E169 WITH ZTINS-ZFREQNO.
    WHEN 8.
       MESSAGE E173.
 ENDCASE.
*-----------------------------------------------------------------------
* 변경전 문서 조회
*-----------------------------------------------------------------------
 TMP_ZFAMDNO = W_ZFAMDNO - 1.

 CALL  FUNCTION 'ZIM_GET_INSURANCE_DOC'
    EXPORTING
       ZFREQNO             =          W_ZFREQNO
       ZFINSEQ             =          W_ZFINSEQ
       ZFAMDNO             =          TMP_ZFAMDNO
    IMPORTING
       W_ZTINS             =          ZTINS_OLD
       W_ZTINSRSP          =          ZTINSRSP_OLD
       W_ZTINSSG3          =          ZTINSSG3_OLD
    TABLES
        IT_ZSINSAGR        =          IT_ZSINSAGR_OLD
        IT_ZSINSSG2        =          IT_ZSINSSG2_OLD
        IT_ZSINSSG5        =          IT_ZSINSSG5_OLD
     EXCEPTIONS
        NOT_FOUND         =    4
        NOT_INPUT         =    8.

*-----------------------------------------------------------------------
* internal Table Append
*-----------------------------------------------------------------------
   REFRESH : IT_ZTDDF1.
   CLEAR : IT_ZTDDF1.
   IT_ZTDDF1-ZFDDENO = W_ZFDHENO.
*>>> 전자문서 시작
   IT_ZTDDF1-ZFDDFDA = '{10'.            APPEND IT_ZTDDF1.
   IT_ZTDDF1-ZFDDFDA = '4BA'.            APPEND IT_ZTDDF1.
   IT_ZTDDF1-ZFDDFDA = W_ZFDHENO.        APPEND IT_ZTDDF1.
   IT_ZTDDF1-ZFDDFDA = ZTINS-ZFEDFU.     APPEND IT_ZTDDF1.
   IT_ZTDDF1-ZFDDFDA = ''.               APPEND IT_ZTDDF1.
   IT_ZTDDF1-ZFDDFDA = '}10'.            APPEND IT_ZTDDF1.
*>>> 보험증권번호
   IT_ZTDDF1-ZFDDFDA = '{11'.            APPEND IT_ZTDDF1.
   IT_ZTDDF1-ZFDDFDA = '4AA'.            APPEND IT_ZTDDF1.
   IT_ZTDDF1-ZFDDFDA = ZTINS_OLD-ZFINNO. APPEND IT_ZTDDF1.
   IT_ZTDDF1-ZFDDFDA = '}11'.            APPEND IT_ZTDDF1.

*>>> 배서유형 구분자
*-----------------------------------------------------------------------
* 통관금액 배서여부( Duty Amount ) ==> 미반영 ( ZTINS-ZFDUYN  : A1 )
* 보험료 배서여부                  ==> 미반영 ( ZTINS-ZFINAYN : A2 )
* 보험금액 배서여부                ==> 미반영 ( ZTINS-ZFTAMYN : A3 )
*-----------------------------------------------------------------------
* 송장금액 배서여부
   IF ZTINS-ZFIVYN EQ 'X'.
      IT_ZTDDF1-ZFDDFDA = '{12'.            APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = 'A0'.             APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = '4AA'.            APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = '}12'.            APPEND IT_ZTDDF1.
   ENDIF.
* 희망이익율 배서여부
   IF ZTINS-ZFPEYN EQ 'X'.
      IT_ZTDDF1-ZFDDFDA = '{12'.            APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = 'A4'.             APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = '4AA'.            APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = '}12'.            APPEND IT_ZTDDF1.
   ENDIF.
* 관세율 배서여부
   IF ZTINS-ZFPDYN EQ 'X'.
      IT_ZTDDF1-ZFDDFDA = '{12'.            APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = 'A5'.             APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = '4AA'.            APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = '}12'.            APPEND IT_ZTDDF1.
   ENDIF.
* 출항일   배서여부
   IF ZTINS-ZFDTYN EQ 'X'.
      IT_ZTDDF1-ZFDDFDA = '{12'.            APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = 'A6'.             APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = '4AA'.            APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = '}12'.            APPEND IT_ZTDDF1.
   ENDIF.
* Port / AirLine 배서여부
   IF ZTINS-ZFPRYN EQ 'X'.
      IT_ZTDDF1-ZFDDFDA = '{12'.            APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = 'A7'.             APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = '4AA'.            APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = '}12'.            APPEND IT_ZTDDF1.
   ENDIF.
* 문서번호 배서여부
   IF ZTINS-ZFDOYN EQ 'X'.
      IT_ZTDDF1-ZFDDFDA = '{12'.            APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = 'A8'.             APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = '4AA'.            APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = '}12'.            APPEND IT_ZTDDF1.
   ENDIF.
* 운송수단 배서여부
   IF ZTINS-ZFTMYN EQ 'X'.
      IT_ZTDDF1-ZFDDFDA = '{12'.            APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = 'A9'.             APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = '4AA'.            APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = '}12'.            APPEND IT_ZTDDF1.
   ENDIF.
* HS 코드 배서여부
   IF ZTINS-ZFHSYN EQ 'X'.
      IT_ZTDDF1-ZFDDFDA = '{12'.            APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = 'B0'.             APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = '4AA'.            APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = '}12'.            APPEND IT_ZTDDF1.
   ENDIF.
* 상품명세 배서여부
   IF ZTINS-ZFGDYN EQ 'X'.
      IT_ZTDDF1-ZFDDFDA = '{12'.            APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = 'B1'.             APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = '4AA'.            APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = '}12'.            APPEND IT_ZTDDF1.
   ENDIF.
* Partial 배서여부
   IF ZTINS-ZFPAYN EQ 'X'.
      IT_ZTDDF1-ZFDDFDA = '{12'.            APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = 'B2'.             APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = '4AA'.            APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = '}12'.            APPEND IT_ZTDDF1.
   ENDIF.
* 질권기관 배서여부
   IF ZTINS-ZFEIYN EQ 'X'.
      IT_ZTDDF1-ZFDDFDA = '{12'.            APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = 'B3'.             APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = '4AA'.            APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = '}12'.            APPEND IT_ZTDDF1.
   ENDIF.
* 부보조건 배서여부
   IF ZTINS-ZFCDYN EQ 'X'.
      IT_ZTDDF1-ZFDDFDA = '{12'.            APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = 'B4'.             APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = '4AA'.            APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = '}12'.            APPEND IT_ZTDDF1.
   ENDIF.
* 기타유형 배서여부
   IF ZTINS-ZFETYN EQ 'X'.
      IT_ZTDDF1-ZFDDFDA = '{12'.            APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = 'B5'.             APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = '4AA'.            APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = '}12'.            APPEND IT_ZTDDF1.
   ENDIF.
* 추가사항 배서여부
   IF ZTINS-ZFADYN EQ 'X'.
      IT_ZTDDF1-ZFDDFDA = '{12'.            APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = 'B6'.             APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = '4AA'.            APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = '}12'.            APPEND IT_ZTDDF1.
   ENDIF.
*-----------------------------------------------------------------------
* 변경전 / 변경후의 DATA를 모두 전송하는 업무
   IF ZTINS-ZFIVYN EQ 'X' OR ZTINS-ZFPEYN EQ 'X' OR
      ZTINS-ZFPDYN EQ 'X' OR ZTINS-ZFPRYN EQ 'X' OR
      ZTINS-ZFDOYN EQ 'X' OR ZTINS-ZFTMYN EQ 'X' OR
      ZTINS-ZFHSYN EQ 'X' OR ZTINS-ZFGDYN EQ 'X' OR
      ZTINS-ZFPAYN EQ 'X' OR ZTINS-ZFDTYN EQ 'X'.
*-----------------------------------------------------------------------
* ------>  이전 값
      IT_ZTDDF1-ZFDDFDA = '{13'.               APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = '11'.                APPEND IT_ZTDDF1.
* 송장금액 배서여부
      IF ZTINS-ZFIVYN EQ 'X'.
         IT_ZTDDF1-ZFDDFDA = '{20'.            APPEND IT_ZTDDF1.
         IT_ZTDDF1-ZFDDFDA = '77'.             APPEND IT_ZTDDF1.
         WRITE   ZTINS_OLD-ZFIVAMT         TO   W_TEXT_AMOUNT
                                 CURRENCY       ZTINS_OLD-WAERS.
         PERFORM    P2000_WRITE_NO_MASK     CHANGING  W_TEXT_AMOUNT.
         IT_ZTDDF1-ZFDDFDA = W_TEXT_AMOUNT.        APPEND IT_ZTDDF1.
         IT_ZTDDF1-ZFDDFDA = ZTINS_OLD-WAERS.          APPEND IT_ZTDDF1.
         IT_ZTDDF1-ZFDDFDA = '}20'.            APPEND IT_ZTDDF1.
      ENDIF.
* 희망이익율 배서여부
      IF ZTINS-ZFPEYN EQ 'X'.
         WRITE  ZTINS_OLD-ZFPEIV   TO    W_TEXT_AMOUNT.
         PERFORM    P2000_WRITE_NO_MASK     CHANGING  W_TEXT_AMOUNT.
         IT_ZTDDF1-ZFDDFDA = '{21'.            APPEND IT_ZTDDF1.
         IT_ZTDDF1-ZFDDFDA = '7'.              APPEND IT_ZTDDF1.
         IT_ZTDDF1-ZFDDFDA = W_TEXT_AMOUNT.    APPEND IT_ZTDDF1.
         IT_ZTDDF1-ZFDDFDA = '}21'.            APPEND IT_ZTDDF1.
      ENDIF.
* 관세율 배서여부
      IF ZTINS-ZFPDYN EQ 'X'.
         WRITE  ZTINS_OLD-ZFPEDU   TO    W_TEXT_AMOUNT.
         PERFORM    P2000_WRITE_NO_MASK     CHANGING  W_TEXT_AMOUNT.
         IT_ZTDDF1-ZFDDFDA = '{21'.            APPEND IT_ZTDDF1.
         IT_ZTDDF1-ZFDDFDA = '4AD'.            APPEND IT_ZTDDF1.
         IT_ZTDDF1-ZFDDFDA = W_TEXT_AMOUNT.    APPEND IT_ZTDDF1.
         IT_ZTDDF1-ZFDDFDA = '}21'.            APPEND IT_ZTDDF1.
      ENDIF.
* 출항일   배서여부
      IF ZTINS-ZFDTYN EQ 'X'.
         IT_ZTDDF1-ZFDDFDA = '{22'.                 APPEND IT_ZTDDF1.
         IT_ZTDDF1-ZFDDFDA = '136'.                 APPEND IT_ZTDDF1.
         IT_ZTDDF1-ZFDDFDA = ZTINSSG3_OLD-ZFDPDT+2. APPEND IT_ZTDDF1.
         IT_ZTDDF1-ZFDDFDA = '101'.                 APPEND IT_ZTDDF1.
         IT_ZTDDF1-ZFDDFDA = '}22'.                 APPEND IT_ZTDDF1.
      ENDIF.
* Port / AirLine 배서여부
      IF ZTINS-ZFPRYN EQ 'X'.
* 선적국가
         IF ZTINSSG3-ZFSHCU    NE ZTINSSG3_OLD-ZFSHCU OR
            ZTINSSG3-ZFSHCUNM  NE ZTINSSG3_OLD-ZFSHCUNM.
            IT_ZTDDF1-ZFDDFDA = '{23'.                 APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = '9'.                   APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = ZTINSSG3_OLD-ZFSHCU.   APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = ZTINSSG3_OLD-ZFSHCUNM. APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = '}23'.                 APPEND IT_ZTDDF1.
         ENDIF.
* 도착국가
         IF ZTINSSG3-ZFARCU    NE ZTINSSG3_OLD-ZFARCU OR
            ZTINSSG3-ZFARCUNM  NE ZTINSSG3_OLD-ZFARCUNM.
            IT_ZTDDF1-ZFDDFDA = '{23'.                 APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = '12'.                  APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = ZTINSSG3_OLD-ZFARCU.   APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = ZTINSSG3_OLD-ZFARCUNM. APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = '}23'.                 APPEND IT_ZTDDF1.
         ENDIF.
* 최종도착국가
         IF ZTINSSG3-ZFLACU    NE ZTINSSG3_OLD-ZFLACU OR
            ZTINSSG3-ZFLACUNM  NE ZTINSSG3_OLD-ZFLACUNM.
            IT_ZTDDF1-ZFDDFDA = '{23'.                 APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = '20'.                  APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = ZTINSSG3_OLD-ZFLACU.   APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = ZTINSSG3_OLD-ZFLACUNM. APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = '}23'.                 APPEND IT_ZTDDF1.
         ENDIF.
* 환적지  국가
         IF ZTINSSG3-ZFTRCU    NE ZTINSSG3_OLD-ZFTRCU OR
            ZTINSSG3-ZFTRCUNM  NE ZTINSSG3_OLD-ZFTRCUNM.
            IT_ZTDDF1-ZFDDFDA = '{23'.                 APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = '13'.                  APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = ZTINSSG3_OLD-ZFTRCU.   APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = ZTINSSG3_OLD-ZFTRCUNM. APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = '}23'.                 APPEND IT_ZTDDF1.
         ENDIF.
      ENDIF.
* 문서번호 배서여부
*     IF ZTINS-ZFDOYN EQ 'X'.
* 서류번호 1 ~ 3 ( 최대 3회 반복 )
*        IF ZTINS-ZFREDOC1 NE ZTINS_OLD-ZFREDOC1 OR
*           ZTINS-ZFREDON1 NE ZTINS_OLD-ZFREDON1.
         IF NOT ZTINS-ZFREDOC1 IS INITIAL.
            IT_ZTDDF1-ZFDDFDA = '{24'.                APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = ZTINS_OLD-ZFREDOC1.   APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = ZTINS_OLD-ZFREDON1.   APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = '}24'.                APPEND IT_ZTDDF1.
         ENDIF.
*        IF ZTINS-ZFREDOC2 NE ZTINS_OLD-ZFREDOC2 OR
*           ZTINS-ZFREDON2 NE ZTINS_OLD-ZFREDON2.
         IF NOT ZTINS-ZFREDOC2 IS INITIAL.
            IT_ZTDDF1-ZFDDFDA = '{24'.                APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = ZTINS_OLD-ZFREDOC2.   APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = ZTINS_OLD-ZFREDON2.   APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = '}24'.                APPEND IT_ZTDDF1.
         ENDIF.
*        IF ZTINS-ZFREDOC3 NE ZTINS_OLD-ZFREDOC3 OR
*           ZTINS-ZFREDON3 NE ZTINS_OLD-ZFREDON3.
         IF NOT ZTINS-ZFREDOC3 IS INITIAL.
            IT_ZTDDF1-ZFDDFDA = '{24'.                APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = ZTINS_OLD-ZFREDOC3.   APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = ZTINS_OLD-ZFREDON3.   APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = '}24'.                APPEND IT_ZTDDF1.
         ENDIF.
*     ENDIF.
* 운송수단 배서여부
      IF ZTINS-ZFTMYN EQ 'X'.
         IF ZTINSSG3-ZFCARNU NE ZTINSSG3_OLD-ZFCARNU OR
            ZTINSSG3-ZFCARNM NE ZTINSSG3_OLD-ZFCARNM.
            IT_ZTDDF1-ZFDDFDA = '{25'.                APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = '20'.                 APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = ZTINSSG3_OLD-ZFCARNU. APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = ZTINSSG3_OLD-ZFCARNM. APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = '}25'.                APPEND IT_ZTDDF1.
         ENDIF.
      ENDIF.
* HS 코드 배서여부
      IF ZTINS-ZFHSYN EQ 'X'.
         IF ZTINS-ZFRSTAW NE ZTINS_OLD-ZFRSTAW.
            IT_ZTDDF1-ZFDDFDA = '{26'.                APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = '1'.                  APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = ZTINS_OLD-ZFRSTAW.    APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = '}26'.                APPEND IT_ZTDDF1.
         ENDIF.
      ENDIF.
* 상품명세 배서여부
      IF ZTINS-ZFGDYN EQ 'X'.
         IF IT_ZSINSSG2[] NE IT_ZSINSSG2_OLD[].
            LOOP AT IT_ZSINSSG2_OLD.
               IT_ZTDDF1-ZFDDFDA = '{27'.          APPEND IT_ZTDDF1.
               IT_ZTDDF1-ZFDDFDA = 'AAA'.          APPEND IT_ZTDDF1.
               IT_ZTDDF1-ZFDDFDA = IT_ZSINSSG2_OLD-ZFDSOG1.
               APPEND IT_ZTDDF1.
               IT_ZTDDF1-ZFDDFDA = IT_ZSINSSG2_OLD-ZFDSOG2.
               APPEND IT_ZTDDF1.
               IT_ZTDDF1-ZFDDFDA = IT_ZSINSSG2_OLD-ZFDSOG3.
               APPEND IT_ZTDDF1.
               IT_ZTDDF1-ZFDDFDA = IT_ZSINSSG2_OLD-ZFDSOG4.
               APPEND IT_ZTDDF1.
               IT_ZTDDF1-ZFDDFDA = IT_ZSINSSG2_OLD-ZFDSOG5.
               APPEND IT_ZTDDF1.
               IT_ZTDDF1-ZFDDFDA = '}27'.          APPEND IT_ZTDDF1.
* 상품수량 및 단위
               IF NOT ( IT_ZSINSSG2_OLD-ZFPKCN  IS INITIAL AND
                  IT_ZSINSSG2_OLD-ZFPKCNM IS INITIAL ).
                  IT_ZTDDF1-ZFDDFDA = '{30'.        APPEND IT_ZTDDF1.
                  WRITE   IT_ZSINSSG2_OLD-ZFPKCN    TO   W_TEXT_AMOUNT
                                          UNIT IT_ZSINSSG2-ZFPKCNM.
                  PERFORM    P2000_WRITE_NO_MASK
                                          CHANGING  W_TEXT_AMOUNT.
                  IT_ZTDDF1-ZFDDFDA = W_TEXT_AMOUNT.  APPEND IT_ZTDDF1.
*>>> UNIT ISO CODE <=== 내부코드로 변환( PKG    단위 )
                  PERFORM    SET_UNIT_CONV_TO_EXTERNAL
                                    CHANGING IT_ZSINSSG2_OLD-ZFPKCNM.
                  IT_ZTDDF1-ZFDDFDA = IT_ZSINSSG2_OLD-ZFPKCNM.
                  APPEND IT_ZTDDF1.
                  IT_ZTDDF1-ZFDDFDA = '}30'.         APPEND IT_ZTDDF1.
               ENDIF.
            ENDLOOP.
         ENDIF.
      ENDIF.
* Partial 배서여부  ===> DB 미반영
      IF ZTINS-ZFPAYN EQ 'X'.

      ENDIF.
      IT_ZTDDF1-ZFDDFDA = '}13'.               APPEND IT_ZTDDF1.
************************************************************************
************************************************************************
************************************************************************
*-----------------------------------------------------------------------
* ------>  이후 값
      IT_ZTDDF1-ZFDDFDA = '{13'.               APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = '36'.                APPEND IT_ZTDDF1.
* 송장금액 배서여부
      IF ZTINS-ZFIVYN EQ 'X'.
         IT_ZTDDF1-ZFDDFDA = '{20'.            APPEND IT_ZTDDF1.
         IT_ZTDDF1-ZFDDFDA = '77'.             APPEND IT_ZTDDF1.
         WRITE   ZTINS-ZFIVAMT          TO     W_TEXT_AMOUNT
                              CURRENCY         ZTINS-WAERS.
         PERFORM    P2000_WRITE_NO_MASK        CHANGING  W_TEXT_AMOUNT.
         IT_ZTDDF1-ZFDDFDA = W_TEXT_AMOUNT.    APPEND IT_ZTDDF1.
         IT_ZTDDF1-ZFDDFDA = ZTINS-WAERS.      APPEND IT_ZTDDF1.
         IT_ZTDDF1-ZFDDFDA = '}20'.            APPEND IT_ZTDDF1.
      ENDIF.
* 희망이익율 배서여부
      IF ZTINS-ZFPEYN EQ 'X'.
         WRITE  ZTINS-ZFPEIV                TO        W_TEXT_AMOUNT.
         PERFORM    P2000_WRITE_NO_MASK     CHANGING  W_TEXT_AMOUNT.
         IT_ZTDDF1-ZFDDFDA = '{21'.            APPEND IT_ZTDDF1.
         IT_ZTDDF1-ZFDDFDA = '7'.              APPEND IT_ZTDDF1.
         IT_ZTDDF1-ZFDDFDA = W_TEXT_AMOUNT.    APPEND IT_ZTDDF1.
         IT_ZTDDF1-ZFDDFDA = '}21'.            APPEND IT_ZTDDF1.
      ENDIF.
* 관세율 배서여부
      IF ZTINS-ZFPDYN EQ 'X'.
         WRITE  ZTINS-ZFPEDU   TO    W_TEXT_AMOUNT.
         PERFORM    P2000_WRITE_NO_MASK     CHANGING  W_TEXT_AMOUNT.
         IT_ZTDDF1-ZFDDFDA = '{21'.            APPEND IT_ZTDDF1.
         IT_ZTDDF1-ZFDDFDA = '4AD'.            APPEND IT_ZTDDF1.
         IT_ZTDDF1-ZFDDFDA = W_TEXT_AMOUNT.    APPEND IT_ZTDDF1.
         IT_ZTDDF1-ZFDDFDA = '}21'.            APPEND IT_ZTDDF1.
      ENDIF.
* 출항일   배서여부
      IF ZTINS-ZFDTYN EQ 'X'.
         IT_ZTDDF1-ZFDDFDA = '{22'.                APPEND IT_ZTDDF1.
         IT_ZTDDF1-ZFDDFDA = '136'.                APPEND IT_ZTDDF1.
         IT_ZTDDF1-ZFDDFDA = ZTINSSG3-ZFDPDT+2.    APPEND IT_ZTDDF1.
         IT_ZTDDF1-ZFDDFDA = '101'.                APPEND IT_ZTDDF1.
         IT_ZTDDF1-ZFDDFDA = '}22'.                APPEND IT_ZTDDF1.
      ENDIF.
* Port / AirLine 배서여부
      IF ZTINS-ZFPRYN EQ 'X'.
* 선적국가
         IF ZTINSSG3-ZFSHCU    NE ZTINSSG3_OLD-ZFSHCU OR
            ZTINSSG3-ZFSHCUNM  NE ZTINSSG3_OLD-ZFSHCUNM.
            IT_ZTDDF1-ZFDDFDA = '{23'.                 APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = '9'.                   APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = ZTINSSG3-ZFSHCU.       APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = ZTINSSG3-ZFSHCUNM.     APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = '}23'.                 APPEND IT_ZTDDF1.
         ENDIF.
* 도착국가
         IF ZTINSSG3-ZFARCU    NE ZTINSSG3_OLD-ZFARCU OR
            ZTINSSG3-ZFARCUNM  NE ZTINSSG3_OLD-ZFARCUNM.
            IT_ZTDDF1-ZFDDFDA = '{23'.                 APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = '12'.                  APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = ZTINSSG3-ZFARCU.       APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = ZTINSSG3-ZFARCUNM.     APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = '}23'.                 APPEND IT_ZTDDF1.
         ENDIF.
* 최종도착국가
         IF ZTINSSG3-ZFLACU    NE ZTINSSG3_OLD-ZFLACU OR
            ZTINSSG3-ZFLACUNM  NE ZTINSSG3_OLD-ZFLACUNM.
            IT_ZTDDF1-ZFDDFDA = '{23'.                 APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = '20'.                  APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = ZTINSSG3-ZFLACU.       APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = ZTINSSG3-ZFLACUNM.     APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = '}23'.                 APPEND IT_ZTDDF1.
         ENDIF.
* 환적지  국가
         IF ZTINSSG3-ZFTRCU    NE ZTINSSG3_OLD-ZFTRCU OR
            ZTINSSG3-ZFTRCUNM  NE ZTINSSG3_OLD-ZFTRCUNM.
            IT_ZTDDF1-ZFDDFDA = '{23'.                 APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = '13'.                  APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = ZTINSSG3-ZFTRCU.       APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = ZTINSSG3-ZFTRCUNM.     APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = '}23'.                 APPEND IT_ZTDDF1.
         ENDIF.
      ENDIF.
* 문서번호 배서여부
      IF ZTINS-ZFDOYN EQ 'X'.
* 서류번호 1 ~ 3 ( 최대 3회 반복 )
*         IF ZTINS-ZFREDOC1 NE ZTINS_OLD-ZFREDOC1 OR
*            ZTINS-ZFREDON1 NE ZTINS_OLD-ZFREDON1.
         IF NOT ZTINS-ZFREDOC1 IS INITIAL.
            IT_ZTDDF1-ZFDDFDA = '{24'.                APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = ZTINS-ZFREDOC1.       APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = ZTINS-ZFREDON1.       APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = '}24'.                APPEND IT_ZTDDF1.
         ENDIF.
*         IF ZTINS-ZFREDOC2 NE ZTINS_OLD-ZFREDOC2 OR
*            ZTINS-ZFREDON2 NE ZTINS_OLD-ZFREDON2.
         IF NOT ZTINS-ZFREDOC2 IS INITIAL.
            IT_ZTDDF1-ZFDDFDA = '{24'.                APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = ZTINS-ZFREDOC2.       APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = ZTINS-ZFREDON2.       APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = '}24'.                APPEND IT_ZTDDF1.
         ENDIF.
*         IF ZTINS-ZFREDOC3 NE ZTINS_OLD-ZFREDOC3 OR
*            ZTINS-ZFREDON3 NE ZTINS_OLD-ZFREDON3.
         IF NOT ZTINS-ZFREDOC3 IS INITIAL.
            IT_ZTDDF1-ZFDDFDA = '{24'.                APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = ZTINS-ZFREDOC3.       APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = ZTINS-ZFREDON3.       APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = '}24'.                APPEND IT_ZTDDF1.
         ENDIF.
      ENDIF.
* 운송수단 배서여부
      IF ZTINS-ZFTMYN EQ 'X'.
         IF ZTINSSG3-ZFCARNU NE ZTINSSG3_OLD-ZFCARNU OR
            ZTINSSG3-ZFCARNM NE ZTINSSG3_OLD-ZFCARNM.
            IT_ZTDDF1-ZFDDFDA = '{25'.                APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = '20'.                 APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = ZTINSSG3-ZFCARNU.     APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = ZTINSSG3-ZFCARNM.     APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = '}25'.                APPEND IT_ZTDDF1.
         ENDIF.
      ENDIF.
* HS 코드 배서여부
      IF ZTINS-ZFHSYN EQ 'X'.
         IF ZTINS-ZFRSTAW NE ZTINS_OLD-ZFRSTAW.
            IT_ZTDDF1-ZFDDFDA = '{26'.                APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = '1'.                  APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = ZTINS-ZFRSTAW.        APPEND IT_ZTDDF1.
            IT_ZTDDF1-ZFDDFDA = '}26'.                APPEND IT_ZTDDF1.
         ENDIF.
      ENDIF.
* 상품명세 배서여부
      IF ZTINS-ZFGDYN EQ 'X'.
         IF IT_ZSINSSG2[] NE IT_ZSINSSG2_OLD[].
            LOOP AT IT_ZSINSSG2.
               IT_ZTDDF1-ZFDDFDA = '{27'.          APPEND IT_ZTDDF1.
               IT_ZTDDF1-ZFDDFDA = 'AAA'.          APPEND IT_ZTDDF1.
               IT_ZTDDF1-ZFDDFDA = IT_ZSINSSG2-ZFDSOG1.
               APPEND IT_ZTDDF1.
               IT_ZTDDF1-ZFDDFDA = IT_ZSINSSG2-ZFDSOG2.
               APPEND IT_ZTDDF1.
               IT_ZTDDF1-ZFDDFDA = IT_ZSINSSG2-ZFDSOG3.
               APPEND IT_ZTDDF1.
               IT_ZTDDF1-ZFDDFDA = IT_ZSINSSG2-ZFDSOG4.
               APPEND IT_ZTDDF1.
               IT_ZTDDF1-ZFDDFDA = IT_ZSINSSG2-ZFDSOG5.
               APPEND IT_ZTDDF1.
               IT_ZTDDF1-ZFDDFDA = '}27'.          APPEND IT_ZTDDF1.
* 상품수량 및 단위
               IF NOT ( IT_ZSINSSG2-ZFPKCN  IS INITIAL AND
                  IT_ZSINSSG2-ZFPKCNM IS INITIAL ).
                  IT_ZTDDF1-ZFDDFDA = '{30'.        APPEND IT_ZTDDF1.
                  WRITE   IT_ZSINSSG2-ZFPKCN  TO   W_TEXT_AMOUNT
                                              UNIT IT_ZSINSSG2-ZFPKCNM.
                  PERFORM    P2000_WRITE_NO_MASK
                                          CHANGING  W_TEXT_AMOUNT.
                  IT_ZTDDF1-ZFDDFDA = W_TEXT_AMOUNT.  APPEND IT_ZTDDF1.
*>>> UNIT ISO CODE <=== 내부코드로 변환( PKG    단위 )
                  PERFORM    SET_UNIT_CONV_TO_EXTERNAL
                                    CHANGING IT_ZSINSSG2_OLD-ZFPKCNM.
                  IT_ZTDDF1-ZFDDFDA = IT_ZSINSSG2_OLD-ZFPKCNM.
                  APPEND IT_ZTDDF1.
                  IT_ZTDDF1-ZFDDFDA = '}30'.         APPEND IT_ZTDDF1.
               ENDIF.
            ENDLOOP.
         ENDIF.
      ENDIF.
* Partial 배서여부  ===> DB 미반영
      IT_ZTDDF1-ZFDDFDA = '}13'.               APPEND IT_ZTDDF1.
   ENDIF.
* 질권기관
   IF NOT ( ZTINS-ZFOPBNCD IS INITIAL AND ZTINS-ZFEINM IS INITIAL AND
            ZTINS-ZFEIBR   IS INITIAL ).
      IT_ZTDDF1-ZFDDFDA = '{14'.            APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = 'INS'.            APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = ZTINS-ZFEINM.     APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = ZTINS-ZFEIBR.     APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = ''.               APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = ''.               APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = ''.               APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = '}14'.            APPEND IT_ZTDDF1.
   ENDIF.
* 기본 및 부가조건 코드
   SORT IT_ZSINSAGR BY ZFBCNYN DESCENDING.
   LOOP AT IT_ZSINSAGR.
      IT_ZTDDF1-ZFDDFDA = '{14'.               APPEND IT_ZTDDF1.
* 기본 조건인지 부가 조건인지 구분
      IF IT_ZSINSAGR-ZFBCNYN EQ 'X'.
         IT_ZTDDF1-ZFDDFDA = '4AA'.            APPEND IT_ZTDDF1.
      ELSE.
         IT_ZTDDF1-ZFDDFDA = '4AB'.            APPEND IT_ZTDDF1.
      ENDIF.
      IT_ZTDDF1-ZFDDFDA = IT_ZSINSAGR-ZFCNCD.   APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = '4AA'.                APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = IT_ZSINSAGR-ZFCNCDNM. APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = ''.               APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = ''.               APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = '}14'.               APPEND IT_ZTDDF1.
   ENDLOOP.

*>>> 피보험자 상호
   IF NOT ZTINS-ZFINSU1 IS INITIAL OR NOT ZTINS-ZFINSU2 IS INITIAL.
      IT_ZTDDF1-ZFDDFDA = '{15'.            APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = 'BM'..            APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = ZTINS-ZFINSU1.    APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = ZTINS-ZFINSU2.    APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = '}15'.            APPEND IT_ZTDDF1.
   ENDIF.
* 보험계약자
   IF NOT ZTINS-ZFELENM IS INITIAL OR NOT ZTINS-ZFREPRE IS INITIAL.
      IT_ZTDDF1-ZFDDFDA = '{15'.            APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = '4AA'.            APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = ZTINS-ZFELENM.    APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = ZTINS-ZFREPRE.    APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = '}15'.            APPEND IT_ZTDDF1.
   ENDIF.
* 손보사
   IF NOT ZTINS-ZFCONN1 IS INITIAL OR NOT ZTINS-ZFCONN2 IS INITIAL.
      IT_ZTDDF1-ZFDDFDA = '{15'.            APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = '4AD'.            APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = ZTINS-ZFCONN1.    APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = ZTINS-ZFCONN2.    APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = '}15'.            APPEND IT_ZTDDF1.
   ENDIF.
* 손보사 영업담당자 코드
   IF NOT ( ZTINS-ZFCONID IS INITIAL AND ZTINS-ZFCONNM IS INITIAL ).
      IT_ZTDDF1-ZFDDFDA = '{16'.            APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = 'IC'.             APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = ZTINS-ZFCONID.    APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = ZTINS-ZFCONNM.    APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = '}16'.            APPEND IT_ZTDDF1.
   ENDIF.

*-----------------------------------------------------------------------
* 사본 / 원본 발급회수
*-----------------------------------------------------------------------
   IF ZTINS-ZFNUOD NE ZTINS_OLD-ZFNUOD.
      IT_ZTDDF1-ZFDDFDA = '{17'.            APPEND IT_ZTDDF1.
      IT_ZTDDF1-ZFDDFDA = '4AB'.            APPEND IT_ZTDDF1.
* 원본
      IF ZTINS-ZFNUOD GE 10.
         IT_ZTDDF1-ZFDDFDA = ZTINS-ZFNUOD.       APPEND IT_ZTDDF1.
      ELSE.
         IT_ZTDDF1-ZFDDFDA = ZTINS-ZFNUOD+1(1).  APPEND IT_ZTDDF1.
      ENDIF.
      IT_ZTDDF1-ZFDDFDA = '}17'.            APPEND IT_ZTDDF1.
   ENDIF.

* FLAT DATA CREATE
   CALL FUNCTION 'ZIM_EDI_FLAT_ITEM_INSERT'
        EXPORTING
              W_ZFDHENO         =        W_ZFDHENO
        TABLES
              IT_ZTDDF1         =        IT_ZTDDF1
        EXCEPTIONS
              DB_ERROR         =         4.

   IF SY-SUBRC NE 0.
      MESSAGE E118 WITH W_ZFDHENO.
   ENDIF.

ENDFUNCTION.
