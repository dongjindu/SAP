FUNCTION ZIM_LG_IMPREQ_EDI_DOC.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(W_ZFBLNO) LIKE  ZTIDS-ZFBLNO
*"     VALUE(W_ZFCLSEQ) LIKE  ZTIDS-ZFCLSEQ
*"     VALUE(W_ZFDHENO) LIKE  ZTDHF1-ZFDHENO
*"     VALUE(W_BAHNS) LIKE  LFA1-BAHNS
*"  EXPORTING
*"     REFERENCE(W_EDI_RECORD)
*"  EXCEPTIONS
*"      CREATE_ERROR
*"----------------------------------------------------------------------
DATA : L_TEXT         LIKE     DD07T-DDTEXT,
       L_TEXT280(280) TYPE     C,
       L_TEXT350(350) TYPE     C,
       L_BOOLEAN      TYPE     C,
       L_TYPE(002)    TYPE     C,
       L_TYPE_TEMP    LIKE     L_TYPE,
       L_CODE(003)    TYPE     C,
       L_ZFDSOG1      LIKE     IT_ZSMLCSG7G-ZFDSOG1,
       L_ZFDSOG2      LIKE     IT_ZSMLCSG7G-ZFDSOG1,
       L_ZFDSOG3      LIKE     IT_ZSMLCSG7G-ZFDSOG1,
       L_ZFDSOG4      LIKE     IT_ZSMLCSG7G-ZFDSOG1,
       L_ZFDSOG5      LIKE     IT_ZSMLCSG7G-ZFDSOG1,
       L_ZFOPNDT      LIKE     ZTREQST-ZFOPNDT,
       W_LIN_CNT      TYPE     I,
       W_DET_CNT      TYPE     I,
       W_HS_CNT       TYPE     I.

*> CUSTOM CLEARANCE DATA GET
   CALL FUNCTION 'ZIM_GET_IDR_DOCUMENT'
        EXPORTING
              ZFBLNO             =       W_ZFBLNO
              ZFCLSEQ            =       W_ZFCLSEQ
        IMPORTING
              W_ZTIDR            =       ZTIDR
        TABLES
              IT_ZSIDRHS         =       IT_ZSIDRHS
              IT_ZSIDRHS_ORG     =       IT_ZSIDRHS_ORG
              IT_ZSIDRHSD        =       IT_ZSIDRHSD
              IT_ZSIDRHSD_ORG    =       IT_ZSIDRHSD_ORG
              IT_ZSIDRHSL        =       IT_ZSIDRHSL
              IT_ZSIDRHSL_ORG    =       IT_ZSIDRHSL_ORG
        EXCEPTIONS
              NOT_FOUND     =       4
              NOT_INPUT     =       8.

   CASE SY-SUBRC.
      WHEN 4.
         MESSAGE E018 WITH W_ZFREQNO RAISING  CREATE_ERROR.
      WHEN 8.
         MESSAGE E019 RAISING  CREATE_ERROR.
   ENDCASE.

*>>관세사 정보 GET!
   CLEAR ZTIMIMG10.
   SELECT SINGLE * FROM ZTIMIMG10 WHERE ZFCUT EQ ZTIDR-ZFCUT.

*>> BL 자료 GET!
   CLEAR ZTBL.
   SELECT SINGLE * FROM ZTBL      WHERE ZFBLNO EQ W_ZFBLNO.

*-----------------------------------------------------------------------
* FLAT-FILE RECORD CREATE
*-----------------------------------------------------------------------
   L_TYPE  =  '00'.

*>> FLAT FILE HEADER MAKE.
   PERFORM  P3000_HEADER_MAKE    USING    'IMPREQ'
                                          ZTIDR-BUKRS
                                          ZTIMIMG10-ZFVEN
                                          W_ZFDHENO
                                 CHANGING W_EDI_RECORD.

*>> FLAT FILE BEG MAKE.(전자문서 시작, 신고의뢰번호)
   L_TYPE = '01'.
   PERFORM  P3000_BGM_MAKE       USING    '929'
                                          W_ZFDHENO
*                                         ZTIDR-ZFIDRNO
                                          ' '
                                          'AB'
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.

*>> FLAT FILE CST MAKE(세관 물품 상태)
   L_TYPE = '02'.
   PERFORM  P3000_CST_MAKE       USING    ZTIDR-ZFPONC
                                          '5BB'
                                          'KCS'
                                          ZTIDR-ZFITKD
                                          '5BC'
                                          'KCS'
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.
*>> FLAT FILE DTM MAKE( 신고의뢰일자).
   L_TYPE = '03'.
   PERFORM  P3000_DTM_MAKE       USING    '286'
                                          SY-DATUM
                                          '102'
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.

*>> FLAT FILE DTM MAKE( 신고희망일자).
   L_TYPE = '03'.
   PERFORM  P3000_DTM_MAKE       USING    '318'
                                          ZTIDR-ZFIDWDT
                                          '102'
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.

*>> FLAT FILE DTM MAKE( 입항일자 ).
   IF NOT ZTIDR-ZFENDT IS INITIAL.
      L_TYPE = '03'.
      PERFORM  P3000_DTM_MAKE    USING    '178'
                                          ZTIDR-ZFENDT
                                          '102'
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.
   ENDIF.

*>> FLAT FILE DTM MAKE( 반입일자 )
   IF NOT ZTIDR-ZFINDT IS INITIAL.
      L_TYPE = '03'.
      PERFORM  P3000_DTM_MAKE    USING    '146'
                                          ZTIDR-ZFINDT
                                          '102'
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.
   ENDIF.

*>> FLAT FILE GIS MAKE ( 수입자 구분 )
   IF NOT ZTIDR-ZFIMCD IS INITIAL.
      L_TYPE = '04'.
      PERFORM  P3000_GIS_MAKE    USING    ZTIDR-ZFIMCD
                                          '5AR'
                                          'KCS'
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.
   ENDIF.

*>> FLAT FILE GIS MAKE ( 신고 구분 )
   IF NOT ZTIDR-ZFIDRCD IS INITIAL.
      L_TYPE = '04'.
      PERFORM  P3000_GIS_MAKE    USING    ZTIDR-ZFIDRCD
                                          '105'
                                          'KCS'
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.
   ENDIF.

*>> FLAT FILE GIS MAKE(결제방법).
   L_TYPE = '04'.
   PERFORM  P3000_GIS_MAKE       USING    ZTIDR-ZFAMCD
                                          '153'
                                          'KCS'
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.

*>> FLAT FILE GIS MAKE( 징수형태 ).
   IF NOT ZTIDR-ZFCOCD IS INITIAL.
      L_TYPE = '04'.
      PERFORM  P3000_GIS_MAKE    USING    ZTIDR-ZFCOCD
                                          '132'
                                          'KCS'
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.
   ENDIF.

*>> FLAT FILE GIS MAKE( 통관계획 부호).
   IF NOT ZTIDR-ZFCUPR IS INITIAL.
      L_TYPE = '04'.
      PERFORM P3000_GIS_MAKE     USING    ZTIDR-ZFCUPR
                                          '5AO'
                                          'KCS'
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.
   ENDIF.

*>> FLAT FILE LOC MAKE( 신고지 세관 )
   IF NOT ( ZTIDR-ZFINRC  IS INITIAL OR
            ZTIDR-ZFINRCD IS INITIAL ).
      L_TYPE = '05'.
      PERFORM  P3000_LOC_IDR    USING    '41'
                                         ZTIDR-ZFINRC
                                         '113'
                                         'KCS'
                                         ZTIDR-ZFINRCD
                                         '5AB'
                                         'KCS'
                                         SPACE
                                         SPACE
                                         SPACE
                                CHANGING L_TYPE
                                         W_EDI_RECORD.
   ENDIF.

*>> FLAT FILE LOC MAKE( 도착항 )
   L_TYPE = '05'.
   PERFORM  P3000_LOC_2_MAKE   USING    '60'
                                        ZTIDR-ZFAPRTC
                                        SPACE
                                        SPACE
                               CHANGING L_TYPE
                                        W_EDI_RECORD.

*>> FLAT FILE LOC MAKE( 적출국 )
   L_TYPE = '05'.
   PERFORM  P3000_LOC_2_MAKE   USING    '35'
                                        ZTIDR-ZFSCON
                                        SPACE
                                        SPACE
                               CHANGING L_TYPE
                                        W_EDI_RECORD.

*>> FLAT FILE LOC MAKE( 검사장소 ).
   IF NOT ZTIDR-ZFISPL IS INITIAL.
      L_TYPE = '05'.
      PERFORM  P3000_LOC_2_MAKE   USING    '43'
                                           ZTIDR-ZFISPL
                                           '115'
                                           'KCS'
                                  CHANGING L_TYPE
                                           W_EDI_RECORD.
   ENDIF.

*>> FLAT FILE EQD MAKE( 운송용기 ).
   L_TYPE = '06'.
   PERFORM  P3000_EQD_MAKE        USING    ZTIDR-ZFTRCN
                                  CHANGING L_TYPE
                                           W_EDI_RECORD.

*>> FLAT FILE RFF MAKE( House BL No ).
   L_TYPE = '07'.
   PERFORM  P3000_RFF_MAKE     USING    'BH'
                                        ZTBL-ZFHBLNO
                               CHANGING L_TYPE
                                        W_EDI_RECORD.

*>> FLAT FILE RFF MAKE( 화물관리번호 ).
   IF NOT ZTIDR-ZFGOMNO IS INITIAL.
      L_TYPE = '07'.
      PERFORM  P3000_RFF_MAKE     USING    'XC'
                                           ZTIDR-ZFGOMNO
                                  CHANGING L_TYPE
                                           W_EDI_RECORD.
   ENDIF.

*>> FLAT FILE RFF MAKE( MASTER BL No ).
   IF NOT ZTBL-ZFMBLNO IS INITIAL.

      PERFORM  P3000_RFF_MAKE     USING    'BM'
                                           ZTBL-ZFMBLNO
                                  CHANGING L_TYPE
                                           W_EDI_RECORD.
   ENDIF.

*>> FLAT FILE RFF MAKE( 무역업체 참조번호 ).
   IF NOT ZTIDR-ZFIMCR IS INITIAL.
      L_TYPE = '07'.
      PERFORM  P3000_RFF_MAKE     USING    'ABQ'
                                           ZTIDR-ZFIMCR
                                  CHANGING L_TYPE
                                           W_EDI_RECORD.
   ENDIF.

*>> FLAT FILE TDT MAKE( 운송수단 ).
   L_TYPE = '08'.
   PERFORM  P3000_TDT_MAKE        USING    '20'
                                           ZTIDR-ZFTRMET
                                           ZTIDR-ZFCARNM
                                  CHANGING L_TYPE
                                           W_EDI_RECORD.

*>> FLAT FILE TPL MAKE( 선기국적 )
   L_TYPE = '09'.
   PERFORM  P3000_TPL_MAKE        USING    ZTIDR-ZFCAC
                                  CHANGING L_TYPE
                                           W_EDI_RECORD.


*>> FLAT FILE NAD MAKE( 신고의뢰자)
   L_TYPE = '10'.
   PERFORM  P3000_NAD_MAKE        USING    'MS'
                                           ZTIDR-ZFAPNM
                                           SPACE
                                           SPACE
                                           SPACE
                                           SPACE
                                  CHANGING L_TYPE
                                           W_EDI_RECORD.

*>> FLAT FILE NAD MAKE( 수입자 )
   L_TYPE = '10'.
   PERFORM  P3000_NAD_MAKE        USING    'IM'
                                           ZTIDR-ZFIAPNM
                                           SPACE
                                           SPACE
                                           SPACE
                                           SPACE
                                  CHANGING L_TYPE
                                           W_EDI_RECORD.

*>> FLAT FILE NAD MAKE( 납세의무자 )
   L_TYPE = '10'.
   PERFORM  P3000_NAD_IDR         USING    'PR'
                                           ZTIDR-ZFTDNO
                                           '5AN'
                                           'KCS'
                                           ZTIDR-ZFTDNM1
                                           ZTIDR-ZFTDNM2
                                           ZTIDR-ZFTDAD1
                                           ZTIDR-ZFTDAD2
                                  CHANGING L_TYPE
                                           W_EDI_RECORD.

*>> FLAT FILE RFF MAKE( 사업자등록번호)
   L_TYPE = '11'.
   PERFORM  P3000_RFF_MAKE        USING    'AHP'
                                           ZTIDR-ZFTDTC
                                  CHANGING L_TYPE
                                           W_EDI_RECORD.

*>> FLAT FILE NAD MAKE( 무역대리점 )
   IF NOT ( ZTIDR-ZFTRDNO IS INITIAL OR
            ZTIDR-ZFTRDNM IS INITIAL ).
      L_TYPE = '10'.
      PERFORM  P3000_NAD_IDR         USING    'SR'
                                              ZTIDR-ZFTRDNO
                                              '5AS'
                                              'KTA'
                                              ZTIDR-ZFTRDNM
                                              SPACE
                                              SPACE
                                              SPACE
                                     CHANGING L_TYPE
                                              W_EDI_RECORD.
   ENDIF.

*>> FLAT FILE NAD MAKE( 공급자 )
   L_TYPE = '10'.
   PERFORM  P3000_NAD_IDR         USING    'SU'
                                           ZTIDR-ZFSUPNO
                                           '160'
                                           'KCS'
                                           ZTIDR-ZFSUPNM
                                           SPACE
                                           ZTIDR-ZFSUPC
                                           SPACE
                                  CHANGING L_TYPE
                                           W_EDI_RECORD.

*>> FLAT FILE NAD MAKE( 특송업체부호 )
   IF NOT ZTIDR-ZFSTRCD IS INITIAL.
      L_TYPE = '10'.
      PERFORM  P3000_NAD_IDR         USING    'BA'
                                              ZTIDR-ZFSTRCD
                                              '101'
                                              'KCS'
                                              SPACE
                                              SPACE
                                              SPACE
                                              SPACE
                                     CHANGING L_TYPE
                                              W_EDI_RECORD.
   ENDIF.

*>> FLAT FILE TOD MAKE ( 인도조건 )
   L_TYPE = '12'.
   PERFORM  P3000_TOD_IDR           USING    ZTIDR-INCO1
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.

*>> FLAT FILE MOA MAKE ( 결제금액 )
   L_TYPE = '13'.
   PERFORM  P3000_MOA_MAKE          USING    '38'
                                             ZTIDR-ZFSTAMT
                                             ZTIDR-ZFSTAMC
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.

*>> FLAT FILE MOA MAKE ( 과세가격 ).
   IF NOT ZTIDR-ZFTBAK IS INITIAL.
      L_TYPE = '13'.
      PERFORM  P3000_MOA_MAKE       USING    '43'
                                             ZTIDR-ZFTBAK
                                            'KRW'
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.
   ENDIF.

*>> FLAT FILE MOA MAKE ( 과세가격(미화) ).
   IF NOT ZTIDR-ZFTBAU IS INITIAL.
      L_TYPE = '13'.
      PERFORM  P3000_MOA_MAKE       USING    '43'
                                             ZTIDR-ZFTBAU
                                             'USD'
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.
   ENDIF.

*>> FLAT FILE MOA MAKE ( 운임 ).
   IF NOT ZTIDR-ZFTRT IS INITIAL.
      L_TYPE = '13'.
      PERFORM  P3000_MOA_MAKE       USING    '144'
                                              ZTIDR-ZFTRT
                                             'KRW'
                                     CHANGING L_TYPE
                                              W_EDI_RECORD.
   ENDIF.

*>> FLAT FILE MOA MAKE ( 보험료 ).
   IF NOT ZTIDR-ZFINAMTS IS INITIAL.
      L_TYPE = '13'.
      PERFORM  P3000_MOA_MAKE       USING    '144'
                                              ZTIDR-ZFINAMTS
                                             'KRW'
                                     CHANGING L_TYPE
                                              W_EDI_RECORD.
   ENDIF.

*>> FLAT FILE MOA MAKE ( 가산금액 ).
   IF NOT ZTIDR-ZFADAMK IS INITIAL.
      L_TYPE = '13'.
      PERFORM  P3000_MOA_MAKE       USING    '160'
                                             ZTIDR-ZFADAMK
                                            'KRW'
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.
   ENDIF.

*>> FLAT FILE MOA MAKE ( 공제금액 ).
   IF NOT ZTIDR-ZFDUAMK IS INITIAL.
      L_TYPE = '13'.
      PERFORM  P3000_MOA_MAKE       USING    '46'
                                             ZTIDR-ZFDUAMK
                                             'KRW'
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.
   ENDIF.

*>> FLAT FILE UNS MAKE( 제어부분 )
   L_TYPE = '14'.
   PERFORM  P3000_UNS_MAKE          USING    'D'
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.

*----------------------< 란 사항 SEND >--------------------------------*
   LOOP  AT  IT_ZSIDRHS  WHERE  ZFBLNO  EQ  W_ZFBLNO
                         AND    ZFCLSEQ EQ  W_ZFCLSEQ.

*>> FLAT FILE LIN MAKE( 란번호 )
      L_TYPE = '15'.
      PERFORM  P3000_LIN_MAKE        USING    IT_ZSIDRHS-ZFCONO
                                              IT_ZSIDRHS-STAWN
                                              'HS'
                                     CHANGING L_TYPE
                                              W_EDI_RECORD.

*>> FLAT FILE FTX MAKE( 상품명세 )
      IF IT_ZSIDRHS-ZFTGDNM IS INITIAL.
         W_ZFTGDNM = 'T'.
      ELSE.
         W_ZFTGDNM = IT_ZSIDRHS-ZFTGDNM.
      ENDIF.
      IF IT_ZSIDRHS-ZFGCCD IS INITIAL.
         W_ZFGCCD  = 'B'.
      ELSE.
         W_ZFGCCD  = IT_ZSIDRHS-ZFGCCD.
      ENDIF.

      L_TYPE = '16'.
      PERFORM  P3000_FTX_MAKE    USING    'AAA'
                                          IT_ZSIDRHS-ZFGDNM
                                          W_ZFTGDNM
                                          W_ZFGCCD
                                          SPACE
                                          SPACE
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.

*>> FLAT FILE LOC MAKE( 원산지 )
      IF NOT IT_ZSIDRHS-ZFORIG IS INITIAL.
         L_TYPE = '17'.
         PERFORM  P3000_LOC_IDR    USING    '27'
                                            IT_ZSIDRHS-ZFORIG
                                            '162'
                                            SPACE
                                            SPACE
                                            SPACE
                                            SPACE
                                            SPACE
                                            SPACE
                                            SPACE
                                   CHANGING L_TYPE
                                            W_EDI_RECORD.
      ENDIF.
*>> FLAT FILE LOC MAKE( 사후확인기관 )
      IF NOT IT_ZSIDRHS-ZFMOR1 IS INITIAL.
         L_TYPE = '17'.
         PERFORM  P3000_LOC_IDR USING    '44'
                                         IT_ZSIDRHS-ZFMOR1
                                         '5FC'
                                         'KCS'
                                         IT_ZSIDRHS-ZFMOR2
                                         '5FC'
                                         'KCS'
                                         IT_ZSIDRHS-ZFMOR3
                                         '5FC'
                                         'KCS'
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.
      ENDIF.

*>> FLAT FILE MEA MAKE( 순중량 )
      L_TYPE = '18'.
      IF IT_ZSIDRHS-ZFWETM IS INITIAL.
         IT_ZSIDRHS-ZFWETM = 'TO'.
      ENDIF.
      PERFORM  P3000_MEA_MAKE    USING   'WT'
                                         IT_ZSIDRHS-ZFWETM
                                         IT_ZSIDRHS-ZFWET
                                CHANGING L_TYPE
                                         W_EDI_RECORD.

*>> FLAT FILE MEA MAKE( 수량 )
      IF NOT IT_ZSIDRHS-ZFQNT IS INITIAL AND
         NOT IT_ZSIDRHS-ZFQNTM IS INITIAL.
         L_TYPE = '18'.
         W_POINT = 0.
         PERFORM  P3000_MEA_MAKE    USING   '5AA'
                                            IT_ZSIDRHS-ZFQNTM
                                            IT_ZSIDRHS-ZFQNT
                                   CHANGING L_TYPE
                                            W_EDI_RECORD.
      ENDIF.

*>> FLAT FILE MEA MAKE( 환급물량 )
      IF NOT IT_ZSIDRHS-ZFREQN IS INITIAL AND
         NOT IT_ZSIDRHS-ZFREQNM IS INITIAL.
         L_TYPE = '18'.
         PERFORM   P3000_MEA_MAKE   USING   '5DA'
                                            IT_ZSIDRHS-ZFREQNM
                                            IT_ZSIDRHS-ZFREQN
                                   CHANGING L_TYPE
                                            W_EDI_RECORD.
      ENDIF.

*>> FLAT FILE MEA MAKE( 첨부서류 ).
      SELECT COUNT( * ) INTO W_LIN_CNT   FROM  ZTIDRHSL
      WHERE  ZFBLNO     EQ   W_ZFBLNO
      AND    ZFCLSEQ    EQ   W_ZFCLSEQ
      AND    ZFCONO     EQ   IT_ZSIDRHS-ZFCONO.

      IF NOT W_LIN_CNT IS INITIAL.
         L_TYPE = '18'.
         PERFORM   P3000_MEA_MAKE    USING    'CT'
                                              'NMB'
                                              W_LIN_CNT
                                     CHANGING L_TYPE
                                              W_EDI_RECORD.
      ENDIF.

*>> FLAT FILE MEA MAKE( 총규격수 )
      SELECT COUNT( * ) INTO W_DET_CNT   FROM  ZTIDRHSD
      WHERE  ZFBLNO     EQ   W_ZFBLNO
      AND    ZFCLSEQ    EQ   W_ZFCLSEQ
      AND    ZFCONO     EQ   IT_ZSIDRHS-ZFCONO.
      IF NOT W_DET_CNT IS INITIAL.
         L_TYPE = '18'.
         PERFORM   P3000_MEA_MAKE     USING   'CT'
                                              'NMB'
                                              W_DET_CNT
                                     CHANGING L_TYPE
                                              W_EDI_RECORD.
      ENDIF.

*>> FLAT FILE MOA MAKE( 과세가격 원화)
      L_TYPE = '19'.
      PERFORM  P3000_MOA_MAKE      USING   '40'
                                           IT_ZSIDRHS-ZFTBAK
                                           'KRW'
                                  CHANGING L_TYPE
                                           W_EDI_RECORD.

*>> FLAT FILE MOA MAKE( 과세가격 외화 )
      IF NOT IT_ZSIDRHS-ZFTBAU IS INITIAL.
         L_TYPE = '19'.
         PERFORM  P3000_MOA_MAKE      USING   '40'
                                              IT_ZSIDRHS-ZFTBAU
                                              'USD'
                                      CHANGING L_TYPE
                                               W_EDI_RECORD.
      ENDIF.

*----------------< 수입요건 사항 SEND >--------------------------------*
      LOOP  AT  IT_ZSIDRHSL WHERE  ZFBLNO  EQ  W_ZFBLNO
                            AND    ZFCLSEQ EQ  W_ZFCLSEQ
                            AND    ZFCONO  EQ  IT_ZSIDRHS-ZFCONO.


*>> FLAT FILE DOC MAKE ( 수입요건 확인서 )
         L_TYPE = '20'.
         PERFORM  P3000_DOC_IDR     USING    '911'
                                             '수입승인서'
                                             IT_ZSIDRHSL-ZFCNNO
                                             IT_ZSIDRHSL-ZFLACD
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.

*>> FLAT FILE DTM MAKE ( 발급일자 )
         L_TYPE = '21'.
         PERFORM  P3000_DTM_MAKE     USING   '182'
                                             IT_ZSIDRHSL-ZFISZDT
                                             '102'
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.

*>> FLAT FILE MEA MAKE( 사용수량 )
         IF NOT ( IT_ZSIDRHSL-ZFCUQN  IS INITIAL OR
                  IT_ZSIDRHSL-ZFCUQNM IS INITIAL ).
            L_TYPE = '22'.
            PERFORM  P3000_MEA_MAKE  USING   '5AA'
                                             IT_ZSIDRHSL-ZFCUQNM
                                             IT_ZSIDRHSL-ZFCUQN
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.
         ENDIF.

      ENDLOOP.

*>> FLAT FILE TAX MAKE( 관세 ).
      IF NOT IT_ZSIDRHS-ZFCUAMT IS INITIAL AND
         NOT IT_ZSIDRHS-ZFTXCD  IS INITIAL.
         L_TYPE = '23'.
         PERFORM  P3000_TAX_MAKE        USING    '1'
                                              'CUD'
                                              IT_ZSIDRHS-ZFTXCD
                                              '5AT'
                                              'KCS'
                                              SPACE
                                              SPACE
                                              SPACE
                                              SPACE
                                              IT_ZSIDRHS-ZFRDRT
                                              IT_ZSIDRHS-ZFTXAMCD
                                              SPACE
                                              SPACE
                                     CHANGING L_TYPE
                                              W_EDI_RECORD.
      ENDIF.

*>> FLAT FILE MOA MAKE( 관세액 )
      IF NOT IT_ZSIDRHS-ZFCUAMT IS INITIAL AND
         NOT IT_ZSIDRHS-ZFTXCD  IS INITIAL.
         L_TYPE = '24'.
         PERFORM  P3000_MOA_MAKE         USING   '55'
                                                 IT_ZSIDRHS-ZFCUAMT
                                                 'KRW'
                                        CHANGING L_TYPE
                                                 W_EDI_RECORD.
      ENDIF.

*>> FLAT FILE MOA MAKE ( 관세감면액 )
      IF NOT IT_ZSIDRHS-ZFCUAMT IS INITIAL AND
         NOT IT_ZSIDRHS-ZFTXCD  IS INITIAL AND
         NOT IT_ZSIDRHS-ZFCCAMT IS INITIAL.
         L_TYPE = '24'.
         PERFORM  P3000_MOA_MAKE         USING   '46'
                                                 IT_ZSIDRHS-ZFCCAMT
                                                 'KRW'
                                        CHANGING L_TYPE
                                                 W_EDI_RECORD.
      ENDIF.

*>> FLAT FILE GIS MAKE ( 관세 감면/ 분납 부호 )
      IF NOT IT_ZSIDRHS-ZFCDPCD IS INITIAL AND
         NOT IT_ZSIDRHS-ZFCDPNO IS INITIAL AND
         NOT IT_ZSIDRHS-ZFCUAMT IS INITIAL AND
         NOT IT_ZSIDRHS-ZFTXCD  IS INITIAL AND
         NOT IT_ZSIDRHS-ZFCCAMT IS INITIAL.
         L_TYPE = '25'.
         PERFORM  P3000_GIS_MAKE         USING   IT_ZSIDRHS-ZFCDPCD
                                                 IT_ZSIDRHS-ZFCDPNO
                                                 SPACE
                                        CHANGING L_TYPE
                                                 W_EDI_RECORD.
      ENDIF.

*>> FLAT FILE TAX MAKE( 내국세 )
      IF NOT ( IT_ZSIDRHS-ZFHMTCD IS INITIAL  OR
               IT_ZSIDRHS-ZFHMTRT IS INITIAL  OR
               IT_ZSIDRHS-ZFHMTTY IS INITIAL  OR
               IT_ZSIDRHS-ZFHMAMT IS INITIAL ).

         L_TYPE = '23'.
         PERFORM  P3000_TAX_MAKE        USING    '1'
                                                 'PRF'
                                                 IT_ZSIDRHS-ZFHMTCD
                                                 '5AZ'
                                                 'KCS'
                                                 IT_ZSIDRHS-ZFHMTRT
                                                 IT_ZSIDRHS-ZFHMTTY
                                                 '5AU'
                                                 'KCS'
                                                 SPACE
                                                 IT_ZSIDRHS-ZFSCCD
                                                 '5AV'
                                                 'KCS'
                                        CHANGING L_TYPE
                                                 W_EDI_RECORD.

*>> FLAT FILE MOA MAKE ( 내국세액 )
      IF NOT ( IT_ZSIDRHS-ZFHMTCD IS INITIAL  OR
               IT_ZSIDRHS-ZFHMTRT IS INITIAL  OR
               IT_ZSIDRHS-ZFHMTTY IS INITIAL  OR
               IT_ZSIDRHS-ZFHMAMT IS INITIAL ).

            L_TYPE = '24'.
            PERFORM  P3000_MOA_MAKE         USING   '56'
                                                    IT_ZSIDRHS-ZFHMAMT
                                                    'KRW'
                                           CHANGING L_TYPE
                                                    W_EDI_RECORD.
         ENDIF.
      ENDIF.

*>> FLAT FILE TAX MAKE ( 교육세 )
      IF NOT IT_ZSIDRHS-ZFETXCD IS INITIAL AND
         NOT IT_ZSIDRHS-ZFEDAMT IS INITIAL.
         L_TYPE = '23'.
         PERFORM  P3000_TAX_MAKE            USING   '1'
                                                    '5AB'
                                                    IT_ZSIDRHS-ZFETXCD
                                                    '5AW'
                                                    'KCS'
                                                    SPACE
                                                    SPACE
                                                    SPACE
                                                    SPACE
                                                    SPACE
                                                    SPACE
                                                    SPACE
                                                    SPACE
                                           CHANGING L_TYPE
                                                    W_EDI_RECORD.

*>> FLAT FILE MOA MAKE( 교육세 ).
      IF NOT IT_ZSIDRHS-ZFETXCD IS INITIAL AND
         NOT IT_ZSIDRHS-ZFEDAMT IS INITIAL.
            L_TYPE = '24'.
            PERFORM  P3000_MOA_MAKE          USING   '56'
                                                     IT_ZSIDRHS-ZFEDAMT
                                                     'KRW'
                                             CHANGING L_TYPE
                                                      W_EDI_RECORD.
         ENDIF.

      ENDIF.

*>> FLAT FILE TAX MAKE ( 농특세 )
      IF NOT IT_ZSIDRHS-ZFATXCD IS INITIAL AND
         NOT IT_ZSIDRHS-ZFAGAMT IS INITIAL.
         L_TYPE = '23'.
         PERFORM  P3000_TAX_MAKE            USING   '1'
                                                    'CAP'
                                                    IT_ZSIDRHS-ZFATXCD
                                                    '5AX'
                                                    'KCS'
                                                    SPACE
                                                    SPACE
                                                    SPACE
                                                    SPACE
                                                    SPACE
                                                    SPACE
                                                    SPACE
                                                    SPACE
                                           CHANGING L_TYPE
                                                    W_EDI_RECORD.

*>> FLAT FILE MOA MAKE( 농특세 ).
      IF NOT IT_ZSIDRHS-ZFATXCD IS INITIAL AND
         NOT IT_ZSIDRHS-ZFAGAMT IS INITIAL.
            L_TYPE = '24'.
            PERFORM  P3000_MOA_MAKE          USING   '56'
                                                     IT_ZSIDRHS-ZFAGAMT
                                                     'KRW'
                                             CHANGING L_TYPE
                                                      W_EDI_RECORD.
         ENDIF.

      ENDIF.

*>> FLAT FILE TAX MAKE ( 부가세 )
      IF NOT IT_ZSIDRHS-ZFVTXCD IS INITIAL AND
         NOT IT_ZSIDRHS-ZFVTXTY IS INITIAL AND
         NOT IT_ZSIDRHS-ZFVAAMT IS INITIAL.
         L_TYPE = '23'.
         PERFORM  P3000_TAX_MAKE          USING   '1'
                                                  'VAT'
                                                  IT_ZSIDRHS-ZFVTXCD
                                                  '5AY'
                                                  'KCS'
                                                  SPACE
                                                  IT_ZSIDRHS-ZFVTXTY
                                                  '52'
                                                  'KCS'
                                                  SPACE
                                                  SPACE
                                                  SPACE
                                                  SPACE
                                         CHANGING L_TYPE
                                                  W_EDI_RECORD.
*>> FLAT FILE MOA MAKE ( 부가세액 )
      IF NOT IT_ZSIDRHS-ZFVTXCD IS INITIAL AND
         NOT IT_ZSIDRHS-ZFVTXTY IS INITIAL AND
         NOT IT_ZSIDRHS-ZFVAAMT IS INITIAL.
            L_TYPE = '24'.
            PERFORM  P3000_MOA_MAKE         USING   '1'
                                                    IT_ZSIDRHS-ZFVAAMT
                                                    'KRW'
                                            CHANGING L_TYPE
                                                     W_EDI_RECORD.
         ENDIF.

*>> FLAT FILE MOA MAKE ( 부가세 감면액 )
         IF NOT IT_ZSIDRHS-ZFVCAMT IS INITIAL.
            L_TYPE = '24'.
            PERFORM  P3000_MOA_MAKE        USING   '46'
                                                   IT_ZSIDRHS-ZFVCAMT
                                                   'KRW'
                                           CHANGING L_TYPE
                                                    W_EDI_RECORD.
         ENDIF.
      ENDIF.

*>> FLAT FILE TAX MAKE ( 특수세액 계산근거 )
      IF NOT IT_ZSIDRHS-ZFSCCS IS INITIAL.
         L_TYPE = '23'.
         PERFORM  P3000_TAX_MAKE       USING   '9'
                                               SPACE
                                               SPACE
                                               SPACE
                                               SPACE
                                               IT_ZSIDRHS-ZFSCCS
                                               SPACE
                                               SPACE
                                               SPACE
                                               SPACE
                                               SPACE
                                               SPACE
                                               SPACE
                                      CHANGING L_TYPE
                                               W_EDI_RECORD.
      ENDIF.




*-----------------------< 규격 사항 SEND >-----------------------------*
      LOOP AT IT_ZSIDRHSD WHERE ZFBLNO  EQ  IT_ZSIDRHS-ZFBLNO
                          AND   ZFCLSEQ EQ  IT_ZSIDRHS-ZFCLSEQ
                          AND   ZFCONO  EQ  IT_ZSIDRHS-ZFCONO.

*>> FLAT FILE SEQ MAKE ( 규격번호 )
         L_TYPE = '26'.
         PERFORM  P3000_SEQ_MAKE       USING   IT_ZSIDRHSD-ZFRONO
                                      CHANGING L_TYPE
                                               W_EDI_RECORD.
*>> FLAT FILE GIR MAKE ( 규격 )
         IF NOT IT_ZSIDRHSD-ZFGDDS1 IS INITIAL.

            L_TYPE = '27'.
            PERFORM  P3000_GIR_MAKE       USING   '1'
                                                  IT_ZSIDRHSD-ZFGDDS1
                                                  IT_ZSIDRHSD-ZFGDDS2
                                                  IT_ZSIDRHSD-ZFGDDS3
                                         CHANGING L_TYPE
                                                  W_EDI_RECORD.
         ENDIF.

*>> FLAT FILE GIR MAKE ( 성분 )
         IF NOT IT_ZSIDRHSD-ZFGDIN1 IS INITIAL.
            L_TYPE = '27'.
            PERFORM  P3000_GIR_MAKE       USING   '5'
                                                  IT_ZSIDRHSD-ZFGDIN1
                                                  IT_ZSIDRHSD-ZFGDIN2
                                                  SPACE
                                         CHANGING L_TYPE
                                                  W_EDI_RECORD.
         ENDIF.

*>> FLAT FILE MEA MAKE( 수량 )
         IF NOT IT_ZSIDRHSD-ZFQNT IS INITIAL AND
            NOT IT_ZSIDRHSD-ZFQNTM IS INITIAL.
            L_TYPE = '28'.
            W_POINT = 4.
            PERFORM  P3000_MEA_MAKE       USING   '5AA'
                                                  IT_ZSIDRHSD-ZFQNTM
                                                  IT_ZSIDRHSD-ZFQNT
                                         CHANGING L_TYPE
                                                  W_EDI_RECORD.
         ENDIF.

*>> FLAT FILE MOA MAKE( 단가 )
         L_TYPE = '29'.
         WRITE IT_ZSIDRHSD-NETPR      TO           W_TEXT_AMOUNT
                                      CURRENCY     IT_ZSIDRHSD-ZFCUR.
         PERFORM    P2000_WRITE_NO_MASK     CHANGING  W_TEXT_AMOUNT.

         PERFORM  P3000_MOA_IDR          USING   '146'
                                                 W_TEXT_AMOUNT
                                        CHANGING L_TYPE
                                                 W_EDI_RECORD.

*>> FLAT FILE MOA MAKE( 금액 )
         IF NOT IT_ZSIDRHSD-ZFAMT IS INITIAL.
            WRITE    IT_ZSIDRHSD-ZFAMT  TO      W_TEXT_AMOUNT
                     CURRENCY                   IT_ZSIDRHSD-ZFCUR.
            PERFORM    P2000_WRITE_NO_MASK     CHANGING  W_TEXT_AMOUNT.

            L_TYPE = '29'.
            PERFORM  P3000_MOA_IDR       USING   '203'
                                                 W_TEXT_AMOUNT
                                        CHANGING L_TYPE
                                                 W_EDI_RECORD.
         ENDIF.

      ENDLOOP.

   ENDLOOP.

*>> FLAT FILE UNS MAKE ( 요약부문 )
   L_TYPE = '30'.
   PERFORM  P3000_UNS_MAKE      USING   'S'
                               CHANGING L_TYPE
                                        W_EDI_RECORD.

*>> FLAT FILE CNT MAKE ( 총중량 )
   L_TYPE = '31'.
   PERFORM  P3000_CNT_MAKE      USING   '7'
                                        ZTIDR-ZFTOWT
                                        'KG'
                               CHANGING L_TYPE
                                        W_EDI_RECORD.

*>> FLAT FILE CNT MAKE ( 총포장갯수 )
*   IF NOT ZTIDR-ZFPKCNT IS INITIAL.
   L_TYPE = '31'.
   PERFORM  P3000_CNT_MAKE      USING   '11'
                                        ZTIDR-ZFPKCNT
                                        ZTIDR-ZFPKNM
                                CHANGING L_TYPE
                                        W_EDI_RECORD.
*   ENDIF.

*>> FLAT FILE CNT MAKE ( 총란수 )
   SELECT COUNT( * )  INTO  W_HS_CNT  FROM  ZTIDRHS
   WHERE  ZFBLNO      EQ    W_ZFBLNO
   AND    ZFCLSEQ     EQ    W_ZFCLSEQ.

   IF NOT W_HS_CNT IS INITIAL.
      L_TYPE = '31'.
      PERFORM  P3000_CNT_MAKE     USING   '5'
                                          W_HS_CNT
                                          SPACE
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.
   ENDIF.

*>> FLAT FILE MOA MAKE ( 총관세 ).
   IF NOT ZTIDR-ZFCUAMTS IS INITIAL.
      L_TYPE = '32'.
      PERFORM  P3000_TAX_IDR         USING   '3'
                                             'CUD'
                                             SPACE
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.
      L_TYPE = '33'.
      PERFORM  P3000_MOA_MAKE     USING   '55'
                                          ZTIDR-ZFCUAMTS
                                          'KRW'
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.
   ENDIF.

   IF NOT ZTIDR-ZFSCAMTS IS INITIAL.
*>> FLAT FILE TAX MAKE ( 특소세 ).
      L_TYPE = '32'.
      PERFORM  P3000_TAX_IDR         USING   '3'
                                             'CST'
                                             SPACE
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.

*>> FLAT FILE MOA MAKE ( 특소세 ).
      L_TYPE = '33'.
      PERFORM  P3000_MOA_MAKE     USING   '161'
                                          ZTIDR-ZFSCAMTS
                                          'KRW'
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.
   ENDIF.

   IF NOT ZTIDR-ZFTRAMTS IS INITIAL.
*>> FLAT FILE TAX MAKE ( 교통세 ).
      L_TYPE = '32'.
      PERFORM  P3000_TAX_IDR         USING   '3'
                                             '5AA'
                                             SPACE
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.

*>> FLAT FILE MOA MAKE ( 교통세 ).
      L_TYPE = '33'.
      PERFORM  P3000_MOA_MAKE     USING   '161'
                                          ZTIDR-ZFTRAMTS
                                          'KRW'
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.
   ENDIF.

   IF NOT ZTIDR-ZFDRAMTS IS INITIAL.
*>> FLAT FILE TAX MAKE ( 주세 ).
      L_TYPE = '32'.
      PERFORM  P3000_TAX_IDR         USING   '3'
                                             'TAC'
                                             SPACE
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.

*>> FLAT FILE MOA MAKE ( 주세 ).
      L_TYPE = '33'.
      PERFORM  P3000_MOA_MAKE     USING   '161'
                                          ZTIDR-ZFDRAMTS
                                          'KRW'
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.
   ENDIF.

   IF NOT ZTIDR-ZFEDAMTS IS INITIAL.
*>> FLAT FILE TAX MAKE ( 교육세 ).
      L_TYPE = '32'.
      PERFORM  P3000_TAX_IDR         USING   '3'
                                             '5AB'
                                             SPACE
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.

*>> FLAT FILE MOA MAKE ( 교육세 ).
      L_TYPE = '33'.
      PERFORM  P3000_MOA_MAKE     USING   '161'
                                          ZTIDR-ZFEDAMTS
                                          'KRW'
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.
   ENDIF.

   IF NOT ZTIDR-ZFAGAMTS IS INITIAL.
*>> FLAT FILE TAX MAKE ( 농특세 ).
      L_TYPE = '32'.
      PERFORM  P3000_TAX_IDR         USING   '3'
                                             'CAP'
                                             SPACE
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.

*>> FLAT FILE MOA MAKE ( 농특세 ).
      L_TYPE = '33'.
      PERFORM  P3000_MOA_MAKE     USING   '161'
                                          ZTIDR-ZFAGAMTS
                                          'KRW'
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.
   ENDIF.


   IF NOT ZTIDR-ZFVAAMTS IS INITIAL.
*>> FLAT FILE TAX MAKE (  부가세 ).
      L_TYPE = '32'.
      PERFORM  P3000_TAX_IDR         USING   '3'
                                             'VAT'
                                             SPACE
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.

*>> FLAT FILE MOA MAKE ( 부가세 ).
      L_TYPE = '33'.
      PERFORM  P3000_MOA_MAKE     USING   '161'
                                          ZTIDR-ZFVAAMTS
                                          'KRW'
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.
   ENDIF.

   IF NOT ZTIDR-ZFIDAMTS IS INITIAL.
*>> FLAT FILE TAX MAKE (  신고지연 가산세 ).
      L_TYPE = '32'.
      PERFORM  P3000_TAX_IDR         USING   '6'
                                             SPACE
                                             SPACE
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.

*>> FLAT FILE MOA MAKE ( 신고지연 가산세 ).
      L_TYPE = '33'.
      PERFORM  P3000_MOA_MAKE     USING   '161'
                                          ZTIDR-ZFIDAMTS
                                          'KRW'
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.
   ENDIF.

   IF NOT ZTIDR-ZFTXAMTS IS INITIAL.
*>> FLAT FILE TAX MAKE (  총세액 ).
      L_TYPE = '32'.
      PERFORM  P3000_TAX_IDR         USING   '4'
                                             SPACE
                                             SPACE
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.

*>> FLAT FILE MOA MAKE ( 총세액 ).
      L_TYPE = '33'.
      PERFORM  P3000_MOA_MAKE     USING   '161'
                                          ZTIDR-ZFTXAMTS
                                          'KRW'
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.
   ENDIF.

*>> 마지막 New Line Char. Cut.
   PERFORM P3000_EDI_RECORD_ADJUST  CHANGING W_EDI_RECORD.

ENDFUNCTION.
