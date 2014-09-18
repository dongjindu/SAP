FUNCTION ZIM_LG_LOCAPP_EDI_DOC .
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(W_ZFREQNO) LIKE  ZTREQHD-ZFREQNO
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
*       "MASTER L/C Seg 7 상품명세-상품용역명세
       L_ZFDSOG2      LIKE     IT_ZSMLCSG7G-ZFDSOG1,
       L_ZFDSOG3      LIKE     IT_ZSMLCSG7G-ZFDSOG1,
       L_ZFDSOG4      LIKE     IT_ZSMLCSG7G-ZFDSOG1,
       L_ZFDSOG5      LIKE     IT_ZSMLCSG7G-ZFDSOG1.

*> MASTER L/C READ.
   CALL FUNCTION 'ZIM_GET_LOCAL_LC_DATA'
        EXPORTING
              ZFREQNO           =       W_ZFREQNO
        IMPORTING
              W_ZTLLCHD         =       ZTLLCHD
              W_ZTLLCSG23       =       ZTLLCSG23
        TABLES
              IT_ZSLLCOF        =       IT_ZSLLCOF     "물품매도확약서
              IT_ZSLLCOF_ORG    =       IT_ZSLLCOF_ORG "물품매도확약서.
        EXCEPTIONS
              NOT_FOUND     =       4
              NOT_INPUT     =       8.

   CASE SY-SUBRC.
      WHEN 4.
         MESSAGE E018 WITH W_ZFREQNO RAISING  CREATE_ERROR.
      WHEN 8.
         MESSAGE E019 RAISING  CREATE_ERROR.
   ENDCASE.

   SELECT SINGLE * FROM ZTREQHD
          WHERE    ZFREQNO  EQ   W_ZFREQNO.

   SELECT SINGLE * FROM ZTREQST
          WHERE    ZFREQNO  EQ   W_ZFREQNO
          AND      ZFAMDNO  EQ   '00000'.


*-----------------------------------------------------------------------
* FLAT-FILE RECORD CREATE
*-----------------------------------------------------------------------
   L_TYPE  =  '00'.

*>> FLAT FILE HEADER MAKE.
   PERFORM  P3000_HEADER_MAKE    USING    'LOCAPP'
                                          ZTREQHD-BUKRS
                                          ZTREQHD-ZFOPBN
                                          W_ZFDHENO
                                 CHANGING W_EDI_RECORD.

*>> FLAT FILE BEG MAKE.(전자문서 시작)
   L_TYPE = '01'.
   PERFORM  P3000_BGM_MAKE       USING    '2AD'
                                          W_ZFDHENO
                                          ZTLLCHD-ZFEDFN
                                          'AB'
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.

*>> FLAT FILE BUS MAKE.(개설근거별 용도)
   L_TYPE = '02'.
   PERFORM  P3000_BUS_1_MAKE     USING    '1'
                                          ZTLLCHD-ZFUSG
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.

*>> FLAT FILE RFF MAKE.(물품매도확약서번호)
   L_TYPE = '03'.
   LOOP AT IT_ZSLLCOF.
      PERFORM  P3000_RFF_MAKE       USING    'AAG'
                                             IT_ZSLLCOF-ZFOFFER
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.
   ENDLOOP.
*>> FLAT FILE RFF MAKE.(개설회차)
   PERFORM  P3000_RFF_MAKE       USING    '2AG'
                                          ZTLLCHD-ZFOPCNT
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.

*>> FLAT FILE DTM MAKE.(개설신청일자)
   L_TYPE = '04'.
   PERFORM  P3000_DTM_MAKE       USING    '2AA'
                                          ZTREQST-ZFAPPDT
                                          '101'
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.

*>> FLAT FILE DTM MAKE.(선적서류제시기간)
   PERFORM  P3000_DTM_MAKE       USING    '272'
                                          ZTLLCHD-ZFDPRP
                                          '804'
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.
*>> FLAT FILE DTM MAKE.(물품인도기일)
   PERFORM  P3000_DTM_MAKE       USING    '2'
                                          ZTLLCHD-ZFGDDT
                                          '101'
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.

*>> FLAT FILE DTM MAKE.(유효기일)
   PERFORM  P3000_DTM_MAKE       USING    '123'
                                          ZTLLCHD-ZFEXDT
                                          '101'
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.

*>> FLAT FILE TSR MAKE.(분할인도 허용여부).
   L_TYPE = '05'.
   PERFORM  P3000_TSR_1_MAKE       USING    ZTLLCHD-ZFPRAL
                                   CHANGING L_TYPE
                                            W_EDI_RECORD.

*>> FLAT FILE FTX MAKE.(대표 공급물품명).
     L_TYPE = '06'.
     PERFORM P3000_FTX_MAKE        USING    'AAA'
                                             ZTLLCHD-ZFGDSC1
                                             ZTLLCHD-ZFGDSC2
                                             ZTLLCHD-ZFGDSC3
                                             ZTLLCHD-ZFGDSC4
                                             ZTLLCHD-ZFGDSC5
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.
*>> FLAT FILE FTX MAKE.(기타정보).
   IF NOT ( ZTLLCHD-ZFGDSC1 IS INITIAL AND
            ZTLLCHD-ZFGDSC2 IS INITIAL AND
            ZTLLCHD-ZFGDSC3 IS INITIAL AND
            ZTLLCHD-ZFGDSC4 IS INITIAL AND
            ZTLLCHD-ZFGDSC5 IS INITIAL ).
      PERFORM  P3000_FTX_MAKE       USING    'ACB'
                                             ZTLLCHD-ZFGDSC1
                                             ZTLLCHD-ZFGDSC2
                                             ZTLLCHD-ZFGDSC3
                                             ZTLLCHD-ZFGDSC4
                                             ZTLLCHD-ZFGDSC5
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.
   ENDIF.

*>> FLAT FILE FII MAKE.(개설(의뢰)은행).
   L_TYPE = '07'.
   PERFORM  P3000_FII_MAKE_1     USING    'AZ'
                                          ZTLLCHD-ZFOPBNCD
                                          '25'
                                          'BOK'
                                          ZTLLCHD-ZFOBNM
                                          ZTLLCHD-ZFOBBR
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.
*>> FLAT FILE NAD MAKE.(개설의뢰인).
   L_TYPE = '08'.
   PERFORM  P3000_NAD_AX_MAKE    USING    'DF'
                                          ZTLLCSG23-ZFAPPNM1
                                          ZTLLCSG23-ZFAPPNM2
                                          ZTLLCSG23-ZFAPPNM3
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.
*>> FLAT FILE NAD MAKE.(수혜자).
   PERFORM  P3000_NAD_AX_MAKE    USING    'DG'
                                          ZTLLCSG23-ZFBENI1
                                          ZTLLCSG23-ZFBENI2
                                          ZTLLCSG23-ZFBENI3
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.
*>> FLAT FILE NAD MAKE.(전자서명)
   PERFORM  P3000_NAD_AX_MAKE    USING    'AX'
                                          ZTLLCSG23-ZFELENM
                                          ZTLLCSG23-ZFREPRE
                                          ZTLLCSG23-ZFELEID
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.

*>> FLAT FILE DOC MAKE.(물품수령증명서).
    L_TYPE = '09'.
    PERFORM  P3000_DOC_MAKE        USING    '2AH'
                                            ZTLLCSG23-ZFNODOM
                                   CHANGING L_TYPE
                                            W_EDI_RECORD.

*>> FLAT FILE DOC MAKE.(세금계산서 사본).
    IF NOT ZTLLCSG23-ZFBILYN IS INITIAL.
       PERFORM  P3000_DOC_MAKE        USING    '2AJ'
                                               ZTLLCSG23-ZFNOBIL
                                      CHANGING L_TYPE
                                               W_EDI_RECORD.
    ELSE.
*>> FLAT FILE DOC MAKE.(송장).
       PERFORM  P3000_DOC_MAKE        USING    '2AK'
                                               ZTLLCSG23-ZFNOBIL
                                      CHANGING L_TYPE
                                               W_EDI_RECORD.
    ENDIF.
*>> FLAT FILE DOC MAKE.(내국신용장사본).
    PERFORM  P3000_DOC_MAKE        USING    '2AP'
                                            ZTLLCSG23-ZFNOLLC
                                   CHANGING L_TYPE
                                            W_EDI_RECORD.

*>> FLAT FILE DOC MAKE.(물품매도확약서 사본).
   PERFORM  P3000_DOC_MAKE        USING    '310'
                                           ZTLLCSG23-ZFNOOF
                                  CHANGING L_TYPE
                                           W_EDI_RECORD.

*>> FLAT FILE FTX MAKE.(기타구비서류).
   IF NOT ( ZTLLCSG23-ZFEDOC1 IS INITIAL AND
            ZTLLCSG23-ZFEDOC2 IS INITIAL AND
            ZTLLCSG23-ZFEDOC3 IS INITIAL AND
            ZTLLCSG23-ZFEDOC4 IS INITIAL AND
            ZTLLCSG23-ZFEDOC5 IS INITIAL ).
      L_TYPE = '10'.
      PERFORM  P3000_FTX_MAKE       USING    'ABX'
                                             ZTLLCSG23-ZFEDOC1
                                             ZTLLCSG23-ZFEDOC2
                                             ZTLLCSG23-ZFEDOC3
                                             ZTLLCSG23-ZFEDOC4
                                             ZTLLCSG23-ZFEDOC5
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.
   ENDIF.
*>> FLAT FILE BUS MAKE.(내국신용장의 종류).
   L_TYPE = '11'.
   PERFORM  P3000_BUS_MAKE       USING    ZTLLCHD-ZFLLCTY
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.
*>> FLAT FILE MOA MAKE.(개설금액표시(원화, 외화))
   L_TYPE = '12'.
   IF ZTLLCHD-ZFLLCTY = '2AA' OR ZTLLCHD-ZFLLCTY = '2AC'. "원화표시
     PERFORM  P3000_MOA_MAKE       USING    '2AE'
                                            ZTLLCHD-ZFOPKAM
                                            ZTLLCHD-ZFKRW
                                   CHANGING L_TYPE
                                            W_EDI_RECORD.
   ELSE. "외화표시
     PERFORM  P3000_MOA_MAKE       USING    '2AD'
                                            ZTLLCHD-ZFOPAMT
                                            ZTLLCHD-ZFOPAMTC
                                   CHANGING L_TYPE
                                            W_EDI_RECORD.
   ENDIF.

*>> FLAT FILE DOC MAKE.(개설근거서류 종류)
   IF NOT ZTLLCHD-ZFOPRED IS INITIAL.
      L_TYPE = '13'.
      PERFORM  P3000_DOC_1_MAKE     USING    ZTLLCHD-ZFOPRED
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.
   ENDIF.

*>> FLAT FILE RFF MAKE.(신용장번호)
   IF NOT ZTLLCHD-ZFDCNO IS INITIAL.
     L_TYPE = '14'.
     PERFORM  P3000_RFF_MAKE       USING    'AAC'
                                            ZTLLCHD-ZFDCNO
                                   CHANGING L_TYPE
                                            W_EDI_RECORD.
   ENDIF.
*>> FLAT FILE MOA MAKE.(결제통화 및 금액)
   IF NOT ZTLLCHD-ZFDCAMT IS INITIAL.
     L_TYPE = '15'.
     PERFORM  P3000_MOA_MAKE       USING    '212'
                                            ZTLLCHD-ZFDCAMT
                                            ZTLLCHD-WAERS
                                   CHANGING L_TYPE
                                            W_EDI_RECORD.
   ENDIF.

*>> FLAT FILE DTM MAKE.(선적기일)
   IF NOT ZTLLCHD-ZFDEDT IS INITIAL.
     L_TYPE = '16'.
     PERFORM  P3000_DTM_MAKE       USING    '38'
                                            ZTLLCHD-ZFDEDT
                                            '101'
                                   CHANGING L_TYPE
                                            W_EDI_RECORD.
   ENDIF.

*>> FLAT FILE DTM MAKE.(유효기일)
   IF NOT ZTLLCHD-ZFDEXDT IS INITIAL.
     L_TYPE = '16'.
     PERFORM  P3000_DTM_MAKE       USING    '123'
                                            ZTLLCHD-ZFDEXDT
                                            '101'
                                   CHANGING L_TYPE
                                            W_EDI_RECORD.
   ENDIF.

*>> FLAT FILE NAD MAKE.(수출상대방)
   IF NOT ZTLLCHD-ZFEXPR1 IS INITIAL.
     L_TYPE = '17'.
     PERFORM  P3000_NAD_MAKE       USING    'IM'
                                            ZTLLCHD-ZFEXPR1
                                            ZTLLCHD-ZFEXPR2
                                            ZTLLCHD-ZFEXPR3
                                            SPACE
                                            SPACE
                                   CHANGING L_TYPE
                                            W_EDI_RECORD.
   ENDIF.

*>> FLAT FILE LOC MAKE.(수출지역)
   IF NOT ZTLLCHD-ZFEXAR IS INITIAL.
     L_TYPE = '18'.
     PERFORM  P3000_LOC_2_MAKE       USING    '28'
                                              ZTLLCHD-ZFEXAR
                                              '162'
                                              '5'
                                   CHANGING L_TYPE
                                            W_EDI_RECORD.
   ENDIF.

*>> FLAT FILE FII MAKE.(발행은행)
   IF NOT ZTLLCHD-ZFISBN IS INITIAL.
     L_TYPE = '19'.
     PERFORM  P3000_FII_MAKE       USING    'AZ'
                                            ZTLLCHD-ZFISBN
                                            ZTLLCHD-ZFISBNB
                                            SPACE
                                   CHANGING L_TYPE
                                            W_EDI_RECORD.
   ENDIF.

*>> FLAT FILE PAT MAKE.(대금결제조건)
   IF NOT ZTLLCHD-ZFDCNO IS INITIAL.
     L_TYPE = '20'.
     PERFORM  P3000_PAT_1_MAKE       USING    '1'
                                            ZTLLCHD-ZFTOP
                                   CHANGING L_TYPE
                                            W_EDI_RECORD.
   ENDIF.

*>> FLAT FILE FTX MAKE.(대표 수출물품명)
   IF NOT ZTLLCHD-ZFEXGNM1 IS INITIAL.
     L_TYPE = '21'.
     PERFORM  P3000_FTX_MAKE       USING    'AAA'
                                            ZTLLCHD-ZFEXGNM1
                                            ZTLLCHD-ZFEXGNM2
                                            ZTLLCHD-ZFEXGNM3
                                            ZTLLCHD-ZFEXGNM4
                                            ZTLLCHD-ZFEXGNM5
                                   CHANGING L_TYPE
                                            W_EDI_RECORD.
   ENDIF.

   PERFORM P3000_EDI_RECORD_ADJUST  CHANGING W_EDI_RECORD.

ENDFUNCTION.
