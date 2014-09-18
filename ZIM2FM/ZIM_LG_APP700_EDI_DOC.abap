FUNCTION ZIM_LG_APP700_EDI_DOC .
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
       L_ZFDSOG2      LIKE     IT_ZSMLCSG7G-ZFDSOG1,
       L_ZFDSOG3      LIKE     IT_ZSMLCSG7G-ZFDSOG1,
       L_ZFDSOG4      LIKE     IT_ZSMLCSG7G-ZFDSOG1,
       L_ZFDSOG5      LIKE     IT_ZSMLCSG7G-ZFDSOG1.

*> MASTER L/C READ.
   CALL FUNCTION 'ZIM_GET_MASTER_LC_DATA'
        EXPORTING
              ZFREQNO           =       W_ZFREQNO
        IMPORTING
              W_ZTMLCHD         =       ZTMLCHD
              W_ZTMLCSG2        =       ZTMLCSG2
              W_ZTMLCSG910      =       ZTMLCSG910
        TABLES
              IT_ZSMLCSG7G      =       IT_ZSMLCSG7G
              IT_ZSMLCSG7O      =       IT_ZSMLCSG7O
              IT_ZSMLCSG8E      =       IT_ZSMLCSG8E
              IT_ZSMLCSG9O      =       IT_ZSMLCSG9O
              IT_ZSMLCSG7G_ORG  =       IT_ZSMLCSG7G_ORG
              IT_ZSMLCSG7O_ORG  =       IT_ZSMLCSG7O_ORG
              IT_ZSMLCSG8E_ORG  =       IT_ZSMLCSG8E_ORG
              IT_ZSMLCSG9O_ORG  =       IT_ZSMLCSG9O_ORG
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
          WHERE    ZFREQNO  EQ    W_ZFREQNO
          AND      ZFAMDNO  EQ    '00000'.
*-----------------------------------------------------------------------
* FLAT-FILE RECORD CREATE
*-----------------------------------------------------------------------
   L_TYPE  =  '00'.

*>> FLAT FILE HEADER MAKE.
   PERFORM  P3000_HEADER_MAKE    USING    'APP700'
                                          ZTREQHD-BUKRS
                                          ZTREQHD-ZFOPBN
                                          W_ZFDHENO
                                 CHANGING W_EDI_RECORD.

*>> FLAT FILE BEG MAKE.(전자문서 시작)
   L_TYPE = '01'.
   PERFORM  P3000_BGM_MAKE       USING    '460'
                                          W_ZFDHENO
                                          ZTMLCHD-ZFEDFN
                                          'AB'
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.

*>> FLAT FILE BUS MAKE.(신용장 종류)
   L_TYPE = '02'.
   PERFORM  P3000_BUS_MAKE       USING    ZTMLCHD-ZFLCTY
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.

*>> FLAT FILE INP MAKE.(개설방법)
   L_TYPE = '03'.
   PERFORM  P3000_INP_MAKE       USING    '1'
                                          '5'
                                          ZTMLCHD-ZFOPME
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.

*>> FLAT FILE INP MAKE.(확인지시 문언)
   IF NOT ZTMLCHD-ZFCNIS IS INITIAL.
      PERFORM  P3000_INP_MAKE       USING    '1'
                                             '4'
                                             ZTMLCHD-ZFCNIS
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.
   ENDIF.

*>> FLAT FILE FCA MAKE.(수수료부담자)
   L_TYPE = '04'.
   PERFORM  P3000_FCA_MAKE       USING    ZTMLCHD-ZFCHG
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.

*>> FLAT FILE DTM MAKE.(개설신청일자)
   L_TYPE = '05'.
   PERFORM  P3000_DTM_MAKE       USING    '2AA'
                                          ZTREQST-ZFAPPDT
                                          '101'
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.

*>> FLAT FILE DTM MAKE.(선적서류제시기간)
   IF NOT ZTMLCHD-ZFPFPR IS INITIAL.
      PERFORM  P3000_DTM_MAKE       USING    '272'
                                             ZTMLCHD-ZFPFPR
                                             '804'
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.
   ENDIF.

*>> FLAT FILE TSR MAKE.(운송관련 요구사항(환적,분할 선적.)).
   L_TYPE = '06'.
   PERFORM  P3000_TSR_MAKE       USING    ZTMLCHD-ZFTRMT
                                          ZTMLCHD-ZFPRMT
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.

*>> FLAT FILE PAI MAKE.(신용공여주체).
   IF NOT ZTMLCHD-ZFPAGR IS INITIAL.
      L_TYPE = '07'.
      PERFORM  P3000_PAI_MAKE       USING    ZTMLCHD-ZFPAGR
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.
   ENDIF.

*>> FLAT FILE PAT MAKE.(지급조건).
   IF NOT ZTMLCHD-ZFTRTX1 IS INITIAL.
      L_TYPE = '08'.
      CASE ZTMLCHD-ZFTRMB.
         WHEN '2AO'.          ">화환어음조건(Draft at...)
            L_CODE = '2AB'.
         WHEN '2AM'.          ">혼합어음조건(Mixed Payment)
            L_CODE = '6'.
         WHEN '2AN'.          ">연지급어음조건(Deferred Payment)
            L_CODE = '4'.
         WHEN OTHERS.         ">화환어음조건(Draft at...)
            L_CODE = '2AB'.
      ENDCASE.
      PERFORM  P3000_PAT_MAKE       USING    L_CODE
                                             ZTMLCHD-ZFTRMT
                                             ZTMLCHD-ZFTRTX1
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.
   ENDIF.
   IF NOT ZTMLCHD-ZFTRTX2 IS INITIAL.
      CASE ZTMLCHD-ZFTRMT.
         WHEN '2AO'.          ">화환어음조건(Draft at...)
            L_CODE = '2AB'.
         WHEN '2AM'.          ">혼합어음조건(Mixed Payment)
            L_CODE = '6'.
         WHEN '2AN'.          ">연지급어음조건(Deferred Payment)
            L_CODE = '4'.
         WHEN OTHERS.         ">화환어음조건(Draft at...)
            L_CODE = '2AB'.
      ENDCASE.
      PERFORM  P3000_PAT_MAKE       USING    L_CODE
                                             ZTMLCHD-ZFTRMT
                                             ZTMLCHD-ZFTRTX2
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.
   ENDIF.
   IF NOT ZTMLCHD-ZFTRTX3 IS INITIAL.
      CASE ZTMLCHD-ZFTRMT.
         WHEN '2AO'.          ">화환어음조건(Draft at...)
            L_CODE = '2AB'.
         WHEN '2AM'.          ">혼합어음조건(Mixed Payment)
            L_CODE = '6'.
         WHEN '2AN'.          ">연지급어음조건(Deferred Payment)
            L_CODE = '4'.
         WHEN OTHERS.         ">화환어음조건(Draft at...)
            L_CODE = '2AB'.
      ENDCASE.
      PERFORM  P3000_PAT_MAKE       USING    L_CODE
                                             ZTMLCHD-ZFTRMT
                                             ZTMLCHD-ZFTRTX3
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.
   ENDIF.
   IF NOT ZTMLCHD-ZFTRTX4 IS INITIAL AND
      NOT ( ZTMLCHD-ZFTRMT IS INITIAL OR
            ZTMLCHD-ZFTRMT EQ '2AO' ).
      CASE ZTMLCHD-ZFTRMT.
         WHEN '2AO'.          ">화환어음조건(Draft at...)
            L_CODE = '2AB'.
         WHEN '2AM'.          ">혼합어음조건(Mixed Payment)
            L_CODE = '6'.
         WHEN '2AN'.          ">연지급어음조건(Deferred Payment)
            L_CODE = '4'.
         WHEN OTHERS.         ">화환어음조건(Draft at...)
            L_CODE = '2AB'.
      ENDCASE.
      PERFORM  P3000_PAT_MAKE       USING    L_CODE
                                             ZTMLCHD-ZFTRMT
                                             ZTMLCHD-ZFTRTX4
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.
   ENDIF.

*>> FLAT FILE FTX MAKE.(기타 정보).
   IF NOT ( ZTMLCHD-ZFETC1 IS INITIAL AND
            ZTMLCHD-ZFETC2 IS INITIAL AND
            ZTMLCHD-ZFETC3 IS INITIAL AND
            ZTMLCHD-ZFETC4 IS INITIAL AND
            ZTMLCHD-ZFETC5 IS INITIAL ).
      L_TYPE = '09'.
      PERFORM  P3000_FTX_MAKE       USING    'ACB'
                                             ZTMLCHD-ZFETC1
                                             ZTMLCHD-ZFETC2
                                             ZTMLCHD-ZFETC3
                                             ZTMLCHD-ZFETC4
                                             ZTMLCHD-ZFETC5
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.
   ENDIF.

*>> FLAT FILE FII MAKE.(개설(의뢰)은행).
   L_TYPE = '10'.
   PERFORM  P3000_FII_MAKE       USING    'AW'
                                          ZTMLCHD-ZFOPBNCD
                                          ZTMLCHD-ZFOBNM
                                          ZTMLCHD-ZFOBBR
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.

*>> FLAT FILE FII MAKE.(개설(의뢰)은행 전화번호).
   IF NOT ZTMLCHD-ZFOBPH IS INITIAL.
      L_TYPE = '11'.
      PERFORM  P3000_COM_MAKE       USING    ZTMLCHD-ZFOBPH(14)
                                             'TE'
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.
   ENDIF.

*>> FLAT FILE FII MAKE.(통지은행).
   IF NOT ZTMLCHD-ZFABNM IS INITIAL.
      L_TYPE = '10'.
      PERFORM  P3000_FII_MAKE       USING    '2AA'
                                             SPACE
                                             ZTMLCHD-ZFABNM
                                             ZTMLCHD-ZFABBR
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.
   ENDIF.

*>> FLAT FILE NAD MAKE.(개설의뢰인).
   L_TYPE = '12'.
   PERFORM  P3000_NAD_MAKE       USING    'DF'
                                          ZTMLCSG2-ZFAPPNM
                                          ZTMLCSG2-ZFAPPAD1
                                          ZTMLCSG2-ZFAPPAD2
                                          ZTMLCSG2-ZFAPPAD3
                                          SPACE
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.
*>> FLAT FILE FII MAKE.(개설(의뢰)은행).
   IF NOT ZTMLCSG2-ZFTELNO IS INITIAL.
      L_TYPE = '13'.
      PERFORM  P3000_COM_MAKE        USING     ZTMLCSG2-ZFTELNO(14)
                                              'TE'
                                     CHANGING L_TYPE
                                              W_EDI_RECORD.
   ENDIF.

*>> FLAT FILE NAD MAKE.(수익자).
   L_TYPE = '12'.
   PERFORM  P3000_NAD_MAKE       USING    'DG'
                                          ZTMLCSG2-ZFBENI1
                                          ZTMLCSG2-ZFBENI2
                                          ZTMLCSG2-ZFBENI3
                                          ZTMLCSG2-ZFBENI4
                                          ZTMLCSG2-ZFBENIA
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.

*>> FLAT FILE NAD MAKE.(개설의뢰인 전자서명).
   L_TYPE = '12'.
   PERFORM  P3000_NAD_MAKE       USING    '2AE'
                                          ZTMLCSG2-ZFELENM
                                          ZTMLCSG2-ZFREPRE
                                          ZTMLCSG2-ZFELEID
                                          ZTMLCSG2-ZFELEAD1
                                          ZTMLCSG2-ZFELEAD2
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.
*>> FLAT FILE DTM MAKE.(유효기일)
   L_TYPE = '14'.
   PERFORM  P3000_DTM_MAKE       USING    '123'
                                          ZTMLCHD-ZFEXDT
                                          '101'
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.
*>> FLAT FILE LOC MAKE.(유효장소)
   L_TYPE = '15'.
   PERFORM  P3000_LOC_MAKE       USING    '143'
                                          ZTMLCHD-ZFEXPL
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.
*>> FLAT FILE MOA MAKE.(금액)
   L_TYPE = '16'.
   PERFORM  P3000_MOA_MAKE       USING    '212'
                                          ZTMLCHD-ZFOPAMT
                                          ZTMLCHD-WAERS
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.
*>> FLAT FILE FTX MAKE.(부가금액조건)
   IF NOT ( ZTMLCHD-ZFAAMT1 IS INITIAL AND
            ZTMLCHD-ZFAAMT2 IS INITIAL AND
            ZTMLCHD-ZFAAMT3 IS INITIAL AND
            ZTMLCHD-ZFAAMT4 IS INITIAL ).
      L_TYPE = '17'.
      PERFORM  P3000_FTX_MAKE       USING    'ABT'
                                          ZTMLCHD-ZFAAMT1
                                          ZTMLCHD-ZFAAMT2
                                          ZTMLCHD-ZFAAMT3
                                          ZTMLCHD-ZFAAMT4
                                          SPACE
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.
   ENDIF.

*>> FLAT FILE ALC MAKE.(과부족 허용율 사용여부)
   IF NOT ZTMLCHD-ZFALCQ IS INITIAL.
      L_TYPE = '18'.
      PERFORM  P3000_ALC_MAKE       USING    ZTMLCHD-ZFALCQ
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.
   ENDIF.
*>> FLAT FILE ALC MAKE.(과부족 허용율)
   IF NOT ZTMLCHD-ZFALCP IS INITIAL OR
      NOT ZTMLCHD-ZFALCM IS INITIAL.
      L_TYPE = '19'.
      PERFORM  P3000_PCD_MAKE       USING    '13'
                                             ZTMLCHD-ZFALCP
                                             ZTMLCHD-ZFALCM
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.
   ENDIF.
*>> FLAT FILE LOC MAKE.(선적항)
   L_TYPE = '20'.
   PERFORM  P3000_LOC_MAKE       USING    '149'
                                          ZTMLCHD-ZFSPRT
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.
*>> FLAT FILE LOC MAKE.(도착항)
   L_TYPE = '20'.
   PERFORM  P3000_LOC_MAKE       USING    '148'
                                          ZTMLCHD-ZFAPRT
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.
*>> FLAT FILE DTM MAKE.(최종선적일자)
   IF ZTMLCHD-ZFSHPR1 IS INITIAL AND
      ZTMLCHD-ZFSHPR2 IS INITIAL AND
      ZTMLCHD-ZFSHPR3 IS INITIAL.
      L_TYPE = '21'.
      PERFORM  P3000_DTM_MAKE       USING    '38'
                                             ZTMLCHD-ZFLTSD
                                             '101'
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.
   ELSE.
*>> FLAT FILE FTX MAKE.(선적기간)
      L_TYPE = '22'.
      PERFORM  P3000_FTX_MAKE       USING    '2AF'
                                             ZTMLCHD-ZFSHPR1
                                             ZTMLCHD-ZFSHPR2
                                             ZTMLCHD-ZFSHPR3
                                             SPACE
                                             SPACE
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.
   ENDIF.
*>> FLAT FILE TOD MAKE.(TERMS OF DELIVERY)
   L_TYPE = '23'.
   PERFORM  P3000_TOD_MAKE       USING    ZTMLCHD-INCO1
                                          ZTMLCHD-ZFINCN
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.

*>> FLAT FILE LOC MAKE.(TERMS OF DELIVERY)
   IF NOT ZTMLCHD-ZFINCP IS INITIAL.
      L_TYPE = '24'.
      PERFORM  P3000_LOC_MAKE       USING    '1'
                                             ZTMLCHD-ZFINCP
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.
   ENDIF.

****>>>>>>>>>>> 원산지는 상품명세로 대신함.

*>> FLAT FILE FTX MAKE.(상품명세)
   DESCRIBE TABLE IT_ZSMLCSG7G LINES W_LINE.
   IF W_LINE GT 0.
      L_TYPE = '25'.
*> LOOP START.
      LOOP AT IT_ZSMLCSG7G.
         PERFORM  P2000_SPACE_AND_CHANGE_SYMBOL
                  CHANGING IT_ZSMLCSG7G-ZFDSOG1.

         W_MOD = SY-TABIX MOD 5.
         CASE W_MOD.
            WHEN 1.
               CLEAR:L_ZFDSOG1, L_ZFDSOG2, L_ZFDSOG3,
                     L_ZFDSOG4, L_ZFDSOG5.

               MOVE  IT_ZSMLCSG7G-ZFDSOG1 TO L_ZFDSOG1.
            WHEN 2.
               MOVE  IT_ZSMLCSG7G-ZFDSOG1 TO L_ZFDSOG2.
            WHEN 3.
               MOVE  IT_ZSMLCSG7G-ZFDSOG1 TO L_ZFDSOG3.
            WHEN 4.
               MOVE  IT_ZSMLCSG7G-ZFDSOG1 TO L_ZFDSOG4.
            WHEN 0.
               MOVE  IT_ZSMLCSG7G-ZFDSOG1 TO L_ZFDSOG5.
               PERFORM  P3000_FTX_MAKE       USING    'AAA'
                                                      L_ZFDSOG1
                                                      L_ZFDSOG2
                                                      L_ZFDSOG3
                                                      L_ZFDSOG4
                                                      L_ZFDSOG5
                                             CHANGING L_TYPE
                                                      W_EDI_RECORD.
         ENDCASE.
      ENDLOOP.
      IF W_MOD NE 0.
         PERFORM  P3000_FTX_MAKE       USING    'AAA'
                                                L_ZFDSOG1
                                                L_ZFDSOG2
                                                L_ZFDSOG3
                                                L_ZFDSOG4
                                                L_ZFDSOG5
                                       CHANGING L_TYPE
                                                W_EDI_RECORD.
      ENDIF.
   ENDIF.

*>> FLAT FILE ALI MAKE.(주요 부가조건)
   IF ZTMLCHD-ZFADCD1 EQ 'X'.    ">SHIPMENT BY...
      L_TYPE = '26'.
      PERFORM  P3000_ALI_MAKE       USING    '2AA'
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.
      L_TYPE = '27'.
      PERFORM  P3000_NAD_MAKE       USING    'CA'
                                             ZTMLCHD-ZFCARR
                                             SPACE
                                             SPACE
                                             SPACE
                                             SPACE
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.
   ENDIF.
   IF ZTMLCHD-ZFADCD2 EQ 'X'.    ">ACCEPTANCE COMMISSION & DISCOUNT..
      L_TYPE = '26'.
      PERFORM  P3000_ALI_MAKE       USING    '2AB'
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.
   ENDIF.
   IF ZTMLCHD-ZFADCD3 EQ 'X'.    ">ALL DOCUMENT...
      L_TYPE = '26'.
      PERFORM  P3000_ALI_MAKE       USING    '2AC'
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.
   ENDIF.
   IF ZTMLCHD-ZFADCD4 EQ 'X'.    ">LATE PRESENTATION BL ACCEPTABLE..
      L_TYPE = '26'.
      PERFORM  P3000_ALI_MAKE       USING    '2AD'
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.
   ENDIF.
   IF ZTMLCHD-ZFADCD5 EQ 'X'.    ">OTHERS ADDITIONAL CONDITIONS...
      L_TYPE = '26'.
      PERFORM  P3000_ALI_MAKE       USING    '2AE'
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.
*> LOOP START.
      L_TYPE = '28'.
      W_MOD = 0.
      LOOP AT IT_ZSMLCSG9O.
         PERFORM  P2000_SPACE_AND_CHANGE_SYMBOL
                  CHANGING IT_ZSMLCSG9O-ZFODOC1.

         W_MOD = SY-TABIX MOD 5.
         CASE W_MOD.
            WHEN 1.
               CLEAR:L_ZFDSOG1, L_ZFDSOG2, L_ZFDSOG3,
                     L_ZFDSOG4, L_ZFDSOG5.

               MOVE  IT_ZSMLCSG9O-ZFODOC1 TO L_ZFDSOG1.
            WHEN 2.
               MOVE  IT_ZSMLCSG9O-ZFODOC1 TO L_ZFDSOG2.
            WHEN 3.
               MOVE  IT_ZSMLCSG9O-ZFODOC1 TO L_ZFDSOG3.
            WHEN 4.
               MOVE  IT_ZSMLCSG9O-ZFODOC1 TO L_ZFDSOG4.
            WHEN 0.
               MOVE  IT_ZSMLCSG9O-ZFODOC1 TO L_ZFDSOG5.
               PERFORM  P3000_FTX_MAKE       USING    'ABS'
                                                      L_ZFDSOG1
                                                      L_ZFDSOG2
                                                      L_ZFDSOG3
                                                      L_ZFDSOG4
                                                      L_ZFDSOG5
                                             CHANGING L_TYPE
                                                      W_EDI_RECORD.
         ENDCASE.
      ENDLOOP.
      IF W_MOD NE 0.
         PERFORM  P3000_FTX_MAKE       USING    'ABS'
                                                L_ZFDSOG1
                                                L_ZFDSOG2
                                                L_ZFDSOG3
                                                L_ZFDSOG4
                                                L_ZFDSOG5
                                       CHANGING L_TYPE
                                                W_EDI_RECORD.
      ENDIF.
   ENDIF.

*> 주요구비서류..
   IF ZTMLCSG910-ZFCOMYN EQ 'X'.    ">COMMERCIAL INVOICE.
      L_TYPE = '29'.
      PERFORM  P3000_DOC_MAKE       USING    '380'
                                             ZTMLCSG910-ZFNOCOM
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.

   ENDIF.
   IF ZTMLCSG910-ZFOCEYN EQ 'X'.    ">BILL OF LADING.
      L_TYPE = '29'.
      PERFORM  P3000_DOC_MAKE       USING    '705'
                                             SPACE
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.
      L_TYPE = '31'.
      PERFORM  P3000_ALI_MAKE       USING    ZTMLCSG910-ZFOCEAC
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.
      L_TYPE = '32'.
      PERFORM  P3000_NAD_MAKE       USING    'CN'
                                             ZTMLCSG910-ZFOCEC1
                                             ZTMLCSG910-ZFOCEC2
                                             SPACE
                                             SPACE
                                             SPACE
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.
      L_TYPE = '32'.
      PERFORM  P3000_NAD_MAKE       USING    'NI'
                                             ZTMLCSG910-ZFOCEAN
                                             SPACE
                                             SPACE
                                             SPACE
                                             SPACE
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.
   ENDIF.
   IF ZTMLCSG910-ZFAIRYN EQ 'X'.    ">AWB.
      L_TYPE = '29'.
      PERFORM  P3000_DOC_MAKE       USING    '740'
                                             SPACE
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.
      L_TYPE = '31'.
      PERFORM  P3000_ALI_MAKE       USING    ZTMLCSG910-ZFAIRAC
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.
      L_TYPE = '32'.
      PERFORM  P3000_NAD_MAKE       USING    'CN'
                                             ZTMLCSG910-ZFAIRC1
                                             ZTMLCSG910-ZFAIRC2
                                             SPACE
                                             SPACE
                                             SPACE
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.
      L_TYPE = '32'.
      PERFORM  P3000_NAD_MAKE       USING    'NI'
                                             ZTMLCSG910-ZFAIRAN
                                             SPACE
                                             SPACE
                                             SPACE
                                             SPACE
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.
   ENDIF.
   IF ZTMLCSG910-ZFINYN EQ 'X'.    ">INSURANCE POLICY.
      L_TYPE = '29'.
      PERFORM  P3000_DOC_MAKE       USING    '530'
                                             SPACE
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.
      L_TYPE = '30'.
      PERFORM  P3000_FTX_MAKE       USING    'INS'
                                             ZTMLCSG910-ZFINCO1
                                             ZTMLCSG910-ZFINCO2
                                             SPACE
                                             SPACE
                                             SPACE
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.
   ENDIF.
   IF ZTMLCSG910-ZFPACYN EQ 'X'.    ">PACKING LIST.
      L_TYPE = '29'.
      PERFORM  P3000_DOC_MAKE       USING    '271'
                                             ZTMLCSG910-ZFNOPAC
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.

   ENDIF.
   IF ZTMLCSG910-ZFCEOYN EQ 'X'.    ">CERTIFICATE OF ORGIN..
      L_TYPE = '29'.
      PERFORM  P3000_DOC_MAKE       USING    '861'
                                             SPACE
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.

   ENDIF.
   IF ZTMLCSG910-ZFOTDYN EQ 'X'.    ">OTHER DOCUMENTS REQUIRED.
      L_TYPE = '29'.
      PERFORM  P3000_DOC_MAKE       USING    '2AA'
                                             SPACE
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.
      L_TYPE = '30'.
      W_MOD = 0.
      LOOP AT IT_ZSMLCSG8E.
         PERFORM  P2000_SPACE_AND_CHANGE_SYMBOL
                  CHANGING IT_ZSMLCSG8E-ZFOACD1.

         W_MOD = SY-TABIX MOD 5.
         CASE W_MOD.
            WHEN 1.
               CLEAR:L_ZFDSOG1, L_ZFDSOG2, L_ZFDSOG3,
                     L_ZFDSOG4, L_ZFDSOG5.

               MOVE  IT_ZSMLCSG8E-ZFOACD1 TO L_ZFDSOG1.
            WHEN 2.
               MOVE  IT_ZSMLCSG8E-ZFOACD1 TO L_ZFDSOG2.
            WHEN 3.
               MOVE  IT_ZSMLCSG8E-ZFOACD1 TO L_ZFDSOG3.
            WHEN 4.
               MOVE  IT_ZSMLCSG8E-ZFOACD1 TO L_ZFDSOG4.
            WHEN 0.
               MOVE  IT_ZSMLCSG8E-ZFOACD1 TO L_ZFDSOG5.
               PERFORM  P3000_FTX_MAKE       USING    'ABX'
                                                      L_ZFDSOG1
                                                      L_ZFDSOG2
                                                      L_ZFDSOG3
                                                      L_ZFDSOG4
                                                      L_ZFDSOG5
                                             CHANGING L_TYPE
                                                      W_EDI_RECORD.
         ENDCASE.
      ENDLOOP.
      IF W_MOD NE 0.
         PERFORM  P3000_FTX_MAKE       USING    'ABX'
                                                L_ZFDSOG1
                                                L_ZFDSOG2
                                                L_ZFDSOG3
                                                L_ZFDSOG4
                                                L_ZFDSOG5
                                       CHANGING L_TYPE
                                                W_EDI_RECORD.
      ENDIF.
   ENDIF.

*>----------------------------------------------------------------------
*>> Segment Group 11은 반영하지 않음.(수입승인 관련 데이타).
*>----------------------------------------------------------------------
*>> 수입승인번호 및 수입승인금액.....


*>> 마지막 New Line Char. Cut.
   PERFORM P3000_EDI_RECORD_ADJUST  CHANGING W_EDI_RECORD.

ENDFUNCTION.
