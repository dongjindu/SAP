FUNCTION ZIM_LG_APP707_EDI_DOC.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(W_ZFREQNO) LIKE  ZTREQHD-ZFREQNO
*"     VALUE(W_ZFAMDNO) LIKE  ZTREQST-ZFAMDNO
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
       L_ZFAMDNO      LIKE     ZTREQST-ZFAMDNO,
       L_ZFOPNDT      LIKE     ZTREQST-ZFOPNDT.

*> MASTER L/C AMEND READ.
   CALL FUNCTION 'ZIM_GET_MASTER_LC_AMEND_DATA'
        EXPORTING
              ZFREQNO            =       W_ZFREQNO
              ZFAMDNO            =       W_ZFAMDNO
        IMPORTING
              W_ZTMLCAMHD        =       ZTMLCAMHD
        TABLES
              IT_ZSMLCAMNARR     =       IT_ZSMLCAMNARR
              IT_ZSMLCAMNARR_ORG =       IT_ZSMLCAMNARR_ORG
        EXCEPTIONS
              NOT_FOUND     =       4
              NOT_INPUT     =       8.

   CASE SY-SUBRC.
      WHEN 4.
         MESSAGE E018 WITH W_ZFREQNO RAISING  CREATE_ERROR.
      WHEN 8.
         MESSAGE E019 RAISING  CREATE_ERROR.
   ENDCASE.

*>> 수입의뢰 HEADER TABLE.
   SELECT SINGLE * FROM ZTREQHD
          WHERE    ZFREQNO  EQ   W_ZFREQNO.

*>> 수입의뢰 상태 TABLE
   SELECT SINGLE * FROM ZTREQST
          WHERE    ZFREQNO  EQ   W_ZFREQNO
          AND      ZFAMDNO  EQ   W_ZFAMDNO.

*>> LC HEADER TABLE
   SELECT SINGLE * FROM ZTMLCHD
          WHERE    ZFREQNO   EQ   W_ZFREQNO.

*>> [수입] Master L/C Seg 2.
   SELECT SINGLE * FROM ZTMLCSG2
          WHERE    ZFREQNO   EQ   W_ZFREQNO.

*   L_ZFAMDNO = W_ZFAMDNO - 1.
   L_ZFAMDNO = '00000'.   ">개설일자...

*-----------------------------------------------------------------------
* FLAT-FILE RECORD CREATE
*-----------------------------------------------------------------------
   L_TYPE  =  '00'.

*>> FLAT FILE HEADER MAKE.
   PERFORM  P3000_HEADER_MAKE    USING    'APP707'
                                          ZTREQHD-BUKRS
                                          ZTREQHD-ZFOPBN
                                          W_ZFDHENO
                                 CHANGING W_EDI_RECORD.

*>> FLAT FILE BEG MAKE.(전자문서 시작)
   L_TYPE = '01'.
   PERFORM  P3000_BGM_MAKE       USING    '469'
                                          W_ZFDHENO
                                          ZTMLCAMHD-ZFEDFN
                                          'AB'
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.

*>> FLAT FILE INP MAKE(개설방법)
   L_TYPE = '02'.
   PERFORM  P3000_INP_MAKE       USING    '1'
                                          '5'
                                          ZTMLCAMHD-ZFOPME
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.

*>> FLAT FILE RFF MAKE(신용장번호).
   L_TYPE = '03'.
   PERFORM  P3000_RFF_MAKE       USING    '2AD'
                                          ZTREQST-ZFOPNNO
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.

*>> FLAT FILE RFF MAKE(조건변경횟수).
   L_TYPE = '03'.
   PERFORM  P3000_RFF_MAKE       USING    '2AB'
                                          W_ZFAMDNO
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.

*>> FLAT FILE DTM MAKE(조건변경신청일자).
   L_TYPE = '04'.
   PERFORM  P3000_DTM_MAKE       USING    '2AA'
                                          ZTREQST-ZFAPPDT
                                          '101'
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.

*>> FLAT FILE DTM MAKE(개설일자).
   SELECT SINGLE ZFOPNDT INTO  L_ZFOPNDT
   FROM   ZTREQST
   WHERE  ZFREQNO        EQ    W_ZFREQNO
   AND    ZFAMDNO        EQ    L_ZFAMDNO.

   L_TYPE = '04'.
   PERFORM  P3000_DTM_MAKE       USING    '182'
                                          L_ZFOPNDT
                                          '101'
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.

*>> FLAT FILE DTM MAKE(유효기일 변경).
   IF NOT ZTMLCAMHD-ZFNEXDT IS INITIAL.
      L_TYPE = '04'.
      PERFORM  P3000_DTM_MAKE    USING    '123'
                                          ZTMLCAMHD-ZFNEXDT
                                          '101'
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.
   ENDIF.

*>> FLAT FILE BUS MAKE.(최종 선적일 변경).
   IF NOT ZTMLCAMHD-ZFNLTSD IS INITIAL.
      L_TYPE = '04'.
      PERFORM  P3000_DTM_MAKE    USING    '38'
                                          ZTMLCAMHD-ZFNLTSD
                                          '101'
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.
   ENDIF.

*>> FLAT FILE LOC MAKE( 선적항 변경).
   IF NOT ZTMLCAMHD-ZFNSPRT IS INITIAL.
      L_TYPE = '05'.
      PERFORM  P3000_LOC_MAKE    USING    '149'
                                          ZTMLCAMHD-ZFNSPRT
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.
   ENDIF.

*>> FLAT FILE LOC MAKE( 도착항 변경).
   IF NOT ZTMLCAMHD-ZFNAPRT IS INITIAL.
      L_TYPE = '05'.
      PERFORM  P3000_LOC_MAKE    USING    '148'
                                          ZTMLCAMHD-ZFNAPRT
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.
   ENDIF.

*>> FLAT FILE FTX MAKE(기타정보).
   IF NOT ( ZTMLCAMHD-ZFETC1 IS INITIAL AND
            ZTMLCAMHD-ZFETC2 IS INITIAL AND
            ZTMLCAMHD-ZFETC3 IS INITIAL AND
            ZTMLCAMHD-ZFETC4 IS INITIAL AND
            ZTMLCAMHD-ZFETC5 IS INITIAL ).
      L_TYPE = '06'.
      PERFORM  P3000_FTX_MAKE    USING    'ACB'
                                          ZTMLCAMHD-ZFETC1
                                          ZTMLCAMHD-ZFETC2
                                          ZTMLCAMHD-ZFETC3
                                          ZTMLCAMHD-ZFETC4
                                          ZTMLCAMHD-ZFETC5
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.
   ENDIF.

*>> FLAT FILE FTX MAKE( 부가금액 변경 ).
   IF NOT ( ZTMLCAMHD-ZFNAMT1 IS INITIAL AND
            ZTMLCAMHD-ZFNAMT2 IS INITIAL AND
            ZTMLCAMHD-ZFNAMT3 IS INITIAL AND
            ZTMLCAMHD-ZFNAMT4 IS INITIAL ).
      L_TYPE = '06'.
      PERFORM  P3000_FTX_MAKE     USING    'ABT'
                                           ZTMLCAMHD-ZFNAMT1
                                           ZTMLCAMHD-ZFNAMT2
                                           ZTMLCAMHD-ZFNAMT3
                                           ZTMLCAMHD-ZFNAMT4
                                           SPACE
                                  CHANGING L_TYPE
                                           W_EDI_RECORD.
   ENDIF.

*>> FLAT FILE FTX MAKE( 선적기간 변경 ).
   IF NOT ( ZTMLCAMHD-ZFNSHPR1 IS INITIAL AND
            ZTMLCAMHD-ZFNSHPR2 IS INITIAL AND
            ZTMLCAMHD-ZFNSHPR3 IS INITIAL ).
      L_TYPE = '06'.
      PERFORM  P3000_FTX_MAKE     USING    '2AF'
                                           ZTMLCAMHD-ZFNSHPR1
                                           ZTMLCAMHD-ZFNSHPR2
                                           ZTMLCAMHD-ZFNSHPR3
                                           SPACE
                                           SPACE
                                  CHANGING L_TYPE
                                           W_EDI_RECORD.
   ENDIF.

*>> 기타 조건변경사항.
   CLEAR : W_MOD.
   L_TYPE = '06'.
   LOOP AT IT_ZSMLCAMNARR.
      PERFORM  P2000_SPACE_AND_CHANGE_SYMBOL
               CHANGING IT_ZSMLCAMNARR-ZFNARR.

      W_MOD = SY-TABIX MOD 5.
      CASE W_MOD.
         WHEN 1.
            CLEAR:L_ZFDSOG1, L_ZFDSOG2, L_ZFDSOG3,
                  L_ZFDSOG4, L_ZFDSOG5.

            MOVE  IT_ZSMLCAMNARR-ZFNARR(50) TO L_ZFDSOG1.
         WHEN 2.
            MOVE  IT_ZSMLCAMNARR-ZFNARR(50) TO L_ZFDSOG2.
         WHEN 3.
            MOVE  IT_ZSMLCAMNARR-ZFNARR(50) TO L_ZFDSOG3.
         WHEN 4.
            MOVE  IT_ZSMLCAMNARR-ZFNARR(50) TO L_ZFDSOG4.
         WHEN 0.
            MOVE  IT_ZSMLCAMNARR-ZFNARR(50) TO L_ZFDSOG5.
            PERFORM  P3000_FTX_MAKE       USING    '2AD'
                                                   L_ZFDSOG1
                                                   L_ZFDSOG2
                                                   L_ZFDSOG3
                                                   L_ZFDSOG4
                                                   L_ZFDSOG5
                                          CHANGING L_TYPE
                                                   W_EDI_RECORD.
      ENDCASE.
      IF W_MOD NE 0.
         PERFORM  P3000_FTX_MAKE       USING    '2AD'
                                                L_ZFDSOG1
                                                L_ZFDSOG2
                                                L_ZFDSOG3
                                                L_ZFDSOG4
                                                L_ZFDSOG5
                                       CHANGING L_TYPE
                                                W_EDI_RECORD.
      ENDIF.
   ENDLOOP.

*>> FLAT FILE FTX MAKE( 수익자 상호/주소 변경 ).
*   IF NOT ( ZTMLCAMHD-ZFBENI   IS INITIAL AND
*            ZTMLCAMHD-ZFBENI2  IS INITIAL AND
*            ZTMLCAMHD-ZFBENI3  IS INITIAL AND
*            ZTMLCAMHD-ZFBENI4  IS INITIAL AND
*            ZTMLCAMHD-ZFBENIA  IS INITIAL ).
*      L_TYPE = '06'.
*      PERFORM  P3000_FTX_MAKE     USING    '2AF'
*                                           ZTMLCAMHD-ZFBENI
*                                           ZTMLCAMHD-ZFBENI2
*                                           ZTMLCAMHD-ZFBENI3
*                                           ZTMLCAMHD-ZFBENI4
*                                           ZTMLCAMHD-ZFBENIA
*                                  CHANGING L_TYPE
*                                           W_EDI_RECORD.
*   ENDIF.
*
*   LOOP AT IT_ZSMLCSG7G.
*   ENDLOOP.

*>> FLAT FILE FII MAKE.(개설(의뢰)은행).
   L_TYPE = '07'.
   PERFORM  P3000_FII_MAKE       USING    'AW'
                                          ZTMLCHD-ZFOPBNCD
                                          ZTMLCHD-ZFOBNM
                                          ZTMLCHD-ZFOBBR
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.

*>> FLAT FILE FII MAKE.(개설(의뢰)은행 전화번호).
   IF NOT ZTMLCHD-ZFOBPH IS INITIAL.
      L_TYPE = '08'.
      PERFORM  P3000_COM_MAKE       USING    ZTMLCHD-ZFOBPH(14)
                                             'TE'
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.
   ENDIF.

*>> FLAT FILE FII MAKE.(통지은행).
   IF NOT ZTMLCHD-ZFABNM IS INITIAL.
      L_TYPE = '07'.
      PERFORM  P3000_FII_MAKE       USING    '2AA'
                                             SPACE
                                             ZTMLCHD-ZFABNM
                                             ZTMLCHD-ZFABBR
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.
   ENDIF.

*>> FLAT FILE NAD MAKE.(개설의뢰인).
   L_TYPE = '09'.
   PERFORM  P3000_NAD_MAKE       USING    'DF'
                                          ZTMLCSG2-ZFAPPNM
                                          ZTMLCSG2-ZFAPPAD1
                                          ZTMLCSG2-ZFAPPAD2
                                          ZTMLCSG2-ZFAPPAD3
                                          SPACE
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.

*>> FLAT FILE FII MAKE.(개설(의뢰) 전화번호).
   IF NOT ZTMLCSG2-ZFTELNO IS INITIAL.
      L_TYPE = '10'.
      PERFORM  P3000_COM_MAKE        USING     ZTMLCSG2-ZFTELNO(14)
                                              'TE'
                                     CHANGING L_TYPE
                                              W_EDI_RECORD.
   ENDIF.

*>> FLAT FILE NAD MAKE.(수익자).
   L_TYPE = '09'.
   PERFORM  P3000_NAD_MAKE       USING    'DG'
                                          ZTMLCSG2-ZFBENI1
                                          ZTMLCSG2-ZFBENI2
                                          ZTMLCSG2-ZFBENI3
                                          ZTMLCSG2-ZFBENI4
                                          ZTMLCSG2-ZFBENIA
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.

*>> FLAT FILE NAD MAKE.(개설의뢰인 전자서명).
   L_TYPE = '09'.
   PERFORM  P3000_NAD_MAKE       USING    '2AE'
                                          ZTMLCSG2-ZFELENM
                                          ZTMLCSG2-ZFREPRE
                                          ZTMLCSG2-ZFELEID
                                          ZTMLCSG2-ZFELEAD1
                                          ZTMLCSG2-ZFELEAD2
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.

*>> 신용장 금액 변경시.
   IF NOT ZTMLCAMHD-ZFIDCD IS INITIAL.
*>> FLAT FILE MOA MAKE( 증액분 ).
      IF ZTMLCAMHD-ZFIDCD EQ '+'.
         L_TYPE = '11'.
         PERFORM  P3000_MOA_MAKE    USING    '2AA'
                                             ZTMLCAMHD-ZFIDAM
                                             ZTMLCAMHD-WAERS
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.
*>> FLAT FILE MOA MAKE( 감액분 ).
      ELSE.
         L_TYPE = '11'.
         PERFORM  P3000_MOA_MAKE    USING    '2AB'
                                             ZTMLCAMHD-ZFIDAM
                                             ZTMLCAMHD-WAERS
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.
      ENDIF.
*>> FLAT FILE MOA MAKE( 최종변경금액).
      L_TYPE = '11'.
      PERFORM  P3000_MOA_MAKE        USING    '212'
                                              ZTMLCAMHD-ZFNDAMT
                                              ZTMLCAMHD-WAERS
                                     CHANGING L_TYPE
                                              W_EDI_RECORD.
   ENDIF.

*>> 과부족 변경시.
   IF NOT ZTMLCAMHD-ZFALCQ  IS  INITIAL.
*>>  FLAT FILE ALC MAKE( 과부족 허용).
      L_TYPE = '12'.
      PERFORM  P3000_ALC_MAKE        USING    ZTMLCAMHD-ZFALCQ
                                     CHANGING L_TYPE
                                              W_EDI_RECORD.
*>> FLAT FILE PCD MAKE( 과부족율)
      L_TYPE = '13'.
      PERFORM  P3000_PCD_MAKE        USING    '13'
                                              ZTMLCAMHD-ZFALCP
                                              ZTMLCAMHD-ZFALCM
                                     CHANGING L_TYPE
                                              W_EDI_RECORD.
   ENDIF.

*>> 수입승인번호.
   IF NOT ZTMLCAMHD-ZFILNO IS INITIAL.
      L_TYPE = '14'.
      PERFORM  P3000_RFF_MAKE       USING    'IP'
                                             ZTMLCAMHD-ZFILNO
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.
      L_TYPE = '15'.
      PERFORM  P3000_MOA_MAKE       USING    '2AC'
                                             ZTMLCAMHD-ZFILAMT
                                             ZTMLCAMHD-ZFILCUR
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.

   ENDIF.

*>> 마지막 New Line Char. Cut.
   PERFORM P3000_EDI_RECORD_ADJUST  CHANGING W_EDI_RECORD.

ENDFUNCTION.
