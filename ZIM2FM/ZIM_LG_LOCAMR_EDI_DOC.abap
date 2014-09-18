FUNCTION ZIM_LG_LOCAMR_EDI_DOC.
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
       L_ZFAMDNO      LIKE     ZTREQST-ZFAMDNO,
       L_ZFOPNDT      LIKE     ZTREQST-ZFOPNDT,
       L_ZFDSOG1      LIKE     IT_ZSMLCSG7G-ZFDSOG1,
       L_ZFDSOG2      LIKE     IT_ZSMLCSG7G-ZFDSOG1,
       L_ZFDSOG3      LIKE     IT_ZSMLCSG7G-ZFDSOG1,
       L_ZFDSOG4      LIKE     IT_ZSMLCSG7G-ZFDSOG1,
       L_ZFDSOG5      LIKE     IT_ZSMLCSG7G-ZFDSOG1.

*> LOCAL L/C AMEND READ.
   CALL FUNCTION 'ZIM_GET_LOCAL_LC_AMEND_DATA'
        EXPORTING
            ZFREQNO             =       W_ZFREQNO
            ZFAMDNO             =       W_ZFAMDNO
        IMPORTING
            W_ZTLLCAMHD         =       ZTLLCAMHD
        TABLES
            IT_ZSLLCAMSGOF      =       IT_ZSLLCAMSGOF
            IT_ZSLLCAMSGOF_ORG  =       IT_ZSLLCAMSGOF_ORG
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
          AND      ZFAMDNO  EQ   W_ZFAMDNO.

   SELECT SINGLE * FROM ZTLLCHD
          WHERE    ZFREQNO  EQ   W_ZFREQNO.

   SELECT SINGLE * FROM ZTLLCSG23
          WHERE    ZFREQNO EQ    W_ZFREQNO.


*-----------------------------------------------------------------------
* FLAT-FILE RECORD CREATE
*-----------------------------------------------------------------------
   L_TYPE  =  '00'.

*>> FLAT FILE HEADER MAKE.
   PERFORM  P3000_HEADER_MAKE    USING    'LOCAMR'
                                          ZTREQHD-BUKRS
                                          ZTREQHD-ZFOPBN
                                          W_ZFDHENO
                                 CHANGING W_EDI_RECORD.

*>> FLAT FILE BEG MAKE.(전자문서 시작)
   L_TYPE = '01'.
   PERFORM  P3000_BGM_MAKE       USING    '2AE'
                                          W_ZFDHENO
                                          ZTREQST-ZFDOCNO
                                          'AB'
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.

*>> FLAT FILE RFF MAKE.(내국신용장번호)
   L_TYPE = '02'.
   PERFORM  P3000_RFF_MAKE       USING    'LC'
                                          ZTREQST-ZFOPNNO
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.

*>> FLAT FILE RFF MAKE.(물품매도확약서번호)
   LOOP AT IT_ZSLLCAMSGOF FROM 1 TO 9.
      PERFORM  P3000_RFF_MAKE       USING    'AAG'
                                             IT_ZSLLCAMSGOF-ZFSGOF
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.
   ENDLOOP.

*>> FLAT FILE DTM MAKE.(변경신청일)
   L_TYPE = '03'.
   PERFORM  P3000_DTM_MAKE       USING    '2AA'
                                          ZTREQST-ZFAPPDT
                                          '101'
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.

*>> FLAT FILE DTM MAKE.(개설일자)
   L_ZFAMDNO = '00000'.
   SELECT SINGLE ZFOPNDT INTO L_ZFOPNDT
          FROM ZTREQST
          WHERE ZFREQNO EQ W_ZFREQNO
            AND ZFAMDNO EQ L_ZFAMDNO.

   PERFORM  P3000_DTM_MAKE       USING    '182'
                                          L_ZFOPNDT
                                          '101'
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.

*>> FLAT FILE FTX MAKE.(기타 정보).
   IF NOT ( ZTLLCAMHD-ZFETC1 IS INITIAL AND
            ZTLLCAMHD-ZFETC2 IS INITIAL AND
            ZTLLCAMHD-ZFETC3 IS INITIAL AND
            ZTLLCAMHD-ZFETC4 IS INITIAL AND
            ZTLLCAMHD-ZFETC5 IS INITIAL ).
      L_TYPE = '04'.
      PERFORM  P3000_FTX_MAKE       USING    'ACB'
                                             ZTLLCAMHD-ZFETC1
                                             ZTLLCAMHD-ZFETC2
                                             ZTLLCAMHD-ZFETC3
                                             ZTLLCAMHD-ZFETC4
                                             ZTLLCAMHD-ZFETC5
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.
   ENDIF.

*>> FLAT FILE FII MAKE.(개설(의뢰)은행).
   L_TYPE = '05'.
   PERFORM  P3000_FII_MAKE_1       USING    'AZ'
                                          ZTLLCHD-ZFOPBNCD
                                            '25'
                                            'BOK'
                                          ZTLLCHD-ZFOBNM
                                          ZTLLCHD-ZFOBBR
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.

*>> FLAT FILE NAD MAKE.(개설의뢰인).
   L_TYPE = '06'.
   PERFORM  P3000_NAD_AX_MAKE      USING    'DF'
                                           ZTLLCSG23-ZFAPPNM1
                                           ZTLLCSG23-ZFAPPNM2
                                           ZTLLCSG23-ZFAPPNM3
                                  CHANGING L_TYPE
                                           W_EDI_RECORD.
*>> FLAT FILE NAD MAKE.(수혜자)
   PERFORM  P3000_NAD_AX_MAKE      USING    'DG'
                                           ZTLLCAMHD-ZFBENI1
                                           ZTLLCAMHD-ZFBENI2
                                           ZTLLCAMHD-ZFBENI3
                                  CHANGING L_TYPE
                                           W_EDI_RECORD.

*>> FLAT FILE NAD MAKE.(개설의뢰인(전자서명))
   PERFORM  P3000_NAD_AX_MAKE       USING    'AX'
                                            ZTLLCSG23-ZFELENM
                                            ZTLLCSG23-ZFREPRE
                                            ZTLLCSG23-ZFELEID
                                   CHANGING L_TYPE
                                            W_EDI_RECORD.
*>> FLAT FILE RFF MAKE.(조건변경회수).
      L_TYPE = '07'.
      PERFORM  P3000_RFF_MAKE       USING    '2AB'
                                             ZTLLCAMHD-ZFAMDNO
                                    CHANGING L_TYPE
                                             W_EDI_RECORD.

*>> FLAT FILE DTM MAKE.(변경후 물품인도기일)
    IF NOT ZTLLCAMHD-ZFNGDDT IS INITIAL.
       L_TYPE = '08'.
       PERFORM P3000_DTM_MAKE         USING     '2'
                                                ZTLLCAMHD-ZFNGDDT
                                                '101'
                                      CHANGING L_TYPE
                                               W_EDI_RECORD.
     ENDIF.
*>> FLAT FILE DTM MAKE.(변경후 유효기일)
     IF NOT ZTLLCAMHD-ZFNEXDT IS INITIAL.
       L_TYPE = '08'.
       PERFORM P3000_DTM_MAKE         USING     '123'
                                                ZTLLCAMHD-ZFNEXDT
                                                '101'
                                      CHANGING L_TYPE
                                               W_EDI_RECORD.
     ENDIF.
*>> FLAT FILE FTX MAKE.(부가금액조건)
   IF NOT ( ZTLLCAMHD-ZFECON1 IS INITIAL AND
            ZTLLCAMHD-ZFECON2 IS INITIAL AND
            ZTLLCAMHD-ZFECON3 IS INITIAL AND
            ZTLLCAMHD-ZFECON4 IS INITIAL AND
            ZTLLCAMHD-ZFECON5 IS INITIAL ).
      L_TYPE = '09'.
      PERFORM  P3000_FTX_MAKE       USING    'CHG'
                                          ZTLLCAMHD-ZFECON1
                                          ZTLLCAMHD-ZFECON2
                                          ZTLLCAMHD-ZFECON3
                                          ZTLLCAMHD-ZFECON4
                                          ZTLLCAMHD-ZFECON5
                                 CHANGING L_TYPE
                                          W_EDI_RECORD.
   ENDIF.

*>> FLAT FILE BUS MAKE.(신용장종류)
   IF NOT ZTLLCAMHD-ZFNLLCTY IS INITIAL.
     L_TYPE = '10'.
     PERFORM P3000_BUS_MAKE           USING    ZTLLCAMHD-ZFNLLCTY
                                      CHANGING L_TYPE
                                               W_EDI_RECORD.
    ENDIF.

*>> FLAT FILE MOA MAKE.(최종금액)
   L_TYPE = '11'.
   IF NOT ZTLLCAMHD-ZFNLLCTY IS INITIAL.
     IF ZTLLCAMHD-ZFNLLCTY = '2AA' OR ZTLLCAMHD-ZFNLLCTY = '2AB'.
        PERFORM P3000_MOA_MAKE           USING    '2AD'
                                                  ZTREQHD-ZFLASTAM
                                                  ZTREQHD-WAERS
                                         CHANGING L_TYPE
                                                  W_EDI_RECORD.
     ELSE.
        PERFORM P3000_MOA_MAKE           USING    '2AE'
                                                  ZTREQHD-ZFLASTAM
                                                  ZTREQHD-WAERS
                                         CHANGING L_TYPE
                                                  W_EDI_RECORD.
     ENDIF.
   ENDIF.

*>> 마지막 New Line Char. Cut.
   PERFORM P3000_EDI_RECORD_ADJUST  CHANGING W_EDI_RECORD.

ENDFUNCTION.
