*&---------------------------------------------------------------------*
*& INCLUDE ZRIMLORCTOP.
*&---------------------------------------------------------------------*
*&  프로그램명 : L/C 개설 실적을 위한 INCUDE                           *
*&      작성자 : 홍재풍 INFOLINK Ltd.                                  *
*&      작성일 : 2000.02.28                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
TABLES : ZTREQHD,
         ZTREQST,
         LFA1,
         T005T.
 DATA :W_AMDNO   LIKE   ZTREQST-ZFAMDNO,
       W_ZFREQNO LIKE   ZTREQST-ZFREQNO,
       W_ZFMATGB LIKE   ZTREQHD-ZFMATGB,
       W_LAND1   LIKE   LFA1-LAND1,
       W_ZFUSAT  LIKE   ZTMLCHD-ZFUSAT,
       W_ZFREQTY LIKE   ZTREQHD-ZFREQTY,
       W_ZFBACD  LIKE   ZTREQHD-ZFBACD,
       W_ZFJEWGB LIKE   ZTREQHD-ZFJEWGB,
       W_OPAMT   LIKE   ZTREQST-ZFUSDAM,
       W_ZFOPBN  LIKE   ZTREQHD-ZFOPBN,
       W_ZFSHCU  LIKE   ZTREQHD-ZFSHCU,
       W_LINES   TYPE   I,
       W_FLAG    TYPE   I   VALUE   1,
       W_FIELD_NM LIKE DD03D-FIELDNAME,
       W_COUNT   TYPE   C,
       W_NAME(25)       TYPE   C,
       W_COUNT1(3)      TYPE   C,
       W_OPAMT1(18)     TYPE   C,
       W_COUNT2(3)      TYPE   C,
       W_OPAMT2(18)     TYPE   C,
       W_COUNT3(3)      TYPE   C,
       W_OPAMT3(18)     TYPE   C,
       W_COUNT4(3)      TYPE   C,
       W_OPAMT4(18)     TYPE   C,
       W_COUNTS(3)      TYPE   C,
       W_OPAMTS(18)     TYPE   C.
