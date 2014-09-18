FUNCTION ZIM_SET_MATERIAL_GUBUN.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(ZZBUSTYPE)
*"  EXPORTING
*"     REFERENCE(ZFMATGB) LIKE  ZTREQHD-ZFMATGB
*"----------------------------------------------------------------------

  CASE ZZBUSTYPE.
     WHEN 'B '.   ZFMATGB = '1'.     " 보세공장수입
*     WHEN 'D '.   ZFMATGB = '2'.     " 내자구매 ( NOT USED )
     WHEN 'D '.   CLEAR : ZFMATGB.    " 내자구매 ( NOT USED )
     WHEN 'E '.   ZFMATGB =  1.      " 수출용 원자재
     WHEN 'L '.   ZFMATGB = '2'.     " 로칼
     WHEN 'N '.   ZFMATGB = '3'.     " 내수용 수입
     WHEN 'R '.   ZFMATGB = '3'.     " 연구소용 수입
     WHEN 'F '.   ZFMATGB = '4'.     " 시설재
     WHEN 'C '.   ZFMATGB = '5'.     " 상품
     WHEN 'M '.   ZFMATGB = '5'.     " 중계무역
     WHEN 'X '.   ZFMATGB = '5'.     " 삼각무역
     WHEN OTHERS. CLEAR : ZFMATGB.   " 기타
  ENDCASE.

ENDFUNCTION.
