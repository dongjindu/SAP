FUNCTION ZIM_GET_PO_NUMBER_EXTERNAL.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(W_WERKS) LIKE  ZTREQHD-ZFWERKS
*"     VALUE(W_EBELN) LIKE  EKKO-EBELN
*"  EXPORTING
*"     VALUE(W_ADD_EBELN)
*"----------------------------------------------------------------------
*DATA : W_ZZBUSTYPE  LIKE  EKKO-ZZBUSTYPE.
DATA : W_ZZBUSTYPE  TYPE C.

*-----------------------------------------------------------------------
*>>> 2000/12/27 KSB 수정작업
   W_ADD_EBELN = W_EBELN.
   EXIT.
*-----------------------------------------------------------------------

*   SELECT SINGLE ZZBUSTYPE INTO W_ZZBUSTYPE FROM EKKO
*                           WHERE EBELN EQ W_EBELN.

   CASE W_WERKS.
      WHEN '1010'.   " 이천
         CONCATENATE 'H' 'I' W_ZZBUSTYPE(1)
                             W_EBELN INTO W_ADD_EBELN.
      WHEN '1020'.   " 청주
         CONCATENATE 'H' 'C' W_ZZBUSTYPE(1)
                             W_EBELN INTO W_ADD_EBELN.
      WHEN '1030'.   " 구미
         CONCATENATE 'H' 'K' W_ZZBUSTYPE(1)
                             W_EBELN INTO W_ADD_EBELN.
      WHEN '1040'.   " 서울
         CONCATENATE 'H' 'S' W_ZZBUSTYPE(1)
                             W_EBELN INTO W_ADD_EBELN.
      WHEN OTHERS.   " 기타
         CONCATENATE 'H' 'C' W_ZZBUSTYPE(1)
                             W_EBELN INTO W_ADD_EBELN.
      ENDCASE.

ENDFUNCTION.
