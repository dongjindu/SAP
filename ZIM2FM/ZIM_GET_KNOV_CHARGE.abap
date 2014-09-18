FUNCTION ZIM_GET_KNOV_CHARGE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(KNUMV) LIKE  EKKO-KNUMV
*"     VALUE(EBELN) LIKE  EKKO-EBELN
*"     VALUE(W_AMOUNT) LIKE  ZTREQHD-ZFLASTAM
*"  EXPORTING
*"     VALUE(W_KBETR1) LIKE  ZTREQHD-ZFPKCHG
*"     VALUE(W_WAERS1) LIKE  KONV-WAERS
*"     VALUE(W_KBETR2) LIKE  ZTREQHD-ZFPKCHG
*"     VALUE(W_WAERS2) LIKE  KONV-WAERS
*"     VALUE(W_PO_AMOUNT) LIKE  ZTREQHD-ZFLASTAM
*"----------------------------------------------------------------------
*-----------------------------------------------------------------------
* Installing Charge
* 2000/06/05  E&Y 유재오 과장 DEFINE
*   desc : Packing Charge와 Handling Charge Field 추가
*-----------------------------------------------------------------------

   CLEAR : W_KBETR1, W_WAERS1, W_KBETR2, W_WAERS2, W_PO_AMOUNT.

   SELECT SINGLE * FROM ZTIMIMG00.
   IF SY-SUBRC NE 0.
      EXIT.
   ENDIF.


   SELECT * INTO  CORRESPONDING FIELDS OF TABLE IT_KONV
            FROM  KONV
            WHERE KNUMV    EQ  KNUMV
            AND   KPOSN    EQ  '000000'
            AND ( KSCHL    EQ  ZTIMIMG00-ZFKSCHL1  " Packing Charge
            OR    KSCHL    EQ  ZTIMIMG00-ZFKSCHL2 ). " Handling Charge

   LOOP AT IT_KONV.
      CASE IT_KONV-KSCHL.
         WHEN ZTIMIMG00-ZFKSCHL1.       " Packing Charge
            W_KBETR1 = W_KBETR1 + IT_KONV-KBETR.
            W_WAERS1 = IT_KONV-WAERS.
         WHEN ZTIMIMG00-ZFKSCHL2.       " Handling Charge
            W_KBETR2 = W_KBETR2 + IT_KONV-KBETR.
            W_WAERS2 = IT_KONV-WAERS.
         WHEN OTHERS.
      ENDCASE.
   ENDLOOP.

   REFRESH : IT_EKPO.
   SELECT * INTO TABLE IT_EKPO FROM EKPO
                 WHERE EBELN EQ EBELN
                 AND   LOEKZ EQ SPACE.        " 삭제지시자

   CLEAR : W_PO_AMOUNT.
   LOOP AT IT_EKPO.
      IF IT_EKPO-PEINH NE 0.
         W_PO_AMOUNT = W_PO_AMOUNT +
                      ( IT_EKPO-MENGE *
                      ( IT_EKPO-NETPR / IT_EKPO-PEINH ) ).
      ENDIF.
   ENDLOOP.

   IF W_PO_AMOUNT NE 0.
      W_KBETR1 = ( W_AMOUNT / W_PO_AMOUNT ) * W_KBETR1.
      W_KBETR2 = ( W_AMOUNT / W_PO_AMOUNT ) * W_KBETR2.
   ENDIF.

ENDFUNCTION.
