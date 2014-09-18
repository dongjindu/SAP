FUNCTION ZIM_GET_PAYORD_DOC_DATA.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(ZFCIVRN) LIKE  ZTCIVHD-ZFCIVRN
*"  EXPORTING
*"     VALUE(W_ZTTTHD) LIKE  ZTTTHD STRUCTURE  ZTTTHD
*"  TABLES
*"      IT_ZSTTSG5 STRUCTURE  ZSTTSG5
*"      IT_ZSTTSG5_ORG STRUCTURE  ZSTTSG5
*"  EXCEPTIONS
*"      NOT_FOUND
*"      NOT_INPUT
*"----------------------------------------------------------------------
  REFRESH : IT_ZSTTSG5, IT_ZSTTSG5_ORG.

  CLEAR : W_ZTTTHD.

  IF ZFCIVRN IS INITIAL.
     RAISE NOT_INPUT.
  ENDIF.

  SELECT SINGLE * INTO   W_ZTTTHD   FROM ZTTTHD
                  WHERE  ZFCIVRN    EQ   ZFCIVRN.
*                 AND    ZFAMDNO   EQ   ZFAMDNO.

  IF SY-SUBRC NE 0.
     RAISE NOT_FOUND.
  ENDIF.

  SELECT *  INTO CORRESPONDING FIELDS OF TABLE IT_ZSTTSG5
            FROM ZTTTSG5
            WHERE  ZFCIVRN      EQ   ZFCIVRN
*            AND    ZFAMDNO      EQ   ZFAMDNO
            ORDER BY  ZFLSG5.

  IT_ZSTTSG5_ORG[]  =  IT_ZSTTSG5[].

ENDFUNCTION.
