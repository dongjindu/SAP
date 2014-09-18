FUNCTION ZIM_GET_CHARGE_DOCUMENT.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(BUKRS) TYPE  ZTBKPF-BUKRS
*"     VALUE(BELNR) TYPE  ZTBKPF-BELNR
*"     VALUE(GJAHR) TYPE  ZTBKPF-GJAHR
*"  EXPORTING
*"     VALUE(W_ZTBKPF) TYPE  ZTBKPF
*"  TABLES
*"      IT_ZSBSEG STRUCTURE  ZSBSEG
*"      IT_ZSBSEG_OLD STRUCTURE  ZSBSEG OPTIONAL
*"      IT_ZSBDIV STRUCTURE  ZSBDIV OPTIONAL
*"      IT_ZSBHIS STRUCTURE  ZSBHIS OPTIONAL
*"  EXCEPTIONS
*"      NOT_FOUND
*"      COMPANDYCODE_NOT_INPUT
*"      DOCUMENT_NO_NOT_INPUT
*"      FISC_YEAR_NOT_INPUT
*"----------------------------------------------------------------------
  REFRESH : IT_ZSBSEG, IT_ZSBDIV, IT_ZSBSEG_OLD, IT_ZSBHIS.

  CLEAR : W_ZTBKPF.

  IF BUKRS IS INITIAL.   RAISE COMPANDYCODE_NOT_INPUT.   ENDIF.
  IF BELNR IS INITIAL.   RAISE DOCUMENT_NO_NOT_INPUT.    ENDIF.
  IF GJAHR IS INITIAL.   RAISE FISC_YEAR_NOT_INPUT.      ENDIF.

*-----------------------------------------------------------------------
*>> 비용 헤더.
*-----------------------------------------------------------------------
  SELECT SINGLE * INTO W_ZTBKPF
                  FROM ZTBKPF
                  WHERE BUKRS EQ BUKRS
                  AND   BELNR EQ BELNR
                  AND   GJAHR EQ GJAHR.
  IF SY-SUBRC NE 0.    RAISE NOT_FOUND.   ENDIF.

*-----------------------------------------------------------------------
*>> 비용 아이템.
*-----------------------------------------------------------------------
  REFRESH : IT_ZSBSEG.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSBSEG
                FROM ZTBSEG
                WHERE BUKRS EQ BUKRS
                AND   BELNR EQ BELNR
                AND   GJAHR EQ GJAHR.

*-----------------------------------------------------------------------
* 비용 아이템 관련 TEXT GET.
*-----------------------------------------------------------------------
  LOOP AT IT_ZSBSEG.
     W_TABIX = SY-TABIX.
     SELECT SINGLE ZFCDNM INTO IT_ZSBSEG-ZFCDNM FROM ZTIMIMG08
                          WHERE ZFCDTY EQ IT_ZSBSEG-ZFCSTGRP
                          AND   ZFCD   EQ IT_ZSBSEG-ZFCD.
*>> 비용 그룹별 TEXT.
     CASE IT_ZSBSEG-ZFCSTGRP.
        WHEN '003'.             ">수입의뢰비용.
        WHEN '004' OR '005'.    ">B/L 관련비용.
        WHEN '006'.             ">통관관련비용.
        WHEN '007'.             ">하역관련비용.
        WHEN OTHERS.
     ENDCASE.
     MODIFY IT_ZSBSEG INDEX W_TABIX.
  ENDLOOP.
  IT_ZSBSEG_OLD[] = IT_ZSBSEG[].

*-----------------------------------------------------------------------
*>> 비용배부내역.
*-----------------------------------------------------------------------
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSBDIV
                FROM ZTBDIV
                WHERE BUKRS EQ BUKRS
                AND   BELNR EQ BELNR
                AND   GJAHR EQ GJAHR.

*-----------------------------------------------------------------------
*>> 비용전기 이력.
*-----------------------------------------------------------------------
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSBHIS
                FROM ZTBHIS
                WHERE BUKRS EQ BUKRS
                AND   BELNR EQ BELNR
                AND   GJAHR EQ GJAHR.


ENDFUNCTION.
