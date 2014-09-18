FUNCTION ZIM_GET_CHARGE_DOCUMENT_CR.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(BUKRS) TYPE  ZTBKPF-BUKRS
*"     VALUE(BELNR) TYPE  ZTBKPF-BELNR
*"     VALUE(GJAHR) TYPE  ZTBKPF-GJAHR
*"  EXPORTING
*"     VALUE(W_ZTBKPF) TYPE  ZTBKPF
*"  TABLES
*"      IT_ZSBSEG STRUCTURE  ZSBSEG OPTIONAL
*"      IT_ZSBSEG_OLD STRUCTURE  ZSBSEG OPTIONAL
*"      IT_ZSBSEG1 STRUCTURE  ZTBSEG1 OPTIONAL
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
*>> Charge Document Header.
*-----------------------------------------------------------------------
  SELECT SINGLE * INTO W_ZTBKPF
                  FROM ZTBKPF
                  WHERE BUKRS EQ BUKRS
                  AND   BELNR EQ BELNR
                  AND   GJAHR EQ GJAHR.
  IF SY-SUBRC NE 0.    RAISE NOT_FOUND.   ENDIF.

*-----------------------------------------------------------------------
*>> Cost Item
*-----------------------------------------------------------------------
  REFRESH : IT_ZSBSEG.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSBSEG
                FROM ZTBSEG
                WHERE BUKRS EQ BUKRS
                AND   BELNR EQ BELNR
                AND   GJAHR EQ GJAHR.
*-----------------------------------------------------------------------
* Cost Item Text Set
*-----------------------------------------------------------------------
  LOOP AT IT_ZSBSEG.
     W_TABIX = SY-TABIX.
     SELECT SINGLE ZFCDNM INTO IT_ZSBSEG-ZFCDNM FROM ZTIMIMG08
                          WHERE ZFCDTY EQ IT_ZSBSEG-ZFCSTGRP
                          AND   ZFCD   EQ IT_ZSBSEG-ZFCD.
     " Expense Group Text
     CASE IT_ZSBSEG-ZFCSTGRP.
        WHEN '003'.             ">Import Request Cost
        WHEN '004' OR '005'.    ">B/L Cost
        WHEN '006'.             ">Customs Clearance Cost
        WHEN '007'.             ">Unlading Cost
        WHEN OTHERS.
     ENDCASE.
     MODIFY IT_ZSBSEG INDEX W_TABIX.
  ENDLOOP.
  IT_ZSBSEG_OLD[] = IT_ZSBSEG[].

*-----------------------------------------------------------------------
*>> Cost Item1
*-----------------------------------------------------------------------
  REFRESH : IT_ZSBSEG1.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSBSEG1
                FROM ZTBSEG1
                WHERE BUKRS EQ BUKRS
                AND   BELNR EQ BELNR
                AND   GJAHR EQ GJAHR.

*-----------------------------------------------------------------------
*>> Cost Distribution
*-----------------------------------------------------------------------
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSBDIV
                FROM ZTBDIV
                WHERE BUKRS EQ BUKRS
                AND   BELNR EQ BELNR
                AND   GJAHR EQ GJAHR.

*-----------------------------------------------------------------------
*>> Cost History
*-----------------------------------------------------------------------
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSBHIS
                FROM ZTBHIS
                WHERE BUKRS EQ BUKRS
                AND   BELNR EQ BELNR
                AND   GJAHR EQ GJAHR.

ENDFUNCTION.
