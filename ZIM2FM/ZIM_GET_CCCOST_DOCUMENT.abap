FUNCTION ZIM_GET_CCCOST_DOCUMENT.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(ZFCCNO) TYPE  ZTCCHD-ZFCCNO
*"  EXPORTING
*"     VALUE(W_ZTCCHD) TYPE  ZTCCHD
*"  TABLES
*"      IT_ZSCCIT STRUCTURE  ZSCCIT
*"      IT_ZSCCIT_OLD STRUCTURE  ZSCCIT OPTIONAL
*"      IT_ZSBDIV STRUCTURE  ZSBDIV OPTIONAL
*"      IT_ZSBHIS STRUCTURE  ZSBHIS OPTIONAL
*"  EXCEPTIONS
*"      NOT_FOUND
*"      COMPANDYCODE_NOT_INPUT
*"      DOCUMENT_NO_NOT_INPUT
*"      FISC_YEAR_NOT_INPUT
*"----------------------------------------------------------------------
  REFRESH : IT_ZSCCIT, IT_ZSBDIV, IT_ZSCCIT_OLD, IT_ZSBHIS.

  CLEAR : W_ZTCCHD.

  IF ZFCCNO IS INITIAL.  RAISE DOCUMENT_NO_NOT_INPUT.   ENDIF.
*  IF BUKRS  IS INITIAL.  RAISE COMPANDYCODE_NOT_INPUT.   ENDIF.
*  IF GJAHR  IS INITIAL.  RAISE FISC_YEAR_NOT_INPUT.      ENDIF.
*-----------------------------------------------------------------------
*>> 관세/부가세 헤더.
*-----------------------------------------------------------------------
  CLEAR ZTCCHD.
  SELECT SINGLE * INTO W_ZTCCHD
                  FROM ZTCCHD
                  WHERE ZFCCNO EQ ZFCCNO.

  IF SY-SUBRC NE 0.    RAISE NOT_FOUND.   ENDIF.
*-----------------------------------------------------------------------
*>> 관세 부가세아이템.
*-----------------------------------------------------------------------
  REFRESH : IT_ZSCCIT.
  CLEAR ZTCCIT.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSCCIT
                FROM ZTCCIT
                WHERE ZFCCNO EQ ZFCCNO.

  LOOP AT IT_ZSCCIT.
     W_TABIX = SY-TABIX.
     SELECT SINGLE *
            FROM ZTCUCLIV
           WHERE ZFIVNO = IT_ZSCCIT-ZFIVNO.
     IF SY-SUBRC EQ 0.
          CLEAR ZTIDS.
          SELECT SINGLE *
            FROM ZTIDS
           WHERE ZFBLNO   EQ ZTCUCLIV-ZFBLNO
             AND ZFCLSEQ  EQ ZTCUCLIV-ZFCLSEQ.
           MOVE: ZTIDS-ZFIDRNO  TO IT_ZSCCIT-ZFIDRNO.
           MODIFY IT_ZSCCIT INDEX W_TABIX.
     ENDIF.
  ENDLOOP.

  IT_ZSCCIT_OLD[] = IT_ZSCCIT[].

*----------------------------------------------------------------------
*>> 비용배부내역.
*----------------------------------------------------------------------
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSBDIV
*                FROM ZTBDIV
*                WHERE ZFCCNO EQ ZFCCNO
*                AND   BELNR EQ BELNR
*                AND   GJAHR EQ GJAHR.

*----------------------------------------------------------------------
**>> 비용전기 이력.
**----------------------------------------------------------------------
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSBHIS
*                FROM ZTBHIS
*                WHERE ZFCCNO EQ ZFCCNO
*                AND   BELNR  EQ BELNR
*                AND   GJAHR  EQ GJAHR.
*
*
ENDFUNCTION.
