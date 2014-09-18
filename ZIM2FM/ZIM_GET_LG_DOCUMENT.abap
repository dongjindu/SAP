FUNCTION ZIM_GET_LG_DOCUMENT.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(ZFBLNO) LIKE  ZTBL-ZFBLNO
*"     VALUE(ZFLGSEQ) LIKE  ZTLG-ZFLGSEQ
*"  EXPORTING
*"     VALUE(W_ZTLG) LIKE  ZTLG STRUCTURE  ZTLG
*"  TABLES
*"      IT_ZSLGGOD STRUCTURE  ZSLGGOD
*"      IT_ZSLGGOD_ORG STRUCTURE  ZSLGGOD
*"  EXCEPTIONS
*"      NOT_FOUND
*"      NOT_INPUT
*"----------------------------------------------------------------------
REFRESH : IT_ZSLGGOD, IT_ZSLGGOD_ORG.
CLEAR : W_ZTLG.

  IF ZFBLNO IS INITIAL.   RAISE NOT_INPUT.   ENDIF.

  SELECT SINGLE * INTO W_ZTLG FROM ZTLG
                              WHERE ZFBLNO  EQ ZFBLNO
                              AND   ZFLGSEQ EQ ZFLGSEQ.
  IF SY-SUBRC NE 0.    RAISE NOT_FOUND.   ENDIF.

* L/G ªÛ«∞
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSLGGOD
           FROM ZTLGGOD
           WHERE ZFBLNO  EQ ZFBLNO
           AND   ZFLGSEQ EQ ZFLGSEQ
           ORDER BY ZFLGOD.

ENDFUNCTION.
