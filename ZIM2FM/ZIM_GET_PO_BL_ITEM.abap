FUNCTION ZIM_GET_PO_BL_ITEM.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(EBELN) LIKE  EKKO-EBELN
*"     VALUE(LOEKZ) LIKE  EKPO-LOEKZ DEFAULT ' '
*"     VALUE(ELIKZ) LIKE  EKPO-ELIKZ DEFAULT ' '
*"  EXPORTING
*"     VALUE(W_ITEM_CNT) LIKE  SY-TABIX
*"     VALUE(W_TOT_AMOUNT) LIKE  ZTBL-ZFBLAMT
*"     VALUE(W_ZFMAUD) LIKE  SY-DATUM
*"     VALUE(W_ZFWERKS) LIKE  ZTREQHD-ZFWERKS
*"     VALUE(W_BEDNR) LIKE  EKPO-BEDNR
*"     VALUE(W_MATNR) LIKE  EKPO-MATNR
*"     VALUE(W_TXZ01) LIKE  EKPO-TXZ01
*"  TABLES
*"      IT_ZSBLIT STRUCTURE  ZSBLIT OPTIONAL
*"      IT_ZSBLIT_ORG STRUCTURE  ZSBLIT OPTIONAL
*"  EXCEPTIONS
*"      KEY_INCOMPLETE
*"      NOT_FOUND
*"      NO_REFERENCE
*"      NO_AMOUNT
*"----------------------------------------------------------------------
DATA : WL_DATE      LIKE  SY-DATUM.
DATA : W_TOT_MENGE  LIKE  ZTBLIT-BLMENGE.
DATA : W_MENGE      LIKE  ZTBLIT-BLMENGE.
DATA : W_ZFBLNO     LIKE  ZTBL-ZFBLNO.
DATA : W_ZFSHNO     LIKE  ZTBL-ZFSHNO.

  REFRESH : IT_ZSBLIT, IT_ZSBLIT_ORG.
  CLEAR : W_ITEM_CNT, W_TOT_AMOUNT, W_ZFMAUD, W_ZFWERKS, W_TOT_MENGE,
          W_MATNR, W_TXZ01.

  IF EBELN IS INITIAL.
     RAISE KEY_INCOMPLETE.
  ENDIF.

*>> P/O Header SELECT(2001.06.12).
  SELECT SINGLE * FROM EKKO  WHERE  EBELN EQ EBELN.

* P/O ITEM SELECT
  SELECT * INTO TABLE IT_EKPO FROM EKPO
                              WHERE EBELN EQ EBELN
                              AND   LOEKZ EQ LOEKZ
                              AND   PSTYP NE '6'.

  IF SY-SUBRC NE 0.
     RAISE NOT_FOUND.
  ENDIF.

  SORT IT_EKPO BY EBELP.

*-----------------------------------------------------------------------
* P/O ITEM LOOP
*-----------------------------------------------------------------------
  LOOP AT IT_EKPO.
*-----------------------------------------------------------------------
* Delivery Date
     SELECT * FROM  EKET UP TO 1 ROWS
                                WHERE EBELN EQ EBELN
                                AND   EBELP EQ IT_EKPO-EBELP
                                ORDER BY EINDT.
        EXIT.
     ENDSELECT.
     WL_DATE   = EKET-EINDT.

     CLEAR IT_ZSBLIT.
* data Move
     MOVE-CORRESPONDING IT_EKPO TO IT_ZSBLIT.
*     MOVE : IT_EKPO-EBELP TO IT_ZSBLIT-ZFBLIT.    " 일련번호

     IF EKKO-BSTYP EQ 'F'.       ">구매오더.
        MOVE IT_EKPO-MENGE TO IT_ZSBLIT-MENGE_PO.     " 구매오더 수량
     ELSEIF EKKO-BSTYP EQ 'L'.   ">납품일정계약.
        MOVE IT_EKPO-KTMNG TO IT_ZSBLIT-MENGE_PO.     " 목표수량.
     ELSEIF EKKO-BSTYP EQ 'K'.   ">일괄계약.
        MOVE IT_EKPO-KTMNG TO IT_ZSBLIT-MENGE_PO.     " 목표수량.
     ENDIF.

*-----------------------------------------------------------------------
* 수입의뢰 번호 조회 Loop
*-----------------------------------------------------------------------
*     SELECT ZFBLNO    INTO  W_ZFBLNO
*                      FROM  ZTBLIT
*                      WHERE EBELN EQ EBELN
*                      GROUP BY ZFBLNO.

*-----------------------------------------------------------------------
* 수입의뢰 품목별 Summary
*-----------------------------------------------------------------------
*            W_MENGE = 0.
            SELECT SUM( BLMENGE ) INTO W_TOT_MENGE
                                FROM ZTBLIT
                                WHERE EBELN  EQ IT_EKPO-EBELN
                                AND   EBELP  EQ IT_EKPO-EBELP.

            W_TOT_MENGE = W_TOT_MENGE + W_MENGE.    " BL Item 누적
*       ENDSELECT.
       IT_ZSBLIT-BLMENGE = IT_ZSBLIT-MENGE_PO - W_TOT_MENGE.
       W_TOT_MENGE = 0.
       IF IT_ZSBLIT-BLMENGE <= 0.
          IT_ZSBLIT-BLMENGE = 0.
       ENDIF.
* 대표 플랜트, 제품명, 제품코드, 납기일자
      IF W_ZFWERKS IS INITIAL  AND W_MATNR IS INITIAL AND
         W_TXZ01   IS INITIAL  AND W_BEDNR IS INITIAL.
        IF IT_ZSBLIT-BLMENGE GT 0.
           W_MATNR   = IT_EKPO-MATNR.
           W_TXZ01   = IT_EKPO-TXZ01.
           W_ZFWERKS = IT_EKPO-WERKS.
           IF W_ZFMAUD IS INITIAL.   W_ZFMAUD = WL_DATE.   ENDIF.
* 최초 납기일
           IF WL_DATE LT W_ZFMAUD.
              W_ZFMAUD = WL_DATE.
           ENDIF.
        ENDIF.
     ENDIF.
*-----------------------------------------------------------------------
* B/L Amount
     IF IT_ZSBLIT-PEINH NE 0.
       W_TOT_AMOUNT = W_TOT_AMOUNT +
           ( IT_ZSBLIT-BLMENGE * ( IT_EKPO-BPUMZ / IT_EKPO-BPUMN )
         * ( IT_ZSBLIT-NETPR / IT_ZSBLIT-PEINH ) ).
     ENDIF.

*>> P/O 의 외자 조건에서 H/S code 가져오기.
     SELECT SINGLE STAWN
       INTO IT_ZSBLIT-STAWN
       FROM EIPO
      WHERE EXNUM  EQ  EKKO-EXNUM
        AND EXPOS  EQ  IT_EKPO-EBELP.

*>> P/O에 없으면, 자재마스터에서 H/S code 가져오기.
     IF IT_ZSBLIT-STAWN IS INITIAL.
         SELECT SINGLE STAWN
           INTO IT_ZSBLIT-STAWN
           FROM MARC
          WHERE MATNR EQ IT_EKPO-EMATN
            AND WERKS EQ IT_EKPO-WERKS.
     ENDIF.
*>> 선적차수 GET.
    SELECT MAX( ZFSHNO ) INTO W_ZFSHNO
           FROM  ZTBLIT
           WHERE EBELN EQ IT_ZSBLIT-EBELN
           AND   EBELP EQ IT_ZSBLIT-EBELP.
    IF W_ZFSHNO IS INITIAL.
      IT_ZSBLIT-ZFSHNO = '01'.
    ELSE.
      IT_ZSBLIT-ZFSHNO = W_ZFSHNO + 1.
    ENDIF.

*-----------------------------------------------------------------------
* INTERNAL TABLE APPEND
*-----------------------------------------------------------------------
       APPEND IT_ZSBLIT.
       IT_ZSBLIT_ORG = IT_ZSBLIT.
       W_ITEM_CNT = W_ITEM_CNT + 1.
  ENDLOOP.

  IT_ZSBLIT_ORG[]  =   IT_ZSBLIT[].

ENDFUNCTION.
