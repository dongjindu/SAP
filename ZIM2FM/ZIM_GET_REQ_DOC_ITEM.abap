FUNCTION ZIM_GET_REQ_DOC_ITEM.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(EBELN) LIKE  ZTREQHD-EBELN
*"     REFERENCE(KNUMV) LIKE  EKKO-KNUMV
*"     VALUE(KPOSN) LIKE  KONV-KPOSN OPTIONAL
*"     VALUE(KSCHL) LIKE  KONV-KSCHL
*"     VALUE(ZFREQNO) LIKE  ZSREQHD-ZFREQNO
*"     VALUE(ZFAMDNO) LIKE  ZTREQST-ZFAMDNO OPTIONAL
*"  EXPORTING
*"     VALUE(W_ITEM_CNT) LIKE  SY-TABIX
*"     VALUE(W_TOT_AMOUNT) LIKE  ZTREQHD-ZFLASTAM
*"  TABLES
*"      IT_ZSREQIT STRUCTURE  ZSREQIT
*"      IT_ZSREQIT_ORG STRUCTURE  ZSREQIT
*"  EXCEPTIONS
*"      NOT_FOUND
*"      NOT_INPUT
*"      NO_REFERENCE
*"      NO_AMOUNT
*"----------------------------------------------------------------------
DATA : WL_DATE    LIKE SY-DATUM.
DATA : IT_ZTREQIT LIKE ZTREQIT    OCCURS 200 WITH HEADER LINE.

*-----------------------------------------------------------------------
* 2000/03/04    강석봉 수정 작?
*  desc : 의뢰 품목의 Amend 연번을 DB에서 삭제됨에 따라?
*         Amend 연번 Logic을 누락 시킴.
*-----------------------------------------------------------------------
  REFRESH : IT_ZSREQIT, IT_ZSREQIT_ORG.
  CLEAR : W_ITEM_CNT, W_TOT_AMOUNT.

  IF ZFREQNO IS INITIAL.   RAISE NOT_INPUT.   ENDIF.

*  IF ZFAMDNO IS INITIAL.
** MAX AMEND 횟수 SELECT
*     SELECT MAX( ZFAMDNO ) INTO  ZFAMDNO
*                           FROM  ZTREQST
*                           WHERE ZFREQNO  EQ   ZFREQNO.
*  ENDIF.

  IF EBELN IS INITIAL.
     SELECT SINGLE * FROM ZTREQHD WHERE ZFREQNO  EQ   ZFREQNO.
     MOVE : ZTREQHD-EBELN   TO   EBELN.
  ENDIF.

  SELECT SINGLE * FROM EKKO
         WHERE EBELN EQ EBELN.

  SELECT EBELN INTO CORRESPONDING FIELDS OF TABLE IT_EBELN
  FROM   ZTREQIT
  WHERE  ZFREQNO    EQ  ZFREQNO
  GROUP BY
         EBELN.

************************************************************************
* 전체 SELECT.. ---> LOOP...
************************************************************************
*  SELECT * FROM  ZTREQIT
*           WHERE ZFREQNO  EQ   ZFREQNO
*           ORDER BY ZFITMNO.
*     CLEAR : IT_ZSREQIT, IT_ZSREQIT_ORG.
  REFRESH : IT_ZTREQIT.

* SELECT * INTO TABLE IT_ZTREQIT
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTREQIT
            FROM ZTREQIT
            WHERE ZFREQNO  EQ   ZFREQNO
            ORDER BY ZFITMNO.

  LOOP AT IT_ZTREQIT.
     CLEAR : IT_ZSREQIT, IT_ZSREQIT_ORG.

*-----------------------------------------------------------------------
* PO 수량 SELECT.
*-----------------------------------------------------------------------
     SELECT SINGLE * FROM EKPO WHERE EBELN  EQ   IT_ZTREQIT-EBELN
                               AND   EBELP  EQ   IT_ZTREQIT-EBELP.

     IF SY-SUBRC NE 0.
        MESSAGE S069 WITH EBELN IT_ZTREQIT-ZFITMNO.
     ELSE.
* 삭제 및 납품완료 상태 검증...
        IF SY-TCODE EQ 'ZIM01' OR SY-TCODE EQ 'ZIM11'.
*           IF NOT ( EKPO-LOEKZ IS INITIAL ) OR    " 삭제지시?
*              NOT ( EKPO-ELIKZ IS INITIAL ).      " 납품완?
*              EKPO-MENGE = 0.
*              MESSAGE S070 WITH IT_ZTREQIT-ZFITMNO.
*           ENDIF.
           IF NOT ( EKPO-LOEKZ IS INITIAL ).      ">삭제지시?
              EKPO-MENGE = 0.
              MESSAGE S069 WITH EBELN IT_ZTREQIT-EBELP.
           ENDIF.
        ELSE.
           IF NOT ( EKPO-LOEKZ IS INITIAL ).      " 삭제지시?
              EKPO-MENGE = 0.
              MESSAGE S069 WITH EBELN IT_ZTREQIT-EBELP.
           ENDIF.
        ENDIF.
     ENDIF.

*-----------------------------------------------------------------------
* 수입의뢰 수량 SELECT
*-----------------------------------------------------------------------
     W_TOT_MENGE = 0.
* 수입의뢰번호 조회 Loop....
     SELECT ZFREQNO ZFITMNO INTO  (W_ZFREQNO, W_ZFITMNO)
                    FROM  ZTREQIT
                    WHERE EBELN = IT_ZTREQIT-EBELN
                    AND   EBELP = IT_ZTREQIT-EBELP
                    GROUP BY
                          ZFREQNO ZFITMNO.
** 수입의뢰의 Max Amend 회차 조회...
*        IF NOT ( W_ZFREQNO IS INITIAL ).
*           SELECT MAX( ZFAMDNO ) INTO  W_ZFAMDNO
*                                 FROM  ZTREQIT
*                                 WHERE ZFREQNO   EQ   W_ZFREQNO
*                                 AND   ZFITMNO   EQ   ZTREQIT-ZFITMNO.
* 수입의뢰당 Item Sum....
           SELECT SUM( MENGE ) INTO  W_MENGE
                               FROM  ZTREQIT
                               WHERE ZFREQNO EQ W_ZFREQNO
                               AND   ZFITMNO EQ W_ZFITMNO.

           W_TOT_MENGE = W_TOT_MENGE + W_MENGE.    " 수입의뢰수량 누적..
*       ENDIF.
     ENDSELECT.
*-----------------------------------------------------------------------
* Delivery Date
*    SELECT EINDT INTO WL_DATE  FROM  EKET UP TO 1 ROWS
*-----------------------------------------------------------------------
     SELECT *  FROM  EKET UP TO 1 ROWS
                                WHERE EBELN EQ IT_ZTREQIT-EBELN
                                AND   EBELP EQ IT_ZTREQIT-EBELP
                                ORDER BY EINDT.
        EXIT.
     ENDSELECT.

     IF EKET-EINDT GT 0.
        CALL FUNCTION 'PERIOD_AND_DATE_CONVERT_OUTPUT'
             EXPORTING
                  INTERNAL_DATE   = EKET-EINDT
                  INTERNAL_PERIOD = EKET-LPEIN
             IMPORTING
                  EXTERNAL_DATE   = IT_ZSREQIT-EEIND
                  EXTERNAL_PERIOD = IT_ZSREQIT-LPEIN.
     ENDIF.

*-----------------------------------------------------------------------
* 수입의뢰 Internal Table Append
*-----------------------------------------------------------------------
     MOVE-CORRESPONDING IT_ZTREQIT  TO IT_ZSREQIT.
     MOVE : EKPO-MENGE   TO  IT_ZSREQIT-ZFPOMENGE,  " 구매오더 수?
            W_TOT_MENGE  TO  IT_ZSREQIT-ZFLCMENGE,  " 수입의뢰 수?
*           EKPO-ZZMOGRU TO  IT_ZSREQIT-ZFIRLW,     " 수입추?
*           EKPO-ZZHSCODE TO IT_ZSREQIT-STAWN,     " H/S CODE
            EKET-EINDT   TO  IT_ZSREQIT-ZFEEIND,    " 납품 일자.
            EKPO-BPUMZ   TO  IT_ZSREQIT-BPUMZ,      ">분모.
            EKPO-BPUMN   TO  IT_ZSREQIT-BPUMN,      ">분자.
            EKPO-LOEKZ   TO  IT_ZSREQIT-LOEKZ,      " 삭제지시?
            EKPO-ELIKZ   TO  IT_ZSREQIT-ELIKZ.      " 납품완?

     IF EKKO-BSTYP EQ 'F'.       ">구매오더.
        MOVE EKPO-MENGE TO IT_ZSREQIT-ZFPOMENGE.  " 구매오더 수?
     ELSEIF EKKO-BSTYP EQ 'L'.   ">납품일정계약.
        MOVE EKPO-KTMNG TO IT_ZSREQIT-ZFPOMENGE.  " 목표수량.
     ELSEIF EKKO-BSTYP EQ 'K'.   ">계약.
        MOVE EKPO-KTMNG TO IT_ZSREQIT-ZFPOMENGE.  " 목표수량.
     ENDIF.

* 수입추천 대상여부.
     SELECT * FROM ZTIMIMG09
                   WHERE STAWN    EQ IT_ZSREQIT-STAWN
                   AND   ZFAPLDT  <= SY-DATUM ORDER BY ZFAPLDT.
          EXIT.
     ENDSELECT.
     MOVE ZTIMIMG09-ZFIRLW    TO    IT_ZSREQIT-ZFIRLW.
     APPEND IT_ZSREQIT.

*-----------------------------------------------------------------------
* 수입의뢰 Amount
*-----------------------------------------------------------------------
      W_TOT_AMOUNT = W_TOT_AMOUNT +
           ( IT_ZSREQIT-MENGE *
           ( IT_ZSREQIT-BPUMZ / IT_ZSREQIT-BPUMN ) *
           ( IT_ZSREQIT-NETPR / IT_ZSREQIT-PEINH ) ).
      W_ITEM_CNT = W_ITEM_CNT + 1.
  ENDLOOP.

*  IF SY-SUBRC NE 0.
*     RAISE NOT_FOUND.
*  ENDIF.
   W_SY_SUBRC = SY-SUBRC.

  LOOP  AT  IT_EBELN.
     REFRESH : IT_ZSREQIT_OLD.
*-----------------------------------------------------------------------
* P/O ITME TABLE SELECT ( EKPO )
*-----------------------------------------------------------------------
     CALL FUNCTION 'ZIM_GET_PO_ITEM'
            EXPORTING
                  EBELN   =  IT_EBELN-EBELN
                  KNUMV   =  KNUMV
                  KSCHL   =  KSCHL
                  LOEKZ   =  SPACE
                  ELIKZ   =  SPACE
*        IMPORTING
*              W_ITEM_CNT      =   W_ITEM_CNT
*              W_TOT_AMOUNT    =   W_TOT_AMOUNT
*              W_ZFMAUD        =   W_ZFMAUD
*              W_ZFWERKS       =   W_ZFWERKS
*              W_BEDNR         =   W_BEDNR
            TABLES
*               IT_ZSREQIT      =   IT_ZSREQIT_ORG
                  IT_ZSREQIT_ORG  =   IT_ZSREQIT_OLD
            EXCEPTIONS
                   KEY_INCOMPLETE =   1
                   NOT_FOUND      =   2
                   NO_REFERENCE   =   3
                   NO_AMOUNT      =   4.

*  CASE SY-SUBRC.
*     WHEN 1.    MESSAGE E003.
*     WHEN 2.    MESSAGE E006     WITH ZSREQHD-EBELN.
*     WHEN 3.    MESSAGE E006     WITH ZSREQHD-EBELN.
*     WHEN 4.    MESSAGE E007     WITH ZSREQHD-EBELN.
*  ENDCASE.

     LOOP AT IT_ZSREQIT_OLD.
        W_TABIX = SY-TABIX.
* >> 수입 의뢰 Item Select
        SELECT SINGLE * FROM ZTREQIT WHERE ZFREQNO EQ ZFREQNO
                              AND   ZFITMNO EQ IT_ZSREQIT_OLD-ZFITMNO.
* >> P/O Item Select
*     SELECT SINGLE * FROM EKPO   WHERE EBELN   EQ EBELN
*                                AND   EBELP  EQ IT_ZSREQIT_ORG-ZFITMNO.
        IF SY-SUBRC EQ 0.
           IT_ZSREQIT_OLD-ZFLCMENGE = IT_ZSREQIT_OLD-ZFLCMENGE
                                    - ZTREQIT-MENGE.
*-----------------------------------------------------------------------
*  2000/05/29 : KSB 수정 작?
*        IT_ZSREQIT_ORG-MENGE     = IT_ZSREQIT_ORG-MENGE
*                                 + ZTREQIT-MENGE.

           IT_ZSREQIT_OLD-MENGE     = IT_ZSREQIT_OLD-ZFPOMENGE
                                    - IT_ZSREQIT_OLD-ZFLCMENGE.
*-----------------------------------------------------------------------
           MODIFY IT_ZSREQIT_OLD INDEX W_TABIX.
        ENDIF.
*    IT_ZSREQIT_ORG-STAWN     = ZTREQIT-STAWN.    " H/S CODE
*    IT_ZSREQIT_ORG-ZFIRLW    = EKPO-ZZMOGRU.     " 수입추?
     ENDLOOP.

     LOOP  AT  IT_ZSREQIT_OLD.
        MOVE-CORRESPONDING  IT_ZSREQIT_OLD  TO  IT_ZSREQIT_ORG.
        APPEND  IT_ZSREQIT_ORG.
     ENDLOOP.
  ENDLOOP.

  IF W_SY_SUBRC NE 0.
     RAISE NOT_FOUND.
  ENDIF.

ENDFUNCTION.
