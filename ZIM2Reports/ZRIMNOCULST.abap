*&---------------------------------------------------------------------*
*& Report  ZRIMNOCULST                                                 *
*&---------------------------------------------------------------------*
*&  프로그램명 : 플랜트별 자재의 미통관 LIST                           *
*&      작성자 : 나현주 INFOLINK Ltd.                                  *
*&      작성일 : 2001.04.04                                            *
*&---------------------------------------------------------------------*
*&   DESC. : 1. 자재의 P/O ITEM 별로 진행별 수량 SUM을 구한다.
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&  2001.09.07 NHJ DISPLAY 내역 변경.
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMNOCULST  MESSAGE-ID ZIM
                     LINE-SIZE 90
                     NO STANDARD PAGE HEADING.

*-----------------------------------------------------------------------
* Tables 및 변수 Define
*-----------------------------------------------------------------------
INCLUDE : <ICON>,
          <SYMBOL>,
          ZRIMMATQTYTOP.

*-----------------------------------------------------------------------
* 검색조건 Selection Window.
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:
               S_MATNR   FOR EKPO-MATNR,       " 자재 No.
               S_LIFNR   FOR ZTREQHD-LIFNR,    " Vendor.
               S_EKORG   FOR ZTREQST-EKORG,    " Purch. Org.
               S_EKGRP   FOR ZTREQST-EKGRP,    " Purch. Grp.
               S_WERKS   FOR EKPO-WERKS,       " Plant
               S_LGORT   FOR ZTIVIT-LGORT.     " 저장위치.

SELECTION-SCREEN END OF BLOCK B2.

* Title Text Write
TOP-OF-PAGE.
  CLEAR  WRITE_CHK.
  IF INCLUDE NE 'POPU'.
    PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...
  ENDIF.
*-----------------------------------------------------------------------
* INITIALIZATION
*-----------------------------------------------------------------------
INITIALIZATION.
  SET TITLEBAR 'ZIMR47'.

*-----------------------------------------------------------------------
* START OF SELECT.
*-----------------------------------------------------------------------
START-OF-SELECTION.

* B/L Table Select..
  PERFORM P1000_READ_BL_DATA.

* 하역 수량.
  PERFORM P1000_READ_CG_DATA.

* 수입면허 Table Select..
  PERFORM P1000_READ_ZTIDS_DATA.

*-----------------------------------------------------------------------
*
*-----------------------------------------------------------------------
END-OF-SELECTION.

  CHECK W_ERR_CHK NE 'Y'.

* Title Text Write.
  SET TITLEBAR  'ZIMR47'.
  SET PF-STATUS 'ZIMR47'.

* DATA WRITE
  PERFORM P2000_SORT_DATA.
  PERFORM P2000_WRITE_IT.
  PERFORM P2000_WRITE_DATA.

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.

  CASE SY-UCOMM.
*------- Abbrechen (CNCL) ----------------------------------------------
    WHEN 'CNCL'.
      SET SCREEN 0.    LEAVE SCREEN.
*------- Suchen (SUCH) -------------------------------------------------
    WHEN 'SUCH'.
*------- Sortieren nach Feldbezeichnung (SORB) -------------------------
    WHEN 'SORB'.
*------- Sortieren nach Feldname (SORF) --------------------------------
    WHEN 'SORF'.
*------- Techn. Name ein/aus (TECH) ------------------------------------
    WHEN 'TECH'.
*------- Weiter suchen (WESU) ------------------------------------------
    WHEN 'WESU'.
    WHEN OTHERS.
  ENDCASE.

**----------------------------------------------------------------------
** LINE 선택시 진행현황 DISPLAY
**----------------------------------------------------------------------
*AT LINE-SELECTION.
**>> 자재별 진행관리 DISPLAY 화면 OPEN.
*  IF INCLUDE NE 'POPU'.
*     MOVE IT_TAB-EBELN  TO  W_EBELN.
*     MOVE IT_TAB-EBELP  TO  W_EBELP.
*     INCLUDE = 'POPU'.
*     CALL SCREEN 0100 STARTING AT  10   3
*                      ENDING   AT  95   20.
*     CLEAR : INCLUDE.
**>> 각 문서의 상세화면 DISPLAY.
*   ELSE.
*     DATA : L_TEXT(30).
*
*     GET CURSOR FIELD L_TEXT.
*     CASE  L_TEXT.
*        WHEN 'IT_PO-EBELN'   OR  'IT_PO-EBELP'.
*             SET PARAMETER ID 'BES'  FIELD IT_PO-EBELN.
*             CALL TRANSACTION 'ME23' AND SKIP FIRST SCREEN.
*        WHEN 'IT_RN-ZFREQNO' OR  'IT_RN-ZFITMNO'.
*             SET PARAMETER ID 'ZPREQNO'  FIELD  IT_RN-ZFREQNO.
*             CALL TRANSACTION 'ZIM03' AND SKIP FIRST SCREEN.
*        WHEN 'IT_BL-ZFBLNO'  OR  'IT_BL-ZFBLIT'.
*             SET PARAMETER ID 'ZPBLNO'   FIELD  IT_BL-ZFBLNO.
*             CALL TRANSACTION 'ZIM23' AND SKIP FIRST SCREEN.
*        WHEN 'IT_CG-ZFCGNO'  OR  'IT_CG-ZFCGIT'.
*             SET PARAMETER ID 'ZPBLNO'   FIELD  IT_CG-ZFBLNO.
*             SET PARAMETER ID 'ZPCGPT'   FIELD  IT_CG-ZFCGPT.
*             CALL TRANSACTION 'ZIM83' AND SKIP FIRST SCREEN.
*        WHEN 'IT_IV-ZFIVNO'  OR  'IT_IV-ZFIVDNO'.
*             SET PARAMETER ID 'ZPIVNO'   FIELD  IT_IV-ZFIVNO.
*             CALL TRANSACTION 'ZIM33' AND SKIP FIRST SCREEN.
*        WHEN 'IT_IDS-ZFIDRNO'.
*             SET PARAMETER ID 'ZPBLNO'   FIELD  IT_IDS-ZFBLNO.
*             SET PARAMETER ID 'ZPCLSEQ'  FIELD  IT_IDS-ZFCLSEQ.
*             CALL TRANSACTION 'ZIM76' AND SKIP FIRST SCREEN.
*        WHEN 'IT_IV-MJAHR'    OR   'IT_IV-MBLNR'.
*             SET  PARAMETER ID  'MBN'   FIELD   IT_IV-MBLNR.
*             SET  PARAMETER ID  'MJA'   FIELD   IT_IV-MJAHR.
*             CALL TRANSACTION 'MB03' AND SKIP  FIRST SCREEN.
*     ENDCASE.
*
*   ENDIF.
*
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_BL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_BL_DATA.

  SELECT *
   INTO  CORRESPONDING FIELDS OF TABLE IT_BL
   FROM  ( ZTBL AS H INNER  JOIN ZTBLIT AS I
   ON    H~ZFBLNO     EQ  I~ZFBLNO )
   INNER JOIN EKKO    AS  K
   ON    I~EBELN      EQ  K~EBELN
   WHERE I~WERKS      IN  S_WERKS
   AND   I~LGORT      IN  S_LGORT
   AND   I~MATNR      IN  S_MATNR
   AND   K~LIFNR      IN  S_LIFNR
   AND   K~EKORG      IN  S_EKORG
   AND   K~EKGRP      IN  S_EKGRP.

ENDFORM.                    " P1000_READ_BL_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_ZTIDS_DATA
*&---------------------------------------------------------------------*
*       수입면허 테이블 조회를 위한 Selection 문장..
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_ZTIDS_DATA.

  SELECT *
  INTO   CORRESPONDING FIELDS OF TABLE IT_IDS
  FROM   ZTIDSHSD AS H INNER JOIN ZTIVIT AS I
  ON     H~ZFIVNO      EQ    I~ZFIVNO
  AND    H~ZFIVDNO     EQ    I~ZFIVDNO
  FOR    ALL  ENTRIES  IN    IT_BL
  WHERE  I~ZFBLNO      EQ    IT_BL-ZFBLNO
  AND    I~ZFBLIT      EQ    IT_BL-ZFBLIT.

ENDFORM.                    " P1000_READ_ZFIDS_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_CG_DATA
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_CG_DATA.

  SELECT *
  INTO   CORRESPONDING FIELDS OF TABLE IT_CG
  FROM   ZTCGIT  AS  H
  FOR    ALL ENTRIES  IN  IT_BL
  WHERE  H~ZFBLNO     EQ  IT_BL-ZFBLNO
  AND    H~ZFBLIT     EQ  IT_BL-ZFBLIT.

  LOOP  AT IT_CG.
    W_TABIX  =  SY-TABIX.
    READ TABLE IT_BL WITH KEY ZFBLNO  =  IT_CG-ZFBLNO
                              ZFBLIT  =  IT_CG-ZFBLIT.

    MOVE : IT_BL-LGORT     TO  IT_CG-LGORT.
    MODIFY  IT_CG  INDEX  W_TABIX.
  ENDLOOP.

ENDFORM.                    " P1000_READ_CG_DATA
*&---------------------------------------------------------------------*
*&      Form  P2000_WRITE_DATA
*&---------------------------------------------------------------------*
*       DATA WRITE
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_WRITE_DATA.
  CLEAR : SUM_BL_MENGE,  SUM_CG_MENGE,
          SUM_IDS_MENGE, SUM_NO_MENGE,
          TOT_BL_MENGE,  TOT_CG_MENGE,
          TOT_IDS_MENGE, TOT_NO_MENGE.

  LOOP  AT  IT_TAB.

    W_COUNT = W_COUNT + 1.
    W_MOD   = W_COUNT MOD 2.

    IF SY-TABIX = 1.
*>> PLANT, 저장위치 WRITE.
      WRITE_CHK = '1'.
      MOVE  : IT_TAB-WERKS  TO  SV_WERKS,
              IT_TAB-LGORT  TO  SV_LGORT.

      PERFORM P3000_NAME_GET USING     SV_WERKS
                                       SV_LGORT
                             CHANGING  SV_WERKS_NM
                                       SV_LGORT_NM.

      PERFORM  P3000_TITLE_WRITE.
*>> 자재내역 WRITE.
      WRITE_CHK = '2'.
      MOVE  IT_TAB-MATNR  TO  SV_MATNR.
      MOVE  IT_TAB-MEINS  TO  SV_MEINS.
      MOVE  IT_TAB-MATNM  TO  SV_MATNM.
      PERFORM  P3000_TITLE_WRITE.
      CLEAR WRITE_CHK.
    ENDIF.
*>> PLANT OR 저장위치 가 틀리면 TOTAL WRITE.
    IF IT_TAB-WERKS  NE  SV_WERKS  OR
       IT_TAB-LGORT  NE  SV_LGORT.
*>> TOTAL WRITE
      PERFORM P3000_SUM_WRITE.
      PERFORM P3000_LAST_WRITE.
*>> PLANT, 저장위치 내역 WRITE
      WRITE_CHK = '1'.
      MOVE  : IT_TAB-WERKS  TO  SV_WERKS,
              IT_TAB-LGORT  TO  SV_LGORT.
      PERFORM P3000_NAME_GET USING     SV_WERKS
                                       SV_LGORT
                             CHANGING  SV_WERKS_NM
                                       SV_LGORT_NM.

      PERFORM  P3000_TITLE_WRITE.
*>> 자재내역 WRITE.
      WRITE_CHK = '2'.
      MOVE : IT_TAB-MATNR  TO  SV_MATNR,
             IT_TAB-MEINS  TO  SV_MEINS,
             IT_TAB-MATNM  TO  SV_MATNM.
      PERFORM  P3000_TITLE_WRITE.
      MOVE : IT_TAB-WERKS  TO  SV_WERKS,
             IT_TAB-LGORT  TO  SV_LGORT,
             IT_TAB-MATNR  TO  SV_MATNR,
             IT_TAB-MEINS  TO  SV_MEINS,
             IT_TAB-MATNM  TO  SV_MATNM.
      CLEAR : TOT_BL_MENGE, TOT_CG_MENGE,
              TOT_NO_MENGE, TOT_IDS_MENGE,
              SUM_BL_MENGE, SUM_CG_MENGE,
              SUM_NO_MENGE, SUM_IDS_MENGE, WRITE_CHK.
    ELSE.
      IF IT_TAB-MATNR NE SV_MATNR.
        PERFORM P3000_SUM_WRITE.

        WRITE_CHK = '2'.
        MOVE : IT_TAB-MATNR  TO  SV_MATNR,
               IT_TAB-MEINS  TO  SV_MEINS,
               IT_TAB-MATNM  TO  SV_MATNM.
        PERFORM P3000_TITLE_WRITE.

        CLEAR : SUM_BL_MENGE, SUM_CG_MENGE,
                SUM_NO_MENGE, SUM_IDS_MENGE, WRITE_CHK.
      ENDIF.
    ENDIF.

* 금액 SUM.
    PERFORM  P3000_LINE_WRITE.
    ADD : IT_TAB-BL_MENGE   TO  SUM_BL_MENGE,
          IT_TAB-CG_MENGE   TO  SUM_CG_MENGE,
          IT_TAB-IDS_MENGE  TO  SUM_IDS_MENGE,
          IT_TAB-NO_MENGE   TO  SUM_NO_MENGE.

    ADD : IT_TAB-BL_MENGE   TO  TOT_BL_MENGE,
          IT_TAB-CG_MENGE   TO  TOT_CG_MENGE,
          IT_TAB-IDS_MENGE  TO  TOT_IDS_MENGE,
          IT_TAB-NO_MENGE   TO  TOT_NO_MENGE.

    AT LAST.
      PERFORM P3000_SUM_WRITE.
      PERFORM P3000_LAST_WRITE.
    ENDAT.
  ENDLOOP.

ENDFORM.                    " P2000_WRITE_DATA
*&---------------------------------------------------------------------*
*&      Form  P4000_COMPUTE_RE_QTY
*&---------------------------------------------------------------------*
*       수입의뢰 수량 SUM
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_COMPUTE_RE_QTY.

  LOOP  AT  IT_RN  WHERE  EBELN  =  IT_PO-EBELN
                   AND    EBELP  =  IT_PO-EBELP.
    ADD  IT_RN-MENGE   TO  SUM_RE_MENGE.
  ENDLOOP.

ENDFORM.                    " P4000_COMPUTE_RE_QTY

*&---------------------------------------------------------------------*
*&      Form  P4000_COMPUTE_BL_QTY
*&---------------------------------------------------------------------*
*       BL 수량 SUM
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_COMPUTE_BL_QTY.

  CLEAR  IN_CHK_YN.
  LOOP  AT  IT_BL  WHERE  EBELN  =  IT_PO-EBELN
                   AND    EBELP  =  IT_PO-EBELP.
    ADD  IT_BL-BLMENGE   TO  SUM_BL_MENGE.
    IF IT_BL-ZFELIKZ NE 'X'.
      MOVE  'N'  TO  IN_CHK_YN.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " P4000_COMPUTE_BL_QTY

*&---------------------------------------------------------------------*
*&      Form  P4000_COMPUTE_CG_QTY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_COMPUTE_CG_QTY.

  LOOP  AT  IT_CG  WHERE  ZFBLNO  =  IT_BL-ZFBLNO
                   AND    ZFBLIT  =  IT_BL-ZFBLIT.
    ADD  IT_CG-CGMENGE   TO  SUM_CG_MENGE.
  ENDLOOP.

ENDFORM.                    " P4000_COMPUTE_CG_QTY

*&---------------------------------------------------------------------*
*&      Form  P4000_COMPUTE_IV_QTY
*&---------------------------------------------------------------------*
*       통관/입고 요청 수량 SUM
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_COMPUTE_IV_QTY.

  LOOP  AT  IT_IV  WHERE  EBELN  =  IT_PO-EBELN
                   AND    EBELP  =  IT_PO-EBELP.

    ADD  IT_IV-CCMENGE   TO  SUM_IV_MENGE.
    IF IT_IV-ZFGRST = 'Y'.
      ADD  IT_IV-GRMENGE  TO  SUM_IN_MENGE.
    ENDIF.
    IF IT_IV-ZFCUST = 'Y'.
      ADD  IT_IV-CCMENGE  TO  SUM_IDS_MENGE.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " P4000_COMPUTE_IV_QTY

*&---------------------------------------------------------------------*
*&      Form  P4000_COMPUTE_IDR_QTY
*&---------------------------------------------------------------------*
*       수입신고 수량 SUM
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_COMPUTE_IDR_QTY.

  LOOP  AT  IT_IDR   WHERE  EBELN  =  IT_PO-EBELN
                     AND    EBELP  =  IT_PO-EBELP.
    ADD  IT_IDR-ZFQNT  TO  SUM_IDR_MENGE.
  ENDLOOP.

ENDFORM.                    " P4000_COMPUTE_IDR_QTY

*&---------------------------------------------------------------------*
*&      Form  P4000_COMPUTE_IDS_QTY
*&---------------------------------------------------------------------*
*       수입면허 수량 SUM
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_COMPUTE_IDS_QTY.
  LOOP  AT  IT_IDS   WHERE  ZFBLNO  =  IT_BL-ZFBLNO
                     AND    ZFBLIT  =  IT_BL-ZFBLIT.
    ADD  IT_IDS-ZFQNT  TO  SUM_IDS_MENGE.
  ENDLOOP.

ENDFORM.                    " P4000_COMPUTE_IDS_QTY

*&---------------------------------------------------------------------*
*&      Form  P2000_SORT_DATA
*&---------------------------------------------------------------------*
*       INTERNAL TABLE SORT.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_SORT_DATA.

  SORT  IT_BL  BY  WERKS  LGORT  MATNR  EBELN  EBELP.
  SORT  IT_CG  BY  EBELN  EBELP.
  SORT  IT_IDS BY  EBELN  EBELP.

ENDFORM.                    " P2000_SORT_DATA

*&---------------------------------------------------------------------*
*&      Form  P2000_WRITE_IT
*&---------------------------------------------------------------------*
*       WRITE 하기전에 INTERNAL TABLE MOVE.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_WRITE_IT.

  CLEAR  : SUM_BL_MENGE,  SUM_CG_MENGE, SUM_IDS_MENGE, SUM_NO_MENGE.

  LOOP  AT  IT_BL.

    IF SY-TABIX EQ 1.
      MOVE  : IT_BL-EBELN    TO   W_EBELN,
              IT_BL-EBELP    TO   W_EBELP,
              IT_BL-MATNR    TO   SV_MATNR,
              IT_BL-MEINS    TO   SV_MEINS,
              IT_BL-TXZ01    TO   SV_MATNM,
              IT_BL-WERKS    TO   SV_WERKS,
              IT_BL-LGORT    TO   SV_LGORT.
    ENDIF.

    IF IT_BL-EBELN   NE  W_EBELN  OR
       IT_BL-EBELP   NE  W_EBELP  .

      IF SUM_CG_MENGE EQ 0.
        SUM_NO_MENGE   =  SUM_BL_MENGE  -  SUM_IDS_MENGE.
      ELSE.
        SUM_NO_MENGE   =  SUM_CG_MENGE  -  SUM_IDS_MENGE.
      ENDIF.

*>> 미통관 자료인 경우만 WRITE.
      IF SUM_NO_MENGE NE 0.
        MOVE  :  SV_MATNR       TO  IT_TAB-MATNR,
                 SV_MATNM       TO  IT_TAB-MATNM,
                 W_EBELN        TO  IT_TAB-EBELN,
                 W_EBELP        TO  IT_TAB-EBELP,
                 SV_MEINS       TO  IT_TAB-MEINS,
                 SV_WERKS       TO  IT_TAB-WERKS,
                 SV_LGORT       TO  IT_TAB-LGORT,
                 SUM_BL_MENGE   TO  IT_TAB-BL_MENGE,
                 SUM_CG_MENGE   TO  IT_TAB-CG_MENGE,
                 SUM_IDS_MENGE  TO  IT_TAB-IDS_MENGE,
                 SUM_NO_MENGE   TO  IT_TAB-NO_MENGE.
        APPEND  IT_TAB.
      ENDIF.

      CLEAR : SUM_BL_MENGE,  SUM_CG_MENGE,
              SUM_IDS_MENGE, SUM_NO_MENGE.
      MOVE  : IT_BL-EBELN    TO   W_EBELN,
              IT_BL-EBELP    TO   W_EBELP,
              IT_BL-MATNR    TO   SV_MATNR,
              IT_BL-MEINS    TO   SV_MEINS,
              IT_BL-TXZ01    TO   SV_MATNM,
              IT_BL-WERKS    TO   SV_WERKS,
              IT_BL-LGORT    TO   SV_LGORT.
    ENDIF.

    ADD     IT_BL-BLMENGE     TO   SUM_BL_MENGE.
    PERFORM  P4000_COMPUTE_CG_QTY.
    PERFORM  P4000_COMPUTE_IDS_QTY.

  ENDLOOP.

  SORT  IT_TAB  BY  WERKS  LGORT  MATNR  EBELN  EBELP.

ENDFORM.                    " P2000_WRITE_IT

*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  IF WRITE_CHK IS INITIAL.
    SKIP 2.
    FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
    WRITE : /30  '[ 플랜트 별 미통관 LIST ]'
                 COLOR COL_HEADING INTENSIFIED OFF.
    WRITE : / 'Date : ', SY-DATUM.
    WRITE : / SY-ULINE.
    FORMAT COLOR COL_HEADING INTENSIFIED ON.
    WRITE : / SY-VLINE NO-GAP,
              'P/O             '               NO-GAP, SY-VLINE NO-GAP,
              '         B/L 수량'              NO-GAP, SY-VLINE NO-GAP,
              '         하역수량'              NO-GAP, SY-VLINE NO-GAP,
              '         통관수량'              NO-GAP, SY-VLINE NO-GAP,
              '       미통관수량'              NO-GAP, SY-VLINE NO-GAP.
    WRITE : / SY-ULINE NO-GAP.
  ENDIF.

*>> PLANT, 저장위치 TITLE
  IF WRITE_CHK = '1'.
    FORMAT COLOR COL_BACKGROUND.
    WRITE : SY-VLINE NO-GAP,
            'PLANT : '                      NO-GAP,
            SV_WERKS                        NO-GAP,
            ' '                             NO-GAP,
            SV_WERKS_NM                     NO-GAP,
            '저장위치 : '                   NO-GAP,
            SV_LGORT                        NO-GAP,
            ' '                             NO-GAP,
            SV_LGORT_NM                     NO-GAP,
            '             '                 NO-GAP, SY-VLINE NO-GAP.
    WRITE : / SY-ULINE NO-GAP.
  ENDIF.
*>> 자재내역 TITLE WRITE.
  IF WRITE_CHK = '2'.
    FORMAT COLOR COL_BACKGROUND.
    WRITE : SY-VLINE NO-GAP,
            '자재 : '                       NO-GAP,
            SV_MATNR                        NO-GAP,
            ' '                             NO-GAP,
            SV_MATNM                        NO-GAP,
            '단위 : '                       NO-GAP,
            SV_MEINS                        NO-GAP,
            '            '                  NO-GAP, SY-VLINE NO-GAP.
    WRITE : / SY-ULINE NO-GAP.
  ENDIF.


ENDFORM.                    " P3000_TITLE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_SUM_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_SUM_WRITE.

  FORMAT RESET.
  FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.

  WRITE : / SY-ULINE.
  WRITE:/ SY-VLINE NO-GAP,
       '자재별 소계 '          NO-GAP,
       '    '                  NO-GAP, SY-VLINE NO-GAP,
       SUM_BL_MENGE   UNIT SV_MEINS         NO-GAP, SY-VLINE NO-GAP,
       SUM_CG_MENGE   UNIT SV_MEINS         NO-GAP, SY-VLINE NO-GAP,
       SUM_IDS_MENGE  UNIT SV_MEINS         NO-GAP, SY-VLINE NO-GAP,
       SUM_NO_MENGE   UNIT SV_MEINS         NO-GAP, SY-VLINE NO-GAP.

  WRITE : / SY-ULINE.

ENDFORM.                    " P3000_SUM_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_LINE_WRITE.
  FORMAT RESET.
  IF W_MOD = 1.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ELSE.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ENDIF.

  WRITE:/ SY-VLINE NO-GAP,
       IT_TAB-EBELN         NO-GAP,
       ' '                     NO-GAP,
       IT_TAB-EBELP            NO-GAP, SY-VLINE NO-GAP,
       IT_TAB-BL_MENGE  UNIT IT_TAB-MEINS NO-GAP, SY-VLINE NO-GAP,
       IT_TAB-CG_MENGE  UNIT IT_TAB-MEINS NO-GAP, SY-VLINE NO-GAP,
       IT_TAB-IDS_MENGE UNIT IT_TAB-MEINS NO-GAP, SY-VLINE NO-GAP,
       IT_TAB-NO_MENGE  UNIT IT_TAB-MEINS NO-GAP, SY-VLINE NO-GAP.

* Stored value...
  HIDE IT_TAB.

ENDFORM.                    " P3000_LINE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_LAST_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_LAST_WRITE.

  FORMAT RESET.
  FORMAT COLOR COL_TOTAL INTENSIFIED OFF.

  WRITE:/ SY-VLINE NO-GAP,
       '자재별 합계 '          NO-GAP,
       '    '                  NO-GAP, SY-VLINE NO-GAP,
       TOT_BL_MENGE    UNIT SV_MEINS       NO-GAP, SY-VLINE NO-GAP,
       TOT_CG_MENGE    UNIT SV_MEINS       NO-GAP, SY-VLINE NO-GAP,
       TOT_IDS_MENGE   UNIT SV_MEINS       NO-GAP, SY-VLINE NO-GAP,
       TOT_NO_MENGE    UNIT SV_MEINS       NO-GAP, SY-VLINE NO-GAP.

  WRITE : / SY-ULINE.

ENDFORM.                    " P3000_LAST_WRITE
*&---------------------------------------------------------------------*
*&      Form  P4000_COMPUTE_CIV_QTY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_COMPUTE_CIV_QTY.
  LOOP  AT  IT_CIV WHERE  EBELN  =  IT_PO-EBELN
                   AND    EBELP  =  IT_PO-EBELP.

    ADD  IT_CIV-CMENGE   TO  SUM_CIV_MENGE.
    ADD  IT_CIV-CMENGE   TO  TOT_CIV_MENGE.
  ENDLOOP.

ENDFORM.                    " P4000_COMPUTE_CIV_QTY
*&---------------------------------------------------------------------*
*&      Module  D0100_STATUS_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D0100_STATUS_SCR0100 OUTPUT.
  SET PF-STATUS 'ZIMR47'.

  CASE INCLUDE.
    WHEN 'POPU'.
      SET TITLEBAR 'POPU' WITH '상태 LIST'.
    WHEN OTHERS.
  ENDCASE.

  SUPPRESS DIALOG.

ENDMODULE.                 " D0100_STATUS_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  D0100_LIST_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D0100_LIST_CHECK_SCR0100 INPUT.
  LEAVE TO LIST-PROCESSING.
  CASE INCLUDE.
    WHEN 'POPU'.
      READ  TABLE  IT_PO  WITH  KEY  EBELN  =  W_EBELN
                                     EBELP  =  W_EBELP.
      IF SY-SUBRC EQ 0.
        PERFORM  P1000_WRITE_PO_SCR0100.
      ENDIF.
  ENDCASE.

ENDMODULE.                 " D0100_LIST_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  P1000_WRITE_PO_SCR0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_WRITE_PO_SCR0100.
*>> PO DATA WRITE.
  TOT_LINE  =  TOT_LINE + 1.
  W_MOD     =  TOT_LINE MOD 2.
  IF W_MOD = 1.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ELSE.
    FORMAT COLOR COL_BACKGROUND.
  ENDIF.

  WRITE: /  'PO        ' NO-GAP,
         15 IT_PO-EBELN  NO-GAP.
  WRITE: '-'             NO-GAP,
            IT_PO-EBELP  NO-GAP,
         68 IT_PO-MENGE  UNIT IT_PO-MEINS NO-GAP,
         85 IT_PO-MEINS  NO-GAP.
  HIDE IT_PO.
*>> 수입의뢰 DATA WRITE.
  LOOP  AT  IT_RN  WHERE  EBELN  =  W_EBELN  AND  EBELP = W_EBELP.
    PERFORM  P2000_WRITE_RN_SCR0100.
  ENDLOOP.

ENDFORM.                    " P1000_WRITE_PO_SCR0100

*&---------------------------------------------------------------------*
*&      Form  P2000_WRITE_RN_SCR0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_WRITE_RN_SCR0100.
  TOT_LINE  =  TOT_LINE + 1.
  W_MOD     =  TOT_LINE MOD 2.
  IF W_MOD = 1.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ELSE.
    FORMAT COLOR COL_BACKGROUND.
  ENDIF.

  WRITE: / '수입의뢰  '    NO-GAP,
          17  IT_RN-ZFREQNO NO-GAP,
         '-'               NO-GAP,
             IT_RN-ZFITMNO NO-GAP,
         45  IT_RN-ZFOPNNO(20) NO-GAP,
         68 IT_RN-MENGE UNIT IT_RN-MEINS NO-GAP,
         85 IT_RN-MEINS NO-GAP.
  HIDE IT_RN.

  IF IT_RN-ZFREQTY EQ 'LO' OR IT_RN-ZFREQTY EQ 'PU'.
    LOOP  AT  IT_IV  WHERE  ZFREQNO EQ IT_RN-ZFREQNO
                     AND    ZFITMNO EQ IT_RN-ZFITMNO.
      PERFORM  P5000_WRITE_IV_SCR0100.
    ENDLOOP.
  ELSE.
    LOOP  AT  IT_BL  WHERE  ZFREQNO  =  IT_RN-ZFREQNO
                  AND    ZFITMNO  =  IT_RN-ZFITMNO.
      PERFORM  P3000_WRITE_BL_SCR0100.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " P2000_WRITE_RN_SCR0100

*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_BL_SCR0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_WRITE_BL_SCR0100.

  TOT_LINE  =  TOT_LINE + 1.
  W_MOD     =  TOT_LINE MOD 2.
  IF W_MOD = 1.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ELSE.
    FORMAT COLOR COL_BACKGROUND.
  ENDIF.
  WRITE: /'BL        '      NO-GAP,
         19  IT_BL-ZFBLNO   NO-GAP,
         '-'                NO-GAP,
            IT_BL-ZFITMNO  NO-GAP,
         45 IT_BL-ZFFORD NO-GAP,
         68 IT_BL-BLMENGE UNIT IT_BL-MEINS NO-GAP,
         85 IT_BL-MEINS NO-GAP.
  HIDE  IT_BL.

  LOOP  AT  IT_CG  WHERE  ZFBLNO = IT_BL-ZFBLNO
                   AND    ZFBLIT = IT_BL-ZFBLIT.
    W_LOOP_CNT = W_LOOP_CNT + 1.
    PERFORM P4000_WRITE_CG_SCR0100.
  ENDLOOP.

  IF W_LOOP_CNT < 1.
    LOOP  AT  IT_IV WHERE  ZFBLNO = IT_BL-ZFBLNO
                    AND    ZFBLIT = IT_BL-ZFBLIT.
      W_TABIX  =  SY-TABIX.
      PERFORM P5000_WRITE_IV_SCR0100.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " P3000_WRITE_BL_SCR0100

*&---------------------------------------------------------------------*
*&      Form  P4000_WRITE_CG_SCR0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_WRITE_CG_SCR0100.
  TOT_LINE  =  TOT_LINE + 1.
  W_MOD     =  TOT_LINE MOD 2.
  IF W_MOD = 1.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ELSE.
    FORMAT COLOR COL_BACKGROUND.
  ENDIF.
  WRITE: /'하역        '    NO-GAP,
          21   IT_CG-ZFCGNO NO-GAP,
         '-'               NO-GAP,
            IT_CG-ZFCGIT   NO-GAP,
         45 IT_CG-ZFCGPT   NO-GAP,
         68 IT_CG-CGMENGE UNIT IT_CG-MEINS NO-GAP,
         85 IT_CG-MEINS NO-GAP.
  HIDE IT_CG.

  LOOP  AT  IT_IV WHERE  ZFCGNO  =  IT_CG-ZFCGNO
                  AND    ZFCGIT  =  IT_CG-ZFCGIT.
    W_TABIX  =  SY-TABIX.
    PERFORM  P5000_WRITE_IV_SCR0100.
  ENDLOOP.
ENDFORM.                    " P4000_WRITE_CG_SCR0100

*&---------------------------------------------------------------------*
*&      Form  P5000_WRITE_IV_SCR0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P5000_WRITE_IV_SCR0100.
  TOT_LINE  =  TOT_LINE + 1.
  W_MOD     =  TOT_LINE MOD 2.
  IF W_MOD = 1.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ELSE.
    FORMAT COLOR COL_BACKGROUND.
  ENDIF.
*>> 자재문서 DISPLAY
  SELECT SINGLE *     FROM ZTIVHST
  WHERE  ZFIVNO       EQ   IT_IV-ZFIVNO
  AND    ZFIVHST      EQ   ( SELECT MAX( ZFIVHST )
                             FROM   ZTIVHST
                             WHERE  ZFIVNO  EQ  IT_IV-ZFIVNO ).

  MOVE  ZTIVHST-MBLNR       TO   IT_IV-MBLNR.
  MOVE  ZTIVHST-MJAHR       TO   IT_IV-MJAHR.
  MODIFY  IT_IV  INDEX      W_TABIX.

  WRITE: /'통관요청  '      NO-GAP,
          23  IT_IV-ZFIVNO  NO-GAP,
          '-'              NO-GAP,
             IT_IV-ZFIVDNO NO-GAP,
          68 IT_IV-CCMENGE UNIT IT_IV-MEINS NO-GAP,
          85 IT_IV-MEINS NO-GAP.
  HIDE IT_IV.

  IF IT_IV-ZFBLNO IS INITIAL AND IT_IV-ZFCGNO IS INITIAL.
    IF IT_IV-ZFGRST = 'Y'.
      PERFORM P7000_WRITE_IN_SCRO100.
    ENDIF.
  ELSE.
    LOOP  AT  IT_IDS  WHERE  ZFIVNO  =  IT_IV-ZFIVNO
                      AND    ZFIVDNO =  IT_IV-ZFIVDNO.
      PERFORM  P6000_WRITE_IDR_SCR0100.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " P5000_WRITE_IV_SCR0100

*&---------------------------------------------------------------------*
*&      Form  P6000_WRITE_IDR_SCR0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P6000_WRITE_IDR_SCR0100.
  TOT_LINE  =  TOT_LINE + 1.
  W_MOD     =  TOT_LINE MOD 2.
  IF W_MOD = 1.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ELSE.
    FORMAT COLOR COL_BACKGROUND.
  ENDIF.

  WRITE: /'통관        '    NO-GAP,
          23 IT_IDS-ZFENTNO NO-GAP,
          45 IT_IDS-ZFCUT   NO-GAP,
          68 IT_IDS-ZFQNT   UNIT  IT_IDS-ZFQNTM NO-GAP,
          85 IT_IDS-ZFQNTM  NO-GAP.
  HIDE  IT_IDS.

  IF IT_IV-ZFGRST = 'Y'.
    PERFORM P7000_WRITE_IN_SCRO100.
  ENDIF.

ENDFORM.                    " P6000_WRITE_IDR_SCR0100

*&---------------------------------------------------------------------*
*&      Form  P7000_WRITE_IN_SCRO100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P7000_WRITE_IN_SCRO100.

  TOT_LINE  =  TOT_LINE + 1.
  W_MOD     =  TOT_LINE MOD 2.
  IF W_MOD = 1.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ELSE.
    FORMAT COLOR COL_BACKGROUND.
  ENDIF.

*>> PLANT 명 DISPLAY
  SELECT SINGLE NAME1 INTO W_NAME1
  FROM   T001W
  WHERE  WERKS  EQ  IT_IV-WERKS.
*>> 저장위치 명 DISPLAY.
  SELECT SINGLE LGOBE  INTO W_LGOBE
  FROM   T001L
  WHERE  WERKS  EQ  IT_IV-WERKS
  AND    LGORT  EQ  IT_IV-LGORT.

  WRITE: /'입고        '    NO-GAP,
          25 IT_IV-MJAHR    NO-GAP,
             '-'            NO-GAP,
             IT_IV-MBLNR    NO-GAP,
          45 IT_IV-WERKS    NO-GAP,
             ' '            NO-GAP,
             W_NAME1        NO-GAP,
             ' '            NO-GAP,
             IT_IV-LGORT    NO-GAP,
             W_LGOBE        NO-GAP,
          68 IT_IV-GRMENGE  UNIT IT_IV-MEINS NO-GAP,
          85 IT_IV-MEINS    NO-GAP.

ENDFORM.                    " P7000_WRITE_IN_SCRO100
*&---------------------------------------------------------------------*
*&      Form  P3000_NAME_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SV_WERKS  text
*      -->P_SV_LGORT  text
*      <--P_SV_WERKS_NM  text
*      <--P_SV_LGORT_NM  text
*----------------------------------------------------------------------*
FORM P3000_NAME_GET USING    P_WERKS
                             P_LGORT
                    CHANGING SV_WERKS_NM
                             SV_LGORT_NM.

  CLEAR : SV_WERKS_NM, SV_LGORT_NM.
  SELECT SINGLE NAME1   INTO SV_WERKS_NM
  FROM   T001W
  WHERE  WERKS          EQ   P_WERKS.

  SELECT SINGLE LGOBE   INTO SV_LGORT_NM
  FROM   T001L
  WHERE  WERKS  EQ  P_WERKS
  AND    LGORT  EQ  P_LGORT.

ENDFORM.                    " P3000_NAME_GET
