*&---------------------------------------------------------------------*
*& Report  ZRIMBATCH01                                                 *
*&---------------------------------------------------------------------*
*& 프로그램명 : P/O 변경 자동 UPDATE                                   *
*&     작성자 : 나현주                                                 *
*&     작성일 : 2002.10.29                                             *
*&---------------------------------------------------------------------*
*&      DESC. : P/O 변경내역을 선적전의 수입의뢰 내역에 자동 UPDATE
*&---------------------------------------------------------------------*

REPORT  ZRIMBATCH01 MESSAGE-ID ZIM1.

TABLES : ZTREQIT,
         ZTREQHD,
         ZTBLIT,
         EKKO,
         EKPO.

*----------------------------------------------------------------------
* INTERNAL TABLE DEFINE
*----------------------------------------------------------------------
DATA : BEGIN OF IT_TAB OCCURS 0,
       ZFREQNO  LIKE  ZTREQIT-ZFREQNO,
       ZFITMNO  LIKE  ZTREQIT-ZFITMNO,
       EBELN    LIKE  ZTREQIT-EBELN,
       EBELP    LIKE  ZTREQIT-EBELP.
DATA : END  OF IT_TAB.

*-----------------------------------------------------------------------
* 변수 DEFINE
*-----------------------------------------------------------------------
DATA : W_TABIX    LIKE  SY-TABIX,
       W_CNT      TYPE  I,
       W_TOT_CNT  TYPE  I,
       W_PROC_CNT TYPE  I,
       W_ERR_CNT  TYPE  I.

*-----------------------------------------------------------------------
* START OF SELECTION .
*-----------------------------------------------------------------------
START-OF-SELECTION.

   PERFORM  P1000_READ_BL_DATA.
   PERFORM  P1000_READ_PO_DATA.

*-----------------------------------------------------------------------
* END OF SELECTION .
*-----------------------------------------------------------------------
END-OF-SELECTION.
   MESSAGE  S299  WITH W_TOT_CNT W_PROC_CNT 'Normal'.

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_BL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_BL_DATA.

   SELECT  A~ZFREQNO  A~ZFITMNO  A~EBELN  A~EBELP
   INTO CORRESPONDING FIELDS OF TABLE IT_TAB
   FROM    ZTREQIT  AS  A  INNER  JOIN  ZTREQHD  AS  B
   ON      A~ZFREQNO       EQ     B~ZFREQNO
   WHERE   B~ZFREQTY       NE     'LC'.

   LOOP  AT  IT_TAB.
      W_TABIX  =  SY-TABIX.
      SELECT COUNT( * )  INTO  W_CNT
      FROM   ZTBLIT
      WHERE  ZFREQNO     EQ    IT_TAB-ZFREQNO
      AND    ZFITMNO     EQ    IT_TAB-ZFITMNO.

      IF W_CNT NE 1.
         DELETE  IT_TAB  INDEX  W_TABIX.
      ENDIF.
   ENDLOOP.

ENDFORM.                    " P1000_READ_BL_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_PO_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_PO_DATA.

   CLEAR : W_TOT_CNT, W_PROC_CNT, W_ERR_CNT, W_CNT.

   LOOP  AT  IT_TAB.

      CLEAR : EKPO, ZTREQIT.
      SELECT SINGLE * FROM EKPO
             WHERE  EBELN  EQ   IT_TAB-EBELN
             AND    EBELP  EQ   IT_TAB-EBELP.

      SELECT SINGLE * FROM ZTREQIT
             WHERE  ZFREQNO EQ  IT_TAB-ZFREQNO
             AND    ZFITMNO EQ  IT_TAB-ZFITMNO.

      " PO DATA 와 수입의뢰 DATA 비교 분석후 수정사항대로 UPDATE.
      IF EKPO-MATNR  NE  ZTREQIT-MATNR OR
         EKPO-NETPR  NE  ZTREQIT-NETPR OR
         EKPO-MENGE  NE  ZTREQIT-MENGE OR
         EKPO-MEINS  NE  ZTREQIT-MEINS OR
         EKPO-PEINH  NE  ZTREQIT-PEINH OR
         EKPO-BPRME  NE  ZTREQIT-BPRME OR
         EKPO-BEDNR  NE  ZTREQIT-BEDNR OR
         EKPO-AFNAM  NE  ZTREQIT-AFNAM .

         PERFORM  P2000_ITEM_DATA_AOTO_UPDATE.

      ENDIF.
   ENDLOOP.

ENDFORM.                    " P1000_READ_PO_DATA

*&---------------------------------------------------------------------*
*&      Form  P2000_ITEM_DATA_AOTO_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_ITEM_DATA_AOTO_UPDATE.

   W_TOT_CNT  =  W_TOT_CNT  +  1.

   MOVE : EKPO-MATNR  TO  ZTREQIT-MATNR,
          EKPO-NETPR  TO  ZTREQIT-NETPR,
          EKPO-MENGE  TO  ZTREQIT-MENGE,
          EKPO-MEINS  TO  ZTREQIT-MEINS,
          EKPO-PEINH  TO  ZTREQIT-PEINH,
          EKPO-BPRME  TO  ZTREQIT-BPRME,
          EKPO-BEDNR  TO  ZTREQIT-BEDNR,
          EKPO-AFNAM  TO  ZTREQIT-AFNAM.

   UPDATE  ZTREQIT.
   IF SY-SUBRC NE 0.
      W_ERR_CNT   =  W_ERR_CNT   + 1.
   ELSE.
      W_PROC_CNT  =  W_PROC_CNT  +  1.
   ENDIF.

ENDFORM.                    " P2000_ITEM_DATA_AOTO_UPDATE
