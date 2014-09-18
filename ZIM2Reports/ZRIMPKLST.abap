*&---------------------------------------------------------------------*
*& Report  ZRIMPKLST                                                   *
*&---------------------------------------------------------------------*
*&  Program    : Packing List                                          *
*&  Created on : Shinho, Na INFOLINK Ltd.                              *
*&  Created by : 2004.05.17                                            *
*&  Project    : HMMA                                                  *
*&---------------------------------------------------------------------*
REPORT  ZRIMPKLST   MESSAGE-ID ZIM NO STANDARD PAGE HEADING
                     LINE-SIZE 170.

TABLES : LIKP,       " SD Document: Delivery Header Data.
         LIPS,       " SD document: Delivery Item data.
         MARA,       " Material Master Table..
         ZTCIVHD,    " Commercial Invoice Header Table..
         ZTCIVIT.    " Commercial Invoice Items Table..

DATA:   BEGIN OF IT_ZSCIVIT OCCURS 0.   ">> RETURN.
        INCLUDE STRUCTURE   ZSCIVIT.
DATA:   END   OF IT_ZSCIVIT.

DATA: BEGIN OF IT_LIPS OCCURS 0,
        TOTWT  LIKE ZTCIVIT-CMENGE,
        BOLNR  LIKE LIKP-BOLNR,
        TRAID  LIKE LIKP-TRAID,
        KDMAT  LIKE LIPS-KDMAT,
        POSNR  LIKE LIPS-POSNR,
        VGBEL  LIKE LIPS-VGBEL,
        VGPOS  LIKE LIPS-VGPOS,
        MATNR  LIKE LIPS-MATNR,
        ARKTX  LIKE LIPS-ARKTX,
        LFIMG  LIKE LIPS-LFIMG,
        MEINS  LIKE LIPS-MEINS,
        NTGEW  LIKE MARA-NTGEW,
        GEWEI  LIKE MARA-GEWEI,
      END OF IT_LIPS.

DATA: W_TABIX      LIKE SY-TABIX,
      W_ERR_CHK    TYPE C,
      W_OK_CODE    LIKE SY-UCOMM.

DATA: W_EBELN      LIKE LIPS-VGBEL,
      W_TRAID      LIKE LIKP-TRAID,
      W_KDMAT      LIKE LIPS-KDMAT,
      C_LFIMG      LIKE LIPS-LFIMG,
      W_LFIMG      LIKE LIPS-LFIMG,
      P_LFIMG      LIKE LIPS-LFIMG,
      W_FIRST      TYPE I,
      C_ORDER_ITEM TYPE I,
      P_ORDER_ITEM TYPE I,
      W_ORDER_ITEM TYPE I.
DATA: W_PAGE       TYPE I,                 " Page Counter..
      W_LINE       TYPE I,                 " Line Counter..
      W_COUNT      TYPE I.
*-----------------------------------------------------------------------
* 검색조건 Selection Window.
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
PARAMETER : P_CIVRN LIKE ZTCIVHD-ZFCIVRN.
SELECTION-SCREEN END OF BLOCK B2.

*-----------------------------------------------------------------------
* INITIALIZATION.
*-----------------------------------------------------------------------
INITIALIZATION.
  SET TITLEBAR 'TIT1'.

*-----------------------------------------------------------------------
* TOP-OF-PAGE.
*-----------------------------------------------------------------------
TOP-OF-PAGE.
  PERFORM P3000_TITLE_WRITE.

*-----------------------------------------------------------------------
* START-OF-SELECTION.
*-----------------------------------------------------------------------
START-OF-SELECTION.

  PERFORM P1000_READ_CIV_DATA.
  CHECK W_ERR_CHK EQ 'N'.

*-----------------------------------------------------------------------
* END-OF-SELECTION.
*-----------------------------------------------------------------------
END-OF-SELECTION.
  SET TITLEBAR 'TIT1'.
  SET PF-STATUS 'ZIM55'.

  PERFORM P3000_WRITE_CIV_DATA.

*-----------------------------------------------------------------------
* AT USER-COMMAND.
*-----------------------------------------------------------------------
AT USER-COMMAND.
  CASE SY-UCOMM.
  ENDCASE.

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_CIV_DATA
*&---------------------------------------------------------------------*
FORM P1000_READ_CIV_DATA.

  SELECT SINGLE *
           FROM ZTCIVHD
          WHERE ZFCIVRN = P_CIVRN.
  IF SY-SUBRC EQ 0.
    SELECT *
      FROM LIKP
     WHERE BOLNR = ZTCIVHD-ZFCIVNO.
      IF SY-SUBRC EQ 0.
        SELECT *
          FROM LIPS
         WHERE VBELN = LIKP-VBELN.
          IF SY-SUBRC EQ 0.
            MOVE-CORRESPONDING LIPS TO IT_LIPS.
            MOVE LIKP-BOLNR TO IT_LIPS-BOLNR.
            MOVE LIKP-TRAID TO IT_LIPS-TRAID.
            APPEND IT_LIPS.
          ENDIF.
        ENDSELECT.
      ENDIF.
    ENDSELECT.
  ENDIF.

  LOOP AT IT_LIPS.
    SELECT SINGLE NTGEW
             INTO IT_LIPS-NTGEW
             FROM MARA
            WHERE MATNR = IT_LIPS-MATNR.
    IT_LIPS-TOTWT = IT_LIPS-LFIMG * IT_LIPS-NTGEW.
    MODIFY IT_LIPS INDEX SY-TABIX.
  ENDLOOP.

ENDFORM.                    " P1000_READ_CIV_DATA
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_CIV_DATA
*&---------------------------------------------------------------------*
FORM P3000_WRITE_CIV_DATA.

  SORT IT_LIPS BY TRAID KDMAT VGBEL VGPOS.
  W_PAGE = 1.     W_LINE = 0.     W_COUNT = 0.    W_FIRST = 0.

  LOOP AT IT_LIPS.
    W_COUNT = W_COUNT + 1.
    IF SY-TABIX EQ 1.
      PERFORM P3000_WRITE_ITEM_HEADER.
      PERFORM P3000_WRITE_ITEM_DETAIL.
      PERFORM P3000_MOVE_QTY_DATA_1.
    ELSE.
      IF W_TRAID = IT_LIPS-TRAID.
        ADD 1                   TO W_ORDER_ITEM.
        ADD IT_LIPS-LFIMG       TO W_LFIMG.
        PERFORM P3000_WRITE_ITEM_DETAIL.
        IF W_KDMAT = IT_LIPS-KDMAT.
          ADD 1                 TO C_ORDER_ITEM.
          ADD IT_LIPS-LFIMG     TO C_LFIMG.
        ELSE.
          PERFORM P3000_WRITE_ITEM_SUM_C. " Case item summation.
          PERFORM P3000_MOVE_CQTY_DATA.
        ENDIF.
      ELSE.
        PERFORM P3000_WRITE_ITEM_SUM.       " Summarize container item.
        PERFORM P3000_WRITE_ITEM_HEADER.
        PERFORM P3000_WRITE_ITEM_DETAIL.
        PERFORM P3000_MOVE_CQTY_DATA_1.
        PERFORM P3000_MOVE_QTY_DATA.
      ENDIF.
      PERFORM P2000_PAGE_CHECK.
    ENDIF.

    W_KDMAT = IT_LIPS-KDMAT.

    AT LAST.
      PERFORM P3000_WRITE_ITEM_SUM.
    ENDAT.

  ENDLOOP.

  PERFORM P3000_WRITE_GRAND_TOTAL.

ENDFORM.                    " P3000_WRITE_CIV_DATA
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  SKIP.
  WRITE:  'Packing List' NO-GAP.
  WRITE: / 'Invoice No.:', ZTCIVHD-ZFCIVNO, 110 'Page : ', W_PAGE.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  p3000_write_item_header
*&---------------------------------------------------------------------*
FORM P3000_WRITE_ITEM_HEADER.

  SKIP.
  WRITE: /'Container No.:', IT_LIPS-TRAID.
  WRITE: /'- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -'.

  WRITE: /(29)'Case No.', (19)'Order No.', (09)'L/I',
          (19)'Part No.', (29)'Part Name', (29)'Shipped Qty',
          (20)'TTL NET WGT KG' CENTERED.
ENDFORM.                    " p3000_write_item_header
*&---------------------------------------------------------------------*
*&      Form  p3000_write_item_detail
*&---------------------------------------------------------------------*
FORM P3000_WRITE_ITEM_DETAIL.

  WRITE: / IT_LIPS-KDMAT(29),
       (10)IT_LIPS-VGBEL,     (05)IT_LIPS-VGPOS+1(05), '  ',
       (09)IT_LIPS-POSNR,     (19)IT_LIPS-MATNR, (29)IT_LIPS-ARKTX,
       (20)IT_LIPS-LFIMG UNIT IT_LIPS-MEINS,     (05)IT_LIPS-MEINS,
       (17)IT_LIPS-TOTWT UNIT IT_LIPS-GEWEI NO-GAP, (03)IT_LIPS-GEWEI.

ENDFORM.                    " p3000_write_item_detail
*&---------------------------------------------------------------------*
*&      Form  p3000_move_qty_data_1
*&---------------------------------------------------------------------*
FORM P3000_MOVE_QTY_DATA_1.

  W_ORDER_ITEM = 1.
  C_ORDER_ITEM = 1.
  MOVE IT_LIPS-TRAID    TO W_TRAID.
  MOVE IT_LIPS-LFIMG    TO W_LFIMG. " Ship-Qty
  MOVE IT_LIPS-KDMAT    TO W_KDMAT.
  READ TABLE IT_LIPS INDEX 2.
  IF IT_LIPS-KDMAT NE W_KDMAT.
    MOVE W_LFIMG TO C_LFIMG.
    WRITE: /30 'Case - - - - Items:', C_ORDER_ITEM.
    WRITE: /30 'Case - - - - Piece:', C_LFIMG UNIT IT_LIPS-MEINS.
    SKIP.
  ENDIF.
  READ TABLE IT_LIPS INDEX 1.

ENDFORM.                    " p3000_move_qty_data_1
*&---------------------------------------------------------------------*
*&      Form  p3000_move_qty_data
*&---------------------------------------------------------------------*
FORM P3000_MOVE_QTY_DATA.

  W_ORDER_ITEM = 1.
  C_ORDER_ITEM = 1.
  MOVE IT_LIPS-TRAID    TO W_TRAID.
  MOVE IT_LIPS-LFIMG    TO W_LFIMG. " Ship-Qty
  MOVE IT_LIPS-KDMAT    TO W_KDMAT.

ENDFORM.                    " p3000_move_qty_data
*&---------------------------------------------------------------------*
*&      Form  p3000_write_item_sum
*&---------------------------------------------------------------------*
FORM P3000_WRITE_ITEM_SUM.

  WRITE: /30 'Packing - - - Items:', W_ORDER_ITEM.
  WRITE: /30 'Packing - - - Piece:', W_LFIMG UNIT IT_LIPS-MEINS.

ENDFORM.                    " p3000_write_item_sum
*&---------------------------------------------------------------------*
*&      Form  p3000_write_grand_total
*&---------------------------------------------------------------------*
FORM P3000_WRITE_GRAND_TOTAL.

  SKIP.
  CLEAR: W_ORDER_ITEM, W_LFIMG.

  LOOP AT IT_LIPS.
    ADD 1             TO W_ORDER_ITEM.
    ADD IT_LIPS-LFIMG TO W_LFIMG.
  ENDLOOP.

  WRITE: /30 'Order Grand Total Items:', W_ORDER_ITEM.
  WRITE: /30 'Order Grand Total Piece:', W_LFIMG UNIT IT_LIPS-MEINS.

ENDFORM.                    " p3000_write_grand_total
*&---------------------------------------------------------------------*
*&      Form  P2000_PAGE_CHECK
*&---------------------------------------------------------------------*
FORM P2000_PAGE_CHECK.

  W_LINE = W_LINE + 1.

  IF W_LINE >= 53.
    WRITE : / SY-ULINE.
    W_PAGE = W_PAGE + 1.    W_LINE = 0.
    NEW-PAGE.
  ENDIF.

ENDFORM.                    " P2000_PAGE_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_CONTAINER_PAGE_CHECK
*&---------------------------------------------------------------------*
FORM P2000_CONTAINER_PAGE_CHECK.

  WRITE : / SY-ULINE.
  W_PAGE = W_PAGE + 1.    W_LINE = 0.
  NEW-PAGE.

ENDFORM.                    " P2000_CONTAINER_PAGE_CHECK
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_ITEM_SUM_C
*&---------------------------------------------------------------------*
FORM P3000_WRITE_ITEM_SUM_C.

  WRITE: /30 'Case - - - - Items:', C_ORDER_ITEM.
  WRITE: /30 'Case - - - - Piece:', C_LFIMG UNIT IT_LIPS-MEINS.
  SKIP.

ENDFORM.                    " P3000_WRITE_ITEM_SUM_C
*&---------------------------------------------------------------------*
*&      Form  P3000_MOVE_CQTY_DATA
*&---------------------------------------------------------------------*
FORM P3000_MOVE_CQTY_DATA.

  C_ORDER_ITEM = 1.
  MOVE IT_LIPS-KDMAT    TO W_KDMAT.
  MOVE IT_LIPS-LFIMG    TO C_LFIMG. " Ship-Qty

ENDFORM.                    " P3000_MOVE_CQTY_DATA
*&---------------------------------------------------------------------*
*&      Form  P3000_MOVE_CQTY_DATA_1
*&---------------------------------------------------------------------*
FORM P3000_MOVE_CQTY_DATA_1.

  C_ORDER_ITEM = 1.
  MOVE IT_LIPS-KDMAT    TO W_KDMAT.
  MOVE IT_LIPS-LFIMG    TO C_LFIMG. " Ship-Qty

  W_FIRST = W_COUNT + 1.
  READ TABLE IT_LIPS INDEX W_FIRST.
  IF LIPS-KDMAT NE W_KDMAT.
    WRITE: /30 'Case - - - - Items:', C_ORDER_ITEM.
    WRITE: /30 'Case - - - - Piece:', C_LFIMG UNIT IT_LIPS-MEINS.
  ENDIF.
  SKIP.
  MOVE IT_LIPS-LFIMG TO C_LFIMG.
  READ TABLE IT_LIPS INDEX W_COUNT.

ENDFORM.                    " P3000_MOVE_CQTY_DATA_1
