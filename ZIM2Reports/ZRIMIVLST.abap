*&---------------------------------------------------------------------*
*& Report  ZRIMIVPRC2                                                  *
*&---------------------------------------------------------------------*
*&  Program    : Commercial Invoice List                               *
*&  Created by : Shinho, Na INFOLINK Ltd.                              *
*&  Created on : 2004.05.17                                            *
*&  Project    : HMMA                                                  *
*&---------------------------------------------------------------------*
*& Date        User     Transport    Description
*& 12/01/2004  100471   UD1K913044   Changed the porgram to calculate
*&                                 total values (routine "ITEM_DETAIL").
*&---------------------------------------------------------------------*
REPORT  ZRIMIVLST    MESSAGE-ID ZIM NO STANDARD PAGE HEADING
                     LINE-SIZE 148.

TABLES : MARA,       " Material Master Table..
         ZTCIVHD,    " Commercial Invoice Header Table..
         ZTCIVIT.    " Commercial Invoice Items Table..

DATA:   BEGIN OF IT_ZSCIVIT OCCURS 0.   ">> RETURN.
        INCLUDE STRUCTURE   ZTCIVIT.
DATA:   TOTWT    LIKE       ZTCIVIT-CMENGE,
        END   OF IT_ZSCIVIT.

DATA: W_TABIX      LIKE SY-TABIX,
      W_ERR_CHK    TYPE C,
      W_OK_CODE    LIKE SY-UCOMM,
      W_PAGE       TYPE I,                 " Page Counter..
      W_LINE       TYPE I,                 " Line Counter..
      W_COUNT      TYPE I.

DATA: W_EBELN      LIKE ZTCIVIT-EBELN,
      W_CMENGE     LIKE ZTCIVIT-CMENGE,
      W_MEINS      LIKE ZTCIVIT-MEINS,
      W_ZFIVAMP    LIKE ZTCIVIT-ZFIVAMP,
      W_ZFIVAMC    LIKE ZTCIVIT-ZFIVAMC,
      W_NETPR      LIKE ZTCIVIT-NETPR,
      W_PEINH      LIKE ZTCIVIT-PEINH,
      W_BPRME      LIKE ZTCIVIT-BPRME,
      W_ORDER_ITEM TYPE I.

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

  IF SY-SUBRC NE 0.
    MESSAGE S993.
    EXIT.
  ENDIF.

  SELECT * INTO TABLE IT_ZSCIVIT
           FROM ZTCIVIT
          WHERE ZFCIVRN = P_CIVRN.

  LOOP AT IT_ZSCIVIT.
    SELECT SINGLE *
             FROM MARA
            WHERE MATNR = IT_ZSCIVIT-MATNR.
    IT_ZSCIVIT-TOTWT = MARA-NTGEW * IT_ZSCIVIT-CMENGE.
    MODIFY IT_ZSCIVIT INDEX SY-TABIX.
  ENDLOOP.

ENDFORM.                    " P1000_READ_CIV_DATA
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_CIV_DATA
*&---------------------------------------------------------------------*
FORM P3000_WRITE_CIV_DATA.

  SORT IT_ZSCIVIT BY EBELN EBELP.
  W_PAGE = 1.     W_LINE = 0.     W_COUNT = 0.

  LOOP AT IT_ZSCIVIT.
    IF SY-TABIX EQ 1.
      PERFORM P3000_WRITE_ITEM_HEADER.
      PERFORM P3000_WRITE_ITEM_DETAIL.
      PERFORM P3000_MOVE_QTY_DATA.
    ELSE.
      IF W_EBELN = IT_ZSCIVIT-EBELN.
        ADD 1                   TO W_ORDER_ITEM.
        ADD IT_ZSCIVIT-CMENGE   TO W_CMENGE.
        ADD IT_ZSCIVIT-ZFIVAMP  TO W_ZFIVAMP.
        PERFORM P3000_WRITE_ITEM_DETAIL.
      ELSE.
        PERFORM P3000_WRITE_ITEM_SUM.
        PERFORM P3000_WRITE_ITEM_HEADER.
        PERFORM P3000_WRITE_ITEM_DETAIL.
        PERFORM P3000_MOVE_QTY_DATA.
      ENDIF.
      PERFORM P2000_PAGE_CHECK.
    ENDIF.
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
  WRITE: 'Commercial Invoice List' NO-GAP.
  WRITE: / 'Invoice No.: ' NO-GAP, ZTCIVHD-ZFCIVNO,
       106 'Page: ', W_PAGE.
  SKIP.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  p3000_write_item_header
*&---------------------------------------------------------------------*
FORM P3000_WRITE_ITEM_HEADER.

  WRITE: /'Order No.:', IT_ZSCIVIT-EBELN.
**S> 08/05/11 Paul : ECC6.0
*  WRITE: /'- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - '.
  WRITE (123)sy-uLINE.
**E<
  WRITE:/(05)'Order Seq.', (15)'Part-Number', (40)'Part-Name',
         (20)'Total Weight' CENTERED, (20)'Ship-Qty' CENTERED,
         (14)'Unit-Price' CENTERED, (24)'Amount Extended' CENTERED.

ENDFORM.                    " p3000_write_item_header
*&---------------------------------------------------------------------*
*&      Form  p3000_write_item_detail
*&---------------------------------------------------------------------*
FORM P3000_WRITE_ITEM_DETAIL.
  data: w_netpr type p decimals 3.

  w_netpr = IT_ZSCIVIT-NETPR / IT_ZSCIVIT-PEINH.

  WRITE: / IT_ZSCIVIT-EBELP(05),  (15)IT_ZSCIVIT-MATNR,
          (40)IT_ZSCIVIT-TXZ01,
          (15)IT_ZSCIVIT-TOTWT   UNIT MARA-GEWEI,
          (05)MARA-GEWEI,
          (15)IT_ZSCIVIT-CMENGE  UNIT IT_ZSCIVIT-MEINS,
          (05)IT_ZSCIVIT-MEINS,
*          (09)IT_ZSCIVIT-NETPR   CURRENCY IT_ZSCIVIT-BPRME,
          (09)w_netpr,
          (05)IT_ZSCIVIT-BPRME,
          (19)IT_ZSCIVIT-ZFIVAMP CURRENCY IT_ZSCIVIT-ZFIVAMC,
          (05)IT_ZSCIVIT-ZFIVAMC.

ENDFORM.                    " p3000_write_item_detail
*&---------------------------------------------------------------------*
*&      Form  p3000_move_qty_data
*&---------------------------------------------------------------------*
FORM P3000_MOVE_QTY_DATA.

  W_ORDER_ITEM = 1.
  MOVE IT_ZSCIVIT-EBELN    TO W_EBELN.
  MOVE IT_ZSCIVIT-CMENGE   TO W_CMENGE.   " Ship-Qty
  MOVE IT_ZSCIVIT-MEINS    TO W_MEINS.
  MOVE IT_ZSCIVIT-ZFIVAMP  TO W_ZFIVAMP.  " Amount Extended.
  MOVE IT_ZSCIVIT-ZFIVAMC  TO W_ZFIVAMC.
  MOVE IT_ZSCIVIT-NETPR    TO W_NETPR.    " Unit Price.
  MOVE IT_ZSCIVIT-PEINH    TO W_PEINH.
  MOVE IT_ZSCIVIT-BPRME    TO W_BPRME.

ENDFORM.                    " p3000_move_qty_data
*&---------------------------------------------------------------------*
*&      Form  p3000_write_item_sum
*&---------------------------------------------------------------------*
FORM P3000_WRITE_ITEM_SUM.

  SKIP.
  WRITE: /30 'Order Total Items:', W_ORDER_ITEM.
  WRITE: /30 'Order Total FOB:  ',
             W_CMENGE UNIT W_MEINS, W_MEINS.
  WRITE: /30 'Order Total Price:',
             W_ZFIVAMP CURRENCY W_ZFIVAMC, W_ZFIVAMC.

ENDFORM.                    " p3000_write_item_sum
*&---------------------------------------------------------------------*
*&      Form  p3000_write_grand_total
*&---------------------------------------------------------------------*
FORM P3000_WRITE_GRAND_TOTAL.

  SKIP.
  CLEAR: W_ORDER_ITEM, W_ZFIVAMP, W_CMENGE.

  SELECT COUNT(*)
         INTO W_ORDER_ITEM
         FROM ZTCIVIT
        WHERE ZFCIVRN = P_CIVRN.

  SELECT SUM( ZFIVAMP )
         INTO W_ZFIVAMP
         FROM ZTCIVIT
        WHERE ZFCIVRN = P_CIVRN.

  SELECT SUM( CMENGE )
         INTO W_CMENGE
         FROM ZTCIVIT
        WHERE ZFCIVRN = P_CIVRN.

  WRITE: /30 'Order Grand Total Items:', W_ORDER_ITEM.
  WRITE: /30 'Order Grand Total Qty:  ',
             W_CMENGE UNIT W_MEINS, W_MEINS.
  WRITE: /30 'Order Grand Total Price:',
             W_ZFIVAMP CURRENCY W_ZFIVAMC, W_ZFIVAMC.

ENDFORM.                    " p3000_write_grand_total
*&---------------------------------------------------------------------*
*&      Form  P2000_PAGE_CHECK
*&---------------------------------------------------------------------*
FORM P2000_PAGE_CHECK.

  IF W_LINE >= 53.
    WRITE : / SY-ULINE.
    W_PAGE = W_PAGE + 1.    W_LINE = 0.
    NEW-PAGE.
  ENDIF.

ENDFORM.                    " P2000_PAGE_CHECK
