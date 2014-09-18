*&---------------------------------------------------------------------*
*& Report  ZRIMCPKLST                                                  *
*&---------------------------------------------------------------------*
*&  Program    : Packing List for Customs Clearance                    *
*&  Created on : Shinho, Na INFOLINK Ltd.                              *
*&  Created by : 2004.05.17                                            *
*&  Project    : HMMA                                                  *
*&---------------------------------------------------------------------*
*& Date        User     Transport    Description
*& 12/01/2004  100471   UD1K913044   Changed the porgram to calculate
*&                                    total values.
*&---------------------------------------------------------------------*
REPORT  ZRIMCPKLST   MESSAGE-ID ZIM NO STANDARD PAGE HEADING
                     LINE-SIZE 220.

TABLES : LIKP,       " SD Document: Delivery Header Data.
         LIPS,       " SD document: Delivery Item data.
         MARA,       " Material Master..
         ZTCIVHD,    " Commercial Invoice Header Table..
         ZTBLIT,     " Bill of Lading Items Table..
         ZTCIVIT,    " Commercial Invoice Items Table..
         ZTIMIMG24.  " MID Code Management...

DATA: BEGIN OF IT_LIPS OCCURS 0,
        BLMENGE  LIKE  ZTBLIT-BLMENGE,      " B/L Amount..
        BLMEINS  LIKE  ZTBLIT-MEINS,        " Unit
        NETPR    LIKE  ZTCIVIT-NETPR,       " Unit Price.
        peinh    like  ztcivit-peinh,       " Price unit.
        BPRME    LIKE  ZTCIVIT-BPRME,       " UOM of Unit Price..
        ZFIVAMP  LIKE  ZTCIVIT-ZFIVAMP,     " Shipped Quantity..
        ZFIVAMC  LIKE  ZTCIVIT-ZFIVAMC,     " Shipped currency..
        ZFMID    LIKE  ZTIMIMG24-ZFMID,     " MID..
        STAWN    LIKE  ZTBLIT-STAWN,        " H/S Code..
        BOLNR    LIKE  LIKP-BOLNR,          " Invoice No.
        TRAID    LIKE  LIKP-TRAID,          " Container No.
        KDMAT    LIKE  LIPS-KDMAT,          " Case No.
        POSNR    LIKE  LIPS-POSNR,          " Delivery Item No.
        VGBEL    LIKE  LIPS-VGBEL,          " P/O No.
        VGPOS    LIKE  LIPS-VGPOS,          " P/O Item No.
        MATNR    LIKE  LIPS-MATNR,          " Material No.
        ARKTX    LIKE  LIPS-ARKTX,          " Material Name.
        LFIMG    LIKE  LIPS-LFIMG,          " Qty In case..
        MEINS    LIKE  LIPS-MEINS,          " UOM of LFIMG..
        TOTWT    LIKE  ZTCIVIT-CMENGE,      " Net weight..
        GEWEI    LIKE  MARA-GEWEI,          " Weight Unit..
      END OF IT_LIPS.

DATA: W_TABIX      LIKE SY-TABIX,
      W_ERR_CHK    TYPE C,
      W_OK_CODE    LIKE SY-UCOMM,
      W_PAGE       TYPE I,
      W_COUNT      TYPE I,
      W_LINE       TYPE I.

DATA: W_EBELN      LIKE LIPS-VGBEL,
      W_BLMENGE    LIKE ZTBLIT-BLMENGE,
      W_BLMEINS    LIKE ZTBLIT-MEINS,
      W_ZFIVAMP    LIKE ZTCIVIT-ZFIVAMP,
      W_ZFIVAMC    LIKE ZTCIVIT-ZFIVAMC,
      W_STAWN      LIKE ZTBLIT-STAWN,
      W_TRAID      LIKE LIKP-TRAID,
      W_LFIMG      LIKE LIPS-LFIMG,
      P_LFIMG      LIKE LIPS-LFIMG,
      W_ORDER_ITEM TYPE I,
      P_ORDER_ITEM TYPE I.

*-----------------------------------------------------------------------
* Selection Window.
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
            SELECT SINGLE *
                     FROM ZTCIVIT
                    WHERE ZFCIVRN = P_CIVRN
                      AND EBELN   = IT_LIPS-VGBEL
                      AND EBELP   = IT_LIPS-VGPOS.
            IF SY-SUBRC EQ 0.
              MOVE ZTCIVIT-NETPR   TO IT_LIPS-NETPR.
              move ztcivit-peinh   to it_lips-peinh.
              MOVE ZTCIVIT-BPRME   TO IT_LIPS-BPRME.
              MOVE ZTCIVIT-ZFIVAMP TO IT_LIPS-ZFIVAMP.
              MOVE ZTCIVIT-ZFIVAMC TO IT_LIPS-ZFIVAMC.
              SELECT SINGLE *
                       FROM ZTBLIT
                      WHERE ZFBLNO = ZTCIVIT-ZFBLNO
                        AND ZFBLIT = ZTCIVIT-ZFBLIT.
              IF SY-SUBRC EQ 0.
                MOVE ZTBLIT-STAWN   TO IT_LIPS-STAWN.
                MOVE ZTBLIT-BLMENGE TO IT_LIPS-BLMENGE.
                MOVE ZTBLIT-MEINS   TO IT_LIPS-BLMEINS.
              ENDIF.
              APPEND IT_LIPS.
            ENDIF.
          ENDIF.
        ENDSELECT.
      ENDIF.
    ENDSELECT.
  ENDIF.

  SORT IT_LIPS BY STAWN MATNR.
  delete adjacent duplicates from it_lips comparing stawn matnr.

  LOOP AT IT_LIPS.
    SELECT SINGLE ZFMID
             INTO IT_LIPS-ZFMID
             FROM ZTIMIMG24
            WHERE MATNR = IT_LIPS-MATNR.
    SELECT SINGLE *
             FROM MARA
            WHERE MATNR = IT_LIPS-MATNR.
    IT_LIPS-TOTWT = IT_LIPS-BLMENGE * MARA-NTGEW.
    MODIFY IT_LIPS INDEX SY-TABIX.
  ENDLOOP.

ENDFORM.                    " P1000_READ_CIV_DATA
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_CIV_DATA
*&---------------------------------------------------------------------*
FORM P3000_WRITE_CIV_DATA.

*  SORT IT_LIPS BY STAWN MATNR.
  W_PAGE = 1.     W_LINE = 0.     W_COUNT = 0.
  LOOP AT IT_LIPS.
    IF SY-TABIX EQ 1.
      PERFORM P3000_WRITE_ITEM_HEADER.
      PERFORM P3000_WRITE_ITEM_DETAIL.
      PERFORM P3000_MOVE_QTY_DATA.
    ELSE.
      IF W_STAWN = IT_LIPS-STAWN.
        ADD 1                   TO W_ORDER_ITEM.
        ADD IT_LIPS-BLMENGE     TO W_BLMENGE.
        ADD IT_LIPS-ZFIVAMP     TO W_ZFIVAMP.
        ADD IT_LIPS-LFIMG       TO W_LFIMG.
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
  WRITE:  55 'Hyundai Motor Manufacturing Alabama LLC'.
  WRITE: /60 'Invoice and Packing List Detail'.
  SKIP.
  WRITE: / 'Invoice No.:', ZTCIVHD-ZFCIVNO, 140 'Page: ', W_PAGE.
  SKIP.
  WRITE: (12)'Part No.', (16)'MID', (40)'Part Name',
         (10)'P/O No.', (05)'Seq.', (15)'Shipped Qty' centered,
         (15)'TTL NET WGT KG' CENTERED,
         (13)'Unit Price' centered, (22)'Extended' centered,
         (20)'Container No.', (20)'Case No.',
         (20)'Qty in case' centered.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  p3000_write_item_header
*&---------------------------------------------------------------------*
FORM P3000_WRITE_ITEM_HEADER.
  SKIP.
  WRITE: 'HTS:', IT_LIPS-STAWN.
  WRITE: /'- - - - - - - - - - - - - - - - - - - - - - - - - - '.
ENDFORM.                    " p3000_write_item_header
*&---------------------------------------------------------------------*
*&      Form  p3000_write_item_detail
*&---------------------------------------------------------------------*
FORM P3000_WRITE_ITEM_DETAIL.

  data: w_netpr type p decimals 3.
  w_netpr = IT_LIPS-NETPR / it_lips-peinh.

  WRITE: / IT_LIPS-MATNR(12), IT_LIPS-ZFMID(16), IT_LIPS-ARKTX(40),
           IT_LIPS-VGBEL(10), IT_LIPS-VGPOS+1(05),
           (10)IT_LIPS-BLMENGE UNIT IT_LIPS-BLMEINS NO-GAP,
           (05)IT_LIPS-BLMEINS,
           (10)IT_LIPS-TOTWT UNIT MARA-GEWEI NO-GAP, MARA-GEWEI,
*           (10)IT_LIPS-NETPR NO-GAP CURRENCY IT_LIPS-BPRME,
           (10)w_netpr,
           (03)IT_LIPS-BPRME,
           (19)IT_LIPS-ZFIVAMP NO-GAP CURRENCY IT_LIPS-ZFIVAMC,
           (03)IT_LIPS-ZFIVAMC,
           IT_LIPS-TRAID(20),      IT_LIPS-KDMAT(20),
           (15)IT_LIPS-LFIMG NO-GAP UNIT IT_LIPS-MEINS,
           (05)IT_LIPS-MEINS.

ENDFORM.                    " p3000_write_item_detail
*&---------------------------------------------------------------------*
*&      Form  p3000_move_qty_data
*&---------------------------------------------------------------------*
FORM P3000_MOVE_QTY_DATA.

  W_ORDER_ITEM = 1.
  MOVE IT_LIPS-TRAID    TO W_TRAID.
  MOVE IT_LIPS-STAWN    TO W_STAWN.
  MOVE IT_LIPS-BLMENGE  TO W_BLMENGE.  " Ship-Qty..
  MOVE IT_LIPS-BLMEINS  TO W_BLMEINS.
  MOVE IT_LIPS-ZFIVAMP  TO W_ZFIVAMP.  " Extended..
  MOVE IT_LIPS-ZFIVAMC  TO W_ZFIVAMC.
  MOVE IT_LIPS-LFIMG    TO W_LFIMG.    " Qty in case..

ENDFORM.                    " p3000_move_qty_data
*&---------------------------------------------------------------------*
*&      Form  p3000_write_item_sum
*&---------------------------------------------------------------------*
FORM P3000_WRITE_ITEM_SUM.

  SKIP.
  WRITE: /30 'Total Part Count:', W_ORDER_ITEM.
  WRITE: /30 'Total Ship Qty  :',
             W_BLMENGE UNIT W_BLMEINS, W_BLMEINS.
  WRITE: /30 'Total FOB       :',
             W_ZFIVAMP CURRENCY W_ZFIVAMC, W_ZFIVAMC.

ENDFORM.                    " p3000_write_item_sum
*&---------------------------------------------------------------------*
*&      Form  p3000_write_grand_total
*&---------------------------------------------------------------------*
FORM P3000_WRITE_GRAND_TOTAL.

  SKIP.
  CLEAR: W_ORDER_ITEM, W_BLMENGE, W_ZFIVAMP.

  LOOP AT IT_LIPS.
    ADD 1               TO W_ORDER_ITEM.
    ADD IT_LIPS-BLMENGE TO W_BLMENGE.
    ADD IT_LIPS-ZFIVAMP TO W_ZFIVAMP.
  ENDLOOP.

  WRITE: /30 'Grand Total Part Count:', W_ORDER_ITEM.
  WRITE: /30 'Grand Total Ship Qty  :',
             W_BLMENGE UNIT W_BLMEINS, W_BLMEINS.
  WRITE: /30 'Grand Total FOB       :',
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
