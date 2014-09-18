*&---------------------------------------------------------------------*
*& Report  ZRIMCIVPRT                                                  *
*&---------------------------------------------------------------------*
*&  Program    : Shipping Document (Commercial Invoice)                *
*&  Created by : Na, shinho INFOLINK LTD.                              *
*&  Created on : 2004.04.29                                            *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&---------------------------------------------------------------------*
REPORT  ZRIMCIVPRT MESSAGE-ID ZIM NO STANDARD PAGE HEADING
                   LINE-SIZE 117.

TABLES: LFA1,    ZTBL,      ZTBLIT,    ZTCIVHD,
        ZTCIVIT, ZTIMIMG00, ZTIMIMG24, ZTIMIMGTX.

DATA:   BEGIN OF IT_CIVIT OCCURS 0.
        INCLUDE STRUCTURE   ZTCIVIT.
DATA:   ZFMID LIKE ZTIMIMG24-ZFMID,
        END   OF IT_CIVIT.

DATA:   W_ZFIVAMP LIKE ZTCIVIT-ZFIVAMP,
        W_LINE    TYPE I.

DATA: P_BUKRS LIKE ZTREQHD-BUKRS.

*-----------------------------------------------------------------------
* Selection Windows.
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.

PARAMETERS: P_CIVRN LIKE ZTCIVHD-ZFCIVRN OBLIGATORY.

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

*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_CIV_DATA
*&---------------------------------------------------------------------*
FORM P1000_READ_CIV_DATA.

  SELECT SINGLE *
           FROM ZTCIVHD
          WHERE ZFCIVRN EQ P_CIVRN.

  SELECT * INTO TABLE IT_CIVIT
           FROM ZTCIVIT
          WHERE ZFCIVRN EQ P_CIVRN.

  READ TABLE IT_CIVIT INDEX 1.

  SELECT SINGLE *
           FROM ZTBL
          WHERE ZFBLNO EQ IT_CIVIT-ZFBLNO.

  SELECT SINGLE *
           FROM ZTIMIMGTX
          WHERE BUKRS = ZTCIVHD-BUKRS.
  LOOP AT IT_CIVIT.
    SELECT SINGLE ZFMID
             INTO IT_CIVIT-ZFMID
             FROM ZTIMIMG24
            WHERE MATNR = IT_CIVIT-MATNR.
    MODIFY IT_CIVIT INDEX SY-TABIX.
  ENDLOOP.

ENDFORM.                    " P1000_READ_CIV_DATA
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_CIV_DATA
*&---------------------------------------------------------------------*
FORM P3000_WRITE_CIV_DATA.

  WRITE: 40 'COMMERCIAL INVOICE'.

  SKIP.
  WRITE: SY-ULINE(117).
  WRITE: / SY-VLINE NO-GAP, 'Shipper/Exporter' NO-GAP,
        68 SY-VLINE NO-GAP, 'Invoice Number & Date:' NO-GAP,
           ZTCIVHD-ZFCIDT NO-GAP,
       117 SY-VLINE NO-GAP.

  SELECT SINGLE *
           FROM LFA1
          WHERE LIFNR = ZTBL-LIFNR.

  WRITE: / SY-VLINE NO-GAP, LFA1-NAME1 NO-GAP,
        68 SY-VLINE NO-GAP, ZTCIVHD-ZFCIVNO,
       117 SY-VLINE NO-GAP.

  WRITE: / SY-VLINE NO-GAP, LFA1-NAME2 NO-GAP,
        68 SY-VLINE NO-GAP, SY-ULINE(49) NO-GAP,
       117 SY-VLINE NO-GAP.

  WRITE: / SY-VLINE NO-GAP, LFA1-NAME3 NO-GAP,
        68 SY-VLINE NO-GAP, 'L/C number:' NO-GAP,
       117 SY-VLINE NO-GAP.

  WRITE: / SY-VLINE NO-GAP, 'address     :' NO-GAP, LFA1-ADRNR NO-GAP,
        68 SY-VLINE NO-GAP, ZTBL-ZFOPNNO NO-GAP,
       117 SY-VLINE NO-GAP.

  WRITE: / SY-VLINE NO-GAP, 'city/country:' NO-GAP,
           LFA1-ORT01 NO-GAP, LFA1-LAND1 NO-GAP,
        68 SY-VLINE NO-GAP, SY-ULINE(49) NO-GAP,
       117 SY-VLINE NO-GAP.

  WRITE: / SY-VLINE NO-GAP, 'zipcode     :' NO-GAP, LFA1-PSTLZ NO-GAP,
        68 SY-VLINE NO-GAP, 'Bank:' NO-GAP, LFA1-NAME1 NO-GAP,
       117 SY-VLINE NO-GAP.

  WRITE: / SY-VLINE NO-GAP,
        68 SY-VLINE NO-GAP,
       117 SY-VLINE NO-GAP.

  WRITE: / SY-VLINE NO-GAP, SY-ULINE(67) NO-GAP,
        68 SY-VLINE NO-GAP,
       117 SY-VLINE NO-GAP.

  WRITE: / SY-VLINE NO-GAP, 'Importer/Consignee' NO-GAP,
        68 SY-VLINE NO-GAP,
       117 SY-VLINE NO-GAP.

  WRITE: / SY-VLINE NO-GAP,
        68 SY-VLINE NO-GAP,
       117 SY-VLINE NO-GAP.

  WRITE: / SY-VLINE NO-GAP, ZTIMIMGTX-ZFAPPAD1 NO-GAP,
        68 SY-VLINE NO-GAP, SY-ULINE(49) NO-GAP,
       117 SY-VLINE NO-GAP.

  WRITE: / SY-VLINE NO-GAP, ZTIMIMGTX-ZFAPPAD2 NO-GAP,
        68 SY-VLINE NO-GAP, 'Remarks:' NO-GAP,
       117 SY-VLINE NO-GAP.

  WRITE: / SY-VLINE NO-GAP, ZTIMIMGTX-ZFAPPAD3 NO-GAP,
        68 SY-VLINE NO-GAP,
           'For Direct Delivery to Sub Zone# 222A' NO-GAP,
       117 SY-VLINE NO-GAP.

  WRITE: / SY-VLINE NO-GAP,
        68 SY-VLINE NO-GAP, 'of HMMA Foreigh Trade Zone in' NO-GAP,
       117 SY-VLINE NO-GAP.

  WRITE: / SY-VLINE NO-GAP, SY-ULINE(67) NO-GAP,
        68 SY-VLINE NO-GAP, 'Montgomery, AL 36105' NO-GAP,
       117 SY-VLINE NO-GAP.

  WRITE: / SY-VLINE NO-GAP, 'Notify Party' NO-GAP,
        68 SY-VLINE NO-GAP,
       117 SY-VLINE NO-GAP.

  WRITE: / SY-VLINE NO-GAP, 'MS Kelly Walb' NO-GAP,
        68 SY-VLINE NO-GAP,
       117 SY-VLINE NO-GAP.

  WRITE: / SY-VLINE NO-GAP,
           'Hyundai Motor Manufacturing Alabama, LLC' NO-GAP,
        68 SY-VLINE NO-GAP,
       117 SY-VLINE NO-GAP.

  WRITE: / SY-VLINE NO-GAP,
           '700 HYUNDAI VULD Montgomery AL36/05' NO-GAP,
        68 SY-VLINE NO-GAP,
       117 SY-VLINE NO-GAP.

  WRITE: / SY-VLINE NO-GAP, 'PH: 334-387-8464' NO-GAP,
        68 SY-VLINE NO-GAP,
       117 SY-VLINE NO-GAP.

  WRITE: / SY-VLINE NO-GAP, 'FX: 334-387-8904' NO-GAP,
        68 SY-VLINE NO-GAP,
       117 SY-VLINE NO-GAP.

  WRITE: / SY-VLINE NO-GAP,
        68 SY-VLINE NO-GAP,
       117 SY-VLINE NO-GAP.

  WRITE: / SY-ULINE(117) NO-GAP.

  WRITE: / SY-VLINE NO-GAP, 'Port of loading' NO-GAP,
        30 SY-VLINE NO-GAP, 'Final Destination' NO-GAP,
        68 SY-VLINE NO-GAP, 'Terms' NO-GAP,
       117 SY-VLINE NO-GAP.

  WRITE: / SY-VLINE NO-GAP, ZTBL-ZFSPRTC, ZTBL-ZFSPRT NO-GAP,
        30 SY-VLINE NO-GAP, ZTBL-ZFAPRTC, ZTBL-ZFAPRT NO-GAP,
        68 SY-VLINE NO-GAP,
       117 SY-VLINE NO-GAP.

  WRITE: / SY-VLINE NO-GAP, SY-ULINE(67) NO-GAP,
        30 SY-VLINE NO-GAP,
       117 SY-VLINE NO-GAP.

  WRITE: / SY-VLINE NO-GAP, 'Carrier/Vessel' NO-GAP,
        30 SY-VLINE NO-GAP, 'Date of sailing' NO-GAP,
        68 SY-VLINE NO-GAP,
       117 SY-VLINE NO-GAP.

  WRITE: / SY-VLINE NO-GAP, ZTBL-ZFCARNM NO-GAP,
        30 SY-VLINE NO-GAP, ZTBL-ZFETD NO-GAP,
        68 SY-VLINE NO-GAP,
       117 SY-VLINE NO-GAP.

  WRITE: / SY-VLINE NO-GAP, SY-ULINE(116) NO-GAP,
       117 SY-VLINE NO-GAP.

  WRITE: / SY-VLINE NO-GAP, 'Marks & No.' NO-GAP,
        15 'Part No.   Part Name' NO-GAP, 54 'Quantity' NO-GAP,
        74 'Unit Price' NO-GAP, 93 'Total Value' NO-GAP,
       117 SY-VLINE NO-GAP.

  WRITE: / SY-VLINE NO-GAP, 'HMMA' NO-GAP,
        15 'Commodity' NO-GAP, 35 'MID' NO-GAP, 117 SY-VLINE NO-GAP.

  DESCRIBE TABLE IT_CIVIT LINES W_LINE.

  LOOP AT IT_CIVIT.
    IF W_LINE GT 5.
      IF SY-TABIX EQ 1.
        PERFORM P3000_WRITE_ITEM.
      ENDIF.
    ELSE.
      IF SY-TABIX EQ 1.
        WRITE: / SY-VLINE NO-GAP, 'Montgomery' NO-GAP.
      ELSE.
        WRITE: / SY-VLINE NO-GAP.
      ENDIF.
      IT_CIVIT-NETPR = IT_CIVIT-NETPR / IT_CIVIT-PEINH.

      WRITE: 15 IT_CIVIT-MATNR, 25 IT_CIVIT-TXZ01(29) NO-GAP,
             54 IT_CIVIT-CMENGE UNIT IT_CIVIT-MEINS NO-GAP,
                IT_CIVIT-MEINS NO-GAP,
             74 IT_CIVIT-NETPR CURRENCY IT_CIVIT-BPRME NO-GAP,
                IT_CIVIT-BPRME NO-GAP,
             93 IT_CIVIT-ZFIVAMP CURRENCY IT_CIVIT-ZFIVAMC NO-GAP,
                IT_CIVIT-ZFIVAMC NO-GAP,
            117 SY-VLINE NO-GAP.
      IF SY-TABIX EQ 1.
        WRITE:  / SY-VLINE NO-GAP, 'FTZ 222A' NO-GAP, 15 IT_CIVIT-STAWN,
               35 IT_CIVIT-ZFMID, 117 SY-VLINE NO-GAP.
      ELSE.
        WRITE:  / SY-VLINE NO-GAP, 15 IT_CIVIT-STAWN,
                 35 IT_CIVIT-ZFMID, 117 SY-VLINE NO-GAP.
      ENDIF.
      WRITE:  / SY-VLINE NO-GAP, 117 SY-VLINE NO-GAP.
    ENDIF.
    ADD IT_CIVIT-ZFIVAMP TO W_ZFIVAMP.
  ENDLOOP.


  WRITE: / SY-VLINE NO-GAP, 117 SY-VLINE NO-GAP.
  WRITE: / SY-VLINE NO-GAP, 117 SY-VLINE NO-GAP.
  WRITE: / SY-VLINE NO-GAP, 117 SY-VLINE NO-GAP.
  WRITE: / SY-VLINE NO-GAP, 80 'FOB amount $ -',
           W_ZFIVAMP CURRENCY IT_CIVIT-ZFIVAMC NO-GAP,
       117 SY-VLINE NO-GAP.
  WRITE: / SY-VLINE NO-GAP, 80 '   Freight $ -', 117 SY-VLINE NO-GAP.
  WRITE: / SY-VLINE NO-GAP, 80 ' Insurance $ -', 117 SY-VLINE NO-GAP.
  WRITE: / SY-VLINE NO-GAP, 80 ' Total CIF $ -', 117 SY-VLINE NO-GAP.
  WRITE: / SY-ULINE NO-GAP.
*  WRITE: / SY-VLINE NO-GAP, SY-ULINE(116), 117 SY-VLINE NO-GAP.

ENDFORM.                    " P3000_WRITE_CIV_DATA
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_ITEM
*&---------------------------------------------------------------------*
FORM P3000_WRITE_ITEM.

  WRITE: / SY-VLINE NO-GAP, 'Montgomery' NO-GAP,
       117 SY-VLINE NO-GAP.
  WRITE: / SY-VLINE NO-GAP, 'FTZ 222A' NO-GAP,
       117 SY-VLINE NO-GAP.
  WRITE: / SY-VLINE NO-GAP, 117 SY-VLINE NO-GAP.
  WRITE: / SY-VLINE NO-GAP, 117 SY-VLINE NO-GAP.
  WRITE: / SY-VLINE NO-GAP, 50 'Item Attached' NO-GAP,
       117 SY-VLINE NO-GAP.
  WRITE: / SY-VLINE NO-GAP, 117 SY-VLINE NO-GAP.
  WRITE: / SY-VLINE NO-GAP, 117 SY-VLINE NO-GAP.

ENDFORM.                    " P3000_WRITE_ITEM
