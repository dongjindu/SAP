REPORT ZF_MISS_MM.


* Tables ---------------------------------------------------------------
TABLES: T001, SKB1, BKPF, BSEG, BSAS, MKPF.

* Type declarations ----------------------------------------------------
TYPES: BEGIN OF KONTLIST,
        BUKRS   LIKE BSEG-BUKRS,
        GJAHR   LIKE BSEG-GJAHR,
        HKONT   LIKE BSEG-HKONT,
        SHKZG   LIKE BSEG-SHKZG,
        DMBTR   LIKE BSEG-DMBTR,
      END OF KONTLIST.

* DATA declarations ----------------------------------------------------
DATA: XBKPF LIKE BKPF,
      XMKPF LIKE MKPF,
      XT001 LIKE T001.

DATA: P_BUKRS_OLD LIKE T001-BUKRS.

DATA: BEGIN OF TBSEG OCCURS 0,
        BUKRS LIKE BSEG-BUKRS,
        GJAHR LIKE BSEG-GJAHR,
        BELNR LIKE BSEG-BELNR,
        HKONT LIKE BSEG-HKONT,
        SHKZG LIKE BSEG-SHKZG,
        DMBTR LIKE BSEG-DMBTR,
      END OF TBSEG.

DATA: AWREF LIKE ACCHD-AWREF,
      AWORG LIKE ACCHD-AWORG.

DATA: HKONT_LIST type sorted table of KONTLIST
                 with unique key BUKRS GJAHR HKONT SHKZG
                 with header line.

DATA: INDEX_R    LIKE SY-INDEX VALUE 0,
      SAVE_SUBRC LIKE SY-SUBRC,
      XARCH      LIKE BSAS-XARCH.


* Selection criteria ---------------------------------------------------
selection-screen begin of block 001 with frame title text_001.

 selection-screen begin of line.
    selection-screen comment 1(18) text_011 for field COMPANY.
    selection-screen position 20.
    select-options: COMPANY for bkpf-bukrs memory id buk.
    selection-screen end of line.

 selection-screen begin of line.
    selection-screen comment 1(18) text_012 for field FYEAR.
    selection-screen position 20.
    select-options: FYEAR for bkpf-gjahr memory id gjr.
    selection-screen end of line.

 selection-screen begin of line.
    selection-screen comment 1(18) text_013 for field PERIOD.
    selection-screen position 20.
    select-options: PERIOD for bkpf-monat memory id per.
    selection-screen end of line.

 selection-screen begin of line.
    selection-screen comment 1(18) text_014 for field DOCUMENT.
    selection-screen position 20.
    select-options: DOCUMENT for bkpf-belnr memory id bln.
    selection-screen end of line.

 selection-screen begin of line.
    selection-screen comment 1(18) text_015 for field ACCOUNT.
    selection-screen position 20.
    select-options: ACCOUNT for bseg-hkont memory id hkt.
    selection-screen end of line.

selection-screen end of block 001.

* Initialization -------------------------------------------------------
initialization.

* Selection texts ------------------------------------------------------
text_001 = 'Selection criteria'.
text_011 = 'Company Code'.
text_012 = 'Fiscal Year'.
text_013 = 'Period'.
text_014 = 'FI document number'.
text_015 = 'G/L Account'.


*-----------------------------------------------------------------------
*  AT SELECTION-SCREEN
*-----------------------------------------------------------------------
AT SELECTION-SCREEN ON COMPANY.
PERFORM SELECTION_1.

AT SELECTION-SCREEN ON ACCOUNT.
PERFORM SELECTION_2.


*-----------------------------------------------------------------------
*  START-OF-SELECTION
*-----------------------------------------------------------------------
START-OF-SELECTION.


* Loop at BKPF to select relevant FI data ------------------------------
SELECT * FROM  BKPF
         INTO  XBKPF
         WHERE BUKRS IN COMPANY[]
           AND BELNR IN DOCUMENT[]
           AND GJAHR IN FYEAR[]
           AND MONAT IN PERIOD[]
           AND BSTAT = ' '
           AND AWTYP = 'MKPF '.

CHECK NOT XBKPF IS INITIAL.

* Store links to MM data -----------------------------------------------
AWREF    = XBKPF-AWKEY(10).
AWORG(4) = XBKPF-AWKEY+10(4).

* Is corresponding MM document header missing online? ------------------
  XARCH = ' '.
  SELECT SINGLE * FROM  MKPF
                  INTO  XMKPF
                  WHERE MBLNR = AWREF
                    AND MJAHR = AWORG(4).

  SAVE_SUBRC = SY-SUBRC.

* Check for MM document header being archived --------------------------
  IF SAVE_SUBRC <> 0.
    SELECT SINGLE * FROM  MKPF_ARIDX
                    INTO  XMKPF
                    WHERE MBLNR = AWREF
                      AND MJAHR = AWORG(4).

    IF SY-SUBRC = 0.
      XARCH = 'X'.
    ENDIF.

  ENDIF.

* If MKPF is missing online, check if account is relevant at all -------
  IF SAVE_SUBRC <> 0.
    SELECT  BUKRS BELNR GJAHR SHKZG DMBTR HKONT
             FROM  BSEG
             INTO CORRESPONDING FIELDS OF TABLE TBSEG
             WHERE BUKRS =  XBKPF-BUKRS
               AND BELNR =  XBKPF-BELNR
               AND GJAHR =  XBKPF-GJAHR
               AND HKONT IN ACCOUNT[].

    IF SY-SUBRC = 0.
      ADD 1 TO INDEX_R.

* Write header for output list -----------------------------------------
      IF INDEX_R = 1.
        PERFORM WRITE_HEADER.
      ENDIF.

* Write document information into output list --------------------------
      PERFORM WRITE_LINES.

* Calculate accumulated local currency amounts -------------------------
      IF XARCH = ' '.
        PERFORM ACCOUNT_SUMMARY.
      ENDIF.
    ENDIF.

  ENDIF.

ENDSELECT.

* Write job statistics and account summary list ------------------------
PERFORM WRITE_STATISTICS.


*-----------------------------------------------------------------------
* FORM  SELECTION_1
* check company code
*-----------------------------------------------------------------------
FORM SELECTION_1.

LOOP AT COMPANY.
  SELECT SINGLE * FROM T001
                     WHERE bukrs = COMPANY-low.
  IF SY-SUBRC <> 0.
     MESSAGE ID '62' TYPE 'E' NUMBER '468' with COMPANY-low.
  ENDIF.
ENDLOOP.

ENDFORM.

*-----------------------------------------------------------------------
* FORM  SELECTION_2
* check account
*-----------------------------------------------------------------------
FORM SELECTION_2.

LOOP AT ACCOUNT.
  SELECT SINGLE * FROM SKB1
                    where bukrs in COMPANY[]
                      and saknr =  ACCOUNT-low.
  IF SY-SUBRC <> 0.
    MESSAGE ID '5B' TYPE 'E' NUMBER '751' with ACCOUNT-low.
  ENDIF.
ENDLOOP.

ENDFORM.


*-----------------------------------------------------------------------
* FORM  WRITE_HEADER
* header data for output list
*-----------------------------------------------------------------------
FORM WRITE_HEADER.

  SKIP.

  FORMAT COLOR COL_NORMAL OFF.
  WRITE: / 'SEARCH FI DOCUMENTS FOR MISSING MATERIAL DOCUMENTS'.

  SKIP 1.

  ULINE AT /0(57) NO-GAP.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE: /  sy-vline no-gap,
            (5) 'BUKRS'      left-justified  no-gap, sy-vline no-gap,
            (5) 'GJAHR'      left-justified  no-gap, sy-vline no-gap,
            (5) 'MONAT'      left-justified  no-gap, sy-vline no-gap,
           (10) 'BELNR'      left-justified  no-gap, sy-vline no-gap,
           (10) 'MBLNR'      left-justified  no-gap, sy-vline no-gap,
            (5) 'MJAHR'      left-justified  no-gap, sy-vline no-gap,
            (9) 'ARCH_INDX'  left-justified  no-gap, sy-vline no-gap.
  FORMAT COLOR OFF.
  ULINE AT /0(57) NO-GAP.

ENDFORM.


*-----------------------------------------------------------------------
* FORM  WRITE_LINES
* document data for output list
*-----------------------------------------------------------------------
FORM WRITE_LINES.

  WRITE: /    sy-vline no-gap,
        (5) XBKPF-BUKRS   COLOR COL_KEY    INTENSIFIED ON  no-gap,
              sy-vline no-gap,
        (5) XBKPF-GJAHR   COLOR COL_KEY    INTENSIFIED ON  no-gap,
              sy-vline no-gap,
        (5) XBKPF-MONAT   COLOR COL_NORMAL INTENSIFIED OFF no-gap,
              sy-vline no-gap,
       (10) XBKPF-BELNR   COLOR COL_NORMAL INTENSIFIED OFF no-gap,
              sy-vline no-gap,
       (10) AWREF         COLOR COL_NORMAL INTENSIFIED OFF no-gap,
              sy-vline no-gap,
        (5) AWORG         COLOR COL_NORMAL INTENSIFIED OFF no-gap,
              sy-vline no-gap,
        (9) XARCH         COLOR COL_NORMAL INTENSIFIED OFF no-gap,
              sy-vline no-gap.


ENDFORM.


*-----------------------------------------------------------------------
* FORM  WRITE_STATISTICS
* number of detected documents and account summary list
*-----------------------------------------------------------------------
FORM WRITE_STATISTICS.

  ULINE AT /0(57) NO-GAP.

* Number of detected documents -----------------------------------------
  FORMAT COLOR COL_TOTAL.
    WRITE: /    sy-vline no-gap,
          (11) INDEX_R, (43) 'DOCUMENTS FOUND'
                COLOR COL_TOTAL INTENSIFIED ON  no-gap,
                sy-vline no-gap.

  ULINE AT /0(57) NO-GAP.

  FORMAT COLOR OFF.
  SKIP 2.

* Account summary list -------------------------------------------------
  IF NOT HKONT_LIST IS INITIAL.

    ULINE AT /0(45) NO-GAP.
    WRITE: / sy-vline no-gap,
            (43) 'DOCUMENT TOTALS (ONLINE DATA ONLY)' left-justified
                  COLOR COL_HEADING INTENSIFIED no-gap,
             sy-vline no-gap.

    ULINE AT /0(45) NO-GAP.

    FORMAT COLOR COL_HEADING INTENSIFIED OFF.
    WRITE: /  sy-vline no-gap,
              (5) 'BUKRS'   left-justified  no-gap, sy-vline no-gap,
              (5) 'GJAHR'   left-justified  no-gap, sy-vline no-gap,
             (11) 'HKONT'   left-justified  no-gap, sy-vline no-gap,
              (5) 'SHKZG'   left-justified  no-gap, sy-vline no-gap,
             (13) 'DMBTR'  right-justified  no-gap, sy-vline no-gap.

    FORMAT COLOR OFF.
    ULINE AT /0(45) NO-GAP.


    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
    P_BUKRS_OLD = '0000'.

    LOOP AT HKONT_LIST.
      IF HKONT_LIST-BUKRS <> P_BUKRS_OLD.
        SELECT SINGLE * FROM  T001
                        INTO  XT001
                        WHERE BUKRS = HKONT_LIST-BUKRS.
        P_BUKRS_OLD = HKONT_LIST-BUKRS.
      ENDIF.

    WRITE: /    sy-vline no-gap,
          (5) HKONT_LIST-BUKRS  COLOR COL_KEY    INTENSIFIED     no-gap,
                sy-vline no-gap,
          (5) HKONT_LIST-GJAHR  COLOR COL_KEY    INTENSIFIED     no-gap,
                sy-vline no-gap,
         (11) HKONT_LIST-HKONT  COLOR COL_KEY    INTENSIFIED     no-gap,
                sy-vline no-gap,
          (5) HKONT_LIST-SHKZG  COLOR COL_NORMAL INTENSIFIED OFF no-gap,
                sy-vline no-gap,
         (13) HKONT_LIST-DMBTR  CURRENCY XT001-WAERS
                                COLOR COL_NORMAL INTENSIFIED OFF no-gap,
                sy-vline no-gap.

    FORMAT INTENSIFIED ON.

    ENDLOOP.
    ULINE AT /0(45) NO-GAP.

  ENDIF.

ENDFORM.


*-----------------------------------------------------------------------
* FORM  ACCOUNT_SUMMARY
* accumulated local currency amounts
*-----------------------------------------------------------------------
FORM ACCOUNT_SUMMARY.

  LOOP AT TBSEG.
    HKONT_LIST-BUKRS = TBSEG-BUKRS.
    HKONT_LIST-GJAHR = TBSEG-GJAHR.
    HKONT_LIST-HKONT = TBSEG-HKONT.
    HKONT_LIST-SHKZG = TBSEG-SHKZG.
    HKONT_LIST-DMBTR = TBSEG-DMBTR.

    COLLECT HKONT_LIST.
  ENDLOOP.

ENDFORM.
