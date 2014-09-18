*----------------------------------------------------------------------*
*   INCLUDE ZRFIG01I_WRITE                                             *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  Write_Documents
*&---------------------------------------------------------------------*
FORM WRITE_DOCUMENTS.
  SET PF-STATUS 'LIBS1'.
**-> t001: Company Codes's name <- adrc-name1 + adrc-name2
**
  SELECT SINGLE * FROM T001
         WHERE  BUKRS = IT_BKPF-BUKRS.
  CHECK SY-SUBRC = 0.
  SELECT SINGLE NAME1 NAME2 INTO (WA_L_NAME1, WA_L_NAME2)
    FROM ADRC
    WHERE ADDRNUMBER = T001-ADRNR AND
          DATE_FROM <= SY-DATUM.
*  concatenate wa_l_name1 '_' wa_l_name2 into wa_l_company_name.
  WA_L_COMPANY_NAME =  WA_L_NAME1.
  DATA: L_POS TYPE I.
  L_POS = STRLEN( WA_L_COMPANY_NAME ) + 1.
  WA_L_COMPANY_NAME+L_POS = WA_L_NAME2.
**/////////////////////////////////////////////////////////////
  LOOP AT IT_BKPF WHERE CHKBOX EQ 'X'.
    CLEAR IT_S.        " clear summary fields
    CLEAR SY-PAGNO.
    AT NEW BELNR.
      PERFORM READ_BSEG.
      NEW-PAGE.
      PERFORM WRITE_HEADER_LINE.
    ENDAT.
    LOOP AT IT_BSEG.
      PERFORM WRITE_DETAIL_LINE.
    ENDLOOP.
    PERFORM WRITE_SIGN_BOX.
  ENDLOOP.
ENDFORM.                    " Write_Documents
*&---------------------------------------------------------------------*
*&      Form  write_detail_line
*&---------------------------------------------------------------------*
FORM WRITE_DETAIL_LINE.

  CLEAR IT_L.
  FORMAT   INTENSIFIED OFF.
  DATA : CHK1(1),
         CHK2(1),
         CHK3(1).
**-> read G/L Account's short text
  SELECT SINGLE TXT20
    FROM SKAT
    INTO IT_L-TXT20
   WHERE SPRAS = SY-LANGU
     AND KTOPL = T001-KTOPL
     AND SAKNR = IT_BSEG-HKONT.

**-> dmbtr
  CASE IT_BSEG-SHKZG.
    WHEN 'S'. "Debit
      MOVE IT_BSEG-DMBTR TO IT_L-DR.
      ADD  IT_BSEG-DMBTR TO IT_S-SDR.
    WHEN 'H'. "Credit
      MOVE IT_BSEG-DMBTR TO IT_L-CR.
      ADD  IT_BSEG-DMBTR TO IT_S-SCR.
  ENDCASE.
**-> Account type
  CASE IT_BSEG-KOART.
    WHEN 'A'. "asset
      SELECT SINGLE TXT50
        FROM ANLA       "Asset Master Record Segment
        INTO IT_L-NAME1
       WHERE BUKRS = IT_BKPF-BUKRS
         AND ANLN1 = IT_BSEG-ANLN1  "Asset
         AND ANLN2 = IT_BSEG-ANLN2. "Sub-Number

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
           EXPORTING
                INPUT  = IT_BSEG-ANLN1
           IMPORTING
                OUTPUT = IT_L-CODE.

      CONCATENATE IT_L-CODE     '-'
                  IT_BSEG-ANLN2 '('
                  IT_BSEG-ANBWA ')'
             INTO IT_L-CODE.
      IF IT_BSEG-AUFNR IS INITIAL.
        SELECT SINGLE EAUFN INTO IT_BSEG-AUFNR
          FROM ANLA
          WHERE BUKRS EQ IT_BSEG-BUKRS AND
                ANLN1 EQ IT_BSEG-ANLN1 AND
                ANLN2 EQ IT_BSEG-ANLN2.
      ENDIF.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
           EXPORTING
                INPUT  = IT_BSEG-AUFNR
           IMPORTING
                OUTPUT = IT_L-ASSGN2.

      DATA: WA_L_TEMP(20).
      PERFORM GET_POSITION_ID CHANGING IT_L-ASSGN1 WA_L_TEMP.

    WHEN 'D'. "customers
      SELECT SINGLE NAME1
        FROM KNA1       "General Data in Customer Master
        INTO IT_L-NAME1
       WHERE KUNNR = IT_BSEG-KUNNR.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
           EXPORTING
                INPUT  = IT_BSEG-KUNNR
           IMPORTING
                OUTPUT = IT_L-CODE.
      IF NOT IT_BSEG-UMSKZ IS INITIAL.
        CONCATENATE IT_L-CODE '/'
                  '('  IT_BSEG-UMSKZ  ')'
           INTO IT_L-CODE.
      ENDIF.
*   --> Assignment 1 (Payt terms+ Pmnt method + Pmnt block)
      IF NOT IT_BSEG-ZTERM IS INITIAL.
        MOVE '/'  TO CHK1.
      ELSE.
        MOVE ' '  TO CHK1.
      ENDIF.
      IF NOT IT_BSEG-ZLSCH IS INITIAL.
        MOVE '/'  TO CHK2.
      ELSE.
        MOVE ' '  TO CHK2.
      ENDIF.
      IF NOT IT_BSEG-ZLSPR IS INITIAL.
        MOVE '/'  TO CHK3.
      ELSE.
        MOVE ' '  TO CHK3.
      ENDIF.

      CONCATENATE IT_BSEG-ZTERM CHK1
                  IT_BSEG-ZLSCH CHK2
                  IT_BSEG-ZLSPR
             INTO IT_L-ASSGN1.
*********************************
    WHEN 'K'. "vendors
*   --> account type (name + vendor + space.gl)
      SELECT SINGLE NAME1 INTO IT_L-NAME1 FROM LFA1
        WHERE LIFNR EQ IT_BSEG-LIFNR.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
           EXPORTING
                INPUT  = IT_BSEG-LIFNR
           IMPORTING
                OUTPUT = IT_L-CODE.
      IF NOT IT_BSEG-UMSKZ IS INITIAL.
        CONCATENATE IT_L-CODE '/'
                  '('  IT_BSEG-UMSKZ  ')'
           INTO IT_L-CODE.
      ENDIF.
*   --> Assignment 1 (Payt terms+ Pmnt method + Pmnt block)

      IF NOT IT_BSEG-ZTERM IS INITIAL.
        MOVE '/'  TO CHK1.
      ELSE.
        MOVE ' '  TO CHK1.
      ENDIF.
      IF NOT IT_BSEG-ZLSCH IS INITIAL.
        MOVE '/'  TO CHK2.
      ELSE.
        MOVE ' '  TO CHK2.
      ENDIF.
      IF NOT IT_BSEG-ZLSPR IS INITIAL.
        MOVE '/'  TO CHK3.
      ELSE.
        MOVE ' '  TO CHK3.
      ENDIF.

      CONCATENATE IT_BSEG-ZTERM CHK1
                  IT_BSEG-ZLSCH CHK2
                  IT_BSEG-ZLSPR
             INTO IT_L-ASSGN1.

      IF IT_BSEG-UMSKS = 'A'.    "Down payment
        DATA: WA_CHAR50(50), WA_CHAR20(20).
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
             EXPORTING
                  INPUT  = IT_BSEG-AUFNR
             IMPORTING
                  OUTPUT = IT_L-ORDNO.
        PERFORM GET_POSITION_ID CHANGING WA_CHAR20
                                         WA_CHAR50.
        IF NOT IT_L-ORDNO IS INITIAL.
          CONCATENATE IT_L-ORDNO '/' WA_CHAR20 INTO IT_L-ORDNO.
        ENDIF.
        IF NOT IT_BSEG-EBELN IS INITIAL.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
               EXPORTING
                    INPUT  = IT_BSEG-EBELN
               IMPORTING
                    OUTPUT = WA_CHAR50.
          IT_L-ASSGN1 = WA_CHAR50.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
               EXPORTING
                    INPUT  = IT_BSEG-EBELP
               IMPORTING
                    OUTPUT = WA_CHAR50.
          CONCATENATE IT_L-ASSGN1 '-' WA_CHAR50 INTO IT_L-ASSGN1.
        ENDIF.
      ENDIF.
*=======2003/12/04 downpayment
      SELECT * INTO CORRESPONDING FIELDS OF TABLE  IT_T074U
      FROM T074U
      WHERE UMSKS = 'A'.
      CLEAR WA_T_CNT.
      DESCRIBE TABLE IT_T074U LINES WA_T_CNT.
      IF WA_T_CNT > 0.
        SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_LFC3
        FROM LFC3
        FOR ALL ENTRIES IN IT_T074U
        WHERE LIFNR = IT_BSEG-LIFNR
        AND   BUKRS = 'H201'
        AND   GJAHR = IT_BKPF-GJAHR
        AND   SHBKZ = IT_T074U-UMSKZ.
      ENDIF.
      CLEAR WA_T_CNT.
      DESCRIBE TABLE IT_LFC3 LINES WA_T_CNT.
      CLEAR : WA_DP, WA_NAME1, WA_CNT.
      IF WA_T_CNT > 0.
        WA_NAME1 = IT_L-NAME1.
        LOOP AT IT_LFC3.
          WA_DP = WA_DP + IT_LFC3-SALDV +
                     IT_LFC3-SOLLL + IT_LFC3-HABNL.
        ENDLOOP.

        SELECT COUNT(*) INTO WA_CNT
        FROM FDM1
        WHERE EBELN = IT_BSEG-EBELN
        AND   EBELP = IT_BSEG-EBELP.
      ENDIF.
    WHEN 'M'. "material
    WHEN 'S'. "G/L accounts
      IF IT_BSEG-HKONT+4(2) = '16'    OR
         IT_BSEG-HKONT+4(3) = '901'.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
             EXPORTING
                  INPUT  = IT_BSEG-AUFNR
             IMPORTING
                  OUTPUT = IT_L-ASSGN1.
        PERFORM GET_POSITION_ID CHANGING IT_L-CODE
                                         IT_L-NAME1.
      ENDIF.
******>>> Expense
      IF IT_BSEG-HKONT+4(1) = '6'.
        DATA: IT_LIST TYPE TABLE OF IFMKACOO WITH HEADER LINE.
        RANGES: R_KOSTL FOR BSEG-KOSTL.
        IF ( IT_BSEG-FISTL IS INITIAL ) AND
           ( NOT IT_BSEG-KOSTL IS INITIAL ).
          R_KOSTL-SIGN = 'I'.
          R_KOSTL-OPTION = 'EQ'.
          R_KOSTL-LOW  = IT_BSEG-KOSTL.
          APPEND R_KOSTL.
          CALL FUNCTION 'HHM_KONT_READ_FROM_CO_OBJEKT'
               EXPORTING
                    I_FIKRS = 'H201'
                    I_KOKRS = 'H201'
                    I_KOSTL = 'X'
               TABLES
                    T_KOSTL = R_KOSTL
                    T_LIST  = IT_LIST.

          LOOP AT IT_LIST.
            MOVE IT_LIST-FISTL TO IT_BSEG-FISTL.
          ENDLOOP.
        ENDIF.

        SELECT SINGLE BEZEICH
          FROM FMFCTRT                 "Funds Center Text
          INTO IT_L-NAME1
         WHERE FICTR = IT_BSEG-FISTL.  "Funds Center
        MOVE IT_BSEG-FISTL TO IT_L-CODE.
      ENDIF.
  ENDCASE.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
       EXPORTING
            INPUT  = IT_L-CODE
       IMPORTING
            OUTPUT = IT_L-CODE.
  IF ( IT_L-NAME1 IS INITIAL AND IT_L-CODE IS INITIAL ) .
    CLEAR : IT_L-ACCT_TYPE.
  ELSE.
    CONCATENATE IT_L-NAME1 '/' IT_L-CODE INTO IT_L-ACCT_TYPE.
  ENDIF.

**--> Tax rate
  DATA: L_RATE TYPE P DECIMALS 1.
  MOVE IT_BSEG-MWSKZ TO IT_L-TAXCD.
  IF IT_BSEG-FWBAS NE 0.
    L_RATE = IT_BSEG-DMBTR / IT_BSEG-FWBAS * 100.
    IT_L-RATE = L_RATE.
    SELECT SINGLE TEXT1 INTO IT_L-ACCT_TYPE FROM T007S
      WHERE SPRAS = SY-LANGU AND
            KALSM = 'TAXUS' AND
            MWSKZ = IT_BSEG-MWSKZ.
  ENDIF.
  DATA: BEGIN OF IT_TAX OCCURS 0,
           KSCHL LIKE KONP-KSCHL,
           KBETR LIKE KONP-KBETR,
        END OF IT_TAX.
  DATA: WA_L_TAX LIKE BSEG-DMBTR.
**--> Parked doc's tax calculate
**--> refrence t-code : FTXP - maintain tax code
  IF IT_BSEG-MWSKZ NE SPACE AND  " exist tax code
     IT_BSEG-SHKZG EQ 'S'   AND  " only debit
     IT_BKPF-BSTAT EQ 'V'.       " only parked doc.
    SELECT B~KSCHL B~KBETR INTO TABLE IT_TAX
      FROM A003 AS A INNER JOIN KONP AS B ON B~KNUMH EQ A~KNUMH
      WHERE A~KAPPL = 'TX' AND
            A~ALAND = 'US' AND
            A~MWSKZ EQ IT_BSEG-MWSKZ.
    LOOP AT IT_TAX WHERE KSCHL+3(1) EQ 'I'.    "case A/P
      WA_L_TAX = IT_BSEG-DMBTR * IT_TAX-KBETR / 1000.
      ADD WA_L_TAX TO IT_L-DRT.
      ADD WA_L_TAX TO IT_S-SDRT.
    ENDLOOP.
  ENDIF.

  IF SY-LINNO > 85.
    NEW-PAGE.
    PERFORM WRITE_HEADER_LINE.
  ENDIF.

**-> detail 1 line
  WRITE:  /(01) WA_VL NO-GAP,
           (03) IT_BSEG-BUZEI NO-GAP, WA_VL NO-GAP, " Line No
           (20) IT_L-TXT20 NO-GAP, WA_VL NO-GAP, " Acct Short Text
           (49) IT_L-ACCT_TYPE  NO-GAP,
           (08) IT_L-TAXCD      NO-GAP, "Tax Code
           (04) IT_L-RATE       NO-GAP, "Tax Rate
           (01) WA_VL NO-GAP,
           (20) IT_L-DR NO-GAP CURRENCY IT_BSEG-PSWSL NO-ZERO, "Dr Amt
           (01) WA_VL NO-GAP,
           (20) IT_L-CR NO-GAP CURRENCY IT_BSEG-PSWSL NO-ZERO, "Cr Amt
           (01) WA_VL.

* 2 line
  DATA: WA_L_KTEXT    TYPE KTEXT,
        WA_L_FUNC_AREA TYPE FKBER,
        WA_L_KOSTL  TYPE CHAR10.
  IF IT_BSEG-KOSTL NE ' '.
    CALL FUNCTION 'Z_FFI_GET_KOSTL'
         EXPORTING
              I_KOKRS     = 'H201'
              I_KOSTL     = IT_BSEG-KOSTL
         IMPORTING
              E_KTEXT     = WA_L_KTEXT
              E_FUNC_AREA = WA_L_FUNC_AREA.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
         EXPORTING
              INPUT  = IT_BSEG-KOSTL  "Cost Center
         IMPORTING
              OUTPUT = WA_L_KOSTL.

    CONCATENATE WA_L_KTEXT '/' WA_L_KOSTL '/' WA_L_FUNC_AREA
           INTO IT_L-ORDNO.

  ENDIF.
  WRITE:/  WA_VL NO-GAP,
         (03) ' ' NO-GAP,  WA_VL NO-GAP,
         (20) IT_BSEG-HKONT NO-GAP, WA_VL NO-GAP,  " G/L Account
         (31) IT_L-ORDNO NO-GAP,
         (18) IT_L-ASSGN1 NO-GAP,
         (12) IT_L-ASSGN2 NO-GAP,
         (01) WA_VL NO-GAP,
         (20) IT_L-DRT NO-GAP CURRENCY IT_BSEG-PSWSL NO-ZERO, "Dr Amt
         (01) WA_VL NO-GAP,
         (20) IT_L-CRT NO-GAP CURRENCY IT_BSEG-PSWSL NO-ZERO, "Cr Amt
         (01) WA_VL NO-GAP.

* 3 line
*-----2003/12/10
  CLEAR WA_FINAL_TXT.
  IF WA_DP > 0.
    IF IT_BSEG-KOART <> 'K'.
      IF WA_CNT = 0.
        WA_FINAL_TXT = 'Final Invoice'.
        CLEAR WA_CNT.
      ENDIF.
    ENDIF.
  ENDIF.

  WRITE:/ WA_VL NO-GAP,
    (03) ' ' NO-GAP, WA_VL NO-GAP,
    (20) WA_FINAL_TXT NO-GAP, WA_VL NO-GAP,
    (49) IT_BSEG-SGTXT NO-GAP,
    (12) IT_BSEG-FDTAG NO-GAP NO-ZERO, WA_VL NO-GAP.
*------------------------------------------*
  IF IT_BKPF-WAERS = 'USD'.
    WRITE:
         (20) ' ' NO-GAP, WA_VL NO-GAP,
         (20) ' ' NO-GAP, WA_VL NO-GAP.
  ELSE.
    IF IT_BSEG-SHKZG = 'S'.
      WRITE:
         (20) IT_BSEG-WRBTR NO-GAP CURRENCY IT_BKPF-WAERS NO-ZERO, "
         (01) WA_VL NO-GAP,
         (20) SPACE NO-GAP,
         (01) WA_VL NO-GAP.
    ELSE.
      WRITE:
         (20) SPACE NO-GAP,
         (01) WA_VL NO-GAP,
         (20) IT_BSEG-WRBTR NO-GAP CURRENCY IT_BKPF-WAERS NO-ZERO, "
         (01) WA_VL NO-GAP.
    ENDIF.
  ENDIF.

  NEW-LINE. ULINE AT (WA_WIDTH).
ENDFORM.
*&---------------------------------------------------------------------*
*       FORM WRITE_Empty_LINE                                          *
*&---------------------------------------------------------------------*
FORM WRITE_EMPTY_LINE.
  DO 3 TIMES.
    WRITE:  /(01) WA_VL NO-GAP,
           (03) SPACE NO-GAP,
           (01) WA_VL NO-GAP, " Line No
           (20) SPACE NO-GAP,
           (01) WA_VL NO-GAP,
           (49) SPACE NO-GAP,
           (08) SPACE NO-GAP, "Tax Code
           (04) SPACE NO-GAP, "Tax Rate
           (01) WA_VL NO-GAP,
           (20) SPACE NO-GAP, "Dr Amt
           (01) WA_VL NO-GAP,
           (20) SPACE NO-GAP, "Cr Amt
           (01) WA_VL.
  ENDDO.
  NEW-LINE. ULINE AT (WA_WIDTH).

ENDFORM.
*&---------------------------------------------------------------------*
*       FORM WRITE_HEADER_LINE                                         *
*&---------------------------------------------------------------------*
FORM WRITE_HEADER_LINE.
  DATA: L_U_DEPARTMENT   TYPE AD_DPRTMNT,
        L_U_NAME_TEXT    TYPE AD_NAMTEXT.
  DATA: L_P_DEPARTMENT   TYPE AD_DPRTMNT,
        L_P_NAME_TEXT    TYPE AD_NAMTEXT.

**--> t001: Company Codes
  SELECT SINGLE * FROM T001
         WHERE BUKRS EQ IT_BKPF-BUKRS.
**--> t003t: Document type
  SELECT SINGLE * FROM T003T
         WHERE SPRAS = SY-LANGU  AND
               BLART = IT_BKPF-BLART.

**--> get person info.
  CALL FUNCTION 'Z_FFI_GET_PERSON_INFO'
       EXPORTING
            I_USNAM      = IT_BKPF-USNAM
       IMPORTING
            E_DEPARTMENT = L_U_DEPARTMENT
            E_NAME_TEXT  = L_U_NAME_TEXT.

  CALL FUNCTION 'Z_FFI_GET_PERSON_INFO'
       EXPORTING
            I_USNAM      = IT_BKPF-PPNAM
       IMPORTING
            E_DEPARTMENT = L_P_DEPARTMENT
            E_NAME_TEXT  = L_P_NAME_TEXT.

  FORMAT INTENSIFIED  OFF.
  SKIP 6.

  WRITE:  (130) 'Accounting Document' CENTERED.
  WRITE:  (130) '===================' CENTERED.

  SKIP 2.

  WRITE:/    'Company code :', IT_BKPF-BUKRS, '-',  WA_L_COMPANY_NAME,
        98   'Print date : ' NO-GAP,
              SY-DATUM  MM/DD/YYYY,
              SY-UZEIT  USING EDIT MASK '__:__:__'.

  WRITE:/116 'Page:' NO-GAP, SY-PAGNO NO-GAP,
             '/ '     NO-GAP,
             (2) WA_TOTAL_PAGE_NUMBER NO-GAP LEFT-JUSTIFIED.
* check reverse doc
  IF NOT IT_BKPF-STBLG IS INITIAL.
    IF IT_BKPF-STGRD IS INITIAL.
      SELECT SINGLE STGRD INTO IT_BKPF-STGRD FROM BKPF
        WHERE BUKRS = IT_BKPF-BUKRS AND
              BELNR = IT_BKPF-STBLG AND
              GJAHR = IT_BKPF-STJAH.
    ENDIF.
    DATA: WA_L_REVERSE(20).
    CONCATENATE ' REVERSED('
                IT_BKPF-STGRD
                ')'
          INTO  WA_L_REVERSE.
  ENDIF.

  NEW-LINE.  ULINE AT (WA_WIDTH).
  WRITE:  /(01) WA_VL NO-GAP,
           (12) 'Doc. No.  : '  NO-GAP CENTERED,
           (10) IT_BKPF-BELNR      NO-GAP LEFT-JUSTIFIED,
           (13) WA_L_REVERSE NO-GAP,
           (01) WA_VL NO-GAP,
           (15) 'Posting  Date : '  NO-GAP CENTERED,
           (12) IT_BKPF-BUDAT      NO-GAP CENTERED  MM/DD/YYYY,
           (01) WA_VL NO-GAP,
           (10) 'Inv/Ref : '       NO-GAP CENTERED,
           (18) IT_BKPF-XBLNR      NO-GAP LEFT-JUSTIFIED,
           (01) WA_VL NO-GAP,
           (09) 'Posted : '       NO-GAP CENTERED.
  IF IT_BKPF-BSTAT = ' '.
    WRITE: (10) IT_BKPF-USNAM      NO-GAP,
           (16) L_U_NAME_TEXT      NO-GAP,
           (01) WA_VL NO-GAP.

  ELSE.
    WRITE: (10) SPACE NO-GAP,
           (16) SPACE NO-GAP,
           (01) WA_VL NO-GAP.
  ENDIF.

  WRITE:  /(01) WA_VL NO-GAP,
           (12) 'Doc. Type : '  NO-GAP,
           (02) IT_BKPF-BLART NO-GAP,
           (01) '-' NO-GAP,
           (20) T003T-LTEXT NO-GAP,
           (01) WA_VL NO-GAP,
           (15) 'Document Date : ' NO-GAP CENTERED,
           (12) IT_BKPF-BLDAT      NO-GAP CENTERED  MM/DD/YYYY,
           (01) WA_VL NO-GAP,
           (15) 'Doc.Currency : '  NO-GAP CENTERED,
           (13) IT_BKPF-WAERS      NO-GAP LEFT-JUSTIFIED,
           (01) WA_VL NO-GAP,
           (09) 'Parked : '    NO-GAP CENTERED.
  IF IT_BKPF-BSTAT = ' '.
    WRITE: (10) IT_BKPF-PPNAM      NO-GAP,
           (16) L_P_NAME_TEXT      NO-GAP,
           (01) WA_VL NO-GAP.
  ELSE.
    WRITE: (10) IT_BKPF-USNAM      NO-GAP,
           (16) L_U_NAME_TEXT      NO-GAP,
           (01) WA_VL NO-GAP.
  ENDIF.

  NEW-LINE.
  ULINE AT (WA_WIDTH).

* Title line 1
  WRITE:/   WA_VL NO-GAP,
            (03) ' '   NO-GAP CENTERED, WA_VL NO-GAP,
            (20) 'GL Account Name' NO-GAP CENTERED, WA_VL NO-GAP,
            (48) 'Account Detail Information' NO-GAP CENTERED,
                 "<- Account Type
            (01) WA_VL NO-GAP,
            (07) 'Tax Cd.' NO-GAP,WA_VL NO-GAP,
            (04) 'Rate' NO-GAP, WA_VL NO-GAP,
            (20) ' '  NO-GAP CENTERED, WA_VL NO-GAP,
            (20) ' ' NO-GAP CENTERED, WA_VL NO-GAP.
* Title line 2
  NEW-LINE.
  WRITE: WA_VL NO-GAP.
  ULINE AT 5(84). WRITE: 109 WA_VL NO-GAP, 130 WA_VL NO-GAP.

* Title line 3
  WRITE:/  WA_VL NO-GAP,
          (03) 'No.' NO-GAP CENTERED,       WA_VL NO-GAP,
          (20) SPACE NO-GAP,                WA_VL NO-GAP,
          (30) 'Cost Center / Order No' NO-GAP CENTERED, WA_VL NO-GAP,
          (17) 'Assignment1'     NO-GAP CENTERED, WA_VL NO-GAP,
          (12) 'Assignment2'     NO-GAP CENTERED, WA_VL NO-GAP,
          (20) 'Debit'  NO-GAP CENTERED, WA_VL NO-GAP,
          (20) 'Credit' NO-GAP CENTERED, WA_VL NO-GAP.

* Title line 4
  WRITE:/  WA_VL NO-GAP,
           (03) ' ' NO-GAP CENTERED,  WA_VL NO-GAP,
           (20) 'GL Account' NO-GAP CENTERED, WA_VL NO-GAP,
           (61) WA_UL NO-GAP, WA_VL NO-GAP,
        109(01) WA_VL NO-GAP,
        130(01) WA_VL NO-GAP.
* Title line 5
  WRITE:/  WA_VL NO-GAP,
           (03) ' ' NO-GAP, WA_VL NO-GAP,
           (20) ' ' NO-GAP, WA_VL NO-GAP,
           (48) 'Text' NO-GAP CENTERED, WA_VL NO-GAP,
           (12) 'Date' NO-GAP CENTERED, WA_VL NO-GAP,
           (20) ' ' NO-GAP, WA_VL NO-GAP,
           (20) ' ' NO-GAP, WA_VL NO-GAP.
  NEW-LINE.
  ULINE AT (WA_WIDTH).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  WRITE_SIGN_BOX                                           *
*&---------------------------------------------------------------------*
FORM WRITE_SIGN_BOX.

  IF SY-LINNO > 56.
    NEW-PAGE.
    PERFORM WRITE_HEADER_LINE.
  ENDIF.
  DO.
    IF SY-LINNO > 56.
      EXIT.
    ENDIF.
    PERFORM WRITE_EMPTY_LINE.
  ENDDO.
  FORMAT   INTENSIFIED OFF.
  ULINE AT (WA_WIDTH).
*----JHS MODIFY 2003/09/04
  IF IT_BKPF-BSTAT = 'S'.
    IF IT_BSEG-UMSKS <> 'A'.    "Down payment
      IF IT_S-SDR NE IT_S-SCR.
        WRITE :/ 'This document is uncompleted.'.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.
  WRITE:/  WA_VL NO-GAP,
          (10) ' MEMO : '  NO-GAP.
  IF WA_DP > 0.
    IF WA_CNT > 0.
      WA_FINAL = 'Final Document'.
    ELSE.
      WA_FINAL = ' '.
    ENDIF.
    WRITE: (30) WA_NAME1, 'D/Payment ', (12) WA_DP, (16) WA_FINAL
    NO-GAP, WA_VL NO-GAP.
  ELSE.
    WRITE: (71) SPACE NO-GAP, WA_VL NO-GAP.
  ENDIF.
  CLEAR  : WA_NAME1, WA_DP, WA_FINAL, WA_CNT.
  WRITE:  (04) 'Sum ' NO-GAP CENTERED, WA_VL NO-GAP,
          (20) IT_S-SDR NO-GAP CURRENCY IT_BSEG-PSWSL NO-ZERO,
               WA_VL NO-GAP,
          (20) IT_S-SCR NO-GAP CURRENCY IT_BSEG-PSWSL NO-ZERO,
               WA_VL NO-GAP.

  WRITE:/  WA_VL NO-GAP,
          (81) '      '  NO-GAP, WA_VL NO-GAP.
  IF IT_BKPF-BSTAT = 'V'.
    WRITE:(04) 'Tax ' NO-GAP CENTERED, WA_VL NO-GAP.
  ELSE.
    WRITE:(04) '    ' NO-GAP CENTERED, WA_VL NO-GAP.
  ENDIF.
  WRITE:  (20) IT_S-SDRT NO-GAP CURRENCY IT_BSEG-PSWSL NO-ZERO,
               WA_VL NO-GAP,
          (20) IT_S-SCRT NO-GAP CURRENCY IT_BSEG-PSWSL NO-ZERO,
               WA_VL NO-GAP.

  NEW-LINE. ULINE AT (WA_WIDTH).

  FORMAT   INTENSIFIED OFF.
*---2003/09/08 jhs modify Requestor
  WRITE:/  WA_VL NO-GAP, (11)  SPACE    NO-GAP CENTERED,
           WA_VL NO-GAP, (40)  'Name'   NO-GAP CENTERED,
           WA_VL NO-GAP, (40)  'Signature'   NO-GAP CENTERED,
           WA_VL NO-GAP, (34)  'Date'   NO-GAP CENTERED,
           WA_VL NO-GAP.
  WRITE:/  WA_VL NO-GAP, (11) SPACE NO-GAP CENTERED,
           WA_VL NO-GAP, (117) WA_UL NO-GAP CENTERED.
  WRITE:/  WA_VL NO-GAP, (11) 'Requestor'    NO-GAP CENTERED,
           WA_VL NO-GAP, (40) SPACE NO-GAP CENTERED,
           WA_VL NO-GAP, (40) SPACE NO-GAP CENTERED,
           WA_VL NO-GAP, (34) SPACE NO-GAP CENTERED,
           WA_VL NO-GAP.
  WRITE:/  WA_VL NO-GAP, (11) SPACE NO-GAP CENTERED,
           WA_VL NO-GAP, (40) SPACE NO-GAP CENTERED,
           WA_VL NO-GAP, (40) SPACE NO-GAP CENTERED,
           WA_VL NO-GAP, (34) SPACE NO-GAP CENTERED,
           WA_VL NO-GAP.
  WRITE:/  WA_VL NO-GAP, (11) SPACE NO-GAP CENTERED,
           WA_VL NO-GAP, (40) SPACE NO-GAP CENTERED,
           WA_VL NO-GAP, (40) SPACE NO-GAP CENTERED,
           WA_VL NO-GAP, (34) SPACE NO-GAP CENTERED,
           WA_VL NO-GAP.

*  NEW-LINE.
  WRITE :/ WA_VL NO-GAP,
           (128) WA_UL  NO-GAP,
           WA_VL NO-GAP.

  WRITE :/ WA_VL NO-GAP,
           (128) WA_UL  NO-GAP,
           WA_VL NO-GAP.
*--------Department approval  date finance approval date
  WRITE:/  WA_VL NO-GAP, (22)  'Department'   NO-GAP CENTERED,
           WA_VL NO-GAP, (29)  'Approval'     NO-GAP CENTERED,
           WA_VL NO-GAP, (12)  'Date'         NO-GAP CENTERED,
           WA_VL NO-GAP, (20)  'Finance'      NO-GAP CENTERED,
           WA_VL NO-GAP, (29)  'Approval'     NO-GAP CENTERED,
           WA_VL NO-GAP, (11)  'Date'         NO-GAP CENTERED,
           WA_VL NO-GAP.
  WRITE :/ WA_VL NO-GAP,
           (128) WA_UL  NO-GAP,
           WA_VL NO-GAP.
*---------------------Head of department
  WRITE:/  WA_VL NO-GAP, (22)  SPACE   NO-GAP CENTERED,
           WA_VL NO-GAP, (29)  SPACE   NO-GAP CENTERED,
           WA_VL NO-GAP, (12)  SPACE   NO-GAP CENTERED,
           WA_VL NO-GAP, (20)  SPACE   NO-GAP CENTERED,
           WA_VL NO-GAP, (29)  SPACE   NO-GAP CENTERED,
           WA_VL NO-GAP, (11)  SPACE   NO-GAP CENTERED,
           WA_VL NO-GAP.
*
  WRITE:/  WA_VL NO-GAP, (22)  'Head of Department'   NO-GAP CENTERED,
           WA_VL NO-GAP, (29)  SPACE    NO-GAP CENTERED,
           WA_VL NO-GAP, (12)  SPACE    NO-GAP CENTERED,
           WA_VL NO-GAP, (20)  'Cost Accounting'      NO-GAP CENTERED,
           WA_VL NO-GAP, (29)  SPACE     NO-GAP CENTERED,
           WA_VL NO-GAP, (11)  SPACE     NO-GAP CENTERED,
           WA_VL NO-GAP.
*
  WRITE:/  WA_VL NO-GAP, (22)  SPACE   NO-GAP CENTERED,
           WA_VL NO-GAP, (29)  SPACE   NO-GAP CENTERED,
           WA_VL NO-GAP, (12)  SPACE   NO-GAP CENTERED,
           WA_VL NO-GAP, (20)  SPACE   NO-GAP CENTERED,
           WA_VL NO-GAP, (29)  SPACE   NO-GAP CENTERED,
           WA_VL NO-GAP, (11)  SPACE   NO-GAP CENTERED,
           WA_VL NO-GAP.
  WRITE :/ WA_VL NO-GAP,
           (128) WA_UL  NO-GAP,
           WA_VL NO-GAP.

*---------------------Head of sub division
  WRITE:/  WA_VL NO-GAP, (22)  SPACE   NO-GAP CENTERED,
           WA_VL NO-GAP, (29)  SPACE   NO-GAP CENTERED,
           WA_VL NO-GAP, (12)  SPACE   NO-GAP CENTERED,
           WA_VL NO-GAP, (20)  SPACE   NO-GAP CENTERED,
           WA_VL NO-GAP, (29)  SPACE   NO-GAP CENTERED,
           WA_VL NO-GAP, (11)  SPACE   NO-GAP CENTERED,
           WA_VL NO-GAP.
*
  WRITE:/  WA_VL NO-GAP, (22)  'Head of Sub Division'   NO-GAP CENTERED,
           WA_VL NO-GAP, (29)  SPACE    NO-GAP CENTERED,
           WA_VL NO-GAP, (12)  SPACE    NO-GAP CENTERED,
           WA_VL NO-GAP, (20)  'General Accounting'  NO-GAP CENTERED,
           WA_VL NO-GAP, (29)  SPACE     NO-GAP CENTERED,
           WA_VL NO-GAP, (11)  SPACE     NO-GAP CENTERED,
           WA_VL NO-GAP.
*
  WRITE:/  WA_VL NO-GAP, (22)  SPACE   NO-GAP CENTERED,
           WA_VL NO-GAP, (29)  SPACE   NO-GAP CENTERED,
           WA_VL NO-GAP, (12)  SPACE   NO-GAP CENTERED,
           WA_VL NO-GAP, (20)  SPACE   NO-GAP CENTERED,
           WA_VL NO-GAP, (29)  SPACE   NO-GAP CENTERED,
           WA_VL NO-GAP, (11)  SPACE   NO-GAP CENTERED,
           WA_VL NO-GAP.
  WRITE :/ WA_VL NO-GAP,
           (128) WA_UL  NO-GAP,
           WA_VL NO-GAP.

*---------------------Head of  division
  WRITE:/  WA_VL NO-GAP, (22)  SPACE   NO-GAP CENTERED,
           WA_VL NO-GAP, (29)  SPACE   NO-GAP CENTERED,
           WA_VL NO-GAP, (12)  SPACE   NO-GAP CENTERED,
           WA_VL NO-GAP, (20)  SPACE   NO-GAP CENTERED,
           WA_VL NO-GAP, (29)  SPACE   NO-GAP CENTERED,
           WA_VL NO-GAP, (11)  SPACE   NO-GAP CENTERED,
           WA_VL NO-GAP.
*
  WRITE:/  WA_VL NO-GAP, (22)  'Head of Division'   NO-GAP CENTERED,
           WA_VL NO-GAP, (29)  SPACE    NO-GAP CENTERED,
           WA_VL NO-GAP, (12)  SPACE    NO-GAP CENTERED,
           WA_VL NO-GAP, (20)  'Treasurer'  NO-GAP CENTERED,
           WA_VL NO-GAP, (29)  SPACE     NO-GAP CENTERED,
           WA_VL NO-GAP, (11)  SPACE     NO-GAP CENTERED,
           WA_VL NO-GAP.
*
  WRITE:/  WA_VL NO-GAP, (22)  SPACE   NO-GAP CENTERED,
           WA_VL NO-GAP, (29)  SPACE   NO-GAP CENTERED,
           WA_VL NO-GAP, (12)  SPACE   NO-GAP CENTERED,
           WA_VL NO-GAP, (20)  SPACE   NO-GAP CENTERED,
           WA_VL NO-GAP, (29)  SPACE   NO-GAP CENTERED,
           WA_VL NO-GAP, (11)  SPACE   NO-GAP CENTERED,
           WA_VL NO-GAP.
  WRITE :/ WA_VL NO-GAP,
           (128) WA_UL  NO-GAP,
           WA_VL NO-GAP.

*---------------------President / CEO
  WRITE:/  WA_VL NO-GAP, (22)  SPACE   NO-GAP CENTERED,
           WA_VL NO-GAP, (29)  SPACE   NO-GAP CENTERED,
           WA_VL NO-GAP, (12)  SPACE   NO-GAP CENTERED,
           WA_VL NO-GAP, (20)  SPACE   NO-GAP CENTERED,
           WA_VL NO-GAP, (29)  SPACE   NO-GAP CENTERED,
           WA_VL NO-GAP, (11)  SPACE   NO-GAP CENTERED,
           WA_VL NO-GAP.
*
  WRITE:/  WA_VL NO-GAP, (22)  'President / CEO'   NO-GAP CENTERED,
           WA_VL NO-GAP, (29)  SPACE    NO-GAP CENTERED,
           WA_VL NO-GAP, (12)  SPACE    NO-GAP CENTERED,
           WA_VL NO-GAP, (20)  'CFO'  NO-GAP CENTERED,
           WA_VL NO-GAP, (29)  SPACE     NO-GAP CENTERED,
           WA_VL NO-GAP, (11)  SPACE     NO-GAP CENTERED,
           WA_VL NO-GAP.
*
  WRITE:/  WA_VL NO-GAP, (22)  SPACE   NO-GAP CENTERED,
           WA_VL NO-GAP, (29)  SPACE   NO-GAP CENTERED,
           WA_VL NO-GAP, (12)  SPACE   NO-GAP CENTERED,
           WA_VL NO-GAP, (20)  SPACE   NO-GAP CENTERED,
           WA_VL NO-GAP, (29)  SPACE   NO-GAP CENTERED,
           WA_VL NO-GAP, (11)  SPACE   NO-GAP CENTERED,
           WA_VL NO-GAP.
  WRITE :/ WA_VL NO-GAP,
           (128) WA_UL  NO-GAP,
           WA_VL NO-GAP.

ENDFORM.                    " DIspaceLAY_END_OF_PAGE
