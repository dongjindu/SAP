*----------------------------------------------------------------------*
*   INCLUDE ZRFIG01I_WRITE                                             *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  Write_Documents
*&---------------------------------------------------------------------*
FORM write_documents.
  SET PF-STATUS 'LIBS1'.
**-> t001: Company Codes's name <- adrc-name1 + adrc-name2
**
  SELECT SINGLE * FROM t001
         WHERE  bukrs = it_bkpf-bukrs.
  CHECK sy-subrc = 0.
  SELECT SINGLE name1 name2 INTO (wa_l_name1, wa_l_name2)
    FROM adrc
    WHERE addrnumber = t001-adrnr AND
          date_from <= sy-datum.
*  concatenate wa_l_name1 '_' wa_l_name2 into wa_l_company_name.
  wa_l_company_name =  wa_l_name1.
  DATA: l_pos TYPE i,
        l_idx LIKE sy-tabix.

  l_pos = strlen( wa_l_company_name ) + 1.
  wa_l_company_name+l_pos = wa_l_name2.
**/////////////////////////////////////////////////////////////
  SORT it_bkpf BY  belnr gjahr.                             "UD1K922521
  LOOP AT it_bkpf WHERE chkbox EQ 'X'.
    l_idx = sy-tabix.
    CLEAR it_s.        " clear summary fields
    CLEAR sy-pagno.
    AT NEW belnr.
      IF s_rbelnr-low IS INITIAL.
        PERFORM read_bseg.
      ELSE.
        PERFORM read_rseg.
      ENDIF.
      NEW-PAGE.
      READ TABLE it_bkpf INDEX l_idx  INTO it_bkpf.
      PERFORM write_header_line.
    ENDAT.
    CLEAR ll_wrbtr.
    DATA $flag.
    LOOP AT it_bseg.
      AT NEW belnr.
        $flag = true.
      ENDAT.
      PERFORM write_detail_line CHANGING $flag.
*+ by ig.moon 4/11/2008 {
      CHECK  it_bseg-koart EQ 'K'.
* }
      ll_wrbtr = ll_wrbtr + it_bseg-dmbtr.
    ENDLOOP.
    PERFORM write_sign_box.
  ENDLOOP.
*  if sy-subrc eq 4.
*    message i000 with 'First, You have to check for print in list'.
*  endif.
ENDFORM.                    " Write_Documents

*&------------------------------------------------------*
*&      Form  write_detail_line
*&------------------------------------------------------*
FORM write_detail_line CHANGING p_flag..

  DATA : wa_menge(10) TYPE p.

  CLEAR it_l.
  FORMAT   INTENSIFIED OFF.
  DATA : chk1(1),
         chk2(1),
         chk3(1).
**-> read G/L Account's short text
  SELECT SINGLE txt20
    FROM skat
    INTO it_l-txt20
   WHERE spras = sy-langu
     AND ktopl = t001-ktopl
     AND saknr = it_bseg-hkont.

**-> dmbtr
  CASE it_bseg-shkzg.
    WHEN 'S'. "Debit
      IF s_rbelnr-low IS INITIAL.
        MOVE it_bseg-dmbtr TO it_l-dr.
        ADD  it_bseg-dmbtr TO it_s-sdr.
      ELSE.
        MOVE it_bseg-wrbtr TO it_l-dr.
        ADD  it_bseg-wrbtr TO it_s-sdr.
      ENDIF.
    WHEN 'H'. "Credit
      MOVE it_bseg-dmbtr TO it_l-cr.
      ADD  it_bseg-dmbtr TO it_s-scr.
  ENDCASE.

**-> Account type
  CASE it_bseg-koart.
    WHEN 'A'. "asset
      SELECT SINGLE txt50
        FROM anla       "Asset Master Record Segment
        INTO it_l-name1
       WHERE bukrs = it_bkpf-bukrs
         AND anln1 = it_bseg-anln1  "Asset
         AND anln2 = it_bseg-anln2. "Sub-Number

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
           EXPORTING
                input  = it_bseg-anln1
           IMPORTING
                output = it_l-code.

      CONCATENATE it_l-code     '-'
                  it_bseg-anln2 '('
                  it_bseg-anbwa ')'
             INTO it_l-code.
      IF it_bseg-aufnr IS INITIAL.
        SELECT SINGLE eaufn INTO it_bseg-aufnr
          FROM anla
          WHERE bukrs EQ it_bseg-bukrs AND
                anln1 EQ it_bseg-anln1 AND
                anln2 EQ it_bseg-anln2.
      ENDIF.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
           EXPORTING
                input  = it_bseg-aufnr
           IMPORTING
                output = it_l-assgn2.

*      MOVE it_bseg-aufnr  TO  it_l-assgn2.
      DATA: wa_l_temp(20).
      PERFORM get_position_id CHANGING it_l-assgn1 wa_l_temp.

    WHEN 'D'. "customers
      SELECT SINGLE name1
        FROM kna1       "General Data in Customer Master
        INTO it_l-name1
       WHERE kunnr = it_bseg-kunnr.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
           EXPORTING
                input  = it_bseg-kunnr
           IMPORTING
                output = it_l-code.
*      IF NOT it_bseg-umskz IS INITIAL.   "UD1K921782
      IF NOT it_bseg-zumsk IS INITIAL.
        CONCATENATE it_l-code '/'
*                  '('  it_bseg-umskz  ')'  "UD1K921782
                   '('  it_bseg-zumsk  ')'
           INTO it_l-code.
      ENDIF.
*   --> Assignment 1 (Payt terms+ Pmnt method + Pmnt block)
      IF NOT it_bseg-zterm IS INITIAL.
        MOVE '/'  TO chk1.
      ELSE.
        MOVE ' '  TO chk1.
      ENDIF.
      IF NOT it_bseg-zlsch IS INITIAL.
        MOVE '/'  TO chk2.
      ELSE.
        MOVE ' '  TO chk2.
      ENDIF.
      IF NOT it_bseg-zlspr IS INITIAL.
        MOVE '/'  TO chk3.
      ELSE.
        MOVE ' '  TO chk3.
      ENDIF.

      CONCATENATE it_bseg-zterm chk1
                  it_bseg-zlsch chk2
                  it_bseg-zlspr
             INTO it_l-assgn1.
*********************************
    WHEN 'K'. "vendors
*   --> account type (name + vendor + space.gl)
      DATA: l_lifnr LIKE lfa1-lifnr.

      IF it_bseg-empfb <> space.  "Alt.Payee
        SELECT SINGLE * FROM lfa1
          WHERE lifnr EQ it_bseg-empfb.
      ELSE.
        SELECT SINGLE * FROM lfa1
          WHERE lifnr EQ it_bseg-lifnr.
      ENDIF.

      IF lfa1-xcpdk = 'X'.  " one time
        SELECT SINGLE * FROM bsec
            WHERE bukrs = it_bseg-bukrs
              AND gjahr = it_bseg-gjahr
              AND belnr = it_bseg-belnr.
        it_l-name1  = bsec-name1.
      ELSE.
        it_l-name1  = lfa1-name1.
      ENDIF.

*      SELECT SINGLE name1 INTO it_l-name1 FROM lfa1
*        WHERE lifnr EQ it_bseg-lifnr.


      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
           EXPORTING
                input  = it_bseg-lifnr
           IMPORTING
                output = it_l-code.
*      IF NOT it_bseg-umskz IS INITIAL.     "UD1K921782
      IF NOT  it_bseg-zumsk IS INITIAL.
        CONCATENATE it_l-code '/'
*                 '('  it_bseg-umskz  ')'   "UD1K921782
                  '('  it_bseg-zumsk  ')'
           INTO it_l-code.
      ENDIF.
*   --> Assignment 1 (Payt terms+ Pmnt method + Pmnt block)

      IF NOT it_bseg-zterm IS INITIAL.
        MOVE '/'  TO chk1.
      ELSE.
        MOVE ' '  TO chk1.
      ENDIF.
      IF NOT it_bseg-zlsch IS INITIAL.
        MOVE '/'  TO chk2.
      ELSE.
        MOVE ' '  TO chk2.
      ENDIF.
      IF NOT it_bseg-zlspr IS INITIAL.
        MOVE '/'  TO chk3.
      ELSE.
        MOVE ' '  TO chk3.
      ENDIF.

      CONCATENATE it_bseg-zterm chk1
                  it_bseg-zlsch chk2
                  it_bseg-zlspr
             INTO it_l-assgn1.

      IF it_bseg-empfb <> space.
        CONCATENATE it_bseg-empfb ':' it_l-assgn1
             INTO it_l-assgn1.
      ENDIF.


      IF it_bseg-umsks = 'A'.    "Down payment
        DATA: wa_char50(50), wa_char20(20).
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
             EXPORTING
                  input  = it_bseg-aufnr
             IMPORTING
                  output = it_l-ordno.
        PERFORM get_position_id CHANGING wa_char20
                                         wa_char50.
        IF NOT it_l-ordno IS INITIAL.
          CONCATENATE it_l-ordno '/' wa_char20 INTO it_l-ordno.
        ENDIF.
        IF NOT it_bseg-ebeln IS INITIAL.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
               EXPORTING
                    input  = it_bseg-ebeln
               IMPORTING
                    output = wa_char50.
          it_l-assgn1 = wa_char50.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
               EXPORTING
                    input  = it_bseg-ebelp
               IMPORTING
                    output = wa_char50.
          CONCATENATE it_l-assgn1 '-' wa_char50 INTO it_l-assgn1.
        ENDIF.
      ENDIF.
*=======2003/12/04 downpayment
      SELECT * INTO CORRESPONDING FIELDS OF TABLE  it_t074u
      FROM t074u.
*      WHERE umsks in ('A', 'N', 'T').    "A,  "SAWON== N, T

      CLEAR wa_t_cnt.
      DESCRIBE TABLE it_t074u LINES wa_t_cnt.
      REFRESH : it_lfc3.
      CLEAR   : it_lfc3.
      CLEAR : wa_dp_chk.
      IF wa_t_cnt > 0.
        wa_dp_chk = 'Q'.
        SELECT * INTO CORRESPONDING FIELDS OF TABLE it_lfc3
        FROM lfc3
        FOR ALL ENTRIES IN it_t074u
        WHERE lifnr = it_bseg-lifnr
        AND   bukrs = 'H201'
        AND   gjahr = it_bkpf-gjahr
        AND   shbkz = it_t074u-umskz.

      ENDIF.

      CLEAR wa_t_cnt.
      DESCRIBE TABLE it_lfc3 LINES wa_t_cnt.
      CLEAR : wa_dp, wa_name1, wa_cnt.
      IF wa_t_cnt > 0.
        wa_name1 = it_l-name1.
        LOOP AT it_lfc3 WHERE gjahr = it_bk-gjahr.
          IF it_l-code+0(1) = '1'.     "empolyee
            IF it_lfc3-shbkz = 'N' OR
               it_lfc3-shbkz = 'T'.
              wa_dp = wa_dp + it_lfc3-saldv +
                   it_lfc3-solll - it_lfc3-habnl.
            ENDIF.
          ELSE.
*             IF IT_LFC3-SHBKZ = 'A'.
            wa_dp = wa_dp + it_lfc3-saldv +
                 it_lfc3-solll - it_lfc3-habnl.
*             ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
    WHEN 'M'. "material
    WHEN 'S'. "G/L accounts
******>>> Investment
      IF it_bseg-hkont+4(2) = '16'    OR
         it_bseg-hkont+4(3) = '901'.
*    move  it_bseg-aufnr to it_l-assgn2.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
             EXPORTING
                  input  = it_bseg-aufnr
             IMPORTING
                  output = it_l-assgn1.
        PERFORM get_position_id CHANGING it_l-code
                                         it_l-name1.


      ENDIF.
******>>> Expense
      IF it_bseg-hkont+4(1) = '6' OR
         it_bseg-hkont+4(1) = '7' OR                        "UD1K922288
         it_bseg-hkont+4(6) BETWEEN '531000'                "UD1K922288
                                AND '539999'.               "UD1K922288
        DATA: it_list TYPE TABLE OF ifmkacoo WITH HEADER LINE.
        RANGES: r_kostl FOR bseg-kostl.
        IF ( it_bseg-fistl IS INITIAL ) AND
           ( NOT it_bseg-kostl IS INITIAL ).
          r_kostl-sign = 'I'.
          r_kostl-option = 'EQ'.
          r_kostl-low  = it_bseg-kostl.
          APPEND r_kostl.
          CALL FUNCTION 'HHM_KONT_READ_FROM_CO_OBJEKT'
               EXPORTING
                    i_fikrs = 'H201'
                    i_kokrs = 'H201'
                    i_kostl = 'X'
               TABLES
                    t_kostl = r_kostl
                    t_list  = it_list.

          LOOP AT it_list.
            MOVE it_list-fistl TO it_bseg-fistl.
          ENDLOOP.
        ENDIF.

        SELECT SINGLE bezeich
          FROM fmfctrt                 "Funds Center Text
          INTO it_l-name1
         WHERE fictr = it_bseg-fistl.  "Funds Center
        MOVE it_bseg-fistl TO it_l-code.

      ENDIF.

*******>>> ooder investment 2004/03/24
*      IF it_bseg-aufnr <> ' '.
**    move  it_bseg-aufnr to it_l-assgn2.
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*             EXPORTING
*                  input  = it_bseg-aufnr
*             IMPORTING
*                  output = it_l-aufnr.
*        PERFORM check_iv_order CHANGING it_l-aufnr
*                                        it_l-name1.
*        IF sy-subrc = 0.
*          it_l-code = it_l-aufnr.
*        ENDIF.
*      ENDIF.
*

  ENDCASE.


  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
       EXPORTING
            input  = it_l-code
       IMPORTING
            output = it_l-code.
  IF ( it_l-name1 IS INITIAL AND it_l-code IS INITIAL ) .
    CLEAR : it_l-acct_type.
  ELSE.
    CONCATENATE it_l-name1 '/' it_l-code INTO it_l-acct_type.
  ENDIF.

**--> Tax rate
* Begin of changes - UD1K921056 /UD1K921579
*  if   it_bseg-koart eq 'K' and it_bseg-QSSKZ = 'XX' .

*FIXME - Withholding tax information...
  IF   it_bseg-koart EQ 'K'.
    PERFORM get_taxcode_rate.
  ELSE.
* End of changes -  UD1K921056 / UD1K921579
    DATA: l_rate TYPE p DECIMALS 1.
    MOVE it_bseg-mwskz TO it_l-taxcd.
    IF it_bseg-fwbas NE 0.
      l_rate = it_bseg-dmbtr / it_bseg-fwbas * 100.
      it_l-rate = l_rate.
      SELECT SINGLE text1 INTO it_l-acct_type FROM t007s
        WHERE spras = sy-langu AND
              kalsm = 'TAXUS' AND
              mwskz = it_bseg-mwskz.
    ENDIF.
    DATA: BEGIN OF it_tax OCCURS 0,
             kschl LIKE konp-kschl,
             kbetr LIKE konp-kbetr,
          END OF it_tax.
    DATA: wa_l_tax LIKE bseg-dmbtr.
**--> Parked doc's tax calculate
**--> refrence t-code : FTXP - maintain tax code
    IF it_bseg-mwskz NE space AND  " exist tax code
*      it_bseg-shkzg EQ 'S'   AND  " only debit  (ANDY comment)
       it_bkpf-bstat EQ 'V'.       " only parked doc.
      SELECT b~kschl b~kbetr INTO TABLE it_tax
        FROM a003 AS a INNER JOIN konp AS b ON b~knumh EQ a~knumh
        WHERE a~kappl = 'TX' AND
              a~aland = 'US' AND
              a~mwskz EQ it_bseg-mwskz.
      LOOP AT it_tax WHERE kschl+3(1) EQ 'I'.    "case A/P
        wa_l_tax = it_bseg-dmbtr * it_tax-kbetr / 1000.
        ADD wa_l_tax TO it_l-drt.
*      ADD wa_l_tax TO it_s-sdrt.
*---2004/03/23
        ADD wa_l_tax TO it_s-scrt.
      ENDLOOP.
    ENDIF.
  ENDIF.

  IF sy-linno > 85.
    NEW-PAGE.
    PERFORM write_header_line.
  ENDIF.

**-> detail 1 line
  WRITE:  /(01) wa_vl NO-GAP,
           (03) it_bseg-buzei NO-GAP, wa_vl NO-GAP, " Line No
           (20) it_l-txt20 NO-GAP, wa_vl NO-GAP, " Acct Short Text
           (49) it_l-acct_type  NO-GAP,
           (04) it_l-taxcd      NO-GAP, "Tax Code
           (08) it_l-rate       NO-GAP, "Tax Rate
           (01) wa_vl NO-GAP,
           (20) it_l-dr NO-GAP CURRENCY it_bseg-pswsl NO-ZERO, "Dr Amt
           (01) wa_vl NO-GAP,
           (20) it_l-cr NO-GAP CURRENCY it_bseg-pswsl NO-ZERO, "Cr Amt
           (01) wa_vl.

* 2 line

  DATA: wa_l_ktext    TYPE ktext,
        wa_l_func_area TYPE fkber,
        wa_l_kostl  TYPE char10.
  IF it_bseg-kostl NE ' '.
    CALL FUNCTION 'Z_FFI_GET_KOSTL'
         EXPORTING
              i_kokrs     = 'H201'
              i_kostl     = it_bseg-kostl
         IMPORTING
              e_ktext     = wa_l_ktext
              e_func_area = wa_l_func_area.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
         EXPORTING
              input  = it_bseg-kostl  "Cost Center
         IMPORTING
              output = wa_l_kostl.

    CONCATENATE wa_l_ktext '/' wa_l_kostl '/' wa_l_func_area
           INTO it_l-ordno.

  ENDIF.
**************************2004/04/22
*data : wa_ebeln like bseg-ebeln,
*       wa_ebelp like bseg-ebelp.
*
  IF it_bseg-bschl >  '79' OR
     it_bk-blart  = 'KI'   OR
     it_bk-blart  = 'SV' .
    IF it_bseg-ebeln <> ' '.
      CLEAR it_l-ordno.
      CONCATENATE it_bseg-ebeln '/' it_bseg-ebelp INTO it_l-ordno.
    ENDIF.
    IF it_bseg-aufnr <> ' '.
      it_l-ordno = it_bseg-aufnr.
    ENDIF.
    IF it_bseg-matnr <> ' '.
      CLEAR : it_l-assgn1, it_l-assgn2, wa_menge.
      it_l-assgn1 = it_bseg-matnr.
      wa_menge =  it_bseg-menge.
      it_l-assgn2 = wa_menge.
    ENDIF.
*--sales order
    IF it_bseg-vbel2 <> ' '.
      it_l-ordno = it_bseg-vbel2.
    ENDIF.
  ENDIF.

  IF it_l-assgn1 = ' '.
    it_l-assgn1 = it_bseg-zuonr.
  ENDIF.
*  IF it_l-assgn2 = ' '.
*      wa_menge =  it_bseg-menge.
*     it_l-assgn2 = wa_menge.
*  ENDIF.
*  IF it_l-ordno = ' '.
*    CONCATENATE it_bseg-ebeln '/' it_bseg-ebelp INTO it_l-ordno.
*  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
       EXPORTING
            input  = it_l-ordno
       IMPORTING
            output = it_l-ordno.
***************************************************
  WRITE:/  wa_vl NO-GAP,
         (03) ' ' NO-GAP,  wa_vl NO-GAP,
         (20) it_bseg-hkont NO-GAP, wa_vl NO-GAP,  " G/L Account
         (31) it_l-ordno NO-GAP,
         (18) it_l-assgn1 NO-GAP,
         (12) it_l-assgn2 NO-GAP,
         (01) wa_vl NO-GAP,
         (20) it_l-drt NO-GAP CURRENCY it_bseg-pswsl NO-ZERO, "Dr Amt
         (01) wa_vl NO-GAP,
         (20) it_l-crt NO-GAP CURRENCY it_bseg-pswsl NO-ZERO, "Cr Amt
         (01) wa_vl NO-GAP.
  CLEAR : it_l-assgn1, it_l-assgn2.
  CLEAR : it_l-ordno, wa_menge.
*
* 3 line
*-----2003/12/10
  CLEAR wa_final_txt.
*  IF wa_dp > 0.
*    IF it_bseg-koart <> 'K'.
*      IF wa_cnt = 0.
*        wa_final_txt = 'Final Invoice'.
*        CLEAR wa_cnt.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*----2004/01/15
  IF it_bseg-ebeln <> ' '.
    IF wa_dp_chk = 'Q'.
      IF it_bseg-koart <> 'K'.
        CLEAR wa_f_cnt.
        SELECT COUNT(*) INTO wa_f_cnt
        FROM fdm1
        WHERE ebeln = it_bseg-ebeln
        AND   ebelp = it_bseg-ebelp.
        IF wa_f_cnt = 0.
          wa_final_txt = 'Final Invoice'.
          CLEAR wa_cnt.
          CLEAR wa_dp_chk.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
  WRITE:/ wa_vl NO-GAP,
    (03) ' ' NO-GAP, wa_vl NO-GAP,
*    (20) ' ' NO-GAP, wa_vl NO-GAP,
    (20) wa_final_txt NO-GAP, wa_vl NO-GAP,
    (49) it_bseg-sgtxt NO-GAP.
*---2004/04/29
  IF it_bseg-fdtag <> '00000000 '.
    WRITE : (12) it_bseg-fdtag NO-GAP NO-ZERO, wa_vl NO-GAP.
  ELSE.
    WRITE : (12) it_bseg-zfbdt NO-GAP NO-ZERO, wa_vl NO-GAP.
  ENDIF.
*------------------------------------------*
  IF it_bkpf-waers = 'USD'.
    WRITE:
         (20) ' ' NO-GAP, wa_vl NO-GAP.
*         (20) ' ' NO-GAP, wa_vl NO-GAP.

    IF p_flag EQ true.
* by ig.moon 05/22/2008 {
      DATA : $total TYPE dmbtr,
             s_total(15),
             f_total(18).

      LOOP AT t_it_ekbe.
        AT LAST.
          SUM.
          $total = t_it_ekbe-dmbtr.
        ENDAT.
      ENDLOOP.
      IF $total <> 0.
        WRITE $total TO s_total CURRENCY it_bkpf-waers RIGHT-JUSTIFIED.
        CONCATENATE '(' s_total ')' INTO f_total.
        CONDENSE f_total.
        WRITE:
             (20) f_total NO-GAP RIGHT-JUSTIFIED,
             (01) wa_vl NO-GAP.
        CLEAR p_flag.
      ELSE.
        WRITE:
             (20) space NO-GAP RIGHT-JUSTIFIED,
             (01) wa_vl NO-GAP.
      ENDIF.

    ELSE.
      WRITE: (20) ' ' NO-GAP, wa_vl NO-GAP.
    ENDIF.
* }

  ELSE.

    IF it_bseg-shkzg = 'S'.
      WRITE:
         (20) it_bseg-wrbtr NO-GAP CURRENCY it_bkpf-waers NO-ZERO, "
         (01) wa_vl NO-GAP,
         (20) space NO-GAP,
         (01) wa_vl NO-GAP.
    ELSE.
      WRITE:
         (20) space NO-GAP,
         (01) wa_vl NO-GAP,
         (20) it_bseg-wrbtr NO-GAP CURRENCY it_bkpf-waers NO-ZERO, "
         (01) wa_vl NO-GAP.
    ENDIF.
  ENDIF.


  NEW-LINE. ULINE AT (wa_width).
* Begin of changes  UD1K921499
* For 1042 vendors Print extra line for With Holding tax
  IF it_bseg-koart EQ 'K' AND
     with_item-witht EQ '42' AND  "Only for 1042 print withtax
    (  with_item-wt_qbshb   > 0 OR
      with_item-wt_qbshb  <= 0 ) AND
       with_item-wt_withcd NE 'XX' .           " UD1K921782.
    PERFORM print_withtax.
  ENDIF.
* End of changes - UD1K921499
ENDFORM.

*-------------------------------------------------------*
*       FORM WRITE_Empty_LINE                           *
*-------------------------------------------------------*
FORM write_empty_line.
  DO 3 TIMES.
    WRITE:  /(01) wa_vl NO-GAP,
           (03) space NO-GAP,
           (01) wa_vl NO-GAP, " Line No
           (20) space NO-GAP,
           (01) wa_vl NO-GAP,
           (49) space NO-GAP,
           (08) space NO-GAP, "Tax Code
           (04) space NO-GAP, "Tax Rate
           (01) wa_vl NO-GAP,
           (20) space NO-GAP, "Dr Amt
           (01) wa_vl NO-GAP,
           (20) space NO-GAP, "Cr Amt
           (01) wa_vl.
  ENDDO.
  NEW-LINE. ULINE AT (wa_width).

ENDFORM.

*-------------------------------------------------------*
*       FORM WRITE_HEADER_LINE                          *
*-------------------------------------------------------*
FORM write_header_line.
  DATA: l_u_department   TYPE ad_dprtmnt,
        l_u_name_text    TYPE ad_namtext.
  DATA: l_p_department   TYPE ad_dprtmnt,
        l_p_name_text    TYPE ad_namtext,
        wa_l_reverse(20).
  .

*"     VALUE(E_DEPARTMENT) TYPE  AD_DPRTMNT
*"     VALUE(E_NAME_TEXT) TYPE  AD_NAMTEXT

**--> t001: Company Codes
  SELECT SINGLE * FROM t001
         WHERE bukrs EQ it_bkpf-bukrs.
**--> t003t: Document type
  SELECT SINGLE * FROM t003t
         WHERE spras = sy-langu  AND
               blart = it_bkpf-blart.

**--> get person info.
  CALL FUNCTION 'Z_FFI_GET_PERSON_INFO'
       EXPORTING
            i_usnam      = it_bkpf-usnam
       IMPORTING
            e_department = l_u_department
            e_name_text  = l_u_name_text.

  CALL FUNCTION 'Z_FFI_GET_PERSON_INFO'
       EXPORTING
            i_usnam      = it_bkpf-ppnam
       IMPORTING
            e_department = l_p_department
            e_name_text  = l_p_name_text.

  FORMAT INTENSIFIED  OFF.
  SKIP 6.

  WRITE:  (130) 'Accounting Document' CENTERED.
  WRITE:  (130) '===================' CENTERED.

  SKIP 2.

  WRITE:/    'Company code :', it_bkpf-bukrs, '-',  wa_l_company_name,
        98   'Print date : ' NO-GAP,
              sy-datum  MM/DD/YYYY,
              sy-uzeit  USING EDIT MASK '__:__:__'.

  WRITE:/116 'Page:' NO-GAP, sy-pagno NO-GAP,
             '/ '     NO-GAP,
             (2) wa_total_page_number NO-GAP LEFT-JUSTIFIED.
* check reverse doc
  IF NOT it_bkpf-stblg IS INITIAL.
    IF it_bkpf-stgrd IS INITIAL.
      SELECT SINGLE stgrd INTO it_bkpf-stgrd FROM bkpf
        WHERE bukrs = it_bkpf-bukrs AND
              belnr = it_bkpf-stblg AND
              gjahr = it_bkpf-stjah.
    ENDIF.
    CONCATENATE ' REVERSED('
                it_bkpf-stgrd
                ')'
          INTO  wa_l_reverse.
*Begin of changes - UD1K922256
  ELSEIF it_bkpf-revtx <> space.
    CONCATENATE ' REVERSED('
         it_bkpf-stgrd
         ')'
   INTO  wa_l_reverse.
* End  of changes - UD1K922256
  ENDIF.

  NEW-LINE.  ULINE AT (wa_width).
  WRITE:  /(01) wa_vl NO-GAP,
           (12) 'Doc. No.  : '  NO-GAP CENTERED,
           (10) it_bkpf-belnr      NO-GAP LEFT-JUSTIFIED,
           (13) wa_l_reverse NO-GAP,
           (01) wa_vl NO-GAP,
           (15) 'Posting  Date : '  NO-GAP CENTERED,
           (12) it_bkpf-budat      NO-GAP CENTERED  MM/DD/YYYY,
           (01) wa_vl NO-GAP,
           (10) 'Inv/Ref : '       NO-GAP CENTERED,
           (18) it_bkpf-xblnr      NO-GAP LEFT-JUSTIFIED,
           (01) wa_vl NO-GAP,
           (09) 'Posted : '       NO-GAP CENTERED.
  IF it_bkpf-bstat = ' '.
    WRITE: (10) it_bkpf-usnam      NO-GAP,
           (16) l_u_name_text      NO-GAP,
           (01) wa_vl NO-GAP.

  ELSE.
    WRITE: (10) space NO-GAP,
           (16) space NO-GAP,
           (01) wa_vl NO-GAP.
  ENDIF.

  WRITE:  /(01) wa_vl NO-GAP,
           (12) 'Doc. Type : '  NO-GAP,
           (02) it_bkpf-blart NO-GAP,
           (01) '-' NO-GAP,
           (20) t003t-ltext NO-GAP,
           (01) wa_vl NO-GAP,
           (15) 'Document Date : ' NO-GAP CENTERED,
           (12) it_bkpf-bldat      NO-GAP CENTERED  MM/DD/YYYY,
           (01) wa_vl NO-GAP,
           (15) 'Doc.Currency : '  NO-GAP CENTERED,
           (13) it_bkpf-waers      NO-GAP LEFT-JUSTIFIED,
           (01) wa_vl NO-GAP,
           (09) 'Parked : '    NO-GAP CENTERED.
  IF it_bkpf-bstat = ' '.
    WRITE: (10) it_bkpf-ppnam      NO-GAP,
           (16) l_p_name_text      NO-GAP,
           (01) wa_vl NO-GAP.
  ELSE.
    WRITE: (10) it_bkpf-usnam      NO-GAP,
           (16) l_u_name_text      NO-GAP,
           (01) wa_vl NO-GAP.
  ENDIF.

  NEW-LINE.
  ULINE AT (wa_width).


* Title line 1
  WRITE:/   wa_vl NO-GAP,
            (03) ' '   NO-GAP CENTERED, wa_vl NO-GAP,
            (20) 'GL Account Name' NO-GAP CENTERED, wa_vl NO-GAP,
            (48) 'Account Detail Information' NO-GAP CENTERED,
                 "<- Account Type
            (01) wa_vl NO-GAP,
            (07) 'Tax Cd.' NO-GAP,wa_vl NO-GAP,
            (04) 'Rate' NO-GAP, wa_vl NO-GAP,
            (20) ' '  NO-GAP CENTERED, wa_vl NO-GAP,
            (20) ' ' NO-GAP CENTERED, wa_vl NO-GAP.
* Title line 2
  NEW-LINE.
  WRITE: wa_vl NO-GAP.
  ULINE AT 5(84). WRITE: 109 wa_vl NO-GAP, 130 wa_vl NO-GAP.

* Title line 3
  WRITE:/  wa_vl NO-GAP,
          (03) 'No.' NO-GAP CENTERED,       wa_vl NO-GAP,
          (20) space NO-GAP,                wa_vl NO-GAP,
          (30) 'Cost Center / Order No' NO-GAP CENTERED, wa_vl NO-GAP,
          (17) 'Assignment1'     NO-GAP CENTERED, wa_vl NO-GAP,
          (12) 'Assignment2'     NO-GAP CENTERED, wa_vl NO-GAP,
          (20) 'Debit'  NO-GAP CENTERED, wa_vl NO-GAP,
          (20) 'Credit' NO-GAP CENTERED, wa_vl NO-GAP.

* Title line 4
  WRITE:/  wa_vl NO-GAP,
           (03) ' ' NO-GAP CENTERED,  wa_vl NO-GAP,
           (20) 'GL Account' NO-GAP CENTERED, wa_vl NO-GAP,
           (61) wa_ul NO-GAP, wa_vl NO-GAP,
        109(01) wa_vl NO-GAP,
        130(01) wa_vl NO-GAP.
* Title line 5
  WRITE:/  wa_vl NO-GAP,
           (03) ' ' NO-GAP, wa_vl NO-GAP,
           (20) ' ' NO-GAP, wa_vl NO-GAP,
           (48) 'Text' NO-GAP CENTERED, wa_vl NO-GAP,
           (12) 'Date' NO-GAP CENTERED, wa_vl NO-GAP,
           (20) ' ' NO-GAP, wa_vl NO-GAP,
           (20) ' ' NO-GAP, wa_vl NO-GAP.
  NEW-LINE.
  ULINE AT (wa_width).

*  CLEAR: iline2-belnr.

ENDFORM.

*&------------------------------------------------------*
*&      Form  WRITE_SIGN_BOX
*&------------------------------------------------------*
FORM write_sign_box.

*Begin of changes - UD1K918802
*  IF sy-linno > 56.
  IF sy-linno > 58.
* End of changes - UD1K918802
    NEW-PAGE.
    IF r_a EQ 'X'.
      PERFORM write_header_line.
    ELSEIF r_b EQ 'X'.
      PERFORM write_internal_detail_line.
    ENDIF.
  ENDIF.
  DO.
    IF sy-linno > 56.
      EXIT.
    ENDIF.
    PERFORM write_empty_line.
  ENDDO.
  FORMAT   INTENSIFIED OFF.
  ULINE AT (wa_width).
*----JHS MODIFY 2003/09/04
  IF it_bkpf-bstat = 'S'.
    IF it_bseg-umsks <> 'A'.    "Down payment

      IF it_s-sdr NE it_s-scr.
        WRITE :/ 'This document is uncompleted.'.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.
  WRITE:/  wa_vl NO-GAP,
          (10) ' MEMO : '  NO-GAP.
*-----
  IF wa_dp > 0.
*    IF wa_cnt > 0.
*      wa_final = 'Final Document'.
*    ELSE.
*      wa_final = ' '.
*    ENDIF.
*   WRITE: (30) wa_name1, 'D/Payment ', (12) wa_dp  " (16) wa_final
    WRITE: (37) wa_name1, 'D/Payment ', (22) wa_dp
    NO-GAP, wa_vl NO-GAP.
  ELSE.
    WRITE: (71) space NO-GAP, wa_vl NO-GAP.
  ENDIF.
  CLEAR  : wa_name1, wa_dp, wa_final, wa_cnt.
*  IF it_bseg-umsks EQ 'A'. " down payment
**--JHS MODIFY 2003/09/04
*    IF it_bkpf-BSTAT = 'S'.
*     WRITE: (71) 'This document is uncompleted. (Down payment
*request)'
*             NO-GAP, wa_vl NO-GAP.
*    ELSE.
*      WRITE: (71) space NO-GAP, wa_vl NO-GAP.
*    ENDIF.
*  ELSE.
*    WRITE: (71) space NO-GAP, wa_vl NO-GAP.
*  ENDIF.
*---------------------------------*
  WRITE:
          (04) 'Sum ' NO-GAP CENTERED, wa_vl NO-GAP,
          (20) it_s-sdr NO-GAP CURRENCY it_bseg-pswsl NO-ZERO,
               wa_vl NO-GAP,
          (20) it_s-scr NO-GAP CURRENCY it_bseg-pswsl NO-ZERO,
               wa_vl NO-GAP.

  WRITE:/  wa_vl NO-GAP,
          (81) '      '  NO-GAP, wa_vl NO-GAP.
  IF it_bkpf-bstat = 'V'.
    WRITE:
          (04) 'Tax ' NO-GAP CENTERED, wa_vl NO-GAP.
  ELSE.
    WRITE:
          (04) '    ' NO-GAP CENTERED, wa_vl NO-GAP.
  ENDIF.
  WRITE:
          (20) it_s-sdrt NO-GAP CURRENCY it_bseg-pswsl NO-ZERO,
               wa_vl NO-GAP,
          (20) it_s-scrt NO-GAP CURRENCY it_bseg-pswsl NO-ZERO,
               wa_vl NO-GAP.

  NEW-LINE. ULINE AT (wa_width).

  FORMAT   INTENSIFIED OFF.
*---2003/09/08 jhs modify Requestor
* *Begin of changes - UD1K918802
*  WRITE:/  wa_vl NO-GAP, (11)  space    NO-GAP CENTERED,
*           wa_vl NO-GAP, (40)  'Name'   NO-GAP CENTERED,
*           wa_vl NO-GAP, (40)  'Signature'   NO-GAP CENTERED,
*           wa_vl NO-GAP, (34)  'Date'   NO-GAP CENTERED,
*           wa_vl NO-GAP.
*  WRITE:/  wa_vl NO-GAP, (11) space NO-GAP CENTERED,
*           wa_vl NO-GAP, (117) wa_ul NO-GAP CENTERED.

* WRITE:/  wa_vl NO-GAP, (11) 'Requestor'    NO-GAP CENTERED,
*           wa_vl NO-GAP, (40) space NO-GAP CENTERED,
*           wa_vl NO-GAP, (40) space NO-GAP CENTERED,
*           wa_vl NO-GAP, (34) space NO-GAP CENTERED,
*           wa_vl NO-GAP.
*
*  WRITE:/  wa_vl NO-GAP, (11) space NO-GAP CENTERED,
*           wa_vl NO-GAP, (40) space NO-GAP CENTERED,
*           wa_vl NO-GAP, (40) space NO-GAP CENTERED,
*           wa_vl NO-GAP, (34) space NO-GAP CENTERED,
*           wa_vl NO-GAP.
*
*  WRITE:/  wa_vl NO-GAP, (11) space NO-GAP CENTERED,
*           wa_vl NO-GAP, (40) space NO-GAP CENTERED,
*           wa_vl NO-GAP, (40) space NO-GAP CENTERED,
*           wa_vl NO-GAP, (34) space NO-GAP CENTERED,
*           wa_vl NO-GAP.

* Begin of changes - UD1K919672
*  READ  TABLE it_dupinvh WITH KEY dmbtr = it_bseg-dmbtr.
* by ig.moon 12/16/2008 {
*-  READ  TABLE it_dupinvh WITH KEY dmbtr = ll_wrbtr.
*+
  READ  TABLE it_dupinvh INDEX 1.
* }

  IF sy-subrc EQ 0.
    WRITE:/ wa_vl NO-GAP, (35) 'Document Processing Warnings:' NO-GAP,
           (30) 'Duplicate Invoice :',
           AT 57 it_dupinvh-belnr NO-GAP COLOR COL_NEGATIVE,
           AT 130 wa_vl NO-GAP.

  ELSE.


    IF NOT l_wrbtr IS INITIAL.

* Begin of changes - UD1K921499
*    WRITE:/ wa_vl NO-GAP, (51) 'Document Processing Warnings:' NO-GAP,
*              (77)  space    NO-GAP CENTERED,
*                wa_vl NO-GAP.
      WRITE:/ wa_vl NO-GAP, (35) 'Document Processing Warnings:' NO-GAP,
                (30)  l_text    NO-GAP CENTERED,
                 (57) l_wrbtr NO-GAP, '     ',
                  wa_vl NO-GAP.
    ELSE.

* Begin of changes - UD1K921499
*    WRITE:/ wa_vl NO-GAP, (51) 'Document Processing Warnings:' NO-GAP,
*              (77)  space    NO-GAP CENTERED,
*                wa_vl NO-GAP.
      WRITE:/ wa_vl NO-GAP, (35) 'Document Processing Warnings:' NO-GAP,
                (30)  l_text    NO-GAP CENTERED,
                 (57) space NO-GAP, '     ',
                  wa_vl NO-GAP.


    ENDIF.
* End of changes - UD1K921499
  ENDIF.
* End of changes - UD1K919672

* by ig.moon 5/23/2008 {
  DATA : BEGIN OF it_text OCCURS 10,
          text77(77),
         END OF it_text.
  DATA : BEGIN OF it_text2 OCCURS 10,
          text77(77),
         END OF it_text2.
  DATA : $pos TYPE i.
  DATA : no_pr.
  LOOP AT t_it_ekbe.
    AT END OF ebeln.
      SUM.
      IF t_it_ekbe-dmbtr EQ 0.
        no_pr = true.
      ENDIF.
    ENDAT.
  ENDLOOP.

  IF no_pr  NE space.
    it_text-text77 = 'Downpayment Balance:'.
    APPEND it_text.

    SORT t_it_ekbe BY ebeln.
    LOOP AT t_it_ekbe.
      $pos = 0.
      AT END OF ebeln.
        SUM.
        WRITE t_it_ekbe-ebeln TO it_text-text77+$pos.
        ADD 11 TO $pos.
        WRITE '(' TO it_text-text77+$pos.
        ADD 1 TO $pos.
        WRITE t_it_ekbe-dmbtr TO it_text-text77+$pos(20).
        ADD 21 TO $pos.
        WRITE '),' TO it_text-text77+$pos.
        APPEND it_text.
      ENDAT.
    ENDLOOP.

    DO 3 TIMES.
      READ TABLE it_text INDEX sy-index.
      IF sy-subrc EQ 0.
        SHIFT it_text-text77 RIGHT DELETING TRAILING  ' '.
        SHIFT it_text-text77 RIGHT.
        CONDENSE it_text-text77.
        MODIFY it_text INDEX sy-tabix.
      ELSE.
        CLEAR it_text.
        APPEND it_text.
      ENDIF.
    ENDDO.

    LOOP AT it_text.
      CONCATENATE it_text2-text77 it_text-text77 INTO it_text2-text77.
      $pos = strlen( it_text2-text77 ).
      IF $pos > 50 OR sy-tabix EQ 3.
        APPEND it_text2.
      ENDIF.
    ENDLOOP.

  ENDIF.

  DO 3 TIMES.
    READ TABLE it_text INDEX sy-index.
    IF sy-subrc EQ 0.
    ELSE.
      CLEAR it_text2.
      APPEND it_text2.
    ENDIF.
  ENDDO.

  DO 3 TIMES.
    READ TABLE it_text INDEX sy-index.
    WRITE :/ wa_vl NO-GAP, (51) space NO-GAP ,
                (77)  it_text-text77  NO-GAP, ",
               wa_vl NO-GAP.
  ENDDO.

* }

* End of changes  UD1K918802

*  NEW-LINE.
  WRITE :/ wa_vl NO-GAP,
           (128) wa_ul  NO-GAP,
           wa_vl NO-GAP.

  WRITE :/ wa_vl NO-GAP,
           (128) wa_ul  NO-GAP,
           wa_vl NO-GAP.
*--------Department approval  date finance approval date
  WRITE:/  wa_vl NO-GAP, (22)  'Req.Department'   NO-GAP CENTERED,
           wa_vl NO-GAP, (29)  'Approval'     NO-GAP CENTERED,
           wa_vl NO-GAP, (12)  'Date'         NO-GAP CENTERED,
           wa_vl NO-GAP, (20)  'Finance Div.'      NO-GAP CENTERED,
           wa_vl NO-GAP, (29)  'Approval'     NO-GAP CENTERED,
           wa_vl NO-GAP, (11)  'Date'         NO-GAP CENTERED,
           wa_vl NO-GAP.
  WRITE :/ wa_vl NO-GAP,
           (128) wa_ul  NO-GAP,
           wa_vl NO-GAP.

*---------------------Head of department
  WRITE:/  wa_vl NO-GAP, (22)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (29)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (12)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (20)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (29)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (11)  space   NO-GAP CENTERED,
           wa_vl NO-GAP.
* Begin of changes - UD1K918802
*  WRITE:/  wa_vl NO-GAP, (22)  'Head of Department'   NO-GAP CENTERED,
*           wa_vl NO-GAP, (29)  space    NO-GAP CENTERED,
*           wa_vl NO-GAP, (12)  space    NO-GAP CENTERED,
*           wa_vl NO-GAP, (20)  'Cost Accounting'      NO-GAP CENTERED,
*           wa_vl NO-GAP, (29)  space     NO-GAP CENTERED,
*           wa_vl NO-GAP, (11)  space     NO-GAP CENTERED,
*           wa_vl NO-GAP.
  WRITE:/  wa_vl NO-GAP, (22)  'Requestor'   NO-GAP CENTERED,
           wa_vl NO-GAP, (29)  space NO-GAP CENTERED,
           wa_vl NO-GAP, (12)  space    NO-GAP CENTERED,
*           wa_vl NO-GAP, (20)  'General Accounting'       NO-GAP
*CENTERED,
           wa_vl NO-GAP, (20)  'Processor'       NO-GAP
CENTERED,
           wa_vl NO-GAP, (29)  space     NO-GAP CENTERED,
           wa_vl NO-GAP, (11)  space     NO-GAP CENTERED,
           wa_vl NO-GAP.
* End of changes - UD1K918802


*
* WRITE:/  wa_vl NO-GAP, (22)  space NO-GAP CENTERED,"UD1K918802
  WRITE:/  wa_vl NO-GAP, (22)  space  NO-GAP CENTERED,
          wa_vl NO-GAP, (29)  space   NO-GAP CENTERED,
          wa_vl NO-GAP, (12)  space   NO-GAP CENTERED,
          wa_vl NO-GAP, (20)  space  NO-GAP CENTERED,
          wa_vl NO-GAP, (29)  space   NO-GAP CENTERED,
          wa_vl NO-GAP, (11)  space   NO-GAP CENTERED,
          wa_vl NO-GAP.
  WRITE :/ wa_vl NO-GAP,
           (128) wa_ul  NO-GAP,
           wa_vl NO-GAP.

*---------------------Head of sub division
  WRITE:/  wa_vl NO-GAP, (22)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (29)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (12)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (20)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (29)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (11)  space   NO-GAP CENTERED,
           wa_vl NO-GAP.
*
* Begin of changes - UD1K918802
*  WRITE:/  wa_vl NO-GAP, (22)  'Head of Sub Division'   NO-GAP CENTERED
*,
*           wa_vl NO-GAP, (29)  space    NO-GAP CENTERED,
*           wa_vl NO-GAP, (12)  space    NO-GAP CENTERED,
*           wa_vl NO-GAP, (20)  'General Accounting'  NO-GAP CENTERED,
*           wa_vl NO-GAP, (29)  space     NO-GAP CENTERED,
*           wa_vl NO-GAP, (11)  space     NO-GAP CENTERED,
*           wa_vl NO-GAP.
  WRITE:/  wa_vl NO-GAP, (22)  'Head of Department' NO-GAP CENTERED,
           wa_vl NO-GAP, (29)  space    NO-GAP CENTERED,
           wa_vl NO-GAP, (12)  space    NO-GAP CENTERED,
*           wa_vl NO-GAP, (20)  'Cost Controlling'  NO-GAP CENTERED,
           wa_vl NO-GAP, (20)  'Manager'  NO-GAP CENTERED,
           wa_vl NO-GAP, (29)  space     NO-GAP CENTERED,
           wa_vl NO-GAP, (11)  space     NO-GAP CENTERED,
           wa_vl NO-GAP.
* End of changes - UD1K918802
*
  WRITE:/  wa_vl NO-GAP, (22)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (29)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (12)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (20)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (29)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (11)  space   NO-GAP CENTERED,
           wa_vl NO-GAP.
  WRITE :/ wa_vl NO-GAP,
           (128) wa_ul  NO-GAP,
           wa_vl NO-GAP.

*---------------------Head of  division
  WRITE:/  wa_vl NO-GAP, (22)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (29)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (12)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (20)  'Head of Department'   NO-GAP CENTERED,
           wa_vl NO-GAP, (29)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (11)  space   NO-GAP CENTERED,
           wa_vl NO-GAP.
*
* Begin of changes - UD1K918802
*  WRITE:/  wa_vl NO-GAP, (22)  'Head of Division'   NO-GAP CENTERED,
*           wa_vl NO-GAP, (29)  space    NO-GAP CENTERED,
*           wa_vl NO-GAP, (12)  space    NO-GAP CENTERED,
*           wa_vl NO-GAP, (20)  'Treasurer'  NO-GAP CENTERED,
*           wa_vl NO-GAP, (29)  space     NO-GAP CENTERED,
*           wa_vl NO-GAP, (11)  space     NO-GAP CENTERED,
*           wa_vl NO-GAP.
  WRITE:/  wa_vl NO-GAP, (22)  'Chief of Division'   NO-GAP CENTERED,
           wa_vl NO-GAP, (29)  space    NO-GAP CENTERED,
           wa_vl NO-GAP, (12)  space    NO-GAP CENTERED,
           wa_vl NO-GAP, (20)  '          ----------' NO-GAP CENTERED,
           wa_vl NO-GAP, (29)  '                 -------------'
           NO-GAP CENTERED,
           wa_vl NO-GAP, (11)  '-------------'    NO-GAP CENTERED,
           wa_vl NO-GAP.
* End of changes - UD1K918802
*
  WRITE:/  wa_vl NO-GAP, (22)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (29)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (12)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (20)  '          |Treasury '   NO-GAP CENTERED,
           wa_vl NO-GAP, (29)  '      |              '
           NO-GAP CENTERED,
           wa_vl NO-GAP, (11)  space   NO-GAP CENTERED,
           wa_vl NO-GAP.
  WRITE :/ wa_vl NO-GAP,
           (128) wa_ul  NO-GAP,
           wa_vl NO-GAP.

*---------------------President / CEO
  WRITE:/  wa_vl NO-GAP, (22)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (29)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (12)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (20)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (29)  space   NO-GAP CENTERED,
           wa_vl NO-GAP, (11)  space   NO-GAP CENTERED,
           wa_vl NO-GAP.
*
*WRITE:/wa_vl NO-GAP, (22)'President / CEO' NO-GAP CENTERED,"UD1K918802
  WRITE:/  wa_vl NO-GAP, (22)  'President/CEO'   NO-GAP CENTERED,
          wa_vl NO-GAP, (29)  space    NO-GAP CENTERED,
          wa_vl NO-GAP, (12)  space    NO-GAP CENTERED,
          wa_vl NO-GAP, (20)  'CFO'  NO-GAP CENTERED,
          wa_vl NO-GAP, (29)  space     NO-GAP CENTERED,
          wa_vl NO-GAP, (11)  space     NO-GAP CENTERED,
          wa_vl NO-GAP.
*
  WRITE:/ wa_vl NO-GAP, (22)  space NO-GAP CENTERED,        "8802
             wa_vl NO-GAP, (29)  space   NO-GAP CENTERED,
             wa_vl NO-GAP, (12)  space   NO-GAP CENTERED,
             wa_vl NO-GAP, (20)  space   NO-GAP CENTERED,
             wa_vl NO-GAP, (29)  space   NO-GAP CENTERED,
             wa_vl NO-GAP, (11)  space   NO-GAP CENTERED,
             wa_vl NO-GAP.
  WRITE :/ wa_vl NO-GAP,
           (128) wa_ul  NO-GAP,
           wa_vl NO-GAP.

ENDFORM.                    " DIspaceLAY_END_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  get_taxcode_rate
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_taxcode_rate.
* Begin of changes - UD1K921549
* Get Withholding Tax
  DATA :  l_wrbtr TYPE bseg-wrbtr.
*  select single * from lfbw
*     where   lifnr eq it_bseg-lifnr and
*             BUKRS eq it_bseg-bukrs.
*  if sy-subrc eq 0.
*    select single * from t059z  where land1 = 'US'
*                             and witht = lfbw-witht
*                             and wt_withcd = lfbw-WT_WITHCD.
*    if t059Z-XQFOR eq 'X' .
*      select single * from T059FB where land1 = t059Z-land1 and
*                                        witht =  t059z-witht and
*                                       wt_withcd = t059z-wt_withcd.
*      it_l-rate =  T059FB-QSATZ.
**            it_l-taxcd =    T059FB-wt_withcd.     "UD1K921499
*    else.
*      it_l-rate =  t059Z-qsatz.
**            it_l-taxcd =    t059Z-wt_withcd.     "UD1K921499
*    endif.
*  endif.
* Begin of changes - UD1K921499 / UD1K921549
  SELECT SINGLE * FROM with_item
         WHERE bukrs  = it_bseg-bukrs AND
               belnr  = it_bseg-belnr AND
               gjahr  = it_bseg-gjahr AND
               buzei  = it_bseg-buzei .
*               WITHT =  '42'.
  IF sy-subrc EQ 0.
    SELECT SINGLE * FROM t059p WHERE land1 = 'US' AND
                                     witht = with_item-witht.
    IF sy-subrc EQ 0 AND t059p-wt_postm EQ '2'.
      SELECT SINGLE * FROM t059fb WHERE land1 = 'US' AND
                        witht =     with_item-witht AND
                        wt_withcd = with_item-wt_withcd.
      IF sy-subrc EQ 0.
* * Deduct With holding Tax from Credit Amount
        it_l-taxcd =  with_item-wt_withcd.                  "UD1K921782
        it_l-rate =  t059fb-qsatz.
        CLEAR l_wrbtr.
        l_wrbtr = it_bseg-wrbtr *  it_l-rate / 100.
        it_l-cr = it_l-cr - l_wrbtr.
      ENDIF.
    ENDIF.
  ENDIF.
* Deduct With holding Tax from Credit Amount
*  clear l_wrbtr.
*  if   WITH_ITEM-WITHT eq '42'.
*    l_wrbtr = it_bseg-wrbtr *  it_l-rate / 100.
*    it_l-cr = it_l-cr - l_wrbtr.
*  endif.

* End of changes - UD1K921499
ENDFORM.                    " get_taxcode_rate
*&---------------------------------------------------------------------*
*&      Form  print_withtax
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM print_withtax.

  CLEAR l_wrbtr.
  l_wrbtr =  it_bseg-wrbtr * it_l-rate / 100.
  IF with_item-wt_qbshb <> 0 .
    l_text = 'Withholding Tax amount  held' .
  ELSE.
    l_text = 'Withholding Tax amount to be withheld' .
  ENDIF.
  with_flag = 'X'.
* 1st Line
  WRITE:  /(01) wa_vl NO-GAP,
        (03) '' NO-GAP, wa_vl NO-GAP, " Line No
        (20) '' NO-GAP, wa_vl NO-GAP, "Acct Short
        (49) l_text NO-GAP,
        (04) '' NO-GAP, "Tax Code
        (08) '' NO-GAP, "Tax Rate
        (01) wa_vl NO-GAP,
        (20) '' NO-GAP CURRENCY it_bseg-pswsl NO-ZERO, "Dr Amt
        (01) wa_vl NO-GAP,
        (20) l_wrbtr NO-GAP CURRENCY it_bseg-pswsl NO-ZERO, "Cr Amt
        (01) wa_vl.
* 2nd Line
  WRITE:/  wa_vl NO-GAP,
          (03) ' ' NO-GAP,  wa_vl NO-GAP,
          (20) '' NO-GAP, wa_vl NO-GAP,  " G/L Account
          (31) '' NO-GAP,
          (18) '' NO-GAP,
          (12) '' NO-GAP,
          (01) wa_vl NO-GAP,
          (20) '' NO-GAP CURRENCY it_bseg-pswsl NO-ZERO, "Dr Amt
          (01) wa_vl NO-GAP,
          (20) '' NO-GAP CURRENCY it_bseg-pswsl NO-ZERO, "Cr Amt
          (01) wa_vl NO-GAP.
* 3rd Line
  WRITE:/ wa_vl NO-GAP,
      (03) ' ' NO-GAP, wa_vl NO-GAP,
      (20) '' NO-GAP, wa_vl NO-GAP,
      (49) '' NO-GAP.
*---2004/04/29
  IF it_bseg-fdtag <> '00000000 '.
    WRITE : (12) '' NO-GAP NO-ZERO, wa_vl NO-GAP.
  ELSE.
    WRITE : (12) '' NO-GAP NO-ZERO, wa_vl NO-GAP.
  ENDIF.
*------------------------------------------*
  IF it_bkpf-waers = 'USD'.
    WRITE:
         (20) ' ' NO-GAP, wa_vl NO-GAP,
         (20) ' ' NO-GAP, wa_vl NO-GAP.
  ELSE.
    IF it_bseg-shkzg = 'S'.
      WRITE:
         (20) '' NO-GAP CURRENCY it_bkpf-waers NO-ZERO, "
         (01) wa_vl NO-GAP,
         (20) space NO-GAP,
         (01) wa_vl NO-GAP.
    ELSE.
      WRITE:
         (20) space NO-GAP,
         (01) wa_vl NO-GAP,
         (20) '' NO-GAP CURRENCY it_bkpf-waers NO-ZERO, "
         (01) wa_vl NO-GAP.
    ENDIF.
  ENDIF.


  NEW-LINE. ULINE AT (wa_width).


ENDFORM.                    " print_withtax
