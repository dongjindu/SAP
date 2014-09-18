*"  IMPORTING
*"     VALUE(FISCAL_YEAR) LIKE  TXW_DIR2-FISC_YEAR
*"     VALUE(PACKAGE_SIZE) TYPE  I
*"     VALUE(MAX_DOCS) LIKE  TXW_COMMON-MAX_DOCS
*"  TABLES
*"      SEL_COMPANY_CODE STRUCTURE  TXW_SBUKRS
*"      SEL_PERIOD STRUCTURE  TXW_SPERIO
*"      SEL_PARMS STRUCTURE  RSPARAMS
*"      SEL_CUST_SEGMENT STRUCTURE  TXW_SEXPST
DATA: BEGIN OF customer_struct,
zdglfunct LIKE txw_c_strc-exp_struct VALUE 'ZDGLFUNCT',
END OF customer_struct.
DATA: tsl LIKE glfunct-tsl01,
      hsl LIKE glfunct-hsl01,
      ksl LIKE glfunct-ksl01,
      msl LIKE glfunct-msl01,
      period LIKE sel_period-low.
DATA: unit_hsl LIKE tcurc-waers,
      unit_ksl LIKE tcurc-waers.
* ledgers that are valid for table ZZXXXX
DATA: BEGIN OF tab_ledger OCCURS 0,
                rldnr LIKE t881-rldnr,
                curt1 LIKE t881-curt1,
                curt2 LIKE t881-curt2,
      END OF tab_ledger.
* ledgers that are valid for current company code
DATA: BEGIN OF ledger OCCURS 0,
                rldnr LIKE t881-rldnr,
      END OF ledger.
*....... new data source: FI-SL table GLFUNCT ...*
* get ledgers for summary table GLFUNCT
SELECT rldnr curt1 curt2 FROM t881
  INTO CORRESPONDING FIELDS OF TABLE tab_ledger
  WHERE tab = 'GLFUNCT'.

* initialize new data segment
CALL FUNCTION 'TXW_SEGMENT_WRITE_INIT'
     EXPORTING
          export_structure = customer_struct-zdglfunct.

* individual selection by company code
LOOP AT sel_company_code.
* get ledgers that are valid for current company code
  SELECT rldnr FROM t882
    INTO CORRESPONDING FIELDS OF TABLE ledger
    FOR ALL ENTRIES IN tab_ledger
    WHERE rldnr = tab_ledger-rldnr
      AND bukrs = sel_company_code-low.
* individual selection by ledger
  LOOP AT ledger.
* get currency units
    READ TABLE tab_ledger WITH KEY rldnr = ledger-rldnr.
    CALL FUNCTION 'G_CURRENCY_FROM_CT_GET'
         EXPORTING
              bukrs    = sel_company_code-low
              ct       = tab_ledger-curt1
              rldnr    = ledger-rldnr
         IMPORTING
              currency = unit_hsl
         EXCEPTIONS
              OTHERS   = 15.
    CALL FUNCTION 'G_CURRENCY_FROM_CT_GET'
         EXPORTING
              bukrs    = sel_company_code-low
              ct       = tab_ledger-curt2
              rldnr    = ledger-rldnr
         IMPORTING
              currency = unit_ksl
         EXCEPTIONS
              OTHERS   = 15.
* read and export records
    SELECT * FROM glfunct
    WHERE rldnr = ledger-rldnr "ledger
    AND rvers = '001' "actual version
    AND rrcty = '0' "actual data
    AND rbukrs = sel_company_code-low
    AND ryear = fiscal_year.
      MOVE-CORRESPONDING glfunct TO zdglfunct.
* create one record for each period in SEL_PERIOD
      DO 16 TIMES VARYING tsl FROM glfunct-tsl01
      NEXT glfunct-tsl02
      VARYING hsl FROM glfunct-hsl01
      NEXT glfunct-hsl02
      VARYING ksl FROM glfunct-ksl01
      NEXT glfunct-ksl02
      VARYING msl FROM glfunct-msl01
      NEXT glfunct-msl02.
        zdglfunct-rpmax = sy-index. "set period
        IF zdglfunct-rpmax IN sel_period.
* convert value fields
          PERFORM write_currency "convert KSL to character
          USING tsl
          zdglfunct-rtcur
          zdglfunct-tsl01.
          PERFORM write_currency "convert KSL to character
          USING hsl
          unit_hsl
          zdglfunct-hsl01.
          PERFORM write_currency "convert KSL to character
          USING ksl
          unit_ksl
          zdglfunct-ksl01.
          PERFORM write_quantity "convert ASL to character
          USING msl
          zdglfunct-runit
          zdglfunct-msl01.
* export record
          CALL FUNCTION 'TXW_SEGMENT_RECORD_EXPORT'
               EXPORTING
                    data_record = zdglfunct.
        ENDIF.
      ENDDO.
    ENDSELECT.
  ENDLOOP. "LOOP AT LEDGER.
ENDLOOP. "LOOP AT SEL_COMPANY_CODE.
* close data segment
CALL FUNCTION 'TXW_SEGMENT_WRITE_CLOSE'.
