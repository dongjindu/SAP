REPORT RFZALI00        MESSAGE-ID 0.
*  LINE-SIZE 80
*  LINE-COUNT 65.
*  NO STANDARD PAGE HEADING.

TABLES:  T012, T012K, BNKA, t042t,
         REGUH,  REGUP,  REGUPW, T042Z, BHDGD.
*

*DATA: REG LIKE REGUH OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF ZAHLBED,
        TAGE(3)    TYPE C,    "(3) gen?t - analog zum Eingabefeld
        STRI(1)    TYPE C VALUE '/',
        PROZ(6)    TYPE C,
      END OF ZAHLBED.

* for send to bank
TYPES: BEGIN OF REG_TYPE,

        UBNKS  LIKE REGUH-UBNKS,
        UBKNT  LIKE REGUH-UBKNT,  "Our account number at the bank
        UBNKL  LIKE REGUH-UBNKL,

        ZBNKL  LIKE REGUH-ZBNKL,
        ZBNKN  LIKE REGUH-ZBNKN,
        ZLAND  LIKE REGUH-ZLAND,
        KOINH  LIKE REGUH-KOINH,
        VALUT  LIKE REGUH-VALUT,
        RBETR  LIKE REGUH-RBETR,
        ZNME1  LIKE REGUH-ZNME1,
        LAUFD  LIKE REGUH-LAUFD,
        XVORL  LIKE REGUH-XVORL,
        ZTELF  LIKE REGUH-ZTELF,
        LAUFI  LIKE REGUH-LAUFI,
        ZBUKR  LIKE REGUH-ZBUKR,
        LIFNR  LIKE REGUH-LIFNR,
        KUNNR  LIKE REGUH-KUNNR,
        EMPFG  LIKE REGUH-EMPFG,
        VBLNR  LIKE REGUH-VBLNR,
        NAME1  LIKE REGUH-NAME1,
        RZAWE  LIKE REGUH-RZAWE,
      END OF REG_TYPE.

DATA: REG TYPE REG_TYPE OCCURS 0 WITH HEADER LINE.

* for internal report
TYPES: BEGIN OF REG2_TYPE,
        UBNKL  LIKE REGUH-UBNKL,  "Bank number of our bank
        LIFNR  LIKE REGUH-LIFNR,  "Account number of vendor or creditor
        VBLNR  LIKE REGUH-VBLNR,  "Document Number of the Payment
        ZBNKL  LIKE REGUH-ZBNKL,  "Bank number of the payee's bank
        UBNKS  LIKE REGUH-UBNKS,
        UBKNT  LIKE REGUH-UBKNT,  "Our account number at the bank
        ZBNKN  LIKE REGUH-ZBNKN,
        KOINH  LIKE REGUH-KOINH,
        VALUT  LIKE REGUH-VALUT,
        RBETR  LIKE REGUH-RBETR,
        ZNME1  LIKE REGUH-ZNME1,
        LAUFD  LIKE REGUH-LAUFD,
        XVORL  LIKE REGUH-XVORL,
        ZTELF  LIKE REGUH-ZTELF,
        LAUFI  LIKE REGUH-LAUFI,
        ZBUKR  LIKE REGUH-ZBUKR,
        KUNNR  LIKE REGUH-KUNNR,
        EMPFG  LIKE REGUH-EMPFG,
        NAME1  LIKE REGUH-NAME1,
        RZAWE  LIKE REGUH-RZAWE,
      END OF REG2_TYPE.

DATA: REG2 TYPE REG2_TYPE OCCURS 0 WITH HEADER LINE.

DATA: FLAG,
      ZLIFNR LIKE REGUH-LIFNR,
      ZUBNKL LIKE REGUH-UBNKL,
      ZVBLNR LIKE REGUH-VBLNR,
      ZZBNKL LIKE REGUH-ZBNKL.
*

DATA: LINE_CNT     TYPE I VALUE 0,
      TTITLE(25),
      ABZFW(8)     TYPE P,
      NETTO(8)     TYPE P,
      WREGUP-DMBTR LIKE REGUP-DMBTR,
      WREGUP-WRBTR LIKE REGUP-WRBTR,
      WREGUP-SKNTO LIKE REGUP-SKNTO,
      WREGUP-WSKTO LIKE REGUP-WSKTO,
      WREGUP-QBSHB LIKE REGUP-QBSHB,
      T_AMOUNT     TYPE P DECIMALS 2 VALUE 0.

DATA: C_WRBTR_TOTAL LIKE REGUP-DMBTR,
      C_ABZFW_TOTAL LIKE REGUP-DMBTR,
      C_NETTO_TOTAL LIKE REGUP-DMBTR,
      V_WRBTR_TOTAL LIKE REGUP-DMBTR,
      V_ABZFW_TOTAL LIKE REGUP-DMBTR,
      V_NETTO_TOTAL LIKE REGUP-DMBTR.

DATA:
      LAUFD(8)     TYPE C.


tables: t001, fdis50_tab.
data:   bnkko_err-flag,         "bank not exist
        g_bnkko_err-flag,       "bank no error
        formular_open-flag.     "form open flag

data: abtab-foname  like rfpdo1-fdisform.

data: sum-dmshb like fdes-dmshb,
      sum-wrshb like fdes-wrshb,
      fodevice(20).
data: hlp_found like sy-tfill.
data: o_print_flag.
data: begin of err_fo_tab occurs 5,
        name(30),
      end of err_fo_tab.

data: begin of errtab_t018d occurs 5,
        bukrs like t001-bukrs,
      end of errtab_t018d.


* for sapscript
data: begin of ibank1,                 "paying bank
         banks like   bnka-banks,
         bankl like   bnka-bankl ,    "bank key
         banka like   bnka-banka ,    "name of bank
         ernam like   bnka-ernam ,
         provz like   bnka-provz ,    "Region
         stras like   bnka-stras ,    "street
         ort01 like   bnka-ort01 ,    "city
         bankn like   t012k-bankn ,
         banko like   t012k-hkont ,
      end of ibank1,

     begin of ibank2,                  "receiving
        banks like   bnka-banks ,
        bankl like   bnka-bankl ,
        ernam like   bnka-ernam ,
        banka like   bnka-banka ,
        provz like   bnka-provz ,
        stras like   bnka-stras ,
        ort01 like   bnka-ort01 ,
        bankn like   t012k-bankn ,

        datum like   fdes-datum,
        dmshb like   fdes-dmshb,
        wrshb like   fdes-wrshb,

     end of ibank2.

*INCLUDE RFDBRMAC.

*BEGIN_OF_BLOCK 1.
*SELECT-OPTIONS:
*  ZAHLWEG      FOR  REGUH-RZAWE,
*  ZUSATZ       FOR  REGUH-UZAWE,
*  SACHBEAR     FOR  REGUH-BUSAB,
*  ZGSBER       FOR  REGUH-SRTGB,
*  HAUSBANK     FOR  REGUH-HBKID,
*  KKONTO       FOR  REGUH-LIFNR MATCHCODE OBJECT KRED,
*  DKONTO       FOR  REGUH-KUNNR MATCHCODE OBJECT DEBI,
*  POKENKZ      FOR  REGUP-POKEN.
*END_OF_BLOCK 1.
*

* Fix
SELECTION-SCREEN BEGIN OF BLOCK 2 WITH FRAME.
PARAMETERS: PREVIEW AS CHECKBOX.

parameters: foname  like rfpdo1-fdisform DEFAULT 'Z_F_FDIS',
            FR_PRT  like rfpdo1-dzisfode,
            p_RZAWE like reguh-RZAWE  default 'W'.


SELECTION-SCREEN END OF BLOCK 2.
PARAMETERS: BUKRS        LIKE T001-BUKRS memory id BUK.
PARAMETERS: TITLE        LIKE RFPDO-ALLGLINE.

*at  selection-screen  on foname.
*  if foname <> space.
*    call function 'SELECT_FORM'
*         exporting
*              form       = foname
*         importing
*              entries    = hlp_found
*         tables
*              selections = selections.
*    if hlp_found = 0.
*      message e049(f3) with foname .
*    endif.
*  endif.

*-----------------------------------------------------------------------
START-OF-SELECTION.
*-----------------------------------------------------------------------
start-of-selection.
  if FR_PRT = 'X'.
    fodevice = 'PRINTER'.
  else.
    fodevice = 'SCREEN'.
  endif.

  select single * from t042t  "text
       where bukrs = bukrs.

  IF PREVIEW = 'X' AND ZW_XVORL = 'X'.
    write:/ 'Correct report option !!!'.
  else.
    IF PREVIEW = 'X'.
*    PERFORM SEND_TO_BANK_RTN.
      PERFORM SEND_TO_BANK_RTN2.
    ELSE.
      PERFORM INTERNAL_APPROVE_RTN.
    ENDIF.
  endif.

*-----------------------------------------------------------------------
END-OF-SELECTION.
*-----------------------------------------------------------------------

TOP-OF-PAGE.

  check PREVIEW = SPACE.
  MOVE: SPACE    TO BHDGD-LINE1,
        SPACE    TO BHDGD-LINE2,
        TITLE    TO BHDGD-LINE2.
  IF ZW_XVORL EQ SPACE.
    MOVE TEXT-008 TO BHDGD-LINE1.
  ELSE.
    MOVE TEXT-007 TO BHDGD-LINE1.
  ENDIF.
  WRITE ZW_LAUFD TO LAUFD DD/MM/YY.
  REPLACE:
     '&LAUFD' WITH    LAUFD INTO BHDGD-LINE1,
     '&LAUFI' WITH ZW_LAUFI INTO BHDGD-LINE1.
  BHDGD-INIFL = '0'.


  FORMAT INTENSIFIED.
  PERFORM BATCH-HEADING(RSBTCHH0).


  PERFORM WRITE_INTER_HEADER_RTN.

*  IF REG-XVORL = 'X'.
*    TTITLE = 'Payment proposal list'.
*  ELSE.
*    TTITLE = 'Payment Order list'.
*  ENDIF.

*
END-OF-PAGE.

*  WRITE: / '1470 Valley Road, NJ 07444, Telephone: 1-800-fly-fast.'.
*  WRITE: / 'Hankook Tire is forever'.
*
*---------------------
FORM SEND_TO_BANK_RTN.
*---------------------
  SELECT * FROM REGUH INTO CORRESPONDING FIELDS OF TABLE REG
    WHERE LAUFD = ZW_LAUFD
      AND LAUFI = ZW_LAUFI
      AND XVORL = ZW_XVORL
      AND VBLNR <> ''.

  CHECK SY-SUBRC = 0.

  PERFORM CALL_FUNCTION_SET_PRINT.

  SORT REG BY UBNKL ZBNKL.
  FLAG = '*'.

  LOOP AT REG.
    IF REG-UBNKL <> ZUBNKL.    "Our Bank No
      NEW-PAGE.
      PERFORM WRITE_BANK_HEADER_RTN.
      T_AMOUNT = 0.
    ENDIF.

    PERFORM WRITE_BANK_DETAIL_RTN.

    ZUBNKL = REG-UBNKL.

    AT END OF UBNKL.
      WRITE: /57 'Total:', T_AMOUNT.
      ULINE /1(80).
    ENDAT.

  ENDLOOP.

ENDFORM.

*--------------------------
FORM WRITE_BANK_HEADER_RTN.
*--------------------------

  SELECT SINGLE * FROM BNKA
                 WHERE BANKS = REG-UBNKS
                   AND BANKL = REG-UBNKL.

  SKIP 3.
  WRITE: / 'TO:', 40 'DATE:', REG-LAUFD.
  WRITE: / BNKA-BANKA.
  WRITE: /40 'Your account with us:', REG-UBKNT.
  WRITE: / BNKA-STRAS, 40 'Our account with you:', REG-UBNKL.
  WRITE: / BNKA-ORT01.
  SKIP 3.
  WRITE: / 'Dear Sir or Madam:'.
  WRITE: / 'With this letter, we are confirming the listed',
           'payment orders.'.
  SKIP 3.
  WRITE: / 'Kind regards'.
  SKIP 3.
  ULINE /1(80).
  WRITE: /(10) 'Bank number', (18) 'Account',
          (20) 'Account holder', 'Value date',
          (18) 'Amount in USD' RIGHT-JUSTIFIED.
  WRITE: /(35) 'Name of the financial institution', 'Telephone'.
  ULINE /1(80).

ENDFORM.
*----------------------------------------------------------------------
* script print out
*----------------------------------------------------------------------
FORM SEND_TO_BANK_RTN2.

* select wire transfer document (after posting)
  SELECT * FROM REGUH INTO CORRESPONDING FIELDS OF TABLE REG
    WHERE LAUFD = ZW_LAUFD
      AND LAUFI = ZW_LAUFI
      AND XVORL = ZW_XVORL
      AND VBLNR <> ''
      AND RZAWE = p_rzawe.

  CHECK SY-SUBRC = 0.

  SORT REG BY UBNKL ZBNKL.

  FLAG = '*'.

  perform open_formularprint.

  LOOP AT REG.
    at new ubnkl.  " our bank
      SELECT SINGLE * FROM BNKA
                 WHERE BANKS = REG-UBNKS
                   AND BANKL = REG-UBNKL.
*      REG-LAUFD. date
      ibank1-banka = BNKA-BANKA.
      ibank1-bankn = REG-UBKNT.
      ibank1-bankl = REG-UBNKL.
      ibank1-ort01 = BNKA-ORT01.
      ibank1-stras = BNKA-STRAS.
      ibank1-provz = BNKA-PROVZ.
      select single * from t012  where bankl = reg-ubnkl.
      select single hkont from t012k into ibank1-banko
                   where bukrs = bukrs
                     and hbkid = t012-hbkid
                     and bankn = reg-ubknt.

      sum-dmshb = 0.
      sum-wrshb = 0.
      if bnkko_err-flag = space.
        perform start_formularprint.
      endif.
    endat.

    if bnkko_err-flag = space.
      if g_bnkko_err-flag = space.
        perform get_payee_bank.
        perform single_item_print.
*        sum-dmshb = sum-dmshb + ibank2-dmshb.
        sum-wrshb = sum-wrshb + ibank2-wrshb.
      else.
*        perform  err_bnkko_collect using abtab-gbukr abtab-ggrup.
      endif.
    else.
*      perform err_bnkko_collect using   abtab-bukrs abtab-grupp.
    endif.

*    ZUBNKL = REG-UBNKL.

    AT END OF UBNKL.
      if bnkko_err-flag = space.
        perform end_formularprint.
      endif.

    ENDAT.

  ENDLOOP.

  perform close_formularprint.

ENDFORM.

*---------------------------------------------------------------------*
*  SUBROUTINES -------------------------------------------------------*
*---------------------------------------------------------------------*

form open_formularprint.
  call function 'OPEN_FORM'
       EXPORTING
            form     = foname
            device   = fodevice
            dialog   = 'X'
       EXCEPTIONS
            canceled = 1
            device   = 2
            form     = 3
            options  = 4
            unclosed = 5.

  case sy-subrc.
    when 0.
      formular_open-flag = 'X'.
    when others.
      clear   formular_open-flag.
      perform err_formular_collect using foname.
  endcase.
endform.

*---------------------------------------------------------------------*
*       FORM START_FORMULARPRINT                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form start_formularprint.

  check formular_open-flag <> space.
  move sy-uname to fdis50_tab-usrid.

  clear t001.
  select single * from  t001
         where bukrs = bukrs.
  check sy-subrc = 0.

  move:    ibank1-banks   to  fdis50_tab-banks,
           ibank1-bankl   to  fdis50_tab-bankl,
           ibank1-ernam   to  fdis50_tab-ernam ,
           ibank1-bankn   to  fdis50_tab-bankn,
           ibank1-ernam   to  fdis50_tab-ernam ,
           ibank1-banka   to  fdis50_tab-banka,
           ibank1-provz   to  fdis50_tab-provz,
           ibank1-stras   to  fdis50_tab-stras,
           ibank1-ort01   to  fdis50_tab-ort01 ,
           ibank1-banko   to  fdis50_tab-bnkko.

* fixit
*  move:    bnka-dispw    to  fdis50_tab-waers .

  call function 'START_FORM'
       EXPORTING
            language = t001-spras
       IMPORTING
            language = t001-spras.
*      exceptions
*           form     = 1
*           unended  = 2.


  call function 'WRITE_FORM'
       EXPORTING
            window  = 'ADDRESS'  "Adresse
            element = '0020'.

  call function 'WRITE_FORM'
       EXPORTING
            window  = 'HEADER'
            element = '0010'. "Formularkopf

  call function 'WRITE_FORM'
       EXPORTING
            window  = 'FOOTER'  "Formularfuss
            element = '0050'.

  call function 'WRITE_FORM'
       EXPORTING
            element = '1000'. "Anschreiben

  call function 'WRITE_FORM'
       EXPORTING
            element = '5010'. "Ueberschrift

  call function 'WRITE_FORM'
       EXPORTING
            window   = 'MAIN'
            element  = '5010'
            function = 'SET'
            type     = 'TOP'.

  o_print_flag = 'X'.
endform.
*---------------------------------------------------------------------*
*  get payee bank info
*---------------------------------------------------------------------*
form get_payee_bank.
  ibank2-bankl = REG-ZBNKL.
  ibank2-bankn = REG-ZBNKN.
  ibank2-datum = reg-valut.
  ibank2-wrshb = - reg-rbetr.  " positive amount
  ibank2-ernam = reg-znme1.

  select single BANKA into ibank2-banka
          from bnka
          where banks = reg-zland
            and bankl = reg-zbnkl.
*  ibank2-banka = REG-ZNME1.

endform.
*---------------------------------------------------------------------*
*  payee
*---------------------------------------------------------------------*
form single_item_print.
  check formular_open-flag <> space.

  move:   ibank2-banks   to  fdis50_tab-g_banks,
          ibank2-bankl   to  fdis50_tab-g_bankl,
          ibank2-bankn   to  fdis50_tab-g_bnkko,
          ibank2-ernam   to  fdis50_tab-g_ernam,
          ibank2-banka   to  fdis50_tab-g_banka,
          ibank2-provz   to  fdis50_tab-g_provz,
          ibank2-stras   to  fdis50_tab-g_stras,
          ibank2-ort01   to  fdis50_tab-g_ort01.

  move:  ibank2-datum to fdis50_tab-datum,
         ibank2-wrshb to fdis50_tab-wrshb,
         ibank2-bankn to fdis50_tab-g_bankn.

  call function 'WRITE_FORM'
       EXPORTING
            element = '5100'.
endform.

*---------------------------------------------------------------------*
*       FORM END_FORMULARPRINT                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form   end_formularprint.
  check formular_open-flag <> space.
  move sum-wrshb to fdis50_tab-wrshb.
  move sum-dmshb to fdis50_tab-dmshb.

  call function 'WRITE_FORM'
       EXPORTING
            element = '5500'.

  call function 'END_FORM'.
endform.


*---------------------------------------------------------------------*
*       FORM CLOSE_FORMULARPRINT                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form close_formularprint.
  check formular_open-flag <> space.
  call function 'CLOSE_FORM'.

  clear formular_open-flag .
endform.

*---------------------------------------------------------------------*
form  err_formular_collect using name.
  err_fo_tab-name = name.
  collect err_fo_tab.
endform.
*--------------------------
FORM WRITE_BANK_DETAIL_RTN.
*--------------------------
*  SKIP 1.
  WRITE: /(10) REG-ZBNKL, REG-ZBNKN, (20) REG-KOINH, REG-VALUT,
           REG-RBETR.
  WRITE: / REG-ZNME1, REG-ZTELF.
  ULINE /1(80).

  T_AMOUNT = T_AMOUNT + REG-RBETR.

ENDFORM.

*-------------------------
FORM INTERNAL_APPROVE_RTN.
*-------------------------
  NEW-PAGE LINE-SIZE 130.

  SELECT * FROM REGUH INTO CORRESPONDING FIELDS OF TABLE REG2
    WHERE LAUFD = ZW_LAUFD
      AND LAUFI = ZW_LAUFI
      AND XVORL = ZW_XVORL
*      AND VBLNR <> ''.
    ORDER BY LIFNR UBNKL VBLNR ZBNKL.

  CHECK SY-SUBRC = 0.

  PERFORM CALL_FUNCTION_SET_PRINT.

  FLAG = '*'.

  DATA: C_WRBTR_TOTAL LIKE REGUP-DMBTR,
        C_ABZFW_TOTAL LIKE REGUP-DMBTR,
        C_NETTO_TOTAL LIKE REGUP-DMBTR,
        V_WRBTR_TOTAL LIKE REGUP-DMBTR,
        V_ABZFW_TOTAL LIKE REGUP-DMBTR,
        V_NETTO_TOTAL LIKE REGUP-DMBTR.

  LOOP AT REG2.

*    IF FLAG = '*'.
*      ZLIFNR = REG2-LIFNR.
*      ZUBNKL = REG2-UBNKL.
*      ZVBLNR = REG2-VBLNR.
*      ZZBNKL = REG2-ZBNKL.
*      FLAG = ''.
*    ENDIF.

*    IF REG2-LIFNR <> ZLIFNR.    "VENDER
*      WRITE: /61 'Vender total:', (15) V_WRBTR_TOTAL,
*                                  (15) V_ABZFW_TOTAL,
*                                  (15) V_NETTO_TOTAL.
*      V_WRBTR_TOTAL = 0.
*      V_ABZFW_TOTAL = 0.
*      V_NETTO_TOTAL = 0.
*    ENDIF.

    SELECT SINGLE * FROM BNKA  WHERE BANKS = REG2-UBNKS
                                 AND BANKL = REG2-UBNKL.

    SELECT SINGLE * FROM T042Z  WHERE LAND1 = REG2-UBNKS
                                  AND ZLSCH = REG2-RZAWE.

    SKIP 1.
    WRITE: / REG2-VBLNR, REG2-RZAWE, (10) T042Z-TEXT1,
             REG2-UBNKL,BNKA-BANKA.

    PERFORM WRITE_INTER_DETAIL_RTN.

    ZLIFNR = REG2-LIFNR.
    ZUBNKL = REG2-UBNKL.
    ZVBLNR = REG2-VBLNR.
    ZZBNKL = REG2-ZBNKL.

    AT END OF LIFNR.
*      WRITE: /57 'Total:', T_AMOUNT.
      ULINE /1(132).
    ENDAT.

  ENDLOOP.

ENDFORM.

*--------------------------
FORM WRITE_INTER_HEADER_RTN.
*--------------------------

  SELECT SINGLE * FROM BNKA
                 WHERE BANKS = REG2-UBNKS
                   AND BANKL = REG2-UBNKL.

  ULINE /1(132).
  WRITE: /(10) 'PaymentDoc', (12) 'Pay method', 'Our account'.
  WRITE: /10(10) 'Doc.no', 'DT', (10) 'DocDate', (10) 'BaseDte',
              'Cond', 'Pmnt term ', 'PK', 'P', 'S', 'Curr',
            (15) 'Gross curr'   RIGHT-JUSTIFIED,
            (15) 'Deduct curr'  RIGHT-JUSTIFIED,
            (15) 'Net currency' RIGHT-JUSTIFIED.
  ULINE /1(132).

  WRITE: / 'Vender:', REG2-LIFNR, REG2-NAME1.

ENDFORM.

*---------------------------
FORM WRITE_INTER_DETAIL_RTN.
*---------------------------
  ULINE /10(122).

  SELECT * FROM REGUP
    WHERE LAUFD = REG2-LAUFD
      AND LAUFI = REG2-LAUFI
      AND XVORL = REG2-XVORL
      AND ZBUKR = REG2-ZBUKR
      AND LIFNR = REG2-LIFNR
      AND KUNNR = REG2-KUNNR
      AND EMPFG = REG2-EMPFG
      AND VBLNR = REG2-VBLNR.

    CHECK SY-SUBRC = 0.

    WRITE REGUP-ZBDXT TO ZAHLBED-TAGE NO-SIGN.
    WRITE REGUP-ZBDXP TO ZAHLBED-PROZ NO-SIGN.

    IF REGUP-SHKZG EQ 'H'.
      WREGUP-DMBTR = REGUP-DMBTR * -1.
      WREGUP-WRBTR = REGUP-WRBTR * -1.
      WREGUP-WSKTO = REGUP-WSKTO * -1.
      WREGUP-QBSHB = REGUP-QBSHB * -1.
    ELSE.
      WREGUP-DMBTR = REGUP-DMBTR.
      WREGUP-WRBTR = REGUP-WRBTR.
      WREGUP-WSKTO = REGUP-WSKTO.
      WREGUP-QBSHB = REGUP-QBSHB.
    ENDIF.

    ABZFW = WREGUP-WSKTO + WREGUP-QBSHB.
    NETTO = WREGUP-WRBTR - ABZFW.

    WRITE: /10 REGUP-BELNR,   "Accounting document number
             REGUP-BLART,   "Document type
             REGUP-BLDAT,   "Document date in document
             REGUP-ZFBDT,   "Baseline date for due date calculation
             REGUP-ZTERM,
             ZAHLBED,
             REGUP-BSCHL,
             REGUP-ZLSCH,   "Payment method
             REGUP-ZLSPR,   "Payment Block Key
             REGUP-WAERS,
             (15) WREGUP-WRBTR CURRENCY REGUP-WAERS,
             (15) ABZFW CURRENCY REGUP-WAERS NO-ZERO,
             (15) NETTO CURRENCY REGUP-WAERS NO-GAP.
* Andy
    if not regup-sgtxt is initial.
      WRITE:  /24 'Remark:', REGUP-SGTXT.
    endif.

    C_WRBTR_TOTAL = C_WRBTR_TOTAL + WREGUP-WRBTR.
    C_ABZFW_TOTAL = C_ABZFW_TOTAL + ABZFW.
    C_NETTO_TOTAL = C_NETTO_TOTAL + NETTO.
    V_WRBTR_TOTAL = V_WRBTR_TOTAL + WREGUP-WRBTR.
    V_ABZFW_TOTAL = V_ABZFW_TOTAL + ABZFW.
    V_NETTO_TOTAL = V_NETTO_TOTAL + NETTO.

  ENDSELECT.

  ULINE /10(122).

  WRITE: /68 'Total:', (15) C_WRBTR_TOTAL, (15) C_ABZFW_TOTAL,
          (15) C_NETTO_TOTAL.

  C_WRBTR_TOTAL = 0.
  C_ABZFW_TOTAL = 0.
  C_NETTO_TOTAL = 0.

ENDFORM.

*----------------------------
FORM CALL_FUNCTION_SET_PRINT.
*----------------------------

  CALL FUNCTION 'SET_PRINT_PARAMETERS'
       EXPORTING
*            archive_mode   = '3'
*            copies         = '5'
*            department     = 'BASIS'
*            destination    = 'LT50'
*            expiration     = '0'
            immediately    = 'X'
            layout         = 'X_65_80'
*            line_count     = 54
*            line_size      = 20
*            list_name      = 'Test'
*            list_text      = 'Test for User''s Guide'
*            new_list_id    = 'X'
*            receiver       = 'KELLERH'
*            release        = ' '
            sap_cover_page = ' '.

ENDFORM.
