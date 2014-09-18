REPORT zrfig01           MESSAGE-ID zmfi
                         NO STANDARD PAGE HEADING
                         LINE-SIZE   130   LINE-COUNT   90.
*&------------------------------------------------------------------
*& Author                 : hw.zhang
*& Creation Date          : 07/10/2003
*& Specification By       :
*& Development Request No : UD1K901600
*& Addl documentation     :
*& Description  : print FI document
*&
*& Modification Log
*& Date     Developer      Request ID      Description
*& 01/04/06 Manjunath      UD1K918802      Bug fix / Footer changes
*& 03/08/06 Manjunath      UD1K919672      Check for duplicate Invoice
*&                                         and print the same in footer
*& 03/13/06  Manjunath     UD1K919672      Print Duplicate invoice in
*&                                         footer
*& 06/14/06  Manju         UD1K921056      Program changes to Tax code &
*&                                         tax Rate.
*& 07/26/06  Manju         UD1K921499      Tax Code program changes
*& 07/28/06  Manju         UD1K921549      Tax code program logic change
*& 07/31/06  Manju         UD1K921579      Tax code pgm logic change to
*                                          check config.
*& 08/14/06  Manju         UD1K921747      Bug fixing for Duplicate
*&                                         invoice warning
*& 08/17/06  Manju         UD1K921782      Program corrections not to
*&                                         print withholding is 'XX'.
*& 08/22/06  Manju         UD1K921831      Duplicate Invoice bug fix.
*& 08/23/06  Manju         UD1K921847      Duplicate Invoice bug fix
*&                                         to avoid short Dump
*& 09/22/06  Manju         UD1K922256      Print Reverse Document
*& 09/26/06  Manju         UD1K922288      Print cost center for Expense
*                                          G/L Accounts.
*& 10/12/06  Manju         UD1K922521      Print the accounts in the
*&                                         order it is selected.

*& 05/22/08  ig.moon       UD1K943687      Add field for Pur.Amt & Info
*&--------------------------------------------------------------------
TYPE-POOLS: slis.
INCLUDE <icon>.
INCLUDE <symbol>.
CLASS cl_gui_resources DEFINITION LOAD.

************************************************************************
************************* Global data **********************************
************************************************************************
TABLES: t001,    "Company code
        t003t,   "Document Type Texts
        t005,    "Countris
        t007s,   "Tax Code Names
        bkpf,    "Accounting Document Header
        vbkpf,   "Document Header for Document Parking
        bseg,    "Accounting Document Segment
        vbsegs,  "Document Segment for G/L Accounts Document Parking
        vbsegk,  "Document Segment for Vendor Document Parking
        vbsegd,  "Document Segment for Customer Document Parking
        vbsega,  "Document Segment for Assets Document Parking
        vbset,   "Document Segment for Taxes Document Parking
        bsec,
        usr21,   "Assign User Name Address Key
        adcp,    "Person/ Address Assignment
        anla,    "Asset Master Record Segment
        kna1,    "General Data in Customer Master
        lfa1,    "Vendor Master (General Section)
        tgsbt,   "Business Area Names
        skat,    "G/L Account Master Record
        aufk,    "Order Master Data
        csks,    "Cost Center Master Data
        cskt,    "Cost Center Texts
        fmfctrt, "Funds Center Texts
        rbkp,     "Document Header: Invoice Receipt
        lfbw,     " Vendor / Company
        t059fb,
        t059z,
        with_item,
        t059p, bsik, bds_bar_in.
* BKPF : G/L Header
DATA: BEGIN OF it_bk OCCURS 100,
      blart    LIKE  bkpf-blart,       "Doc. Type.
      budat    LIKE  bkpf-budat,       "Posting Date
      bldat    LIKE  bkpf-bldat,       "doc. date
      cpudt    LIKE  bkpf-cpudt,       "Enrry data
      usnam    LIKE  bkpf-usnam,       "User Name
      ppnam    LIKE  bkpf-ppnam,       "Parked by
      waers    LIKE  bkpf-waers,       "Currency key
      tcode    LIKE  bkpf-tcode,       "Transaction Code
      xblnr    LIKE  bkpf-xblnr,       "Reference
      bstat    LIKE  bkpf-bstat,       "Doc. Status
      depart   LIKE  adcp-department,  "Dept Name
      xprfg    LIKE  vbkpf-xprfg,      "Doc. Complete
      stblg    LIKE  bkpf-stblg,       "Reverse doc no
      stjah    LIKE  bkpf-stjah,       "Reverse year
      stgrd    LIKE  bkpf-stgrd,       "Reverse reason
      bukrs    LIKE  bkpf-bukrs,       "Company Code
      gjahr    LIKE  bkpf-gjahr,       "Fiscal Year
      belnr    LIKE  bkpf-belnr,       "Doc. No.
      awkey    LIKE bkpf-awkey,        "Object key
      awtyp    LIKE bkpf-awtyp,        "Reference procedure UD1K922256
      revtx(7),                        "Reverse UD1K922256
** Furong on 05/01/14
    bktxt like bkpf-bktxt,
** End
END OF it_bk.

DATA : BEGIN OF it_vbkpf OCCURS 0,
         belnr  LIKE vbkpf-belnr,
         xprfg  LIKE vbkpf-xprfg,
       END OF it_vbkpf.
*------2003/12/04 modify from jhs
DATA : BEGIN OF it_t074u OCCURS 0,
          umskz  LIKE t074u-umskz, "Special G/L Indicator
       END OF it_t074u.

DATA : BEGIN OF it_lfc3 OCCURS 0.
        INCLUDE STRUCTURE lfc3.
DATA : END OF it_lfc3.

DATA : BEGIN OF it_fdm1 OCCURS 0.
        INCLUDE STRUCTURE fdm1.
DATA : END OF it_fdm1.
* it_out for alv
DATA: BEGIN OF it_bkpf OCCURS 0.
        INCLUDE STRUCTURE it_bk.
DATA:   chkbox TYPE c,
        tabcolor     TYPE slis_t_specialcol_alv,
      END OF it_bkpf.

* BSEG : G/L Data segment
*DATA: it_bseg TYPE TABLE OF bseg WITH HEADER LINE.
DATA : BEGIN OF it_bseg OCCURS 0.
        INCLUDE STRUCTURE bseg.
DATA : END OF it_bseg.
* COEP : Inteanal order
DATA: it_coep TYPE TABLE OF coep WITH HEADER LINE.

* BSEG: additional info
DATA: BEGIN OF it_l,
        txt20(20),      " account short text
        acct_type(48),  " account detail info.
        taxcd(7),       "
*        rate(4),
         rate LIKE t059z-qproz,
        ordno(30),
        assgn1(17),
        assgn2(12),
        text(48),
        date(12),
        dr  LIKE bseg-dmbtr,
        cr  LIKE bseg-dmbtr,
        drt  LIKE bseg-dmbtr, "tax
        crt  LIKE bseg-dmbtr, "tax
        name1 LIKE lfa1-name1,
        code(20),
        aufnr LIKE aufk-aufnr,
    END OF it_l.
* summary amount of fi doc.
DATA: BEGIN OF it_s,
        sdr  LIKE bseg-dmbtr,
        scr  LIKE bseg-dmbtr,
        sdrt LIKE bseg-dmbtr,
        scrt LIKE bseg-dmbtr,
      END OF it_s.

DATA: wa_width TYPE i VALUE 130, "Paper width
      wa_ul  LIKE sy-uline VALUE '-',
      wa_vl  LIKE sy-vline VALUE '|',
      wa_sp  LIKE space    VALUE ' ',
      wa_total_page_number TYPE i,
      wa_final(16).

* Begin of changes - UD1K918802
DATA : wa_sakto LIKE ekkn-sakto,
       wa_kostl LIKE ekkn-kostl,
       wa_aufnr LIKE ekkn-aufnr,
       wa_buzei LIKE rseg-buzei,
       l_pg TYPE p DECIMALS 2.
* End of changes - UD1K918802

* Begin of changes - UD1K919672
DATA : BEGIN OF it_dupinv OCCURS 0,
       bukrs LIKE bkpf-bukrs,
       belnr LIKE bkpf-belnr,
       gjahr LIKE bkpf-gjahr,
       END OF it_dupinv.

DATA : BEGIN OF it_dupinvh OCCURS 0,
         belnr LIKE bseg-belnr,
         dmbtr LIKE bseg-dmbtr,
         bschl LIKE bseg-bschl,
         koart LIKE bseg-koart,
       END OF it_dupinvh.

DATA : it_dupinvh1 LIKE TABLE OF it_dupinvh WITH HEADER LINE.
DATA : l_xblnr LIKE bkpf-xblnr,
       with_flag TYPE c,                                    "UD1K921499
       l_wrbtr LIKE bseg-wrbtr,                             "UD1K921499
       ll_wrbtr LIKE bseg-wrbtr,
       l_text(45) TYPE c.                                   "UD1K921499

* End of changes - UD1K919672

* for company code name...
DATA: wa_l_name1(40), wa_l_name2(40), wa_l_company_name(80).

DATA : wa_t_cnt TYPE i,
       wa_cnt   TYPE i,
       wa_final_cnt   TYPE i,
       wa_f_cnt   TYPE i,
       wa_dp_chk,
       wa_final_txt(20),
       wa_dp   LIKE lfc3-saldv,
       wa_name1 LIKE lfa1-name1.
*Issue Number : FI-20040729-001 Requested by GHLEE ,2004/08/25
*Changed by WSKIM
*---Start
DATA : BEGIN OF it_cb OCCURS 0,
        kokrs LIKE cobk-kokrs,
        belnr LIKE cobk-belnr,
        gjahr LIKE cobk-gjahr,
        budat LIKE cobk-budat,
        usnam LIKE cobk-usnam,
        bldat LIKE cobk-bldat,
        cpudt LIKE cobk-cpudt,
        blart LIKE cobk-blart,
        kwaer LIKE cobk-kwaer,
        bltxt LIKE cobk-bltxt,
       END OF it_cb.
*---End

* 5/22/2008 {
DATA: BEGIN OF it_ekbe OCCURS 0,
            ebeln TYPE ebeln,
            ebelp TYPE ebelp,
            belnr TYPE mblnr,
            shkzg TYPE shkzg,
            dmbtr TYPE dmbtr,
      END   OF it_ekbe.

DATA: BEGIN OF it_ebeln OCCURS 0,
            ebeln TYPE ebeln,
      END   OF it_ebeln.

DATA t_it_ekbe LIKE it_ekbe OCCURS 0 WITH HEADER LINE.

* +ig.moon 4/6/2009 {
* Printing option for Label
DATA: zoptions LIKE	itcpo OCCURS 0 WITH HEADER LINE.
DATA: zprinter(4) VALUE 'RFL'.

DATA : $awkey TYPE awkey,
       $barcode(20).

* }
****************************** constants *******************************
CONSTANTS:  false VALUE ' ',
            true  VALUE 'X'.

DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.
* }

*----------------------------------------------------------------------
*
* SELECTION-SCREEN
*
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE text-010.
SELECT-OPTIONS:
  s_bukrs  FOR   bkpf-bukrs DEFAULT 'H201' MEMORY ID buk,
  s_belnr  FOR   bkpf-belnr,
  s_rbelnr FOR   rbkp-belnr,
  s_gjahr  FOR   bkpf-gjahr MEMORY ID gjr.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(10) text-012 FOR FIELD r_a.
PARAMETER  r_a RADIOBUTTON GROUP r1." normal
SELECTION-SCREEN COMMENT 38(10) text-013 FOR FIELD r_b.
PARAMETER  r_b RADIOBUTTON GROUP r1." Reposting
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b0.

*SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-010.
SELECT-OPTIONS:
  s_blart  FOR   bkpf-blart MEMORY ID bar,
  s_budat  FOR   bkpf-budat,
  s_xblnr  FOR   bkpf-xblnr,
  s_cpudt  FOR   bkpf-cpudt.

SELECTION-SCREEN END OF BLOCK b1.


SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-020.
PARAMETER   p_own TYPE c AS CHECKBOX DEFAULT 'X'.
*SELECTION-SCREEN END OF BLOCK b2.
*
*SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-030.
PARAMETER : p_parked AS CHECKBOX,
            p_posted AS CHECKBOX.
PARAMETER : p_limit  TYPE i  DEFAULT '250'.
SELECTION-SCREEN END OF BLOCK b2.

*-------------------------------------------------------*
* Include for ALV
*-------------------------------------------------------*
INCLUDE: zrfig01i_alv,
         zrfig01i_write.

*-------------------------------------------------------*
AT SELECTION-SCREEN.
*-------------------------------------------------------*
  IF s_bukrs-low IS INITIAL.
    MESSAGE e000(zmfi) WITH text-014.
  ENDIF.

  IF s_gjahr-low IS INITIAL.
    MESSAGE e000(zmfi) WITH text-015.
  ENDIF.
  IF s_cpudt-low IS INITIAL.
    MESSAGE e000(zmfi) WITH text-016.
  ENDIF.
  IF p_parked IS INITIAL AND p_posted IS INITIAL.
    MESSAGE e000(zmfi) WITH text-017.
  ENDIF.

*-------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
*-------------------------------------------------------*
** Authority check
  AUTHORITY-CHECK OBJECT 'Z_BKPF_BES' ID 'BRGRU'
                  FIELD '*'.
  IF sy-subrc <> 0.
    AUTHORITY-CHECK OBJECT 'Z_BKPF_BES' ID 'BRGRU'
                    FIELD 'FI'.
    IF sy-subrc <> 0.
      p_own = 'X'.
      LOOP AT SCREEN.
        IF screen-name = 'P_OWN'.
          screen-input =  0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

*-------------------------------------------------------*
INITIALIZATION.
*-------------------------------------------------------*
  wa_vl = sy-vline.
  wa_ul = sy-uline.
  p_parked = 'X'.
  p_posted = 'X'.

  s_gjahr-low = sy-datum(4).
  APPEND s_gjahr.

  s_cpudt-low  = sy-datum.
  s_cpudt-high = sy-datum.
  APPEND s_cpudt.

  wa_repid = sy-repid.
* ==> Change Variant saving type
*                         U-???, X-??(??), A-??, ' '-????
  wa_var_save = 'A'.
* ==> Change first mode   GRID or LIST
  wa_alv_function_name = 'REUSE_ALV_LIST_DISPLAY'.
  REFRESH : gt_fieldcat.
  CLEAR   : gs_layout.


*-------------------------------------------------------*
START-OF-SELECTION.
*-------------------------------------------------------*
*-------------------------------------------------------*
END-OF-SELECTION.
*-------------------------------------------------------*
  IF NOT r_a IS INITIAL.
    IF s_rbelnr-low IS INITIAL.
      PERFORM read_bkpf.
    ELSE.
      PERFORM read_rbkp.
    ENDIF.
  ELSEIF NOT r_b IS INITIAL.
    PERFORM read_cobk.
  ENDIF.

  DESCRIBE TABLE it_bkpf LINES sy-index.
  IF sy-index = 0.
    MESSAGE s000(zmfi) WITH text-018.
  ELSEIF sy-index  > p_limit.
    MESSAGE s000(zmfi) WITH 'Too many documents selected'.
    EXIT.
  ENDIF.

  PERFORM make_alv_bkpf.


  IF sy-xcode EQ 'PRI_BAR'.

    PERFORM init_print.
    PERFORM printing.

  ELSE.

*Line items

    IF NOT r_a IS INITIAL.
      PERFORM write_documents.
    ELSEIF NOT r_b IS INITIAL.
      PERFORM write_internal_documents.
    ENDIF.

  ENDIF.

AT USER-COMMAND.
  IF sy-ucomm EQ 'PRI_BAR'.
    PERFORM init_print.
    PERFORM pri_bar_1.
  ENDIF.

*&------------------------------------------------------*
*&      Form  READ BKPF
*&------------------------------------------------------*
FORM read_bkpf.
  RANGES : r_bstat FOR bkpf-bstat,
           r_usnam FOR bkpf-usnam,
           r_ppnam FOR bkpf-ppnam.

  REFRESH it_bkpf. CLEAR it_bkpf.
  REFRESH it_bseg. CLEAR it_bseg.

* check and read parked data
  IF p_parked = 'X'.
    r_bstat-sign   = 'I'.
    r_bstat-option = 'EQ'.
    r_bstat-low    = 'V'.
    APPEND r_bstat.
  ENDIF.
* check and read posted data
  IF p_posted = 'X'.
    r_bstat-sign   = 'I'.
    r_bstat-option = 'EQ'.
    r_bstat-low    = ' '.
    APPEND r_bstat.
    r_bstat-low    = 'A'.
    APPEND r_bstat.
    r_bstat-low    = 'B'.
    APPEND r_bstat.
    r_bstat-low    = 'D'.
    APPEND r_bstat.
    r_bstat-low    = 'S'.
    APPEND r_bstat.
  ENDIF.

  IF p_own EQ 'X'.
    r_usnam-sign = 'I'.
    r_usnam-option = 'EQ'.
    r_usnam-low = sy-uname.
    APPEND r_usnam.
    r_ppnam-sign = 'I'.
    r_ppnam-option = 'EQ'.
    r_ppnam-low = sy-uname.
    APPEND r_ppnam.
  ENDIF.
  SELECT bukrs belnr gjahr bstat
         budat usnam ppnam tcode xblnr
         bldat cpudt blart bktxt waers
         stblg stjah stgrd awkey awtyp
    FROM bkpf
    INTO CORRESPONDING FIELDS OF TABLE it_bk
   WHERE bukrs IN s_bukrs
     AND gjahr IN s_gjahr
     AND belnr IN s_belnr
     AND blart IN s_blart
     AND budat IN s_budat
     AND xblnr IN s_xblnr
     AND cpudt IN s_cpudt
     AND bstat IN r_bstat
     AND ( usnam IN r_usnam OR ppnam IN r_ppnam ).

*====JHS MODIFY 2004.01.26
  CLEAR : wa_t_cnt.
  DESCRIBE TABLE it_bk LINES wa_t_cnt.
  IF wa_t_cnt > 0.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_vbkpf
    FROM vbkpf
    FOR ALL ENTRIES IN it_bk
    WHERE belnr = it_bk-belnr.
  ENDIF.

  LOOP AT it_bk.
*----2004/01/26.   jhs modify
    IF it_bk-bstat = 'V'.
      READ TABLE it_vbkpf WITH KEY belnr = it_bk-belnr
                                   xprfg = 'X'.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
    ENDIF.
* Begin of changes - UD1K922256
    IF it_bk-awtyp  EQ 'RMRP' .
      SELECT SINGLE *
          FROM rbkp
         WHERE stblg = it_bk-awkey(10)
           AND stjah = it_bk-awkey+10(4).
      IF sy-subrc EQ 0.
        it_bk-revtx = 'REV'.
      ENDIF.
    ENDIF.
* End of changes - UD1K922256
    MOVE-CORRESPONDING it_bk TO it_bkpf.
    APPEND it_bkpf. CLEAR it_bkpf.
  ENDLOOP.

ENDFORM.
*&-----------------------------------------------------*
*&      Form  READ_BSEG
*&-----------------------------------------------------*
FORM read_bseg.

  DATA: wa_l_count TYPE i VALUE 0.

  CASE  it_bkpf-bstat.
    WHEN 'V'.
      REFRESH it_bseg. CLEAR it_bseg.

* ig.moon 12/15/2008 {
      PERFORM  bseg_read_process.
* }

      PERFORM  vbsegs_read_process.  "G/L Account Document Parking
      PERFORM  vbsegk_read_process.  "Vendor Document Parking
      PERFORM  vbsegd_read_process.  "Customer Document Parking
      PERFORM  vbsega_read_process.  "Asset Document Parking
      PERFORM  vbset_read_process.   "Taxes Document Parking
    WHEN OTHERS.
      PERFORM  bseg_read_process.
  ENDCASE.
  SORT it_bseg BY buzei.


  __cls : it_ekbe, t_it_ekbe,it_ebeln.

  IF NOT it_bseg[] IS INITIAL.

    LOOP AT it_bseg.
      it_ebeln-ebeln = it_bseg-ebeln.
      CHECK it_ebeln-ebeln NE space.
      COLLECT it_ebeln.
    ENDLOOP.

    LOOP AT it_ebeln.
      SELECT ebeln ebelp belnr shkzg
      SUM( dmbtr ) APPENDING TABLE it_ekbe
      FROM ekbe
      WHERE ebeln EQ it_ebeln-ebeln
      AND vgabe EQ '4'
      GROUP by ebeln ebelp belnr shkzg.
    ENDLOOP.

  ENDIF.

  LOOP AT it_ekbe.
    t_it_ekbe = it_ekbe.
    IF t_it_ekbe-shkzg EQ 'H'.
      t_it_ekbe-dmbtr = t_it_ekbe-dmbtr * -1.
    ENDIF.
    CLEAR : t_it_ekbe-ebelp, t_it_ekbe-shkzg.
    COLLECT t_it_ekbe.
  ENDLOOP.

  LOOP AT it_bseg.
    IF it_bseg-hkont IS INITIAL.
      it_bseg-hkont = it_bseg-saknr.
    ENDIF.
**--> Noted items : change hkont
    IF it_bkpf-bstat = 'S'.
      SELECT SINGLE skont INTO it_bseg-hkont FROM t074
        WHERE ktopl = 'HNA1' AND
              koart = it_bseg-koart AND
              umskz = it_bseg-zumsk AND
              hkont = it_bseg-saknr.

    ENDIF.

    MODIFY it_bseg TRANSPORTING hkont.
    ADD 1 TO wa_l_count.

  ENDLOOP.

* Begin of changes - UD1K918802
*  wa_total_page_number = wa_l_count  / 16 + 1.
  l_pg = wa_l_count  / 16 .
  IF l_pg < 1.
    wa_total_page_number = 1.
  ELSE.
    wa_total_page_number = l_pg + 1.
  ENDIF.

* End of changes - UD1K918802

ENDFORM.

*&------------------------------------------------------*
*&      Form  BSEG_READ_PROCESS
*&------------------------------------------------------*
FORM bseg_read_process.

  DATA $lifnr TYPE lifnr.
  DATA : fr_date TYPE d,
         to_date TYPE d.

  REFRESH: it_bseg,it_dupinv,it_dupinvh.
  CLEAR : it_bseg,it_dupinv,it_dupinvh.

  SELECT *
    FROM bseg
    INTO CORRESPONDING FIELDS OF TABLE it_bseg
   WHERE bukrs = it_bkpf-bukrs
     AND gjahr = it_bkpf-gjahr
     AND belnr = it_bkpf-belnr.

* Begin of changes UD1K919672 / UD1K921747
  IF it_bkpf-blart = 'RE'
  OR it_bkpf-blart = 'RI'
  OR it_bkpf-blart = 'KR'
  OR it_bkpf-blart = 'KA'
  OR it_bkpf-blart = 'KG'
  OR it_bkpf-blart = 'KI'
  OR it_bkpf-blart = 'KZ'
  OR it_bkpf-blart = 'ZP'.

* if Document is reversed don't check for Duplicate Invoice
    IF  it_bkpf-revtx IS INITIAL  OR                        "UD1K922256
        it_bkpf-stblg IS INITIAL.                           "UD1K922256
      IF NOT it_bkpf-xblnr IS INITIAL.                      "UD1K921847
*

        IF it_bkpf-bstat EQ space.
          SELECT SINGLE lifnr INTO $lifnr
                         FROM bsik WHERE
                            bukrs EQ it_bkpf-bukrs
                            AND gjahr EQ it_bkpf-gjahr
                            AND belnr EQ it_bkpf-belnr.
        ELSE.
          SELECT SINGLE lifnr INTO $lifnr
                        FROM vbsegk WHERE
                           ausbk EQ it_bkpf-bukrs
                           AND belnr EQ it_bkpf-belnr
                           AND gjahr EQ it_bkpf-gjahr.
        ENDIF.
        CHECK sy-subrc EQ 0.
*
        CONCATENATE it_bkpf-bldat(4) : '0101' INTO fr_date,
                                       '1231' INTO to_date.
        SELECT bukrs belnr gjahr   FROM bkpf
                INTO TABLE it_dupinv
             WHERE bukrs  =  it_bkpf-bukrs
                AND xblnr =  it_bkpf-xblnr
                AND bldat BETWEEN fr_date AND to_date.


      ENDIF.

      DELETE it_dupinv WHERE belnr EQ it_bkpf-belnr.
      READ TABLE it_dupinv INDEX 1.
      IF sy-subrc NE 0.
        l_xblnr = it_bkpf-xblnr.
        REPLACE '_' WITH space INTO l_xblnr.
        REPLACE '*' WITH space INTO l_xblnr.
        CONDENSE l_xblnr NO-GAPS.
        IF NOT l_xblnr IS INITIAL.                          "UD1K921847
          SELECT bukrs belnr gjahr   FROM bkpf
                INTO TABLE it_dupinv
             WHERE bukrs  =  it_bkpf-bukrs
               AND xblnr =  l_xblnr
               AND gjahr =  it_bkpf-gjahr.
        ENDIF.
      ENDIF.
*UD1K921831
      DELETE it_dupinv WHERE belnr EQ it_bkpf-belnr.
      IF NOT it_dupinv[] IS INITIAL.

        SELECT belnr dmbtr bschl
             INTO TABLE it_dupinvh1  FROM bseg
             FOR ALL ENTRIES IN it_dupinv
            WHERE bukrs   = it_dupinv-bukrs
              AND belnr   = it_dupinv-belnr
              AND gjahr   = it_dupinv-gjahr
              AND koart   = 'K'
              AND lifnr   = $lifnr. " 12/15/2008

        SELECT belnr dmbtr bschl
             APPENDING TABLE it_dupinvh1  FROM vbsegk
             FOR ALL ENTRIES IN it_dupinv
            WHERE ausbk   = it_dupinv-bukrs
              AND belnr   = it_dupinv-belnr
              AND gjahr   = it_dupinv-gjahr
              AND lifnr   = $lifnr. " 12/15/2008

        LOOP AT it_dupinvh1.
          MOVE-CORRESPONDING it_dupinvh1 TO  it_dupinvh.
          it_dupinvh-bschl = ''.
          COLLECT it_dupinvh.
        ENDLOOP.

      ENDIF.
    ENDIF.   "it_bkpf-revtx
  ENDIF.
* End   of changes UD1K919672

ENDFORM.

*&------------------------------------------------------*
*&      Form  VBSEGS_READ_PROCESS
*&------------------------------------------------------*
FORM vbsegs_read_process.
  SELECT *
    FROM vbsegs
    INTO CORRESPONDING FIELDS OF TABLE it_bseg
   WHERE ausbk = it_bkpf-bukrs
     AND gjahr = it_bkpf-gjahr
     AND belnr = it_bkpf-belnr.
  LOOP AT it_bseg WHERE koart EQ ' '.
    it_bseg-koart = 'S'.
    MODIFY it_bseg TRANSPORTING koart.
  ENDLOOP.
ENDFORM.

*&------------------------------------------------------*
*&      Form  VBSEGK_READ_PROCESS
*&------------------------------------------------------*
FORM vbsegk_read_process.
  SELECT *
    FROM vbsegk
    APPENDING CORRESPONDING FIELDS OF TABLE it_bseg
   WHERE ausbk = it_bkpf-bukrs
     AND gjahr = it_bkpf-gjahr
     AND belnr = it_bkpf-belnr.
  LOOP AT it_bseg WHERE koart EQ ' '.
    it_bseg-koart = 'K'.
    MODIFY it_bseg TRANSPORTING koart.
  ENDLOOP.

ENDFORM.

*&------------------------------------------------------*
*&      Form  VBSEGD_READ_PROCESS
*&------------------------------------------------------*
FORM vbsegd_read_process.
  SELECT *
    FROM vbsegd
    APPENDING CORRESPONDING FIELDS OF TABLE it_bseg
   WHERE ausbk = it_bkpf-bukrs
     AND gjahr = it_bkpf-gjahr
     AND belnr = it_bkpf-belnr.
  LOOP AT it_bseg WHERE koart EQ ' '.
    it_bseg-koart = 'D'.
    MODIFY it_bseg TRANSPORTING koart.
  ENDLOOP.

ENDFORM.

*&------------------------------------------------------*
*&      Form  VBSEGA_READ_PROCESS
*&------------------------------------------------------*
FORM vbsega_read_process.
  SELECT *
    FROM vbsega
    APPENDING CORRESPONDING FIELDS OF TABLE it_bseg
   WHERE ausbk = it_bkpf-bukrs
     AND gjahr = it_bkpf-gjahr
     AND belnr = it_bkpf-belnr.
  LOOP AT it_bseg WHERE koart EQ ' '.
    it_bseg-koart = 'A'.
    MODIFY it_bseg TRANSPORTING koart.
  ENDLOOP.

ENDFORM.

*&------------------------------------------------------*
*&      Form  VBSET_READ_PROCESS
*&------------------------------------------------------*
FORM vbset_read_process.
*  CLEAR: it_bkpf-buzei, it_bkpf-buzei1.
*
*  LOOP AT iline1 WHERE belnr = it_bkpf-belnr.
*    it_bkpf-buzei  =  iline1-buzei.
*    IF it_bkpf-buzei  > it_bkpf-buzei1.
*      it_bkpf-buzei1 = it_bkpf-buzei.
*    ELSE.
*      it_bkpf-buzei1 = it_bkpf-buzei1.
*    ENDIF.
*  ENDLOOP.
*
*
*  REFRESH it_bseg.
*  SELECT * FROM vbset
*   WHERE ausbk = it_bkpf-bukrs
*     AND belnr = it_bkpf-belnr
*     AND gjahr = it_bkpf-gjahr.
*
*    CLEAR: it_bseg.
*    it_bkpf-buzei1 = it_bkpf-buzei1 +  1.
*
*    it_bseg-saknr = 'TAXACCOUNT'.
*    it_bseg-buzei = it_bkpf-buzei1.
*    it_bseg-mwskz = vbset-mwskz.
*    it_bseg-txjcd = vbset-txjcd.
*
*    CASE vbset-shkzg.
*      WHEN  'S'.     it_bseg-s_gumak = vbset-fwste.
*      WHEN  'H'.     it_bseg-h_gumak = vbset-fwste.
*    ENDCASE.
*
*    it_bseg-hkont = 'TAXACCOUNT'.
*    it_bseg-txt20 = 'Tax Account'.
*
** use tax payable
*    IF vbset-fwste = 0.
*      it_bseg-sgtxt = '***Tax Not Calculated***'.
*    ELSE.
*      it_bseg-h_gumak = vbset-fwste.
*    ENDIF.
*    PERFORM  copy_to_iline1.
*  ENDSELECT.
*
*  CLEAR  sy-subrc.
*
*  LOOP AT iline3.
*    IF sy-subrc <> 0.
*      EXIT.
*    ENDIF.
*    it_bkpf-buzei1 = it_bkpf-buzei1 +  1.
**    PERFORM  copy_to_iline11.
*  ENDLOOP.
*
*  REFRESH iline3.
*  CLEAR   iline3.
ENDFORM.


*&------------------------------------------------------*
*&      Form  get_account_info
*&------------------------------------------------------*
*FORM get_account_info.
*  data: l_name1 type name1.
*  CLEAR  l_name1.
*  CASE it_bseg-koart.     "Account Type
*    WHEN 'A'.
*      SELECT SINGLE txt50
*        FROM anla       "Asset Master Record Segment
*        INTO l_name1
*       WHERE bukrs = it_bkpf-bukrs
*         AND anln1 = it_bseg-anln1  "Asset
*         AND anln2 = it_bseg-anln2. "Sub-Number
*    WHEN 'D'.
*      SELECT SINGLE name1
*        FROM kna1       "General Data in Customer Master
*        INTO l_name1
*       WHERE kunnr = it_bseg-kunnr.
*    WHEN 'K'.
*      SELECT SINGLE name1
*        FROM lfa1       "Vendor Master (General Section)
*        INTO l_name1
*       WHERE lifnr = it_bseg-lifnr.
*  ENDCASE.
*  IF it_bseg-hkont+4(1) = '6'.
*    SELECT SINGLE bezeich
*      FROM fmfctrt                 "Funds Center Text
*      INTO l_name1
*     WHERE fictr = it_bseg-fistl.    "Funds Center
*    it_bseg-accnt = it_bseg-fistl.
*  ENDIF.
*ENDFORM.
*
**&------------------------------------------------------*
**&      Form  CSKS_READ_PROCESS
**&------------------------------------------------------*
*FORM csks_read_process.
*  IF NOT it_bseg-aufnr IS INITIAL.
*    SELECT SINGLE ktext
*      FROM aufk                      "Order Master Data
*      INTO it_bseg-ktext
*     WHERE aufnr = it_bseg-aufnr.      "Order
*    it_bseg-kostl = it_bseg-aufnr+2(10). "Cost Center
*  ELSE.
*    SELECT SINGLE ktext
*      FROM cskt                      "Cost Center Text
*      INTO it_bseg-ktext
*     WHERE kostl = it_bseg-kostl.      "Cost Center
*  ENDIF.
*ENDFORM.


*&-----------------------------------------------------*
*&      Form  get_vtbfha
*&-----------------------------------------------------*
*FORM get_vtbfha.
*  SELECT SINGLE rpzahl
*    INTO temp-rpzahl
*    FROM vtbfhapo
*   WHERE bukrs EQ it_bkpf-bukrs
*     AND gjahr EQ it_bkpf-gjahr
*     AND belnr EQ it_bkpf-belnr.
*
*  SELECT SINGLE name1
*    FROM bp000
*    INTO l_name1
*   WHERE partnr = temp-rpzahl.
*
*ENDFORM.
*&-----------------------------------------------------*
*&      Form  get_vdarl
*&-----------------------------------------------------*
*FORM get_vdarl.
*  SELECT SINGLE ranl
*    INTO temp-ranl
*    FROM vdbeki
*   WHERE bukrs  EQ it_bkpf-bukrs
*     AND dgjahr EQ it_bkpf-gjahr
*     AND rvzblg EQ it_bkpf-belnr.
*
*  CHECK sy-subrc EQ 0.
*
*  SELECT SINGLE rdarnehm
*    INTO temp-rdarnehm
*    FROM vdarl
*   WHERE bukrs EQ it_bkpf-bukrs
*     AND ranl  EQ temp-ranl.
*
*ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  get_position_id
*&---------------------------------------------------------------------*
FORM get_position_id CHANGING p_code p_name.
  DATA: l_objnr LIKE imzo-objnr,
        l_posnr LIKE imzo-posnr,
        l_posid LIKE impr-posid.

*  p_assgn1 = '#############'.
  CONCATENATE 'OR'  it_bseg-aufnr INTO l_objnr.
  SELECT SINGLE posnr INTO l_posnr
    FROM imzo WHERE objnr EQ l_objnr.
  IF sy-subrc NE 0.
    EXIT.
  ENDIF.
  SELECT SINGLE a~posid b~post1 INTO (p_code, p_name)
    FROM impr AS a INNER JOIN impu AS b ON b~posnr EQ a~posnr
    WHERE a~posnr EQ l_posnr AND
          b~spras EQ sy-langu.
ENDFORM.                    " get_position_id
*&---------------------------------------------------------------------*
*&      Form  check_iv_order
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_IT_L_CODE  text
*      <--P_IT_L_NAME1  text
*----------------------------------------------------------------------*
FORM check_iv_order CHANGING p_code
                             p_name.
  SELECT SINGLE aufnr ktext INTO (p_code, p_name)
  FROM aufk
  WHERE auart = 'P'
  AND   aufnr = p_code.
ENDFORM.                    " check_iv_order
*&---------------------------------------------------------------------*
*&      Form  READ_COBK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_cobk.
  RANGES : r_bstat FOR bkpf-bstat,
           r_usnam FOR bkpf-usnam,
           r_ppnam FOR bkpf-ppnam.

  REFRESH it_bkpf. CLEAR it_bkpf.
  REFRESH it_bseg. CLEAR it_bseg.
  REFRESH it_cb. CLEAR it_cb.


  SELECT kokrs belnr gjahr budat usnam bldat cpudt
         blart kwaer bltxt
     FROM cobk
      INTO CORRESPONDING FIELDS OF TABLE it_cb
    WHERE kokrs IN s_bukrs
      AND gjahr IN s_gjahr
      AND belnr IN s_belnr
*     AND blart IN s_blart
      AND vrgng IN ('RKU1', 'RKU2', 'RKU3')
      AND budat IN s_budat
*      and xblnr in s_xblnr
      AND cpudt IN s_cpudt.
*     AND bstat IN r_bstat
*     AND ( usnam IN r_usnam OR ppnam IN r_ppnam ).

  CLEAR : wa_t_cnt.
  DESCRIBE TABLE it_cb LINES wa_t_cnt.
  IF wa_t_cnt <> 0.
*    MESSAGE w000(zmfi) WITH text-018.
*  ELSE.
    LOOP AT it_cb.
      MOVE-CORRESPONDING it_cb TO it_bkpf.
      MOVE it_cb-kokrs         TO it_bkpf-bukrs.
      MOVE it_cb-kwaer         TO it_bkpf-waers.
      APPEND it_bkpf.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " READ_COBK
*&---------------------------------------------------------------------*
*&      Form  write_internal_documents
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_internal_documents.
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
  DATA: l_pos TYPE i.
  l_pos = strlen( wa_l_company_name ) + 1.
  wa_l_company_name+l_pos = wa_l_name2.
*Internal documents
  SORT it_bkpf BY  belnr gjahr.                             "UD1K922521
  LOOP AT it_bkpf WHERE chkbox EQ 'X'.
    CLEAR it_s.        " clear summary fields
    CLEAR sy-pagno.
    AT NEW belnr.
      PERFORM read_coep.

      NEW-PAGE.
      PERFORM write_header_line.
    ENDAT.
    LOOP AT it_coep.
      PERFORM write_internal_detail_line.

    ENDLOOP.
    PERFORM write_sign_box.

  ENDLOOP.

ENDFORM.                    " write_internal_documents
*&---------------------------------------------------------------------*
*&      Form  read_coep
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_coep.
  SELECT *
    FROM coep
    INTO CORRESPONDING FIELDS OF TABLE it_coep
   WHERE kokrs = it_bkpf-bukrs
     AND gjahr = it_bkpf-gjahr
     AND belnr = it_bkpf-belnr
     AND wrttp IN ('04' , '11').
  SORT it_coep BY wtgbtr .
ENDFORM.                    " read_coep
*&---------------------------------------------------------------------*
*&      Form  write_internal_detail_line
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_internal_detail_line.
  DATA : wa_menge(10) TYPE p.

  CLEAR it_l.
  FORMAT   INTENSIFIED OFF.
  DATA : chk1(1),
         chk2(1),
         chk3(1).
**-> read Cost elements's short text
  SELECT SINGLE ktext
    FROM csku
    INTO it_l-txt20
   WHERE spras = sy-langu
     AND ktopl = 'HNA1'
     AND kstar = it_coep-kstar.

**-> dmbtr
  MOVE it_coep-wtgbtr TO it_l-dr.
  ADD  it_coep-wtgbtr TO it_s-sdr.

  IF sy-linno > 85.
    NEW-PAGE.
    PERFORM write_internal_detail_line.
  ENDIF.

**-> detail 1 line
  WRITE:  /(01) wa_vl NO-GAP,
           (03) it_coep-buzei NO-GAP, wa_vl NO-GAP, " Line No
           (20) it_l-txt20 NO-GAP, wa_vl NO-GAP, " Acct Short Text
           (49) it_l-acct_type  NO-GAP,
           (08) it_l-taxcd      NO-GAP, "Tax Code
           (04) it_l-rate       NO-GAP, "Tax Rate
           (01) wa_vl NO-GAP,
           (20) it_l-dr NO-GAP CURRENCY it_coep-twaer  NO-ZERO, "Dr Amt
           (01) wa_vl NO-GAP,
           (20) it_l-cr NO-GAP CURRENCY it_coep-twaer  NO-ZERO, "Cr Amt
           (01) wa_vl.

* 2 line
  DATA: wa_l_ktext    TYPE ktext,
        wa_l_func_area TYPE fkber,
        wa_l_kostl  TYPE char10.
  IF it_coep-objnr+6(10) NE ' '.
    CALL FUNCTION 'Z_FFI_GET_KOSTL'
         EXPORTING
              i_kokrs     = 'H201'
              i_kostl     = it_coep-objnr+6(10)
         IMPORTING
              e_ktext     = wa_l_ktext
              e_func_area = wa_l_func_area.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
         EXPORTING
              input  = it_coep-objnr+6(10)  "Cost Center
         IMPORTING
              output = wa_l_kostl.

    CONCATENATE wa_l_ktext '/' wa_l_kostl '/' wa_l_func_area
           INTO it_l-ordno.

  ENDIF.
  WRITE:/  wa_vl NO-GAP,
         (03) ' ' NO-GAP,  wa_vl NO-GAP,
         (20) it_coep-kstar  NO-GAP, wa_vl NO-GAP,  " G/L Account
         (31) it_l-ordno NO-GAP,
         (18) it_l-assgn1 NO-GAP,
         (12) it_l-assgn2 NO-GAP,
         (01) wa_vl NO-GAP,
         (20) it_l-drt NO-GAP CURRENCY it_coep-twaer NO-ZERO, "Dr Amt
         (01) wa_vl NO-GAP,
         (20) it_l-crt NO-GAP CURRENCY it_coep-twaer NO-ZERO, "Cr Amt
         (01) wa_vl NO-GAP.
  CLEAR : it_l-assgn1, it_l-assgn2.
  CLEAR : it_l-ordno, wa_menge.
* 3 line
  CLEAR wa_final_txt.
  IF it_coep-belnr   <> ' '.
    SELECT SINGLE bltxt  INTO wa_final_txt
     FROM cobk
    WHERE belnr = it_coep-belnr
    AND   kokrs = 'H201'.
  ENDIF.
  WRITE:/ wa_vl NO-GAP,
    (03) ' ' NO-GAP, wa_vl NO-GAP,
    (20) wa_final_txt NO-GAP, wa_vl NO-GAP,
    (49) it_bseg-sgtxt NO-GAP.

  WRITE : (12) it_bseg-fdtag NO-GAP NO-ZERO, wa_vl NO-GAP.
  WRITE:
       (20) ' ' NO-GAP, wa_vl NO-GAP,
       (20) ' ' NO-GAP, wa_vl NO-GAP.

  NEW-LINE. ULINE AT (wa_width).

ENDFORM.                    " write_internal_detail_line
*&---------------------------------------------------------------------*
*&      Form  read_rbkp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_rbkp.
  RANGES : r_bstat FOR bkpf-bstat,
           r_usnam FOR bkpf-usnam,
           r_ppnam FOR bkpf-ppnam.

*  REFRESH it_bkpf. CLEAR it_bkpf.
*  REFRESH it_bseg. CLEAR it_bseg.

* check and read parked data
  IF p_parked = 'X'.
    r_bstat-sign   = 'I'.
    r_bstat-option = 'EQ'.
    r_bstat-low    = 'V'.
    APPEND r_bstat.
  ENDIF.
* check and read posted data
  IF p_posted = 'X'.
    r_bstat-sign   = 'I'.
    r_bstat-option = 'EQ'.
    r_bstat-low    = ' '.
    APPEND r_bstat.
    r_bstat-low    = 'A'.  "parked
    APPEND r_bstat.
    r_bstat-low    = 'B'.  "park & completed
    APPEND r_bstat.
*    r_bstat-low    = 'C'.  "parked & held
*    APPEND r_bstat.
*    r_bstat-low    = 'D'.  "enter & held
*    APPEND r_bstat.
    r_bstat-low    = 'E'.  "parked & released
    APPEND r_bstat.
  ENDIF.

  IF p_own EQ 'X'.
    r_usnam-sign = 'I'.
    r_usnam-option = 'EQ'.
    r_usnam-low = sy-uname.
    APPEND r_usnam.
    r_ppnam-sign = 'I'.
    r_ppnam-option = 'EQ'.
    r_ppnam-low = sy-uname.
    APPEND r_ppnam.
  ENDIF.

  SELECT bukrs belnr gjahr
         budat usnam tcode xblnr
         bldat cpudt blart bktxt waers
         stblg stjah
    FROM rbkp
    INTO CORRESPONDING FIELDS OF TABLE it_bk
   WHERE bukrs IN s_bukrs
     AND gjahr IN s_gjahr
     AND belnr IN s_rbelnr
     AND blart IN s_blart
     AND budat IN s_budat
     AND xblnr IN s_xblnr
     AND cpudt IN s_cpudt
     AND rbstat IN r_bstat
     AND ( usnam IN r_usnam ).

**====JHS MODIFY 2004.01.26
*  CLEAR : wa_t_cnt.
*  DESCRIBE TABLE it_bk LINES wa_t_cnt.
*  IF wa_t_cnt > 0.
*    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_vbkpf
*    FROM vbkpf
*    FOR ALL ENTRIES IN it_bk
*    WHERE belnr = it_bk-belnr.
*  ENDIF.

  LOOP AT it_bk.
**----2004/01/26.   jhs modify
*    IF it_bk-bstat = 'V'.
*      READ TABLE it_vbkpf WITH KEY belnr = it_bk-belnr
*                                   xprfg = 'X'.
*      IF sy-subrc <> 0.
*        CONTINUE.
*      ENDIF.
*    ENDIF.
*
    MOVE-CORRESPONDING it_bk TO it_bkpf.
    APPEND it_bkpf.
  ENDLOOP.

ENDFORM.                    " read_rbkp
*&---------------------------------------------------------------------*
*&      Form  read_rseg
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_rseg.
  DATA: wa_l_count TYPE i VALUE 0.

  REFRESH it_bseg. CLEAR it_bseg.

  SELECT *
    FROM rseg
    INTO CORRESPONDING FIELDS OF TABLE it_bseg
   WHERE bukrs = it_bkpf-bukrs
     AND gjahr = it_bkpf-gjahr
     AND belnr = it_bkpf-belnr.

* Begin of changes -  UD1K918802
* Since RSEG table doesn't Hold G/L account, Cost center details..
* Try to fetch from EKKN by using PO number & Po line item
* Also same internal table is used for both BSEG & RSEG, In BSEG BUZEI
*length is 3 and where as in RSEG it is 6 so value are trucated
* Avoided using different internal table just to avoid further changes
* in the program. SO had to write select STMT within LOOP.
  IF p_parked EQ 'X'.
    LOOP AT it_bseg.
      SELECT SINGLE sakto kostl aufnr INTO
       (wa_sakto, wa_kostl,wa_aufnr) FROM ekkn
       WHERE  ebeln =  it_bseg-ebeln
           AND ebelp = it_bseg-ebelp.
      IF sy-subrc EQ 0.
        it_bseg-kostl =  wa_kostl.
        it_bseg-hkont =  wa_sakto.
        it_bseg-aufnr = wa_aufnr.
      ENDIF.
      SELECT SINGLE buzei INTO wa_buzei
        FROM rseg
       WHERE  belnr = it_bkpf-belnr
          AND gjahr = it_bseg-gjahr
          AND bukrs = it_bseg-bukrs
          AND ebeln = it_bseg-ebeln
          AND ebelp = it_bseg-ebelp.
      IF sy-subrc EQ 0.
        UNPACK wa_buzei TO wa_buzei.
        it_bseg-buzei = wa_buzei.
      ENDIF.
      MODIFY it_bseg.
    ENDLOOP.
  ENDIF.
* End of changes -  UD1K918802

  SORT it_bseg BY buzei.

*  LOOP AT it_bseg.
*    IF it_bseg-hkont IS INITIAL.
*      it_bseg-hkont = it_bseg-saknr.
*    ENDIF.
***--> Noted items : change hkont
*    IF it_bkpf-bstat = 'S'.
*      SELECT SINGLE skont INTO it_bseg-hkont FROM t074
*        WHERE ktopl = 'HNA1' AND
*              koart = it_bseg-koart AND
*              umskz = it_bseg-zumsk AND
*              hkont = it_bseg-saknr.
*
*    ENDIF.
*
*    MODIFY it_bseg TRANSPORTING hkont.
*    ADD 1 TO wa_l_count.
*  ENDLOOP.
*
*  wa_total_page_number = wa_l_count  / 16 + 1.

ENDFORM.                    " read_rseg
*&---------------------------------------------------------------------*
*&      Form  printing
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM printing.

  CALL FUNCTION 'OPEN_FORM'
       EXPORTING
            device                      = 'PRINTER'
            dialog                      = 'X'
            form                        = 'ZFI_IV_BARCODE'
            language                    = sy-langu
            options                     = zoptions
            raw_data_interface          = '*'
       EXCEPTIONS
            canceled                    = 1
            device                      = 2
            form                        = 3
            options                     = 4
            unclosed                    = 5
            mail_options                = 6
            archive_error               = 7
            invalid_fax_number          = 8
            more_params_needed_in_batch = 9
            spool_error                 = 10
            OTHERS                      = 11.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

  LOOP AT it_bkpf WHERE chkbox EQ 'X'.

    CONCATENATE it_bkpf-bukrs it_bkpf-awkey(10) it_bkpf-gjahr
     INTO $awkey.
    CLEAR bds_bar_in.

    SELECT SINGLE * FROM bds_bar_in WHERE
            object_key EQ $awkey.
    IF bds_bar_in-barcode IS INITIAL.
      bds_bar_in-barcode = '1234567890'.
    ENDIF.

    CONCATENATE '*' bds_bar_in-barcode '*' INTO $barcode.

    CALL FUNCTION 'WRITE_FORM'
         EXPORTING
              element  = 'PRINT_LABEL'
              function = 'SET'
              type     = 'BODY'
              window   = 'MAIN'.

    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDLOOP.

  CALL FUNCTION 'CLOSE_FORM'.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " printing
*&---------------------------------------------------------------------*
*&      Form  init_print
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_print.
*move zqmnum+6(6)  to zoptions-TDDATASET. "UD1K940504
  MOVE '1' TO zoptions-tdcopies.
  MOVE 'X' TO zoptions-tdimmed.
  MOVE 'X' TO zoptions-tdnewid.
  MOVE zprinter TO zoptions-tddest.
  APPEND zoptions.

ENDFORM.                    " init_print

*&---------------------------------------------------------------------*
*&      Form  pri_bar_one
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pri_bar_1.

  CALL FUNCTION 'OPEN_FORM'
       EXPORTING
            device                      = 'PRINTER'
            dialog                      = 'X'
            form                        = 'ZFI_IV_BARCODE'
            language                    = sy-langu
            options                     = zoptions
            raw_data_interface          = '*'
       EXCEPTIONS
            canceled                    = 1
            device                      = 2
            form                        = 3
            options                     = 4
            unclosed                    = 5
            mail_options                = 6
            archive_error               = 7
            invalid_fax_number          = 8
            more_params_needed_in_batch = 9
            spool_error                 = 10
            OTHERS                      = 11.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

  CONCATENATE it_bkpf-bukrs it_bkpf-awkey(10) it_bkpf-gjahr
   INTO $awkey.
  CLEAR bds_bar_in.

  SELECT SINGLE * FROM bds_bar_in WHERE
          object_key EQ $awkey.
  IF bds_bar_in-barcode IS INITIAL.
    bds_bar_in-barcode = '1234567890'.
  ENDIF.

  CONCATENATE '*' bds_bar_in-barcode '*' INTO $barcode.

  CALL FUNCTION 'WRITE_FORM'
       EXPORTING
            element  = 'PRINT_LABEL'
            function = 'SET'
            type     = 'BODY'
            window   = 'MAIN'.

  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CALL FUNCTION 'CLOSE_FORM'.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    " pri_bar_one
