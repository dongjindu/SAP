REPORT zficbo110l NO STANDARD PAGE HEADING
                                   LINE-SIZE 120
                                   LINE-COUNT 90.

INCLUDE: <SYMBOL>.

* Table i??i??.
TABLES: t001, T007S, T005,
        bkpf,                          " i??i‘œi?¤e?”i…Œi?´e¸”
        vbsegk,                        "
        vbsegd,                        "
        vbsegs,                        "
        vbsega,
        vbkpf,
        bseg,
*        ttxjt,                         " i??i?…i??e?´i?­
        j_1bbranch,
        cskt,
        vbset,
        usr21,
        adcp,
        anla,
        kna1,                          " e±°e??i²?
        lfa1,                          " e§¤i?…e±°e??i²?
        tgsbt,                         " i??i?…i??i?­
        skat,                          " e³?i??e³¼eª?eª…
        AUFK, csks.

* Structure for bkpf
DATA: BEGIN OF ibkpf occurs 100,
        bukrs            LIKE  bkpf-bukrs,
        belnr            LIKE  bkpf-belnr,        " i??i‘œe²?i?¸
        gjahr            LIKE  bkpf-gjahr,
        xblnr            LIKE  bkpf-xblnr,
        budat            LIKE  bkpf-budat,        " i??e¸°i?¼
        usnam            LIKE  bkpf-usnam,        " i?‘i?±e¶€i?œ/i?‘i?±
        bstat            LIKE  bkpf-bstat,        " i??i‘œi??i?œ
        depart           LIKE  adcp-department,   " i†Œi†?e¶€i?œeª…
        tcode            like  bkpf-tcode,
        XPRFG            like  vbkpf-XPRFG,       "complete or not
      END   OF ibkpf.

* temp table
DATA: BEGIN OF ibseg occurs 0,
        buzei            LIKE  bseg-buzei,        " seq.
        hkont            LIKE  bseg-hkont,        " G/L acct
        txt20            LIKE  skat-txt20,        " gl text
        BSCHL            LIKE  bseg-BSCHL,        " Posting Key
        accnt            LIKE  bseg-kunnr,        " Account(D/K/A)
        name1            LIKE  kna1-name1,        " Account Name

        AUFNR            LIKE  bseg-aufnr,        " internal order
        kostl            LIKE  bseg-kostl,        " cost center
        ktext            LIKE  cepct-ktext,       " cost obj text

        zuonr            LIKE  bseg-zuonr,        " Assignment
        sgtxt            LIKE  bseg-sgtxt,        " TEXT

        gsber            LIKE  bseg-gsber,        " BA
        gtext            LIKE  tgsbt-gtext,       " BA Name

        mwskz            LIKE  vbsega-mwskz,      " Tax Code
        txjcd            LIKE  vbsega-txjcd,      " Jurisdiction
        bupla            LIKE  bseg-bupla,        " biz place (KR)
        jrtxt            LIKE  ttxjt-text1,       " Tax name
        WMWST            LIKE  vbsegd-WMWST,      " Tax Amt

        dmbtr            LIKE  bseg-dmbtr,        " Amt
        s_gumak          LIKE  bseg-dmbtr,        " Debit
        h_gumak          LIKE  bseg-dmbtr,        " Credit
        shkzg            LIKE  bseg-shkzg,        " Dr/Cr
        kunnr            LIKE  bseg-kunnr,        " vendor
        lifnr            LIKE  bseg-lifnr,        " customer
        ANLN1            LIKE  bseg-ANLN1,        " asset
        ANLN2            LIKE  bseg-ANLN2,        " asset sub
        ANBWA            LIKE  bseg-ANBWA,        " asset transaction(3)
        koart            LIKE  bseg-koart,
        SAKNR            LIKE  bseg-saknr,        " G/L acct
        ZTERM            LIKE  bseg-ZTERM,        " pay term
        ZLSCH            LIKE  bseg-ZLSCH,        " pay method
        ZLSPR            LIKE  bseg-ZLSPR,        " pay block
        AUGBL            LIKE  bseg-AUGBL,        " clearing doc
      END   OF ibseg.

TYPES:  BEGIN OF tsave,
        hkont            LIKE  bseg-hkont,        " G/L e³?i??i½”e“œ
        txt20            LIKE  skat-txt20,        " e³?i??e³¼eª?eª…
        BSCHL            LIKE  bseg-BSCHL,        " Posting Key
        accnt            LIKE  bseg-kunnr,        " Account(D/K/A)
        name1            LIKE  kna1-name1,        " e³?e°?eª…
        kostl            LIKE  bseg-kostl,        " i†?i?µi?¼i?°
        ktext            LIKE  cepct-ktext,       " e¶€i?œeª…
        sgtxt            LIKE  bseg-sgtxt,        " TEXT
        gsber            LIKE  bseg-gsber,        " i??i?…i??i?­
        gtext            LIKE  tgsbt-gtext,       " i??i?…i??i?­i?¤eª…
        mwskz            LIKE  vbsega-mwskz,      " i?¸e¸?i½”e“œ
        txjcd            LIKE  vbsega-txjcd,      " i?¸e?´i??i?…i??
        jrtxt            LIKE  ttxjt-TEXT1,       " i?¸e?´i??i?…i??e?´
        zuonr            LIKE  bseg-zuonr,        " e°°e¶€
        ANLN2            LIKE  bseg-ANLN2,        " asset sub
        ANBWA            LIKE  bseg-ANBWA,        " asset transaction(3)
      END   OF tsave.

DATA: d1 type tsave,
      s1 type tsave.

* Types
TYPES: BEGIN OF tiline1,
        belnr            LIKE  bkpf-belnr,        " i??i‘œe²?i?¸
        xblnr            LIKE  bkpf-xblnr,
        budat            LIKE  bkpf-budat,        " i??e¸°i?¼
        usnam            LIKE  bkpf-usnam,        " i?‘i?±e¶€i?œ/i?‘i?±
        depart           LIKE  adcp-department,   " i†Œi†?e¶€i?œeª…
        hkont            LIKE  bseg-hkont,        " G/L e³?i??i½”e“œ
        buzei            LIKE  bseg-buzei,        " seq.
        BSCHL            LIKE  bseg-BSCHL,        " Posting Key
        txt20            LIKE  skat-txt20,        " e³?i??e³¼eª?eª…
        accnt            LIKE  bseg-kunnr,        " e³?e°?/eµ?e§¤i²?
        ANLN2            LIKE  bseg-ANLN2,        " asset sub
        ANBWA            LIKE  bseg-ANBWA,        " asset transaction(3)
        name1            LIKE  kna1-name1,        " e³?e°?eª…/eµ?e§¤i²?
        kostl            LIKE  bseg-kostl,        " i†?i?µi?¼i?°
        ktext            LIKE  cepct-ktext,       " e¶€i?œeª…
        sgtxt            LIKE  bseg-sgtxt,        " TEXT
        gsber            LIKE  bseg-gsber,        " i??i?…i??i?­
        gtext            LIKE  tgsbt-gtext,       " i??i?…i??i?­i?¤eª…
        mwskz            LIKE  vbsega-mwskz,      " i?¸e¸?i½”e“œ
        txjcd            LIKE  vbsega-txjcd,      " i?¸e?´i??i?…i??
        jrtxt            LIKE  ttxjt-TEXT1,       "
        zuonr            LIKE  bseg-zuonr,        " e°°e¶€
        XPRFG            like  vbkpf-XPRFG,       " complete or not

        s_gumak          LIKE  bseg-dmbtr,        " i°¨e³€e¸?i?¡
        h_gumak          LIKE  bseg-dmbtr,        " eŒ€e³€e¸?i?¡
      END   OF tiline1.

DATA: iline1 type tiline1 occurs 0 with header line,
      iline2 type tiline1 occurs 0 with header line,
      iline3 type tiline1 occurs 0 with header line.


DATA: GCR like t001-WAERS,
      iline1_line(120)    TYPE  c,
      temp-ranl          like   vdbeki-ranl,
      temp-rdarnehm      like   vdarl-rdarnehm,
      p-sgtxt(31)         TYPE  c,                " TEXT
      temp-awtyp          like bkpf-awtyp,
      temp-rpzahl         like vtbfhapo-rpzahl,
      p-tgtxt(28)         TYPE  c,
      top_count(02)       TYPE  c,
      count_num           TYPE  n,
      end_count(02)       TYPE  c,
      p-hkont             LIKE  bseg-hkont,
      ibkpf-buzei          LIKE  bseg-buzei,
      ibkpf-buzei1         LIKE  bseg-buzei,
      count_line(02)      TYPE  n  VALUE 0,
      g_butxt             LIKE  t001-butxt,
      tot-s_gumak         LIKE  bseg-dmbtr,       " i°¨e³€e¸?i?¡
      tot-h_gumak         LIKE  bseg-dmbtr,       " eŒ€e³€e¸?i?¡
      hap-s_gumak         LIKE  bseg-dmbtr,       " i°¨e³€e¸?i?¡
      hap-h_gumak         LIKE  bseg-dmbtr.       " eŒ€e³€e¸?i?¡

data: GV like sy-vline,
      GUL like sy-uline,
      GS8(8) type c value '        '.

data: it007s like t007s occurs 0 with header line.

SELECTION-SCREEN BEGIN OF   BLOCK bl1
                            WITH  FRAME
                            TITLE text-001. "Select-Option
PARAMETERS:        p_bukrs      LIKE  bkpf-bukrs  memory id BUK.
PARAMETERS:        p_gjahr      LIKE  bkpf-gjahr  memory id GJR.
SELECT-OPTIONS:    p_belnr      FOR   bkpf-belnr. " OBLIGATORY.
SELECTION-SCREEN END   OF   BLOCK bl1.
*SELECTION-SCREEN BEGIN OF   BLOCK bl2
*                            WITH  FRAME
*                            TITLE text-099.
PARAMETERS:        accsum   type c default ' '   no-display.
PARAMETERS:        p_park   type c default ' '. "no-display.
PARAMETERS:        p_own    type c default 'X'   no-display.

*SELECTION-SCREEN END   OF   BLOCK bl2.

*---------------------------------------------------------------------*
INITIALIZATION.
*---------------------------------------------------------------------*
  GUL = sy-uline.
  GV = sy-vline.

*---------------------------------------------------------------------*
START-OF-SELECTION.
*---------------------------------------------------------------------*
  if p_park = space and p_belnr is initial.
    exit.
  endif.

* Check if entered company codes and chart of accounts are valid
  SELECT SINGLE * FROM T001 WHERE BUKRS = P_BUKRS.
  check sy-subrc = 0.
  g_butxt =  t001-butxt.
  GCR     =  t001-WAERS.  "currency
  select single * from t005 where LAND1 = t001-LAND1.
* print tax code, not jurisdiction
  select * from T007S into table it007s
     where spras = sy-langu
       and kalsm = t005-KALSM.

  REFRESH  iline1.
  PERFORM  bkpf_read_process.
  perform  bseg_read.

*---------------------------------------------------------------------*
END-OF-SELECTION.
*---------------------------------------------------------------------*
  PERFORM  output_process.

************************************************************************
* AT LINE-SELECTION                                                    *
************************************************************************
AT LINE-SELECTION.
  check iline2-belnr <> space.
  SET PARAMETER ID 'BLP' FIELD iline2-belnr.
  SET PARAMETER ID 'BUK' FIELD p_bukrs.
  SET PARAMETER ID 'GJR' FIELD p_gjahr.

  CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

*---------------------------------------------------------------------*
TOP-OF-PAGE.
*---------------------------------------------------------------------*
  IF  top_count  =  'AA'.
    PERFORM display_top_of_page.
  ENDIF.

*---------------------------------------------------------------------*
END-OF-PAGE.
*---------------------------------------------------------------------*
  PERFORM  display_end_of_page.

*---------------------------------------------------------------------*
*       FORM DISPLAY_TOP_OF_PAGE                                      *
*---------------------------------------------------------------------*
FORM display_top_of_page.

  FORMAT   INTENSIFIED  OFF.
  SKIP 6.

  WRITE:   'Accounting Document'  TO  iline1_line.
  WRITE:   iline1_line CENTERED.
  WRITE:   '============================'  TO  iline1_line.
  WRITE:/  iline1_line CENTERED.

* PRINT-CONTROL CPI 18.
  SKIP 1.
  WRITE:/4  ' Date:' NO-GAP, sy-datum  DD/MM/YYYY,
        40  'Company:', p_bukrs, g_butxt,
        90  'Time:' NO-GAP, sy-uzeit  USING EDIT MASK '__:__:__',
        106 'Page:' No-GAP, sy-PAGNO NO-GAP.

  WRITE:/4 GUL(113).              " e°‘i¤?i§?..
  FORMAT   INTENSIFIED  OFF.
  WRITE:/4 GV NO-GAP, 'Doc Number' NO-GAP CENTERED,    " i??i‘œe²?
           GV NO-GAP, iline2-belnr NO-GAP CENTERED,   "
        29 GV NO-GAP, 'TrnDate' NO-GAP CENTERED,       " i??e¸°i?¼
           GV NO-GAP, iline2-budat NO-GAP CENTERED,   "
        50 GV NO-GAP, 'Inv/Ref' NO-GAP CENTERED,        " i?‘i?±e¶€
           GV NO-GAP, iline2-xblnr NO-GAP LEFT-JUSTIFIED,
        91 GV NO-GAP, 'User ID' NO-GAP CENTERED,        " i?‘i?±
           GV NO-GAP, iline2-usnam NO-GAP CENTERED.
  hide: iline2-belnr.

  if ibkpf-bstat = 'V'.
    write: GV NO-GAP, 'P' NO-GAP, GV NO-GAP.
    if iline2-XPRFG = 'X'.
      write: SYM_CHECK_MARK AS SYMBOL NO-GAP.
    else.
      write: ' ' no-gap.
    endif.
  else.
    write: GV NO-GAP, 'R' NO-GAP, GV NO-GAP, ' ' NO-GAP.
  endif.
  write: GV NO-GAP.

  WRITE:/4 GUL(113).

  WRITE:/4  GV NO-GAP, '   '   NO-GAP CENTERED,             " 3 space
            GV NO-GAP, 'Account Name        ' NO-GAP CENTERED,
            GV NO-GAP, 'Account  ' NO-GAP.
  WRITE: 60 GV NO-GAP, 'Assignment              ' NO-GAP,
         82 GV NO-GAP, 'Debit Amt       ' NO-GAP CENTERED,
            GV NO-GAP, 'Credit Amt      ' NO-GAP CENTERED,
            GV.

* uline
  WRITE:/4 GV NO-GAP, '   ' NO-GAP CENTERED,
           GV NO-GAP, GUL(73) NO-GAP,
           GV NO-GAP, '                ' NO-GAP,
           GV NO-GAP, '                ' NO-GAP,
           GV.

* 2 line
  WRITE:/4 GV NO-GAP, ' No' NO-GAP CENTERED,                " 3 space
           GV NO-GAP, 'G/L Account         ' NO-GAP CENTERED,
           GV NO-GAP, 'CstCtr/OrderNo      ' NO-GAP.
  WRITE:
        60 GV NO-GAP, 'Tax Code' NO-GAP CENTERED,
           ' '      NO-GAP, '      Jrsdtn' NO-GAP,
           GV NO-GAP, '                ' NO-GAP,
           GV NO-GAP, '                ' NO-GAP,
           GV.

  WRITE:/4 GV NO-GAP, '   ' NO-GAP CENTERED,
           GV NO-GAP, GUL(73) NO-GAP,
           GV NO-GAP, '                ' NO-GAP,
           GV NO-GAP, '                ' NO-GAP,
           GV.

* 3 line
  WRITE:/4 GV NO-GAP, '   ' NO-GAP CENTERED,                " 3 space
             GV NO-GAP,
             '                    ' NO-GAP,
             GV NO-GAP, 'Notes      ',
             '                                        ' NO-GAP.
  WRITE:
           GV NO-GAP, '                ' NO-GAP,
           GV NO-GAP, '                ' NO-GAP,
           GV.
  WRITE:/4 GUL(113).                                    " e°‘i¤?i§?

  Clear: iline2-belnr.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  BKPF_READ_PROCESS
*&---------------------------------------------------------------------*
FORM bkpf_read_process.

  CLEAR    count_num.
  REFRESH: iline1, iline3.


  if p_park = 'X'.
    if p_own = 'X'.
      SELECT bukrs belnr budat usnam gjahr bstat xblnr tcode XPRFG
        from vbkpf
        INTO CORRESPONDING FIELDS OF TABLE ibkpf
        WHERE  bukrs  =   p_bukrs
        AND    gjahr  =   p_gjahr
        AND    belnr  IN  p_belnr
        and    usnam  = sy-uname.
    else.
      SELECT bukrs belnr budat usnam gjahr bstat xblnr tcode XPRFG
        from vbkpf
        INTO CORRESPONDING FIELDS OF TABLE ibkpf
        WHERE  bukrs  =   p_bukrs
        AND    gjahr  =   p_gjahr
        AND    belnr  IN  p_belnr.
    endif.

  else.
    SELECT bukrs belnr budat usnam gjahr bstat xblnr tcode
      from bkpf
      INTO CORRESPONDING FIELDS OF TABLE ibkpf
      WHERE  bukrs  =   p_bukrs
      AND    gjahr  =   p_gjahr
      AND    belnr  IN  p_belnr
      AND    bstat  IN  (' ', 'A', 'B', 'D', 'S', 'V')
      AND    blart  in  ( select blart from T003 ).  "tunning
  endif.

ENDFORM.                               " BKPF_READ_PROCESS

*&---------------------------------------------------------------------*
*&      Form  BSEG_READ_PROCESS
*&---------------------------------------------------------------------*
FORM bseg_read_process.
  refresh ibseg.

  SELECT * FROM bseg  into corresponding fields of table ibseg
    WHERE  bukrs  =  ibkpf-bukrs
    AND    gjahr  =  ibkpf-gjahr
    AND    belnr  =  ibkpf-belnr.

  perform fill_itab.

ENDFORM.                               " BSEG_READ_PROCESS
*&---------------------------------------------------------------------*
*&      Form  copy_to_iline1
*&---------------------------------------------------------------------*
FORM copy_to_iline1.
  CLEAR   iline1.

*  MOVE: ibkpf-belnr   TO  iline1-belnr,
*        ibkpf-xblnr   TO  iline1-xblnr,
*        ibkpf-budat   TO  iline1-budat,
*        ibkpf-usnam   TO  iline1-usnam,
*        ibkpf-XPRFG   TO  iline1-XPRFG,
*        ibkpf-depart  TO  iline1-depart.
*
*  MOVE: ibseg-buzei   TO  iline1-buzei,
*        ibseg-hkont   TO  iline1-hkont,
*        ibseg-txt20   TO  iline1-txt20,
*        ibseg-accnt   TO  iline1-accnt,
*        ibseg-kostl   TO  iline1-kostl,
*        ibseg-bschl   TO  iline1-bschl,
*        ibseg-ktext   TO  iline1-ktext,
*        ibseg-name1   TO  iline1-name1,
*        ibseg-sgtxt   TO  iline1-sgtxt,
*        ibseg-gsber   TO  iline1-gsber,
*        ibseg-gtext   TO  iline1-gtext,
*        ibseg-mwskz   TO  iline1-mwskz,
*        ibseg-txjcd   TO  iline1-txjcd,
*        ibseg-zuonr   TO  iline1-zuonr,
*        ibseg-s_gumak TO  iline1-s_gumak,
*        ibseg-h_gumak TO  iline1-h_gumak.
  move-corresponding ibkpf to iline1.
  move-corresponding ibseg to iline1.

  if ibseg-koart = 'K' or ibseg-koart = 'D'.
    iline1-kostl = '!:# @'.
    REPLACE '!' WITH ibseg-zterm INTO iline1-kostl. "pay term
    REPLACE '#' WITH ibseg-zlsch INTO iline1-kostl. "pay method
    REPLACE '@' WITH ibseg-zlspr INTO iline1-kostl. "pay block

    iline1-ktext = ibseg-AUGBL.        " clearing doc
  endif.

* print tax code, not jurisdiction
  read table iT007S with key MWSKZ = ibseg-MWSKZ.
  ibseg-jrtxt = it007s-text1.

* branch (KR)
*  SELECT single name FROM j_1bbranch  INTO ibseg-jrtxt
*    where    branch  =  ibseg-txjcd.

  APPEND  iline1.

ENDFORM.                               " copy_to_iline1

*&---------------------------------------------------------------------*
*&      Form  OUTPUT_PROCESS
*&---------------------------------------------------------------------*
FORM output_process.
  REFRESH  iline2.
  CLEAR    iline2.

* detail or summary
  IF  accsum  =  'X'.
    PERFORM      jibgye_process.
  ELSE.
    iline2[] = iline1[].
    SORT  iline2 BY belnr buzei.
  ENDIF.

  CLEAR iline1.
  top_count = 'ZZ'.


  LOOP AT iline2.
    top_count = 'AA'.                             " i?¤e?”e?¼ i°?e?”e?¤.

    PERFORM   line_write_process.                 " i?­eª?i?? i“´e?¤.
    COMPUTE  hap-s_gumak  =  hap-s_gumak  +  iline2-s_gumak.
    COMPUTE  hap-h_gumak  =  hap-h_gumak  +  iline2-h_gumak.

    IF  sy-linno  >= 82.
      end_count   =  'ZZ'.                     " linei?? i°?i??e²?i?¸i§€
      PERFORM display_end_of_page.             " end-of-pagei°?e³?
*         NEW-PAGE.                                " i??e¡œi?´i??i?´i§€
      CLEAR   count_line.                      " e³€i??i´?e¸°i™”
    ENDIF.

    AT END OF belnr.
      IF sy-linno <  82.
        end_count   =  'AA'.                     " linei?? i°?i??e²?i?¸
        count_line  =  82 - sy-linno.
        SKIP    count_line.
        PERFORM display_end_of_page.
*         NEW-PAGE.
      ELSE.
*         END_COUNT   =  'ZZ'.                     " linei?? i°?i??e²?
*         PERFORM DISPLAY_END_OF_PAGE.
*         NEW-PAGE.
      ENDIF.
      CLEAR:   hap-s_gumak,  hap-h_gumak, count_line.   " i??e³? clear
    ENDAT.

  ENDLOOP.

ENDFORM.                               " OUTPUT_PROCESS

*&---------------------------------------------------------------------*
*&      Form  CSKS_READ_PROCESS
*&---------------------------------------------------------------------*
FORM csks_read_process.
  if not ibseg-aufnr is initial.
*order
    select single KTEXT from aufk into ibseg-ktext
      where aufnr   = ibseg-aufnr.
    ibseg-kostl = ibseg-aufnr+2(10).  "length = 10

  else.
*cost center
    SELECT single ktext FROM cskt INTO ibseg-ktext
      WHERE  kostl  =  ibseg-kostl.
  endif.
ENDFORM.                               " CSKS_READ_PROCESS
*&---------------------------------------------------------------------*
*&      Form  SKAT_PROCESS
*&---------------------------------------------------------------------*
FORM skat_process.

  SELECT SINGLE txt20 FROM skat INTO ibseg-txt20
    WHERE  spras  =  sy-langu
    AND    ktopl  =  t001-ktopl
    AND    saknr  =  ibseg-hkont.

  IF  ibseg-hkont  =  'TAXACCOUNT'.
    MOVE  'Tax Account'   TO  ibseg-txt20.
  ENDIF.

ENDFORM.                               " SKAT_PROCESS

*&---------------------------------------------------------------------*
*&      Form  TGSBT_PROCESS
*&---------------------------------------------------------------------*
FORM tgsbt_process.

  SELECT gtext FROM tgsbt INTO ibseg-gtext
    WHERE  gsber  =  ibseg-gsber.    " i??i?…i??i?­
*    ibseg-GTEXT   =  TGSBT-GTEXT.      " i??i?…i??i?­eª…
  ENDSELECT.

  CLEAR  sy-subrc.
ENDFORM.                               " TGSBT_PROCESS
*&---------------------------------------------------------------------*
*&      Form  LINE_WRITE_PROCESS
*&---------------------------------------------------------------------*
FORM line_write_process.
  FORMAT   INTENSIFIED OFF.

* trim 0
  Data: l_acct(10) type c.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
       EXPORTING
            input  = iline2-accnt
       IMPORTING
            output = l_ACCT.

  RESERVE 8 LINES.                                " e°‘i?´ 8e?¼i?¸i??e??
  WRITE:/4 GV NO-GAP, iline2-buzei NO-GAP,  " seq.
           GV NO-GAP, iline2-txt20 NO-GAP,  " e³?i??e³¼eª?eª…
           GV NO-GAP, iline2-name1(20) NO-GAP,  " Account name
           50 l_acct  NO-GAP RIGHT-JUSTIFIED.

  WRITE:60 ' ' NO-GAP, iline2-zuonr    NO-GAP LEFT-JUSTIFIED,
        80 iline2-BSCHL NO-GAP,
        82 GV  NO-GAP, iline2-s_gumak  NO-GAP CURRENCY GCR,
           GV  NO-GAP, iline2-h_gumak  NO-GAP CURRENCY GCR,
           GV.

* 2 line
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
       EXPORTING
            input  = iline2-kostl
       IMPORTING
            output = l_ACCT.
  WRITE:/4  GV NO-GAP, '   '         NO-GAP,  " space
            GV NO-GAP, iline2-hkont  NO-GAP,  " G/L
         29 GV NO-GAP, iline2-ktext(20) NO-GAP,
         50 l_acct  NO-GAP RIGHT-JUSTIFIED.

  WRITE: 61 iline2-mwskz NO-GAP LEFT-JUSTIFIED,
            ' ' NO-GAP, iline2-jrtxt(18) NO-GAP,
*        72 iline2-jrtxt(10) NO-GAP RIGHT-JUSTIFIED,
            GV NO-GAP, '                '
                               NO-GAP UNDER iline2-s_gumak,
            GV NO-GAP, '                '
                               NO-GAP UNDER iline2-h_gumak,
            GV NO-GAP.

* 3 line
  WRITE:/4 GV NO-GAP, '   '         NO-GAP,
           GV NO-GAP, '                    '    NO-GAP,
           GV NO-GAP, iline2-sgtxt NO-GAP UNDER iline2-ktext,
        79 iline2-ANBWA NO-GAP,
        82 GV NO-GAP, '                ' NO-GAP,
           GV NO-GAP, '                ' NO-GAP,
           GV.
  WRITE:/4 GUL(113).

  count_line  =  count_line  +  4.
ENDFORM.                               " LINE_WRITE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_END_OF_PAGE
*&---------------------------------------------------------------------*
FORM display_end_of_page.

  IF  end_count  =  'AA'.
    WRITE:/4 GUL(113).                          " line
  ENDIF.

  FORMAT   INTENSIFIED OFF.
  WRITE:/4 GV NO-GAP, ' MEMO :                 ' NO-GAP,
           '                                                '
                    NO-GAP CENTERED,  " e¹?e³?
           GV NO-GAP, 'Sum ' NO-GAP CENTERED,               " 13 i??e³?
           GV NO-GAP, hap-s_gumak NO-GAP  CURRENCY GCR,
           GV NO-GAP, hap-h_gumak NO-GAP  CURRENCY GCR,
           GV NO-GAP.
  WRITE:/4 GUL(113).

  FORMAT   INTENSIFIED OFF.
  WRITE:/4 GV NO-GAP, '    ' NO-GAP CENTERED,
           GV NO-GAP, 'Manager ' NO-GAP CENTERED,         " i?‘i?±
           GV NO-GAP, 'Director' NO-GAP CENTERED,           " i™?i?¸1
           GV NO-GAP, 'Sn.Drctr' NO-GAP CENTERED,           " i™?i?¸2
           GV NO-GAP, 'Proxy   ' NO-GAP CENTERED,         " i??e²°
           GV NO-GAP, '                ' NO-GAP CENTERED,
                       '             ' NO-GAP CENTERED,
           GV NO-GAP, '    ' NO-GAP CENTERED,         "
           GV NO-GAP, 'CostAcct' NO-GAP CENTERED,         " e?´e?¹
           GV NO-GAP, 'Accntng ' NO-GAP CENTERED,           " i™?i?¸1
           GV NO-GAP, 'Treasury' NO-GAP CENTERED,           " i™?i?¸2
           GV NO-GAP, 'CFO     ' NO-GAP CENTERED,         " i??e²°
           GV NO-GAP.

  WRITE:/4 GV NO-GAP, '    ' NO-GAP CENTERED,
           GV NO-GAP, GUL(08) NO-GAP CENTERED,     " i?‘i?±
           GV NO-GAP, GUL(08) NO-GAP CENTERED,     " e³¼  i??
           GV NO-GAP, GUL(08) NO-GAP CENTERED,     " e¶€  i??
           GV NO-GAP, GUL(08) NO-GAP CENTERED,     " space
           GV NO-GAP, '                ' NO-GAP CENTERED,
                       '             ' NO-GAP CENTERED,
           GV NO-GAP, '    ' NO-GAP CENTERED,         "
           GV NO-GAP, GUL(08) NO-GAP CENTERED,
           GV NO-GAP, GUL(08) NO-GAP CENTERED,
           GV NO-GAP, GUL(08) NO-GAP CENTERED,
           GV NO-GAP, GUL(08) NO-GAP CENTERED,
           GV NO-GAP.

  WRITE:/4 GV NO-GAP, 'Dept' NO-GAP CENTERED,
           GV NO-GAP, GS8    NO-GAP CENTERED,
           GV NO-GAP, GS8    NO-GAP CENTERED,
           GV NO-GAP, GS8    NO-GAP CENTERED,
           GV NO-GAP, GS8    NO-GAP CENTERED,
           GV NO-GAP, '                ' NO-GAP CENTERED,
                       '             ' NO-GAP CENTERED,
           GV NO-GAP, 'Acct' NO-GAP CENTERED,
           GV NO-GAP, GS8    NO-GAP CENTERED,
           GV NO-GAP, GS8    NO-GAP CENTERED,
           GV NO-GAP, GS8    NO-GAP CENTERED,
           GV NO-GAP, GS8    NO-GAP CENTERED,
           GV NO-GAP.

  WRITE:/4 GV NO-GAP, '    ' NO-GAP CENTERED,
           GV NO-GAP, GS8    NO-GAP CENTERED,
           GV NO-GAP, GS8    NO-GAP CENTERED,
           GV NO-GAP, GS8    NO-GAP CENTERED,
           GV NO-GAP, GS8    NO-GAP CENTERED,
           GV NO-GAP, '                ' NO-GAP CENTERED,
                       '             ' NO-GAP CENTERED,
           GV NO-GAP, '    ' NO-GAP CENTERED,
           GV NO-GAP, GS8    NO-GAP CENTERED,
           GV NO-GAP, GS8    NO-GAP CENTERED,
           GV NO-GAP, GS8    NO-GAP CENTERED,
           GV NO-GAP, GS8    NO-GAP CENTERED,
           GV NO-GAP.
  WRITE:/4 GUL(113).
ENDFORM.                    " DISPLAY_END_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  VBSEGSK_READ_PROCESS
*&---------------------------------------------------------------------*
FORM vbsegsk_read_process.

  refresh ibseg.

  SELECT * FROM vbsegk  into corresponding fields of table ibseg
    WHERE  ausbk  =  ibkpf-bukrs        " i?Œi??i½”e“œ
    AND    gjahr  =  ibkpf-gjahr
    AND    belnr  =  ibkpf-belnr.       " i??i‘œe²?i?¸

  perform fill_itab.

ENDFORM.                    " VBSEGSK_READ_PROCESS

*&---------------------------------------------------------------------*
*&      Form  VBSEGA_READ_PROCESS
*&---------------------------------------------------------------------*
FORM vbsega_read_process.

  refresh ibseg.

  SELECT * FROM vbsega  into corresponding fields of table ibseg
    WHERE  ausbk  =  ibkpf-bukrs                   " i?Œi??i½”e“œ
    AND    gjahr  =  ibkpf-gjahr
    AND    belnr  =  ibkpf-belnr.                  " i??i‘œe²?i?¸

  perform fill_itab.

ENDFORM.                    " VBSEGA_READ_PROCESS

*&---------------------------------------------------------------------*
*&      Form  VBSEGS_READ_PROCESS
*&---------------------------------------------------------------------*
FORM vbsegs_read_process.

  refresh ibseg.

  SELECT * FROM vbsegs  into corresponding fields of table ibseg
      WHERE  ausbk  =  ibkpf-bukrs        " i?Œi??i½”e“œ
      AND    gjahr  =  ibkpf-gjahr
      AND    belnr  =  ibkpf-belnr.       " i??i‘œe²?i?¸

  perform fill_itab.

ENDFORM.                    " VBSEGS_READ_PROCESS

*&---------------------------------------------------------------------*
*&      Form  VBSEGD_READ_PROCESS
*&---------------------------------------------------------------------*
FORM vbsegd_read_process.

  refresh ibseg.

  SELECT * FROM vbsegd into corresponding fields of table ibseg
    WHERE  ausbk  =  ibkpf-bukrs
    AND    gjahr  =  ibkpf-gjahr
    AND    belnr  =  ibkpf-belnr.

  perform fill_itab.

ENDFORM.                    " VBSEGD_READ_PROCESS
*&---------------------------------------------------------------------*
*&      Form  VBSET_READ_PROCESS
*&---------------------------------------------------------------------*
FORM vbset_read_process.
  CLEAR: ibkpf-buzei,  ibkpf-buzei1.                  " i´?e¸°i™”

  LOOP AT iline1 WHERE belnr = ibkpf-belnr.
    ibkpf-buzei  =  iline1-buzei.                    " e¨¼i?€i?€i??
    IF  ibkpf-buzei  >  ibkpf-buzei1.
      ibkpf-buzei1 =  ibkpf-buzei.
    ELSE.
      ibkpf-buzei1 =  ibkpf-buzei1.
    ENDIF.
  ENDLOOP.


  refresh ibseg.
  SELECT * FROM vbset
    WHERE  ausbk  =  ibkpf-bukrs
    AND    belnr  =  ibkpf-belnr
    AND    gjahr  =  ibkpf-gjahr.

    CLEAR:  ibseg.
    ibkpf-buzei1 =  ibkpf-buzei1 +  1.

    ibseg-saknr =  'TAXACCOUNT'.
    ibseg-buzei =  ibkpf-buzei1.
    ibseg-mwskz =  vbset-mwskz.
    ibseg-txjcd =  vbset-txjcd.

    CASE vbset-shkzg.
      WHEN  'S'.     ibseg-s_gumak = vbset-fwste.   " i°¨e³€
      WHEN  'H'.     ibseg-h_gumak = vbset-fwste.   " eŒ€e³€
    ENDCASE.

    ibseg-hkont = 'TAXACCOUNT'.
    ibseg-txt20 = 'Tax Account'.

* use tax payable
    if vbset-FWSTE = 0.
      ibseg-sgtxt = '***Tax Not Calculated***'.
    else.
      ibseg-h_gumak = vbset-fwste.
    endif.
    PERFORM        copy_to_iline1.                " Bodyi?? i“´e?¤.
  ENDSELECT.

  CLEAR  sy-subrc.                                  " i´?e¸°i™”

  LOOP AT iline3.                                  " i?¸e¸?e¸?i?¡i?´i??
    IF sy-subrc <> 0. EXIT. ENDIF.
    ibkpf-buzei1    =  ibkpf-buzei1 +  1.                 " seqi|?e°€
*    PERFORM        skat_process  USING iline3-hkont.   " e³?i??e³¼eª?
    PERFORM        copy_to_iline11.                 " Bodyi?? i“´e?¤
  ENDLOOP.

  REFRESH iline3.
  CLEAR   iline3.
ENDFORM.                    " VBSET_READ_PROCESS
*&---------------------------------------------------------------------*
*&      Form  get_account_info
*&---------------------------------------------------------------------*
FORM get_account_info.

  CLEAR  ibseg-name1.                             " e³€i??i´?e¸°i™”
  case ibseg-koart.
    when 'A'.
      SELECT single txt50 FROM anla  INTO ibseg-name1
        WHERE  bukrs  =  p_bukrs
          AND  anln1  =  ibseg-anln1
          And  anln2  =  ibseg-anln2.
    when 'D'.
      SELECT single name1 FROM kna1 INTO ibseg-name1
        WHERE  kunnr  =  ibseg-kunnr.
    when 'K'.
      SELECT single name1 FROM lfa1 INTO ibseg-name1
        WHERE  lifnr  =  ibseg-lifnr.
  endcase.
ENDFORM.                    " get_account_info
*&---------------------------------------------------------------------*
*&      Form  ADCP_READ_PROCESS
*&---------------------------------------------------------------------*
FORM adcp_read_process USING    p_ibkpf_usnam.
  DATA: temp-persnumber  LIKE  usr21-persnumber,
        temp-addrnumber  LIKE  usr21-addrnumber.

  SELECT persnumber addrnumber FROM usr21
   INTO (temp-persnumber, temp-addrnumber)
    WHERE  bname  =  p_ibkpf_usnam.
    SELECT  department FROM  adcp INTO ibkpf-depart
      WHERE  persnumber  =  temp-persnumber
      AND    addrnumber  =  temp-addrnumber.
    ENDSELECT.
  ENDSELECT.

  CLEAR  sy-subrc.
ENDFORM.                    " ADCP_READ_PROCESS

*&---------------------------------------------------------------------*
*&      Form  JIBGYE_PROCESS
*&---------------------------------------------------------------------*
FORM jibgye_process.
  SORT  iline1 BY belnr hkont buzei.

  LOOP AT iline1.
    IF  sy-subrc  <>  0.  EXIT.  ENDIF.

    CLEAR  iline1-buzei.                          " i´?e¸°i™”(seq)

    MOVE-CORRESPONDING iline1 TO iline2.         " i…Œi?´e¸”i?´e?™
    MOVE-CORRESPONDING iline1 TO s1.

    AT NEW hkont.
      MOVE-CORRESPONDING s1   TO d1.              " i??i?œe³€i??i?€i??
    ENDAT.

    MOVE-CORRESPONDING d1     TO iline2.

    AT END OF hkont.
      SUM.                                        " i??
    ENDAT.

    COLLECT  iline2.                             " e²°e³¼e°’i?? i?€i??
  ENDLOOP.

  data: idx type i.
  clear idx.
  loop at iline2.
    idx = idx + 1.
    iline2-buzei = idx.
    modify iline2 index sy-tabix.

    at end of belnr.
      clear idx.
    endat.

  endloop.


ENDFORM.                    " JIBGYE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  copy_to_iline11
*&---------------------------------------------------------------------*
FORM copy_to_iline11.
  CLEAR iline1.

  MOVE: iline3-belnr   TO  iline1-belnr,                " i??i‘œe²?i?¸
        iline3-budat   TO  iline1-budat,                " i??e¸°i?¼
        iline3-usnam   TO  iline1-usnam,                " i??i??i??i?´
        iline3-depart  TO  iline1-depart.       " e¶€i?œeª…

  MOVE: ibkpf-buzei1    TO  iline1-buzei,                " seq
        iline3-hkont  TO  iline1-hkont,                " G/L e³?i??i½”
        ibseg-txt20    TO  iline1-txt20,                " e³?i??e³¼eª?
        iline3-accnt  TO  iline1-accnt,                " e³?e°?
        iline3-ktext  TO  iline1-ktext,
        iline3-name1  TO  iline1-name1,                " e±°e??i²?eª…
        iline3-kostl  TO  iline1-kostl,                " i†?i?µi?¼i?°
        iline3-sgtxt  TO  iline1-sgtxt,                " TEXT
        iline3-gsber  TO  iline1-gsber,                " i??i?…i??i?­
        iline3-gtext  TO  iline1-gtext,                " i??i?…i??i?­
        iline3-mwskz  TO  iline1-mwskz,                " i?¸e¸?i½”e“œ
        iline3-txjcd  TO  iline1-txjcd,                " i?¸e?´i??i?…
        iline3-zuonr  TO  iline1-zuonr,                " e°°e¶€
        iline3-s_gumak    TO   iline1-s_gumak,         " i°¨e³€
        iline3-h_gumak    TO   iline1-h_gumak.         " eŒ€e³€

  SELECT name FROM j_1bbranch INTO ibseg-jrtxt             " i?¸e?´i??
*    WHERE  kalsm  =  'TAXKR'                            " i?? i?½e?”e?¤
    where    branch  =  iline3-txjcd.
  ENDSELECT.
  iline1-jrtxt  =   ibseg-jrtxt.
  CLEAR  sy-subrc.

  APPEND  iline1.                                     " e·¸e??e?”i??e³?.

ENDFORM.                    " copy_to_iline11
*&---------------------------------------------------------------------*
*&      Form  get_vtbfha
*&---------------------------------------------------------------------*
form get_vtbfha.
  SELECT SINGLE rpzahl
      INTO temp-rpzahl
      FROM vtbfhapo
     WHERE bukrs EQ ibkpf-bukrs
       AND gjahr EQ ibkpf-gjahr
       AND belnr EQ ibkpf-belnr.
  SELECT single name1 FROM BP000 INTO ibseg-name1
     WHERE  partnr  =  temp-rpzahl.

endform.                    " get_vtbfha
*&---------------------------------------------------------------------*
*&      Form  get_vdarl
*&---------------------------------------------------------------------*
form get_vdarl.
  SELECT SINGLE ranl
      INTO temp-ranl
      FROM vdbeki
     WHERE bukrs  EQ ibkpf-bukrs
       AND dgjahr EQ ibkpf-gjahr
       AND rvzblg EQ ibkpf-belnr.

  CHECK sy-subrc EQ 0.

*e±°e??i²?, e¸?iœµi??i’?iœ?i??...
  SELECT SINGLE  rdarnehm
    INTO temp-rdarnehm
    FROM vdarl
   WHERE bukrs EQ ibkpf-bukrs
     AND ranl  EQ temp-ranl.

endform.                    " get_vdarl
*&---------------------------------------------------------------------*
*&      Form  fill_itab
*&---------------------------------------------------------------------*
FORM fill_itab.
  loop at ibseg.

    IF NOT ibseg-kunnr IS INITIAL.
      ibseg-accnt = ibseg-kunnr.  ibseg-koart = 'D'.
    ELSEIF NOT ibseg-lifnr IS INITIAL.
      ibseg-accnt = ibseg-lifnr.  ibseg-koart = 'K'.
    ELSEIF NOT ibseg-anln1 IS INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
           EXPORTING
                input  = ibseg-anln1
           IMPORTING
                output = ibseg-accnt.

      ibseg-koart = 'A'.
    ENDIF.

    CASE ibseg-shkzg.
      WHEN  'S'.  ibseg-s_gumak = ibseg-dmbtr.       " debit
      WHEN  'H'.  ibseg-h_gumak = ibseg-dmbtr.       " credit
    ENDCASE.

    if ibseg-hkont is initial.
      ibseg-hkont = ibseg-saknr.
    endif.

    PERFORM          get_account_info.

    PERFORM          csks_read_process.
    PERFORM          tgsbt_process.
    PERFORM          skat_process.
    PERFORM          copy_to_iline1.

    IF  ibseg-wmwst > 0.
      CLEAR: ibseg-s_gumak, ibseg-h_gumak.
      CASE vbsegd-shkzg.
        WHEN  'S'.  ibseg-h_gumak = ibseg-wmwst.
        WHEN  'H'.  ibseg-s_gumak = ibseg-wmwst.
      ENDCASE.
      ibseg-hkont =    'TAXACCOUNT'.                   " Tax Accnt
      MOVE-CORRESPONDING ibkpf TO iline3.            " i?¤e?”
      MOVE-CORRESPONDING ibseg TO iline3.            " e?°i?´i?€
      APPEND iline3.
    ENDIF.
  ENDloop.

ENDFORM.                    " fill_itab
*&---------------------------------------------------------------------*
*&      Form  bseg_read
*&---------------------------------------------------------------------*
FORM bseg_read.

  loop at ibkpf.
    IF ibkpf-usnam  =  'RFC-USER'.      " RFC
      ibkpf-usnam  =  ibkpf-xblnr.
      clear ibkpf-xblnr.
    ENDIF.

*   read personnel info
    PERFORM  adcp_read_process USING  ibkpf-usnam.

    CASE   ibkpf-bstat.
      WHEN space OR 'A' OR 'B' OR 'D' OR 'S'.
        PERFORM   bseg_read_process.

      WHEN OTHERS.
        PERFORM   vbsegsk_read_process.
        PERFORM   vbsega_read_process.
        PERFORM   vbsegs_read_process.
        PERFORM   vbsegd_read_process.
        PERFORM   vbset_read_process.
    ENDCASE.

  ENDloop.

ENDFORM.                    " bseg_read
