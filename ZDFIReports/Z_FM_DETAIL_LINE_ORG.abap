REPORT zfifmr02a  NO STANDARD PAGE HEADING  MESSAGE-ID zmfi
                                            LINE-COUNT 65
                                            LINE-SIZE 206.
*----------------------------------------------------------------------*
*
* BPEJ has two budget: FM budget - WRTTP = 43, 46
*                      IM budget - WRTTP = 47
*
* If you use budget release, WRTTP = 46 is available budget
*
* to have monthly report
*  1. use BLDAT - bpbk, FMIFIHD
*  2. use fund (monthly)
*
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: bkpf, bseg, vbsegs, usr02,
        bpej,                "개별항목  연간값
        bpbk,
        t001,
        TKA02,
        fmfctr,              "FIFM: 예산관리센터 마스터
        fmfpo,               "FIFM: 약정사항 항목

* fm item, openitem, total
        FMIFIIT,             "FM Line
        FMIFIHD,             "FM Header(FI)
        fmioi,               "openitem
        fmit.                "Totals Table for Funds Management

*        zfivd002.                   "BSIS+BKPF

*----------------------------------------------------------------------*
* DATA
*----------------------------------------------------------------------*
* Internal table
DATA: BEGIN OF ILINE OCCURS 0,
        fictr     LIKE fmfctr-fictr,           "펀드센타
        bezeich   LIKE fmfctrt-bezeich,        "이름
        fipos     LIKE fmfpo-fipos,            "약정항목
        bezeichh  LIKE fmfpot-bezeich,         "이름
        geber     LIKE bpej-geber,             "펀드
        wtkbud    LIKE bpej-wtjhr,             "기초금액
        wtkbn0    LIKE bpej-wtjhr,             "추가금액
        wtjhrc    LIKE bpej-wtjhr,             "반환금액
        wtjhrd    LIKE bpej-wtjhr,             "Transfer

        wtjhre    LIKE bpej-wtjhr,             "수정예산
        trbtra    LIKE fmifiit-trbtr,          "실적
        trbtrb    LIKE fmifiit-trbtr,          "실적계
        trbtrc    LIKE fmifiit-trbtr,          "잔액
      END OF ILINE.

DATA: BEGIN OF ILINE_add OCCURS 0,
        fictr     LIKE fmfctr-fictr,           "예산관리센터
        bezeich   LIKE fmfctrt-bezeich,        "이름
        fipos     LIKE fmfpo-fipos,            "약정항목
        bezeichh  LIKE fmfpot-bezeich,         "이름
        geber     LIKE bpej-geber,             "펀드
        wtjhr     LIKE bpej-wtjhr,             "금액
        sgtext    LIKE bpbk-sgtext,            "텍스트
      END OF ILINE_add.

DATA: BEGIN OF itab_rtn OCCURS 0,
        fictr     LIKE fmfctr-fictr,           "예산관리센터
        bezeich   LIKE fmfctrt-bezeich,        "이름
        fipos     LIKE fmfpo-fipos,            "약정항목
        bezeichh  LIKE fmfpot-bezeich,         "이름
        geber     LIKE bpej-geber,             "펀드
        wtjhr     LIKE bpej-wtjhr,             "금액
        sgtext    LIKE bpbk-sgtext,            "텍스트
      END OF itab_rtn.

DATA: BEGIN OF ILINE_tra OCCURS 0,
        fictrs     LIKE fmfctr-fictr,           "센더펀드센터
        bezeichs   LIKE fmfctrt-bezeich,        "센더이름
        fictrr     LIKE fmfctr-fictr,           "리시버펀드센터
        bezeichr   LIKE fmfctrt-bezeich,        "리시버이름
        fiposs     LIKE fmfpo-fipos,            "약정항목
        bezeichhs  LIKE fmfpot-bezeich,         "이름
        fiposr     LIKE fmfpo-fipos,            "약정항목
        bezeichhr  LIKE fmfpot-bezeich,         "이름
        gebers     LIKE bpej-geber,             "센더펀드
        geberr     LIKE bpej-geber,             "리시버펀드
        wtjhr      LIKE bpej-wtjhr,             "금액
        sgtext     LIKE bpbk-sgtext,            "텍스트
      END OF ILINE_tra.

DATA: BEGIN OF ILINE_fi OCCURS 0,
        kostl    LIKE bsis-kostl,
        usnam    LIKE bsis-kostl,
        hkont    LIKE bsis-hkont,
        wrbtr    LIKE bsis-wrbtr,
        belnr    LIKE bsis-belnr,
        sgtxt    LIKE bsis-sgtxt,
        ktext    LIKE cskt-ktext,
        usnat    LIKE cskt-ktext,
        txt20    LIKE skat-txt20,
        BSTAT    like bkpf-bstat,
      END OF ILINE_fi.

DATA: BEGIN OF it_fmifiit OCCURS 0,
        fictr     LIKE fmfctr-fictr,           "펀드센타
        fipos     LIKE fmfpo-fipos,            "약정항목
        geber     LIKE bpej-geber,             "펀드
        FMBELNR   like fmifiit-FMBELNR,
        knbelnr   LIKE fmifiit-knbelnr,        "FI전표번호
        trbtr     LIKE fmifiit-trbtr,          "실적금액
        wrttp     LIKE fmifiit-wrttp,          "값유형
        HKONT     like fmifiit-HKONT,
        btart     like v_fmifi-btart,          "amount type
        bezeich   LIKE fmfctrt-bezeich,        "펀드센타이름
        bezeichh  LIKE fmfpot-bezeich,         "약정항목이름
      END OF it_fmifiit.

DATA: BEGIN OF it_bpej OCCURS 0,
        belnr    LIKE bpej-belnr,
        buzei    LIKE bpej-buzei,
        wtjhr    LIKE bpej-wtjhr,
        objnr    LIKE bpej-objnr,
        posit    LIKE bpej-posit,
        geber    LIKE bpej-geber,
        sgtext   LIKE bpbk-sgtext,
        vorga    LIKE bpej-vorga,
        fictr    LIKE fmfctr-fictr,
        bezeich  LIKE fmfctrt-bezeich,
        fipos    LIKE fmfpo-fipos,
        bezeichh LIKE fmfpot-bezeich,
      END OF it_bpej.

DATA: it_temp  LIKE it_bpej OCCURS 0 WITH HEADER LINE,
      it_temp2 LIKE it_bpej OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF it_fmfctr OCCURS 0,
        fictr   LIKE fmfctr-fictr,
        bezeich LIKE fmfctrt-bezeich,
        bossid  LIKE fmfctr-bossid,
      END OF it_fmfctr.

DATA: BEGIN OF it_fmfpo OCCURS 0,
        fipos    LIKE fmfpo-fipos,
        bezeichh LIKE fmfpot-bezeich,
        posit    LIKE fmfpo-posit,
      END OF it_fmfpo.

DATA: BEGIN OF it_belnr OCCURS 0,
        belnr LIKE bpej-belnr,
      END OF it_belnr.

DATA: BEGIN OF it_rewrite OCCURS 0,
        belnr LIKE fmifiit-knbelnr,
      END OF it_rewrite.

DATA: BEGIN OF it_cskt OCCURS 0,
        kostl LIKE cskt-kostl,
        ktext LIKE cskt-ktext,
      END OF it_cskt.

DATA: BEGIN OF it_skat OCCURS 0,
        saknr LIKE skat-saknr,
        txt20 LIKE skat-txt20,
      END OF it_skat.

* Excel down
DATA: BEGIN OF it_down OCCURS 0,
        fictr(16)     TYPE c,           "펀드센타
        bezeich(20)   TYPE c,           "이름
        fipos(14)     TYPE c,           "약정항목
        bezeichh(20)  TYPE c,           "이름
        geber(10)     TYPE c,           "펀드
        wtkbud(21)    TYPE c,           "기초금액
        wtkbn0(21)    TYPE c,           "추가금액
        wtjhrd(21)    TYPE c,           "전용금액
        wtjhre(21)    TYPE c,           "수정금액
        trbtra(16)    TYPE c,           "실적금액
        trbtrb(16)    TYPE c,           "실적계금액
        trbtrc(16)    TYPE c,           "잔액
      END OF it_down.

DATA: BEGIN OF it_down_add OCCURS 0,
        fictr(16)     TYPE c,         "예산관리센터
        bezeich(20)   TYPE c,         "이름
        fipos(14)     TYPE c,         "약정항목
        bezeichh(20)  TYPE c,         "이름
        geber(10)     TYPE c,         "펀드
        wtjhr(21)     TYPE c,         "금액
        sgtext(50)    TYPE c,         "텍스트
      END OF it_down_add.

DATA: BEGIN OF it_down_sub OCCURS 0,
        fictr(16)     TYPE c,        "예산관리센터
        bezeich(20)   TYPE c,        "이름
        fipos(14)     TYPE c,        "약정항목
        bezeichh(20)  TYPE c,        "이름
        geber(10)     TYPE c,        "펀드
        wtjhr(21)     TYPE c,        "금액
        sgtext(50)    TYPE c,        "텍스트
      END OF it_down_sub.

DATA: BEGIN OF it_down_tra OCCURS 0,
        fictrs(16)     TYPE c,        "센더펀드센터
        bezeichs(20)   TYPE c,        "센더이름
        fictrr(14)     TYPE c,        "리시버펀드센터
        bezeichr(20)   TYPE c,        "리시버이름
        fiposs(16)     TYPE c,        "약정항목
        bezeichhs(20)  TYPE c,        "이름
        fiposr(14)     TYPE c,        "약정항목
        bezeichhr(20)  TYPE c,        "이름
        gebers(10)     TYPE c,        "센더펀드
        geberr(10)     TYPE c,        "리시버펀드
        wtjhr(21)      TYPE c,        "금액
        sgtext(50)     TYPE c,        "텍스트
      END OF it_down_tra.

DATA: BEGIN OF it_down_fi OCCURS 0,
        kostl(10)    TYPE c,
        ktext(20)    TYPE c,
        usnam(10)    TYPE c,
        usnat(20)    TYPE c,
        hkont(11)    TYPE c,
        txt20(20)    TYPE c,
        wrbtr(16)    TYPE c,
        belnr(11)    TYPE c,
        sgtxt(50)    TYPE c,
      END OF it_down_fi.

* Work Area
DATA: wa_temp LIKE it_temp.

* Variables
DATA: v_mod       TYPE i,
      v_cnt       TYPE i,
      v_vorga     LIKE bpej-vorga,
      v_vorgab    LIKE bpej-vorga,
      v_field(20) TYPE c,
      v_off       TYPE i,
      v_sortf(10) TYPE c,
      sum_wtkbud  LIKE bpej-wtjhr,
      sum_wtkbn0  LIKE bpej-wtjhr,
      sum_wtjhrc  LIKE bpej-wtjhr,
      sum_wtjhrd  LIKE bpej-wtjhr,
      sum_wtjhre  LIKE bpej-wtjhr,
      sum_trbtra  LIKE fmifiit-trbtr,
      sum_trbtrb  LIKE fmifiit-trbtr,
      sum_trbtrc  LIKE fmifiit-trbtr.

data: p_fikrs  like FMIFIIT-fikrs,
      gCR    like t001-WAERS,
      g_history type c.
RANGES:R_cmmt for FMIFIIT-WRTTP,
       r_acct for FMIFIIT-WRTTP,
       P_DATUM FOR SY-DATUM.


*** start include ******************************************************
INCLUDE <ICON>.


*----------------------------------------------------------------------*
* SELECTION SCREEN ( SELECT-OPTIONS & PARAMETERS )
*----------------------------------------------------------------------*
parameters: p_bukrs  like t001-bukrs memory id BUK,
            p_gjahr  like bpej-gjahr memory id GJR.
SELECT-OPTIONS:
            p_monat  for BKPF-MONAT.

SELECTION-SCREEN BEGIN OF BLOCK sb WITH FRAME TITLE text-s10.
SELECT-OPTIONS:
                s_geber  FOR bpej-geber   memory id FIC,
                s_fictr  FOR fmfctr-fictr memory id FIS,
                s_fipos  FOR fmfpo-fipos,
                s_bossid FOR USR02-BNAME. "Manager
SELECTION-SCREEN END OF BLOCK sb.
parameters: p_t type n.
parameters: p_s type n default '1'.
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.


*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

  select single * from t001 where bukrs = p_bukrs.
  check sy-subrc = 0.
  p_fikrs = t001-fikrs.
  gCR   = t001-WAERS.
  select single * from tka02 where bukrs = p_bukrs.

  perform init_variable.

* Select Data
  PERFORM select_data.

* Modify Internal Table
  PERFORM modify_it_table.

* Message
  DESCRIBE TABLE ILINE LINES v_cnt.
  IF v_cnt = 0.
    MESSAGE s006.
    EXIT.
  ENDIF.

*----------------------------------------------------------------------*
* END OF SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.

  SORT ILINE BY fictr fipos geber.
  PERFORM write_item.

*----------------------------------------------------------------------*
* TOP OF PAGE
*----------------------------------------------------------------------*
TOP-OF-PAGE.
  PERFORM write_header.

TOP-OF-PAGE DURING LINE-SELECTION.

  IF g_history = space.
    PERFORM write_header.
  ELSE.
    PERFORM rewrite_header.
  ENDIF.

*----------------------------------------------------------------------*
* AT LINE SELECTION
*----------------------------------------------------------------------*
AT LINE-SELECTION.

  GET CURSOR FIELD v_field.
  g_history = 'X'.
  CASE v_field.
* supplement / transfer / return /
    WHEN 'ILINE-wtkbn0'.
      PERFORM rewrite_wtkbn0.
    when 'ILINE-WTJHRC'.
      PERFORM rewrite_wtjhrc.
    when 'ILINE-WTJHRD'.
      PERFORM rewrite_tra.

* actual
    WHEN 'ILINE-TRBTRB' or 'ILINE-TRBTRA'.
      PERFORM rewrite_trbtrb.
* fi doc
    when 'ILINE_FI-BELNR'.
      perform go_fi_doc.
    when others.
      g_history = space.
  ENDCASE.

************************************************************************
***  (SHIFT+PF1) Execute Download
************************************************************************
AT PF13.
  PERFORM download_data.
************************************************************************
***  (SHIFT+PF4) Sort
************************************************************************
AT PF14.
  PERFORM sort_data.
  sy-lsind = sy-lsind - 1.

*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_data.

* Budget
* monthly budget is entered into BPPE(month), BPJA, BPEP(line)
* yearly budget is in the 1st field:WTP01
* KBFR - release
  SELECT l~belnr l~buzei l~wtjhr l~objnr l~posit l~geber
         h~sgtext l~vorga
                     INTO TABLE it_bpej
                     FROM bpej as l inner join bpbk as h
                       ON l~belnr  = h~belnr
                     WHERE l~gjahr = p_gjahr
                       and h~bldat in p_datum
                       and l~WRTTP = '43'      "no budget release
                       AND l~posit > 'FP000001'
                       AND l~vorga IN ('KBUD', 'KBN0', 'KBR0',
                                       'KBUE', 'KBUS').


* actual
  SELECT fistl fipex fonds FMBELNR knbelnr trbtr wrttp hkont btart
               INTO TABLE it_fmifiit
               FROM v_fmifi
               WHERE fikrs =  p_fikrs
               AND   gjahr =  p_gjahr
               and   bldat in p_datum
               AND   fonds IN s_geber
               AND   fistl IN s_fictr
               AND   fipex IN s_fipos
               AND   wrttp IN r_cmmt
               and   btart = '0100'.  "original
*master
* Commitment Item
  SELECT fipos bezeich posit
               INTO TABLE it_fmfpo
               FROM u_12439
               WHERE fikrs = p_fikrs
               AND   datbis >= sy-datum
               AND   spras = sy-langu.

* Funds center
  SELECT fictr bezeich bossid  INTO TABLE it_fmfctr
               FROM u_12410
               WHERE  fikrs = p_fikrs
               AND    datbis >= sy-datum
               AND    spras = sy-langu.

* 코스터센터(Text), G/L 계정(Text)
  PERFORM select_text.

ENDFORM.                    " SELECT_DATA
*&---------------------------------------------------------------------*
*&      Form  modify_it_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_it_table.

* 기초, 추가, 반환, 전송(Collect)
  PERFORM move_it_bpej.

* 실적, 실적계(Collect)
  PERFORM move_it_fmifiit.

ENDFORM.                    " modify_it_table
*&---------------------------------------------------------------------*
*&      Form  move_it_bpej
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM move_it_bpej.

  LOOP AT it_bpej.
    CLEAR: it_fmfpo, it_fmfctr.
* 약정항목, 이름
    READ TABLE it_fmfpo WITH KEY posit = it_bpej-posit.
    IF sy-subrc = 0.
      ILINE-fipos = it_bpej-fipos = it_fmfpo-fipos.
      ILINE-bezeichh = it_bpej-bezeichh = it_fmfpo-bezeichh.
    ENDIF.
* 펀드센터
    ILINE-fictr = it_bpej-fictr = it_bpej-objnr+6(16).
* 펀드센터(이름)
    READ TABLE it_fmfctr WITH KEY fictr = ILINE-fictr.
    IF sy-subrc = 0.
      ILINE-bezeich = it_bpej-bezeich = it_fmfctr-bezeich.
    ENDIF.
    MODIFY it_bpej.

* 조건(펀드, 펀드센터, 약정항목, 책임자)
    IF it_bpej-geber IN s_geber AND
       ILINE-fictr  IN s_fictr AND
       ILINE-fipos  IN s_fipos AND
       it_fmfctr-bossid IN s_bossid.

      MOVE: it_bpej-geber TO ILINE-geber.
      IF it_bpej-vorga = 'KBUD'.                    "기초
        MOVE: it_bpej-wtjhr TO ILINE-wtkbud.
      ELSEIF it_bpej-vorga = 'KBN0'.                "추가
        MOVE: it_bpej-wtjhr TO ILINE-wtkbn0.
        MOVE-CORRESPONDING it_bpej TO ILINE_add.  "Drill down용
        APPEND ILINE_add.  CLEAR ILINE_add.
      ELSEIF it_bpej-vorga = 'KBR0'.
        MOVE: it_bpej-wtjhr TO ILINE-wtjhrc.      "반환
        MOVE-CORRESPONDING it_bpej TO itab_rtn.  "Drill down용
        APPEND itab_rtn.  CLEAR itab_rtn.
      ELSEIF it_bpej-vorga = 'KBUE' OR               "전송
             it_bpej-vorga = 'KBUS'.
        MOVE: it_bpej-wtjhr TO ILINE-wtjhrd.
        MOVE-CORRESPONDING it_bpej TO it_temp.
        APPEND it_temp.  CLEAR it_temp.
      ENDIF.
      MOVE: it_bpej-geber TO ILINE-geber.
      COLLECT ILINE.  CLEAR ILINE.

*      DELETE it_bpej.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " move_it_bpej
*&---------------------------------------------------------------------*
*&      Form  move_it_fmifiit
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM move_it_fmifiit.


  LOOP AT it_fmifiit.
* 조건(책임자)
    CHECK it_fmfctr-bossid IN s_bossid.

    CLEAR: it_fmfpo, it_fmfctr.
* 약정항목(이름)
    READ TABLE it_fmfpo WITH KEY fipos = it_fmifiit-fipos.
    IF sy-subrc = 0.
      ILINE-bezeichh = it_fmfpo-bezeichh.
    ENDIF.
* 펀드센터(이름)
    READ TABLE it_fmfctr WITH KEY fictr = it_fmifiit-fictr.
    IF sy-subrc = 0.
      ILINE-bezeich = it_fmfctr-bezeich.
    ENDIF.

* actual (park doc, real)
    IF it_fmifiit-wrttp in r_cmmt.
      ILINE-trbtrb = it_fmifiit-trbtr * ( - 1 ).

      IF it_fmifiit-wrttp in r_acct.
        ILINE-trbtra = it_fmifiit-trbtr * ( - 1 ).
      ENDIF.
    ENDIF.

    MOVE: it_fmifiit-fictr TO ILINE-fictr,
          it_fmifiit-fipos TO ILINE-fipos,
          it_fmifiit-geber TO ILINE-geber.
    COLLECT ILINE.  CLEAR ILINE.
  ENDLOOP.

ENDFORM.                    " move_it_fmifiit
*&---------------------------------------------------------------------*
*&      Form  rewrite_wtkbn0
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM rewrite_wtkbn0.

  SORT ILINE_add BY fictr geber.
  LOOP AT ILINE_add WHERE fictr = ILINE-fictr
                       AND   fipos = ILINE-fipos
                       AND   geber = ILINE-geber.
    WRITE: /'|' NO-GAP, AT (16) ILINE_add-fictr NO-GAP,
            '|' NO-GAP, AT (20) ILINE_add-bezeich NO-GAP,
            '|' NO-GAP, AT (14) ILINE_add-fipos NO-GAP,
            '|' NO-GAP, AT (20) ILINE_add-bezeichh NO-GAP,
            '|' NO-GAP, AT (10) ILINE_add-geber NO-GAP,
           '|' NO-GAP, AT (21) ILINE_add-wtjhr NO-GAP CURRENCY gCR,
            '|' NO-GAP, AT (50) ILINE_add-sgtext NO-GAP,
            '|'.
    WRITE:/001(159) sy-uline.
  ENDLOOP.

ENDFORM.                    " rewrite_wtkbn0
*&---------------------------------------------------------------------*
*&      Form  rewrite_trbtrb
*&---------------------------------------------------------------------*
*       actual
*----------------------------------------------------------------------*
FORM rewrite_trbtrb.
  CLEAR: ILINE_fi, it_rewrite.
  REFRESH: ILINE_fi, it_rewrite.


*Amount type : BTART
*-----------
*The document type classifies document amounts. If you add all the
*amounts for a document, you arrive at the "current value" of the
*document.
*
*Example: Purchase order 4711
*Amount type Amount
*1.PO created with value 100 Original -100
*2.Reduced by invoice for 20 Reduction + 20
*3.Carried forward in FYC Old year + 80
* New year - 80
*4.Reduced by invoice for 20 Reduction + 20
*------------------------------------------
*Current value - 60
*
* 100 - original
* 150 - change
* 200 - reduce
* 250 - paid
* 300 - c/f


  LOOP AT it_fmifiit WHERE geber = ILINE-geber
                     AND   fictr = ILINE-fictr
                     AND   fipos = ILINE-fipos
                     and   btart = '0100'.

    if v_field = 'ILINE-TRBTRA'. "actual only
      if it_fmifiit-wrttp = '60'. "park document
        continue.
      endif.
    endif.

    select single usnam bstat
           into (iline_fi-usnam, iline_fi-bstat)
           from bkpf
           WHERE belnr = it_fmifiit-knbelnr
             AND gjahr = p_gjahr
             and bukrs = p_bukrs
             and STBLG = space.

    check sy-subrc = 0.
    if it_fmifiit-wrttp = '60'. "park document
      select * from vbsegs
           WHERE belnr   = it_fmifiit-knbelnr
             AND gjahr   = p_gjahr
             and bukrs   = p_bukrs.
        if vbsegs-saknr  = it_fmifiit-hkont.
          iline_fi-kostl = vbsegs-kostl.
          iline_fi-belnr = it_fmifiit-knbelnr.
          iline_fi-hkont = vbsegs-SAKNR.
          iline_fi-wrbtr = vbsegs-wrbtr.
          iline_fi-sgtxt = vbsegs-sgtxt.
          append iline_fi.
        endif.
      endselect.
    else.
      select * from bseg
           WHERE belnr   = it_fmifiit-knbelnr
             AND gjahr   = p_gjahr
             and bukrs   = p_bukrs.
        if bseg-hkont = it_fmifiit-hkont.
          iline_fi-kostl = bseg-kostl.
          iline_fi-belnr = it_fmifiit-knbelnr.
          iline_fi-hkont = bseg-hkont.
          iline_fi-wrbtr = bseg-wrbtr.
          iline_fi-sgtxt = bseg-sgtxt.
          append iline_fi.
        endif.
      endselect.
    endif.
  ENDLOOP.

  CHECK sy-subrc = 0.

* Rewrite
  PERFORM rewrite_fi.

ENDFORM.                    " rewrite_trbtrb
*&---------------------------------------------------------------------*
*&      Form  rewrite_tra
*&---------------------------------------------------------------------*
*       transfer
*----------------------------------------------------------------------*
FORM rewrite_tra.
  perFORM get_transfer_info.


  SORT ILINE_tra BY fictrs gebers.
  LOOP AT ILINE_tra.
    WRITE: /'|' NO-GAP, AT (7) ILINE_tra-fictrs NO-GAP,
            '|' NO-GAP, AT (20) ILINE_tra-bezeichs NO-GAP,
            '|' NO-GAP, AT (7) ILINE_tra-fiposs NO-GAP,
            '|' NO-GAP, AT (20) ILINE_tra-bezeichhs NO-GAP,
            '|' NO-GAP, AT (7) ILINE_tra-fictrr NO-GAP,
            '|' NO-GAP, AT (20) ILINE_tra-bezeichr NO-GAP,
            '|' NO-GAP, AT (7) ILINE_tra-fiposr NO-GAP,
            '|' NO-GAP, AT (20) ILINE_tra-bezeichhr NO-GAP,
            '|' NO-GAP, AT (10) ILINE_tra-gebers NO-GAP,
            '|' NO-GAP, AT (10) ILINE_tra-geberr NO-GAP,
           '|' NO-GAP, AT (16) ILINE_tra-wtjhr NO-GAP CURRENCY gCR,
            '|' NO-GAP, AT (40) ILINE_tra-sgtext NO-GAP,
            '|'.
    WRITE:/001(244) sy-uline.
  ENDLOOP.

ENDFORM.                    " rewrite_tra
*&---------------------------------------------------------------------*
FORM get_transfer_info.

  CLEAR: it_temp2.  REFRESH it_temp2.

  LOOP AT it_temp WHERE fictr = ILINE-fictr
                  AND   fipos = ILINE-fipos
                  AND   geber = ILINE-geber.
    MOVE-CORRESPONDING it_temp TO it_temp2.
    APPEND it_temp2.  CLEAR it_temp2.
    MOVE: it_temp-belnr TO it_belnr-belnr.
    COLLECT it_belnr.  CLEAR it_belnr.
  ENDLOOP.

* 없는 부분 추가
  LOOP AT it_belnr.
    READ TABLE it_bpej WITH KEY belnr = it_belnr-belnr.
    CHECK sy-subrc = 0.
    MOVE-CORRESPONDING it_bpej TO it_temp2.
    APPEND it_temp2.  CLEAR it_temp2.
  ENDLOOP.

* 센더, 리시버로 구분
  SORT it_temp2 BY belnr buzei.
  LOOP AT it_temp2.
    v_mod = sy-tabix MOD 2.
    IF v_mod = 1.
      MOVE: it_temp2 TO wa_temp.
    ELSEIF v_mod = 0.
      PERFORM move_sen_tra.
      PERFORM move_rec_tra.
      APPEND ILINE_tra.  CLEAR ILINE_tra.
      CLEAR wa_temp.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " rewrite_wtjhrd
*&---------------------------------------------------------------------*
*&      Form  move_sen_tra
*&---------------------------------------------------------------------*
FORM move_sen_tra.
* sender
  MOVE: wa_temp-fictr   TO ILINE_tra-fictrs,
        wa_temp-bezeich  TO ILINE_tra-bezeichs,
        wa_temp-fipos    TO ILINE_tra-fiposs,
        wa_temp-bezeichh TO ILINE_tra-bezeichhs,
        wa_temp-geber    TO ILINE_tra-gebers,
        wa_temp-sgtext   TO ILINE_tra-sgtext.

ENDFORM.                    " move_sen_tra
*&---------------------------------------------------------------------*
*&      Form  move_rec_tra
*&---------------------------------------------------------------------*
FORM move_rec_tra.

  MOVE:  it_temp2-fictr    TO ILINE_tra-fictrr,
         it_temp2-bezeich  TO ILINE_tra-bezeichr,
         it_temp2-fipos    TO ILINE_tra-fiposr,
         it_temp2-bezeichh TO ILINE_tra-bezeichhr,
         it_temp2-geber    TO ILINE_tra-geberr,
         it_temp2-wtjhr    TO ILINE_tra-wtjhr.

ENDFORM.                    " move_rec_tra
*&---------------------------------------------------------------------*
*&      Form  rewrite_fi
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM rewrite_fi.

  SORT ILINE_fi BY kostl usnam hkont.
  LOOP AT ILINE_fi.
    PERFORM read_text.
    WRITE: /'|' NO-GAP, AT (1)  ILINE_fi-bstat NO-GAP,
            '|' NO-GAP, AT (7)  ILINE_fi-kostl NO-GAP,
            '|' NO-GAP, AT (20) ILINE_fi-ktext NO-GAP,
            '|' NO-GAP, AT (10)  ILINE_fi-usnam NO-GAP,
            '|' NO-GAP, AT (20) ILINE_fi-usnat NO-GAP,
            '|' NO-GAP, AT (7) ILINE_fi-hkont NO-GAP,
            '|' NO-GAP, AT (20) ILINE_fi-txt20 NO-GAP,
            '|' NO-GAP, AT (16) ILINE_fi-wrbtr NO-GAP,
            '|' NO-GAP, AT (10) ILINE_fi-belnr NO-GAP,
            '|' NO-GAP, AT (50) ILINE_fi-sgtxt NO-GAP,
            '|'.
    hide: ILINE_FI-BELNR.
  ENDLOOP.
  clear: ILINE_FI-BELNR.
  WRITE:/001(170) sy-uline.
ENDFORM.                    " rewrite_fi
*&---------------------------------------------------------------------*
*&      Form  select_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_text.

  SELECT kostl ktext  INTO TABLE it_cskt
                      FROM cskt
                      WHERE spras = sy-langu
                      AND   kokrs = tka02-kokrs
                      AND   datbi >= sy-datum.

  SELECT saknr txt20  INTO TABLE it_skat
                      FROM skat
                      WHERE spras = sy-langu
                      AND   ktopl = t001-ktopl.

ENDFORM.                    " select_text
*&---------------------------------------------------------------------*
*&      Form  READ_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_text.

  READ TABLE it_cskt WITH KEY kostl = ILINE_fi-kostl.
  IF sy-subrc = 0.
    MOVE it_cskt-ktext TO ILINE_fi-ktext.
  ENDIF.
  READ TABLE it_cskt WITH KEY kostl = ILINE_fi-usnam.
  IF sy-subrc = 0.
    MOVE it_cskt-ktext TO ILINE_fi-usnat.
  ENDIF.
  READ TABLE it_skat WITH KEY saknr = ILINE_fi-hkont.
  IF sy-subrc = 0.
    MOVE it_skat-txt20 TO ILINE_fi-txt20.
  ENDIF.

ENDFORM.                    " READ_TEXT
*&---------------------------------------------------------------------*
*&      Form  SORT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sort_data.

  GET CURSOR FIELD v_field.
  v_sortf = v_field+9(7).
  SORT ILINE BY (v_sortf).

  PERFORM write_header.
  PERFORM write_item.

ENDFORM.                    " SORT_DATA
*&---------------------------------------------------------------------*
*&      Form  download_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM download_data.

  PERFORM down_list.
  PERFORM call_function.
*  CASE sy-pfkey.
*    WHEN 'LIST'.                             "Main
*    WHEN 'LIST2'.                            "추가
*      PERFORM down_list2.
*      PERFORM call_function2.
*    WHEN 'LIST3'.                            "반환
*      PERFORM down_list3.
*      PERFORM call_function3.
*    WHEN 'LIST4'.                            "전송
*      PERFORM down_list4.
*      PERFORM call_function4.
*    WHEN 'LIST5'.                            "실적계
*      PERFORM down_list5.
*      PERFORM call_function5.
*  ENDCASE.
*
ENDFORM.                    " download_data
*&---------------------------------------------------------------------*
*&      Form  rewrite_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM rewrite_header.

  CASE v_field.
* supplement / return /
    WHEN 'ILINE-wtkbn0' or 'ILINE-WTJHRC'.
      PERFORM rewrite_rev_hd.
* transfer
    when 'ILINE-WTJHRD'.
      PERFORM rewrite_transfer_hd.
* actual
    WHEN 'ILINE-TRBTRA' or 'ILINE-TRBTRB'.
      PERFORM rewrite_act_hd.
  ENDCASE.

ENDFORM.                    " rewrite_header
*&---------------------------------------------------------------------*
*&      Form  rewrite_wtkbn0_hd
*&---------------------------------------------------------------------*
FORM rewrite_rev_hd.

  WRITE: /001(159) 'Budget Revise History' CENTERED.
  SKIP 2.

  FORMAT COLOR 1 INTENSIFIED ON.
  WRITE: /001(159) sy-uline.
  WRITE: /'|' NO-GAP, AT (16) 'FnCtr' CENTERED NO-GAP,
          '|' NO-GAP, AT (20) 'Text' CENTERED NO-GAP,
          '|' NO-GAP, AT (14) 'CmmtItm' CENTERED NO-GAP,
          '|' NO-GAP, AT (20) 'Text' CENTERED NO-GAP,
          '|' NO-GAP, AT (10) 'Fund' CENTERED NO-GAP,
          '|' NO-GAP, AT (21) 'Amount' CENTERED NO-GAP,
          '|' NO-GAP, AT (50) 'Text' CENTERED NO-GAP,
          '|'.
  WRITE: /001(159) sy-uline.
  FORMAT COLOR OFF.

ENDFORM.                    " rewrite_wtkbn0_hd
*&---------------------------------------------------------------------*
*&      Form  rewrite_wtjhrd_hd
*&---------------------------------------------------------------------*
FORM rewrite_transfer_hd.

  WRITE: /001(159) 'Budget Transfer History' CENTERED.
  SKIP 2.

  FORMAT COLOR 1 INTENSIFIED ON.
  WRITE: /001(244) sy-uline.
  WRITE: /'|' NO-GAP, AT (7) 'Sender' CENTERED NO-GAP,
          '|' NO-GAP, AT (20) 'Text' CENTERED NO-GAP,
          '|' NO-GAP, AT (7) 'CmmtItm' CENTERED NO-GAP,
          '|' NO-GAP, AT (20) 'Text' CENTERED NO-GAP,
          '|' NO-GAP, AT (7) 'Receiver' CENTERED NO-GAP,
          '|' NO-GAP, AT (20) 'Text' CENTERED NO-GAP,
          '|' NO-GAP, AT (7) 'CmmtItm' CENTERED NO-GAP,
          '|' NO-GAP, AT (20) 'Text' CENTERED NO-GAP,
          '|' NO-GAP, AT (10) 'Fund' CENTERED NO-GAP,
          '|' NO-GAP, AT (10) 'Fund' CENTERED NO-GAP,
          '|' NO-GAP, AT (16) 'Amount' CENTERED NO-GAP,
          '|' NO-GAP, AT (40) 'Text' CENTERED NO-GAP,
          '|'.
  WRITE: /001(244) sy-uline.
  FORMAT COLOR OFF.

ENDFORM.                    " rewrite_wtjhrd_hd
*&---------------------------------------------------------------------*
*&      Form  rewrite_wtkbn0_hd
*&---------------------------------------------------------------------*
FORM rewrite_act_hd.

  WRITE: /001(170) 'Budget Actual Use History' CENTERED.
  SKIP 2.

  FORMAT COLOR 1 INTENSIFIED ON.
  WRITE: /001(170) sy-uline.
  WRITE: /'|' NO-GAP, AT (1) 'P' CENTERED NO-GAP,
          '|' NO-GAP, AT (7) 'FnCtr' CENTERED NO-GAP,
          '|' NO-GAP, AT (20) 'FnCtrName' CENTERED NO-GAP,
          '|' NO-GAP, AT (10) 'UserID' CENTERED NO-GAP,
          '|' NO-GAP, AT (20) 'UserName' CENTERED NO-GAP,
          '|' NO-GAP, AT (7) 'CmmtItm' CENTERED NO-GAP,
          '|' NO-GAP, AT (20) 'CmmtItmName' CENTERED NO-GAP,
          '|' NO-GAP, AT (16) 'Amount' CENTERED NO-GAP,
          '|' NO-GAP, AT (10) 'Doc No.' CENTERED NO-GAP,
          '|' NO-GAP, AT (50) 'Text' CENTERED NO-GAP,
          '|'.
  WRITE: /001(170) sy-uline.
  FORMAT COLOR OFF.

ENDFORM.                    " rewrite_wtkbn0_hd
*&---------------------------------------------------------------------*
*&      Form  down_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM down_list.

  CLEAR it_down.  REFRESH it_down.

  it_down-fictr = 'FnCtr'.
  it_down-bezeich = 'Text'.
  it_down-fipos = 'CmmtItem'.
  it_down-bezeichh = 'Text'.
  it_down-geber = 'Fund'.
  it_down-wtkbud = 'InitBal'.
  it_down-wtkbn0 = 'Supplement'.
  it_down-wtjhrd = 'Transfer'.
  it_down-wtjhre = 'CurBudget'.
  it_down-trbtra = 'Actual'.
  it_down-trbtrb = 'ActSum'.
  it_down-trbtrc = 'Balance'.
  APPEND it_down.  CLEAR it_down.

  LOOP AT ILINE.
    it_down-fictr = ILINE-fictr.
    it_down-bezeich = ILINE-bezeich.
    it_down-fipos = ILINE-fipos.
    it_down-bezeichh = ILINE-bezeichh.
    it_down-geber = ILINE-geber.
    it_down-wtkbud = ILINE-wtkbud.
    it_down-wtkbn0 = ILINE-wtkbn0.
    it_down-wtjhrd = ILINE-wtjhrd.
    it_down-wtjhre = ILINE-wtjhre.
    it_down-trbtra = ILINE-trbtra.
    it_down-trbtrb = ILINE-trbtrb.
    it_down-trbtrc = ILINE-trbtrc.
    APPEND it_down.  CLEAR it_down.
  ENDLOOP.

ENDFORM.                    " down_list
*&---------------------------------------------------------------------*
*FORM down_list2.
*
*  CLEAR it_down_add.  REFRESH it_down_add.
*
*  it_down_add-fictr = '펀드센타'.
*  it_down_add-bezeich = 'Text'.
*  it_down_add-fipos = '약정항목'.
*  it_down_add-bezeichh = 'Text'.
*  it_down_add-geber = '펀 드'.
*  it_down_add-wtjhr = '금   액'.
*  it_down_add-sgtext = 'Text'.
*  APPEND it_down_add. CLEAR it_down_add.
*
*  LOOP AT ILINE_add.
*    MOVE-CORRESPONDING ILINE_add TO it_down_add.
*    it_down_add-wtjhr = ILINE_add-wtjhr * 100.
*    APPEND it_down_add.  CLEAR it_down_add.
*  ENDLOOP.
*
*
*ENDFORM.                    " down_list2
*
*FORM down_list3.
*
*  CLEAR it_down_sub.  REFRESH it_down_sub.
*
*  it_down_add-fictr = '펀드센타'.
*  it_down_add-bezeich = 'Text'.
*  it_down_add-fipos = '약정항목'.
*  it_down_add-bezeichh = 'Text'.
*  it_down_add-geber = '펀 드'.
*  it_down_add-wtjhr = '금   액'.
*  it_down_add-sgtext = 'Text'.
*  APPEND it_down_sub. CLEAR it_down_sub.
*
*  LOOP AT itab_rtn.
*    MOVE-CORRESPONDING itab_rtn TO it_down_sub.
*    it_down_sub-wtjhr = itab_rtn-wtjhr * 100.
*    APPEND it_down_sub.  CLEAR it_down_sub.
*  ENDLOOP.
*
*ENDFORM.                    " down_list3
*
*FORM down_list4.
*
*  CLEAR it_down_tra.  REFRESH it_down_tra.
*
*  it_down_tra-fictrs = '센더펀드센타'.
*  it_down_tra-bezeichs = 'Text'.
*  it_down_tra-fictrr = '센더약정항목'.
*  it_down_tra-bezeichr = 'Text'.
*  it_down_tra-fiposs = '리시버펀드센타'.
*  it_down_tra-bezeichhs = 'Text'.
*  it_down_tra-fiposr = '리시버약정항목'.
*  it_down_tra-bezeichhr = 'Text'.
*  it_down_tra-gebers = '센더연월'.
*  it_down_tra-geberr = '리시버연월'.
*  it_down_tra-wtjhr = '금   액'.
*  it_down_tra-sgtext = 'Text'.
*  APPEND it_down_tra.  CLEAR it_down_tra.
*
*  LOOP AT ILINE_tra.
*    MOVE-CORRESPONDING ILINE_tra TO it_down_tra.
*    it_down_tra-wtjhr = ILINE_tra-wtjhr * 100.
*    APPEND it_down_tra.  CLEAR it_down_tra.
*  ENDLOOP.
*
*ENDFORM.                    " down_list4

*&---------------------------------------------------------------------*
*&      Form  call_function
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_function.

  DATA: ufile LIKE rlgrap-filename,
        lv_cancle TYPE c.

  ufile = 'C:\temp\'.
  CALL FUNCTION 'DOWNLOAD'
       EXPORTING
            filename                = ufile
            filetype                = 'DAT'  "WK1
            filetype_no_change      = ' '
            filetype_no_show        = ' '
       IMPORTING
            cancel                  = lv_cancle
       TABLES
            data_tab                = it_down
       EXCEPTIONS
            invalid_filesize        = 1
            invalid_table_width     = 2
            invalid_type            = 3
            no_batch                = 4
            unknown_error           = 5
            gui_refuse_filetransfer = 6
            customer_error          = 7
            OTHERS                  = 8.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " call_function
*&---------------------------------------------------------------------*
*&      Form  call_function2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM call_function2.
*
*  DATA: ufile LIKE rlgrap-filename,
*              lv_cancle TYPE c.
*
*  ufile = 'C:\temp\'.
*  CALL FUNCTION 'DOWNLOAD'
*       EXPORTING
*            filename                = ufile
*            filetype                = 'DAT'  "WK1
*            filetype_no_change      = ' '
*            filetype_no_show        = ' '
*       IMPORTING
*            cancel                  = lv_cancle
*       TABLES
*            data_tab                = it_down_add
*       EXCEPTIONS
*            invalid_filesize        = 1
*            invalid_table_width     = 2
*            invalid_type            = 3
*            no_batch                = 4
*            unknown_error           = 5
*            gui_refuse_filetransfer = 6
*            customer_error          = 7
*            OTHERS                  = 8.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
*
*
*ENDFORM.                    " call_function2
*FORM call_function3.
*
*  DATA: ufile LIKE rlgrap-filename,
*            lv_cancle TYPE c.
*
*  ufile = 'C:\temp\'.
*  CALL FUNCTION 'DOWNLOAD'
*       EXPORTING
*            filename                = ufile
*            filetype                = 'DAT'  "WK1
*            filetype_no_change      = ' '
*            filetype_no_show        = ' '
*       IMPORTING
*            cancel                  = lv_cancle
*       TABLES
*            data_tab                = it_down_sub
*       EXCEPTIONS
*            invalid_filesize        = 1
*            invalid_table_width     = 2
*            invalid_type            = 3
*            no_batch                = 4
*            unknown_error           = 5
*            gui_refuse_filetransfer = 6
*            customer_error          = 7
*            OTHERS                  = 8.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
*
*ENDFORM.                    " call_function3
*&---------------------------------------------------------------------*
*&      Form  call_function4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM call_function4.
*
*  DATA: ufile LIKE rlgrap-filename,
*          lv_cancle TYPE c.
*
*  ufile = 'C:\temp\'.
*  CALL FUNCTION 'DOWNLOAD'
*       EXPORTING
*            filename                = ufile
*            filetype                = 'DAT'
*            filetype_no_change      = ' '
*            filetype_no_show        = ' '
*       IMPORTING
*            cancel                  = lv_cancle
*       TABLES
*            data_tab                = it_down_tra
*       EXCEPTIONS
*            invalid_filesize        = 1
*            invalid_table_width     = 2
*            invalid_type            = 3
*            no_batch                = 4
*            unknown_error           = 5
*            gui_refuse_filetransfer = 6
*            customer_error          = 7
*            OTHERS                  = 8.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.

*ENDFORM.                    " call_function4
*&---------------------------------------------------------------------*
*&      Form  rewrite_wtjhrC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM rewrite_wtjhrc.

  SORT itab_rtn BY fictr geber.
  LOOP AT itab_rtn WHERE fictr = ILINE-fictr
                       AND   fipos = ILINE-fipos
                       AND   geber = ILINE-geber.
    WRITE: /'|' NO-GAP, AT (16) itab_rtn-fictr NO-GAP,
            '|' NO-GAP, AT (20) itab_rtn-bezeich NO-GAP,
            '|' NO-GAP, AT (14) itab_rtn-fipos NO-GAP,
            '|' NO-GAP, AT (20) itab_rtn-bezeichh NO-GAP,
            '|' NO-GAP, AT (10) itab_rtn-geber NO-GAP,
           '|' NO-GAP, AT (21) itab_rtn-wtjhr NO-GAP CURRENCY gCR,
            '|' NO-GAP, AT (50) itab_rtn-sgtext NO-GAP,
            '|'.
    WRITE:/001(159) sy-uline.
  ENDLOOP.

ENDFORM.                    " rewrite_wtjhrC
*&---------------------------------------------------------------------*
*&      Form  call_function5
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_function5.

  DATA: ufile LIKE rlgrap-filename,
           lv_cancle TYPE c.

  ufile = 'C:\temp\'.
  CALL FUNCTION 'DOWNLOAD'
       EXPORTING
            filename                = ufile
            filetype                = 'DAT'
            filetype_no_change      = ' '
            filetype_no_show        = ' '
       IMPORTING
            cancel                  = lv_cancle
       TABLES
            data_tab                = it_down_fi
       EXCEPTIONS
            invalid_filesize        = 1
            invalid_table_width     = 2
            invalid_type            = 3
            no_batch                = 4
            unknown_error           = 5
            gui_refuse_filetransfer = 6
            customer_error          = 7
            OTHERS                  = 8.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " call_function5
*&---------------------------------------------------------------------*
*&      Form  down_list5
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM down_list5.

  CLEAR it_down_fi.  REFRESH it_down_fi.

  it_down_fi-kostl = '코스트센타'.
  it_down_fi-ktext = 'Text'.
  it_down_fi-usnam = '작성부서'.
  it_down_fi-usnat = 'Text'.
  it_down_fi-hkont = '계정코드'.
  it_down_fi-txt20 = 'Text'.
  it_down_fi-wrbtr = '금   액'.
  it_down_fi-belnr = '전표번호'.
  it_down_fi-sgtxt = 'Text'.
  APPEND it_down_fi.  CLEAR it_down_fi.

  LOOP AT ILINE_fi.
    MOVE-CORRESPONDING ILINE_fi TO it_down_fi.
    it_down_fi-wrbtr = ILINE_fi-wrbtr * 100.
    APPEND it_down_fi.  CLEAR it_down_fi.
  ENDLOOP.

ENDFORM.                    " down_list5
*&---------------------------------------------------------------------*
*&      Form  itab_line
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM itab_line.

  perform line_calc.
  WRITE: /'|' NO-GAP, AT (7) ILINE-fictr NO-GAP,
          '|' NO-GAP, AT (20) ILINE-bezeich NO-GAP,
          '|' NO-GAP, AT (7) ILINE-fipos NO-GAP,
          '|' NO-GAP, AT (20) ILINE-bezeichh NO-GAP,
          '|' NO-GAP, AT (10) ILINE-geber NO-GAP,
          '|' NO-GAP,
  AT (16) ILINE-wtkbud NO-GAP decimals 0 round p_t CURRENCY gCR,
          '|' NO-GAP,
  AT (16) ILINE-wtkbn0 NO-GAP decimals 0 round p_t CURRENCY gCR,
          '|' NO-GAP,
  AT (16) ILINE-wtjhrd NO-GAP decimals 0 round p_t CURRENCY gCR,
          '|' NO-GAP,
  AT (16) ILINE-wtjhrc NO-GAP decimals 0 round p_t CURRENCY gCR,
          '|' NO-GAP,
  AT (16) ILINE-wtjhre NO-GAP decimals 0 round p_t CURRENCY gCR color 3,
          '|' NO-GAP,
  AT (16) ILINE-trbtra NO-GAP decimals 0 round p_t CURRENCY gCR,
          '|' NO-GAP,
  AT (16) ILINE-trbtrb NO-GAP decimals 0 round p_t CURRENCY gCR,
          '|' NO-GAP,
  AT (16) ILINE-trbtrc NO-GAP decimals 0 round p_t CURRENCY gCR color 3,
          '|'.
  HIDE:  ILINE-fictr, ILINE-fipos, ILINE-geber.

ENDFORM.                    " itab_line
*&---------------------------------------------------------------------*
*&      Form  line_calc
*&---------------------------------------------------------------------*
FORM line_calc.
* 수정예산 = 기초 + 추가 + 전송
  ILINE-wtjhre =
  ILINE-wtkbud + ILINE-wtkbn0 + ILINE-wtjhrd.
* 잔액 = 수정예산 - 실적
  ILINE-trbtrc = ILINE-wtjhre - ILINE-trbtrb.
ENDFORM.                    " line_calc
*&---------------------------------------------------------------------*
*&      Form  itab_summary
*&---------------------------------------------------------------------*
FORM itab_summary USING    VALUE(l_title).
  WRITE:/001(237) sy-uline.
  FORMAT COLOR 3 INTENSIFIED ON.

  perform line_calc.
  WRITE: /'|' NO-GAP.
  write: (68) l_title NO-GAP CENTERED.
  write: '|' NO-GAP,
  AT (16) ILINE-wtkbud NO-GAP decimals 0 round p_t CURRENCY gCR,
      '|' NO-GAP,
  AT (16) ILINE-wtkbn0 NO-GAP decimals 0 round p_t CURRENCY gCR,
      '|' NO-GAP,
  AT (16) ILINE-wtjhrd NO-GAP decimals 0 round p_t CURRENCY gCR,
      '|' NO-GAP,
  AT (16) ILINE-wtjhrc NO-GAP decimals 0 round p_t CURRENCY gCR,
      '|' NO-GAP,
  AT (16) ILINE-wtjhre NO-GAP decimals 0 round p_t CURRENCY gCR color 3,
      '|' NO-GAP,
  AT (16) ILINE-trbtra NO-GAP decimals 0 round p_t CURRENCY gCR,
      '|' NO-GAP,
  AT (16) ILINE-trbtrb NO-GAP decimals 0 round p_t CURRENCY gCR,
      '|' NO-GAP,
  AT (16) ILINE-trbtrc NO-GAP decimals 0 round p_t CURRENCY gCR color 3,
      '|'.

  WRITE:/001(237) sy-uline.
  FORMAT COLOR OFF.

ENDFORM.                    " itab_summary
*&---------------------------------------------------------------------*
*&      Form  go_fi_doc
*&---------------------------------------------------------------------*
FORM go_fi_doc.
  SET PARAMETER ID 'BLN' FIELD ILINE_FI-BELNR.
  SET PARAMETER ID 'BUK' FIELD p_bukrs.
  SET PARAMETER ID 'GJR' FIELD p_gjahr.
  CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
ENDFORM.                    " go_fi_doc
*&---------------------------------------------------------------------*
*&      Form  WRITE_ITEM
*&---------------------------------------------------------------------*
FORM write_item.

  LOOP AT ILINE.
    perform itab_line.

    AT END OF fipos.
      if p_s = 2.
        sum.
        perform itab_summary using 'Subtotal'.
      endif.
    ENDAT.

    AT END OF fictr.
      if p_s = 1.
        sum.
        perform itab_summary using 'S U M'.
      endif.
    ENDAT.
    AT last.
      sum.
      perform itab_summary using 'T O T A L'.
    ENDAT.
  ENDLOOP.
  CLEAR: ILINE-fictr, ILINE-fipos, ILINE-geber.


ENDFORM.                    " WRITE_ITEM
*&---------------------------------------------------------------------*
*&      Form  write_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_header.
  WRITE: /2  ICON_CREATE AS ICON,
             'Press SHIFT + F1 to Execute Download    ',
             ICON_DESELECT_ALL AS ICON,
             'Press SHIFT + F2 to Sort',
             '  Scale:', p_t.

  WRITE: /001(237) 'Plan/Actual Report' CENTERED.
  SKIP 2.

  FORMAT COLOR 1 INTENSIFIED ON.
  WRITE: /001(237) sy-uline.
  WRITE: /'|' NO-GAP, AT (7) 'FndCntr' CENTERED NO-GAP,
          '|' NO-GAP, AT (20) 'Text' CENTERED NO-GAP,
          '|' NO-GAP, AT (7) 'Cmmit Itm' CENTERED NO-GAP,
          '|' NO-GAP, AT (20) 'Text' CENTERED NO-GAP,
          '|' NO-GAP, AT (10) 'Fund' CENTERED NO-GAP,
          '|' NO-GAP, AT (16) 'OrgBudget' CENTERED NO-GAP,
          '|' NO-GAP, AT (16) 'Supplement' CENTERED NO-GAP,
          '|' NO-GAP, AT (16) 'Transfer' CENTERED NO-GAP,
          '|' NO-GAP, AT (16) 'Return' CENTERED NO-GAP,
          '|' NO-GAP, AT (16) 'CurBudget' CENTERED NO-GAP,
          '|' NO-GAP, AT (16) 'Actual'  CENTERED NO-GAP,
          '|' NO-GAP, AT (16) 'Act+Park'    CENTERED NO-GAP,
          '|' NO-GAP, AT (16) 'Availble'   CENTERED NO-GAP,
          '|'.
  WRITE: /001(237) sy-uline.
  FORMAT COLOR OFF.

ENDFORM.                    " write_header
*&---------------------------------------------------------------------*
*&      Form  init_variable
*&---------------------------------------------------------------------*
FORM init_variable.
*Commitment Item
*50:PR,
*51:PO,
*52:Trip commit,
*54:Invoice,
*60 : park document
*65	Fund commitment
*58	Down payment req,
*Actual
*57	payment
*61	down payment
*63	down payment clearing
*64	payment transfer
  CLEAR: R_cmmt, r_acct.
  R_cmmt-SIGN = 'I'.   R_cmmt-OPTION = 'EQ'.
  R_acct-SIGN = 'I'.   R_acct-OPTION = 'EQ'.
  R_acct-low  = '50'.  APPEND R_acct.
  R_acct-low  = '51'.  APPEND R_acct.
  R_acct-low  = '52'.  APPEND R_acct.
  R_acct-low  = '54'.  APPEND R_acct.
  R_acct-low  = '65'.  APPEND R_acct.
  R_cmmt[] = r_acct[].
  R_cmmt-low  = '60'.  APPEND R_cmmt.

* set month -> date
  check not p_monat is initial.

  P_DATUM-LOW+0(04) = P_GJAHR.
  P_DATUM-LOW+4(02) = P_Monat-low.
  P_DATUM-LOW+6(02) = '01'.

  if p_monat-high < p_monat-low.
     p_monat-high = p_monat-low.
  endif.

  P_DATUM-HIGH+0(04) = P_GJAHR.
  P_DATUM-HIGH+4(02) = P_Monat-high.
  P_DATUM-HIGH+6(02) = '01'.

    CALL FUNCTION 'MONTH_PLUS_DETERMINE'
         EXPORTING
              MONTHS  = 1
              OLDDATE = P_DATUM-high
         IMPORTING
              NEWDATE = P_DATUM-HIGH.
    P_DATUM-HIGH   = P_DATUM-HIGH - 1.
    p_datum-sign   = 'I'.
    p_datum-OPTION = 'BT'.
    append p_datum.

ENDFORM.                    " init_variable
