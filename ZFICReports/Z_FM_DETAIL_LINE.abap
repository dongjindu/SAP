report zfifmr02a  no standard page heading  message-id zmfi
                                            line-count 65
                                            line-size 206.
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
tables: bkpf, bseg, vbsegs, usr02,
        bpej,                "개별항목  연간값
        bpbk,
        t001,
        tka02,
        fmfctr,              "FIFM: 예산관리센터 마스터
        fmci,               "FIFM: 약� �사항 항목

* fm item, openitem, total
        fmifiit,             "FM Line
        fmifihd,             "FM Header(FI)
        fmioi,               "openitem
        fmit.                "Totals Table for Funds Management

*        zfivd002.                   "BSIS+BKPF

*----------------------------------------------------------------------*
* DATA
*----------------------------------------------------------------------*
* Internal table
data: begin of iline occurs 0,
        fictr     like fmfctr-fictr,           "펀드센타
        bezeich   like fmfctrt-bezeich,        "이름
        fipos     like fmci-fipos,            "약� �항목
        bezeih  like fmcit-bezei,         "이름
        geber     like bpej-geber,             "펀드
        wtkbud    like bpej-wtjhr,             "기초금액
        wtkbn0    like bpej-wtjhr,             "추가금액
        wtjhrc    like bpej-wtjhr,             "반환금액
        wtjhrd    like bpej-wtjhr,             "Transfer

        wtjhre    like bpej-wtjhr,             "수� �예산
        trbtra    like fmifiit-trbtr,          "실� �
        trbtrb    like fmifiit-trbtr,          "실� �계
        trbtrc    like fmifiit-trbtr,          "잔액
      end of iline.

data: begin of iline_add occurs 0,
        fictr     like fmfctr-fictr,           "예산관리센터
        bezeich     like fmfctrt-bezeich,        "이름
        fipos     like fmci-fipos,            "약� �항목
        bezeih  like fmcit-bezei,         "이름
        geber     like bpej-geber,             "펀드
        wtjhr     like bpej-wtjhr,             "금액
        sgtext    like bpbk-sgtext,            "텍스트
      end of iline_add.

data: begin of itab_rtn occurs 0,
        fictr     like fmfctr-fictr,           "예산관리센터
        bezeich     like fmfctrt-bezeich,        "이름
        fipos     like fmci-fipos,            "약� �항목
        bezeih  like fmcit-bezei,         "이름
        geber     like bpej-geber,             "펀드
        wtjhr     like bpej-wtjhr,             "금액
        sgtext    like bpbk-sgtext,            "텍스트
      end of itab_rtn.

data: begin of iline_tra occurs 0,
        fictrs     like fmfctr-fictr,           "센더펀드센터
        bezeis     like fmfctrt-bezeich,        "센더이름
        fictrr     like fmfctr-fictr,           "리시버펀드센터
        bezeir     like fmfctrt-bezeich,        "리시버이름
        fiposs     like fmci-fipos,            "약� �항목
        bezeihs  like fmcit-bezei,         "이름
        fiposr     like fmci-fipos,            "약� �항목
        bezeihr  like fmcit-bezei,         "이름
        gebers     like bpej-geber,             "센더펀드
        geberr     like bpej-geber,             "리시버펀드
        wtjhr      like bpej-wtjhr,             "금액
        sgtext     like bpbk-sgtext,            "텍스트
      end of iline_tra.

data: begin of iline_fi occurs 0,
        kostl    like bsis-kostl,
        usnam    like bsis-kostl,
        hkont    like bsis-hkont,
        wrbtr    like bsis-wrbtr,
        belnr    like bsis-belnr,
        sgtxt    like bsis-sgtxt,
        ktext    like cskt-ktext,
        usnat    like cskt-ktext,
        txt20    like skat-txt20,
        bstat    like bkpf-bstat,
      end of iline_fi.

data: begin of it_fmifiit occurs 0,
        fictr     like fmfctr-fictr,           "펀드센타
        fipos     like fmci-fipos,            "약� �항목
        geber     like bpej-geber,             "펀드
        fmbelnr   like fmifiit-fmbelnr,
        knbelnr   like fmifiit-knbelnr,        "FI� �표번호
        trbtr     like fmifiit-trbtr,          "실� �금액
        wrttp     like fmifiit-wrttp,          "값� 형
        hkont     like fmifiit-hkont,
        btart     like v_fmifi-btart,          "amount type
        bezeich     like fmfctrt-bezeich,        "펀드센타이름
        bezeih  like fmcit-bezei,         "약� �항목이름
      end of it_fmifiit.

data: begin of it_bpej occurs 0,
        belnr    like bpej-belnr,
        buzei    like bpej-buzei,
        wtjhr    like bpej-wtjhr,
        objnr    like bpej-objnr,
        posit    like bpej-posit,
        geber    like bpej-geber,
        sgtext   like bpbk-sgtext,
        vorga    like bpej-vorga,
        fictr    like fmfctr-fictr,
        bezeich  like fmfctrt-bezeich,
        fipos    like fmci-fipos,
        bezeih like fmcit-bezei,
      end of it_bpej.

data: it_temp  like it_bpej occurs 0 with header line,
      it_temp2 like it_bpej occurs 0 with header line.

data: begin of it_fmfctr occurs 0,
        fictr   like fmfctr-fictr,
        bezeich like fmfctrt-bezeich,
        bossid  like fmfctr-bossid,
      end of it_fmfctr.

data: begin of it_fmci occurs 0,
        fipos    like fmci-fipos,
        bezeih like fmcit-bezei,
        posit    like fmci-posit,
      end of it_fmci.

data: begin of it_belnr occurs 0,
        belnr like bpej-belnr,
      end of it_belnr.

data: begin of it_rewrite occurs 0,
        belnr like fmifiit-knbelnr,
      end of it_rewrite.

data: begin of it_cskt occurs 0,
        kostl like cskt-kostl,
        ktext like cskt-ktext,
      end of it_cskt.

data: begin of it_skat occurs 0,
        saknr like skat-saknr,
        txt20 like skat-txt20,
      end of it_skat.

* Excel down
data: begin of it_down occurs 0,
        fictr(16)     type c,           "펀드센타
        bezei(20)   type c,           "이름
        fipos(14)     type c,           "약� �항목
        bezeih(20)  type c,           "이름
        geber(10)     type c,           "펀드
        wtkbud(21)    type c,           "기초금액
        wtkbn0(21)    type c,           "추가금액
        wtjhrd(21)    type c,           "� �용금액
        wtjhre(21)    type c,           "수� �금액
        trbtra(16)    type c,           "실� �금액
        trbtrb(16)    type c,           "실� �계금액
        trbtrc(16)    type c,           "잔액
      end of it_down.

data: begin of it_down_add occurs 0,
        fictr(16)     type c,         "예산관리센터
        bezei(20)   type c,         "이름
        fipos(14)     type c,         "약� �항목
        bezeih(20)  type c,         "이름
        geber(10)     type c,         "펀드
        wtjhr(21)     type c,         "금액
        sgtext(50)    type c,         "텍스트
      end of it_down_add.

data: begin of it_down_sub occurs 0,
        fictr(16)     type c,        "예산관리센터
        bezei(20)   type c,        "이름
        fipos(14)     type c,        "약� �항목
        bezeih(20)  type c,        "이름
        geber(10)     type c,        "펀드
        wtjhr(21)     type c,        "금액
        sgtext(50)    type c,        "텍스트
      end of it_down_sub.

data: begin of it_down_tra occurs 0,
        fictrs(16)     type c,        "센더펀드센터
        bezeis(20)   type c,        "센더이름
        fictrr(14)     type c,        "리시버펀드센터
        bezeir(20)   type c,        "리시버이름
        fiposs(16)     type c,        "약� �항목
        bezeihs(20)  type c,        "이름
        fiposr(14)     type c,        "약� �항목
        bezeihr(20)  type c,        "이름
        gebers(10)     type c,        "센더펀드
        geberr(10)     type c,        "리시버펀드
        wtjhr(21)      type c,        "금액
        sgtext(50)     type c,        "텍스트
      end of it_down_tra.

data: begin of it_down_fi occurs 0,
        kostl(10)    type c,
        ktext(20)    type c,
        usnam(10)    type c,
        usnat(20)    type c,
        hkont(11)    type c,
        txt20(20)    type c,
        wrbtr(16)    type c,
        belnr(11)    type c,
        sgtxt(50)    type c,
      end of it_down_fi.

* Work Area
data: wa_temp like it_temp.

* Variables
data: v_mod       type i,
      v_cnt       type i,
      v_vorga     like bpej-vorga,
      v_vorgab    like bpej-vorga,
      v_field(20) type c,
      v_off       type i,
      v_sortf(10) type c,
      sum_wtkbud  like bpej-wtjhr,
      sum_wtkbn0  like bpej-wtjhr,
      sum_wtjhrc  like bpej-wtjhr,
      sum_wtjhrd  like bpej-wtjhr,
      sum_wtjhre  like bpej-wtjhr,
      sum_trbtra  like fmifiit-trbtr,
      sum_trbtrb  like fmifiit-trbtr,
      sum_trbtrc  like fmifiit-trbtr.

data: p_fikrs  like fmifiit-fikrs,
      gcr    like t001-waers,
      g_history type c.
ranges:r_cmmt for fmifiit-wrttp,
       r_acct for fmifiit-wrttp,
       p_datum for sy-datum.


*** start include ******************************************************
include <icon>.


*----------------------------------------------------------------------*
* SELECTION SCREEN ( SELECT-OPTIONS & PARAMETERS )
*----------------------------------------------------------------------*
parameters: p_bukrs  like t001-bukrs memory id buk,
            p_gjahr  like bpej-gjahr memory id gjr.
select-options:
            p_monat  for bkpf-monat.

selection-screen begin of block sb with frame title text-s10.
select-options:
                s_geber  for bpej-geber   memory id fic,
                s_fictr  for fmfctr-fictr memory id fis,
                s_fipos  for fmci-fipos,
                s_bossid for usr02-bname. "Manager
selection-screen end of block sb.
parameters: p_t type n.
parameters: p_s type n default '1'.
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
at selection-screen.


*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
start-of-selection.

  select single * from t001 where bukrs = p_bukrs.
  check sy-subrc = 0.
  p_fikrs = t001-fikrs.
  gcr   = t001-waers.
  select single * from tka02 where bukrs = p_bukrs.

  perform init_variable.

* Select Data
  perform select_data.

* Modify Internal Table
  perform modify_it_table.

* Message
  describe table iline lines v_cnt.
  if v_cnt = 0.
    message s006.
    exit.
  endif.

*----------------------------------------------------------------------*
* END OF SELECTION
*----------------------------------------------------------------------*
end-of-selection.

  sort iline by fictr fipos geber.
  perform write_item.

*----------------------------------------------------------------------*
* TOP OF PAGE
*----------------------------------------------------------------------*
top-of-page.
  perform write_header.

top-of-page during line-selection.

  if g_history = space.
    perform write_header.
  else.
    perform rewrite_header.
  endif.

*----------------------------------------------------------------------*
* AT LINE SELECTION
*----------------------------------------------------------------------*
at line-selection.

  get cursor field v_field.
  g_history = 'X'.
  case v_field.
* supplement / transfer / return /
    when 'ILINE-wtkbn0'.
      perform rewrite_wtkbn0.
    when 'ILINE-WTJHRC'.
      perform rewrite_wtjhrc.
    when 'ILINE-WTJHRD'.
      perform rewrite_tra.

* actual
    when 'ILINE-TRBTRB' or 'ILINE-TRBTRA'.
      perform rewrite_trbtrb.
* fi doc
    when 'ILINE_FI-BELNR'.
      perform go_fi_doc.
    when others.
      g_history = space.
  endcase.

************************************************************************
***  (SHIFT+PF1) Execute Download
************************************************************************
at pf13.
  perform download_data.
************************************************************************
***  (SHIFT+PF4) Sort
************************************************************************
at pf14.
  perform sort_data.
  sy-lsind = sy-lsind - 1.

*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form select_data.

* Budget
* monthly budget is entered into BPPE(month), BPJA, BPEP(line)
* yearly budget is in the 1st field:WTP01
* KBFR - release
  select l~belnr l~buzei l~wtjhr l~objnr l~posit l~geber
         h~sgtext l~vorga
                     into table it_bpej
                     from bpej as l inner join bpbk as h
                       on l~belnr  = h~belnr
                     where l~gjahr = p_gjahr
                       and h~bldat in p_datum
                       and l~wrttp = '43'      "no budget release
                       and l~posit > 'FP000001'
                       and l~vorga in ('KBUD', 'KBN0', 'KBR0',
                                       'KBUE', 'KBUS').


* actual
  select fistl fipex fonds fmbelnr knbelnr trbtr wrttp hkont btart
               into table it_fmifiit
               from v_fmifi
               where fikrs =  p_fikrs
               and   gjahr =  p_gjahr
               and   bldat in p_datum
               and   fonds in s_geber
               and   fistl in s_fictr
               and   fipex in s_fipos
               and   wrttp in r_cmmt
               and   btart = '0100'.  "original
*master
* Commitment Item
  select fipos bezeich posit
               into table it_fmci
               from u_12439
               where fikrs = p_fikrs
               and   datbis >= sy-datum
               and   spras = sy-langu.

* Funds center
  select fictr bezeich bossid  into table it_fmfctr
               from u_12410
               where  fikrs = p_fikrs
               and    datbis >= sy-datum
               and    spras = sy-langu.

* 코스터센터(Text), G/L 계� �(Text)
  perform select_text.

endform.                    " SELECT_DATA
*&---------------------------------------------------------------------*
*&      Form  modify_it_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form modify_it_table.

* 기초, 추가, 반환, � �송(Collect)
  perform move_it_bpej.

* 실� �, 실� �계(Collect)
  perform move_it_fmifiit.

endform.                    " modify_it_table
*&---------------------------------------------------------------------*
*&      Form  move_it_bpej
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form move_it_bpej.

  loop at it_bpej.
    clear: it_fmci, it_fmfctr.
* 약� �항목, 이름
    read table it_fmci with key posit = it_bpej-posit.
    if sy-subrc = 0.
      iline-fipos = it_bpej-fipos = it_fmci-fipos.
      iline-bezeih = it_bpej-bezeih = it_fmci-bezeih.
    endif.
* 펀드센터
    iline-fictr = it_bpej-fictr = it_bpej-objnr+6(16).
* 펀드센터(이름)
    read table it_fmfctr with key fictr = iline-fictr.
    if sy-subrc = 0.
      iline-bezeich = it_bpej-bezeich = it_fmfctr-bezeich.
    endif.
    modify it_bpej.

* 조건(펀드, 펀드센터, 약� �항목, 책임자)
    if it_bpej-geber in s_geber and
       iline-fictr  in s_fictr and
       iline-fipos  in s_fipos and
       it_fmfctr-bossid in s_bossid.

      move: it_bpej-geber to iline-geber.
      if it_bpej-vorga = 'KBUD'.                    "기초
        move: it_bpej-wtjhr to iline-wtkbud.
      elseif it_bpej-vorga = 'KBN0'.                "추가
        move: it_bpej-wtjhr to iline-wtkbn0.
        move-corresponding it_bpej to iline_add.  "Drill down용
        append iline_add.  clear iline_add.
      elseif it_bpej-vorga = 'KBR0'.
        move: it_bpej-wtjhr to iline-wtjhrc.      "반환
        move-corresponding it_bpej to itab_rtn.  "Drill down용
        append itab_rtn.  clear itab_rtn.
      elseif it_bpej-vorga = 'KBUE' or               "� �송
             it_bpej-vorga = 'KBUS'.
        move: it_bpej-wtjhr to iline-wtjhrd.
        move-corresponding it_bpej to it_temp.
        append it_temp.  clear it_temp.
      endif.
      move: it_bpej-geber to iline-geber.
      collect iline.  clear iline.

*      DELETE it_bpej.
    endif.
  endloop.

endform.                    " move_it_bpej
*&---------------------------------------------------------------------*
*&      Form  move_it_fmifiit
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form move_it_fmifiit.


  loop at it_fmifiit.
* 조건(책임자)
    check it_fmfctr-bossid in s_bossid.

    clear: it_fmci, it_fmfctr.
* 약� �항목(이름)
    read table it_fmci with key fipos = it_fmifiit-fipos.
    if sy-subrc = 0.
      iline-bezeih = it_fmci-bezeih.
    endif.
* 펀드센터(이름)
    read table it_fmfctr with key fictr = it_fmifiit-fictr.
    if sy-subrc = 0.
      iline-bezeich = it_fmfctr-bezeich.
    endif.

* actual (park doc, real)
    if it_fmifiit-wrttp in r_cmmt.
      iline-trbtrb = it_fmifiit-trbtr * ( - 1 ).

      if it_fmifiit-wrttp in r_acct.
        iline-trbtra = it_fmifiit-trbtr * ( - 1 ).
      endif.
    endif.

    move: it_fmifiit-fictr to iline-fictr,
          it_fmifiit-fipos to iline-fipos,
          it_fmifiit-geber to iline-geber.
    collect iline.  clear iline.
  endloop.

endform.                    " move_it_fmifiit
*&---------------------------------------------------------------------*
*&      Form  rewrite_wtkbn0
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form rewrite_wtkbn0.

  sort iline_add by fictr geber.
  loop at iline_add where fictr = iline-fictr
                       and   fipos = iline-fipos
                       and   geber = iline-geber.
    write: /'|' no-gap, at (16) iline_add-fictr no-gap,
            '|' no-gap, at (20) iline_add-bezeich no-gap,
            '|' no-gap, at (14) iline_add-fipos no-gap,
            '|' no-gap, at (20) iline_add-bezeih no-gap,
            '|' no-gap, at (10) iline_add-geber no-gap,
           '|' no-gap, at (21) iline_add-wtjhr no-gap currency gcr,
            '|' no-gap, at (50) iline_add-sgtext no-gap,
            '|'.
    write:/001(159) sy-uline.
  endloop.

endform.                    " rewrite_wtkbn0
*&---------------------------------------------------------------------*
*&      Form  rewrite_trbtrb
*&---------------------------------------------------------------------*
*       actual
*----------------------------------------------------------------------*
form rewrite_trbtrb.
  clear: iline_fi, it_rewrite.
  refresh: iline_fi, it_rewrite.


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


  loop at it_fmifiit where geber = iline-geber
                     and   fictr = iline-fictr
                     and   fipos = iline-fipos
                     and   btart = '0100'.

    if v_field = 'ILINE-TRBTRA'. "actual only
      if it_fmifiit-wrttp = '60'. "park document
        continue.
      endif.
    endif.

    select single usnam bstat
           into (iline_fi-usnam, iline_fi-bstat)
           from bkpf
           where belnr = it_fmifiit-knbelnr
             and gjahr = p_gjahr
             and bukrs = p_bukrs
             and stblg = space.

    check sy-subrc = 0.
    if it_fmifiit-wrttp = '60'. "park document
      select * from vbsegs
           where belnr   = it_fmifiit-knbelnr
             and gjahr   = p_gjahr
             and bukrs   = p_bukrs.
        if vbsegs-saknr  = it_fmifiit-hkont.
          iline_fi-kostl = vbsegs-kostl.
          iline_fi-belnr = it_fmifiit-knbelnr.
          iline_fi-hkont = vbsegs-saknr.
          iline_fi-wrbtr = vbsegs-wrbtr.
          iline_fi-sgtxt = vbsegs-sgtxt.
          append iline_fi.
        endif.
      endselect.
    else.
      select * from bseg
           where belnr   = it_fmifiit-knbelnr
             and gjahr   = p_gjahr
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
  endloop.

  check sy-subrc = 0.

* Rewrite
  perform rewrite_fi.

endform.                    " rewrite_trbtrb
*&---------------------------------------------------------------------*
*&      Form  rewrite_tra
*&---------------------------------------------------------------------*
*       transfer
*----------------------------------------------------------------------*
form rewrite_tra.
  perform get_transfer_info.


  sort iline_tra by fictrs gebers.
  loop at iline_tra.
    write: /'|' no-gap, at (7) iline_tra-fictrs no-gap,
            '|' no-gap, at (20) iline_tra-bezeis no-gap,
            '|' no-gap, at (7) iline_tra-fiposs no-gap,
            '|' no-gap, at (20) iline_tra-bezeihs no-gap,
            '|' no-gap, at (7) iline_tra-fictrr no-gap,
            '|' no-gap, at (20) iline_tra-bezeir no-gap,
            '|' no-gap, at (7) iline_tra-fiposr no-gap,
            '|' no-gap, at (20) iline_tra-bezeihr no-gap,
            '|' no-gap, at (10) iline_tra-gebers no-gap,
            '|' no-gap, at (10) iline_tra-geberr no-gap,
           '|' no-gap, at (16) iline_tra-wtjhr no-gap currency gcr,
            '|' no-gap, at (40) iline_tra-sgtext no-gap,
            '|'.
    write:/001(244) sy-uline.
  endloop.

endform.                    " rewrite_tra
*&---------------------------------------------------------------------*
form get_transfer_info.

  clear: it_temp2.  refresh it_temp2.

  loop at it_temp where fictr = iline-fictr
                  and   fipos = iline-fipos
                  and   geber = iline-geber.
    move-corresponding it_temp to it_temp2.
    append it_temp2.  clear it_temp2.
    move: it_temp-belnr to it_belnr-belnr.
    collect it_belnr.  clear it_belnr.
  endloop.

* 없는 부분 추가
  loop at it_belnr.
    read table it_bpej with key belnr = it_belnr-belnr.
    check sy-subrc = 0.
    move-corresponding it_bpej to it_temp2.
    append it_temp2.  clear it_temp2.
  endloop.

* 센더, 리시버로 구분
  sort it_temp2 by belnr buzei.
  loop at it_temp2.
    v_mod = sy-tabix mod 2.
    if v_mod = 1.
      move: it_temp2 to wa_temp.
    elseif v_mod = 0.
      perform move_sen_tra.
      perform move_rec_tra.
      append iline_tra.  clear iline_tra.
      clear wa_temp.
    endif.
  endloop.

endform.                    " rewrite_wtjhrd
*&---------------------------------------------------------------------*
*&      Form  move_sen_tra
*&---------------------------------------------------------------------*
form move_sen_tra.
* sender
  move: wa_temp-fictr   to iline_tra-fictrs,
        wa_temp-bezeich  to iline_tra-bezeis,
        wa_temp-fipos    to iline_tra-fiposs,
        wa_temp-bezeih to iline_tra-bezeihs,
        wa_temp-geber    to iline_tra-gebers,
        wa_temp-sgtext   to iline_tra-sgtext.

endform.                    " move_sen_tra
*&---------------------------------------------------------------------*
*&      Form  move_rec_tra
*&---------------------------------------------------------------------*
form move_rec_tra.

  move:  it_temp2-fictr    to iline_tra-fictrr,
         it_temp2-bezeich  to iline_tra-bezeir,
         it_temp2-fipos    to iline_tra-fiposr,
         it_temp2-bezeih to iline_tra-bezeihr,
         it_temp2-geber    to iline_tra-geberr,
         it_temp2-wtjhr    to iline_tra-wtjhr.

endform.                    " move_rec_tra
*&---------------------------------------------------------------------*
*&      Form  rewrite_fi
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form rewrite_fi.

  sort iline_fi by kostl usnam hkont.
  loop at iline_fi.
    perform read_text.
    write: /'|' no-gap, at (1)  iline_fi-bstat no-gap,
            '|' no-gap, at (7)  iline_fi-kostl no-gap,
            '|' no-gap, at (20) iline_fi-ktext no-gap,
            '|' no-gap, at (10)  iline_fi-usnam no-gap,
            '|' no-gap, at (20) iline_fi-usnat no-gap,
            '|' no-gap, at (7) iline_fi-hkont no-gap,
            '|' no-gap, at (20) iline_fi-txt20 no-gap,
            '|' no-gap, at (16) iline_fi-wrbtr no-gap,
            '|' no-gap, at (10) iline_fi-belnr no-gap,
            '|' no-gap, at (50) iline_fi-sgtxt no-gap,
            '|'.
    hide: iline_fi-belnr.
  endloop.
  clear: iline_fi-belnr.
  write:/001(170) sy-uline.
endform.                    " rewrite_fi
*&---------------------------------------------------------------------*
*&      Form  select_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form select_text.

  select kostl ktext  into table it_cskt
                      from cskt
                      where spras = sy-langu
                      and   kokrs = tka02-kokrs
                      and   datbi >= sy-datum.

  select saknr txt20  into table it_skat
                      from skat
                      where spras = sy-langu
                      and   ktopl = t001-ktopl.

endform.                    " select_text
*&---------------------------------------------------------------------*
*&      Form  READ_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_text.

  read table it_cskt with key kostl = iline_fi-kostl.
  if sy-subrc = 0.
    move it_cskt-ktext to iline_fi-ktext.
  endif.
  read table it_cskt with key kostl = iline_fi-usnam.
  if sy-subrc = 0.
    move it_cskt-ktext to iline_fi-usnat.
  endif.
  read table it_skat with key saknr = iline_fi-hkont.
  if sy-subrc = 0.
    move it_skat-txt20 to iline_fi-txt20.
  endif.

endform.                    " READ_TEXT
*&---------------------------------------------------------------------*
*&      Form  SORT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form sort_data.

  get cursor field v_field.
  v_sortf = v_field+9(7).
  sort iline by (v_sortf).

  perform write_header.
  perform write_item.

endform.                    " SORT_DATA
*&---------------------------------------------------------------------*
*&      Form  download_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form download_data.

  perform down_list.
  perform call_function.
*  CASE sy-pfkey.
*    WHEN 'LIST'.                             "Main
*    WHEN 'LIST2'.                            "추가
*      PERFORM down_list2.
*      PERFORM call_function2.
*    WHEN 'LIST3'.                            "반환
*      PERFORM down_list3.
*      PERFORM call_function3.
*    WHEN 'LIST4'.                            "� �송
*      PERFORM down_list4.
*      PERFORM call_function4.
*    WHEN 'LIST5'.                            "실� �계
*      PERFORM down_list5.
*      PERFORM call_function5.
*  ENDCASE.
*
endform.                    " download_data
*&---------------------------------------------------------------------*
*&      Form  rewrite_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form rewrite_header.

  case v_field.
* supplement / return /
    when 'ILINE-wtkbn0' or 'ILINE-WTJHRC'.
      perform rewrite_rev_hd.
* transfer
    when 'ILINE-WTJHRD'.
      perform rewrite_transfer_hd.
* actual
    when 'ILINE-TRBTRA' or 'ILINE-TRBTRB'.
      perform rewrite_act_hd.
  endcase.

endform.                    " rewrite_header
*&---------------------------------------------------------------------*
*&      Form  rewrite_wtkbn0_hd
*&---------------------------------------------------------------------*
form rewrite_rev_hd.

  write: /001(159) 'Budget Revise History' centered.
  skip 2.

  format color 1 intensified on.
  write: /001(159) sy-uline.
  write: /'|' no-gap, at (16) 'FnCtr' centered no-gap,
          '|' no-gap, at (20) 'Text' centered no-gap,
          '|' no-gap, at (14) 'CmmtItm' centered no-gap,
          '|' no-gap, at (20) 'Text' centered no-gap,
          '|' no-gap, at (10) 'Fund' centered no-gap,
          '|' no-gap, at (21) 'Amount' centered no-gap,
          '|' no-gap, at (50) 'Text' centered no-gap,
          '|'.
  write: /001(159) sy-uline.
  format color off.

endform.                    " rewrite_wtkbn0_hd
*&---------------------------------------------------------------------*
*&      Form  rewrite_wtjhrd_hd
*&---------------------------------------------------------------------*
form rewrite_transfer_hd.

  write: /001(159) 'Budget Transfer History' centered.
  skip 2.

  format color 1 intensified on.
  write: /001(244) sy-uline.
  write: /'|' no-gap, at (7) 'Sender' centered no-gap,
          '|' no-gap, at (20) 'Text' centered no-gap,
          '|' no-gap, at (7) 'CmmtItm' centered no-gap,
          '|' no-gap, at (20) 'Text' centered no-gap,
          '|' no-gap, at (7) 'Receiver' centered no-gap,
          '|' no-gap, at (20) 'Text' centered no-gap,
          '|' no-gap, at (7) 'CmmtItm' centered no-gap,
          '|' no-gap, at (20) 'Text' centered no-gap,
          '|' no-gap, at (10) 'Fund' centered no-gap,
          '|' no-gap, at (10) 'Fund' centered no-gap,
          '|' no-gap, at (16) 'Amount' centered no-gap,
          '|' no-gap, at (40) 'Text' centered no-gap,
          '|'.
  write: /001(244) sy-uline.
  format color off.

endform.                    " rewrite_wtjhrd_hd
*&---------------------------------------------------------------------*
*&      Form  rewrite_wtkbn0_hd
*&---------------------------------------------------------------------*
form rewrite_act_hd.

  write: /001(170) 'Budget Actual Use History' centered.
  skip 2.

  format color 1 intensified on.
  write: /001(170) sy-uline.
  write: /'|' no-gap, at (1) 'P' centered no-gap,
          '|' no-gap, at (7) 'FnCtr' centered no-gap,
          '|' no-gap, at (20) 'FnCtrName' centered no-gap,
          '|' no-gap, at (10) 'UserID' centered no-gap,
          '|' no-gap, at (20) 'UserName' centered no-gap,
          '|' no-gap, at (7) 'CmmtItm' centered no-gap,
          '|' no-gap, at (20) 'CmmtItmName' centered no-gap,
          '|' no-gap, at (16) 'Amount' centered no-gap,
          '|' no-gap, at (10) 'Doc No.' centered no-gap,
          '|' no-gap, at (50) 'Text' centered no-gap,
          '|'.
  write: /001(170) sy-uline.
  format color off.

endform.                    " rewrite_wtkbn0_hd
*&---------------------------------------------------------------------*
*&      Form  down_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form down_list.

  clear it_down.  refresh it_down.

  it_down-fictr = 'FnCtr'.
  it_down-bezei = 'Text'.
  it_down-fipos = 'CmmtItem'.
  it_down-bezeih = 'Text'.
  it_down-geber = 'Fund'.
  it_down-wtkbud = 'InitBal'.
  it_down-wtkbn0 = 'Supplement'.
  it_down-wtjhrd = 'Transfer'.
  it_down-wtjhre = 'CurBudget'.
  it_down-trbtra = 'Actual'.
  it_down-trbtrb = 'ActSum'.
  it_down-trbtrc = 'Balance'.
  append it_down.  clear it_down.

  loop at iline.
    it_down-fictr = iline-fictr.
    it_down-bezei = iline-bezeich.
    it_down-fipos = iline-fipos.
    it_down-bezeih = iline-bezeih.
    it_down-geber = iline-geber.
    it_down-wtkbud = iline-wtkbud.
    it_down-wtkbn0 = iline-wtkbn0.
    it_down-wtjhrd = iline-wtjhrd.
    it_down-wtjhre = iline-wtjhre.
    it_down-trbtra = iline-trbtra.
    it_down-trbtrb = iline-trbtrb.
    it_down-trbtrc = iline-trbtrc.
    append it_down.  clear it_down.
  endloop.

endform.                    " down_list
*&---------------------------------------------------------------------*
*FORM down_list2.
*
*  CLEAR it_down_add.  REFRESH it_down_add.
*
*  it_down_add-fictr = '펀드센타'.
*  it_down_add-bezei = 'Text'.
*  it_down_add-fipos = '약� �항목'.
*  it_down_add-bezeih = 'Text'.
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
*  it_down_add-bezei = 'Text'.
*  it_down_add-fipos = '약� �항목'.
*  it_down_add-bezeih = 'Text'.
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
*  it_down_tra-bezeis = 'Text'.
*  it_down_tra-fictrr = '센더약� �항목'.
*  it_down_tra-bezeir = 'Text'.
*  it_down_tra-fiposs = '리시버펀드센타'.
*  it_down_tra-bezeihs = 'Text'.
*  it_down_tra-fiposr = '리시버약� �항목'.
*  it_down_tra-bezeihr = 'Text'.
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
form call_function.

  data: ufile like rlgrap-filename,
        lv_cancle type c.

  ufile = 'C:\temp\'.
  call function 'DOWNLOAD'
       exporting
            filename                = ufile
            filetype                = 'DAT'  "WK1
            filetype_no_change      = ' '
            filetype_no_show        = ' '
       importing
            cancel                  = lv_cancle
       tables
            data_tab                = it_down
       exceptions
            invalid_filesize        = 1
            invalid_table_width     = 2
            invalid_type            = 3
            no_batch                = 4
            unknown_error           = 5
            gui_refuse_filetransfer = 6
            customer_error          = 7
            others                  = 8.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

endform.                    " call_function
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
form rewrite_wtjhrc.

  sort itab_rtn by fictr geber.
  loop at itab_rtn where fictr = iline-fictr
                       and   fipos = iline-fipos
                       and   geber = iline-geber.
    write: /'|' no-gap, at (16) itab_rtn-fictr no-gap,
            '|' no-gap, at (20) itab_rtn-bezeich no-gap,
            '|' no-gap, at (14) itab_rtn-fipos no-gap,
            '|' no-gap, at (20) itab_rtn-bezeih no-gap,
            '|' no-gap, at (10) itab_rtn-geber no-gap,
           '|' no-gap, at (21) itab_rtn-wtjhr no-gap currency gcr,
            '|' no-gap, at (50) itab_rtn-sgtext no-gap,
            '|'.
    write:/001(159) sy-uline.
  endloop.

endform.                    " rewrite_wtjhrC
*&---------------------------------------------------------------------*
*&      Form  call_function5
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form call_function5.

  data: ufile like rlgrap-filename,
           lv_cancle type c.

  ufile = 'C:\temp\'.
  call function 'DOWNLOAD'
       exporting
            filename                = ufile
            filetype                = 'DAT'
            filetype_no_change      = ' '
            filetype_no_show        = ' '
       importing
            cancel                  = lv_cancle
       tables
            data_tab                = it_down_fi
       exceptions
            invalid_filesize        = 1
            invalid_table_width     = 2
            invalid_type            = 3
            no_batch                = 4
            unknown_error           = 5
            gui_refuse_filetransfer = 6
            customer_error          = 7
            others                  = 8.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

endform.                    " call_function5
*&---------------------------------------------------------------------*
*&      Form  down_list5
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form down_list5.

  clear it_down_fi.  refresh it_down_fi.

  it_down_fi-kostl = '코스트센타'.
  it_down_fi-ktext = 'Text'.
  it_down_fi-usnam = '작성부서'.
  it_down_fi-usnat = 'Text'.
  it_down_fi-hkont = '계� �코드'.
  it_down_fi-txt20 = 'Text'.
  it_down_fi-wrbtr = '금   액'.
  it_down_fi-belnr = '� �표번호'.
  it_down_fi-sgtxt = 'Text'.
  append it_down_fi.  clear it_down_fi.

  loop at iline_fi.
    move-corresponding iline_fi to it_down_fi.
    it_down_fi-wrbtr = iline_fi-wrbtr * 100.
    append it_down_fi.  clear it_down_fi.
  endloop.

endform.                    " down_list5
*&---------------------------------------------------------------------*
*&      Form  itab_line
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form itab_line.

  perform line_calc.
  write: /'|' no-gap, at (7) iline-fictr no-gap,
          '|' no-gap, at (20) iline-bezeich no-gap,
          '|' no-gap, at (7) iline-fipos no-gap,
          '|' no-gap, at (20) iline-bezeih no-gap,
          '|' no-gap, at (10) iline-geber no-gap,
          '|' no-gap,
  at (16) iline-wtkbud no-gap decimals 0 round p_t currency gcr,
          '|' no-gap,
  at (16) iline-wtkbn0 no-gap decimals 0 round p_t currency gcr,
          '|' no-gap,
  at (16) iline-wtjhrd no-gap decimals 0 round p_t currency gcr,
          '|' no-gap,
  at (16) iline-wtjhrc no-gap decimals 0 round p_t currency gcr,
          '|' no-gap,
  at (16) iline-wtjhre no-gap decimals 0 round p_t currency gcr color 3,
          '|' no-gap,
  at (16) iline-trbtra no-gap decimals 0 round p_t currency gcr,
          '|' no-gap,
  at (16) iline-trbtrb no-gap decimals 0 round p_t currency gcr,
          '|' no-gap,
  at (16) iline-trbtrc no-gap decimals 0 round p_t currency gcr color 3,
          '|'.
  hide:  iline-fictr, iline-fipos, iline-geber.

endform.                    " itab_line
*&---------------------------------------------------------------------*
*&      Form  line_calc
*&---------------------------------------------------------------------*
form line_calc.
* 수� �예산 = 기초 + 추가 + � �송
  iline-wtjhre =
  iline-wtkbud + iline-wtkbn0 + iline-wtjhrd.
* 잔액 = 수� �예산 - 실� �
  iline-trbtrc = iline-wtjhre - iline-trbtrb.
endform.                    " line_calc
*&---------------------------------------------------------------------*
*&      Form  itab_summary
*&---------------------------------------------------------------------*
form itab_summary using    value(l_title).
  write:/001(237) sy-uline.
  format color 3 intensified on.

  perform line_calc.
  write: /'|' no-gap.
  write: (68) l_title no-gap centered.
  write: '|' no-gap,
  at (16) iline-wtkbud no-gap decimals 0 round p_t currency gcr,
      '|' no-gap,
  at (16) iline-wtkbn0 no-gap decimals 0 round p_t currency gcr,
      '|' no-gap,
  at (16) iline-wtjhrd no-gap decimals 0 round p_t currency gcr,
      '|' no-gap,
  at (16) iline-wtjhrc no-gap decimals 0 round p_t currency gcr,
      '|' no-gap,
  at (16) iline-wtjhre no-gap decimals 0 round p_t currency gcr color 3,
      '|' no-gap,
  at (16) iline-trbtra no-gap decimals 0 round p_t currency gcr,
      '|' no-gap,
  at (16) iline-trbtrb no-gap decimals 0 round p_t currency gcr,
      '|' no-gap,
  at (16) iline-trbtrc no-gap decimals 0 round p_t currency gcr color 3,
      '|'.

  write:/001(237) sy-uline.
  format color off.

endform.                    " itab_summary
*&---------------------------------------------------------------------*
*&      Form  go_fi_doc
*&---------------------------------------------------------------------*
form go_fi_doc.
  set parameter id 'BLN' field iline_fi-belnr.
  set parameter id 'BUK' field p_bukrs.
  set parameter id 'GJR' field p_gjahr.
  call transaction 'FB03' and skip first screen.
endform.                    " go_fi_doc
*&---------------------------------------------------------------------*
*&      Form  WRITE_ITEM
*&---------------------------------------------------------------------*
form write_item.

  loop at iline.
    perform itab_line.

    at end of fipos.
      if p_s = 2.
        sum.
        perform itab_summary using 'Subtotal'.
      endif.
    endat.

    at end of fictr.
      if p_s = 1.
        sum.
        perform itab_summary using 'S U M'.
      endif.
    endat.
    at last.
      sum.
      perform itab_summary using 'T O T A L'.
    endat.
  endloop.
  clear: iline-fictr, iline-fipos, iline-geber.


endform.                    " WRITE_ITEM
*&---------------------------------------------------------------------*
*&      Form  write_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form write_header.
  write: /2  icon_create as icon,
             'Press SHIFT + F1 to Execute Download    ',
             icon_deselect_all as icon,
             'Press SHIFT + F2 to Sort',
             '  Scale:', p_t.

  write: /001(237) 'Plan/Actual Report' centered.
  skip 2.

  format color 1 intensified on.
  write: /001(237) sy-uline.
  write: /'|' no-gap, at (7) 'FndCntr' centered no-gap,
          '|' no-gap, at (20) 'Text' centered no-gap,
          '|' no-gap, at (7) 'Cmmit Itm' centered no-gap,
          '|' no-gap, at (20) 'Text' centered no-gap,
          '|' no-gap, at (10) 'Fund' centered no-gap,
          '|' no-gap, at (16) 'OrgBudget' centered no-gap,
          '|' no-gap, at (16) 'Supplement' centered no-gap,
          '|' no-gap, at (16) 'Transfer' centered no-gap,
          '|' no-gap, at (16) 'Return' centered no-gap,
          '|' no-gap, at (16) 'CurBudget' centered no-gap,
          '|' no-gap, at (16) 'Actual'  centered no-gap,
          '|' no-gap, at (16) 'Act+Park'    centered no-gap,
          '|' no-gap, at (16) 'Availble'   centered no-gap,
          '|'.
  write: /001(237) sy-uline.
  format color off.

endform.                    " write_header
*&---------------------------------------------------------------------*
*&      Form  init_variable
*&---------------------------------------------------------------------*
form init_variable.
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
  clear: r_cmmt, r_acct.
  r_cmmt-sign = 'I'.   r_cmmt-option = 'EQ'.
  r_acct-sign = 'I'.   r_acct-option = 'EQ'.
  r_acct-low  = '50'.  append r_acct.
  r_acct-low  = '51'.  append r_acct.
  r_acct-low  = '52'.  append r_acct.
  r_acct-low  = '54'.  append r_acct.
  r_acct-low  = '65'.  append r_acct.
  r_cmmt[] = r_acct[].
  r_cmmt-low  = '60'.  append r_cmmt.

* set month -> date
  check not p_monat is initial.

  p_datum-low+0(04) = p_gjahr.
  p_datum-low+4(02) = p_monat-low.
  p_datum-low+6(02) = '01'.

  if p_monat-high < p_monat-low.
     p_monat-high = p_monat-low.
  endif.

  p_datum-high+0(04) = p_gjahr.
  p_datum-high+4(02) = p_monat-high.
  p_datum-high+6(02) = '01'.

    call function 'MONTH_PLUS_DETERMINE'
         exporting
              months  = 1
              olddate = p_datum-high
         importing
              newdate = p_datum-high.
    p_datum-high   = p_datum-high - 1.
    p_datum-sign   = 'I'.
    p_datum-option = 'BT'.
    append p_datum.

endform.                    " init_variable
