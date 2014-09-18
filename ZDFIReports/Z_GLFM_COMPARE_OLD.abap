*
* Andy Choi
* - 2003.04
*
REPORT Z_GLFM_COMPARE LINE-SIZE 135
                      LINE-COUNT 58
                      NO STANDARD PAGE HEADING.


tables: t001, skb1, bsis, bseg, FMIFIIT, bkpf.


parameters: p_bukrs like bsis-bukrs memory id BUK,
            p_gjahr like bsis-gjahr.
select-options: p_MONAT for bsis-MONAT,
                p_budat for bsis-budat.

parameters: p_edit as checkbox.

*p_hkont for bsis-hkont,
DATA: BEGIN OF ifm OCCURS 0,
         ZHLDT   LIKE fmifiit-ZHLDT,
         cnt     type  i,
         FIPEX   LIKE fmifiit-FIPEX,
         SAKNR   LIKE fmifiit-HKONT,
         GJAHR   like fmifiit-GJAHR,
         VOBELNR like fmifiit-vobelnr,
         VOGJAHR like fmifiit-vogjahr,
         KNBELNR LIKE fmifiit-KNBELNR,
         TRBTR   LIKE fmifiit-TRBTR,
         match(1) type c,
      END OF ifm.

DATA: BEGIN OF ifm2 OCCURS 0,
         VOBELNR like fmifiit-vobelnr,
         VOGJAHR like fmifiit-vogjahr,
         ZHLDT   LIKE fmifiit-ZHLDT,
         cnt     type  i,
         FIPEX   LIKE fmifiit-FIPEX,
         SAKNR   LIKE fmifiit-HKONT,
         GJAHR   like fmifiit-GJAHR,
         KNBELNR LIKE fmifiit-KNBELNR,
         TRBTR   LIKE fmifiit-TRBTR,
         match(1) type c,
      END OF ifm2.

data: ibsis like bsis occurs 0 with header line.
DATA: BEGIN OF itab OCCURS 0,
         BUDAT  LIKE  BSIS-BUDAT,
         cnt    type  i,
         HKONT  LIKE  BSIS-HKONT,
         BELNR  LIKE  BSIS-BELNR,
         dmbtr  like  bseg-DMBTR,
         BUZEI  LIKE  BSIS-BUZEI,
         VALUT  LIKE  BSIS-VALUT,
         SHKZG  like  bseg-SHKZG,
         blart  like  bkpf-blart,
     END OF itab.


DATA: BEGIN OF ifmcash OCCURS 0,
         saknr  like skb1-saknr,
      END OF ifmcash.

DATA : BEGIN OF ifmfpo OCCURS 0,
         fipos    LIKE fmfpo-fipos,
       END OF ifmfpo.

DATA: CURSORFIELD(20).
ranges: r_saknr for bsis-hkont.
data: l_TRBTR  like ifm-TRBTR,
      l_TRBTR2 like ifm-TRBTR.

*----------------------------------------------------------
* Start-of-Selection
*----------------------------------------------------------
perform read_master.
perform read_fi.
perform read_fm.

*----------------------------------------------------------
* End-of-Selection
*----------------------------------------------------------
perform glfm_match.
perform display_data.


*&---------------------------------------------------------------------*
AT LINE-SELECTION.
  SET PARAMETER ID 'BUK' FIELD p_bukrs.
  SET PARAMETER ID 'GJR' FIELD p_gjahr.

  GET CURSOR FIELD CURSORFIELD.


  if CURSORFIELD = 'ITAB-BELNR'.
    SET PARAMETER ID 'BLN' FIELD itab-belnr.
    if p_edit = 'X'.
      CALL TRANSACTION 'FB02' AND SKIP FIRST SCREEN.
    else.
      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
    endif.
  elseif CURSORFIELD = 'IFM-KNBELNR'.
    SET PARAMETER ID 'BLN' FIELD ifm-knbelnr.
*      SET PARAMETER ID 'BLP' FIELD HDTAB-BELNR.
    CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
  elseif CURSORFIELD = 'IFM-VOBELNR'.
    SET PARAMETER ID 'BLN' FIELD ifm-vobelnr.
    CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
  elseif CURSORFIELD = 'IFM2-KNBELNR'.
    SET PARAMETER ID 'BLN' FIELD ifm2-knbelnr.
*      SET PARAMETER ID 'BLP' FIELD HDTAB-BELNR.
    CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
  elseif CURSORFIELD = 'IFM2-VOBELNR'.
    SET PARAMETER ID 'BLN' FIELD ifm2-vobelnr.
    CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
  endif.

************************************************************************
***  (SHIFT+PF1) Execute FMPY
************************************************************************
AT pf13.
  PERFORM bkpf_fmpy_change.

************************************************************************
***  (SHIFT+PF4) Release simulation
************************************************************************
AT PF16.

************************************************************************
***  (SHIFT+PF5) Release
************************************************************************
AT PF17.

*&---------------------------------------------------------------------*
*&      Form  print_fm_line
*&---------------------------------------------------------------------*
FORM print_fm_line.
  data: ll_amt like ifm-trbtr.
  clear ll_amt.

  refresh ifm2.
  loop at ifm where zhldt = itab-budat
                and cnt   = 0.
    move-corresponding ifm to ifm2. append ifm2.
  endloop.

  sort ifm2 by vobelnr.
  loop at ifm2.
    select single * from bkpf
      where bukrs = p_bukrs
        and gjahr = ifm2-gjahr
        and belnr = ifm2-knbelnr.

    write:/ 'FM:' color col_key.
    format color col_background.
    write:70  ifm2-TRBTR,
          87 ifm2-VOBELNR, ifm2-KNBELNR, bkpf-blart,
             ifm2-zhldt, ifm2-FIPEX.
    hide: ifm2-knbelnr, ifm2-vobelnr.
    l_trbtr = l_trbtr + ifm2-trbtr.

    ll_amt = ll_amt + ifm2-trbtr.

* summ by payment document
    at end of VOBELNR.
      write:55 ll_amt  color col_key.

      clear: ll_amt.
    endat.
  endloop.

ENDFORM.                    " print_fm_line
*&---------------------------------------------------------------------*
*&      Form  glfm_match
*&---------------------------------------------------------------------*
FORM glfm_match.
  data: l_ln type i.


  loop at itab.
    l_ln = sy-tfill. " save current line
    loop at ifm where zhldt = itab-budat
                  and trbtr = itab-dmbtr.

      check ifm-cnt  = 0.
      ifm-cnt = itab-cnt.
      modify ifm index sy-tabix.
      exit.

    endloop.

    sy-tfill = l_ln. " restore current line
  endloop.
ENDFORM.                    " glfm_match
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
FORM display_data.
  loop at itab.
    at new budat.
      format color col_key.
      write:/ '***Date: ', itab-budat.
    endat.

    format color col_background.
    write:/ itab-budat, itab-hkont, itab-belnr, itab-buzei, itab-DMBTR.
    hide: itab-belnr.

    loop at ifm where cnt = itab-cnt.
      if SY-index > 0.  " current line > 0
        write:/ 'FM:'.
      endif.
      select single * from bkpf
        where bukrs = p_bukrs
          and gjahr = ifm-gjahr
          and belnr = ifm-knbelnr.

      write:55 ifm-TRBTR,
            87 ifm-VOBELNR, ifm-KNBELNR,
               bkpf-blart, ifm-zhldt, ifm-FIPEX.
      hide: ifm-knbelnr, ifm-vobelnr.
      l_trbtr = l_trbtr + ifm-trbtr.
    endloop.

    at end of budat.
      sum.

      perform print_fm_line.

      format color col_total.
      write:/38 itab-dmbtr,
             55 l_trbtr.
      l_trbtr2 = l_trbtr2 + l_trbtr.
      clear: l_TRBTR.

      uline.
    endat.

    at last.
      sum.
      uline.
      format color col_total.
      write:/'Total:',
       38 itab-dmbtr,
       55 L_TRBTR2.

*     write:/ itab-budat, itab-hkont, itab-belnr, itab-buzei, itab-DMBTR
*      write:/ 'FM_TOT:                                           ',
*              l_TRBTR2, ifm-zhldt, ifm-FIPEX, ifm-KNBELNR.

    endat.
  endloop.

  clear: ifm-knbelnr, itab-belnr.

ENDFORM.                    " display_data
*&---------------------------------------------------------------------*
*&      Form  read_fi
*&---------------------------------------------------------------------*
FORM read_fi.
  tables: bsas.

  select * from bsas
    appending corresponding fields of table ibsis
    FOR ALL ENTRIES IN ifmcash
    where bukrs = p_bukrs
      and gjahr = p_gjahr
      and MONAT in p_MONAT
      and hkont = ifmcash-saknr
      and budat in p_budat.

  select * from bsis
    appending corresponding fields of table ibsis
    FOR ALL ENTRIES IN ifmcash
    where bukrs = p_bukrs
      and gjahr = p_gjahr
      and MONAT in p_MONAT
      and hkont = ifmcash-saknr
      and budat in p_budat.


  perform move_glitab.

  sort itab by budat belnr   buzei.

* delete multiple line
*  data: itmp like itab.
*  clear itmp.
*  loop at itab.
*    if itmp-belnr <> itab-belnr and
*       itmp-buzei <> itab-buzei.
*      move-corresponding itab to itmp.
*      continue.
*    endif.
*
*    if itmp-belnr = itab-belnr.
*      delete itab.
*    else.
*      move-corresponding itab to itmp.
*    endif.
*  endloop.


ENDFORM.                    " read_fi
*&---------------------------------------------------------------------*
*&      Form  read_fm
*&---------------------------------------------------------------------*
FORM read_fm.
  select * from fmifiit
     into CORRESPONDING FIELDS OF TABLE ifm
     where bukrs = p_bukrs
       and gjahr = p_gjahr
       and perio in p_monat
       and ( WRTTP = '57' or WRTTP = '61' )
       and zhldt in p_budat.

  sort ifm  by ZHLDT vobelnr knbelnr saknr.


*     and BTART = '0250'.  " payment
* downpayment create 2 lineitem - downpay(100) & payment(250)

ENDFORM.                    " read_fm
*&---------------------------------------------------------------------*
*&      Form  read_master
*&---------------------------------------------------------------------*
FORM read_master.
  select single * from t001 where bukrs = p_bukrs.

  SELECT * into corresponding fields of table ifmfpo
       FROM fmfpo
       WHERE fikrs =  t001-fikrs
         and FIVOR = '90'.

  select * into corresponding fields of table ifmcash
    from skb1
    for all entries in ifmfpo
    where bukrs = p_bukrs
        and fipos = ifmfpo-fipos.

  r_saknr-sign = 'I'.
  r_saknr-option = 'EQ'.
  loop at ifmcash.
    r_saknr-low = ifmcash-saknr. append r_saknr.
  endloop.

ENDFORM.                    " read_master
*&---------------------------------------------------------------------*
*&      Form  move_glitab
*&---------------------------------------------------------------------*
FORM move_glitab.
  data:g_cnt type i value 0.
  data:g_skip type i.
  loop at ibsis.
    select single * from bkpf
           where bukrs = p_bukrs
             and gjahr = p_gjahr
             and belnr = ibsis-belnr.
    check bkpf-STBLG = space.

    clear g_skip.
    select * from bseg
       where bukrs = p_bukrs
         and gjahr = p_gjahr
         and belnr = ibsis-belnr
         and buzei <> ibsis-buzei.
      if bseg-hkont in r_saknr.
        g_skip = 1.
        exit.
      endif.
    endselect.
    check g_skip = 0.

*     check not itab-hkont in r_saknr.
    itab-dmbtr = ibsis-dmbtr.
    itab-hkont = ibsis-hkont.
    itab-belnr = ibsis-belnr.
    itab-buzei = ibsis-buzei.
    itab-shkzg = ibsis-shkzg.
    itab-valut = ibsis-valut.
    itab-blart = ibsis-blart.
    itab-budat = ibsis-budat.

    if itab-valut is initial.
      itab-valut = itab-budat.
    endif.
    if ibsis-SHKZG = 'H'.
      itab-dmbtr = itab-dmbtr * -1.
    endif.

* line count
    g_cnt = g_cnt + 1.
    itab-cnt = g_cnt.

    append itab.
*    endselect.
  endloop.
ENDFORM.                    " move_glitab
*&---------------------------------------------------------------------*
*&      Form  bkpf_fmpy_change
*&---------------------------------------------------------------------*
FORM bkpf_fmpy_change.
  data: l_bktxt like bkpf-bktxt.

  GET CURSOR FIELD CURSORFIELD.
  check CURSORFIELD = 'ITAB-BELNR'.

  select single * from bkpf
     where bukrs = p_bukrs
       and gjahr = p_gjahr
       and belnr = itab-belnr.

  if bkpf-bktxt(4) = 'FMPY'.
  else.
    concatenate 'FMPY' bkpf-bktxt into l_bktxt.

    perform call_fb02 using itab-belnr l_bktxt.
    SUBMIT RFFMRPFI
            WITH P_BUKRS  = p_bukrs
            WITH P_GJAHR  = p_gjahr
            WITH P_LIST   = 'X'
            WITH P_LOESCH = 'X'
            WITH P_PRUEF  = ' '
            WITH P_TEST   = ' '
            WITH SO_BELNR-low = itab-belnr
            and return.

  endif.
ENDFORM.                    " bkpf_fmpy_change
*&---------------------------------------------------------------------*
*&      Form  call_fb02
*&---------------------------------------------------------------------*
FORM call_fb02 using f_belnr f_bktxt.
  perform bdc_dynpro      using 'SAPMF05L' '0100'.
  perform bdc_field       using 'BDC_CURSOR'    'RF05L-GJAHR'.
  perform bdc_field       using 'BDC_OKCODE'    '/00'.
  perform bdc_field       using 'RF05L-BELNR'   f_belnr.
  perform bdc_field       using 'RF05L-BUKRS'   p_bukrs.
  perform bdc_field       using 'RF05L-GJAHR'   p_gjahr.
  perform bdc_dynpro      using 'SAPMF05L'      '0700'.
  perform bdc_field       using 'BDC_CURSOR'    'BKPF-BELNR'.
  perform bdc_field       using 'BDC_OKCODE'    '=VK'.
  perform bdc_dynpro      using 'SAPMF05L'      '1710'.
  perform bdc_field       using 'BDC_CURSOR'    'BKPF-BKTXT'.
  perform bdc_field       using 'BDC_OKCODE'    '=ENTR'.
  perform bdc_field       using 'BKPF-BKTXT'    f_bktxt.
*perform bdc_field       using 'BKPF-XBLNR'    record-XBLNR_005.
  perform bdc_dynpro      using 'SAPMF05L'      '0700'.
  perform bdc_field       using 'BDC_CURSOR'    'BKPF-BELNR'.
  perform bdc_field       using 'BDC_OKCODE'    '=AE'.
  perform bdc_transaction using 'FB02'.

ENDFORM.                                                    " call_fb02
*----------------------------------------------------------------------*
*   open dataset                                                       *
*----------------------------------------------------------------------*
FORM OPEN_DATASET USING P_DATASET.
  OPEN DATASET P_DATASET IN TEXT MODE.
  IF SY-SUBRC <> 0.
    WRITE: / TEXT-E00, SY-SUBRC.
    STOP.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
*   close dataset                                                      *
*----------------------------------------------------------------------*
FORM CLOSE_DATASET USING P_DATASET.
  CLOSE DATASET P_DATASET.
ENDFORM.

**----------------------------------------------------------------------
*
**   create batchinput session
*
**   (not for call transaction using...)
*
**----------------------------------------------------------------------
*
*FORM OPEN_GROUP.
*  IF SESSION = 'X'.
*    SKIP.
*    WRITE: /(20) 'Create group'(I01), GROUP.
*    SKIP.
**   open batchinput group
*    CALL FUNCTION 'BDC_OPEN_GROUP'
*         EXPORTING
*              CLIENT   = SY-MANDT
*              GROUP    = GROUP
*              USER     = USER
*              KEEP     = KEEP
*              HOLDDATE = HOLDDATE.
*    WRITE: /(30) 'BDC_OPEN_GROUP'(I02),
*            (12) 'returncode:'(I05),
*                 SY-SUBRC.
*  ENDIF.
*ENDFORM.

**----------------------------------------------------------------------
*
**   end batchinput session
*
**   (call transaction using...: error session)
*
**----------------------------------------------------------------------
*
*FORM CLOSE_GROUP.
*  IF SESSION = 'X'.
**   close batchinput group
*    CALL FUNCTION 'BDC_CLOSE_GROUP'.
*    WRITE: /(30) 'Close session',
*            (12) 'Return code =',
*                 SY-SUBRC.
*  ELSE.
*    IF E_GROUP_OPENED = 'X'.
*      CALL FUNCTION 'BDC_CLOSE_GROUP'.
*      WRITE: /.
*      WRITE: /(30) 'Error session created'.
*    ENDIF.
*  ENDIF.
*ENDFORM.

data: session, smalllog, ctu, group, E_GROUP_OPENED,
      user, keep, holddate.
data: ctumode(1) type c value 'E'.
DATA:   BDCDATA LIKE BDCDATA    OCCURS 0 WITH HEADER LINE.
DATA:   MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.
tables: t100.
*----------------------------------------------------------------------*
*        Start new transaction according to parameters                 *
*----------------------------------------------------------------------*
FORM BDC_TRANSACTION USING TCODE.

  DATA: L_MSTRING(480).
  DATA: L_SUBRC LIKE SY-SUBRC.

* batch input session
  IF SESSION = 'X'.
    CALL FUNCTION 'BDC_INSERT'
         EXPORTING
              TCODE     = TCODE
         TABLES
              DYNPROTAB = BDCDATA.
    IF SMALLLOG <> 'X'.
      WRITE: / 'Insert transaction',
               TCODE,
               'Return code =',
               SY-SUBRC,
               'RECORD:',
               SY-INDEX.
    ENDIF.
* call transaction using
  ELSE.
    REFRESH MESSTAB.
    CALL TRANSACTION TCODE USING BDCDATA
                     MODE   'E'.
*                     UPDATE CUPDATE
*                     MESSAGES INTO MESSTAB.
    L_SUBRC = SY-SUBRC.
    IF SMALLLOG <> 'X'.
      format color COL_KEY.
      WRITE: / 'Return code =',
               L_SUBRC,
               'RECORD:',
               SY-INDEX.
      format color COL_NORMAL.
      LOOP AT MESSTAB.
        SELECT SINGLE * FROM T100 WHERE SPRSL = MESSTAB-MSGSPRA
                                  AND   ARBGB = MESSTAB-MSGID
                                  AND   MSGNR = MESSTAB-MSGNR.
        IF SY-SUBRC = 0.
          L_MSTRING = T100-TEXT.
          IF L_MSTRING CS '&1'.
            REPLACE '&1' WITH MESSTAB-MSGV1 INTO L_MSTRING.
            REPLACE '&2' WITH MESSTAB-MSGV2 INTO L_MSTRING.
            REPLACE '&3' WITH MESSTAB-MSGV3 INTO L_MSTRING.
            REPLACE '&4' WITH MESSTAB-MSGV4 INTO L_MSTRING.
          ELSE.
            REPLACE '&' WITH MESSTAB-MSGV1 INTO L_MSTRING.
            REPLACE '&' WITH MESSTAB-MSGV2 INTO L_MSTRING.
            REPLACE '&' WITH MESSTAB-MSGV3 INTO L_MSTRING.
            REPLACE '&' WITH MESSTAB-MSGV4 INTO L_MSTRING.
          ENDIF.
          CONDENSE L_MSTRING.
          WRITE: / MESSTAB-MSGTYP, L_MSTRING(150).
        ELSE.
          WRITE: / MESSTAB.
        ENDIF.
      ENDLOOP.
      SKIP.
    ENDIF.
** Erzeugen fehlermappe ************************************************
    IF L_SUBRC <> 0 AND GROUP <> SPACE.
      IF E_GROUP_OPENED = ' '.
        CALL FUNCTION 'BDC_OPEN_GROUP'
             EXPORTING
                  CLIENT   = SY-MANDT
                  GROUP    = GROUP
                  USER     = USER
                  KEEP     = KEEP
                  HOLDDATE = HOLDDATE.
        E_GROUP_OPENED = 'X'.
      ENDIF.
      CALL FUNCTION 'BDC_INSERT'
           EXPORTING
                TCODE     = TCODE
           TABLES
                DYNPROTAB = BDCDATA.
    ENDIF.
  ENDIF.
  REFRESH BDCDATA.
ENDFORM.

*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM BDC_DYNPRO USING PROGRAM DYNPRO.
  CLEAR BDCDATA.
  BDCDATA-PROGRAM  = PROGRAM.
  BDCDATA-DYNPRO   = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.
ENDFORM.

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM BDC_FIELD USING FNAM FVAL.
  data: NODATA value ''.
  IF FVAL <> NODATA.
    CLEAR BDCDATA.
    BDCDATA-FNAM = FNAM.
    BDCDATA-FVAL = FVAL.
    APPEND BDCDATA.
  ENDIF.
ENDFORM.
