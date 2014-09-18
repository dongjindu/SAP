*&---------------------------------------------------------------------*
*& Program ID     : ZFMR0004                                           *
*& Program Name   : IM/FM Acutal Data in FI(BSEG)                      *
*& Created by     : YN.Kim                                             *
*& Created on     : 08/22/2011                                         *
*& Reference Pgm  :                                                    *
*&                                                                     *
*& Modification Log                                                    *
*----------------------------------------------------------------------*
* DATE      |  NAME          |Transport | Issue #  |      DESC         *
*----------------------------------------------------------------------*
*                                                                      *
*&=====================================================================*
REPORT  ZFMR0004    MESSAGE-ID zmfi.

*----------------------------------------------------------------------*
*   Macro
*----------------------------------------------------------------------*



*----------------------------------------------------------------------*
*   Data Definition
*----------------------------------------------------------------------*
** Type-Pools
TYPE-POOLS : slis.

INCLUDE zfm_auth_form.

type-pools zfmcm.

TABLES: bkpf,
        bseg,
        t001,
        fmifihd,
        fmifiit,
        fmci,
        skat,
        aufk,
        fmcit,
        fmfctrt,
        fmfint.

INCLUDE <icon>.

*---// Constants
CONSTANTS:
  c_bukrs_ha01         LIKE bkpf-bukrs      VALUE zfmcm_fm_area,
  c_first_button                            VALUE '1',
  c_second_button                           VALUE '2',
  c_cancel_button                           VALUE 'A',
  c_true                                    VALUE 'X'.

*---// Internal table.
DATA : BEGIN OF it_list OCCURS 0,
         monat  LIKE bkpf-monat,
         hkont  LIKE bseg-hkont,
         txt01  LIKE skat-txt20,
         dmbtr  LIKE bseg-dmbtr,
         wrbtr  LIKE bseg-wrbtr,
         pswbt  LIKE bseg-pswbt,
         pswsl  LIKE bseg-pswsl,
         dmbe2  LIKE bseg-dmbe2,
         hwae2  LIKE bkpf-hwae2,
         aufnr  LIKE bseg-aufnr,
         txt02  LIKE aufk-ktext,
         anln1  LIKE bseg-anln1,
         anln2  LIKE bseg-anln2,
         belnr  LIKE bseg-belnr,
         blart  LIKE bkpf-blart,
         bldat  LIKE bkpf-bldat,
         budat  LIKE bkpf-budat,
         waers  LIKE bkpf-waers,
         kurs2  LIKE bkpf-kurs2,
         fipos  LIKE bseg-fipos,
         txt03  LIKE fmcit-bezei,
         fistl  LIKE bseg-fistl,
         txt04  LIKE fmfctrt-bezeich,
         geber  LIKE bseg-geber,
         txt05  LIKE fmfint-bezeich,
         fmbelnr LIKE fmifiit-fmbelnr,
         fipex  LIKE fmifiit-fipex,
         txt06  LIKE fmcit-bezei,
         mfistl LIKE fmifiit-fistl,
         txt07  LIKE fmfctrt-bezeich,
         fonds  LIKE fmifiit-fonds,
         txt08  LIKE fmfint-bezeich,
         stats  LIKE fmifiit-stats,
         wrttp  LIKE fmifiit-wrttp,
         fkbtr  LIKE fmifiit-fkbtr,
         trbtr  LIKE fmifiit-trbtr,
         vrefbn LIKE fmifiit-vrefbn,
         lwaer  LIKE t001-waers,
       END   OF it_list.

DATA: BEGIN OF it_bkpf OCCURS 0,
        belnr  LIKE bkpf-belnr,
        gjahr  LIKE bkpf-gjahr,
        blart  LIKE bkpf-blart,
        bldat  LIKE bkpf-bldat,
        budat  LIKE bkpf-budat,
        monat  LIKE bkpf-monat,
        cpudt  LIKE bkpf-cpudt,
        aedat  LIKE bkpf-aedat,
        upddt  LIKE bkpf-upddt,
        usnam  LIKE bkpf-usnam,
        tcode  LIKE bkpf-tcode,
        waers  LIKE bkpf-waers,
        hwaer  LIKE bkpf-hwaer,
        hwae2  LIKE bkpf-hwae2,
        kurs2  LIKE bkpf-kurs2,
      END   OF it_bkpf.

DATA: BEGIN OF it_bseg OCCURS 0,
        belnr  LIKE bkpf-belnr,
        gjahr  LIKE bkpf-gjahr,
        buzei  LIKE bseg-buzei,
        hkont  LIKE bseg-hkont,
        dmbtr  LIKE bseg-dmbtr,
        wrbtr  LIKE bseg-wrbtr,
        pswbt  LIKE bseg-pswbt,
        pswsl  LIKE bseg-pswsl,
        dmbe2  LIKE bseg-dmbe2,
        aufnr  LIKE bseg-aufnr,
        anln1  LIKE bseg-anln1,
        anln2  LIKE bseg-anln2,
        fistl  LIKE bseg-fistl,
        fipos  LIKE bseg-fipos,
        geber  LIKE bseg-geber,
      END   OF it_bseg.

DATA: BEGIN OF it_fidoc OCCURS 0,
        belnr  LIKE bkpf-belnr,
        gjahr  LIKE bkpf-gjahr,
        buzei  LIKE bseg-buzei,
      END   OF it_fidoc.

DATA: BEGIN OF it_fmit OCCURS 0,
        knbelnr  LIKE fmifiit-knbelnr,
        kngjahr  LIKE fmifiit-kngjahr,
        knbuzei  LIKE fmifiit-knbuzei,
        fmbelnr  LIKE fmifiit-fmbelnr,
        perio    LIKE fmifiit-perio,
        fistl    LIKE fmifiit-fistl,
        fonds    LIKE fmifiit-fonds,
        fipex    LIKE fmifiit-fipex,
        wrttp    LIKE fmifiit-wrttp,
        trbtr    LIKE fmifiit-trbtr,
        fkbtr    LIKE fmifiit-fkbtr,
        stats    LIKE fmifiit-stats,
        twaer    LIKE fmifiit-twaer,
        hkont    LIKE fmifiit-hkont,
        vogjahr  LIKE fmifiit-vogjahr,
        vobelnr  LIKE fmifiit-vobelnr,
        vobuzei  LIKE fmifiit-vobuzei,
        vrefbt   LIKE fmifiit-vrefbt,
        vrefbn   LIKE fmifiit-vrefbn,
      END   OF it_fmit.

*---// Global Variable.

*---// for ALV
DATA : it_fieldcat          TYPE slis_t_fieldcat_alv,
       wa_layout            TYPE slis_layout_alv,
       wa_fieldcat          LIKE LINE OF it_fieldcat.
DATA : gv_repid LIKE sy-repid.
DATA : gv_col_pos TYPE i.
DATA : it_sort         TYPE slis_t_sortinfo_alv WITH HEADER LINE ,
       it_top_page     TYPE slis_t_listheader.

DATA : l_fund TYPE bp_geber VALUE '',
       l_fictr TYPE fistl VALUE '',
       l_fipex TYPE fm_fipex VALUE ''.

*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.

PARAMETERS : p_bukrs LIKE bkpf-bukrs     MEMORY ID buk  OBLIGATORY
                                         DEFAULT c_bukrs_ha01
                                         VALUE CHECK.
SELECT-OPTIONS: s_fistl  FOR fmifiit-fistl,
                s_fipex  FOR fmci-fipex,
                s_fonds  FOR fmifiit-fonds.

PARAMETERS : p_gjahr LIKE bkpf-gjahr     OBLIGATORY.

SELECT-OPTIONS: s_monat  FOR bkpf-monat  OBLIGATORY
                                         NO-EXTENSION.
*PARAMETERS : p_monat  LIKE bkpf-monat.

*PARAMETERS : p_sumup AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK bl1.


*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
INITIALIZATION.
  SET PARAMETER ID 'FIC' FIELD l_fund.
  SET PARAMETER ID 'FIS' FIELD l_fictr.
  SET PARAMETER ID 'FPS' FIELD l_fipex.
  PERFORM init.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

AT SELECTION-SCREEN.

  CLEAR t001.
  SELECT SINGLE * FROM t001 WHERE bukrs = p_bukrs.
  IF sy-subrc <> 0.
    MESSAGE e000 WITH text-m01.
  ENDIF.

*  if s_fistl[] is initial and
*     s_fipex[] is initial.
*    message e000 with text-m03.
*  endif.
*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
*  PERFORM user_auth_check TABLES s_fistl[]
*                          USING p_bukrs.
  PERFORM user_auth_check_geber TABLES s_fonds[]
                                       s_fistl[]
                                USING p_bukrs.

  IF NOT g_auth_check IS INITIAL.
    LEAVE TO SCREEN 0.
  ENDIF.
  PERFORM read_rtn.

*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.
  READ TABLE it_list INDEX 1.
  IF sy-subrc <> 0.
    MESSAGE s000 WITH text-m02.
    EXIT.
  ENDIF.

* Preparation of ALV
  PERFORM pre_report_adj.
* Call ALV LIST
  PERFORM call_alv_list.

*----------------------------------------------------------------------*
* Sub-Routine
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  init
*&---------------------------------------------------------------------*
FORM init .

  p_gjahr = sy-datum+0(4).

  s_monat     = 'IEQ'.
  s_monat-low = sy-datum+4(2).
  S_MONAT-HIGH = SY-DATUM+4(2).
  APPEND s_monat.

*  p_monat = sy-datum+4(2).

ENDFORM.                    " init
*&---------------------------------------------------------------------*
*&      Form  read_rtn
*&---------------------------------------------------------------------*
FORM read_rtn .

  PERFORM select_bkpf.
  PERFORM select_bseg.
  PERFORM select_fmifiit.
  PERFORM merge_data.

ENDFORM.                    " read_rtn
*&---------------------------------------------------------------------*
*&      Form  select_bkpf
*&---------------------------------------------------------------------*
FORM select_bkpf .

  CLEAR: it_bkpf, it_bkpf[].

  SELECT  belnr gjahr blart bldat budat
          monat cpudt aedat upddt usnam
          tcode waers hwaer hwae2 kurs2
    INTO  CORRESPONDING FIELDS OF TABLE it_bkpf
    FROM  bkpf
   WHERE  bukrs  =  p_bukrs
     AND  gjahr  =  p_gjahr
     AND  monat  IN s_monat.
*     AND  monat  =  p_monat.

ENDFORM.                    " select_bkpf
*&---------------------------------------------------------------------*
*&      Form  select_bseg
*&---------------------------------------------------------------------*
FORM select_bseg .

  DATA : lt_bseg LIKE it_bseg OCCURS 0 WITH HEADER LINE.

  CLEAR: it_bseg, it_bseg[].

  LOOP AT it_bkpf.

    CLEAR: lt_bseg, lt_bseg[].

    SELECT  belnr gjahr buzei hkont dmbtr
            wrbtr pswbt pswsl dmbe2 aufnr
            anln1 anln2 fistl fipos geber
      INTO  CORRESPONDING FIELDS OF TABLE lt_bseg
      FROM  bseg
     WHERE  bukrs  =  p_bukrs
       AND  belnr  =  it_bkpf-belnr
       AND  gjahr  =  p_gjahr
       AND  fistl  IN s_fistl
       AND  fipos  IN s_fipex
       AND  geber  IN s_fonds.

    IF sy-subrc <> 0.
      DELETE it_bkpf.
    ELSE.
      APPEND LINES OF lt_bseg TO it_bseg.
    ENDIF.

    CLEAR it_bkpf.

  ENDLOOP.

*> move document number, line item, Document's year.
  LOOP AT it_bseg.
    MOVE: it_bseg-belnr    TO it_fidoc-belnr,
          it_bseg-gjahr    TO it_fidoc-gjahr,
          it_bseg-buzei    TO it_fidoc-buzei.
    APPEND it_fidoc.  CLEAR it_fidoc.
    CLEAR it_bseg.
  ENDLOOP.

  SORT it_bkpf BY belnr.
  SORT it_bseg BY belnr buzei.
  SORT it_fidoc BY belnr buzei.

ENDFORM.                    " select_bseg
*&---------------------------------------------------------------------*
*&      Form  select_fmifiit
*&---------------------------------------------------------------------*
FORM select_fmifiit .

  CLEAR: it_fmit, it_fmit[].

  SELECT  fmbelnr perio   fistl   fonds   fipex
          wrttp   trbtr   fkbtr   stats   twaer
          hkont   vogjahr vobelnr vobuzei kngjahr
          knbelnr knbuzei vrefbt  vrefbn
    INTO  CORRESPONDING FIELDS OF TABLE it_fmit
    FROM  fmifiit
     FOR  ALL ENTRIES IN it_fidoc
   WHERE  knbelnr =  it_fidoc-belnr
     AND  kngjahr =  it_fidoc-gjahr
     AND  knbuzei =  it_fidoc-buzei.

  DELETE it_fmit WHERE wrttp = '60'.

  SORT it_fmit BY knbelnr kngjahr knbuzei.

ENDFORM.                    " select_fmifiit
*&---------------------------------------------------------------------*
*&      Form  merge_data
*&---------------------------------------------------------------------*
FORM merge_data .

  CLEAR: it_list, it_list[].

  LOOP AT it_bseg.

    CLEAR it_list.
    CLEAR it_bkpf.
    READ TABLE it_bkpf WITH KEY belnr = it_bseg-belnr
                                gjahr = it_bseg-gjahr
                                BINARY SEARCH.

    CLEAR it_fmit.

    MOVE-CORRESPONDING it_bseg  TO it_list.
    MOVE-CORRESPONDING it_bkpf  TO it_list.
    MOVE t001-waers             TO it_list-lwaer.

*   find account text.
    PERFORM select_acct_text USING    it_list-hkont
                             CHANGING it_list-txt01.

*   find EIO description.
    IF NOT it_list-aufnr IS INITIAL.
      PERFORM select_aufn_text USING    it_list-aufnr
                               CHANGING it_list-txt02.
    ENDIF.

*   find Commitment item text
    PERFORM select_fmit_text USING    it_list-fipos
                             CHANGING it_list-txt03.

*   find Fund center's text
    PERFORM select_fctr_text USING    it_list-fistl
                             CHANGING it_list-txt04.

*   find fund text.
    PERFORM select_fund_text USING    it_list-geber
                             CHANGING it_list-txt05.

    READ TABLE it_fmit WITH KEY knbelnr = it_bseg-belnr
                                kngjahr = it_bseg-gjahr
                                knbuzei = it_bseg-buzei
                                BINARY SEARCH.
    IF sy-subrc = 0.
      LOOP AT it_fmit FROM sy-tabix.
        IF it_fmit-knbelnr <> it_bseg-belnr OR
           it_fmit-kngjahr <> it_bseg-gjahr OR
           it_fmit-knbuzei <> it_bseg-buzei.
          EXIT.
        ENDIF.

        CLEAR: it_list-fmbelnr, it_list-fipex, it_list-mfistl,
               it_list-fonds,   it_list-stats, it_list-wrttp,
               it_list-fkbtr,   it_list-trbtr, it_list-vrefbn,
               it_list-txt06,   it_list-txt07, it_list-txt08.

        MOVE: it_fmit-fmbelnr  TO it_list-fmbelnr,
              it_fmit-fipex    TO it_list-fipex,
              it_fmit-fistl    TO it_list-mfistl,
              it_fmit-fonds    TO it_list-fonds,
              it_fmit-stats    TO it_list-stats,
              it_fmit-wrttp    TO it_list-wrttp,
              it_fmit-fkbtr    TO it_list-fkbtr,
              it_fmit-trbtr    TO it_list-trbtr,
              it_fmit-vrefbn   TO it_list-vrefbn.

        it_list-fkbtr = it_list-fkbtr * -1.
        it_list-trbtr = it_list-trbtr * -1.

*   find Commitment item text
        PERFORM select_fmit_text USING    it_list-fipex
                                 CHANGING it_list-txt06.

*   find Fund center's text
        PERFORM select_fctr_text USING    it_list-mfistl
                                 CHANGING it_list-txt07.

*   find fund text.
        PERFORM select_fund_text USING    it_list-fonds
                                 CHANGING it_list-txt08.
        APPEND it_list.
      ENDLOOP.

    ELSE.
      APPEND it_list.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " merge_data
*&---------------------------------------------------------------------*
*&      Form  pre_report_adj
*&---------------------------------------------------------------------*
FORM pre_report_adj .

  wa_layout-zebra             = 'X'.
  wa_layout-colwidth_optimize = 'X'.

  PERFORM fieldcat_init .

  it_sort-fieldname = 'MONAT'.
  it_sort-up        = 'X'.
  it_sort-expa      = ' '.
  it_sort-subtot    = ' '.
  APPEND it_sort.

  it_sort-fieldname = 'HKONT'.
  it_sort-up        = 'X'.
  it_sort-expa      = ' '.
  it_sort-subtot    = ' '.
  APPEND it_sort.

  it_sort-fieldname = 'GEBER'.
  it_sort-up        = 'X'.
  it_sort-expa      = ' '.
  it_sort-subtot    = ' '.
  APPEND it_sort.

  it_sort-fieldname = 'FISTL'.
  it_sort-up        = 'X'.
  it_sort-expa      = ' '.
  it_sort-subtot    = ' '.
  APPEND it_sort.

  it_sort-fieldname = 'FIPOS'.
  it_sort-up        = 'X'.
  it_sort-expa      = ' '.
  it_sort-subtot    = ' '.
  APPEND it_sort.

  it_sort-fieldname = 'FONDS'.
  it_sort-up        = 'X'.
  it_sort-expa      = ' '.
  it_sort-subtot    = ' '.
  APPEND it_sort.

  it_sort-fieldname = 'MFISTL'.
  it_sort-up        = 'X'.
  it_sort-expa      = ' '.
  it_sort-subtot    = ' '.
  APPEND it_sort.

  it_sort-fieldname = 'FIPEX'.
  it_sort-up        = 'X'.
  it_sort-expa      = ' '.
  it_sort-subtot    = ' '.
  APPEND it_sort.

ENDFORM.                    " pre_report_adj
*&---------------------------------------------------------------------*
*&      Form  fieldcat_init
*&---------------------------------------------------------------------*
FORM fieldcat_init .

  CLEAR : gv_col_pos, it_fieldcat, it_fieldcat[].

*  PERFORM build_fieldcat  USING
*    'IT_LIST'         'GJAHR'  'X'            space
*    space             space    '4'  'Year'
*    space             space    space          space.

  PERFORM build_fieldcat  USING
    'IT_LIST'     'MONAT'    'X'         space
    space         space      '5'         'Month'
    space         space      space       space.

  PERFORM build_fieldcat  USING
    'IT_LIST'     'HKONT'    'X'         space
    space         space      '10'        'G/L acct.'
    space         space      space       space.

  PERFORM build_fieldcat  USING
    'IT_LIST'     'TXT01'    space       space
    space         space      '20'        'G/L text.'
    space         space      space       space.

  PERFORM build_fieldcat  USING
    'IT_LIST'     'DMBTR'    space       'X'
    'LWAER'       'IT_LIST'  '17'        'AMT Locl'
    'CURR'        space      space       space.

  PERFORM build_fieldcat  USING
    'IT_LIST'     'WRBTR'    space       'X'
    'WAERS'       'IT_LIST'  '17'        'AMT Docu'
    space         space      space       space.

  PERFORM build_fieldcat  USING
    'IT_LIST'     'PSWBT'    space       'X'
    'PSWSL'       'IT_LIST'  '17'        'AMT GL'
    space         space      space       space.

  PERFORM build_fieldcat  USING
    'IT_LIST'     'DMBE2'    space       'X'
    'HWAE2'       'IT_LIST'  '17'        'AMT EURO'
    space         space      space       space.

  PERFORM build_fieldcat  USING
    'IT_LIST'     'AUFNR'    space       space
    space         space      '11'        'EIO'
    space         space      space       space.

  PERFORM build_fieldcat  USING
    'IT_LIST'     'TXT02'    space       space
    space         space      '20'        'EIO Text'
    space         space      space       space.

  PERFORM build_fieldcat  USING
    'IT_LIST'     'ANLN1'    space       space
    space         space      '12'        'Asset'
    space         space      space       space.

  PERFORM build_fieldcat  USING
    'IT_LIST'     'ANLN2'    space       space
    space         space      '5'         'A.sub'
    space         space      space       space.

  PERFORM build_fieldcat  USING
    'IT_LIST'     'BELNR'    space       space
    space         space      '10'        'FI Docu'
    space         space      space       space.

  PERFORM build_fieldcat  USING
    'IT_LIST'     'BLART'    space       space
    space         space      '7'         'Doc. Ty'
    space         space      space       space.

  PERFORM build_fieldcat  USING
    'IT_LIST'     'BLDAT'    space       space
    space         space      '10'        'Doc. Date'
    space         space      space       space.

  PERFORM build_fieldcat  USING
    'IT_LIST'     'BUDAT'    space       space
    space         space      '10'        'Post. Date'
    space         space      space       space.

  PERFORM build_fieldcat  USING
    'IT_LIST'     'WAERS'    space       space
    space         space      '8'         'Doc. Cur'
    space         space      space       space.

  PERFORM build_fieldcat  USING
    'IT_LIST'     'KURS2'    space       space
    space         space      '12'        'EXCH'
    space         space      space       space.

  PERFORM build_fieldcat  USING
    'IT_LIST'     'FIPOS'    space       space
    space         space      '14'        'Cmmit. Item'
    space         space      space       space.

  PERFORM build_fieldcat  USING
    'IT_LIST'     'TXT03'    space       space
    space         space      '20'        'C/I Text'
    space         space      space       space.

  PERFORM build_fieldcat  USING
    'IT_LIST'     'FISTL'    space       space
    space         space      '16'        'Fund CTR.'
    space         space      space       space.

  PERFORM build_fieldcat  USING
    'IT_LIST'     'TXT04'    space       space
    space         space      '20'        'FCTR Text'
    space         space      space       space.

  PERFORM build_fieldcat  USING
    'IT_LIST'     'GEBER'    'X'         space
    space         space      '10'        'EIO Fund'
    space         space      space       space.

  PERFORM build_fieldcat  USING
    'IT_LIST'     'TXT05'    space       space
    space         space      '20'        'Fund Text'
    space         space      space       space.

  PERFORM build_fieldcat  USING
    'IT_LIST'     'FMBELNR'  space       space
    space         space      '10'        'FM Docu'
    space         space      space       space.

  PERFORM build_fieldcat  USING
    'IT_LIST'     'FIPEX'    space       space
    space         space      '14'        'FM Cmit Item'
    space         space      space       space.

  PERFORM build_fieldcat  USING
    'IT_LIST'     'TXT06'    space       space
    space         space      '20'        'FM C/I Text'
    space         space      space       space.

  PERFORM build_fieldcat  USING
    'IT_LIST'     'MFISTL'   space       space
    space         space      '16'        'FM Fund CTR.'
    space         space      space       space.

  PERFORM build_fieldcat  USING
    'IT_LIST'     'TXT07'    space       space
    space         space      '20'        'FM FCTR Text'
    space         space      space       space.

  PERFORM build_fieldcat  USING
    'IT_LIST'     'FONDS'    space       space
    space         space      '10'        'FM Fund'
    space         space      space       space.

  PERFORM build_fieldcat  USING
    'IT_LIST'     'TXT08'    space       space
    space         space      '20'        'FM Fund Text'
    space         space      space       space.

  PERFORM build_fieldcat  USING
    'IT_LIST'     'STATS'    space       space
    space         space      '5'         'Stat.'
    space         space      space       space.

  PERFORM build_fieldcat  USING
    'IT_LIST'     'WRTTP'    space       space
    space         space      '10'        'Val. Ty'
    space         space      space       space.

  PERFORM build_fieldcat  USING
    'IT_LIST'     'FKBTR'    space       'X'
    'HWAE2'       'IT_LIST'  '16'        'AMT EURO'
    space         space      space       space.

  PERFORM build_fieldcat  USING
    'IT_LIST'     'TRBTR'    space       'X'
    'WAERS'       'IT_LIST'  '16'        'AMT Trans'
    space         space      space       space.

  PERFORM build_fieldcat  USING
    'IT_LIST'     'VREFBN'   space       space
    space         space      '10'        'PO no.'
    space         space      space       space.


ENDFORM.                    " fieldcat_init

*&---------------------------------------------------------------------*
*&      Form  Build_FieldCAT
*&---------------------------------------------------------------------*
FORM build_fieldcat USING    value(p_0100)
                             value(p_0101)
                             value(p_0102)
                             value(p_0103)
                             value(p_0104)
                             value(p_0105)
                             value(p_0106)
                             value(p_0107)
                             value(p_0108)
                             value(p_0109)
                             value(p_0110)
                             value(p_0111).


  ADD 1 TO gv_col_pos.
  wa_fieldcat-tabname     = p_0100.
  wa_fieldcat-fieldname   = p_0101.
  wa_fieldcat-key         = p_0102.
  wa_fieldcat-do_sum      = p_0103.
  wa_fieldcat-cfieldname  = p_0104.
  wa_fieldcat-ctabname    = p_0105.
  wa_fieldcat-outputlen   = p_0106.
  wa_fieldcat-seltext_l   = p_0107.
  wa_fieldcat-datatype    = p_0108.
  wa_fieldcat-qfieldname  = p_0109.
  wa_fieldcat-qtabname    = p_0110.
  wa_fieldcat-col_pos     = gv_col_pos.
*  WA_FIELDCAT-OFFSET      = P_0111.
  wa_fieldcat-decimals_out      = p_0111.

  IF p_0101 = 'MATNR'.
    wa_fieldcat-ref_tabname   = 'ZTCO0013'.
    wa_fieldcat-ref_fieldname = 'MATN1'.
  ELSEIF p_0101 = 'SHOPT'.
    wa_fieldcat-ref_tabname   = 'ZTCO0013'.
    wa_fieldcat-ref_fieldname = 'SHOPT'.
  ENDIF.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

ENDFORM.                    " Build_FieldCAT
*&---------------------------------------------------------------------*
*&      Form  call_alv_list
*&---------------------------------------------------------------------*
FORM call_alv_list .

  gv_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program                = gv_repid
*     I_CALLBACK_PF_STATUS_SET          = slis_ev_pf_status_set
     i_callback_user_command           = slis_ev_user_command
*      i_callback_top_of_page            = slis_ev_top_of_page
      is_layout                         = wa_layout
      it_fieldcat                       = it_fieldcat[]
      it_sort                           = it_sort[]
      i_save                            = 'A'
    TABLES
      t_outtab                          = it_list
    EXCEPTIONS
      program_error                     = 1
      OTHERS                            = 2
            .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " call_alv_list
*&---------------------------------------------------------------------*
*&      Form  select_acct_text
*&---------------------------------------------------------------------*
FORM select_acct_text  USING    p_hkont
                       CHANGING p_text.

  CLEAR p_text.

  SELECT SINGLE txt20 INTO p_text
         FROM skat WHERE spras = sy-langu
                     AND ktopl = zfmcm_ktopl
                     AND saknr = p_hkont.
  IF sy-subrc <> 0 AND sy-langu <> 'E'.
    SELECT SINGLE txt20 INTO p_text
           FROM skat WHERE spras = 'E'
                       AND ktopl = zfmcm_ktopl
                       AND saknr = p_hkont.
  ENDIF.

ENDFORM.                    " select_acct_text
*&---------------------------------------------------------------------*
*&      Form  select_aufn_text
*&---------------------------------------------------------------------*
FORM select_aufn_text  USING    p_aufnr
                       CHANGING p_text.

  CLEAR p_text.
  SELECT SINGLE ktext INTO p_text
         FROM aufk
         WHERE aufnr = p_aufnr.

ENDFORM.                    " select_aufn_text
*&---------------------------------------------------------------------*
*&      Form  select_fmit_text
*&---------------------------------------------------------------------*
FORM select_fmit_text  USING    p_fipex
                       CHANGING p_text.

  CLEAR p_text.
  SELECT SINGLE bezei INTO p_text
         FROM fmcit WHERE spras = sy-langu
                      AND fikrs = p_bukrs
                      AND fipex = p_fipex.

  IF sy-subrc <> 0 AND sy-langu <> 'E'.
    SELECT SINGLE bezei INTO p_text
           FROM fmcit WHERE spras = 'E'
                        AND fikrs = p_bukrs
                        AND fipex = p_fipex.
  ENDIF.

ENDFORM.                    " select_fmit_text
*&---------------------------------------------------------------------*
*&      Form  select_fctr_text
*&---------------------------------------------------------------------*
FORM select_fctr_text  USING    p_fictr
                       CHANGING p_text.

  CLEAR p_text.
  SELECT SINGLE bezeich INTO p_text
         FROM fmfctrt WHERE spras = sy-langu
                        AND fikrs = p_bukrs
                        AND fictr = p_fictr
                        AND datbis = '99991231'.

  IF sy-subrc <> 0 AND sy-langu <> 'E'.
    SELECT SINGLE bezeich INTO p_text
           FROM fmfctrt WHERE spras = 'E'
                          AND fikrs = p_bukrs
                          AND fictr = p_fictr
                          AND datbis = '99991231'.
  ENDIF.


ENDFORM.                    " select_fctr_text
*&---------------------------------------------------------------------*
*&      Form  select_fund_text
*&---------------------------------------------------------------------*
FORM select_fund_text  USING    p_fincode
                       CHANGING p_text.


  CLEAR p_text.
  SELECT SINGLE bezeich INTO p_text
         FROM fmfint WHERE spras = sy-langu
                       AND fikrs = p_bukrs
                       AND fincode = p_fincode.

  IF sy-subrc <> 0 AND sy-langu <> 'E'.
    SELECT SINGLE bezeich INTO p_text
           FROM fmfint WHERE spras = 'E'
                         AND fikrs = p_bukrs
                         AND fincode = p_fincode.
  ENDIF.

ENDFORM.                    " select_fund_text

*-----------------------------------------------------------------------
*       FORM PF_STATUS_SET
*-----------------------------------------------------------------------
*       Set Status.
*----------------------------------------------------------------------*
FORM pf_status_set USING  extab TYPE slis_t_extab.

  DATA: ls_extab TYPE slis_extab.

  SET PF-STATUS 'BASE'  EXCLUDING extab.

ENDFORM.                               " PF_STATUS_SET

*---------------------------------------------------------------------*
*       FORM USER_COMMAND                                             *
*---------------------------------------------------------------------*
FORM user_command USING r_ucomm     LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.
  DATA: l_ta TYPE sy-tcode VALUE 'SLIS_DUMMY'.
*
  CASE r_ucomm.
    WHEN '&IC1'.                       "doubleclick
      CASE rs_selfield-fieldname.
        WHEN 'BELNR'.
          CHECK NOT rs_selfield-value IS INITIAL.
          SET PARAMETER ID 'BLN' FIELD rs_selfield-value.
          SET PARAMETER ID 'BUK' FIELD p_bukrs.
          SET PARAMETER ID 'GJR' FIELD p_gjahr.
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDCASE.
  ENDCASE.
  CLEAR r_ucomm.
ENDFORM.                    "USER_COMMAND
