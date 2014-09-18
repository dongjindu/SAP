*&---------------------------------------------------------------------*
*& Program ID     : ZFMR0005                                           *
*& Program Name   : [FM]Status of Operating Budget & Actual            *
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
REPORT  ZFMR0005    MESSAGE-ID zmfi.


*-----/// Table
TABLES: bppe,
        fmci,
        fmit,
        skat,
        bkpf,
        fmcit,
        fmfctr,
        fmfctrt.

INCLUDE <icon>.
include zfm_auth_form.
TYPE-POOLS: slis.
type-pools zfmcm.

*-----/// Constants
CONSTANTS:
  c_bukrs            LIKE t001-bukrs     VALUE zfmcm_fm_area,
  c_wrttp_43         LIKE bppe-wrttp     VALUE '43',    " Orginal Budget
  c_wrttp_46         LIKE bppe-wrttp     VALUE '46',   " Release Budget
  c_trgkz_n          LIKE bppe-trgkz     VALUE 'N',
  c_twaer_eur        LIKE bppe-twaer     VALUE zfmcm_euro,
  c_rldnr_9a         LIKE fmit-rldnr     VALUE '9A',
  c_rrcty_act        LIKE fmit-rrcty     VALUE '0',
  c_wrttp_50         LIKE fmit-rwrttp    VALUE '50',
  c_wrttp_51         LIKE fmit-rwrttp    VALUE '51',
  c_wrttp_60         LIKE fmit-rwrttp    VALUE '60',
  c_wrttp_54         LIKE fmit-rwrttp    VALUE '54',
  c_wrttp_57         LIKE fmit-rwrttp    VALUE '57',
  c_wrttp_61         LIKE fmit-rwrttp    VALUE '61',
  c_wrttp_66         LIKE fmit-rwrttp    VALUE '66',
  c_first_button                         VALUE '1',
  c_second_button                        VALUE '2',
  c_cancel_button                        VALUE 'A',
  c_true                                 VALUE 'X'.

*----/// Structure

*---/// Internal tables
DATA: BEGIN OF it_list OCCURS 0,
        fictr  LIKE fmfctr-fictr,
        fipex  LIKE fmci-fipex,
        twaer  LIKE bppe-twaer,
        orbgt  LIKE bppe-wtp01,
        rlbgt  LIKE bppe-wtp01,
        hkont  LIKE fmit-rhkont,
        npost  LIKE fmit-tsl01,
        post   LIKE fmit-tsl01,
        balan  LIKE fmit-tsl01,
        txt01  LIKE fmfctrt-bezeich,
        txt02  LIKE fmcit-bezei,
        txt03  LIKE skat-txt20,
      END   OF it_list.

DATA: BEGIN OF it_bugt OCCURS 0,
        fictr  LIKE fmfctr-fictr,
        fipex  LIKE fmci-fipex,
        twaer  LIKE bppe-twaer,
        orbgt  LIKE bppe-wtp01,
        rlbgt  LIKE bppe-wtp01,
      END   OF it_bugt.

DATA: BEGIN OF it_actu OCCURS 0,
        fictr  LIKE fmfctr-fictr,
        fipex  LIKE fmci-fipex,
        hkont  LIKE fmit-rhkont,
        npost  LIKE fmit-tsl01,
        post   LIKE fmit-tsl01,
      END   OF it_actu.

DATA: BEGIN OF it_fctr OCCURS 0,
        objnr  LIKE bppe-objnr,
        fictr  LIKE fmfctr-fictr,
      END   OF it_fctr.

DATA: BEGIN OF it_fmci OCCURS 0,
        posit  LIKE fmci-posit,
        fipex  LIKE fmci-fipex,
      END   OF it_fmci.

*---/// Global Varable.

*---/// ALV
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

*&----------------------------------------------------------------------
*---/// Selection screen
*&----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_fictr   FOR fmfctr-fictr,
                s_fipex   FOR fmci-fipex.
SELECTION-SCREEN END OF BLOCK bl1.

SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE text-002.
PARAMETERS    : p_gjahr  LIKE bppe-gjahr OBLIGATORY.
SELECT-OPTIONS: s_monat   FOR bkpf-monat NO-EXTENSION OBLIGATORY.
SELECTION-SCREEN END OF BLOCK bl2.

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


*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM user_auth_check TABLES s_fictr[]
                          USING zfmcm_fm_area .

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
    MESSAGE s000 WITH text-m01.
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

  DATA: BEGIN OF ls_date,
          gjahr(4),
          monat(2),
          datum(2),
        END   OF ls_date.

  ls_date = sy-datum.
  p_gjahr = ls_date-gjahr.

  CLEAR s_monat.
  s_monat      = 'IBT'.
  s_monat-low  = '01'.
  s_monat-high = ls_date-monat.
  APPEND s_monat.

ENDFORM.                    " init
*&---------------------------------------------------------------------*
*&      Form  read_rtn
*&---------------------------------------------------------------------*
FORM read_rtn .

  PERFORM select_fund_ctr.
  PERFORM select_fipex.
  PERFORM select_budget.
  PERFORM select_actual.
  PERFORM merge_data.

ENDFORM.                    " read_rtn
*&---------------------------------------------------------------------*
*&      Form  select_budget
*&---------------------------------------------------------------------*
FORM select_budget .

  DATA: BEGIN OF lt_bppe OCCURS 0,
          objnr  LIKE bppe-objnr,
          posit  LIKE bppe-posit,
          wrttp  LIKE bppe-wrttp,
          gjahr  LIKE bppe-gjahr,
          twaer  LIKE bppe-twaer,
          wtp01  LIKE bppe-wtp01,
          wtp02  LIKE bppe-wtp02,
          wtp03  LIKE bppe-wtp03,
          wtp04  LIKE bppe-wtp04,
          wtp05  LIKE bppe-wtp05,
          wtp06  LIKE bppe-wtp06,
          wtp07  LIKE bppe-wtp07,
          wtp08  LIKE bppe-wtp08,
          wtp09  LIKE bppe-wtp09,
          wtp10  LIKE bppe-wtp10,
          wtp11  LIKE bppe-wtp11,
          wtp12  LIKE bppe-wtp12,
        END   OF lt_bppe.

  DATA: l_monat  LIKE bkpf-monat,
        l_fld_nm1(30),
        l_fld_nm2(30).


  FIELD-SYMBOLS: <fs_budget>,
                 <fs_month>.

  SELECT  objnr posit wrttp gjahr twaer
          wtp01 wtp02 wtp03 wtp04 wtp05
          wtp06 wtp07 wtp08 wtp09 wtp10
          wtp11 wtp12
    INTO  CORRESPONDING FIELDS OF TABLE lt_bppe
    FROM  bppe
     FOR  ALL ENTRIES IN it_fctr
   WHERE  objnr  = it_fctr-objnr
     AND  trgkz  = c_trgkz_n
     AND  wrttp IN (c_wrttp_43, c_wrttp_46)
     AND  gjahr  = p_gjahr
     AND  geber  = space
     AND  versn  = zfmcm_versn_0
     AND  twaer  = c_twaer_eur.

  CLEAR: it_bugt, it_bugt[].

  LOOP AT lt_bppe.

    CLEAR it_bugt.
    CLEAR: it_fctr, it_fmci.

*   Find commitment item using internal commitment item.
    READ TABLE it_fmci WITH KEY posit = lt_bppe-posit
                                        BINARY SEARCH.
    IF sy-subrc = 0.
      it_bugt-fipex = it_fmci-fipex.
    ELSE.
      CONTINUE.
    ENDIF.

*   Find fund center using Object number.
    READ TABLE it_fctr WITH KEY objnr = lt_bppe-objnr
                                        BINARY SEARCH.
    IF sy-subrc = 0.
      it_bugt-fictr = it_fctr-fictr.
    ENDIF.

    it_bugt-twaer = lt_bppe-twaer.

*   choose budget field which is either orignal budget
*                                or release budget.
    CLEAR: l_fld_nm1.
    CASE lt_bppe-wrttp.
      WHEN c_wrttp_43.
        l_fld_nm1 = 'IT_BUGT-ORBGT'.
      WHEN c_wrttp_46.
        l_fld_nm1 = 'IT_BUGT-RLBGT'.
    ENDCASE.
    ASSIGN (l_fld_nm1) TO <fs_budget>.

*   Add monthly amount.
    DO 12 TIMES.
      CLEAR l_monat.
      l_monat = sy-index.

      CHECK l_monat IN s_monat.

      CLEAR l_fld_nm2.
      CONCATENATE 'LT_BPPE-WTP' l_monat INTO l_fld_nm2.
      ASSIGN (l_fld_nm2) TO <fs_month>.
      ADD <fs_month> TO <fs_budget>.
    ENDDO.

    IF <fs_budget> <> 0.
      COLLECT it_bugt.
    ENDIF.

  ENDLOOP.

  DELETE it_bugt WHERE orbgt = 0
                   AND rlbgt = 0.

  SORT it_bugt BY fictr fipex.

ENDFORM.                    " select_budget
*&---------------------------------------------------------------------*
*&      Form  select_fund_ctr
*&---------------------------------------------------------------------*
FORM select_fund_ctr .

  CLEAR: it_fctr, it_fctr[].

  SELECT fictr ctr_objnr AS objnr
    INTO CORRESPONDING FIELDS OF TABLE it_fctr
    FROM fmfctr
   WHERE fikrs  = c_bukrs
     AND fictr IN s_fictr
     AND datbis >= sy-datum
     AND datab  <= sy-datum.

  SORT it_fctr BY objnr.

ENDFORM.                    " select_fund_ctr
*&---------------------------------------------------------------------*
*&      Form  select_fipex
*&---------------------------------------------------------------------*
FORM select_fipex .

  CLEAR: it_fmci, it_fmci[].

  SELECT fipex posit
    INTO CORRESPONDING FIELDS OF TABLE it_fmci
    FROM fmci
   WHERE fikrs  = c_bukrs
     AND fipex IN s_fipex
     AND fivor  = '30'
     AND kateg  = '2'
     AND potyp  = '3'.

  SORT it_fmci BY posit.

ENDFORM.                    " select_fipex
*&---------------------------------------------------------------------*
*&      Form  select_actual
*&---------------------------------------------------------------------*
FORM select_actual .

  DATA: BEGIN OF lt_fmit OCCURS 0,
          rfistl LIKE fmit-rfistl,
          rfipex LIKE fmit-rfipex,
          rwrttp LIKE fmit-rwrttp,
          rhkont LIKE fmit-rhkont,
          tsl01  LIKE fmit-tsl01,
          tsl02  LIKE fmit-tsl02,
          tsl03  LIKE fmit-tsl03,
          tsl04  LIKE fmit-tsl04,
          tsl05  LIKE fmit-tsl05,
          tsl06  LIKE fmit-tsl06,
          tsl07  LIKE fmit-tsl07,
          tsl08  LIKE fmit-tsl08,
          tsl09  LIKE fmit-tsl09,
          tsl10  LIKE fmit-tsl10,
          tsl11  LIKE fmit-tsl11,
          tsl12  LIKE fmit-tsl12,
        END   OF lt_fmit.

  DATA: l_monat  LIKE bkpf-monat,
        l_fld_nm1(30),
        l_fld_nm2(30).

  FIELD-SYMBOLS: <fs_actual>,
                 <fs_month>.

  SELECT  rfistl rfipex rwrttp rhkont tsl01
          tsl02  tsl03  tsl04  tsl05  tsl06
          tsl07  tsl08  tsl09  tsl10  tsl11
          tsl12
    INTO  CORRESPONDING FIELDS OF TABLE lt_fmit
    FROM  fmit
     FOR  ALL ENTRIES IN it_fctr
   WHERE  rldnr  = c_rldnr_9a
     AND  rrcty  = c_rrcty_act
     AND  rvers  = zfmcm_versn_0
     AND  ryear  = p_gjahr
     AND  rtcur  = c_twaer_eur
     AND  rbukrs = c_bukrs
     AND  rfistl = it_fctr-fictr
     AND  rfipex IN s_fipex
     AND  rwrttp IN (c_wrttp_50, c_wrttp_51, c_wrttp_60,
                     c_wrttp_54, c_wrttp_57, c_wrttp_61,
                     c_wrttp_66).

  CLEAR: it_actu, it_actu[].

  LOOP AT lt_fmit.

    CLEAR it_actu.

    MOVE: lt_fmit-rfistl      TO it_actu-fictr,
          lt_fmit-rfipex      TO it_actu-fipex,
          lt_fmit-rhkont      TO it_actu-hkont.

*   choose field which is either not posted or posted.
    CLEAR: l_fld_nm1.
    CASE lt_fmit-rwrttp.
      WHEN c_wrttp_50 OR c_wrttp_51 OR c_wrttp_60.
        l_fld_nm1 = 'IT_ACTU-NPOST'.
      WHEN c_wrttp_54 OR c_wrttp_57 OR c_wrttp_61 OR
           c_wrttp_66.
        l_fld_nm1 = 'IT_ACTU-POST'.
    ENDCASE.
    ASSIGN (l_fld_nm1) TO <fs_actual>.

*   Add monthly amount.
    DO 12 TIMES.
      CLEAR l_monat.
      l_monat = sy-index.

      CHECK l_monat IN s_monat.

      CLEAR l_fld_nm2.
      CONCATENATE 'LT_FMIT-TSL' l_monat INTO l_fld_nm2.
      ASSIGN (l_fld_nm2) TO <fs_month>.
      <fs_month> = <fs_month> * -1.
      ADD <fs_month> TO <fs_actual>.
    ENDDO.

    IF <fs_actual> <> 0.
      COLLECT it_actu.
    ENDIF.
  ENDLOOP.

  DELETE it_actu WHERE npost = 0
                   AND post  = 0.

  SORT it_actu BY fictr fipex.

ENDFORM.                    " select_actual
*&---------------------------------------------------------------------*
*&      Form  merge_data
*&---------------------------------------------------------------------*
FORM merge_data .

  DATA: BEGIN OF lt_key OCCURS 0,
          fictr  LIKE fmfctr-fictr,
          fipex  LIKE fmci-fipex,
        END   OF lt_key.

  DATA: l_index     LIKE sy-tabix,
        l_tot_npost LIKE it_list-npost,
        l_tot_post  LIKE it_list-post.

  LOOP AT it_bugt.
    MOVE-CORRESPONDING it_bugt  TO lt_key.
    COLLECT lt_key.  CLEAR lt_key.
  ENDLOOP.

  LOOP AT it_actu.
    MOVE-CORRESPONDING it_actu  TO lt_key.
    COLLECT lt_key.  CLEAR lt_key.
  ENDLOOP.

  SORT lt_key.

  CLEAR: it_list, it_list[].

  LOOP AT lt_key.


    PERFORM select_fctr USING    lt_key-fictr
                        CHANGING it_list-txt01.

    CLEAR: it_bugt.
    READ TABLE it_bugt WITH KEY fictr = lt_key-fictr
                                fipex = lt_key-fipex
                                BINARY SEARCH.
    IF sy-subrc = 0.
      MOVE: it_bugt-orbgt       TO it_list-orbgt,
            it_bugt-rlbgt       TO it_list-rlbgt.
    ENDIF.

    CLEAR: l_tot_npost, l_tot_post.

    CLEAR: it_actu.
    READ TABLE it_actu WITH KEY fictr = lt_key-fictr
                                fipex = lt_key-fipex
                                BINARY SEARCH.
    IF sy-subrc EQ 0.
      LOOP AT it_actu FROM sy-tabix.
        IF it_actu-fictr <> lt_key-fictr OR
           it_actu-fipex <> lt_key-fipex.
          EXIT.
        ENDIF.

        ADD it_actu-npost       TO l_tot_npost.
        ADD it_actu-post        TO l_tot_post.

        MOVE: lt_key-fictr        TO it_list-fictr,
              lt_key-fipex        TO it_list-fipex.
        MOVE: it_actu-hkont     TO it_list-hkont,
              it_actu-npost     TO it_list-npost,
              it_actu-post      TO it_list-post.

        PERFORM select_fctr USING    lt_key-fictr
                            CHANGING it_list-txt01.

        PERFORM select_item  USING    lt_key-fipex
                             CHANGING it_list-txt02.

        PERFORM select_hkont USING    it_list-hkont
                             CHANGING it_list-txt03.

        APPEND it_list.  CLEAR it_list.
      ENDLOOP.
    ELSE.
      MOVE: lt_key-fictr        TO it_list-fictr,
            lt_key-fipex        TO it_list-fipex.
      PERFORM select_fctr USING    lt_key-fictr
                          CHANGING it_list-txt01.

      PERFORM select_item  USING    lt_key-fipex
                           CHANGING it_list-txt02.

      APPEND it_list.  CLEAR it_list.
    ENDIF.

    CONCATENATE lt_key-fipex ' / Total' INTO it_list-fipex.
    MOVE: lt_key-fictr         TO it_list-fictr,
          it_bugt-orbgt        TO it_list-orbgt,
          it_bugt-rlbgt        TO it_list-rlbgt,
          l_tot_npost          TO it_list-npost,
          l_tot_post           TO it_list-post.
    it_list-balan = it_list-rlbgt - ( it_list-npost +
                                      it_list-post ).
    APPEND it_list.  CLEAR it_list.
  ENDLOOP.

ENDFORM.                    " merge_data
*&---------------------------------------------------------------------*
*&      Form  pre_report_adj
*&---------------------------------------------------------------------*
FORM pre_report_adj .

  wa_layout-zebra             = 'X'.
  wa_layout-colwidth_optimize = 'X'.

  PERFORM fieldcat_init .

*  it_sort-fieldname = 'MONAT'.
*  it_sort-up        = 'X'.
*  it_sort-expa      = ' '.
*  it_sort-subtot    = ' '.
*  APPEND it_sort.

  PERFORM header_build USING it_top_page[].

ENDFORM.                    " pre_report_adj
*&---------------------------------------------------------------------*
*&      Form  fieldcat_init
*&---------------------------------------------------------------------*
FORM fieldcat_init .

  PERFORM build_fieldcat  USING
    'IT_LIST'     'FICTR'    'X'         space
    space         space      '10'        'Fund Ctr.'
    space         space      space       space.

  PERFORM build_fieldcat  USING
    'IT_LIST'     'TXT01'    space         space
    space         space      '20'        'Fund Ctr. Text'
    space         space      space       space.

  PERFORM build_fieldcat  USING
    'IT_LIST'     'FIPEX'    'X'         space
    space         space      '24'        'Commitment Item'
    space         space      space       space.

  PERFORM build_fieldcat  USING
    'IT_LIST'     'TXT02'    space         space
    space         space      '20'        'Commitment Item Text'
    space         space      space       space.

  PERFORM build_fieldcat  USING
    'IT_LIST'     'ORBGT'    space       space
    'TWAER'       'IT_LIST'  '16'        'Original Budget'
    space         space      space       space.

  PERFORM build_fieldcat  USING
    'IT_LIST'     'RLBGT'    space       space
    'TWAER'       'IT_LIST'  '16'        'Budget Released'
    space         space      space       space.

  PERFORM build_fieldcat  USING
    'IT_LIST'     'HKONT'    space       space
    space         space      '10'        'G/L Account'
    space         space      space       space.

  PERFORM build_fieldcat  USING
    'IT_LIST'     'TXT03'    space       space
    space         space      '20'        'G/L Text'
    space         space      space       space.

  PERFORM build_fieldcat  USING
    'IT_LIST'     'NPOST'    space       space
    'TWAER'       'IT_LIST'  '16'        'Not Posted'
    space         space      space       space.

  PERFORM build_fieldcat  USING
    'IT_LIST'     'POST'     space       space
    'TWAER'       'IT_LIST'  '16'        'Posted'
    space         space      space       space.

  PERFORM build_fieldcat  USING
    'IT_LIST'     'BALAN'    space       space
    'TWAER'       'IT_LIST'  '16'        'Balances'
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

  IF p_0101 = 'HKONT'.
    wa_fieldcat-ref_fieldname = 'HKONT'.
    wa_fieldcat-ref_tabname   = 'BSEG'.
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
      i_callback_top_of_page            = slis_ev_top_of_page
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

*-----------------------------------------------------------------------
*       FORM PF_STATUS_SET
*-----------------------------------------------------------------------
*       Set Status.
*----------------------------------------------------------------------*
FORM pf_status_set USING  extab TYPE slis_t_extab.

*  DATA: ls_extab TYPE slis_extab.
*
*  SET PF-STATUS 'BASE'  EXCLUDING extab.

ENDFORM.                               " PF_STATUS_SET

*---------------------------------------------------------------------*
*       FORM USER_COMMAND                                             *
*---------------------------------------------------------------------*
FORM user_command USING r_ucomm     LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.
  DATA: l_ta TYPE sy-tcode VALUE 'SLIS_DUMMY'.
*
  CASE r_ucomm.
*    WHEN '&IC1'.                       "doubleclick
  ENDCASE.
  CLEAR r_ucomm.
ENDFORM.                    "USER_COMMAND

*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM top_of_page.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
*          I_LOGO             = 'ENJOYSAP_LOGO'
          it_list_commentary = it_top_page.

ENDFORM.                    "TOP_OF_PAGE

*&---------------------------------------------------------------------*
*&      Form  header_build
*&---------------------------------------------------------------------*
FORM header_build USING lt_top_page TYPE
                                       slis_t_listheader.

  DATA: ls_line TYPE slis_listheader.
  DATA: l_period(40).

  CLEAR lt_top_page.
  CLEAR ls_line.
  ls_line-typ  = 'H'.
* LS_LINE-KEY:  NOT USED FOR THIS TYPE
  ls_line-info = text-t01.
  APPEND ls_line TO lt_top_page.

  CLEAR ls_line.
  ls_line-typ  = 'S'.
  ls_line-key  = text-h01.    " Fiscal Year
  ls_line-info = p_gjahr.
  APPEND ls_line TO lt_top_page.

  IF NOT s_monat-high IS INITIAL.
    CONCATENATE s_monat-low '~' s_monat-high INTO l_period
                               SEPARATED BY space.
  ELSE.
    l_period = s_monat-low.
  ENDIF.

  CLEAR ls_line.
  ls_line-typ  = 'S'.
  ls_line-key  = text-h02.    " Period
  ls_line-info = l_period.
  APPEND ls_line TO lt_top_page.

ENDFORM.                    " header_build
*&---------------------------------------------------------------------*
*&      Form  select_fctr
*&---------------------------------------------------------------------*
FORM select_fctr  USING    p_fictr
                  CHANGING p_text.

  CLEAR p_text.
  SELECT SINGLE bezeich INTO p_text
         FROM fmfctrt WHERE spras = sy-langu
                        AND fikrs = c_bukrs
                        AND fictr = p_fictr
                        AND datbis = '99991231'.

  IF sy-subrc <> 0 AND sy-langu <> 'E'.
    SELECT SINGLE bezeich INTO p_text
           FROM fmfctrt WHERE spras = 'E'
                          AND fikrs = c_bukrs
                          AND fictr = p_fictr
                          AND datbis = '99991231'.
  ENDIF.

ENDFORM.                    " select_fctr
*&---------------------------------------------------------------------*
*&      Form  select_item
*&---------------------------------------------------------------------*
FORM select_item  USING    p_fipex
                  CHANGING p_text.

  CLEAR p_text.
  SELECT SINGLE bezei INTO p_text
         FROM fmcit WHERE spras = sy-langu
                      AND fikrs = c_bukrs
                      AND fipex = p_fipex.

  IF sy-subrc <> 0 AND sy-langu <> 'E'.
    SELECT SINGLE bezei INTO p_text
           FROM fmcit WHERE spras = 'E'
                        AND fikrs = c_bukrs
                        AND fipex = p_fipex.
  ENDIF.

ENDFORM.                    " select_item
*&---------------------------------------------------------------------*
*&      Form  select_hkont
*&---------------------------------------------------------------------*
FORM select_hkont  USING    p_hkont
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

ENDFORM.                    " select_hkont
