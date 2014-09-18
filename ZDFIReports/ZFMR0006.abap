*&---------------------------------------------------------------------*
*& Program ID     : ZFMR0006                                           *
*& Program Name   : FI-FM Monthly Balance Comparison                   *
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
REPORT  ZFMR0006    MESSAGE-ID zmfi.

*----------------------------------------------------------------------*
*   Data Definition
*----------------------------------------------------------------------*
TYPE-POOLS: slis,
            vrm.

*---// Tables
TABLES: bsis,
        bkpf,
        bseg,
        v_fmifi,
        t001,
        skat.

type-pools zfmcm.

*---// Constants,
CONSTANTS:
  c_bukrs           LIKE bkpf-bukrs         VALUE zfmcm_fm_bukrs,
  c_ktopl           LIKE skat-ktopl         VALUE zfmcm_ktopl,
  c_first_button                            VALUE '1',
  c_second_button                           VALUE '2',
  c_cancel_button                           VALUE 'A',
  c_true                                    VALUE 'X',
  v                                         VALUE '|'.

*---// Internal tables
DATA: BEGIN OF it_head OCCURS 0,
        expand,
        hkont  LIKE bseg-hkont,
        txt20  LIKE skat-txt20,
        fmamt  LIKE glt0-tslvt,    " FM amount
        fiamt  LIKE glt0-tslvt,    " FI amount
        dfamt  LIKE glt0-tslvt,    " Difference
      END   OF it_head.

DATA: BEGIN OF it_item OCCURS 0,
        hkont   LIKE bseg-hkont,
        kngjahr LIKE v_fmifi-kngjahr,
        knbelnr LIKE v_fmifi-knbelnr,
        knbuzei LIKE v_fmifi-knbuzei,
        budat   LIKE v_fmifi-budat,
        fonds   LIKE v_fmifi-fonds,
        fistl   LIKE v_fmifi-fistl,
        fipex   LIKE v_fmifi-fipex,
        fkbtr   LIKE v_fmifi-fkbtr,
        sgtxt   LIKE v_fmifi-sgtxt,
        dmbe2   LIKE bseg-dmbe2,      " Fi amount
        dfamt   LIKE bseg-dmbe2,      " Difference
      END   OF it_item.

DATA: BEGIN OF it_fm   OCCURS 0,
        hkont  LIKE v_fmifi-hkont,
        fkbtr  LIKE glt0-tslvt,
      END   OF it_fm.

DATA: BEGIN OF it_doc OCCURS 0,
        kngjahr LIKE v_fmifi-kngjahr,
        knbelnr LIKE v_fmifi-knbelnr,
        knbuzei LIKE v_fmifi-knbuzei,
      END   OF it_doc.

DATA: BEGIN OF it_fmit OCCURS 0,
        hkont   LIKE bseg-hkont,
        kngjahr LIKE v_fmifi-kngjahr,
        knbelnr LIKE v_fmifi-knbelnr,
        knbuzei LIKE v_fmifi-knbuzei,
        budat   LIKE v_fmifi-budat,
        fonds   LIKE v_fmifi-fonds,
        fistl   LIKE v_fmifi-fistl,
        fipex   LIKE v_fmifi-fipex,
        fkbtr   LIKE v_fmifi-fkbtr,
        sgtxt   LIKE v_fmifi-sgtxt,
      END   OF it_fmit.

DATA: BEGIN OF it_fi   OCCURS 0,
        hkont  LIKE bsis-hkont,
        dmbe2  LIKE glt0-tslvt,
      END   OF it_fi.

data: begin of it_bsis occurs 0,
        gjahr  LIKE bseg-gjahr,
        belnr  LIKE bseg-belnr,
        buzei  LIKE bseg-buzei,
        hkont  LIKE bseg-hkont,
        budat  like bsis-budat,
        FIPOS  like bseg-fipos,
        FISTL  like bseg-fistl,
        GEBER  like bseg-GEBER,
        shkzg  LIKE bseg-shkzg,
        dmbe2  LIKE bseg-dmbe2,
        sgtxt  like bseg-sgtxt,
      end   of it_bsis.

RANGES: r_wrttp FOR v_fmifi-wrttp,
        r_hkont FOR v_fmifi-hkont.

*---// Global variables
DATA : g_col(3) TYPE n.

*---// for ALV
DATA: gs_keyinfo      TYPE slis_keyinfo_alv,
      gs_layout       TYPE slis_layout_alv,
      gs_fieldcat     TYPE slis_fieldcat_alv,
      gt_fieldcat     TYPE slis_t_fieldcat_alv,
      gt_events       TYPE slis_t_event.

DATA : l_fund TYPE bp_geber VALUE '',
       l_fictr TYPE fistl VALUE '',
       l_fipex TYPE fm_fipex VALUE ''.
*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.
PARAMETERS:
  p_bukrs     LIKE  bkpf-bukrs OBLIGATORY MEMORY ID buk
                               DEFAULT c_bukrs,
  p_gjahr     LIKE  bkpf-gjahr OBLIGATORY  MEMORY ID buk,
  p_monat     LIKE  bkpf-monat OBLIGATORY.
SELECT-OPTIONS:
  s_hkont     FOR   bseg-hkont.
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

*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM read_data.


*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.
  READ TABLE it_head INDEX 1.
  IF sy-subrc <> 0.
    MESSAGE s000 WITH text-m02.
    EXIT.
  ENDIF.

  PERFORM display_list.

*----------------------------------------------------------------------*
* Sub-Routine
*----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*&      Form  init
*&---------------------------------------------------------------------*
FORM init .

  p_gjahr = sy-datum+0(4).
  p_monat = sy-datum+4(2).

ENDFORM.                    " init
*&---------------------------------------------------------------------*
*&      Form  read_data
*&---------------------------------------------------------------------*
FORM read_data .

  PERFORM make_exclude_range.
  PERFORM select_fm_data.
  PERFORM select_fi_data.
  PERFORM merge_data_n_calu.
  PERFORM find_difference_doc.

ENDFORM.                    " read_data
*&---------------------------------------------------------------------*
*&      Form  select_fm_data
*&---------------------------------------------------------------------*
FORM select_fm_data .

  DATA: BEGIN OF lt_fmit OCCURS 0.
          INCLUDE STRUCTURE it_fmit.
  DATA:   fmbelnr LIKE v_fmifi-fmbelnr,
          fmbuzei LIKE v_fmifi-fmbuzei,
          wrttp   LIKE v_fmifi-wrttp,
          btart   LIKE v_fmifi-btart,
          stats   LIKE v_fmifi-stats,
      END   OF lt_fmit.

  DATA: l_perio  LIKE v_fmifi-perio.

  l_perio = p_monat.

  SELECT fmbelnr fmbuzei kngjahr knbelnr
         knbuzei hkont   fkbtr   budat
         fonds   fistl   fipex   wrttp
         btart   fkbtr   sgtxt   stats
    INTO CORRESPONDING FIELDS OF TABLE lt_fmit
    FROM v_fmifi
   WHERE bukrs  =  p_bukrs
     AND gjahr  =  p_gjahr
     AND perio  =  l_perio
     AND hkont  IN s_hkont
     AND hkont  IN r_hkont
     AND wrttp  IN r_wrttp.

  LOOP AT lt_fmit.
    MOVE: lt_fmit-hkont     TO it_fm-hkont,
          lt_fmit-fkbtr     TO it_fm-fkbtr.
*   change sign.
    it_fm-fkbtr = it_fm-fkbtr * -1.
    COLLECT  it_fm.  CLEAR it_fm.

    MOVE: lt_fmit-kngjahr   TO it_doc-kngjahr,
          lt_fmit-knbelnr   TO it_doc-knbelnr,
          lt_fmit-knbuzei   TO it_doc-knbuzei.

    COLLECT it_doc.  CLEAR it_doc.

    MOVE-CORRESPONDING lt_fmit  TO it_fmit.
    COLLECT it_fmit.  CLEAR it_fmit.
    CLEAR it_fmit.
  ENDLOOP.

  SORT it_fm BY hkont.
  SORT it_doc BY kngjahr knbelnr knbuzei.
  SORT it_fmit BY hkont kngjahr knbelnr knbuzei.

ENDFORM.                    " select_fm_data
*&---------------------------------------------------------------------*
*&      Form  select_fi_data
*&---------------------------------------------------------------------*
FORM select_fi_data .

  data: begin of lt_glt0 occurs 0,
          hkont  like glt0-racct,
          rtcur  like glt0-rtcur,
          drcrk  like glt0-drcrk,
          fiamt  like glt0-hslvt,
        end   of lt_glt0.

  dATA: BEGIN OF lt_sel OCCURS 0,
          field(40),
        END   OF lt_sel.

*  SELECT gjahr belnr buzei shkzg hkont dmbe2
*    INTO CORRESPONDING FIELDS OF TABLE it_bseg
*    FROM bseg
*     FOR ALL ENTRIES IN it_doc
*   WHERE bukrs  =  p_bukrs
*     AND belnr  =  it_doc-knbelnr
*     AND gjahr  =  it_doc-kngjahr
*     AND buzei  =  it_doc-knbuzei.
*
*  LOOP AT it_bseg.
*    MOVE: it_bseg-hkont  TO it_fi-hkont,
*          it_bseg-dmbe2  TO it_fi-dmbe2.
*    IF it_bseg-shkzg = 'H'.
*      it_fi-dmbe2 = it_fi-dmbe2 * -1.
*    ENDIF.
*    COLLECT it_fi.  CLEAR it_fi.
*  ENDLOOP.
*
*  SORT it_fi BY hkont.
*  SORT it_bseg BY gjahr belnr buzei.

  lt_sel-field      = 'RACCT AS HKONT'.
  APPEND lt_sel.  CLEAR lt_sel.
  lt_sel-field      = 'RTCUR'.
  APPEND lt_sel.  CLEAR lt_sel.
  lt_sel-field      = 'DRCRK'.
  APPEND lt_sel.  CLEAR lt_sel.
  CONCATENATE 'HSL' p_monat INTO lt_sel-field.
  CONCATENATE lt_sel-field 'AS' 'FIAMT' INTO lt_sel-field
                                        SEPARATED BY space.
  APPEND lt_sel.  CLEAR lt_sel.
****2007-08-28 Check ???
  select (lt_sel)
    into corresponding fields of table lt_glt0
    from glt0
     for all entries in it_fm
   where rldnr  =  '00'
     and rrcty  =  '0'
     and rvers  =  '001'
     and bukrs  =  p_bukrs
     and ryear  =  p_gjahr
     and racct  =  it_fm-hkont.

  loop at lt_glt0.
    move: lt_glt0-hkont    to it_fi-hkont,
          lt_glt0-fiamt    to it_fi-dmbe2.
    collect it_fi.  clear it_fi.
  endloop.

  SORT it_fi BY hkont.

ENDFORM.                    " select_fi_data
*&---------------------------------------------------------------------*
*&      Form  make_exclude_range
*&---------------------------------------------------------------------*
FORM make_exclude_range .

  DEFINE ex_append.
    &1-sign   = 'E'.
    &1-option = 'EQ'.
    &1-low    = &2.
    append &1.  clear &1.
  END-OF-DEFINITION.

  CLEAR: r_wrttp, r_wrttp[],
         r_hkont, r_hkont[].
*??? ?? ??
****2007-08-28 check ????
  ex_append: r_hkont    '0000601100',
*             r_hkont    '0000112450',
*             r_hkont    '0000112500',

             r_wrttp    '50',
             r_wrttp    '51',
             r_wrttp    '58',
             r_wrttp    '60'.

ENDFORM.                    " make_exclude_range
*&---------------------------------------------------------------------*
*&      Form  merge_data_n_calu
*&---------------------------------------------------------------------*
FORM merge_data_n_calu .

  DATA: lt_key LIKE bseg-hkont OCCURS 0 WITH HEADER LINE.

  LOOP AT it_fm.
    MOVE it_fm-hkont   TO lt_key.
    COLLECT lt_key.
  ENDLOOP.

  LOOP AT it_fi.
    MOVE it_fi-hkont   TO lt_key.
    COLLECT lt_key.
  ENDLOOP.

  SORT lt_key.

  LOOP AT lt_key.
    CLEAR: it_fm, it_fi.

    READ TABLE it_fm WITH KEY hkont = lt_key
                              BINARY SEARCH.

    READ TABLE it_fi WITH KEY hkont = lt_key
                              BINARY SEARCH.

    MOVE: lt_key        TO it_head-hkont,
          it_fm-fkbtr   TO it_head-fmamt,
          it_fi-dmbe2   TO it_head-fiamt.

    it_head-dfamt = it_head-fmamt - it_head-fiamt.

    SELECT SINGLE txt20 INTO it_head-txt20
      FROM skat WHERE spras = sy-langu
                  AND ktopl = c_ktopl
                  AND saknr = lt_key.

    APPEND it_head.  CLEAR it_head.
  ENDLOOP.

  SORT it_head BY hkont.

ENDFORM.                    " merge_data_n_calu
*&---------------------------------------------------------------------*
*&      Form  find_difference_doc
*&---------------------------------------------------------------------*
FORM find_difference_doc .

  data: l_index like sy-tabix,
        l_dmbe2 like bsis-dmbe2.

  LOOP AT it_head.
    CHECK it_head-dfamt <> 0.

    perform select_fi_doc.

    CLEAR it_fmit.
    READ TABLE it_fmit WITH KEY hkont = it_head-hkont
                                BINARY SEARCH.
    IF sy-subrc = 0.
      LOOP AT it_fmit FROM sy-tabix.
        IF it_fmit-hkont <> it_head-hkont.
          EXIT.
        ENDIF.

        it_fmit-fkbtr = it_fmit-fkbtr * -1.

        CLEAR: it_bsis, l_dmbe2, l_index.
        READ TABLE it_bsis WITH KEY gjahr = it_fmit-kngjahr
                                    belnr = it_fmit-knbelnr
                                    buzei = it_fmit-knbuzei
                                    BINARY SEARCH.
        IF sy-subrc = 0.
          l_index = sy-tabix.
          IF it_bsis-shkzg = 'H'.
            l_dmbe2 = it_bsis-dmbe2 * -1.
          else.
            l_dmbe2 = it_bsis-dmbe2.
          ENDIF.
          delete it_bsis index l_index.
        ENDIF.

        IF it_fmit-fkbtr = l_dmbe2.
          CONTINUE.
        ENDIF.
        MOVE-CORRESPONDING it_fmit  TO it_item.
        MOVE l_dmbe2                TO it_item-dmbe2.
        it_item-dfamt = it_item-fkbtr - it_item-dmbe2.
        APPEND it_item.  CLEAR it_item.
      ENDLOOP.

      loop at it_bsis.
        if it_bsis-dmbe2 = 0.
          continue.
        endif.
        move: it_bsis-hkont   to it_item-hkont,
              it_bsis-gjahr   to it_item-kngjahr,
              it_bsis-belnr   to it_item-knbelnr,
              it_bsis-buzei   to it_item-knbuzei,
              it_bsis-budat   to it_item-budat,
              it_bsis-geber   to it_item-fonds,
              it_bsis-fipos   to it_item-fipex,
              it_bsis-fistl   to it_item-fistl,
              it_bsis-sgtxt   to it_item-sgtxt.
        if it_bsis-shkzg = 'H'.
          it_item-dmbe2 = it_bsis-dmbe2 * -1.
          it_item-dfamt = it_bsis-dmbe2.
        else.
          it_item-dmbe2 = it_bsis-dmbe2.
          it_item-dfamt = it_bsis-dmbe2 * -1.
        endif.
        append it_item.  clear it_item.
      endloop.

    ENDIF.

  ENDLOOP.

  SORT it_item BY hkont kngjahr knbelnr.

ENDFORM.                    " find_difference_doc
*&---------------------------------------------------------------------*
*&      Form  display_list
*&---------------------------------------------------------------------*
FORM display_list .

  PERFORM alv_keyinfo.
  PERFORM alv_fieldcat.
  PERFORM alv_event.
  PERFORM alv_hierachy.

ENDFORM.                    " display_list
*&---------------------------------------------------------------------*
*&      Form  alv_keyinfo
*&---------------------------------------------------------------------*
FORM alv_keyinfo .

  gs_keyinfo-header01   = 'HKONT'.
  gs_keyinfo-item01     = 'HKONT'.

ENDFORM.                    " alv_keyinfo
*&---------------------------------------------------------------------*
*&      Form  alv_fieldcat
*&---------------------------------------------------------------------*
FORM alv_fieldcat .

  gs_layout-zebra          = 'X'.
  gs_layout-expand_fieldname = 'EXPAND'.
*  gs_layout-box_fieldname  = 'CHECKBOX'.
  gs_layout-box_tabname    = 'IT_HEAD'.

  REFRESH gt_fieldcat.

  PERFORM alv_fieldcat_head.
  PERFORM alv_fieldcat_item.

ENDFORM.                    " alv_fieldcat
*&---------------------------------------------------------------------*
*&      Form  alv_event
*&---------------------------------------------------------------------*
FORM alv_event .

  REFRESH gt_events.
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type     = 0
    IMPORTING
      et_events       = gt_events
    EXCEPTIONS
      list_type_wrong = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  DATA: ls_event TYPE slis_alv_event.
*  header event
  CLEAR ls_event.
  READ TABLE gt_events INTO
             ls_event WITH KEY name = slis_ev_top_of_list.
  ls_event-form = 'TOP_OF_LIST'.
  MODIFY  TABLE gt_events FROM ls_event.
  IF sy-subrc NE 0.
    ls_event-name = slis_ev_top_of_list.
    APPEND ls_event TO gt_events.
  ENDIF.

ENDFORM.                    " alv_event
*&---------------------------------------------------------------------*
*&      Form  alv_hierachy
*&---------------------------------------------------------------------*
FORM alv_hierachy .

  CALL FUNCTION 'REUSE_ALV_HIERSEQ_LIST_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
*      i_callback_pf_status_set = slis_ev_pf_status_set
*      i_callback_user_command  = slis_ev_user_command
      is_layout                = gs_layout
      it_fieldcat              = gt_fieldcat
      i_save                   = 'X'
      it_events                = gt_events
      i_tabname_header         = 'IT_HEAD'
      i_tabname_item           = 'IT_ITEM'
      is_keyinfo               = gs_keyinfo
    TABLES
      t_outtab_header          = it_head
      t_outtab_item            = it_item.

ENDFORM.                    " alv_hierachy

*&---------------------------------------------------------------------*
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
FORM setting_fieldcat USING    p_gubun
                               p_field
                               p_value.

  DATA : lv_col(40).

  FIELD-SYMBOLS <fs>.

* START - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'S'.
    CLEAR: gs_fieldcat.
    ADD 1   TO g_col.
    gs_fieldcat-col_pos = g_col.
  ENDIF.

* Setting The Field's Attributes
  CONCATENATE 'GS_FIELDCAT-' p_field  INTO lv_col.
  ASSIGN (lv_col) TO <fs>.
  MOVE   p_value  TO <fs>.

* END - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'E'.
    APPEND gs_fieldcat TO gt_fieldcat.
  ENDIF.

ENDFORM.                    " setting_fieldcat
*&---------------------------------------------------------------------*
*&      Form  alv_fieldcat_head
*&---------------------------------------------------------------------*
FORM alv_fieldcat_head .

  CLEAR: g_col, gt_fieldcat.
  REFRESH: gt_fieldcat.

  PERFORM setting_fieldcat USING :
                     'S' 'TABNAME'       'IT_HEAD',
                     ' ' 'FIELDNAME'     'HKONT',
                     ' ' 'REF_FIELDNAME' 'HKONT',
                     'E' 'REF_TABNAME'   'BSEG',

                     'S' 'TABNAME'       'IT_HEAD',
                     ' ' 'FIELDNAME'     'TXT20',
                     ' ' 'REF_FIELDNAME' 'TXT20',
                     'E' 'REF_TABNAME'   'SKAT',

                     'S' 'TABNAME'       'IT_HEAD',
                     ' ' 'FIELDNAME'     'FMAMT',
                     ' ' 'OUTPUTLEN'     '17',
                     'E' 'SELTEXT_L'     'FM Actual',

                     'S' 'TABNAME'       'IT_HEAD',
                     ' ' 'FIELDNAME'     'FIAMT',
                     ' ' 'OUTPUTLEN'     '17',
                     'E' 'SELTEXT_L'     'FI Actual',

                     'S' 'TABNAME'       'IT_HEAD',
                     ' ' 'FIELDNAME'     'DFAMT',
                     ' ' 'OUTPUTLEN'     '17',
                     'E' 'SELTEXT_L'     'Difference'.

ENDFORM.                    " alv_fieldcat_head

*&---------------------------------------------------------------------*
*&      Form  alv_fieldcat_item
*&---------------------------------------------------------------------*
FORM alv_fieldcat_item .

  PERFORM setting_fieldcat USING :
                     'S' 'TABNAME'       'IT_ITEM',
                     ' ' 'FIELDNAME'     'KNBELNR',
                     ' ' 'REF_FIELDNAME' 'BELNR',
                     'E' 'REF_TABNAME'   'BKPF',

                     'S' 'TABNAME'       'IT_ITEM',
                     ' ' 'FIELDNAME'     'BUDAT',
                     ' ' 'REF_FIELDNAME' 'BUDAT',
                     'E' 'REF_TABNAME'   'BKPF',

                     'S' 'TABNAME'       'IT_ITEM',
                     ' ' 'FIELDNAME'     'FONDS',
                     ' ' 'REF_FIELDNAME' 'FONDS',
                     'E' 'REF_TABNAME'   'FMIFIIT',

                     'S' 'TABNAME'       'IT_ITEM',
                     ' ' 'FIELDNAME'     'FISTL',
                     ' ' 'REF_FIELDNAME' 'FISTL',
                     'E' 'REF_TABNAME'   'FMIFIIT',

                     'S' 'TABNAME'       'IT_ITEM',
                     ' ' 'FIELDNAME'     'FIPEX',
                     ' ' 'REF_FIELDNAME' 'FIPEX',
                     'E' 'REF_TABNAME'   'FMIFIIT',

                     'S' 'TABNAME'       'IT_ITEM',
                     ' ' 'FIELDNAME'     'FKBTR',
                     ' ' 'REF_FIELDNAME' 'FKBTR',
                     'E' 'REF_TABNAME'   'FMIFIIT',

                     'S' 'TABNAME'       'IT_ITEM',
                     ' ' 'FIELDNAME'     'DMBE2',
                     ' ' 'REF_FIELDNAME' 'DMBE2',
                     'E' 'REF_TABNAME'   'BSEG',

                     'S' 'TABNAME'       'IT_ITEM',
                     ' ' 'FIELDNAME'     'DFAMT',
                     ' ' 'OUTPUTLEN'     '17',
                     'E' 'SELTEXT_L'     'Difference',

                     'S' 'TABNAME'       'IT_ITEM',
                     ' ' 'FIELDNAME'     'SGTXT',
                     ' ' 'REF_FIELDNAME' 'SGTXT',
                     'E' 'REF_TABNAME'   'FMIFIIT'.

ENDFORM.                    " alv_fieldcat_item

*---------------------------------------------------------------------*
*       FORM TOP_OF_LIST                                             *
*---------------------------------------------------------------------*
FORM top_of_list.

  DATA l_period(10).

  CONCATENATE p_gjahr '/' p_monat INTO l_period.

  WRITE:/ ' Company : ', p_bukrs,
        / ' Period  : ', l_period,
        / ' Currency: ', zfmcm_euro.

ENDFORM.                    "TOP_OF_LIST
*&---------------------------------------------------------------------*
*&      Form  select_fi_doc
*&---------------------------------------------------------------------*
form select_fi_doc .

  clear: it_bsis, it_bsis[].

  select gjahr belnr buzei hkont budat
         fipos fistl geber shkzg dmbe2 sgtxt
    into corresponding fields of table it_bsis
    from bsis
   where BUKRS  =  p_bukrs
     and hkont  =  it_head-hkont
     and gjahr  =  p_gjahr
     and monat  =  p_monat.

  select gjahr belnr buzei hkont budat
         fipos fistl geber shkzg dmbe2 sgtxt
    appending corresponding fields of table it_bsis
    from bsas
   where BUKRS  =  p_bukrs
     and hkont  =  it_head-hkont
     and gjahr  =  p_gjahr
     and monat  =  p_monat.

  sort it_bsis by gjahr belnr buzei.

endform.                    " select_fi_doc
