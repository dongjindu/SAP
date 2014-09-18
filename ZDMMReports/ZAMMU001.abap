*----------------------------------------------------------------------
* Program ID        : ZAMMU001
* Title             : [MM] Dock-In List by Outbound Orders
* Created on        : 7/15/2011
* Created by        : I.G.MOON
* Description       : [MM] Dock-In List by Outbound Orders
*----------------------------------------------------------------------
REPORT zammu001 MESSAGE-ID zmco LINE-SIZE 255.

TABLES: zmmt0032,zmmt0038,zmmt0051,mara,marc,pkhd,sscrfields.

INCLUDE : z_moon_alv_top,
          z_moon_alv_fnc.

INCLUDE <icon>.                        " icon

TYPES: BEGIN OF ty_o_tab.
        INCLUDE STRUCTURE zmmt0038.
TYPES: END OF ty_o_tab.

TYPES: BEGIN OF ty_i_tab.
        INCLUDE STRUCTURE zmmt0032.
TYPES: END OF ty_i_tab.

TYPES : BEGIN OF ty_row_tab,
          o_pkkey      TYPE pkkey,
          o_rsnum      TYPE rsnum,
          o_rspos      TYPE rspos,
          o_reversed   TYPE reversed,
          o_saedt      TYPE saedt,
          o_saeuz      TYPE saeuz,
          o_werks      TYPE werks_d,
          o_lgort      TYPE lgort_d,
          o_lgpro      TYPE lgpro,
          o_prvbe      TYPE prvbe,
          o_zfeeder    TYPE zefeeder,
          o_matnr      TYPE matnr,
          o_ablad      TYPE stlpl,
          o_pkbmg      TYPE pkbmg,
          o_meins      TYPE meins,
          o_pktim      TYPE pktim,
          o_kwbzm      TYPE kwbzm,
          o_rksta      TYPE rkstat,
          o_sfgsn      TYPE sfgsn,
          o_type       TYPE bapi_mtype,
          o_message    TYPE bapi_msg,
          o_atnam      TYPE zcuser,
          o_atdat      TYPE zcdate,
          o_attim      TYPE zctime,
          o_etnam      TYPE zpp_user,
          o_etdat      TYPE zdate,
          o_ettim      TYPE zcrtime,
          i_pkkey      TYPE pkkey,
          i_rsnum      TYPE rsnum,
          i_rspos      TYPE rspos,
          i_saedt      TYPE saedt,
          i_saeuz	     TYPE saeuz,
          i_budat      TYPE budat,
          i_ferth      TYPE ferth,
          i_formt      TYPE formt,
          i_zfeeder    TYPE zefeeder,
          i_matnr      TYPE matnr,
          i_werks      TYPE werks_d,
          i_lgort      TYPE lgort_d,
          i_umlgo      TYPE umlgo,
          i_menge      TYPE menge_d,
          i_meins      TYPE meins,
          i_wempf      TYPE wempf,
          i_mblnr      TYPE mblnr,
          i_mjahr      TYPE mjahr,
          i_type       TYPE bapi_mtype,
          i_message    TYPE bapi_msg,
          i_atnam      TYPE zcuser,
          i_atdat      TYPE zcdate,
          i_attim      TYPE zctime,
          i_etnam      TYPE zpp_user,
          i_etdat      TYPE zdate,
          i_ettim      TYPE zcrtime,

          diff_qty,
          p_open_tim TYPE p DECIMALS 2,
          p_full_tim TYPE p DECIMALS 2,
          open_tim(20),
          full_tim(20),

          manual,

 END OF ty_row_tab.

TYPES: BEGIN OF ty_out.
INCLUDE  TYPE ty_row_tab.
TYPES: END OF ty_out.

DATA: BEGIN OF it_rsnum OCCURS 0,
          pkkey TYPE  pkkey,
          rsnum TYPE  rsnum,
          rspos TYPE  rspos,
          saedt TYPE  saedt,
          saeuz TYPE  saeuz,
      END OF it_rsnum.

DATA: BEGIN OF it_revers OCCURS 0,
          pkkey TYPE  pkkey,
          rsnum TYPE  rsnum,
          rspos TYPE  rspos,
      END OF it_revers.

DATA: BEGIN OF it_resb OCCURS 0,
        pkkey   LIKE pkps-pkkey,
        saedt   LIKE pkps-saedt,
        saeuz   LIKE pkps-saeuz,
        rsnum   LIKE pkps-rsnum,
        pkimg   LIKE pkps-pkimg,
        matnr   LIKE pkhd-matnr,
        werks   LIKE pkhd-werks,
        prvbe   LIKE pkhd-prvbe,
        zfeeder LIKE pkhd-zfeeder,
     END OF it_resb.

DATA  : it_o_tab TYPE TABLE OF ty_o_tab WITH HEADER LINE,
        it_i_tab TYPE TABLE OF ty_i_tab WITH HEADER LINE.

DATA  : gt_row_tab TYPE TABLE OF ty_row_tab WITH HEADER LINE,
        it_row_tab TYPE TABLE OF ty_row_tab WITH HEADER LINE,
        gt_out     TYPE TABLE OF ty_out     WITH HEADER LINE.

DATA $ix TYPE i.
DATA g_cnt TYPE i.
DATA g_cnt2 TYPE i.
DATA g_cnt3 TYPE i.

DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.

DEFINE u_break.
  if p_debug eq true.
    break-point.
  endif.
END-OF-DEFINITION.

CONSTANTS:  false VALUE ' ',
            true  VALUE 'X'.

DATA: g_error(1).

DATA :top_line    TYPE i,
      line_count  TYPE i,
      tab_lines   TYPE i,
      bottom_line TYPE i.

*DATA  p_revr.

DATA  okcode(4).


*--- ALV
TYPE-POOLS: slis.
DATA : w_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
       w_eventcat TYPE slis_t_event WITH HEADER LINE,
       w_selfield TYPE slis_selfield,
       w_sortcat  TYPE slis_t_sortinfo_alv WITH HEADER LINE,
       w_col_pos  TYPE i,
       w_program  LIKE sy-repid,
       w_top_of_page TYPE slis_t_listheader,
       w_line1 TYPE slis_listheader.

DATA: "gt_fieldcat TYPE slis_t_fieldcat_alv,
      "gs_layout   TYPE slis_layout_alv,
      "gt_sp_group TYPE slis_t_sp_group_alv,
      "gt_events   TYPE slis_t_event,
      gt_sorts    TYPE slis_t_sortinfo_alv WITH HEADER LINE,
      gs_prnt     TYPE slis_print_alv,
      g_repid     LIKE sy-repid.
*---- ALV


SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 4(16)  text-x00 FOR FIELD p_opt.
PARAMETERS p_opt   RADIOBUTTON GROUP radi USER-COMMAND uchk.

SELECTION-SCREEN COMMENT 25(4) text-x01 FOR FIELD p_opt2.
PARAMETERS p_opt2    DEFAULT 'X' RADIOBUTTON GROUP radi.
SELECTION-SCREEN COMMENT 40(8) text-x02 FOR FIELD p_opt3.
PARAMETERS p_opt3    RADIOBUTTON GROUP radi.
SELECTION-SCREEN COMMENT 65(10) text-x03 FOR FIELD p_opt4.
PARAMETERS p_opt4    RADIOBUTTON GROUP radi.
SELECTION-SCREEN END   OF LINE.

SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.

SELECT-OPTIONS : s_rsnum FOR zmmt0038-rsnum,
                 s_etdat FOR sy-datum,
                 s_zfeed FOR zmmt0038-zfeeder,
                 s_prvbe FOR zmmt0038-prvbe,
                 s_rgver FOR pkhd-rgver,
                 s_dispo FOR marc-dispo,
                 s_matnr FOR zmmt0038-matnr,
                 s_lgpro FOR zmmt0038-lgpro,
                 S_TYPE  FOR zmmt0038-TYPE NO INTERVALS.

PARAMETERS p_revr AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK block1.

SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME TITLE text-002.
*PARAMETERS p_comp AS CHECKBOX USER-COMMAND comp.

SELECT-OPTIONS : s_budat FOR zmmt0032-budat,
                 s_mblnr FOR zmmt0032-mblnr.

SELECTION-SCREEN END OF BLOCK block2.

* Layout
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-010.
PARAMETER p_vari TYPE slis_vari.
SELECTION-SCREEN END OF BLOCK b4.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM alv_variant_f4 CHANGING p_vari.

AT SELECTION-SCREEN .
  CASE sscrfields-ucomm.
    WHEN 'COMP'.
      p_opt = p_opt2 = p_opt3 = p_opt4 = false.
      p_opt3 = true.
    WHEN 'UCHK'.
*      IF p_opt4 NE true.
*        p_comp = false.
*      ENDIF.
  ENDCASE.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  sy-title =   '[MM] Dock-In List by Outbound Orders'.

  __cls s_etdat.
  s_etdat = 'IEQ'.
  s_etdat-low = sy-datum - 1.
  s_etdat-high = sy-datum.
  APPEND s_etdat.

  PERFORM default_variant.

START-OF-SELECTION.

  PERFORM initialize            .

  IF p_opt4 = 'X'.  "Missing
    PERFORM get_from_resb.
  ELSEIF p_opt3 EQ 'X'.
    PERFORM get_from_table_completed.
    CHECK g_error EQ space.
    PERFORM fill_table.
    PERFORM finalize.
  ELSE.
    PERFORM get_from_table.
    CHECK g_error EQ space.
    PERFORM fill_table.
    PERFORM finalize.
  ENDIF.


END-OF-SELECTION.

  IF p_opt4 = 'X'.
    PERFORM display_missing_alv.
  ELSE.
    CHECK g_error EQ space.
    PERFORM move_out.
    PERFORM set_output.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  show_progress
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1894   text
*      -->P_1895   text
*----------------------------------------------------------------------*
FORM show_progress USING    pf_text
                            value(pf_val).

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            percentage = pf_val
            text       = pf_text.

ENDFORM.                    " SHOW_PROGRESS
*&---------------------------------------------------------------------*
*&      Form  initialize
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialize.

  CLEAR g_error.
  __cls : it_row_tab, gt_out.
  __cls : it_o_tab,it_i_tab,it_rsnum,it_revers.
  g_cnt = g_cnt2 = g_cnt3 = 0.

*  p_revr = p_opt4.

ENDFORM.                    " initialize
*&---------------------------------------------------------------------*
*&      Form  move_out
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM move_out.

  LOOP AT it_row_tab.
    MOVE-CORRESPONDING it_row_tab TO gt_out.
** On 08/23/13 Request by Mr. K Kim
    if  gt_out-o_type is INITIAL.
        gt_out-o_type = 'N'.
    ENDIF.
** End on 08/23/13
    APPEND gt_out.
  ENDLOOP.

  LOOP AT gt_out WHERE manual = 'X'.
    ADD 1 TO g_cnt2.
  ENDLOOP.

  LOOP AT gt_out WHERE manual = 'D'.
    ADD 1 TO g_cnt3.
  ENDLOOP.

  DESCRIBE TABLE gt_out LINES g_cnt.

  g_cnt = g_cnt - g_cnt2 - g_cnt3.

ENDFORM.                    " move_out
*&---------------------------------------------------------------------*
*&      Form  default_variant
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM default_variant.
  DATA: h_subrc   TYPE sysubrc,
        h_repid   TYPE syrepid,
        h_variant TYPE raldb_vari.

  h_repid = sy-repid.
  CLEAR h_variant.
  h_variant = 'U_'.
  WRITE sy-uname TO h_variant+2.

  h_variant = '_DEFAULT'.

  CALL FUNCTION 'RS_VARIANT_EXISTS'
       EXPORTING
            report  = h_repid
            variant = h_variant
       IMPORTING
            r_c     = h_subrc.

  IF NOT h_subrc IS INITIAL.
    CLEAR h_variant.
    h_variant = 'SAP_TCODE_'.
    WRITE sy-tcode TO h_variant+10.
    CALL FUNCTION 'RS_VARIANT_EXISTS'
         EXPORTING
              report  = h_repid
              variant = h_variant
         IMPORTING
              r_c     = h_subrc.

    IF NOT h_subrc IS INITIAL.
      CLEAR h_variant.
      h_variant = 'SAP&TCODE_'.
      WRITE sy-tcode TO h_variant+10.
      CALL FUNCTION 'RS_VARIANT_EXISTS'
           EXPORTING
                report  = h_repid
                variant = h_variant
           IMPORTING
                r_c     = h_subrc.
    ENDIF.
  ENDIF.

  IF h_subrc IS INITIAL.
    CALL FUNCTION 'RS_SUPPORT_SELECTIONS'
         EXPORTING
              report               = h_repid
              variant              = h_variant
         EXCEPTIONS
              variant_not_existent = 01
              variant_obsolete     = 02.
  ENDIF.

ENDFORM.                    " default_variant
*&---------------------------------------------------------------------*
*&      Form  get_from_table
*&---------------------------------------------------------------------*
FORM get_from_table.

  PERFORM show_progress USING 'Get Outbound order...' 10.

  IF  p_opt2 EQ true.

    SELECT
            zmmt0038~pkkey
            zmmt0038~rsnum
            zmmt0038~rspos
            zmmt0038~reversed
            zmmt0038~saedt
            zmmt0038~saeuz
            zmmt0038~werks
            zmmt0038~lgort
            zmmt0038~lgpro
            zmmt0038~prvbe
            zmmt0038~zfeeder
            zmmt0038~matnr
            zmmt0038~ablad
            zmmt0038~pkbmg
            zmmt0038~meins
            zmmt0038~pktim
            zmmt0038~kwbzm
            zmmt0038~rksta
            zmmt0038~sfgsn
            zmmt0038~type
            zmmt0038~message
            zmmt0038~atnam
            zmmt0038~atdat
            zmmt0038~attim
            zmmt0038~etnam
            zmmt0038~etdat
            zmmt0038~ettim

    INTO CORRESPONDING FIELDS OF TABLE it_o_tab
    FROM  zmmt0038 INNER JOIN marc
    ON  marc~matnr EQ zmmt0038~matnr
    AND marc~werks EQ zmmt0038~werks
    INNER JOIN pkps
        ON pkps~pkkey EQ zmmt0038~pkkey
    INNER JOIN pkhd
        ON pkhd~pknum EQ pkps~pknum
    WHERE zmmt0038~rsnum IN s_rsnum
     AND zmmt0038~etdat IN s_etdat
     AND zmmt0038~zfeeder IN s_zfeed
     AND zmmt0038~matnr IN s_matnr
     AND zmmt0038~prvbe IN s_prvbe
     AND marc~dispo IN s_dispo
     AND zmmt0038~lgpro IN s_lgpro
     AND zmmt0038~type IN s_type
     AND pkhd~rgver IN s_rgver
     AND NOT EXISTS ( SELECT * FROM zmmt0032
                      WHERE pkkey EQ zmmt0038~pkkey
                      AND rsnum EQ zmmt0038~rsnum
                      AND rspos EQ zmmt0038~rspos
                     )
  %_HINTS ORACLE 'FIRST_ROWS(10)'.


  ELSEIF p_opt3 EQ true.

    SELECT
            zmmt0038~pkkey
            zmmt0038~rsnum
            zmmt0038~rspos
            zmmt0038~reversed
            zmmt0038~saedt
            zmmt0038~saeuz
            zmmt0038~werks
            zmmt0038~lgort
            zmmt0038~lgpro
            zmmt0038~prvbe
            zmmt0038~zfeeder
            zmmt0038~matnr
            zmmt0038~ablad
            zmmt0038~pkbmg
            zmmt0038~meins
            zmmt0038~pktim
            zmmt0038~kwbzm
            zmmt0038~rksta
            zmmt0038~sfgsn
            zmmt0038~type
            zmmt0038~message
            zmmt0038~atnam
            zmmt0038~atdat
            zmmt0038~attim
            zmmt0038~etnam
            zmmt0038~etdat
            zmmt0038~ettim

    INTO CORRESPONDING FIELDS OF TABLE it_o_tab
    FROM  zmmt0038 INNER JOIN marc
    ON  marc~matnr EQ zmmt0038~matnr
    AND marc~werks EQ zmmt0038~werks
    INNER JOIN pkps
        ON pkps~pkkey EQ zmmt0038~pkkey
    INNER JOIN pkhd
        ON pkhd~pknum EQ pkps~pknum
    WHERE zmmt0038~rsnum IN s_rsnum
     AND zmmt0038~etdat IN s_etdat
     AND zmmt0038~zfeeder IN s_zfeed
     AND zmmt0038~matnr IN s_matnr
     AND zmmt0038~prvbe IN s_prvbe
     AND marc~dispo IN s_dispo
     AND zmmt0038~lgpro IN s_lgpro
     AND pkhd~rgver IN s_rgver
     AND EXISTS ( SELECT * FROM zmmt0032
                      WHERE pkkey EQ zmmt0038~pkkey
                      AND rsnum EQ zmmt0038~rsnum
                      AND rspos EQ zmmt0038~rspos
                     )
    %_HINTS ORACLE 'FIRST_ROWS(10)'.

  ELSE.

    SELECT
            zmmt0038~pkkey
            zmmt0038~rsnum
            zmmt0038~rspos
            zmmt0038~reversed
            zmmt0038~saedt
            zmmt0038~saeuz
            zmmt0038~werks
            zmmt0038~lgort
            zmmt0038~lgpro
            zmmt0038~prvbe
            zmmt0038~zfeeder
            zmmt0038~matnr
            zmmt0038~ablad
            zmmt0038~pkbmg
            zmmt0038~meins
            zmmt0038~pktim
            zmmt0038~kwbzm
            zmmt0038~rksta
            zmmt0038~sfgsn
            zmmt0038~type
            zmmt0038~message
            zmmt0038~atnam
            zmmt0038~atdat
            zmmt0038~attim
            zmmt0038~etnam
            zmmt0038~etdat
            zmmt0038~ettim

    INTO CORRESPONDING FIELDS OF TABLE it_o_tab
    FROM  zmmt0038 INNER JOIN marc
    ON  marc~matnr EQ zmmt0038~matnr
    AND marc~werks EQ zmmt0038~werks
    INNER JOIN pkps
        ON pkps~pkkey EQ zmmt0038~pkkey
    INNER JOIN pkhd
        ON pkhd~pknum EQ pkps~pknum
    WHERE zmmt0038~rsnum IN s_rsnum
     AND zmmt0038~etdat IN s_etdat
     AND zmmt0038~zfeeder IN s_zfeed
     AND zmmt0038~matnr IN s_matnr
     AND zmmt0038~prvbe IN s_prvbe
     AND marc~dispo IN s_dispo
     AND zmmt0038~lgpro IN s_lgpro
     AND pkhd~rgver IN s_rgver
    %_HINTS ORACLE 'FIRST_ROWS(10)'.
  ENDIF.

  IF sy-subrc NE 0.
    MESSAGE s000 WITH 'No Data was found!'.
    g_error = true.
    EXIT.
  ENDIF.

  CHECK NOT it_o_tab[] IS INITIAL.
  SORT it_o_tab BY rsnum.

  LOOP AT it_o_tab.
    it_rsnum-pkkey = it_o_tab-pkkey.
    it_rsnum-rsnum = it_o_tab-rsnum.
    it_rsnum-rspos = it_o_tab-rspos.
    it_rsnum-saedt = it_o_tab-saedt.
    it_rsnum-saeuz = it_o_tab-saeuz.
    APPEND it_rsnum.

    IF it_o_tab-reversed EQ true.
      it_revers-pkkey = it_o_tab-pkkey.
      it_revers-rsnum = it_o_tab-rsnum.
      it_revers-rspos = it_o_tab-rspos.
      APPEND it_revers.
    ENDIF.

  ENDLOOP.

  IF p_revr EQ true.

  ELSE.
    SORT it_revers BY pkkey rsnum.

    LOOP AT it_o_tab where reversed = 'X'.
      $ix = sy-tabix.
      READ TABLE it_revers WITH KEY pkkey = it_o_tab-pkkey
                                    rsnum = it_o_tab-rsnum
                                    BINARY SEARCH.
      IF sy-subrc EQ 0.
        DELETE it_o_tab INDEX $ix.
      ENDIF.

    ENDLOOP.

  ENDIF.


  PERFORM show_progress USING 'Get Dock-In...' 40.

  IF p_opt2 EQ false. " only all, completed, include cancel.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_i_tab
    FROM  zmmt0032
    FOR ALL ENTRIES IN it_rsnum
    WHERE pkkey EQ it_rsnum-pkkey
     AND  rsnum EQ it_rsnum-rsnum
     AND  rspos EQ it_rsnum-rspos
     AND matnr IN s_matnr.

    SORT it_i_tab BY rsnum.
  ENDIF.

  LOOP AT it_o_tab.

    it_row_tab-o_pkkey      = it_o_tab-pkkey.
    it_row_tab-o_rsnum      = it_o_tab-rsnum.
    it_row_tab-o_rspos      = it_o_tab-rspos.
    it_row_tab-o_reversed   = it_o_tab-reversed.
    it_row_tab-o_saedt      = it_o_tab-saedt.
    it_row_tab-o_saeuz      = it_o_tab-saeuz.
    it_row_tab-o_werks      = it_o_tab-werks.
    it_row_tab-o_lgort      = it_o_tab-lgort.
    it_row_tab-o_lgpro      = it_o_tab-lgpro.
    it_row_tab-o_prvbe      = it_o_tab-prvbe.
    it_row_tab-o_zfeeder    = it_o_tab-zfeeder.
    it_row_tab-o_matnr      = it_o_tab-matnr.
    it_row_tab-o_ablad      = it_o_tab-ablad.
    it_row_tab-o_pkbmg      = it_o_tab-pkbmg.
    it_row_tab-o_meins      = it_o_tab-meins.
    it_row_tab-o_pktim      = it_o_tab-pktim.
    it_row_tab-o_kwbzm      = it_o_tab-kwbzm.
    it_row_tab-o_rksta      = it_o_tab-rksta.
    it_row_tab-o_sfgsn      = it_o_tab-sfgsn.
    it_row_tab-o_type       = it_o_tab-type.
    it_row_tab-o_message    = it_o_tab-message.
    it_row_tab-o_atnam      = it_o_tab-atnam.
    it_row_tab-o_atdat      = it_o_tab-atdat.
    it_row_tab-o_attim      = it_o_tab-attim.
    it_row_tab-o_etnam      = it_o_tab-etnam.
    it_row_tab-o_etdat      = it_o_tab-etdat.
    it_row_tab-o_ettim      = it_o_tab-ettim.

    IF p_opt2 EQ false.
      READ TABLE it_i_tab WITH KEY rsnum = it_o_tab-rsnum BINARY SEARCH.
      IF sy-subrc EQ 0.
        it_row_tab-i_pkkey      = it_i_tab-pkkey.
        it_row_tab-i_rsnum      = it_i_tab-rsnum.
        it_row_tab-i_rspos      = it_i_tab-rspos.
        it_row_tab-i_saedt      = it_i_tab-saedt.
        it_row_tab-i_saeuz     = it_i_tab-saeuz.
        it_row_tab-i_budat      = it_i_tab-budat.
        it_row_tab-i_ferth      = it_i_tab-ferth.
        it_row_tab-i_formt      = it_i_tab-formt.
        it_row_tab-i_zfeeder    = it_i_tab-zfeeder.
        it_row_tab-i_matnr      = it_i_tab-matnr.
        it_row_tab-i_werks      = it_i_tab-werks.
        it_row_tab-i_lgort      = it_i_tab-lgort.
        it_row_tab-i_umlgo      = it_i_tab-umlgo.
        it_row_tab-i_menge      = it_i_tab-menge.
        it_row_tab-i_meins      = it_i_tab-meins.
        it_row_tab-i_wempf      = it_i_tab-wempf.
        it_row_tab-i_mblnr      = it_i_tab-mblnr.
        it_row_tab-i_mjahr      = it_i_tab-mjahr.
        it_row_tab-i_type       = it_i_tab-type.
        it_row_tab-i_message    = it_i_tab-message.
        it_row_tab-i_atnam      = it_i_tab-atnam.
        it_row_tab-i_atdat      = it_i_tab-atdat.
        it_row_tab-i_attim      = it_i_tab-attim.
        it_row_tab-i_etnam      = it_i_tab-etnam.
        it_row_tab-i_etdat      = it_i_tab-etdat.
        it_row_tab-i_ettim      = it_i_tab-ettim.
      ENDIF.
    ENDIF.

    APPEND it_row_tab. CLEAR it_row_tab.
  ENDLOOP.

ENDFORM.                    " get_from_table

*---------------------------------------------------------------------*
*       FORM fill_table                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM fill_table.
  DATA diff_tim  LIKE tvro-fahztd.
  DATA $diff_tim(10) TYPE n.

  DATA $day(4)  TYPE n.
  DATA $hour(2) TYPE n.
  DATA $min(2) TYPE n.

  DATA $total_h(4)  TYPE n.
  DATA $total_m(4)  TYPE n.

  DATA str_total_h(10).
  DATA str_total_m(10).
  DATA  $xloek TYPE xloek.
  DATA  $subrc TYPE i.
  DATA  $l_tm  TYPE zcrtime.
  DATA  $l_dt  TYPE zdate.

  LOOP AT it_row_tab.
    $ix = sy-tabix.
    CLEAR : it_row_tab-open_tim,it_row_tab-full_tim,
            it_row_tab-p_open_tim,it_row_tab-p_full_tim,
            it_row_tab-diff_qty, it_row_tab-manual,$subrc.

    IF NOT it_row_tab-i_pkkey IS INITIAL.

      IF NOT it_row_tab-o_etdat IS INITIAL AND
         NOT it_row_tab-i_etdat IS INITIAL.
* 38 - saedt, saeuz ; line feed target time
* 38 - etdat, ettim ; EAI interface time (ordering time)
* 32 - saedt, saeuz ; dock-in time
* 32 - etdat, ettim ; EAI interface time
        IF ( it_row_tab-o_etdat > it_row_tab-i_saedt )
        OR ( it_row_tab-o_etdat = it_row_tab-i_saedt
         AND it_row_tab-o_ettim > it_row_tab-i_saeuz ).
          $l_dt = it_row_tab-i_etdat.
          $l_tm = it_row_tab-i_ettim.
        ELSE.
          $l_dt = it_row_tab-i_saedt.
          $l_tm = it_row_tab-i_saeuz.
        ENDIF.


        CALL FUNCTION 'SD_CALC_DURATION_FROM_DATETIME'
             EXPORTING
                  i_date1 = it_row_tab-o_etdat
                  i_time1 = it_row_tab-o_ettim
                  i_date2 = $l_dt
                  i_time2 = $l_tm
             IMPORTING
                  e_tdiff = diff_tim.

        IF sy-subrc <> 0.
        ENDIF.
        $diff_tim = diff_tim.
        $day = $diff_tim(4).
        $hour = $diff_tim+4(2).
        $min = $diff_tim+6(2).

        it_row_tab-p_full_tim = ( $day * 24 ) + $hour + ( $min / 60 ).

        $total_h = ( $day * 24 ) + $hour.
        $total_m = $min.

        WRITE $total_h TO str_total_h LEFT-JUSTIFIED NO-ZERO.
        WRITE $total_m TO str_total_m LEFT-JUSTIFIED NO-ZERO.

        CONCATENATE '^^^' str_total_h 'h^^' str_total_m 'm'
        INTO it_row_tab-full_tim.
        CONDENSE it_row_tab-full_tim NO-GAPS.
        REPLACE '^^^h' WITH '' INTO it_row_tab-full_tim.
        REPLACE '^^^' WITH '' INTO it_row_tab-full_tim.
        REPLACE '^^m' WITH '' INTO it_row_tab-full_tim.
        REPLACE '^^' WITH '' INTO it_row_tab-full_tim.
      ENDIF.
      IF it_row_tab-o_pkbmg <> it_row_tab-i_menge.
        it_row_tab-diff_qty = true.
      ENDIF.

    ELSE.

      IF NOT it_row_tab-o_etdat IS INITIAL.

        IF NOT it_row_tab-o_rsnum IS INITIAL.
          SELECT SINGLE cpudt cputm INTO
          (it_row_tab-i_etdat, it_row_tab-i_ettim)
          FROM mseg INNER JOIN mkpf
          ON mkpf~mblnr EQ mseg~mblnr
          AND mkpf~mjahr EQ mseg~mjahr
          WHERE mseg~rsnum EQ it_row_tab-o_rsnum.
          $subrc = sy-subrc.

          IF $subrc NE 0.
            SELECT SINGLE xloek INTO $xloek
              FROM resb
              WHERE rsnum EQ it_row_tab-o_rsnum
                AND xloek = 'X'.
            IF sy-subrc EQ 0.
              it_row_tab-i_etdat = it_row_tab-o_etdat.
              it_row_tab-i_ettim = it_row_tab-o_ettim.
              it_row_tab-manual = 'D'.
            ENDIF.
            $subrc = sy-subrc.
          ELSE.
            it_row_tab-manual = 'X'.
          ENDIF.
        ELSE.
          it_row_tab-i_etdat = it_row_tab-o_etdat.
          it_row_tab-i_ettim = it_row_tab-o_ettim.
          $subrc = 0.
        ENDIF.

        IF $subrc EQ 0.
          CALL FUNCTION 'SD_CALC_DURATION_FROM_DATETIME'
               EXPORTING
                    i_date1 = it_row_tab-o_etdat
                    i_time1 = it_row_tab-o_ettim
                    i_date2 = it_row_tab-i_etdat
                    i_time2 = it_row_tab-i_ettim
               IMPORTING
                    e_tdiff = diff_tim.

        ELSE.

          CALL FUNCTION 'SD_CALC_DURATION_FROM_DATETIME'
               EXPORTING
                    i_date1 = it_row_tab-o_etdat
                    i_time1 = it_row_tab-o_ettim
                    i_date2 = sy-datum
                    i_time2 = sy-uzeit
               IMPORTING
                    e_tdiff = diff_tim.
        ENDIF.

        $diff_tim = diff_tim.
        $day = $diff_tim(4).
        $hour = $diff_tim+4(2).
        $min = $diff_tim+6(2).

        it_row_tab-p_open_tim = ( $day * 24 ) + $hour + ( $min / 60 ).

        $total_h = ( $day * 24 ) + $hour.
        $total_m = $min.

        WRITE $total_h TO str_total_h LEFT-JUSTIFIED NO-ZERO.
        WRITE $total_m TO str_total_m LEFT-JUSTIFIED NO-ZERO.

        CONCATENATE '^^^' str_total_h 'h^^' str_total_m 'm'
        INTO it_row_tab-open_tim.
        CONDENSE it_row_tab-open_tim NO-GAPS.
        REPLACE '^^^h' WITH '' INTO it_row_tab-open_tim.
        REPLACE '^^^' WITH '' INTO it_row_tab-open_tim.
        REPLACE '^^m' WITH '' INTO it_row_tab-open_tim.
        REPLACE '^^' WITH '' INTO it_row_tab-open_tim.
      ENDIF.
    ENDIF.

    MODIFY  it_row_tab INDEX $ix TRANSPORTING p_open_tim
           p_full_tim open_tim full_tim diff_qty manual i_etdat i_ettim.


  ENDLOOP.

ENDFORM.                    " view_from_table

*&---------------------------------------------------------------------*
*&      Form  finalize
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM finalize.
  gt_row_tab[] = it_row_tab[].


ENDFORM.                    " finalize
*&---------------------------------------------------------------------*
*&      Form  set_output
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_output.
  CHECK g_error IS INITIAL.

  PERFORM show_progress     USING 'Preparing screen...' '95'.
  PERFORM init_alv_parm.
  PERFORM fieldcat_init     USING gt_fieldcat[].
  PERFORM sort_build        USING gt_sort[].
  PERFORM alv_events_get    USING:  'P', 'T'.
  PERFORM alv_grid_display  TABLES  gt_out USING ''.

ENDFORM.                    " set_output
*&---------------------------------------------------------------------*
*&      Form  init_alv_parm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_alv_parm.

  __cls   :  gt_fieldcat, gt_sort, gt_events, gt_listheader,
             gt_sp_group.

  CLEAR   :  gs_layout.

  gs_layout-colwidth_optimize = 'X'.

*   Set variant
  gv_repid = gs_variant-report = sy-repid.
  gs_variant-variant = p_vari.

ENDFORM.                    " INIT_ALV_PARM
*&---------------------------------------------------------------------*
*&      Form  fieldcat_init
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCAT[]  text
*----------------------------------------------------------------------*
FORM fieldcat_init USING ft_fieldcat TYPE slis_t_fieldcat_alv .

  DATA: l_pos TYPE i.

  __cls ft_fieldcat.

  DEFINE __catalog.
    l_pos = l_pos + 1.
    clear gs_fieldcat.
    gs_fieldcat-col_pos       = l_pos.
    gs_fieldcat-key           = &1.
    gs_fieldcat-fieldname     = &2.
    gs_fieldcat-seltext_m     = &3.        " Column heading
    gs_fieldcat-outputlen     = &4.        " Column width
    gs_fieldcat-datatype      = &5.        " Data type
    gs_fieldcat-emphasize     = &6.
    gs_fieldcat-cfieldname    = &7.
    gs_fieldcat-no_zero       = &8.

    if gs_fieldcat-fieldname eq 'O_PKBMG' or
        gs_fieldcat-fieldname eq 'I_MENGE'.
    endif.

    append gs_fieldcat to  ft_fieldcat.
  END-OF-DEFINITION.

  __catalog :
    'X' 'O_PKKEY'     'Kanban#'      10  'NUMC' '' '' '',
    'X' 'O_RSNUM'     'RSV#'         10  'NUMC' '' '' '',
    'X' 'O_RSPOS'     'Item'          4  'NUMC' '' '' '',
    'X' 'O_REVERSED'  'R'             1  'CHAR' ' ' '' '',
    ' ' 'O_SAEDT'     'TgDt'            8  'DATS' ' ' '' '',
    ' ' 'O_SAEUZ'     'TgTm'            6  'TIMS' ' ' '' 'X',
    ' ' 'O_WERKS'     'Plant'           4  'CHAR' ' ' '' '',
    ' ' 'O_LGORT'     'SLoc'            4  'CHAR' ' ' '' '',
    ' ' 'O_LGPRO'     'ISLo'            4  'CHAR' ' ' '' '',
    ' ' 'O_PRVBE'     'Sply Area'    10 'CHAR' ' ' '' '',
    ' ' 'O_ZFEEDER'   'Feeder'          5  'CHAR' ' ' '' '',
    ' ' 'O_MATNR'     'Material Number' 18  'CHAR' ' ' '' '',
    ' ' 'O_ABLAD'     'Str.Position' 25  'CHAR' ' ' '' '',
    ' ' 'O_PKBMG'     'R-Qty'    13    'QUAN' ' ' ' ' 'X',
    ' ' 'O_MEINS'     'UoM'            3  'UNIT' ' ' '' '',
    ' ' 'O_PKTIM'     'Dlv.time'      15  'DEC' ' ' '' 'X',
    ' ' 'O_KWBZM'     'Rep.Ld.Time'  11  'DEC' ' ' '' 'X',
    ' ' 'O_RKSTA'     'C'             1  'CHAR' ' ' '' '',
    ' ' 'O_SFGSN'     'S'             1  'CHAR' ' ' '' '',
    ' ' 'O_TYPE'      'Send Status'   1  'CHAR' ' ' '' '',
    ' ' 'O_MESSAGE'   'Message'      40  'CHAR' ' ' '' '',
    ' ' 'O_ATNAM'     'Chg.User'     12  'CHAR' ' ' '' '',
    ' ' 'O_ATDAT'     'Chg.Date'      8  'DATS' ' ' '' '',
    ' ' 'O_ATTIM'     'Chg.Time'      6  'CHAR' ' ' '' '',
    ' ' 'O_ETNAM'     'Cre.User'     12  'CHAR' ' ' '' '',
    ' ' 'O_ETDAT'     'Cre.Date'      8  'DATS' ' ' '' '',
    ' ' 'O_ETTIM'     'Cre.Time'      6  'TIMS' ' ' '' 'X',
*-----------------------------------------------------------*
    ' ' 'I_PKKEY'     'Kanban#'      10  'NUMC' 'C311' '' '',
    ' ' 'I_RSNUM'     'RSV#'         10  'NUMC' 'C311' '' '',
    ' ' 'I_RSPOS'     'Item'          4  'NUMC' 'C311' '' '',
    ' ' 'I_SAEDT'     'DockInDt'      8  'DATS' 'C311' '' '',
    ' ' 'I_SAEUZ'     'DockInTm'      8  'TIMS' 'C311' '' 'X',
    ' ' 'I_BUDAT'     'PstDate'       8  'DATS' 'C311' '' '',
    ' ' 'I_FERTH'     'Memo'         18  'CHAR' 'C311' '' '',
    ' ' 'I_FORMT'     'Format'        4  'CHAR' 'C311' '' '',
    ' ' 'I_ZFEEDER'   'Feeder'       18  'CHAR' 'C311' '' '',
    ' ' 'I_MATNR'     'Material Number'  18 'CHAR' 'C311' '' '',
    ' ' 'I_WERKS'     'Plant'         4  'CHAR' 'C311' '' '',
    ' ' 'I_LGORT'     'SLoc'          4  'CHAR' 'C311' '' '',
    ' ' 'I_UMLGO'     'R-Sloc'        4  'CHAR' 'C311' '' '',
    ' ' 'I_MENGE'     'Qty'          13  'QUAN' 'C311' ' ' 'X',
    ' ' 'I_MEINS'     'UoM'           3  'UNIT' 'C311' '' '',
    ' ' 'I_WEMPF'     'G/R'          12  'DEC' 'C311' '' '',
    ' ' 'I_MBLNR'     'Mat. doc.'    10  'CHAR' 'C311' '' '',
    ' ' 'I_TYPE'      'T'             1  'CHAR' 'C311' '' '',
    ' ' 'I_MESSAGE'   'Message'      40  'CHAR' 'C311' '' '',
    ' ' 'I_ATNAM'     'Chg.User'     12  'CHAR' 'C311' '' '',
    ' ' 'I_ATDAT'     'Chg.Date'      8  'DATS' 'C311' '' '',
    ' ' 'I_ATTIM'     'Chg.Time'      6  'CHAR' 'C311' '' 'X',
    ' ' 'I_ETNAM'     'Cre.User'     12  'CHAR' 'C311' '' '',
    ' ' 'I_ETDAT'     'Cre.Date'      8  'DATS' 'C311' '' '',
    ' ' 'I_ETTIM'     'Cre.Time'      6  'TIMS' 'C311' '' 'X',

    ' ' 'OPEN_TIM'    'Open'         10  'CHAR' 'C511' '' 'X',
    ' ' 'FULL_TIM'    'Fulfill'      10  'CHAR' 'C511' '' 'X',
    ' ' 'P_OPEN_TIM'  'Open'         10  'DEC' 'C511' '' 'X',
    ' ' 'P_FULL_TIM'  'Fulfill'      10  'DEC' 'C511' '' 'X',

    ' ' 'MANUAL'  'Manual'      1  'CHAR' 'C511' '' 'X'.



ENDFORM.                    " fieldcat_init
*&---------------------------------------------------------------------*
*&      Form  sort_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_SORT[]  text
*----------------------------------------------------------------------*
FORM sort_build USING    ft_sort TYPE slis_t_sortinfo_alv.

  DEFINE sort_tab.
    clear gs_sort.
    gs_sort-fieldname = &1.
    gs_sort-spos      = &2.
    gs_sort-up        = &3.
    gs_sort-group     = &4.
    gs_sort-comp      = &5.
    append gs_sort to ft_sort.
  END-OF-DEFINITION.

*  sort_tab :
*     'SACHZ'    ' ' 'X' 'X' 'X',
*     'SACHN'    ' ' 'X' 'X' 'X',
*     'BTRTL'    ' ' 'X' 'X' 'X',
*     'BTEXT'    ' ' 'X' 'X' 'X',
*     'ORGEH'    ' ' 'X' 'X' 'X',
*     'ORGTX'    ' ' 'X' 'X' 'X',
*     'KOSTL'    ' ' 'X' 'X' 'X',
*     'KOSTX'    ' ' 'X' 'X' 'X',
*     'SCHKZ'    ' ' 'X' 'X' 'X',
*     'RTEXT'    ' ' 'X' 'X' 'X',
*     'PERNR'    ' ' 'X' 'X' 'X',
*     'SNAME'    ' ' 'X' 'X' 'X'.
**
ENDFORM.                    " SORT_BUILD
*---------------------------------------------------------------------*
*       FORM user_command                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  FP_UCOMM                                                      *
*  -->  FS                                                            *
*---------------------------------------------------------------------*
FORM user_command USING fp_ucomm LIKE sy-ucomm
                        fs       TYPE slis_selfield.
  CLEAR : g_error.
  FIELD-SYMBOLS : <ls_line>, <matnr>.

  CASE fp_ucomm.
    WHEN '&IC1'.           "Double-Click
      SET PARAMETER ID 'RES' FIELD fs-value.
      CALL TRANSACTION 'MB23' AND SKIP FIRST SCREEN.

  ENDCASE.

ENDFORM.                    "USER_COMMAND

*---------------------------------------------------------------------*
*       FORM top_of_page                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM top_of_page.
  DATA l_text(140).
  DATA l_text2(10).
  DATA l_text3(40).
  DATA l_text4(10).
  DATA l_text5(10).
  DATA l_text41(25).
  DATA l_text51(25).

  REFRESH gt_listheader.


  CASE true.
    WHEN p_opt.
      l_text3 = ' All: '.
    WHEN p_opt2.
      l_text3 = ' Open: '.
    WHEN p_opt3.
      l_text3 = ' Complete: '.
    WHEN p_opt4.
      l_text3 = ' Missing: '.
    WHEN OTHERS.
  ENDCASE.

  l_text = ''.
  WRITE g_cnt TO l_text2.

  IF g_cnt2 > 0.
    WRITE g_cnt2 TO l_text4.
    CONCATENATE ' (manual trf:' l_text4 ')' INTO l_text41.
  ENDIF.
  IF g_cnt3 > 0.
    WRITE g_cnt3 TO l_text5.
    CONCATENATE ' (deleted:' l_text5 ')' INTO l_text51.
  ENDIF.

  WRITE g_cnt3 TO l_text4.
  CONCATENATE l_text l_text3 l_text2 l_text41 l_text51
         INTO l_text.

  PERFORM set_header_line USING:
          'P' 'H' ''      l_text       '',
          'S' 'S' 'Reservation' s_rsnum-low s_rsnum-high,
          'S' 'S' 'Creation Date' s_etdat-low s_etdat-high,
          'S' 'S' 'Feeder' s_zfeed-low s_zfeed-high,
          'S' 'S' 'Supply Area' s_prvbe-low s_prvbe-high,
          'S' 'S' 'Pers.resp.for source' s_rgver-low s_rgver-high,
          'S' 'S' 'MRP controller' s_dispo-low s_dispo-high,
          'S' 'S' 'Material Numbe' s_matnr-low s_matnr-high,
          'S' 'S' 'Issue stor. location' s_lgpro-low s_lgpro-high.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            it_list_commentary = gt_listheader.

ENDFORM.                    "top_of_page

**---------------------------------------------------------------------*
**       FORM PF_STATUS_SET
**---------------------------------------------------------------------*
FORM pf_status_set USING  ft_extab TYPE slis_t_extab.
  SET PF-STATUS '100'.
ENDFORM.                    "PF_STATUS_SET
*&---------------------------------------------------------------------*
*&      Form  get_emp_categ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ROW_TAB_PERSG  text
*      -->P_IT_ROW_TAB_PERSK  text
*      <--P_IT_ROW_TAB_CATEG  text
*----------------------------------------------------------------------*
FORM get_emp_categ USING    f_persg
                            f_persk
                   CHANGING f_categ.

*parameters: p_eg1(1)   type c default 'A' no-display,  "US-Salary
*            p_eg2(1)   type c default 'B' no-display,  "US-Wage
*            p_eg3(1)   type c default 'K' no-display.  "KR-Salary

  CONSTANTS:
   c_eg1(1)   TYPE c VALUE   'A',"US-Salary
   c_eg2(1)   TYPE c VALUE   'B',"US-Wage
   c_eg3(1)   TYPE c VALUE   'K'."KR-Salary

  IF f_persg = '9' AND f_persk = 'U2'.
    f_categ = c_eg3.
  ELSEIF ( ( f_persg = '1' AND f_persk = 'U2' ) OR
           ( f_persg = '1' AND f_persk = 'U3' ) ).
    f_categ = c_eg1.
  ELSE.
    f_categ = c_eg2.
  ENDIF.

ENDFORM.                    " get_emp_categ
*&---------------------------------------------------------------------*
*&      Form  get_from_table_completed
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_from_table_completed.

  PERFORM show_progress USING 'Get Completed order...' 10.

  SELECT
          zmmt0038~pkkey
          zmmt0038~rsnum
          zmmt0038~rspos
          zmmt0038~reversed
          zmmt0038~saedt
          zmmt0038~saeuz
          zmmt0038~werks
          zmmt0038~lgort
          zmmt0038~lgpro
          zmmt0038~prvbe
          zmmt0038~zfeeder
          zmmt0038~matnr
          zmmt0038~ablad
          zmmt0038~pkbmg
          zmmt0038~meins
          zmmt0038~pktim
          zmmt0038~kwbzm
          zmmt0038~rksta
          zmmt0038~sfgsn
          zmmt0038~type
          zmmt0038~message
          zmmt0038~atnam
          zmmt0038~atdat
          zmmt0038~attim
          zmmt0038~etnam
          zmmt0038~etdat
          zmmt0038~ettim

  INTO CORRESPONDING FIELDS OF TABLE it_o_tab
  FROM  zmmt0038 INNER JOIN marc
  ON  marc~matnr EQ zmmt0038~matnr
  AND marc~werks EQ zmmt0038~werks
  INNER JOIN pkps
      ON pkps~pkkey EQ zmmt0038~pkkey
  INNER JOIN pkhd
      ON pkhd~pknum EQ pkps~pknum
  WHERE zmmt0038~rsnum IN s_rsnum
   AND zmmt0038~etdat IN s_etdat
   AND zmmt0038~zfeeder IN s_zfeed
   AND zmmt0038~matnr IN s_matnr
   AND zmmt0038~prvbe IN s_prvbe
   AND marc~dispo IN s_dispo
   AND zmmt0038~lgpro IN s_lgpro
   AND pkhd~rgver IN s_rgver
   AND EXISTS ( SELECT * FROM zmmt0032
                    WHERE pkkey EQ zmmt0038~pkkey
                    AND rsnum EQ zmmt0038~rsnum
                    AND rspos EQ zmmt0038~rspos
                    AND budat IN s_budat
                    AND mblnr  IN s_mblnr
                   )
  %_hints ORACLE 'FIRST_ROWS(10)'.

  IF NOT it_o_tab[] IS INITIAL.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_i_tab
    FROM  zmmt0032
    FOR ALL ENTRIES IN it_o_tab
    WHERE budat IN s_budat
     AND  mblnr  IN s_mblnr
     AND rsnum EQ it_o_tab-rsnum
     AND matnr IN s_matnr.

    IF sy-subrc NE 0.
      MESSAGE s000 WITH 'No Data was found!'.
      g_error = true.
      EXIT.
    ENDIF.

  ENDIF.

  CHECK NOT it_i_tab[] IS INITIAL.
  SORT it_i_tab BY rsnum.

  PERFORM show_progress USING 'Get Outbound Order...' 40.

  SORT it_o_tab BY rsnum.

  LOOP AT it_o_tab.

    IF it_o_tab-reversed EQ true.
      it_revers-pkkey = it_o_tab-pkkey.
      it_revers-rsnum = it_o_tab-rsnum.
      it_revers-rspos = it_o_tab-rspos.
      APPEND it_revers.
    ENDIF.

  ENDLOOP.

  IF p_revr EQ true.
    LOOP AT it_revers.
      DELETE it_o_tab WHERE pkkey EQ it_revers-pkkey
                        AND rsnum EQ it_revers-rsnum
                        AND reversed EQ false.
    ENDLOOP.
  ELSE.
    LOOP AT it_revers.
      DELETE it_o_tab WHERE pkkey EQ it_revers-pkkey
                        AND rsnum EQ it_revers-rsnum.
    ENDLOOP.
  ENDIF.

  LOOP AT it_i_tab.

*    if it_i_tab-rsnum eq '3319985'.
*      break-point.
*    endif.

    it_row_tab-i_pkkey      = it_i_tab-pkkey.
    it_row_tab-i_rsnum      = it_i_tab-rsnum.
    it_row_tab-i_rspos      = it_i_tab-rspos.
    it_row_tab-i_saedt      = it_i_tab-saedt.
    it_row_tab-i_saeuz     = it_i_tab-saeuz.
    it_row_tab-i_budat      = it_i_tab-budat.
    it_row_tab-i_ferth      = it_i_tab-ferth.
    it_row_tab-i_formt      = it_i_tab-formt.
    it_row_tab-i_zfeeder    = it_i_tab-zfeeder.
    it_row_tab-i_matnr      = it_i_tab-matnr.
    it_row_tab-i_werks      = it_i_tab-werks.
    it_row_tab-i_lgort      = it_i_tab-lgort.
    it_row_tab-i_umlgo      = it_i_tab-umlgo.
    it_row_tab-i_menge      = it_i_tab-menge.
    it_row_tab-i_meins      = it_i_tab-meins.
    it_row_tab-i_wempf      = it_i_tab-wempf.
    it_row_tab-i_mblnr      = it_i_tab-mblnr.
    it_row_tab-i_mjahr      = it_i_tab-mjahr.
    it_row_tab-i_type       = it_i_tab-type.
    it_row_tab-i_message    = it_i_tab-message.
    it_row_tab-i_atnam      = it_i_tab-atnam.
    it_row_tab-i_atdat      = it_i_tab-atdat.
    it_row_tab-i_attim      = it_i_tab-attim.
    it_row_tab-i_etnam      = it_i_tab-etnam.
    it_row_tab-i_etdat      = it_i_tab-etdat.
    it_row_tab-i_ettim      = it_i_tab-ettim.

    READ TABLE it_o_tab WITH KEY rsnum = it_i_tab-rsnum BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_row_tab-o_pkkey      = it_o_tab-pkkey.
      it_row_tab-o_rsnum      = it_o_tab-rsnum.
      it_row_tab-o_rspos      = it_o_tab-rspos.
      it_row_tab-o_reversed   = it_o_tab-reversed.
      it_row_tab-o_saedt      = it_o_tab-saedt.
      it_row_tab-o_saeuz      = it_o_tab-saeuz.
      it_row_tab-o_werks      = it_o_tab-werks.
      it_row_tab-o_lgort      = it_o_tab-lgort.
      it_row_tab-o_lgpro      = it_o_tab-lgpro.
      it_row_tab-o_prvbe      = it_o_tab-prvbe.
      it_row_tab-o_zfeeder    = it_o_tab-zfeeder.
      it_row_tab-o_matnr      = it_o_tab-matnr.
      it_row_tab-o_ablad      = it_o_tab-ablad.
      it_row_tab-o_pkbmg      = it_o_tab-pkbmg.
      it_row_tab-o_meins      = it_o_tab-meins.
      it_row_tab-o_pktim      = it_o_tab-pktim.
      it_row_tab-o_kwbzm      = it_o_tab-kwbzm.
      it_row_tab-o_rksta      = it_o_tab-rksta.
      it_row_tab-o_sfgsn      = it_o_tab-sfgsn.
      it_row_tab-o_type       = it_o_tab-type.
      it_row_tab-o_message    = it_o_tab-message.
      it_row_tab-o_atnam      = it_o_tab-atnam.
      it_row_tab-o_atdat      = it_o_tab-atdat.
      it_row_tab-o_attim      = it_o_tab-attim.
      it_row_tab-o_etnam      = it_o_tab-etnam.
      it_row_tab-o_etdat      = it_o_tab-etdat.
      it_row_tab-o_ettim      = it_o_tab-ettim.

    ENDIF.
    APPEND it_row_tab. CLEAR it_row_tab.
  ENDLOOP.

  IF p_opt2 EQ true.
    LOOP AT it_row_tab.
      $ix = sy-tabix.
      IF NOT it_row_tab-i_rsnum IS INITIAL.
        DELETE it_row_tab INDEX $ix.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF p_opt3 EQ true.
    LOOP AT it_row_tab.
      $ix = sy-tabix.
      IF it_row_tab-i_rsnum IS INITIAL.
        DELETE it_row_tab INDEX $ix.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " get_from_table_completed
*&---------------------------------------------------------------------*
*&      Form  get_from_resb
*&---------------------------------------------------------------------*
FORM get_from_resb.

  SELECT a~pkkey a~saedt a~saeuz a~rsnum a~pkimg
         b~matnr b~werks b~prvbe b~zfeeder
     INTO TABLE it_resb
     FROM pkps AS a
     INNER JOIN pkhd AS b
        ON a~pknum  = b~pknum
     WHERE a~pkbst = '2'         "empty
       AND a~rsnum IN s_rsnum
       AND a~saedt IN s_etdat
       AND b~zfeeder IN s_zfeed
       AND b~matnr IN s_matnr
       AND b~prvbe IN s_prvbe
       AND NOT exists ( SELECT * FROM zmmt0038
                          WHERE pkkey EQ a~pkkey
                            AND rsnum EQ a~rsnum
                      ).

*  DATA: lv_idx LIKE sy-index.
*
*  LOOP AT it_resb.
*    lv_idx = sy-tabix.
*    SELECT SINGLE * FROM zmmt0038
*      WHERE pkkey = it_resb-pkkey
*        AND rsnum = it_resb-rsnum.
*    IF sy-subrc = 0.
*      DELETE it_resb INDEX lv_idx.
*    ENDIF.
*  ENDLOOP.

ENDFORM.                    " get_from_resb
*ALV
*&--------------------------------------------------------------------
*&      Form  make_field_category
*&--------------------------------------------------------------------
FORM field_setting TABLES         p_fieldcat_t LIKE gt_fieldcat
                   USING          p_fieldname       " FIELD name
                                  p_title           " field titlw
                                  p_outputlen       " length
                                  p_key             "
                                  p_just            "
                                  p_noout           "
                                  p_round           "
                                  p_cfield          " currency field nam
                                  p_qfield          " quantity field nam
                                  p_dosum           " make sum
                                  .

  DATA: ls_fieldcat TYPE slis_fieldcat_alv.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname  = p_fieldname.
*  ls_fieldcat-seltext_s = p_title.
*  ls_fieldcat-seltext_m = p_title.
  ls_fieldcat-seltext_l  = p_title.
  ls_fieldcat-outputlen  = p_outputlen.
  ls_fieldcat-key        = p_key.
  ls_fieldcat-just       = p_just.
  ls_fieldcat-edit       = ''.   "p_edit.
  ls_fieldcat-no_out     = p_noout.
  ls_fieldcat-decimals_out   = p_round.
*  ls_fieldcat-cfieldname = p_cfield.
  ls_fieldcat-currency   = p_cfield.
  ls_fieldcat-qfieldname = p_qfield.
  ls_fieldcat-do_sum     = p_dosum.

  APPEND ls_fieldcat TO p_fieldcat_t.

ENDFORM.                    " fill_field_category
*ALV
*&---------------------------------------------------------------------*
*&      Form  display_missing_alv
*&---------------------------------------------------------------------*
FORM display_missing_alv.

  PERFORM field_setting TABLES gt_fieldcat USING :
 'PKKEY'   'Kanban'             '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'SAEDT'   'Date'               '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'SAEUZ'   'Time'               '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'RSNUM'   'Reservation'        '12' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'PKIMG'   'Qty'                '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
 'MATNR'   'Material'           '18' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'WERKS'   'Plant'              '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'PRVBE'   'Supply area'        '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'ZFEEDER' 'Feeder'             '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' '.

  g_repid = sy-repid.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
     i_callback_program = g_repid
     it_fieldcat        = gt_fieldcat
*     i_structure_name   = 'ZPPC_ACT_CONF'
     i_save             = 'A'
   TABLES
     t_outtab           = it_resb
   EXCEPTIONS
     program_error      = 1
     OTHERS             = 2.

ENDFORM.                    " display_missing_alv
