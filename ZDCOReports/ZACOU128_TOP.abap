*----------------------------------------------------------------------*
*   INCLUDE ZACOU128_TOP                                               *
*----------------------------------------------------------------------*
INCLUDE zacoui00.
TABLES : mara,tc31a, sscrfields, ztcou128, *ztcou128, *tc31a,
         ztcou129,*ztcou129 .

INCLUDE <icon>.                        " icon
*----------------------------------------------------------------------*
* Macros
*----------------------------------------------------------------------*

DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.

DEFINE __process.
  perform show_progress using &1 &2.
END-OF-DEFINITION.

DEFINE __message.
  call function 'POPUP_TO_INFORM'
       exporting
            titel = &1
            txt1  = &2
            txt2  = sy-subrc.
END-OF-DEFINITION.

DEFINE __focus.
  call method cl_gui_control=>set_focus
      exporting
        control = &1 .
END-OF-DEFINITION.

****************************** constants *******************************
CONSTANTS:  false VALUE ' ',
            true  VALUE 'X'.

DATA: g_error(1),
      g_repid  LIKE sy-repid,
      g_ix     LIKE sy-tabix.

DATA  g_proc_type(3).

****************************** Global Data *****************************
DATA : icon_not_ready TYPE icon_d,
       icon_ready TYPE icon_d,
       icon_confirmed TYPE icon_d,
       icon_released TYPE icon_d.

FIELD-SYMBOLS: <f_field>, <f_field2>.

DATA : it_cosl_temp LIKE cosl OCCURS 0 WITH HEADER LINE.
DATA : BEGIN OF it_cosl OCCURS 0 ,
        lednr    LIKE cosl-lednr,
        objnr    LIKE cosl-objnr,
        kostl    TYPE kostl,
        gjahr    LIKE cosl-gjahr,
        wrttp    LIKE cosl-wrttp,
        versn    LIKE cosl-versn,
        vrgng    LIKE cosl-vrgng,
        perbl    LIKE cosl-perbl,
        kap001   LIKE cosl-kap001,
        kap002   LIKE cosl-kap002,
        kap003   LIKE cosl-kap003,
        kap004   LIKE cosl-kap004,
        kap005   LIKE cosl-kap005,
        kap006   LIKE cosl-kap006,
        kap007   LIKE cosl-kap007,
        kap008   LIKE cosl-kap008,
        kap009   LIKE cosl-kap009,
        kap010   LIKE cosl-kap010,
        kap011   LIKE cosl-kap011,
        kap012   LIKE cosl-kap012,
        mh001    LIKE cosl-kap001,
        mh002    LIKE cosl-kap002,
        mh003    LIKE cosl-kap003,
        mh004    LIKE cosl-kap004,
        mh005    LIKE cosl-kap005,
        mh006    LIKE cosl-kap006,
        mh007    LIKE cosl-kap007,
        mh008    LIKE cosl-kap008,
        mh009    LIKE cosl-kap009,
        mh010    LIKE cosl-kap010,
        mh011    LIKE cosl-kap011,
        mh012    LIKE cosl-kap012,
       END OF it_cosl .

DATA: g_startdt LIKE sy-datum.

DATA : BEGIN OF it_mara OCCURS 0,
       mtart LIKE mara-mtart,
       matnr LIKE mara-matnr,
       bismt LIKE mara-bismt,
       END OF it_mara.

DATA : BEGIN OF it_rate OCCURS 0,
       matnr LIKE mara-matnr,
       bismt LIKE mara-bismt,
       END OF it_rate.

DATA : BEGIN OF it_product OCCURS 0,
       matnr LIKE mara-matnr,
       bismt LIKE mara-bismt,
       END OF it_product.

DATA : BEGIN OF it_crhd OCCURS 0,
       objid LIKE crhd-objid,
       arbpl LIKE crhd-arbpl,
       END OF it_crhd.

DATA : BEGIN OF it_marc OCCURS 0,
       werks LIKE marc-werks,
       matnr LIKE marc-matnr,
       sauft LIKE marc-sauft,
       END OF it_marc.

DATA : BEGIN OF it_mi OCCURS 0,
       plnnr LIKE plko-plnnr,
       matnr LIKE mara-matnr,
       bismt LIKE mara-bismt,
       END OF it_mi.

DATA : BEGIN OF it_mip OCCURS 0,
       matnr LIKE mara-matnr,
       sauft LIKE marc-sauft,
       bismt LIKE mara-bismt,
       END OF it_mip.

DATA: BEGIN OF db_plpo OCCURS 0,
          werks   LIKE plpo-werks,
          matnr   LIKE mara-matnr,
          plnty   LIKE plpo-plnty,
          plnnr   LIKE plpo-plnnr,
          plnkn   LIKE plpo-plnkn,
          arbid   LIKE plpo-arbid,
          zaehl   LIKE plpo-zaehl,
          datuv   LIKE plpo-datuv,

          vgw01   LIKE plpo-vgw01,    "Set
          vgw02   LIKE plpo-vgw02,    "Machine
          vgw03   LIKE plpo-vgw03,    "Labor

          vge01   LIKE plpo-vge01,    "Set
          vge02   LIKE plpo-vge02,    "Machine
          vge03   LIKE plpo-vge03,    "Labor
          aennr   LIKE plpo-aennr,
          plnal   LIKE mapl-plnal,
          aufak   LIKE plpo-aufak,
          datub   LIKE plpo-datuv,
          loekz   LIKE plpo-loekz,
         $flag(1),
        END OF db_plpo.

DATA : BEGIN OF it_plpo_temp OCCURS 0,
       plnty  LIKE plpo-plnty,
       plnnr  LIKE plpo-plnnr,
       plnkn  LIKE plpo-plnkn,
       zaehl  LIKE plpo-zaehl,
       datuv  LIKE plpo-datuv,
       arbid  LIKE plpo-arbid,
       aennr  LIKE plpo-aennr,
       vornr  LIKE plpo-vornr,
       werks  LIKE plpo-werks,
       lar01  LIKE plpo-lar01,
       vge01  LIKE plpo-vge01,
       vgw01  LIKE plpo-vgw01,
       lar02  LIKE plpo-lar02,
       vge02  LIKE plpo-vge02,
       vgw02  LIKE plpo-vgw02,
       lar03  LIKE plpo-lar03,
       vge03  LIKE plpo-vge03,
       vgw03  LIKE plpo-vgw03,
       zgr03  LIKE plpo-zgr03,
       END OF it_plpo_temp.
*production/rate routing
DATA : BEGIN OF it_plpo OCCURS 0,
        werks   LIKE plpo-werks,
        matnr   LIKE mara-matnr,
        plnty   LIKE plpo-plnty,
        type(1),
        arbid   LIKE plpo-arbid,
        vgw01   LIKE plpo-vgw01,    "Set
        vgw02   LIKE plpo-vgw02,    "Machine
        vgw03   LIKE plpo-vgw03,    "Labor
        vge01   LIKE plpo-vge01,    "Unit Set
        vge02   LIKE plpo-vge02,    "Unit Machine
        vge03   LIKE plpo-vge03,    "Unit Labor
        zgr03   LIKE plpo-zgr03,
        plnnr   LIKE plpo-plnnr,
        plnal   LIKE mapl-plnal,
        aufak(7),
      END OF it_plpo.
*reference rate routing.
DATA : BEGIN OF it_plpo_ref OCCURS 0,
        werks   LIKE plpo-werks,
        matnr   LIKE mara-matnr,
        plnty   LIKE plpo-plnty,
        type(1),
        arbid   LIKE plpo-arbid,
        vgw01   LIKE plpo-vgw01,    "Set
        vgw02   LIKE plpo-vgw02,    "Machine
        vgw03   LIKE plpo-vgw03,    "Labor
        vge01   LIKE plpo-vge01,    "Unit Set
        vge02   LIKE plpo-vge02,    "Unit Machine
        vge03   LIKE plpo-vge03,    "Unit Labor
        zgr03   LIKE plpo-zgr03,
        plnnr   LIKE plpo-plnnr,
        plnal   LIKE mapl-plnal,
        aufak(7),
      END OF it_plpo_ref.

DATA : BEGIN OF it_plaf OCCURS 0,
       perio(6),
       plwrk LIKE plaf-plwrk,
       matnr LIKE mara-matnr,
       gsmng LIKE plaf-gsmng,
       sauft LIKE marc-sauft,
       mtart LIKE mara-mtart,
       bismt LIKE mara-bismt,   "old material number
       sfepr LIKE marc-sfepr,
       $flag(1),
       END OF it_plaf.

DATA : BEGIN OF it_plaf_temp OCCURS 0,
       plnum LIKE plaf-plnum,
       plwrk LIKE plaf-plwrk,
       matnr LIKE plaf-matnr,
       gsmng LIKE plaf-gsmng,
       pedtr LIKE plaf-pedtr,
       sauft LIKE marc-sauft,
       mtart LIKE mara-mtart,
       bismt LIKE mara-bismt,
       sfepr LIKE marc-sfepr,
       END OF it_plaf_temp.

*DATA : IT_PLAF_TEMP LIKE PLAF OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF itab OCCURS 0,
       type(1),
       werks LIKE plaf-plwrk,
       matnr LIKE mara-matnr,
       kostl TYPE kostl,
       perio(6),
       eng_mh LIKE it_plaf-gsmng,
       cc_rate TYPE p DECIMALS 6,
       abp_mh LIKE it_plaf-gsmng,
       vgw01   LIKE plpo-vgw01,    "Set
       vgw02   LIKE plpo-vgw02,    "Machine
       vge01   LIKE plpo-vge01,    "Unit set
       vge02  LIKE plpo-vge02,     "Unit machine
       vge03   LIKE plpo-vge03,    "Unit Mh
       mtart LIKE mara-mtart,
       bismt LIKE mara-bismt,
       aufak(7),
       END OF itab .

DATA : BEGIN OF tmpt OCCURS 0,
         perio(6),
         kostl TYPE kostl,
         arbpl  LIKE crhd-arbpl,
         eng_mh LIKE it_plaf-gsmng,
       END OF tmpt .

DATA : BEGIN OF it_rout OCCURS 0,
       werks LIKE plaf-plwrk,
       matnr LIKE mara-matnr,
       type(1),
       bdc_type(1),
       kostl  TYPE kostl,
       abptot LIKE cosl-kap001,
       abp001 LIKE cosl-kap001,
       abp002 LIKE cosl-kap002,
       abp003 LIKE cosl-kap003,
       abp004 LIKE cosl-kap004,
       abp005 LIKE cosl-kap005,
       abp006 LIKE cosl-kap006,
       abp007 LIKE cosl-kap007,
       abp008 LIKE cosl-kap008,
       abp009 LIKE cosl-kap009,
       abp010 LIKE cosl-kap010,
       abp011 LIKE cosl-kap011,
       abp012 LIKE cosl-kap012,
       set001 LIKE cosl-kap001,
       set002 LIKE cosl-kap002,
       set003 LIKE cosl-kap003,
       set004 LIKE cosl-kap004,
       set005 LIKE cosl-kap005,
       set006 LIKE cosl-kap006,
       set007 LIKE cosl-kap007,
       set008 LIKE cosl-kap008,
       set009 LIKE cosl-kap009,
       set010 LIKE cosl-kap010,
       set011 LIKE cosl-kap011,
       set012 LIKE cosl-kap012,
       mch001 LIKE cosl-kap001,
       mch002 LIKE cosl-kap002,
       mch003 LIKE cosl-kap003,
       mch004 LIKE cosl-kap004,
       mch005 LIKE cosl-kap005,
       mch006 LIKE cosl-kap006,
       mch007 LIKE cosl-kap007,
       mch008 LIKE cosl-kap008,
       mch009 LIKE cosl-kap009,
       mch010 LIKE cosl-kap010,
       mch011 LIKE cosl-kap011,
       mch012 LIKE cosl-kap012,
       vge01  LIKE plpod-vge01,
       vge02  LIKE plpod-vge02,
       vge03  LIKE plpod-vge03,
       aufak(7), "  TYPE AUSCHUFAK,
       bismt LIKE mara-bismt,
       END OF it_rout .

DATA : it_bdc_rout LIKE it_rout OCCURS 0 WITH HEADER LINE.
DATA : BEGIN OF it_chg_rout OCCURS 0,
        matnr  LIKE mapl-matnr,
        werks  TYPE werks_d,
        plnty  LIKE plko-plnty,
        plnnr  LIKE plko-plnnr,
        plnal  LIKE plko-plnal,
        aennr  LIKE plko-aennr,
        datuv  LIKE plko-datuv,
        aufak(7), "  TYPE AUSCHUFAK,
        bismt LIKE mara-bismt,
       END OF it_chg_rout .

DATA : BEGIN OF it_chg_rout_del OCCURS 0,
        matnr  LIKE mapl-matnr,
        werks  TYPE werks_d,
        plnty  LIKE plko-plnty,
        plnnr  LIKE plko-plnnr,
        plnal  LIKE plko-plnal,
        aennr  LIKE plko-aennr,
        datuv  LIKE plko-datuv,
        aufak(7), "  TYPE AUSCHUFAK,
        bismt LIKE mara-bismt,
       END OF it_chg_rout_del .

TYPES : BEGIN OF ty_display,
       werks TYPE plwrk,
       matnr TYPE matnr,
       type(1),
       kostl  TYPE kostl,
       abptot TYPE kapxx,
       abp001 TYPE kapxx,
       abp002 TYPE kapxx,
       abp003 TYPE kapxx,
       abp004 TYPE kapxx,
       abp005 TYPE kapxx,
       abp006 TYPE kapxx,
       abp007 TYPE kapxx,
       abp008 TYPE kapxx,
       abp009 TYPE kapxx,
       abp010 TYPE kapxx,
       abp011 TYPE kapxx,
       abp012 TYPE kapxx,
       vge03  TYPE vgwrteh,
       aufak(7),
       bismt TYPE bismt,
       chk(1),
       del(1),
       END OF ty_display .


DATA : g_val_cnt(2) TYPE n.
DATA : g_screen(4) ,
       g_code(4).       "Transaction code

TYPES: BEGIN OF ty_row_tab.
        INCLUDE STRUCTURE ztcou128.
TYPES:
       $perio TYPE jahrper,
       cc_rate TYPE p DECIMALS 6,
       icon TYPE icon_d,
       icon_c TYPE icon_d,
       chk(1).
TYPES: END OF ty_row_tab.

TYPES: BEGIN OF ty_out.
INCLUDE  TYPE ty_row_tab.
TYPES   celltab  TYPE lvc_t_styl.
TYPES   tabcolor TYPE slis_t_specialcol_alv.
TYPES: END OF ty_out.

DATA  : it_display TYPE TABLE OF ty_display WITH HEADER LINE.
DATA  : it_row_tab TYPE TABLE OF ty_row_tab WITH HEADER LINE,
        gt_out     TYPE TABLE OF ty_out     WITH HEADER LINE.

DATA    gt_display LIKE it_display OCCURS 0 WITH HEADER LINE.
*----------------------------------------------------------------------*
* Define local class
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.

    TYPES: BEGIN OF ztcou128_k,
               kokrs TYPE kokrs,
               bdatj TYPE bdatj,
               versn TYPE versn,
               kostl TYPE kostl,
               abtei TYPE abtei,
           END OF ztcou128_k.

    TYPES: ztcou128_key   TYPE STANDARD TABLE OF ztcou128_k,
           ztcou128_table TYPE STANDARD TABLE OF ztcou128.

    METHODS:
      handle_data_changed
         FOR EVENT data_changed OF cl_gui_alv_grid
             IMPORTING er_data_changed,
                       get_deleted_rows
             EXPORTING
                       deleted_rows TYPE ztcou128_table,

      refresh_delta_tables.

  PRIVATE SECTION.
    DATA deleted_rows TYPE STANDARD TABLE OF ztcou128.
    DATA del_rows_display TYPE STANDARD TABLE OF ty_display.

* This flag is set if any error occured in one of the
* following methods:
    DATA: error_in_data TYPE c.
    METHODS:
      update_delta_tables
         IMPORTING
            pr_data_changed TYPE REF TO cl_alv_changed_data_protocol.

ENDCLASS.                   " LCL_EVENT_RECEIVER Definition

*----------------------------------------------------------------------*
* Implementation local class
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

* Setting for Change data
  METHOD handle_data_changed.

* remember deleted lines for saving
    CALL METHOD update_delta_tables( er_data_changed ).
    IF sy-dynnr EQ '0100' OR sy-dynnr EQ '100'.
      PERFORM data_changed USING er_data_changed.
    ELSE.
      PERFORM data_changed_200 USING er_data_changed.
    ENDIF.
  ENDMETHOD.                    " handle_data_changed

  METHOD get_deleted_rows.
    deleted_rows = me->deleted_rows.
  ENDMETHOD.

  METHOD refresh_delta_tables.
    CLEAR me->deleted_rows[].
    CLEAR me->del_rows_display[].
  ENDMETHOD.

  METHOD update_delta_tables.
    DATA: l_del_row TYPE lvc_s_moce,
          ls_ztcou128 TYPE ztcou128,
          ls_outtab LIKE LINE OF gt_out,
          ls_display LIKE LINE OF it_display,
          ls_del_display LIKE LINE OF it_display.
    IF sy-dynnr EQ '0100' OR sy-dynnr EQ '100'.
      LOOP AT pr_data_changed->mt_deleted_rows INTO l_del_row.
        READ TABLE gt_out INTO ls_outtab INDEX l_del_row-row_id.
        IF sy-subrc NE 0.
          MESSAGE i000(0k) WITH text-e01. "Internal error
        ELSE.
          MOVE-CORRESPONDING ls_outtab TO ls_ztcou128.
          APPEND ls_ztcou128 TO deleted_rows.
        ENDIF.
      ENDLOOP.
    ENDIF.
    IF sy-dynnr EQ '200' OR sy-dynnr EQ '0200'.
      LOOP AT pr_data_changed->mt_deleted_rows INTO l_del_row.
        READ TABLE it_display INTO ls_display INDEX l_del_row-row_id.
        IF sy-subrc NE 0.
          MESSAGE i000(0k) WITH text-e01. "Internal error
        ELSE.
          MOVE-CORRESPONDING ls_display TO ls_del_display.
          APPEND ls_display TO del_rows_display.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.

ENDCLASS.                   " LCL_EVENT_RECEIVER Implementation

*----------------------------------------------------------------------*
* Define local class
*----------------------------------------------------------------------*
CLASS lcl_event_route DEFINITION.
  PUBLIC SECTION.

    TYPES: BEGIN OF del_k,
             werks TYPE plwrk,
             matnr TYPE matnr,
             type(1),
             kostl  TYPE kostl,
           END OF del_k.

    TYPES: del_key   TYPE STANDARD TABLE OF del_k,
           del_table TYPE STANDARD TABLE OF ty_display.

    METHODS:
      handle_data_changed
         FOR EVENT data_changed OF cl_gui_alv_grid
             IMPORTING er_data_changed,
                       get_deleted_rows
             EXPORTING
                       deleted_rows TYPE del_table,
         refresh_delta_tables.

  PRIVATE SECTION.
    DATA del_rows_display TYPE STANDARD TABLE OF ty_display.

* This flag is set if any error occured in one of the
* following methods:
    DATA: error_in_data TYPE c.
    METHODS:
      update_delta_tables
         IMPORTING
            pr_data_changed TYPE REF TO cl_alv_changed_data_protocol.

    METHODS:
      get_cell_values
           IMPORTING
             row_id          TYPE int4
             pr_data_changed TYPE REF TO cl_alv_changed_data_protocol
           EXPORTING
             key             TYPE del_k.

ENDCLASS.                   " LCL_EVENT_RECEIVER Definition


*---------------------------------------------------------------------*
*       CLASS lcl_event_route IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS lcl_event_route IMPLEMENTATION.

* Setting for Change data
  METHOD handle_data_changed.

* remember deleted lines for saving
    CALL METHOD update_delta_tables( er_data_changed ).
    PERFORM data_changed_200 USING er_data_changed.
  ENDMETHOD.                    " handle_data_changed

  METHOD get_deleted_rows.
    deleted_rows = me->del_rows_display.
  ENDMETHOD.

  METHOD refresh_delta_tables.
    CLEAR me->del_rows_display[].
  ENDMETHOD.

  METHOD update_delta_tables.

*    DATA ls_key TYPE del_k.
*
*
*    DATA: l_del_row TYPE lvc_s_moce,
*          ls_display LIKE LINE OF it_display,
*          ls_del_display LIKE LINE OF it_display.
*
*    LOOP AT pr_data_changed->mt_deleted_rows INTO l_del_row.
*
*      IF sy-subrc NE 0.
*        MESSAGE i000(0k) WITH text-e01. "Internal error
*      ELSE.
*        MOVE-CORRESPONDING ls_display TO ls_del_display.
*        APPEND ls_display TO del_rows_display.
*        DELETE it_display WHERE werks = ls_display-werks
*                            AND type = ls_display-type
*                            AND matnr = ls_display-matnr
*                            AND kostl =  ls_display-kostl.
*        DELETE gt_display WHERE werks = ls_display-werks
*                            AND type = ls_display-type
*                            AND matnr = ls_display-matnr
*                            AND kostl =  ls_display-kostl.
*      ENDIF.
*    ENDLOOP.

  ENDMETHOD.

  METHOD get_cell_values.
* get values of key cells of row ROW_ID

* WERKS
    CALL METHOD pr_data_changed->get_cell_value
          EXPORTING
                 i_row_id    = row_id
                 i_fieldname = 'WERKS'
               IMPORTING
                 e_value = key-werks.

    IF sy-subrc NE 0.
      MESSAGE i000(0k) WITH text-e02.
    ENDIF.

* MATNR
    CALL METHOD pr_data_changed->get_cell_value
          EXPORTING
                 i_row_id    = row_id
                 i_fieldname = 'MATNR'
               IMPORTING
                 e_value = key-matnr.

    IF sy-subrc NE 0.
      MESSAGE i000(0k) WITH text-e02.
    ENDIF.
* TYPE
    CALL METHOD pr_data_changed->get_cell_value
          EXPORTING
                 i_row_id    = row_id
                 i_fieldname = 'TYPE'
               IMPORTING
                 e_value = key-type.

    IF sy-subrc NE 0.
      MESSAGE i000(0k) WITH text-e02.
    ENDIF.

* KOSTL
    CALL METHOD pr_data_changed->get_cell_value
          EXPORTING
                 i_row_id    = row_id
                 i_fieldname = 'KOSTL'
               IMPORTING
                 e_value = key-kostl.

    IF sy-subrc NE 0.
      MESSAGE i000(0k) WITH text-e02.
    ENDIF.

  ENDMETHOD.

ENDCLASS.                   " LCL_EVENT_RECEIVER Implementation

DATA g_event_receiver  TYPE REF TO lcl_event_receiver.
DATA g_event_route  TYPE REF TO lcl_event_route.

************************************************************************
DATA  : flag_data_changed,
        info(80).

* FOR bdc
DATA: BEGIN OF wa_result,
        matnr LIKE mara-matnr,
        werks LIKE marc-werks,
        plnnr LIKE plkod-plnnr,          "Group
        plnal LIKE plkod-plnal,          "Group counter
        datuv(10) TYPE c,                "Valid from date
        messa(100) TYPE c,
      END OF wa_result.

DATA: wa_bdcdata LIKE bdcdata,
      wa_opt     LIKE ctu_params,
      wa_msg LIKE bdcmsgcoll,
      it_msg LIKE TABLE OF wa_msg,
      it_result LIKE TABLE OF wa_result,
      it_bdcdata LIKE TABLE OF wa_bdcdata.

DATA: BEGIN OF ftab OCCURS 10,
        fcode(6),
      END OF ftab.
