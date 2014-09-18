*----------------------------------------------------------------------
* Program ID        : ZAFIU129
* Title             : [FI] Printing Label for Equipment
* Created on        : Nov.26 2007
* Created by        : I.G.MOON
* Specifications By : Andy Choi
* Description       : Printing Label for Equipment
*----------------------------------------------------------------------
* Modification Logs
* Date       Developer  Request ID  Description
* 02/15/2013 VALERIAN   UD1K956418  Include uncapitalize asset in data
*                                   selection
*----------------------------------------------------------------------
REPORT zafiu128 MESSAGE-ID zmco.
TABLES : anla, anlz, equi, t499s, ekkn, ekko, csks ,zsfiu129,
         *tka02, ankaz.

INCLUDE zacoui00.
TABLES : marc,sscrfields.
INCLUDE <icon>.                        " icon

*----------------------------------------------------------------------*
* Macros
*----------------------------------------------------------------------*

DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.

DEFINE __message.
  call function 'POPUP_TO_INFORM'
       EXPORTING
            titel = &1
            txt1  = &2
            txt2  = sy-subrc.
END-OF-DEFINITION.

DEFINE __focus.
  call method cl_gui_control=>set_focus
      exporting
        control = &1 .
END-OF-DEFINITION.

DEFINE __process.
  perform show_progress using &1 &2.
END-OF-DEFINITION.

DEFINE __set_icon_no_error.
  perform set_icon using pr_data_changed
                         ls_good-row_id
                         icon_led_yellow.
END-OF-DEFINITION.

DEFINE __set_icon_error.
  perform set_icon using pr_data_changed
                         ls_good-row_id
                         icon_led_red.
END-OF-DEFINITION.

DEFINE __set_error.

  call method pr_data_changed->add_protocol_entry
               exporting
    i_msgid = '0K' i_msgno = '000'  i_msgty = 'E'
    i_msgv1 = &1
    i_fieldname = ls_good-fieldname
    i_row_id = ls_good-row_id.

  error_in_data = 'X'.

END-OF-DEFINITION.

****************************** constants *******************************
* constants
CONSTANTS: out_to_screen  VALUE '1',
           out_to_printer VALUE '2',
           out_to_pc_file VALUE '3',
           dialog VALUE ' ',
           no_dialog VALUE 'X',
           directory(80) VALUE 'c:\temp\'. " Down directory

CONSTANTS:  false VALUE ' ',
            true  VALUE 'X'.

DATA: BEGIN OF fields OCCURS 10.
        INCLUDE STRUCTURE help_value.
DATA: END OF fields.
DATA: BEGIN OF valuetab OCCURS 10,
        line(80),
END OF valuetab.

DATA: pripar LIKE pri_params,          " ImageLink structure
      arcpar LIKE arc_params,          " ImageLink structure
      val,
      lay(16),
      lines TYPE i,
      rows TYPE i.
DATA numc1(1) TYPE n.

DATA: g_error(1),
      g_repid  LIKE sy-repid,
      g_ix     LIKE sy-tabix.

DATA : dir.
DATA cursor_f(10).
DATA $sheet(3) TYPE n.
FIELD-SYMBOLS <f>.

****************************** Global Data *****************************

TYPES: BEGIN OF ty_row_tab.
        INCLUDE STRUCTURE zsfiu129.
TYPES  eaufn TYPE am_aufnr.
TYPES  anlkl TYPE anlkl.
TYPES  eswerk type swerk.
TYPES  estort type stort.
TYPES  ektext type ktext.
TYPES: END OF ty_row_tab.

TYPES: BEGIN OF ty_out.
INCLUDE  TYPE ty_row_tab.
TYPES:
     icon TYPE icon-id,
     status(1),
     chk(1),
     err(1),
    $number(13).
TYPES   celltab  TYPE lvc_t_styl.
TYPES   tabcolor TYPE slis_t_specialcol_alv.
TYPES: END OF ty_out.

DATA  : itab       TYPE TABLE OF ty_row_tab WITH HEADER LINE,
        it_row_tab TYPE TABLE OF ty_row_tab WITH HEADER LINE,
        gt_out     TYPE TABLE OF ty_out     WITH HEADER LINE,
        gt_ztfiu128 TYPE TABLE OF ty_row_tab WITH HEADER LINE.

data $itab like itab occurs 0 with header line.

DATA: BEGIN OF it_anlz OCCURS 0,
        bukrs	TYPE bukrs,      " Company Code
        anln1	TYPE anln1,      " Main asset number
        anln2	TYPE anln2,      " Asset sub-number
        kostl	TYPE kostl,
        kostlv TYPE kostlv,
        bdatu TYPE bdatu,      " Date validity ends
      END   OF it_anlz.

DATA: BEGIN OF it_stand OCCURS 0,
        werks	TYPE werks_d,
        stand	TYPE stort_t499s,
      END   OF it_stand.

DATA: BEGIN OF it_t499s OCCURS 0,
        werks	TYPE werks_d,
        stand	TYPE stort_t499s,
        ktext TYPE text40,
      END   OF it_t499s.

DATA: BEGIN OF it_gewrk OCCURS 0,
        objid	TYPE cr_objid,
      END   OF it_gewrk.

DATA: BEGIN OF it_crhd OCCURS 0,
        objid	TYPE objid,
        arbpl TYPE arbpl,
        ktext TYPE cr_ktext,
        STAND type AP_STAND,
      END   OF it_crhd.

DATA: BEGIN OF it_equi OCCURS 0,
        equnr	TYPE equnr,
        ansdt TYPE andti,
        answt	TYPE answt,
        elief TYPE elief,
        herst TYPE herst,
        eqktx TYPE ktx01,
      END   OF it_equi.

DATA: BEGIN OF it_iloan OCCURS 0,
        iloan	TYPE iloan,
      END   OF it_iloan.

DATA: BEGIN OF it_equnr OCCURS 0,
        iloan TYPE iloan,
        equnr TYPE equnr,
        gewrk TYPE gewrk,
      END   OF it_equnr.


DATA: BEGIN OF it_eqart OCCURS 0,
        eqart	TYPE eqart,
      END   OF it_eqart.

DATA: BEGIN OF it_all_equi OCCURS 0,
        equnr TYPE equnr,
        eqtyp TYPE eqtyp,
        eqart TYPE eqart,
        ansdt TYPE andti,
        answt TYPE answt,
        waers TYPE waers,
        elief TYPE elief,
        herst TYPE herst,
        baumm TYPE baumm,
        matnr TYPE matnr,
        sernr TYPE gernr,
        werk TYPE werks_d,
        meins TYPE meins,
        gewrk TYPE gewrk,
        swerk TYPE swerk,
        stort	TYPE pmloc,
        iloan TYPE iloan,
        eqktx	TYPE ktx01,
        eswerk 	TYPE swerk,
        estort 	TYPE pmloc,
        erdat type erdat,
      END   OF it_all_equi.

DATA: BEGIN OF gt_esort OCCURS 0,
        equnr TYPE equnr,
        eswerk 	TYPE swerk,
        estort 	TYPE pmloc,
      END   OF gt_esort.

DATA: BEGIN OF it_anlc OCCURS 0,
        bukrs	TYPE bukrs,      " Company Code
        anln1	TYPE anln1,      " Main asset number
        anln2	TYPE anln2,      " Asset sub-number
        answl	TYPE answl,
      END   OF it_anlc.

DATA $it_anlc LIKE it_anlc OCCURS 0 WITH HEADER LINE.

* Printing option for Label
DATA: zoptions LIKE	itcpo OCCURS 0 WITH HEADER LINE.
DATA: zprinter(4) VALUE 'RFL'.

* temp table for selected rows
DATA  $gt_out LIKE gt_out OCCURS 0 WITH HEADER LINE.
DATA  g_bukrs TYPE bukrs.


*----------------------------------------------------------------------*
* Define local class
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.

    TYPES: BEGIN OF ztfiu128_k,
              bukrs TYPE bukrs,
              anln1 TYPE anln1,
              anln2 TYPE anln2,
*              zseri  type zseri,
           END OF ztfiu128_k.

    TYPES: ztfiu128_key   TYPE STANDARD TABLE OF ztfiu128_k,
           ztfiu128_table TYPE STANDARD TABLE OF ztfiu128.

    METHODS:
      handle_data_changed
         FOR EVENT data_changed OF cl_gui_alv_grid
             IMPORTING er_data_changed.

    METHODS:
      get_inserted_rows
           EXPORTING
              inserted_rows TYPE ztfiu128_key.

    METHODS:
      get_deleted_rows
          EXPORTING
              deleted_rows TYPE ztfiu128_table.

    METHODS:
       refresh_delta_tables.

    METHODS: set_table_is_initial.

    METHODS: set_table_is_not_initial.

    METHODS: table_is_initial
                RETURNING value(initial) TYPE char01.

    METHODS: chk_screen
               RETURNING value(error) TYPE char01.


  PRIVATE SECTION.
    DATA: inserted_rows TYPE ztfiu128_key,
          deleted_rows TYPE STANDARD TABLE OF ztfiu128.
    DATA  error_in_data TYPE c.
    DATA  initial_table TYPE c.

** Methods to modularize event handler method HANDLE_DATA_CHANGED:
*
    METHODS:
      check_double_entries
         IMPORTING
            pr_data_changed TYPE REF TO cl_alv_changed_data_protocol.

    METHODS:
      update_delta_tables
         IMPORTING
            pr_data_changed TYPE REF TO cl_alv_changed_data_protocol.

    METHODS:
      perform_semantic_checks
         IMPORTING
            pr_data_changed TYPE REF TO cl_alv_changed_data_protocol.

    METHODS:
      get_cell_values
           IMPORTING
             row_id          TYPE int4
             pr_data_changed TYPE REF TO cl_alv_changed_data_protocol
           EXPORTING
             key             TYPE ztfiu128_k.

ENDCLASS.                   " LCL_EVENT_RECEIVER Definition

*----------------------------------------------------------------------*
* Implementation local class
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD chk_screen.
    READ TABLE gt_out WITH KEY err = true TRANSPORTING NO FIELDS.
    IF sy-subrc EQ 0.
      error = true.
    ELSE.
      error = space.
    ENDIF.
  ENDMETHOD.

* Setting for Change data
  METHOD handle_data_changed.
    error_in_data = space.

* check if there exist double entries
    CALL METHOD check_double_entries( er_data_changed ).

* remember new or deleted lines for saving
    CALL METHOD update_delta_tables( er_data_changed ).

* check mt_good_cells semantically
    CALL METHOD perform_semantic_checks( er_data_changed ).

    IF error_in_data = 'X'.
      CALL METHOD er_data_changed->display_protocol.
    ENDIF.

  ENDMETHOD.                    " handle_data_changed

  METHOD check_double_entries.

    DATA: lt_good_cells TYPE lvc_t_modi,
          ls_good TYPE lvc_s_modi,
          ls_key TYPE ztfiu128_k,
          ls_ztfiu128 TYPE ztfiu128,
          l_anln1 LIKE gt_out-anln1,
          l_row_id LIKE sy-tabix,
          lt_ins_rows TYPE ztfiu128_key,
          lt_key TYPE ztfiu128_k,
          dup_chk(1).

    LOOP AT pr_data_changed->mt_good_cells INTO ls_good.
      l_row_id = ls_good-row_id.

      CASE ls_good-fieldname.
        WHEN 'ANLN1'.
          CALL METHOD pr_data_changed->get_cell_value
                      EXPORTING
                            i_row_id = ls_good-row_id
                            i_fieldname = ls_good-fieldname
                      IMPORTING e_value = l_anln1.

         IF ( l_anln1 >= '000090000000' AND l_anln1 <= '000099999999' )
                                                    OR l_anln1 EQ space.
            __set_icon_no_error.
          ELSE.
            __set_error text-m01.
          ENDIF.
      ENDCASE.
      IF ls_good-fieldname EQ 'ANLN1' OR
         ls_good-fieldname EQ 'ANLN2'. " or
*         ls_good-fieldname eq 'ZSERI'.
        dup_chk = true.
      ENDIF.

    ENDLOOP.

    CHECK error_in_data NE true.

    CALL METHOD get_cell_values
         EXPORTING row_id          = l_row_id
                   pr_data_changed = pr_data_changed
         IMPORTING key             = ls_key.

    CHECK g_bukrs NE space
      AND ls_key-anln1 NE space
*      and ls_key-zseri ne space
      AND dup_chk EQ true.

    SELECT SINGLE * FROM ztfiu128 INTO ls_ztfiu128
              WHERE bukrs  = g_bukrs
                AND anln1  = ls_key-anln1
                AND anln2 =  ls_key-anln2.
*                and zseri  = ls_key-zseri.

    IF sy-subrc = 0.
      __set_error text-m03.
    ELSE.

      __set_icon_no_error.

    ENDIF.

  ENDMETHOD.

*-------------------------------------------------------
  METHOD update_delta_tables.
  ENDMETHOD.
*---------------------------------------------------------

  METHOD get_cell_values.
  ENDMETHOD.

*---------------------------------------------------------
  METHOD perform_semantic_checks.
  ENDMETHOD.
*------------------------------------------------------

  METHOD get_inserted_rows.
    inserted_rows = me->inserted_rows.
  ENDMETHOD.
*------------------------------------------------------

  METHOD get_deleted_rows.
    deleted_rows = me->deleted_rows.
  ENDMETHOD.
*------------------------------------------------------
  METHOD refresh_delta_tables.
    CLEAR me->inserted_rows[].
    CLEAR me->deleted_rows[].
  ENDMETHOD.
*------------------------------------------------------
  METHOD set_table_is_initial.
    initial_table = 'X'.
  ENDMETHOD.
*------------------------------------------------------
  METHOD set_table_is_not_initial.
    initial_table = space.
  ENDMETHOD.
*------------------------------------------------------
  METHOD table_is_initial.
    IF initial_table = 'X'.
      initial = 'X'.
    ELSE.
      initial = space.
    ENDIF.
  ENDMETHOD.

ENDCLASS.                   " LCL_EVENT_RECEIVER Implementation

DATA g_event_receiver  TYPE REF TO lcl_event_receiver.

************************************************************************
DATA  : flag_data_changed,
        info(80).

DATA: BEGIN OF ftab OCCURS 10,
    fcode(6),
  END OF ftab.

DATA g_exq.

*--------------------------------------------------------------------*
* SELECTION-SCREEN
*--------------------------------------------------------------------*
*************************** Selections *********************************
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.
PARAMETERS p_bukrs LIKE t001-bukrs MEMORY ID buk OBLIGATORY
                                DEFAULT 'H201'.

SELECT-OPTIONS : s_aktiv FOR zsfiu129-aktiv,
                 s_anln1 FOR zsfiu129-anln1,
                 s_kostl FOR csks-kostl,
                 s_mcoa1 FOR zsfiu129-mcoa1,
                 s_ebeln FOR ekko-ebeln.
PARAMETER p_all AS CHECKBOX.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK bl1s WITH FRAME.
SELECT-OPTIONS s_anlkl FOR anla-anlkl NO INTERVALS. " NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK bl1s.

SELECTION-SCREEN SKIP 1.
SELECT-OPTIONS : s_equnr FOR zsfiu129-equnr,
                 s_eqtyp FOR ankaz-eqtyp NO INTERVALS.

PARAMETER p_vali LIKE sy-datum DEFAULT sy-datum OBLIGATORY.
PARAMETER p_exq AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK bl1.

* Layout
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-01s.
PARAMETER p_vari TYPE slis_vari.
SELECTION-SCREEN END OF BLOCK b4.

*SELECTION-SCREEN BEGIN OF BLOCK INSTALL WITH FRAME TITLE TEXT-T03.
*SELECTION-SCREEN PUSHBUTTON  1(18) SCRN USER-COMMAND SCRN.
*SELECTION-SCREEN PUSHBUTTON 21(18) LOCF USER-COMMAND LOCF.
*SELECTION-SCREEN PUSHBUTTON 41(18) DIRP USER-COMMAND DIRP.
*SELECTION-SCREEN PUSHBUTTON 61(18) PSET USER-COMMAND PSET.
*SELECTION-SCREEN END OF BLOCK INSTALL.
*SELECTION-SCREEN FUNCTION KEY :1,2,3,4.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.

  __cls s_anlkl.
  s_anlkl-sign = 'I'.
  s_anlkl-option = 'EQ'.
  s_anlkl-low = '00003300'.
  APPEND s_anlkl.

  s_anlkl-sign = 'I'.
  s_anlkl-option = 'EQ'.
  s_anlkl-low = '00003301'.
  APPEND s_anlkl.

  __cls s_eqtyp.
  s_eqtyp-sign = 'I'.
  s_eqtyp-option = 'EQ'.
  s_eqtyp-low = 'M'.
  APPEND s_eqtyp.

  sy-title = '[FI] Printing Label for Equipment'.
  PERFORM make_button.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM alv_variant_f4 CHANGING p_vari.

*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
*----------------------------------------------------------------------*
  CLEAR g_error.

  CASE sscrfields-ucomm.
    WHEN 'TRAF'.
      PERFORM initialize.
      PERFORM get_equi_data_.
      PERFORM move_out.
      PERFORM set_output .

    WHEN 'PSET' OR 'FC04'.
      PERFORM get_print USING dialog.
  ENDCASE.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*

  PERFORM initialize.
  PERFORM get_equi_data_.
  CHECK g_error EQ  false.
  PERFORM move_out.
  PERFORM set_output .

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  SET_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_output.

  CHECK : g_error IS INITIAL.
  CLEAR flag_data_changed.
  CALL SCREEN 100.

ENDFORM.                    " SET_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  SORT_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_SORT[]  text
*----------------------------------------------------------------------*
FORM sort_build USING ft_sort TYPE lvc_t_sort.
  DEFINE sort_tab.
    clear gs_sort.
    gs_sort-fieldname = &1.
    gs_sort-spos      = &2.
    gs_sort-up        = &3.
    gs_sort-group     = &4.
    gs_sort-subtot    = &5.
    gs_sort-comp      = &6.
    append gs_sort to ft_sort.
  END-OF-DEFINITION.

  sort_tab :
   'ANLKL'  ' ' 'X' ' ' 'X' ' ',
   'ANLN1'  ' ' 'X' ' ' 'X' ' ',
   'EQUNR'  ' ' 'X' ' ' 'X' ' ',
   'ANLN2'  ' ' 'X' ' ' 'X' ' ',
   'MCOA1'  ' ' 'X' ' ' 'X' ' ',
   'EQKTX'  ' ' 'X' ' ' 'X' ' ',
   'HERST'  ' ' 'X' ' ' 'X' ' ',
   'LIFNR'  ' ' 'X' ' ' 'X' ' ',
   'KOSTL'  ' ' 'X' ' ' 'X' ' ',
   'STORT'  ' ' 'X' ' ' 'X' ' ',
   'KTEXT'  ' ' 'X' ' ' 'X' ' ',
   'ARBPL'  ' ' 'X' ' ' 'X' ' ',
   'WKTEXT' ' ' 'X' ' ' 'X' ' ',
   'ESTORT' ' ' 'X' ' ' 'X' ' ',
   'EKTEXT' ' ' 'X' ' ' 'X' ' '.




ENDFORM.                    " SORT_BUILD

*&---------------------------------------------------------------------*
*&      Form  INITIALIZE_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialize.
  CLEAR : g_error.
  __cls : it_row_tab.
  g_bukrs = p_bukrs.

  SELECT SINGLE * INTO *tka02
    FROM tka02
    WHERE bukrs = g_bukrs.

  CLEAR g_exq.
*  g_exq = p_exq.

*  CHECK g_exq EQ false.

  IF NOT s_aktiv[] IS INITIAL OR
     NOT s_anln1[] IS INITIAL OR
     NOT s_kostl[] IS INITIAL OR
     NOT s_mcoa1[] IS INITIAL OR
     NOT s_ebeln[] IS INITIAL.
*    g_exq = true.
  ENDIF.
ENDFORM.                    " INITIALIZE_
*&---------------------------------------------------------------------*
*&      Form  refine_row_itab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refine_row_itab.
  CHECK g_error EQ space.
ENDFORM.                    " refine_row_itab
*&---------------------------------------------------------------------*
*&      Form  MOVE_OUT_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM move_out.

  __cls gt_out.
  LOOP AT it_row_tab.
    MOVE-CORRESPONDING it_row_tab TO gt_out.
    CONCATENATE gt_out-anln1+4 '-' gt_out-anln2 INTO gt_out-$number.
    read table gt_esort with key equnr = it_row_tab-equnr binary search.
    if sy-subrc eq 0.
      gt_out-eswerk = gt_esort-eswerk.
      gt_out-estort = gt_esort-estort.

      READ TABLE it_t499s WITH KEY werks = gt_esort-eswerk
                                   stand =  gt_esort-estort
                                  BINARY SEARCH.
      IF sy-subrc EQ 0.
        gt_out-ektext = it_t499s-ktext.
      ENDIF.

    endif.
    APPEND gt_out.
  ENDLOOP.

  if p_exq eq true.
    delete gt_out where ANLN1 eq space.
  endif.

  PERFORM apply_icon.

ENDFORM.                    " MOVE_OUT_
*&---------------------------------------------------------------------*
*&      Form  get_equi_data_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_equi_data_.

  DATA $ix LIKE sy-tabix.

  CLEAR g_error.

*  select * from ILOA
  __process 'Read Asset data...' '10'.

  PERFORM get_row_data.

  __process 'Read Equi. data...' '20'.

  IF NOT itab[] IS INITIAL.

* cost center {
    __cls it_anlz.

    SELECT bukrs anln1 anln2 kostl kostlv bdatu
    INTO CORRESPONDING FIELDS OF TABLE it_anlz
    FROM anlz
    FOR ALL ENTRIES IN itab
     WHERE bukrs EQ p_bukrs
       AND anln1 EQ itab-anln1
       AND anln2 EQ itab-anln2
       AND kostl IN s_kostl.

    SORT it_anlz BY bukrs anln1 anln2 kostl ASCENDING
                    bdatu DESCENDING.

    DELETE ADJACENT DUPLICATES FROM it_anlz
          COMPARING bukrs anln1 anln2 kostl.

    SORT it_anlz BY bukrs anln1 anln2 kostl.
    DESCRIBE TABLE it_anlz LINES sy-index.
    IF sy-index EQ 0.
      MESSAGE s000 WITH 'No data was found!'.
      g_error = true.
      EXIT.
    ENDIF.
* }
  ENDIF.

  PERFORM get_all_equi.

  IF NOT itab[] IS INITIAL.

    SORT itab BY  bukrs anln1 anln2.
    __process 'Read Equi. data...' '30'.

    PERFORM fill_it_iloan.

    SORT itab BY anln1 anln2 ASCENDING
                       equnr DESCENDING.

    __cls $itab.
    $itab[] = itab[].
*    DELETE ADJACENT DUPLICATES FROM itab
*            COMPARING anln1 anln2.

* values {
    SELECT bukrs anln1 anln2 answl
    INTO CORRESPONDING FIELDS OF TABLE it_anlc
    FROM anlc
    FOR ALL ENTRIES IN itab
     WHERE bukrs EQ p_bukrs
       AND anln1 EQ itab-anln1
       AND anln2 EQ itab-anln2
       AND afabe EQ '01'.
    __cls $it_anlc.
    LOOP AT it_anlc.
      $it_anlc = it_anlc.
      COLLECT $it_anlc.
    ENDLOOP.

    __cls it_anlc.
    it_anlc[] = $it_anlc[].

    SORT it_anlc BY bukrs anln1 anln2.

* equip. data {
    IF NOT it_equnr[] IS INITIAL AND
           it_all_equi[] IS INITIAL.
      __cls : it_equi.
      SELECT a~equnr a~ansdt a~answt a~elief a~herst b~eqktx
      INTO CORRESPONDING FIELDS OF TABLE it_equi
      FROM equi AS a
      INNER JOIN eqkt AS b
      ON b~equnr EQ a~equnr
      AND b~spras EQ 'E'
      FOR ALL ENTRIES IN it_equnr
       WHERE a~equnr EQ it_equnr-equnr
         AND a~lvorm EQ space.

*         AND a~ansdt NE '00000000'.
      SORT it_equi BY equnr.
    ENDIF.
* }
    __process 'Read Equi. data...' '40'.

* Location{
    __cls : it_stand, it_t499s.

    LOOP AT itab.
      it_stand-werks = itab-swerk.
      it_stand-stand = itab-stort.
      APPEND it_stand.
    ENDLOOP.

    SORT it_stand.
    DELETE ADJACENT DUPLICATES FROM it_stand.

    SELECT werks stand ktext INTO TABLE it_t499s
        FROM t499s.
*    FOR ALL ENTRIES IN it_stand
*    WHERE werks EQ it_stand-werks
*    AND stand EQ  it_stand-stand.
    SORT it_t499s BY werks stand.
* }

* work center{
    __cls : it_gewrk, it_crhd .

    LOOP AT itab.
      CHECK NOT itab-gewrk IS INITIAL.
      it_gewrk-objid = itab-gewrk.
      APPEND it_gewrk.
    ENDLOOP.

    SORT it_gewrk.
    DELETE ADJACENT DUPLICATES FROM it_gewrk.

    SELECT a~objid a~arbpl b~ktext a~stand INTO TABLE it_crhd
        FROM crhd AS a
        INNER JOIN crtx AS b
        ON    b~objty EQ a~objty
        AND   b~objid EQ a~objid
        AND   b~spras EQ 'E'
    FOR ALL ENTRIES IN it_gewrk
    WHERE a~objty EQ 'A'
    AND a~objid EQ it_gewrk-objid.
    SORT it_crhd BY objid.
* }
  ENDIF.

  __process 'Read Equi. data...' '50'.

  SORT it_all_equi BY equnr.

  LOOP AT itab.
    $ix = sy-tabix.
    READ TABLE it_anlz WITH KEY bukrs = itab-bukrs
                                anln1 = itab-anln1
                                anln2 = itab-anln2
                                BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-kostl = it_anlz-kostl.
      itab-kostlv = it_anlz-kostlv.
    ELSE.
      DELETE itab INDEX $ix.
      CONTINUE.
    ENDIF.

    READ TABLE it_equi WITH KEY equnr = itab-equnr
                                BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-equnr = it_equi-equnr.
      itab-ansdt = it_equi-ansdt.
      itab-answt = it_equi-answt.
      itab-elief = it_equi-elief.
      itab-herst = it_equi-herst.
      itab-eqktx = it_equi-eqktx.
    ENDIF.

    READ TABLE it_all_equi WITH KEY equnr = itab-equnr
                                BINARY SEARCH.
    IF sy-subrc EQ 0.
      DELETE it_all_equi INDEX sy-tabix.
    ENDIF.

    READ TABLE it_anlc WITH KEY bukrs = itab-bukrs
                                anln1 = itab-anln1
                                anln2 = itab-anln2
                                BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-answl = it_anlc-answl.
    ENDIF.

    READ TABLE it_t499s WITH KEY werks = itab-swerk
                                 stand =  itab-stort
                                BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-ktext = it_t499s-ktext.
    ENDIF.

    READ TABLE it_t499s WITH KEY werks = itab-eswerk
                                 stand =  itab-estort
                                BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-ktext = it_t499s-ktext.
    ENDIF.

    READ TABLE it_crhd WITH KEY objid = itab-gewrk
                                BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-arbpl = it_crhd-arbpl.
      itab-wktext = it_crhd-ktext.
    ENDIF.

    IF itab-lifnr IS INITIAL. " get vendor when lifnr is initial.
      PERFORM get_vendor USING itab-eaufn
                      CHANGING itab-lifnr.
    ENDIF.

    MODIFY itab INDEX $ix.
  ENDLOOP.

  __cls gt_out.

  __cls it_row_tab.
  LOOP AT itab.
    MOVE-CORRESPONDING itab TO it_row_tab.

*    IF ITAB-MENGE <> 0.
*      CLEAR IT_ROW_TAB-ANSWL.
*      IT_ROW_TAB-ANSWL = ITAB-ANSWL / ITAB-MENGE.
*    ENDIF.

    it_row_tab-waers = 'USD'.
    APPEND it_row_tab.
    CLEAR  it_row_tab.
  ENDLOOP.

  __process 'Read Equi. data...' '70'.

  IF g_exq EQ false.

* Location{
    __cls : it_stand, it_t499s.

    LOOP AT it_all_equi.
      it_stand-werks = it_all_equi-swerk.
      it_stand-stand = it_all_equi-stort.
      APPEND it_stand.
    ENDLOOP.

    SORT it_stand.
    DELETE ADJACENT DUPLICATES FROM it_stand.

    SELECT werks stand ktext INTO TABLE it_t499s
        FROM t499s
    FOR ALL ENTRIES IN it_stand
    WHERE werks EQ it_stand-werks
    AND stand EQ  it_stand-stand.
    SORT it_t499s BY werks stand.
* }

* work center{
    __cls : it_gewrk, it_crhd .

    LOOP AT it_all_equi.
      it_gewrk-objid = it_all_equi-gewrk.
      APPEND it_gewrk.
    ENDLOOP.

    SORT it_gewrk.
    DELETE ADJACENT DUPLICATES FROM it_gewrk.

    SELECT objid arbpl INTO TABLE it_crhd
        FROM crhd
    FOR ALL ENTRIES IN it_gewrk
    WHERE objty EQ 'A'
    AND objid EQ it_gewrk-objid.
    SORT it_crhd BY objid.
* }

    LOOP AT $itab.
      $ix = sy-tabix.
      READ TABLE it_anlz WITH KEY bukrs = $itab-bukrs
                                  anln1 = $itab-anln1
                                  anln2 = $itab-anln2
                                  BINARY SEARCH.
      IF sy-subrc EQ 0.
        $itab-kostl = it_anlz-kostl.
        $itab-kostlv = it_anlz-kostlv.
      ELSE.
        DELETE $itab INDEX $ix.
        CONTINUE.
      ENDIF.

      READ TABLE it_equi WITH KEY equnr = $itab-equnr
                                  BINARY SEARCH.
      IF sy-subrc EQ 0.
        $itab-equnr = it_equi-equnr.
        $itab-ansdt = it_equi-ansdt.
        $itab-answt = it_equi-answt.
        $itab-elief = it_equi-elief.
        $itab-herst = it_equi-herst.
        $itab-eqktx = it_equi-eqktx.
      ENDIF.

*    READ TABLE it_anlc WITH KEY bukrs = $itab-bukrs
*                                anln1 = $itab-anln1
*                                anln2 = $itab-anln2
*                                BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      $itab-answl = it_anlc-answl.
*    ENDIF.

      READ TABLE it_t499s WITH KEY werks = $itab-swerk
                                   stand = $itab-stort
                                  BINARY SEARCH.
      IF sy-subrc EQ 0.
        $itab-ktext = it_t499s-ktext.
      ENDIF.

      READ TABLE it_crhd WITH KEY objid = $itab-gewrk
                                  BINARY SEARCH.
      IF sy-subrc EQ 0.
        $itab-arbpl = it_crhd-arbpl.
        $itab-wktext = it_crhd-ktext.
      ENDIF.

      IF $itab-lifnr IS INITIAL. " get vendor when lifnr is initial.
        PERFORM get_vendor USING $itab-eaufn
                        CHANGING $itab-lifnr.
      ENDIF.

      MODIFY $itab INDEX $ix.
    ENDLOOP.


    sort $itab by equnr.

    LOOP AT it_all_equi.

      MOVE-CORRESPONDING it_all_equi TO it_row_tab.

      read table $itab with key equnr = it_all_equi-equnr binary search.
      if sy-subrc eq 0.
        MOVE-CORRESPONDING $itab TO it_row_tab.
      endif.

      it_row_tab-waers = 'USD'.

      READ TABLE it_crhd WITH KEY objid = it_all_equi-gewrk
                                  BINARY SEARCH.
      IF sy-subrc EQ 0.
        it_row_tab-arbpl = it_crhd-arbpl.
      ENDIF.

      READ TABLE it_t499s WITH KEY werks = it_all_equi-swerk
                                   stand =  it_all_equi-stort
                                  BINARY SEARCH.
      IF sy-subrc EQ 0.
        it_row_tab-ktext = it_t499s-ktext.
      ENDIF.
      APPEND it_row_tab.
      CLEAR  it_row_tab.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " get_equi_data_
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET TITLEBAR '100'.
*   Exclude toolbar
  PERFORM exclude_functions.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_alv_100 OUTPUT.
  IF g_custom_container IS INITIAL.
    PERFORM create_and_init_alv.

*   Display alv grid
    CALL METHOD g_grid->set_table_for_first_display
         EXPORTING is_layout            = gs_layo
                   it_toolbar_excluding = gt_exclude
                   i_save               = gc_var_save
                   is_variant           = gs_variant
         CHANGING  it_outtab            = gt_out[]
                   it_fieldcatalog      = gt_fcat[]
                   it_sort              = gt_sort[].
  ELSE.
    CALL METHOD g_grid->refresh_table_display.
  ENDIF.
  PERFORM user_status.

  __focus g_grid.
ENDMODULE.                 " DISPLAY_ALV_100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CLEAR : g_error.

  ok_code = sy-ucomm.
  CLEAR sy-ucomm.
  CASE ok_code.

    WHEN 'BACK' OR 'CANC'.
      PERFORM free_container.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.

    WHEN 'PRNT'.
      PERFORM : bar_print,
                clear_chk,
                refresh_alv.
      __focus g_grid.

    WHEN 'SWITCH'.
      IF sy-dynnr EQ '0100'.
        PERFORM switch_edit_mode.
      ENDIF.
      __focus g_grid.

    WHEN 'SAVE'.
      CLEAR g_error.

      IF g_event_receiver->chk_screen( ) EQ true.
        MESSAGE i000 WITH text-e03.
        EXIT.
      ENDIF.

      PERFORM really?.
      CHECK g_error NE true.

      PERFORM : post_bapi,
                refresh_alv,
                clear_chk.
      __focus g_grid.

    WHEN 'ACTV' OR 'DACT'.
*      CLEAR G_ERROR.
*      PERFORM REALLY_ACT_OR_DACT? USING OK_CODE.
*      CHECK G_ERROR NE TRUE.

      PERFORM : changed_status USING ok_code,
                apply_icon,
                clear_chk,
                refresh_alv.
      __focus g_grid.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  CREATE_AND_INIT_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_and_init_alv.

*   Create object
  PERFORM create_object.

*  Create Object to verify input values.
  CREATE OBJECT g_event_receiver.
  SET HANDLER : g_event_receiver->handle_data_changed FOR g_grid.

*   Create field category
  PERFORM create_field_category USING false.

  CALL METHOD g_grid->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  CALL METHOD g_grid->register_edit_event
       EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD g_grid->set_ready_for_input
     EXPORTING
            i_ready_for_input = 0.

  PERFORM sort_build USING gt_sort[].

*   Setting for layout
  PERFORM set_lvc_layout.

*   Set colors
  PERFORM set_color.

*   Define cell attribute
  PERFORM build_cell_attr.

*   Set variant
  gv_repid = gs_variant-report = sy-repid.
  gs_variant-variant = p_vari.

ENDFORM.                    " CREATE_AND_INIT_ALV
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM exclude_functions.
  PERFORM append_exclude_functions
           TABLES gt_exclude[]
           USING: cl_gui_alv_grid=>mc_fc_average,
                  cl_gui_alv_grid=>mc_fc_graph.
*                  CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO,
*                  CL_GUI_ALV_GRID=>MC_FC_INFO,
*                  CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW,
*                  CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW,
*                  CL_GUI_ALV_GRID=>MC_FC_LOC_CUT,
*                  CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW,
*                  CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW,
*                  CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
*
ENDFORM.                    " EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
*&      Form  CREATE_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FALSE  text
*----------------------------------------------------------------------*
FORM create_field_category USING mode_edit.
  DATA: l_pos       TYPE i.
  DEFINE __catalog.
    l_pos = l_pos + 1.
    clear gs_fcat.
    gs_fcat-col_pos       = l_pos.
    gs_fcat-key           = &1.
    gs_fcat-fieldname     = &2.
    gs_fcat-coltext       = &3.     " Column heading
    gs_fcat-outputlen     = &4.     " Column width
    gs_fcat-datatype      = &5.     " Data type
    gs_fcat-emphasize     = &6.
    append gs_fcat to gt_fcat.
  END-OF-DEFINITION.

  __catalog :
    'X'  'ANLKL'           'A.Cls'                 4  'CHAR' '',
    'X'  'ANLN1'           'AA_Asset#'            12  'CHAR' '',
    'X'  'EQUNR'           'Equip.#'              18  'CHAR' '',
    ' '  'ANLN2'           'AA_Sub.#'              4  'CHAR' '',
    ' '  'MCOA1'           'AA_Description'       40  'CHAR' '',
    ' '  'MENGE'           'AA_Quantity'          15  'QUAN' '',
    ' '  'LIFNR'           'AA_Vendor'            10  'CHAR' '',
    ' '  'ANSWL'           'AA_Values'            15  'CURR' '',
    ' '  'AKTIV'           'AA_Acqui.'             8  'DATS' '',
    ' '  'KOSTL'           'AA_CstCntr.'          10  'CHAR' '',
    ' '  'STORT'           'AA_Location'          10  'CHAR' '',
    ' '  'KTEXT'           'AA_Loc_Desc'          30  'CHAR' '',
    ' '  'EQKTX'           'EQ_Description'       40  'CHAR' '',
    ' '  'HERST'           'EQ_Manufacturer of asset'   30  'CHAR' '',
*    ' '  'ELIEF'           'EQ_Vendor'            10  'CHAR' '',
    ' '  'ANSWT'           'EQ_Values'            15  'CURR' '',
    ' '  'ANSDT'           'EQ_Acqui.'             8  'DATS' '',
    ' '  'ARBPL'           'EQ_Work Center'        8  'CHAR' '',
    ' '  'WKTEXT'          'EQ_Work Desc'         40  'CHAR' '',
    ' '  'ESTORT'          'EQ_Location'          10  'CHAR' '',
    ' '  'EKTEXT'          'EQ_Loc_Desc'          30  'CHAR' ''.
*    ' '  'ICON'            'S'                  3  'ICON' '',
*    ' '  'ERR'             'E'                  1  'CHAR' ''.

  LOOP AT gt_fcat INTO gs_fcat.
    CASE gs_fcat-fieldname.
      WHEN 'MENGE' OR 'ANSWL'. " or 'ZSERI'.
        gs_fcat-just = 'R'.
    ENDCASE.

    IF gs_fcat-fieldname NE 'ICON'.
      gs_fcat-ref_table = 'ZSFIU129'.
      gs_fcat-ref_field = gs_fieldcat-fieldname.
      MODIFY gt_fcat FROM gs_fcat.
    ENDIF.

    IF gs_fcat-fieldname EQ 'ERR'.
      gs_fcat-no_out = 'X'.
      gs_fcat-ref_field = gs_fieldcat-fieldname.
      MODIFY gt_fcat FROM gs_fcat.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " CREATE_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*&      Form  SET_LVC_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_lvc_layout.
  CLEAR gs_layo.
  gs_layo-edit       = 'X'.
  gs_layo-zebra      = 'X'.
  gs_layo-sel_mode   = 'A'.       " Column and row selection
  gs_layo-cwidth_opt = 'X'.
  gs_layo-ctab_fname = 'TABCOLOR'.
  gs_layo-stylefname = 'CELLTAB'.
ENDFORM.                    " SET_LVC_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  SET_COLOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_color.
  CLEAR: gs_specialcol, gt_specialcol[], gt_out-tabcolor[].

  DEFINE __color.
    gs_specialcol-fieldname = &1 .
    gs_specialcol-color-col = &2 .
    gs_specialcol-color-int = &3 .
    append gs_specialcol to gt_specialcol .
  END-OF-DEFINITION.

  __color :
   'ANLN1'  '2' 0,
   'EQUNR'  '3' 0,
   'ANLN2'  '2' 0,
   'MCOA1'  '2' 0,
   'MENGE'  '2' 0,
   'LIFNR'  '2' 0,
   'ANSWL'  '2' 0,
   'AKTIV'  '2' 0,
   'KOSTL'  '2' 0,
   'STORT'  '2' 0,
   'KTEXT'  '2' 0,
   'EQKTX'  '3' 0,
   'HERST'  '3' 0,
   'ELIEF'  '3' 0,
   'ANSWT'  '3' 0,
   'ANSDT'  '3' 0,
   'ARBPL'  '3' 0.
  gt_out-tabcolor[] = gt_specialcol[].
  MODIFY gt_out TRANSPORTING tabcolor WHERE tabcolor IS initial.

ENDFORM.                    " SET_COLOR
*&---------------------------------------------------------------------*
*&      Form  INFO_TEXT_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FALSE  text
*----------------------------------------------------------------------*
FORM info_text_set USING p_true.

  IF p_true EQ true.
    info = text-015.
  ELSE.
    info = text-015.
  ENDIF.

ENDFORM.                    " info_text_set
*&---------------------------------------------------------------------*
*&      Form  REFRESH_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh_alv.
  __set_refresh_mode true.
  CALL METHOD g_grid->refresh_table_display
       EXPORTING is_stable = stable.
ENDFORM.                    " REFRESH_ALV
*&---------------------------------------------------------------------*
*&      Form  get_selected_rows
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$GT_OUT  text
*----------------------------------------------------------------------*
FORM get_selected_rows TABLES $gt_out STRUCTURE gt_out.

  PERFORM clear_chk.

  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
        lt_row_no TYPE lvc_t_roid. "Numeric IDs of Selected Rows

  CALL METHOD g_grid->get_selected_rows
           IMPORTING et_index_rows = lt_rows[]
                     et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    MESSAGE e000
    WITH 'Error founded during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

  __cls $gt_out.

  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    $gt_out[] = gt_out[].
    gt_out-chk = true .
    MODIFY gt_out TRANSPORTING chk WHERE chk EQ false.
  ELSE.
    LOOP AT lt_rows WHERE rowtype IS initial.
      READ TABLE gt_out INDEX lt_rows-index.
      gt_out-chk = true .
      MODIFY gt_out INDEX lt_rows-index .
    ENDLOOP.
    LOOP AT gt_out.
      CHECK gt_out-chk EQ true.
      $gt_out = gt_out.
      APPEND $gt_out.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " get_selected_rows
*&---------------------------------------------------------------------*
*&      Form  UPDATE_SELECTED_ROWS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_selected_rows USING p_flag.

  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
        lt_row_no TYPE lvc_t_roid. "Numeric IDs of Selected Rows

  CALL METHOD g_grid->get_selected_rows
           IMPORTING et_index_rows = lt_rows[]
                     et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    MESSAGE e000
    WITH 'Error founded during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.


  ELSE.
    LOOP AT lt_rows WHERE rowtype IS initial.

    ENDLOOP.
  ENDIF.

ENDFORM.                    " UPDATE_SELECTED_ROWS
*&---------------------------------------------------------------------*
*&      Form  BUILD_CELL_ATTR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_cell_attr.

  DATA: lt_celltab TYPE lvc_t_styl,
        ls_celltab TYPE lvc_s_styl.

  __cls lt_celltab.
  __cls gt_out-celltab.
  MODIFY gt_out TRANSPORTING celltab WHERE NOT celltab IS initial.

  CLEAR gs_fcat.


  LOOP AT gt_fcat INTO gs_fcat.
    ls_celltab-fieldname = gs_fcat-fieldname.
    IF   ls_celltab-fieldname = 'ANSDT' OR
            ls_celltab-fieldname = 'ANSWT'.
      ls_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
    ELSE.
      ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    ENDIF.
    INSERT ls_celltab INTO TABLE lt_celltab.
  ENDLOOP.

  CLEAR gt_out-celltab.
  INSERT LINES OF lt_celltab INTO TABLE gt_out-celltab.
  MODIFY gt_out TRANSPORTING celltab WHERE celltab IS initial.

  PERFORM build_cell_attr1_lock.

ENDFORM.                    " BUILD_CELL_ATTR
*&---------------------------------------------------------------------*
*&      Form  BUILD_CELL_ATTR1_LOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_cell_attr1_lock.

ENDFORM.                    " BUILD_CELL_ATTR1_LOCK
*&---------------------------------------------------------------------*
*&      Form  FREE_CONTAINER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM free_container.

  IF NOT g_event_receiver IS INITIAL.
    FREE g_event_receiver.
  ENDIF.

  IF NOT g_grid IS INITIAL.
    CALL METHOD g_grid->free.
  ENDIF.

  IF NOT g_custom_container IS INITIAL.
    CALL METHOD g_custom_container->free.
  ENDIF.

  FREE : g_grid,g_custom_container.

  CLEAR :  gs_layo,gt_exclude,gt_out[],gt_fcat[],gt_sort[].

ENDFORM.                    " FREE_CONTAINER

*&---------------------------------------------------------------------*
*&      Form  SWITCH_EDIT_MODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM switch_edit_mode.

  DATA answer.
  IF g_grid->is_ready_for_input( ) EQ 0.
    CALL METHOD g_grid->set_ready_for_input
                     EXPORTING i_ready_for_input = 1.
    PERFORM info_text_set USING true.
  ELSE.
**    IF FLAG_DATA_CHANGED EQ TRUE.
**      CALL FUNCTION 'POPUP_TO_CONFIRM_LOSS_OF_DATA'
**           EXPORTING
**                TEXTLINE1     = 'Data has not been saved yet.'
**                TEXTLINE2     = 'Do you want to continue anyway? '
**                TITEL         = 'Confirmation'
**                DEFAULTOPTION = 'N'
**           IMPORTING
**                ANSWER        = ANSWER.
**      CHECK ANSWER EQ 'J'.
**    ENDIF.
**    CLEAR FLAG_DATA_CHANGED.
    CALL METHOD g_grid->set_ready_for_input
                     EXPORTING i_ready_for_input = 0.
    PERFORM info_text_set USING false.
  ENDIF.

  PERFORM user_status.
  PERFORM build_cell_attr.

ENDFORM.                    " SWITCH_EDIT_MODE
*&---------------------------------------------------------------------*
*&      Form  APPLY_ICON
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM apply_icon.

  DATA $ix LIKE sy-tabix.

*ICON_LED_GREEN
*ICON_LED_RED
*ICON_LED_YELLOW

  LOOP AT gt_out.
    $ix = sy-tabix.
    IF gt_out-status EQ true.
      gt_out-icon = icon_led_green.
    ELSE.
      gt_out-icon = icon_led_yellow.
    ENDIF.

    MODIFY gt_out INDEX $ix TRANSPORTING icon.
  ENDLOOP.

ENDFORM.                    " APPLY_ICON
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
            form                        = 'ZFI_ASSET_BLABEL'
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

  LOOP AT $gt_out.
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
*&      Form  INIT_PRINT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_print.

  MOVE '1' TO zoptions-tdcopies.
  MOVE 'X' TO zoptions-tdimmed.
  MOVE 'X' TO zoptions-tdnewid.
  MOVE zprinter TO zoptions-tddest.
  APPEND zoptions.

ENDFORM.                    " INIT_PRINT
*&---------------------------------------------------------------------*
*&      Form  MAKE_BUTTON
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_button.

*  WRITE:
*          ICON_DISPLAY_MORE AS ICON TO PSET,
*         'Print Setup' TO PSET+4(21),
*          ICON_PRINT_WITH_PARAMETERS AS ICON TO DIRP,
*         'Dirct Print' TO DIRP+4(21),
*          ICON_EXPORT AS ICON TO LOCF,
*         'Export to file' TO LOCF+4(21),
*          ICON_LIST AS ICON TO SCRN,
*         'Display ALV' TO SCRN+4(21).
*  WRITE:
*         'ALV Screen'  TO SSCRFIELDS-FUNCTXT_01,
*         'Download'    TO SSCRFIELDS-FUNCTXT_02,
*         'Dirct Print' TO SSCRFIELDS-FUNCTXT_03,
*         'Print Setup' TO SSCRFIELDS-FUNCTXT_04.
ENDFORM.                    " MAKE_BUTTON
*&---------------------------------------------------------------------*
*&      Form  GET_PRINT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_DIALOG  text
*----------------------------------------------------------------------*
FORM get_print USING $no_dialog.

  lay = 'X_70_100'.
  lines = 70.
  rows  = 100.
  CALL FUNCTION 'GET_PRINT_PARAMETERS'
       EXPORTING
            in_archive_parameters  = arcpar
            in_parameters          = pripar
            layout                 = lay
            line_count             = lines
            line_size              = rows
            no_dialog              = $no_dialog
       IMPORTING
            out_archive_parameters = arcpar
            out_parameters         = pripar
            valid                  = val
       EXCEPTIONS
            archive_info_not_found = 1
            invalid_print_params   = 2
            invalid_archive_params = 3
            OTHERS                 = 4.

ENDFORM.                    " GET_PRINT
*&---------------------------------------------------------------------*
*&      Form  SHOW_PROGRESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_&1  text
*      -->P_&2  text
*----------------------------------------------------------------------*
FORM show_progress USING    pf_text
                            value(pf_val).

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            percentage = pf_val
            text       = pf_text.

ENDFORM.                    " SHOW_PROGRESS
*&---------------------------------------------------------------------*
*&      Form  USER_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_status.
  __cls ftab.

  ftab-fcode = 'SWITCH'.
  APPEND ftab.

  IF g_grid->is_ready_for_input( ) EQ 1.
    ftab-fcode = 'PRNT'.
    APPEND ftab.
  ELSE.
    ftab-fcode = 'SAVE'.
    APPEND ftab.
    ftab-fcode = 'ACTV'.
    APPEND ftab.
    ftab-fcode = 'DACT'.
    APPEND ftab.
  ENDIF.

  SET PF-STATUS '100' EXCLUDING ftab.

ENDFORM.                    " USER_STATUS
*&---------------------------------------------------------------------*
*&      Form  REALLY?
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM really?.
  DATA l_answer(1).

  PERFORM pop_up USING
      'Please confirm to again!'
      'Do you really want to save?' ' '
                 CHANGING l_answer.

  IF l_answer NE 'J'.
    g_error = true.
    MESSAGE s000 WITH 'Processing was canceled by user.'.
  ENDIF.


ENDFORM.                    " REALLY?
*&---------------------------------------------------------------------*
*&      Form  POP_UP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1802   text
*      -->P_1803   text
*      -->P_1804   text
*      <--P_L_ANSWER  text
*----------------------------------------------------------------------*
FORM pop_up USING    p_text p_text2 p_canc
            CHANGING p_answer.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
       EXPORTING
            textline1      = p_text
            textline2      = p_text2
            titel          = 'Check!'
            cancel_display = p_canc
       IMPORTING
            answer         = p_answer.


ENDFORM.                    " POP_UP
*&---------------------------------------------------------------------*
*&      Form  SAVE_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM post_bapi.
  PERFORM get_selected_rows TABLES $gt_out.




ENDFORM.                    " SAVE_TABLE
*&---------------------------------------------------------------------*
*&      Form  REALLY_ACT_OR_DACT?
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_OK_CODE  text
*----------------------------------------------------------------------*
FORM really_act_or_dact? USING  p_ok_code.

  DATA l_answer(1).

  PERFORM pop_up USING
      'The data will be changed!'
      'Do you really want to change status?' ' '
                 CHANGING l_answer.

  IF l_answer NE 'J'.
    g_error = true.
    MESSAGE s000 WITH 'Processing was canceled by user.'.
  ENDIF.

ENDFORM.                    " REALLY_ACT_OR_DACT?
*&---------------------------------------------------------------------*
*&      Form  changed_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_OK_CODE  text
*----------------------------------------------------------------------*
FORM changed_status USING    p_ok_code.

  PERFORM get_selected_rows TABLES $gt_out.

  IF p_ok_code EQ 'ACTV'.
    gt_out-status = true.
  ELSE.
    gt_out-status = false.
  ENDIF.

  MODIFY gt_out TRANSPORTING status WHERE chk EQ true.

  CLEAR flag_data_changed.

ENDFORM.                    " changed_status
*&---------------------------------------------------------------------*
*&      Form  clear_chk
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_chk.
  CLEAR gt_out-chk.
  MODIFY gt_out TRANSPORTING chk WHERE chk EQ true.
ENDFORM.                    " clear_chk
*&---------------------------------------------------------------------*
*&      Form  bar_print
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bar_print.

  PERFORM get_selected_rows TABLES $gt_out.
  READ TABLE $gt_out INDEX 1.
  IF sy-subrc EQ 0.
    PERFORM init_print.
    PERFORM printing.
  ELSE.
    MESSAGE s000 WITH 'No data to print!'.
  ENDIF.

ENDFORM.                    " bar_print
*&---------------------------------------------------------------------*
*&      Form  set_icon
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_GOOD_ROW_ID  text
*      -->P_ICON_LED_RED  text
*----------------------------------------------------------------------*
FORM set_icon USING pr_data_changed
                    TYPE REF TO cl_alv_changed_data_protocol
                    p_row_id
                    p_icon.

  CALL METHOD pr_data_changed->modify_cell
        EXPORTING i_row_id    = p_row_id
                  i_fieldname = 'ICON'
                  i_value     = p_icon.

  IF p_icon EQ icon_led_red.
    CALL METHOD pr_data_changed->modify_cell
          EXPORTING i_row_id    = p_row_id
                    i_fieldname = 'ERR'
                    i_value     = true.
  ELSE.
    CALL METHOD pr_data_changed->modify_cell
          EXPORTING i_row_id    = p_row_id
                    i_fieldname = 'ERR'
                    i_value     = space.

  ENDIF.

ENDFORM.                    " set_icon
*&---------------------------------------------------------------------*
*&      Form  get_vendor
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ITAB_EAUFN  text
*      <--P_ITAB_LIFNR  text
*----------------------------------------------------------------------*
FORM get_vendor USING    p_eaufn
                CHANGING p_lifnr.

  CHECK p_eaufn NE space.

  DATA pf_objnr TYPE j_objnr.

  DATA: BEGIN OF lt_covp OCCURS 0,
          gkont   TYPE gkont,
          wkgbtr  TYPE wkgxxx,
        END   OF lt_covp.

  DATA:   wa_tab(72) TYPE c,
          ftab LIKE STANDARD TABLE OF wa_tab WITH NON-UNIQUE
                    DEFAULT KEY INITIAL SIZE 5.

  CALL FUNCTION 'QRP_APO_PKOSA_AUFNR_TO_OBJNR'
       EXPORTING
            if_pkosa_aufnr = p_eaufn
       IMPORTING
            ef_pkosa_objnr = pf_objnr.
* {
  wa_tab = 'GKONT MAX( WKGBTR ) as WKGBTR'.
  APPEND wa_tab TO ftab.

  SELECT DISTINCT (ftab)
        INTO TABLE lt_covp
  FROM covp WHERE objnr EQ pf_objnr
              AND kokrs EQ *tka02-kokrs
              AND gkoar EQ 'K'
              AND awtyp NE 'AUAK'
  GROUP by gkont
  ORDER BY wkgbtr DESCENDING.
* }

  CHECK sy-subrc EQ 0.

  READ TABLE lt_covp INDEX 1.
  IF sy-subrc EQ 0.
    p_lifnr = lt_covp-gkont.
  ENDIF.

ENDFORM.                    " get_vendor
*&---------------------------------------------------------------------*
*&      Form  get_all_equi
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_all_equi.

  data $it_all_equi like it_all_equi occurs 0 with header line.

  __cls : it_eqart, it_all_equi.

  CHECK g_exq EQ false.
*  SELECT eqart INTO TABLE it_eqart FROM ankaz
*                WHERE eqtyp IN s_eqtyp
*                  AND eqart NE space.
*
*  CHECK NOT it_eqart[] IS INITIAL.

  SELECT
      a~equnr a~eqtyp a~eqart
      a~ansdt a~answt a~waers a~elief
      a~herst
      a~baumm a~matnr a~sernr
      a~werk
      a~meins
      b~gewrk
      c~swerk c~stort c~iloan d~eqktx
      c~swerk as eswerk
      c~stort as estort
      b~erdat
      INTO CORRESPONDING FIELDS OF TABLE it_all_equi
      FROM equi AS a
      INNER JOIN equz AS b
      ON b~equnr EQ a~equnr
     AND b~datbi >= p_vali
     AND b~datab <= p_vali
      JOIN iloa AS c
      ON c~iloan EQ b~iloan
      INNER JOIN eqkt AS d
      ON d~equnr EQ a~equnr
      AND d~spras EQ 'E'
*      FOR ALL ENTRIES IN it_eqart
      WHERE a~eqtyp IN s_eqtyp
*        AND a~eqart EQ it_eqart-eqart
        AND a~equnr IN s_equnr
*        AND a~ansdt NE '00000000'
        AND a~lvorm EQ space.

  $it_all_equi[] = it_all_equi[].
  sort $it_all_equi by equnr ascending
                       erdat descending.

  DELETE ADJACENT DUPLICATES FROM $it_all_equi
        COMPARING equnr.

  __cls gt_esort.

  loop at $it_all_equi.
    move-corresponding $it_all_equi to gt_esort.
    append gt_esort.
  endloop.

  sort gt_esort by equnr.

ENDFORM.                    " get_all_equi
*&---------------------------------------------------------------------*
*&      Form  FILL_IT_ILOAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_iloan.
  DATA $ix LIKE sy-tabix.

  __cls : it_iloan,it_equnr.

  CHECK p_all EQ true.

  IF g_exq EQ false.

    __cls : it_equi.

    SORT it_all_equi BY iloan.
    LOOP AT itab.
      CHECK NOT itab-iloan IS INITIAL.
      $ix = sy-tabix.
      READ TABLE it_all_equi WITH KEY iloan = itab-iloan
                                        BINARY SEARCH.
      IF sy-subrc EQ 0.
        itab-equnr = it_all_equi-equnr.
        itab-gewrk = it_all_equi-gewrk.
        MODIFY itab INDEX $ix TRANSPORTING equnr gewrk.
        it_equnr-iloan = it_all_equi-iloan.
        it_equnr-equnr = it_all_equi-equnr.
        it_equnr-gewrk = it_all_equi-gewrk.
        IF it_equnr-equnr NE space.
          APPEND it_equnr.
          MOVE-CORRESPONDING it_all_equi TO it_equi.
          APPEND it_equi.
          CLEAR it_equi.
        ENDIF.
      ENDIF.
    ENDLOOP.

    SORT it_equnr BY iloan.
    DELETE ADJACENT DUPLICATES FROM it_equnr
      COMPARING equnr.

  ENDIF.

*  CHECK g_exq EQ true.

  LOOP AT itab.
    CHECK NOT itab-iloan IS INITIAL.
    it_iloan-iloan = itab-iloan.
    APPEND it_iloan.
  ENDLOOP.

  SORT it_iloan.
  DELETE ADJACENT DUPLICATES FROM it_iloan.
  CHECK NOT it_iloan[] IS INITIAL.

  __process 'Read Equi. data...' '35'.

  SELECT  iloan equnr gewrk INTO TABLE it_equnr
    FROM equz
  FOR ALL ENTRIES IN it_iloan
 WHERE iloan EQ it_iloan-iloan
   AND equnr IN s_equnr
   AND datbi >= p_vali
   AND datab <= p_vali .

  SORT it_equnr BY iloan.
  DELETE ADJACENT DUPLICATES FROM it_equnr
    COMPARING equnr.

  LOOP AT itab.
    CHECK NOT itab-iloan IS INITIAL.
    $ix = sy-tabix.
    READ TABLE it_equnr WITH KEY iloan = itab-iloan
                                      BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-equnr = it_equnr-equnr.
      itab-gewrk = it_equnr-gewrk.
      MODIFY itab INDEX $ix TRANSPORTING equnr gewrk.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " FILL_IT_ILOAN
*&---------------------------------------------------------------------*
*&      Form  GET_ROW_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_row_data.

  IF NOT s_ebeln[] IS INITIAL.
    IF p_all EQ true.
      SELECT  a~bukrs a~anln1 a~anln2
              a~anlar a~lifnr a~aktiv a~mcoa1 a~meins
              a~menge a~mcoa1 a~eaufn
              b~stort
              b~iloan b~swerk a~anlkl
      INTO CORRESPONDING FIELDS OF TABLE itab
      FROM anla AS a
      LEFT OUTER JOIN iloa AS b
      ON b~anlnr EQ a~anln1
       WHERE a~bukrs EQ p_bukrs
         AND a~anln1 IN s_anln1
         AND a~anlkl IN s_anlkl
         AND a~aktiv IN s_aktiv
         AND a~mcoa1 IN s_mcoa1
         AND a~xloev NE true
*        AND a~aktiv NE '00000000'                          "UD1K956418
         AND exists ( SELECT anln1
                        FROM ekkn
                       WHERE ebeln IN s_ebeln
                         AND anln1 EQ a~anln1
                         AND anln2 EQ a~anln2 ) .
    ELSE.
      SELECT  a~bukrs a~anln1 a~anln2
              a~anlar a~lifnr a~aktiv a~mcoa1 a~meins
              a~menge a~mcoa1 a~eaufn
              b~stort
              b~iloan b~swerk
              c~equnr
*              c~gewrk
              b~ppsid as gewrk
              d~ansdt d~answt d~elief d~herst
              e~eqktx a~anlkl
      INTO CORRESPONDING FIELDS OF TABLE itab
      FROM anla AS a
      INNER JOIN iloa AS b
      ON b~anlnr EQ a~anln1
      INNER JOIN equz AS c
      ON c~iloan EQ b~iloan
     AND c~datbi >= p_vali
     AND c~datab <= p_vali
      INNER JOIN equi AS d
      ON d~equnr EQ c~equnr
      INNER JOIN eqkt AS e
      ON e~equnr EQ d~equnr
     AND e~spras EQ 'E'
       WHERE a~bukrs EQ p_bukrs
         AND a~anln1 IN s_anln1
         AND a~anlkl IN s_anlkl
         AND a~aktiv IN s_aktiv
         AND a~mcoa1 IN s_mcoa1
         AND a~xloev NE true
*        AND a~aktiv NE '00000000'                          "UD1K956418
         AND e~equnr IN s_equnr
         AND exists ( SELECT anln1
                        FROM ekkn
                       WHERE ebeln IN s_ebeln
                         AND anln1 EQ a~anln1
                         AND anln2 EQ a~anln2 ) .
    ENDIF.
  ELSE.
    IF p_all EQ true.
      SELECT  a~bukrs a~anln1 a~anln2
              a~anlar a~lifnr a~aktiv a~mcoa1 a~meins
              a~menge a~mcoa1 a~eaufn
              b~stort
              b~iloan b~swerk a~anlkl
      INTO CORRESPONDING FIELDS OF TABLE itab
      FROM anla AS a
      LEFT OUTER JOIN iloa AS b
      ON b~anlnr EQ a~anln1
       WHERE a~bukrs EQ p_bukrs
         AND a~anln1 IN s_anln1
         AND a~anlkl IN s_anlkl
         AND a~aktiv IN s_aktiv
         AND a~mcoa1 IN s_mcoa1
         AND a~xloev NE true.
*        AND a~aktiv NE '00000000'.                         "UD1K956418
    ELSE.

      SELECT  a~bukrs a~anln1 a~anln2
              a~anlar a~lifnr a~aktiv a~mcoa1 a~meins
              a~menge a~mcoa1 a~eaufn
              b~stort
              b~iloan b~swerk
              c~equnr
*              c~gewrk
              b~ppsid as gewrk
              d~ansdt d~answt d~elief d~herst
              e~eqktx a~anlkl
      INTO CORRESPONDING FIELDS OF TABLE itab
      FROM anla AS a
      INNER JOIN iloa AS b
      ON b~anlnr EQ a~anln1
      INNER JOIN equz AS c
      ON c~iloan EQ b~iloan
     AND c~datbi >= p_vali
     AND c~datab <= p_vali
      INNER JOIN equi AS d
      ON d~equnr EQ c~equnr
      INNER JOIN eqkt AS e
      ON e~equnr EQ d~equnr
     AND e~spras EQ 'E'
       WHERE a~bukrs EQ p_bukrs
         AND a~anln1 IN s_anln1
         AND a~anlkl IN s_anlkl
         AND a~aktiv IN s_aktiv
         AND a~mcoa1 IN s_mcoa1
         AND a~xloev NE true
*        AND a~aktiv NE '00000000'                          "UD1K956418
         AND e~equnr IN s_equnr.
    ENDIF.
  ENDIF.

ENDFORM.                    " GET_ROW_DATA
