*----------------------------------------------------------------------
* Program ID        : ZAFIU128
* Title             : [FI] Register Asset Inventory
* Created on        : Last day of Oct. 2007
* Created by        : I.G.MOON
* Specifications By : Andy Choi
* Description       : Register Asset Inventory
*----------------------------------------------------------------------
REPORT zafiu128 MESSAGE-ID zmco.
TABLES : anla, anlz, equi, t499s, ekkn, ekko, csks ,ztfiu128,*ztfiu128,
         *tka02 .

INCLUDE zacoui00.
TABLES : marc,sscrfields.
INCLUDE <icon>.                        " icon

*--------------------------------------------------------------------*
* SELECTION-SCREEN
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.
PARAMETERS p_bukrs LIKE t001-bukrs MEMORY ID buk OBLIGATORY
                                DEFAULT 'H201'.
SELECT-OPTIONS : s_aktiv FOR ztfiu128-aktiv,
                 s_anln1 FOR ztfiu128-anln1,
                 s_kostl FOR csks-kostl,
                 s_mcoa1 FOR ztfiu128-mcoa1.
SELECT-OPTIONS s_ebeln FOR ekko-ebeln.
SELECT-OPTIONS s_anlkl FOR anla-anlkl.

PARAMETERS p_nfix AS CHECKBOX.
PARAMETERS p_actv AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK bl1.

SELECTION-SCREEN BEGIN OF BLOCK view-result WITH FRAME.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN PUSHBUTTON  1(30) traf USER-COMMAND traf.
SELECTION-SCREEN END OF BLOCK view-result.

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
* Macros
*----------------------------------------------------------------------*

DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
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
      g_ix     LIKE sy-tabix,
      gv_index      TYPE i.

DATA : dir.
DATA cursor_f(10).
DATA $sheet(3) TYPE n.
FIELD-SYMBOLS <f>.

****************************** Global Data *****************************

TYPES: BEGIN OF ty_row_tab.
        INCLUDE STRUCTURE ztfiu128.
TYPES  eaufn TYPE am_aufnr.
TYPES: END OF ty_row_tab.

TYPES: BEGIN OF ty_out.
INCLUDE  TYPE ty_row_tab.
TYPES:
     icon TYPE icon-id,
     icon2 TYPE icon-id,
     chk(1),
     err(1),
     $number(13),
     $snumber(8),
     ktext TYPE text40,
     wfl_text(20),
     ind_text(20).
TYPES   celltab  TYPE lvc_t_styl.
TYPES   tabcolor TYPE slis_t_specialcol_alv.
TYPES: END OF ty_out.

DATA  : itab       TYPE TABLE OF ty_row_tab WITH HEADER LINE,
        it_row_tab TYPE TABLE OF ty_row_tab WITH HEADER LINE,
        gt_out     TYPE TABLE OF ty_out     WITH HEADER LINE,
        gt_ztfiu128 TYPE TABLE OF ty_row_tab WITH HEADER LINE.

DATA: BEGIN OF it_anlz OCCURS 0,
        bukrs	TYPE bukrs,      " Company Code
        anln1	TYPE anln1,      " Main asset number
        anln2	TYPE anln2,      " Asset sub-number
        kostl	TYPE kostl,
        kostlv TYPE kostlv,
        adatu TYPE adatu,
        bdatu TYPE bdatu,      " Date validity ends
        stort TYPE stort,
      END   OF it_anlz.

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
              zseri  TYPE zseri,
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

    METHODS:
    handle_double_click FOR EVENT double_click OF cl_gui_alv_grid
            IMPORTING e_row
                      e_column
                      es_row_no.

  PRIVATE SECTION.
    DATA: inserted_rows TYPE ztfiu128_key,
          deleted_rows TYPE STANDARD TABLE OF ztfiu128.
    DATA  error_in_data TYPE c.
    DATA  initial_table TYPE c.

** Methods to modularize event handler method HANDLE_DATA_CHANGED:
*
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

    CALL METHOD update_delta_tables( er_data_changed ).

* check mt_good_cells semantically
    CALL METHOD perform_semantic_checks( er_data_changed ).

    IF error_in_data = 'X'.
      CALL METHOD er_data_changed->display_protocol.
    ENDIF.

  ENDMETHOD.                    " handle_data_changed

*-------------------------------------------------------
  METHOD update_delta_tables.

    DATA: lt_good_cells TYPE lvc_t_modi,
          ls_good TYPE lvc_s_modi,
          ls_key TYPE ztfiu128_k,
          ls_ztfiu128 TYPE ztfiu128,
          l_anln1 LIKE gt_out-anln1,
          l_row_id LIKE sy-tabix,
          lt_ins_rows TYPE ztfiu128_key,
          lt_key TYPE ztfiu128_k,
          dup_chk(1).

    DATA : flag_inserted, flag_deleted.

    DATA: l_ins_row TYPE lvc_s_moce,
          l_del_row TYPE lvc_s_moce,
          ls_outtab LIKE LINE OF gt_out.

    LOOP AT pr_data_changed->mt_deleted_rows INTO l_del_row.
      READ TABLE gt_out INTO ls_outtab INDEX l_del_row-row_id.
      IF sy-subrc NE 0.
        MESSAGE i000(0k) WITH text-e01.
      ELSE.
        MOVE-CORRESPONDING ls_outtab TO ls_ztfiu128.
        APPEND ls_ztfiu128 TO deleted_rows.
* If this line was inserted just before it is deleted:
        DELETE me->inserted_rows
             WHERE anln1  = ls_outtab-anln1
             AND   anln2 = ls_outtab-anln2
             AND   zseri = ls_outtab-zseri.
      ENDIF.
      flag_deleted = true.
    ENDLOOP.

    CHECK flag_deleted NE true.

    IF me->table_is_initial( ) EQ 'X'.
      CALL METHOD get_cell_values
            EXPORTING row_id          = 1
                      pr_data_changed = pr_data_changed
            IMPORTING key             = ls_key.

      APPEND ls_key TO inserted_rows.
      CALL METHOD me->set_table_is_not_initial.
    ENDIF.

    FIELD-SYMBOLS: <fs> TYPE table.    " Output table

    LOOP AT pr_data_changed->mt_inserted_rows INTO l_ins_row.

      ASSIGN pr_data_changed->mp_mod_rows->* TO <fs>.

      LOOP AT <fs> INTO ls_outtab.
        ls_outtab-bukrs  = g_bukrs.
        ls_outtab-zseri  = '0001'.
        MODIFY <fs> FROM ls_outtab INDEX sy-tabix.
      ENDLOOP.

      CALL METHOD get_cell_values
              EXPORTING row_id          = l_ins_row-row_id
                        pr_data_changed = pr_data_changed
              IMPORTING key             = ls_key.

      APPEND ls_key TO inserted_rows.
      flag_inserted = true.

    ENDLOOP.

    CHECK flag_inserted NE true.

    LOOP AT pr_data_changed->mt_good_cells INTO ls_good.
      l_row_id = ls_good-row_id.

      CASE ls_good-fieldname.
        WHEN 'ZMEMO'.
          EXIT.
        WHEN 'ICON' OR 'ICON2' OR 'WFL_TEXT' OR 'IND_TEXT'.
          CALL METHOD pr_data_changed->modify_cell
                 EXPORTING i_row_id    = ls_good-row_id
                           i_fieldname = ls_good-fieldname
                           i_value     = space.
          MESSAGE i000 WITH 'You can not change this field.'.
          __set_icon_no_error.
          EXIT.
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
         ls_good-fieldname EQ 'ANLN2' OR
         ls_good-fieldname EQ 'ZSERI'.
        dup_chk = true.
      ENDIF.
      EXIT.
    ENDLOOP.

    CHECK error_in_data NE true.

    CALL METHOD get_cell_values
         EXPORTING row_id          = l_row_id
                   pr_data_changed = pr_data_changed
         IMPORTING key             = ls_key.

    CHECK g_bukrs NE space
      AND ls_key-anln1 NE space
      AND ls_key-zseri NE space
      AND dup_chk EQ true.

    SELECT SINGLE * FROM ztfiu128 INTO ls_ztfiu128
              WHERE bukrs  = g_bukrs
                AND anln1  = ls_key-anln1
                AND anln2 =  ls_key-anln2
                AND zseri  = ls_key-zseri.

    IF sy-subrc = 0.
      __set_error text-m03.
    ELSE.

      __set_icon_no_error.

    ENDIF.

    READ TABLE gt_out WITH KEY bukrs  = g_bukrs
                               anln1  = ls_key-anln1
                               anln2 =  ls_key-anln2
                               zseri  = ls_key-zseri
                               TRANSPORTING NO FIELDS.

    IF sy-subrc = 0 AND l_row_id NE sy-tabix.
      __set_error text-m04.

    ELSE.
      __set_icon_no_error.
    ENDIF.

  ENDMETHOD.
*---------------------------------------------------------

  METHOD get_cell_values.
* get values of key cells of row ROW_ID

* BUKRS
    CALL METHOD pr_data_changed->get_cell_value
          EXPORTING
                 i_row_id    = row_id
                 i_fieldname = 'BUKRS'
               IMPORTING
                 e_value = key-bukrs.

    IF sy-subrc NE 0.
      MESSAGE i000(0k) WITH text-e02.
    ENDIF.

* ANLN1
    CALL METHOD pr_data_changed->get_cell_value
          EXPORTING
                 i_row_id    = row_id
                 i_fieldname = 'ANLN1'
               IMPORTING
                 e_value = key-anln1.

    IF sy-subrc NE 0.
      MESSAGE i000(0k) WITH text-e02.
    ENDIF.
* ANLN2
    CALL METHOD pr_data_changed->get_cell_value
          EXPORTING
                 i_row_id    = row_id
                 i_fieldname = 'ANLN2'
               IMPORTING
                 e_value = key-anln2.

    IF sy-subrc NE 0.
      MESSAGE i000(0k) WITH text-e02.
    ENDIF.

* SERI
    CALL METHOD pr_data_changed->get_cell_value
          EXPORTING
                 i_row_id    = row_id
                 i_fieldname = 'ZSERI'
               IMPORTING
                 e_value = key-zseri.

    IF sy-subrc NE 0.
      MESSAGE i000(0k) WITH text-e02.
    ENDIF.

  ENDMETHOD.

*---------------------------------------------------------
  METHOD perform_semantic_checks.

    DATA: ls_good TYPE lvc_s_modi,
          l_anln1 TYPE anln1.

    LOOP AT pr_data_changed->mt_good_cells INTO ls_good.
      CASE ls_good-fieldname.
        WHEN 'ZMEMO'. EXIT.
        WHEN 'ANLN1'.
          CALL METHOD pr_data_changed->get_cell_value
             EXPORTING
               i_row_id = ls_good-row_id
               i_fieldname = ls_good-fieldname
             IMPORTING
               e_value = l_anln1.

         IF ( l_anln1 >= '000090000000' AND l_anln1 <= '000099999999' )
                                                    OR l_anln1 EQ space.
            __set_icon_no_error.

          ELSE.

            __set_error text-m01.

          ENDIF.
      ENDCASE.

      IF error_in_data EQ true.

        __set_icon_error.

      ENDIF.

    ENDLOOP.

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

  METHOD handle_double_click.
    PERFORM double_click USING e_row
                               e_column
                               es_row_no.
  ENDMETHOD.                    " handle_double_click

ENDCLASS.                   " LCL_EVENT_RECEIVER Implementation

DATA g_event_receiver  TYPE REF TO lcl_event_receiver.

************************************************************************
DATA  : flag_data_changed,
    info(80).

DATA: BEGIN OF ftab OCCURS 10,
    fcode(6),
  END OF ftab.
*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.

  __cls s_anlkl.
  s_anlkl-sign = 'I'.
  s_anlkl-option = 'EQ'.
  s_anlkl-low = '00003800'.
  APPEND s_anlkl.

*  sy-title = '[FI] Register Asset Inventory'.
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
      PERFORM transfer_.
      PERFORM move_out.
      PERFORM set_output .

    WHEN 'PSET' OR 'FC04'.
      PERFORM get_print USING dialog.
  ENDCASE.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*

  PERFORM initialize.
  PERFORM get_row_data.
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
   'ZASTFIX'  ' ' 'X' ' ' 'X' ' ',
   'ANLN1'  ' ' 'X' ' ' 'X' ' ',
   'ANLN2'  ' ' 'X' ' ' 'X' ' ',
   'MCOA1'  ' ' 'X' ' ' 'X' ' ',
   'ZSERI'   ' ' 'X' ' ' 'X' ' ',
   'LIFNR'  ' ' 'X' ' ' 'X' ' ',
   'AKTIV'  ' ' 'X' ' ' 'X' ' ',
   'ADATU'  ' ' 'X' ' ' 'X' ' ',
   'KOSTL'  ' ' 'X' ' ' 'X' ' '.

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
    APPEND gt_out.
  ENDLOOP.

  PERFORM apply_icon.

ENDFORM.                    " MOVE_OUT_
*&---------------------------------------------------------------------*
*&      Form  TRANSFER_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM transfer_.

  CLEAR g_error.

  IF NOT s_ebeln[] IS INITIAL.

    SELECT  a~bukrs a~anln1 a~anln2
            a~anlar a~lifnr a~aktiv a~mcoa1 a~meins
            a~menge a~mcoa1 a~eaufn
    INTO CORRESPONDING FIELDS OF TABLE itab
    FROM anla AS a
     WHERE a~bukrs EQ p_bukrs
       AND a~anln1 IN s_anln1
       AND a~anlkl IN s_anlkl
       AND a~aktiv IN s_aktiv
       AND a~mcoa1 IN s_mcoa1
       AND a~xloev NE true
       AND ( a~aktiv NE space AND a~aktiv NE '00000000' )
       AND exists ( SELECT anln1
                      FROM ekkn
                     WHERE ebeln IN s_ebeln
                       AND anln1 EQ a~anln1
                       AND anln2 EQ a~anln2 ) .

  ELSE.

    SELECT  a~bukrs a~anln1 a~anln2
            a~anlar a~lifnr a~aktiv a~mcoa1 a~meins
            a~menge a~mcoa1 a~eaufn
    INTO CORRESPONDING FIELDS OF TABLE itab
    FROM anla AS a
     WHERE a~bukrs EQ p_bukrs
       AND a~anln1 IN s_anln1
       AND a~anlkl IN s_anlkl
       AND a~aktiv IN s_aktiv
       AND a~mcoa1 IN s_mcoa1
       AND a~xloev NE true
       AND ( a~aktiv NE space AND a~aktiv NE '00000000' ) .

  ENDIF.

  IF NOT itab[] IS INITIAL.
    __process 'Read Asset data...' '10'.

    SORT itab BY  bukrs anln1 anln2.
    __cls it_anlz.

* cost center
    SELECT bukrs anln1 anln2 kostl kostlv adatu bdatu stort
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
    __cls it_anlc.

    __process 'Read Asset data...' '15'.

* values
    SELECT  bukrs anln1 anln2 answl
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

  ENDIF.

  __process 'Read Asset data...' '20'.

  DATA $ix LIKE sy-tabix.

  LOOP AT itab.
    $ix = sy-tabix.
    READ TABLE it_anlz WITH KEY bukrs = itab-bukrs
                                anln1 = itab-anln1
                                anln2 = itab-anln2
                                BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-kostl = it_anlz-kostl.
      itab-kostlv = it_anlz-kostlv.
      itab-adatu  = it_anlz-adatu.
      IF it_anlz-stort NE space.
        itab-stand = it_anlz-stort.
      ENDIF.
    ELSE.
      DELETE itab INDEX $ix.
      CONTINUE.
    ENDIF.

    READ TABLE it_anlc WITH KEY bukrs = itab-bukrs
                                anln1 = itab-anln1
                                anln2 = itab-anln2
                                BINARY SEARCH.
    IF sy-subrc EQ 0.
      itab-answl = it_anlc-answl.
    ENDIF.

    IF itab-lifnr IS INITIAL. " get vendor when lifnr is initial.
      PERFORM get_vendor USING itab-eaufn
                      CHANGING itab-lifnr.
    ENDIF.
    MODIFY itab INDEX $ix.
  ENDLOOP.

  __cls : gt_out,it_row_tab.

  DATA $answl LIKE it_row_tab-answl.

  LOOP AT itab.

    MOVE-CORRESPONDING itab TO it_row_tab.
    it_row_tab-zastfix = true.
    CLEAR it_row_tab-answl.

    IF itab-menge <> 0.
      it_row_tab-answl = itab-answl / itab-menge.
    ENDIF.
    CLEAR $answl.

    DO itab-menge TIMES.
      it_row_tab-zseri = sy-index.
      it_row_tab-menge = 1.
      it_row_tab-waers = 'USD'.
      IF sy-index EQ itab-menge.
        it_row_tab-answl = itab-answl - $answl.
      ELSE.
        ADD it_row_tab-answl TO $answl.
      ENDIF.
      APPEND it_row_tab.
    ENDDO.
    CLEAR  it_row_tab.
  ENDLOOP.

  PERFORM get_z_table.

  LOOP AT it_row_tab.
    $ix = sy-tabix.
    READ TABLE gt_ztfiu128 WITH KEY
                                  bukrs = it_row_tab-bukrs
                                  anln1 = it_row_tab-anln1
                                  anln2 = it_row_tab-anln2
                                  zseri  = it_row_tab-zseri
                                  BINARY SEARCH.
    IF sy-subrc EQ 0.
      IF it_row_tab-stand EQ space.
        it_row_tab-stand = gt_ztfiu128-stand.
      ENDIF.
      it_row_tab-active = gt_ztfiu128-active.
      it_row_tab-status = gt_ztfiu128-status.
      MODIFY it_row_tab INDEX $ix TRANSPORTING stand status.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " TRANSFER_
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

*      PERFORM REALLY?.
*      CHECK G_ERROR NE TRUE.

      PERFORM : save_table,
                refresh_alv,
                clear_chk.
      __focus g_grid.

    WHEN 'ACTV' OR 'DACT'.
*      CLEAR G_ERROR.
*      PERFORM REALLY_ACT_OR_DACT? USING OK_CODE.
*      CHECK G_ERROR NE TRUE.

      PERFORM : changed_status USING ok_code,
                apply_icon,
                save_table,
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
  SET HANDLER : g_event_receiver->handle_data_changed FOR g_grid,
                g_event_receiver->handle_double_click FOR g_grid.

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
    'X'  'ZASTFIX'         'C'                  1  'CHAR' '',
    'X'  'ANLN1'           'Asset#'            12  'CHAR' '',
    'X'  'ANLN2'           'Sb.A'               4  'CHAR' '',
    'X'  'MCOA1'           'Description'       40  'CHAR' '',
    'X'  'ZSERI'           'Serial#'            4  'CHAR' '',
    ' '  'MENGE'           'Quantity'          15  'QUAN' '',
    ' '  'LIFNR'           'Vendor'            10  'CHAR' '',
    ' '  'AKTIV'           'Acqui.'             8  'DATS' '',
    ' '  'ANSWL'           'Values'            15  'CURR' '',
    ' '  'KOSTL'           'CstCntr.'          10  'CHAR' '',
    ' '  'ADATU'           'Val.From'           8  'DATS' '',
    ' '  'STAND'           'Location'          10  'CHAR' '',
    ' '  'ICON'            'S'                  3  'ICON' '',
    ' '  'ERR'             'E'                  1  'CHAR' '',
    ' '  'ICON2'            'S'                 3  'ICON' '',
    ' '  'WFL_TEXT'        'W/F'               20  'CHAR' '',
    ' '  'IND_TEXT'        'Indicator'         20  'CHAR' '',
    ' '  'ZMEMO'           'Memo'              40  'CHAR' ''.

  LOOP AT gt_fcat INTO gs_fcat.

    CASE gs_fcat-fieldname.
      WHEN 'MENGE' OR 'ANSWL' OR 'ZSERI'.
        gs_fcat-just = 'R'.
    ENDCASE.

    IF gs_fcat-fieldname NE 'ICON'
      AND gs_fcat-fieldname NE 'ICON2'
      AND gs_fcat-fieldname NE 'WFL_TEXT'
      AND gs_fcat-fieldname NE 'IND_TEXT'
      AND gs_fcat-fieldname NE 'ERR'.
      gs_fcat-checktable = '!'.
      gs_fcat-ref_table = 'ZTFIU128'.
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
   'ZASTFIX'  '1' 0,
   'ANLN1'  '1' 0,
   'ANLN2'  '1' 0,
   'MCOA1'  '1' 0,
   'ZSERI'  '1' 0,
   'MENGE'  '2' 0,
   'LIFNR'  '2' 0,
   'AKTIV'  '2' 0,
   'ANSWL'  '2' 0,
   'ADATU'  '2' 0,
   'KOSTL'  '2' 0,
   'STAND'  '3' 0,
   'ICON'   '3' 0,
   'ICON2'  '2' 0,
   'WFL_TEXT' '2' 0,
   'IND_TEXT' '2' 0,
   'ZMEMO' '3' 0.

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
  MODIFY gt_out TRANSPORTING celltab WHERE zastfix EQ true.

  CLEAR gs_fcat.

  LOOP AT gt_fcat INTO gs_fcat.
    ls_celltab-fieldname = gs_fcat-fieldname.
    IF   ls_celltab-fieldname = 'STAND' OR
         ls_celltab-fieldname = 'ZMEMO'.
      ls_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
    ELSE.
      ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    ENDIF.
    INSERT ls_celltab INTO TABLE lt_celltab.
  ENDLOOP.

  CLEAR gt_out-celltab.
  INSERT LINES OF lt_celltab INTO TABLE gt_out-celltab.
  MODIFY gt_out TRANSPORTING celltab WHERE celltab IS initial
                                       AND zastfix EQ true.

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

  DATA: lt_celltab TYPE lvc_t_styl,
        ls_celltab TYPE lvc_s_styl.

  __cls lt_celltab.

  __cls gt_out-celltab.
  MODIFY gt_out TRANSPORTING celltab WHERE zastfix EQ false.

  CLEAR gs_fcat.

  LOOP AT gt_fcat INTO gs_fcat.
    ls_celltab-fieldname = gs_fcat1-fieldname.


    IF ls_celltab-fieldname = 'ZMEMO'.
      ls_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
    ELSE.
      ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    ENDIF.

    INSERT ls_celltab INTO TABLE lt_celltab.
  ENDLOOP.

  INSERT LINES OF lt_celltab INTO TABLE gt_out-celltab.
  MODIFY gt_out TRANSPORTING celltab WHERE zastfix EQ false.

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
*&      Form  get_row_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_row_data.
  __process 'Read Asset data...' '5'.

  PERFORM get_z_table.

  __cls it_row_tab.
  LOOP AT gt_ztfiu128.
    MOVE-CORRESPONDING gt_ztfiu128 TO it_row_tab.
    APPEND it_row_tab.
  ENDLOOP.

ENDFORM.                    " get_row_data
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

  LOOP AT gt_out.
    $ix = sy-tabix.

    CASE gt_out-active.
      WHEN 'X'.
        gt_out-icon = icon_led_green.
      WHEN OTHERS.
        gt_out-icon = icon_led_yellow.
    ENDCASE.


    CASE gt_out-status.
      WHEN '1'.
        gt_out-ind_text = 'Transfer'.
      WHEN '2'.
        gt_out-icon2 = icon_led_yellow.
        gt_out-wfl_text = 'Pending'.
        gt_out-ind_text = 'Scrap'.
      WHEN '3'.
        gt_out-icon2 = icon_led_yellow.
        gt_out-ind_text = 'Scrap'.
      WHEN OTHERS.
        gt_out-icon2 = space.
        gt_out-ind_text = ''.
        gt_out-wfl_text = ''.
    ENDCASE.

    MODIFY gt_out INDEX $ix TRANSPORTING icon icon2 wfl_text ind_text.
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
            form                        = 'ZFI_ASSET_FLABEL'
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

    PERFORM get_ktext USING $gt_out-kostl
                            $gt_out-stand
                   CHANGING $gt_out-ktext.

*    CONCATENATE $gt_out-anln1+4 '-' $gt_out-anln2 INTO $gt_out-$number.
    CONCATENATE $gt_out-anln1+4 '-' $gt_out-zseri INTO $gt_out-$number.

    CONCATENATE $gt_out-$number+4(4) $gt_out-$number+9(4)
        INTO $gt_out-$snumber.


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

*move zqmnum+6(6)  to zoptions-TDDATASET. "UD1K940504
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
  WRITE:
          icon_import AS ICON TO traf,
         'Transfer from Fixed Asset' TO traf+4(25).

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
FORM save_table.

  DATA: lv_cnt(5),
        lv_dcnt(5),
        lv_msg(200).                 " Message
* Save seleted data to table ZTFIU128
  CLEAR: lv_cnt.

  PERFORM get_selected_rows TABLES $gt_out.

  DATA  : i_ztfiu128 LIKE ztfiu128 OCCURS 0 WITH HEADER LINE,
          ls_ztfiu128 LIKE ztfiu128,
          lt_del_rows TYPE TABLE OF ztfiu128.

* Delete Lines
  CALL METHOD g_event_receiver->get_deleted_rows
            IMPORTING deleted_rows = lt_del_rows.

  DESCRIBE TABLE lt_del_rows LINES sy-tabix.
  IF sy-tabix GT 0.
    DELETE ztfiu128 FROM TABLE lt_del_rows.
    lv_dcnt = sy-dbcnt.
  ENDIF.

  CALL METHOD g_event_receiver->refresh_delta_tables.

  READ TABLE $gt_out WITH KEY zseri = space.
  IF sy-subrc EQ 0.
    MESSAGE i000 WITH 'You can not apply with the blank serial#(s).'
                      'Applying the recored(s) will be ignored!'.
  ENDIF.

  READ TABLE $gt_out WITH KEY stand = space.
  IF sy-subrc EQ 0.
    MESSAGE i000 WITH 'You can not apply with the blank location(s).'
                      'Applying the recored(s) will be ignored!'.
  ENDIF.

  LOOP AT $gt_out.
    CHECK $gt_out-zseri NE space.
    CHECK $gt_out-stand NE space.

    MOVE-CORRESPONDING $gt_out TO *ztfiu128.
     *ztfiu128-aedat = sy-datum.
     *ztfiu128-aenam = sy-uname.
    i_ztfiu128 = *ztfiu128.
    APPEND i_ztfiu128.
    lv_cnt = lv_cnt + 1.

  ENDLOOP.
  DATA : line1 LIKE sy-tabix,
         line2 LIKE sy-tabix.

  DESCRIBE TABLE gt_out LINES  line1.
  DESCRIBE TABLE $gt_out LINES  line2.

  READ TABLE i_ztfiu128 INDEX 1.
  IF sy-subrc EQ 0.
    IF line1 EQ line2.

      IF p_nfix EQ false.
        DELETE FROM ztfiu128
            WHERE bukrs EQ p_bukrs
              AND aktiv IN s_aktiv
              AND kostl IN s_kostl
              AND anln1 IN s_anln1
              AND mcoa1 IN s_mcoa1.
      ELSE.
        DELETE FROM ztfiu128
            WHERE bukrs EQ p_bukrs
              AND aktiv IN s_aktiv
              AND kostl IN s_kostl
              AND anln1 IN s_anln1
              AND mcoa1 IN s_mcoa1
              AND zastfix EQ space.
      ENDIF.
    ENDIF.

    MODIFY ztfiu128 FROM TABLE i_ztfiu128.
    COMMIT WORK.
  ENDIF.

  DESCRIBE TABLE lt_del_rows LINES lv_dcnt.
  IF lv_dcnt > 0.
    IF lv_cnt > 0.
      CONCATENATE 'Data''s been deleted' lv_dcnt  'rec(s),'
                  'saved' lv_cnt 'rec(s).'
             INTO lv_msg SEPARATED BY space.
    ELSE.
      CONCATENATE 'Data''s been deleted' lv_dcnt  'rec(s).'
             INTO lv_msg SEPARATED BY space.
    ENDIF.
  ELSE.
    CONCATENATE 'Data''s been saved;'
                 lv_cnt  'rec(s).'
            INTO lv_msg SEPARATED BY space.
  ENDIF.
  IF lv_dcnt > 0 OR lv_cnt > 0.
    MESSAGE s000 WITH lv_msg.
  ENDIF.

  CLEAR flag_data_changed.

  PERFORM  apply_icon.

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
    gt_out-active = true.
  ELSE.
    gt_out-active = false.
  ENDIF.

  MODIFY gt_out TRANSPORTING active WHERE chk EQ true.

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
*&      Form  get_z_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_z_table.
  __cls gt_ztfiu128.

  DATA:   wa_tab(72) TYPE c,
          etab LIKE STANDARD TABLE OF wa_tab WITH NON-UNIQUE
                    DEFAULT KEY INITIAL SIZE 5,
          atab LIKE STANDARD TABLE OF wa_tab WITH NON-UNIQUE
                    DEFAULT KEY INITIAL SIZE 5.

  IF p_nfix EQ true.
    wa_tab = 'ZASTFIX ne ''X'''.
    APPEND wa_tab TO etab.
  ENDIF.

  IF p_actv EQ true.
    wa_tab = 'ACTIVE eq ''X'''.
    APPEND wa_tab TO atab.
  ENDIF.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE gt_ztfiu128
    FROM ztfiu128
    WHERE bukrs EQ p_bukrs
      AND aktiv IN s_aktiv
      AND kostl IN s_kostl
      AND anln1 IN s_anln1
      AND mcoa1 IN s_mcoa1
      AND (etab)
      AND (atab).

  SORT gt_ztfiu128 BY bukrs anln1 anln2 zseri.

ENDFORM.                    " get_z_table
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
*&      Form  get_ktext
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$GT_OUT_KOSTL  text
*      -->P_$GT_OUT_STAND  text
*      <--P_$GT_OUT_KTEXT  text
*----------------------------------------------------------------------*
FORM get_ktext USING    p_kostl
                        p_stand
               CHANGING p_ktext.

  CLEAR p_ktext.
  CHECK p_kostl NE space.
  CHECK p_stand NE space.
  DATA $werks TYPE werks_d.

*  select single werks into $werks
*          from csks
*          where kokrs eq *tka02-kokrs
*            and kostl eq p_kostl
*            and datbi >= sy-datum
*            and datab <= sy-datum.
*
*  check sy-subrc eq 0.

  SELECT SINGLE ktext INTO p_ktext
          FROM t499s
          WHERE " werks eq $werks  and
            stand EQ p_stand.

ENDFORM.                    " get_ktext
*&---------------------------------------------------------------------*
*&      Form  double_click
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*      -->P_E_COLUMN  text
*      -->P_ES_ROW_NO  text
*----------------------------------------------------------------------*
FORM double_click USING  e_row     TYPE lvc_s_row
                         e_column  TYPE lvc_s_col
                         es_row_no TYPE lvc_s_roid.

  CLEAR gv_index.
  gv_index = e_row-index.

  READ TABLE gt_out INDEX gv_index.

  IF sy-subrc EQ 0.
*    if e_column = 'ANLN1' or e_column = 'ANLN2'.
    CHECK gt_out-anln1 NE space.
    SET PARAMETER ID : 'AN1'  FIELD gt_out-anln1,
                       'AN2'  FIELD gt_out-anln2.
    CALL TRANSACTION 'AS03' AND SKIP FIRST SCREEN.
*    endif.
  ENDIF.

  CALL METHOD cl_gui_control=>set_focus EXPORTING control = g_grid.

ENDFORM.                    " DOUBLE_CLICK
