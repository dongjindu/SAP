REPORT zalv.

*****************************************************************
* Use of colours in ALV grid (cell, line and column) *
*****************************************************************

* Table
TABLES : mara.

* Type
TYPES : BEGIN OF ty_mara,
matnr LIKE mara-matnr,
matkl LIKE mara-matkl,
counter(4) TYPE n,
free_text(15) TYPE c,
color_line(4) TYPE c, " Line color
color_cell TYPE lvc_t_scol, " Cell color
END OF ty_mara.

* Structures
DATA : wa_mara TYPE ty_mara,
wa_fieldcat TYPE lvc_s_fcat,
is_layout TYPE lvc_s_layo,
wa_color TYPE lvc_s_scol.

* Internal table
DATA : it_mara TYPE STANDARD TABLE OF ty_mara,
it_fieldcat TYPE STANDARD TABLE OF lvc_s_fcat,
it_color TYPE TABLE OF lvc_s_scol.

* Variables
DATA : okcode LIKE sy-ucomm,
w_alv_grid TYPE REF TO cl_gui_alv_grid,
w_docking_container TYPE REF TO cl_gui_docking_container.

*PARAMETER
PARAMETERS : p_column AS CHECKBOX,
p_line AS CHECKBOX,
p_cell AS CHECKBOX.

START-OF-SELECTION.
  PERFORM get_data.

END-OF-SELECTION.

  PERFORM fill_catalog.
  PERFORM fill_layout.

  CALL SCREEN 2000.
*&--------------------------------------------------------------*
*& Module status_2000 OUTPUT
*&--------------------------------------------------------------*
* text
*---------------------------------------------------------------*
MODULE status_2000 OUTPUT.

*SET PF-STATUS '2000'.

ENDMODULE. " status_2000 OUTPUT
*&---------------------------------------------------------------*
*& Module user_command_2000 INPUT
*&---------------------------------------------------------------*
* text
*----------------------------------------------------------------*
MODULE user_command_2000 INPUT.

  DATA : w_okcode LIKE sy-ucomm.

  MOVE okcode TO w_okcode.
  CLEAR okcode.

  CASE w_okcode.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE. " user_command_2000 INPUT
*&--------------------------------------------------------------*
*& Module alv_grid OUTPUT
*&--------------------------------------------------------------*
* text
*---------------------------------------------------------------*
MODULE alv_grid OUTPUT.

  IF w_docking_container IS INITIAL.
    PERFORM create_objects.
    PERFORM display_alv_grid.
  ENDIF.

ENDMODULE. " alv_grid OUTPUT
*&--------------------------------------------------------------*
*& Form create_objects
*&--------------------------------------------------------------*
* text
*---------------------------------------------------------------*
* --> p1 text
* <-- p2 text
*---------------------------------------------------------------*
FORM create_objects.

* Ratio must be included in [5..95]

  CREATE OBJECT w_docking_container
  EXPORTING
  ratio = 95
  EXCEPTIONS
  cntl_error = 1
  cntl_system_error = 2
  create_error = 3
  lifetime_error = 4
  lifetime_dynpro_dynpro_link = 5
  others = 6.

  CREATE OBJECT w_alv_grid
  EXPORTING
  i_parent = w_docking_container.

ENDFORM. " create_objects
*&--------------------------------------------------------------*
*& Form display_alv_grid
*&--------------------------------------------------------------*
* text
*---------------------------------------------------------------*
* --> p1 text
* <-- p2 text
*---------------------------------------------------------------*
FORM display_alv_grid.

  CALL METHOD w_alv_grid->set_table_for_first_display
  EXPORTING
  is_layout = is_layout
  CHANGING
  it_outtab = it_mara
  it_fieldcatalog = it_fieldcat
  EXCEPTIONS
  invalid_parameter_combination = 1
  program_error = 2
  too_many_lines = 3
  OTHERS = 4.

ENDFORM. " display_alv_grid
*&--------------------------------------------------------------*
*& Form get_data
*&--------------------------------------------------------------*
* text
*---------------------------------------------------------------*
* --> p1 text
* <-- p2 text
*---------------------------------------------------------------*
FORM get_data.

  SELECT * FROM mara UP TO 5 ROWS.
    CLEAR : wa_mara-color_line, wa_mara-color_cell.

    MOVE-CORRESPONDING mara TO wa_mara.
    ADD 1 TO wa_mara-counter.
    MOVE 'Blabla' TO wa_mara-free_text.

    IF wa_mara-counter = '0002'
    AND p_line = 'X'.
* Color line
      MOVE 'C410' TO wa_mara-color_line.
    ELSEIF wa_mara-counter = '0004'
    AND p_cell = 'X'.
* Color cell
      MOVE 'FREE_TEXT' TO wa_color-fname.
      MOVE '5' TO wa_color-color-col.
      MOVE '1' TO wa_color-color-int.
      MOVE '1' TO wa_color-color-inv.
      APPEND wa_color TO it_color.
      wa_mara-color_cell[] = it_color[].
    ENDIF.

    APPEND wa_mara TO it_mara.
  ENDSELECT.

ENDFORM. " get_data
*&--------------------------------------------------------------*
*& Form fill_catalog
*&--------------------------------------------------------------*
* text
*---------------------------------------------------------------*
* --> p1 text
* <-- p2 text
*---------------------------------------------------------------*
FORM fill_catalog.

*****************************************************************
* Colour code : *
* Colour is a 4-char field where : *
* - 1st char = C (color property) *
* - 2nd char = color code (from 0 to 7) *
* 0 = background color *
* 1 = blue *
* 2 = gray *
* 3 = yellow *
* 4 = blue/gray *
* 5 = green *
* 6 = red *
* 7 = orange *
* - 3rd char = intensified (0=off, 1=on) *
* - 4th char = inverse display (0=off, 1=on) *
* *
* Colour overwriting priority : *
* 1. Line *
* 2. Cell *
* 3. Column *
*****************************************************************
  DATA : w_position TYPE i VALUE '1'.

  CLEAR wa_fieldcat.
  MOVE w_position TO wa_fieldcat-col_pos.
  MOVE 'MATNR' TO wa_fieldcat-fieldname.
  MOVE 'MARA' TO wa_fieldcat-ref_table.
  MOVE 'MATNR' TO wa_fieldcat-ref_field.
  APPEND wa_fieldcat TO it_fieldcat.

  ADD 1 TO w_position.

  CLEAR wa_fieldcat.
  MOVE w_position TO wa_fieldcat-col_pos.
  MOVE 'MATKL' TO wa_fieldcat-fieldname.
  MOVE 'MARA' TO wa_fieldcat-ref_table.
  MOVE 'MATKL' TO wa_fieldcat-ref_field.
* Color column
  IF p_column = 'X'.
    MOVE 'C610' TO wa_fieldcat-emphasize.
  ENDIF.
  APPEND wa_fieldcat TO it_fieldcat.

  ADD 1 TO w_position.

  CLEAR wa_fieldcat.
  MOVE w_position TO wa_fieldcat-col_pos.
  MOVE 'COUNTER' TO wa_fieldcat-fieldname.
  MOVE 'N' TO wa_fieldcat-inttype.
  MOVE '4' TO wa_fieldcat-intlen.
  MOVE 'Counter' TO wa_fieldcat-coltext.
  APPEND wa_fieldcat TO it_fieldcat.

  ADD 1 TO w_position.

  CLEAR wa_fieldcat.
  MOVE w_position TO wa_fieldcat-col_pos.
  MOVE 'FREE_TEXT' TO wa_fieldcat-fieldname.
  MOVE 'C' TO wa_fieldcat-inttype.
  MOVE '20' TO wa_fieldcat-intlen.
  MOVE 'Text' TO wa_fieldcat-coltext.
  APPEND wa_fieldcat TO it_fieldcat.

ENDFORM. " fill_catalog
*&--------------------------------------------------------------*
*& Form fill_layout
*&--------------------------------------------------------------*
* text
*---------------------------------------------------------------*
* --> p1 text
* <-- p2 text
*---------------------------------------------------------------*
FORM fill_layout.

* Field that identify color line in internal table
  MOVE 'COLOR_LINE' TO is_layout-info_fname.

* Field that identify cell color in inetrnal table
  MOVE 'COLOR_CELL' TO is_layout-ctab_fname.

ENDFORM. " fill_layout
