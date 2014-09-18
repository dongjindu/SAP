FUNCTION z_fmm_textedit.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(EDIT) TYPE  CHAR1 OPTIONAL
*"     REFERENCE(TITLE) TYPE  CHAR50 OPTIONAL
*"     REFERENCE(LINE_LENGTH) TYPE  SY-INDEX DEFAULT 60
*"     REFERENCE(START_COLUMN) LIKE  SY-CUCOL DEFAULT 10
*"     REFERENCE(START_ROW) LIKE  SY-CUROW DEFAULT 3
*"     REFERENCE(END_COLUMN) LIKE  SY-CUCOL DEFAULT 60
*"     REFERENCE(END_ROW) LIKE  SY-CUROW DEFAULT 25
*"  TABLES
*"      TEXTTAB
*"----------------------------------------------------------------------
  CLEAR:   it_text, p_edit, p_title, startx, starty, endx, endy,
           p_line_length.
  REFRESH  it_text.

  p_line_length = line_length.
  p_edit    = edit.
  p_title   = title.
  it_text[] = texttab[].
  startx = start_column.
  starty = start_row.
  endx   = end_column.
  endy   = end_row.

* necessary to flush the automation queue
  CLASS cl_gui_cfw DEFINITION LOAD.

  CALL SCREEN 100 STARTING AT startx starty  ENDING AT endx endy .

  CHECK NOT it_text[] IS INITIAL.
  CLEAR texttab. REFRESH texttab.
  texttab[] = it_text[].

ENDFUNCTION.
