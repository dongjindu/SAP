REPORT zemmpm99e_upload NO STANDARD PAGE HEADING
                        LINE-SIZE 200
                        LINE-COUNT 65
                        MESSAGE-ID zmmm.

**---
INCLUDE : zrmmpmxxr_incl.

FIELD-SYMBOLS : <fs>.

**---
DATA : it_itab01 LIKE ztmm_mast OCCURS 0 WITH HEADER LINE,
       it_itab02 LIKE ztmm_delisch OCCURS 0 WITH HEADER LINE.

*DATA : BEGIN OF it_itab01 OCCURS 0,
*         werks LIKE ztmm_mast-werks,
*         matnr LIKE ztmm_mast-matnr,
*         dispo LIKE ztmm_mast-dispo,
*         spptl LIKE ztmm_mast-spptl,
*         feedr LIKE ztmm_mast-feedr,
*         zline LIKE ztmm_mast-zline,
*         works LIKE ztmm_mast-works,
*         rh_lh LIKE ztmm_mast-rh_lh,
*         stock_check LIKE ztmm_mast-stock_check,
*         feed_cycle LIKE ztmm_mast-feed_cycle,
*         ztime LIKE ztmm_mast-ztime,
*         erdat LIKE ztmm_mast-erdat,
*         erzet LIKE ztmm_mast-erzet,
*         ernam LIKE ztmm_mast-ernam,
*         aedat LIKE ztmm_mast-aedat,
*         aezet LIKE ztmm_mast-aezet,
*         aenam LIKE ztmm_mast-aenam,
*       END OF it_itab01.
*
*DATA : BEGIN OF it_itab02 OCCURS 0,
*         lifnr LIKE ztmm_delisch-lifnr,
*         matnr LIKE ztmm_delisch-matnr,
*         name1 LIKE ztmm_delisch-name1,
*         maktx LIKE ztmm_delisch-maktx,
*         time1 LIKE ztmm_delisch-time1,
*         time2 LIKE ztmm_delisch-time2,
*         time3 LIKE ztmm_delisch-time3,
*         time4 LIKE ztmm_delisch-time4,
*         time5 LIKE ztmm_delisch-time5,
*         time6 LIKE ztmm_delisch-time6,
*         time7 LIKE ztmm_delisch-time7,
*         time8 LIKE ztmm_delisch-time8,
*         time9 LIKE ztmm_delisch-time9,
*         time10 LIKE ztmm_delisch-time10,
*       END OF it_itab02.

DATA : it_intern TYPE  kcde_cells OCCURS 0 WITH HEADER LINE.

**---
DATA : w_index TYPE i,
       w_lines TYPE i.

**---
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS : p_r01 RADIOBUTTON GROUP gr1 DEFAULT 'X' USER-COMMAND rd1.
SELECTION-SCREEN POSITION 3.
SELECTION-SCREEN COMMENT 3(24) text-002 FOR FIELD p_r01.
PARAMETERS : p_file01 LIKE rlgrap-filename.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN ULINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS : p_r02 RADIOBUTTON GROUP gr1.
SELECTION-SCREEN POSITION 3.
SELECTION-SCREEN COMMENT 3(24) text-003 FOR FIELD p_r02.
PARAMETERS : p_file02 LIKE rlgrap-filename.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK block1.


**---
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file01.
  PERFORM f4_p_upfile USING p_file01.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file02.
  PERFORM f4_p_upfile USING p_file02.

**---
AT SELECTION-SCREEN OUTPUT.
  PERFORM control_screen_output.

**---
START-OF-SELECTION.
  PERFORM read_data.

**---
END-OF-SELECTION.
  IF it_intern[] IS INITIAL.
    MESSAGE s999 WITH text-m01.
  ELSE.
    PERFORM update_table.
    PERFORM display_result.
  ENDIF.




**---





*&---------------------------------------------------------------------*
*&      Form  f4_p_upfile
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4_p_upfile USING p_filename.
*---
  CALL FUNCTION 'WS_FILENAME_GET'
       EXPORTING
            def_path         = p_filename  "* File Name
            mask             = ',*.XLS,*.XLS.'
            mode             = 'O'
       IMPORTING
            filename         = p_filename
       EXCEPTIONS
            inv_winsys       = 1
            no_batch         = 2
            selection_cancel = 3
            selection_error  = 4
            OTHERS           = 5.
ENDFORM.                    " f4_p_upfile

*&---------------------------------------------------------------------*
*&      Form  read_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data.
*---
  CLEAR : it_itab01, it_itab01[], it_itab02, it_itab02[].

  IF p_r01 NE space.
    PERFORM excel_file_upload USING p_file01.
  ELSEIF p_r02 NE space.
    PERFORM excel_file_upload USING p_file02.
  ENDIF.
ENDFORM.                    " read_data

*&---------------------------------------------------------------------*
*&      Form  display_result
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_result.
*---
  ULINE.

  WRITE : / w_lines, text-010.

  ULINE.
ENDFORM.                    " display_result

*&---------------------------------------------------------------------*
*&      Form  control_screen_output
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM control_screen_output.
*---
  LOOP AT SCREEN.
    IF     p_r01 EQ 'X' AND screen-name = 'P_FILE02'.
      screen-input = '0'.
    ELSEIF p_r02 EQ 'X' AND screen-name = 'P_FILE01'.
      screen-input = '0'.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.                    " control_screen_output

*&---------------------------------------------------------------------*
*&      Form  excel_file_upload
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_FILE01  text
*----------------------------------------------------------------------*
FORM excel_file_upload USING    p_p_file01.
*---
  DATA : l_begin_col TYPE i VALUE '1',
         l_begin_row TYPE i VALUE '1',
         l_end_col   TYPE i VALUE '256',
         l_end_row   TYPE i VALUE '65536'.

  CALL FUNCTION 'KCD_EXCEL_OLE_TO_INT_CONVERT'
       EXPORTING
            filename                = p_p_file01
            i_begin_col             = l_begin_col
            i_begin_row             = l_begin_row
            i_end_col               = l_end_col
            i_end_row               = l_end_row
       TABLES
            intern                  = it_intern
       EXCEPTIONS
            inconsistent_parameters = 1
            upload_ole              = 2
            OTHERS                  = 3.
ENDFORM.                    " excel_file_upload

*&---------------------------------------------------------------------*
*&      Form  update_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_table.
*---
  SORT it_intern BY row col.

  IF p_r01 NE space.
    PERFORM update_table_stl.
  ELSEIF p_r02 NE space.
    PERFORM update_table_sub.
  ENDIF.
ENDFORM.                    " update_table

*&---------------------------------------------------------------------*
*&      Form  update_table_stl
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_table_stl.
*---
  LOOP AT it_intern.
*    MOVE : it_intern-col TO w_index.
    w_index = it_intern-col + 1.
    ASSIGN COMPONENT w_index OF STRUCTURE it_itab01 TO <fs>.
    MOVE : it_intern-value TO <fs>.
    AT END OF row.
      PERFORM read_mrp_controller.
      PERFORM get_unit_of_measure.
      PERFORM get_control_cycle.
      it_itab01-erdat = it_itab01-aedat = sy-datum.
      it_itab01-erzet = it_itab01-aezet = sy-uzeit.
      it_itab01-ernam = it_itab01-aenam = sy-uname.
      PERFORM check_material_master.
      CHECK sy-subrc EQ 0.
      APPEND it_itab01.
      CLEAR it_itab01.
    ENDAT.
  ENDLOOP.

  MODIFY ztmm_mast FROM TABLE it_itab01.

  DESCRIBE TABLE it_itab01 LINES w_lines.
ENDFORM.                    " update_table_stl

*&---------------------------------------------------------------------*
*&      Form  update_table_sub
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_table_sub.
*---
  LOOP AT it_intern.
*    MOVE : it_intern-col TO w_index.
    w_index = it_intern-col + 1.
    ASSIGN COMPONENT w_index OF STRUCTURE it_itab02 TO <fs>.
    MOVE : it_intern-value TO <fs>.
    AT END OF row.
*      it_itab02-erdat = it_itab02-aedat = sy-datum.
*      it_itab02-erzet = it_itab02-aezet = sy-uzeit.
*      it_itab02-ernam = it_itab02-aenam = sy-uname.
      APPEND it_itab02.
      CLEAR it_itab02.
    ENDAT.
  ENDLOOP.

  MODIFY ztmm_delisch FROM TABLE it_itab02.

  DESCRIBE TABLE it_itab02 LINES w_lines.
ENDFORM.                    " update_table_sub

*&---------------------------------------------------------------------*
*&      Form  read_mrp_controller
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_mrp_controller.
*---
  CLEAR : marc.

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
       EXPORTING
            input        = it_itab01-matnr
       IMPORTING
            output       = it_itab01-matnr
       EXCEPTIONS
            length_error = 1
            OTHERS       = 2.

  SELECT SINGLE dispo INTO it_itab01-dispo
                      FROM marc
                     WHERE matnr EQ it_itab01-matnr
                       AND werks EQ it_itab01-werks.
ENDFORM.                    " read_mrp_controller

*&---------------------------------------------------------------------*
*&      Form  check_material_master
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_material_master.
*---
  CLEAR : mara.

  SELECT SINGLE * FROM mara
                 WHERE matnr EQ it_itab01-matnr.
ENDFORM.                    " check_material_master

*&---------------------------------------------------------------------*
*&      Form  get_unit_of_measure
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_unit_of_measure.
*---
  CLEAR : mara.

  SELECT SINGLE meins INTO it_itab01-meins
                      FROM mara
                     WHERE matnr EQ it_itab01-matnr.
ENDFORM.                    " get_unit_of_measure

*&---------------------------------------------------------------------*
*&      Form  get_control_cycle
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_control_cycle.
*---
  CLEAR : pkhd.

  SELECT SINGLE prvbe INTO it_itab01-zline
                      FROM pkhd
                     WHERE matnr EQ it_itab01-matnr
                       AND werks EQ it_itab01-werks.
ENDFORM.                    " get_control_cycle
