REPORT zemmpm99e_kdpo_temp NO STANDARD PAGE HEADING
                           LINE-SIZE 200
                           LINE-COUNT 65
                           MESSAGE-ID zmmm.

**---
INCLUDE : zrmmpmxxr_incl.

FIELD-SYMBOLS : <fs>.

**---
DATA : it_itab01 LIKE ztmm_kd_asn OCCURS 0 WITH HEADER LINE.

DATA : it_intern TYPE  kcde_cells OCCURS 0 WITH HEADER LINE.

**---
DATA : w_index TYPE i,
       w_lines TYPE i.

**---
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
PARAMETERS : p_file01 LIKE rlgrap-filename.
SELECTION-SCREEN END OF BLOCK block1.

**---
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file01.
  PERFORM f4_p_upfile USING p_file01.

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
  CLEAR : it_itab01, it_itab01[].

  PERFORM excel_file_upload USING p_file01.
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

  PERFORM update_table_stl.
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
    w_index = it_intern-col + 1.
    ASSIGN COMPONENT w_index OF STRUCTURE it_itab01 TO <fs>.
    MOVE : it_intern-value TO <fs>.
    AT END OF row.
      APPEND it_itab01.
      CLEAR it_itab01.
    ENDAT.
  ENDLOOP.

  MODIFY ztmm_kd_asn FROM TABLE it_itab01.

  DESCRIBE TABLE it_itab01 LINES w_lines.
ENDFORM.                    " update_table_stl
