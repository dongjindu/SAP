*----------------------------------------------------------------------*
***INCLUDE MP987000I01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  TRACK_LISTBOX_CHECK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE track_listbox_check INPUT.

  CASE ok-code.
    WHEN 'TRACK'.
*     set up to track the value of the grade list boxes.
      PERFORM change_listbox_grade USING p9870-track.
  ENDCASE.

ENDMODULE.                 " TRACK_LISTBOX_CHECK  INPUT
