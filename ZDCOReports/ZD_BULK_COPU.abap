*&---------------------------------------------------------------------*
*& Report  ZD_BULK_COPU
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

report  zd_bulk_copu.

data:  begin of gt_itab occurs 0,
       line(128),
       end   of gt_itab.

parameters: p_prog like trdir-name obligatory.

at selection-screen.

start-of-selection.

  read   report p_prog into gt_itab.
  editor-call   for     gt_itab.
  insert report p_prog from gt_itab.
