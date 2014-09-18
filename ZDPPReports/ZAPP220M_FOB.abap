*----------------------------------------------------------------------*
***INCLUDE ZAPP220M_FOB .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  comlumn_value_select
*&---------------------------------------------------------------------*
*       Searching 219's Value (ZTBM_219_VALUE)
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form comlumn_value_select.

  select * from  ztbm_219_value
    into  corresponding  fields of table it_219val
    where model  eq   is219-model
      and serial eq   is219-name219.

endform.                    " comlumn_value_select
*&---------------------------------------------------------------------*
*&      Form  next_column_select
*&---------------------------------------------------------------------*
*       Searching The next Code Column.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form next_column_select.

  is219-name219 = is219-name219 +  1.
  perform 219_column_value.

  perform  comlumn_value_select.

endform.                    " next_column_select
*&---------------------------------------------------------------------*
*&      Form  219_column_value
*&---------------------------------------------------------------------*
*       Handling Error & Searching 219's description
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form 219_column_value.

  clear is219-desc219.

  if is219-name219 > '219'.
    message s000 with 'It is last 219 Options'.
    move   '219'   to  is219-name219.
  endif.
  if is219-name219 < '001'.
    message s000 with 'It is the first 219 options'.
    move   '001'   to  is219-name219.
  endif.

  select  single  desc_219 into  is219-desc219
    from  ztbm_219_desc
   where  model     eq  is219-model
     and  name_219  eq  is219-name219.

endform.                    " 219_column_value
*&---------------------------------------------------------------------*
*&      Form  PREV_column_select
*&---------------------------------------------------------------------*
*       Searching The Previous Code Column.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form prev_column_select.
  is219-name219 = is219-name219 -  1.
  perform 219_column_value.

  perform  comlumn_value_select.

endform.                    " PREV_column_select
