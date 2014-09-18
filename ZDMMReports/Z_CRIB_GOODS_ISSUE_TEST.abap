report z_crib_goods_issue_test .

constants:  false value ' ',
            true  value 'X'.

define __cls.                          " clear & refresh
  clear &1.refresh &1.
end-of-definition.

define __process.
  perform show_progress using &1 &2.
end-of-definition.

define __message.
  call function 'POPUP_TO_INFORM'
       exporting
            titel = &1
            txt1  = &2
            txt2  = sy-subrc.
end-of-definition.

define u_break.
  if p_debug eq true.
    break-point.
  endif.
end-of-definition.
data  f(50).                           " for get cursor field

data : w_materialdocument like bapi2017_gm_head_ret-mat_doc.

parameters : p_werks like marc-werks default 'P001',
             p_loc   like afpo-lgort default 'P451',
             p_matnr like marc-matnr default 'N_CRIBTEST_NOV2',
             p_bwart like tqhu1-bwart default '291',
             p_qty   like aufm-erfmg default '1',
             p_date like sy-datum default sy-datum,
             p_text(20) default 'Test...',
             p_debug as checkbox.

start-of-selection.
  perform bapi_post.

*&---------------------------------------------------------------------*
*&      Form  BAPI_post
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form bapi_post.

  data : w_goodsmvt_header  like bapi2017_gm_head_01,
         w_goodsmvt_code    like bapi2017_gm_code,
         w_goodsmvt_headret like bapi2017_gm_head_ret,
         w_matdocumentyear  like bapi2017_gm_head_ret-doc_year,
         it_goodsmvt_item
              like table of bapi2017_gm_item_create  with header line,
         it_goodsmvt_serialnumber
              like table of bapi2017_gm_serialnumber with header line,
         it_return
              like table of bapiret2                 with header line.

  data $subrc.

  clear : w_goodsmvt_header,
          w_goodsmvt_code,
          w_goodsmvt_headret,
          w_materialdocument,
          w_matdocumentyear,
          it_return,
          it_return[].

  move : p_date to w_goodsmvt_header-pstng_date,
         p_date to w_goodsmvt_header-doc_date,
         p_text to w_goodsmvt_header-header_txt,
         '03' to w_goodsmvt_code-gm_code.

  it_goodsmvt_item-material  = p_matnr.
  it_goodsmvt_item-move_type = p_bwart.
  it_goodsmvt_item-plant     = p_werks.
  it_goodsmvt_item-stge_loc  = p_loc.
  it_goodsmvt_item-entry_qnt = p_qty.

  append it_goodsmvt_item.


  u_break.

  call function 'Z_GOODSMVT_CREATE'
       exporting
            goodsmvt_header  = w_goodsmvt_header
            goodsmvt_code    = w_goodsmvt_code
       importing
            goodsmvt_headret = w_goodsmvt_headret
            materialdocument = w_materialdocument
            matdocumentyear  = w_matdocumentyear
       tables
            goodsmvt_item    = it_goodsmvt_item
            return           = it_return.

  u_break.

  read table it_return with key type = 'E'.
  if sy-subrc eq 0.
    rollback work.
    write:/ 'error!.'.
  else.
    commit work and wait.
    write:/ 'Posted with #',
             w_materialdocument hotspot color col_positive.
  endif.

  __cls it_goodsmvt_item.

endform.                    " BAPI_post


at line-selection.

  check w_materialdocument ne space.


  set parameter id 'MBN' field w_materialdocument.
  call transaction 'MB03' and skip first screen.
