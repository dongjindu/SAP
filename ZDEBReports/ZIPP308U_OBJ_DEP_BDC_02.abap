************************************************************************
* Program Name      : ZIPP308U_OBJ_DEP_BDC
* Author            : Bongsoo, Kim
* Creation Date     : 2003.08.22.
* Specifications By : Bongsoo, Kim
* Pattern           : 2.1
* Development Request No : UD1K901999
* Addl Documentation:
* Description       : OBJECT DEPENDENCY
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
report zipp308u_obj_dep_bdc
                no standard page heading
                line-size  1023
                line-count 65
                message-id zmbm.
*----------------------------------------------------------------------*
* INCLUDE
*----------------------------------------------------------------------*
include zipp308l_obj_dep_bdc_t2.
include zipp308l_obj_dep_bdc_f02.
*----------------------------------------------------------------------*
* EVENT
*----------------------------------------------------------------------*
initialization.
  perform initialization.

at selection-screen output.
  perform screen_modify.


start-of-selection.
  perform read_process.

  perform check_data.

  perform data_process.
  perform bdc_process.
  perform update_process.
  perform write_process.

end-of-selection.
*&---------------------------------------------------------------------*
*&      Form  enter_code
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form enter_code.
  data $string(30).
  data $string2(30).
  data $line(2) type n.

  if not  it_aodp-cspec  is initial .
    $line = '02'.
  else.
    $line = '01'.
  endif.

  concatenate 'RSTXP-TDLINECOM(' $line ')' into $string.
  concatenate 'RSTXP-TDLINE(' $line ')'    into $string2.

  perform dynpro using:
        'X' 'SAPLEDITOR_START'    '3210',
        ' ' 'BDC_CURSOR'          $string,
         ' ' $string2    ln_c, "EDIC: Program
         ' ' 'BDC_OKCODE'  '=ACIL'.
  ln_cnt = ln_cnt + 10.
  last_line  = last_line + 4.

endform.                    " enter_code
*&---------------------------------------------------------------------*
*&      Form  check_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_data.
  data $ix like sy-tabix.
  loop at it_aodp.
    $ix = sy-tabix.
    select single knnam into cukb-knnam from cukb
        where knnam eq it_aodp-dpid.
    if sy-subrc eq 0.
      it_aodp-zbdat = sy-datum.
      it_aodp-zbtim = sy-uzeit.
      it_aodp-zbnam = sy-uname.
      it_aodp-zresult = 'P'.
      it_aodp-zmsg = 'already exists.' .
      modify it_aodp index $ix transporting zbdat zbtim zbnam
                               zresult zmsg .
      write: / it_aodp-dpid, ' already exists.'.
*      DELETE IT_AODP INDEX $IX.
    endif.
  endloop.
endform.                    " check_data
*&---------------------------------------------------------------------*
*&      Form  create_string
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_STRING  text
*      <--P_$STRING  text
*----------------------------------------------------------------------*
form create_string tables   p_i_string structure i_string
                   changing p_string.
  data $flag(1).
*  delete i_string where $code eq '^^'.
  sort i_string.
  delete adjacent duplicates from i_string.

  clear p_string.
  loop at i_string.
    at last.
      $flag = 'X'.
    endat.
    if  $flag eq 'X'.
      concatenate p_string '''' i_string-$code ''''
                   into p_string.
    else.
      concatenate p_string '''' i_string-$code '''' ','
                   into p_string.
    endif.
  endloop.

endform.                    " create_string
