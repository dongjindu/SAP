************************************************************************
* Program Name           : ZIPP110I_APS_7DW
* Author                 : Woon-Mook, Choi
* Creation Date          : 2003.08.23.
* Specifications By      : Woon-Mook, Choi
* Development Request No :
* Addl Documentation:
* Description       : Un-input and MITU Order
*----------------------------------------------------------------------
* Modification Logs
* Date       Developer    RequestNo    Description
*----------------------------------------------------------------------
*
*
************************************************************************
report zipp110i_aps_7dw  no standard page heading.
*
tables: ztpp_wosum,
        ztpp_pmt07dw.
tables: ausp.
data: it_wosum like ztpp_wosum  occurs 0 with header line.
data: begin of wa_wosum.
        include structure ztpp_wosum.
data  end of wa_wosum.
data: begin of it_7dw occurs 0.
        include structure zspp_pmt07dw.
*data:   zzret  type c,  "Changed by Tonkey 12/10/2003
data: end of it_7dw.

data: begin of it_equi occurs  0,
        objek  like  ausp-objek,
        atwrt  like  ausp-atwrt,
      end of it_equi.
data: g_model(2).
data: it_lines  type  i.
parameters   go  type  c.
*----------------------------------------------------------------------
start-of-selection.

*  SELECT * FROM  ztpp_wosum
**  into corresponding fields of table it_wosum
*    WHERE ( seqqty - rp01tq ) > 0.

  refresh: it_wosum.
  clear wa_wosum.

  EXEC SQL PERFORMING loop_output.
    SELECT SUBSTR(FSC,1,2), SUM(SEQQTY - RP01TQ), SUM(MITUQTY)
      INTO  :WA_WOSUM-FSC, :WA_WOSUM-SEQQTY, :WA_WOSUM-MITUQTY
      FROM   ztpp_wosum
      where  mandt = :sy-mandt
     GROUP BY  SUBSTR(FSC,1,2)
  ENDEXEC.
  refresh  it_7dw.
  loop at it_wosum.
    clear  it_7dw.

    move   '1'               to  it_7dw-plnt.
    move   '1'               to  it_7dw-line.
    move   it_wosum-fsc(2)   to  it_7dw-modl.
    move   'E'               to  it_7dw-rgub.
    move   it_wosum-seqqty   to  it_7dw-seqq.
    move   it_wosum-mituqty  to  it_7dw-hqty.
    g_model = it_wosum-fsc(2).
    perform  seq_code_search  using      g_model
                              changing  it_7dw-scod.
    append it_7dw.

  endloop.

* Interface table record delete ---------------------
  select single * from ztpp_pmt07dw
    where  plnt  =  '1'.
  if sy-subrc eq 0.
    delete from ztpp_pmt07dw  client specified
           where  plnt  =  '1'.
    if sy-subrc  ne  0.
      write:/ 'DELETE ERROR'.
      stop.
    endif.
  endif.
* ---------------------------------------------------
  loop at it_7dw.
    clear: ztpp_pmt07dw.
    move-corresponding  it_7dw    to   ztpp_pmt07dw.
    move  sy-uname                to   ztpp_pmt07dw-zuser.
    move  sy-datum                to   ztpp_pmt07dw-zsdat.
    move  sy-uzeit                to   ztpp_pmt07dw-zstim.
    insert  ztpp_pmt07dw.
    if sy-subrc ne 0.
      message i003(zmpp) with '7DW TABLE CREATE ERROR' sy-subrc.
      rollback work.
      exit.
    endif.
    write:/ it_7dw-plnt, it_7dw-line, it_7dw-modl,
            it_7dw-scod, it_7dw-seqq, it_7dw-hqty.
  endloop.
* ---------------------------------------------------------------------
*  EAI FUNCTION CALL  Un-input & MITU information send to ALC
*
  data: l_subrc type  i.
*
  refresh: it_7dw. clear it_7dw.
  select * from  ztpp_pmt07dw
     into corresponding fields of table  it_7dw
     where plnt eq  '1'.

  call function 'Z_FPP_SET_PMT07DW'
       destination  'WMPP01'
       tables
                  mituord =  it_7dw.
*       EXCEPTIONS
*                  Subrc   =  l_subrc.

  if l_subrc  ne  space.
    write:/ sy-uline(90).
    loop at it_7dw.
      write:/ sy-vline, it_7dw-plnt,
              sy-vline, it_7dw-line,
              sy-vline, it_7dw-modl,
              sy-vline, it_7dw-scod,
              sy-vline, it_7dw-seqq,
              sy-vline, it_7dw-hqty,
              sy-vline, it_7dw-zzret,
              sy-vline.
    endloop.
    write:/ sy-uline(90).
  endif.


*&---------------------------------------------------------------------*
*&      Form  loop_output
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form loop_output.
  move-corresponding   wa_wosum  to  it_wosum.
  append it_wosum.
  clear wa_wosum.
endform.                    " loop_output
*&---------------------------------------------------------------------*
*&      Form  SEQ_CODE_SEARCH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_WOSUM_FSC  text
*----------------------------------------------------------------------*
form seq_code_search using     p_model
                     changing  p_scod.

  data: l_model  like  ausp-atinn.
  data: l_bodys  like  ausp-atinn.
  data: l_seqcd  like  ausp-atinn.

  perform  conversion_atinn_call  using  'P_MODEL'
                                          l_model.
  perform  conversion_atinn_call  using  'P_BODY_SERIAL'
                                          l_bodys.
  perform  conversion_atinn_call  using  'P_SEQUENCE_CODE'
                                          l_seqcd.
* ---------------------------------------------------------------------

  refresh: it_equi.  clear  it_equi.

  EXEC SQL PERFORMING  APPENDING_EQUI.
    SELECT  OBJEK, ATWRT
      INTO  :IT_EQUI
      FROM   AUSP
      WHERE  ATINN  = :L_MODEL
  ENDEXEC.

  describe table  it_equi   lines  it_lines.

  check it_lines > 0.
*
  loop at it_equi.
    select  single  atwrt
      into   it_equi-atwrt
      from   ausp
      where  objek  eq   it_equi-objek
        and  atinn  =    l_bodys.  "P_BODY_SERIAL
    if sy-subrc  ne  0.
      clear  it_equi-atwrt.
    endif.
    modify  it_equi.
  endloop.

  data: l_scod(2) type n.

  sort it_equi  by  atwrt  descending. " The biggest data search

  read table  it_equi  index   1.
  if sy-subrc eq 0.
    select  single  atwrt
      into   p_scod                   " The biggest SEQUENCE_CODE
      from   ausp
      where  objek  eq   it_equi-objek
        and  atinn  =    l_seqcd.     "P_SEQUENCE_CODE
  endif.

endform.                    " SEQ_CODE_SEARCH
*&---------------------------------------------------------------------*
*&      Form  APPENDING_EQUI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form appending_equi.
  check  it_equi-atwrt(2) eq  g_model.
  append  it_equi.
endform.                    " APPENDING_EQUI
*&---------------------------------------------------------------------*
*&      Form  CONVERSION_ATINN_CALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0274   text
*      -->P_L_MODEL  text
*----------------------------------------------------------------------*
form conversion_atinn_call using    p_value
                                    p_atinn.

  call function 'CONVERSION_EXIT_ATINN_INPUT'
       exporting
            input  = p_value
       importing
            output = p_atinn.

endform.                    " CONVERSION_ATINN_CALL
