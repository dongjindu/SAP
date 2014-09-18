************************************************************************
* Program Name           : ZIPP110I_APS_7DW1
* Author                 : Woon-Mook, Choi
* Creation Date          : 2003.08.23.
* Specifications By      : Woon-Mook, Choi
* Pattern           : 1.1
* Development Request No :
* Addl Documentation:
* Description       : Un-input and MITU Order
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
report zipp110i_aps_7dw1 message-id zmpp
                         no standard page heading.
*
tables: ztpp_wosum,
        ztpp_pmt07dw.
tables: ausp.
data: it_wosum like ztpp_wosum  occurs 0 with header line.
data: wa_wosum            like ztpp_wosum .
data: it_7dw              like table of zspp_pmt07dw with header line.
data: begin of lt_wosum occurs  0,
        FSC LIKE ztpp_wosum-FSC,
        SEQQTY LIKE ztpp_wosum-SEQQTY,
        RP01TQ LIKE ztpp_wosum-RP01TQ,
        MITUQTY LIKE ztpp_wosum-MITUQTY,
        MODEL(2),
      end of lt_wosum.
data: begin of it_equi occurs  0,
        objek  like  ausp-objek,
        atwrt  like  ausp-atwrt,
      end of it_equi.
data: g_model(3).
data: it_lines  type  i,
      W_INDEX LIKE SY-TABIX..

*----------------------------------------------------------------------

************************************************************************
*              SELECTION SCREEN LAYOUT                                 *
************************************************************************
selection-screen: begin of block b1 with frame .
selection-screen begin of line.
parameters: p_run          type c as checkbox  default 'X'.
selection-screen comment  (55) text-001 for field p_run.
selection-screen end of line.
selection-screen: end of block b1.

************************************************************************
*              START-OF-SELECTION PROCESSING                           *
************************************************************************
start-of-selection.
  check p_run = 'X'  .
  refresh: it_wosum.
  clear: wa_wosum, it_wosum.
** Changed by Furong on 10/11/07 for EBOM
  refresh: it_wosum, LT_WOSUM.
*  EXEC SQL PERFORMING loop_output.
*    SELECT SUBSTR(FSC,7,2), SUM(SEQQTY - RP01TQ), SUM(MITUQTY)
*      INTO  :WA_WOSUM-FSC, :WA_WOSUM-SEQQTY, :WA_WOSUM-MITUQTY
*      FROM   ztpp_wosum
*      where  mandt = :sy-mandt
*     GROUP BY  SUBSTR(FSC,7,2)
*  ENDEXEC.

  select FSC SEQQTY RP01TQ MITUQTY INTO CORRESPONDING FIELDS OF TABLE
     LT_WOSUM
    FROM ztpp_wosum.
*     where  mandt = sy-mandt.
  LOOP AT LT_WOSUM.
    W_INDEX = SY-TABIX.
    IF LT_WOSUM-FSC+13(1) = SPACE.
       LT_WOSUM-MODEL = LT_WOSUM-FSC+6(2).
    ELSE.
       LT_WOSUM-MODEL = LT_WOSUM-FSC+5(2).
    ENDIF.
    MODIFY LT_WOSUM INDEX W_INDEX.
  ENDLOOP.

  loop at LT_wosum.
     it_wosum-FSC = LT_WOSUM-MODEL.
     it_wosum-SEQQTY = LT_wosum-SEQQTY - LT_wosum-RP01TQ.
     IT_wosum-MITUQTY = LT_wosum-MITUQTY.
     COLLECT it_wosum.
     clear LT_wosum.
  endloop.

** End of change
  refresh  it_7dw.
  loop at it_wosum.
    clear  it_7dw.

    move   '1'               to  it_7dw-plnt.
    move   '1'               to  it_7dw-line.
    move   it_wosum-fsc(2)   to  it_7dw-modl.
    move   'E'               to  it_7dw-rgub.
    move   it_wosum-seqqty   to  it_7dw-seqq.
    move   it_wosum-mituqty  to  it_7dw-hqty.
    concatenate it_wosum-fsc(2) '%' into g_model .

** : Logic change(10/18/2011 KDM) Performance Tuning
** : KDM01
    perform  seq_code_search  using      g_model
                              changing  it_7dw-scod.
    append it_7dw.
  endloop.

* Interface table record delete ---------------------
  delete from ztpp_pmt07dw  client specified
         where  mandt = sy-mandt .
* ---------------------------------------------------
  data: l_flag,
        l_text(60) type c,
        l_int type i.
  loop at it_7dw.
    clear: ztpp_pmt07dw.
    move-corresponding  it_7dw    to   ztpp_pmt07dw.
    move  sy-uname                to   ztpp_pmt07dw-zuser.
    move  sy-datum                to   ztpp_pmt07dw-zsdat.
    move  sy-uzeit                to   ztpp_pmt07dw-zstim.
    insert ztpp_pmt07dw .
    if sy-subrc ne 0.
      message w001 with text-102.
      rollback work.
      l_flag = 'X'.
      exit.
    endif.
  endloop.
  if l_flag <> 'X'.
    describe table it_7dw lines l_int.
    write l_int to l_text left-justified.
    concatenate 'Created Record Count :' l_text
      into l_text.
    message s001 with l_text.
    message s001 with text-101.
  endif.
** ---------------------------------------------------------------------
**  EAI FUNCTION CALL  Un-input & MITU information send to ALC
**
*  data: l_subrc type  i.
**
*  refresh: it_7dw. clear it_7dw.
*  select * from  ztpp_pmt07dw
*     into corresponding fields of table  it_7dw
*     where plnt eq  '1'.
*
*  call function 'Z_FPP_SET_PMT07DW'
*       destination  'WMPP01'
*       tables
*                  mituord =  it_7dw.
**       EXCEPTIONS
**                  Subrc   =  l_subrc.
*
*  if l_subrc  ne  space.
*    write:/ sy-uline(90).
*    loop at it_7dw.
*      write:/ sy-vline, it_7dw-plnt,
*              sy-vline, it_7dw-line,
*              sy-vline, it_7dw-modl,
*              sy-vline, it_7dw-scod,
*              sy-vline, it_7dw-seqq,
*              sy-vline, it_7dw-hqty,
*              sy-vline, it_7dw-zzret,
*              sy-vline.
*    endloop.
*    write:/ sy-uline(90).
*  endif.


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
form seq_code_search using     p_model  changing  p_scod.
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

* UD1K941348 by IG.MOON 8/17/2007 {

* data: l_ausp   like table of ausp   with header line.
*
*  select *  into table l_ausp
*    from ausp          " UP TO 1 ROWS
*   where objek in ( select objek from ausp where atinn = l_model
*                                             and atwrt like p_model
*                                             and klart = '002'  )
*     and atinn = l_bodys
*     and klart = '002'   .
*
*****//  ORDER BY atwrt DESCENDING.
*****//  ENDSELECT.
*
*  delete l_ausp where objek = space.
*  sort l_ausp by atwrt descending.
*
** : ECC Upgrade error correction(10/18/2011 - KDM01)
** : 4.6C Logic Block(11/26/2011)
*  data : begin of l_ausp occurs 0,
*           objek     like ausp-objek,
*           atwrt     like ausp-atwrt,
*         end   of  l_ausp.
*
*  select objek atwrt into table l_ausp
*    from ausp up to 1 rows
*   where objek in ( select objek from ausp where klart = '002'
*                                             and atinn = l_model
*                                             and atwrt like p_model )
*     and klart = '002'
*     and atinn = l_bodys
*     and objek ne space
*     order by atwrt descending.
*
**}
*
*  read table l_ausp index 1.
*
*  select  single  atwrt
*    into   p_scod                   " The biggest SEQUENCE_CODE
*    from   ausp
*    where  objek  eq   l_ausp-objek
*      and  atinn  =    l_seqcd      "P_SEQUENCE_CODE
*      and  klart   =    '002'      .
*
**  endif.

** : ECC Upgrade error correction(10/18/2011 - KDM01)
** If this program have a problem in performance,
** please use this part.

  data : begin of l_ausp occurs 0,
           objek     like ausp-objek,
           atwrt     like ausp-atwrt,
         end   of  l_ausp.

  select objek atwrt into table l_ausp
    from ausp "up to 1 rows
   where objek in ( select objek from ausp where klart = '002'
                                             and atinn = l_model
                                             and atwrt like p_model )
     and atinn = l_bodys
     and klart = '002'
*     and objek ne space
*     order by atwrt descending
    .

*}

  SORT l_ausp BY atwrt descending.
  read table l_ausp index 1.

  select  single  atwrt
    into   p_scod                   " The biggest SEQUENCE_CODE
    from   ausp
    where  objek  eq   l_ausp-objek
      and  atinn  =    l_seqcd      "P_SEQUENCE_CODE
      and  klart   =    '002'      .

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
