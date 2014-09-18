************************************************************************
* Program Name      : ZIPP108I_APS_6EA1
* Author            : Dongyeop, Han
* Creation Date     : 2003.11.12.
* Specifications By : Dongyeop, Han
* Pattern           : 1.1
* Development Request No : UD1K902031
* Addl Documentation:
* Description       : Monthly Actual 2(6EA)
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
report  zipp108i_aps_6ea1 no standard page heading
                          message-id zmpp.

************************************************************************
*              D A T A     A R E A                                     *
************************************************************************
tables:ztpp_wosum2,
       ztpp_pmt06ea.
data: begin of it_wosum2 occurs 0,
      cr_date     like ztpp_wosum2-cr_date,
      mi          like ztpp_wosum2-mi,
      wo_ser      like ztpp_wosum2-wo_ser,
      nation      like ztpp_wosum2-nation,
      dealer      like ztpp_wosum2-dealer,
      intc        like ztpp_wosum2-intc,
      extc        like ztpp_wosum2-extc,
      ocn         like ztpp_wosum2-ocn,
      team        like ztpp_wosum2-team,
      version     like ztpp_wosum2-version,
      rp08mq      like ztpp_wosum2-rp08mq,
      rp06mq      like ztpp_wosum2-rp06mq,
      rp04mq      like ztpp_wosum2-rp04mq,
      rp02mq      like ztpp_wosum2-rp02mq,
      rp01mq      like ztpp_wosum2-rp01mq,
      rp08aq      like ztpp_wosum2-rp08aq,
      rp06aq      like ztpp_wosum2-rp06aq,
      rp04aq      like ztpp_wosum2-rp04aq,
      rp02aq      like ztpp_wosum2-rp02aq,
      rp01aq      like ztpp_wosum2-rp01aq,
      estprodqty  like ztpp_wosum2-estprodqty,
      rp01cq      like ztpp_wosum2-rp01cq,
      rp02cq      like ztpp_wosum2-rp02cq,
      rp03cq      like ztpp_wosum2-rp03cq,
      rp04cq      like ztpp_wosum2-rp04cq,
      rp05cq      like ztpp_wosum2-rp05cq,
      rp06cq      like ztpp_wosum2-rp06cq,
      rp07cq      like ztpp_wosum2-rp07cq,
*      PLANQTY     LIKE ZTPP_WOSUM-PLANQTY,
*      SEQQTY      LIKE ZTPP_WOSUM-SEQQTY,
*      MITUQTY     LIKE ZTPP_WOSUM-MITUQTY,
*      FORECASTQTY LIKE ZTPP_WOSUM-FORECASTQTY,
      remordqty like ztpp_wosum2-remordqty,
      end of it_wosum2.
data: it_wosum like ztpp_wosum occurs 0 with header line.
data: it_pmt06ea like ztpp_pmt06ea occurs 0 with header line.
data: wa_date like sy-datum.

************************************************************************
*              SELECTION SCREEN LAYOUT                                 *
************************************************************************
selection-screen: begin of block b1 with frame .
selection-screen begin of line.
parameters: p_run          type c as checkbox  default 'X'.
selection-screen comment  (55) text-100 for field p_run.
selection-screen end of line.
selection-screen: end of block b1.


************************************************************************
*              INITIALIZATION                                          *
************************************************************************
initialization.

************************************************************************
*              AT SELECTION SCREEN                                     *
************************************************************************
at selection-screen.

************************************************************************
*              START-OF-SELECTION PROCESSING                           *
************************************************************************
start-of-selection.
  check p_run = 'X'  .
  perform data_select.
  perform data_join.
  perform data_update.

end-of-selection.

************************************************************************
*              TOP-OF-PAGE                                             *
************************************************************************
top-of-page.





*&---------------------------------------------------------------------*
*&      Form  DATA_SELECT
*&---------------------------------------------------------------------*
form data_select.
  select single max( cr_date )
         from ztpp_wosum2
         into wa_date.

  select * from ztpp_wosum2
           into corresponding fields of table it_wosum2
           where cr_date = wa_date.
  sort it_wosum2.
  if it_wosum2[] is initial.
    message e001 with text-001.
  endif.

  select * from ztpp_wosum
           into table it_wosum.
  sort it_wosum.

endform.                    " DATA_SELECT
*&---------------------------------------------------------------------*
*&      Form  DATA_JOIN
*&---------------------------------------------------------------------*
form data_join.
  loop at it_wosum2.
    it_pmt06ea-stdt    = it_wosum2-cr_date(6).
    it_pmt06ea-plnt    = '1'.
    it_pmt06ea-line    = '1'.
    it_pmt06ea-modl    = it_wosum2-mi(3).
    it_pmt06ea-usee    = it_wosum2-wo_ser(1).
    it_pmt06ea-pack    = it_wosum2-wo_ser+1(4).
    it_pmt06ea-regn    = it_wosum2-wo_ser+5(1).
    it_pmt06ea-serl    = it_wosum2-wo_ser+6(3).
    concatenate it_wosum2-nation it_wosum2-dealer into it_pmt06ea-dist.
    it_pmt06ea-intc    = it_wosum2-intc.
    it_pmt06ea-extc    = it_wosum2-extc.
    it_pmt06ea-bmdl    = it_wosum2-mi.
    it_pmt06ea-ocnn    = it_wosum2-ocn.
    it_pmt06ea-vers    = it_wosum2-version.
    it_pmt06ea-team    = it_wosum2-team.
    it_pmt06ea-smdl    = ' '.
    it_pmt06ea-msof    = it_wosum2-rp08mq.
    it_pmt06ea-mtin    = it_wosum2-rp06mq.
    it_pmt06ea-mpot    = it_wosum2-rp04mq.
    it_pmt06ea-mpin    = it_wosum2-rp02mq.
    it_pmt06ea-mbin    = it_wosum2-rp01mq.
    it_pmt06ea-dsof    = it_wosum2-rp08aq.
    it_pmt06ea-dtin    = it_wosum2-rp06aq.
    it_pmt06ea-dpot    = it_wosum2-rp04aq.
    it_pmt06ea-dpin    = it_wosum2-rp02aq.
    it_pmt06ea-dbin    = it_wosum2-rp01aq.
    it_pmt06ea-expq    = it_wosum2-estprodqty.
    it_pmt06ea-bdyq    = it_wosum2-rp01cq.
    it_pmt06ea-pntq    = it_wosum2-rp02cq +
                         it_wosum2-rp03cq + it_wosum2-rp04cq +
                         it_wosum2-rp05cq.
    it_pmt06ea-trmq    = it_wosum2-rp06cq + it_wosum2-rp07cq.

    it_pmt06ea-remq    = it_wosum2-remordqty.
    it_pmt06ea-lcgu    = 'O'.
    it_pmt06ea-crtd    = sy-datum.
    read table it_wosum with key wo_ser = it_wosum2-wo_ser
                                 nation = it_wosum2-nation
                                 dealer = it_wosum2-dealer
                                 extc   = it_wosum2-extc
                                 intc   = it_wosum2-intc.
    if sy-subrc = 0.
      it_pmt06ea-conq    = it_wosum-planqty.
      it_pmt06ea-seqq    = it_wosum-seqqty.
      it_pmt06ea-mitq    = it_wosum-mituqty.
      it_pmt06ea-forq    = it_wosum-forecastqty.
    endif.
    append it_pmt06ea. clear it_pmt06ea.
  endloop.
endform.                    " DATA_JOIN
*&---------------------------------------------------------------------*
*&      Form  DATA_UPDATE
*&---------------------------------------------------------------------*
form data_update.
  data: l_text(60) type c,
        l_int type i.
  modify ztpp_pmt06ea from table it_pmt06ea.
  if sy-subrc = 0.
    commit work.
    describe table it_pmt06ea lines l_int.
    write l_int to l_text left-justified.
    concatenate 'Created Record Count :' l_text
      into l_text.
    message  s001 with l_text.
    message  s001 with text-002.
  else.
    rollback work.
    message  w001 with text-003.
  endif.
endform.                    " DATA_UPDATE
