************************************************************************
* Program Name      : ZIPP103I_APS_1AA1
* Author            : Dongyeop, Han
* Creation Date     : 2003.11.12.
* Specifications By : Dongyeop, Han
* Pattern           : 1.1
* Development Request No : UD1K902031
* Addl Documentation:
* Description       : Monthly Actual 1(1AA)
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 25/08/2005 Furong Wang               get sales-saleqt
************************************************************************
REPORT  zipp103i_aps_1aa1 NO STANDARD PAGE HEADING
                          MESSAGE-ID zmpp.

************************************************************************
*              D A T A     A R E A                                     *
************************************************************************
TABLES:ztpp_wosum2,
       ztpp_pmt01aa.
DATA: BEGIN OF it_wosum2 OCCURS 0,
      cr_month  LIKE ztpp_wosum2-cr_month,
      cr_date   LIKE ztpp_wosum2-cr_date,
      mi        LIKE ztpp_wosum2-mi,
      wo_ser    LIKE ztpp_wosum2-wo_ser,
      nation    LIKE ztpp_wosum2-nation,
      dealer    LIKE ztpp_wosum2-dealer,
      intc      LIKE ztpp_wosum2-intc,
      extc      LIKE ztpp_wosum2-extc,
*      MONT      LIKE ZTPP_WOSUM2-MONT,
      ocn       LIKE ztpp_wosum2-ocn,
      version   LIKE ztpp_wosum2-version,
      region    LIKE ztpp_wosum2-region,
      team      LIKE ztpp_wosum2-team,
* CHANGED BY FURONG ON 05/01/07
*      wholesale LIKE ztpp_wosum2-wholesale,
* END OF CHANGE
      rp08mq    LIKE ztpp_wosum2-rp08mq,
      rp09mq    LIKE ztpp_wosum2-rp09mq,
      rp15mq    LIKE ztpp_wosum2-rp15mq,
      stock     LIKE ztpp_wosum2-stock,
      orderqty  LIKE ztpp_wosum2-orderqty,
      remordqty LIKE ztpp_wosum2-remordqty,
      END OF it_wosum2.

DATA: it_pmt01aa LIKE ztpp_pmt01aa OCCURS 0 WITH HEADER LINE.

DATA: wa_date LIKE sy-datum,
      wa_mont LIKE ztpp_pmt01aa-mont.

************************************************************************
*              SELECTION SCREEN LAYOUT                                 *
************************************************************************
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME .
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_run          TYPE c AS CHECKBOX  DEFAULT 'X'.
SELECTION-SCREEN COMMENT  (55) text-001 FOR FIELD p_run.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK b1.

************************************************************************
*              INITIALIZATION                                          *
************************************************************************
INITIALIZATION.

************************************************************************
*              AT SELECTION SCREEN                                     *
************************************************************************
AT SELECTION-SCREEN.

************************************************************************
*              START-OF-SELECTION PROCESSING                           *
************************************************************************
START-OF-SELECTION.
  CHECK p_run = 'X'  .
  PERFORM data_select.
  PERFORM data_join.
  PERFORM data_update.

END-OF-SELECTION.

************************************************************************
*              TOP-OF-PAGE                                             *
************************************************************************
TOP-OF-PAGE.





*&---------------------------------------------------------------------*
*&      Form  DATA_SELECT
*&---------------------------------------------------------------------*
FORM data_select.
  SELECT SINGLE MAX( cr_date )
         FROM ztpp_wosum2
         INTO wa_date .
*        where CR_MONTH = '200312'
*          and CR_DATE  = '20031209'
*          and WO_SER  like 'E0312%' .

  SELECT cr_month cr_date mi wo_ser nation dealer intc extc ocn version
         region team
*         RP15MQ AS wholesale
         rp08mq rp09mq rp15mq stock orderqty remordqty
         FROM ztpp_wosum2
         INTO TABLE it_wosum2
         WHERE cr_date = wa_date .
*          AND perf    = 'Y'    .

  SORT it_wosum2.
  IF it_wosum2[] IS INITIAL.
    MESSAGE e001 WITH text-003.
  ELSE.
    DELETE FROM ztpp_pmt01aa WHERE mont = wa_date(6) .
*          client specified where mandt = sy-mandt.
  ENDIF.
ENDFORM.                    " DATA_SELECT
*&---------------------------------------------------------------------*
*&      Form  DATA_JOIN
*&---------------------------------------------------------------------*
FORM data_join.
  LOOP AT it_wosum2.
    it_pmt01aa-mont    = it_wosum2-cr_month.
    it_pmt01aa-modl    = it_wosum2-mi(3).
    it_pmt01aa-usee    = it_wosum2-wo_ser(1).
    it_pmt01aa-pack    = it_wosum2-wo_ser+1(4).
    it_pmt01aa-regn    = it_wosum2-wo_ser+5(1).
    it_pmt01aa-serl    = it_wosum2-wo_ser+6(3).
    CONCATENATE it_wosum2-nation it_wosum2-dealer INTO it_pmt01aa-dist.
    it_pmt01aa-intc    = it_wosum2-intc.
    it_pmt01aa-extc    = it_wosum2-extc.
    it_pmt01aa-bmdl    = it_wosum2-mi.
    it_pmt01aa-ocnn    = it_wosum2-ocn.
    it_pmt01aa-vers    = it_wosum2-version.
    it_pmt01aa-reg2    = it_wosum2-region.
    it_pmt01aa-team    = it_wosum2-team.
    it_pmt01aa-sspc    = ' '.
    IF it_wosum2-nation = 'B28'.
    it_pmt01aa-saleqt  = it_wosum2-rp09mq.
    ELSE.
    it_pmt01aa-saleqt  = it_wosum2-rp15mq.
    ENDIF.
    it_pmt01aa-prodqt  = it_wosum2-rp08mq.
    it_pmt01aa-estcqt  = it_wosum2-stock.
    it_pmt01aa-cntrqt  = it_wosum2-orderqty.
    it_pmt01aa-oremqt  = it_wosum2-remordqty.
    it_pmt01aa-andat   = sy-datum.
    it_pmt01aa-zuser   = sy-uname.
    CONCATENATE it_wosum2-cr_date it_wosum2-wo_ser it_wosum2-nation
                it_wosum2-dealer  it_wosum2-extc   it_wosum2-intc
           INTO it_pmt01aa-zmsg   SEPARATED BY space.
    APPEND it_pmt01aa. CLEAR it_pmt01aa.
  ENDLOOP.
ENDFORM.                    " DATA_JOIN

*&---------------------------------------------------------------------*
*&      Form  DATA_UPDATE
*&---------------------------------------------------------------------*
FORM data_update.
  DATA: l_text(60) TYPE c ,
        l_int TYPE i.

  DELETE FROM ztpp_pmt01aa WHERE mont = wa_date(6).

  MODIFY ztpp_pmt01aa FROM TABLE it_pmt01aa.
  IF sy-subrc = 0.
    DESCRIBE TABLE it_pmt01aa LINES l_int .
    WRITE l_int TO l_text LEFT-JUSTIFIED .
    CONCATENATE 'Created Record count :' l_text
      INTO l_text.
    MESSAGE s001 WITH l_text .
    COMMIT WORK.
    MESSAGE  s001 WITH text-101.
  ELSE.
    ROLLBACK WORK.
    MESSAGE  e001 WITH text-102.
  ENDIF.
ENDFORM.                    " DATA_UPDATE
