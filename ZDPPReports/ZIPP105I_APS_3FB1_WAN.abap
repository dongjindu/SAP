REPORT  zipp105i_aps_3fb1 NO STANDARD PAGE HEADING
                          MESSAGE-ID zmpp.

* Program Name      : ZIPP105I_APS_3FB1_WAN
* Author            : Dongyeop, Han
* Creation Date     : 2003.11.13.
* Specifications By : Dongyeop, Han
* Pattern           : 1.1
* Development Request No : UD1K902031
* Addl Documentation:
* Description       : In-process Work Order
*
************************************************************************
* Modification Logs
* Date        Developer    RequestNo    Description
* 09/19/2005  Shiva        UD1K917652  Select condition chage to
*                                     reporting point '09' instead '13'.
* 01/17/2007  Haseeb Mohammad  UD1K930018 HDTicket # 71HB372592.
*                                        Performance enhancements.
* 08/20/2007  Performance Tuning by IG.MOON
************************************************************************
*              D A T A     A R E A                                     *
************************************************************************

TABLES:ztpp_wosum,
       ztpp_pmt03fb.

* UD1K941374 by IG.MOON 8/20/2007 {
*data: it_wosum   like ztpp_wosum occurs 0 with header line.
*DATA: it_wosum2   LIKE ztpp_wosum2 OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF it_wosum OCCURS 0.
DATA $wa_wocl LIKE equi-equnr.
        INCLUDE STRUCTURE ztpp_wosum.
DATA  wa_wocl LIKE equi-equnr.
DATA: END OF it_wosum.

DATA :
    BEGIN OF it_wosum2 OCCURS 0,
        wo_ser LIKE ztpp_wosum2-wo_ser,
        nation LIKE ztpp_wosum2-nation,
        dealer LIKE ztpp_wosum2-dealer,
        extc   LIKE ztpp_wosum2-extc,
        intc   LIKE ztpp_wosum2-intc,
        estprodqty LIKE ztpp_wosum2-estprodqty,
        rp08mq  LIKE ztpp_wosum2-rp08mq,
    END OF it_wosum2.

DATA :
    BEGIN OF l_ausp OCCURS 0,
        objek LIKE ausp-objek,
        atinn LIKE ausp-atinn,
        atwrt LIKE ausp-atwrt,
        atflv LIKE ausp-atflv,
        atfor LIKE cabn-atfor,
        atnam LIKE cabn-atnam,
    END OF l_ausp.

* }

DATA: it_pmt03fb LIKE ztpp_pmt03fb OCCURS 0 WITH HEADER LINE.
DATA: it_char   LIKE TABLE OF zspp_vin_value WITH HEADER LINE,
      it_head   LIKE TABLE OF zspp_vin_value WITH HEADER LINE.

DATA: wa_date LIKE sy-datum,
      wa_wocl LIKE equi-equnr,
      wa_perf_yn(30),
      wa_prod_flag(30).
************************************************************************
*              SELECTION SCREEN LAYOUT                                 *
************************************************************************
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME .
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_run          TYPE c AS CHECKBOX  DEFAULT 'X'.
SELECTION-SCREEN COMMENT  (55) text-100 FOR FIELD p_run.
SELECTION-SCREEN END OF LINE.
SELECT-OPTIONS: s_wo_ser FOR  ztpp_wosum-wo_ser.
SELECT-OPTIONS: s_datum FOR sy-datum.
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

* UD1K941374 by IG.MOON 8/20/2007 {
  IF '<run fast'>'!'.
    PERFORM data_join_new. " tuned
  ELSE.
    PERFORM data_join.     " old
  ENDIF.
* }

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
  SELECT a~wo_ser a~nation a~dealer a~extc a~intc a~initqty a~modqty
         a~seqqty a~planqty a~forecastqty a~mituqty a~wocredate
         a~womoddate a~fsc a~version a~sales a~rp01tq a~rp02tq a~rp03tq
         a~rp04tq a~rp05tq a~rp06tq a~rp07tq a~rp08tq a~rp09tq a~rp10tq
         a~rp11tq a~rp12tq a~rp13tq a~rp14tq a~rp15tq a~rp16tq a~rp01dq
         a~rp02dq a~rp03dq a~rp04dq a~rp05dq a~rp06dq a~rp07dq a~rp08dq
         a~rp09dq a~rp10dq a~rp11dq a~rp12dq a~rp13dq a~rp14dq a~rp15dq
         a~rp16dq a~t01pq  a~t06pq  a~t08pq  a~t12pq  a~t17pq  a~t20pq
         a~t01dq  a~t06dq  a~t08dq  a~t12dq  a~t17dq  a~t20dq
    INTO CORRESPONDING FIELDS OF TABLE it_wosum
    FROM ztpp_wosum AS a INNER JOIN ztpp_wosum AS b
      ON a~wo_ser = b~wo_ser
     AND a~nation = b~nation
     AND a~dealer = b~dealer
     AND a~extc   = b~extc
     AND a~intc   = b~intc
*   WHERE a~modqty > b~rp13tq
*where a~modqty > b~rp09tq
    WHERE a~wocredate IN s_datum
      AND a~wo_ser    IN s_wo_ser.   "Added on 02.11.2014 Victor
*     AND a~WO_SER LIKE 'E0312%' .

* UD1K941374 by IG.MOON 8/20/2007 {
*  sort it_wosum.
* }

  IF it_wosum[] IS INITIAL.
    MESSAGE w001 WITH text-001.
  ENDIF.

*  SELECT SINGLE MAX( CR_DATE )
*         FROM ZTPP_WOSUM2
*         INTO WA_DATE.


* UD1K941374 by IG.MOON 8/20/2007 {

*  SELECT * FROM ztpp_wosum2
*           INTO TABLE it_wosum2   .
**//        WHERE CR_DATE = WA_DATE.
*
  SELECT wo_ser nation dealer extc intc estprodqty rp08mq
  FROM ztpp_wosum2
  INTO TABLE it_wosum2.

* }

  SORT it_wosum2 BY wo_ser nation dealer extc intc .

ENDFORM.                    " DATA_SELECT
*&---------------------------------------------------------------------*
*&      Form  DATA_JOIN
*&---------------------------------------------------------------------*
FORM data_join.
  LOOP AT it_wosum.
    CLEAR: it_char[], it_head[], wa_perf_yn.
    CONCATENATE it_wosum-wo_ser it_wosum-nation it_wosum-dealer
                it_wosum-extc it_wosum-intc INTO wa_wocl.
* This fucntion call is made but the data is not used anywhere in the
*program, so commenting this line is not effecting the output data.
*    PERFORM ftp_handling_master TABLES it_char   "Manju & Haseeb
*                                 USING wa_wocl. "Manju & Haseeb
    it_pmt03fb-plnt = '1'.
** Changed by Furong on 10/09/07 for EBOM
    IF it_wosum-fsc+13(1) = space.
      it_pmt03fb-modl = it_wosum-fsc+6(3).
    ELSE.
      it_pmt03fb-modl = it_wosum-fsc+5(3).
    ENDIF.
** End of change
    it_pmt03fb-usee = it_wosum-wo_ser(1).
    it_pmt03fb-pack = it_wosum-wo_ser+1(4).
    it_pmt03fb-regn = it_wosum-wo_ser+5(1).
    it_pmt03fb-serl = it_wosum-wo_ser+6(3).
    CONCATENATE it_wosum-nation it_wosum-dealer
                INTO it_pmt03fb-dist.
    it_pmt03fb-team = ' '.
    it_pmt03fb-extc = it_wosum-extc.
    it_pmt03fb-intc = it_wosum-intc.
** Changed by Furong on 10/09/07 for EBOM
    IF it_wosum-fsc+13(1) = space.
      it_pmt03fb-bmdl = it_wosum-fsc+6(8).
    ELSE.
      it_pmt03fb-bmdl = it_wosum-fsc+5(9).
    ENDIF.
* End of change
    it_pmt03fb-ocnn = it_wosum-fsc+14(4).
    it_pmt03fb-vers = it_wosum-version.
    PERFORM ftp_handling_master TABLES it_head
                                 USING wa_wocl(14).
    SORT it_head BY     atnam. "Manju & haseeb
    PERFORM read_wohd         USING 'P_DESTINATION_CODE'
                           CHANGING it_pmt03fb-dest.
    PERFORM read_wohd         USING 'P_LC_NO'
                           CHANGING it_pmt03fb-lcno.
*    PERFORM read_wohd         USING 'P_PERF_YN'
*                             CHANGING wa_perf_yn.
    PERFORM read_wohd         USING 'P_PROD_FLAG'
                             CHANGING  wa_prod_flag.

*    IF wa_perf_yn = 'Y'.
*      it_pmt03fb-cmpt = 'O'.
*    ELSE.
*      it_pmt03fb-cmpt = 'X'.
*    ENDIF.
    IF wa_prod_flag = 'Y'.
      it_pmt03fb-cmpt = 'O'.
    ELSE.
      it_pmt03fb-cmpt = 'X'.
    ENDIF.

*    IT_PMT03FB-EXPQ = IT_WOSUM2-ESTPRODQTY
    it_pmt03fb-ordq = it_wosum-modqty.
    it_pmt03fb-conq = it_wosum-planqty.
    it_pmt03fb-forq = it_wosum-forecastqty.
    it_pmt03fb-mitq = it_wosum-mituqty.
    it_pmt03fb-pt00 = it_wosum-seqqty.
    it_pmt03fb-pt01 = it_wosum-rp01tq.
    it_pmt03fb-pt02 = it_wosum-rp02tq.
    it_pmt03fb-pt03 = it_wosum-rp03tq.
    it_pmt03fb-pt04 = it_wosum-rp04tq.
    it_pmt03fb-pt05 = it_wosum-rp05tq.
    it_pmt03fb-pt06 = it_wosum-rp06tq.
    it_pmt03fb-pt07 = it_wosum-rp07tq.
    it_pmt03fb-pt08 = it_wosum-rp08tq.
    it_pmt03fb-pt09 = it_wosum-rp09tq.
    it_pmt03fb-pt10 = it_wosum-rp10tq + it_wosum-rp11tq +
                      it_wosum-rp12tq.
    it_pmt03fb-pt11 = it_wosum-rp13tq.
*    IT_PMT03FB-PRDQ = IT_WOSUM2-RP08MQ.
*      IT_PMT03FB-LCGU =
**>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
* Start : Added By Tonkey on 03/31/2004
**>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    PERFORM read_wohd         USING 'P_LC_COUNT'
                             CHANGING it_pmt03fb-lcgu.
    PERFORM read_wohd         USING 'P_WO_MODI_DATE'
                           CHANGING it_pmt03fb-sttm.
    PERFORM read_wohd         USING 'P_WO_CREATE_DATE'
                           CHANGING it_pmt03fb-crtd.
**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
* End   : Added By Tonkey on 03/31/2004
**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

*    it_pmt03fb-sttm = sy-datum.
    it_pmt03fb-cuser = sy-uname(7).
*    it_pmt03fb-crtd = sy-datum.
    READ TABLE it_wosum2 WITH KEY wo_ser = it_wosum-wo_ser
                                  nation = it_wosum-nation
                                  dealer = it_wosum-dealer
                                  extc   = it_wosum-extc
                                  intc   = it_wosum-intc
                                  BINARY SEARCH.  "Manju & Haseeb
    IF sy-subrc = 0.
      it_pmt03fb-expq = it_wosum2-estprodqty.
      it_pmt03fb-prdq = it_wosum2-rp08mq.
    ENDIF.
    APPEND it_pmt03fb.  CLEAR it_pmt03fb.
*    ENDIF.
  ENDLOOP.
ENDFORM.                    " DATA_JOIN

*&---------------------------------------------------------------------*
*&      Form  DATA_UPDATE
*&---------------------------------------------------------------------*
FORM data_update.
  DATA: l_text(60) TYPE c,
        l_int TYPE i.
  DELETE FROM ztpp_pmt03fb CLIENT SPECIFIED WHERE mandt = sy-mandt.

*  MODIFY ztpp_pmt03fb FROM TABLE it_pmt03fb. "Manju
  INSERT ztpp_pmt03fb FROM TABLE it_pmt03fb. "Haseeb

  IF sy-subrc = 0.
    DESCRIBE TABLE it_pmt03fb LINES l_int.
    WRITE l_int TO l_text LEFT-JUSTIFIED.
    COMMIT WORK.
    CONCATENATE 'Created Record count :' l_text
      INTO l_text.
    MESSAGE  s001 WITH l_text.
    MESSAGE  s001 WITH text-002.
  ELSE.
    ROLLBACK WORK.
    MESSAGE  w001 WITH text-003.
  ENDIF.
ENDFORM.                    " DATA_UPDATE

*&---------------------------------------------------------------------*
*&      Form  FTP_HANDLING_MASTER
*&---------------------------------------------------------------------*
FORM ftp_handling_master TABLES   pa_char
                         USING    p_wocl.
  DATA: l_wocl             LIKE mara-matnr.
  l_wocl = p_wocl.
  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'  "haseeb
         EXPORTING
             object       = l_wocl
             mode         = 'R'
             ctype        = '001'
             display      = 'X'
        TABLES
             val_table    = pa_char
        EXCEPTIONS
             no_data      = 1
             error_mode   = 2
             error_object = 3
             OTHERS       = 4.
ENDFORM.                    " FTP_HANDLING_MASTER

*&---------------------------------------------------------------------*
*&      Form  READ_WOHD
*&---------------------------------------------------------------------*
FORM read_wohd USING
                   p_atnam
            CHANGING p_atwrt.
  READ TABLE it_head WITH KEY atnam = p_atnam BINARY SEARCH.
  IF sy-subrc EQ 0.
    p_atwrt = it_head-atwrt.
  ENDIF.
ENDFORM.                    " READ_WOHD

*&---------------------------------------------------------------------*
*&      Form  READ_WOCL
*&---------------------------------------------------------------------*
FORM read_wocl USING    p_atnam
            CHANGING p_atwrt.
  READ TABLE l_ausp WITH KEY atnam = p_atnam.
  IF sy-subrc EQ 0.
    p_atwrt = l_ausp-atwrt.
  ENDIF.
ENDFORM.                    " READ_WOCL
*&---------------------------------------------------------------------*
*&      Form  data_join_new
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_join_new.
  DATA : $ix LIKE sy-tabix,
         $flag(1).

  DATA : $dest LIKE it_pmt03fb-dest,
         $lcno LIKE it_pmt03fb-lcno,
         $cmpt LIKE it_pmt03fb-cmpt,
         $lcgu LIKE it_pmt03fb-lcgu,
         $sttm LIKE it_pmt03fb-sttm,
         $crtd LIKE it_pmt03fb-crtd.

  DATA: BEGIN OF l_object OCCURS 0,
          objek LIKE ausp-objek,
        END OF l_object.

  LOOP AT it_wosum.
    $ix = sy-tabix.
    CONCATENATE it_wosum-wo_ser it_wosum-nation it_wosum-dealer
                it_wosum-extc it_wosum-intc INTO it_wosum-wa_wocl.
    l_object-objek = it_wosum-$wa_wocl = it_wosum-wa_wocl(14).
    MODIFY it_wosum INDEX $ix TRANSPORTING $wa_wocl wa_wocl.
    APPEND l_object.
  ENDLOOP.

  SORT l_object.
  DELETE ADJACENT DUPLICATES FROM l_object.

  REFRESH : l_ausp.
  CLEAR : l_ausp.

  SELECT a~objek a~atinn a~atwrt a~atflv b~atfor b~atnam
  INTO CORRESPONDING FIELDS OF TABLE l_ausp
    FROM ausp AS a
    JOIN cabn AS b
   ON b~atinn EQ a~atinn
   FOR ALL ENTRIES IN l_object
   WHERE a~objek = l_object-objek
     AND a~klart = '001'
    %_HINTS ORACLE 'FIRST_ROWS(10)'.


  DATA: l_vals(8)           TYPE n            ,
        l_time(6)           TYPE n            .

  LOOP AT l_ausp WHERE atfor EQ 'NUM' OR
                       atfor EQ 'DATE' OR
                       atfor EQ 'TIME'.
    CASE l_ausp-atfor .
      WHEN 'NUM' .
        l_ausp-atwrt = l_ausp-atflv.
      WHEN 'DATE'.
        l_ausp-atwrt = l_vals  = l_ausp-atflv.
      WHEN 'TIME'.
        l_ausp-atwrt = l_time = l_ausp-atflv .
    ENDCASE.
    MODIFY l_ausp TRANSPORTING atwrt.
  ENDLOOP.

  SORT l_ausp BY objek atnam.

  LOOP AT it_wosum.

    AT NEW $wa_wocl.
      $flag = 'X'.
    ENDAT.

    IF $flag EQ 'X'.
      CLEAR: $dest,
             $lcno,
             $cmpt,
             $lcgu,
             $sttm,
             $crtd.

      PERFORM read_wohd_new         USING 'P_DESTINATION_CODE'
                                           it_wosum-$wa_wocl
                                  CHANGING $dest.
      PERFORM read_wohd_new         USING 'P_LC_NO'
                                          it_wosum-$wa_wocl
                                  CHANGING $lcno.
      PERFORM read_wohd_new         USING 'P_PROD_FLAG'
                                           it_wosum-$wa_wocl
                               CHANGING  wa_prod_flag.

      IF wa_prod_flag = 'Y'.
        $cmpt = 'O'.
      ELSE.
        $cmpt = 'X'.
      ENDIF.

      PERFORM read_wohd_new         USING 'P_LC_COUNT'
                                       it_wosum-$wa_wocl
                               CHANGING $lcgu.
      PERFORM read_wohd_new         USING 'P_WO_MODI_DATE'
                                       it_wosum-$wa_wocl
                             CHANGING $sttm.
      PERFORM read_wohd_new         USING 'P_WO_CREATE_DATE'
                                       it_wosum-$wa_wocl
                             CHANGING $crtd.
      CLEAR $flag.
    ENDIF.

    it_pmt03fb-dest = $dest.
    it_pmt03fb-lcno = $lcno.
    it_pmt03fb-cmpt = $cmpt.
    it_pmt03fb-lcgu = $lcgu.
    it_pmt03fb-sttm = $sttm.
    it_pmt03fb-crtd = $crtd.

    it_pmt03fb-plnt = '1'.
** Changed by Furong on 10/09/07 for EBOM
    IF it_wosum-fsc+13(1) = space.
      it_pmt03fb-modl = it_wosum-fsc+6(3).
    ELSE.
      it_pmt03fb-modl = it_wosum-fsc+5(3).
    ENDIF.
** End of change
    it_pmt03fb-usee = it_wosum-wo_ser(1).
    it_pmt03fb-pack = it_wosum-wo_ser+1(4).
    it_pmt03fb-regn = it_wosum-wo_ser+5(1).
    it_pmt03fb-serl = it_wosum-wo_ser+6(3).
    CONCATENATE it_wosum-nation it_wosum-dealer
                INTO it_pmt03fb-dist.
    it_pmt03fb-team = ' '.
    it_pmt03fb-extc = it_wosum-extc.
    it_pmt03fb-intc = it_wosum-intc.
** Changed by Furong on 10/09/07 for EBOM
    IF it_wosum-fsc+13(1) = space.
      it_pmt03fb-bmdl = it_wosum-fsc+6(8).
    ELSE.
      it_pmt03fb-bmdl = it_wosum-fsc+5(9).
    ENDIF.
* End of change
    it_pmt03fb-ocnn = it_wosum-fsc+14(4).
    it_pmt03fb-vers = it_wosum-version.
    it_pmt03fb-ordq = it_wosum-modqty.
    it_pmt03fb-conq = it_wosum-planqty.
    it_pmt03fb-forq = it_wosum-forecastqty.
    it_pmt03fb-mitq = it_wosum-mituqty.
    it_pmt03fb-pt00 = it_wosum-seqqty.
    it_pmt03fb-pt01 = it_wosum-rp01tq.
    it_pmt03fb-pt02 = it_wosum-rp02tq.
    it_pmt03fb-pt03 = it_wosum-rp03tq.
    it_pmt03fb-pt04 = it_wosum-rp04tq.
    it_pmt03fb-pt05 = it_wosum-rp05tq.
    it_pmt03fb-pt06 = it_wosum-rp06tq.
    it_pmt03fb-pt07 = it_wosum-rp07tq.
    it_pmt03fb-pt08 = it_wosum-rp08tq.
    it_pmt03fb-pt09 = it_wosum-rp09tq.
    it_pmt03fb-pt10 = it_wosum-rp10tq + it_wosum-rp11tq +
                      it_wosum-rp12tq.
    it_pmt03fb-pt11 = it_wosum-rp13tq.
    it_pmt03fb-cuser = sy-uname(7).

    READ TABLE it_wosum2 WITH KEY wo_ser = it_wosum-wo_ser
                                  nation = it_wosum-nation
                                  dealer = it_wosum-dealer
                                  extc   = it_wosum-extc
                                  intc   = it_wosum-intc
                                  BINARY SEARCH.
    IF sy-subrc = 0.
      it_pmt03fb-expq = it_wosum2-estprodqty.
      it_pmt03fb-prdq = it_wosum2-rp08mq.
    ENDIF.
    APPEND it_pmt03fb.  CLEAR it_pmt03fb.

  ENDLOOP.

ENDFORM.                    " data_join_new

*---------------------------------------------------------------------*
*       FORM read_wohd                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_WA_WOCL                                                     *
*  -->  P_ATNAM                                                       *
*  -->  P_ATWRT                                                       *
*---------------------------------------------------------------------*
FORM read_wohd_new USING
                   p_atnam
                   p_wa_wocl
            CHANGING p_atwrt.
  READ TABLE l_ausp WITH KEY   objek = p_wa_wocl
                                atnam = p_atnam BINARY SEARCH.
  IF sy-subrc EQ 0.
    p_atwrt = l_ausp-atwrt.
  ENDIF.
ENDFORM.                    " READ_WOHD
