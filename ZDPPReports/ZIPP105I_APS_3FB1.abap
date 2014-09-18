REPORT  ZIPP105I_APS_3FB1 NO STANDARD PAGE HEADING
                          MESSAGE-ID ZMPP.
************************************************************************
* Program Name      : ZIPP105I_APS_3FB1
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
* 04/09/2008  Furong       No data generated if wa_prod_flag = 'N' for
*                          work order header
************************************************************************
*              D A T A     A R E A                                     *
************************************************************************
TABLES:ZTPP_WOSUM,
       ZTPP_PMT03FB.

* UD1K941374 by IG.MOON 8/20/2007 {
*data: it_wosum   like ztpp_wosum occurs 0 with header line.
*DATA: it_wosum2   LIKE ztpp_wosum2 OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF IT_WOSUM OCCURS 0.
DATA $WA_WOCL LIKE EQUI-EQUNR.
        INCLUDE STRUCTURE ZTPP_WOSUM.
DATA  WA_WOCL LIKE EQUI-EQUNR.
DATA: END OF IT_WOSUM.

DATA :
    BEGIN OF IT_WOSUM2 OCCURS 0,
        WO_SER LIKE ZTPP_WOSUM2-WO_SER,
        NATION LIKE ZTPP_WOSUM2-NATION,
        DEALER LIKE ZTPP_WOSUM2-DEALER,
        EXTC   LIKE ZTPP_WOSUM2-EXTC,
        INTC   LIKE ZTPP_WOSUM2-INTC,
        ESTPRODQTY LIKE ZTPP_WOSUM2-ESTPRODQTY,
        RP08MQ  LIKE ZTPP_WOSUM2-RP08MQ,
    END OF IT_WOSUM2.

DATA :
    BEGIN OF L_AUSP OCCURS 0,
        OBJEK LIKE AUSP-OBJEK,
        ATINN LIKE AUSP-ATINN,
        ATWRT LIKE AUSP-ATWRT,
        ATFLV LIKE AUSP-ATFLV,
        ATFOR LIKE CABN-ATFOR,
        ATNAM LIKE CABN-ATNAM,
    END OF L_AUSP.

* }

DATA: IT_PMT03FB LIKE ZTPP_PMT03FB OCCURS 0 WITH HEADER LINE.
DATA: IT_CHAR   LIKE TABLE OF ZSPP_VIN_VALUE WITH HEADER LINE,
      IT_HEAD   LIKE TABLE OF ZSPP_VIN_VALUE WITH HEADER LINE.

DATA: WA_DATE LIKE SY-DATUM,
      WA_WOCL LIKE EQUI-EQUNR,
      WA_PERF_YN(30),
      WA_PROD_FLAG(30).
************************************************************************
*              SELECTION SCREEN LAYOUT                                 *
************************************************************************
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME .
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: P_RUN          TYPE C AS CHECKBOX  DEFAULT 'X'.
SELECTION-SCREEN COMMENT  (55) TEXT-100 FOR FIELD P_RUN.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK B1.


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
  CHECK P_RUN = 'X'  .
  PERFORM DATA_SELECT.

* UD1K941374 by IG.MOON 8/20/2007 {
  IF '<run fast'>'!'.
    PERFORM DATA_JOIN_NEW. " tuned
  ELSE.
    PERFORM DATA_JOIN.     " old
  ENDIF.
* }

  PERFORM DATA_UPDATE.

END-OF-SELECTION.

************************************************************************
*              TOP-OF-PAGE                                             *
************************************************************************
TOP-OF-PAGE.


*&---------------------------------------------------------------------*
*&      Form  DATA_SELECT
*&---------------------------------------------------------------------*
FORM DATA_SELECT.

** Changed by Furong on 03/18/08
*  select a~wo_ser a~nation a~dealer a~extc a~intc a~initqty a~modqty
*         a~seqqty a~planqty a~forecastqty a~mituqty a~wocredate
*         a~womoddate a~fsc a~version a~sales a~rp01tq a~rp02tq a~rp03tq
*         a~rp04tq a~rp05tq a~rp06tq a~rp07tq a~rp08tq a~rp09tq a~rp10tq
*         a~rp11tq a~rp12tq a~rp13tq a~rp14tq a~rp15tq a~rp16tq a~rp01dq
*         a~rp02dq a~rp03dq a~rp04dq a~rp05dq a~rp06dq a~rp07dq a~rp08dq
*         a~rp09dq a~rp10dq a~rp11dq a~rp12dq a~rp13dq a~rp14dq a~rp15dq
*         a~rp16dq a~t01pq  a~t06pq  a~t08pq  a~t12pq  a~t17pq  a~t20pq
*         a~t01dq  a~t06dq  a~t08dq  a~t12dq  a~t17dq  a~t20dq
*    into corresponding fields of table it_wosum
*    from ztpp_wosum as a inner join ztpp_wosum as b
*      on a~wo_ser = b~wo_ser
*     and a~nation = b~nation
*     and a~dealer = b~dealer
*     and a~extc   = b~extc
*     and a~intc   = b~intc
**   WHERE a~modqty > b~rp13tq    .
*    where a~modqty > b~rp09tq    .
**     AND a~WO_SER LIKE 'E0312%' .

  SELECT A~WO_SER A~NATION A~DEALER A~EXTC A~INTC A~INITQTY A~MODQTY
         A~SEQQTY A~PLANQTY A~FORECASTQTY A~MITUQTY A~WOCREDATE
         A~WOMODDATE A~FSC A~VERSION A~SALES A~RP01TQ A~RP02TQ A~RP03TQ
         A~RP04TQ A~RP05TQ A~RP06TQ A~RP07TQ A~RP08TQ A~RP09TQ A~RP10TQ
         A~RP11TQ A~RP12TQ A~RP13TQ A~RP14TQ A~RP15TQ A~RP16TQ A~RP01DQ
         A~RP02DQ A~RP03DQ A~RP04DQ A~RP05DQ A~RP06DQ A~RP07DQ A~RP08DQ
         A~RP09DQ A~RP10DQ A~RP11DQ A~RP12DQ A~RP13DQ A~RP14DQ A~RP15DQ
         A~RP16DQ A~T01PQ  A~T06PQ  A~T08PQ  A~T12PQ  A~T17PQ  A~T20PQ
         A~T01DQ  A~T06DQ  A~T08DQ  A~T12DQ  A~T17DQ  A~T20DQ
    INTO CORRESPONDING FIELDS OF TABLE IT_WOSUM
    FROM ZTPP_WOSUM AS A INNER JOIN ZTPP_WOSUM AS B
      ON A~WO_SER = B~WO_SER
     AND A~NATION = B~NATION
     AND A~DEALER = B~DEALER
     AND A~EXTC   = B~EXTC
     AND A~INTC   = B~INTC
     WHERE ( A~MODQTY <> B~SEQQTY OR
             A~PLANQTY <> 0 OR
             A~FORECASTQTY <> 0 ).
** End of change

* UD1K941374 by IG.MOON 8/20/2007 {
*  sort it_wosum.
* }

  IF IT_WOSUM[] IS INITIAL.
    MESSAGE W001 WITH TEXT-001.
  ENDIF.

*  SELECT SINGLE MAX( CR_DATE )
*         FROM ZTPP_WOSUM2
*         INTO WA_DATE.


* UD1K941374 by IG.MOON 8/20/2007 {

*  SELECT * FROM ztpp_wosum2
*           INTO TABLE it_wosum2   .
**//        WHERE CR_DATE = WA_DATE.
*
  SELECT WO_SER NATION DEALER EXTC INTC ESTPRODQTY RP08MQ
  FROM ZTPP_WOSUM2
  INTO TABLE IT_WOSUM2.

* }

  SORT IT_WOSUM2 BY WO_SER NATION DEALER EXTC INTC .

ENDFORM.                    " DATA_SELECT
*&---------------------------------------------------------------------*
*&      Form  DATA_JOIN
*&---------------------------------------------------------------------*
FORM DATA_JOIN.
  LOOP AT IT_WOSUM.
    CLEAR: IT_CHAR[], IT_HEAD[], WA_PERF_YN.
    CONCATENATE IT_WOSUM-WO_SER IT_WOSUM-NATION IT_WOSUM-DEALER
                IT_WOSUM-EXTC IT_WOSUM-INTC INTO WA_WOCL.
* This fucntion call is made but the data is not used anywhere in the
*program, so commenting this line is not effecting the output data.
*    PERFORM ftp_handling_master TABLES it_char   "Manju & Haseeb
*                                 USING wa_wocl. "Manju & Haseeb
    IT_PMT03FB-PLNT = '1'.
** Changed by Furong on 10/09/07 for EBOM
    IF IT_WOSUM-FSC+13(1) = SPACE.
      IT_PMT03FB-MODL = IT_WOSUM-FSC+6(3).
    ELSE.
      IT_PMT03FB-MODL = IT_WOSUM-FSC+5(3).
    ENDIF.
** End of change
    IT_PMT03FB-USEE = IT_WOSUM-WO_SER(1).
    IT_PMT03FB-PACK = IT_WOSUM-WO_SER+1(4).
    IT_PMT03FB-REGN = IT_WOSUM-WO_SER+5(1).
    IT_PMT03FB-SERL = IT_WOSUM-WO_SER+6(3).
    CONCATENATE IT_WOSUM-NATION IT_WOSUM-DEALER
                INTO IT_PMT03FB-DIST.
    IT_PMT03FB-TEAM = ' '.
    IT_PMT03FB-EXTC = IT_WOSUM-EXTC.
    IT_PMT03FB-INTC = IT_WOSUM-INTC.
** Changed by Furong on 10/09/07 for EBOM
    IF IT_WOSUM-FSC+13(1) = SPACE.
      IT_PMT03FB-BMDL = IT_WOSUM-FSC+6(8).
    ELSE.
      IT_PMT03FB-BMDL = IT_WOSUM-FSC+5(9).
    ENDIF.
* End of change
    IT_PMT03FB-OCNN = IT_WOSUM-FSC+14(4).
    IT_PMT03FB-VERS = IT_WOSUM-VERSION.
    PERFORM FTP_HANDLING_MASTER TABLES IT_HEAD
                                 USING WA_WOCL(14).
    SORT IT_HEAD BY     ATNAM. "Manju & haseeb
    PERFORM READ_WOHD         USING 'P_DESTINATION_CODE'
                           CHANGING IT_PMT03FB-DEST.
    PERFORM READ_WOHD         USING 'P_LC_NO'
                           CHANGING IT_PMT03FB-LCNO.
*    PERFORM read_wohd         USING 'P_PERF_YN'
*                             CHANGING wa_perf_yn.
    PERFORM READ_WOHD         USING 'P_PROD_FLAG'
                             CHANGING  WA_PROD_FLAG.

*    IF wa_perf_yn = 'Y'.
*      it_pmt03fb-cmpt = 'O'.
*    ELSE.
*      it_pmt03fb-cmpt = 'X'.
*    ENDIF.

** Changed by Furong on 04/09/08
    IF WA_PROD_FLAG = 'Y'.
      IT_PMT03FB-CMPT = 'O'.
    ELSE.
      IT_PMT03FB-CMPT = 'X'.
    ENDIF.
    IF WA_PROD_FLAG = 'N'.
      CLEAR: IT_PMT03FB.
      CONTINUE.
    ENDIF.
** End of change on 04/09/08

*    IT_PMT03FB-EXPQ = IT_WOSUM2-ESTPRODQTY
    IT_PMT03FB-ORDQ = IT_WOSUM-MODQTY.
    IT_PMT03FB-CONQ = IT_WOSUM-PLANQTY.
    IT_PMT03FB-FORQ = IT_WOSUM-FORECASTQTY.
    IT_PMT03FB-MITQ = IT_WOSUM-MITUQTY.
    IT_PMT03FB-PT00 = IT_WOSUM-SEQQTY.
    IT_PMT03FB-PT01 = IT_WOSUM-RP01TQ.
    IT_PMT03FB-PT02 = IT_WOSUM-RP02TQ.
    IT_PMT03FB-PT03 = IT_WOSUM-RP03TQ.
    IT_PMT03FB-PT04 = IT_WOSUM-RP04TQ.
    IT_PMT03FB-PT05 = IT_WOSUM-RP05TQ.
    IT_PMT03FB-PT06 = IT_WOSUM-RP06TQ.
    IT_PMT03FB-PT07 = IT_WOSUM-RP07TQ.
    IT_PMT03FB-PT08 = IT_WOSUM-RP08TQ.
    IT_PMT03FB-PT09 = IT_WOSUM-RP09TQ.
    IT_PMT03FB-PT10 = IT_WOSUM-RP10TQ + IT_WOSUM-RP11TQ +
                      IT_WOSUM-RP12TQ.
    IT_PMT03FB-PT11 = IT_WOSUM-RP13TQ.

*--<Victor 07.07.2011

*-  Body + Dealer + '' or F
    SELECT COUNT( * ) INTO it_pmt03fb-dealer_bqty
    FROM ztsd_um
     WHERE wo_serial  = it_wosum-wo_ser
       AND wo_nation  = it_wosum-nation
       AND wo_dealer  = it_wosum-dealer
       AND wo_extc    = it_wosum-extc
       AND wo_intc    = it_wosum-intc
       AND body_no    <> ''
       AND ( status = '' OR status = 'F' )
       AND wo_dealer1 <> ''.

*-  No Body + Dealer + ''
    SELECT COUNT( * ) INTO it_pmt03fb-dealer_nqty
    FROM ztsd_um
     WHERE wo_serial  = it_wosum-wo_ser
       AND wo_nation  = it_wosum-nation
       AND wo_dealer  = it_wosum-dealer
       AND wo_extc    = it_wosum-extc
       AND wo_intc    = it_wosum-intc
       AND body_no    = ''
       AND status = ''
       AND wo_dealer1 <> ''.
*-->

*    IT_PMT03FB-PRDQ = IT_WOSUM2-RP08MQ.
*      IT_PMT03FB-LCGU =
**>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
* Start : Added By Tonkey on 03/31/2004
**>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    PERFORM READ_WOHD         USING 'P_LC_COUNT'
                             CHANGING IT_PMT03FB-LCGU.
    PERFORM READ_WOHD         USING 'P_WO_MODI_DATE'
                           CHANGING IT_PMT03FB-STTM.
    PERFORM READ_WOHD         USING 'P_WO_CREATE_DATE'
                           CHANGING IT_PMT03FB-CRTD.
**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
* End   : Added By Tonkey on 03/31/2004
**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

*    it_pmt03fb-sttm = sy-datum.
    IT_PMT03FB-CUSER = SY-UNAME(7).
*    it_pmt03fb-crtd = sy-datum.
    READ TABLE IT_WOSUM2 WITH KEY WO_SER = IT_WOSUM-WO_SER
                                  NATION = IT_WOSUM-NATION
                                  DEALER = IT_WOSUM-DEALER
                                  EXTC   = IT_WOSUM-EXTC
                                  INTC   = IT_WOSUM-INTC
                                  BINARY SEARCH.  "Manju & Haseeb
    IF SY-SUBRC = 0.
      IT_PMT03FB-EXPQ = IT_WOSUM2-ESTPRODQTY.
      IT_PMT03FB-PRDQ = IT_WOSUM2-RP08MQ.
    ENDIF.
    APPEND IT_PMT03FB.  CLEAR IT_PMT03FB.
*    ENDIF.
  ENDLOOP.
ENDFORM.                    " DATA_JOIN

*&---------------------------------------------------------------------*
*&      Form  DATA_UPDATE
*&---------------------------------------------------------------------*
FORM DATA_UPDATE.
  DATA: L_TEXT(60) TYPE C,
        L_INT TYPE I.
  DELETE FROM ZTPP_PMT03FB CLIENT SPECIFIED WHERE MANDT = SY-MANDT.

*  MODIFY ztpp_pmt03fb FROM TABLE it_pmt03fb. "Manju
  INSERT ZTPP_PMT03FB FROM TABLE IT_PMT03FB. "Haseeb

  IF SY-SUBRC = 0.
    DESCRIBE TABLE IT_PMT03FB LINES L_INT.
    WRITE L_INT TO L_TEXT LEFT-JUSTIFIED.
    COMMIT WORK.
    CONCATENATE 'Created Record count :' L_TEXT
      INTO L_TEXT.
    MESSAGE  S001 WITH L_TEXT.
    MESSAGE  S001 WITH TEXT-002.
  ELSE.
    ROLLBACK WORK.
    MESSAGE  W001 WITH TEXT-003.
  ENDIF.
ENDFORM.                    " DATA_UPDATE

*&---------------------------------------------------------------------*
*&      Form  FTP_HANDLING_MASTER
*&---------------------------------------------------------------------*
FORM FTP_HANDLING_MASTER TABLES   PA_CHAR
                         USING    P_WOCL.
  DATA: L_WOCL             LIKE MARA-MATNR.
  L_WOCL = P_WOCL.
  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'  "haseeb
         EXPORTING
             OBJECT       = L_WOCL
             MODE         = 'R'
             CTYPE        = '001'
             DISPLAY      = 'X'
        TABLES
             VAL_TABLE    = PA_CHAR
        EXCEPTIONS
             NO_DATA      = 1
             ERROR_MODE   = 2
             ERROR_OBJECT = 3
             OTHERS       = 4.
ENDFORM.                    " FTP_HANDLING_MASTER

*&---------------------------------------------------------------------*
*&      Form  READ_WOHD
*&---------------------------------------------------------------------*
FORM READ_WOHD USING
                   P_ATNAM
            CHANGING P_ATWRT.
  CLEAR: P_ATWRT.
  READ TABLE IT_HEAD WITH KEY ATNAM = P_ATNAM BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    P_ATWRT = IT_HEAD-ATWRT.
  ENDIF.
ENDFORM.                    " READ_WOHD

*&---------------------------------------------------------------------*
*&      Form  READ_WOCL
*&---------------------------------------------------------------------*
FORM READ_WOCL USING    P_ATNAM
            CHANGING P_ATWRT.
  READ TABLE L_AUSP WITH KEY ATNAM = P_ATNAM.
  IF SY-SUBRC EQ 0.
    P_ATWRT = L_AUSP-ATWRT.
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
FORM DATA_JOIN_NEW.
  DATA : $IX LIKE SY-TABIX,
         $FLAG(1).

  DATA : $DEST LIKE IT_PMT03FB-DEST,
         $LCNO LIKE IT_PMT03FB-LCNO,
         $CMPT LIKE IT_PMT03FB-CMPT,
         $LCGU LIKE IT_PMT03FB-LCGU,
         $STTM LIKE IT_PMT03FB-STTM,
         $CRTD LIKE IT_PMT03FB-CRTD.

  DATA: BEGIN OF L_OBJECT OCCURS 0,
          OBJEK LIKE AUSP-OBJEK,
        END OF L_OBJECT.

  LOOP AT IT_WOSUM.
    $IX = SY-TABIX.
    CONCATENATE IT_WOSUM-WO_SER IT_WOSUM-NATION IT_WOSUM-DEALER
                IT_WOSUM-EXTC IT_WOSUM-INTC INTO IT_WOSUM-WA_WOCL.
    L_OBJECT-OBJEK = IT_WOSUM-$WA_WOCL = IT_WOSUM-WA_WOCL(14).
    MODIFY IT_WOSUM INDEX $IX TRANSPORTING $WA_WOCL WA_WOCL.
    APPEND L_OBJECT.
  ENDLOOP.

  SORT L_OBJECT.
  DELETE ADJACENT DUPLICATES FROM L_OBJECT.

  REFRESH : L_AUSP.
  CLEAR : L_AUSP.

  SELECT A~OBJEK A~ATINN A~ATWRT A~ATFLV B~ATFOR B~ATNAM
  INTO CORRESPONDING FIELDS OF TABLE L_AUSP
    FROM AUSP AS A
    JOIN CABN AS B
   ON B~ATINN EQ A~ATINN
   FOR ALL ENTRIES IN L_OBJECT
   WHERE A~OBJEK = L_OBJECT-OBJEK
     AND A~KLART = '001'
    %_HINTS ORACLE 'FIRST_ROWS(10)'.


  DATA: L_VALS(8)           TYPE N            ,
        L_TIME(6)           TYPE N            .

  LOOP AT L_AUSP WHERE ATFOR EQ 'NUM' OR
                       ATFOR EQ 'DATE' OR
                       ATFOR EQ 'TIME'.
    CASE L_AUSP-ATFOR .
      WHEN 'NUM' .
        L_AUSP-ATWRT = L_AUSP-ATFLV.
      WHEN 'DATE'.
        L_AUSP-ATWRT = L_VALS  = L_AUSP-ATFLV.
      WHEN 'TIME'.
        L_AUSP-ATWRT = L_TIME = L_AUSP-ATFLV .
    ENDCASE.
    MODIFY L_AUSP TRANSPORTING ATWRT.
  ENDLOOP.

  SORT L_AUSP BY OBJEK ATNAM.

  LOOP AT IT_WOSUM.

    AT NEW $WA_WOCL.
      $FLAG = 'X'.
    ENDAT.

    IF $FLAG EQ 'X'.
      CLEAR: $DEST,
             $LCNO,
             $CMPT,
             $LCGU,
             $STTM,
             $CRTD.

      PERFORM READ_WOHD_NEW         USING 'P_DESTINATION_CODE'
                                           IT_WOSUM-$WA_WOCL
                                  CHANGING $DEST.
      PERFORM READ_WOHD_NEW         USING 'P_LC_NO'
                                          IT_WOSUM-$WA_WOCL
                                  CHANGING $LCNO.
      PERFORM READ_WOHD_NEW         USING 'P_PROD_FLAG'
                                           IT_WOSUM-$WA_WOCL
                               CHANGING  WA_PROD_FLAG.

      IF WA_PROD_FLAG = 'Y'.
        $CMPT = 'O'.
      ELSE.
        $CMPT = 'X'.
      ENDIF.
** Changed by Furong on 04/09/08
      IF WA_PROD_FLAG = 'N'.
        CLEAR: IT_PMT03FB.
        CONTINUE.
      ENDIF.
** End of change on 04/09/08
      PERFORM READ_WOHD_NEW         USING 'P_LC_COUNT'
                                       IT_WOSUM-$WA_WOCL
                               CHANGING $LCGU.
      PERFORM READ_WOHD_NEW         USING 'P_WO_MODI_DATE'
                                       IT_WOSUM-$WA_WOCL
                             CHANGING $STTM.
      PERFORM READ_WOHD_NEW         USING 'P_WO_CREATE_DATE'
                                       IT_WOSUM-$WA_WOCL
                             CHANGING $CRTD.
      CLEAR $FLAG.
    ENDIF.

    IT_PMT03FB-DEST = $DEST.
    IT_PMT03FB-LCNO = $LCNO.
    IT_PMT03FB-CMPT = $CMPT.
    IT_PMT03FB-LCGU = $LCGU.
    IT_PMT03FB-STTM = $STTM.
    IT_PMT03FB-CRTD = $CRTD.

    IT_PMT03FB-PLNT = '1'.
** Changed by Furong on 10/09/07 for EBOM
    IF IT_WOSUM-FSC+13(1) = SPACE.
      IT_PMT03FB-MODL = IT_WOSUM-FSC+6(3).
    ELSE.
      IT_PMT03FB-MODL = IT_WOSUM-FSC+5(3).
    ENDIF.
** End of change
    IT_PMT03FB-USEE = IT_WOSUM-WO_SER(1).
    IT_PMT03FB-PACK = IT_WOSUM-WO_SER+1(4).
    IT_PMT03FB-REGN = IT_WOSUM-WO_SER+5(1).
    IT_PMT03FB-SERL = IT_WOSUM-WO_SER+6(3).
    CONCATENATE IT_WOSUM-NATION IT_WOSUM-DEALER
                INTO IT_PMT03FB-DIST.
    IT_PMT03FB-TEAM = ' '.
    IT_PMT03FB-EXTC = IT_WOSUM-EXTC.
    IT_PMT03FB-INTC = IT_WOSUM-INTC.
** Changed by Furong on 10/09/07 for EBOM
    IF IT_WOSUM-FSC+13(1) = SPACE.
      IT_PMT03FB-BMDL = IT_WOSUM-FSC+6(8).
    ELSE.
      IT_PMT03FB-BMDL = IT_WOSUM-FSC+5(9).
    ENDIF.
* End of change
    IT_PMT03FB-OCNN = IT_WOSUM-FSC+14(4).
    IT_PMT03FB-VERS = IT_WOSUM-VERSION.
    IT_PMT03FB-ORDQ = IT_WOSUM-MODQTY.
    IT_PMT03FB-CONQ = IT_WOSUM-PLANQTY.
    IT_PMT03FB-FORQ = IT_WOSUM-FORECASTQTY.
    IT_PMT03FB-MITQ = IT_WOSUM-MITUQTY.
    IT_PMT03FB-PT00 = IT_WOSUM-SEQQTY.
    IT_PMT03FB-PT01 = IT_WOSUM-RP01TQ.
    IT_PMT03FB-PT02 = IT_WOSUM-RP02TQ.
    IT_PMT03FB-PT03 = IT_WOSUM-RP03TQ.
    IT_PMT03FB-PT04 = IT_WOSUM-RP04TQ.
    IT_PMT03FB-PT05 = IT_WOSUM-RP05TQ.
    IT_PMT03FB-PT06 = IT_WOSUM-RP06TQ.
    IT_PMT03FB-PT07 = IT_WOSUM-RP07TQ.
    IT_PMT03FB-PT08 = IT_WOSUM-RP08TQ.
    IT_PMT03FB-PT09 = IT_WOSUM-RP09TQ.
    IT_PMT03FB-PT10 = IT_WOSUM-RP10TQ + IT_WOSUM-RP11TQ +
                      IT_WOSUM-RP12TQ.
    IT_PMT03FB-PT11 = IT_WOSUM-RP13TQ.
    IT_PMT03FB-CUSER = SY-UNAME(7).

*--<Victor 07.07.2011

*-  Body + Dealer + '' or F
    SELECT COUNT( * ) INTO it_pmt03fb-dealer_bqty
    FROM ztsd_um
     WHERE wo_serial  = it_wosum-wo_ser
       AND wo_nation  = it_wosum-nation
       AND wo_dealer  = it_wosum-dealer
       AND wo_extc    = it_wosum-extc
       AND wo_intc    = it_wosum-intc
       AND body_no    <> ''
       AND ( status = '' OR status = 'F' )
       AND wo_dealer1 <> ''.

*-  No Body + Dealer + ''
    SELECT COUNT( * ) INTO it_pmt03fb-dealer_nqty
    FROM ztsd_um
     WHERE wo_serial  = it_wosum-wo_ser
       AND wo_nation  = it_wosum-nation
       AND wo_dealer  = it_wosum-dealer
       AND wo_extc    = it_wosum-extc
       AND wo_intc    = it_wosum-intc
       AND body_no    = ''
       AND status = ''
       AND wo_dealer1 <> ''.
*-->

    READ TABLE IT_WOSUM2 WITH KEY WO_SER = IT_WOSUM-WO_SER
                                  NATION = IT_WOSUM-NATION
                                  DEALER = IT_WOSUM-DEALER
                                  EXTC   = IT_WOSUM-EXTC
                                  INTC   = IT_WOSUM-INTC
                                  BINARY SEARCH.
    IF SY-SUBRC = 0.
      IT_PMT03FB-EXPQ = IT_WOSUM2-ESTPRODQTY.
      IT_PMT03FB-PRDQ = IT_WOSUM2-RP08MQ.
    ENDIF.
    APPEND IT_PMT03FB.  CLEAR IT_PMT03FB.

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
FORM READ_WOHD_NEW USING
                   P_ATNAM
                   P_WA_WOCL
            CHANGING P_ATWRT.
** Change by Furong on 05/16/08 ;UD1K943652
  CLEAR: P_ATWRT.
** End of change
  READ TABLE L_AUSP WITH KEY   OBJEK = P_WA_WOCL
                                ATNAM = P_ATNAM BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    P_ATWRT = L_AUSP-ATWRT.
  ENDIF.
ENDFORM.                    " READ_WOHD
