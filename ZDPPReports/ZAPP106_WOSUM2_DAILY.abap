REPORT  ZAPP106_WOSUM2_DAILY  NO STANDARD PAGE HEADING
                              MESSAGE-ID ZMPP.
************************************************************************
* Program Name      : ZAPP106_WOSUM2_DAILY
* Author            : Furong Wang
* Creation Date     : 01/25/2006
* Specifications By : MY Hur
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       : WorkOrder Summary2 Creation
*
************************************************************************
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************

TABLES: AUSP ,               " Characteristic Value..
        ZTPP_COMMON_VALS,    " Common Value Tables..
        ZTPP_WOSUM,          "ERP_WO QTY SUMMARY
        ZTPP_WOSUM2.         "WORK ORDER SUMMARY DAILY

DATA: IT_WOSUM2              LIKE ZTPP_WOSUM2 OCCURS 0 WITH HEADER LINE,
      IT_DAILY               LIKE ZTPP_WOSUM2 OCCURS 0 WITH HEADER LINE,
      IT_LIST                LIKE ZTPP_WOSUM2 OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF IT_WOSUM      OCCURS 0.
        INCLUDE STRUCTURE    ZTPP_WOSUM.
DATA:     PERF               TYPE C    ,
          PROD               TYPE C    ,
          MATNR              LIKE MARA-MATNR,
      END OF IT_WOSUM.

DATA: IT_DAY_SUM             LIKE ZTPP_DAY_SUM OCCURS 0 WITH HEADER LINE
.

DATA: WA_MATNR               LIKE MARA-MATNR,
      WA_FDATE               LIKE SY-DATUM,
      WA_TDATE               LIKE SY-DATUM.
DATA: WA_ATINN_PERF           LIKE AUSP-ATINN ,    " P_PERF_YN
      WA_ATINN_PROD           LIKE AUSP-ATINN.    " P_PROD_FLAG
************************************************************************
*              SELECTION SCREEN LAYOUT                                 *
************************************************************************
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-004.
PARAMETERS: P_CLOSE       TYPE C AS CHECKBOX.
SKIP 1 .
SELECT-OPTIONS:
            S_DATE        FOR  SY-DATUM     NO-EXTENSION.
SELECTION-SCREEN: END OF BLOCK B1.

************************************************************************
*              INITIALIZATION                                          *
************************************************************************
INITIALIZATION.
  " Decide the Working Intervals...
*  IF sy-uzeit < '063000' .
  WA_TDATE = SY-DATUM - 1.
*  ELSE.
*    wa_tdate = sy-datum    .
*  ENDIF.

  SELECT SINGLE *
    FROM ZTPP_COMMON_VALS
   WHERE JOBS = SY-REPID  .

  WA_FDATE = ZTPP_COMMON_VALS-DATES + 1 .

  IF SY-DATUM < WA_FDATE OR SY-SUBRC NE 0.
    MESSAGE W001 WITH 'Error Data in the ZTPP_COMMON_VALS Table...' .
  ENDIF.
  CONCATENATE 'IBT' WA_FDATE WA_TDATE   INTO S_DATE .
  APPEND S_DATE.

************************************************************************
*              AT SELECTION SCREEN                                     *
************************************************************************
AT SELECTION-SCREEN.

************************************************************************
*              START-OF-SELECTION PROCESSING                           *
************************************************************************
START-OF-SELECTION.
  GET TIME.
  WRITE AT: /001(050) 'Start time for the Processing.....',
             050(010) SY-DATUM ,
             062(010) SY-UZEIT .

*  PERFORM set_atinn  .
  PERFORM DATA_SELECT.
  PERFORM DATA_SUM.
  PERFORM CLOSING_CHECK.
  PERFORM CREATE_LIST.

END-OF-SELECTION.
  GET TIME.
  WRITE AT: /001(050) 'End time for the Processing.....',
             050(010) SY-DATUM ,
             062(010) SY-UZEIT .

************************************************************************
*              TOP-OF-PAGE                                             *
************************************************************************
TOP-OF-PAGE.


*&---------------------------------------------------------------------*
*&      Form  DATA_SELECT
*&---------------------------------------------------------------------*
FORM DATA_SELECT.
  DATA: L_ATWRT              LIKE AUSP-ATWRT.

  WA_TDATE = S_DATE-HIGH.
  SELECT A~WO_SER A~NATION A~DEALER A~EXTC A~INTC A~INITQTY A~MODQTY
         A~SEQQTY A~PLANQTY A~FORECASTQTY A~MITUQTY A~WOCREDATE
         A~WOMODDATE A~FSC A~VERSION A~SALES
         A~RP01TQ A~RP02TQ A~RP03TQ
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
   WHERE A~MODQTY > B~RP16TQ.
*   WHERE a~modqty > b~rp09tq.

  LOOP AT IT_WOSUM.
    CONCATENATE IT_WOSUM-WO_SER IT_WOSUM-NATION IT_WOSUM-DEALER
           INTO IT_WOSUM-MATNR.

    CLEAR L_ATWRT.
    SELECT SINGLE ATWRT INTO L_ATWRT
      FROM AUSP
     WHERE OBJEK = IT_WOSUM-MATNR
       AND KLART = '001'
       AND ATINN = WA_ATINN_PERF.

    IT_WOSUM-PERF = L_ATWRT(1)   .

    CLEAR L_ATWRT .
    SELECT SINGLE ATWRT INTO L_ATWRT
      FROM AUSP
     WHERE OBJEK = IT_WOSUM-MATNR
       AND KLART = '001'
       AND ATINN = WA_ATINN_PROD    .
    IT_WOSUM-PROD = L_ATWRT(1)   .
    MODIFY IT_WOSUM.
  ENDLOOP.

  SORT IT_WOSUM.
  IF IT_WOSUM[] IS INITIAL.
    MESSAGE E001 WITH TEXT-001 .
  ENDIF.

  " Check the WOSUM2 Record!!!!
  SELECT * FROM ZTPP_WOSUM2
           INTO TABLE IT_WOSUM2
   WHERE CR_DATE = WA_TDATE .
  IF SY-SUBRC = 0.
    DELETE FROM ZTPP_WOSUM2 WHERE CR_DATE = WA_TDATE .
  ENDIF.

** get day_sum data

  SELECT * INTO TABLE IT_DAY_SUM
   FROM ZTPP_DAY_SUM
    FOR ALL ENTRIES IN IT_WOSUM
   WHERE WO_SER = IT_WOSUM-WO_SER
     AND NATION = IT_WOSUM-NATION
     AND DEALER = IT_WOSUM-DEALER
     AND EXTC = IT_WOSUM-EXTC
     AND INTC = IT_WOSUM-INTC
     AND WDATE BETWEEN S_DATE-LOW AND S_DATE-HIGH.

ENDFORM.                    " DATA_SELECT

*&---------------------------------------------------------------------*
*&      Form  DATA_SUM
*&---------------------------------------------------------------------*
FORM DATA_SUM.
  DATA: L_QTY(5),
        L_NO(2) TYPE N,
        L_FIELD(20),
        L_FIELD_SUM(20),
        L_DATE LIKE SY-DATUM.

  FIELD-SYMBOLS: <FS_AQ>, <FS_MQ>, <FS_CQ>, <FS_SUM>.

  SORT IT_DAY_SUM BY WO_SER NATION DEALER EXTC INTC.
  LOOP AT IT_WOSUM.
    CLEAR: IT_DAILY, WA_MATNR.
    L_NO = 1.

    LOOP AT IT_DAY_SUM WHERE WO_SER = IT_WOSUM-WO_SER
                         AND NATION = IT_WOSUM-NATION
                         AND DEALER = IT_WOSUM-DEALER
                         AND EXTC = IT_WOSUM-EXTC
                         AND INTC = IT_WOSUM-INTC.

      IT_DAILY-RP01MQ = IT_DAILY-RP01MQ + IT_DAY_SUM-RP01Q.
      IT_DAILY-RP02MQ = IT_DAILY-RP02MQ + IT_DAY_SUM-RP02Q.
      IT_DAILY-RP03MQ = IT_DAILY-RP03MQ + IT_DAY_SUM-RP03Q.
      IT_DAILY-RP04MQ = IT_DAILY-RP04MQ + IT_DAY_SUM-RP04Q.
      IT_DAILY-RP05MQ = IT_DAILY-RP05MQ + IT_DAY_SUM-RP05Q.
      IT_DAILY-RP06MQ = IT_DAILY-RP06MQ + IT_DAY_SUM-RP06Q.
      IT_DAILY-RP07MQ = IT_DAILY-RP07MQ + IT_DAY_SUM-RP17Q.
      IT_DAILY-RP08MQ = IT_DAILY-RP08MQ + IT_DAY_SUM-RP18Q.
      IT_DAILY-RP09MQ = IT_DAILY-RP09MQ + IT_DAY_SUM-RP19Q.
      IT_DAILY-RP10MQ = IT_DAILY-RP10MQ + IT_DAY_SUM-RP20Q.
      IT_DAILY-RP11MQ = IT_DAILY-RP11MQ + IT_DAY_SUM-RP21Q.
      IT_DAILY-RP12MQ = IT_DAILY-RP12MQ + IT_DAY_SUM-RP22Q.
      IT_DAILY-RP13MQ = IT_DAILY-RP13MQ + IT_DAY_SUM-RP23Q.

      IT_DAILY-RP14MQ = IT_DAILY-RP14MQ + IT_DAY_SUM-RP24Q.
      IT_DAILY-RP14MQ = IT_DAILY-RP14MQ + IT_DAY_SUM-RP26Q.

      IT_DAILY-RP15MQ = IT_DAILY-RP15MQ + IT_DAY_SUM-RP25Q.
      IT_DAILY-RP15MQ = IT_DAILY-RP15MQ + IT_DAY_SUM-RP27Q.

      IT_DAILY-RP16MQ = IT_DAILY-RP01MQ + IT_DAY_SUM-RP28Q.
    ENDLOOP.

    READ TABLE IT_DAY_SUM  WITH KEY
                   WO_SER = IT_WOSUM-WO_SER
                   NATION = IT_WOSUM-NATION
                   DEALER = IT_WOSUM-DEALER
                   EXTC = IT_WOSUM-EXTC
                   INTC = IT_WOSUM-INTC
                   WDATE = S_DATE-HIGH.

    IF SY-SUBRC EQ 0.
      IT_DAILY-RP01AQ = IT_DAY_SUM-RP01Q.
      IT_DAILY-RP02AQ = IT_DAY_SUM-RP02Q.
      IT_DAILY-RP03AQ = IT_DAY_SUM-RP03Q.
      IT_DAILY-RP04AQ = IT_DAY_SUM-RP04Q.
      IT_DAILY-RP05AQ = IT_DAY_SUM-RP05Q.
      IT_DAILY-RP06AQ = IT_DAY_SUM-RP06Q.
      IT_DAILY-RP07AQ = IT_DAY_SUM-RP17Q.
      IT_DAILY-RP08AQ = IT_DAY_SUM-RP18Q.
      IT_DAILY-RP09AQ = IT_DAY_SUM-RP19Q.
      IT_DAILY-RP10AQ = IT_DAY_SUM-RP20Q.
      IT_DAILY-RP11AQ = IT_DAY_SUM-RP21Q.
      IT_DAILY-RP12AQ = IT_DAY_SUM-RP22Q.
      IT_DAILY-RP13AQ = IT_DAY_SUM-RP23Q.
      IT_DAILY-RP14AQ = IT_DAY_SUM-RP24Q + IT_DAY_SUM-RP26Q.
      IT_DAILY-RP15AQ = IT_DAY_SUM-RP25Q + IT_DAY_SUM-RP27Q.
      IT_DAILY-RP16AQ = IT_DAY_SUM-RP28Q.
    ENDIF.
    IT_DAILY-CR_MONTH  = S_DATE-LOW(6).
    IT_DAILY-CR_DATE   = S_DATE-HIGH.
    IT_DAILY-WO_SER    = IT_WOSUM-WO_SER.
    IT_DAILY-NATION    = IT_WOSUM-NATION.
    IT_DAILY-DEALER    = IT_WOSUM-DEALER.
    IT_DAILY-EXTC      = IT_WOSUM-EXTC.
    IT_DAILY-INTC      = IT_WOSUM-INTC.
** Changed by Furong on 10/09/07 for EBOM
*    it_daily-mi        = it_wosum-fsc+6(8).
    IF IT_WOSUM-FSC+13(1) = SPACE.
      IT_DAILY-MI        = IT_WOSUM-FSC+6(8).
    ELSE.
      IT_DAILY-MI        = IT_WOSUM-FSC+5(9).
    ENDIF.
* End of change
    IT_DAILY-OCN       = IT_WOSUM-FSC+14(4).
    IT_DAILY-VERSION   = IT_WOSUM-VERSION.
    IT_DAILY-REGION    = ' '.                    " Undefined Logic!
    IT_DAILY-TEAM      = ' '.                    " Undefined Logic!
    IT_DAILY-ORDERQTY  = IT_WOSUM-MODQTY.
    CASE IT_WOSUM-DEALER.
      WHEN 'XX'.
        IT_DAILY-STOCK     = IT_WOSUM-RP08TQ - IT_WOSUM-RP09TQ.
        IT_DAILY-REMORDQTY = IT_WOSUM-MODQTY - IT_WOSUM-RP08TQ.
        IT_DAILY-RP01CQ    = IT_WOSUM-RP01TQ - IT_WOSUM-RP02TQ.
        IT_DAILY-RP02CQ    = IT_WOSUM-RP02TQ - IT_WOSUM-RP03TQ.
        IT_DAILY-RP03CQ    = IT_WOSUM-RP03TQ - IT_WOSUM-RP04TQ.
        IT_DAILY-RP04CQ    = IT_WOSUM-RP04TQ - IT_WOSUM-RP08TQ.
*       it_daily-rp05cq    = it_wosum-rp05tq - it_wosum-rp06tq.
*       it_daily-rp06cq    = it_wosum-rp06tq - it_wosum-rp07tq.
*       it_daily-rp07cq    = it_wosum-rp07tq - it_wosum-rp08tq.
        IT_DAILY-RP08CQ    = IT_WOSUM-RP08TQ                  .
*       it_daily-rp09cq    = it_wosum-rp09tq - it_wosum-rp10tq.
*       it_daily-rp10cq    = it_wosum-rp10tq - it_wosum-rp11tq.
*       it_daily-rp11cq    = it_wosum-rp11tq - it_wosum-rp12tq.
*       it_daily-rp12cq    = it_wosum-rp12tq - it_wosum-rp13tq.
*       it_daily-rp13cq    = it_wosum-rp13tq - it_wosum-rp14tq.
*       it_daily-rp14cq    = it_wosum-rp14tq - it_wosum-rp15tq.
*       it_daily-rp15cq    = it_wosum-rp15tq - it_wosum-rp16tq.
*       it_daily-rp16cq    = it_wosum-rp16tq .
      WHEN 'XY'.
        IT_DAILY-STOCK     = IT_WOSUM-RP08TQ - IT_WOSUM-RP09TQ.
        IT_DAILY-REMORDQTY = IT_WOSUM-MODQTY - IT_WOSUM-RP08TQ.
        IT_DAILY-RP01CQ    = IT_WOSUM-RP01TQ - IT_WOSUM-RP02TQ.
        IT_DAILY-RP02CQ    = IT_WOSUM-RP02TQ - IT_WOSUM-RP03TQ.
        IT_DAILY-RP03CQ    = IT_WOSUM-RP03TQ - IT_WOSUM-RP04TQ.
        IT_DAILY-RP04CQ    = IT_WOSUM-RP04TQ - IT_WOSUM-RP05TQ.
        IT_DAILY-RP05CQ    = IT_WOSUM-RP05TQ - IT_WOSUM-RP06TQ.
        IT_DAILY-RP06CQ    = IT_WOSUM-RP06TQ - IT_WOSUM-RP08TQ.
*       it_daily-rp07cq    = it_wosum-rp07tq - it_wosum-rp08tq.
        IT_DAILY-RP08CQ    = IT_WOSUM-RP08TQ                  .
*       it_daily-rp09cq    = it_wosum-rp09tq - it_wosum-rp10tq.
*       it_daily-rp10cq    = it_wosum-rp10tq - it_wosum-rp11tq.
*       it_daily-rp11cq    = it_wosum-rp11tq - it_wosum-rp12tq.
*       it_daily-rp12cq    = it_wosum-rp12tq - it_wosum-rp13tq.
*       it_daily-rp13cq    = it_wosum-rp13tq - it_wosum-rp14tq.
*       it_daily-rp14cq    = it_wosum-rp14tq - it_wosum-rp15tq.
*       it_daily-rp15cq    = it_wosum-rp15tq - it_wosum-rp16tq.
*       it_daily-rp16cq    = it_wosum-rp16tq .
      WHEN OTHERS.
        IT_DAILY-STOCK     = IT_WOSUM-RP08TQ - IT_WOSUM-RP09TQ.
        IT_DAILY-REMORDQTY = IT_WOSUM-MODQTY - IT_WOSUM-RP08TQ.
        IT_DAILY-RP01CQ    = IT_WOSUM-RP01TQ - IT_WOSUM-RP02TQ.
        IT_DAILY-RP02CQ    = IT_WOSUM-RP02TQ - IT_WOSUM-RP03TQ.
        IT_DAILY-RP03CQ    = IT_WOSUM-RP03TQ - IT_WOSUM-RP04TQ.
        IT_DAILY-RP04CQ    = IT_WOSUM-RP04TQ - IT_WOSUM-RP05TQ.
        IT_DAILY-RP05CQ    = IT_WOSUM-RP05TQ - IT_WOSUM-RP06TQ.
        IT_DAILY-RP06CQ    = IT_WOSUM-RP06TQ - IT_WOSUM-RP07TQ.
        IT_DAILY-RP07CQ    = IT_WOSUM-RP07TQ - IT_WOSUM-RP08TQ.
        IT_DAILY-RP08CQ    = IT_WOSUM-RP08TQ - IT_WOSUM-RP09TQ.
        IT_DAILY-RP09CQ    = IT_WOSUM-RP09TQ - IT_WOSUM-RP12TQ.
*        it_daily-rp10cq    = it_wosum-rp10tq - it_wosum-rp11tq.'no data
*        it_daily-rp11cq    = it_wosum-rp11tq - it_wosum-rp12tq.'no data
        IT_DAILY-RP12CQ    = IT_WOSUM-RP12TQ - IT_WOSUM-RP15TQ.
*        it_daily-rp13cq    = it_wosum-rp13tq - it_wosum-rp14tq.'no data
*        it_daily-rp14cq    = it_wosum-rp14tq - it_wosum-rp15tq.'no data
*        it_daily-rp15cq    = it_wosum-rp15tq - it_wosum-rp16tq.'no data
*        it_daily-rp16cq    = it_wosum-rp16tq .
    ENDCASE.
    IT_DAILY-ESTPRODQTY =  0 .                    " Undefined Logic!
    IT_DAILY-PERF      = IT_WOSUM-PERF   .
    IT_DAILY-PROD      = IT_WOSUM-PROD   .
    IT_DAILY-WHOLESALE =   0 .                    " Undefined Logic!
    IT_DAILY-RETAIL    =   0 .                    " Undefined Logic!
    IT_DAILY-ERDAT     = SY-DATUM.
    IT_DAILY-ERZET     = SY-UZEIT.
    IT_DAILY-ERNAM     = SY-UNAME.
    APPEND IT_DAILY.
    CLEAR: IT_WOSUM.
  ENDLOOP.
  SORT IT_DAILY.
ENDFORM.                    " DAILY_SUM

*&---------------------------------------------------------------------*
*&      Form  CREATE_LIST
*&---------------------------------------------------------------------*
FORM CREATE_LIST.

  INSERT ZTPP_WOSUM2 FROM TABLE IT_DAILY ACCEPTING DUPLICATE KEYS .
  IF SY-SUBRC = 0.
    COMMIT WORK.
    MESSAGE  S001 WITH  TEXT-003 .
  ELSE.
    ROLLBACK WORK.
    MESSAGE  E001 WITH TEXT-002 .
  ENDIF.
ENDFORM.                    " CREATE_LIST

*&---------------------------------------------------------------------*
*&      Form  closing_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CLOSING_CHECK.
  IF P_CLOSE = 'X'.
    UPDATE ZTPP_COMMON_VALS  SET DATES = S_DATE-HIGH
                           WHERE JOBS  = SY-REPID   .
    IF SY-SUBRC NE 0.
      MESSAGE W001 WITH 'Can not Update the Value in ZTPP_COMMON_VALS'.
    ENDIF.
  ENDIF.
ENDFORM.                    " closing_check
