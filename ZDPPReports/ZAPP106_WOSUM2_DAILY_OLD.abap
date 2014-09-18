************************************************************************
* Program Name      : ZAPP106_WOSUM2_DAILY
* Author            : DongYeop, Han
* Creation Date     : 2003.11.10.
* Specifications By : DongYeop, Han
* Pattern           :
* Development Request No : UD1K902031
* Addl Documentation:
* Description       : WorkOrder Summary2(Daily)Creation
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

REPORT  ZAPP106_WOSUM2_DAILY  NO STANDARD PAGE HEADING
                              MESSAGE-ID ZDPP.


************************************************************************
*              D A T A     A R E A                                     *
************************************************************************
TABLES: ZTPP_WOSUM,          "ERP_WO QTY SUMMARY
        ZTPP_WOSUM2.         "WORK ORDER SUMMARY DAILY

DATA: IT_WOSUM LIKE ZTPP_WOSUM OCCURS 0 WITH HEADER LINE.
DATA: IT_WOSUM2 LIKE ZTPP_WOSUM2 OCCURS 0 WITH HEADER LINE.
DATA: IT_WOSUM2_C LIKE ZTPP_WOSUM2 OCCURS 0 WITH HEADER LINE.
DATA: IT_DAILY LIKE ZTPP_WOSUM2 OCCURS 0 WITH HEADER LINE.
DATA: IT_LIST LIKE ZTPP_WOSUM2 OCCURS 0 WITH HEADER LINE.

DATA: F_DATE LIKE SY-DATUM,
      L_DATE LIKE SY-DATUM.
************************************************************************
*              SELECTION SCREEN LAYOUT                                 *
************************************************************************
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS: P_DATE LIKE SY-DATUM DEFAULT SY-DATUM.
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
  PERFORM DATA_SELECT.
  PERFORM DATA_SUM.
  PERFORM CREATE_LIST.

END-OF-SELECTION.

************************************************************************
*              TOP-OF-PAGE                                             *
************************************************************************
TOP-OF-PAGE.


*&---------------------------------------------------------------------*
*&      Form  DATA_SELECT
*&---------------------------------------------------------------------*
FORM DATA_SELECT.
  PERFORM DATE_INIT.
  SELECT * FROM ZTPP_WOSUM
           INTO TABLE IT_WOSUM.
  SORT IT_WOSUM.
  IF IT_WOSUM[] IS INITIAL.
    IF SY-BATCH NE 'X'.
      MESSAGE E001 WITH 'NO DATA!!'.
    ENDIF.
  ENDIF.
  SELECT * FROM ZTPP_WOSUM2
           INTO TABLE IT_WOSUM2
           WHERE CR_DATE BETWEEN F_DATE AND L_DATE.
  SORT IT_WOSUM2.
ENDFORM.                    " DATA_SELECT
*&---------------------------------------------------------------------*
*&      Form  DATE_INIT
*&---------------------------------------------------------------------*
FORM DATE_INIT.
  IF P_DATE+6(2) = '01'.
    CLEAR: F_DATE,
           L_DATE.
  ELSE.
    CONCATENATE SY-DATUM(6) '01' INTO F_DATE.
    L_DATE = P_DATE - 1.
    IF L_DATE+4(2) NE P_DATE+4(2).
      L_DATE = P_DATE.
    ENDIF.
  ENDIF.
*  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
*    EXPORTING
*      DAY_IN                  = SY-DATUM
*    IMPORTING
*      LAST_DAY_OF_MONTH       = L_DATE
** EXCEPTIONS
**   DAY_IN_NO_DATE          = 1
**   OTHERS                  = 2
*            .
*  IF SY-SUBRC <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.

ENDFORM.                    " DATE_INIT
*&---------------------------------------------------------------------*
*&      Form  DATA_SUM
*&---------------------------------------------------------------------*
FORM DATA_SUM.
  DATA L_QTY(5).
  LOOP AT IT_WOSUM.
    L_QTY = IT_WOSUM-MODQTY - IT_WOSUM-RP16TQ.
    IF L_QTY > 0.
*----->DAILY
      IT_DAILY-CR_DATE   = P_DATE.
      IT_DAILY-WO_SER    = IT_WOSUM-WO_SER.
      IT_DAILY-NATION    = IT_WOSUM-NATION.
      IT_DAILY-DEALER    = IT_WOSUM-DEALER.
      IT_DAILY-EXTC      = IT_WOSUM-EXTC.
      IT_DAILY-INTC      = IT_WOSUM-INTC.
      IT_DAILY-MI        = IT_WOSUM-FSC+6(8).
      IT_DAILY-OCN       = IT_WOSUM-FSC+14(4).
      IT_DAILY-VERSION   = IT_WOSUM-VERSION.
      IT_DAILY-REGION    = ' '.
      IT_DAILY-TEAM      = ' '.
      IT_DAILY-ORDERQTY  = IT_WOSUM-MODQTY.
      IT_DAILY-STOCK     = IT_WOSUM-RP08TQ - IT_WOSUM-RP09TQ.
      IT_DAILY-REMORDQTY = IT_WOSUM-RP08TQ.
      IT_DAILY-RP01CQ    = IT_WOSUM-RP01TQ - IT_WOSUM-RP02TQ.
      IT_DAILY-RP02CQ    = IT_WOSUM-RP02TQ - IT_WOSUM-RP03TQ.
      IT_DAILY-RP03CQ    = IT_WOSUM-RP03TQ - IT_WOSUM-RP04TQ.
      IT_DAILY-RP04CQ    = IT_WOSUM-RP04TQ - IT_WOSUM-RP05TQ.
      IT_DAILY-RP05CQ    = IT_WOSUM-RP05TQ - IT_WOSUM-RP06TQ.
      IT_DAILY-RP06CQ    = IT_WOSUM-RP06TQ - IT_WOSUM-RP07TQ.
      IT_DAILY-RP07CQ    = IT_WOSUM-RP07TQ - IT_WOSUM-RP08TQ.
      IT_DAILY-RP08CQ    = IT_WOSUM-RP08TQ - IT_WOSUM-RP09TQ.
      IT_DAILY-RP09CQ    = IT_WOSUM-RP09TQ - IT_WOSUM-RP10TQ.
      IT_DAILY-RP10CQ    = IT_WOSUM-RP10TQ - IT_WOSUM-RP11TQ.
      IT_DAILY-RP11CQ    = IT_WOSUM-RP11TQ - IT_WOSUM-RP12TQ.
      IT_DAILY-RP12CQ    = IT_WOSUM-RP12TQ - IT_WOSUM-RP13TQ.
      IT_DAILY-RP13CQ    = IT_WOSUM-RP13TQ - IT_WOSUM-RP14TQ.
      IT_DAILY-RP14CQ    = IT_WOSUM-RP14TQ - IT_WOSUM-RP15TQ.
      IT_DAILY-RP15CQ    = IT_WOSUM-RP15TQ - IT_WOSUM-RP16TQ.
      IT_DAILY-RP16CQ    = IT_WOSUM-RP16TQ .
      IT_DAILY-RP01AQ    = IT_WOSUM-RP01DQ .
      IT_DAILY-RP02AQ    = IT_WOSUM-RP02DQ .
      IT_DAILY-RP03AQ    = IT_WOSUM-RP03DQ .
      IT_DAILY-RP04AQ    = IT_WOSUM-RP04DQ .
      IT_DAILY-RP05AQ    = IT_WOSUM-RP05DQ .
      IT_DAILY-RP06AQ    = IT_WOSUM-RP06DQ .
      IT_DAILY-RP07AQ    = IT_WOSUM-RP07DQ .
      IT_DAILY-RP08AQ    = IT_WOSUM-RP08DQ .
      IT_DAILY-RP09AQ    = IT_WOSUM-RP09DQ .
      IT_DAILY-RP10AQ    = IT_WOSUM-RP10DQ .
      IT_DAILY-RP11AQ    = IT_WOSUM-RP11DQ .
      IT_DAILY-RP12AQ    = IT_WOSUM-RP12DQ .
      IT_DAILY-RP13AQ    = IT_WOSUM-RP13DQ .
      IT_DAILY-RP14AQ    = IT_WOSUM-RP14DQ .
      IT_DAILY-RP15AQ    = IT_WOSUM-RP15DQ .
      IT_DAILY-RP16AQ    = IT_WOSUM-RP16DQ .
*--->MONTHLY
      IT_DAILY-RP01MQ    = IT_DAILY-RP01AQ .
      IT_DAILY-RP02MQ    = IT_DAILY-RP02AQ .
      IT_DAILY-RP03MQ    = IT_DAILY-RP03AQ .
      IT_DAILY-RP04MQ    = IT_DAILY-RP04AQ .
      IT_DAILY-RP05MQ    = IT_DAILY-RP05AQ .
      IT_DAILY-RP06MQ    = IT_DAILY-RP06AQ .
      IT_DAILY-RP07MQ    = IT_DAILY-RP07AQ .
      IT_DAILY-RP08MQ    = IT_DAILY-RP08AQ .
      IT_DAILY-RP09MQ    = IT_DAILY-RP09AQ .
      IT_DAILY-RP10MQ    = IT_DAILY-RP10AQ .
      IT_DAILY-RP11MQ    = IT_DAILY-RP11AQ .
      IT_DAILY-RP12MQ    = IT_DAILY-RP12AQ .
      IT_DAILY-RP13MQ    = IT_DAILY-RP13AQ .
      IT_DAILY-RP14MQ    = IT_DAILY-RP14AQ .
      IT_DAILY-RP15MQ    = IT_DAILY-RP15AQ .
      IT_DAILY-RP16MQ    = IT_DAILY-RP16AQ .
      IT_DAILY-ERDAT     = SY-DATUM.
      IT_DAILY-ERZET     = SY-UZEIT.
      IT_DAILY-ERNAM     = SY-UNAME.
      APPEND IT_DAILY. CLEAR IT_DAILY.
    ENDIF.
  ENDLOOP.
  SORT IT_DAILY.
ENDFORM.                    " DAILY_SUM
*&---------------------------------------------------------------------*
*&      Form  CREATE_LIST
*&---------------------------------------------------------------------*
FORM CREATE_LIST.
  IF IT_WOSUM2[] IS INITIAL.
    MODIFY ZTPP_WOSUM2 FROM TABLE IT_DAILY.
    IF SY-SUBRC = 0.
      COMMIT WORK.
      IF SY-BATCH NE 'X'.
        MESSAGE  S001 WITH 'Completed Update successfully !'.
      ENDIF.
    ELSE.
      ROLLBACK WORK.
      IF SY-BATCH NE 'X'.
        MESSAGE  E001 WITH 'Failed in Update !'.
      ENDIF.
    ENDIF.
  ELSE.
    LOOP AT IT_WOSUM2.
      IT_WOSUM2_C-WO_SER    = IT_WOSUM2-WO_SER.
      IT_WOSUM2_C-NATION      = IT_WOSUM2-NATION.
      IT_WOSUM2_C-DEALER      = IT_WOSUM2-DEALER.
      IT_WOSUM2_C-EXTC        = IT_WOSUM2-EXTC.
      IT_WOSUM2_C-INTC        = IT_WOSUM2-INTC.
      IT_WOSUM2_C-RP01AQ    = IT_WOSUM2-RP01AQ .
      IT_WOSUM2_C-RP02AQ    = IT_WOSUM2-RP02AQ .
      IT_WOSUM2_C-RP03AQ    = IT_WOSUM2-RP03AQ .
      IT_WOSUM2_C-RP04AQ    = IT_WOSUM2-RP04AQ .
      IT_WOSUM2_C-RP05AQ    = IT_WOSUM2-RP05AQ .
      IT_WOSUM2_C-RP06AQ    = IT_WOSUM2-RP06AQ .
      IT_WOSUM2_C-RP07AQ    = IT_WOSUM2-RP07AQ .
      IT_WOSUM2_C-RP08AQ    = IT_WOSUM2-RP08AQ .
      IT_WOSUM2_C-RP09AQ    = IT_WOSUM2-RP09AQ .
      IT_WOSUM2_C-RP10AQ    = IT_WOSUM2-RP10AQ .
      IT_WOSUM2_C-RP11AQ    = IT_WOSUM2-RP11AQ .
      IT_WOSUM2_C-RP12AQ    = IT_WOSUM2-RP12AQ .
      IT_WOSUM2_C-RP13AQ    = IT_WOSUM2-RP13AQ .
      IT_WOSUM2_C-RP14AQ    = IT_WOSUM2-RP14AQ .
      IT_WOSUM2_C-RP15AQ    = IT_WOSUM2-RP15AQ .
      IT_WOSUM2_C-RP16AQ    = IT_WOSUM2-RP16AQ .
      COLLECT IT_WOSUM2_C. CLEAR IT_WOSUM2_C.
    ENDLOOP.


    LOOP AT IT_DAILY.
      READ TABLE IT_WOSUM2_C WITH KEY WO_SER     = IT_DAILY-WO_SER
                                      NATION     = IT_DAILY-NATION
                                      DEALER     = IT_DAILY-DEALER
                                      EXTC       = IT_DAILY-EXTC
                                      INTC       = IT_DAILY-INTC
                                      BINARY SEARCH.
      IF SY-SUBRC = 0.
        MOVE-CORRESPONDING IT_DAILY TO IT_LIST.
        IT_LIST-RP01MQ    = IT_LIST-RP01AQ + IT_WOSUM2_C-RP01AQ .
        IT_LIST-RP02MQ    = IT_LIST-RP02AQ + IT_WOSUM2_C-RP02AQ .
        IT_LIST-RP03MQ    = IT_LIST-RP03AQ + IT_WOSUM2_C-RP03AQ .
        IT_LIST-RP04MQ    = IT_LIST-RP04AQ + IT_WOSUM2_C-RP04AQ .
        IT_LIST-RP05MQ    = IT_LIST-RP05AQ + IT_WOSUM2_C-RP05AQ .
        IT_LIST-RP06MQ    = IT_LIST-RP06AQ + IT_WOSUM2_C-RP06AQ .
        IT_LIST-RP07MQ    = IT_LIST-RP07AQ + IT_WOSUM2_C-RP07AQ .
        IT_LIST-RP08MQ    = IT_LIST-RP08AQ + IT_WOSUM2_C-RP08AQ .
        IT_LIST-RP09MQ    = IT_LIST-RP09AQ + IT_WOSUM2_C-RP09AQ .
        IT_LIST-RP10MQ    = IT_LIST-RP10AQ + IT_WOSUM2_C-RP10AQ .
        IT_LIST-RP11MQ    = IT_LIST-RP11AQ + IT_WOSUM2_C-RP11AQ .
        IT_LIST-RP12MQ    = IT_LIST-RP12AQ + IT_WOSUM2_C-RP12AQ .
        IT_LIST-RP13MQ    = IT_LIST-RP13AQ + IT_WOSUM2_C-RP13AQ .
        IT_LIST-RP14MQ    = IT_LIST-RP14AQ + IT_WOSUM2_C-RP14AQ .
        IT_LIST-RP15MQ    = IT_LIST-RP15AQ + IT_WOSUM2_C-RP15AQ .
        IT_LIST-RP16MQ    = IT_LIST-RP16AQ + IT_WOSUM2_C-RP16AQ .
        APPEND IT_LIST. CLEAR IT_LIST.
      ELSE.
        MOVE-CORRESPONDING IT_DAILY TO IT_LIST.
        APPEND IT_LIST. CLEAR IT_LIST.
      ENDIF.
    ENDLOOP.
    LOOP AT IT_LIST.
      DELETE FROM ZTPP_WOSUM2  WHERE CR_DATE = IT_LIST-CR_DATE
                                 AND WO_SER  = IT_LIST-WO_SER
                                 AND NATION  = IT_LIST-NATION
                                 AND DEALER  = IT_LIST-DEALER
                                 AND EXTC    = IT_LIST-EXTC
                                 AND INTC    = IT_LIST-INTC.
    ENDLOOP.
    COMMIT WORK.
    INSERT ZTPP_WOSUM2 FROM TABLE IT_LIST ACCEPTING DUPLICATE KEYS .
    IF SY-SUBRC = 0.
      IF SY-BATCH NE 'X'.
        MESSAGE  S001 WITH 'Completed Update successfully !'.
      ENDIF.
    ELSE.
      ROLLBACK WORK.
      IF SY-BATCH NE 'X'.
        MESSAGE  E001 WITH 'Failed in Update !'.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " CREATE_LIST
