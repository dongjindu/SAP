************************************************************************
* Author                 : jun ho choi
* Creation Date          : 2003-09-01
* Specifications By      :
* Development Request No : UD1K901594
* Pattern                : 3-1
* Addl documentation     :
* Description            : Vehicle Order Interface.
*
*
*
* Modification Log
* Date       Developer    Request ID Description
*
************************************************************************
REPORT ZISD03U_VEHICLE_ORDER NO STANDARD PAGE HEADING
                             MESSAGE-ID ZMSD
                             LINE-SIZE 168.


*
TABLES : ZTPP_WOSUM,
         ZTSD_VEH_OR,
         USR01,
         MARA,
         VBAP,
         KNVV,
         ZTPP_COMMON_VALS.


*
DATA : BEGIN OF IT_WOSUM OCCURS 0.
       INCLUDE STRUCTURE ZTPP_WOSUM.
DATA : END OF IT_WOSUM.

DATA : BEGIN OF IT_VEH_OR OCCURS 0.
       INCLUDE STRUCTURE ZTSD_VEH_OR.
DATA : END OF IT_VEH_OR.

DATA : BEGIN OF BDC_TAB OCCURS 0.
       INCLUDE STRUCTURE BDCDATA.
DATA : END OF BDC_TAB.

DATA : BEGIN OF MESS_TAB OCCURS 0.
       INCLUDE STRUCTURE BDCMSGCOLL.
DATA : END OF MESS_TAB.

DATA : BEGIN OF BDC_LIST OCCURS 0,
       GUBUN_S(1),
       GUBUN_W(1),
       GUBUN_M(1),
       MESSAGE(75),

       WO_SER LIKE ZTPP_WOSUM-WO_SER,
       NATION LIKE ZTPP_WOSUM-NATION,
       DEALER LIKE ZTPP_WOSUM-DEALER,
       EXTC   LIKE ZTPP_WOSUM-EXTC,
       INTC   LIKE ZTPP_WOSUM-INTC,
       MODQTY LIKE ZTPP_WOSUM-MODQTY,
       FSC    LIKE ZTPP_WOSUM-FSC,
       SALES  LIKE ZTPP_WOSUM-SALES,
       END OF BDC_LIST.

* BAPI
DATA : BEGIN OF ORDER_HEADER_INX.
       INCLUDE STRUCTURE BAPISDH1X.
DATA : END OF ORDER_HEADER_INX.

DATA : BEGIN OF RETURN OCCURS 0.
       INCLUDE STRUCTURE BAPIRET2.
DATA : END OF RETURN.

DATA : BEGIN OF ORDER_ITEM_IN OCCURS 0.
       INCLUDE STRUCTURE BAPISDITM.
DATA : END OF ORDER_ITEM_IN.

DATA : BEGIN OF ORDER_ITEM_INX OCCURS 0.
       INCLUDE STRUCTURE BAPISDITMX.
DATA : END OF ORDER_ITEM_INX.

DATA : BEGIN OF SCHEDULE_LINES OCCURS 0.
       INCLUDE STRUCTURE BAPISCHDL.
DATA : END OF SCHEDULE_LINES.

DATA : BEGIN OF SCHEDULE_LINESX OCCURS 0.
       INCLUDE STRUCTURE BAPISCHDLX.
DATA : END OF SCHEDULE_LINESX.
* BAPI

DATA : W_CNT TYPE I,
       W_CNT_S TYPE I,
       W_CNT_E TYPE I,
       W_DATE(6) TYPE N,
       W_CHAR_5(5),
       W_CHAR_MOD(10),
       W_CHAR_SEQ(10),
       W_CHAR_20(20),
       W_VBELN LIKE VBAK-VBELN,
       W_INDEX LIKE SY-TABIX.

DATA : VARIANT LIKE INDX-SRTFD VALUE 'ZISD03_01'.

DATA : snd_jobs TYPE i VALUE 1,
       rcv_jobs TYPE i VALUE 1,
       excp_flag(1),
       w_taskname(4) TYPE n VALUE '0001',
       w_classname LIKE rzllitab-classname VALUE 'parallel_generators'.


*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS : P_DATE LIKE SY-DATUM.
SELECTION-SCREEN SKIP 1.
PARAMETERS : P_MODE(1) DEFAULT 'N' NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK B1.


*
INITIALIZATION.
  SELECT SINGLE DATES
                INTO P_DATE
                FROM ZTPP_COMMON_VALS
               WHERE JOBS = 'ZAPP703C_WORKORDER_MAINT'.
  IF SY-SUBRC = 0.
    P_DATE = P_DATE - 1.
  ELSE.
    P_DATE = SY-DATUM.
  ENDIF.


*
TOP-OF-PAGE.
  PERFORM TOP_OF_PAGE.


*
TOP-OF-PAGE DURING LINE-SELECTION.
  PERFORM TOP_OF_PAGE.


*
START-OF-SELECTION.
  PERFORM GET_DATA.
  PERFORM BDC_PROCESS.
  PERFORM DISPLAY_RESULT.
  PERFORM SUBMIT_ZISD15.


*
END-OF-SELECTION.


*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
FORM GET_DATA.
  IF SY-BATCH EQ 'X'.
    IMPORT P_DATE FROM DATABASE INDX(ZZ) ID VARIANT.
  ENDIF.

  REFRESH : IT_WOSUM.
  CLEAR   : IT_WOSUM.

  SELECT *
         INTO TABLE IT_WOSUM
         FROM ZTPP_WOSUM
        WHERE WOMODDATE GT P_DATE.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  BDC_PROCESS
*&---------------------------------------------------------------------*
FORM BDC_PROCESS.
  DESCRIBE TABLE IT_WOSUM LINES W_CNT.
  IF W_CNT = 0.
    SKIP 5.
    WRITE:/ 'No Entry'.
    STOP.
  ENDIF.

  REFRESH : BDC_LIST.
  CLEAR   : BDC_LIST.

  LOOP AT IT_WOSUM.
    PERFORM CHECK_EXIT_IN_ZTSD_VEH_OR.
    IF SY-SUBRC EQ 0.
      PERFORM CHECK_QTY_IN_ZTSD_VEH_OR.
      IF SY-SUBRC <> 0.
        CONTINUE.
      ENDIF.
    ELSE.
      PERFORM CHECK_QTY_IN_VBAP.
      IF SY-SUBRC <> 0.
        CONTINUE.
      ENDIF.
    ENDIF.

    PERFORM MOVE_IT_WOSUM_2_ZTSD_VEH_OR.

    PERFORM job_parallel.

    APPEND BDC_LIST. CLEAR BDC_LIST.
  ENDLOOP.

  WAIT UNTIL rcv_jobs >= snd_jobs UP TO '100' SECONDS. "?

ENDFORM.                    " BDC_PROCESS
*&---------------------------------------------------------------------*
*&      Form  CHECK_EXIT_IN_ZTSD_VEH_OR
*&---------------------------------------------------------------------*
FORM CHECK_EXIT_IN_ZTSD_VEH_OR.
  SELECT SINGLE *
         FROM ZTSD_VEH_OR
        WHERE WO_SER = IT_WOSUM-WO_SER
        AND   NATION = IT_WOSUM-NATION
        AND   DEALER = IT_WOSUM-DEALER
        AND   EXTC   = IT_WOSUM-EXTC
        AND   INTC   = IT_WOSUM-INTC.
ENDFORM.                    " CHECK_EXIT_IN_ZTSD_VEH_OR
*&---------------------------------------------------------------------*
*&      Form  CHECK_QTY_IN_ZTSD_VEH_OR
*&---------------------------------------------------------------------*
FORM CHECK_QTY_IN_ZTSD_VEH_OR.
  SY-SUBRC = 0.
  IF ZTSD_VEH_OR-MODQTY = IT_WOSUM-MODQTY AND
     ZTSD_VEH_OR-SEQQTY = IT_WOSUM-SEQQTY.
    SY-SUBRC = 4.
  ENDIF.
ENDFORM.                    " CHECK_QTY_IN_ZTSD_VEH_OR
*&---------------------------------------------------------------------*
*&      Form  CHECK_QTY_IN_VBAP
*&---------------------------------------------------------------------*
FORM CHECK_QTY_IN_VBAP.
  DATA : W_10 LIKE IT_WOSUM-SEQQTY,
         W_20 LIKE IT_WOSUM-MODQTY.

  SY-SUBRC = 0.
  IF NOT IT_WOSUM-SALES IS INITIAL.
    CLEAR : W_10, W_20.

    SELECT *
           FROM VBAP
          WHERE VBELN EQ IT_WOSUM-SALES.
      CASE VBAP-POSNR.
        WHEN '000010'.
          W_10 = VBAP-KWMENG.
        WHEN '000020'.
          W_20 = VBAP-KWMENG.
      ENDCASE.
    ENDSELECT.

    W_20 = W_20 + W_10.

    IF W_10 EQ IT_VEH_OR-SEQQTY AND
       W_20 EQ IT_VEH_OR-MODQTY.
      SY-SUBRC = 4.
    ELSE.
*   Quantity decrease is not allowed
      IF IT_VEH_OR-WO_SER+1(4) NE SY-DATUM+2(4) AND
         W_20 < IT_VEH_OR-MODQTY.
        SY-SUBRC = 1.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " CHECK_QTY_IN_VBAP
*&---------------------------------------------------------------------*
*&      Form  MOVE_IT_WOSUM_2_ZTSD_VEH_OR
*&---------------------------------------------------------------------*
FORM MOVE_IT_WOSUM_2_ZTSD_VEH_OR.
  CLEAR ZTSD_VEH_OR.

  ZTSD_VEH_OR-PDATE  = P_DATE.
  ZTSD_VEH_OR-WO_SER = IT_WOSUM-WO_SER.
  ZTSD_VEH_OR-NATION = IT_WOSUM-NATION.
  ZTSD_VEH_OR-DEALER = IT_WOSUM-DEALER.
  ZTSD_VEH_OR-EXTC   = IT_WOSUM-EXTC.
  ZTSD_VEH_OR-INTC   = IT_WOSUM-INTC.
  ZTSD_VEH_OR-SEQQTY = IT_WOSUM-SEQQTY.
  ZTSD_VEH_OR-MODQTY = IT_WOSUM-MODQTY.
  ZTSD_VEH_OR-FSC    = IT_WOSUM-FSC.
  ZTSD_VEH_OR-SALES  = IT_WOSUM-SALES.

  MODIFY ZTSD_VEH_OR.
  COMMIT WORK.
*
  BDC_LIST-WO_SER = IT_WOSUM-WO_SER.
  BDC_LIST-NATION = IT_WOSUM-NATION.
  BDC_LIST-DEALER = IT_WOSUM-DEALER.
  BDC_LIST-EXTC   = IT_WOSUM-EXTC.
  BDC_LIST-INTC   = IT_WOSUM-INTC.
  BDC_LIST-MODQTY = IT_WOSUM-MODQTY.
  BDC_LIST-FSC    = IT_WOSUM-FSC.
  BDC_LIST-SALES  = IT_WOSUM-SALES.
ENDFORM.                    " MOVE_IT_WOSUM_2_ZTSD_VEH_OR
*&---------------------------------------------------------------------*
*&      Form  JOB_PARALLEL
*&---------------------------------------------------------------------*
FORM job_parallel.
  DO.
    REFRESH IT_VEH_OR.
    MOVE-CORRESPONDING ZTSD_VEH_OR TO IT_VEH_OR.
    APPEND IT_VEH_OR. CLEAR IT_VEH_OR.

    CALL FUNCTION 'Z_FSD_VEHICLE_ORDER'
      STARTING NEW TASK w_taskname
               DESTINATION IN GROUP w_classname
      PERFORMING return_01 ON END OF TASK
      TABLES
        it_VEH_OR             = it_VEH_OR
      EXCEPTIONS
        communication_failure = 1
        system_failure        = 2
        RESOURCE_FAILURE      = 3
        OTHERS                = 4.

    CASE sy-subrc.
      WHEN 0.
        w_taskname = w_taskname + 1.
        snd_jobs = snd_jobs  + 1.
        EXIT.
      WHEN 1 OR 2.
        excp_flag = 'X'.
*       EXIT.
      WHEN 3.
*       Receive reply to asynchronous RFC calls
        IF excp_flag = space.
          excp_flag = 'X'.
*         First attempt for RESOURCE_Failure handling
          WAIT UNTIL rcv_jobs >= snd_jobs UP TO '0.01' SECONDS.
        ELSE.
*         Second attempt for RESOURCE_Failure handling
          WAIT UNTIL rcv_jobs >= snd_jobs UP TO '0.1' SECONDS.
        ENDIF.
        IF sy-subrc = 0.
          CLEAR excp_flag. " Reset flag
        ENDIF.
    ENDCASE.
  ENDDO.
ENDFORM.                    " JOB_PARALLEL
*&---------------------------------------------------------------------*
*&      Form  RETURN_01
*&---------------------------------------------------------------------*
FORM return_01 USING p_taskname.

  RECEIVE RESULTS FROM FUNCTION 'Z_FSD_VEHICLE_ORDER'
    TABLES
      it_VEH_OR             = it_VEH_OR
    EXCEPTIONS
      communication_failure = 1
      system_failure        = 2
      RESOURCE_FAILURE      = 3
      OTHERS                = 4.

  CHECK sy-subrc = 0.

  READ TABLE IT_VEH_OR INDEX 1.

  BDC_LIST-GUBUN_S = IT_VEH_OR-GUBUN_S.
  BDC_LIST-GUBUN_W = IT_VEH_OR-GUBUN_W.
  BDC_LIST-GUBUN_M = IT_VEH_OR-GUBUN_M.
  BDC_LIST-MESSAGE = IT_VEH_OR-MESSAGE.
  BDC_LIST-SALES   = IT_VEH_OR-SALES.

  rcv_jobs = rcv_jobs + 1.
ENDFORM.                                                    " RETURN_01
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_RESULT
*&---------------------------------------------------------------------*
FORM DISPLAY_RESULT.
  WRITE:/ ''.
  LOOP AT BDC_LIST.
    WRITE:/ SY-VLINE.
    CASE BDC_LIST-GUBUN_S.
      WHEN 'S'.
        FORMAT COLOR COL_POSITIVE.
        WRITE: (02) BDC_LIST-GUBUN_S.
        FORMAT COLOR COL_POSITIVE OFF.
      WHEN 'W'.
        FORMAT COLOR COL_GROUP.
        WRITE: (02) BDC_LIST-GUBUN_S.
        FORMAT COLOR COL_GROUP OFF.
      WHEN 'E'.
        FORMAT COLOR COL_NEGATIVE.
        WRITE: (02) BDC_LIST-GUBUN_S.
        FORMAT COLOR COL_NEGATIVE OFF.
      WHEN OTHERS.
        WRITE: (02) ''.
    ENDCASE.

    WRITE:  SY-VLINE.
    CASE BDC_LIST-GUBUN_W.
      WHEN 'S'.
        FORMAT COLOR COL_POSITIVE.
        WRITE: (02) BDC_LIST-GUBUN_W.
        FORMAT COLOR COL_POSITIVE OFF.
      WHEN 'W'.
        FORMAT COLOR COL_GROUP.
        WRITE: (02) BDC_LIST-GUBUN_W.
        FORMAT COLOR COL_GROUP OFF.
      WHEN 'E'.
        FORMAT COLOR COL_NEGATIVE.
        WRITE: (02) BDC_LIST-GUBUN_W.
        FORMAT COLOR COL_NEGATIVE OFF.
      WHEN OTHERS.
        WRITE: (02) ''.
    ENDCASE.

    WRITE:  SY-VLINE.
    CASE BDC_LIST-GUBUN_M.
      WHEN 'S'.
        FORMAT COLOR COL_POSITIVE.
        WRITE: (02) BDC_LIST-GUBUN_M.
        FORMAT COLOR COL_POSITIVE OFF.
      WHEN 'W'.
        FORMAT COLOR COL_GROUP.
        WRITE: (02) BDC_LIST-GUBUN_M.
        FORMAT COLOR COL_GROUP OFF.
      WHEN 'E'.
        FORMAT COLOR COL_NEGATIVE.
        WRITE: (02) BDC_LIST-GUBUN_M.
        FORMAT COLOR COL_NEGATIVE OFF.
      WHEN OTHERS.
        WRITE: (02) ''.
    ENDCASE.

    CONCATENATE BDC_LIST-WO_SER BDC_LIST-NATION BDC_LIST-DEALER
                INTO W_CHAR_20.

    WRITE:  SY-VLINE, (14) W_CHAR_20+0(14),
            SY-VLINE, (03) BDC_LIST-EXTC,
            SY-VLINE, (03) BDC_LIST-INTC,
            SY-VLINE, (07) BDC_LIST-MODQTY,
            SY-VLINE, (18) BDC_LIST-FSC,
            SY-VLINE, (10) BDC_LIST-SALES,
            SY-VLINE, (75) BDC_LIST-MESSAGE,
            SY-VLINE.

    W_INDEX = SY-TABIX.
    HIDE : W_INDEX.

    WRITE:/(167) SY-ULINE.
  ENDLOOP.

ENDFORM.                    " DISPLAY_RESULT
*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM TOP_OF_PAGE.
  DESCRIBE TABLE BDC_LIST LINES W_CNT.
  WRITE:/ 'Total   records :', W_CNT.

  W_CNT_S = 0. W_CNT_E = 0.
  LOOP AT BDC_LIST.
    IF BDC_LIST-GUBUN_S = 'S'.
      W_CNT_S = W_CNT_S + 1.
    ENDIF.
    IF BDC_LIST-GUBUN_S <> 'S'.
      W_CNT_E = W_CNT_E + 1.
    ENDIF.
  ENDLOOP.
  WRITE:/ 'Success records :', W_CNT_S.
  WRITE:/ 'Error   records :', W_CNT_E.

  FORMAT COLOR COL_HEADING.
  WRITE:/(167) SY-ULINE.
  WRITE:/ SY-VLINE, (02) 'SO',
          SY-VLINE, (02) 'WS',
          SY-VLINE, (02) 'WC',
          SY-VLINE, (14) 'W/O Number',
          SY-VLINE, (03) 'Ext',
          SY-VLINE, (03) 'Int',
          SY-VLINE, (07) 'Mod Qty',
          SY-VLINE, (18) 'FSC',
          SY-VLINE, (10) 'SalesOrder',
          SY-VLINE, (75) 'Message',
          SY-VLINE.
  WRITE:/(167) SY-ULINE.
  FORMAT COLOR COL_HEADING OFF.
ENDFORM.                    " TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  SUBMIT_ZISD15
*&---------------------------------------------------------------------*
FORM SUBMIT_ZISD15.
  DATA : EVENTID LIKE TBTCJOB-EVENTID,
         VARIANT LIKE INDX-SRTFD VALUE 'ZISD15_01'.

  EXPORT P_DATE TO DATABASE INDX(ZZ) ID VARIANT.

  EVENTID = 'ZISD15_01'.

  CALL FUNCTION 'BP_EVENT_RAISE'
    EXPORTING
      EVENTID                      = EVENTID
*     EVENTPARM                    = ' '
*     TARGET_INSTANCE              = ' '
    EXCEPTIONS
      BAD_EVENTID                  = 1
      EVENTID_DOES_NOT_EXIST       = 2
      EVENTID_MISSING              = 3
      RAISE_FAILED                 = 4
      OTHERS                       = 5.
ENDFORM.                    " SUBMIT_ZISD15
