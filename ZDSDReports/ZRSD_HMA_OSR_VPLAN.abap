************************************************************************
* Program Name      : ZAPP805R_PLAN_REFRESH
* Author            : Bobby
* Creation Date     : 2003.12.23.
* Specifications By : Bobby
* Pattern           : 2.1
* Development Request No : UD1K90228
* Addl Documentation:
* Description       : Plan Order Refresh
*
* Modification Logs
*   Date         Developer    RequestNo    Description
*#1 02/23/2005   100701       UD1K914664
* Change : Change the Time Tule(Tack-Time --> Lead Time)
*#2 02/25/2005   wskim        UD1K914664   Rescheduling
*#3 02/28/2005   wskim        UD1K914687   Reschduling - include
*                                           table  ZTPP_PMT07JB_C
*#4 04/07/2005   chris        UD1K915416   Add logic to use either trim
*                                          input or body input schedule
**               FR Wang                   Check plan open date < =
*                                start date
*#5 11/17/2010   sjlee ZSDAT, ZSTIM = SY-DATUM , SY-UZEIT
************************************************************************
REPORT  ZAPP805R_PLAN_REFRESH   MESSAGE-ID ZMPP.

*----------------------------------------------------------------------
* TABLES DECLARATION
*----------------------------------------------------------------------
TABLES: ZTPP_INPUT_OSR,AUSP , USR01,ZTPP_PMT07JB_A.


*----------------------------------------------------------------------
* INTERNAL-TABLES DECLARATION
*----------------------------------------------------------------------
DATA: BEGIN OF IT_DATA        OCCURS 0,
        OBJEK                 LIKE AUSP-OBJEK.      " Vehicle Code
        INCLUDE STRUCTURE     ZTPP_INPUT_OSR.
DATA: END OF IT_DATA.

DATA: BEGIN OF IT_MASTER      OCCURS 0,
        SEQ                   TYPE I  ,             " Sequence
        DATE                  TYPE D  ,             " Date
        DAY                   LIKE KAPA-TAGNR,      " Day
        SHIFT                 LIKE KAPA-SCHNR,      " Shift
        TIME                  TYPE KAPENDZT  ,      " Times for working
        TUN                   TYPE LD_LANTU  ,      " Unit  for Time
        UPH                   TYPE ZVPP_LD-LRATE,   " UPH
      END OF IT_MASTER.

DATA: IT_DISP            LIKE TABLE OF ZTPP_ALC_BINPUT WITH HEADER LINE.
*---Start #3 wskim 02/28/2005
DATA: IT_7JB LIKE ZTPP_PMT07JB_A OCCURS 0 WITH HEADER LINE,
      IT_CFILE    LIKE TABLE OF ZTPP_PMT07JB_C WITH HEADER LINE,
      IT_CFILE2  LIKE TABLE OF ZTPP_PMT07JB_C WITH HEADER LINE.
*---end
*--->added by chris on 04/27/2005
DATA: IT_INPUTPLAN LIKE ZTPP_INPUT_OSR OCCURS 0 WITH HEADER LINE.
*--->end

*----------------------------------------------------------------------
* WORKING-AREA DECLARATION
*----------------------------------------------------------------------
DATA: WA_DISP             LIKE IT_DISP                             ,
      WA_DATA             LIKE IT_DATA                             ,
      WA_WDATE            LIKE SY-DATUM                            ,
      WA_KALID            LIKE KAKO-KALID                          ,
      WA_UZEIT            LIKE SY-UZEIT                            ,
      WA_INDEX            LIKE SY-TABIX                            ,
      WA_SND_JOBS         TYPE I                                   ,
      WA_RCV_JOBS         TYPE I                                   ,
      WA_TASKNAME(4)      TYPE N VALUE '0001'                      ,
      WA_EXCP_FLAG        TYPE C                                   ,
      WA_ERROR            TYPE C                                   ,
      WA_FLAG             TYPE C                                   ,
      WA_HOUR             TYPE I                                   .
*---start #2 wskim
DATA : F_COUNT TYPE I.
*---end
DATA : IT_HOLD  LIKE ZTPP_HOLD_CAR OCCURS 0 WITH HEADER LINE.
DATA : IT_HOLD_SCH LIKE ZTPP_HOLD_CAR OCCURS 0 WITH HEADER LINE.
DATA : G_LAST_SEQ_DATE   LIKE SY-DATUM.
DATA : G_SHOP LIKE CRHD-ARBPL.
DATA : BEGIN OF IT_CABN OCCURS 0,
        ATNAM   LIKE CABN-ATNAM,
        ATINN   LIKE CABN-ATINN,
       END OF IT_CABN.
*----------------------------------------------------------------------
* FIELD-SYMBOLS DECLARATION
*----------------------------------------------------------------------
FIELD-SYMBOLS: <WA_DFIELD>    TYPE ANY.

*----------------------------------------------------------------------
* SCREEN-VARIABLES DECLARATION
*----------------------------------------------------------------------

*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-002.
PARAMETERS: P_DATES  TYPE D   DEFAULT SY-DATUM    OBLIGATORY.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS : P_TRIM RADIOBUTTON GROUP RD.
SELECTION-SCREEN COMMENT 10(25) TEXT-012 FOR FIELD P_TRIM.
PARAMETERS: P_BODY RADIOBUTTON GROUP RD DEFAULT 'X',
            P_LOG       DEFAULT 'X' NO-DISPLAY.
SELECTION-SCREEN COMMENT 40(25) TEXT-011 FOR FIELD P_BODY.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B1.

*----------------------------------------------------------------------
* INCLUDE PROGRAM.....
*----------------------------------------------------------------------

*----------------------------------------------------------------------
AT SELECTION-SCREEN.
*----------------------------------------------------------------------
* set the shop for schedule
  PERFORM SET_SHOP.
  " Get the Date for the Production Reporting Date(Last Date)
  WA_WDATE = P_DATES - 1.
  PERFORM READ_SHOP_CALID   USING WA_KALID.
  PERFORM READ_WORKING_DATE USING '-'  WA_KALID  WA_WDATE.
  PERFORM GET_DAY          USING WA_WDATE IT_MASTER-DAY .
  PERFORM GET_WORKING_TIME USING WA_WDATE IT_MASTER-TIME IT_MASTER-DAY .
  PERFORM GET_UPH          USING WA_WDATE IT_MASTER-UPH
                                 IT_MASTER-SHIFT  G_SHOP.
  IT_MASTER-SEQ    = 99.
  IT_MASTER-DATE = WA_WDATE .
  APPEND IT_MASTER.  CLEAR: IT_MASTER.

*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------
  PERFORM SET_INFORMATION USING P_DATES.
  PERFORM READ_INPUTPLAN .
  PERFORM CREATE_SUMMARY .
*----start#2 wskim 02/25/2005
  PERFORM RESCHEDULING_INPUTPLAN.
*  PERFORM recreate_ztpp_pmt07jb_c.
*----end


*----requested by MY HUR changed by chris
*    schedule the body input plan by body shop UPH
*    and update to field RD01 in ZTPP_INPUT_OSR
  "set the body shop flag
  P_BODY = 'X'.
*    set shop for shcedule and read the UPH and time
  PERFORM GET_BODY_SHOP_DATA.


*----end of change on 04/27/2005
*----------------------------------------------------------------------
END-OF-SELECTION.
*----------------------------------------------------------------------



*&---------------------------------------------------------------------*
*&      Form  READ_INPUTPLAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_INPUTPLAN.
  DATA: L_COUNT(05) TYPE N.
  DATA: L_TEXT(30).
  DATA: WA_DATA LIKE IT_DATA.
  DATA: L_TABIX TYPE I.

  IF G_SHOP = 'B'.

* by Daniel on 04/15/2011 {
*    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_DATA
*      FROM ZTPP_INPUT_OSR
*     WHERE STATUS = '00'.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_DATA
      FROM ZTPP_INPUT_OSR
     WHERE STATUS = '00' AND RS18 = 'A'.
* }

  ELSEIF G_SHOP = 'T'.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_DATA
     FROM ZTPP_INPUT_OSR
    WHERE STATUS IN ('00','01','02','03','04','05')
      AND MITU NE 'Y'.
*    excluding the 'XX' 'XY' cars for status 01--05
    LOOP AT IT_DATA WHERE STATUS NE '00'.
      IF IT_DATA-WORK_ORDER+12(2) = 'XX' OR
         IT_DATA-WORK_ORDER+12(2) = 'XY' .
        DELETE IT_DATA.
      ENDIF.
    ENDLOOP.

  ENDIF.

  DESCRIBE TABLE IT_DATA LINES  WA_HOUR .
  IF WA_HOUR = 0.
*OSR!!!
*    DELETE FROM ZTPP_ALC_BINPUT CLIENT SPECIFIED WHERE MANDT = SY-MANDT
*.
*    LEAVE PROGRAM .
  ENDIF.

  SORT IT_DATA BY SERIAL .
* reading all waiting holding cars
  PERFORM READ_HOLD_CARS.

* output the qty of each status for verify and check
  SORT IT_DATA BY STATUS DESCENDING.
  LOOP AT IT_DATA.
    L_COUNT = L_COUNT + 1.
    L_TABIX = SY-TABIX + 1.
    CLEAR: WA_DATA.
    READ TABLE IT_DATA INTO WA_DATA INDEX L_TABIX .
    IF WA_DATA-STATUS NE IT_DATA-STATUS.

      CONCATENATE IT_DATA-STATUS 'status total :' INTO L_TEXT
         SEPARATED BY SPACE.
      IF P_LOG EQ 'X'.
        WRITE:/ L_TEXT , L_COUNT.
      ENDIF.
      CLEAR: L_COUNT.
    ENDIF.
  ENDLOOP.
** changed by furong on 08/16/2005
  SORT IT_DATA BY SERIAL.
** end of change
ENDFORM.                    " READ_INPUTPLAN

*&---------------------------------------------------------------------*
*&      Form  CREATE_SUMMARY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_SUMMARY.
  DATA: L_WORDER             LIKE MARA-MATNR,
        L_FLAG               TYPE C ,
        L_MAX                TYPE I ,
        L_HOURS              TYPE I ,
        L_POS                TYPE I ,
        L_POS1               TYPE I ,
        L_SERIAL             LIKE IT_DATA-BODY_SER,
        L_COUNT              TYPE I ,
        F_NUM                TYPE I ,
        L_LOOP               TYPE I ,
        L_SKIP               TYPE C ,
        L_HOLD               TYPE C ,
        L_YN                 TYPE C ,
        L_CHK                TYPE P DECIMALS 3,
        L_TABIX              LIKE SY-TABIX,
        L_INDEX              LIKE SY-TABIX.

* requested by my HUR changed by chris
* refresh and reschedule the holding cars
*  PERFORM REFRESH_HOLD_CARS.
* end of change on 04/08/2005

  DESCRIBE TABLE IT_DATA LINES L_MAX.
  L_LOOP = L_INDEX = 1.

  " First Day Data...
  SORT IT_MASTER BY SEQ .
  LOOP AT IT_MASTER WHERE SEQ > 80 AND SEQ < 90 .
    IF L_INDEX > L_MAX.
      EXIT.
    ENDIF.
    L_CHK = IT_MASTER-TIME / 3600 .
    L_POS1 = IT_MASTER-UPH * L_CHK.
*   check how many holding cars has been scheduled for this date
    PERFORM CHECK_COUNT USING L_COUNT
                              IT_MASTER-DATE.

    L_POS1 = L_POS1 - L_COUNT.          "deduct the shceduled qty
    L_COUNT = L_POS1.
    LOOP AT IT_DATA FROM L_INDEX TO L_MAX.
      IF L_COUNT = 0.
        EXIT.
      ENDIF.
*     if the car is xx xy car don't count it.
      PERFORM CHECK_XXXY USING IT_DATA L_YN.
*     if the car is holding car skip it
      PERFORM CHECK_HOLD_CAR USING IT_DATA IT_MASTER-DATE L_HOLD.
      IF L_HOLD = 'N'.    "Not a holding car, do refresh
        PERFORM REFRESH_DATE_PLAF  USING IT_MASTER-DATE .
        IF L_YN = 'N' OR G_SHOP = 'B'. "Don't count XX XY car
          L_COUNT = L_COUNT - 1.
          G_LAST_SEQ_DATE = IT_MASTER-DATE.
          F_COUNT = L_COUNT.
        ENDIF.
      ENDIF.
      F_NUM = F_NUM + 1.
    ENDLOOP.
    L_INDEX = L_INDEX + F_NUM .     "next begin postion
    CLEAR F_NUM.
  ENDLOOP.

  " Daily Data...

  DO 21 TIMES.
    L_TABIX = L_TABIX + 1 .
    READ TABLE IT_MASTER WITH KEY SEQ = L_TABIX.
    IF L_INDEX > L_MAX.
      EXIT.
    ENDIF.
    L_CHK = IT_MASTER-TIME / 3600 .
    L_POS =  IT_MASTER-UPH * L_CHK  .
*   check how many holding cars has been scheduled for this date
    PERFORM CHECK_COUNT USING L_COUNT
                              IT_MASTER-DATE.

    L_POS = L_POS - L_COUNT.          "deduct the shceduled qty
    L_COUNT = L_POS.

    LOOP AT IT_DATA FROM L_INDEX TO L_MAX.
      IF L_COUNT = 0.
        EXIT.
      ENDIF.
*     if the car is xx xy car don't count it.
      PERFORM CHECK_XXXY USING IT_DATA L_YN.

*     if the car is holding car skip it
      PERFORM CHECK_HOLD_CAR USING IT_DATA IT_MASTER-DATE L_HOLD.
      IF L_HOLD = 'N'.    "Not a holding car, do refresh
        PERFORM REFRESH_DATE_PLAF  USING IT_MASTER-DATE .
        IF G_SHOP = 'B' OR
          L_YN = 'N'.               "xx xy car don't count
          L_COUNT = L_COUNT - 1.
*         recording the last date scheduled
          G_LAST_SEQ_DATE  = IT_MASTER-DATE.
          F_COUNT = L_COUNT.       "qty left
        ENDIF.
      ENDIF.

      F_NUM = F_NUM + 1.
    ENDLOOP.

    L_INDEX = L_INDEX + F_NUM .
    CLEAR: F_NUM.
  ENDDO.
  WAIT UNTIL WA_RCV_JOBS >= WA_SND_JOBS.
ENDFORM.                    " CREATE_SUMMARY

*&---------------------------------------------------------------------*
*&      Form  READ_WORKING_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
*----------------------------------------------------------------------*
FORM READ_WORKING_DATE USING  PA_TYPE  PA_KALID  PA_WDATE.
  CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
       EXPORTING
            CORRECT_OPTION               = PA_TYPE
            DATE                         = PA_WDATE
            FACTORY_CALENDAR_ID          = PA_KALID
       IMPORTING
            DATE                         = PA_WDATE
       EXCEPTIONS
            CALENDAR_BUFFER_NOT_LOADABLE = 1
            CORRECT_OPTION_INVALID       = 2
            DATE_AFTER_RANGE             = 3
            DATE_BEFORE_RANGE            = 4
            DATE_INVALID                 = 5
            FACTORY_CALENDAR_NOT_FOUND   = 6
            OTHERS                       = 7.
ENDFORM.                    " READ_WORKING_DATE

*&---------------------------------------------------------------------*
*&      Form  READ_SHOP_CALID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_KALID  text
*----------------------------------------------------------------------*
FORM READ_SHOP_CALID USING    PA_KALID.
  SELECT SINGLE KALID INTO PA_KALID
    FROM ZVPP_CAPACITY
   WHERE ARBPL = G_SHOP   .
ENDFORM.                    " READ_SHOP_CALID

*&---------------------------------------------------------------------*
*&      Form  SET_INFORMATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_INFORMATION USING P_DATES.
  DATA: L_DATE               TYPE D ,
        L_COUNT              TYPE I .

  " Set the BASIC Information for the UPH & Work Time...
  CLEAR: L_COUNT.                 L_DATE = P_DATES  .
  PERFORM READ_WORKING_DATE USING '+'  WA_KALID  L_DATE .
*  IF L_DATE = P_DATES .
    IT_MASTER-SEQ    = '80'.       IT_MASTER-DATE   = L_DATE .
    PERFORM GET_DAY          USING L_DATE IT_MASTER-DAY  .
    PERFORM GET_WORKTIME1    USING L_DATE IT_MASTER-TIME IT_MASTER-DAY .
*  ELSE.
*    L_DATE = P_DATES .
*    PERFORM GET_DAY         USING L_DATE IT_MASTER-DAY  .
*    IT_MASTER-SEQ    = '81'.       IT_MASTER-DATE   = L_DATE .
*    APPEND IT_MASTER.       CLEAR: IT_MASTER.
*  ENDIF.

  " From D+1 Day To D+21 Day..  (Only Working Dates in FACTORY-Calendar)
*  L_DATE = P_DATES .
  DO 21 TIMES.
    L_COUNT  = L_COUNT + 1.
    L_DATE   = L_DATE  + 1.
    PERFORM READ_WORKING_DATE USING '+'  WA_KALID  L_DATE.
    PERFORM GET_DAY          USING L_DATE IT_MASTER-DAY  .
    PERFORM GET_WORKING_TIME USING L_DATE IT_MASTER-TIME IT_MASTER-DAY.
    PERFORM GET_UPH        USING L_DATE IT_MASTER-UPH
                                 IT_MASTER-SHIFT G_SHOP.
    IT_MASTER-SEQ    = L_COUNT.
    IT_MASTER-DATE   = L_DATE .
    APPEND IT_MASTER.  CLEAR: IT_MASTER.
  ENDDO.
ENDFORM.                    " SET_INFORMATION

*&---------------------------------------------------------------------*
*&      Form  GET_WORKING_TIME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
*      -->P_IT_MASTER_WKTIME  text
*----------------------------------------------------------------------*
FORM GET_WORKING_TIME USING    PA_WDATE  PA_WKTIME  PA_DAY.
  DATA: L_WTIME       LIKE ZVPP_CAPACITY-ENDZT ,
        L_DATE        TYPE D ,
        L_EINZT       LIKE TC37A-EINZT ,
        LT_CAPA       LIKE TABLE OF ZVPP_CAPACITY      WITH HEADER LINE.

  CLEAR: LT_CAPA, LT_CAPA[], L_WTIME.
  SELECT * INTO TABLE LT_CAPA
    FROM ZVPP_CAPACITY
   WHERE ARBPL = G_SHOP
     AND DATUB >= PA_WDATE .

  SORT LT_CAPA BY DATUB .
  READ TABLE LT_CAPA INDEX 1.
  L_DATE = LT_CAPA-DATUB    .

  LOOP AT LT_CAPA WHERE DATUB = L_DATE AND TAGNR = PA_DAY .
    CLEAR: L_EINZT.
    SELECT SINGLE EINZT INTO L_EINZT
      FROM TC37A
     WHERE SCHGRUP  = LT_CAPA-MOSID
       AND KAPTPROG = LT_CAPA-TPROG
       AND ENDDA   >= PA_WDATE
       AND BEGDA   <= PA_WDATE     .
    L_WTIME = L_WTIME + L_EINZT    .
  ENDLOOP.
  PA_WKTIME = L_WTIME .
ENDFORM.                    " GET_WORKING_TIME

*&---------------------------------------------------------------------*
*&      Form  GET_WORKTIME1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
*      -->P_IT_MASTER_WKTIME  text
*----------------------------------------------------------------------*
FORM GET_WORKTIME1 USING    PA_WDATE  PA_WKTIME  PA_DAY.
  DATA: L_WTIME       LIKE ZVPP_CAPACITY-ENDZT ,
        L_DATE        TYPE D ,
        L_FLAG        TYPE C ,
        L_EINZT       LIKE TC37A-EINZT ,
        LT_CAPA       LIKE TABLE OF ZVPP_CAPACITY      WITH HEADER LINE.

  CLEAR: LT_CAPA, LT_CAPA[], L_WTIME.
  SELECT * INTO TABLE LT_CAPA
    FROM ZVPP_CAPACITY
   WHERE ARBPL = G_SHOP
     AND DATUB >= PA_WDATE .

  SORT LT_CAPA BY DATUB TAGNR SCHNR .
  READ TABLE LT_CAPA INDEX 1.
  L_DATE = LT_CAPA-DATUB    .

  LOOP AT LT_CAPA WHERE DATUB = L_DATE AND TAGNR = PA_DAY .
    CLEAR: L_EINZT.
    SELECT SINGLE EINZT INTO L_EINZT
      FROM TC37A
     WHERE SCHGRUP  = LT_CAPA-MOSID
       AND KAPTPROG = LT_CAPA-TPROG
       AND ENDDA   >= PA_WDATE
       AND BEGDA   <= PA_WDATE     .
    IT_MASTER-TIME  = L_EINZT      .
    IT_MASTER-SHIFT = LT_CAPA-SCHNR.
    IT_MASTER-SEQ   = 80 + LT_CAPA-SCHNR .
    PERFORM GET_UPH   USING PA_WDATE IT_MASTER-UPH
                            IT_MASTER-SHIFT G_SHOP.
    APPEND IT_MASTER.
    L_FLAG = 'X' .
  ENDLOOP.
  IF L_FLAG = SPACE.
    PERFORM GET_UPH   USING PA_WDATE IT_MASTER-UPH
                            IT_MASTER-SHIFT G_SHOP.
    APPEND IT_MASTER.
  ENDIF.
  CLEAR: IT_MASTER.
  PA_WKTIME = L_WTIME .
ENDFORM.                    " GET_WORKTIME1

*&---------------------------------------------------------------------*
*&      Form  GET_DAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
*      -->P_IT_MASTER_DAY  text
*----------------------------------------------------------------------*
FORM GET_DAY USING    PA_WDATE  PA_DAY.
  DATA: L_DAY         LIKE SCAL-INDICATOR .

  CALL FUNCTION 'DATE_COMPUTE_DAY'
       EXPORTING
            DATE = PA_WDATE
       IMPORTING
            DAY  = L_DAY.

  PA_DAY = L_DAY.
ENDFORM.                    " GET_DAY

*&---------------------------------------------------------------------*
*&      Form  GET_UPH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
*      -->P_IT_MASTER_UPH  text
*----------------------------------------------------------------------*
FORM GET_UPH USING    PA_WDATE  PA_UPH  PA_SHIFT P_SHOP.
  DATA: W_UPH LIKE ZTPP_STATUS-UPH.

  CALL FUNCTION 'Z_FPP_GET_UPH'
       EXPORTING
            DATE  = PA_WDATE
            SHIFT = PA_SHIFT
            SHOP  = P_SHOP
       IMPORTING
            UPH   = W_UPH.
  PA_UPH = W_UPH.


ENDFORM.                    " GET_UPH

*&---------------------------------------------------------------------*
*&      Form  REFRESH_DATE_PLAF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_MASTER_DATE  text
*----------------------------------------------------------------------*
FORM REFRESH_DATE_PLAF USING    PA_DATE.
  DATA: L_PORDER       LIKE BAPI_PLDORD-PLDORD_NUM.
  DATA: L_FIN_DATE     LIKE SY-DATUM.
*OSR!!!
  EXIT.

  PERFORM READ_PLANORDER   USING L_PORDER.

* get the plan order finish date
*  l_fin_date = pa_date + 2.
  L_FIN_DATE = PA_DATE.

  PERFORM READ_WORKING_DATE USING '+'  WA_KALID  L_FIN_DATE.

  DO .
    CALL FUNCTION 'Z_FPP_PLANORDER_RESCHEDULE'
          STARTING NEW TASK WA_TASKNAME DESTINATION IN GROUP 'PG_SEQ'
           PERFORMING RETURN_STEP1 ON END OF TASK
        EXPORTING
          P_PORDER                    = L_PORDER
          P_DATE                      = PA_DATE
          P_FIN_DATE                  = L_FIN_DATE
        EXCEPTIONS
          COMMUNICATION_FAILURE       = 1
          SYSTEM_FAILURE              = 2
          RESOURCE_FAILURE            = 3
          OTHERS                      = 4 .

    CASE SY-SUBRC.
      WHEN 0.
        WA_TASKNAME = WA_TASKNAME  + 1.
        WA_SND_JOBS = WA_SND_JOBS  + 1.
        CLEAR: WA_EXCP_FLAG .
        EXIT.
      WHEN 1 OR 2.
        WA_EXCP_FLAG = 'X'.
      WHEN 3.
*Receive reply to asynchronous RFC calls
        IF WA_EXCP_FLAG = SPACE.
          WA_EXCP_FLAG = 'X'.
*First attempt for RESOURCE_Failure handling
          WAIT UNTIL WA_RCV_JOBS >= WA_SND_JOBS UP TO '0.01' SECONDS.
        ELSE.
*Second attempt for RESOURCE_Failure handling
          WAIT UNTIL WA_RCV_JOBS >= WA_SND_JOBS UP TO '0.1' SECONDS.
        ENDIF.
        IF SY-SUBRC = 0.
          CLEAR WA_EXCP_FLAG. " Reset flag
*        ELSE.
*          EXIT.
        ENDIF.
    ENDCASE.
  ENDDO.
ENDFORM.                    " REFRESH_DATE_PLAF

*&---------------------------------------------------------------------*
*&      Form  READ_PLANORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_PORDER  text
*----------------------------------------------------------------------*
FORM READ_PLANORDER USING    PA_PORDER.
  DATA: L_VEHICLE   LIKE MARA-MATNR   ,
        LT_VAL      LIKE TABLE OF ZSPP_VIN_VALUE       WITH HEADER LINE.

  CLEAR: LT_VAL, LT_VAL[].
  LT_VAL-ATNAM = 'P_PLAN_ORDER'.  APPEND LT_VAL.
  CONCATENATE IT_DATA-MODL IT_DATA-BODY_SER  INTO L_VEHICLE.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
    EXPORTING
      OBJECT             = L_VEHICLE
      MODE               = 'R'
      CTYPE              = '002'
*     DISPLAY            = 'D'
    TABLES
      VAL_TABLE          = LT_VAL .

  READ TABLE LT_VAL INDEX 1.
  PA_PORDER = LT_VAL-ATWRT .
ENDFORM.                    " READ_PLANORDER

*&---------------------------------------------------------------------*
*&      Form  return_step1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM RETURN_STEP1  USING P_TASKNAME.
  RECEIVE RESULTS FROM FUNCTION 'Z_FPP_PLANORDER_RESCHEDULE'
         EXCEPTIONS
         COMMUNICATION_FAILURE       = 1
         SYSTEM_FAILURE              = 2
         RESOURCE_FAILURE            = 3
         OTHERS                      = 4.

  CHECK SY-SUBRC = 0.
  WA_RCV_JOBS  = WA_RCV_JOBS + 1.
ENDFORM.                    " return_step1
*&---------------------------------------------------------------------*
*&      Form  rescheduling_inputplan
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM RESCHEDULING_INPUTPLAN.
  DATA : IT_INPUT   LIKE ZTPP_INPUT_OSR OCCURS 0 WITH HEADER LINE.
  DATA : Z_RSNUM    LIKE ZTPP_INPUT_OSR-RSNUM,
         Z_SEQ_DATE LIKE  ZTPP_INPUT_OSR-SEQ_DATE.

  REFRESH IT_INPUT.CLEAR : Z_RSNUM,Z_SEQ_DATE.
* find the last scheduled date
*"comment by chris--because some holding cars has been scheduled
* for future date
*  SELECT MAX( rsnum ) INTO z_rsnum
*    FROM ZTPP_INPUT_OSR
*      WHERE mitu EQ space
*        AND status EQ '00'.

  PERFORM GET_WORKDAY_UPH USING G_LAST_SEQ_DATE.
*---Start#3 wskim 03/14/2005
*get date the last SEQ_DATE

* by Daniel on 4/15/11 {
*  SELECT MAX( SEQ_DATE ) INTO Z_SEQ_DATE
*    FROM ZTPP_INPUT_OSR
*      WHERE  MITU EQ SPACE.

  SELECT MAX( SEQ_DATE ) INTO Z_SEQ_DATE
    FROM ZTPP_INPUT_OSR
      WHERE  MITU EQ SPACE AND RS18 = 'A'.
* }


  PERFORM  FILTERING_DATE USING Z_SEQ_DATE.
*---End
  PERFORM READ_INPUTPLAN_RE .
  PERFORM CREATE_SUMMARY_RE USING F_COUNT Z_SEQ_DATE.

ENDFORM.                    " rescheduling_inputplan
*&---------------------------------------------------------------------*
*&      Form  get_workday_uph
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_WORKDAY_UPH USING P_RENUM.

  REFRESH IT_MASTER.CLEAR IT_MASTER.
*date conversion : user format
  MOVE P_RENUM TO WA_WDATE.
*  PERFORM date_conversion USING p_renum
*                          CHANGING wa_wdate.

  PERFORM READ_SHOP_CALID   USING WA_KALID.
  PERFORM READ_WORKING_DATE USING '-'  WA_KALID  WA_WDATE.
  PERFORM GET_DAY          USING WA_WDATE IT_MASTER-DAY .
  PERFORM GET_WORKING_TIME USING WA_WDATE IT_MASTER-TIME IT_MASTER-DAY .
  PERFORM GET_UPH        USING WA_WDATE IT_MASTER-UPH
                               IT_MASTER-SHIFT  G_SHOP.
  IT_MASTER-SEQ    = 99.
  IT_MASTER-DATE = WA_WDATE .
  APPEND IT_MASTER.  CLEAR: IT_MASTER.

  PERFORM SET_INFORMATION USING WA_WDATE.

ENDFORM.                    " get_workday_uph
*&---------------------------------------------------------------------*
*&      Form  date_conversion
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_RENUM  text
*      <--P_WA_WDATE  text
*----------------------------------------------------------------------*
FORM DATE_CONVERSION  USING    P_BEFORE_DATE
                     CHANGING P_AFTER_DATE.

  SELECT SINGLE *  FROM USR01
        WHERE BNAME = SY-UNAME.

  CASE USR01-DATFM.
    WHEN '1'. "DD.MM.YYYY
      P_AFTER_DATE+4(4) =   P_BEFORE_DATE+0(4).
      P_AFTER_DATE+2(2) =   P_BEFORE_DATE+4(2).
      P_AFTER_DATE+0(2) =   P_BEFORE_DATE+6(2).
    WHEN '2' OR '3'. "MM/DD/YYYY "MM-DD-YYYY
      P_AFTER_DATE+4(4) =   P_BEFORE_DATE+0(4).
      P_AFTER_DATE+0(2) =   P_BEFORE_DATE+4(2).
      P_AFTER_DATE+2(2) =   P_BEFORE_DATE+6(2).
  ENDCASE.

ENDFORM.                    " date_conversion
*&---------------------------------------------------------------------*
*&      Form  read_inputplan_re
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_INPUTPLAN_RE .


  REFRESH IT_DATA.

* by Daniel on 04/15/11 {
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_DATA
*    FROM ZTPP_INPUT_OSR
*   WHERE  STATUS = SPACE.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_DATA
    FROM ZTPP_INPUT_OSR
   WHERE  STATUS = SPACE AND RS18 = 'A'.
* }

* comment by chris--because xx xy car can not be counted for Trim input
*                   plan but need to be counted for body input plan
*  SELECT COUNT( * ) INTO z_count
*    FROM ZTPP_INPUT_OSR
*       WHERE status EQ '00'
*         AND mitu EQ space
*         AND rsnum EQ p_rsnum.
* end of chris comment-out

*  DESCRIBE TABLE it_data LINES  wa_hour .
*  IF wa_hour = 0.
*    DELETE FROM ztpp_alc_binput CLIENT SPECIFIED WHERE mandt = sy-mandt
  .
*    LEAVE PROGRAM .
*  ENDIF.

  SORT IT_DATA BY SERIAL .
  LOOP AT IT_DATA.
    CLEAR IT_DATA-RD01.
    MODIFY IT_DATA FROM IT_DATA INDEX SY-TABIX.
  ENDLOOP.



ENDFORM.                    " read_inputplan_re
*&---------------------------------------------------------------------*
*&      Form  create_summary_re
*&---------------------------------------------------------------------*
*       this is the new reshceduling logic
*----------------------------------------------------------------------*
* in this logic, XX XY cars can be scheduled but don't count if trim
* input UPH is used ,because
*     'xx' 'xy' cars will go to body shop but will not go to
*     trim shop, so if we use the trim input uph to schedule
*     the cars, we must include enough cars for trim input
*     after excluding the 'xx' 'xy' cars
* holding cars will be scheduled if the planed schedule date is the
*     same as the date we are doing reschedule
*
*----------------------------------------------------------------------*
FORM CREATE_SUMMARY_RE USING F_COUNT P_SEQ_DATE.
  DATA: L_WORDER             LIKE MARA-MATNR,
        L_FLAG               TYPE C ,
        L_MAX                TYPE I ,
        L_HOURS              TYPE I ,
        L_POS                TYPE I ,
        L_LOOP               TYPE I ,
        L_CNT                TYPE I ,
        L_DATA_ERROR         TYPE C ,
        L_SKIP               TYPE C ,
        L_CHK                TYPE P DECIMALS 3,
        L_TABIX              LIKE SY-TABIX,
        L_INDEX              LIKE SY-TABIX,
        L_SERIAL             LIKE ZTPP_INPUT_PLAN-SERIAL,
        F_NUM TYPE I.
  DATA : IT_INPUT LIKE ZTPP_INPUT_OSR OCCURS 0 WITH HEADER LINE,
         IT_7JBA  LIKE ZTPP_PMT07JB_A OCCURS 0 WITH HEADER LINE.
  DATA : Z_NUM LIKE ZTPP_PMT07JB_A-SSR1.
  DATA : L_YN, L_HOLD, L_XX.
  DATA: L_LINES TYPE I,
        L_LINES_N(5) TYPE N.

* output total qty of blank status
  DESCRIBE TABLE IT_DATA LINES L_LINES.
  L_LINES_N = L_LINES.
  IF P_LOG EQ 'X'.
    WRITE:/ 'Blank status total :', L_LINES_N.
  ENDIF.
  REFRESH :IT_INPUT,IT_7JBA.

  CLEAR :L_SERIAL,F_NUM,L_INDEX.
  DESCRIBE TABLE IT_DATA LINES L_MAX.
  L_LOOP = L_INDEX = 1.


  " First Day Data...
  SORT IT_MASTER BY SEQ .

* continue to schedule for the scheduled last date
* f_count:  qty left for the last day
  LOOP AT IT_MASTER WHERE  SEQ EQ '99' .
    L_CHK = IT_MASTER-TIME / 3600    .
    L_POS = ( IT_MASTER-UPH * L_CHK ).

    LOOP AT IT_DATA.
      IF  F_COUNT = 0.
        EXIT.
      ENDIF.
*     check if the car is 'xx''xy' car
      PERFORM CHECK_XXXY USING IT_DATA L_XX.

      MOVE IT_MASTER-DATE TO IT_DATA-RSNUM .
      L_SERIAL = IT_DATA-SERIAL.

      IF G_SHOP = 'T'.              "using trim UPH
        IF L_XX = 'N'.              "If xx,xy, don't count it
          F_COUNT = F_COUNT - 1.
        ENDIF.
      ELSE.                         "Using body UPH
        F_COUNT = F_COUNT - 1.
      ENDIF.
      MODIFY IT_DATA FROM IT_DATA.

      F_NUM = F_NUM + 1.
    ENDLOOP.
  ENDLOOP.

* next serial number
  IF F_NUM = 0.
    READ TABLE IT_DATA INDEX 1.
    L_SERIAL = IT_DATA-SERIAL.
  ELSE.
    L_SERIAL = L_SERIAL + 1.
  ENDIF.
* next index of internal table it_data
  F_NUM = F_NUM + 1.

  LOOP AT IT_MASTER WHERE SEQ < 80.
    IF L_MAX   <  F_NUM .  " no more car left for schedule
      EXIT.
    ENDIF.

    L_CHK = IT_MASTER-TIME / 3600 .
    L_POS =  ( IT_MASTER-UPH * L_CHK  ).
*   check how many holding cars have been scheduled for this date
    PERFORM CHECK_COUNT USING F_COUNT
                              IT_MASTER-DATE.

    L_POS = L_POS - F_COUNT.          "deduct the shceduled qty
    F_COUNT = L_POS.
    LOOP AT IT_DATA FROM F_NUM TO L_MAX.
      IF F_COUNT = 0.
        EXIT.
      ENDIF.
      L_SERIAL = IT_DATA-SERIAL.
*     check if the car is 'xx''xy' car
      PERFORM CHECK_XXXY USING IT_DATA L_XX.

      MOVE IT_MASTER-DATE TO IT_DATA-RSNUM .
      IF G_SHOP = 'T'.              "using trim UPH
        IF L_XX = 'N'.              "If xx xy car, don't count it
          F_COUNT = F_COUNT - 1.
        ENDIF.
      ELSE.                         "Using body UPH,always count it
        F_COUNT = F_COUNT - 1.
      ENDIF.
      MODIFY IT_DATA FROM IT_DATA.  "schedule date

    ENDLOOP.
    F_NUM = F_NUM + L_POS.            "next date's begin index

  ENDLOOP.

  CLEAR Z_NUM.
  LOOP AT IT_DATA.
    MOVE-CORRESPONDING IT_DATA TO IT_INPUT.
*---Start#3 wskim 03/14/2005
    IF IT_DATA-RSNUM EQ '00000000'.
      MOVE P_SEQ_DATE TO IT_INPUT-RSNUM.
    ENDIF.
*---End

    APPEND IT_INPUT.
  ENDLOOP.


* reading the original data
  SELECT * INTO TABLE IT_7JBA
     FROM ZTPP_PMT07JB_A
      WHERE  GUBB = 'A'.
  .
  REFRESH IT_7JB.

  SORT IT_7JBA BY SQDT SSR1.

** chnaged by Furong on 15 Aug
*  SORT IT_INPUT BY SEQ_DATE SEQ_SERIAL.
  SORT IT_INPUT BY SERIAL.
*
** end of change
*   transfer the rescheduled sequence date to 7jb_a table
  LOOP AT IT_7JBA.
    MOVE-CORRESPONDING IT_7JBA TO ZTPP_PMT07JB_A.

    READ TABLE IT_INPUT INDEX SY-TABIX.

* * check if the two data has the same sequence
    IF SY-SUBRC NE 0 OR
      IT_7JBA-ORDR NE IT_INPUT-WORK_ORDER+0(9) OR
      IT_7JBA-DIST NE IT_INPUT-WORK_ORDER+9(5) OR
      IT_7JBA-EXTC NE IT_INPUT-EXTC            OR
      IT_7JBA-INTC NE IT_INPUT-INTC.
      IF L_DATA_ERROR NE 'X'.
        IF P_LOG EQ 'X'.
         WRITE:/ '7jb data has different sequence with input plan data'.
        ENDIF.
        L_DATA_ERROR = 'X'.
      ENDIF.
    ENDIF.

    MOVE : IT_INPUT-RSNUM TO ZTPP_PMT07JB_A-SQDT.
    MOVE-CORRESPONDING ZTPP_PMT07JB_A TO IT_7JB.

    APPEND IT_7JB.
    CLEAR : ZTPP_PMT07JB_A, IT_7JB.
  ENDLOOP.

  data: l_idx like sy-tabix.

* delete old records of input_plan physical table before seq_date change
*Tuning by Andy
  LOOP AT IT_INPUT.
* by Daniel on 4/15/11
*    DELETE FROM ZTPP_INPUT_OSR
*         WHERE SEQ_DATE = IT_INPUT-SEQ_DATE
*           AND SERIAL = IT_INPUT-SERIAL.
    DELETE FROM ZTPP_INPUT_OSR
         WHERE SEQ_DATE = IT_INPUT-SEQ_DATE
           AND SERIAL = IT_INPUT-SERIAL
           AND RS18 = 'A'.
* }
  ENDLOOP.
  IF SY-SUBRC NE 0.
    IF P_LOG EQ 'X'.
      WRITE:/ 'input_plan records deletion error'.
    ENDIF.
    EXIT.
  ENDIF.


* create new sequence serial number and also save the
* number to input_plan table to keep consistency

  LOOP AT IT_7JB.
    ON CHANGE OF IT_7JB-SQDT.
      CLEAR Z_NUM.
    ENDON.
    l_idx = SY-TABIX.
    Z_NUM = Z_NUM + 1.
    MOVE Z_NUM       TO  IT_7JB-SSR1.
    "read input plan data
    READ TABLE IT_INPUT INDEX l_idx.
    IF SY-SUBRC NE 0.
      IF P_LOG EQ 'X'.
        WRITE:/ 'Warning: ', it_7jb-SQDT, it_7jb-MODL, it_7jb-SSR1,
                             it_7jb-ORDR, it_7jb-DIST,
                             it_7jb-EXTC, it_7jb-INTC.
      ENDIF.
    ELSE.

*      IT_INPUT-SEQ_DATE   = IT_7JB-SQDT.  "keep the two seq_date same
*      IT_INPUT-SEQ_SERIAL = Z_NUM.        "keep the two serial no same

      IT_INPUT-RS01       = Z_NUM.
*Vehicle plan date/time stamp.
    IT_INPUT-ZEDAT = SY-DATUM.
    IT_INPUT-ZETIM = SY-UZEIT.

      MODIFY IT_INPUT FROM IT_INPUT INDEX SY-TABIX
          TRANSPORTING RS01 ZEDAT ZETIM. "SEQ_DATE SEQ_SERIAL

    ENDIF.

    MODIFY IT_7JB FROM IT_7JB.
    CLEAR IT_7JB.
  ENDLOOP.

* update the input_plan physical table
*  loop at it_input.
*    update ZTPP_INPUT_OSR FROM IT_INPUT.
*  endloop.
  INSERT ZTPP_INPUT_OSR FROM TABLE IT_INPUT
                 ACCEPTING DUPLICATE KEYS .
  IF SY-SUBRC = 0.

*   DELETE FROM  ztpp_pmt07jb_a WHERE gubb = 'A'.
*   MODIFY ztpp_pmt07jb_a FROM TABLE it_7jb.

*   IF SY-SUBRC EQ 0.
*      MESSAGE s000 WITH 'Successfully Updated'.
*      COMMIT WORK.
*   else.
*      write:/ '7jb_a table update fail'.
*   ENDIF.
  ENDIF.


ENDFORM.                    " create_summary_re
*&---------------------------------------------------------------------*
*&      Form  date_conversion_r
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_MASTER_DATE  text
*      <--P_IT_DATA_RSNUM  text
*----------------------------------------------------------------------*
FORM DATE_CONVERSION_R  USING    P_BEFORE_DATE
                     CHANGING P_AFTER_DATE.

  SELECT SINGLE *  FROM USR01
        WHERE BNAME = SY-UNAME.

  CASE USR01-DATFM.
    WHEN '1'. "DD.MM.YYYY
      P_AFTER_DATE+4(4) =   P_BEFORE_DATE+0(4).
      P_AFTER_DATE+2(2) =   P_BEFORE_DATE+4(2).
      P_AFTER_DATE+0(2) =   P_BEFORE_DATE+6(2).
    WHEN '2' OR '3'. "MM/DD/YYYY "MM-DD-YYYY
      P_AFTER_DATE+4(4) =   P_BEFORE_DATE+0(4).
      P_AFTER_DATE+0(2) =   P_BEFORE_DATE+4(2).
      P_AFTER_DATE+2(2) =   P_BEFORE_DATE+6(2).
  ENDCASE.

ENDFORM.                    " date_conversion_r
*&---------------------------------------------------------------------*
*&      Form  filtering_Date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILTERING_DATE USING P_SEQ_DATE.

  DATA : WA_MASTER LIKE IT_MASTER.

  LOOP AT IT_MASTER INTO WA_MASTER WHERE DATE > P_SEQ_DATE.
    DELETE TABLE IT_MASTER FROM WA_MASTER .
  ENDLOOP.


ENDFORM.                    " filtering_Date
*&---------------------------------------------------------------------*
*&      Form  set_shop
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_SHOP.
  IF P_BODY = 'X'.
    G_SHOP = 'B'.
  ELSE.
    G_SHOP = 'T'.
  ENDIF.
ENDFORM.                    " set_shop
*&---------------------------------------------------------------------*
*&      Form  check_xxxy
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_DATA  text
*      -->P_L_YN  text
*----------------------------------------------------------------------*
FORM CHECK_XXXY USING    PT_DATA  LIKE IT_DATA
                         P_YN.
  IF PT_DATA-WORK_ORDER+12(2) EQ 'XX' OR
     PT_DATA-WORK_ORDER+12(2) EQ 'XY'.
    P_YN = 'Y'.
  ELSE.
    P_YN = 'N'.
  ENDIF.

ENDFORM.                    " check_xxxy
*&---------------------------------------------------------------------*
*&      Form  check_hold_car
*&---------------------------------------------------------------------*
*      Check if the car is holding for future schedule
*----------------------------------------------------------------------*
FORM CHECK_HOLD_CAR USING    PT_DATA  LIKE IT_DATA
                             P_DATE   LIKE SY-DATUM
                             P_YN.

  READ TABLE IT_HOLD WITH KEY MODL = PT_DATA-MODL
                          BODY_SER = PT_DATA-BODY_SER.
  IF SY-SUBRC = 0.
    P_YN = 'Y'.
  ELSE.
    P_YN = 'N'.
  ENDIF.

ENDFORM.                    " check_hold_car

*&---------------------------------------------------------------------*
*&      Form  read_hold_cars
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_HOLD_CARS.

  REFRESH IT_HOLD.

  SELECT * INTO TABLE IT_HOLD FROM ZTPP_HOLD_CAR
         WHERE  ( STATUS EQ 'W' ) OR ( STATUS EQ SPACE ).

ENDFORM.                    " read_hold_cars
*&---------------------------------------------------------------------*
*&      Form  refresh_hold_cars
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REFRESH_HOLD_CARS.
  DATA: L_PASS.
  DATA: L_DATE LIKE  SY-DATUM.

  PERFORM READ_ATINN USING 'P_RP_SATUS'.
  PERFORM READ_ATINN USING 'P_RP06_SHOP_DATE'.
  LOOP AT IT_HOLD.
*   check the reporting point
    PERFORM CHECK_RP USING L_PASS L_DATE.

*   PLANNED SCHEDULE DATE HAS PASSED
    IF IT_HOLD-RES_DATE LT SY-DATUM.
      IT_HOLD-STATUS = 'P'.
      IT_HOLD-RES_DATE = L_DATE.
      MODIFY IT_HOLD.
      CONTINUE.
    ENDIF.

*   CAR HAS PASSED THE TRIM INPUT POINT
    IF  L_PASS = 'Y'.
      IT_HOLD-STATUS = 'P'.
      IT_HOLD-RES_DATE = L_DATE.
      MODIFY IT_HOLD.
      CONTINUE.
    ENDIF.

    CLEAR: IT_DATA.
    IT_DATA-MODL = IT_HOLD-MODL.
    IT_DATA-BODY_SER = IT_HOLD-BODY_SER.
    PERFORM REFRESH_DATE_PLAF  USING IT_HOLD-RES_DATE .
    IT_HOLD_SCH  = IT_HOLD.
    APPEND IT_HOLD_SCH.
  ENDLOOP.

* UPDATE THE DATABASE
*  MODIFY ZTPP_HOLD_CAR FROM TABLE IT_HOLD.
  IF SY-SUBRC = 0.
    MESSAGE S000 WITH TEXT-003.
    COMMIT WORK.
  ELSE.
    MESSAGE S000 WITH TEXT-004.
    ROLLBACK WORK.
  ENDIF.

ENDFORM.                    " refresh_hold_cars
*&---------------------------------------------------------------------*
*&      Form  check_count
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_COUNT  text
*      -->P_IT_MASTER_DATE  text
*----------------------------------------------------------------------*
FORM CHECK_COUNT USING    P_COUNT
                          P_DATE.

  CLEAR: P_COUNT.
  LOOP AT IT_HOLD_SCH WHERE RES_DATE = P_DATE.
    P_COUNT = P_COUNT + 1.
  ENDLOOP.

ENDFORM.                    " check_count
*&---------------------------------------------------------------------*
*&      Form  get_body_shop_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_BODY_SHOP_DATA.
  PERFORM CLEAR_VARIBLE.
  PERFORM SET_SHOP.
*  " Get the Date for the Production Reporting Date(Last Date)
  WA_WDATE = P_DATES - 1.
  PERFORM READ_SHOP_CALID   USING WA_KALID.
  PERFORM READ_WORKING_DATE USING '-'  WA_KALID  WA_WDATE.
  PERFORM GET_DAY          USING WA_WDATE IT_MASTER-DAY .
  PERFORM GET_WORKING_TIME USING WA_WDATE IT_MASTER-TIME IT_MASTER-DAY .
  PERFORM GET_UPH        USING WA_WDATE IT_MASTER-UPH
                               IT_MASTER-SHIFT G_SHOP.
  IT_MASTER-SEQ    = 99.
  IT_MASTER-DATE = WA_WDATE .
  APPEND IT_MASTER.  CLEAR: IT_MASTER.
* get 21 days information
  PERFORM SET_INFORMATION USING P_DATES.
  PERFORM READ_INPUTPLAN_B .
* making the schedule.
  PERFORM SCHEDULE_BODY_INPUT.

**** schedule the vehicle with status blank.

* read the neccesory data and do the schedule
  PERFORM RESCHEDULE_INPUTPLAN_BODY.

ENDFORM.                    " get_body_shop_data
*&---------------------------------------------------------------------*
*&      Form  schedule_body_input
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SCHEDULE_BODY_INPUT.
  DATA: L_WORDER             LIKE MARA-MATNR,
          L_FLAG               TYPE C ,
          L_MAX                TYPE I ,
          L_HOURS              TYPE I ,
          L_POS                TYPE I ,
          L_POS1               TYPE I ,
          L_SERIAL             LIKE IT_DATA-BODY_SER,
          L_COUNT              TYPE I ,
          F_NUM                TYPE I ,
          L_LOOP               TYPE I ,
          L_SKIP               TYPE C ,
          L_HOLD               TYPE C ,
          L_YN                 TYPE C ,
          L_CHK                TYPE P DECIMALS 3,
          L_TABIX              LIKE SY-TABIX,
          L_INDEX              LIKE SY-TABIX.



  DESCRIBE TABLE IT_DATA LINES L_MAX.
  L_LOOP = L_INDEX = 1.

  " First Day Data...
  SORT IT_MASTER BY SEQ .
  LOOP AT IT_MASTER WHERE SEQ > 80 AND SEQ < 90 .
    IF L_INDEX > L_MAX.
      EXIT.
    ENDIF.
    L_CHK = IT_MASTER-TIME / 3600 .
    L_POS1 = IT_MASTER-UPH * L_CHK.
*   check how many holding cars has been scheduled for this date
    PERFORM CHECK_COUNT USING L_COUNT
                              IT_MASTER-DATE.

    L_POS1 = L_POS1 - L_COUNT.          "deduct the shceduled qty
    L_COUNT = L_POS1.
    LOOP AT IT_DATA FROM L_INDEX TO L_MAX.
      IF L_COUNT = 0.
        EXIT.
      ENDIF.
*     if the car is holding car skip it
      PERFORM CHECK_HOLD_CAR USING IT_DATA IT_MASTER-DATE L_HOLD.
      IF L_HOLD = 'N'.    "Not a holding car, do refresh
*        PERFORM refresh_date_plaf  USING it_master-date.
        MOVE-CORRESPONDING IT_DATA TO IT_INPUTPLAN.
        IT_INPUTPLAN-RD01 = IT_MASTER-DATE.

        IT_INPUTPLAN-ZEDAT = SY-DATUM.
        IT_INPUTPLAN-ZETIM = SY-UZEIT.
        APPEND IT_INPUTPLAN. CLEAR: IT_INPUTPLAN.

        L_COUNT = L_COUNT - 1.
        G_LAST_SEQ_DATE  = IT_MASTER-DATE.
        F_COUNT = L_COUNT.
      ENDIF.
      F_NUM = F_NUM + 1.
    ENDLOOP.
    L_INDEX = L_INDEX + F_NUM .     "next begin postion
    CLEAR F_NUM.
  ENDLOOP.

  " Daily Data...

  DO 21 TIMES.
    L_TABIX = L_TABIX + 1 .
    READ TABLE IT_MASTER WITH KEY SEQ = L_TABIX.
    IF L_INDEX > L_MAX.
      EXIT.
    ENDIF.
    L_CHK = IT_MASTER-TIME / 3600 .
    L_POS =  IT_MASTER-UPH * L_CHK  .
*   check how many holding cars has been scheduled for this date
    PERFORM CHECK_COUNT USING L_COUNT
                              IT_MASTER-DATE.

    L_POS = L_POS - L_COUNT.          "deduct the shceduled qty
    L_COUNT = L_POS.

    LOOP AT IT_DATA FROM L_INDEX TO L_MAX.
      IF L_COUNT = 0.
        EXIT.
      ENDIF.

*     if the car is holding car skip it
      PERFORM CHECK_HOLD_CAR USING IT_DATA IT_MASTER-DATE L_HOLD.
      IF L_HOLD = 'N'.    "Not a holding car, do refresh

        MOVE-CORRESPONDING IT_DATA TO IT_INPUTPLAN.
        IT_INPUTPLAN-RD01 = IT_MASTER-DATE.

        IT_INPUTPLAN-ZEDAT = SY-DATUM.
        IT_INPUTPLAN-ZETIM = SY-UZEIT.
        APPEND IT_INPUTPLAN. CLEAR: IT_INPUTPLAN.

        L_COUNT = L_COUNT - 1.
*       recording the last date scheduled
        G_LAST_SEQ_DATE  = IT_MASTER-DATE.
        F_COUNT = L_COUNT.       "qty left

      ENDIF.

      F_NUM = F_NUM + 1.
    ENDLOOP.

    L_INDEX = L_INDEX + F_NUM .
    CLEAR: F_NUM.
  ENDDO.

ENDFORM.                    " schedule_body_input
*&---------------------------------------------------------------------*
*&      Form  CLEAR_VARIBLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CLEAR_VARIBLE.
  CLEAR: IT_MASTER           ,
         IT_MASTER[]         ,
         IT_DATA             ,
         IT_DATA[]           ,
         WA_DISP             ,
         WA_DATA             ,
         WA_WDATE            ,
         WA_KALID            ,
         WA_UZEIT            ,
         WA_INDEX            ,
         WA_SND_JOBS         ,
         WA_RCV_JOBS         ,
         WA_TASKNAME(4)      ,
         WA_EXCP_FLAG        ,
         WA_ERROR            ,
         WA_FLAG             ,
         WA_HOUR             ,
         F_COUNT             ,
         IT_HOLD             ,
         IT_HOLD[]           ,
         IT_HOLD_SCH         ,
         G_LAST_SEQ_DATE     .

ENDFORM.                    " CLEAR_VARIBLE
*&---------------------------------------------------------------------*
*&      Form  reschedule_inputplan_body
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM RESCHEDULE_INPUTPLAN_BODY.
  DATA : IT_INPUT   LIKE ZTPP_INPUT_OSR OCCURS 0 WITH HEADER LINE.
  DATA : Z_RSNUM    LIKE ZTPP_INPUT_OSR-RSNUM,
         Z_SEQ_DATE LIKE  ZTPP_INPUT_OSR-SEQ_DATE.


  PERFORM GET_WORKDAY_UPH USING G_LAST_SEQ_DATE.

*get date the last SEQ_DATE
  SELECT MAX( SEQ_DATE ) INTO Z_SEQ_DATE
    FROM ZTPP_INPUT_OSR
      WHERE  MITU EQ SPACE.

* delete date after the maximum sequence date
  PERFORM  FILTERING_DATE USING Z_SEQ_DATE.

* read the input plan data with status blank
  PERFORM READ_INPUTPLAN_RE .
*
* reshcedule the vehicles with status blank
* this only update the input plan date, no
* update for plan order
  PERFORM SCHEDULE_BLAND USING F_COUNT Z_SEQ_DATE.

ENDFORM.                    " reschedule_inputplan_body
*&---------------------------------------------------------------------*
*&      Form  schedule_bland
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_F_COUNT  text
*      -->P_Z_SEQ_DATE  text
*----------------------------------------------------------------------*
FORM SCHEDULE_BLAND USING F_COUNT P_SEQ_DATE.
  DATA: L_WORDER             LIKE MARA-MATNR,
        L_FLAG               TYPE C ,
        L_MAX                TYPE I ,
        L_HOURS              TYPE I ,
        L_POS                TYPE I ,
        L_LOOP               TYPE I ,
        L_CNT                TYPE I ,
        L_SKIP               TYPE C ,
        L_CHK                TYPE P DECIMALS 3,
        L_TABIX              LIKE SY-TABIX,
        L_INDEX              LIKE SY-TABIX,
        L_SERIAL             LIKE ZTPP_INPUT_OSR-SERIAL,
        F_NUM TYPE I.
  DATA : IT_INPUT LIKE ZTPP_INPUT_OSR OCCURS 0 WITH HEADER LINE,
         IT_7JBA  LIKE ZTPP_PMT07JB_A OCCURS 0 WITH HEADER LINE.
  DATA : Z_NUM LIKE ZTPP_PMT07JB_A-SSR1.
  DATA : L_YN, L_HOLD, L_XX.

  REFRESH :IT_INPUT,IT_7JBA.

  CLEAR :L_SERIAL,F_NUM,L_INDEX.
  DESCRIBE TABLE IT_DATA LINES L_MAX.
  L_LOOP = L_INDEX = 1.


  " First Day Data...
  SORT IT_MASTER BY SEQ .

* continue to schedule for the scheduled last date
* f_count:  qty left for the last day
  LOOP AT IT_MASTER WHERE  SEQ EQ '99' .
    L_CHK = IT_MASTER-TIME / 3600    .
    L_POS = ( IT_MASTER-UPH * L_CHK ).

    LOOP AT IT_DATA.
      IF  F_COUNT = 0.
        EXIT.
      ENDIF.

      MOVE IT_MASTER-DATE TO IT_DATA-RD01 .
      L_SERIAL = IT_DATA-SERIAL.

      F_COUNT = F_COUNT - 1.

      MODIFY IT_DATA FROM IT_DATA.

      F_NUM = F_NUM + 1.
    ENDLOOP.
  ENDLOOP.

* next serial number
  IF F_NUM = 0.
    READ TABLE IT_DATA INDEX 1.
    L_SERIAL = IT_DATA-SERIAL.
  ELSE.
    L_SERIAL = L_SERIAL + 1.
  ENDIF.
* next index of internal table it_data
  F_NUM = F_NUM + 1.

  LOOP AT IT_MASTER WHERE SEQ < 80.
    IF L_MAX   <  F_NUM .  " no more car left for schedule
      EXIT.
    ENDIF.

    L_CHK = IT_MASTER-TIME / 3600 .
    L_POS =  ( IT_MASTER-UPH * L_CHK  ).
*   check how many holding cars have been scheduled for this date
    PERFORM CHECK_COUNT USING F_COUNT
                              IT_MASTER-DATE.

    L_POS = L_POS - F_COUNT.          "deduct the shceduled qty
    F_COUNT = L_POS.
    LOOP AT IT_DATA FROM F_NUM TO L_MAX.
      IF F_COUNT = 0.
        EXIT.
      ENDIF.
      L_SERIAL = IT_DATA-SERIAL.
*     check if the car is 'xx''xy' car
      PERFORM CHECK_XXXY USING IT_DATA L_XX.

      MOVE IT_MASTER-DATE TO IT_DATA-RD01 .
      IF G_SHOP = 'T'.              "using trim UPH
        IF L_XX = 'N'.              "If xx xy car, don't count it
          F_COUNT = F_COUNT - 1.
        ENDIF.
      ELSE.                         "Using body UPH,always count it
        F_COUNT = F_COUNT - 1.
      ENDIF.
      MODIFY IT_DATA FROM IT_DATA.  "schedule date

    ENDLOOP.
    F_NUM = F_NUM + L_POS.            "next date's begin index

  ENDLOOP.

  CLEAR Z_NUM.
  LOOP AT IT_DATA.
    MOVE-CORRESPONDING IT_DATA TO IT_INPUTPLAN.
*---Start#3 wskim 03/14/2005
    IF IT_DATA-RD01 EQ '00000000'.
      MOVE P_SEQ_DATE TO IT_INPUTPLAN-RD01.
    ENDIF.
*---End
*# Req. WCLee : ZSDAT , ZSTIM by sjlee
    IT_INPUTPLAN-ZEDAT = SY-DATUM.
    IT_INPUTPLAN-ZETIM = SY-UZEIT.
    APPEND IT_INPUTPLAN.
  ENDLOOP.
*
* update the input plan table

  UPDATE ZTPP_INPUT_OSR FROM TABLE IT_INPUTPLAN.
  IF SY-SUBRC = 0.
    MESSAGE S000 WITH 'Successfully Updated Body Plan'.
    COMMIT WORK.
  ELSE.
    MESSAGE S000 WITH 'Failed to Update Body Plan'.
  ENDIF.

ENDFORM.                    " schedule_bland
*&---------------------------------------------------------------------*
*&      Form  read_inputplan_B
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_INPUTPLAN_B.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_DATA
     FROM ZTPP_INPUT_OSR
     WHERE STATUS = '00' AND
           MITU NE 'Y'.

  DESCRIBE TABLE IT_DATA LINES  WA_HOUR .
  IF WA_HOUR = 0.
*OSR!!!
*    DELETE FROM ZTPP_ALC_BINPUT CLIENT SPECIFIED WHERE MANDT = SY-MANDT
*.
*    LEAVE PROGRAM .
  ENDIF.

  SORT IT_DATA BY SERIAL .
* reading all waiting holding cars
  PERFORM READ_HOLD_CARS.

* set the initial last date
  G_LAST_SEQ_DATE = P_DATES.

ENDFORM.                    " read_inputplan_B
*&---------------------------------------------------------------------*
*&      Form  check_rp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_RP USING P_PASS P_SHOP_DATE.
  DATA: WA_HOLD LIKE IT_HOLD .
  DATA: L_ATWRT LIKE AUSP-ATWRT.
  DATA: L_STATUS LIKE IT_DATA-STATUS.

  CLEAR: P_PASS, IT_DATA.

  READ TABLE IT_CABN WITH KEY ATNAM = 'P_RP_STATUS'.
  PERFORM GET_STATUS USING L_STATUS.
*  if the car has gone to trim shop
  IF SY-SUBRC = 0 AND L_STATUS GE '06'.
    PERFORM GET_SHOP_DATE USING 'P_RP06_SHOP_DATE' L_ATWRT.
    P_SHOP_DATE = L_ATWRT.
    P_PASS = 'Y'.
  ENDIF.
ENDFORM.                    " check_rp
*&---------------------------------------------------------------------*
*&      Form  GET_SHOP_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0197   text
*----------------------------------------------------------------------*
FORM GET_SHOP_DATE USING  P_ATNAM P_ATWRT.
  DATA: L_OBJEK  LIKE AUSP-OBJEK.
  DATA: L_ATINN   LIKE AUSP-ATINN.
* OBJEK KEY
  CONCATENATE IT_HOLD-MODL IT_HOLD-BODY_SER INTO L_OBJEK.
* GET ATINN
  READ TABLE IT_CABN WITH KEY ATNAM = P_ATNAM.
* READ THE CHARACTERIC VALUE
  SELECT SINGLE ATWRT INTO P_ATWRT
   FROM AUSP
   WHERE OBJEK = L_OBJEK
    AND  ATINN = L_ATINN.
  IF SY-SUBRC NE 0.
    MESSAGE S000 WITH 'HOLDING CAR HAS NO RP06 SHOP DATE'.
  ENDIF.

ENDFORM.                    " GET_SHOP_DATE
*&---------------------------------------------------------------------*
*&      Form  read_atinn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_ATINN USING P_ATNAM.

  SELECT SINGLE ATNAM ATINN INTO IT_CABN
    FROM CABN
    WHERE ATNAM = P_ATNAM.
  IF SY-SUBRC = 0.
    APPEND IT_CABN.
    CLEAR: IT_CABN.
  ENDIF.

ENDFORM.                    " read_atinn
*&---------------------------------------------------------------------*
*&      Form  GET_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_STATUS  text
*----------------------------------------------------------------------*
FORM GET_STATUS USING    P_STATUS.
  SELECT SINGLE STATUS INTO P_STATUS
   FROM ZTPP_INPUT_OSR
   WHERE MODL = IT_HOLD-MODL
    AND  BODY_SER = IT_HOLD-BODY_SER.
  IF SY-SUBRC NE 0.
    MESSAGE S000 WITH 'HOLDING CAR HAS NO STATUS'.
  ENDIF.
ENDFORM.                    " GET_STATUS
