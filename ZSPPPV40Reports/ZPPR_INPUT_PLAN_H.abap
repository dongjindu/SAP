************************************************************************
* Program Name      : ZPPR_INPUT_PLAN_H
* Author            : Furong Wang
* Creation Date     :
* Specifications By :
* Pattern           :
* Development Request
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT  ZPPR_INPUT_PLAN_H   MESSAGE-ID ZMPP.

TABLES: ZTPP_INPUTPLAN_H.
*----------------------------------------------------------------------
* TABLES DECLARATION
*----------------------------------------------------------------------
DATA: BEGIN OF IT_TEMP OCCURS 0,
      OBJEK LIKE AUSP-OBJEK,      " EQUI-EQUNR
      ATINN LIKE AUSP-ATINN,      " CHARACTERISTIC
      ATWRT LIKE AUSP-ATWRT,      " CHARACTERISTIC VAL
      ATFLV LIKE AUSP-ATFLV,      " Date & Number  VAL
      END OF IT_TEMP.

DATA: BEGIN OF IT_DATA        OCCURS 0,
      OBJEK LIKE AUSP-OBJEK.      " Vehicle Code
        INCLUDE STRUCTURE ZTPP_INPUTPLAN_H.
DATA: USAGE TYPE C,
      END OF IT_DATA.

*----------------------------------------------------------------------
* WORKING-AREA DECLARATION
*----------------------------------------------------------------------
DATA: WA_DATA             LIKE IT_DATA                             ,
      WA_UPH(3)           TYPE N                                   ,
      WA_SERIAL           LIKE ZTPP_DVRT1-SERIAL                   ,
      W_DATE LIKE SY-DATUM,
      W_TIME LIKE SY-UZEIT.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 3.
PARAMETERS : P_SAVE AS CHECKBOX default 'X'.
SELECTION-SCREEN COMMENT 8(25) TEXT-M16 FOR FIELD P_SAVE.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B1.


*----------------------------------------------------------------------
INITIALIZATION.
*----------------------------------------------------------------------

*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------
  PERFORM DISPLAY_PROCESS.
  perform checking_batch_job.
  message s000 with 'Delete old plan...'.
  PERFORM DELETE_OLD_INPUTPLAN.
  message s000 with 'Read vehicles...'.
  PERFORM READ_VEHICLE.
  message s000 with 'Generate input plan'.
  PERFORM GENERATE_INPUTPLAN.
  message s000 with 'Check holding car...'.
  PERFORM CHECK_HOLDING_CAR.
*&---------------------------------------------------------------------*
*&      Form  delete_old_inputplan
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DELETE_OLD_INPUTPLAN.
  W_DATE = SY-DATUM.
  W_TIME = SY-UZEIT.

  DELETE FROM ZTPP_INPUTPLAN_H CLIENT SPECIFIED WHERE MANDT = SY-MANDT.
ENDFORM.                    " delete_old_inputplan
*&---------------------------------------------------------------------*
*&      Form  read_vehicle
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_VEHICLE.
  DATA: L_OBJEK              LIKE AUSP-OBJEK,
         L_OBJID              LIKE CRHD-OBJID,
         L_CLINT              LIKE KLAH-CLINT,
         L_ATINN1             LIKE AUSP-ATINN,    " Internal Color
         L_ATINN2             LIKE AUSP-ATINN,    " Plan Order
         L_ATINN3             LIKE AUSP-ATINN,    " MITU
         L_ATINN4             LIKE AUSP-ATINN,    " Version
         L_ATINN5             LIKE AUSP-ATINN,    " Model Index
         L_ATINN6             LIKE AUSP-ATINN,    " OCN
         L_ATINN7             LIKE AUSP-ATINN,    " Serial Code
         L_ATINN8             LIKE AUSP-ATINN,    " Work Order
         L_ATINN9             LIKE AUSP-ATINN,    " External Color
         L_ATINNA             LIKE AUSP-ATINN,    " Status
         L_ATINN01            LIKE AUSP-ATINN,              " RP01
         L_ATINN02            LIKE AUSP-ATINN,              " RP02
         L_ATINN03            LIKE AUSP-ATINN,              " RP03
         L_ATINN04            LIKE AUSP-ATINN,              " RP04
         L_ATINN05            LIKE AUSP-ATINN,              " RP05
         L_ATINN06            LIKE AUSP-ATINN,              " RP06
         L_ATINN07            LIKE AUSP-ATINN,              " RP07
         L_ATINN08            LIKE AUSP-ATINN,              " RP08
         L_ATINN09            LIKE AUSP-ATINN,              " RP09
         L_ATINN10            LIKE AUSP-ATINN,              " RP10
         L_ATINN11            LIKE AUSP-ATINN,              " RP11
         L_ATINN12            LIKE AUSP-ATINN,              " RP12
         L_ATINN13            LIKE AUSP-ATINN,              " RP13
         L_ATINN14            LIKE AUSP-ATINN,              " RP14
         L_ATINN15            LIKE AUSP-ATINN,              " RP15
         L_ATINN16            LIKE AUSP-ATINN,              " RP16
         L_ATINN17            LIKE AUSP-ATINN,              " RP17
         L_ATINN18            LIKE AUSP-ATINN,              " RP18
         L_ATINN19            LIKE AUSP-ATINN,    " P_USAGE_CAR
         L_ATINN20            LIKE AUSP-ATINN,    " P_STATUS
         L_ATINN21            LIKE AUSP-ATINN,    " Model
         L_ATINN22            LIKE AUSP-ATINN,    " Body-Serial
         L_ATINN23            LIKE AUSP-ATINN,    " SEQUENCE_DATE
         L_ATINN24            LIKE AUSP-ATINN,    " SEQUENCE_SERIAL
         L_COUNT              TYPE I         ,
         L_VALS(8)            TYPE N         ,
         L_DATE               TYPE D         .

  DATA: BEGIN OF L1_KEYS     OCCURS 0,
          OBJEK              LIKE AUSP-ATWRT,
        END OF L1_KEYS.

  PERFORM CALL_ATINN USING 'P_INT_COLOR'         L_ATINN1.
  PERFORM CALL_ATINN USING 'P_PLAN_ORDER'        L_ATINN2.
  PERFORM CALL_ATINN USING 'P_MITU'              L_ATINN3.
  PERFORM CALL_ATINN USING 'P_VERSION'           L_ATINN4.
  PERFORM CALL_ATINN USING 'P_MI'                L_ATINN5.
  PERFORM CALL_ATINN USING 'P_OCN'               L_ATINN6.
  PERFORM CALL_ATINN USING 'P_SEQUENCE_CODE'     L_ATINN7.
  PERFORM CALL_ATINN USING 'P_WORK_ORDER'        L_ATINN8.
  PERFORM CALL_ATINN USING 'P_EXT_COLOR'         L_ATINN9.
  PERFORM CALL_ATINN USING 'P_RP_STATUS'         L_ATINNA.
  PERFORM CALL_ATINN USING 'P_USAGE_CAR'         L_ATINN19.
  PERFORM CALL_ATINN USING 'P_STATUS'            L_ATINN20.
  PERFORM CALL_ATINN USING 'P_MODEL'             L_ATINN21.
  PERFORM CALL_ATINN USING 'P_BODY_SERIAL'       L_ATINN22.
  PERFORM CALL_ATINN USING 'P_SEQUENCE_DATE'     L_ATINN23.
  PERFORM CALL_ATINN USING 'P_SEQUENCE_SERIAL'   L_ATINN24.
  PERFORM CALL_ATINN USING 'P_RP01_ACTUAL_DATE'  L_ATINN01.
  PERFORM CALL_ATINN USING 'P_RP02_ACTUAL_DATE'  L_ATINN02.
  PERFORM CALL_ATINN USING 'P_RP03_ACTUAL_DATE'  L_ATINN03.
  PERFORM CALL_ATINN USING 'P_RP04_ACTUAL_DATE'  L_ATINN04.
  PERFORM CALL_ATINN USING 'P_RP05_ACTUAL_DATE'  L_ATINN05.
  PERFORM CALL_ATINN USING 'P_RP06_ACTUAL_DATE'  L_ATINN06.
  PERFORM CALL_ATINN USING 'P_RP07_ACTUAL_DATE'  L_ATINN07.
  PERFORM CALL_ATINN USING 'P_RP08_ACTUAL_DATE'  L_ATINN08.
  PERFORM CALL_ATINN USING 'P_RP09_ACTUAL_DATE'  L_ATINN09.
  PERFORM CALL_ATINN USING 'P_RP10_ACTUAL_DATE'  L_ATINN10.
  PERFORM CALL_ATINN USING 'P_RP11_ACTUAL_DATE'  L_ATINN11.
  PERFORM CALL_ATINN USING 'P_RP12_ACTUAL_DATE'  L_ATINN12.
  PERFORM CALL_ATINN USING 'P_RP13_ACTUAL_DATE'  L_ATINN13.
  PERFORM CALL_ATINN USING 'P_RP14_ACTUAL_DATE'  L_ATINN14.
  PERFORM CALL_ATINN USING 'P_RP15_ACTUAL_DATE'  L_ATINN15.
  PERFORM CALL_ATINN USING 'P_RP16_ACTUAL_DATE'  L_ATINN16.
  PERFORM CALL_ATINN USING 'P_RP17_ACTUAL_DATE'  L_ATINN17.
  PERFORM CALL_ATINN USING 'P_RP18_ACTUAL_DATE'  L_ATINN18.

  SELECT SINGLE CLINT INTO L_CLINT
    FROM KLAH
   WHERE CLASS = 'P_VEHICLE_MASTER'.

  SELECT OBJEK INTO TABLE L1_KEYS
    FROM AUSP
   WHERE KLART = '002'
     AND ATINN = L_ATINNA
     AND ATWRT < '19'.

  LOOP AT L1_KEYS.
    SELECT OBJEK ATINN ATWRT ATFLV APPENDING TABLE IT_TEMP
      FROM AUSP
     WHERE OBJEK =  L1_KEYS-OBJEK
       AND KLART = '002' .
  ENDLOOP.

  SORT IT_TEMP BY OBJEK.
  READ TABLE IT_TEMP INDEX 1 .
  L_OBJEK = IT_TEMP-OBJEK    .

  LOOP AT IT_TEMP.
    IF L_OBJEK NE IT_TEMP-OBJEK         .
      IT_DATA-OBJEK = L_OBJEK           .
*      IF it_data-status = space OR it_data-status = '00'.
*        it_data-status = '00'   .
*      ENDIF.

      APPEND IT_DATA.
      CLEAR: IT_DATA.
      L_OBJEK = IT_TEMP-OBJEK .
    ENDIF.
    CASE IT_TEMP-ATINN.
      WHEN L_ATINN2   .                   " PLAN ORDER
        IT_DATA-PLNUM = IT_TEMP-ATWRT(10).
      WHEN L_ATINN3   .                   " MITU
        IT_DATA-MITU = IT_TEMP-ATWRT(1)  .
      WHEN L_ATINN1   .                   " Internal Color
        IT_DATA-INTC   = IT_TEMP-ATWRT(3) .
      WHEN L_ATINN4   .                   " External Color
        IT_DATA-VERS   = IT_TEMP-ATWRT(3) .
      WHEN L_ATINN5   .                   " MI
        IT_DATA-MI     = IT_TEMP-ATWRT(9) .
      WHEN L_ATINN6   .                   " OCNN
        IT_DATA-OCNN   = IT_TEMP-ATWRT(4).
      WHEN L_ATINN7   .                   " SEQ_CODE
        IT_DATA-SEQ_CODE = IT_TEMP-ATWRT(2).
      WHEN L_ATINN8   .                   " Work Order
        IT_DATA-WORK_ORDER = IT_TEMP-ATWRT(14).
      WHEN L_ATINN9   .                   " External Color
        IT_DATA-EXTC   = IT_TEMP-ATWRT(3) .
      WHEN L_ATINNA   .                   " Status
        IT_DATA-STATUS = IT_TEMP-ATWRT(2) .
      WHEN L_ATINN01  .                                     " RP01
        IT_DATA-RP01   = IT_TEMP-ATWRT(14).
      WHEN L_ATINN02  .                                     " RP02
        IT_DATA-RP02  = IT_TEMP-ATWRT(14).
      WHEN L_ATINN03  .                                     " RP03
        IT_DATA-RP03  = IT_TEMP-ATWRT(14).
      WHEN L_ATINN04  .                                     " RP04
        IT_DATA-RP04 = IT_TEMP-ATWRT(14).
      WHEN L_ATINN05  .                                     " RP05
        IT_DATA-RP05 = IT_TEMP-ATWRT(14).
      WHEN L_ATINN06  .                                     " RP06
        IT_DATA-RP06 = IT_TEMP-ATWRT(14).
      WHEN L_ATINN07  .                                     " RP07
        IT_DATA-RP07 = IT_TEMP-ATWRT(14).
      WHEN L_ATINN08  .                                     " RP08
        IT_DATA-RP08 = IT_TEMP-ATWRT(14).
      WHEN L_ATINN09  .                                     " RP09
        IT_DATA-RP09 = IT_TEMP-ATWRT(14).
      WHEN L_ATINN10  .                                     " RP10
        IT_DATA-RP10 = IT_TEMP-ATWRT(14).
      WHEN L_ATINN11  .                                     " RP11
        IT_DATA-RP11 = IT_TEMP-ATWRT(14).
      WHEN L_ATINN12  .                                     " RP12
        IT_DATA-RP12 = IT_TEMP-ATWRT(14).
      WHEN L_ATINN13  .                                     " RP13
        IT_DATA-RP13 = IT_TEMP-ATWRT(14).
      WHEN L_ATINN14  .                                     " RP14
        IT_DATA-RP14 = IT_TEMP-ATWRT(14).
      WHEN L_ATINN15  .                                     " RP15
        IT_DATA-RP15 = IT_TEMP-ATWRT(14).
      WHEN L_ATINN16  .                                     " RP16
        IT_DATA-RP16 = IT_TEMP-ATWRT(14).
      WHEN L_ATINN17  .                                     " RP17
        IT_DATA-RP17 = IT_TEMP-ATWRT(14).
      WHEN L_ATINN18  .                                     " RP18
        IT_DATA-RP18  = IT_TEMP-ATWRT(14).
      WHEN L_ATINN19  .                                " P_USAGE_CAR
        IT_DATA-USAGE = IT_TEMP-ATWRT(01).
      WHEN L_ATINN20  .                                " P_STATUS
        IT_DATA-B     = IT_TEMP-ATWRT    .
      WHEN L_ATINN21  .                                     " Model
        IT_DATA-MODL  = IT_TEMP-ATWRT(03).
      WHEN L_ATINN22  .                               " Body-Serial
        IT_DATA-BODY_SER  = IT_TEMP-ATWRT(06).
      WHEN L_ATINN23  .                               " SEQUENCE_DATE
        IT_DATA-SEQ_DATE  = L_DATE  = L_VALS = IT_TEMP-ATFLV    .
      WHEN L_ATINN24  .                               " SEQUENCE_SERIAL
        IT_DATA-SEQ_SERIAL = IT_TEMP-ATWRT(05).
        " SERIAL         .
    ENDCASE.
  ENDLOOP.

  DESCRIBE TABLE IT_TEMP LINES L_COUNT .
  IF L_COUNT > 0 .
    IT_DATA-OBJEK = L_OBJEK .
    APPEND IT_DATA.
  ENDIF.

ENDFORM.                    " read_vehicle
*&---------------------------------------------------------------------*
*&      Form  call_atinn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0339   text
*      -->P_L_ATINN1  text
*----------------------------------------------------------------------*
FORM CALL_ATINN USING    PA_ATNAM  PA_ATINN.
  CLEAR: PA_ATINN.
  SELECT SINGLE ATINN INTO PA_ATINN
    FROM CABN
   WHERE ATNAM = PA_ATNAM.
ENDFORM.                    " call_atinn

*&---------------------------------------------------------------------*
*&      Form  generate_inputplan
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GENERATE_INPUTPLAN.
  DATA: L_SDAT            LIKE SY-DATUM         ,
        L_TIME            LIKE SY-UZEIT         ,
        L_ADDTIME         LIKE SY-UZEIT         ,
        L_FLAG            TYPE C                ,
        L_FLAG2           TYPE C                ,
        L_DNAME(30)       TYPE C                ,
        L_NO(2)           TYPE N                ,
        L_PRE(2)          TYPE N                ,
        L_WBS             LIKE ZTPP_COMMON_VALS-ITEM4,
        L_PRJ             LIKE ZTPP_COMMON_VALS-ITEM4,
        L_MODEL           LIKE ZTPP_COMMON_VALS-KEY3 ,
        L_KALID           LIKE KAKO-KALID            ,
        L_PCNT            TYPE I                     ,
        L_BCNT            TYPE I                     ,
        L_COUNT           TYPE I                     ,
        L_STATUS          LIKE ZTPP_DVRT1-STATUS,
        L_7JB             LIKE TABLE OF ZTPP_PMT07JB_A WITH HEADER LINE,
        L_DATA            LIKE TABLE OF IT_DATA        WITH HEADER LINE.

  " Make the DVRT Layout
  SORT IT_DATA BY STATUS DESCENDING RP17 RP16 RP15 RP14 RP13 RP12 RP11
            RP10 RP09 RP08 RP07 RP06 RP05 RP04 RP03 RP02 RP01 MITU
            SEQ_DATE SEQ_SERIAL.

  " Scarp-Car Delete...
  DELETE IT_DATA WHERE ( USAGE = 'S' OR USAGE = 'D' ).
  L_DATA[] = IT_DATA[].

  " Fill the Time-Stamp.... in the production-line..
  CLEAR: WA_SERIAL.
  LOOP AT L_DATA .
    WA_SERIAL = WA_SERIAL +  1 .
    L_DATA-SERIAL = WA_SERIAL.
    L_DATA-RS18   = 'A' .
    MODIFY L_DATA.
    MOVE-CORRESPONDING L_DATA TO WA_DATA.
  ENDLOOP.

*  SELECT * INTO TABLE L_7JB
*    FROM ZTPP_PMT07JB_A
*   WHERE GUBB = 'A'.
*
*  SORT L_7JB BY SQDT SSR1 SSR2.
*
*  LOOP AT L_7JB.
*    CLEAR: L_DATA.
*    MOVE-CORRESPONDING L_7JB TO L_DATA .
*    WA_SERIAL         = WA_SERIAL +  1 .
*    L_DATA-SEQ_DATE   = L_7JB-SQDT     .
*    L_DATA-SEQ_SERIAL = L_7JB-SSR1     .
*    L_DATA-SEQ_CODE   = L_7JB-SQCD     .
*    L_DATA-SERIAL     = WA_SERIAL      .
*    IF L_7JB-GUBB     = 'A' AND L_7JB-GUB1 = '2'.
*      L_DATA-RS18     = 'B' .
*    ELSE.
*      L_DATA-RS18     = 'A' .
*    ENDIF.
*    CONCATENATE L_7JB-ORDR L_7JB-DIST  INTO L_DATA-WORK_ORDER.
*    IF L_7JB-ORDR+(1) = 'F'.
*      L_DATA-MI = L_7JB-BMDL.
*    ELSE.
*      PERFORM GET_MI    USING L_DATA-WORK_ORDER L_DATA-EXTC L_DATA-INTC
*                              L_DATA-MI  .
*    ENDIF.
*    APPEND L_DATA.
*  ENDLOOP.

  " Set the Paint-Reject Vehicle and WBS Vehicle in the T Field
*  PERFORM GET_WAIT_TIME     USING 'PRJ'  L_PRJ     .
*  PERFORM GET_WAIT_TIME     USING 'WBS'  L_WBS     .
*  PERFORM read_shop_calid   USING  L_kalid         .
*  PERFORM GET_MASTER        using  l_pcnt l_bcnt L_kalid L_WBS L_PRJ .
  SORT L_DATA BY SERIAL.
  LOOP AT L_DATA WHERE STATUS = '01' .     " WBS Setting..
    IF L_MODEL NE L_DATA-MODL .
      L_MODEL = L_DATA-MODL .
      PERFORM GET_WAIT_TIME    USING 'PRJ'  L_PRJ L_MODEL      .
      PERFORM GET_WAIT_TIME    USING 'WBS'  L_WBS L_MODEL      .
      PERFORM READ_SHOP_CALID  USING  L_KALID                  .
      PERFORM GET_MASTER       USING  L_PCNT L_BCNT L_KALID
                                      L_WBS L_PRJ L_MODEL      .
    ENDIF.
    L_COUNT = L_COUNT + 1.
    IF L_COUNT > L_BCNT  .
      L_DATA-T = 'WBS'   .
      MODIFY L_DATA      .
    ENDIF.
  ENDLOOP.

  CLEAR: L_MODEL.

  LOOP AT L_DATA WHERE STATUS > '01' AND STATUS < '04'.  " PRJ Setting..
    IF L_MODEL NE L_DATA-MODL .
      L_MODEL = L_DATA-MODL .
      PERFORM GET_WAIT_TIME    USING 'PRJ'  L_PRJ L_MODEL      .
      PERFORM GET_WAIT_TIME    USING 'WBS'  L_WBS L_MODEL      .
      PERFORM READ_SHOP_CALID  USING  L_KALID                  .
      PERFORM GET_MASTER       USING  L_PCNT L_BCNT L_KALID
                                      L_WBS L_PRJ L_MODEL      .
    ENDIF.
    L_COUNT = L_COUNT + 1.
    IF L_COUNT > L_PCNT  .
      L_DATA-T = 'PRJ'   .
      MODIFY L_DATA      .
    ENDIF.
  ENDLOOP.
  IT_DATA[] = L_DATA[].
  CLEAR: L_DATA, L_DATA[].

  LOOP AT IT_DATA.
    MOVE-CORRESPONDING IT_DATA TO ZTPP_INPUTPLAN_H.
    ZTPP_INPUTPLAN_H-ZEDAT = W_DATE.
    ZTPP_INPUTPLAN_H-ZETIM = W_TIME.
    MODIFY ZTPP_INPUTPLAN_H FROM ZTPP_INPUTPLAN_H.
  ENDLOOP.

ENDFORM.                    " generate_inputplan

*&---------------------------------------------------------------------*
*&      Form  GET_MI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_DATA_WORK_ORDER  text
*      -->P_L_DATA_EXTC  text
*      -->P_L_DATA_INTC  text
*      -->P_L_DATA_MI  text
*----------------------------------------------------------------------*
FORM GET_MI USING    PA_WORDER  PA_EXTC  PA_INTC  PA_MI.
  DATA: L_VALS       LIKE TABLE OF ZSPP_VIN_VALUE WITH HEADER LINE,
        L_WORDER     LIKE MARA-MATNR.

  CLEAR: L_VALS, L_VALS[].
  CONCATENATE PA_WORDER PA_EXTC PA_INTC INTO L_WORDER.

  L_VALS-ATNAM = 'P_MI'  .    APPEND L_VALS.
  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            OBJECT       = L_WORDER
            CTYPE        = '001'
       TABLES
            VAL_TABLE    = L_VALS
       EXCEPTIONS
            NO_DATA      = 1
            ERROR_MODE   = 2
            ERROR_OBJECT = 3
            ERROR_VALUE  = 4
            OTHERS       = 5.

  IF SY-SUBRC = 0.
    READ TABLE L_VALS INDEX 1 .
    PA_MI = L_VALS-ATWRT      .
  ENDIF.
ENDFORM.                    " GET_MI

*&---------------------------------------------------------------------*
*&      Form  GET_WAIT_TIME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0874   text
*      -->P_L_PRJ  text
*----------------------------------------------------------------------*
FORM GET_WAIT_TIME USING    PA_VAL  PA_RETURN  PA_MODEL.
  CLEAR: PA_RETURN .

  SELECT SINGLE ITEM4  INTO PA_RETURN
    FROM ZTPP_COMMON_VALS
   WHERE JOBS = 'ZAPP903R_INPUT_PLAN'
     AND KEY2 = PA_VAL
     AND KEY3 = PA_MODEL.
ENDFORM.                    " GET_WAIT_TIME
*&---------------------------------------------------------------------*
*&      Form  read_shop_calid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_KALID  text
*----------------------------------------------------------------------*
FORM READ_SHOP_CALID USING    PA_KALID.
  SELECT SINGLE FABKL INTO PA_KALID
    FROM T001W
   WHERE WERKS = 'P001' .
ENDFORM.                    " READ_SHOP_CALID

*&---------------------------------------------------------------------*
*&      Form  GET_MASTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_COUNT  text
*----------------------------------------------------------------------*
FORM GET_MASTER USING    PA_PCNT  PA_BCNT  PA_KALID
                         PA_WBS  PA_PRJ    PA_MODEL .
  DATA: L_DATE               TYPE D ,
        L_CHK                TYPE P DECIMALS 3,
        L_DAY                LIKE KAPA-TAGNR,      " Day
        L_TIME               TYPE KAPENDZT  ,      " Times for working
        L_UPH                TYPE ZVPP_LD-LRATE,   " UPH
        L_COUNT              TYPE I .

  L_DATE = SY-DATUM .
  PERFORM READ_WORKING_DATE USING '+'  PA_KALID  L_DATE.
  IF L_DATE = SY-DATUM .
    PERFORM GET_DAY          USING L_DATE  L_DAY         .
    PERFORM GET_WORKTIME     USING L_DATE  L_TIME  L_DAY  'B'.
    PERFORM GET_UPH          USING L_DATE  L_UPH   'B'   .
    L_CHK = L_TIME / 3600 .
    PA_BCNT = CEIL( L_UPH * L_CHK )  .
    PERFORM GET_WORKTIME     USING L_DATE  L_TIME  L_DAY  'T'.
    PERFORM GET_UPH          USING L_DATE  L_UPH   'T'   .
    L_CHK = L_TIME / 3600 .
    PA_BCNT = CEIL( L_UPH * L_CHK )  .
  ELSE.
    PA_BCNT = PA_PCNT = 0 .
  ENDIF.
ENDFORM.                    " GET_MASTER

*&---------------------------------------------------------------------*
*&      Form  read_working_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1055   text
*      -->P_PA_KALID  text
*      -->P_L_DATE  text
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
*&      Form  get_day
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_DATE  text
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
*&      Form  get_worktime
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_DATE  text
*      -->P_IT_MASTER_TIME  text
*      -->P_IT_MASTER_DAY  text
*----------------------------------------------------------------------*
FORM GET_WORKTIME     USING    PA_WDATE  PA_WKTIME  PA_DAY  PA_WC.
  DATA: L_WTIME       LIKE ZVPP_CAPACITY-ENDZT ,
        L_DATE        TYPE D ,
        L_FLAG        TYPE C ,
        L_EINZT       LIKE TC37A-EINZT ,
        LT_CAPA       LIKE TABLE OF ZVPP_CAPACITY      WITH HEADER LINE.

  CLEAR: LT_CAPA, LT_CAPA[], L_WTIME.
  SELECT * INTO TABLE LT_CAPA
    FROM ZVPP_CAPACITY
   WHERE ARBPL = PA_WC
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
    L_WTIME  = L_WTIME  + L_EINZT.
  ENDLOOP.
  PA_WKTIME = L_WTIME .
ENDFORM.                    " GET_WORKTIME

*&---------------------------------------------------------------------*
*&      Form  get_uph
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_DATE  text
*      -->P_IT_MASTER_UPH  text
*      -->P_IT_MASTER_SHIFT  text
*----------------------------------------------------------------------*
FORM GET_UPH USING    PA_WDATE  PA_UPH  PA_WC.
  DATA: W_UPH  LIKE ZTPP_STATUS-UPH.
  CALL FUNCTION 'Z_FPP_GET_UPH'
    EXPORTING
      DATE          = PA_WDATE
*       SHIFT         =
      SHOP          = PA_WC
    IMPORTING
      UPH           = W_UPH
            .
  PA_UPH = W_UPH.

*  DATA lw_ld          LIKE zvpp_ld .
*
*    SELECT SINGLE * INTO lw_ld
*      FROM zvpp_ld
*     WHERE ld_perst <= pa_wdate
*       AND ld_pered >= pa_wdate
*       AND arbpl     = PA_WC     .
*
*  IF lw_ld-lantu = 0.
*    pa_uph = 0 .
*  ELSE.
*    pa_uph = lw_ld-lrate / lw_ld-lantu .
*  ENDIF.
ENDFORM.                    " GET_UPH
*&---------------------------------------------------------------------*
*&      Form  check_holding_car
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_HOLDING_CAR.
  DATA : LT_HOLD LIKE ZTPP_HOLD_CAR OCCURS 0 WITH HEADER LINE.

  REFRESH : LT_HOLD.

  SELECT * INTO TABLE LT_HOLD FROM ZTPP_HOLD_CAR
         WHERE  ( STATUS NE 'D' )
             OR ( STATUS NE 'P' ).
*
*  SELECT * INTO TABLE it_data FROM ztpp_input_plan
*         WHERE mitu <> 'Y'
*           AND status < '06'.

  DELETE IT_DATA WHERE MITU = 'Y'
           OR STATUS >= '06'.

  LOOP AT IT_DATA.
    IF IT_DATA-WORK_ORDER+12(2) = 'XX' OR
       IT_DATA-WORK_ORDER+12(2) = 'XY' .
      DELETE IT_DATA.
    ENDIF.
  ENDLOOP.

  LOOP AT LT_HOLD WHERE RES_DATE > W_DATE.
    READ TABLE IT_DATA WITH KEY MODL = LT_HOLD-MODL
                            BODY_SER = LT_HOLD-BODY_SER.
    IF SY-SUBRC = 0.
*      it_hold-status = 'W'.
*      MODIFY it_hold FROM it_hold.

      UPDATE ZTPP_INPUTPLAN_H SET T = 'HOLD'
                               RSNUM = LT_HOLD-RES_DATE
               WHERE MODL = LT_HOLD-MODL
                 AND BODY_SER = LT_HOLD-BODY_SER.
      COMMIT WORK.
    ENDIF.
  ENDLOOP.

*  LOOP AT it_hold WHERE res_date <= wa_dates.
*    IF it_hold-res_date =  wa_dates.
*      it_hold-status = 'D'.
*    ELSE.
*      it_hold-status = 'P'.
*    ENDIF.
*    MODIFY it_hold FROM it_hold.
*  ENDLOOP.

*  MODIFY ztpp_hold_car FROM TABLE it_hold.
*  IF sy-subrc = 0.
*    MESSAGE s000 WITH text-003.
*    COMMIT WORK.
*  ELSE.
*    MESSAGE s000 WITH text-004.
*    ROLLBACK WORK.
*  ENDIF.

ENDFORM.                    " check_holding_car
*&---------------------------------------------------------------------*
*&      Form  display_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_PROCESS.

CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
 EXPORTING
*   PERCENTAGE       = 0
   TEXT             = 'Data in Processing'.


ENDFORM.                    " display_process
**&---------------------------------------------------------------------
**
**&      Module  STATUS_0800  OUTPUT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
*MODULE STATUS_0800 OUTPUT.
**  SET PF-STATUS 'xxxxxxxx'.
**  SET TITLEBAR 'xxx'.
**CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
** EXPORTING
***   PERCENTAGE       = 0
**   TEXT             = 'Data in Processing'.
*
*ENDMODULE.                 " STATUS_0800  OUTPUT
**&---------------------------------------------------------------------
**
**&      Module  USER_COMMAND_0800  INPUT
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
*MODULE USER_COMMAND_0800 INPUT.
*   LEAVE SCREEN.
*ENDMODULE.                 " USER_COMMAND_0800  INPUT
*&---------------------------------------------------------------------*
*&      Form  checking_batch_job
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM checking_batch_job.
 data: l_BACKJOB LIKE  SY-REPID,
       LT_JOBLIST LIKE TBTCJOB OCCURS 0 WITH HEADER LINE.
 l_BACKJOB = sy-repid.

 CALL FUNCTION 'BP_FIND_JOBS_WITH_PROGRAM'
     EXPORTING
      abap_program_name             = l_backjob
      dialog                        = 'N'
      status                        = 'R'
    TABLES
      joblist                       = lt_joblist
    EXCEPTIONS
      no_jobs_found                 = 1
      program_specification_missing = 2
      invalid_dialog_type           = 3
      job_find_canceled             = 4
      OTHERS                        = 5.

 IF SY-BATCH EQ 'X'.
    READ TABLE LT_JOBLIST INDEX 2.
    IF SY-SUBRC EQ 0.
      MESSAGE S999(PP) WITH TEXT-M01.
      LEAVE PROGRAM.
    ENDIF.
  ELSE.
    READ TABLE LT_JOBLIST INDEX 1.
    IF SY-SUBRC EQ 0.
      MESSAGE E999(PP) WITH TEXT-M01.
    ENDIF.
  ENDIF.

ENDFORM.                    " checking_batch_job
