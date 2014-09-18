*----------------------------------------------------------------------*
***INCLUDE ZAPP704C_INC_FUNCTION .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  READ_VEHICLE_MASTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_POINT  text
*----------------------------------------------------------------------*
FORM read_vehicle_master USING    pa_point.
  DATA: l_objek              LIKE ausp-objek,
        l_objid              LIKE crhd-objid,
        l_clint              LIKE klah-clint,
        l_atinn              LIKE ausp-atinn,    " Reporting Point
        l_atinn1             LIKE ausp-atinn,    " Internal Color
        l_atinn2             LIKE ausp-atinn,    " Plan Order
        l_atinn3             LIKE ausp-atinn,    " MITU
        l_atinn4             LIKE ausp-atinn,    " Version
        l_atinn5             LIKE ausp-atinn,    " Model Index
        l_atinn6             LIKE ausp-atinn,    " OCN
        l_atinn7             LIKE ausp-atinn,    " Serial Code
        l_atinn8             LIKE ausp-atinn,    " Work Order
        l_atinn9             LIKE ausp-atinn,    " External Color
        l_atinna             LIKE ausp-atinn,    " Status
        l_atinn01            LIKE ausp-atinn,               " RP01
        l_atinn02            LIKE ausp-atinn,               " RP02
        l_atinn03            LIKE ausp-atinn,               " RP03
        l_atinn04            LIKE ausp-atinn,               " RP04
        l_atinn05            LIKE ausp-atinn,               " RP05
        l_atinn06            LIKE ausp-atinn,               " RP06
        l_atinn07            LIKE ausp-atinn,               " RP07
        l_atinn08            LIKE ausp-atinn,               " RP08
        l_atinn09            LIKE ausp-atinn,               " RP09
        l_atinn10            LIKE ausp-atinn,               " RP10
        l_atinn11            LIKE ausp-atinn,               " RP11
        l_atinn12            LIKE ausp-atinn,               " RP12
        l_atinn13            LIKE ausp-atinn,               " RP13
        l_atinn14            LIKE ausp-atinn,               " RP14
        l_atinn15            LIKE ausp-atinn,               " RP15
        l_atinn16            LIKE ausp-atinn,               " RP16
        l_atinn17            LIKE ausp-atinn,               " RP17
        l_atinn18            LIKE ausp-atinn,               " RP18
        l_atinn19            LIKE ausp-atinn,    " Plant
        l_atinn20            LIKE ausp-atinn,    " Line
        l_atinn21            LIKE ausp-atinn,    " Model
        l_atinn22            LIKE ausp-atinn,    " Body-Serial
        l_atinn23            LIKE ausp-atinn,    " SEQUENCE_DATE
        l_atinn24            LIKE ausp-atinn,    " SEQUENCE_SERIAL
        l_moye               TYPE c         ,
        l_mi(8)              TYPE c         ,
        l_ocn(4)             TYPE n         ,
        l_destination(5)     TYPE c         ,
        l_count              TYPE i         ,
        l_vals(8)            TYPE n         ,
        l_date               TYPE d         ,
        l_name               LIKE ausp-atwrt.

  DATA: BEGIN OF l1_keys     OCCURS 0,
          objek              LIKE ausp-atwrt,
        END OF l1_keys.

  CONCATENATE 'P_RP' pa_point '_ACTUAL_DATE'  INTO  l_name.

  SELECT SINGLE atinn INTO l_atinn
    FROM cabn
   WHERE atnam = l_name               .           " P_RPxx_ACTUAL_DATE

  SELECT SINGLE atinn INTO l_atinn5
    FROM cabn
   WHERE atnam = 'P_MI'               .           " P_MI

  SELECT SINGLE atinn INTO l_atinn6
    FROM cabn
   WHERE atnam = 'P_OCN'              .           " P_OCN

  SELECT SINGLE atinn INTO l_atinn7
    FROM cabn
   WHERE atnam = 'P_SEQUENCE_CODE'    .           " P_SEQUENCE_CODE

  SELECT SINGLE atinn INTO l_atinna
    FROM cabn
   WHERE atnam = 'P_RP_STATUS'        .           " P_RP_STATUS

  SELECT SINGLE atinn INTO l_atinn4
    FROM cabn
   WHERE atnam = 'P_VERSION'          .           " P_VERSION

  SELECT SINGLE atinn INTO l_atinn3
    FROM cabn
   WHERE atnam = 'P_MITU'             .           " P_MITU

  SELECT SINGLE atinn INTO l_atinn2
    FROM cabn
   WHERE atnam = 'P_PLAN_ORDER'       .           " P_PLAN_ORDER

  SELECT SINGLE atinn INTO l_atinn1
    FROM cabn
   WHERE atnam = 'P_INT_COLOR'        .           " P_INT_COLOR

  SELECT SINGLE atinn INTO l_atinn9
    FROM cabn
   WHERE atnam = 'P_EXT_COLOR'        .           " P_EXT_COLOR

  SELECT SINGLE atinn INTO l_atinna
    FROM cabn
   WHERE atnam = 'P_RP_STATUS'        .           " P_RP_STATUS

  SELECT SINGLE atinn INTO l_atinn8
    FROM cabn
   WHERE atnam = 'P_WORK_ORDER'       .           " P_WORK_ORDER

  SELECT SINGLE atinn INTO l_atinn01
    FROM cabn
   WHERE atnam = 'P_RP01_ACTUAL_DATE' .           " Report 01

  SELECT SINGLE atinn INTO l_atinn02
    FROM cabn
   WHERE atnam = 'P_RP02_ACTUAL_DATE' .           " Report 02

  SELECT SINGLE atinn INTO l_atinn03
    FROM cabn
   WHERE atnam = 'P_RP03_ACTUAL_DATE' .           " Report 03

  SELECT SINGLE atinn INTO l_atinn04
    FROM cabn
   WHERE atnam = 'P_RP04_ACTUAL_DATE' .           " Report 04

  SELECT SINGLE atinn INTO l_atinn05
    FROM cabn
   WHERE atnam = 'P_RP05_ACTUAL_DATE' .           " Report 05

  SELECT SINGLE atinn INTO l_atinn06
    FROM cabn
   WHERE atnam = 'P_RP06_ACTUAL_DATE' .           " Report 06

  SELECT SINGLE atinn INTO l_atinn07
    FROM cabn
   WHERE atnam = 'P_RP07_ACTUAL_DATE' .           " Report 07

  SELECT SINGLE atinn INTO l_atinn08
    FROM cabn
   WHERE atnam = 'P_RP08_ACTUAL_DATE' .           " Report 08

  SELECT SINGLE atinn INTO l_atinn09
    FROM cabn
   WHERE atnam = 'P_RP09_ACTUAL_DATE' .           " Report 09

  SELECT SINGLE atinn INTO l_atinn10
    FROM cabn
   WHERE atnam = 'P_RP10_ACTUAL_DATE' .           " Report 10

  SELECT SINGLE atinn INTO l_atinn11
    FROM cabn
   WHERE atnam = 'P_RP11_ACTUAL_DATE' .           " Report 11

  SELECT SINGLE atinn INTO l_atinn12
    FROM cabn
   WHERE atnam = 'P_RP12_ACTUAL_DATE' .           " Report 12

  SELECT SINGLE atinn INTO l_atinn13
    FROM cabn
   WHERE atnam = 'P_RP13_ACTUAL_DATE' .           " Report 13

  SELECT SINGLE atinn INTO l_atinn14
    FROM cabn
   WHERE atnam = 'P_RP14_ACTUAL_DATE' .           " Report 14

  SELECT SINGLE atinn INTO l_atinn15
    FROM cabn
   WHERE atnam = 'P_RP15_ACTUAL_DATE' .           " Report 15

  SELECT SINGLE atinn INTO l_atinn16
    FROM cabn
   WHERE atnam = 'P_RP16_ACTUAL_DATE' .           " Report 16

  SELECT SINGLE atinn INTO l_atinn17
    FROM cabn
   WHERE atnam = 'P_RP17_ACTUAL_DATE' .           " Report 17

  SELECT SINGLE atinn INTO l_atinn18
    FROM cabn
   WHERE atnam = 'P_RP18_ACTUAL_DATE' .           " Report 18

  SELECT SINGLE atinn INTO l_atinn19
    FROM cabn
   WHERE atnam = 'P_TRIM_PLANT_NO'    .           " Plant

  SELECT SINGLE atinn INTO l_atinn20
    FROM cabn
   WHERE atnam = 'P_TRIM_LINE_NO'     .           " Line

  SELECT SINGLE atinn INTO l_atinn21
    FROM cabn
   WHERE atnam = 'P_MODEL'            .           " Model

  SELECT SINGLE atinn INTO l_atinn22
    FROM cabn
   WHERE atnam = 'P_BODY_SERIAL'      .           " Body-Serial

  SELECT SINGLE atinn INTO l_atinn23
    FROM cabn
   WHERE atnam = 'P_SEQUENCE_DATE'    .           " SEQUENCE_DATE

  SELECT SINGLE atinn INTO l_atinn24
    FROM cabn
   WHERE atnam = 'P_SEQUENCE_SERIAL'  .           " SEQUENCE_SERIAL

  SELECT SINGLE clint INTO l_clint
    FROM klah
   WHERE class = 'P_VEHICLE_MASTER'.

  SELECT * INTO      CORRESPONDING FIELDS OF TABLE l1_keys
    FROM ausp
   WHERE objek IN ( select OBJEK from AUSP WHERE atinn = l_atinn2
                                             AND klart = '002'
                                             AND atwrt > space )
     and klart = '002'
     AND atinn = l_atinna
     AND atwrt < '18'    .

  LOOP AT l1_keys.
    SELECT * APPENDING CORRESPONDING FIELDS OF TABLE it_temp
      FROM ausp
     WHERE objek =  l1_keys-objek
       AND klart = '002' .
  ENDLOOP.

  SORT it_temp BY objek.
  READ TABLE it_temp INDEX 1 .
  l_objek = it_temp-objek    .

  LOOP AT it_temp.                        " PLAN ORDER / VIN ASSIGNMENT
    IF l_objek NE it_temp-objek         .
      it_data-objek = l_objek           .
      IF it_data-status = space OR it_data-status = '00'.
        it_data-rp_no  = it_data-status = '00'   .
        it_data-rp     = 'RP00' .
      ENDIF.
      APPEND it_data.
      CLEAR: it_data.
      l_objek = it_temp-objek .
    ENDIF.
    CASE it_temp-atinn.
      WHEN l_atinn2   .                   " PLAN ORDER
        it_data-plnum = it_temp-atwrt(10).
        SELECT SINGLE rsnum INTO it_data-rsnum
          FROM plaf
         WHERE plnum = it_data-plnum .
      WHEN l_atinn3   .                   " MITU
        it_data-mitu = it_temp-atwrt(1)  .
      WHEN l_atinn1   .                   " Internal Color
        it_data-intc   = it_temp-atwrt(3) .
      WHEN l_atinn4   .                   " External Color
        it_data-vers   = it_temp-atwrt(3) .
      WHEN l_atinn5   .                   " MI
        it_data-mi     = it_temp-atwrt(7) .
      WHEN l_atinn6   .                   " OCNN
        it_data-ocnn   = it_temp-atwrt(4).
      WHEN l_atinn7   .                   " SEQ_CODE
        it_data-seq_code = it_temp-atwrt(2).
      WHEN l_atinn8   .                   " Work Order
        it_data-work_order = it_temp-atwrt(14).
      WHEN l_atinn9   .                   " External Color
        it_data-extc   = it_temp-atwrt(3) .
      WHEN l_atinna   .                   " Status
        it_data-status = it_temp-atwrt(2) .
        it_data-rp_no  = it_temp-atwrt(2) .
        CONCATENATE 'RP' it_temp-atwrt(2)  INTO it_data-rp.
      WHEN l_atinn01  .                                     " RP01
        it_data-rp01   = it_temp-atwrt(14).
      WHEN l_atinn02  .                                     " RP02
        it_data-rp02  = it_temp-atwrt(14).
      WHEN l_atinn03  .                                     " RP03
        it_data-rp03  = it_temp-atwrt(14).
      WHEN l_atinn04  .                                     " RP04
        it_data-rp04 = it_temp-atwrt(14).
      WHEN l_atinn05  .                                     " RP05
        it_data-rp05 = it_temp-atwrt(14).
      WHEN l_atinn06  .                                     " RP06
        it_data-rp06 = it_temp-atwrt(14).
      WHEN l_atinn07  .                                     " RP07
        it_data-rp07 = it_temp-atwrt(14).
      WHEN l_atinn08  .                                     " RP08
        it_data-rp08 = it_temp-atwrt(14).
      WHEN l_atinn09  .                                     " RP09
        it_data-rp09 = it_temp-atwrt(14).
      WHEN l_atinn10  .                                     " RP10
        it_data-rp10 = it_temp-atwrt(14).
      WHEN l_atinn11  .                                     " RP11
        it_data-rp11 = it_temp-atwrt(14).
      WHEN l_atinn12  .                                     " RP12
        it_data-rp12 = it_temp-atwrt(14).
      WHEN l_atinn13  .                                     " RP13
        it_data-rp13 = it_temp-atwrt(14).
      WHEN l_atinn14  .                                     " RP14
        it_data-rp14 = it_temp-atwrt(14).
      WHEN l_atinn15  .                                     " RP15
        it_data-rp15 = it_temp-atwrt(14).
      WHEN l_atinn16  .                                     " RP16
        it_data-rp16 = it_temp-atwrt(14).
      WHEN l_atinn17  .                                     " RP17
        it_data-rp17 = it_temp-atwrt(14).
      WHEN l_atinn18  .                                     " RP18
        it_data-rp18  = it_temp-atwrt(14).
      WHEN l_atinn19  .                                     " Plant
        it_data-plnt  = it_temp-atwrt(01).
      WHEN l_atinn20  .                                     " Line
        it_data-line  = it_temp-atwrt(01).
      WHEN l_atinn21  .                                     " Model
        it_data-modl  = it_temp-atwrt(03).
      WHEN l_atinn22  .                               " Body-Serial
        it_data-body_ser  = it_temp-atwrt(06).
      WHEN l_atinn23  .                               " SEQUENCE_DATE
        it_data-seq_date  = l_date  = l_vals = it_temp-atflv    .
      WHEN l_atinn24  .                               " SEQUENCE_SERIAL
        it_data-seq_serial = it_temp-atwrt(05).
    ENDCASE.
  ENDLOOP.

  DESCRIBE TABLE it_temp LINES l_count .
  IF l_count > 0 .
    it_data-objek = l_objek .
    IF it_data-status = space .
      it_data-rp_no  = it_data-status = '00'   .
      it_data-rp     = 'RP00' .
    ENDIF.
    APPEND it_data.
  ENDIF.
ENDFORM.                    " READ_VEHICLE_MASTER

*&---------------------------------------------------------------------*
*&      Form  CREATE_CAPACITY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_capacity .
  DATA: l_days             TYPE i  ,
        l_date             TYPE d  ,
        l_day              LIKE zvpp_capacity-tagnr,
        l_paunr            LIKE tc37p-paunr,
        l_tagnr            LIKE kapa-tagnr ,
        l_arbpl            LIKE crhd-arbpl ,
        lw_capa            LIKE zvpp_capacity,
        lt_capa            LIKE TABLE OF zvpp_capacity WITH HEADER LINE,
        lt_arbpl           LIKE TABLE OF crhd          WITH HEADER LINE.

  " Read the Shop's Capacity - Working & Break Time Shcedule..
  SELECT * INTO TABLE lt_arbpl
    FROM crhd
   WHERE arbpl IN s_arbpls
     AND verwe = '0011'   .

  LOOP AT lt_arbpl.
*   l_date = sy-datum - 1.  CLEAR: l_days.
    l_date = wa_wdate  - 1.  CLEAR: l_days.
    DO.
      l_date = l_date + 1  .
      PERFORM call_workday  USING lt_arbpl-arbpl  l_date      .
      l_days = l_days + 1  .
      IF l_days > 150      .    EXIT.      ENDIF              .
      PERFORM call_days USING l_date    l_day                 .
      READ TABLE it_mas_capa  WITH KEY arbpl = lt_arbpl-arbpl
                                       datub = l_date         .
      IF sy-subrc = 0.
        LOOP AT it_mas_capa WHERE arbpl = lt_arbpl-arbpl AND
                                  datub = l_date         AND
                                  tagnr = l_day              .
          MOVE-CORRESPONDING it_mas_capa TO it_tims.
          SELECT SINGLE begzt endzt paplan
            INTO (it_tims-s_stime, it_tims-s_etime, it_tims-sprog)
            FROM tc37a
           WHERE schgrup  = 'HA'
             AND kaptprog = it_tims-tprog
             AND begda   <= wa_wdate
             AND endda    = l_date     .
          PERFORM setup_time_intervals USING l_date sy-subrc      .
          PERFORM append_breaktime     USING l_day  lt_arbpl-arbpl.
        ENDLOOP.
      ELSE.
        SELECT SINGLE *  INTO lw_capa
          FROM zvpp_capacity
         WHERE arbpl = lt_arbpl-arbpl
           AND datub >= wa_wdate .

        LOOP AT it_mas_capa WHERE arbpl = lt_arbpl-arbpl AND
                                  datub = lw_capa-datub  AND
                                      "   l_date         AND
                                  tagnr = l_day              .
*         IF lw_capa-datub NE it_mas_capa-datub.
*           EXIT .
*         ENDIF.
          MOVE-CORRESPONDING it_mas_capa TO it_tims.
          SELECT SINGLE begzt endzt paplan
            INTO (it_tims-s_stime, it_tims-s_etime, it_tims-sprog)
            FROM tc37a
           WHERE schgrup  = 'HA'
             AND kaptprog = it_tims-tprog
             AND begda   <= wa_wdate
             AND endda   >= wa_wdate    .
          IF it_mas_capa-tprog = space.
            PERFORM setup_time_interval2 USING l_date sy-subrc      .
*           PERFORM append_breaktime     USING l_date lt_arbpl-arbpl.
          ELSE.
            PERFORM setup_time_intervals USING l_date sy-subrc      .
            PERFORM append_breaktime     USING l_date lt_arbpl-arbpl.
          ENDIF.
        ENDLOOP.
      ENDIF.
      CLEAR: it_37p, it_37p[].
    ENDDO.
  ENDLOOP.
ENDFORM.                    " CREATE_CAPACITY

*&---------------------------------------------------------------------*
*&      Form  CHECK_TIMESTAMP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_SDAT  text
*      -->P_L_TIME  text
*      -->P_L_ADDTIME  text
*----------------------------------------------------------------------*
FORM check_timestamp USING    pa_date  pa_time  pa_gap  pa_arbpl .
  TABLES: tfacs.

  DATA: l_tims              LIKE it_tims, " Working Area for IT_TIMS
        l_cdate             TYPE d ,      " Check date for the Working..
        l_day               LIKE scal-indicator,
        l_days              LIKE tc37p-paunr   ,
        l_ctimestamp(14)    TYPE c,
        l_date              TYPE d ,
        l_flag              TYPE c ,
        l_gap               LIKE kapa-begzt,
        ls_time             TYPE t ,
        lc_time             TYPE t ,
        l_time              TYPE t .

  CHECK pa_gap > 0 .

  l_date = pa_date.         l_time = pa_time.
* PERFORM call_addtime  USING l_date l_time pa_gap .

  " Check the Date...   --> Holiday
* PERFORM call_workday  USING pa_arbpl l_date      .

  " Check the Working Time...
  CALL FUNCTION 'DATE_COMPUTE_DAY'
       EXPORTING
            date = l_date
       IMPORTING
            day  = l_day.

*  l_days = l_day .   CLEAR: l_flag.
  l_gap   = pa_gap.
  PERFORM check_workstamp       USING  l_date  l_day  pa_arbpl
                                       pa_time l_time l_gap   .
  pa_date = l_date.        pa_time = l_time.
ENDFORM.                    " CHECK_TIMESTAMP

*&---------------------------------------------------------------------*
*&      Form  check_overday
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TIMS_C_STIME  text
*      -->P_IT_TIMS_C_ETIME  text
*----------------------------------------------------------------------*
FORM check_overday USING    pa_stime  pa_etime.
  DATA: l_day               LIKE kapa-begzt.

  l_day = '240000' .
  IF pa_stime > pa_etime .
    pa_etime = pa_etime + l_day.
  ENDIF.
ENDFORM.                    " check_overday

*&---------------------------------------------------------------------*
*&      Form  check_break
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PA_TIME  text
*      -->P_L_TIME  text
*      -->P_L_FLAG  text
*----------------------------------------------------------------------*
FORM check_break  USING  pa_day  pa_stime  pa_etime
                        pa_time pa_flag   pa_arbpl .
  DATA: l_time              TYPE t         ,
        l_date              TYPE d         ,
        l_check             TYPE c         ,
        l_gap               LIKE kapa-begzt.

*  loop at it_break where tagnr = pa_day and arbpl = pa_arbpl
*                     and w_stime >= pa_stime
*                     and w_etime <= pa_etime .
*    l_gap = l_gap + ( it_break-w_etime - it_break-w_stime ) .
*  endloop.
*
*  " 추가한 시간이 다음의 휴식 시간 범위에
*걸리는가??? ..."
*  l_time  = pa_etime + l_gap .
*  if pe_time < l_time.
*    " Over days.....
*  endif.
*
*  loop at it_break where tagnr = pa_day and arbpl = pa_arbpl
*                    and w_stime >= l_time
*                    and w_etime <= pa_etime .
*    pa_flag = 'X' .
*  endloop.

  LOOP AT it_break WHERE wdate >= pa_day AND arbpl = pa_arbpl
                     AND w_stime >= pa_stime .
    l_gap = l_gap + ( it_break-w_etime - it_break-w_stime ) .
  ENDLOOP.

  " 추가한 시간이 다음의 휴식 시간 범위에
  "걸리는가??? ..."
* perform call_addtime  using pa_day pa_etime l_gap.
  CALL FUNCTION 'C14B_ADD_TIME'
       EXPORTING
            i_starttime = pa_etime
            i_startdate = pa_day
            i_addtime   = l_gap
       IMPORTING
            e_endtime   = l_time
            e_enddate   = l_date.

  IF pa_day NE l_date.
    " Over days.....
  ENDIF.

  l_check = 'X' .    CLEAR: l_gap.

  DO.
    LOOP AT it_break WHERE wdate >= l_date AND arbpl = pa_arbpl
                      AND w_etime >= l_time   .
      IF it_break-wdate = l_date AND it_break-w_stime <= l_time .
        " Break time
        l_gap = l_time - it_break-w_stime .
      ELSE.
        CLEAR: l_check .
      ENDIF.
      EXIT.                               " LOOP Exit...
    ENDLOOP.

    IF l_check IS INITIAL.
      EXIT.                              " DO loop exit...
    ELSE.
      l_time = it_break-w_etime + l_gap.
    ENDIF.
  ENDDO.
ENDFORM.                    " check_break

*&---------------------------------------------------------------------*
*&      Form  call_addtime
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_DATE  text
*      -->P_L_TIME  text
*      -->P_PA_GAP  text
*----------------------------------------------------------------------*
FORM call_addtime USING    pa_date  pa_time  pa_addtime
                           pa_ndate pa_ntime .
  DATA: l_gap               TYPE t .

  l_gap = pa_addtime .
  CALL FUNCTION 'C14B_ADD_TIME'
       EXPORTING
            i_starttime = pa_time
            i_startdate = pa_date
            i_addtime   = l_gap
       IMPORTING
            e_endtime   = pa_ntime
            e_enddate   = pa_ndate.
ENDFORM.                    " call_addtime

*&---------------------------------------------------------------------*
*&      Form  call_workday
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PA_ARBPL  text
*      -->P_L_DATE  text
*----------------------------------------------------------------------*
FORM call_workday USING    pa_arbpl  pa_date.
  DATA: l_ident             LIKE tfacs-ident.

  SELECT SINGLE kalid  INTO l_ident
    FROM zvpp_capacity
   WHERE arbpl = pa_arbpl .

  CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
       EXPORTING
            correct_option               = '+'
            date                         = pa_date
            factory_calendar_id          = l_ident
       IMPORTING
            date                         = pa_date
       EXCEPTIONS
            calendar_buffer_not_loadable = 1
            correct_option_invalid       = 2
            date_after_range             = 3
            date_before_range            = 4
            date_invalid                 = 5
            factory_calendar_not_found   = 6
            OTHERS                       = 7.
ENDFORM.                    " call_workday

*&---------------------------------------------------------------------*
*&      Form  call_days
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_DATE  text
*      -->P_L_DAYS  text
*----------------------------------------------------------------------*
FORM call_days USING    pa_date  pa_days.
  DATA: l_days               LIKE scal-indicator.

  CALL FUNCTION 'DATE_COMPUTE_DAY'
       EXPORTING
            date = pa_date
       IMPORTING
            day  = l_days.

  pa_days = l_days .
ENDFORM.                    " call_days

*&---------------------------------------------------------------------*
*&      Form  setup_time_interval2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM setup_time_interval2     USING pa_date  pa_subrc .
  DATA: l_line                TYPE i.

  it_tims-wdate      = pa_date             .
  it_tims-flag       = 'W'                 .
  it_tims-paunr      = 00                  .

  it_tims-w_stime = it_tims-c_stime = it_mas_capa-begzt  .
  it_tims-w_etime = it_tims-c_etime = it_mas_capa-endzt  .
  APPEND it_tims.
  CLEAR: it_tims.
ENDFORM.                    " setup_time_interval2

*&---------------------------------------------------------------------*
*&      Form  setup_time_intervals
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM setup_time_intervals     USING pa_date  pa_subrc .
  DATA: l_line                TYPE i.

  it_tims-wdate      = pa_date             .
*   it_tims-s_stime    = it_mas_capa-begzt   .
*   it_tims-s_etime    = it_mas_capa-endzt   .
  it_tims-flag       = 'W'                 .
  it_tims-paunr      = 00                  .

  " Append Break-Time.
  SELECT *  INTO TABLE it_37p
    FROM tc37p
   WHERE schgrup = 'HA'
     AND paplan  = it_tims-sprog .

  DESCRIBE TABLE it_37p LINES l_line.
  IF l_line = 0.
    it_tims-w_stime = it_tims-c_stime = it_mas_capa-begzt  .
    it_tims-w_etime = it_tims-c_etime = it_mas_capa-endzt  .
    APPEND it_tims.
  ELSE.
    SORT it_37p BY paunr                 .
    READ TABLE it_37p INDEX 1            .
    IF it_tims-s_stime = it_37p-paubeg   .
      it_tims-w_stime = it_tims-c_stime = it_37p-paubeg   .
      it_tims-w_etime = it_tims-c_etime = it_37p-pauend   .
      it_tims-flag    = 'B'                               .
      DELETE it_37p  INDEX 1                              .
    ELSE.
      it_tims-w_stime = it_tims-c_stime = it_tims-s_stime .
      it_tims-w_etime = it_tims-c_etime = it_37p-paubeg   .
    ENDIF.
    APPEND it_tims.
    LOOP AT it_37p.
      it_tims-paunr   = it_37p-paunr                       .
      it_tims-w_stime = it_tims-c_stime = it_37p-paubeg    .
      it_tims-w_etime = it_tims-c_etime = it_37p-pauend    .
*       perform check_overday  using it_tims-c_stime it_tims-c_etime.
      it_tims-flag    = 'B'              .
      APPEND it_tims.
    ENDLOOP.
    it_tims-paunr   = it_37p-paunr + 1   .
    it_tims-w_stime = it_tims-c_stime = it_37p-pauend      .
    it_tims-w_etime = it_tims-c_etime = it_tims-s_etime    .
*     perform check_overday  using it_tims-c_stime it_tims-c_etime.
    it_tims-flag    = 'W'                .
    APPEND it_tims                       .
  ENDIF.
  CLEAR: it_tims.
ENDFORM.                    " setup_time_intervals

*&---------------------------------------------------------------------*
*&      Form  append_breaktime
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_breaktime        USING pa_day  pa_arbpl .
  DATA: l_paunr              LIKE tc37p-paunr,
        l_tims               LIKE it_tims,
        lc_tims              LIKE it_tims,
        l_cnt                TYPE i.

  DATA: lt_tims              LIKE TABLE OF it_tims     WITH HEADER LINE.

  CLEAR: l_cnt, l_paunr.
  LOOP AT it_tims WHERE wdate = pa_day  AND arbpl = pa_arbpl.
    l_cnt = l_cnt + 1.
    IF l_cnt = 1.
      l_tims  = lt_tims       = it_tims .
      l_paunr = lt_tims-paunr = 1.
      lt_tims-cdate = lt_tims-wdate     .
      APPEND lt_tims   .
      lc_tims = lt_tims.
      DELETE it_tims   .
      CONTINUE.
    ENDIF.
    IF lt_tims-w_etime = it_tims-w_stime.
      l_tims  = lt_tims = it_tims      .
      l_paunr = l_paunr + 1.
      lt_tims-paunr = l_paunr          .
*     lt_tims-flag    = 'W'            .
      IF lt_tims-w_stime < lc_tims-w_stime.
        lt_tims-cdate   = lt_tims-wdate + 1.
      ELSE.
        lt_tims-cdate   = lc_tims-cdate    .
      ENDIF.
      APPEND lt_tims . lc_tims = lt_tims.
    ELSE.
      lt_tims = it_tims                .
      lt_tims-flag    = 'W'            .
      lt_tims-w_stime = it_tims-c_stime = l_tims-w_etime .
      lt_tims-w_etime = it_tims-c_etime = it_tims-w_stime.
      PERFORM check_overday  USING it_tims-c_stime it_tims-c_etime.
      l_paunr = l_paunr + 1            .
      lt_tims-paunr = l_paunr          .
      IF lt_tims-w_stime < lc_tims-w_stime.
        lt_tims-cdate   = lt_tims-wdate + 1.
      ELSE.
        lt_tims-cdate   = lc_tims-cdate    .
      ENDIF.
      APPEND lt_tims . lc_tims = lt_tims.
      IF lt_tims-w_etime = lt_tims-s_etime.
      ELSE.
        l_tims  = lt_tims = it_tims      .
        lt_tims-flag    = 'B'            .
        l_paunr = l_paunr + 1.
        lt_tims-paunr = l_paunr          .
        IF lt_tims-w_stime < lc_tims-w_stime.
          lt_tims-cdate   = lt_tims-wdate + 1.
        ELSE.
          lt_tims-cdate   = lc_tims-cdate    .
        ENDIF.
        APPEND lt_tims . lc_tims = lt_tims.
      ENDIF.
    ENDIF.
    DELETE it_tims   .
  ENDLOOP.
  APPEND LINES OF lt_tims  TO  it_tims.
ENDFORM.                    " append_breaktime

*&---------------------------------------------------------------------*
*&      Form  CHECK_WORKSTAMP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_DATE  text
*      -->P_L_DAY  text
*----------------------------------------------------------------------*
FORM check_workstamp USING    pa_date   pa_day    pa_arbpl
                              pa_stime  pa_etime  pa_gap .
  DATA: l_sdate               TYPE d,
        l_edate               TYPE d,
        l_check               TYPE c,
        l_wflag               TYPE c,
*        l_loops               TYPE i,
        l_gap                 LIKE kapa-begzt,
*        l_test                LIKE kapa-begzt,
        l_cdate               TYPE d,
        l_ctime               TYPE t,
        l_stime               TYPE t,
        l_etime               TYPE t.

  " Check the Input-date for the Workday / Holiday...
  l_stime = pa_stime.         l_sdate = pa_date.
  l_etime = pa_stime.         l_edate = pa_date.  l_gap = pa_gap.

*  PERFORM call_workday  USING pa_arbpl l_sdate .
*  IF l_sdate = pa_date.  l_wflag = 'X'.  ENDIF.

  " Set the From Timestamp & To Timestamp....
  DO.
    PERFORM call_addtime  USING l_sdate l_stime  l_gap  l_edate l_etime.
    l_cdate = l_edate .  CLEAR: l_gap.
    PERFORM call_workday  USING pa_arbpl l_cdate .
    IF l_etime > wa_etime AND l_etime < wa_stime .
      l_edate = l_cdate.
      l_etime = l_etime - wa_etime .
      l_etime = l_etime + wa_stime .
    ENDIF.

    l_wflag = 'X'.
    LOOP AT it_tims WHERE arbpl    = pa_arbpl AND cdate   >= l_sdate
                      AND flag     = 'B' .
      " Finish condition...
      IF l_wflag = 'X' AND it_tims-w_stime < l_stime
                       AND it_tims-cdate = l_sdate.
        CONTINUE.
      ELSE.
        CLEAR: l_wflag.
      ENDIF.

      IF it_tims-cdate > l_edate OR it_tims-w_stime > l_etime.
        EXIT.
      ENDIF.
      IF l_etime <= it_tims-w_etime .
        l_gap = l_gap + ( l_etime         - it_tims-w_stime ) .
        l_etime = it_tims-w_etime   .
      ELSE.
        l_gap = l_gap + ( it_tims-w_etime - it_tims-w_stime ) .
      ENDIF.
    ENDLOOP.

    IF l_gap = 0 .  EXIT.  ENDIF.

    l_stime = l_etime .         l_sdate = l_edate.
  ENDDO.
  pa_etime = l_etime .        pa_date = l_edate .
ENDFORM.                    " CHECK_WORKSTAMP
