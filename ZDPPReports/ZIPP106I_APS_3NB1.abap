************************************************************************
* Program Name      : ZIPP106I_APS_3NB1
* Author            : Bobby
* Creation Date     : 2003.11.12
* Specifications By :
* Pattern           : 1.1
* Development Request No :  UD1K901977
* Addl Documentation:
* Description       : Create the contents of the Table ZTPP_PMT03NB
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT zipp106i_aps_3nb1    NO STANDARD PAGE HEADING MESSAGE-ID zmpp.
*                          LINE-SIZE 120
*----------------------------------------------------------------------*
*  EXTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
TABLES : ztpp_pmt03nb.

*----------------------------------------------------------------------*
*  INTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
DATA : it_dvrt2             LIKE TABLE OF ztpp_input_plan
                                                       WITH HEADER LINE,
       it_3nb               LIKE TABLE OF ztpp_pmt03nb WITH HEADER LINE.

DATA: BEGIN OF it_master  OCCURS 0,
        seq               TYPE i  ,             " Sequence
        date              TYPE d  ,             " Date
        day               LIKE kapa-tagnr,      " Day
        shift             LIKE kapa-schnr,      " Shift
        time              TYPE kapendzt  ,      " Times for working
        uph               TYPE zvpp_ld-lrate,   " UPH
        tqty              TYPE i  ,             " Day's Total Veh.
      END OF it_master.

DATA: BEGIN OF it_shift   OCCURS 0,
        seq               TYPE i  ,             " Sequence
        date              TYPE d  ,             " Date
        day               LIKE kapa-tagnr,      " Day
        shift             LIKE kapa-schnr,      " Shift
        time              TYPE kapendzt  ,      " Times for working(NET)
        total             TYPE kapendzt  ,      " Times for working(GRS)
        ftime             TYPE kapendzt  ,      " Start Time.
        uph               TYPE zvpp_ld-lrate,   " UPH
        tqty              TYPE i  ,             " Shift's Total Veh.
        hqty              TYPE i  ,             " Hour's Total Veh.
        hloop             TYPE i  ,             " Hour's LOOP.
      END OF it_shift .

*----------------------------------------------------------------------*
*  GLOBAL VARIABLE DECLARATION
*----------------------------------------------------------------------*
DATA: WA_COUNT            TYPE I                                   ,
      wa_kalid            LIKE kako-kalid                          ,
      wa_wdate            LIKE ztpp_day_sum-wdate                  .

*----------------------------------------------------------------------*
*  CONSTANS DECLARATION
*----------------------------------------------------------------------*
DATA : c_dest(10) VALUE 'WMPP01'.   "Outbound Interface Destination

*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME .
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_run          TYPE c AS CHECKBOX  DEFAULT 'X'.
SELECTION-SCREEN COMMENT  (55) text-001 FOR FIELD p_run.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK b1.

************************************************************************
* INITIALIZAION
************************************************************************
INITIALIZATION.
* PERFORM INITIALIZATION.

************************************************************************
* AT SELECTION-SCREEN
************************************************************************
AT SELECTION-SCREEN.
*  PERFORM AT_SELECTION-SCREEN.

************************************************************************
* START-OF-SELECTION
************************************************************************
START-OF-SELECTION.
  CHECK p_run = 'X'     .
  PERFORM get_master    .       " Get the Master Information(UPH/SHIFT)
  PERFORM get_data      .
  PERFORM excute_process.
  PERFORM save_3nb      .

************************************************************************
* END-OF-SELECTION
************************************************************************
END-OF-SELECTION.
*  PERFORM LIST_PROCESS.

*&---------------------------------------------------------------------*
*&      Form  EXCUTE_PROCESS
*&---------------------------------------------------------------------*
FORM excute_process.
  DATA: l_seqn               LIKE ztpp_pmt03nb-seqn.

  CLEAR: l_seqn   .
  LOOP AT it_dvrt2.
    CLEAR: it_3nb .
    l_seqn = l_seqn + 1.
    it_3nb-plnt  = '1' .
    it_3nb-seqn  = l_seqn.
    it_3nb-sqcd  = it_dvrt2-seq_code.
    it_3nb-line  = '1' .
    it_3nb-zmsg  = it_dvrt2-b     .
    IF it_dvrt2-body_ser NE space .
      CONCATENATE it_dvrt2-modl  it_dvrt2-body_ser  INTO it_3nb-vhno.
*      concatenate it_dvrt2-t(3)  it_dvrt2-body_ser  into it_3nb-vhno.
    ENDIF.
    it_3nb-ordr  = it_dvrt2-work_order(9).
    it_3nb-dist  = it_dvrt2-work_order+9(5).
    it_3nb-extc  = it_dvrt2-extc    .
    it_3nb-intc  = it_dvrt2-intc    .
    it_3nb-bmdl  = it_dvrt2-mi      .
    it_3nb-ocnn  = it_dvrt2-ocnn    .
    it_3nb-vers  = it_dvrt2-vers    .
    IF it_dvrt2-body_ser = space .
      it_3nb-flgu = 'B'.
    ELSE.
      it_3nb-flgu = 'A'.
    ENDIF.
    PERFORM check_status USING it_3nb-stgu it_dvrt2-mitu .
    it_3nb-lcgu  = 'A'.                 " it_dvrt2-seq_code.
    it_3nb-lot1  = it_3nb-lot2  = 0.    "
    PERFORM check_vin USING 'B01' '*' ' ' it_3nb-body .
    PERFORM check_vin USING 'T'   '*' ' ' it_3nb-ztrim.
    IF it_dvrt2-mitu = 'Y'.
      it_3nb-hold  = '*'    .
    ENDIF.
    PERFORM check_vin USING 'P37' '*' ' ' it_3nb-pbss .
    it_3nb-zdate = sy-datum  .
*    it_3nb-sdat  = it_dvrt2-seq_date .
    it_3nb-sdat  = it_dvrt2-rD18 .
    APPEND it_3nb.
  ENDLOOP.
ENDFORM.                    " EXCUTE_PROCESS

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_dvrt2
    FROM ztpp_input_plan .

  " Set the Execution Date in the field IT_DVRT2-SQDT
  SORT it_dvrt2 BY serial.
  PERFORM SET_DATE_FIELD .

  DELETE FROM ztpp_pmt03nb CLIENT SPECIFIED WHERE mandt = sy-mandt.
ENDFORM.                    " GET_DATA

*&---------------------------------------------------------------------*
*&      Form  check_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0132   text
*      -->P_0133   text
*      -->P_0134   text
*      -->P_IT_3NB_STGU  text
*----------------------------------------------------------------------*
FORM check_status USING   pa_field  pa_mitu  .
  IF pa_mitu = 'Y' .
    pa_field = 'C' .
    EXIT.
  ENDIF.

  IF it_dvrt2-status < '01' .
    pa_field = 'B'  .
  ELSE.
    pa_field = 'A'  .
  ENDIF.
ENDFORM.                    " check_status

*&---------------------------------------------------------------------*
*&      Form  CHECK_VIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0148   text
*      -->P_0149   text
*      -->P_0150   text
*      -->P_0151   text
*      -->P_IT_3NB_BODY  text
*----------------------------------------------------------------------*
FORM check_vin USING    pa_vals  pa_true  pa_false  pa_field .
  DATA: l_atwrt              LIKE ausp-atwrt.

  CLEAR: l_atwrt.
  l_atwrt = it_dvrt2-b .

  CASE pa_vals.
    WHEN 'T' .
      l_atwrt = l_atwrt(1) .
    WHEN OTHERS.
  ENDCASE.

  IF l_atwrt = pa_vals.
    pa_field = pa_true .
  ELSE.
    pa_field = pa_false.
  ENDIF.
ENDFORM.                    " CHECK_VIN

*&---------------------------------------------------------------------*
*&      Form  save_3nb
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_3nb.
  DATA: l_text(60) TYPE c,
        l_int TYPE i.
  MODIFY ztpp_pmt03nb FROM TABLE it_3nb .
  IF sy-subrc = 0.
    COMMIT WORK.
    DESCRIBE TABLE it_3nb LINES l_int.
    WRITE l_int TO l_text LEFT-JUSTIFIED .
    CONCATENATE 'Created Record Count :' l_text
      INTO l_text.
    MESSAGE s001 WITH l_text .
    MESSAGE s001 WITH text-101.
  ELSE.
    MESSAGE w001 WITH text-102.
  ENDIF.
ENDFORM.                                                    " save_3nb

*&---------------------------------------------------------------------*
*&      Form  GET_MASTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_master.
  DATA: l_chk                TYPE p DECIMALS 3,
        l_date               type d,
        L_COUNT              TYPE I.

  " Master Data for the 1 Year's Information
  l_date = wa_wdate =  sy-datum - 1 .
  CLEAR: l_count.
  PERFORM read_shop_calid   USING wa_kalid.
  DO 365 TIMES.
    l_count  = l_count + 1.      CLEAR: l_chk.
    l_date   =  l_date   + 1 .
    PERFORM read_work_date USING '+'  wa_kalid  l_date   .
    PERFORM get_day        USING l_date   it_master-day  .
    PERFORM get_worktime2  USING l_date   it_master-time it_master-day.
    PERFORM get_uph        USING l_date   it_master-uph it_master-shift.
    it_master-seq    = l_count.  it_master-date   = l_date   .
    l_chk = it_master-time / 3600 .
    it_master-tqty   = ceil( it_master-uph * l_chk )  .
    APPEND it_master.  CLEAR: it_master.
  ENDDO.
*
*  " Master Data for the 60 Day's Shift & UPH Information (Detail)
*  l_date = wa_wdate        .
*  CLEAR: l_count.
*  DO 60 TIMES.
*    L_COUNT = L_COUNT + 1 .
*    l_date   =  l_date   + 1 .
*    PERFORM read_work_date   USING '+'  wa_kalid   l_date   .
*    PERFORM get_day          USING l_date   it_shift-day    .
*   PERFORM get_uph          USING l_date   it_shift-uph it_shift-shift.
*    it_shift-seq    = l_count .   it_shift-date   = l_date   .
*   PERFORM get_worktime1    USING l_date   it_shift-time it_shift-day .
*    CLEAR: it_shift.
*  ENDDO.
ENDFORM.                    " GET_MASTER

*&---------------------------------------------------------------------*
*&      Form  READ_SHOP_CALID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_KALID  text
*----------------------------------------------------------------------*
FORM read_shop_calid USING    pa_kalid.
  SELECT SINGLE kalid INTO pa_kalid
    FROM zvpp_capacity
   WHERE arbpl = 'T'   .
ENDFORM.                    " READ_SHOP_CALID

*&---------------------------------------------------------------------*
*&      Form  READ_WORK_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
*----------------------------------------------------------------------*
FORM READ_WORK_DATE    USING  pa_type  pa_kalid  pa_wdate.
  CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
       EXPORTING
            correct_option               = pa_type
            date                         = pa_wdate
            factory_calendar_id          = pa_kalid
       IMPORTING
            date                         = pa_wdate
       EXCEPTIONS
            calendar_buffer_not_loadable = 1
            correct_option_invalid       = 2
            date_after_range             = 3
            date_before_range            = 4
            date_invalid                 = 5
            factory_calendar_not_found   = 6
            OTHERS                       = 7.
ENDFORM.                    " READ_WORK_DATE

*&---------------------------------------------------------------------*
*&      Form  GET_DAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
*      -->P_IT_MASTER_DAY  text
*----------------------------------------------------------------------*
FORM get_day USING    pa_wdate  pa_day.
  DATA: l_day         LIKE scal-indicator .

  CALL FUNCTION 'DATE_COMPUTE_DAY'
       EXPORTING
            date = pa_wdate
       IMPORTING
            day  = l_day.

  pa_day = l_day.
ENDFORM.                    " GET_DAY

*&---------------------------------------------------------------------*
*&      Form  GET_UPH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
*      -->P_IT_MASTER_UPH  text
*----------------------------------------------------------------------*
FORM get_uph USING    pa_wdate  pa_uph  pa_shift .
  DATA lw_ld          LIKE zvpp_ld .
  data: lt_ld   like zvpp_ld occurs 0 with header line.

  IF pa_shift IS INITIAL .
* requested by MY HUR changed by chris
* because two shift could exist, read one record
* only one shift is calculated
*    SELECT SINGLE * INTO lw_ld
*      FROM zvpp_ld
*     WHERE ld_perst <= pa_wdate
*       AND ld_pered >= pa_wdate
*       AND arbpl     = 'T'      .
    SELECT * INTO table lt_ld
      FROM zvpp_ld
     WHERE ld_perst <= pa_wdate
       AND ld_pered >= pa_wdate
       AND arbpl     = 'T'     .

* end of change on 06/13/2005
  ELSE.
* requested by MY HUR changed by chris
* because two shift could exist, read one record
* only one shift is calculated
* and one shift could have more than one record
* to difine diferent rate for different period
* of time
*    SELECT SINGLE * INTO lw_ld
*      FROM zvpp_ld
*     WHERE ld_perst <= pa_wdate
*       AND ld_pered >= pa_wdate
*       AND ld_shift  = pa_shift
*       AND arbpl     = 'T'      .
    SELECT * INTO table lt_ld
      FROM zvpp_ld
     WHERE ld_perst <= pa_wdate
       AND ld_pered >= pa_wdate
       AND ld_shift  = pa_shift
       AND arbpl     = 'T'    .


  ENDIF.
* add by chris on 06/13/2005
    loop at lt_ld.
      lw_ld-lrate = lw_ld-lrate + lt_ld-lrate.
      lw_ld-lantu = lw_ld-lantu + lt_ld-lantu.
    endloop.
* end of add.

  IF lw_ld-lantu = 0.
    pa_uph = 0 .
  ELSE.
    pa_uph = lw_ld-lrate / lw_ld-lantu .
  ENDIF.
ENDFORM.                    " GET_UPH

*&---------------------------------------------------------------------*
*&      Form  GET_WORKTIME1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
*      -->P_IT_MASTER_WKTIME  text
*----------------------------------------------------------------------*
FORM get_worktime1 USING    pa_wdate  pa_wktime  pa_day.
  DATA: l_wtime       LIKE zvpp_capacity-endzt ,
        l_chk         TYPE p DECIMALS 3        ,
        l_tc37a       LIKE tc37a               ,
        l_date        TYPE d ,
        l_flag        TYPE c ,
        l_einzt       LIKE tc37a-einzt ,
        lt_capa       LIKE TABLE OF zvpp_capacity      WITH HEADER LINE.

  CLEAR: lt_capa, lt_capa[], l_wtime.
  SELECT * INTO TABLE lt_capa
    FROM zvpp_capacity
   WHERE arbpl = 'T'
     AND datub >= pa_wdate .

  SORT lt_capa BY datub tagnr schnr .
  READ TABLE lt_capa INDEX 1.
  l_date = lt_capa-datub    .

  LOOP AT lt_capa WHERE datub = l_date AND tagnr = pa_day .
    CLEAR: l_einzt.
    SELECT SINGLE * INTO l_tc37a       " l_einzt
      FROM tc37a
     WHERE schgrup  = lt_capa-mosid
       AND kaptprog = lt_capa-tprog
       AND endda   >= pa_wdate
       AND begda   <= pa_wdate     .
    it_shift-time  = l_tc37a-einzt .
    IF l_tc37a-begzt >= l_tc37a-endzt.
      it_shift-total = l_tc37a-endzt +   l_tc37a-begzt            .
    ELSE.
      it_shift-total = l_tc37a-endzt -   l_tc37a-begzt            .
    ENDIF.
    it_shift-ftime = l_tc37a-begzt .
    it_shift-shift = lt_capa-schnr .
    l_chk = it_shift-time / 3600 .
    it_shift-tqty  = ceil( it_shift-uph * l_chk )  .
    l_chk = ceil( it_shift-time / 7200 ) .
    it_shift-hqty  = ceil( it_shift-tqty / l_chk ) .
    IT_SHIFT-HLOOP = ceil( it_shift-time / 7200 )  .
    APPEND it_shift.
  ENDLOOP.
ENDFORM.                    " GET_WORKTIME1

*&---------------------------------------------------------------------*
*&      Form  GET_WORKTIME2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
*      -->P_IT_MASTER_TIME  text
*      -->P_IT_MASTER_DAY  text
*----------------------------------------------------------------------*
FORM GET_WORKTIME2 USING    pa_wdate  pa_wktime  pa_day.
  DATA: l_wtime       LIKE zvpp_capacity-endzt ,
        l_date        TYPE d ,
        l_einzt       LIKE tc37a-einzt ,
        lt_capa       LIKE TABLE OF zvpp_capacity      WITH HEADER LINE.

  CLEAR: lt_capa, lt_capa[], l_wtime.
  SELECT * INTO TABLE lt_capa
    FROM zvpp_capacity
   WHERE arbpl = 'T'
     AND datub >= pa_wdate .

  SORT lt_capa BY datub .
  READ TABLE lt_capa INDEX 1.
  l_date = lt_capa-datub    .

  LOOP AT lt_capa WHERE datub = l_date AND tagnr = pa_day .
    CLEAR: l_einzt.
    SELECT SINGLE einzt INTO l_einzt
      FROM tc37a
     WHERE schgrup  = lt_capa-mosid
       AND kaptprog = lt_capa-tprog
       AND endda   >= pa_wdate
       AND begda   <= pa_wdate     .
    l_wtime = l_wtime + l_einzt    .
  ENDLOOP.
  pa_wktime = l_wtime .
ENDFORM.                    " GET_WORKTIME2

*&---------------------------------------------------------------------*
*&      Form  SET_DATE_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_DATE_FIELD.
  DATA: l_chk        TYPE p DECIMALS 3,
        L_MAX        TYPE I,
        L_START      TYPE I,
        L_DATE       TYPE D,
        L_END        TYPE I.

  L_START =  1 .     L_DATE = WA_WDATE.
  DESCRIBE TABLE it_dvrt2 LINES L_MAX.
  SORT IT_MASTER BY SEQ.

  LOOP AT IT_MASTER.
    L_DATE = L_DATE + 1.
    PERFORM read_work_date USING '+'  wa_kalid  l_date   .
    IF it_master-uph = 0.  CONTINUE.  ENDIF.
    l_chk = it_master-time / 3600 .
    l_END = l_END + CEIL( it_master-uph * l_chk )  .
    IF L_END > L_MAX.      EXIT.      ENDIF.
    LOOP AT IT_DVRT2 FROM  L_START TO L_END.
      IT_DVRT2-RD18  =  L_DATE.
      MODIFY IT_DVRT2.
    ENDLOOP.
    L_START = L_END + 1.
  ENDLOOP.
ENDFORM.                    " SET_DATE_FIELD
