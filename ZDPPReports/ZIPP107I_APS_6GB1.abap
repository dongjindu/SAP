************************************************************************
* Program Name      : ZIPP107I_APS_6GB1
* Author            : Bobby
* Creation Date     : 2003.11.12
* Specifications By :
* Pattern           : 1.1
* Development Request No :  UD1K901977
* Addl Documentation:
* Description       : Create the contents of the Table ZTPP_PMT06GB
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT zipp107i_aps_6gb1    NO STANDARD PAGE HEADING MESSAGE-ID zmpp.
*                          LINE-SIZE 120
*----------------------------------------------------------------------*
*  EXTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
TABLES : ztpp_wosum2 ,
         ztpp_pmt06gb,
         ztpp_pmt07jb_a.

*----------------------------------------------------------------------*
*  INTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
DATA : wa_wosum             LIKE ztpp_wosum                            ,
       wa_wosum2            LIKE ztpp_wosum2                           ,
       it_dvrt2             LIKE TABLE OF ztpp_input_plan
                                                       WITH HEADER LINE,
       it_6gb               LIKE TABLE OF ztpp_pmt06gb WITH HEADER LINE.

DATA: BEGIN OF it_val       OCCURS 0,
       wdate                TYPE d,
       no(2)                TYPE n,
       arbpl(10)            TYPE c,             " Work Center Name
      END OF it_val.

DATA: BEGIN OF it_master  OCCURS 0,
        seq               TYPE i  ,             " Sequence
        arbpl(10)         TYPE c  ,             " Work Center Name
        date              TYPE d  ,             " Date
        day               LIKE kapa-tagnr,      " Day
        shift             LIKE kapa-schnr,      " Shift
        time              TYPE kapendzt  ,      " Times for working
        uph               TYPE zvpp_ld-lrate,   " UPH
        tqty              TYPE i  ,             " Day's Total Veh.
        kalid             LIKE kako-kalid ,     " Factory-Calendar
      END OF it_master.


************************************************************************
*              SELECTION SCREEN LAYOUT                                 *
************************************************************************
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME .
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_run          TYPE c AS CHECKBOX  DEFAULT 'X',
            p_result       TYPE c NO-DISPLAY  MEMORY ID pid,
            p_datum        TYPE d NO-DISPLAY  MEMORY ID pid02.
SELECTION-SCREEN COMMENT  (55) text-001 FOR FIELD p_run.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK b1.

*----------------------------------------------------------------------*
*  GLOBAL VARIABLE DECLARATION
*----------------------------------------------------------------------*
DATA: wa_datum              TYPE sy-datum,
      wa_fabkl              LIKE t001w-fabkl.
DATA: wa_count              TYPE i                                   ,
      wa_kalid              LIKE kako-kalid                          ,
      wa_wdate              LIKE ztpp_day_sum-wdate                  .


FIELD-SYMBOLS: <fs_field>   TYPE ANY.


************************************************************************
* START-OF-SELECTION
************************************************************************
START-OF-SELECTION.
  CHECK p_run = 'X'  .
  PERFORM get_master    .       " Get the Master Information(UPH/SHIFT)
  PERFORM get_data      .
  PERFORM excute_process.
  PERFORM save_6gb      .

************************************************************************
* END-OF-SELECTION
************************************************************************
END-OF-SELECTION.
*  PERFORM LIST_PROCESS.

*&---------------------------------------------------------------------*
*&      Form  EXCUTE_PROCESS
*&---------------------------------------------------------------------*
FORM excute_process.
  DATA: l_flag               TYPE c                ,
        l_status             TYPE ztpp_status-rp_point ,
        l_loops              TYPE i                ,
        lt_6gb               LIKE TABLE OF it_6gb      WITH HEADER LINE,
        l_dvrt               LIKE ztpp_input_plan  ,
        l_count TYPE sy-tabix,
        l_tabix TYPE sy-tabix.

  CLEAR: lt_6gb, lt_6gb[], l_dvrt.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_dvrt2
    FROM ztpp_input_plan
   WHERE status <   '07' .

**>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
* Start : Added By Tonkey on 03/12/2004.
* Change Request No: UD1K908040
**>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
* PLGU('A': Confirmed or 'B': Only Planned), Work Order No.
  SORT it_dvrt2 BY serial.
  PERFORM set_date_field  USING 'P'  .
  SORT IT_DVRT2 BY WORK_ORDER EXTC INTC .
  DESCRIBE TABLE it_dvrt2 LINES l_count .
  LOOP AT it_dvrt2 .
    l_tabix = sy-tabix.
**  If it is the first row...
    IF sy-tabix = 1.
      l_dvrt = it_dvrt2.
*     Set Each Day's Bukt QTY
      PERFORM calc_qty_n USING  'P'  it_dvrt2 .
      IF l_tabix <> l_count .
        CONTINUE.
**    If The row is the last ...
      ELSE.
        PERFORM search_wosum2 USING l_dvrt-work_order
                                    l_dvrt-extc
                                    l_dvrt-intc .
        PERFORM filling_6gb  USING 'T'  wa_wosum2-rp06aq  l_dvrt.
        APPEND it_6gb.
        CONTINUE.
      ENDIF.
    ENDIF.
**  If The Previous and The Present Row are Equal ...
    IF it_dvrt2-work_order = l_dvrt-work_order AND
       it_dvrt2-extc       = l_dvrt-extc       AND
       it_dvrt2-intc       = l_dvrt-intc       .
*       it_dvrt2-rd18       = l_dvrt-rd18       .
*     Sum Each Day's Bukt QTY
      PERFORM calc_qty_n USING  'P'  it_dvrt2 .
      IF l_tabix <> l_count .
        CONTINUE .
**    If The row is the last ...
      ELSE.
        PERFORM search_wosum2 USING l_dvrt-work_order
                                    l_dvrt-extc
                                    l_dvrt-intc .
        PERFORM filling_6gb  USING 'T'  wa_wosum2-rp06aq  l_dvrt.
        APPEND it_6gb.
      ENDIF.
**  If The Previous and The Present Row are not equal ...
    ELSE.
      PERFORM search_wosum2 USING l_dvrt-work_order
                                  l_dvrt-extc
                                  l_dvrt-intc .
      PERFORM filling_6gb  USING 'T'  wa_wosum2-rp06aq  l_dvrt.
      APPEND it_6gb .      CLEAR: it_6gb .
*
      l_dvrt = it_dvrt2.
*     Set Each Day's Bukt QTY
      PERFORM calc_qty_n USING 'P'  it_dvrt2 .
      IF l_tabix <> l_count .
        CONTINUE .
**    If The row is the last ...
      ELSE.
        PERFORM search_wosum2 USING l_dvrt-work_order
                                    l_dvrt-extc
                                    l_dvrt-intc .
        PERFORM filling_6gb  USING 'T'  wa_wosum2-rp06aq  l_dvrt.
        APPEND it_6gb.
      ENDIF.
    ENDIF.
  ENDLOOP.
**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
* End   : Added By Tonkey on 03/12/2004.
**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

  SORT it_6gb BY gubb ordr extc intc plgu .

**>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
* Start : Added By Tonkey on 03/11/2004.
* Change Request No: UD1K908040
**>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  LOOP AT it_6gb WHERE gubb = 'T'.  "Trim In
    lt_6gb = it_6gb.
    READ TABLE it_val WITH KEY wdate = wa_datum.
    IF sy-subrc = 0.
*     The Case of Confirmed Order
      IF lt_6gb-plgu = 'A'.
        PERFORM clear_field USING 'B' it_val-no lt_6gb.
        APPEND lt_6gb.
*     The Case of Only Planned Order
      ELSE.
        PERFORM clear_field USING 'A' it_val-no lt_6gb.
        APPEND lt_6gb.
      ENDIF.
      DELETE it_6gb.
    ENDIF.
  ENDLOOP.
  APPEND LINES OF lt_6gb TO it_6gb     .

  CLEAR: lt_6gb, lt_6gb[], l_dvrt, it_dvrt2, it_dvrt2[].

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_dvrt2
    FROM ztpp_input_plan
   WHERE status <   '01' .

**>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
* Start : Added By Tonkey on 03/12/2004.
* Change Request No: UD1K908040
**>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
* PLGU('A': Confirmed or 'B': Only Planned), Work Order No.
  SORT it_dvrt2 BY serial.
  PERFORM set_date_field  USING 'B'  .
  SORT IT_DVRT2 BY WORK_ORDER EXTC INTC .
  DESCRIBE TABLE it_dvrt2 LINES l_count .
  LOOP AT it_dvrt2 .
    l_tabix = sy-tabix.
**  If it is the first row...
    IF sy-tabix = 1.
      l_dvrt = it_dvrt2.
*     Set Each Day's Bukt QTY
      PERFORM calc_qty_n USING  'B'  it_dvrt2 .
      IF l_tabix <> l_count .
        CONTINUE.
**    If The row is the last ...
      ELSE.
        PERFORM search_wosum2 USING l_dvrt-work_order
                                    l_dvrt-extc
                                    l_dvrt-intc .
        PERFORM filling_6gb  USING 'B'  wa_wosum2-rp01aq  l_dvrt.
        APPEND it_6gb.
        CONTINUE.
      ENDIF.
    ENDIF.
**  If The Previous and The Present Row are Equal ...
    IF it_dvrt2-work_order = l_dvrt-work_order AND
       it_dvrt2-extc       = l_dvrt-extc       AND
       it_dvrt2-intc       = l_dvrt-intc       .
*       it_dvrt2-rd18       = l_dvrt-rd18       .
*     Sum Each Day's Bukt QTY
      PERFORM calc_qty_n USING 'B'  it_dvrt2 .
      IF l_tabix <> l_count .
        CONTINUE .
**    If The row is the last ...
      ELSE.
        PERFORM search_wosum2 USING l_dvrt-work_order
                                    l_dvrt-extc
                                    l_dvrt-intc .
        PERFORM filling_6gb  USING 'B'  wa_wosum2-rp01aq  l_dvrt.
        APPEND it_6gb.
      ENDIF.
**  If The Previous and The Present Row are not equal ...
    ELSE.
      PERFORM search_wosum2 USING l_dvrt-work_order
                                  l_dvrt-extc
                                  l_dvrt-intc .
      PERFORM filling_6gb  USING 'B'  wa_wosum2-rp01aq  l_dvrt.
      APPEND it_6gb .      CLEAR: it_6gb .
*
      l_dvrt = it_dvrt2.
*     Set Each Day's Bukt QTY
      PERFORM calc_qty_n USING 'B'  it_dvrt2 .
      IF l_tabix <> l_count .
        CONTINUE .
**    If The row is the last ...
      ELSE.
        PERFORM search_wosum2 USING l_dvrt-work_order
                                    l_dvrt-extc
                                    l_dvrt-intc .
        PERFORM filling_6gb  USING 'B'  wa_wosum2-rp01aq  l_dvrt.
        APPEND it_6gb.
      ENDIF.
    ENDIF.
  ENDLOOP.

  SORT it_6gb BY gubb ordr extc intc plgu.
**>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
* Start : Added By Tonkey on 03/11/2004.
* Change Request No: UD1K908040
**>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  LOOP AT it_6gb WHERE gubb = 'B'.  "Body In
    lt_6gb = it_6gb.
    READ TABLE it_val WITH KEY wdate = wa_datum.
    IF sy-subrc = 0.
*     The Case of Confirmed Order
      IF lt_6gb-plgu = 'A'.
        PERFORM clear_field USING 'B' it_val-no lt_6gb.
        APPEND lt_6gb.
*     The Case of Only Planned Order
      ELSE.
        PERFORM clear_field USING 'A' it_val-no lt_6gb.
        APPEND lt_6gb.
      ENDIF.
      DELETE it_6gb.
    ENDIF.
  ENDLOOP.
**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
* End   : Added By Tonkey on 03/11/2004.
**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  APPEND LINES OF lt_6gb TO it_6gb     .

  CLEAR: lt_6gb, lt_6gb[], l_dvrt, it_dvrt2, it_dvrt2[].

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_dvrt2
    FROM ztpp_input_plan
   WHERE status <   '18' .

**>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
* Start : Added By Tonkey on 03/12/2004.
* Change Request No: UD1K908040
**>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
* PLGU('A': Confirmed or 'B': Only Planned), Work Order No.
  SORT it_dvrt2 BY serial.
  PERFORM set_date_field  USING 'T'  .
  SORT IT_DVRT2 BY WORK_ORDER EXTC INTC .
  DESCRIBE TABLE it_dvrt2 LINES l_count .
  LOOP AT it_dvrt2 .
    l_tabix = sy-tabix.
**  If it is the first row...
    IF sy-tabix = 1.
      l_dvrt = it_dvrt2.
*     Set Each Day's Bukt QTY
      PERFORM calc_qty_n USING 'T'  it_dvrt2 .
      IF l_tabix <> l_count .
        CONTINUE.
**    If The row is the last ...
      ELSE.
        PERFORM search_wosum2 USING l_dvrt-work_order
                                    l_dvrt-extc
                                    l_dvrt-intc .
        PERFORM filling_6gb  USING 'S'  wa_wosum2-rp08aq  l_dvrt.
        APPEND it_6gb.
        CONTINUE.
      ENDIF.
    ENDIF.
**  If The Previous and The Present Row are Equal ...
    IF it_dvrt2-work_order = l_dvrt-work_order AND
       it_dvrt2-extc       = l_dvrt-extc       AND
       it_dvrt2-intc       = l_dvrt-intc       .
*       it_dvrt2-rd18       = l_dvrt-rd18       .
*     Sum Each Day's Bukt QTY
      PERFORM calc_qty_n USING 'T'  it_dvrt2 .
      IF l_tabix <> l_count .
        CONTINUE .
**    If The row is the last ...
      ELSE.
        PERFORM search_wosum2 USING l_dvrt-work_order
                                    l_dvrt-extc
                                    l_dvrt-intc .
        PERFORM filling_6gb  USING 'S'  wa_wosum2-rp08aq  l_dvrt.
        APPEND it_6gb.
      ENDIF.
**  If The Previous and The Present Row are not equal ...
    ELSE.
      PERFORM search_wosum2 USING l_dvrt-work_order
                                  l_dvrt-extc
                                  l_dvrt-intc .
      PERFORM filling_6gb  USING 'S'  wa_wosum2-rp08aq  l_dvrt.
      APPEND it_6gb .      CLEAR: it_6gb .
*
      l_dvrt = it_dvrt2.
*     Set Each Day's Bukt QTY
      PERFORM calc_qty_n USING 'T'  it_dvrt2 .
      IF l_tabix <> l_count .
        CONTINUE .
**    If The row is the last ...
      ELSE.
        PERFORM search_wosum2 USING l_dvrt-work_order
                                    l_dvrt-extc
                                    l_dvrt-intc .
        PERFORM filling_6gb  USING 'S'  wa_wosum2-rp08aq  l_dvrt.
        APPEND it_6gb.
      ENDIF.
    ENDIF.
  ENDLOOP.

  SORT it_6gb BY gubb ordr extc intc .
**>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
* Start : Added By Tonkey on 03/11/2004.
* Change Request No: UD1K908040
**>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  LOOP AT it_6gb WHERE gubb = 'S'.  "Sign OFF
    lt_6gb = it_6gb.
    READ TABLE it_val WITH KEY wdate = wa_datum.
    IF sy-subrc = 0.
*     The Case of Confirmed Order
      IF lt_6gb-plgu = 'A'.
        PERFORM clear_field USING 'B' it_val-no lt_6gb.
        APPEND lt_6gb.
*     The Case of Only Planned Order
      ELSE.
        PERFORM clear_field USING 'A' it_val-no lt_6gb.
        APPEND lt_6gb.
      ENDIF.
      DELETE it_6gb.
    ENDIF.
  ENDLOOP.
  APPEND LINES OF lt_6gb TO it_6gb     .
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
  DATA: l_date               TYPE d,
        l_wdate              TYPE d,
        l_no(2)              TYPE n,
        l_check              TYPE i.

  DELETE FROM ztpp_pmt06gb CLIENT SPECIFIED WHERE mandt = sy-mandt.

  l_date = wa_wdate .
  PERFORM read_shop_calid   USING 'T'  wa_kalid.
  DO 31 TIMES .
*   Start to set dates from today.
    l_date = l_date + 1  .    l_check = l_check + 1 .
*   PERFORM read_work_date USING '+'  wa_kalid  l_date   .
    it_val-wdate = l_date          .
    it_val-no    = l_check         .
    it_val-ARBPL = 'T'             .
    APPEND it_val.
  ENDDO.

  l_date = wa_wdate . CLEAR: L_CHECK.
  PERFORM read_shop_calid   USING 'B'  wa_kalid.
  DO 31 TIMES .
*   Start to set dates from today.
    l_date = l_date + 1  .    l_check = l_check + 1 .
*   PERFORM read_work_date USING '+'  wa_kalid  l_date   .
    it_val-wdate = l_date          .
    it_val-no    = l_check         .
    it_val-ARBPL = 'B'             .
    APPEND it_val.
  ENDDO.

  l_date = wa_wdate . CLEAR: L_CHECK.
  PERFORM read_shop_calid   USING 'P'  wa_kalid.
  DO 31 TIMES .
*   Start to set dates from today.
    l_date = l_date + 1  .    l_check = l_check + 1 .
*   PERFORM read_work_date USING '+'  wa_kalid  l_date   .
    it_val-wdate = l_date          .
    it_val-no    = l_check         .
    it_val-ARBPL = 'P'             .
    APPEND it_val.
  ENDDO.

  " Read the Sequence's Date for Confirmation(Fixed..)
  SELECT MAX( SQDT ) INTO WA_DATUM
    FROM ZTPP_PMT07JB_A
   WHERE GUBB = 'A'
     AND GUB1 = '1' .
ENDFORM.                    " GET_DATA

*&---------------------------------------------------------------------*
*&      Form  calc_qty
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calc_qty              USING  pa_rp  pa_data  LIKE ztpp_input_plan.
  FIELD-SYMBOLS: <l_gap>     .

  DATA: l_name(40)           TYPE c,
        l_gap                TYPE i,
        l_no(2)              TYPE n,
        l_vals               TYPE d,
        l_wdate              TYPE d,
        l_date               TYPE d.

  CONCATENATE 'PA_DATA-'  pa_rp   INTO  l_name .
  ASSIGN (l_name)                 TO    <l_gap>.
  l_vals = <l_gap>(8) .
  l_date = sy-datum - 1.    CLEAR: l_no.
  READ TABLE it_val WITH KEY wdate = l_vals.
  CHECK sy-subrc = 0.
  l_no = it_val-no  .

  CASE l_no.
    WHEN '01' .
      it_6gb-bukt01 = it_6gb-bukt01 + 1 .
    WHEN '02' .
      it_6gb-bukt02 = it_6gb-bukt02 + 1 .
    WHEN '03' .
      it_6gb-bukt03 = it_6gb-bukt03 + 1 .
    WHEN '04' .
      it_6gb-bukt04 = it_6gb-bukt04 + 1 .
    WHEN '05' .
      it_6gb-bukt05 = it_6gb-bukt05 + 1 .
    WHEN '06' .
      it_6gb-bukt06 = it_6gb-bukt06 + 1 .
    WHEN '07' .
      it_6gb-bukt07 = it_6gb-bukt07 + 1 .
    WHEN '08' .
      it_6gb-bukt08 = it_6gb-bukt08 + 1 .
    WHEN '09' .
      it_6gb-bukt09 = it_6gb-bukt09 + 1 .
    WHEN '10' .
      it_6gb-bukt10 = it_6gb-bukt10 + 1 .
    WHEN '11' .
      it_6gb-bukt11 = it_6gb-bukt11 + 1 .
    WHEN '12' .
      it_6gb-bukt12 = it_6gb-bukt12 + 1 .
    WHEN '13' .
      it_6gb-bukt13 = it_6gb-bukt13 + 1 .
    WHEN '14' .
      it_6gb-bukt14 = it_6gb-bukt14 + 1 .
    WHEN '15' .
      it_6gb-bukt15 = it_6gb-bukt15 + 1 .
    WHEN '16' .
      it_6gb-bukt16 = it_6gb-bukt16 + 1 .
    WHEN '17' .
      it_6gb-bukt17 = it_6gb-bukt17 + 1 .
    WHEN '18' .
      it_6gb-bukt18 = it_6gb-bukt18 + 1 .
    WHEN '19' .
      it_6gb-bukt19 = it_6gb-bukt19 + 1 .
    WHEN '20' .
      it_6gb-bukt20 = it_6gb-bukt20 + 1 .
    WHEN '21' .
      it_6gb-bukt21 = it_6gb-bukt21 + 1 .
    WHEN '22' .
      it_6gb-bukt22 = it_6gb-bukt22 + 1 .
    WHEN '23' .
      it_6gb-bukt23 = it_6gb-bukt23 + 1 .
    WHEN '24' .
      it_6gb-bukt24 = it_6gb-bukt24 + 1 .
    WHEN '25' .
      it_6gb-bukt25 = it_6gb-bukt25 + 1 .
    WHEN '26' .
      it_6gb-bukt26 = it_6gb-bukt26 + 1 .
    WHEN '27' .
      it_6gb-bukt27 = it_6gb-bukt27 + 1 .
    WHEN '28' .
      it_6gb-bukt28 = it_6gb-bukt28 + 1 .
    WHEN '29' .
      it_6gb-bukt29 = it_6gb-bukt29 + 1 .
    WHEN '30' .
      it_6gb-bukt30 = it_6gb-bukt30 + 1 .
    WHEN '31' .
      it_6gb-bukt31 = it_6gb-bukt31 + 1 .
  ENDCASE.
ENDFORM.                    " calc_qty

*&---------------------------------------------------------------------*
*&      Form  calc_qty_n
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calc_qty_n           USING  PA_WC
                                 pa_data  LIKE ztpp_input_plan.
  DATA: l_gap                TYPE i,
        l_no(2)              TYPE n,
        l_vals               TYPE d,
        l_wdate              TYPE d.

  l_vals = pa_data-rd18.
  CLEAR: l_no.
  READ TABLE it_val WITH KEY wdate = l_vals  ARBPL = PA_WC .
  CHECK sy-subrc = 0.
  l_no = it_val-no  .

  CASE l_no.
    WHEN '01' .
      it_6gb-bukt01 = it_6gb-bukt01 + 1 .
    WHEN '02' .
      it_6gb-bukt02 = it_6gb-bukt02 + 1 .
    WHEN '03' .
      it_6gb-bukt03 = it_6gb-bukt03 + 1 .
    WHEN '04' .
      it_6gb-bukt04 = it_6gb-bukt04 + 1 .
    WHEN '05' .
      it_6gb-bukt05 = it_6gb-bukt05 + 1 .
    WHEN '06' .
      it_6gb-bukt06 = it_6gb-bukt06 + 1 .
    WHEN '07' .
      it_6gb-bukt07 = it_6gb-bukt07 + 1 .
    WHEN '08' .
      it_6gb-bukt08 = it_6gb-bukt08 + 1 .
    WHEN '09' .
      it_6gb-bukt09 = it_6gb-bukt09 + 1 .
    WHEN '10' .
      it_6gb-bukt10 = it_6gb-bukt10 + 1 .
    WHEN '11' .
      it_6gb-bukt11 = it_6gb-bukt11 + 1 .
    WHEN '12' .
      it_6gb-bukt12 = it_6gb-bukt12 + 1 .
    WHEN '13' .
      it_6gb-bukt13 = it_6gb-bukt13 + 1 .
    WHEN '14' .
      it_6gb-bukt14 = it_6gb-bukt14 + 1 .
    WHEN '15' .
      it_6gb-bukt15 = it_6gb-bukt15 + 1 .
    WHEN '16' .
      it_6gb-bukt16 = it_6gb-bukt16 + 1 .
    WHEN '17' .
      it_6gb-bukt17 = it_6gb-bukt17 + 1 .
    WHEN '18' .
      it_6gb-bukt18 = it_6gb-bukt18 + 1 .
    WHEN '19' .
      it_6gb-bukt19 = it_6gb-bukt19 + 1 .
    WHEN '20' .
      it_6gb-bukt20 = it_6gb-bukt20 + 1 .
    WHEN '21' .
      it_6gb-bukt21 = it_6gb-bukt21 + 1 .
    WHEN '22' .
      it_6gb-bukt22 = it_6gb-bukt22 + 1 .
    WHEN '23' .
      it_6gb-bukt23 = it_6gb-bukt23 + 1 .
    WHEN '24' .
      it_6gb-bukt24 = it_6gb-bukt24 + 1 .
    WHEN '25' .
      it_6gb-bukt25 = it_6gb-bukt25 + 1 .
    WHEN '26' .
      it_6gb-bukt26 = it_6gb-bukt26 + 1 .
    WHEN '27' .
      it_6gb-bukt27 = it_6gb-bukt27 + 1 .
    WHEN '28' .
      it_6gb-bukt28 = it_6gb-bukt28 + 1 .
    WHEN '29' .
      it_6gb-bukt29 = it_6gb-bukt29 + 1 .
    WHEN '30' .
      it_6gb-bukt30 = it_6gb-bukt30 + 1 .
    WHEN '31' .
      it_6gb-bukt31 = it_6gb-bukt31 + 1 .
  ENDCASE.
ENDFORM.                    " calc_qty_n

*&---------------------------------------------------------------------*
*&      Form  save_6gb
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_6gb.
  DATA: l_text(60) TYPE c,
        l_int TYPE i.

  MODIFY ztpp_pmt06gb FROM TABLE it_6gb .
  IF sy-subrc = 0.
    DESCRIBE TABLE it_6gb LINES l_int.
    SET PARAMETER ID 'PID03' FIELD l_int.
    WRITE l_int TO l_text LEFT-JUSTIFIED.
    CONCATENATE 'Created Record Count :' l_text
      INTO l_text .
    MESSAGE s001 WITH l_text .
    MESSAGE s001 WITH text-101.
    p_result = 'X'.
    SET PARAMETER ID 'PID' FIELD p_result.
  ELSE.
    MESSAGE e001 WITH text-102.
    p_result = ' '.
  ENDIF.
ENDFORM.                                                    " save_6gb

*&---------------------------------------------------------------------*
*&      Form  FILLING_6GB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM filling_6gb  USING pa_val1  pa_val2  pa_data  LIKE ztpp_input_plan.
  it_6gb-guba  = 'A'   .
  it_6gb-gubb  = pa_val1.
  it_6gb-plnt  = '1'   .
  it_6gb-line  = '1'   .
  it_6gb-modl  = pa_data-modl.
  it_6gb-ordr  = pa_data-work_order(14).
  it_6gb-extc  = pa_data-extc    .
  it_6gb-intc  = pa_data-intc    .
  it_6gb-bmdl  = pa_data-mi      .
  it_6gb-ocnn  = pa_data-ocnn    .
  it_6gb-vers  = pa_data-vers    .
  it_6gb-preq =  pa_val2          .
  it_6gb-pbsq  = wa_wosum2-rp05cq  .
  it_6gb-rejq  = wa_wosum2-paintrej.
  it_6gb-wbsq  = wa_wosum2-wbsqty  .
  it_6gb-lcgu  = 'O'.                 " it_dvrt2-seq_code.
**>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
* Start : Added By Tonkey on 03/31/2004 .
**>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  PERFORM search_remq USING    it_6gb-ordr
                               it_6gb-extc
                               it_6gb-intc
                      CHANGING it_6gb-remq .
**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
* End   : Added By Tonkey on 03/31/2004 .
**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
*  it_6gb-remq  = wa_wosum2-remordqty.
  it_6gb-cdate = sy-datum         .
  it_6gb-cuser = sy-uname         .
**>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
* Start : Added By Tonkey on 03/11/2004.
* Change Request No: UD1K908040
**>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  CASE pa_data-rs18.
    WHEN 'A'.        "Confirmed
      it_6gb-plgu = 'A'.
    WHEN 'B'.        "Only Planned
      it_6gb-plgu = 'B'.
    WHEN 'E'.        "Error
      it_6gb-plgu = 'E'.
      IF sy-batch = ' '.
*        WRITE:/ 'GUB1''Value should be ''1'' or ''2''.'.
      ELSE.
        MESSAGE w001 WITH 'GUB1''Value should be ''1'' or ''2''.'.
      ENDIF.
  ENDCASE.

**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
* End   : Added By Tonkey on 03/11/2004.
**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
ENDFORM.                    " FILLING_6GB

*&---------------------------------------------------------------------*
*&      Form  clear_field
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_VAL_NO  text
*      -->P_LT_6GB  text
*----------------------------------------------------------------------*
FORM clear_field USING    pa_type pa_no  pa_6gb  LIKE  ztpp_pmt06gb .
  DATA: l_idx             TYPE i,
        l_no(2)           TYPE n,
        l_name(30)        TYPE c.

  CASE pa_type.
    WHEN 'A'.
      l_idx = pa_no.
      DO l_idx TIMES.
        l_no = l_no + 1.
        CONCATENATE 'PA_6GB-BUKT' l_no INTO l_name.
        ASSIGN (l_name)                TO   <fs_field>.
        <fs_field> = 0.
      ENDDO.
    WHEN 'B'.
*********************************
*   It's needed to be checked ...
      l_idx = 31 - pa_no.
*********************************
      l_no  = 31        .
      DO l_idx TIMES.
        l_no = l_no - 1.
        CONCATENATE 'PA_6GB-BUKT' l_no INTO l_name.
        ASSIGN (l_name)                TO   <fs_field>.
        <fs_field> = 0.
      ENDDO.
  ENDCASE.
ENDFORM.                    " clear_field

*&---------------------------------------------------------------------*
*&      Form  search_wosum2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_DVRT_WORK_ORDER  text
*      -->P_L_DVRT_EXTC  text
*      -->P_L_DVRT_INTC  text
*----------------------------------------------------------------------*
FORM search_wosum2 USING    p_dvrt_work_order
                            p_dvrt_extc
                            p_dvrt_intc.
  SELECT        * INTO CORRESPONDING FIELDS OF wa_wosum2
    FROM ztpp_wosum2 UP TO 1 ROWS
   WHERE wo_ser = p_dvrt_work_order(9)
     AND nation = p_dvrt_work_order+9(3)
     AND dealer = p_dvrt_work_order+12(2)
     AND extc   = p_dvrt_extc
     AND intc   = p_dvrt_intc
    ORDER BY cr_date DESCENDING .
  ENDSELECT.

ENDFORM.                    " search_wosum2
*&---------------------------------------------------------------------*
*&      Form  search_remq
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_6GB_ORDR  text
*      -->P_IT_6GB_EXTC  text
*      -->P_IT_6GB_INTC  text
*      <--P_IT_6GB_REMQ  text
*----------------------------------------------------------------------*
FORM search_remq USING    p_ordr
                          p_extc
                          p_intc
                 CHANGING p_remq.
  DATA: l_dist TYPE ztpp_pmt07jb_a-dist.
  SELECT *
    FROM ztpp_wosum2
    WHERE wo_ser = p_ordr(9)
      AND nation = p_ordr+9(3)
      AND dealer = p_ordr+12(2)
      AND extc   = p_extc
      AND intc   = p_intc
    ORDER BY cr_date DESCENDING .
    CONCATENATE ztpp_wosum2-nation ztpp_wosum2-dealer
      INTO l_dist .
    SELECT SINGLE *
      FROM ztpp_pmt07jb_a
      WHERE ordr = ztpp_wosum2-wo_ser AND
            dist = l_dist AND
            extc = ztpp_wosum2-extc AND
            intc = ztpp_wosum2-intc AND
            gubb <> '*'             AND
            gubb <> 'A'               .
    IF sy-subrc = 0.
      MOVE ztpp_wosum2-remordqty TO p_remq .
      EXIT.
    ELSE.
      CONTINUE.
    ENDIF.
  ENDSELECT.
ENDFORM.                    " search_remq

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
        l_date               TYPE d,
        l_count              TYPE i.

  " Master Data for the 1 Year's Information
  l_date = wa_wdate =  sy-datum - 1 .
  CLEAR: l_count.
  PERFORM read_shop_calid   USING 'T'  wa_kalid.
  DO 35 TIMES.
    l_count  = l_count + 1.      CLEAR: l_chk.
    l_date   =  l_date   + 1 .
    PERFORM read_work_date USING '+'  wa_kalid  l_date   .
    PERFORM get_day        USING l_date   it_master-day  .
    PERFORM get_worktime2  USING 'T'      l_date
                                 it_master-time it_master-day.
    PERFORM get_uph        USING 'T'      l_date
                                 it_master-uph it_master-shift.
    it_master-seq    = l_count.  it_master-date   = l_date   .
    l_chk = it_master-time / 3600 .
    it_master-tqty   = ceil( it_master-uph * l_chk )  .
    it_master-arbpl  = 'T'  .
    it_master-kalid  = wa_kalid.
    APPEND it_master.  CLEAR: it_master.
  ENDDO.

  " Master Data for the 1 Year's Information
  l_date = wa_wdate =  sy-datum - 1 .
  CLEAR: l_count.
  PERFORM read_shop_calid   USING 'B'  wa_kalid.
  DO 35 TIMES.
    l_count  = l_count + 1.      CLEAR: l_chk.
    l_date   =  l_date   + 1 .
    PERFORM read_work_date USING '+'  wa_kalid  l_date   .
    PERFORM get_day        USING l_date   it_master-day  .
    PERFORM get_worktime2  USING 'B'      l_date
                                 it_master-time it_master-day.
    PERFORM get_uph        USING 'B'      l_date
                                 it_master-uph it_master-shift.
    it_master-seq    = l_count.  it_master-date   = l_date   .
    l_chk = it_master-time / 3600 .
    it_master-tqty   = ceil( it_master-uph * l_chk )  .
    it_master-arbpl  = 'B'  .
    it_master-kalid  = wa_kalid.
    APPEND it_master.  CLEAR: it_master.
  ENDDO.

  " Master Data for the 1 Year's Information
  l_date = wa_wdate =  sy-datum - 1 .
  CLEAR: l_count.
  PERFORM read_shop_calid   USING 'P'  wa_kalid.
  DO 35 TIMES.
    l_count  = l_count + 1.      CLEAR: l_chk.
    l_date   =  l_date   + 1 .
    PERFORM read_work_date USING '+'  wa_kalid  l_date   .
    PERFORM get_day        USING l_date   it_master-day  .
    PERFORM get_worktime2  USING 'P'      l_date
                                 it_master-time it_master-day.
    PERFORM get_uph        USING 'P'      l_date
                                 it_master-uph it_master-shift.
    it_master-seq    = l_count.  it_master-date   = l_date   .
    l_chk = it_master-time / 3600 .
    it_master-tqty   = ceil( it_master-uph * l_chk )  .
    it_master-arbpl  = 'P'  .
    it_master-kalid  = wa_kalid.
    APPEND it_master.  CLEAR: it_master.
  ENDDO.
ENDFORM.                    " GET_MASTER

*&---------------------------------------------------------------------*
*&      Form  READ_SHOP_CALID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_KALID  text
*----------------------------------------------------------------------*
FORM read_shop_calid USING  pa_arbpl  pa_kalid.
  SELECT SINGLE kalid INTO pa_kalid
    FROM zvpp_capacity
   WHERE arbpl = pa_arbpl  .
ENDFORM.                    " READ_SHOP_CALID

*&---------------------------------------------------------------------*
*&      Form  READ_WORK_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
*----------------------------------------------------------------------*
FORM read_work_date    USING  pa_type  pa_kalid  pa_wdate.
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
FORM get_uph USING    PA_WC  pa_wdate  pa_uph  pa_shift .
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
       AND arbpl     = PA_WC     .

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
       AND arbpl     = PA_WC    .


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
*&      Form  GET_WORKTIME2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
*      -->P_IT_MASTER_TIME  text
*      -->P_IT_MASTER_DAY  text
*----------------------------------------------------------------------*
FORM get_worktime2 USING  pa_arbpl  pa_wdate  pa_wktime  pa_day.
  DATA: l_wtime       LIKE zvpp_capacity-endzt ,
        l_date        TYPE d ,
        l_einzt       LIKE tc37a-einzt ,
        lt_capa       LIKE TABLE OF zvpp_capacity      WITH HEADER LINE.

  CLEAR: lt_capa, lt_capa[], l_wtime.
  SELECT * INTO TABLE lt_capa
    FROM zvpp_capacity
   WHERE arbpl = pa_arbpl
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
FORM set_date_field   USING pa_arbpl.
  DATA: l_chk        TYPE p DECIMALS 3,
        l_max        TYPE i,
        l_start      TYPE i,
        l_date       TYPE d,
        l_end        TYPE i.

  l_start =  1 .     l_date = wa_wdate.
  DESCRIBE TABLE it_dvrt2 LINES l_max.
  SORT it_master BY seq.

  LOOP AT it_master WHERE arbpl = pa_arbpl.
    l_date = l_date + 1.
    PERFORM read_work_date USING '+'  it_master-kalid  l_date   .
    IF it_master-uph  = 0.  CONTINUE.  ENDIF.
    IF it_master-TIME = 0.  CONTINUE.  ENDIF.
    l_chk = it_master-time / 3600 .
    l_end = l_end + CEIL( it_master-uph * l_chk )  .
    IF l_end > l_max.      EXIT.      ENDIF.
    LOOP AT it_dvrt2 FROM  l_start TO l_end.
      it_dvrt2-rd18  =  l_date.
      MODIFY it_dvrt2.
    ENDLOOP.
    l_start = l_end + 1.
  ENDLOOP.

  loop at it_dvrt2 where rd18 is initial.
    it_dvrt2-rd18  =  '99991231'.
    MODIFY it_dvrt2.
  endloop.
ENDFORM.                    " SET_DATE_FIELD
