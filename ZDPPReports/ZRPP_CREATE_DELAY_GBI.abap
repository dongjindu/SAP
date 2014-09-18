************************************************************************
* Program Name      : ZRPP_CREATE_DELAY_GBI
* Creation Date     : 06/2013
* Development Request No :
* Addl Documentation:
* Description       :
* Modification Logs
* Date            Developer        RequestNo      Description
* 06/19/2014      BS Bae           UD1K960652     Definition change on
*                      Delay Car of Trim (RP7~RP17 --> RP7~RP19 by GBI)
************************************************************************

REPORT  zrpp_create_delay_gbi  MESSAGE-ID zmpp.
*-----// Internal tables
DATA: BEGIN OF it_wip OCCURS 0,
        objek      LIKE ausp-objek,
        rpid       LIKE ausp-atwrt,
        usage_car  LIKE ausp-atwrt,
        rp01_adate LIKE ausp-atwrt,
        rp02_adate LIKE ausp-atwrt,
        rp05_adate LIKE ausp-atwrt,
        rp07_adate LIKE ausp-atwrt,
        rp19_adate LIKE ausp-atwrt,
      END   OF it_wip.

DATA: BEGIN OF it_working_day OCCURS 0,
        date    LIKE sy-datum,
        dayfree LIKE hrvsched-noday,
      END   OF it_working_day.

DATA: it_delay LIKE ztpp_delay_gbi OCCURS 0 WITH HEADER LINE.

*-----// Global variable
DATA: v_rp_status LIKE cabn-atinn,
      v_usage_car LIKE cabn-atinn,
      v_rp01      LIKE cabn-atinn,
      v_rp02      LIKE cabn-atinn,
      v_rp05      LIKE cabn-atinn,
      v_rp07      LIKE cabn-atinn,
      v_rp19      LIKE cabn-atinn,
      v_worder    LIKE cabn-atinn,
      v_yesterday LIKE sy-datum,
      v_adate     TYPE string.

CONSTANTS: c_werks LIKE t001w-werks VALUE 'P001',
           c_body                   VALUE 'B',
           c_paint                  VALUE 'P',
           c_pbs                    VALUE 'S',
           c_trim                   VALUE 'T'.

************************************************************************
*              SELECTION SCREEN LAYOUT                                 *
************************************************************************
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-t01.
PARAMETERS: p_check AS CHECKBOX.
SELECTION-SCREEN: END OF BLOCK b1.

INITIALIZATION.
  PERFORM initialization.

AT SELECTION-SCREEN.
  CHECK sy-ucomm EQ 'ONLI'.
  IF p_check IS INITIAL.
    LEAVE PROGRAM.
  ENDIF.

  PERFORM read_data.

START-OF-SELECTION.
  PERFORM make_delay_info.
  PERFORM update_table.

*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data .
  PERFORM read_master.

  PERFORM get_current_wip.
*  PERFORM get_rp17.

  DELETE it_wip WHERE usage_car <> 'P'.

  IF it_wip[] IS INITIAL.
    MESSAGE e000 WITH text-m05.
  ENDIF.

  PERFORM get_working_day.
ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  READ_MASTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_master .

  SELECT SINGLE atinn INTO v_rp01
    FROM cabn WHERE atnam = 'P_RP01_ACTUAL_DATE'.

  SELECT SINGLE atinn INTO v_rp02
    FROM cabn WHERE atnam = 'P_RP02_ACTUAL_DATE'.

  SELECT SINGLE atinn INTO v_rp05
    FROM cabn WHERE atnam = 'P_RP05_ACTUAL_DATE'.

  SELECT SINGLE atinn INTO v_rp07
    FROM cabn WHERE atnam = 'P_RP07_ACTUAL_DATE'.

  SELECT SINGLE atinn INTO v_rp19
    FROM cabn WHERE atnam = 'P_RP19_ACTUAL_DATE'.

  SELECT SINGLE atinn INTO v_rp_status
    FROM cabn WHERE atnam = 'P_RP_STATUS'.

  SELECT SINGLE atinn INTO v_usage_car
    FROM cabn WHERE atnam = 'P_USAGE_CAR'.

ENDFORM.                    " READ_MASTER
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialization .
  v_yesterday = sy-datum - 1.
ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  GET_CURRENT_WIP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_current_wip .
  SELECT a~objek
         a~atwrt AS rpid
         b~atwrt AS usage_car
         c~atwrt AS rp01_adate
         d~atwrt AS rp02_adate
         e~atwrt AS rp05_adate
         f~atwrt AS rp07_adate
    INTO CORRESPONDING FIELDS OF TABLE it_wip
    FROM ausp AS a LEFT OUTER JOIN ausp AS b
                     ON b~objek = a~objek
                    AND b~atinn = v_usage_car
                    AND b~mafid = a~mafid
                    AND b~klart = a~klart
                    AND b~atwrt = 'P'
                   LEFT OUTER JOIN ausp AS c
                     ON c~objek = a~objek
                    AND c~atinn = v_rp01
                    AND c~mafid = a~mafid
                    AND c~klart = a~klart
                   LEFT OUTER JOIN ausp AS d
                     ON d~objek = a~objek
                    AND d~atinn = v_rp02
                    AND d~mafid = a~mafid
                    AND d~klart = a~klart
                   LEFT OUTER JOIN ausp AS e
                     ON e~objek = a~objek
                    AND e~atinn = v_rp05
                    AND e~mafid = a~mafid
                    AND e~klart = a~klart
                   LEFT OUTER JOIN ausp AS f
                     ON f~objek = a~objek
                    AND f~atinn = v_rp07
                    AND f~mafid = a~mafid
                    AND f~klart = a~klart
   WHERE a~klart EQ '002'
     AND a~atinn EQ v_rp_status
     AND a~atwrt IN ('01','02','03','04','05',
                     '06','07','08','09','10',
                     '11','12','13','14','15',
                     '16','17','18').
ENDFORM.                    " GET_CURRENT_WIP
*&---------------------------------------------------------------------*
*&      Form  GET_RP17
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_rp17 .
  CONCATENATE v_yesterday '%' INTO v_adate.

  SELECT a~objek
         h~atwrt AS rpid
         b~atwrt AS usage_car
         c~atwrt AS rp01_adate
         d~atwrt AS rp02_adate
         e~atwrt AS rp05_adate
         f~atwrt AS rp07_adate
         g~atwrt AS rp19_adate
    APPENDING CORRESPONDING FIELDS OF TABLE it_wip
    FROM ausp AS a LEFT OUTER JOIN ausp AS b
                     ON b~objek = a~objek
                    AND b~atinn = v_usage_car
                    AND b~mafid = a~mafid
                    AND b~klart = a~klart
                    AND b~atwrt = 'P'
                   LEFT OUTER JOIN ausp AS c
                     ON c~objek = a~objek
                    AND c~atinn = v_rp01
                    AND c~mafid = a~mafid
                    AND c~klart = a~klart
                   LEFT OUTER JOIN ausp AS d
                     ON d~objek = a~objek
                    AND d~atinn = v_rp02
                    AND d~mafid = a~mafid
                    AND d~klart = a~klart
                   LEFT OUTER JOIN ausp AS e
                     ON e~objek = a~objek
                    AND e~atinn = v_rp05
                    AND e~mafid = a~mafid
                    AND e~klart = a~klart
                   LEFT OUTER JOIN ausp AS f
                     ON f~objek = a~objek
                    AND f~atinn = v_rp07
                    AND f~mafid = a~mafid
                    AND f~klart = a~klart
                   LEFT OUTER JOIN ausp AS g
                     ON g~objek = a~objek
                    AND g~atinn = v_rp19
                    AND g~mafid = a~mafid
                    AND g~klart = a~klart
                   LEFT OUTER JOIN ausp AS h
                     ON h~objek = a~objek
                    AND h~atinn = v_rp_status
                    AND h~mafid = a~mafid
                    AND h~klart = a~klart
   WHERE a~klart EQ   '002'
     AND a~atinn EQ   v_rp19
     AND a~atwrt LIKE v_adate.
ENDFORM.                    " GET_RP17
*&---------------------------------------------------------------------*
*&      Form  MAKE_DELAY_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_delay_info .
  DATA: lv_deldat LIKE ztpp_delay_gbi-zdeldat.

  LOOP AT it_wip.
    IF it_wip-rp02_adate IS INITIAL.
      PERFORM get_day_gap         USING it_wip-rp01_adate(8)
                                        v_yesterday lv_deldat.
*      PERFORM correct_delay_count USING c_body lv_deldat.
      PERFORM append_delay_veh USING c_body lv_deldat.
      CONTINUE.
    ELSEIF it_wip-rp02_adate(8) EQ v_yesterday.
      PERFORM get_day_gap         USING it_wip-rp01_adate(8)
                                        it_wip-rp02_adate(8) lv_deldat.
*      PERFORM correct_delay_count USING c_body lv_deldat.
      PERFORM append_delay_veh USING c_body lv_deldat.
    ENDIF.

    IF it_wip-rp05_adate IS INITIAL.
      PERFORM get_day_gap         USING it_wip-rp02_adate(8)
                                        v_yesterday lv_deldat.
*      PERFORM correct_delay_count USING c_paint lv_deldat.
      PERFORM append_delay_veh USING c_paint lv_deldat.
      CONTINUE.
    ELSEIF it_wip-rp05_adate(8) EQ v_yesterday.
      PERFORM get_day_gap         USING it_wip-rp02_adate(8)
                                        it_wip-rp05_adate(8) lv_deldat.
*      PERFORM correct_delay_count USING c_paint lv_deldat.
      PERFORM append_delay_veh USING c_paint lv_deldat.
    ENDIF.

    IF it_wip-rp07_adate IS INITIAL.
      PERFORM get_day_gap         USING it_wip-rp05_adate(8)
                                        v_yesterday lv_deldat.
*      PERFORM correct_delay_count USING c_pbs lv_deldat.
      PERFORM append_delay_veh USING c_pbs lv_deldat.
      CONTINUE.
    ELSEIF it_wip-rp07_adate(8) EQ v_yesterday.
      PERFORM get_day_gap         USING it_wip-rp05_adate(8)
                                        it_wip-rp07_adate(8) lv_deldat.
*      PERFORM correct_delay_count USING c_pbs lv_deldat.
      PERFORM append_delay_veh USING c_pbs lv_deldat.
    ENDIF.

    IF it_wip-rp19_adate IS INITIAL.
      PERFORM get_day_gap         USING it_wip-rp07_adate(8)
                                        v_yesterday lv_deldat.
*      PERFORM correct_delay_count USING c_trim lv_deldat.
      PERFORM append_delay_veh USING c_trim lv_deldat.
      CONTINUE.
    ELSEIF it_wip-rp19_adate(8) EQ v_yesterday.
      PERFORM get_day_gap         USING it_wip-rp07_adate(8)
                                        it_wip-rp19_adate(8) lv_deldat.
*      PERFORM correct_delay_count USING c_trim lv_deldat.
      PERFORM append_delay_veh USING c_trim lv_deldat.
    ENDIF.

  ENDLOOP.
ENDFORM.                    " MAKE_DELAY_INFO
*&---------------------------------------------------------------------*
*&      Form  GET_DAY_GAP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_WIP_RP01_ADATE  text
*      -->P_V_YESTERDAY  text
*----------------------------------------------------------------------*
FORM get_day_gap USING pv_shop_in pv_shop_out pv_gap.
  DATA: lv_shop_in   LIKE sy-datum,
        lv_last_day  LIKE sy-datum,
        lv_gap_total TYPE i.

  CLEAR: pv_gap.

  CHECK pv_shop_in < pv_shop_out.

  lv_shop_in  = pv_shop_in.
  lv_last_day = pv_shop_out.

  lv_gap_total = lv_last_day - lv_shop_in.
  pv_gap       = lv_last_day - lv_shop_in.

  WHILE lv_shop_in NE lv_last_day.
    READ TABLE it_working_day WITH KEY date = lv_last_day
                              BINARY SEARCH.
    IF sy-subrc NE 0.
      MESSAGE e000 WITH text-m01.
    ENDIF.

    IF it_working_day-dayfree = 'X'.
      pv_gap = pv_gap - 1.
    ENDIF.

    lv_last_day = lv_last_day - 1.
  ENDWHILE.
ENDFORM.                    " GET_DAY_GAP
*&---------------------------------------------------------------------*
*&      Form  GET_WORKING_DAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_working_day .
  DATA: lv_first_day LIKE sy-datum,
        lv_date      LIKE sy-datum,
        lv_dayfree   LIKE hrvsched-noday,
        lv_gap       TYPE i.

  LOOP AT it_wip.
    IF   it_wip-rp01_adate(8) IS NOT INITIAL AND
       ( lv_first_day > it_wip-rp01_adate(8) OR
         lv_first_day IS INITIAL ).
      lv_first_day = it_wip-rp01_adate(8).
    ENDIF.

    IF   it_wip-rp02_adate(8) IS NOT INITIAL AND
       ( lv_first_day > it_wip-rp02_adate(8) OR
         lv_first_day IS INITIAL ).
      lv_first_day = it_wip-rp02_adate(8).
    ENDIF.

    IF   it_wip-rp05_adate(8) IS NOT INITIAL AND
       ( lv_first_day > it_wip-rp05_adate(8) OR
         lv_first_day IS INITIAL ).
      lv_first_day = it_wip-rp05_adate(8).
    ENDIF.

    IF   it_wip-rp07_adate(8) IS NOT INITIAL AND
       ( lv_first_day > it_wip-rp07_adate(8) OR
         lv_first_day IS INITIAL ).
      lv_first_day = it_wip-rp07_adate(8).
    ENDIF.
  ENDLOOP.

  lv_gap  = v_yesterday - lv_first_day + 1.
  lv_date = lv_first_day.

  DO lv_gap TIMES.
    CALL FUNCTION 'RH_GET_DATE_DAYNAME'
      EXPORTING
        langu               = 'E'
        date                = lv_date
        calid               = 'HM'
      IMPORTING
        dayfree             = lv_dayfree
      EXCEPTIONS
        no_langu            = 1
        no_date             = 2
        no_daytxt_for_langu = 3
        invalid_date        = 4
        OTHERS              = 5.
    IF sy-subrc <> 0.
      MESSAGE e000 WITH text-m02.
    ENDIF.

    CLEAR: it_working_day.

    MOVE: lv_date TO it_working_day-date,
          lv_dayfree TO it_working_day-dayfree.

    APPEND it_working_day.

    lv_date = lv_date + 1.
  ENDDO.

  SORT it_working_day BY date.
ENDFORM.                    " GET_WORKING_DAY
*&---------------------------------------------------------------------*
*&      Form  CORRECT_DELAY_COUNT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_C_BODY  text
*      -->P_LV_DELDAT  text
*----------------------------------------------------------------------*
FORM correct_delay_count USING pv_shop pv_deldat.
  CLEAR: it_delay.

  READ TABLE it_delay WITH KEY bwerk      = c_werks
                               pdate      = v_yesterday
                               model_code = it_wip-objek(3)
                               usg_car    = it_wip-usage_car
                               zshop      = pv_shop
                               zdeldat    = pv_deldat.
  IF sy-subrc EQ 0.
    it_delay-zdelcnt = it_delay-zdelcnt + 1.

    MODIFY it_delay INDEX sy-tabix.
  ELSE.
    it_delay-bwerk      = c_werks.
    it_delay-pdate      = v_yesterday.
    it_delay-model_code = it_wip-objek(3).
    it_delay-usg_car    = it_wip-usage_car.
    it_delay-zshop      = pv_shop.
    it_delay-zdeldat    = pv_deldat.
    it_delay-zdelcnt    = 1.
    it_delay-ernam      = sy-uname.
    it_delay-erdat      = sy-datum.
    it_delay-erzet      = sy-uzeit.
    it_delay-aenam      = sy-uname.
    it_delay-aedat      = sy-datum.
    it_delay-aezet      = sy-uzeit.

    APPEND it_delay.
  ENDIF.
ENDFORM.                    " CORRECT_DELAY_COUNT
*&---------------------------------------------------------------------*
*&      Form  UPDATE_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_table .
  DATA: lt_delay_old LIKE ztpp_delay_gbi OCCURS 0 WITH HEADER LINE.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_delay_old
    FROM ztpp_delay_gbi
   WHERE bwerk = c_werks
     AND pdate = v_yesterday.

  SORT it_delay BY bwerk pdate model_code usg_car zshop zdeldat.
  LOOP AT lt_delay_old.
    READ TABLE it_delay WITH KEY bwerk      = lt_delay_old-bwerk
                                 pdate      = lt_delay_old-pdate
                                 model_code = lt_delay_old-model_code
                                 usg_car    = lt_delay_old-usg_car
                                 zshop      = lt_delay_old-zshop
                                 zdeldat    = lt_delay_old-zdeldat
                        BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_delay-ernam = lt_delay_old-ernam.
      it_delay-erdat = lt_delay_old-erdat.
      it_delay-erzet = lt_delay_old-erzet.

      MODIFY it_delay INDEX sy-tabix.
    ENDIF.
  ENDLOOP.

  DELETE FROM ztpp_delay_gbi
   WHERE bwerk = c_werks
     AND pdate = v_yesterday.

  INSERT ztpp_delay_gbi FROM TABLE it_delay ACCEPTING DUPLICATE KEYS.
  IF sy-subrc NE 0.
    ROLLBACK WORK.
    MESSAGE e000 WITH text-m03.
  ELSE.
    MESSAGE s000 WITH text-m04.
  ENDIF.
ENDFORM.                    " UPDATE_TABLE
*&---------------------------------------------------------------------*
*&      Form  APPEND_DELAY_VEH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_C_BODY  text
*      -->P_LV_DELDAT  text
*----------------------------------------------------------------------*
FORM append_delay_veh USING pv_shop pv_deldat.
  CLEAR: it_delay.

  it_delay-bwerk      = c_werks.
  it_delay-pdate      = v_yesterday.
  it_delay-model_code = it_wip-objek(3).
  it_delay-body_no    = it_wip-objek+3(6).
  it_delay-usg_car    = it_wip-usage_car.
  it_delay-zshop      = pv_shop.
  it_delay-zdeldat    = pv_deldat.
  it_delay-zdelcnt    = 1.
  it_delay-ernam      = sy-uname.
  it_delay-erdat      = sy-datum.
  it_delay-erzet      = sy-uzeit.
  it_delay-aenam      = sy-uname.
  it_delay-aedat      = sy-datum.
  it_delay-aezet      = sy-uzeit.

  APPEND it_delay.
ENDFORM.                    " APPEND_DELAY_VEH
