*----------------------------------------------------------------------*
*   INCLUDE MZPP_PRO_DAILY_SUMMARYF01                                  *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  get_data_cont1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_cont1.
  IF  p_rp = space.
    MESSAGE i001 WITH 'Please, Select the RP Point'.
    REFRESH:it_act,it_wip.
    EXIT.
  ELSE.
    REFRESH :it_act,it_wip,itftext.
    CLEAR : it_wip,it_act.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_act
       FROM ztpp_pro_sum
        WHERE p_date EQ p_date
          AND werks EQ p_plant.

    DESCRIBE TABLE it_act LINES w_int.
    IF w_int <> 0.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE it_wip
         FROM ztpp_pro_sum
          WHERE p_date EQ p_date
            AND werks  EQ p_plant.
**data conversion
      PERFORM get_date_ranges USING p_date
                              CHANGING y_date.
*Plan & Actual
      PERFORM get_data_actual USING y_date.
*WIP
      PERFORM get_data_wip.
*read text
      PERFORM read_long_text.

      SELECT SINGLE erdat erzet INTO (n_date,n_time)
           FROM ztpp_pro_sum
          WHERE p_date EQ p_date
            AND werks  EQ p_plant.

    ELSE.
      MESSAGE i001 WITH 'No data on the selection date'.
      EXIT.
    ENDIF.
  ENDIF.
ENDFORM.                    " get_data_cont1
*&---------------------------------------------------------------------*
*&      Form  get_data_actual
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_actual USING y_date.
  LOOP AT it_act .
*Yesterday plan & Actual
    PERFORM yesterday_actual USING p_rp it_act-model y_date.
*Yesterday diff
    it_act-ydiff = it_act-yact - it_act-yplan.
*Summary Actual
    PERFORM summary_month USING p_rp it_act-model.
*Summary month diff
    it_act-sdiff = it_act-sact - it_act-splan.
*Remain qty = operation plan - month actual
    it_act-reqty = it_act-mope - it_act-sact.

    MODIFY it_act FROM it_act .
  ENDLOOP.
*Total
  PERFORM total_sum USING it_act-model.

ENDFORM.                    " get_data_actual
*&---------------------------------------------------------------------*
*&      Form  YESTERDAY_ACTUAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM yesterday_actual USING p_rp p_model y_date.
  CLEAR: z_dtime,z_dqty.
*yesterday actual
  IF p_rp = '01'.
    SELECT SINGLE  rp01
      INTO (it_act-yact)  FROM ztpp_pro_sum
          WHERE werks EQ p_plant
            AND model EQ p_model
            AND p_date EQ y_date.
  ELSEIF p_rp = '02'.
    SELECT SINGLE  rp02
      INTO (it_act-yact)  FROM ztpp_pro_sum
          WHERE werks EQ p_plant
            AND model EQ p_model
            AND p_date EQ y_date.
  ELSEIF p_rp = '04'.
    SELECT SINGLE  rp04
      INTO (it_act-yact)  FROM ztpp_pro_sum
          WHERE werks EQ p_plant
            AND model EQ p_model
            AND p_date EQ y_date.
  ELSEIF p_rp = '06'.
    SELECT SINGLE  rp06
      INTO (it_act-yact)  FROM ztpp_pro_sum
          WHERE werks EQ p_plant
            AND model EQ p_model
            AND p_date EQ y_date.
  ELSEIF p_rp = '07'.
    SELECT SINGLE rp07
      INTO (it_act-yact)  FROM ztpp_pro_sum
          WHERE werks EQ p_plant
            AND model EQ p_model
            AND p_date EQ y_date.
  ELSEIF p_rp = '18'.
    SELECT SINGLE rp18
      INTO (it_act-yact)  FROM ztpp_pro_sum
          WHERE werks EQ p_plant
            AND model EQ p_model
            AND p_date EQ y_date.
  ELSE.
    CLEAR it_act-yact.
  ENDIF.

ENDFORM.                    " YESTERDAY_ACTUAL
*&---------------------------------------------------------------------*
*&      Form  summary_month
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ACT_MODEL  text
*----------------------------------------------------------------------*
FORM summary_month USING  p_rp  p_model.
*Summary Actual
  IF p_rp = '01'.
    SELECT SUM( rp01 ) INTO it_act-sact
       FROM ztpp_pro_sum
          WHERE werks EQ p_plant
            AND model EQ p_model
            AND p_date IN s_date.
  ELSEIF p_rp = '02'.
    SELECT SUM( rp02 ) INTO it_act-sact
        FROM ztpp_pro_sum
          WHERE werks EQ p_plant
            AND model EQ p_model
            AND p_date IN s_date.
  ELSEIF p_rp = '04'.
    SELECT SUM( rp04 ) INTO it_act-sact
        FROM ztpp_pro_sum
          WHERE werks EQ p_plant
            AND model EQ p_model
            AND p_date IN s_date.
  ELSEIF p_rp = '06'.
    SELECT SUM( rp06 ) INTO it_act-sact
        FROM ztpp_pro_sum
          WHERE werks EQ p_plant
            AND model EQ p_model
            AND p_date IN s_date.
  ELSEIF p_rp = '07'.
    SELECT SUM( rp07 ) INTO it_act-sact
        FROM ztpp_pro_sum
          WHERE werks EQ p_plant
            AND model EQ p_model
            AND p_date IN s_date.
  ELSEIF p_rp = '18'.
    SELECT SUM( rp18 ) INTO it_act-sact
        FROM ztpp_pro_sum
          WHERE werks EQ p_plant
            AND model EQ p_model
            AND p_date IN s_date.
  ELSE.
    CLEAR it_act-sact.
  ENDIF.


ENDFORM.                    " summary_month
*&---------------------------------------------------------------------*
*&      Form  get_date_ranges
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_date_ranges USING p_date
                     CHANGING y_date.
**Yesterday
*  PERFORM conversion_date_yerterday USING p_date
*                          CHANGING y_date.
  y_date = p_date.
*Summary Month
  REFRESH s_date.CLEAR s_date.
  s_date-sign = 'I'.
  s_date-option = 'BT'.
  s_date-low(6) = p_date(6).
  s_date-low+6(2) = '01'.
  s_date-high = p_date.
  APPEND s_date.


ENDFORM.                    " get_date_ranges
*&---------------------------------------------------------------------*
*&      Form  total_sum
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ACT_MODEL  text
*----------------------------------------------------------------------*
FORM total_sum USING    p_model.
  DATA : BEGIN OF it_asum OCCURS 0,
          mann  LIKE ztpp_pro_sum-rp01, "Month-plan annual
          mope  LIKE ztpp_pro_sum-rp01, "Month-plan oper
          yplan LIKE ztpp_pro_sum-rp01, "Yesterday plan
          yact  LIKE ztpp_pro_sum-rp01, "Yesterday-actual
          ydiff LIKE ztpp_pro_sum-rp01, "Yesterday-diff
          splan LIKE ztpp_pro_sum-rp01, "Summary Month
          sact  LIKE ztpp_pro_sum-rp01, "Summary actual
          sdiff LIKE ztpp_pro_sum-rp01, "Summary difference
          reqty LIKE ztpp_pro_sum-rp01, "Remain Qty
          tplan LIKE ztpp_pro_sum-rp01, "Today plan
          twip  LIKE ztpp_pro_sum-rp01, "T/R WIP
       END OF it_asum.

  REFRESH it_asum.CLEAR : it_asum.
  LOOP AT it_act WHERE model <> 'TOTAL'.
    MOVE-CORRESPONDING it_act TO it_asum.
    COLLECT it_asum.
  ENDLOOP.

  READ TABLE it_asum INDEX 1.
  MOVE-CORRESPONDING it_asum TO it_act.
  MOVE 'TOTAL' TO it_act-model.
  APPEND it_act .

ENDFORM.                    " total_sum
*&---------------------------------------------------------------------*
*&      Form  get_data_wip
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_wip.
*Delay production
  PERFORM delayed_production.
*Total
  PERFORM wip_total.
ENDFORM.                    " get_data_wip

*&---------------------------------------------------------------------*
*&      Form  GET_FUCTION_CAHR_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_fuction_cahr_value CHANGING p_flag.
  CALL FUNCTION 'Z_FCA_GET_VEHICLE_MASTER'
    EXPORTING
      i_atnam                             = p_char_status
      i_atwrt_s                           = p_i_atwrt_s
      i_atwrt_e                           = p_i_atwrt_e
*   I_OBJEK                             =
*   I_COUNT                             = 1000000
   IMPORTING
      e_hit_count                         = t_count
    TABLES
      t_condition                         = it_condition
      t_value                             = it_value
      t_vehicle                           = it_vehicle
    EXCEPTIONS
      date_overflow                       = 1
      invalid_date                        = 2
      condition_does_not_exist            = 3
      characteristic_does_not_exist       = 4
      OTHERS                              = 5 .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    p_flag = 'X'.
  ENDIF.
ENDFORM.                    " GET_FUCTION_CAHR_VALUE
*&---------------------------------------------------------------------*
*&      Form  wip_total
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM wip_total.
  DATA : BEGIN OF it_wsum OCCURS 0,
         dqty  LIKE ztpp_pro_sum-rp01,   "Delay Prod Qty
         drate(3) TYPE p DECIMALS 2,     "Delay Prod rate
         cretu LIKE ztpp_pro_sum-rp01,   "control gate return
         cpass LIKE ztpp_pro_sum-rp01,   "control gate passed
         vpc   LIKE ztpp_pro_sum-rp01,   "vpc
         truck LIKE ztpp_pro_sum-rp01,   "trucking
         rai   LIKE ztpp_pro_sum-rp01,   "railing
         sqty  LIKE ztpp_pro_sum-rp01,   "sales qty
         shmaw LIKE ztpp_pro_sum-rp01,   "sales hma wip
         sfut  LIKE ztpp_pro_sum-rp01,   "sales future use
         ysum  LIKE ztpp_pro_sum-rp01,   "2004 summary
         END OF it_wsum.
  REFRESH it_wsum.CLEAR : it_wsum.
  LOOP AT it_wip.
    MOVE-CORRESPONDING it_wip TO it_wsum.
    COLLECT it_wsum.
  ENDLOOP.

  READ TABLE it_wsum INDEX 1.
  MOVE-CORRESPONDING it_wsum TO it_wip.
  MOVE 'TOTAL' TO it_wip-model.
  APPEND it_wip .

ENDFORM.                    " wip_total
*&---------------------------------------------------------------------*
*&      Form  delayed_production
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delayed_production.
  LOOP AT it_wip .
    p_dtime = it_wip-dtime.
* %
    READ TABLE it_act WITH KEY model = it_wip-model.
    IF it_act-twip <> 0.
      it_wip-drate = ( it_wip-dqty / it_act-twip ) * 100.
    ENDIF.
    IF p_rp <> '18'.
      it_wip-ysum = 0.
    ENDIF.
    MODIFY TABLE it_wip FROM it_wip.
  ENDLOOP.

ENDFORM.                    " delayed_production
*&---------------------------------------------------------------------*
*&      Form  conversion_date_yerterday
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DATE  text
*      <--P_Y_DATE  text
*----------------------------------------------------------------------*
FORM conversion_date_yerterday  USING  p_date
                     CHANGING c_date.
  DATA :lw_date  TYPE d,
        lt_date LIKE lw_date,
        holiday,
        w_kalid     LIKE kako-kalid,            "Calender ID
        w_mosid     LIKE kako-mosid,            "Schedule group
        w_kapid     LIKE kako-kapid,            "Capacity ID
        lw_daynr LIKE hrvsched-daynr,
       lw_dayfree LIKE hrvsched-noday.
  CLEAR : w_kalid, w_mosid,w_kapid,lw_date, lw_daynr,
          lw_dayfree.
*----- Read Shop Calendar ID
  SELECT SINGLE c~kalid c~mosid b~kapid
    INTO (w_kalid, w_mosid, w_kapid)
    FROM crhd AS a INNER JOIN crca AS b
                      ON a~objty = b~objty
                     AND a~objid = b~objid
                   INNER JOIN kako AS c
                      ON b~kapid = c~kapid
   WHERE a~werks =  p_plant
     AND a~arbpl =  'T'
     AND b~fork2 =  'SAP006'.

  lw_date = p_date - 1.
  DO.
    CALL FUNCTION 'RH_GET_DATE_DAYNAME'
         EXPORTING
              langu               = sy-langu
              date                = lw_date
              calid               = w_kalid
         IMPORTING
              daynr               = lw_daynr
              dayfree             = lw_dayfree
         EXCEPTIONS
              no_langu            = 1
              no_date             = 2
              no_daytxt_for_langu = 3
              invalid_date        = 4
              OTHERS              = 5.
    IF sy-subrc <> 0.
      RAISE etc_exception.
    ENDIF.

    IF lw_dayfree EQ 'X'.
      lw_date = lw_date - 1.
      CONTINUE.
    ELSE.
      c_date = lw_date .
      EXIT.
    ENDIF.
  ENDDO.
ENDFORM.                    " conversion_date_yerterday
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_data.
*Screen data
  PERFORM save_screen_data.
*CHECK TEXT AND SAVE
  PERFORM text_check_save.
ENDFORM.                    " SAVE_DATA
*&---------------------------------------------------------------------*
*&      Form  text_check_SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM text_check_save.
  DATA : wa_thead LIKE thead.
  CLEAR wa_thead.

  CONCATENATE 'P' p_date INTO name.
  wa_thead-tdobject = 'ZPP_SUM'.
  wa_thead-tdname = name.
  wa_thead-tdid = 'ZPP1'.
  wa_thead-tdspras = 'E'.

  CALL FUNCTION 'SAVE_TEXT'
    EXPORTING
     client                = sy-mandt
      header               = wa_thead
*    INSERT                = ' '
*    SAVEMODE_DIRECT       = ' '
*    OWNER_SPECIFIED       = ' '
*    LOCAL_CAT             = ' '
*  IMPORTING
*    FUNCTION              =
*    NEWHEADER             =
    TABLES
      lines                 = itftext
   EXCEPTIONS
     id                    = 1
     language              = 2
     name                  = 3
     object                = 4
     OTHERS                = 5
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    " text_check_SAVE
*&---------------------------------------------------------------------*
*&      Form  read_long_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_long_text.
  CLEAR name.

  CONCATENATE 'P' p_date INTO name.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
*    CLIENT                        = SY-MANDT
      id                            = 'ZPP1'
      language                      = 'E'
      name                          =  name
      object                        = 'ZPP_SUM'
*    ARCHIVE_HANDLE                = 0
*    LOCAL_CAT                     = ' '
*  IMPORTING
*    HEADER                        = Thead
    TABLES
      lines                         = itftext
  EXCEPTIONS
    id                            = 1
    language                      = 2
    name                          = 3
    not_found                     = 4
    object                        = 5
    reference_check               = 6
    wrong_access_to_archive       = 7
    OTHERS                        = 8  .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " read_long_text
*&---------------------------------------------------------------------*
*&      Form  save_screen_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_screen_data.
  LOOP AT it_act WHERE model <> 'TOTAL'.

    UPDATE ztpp_pro_sum SET : twip = it_act-twip
      WHERE p_date = p_date
        AND werks  = p_plant
        AND model  = it_act-model.

  ENDLOOP.

  LOOP AT it_wip WHERE model <> 'TOTAL'.

    UPDATE ztpp_pro_sum SET : dqty  = it_wip-dqty
                              cretu = it_wip-cretu
                              cpass = it_wip-cpass
                              sqty  = it_wip-sqty
                              shmaw = it_wip-shmaw
                              ysum  = it_wip-ysum
      WHERE p_date = p_date
        AND werks  = p_plant
        AND model  = it_wip-model.

  ENDLOOP.
  MESSAGE s001 WITH 'Successfuly Updated'.
ENDFORM.                    " save_screen_data
*&---------------------------------------------------------------------*
*&      Form  PRINT_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRINT_REPORT.
  DATA :LW_FUNCNAME TYPE RS38L_FNAM .
  CLEAR :LW_FUNCNAME.


* Begin of changes - UD1K919014
  if P_RP = '01'  .
    data : l_var(4) type c value 'RP02'.
* Current System displays one reporting point..When Option 1 is choosen
*need to display all 5 reporting points.
    refresh it_act1. clear it_act1.
    loop at it_act where model <> 'TOTAL'.
      move-corresponding it_act to it_act1.
      it_act1-opt = '01'.
      Append  it_act1.

*  Populate internal table for Option 02
      move-corresponding it_act to it_act1.
      it_act1-opt = '02'.
      SELECT SINGLE   RP02
       INTO (it_act1-yact)  FROM ztpp_pro_sum
          WHERE werks EQ p_plant
            AND model EQ it_act-model
            AND p_date EQ y_date.
*Yesterday diff
      it_act1-ydiff = it_act1-yact - it_act1-yplan.
* Summary month
      SELECT SUM( RP02 ) INTO it_act1-sact
       FROM ztpp_pro_sum
         WHERE werks EQ p_plant
           AND model EQ  it_act-model
           AND p_date IN s_date.

      it_act1-sdiff = it_act1-sact - it_act1-splan.
*Remain Qty = Operation plan - month actual
      it_act1-reqty = it_act1-mope - it_act1-sact.
      append it_act1.

*  Populate internal table for Option    04
      move-corresponding it_act to it_act1.
      it_act1-opt = '04'.
      SELECT SINGLE  RP04
       INTO (it_act1-yact)  FROM ztpp_pro_sum
          WHERE werks EQ p_plant
            AND model EQ it_act-model
            AND p_date EQ y_date.
*Yesterday diff
      it_act1-ydiff = it_act1-yact - it_act1-yplan.
* Summary month
      SELECT SUM( RP04 ) INTO it_act1-sact
       FROM ztpp_pro_sum
         WHERE werks EQ p_plant
           AND model EQ  it_act-model
           AND p_date IN s_date.

      it_act1-sdiff = it_act1-sact - it_act1-splan.
*Remain Qty = Operation plan - month actual
      it_act1-reqty = it_act1-mope - it_act1-sact.
      append it_act1.


*  Populate for Option    06
      move-corresponding it_act to it_act1.
      it_act1-opt = '06'.
      SELECT SINGLE  RP06
       INTO (it_act1-yact)  FROM ztpp_pro_sum
          WHERE werks EQ p_plant
            AND model EQ it_act-model
            AND p_date EQ y_date.
*Yesterday diff
      it_act1-ydiff = it_act1-yact - it_act1-yplan.
* Summary month
      SELECT SUM( RP06 ) INTO it_act1-sact
       FROM ztpp_pro_sum
         WHERE werks EQ p_plant
           AND model EQ  it_act-model
           AND p_date IN s_date.

      it_act1-sdiff = it_act1-sact - it_act1-splan.
*Remain Qty = Operation plan - month actual
      it_act1-reqty = it_act1-mope - it_act1-sact.
      append it_act1.

*  Populate for Option    07
      move-corresponding it_act to it_act1.
      it_act1-opt = '07'.
      SELECT SINGLE  RP07
      INTO (it_act1-yact)  FROM ztpp_pro_sum
         WHERE werks EQ p_plant
           AND model EQ it_act-model
           AND p_date EQ y_date.
*Yesterday diff
      it_act1-ydiff = it_act1-yact - it_act1-yplan.
* Summary month
      SELECT SUM( RP07 ) INTO it_act1-sact
       FROM ztpp_pro_sum
         WHERE werks EQ p_plant
           AND model EQ  it_act-model
           AND p_date IN s_date.

      it_act1-sdiff = it_act1-sact - it_act1-splan.
*Remain Qty = Operation plan - month actual
      it_act1-reqty = it_act1-mope - it_act1-sact.
      append it_act1.

*  Populate for Option    18
      move-corresponding it_act to it_act1.
      it_act1-opt = '18'.
      SELECT SINGLE  RP18
     INTO (it_act1-yact)  FROM ztpp_pro_sum
        WHERE werks EQ p_plant
          AND model EQ it_act-model
          AND p_date EQ y_date.
*Yesterday diff
      it_act1-ydiff = it_act1-yact - it_act1-yplan.
* Summary month
      SELECT SUM( RP18 ) INTO it_act1-sact
       FROM ztpp_pro_sum
         WHERE werks EQ p_plant
           AND model EQ  it_act-model
           AND p_date IN s_date.

      it_act1-sdiff = it_act1-sact - it_act1-splan.
*Remain Qty = Operation plan - month actual
      it_act1-reqty = it_act1-mope - it_act1-sact.
      append it_act1.
    endloop.
* Compute  TOTAL
    perform compute_totals.

    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
       EXPORTING
            FORMNAME           = WA_FORMNAME1
*            VARIANT            = ' '
*            DIRECT_CALL        = ' '
       IMPORTING
            FM_NAME            = LW_FUNCNAME
       EXCEPTIONS
            NO_FORM            = 1
            NO_FUNCTION_MODULE = 2
            OTHERS             = 3.

    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    CHECK SY-SUBRC = 0.
    sort it_act1 by opt model.
    CALL FUNCTION LW_FUNCNAME
         EXPORTING
              P_PLANT          = P_PLANT
              P_RP             = P_RP
              P_DATE           = P_DATE
              P_DTIME          = P_DTIME
              N_DATE           = N_DATE
              N_TIME           = N_TIME
         TABLES
              T_ACT            = IT_ACT1
              T_WIP            = IT_WIP
              TTEXT            = ITFTEXT
         EXCEPTIONS
              FORMATTING_ERROR = 1
              INTERNAL_ERROR   = 2
              SEND_ERROR       = 3
              USER_CANCELED    = 4
              OTHERS           = 5.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.


  else.

    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
       EXPORTING
            FORMNAME           = WA_FORMNAME
*            VARIANT            = ' '
*            DIRECT_CALL        = ' '
       IMPORTING
            FM_NAME            = LW_FUNCNAME
       EXCEPTIONS
            NO_FORM            = 1
            NO_FUNCTION_MODULE = 2
            OTHERS             = 3.

    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    CHECK SY-SUBRC = 0.

** "/Dev-'/1BCDWB/SF00000028'
    CALL FUNCTION LW_FUNCNAME
         EXPORTING
              P_PLANT          = P_PLANT
              P_RP             = P_RP
              P_DATE           = P_DATE
              P_DTIME          = P_DTIME
              N_DATE           = N_DATE
              N_TIME           = N_TIME
         TABLES
              T_ACT            = IT_ACT
              T_WIP            = IT_WIP
              TTEXT            = ITFTEXT
         EXCEPTIONS
              FORMATTING_ERROR = 1
              INTERNAL_ERROR   = 2
              SEND_ERROR       = 3
              USER_CANCELED    = 4
              OTHERS           = 5.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  endif.
* END of changes - UD1K919014
ENDFORM.                    " PRINT_REPORT
*&---------------------------------------------------------------------*
*&      Form  compute_totals
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM compute_totals.
  DATA : BEGIN OF it_asum OCCURS 0,
            opt(2)    type c,
            model LIKE ztpp_pro_sum-model,"Model
            mann  LIKE ztpp_pro_sum-rp01, "Month-plan annual
            mope  LIKE ztpp_pro_sum-rp01, "Month-plan oper
            yplan LIKE ztpp_pro_sum-rp01, "Yesterday plan
            yact  LIKE ztpp_pro_sum-rp01, "Yesterday-actual
            ydiff LIKE ztpp_pro_sum-rp01, "Yesterday-diff
            splan LIKE ztpp_pro_sum-rp01, "Summary Month
            sact  LIKE ztpp_pro_sum-rp01, "Summary actual
            sdiff LIKE ztpp_pro_sum-rp01, "Summary difference
            reqty LIKE ztpp_pro_sum-rp01, "Remain Qty
            tplan LIKE ztpp_pro_sum-rp01, "Today plan
            twip  LIKE ztpp_pro_sum-rp01, "T/R WIP
         END OF it_asum.

  REFRESH it_asum.CLEAR : it_asum.
  LOOP AT it_act1 WHERE model <> 'TOTAL'.
    MOVE-CORRESPONDING it_act1 TO it_asum.
    MOVE 'TOTAL' TO it_asum-model.
    COLLECT it_asum.
  ENDLOOP.

  append lines of it_asum to it_act1.


ENDFORM.                    " compute_totals
