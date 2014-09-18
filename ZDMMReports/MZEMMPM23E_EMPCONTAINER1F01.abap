************************************************************************
* Program name : SAPMZEMMPM23E_EMPCONTAINER1
* Created by  : Min-su Park                                            *
* Created on  : 2003.09.25.                                            *
* Description :                                                        *
*   1. Modified Welcome screen-For Inbound delivery-Putaway process    *
*   2. Empty Container management                                      *
*                                                                      *
* Modification Log                                                     *
* Date            Developer        Request No.    Description          *
* 2003.09.19.     Min-su Park      UD1K901873     Initial Coding       *
*                                                                      *
************************************************************************
*----------------------------------------------------------------------*
***INCLUDE MZEMMPM23E_EMPCONTAINERF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save.
  CASE w_mode.
    WHEN 'C'.
*--UD1K922620  BY HASEEB
    PERFORM job.


      IF W_DELETE = 'Y'.
        MESSAGE s022.
      ENDIF.

      W_DELETE = ''.
*--UD1K922620  BY HASEEB

*      PERFORM CREATE_INBOUND_TO.
*      PERFORM CREATE_SAVE.
**     PERFORM GR.
*      PERFORM TEST.
    WHEN 'M'.
      PERFORM change_save.
  ENDCASE.

ENDFORM.                    " SAVE
*&---------------------------------------------------------------------*
*&      Form  BDC_PASS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0010   text
*      -->P_0011   text
*      -->P_0012   text
*----------------------------------------------------------------------*
FORM bdc_pass USING par1 par2 par3.
  CLEAR it_bdc.
  IF par1 = 'X'.
    it_bdc-dynbegin = 'X'.
    it_bdc-program  = par2.
    it_bdc-dynpro   = par3.
    APPEND it_bdc.
  ELSE.
    it_bdc-fnam = par2.
    it_bdc-fval = par3.
    APPEND it_bdc.
  ENDIF.
ENDFORM.                    " BDC_PASS
*&---------------------------------------------------------------------*
*&      Form  CREATE_SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_save.
  DATA    : date(10).
  DATA    : viech(20), contain(20).
  CLEAR   : it_bdc, it_message.
  REFRESH : it_bdc, it_message.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
       EXPORTING
            input  = leci_tra_dyn-vehicle_reg_numb
       IMPORTING
            output = viech.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
       EXPORTING
            input  = leci_tra_dyn-cont_reg_numb1
       IMPORTING
            output = contain.

  WRITE : leci_tra_dyn-pass_date TO date.
  PERFORM bdc_pass USING:
    'X' 'SAPRLECHKIN' '1000'                            ,
*Check in
    ' ' 'LECI_TRA_DYN-PASS_DATE' date                   ,
*Check in - Time
    ' ' 'LECI_TRA_DYN-PASS_TIME' leci_tra_dyn-pass_time+0(4),
*Truck license
    ' ' 'LECI_TRA_DYN-VEHICLE_REG_NUMB' viech           ,
*1st container number
    ' ' 'LECI_TRA_DYN-CONT_REG_NUMB1' contain           ,
*Deliver last name
    ' ' 'LECI_TRA_DYN-NAME_DRVR' leci_tra_dyn-name_drvr ,
*Door for whse
    ' ' 'LECI_TRA_DYN-WHS_GATE'  leci_tra_dyn-whs_gate  ,
*PrkngSpce
    ' ' 'LECI_TRA_DYN-PARKING_TXT' leci_tra_dyn-parking_txt,
    ' ' 'BDC_OKCODE' '=SAVE'.

  CALL TRANSACTION 'LECI'
           USING it_bdc
           MODE w_status
           UPDATE'S'
           MESSAGES INTO it_message.
  IF sy-subrc <> 0.
    READ TABLE it_message WITH KEY msgtyp = 'E'.
    IF sy-subrc = 0.
      PERFORM error_message_display.
    ENDIF.
  ELSE.
    PERFORM create_ztmm_container_ct_yard.
    w_mode = 'E'.
  ENDIF.
ENDFORM.                    " CREATE_SAVE
*&---------------------------------------------------------------------*
*&      Form  CHANGE_SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_save.
  DATA : viech(20), viech1(20), contain(20) .
  CLEAR   : it_bdc, it_message.
  REFRESH : it_bdc, it_message.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
       EXPORTING
            input  = leci_tra_dyn-vehicle_reg_numb
       IMPORTING
            output = viech.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
       EXPORTING
            input  = leci_tra_dyn-cont_reg_numb2
       IMPORTING
            output = viech1.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
       EXPORTING
            input  = leci_tra_dyn-cont_reg_numb1
       IMPORTING
            output = contain.
  PERFORM bdc_pass USING:
    'X' 'SAPRLECHKIN' '1000'                 ,
    ' ' 'LECI_SELOPT_DYN-PASS_NUMB' w_pass_numb,
    ' ' 'BDC_OKCODE' '=SEARCH'               .

  PERFORM bdc_pass USING:
    'X' 'SAPRLECHKIN' '1000'                 ,
    ' ' 'BDC_OKCODE' '=TOGGLE'               .

  PERFORM bdc_pass USING:
    'X' 'SAPRLECHKIN' '1000'                            .
*   Check out
  IF leci_tra_dyn-leave_date IS INITIAL.
  ELSE.
    DATA : date(10).
    WRITE : leci_tra_dyn-leave_date TO date.
    PERFORM bdc_pass USING:
     ' ' 'LECI_TRA_DYN-LEAVE_DATE' date                        ,
     ' ' 'LECI_TRA_DYN-LEAVE_TIME' leci_tra_dyn-leave_time+0(4).
  ENDIF.
*   Truck's license
  PERFORM bdc_pass USING:
    ' ' 'LECI_TRA_DYN-VEHICLE_REG_NUMB' viech,
*   1st Container Number
    ' ' 'LECI_TRA_DYN-CONT_REG_NUMB1' contain,
*   Driver Name
    ' ' 'LECI_TRA_DYN-NAME_DRVR' leci_tra_dyn-name_drvr,
*   Door for whse
    ' ' 'LECI_TRA_DYN-WHS_GATE'  leci_tra_dyn-whs_gate ,
*   2st Container Number
    ' ' 'LECI_TRA_DYN-CONT_REG_NUMB2' viech1,
    ' ' 'BDC_OKCODE' '=SAVE'.

  CALL TRANSACTION 'LECI'
       USING it_bdc
       MODE w_status
       UPDATE'S'
       MESSAGES INTO it_message.
  IF sy-subrc <> 0.
    READ TABLE it_message WITH KEY msgtyp = 'E'.
    IF sy-subrc = 0.
      PERFORM error_message_display.
    ENDIF.
  ELSE.
    PERFORM change_ztmm_container_ct_yard.
  ENDIF.
  CLEAR w_pass_numb.
ENDFORM.                    " CHANGE_SAVE
*&---------------------------------------------------------------------*
*&      Form  FIND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM find.
  TABLES leci_event_data.
  DATA : chk_in  LIKE leci_event-event_date,
         chk_in_time  LIKE leci_tra_dyn-pass_time,
         chk_out LIKE leci_event-event_date,
         chk_out_time LIKE leci_tra_dyn-leave_time,
         flg_proc_fin LIKE leci_event-flg_proc_fin,
         guid_event_data LIKE leci_event_data-guid_event_data,
         chk_guid_event1  LIKE leci_event_data-guid_event_data,
         chk_guid_event2  LIKE leci_event_data-guid_event_data.

  CASE w_fcode.
    WHEN 'PRINT'.
      SELECT SINGLE *
              FROM leci_event_data
             WHERE cont_reg_numb1 = leci_tra_dyn-cont_reg_numb1.
    WHEN OTHERS.
* mspark(20031007)-START
      IF leci_selopt_dyn-pass_numb IS INITIAL AND
         w_cont_reg_numb1 IS INITIAL.
        MESSAGE e014.
      ELSEIF leci_selopt_dyn-pass_numb IS INITIAL AND
             NOT w_cont_reg_numb1 IS INITIAL.

      ELSEIF NOT leci_selopt_dyn-pass_numb IS INITIAL AND
             w_cont_reg_numb1 IS INITIAL.
        SELECT SINGLE guid_event_data
                 INTO guid_event_data
                 FROM leci_event
                WHERE pass_numb = leci_selopt_dyn-pass_numb.
        SELECT SINGLE cont_reg_numb1
                 INTO w_cont_reg_numb1
                 FROM leci_event_data
                WHERE guid_event_data = guid_event_data.
      ELSEIF NOT leci_selopt_dyn-pass_numb IS INITIAL AND
             NOT w_cont_reg_numb1 IS INITIAL.
        SELECT SINGLE guid_event_data
                 INTO chk_guid_event1
                 FROM leci_event
                WHERE pass_numb = leci_selopt_dyn-pass_numb.
        SELECT SINGLE guid_event_data
                 INTO chk_guid_event2
                 FROM leci_event_data
                WHERE cont_reg_numb1 = w_cont_reg_numb1.
        IF chk_guid_event1 = chk_guid_event2.

        ELSE.
          MESSAGE e015.
        ENDIF.
      ENDIF.
* mspark(20031007)-END
      SELECT SINGLE *
               FROM leci_event_data
             WHERE cont_reg_numb1 = w_cont_reg_numb1.
  ENDCASE.
  IF sy-subrc <> 0.
    MESSAGE e004 WITH leci_tra_dyn-cont_reg_numb1.
  ELSE.
*Get Screen field
    CLEAR : leci_tra_dyn, w_pass_numb.
    leci_tra_dyn-vehicle_reg_numb
      = leci_event_data-vehicle_reg_numb.  "Truck's license
    leci_tra_dyn-cont_reg_numb1
      = leci_event_data-cont_reg_numb1  .  "1st Container no
    leci_tra_dyn-name_drvr
      = leci_event_data-name_drvr       .  "Deliver last name
    leci_tra_dyn-id_card_numb
      = leci_event_data-id_card_numb    .  "ID Number(Dock Number)
    leci_tra_dyn-parking_txt
      = leci_event_data-parking_txt     .  "PrkngSpce
    CASE w_chk_point.
      WHEN 'GATE 1'.
        SPLIT leci_event_data-parking_txt
         AT '-' INTO lein-letyp w_nltyp w_nlber w_nlpla.
      WHEN OTHERS.
        SPLIT leci_event_data-parking_txt
         AT '-' INTO w_nltyp w_nlber w_nlpla.
    ENDCASE.

   SELECT SINGLE event_date event_time flg_proc_fin pass_numb chk_point
                           FROM leci_event
                   INTO (chk_in, chk_in_time, flg_proc_fin, w_pass_numb,
                                 w_chk_point)
                          WHERE event = 'CI'
                  AND guid_event_data = leci_event_data-guid_event_data.
    leci_tra_dyn-pass_date = chk_in.       "Check in
    leci_tra_dyn-pass_time = chk_in_time.  "Check in Time
    leci_selopt_dyn-pass_numb = w_pass_numb.
    SELECT SINGLE event_date event_time
             FROM leci_event
             INTO (chk_out, chk_out_time)
            WHERE event = 'CO'
              AND guid_event_data = leci_event_data-guid_event_data.
    leci_tra_dyn-leave_date = chk_out.     "Check out
    leci_tra_dyn-leave_time = chk_out_time.
    IF flg_proc_fin = 'X'.
      w_mode = 'D'.
    ELSE.
      w_mode = 'M'.
    ENDIF.
  ENDIF.
  IF w_fcode = 'PRINT'.
    w_mode = 'E'.
  ENDIF.
ENDFORM.                    " FIND
*&---------------------------------------------------------------------*
*&      Form  CREATE_INBOUND_TO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_inbound_to.
  DATA : traid LIKE likp-traid,
         it_likps  LIKE likp OCCURS 0 WITH HEADER LINE ,
         contain(20)          .

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
       EXPORTING
            input  = leci_tra_dyn-cont_reg_numb1
       IMPORTING
            output = contain.

*Find MsntrnspId which is same with Container No.
  SELECT *
         INTO CORRESPONDING FIELDS OF TABLE it_likps
         FROM likp
        WHERE lfart = 'EL'
*          AND traty = '0005'
          AND traid = contain.
  CASE sy-subrc.
    WHEN 0.
      LOOP AT it_likps.
        PERFORM create_to_from_inbound USING it_likps.
      ENDLOOP.
    WHEN OTHERS.
      MESSAGE e005.
  ENDCASE.
ENDFORM.                    " CREATE_INBOUND_TO
*&---------------------------------------------------------------------*
*&      Form  CREATE_TO_FROM_INBOUND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VBELN  text
*----------------------------------------------------------------------*
FORM create_to_from_inbound USING p_likp LIKE likp.
  DATA : BEGIN OF it_lips OCCURS 0.
          INCLUDE STRUCTURE lips.
  DATA : END OF it_lips.
  DATA : no TYPE i     .

*[ 0 ] . Get Inbound Items
  SELECT * FROM lips
           INTO CORRESPONDING FIELDS OF TABLE it_lips
          WHERE vbeln = p_likp-vbeln.
  CHECK sy-subrc = 0.
  DESCRIBE TABLE it_lips LINES no.

*[ 1 ] . Create TO
  CLEAR   : it_bdc, it_message.
  REFRESH : it_bdc, it_message.

  PERFORM bdc_pass USING:
     'X' 'SAPML03T'    '0151'      ,
     ' ' 'LTAK-LGNUM'  'P01'       ,
     ' ' 'VBLKK-VBELN' p_likp-vbeln,
     ' ' 'BDC_OKCODE'  '/00'       .

  CASE no.
    WHEN 1.
*[ 1 ] - 1. TO Creation from one-Item
      LOOP AT it_lips.
*        IF IT_LIPS-LIFEXPOS IS INITIAL.
*          MESSAGE E011 WITH SY-TABIX.
*        ELSE.
*          IF IT_LIPS-LIFEXPOS = 10.
*            CONCATENATE NLPLA '/' IT_LIPS-LIFEXPOS+4(2) INTO NLPLAT.
*          ELSE.
*            CONCATENATE NLPLA '/' IT_LIPS-LIFEXPOS+5(1) INTO NLPLAT.
*          ENDIF.
*        ENDIF.
        IF sy-tabix > 1.
          PERFORM bdc_pass USING:
            'X' 'SAPML03T'    '0104'     ,
            ' ' 'BDC_OKCODE'  '=TATB'    .
        ENDIF.
        PERFORM bdc_pass USING:
          'X' 'SAPML03T'    '0104'     ,
          ' ' 'RL03T-LETY2' 'BB'       ,
          ' ' 'BDC_OKCODE'  '/00'      .

        PERFORM bdc_pass USING:
          'X' 'SAPML03T'    '0104'           ,
          ' ' 'LTAPE-NLENR(01)' it_lips-kdmat,
          ' ' 'BDC_OKCODE'  '/00'            .

        PERFORM bdc_pass USING:
          'X' 'SAPML03T'    '0104'     ,
          ' ' 'BDC_OKCODE'  '=TAH1'    .
        PERFORM bdc_pass USING:
          'X' 'SAPML03T'    '0102'     ,
          ' ' 'LTAP-NLTYP'  w_nltyp    , "100
          ' ' 'LTAP-NLBER'  w_nlber    , "001
          ' ' 'LTAP-NLPLA'  w_nlpla    ,                    "AA - 02
**        ' ' 'LTAP-NLENR'  IT_LIPS-EAN11, "Storage Unit
*         ' ' 'LTAP-NLENR' IT_LIPS-KDMAT,
          ' ' 'BDC_OKCODE'  '/00'      .
      ENDLOOP.
      PERFORM bdc_pass USING:
         'X' 'SAPML03T'    '0104'     ,
         ' ' 'BDC_OKCODE'  '=BU'      .

    WHEN OTHERS.
*[ 1 ] - 2. TO Creation from multi-Item
      PERFORM bdc_pass USING:
         'X' 'SAPML03T'    '0154'      ,
         ' ' 'BDC_OKCODE'  '=MRKA'     . "Select All

      PERFORM bdc_pass USING:
         'X' 'SAPML03T'    '0154'      ,
         ' ' 'BDC_OKCODE'  '=TPAL'     . "Palletization

      LOOP AT it_lips.
*        IF IT_LIPS-LIFEXPOS IS INITIAL.
*          MESSAGE E011 WITH SY-TABIX.
*        ELSE.
*          IF IT_LIPS-LIFEXPOS = 10.
*            CONCATENATE NLPLA '/' IT_LIPS-LIFEXPOS+4(2) INTO NLPLAT.
*          ELSE.
*            CONCATENATE NLPLA '/' IT_LIPS-LIFEXPOS+5(1) INTO NLPLAT.
*          ENDIF.
*        ENDIF.
        IF sy-tabix > 1.
          PERFORM bdc_pass USING:
            'X' 'SAPML03T'    '0104'     ,
            ' ' 'BDC_OKCODE'  '=TATB'    .
        ENDIF.
        PERFORM bdc_pass USING:
          'X' 'SAPML03T'    '0104'     ,
          ' ' 'RL03T-LETY2' 'BB'       ,
          ' ' 'BDC_OKCODE'  '/00'      .

        PERFORM bdc_pass USING:
          'X' 'SAPML03T'    '0104'           ,
          ' ' 'LTAPE-NLENR(01)' it_lips-kdmat,
          ' ' 'BDC_OKCODE'  '/00'            .

        PERFORM bdc_pass USING:
          'X' 'SAPML03T'    '0104'     ,
          ' ' 'BDC_OKCODE'  '=TAH1'    .
        PERFORM bdc_pass USING:
          'X' 'SAPML03T'    '0102'     ,
          ' ' 'LTAP-NLTYP'  w_nltyp    , "100
          ' ' 'LTAP-NLBER'  w_nlber    , "001
          ' ' 'LTAP-NLPLA'  w_nlpla    ,                    "AA - 02
**        ' ' 'LTAP-NLENR'  IT_LIPS-EAN11, "Storage Unit
*         ' ' 'LTAP-NLENR' IT_LIPS-KDMAT,
          ' ' 'BDC_OKCODE'  '/00'      .
      ENDLOOP.
      PERFORM bdc_pass USING:
         'X' 'SAPML03T'    '0104'     ,
         ' ' 'BDC_OKCODE'  '=BU'      .
  ENDCASE.

  CALL TRANSACTION 'LT03'
              USING it_bdc
              MODE w_status
              UPDATE'S'
              MESSAGES INTO it_message.
  IF sy-subrc <> 0.
    READ TABLE it_message WITH KEY msgtyp = 'E'.
    IF sy-subrc = 0.
      PERFORM error_message_display.
    ELSE.
    ENDIF.
  ENDIF.
ENDFORM.                    " CREATE_TO_FROM_INBOUND
*&---------------------------------------------------------------------*
*&      Form  SAVE_ZTMM_DOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_ztmm_dock.
  CASE w_mode.
    WHEN 'C'. PERFORM create_dock.
    WHEN 'M'. PERFORM change_dock.
  ENDCASE.
ENDFORM.                    " SAVE_ZTMM_DOCK
*&---------------------------------------------------------------------*
*&      Form  CREATE_DOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_dock.
  FIELD-SYMBOLS : <fs>.
  TABLES : ztmm_dock.
  DATA : field(20), cnt(02) TYPE n, flg1, flg2, date(10),
         viech(20), contain(20).
  CLEAR   : it_bdc, it_message.
  REFRESH : it_bdc, it_message.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
       EXPORTING
            input  = leci_tra_dyn-vehicle_reg_numb
       IMPORTING
            output = viech.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
       EXPORTING
            input  = leci_tra_dyn-cont_reg_numb1
       IMPORTING
            output = contain.

  WRITE : leci_tra_dyn-pass_date TO date.

  PERFORM bdc_pass USING:
    'X' 'SAPRLECHKIN' '1000'                            ,
*Check in
    ' ' 'LECI_TRA_DYN-PASS_DATE'  date                  ,
*Check in - Time
    ' ' 'LECI_TRA_DYN-PASS_TIME' leci_tra_dyn-pass_time+0(4),
*Truck license
    ' ' 'LECI_TRA_DYN-VEHICLE_REG_NUMB' viech           ,
*1st container number
    ' ' 'LECI_TRA_DYN-CONT_REG_NUMB1' contain           ,
*Deliver last name
    ' ' 'LECI_TRA_DYN-NAME_DRVR' leci_tra_dyn-name_drvr ,
*ID number(Dock Number)
    ' ' 'LECI_TRA_DYN-ID_CARD_NUMB' leci_tra_dyn-id_card_numb,
*Door for whse
    ' ' 'LECI_TRA_DYN-WHS_GATE'  leci_tra_dyn-whs_gate ,
*PrkngSpce
    ' ' 'LECI_TRA_DYN-PARKING_TXT' leci_tra_dyn-parking_txt,
    ' ' 'BDC_OKCODE' '=SAVE'.

*Check Dock Full or Not And Existence.
**--- insert by stlim (2004/04/26)
  IF sy-tcode NE c_tcode.
    SELECT SINGLE * FROM ztmm_dock
            WHERE zdock = leci_tra_dyn-id_card_numb.
    CASE sy-subrc.
      WHEN 0.     "Check Dock Full or Not
        DO 10 TIMES.
          cnt = sy-index.
          CONCATENATE 'ZTMM_DOCK-TR_' cnt INTO field.
          ASSIGN (field) TO <fs>.
          IF <fs> IS INITIAL.
            flg1 = 'X'.
          ENDIF.
          IF <fs> = leci_tra_dyn-cont_reg_numb1.
            flg2 = 'X'.
          ENDIF.
        ENDDO.
        IF flg1 <> 'X'. MESSAGE e007. ENDIF.
        IF flg2 = 'X' . MESSAGE e008. ENDIF.
      WHEN OTHERS. "Check Dock Existence or not
        MESSAGE e006.
    ENDCASE.
  ENDIF.
**--- end of insert

*Call Transaction.
  CALL TRANSACTION 'LECI'
           USING it_bdc
           MODE w_status
           UPDATE'S'
           MESSAGES INTO it_message.
  IF sy-subrc <> 0.
    READ TABLE it_message WITH KEY msgtyp = 'E'.
    IF sy-subrc = 0.
      PERFORM error_message_display.
    ENDIF.
  ELSE.
    DO 10 TIMES.
      cnt = sy-index.
      CONCATENATE 'ZTMM_DOCK-TR_' cnt INTO field.
      ASSIGN (field) TO <fs>.
      IF <fs> IS INITIAL.
        <fs> = leci_tra_dyn-cont_reg_numb1.
        ztmm_dock-erdat = sy-datum.
        ztmm_dock-erzet = sy-uzeit.
        ztmm_dock-ernam = sy-uname.
        UPDATE ztmm_dock FROM ztmm_dock.
        EXIT.
      ENDIF.
    ENDDO.
  ENDIF.
ENDFORM.                    " CREATE_DOCK
*&---------------------------------------------------------------------*
*&      Form  CHANGE_DOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_dock.
  FIELD-SYMBOLS : <fs>.
  DATA : field(20), cnt(02) TYPE n, date(10),
         viech(20), contain(20) .
  CLEAR   : it_bdc, it_message.
  REFRESH : it_bdc, it_message.
  WRITE : leci_tra_dyn-leave_date TO date.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
       EXPORTING
            input  = leci_tra_dyn-vehicle_reg_numb
       IMPORTING
            output = viech.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
       EXPORTING
            input  = leci_tra_dyn-cont_reg_numb1
       IMPORTING
            output = contain.
  PERFORM bdc_pass USING:
    'X' 'SAPRLECHKIN' '1000'                 ,
    ' ' 'LECI_SELOPT_DYN-PASS_NUMB' w_pass_numb,
    ' ' 'BDC_OKCODE' '=SEARCH'               .

  PERFORM bdc_pass USING:
    'X' 'SAPRLECHKIN' '1000'                 ,
    ' ' 'BDC_OKCODE' '=TOGGLE'               .

  PERFORM bdc_pass USING:
    'X' 'SAPRLECHKIN' '1000'                            .
*   Check out
  IF leci_tra_dyn-leave_date IS INITIAL.
  ELSE.
    PERFORM bdc_pass USING:
     ' ' 'LECI_TRA_DYN-LEAVE_DATE' date  ,
     ' ' 'LECI_TRA_DYN-LEAVE_TIME' leci_tra_dyn-leave_time+0(4).
  ENDIF.
*   Truck's license
  PERFORM bdc_pass USING:
    ' ' 'LECI_TRA_DYN-VEHICLE_REG_NUMB' viech,
*   1st Container Number
    ' ' 'LECI_TRA_DYN-CONT_REG_NUMB1' contain,
*   Driver last Name
    ' ' 'LECI_TRA_DYN-NAME_DRVR' leci_tra_dyn-name_drvr,
*   ID number(Dock Number)
    ' ' 'LECI_TRA_DYN-ID_CARD_NUMB' leci_tra_dyn-id_card_numb,
*Door for whse
    ' ' 'LECI_TRA_DYN-WHS_GATE'  leci_tra_dyn-whs_gate ,
    ' ' 'BDC_OKCODE' '=SAVE'.

  CALL TRANSACTION 'LECI'
       USING it_bdc
       MODE w_status
       UPDATE'S'
       MESSAGES INTO it_message.
  IF sy-subrc <> 0.
    READ TABLE it_message WITH KEY msgtyp = 'E'.
    IF sy-subrc = 0.
      PERFORM error_message_display.
    ENDIF.
  ELSE.
    CLEAR ztmm_dock.
    SELECT SINGLE * FROM ztmm_dock
            WHERE zdock = leci_tra_dyn-id_card_numb.
    IF sy-subrc = 0.
      CHECK NOT leci_tra_dyn-leave_date IS INITIAL.
      w_mode = 'D'.
      DO 10 TIMES.
        cnt = sy-index.
        CONCATENATE 'ZTMM_DOCK-TR_' cnt INTO field.
        ASSIGN (field) TO <fs>.
        IF <fs> = leci_tra_dyn-cont_reg_numb1.
          CLEAR : <fs>.
*           Time Stmp
*--- blocked by stlim (2004/07/27)
*          ztmm_container-aedat = sy-datum.
*          ztmm_container-aezet = sy-uzeit.
*          ztmm_container-aenam = sy-uname.
*--- end of block

*--- insert by stlim (2004/07/27)
          ztmm_dock-aedat = sy-datum.
          ztmm_dock-aezet = sy-uzeit.
          ztmm_dock-aenam = sy-uname.
*--- end of insert
          UPDATE ztmm_dock FROM ztmm_dock.
          EXIT.
        ENDIF.
      ENDDO.
    ENDIF.

  ENDIF.
ENDFORM.                    " CHANGE_DOCK
*&---------------------------------------------------------------------*
*&      Form  CREATE_ZTMM_CONTAINER_CT_YARD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_ztmm_container_ct_yard.
  CLEAR ztmm_container.
*[ 1 ] Get Storage Bin Data after TO Creation correctly
*Data emptied, Previous Location, Empty


  SELECT SINGLE bdatu "Date emptied
                lgpla "Previous Location
                kzler "Empty(STATUS)
           INTO (ztmm_container-bdatu,
                 ztmm_container-lgpla,
                 ztmm_container-kzler)
           FROM lagp
          WHERE lgnum = 'P01'
            AND lgtyp = w_nltyp
            AND lgpla = w_nlpla.
*[ 2 ] Get Container Number and Check-in from Screen.
  ztmm_container-cont_reg_numb1 = leci_tra_dyn-cont_reg_numb1.
  ztmm_container-pass_date      = leci_tra_dyn-pass_date     .
*[ 3 ] TIME STMP
  ztmm_container-erdat = sy-datum.
  ztmm_container-erzet = sy-uzeit.
  ztmm_container-ernam = sy-uname.
  INSERT ztmm_container FROM ztmm_container.



ENDFORM.                    " CREATE_ZTMM_CONTAINER_CT_YARD
*&---------------------------------------------------------------------*
*&      Form  CHANGE_ZTMM_CONTAINER_CT_YARD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_ztmm_container_ct_yard.
*  CLEAR ZTMM_CONTAINER.
*  SELECT SINGLE * FROM ZTMM_CONTAINER
*                 WHERE CONT_REG_NUMB1 = W_CONT_REG_NUMB1.
**[ 1 ] Get Change Field.
**Container
*  ZTMM_CONTAINER-CONT_REG_NUMB1 = W_CONT_REG_NUMB1.
**Check-in
*  ZTMM_CONTAINER-PASS_DATE      = LECI_TRA_DYN-PASS_DATE.
**Check-out
*  ZTMM_CONTAINER-LEAVE_DATE     = LECI_TRA_DYN-LEAVE_DATE.
**2st Container Number
*  ZTMM_CONTAINER-CONT_REG_NUMB2 = LECI_TRA_DYN-CONT_REG_NUMB2.
**[ 2 ] Time Stmp
*  ZTMM_CONTAINER-AEDAT = SY-DATUM.
*  ZTMM_CONTAINER-AEZET = SY-UZEIT.
*  ZTMM_CONTAINER-AENAM = SY-UNAME.
*  UPDATE ZTMM_CONTAINER FROM ZTMM_CONTAINER.
  DATA : wa_container LIKE ztmm_container.
*MSPARK(2003/11/06)
  CHECK NOT leci_tra_dyn-leave_date IS INITIAL.
*    AND NOT LECI_TRA_DYN-CONT_REG_NUMB2 IS INITIAL.
  CLEAR ztmm_container.

  SELECT SINGLE * FROM ztmm_container
                  INTO wa_container
                 WHERE cont_reg_numb1 = leci_tra_dyn-cont_reg_numb2.
*[ 1 ] Get Change Field.
*Check-in
  wa_container-pass_date      = leci_tra_dyn-pass_date.
*Check-out
  wa_container-leave_date     = leci_tra_dyn-leave_date.
**2st Container Number
  wa_container-returned       = 'X'.
  wa_container-aedat = sy-datum.
  wa_container-aezet = sy-uzeit.
  wa_container-aenam = sy-uname.

  IF leci_tra_dyn-cont_reg_numb2 <> space.
*Container
    DELETE FROM ztmm_container
           WHERE cont_reg_numb1 = wa_container-cont_reg_numb1.
    wa_container-cont_reg_numb1 = leci_tra_dyn-cont_reg_numb2.
    INSERT ztmm_container FROM wa_container.
  ELSE.
*[ 2 ] Time Stmp
    UPDATE ztmm_container FROM wa_container.
  ENDIF.
  .
ENDFORM.                    " CHANGE_ZTMM_CONTAINER_CT_YARD
*&---------------------------------------------------------------------*
*&      Form  FILL_STORAGE_TYPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_storage_type.
  DATA : letyp LIKE lein-letyp.
  CLEAR : it_scrfield, it_scrfield[].
*  SELECT SINGLE LETYP
*                INTO LETYP
*                FROM LEIN
*               WHERE LENUM = IT_STORAGE-
  it_scrfield-fieldname  = 'W_NLTYP'.   "Storge type
  it_scrfield-fieldvalue = it_storage-lgtyp.
  APPEND it_scrfield.

  CLEAR it_scrfield.
  it_scrfield-fieldname  = 'W_NLBER'.   "Storage section
  it_scrfield-fieldvalue = it_storage-lgber.
  APPEND it_scrfield.

  CLEAR it_scrfield.
  it_scrfield-fieldname  = 'W_NLPLA'.   "Storage bin
  it_scrfield-fieldvalue = it_storage-lgpla.
  APPEND it_scrfield.

**--- insert by stlim (2004/04/26)
  CHECK NOT sy-tcode EQ c_tcode.
**--- end of insert

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
       EXPORTING
            dyname               = sy-cprog
            dynumb               = sy-dynnr
       TABLES
            dynpfields           = it_scrfield
       EXCEPTIONS
            invalid_abapworkarea = 1
            invalid_dynprofield  = 2
            invalid_dynproname   = 3
            invalid_dynpronummer = 4
            invalid_request      = 5
            no_fielddescription  = 6
            undefind_error       = 7
            OTHERS               = 8.
  CHECK sy-subrc = 0.
  LOOP AT it_scrfield.
    CASE it_scrfield-fieldname.
      WHEN 'W_NLTYP'. w_nltyp = it_scrfield-fieldvalue.
      WHEN 'W_NLBER'. w_nlber = it_scrfield-fieldvalue.
      WHEN 'w_NLPLA'. w_nlpla = it_scrfield-fieldvalue.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    " FILL_STORAGE_TYPE
*&---------------------------------------------------------------------*
*&      Form  ERROR_MESSAGE_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM error_message_display.
  DATA : txt LIKE t100-text.
  DATA : msgnr(3) TYPE n.

  msgnr = it_message-msgnr.
  CALL FUNCTION 'TB_MESSAGE_BUILD_TEXT'
       EXPORTING
            langu = sy-langu
            msgid = it_message-msgid
            msgno = msgnr
            msgv1 = it_message-msgv1+0(50)
            msgv2 = it_message-msgv2+0(50)
            msgv3 = it_message-msgv3+0(50)
            msgv4 = it_message-msgv4+0(50)
       IMPORTING
            text  = txt.
  IF sy-subrc = 0.
    MESSAGE e009 WITH txt.
  ENDIF.
ENDFORM.                    " ERROR_MESSAGE_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  PRINT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM print.
  PERFORM find.
  PERFORM print_excution.
ENDFORM.                    " PRINT
*&---------------------------------------------------------------------*
*&      Form  PRINT_EXCUTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM print_excution.
  DATA    : date(10).
  CLEAR   : it_bdc, it_message.
  REFRESH : it_bdc, it_message.

  WRITE : leci_tra_dyn-pass_date TO date.
  PERFORM bdc_pass USING:
    'X' 'SAPRLECHKIN' '1000'                 ,
    ' ' 'LECI_SELOPT_DYN-PASS_NUMB' w_pass_numb,
    ' ' 'BDC_OKCODE'  '=SEARCH'              .
  PERFORM bdc_pass USING:
   'X' 'SAPRLECHKIN' '1000'                 ,
   ' ' 'BDC_OKCODE'  '=PRINT'               .

  CALL TRANSACTION 'LECI'
           USING it_bdc
           MODE w_status
           UPDATE'S'
           MESSAGES INTO it_message.
  IF sy-subrc <> 0.
    READ TABLE it_message WITH KEY msgtyp = 'E'.
    IF sy-subrc = 0.
      PERFORM error_message_display.
    ENDIF.
  ELSE.
    w_mode = 'C'.
  ENDIF.
ENDFORM.                    " PRINT_EXCUTION
*&---------------------------------------------------------------------*
*&      Form  GR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gr.
  CLEAR   : it_bdc, it_message.
  REFRESH : it_bdc, it_message.

  PERFORM bdc_pass USING:
        'X' 'SAPMV50A'   '4104'    ,
        ' ' 'BDC_OKCODE' '=WABU_T' .
  CALL TRANSACTION 'VL32N'
              USING it_bdc
              MODE w_status
              UPDATE'S'
              MESSAGES INTO it_message.
  IF sy-subrc <> 0.
    READ TABLE it_message WITH KEY msgtyp = 'E'.
    IF sy-subrc = 0.
      PERFORM error_message_display.
    ELSE.
      w_mode = 'C'.
    ENDIF.
  ENDIF.
  w_mode = 'C'.
ENDFORM.                    " GR
*&---------------------------------------------------------------------*
*&      Form  TEST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM test.
  DATA : traid LIKE likp-traid,
         likp  LIKE likp      ,
         contain(20)          .
  DATA : BEGIN OF it_lips OCCURS 0.
          INCLUDE STRUCTURE lips.
  DATA : END OF it_lips.
  DATA : no TYPE i,
         nlplat LIKE w_nlpla.

* HEADER
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
       EXPORTING
            input  = leci_tra_dyn-cont_reg_numb1
       IMPORTING
            output = contain.

  SELECT SINGLE *
           INTO likp
           FROM likp
          WHERE lfart = 'EL'
            AND traty = '0005'
            AND traid = contain.

* ITEM
  SELECT * FROM lips
           INTO CORRESPONDING FIELDS OF TABLE it_lips
          WHERE vbeln = likp-vbeln.
  CHECK sy-subrc = 0.
  DESCRIBE TABLE it_lips LINES no.

* MVT_ITEM .
  LOOP AT it_lips.
    CLEAR: it_goodsmvt_item, wa_goodsmvt_item.
    wa_goodsmvt_item-material  =
             it_lips-matnr.                    "'85850-3K100'.
    wa_goodsmvt_item-move_type =
             '101'.                                         "'101'.
    wa_goodsmvt_item-vendor    =
             likp-lifnr.  "'0000400016'.
    wa_goodsmvt_item-entry_qnt =
             it_lips-lgmng.                                 "'1000'.
    wa_goodsmvt_item-entry_uom =
             it_lips-meins.                    "'EA'.
    wa_goodsmvt_item-po_number =
             it_lips-vgbel.  "'4200000239'.  (POno)
    wa_goodsmvt_item-po_item   =
             it_lips-vgpos.                                 "'00001'.
    wa_goodsmvt_item-mvt_ind   = 'B'.
    APPEND wa_goodsmvt_item TO it_goodsmvt_item.
  ENDLOOP.

*Header
  wa_goodsmvt_header-pstng_date = sy-datum.
  wa_goodsmvt_header-doc_date   = sy-datum.
  wa_goodsmvt_header-ref_doc_no =
              likp-vbeln.  " '180000335'.
* Make goodsmvt_code
  wa_goodsmvt_code-gm_code = '01'.

* Execute BAPI for Post
  PERFORM bapi_goodsmvt_create TABLES   it_goodsmvt_item
                                        it_bapiret2
                               USING    wa_goodsmvt_header
                                        wa_goodsmvt_code
                               CHANGING wa_goodsmvt_headret.
ENDFORM.                    " TEST
*&---------------------------------------------------------------------*
*&      Form  bapi_goodsmvt_create
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bapi_goodsmvt_create
      TABLES   imt_goodsmvt_item
                   STRUCTURE bapi2017_gm_item_create
               ext_return
                   STRUCTURE bapiret2
      USING    value(im_goodsmvt_header)  LIKE bapi2017_gm_head_01
               value(im_goodsmvt_code)    LIKE bapi2017_gm_code
      CHANGING value(ex_goodsmvt_headret) LIKE bapi2017_gm_head_ret.

  CLEAR: ext_return, ext_return[].
  CLEAR: ex_goodsmvt_headret.

  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
    EXPORTING
      goodsmvt_header             = im_goodsmvt_header
      goodsmvt_code               = im_goodsmvt_code
*    TESTRUN                     = 'X'
    IMPORTING
      goodsmvt_headret            = ex_goodsmvt_headret
*   MATERIALDOCUMENT            =
*   MATDOCUMENTYEAR             =
    TABLES
      goodsmvt_item               = imt_goodsmvt_item
*   GOODSMVT_SERIALNUMBER       =
      return                      = ext_return.

  CLEAR: ext_return.
  READ TABLE ext_return WITH KEY type = 'E'.
  IF sy-subrc = 0.  "Error Occurred !
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
*     EXPORTING
*       WAIT          =
*     IMPORTING
*       RETURN        =    .
  ENDIF.
ENDFORM.                    "bapi_goodsmvt_create
*&---------------------------------------------------------------------*
*&      Form  JOB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM job.
  DATA : jobname LIKE tbtcjob-jobname  ,
         jobcnt  LIKE tbtcjob-jobcount ,
         contain(20)                   .

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
       EXPORTING
            input  = leci_tra_dyn-cont_reg_numb1
       IMPORTING
            output = contain.

  jobname = contain.
*[ 1 ] Job open.
*  PERFORM job_open USING   jobname
*                   CHANGING jobcnt.
*
*  EXPORT : LECI_TRA_DYN TO MEMORY ID 'LECI_TRA_DYN',
*           W_NLTYP      TO MEMORY ID 'W_NLTYP'     ,
*           W_NLBER      TO MEMORY ID 'W_NLBER'     ,
*           W_NLPLA      TO MEMORY ID 'W_NLPLA'     .

*[ 2 ] Processing Welcome
  DATA : prgid LIKE sy-repid VALUE 'ZEMMPM23E_WELCOME'.
  SUBMIT (prgid) WITH p_numb1  = leci_tra_dyn-cont_reg_numb1
                 WITH p_vhe    = leci_tra_dyn-vehicle_reg_numb
                 WITH p_pdate  = leci_tra_dyn-pass_date
                 WITH p_ptime  = leci_tra_dyn-pass_time
                 WITH p_ndrv   = leci_tra_dyn-name_drvr
                 WITH p_whgate = leci_tra_dyn-whs_gate
                 WITH p_ptxt   = leci_tra_dyn-parking_txt
                 WITH p_nltyp  = w_nltyp
                 WITH p_nlber  = w_nlber
                 WITH p_nlpla  = w_nlpla
**--- insert by stlim (2004/04/26)
                 WITH p_date   = w_button_click_date
                 WITH p_time   = w_button_click_time
**--- end of insert
*                 USER sy-uname
*                  VIA JOB jobname
*                 number jobcnt
                  AND RETURN.

*[ 3 ] Job Close.
*  PERFORM job_close USING jobname
*                           jobcnt.

ENDFORM.                    " JOB
*&---------------------------------------------------------------------*
*&      Form  JOB_OPEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_JOBNAME  text
*      <--P_JOBCNT  text
*----------------------------------------------------------------------*
FORM job_open USING    p_jobname
              CHANGING p_jobcnt.
  CALL FUNCTION 'JOB_OPEN'
       EXPORTING
            jobname          = p_jobname
       IMPORTING
            jobcount         = p_jobcnt
       EXCEPTIONS
            cant_create_job  = 1
            invalid_job_data = 2
            jobname_missing  = 3
            OTHERS           = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " JOB_OPEN
*&---------------------------------------------------------------------*
*&      Form  JOB_CLOSE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_JOBNAME  text
*      -->P_JOBCNT  text
*----------------------------------------------------------------------*
FORM job_close USING    p_jobname
                        p_jobcnt.
  CALL FUNCTION 'JOB_CLOSE'
       EXPORTING
            jobcount  = p_jobcnt
            jobname   = p_jobname
            strtimmed = 'X'
       EXCEPTIONS
            OTHERS    = 4.

ENDFORM.                    " JOB_CLOSE
*&---------------------------------------------------------------------*
*&      Form  LOG_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM log_display.
  DATA : BEGIN OF sel OCCURS 10,
            sign(1),
            option(2),
            low  LIKE ztmm_ct_errlog-zcontainer,
            high LIKE ztmm_ct_errlog-zcontainer,
         END   OF sel.
  DATA : contain(20)                   .

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
       EXPORTING
            input  = leci_tra_dyn-cont_reg_numb1
       IMPORTING
            output = contain.

  sel-sign  = 'I' .
  sel-option = 'EQ'.
  sel-low    = contain.
  APPEND sel.

  DATA : prgid LIKE sy-repid VALUE 'ZEMMPM23E_ERRORLOG'.
  SUBMIT (prgid) WITH p_cntain  IN sel
                 AND RETURN.


ENDFORM.                    " LOG_DISPLAY
