************************************************************************
* Program Name      : SAPMZAPM11_ORDER
* Author            : MYOUNGHO PARK
* Creation Date     : 2003.10.31.
* Specifications By :
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       : BDC Program that allows batch processing of
* Emergency maintenance work on Order creation and goods issue
* processing* in one transaction. This can be linked with external
* systems(such as Barcode machines), to input data into a interface..
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
* IMPORTANT NOTE: CURRENTLY THE PROGRAM DOES NOT ALLOW TO ENTER
* FUNCTION LOCATION. TO MAKE FUNCTION LOCATION INPUT FIELD JUST
* CHANGE THE SCREEN ATTRIBUTES BY GOING TO THE SCREEN LAYOUT.
* CODE ALREADY EXISTS TO HANDLE EVERYTHING ELSE.

*&---------------------------------------------------------------------*
*& Report  SAPMZAPM11_ORDER                                            *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  sapmzapm11_order              .

TYPE-POOLS cxtab .  "//Table_control Object type pool

FIELD-SYMBOLS: <tc>  TYPE cxtab_control.  "for table control

TABLES: zspm_bako,  "//Barcode Header
        zspm_bapo,  "//Barcode Item
        t001w,      "//Plants/Branches
        eqkt,       "//Equipment Short Texts
        iflotx,     "//Functional Location: Short Texts
        mkpf,       "//Header: Material Document
        mseg,        "//Document Segment: Material
        vm07m,
        dm07m.

*** items
DATA: it_bapo LIKE zspm_bapo OCCURS 0 WITH HEADER LINE.

*** PM component
DATA: it_zspm_resbd LIKE zspm_resbd OCCURS 0 WITH HEADER LINE.

*** BDC messages
DATA: BEGIN OF it_message OCCURS 0,
        type    LIKE bapiret2-type,
        id      LIKE bapiret2-id,
        number  LIKE bapiret2-number,
        message LIKE bapiret2-message,
      END OF it_message.

**** Global Variables
DATA : wa_mode VALUE 'N'.
DATA : wa_tcname LIKE feld-name. "table control name
DATA : wa_answer.
DATA : wa_save.
DATA : wa_sel.
DATA : wa_subrc LIKE sy-subrc.
DATA : wa_orderid TYPE aufnr.
DATA : wa_no_item.

**** Table Control...
CONTROLS: tc_0100 TYPE TABLEVIEW USING SCREEN 0100.

DATA:     g_tc_0100_lines  LIKE sy-loopc.

** For ALV
TYPE-POOLS: slis.

DATA : gv_repid LIKE sy-repid.
DATA : gv_status       TYPE slis_formname VALUE 'PF_STATUS'.
DATA : gv_user_command TYPE slis_formname VALUE 'USER_COMMAND'.
DATA : it_sort         TYPE slis_t_sortinfo_alv WITH HEADER LINE .
DATA : gv_col_pos TYPE i.

DATA : it_fieldcat          TYPE slis_t_fieldcat_alv,
       wa_fieldcat          LIKE LINE OF it_fieldcat,
       it_eventcat          TYPE slis_t_event,
       wa_eventcat          LIKE LINE OF it_eventcat.

DATA : it_events            TYPE slis_t_event,
       it_event_exit      TYPE slis_t_event_exit.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  IF wa_save EQ 'X'.
    SET PF-STATUS '0100' EXCLUDING 'CREA'.
    SET TITLEBAR  '0100'.
  ELSE.
    SET PF-STATUS '0100'.
    SET TITLEBAR  '0100'.
  ENDIF.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  INITIAL_VALUE  OUTPUT
*&---------------------------------------------------------------------*
*       Initialize Values
*----------------------------------------------------------------------*
MODULE initial_value OUTPUT.
  PERFORM inital_vlaue.
ENDMODULE.                 " INITIAL_VALUE  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CHECK_BUDAT
*&---------------------------------------------------------------------*
*       This Part is copied from standard program...
*
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_budat.
  mkpf-bldat  = zspm_bako-budat.
  mkpf-budat  = zspm_bako-budat.

  IF mkpf-bldat > sy-datlo.
    PERFORM nachrichtencode_ermitteln(sapfm07m) USING 'M7' '088'. "ok
  ENDIF.

  mseg-gjahr = sy-datum.
  mseg-bukrs = zspm_bako-bukrs.

  DATA : monat LIKE vm07m-monat.
  DATA : gjahr LIKE mseg-gjahr.

  CALL FUNCTION 'FI_PERIOD_DETERMINE'
    EXPORTING
      i_bukrs = mseg-bukrs
      i_budat = mkpf-bldat
    IMPORTING
      e_monat = monat
      e_gjahr = gjahr.
  IF NOT mseg-gjahr = gjahr.
    PERFORM nachrichtencode_ermitteln(sapfm07m) USING 'M7' '089'."ok
  ENDIF.

ENDFORM.                    " CHECK_BUDAT
*&---------------------------------------------------------------------*
*&      Module  TABLE_CONTROL_OUTPUT_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE table_control_output_0100 OUTPUT.
  CONCATENATE 'TC_' sy-dynnr INTO wa_tcname.
  ASSIGN (wa_tcname) TO <tc>.

  READ TABLE it_bapo INDEX <tc>-current_line .
  IF sy-subrc EQ 0.
    it_bapo-posnr = <tc>-current_line .
    MOVE-CORRESPONDING it_bapo TO zspm_bapo.
  ELSE.
    CLEAR zspm_bapo.
  ENDIF.
ENDMODULE.                 " TABLE_CONTROL_OUTPUT_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_TABLE_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_screen_table_0100 OUTPUT.

ENDMODULE.                 " MODIFY_SCREEN_TABLE_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TABLE_CONTROL_LINES_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE table_control_lines_0100 OUTPUT.
*--- Internal Table Lines Number to Table Control Lines Number.
  CONCATENATE 'TC_' sy-dynnr INTO wa_tcname.
  ASSIGN (wa_tcname) TO <tc>.

  DESCRIBE TABLE it_bapo LINES <tc>-lines.
ENDMODULE.                 " TABLE_CONTROL_LINES_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TABLE_CONTROL_INPUT_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE table_control_input_0100 INPUT.
  CONCATENATE 'TC_' sy-dynnr INTO wa_tcname.
  ASSIGN (wa_tcname) TO <tc>.

  READ TABLE it_bapo INDEX <tc>-current_line.
  IF sy-subrc NE 0.
    MOVE <tc>-current_line TO zspm_bapo-posnr.
    MOVE-CORRESPONDING zspm_bapo TO it_bapo.
    APPEND it_bapo.
  ELSE.
    MOVE-CORRESPONDING zspm_bapo TO it_bapo.

    MODIFY it_bapo INDEX <tc>-current_line TRANSPORTING
                                                    posnr
                                                    matnr
                                                    maktx
                                                    menge
                                                    meins
                                                    werks
                                                    lgort
                                                    check.
  ENDIF.
ENDMODULE.                 " TABLE_CONTROL_INPUT_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_BUKRS_INFO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_bukrs_info INPUT.
  IF NOT zspm_bako-bukrs IS INITIAL.
    PERFORM get_bukrs_desc.
  ENDIF.
ENDMODULE.                 " CHECK_BUKRS_INFO  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_TPLNR_INFO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_tplnr_info INPUT.
* IMPORTANT NOTE: CURRENTLY THE PROGRAM DOES NOT ALLOW TO ENTER
* FUNCTION LOCATION. TO MAKE FUNCTION LOCATION INPUT FIELD JUST
* CHANGE THE SCREEN ATTRIBUTES BY GOING TO THE SCREEN LAYOUT.
* CODE ALREADY EXISTS TO HANDLE EVERYTHING ELSE.

  IF NOT zspm_bako-tplnr IS INITIAL.
    PERFORM get_tplnr_desc.
***Modification by 100565 TO DISPLAY FN LOC
*ELSEIF ZSPM_BAKO-TPLNR IS INITIAL.
* SELECT SINGLE TPLNR  FROM V_EQUI INTO ZSPM_BAKO-TPLNR WHERE EQUNR =
*ZSPM_BAKO-EQUNR.
*PERFORM GET_TPLNR_DESC.


  ENDIF.
ENDMODULE.                 " CHECK_TPLNR_INFO  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_EQUIP_INFO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_equip_info INPUT.
  IF NOT zspm_bako-equnr IS INITIAL.
    PERFORM get_equip_desc.
  ENDIF.
ENDMODULE.                 " CHECK_EQUIP_INFO  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_PLANT_INFO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_plant_info INPUT.
  IF NOT zspm_bako-werks IS INITIAL.
    PERFORM get_plant_desc.
  ENDIF.
ENDMODULE.                 " CHECK_PLANT_INFO  INPUT
*&---------------------------------------------------------------------*
*&      Form  GET_TPLNR_DESC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_tplnr_desc.
  CHECK zspm_bako-tplnr NE space.
  SELECT SINGLE pltxt iwerk ingrp
         INTO (zspm_bako-pltxt, zspm_bako-werks, zspm_bako-ingrp)
                FROM  iflo
                WHERE tplnr = zspm_bako-tplnr
                AND   spras = sy-langu
                AND   lvorm = space.
  IF sy-subrc NE 0.
    MESSAGE e000(zmpm) WITH text-001 zspm_bako-tplnr text-m01.
  ENDIF.
ENDFORM.                    " GET_TPLNR_DESC
*&---------------------------------------------------------------------*
*&      Form  GET_EQUIP_DESC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_equip_desc.
  CHECK zspm_bako-equnr NE space.

  SELECT SINGLE eqktx INTO zspm_bako-eqktx
                FROM  eqkt
                WHERE equnr = zspm_bako-equnr
                AND   spras = sy-langu.
  IF sy-subrc NE 0.
    MESSAGE e000(zmpm) WITH text-002 zspm_bako-equnr text-m01.
  ELSE.
    SELECT SINGLE iwerk ingrp
           INTO (zspm_bako-werks, zspm_bako-ingrp)
                FROM  itob
                WHERE equnr = zspm_bako-equnr
                AND   spras = sy-langu
                AND   lvorm = space.
  ENDIF.
ENDFORM.                    " GET_EQUIP_DESC
*&---------------------------------------------------------------------*
*&      Form  GET_PLANT_DESC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_plant_desc.
  SELECT SINGLE name1 INTO zspm_bako-name1
                FROM  t001w
                WHERE werks = zspm_bako-werks
                AND   spras = sy-langu.
  IF sy-subrc NE 0.
    MESSAGE e000(zmpm) WITH text-003 zspm_bako-werks text-m01.
  ENDIF.
ENDFORM.                    " GET_PLANT_DESC
*&---------------------------------------------------------------------*
*&      Module  check_posting_date  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_posting_date INPUT.
**** verify Posting Period.....
  IF NOT zspm_bako-werks IS INITIAL.
    PERFORM check_budat.
  ENDIF.
ENDMODULE.                 " check_posting_date  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command INPUT.

ENDMODULE.                 " USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*&      Form  GET_BUKRS_DESC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_bukrs_desc.
  SELECT SINGLE butxt INTO zspm_bako-butxt
         FROM t001
         WHERE bukrs = zspm_bako-bukrs.
  IF sy-subrc NE 0.
    MESSAGE e000(zmpm) WITH text-004 zspm_bako-bukrs text-m01.
  ENDIF.
ENDFORM.                    " GET_BUKRS_DESC
*&---------------------------------------------------------------------*
*&      Module  TC_0100_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tc_0100_user_command INPUT.
  PERFORM user_ok_tc USING    'TC_0100'
                              'IT_BAPO'
                              'CHECK'
                     CHANGING sy-ucomm.
ENDMODULE.                 " TC_0100_USER_COMMAND  INPUT

INCLUDE mzapm11_orderf01.
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
*--- FUNCTION  TYPE E.
  CASE sy-ucomm.
    WHEN 'BACK'.
      CLEAR: sy-ucomm.
      PERFORM leave_program.

    WHEN 'RW'.
      CLEAR: sy-ucomm.
      PERFORM leave_program.

    WHEN '%EX'.
      CLEAR: sy-ucomm.
      PERFORM leave_program.

    WHEN OTHERS.
      CLEAR: sy-ucomm.
  ENDCASE.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Form  LEAVE_PROGRAM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM leave_program.
  IF sy-dynnr = '0100'.
    PERFORM confirm_next_step USING  text-m02
                                     text-m03 .
    IF  wa_answer NE 'J'.
*      STOP.
      EXIT.
    ENDIF.
  ENDIF.
  LEAVE TO SCREEN 0.
ENDFORM.                    " LEAVE_PROGRAM
*&---------------------------------------------------------------------*
*&      Form  CONFIRM_NEXT_STEP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXT_T01  text
*      -->P_TEXT_C01  text
*----------------------------------------------------------------------*
FORM confirm_next_step USING    p_title
                                p_text.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      defaultoption  = 'Y'
      textline1      = p_text
      titel          = p_title
      cancel_display = 'X'
    IMPORTING
      answer         = wa_answer.
ENDFORM.                    " CONFIRM_NEXT_STEP
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.

    WHEN 'CLEAR'.
      CLEAR: sy-ucomm.
      PERFORM refresh_input_fields.

    WHEN 'CREA'.
      CLEAR: sy-ucomm, wa_no_item.
      CLEAR: it_message, it_message[].

      IF it_bapo IS INITIAL.
        IF sy-dynnr = '0100'.
          PERFORM confirm_next_step USING  text-m08
                                           text-m09 .
          IF  wa_answer NE 'J'.
            EXIT.
          ELSE.
            wa_no_item = 'X'.
          ENDIF.
        ENDIF.
      ENDIF.

      PERFORM create_order.

      IF wa_no_item EQ 'X'.
        wa_save = 'X'.
        zspm_bako-aufnr = wa_orderid.
      ELSE.
        IF wa_subrc EQ 0.
*        PERFORM GOODS_MOVEMENT.
***  Goods movement ( MB11 )
          PERFORM goods_movement2.
          IF wa_subrc NE 0.
*** Delete new Order
            PERFORM complete_order USING wa_orderid.
            PERFORM delete_order USING wa_orderid.
            MESSAGE e000(zmpm) WITH text-m07.
          ELSE.
            wa_save = 'X'.
            zspm_bako-aufnr = wa_orderid.
          ENDIF.

        ENDIF.
      ENDIF.

    WHEN 'LOG'.
      CLEAR: sy-ucomm.
      PERFORM display_message.

**** ADD 29/04/2004
    WHEN 'DISP'.
      CLEAR : sy-ucomm.
      PERFORM maintain_order_description.
* Modification by 100565 01/24/06
    WHEN 'COMP'.
      CLEAR : sy-ucomm.
      IF zspm_bako-aufnr <> space.
        SET PARAMETER ID 'ANR' FIELD zspm_bako-aufnr.
        CALL TRANSACTION 'ZPMA01' AND SKIP FIRST SCREEN.
      ELSE.
        MESSAGE e000(zmpm) WITH text-m11.
      ENDIF.
* End Modification by 100565 01/24/06
    WHEN OTHERS.
      CLEAR: sy-ucomm.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  CREATE_ORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_order.
  DATA: it_messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
  DATA: wa_text(200).
  DATA: wa_msgnr LIKE sy-msgno,
        wa_msgid LIKE sy-msgid,
        wa_msgv1 LIKE sy-msgv1,
        wa_msgv2 LIKE sy-msgv2,
        wa_msgv3 LIKE sy-msgv3,
        wa_msgv4 LIKE sy-msgv4.

  CLEAR: it_zspm_resbd, it_zspm_resbd[], wa_orderid.


  LOOP AT it_bapo.
    CLEAR: it_zspm_resbd.
    MOVE-CORRESPONDING it_bapo TO it_zspm_resbd.
*    MOVE : ZSPM_BAKO-WERKS TO IT_ZSPM_RESBD-WERKS.
    APPEND it_zspm_resbd.
  ENDLOOP.

  CALL FUNCTION 'Z_FPM_CREATE_EMERGENCCY_ORDER'
    EXPORTING
      ctu       = 'X'
      mode      = wa_mode
      update    = 'L'
*     GROUP     =
*     USER      =
*     KEEP      =
*     HOLDDATE  =
*     NODATA    =
      pm_aufart = 'PM02'
      priok     = '1'
      tplnr     = zspm_bako-tplnr
      equnr     = zspm_bako-equnr
      ktext     = zspm_bako-ktext
      gstrp     = zspm_bako-budat
      atp_check = ' '
    IMPORTING
      subrc     = wa_subrc
    TABLES
      resbd     = it_zspm_resbd
      messtab   = it_messtab.

*  READ TABLE IT_MESSTAB   WITH KEY MSGTYP = 'S'
*                                  MSGID  = '00'
*                                  MSGNR  = '344'.
*  IF SY-SUBRC EQ 0.
*    CLEAR : IT_MESSTAB, IT_MESSTAB[].
*    CALL FUNCTION 'Z_FPM_CREATE_EMERGENCCY_ORDER'
*         EXPORTING
*              CTU       = 'X'
*              MODE      = WA_MODE
*              UPDATE    = 'L'
*              PM_AUFART = 'PM02'
*              PRIOK     = '1'
*              TPLNR     = ZSPM_BAKO-TPLNR
*              EQUNR     = ZSPM_BAKO-EQUNR
*              KTEXT     = ZSPM_BAKO-KTEXT
*              GSTRP     = ZSPM_BAKO-BUDAT
*              ATP_CHECK = 'X'
*         IMPORTING
*              SUBRC     = WA_SUBRC
*         TABLES
*              RESBD     = IT_ZSPM_RESBD
*              MESSTAB   = IT_MESSTAB.
*
*  ENDIF.
*
  LOOP AT it_messtab.
    CLEAR: wa_text.
    MOVE:   it_messtab-msgnr TO wa_msgnr,
            it_messtab-msgid TO wa_msgid,
            it_messtab-msgv1 TO wa_msgv1,
            it_messtab-msgv2 TO wa_msgv2,
            it_messtab-msgv3 TO wa_msgv3,
            it_messtab-msgv4 TO wa_msgv4.

    CALL FUNCTION 'TB_MESSAGE_BUILD_TEXT'
      EXPORTING
        langu = sy-langu
        msgid = wa_msgid
        msgno = wa_msgnr
        msgv1 = wa_msgv1
        msgv2 = wa_msgv2
        msgv3 = wa_msgv3
        msgv4 = wa_msgv4
      IMPORTING
        text  = wa_text.
    CLEAR:  it_message.
    MOVE : wa_text TO it_message-message,
           it_messtab-msgtyp TO it_message-type,
           it_messtab-msgid  TO it_message-id,
           it_messtab-msgnr  TO it_message-number.
    APPEND it_message.
  ENDLOOP.
  READ TABLE it_messtab WITH KEY msgtyp = 'S'
                                 msgid  = 'IW'
                                 msgnr  = '085'.
  wa_subrc = sy-subrc.
  IF sy-subrc EQ 0.
    COMMIT WORK.
    wa_orderid = it_messtab-msgv1.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_orderid
      IMPORTING
        output = wa_orderid.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 100
        text       = text-m05.
  ENDIF.
ENDFORM.                    " CREATE_ORDER
*&---------------------------------------------------------------------*
*&      Form  GOODS_MOVEMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM goods_movement.
  DATA: wa_message    LIKE bapiret2.
  DATA: wa_document   LIKE bapi2017_gm_head_ret-mat_doc.

**** Material Document Header Data
  DATA: wa_goodsmvt_header LIKE bapi2017_gm_head_01.

**** Material Document Items
  DATA: it_goodsmvt_item LIKE bapi2017_gm_item_create
                           OCCURS 0 WITH HEADER LINE.

**** Return Messages
  DATA: it_return LIKE bapiret2 OCCURS 0 WITH HEADER LINE.

**** Key Assignment GM_CODE to Transaction
  DATA: wa_gm_code LIKE bapi2017_gm_code.
  wa_gm_code-gm_code = '06'.   "//T-CODE : MB11

**** ASSIGN header value...
  wa_goodsmvt_header-pstng_date = zspm_bako-budat.
  wa_goodsmvt_header-doc_date = zspm_bako-budat.

**** Assign item value....
  LOOP AT it_bapo.
    CLEAR: it_goodsmvt_item.
    it_goodsmvt_item-material  = it_bapo-matnr.
*    IT_GOODSMVT_ITEM-ACTIVITY =  '0010'.
    it_goodsmvt_item-plant     = zspm_bako-werks.
    it_goodsmvt_item-stge_loc  = it_bapo-lgort.
    it_goodsmvt_item-move_type = '261'.
    it_goodsmvt_item-entry_qnt = it_bapo-menge.
    it_goodsmvt_item-entry_uom = it_bapo-meins.
*    IT_GOODSMVT_ITEM-WITHDRAWN = 'X'.
    it_goodsmvt_item-orderid    = wa_orderid.
    it_goodsmvt_item-order_itno = it_bapo-posnr.
    APPEND it_goodsmvt_item.
  ENDLOOP.



  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
    EXPORTING
      goodsmvt_header       = wa_goodsmvt_header
      goodsmvt_code         = wa_gm_code
*     TESTRUN               = ' '
    IMPORTING
*     GOODSMVT_HEADRET      =
      materialdocument      = wa_document
*     MATDOCUMENTYEAR       =
    TABLES
      goodsmvt_item         = it_goodsmvt_item
*     GOODSMVT_SERIALNUMBER =
      return                = it_return.
  LOOP AT  it_return  .
    CALL FUNCTION 'BALW_BAPIRETURN_GET2'
      EXPORTING
        type   = it_return-type
        cl     = it_return-id
        number = it_return-number
        par1   = it_return-message_v1
        par2   = it_return-message_v2
        par3   = it_return-message_v3
        par4   = it_return-message_v4
      IMPORTING
        return = wa_message.
    CLEAR:  it_message.
    MOVE-CORRESPONDING wa_message TO it_message.
    APPEND it_message.
  ENDLOOP.

  IF NOT wa_document IS INITIAL.
    CLEAR: it_message.
    CONCATENATE text-005 wa_document text-006 INTO it_message-message.
    MOVE  'S'   TO it_message-type .
    APPEND it_message.
  ENDIF.

ENDFORM.                    " GOODS_MOVEMENT
*&---------------------------------------------------------------------*
*&      Module  CHECK_MATNR_INFO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_matnr_info INPUT.
  IF NOT zspm_bapo-matnr IS INITIAL.
    SELECT SINGLE maktx INTO zspm_bapo-maktx
           FROM makt
           WHERE matnr = zspm_bapo-matnr
           AND   spras = sy-langu.

    SELECT SINGLE meins INTO zspm_bapo-meins
           FROM mara
           WHERE  matnr = zspm_bapo-matnr.
  ENDIF.
ENDMODULE.                 " CHECK_MATNR_INFO  INPUT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_message.
  CALL SCREEN 0110.

ENDFORM.                    " DISPLAY_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  PRE_REPORT_ADJ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pre_report_adj.
* Building Field Cat.
  CLEAR : gv_col_pos, it_fieldcat, it_fieldcat[].

* Key
  PERFORM build_fieldcat USING
    'IT_MESSAGE' 'TYPE'  'X'     space space
     space    '1'     'Type'  space space space.

  PERFORM build_fieldcat USING
    'IT_MESSAGE' 'ID'  'X'     space space
     space    '20'     'Message ID'  space space space.

  PERFORM build_fieldcat USING
    'IT_MESSAGE' 'NUMBER'  'X'     space space
     space    '3'     'Message Number'  space space space.

  PERFORM build_fieldcat USING
    'IT_MESSAGE' 'MESSAGE'  'X'     space space
     space    '100'     'Message'  space space space.

*** Sort
  SORT it_message BY type id number message.
  CLEAR: it_message.

  it_sort-fieldname = 'TYPE'.
  it_sort-up        = 'X'.
  it_sort-expa      = 'X'.
  it_sort-subtot    = 'X'.
  APPEND it_sort.

  it_sort-fieldname = 'ID'.
  it_sort-up        = 'X'.
  it_sort-expa      = 'X'.
  it_sort-subtot    = 'X'.
  APPEND it_sort.

  it_sort-fieldname = 'NUMBER'.
  it_sort-up        = 'X'.
  it_sort-expa      = 'X'.
  it_sort-subtot    = 'X'.
  APPEND it_sort.

  it_sort-fieldname = 'MESSAGE'.
  it_sort-up        = 'X'.
  it_sort-expa      = 'X'.
  it_sort-subtot    = 'X'.
  APPEND it_sort.

* Set Event
  DATA : wa_l_event  TYPE slis_alv_event.
  wa_l_event-name = slis_ev_top_of_page.
  wa_l_event-form = 'BASIC_TOP_OF_PAGE'.
  APPEND wa_l_event TO it_events.

ENDFORM.                    " PRE_REPORT_ADJ
*&---------------------------------------------------------------------*
*&      Form  CALL_ALV_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_alv_list.
  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
    EXPORTING
      i_callback_program       = gv_repid
      i_callback_pf_status_set = gv_status
      i_callback_user_command  = gv_user_command
      it_fieldcat              = it_fieldcat[]
      it_sort                  = it_sort[]
      i_save                   = 'A'
      it_events                = it_events
      it_event_exit            = it_event_exit  "
    TABLES
      t_outtab                 = it_message
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " CALL_ALV_LIST
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1197   text
*      -->P_1198   text
*      -->P_1199   text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_1203   text
*      -->P_1204   text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*----------------------------------------------------------------------*
FORM build_fieldcat USING    value(p_0100)
                             value(p_0101)
                             value(p_0102)
                             value(p_0103)
                             value(p_0104)
                             value(p_0105)
                             value(p_0106)
                             value(p_0107)
                             value(p_0108)
                             value(p_0109)
                             value(p_0110).

  ADD 1 TO gv_col_pos.
  wa_fieldcat-tabname     = p_0100.
  wa_fieldcat-fieldname   = p_0101.
  wa_fieldcat-key         = p_0102.
  wa_fieldcat-do_sum      = p_0103.
  wa_fieldcat-cfieldname  = p_0104.
  wa_fieldcat-ctabname    = p_0105.
  wa_fieldcat-outputlen   = p_0106.
  wa_fieldcat-seltext_l   = p_0107.
  wa_fieldcat-datatype    = p_0108.
  wa_fieldcat-qfieldname  = p_0109.
  wa_fieldcat-qtabname    = p_0110.
  wa_fieldcat-col_pos     = gv_col_pos.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

ENDFORM.                    " BUILD_FIELDCAT
*---------------------------------------------------------------------*
*       FORM BASIC_TOP_OF_PAGE                                        *
*---------------------------------------------------------------------*
*       Display Title                                                 *
*---------------------------------------------------------------------*
FORM basic_top_of_page.

  WRITE  AT 20 'Resault of Emergency Order Creation'
         INVERSE COLOR 3.
  SKIP.
  WRITE : / 'Plant               : ', zspm_bako-werks, '   '
                                    , zspm_bako-name1.
  WRITE : / 'Functional Location : ', zspm_bako-tplnr, '   '
                                    , zspm_bako-pltxt.
  WRITE : / 'Equipment           : ', zspm_bako-equnr, '   '
                                    , zspm_bako-eqktx.
  WRITE : / 'Posting date        : ', zspm_bako-budat.
  WRITE : / 'Date                : ', sy-datum.
  SKIP.
ENDFORM.                    "BASIC_TOP_OF_PAGE
*-----------------------------------------------------------------------
*    FORM PF_STATUS_VAR
*-----------------------------------------------------------------------
FORM pf_status USING  extab TYPE slis_t_extab.
  SET PF-STATUS 'BALVLIST'  EXCLUDING extab. " OF PROGRAM 'ZAPM08_ANBD'.
ENDFORM.                    "PF_STATUS
*&---------------------------------------------------------------------*
*&      Module  STATUS_0110  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0110 OUTPUT.
  SUPPRESS DIALOG.
  LEAVE TO LIST-PROCESSING AND RETURN TO SCREEN 0.
  SET TITLEBAR '110'.
  SET PF-STATUS space.

  gv_repid = sy-repid.

* Preparation of ALV
  PERFORM pre_report_adj.

* Call ALV LIST
  PERFORM call_alv_list.
ENDMODULE.                 " STATUS_0110  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  GOODS_MOVEMENT2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM goods_movement2.
  DATA: it_messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
  DATA: wa_text(200).
  DATA: wa_msgnr LIKE sy-msgno,
        wa_msgid LIKE sy-msgid,
        wa_msgv1 LIKE sy-msgv1,
        wa_msgv2 LIKE sy-msgv2,
        wa_msgv3 LIKE sy-msgv3,
        wa_msgv4 LIKE sy-msgv4.

  CALL FUNCTION 'Z_FPM_GOODS_MOVEMENT'
    EXPORTING
      ctu      = 'X'
      mode     = wa_mode
      update   = 'L'
*     GROUP    =
*     USER     =
*     KEEP     =
*     HOLDDATE =
*     NODATA   = '/'
      bldat    = zspm_bako-budat
      budat    = zspm_bako-budat
      xfull    = ' '
      aufnr    = wa_orderid
      wempf    = zspm_bako-wempf    "/ 05/17/2004 - sllee
    IMPORTING
      subrc    = wa_subrc
    TABLES
      messtab  = it_messtab.

  LOOP AT it_messtab.
    CLEAR: wa_text.
    MOVE:   it_messtab-msgnr TO wa_msgnr,
            it_messtab-msgid TO wa_msgid,
            it_messtab-msgv1 TO wa_msgv1,
            it_messtab-msgv2 TO wa_msgv2,
            it_messtab-msgv3 TO wa_msgv3,
            it_messtab-msgv4 TO wa_msgv4.

    CALL FUNCTION 'TB_MESSAGE_BUILD_TEXT'
      EXPORTING
        langu = sy-langu
        msgid = wa_msgid
        msgno = wa_msgnr
        msgv1 = wa_msgv1
        msgv2 = wa_msgv2
        msgv3 = wa_msgv3
        msgv4 = wa_msgv4
      IMPORTING
        text  = wa_text.
    CLEAR:  it_message.
    MOVE : wa_text TO it_message-message,
           it_messtab-msgtyp TO it_message-type,
           it_messtab-msgid  TO it_message-id,
           it_messtab-msgnr  TO it_message-number.
    APPEND it_message.
  ENDLOOP.
  READ TABLE it_messtab WITH KEY msgtyp = 'S'
                                 msgid  = 'M7'
                                 msgnr  = '060'.
  wa_subrc = sy-subrc.
  IF sy-subrc EQ 0.
    COMMIT WORK.
  ENDIF.
  .

ENDFORM.                    " GOODS_MOVEMENT2
*&---------------------------------------------------------------------*
*&      Form  REFRESH_INPUT_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh_input_fields.
  CLEAR : wa_subrc, wa_save.
  CLEAR : zspm_bako.
  CLEAR : it_bapo, it_bapo[],
          it_zspm_resbd, it_zspm_resbd[],
          it_message, it_message[].

  PERFORM inital_vlaue.
ENDFORM.                    " REFRESH_INPUT_FIELDS
*&---------------------------------------------------------------------*
*&      Form  INITAL_VLAUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM inital_vlaue.
*****Clear Fn location
  IF zspm_bako-equnr IS INITIAL.
    zspm_bako-tplnr = space.
  ENDIF.

**** set Company code value....
  IF zspm_bako-bukrs IS INITIAL.
    zspm_bako-bukrs = 'H201'.
  ENDIF.

**** set  Plant initial value....
*  IF ZSPM_BAKO-WERKS IS INITIAL.
*    ZSPM_BAKO-WERKS = 'P001'.
*  ENDIF.

**** set  Posting date initial value....
  IF zspm_bako-budat IS INITIAL.
    zspm_bako-budat = sy-datum.
  ENDIF.

**** set  Posting date initial value....
  IF zspm_bako-wempf IS INITIAL.
    zspm_bako-wempf = sy-uname.


  ENDIF.


ENDFORM.                    " INITAL_VLAUE
*&---------------------------------------------------------------------*
*&      Form  DELETE_ORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_ORDERID  text
*----------------------------------------------------------------------*
FORM delete_order USING    p_orderid.
  DATA: it_messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
  DATA: wa_text(200).
  DATA: wa_msgnr LIKE sy-msgno,
        wa_msgid LIKE sy-msgid,
        wa_msgv1 LIKE sy-msgv1,
        wa_msgv2 LIKE sy-msgv2,
        wa_msgv3 LIKE sy-msgv3,
        wa_msgv4 LIKE sy-msgv4.

  CALL FUNCTION 'Z_FPM_DELETE_ORDER'
    EXPORTING
      ctu     = 'X'
      mode    = 'N'
      update  = 'L'
      aufnr   = p_orderid
    IMPORTING
      subrc   = wa_subrc
    TABLES
      messtab = it_messtab.
  LOOP AT it_messtab.
    CLEAR: wa_text.
    MOVE:   it_messtab-msgnr TO wa_msgnr,
            it_messtab-msgid TO wa_msgid,
            it_messtab-msgv1 TO wa_msgv1,
            it_messtab-msgv2 TO wa_msgv2,
            it_messtab-msgv3 TO wa_msgv3,
            it_messtab-msgv4 TO wa_msgv4.

    CALL FUNCTION 'TB_MESSAGE_BUILD_TEXT'
      EXPORTING
        langu = sy-langu
        msgid = wa_msgid
        msgno = wa_msgnr
        msgv1 = wa_msgv1
        msgv2 = wa_msgv2
        msgv3 = wa_msgv3
        msgv4 = wa_msgv4
      IMPORTING
        text  = wa_text.
    CLEAR:  it_message.
    MOVE : wa_text TO it_message-message,
           it_messtab-msgtyp TO it_message-type,
           it_messtab-msgid  TO it_message-id,
           it_messtab-msgnr  TO it_message-number.
    APPEND it_message.
  ENDLOOP.

ENDFORM.                    " DELETE_ORDER
*&---------------------------------------------------------------------*
*&      Form  COMPLETE_ORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_ORDERID  text
*----------------------------------------------------------------------*
FORM complete_order USING    p_orderid.
  DATA: it_messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
  DATA: wa_text(200).
  DATA: wa_msgnr LIKE sy-msgno,
        wa_msgid LIKE sy-msgid,
        wa_msgv1 LIKE sy-msgv1,
        wa_msgv2 LIKE sy-msgv2,
        wa_msgv3 LIKE sy-msgv3,
        wa_msgv4 LIKE sy-msgv4.

  CALL FUNCTION 'Z_FPM_COMPLETE_ORDER'
    EXPORTING
      ctu      = 'X'
      mode     = wa_mode
      update   = 'L'
*     GROUP    =
*     USER     =
*     KEEP     =
*     HOLDDATE =
      nodata   = ' '
      aufnr    = p_orderid
    IMPORTING
      subrc    = wa_subrc
    TABLES
      messtab  = it_messtab.
  LOOP AT it_messtab.
    CLEAR: wa_text.
    MOVE:   it_messtab-msgnr TO wa_msgnr,
            it_messtab-msgid TO wa_msgid,
            it_messtab-msgv1 TO wa_msgv1,
            it_messtab-msgv2 TO wa_msgv2,
            it_messtab-msgv3 TO wa_msgv3,
            it_messtab-msgv4 TO wa_msgv4.

    CALL FUNCTION 'TB_MESSAGE_BUILD_TEXT'
      EXPORTING
        langu = sy-langu
        msgid = wa_msgid
        msgno = wa_msgnr
        msgv1 = wa_msgv1
        msgv2 = wa_msgv2
        msgv3 = wa_msgv3
        msgv4 = wa_msgv4
      IMPORTING
        text  = wa_text.
    CLEAR:  it_message.
    MOVE : wa_text TO it_message-message,
           it_messtab-msgtyp TO it_message-type,
           it_messtab-msgid  TO it_message-id,
           it_messtab-msgnr  TO it_message-number.
    APPEND it_message.
  ENDLOOP.

ENDFORM.                    " COMPLETE_ORDER
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_INPUT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_screen_input OUTPUT.
  LOOP AT SCREEN .
    IF screen-group1 = 'GR1'.
      IF  zspm_bako-tplnr EQ space AND
          zspm_bako-equnr NE space.
        screen-input = 0.
      ENDIF.
    ELSEIF screen-group1 = 'GR2'.
      IF  zspm_bako-tplnr NE space AND
          zspm_bako-equnr EQ space.
        screen-input = 0.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
ENDMODULE.                 " MODIFY_SCREEN_INPUT  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_INGRP_INFO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_ingrp_info INPUT.
  IF NOT zspm_bako-ingrp IS INITIAL.
    PERFORM get_ingrp_desc.
  ENDIF.
ENDMODULE.                 " CHECK_INGRP_INFO  INPUT
*&---------------------------------------------------------------------*
*&      Form  GET_INGRP_DESC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_ingrp_desc.
  SELECT SINGLE innam INTO zspm_bako-innam
         FROM t024i
         WHERE iwerk = zspm_bako-werks
         AND   ingrp = zspm_bako-ingrp.
ENDFORM.                    " GET_INGRP_DESC
*&---------------------------------------------------------------------*
*&      Module  CHECK_TPLNR_&_EQUNR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_tplnr_equnr INPUT.
  IF zspm_bako-tplnr NE space AND zspm_bako-equnr NE space.
    MESSAGE e000(zmpm) WITH text-m04.
  ENDIF.
ENDMODULE.                 " CHECK_TPLNR_&_EQUNR  INPUT
*&---------------------------------------------------------------------*
*&      Module  VAL_REQ_LGORT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE val_req_lgort INPUT.
  PERFORM val_req_lgort.
ENDMODULE.                 " VAL_REQ_LGORT  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_0100_GET_LINES  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tc_0100_get_lines OUTPUT.
  g_tc_0100_lines = sy-loopc.
ENDMODULE.                 " TC_0100_GET_LINES  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  VAL_REQ_LGORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM val_req_lgort.
  DATA : field_tab       LIKE dfies OCCURS 0,
         return_tab      LIKE ddshretval OCCURS 0,
         dynpfld_mapping LIKE dselc OCCURS 0 WITH HEADER LINE.



  DATA BEGIN OF it_dynpread OCCURS 4.
          INCLUDE STRUCTURE dynpread.
  DATA END OF it_dynpread.

  DATA: BEGIN OF it_mard   OCCURS 0,
          werks LIKE mard-werks,
          lgort LIKE mard-lgort,
          lgobe LIKE t001l-lgobe,
          labst LIKE mard-labst,
        END OF it_mard.

  DATA : wa_progname LIKE sy-repid.
  DATA : wa_dynnum   LIKE sy-dynnr.
  DATA : wa_line TYPE i.

  DATA : wa_matnr LIKE zspm_bapo-matnr.
  DATA : wa_werks LIKE zspm_bapo-werks.

  wa_progname = sy-repid.
  wa_dynnum   = sy-dynnr.

  GET CURSOR LINE wa_line.

  MOVE : 'ZSPM_BAPO-MATNR' TO it_dynpread-fieldname,
          wa_line          TO it_dynpread-stepl.
  APPEND it_dynpread.

  MOVE : 'ZSPM_BAPO-WERKS' TO it_dynpread-fieldname,
          wa_line          TO it_dynpread-stepl.
  APPEND it_dynpread.



  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname     = wa_progname
      dynumb     = wa_dynnum
    TABLES
      dynpfields = it_dynpread
    EXCEPTIONS
      OTHERS     = 1.

  READ TABLE it_dynpread WITH KEY  fieldname =  'ZSPM_BAPO-MATNR'.
  IF sy-subrc EQ 0.
    wa_matnr = it_dynpread-fieldvalue.
  ENDIF.

  READ TABLE it_dynpread WITH KEY  fieldname =  'ZSPM_BAPO-WERKS'.
  IF sy-subrc EQ 0.
    wa_werks = it_dynpread-fieldvalue.
  ENDIF.

  SELECT * INTO  CORRESPONDING FIELDS OF TABLE it_mard
           FROM  mard
           WHERE matnr = wa_matnr.
*             AND WERKS = WA_WERKS.
  CHECK sy-subrc EQ 0.
  LOOP AT it_mard.
    SELECT SINGLE lgobe FROM t001l INTO it_mard-lgobe
           WHERE werks = it_mard-werks
           AND   lgort = it_mard-lgort.
    IF sy-subrc IS INITIAL.
      MODIFY it_mard.
    ENDIF.
  ENDLOOP.

  MOVE : 'F0001'             TO dynpfld_mapping-fldname,
         'ZSPM_BAPO-WERKS'   TO dynpfld_mapping-dyfldname.
  APPEND dynpfld_mapping.
  MOVE : 'F0002'             TO dynpfld_mapping-fldname,
         'ZSPM_BAPO-LGORT'   TO dynpfld_mapping-dyfldname.
  APPEND dynpfld_mapping.
  CLEAR dynpfld_mapping.


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'LGORT'
      dynpprog        = wa_progname
      dynpnr          = wa_dynnum
      dynprofield     = 'ZSPM_BAKO-LGORT'
      value_org       = 'S'
    TABLES
      value_tab       = it_mard
      field_tab       = field_tab
      return_tab      = return_tab
      dynpfld_mapping = dynpfld_mapping.


ENDFORM.                    " VAL_REQ_LGORT
*&---------------------------------------------------------------------*
*&      Module  CHECK_WERKS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_werks INPUT.
  IF zspm_bapo-werks IS INITIAL.
    zspm_bapo-werks = zspm_bako-werks.
  ENDIF.
ENDMODULE.                 " CHECK_WERKS  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_LGORT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_lgort INPUT.
  IF zspm_bapo-lgort IS INITIAL.
    MESSAGE e000(zmpm) WITH text-m18.
  ENDIF.
ENDMODULE.                 " CHECK_LGORT  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_MENGE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_menge INPUT.
  IF zspm_bapo-menge IS INITIAL.
    MESSAGE e000(zmpm) WITH text-m19.
  ENDIF.
ENDMODULE.                 " CHECK_MENGE  INPUT
*&---------------------------------------------------------------------*
*&      Module  VAL_REQ_KTEXT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE val_req_ktext INPUT.
  PERFORM val_req_ktext.
ENDMODULE.                 " VAL_REQ_KTEXT  INPUT
*&---------------------------------------------------------------------*
*&      Form  VAL_REQ_KTEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM val_req_ktext.
  DATA BEGIN OF it_dynpread OCCURS 4.
          INCLUDE STRUCTURE dynpread.
  DATA END OF it_dynpread.

  DATA: BEGIN OF it_ktext   OCCURS 0,
          ktext LIKE zspm_bako-ktext,
        END OF it_ktext.

  DATA : wa_progname LIKE sy-repid.
  DATA : wa_dynnum   LIKE sy-dynnr.
  DATA : wa_line TYPE i.

  wa_progname = sy-repid.
  wa_dynnum   = sy-dynnr.



  SELECT ktext
         INTO CORRESPONDING FIELDS OF TABLE it_ktext
         FROM ztpm_ktext.
*         WHERE SPRAS = SY-LANGU.

*  MOVE : 'Repair Press Machine' TO IT_KTEXT-KTEXT.
*  APPEND IT_KTEXT.
*  MOVE : 'Repair Body Machine' TO IT_KTEXT-KTEXT.
*  APPEND IT_KTEXT.
*  MOVE : 'Repair Paint Machine' TO IT_KTEXT-KTEXT.
*  APPEND IT_KTEXT.
*  MOVE : 'Repair Trim Machine' TO IT_KTEXT-KTEXT.
*  APPEND IT_KTEXT.
*  MOVE : 'Repair Engine Machine' TO IT_KTEXT-KTEXT.
*  APPEND IT_KTEXT.
*  CLEAR IT_KTEXT.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'KTEXT'
      dynpprog    = wa_progname
      dynpnr      = wa_dynnum
      dynprofield = 'ZSPM_BAKO-KTEXT'
      value_org   = 'S'
    TABLES
      value_tab   = it_ktext.
*            FIELD_TAB       = FIELD_TAB
*            RETURN_TAB      = RETURN_TAB
*            DYNPFLD_MAPPING = DYNPFLD_MAPPING.

ENDFORM.                    " VAL_REQ_KTEXT
*&---------------------------------------------------------------------*
*&      Form  MAINTAIN_ORDER_DESCRIPTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM maintain_order_description.
  CALL TRANSACTION 'ZTPM_KTEXT'.
ENDFORM.                    " MAINTAIN_ORDER_DESCRIPTION
