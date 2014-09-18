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
* 03/17/2005      Shiva            UD1K915019     Display storage bin
*                              for selection after the error transaction
*                              is fixed.
*  11/30/2006      Haseeb Mohammad  UD1K922430     Save check on
*ZTMM_container
************************************************************************

*----------------------------------------------------------------------*
***INCLUDE MZEMMPM23E_EMPCONTAINERI01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  w_fcode = w_ok_code.
  CLEAR w_ok_code.
  CASE w_fcode.
    WHEN 'SAVE'.
**--- insert by stlim (2004/04/26)
      CLEAR : w_button_click_time, w_button_click_date.
      MOVE : sy-uzeit TO w_button_click_time,
             sy-datum TO w_button_click_date.
**--- end of insert
      IF w_chk_point = 'GATE 1' . "Chk Point = 'container yard'
        PERFORM save.
      ELSE.                       "Chk Point = 'CC' AND Another
        PERFORM save_ztmm_dock  .
      ENDIF.
    WHEN 'PRINT'.
      PERFORM print.
    WHEN 'LOG'  .
*      PERFORM LOG_DISPLAY.
      CALL TRANSACTION 'ZWME05'.
    WHEN 'BUTTON_FIND'.
      PERFORM find.
    WHEN 'CREATE'.
      w_mode = 'C'.
      CLEAR : leci_tra_dyn,
              ltak-lgnum  ,
              lein-letyp  ,
              w_nltyp    ,
              w_nlber    ,
              w_nlpla    .

    WHEN 'CHANGE'.
      w_mode = 'M'.
      CLEAR : leci_tra_dyn,
              ltak-lgnum  ,
              lein-letyp  ,
              w_nltyp    ,
              w_nlber    ,
              w_nlpla    .
  ENDCASE.
  ltak-lgnum = 'P01'.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHK_FIELD  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE chk_field INPUT.
*Check in input check
  IF leci_tra_dyn-pass_date IS INITIAL
     AND w_ok_code = 'SAVE'.
    CLEAR w_ok_code.
    MESSAGE e003.
  ENDIF.
*Truck's license input check
  IF leci_tra_dyn-vehicle_reg_numb IS INITIAL
     AND w_ok_code = 'SAVE'.
    CLEAR w_ok_code.
    MESSAGE e003.
  ENDIF.
*1st contnr's no input check
**--- insert by stlim (2004/04/26)
  IF sy-tcode NE c_tcode.
    IF leci_tra_dyn-cont_reg_numb1 IS INITIAL
       AND w_ok_code = 'SAVE'.
      CLEAR w_ok_code.
      MESSAGE e003.
    ENDIF.
  ENDIF.
**--- end of insert
*Deliver last name input check
  IF leci_tra_dyn-name_drvr IS INITIAL
     AND w_ok_code = 'SAVE'.
    CLEAR w_ok_code.
    MESSAGE e003.
  ENDIF.
ENDMODULE.     " CHK_FIELD  INPUT
*&---------------------------------------------------------------------*
*&      Module  CONTAINER_EXIST_CHK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE container_exist_chk INPUT.
**--- insert by stlim (2004/05/03)
**--- end of insert
  DATA : MSG_TEXT1(100) TYPE C,
         MSG_TEXT2(100) TYPE C,
         MSG_TEXT3(100) TYPE C,
         w_return type c.
*---
  SELECT SINGLE * FROM ztmm_container
                 WHERE cont_reg_numb1 = leci_tra_dyn-cont_reg_numb1.
  IF sy-subrc = 0 AND w_mode = 'C'.
*    CLEAR w_ok_code.
*    MESSAGE e008 WITH leci_tra_dyn-cont_reg_numb1.
* 10-11-2006 Haseeb Mohammad UD1K922430
    CASE w_ok_code.
      WHEN 'SAVE'.
        CONCATENATE 'Container number  :' ' '
                    ZTMM_CONTAINER-CONT_REG_NUMB1
                    ' ' ' already in system.'   INTO MSG_TEXT1.
        CONCATENATE  'Parked at  :' ' ' ZTMM_CONTAINER-LGTYP '-'
                     ZTMM_CONTAINER-LGPLA '-'
                     ZTMM_CONTAINER-LGBER ' ' ', on pass Date  :'
                     ' ' ZTMM_CONTAINER-PASS_DATE INTO MSG_TEXT2.
        CONCATENATE  '' ' Overwrite Existing Record ?' INTO MSG_TEXT3.
*        MESSAGE i008 WITH MSG_TEXT.
        CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_MESSAGE'
             EXPORTING
                  DIAGNOSETEXT1  = MSG_TEXT1
                  DIAGNOSETEXT2  = MSG_TEXT2
                  DIAGNOSETEXT3  = MSG_TEXT3
                  TEXTLINE1      = 'SAVE'
                  TITEL          = 'Confirm Save'
                  START_COLUMN   = 25
                  START_ROW      = 6
                  CANCEL_DISPLAY = ''
             IMPORTING
                  ANSWER         = w_return.
        IF W_RETURN = 'J'.
          W_DELETE = 'Y'.
        ELSE.
          MESSAGE i999 WITH 'NO CHANGE MADE'.
*        W_MODE  = 'K'.
          CLEAR w_ok_code.
          LEAVE TO SCREEN 100.
        ENDIF.

*
*        CALL FUNCTION 'COPO_POPUP_TO_GOON'
*         EXPORTING
*           TEXTLINE1       = LECI_TRA_DYN-CONT_REG_NUMB1
*           TEXTLINE2       = 'Do you want to OVERWRITE this? '
**          TEXTLINE3       = ' '
*           TITEL           = 'Save? '
*         IMPORTING
*           ANSWER          = w_return.
*
*        IF W_RETURN = 'G'.
*          W_DELETE = 'Y'.
*        ELSE.
*          MESSAGE i999 WITH 'NO CHANGE MADE'.
**        W_MODE  = 'K'.
*          CLEAR w_ok_code.
*          LEAVE TO SCREEN 100.
*        ENDIF.

      WHEN 'CHANGE'.
      WHEN 'CREATE'.
      WHEN OTHERS.

        CONCATENATE ZTMM_CONTAINER-LGTYP '-' ZTMM_CONTAINER-LGPLA '-'
                   ZTMM_CONTAINER-LGBER INTO MSG_TEXT1.
        MESSAGE i040 WITH ZTMM_CONTAINER-CONT_REG_NUMB1 msg_text1
                ZTMM_CONTAINER-PASS_DATE.
        CLEAR w_ok_code.

    ENDCASE.
  ENDIF.
* 10-11-2006 Haseeb Mohammad UD1K922430
  CLEAR ztmm_container.
ENDMODULE.                 " CONTAINER_EXIST_CHK  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_command INPUT.
  CASE w_ok_code.
    WHEN 'EXIT' OR 'BACK' OR 'CANC'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " EXIT_COMMAND  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHK_FIELD_DOCK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE chk_field_dock INPUT.
  IF w_nltyp = space AND w_ok_code = 'SAVE'
     AND w_chk_point = 'GATE 1'.
    CLEAR w_ok_code.
    MESSAGE e003.
  ENDIF.

  IF w_nlber = space AND w_ok_code = 'SAVE'
     AND w_chk_point = 'GATE 1'.
    CLEAR w_ok_code.
    MESSAGE e003.
  ENDIF.

  IF w_nlpla = space AND w_ok_code = 'SAVE'
     AND w_chk_point = 'GATE 1'.
    CLEAR w_ok_code.
    MESSAGE e003.
  ENDIF.

  IF lein-letyp = space AND w_ok_code = 'SAVE'
     AND w_chk_point = 'GATE 1'.
    CLEAR w_ok_code.
    MESSAGE e003.
  ENDIF.
*In Case Check point is exception cart yard,
*Dock Number Input Check.
  CHECK w_chk_point <> 'GATE 1'.
*  IF LECI_TRA_DYN-ID_CARD_NUMB IS INITIAL
*     AND W_OK_CODE = 'SAVE'.
*    CLEAR W_OK_CODE.
*    MESSAGE E003.
*  ENDIF.
ENDMODULE.                 " CHK_FIELD_DOCK  INPUT
*&---------------------------------------------------------------------*
*&      Module  DOCKID_ENTRY  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE dockid_entry INPUT.
  DATA: it_return LIKE ddshretval OCCURS 10 WITH HEADER LINE.
  DATA: it_dfies LIKE dfies OCCURS 10 WITH HEADER LINE.

  DATA: BEGIN OF it_dock OCCURS 0.
          INCLUDE STRUCTURE ztmm_dock.
  DATA: occupied TYPE i          ,
        waiting  TYPE i          .
  DATA: END OF it_dock.

  DATA: BEGIN OF it_ddock OCCURS 0  ,
          zdock LIKE ztmm_dock-zdock,
          occupied                  ,
          waiting                   ,
        END OF it_ddock.

  FIELD-SYMBOLS : <fs1>.
  DATA: w_cnt1_flg(02) TYPE n, w_name(20), w_cnt2_flg TYPE i.

* [ 1 ] Get Dock Number
  SELECT * FROM ztmm_dock
           INTO CORRESPONDING FIELDS OF TABLE it_dock
          WHERE chk_point = w_chk_point.
  LOOP AT it_dock.
    DO 10 TIMES.
      w_cnt1_flg = sy-index.
      CONCATENATE 'IT_DOCK-TR_' w_cnt1_flg INTO w_name.
      ASSIGN (w_name) TO <fs1>.
      IF <fs1> <> space. w_cnt2_flg = w_cnt2_flg + 1. ENDIF.
    ENDDO.
    IF w_cnt2_flg > 0.
      it_dock-occupied = 1       .
      it_dock-waiting  = w_cnt2_flg - 1.
    ELSE.
      it_dock-occupied = 0.
      it_dock-waiting  = 0.
    ENDIF.
    MODIFY it_dock.
    CLEAR w_cnt2_flg.
  ENDLOOP.

  CLEAR : it_ddock, it_ddock[].
  LOOP AT it_dock.
    MOVE-CORRESPONDING it_dock TO it_ddock.
    APPEND it_ddock.
    CLEAR it_ddock.
  ENDLOOP.

* [ 2 ] Setting Fields
  CLEAR: it_dfies, it_dfies[],
         it_return, it_return[].
  it_dfies-fieldname = 'ZDOCK'.
  it_dfies-position  = 1.
  it_dfies-offset    = 0.
  it_dfies-intlen    = 10.
  it_dfies-outputlen = 10.
  it_dfies-scrtext_s = 'Dock'.
  APPEND it_dfies.

  CLEAR: it_dfies.
  it_dfies-fieldname = 'OCCUPIED'.
  it_dfies-position  = 2.
  it_dfies-offset    = 10.
  it_dfies-intlen    = 1.
  it_dfies-outputlen = 1.
  it_dfies-scrtext_s = 'Occupied'.
  APPEND it_dfies.

  CLEAR: it_dfies.
  it_dfies-fieldname = 'WAITING'.
  it_dfies-position  = 3.
  it_dfies-offset    = 11.
  it_dfies-intlen    = 1.
  it_dfies-outputlen = 1.
  it_dfies-scrtext_s = 'Waiting'.
  APPEND it_dfies.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*   DDIC_STRUCTURE         = ' '
      retfield               = 'ZDOCK'
*   PVALKEY                = ' '
      dynpprog               = sy-cprog
      dynpnr                 = sy-dynnr
      dynprofield            = 'ZDOCK'
*   STEPL                  = 0
*   WINDOW_TITLE           =
*   VALUE                  = ' '
      value_org              = 'S'
*   MULTIPLE_CHOICE        = ' '
*   DISPLAY                = ' '
*   CALLBACK_PROGRAM       = ' '
*   CALLBACK_FORM          = ' '
    TABLES
      value_tab              = it_ddock
      field_tab              = it_dfies
*      RETURN_TAB             = IT_RETURN
*      DYNPFLD_MAPPING        = DSELC
* EXCEPTIONS
*   PARAMETER_ERROR        = 1
*   NO_VALUES_FOUND        = 2
*   OTHERS                 = 3
            .

ENDMODULE.                 " DOCKID_ENTRY  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_CHK_POINT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_chk_point INPUT.
*GET Check Point
  CASE w_mode.
    WHEN 'C'.
      w_chk_point = leci_chkpt_dyn-chkin_point.
    WHEN 'M'.
*    Check Point is gotten from Form FIND.
  ENDCASE.
ENDMODULE.                 " GET_CHK_POINT  INPUT
**&---------------------------------------------------------------------
*
**&      Module  NLTYP_POSSIBEL  INPUT
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
*MODULE NLTYP_POSSIBEL INPUT.
*  DATA: BEGIN OF DSELC OCCURS 0.
*          INCLUDE STRUCTURE DSELC.
*  DATA: END OF DSELC.
*
*
** [ 1 ] Get Storage bin Data from LAGP(table)
*  SELECT * FROM LAGP
*           INTO CORRESPONDING FIELDS OF TABLE IT_STORAGE
*          WHERE LGNUM = 'P01'.
** [ 2 ] Setting Fields
*  CLEAR: IT_DFIES, IT_DFIES[],
*         IT_RETURN, IT_RETURN[].
*  IT_DFIES-FIELDNAME = 'LGNUM'.
*  IT_DFIES-POSITION  = 1.
*  IT_DFIES-OFFSET    = 0.
*  IT_DFIES-INTLEN    = 3.
*  IT_DFIES-OUTPUTLEN = 3.
*  IT_DFIES-SCRTEXT_S = 'WhN'.
*  APPEND IT_DFIES.
*
*  CLEAR: IT_DFIES.
*  IT_DFIES-FIELDNAME = 'LGTYP'.
*  IT_DFIES-POSITION  = 2.
*  IT_DFIES-OFFSET    = 3.
*  IT_DFIES-INTLEN    = 3.
*  IT_DFIES-OUTPUTLEN = 3.
*  IT_DFIES-SCRTEXT_S = 'Tpe'.
*  APPEND IT_DFIES.
*
*  CLEAR: IT_DFIES.
*  IT_DFIES-FIELDNAME = 'LGBER'.
*  IT_DFIES-POSITION  = 3.
*  IT_DFIES-OFFSET    = 6.
*  IT_DFIES-INTLEN    = 3.
*  IT_DFIES-OUTPUTLEN = 3.
*  IT_DFIES-SCRTEXT_S = 'Sec'.
*  APPEND IT_DFIES.
*
*  CLEAR: IT_DFIES.
*  IT_DFIES-FIELDNAME = 'LGPLA'.
*  IT_DFIES-POSITION  = 4.
*  IT_DFIES-OFFSET    = 9.
*  IT_DFIES-INTLEN    = 10.
*  IT_DFIES-OUTPUTLEN = 10.
*  IT_DFIES-SCRTEXT_S = 'Stor.bin'.
*  APPEND IT_DFIES.
*
*  CLEAR: IT_DFIES.
*  IT_DFIES-FIELDNAME = 'KZLER'.
*  IT_DFIES-POSITION  = 5.
*  IT_DFIES-OFFSET    = 19.
*  IT_DFIES-INTLEN    = 5.
*  IT_DFIES-OUTPUTLEN = 5.
*  IT_DFIES-SCRTEXT_S = 'Empty'.
*  APPEND IT_DFIES.
*
*  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
*    EXPORTING
**   DDIC_STRUCTURE         = ' '
*      RETFIELD               = 'LGTYP'
**   PVALKEY                = ' '
*      DYNPPROG               = SY-CPROG
*      DYNPNR                 = SY-DYNNR
*      DYNPROFIELD            = 'LGTYP'
**   STEPL                  = 0
**   WINDOW_TITLE           =
**   VALUE                  = ' '
*      VALUE_ORG              = 'S'
**   MULTIPLE_CHOICE        = ' '
**   DISPLAY                = ' '
**   CALLBACK_PROGRAM       = ' '
**   CALLBACK_FORM          = ' '
*    TABLES
*      VALUE_TAB              = IT_STORAGE
*      FIELD_TAB              = IT_DFIES
*      RETURN_TAB             = IT_RETURN
*      DYNPFLD_MAPPING        = DSELC
** EXCEPTIONS
**   PARAMETER_ERROR        = 1
**   NO_VALUES_FOUND        = 2
**   OTHERS                 = 3
*            .
*ENDMODULE.                 " NLTYP_POSSIBEL  INPUT
*&---------------------------------------------------------------------*
*&      Module  NLTYP_POSSIBEL_ENTRY  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE nltyp_possibel_entry INPUT.
  DATA : w_sfield   LIKE  help_info-fieldname,
         it_fields  LIKE  help_value OCCURS 1 WITH HEADER LINE,
         w_svalue   LIKE  help_info-fldvalue,
         w_idx    LIKE  sy-tabix          ,
         w_lgtyp      LIKE  lagp-lgtyp        .
  DATA : w_no TYPE i.

* [ 1 ] Get Storage bin Data from LAGP(table)
*  IF SY-MANDT = '140'.
**     W_LGTYP = '100'.
*  ELSE.
  w_lgtyp = '411'.
*  ENDIF.

  SELECT * FROM lagp
           INTO CORRESPONDING FIELDS OF TABLE it_storage
          WHERE lgnum = 'P01'
            AND lgtyp IN (w_lgtyp , '411')
            AND kzler = 'X'.

**--- insert by stlim (2004/05/18)
  TABLES : ztmm_ct_errlog.

  DATA : it_ztmm_ct_errlog LIKE ztmm_ct_errlog OCCURS 0
                                               WITH HEADER LINE.
  DATA : l_parking_txt LIKE ztmm_ct_errlog-parking_txt.

  CLEAR : it_storage, it_ztmm_ct_errlog, it_ztmm_ct_errlog[].

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_ztmm_ct_errlog
           FROM ztmm_ct_errlog
           where flag ne 'X'.

  LOOP AT it_storage.
    CLEAR : l_parking_txt, it_ztmm_ct_errlog.
    CONCATENATE 'BB' '-' it_storage-lgtyp '-' it_storage-lgber
                     '-' it_storage-lgpla INTO l_parking_txt.
    READ TABLE it_ztmm_ct_errlog WITH KEY parking_txt = l_parking_txt.
    IF sy-subrc EQ 0.
      DELETE it_storage.
    ENDIF.
  ENDLOOP.
**--- end of insert

  DESCRIBE TABLE it_storage LINES w_no.
  IF w_no = 0.
    MESSAGE e012.
  ENDIF.
* [ 2 ] Possible Entry
  CLEAR : it_fields, it_fields[].
  it_fields-tabname   = 'LAGP'.
  it_fields-fieldname = 'LGNUM'.
  it_fields-selectflag = ' '.
  APPEND it_fields.
  CLEAR : it_fields.
  it_fields-tabname   = 'LAGP'.
  it_fields-fieldname = 'LGTYP'.
  it_fields-selectflag = ' '.
  APPEND it_fields.
  CLEAR : it_fields.
  it_fields-tabname   = 'LAGP'.
  it_fields-fieldname = 'LGBER'.
  it_fields-selectflag = ' '.
  APPEND it_fields.
  CLEAR : it_fields.
  it_fields-tabname = 'LAGP'.
  it_fields-fieldname = 'LGPLA'.
  it_fields-selectflag = ' '.
  APPEND it_fields.
  CLEAR : it_fields.
  it_fields-tabname = 'LAGP'.
  it_fields-fieldname = 'KZLER'.
  it_fields-selectflag = ' '.
  APPEND it_fields.

  CALL FUNCTION 'HELP_VALUES_GET_NO_DD_NAME'
    EXPORTING
      selectfield                         = w_sfield
    IMPORTING
      ind                                 = w_idx
      select_value                        = w_svalue
    TABLES
      fields                              = it_fields
      full_table                          = it_storage
*   USER_SEL_FIELDS                     =
*   HEADING_TABLE                       =
* EXCEPTIONS
*   FULL_TABLE_EMPTY                    = 1
*   NO_TABLESTRUCTURE_GIVEN             = 2
*   NO_TABLEFIELDS_IN_DICTIONARY        = 3
*   MORE_THEN_ONE_SELECTFIELD           = 4
*   NO_SELECTFIELD                      = 5
*   OTHERS                              = 6
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  CHECK w_idx > 0 AND w_mode = 'C'.
  CLEAR it_storage.
  READ TABLE it_storage INDEX w_idx.
  PERFORM fill_storage_type.
ENDMODULE.                 " NLTYP_POSSIBEL  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LECI_TRA_DYN_TEXT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_leci_tra_dyn_text INPUT.
  CASE w_chk_point.
    WHEN 'GATE 1'.
      CONCATENATE lein-letyp  '-'
                  w_nltyp '-'
                  w_nlber '-'
                  w_nlpla
                  INTO leci_tra_dyn-parking_txt.
    WHEN OTHERS.
      CONCATENATE w_nltyp '-'
                  w_nlber '-'
                  w_nlpla
                  INTO leci_tra_dyn-parking_txt.
  ENDCASE.
ENDMODULE.                 " GET_LECI_TRA_DYN_TEXT  INTUT
*&---------------------------------------------------------------------*
*&      Module  GET_TXT10  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_txt10 INPUT.
  IF w_mode = 'C'.
    w_txt10 = 'Selection Possible'.
  ELSE.
    w_txt10 = 'Selection Impossible'.
  ENDIF.
ENDMODULE.                 " GET_TXT10  INPUT
