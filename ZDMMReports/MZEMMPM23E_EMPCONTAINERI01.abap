************************************************************************
* Program name : SAPMZEMMPM23E_EMPCONTAINER
* Created by   : Min-su Park
* Created on   : 2003.09.25.
* Pattern      :
* Description  :
*   1. Modified Welcome screen-For Inbound delivery-Putaway process    *
*   2. Empty Container management                                      *
*                                                                      *
* Modification Log                                                     *
* Date            Developer        Request No.    Description          *
* 2003.09.19.     Min-su Park      UD1K901873     Initial Coding       *
*                                                                      *
************************************************************************
*----------------------------------------------------------------------*
***INCLUDE MZEMMPM23E_EMPCONTAINERI01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0100 input.
  w_fcode = w_ok_code.
  clear w_ok_code.
  case w_fcode.
    when 'SAVE'.
      if w_chk_point = 'GATE 1' . "Chk Point = 'container yard'
        perform save.
       else.                       "Chk Point = 'CC' AND Another
        perform save_ztmm_dock  .
      endif.
    when 'PRINT'.
        perform print.
    when 'BUTTON_FIND'.
      perform find.
    when 'CREATE'.
      w_mode = 'C'.
      clear : leci_tra_dyn,
              ltak-lgnum  ,
              lein-letyp  ,
              w_nltyp    ,
              w_nlber    ,
              w_nlpla    .

    when 'CHANGE'.
      w_mode = 'M'.
      clear : leci_tra_dyn,
              ltak-lgnum  ,
              lein-letyp  ,
              w_nltyp    ,
              w_nlber    ,
              w_nlpla    .
  endcase.
  ltak-lgnum = 'P01'.
endmodule.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHK_FIELD  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module chk_field input.
*Check in input check
  if leci_tra_dyn-pass_date is initial
     and w_ok_code = 'SAVE'.
    clear w_ok_code.
    message e003.
  endif.
*Truck's license input check
  if leci_tra_dyn-vehicle_reg_numb is initial
     and w_ok_code = 'SAVE'.
    clear w_ok_code.
    message e003.
  endif.
*1st contnr's no input check
  if leci_tra_dyn-cont_reg_numb1 is initial
     and w_ok_code = 'SAVE'.
    clear w_ok_code.
    message e003.
  endif.
*Deliver last name input check
  if leci_tra_dyn-name_drvr is initial
     and w_ok_code = 'SAVE'.
    clear w_ok_code.
    message e003.
  endif.
endmodule.     " CHK_FIELD  INPUT
*&---------------------------------------------------------------------*
*&      Module  CONTAINER_EXIST_CHK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module container_exist_chk input.
  select single * from ztmm_container
                 where cont_reg_numb1 = leci_tra_dyn-cont_reg_numb1.
  if sy-subrc = 0 and w_mode = 'C' and w_ok_code = 'SAVE'.
    clear w_ok_code.
    message e008 with leci_tra_dyn-cont_reg_numb1.
  endif.
  clear ztmm_container.
endmodule.                 " CONTAINER_EXIST_CHK  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module exit_command input.
  case w_ok_code.
    when 'EXIT' or 'BACK' or 'CANC'.
      leave program.
  endcase.
endmodule.                 " EXIT_COMMAND  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHK_FIELD_DOCK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module chk_field_dock input.
  if w_nltyp = space and w_ok_code = 'SAVE'
     and w_chk_point = 'GATE 1'.
    clear w_ok_code.
    message e003.
  endif.

  if w_nlber = space and w_ok_code = 'SAVE'
     and w_chk_point = 'GATE 1'.
    clear w_ok_code.
    message e003.
  endif.

  if w_nlpla = space and w_ok_code = 'SAVE'
     and w_chk_point = 'GATE 1'.
    clear w_ok_code.
    message e003.
  endif.

  if lein-letyp = space and w_ok_code = 'SAVE'
     and w_chk_point = 'GATE 1'.
    clear w_ok_code.
    message e003.
  endif.
*In Case Check point is exception cart yard,
*Dock Number Input Check.
  check w_chk_point <> 'GATE 1'.
*  IF LECI_TRA_DYN-ID_CARD_NUMB IS INITIAL
*     AND W_OK_CODE = 'SAVE'.
*    CLEAR W_OK_CODE.
*    MESSAGE E003.
*  ENDIF.
endmodule.                 " CHK_FIELD_DOCK  INPUT
*&---------------------------------------------------------------------*
*&      Module  DOCKID_ENTRY  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module dockid_entry input.
  data: it_return like ddshretval occurs 10 with header line.
  data: it_dfies like dfies occurs 10 with header line.

  data: begin of it_dock occurs 0.
      include structure ztmm_dock.
  data: occupied type i          ,
        waiting  type i          .
  data: end of it_dock.

  data: begin of it_ddock occurs 0  ,
          zdock like ztmm_dock-zdock,
          occupied                  ,
          waiting                   ,
        end of it_ddock.

  field-symbols : <fs1>.
  data: w_cnt1_flg(02) type n, w_name(20), w_cnt2_flg type i.

* [ 1 ] Get Dock Number
  select * from ztmm_dock
           into corresponding fields of table it_dock
          where chk_point = w_chk_point.
  loop at it_dock.
   do 10 times.
     w_cnt1_flg = sy-index.
     concatenate 'IT_DOCK-TR_' w_cnt1_flg into w_name.
     assign (w_name) to <fs1>.
     if <fs1> <> space. w_cnt2_flg = w_cnt2_flg + 1. endif.
   enddo.
   if w_cnt2_flg > 0.
      it_dock-occupied = 1       .
      it_dock-waiting  = w_cnt2_flg - 1.
   else.
      it_dock-occupied = 0.
      it_dock-waiting  = 0.
   endif.
   modify it_dock.
   clear w_cnt2_flg.
  endloop.

  clear : it_ddock, it_ddock[].
  loop at it_dock.
    move-corresponding it_dock to it_ddock.
    append it_ddock.
    clear it_ddock.
  endloop.

* [ 2 ] Setting Fields
   clear: it_dfies, it_dfies[],
          it_return, it_return[].
   it_dfies-fieldname = 'ZDOCK'.
   it_dfies-position  = 1.
   it_dfies-offset    = 0.
   it_dfies-intlen    = 10.
   it_dfies-outputlen = 10.
   it_dfies-scrtext_s = 'Dock'.
   append it_dfies.

   clear: it_dfies.
   it_dfies-fieldname = 'OCCUPIED'.
   it_dfies-position  = 2.
   it_dfies-offset    = 10.
   it_dfies-intlen    = 1.
   it_dfies-outputlen = 1.
   it_dfies-scrtext_s = 'Occupied'.
   append it_dfies.

   clear: it_dfies.
   it_dfies-fieldname = 'WAITING'.
   it_dfies-position  = 3.
   it_dfies-offset    = 11.
   it_dfies-intlen    = 1.
   it_dfies-outputlen = 1.
   it_dfies-scrtext_s = 'Waiting'.
   append it_dfies.

  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
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
    tables
      value_tab              = it_ddock
      field_tab              = it_dfies
*      RETURN_TAB             = IT_RETURN
*      DYNPFLD_MAPPING        = DSELC
* EXCEPTIONS
*   PARAMETER_ERROR        = 1
*   NO_VALUES_FOUND        = 2
*   OTHERS                 = 3
            .

endmodule.                 " DOCKID_ENTRY  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_CHK_POINT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module get_chk_point input.
*GET Check Point
  case w_mode.
    when 'C'.
      w_chk_point = leci_chkpt_dyn-chkin_point.
    when 'M'.
*    Check Point is gotten from Form FIND.
  endcase.
endmodule.                 " GET_CHK_POINT  INPUT
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
module nltyp_possibel_entry input.
  data : w_sfield   like  help_info-fieldname,
         it_fields  like  help_value occurs 1 with header line,
         w_svalue   like  help_info-fldvalue,
         w_idx    like  sy-tabix          ,
         w_lgtyp      like  lagp-lgtyp        .
  data : w_no type i.

* [ 1 ] Get Storage bin Data from LAGP(table)
*  IF SY-MANDT = '140'.
*     W_LGTYP = '100'.
*  ELSE.
     w_lgtyp = '411'.
*  ENDIF.

  select * from lagp
           into corresponding fields of table it_storage
          where lgnum = 'P01'
            and lgtyp in (w_lgtyp , '411')
            and kzler = 'X'.
  describe table it_storage lines w_no.
  if w_no = 0.
     message e012.
  endif.
* [ 2 ] Possible Entry
  clear : it_fields, it_fields[].
  it_fields-tabname   = 'LAGP'.
  it_fields-fieldname = 'LGNUM'.
  it_fields-selectflag = ' '.
  append it_fields.
  clear : it_fields.
  it_fields-tabname   = 'LAGP'.
  it_fields-fieldname = 'LGTYP'.
  it_fields-selectflag = ' '.
  append it_fields.
  clear : it_fields.
  it_fields-tabname   = 'LAGP'.
  it_fields-fieldname = 'LGBER'.
  it_fields-selectflag = ' '.
  append it_fields.
  clear : it_fields.
  it_fields-tabname = 'LAGP'.
  it_fields-fieldname = 'LGPLA'.
  it_fields-selectflag = ' '.
  append it_fields.
  clear : it_fields.
  it_fields-tabname = 'LAGP'.
  it_fields-fieldname = 'KZLER'.
  it_fields-selectflag = ' '.
  append it_fields.

call function 'HELP_VALUES_GET_NO_DD_NAME'
  exporting
    selectfield                         = w_sfield
  importing
    ind                                 = w_idx
    select_value                        = w_svalue
  tables
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
if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
endif.
 check w_idx > 0 and w_mode = 'C'.
 clear it_storage.
 read table it_storage index w_idx.
  perform fill_storage_type.
endmodule.                 " NLTYP_POSSIBEL  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LECI_TRA_DYN_TEXT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module get_leci_tra_dyn_text input.
case w_chk_point.
 when 'GATE 1'.
  concatenate lein-letyp  '-'
              w_nltyp '-'
              w_nlber '-'
              w_nlpla
              into leci_tra_dyn-parking_txt.
 when others.
  concatenate w_nltyp '-'
              w_nlber '-'
              w_nlpla
              into leci_tra_dyn-parking_txt.
endcase.
endmodule.                 " GET_LECI_TRA_DYN_TEXT  INTUT
*&---------------------------------------------------------------------*
*&      Module  GET_TXT10  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module get_txt10 input.
  if w_mode = 'C'.
     w_txt10 = 'Selection Possible'.
  else.
     w_txt10 = 'Selection Impossible'.
  endif.
endmodule.                 " GET_TXT10  INPUT
