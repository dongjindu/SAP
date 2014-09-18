************************************************************************
* Program name : ZEMMGM10E_ASSIGN_BLANK
* Created by   : Min-su Park
* Created on   : 2003.11.11.
* Pattern      : Report 1-1
* Description  : Assign BLANK to STEEL
*
* Modification Log
* Date            Developer        Request No.    Description
* 2003.11.11.     Min-su Park      UD1K901873     Initial Coding
*
************************************************************************

*----------------------------------------------------------------------*
*   INCLUDE ZEMMGM10E_ASSIGN_BLANKPAI                                  *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_command INPUT.
  w_fcode = ok_code.
  CASE w_fcode.
    WHEN 'EXIT' OR 'CANC' OR 'BACK'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " EXIT_COMMAND  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  w_fcode = ok_code.
  CASE w_fcode.
    WHEN 'SAVE'.
      PERFORM bom_create.
    WHEN 'P--' OR 'P-' OR 'P+' OR 'P++'.
      PERFORM page_control.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_screen INPUT.
  MODIFY it_bom INDEX tc_bom-current_line.
ENDMODULE.                 " MODIFY_SCREEN  INPUT
*&---------------------------------------------------------------------*
*&      Module  STEEL_ENTRY  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE steel_entry INPUT.
  DATA: it_return LIKE ddshretval OCCURS 10 WITH HEADER LINE.
  DATA: it_dfies LIKE dfies OCCURS 10 WITH HEADER LINE.
* [ 2 ] Setting Fields
  CLEAR: it_dfies, it_dfies[],
         it_return, it_return[].
  it_dfies-fieldname = 'MAKTX'.
  it_dfies-position  = 1.
  it_dfies-offset    = 0.
  it_dfies-intlen    = 40.
  it_dfies-outputlen = 40.
  it_dfies-scrtext_s = 'Characteristics'.
  APPEND it_dfies.

  CLEAR: it_dfies.
  it_dfies-fieldname = 'MATNR'.
  it_dfies-position  = 2.
  it_dfies-offset    = 40.
  it_dfies-intlen    = 18.
  it_dfies-outputlen = 18.
  it_dfies-scrtext_s = 'STEEL'.
  APPEND it_dfies.


*/ Begin of Added by Hakchin 20040109
* We make Possible Entries about Quality in the form
* Quality_XXX_Thick_Width_Length.

*Dynamic read OF screen value
  PERFORM dynp_values_read TABLES   it_dynpread
                           USING    'IT_BOM-CHARACTER'  "Characteristic
                           CHANGING f4rc.
  READ TABLE it_dynpread INTO wa_dynpread INDEX 1.

  DATA: lt_ammat LIKE TABLE OF it_ammat.
  FIELD-SYMBOLS: <fs_ammat> LIKE it_ammat.
  DATA: lv_fdpos1 LIKE sy-fdpos.
  DATA: lv_fdpos2 LIKE sy-fdpos.

  CLEAR: lt_ammat.
  LOOP AT it_ammat ASSIGNING <fs_ammat>.
    SEARCH <fs_ammat>-maktx2 FOR '_'.

    CHECK sy-subrc = 0.
    lv_fdpos1 = sy-fdpos.
    SEARCH wa_dynpread-fieldvalue FOR '_'.

    CHECK sy-subrc = 0.
    lv_fdpos2 = sy-fdpos.
    IF <fs_ammat>-maktx2(lv_fdpos1) =
       wa_dynpread-fieldvalue(lv_fdpos1).
      APPEND <fs_ammat> TO lt_ammat.
    ENDIF.
  ENDLOOP.
*/ End of Added by Hakchin 20040109

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*   DDIC_STRUCTURE         = ' '
      retfield               = 'MATNR'
*   PVALKEY                = ' '
      dynpprog               = sy-cprog
      dynpnr                 = sy-dynnr
      dynprofield            = 'MATNR'
*   STEPL                  = 0
*   WINDOW_TITLE           =
*   VALUE                  = ' '
      value_org              = 'S'
*   MULTIPLE_CHOICE        = ' '
*   DISPLAY                = ' '
*   CALLBACK_PROGRAM       = ' '
*   CALLBACK_FORM          = ' '
    TABLES
*      value_tab              = it_ammat
      value_tab              = lt_ammat
      field_tab              = it_dfies
*      RETURN_TAB             = IT_RETURN
*      DYNPFLD_MAPPING        = DSELC
* EXCEPTIONS
*   PARAMETER_ERROR        = 1
*   NO_VALUES_FOUND        = 2
*   OTHERS                 = 3
            .

ENDMODULE.                 " STEEL_ENTRY  INPUT
