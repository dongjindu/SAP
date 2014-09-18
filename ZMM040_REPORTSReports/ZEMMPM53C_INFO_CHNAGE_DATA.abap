*&--------------------------------------------------------------------
*& REPORT                 : ZEMMPM53C_INFO_CHANGE_DATA
*& Author                 : FURONG WANG
*& Creation Date          : 08/09/2005
*& Specification By       :
*& Pattern                : Report 1-1 1
*& Development Request No :
*& Addl documentation     :
*& Description            :  Info record creation or update
*& Modification Log
*& Date       Developer    Request ID      Description
*&
*&--------------------------------------------------------------------
REPORT  zemmpm53c_info  MESSAGE-ID zmmm.

*DATA : BEGIN OF gt_out OCCURS 0,
*        icon(4),
*        chkbox  TYPE c,
*        light   TYPE c,
*        tabcolor     TYPE slis_t_specialcol_alv.
*        INCLUDE STRUCTURE ztmm_if_price.
*DATA:  END OF gt_out.
*DATA : it_out LIKE gt_out OCCURS 0 WITH HEADER LINE.
*DATA  :wa_out LIKE gt_out.
*
DATA: it_if_price TYPE ztmm_if_price OCCURS 0 WITH HEADER LINE.
DATA : w_int TYPE i.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  1(10) text-003.
SELECTION-SCREEN POSITION 12.
PARAMETERS  pb_e TYPE c  RADIOBUTTON GROUP ra01.
SELECTION-SCREEN COMMENT  22(9) text-004.
SELECTION-SCREEN POSITION 31.
PARAMETERS  pb_s TYPE c  RADIOBUTTON GROUP ra01.
SELECTION-SCREEN COMMENT  41(11) text-005.
SELECTION-SCREEN POSITION 52.
PARAMETERS  pb_a TYPE c  RADIOBUTTON GROUP ra01.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b2.
SELECTION-SCREEN END OF BLOCK b1.
*Selection condition
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-002.

SELECT-OPTIONS : s_lifnr FOR it_if_price-lifnr,
                 s_matnr FOR it_if_price-matnr,
                 s_inf_d FOR it_if_price-ZBDAT.
SELECTION-SCREEN END OF BLOCK b3.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
*AT SELECTION-SCREEN OUTPUT.
*  PERFORM selection_output.
*---------------------------------------------------------------------
*    M   A   I   N
*---------------------------------------------------------------------
START-OF-SELECTION.
 PERFORM select_data.
 IF it_if_price[] IS INITIAL.
    MESSAGE s009 WITH 'No found data '.
    EXIT.
 ENDIF.
 CALL SCREEN 9200.
*&---------------------------------------------------------------------*
*&      Form  select_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_data.
*get data
  REFRESH: it_if_price.
  CASE 'X'.
    WHEN pb_e.
      SELECT * FROM ztmm_if_price INTO TABLE it_if_price
       WHERE zresult EQ 'E'
         AND lifnr IN s_lifnr
         AND matnr IN s_matnr
         AND inf_d IN s_inf_d.

    WHEN pb_s.
      SELECT * FROM ztmm_if_price INTO TABLE it_if_price
       WHERE zresult EQ 'S'
         AND lifnr IN s_lifnr
         AND matnr IN s_matnr
         AND inf_d IN s_inf_d.

    WHEN pb_a.
      SELECT * FROM ztmm_if_price INTO TABLE it_if_price
       WHERE lifnr IN s_lifnr
         AND matnr IN s_matnr
         AND inf_d IN s_inf_d.

  ENDCASE.

  DESCRIBE TABLE it_if_price LINES w_int.
  IF w_int = 0.
    EXIT.
*  ELSE.
*    PERFORM move_value.
  ENDIF.
ENDFORM.                    " select_data

*&spwizard: declaration of tablecontrol 'TC_9200' itself
controls: TC_9200 type tableview using screen 9200.

*&spwizard: output module for tc 'TC_9200'. do not change this line!
*&spwizard: update lines for equivalent scrollbar
module TC_9200_change_tc_attr output.
  describe table IT_IF_PRICE lines TC_9200-lines.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  TC_9200_change_field_attr  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_9200_change_field_attr OUTPUT.
  LOOP AT SCREEN.
    IF SCREEN-NAME = 'IT_IF_PRICE-MATNR' OR
       SCREEN-NAME = 'IT_IF_PRICE-LIFNR' OR
       SCREEN-NAME = 'IT_IF_PRICE-ZSEQ'.
       SCREEN-INPUT = 0.
*       SCREEN-INVISIBLE = 1.
    ELSE.
       SCREEN-INPUT = 1.
*       SCREEN-INVISIBLE = 1.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

ENDMODULE.                 " TC_9200_change_field_attr  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_9200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9200 OUTPUT.
  SET PF-STATUS '9200'.
  SET TITLEBAR '9200'.

ENDMODULE.                 " STATUS_9200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  if sy-ucomm = 'EXIT' OR sy-ucomm = 'CANC'.
     CLEAR: sy-ucomm.
     LEAVE TO SCREEN 0.
  endif.
ENDMODULE.                 " exit  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_9200_modify  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_9200_modify INPUT.
  modify it_if_price index tc_9200-current_line.
ENDMODULE.                 " TC_9200_modify  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9200 INPUT.
  case sy-ucomm.
  when 'SAVE'.
   PERFORM SAVE_DATA.
  when 'EXIT'.
   CLEAR SY-UCOMM.
   LEAVE TO SCREEN 0.
  endcase.
ENDMODULE.                 " USER_COMMAND_9200  INPUT
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_DATA.
 MODIFY ZTMM_IF_PRICE FROM TABLE IT_IF_PRICE.
 MESSAGE S999 WITH TEXT-S01.
ENDFORM.                    " SAVE_DATA
