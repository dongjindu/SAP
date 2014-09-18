REPORT ZPPR_FSC_VIN_BYBODY.
******************************************************************
*  Date        Developer        Request     Description
* 12/18/2006   Manju            UD1K930012  Initial Coding
******************************************************************


********************************************************************
Tables : AUSP.

type-pools : slis.

*********************************************************************

**************************************************************
* Declaration
data : it_ausp like ausp occurs 0  with header line  .

DATA : w_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
       w_eventcat TYPE slis_t_event WITH HEADER LINE,
       w_selfield TYPE slis_selfield,
       w_sortcat  TYPE slis_t_sortinfo_alv WITH HEADER LINE,
       wa_repid like sy-repid.

DATA: gt_fieldcat TYPE slis_t_fieldcat_alv,
      gs_layout   TYPE slis_layout_alv,
      gt_sp_group TYPE slis_t_sp_group_alv,
      gt_events   TYPE slis_t_event,
      gt_sorts    TYPE slis_t_sortinfo_alv WITH HEADER LINE,
      gs_prnt     TYPE slis_print_alv.

data : begin of wa_out occurs 0,
         objek like ausp-objek,
         P_MY  like ausp-ATWRT,
         P_DC  like ausp-ATWRT,
         P_MI  like ausp-ATWRT,
         P_OCN like ausp-ATWRT,
         P_VIN like ausp-ATWRT,
        end of wa_out.
data : begin of it_out occurs 0,
         objek like ausp-objek,
         P_SPCODE like ausp-ATWRT,
         P_VIN like ausp-ATWRT,
        end of it_out.


DATA: L_ATINN1 LIKE CABN-ATINN,
      L_ATINN2 LIKE CABN-ATINN,
      L_ATINN3 LIKE CABN-ATINN,
      L_ATINN4 LIKE CABN-ATINN,
      L_ATINN5 LIKE CABN-ATINN,
      L_ATINN6 LIKE CABN-ATINN.

**************************************************************


**************************************************************
* Selection Screen
**************************************************************

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-002.

select-options : s_OBJEK  for AUSP-OBJEK obligatory.

selection-screen end of block b1.

**************************************************************
* Start of selection
**************************************************************

start-of-selection.

* Select Data
  Perform select_data.


**************************************************************
* End of Selection
**************************************************************
end-of-selection.

  perform display_data.
*&---------------------------------------------------------------------*
*&      Form  select_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_data.

  perform get_atinn.

  select * into table it_ausp from ausp
      where objek  in s_objek and
            ATINN in (L_ATINN1,L_ATINN2,L_ATINN3,L_ATINN4,L_ATINN5)
and            KLART  eq '002'.

sort it_ausp by objek.

loop at it_ausp.
  move it_ausp-objek to it_out-objek.

  case it_ausp-ATINN.

  when L_ATINN1.
   move it_ausp-ATWRT to wa_out-P_MY.

  when L_ATINN2.
   move it_ausp-ATWRT to wa_out-P_DC.

  when L_ATINN3.
   move it_ausp-ATWRT to wa_out-P_MI.

  when L_ATINN4.
   move it_ausp-ATWRT to wa_out-P_OCN.

  when L_ATINN5.
   move it_ausp-ATWRT to it_out-P_VIN.

  endcase.

  at end of objek.
  concatenate wa_out-P_MY wa_out-P_DC wa_out-P_MI into it_out-P_SPCODE.
  concatenate  it_out-P_SPCODE wa_out-P_OCN into it_out-P_SPCODE
separated by space.
  append  it_out.
  clear :it_out, wa_out.
  endat.

endloop.

ENDFORM.                    " select_data
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data.

  Perform build_fieldcatalog.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
         i_callback_program       = wa_repid
*         i_callback_pf_status_set = 'ALV_EVENT_PF_STATUS_SET'
*         i_callback_user_command  = 'ALV_EVENT_USER_COMMAND'
         is_layout                = gs_layout
         it_fieldcat              = gt_fieldcat[]
         it_special_groups        = gt_sp_group[]
         it_sort                  = gt_sorts[]
*         i_save                  = wa_var_save
*         is_variant              = wa_var
         it_events                =  w_eventcat[]
         is_print                 = gs_prnt
    TABLES
         t_outtab                 = it_out.

ENDFORM.                    " display_data
*&---------------------------------------------------------------------*
*&      Form  build_fieldcatalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcatalog.


  PERFORM field_setting TABLES gt_fieldcat USING :
    'OBJEK'  'Body Serial Number' '10' 'X' 'L'  ' '  ' '  '  ' ' '  ' ',
*    'P_MY'   'MODEL YEAR'         '20' ' ' 'L'  ' '  ' '  '  ' ' '  ' '
*,
*    'P_DC'   'Destination Code'  '20' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
*    'P_MI'   'MI'               '20' '' 'L'  ' '  ' '  '  ' ' '  ' ',
*    'P_OCN'  'OCN'              '20'  '' 'L'  ' '  ' '  '  ' ' '  ' ',
    'P_SPCODE'  'FULL SPEC CODE'  '30'  '' 'L'  ' '  ' '  '  ' ' '  ' ',
    'P_VIN'  'VIN Number'       '30'  '' 'L'  ' '  ' '  '  ' ' '  ' '.


ENDFORM.                    " build_fieldcatalog
*&---------------------------------------------------------------------*
*&      Form  field_setting
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCAT  text
*      -->P_0179   text
*      -->P_0180   text
*      -->P_0181   text
*      -->P_0182   text
*      -->P_0183   text
*      -->P_0184   text
*      -->P_0185   text
*      -->P_0186   text
*      -->P_0187   text
*      -->P_0188   text
*----------------------------------------------------------------------*
FORM field_setting TABLES   p_fieldcat_t LIKE gt_fieldcat USING
                             p_fieldname       " FIELD name
                                  p_title           " field titlw
                                  p_outputlen       " length
                                  p_key             "
                                  p_just            "
                                  p_noout           "
                                  p_round           "
                                  p_cfield          " currency field nam
                                  p_qfield          " quantity field nam
                                  p_dosum  .         " make sum


  DATA: ls_fieldcat TYPE slis_fieldcat_alv.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname  = p_fieldname.
*  ls_fieldcat-seltext_s = p_title.
*  ls_fieldcat-seltext_m = p_title.
  ls_fieldcat-seltext_l  = p_title.
  ls_fieldcat-outputlen  = p_outputlen.
  ls_fieldcat-key        = p_key.
  ls_fieldcat-just       = p_just.
  ls_fieldcat-edit       = ''.   "p_edit.
  ls_fieldcat-no_out     = p_noout.
  ls_fieldcat-decimals_out   = p_round.
*  ls_fieldcat-cfieldname = p_cfield.
  ls_fieldcat-currency   = p_cfield.
  ls_fieldcat-qfieldname = p_qfield.
  ls_fieldcat-do_sum     = p_dosum.

  APPEND ls_fieldcat TO gt_fieldcat.


ENDFORM.                    " field_setting
*&---------------------------------------------------------------------*
*&      Form  get_atinn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_atinn.


  SELECT SINGLE ATINN
         INTO L_ATINN1
         FROM CABN
         WHERE ATNAM = 'P_MODEL_YEAR'.

  SELECT SINGLE ATINN
        INTO L_ATINN2
        FROM CABN
        WHERE ATNAM = 'P_DESTINATION_CODE'.


  SELECT SINGLE ATINN
         INTO L_ATINN3
         FROM CABN
         WHERE ATNAM = 'P_MI'.

  SELECT SINGLE ATINN
         INTO L_ATINN4
         FROM CABN
         WHERE ATNAM = 'P_OCN'.


  SELECT SINGLE ATINN
           INTO L_ATINN5
           FROM CABN
           WHERE ATNAM = 'P_VIN'.

ENDFORM.                    " get_atinn
