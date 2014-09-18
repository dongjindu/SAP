************************************************************************
* Program name: ZEMMGM11_MAT_GROUP_INFO
* Created by  : hj.song
* Created on  : 2003.11.7
* Pattern     : Report 1-1
* Description : Material group information (with Vaatz interface)
*                                                                      *
* Modification Log                                                     *
* Date            Developer        Request No.    Description          *
* 2003.11.07.     hj.song          UD1K902172     Initial Coding       *
*                                                                      *
************************************************************************
*&---------------------------------------------------------------------*
*& Report  ZEMMGM11_MAT_GROUP_INFO                                     *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*
REPORT  zemmgm11_mat_group_info  MESSAGE-ID zmmm.

TABLES : ztmm_magroup, *ztmm_magroup,
         t023,
         t134.

DATA : BEGIN OF it_list OCCURS 0.
        INCLUDE STRUCTURE ztmm_magroup.
DATA :   mark,
       END OF it_list.

DATA :
       save_ok         LIKE  sy-ucomm,
       ok_code         LIKE  sy-ucomm,

       w_loop_cnt      LIKE   sy-loopc,
       w_tcname        LIKE   feld-name,
       w_selection(1),
       w_select_value  LIKE   help_info-fldvalue,

       s_rmmg1-mbrsh     LIKE   rmmg1-mbrsh,
       s_rmmg1-vkorg     LIKE   rmmg1-vkorg,
       s_rmmg1-vtweg     LIKE   rmmg1-vtweg,
       s_mara-profl      LIKE   mara-profl,
       s_mvke-mtpos      LIKE   mvke-mtpos,
       s_mvke-ktgrm      LIKE   mvke-ktgrm,
       s_marc-mtvfp      LIKE   marc-mtvfp,
       s_mara-tragr      LIKE   mara-tragr,
       s_marc-ladgr      LIKE   marc-ladgr,
       s_marc-beskz      LIKE   marc-beskz,
       s_marc-altsl      LIKE   marc-altsl,
       s_marc-sbdkz      LIKE   marc-sbdkz,
       s_mbew-ekalr      LIKE   mbew-ekalr,
       s_klart           LIKE   rmclm-klart.

DATA : wa_help_infos   LIKE   help_info,
       it_dynpselect   LIKE   TABLE OF dselc
                             WITH HEADER LINE,
       it_dynpvaluetab LIKE   TABLE OF dval
                             WITH HEADER LINE.

CONTROLS : tc0100 TYPE TABLEVIEW USING SCREEN 0100.

FIELD-SYMBOLS <tc>  TYPE cxtab_control.



SELECTION-SCREEN BEGIN OF SCREEN 102 AS SUBSCREEN.
SELECT-OPTIONS : s_matkl  FOR  t023-matkl NO-EXTENSION.
PARAMETERS     : p_mtart  LIKE t134-mtart.
SELECTION-SCREEN END OF SCREEN 102.


START-OF-SELECTION.
* read default value for display
  PERFORM read_default_value.
* call screen
  CALL SCREEN 100.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET PF-STATUS '0100'.
  SET TITLEBAR  '0100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  save_ok = ok_code.
  CLEAR ok_code.

  CASE save_ok.
* select basic data
    WHEN 'SEAR'.
      PERFORM select_data.
      REFRESH CONTROL 'TC0100' FROM SCREEN '0100'.
      CALL SCREEN '0100'.
* create db
    WHEN 'CREA'.
      PERFORM check_entry_list.
      PERFORM check_line_selection.
      PERFORM create_data.
* delete db
    WHEN 'DELE'.
      PERFORM check_entry_list.
      PERFORM check_line_selection.
      PERFORM delete_data.
* copy default value
    WHEN 'COPY'.
      PERFORM check_line_selection.
      READ TABLE it_list WITH KEY mark = 'X'.
      ztmm_magroup-matkl = it_list-matkl.
      ztmm_magroup-mtart = it_list-mtart.
*      PERFORM check_required_filed USING ztmm_magroup.
      LOOP AT it_list WHERE mark = 'X'.
        PERFORM copy_data.
      ENDLOOP.
* select all
    WHEN '&ALL'.
      PERFORM select_all_pro USING 'X'.
* deselect all
    WHEN '&SAL'.
      PERFORM select_all_pro USING space.
* ascending
    WHEN '&OUP'.
      PERFORM sort_pro USING 'A'.
* descending
    WHEN '&ODN'.
      PERFORM sort_pro USING 'D'.
* page up, down
    WHEN 'P-' OR 'P--' OR 'P++' OR 'P+'.
      PERFORM page_scroll USING save_ok.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  LOOP_CNT  OUTPUT
*&---------------------------------------------------------------------*
MODULE loop_cnt OUTPUT.

  w_loop_cnt = sy-loopc.

ENDMODULE.                 " LOOP_CNT  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_TC_LINE  OUTPUT
*&---------------------------------------------------------------------*
MODULE set_tc_line OUTPUT.

  CONCATENATE 'TC' sy-dynnr INTO w_tcname.
  ASSIGN (w_tcname) TO <tc>.

  IF     sy-dynnr = '0100'.
    DESCRIBE TABLE it_list LINES <tc>-lines.
  ENDIF.

ENDMODULE.                 " SET_TC_LINE  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_list-MARK  INPUT
*&---------------------------------------------------------------------*
MODULE it_list-mark INPUT.

  it_list-mark = 'X'.

ENDMODULE.                 " IT_list-MARK  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_list_MODIFY  INPUT
*&---------------------------------------------------------------------*
MODULE it_list_modify INPUT.

  MODIFY it_list INDEX tc0100-current_line. " TRANSPORTING mark.

ENDMODULE.                 " IT_list_MODIFY  INPUT
*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
*       select basic data
*----------------------------------------------------------------------*
FORM select_data.

  DATA :
         BEGIN  OF lt_t023  OCCURS 0,
           matkl   LIKE  t023-matkl,
         END    OF lt_t023,
         BEGIN  OF lt_t134  OCCURS 0,
           matkl   LIKE  t023-matkl,
           mtart   LIKE  t134-mtart,
         END    OF lt_t134.

  CLEAR:
         lt_t023[], lt_t023, lt_t134[], lt_t134,
         it_list[], it_list.

* select material group from master table
  SELECT matkl FROM t023
         INTO TABLE lt_t023
        WHERE matkl    IN    s_matkl.
* add material type (just only 'ROH1', 'ERSA', 'NLAG')
  LOOP AT lt_t023.
    IF p_mtart NE ''.
      lt_t134-matkl  =  lt_t023-matkl.
      lt_t134-mtart  =  p_mtart.
      APPEND lt_t134.
    ELSE.
      lt_t134-matkl  =  lt_t023-matkl.
      lt_t134-mtart  =  'ROH1'.
      APPEND lt_t134.
      lt_t134-matkl  =  lt_t023-matkl.
      lt_t134-mtart  =  'ERSA'.
      APPEND lt_t134.
      lt_t134-matkl  =  lt_t023-matkl.
      lt_t134-mtart  =  'NLAG'.
      APPEND lt_t134.
    ENDIF.
  ENDLOOP.
* modify material group infomation from ztable
  LOOP AT lt_t134.
    CLEAR *ztmm_magroup.
    SELECT SINGLE * FROM ztmm_magroup
           INTO *ztmm_magroup
          WHERE matkl  =  lt_t134-matkl
          AND   mtart  =  lt_t134-mtart.
     *ztmm_magroup-matkl  =  lt_t134-matkl.
     *ztmm_magroup-mtart  =  lt_t134-mtart.
    MOVE-CORRESPONDING *ztmm_magroup TO it_list.
    APPEND it_list. CLEAR it_list.
  ENDLOOP.

  IF sy-subrc <> 0.
    MESSAGE s999 WITH text-001.
    LEAVE TO SCREEN sy-dynnr.
  ENDIF.

ENDFORM.                    " SELECT_DATA
*&---------------------------------------------------------------------*
*&      Form  CREATE_DATA
*&---------------------------------------------------------------------*
*       create database
*----------------------------------------------------------------------*
FORM create_data.

  DATA : lt_list  LIKE  TABLE OF ztmm_magroup WITH HEADER LINE.
  CLEAR: lt_list[], lt_list.

  LOOP AT it_list WHERE mark = 'X'.
    MOVE it_list  TO  lt_list.
*    PERFORM check_required_filed USING lt_list.
    COLLECT lt_list. CLEAR lt_list.
    it_list-mark = ''.
    MODIFY it_list TRANSPORTING mark.
  ENDLOOP.

  MODIFY ztmm_magroup FROM TABLE lt_list.
  IF sy-subrc EQ 0.
    MESSAGE s999 WITH text-002.
    LEAVE TO SCREEN sy-dynnr.
  ELSE.
    MESSAGE s999 WITH text-003.
    LEAVE TO SCREEN sy-dynnr.
  ENDIF.

ENDFORM.                    " CREATE_DATA
*&---------------------------------------------------------------------*
*&      Form  COPY_DATA
*&---------------------------------------------------------------------*
*       copy default value
*----------------------------------------------------------------------*
FORM copy_data.

  it_list-mandt   =    sy-mandt.
  it_list-ekgrp   =    ztmm_magroup-ekgrp.  "pur group
*  it_list-disgr   =    ztmm_magroup-disgr.
  it_list-kautb   =    ztmm_magroup-kautb.  "auto po
  it_list-xchpf   =    ztmm_magroup-xchpf.  "batch mag
*  it_list-fabkz   =    ztmm_magroup-fabkz.
*  it_list-dismm   =    ztmm_magroup-dismm.
*  it_list-minbe   =    ztmm_magroup-minbe.
*  it_list-dispo   =    ztmm_magroup-dispo.
*  it_list-disls   =    ztmm_magroup-disls.
*  it_list-rgekz   =    ztmm_magroup-rgekz.
*  it_list-fhori   =    ztmm_magroup-fhori.
  it_list-bklas   =    ztmm_magroup-bklas.  "Valuation class
  it_list-vprsv   =    ztmm_magroup-vprsv.  "Price control indi
*  it_list-lgpro   =    ztmm_magroup-lgpro.
*  it_list-vspvb   =    ztmm_magroup-vspvb.
*  it_list-lgfsb   =    ztmm_magroup-lgfsb.
*  it_list-profl   =    ztmm_magroup-profl.  "MIP/LP/KD
  it_list-mlast   =    ztmm_magroup-mlast.  "Price Deter
*  it_list-zplp1   =    ztmm_magroup-zplp1.  "Planned Price 1
*  it_list-zplp3   =    ztmm_magroup-zplp3.
*  it_list-zpld1   =    ztmm_magroup-zpld1.  "Planned Price 1 Is Valid
*  it_list-zpld3   =    ztmm_magroup-zpld3.
  it_list-mark    =    ' '.

  MODIFY it_list. CLEAR it_list.

ENDFORM.                    " COPY_DATA
*&---------------------------------------------------------------------*
*&      Form  DELETE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_data.

  DATA : lt_list  LIKE  TABLE OF it_list WITH HEADER LINE.
  CLEAR: lt_list[], lt_list.

  LOOP AT it_list WHERE mark = 'X'.
    MOVE it_list  TO  lt_list.
    COLLECT lt_list. CLEAR lt_list.
    it_list = ''.
    MODIFY it_list TRANSPORTING mark.
  ENDLOOP.

  DELETE ztmm_magroup FROM TABLE lt_list.
  IF sy-subrc EQ 0.
    MESSAGE s999 WITH text-004.
    LEAVE TO SCREEN sy-dynnr.
  ELSE.
    MESSAGE s999 WITH text-003.
    LEAVE TO SCREEN sy-dynnr.
  ENDIF.

ENDFORM.                    " DELETE_DATA
*&---------------------------------------------------------------------*
*&      Form  SELECT_ALL_PRO
*&---------------------------------------------------------------------*
*       select(deselect) all
*----------------------------------------------------------------------*
*      -->P_0108   mark
*----------------------------------------------------------------------*
FORM select_all_pro USING    value(p_0108).

  LOOP AT it_list.
    it_list-mark = p_0108.
    MODIFY it_list. CLEAR it_list.
  ENDLOOP.

ENDFORM.                    " SELECT_ALL_PRO
*&---------------------------------------------------------------------*
*&      Form  SORT_PRO
*&---------------------------------------------------------------------*
*       ascending(descending)
*----------------------------------------------------------------------*
*      -->P_0120   ascending(descending) FLAG
*----------------------------------------------------------------------*
FORM sort_pro USING    value(p_0120).

  DATA : w_field  TYPE fieldname,
         f1       TYPE fieldname,
         f2       TYPE fieldname.

  GET CURSOR FIELD w_field.
  CHECK sy-subrc EQ 0.
  SPLIT w_field AT '-'  INTO f1 f2.

  IF     p_0120 = 'A'.
    SORT it_list ASCENDING  BY (f2).
  ELSEIF p_0120 = 'D'.
    SORT it_list DESCENDING BY (f2).
  ENDIF.

ENDFORM.                    " SORT_PRO
*&---------------------------------------------------------------------*
*&      Form  CHECK_ENTRY_LIST
*&---------------------------------------------------------------------*
*       check entry list
*----------------------------------------------------------------------*
FORM check_entry_list.

  DATA : lw_line  TYPE  i.
  CLEAR: lw_line.

  DESCRIBE TABLE it_list LINES lw_line.
  IF lw_line EQ 0.
    MESSAGE s999 WITH text-001.
    LEAVE TO SCREEN sy-dynnr.
  ENDIF.

ENDFORM.                    " CHECK_ENTRY_LIST
*&---------------------------------------------------------------------*
*&      Form  CHECK_LINE_SELECTION
*&---------------------------------------------------------------------*
*       check no selection line.
*----------------------------------------------------------------------*
FORM check_line_selection.

  DATA : lw_mark  TYPE  i.
  CLEAR: lw_mark.

  LOOP AT it_list WHERE mark EQ 'X'.
    ADD 1 TO lw_mark.
  ENDLOOP.

  IF lw_mark EQ 0.
    MESSAGE s999 WITH text-006.
    LEAVE TO SCREEN sy-dynnr.
  ENDIF.

ENDFORM.                    " CHECK_LINE_SELECTION
*&---------------------------------------------------------------------*
*&      Form  PAGE_SCROLL
*&---------------------------------------------------------------------*
*       scrolling page
*----------------------------------------------------------------------*
FORM page_scroll USING    p_save_ok.

  CALL FUNCTION 'LOAN_TABLECONTROL_SCROLLING'
       EXPORTING
            i_ok_code                = p_save_ok
            i_visible_lines_in_table = '15'
       CHANGING
            c_tablecontrol           = tc0100.

ENDFORM.                    " PAGE_SCROLL
*&---------------------------------------------------------------------*
*&      Module  IT_LIST-EKGRP_F4  INPUT
*&---------------------------------------------------------------------*
MODULE it_list-ekgrp_f4 INPUT.

  PERFORM ekgrp_f4 USING it_list-ekgrp.

ENDMODULE.                 " IT_LIST-EKGRP_F4  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_LIST-DISGR_F4  INPUT
*&---------------------------------------------------------------------*
MODULE it_list-disgr_f4 INPUT.

*  PERFORM disgr_f4 USING it_list-disgr.

ENDMODULE.                 " IT_LIST-DISGR_F4  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_LIST-DISMM_F4  INPUT
*&---------------------------------------------------------------------*
MODULE it_list-dismm_f4 INPUT.

*  PERFORM dismm_f4 USING it_list-dismm.

ENDMODULE.                 " IT_LIST-DISMM_F4  INPUT
*&---------------------------------------------------------------------*
*&      Module  it_list-DISPO_F4  INPUT
*&---------------------------------------------------------------------*
*       MRP controller  search help
*----------------------------------------------------------------------*
MODULE it_list-dispo_f4 INPUT.

*  PERFORM dispo_f4 USING it_list-dispo.

ENDMODULE.                 " it_list-DISPO_F4  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_LIST-DISLS_F4  INPUT
*&---------------------------------------------------------------------*
*       lot size  search help
*----------------------------------------------------------------------*
MODULE it_list-disls_f4 INPUT.

*  PERFORM disls_f4 USING it_list-disls.

ENDMODULE.                 " IT_LIST-DISLS_F4  INPUT
*&---------------------------------------------------------------------*
*&      Module  it_list-FHORI_F4  INPUT
*&---------------------------------------------------------------------*
*       schedule margin key  search help
*----------------------------------------------------------------------*
MODULE it_list-fhori_f4 INPUT.

*  PERFORM fhori_f4 USING it_list-fhori.

ENDMODULE.                 " it_list-FHORI_F4  INPUT
*&---------------------------------------------------------------------*
*&      Module  it_list-BKLAS_F4  INPUT
*&---------------------------------------------------------------------*
*       valuation calss  search help
*----------------------------------------------------------------------*
MODULE it_list-bklas_f4 INPUT.

  PERFORM bklas_f4 USING it_list-bklas.

ENDMODULE.                 " it_list-BKLAS_F4  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_LIST-MEINS_F4  INPUT
*&---------------------------------------------------------------------*
MODULE it_list-meins_f4 INPUT.

  PERFORM meins_f4 USING it_list-meins.

ENDMODULE.                 " IT_LIST-MEINS_F4  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_LIST-LGPRO_F4  INPUT
*&---------------------------------------------------------------------*
*       Issue Storage Location  search help
*----------------------------------------------------------------------*
MODULE it_list-lgpro_f4 INPUT.

*  PERFORM lgpro_f4 USING it_list-lgpro.

ENDMODULE.                 " IT_LIST-LGPRO_F4  INPUT
*&---------------------------------------------------------------------*
*&      Module  VSPVB_F4  INPUT
*&---------------------------------------------------------------------*
*       Proposed Supply Area  search help
*----------------------------------------------------------------------*
MODULE it_list-vspvb_f4 INPUT.

*  PERFORM vspvb_f4 USING it_list-vspvb.

ENDMODULE.                 " IT_LIST-VSPVB_F4  INPUT
*&---------------------------------------------------------------------*
*&      Form  CHECK_REQUIRED_FILED
*&---------------------------------------------------------------------*
*       check required fields
*----------------------------------------------------------------------*
FORM check_required_filed
                         USING p_ztmm_magroup LIKE ztmm_magroup.

*  CASE p_ztmm_magroup-mtart.
*    WHEN 'NLAG'.
*      IF p_ztmm_magroup-disgr = '' OR p_ztmm_magroup-dismm = ''
*      OR p_ztmm_magroup-dispo = '' OR p_ztmm_magroup-disls = ''
*      OR p_ztmm_magroup-fhori = '' OR p_ztmm_magroup-vprsv = ''.
*        MESSAGE s055(00). LEAVE TO SCREEN sy-dynnr.
*      ENDIF.
*    WHEN OTHERS.
*      IF p_ztmm_magroup-disgr = '' OR p_ztmm_magroup-dismm = ''
*      OR p_ztmm_magroup-dispo = '' OR p_ztmm_magroup-disls = ''
*      OR p_ztmm_magroup-fhori = '' OR p_ztmm_magroup-bklas = ''
*      OR p_ztmm_magroup-vprsv = ''.
*        MESSAGE s055(00). LEAVE TO SCREEN sy-dynnr.
*      ENDIF.
*  ENDCASE.

ENDFORM.                    " CHECK_REQUIRED_FILED
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE exit INPUT.

  save_ok = ok_code.
  CLEAR ok_code.

  CASE save_ok.
    WHEN 'BACK'.
      LEAVE PROGRAM.
*      SET SCREEN 0. LEAVE SCREEN.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'CANC'.
      LEAVE.
  ENDCASE.

ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Form  read_default_value
*&---------------------------------------------------------------------*
*       read default(fixed) value for screen display
*----------------------------------------------------------------------*
FORM read_default_value.
* This values are default(fixed) value when material master creation
* using F/M 'zfmm_material_master'
* Latter part this vales would be chaned and appended
* !! and then you have to append fields on screen 0100 also
* F/M 'zfmm_material_master' have to modify....
* if it would bother, u can make maintainence view ..whatever u are^^

*indus sector
  s_rmmg1-mbrsh = 'A'.
**Sales organization
*  s_rmmg1-vkorg = 'D100'.
**Distribution channel
*  s_rmmg1-vtweg = '40'.
**KD/LP
*  s_mara-profl  = 'V'.
**item cat grp
*  s_mvke-mtpos  = 'BANS'.
**acct assignment grp
*  s_mvke-ktgrm  = '40'.
**availavility check
*  s_marc-mtvfp  = '02'.
**trans grp
*  s_mara-tragr  = '0001'.
**loading grp
*  s_marc-ladgr  = '0001'.
**Procurement Type
*  s_marc-beskz  = 'F'.
**Selection method
*  s_marc-altsl  = '2'.
**Individual/coll
*  s_marc-sbdkz  = '2'.
*with quantity structure(cost est.w/QS)
  s_mbew-ekalr  = 'X'.
*class type
  s_klart       = '001'.

ENDFORM.                    " read_default_value
*&---------------------------------------------------------------------*
*&      Form  ekgrp_f4
*&---------------------------------------------------------------------*
*       pur.gorup search help
*----------------------------------------------------------------------*
FORM ekgrp_f4 USING    p_ekgrp.

  CLEAR : wa_help_infos,
          it_dynpselect[], it_dynpselect,
          it_dynpvaluetab[], it_dynpvaluetab,
          w_selection, w_select_value.
* import parameter
  wa_help_infos-call         =  'T'.
  wa_help_infos-call         =  'F'.
  wa_help_infos-tabname      =  'MARC'.
  wa_help_infos-fieldname    =  'EKGRP'.
  wa_help_infos-spras        =  sy-langu.

  CALL FUNCTION 'DD_SHLP_CALL_FROM_DYNP'
    EXPORTING
      help_infos         = wa_help_infos
    IMPORTING
      selection          = w_selection
      select_value       = w_select_value
*      RSMDY_RET          =
    TABLES
      dynpselect         = it_dynpselect
      dynpvaluetab       = it_dynpvaluetab.
  CHECK w_selection EQ 'X'.
  p_ekgrp = w_select_value.

ENDFORM.                                                    " ekgrp_f4
*&---------------------------------------------------------------------*
*&      Module  ZTMM_MAGROUP-EKGRP_F4  INPUT
*&---------------------------------------------------------------------*
MODULE ztmm_magroup-ekgrp_f4 INPUT.

  PERFORM ekgrp_f4 USING ztmm_magroup-ekgrp.

ENDMODULE.                 " ZTMM_MAGROUP-EKGRP_F4  INPUT
*&---------------------------------------------------------------------*
*&      Form  disgr_f4
*&---------------------------------------------------------------------*
*        MRP group  search help
*----------------------------------------------------------------------*
FORM disgr_f4 USING    p_disgr.

  CLEAR : wa_help_infos,
        it_dynpselect[], it_dynpselect,
        it_dynpvaluetab[], it_dynpvaluetab,
        w_selection, w_select_value.
* import parameter
  wa_help_infos-call         =  'T'.
  wa_help_infos-call         =  'F'.
  wa_help_infos-tabname      =  'MARC'.
  wa_help_infos-fieldname    =  'DISGR'.
  wa_help_infos-spras        =  sy-langu.


  CALL FUNCTION 'DD_SHLP_CALL_FROM_DYNP'
    EXPORTING
      help_infos         = wa_help_infos
   IMPORTING
     selection          = w_selection
     select_value       = w_select_value
*     RSMDY_RET          =
    TABLES
      dynpselect         = it_dynpselect
      dynpvaluetab       = it_dynpvaluetab.
  CHECK w_selection EQ 'X'.
  p_disgr = w_select_value.

ENDFORM.                                                    " disgr_f4
*&---------------------------------------------------------------------*
*&      Module  ZTMM_MAGROUP-DISGR_F4  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ztmm_magroup-disgr_f4 INPUT.

*  PERFORM disgr_f4 USING ztmm_magroup-disgr.

ENDMODULE.                 " ZTMM_MAGROUP-DISGR_F4  INPUT
*&---------------------------------------------------------------------*
*&      Form  dismm_f4
*&---------------------------------------------------------------------*
*        MRP type  search help
*----------------------------------------------------------------------*
FORM dismm_f4 USING    p_dismm.

  CLEAR : wa_help_infos,
        it_dynpselect[], it_dynpselect,
        it_dynpvaluetab[], it_dynpvaluetab,
        w_selection, w_select_value.
* import parameter
  wa_help_infos-call         =  'T'.
  wa_help_infos-call         =  'F'.
  wa_help_infos-tabname      =  'MARC'.
  wa_help_infos-fieldname    =  'DISMM'.
  wa_help_infos-spras        =  sy-langu.


  CALL FUNCTION 'DD_SHLP_CALL_FROM_DYNP'
    EXPORTING
      help_infos         = wa_help_infos
   IMPORTING
     selection          = w_selection
     select_value       = w_select_value	
*     RSMDY_RET          =
    TABLES
      dynpselect         = it_dynpselect
      dynpvaluetab       = it_dynpvaluetab.
  CHECK w_selection EQ 'X'.
  p_dismm = w_select_value.

ENDFORM.                                                    " dismm_f4
*&---------------------------------------------------------------------*
*&      Module  ZTMM_MAGROUP-DISMM_F4  INPUT
*&---------------------------------------------------------------------*
MODULE ztmm_magroup-dismm_f4 INPUT.

*  PERFORM dismm_f4 USING ztmm_magroup-dismm.

ENDMODULE.                 " ZTMM_MAGROUP-DISMM_F4  INPUT
*&---------------------------------------------------------------------*
*&      Form  meins_f4
*&---------------------------------------------------------------------*
*       UOM
*----------------------------------------------------------------------*
FORM meins_f4 USING    p_meins.

  CLEAR : wa_help_infos,
          it_dynpselect[], it_dynpselect,
          it_dynpvaluetab[], it_dynpvaluetab,
          w_selection, w_select_value.
* import parameter
  wa_help_infos-call         =  'T'.
  wa_help_infos-call         =  'F'.
  wa_help_infos-tabname      =  'MARA'.
  wa_help_infos-fieldname    =  'MEINS'.
  wa_help_infos-spras        =  sy-langu.


  CALL FUNCTION 'DD_SHLP_CALL_FROM_DYNP'
    EXPORTING
      help_infos         = wa_help_infos
   IMPORTING
     selection          = w_selection
     select_value       = w_select_value	
*     RSMDY_RET          =
    TABLES
      dynpselect         = it_dynpselect
      dynpvaluetab       = it_dynpvaluetab.
  CHECK w_selection EQ 'X'.
  p_meins = w_select_value.

ENDFORM.                                                    " meins_f4
*&---------------------------------------------------------------------*
*&      Module  ZTMM_MAGROUP-MEINS_F4  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ztmm_magroup-meins_f4 INPUT.

  PERFORM meins_f4 USING ztmm_magroup-meins.

ENDMODULE.                 " ZTMM_MAGROUP-MEINS_F4  INPUT
*&---------------------------------------------------------------------*
*&      Form  dispo_f4
*&---------------------------------------------------------------------*
FORM dispo_f4 USING    p_dispo.

  CLEAR : wa_help_infos,
          it_dynpselect[], it_dynpselect,
          it_dynpvaluetab[], it_dynpvaluetab,
          w_selection, w_select_value.
* import parameter
  wa_help_infos-call         =  'T'.
  wa_help_infos-call         =  'F'.
  wa_help_infos-tabname      =  'MARC'.
  wa_help_infos-fieldname    =  'DISPO'.
  wa_help_infos-spras        =  sy-langu.


  CALL FUNCTION 'DD_SHLP_CALL_FROM_DYNP'
    EXPORTING
      help_infos         = wa_help_infos
   IMPORTING
     selection          = w_selection
     select_value       = w_select_value
*     RSMDY_RET          =
    TABLES
      dynpselect         = it_dynpselect
      dynpvaluetab       = it_dynpvaluetab.
  CHECK w_selection EQ 'X'.
  p_dispo = w_select_value.

ENDFORM.                                                    " dispo_f4
*&---------------------------------------------------------------------*
*&      Module  ZTMM_MAGROUP-DISPO_F4  INPUT
*&---------------------------------------------------------------------*
MODULE ztmm_magroup-dispo_f4 INPUT.

*  PERFORM dispo_f4 USING ztmm_magroup-dispo.

ENDMODULE.                 " ZTMM_MAGROUP-DISPO_F4  INPUT
*&---------------------------------------------------------------------*
*&      Form  disls_f4
*&---------------------------------------------------------------------*
FORM disls_f4 USING    p_disls.

  CLEAR : wa_help_infos,
          it_dynpselect[], it_dynpselect,
          it_dynpvaluetab[], it_dynpvaluetab,
          w_selection, w_select_value.
* import parameter
  wa_help_infos-call         =  'T'.
  wa_help_infos-call         =  'F'.
  wa_help_infos-tabname      =  'MARC'.
  wa_help_infos-fieldname    =  'DISLS'.
  wa_help_infos-spras        =  sy-langu.


  CALL FUNCTION 'DD_SHLP_CALL_FROM_DYNP'
    EXPORTING
      help_infos         = wa_help_infos
   IMPORTING
     selection          = w_selection
     select_value       = w_select_value
*     RSMDY_RET          =
    TABLES
      dynpselect         = it_dynpselect
      dynpvaluetab       = it_dynpvaluetab.
  CHECK w_selection EQ 'X'.
  p_disls = w_select_value.

ENDFORM.                                                    " disls_f4
*&---------------------------------------------------------------------*
*&      Module  ZTMM_MAGROUP-DISLS_F4  INPUT
*&---------------------------------------------------------------------*
MODULE ztmm_magroup-disls_f4 INPUT.

*  PERFORM disls_f4 USING ztmm_magroup-disls.

ENDMODULE.                 " ZTMM_MAGROUP-DISLS_F4  INPUT
*&---------------------------------------------------------------------*
*&      Form  fhori_f4
*&---------------------------------------------------------------------*
FORM fhori_f4 USING    p_fhori.

  CLEAR : wa_help_infos,
          it_dynpselect[], it_dynpselect,
          it_dynpvaluetab[], it_dynpvaluetab,
          w_selection, w_select_value.
* import parameter
  wa_help_infos-call         =  'T'.
  wa_help_infos-call         =  'F'.
  wa_help_infos-tabname      =  'MARC'.
  wa_help_infos-fieldname    =  'FHORI'.
  wa_help_infos-spras        =  sy-langu.


  CALL FUNCTION 'DD_SHLP_CALL_FROM_DYNP'
    EXPORTING
      help_infos         = wa_help_infos
   IMPORTING
     selection          = w_selection
     select_value       = w_select_value	
*     RSMDY_RET          =
    TABLES
      dynpselect         = it_dynpselect
      dynpvaluetab       = it_dynpvaluetab.
  CHECK w_selection EQ 'X'.
  p_fhori = w_select_value.

ENDFORM.                                                    " fhori_f4
*&---------------------------------------------------------------------*
*&      Module  ZTMM_MAGROUP-FHORI_F4  INPUT
*&---------------------------------------------------------------------*
MODULE ztmm_magroup-fhori_f4 INPUT.

*  PERFORM fhori_f4 USING ztmm_magroup-fhori.

ENDMODULE.                 " ZTMM_MAGROUP-FHORI_F4  INPUT
*&---------------------------------------------------------------------*
*&      Form  bklas_f4
*&---------------------------------------------------------------------*
FORM bklas_f4 USING    p_bklas.

  CLEAR : wa_help_infos,
          it_dynpselect[], it_dynpselect,
          it_dynpvaluetab[], it_dynpvaluetab,
          w_selection, w_select_value.
* import parameter
  wa_help_infos-call         =  'T'.
  wa_help_infos-call         =  'F'.
  wa_help_infos-tabname      =  'MBEW'.
  wa_help_infos-fieldname    =  'BKLAS'.
  wa_help_infos-spras        =  sy-langu.


  CALL FUNCTION 'DD_SHLP_CALL_FROM_DYNP'
    EXPORTING
      help_infos         = wa_help_infos
   IMPORTING
     selection          = w_selection
     select_value       = w_select_value	
*     RSMDY_RET          =
    TABLES
      dynpselect         = it_dynpselect
      dynpvaluetab       = it_dynpvaluetab.
  CHECK w_selection EQ 'X'.
  p_bklas = w_select_value.

ENDFORM.                                                    " bklas_f4
*&---------------------------------------------------------------------*
*&      Module  ZTMM_MAGROUP-BKLAS_F4  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ztmm_magroup-bklas_f4 INPUT.

  PERFORM bklas_f4 USING ztmm_magroup-bklas.

ENDMODULE.                 " ZTMM_MAGROUP-BKLAS_F4  INPUT
*&---------------------------------------------------------------------*
*&      Form  lgpro_f4
*&---------------------------------------------------------------------*
FORM lgpro_f4 USING    p_lgpro.

  CLEAR : wa_help_infos,
          it_dynpselect[], it_dynpselect,
          it_dynpvaluetab[], it_dynpvaluetab,
          w_selection, w_select_value.
* import parameter
  wa_help_infos-call         =  'T'.
  wa_help_infos-call         =  'F'.
  wa_help_infos-tabname      =  'MARC'.
  wa_help_infos-fieldname    =  'LGPRO'.
  wa_help_infos-spras        =  sy-langu.


  CALL FUNCTION 'DD_SHLP_CALL_FROM_DYNP'
    EXPORTING
      help_infos         = wa_help_infos
   IMPORTING
     selection          = w_selection
     select_value       = w_select_value	
*     RSMDY_RET          =
    TABLES
      dynpselect         = it_dynpselect
      dynpvaluetab       = it_dynpvaluetab.
  CHECK w_selection EQ 'X'.
  p_lgpro = w_select_value.

ENDFORM.                                                    " lgpro_f4
*&---------------------------------------------------------------------*
*&      Module  ZTMM_MAGROUP-LGPRO_F4  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ztmm_magroup-lgpro_f4 INPUT.

*  PERFORM lgpro_f4 USING ztmm_magroup-lgpro.

ENDMODULE.                 " ZTMM_MAGROUP-LGPRO_F4  INPUT
*&---------------------------------------------------------------------*
*&      Form  vspvb_f4
*&---------------------------------------------------------------------*
FORM vspvb_f4 USING    p_vspvb.

  CLEAR : wa_help_infos,
          it_dynpselect[], it_dynpselect,
          it_dynpvaluetab[], it_dynpvaluetab,
          w_selection, w_select_value.
* import parameter
  wa_help_infos-call         =  'T'.
  wa_help_infos-call         =  'F'.
  wa_help_infos-tabname      =  'MARC'.
  wa_help_infos-fieldname    =  'VSPVB'.
  wa_help_infos-spras        =  sy-langu.


  CALL FUNCTION 'DD_SHLP_CALL_FROM_DYNP'
    EXPORTING
      help_infos         = wa_help_infos
   IMPORTING
     selection          = w_selection
     select_value       = w_select_value	
*     RSMDY_RET          =
    TABLES
      dynpselect         = it_dynpselect
      dynpvaluetab       = it_dynpvaluetab.
  CHECK w_selection EQ 'X'.
  p_vspvb = w_select_value.

ENDFORM.                                                    " vspvb_f4
*&---------------------------------------------------------------------*
*&      Module  ZTMM_MAGROUP-VSPVB_F4  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ztmm_magroup-vspvb_f4 INPUT.

*  PERFORM vspvb_f4 USING ztmm_magroup-vspvb.

ENDMODULE.                 " ZTMM_MAGROUP-VSPVB_F4  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_LIST-LGFSB_F4  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE it_list-lgfsb_f4 INPUT.

*  PERFORM lgfsb_f4 USING it_list-lgfsb.

ENDMODULE.                 " IT_LIST-LGFSB_F4  INPUT
*&---------------------------------------------------------------------*
*&      Form  lgfsb_f4
*&---------------------------------------------------------------------*
FORM lgfsb_f4 USING    p_lgfsb.

  CLEAR : wa_help_infos,
          it_dynpselect[], it_dynpselect,
          it_dynpvaluetab[], it_dynpvaluetab,
          w_selection, w_select_value.
* import parameter
  wa_help_infos-call         =  'T'.
  wa_help_infos-call         =  'F'.
  wa_help_infos-tabname      =  'MARC'.
  wa_help_infos-fieldname    =  'LGFSB'.
  wa_help_infos-spras        =  sy-langu.


  CALL FUNCTION 'DD_SHLP_CALL_FROM_DYNP'
    EXPORTING
      help_infos         = wa_help_infos
   IMPORTING
     selection          = w_selection
     select_value       = w_select_value	
*     RSMDY_RET          =
    TABLES
      dynpselect         = it_dynpselect
      dynpvaluetab       = it_dynpvaluetab.
  CHECK w_selection EQ 'X'.
  p_lgfsb = w_select_value.

ENDFORM.                                                    " lgfsb_f4
*&---------------------------------------------------------------------*
*&      Module  ZTMM_MAGROUP-LGFSB_F4  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ztmm_magroup-lgfsb_f4 INPUT.

*  PERFORM lgfsb_f4 USING ztmm_magroup-lgfsb.

ENDMODULE.                 " ZTMM_MAGROUP-LGFSB_F4  INPUT
*&---------------------------------------------------------------------*
*&      Module  it_list-PROFL_f4  INPUT
*&---------------------------------------------------------------------*
*       MIP/LP/KD
*----------------------------------------------------------------------*
MODULE it_list-profl_f4 INPUT.

*  PERFORM profl_f4 USING it_list-profl.

ENDMODULE.                 " it_list-PROFL_f4  INPUT
*&---------------------------------------------------------------------*
*&      Form  profl_f4
*&---------------------------------------------------------------------*
*       MIP/LP/KD
*----------------------------------------------------------------------*
FORM profl_f4 USING    p_profl.

  CLEAR : wa_help_infos,
         it_dynpselect[], it_dynpselect,
         it_dynpvaluetab[], it_dynpvaluetab,
         w_selection, w_select_value.
* import parameter
  wa_help_infos-call         =  'T'.
  wa_help_infos-call         =  'F'.
  wa_help_infos-tabname      =  'MARA'.
  wa_help_infos-fieldname    =  'PROFL'.
  wa_help_infos-spras        =  sy-langu.


  CALL FUNCTION 'DD_SHLP_CALL_FROM_DYNP'
    EXPORTING
      help_infos         = wa_help_infos
   IMPORTING
     selection          = w_selection
     select_value       = w_select_value	
*     RSMDY_RET          =
    TABLES
      dynpselect         = it_dynpselect
      dynpvaluetab       = it_dynpvaluetab.
  CHECK w_selection EQ 'X'.
  p_profl = w_select_value.

ENDFORM.                                                    " profl_f4
*&---------------------------------------------------------------------*
*&      Module  ztmm_magroup-PROFL_f4  INPUT
*&---------------------------------------------------------------------*
*       MIP/LP/KD
*----------------------------------------------------------------------*
MODULE ztmm_magroup-profl_f4 INPUT.

*  PERFORM profl_f4 USING ztmm_magroup-profl.

ENDMODULE.                 " ztmm_magroup-PROFL_f4  INPUT
*&---------------------------------------------------------------------*
*&      Module  it_list-mlast_f4  INPUT
*&---------------------------------------------------------------------*
*       Price Determination
*----------------------------------------------------------------------*
MODULE it_list-mlast_f4 INPUT.

  PERFORM mlast_f4 USING it_list-mlast.

ENDMODULE.                 " it_list-mlast_f4  INPUT
*&---------------------------------------------------------------------*
*&      Form  mlast_f4
*&---------------------------------------------------------------------*
*       Price Determination
*----------------------------------------------------------------------*
FORM mlast_f4 USING    p_mlast.

  CLEAR : wa_help_infos,
         it_dynpselect[], it_dynpselect,
         it_dynpvaluetab[], it_dynpvaluetab,
         w_selection, w_select_value.
* import parameter
  wa_help_infos-call         =  'T'.
  wa_help_infos-call         =  'F'.
  wa_help_infos-tabname      =  'MBEW'.
  wa_help_infos-fieldname    =  'MLAST'.
  wa_help_infos-spras        =  sy-langu.


  CALL FUNCTION 'DD_SHLP_CALL_FROM_DYNP'
    EXPORTING
      help_infos         = wa_help_infos
   IMPORTING
     selection          = w_selection
     select_value       = w_select_value	
*     RSMDY_RET          =
    TABLES
      dynpselect         = it_dynpselect
      dynpvaluetab       = it_dynpvaluetab.
  CHECK w_selection EQ 'X'.
  p_mlast = w_select_value.

ENDFORM.                                                    " mlast_f4
*&---------------------------------------------------------------------*
*&      Module  ztmm_magroup-mlast_f4  INPUT
*&---------------------------------------------------------------------*
*       Price Determination
*----------------------------------------------------------------------*
MODULE ztmm_magroup-mlast_f4 INPUT.

  PERFORM mlast_f4 USING ztmm_magroup-mlast.

ENDMODULE.                 " ztmm_magroup-mlast_f4  INPUT
