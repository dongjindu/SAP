************************************************************************
* Program name: ZEMMGM18_VIEW_EXTEND
* Created by  : hj.song
* Created on  : 2004.02.09
* Pattern     : report 1-1
* Description : Material master view extend(mass)
*                                                                      *
* Modification Log                                                     *
* Date            Developer        Request No.    Description          *
* 2004.02.09.     hj.song                         Initial Coding       *
* 04/12/2005      Shiva            UD1K915510     Restrict the selection
*                                               only to 'ROH1' & 'ERSA'.
* 10/20/2006      Haseeb           UD1K922649     Display Mn lot zise *
*                                                  (marc-bstmi) and
*                                                  Rounding order
*  quantity(marc-bstrf) in MM02 views. Added two columns to Tab control
*   in screen.
************************************************************************
REPORT  zemmgm18_view_extend  MESSAGE-ID zmmm.

TABLES : t001w,
         mara,
         marc,
         rmclf,
         ztmm_magroup,
         ztmm_view_ext.

DATA : BEGIN OF it_view OCCURS 0,
         matnr  LIKE  mara-matnr,
         maktx  LIKE  makt-maktx,
         meins  LIKE  mara-meins,
         werks  LIKE  marc-werks,
         mtart  LIKE  mara-mtart,
         matkl  LIKE  mara-matkl,
         class  LIKE  rmclf-class,
         ernam  LIKE  mara-ernam,
         dismm  LIKE  marc-dismm,  "mrp type
         disgr  LIKE  marc-disgr,  "mrp group
         dispo  LIKE  marc-dispo,  "mrp controller
         minbe  LIKE  marc-minbe,  "reorder point
         disls  LIKE  marc-disls,  "lot size
         beskz  LIKE  marc-beskz,  "procurement
         rgekz  LIKE  marc-rgekz,  "backflush
         lgpro  LIKE  marc-lgpro,  "Issue Storage Location
         vspvb  LIKE  marc-vspvb,  "Supply Area
         lgfsb  LIKE  marc-lgfsb,  "s/loc for ep
         plifz  LIKE  marc-plifz,  "plnd del time
         fhori  LIKE  marc-fhori,  "Scheduling Margin Key for Floats
         mtvfp  LIKE  marc-mtvfp,  "avail check
         altsl  LIKE  marc-altsl,  "selection method
         sbdkz  LIKE  marc-sbdkz,  "indi/coll
         wzeit  LIKE  marc-wzeit,  "tot repl laed time
*Start 10/20/2006      Haseeb           UD1K922649
         bstmi  LIKE  marc-bstmi,
         bstrf  LIKE  marc-bstrf,
*End   10/20/2006      Haseeb           UD1K922649
         mark,
       END OF it_view,

       it_ztmm_view_ext  LIKE  TABLE OF ztmm_view_ext
                         WITH HEADER LINE,

       wa_ztmm_magroup   LIKE ztmm_magroup,
       wa_ztmm_view_ext  LIKE ztmm_view_ext.

* BAPI Structure
DATA: wa_head       LIKE bapimathead, "Header with control information
      wa_mara       LIKE bapi_mara,   "client-specific material DATA
      wa_marax      LIKE bapi_marax,  "information on update for client
      wa_marc       LIKE bapi_marc,   "plant-specific material DATA
      wa_marcx      LIKE bapi_marcx,  "information on update for plant
      wa_mpop       LIKE bapi_mpop,   "forecast parameters
      wa_mpopx      LIKE bapi_mpopx,  "info on update for forecast
      wa_mpgd       LIKE bapi_mpgd,   "planning DATA
      wa_mpgdx      LIKE bapi_mpgdx,  "info on update for planning
      wa_mbew       LIKE bapi_mbew,   "valuation DATA
      wa_mbewx      LIKE bapi_mbewx,  "information update for valuation
      wa_mvke       LIKE bapi_mvke,   "sales DATA
      wa_mvkex      LIKE bapi_mvkex,  "information on update for sales
      wa_return     LIKE bapiret2.
DATA: it_makt       LIKE TABLE OF bapi_makt
                                  WITH HEADER LINE,
      it_mlan       LIKE TABLE OF bapi_mlan
                                  WITH HEADER LINE,
      it_mess       LIKE TABLE OF bapi_matreturn2
                                  WITH HEADER LINE,
      it_mltx       LIKE TABLE OF bapi_mltx
                                  WITH HEADER LINE.


DATA :
       save_ok         LIKE  sy-ucomm,
       ok_code         LIKE  sy-ucomm,

       w_loop_cnt      LIKE   sy-loopc,
       w_tcname        LIKE   feld-name,
       w_selection(1),
       w_select_value  LIKE   help_info-fldvalue.

DATA : wa_help_infos   LIKE   help_info,
       it_dynpselect   LIKE   TABLE OF dselc
                             WITH HEADER LINE,
       it_dynpvaluetab LIKE   TABLE OF dval
                             WITH HEADER LINE.

DATA : w_mtart_flg(1).


CONTROLS : tc0100 TYPE TABLEVIEW USING SCREEN 0100.

FIELD-SYMBOLS <tc>  TYPE cxtab_control.

** ------------------------------
**    ALV (In case of ALV OOP)
** ------------------------------
* Variables for ALV
DATA: it_alvoop_fcat TYPE lvc_t_fcat.

** ------------------------------
**    ALV (In case of ALV FM use)
** ------------------------------
** --> TYPE Declaration
TYPE-POOLS: slis.

DATA: it_alvfm_fcat   TYPE slis_t_fieldcat_alv,
      it_alvfm_sort   TYPE slis_t_sortinfo_alv,
      it_alvfm_event  TYPE slis_t_event.

DATA: BEGIN OF wa_outtab,
       matnr      LIKE mara-matnr,
       bb_msgtyp  LIKE ztlog-bb_msgtyp,
       ba_message LIKE ztlog-ba_message,
      END OF wa_outtab.
DATA: it_outtab LIKE TABLE OF wa_outtab.
**** Constants&Vars for Number range object ****************************
CONSTANTS: c_nro_nr_00   VALUE '00' LIKE inri-nrrangenr. "Header Part
CONSTANTS: c_nro_nr_01   VALUE '01' LIKE inri-nrrangenr. "Item Part
CONSTANTS: c_nro_nr_09   VALUE '09' LIKE inri-nrrangenr. "App. Doc no.
* Number range object
DATA:      w_nro_object  VALUE 'ZMMNRO0002'
                               LIKE inri-object. "NRO for ZBDCMSGCOLL
* Number_Get_Next
DATA:      w_nro_number  TYPE num10.      " Same type of w_nro_object

DATA: w_zdocno TYPE num10.       "App. Doc. No.



SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS :    s_matnr  FOR   mara-matnr,
                    s_werks  FOR   t001w-werks.
SELECTION-SCREEN END OF BLOCK b1.

PARAMETERS     :    p_mtart  LIKE  mara-mtart.
SELECT-OPTIONS :    s_matkl  FOR   mara-matkl,
                    s_ernam  FOR   mara-ernam.
PARAMETERS     :    p_class  LIKE  rmclf-class.

********************************************************************
********************************************************************


START-OF-SELECTION.
  PERFORM set_parameters.
  PERFORM get.

END-OF-SELECTION.
  CALL SCREEN '0100'.

*&---------------------------------------------------------------------*
*&      Form  set_parameters
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_parameters.

  IF p_mtart EQ ''.
    w_mtart_flg = 'X'.
  ELSE.
    IF p_mtart NE 'ROH1' and p_mtart NE 'ERSA'.
      MESSAGE e031.
    ENDIF.
  ENDIF.

ENDFORM.                    " set_parameters
*&---------------------------------------------------------------------*
*&      Form get
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get.
  CLEAR : it_view[], it_view.
* read basic data

  IF w_mtart_flg = 'X'.
    SELECT b~matnr
         b~meins
         a~werks
         b~mtart
         b~matkl
         b~ernam
         c~maktx
         a~bstmi
         a~bstrf
        INTO CORRESPONDING FIELDS OF TABLE it_view
        FROM marc AS a INNER JOIN mara AS b
          ON a~mandt  EQ  b~mandt
         AND a~matnr  EQ  b~matnr
             INNER JOIN makt AS c
                ON b~mandt  EQ  c~mandt
               AND b~matnr  EQ  c~matnr
       WHERE a~werks  IN  s_werks
       AND   a~dismm  EQ  '' "MRP type
       AND   b~matnr  IN  s_matnr
       AND   b~mtart  IN  ('ROH1', 'ERSA')
       AND   b~matkl  IN  s_matkl
       AND   b~ernam  IN  s_ernam
       AND   b~lvorm  EQ  ''
       AND   c~spras  EQ  sy-langu
*/Begin of Added by Hakchin(20040408)
       AND  ( NOT b~pstat  LIKE '%D%' ).  "MRP View does not exist
*/End of Added by Hakchin(20040408)
  ELSE.
    SELECT b~matnr
           b~meins
           a~werks
           b~mtart
           b~matkl
           b~ernam
           c~maktx
           a~bstmi
           a~bstrf
          INTO CORRESPONDING FIELDS OF TABLE it_view
          FROM marc AS a INNER JOIN mara AS b
            ON a~mandt  EQ  b~mandt
           AND a~matnr  EQ  b~matnr
               INNER JOIN makt AS c
                  ON b~mandt  EQ  c~mandt
                 AND b~matnr  EQ  c~matnr
         WHERE a~werks  IN  s_werks
         AND   a~dismm  EQ  ''
         AND   b~matnr  IN  s_matnr
         AND   b~mtart  EQ  p_mtart
         AND   b~matkl  IN  s_matkl
         AND   b~ernam  IN  s_ernam
         AND   b~lvorm  EQ  ''
         AND   c~spras  EQ  sy-langu
*/Begin of Added by Hakchin(20040408)
         AND  ( NOT b~pstat  LIKE '%D%' ).  "MRP View does not exist
*/End of Added by Hakchin(20040408)
  ENDIF.

  IF sy-subrc EQ 0.
* modify class number
    LOOP AT it_view.
      PERFORM read_class USING it_view-matnr
                      CHANGING it_view-class.
      IF p_class NE ''.
        IF     p_class NE it_view-class.
          DELETE it_view.
        ELSE.
          PERFORM default_data.
          MODIFY it_view.
        ENDIF.
      ELSE.
        PERFORM default_data.
        MODIFY it_view.
      ENDIF.
    ENDLOOP.
    SORT it_view BY matnr.  "Added by Hakchin.
  ENDIF.
ENDFORM.                    " get
*&---------------------------------------------------------------------*
*&      Form  read_class
*&---------------------------------------------------------------------*
*       read class
*----------------------------------------------------------------------*
*      -->P_MATNR  material
*      <--P_CLASS  class no
*----------------------------------------------------------------------*
FORM read_class USING    p_matnr
                CHANGING p_class.

  DATA : lw_objectkey_imp     LIKE  bapi1003_key-object,
         lw_objecttable_imp   LIKE  bapi1003_key-objecttable
                                    VALUE 'MARA',
         lw_classtype_imp     LIKE  bapi1003_key-classtype
                                    VALUE '001',
         lw_classnum          LIKE  bapi1003_key-classnum,

         lt_alloclist       LIKE  TABLE OF bapi1003_alloc_list
                                           WITH HEADER LINE,
         lt_return          LIKE  TABLE OF bapiret2
                                           WITH HEADER LINE.

  MOVE p_matnr TO lw_objectkey_imp.
* get class number
  CALL FUNCTION 'BAPI_OBJCL_GETCLASSES'
    EXPORTING
      objectkey_imp         = lw_objectkey_imp
      objecttable_imp       = lw_objecttable_imp
      classtype_imp         = lw_classtype_imp
*     READ_VALUATIONS       =
      keydate               = sy-datum
      language              = sy-langu
    TABLES
      alloclist             = lt_alloclist
*     ALLOCVALUESCHAR       =
*     ALLOCVALUESCURR       =
*     ALLOCVALUESNUM        =
      return                = lt_return.

  IF lt_alloclist[] IS INITIAL.
  ELSE.
    LOOP AT lt_alloclist.
      lw_classnum  =  lt_alloclist-classnum.
      p_class      =  lw_classnum.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " read_class
*&---------------------------------------------------------------------*
*&      Form  default_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM default_data.

  it_view-beskz  =  'F'.
*/Begin of Changed by Hakchin(20040324)
  CASE it_view-mtart.
    WHEN 'ROH1'.
      it_view-dismm  =  'PD'.  "MRP Type
      it_view-rgekz  =  '1'.   "Backflush
      it_view-mtvfp  =  'KP'.  "availability check
    WHEN 'ERSA'.
      it_view-rgekz  =  space.
      it_view-mtvfp  =  '02'.   "availability check
    WHEN OTHERS.
      it_view-rgekz  =  space.
  ENDCASE.
*/End of Changed by Hakchin(20040324)
  it_view-fhori  =  '000'.

  it_view-altsl  =  '2'.    "Method for Selecting
  it_view-sbdkz  =  '2'.    "ind. for individual and coll.


ENDFORM.                    " default_data
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET PF-STATUS '0100'.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  loop_cnt  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE loop_cnt OUTPUT.
  w_loop_cnt = sy-loopc.
ENDMODULE.                 " loop_cnt  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  set_tc_line  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_tc_line OUTPUT.
  CONCATENATE 'TC' sy-dynnr INTO w_tcname.
  ASSIGN (w_tcname) TO <tc>.

  IF     sy-dynnr = '0100'.
    DESCRIBE TABLE it_view LINES <tc>-lines.
  ENDIF.
ENDMODULE.                 " set_tc_line  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  save_ok = ok_code.
  CLEAR ok_code.

  CASE save_ok.
    WHEN 'SAVE'.
      PERFORM save.

*/Begin of Added by Hakchin(20040413)
    WHEN 'DSPL'.
      PERFORM dsp_log.
*/End of Added by Hakchin(20040413)

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

ENDMODULE.                 " user_command_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  it_view-mark  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE it_view-mark INPUT.
  it_view-mark = 'X'.
ENDMODULE.                 " it_view-mark  INPUT
*&---------------------------------------------------------------------*
*&      Module  it_view_modify  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE it_view_modify INPUT.
  MODIFY it_view INDEX tc0100-current_line.
ENDMODULE.                 " it_view_modify  INPUT
*&---------------------------------------------------------------------*
*&      Form  select_all_pro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM select_all_pro USING    value(p_0484).

  LOOP AT it_view.
    it_view-mark = p_0484.
    MODIFY it_view. CLEAR it_view.
  ENDLOOP.

ENDFORM.                    " select_all_pro
*&---------------------------------------------------------------------*
*&      Form  sort_pro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM sort_pro USING    value(p_0496).

  DATA : w_field  TYPE fieldname,
         f1       TYPE fieldname,
         f2       TYPE fieldname.

  GET CURSOR FIELD w_field.
  CHECK sy-subrc EQ 0.
  SPLIT w_field AT '-'  INTO f1 f2.

  IF     p_0496 = 'A'.
    SORT it_view ASCENDING  BY (f2).
  ELSEIF p_0496 = 'D'.
    SORT it_view DESCENDING BY (f2).
  ENDIF.

ENDFORM.                    " sort_pro
*&---------------------------------------------------------------------*
*&      Form  page_scroll
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM page_scroll USING    p_save_ok.

  CALL FUNCTION 'LOAN_TABLECONTROL_SCROLLING'
       EXPORTING
            i_ok_code                = p_save_ok
            i_visible_lines_in_table = '15'
       CHANGING
            c_tablecontrol           = tc0100.

ENDFORM.                    " page_scroll
*&---------------------------------------------------------------------*
*&      Form  save
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM save.

  CLEAR : it_ztmm_view_ext[], it_ztmm_view_ext.

  LOOP AT it_view WHERE mark = 'X'.
    PERFORM required_check.
    PERFORM post.
  ENDLOOP.

  PERFORM error.

ENDFORM.                    " save
*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  save_ok = ok_code.
  CLEAR ok_code.

  CASE save_ok.
    WHEN 'BACK'.
      SET SCREEN 0. LEAVE SCREEN.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'CANC'.
      LEAVE.
  ENDCASE.
ENDMODULE.                 " exit  INPUT
*&---------------------------------------------------------------------*
*&      Form  required_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM required_check.

  it_view-mark = ''. MODIFY it_view TRANSPORTING mark.

*  IF  it_view-dismm = 'VB'.
*    IF  it_view-dismm = ''  OR  it_view-dispo = ''
*    OR  it_view-disls = ''  OR  it_view-fhori = ''
*    OR  it_view-mtvfp = ''  OR  it_view-minbe = ''.
*      MESSAGE s003. LEAVE TO SCREEN sy-dynnr.
*    ENDIF.
*  ELSE.
*    IF  it_view-dismm = ''  OR  it_view-dispo = ''
*    OR  it_view-disls = ''  OR  it_view-fhori = ''
*    OR  it_view-mtvfp = ''.
*      MESSAGE s003. LEAVE TO SCREEN sy-dynnr.
*    ENDIF.
*  ENDIF.

*/Begin of Added by Hakchin(20040324)
  CASE it_view-mtart.
    WHEN 'ROH1'.
      IF it_view-werks = 'P001'.
        IF it_view-matkl = 'AM'.
          IF it_view-dismm IS INITIAL OR
           it_view-disgr IS INITIAL OR
           it_view-dispo IS INITIAL OR
*         it_view-minbe IS INITIAL OR "Reorder point
           it_view-disls IS INITIAL OR
           it_view-beskz IS INITIAL OR
           it_view-rgekz IS INITIAL OR
           it_view-lgpro IS INITIAL OR
*           it_view-vspvb IS INITIAL OR  "Processed S area
           it_view-lgfsb IS INITIAL OR
           it_view-plifz IS INITIAL OR
           it_view-fhori IS INITIAL OR
           it_view-mtvfp IS INITIAL OR
           it_view-altsl IS INITIAL OR
           it_view-sbdkz IS INITIAL.
*         it_view-wzeit IS INITIAL.  "tot repl laed time
            MESSAGE s003. LEAVE TO SCREEN sy-dynnr.
          ENDIF.

        ELSE.  "Not 'AM'
          IF it_view-dismm IS INITIAL OR
           it_view-disgr IS INITIAL OR
           it_view-dispo IS INITIAL OR
*         it_view-minbe IS INITIAL OR "Reorder point
           it_view-disls IS INITIAL OR
           it_view-beskz IS INITIAL OR
           it_view-rgekz IS INITIAL OR
           it_view-lgpro IS INITIAL OR
           it_view-vspvb IS INITIAL OR
           it_view-lgfsb IS INITIAL OR
           it_view-plifz IS INITIAL OR
           it_view-fhori IS INITIAL OR
           it_view-mtvfp IS INITIAL OR
           it_view-altsl IS INITIAL OR
           it_view-sbdkz IS INITIAL.
*         it_view-wzeit IS INITIAL.  "tot repl laed time
            MESSAGE s003. LEAVE TO SCREEN sy-dynnr.
          ENDIF.

        ENDIF.


      ELSEIF it_view-werks = 'E001'.
        IF it_view-dismm IS INITIAL OR
         it_view-disgr IS INITIAL OR
         it_view-dispo IS INITIAL OR
*         it_view-minbe IS INITIAL OR "Reorder point
         it_view-disls IS INITIAL OR  "lot size
         it_view-beskz IS INITIAL OR
         it_view-rgekz IS INITIAL OR
         it_view-lgpro IS INITIAL OR
*         it_view-vspvb IS INITIAL OR "Processed S area
         it_view-lgfsb IS INITIAL OR
         it_view-plifz IS INITIAL OR
        it_view-fhori IS INITIAL OR  "Scheduling Margin Key for Floats
         it_view-mtvfp IS INITIAL OR
         it_view-altsl IS INITIAL OR
         it_view-sbdkz IS INITIAL.
*         it_view-wzeit IS INITIAL.  "tot repl laed time
          MESSAGE s003. LEAVE TO SCREEN sy-dynnr.
        ENDIF.
** added for E002 plant
   ELSEIF it_view-werks = 'E002'.
        IF it_view-dismm IS INITIAL OR
         it_view-disgr IS INITIAL OR
         it_view-dispo IS INITIAL OR
*         it_view-minbe IS INITIAL OR "Reorder point
         it_view-disls IS INITIAL OR  "lot size
         it_view-beskz IS INITIAL OR
         it_view-rgekz IS INITIAL OR
         it_view-lgpro IS INITIAL OR
*         it_view-vspvb IS INITIAL OR "Processed S area
         it_view-lgfsb IS INITIAL OR
         it_view-plifz IS INITIAL OR
        it_view-fhori IS INITIAL OR  "Scheduling Margin Key for Floats
         it_view-mtvfp IS INITIAL OR
         it_view-altsl IS INITIAL OR
         it_view-sbdkz IS INITIAL.
*         it_view-wzeit IS INITIAL.  "tot repl laed time
          MESSAGE s003. LEAVE TO SCREEN sy-dynnr.
        ENDIF.
** end on 12/19/11
      ENDIF.
    WHEN 'ERSA'.
      IF it_view-dismm = 'VB'.
        IF it_view-dismm IS INITIAL OR
         it_view-disgr IS INITIAL OR
         it_view-dispo IS INITIAL OR
         it_view-minbe IS INITIAL OR "Reorder point
         it_view-disls IS INITIAL OR "Lot size
         it_view-beskz IS INITIAL OR
*         it_view-rgekz IS INITIAL OR "Backflush
         it_view-lgpro IS INITIAL OR
*         it_view-vspvb IS INITIAL OR "Processed S area
         it_view-lgfsb IS INITIAL OR
         it_view-plifz IS INITIAL OR
         it_view-fhori IS INITIAL OR
         it_view-mtvfp IS INITIAL OR
         it_view-altsl IS INITIAL OR
         it_view-sbdkz IS INITIAL.
*         it_view-wzeit IS INITIAL.  "tot repl laed time
          MESSAGE s003. LEAVE TO SCREEN sy-dynnr.
        ENDIF.

      ELSEIF it_view-dismm = 'ND'.
        IF it_view-dismm IS INITIAL OR
         it_view-disgr IS INITIAL OR
         it_view-dispo IS INITIAL OR
*         it_view-minbe IS INITIAL OR "Reorder point
*         it_view-disls IS INITIAL OR "Lot size
         it_view-beskz IS INITIAL OR
*         it_view-rgekz IS INITIAL OR "Backflush
         it_view-lgpro IS INITIAL OR
*         it_view-vspvb IS INITIAL OR "Processed S area
         it_view-lgfsb IS INITIAL OR
         it_view-plifz IS INITIAL OR
         it_view-fhori IS INITIAL OR
         it_view-mtvfp IS INITIAL OR
         it_view-altsl IS INITIAL OR
         it_view-sbdkz IS INITIAL.
*         it_view-wzeit IS INITIAL.  "tot repl laed time
          MESSAGE s003. LEAVE TO SCREEN sy-dynnr.
        ENDIF.

      ENDIF.
    WHEN OTHERS.
  ENDCASE.
*/End of Added by Hakchin(20040324)
ENDFORM.                    " required_check
*&---------------------------------------------------------------------*
*&      Form  post
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM post.
* App Doc No
  PERFORM number_get_next USING    c_nro_nr_09     "NRO Interval
                                   w_nro_object    "NRO Object
                          CHANGING w_zdocno.     "App Doc No
  COMMIT WORK.

* create MRP 1,2,3,4 view
  PERFORM bapi_material_savedata USING it_view.

ENDFORM.                    " post
*&---------------------------------------------------------------------*
*&      Form  bapi_material_savedata
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_VIEW  text
*      -->P_0745   text
*----------------------------------------------------------------------*
FORM bapi_material_savedata
                      USING    pt_view    LIKE  it_view.

  PERFORM read_material_gorup
                         USING  pt_view-matkl  pt_view-mtart
                                wa_ztmm_magroup.
  PERFORM read_bapi_data USING  wa_head
                                wa_marc
                                wa_marcx
                                pt_view
                                wa_ztmm_magroup.
  PERFORM exec_bapi_material_savedata
                         USING  wa_head
                                wa_marc
                                wa_marcx
                                wa_return.
  PERFORM update_log_table
                         TABLES   it_ztmm_view_ext
                         USING    pt_view
                                  wa_return.

ENDFORM.                    " bapi_material_savedata
*&---------------------------------------------------------------------*
*&      Form  read_bapi_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM read_bapi_data
                    USING    pa_head    LIKE  wa_head
                             pa_marc    LIKE  wa_marc
                             pa_marcx   LIKE  wa_marcx
                             pt_view    LIKE  it_view
                             pa_ztmm_magroup LIKE ztmm_magroup.

  CLEAR : pa_head, pa_marc, pa_marcx.
**** HEADDATA
*Material number
  pa_head-material        = pt_view-matnr.
*Industry sector
  pa_head-ind_sector      = 'A'.
* material type
  pa_head-matl_type       = pt_view-mtart.
* views
  pa_head-mrp_view        = 'X'.   "Material Requirements

** Append sales org view when MRP controller is 'P01'
*  IF ps_zsmm_mat_01-dispo = 'P01'.
*    pa_head-sales_view      = 'X'.   "Sales View
*  ENDIF.


**** BAPI_MARC
*Plant
  pa_marc-plant           = pt_view-werks.
*MRP type
  pa_marc-mrp_type        = pt_view-dismm.
*MRP controller
  pa_marc-mrp_ctrler      = pt_view-dispo.
*Lot size
  pa_marc-lotsizekey      = pt_view-disls.
*Procurement Type
  pa_marc-proc_type       = pt_view-beskz.
*Reorder point
  pa_marc-reorder_pt      = pt_view-minbe.

*Start 10/20/2006      Haseeb           UD1K922649
* Minimum lot size
  pa_marc-minlotsize      = pt_view-bstmi.
* Rounding value order quantity
  pa_marc-round_val       = pt_view-bstrf.

* End 10/20/2006      Haseeb           UD1K922649

*Individual/coll
  pa_marc-dep_req_id      = pt_view-sbdkz.
*Selection method
  pa_marc-alt_bom_id      = pt_view-altsl.
*Scheduling Margin Key
  pa_marc-sm_key          = pt_view-fhori.
*Backflush
  pa_marc-backflush       = pt_view-rgekz.
*Loading group
  pa_marc-loadinggrp      = '0001'.
*availability check
  pa_marc-availcheck      = pt_view-mtvfp.
*Issue Storage Location
  pa_marc-iss_st_loc      = pt_view-lgpro.
*MRP Group
  pa_marc-mrp_group       = pt_view-disgr.
*Default storage location for external procurement
  pa_marc-sloc_exprc      = pt_view-lgfsb.
*Proposed Supply Area in Material Master Record
  pa_marc-supply_area     = pt_view-vspvb.
*plnd del time
  pa_marc-plnd_delry      = pt_view-plifz.
*tot repl laed time
  pa_marc-replentime      = pt_view-wzeit.

***read ztmm_magroup(maintain 'zmme10')
*Batch management
  pa_marc-batch_mgmt      = pa_ztmm_magroup-xchpf.
*auto po
  pa_marc-auto_p_ord      = pa_ztmm_magroup-kautb.
**jit
*  pa_marc-jit_relvt       = it_view-fabkz.
*Purchasing grp
  pa_marc-pur_group       = pa_ztmm_magroup-ekgrp.


  pa_marcx-plant           = pa_marc-plant.
  pa_marcx-mrp_type        = 'X'.
  pa_marcx-mrp_ctrler      = 'X'.
  pa_marcx-lotsizekey      = 'X'.
  pa_marcx-proc_type       = 'X'.
  pa_marcx-dep_req_id      = 'X'.
  pa_marcx-alt_bom_id      = 'X'.
  pa_marcx-sm_key          = 'X'.
  pa_marcx-loadinggrp      = 'X'.
  pa_marcx-availcheck      = 'X'.
  pa_marcx-mrp_group       = 'X'.
  pa_marcx-sloc_exprc      = 'X'.

*Start 10/20/2006      Haseeb           UD1K922649
  pa_marcx-minlotsize      = 'X'.
  pa_marcx-round_val       = 'X'.
*End   10/20/2006      Haseeb           UD1K922649

  IF pa_marc-reorder_pt  NE ''.  pa_marcx-reorder_pt  = 'X'. ENDIF.
  IF pa_marc-backflush   NE ''.  pa_marcx-backflush   = 'X'. ENDIF.
  IF pa_marc-batch_mgmt  NE ''.  pa_marcx-batch_mgmt  = 'X'. ENDIF.
  IF pa_marc-auto_p_ord  NE ''.  pa_marcx-auto_p_ord  = 'X'. ENDIF.
  IF pa_marc-iss_st_loc  NE ''.  pa_marcx-iss_st_loc  = 'X'. ENDIF.
  IF pa_marc-jit_relvt   NE ''.  pa_marcx-jit_relvt   = 'X'. ENDIF.
  IF pa_marc-supply_area NE ''.  pa_marcx-supply_area = 'X'. ENDIF.
  IF pa_marc-pur_group   NE ''.  pa_marcx-pur_group   = 'X'. ENDIF.
  IF pa_marc-plnd_delry  NE ''.  pa_marcx-plnd_delry  = 'X'. ENDIF.
  IF pa_marc-replentime  NE ''.  pa_marcx-replentime  = 'X'. ENDIF.


ENDFORM.                    " read_bapi_data
*&---------------------------------------------------------------------*
*&      Form  exec_bapi_material_savedata
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM exec_bapi_material_savedata USING    pa_head    LIKE wa_head
                                          pa_marc    LIKE wa_marc
                                          pa_marcx   LIKE wa_marcx
                                          pa_return  LIKE wa_return.

  CLEAR : pa_return.
  DATA: lt_bapi_matreturn2 LIKE TABLE OF bapi_matreturn2.
  DATA: ls_bapi_matreturn2 LIKE LINE OF lt_bapi_matreturn2.
  CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
       EXPORTING
            headdata       = pa_head
            plantdata      = pa_marc
            plantdatax     = pa_marcx
       IMPORTING
            return         = pa_return
       TABLES
            returnmessages = lt_bapi_matreturn2.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait          = 'X'
* IMPORTING
*   RETURN        =
            .

*/Begin of Added by Hakchin(20040413)

**** (Begin)BAPI Log to the table ZTLOG
  IF lt_bapi_matreturn2 IS INITIAL.  "SUCCESS
    CLEAR: ls_bapi_matreturn2.
    ls_bapi_matreturn2-type       = 'S'.  "SUCCESS
    ls_bapi_matreturn2-id         = 'ZMMM'.
    ls_bapi_matreturn2-number     = '999'.
    ls_bapi_matreturn2-message_v1 = 'Material'.
    ls_bapi_matreturn2-message_v2 = pa_head-material.
    ls_bapi_matreturn2-message_v3 = 'is created/changed.'.
*    ls_bapi_matreturn2-message_v4 = 'is created.'.
    APPEND ls_bapi_matreturn2 TO lt_bapi_matreturn2.
  ENDIF.

  DATA: logno_h       TYPE num10.
  DATA: lv_ztcode     TYPE tcode.
  DATA: lv_zprogramm  TYPE programm.
  DATA: lv_tcode      TYPE tcode.
  DATA: lv_fm_name    TYPE rs38l_fnam.

  lv_ztcode    = sy-tcode.
  lv_zprogramm = sy-cprog.
  lv_tcode     = 'MM01/MM02'.
  lv_fm_name   = 'BAPI_MATERIAL_SAVEDATA'.

  CALL FUNCTION 'Z_FMM_6001_03_LOG_TO_ZTABLE'
    IN UPDATE TASK
    EXPORTING
      im_zdocno            = w_zdocno
      im_ztcode            = lv_ztcode
      im_zprogramm         = lv_zprogramm
      im_tcode             = lv_tcode
      im_fm_name           = lv_fm_name
   TABLES
*     imt_bdcmsgcoll       = it_bdcmsgcoll
     imt_bapiret2         = lt_bapi_matreturn2.
  COMMIT WORK.
**** (End)BAPI Log to the table ZTLOG
*/End of Added by Hakchin(20040413)

ENDFORM.                    " exec_bapi_material_savedata
*&---------------------------------------------------------------------*
*&      Form  read_material_gorup
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM read_material_gorup
                USING p_matkl  p_mtart
                      pa_ztmm_magroup.
  CLEAR pa_ztmm_magroup.
  SELECT SINGLE * INTO pa_ztmm_magroup
         FROM ztmm_magroup
        WHERE matkl  =  p_matkl
        AND   mtart  =  p_mtart.

ENDFORM.                    " read_material_gorup
*&---------------------------------------------------------------------*
*&      Form  update_log_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM update_log_table
                TABLES   pt_ztmm_view_ext  STRUCTURE it_ztmm_view_ext
                USING    pt_view           LIKE it_view
                         pa_return         LIKE bapiret2.

  IF   pa_return-type NE 'S'.
    pt_ztmm_view_ext-mandt   =  sy-mandt.  "client
    pt_ztmm_view_ext-werks   =  pt_view-werks.   "plant
    pt_ztmm_view_ext-matnr   =  pt_view-matnr.   "material
    pt_ztmm_view_ext-zuser   =  'MRPview'.
    pt_ztmm_view_ext-zbdat   =  sy-datum.  "SAP BDC EXECUTED DATE
    pt_ztmm_view_ext-zbtim   =  sy-uzeit.  "SAP BDC EXECUTED TIME
    pt_ztmm_view_ext-zbnam   =  sy-uname.  "BDC User ID
    pt_ztmm_view_ext-zresult =  'E'.
    IF pa_return-message EQ ''.
      pt_ztmm_view_ext-zmsg    = 'Error during MRP view creation'.
    ELSE.
      pt_ztmm_view_ext-zmsg    =  pa_return-message.
    ENDIF.

    APPEND pt_ztmm_view_ext. CLEAR pt_ztmm_view_ext.
  ENDIF.

ENDFORM.                    " update_log_table
*&---------------------------------------------------------------------*
*&      Module  it_view-disgr_f4  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE it_view-disgr_f4 INPUT.

  PERFORM disgr_f4 USING it_view-disgr.

ENDMODULE.                 " it_view-disgr_f4  INPUT
*&---------------------------------------------------------------------*
*&      Form  disgr_f4
*&---------------------------------------------------------------------*
*       text
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
*&      Module  it_view-vspvb_f4  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE it_view-vspvb_f4 INPUT.

  PERFORM vspvb_f4 USING it_view-vspvb.

ENDMODULE.                 " it_view-vspvb_f4  INPUT
*&---------------------------------------------------------------------*
*&      Form  vspvb_f4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
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
*&      Form  error
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM error.
  IF it_ztmm_view_ext[] IS INITIAL.
    MESSAGE s032.
  ELSE.
    MODIFY ztmm_view_ext FROM TABLE it_ztmm_view_ext.
    IF sy-subrc EQ 0.
      COMMIT WORK.
    ENDIF.
*    MESSAGE s030.
*/Begin of Added by Hakchin(20040415)
    MESSAGE i999(zmmm) WITH 'Error during MRP view creation!'
                            'Check Log View !'.
*/End of Added by Hakchin(20040415)
  ENDIF.

ENDFORM.                    " error
*&---------------------------------------------------------------------*
*&      Module  set_cursor  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_cursor OUTPUT.
  SET CURSOR FIELD 'PB_&ALL'.
ENDMODULE.                 " set_cursor  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  number_get_next
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_C_NRO_NR_09  text
*      -->P_W_NRO_OBJECT  text
*      <--P_W_ZDOCNO  text
*----------------------------------------------------------------------*
FORM number_get_next
           USING    value(p_nro_interval) LIKE inri-nrrangenr
                    value(p_nro_object)   LIKE inri-object
           CHANGING value(p_nro_next).
  CALL FUNCTION 'NUMBER_GET_NEXT'
       EXPORTING
            nr_range_nr             = p_nro_interval
            object                  = p_nro_object
       IMPORTING
            number                  = p_nro_next
       EXCEPTIONS
            interval_not_found      = 1
            number_range_not_intern = 2
            object_not_found        = 3
            quantity_is_0           = 4
            quantity_is_not_1       = 5
            interval_overflow       = 6
            OTHERS                  = 7.
  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    "number_get_next
*&---------------------------------------------------------------------*
*&      Form  dsp_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM dsp_log.
*  DATA: BEGIN OF ls_zdocno,
*           sign(1),
*           option(2),
*           low  LIKE ztlog-zdocno,
*           high LIKE ztlog-zdocno,
*        END   OF ls_zdocno.
*  DATA: lt_zdocno LIKE TABLE OF ls_zdocno.
*
*  MOVE: 'I'      TO ls_zdocno-sign,
*        'BT'     TO ls_zdocno-option,
*        w_zdocno TO ls_zdocno-low.
**        sy-uname TO ls_zdocno-high.
*  APPEND ls_zdocno TO lt_zdocno.
*
*  SUBMIT zrmmgm99r_logview
*         WITH s_zdocno IN lt_zdocno
*         AND RETURN.


*/Begin of Display ALV List of Log View
  PERFORM set_it_alvfm_fcat.  "Field Catalog for ALV FM
  PERFORM set_it_alvfm_sort
                 USING it_alvfm_sort.
*  PERFORM set_it_alvfm_event.
  PERFORM set_it_outtab USING w_zdocno.

  PERFORM dsp_alv_list
        USING it_alvfm_fcat
              it_alvfm_sort
              it_alvfm_event
              it_outtab.

*/End of Display ALV List of Log View






ENDFORM.                    " dsp_log
*&---------------------------------------------------------------------*
*&      Form  dsp_alv_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ALVFM_FCAT  text
*      -->P_IT_ALVFM_SORT  text
*      -->P_IT_ALVFM_EVENT  text
*      -->P_IT_OUTTAB  text
*----------------------------------------------------------------------*
FORM dsp_alv_list
      USING imt_alvfm_fcat  LIKE it_alvfm_fcat
            imt_alvfm_sort  LIKE it_alvfm_sort
            imt_alvfm_event LIKE it_alvfm_event
            imt_outtab      TYPE STANDARD TABLE.

  DATA: lv_repid LIKE sy-repid.
  lv_repid = sy-repid.
  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
   EXPORTING
*   I_INTERFACE_CHECK              = ' '
*   I_BYPASSING_BUFFER             =
*   I_BUFFER_ACTIVE                = ' '
     i_callback_program             = lv_repid
     i_callback_pf_status_set       = 'PS_TB_ALV'
*     i_callback_user_command        = 'USER_COMMAND'
*     i_structure_name               = im_structure_name
"You can ALV w/o Structure. At this time imt_alvfm_fcat have to be used.
*   IS_LAYOUT                      =
     it_fieldcat                    = imt_alvfm_fcat
*   IT_EXCLUDING                   =
*   IT_SPECIAL_GROUPS              =
     it_sort                        = imt_alvfm_sort
*   IT_FILTER                      =
*   IS_SEL_HIDE                    =
*   I_DEFAULT                      = 'X'
     i_save                         = 'A'
*   IS_VARIANT                     =
*     it_events                      = imt_alvfm_event
*   IT_EVENT_EXIT                  =
*   IS_PRINT                       =
*   IS_REPREP_ID                   =
*   I_SCREEN_START_COLUMN          = 0
*   I_SCREEN_START_LINE            = 0
*   I_SCREEN_END_COLUMN            = 0
*   I_SCREEN_END_LINE              = 0
* IMPORTING
*   E_EXIT_CAUSED_BY_CALLER        =
*   ES_EXIT_CAUSED_BY_USER         =
    TABLES
      t_outtab                       = imt_outtab
   EXCEPTIONS
     program_error                  = 1
     OTHERS                         = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    "dsp_alv_list
*&---------------------------------------------------------------------*
*&      Form  set_it_alvfm_fcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_it_alvfm_fcat.
  PERFORM set_it_alvoop_fcat.  "Field Catalog for ALV OOP

  FIELD-SYMBOLS: <lf_alvoop_fcat> LIKE LINE OF it_alvoop_fcat.
  DATA: ls_alvfm_fcat LIKE LINE OF it_alvfm_fcat.

  CLEAR: it_alvfm_fcat.
  LOOP AT it_alvoop_fcat ASSIGNING <lf_alvoop_fcat>.
    CLEAR: ls_alvfm_fcat.
    MOVE-CORRESPONDING <lf_alvoop_fcat> TO ls_alvfm_fcat.
    PERFORM modify_ls_alvfm_fcat CHANGING ls_alvfm_fcat.
    APPEND ls_alvfm_fcat TO it_alvfm_fcat.
  ENDLOOP.

ENDFORM.                    " set_it_alvfm_fcat
*&---------------------------------------------------------------------*
*&      Form  set_it_alvoop_fcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_it_alvoop_fcat.
  DATA: ls_alvoop_fcat TYPE lvc_s_fcat.

  ls_alvoop_fcat-fieldname = 'MATNR'.
  ls_alvoop_fcat-inttype   = 'C'.
  ls_alvoop_fcat-coltext   = 'Material'.
*   ls_alvoop_fcat-decimals_o   = '2'.
  ls_alvoop_fcat-outputlen = 18.
  APPEND ls_alvoop_fcat TO it_alvoop_fcat.

  ls_alvoop_fcat-fieldname = 'BB_MSGTYP'.
  ls_alvoop_fcat-inttype   = 'C'.
  ls_alvoop_fcat-coltext   = 'Message type'.
*   ls_alvoop_fcat-decimals_o   = '2'.
  ls_alvoop_fcat-outputlen = 1.
  APPEND ls_alvoop_fcat TO it_alvoop_fcat.

  ls_alvoop_fcat-fieldname = 'BA_MESSAGE'.
  ls_alvoop_fcat-inttype   = 'C'.
  ls_alvoop_fcat-coltext   = 'Message text'.
*   ls_alvoop_fcat-decimals_o   = '2'.
  ls_alvoop_fcat-outputlen = 100.
  APPEND ls_alvoop_fcat TO it_alvoop_fcat.

ENDFORM.                    " set_it_alvoop_fcat
*&---------------------------------------------------------------------*
*&      Form  set_it_alvfm_sort
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_it_alvfm_sort
           USING ext_alvfm_sort LIKE it_alvfm_sort.
  CLEAR: ext_alvfm_sort.
  DATA: ls_alvfm_sort LIKE LINE OF ext_alvfm_sort.

* LIST SORT SEQENCE
  ls_alvfm_sort-spos           = 1.
  ls_alvfm_sort-fieldname      = 'MATNR'.
  ls_alvfm_sort-tabname        = 'IMT_OUTTAB'.
  ls_alvfm_sort-up             = 'X'.
  APPEND ls_alvfm_sort TO ext_alvfm_sort.

  ls_alvfm_sort-spos           = 2.
  ls_alvfm_sort-fieldname      = 'BB_MSGTYP'.
  ls_alvfm_sort-tabname        = 'IMT_OUTTAB'.
  ls_alvfm_sort-up             = 'X'.
  APPEND ls_alvfm_sort TO ext_alvfm_sort.

ENDFORM.                    " set_it_alvfm_sort
*&---------------------------------------------------------------------*
*&      Form  set_it_alvfm_event
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_it_alvfm_event.

ENDFORM.                    " set_it_alvfm_event
*&---------------------------------------------------------------------*
*&      Form  modify_ls_alvfm_fcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LS_ALVFM_FCAT  text
*----------------------------------------------------------------------*
FORM modify_ls_alvfm_fcat
            CHANGING ies_alvfm_fcat LIKE LINE OF it_alvfm_fcat.
  IF ies_alvfm_fcat-fieldname = 'MATNR'.
    ies_alvfm_fcat-seltext_s = ies_alvfm_fcat-seltext_m =
    ies_alvfm_fcat-seltext_l =  'Material'.
  ELSEIF ies_alvfm_fcat-fieldname = 'BB_MSGTYP'.
    ies_alvfm_fcat-seltext_s = ies_alvfm_fcat-seltext_m =
    ies_alvfm_fcat-seltext_l = 'Message type'.
  ELSEIF ies_alvfm_fcat-fieldname = 'BA_MESSAGE'.
    ies_alvfm_fcat-seltext_s = ies_alvfm_fcat-seltext_m =
    ies_alvfm_fcat-seltext_l = 'Message text'.
  ENDIF.
ENDFORM.                    " modify_ls_alvfm_fcat
*&---------------------------------------------------------------------*
*&      Form  set_it_outtab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ZDOCNO  text
*----------------------------------------------------------------------*
FORM set_it_outtab USING    value(im_zdocno).
  SELECT bb_msgv1 AS matnr
         bb_msgtyp
         ba_message
    INTO CORRESPONDING FIELDS OF TABLE it_outtab
    FROM ztlog
    WHERE zdocno = im_zdocno AND
          bb_msgtyp = 'E'.
ENDFORM.                    " set_it_outtab3
*&---------------------------------------------------------------------*
*&      Form  PS_TB_ALV
*&---------------------------------------------------------------------*
FORM ps_tb_alv USING rt_extab TYPE slis_t_extab.
  CLEAR: rt_extab[], rt_extab.

* Instanciate PF-STATUS & TITLEBAR.
  DATA: lv_title(80).
  lv_title = 'Error Log View'.
  SET TITLEBAR 'TB' WITH lv_title.
  SET PF-STATUS 'STANDARD' OF PROGRAM 'SAPLSALV'.
ENDFORM.
