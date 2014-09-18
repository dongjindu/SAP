************************************************************************
* Program name: ZEMMGM19_MATERIAL_CHANGE_MASS
* Created by  : hj.song
* Created on  : 2004.02.17
* Pattern     : report 1-1
* Description : Material master change(mass)
*                                                                      *
* Modification Log                                                     *
* Date            Developer        Request No.    Description          *
* 2004.02.09.     hj.song                         Initial Coding       *
* 04/12/2005      Shiva            UD1K915510     Restrict the selection
*                                               only to 'ROH1' & 'ERSA'.
************************************************************************
REPORT  zemmgm19_material_change_mass  MESSAGE-ID zmmm.

TABLES : t001,
         t001w,
         t001k,
         mara,
         marc,
         mbew,
         rmclf,
         ztmm_magroup,
         ztmm_view_ext.

DATA : BEGIN OF it_view OCCURS 0,
         matnr  LIKE  mara-matnr,
         maktx  LIKE  makt-maktx,
         bwkey  LIKE  t001k-bwkey,
         werks  LIKE  marc-werks,
         mtart  LIKE  mara-mtart,
         meins  LIKE  mara-meins,
         waers  LIKE  t001-waers,
         matkl  LIKE  mara-matkl,
         class  LIKE  rmclf-class,
         mstae  LIKE  mara-mstae, "Xplant matl status
         profl  LIKE  mara-profl, "LP/KD/MIP
         bstme  LIKE  mara-bstme, "Order unit
         mtpos_mara  LIKE  mara-mtpos_mara, "GenltmenCatGroup
         ekgrp  LIKE  marc-ekgrp, "Purchasing group
         kautb  LIKE  marc-kautb, "auto po
         webaz  LIKE  marc-webaz, "GR processing time
         usequ  LIKE  marc-usequ, "quata arr.useage
         kzkri  LIKE  marc-kzkri, "Critical part
         insmk  LIKE  marc-insmk, "Post to Inspection Stock
         kordb  LIKE  marc-kordb, "Source list
         fabkz  LIKE  marc-fabkz, "JIT delivery schedules
         stawn  LIKE  marc-stawn, "Comm/Imp code number
         herkl  LIKE  marc-herkl, "Country of origin
         herkr  LIKE  marc-herkr, "Region of origin
         disgr  LIKE  marc-disgr, "MRP Group
         dismm  LIKE  marc-dismm, "MRP type
         dispo  LIKE  marc-dispo, "MRP controller
         disls  LIKE  marc-disls, "Lot size
         minbe  LIKE  marc-minbe, "Reorder point
         fxhor  LIKE  marc-fxhor, "Planning time fence
         lfrhy  LIKE  marc-lfrhy, "Planning cycle
         maabc  LIKE  marc-maabc, "ABC indicator
         bstmi  LIKE  marc-bstmi, "Minimum lot size
         bstma  LIKE  marc-bstma, "Maximum lot size
         bstfe  LIKE  marc-bstfe, "Fixed lot size
         bstrf  LIKE  marc-bstrf, "Rounding value
         mabst  LIKE  marc-mabst, "Maximum stock level
         rdprf  LIKE  marc-rdprf, "Rounding profile
         mmsta  LIKE  marc-mmsta, "Plant-Specific Material
         beskz  LIKE  marc-beskz, "Procurement Type
         kzech  LIKE  marc-kzech, "batch entry
         sobsl  LIKE  marc-sobsl, "Special procurement type
         lgpro  LIKE  marc-lgpro, "Issue Storage Location
         lgfsb  LIKE  marc-lgfsb, "storage location for ep
         vspvb  LIKE  marc-vspvb, "Proposed Supply Area
         rgekz  LIKE  marc-rgekz, "Backflush
         plifz  LIKE  marc-plifz, "Planned delivery time
         fhori  LIKE  marc-fhori, "Scheduling Margin Key
         mtvfp  LIKE  marc-mtvfp, "availability check
         altsl  LIKE  marc-altsl, "Method for Selecting
         sbdkz  LIKE  marc-sbdkz, "individual and coll.
         abcin  LIKE  marc-abcin, "cycle counting
         ccfix  LIKE  marc-ccfix, "CC indicator is fixed
         xchpf  LIKE  marc-xchpf, "Batch management
         bklas  LIKE  mbew-bklas,"Valuation class
         mlast  LIKE  mbew-mlast,"Material Price Determin
         stprs  LIKE  mbew-stprs,"Standard price
         vprsv  LIKE  mbew-vprsv,"Price control
         lbkum  LIKE  mbew-lbkum,"Total valuated stock
         salk3  LIKE  mbew-salk3,"Value of total valuated stock
         salkv  LIKE  mbew-salkv,"per unit price
         zplp1  LIKE  mbew-zplp1,"Planned Price 1
         zplp3  LIKE  mbew-zplp3,"Planned Price 3
         zpld1  LIKE  mbew-zpld1,"Planned Price 1 date
         zpld3  LIKE  mbew-zpld3,"Planned Price 3 date
         mark,
       END OF it_view,

       it_ztmm_view_ext  LIKE  TABLE OF ztmm_view_ext
                         WITH HEADER LINE,

       wa_wmara LIKE mara,
       wa_wmarc LIKE marc,
       wa_wmbew LIKE mbew,

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

DATA : w_mtart_flg(1),
       w_count TYPE i.


CONTROLS : tc0100 TYPE TABLEVIEW USING SCREEN 0100.

FIELD-SYMBOLS <tc>  TYPE cxtab_control.



SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS :    s_matnr  FOR   mara-matnr,
                    s_werks  FOR   t001w-werks.
SELECTION-SCREEN END OF BLOCK b1.

PARAMETERS     :    p_mtart  LIKE  mara-mtart.
SELECT-OPTIONS :    s_matkl  FOR   mara-matkl,
                    s_ernam  FOR   mara-ernam.
PARAMETERS     :    p_class  LIKE  rmclf-class.



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
           a~werks
           b~mtart
           c~maktx
           b~meins
           d~bwkey

        INTO CORRESPONDING FIELDS OF TABLE it_view
        FROM marc AS a INNER JOIN mara AS b
          ON a~mandt  EQ  b~mandt
         AND a~matnr  EQ  b~matnr
             INNER JOIN makt AS c
                ON b~mandt  EQ  c~mandt
               AND b~matnr  EQ  c~matnr
                   INNER JOIN t001w AS d
                      ON a~mandt  EQ  d~mandt
                     AND a~werks  EQ  d~werks
       WHERE a~werks  IN  s_werks
       AND   b~matnr  IN  s_matnr
       AND   b~mtart  IN  ('ROH1', 'ERSA')
       AND   b~matkl  IN  s_matkl
       AND   b~ernam  IN  s_ernam
       AND   b~lvorm  EQ  ''
       AND   c~spras  EQ  sy-langu.
  ELSE.
    SELECT b~matnr
           a~werks
           b~mtart
           c~maktx
           b~meins
           d~bwkey

          INTO CORRESPONDING FIELDS OF TABLE it_view
          FROM marc AS a INNER JOIN mara AS b
            ON a~mandt  EQ  b~mandt
           AND a~matnr  EQ  b~matnr
               INNER JOIN makt AS c
                  ON b~mandt  EQ  c~mandt
                 AND b~matnr  EQ  c~matnr
                     INNER JOIN t001w AS d
                        ON a~mandt  EQ  d~mandt
                       AND a~werks  EQ  d~werks
         WHERE a~werks  IN  s_werks
         AND   b~matnr  IN  s_matnr
         AND   b~mtart  EQ  p_mtart
         AND   b~matkl  IN  s_matkl
         AND   b~ernam  IN  s_ernam
         AND   b~lvorm  EQ  ''
         AND   c~spras  EQ  sy-langu.
  ENDIF.

  IF sy-subrc EQ 0.
* modify class number
    LOOP AT it_view.
      ADD 1 TO w_count.
      PERFORM read_class USING it_view-matnr
                    CHANGING it_view-class.
      IF p_class NE ''.
        IF     p_class NE it_view-class.
          DELETE it_view.
        ELSE.
          PERFORM modify_data.
          MODIFY it_view. CLEAR it_view.
        ENDIF.
      ELSE.
        PERFORM modify_data.
        MODIFY it_view. CLEAR it_view.
      ENDIF.
    ENDLOOP.
  ENDIF.

  SORT it_view BY matnr werks.

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
*&      Form  modify_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM modify_data.

  CLEAR : wa_wmara, wa_wmarc.

  IF w_count EQ 1.
* get Currency Key
    SELECT SINGLE waers INTO t001-waers
           FROM t001
          WHERE bukrs  EQ  'H201'.
  ENDIF.
  it_view-waers = t001-waers.

* get mara
  CALL FUNCTION 'MARA_SINGLE_READ'
    EXPORTING
*   KZRFB                   = ' '
*   MAXTZ                   = 0
      matnr                   = it_view-matnr
*   SPERRMODUS              = ' '
*   STD_SPERRMODUS          = ' '
*   OUTPUT_NO_MESSAGE       =
   IMPORTING
     wmara                   = wa_wmara
   EXCEPTIONS
     lock_on_material        = 1
     lock_system_error       = 2
     wrong_call              = 3
     not_found               = 4
     OTHERS                  = 5
            .
  IF sy-subrc <> 0.
  ELSE.
    MOVE-CORRESPONDING wa_wmara TO it_view.
  ENDIF.

* get marc
  CALL FUNCTION 'MARC_SINGLE_READ'
    EXPORTING
*   KZRFB                   = ' '
*   MAXTZ                   = 0
      matnr                   = it_view-matnr
      werks                   = it_view-werks
*   SPERRMODUS              = ' '
*   STD_SPERRMODUS          = ' '
   IMPORTING
     wmarc                   = wa_wmarc
   EXCEPTIONS
     lock_on_marc            = 1
     lock_system_error       = 2
     wrong_call              = 3
     not_found               = 4
     OTHERS                  = 5
            .
  IF sy-subrc <> 0.
  ELSE.
    MOVE-CORRESPONDING wa_wmarc TO it_view.
  ENDIF.

*get mbew
  SELECT  SINGLE bklas  mlast  stprs
                 vprsv  lbkum  salk3
                 salkv  zplp1  zplp3
                 zpld1  zpld3

         INTO (it_view-bklas, it_view-mlast, it_view-stprs,
               it_view-vprsv, it_view-lbkum, it_view-salk3,
               it_view-salkv, it_view-zplp1, it_view-zplp3,
               it_view-zpld1, it_view-zpld3)
         FROM mbew
        WHERE matnr  =  it_view-matnr
        AND   bwkey  =  it_view-bwkey.


ENDFORM.                    " modify_data
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

*  IF  it_view-beskz = ''  OR  it_view-mlast = ''
*  OR  it_view-stprs = ''  OR  it_view-vprsv = ''
*  OR  it_view-lbkum = ''  OR  it_view-salk3 = ''
*  OR  it_view-salkv = ''.
*    MESSAGE s003. LEAVE TO SCREEN sy-dynnr.
*  ENDIF.


ENDFORM.                    " required_check
*&---------------------------------------------------------------------*
*&      Form  post
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM post.
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

  PERFORM read_bapi_data TABLES it_makt
                         USING  pt_view
                                wa_head
                                wa_mara
                                wa_marax
                                wa_marc
                                wa_marcx
                                wa_mbew
                                wa_mbewx.

  PERFORM exec_bapi_material_savedata
                         TABLES it_makt
                         USING  wa_head
                                wa_mara
                                wa_marax
                                wa_marc
                                wa_marcx
                                wa_mbew
                                wa_mbewx
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
FORM read_bapi_data TABLES   pt_makt    STRUCTURE it_makt
                    USING    pt_view    LIKE  it_view
                             pa_head    LIKE  wa_head
                             pa_mara    LIKE  wa_mara
                             pa_marax   LIKE  wa_marax
                             pa_marc    LIKE  wa_marc
                             pa_marcx   LIKE  wa_marcx
                             pa_mbew    LIKE  wa_mbew
                             pa_mbewx   LIKE  wa_mbewx.

  CLEAR : pa_head,
          pa_mara, pa_marax,
          pa_marc, pa_marcx,
          pa_mbew, pa_mbewx,
          pt_makt[], pt_makt.

**** HEADDATA *****
*Material number
  pa_head-material        = pt_view-matnr.
*Industry sector
  pa_head-ind_sector      = 'A'.
* material type
  pa_head-matl_type       = pt_view-mtart.

** views
*  pa_head-basic_view      = 'X'.
*  pa_head-purchase_view   = 'X'.
*  pa_head-mrp_view        = 'X'.
*  pa_head-account_view    = 'X'.
*  pa_head-cost_view       = 'X'.


**** BAPI_MARA ****
  pa_mara-matl_group      = pt_view-matkl.
  pa_mara-base_uom        = pt_view-meins.
  pa_mara-pur_status      = pt_view-mstae.
  pa_mara-item_cat        = pt_view-mtpos_mara.
  pa_mara-hazmatprof      = pt_view-profl.
  pa_mara-po_unit         = pt_view-bstme.

  IF NOT pa_mara-matl_group IS INITIAL.
    pa_marax-matl_group  =  'X'.
  ENDIF.
  IF NOT pa_mara-base_uom   IS INITIAL.
    pa_marax-base_uom    =  'X'.
  ENDIF.
  IF NOT pa_mara-pur_status IS INITIAL.
    pa_marax-pur_status  =  'X'.
  ENDIF.
  IF NOT pa_mara-item_cat   IS INITIAL.
    pa_marax-item_cat    =  'X'.
  ENDIF.
  IF NOT pa_mara-hazmatprof IS INITIAL.
    pa_marax-hazmatprof  =  'X'.
  ENDIF.
  IF NOT pa_mara-po_unit    IS INITIAL.
    pa_marax-po_unit     =  'X'.
  ENDIF.


**** BAPI_MARC ****
  pa_marc-plant           = pt_view-werks.
  pa_marc-pur_group       = pt_view-ekgrp.
  pa_marc-auto_p_ord      = pt_view-kautb.
  pa_marc-gr_pr_time      = pt_view-webaz.
  pa_marc-quotausage      = pt_view-usequ.
  pa_marc-crit_part       = pt_view-kzkri.
  pa_marc-ind_post_to_insp_stock = pt_view-insmk.
  pa_marc-sourcelist      = pt_view-kordb.
  pa_marc-jit_relvt       = pt_view-fabkz.
  pa_marc-comm_code       = pt_view-stawn.
  pa_marc-countryori      = pt_view-herkl.
  pa_marc-regionorig      = pt_view-herkr.
  pa_marc-mrp_group       = pt_view-disgr.
  pa_marc-mrp_type        = pt_view-dismm.
  pa_marc-mrp_ctrler      = pt_view-dispo.
  pa_marc-lotsizekey      = pt_view-disls.
  pa_marc-reorder_pt      = pt_view-minbe.
  pa_marc-pl_ti_fnce      = pt_view-fxhor.
  pa_marc-plng_cycle      = pt_view-lfrhy.
  pa_marc-abc_id          = pt_view-maabc.
  pa_marc-minlotsize      = pt_view-bstmi.
  pa_marc-maxlotsize      = pt_view-bstma.
  pa_marc-fixed_lot       = pt_view-bstfe.
  pa_marc-round_val       = pt_view-bstrf.
  pa_marc-max_stock       = pt_view-mabst.
  pa_marc-round_prof      = pt_view-rdprf.
  pa_marc-pur_status      = pt_view-mmsta.
  pa_marc-proc_type       = pt_view-beskz.
  pa_marc-batchentry      = pt_view-kzech.
  pa_marc-spproctype      = pt_view-sobsl.
  pa_marc-iss_st_loc      = pt_view-lgpro.
  pa_marc-sloc_exprc      = pt_view-lgfsb.
  pa_marc-supply_area     = pt_view-vspvb.
  pa_marc-backflush       = pt_view-rgekz.
  pa_marc-plnd_delry      = pt_view-plifz.
  pa_marc-sm_key          = pt_view-fhori.
  pa_marc-availcheck      = pt_view-mtvfp.
  pa_marc-alt_bom_id      = pt_view-altsl.
  pa_marc-dep_req_id      = pt_view-sbdkz.
  pa_marc-cc_ph_inv       = pt_view-abcin.
  pa_marc-cc_fixed        = pt_view-ccfix.
  pa_marc-batch_mgmt      = pt_view-xchpf.

  pa_marcx-plant           = pa_marc-plant.

  IF NOT pa_marc-pur_group IS INITIAL.
    pa_marcx-pur_group       = 'X'.
  ENDIF.
  IF NOT pa_marc-auto_p_ord IS INITIAL.
    pa_marcx-auto_p_ord      = 'X'.
  ENDIF.
  IF NOT pa_marc-gr_pr_time IS INITIAL.
    pa_marcx-gr_pr_time      = 'X'.
  ENDIF.
  IF NOT pa_marc-quotausage IS INITIAL.
    pa_marcx-quotausage      = 'X'.
  ENDIF.
  IF NOT pa_marc-crit_part IS INITIAL.
    pa_marcx-crit_part       = 'X'.
  ENDIF.
  IF NOT pa_marc-ind_post_to_insp_stock IS INITIAL.
    pa_marcx-ind_post_to_insp_stock = 'X'.
  ENDIF.
  IF NOT pa_marc-sourcelist IS INITIAL.
    pa_marcx-sourcelist      = 'X'.
  ENDIF.
  IF NOT pa_marc-jit_relvt IS INITIAL.
    pa_marcx-jit_relvt       = 'X'.
  ENDIF.
  IF NOT pa_marc-comm_code IS INITIAL.
    pa_marcx-comm_code       = 'X'.
  ENDIF.
  IF NOT pa_marc-countryori IS INITIAL.
    pa_marcx-countryori      = 'X'.
  ENDIF.
  IF NOT pa_marc-regionorig IS INITIAL.
    pa_marcx-regionorig      = 'X'.
  ENDIF.
  IF NOT pa_marc-mrp_group IS INITIAL.
    pa_marcx-mrp_group       = 'X'.
  ENDIF.
  IF NOT pa_marc-mrp_type IS INITIAL.
    pa_marcx-mrp_type        = 'X'.
  ENDIF.
  IF NOT pa_marc-mrp_ctrler IS INITIAL.
    pa_marcx-mrp_ctrler      = 'X'.
  ENDIF.
  IF NOT pa_marc-lotsizekey IS INITIAL.
    pa_marcx-lotsizekey      = 'X'.
  ENDIF.
  IF NOT pa_marc-reorder_pt IS INITIAL.
    pa_marcx-reorder_pt      = 'X'.
  ENDIF.
  IF NOT pa_marc-pl_ti_fnce IS INITIAL.
    pa_marcx-pl_ti_fnce      = 'X'.
  ENDIF.
  IF NOT pa_marc-plng_cycle IS INITIAL.
    pa_marcx-plng_cycle      = 'X'.
  ENDIF.
  IF NOT pa_marc-abc_id IS INITIAL.
    pa_marcx-abc_id          = 'X'.
  ENDIF.
  IF NOT pa_marc-minlotsize IS INITIAL.
    pa_marcx-minlotsize      = 'X'.
  ENDIF.
  IF NOT pa_marc-maxlotsize IS INITIAL.
    pa_marcx-maxlotsize      = 'X'.
  ENDIF.
  IF NOT pa_marc-fixed_lot IS INITIAL.
    pa_marcx-fixed_lot       = 'X'.
  ENDIF.
  IF NOT pa_marc-round_val IS INITIAL.
    pa_marcx-round_val       = 'X'.
  ENDIF.
  IF NOT pa_marc-max_stock IS INITIAL.
    pa_marcx-max_stock       = 'X'.
  ENDIF.
  IF NOT pa_marc-round_prof IS INITIAL.
    pa_marcx-round_prof      = 'X'.
  ENDIF.
  IF NOT pa_marc-pur_status IS INITIAL.
    pa_marcx-pur_status      = 'X'.
  ENDIF.
  IF NOT pa_marc-proc_type IS INITIAL.
    pa_marcx-proc_type       = 'X'.
  ENDIF.
  IF NOT pa_marc-batchentry IS INITIAL.
    pa_marcx-batchentry      = 'X'.
  ENDIF.
  IF NOT pa_marc-spproctype IS INITIAL.
    pa_marcx-spproctype      = 'X'.
  ENDIF.
  IF NOT pa_marc-iss_st_loc IS INITIAL.
    pa_marcx-iss_st_loc      = 'X'.
  ENDIF.
  IF NOT pa_marc-sloc_exprc IS INITIAL.
    pa_marcx-sloc_exprc      = 'X'.
  ENDIF.
  IF NOT pa_marc-supply_area IS INITIAL.
    pa_marcx-supply_area     = 'X'.
  ENDIF.
  IF NOT pa_marc-backflush IS INITIAL.
    pa_marcx-backflush       = 'X'.
  ENDIF.
  IF NOT pa_marc-plnd_delry IS INITIAL.
    pa_marcx-plnd_delry      = 'X'.
  ENDIF.
  IF NOT pa_marc-sm_key IS INITIAL.
    pa_marcx-sm_key          = 'X'.
  ENDIF.
  IF NOT pa_marc-availcheck IS INITIAL.
    pa_marcx-availcheck      = 'X'.
  ENDIF.
  IF NOT pa_marc-alt_bom_id IS INITIAL.
    pa_marcx-alt_bom_id      = 'X'.
  ENDIF.
  IF NOT pa_marc-dep_req_id IS INITIAL.
    pa_marcx-dep_req_id      = 'X'.
  ENDIF.
  IF NOT pa_marc-cc_ph_inv IS INITIAL.
    pa_marcx-cc_ph_inv       = 'X'.
  ENDIF.
  IF NOT pa_marc-cc_fixed IS INITIAL.
    pa_marcx-cc_fixed        = 'X'.
  ENDIF.



**** BAPI_MBEW ****
  pa_mbew-val_area        = pt_view-bwkey.
  pa_mbew-val_type        = ''.
  pa_mbew-val_class       = pt_view-bklas.
  pa_mbew-plndprice1      = pt_view-zplp1.
  pa_mbew-plndprice3      = pt_view-zplp3.
  pa_mbew-plndprdate1     = pt_view-zpld1.
  pa_mbew-plndprdate3     = pt_view-zpld3.


  pa_mbewx-val_area       = pt_view-bwkey.
  pa_mbewx-val_type       = ''.
  pa_mbewx-val_class      = 'X'.
  pa_mbewx-plndprice1     = 'X'.
  pa_mbewx-plndprice3     = 'X'.
  pa_mbewx-plndprdate1    = 'X'.
  pa_mbewx-plndprdate3    = 'X'.


**** BAPI_MAKT *****
  pt_makt-langu        = sy-langu.
  pt_makt-matl_desc    = pt_view-maktx.
  pt_makt-del_flag     = ''.
  APPEND pt_makt. CLEAR pt_makt.


ENDFORM.                    " read_bapi_data
*&---------------------------------------------------------------------*
*&      Form  exec_bapi_material_savedata
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM exec_bapi_material_savedata TABLES   pt_makt    STRUCTURE it_makt
                                 USING    pa_head    LIKE wa_head
                                          pa_mara    LIKE wa_mara
                                          pa_marax   LIKE wa_marax
                                          pa_marc    LIKE wa_marc
                                          pa_marcx   LIKE wa_marcx
                                          pa_mbew    LIKE wa_mbew
                                          pa_mbewx   LIKE wa_mbewx
                                          pa_return  LIKE wa_return.

  CLEAR : pa_return.

  CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
    EXPORTING
     headdata                   = pa_head
     clientdata                 = pa_mara
     clientdatax                = pa_marax
     plantdata                  = pa_marc
     plantdatax                 = pa_marcx
*   FORECASTPARAMETERS         =
*   FORECASTPARAMETERSX        =
*   PLANNINGDATA               =
*   PLANNINGDATAX              =
*   STORAGELOCATIONDATA        =
*   STORAGELOCATIONDATAX       =
     valuationdata              = pa_mbew
     valuationdatax             = pa_mbewx
*   WAREHOUSENUMBERDATA        =
*   WAREHOUSENUMBERDATAX       =
*   SALESDATA                  =
*   SALESDATAX                 =
*   STORAGETYPEDATA            =
*   STORAGETYPEDATAX           =
*   FLAG_ONLINE                = ' '
*   FLAG_CAD_CALL              = ' '
   IMPORTING
     return                     = pa_return
   TABLES
     materialdescription        = pt_makt
*   UNITSOFMEASURE             =
*   UNITSOFMEASUREX            =
*   INTERNATIONALARTNOS        =
*   MATERIALLONGTEXT           =
*   TAXCLASSIFICATIONS         =
*   RETURNMESSAGES             =
*   PRTDATA                    =
*   PRTDATAX                   =
*   EXTENSIONIN                =
*   EXTENSIONINX               =
            .

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
* EXPORTING
*   WAIT          =
* IMPORTING
*   RETURN        =
            .


ENDFORM.                    " exec_bapi_material_savedata
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
    pt_ztmm_view_ext-zuser   =  'MassChange'.
    pt_ztmm_view_ext-zbdat   =  sy-datum.  "SAP BDC EXECUTED DATE
    pt_ztmm_view_ext-zbtim   =  sy-uzeit.  "SAP BDC EXECUTED TIME
    pt_ztmm_view_ext-zbnam   =  sy-uname.  "BDC User ID
    pt_ztmm_view_ext-zresult =  'E'.
    IF pa_return-message EQ ''.
      pt_ztmm_view_ext-zmsg    = 'Error Material master change'.
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
    MESSAGE s033.
  ELSE.
    MODIFY ztmm_view_ext FROM TABLE it_ztmm_view_ext.
    IF sy-subrc EQ 0.
      COMMIT WORK.
    ENDIF.
    MESSAGE s034.
  ENDIF.
ENDFORM.                    " error
