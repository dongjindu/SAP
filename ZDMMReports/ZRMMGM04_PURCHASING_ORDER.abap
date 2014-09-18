************************************************************************
* Program Name      : ZRMMGM04_PURCHASING_ORDER
* Author            : hj.song
* Creation Date     : 2003.11.14.
* Specifications By : hj.song
* Pattern           : Report 1-1
* Development Request No : UD1K902172
* Addl Documentation:
* Description       : Purchasing order list
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2003.11.14.     hj.song          UD1K902172     Initial Coding
* 2004.01.27      hj.song          UD1K906014     ????
* 2004.01.30      hj.song          UD1K906617     ????
* 2004/08/25      Shiva            UD1K912037     Commented displaying
*                                                 the system message
*                                               creating interface log.
************************************************************************
*&---------------------------------------------------------------------*
*& Report  ZRMMGM04_PURCHASING_ORDER                                   *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*
REPORT zrmmgm04_purchasing_order MESSAGE-ID zmmm.


INCLUDE zrmmgm04_purchasing_order_top.

START-OF-SELECTION.
  PERFORM set_parameters.
  PERFORM read_data.

END-OF-SELECTION.
  PERFORM call_screen.

*&---------------------------------------------------------------------*
*&      Form  read_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM read_data.

  CLEAR : it_basic[], it_basic,
          it_basic1[], it_basic1.
*purchasing order
  IF      rp1  =  'X'.
    IF      rv1  =  'X'.   "released
      PERFORM read_po_released.
    ELSEIF  rv2  =  'X'.   "sucess
      PERFORM read_po_sucess.
    ELSEIF  rv3  =  'X'.   "error
      PERFORM read_po_error.
    ENDIF.
*processing infor
  ELSEIF  rp2  =  'X'.
    IF      rv1  =  'X'.   "released
      PERFORM read_processing_released.
    ELSEIF  rv2  =  'X'.   "sucess
      PERFORM read_processing_sucess.
    ELSEIF  rv3  =  'X'.   "error
      PERFORM read_processing_error.
    ENDIF.
  ENDIF.

ENDFORM.                    " read_data
*&---------------------------------------------------------------------*
*&      Form  read_material_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM read_material_text USING    p_matnr  p_maktx.

  SELECT SINGLE maktx INTO p_maktx
         FROM makt
        WHERE matnr EQ p_matnr
        AND   spras EQ sy-langu.

ENDFORM.                    " read_material_text
*&---------------------------------------------------------------------*
*&      Form  read_vendor_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM read_vendor_text USING    p_lifnr  p_name1.

  SELECT SINGLE name1 INTO p_name1
         FROM lfa1
        WHERE lifnr EQ p_lifnr.

ENDFORM.                    " read_vendor_text
*&---------------------------------------------------------------------*
*&      Form  set_parameters
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_parameters.

  IF    p_lifnr  EQ  ''.  p_lifnr  =  '%'.  ENDIF.
  IF    p_matnr  EQ  ''.  p_matnr  =  '%'.  ENDIF.

ENDFORM.                    " set_parameters
*&---------------------------------------------------------------------*
*&      Module  STATUS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status OUTPUT.

  SET PF-STATUS '0100'.

ENDMODULE.                 " STATUS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  save_ok = ok_code.
  CLEAR ok_code.

  CASE save_ok.
* interface exec
    WHEN 'INTF'.
      PERFORM interface_process.
* select all
    WHEN '&ALL'.
      PERFORM select_all_pro USING  'X'.
* deselect all
    WHEN '&SAL'.
      PERFORM select_all_pro USING space.
* page up, down
    WHEN 'P-' OR 'P--' OR 'P++' OR 'P+'.
      PERFORM page_scroll USING save_ok.
* delete
    WHEN 'DELE'.
      PERFORM delete_temp.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
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
    DESCRIBE TABLE it_header   LINES <tc>-lines.
  ELSEIF sy-dynnr = '0110'.
    DESCRIBE TABLE it_po       LINES <tc>-lines.
  ELSEIF sy-dynnr = '0120'.
    DESCRIBE TABLE it_material LINES <tc>-lines.
  ELSEIF sy-dynnr = '0130'.
    DESCRIBE TABLE it_poitem   LINES <tc>-lines.
  ELSEIF sy-dynnr = '0200'.
    DESCRIBE TABLE it_header1  LINES <tc>-lines.
  ENDIF.

ENDMODULE.                 " set_tc_line  OUTPUT
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
*&      Form  interface_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM interface_process.

  CLEAR : it_rfc[], it_rfc,
          it_rfc1[], it_rfc1.
* purchasing order
  IF      rp1  =  'X'.
    PERFORM screen_po.
* processing info
  ELSEIF  rp2  =  'X'.
    PERFORM screen_processing.
  ENDIF.

ENDFORM.                    " interface_process
*&---------------------------------------------------------------------*
*&      Form  exec_rfc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM exec_rfc.

  DATA : lw_msgtxt(255),
         lw_tabix  LIKE sy-tabix.
  CLEAR: lw_msgtxt, lw_tabix.

* RFC function call
  CHECK NOT it_rfc[] IS INITIAL.
  CALL FUNCTION 'Z_FMM_SET_PUR_ORDER'
    DESTINATION              c_dest
    TABLES
      et_ztmm_pur_order      = it_rfc
    EXCEPTIONS
      communication_failure  = 1  MESSAGE lw_msgtxt
      system_failure         = 2  MESSAGE lw_msgtxt.
* success, fail log update
  LOOP AT it_rfc.
    lw_tabix = sy-tabix.

    it_rfc-zuser  =   sy-uname.
    it_rfc-zsdat  =   sy-datum.
    it_rfc-zstim  =   sy-uzeit.
    it_rfc-zmode  =   'C'.

    IF it_rfc-zzret = 'S'.
      it_rfc-zmsg   =   'Purchasing order success'.
      it_rfc-flag   = it_rfc-zzret.
      MODIFY it_rfc INDEX lw_tabix.
    ELSE.
      IF lw_msgtxt NE ''.
        it_rfc-zmsg   = lw_msgtxt.
      ELSE.
        it_rfc-zmsg   =  'Purchasing order fail'.
      ENDIF.
      it_rfc-zzret = 'E'.
      it_rfc-flag  = it_rfc-zzret.
      MODIFY it_rfc INDEX lw_tabix.
    ENDIF.
  ENDLOOP.

* modify ztable
  MODIFY ztmm_pur_order FROM TABLE it_rfc.
  IF sy-subrc EQ 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.



ENDFORM.                    " exec_rfc
*&---------------------------------------------------------------------*
*&      Form  create_interface_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM create_interface_log.

  CLEAR : wa_ztca_if_log, w_total.
  DESCRIBE TABLE it_rfc LINES w_total.
  LOOP AT it_rfc.
    IF     it_rfc-flag = 'S'.
      wa_ztca_if_log-zsucc = wa_ztca_if_log-zsucc + 1.
    ELSEIF it_rfc-flag = 'E'.
      wa_ztca_if_log-error = wa_ztca_if_log-error + 1.
    ENDIF.
  ENDLOOP.

  CHECK w_total <> 0.
  wa_ztca_if_log-tcode    = 'ZMMR15'.
*  wa_ZTCA_IF_LOG-ZSLNO    = WA_JOB-SLNO.
*  wa_ZTCA_IF_LOG-JOBCOUNT = WA_JOB-INT.
  wa_ztca_if_log-total    = w_total.
  wa_ztca_if_log-erdat    = sy-datum. "Created on.
  wa_ztca_if_log-erzet    = sy-uzeit. "Created time.
  wa_ztca_if_log-ernam    = sy-uname. "Created by.

  CALL FUNCTION 'Z_FCA_EAI_INTERFACE_LOG'
    EXPORTING
      i_ztca_if_log              = wa_ztca_if_log
*   IMPORTING
*     E_ZTCA_IF_LOG              =
   EXCEPTIONS
     update_failed              = 1
     number_range_error         = 2
     tcode_does_not_exist       = 3
     OTHERS                     = 4
            .
  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


* message
  IF     w_total  =  wa_ztca_if_log-zsucc.
    MESSAGE s028. LEAVE TO SCREEN sy-dynnr.
  ELSEIF w_total  =  wa_ztca_if_log-error.
*    MESSAGE e029. LEAVE TO SCREEN sy-dynnr.
    MESSAGE s999 WITH 'EAI Error Occurred!'.
    LEAVE TO SCREEN sy-dynnr.
  ELSE.
*    MESSAGE e029. LEAVE TO SCREEN sy-dynnr.
    MESSAGE s999 WITH 'EAI Error Occurred!'.
    LEAVE TO SCREEN sy-dynnr.
  ENDIF.

ENDFORM.                    " create_interface_log
*&---------------------------------------------------------------------*
*&      Form  select_all_pro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM select_all_pro  USING    value(p_0647).

  CASE sy-dynnr.
    WHEN '0100'.
      LOOP AT it_header.
        it_header-mark = p_0647.
        MODIFY it_header. CLEAR it_header.
      ENDLOOP.
    WHEN '0110'.
      LOOP AT it_po.
        it_po-mark = p_0647.
        MODIFY it_po. CLEAR it_po.
      ENDLOOP.
    WHEN '0120'.
      LOOP AT it_material.
        it_material-mark = p_0647.
        MODIFY it_material. CLEAR it_material.
      ENDLOOP.
    WHEN '0130'.
      LOOP AT it_poitem.
        it_poitem-mark = p_0647.
        MODIFY it_poitem. CLEAR it_poitem.
      ENDLOOP.
    WHEN '0200'.
      LOOP AT it_header1.
        it_header1-mark = p_0647.
        MODIFY it_header1. CLEAR it_header1.
      ENDLOOP.
  ENDCASE.

ENDFORM.                    " select_all_pro
*&---------------------------------------------------------------------*
*&      Form  page_scroll
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM page_scroll USING    p_save_ok.

  CASE sy-dynnr.
    WHEN '0100'.
      CALL FUNCTION 'LOAN_TABLECONTROL_SCROLLING'
           EXPORTING
                i_ok_code                = p_save_ok
                i_visible_lines_in_table = '15'
           CHANGING
                c_tablecontrol           = tc0100.
    WHEN '0110'.
      CALL FUNCTION 'LOAN_TABLECONTROL_SCROLLING'
           EXPORTING
                i_ok_code                = p_save_ok
                i_visible_lines_in_table = '15'
           CHANGING
                c_tablecontrol           = tc0110.
    WHEN '0120'.
      CALL FUNCTION 'LOAN_TABLECONTROL_SCROLLING'
           EXPORTING
                i_ok_code                = p_save_ok
                i_visible_lines_in_table = '15'
           CHANGING
                c_tablecontrol           = tc0120.
    WHEN '0130'.
      CALL FUNCTION 'LOAN_TABLECONTROL_SCROLLING'
           EXPORTING
                i_ok_code                = p_save_ok
                i_visible_lines_in_table = '15'
           CHANGING
                c_tablecontrol           = tc0130.
    WHEN '0200'.
      CALL FUNCTION 'LOAN_TABLECONTROL_SCROLLING'
           EXPORTING
                i_ok_code                = p_save_ok
                i_visible_lines_in_table = '15'
           CHANGING
                c_tablecontrol           = tc0200.

  ENDCASE.

ENDFORM.                    " page_scroll
*&---------------------------------------------------------------------*
*&      Module  it_header-mark  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE it_header-mark INPUT.

  it_header-mark = 'X'.

ENDMODULE.                 " it_header-mark  INPUT
*&---------------------------------------------------------------------*
*&      Module  it_header_modify  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE it_header_modify INPUT.

  MODIFY it_header INDEX tc0100-current_line TRANSPORTING mark.

ENDMODULE.                 " it_header_modify  INPUT
*&---------------------------------------------------------------------*
*&      Module  it_header1-mark  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE it_header1-mark INPUT.

  it_header1-mark = 'X'.

ENDMODULE.                 " it_header1-mark  INPUT
*&---------------------------------------------------------------------*
*&      Module  it_header1_modify  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE it_header1_modify INPUT.

  MODIFY it_header1 INDEX tc0200-current_line TRANSPORTING mark.

ENDMODULE.                 " it_header1_modify  INPUT
*&---------------------------------------------------------------------*
*&      Form  read_po_released
*&---------------------------------------------------------------------*
*       read po released
*----------------------------------------------------------------------*
FORM read_po_released.
  SELECT  a~bsart  b~pstyp  a~bukrs
          b~werks  b~ebeln  b~ebelp
          b~matnr  a~lifnr  c~menge
          b~meins  c~eindt  b~netpr
          a~waers  a~bedat  c~etenr

    INTO CORRESPONDING FIELDS OF TABLE it_basic
    FROM ekko AS a
      INNER JOIN ekpo AS b
      ON a~mandt  EQ  b~mandt AND
         a~ebeln  EQ  b~ebeln
      INNER JOIN eket AS c
      ON b~mandt  EQ  c~mandt AND
         b~ebeln  EQ  c~ebeln AND
         b~ebelp  EQ  c~ebelp
    WHERE a~bedat IN    s_bedat AND
          a~lifnr LIKE  p_lifnr AND
          b~matnr LIKE  p_matnr AND
** Changed on 06/20/13 for SA
*          b~matnr LIKE  'AM%'   AND
** End on 06/20/13
          c~eindt IN    s_eindt AND
          b~loekz EQ    ''      AND
** Changed on 06/20/13 for SA
         a~bsart EQ    'JIT'.   "sa
*          a~bsart EQ    'NB'    AND   "po
*          a~frgke EQ    'F'.   "release
** End on 06/20/13

*  if sy-subrc eq 0.
*  endif.
  SORT it_basic BY ebeln ebelp etenr.


  DATA: lv_subrc LIKE sy-subrc.

  LOOP AT it_basic INTO wa_basic.
*/Begin of Added by Hakchin(20040422)
    it_basic = wa_basic.
*/End of Added by Hakchin(20040422)


* get material text
    PERFORM read_material_text  USING wa_basic-matnr
                                      wa_basic-maktx.
* get vendor text
    PERFORM read_vendor_text    USING wa_basic-lifnr
                                      wa_basic-name1.
* get characteristic
    CLEAR wa_zsmm_class.
    CALL FUNCTION 'Z_FMM_GET_CHARACT'
         EXPORTING
              i_matnr        = wa_basic-matnr
         IMPORTING
              es_zsmm_class  = wa_zsmm_class
         EXCEPTIONS
              data_not_found = 1
              OTHERS         = 2.
    IF sy-subrc EQ 0.
      wa_basic-prop        =  wa_zsmm_class-zprop.
      wa_basic-coating     =  wa_zsmm_class-zcoating.
      wa_basic-thick       =  wa_zsmm_class-zthick.
      wa_basic-width       =  wa_zsmm_class-zwidth.
      wa_basic-length      =  wa_zsmm_class-zlength.
    ENDIF.
* others
    CLEAR *t163y.
    SELECT SINGLE epstp  INTO *t163y-epstp
                  FROM  t163y
                 WHERE spras  EQ  sy-langu
                 AND   pstyp  EQ  wa_basic-pstyp.
    CONCATENATE wa_basic-bsart  *t163y-epstp
                    INTO wa_basic-doc_type. "doc type
* interface flag
    SELECT SINGLE flag INTO wa_basic-flag
          FROM ztmm_pur_order
         WHERE doc_type  =  wa_basic-doc_type
         AND   bukrs     =  wa_basic-bukrs
         AND   werks     =  wa_basic-werks
         AND   ebeln     =  wa_basic-ebeln
         AND   ebelp     =  wa_basic-ebelp
         AND   etenr     =  wa_basic-etenr
         AND   matnr     =  wa_basic-matnr.
    lv_subrc = sy-subrc.
    MODIFY it_basic FROM wa_basic.
*/Begin of Added by Hakchin(20040422)
    IF lv_subrc = 0.
      DELETE it_basic.
    ENDIF.
*/End of Added by Hakchin(20040422)
  ENDLOOP.
ENDFORM.                    " read_po_released
*&---------------------------------------------------------------------*
*&      Form  read_po_error
*&---------------------------------------------------------------------*
*       read_po_error
*----------------------------------------------------------------------*
FORM read_po_error.
  SELECT * INTO TABLE it_basic
           FROM ztmm_pur_order
       WHERE bedat  IN      s_bedat
       AND   lifnr  LIKE    p_lifnr
       AND   matnr  LIKE    p_matnr
       AND   eindt  IN      s_eindt
       AND   flag   EQ      'E'.

ENDFORM.                    " read_po_error
*&---------------------------------------------------------------------*
*&      Form  call_screen
*&---------------------------------------------------------------------*
FORM call_screen.

  IF      rf1  =  'X'.   "all
    PERFORM all.
  ELSEIF  rf2  =  'X'.   "group by po no
    PERFORM group_by_pono.
  ELSEIF  rf3  =  'X'.   "group by material
    PERFORM group_by_material.
  ELSEIF  rf4  =  'X'.   "group by po item
    PERFORM group_by_poitem.
  ENDIF.

ENDFORM.                    " call_screen
*&---------------------------------------------------------------------*
*&      Form  all
*&---------------------------------------------------------------------*
FORM all.

  CLEAR : it_header[], it_header,
          it_header1[], it_header1.
*purchasing order
  IF      rp1  =  'X'.
    it_header[] = it_basic[].

    REFRESH CONTROL 'TC0100' FROM SCREEN '0100'.
    CALL SCREEN '0100'.
*processing info
  ELSEIF  rp2  =  'X'.
    it_header1[] = it_basic1[].

    REFRESH CONTROL 'TC0200' FROM SCREEN '0200'.
    CALL SCREEN '0200'.
  ENDIF.

ENDFORM.                    " all
*&---------------------------------------------------------------------*
*&      Form  group_by_pono
*&---------------------------------------------------------------------*
FORM group_by_pono.

  CLEAR : it_po[], it_po.

*purchasing order
  IF      rp1  =  'X'.
    LOOP AT it_basic.
      it_po-ebeln  =  it_basic-ebeln.
      it_po-flag   =  it_basic-flag.
      COLLECT it_po. CLEAR it_po.
    ENDLOOP.

    SORT it_basic BY ebeln eindt.
    LOOP AT it_po.
      READ TABLE it_basic WITH KEY ebeln = it_po-ebeln.
      IF sy-subrc = 0.
        it_po-eindt = it_basic-eindt.
        MODIFY it_po. CLEAR it_po.
      ENDIF.
    ENDLOOP.
*processing info
  ELSEIF  rp2  =  'X'.
    LOOP AT it_basic1.
      it_po-ebeln  =  it_basic1-ebeln.
      it_po-flag   =  it_basic1-flag.
      COLLECT it_po. CLEAR it_po.
    ENDLOOP.

    SORT it_basic1 BY ebeln eindt.
    LOOP AT it_po.
      READ TABLE it_basic1 WITH KEY ebeln = it_po-ebeln.
      IF sy-subrc = 0.
        it_po-eindt = it_basic1-eindt.
        MODIFY it_po. CLEAR it_po.
      ENDIF.
    ENDLOOP.
  ENDIF.

  SORT it_po BY ebeln.
  REFRESH CONTROL 'TC0110' FROM SCREEN '0110'.
  CALL SCREEN '0110'.

ENDFORM.                    " group_by_pono
*&---------------------------------------------------------------------*
*&      Module  it_po-mark  INPUT
*&---------------------------------------------------------------------*
MODULE it_po-mark INPUT.

  it_po-mark = 'X'.

ENDMODULE.                 " it_po-mark  INPUT
*&---------------------------------------------------------------------*
*&      Module  it_po_modify  INPUT
*&---------------------------------------------------------------------*
MODULE it_po_modify INPUT.

  MODIFY it_po INDEX tc0110-current_line TRANSPORTING mark.

ENDMODULE.                 " it_po_modify  INPUT
*&---------------------------------------------------------------------*
*&      Module  it_material-mark  INPUT
*&---------------------------------------------------------------------*
MODULE it_material-mark INPUT.

  it_material-mark = 'X'.

ENDMODULE.                 " it_material-mark  INPUT
*&---------------------------------------------------------------------*
*&      Module  it_material_modify  INPUT
*&---------------------------------------------------------------------*
MODULE it_material_modify INPUT.

  MODIFY it_material INDEX tc0120-current_line TRANSPORTING mark.

ENDMODULE.                 " it_material_modify  INPUT
*&---------------------------------------------------------------------*
*&      Module  it_poitem-mark  INPUT
*&---------------------------------------------------------------------*
MODULE it_poitem-mark INPUT.

  it_poitem-mark = 'X'.

ENDMODULE.                 " it_poitem-mark  INPUT
*&---------------------------------------------------------------------*
*&      Module  it_poitem_modify  INPUT
*&---------------------------------------------------------------------*
MODULE it_poitem_modify INPUT.

  MODIFY it_poitem INDEX tc0130-current_line TRANSPORTING mark.

ENDMODULE.                 " it_poitem_modify  INPUT
*&---------------------------------------------------------------------*
*&      Form  group_by_material
*&---------------------------------------------------------------------*
FORM group_by_material.

  CLEAR : it_material[], it_material.

*purchasing order
  IF      rp1  =  'X'.
    SORT it_basic BY matnr.
    LOOP AT it_basic.
      it_material-matnr  =  it_basic-matnr.
      it_material-maktx  =  it_basic-maktx.
      it_material-menge  =  it_basic-menge.
      it_material-meins  =  it_basic-meins.
      it_material-flag   =  it_basic-flag.
      COLLECT it_material. CLEAR it_material.
    ENDLOOP.
*processing info
  ELSEIF  rp2  =  'X'.
    SORT it_basic1 BY matnr.
    LOOP AT it_basic1.
      it_material-matnr  =  it_basic1-matnr.
      it_material-maktx  =  it_basic1-maktx.
      it_material-menge  =  it_basic1-menge.
      it_material-meins  =  it_basic1-meins.
      it_material-flag   =  it_basic1-flag.
      COLLECT it_material. CLEAR it_material.
    ENDLOOP.
  ENDIF.

  SORT it_material BY matnr.
  REFRESH CONTROL 'TC0120' FROM SCREEN '0120'.
  CALL SCREEN '0120'.

ENDFORM.                    " group_by_material
*&---------------------------------------------------------------------*
*&      Form  group_by_poitem
*&---------------------------------------------------------------------*
FORM group_by_poitem.

  CLEAR : it_poitem[], it_poitem.

*purchasing order
  IF      rp1  =  'X'.
    LOOP AT it_basic.
      it_poitem-ebeln  =  it_basic-ebeln.
      it_poitem-ebelp  =  it_basic-ebelp.
      it_poitem-matnr  =  it_basic-matnr.
      it_poitem-maktx  =  it_basic-maktx.
      it_poitem-menge  =  it_basic-menge.
      it_poitem-meins  =  it_basic-meins.
      it_poitem-flag   =  it_basic-flag.
      COLLECT it_poitem. CLEAR it_poitem.
    ENDLOOP.

    SORT it_basic BY ebeln ebelp eindt.
    LOOP AT it_poitem.
      READ TABLE it_basic WITH KEY ebeln = it_poitem-ebeln
                                   ebelp = it_poitem-ebelp.
      IF sy-subrc = 0.
        it_poitem-eindt = it_basic-eindt.
        MODIFY it_poitem. CLEAR it_poitem.
      ENDIF.
    ENDLOOP.
*processing info
  ELSEIF  rp2  =  'X'.
    LOOP AT it_basic1.
      it_poitem-ebeln  =  it_basic1-ebeln.
      it_poitem-ebelp  =  it_basic1-ebelp.
      it_poitem-matnr  =  it_basic1-matnr.
      it_poitem-maktx  =  it_basic1-maktx.
      it_poitem-menge  =  it_basic1-menge.
      it_poitem-meins  =  it_basic1-meins.
      it_poitem-flag   =  it_basic1-flag.
      COLLECT it_poitem. CLEAR it_poitem.
    ENDLOOP.

    SORT it_basic1 BY ebeln ebelp eindt.
    LOOP AT it_poitem.
      READ TABLE it_basic1 WITH KEY ebeln = it_poitem-ebeln
                                    ebelp = it_poitem-ebelp.
      IF sy-subrc = 0.
        it_poitem-eindt = it_basic1-eindt.
        MODIFY it_poitem. CLEAR it_poitem.
      ENDIF.
    ENDLOOP.
  ENDIF.

  SORT it_poitem BY ebeln ebelp matnr.
  REFRESH CONTROL 'TC0130' FROM SCREEN '0130'.
  CALL SCREEN '0130'.

ENDFORM.                    " group_by_poitem
*&---------------------------------------------------------------------*
*&      Form  interface_process_others
*&---------------------------------------------------------------------*
FORM interface_process_others.

  CLEAR : it_rfc[], it_rfc.

  CASE sy-dynnr.
    WHEN '0110'.
      LOOP AT it_po WHERE mark = 'X'.
        LOOP AT it_header WHERE ebeln = it_po-ebeln.
          MOVE-CORRESPONDING it_header TO it_rfc.
          APPEND it_rfc. CLEAR it_rfc.
        ENDLOOP.
      ENDLOOP.
    WHEN '0120'.
      LOOP AT it_material WHERE mark = 'X'.
        LOOP AT it_header WHERE matnr = it_material-matnr.
          MOVE-CORRESPONDING it_header TO it_rfc.
          APPEND it_rfc. CLEAR it_rfc.
        ENDLOOP.
      ENDLOOP.
    WHEN '0130'.
      LOOP AT it_poitem WHERE mark = 'X'.
        LOOP AT it_header WHERE ebeln = it_poitem-ebeln
                          AND   ebelp = it_poitem-ebelp.
          MOVE-CORRESPONDING it_header TO it_rfc.
          APPEND it_rfc. CLEAR it_rfc.
        ENDLOOP.
      ENDLOOP.
  ENDCASE.

* execute rfc fm on all data and update ztable
  PERFORM exec_rfc.
* interface log
  PERFORM create_interface_log.

ENDFORM.                    " interface_process_others
*&---------------------------------------------------------------------*
*&      Form  read_po_sucess
*&---------------------------------------------------------------------*
*       read_po_sucess
*&---------------------------------------------------------------------*
FORM read_po_sucess.

  SELECT * INTO TABLE it_basic
           FROM ztmm_pur_order
       WHERE bedat  IN      s_bedat
       AND   lifnr  LIKE    p_lifnr
       AND   matnr  LIKE    p_matnr
       AND   eindt  IN      s_eindt
       AND   flag   EQ      'S'.

ENDFORM.                    " read_po_sucess
*&---------------------------------------------------------------------*
*&      Form  read_processing_released
*&---------------------------------------------------------------------*
*       read_processing_released
*----------------------------------------------------------------------*
FORM read_processing_released.

  SELECT  a~bsart  b~pstyp  a~bukrs
          b~werks  b~ebeln  b~ebelp
          b~matnr  a~lifnr  c~menge
          b~meins  c~eindt  b~netpr
          a~waers  a~bedat  c~etenr
          b~lgort  c~charg

        INTO CORRESPONDING FIELDS OF TABLE it_basic1
        FROM ekko AS a INNER JOIN ekpo AS b
          ON a~mandt  EQ  b~mandt
         AND a~ebeln  EQ  b~ebeln
           INNER JOIN eket AS c
              ON b~mandt  EQ  c~mandt
             AND b~ebeln  EQ  c~ebeln
             AND b~ebelp  EQ  c~ebelp
       WHERE a~bedat  IN      s_bedat
       AND   a~lifnr  LIKE    p_lifnr
       AND   b~matnr  LIKE    p_matnr
       AND   b~matnr  LIKE    'AM%'
       AND   c~eindt  IN      s_eindt
       AND   b~loekz  EQ      ''
       AND   a~bsart  EQ      'NB'    "po
       AND   a~frgke  EQ      'F'     "release
       AND   b~pstyp  EQ      '3'.    "subcontracting
*  if sy-subrc eq 0.
*  endif.
  SORT it_basic1 BY ebeln ebelp.


*/Begin of Added by Hakchin(20040422)
  DATA: lv_subrc LIKE sy-subrc.
*/End of Added by Hakchin(20040422)

  LOOP AT it_basic1 INTO wa_basic1.
*/Begin of Added by Hakchin(20040422)
    it_basic1 = wa_basic1.
*/End of Added by Hakchin(20040422)
* get material text
    PERFORM read_material_text  USING wa_basic1-matnr
                                      wa_basic1-maktx.
* get vendor text
    PERFORM read_vendor_text    USING wa_basic1-lifnr
                                      wa_basic1-name1.
* get characteristic
    CLEAR wa_zsmm_class.
    CALL FUNCTION 'Z_FMM_GET_CHARACT'
         EXPORTING
              i_matnr        = wa_basic1-matnr
         IMPORTING
              es_zsmm_class  = wa_zsmm_class
         EXCEPTIONS
              data_not_found = 1
              OTHERS         = 2.
    IF sy-subrc EQ 0.
      wa_basic1-prop        =  wa_zsmm_class-zprop.
      wa_basic1-coating     =  wa_zsmm_class-zcoating.
      wa_basic1-thick       =  wa_zsmm_class-zthick.
      wa_basic1-width       =  wa_zsmm_class-zwidth.
      wa_basic1-length      =  wa_zsmm_class-zlength.
    ENDIF.
* get component
    PERFORM read_componenet     USING wa_basic1.
* others
    CLEAR *t163y.
    SELECT SINGLE epstp  INTO *t163y-epstp
                  FROM  t163y
                 WHERE spras  EQ  sy-langu
                 AND   pstyp  EQ  wa_basic1-pstyp.
    CONCATENATE wa_basic1-bsart  *t163y-epstp
                    INTO wa_basic1-doc_type. "doc type
    wa_basic1-lgpro_f  =  wa_basic1-lgort.   "from storg.loc
    wa_basic1-lgpro_e  =  wa_basic1-lifnr.   "to storg.loc

* interface flag
    SELECT SINGLE flag INTO wa_basic1-flag
          FROM ztmm_pro_info
         WHERE doc_type  =  wa_basic1-doc_type
         AND   bukrs     =  wa_basic1-bukrs
         AND   werks     =  wa_basic1-werks
         AND   ebeln     =  wa_basic1-ebeln
         AND   ebelp     =  wa_basic1-ebelp
         AND   etenr     =  wa_basic1-etenr
         AND   matnr     =  wa_basic1-matnr.
*/Begin of Added by Hakchin(20040422)
    lv_subrc = sy-subrc.
*/End of Added by Hakchin(20040422)
    MODIFY it_basic1 FROM wa_basic1.
*/Begin of Added by Hakchin(20040422)
    IF lv_subrc = 0.
      DELETE it_basic1.
    ENDIF.
*/End of Added by Hakchin(20040422)
  ENDLOOP.

ENDFORM.                    " read_processing_released
*&---------------------------------------------------------------------*
*&      Form  read_processing_sucess
*&---------------------------------------------------------------------*
*       read_processing_sucess
*----------------------------------------------------------------------*
FORM read_processing_sucess.

  SELECT * INTO TABLE it_basic1
           FROM ztmm_pro_info
       WHERE lifnr  LIKE    p_lifnr
       AND   matnr  LIKE    p_matnr
       AND   eindt  IN      s_eindt
       AND   flag   EQ      'S'.

ENDFORM.                    " read_processing_sucess
*&---------------------------------------------------------------------*
*&      Form  read_processing_error
*&---------------------------------------------------------------------*
*       read_processing_error
*----------------------------------------------------------------------*
FORM read_processing_error.

  SELECT * INTO TABLE it_basic1
           FROM ztmm_pro_info
       WHERE lifnr  LIKE    p_lifnr
       AND   matnr  LIKE    p_matnr
       AND   eindt  IN      s_eindt
       AND   flag   EQ      'E'.

ENDFORM.                    " read_processing_error
*&---------------------------------------------------------------------*
*&      Form  read_componenet
*&---------------------------------------------------------------------*
*        read_componenet
*----------------------------------------------------------------------*
FORM read_componenet USING  pa_basic1 LIKE it_basic1.

  DATA  : lt_vlcporder_it  LIKE TABLE OF vlcporder
                                WITH HEADER LINE,
          lt_resb_et       LIKE TABLE OF resb
                                WITH HEADER LINE.
  CLEAR : it_comp[], it_comp,
          lt_vlcporder_it[], lt_vlcporder_it,
          lt_resb_et[], lt_resb_et.

*  SELECT  matnr  nomng
*         INTO TABLE it_comp
*         FROM resb
*        WHERE ebeln  EQ  pa_header-ebeln
*        AND   ebelp  EQ  pa_header-ebelp.

* modify by tuning--> but not improve

  lt_vlcporder_it-ebeln  =  pa_basic1-ebeln.
  lt_vlcporder_it-ebelp  =  pa_basic1-ebelp.
  APPEND  lt_vlcporder_it.

  CALL FUNCTION 'VELO14_READ_COMPONENTS_OF_PO'
       TABLES
            vlcporder_it          = lt_vlcporder_it
            resb_et               = lt_resb_et
       EXCEPTIONS
            no_data_received      = 1
            nothing_found_in_eket = 2
            nothing_found_in_resb = 3
            OTHERS                = 4.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  LOOP AT  lt_resb_et.
    it_comp-matnr  =  lt_resb_et-matnr.
    it_comp-nomng  =  lt_resb_et-nomng.
    APPEND it_comp.
  ENDLOOP.

  LOOP AT it_comp.
    CASE sy-tabix.
      WHEN 1.
        pa_basic1-matnr_c1  = it_comp-matnr.
        pa_basic1-erfmg_c1  = it_comp-nomng.
        PERFORM read_material_text USING pa_basic1-matnr_c1
                                         pa_basic1-maktx_c1.
      WHEN 2.
        pa_basic1-matnr_c2  = it_comp-matnr.
        pa_basic1-erfmg_c2  = it_comp-nomng.
        PERFORM read_material_text USING pa_basic1-matnr_c2
                                         pa_basic1-maktx_c2.
      WHEN 3.
        pa_basic1-matnr_c3  = it_comp-matnr.
        pa_basic1-erfmg_c3  = it_comp-nomng.
        PERFORM read_material_text USING pa_basic1-matnr_c3
                                         pa_basic1-maktx_c3.
      WHEN 4.
        pa_basic1-matnr_c4  = it_comp-matnr.
        pa_basic1-erfmg_c4  = it_comp-nomng.
        PERFORM read_material_text USING pa_basic1-matnr_c4
                                         pa_basic1-maktx_c4.
      WHEN 5.
        pa_basic1-matnr_c5  = it_comp-matnr.
        pa_basic1-erfmg_c5  = it_comp-nomng.
        PERFORM read_material_text USING pa_basic1-matnr_c5
                                         pa_basic1-maktx_c5.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " read_componenet
*&---------------------------------------------------------------------*
*&      Form  screen_po
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM screen_po.

* move local itab only marking
  CASE sy-dynnr.
    WHEN '0100'.
      LOOP AT it_header WHERE mark = 'X'.
        MOVE-CORRESPONDING it_header  TO it_rfc.
        APPEND it_rfc. CLEAR it_rfc.
        it_header-mark = ''.
        MODIFY it_header TRANSPORTING mark.
      ENDLOOP.
    WHEN '0110'.
      LOOP AT it_po WHERE mark = 'X'.
        LOOP AT it_basic WHERE ebeln = it_po-ebeln.
          MOVE-CORRESPONDING it_basic TO it_rfc.
          APPEND it_rfc. CLEAR it_rfc.
        ENDLOOP.
        it_po-mark = ''.
        MODIFY it_po TRANSPORTING mark.
      ENDLOOP.
    WHEN '0120'.
      LOOP AT it_material WHERE mark = 'X'.
        LOOP AT it_basic WHERE matnr = it_material-matnr.
          MOVE-CORRESPONDING it_basic TO it_rfc.
          APPEND it_rfc. CLEAR it_rfc.
        ENDLOOP.
        it_material-mark = ''.
        MODIFY it_material TRANSPORTING mark.
      ENDLOOP.
    WHEN '0130'.
      LOOP AT it_poitem WHERE mark = 'X'.
        LOOP AT it_basic WHERE ebeln = it_poitem-ebeln
                         AND   ebelp = it_poitem-ebelp.
          MOVE-CORRESPONDING it_basic TO it_rfc.
          APPEND it_rfc. CLEAR it_rfc.
        ENDLOOP.
        it_poitem-mark = ''.
        MODIFY it_poitem TRANSPORTING mark.
      ENDLOOP.
  ENDCASE.

* execute rfc fm on all data and update ztable
  PERFORM exec_rfc.
* interface log
  PERFORM create_interface_log.

ENDFORM.                    " screen_po
*&---------------------------------------------------------------------*
*&      Form  screen_processing
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM screen_processing.

* move local itab only marking
  CASE sy-dynnr.
    WHEN '0200'.
      LOOP AT it_header1 WHERE mark = 'X'.
        MOVE-CORRESPONDING it_header1  TO it_rfc1.
        APPEND it_rfc1. CLEAR it_rfc1.
        it_header1-mark = ''.
        MODIFY it_header1 TRANSPORTING mark.
      ENDLOOP.
    WHEN '0110'.
      LOOP AT it_po WHERE mark = 'X'.
        LOOP AT it_basic1 WHERE ebeln = it_po-ebeln.
          MOVE-CORRESPONDING it_basic1 TO it_rfc1.
          APPEND it_rfc1. CLEAR it_rfc1.
        ENDLOOP.
        it_po-mark = ''.
        MODIFY it_po TRANSPORTING mark.
      ENDLOOP.
    WHEN '0120'.
      LOOP AT it_material WHERE mark = 'X'.
        LOOP AT it_basic1 WHERE matnr = it_material-matnr.
          MOVE-CORRESPONDING it_basic1 TO it_rfc1.
          APPEND it_rfc1. CLEAR it_rfc1.
        ENDLOOP.
        it_material-mark = ''.
        MODIFY it_material TRANSPORTING mark.
      ENDLOOP.
    WHEN '0130'.
      LOOP AT it_poitem WHERE mark = 'X'.
        LOOP AT it_basic1 WHERE ebeln = it_poitem-ebeln
                          AND   ebelp = it_poitem-ebelp.
          MOVE-CORRESPONDING it_basic1 TO it_rfc1.
          APPEND it_rfc1. CLEAR it_rfc1.
        ENDLOOP.
        it_poitem-mark = ''.
        MODIFY it_poitem TRANSPORTING mark.
      ENDLOOP.
  ENDCASE.

* execute rfc fm on all data and update ztable
  PERFORM exec_rfc_processing.
* interface log
  PERFORM create_interface_log_pro.

ENDFORM.                    " screen_processing
*&---------------------------------------------------------------------*
*&      Form  exec_rfc_processing
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM exec_rfc_processing.
  DATA : lw_msgtxt(255),
         lw_tabix  LIKE sy-tabix.
  CLEAR: lw_msgtxt, lw_tabix.

* RFC function call
  CHECK NOT it_rfc1[] IS INITIAL.
  CALL FUNCTION 'Z_FMM_SET_PROCESSING_INFO'
    DESTINATION              c_dest
    TABLES
      et_ztmm_pro_info       = it_rfc1
    EXCEPTIONS
      communication_failure  = 1  MESSAGE lw_msgtxt
      system_failure         = 2  MESSAGE lw_msgtxt.
* success, fail log update
  LOOP AT it_rfc1.
    lw_tabix = sy-tabix.

    it_rfc1-zuser  =   sy-uname.
    it_rfc1-zsdat  =   sy-datum.
    it_rfc1-zstim  =   sy-uzeit.
    it_rfc1-zmode  =   'C'.

    IF it_rfc1-zzret = 'S'.
      it_rfc1-zmsg   =   'Processing info success'.
      it_rfc1-flag   =   it_rfc1-zzret.
      MODIFY it_rfc1 INDEX lw_tabix.
    ELSE.
      IF lw_msgtxt NE ''.
        it_rfc1-zmsg   = lw_msgtxt.
      ELSE.
        it_rfc1-zmsg   =  'Processing info fail'.
      ENDIF.
      it_rfc1-zzret = 'E'.
      it_rfc1-flag  = it_rfc1-zzret.
      MODIFY it_rfc1 INDEX lw_tabix.
    ENDIF.
  ENDLOOP.

* modify ztable
  MODIFY  ztmm_pro_info FROM TABLE it_rfc1.
  IF sy-subrc EQ 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

*  LOOP AT it_header1.
*    READ TABLE it_rfc1 WITH KEY doc_type  = it_header1-doc_type
*                                bukrs     = it_header1-bukrs
*                                werks     = it_header1-werks
*                                ebeln     = it_header1-ebeln
*                                ebelp     = it_header1-ebelp
*                                etenr     = it_header1-etenr
*                                matnr     = it_header1-matnr.
*    IF sy-subrc EQ 0.
*      it_header1-flag = it_rfc1-flag.
*      MODIFY it_header1 TRANSPORTING flag.
*    ENDIF.
*  ENDLOOP.

ENDFORM.                    " exec_rfc_processing
*&---------------------------------------------------------------------*
*&      Form  CREATE_INTERFACE_LOG_PRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM create_interface_log_pro.

  CLEAR : wa_ztca_if_log, w_total.
  DESCRIBE TABLE it_rfc1 LINES w_total.
  LOOP AT it_rfc1.
    IF     it_rfc1-flag = 'S'.
      wa_ztca_if_log-zsucc = wa_ztca_if_log-zsucc + 1.
    ELSEIF it_rfc1-flag = 'E'.
      wa_ztca_if_log-error = wa_ztca_if_log-error + 1.
    ENDIF.
  ENDLOOP.

  CHECK w_total <> 0.
  wa_ztca_if_log-tcode    = 'ZMMR16'.
*  wa_ZTCA_IF_LOG-ZSLNO    = WA_JOB-SLNO.
*  wa_ZTCA_IF_LOG-JOBCOUNT = WA_JOB-INT.
  wa_ztca_if_log-total    = w_total.
  wa_ztca_if_log-erdat    = sy-datum. "Created on.
  wa_ztca_if_log-erzet    = sy-uzeit. "Created time.
  wa_ztca_if_log-ernam    = sy-uname. "Created by.

  CALL FUNCTION 'Z_FCA_EAI_INTERFACE_LOG'
    EXPORTING
      i_ztca_if_log              = wa_ztca_if_log
*   IMPORTING
*     E_ZTCA_IF_LOG              =
   EXCEPTIONS
     update_failed              = 1
     number_range_error         = 2
     tcode_does_not_exist       = 3
     OTHERS                     = 4
            .
  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* message
  IF     w_total  =  wa_ztca_if_log-zsucc.
    MESSAGE s028. LEAVE TO SCREEN sy-dynnr.
  ELSEIF w_total  =  wa_ztca_if_log-error.
*    MESSAGE e029. LEAVE TO SCREEN sy-dynnr.
    MESSAGE s999 WITH 'EAI Error Occurred!'.
    LEAVE TO SCREEN sy-dynnr.
  ELSE.
*    MESSAGE e029. LEAVE TO SCREEN sy-dynnr.
    MESSAGE s999 WITH 'EAI Error Occurred!'.
    LEAVE TO SCREEN sy-dynnr.
  ENDIF.


ENDFORM.                    " CREATE_INTERFACE_LOG_PRO
*&---------------------------------------------------------------------*
*&      Module  modify_screen  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_screen OUTPUT.

  LOOP AT SCREEN.
    IF screen-name  =  'B_DELE'.
      IF      rv1  =  'X'.
        screen-invisible  = '1'.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

ENDMODULE.                 " modify_screen  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  delete_temp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM delete_temp.

  IF      rp1  =  'X'.
    PERFORM delete_po.
  ELSEIF  rp2  =  'X'.
    PERFORM delete_process.
  ENDIF.

ENDFORM.                    " delete_temp
*&---------------------------------------------------------------------*
*&      Form  delete_po
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM delete_po.

  CASE sy-dynnr.
    WHEN '0100'.
      LOOP AT it_header WHERE mark = 'X'.
        MOVE-CORRESPONDING it_header TO it_ztmm_pur_order.
        APPEND it_ztmm_pur_order. CLEAR it_ztmm_pur_order.
      ENDLOOP.
    WHEN '0110'.
      LOOP AT it_po WHERE mark = 'X'.
        LOOP AT it_basic WHERE ebeln = it_po-ebeln.
          MOVE-CORRESPONDING it_basic TO it_ztmm_pur_order.
          APPEND it_ztmm_pur_order. CLEAR it_ztmm_pur_order.
        ENDLOOP.
      ENDLOOP.
    WHEN '0120'.
      LOOP AT it_material WHERE mark = 'X'.
        LOOP AT it_basic WHERE matnr = it_material-matnr.
          MOVE-CORRESPONDING it_basic TO it_ztmm_pur_order.
          APPEND it_ztmm_pur_order. CLEAR it_ztmm_pur_order.
        ENDLOOP.
      ENDLOOP.
    WHEN '0130'.
      LOOP AT it_poitem WHERE mark = 'X'.
        LOOP AT it_basic WHERE ebeln = it_poitem-ebeln
                         AND   ebelp = it_poitem-ebelp.
          MOVE-CORRESPONDING it_basic TO it_ztmm_pur_order.
          APPEND it_ztmm_pur_order. CLEAR it_ztmm_pur_order.
        ENDLOOP.
      ENDLOOP.
  ENDCASE.

  DELETE ztmm_pur_order FROM TABLE it_ztmm_pur_order.
*/Begin of Added by Hakchin(20040423)
*  IF sy-subrc = 0. CLEAR: it_ztmm_pur_order[]. ENDIF.
  LOOP AT it_header.
    LOOP AT it_ztmm_pur_order
              WHERE doc_type  =  it_header-doc_type AND
                    bukrs     =  it_header-bukrs    AND
                    werks     =  it_header-werks    AND
                    ebeln     =  it_header-ebeln    AND
                    ebelp     =  it_header-ebelp    AND
                    etenr     =  it_header-etenr    AND
                    matnr     =  it_header-matnr.

      DELETE it_header.
      EXIT.
    ENDLOOP.
  ENDLOOP.
*/End of Added by Hakchin(20040423)
ENDFORM.                    " delete_po
*&---------------------------------------------------------------------*
*&      Form  delete_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM delete_process.

  CASE sy-dynnr.
    WHEN '0200'.
      LOOP AT it_header1 WHERE mark = 'X'.
        MOVE-CORRESPONDING it_header1 TO it_ztmm_pro_info.
        APPEND it_ztmm_pro_info. CLEAR it_ztmm_pro_info.
      ENDLOOP.
    WHEN '0110'.
      LOOP AT it_po WHERE mark = 'X'.
        LOOP AT it_basic1 WHERE ebeln = it_po-ebeln.
          MOVE-CORRESPONDING it_basic1 TO it_ztmm_pro_info.
          APPEND it_ztmm_pro_info. CLEAR it_ztmm_pro_info.
        ENDLOOP.
      ENDLOOP.
    WHEN '0120'.
      LOOP AT it_material WHERE mark = 'X'.
        LOOP AT it_basic1 WHERE matnr = it_material-matnr.
          MOVE-CORRESPONDING it_basic1 TO it_ztmm_pro_info.
          APPEND it_ztmm_pro_info. CLEAR it_ztmm_pro_info.
        ENDLOOP.
      ENDLOOP.
    WHEN '0130'.
      LOOP AT it_poitem WHERE mark = 'X'.
        LOOP AT it_basic1 WHERE ebeln = it_poitem-ebeln
                          AND   ebelp = it_poitem-ebelp.
          MOVE-CORRESPONDING it_basic1 TO it_ztmm_pro_info.
          APPEND it_ztmm_pro_info. CLEAR it_ztmm_pro_info.
        ENDLOOP.
      ENDLOOP.
  ENDCASE.

  DELETE ztmm_pro_info FROM TABLE it_ztmm_pro_info.
*/Begin of Added by Hakchin(20040423)
*  IF sy-subrc = 0. CLEAR: it_ztmm_pro_info[]. ENDIF.
  LOOP AT it_header.
    LOOP AT it_ztmm_pro_info
              WHERE doc_type  =  it_header-doc_type AND
                    bukrs     =  it_header-bukrs    AND
                    werks     =  it_header-werks    AND
                    ebeln     =  it_header-ebeln    AND
                    ebelp     =  it_header-ebelp    AND
                    etenr     =  it_header-etenr    AND
                    matnr     =  it_header-matnr.

      DELETE it_header.
      EXIT.
    ENDLOOP.
  ENDLOOP.
*/End of Added by Hakchin(20040423)
ENDFORM.                    " delete_process
