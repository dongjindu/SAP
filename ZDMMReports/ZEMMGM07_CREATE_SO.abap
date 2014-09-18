************************************************************************
* Program name: ZEMMGM07_CREATE_SO                                     *
* Created by  : hj.song                                                *
* Created on  : 2003.10.01                                             *
* Pattern     :
* Description : Create sales order from cutting request                *
*
* Modification Log                                                     *
* Date            Developer        Request No.    Description          *
* 2003.10.14.     hj.song          UD1K902172     Initial Coding       *
*                                                                      *
************************************************************************

*&---------------------------------------------------------------------*
*& Report  ZEMMGM07_CREATE_SO                                          *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  zemmgm07_create_so  MESSAGE-ID zmmm.


TABLES : ztmm_so_inf,
         t001w,
         makt,
         lfa1,
         usr01.

* itab
DATA : BEGIN OF it_ztmm_so_inf OCCURS 0.
INCLUDE    STRUCTURE ztmm_so_inf.
DATA :  status(8),
        maktx      LIKE  makt-maktx,
        name2      LIKE  lfa1-name1,
        mark,
       END OF it_ztmm_so_inf.

* bdc itab
DATA : BEGIN OF bdc_tab OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA : END OF bdc_tab.

DATA : BEGIN OF mess_tab OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA : END OF mess_tab.

* screen data
DATA : s_werks         LIKE  t001w-werks,
       s_name1         LIKE  t001w-name1,
       s_item_no       LIKE  ztmm_so_inf-item_no,
       s_maktx         LIKE  makt-maktx,
       s_depen_place   LIKE  ztmm_so_inf-depen_place,
       s_name2         LIKE  lfa1-name1,
       s_matl          LIKE  ztmm_so_inf-matl,
       s_coat_qty      LIKE  ztmm_so_inf-coat_qty,
       s_thick         LIKE  ztmm_so_inf-thick,
       s_width         LIKE  ztmm_so_inf-width,
       s_length        LIKE  ztmm_so_inf-length,
       s_request_date_f LIKE ztmm_so_inf-request_date,
       s_request_date_e LIKE ztmm_so_inf-request_date,
       rb1, rb2, rb3.

*
DATA : save_ok     LIKE  sy-ucomm,
       ok_code     LIKE  sy-ucomm,

       w_loop_cnt  LIKE  sy-loopc,
       w_tcname    LIKE  feld-name,
       w_mode(1)   TYPE  c VALUE 'N',
       w_message   TYPE  bapi_msg,
* bdc data
       w_bdc_auart  LIKE  vbak-auart,
       w_bdc_vkorg  LIKE  vbak-vkorg,
       w_bdc_vtweg  LIKE  vbak-vtweg,
       w_bdc_spart  LIKE  vbak-spart,
       w_last_date1 LIKE  sy-datum,
       w_last_date2 LIKE  sy-datum,
       w_date1      LIKE  sy-datum,
       w_posnr(6)   TYPE  n.


* table control
CONTROLS : tc0200 TYPE TABLEVIEW USING SCREEN 0200.

* Field SYMBOL
FIELD-SYMBOLS <tc>  TYPE cxtab_control.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET PF-STATUS '0100'.
  SET TITLEBAR  '0100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  save_ok = ok_code.
  CLEAR ok_code.

  CASE save_ok.
* select basic data
    WHEN 'SEAR'.
      PERFORM select_data.
      PERFORM modify_it_ztmm_so_inf.
      REFRESH CONTROL 'TC0200' FROM SCREEN '0200'.
      CALL SCREEN '0200'.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  select_data
*&---------------------------------------------------------------------*
*       select basic data
*----------------------------------------------------------------------*
FORM select_data.

  PERFORM set_parameters.

  CLEAR : it_ztmm_so_inf[], it_ztmm_so_inf.

  IF     rb1 = 'X'.      "in approval
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_ztmm_so_inf
           FROM ztmm_so_inf
          WHERE item_no     =    s_item_no
          AND   depen_place LIKE s_depen_place
          AND   matl        LIKE s_matl
          AND   coat_qty    LIKE s_coat_qty
          AND   thick       =    s_thick
          AND   width       =    s_width
          AND   length      =    s_length
*          and   request_date
*                between s_request_date_f and s_request_date_e
          AND   so_stat     =    ''.
  ELSEIF rb2 = 'X'.      "finished
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_ztmm_so_inf
           FROM ztmm_so_inf
          WHERE item_no     =    s_item_no
          AND   depen_place LIKE s_depen_place
          AND   matl        LIKE s_matl
          AND   coat_qty    LIKE s_coat_qty
          AND   thick       =    s_thick
          AND   width       =    s_width
          AND   length      =    s_length
*          and   request_date
*                between s_request_date_f and s_request_date_e
          AND   so_stat     =   'B'.
  ELSEIF rb3 = 'X'.       ""finished, change, delete, stand by
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_ztmm_so_inf
           FROM ztmm_so_inf
          WHERE item_no     =    s_item_no
          AND   depen_place LIKE s_depen_place
          AND   matl        LIKE s_matl
          AND   coat_qty    LIKE s_coat_qty
          AND   thick       =    s_thick
          AND   width       =    s_width
*          and   request_date
*                between s_request_date_f and s_request_date_e
          AND   length      =    s_length.
  ENDIF.

ENDFORM.                    " select_data
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       exit
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

ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Form  set_parameters
*&---------------------------------------------------------------------*
*       init parameters
*----------------------------------------------------------------------*
FORM set_parameters.

  IF  s_item_no     = ''.  s_item_no     = '%'.  ENDIF.
  IF  s_depen_place = ''.  s_depen_place = '%'.  ENDIF.
  IF  s_matl        = ''.  s_matl        = '%'.  ENDIF.
  IF  s_coat_qty    = ''.  s_coat_qty    = '%'.  ENDIF.
*  if  s_thick       = ''.  s_thick       = '99999'.  endif.
*  if  s_width       = ''.  s_width       = '99999'.  endif.
*  if  s_length      = ''.  s_length      = '99999'.  endif.

ENDFORM.                    " set_parameters
*&---------------------------------------------------------------------*
*&      Module  INIT_SCREEN0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE init_screen0100 OUTPUT.

  CLEAR : s_werks,    s_item_no,  s_depen_place, s_matl,
          s_coat_qty, s_thick,    s_width,       s_length.

  s_werks = 'P001'.

ENDMODULE.                 " INIT_SCREEN0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  read_plant  INPUT
*&---------------------------------------------------------------------*
MODULE read_plant INPUT.

  PERFORM read_plant USING s_werks  s_name1.

ENDMODULE.                 " read_plant  INPUT
*&---------------------------------------------------------------------*
*&      Module  read_material  INPUT
*&---------------------------------------------------------------------*
MODULE read_material INPUT.

  PERFORM read_material USING s_item_no  s_maktx.

ENDMODULE.                 " read_material  INPUT
*&---------------------------------------------------------------------*
*&      Module  read_vendor  INPUT
*&---------------------------------------------------------------------*
MODULE read_vendor INPUT.

  PERFORM read_vendor USING s_depen_place  s_name2.

ENDMODULE.                 " read_vendor  INPUT
*&---------------------------------------------------------------------*
*&      Form  read_plant
*&---------------------------------------------------------------------*
FORM read_plant USING    p_werks   p_text.

  SELECT SINGLE name1 INTO p_text
         FROM t001w
        WHERE werks = p_werks.
  CHECK sy-subrc <> 0.
  MESSAGE e013 WITH p_werks.

ENDFORM.                    " read_plant
*&---------------------------------------------------------------------*
*&      Form  read_material
*&---------------------------------------------------------------------*
FORM read_material USING    p_matnr  p_maktx.

  DATA : lw_matnr  LIKE  mara-matnr.
  CLEAR: lw_matnr.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
            input  = p_matnr
       IMPORTING
            output = lw_matnr.

  SELECT SINGLE maktx INTO p_maktx
       FROM makt
      WHERE matnr = lw_matnr
      AND   spras = sy-langu.
  CHECK sy-subrc <> 0.
  MESSAGE e013 WITH lw_matnr.

ENDFORM.                    " read_material
*&---------------------------------------------------------------------*
*&      Form  read_vendor
*&---------------------------------------------------------------------*
FORM read_vendor USING    p_lifnr  p_name1.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
            input  = p_lifnr
       IMPORTING
            output = p_lifnr.

  SELECT SINGLE name1 INTO p_name1
       FROM lfa1
      WHERE lifnr = p_lifnr.
  CHECK sy-subrc <> 0.
  MESSAGE e013 WITH p_lifnr.

ENDFORM.                    " read_vendor
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0200 OUTPUT.

  SET PF-STATUS '0200'.
  SET TITLEBAR  '0200'.

ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_0200  INPUT
*&---------------------------------------------------------------------*
*       user command 0200
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

  save_ok = ok_code.
  CLEAR ok_code.

  CASE save_ok.
* create sales order
    WHEN 'APPO'.
      PERFORM check_line_selection.
      PERFORM check_status USING save_ok.
      PERFORM creat_sales_order.
* change_sales_order
    WHEN 'CHAN'.
      PERFORM check_line_selection.
      PERFORM check_status USING save_ok.
      PERFORM change_sales_order.
* delete_sales_order
    WHEN 'DELE'.
      PERFORM check_line_selection.
      PERFORM check_status USING save_ok.
      PERFORM delete_sales_order.
* page up, down
    WHEN 'P-' OR 'P--' OR 'P++' OR 'P+'.
      PERFORM page_scroll USING save_ok.
  ENDCASE.

ENDMODULE.                 " user_command_0200  INPUT
*&---------------------------------------------------------------------*
*&      Module  loop_cnt  OUTPUT
*&---------------------------------------------------------------------*
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

  IF     sy-dynnr = '0200'.
    DESCRIBE TABLE it_ztmm_so_inf LINES <tc>-lines.
  ENDIF.

ENDMODULE.                 " set_tc_line  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  it_ztmm_so_inf-mark  INPUT
*&---------------------------------------------------------------------*
MODULE it_ztmm_so_inf-mark INPUT.

  it_ztmm_so_inf-mark = 'X'.

ENDMODULE.                 " it_ztmm_so_inf-mark  INPUT
*&---------------------------------------------------------------------*
*&      Module  it_ztmm_so_inf_modify  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE it_ztmm_so_inf_modify INPUT.

  MODIFY it_ztmm_so_inf INDEX tc0200-current_line TRANSPORTING mark.

ENDMODULE.                 " it_ztmm_so_inf_modify  INPUT
*&---------------------------------------------------------------------*
*&      Form  modify_it_ztmm_so_inf
*&---------------------------------------------------------------------*
FORM modify_it_ztmm_so_inf.

  LOOP AT it_ztmm_so_inf.
    PERFORM read_material USING it_ztmm_so_inf-item_no
                                it_ztmm_so_inf-maktx.
    PERFORM read_vendor   USING it_ztmm_so_inf-depen_place
                                it_ztmm_so_inf-name2.
    PERFORM read_status   USING it_ztmm_so_inf-so_stat
                                it_ztmm_so_inf-status.

    MODIFY it_ztmm_so_inf. CLEAR it_ztmm_so_inf.
  ENDLOOP.

ENDFORM.                    " modify_it_ztmm_so_inf
*&---------------------------------------------------------------------*
*&      Form  read_status
*&---------------------------------------------------------------------*
*       read sales order status
*----------------------------------------------------------------------*
FORM read_status USING    p_so_stat   p_status.

  CASE  p_so_stat.
    WHEN 'B'.
      p_status = 'Finish'.
    WHEN 'C'.
      p_status = 'Change'.
    WHEN 'D'.
      p_status = 'Delete'.
    WHEN OTHERS.
      p_status = 'Stand by'.
  ENDCASE.

ENDFORM.                    " read_status
*&---------------------------------------------------------------------*
*&      Form  creat_sales_order
*&---------------------------------------------------------------------*
FORM creat_sales_order.

  LOOP AT it_ztmm_so_inf WHERE mark = 'X'.
    PERFORM check_dummy.
    PERFORM creat_so_bdc.
    PERFORM bdc_result USING 'B'.
  ENDLOOP.

ENDFORM.                    " creat_sales_order
*&---------------------------------------------------------------------*
*&      Form  change_sales_order
*&---------------------------------------------------------------------*
FORM change_sales_order.

  LOOP AT it_ztmm_so_inf WHERE mark = 'X'.
    PERFORM change_so_bdc.
    PERFORM bdc_result USING 'C'.
  ENDLOOP.

ENDFORM.                    " change_sales_order
*&---------------------------------------------------------------------*
*&      Form  delete_sales_order
*&---------------------------------------------------------------------*
FORM delete_sales_order.

  LOOP AT it_ztmm_so_inf WHERE mark = 'X'.
    PERFORM delete_so_bdc.
    PERFORM bdc_result USING 'D'.
  ENDLOOP.

ENDFORM.                    " delete_sales_order
*&---------------------------------------------------------------------*
*&      Form  page_scroll
*&---------------------------------------------------------------------*
*       page up down scroll
*----------------------------------------------------------------------*
FORM page_scroll USING    p_save_ok.

  CALL FUNCTION 'LOAN_TABLECONTROL_SCROLLING'
       EXPORTING
            i_ok_code                = p_save_ok
            i_visible_lines_in_table = '20'
       CHANGING
            c_tablecontrol           = tc0200.

ENDFORM.                    " page_scroll
*&---------------------------------------------------------------------*
*&      Form  creat_so_bdc
*&---------------------------------------------------------------------*
FORM creat_so_bdc.

  DATA : lw_matnr   LIKE   mara-matnr,
         lw_kwmeng(15).
  CLEAR: lw_matnr, lw_kwmeng.

  REFRESH: bdc_tab, mess_tab.
  CLEAR  : bdc_tab, mess_tab.

* conversion material code
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
            input  = it_ztmm_so_inf-item_no
       IMPORTING
            output = lw_matnr.
* move quantity
  WRITE  it_ztmm_so_inf-request_qty  TO  lw_kwmeng.

* header data
  "last day in next months
  CALL FUNCTION 'MONTH_PLUS_DETERMINE'
       EXPORTING
            months  = 1
            olddate = sy-datum
       IMPORTING
            newdate = w_last_date1.

  CALL FUNCTION 'MM_LAST_DAY_OF_MONTHS'
       EXPORTING
            day_in            = w_last_date1
       IMPORTING
            last_day_of_month = w_last_date1
       EXCEPTIONS
            day_in_no_date    = 1
            OTHERS            = 2.

  SELECT SINGLE *
         FROM usr01
        WHERE bname = sy-uname.
  CASE usr01-datfm.
    WHEN '1'. "DD.MM.YYYY
      w_last_date2+4(4) = w_last_date1+0(4).
      w_last_date2+2(2) = w_last_date1+4(2).
      w_last_date2+0(2) = w_last_date1+6(2).
    WHEN '2' OR '3'. "MM/DD/YYYY "MM-DD-YYYY
      w_last_date2+4(4) = w_last_date1+0(4).
      w_last_date2+0(2) = w_last_date1+4(2).
      w_last_date2+2(2) = w_last_date1+6(2).
    WHEN OTHERS.
      w_last_date2+0(4) = w_last_date1+0(4).
      w_last_date2+4(2) = w_last_date1+4(2).
      w_last_date2+6(2) = w_last_date1+6(2).
  ENDCASE.

  PERFORM bdc_fill USING :
          'X' 'SAPMV45A'             '0101',
          ' ' 'VBAK-AUART'           w_bdc_auart,  "so doc type
          ' ' 'VBAK-VKORG'           w_bdc_vkorg,  "sales org
          ' ' 'VBAK-VTWEG'           w_bdc_vtweg,  "dis.channel
          ' ' 'VBAK-SPART'           w_bdc_spart,  "division
          ' ' 'BDC_OKCODE'           '/00',
          'X' 'SAPMV45A'             '4001',
                                                   "Request no
          ' ' 'VBKD-BSTKD'           it_ztmm_so_inf-depen_no,
                                                   "Request vendor
          ' ' 'KUAGV-KUNNR'          it_ztmm_so_inf-depen_place,
          ' ' 'RV45A-KETDAT'         w_last_date2, "date
          ' ' 'BDC_OKCODE'           '/00'.

* item data
  w_posnr = 10.

  SELECT SINGLE *
         FROM usr01
        WHERE bname = sy-uname.
  CASE usr01-datfm.
    WHEN '1'. "DD.MM.YYYY
      w_date1+2(2) = it_ztmm_so_inf-request_date+0(2).
      w_date1+0(2) = it_ztmm_so_inf-request_date+2(2).
      w_date1+4(4) = it_ztmm_so_inf-request_date+4(4).
    WHEN '2' OR '3'. "MM/DD/YYYY "MM-DD-YYYY
      w_date1+0(2) = it_ztmm_so_inf-request_date+0(2).
      w_date1+2(2) = it_ztmm_so_inf-request_date+2(2).
      w_date1+4(4) = it_ztmm_so_inf-request_date+4(4).
    WHEN OTHERS.
      w_date1+4(2) = it_ztmm_so_inf-request_date+0(2).
      w_date1+6(2) = it_ztmm_so_inf-request_date+2(2).
      w_date1+0(4) = it_ztmm_so_inf-request_date+4(4).
  ENDCASE.

  PERFORM bdc_fill USING :
          'X' 'SAPMV45A'             '4001',
          ' ' 'BDC_OKCODE'           '=POAN',
          'X' 'SAPMV45A'             '4001',
          ' ' 'RV45A-MABNR(02)'      lw_matnr,   "mat.no
          ' ' 'RV45A-KWMENG(02)'     lw_kwmeng,  "req.quantity
          ' ' 'RV45A-ETDAT(02)'      w_date1,    "date
          ' ' 'BDC_OKCODE'           '/00',
          'X' 'SAPMV45A'             '4001',
          ' ' 'BDC_OKCODE'           '=POPO',
          'X' 'SAPMV45A'             '0251',
          ' ' 'RV45A-POSNR'           w_posnr,   "item no
          ' ' 'BDC_OKCODE'           '=POSI',
          'X' 'SAPMV45A'             '4001',
          ' ' 'RV45A-VBAP_SELKZ(01)' 'X',
          ' ' 'BDC_OKCODE'           '=PBES',
          'X' 'SAPMV45A'             '4003',
*          ' ' 'VBKD-BSTKD'           "order no
          ' ' 'BDC_OKCODE'           '/EBACK'.

  PERFORM bdc_fill USING :
            'X' 'SAPMV45A'             '4001',
            ' ' 'BDC_OKCODE'           '=SICH'.

  CALL TRANSACTION 'VA01' USING bdc_tab MODE w_mode
                                UPDATE 'S'
                                MESSAGES INTO mess_tab.



ENDFORM.                    " creat_so_bdc
*&---------------------------------------------------------------------*
*&      Form  check_dummy
*&---------------------------------------------------------------------*
*       modify dummy fields
*----------------------------------------------------------------------*
FORM check_dummy.

  IF w_bdc_auart = 'ZOSO'.
    w_bdc_vkorg = 'D100'.
    w_bdc_vtweg = '30'.
    w_bdc_spart = '40'.
  ELSE.
    w_bdc_vkorg = 'D100'.
    w_bdc_vtweg = '30'.
    w_bdc_spart = '30'.
  ENDIF.

ENDFORM.                    " check_dummy
*&---------------------------------------------------------------------*
*&      Form  BDC_FILL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM bdc_fill USING    p1 p2 p3.

  CLEAR bdc_tab.
  IF p1 = 'X'.
    bdc_tab-dynbegin = p1.
    bdc_tab-program  = p2.
    bdc_tab-dynpro   = p3.
  ELSE.
    bdc_tab-dynbegin = p1.
    bdc_tab-fnam     = p2.
    bdc_tab-fval     = p3.
  ENDIF.
  APPEND bdc_tab.

ENDFORM.                    " BDC_FILL
*&---------------------------------------------------------------------*
*&      Form  change_so_bdc
*&---------------------------------------------------------------------*
FORM change_so_bdc.

  DATA : lw_matnr   LIKE   mara-matnr,
         lw_kwmeng(15).
  CLEAR: lw_matnr, lw_kwmeng.

  REFRESH: bdc_tab, mess_tab.
  CLEAR  : bdc_tab, mess_tab.
* move quantity
  WRITE  it_ztmm_so_inf-fin_qty  TO  lw_kwmeng.

* header data
  PERFORM bdc_fill USING :
          'X' 'SAPMV45A'             '0102',
          ' ' 'VBAK-VBELN'           it_ztmm_so_inf-so_no, "so no
          ' ' 'BDC_OKCODE'           '/00'.

* item data
  w_posnr = 10.

  PERFORM bdc_fill USING :
          'X' 'SAPMV45A'             '4001',
          ' ' 'BDC_OKCODE'           '=POPO',
          'X' 'SAPMV45A'             '0251',
          ' ' 'RV45A-POSNR'          w_posnr,
          ' ' 'BDC_OKCODE'           '=POSI',
          'X' 'SAPMV45A'             '4001',
          ' ' 'RV45A-KWMENG(01)'     lw_kwmeng,
          ' ' 'BDC_OKCODE'           '=SICH'.

  CALL TRANSACTION 'VA02' USING bdc_tab MODE w_mode
                                UPDATE 'S'
                                MESSAGES INTO mess_tab.


ENDFORM.                    " change_so_bdc
*&---------------------------------------------------------------------*
*&      Form  delete_so_bdc
*&---------------------------------------------------------------------*
FORM delete_so_bdc.

  PERFORM bdc_fill USING :
        'X' 'SAPMV45A'             '0102',
        ' ' 'VBAK-VBELN'           it_ztmm_so_inf-so_no, "so no
        ' ' 'BDC_OKCODE'           '/00',
        'X' 'SAPMV45A'             '4001',
        ' ' 'BDC_OKCODE'           '/ELOES'.

  CALL TRANSACTION 'VA02' USING bdc_tab MODE w_mode
                          UPDATE 'S'
                          MESSAGES INTO mess_tab.

ENDFORM.                    " delete_so_bdc
*&---------------------------------------------------------------------*
*&      Form  MESSAGE_TEXT_BUILD
*&---------------------------------------------------------------------*
*       MESSAGE_TEXT_BUILD
*----------------------------------------------------------------------*
FORM message_text_build USING p_message.

  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
       EXPORTING
            msgid               = sy-msgid
            msgnr               = sy-msgno
            msgv1               = sy-msgv1
            msgv2               = sy-msgv2
            msgv3               = sy-msgv3
            msgv4               = sy-msgv4
       IMPORTING
            message_text_output = p_message.

ENDFORM.                    " MESSAGE_TEXT_BUILD
*&---------------------------------------------------------------------*
*&      Form  bdc_result
*&---------------------------------------------------------------------*
*       bdc result return
*----------------------------------------------------------------------*
*      -->P_0763   sales order doc no status
*----------------------------------------------------------------------*
FORM bdc_result USING    value(p_0763).

  DATA : lw_banfn  LIKE  vbep-banfn.
  CLEAR: lw_banfn.

  READ TABLE mess_tab WITH KEY msgtyp = 'E'.
  IF sy-subrc <> 0.
    CASE p_0763.
      WHEN 'B'.
        READ TABLE mess_tab WITH KEY msgtyp = 'S'
                                     msgid  = 'V1'
                                     msgnr  = '311'.
        CHECK sy-subrc EQ 0.
        SELECT SINGLE banfn INTO lw_banfn
               FROM vbep
              WHERE vbeln   = sy-msgv2
              AND   posnr   = w_posnr.
        MOVE : it_ztmm_so_inf-request_qty TO it_ztmm_so_inf-fin_qty,
               "sales order doc no
               sy-msgv2                   TO it_ztmm_so_inf-so_no,
               "purchase requisition
               lw_banfn                   TO it_ztmm_so_inf-pr_no,
               "so doc staus(finished)
               'B'                        TO it_ztmm_so_inf-so_stat,
               sy-datum                   TO it_ztmm_so_inf-aedat,
               sy-uzeit                   TO it_ztmm_so_inf-aezet,
               sy-uname                   TO it_ztmm_so_inf-aenam.
        MODIFY it_ztmm_so_inf.

        UPDATE ztmm_so_inf FROM it_ztmm_so_inf.
      WHEN 'C'.
        READ TABLE mess_tab WITH KEY msgtyp = 'S'
                                     msgid  = 'V1'
                                     msgnr  = '311'.
        CHECK sy-subrc EQ 0.
        MOVE : 'C'                        TO it_ztmm_so_inf-so_stat,
               sy-datum                   TO it_ztmm_so_inf-aedat,
               sy-uzeit                   TO it_ztmm_so_inf-aezet,
               sy-uname                   TO it_ztmm_so_inf-aenam.
        MODIFY it_ztmm_so_inf.

        UPDATE ztmm_so_inf FROM it_ztmm_so_inf.
      WHEN 'D'.
        READ TABLE mess_tab WITH KEY msgtyp = 'S'
                                     msgid  = 'V1'
                                     msgnr  = '008'.
        CHECK sy-subrc EQ 0.
        MOVE : 'D'                        TO it_ztmm_so_inf-so_stat,
               sy-datum                   TO it_ztmm_so_inf-aedat,
               sy-uzeit                   TO it_ztmm_so_inf-aezet,
               sy-uname                   TO it_ztmm_so_inf-aenam.
        MODIFY it_ztmm_so_inf.

        UPDATE ztmm_so_inf FROM it_ztmm_so_inf.
    ENDCASE.
    IF sy-subrc EQ 0.
      CLEAR w_message.
      PERFORM message_text_build USING w_message.
      MESSAGE s000 WITH w_message.
    ENDIF.
  ENDIF.

ENDFORM.                    " bdc_result
*&---------------------------------------------------------------------*
*&      Form  check_line_selection
*&---------------------------------------------------------------------*
FORM check_line_selection.

  DATA : lw_mark  TYPE  i.
  CLEAR: lw_mark.

  LOOP AT it_ztmm_so_inf WHERE mark EQ 'X'.
    ADD 1 TO lw_mark.
  ENDLOOP.

  IF lw_mark EQ 0.
    MESSAGE s999 WITH text-001.
    LEAVE TO SCREEN sy-dynnr.
  ENDIF.

ENDFORM.                    " check_line_selection
*&---------------------------------------------------------------------*
*&      Form  check_status
*&---------------------------------------------------------------------*
*       check sales order doc status
*---------------------------------------------------------------------*
*      -->P_SAVE_OK  ok code
*----------------------------------------------------------------------*
FORM check_status USING    p_save_ok.
* so creation => so doc status is sapce
* so changing => so doc status is finished
* so deletion => so doc status is finished or changing
  CASE p_save_ok.
    WHEN 'APPO'.
      IF it_ztmm_so_inf-so_stat NE ''.
        MESSAGE s999 WITH text-002.
        LEAVE TO SCREEN sy-dynnr.
      ENDIF.
    WHEN 'CHAN'.
      IF it_ztmm_so_inf-so_stat NE 'B'.
        MESSAGE s999 WITH text-002.
        LEAVE TO SCREEN sy-dynnr.
      ENDIF.
    WHEN 'DELE'.
      IF it_ztmm_so_inf-so_stat EQ ' '
      OR it_ztmm_so_inf-so_stat EQ 'D'.
        MESSAGE s999 WITH text-002.
        LEAVE TO SCREEN sy-dynnr.
      ENDIF.
  ENDCASE.

ENDFORM.                    " check_status
