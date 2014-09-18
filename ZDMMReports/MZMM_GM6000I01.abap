*----------------------------------------------------------------------*
*   INCLUDE MZMM_GM6000I01                                             *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  save_ok_code = ok_code.
  CLEAR ok_code.
  CASE sy-dynnr.
    WHEN '0100'.   "Main Screen
      CASE save_ok_code.
        WHEN 'EXIT'.
*          CALL METHOD crv_docking_container->free.
          LEAVE PROGRAM.
        WHEN 'CANC'.
          CALL METHOD crv_docking_container->free.
          LEAVE TO TRANSACTION sy-tcode.
      ENDCASE.
  ENDCASE.
ENDMODULE.                 " exit  INPUT
*&---------------------------------------------------------------------*
*&      Module  back  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE back INPUT.
  save_ok_code = ok_code.
  CLEAR ok_code.
  CHECK save_ok_code = 'BACK'.
  CASE sy-dynnr.
    WHEN 100.
      CALL METHOD crv_docking_container->free.
      LEAVE TO TRANSACTION sy-tcode.
  ENDCASE.
ENDMODULE.                 " back  INPUT
*&---------------------------------------------------------------------*
*&      Module  dynf  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE dynf INPUT.
  CHECK save_ok_code = 'DYNF'.   "Tree On/Off
  IF dynftext = c_tree_on.
*    CALL METHOD crv_docking_container->set_visible( C_visible_true ).
    CALL METHOD crv_docking_container->set_visible
      EXPORTING
        visible           = c_visible_true
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    dynftext = c_tree_off.   "Tree Off
  ELSE.
*    CALL METHOD crv_docking_container->set_visible( C_visible_false ).
    CALL METHOD crv_docking_container->set_visible
      EXPORTING
        visible           = c_visible_false
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    dynftext = c_tree_on.    "Tree On
  ENDIF.
ENDMODULE.                 " dynf  INPUT
*&---------------------------------------------------------------------*
*&      Module  get_data_from_sa_0110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_data_from_sa_0110 INPUT.
  CASE ss_0110.
    WHEN 9001.
      CALL FUNCTION 'Z_FMM_6000_01_GET_DATA'
           IMPORTING
                ex_bukrs         = w_bukrs
                ex_licode        = w_licode
                ex_opcode        = w_opcode
                ex_cblicode      = cblicode
                ex_werks         = w_werks
                ex_fl_once       = w_once_flg
                ex_visiblelines  = w_visiblelines
                ex_lines         = w_lines
                ex_top_line      = w_top_line
           TABLES
                ext_zsmm_6000_01 = it_exdata_zsmm_6000_01.

    WHEN 9002.

      CALL FUNCTION 'Z_FMM_6000_02_GET_DATA'
           IMPORTING
                ex_bukrs           = w_bukrs
                ex_matnr           = w_matnr
                ex_werks           = w_werks
                ex_ztconodisp      = w_ztconodisp
                ex_ztcono          = w_ztcono
                ex_zapplied_date   = w_zapplied_date
                ex_erdat           = w_erdat
                ex_zch_desc_cd     = w_zch_desc_cd
                ex_zch_reason_cd   = w_zch_reason_cd
                ex_zinv_process_cd = w_zinv_process_cd
                ex_zch_matnr1      = w_zch_matnr1
                ex_zch_matnr2      = w_zch_matnr2
                ex_zxfer_matnr     = w_zxfer_matnr
                ex_fl_once         = w_once_flg
                ex_visiblelines    = w_visiblelines
                ex_lines           = w_lines
                ex_top_line        = w_top_line
           TABLES
                ext_zsmm_6000_02   = it_exdata_zsmm_6000_02.
    WHEN 9003.
      CALL FUNCTION 'Z_FMM_6000_03_GET_DATA'
           IMPORTING
                ex_matnr         = w_matnr
                ex_werks         = w_werks
                ex_ztcono        = w_ztcono
                ex_fl_once       = w_once_flg
           TABLES
                ext_zsmm_6000_03 = it_exdata_zsmm_6000_03.

    WHEN 9004.
      CALL FUNCTION 'Z_FMM_6000_04_GET_DATA'
           IMPORTING
                ex_matnr         = w_matnr
                ex_werks         = w_werks
                ex_licode        = w_licode
                ex_opcode        = w_opcode
                ex_fl_once       = w_once_flg
           TABLES
                ext_zsmm_6000_04 = it_exdata_zsmm_6000_04.

    WHEN 9005.
      CALL FUNCTION 'Z_FMM_6000_05_GET_DATA'
           IMPORTING
                ex_matnr         = w_matnr
                ex_werks         = w_werks
                ex_licode        = w_licode
                ex_opcode        = w_opcode
                ex_fl_once       = w_once_flg
           TABLES
                ext_zsmm_6000_05 = it_exdata_zsmm_6000_05.
    WHEN 9006.
      CALL FUNCTION 'Z_FMM_6000_06_GET_DATA'
           IMPORTING
                ex_matnr         = w_matnr
                ex_werks         = w_werks
                ex_licode        = w_licode
                ex_opcode        = w_opcode
                ex_fl_once       = w_once_flg
           TABLES
                ext_zsmm_6000_06 = it_exdata_zsmm_6000_06.

  ENDCASE.
ENDMODULE.                 " get_data_from_sa_0110  INPUT
*&---------------------------------------------------------------------*
*&      Module  save  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE save INPUT.
  CHECK save_ok_code = 'SAVE'.

  CASE ss_0110.
    WHEN 9001.
      PERFORM save_6000_01.
    WHEN 9002.
      PERFORM save_6000_02.
    WHEN 9005.
      PERFORM save_6000_05.
  ENDCASE.
ENDMODULE.                 " save  INPUT
*&---------------------------------------------------------------------*
*&      Module  disp  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE disp INPUT.
  CHECK save_ok_code = 'DISP'.
  CASE ss_0110.
    WHEN 9001.
      IF w_werks IS INITIAL.
        MESSAGE i999(zmmm) WITH 'Fill the Plant!'.
        EXIT.
      ENDIF.
      PERFORM disp_6000_01.
    WHEN 9002.
      IF w_matnr IS INITIAL.
        MESSAGE i999(zmmm) WITH 'Fill the Material number!'.
        EXIT.
      ELSEIF w_werks IS INITIAL.
        MESSAGE i999(zmmm) WITH 'Fill the Plant!'.
        EXIT.
      ENDIF.
      PERFORM disp_6000_02.
    WHEN 9003.
      IF w_matnr IS INITIAL.
        MESSAGE i999(zmmm) WITH 'Fill the Material number!'.
        EXIT.
      ELSEIF w_werks IS INITIAL.
        MESSAGE i999(zmmm) WITH 'Fill the Plant!'.
        EXIT.
      ENDIF.
      PERFORM disp_6000_03.
    WHEN 9004.
      IF w_werks IS INITIAL.
        MESSAGE i999(zmmm) WITH 'Fill the Plant!'.
        EXIT.
      ELSEIF w_licode IS INITIAL.
        MESSAGE i999(zmmm) WITH 'Fill the Line Code!'.
        EXIT.
      ENDIF.
      PERFORM disp_6000_04.
    WHEN 9005.
      IF w_matnr IS INITIAL.
        MESSAGE i999(zmmm) WITH 'Fill the Material number!'.
        EXIT.
      ELSEIF w_werks IS INITIAL.
        MESSAGE i999(zmmm) WITH 'Fill the Plant!'.
        EXIT.
      ENDIF.
      PERFORM disp_6000_05.
    WHEN 9006.
      IF w_matnr IS INITIAL.
        MESSAGE i999(zmmm) WITH 'Fill the Material number!'.
        EXIT.
      ELSEIF w_werks IS INITIAL.
        MESSAGE i999(zmmm) WITH 'Fill the Plant!'.
        EXIT.
      ENDIF.
      PERFORM disp_6000_06.
  ENDCASE.
ENDMODULE.                 " disp  INPUT
*&---------------------------------------------------------------------*
*&      Module  dele  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE dele INPUT.
  CHECK save_ok_code = 'DELE'.
  CASE ss_0110.
    WHEN 9001.
** For deletion of items
      PERFORM make_it_exdata_zsmm_6000_01del.
      DELETE it_exdata_zsmm_6000_01 WHERE sel_flag = 'X'.

* When Delete, We use delete and save at one time by user need.
      PERFORM save_6000_01.
      IF sy-subrc = 0.
        MESSAGE s999(zmmm) WITH 'Deleted Successfully!'.
      ENDIF.
    WHEN 9002.
      DATA: ck_cnt LIKE sy-subrc.
** For deletion of items
      PERFORM make_it_exdata_zsmm_6000_02del
                         CHANGING ck_cnt.
      CHECK NOT it_exdata_zsmm_6000_02del IS INITIAL.

      CHECK ck_cnt IS INITIAL.

      DELETE it_exdata_zsmm_6000_02 WHERE sel_flag = 'X'.
* When Delete, We use delete and save at one time by user need.
      PERFORM save_6000_02.
      IF sy-subrc = 0.
        MESSAGE s999(zmmm) WITH 'Deleted Successfully!'.
      ENDIF.
  ENDCASE.
ENDMODULE.                 " dele  INPUT
*&      Module  lb_zstatus  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE lb_zstatus INPUT.
  CHECK save_ok_code = 'LB_ZSTATUS'.
* Adjust zstatus_before & zstatus
  LOOP AT it_exdata_zsmm_6000_02 ASSIGNING <fs_exdata_zsmm_6000_02>.
    IF NOT <fs_exdata_zsmm_6000_02>-zstatus_before IS INITIAL.
      IF <fs_exdata_zsmm_6000_02>-zstatus = 'A'.
        CLEAR <fs_exdata_zsmm_6000_02>-zstatus.
        "zstatus = 'A' is impossible!
      ENDIF.
    ELSE.
      <fs_exdata_zsmm_6000_02>-zstatus = 'A'.
    ENDIF.
  ENDLOOP.
ENDMODULE.                 " lb_zstatus  INPUT
*&---------------------------------------------------------------------*
*&      Module  inse  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE inse INPUT.
  CHECK save_ok_code = 'INSE'.
  CASE ss_0110.
    WHEN 9001.      "Operation Master
      CLEAR wa_exdata_zsmm_6000_01.
      wa_exdata_zsmm_6000_01-sel_flag = 'X'.

      wa_exdata_zsmm_6000_01-zsection = 'B'.    "Operation Code
      wa_exdata_zsmm_6000_01-use_flag = 'A'.    "Yes

      APPEND wa_exdata_zsmm_6000_01 TO it_exdata_zsmm_6000_01.

*Adjust Cursor Location
      w_lines_tmp = w_lines - w_visiblelines + 2.
      IF w_top_line LE w_lines_tmp.
        w_top_line = w_lines - w_visiblelines + 2.
      ENDIF.

    WHEN 9002.      "Operation History Management
      CLEAR wa_exdata_zsmm_6000_02.
      wa_exdata_zsmm_6000_02-sel_flag = 'X'.

      wa_exdata_zsmm_6000_02-mandt = sy-mandt.
      wa_exdata_zsmm_6000_02-bukrs = w_bukrs.  "Company code 'H201'
      wa_exdata_zsmm_6000_02-werks = w_werks.  "Plant
      wa_exdata_zsmm_6000_02-matnr = w_matnr.  "Material

      APPEND wa_exdata_zsmm_6000_02 TO it_exdata_zsmm_6000_02.

*Adjust Cursor Location
      w_lines_tmp = w_lines - w_visiblelines + 2.
      IF w_top_line LE w_lines_tmp.
        w_top_line = w_lines - w_visiblelines + 2.
      ENDIF.

  ENDCASE.
ENDMODULE.                 " inse  INPUT
