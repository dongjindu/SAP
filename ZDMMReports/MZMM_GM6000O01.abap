*--------------------------------------------------------
*   INCLUDE MZMM_GM6000O01
*
*--------------------------------------------------------
*&-------------------------------------------------------
*&      Module  initial_data  OUTPUT
*&-------------------------------------------------------
*       text
*--------------------------------------------------------
MODULE initial_data OUTPUT.
  CASE sy-dynnr.
    WHEN 0110.
      IF pgm IS INITIAL.
        pgm = 'SAPLZGMM_6000_SS'.
      ENDIF.
      IF ss_0110 IS INITIAL.
        ss_0110 = '9000'.
      ENDIF.
  ENDCASE.
ENDMODULE.                 " initial_data  OUTPUT
*&-------------------------------------------------------
*&      Module  status  OUTPUT
*&-------------------------------------------------------
*       text
*--------------------------------------------------------
MODULE status OUTPUT.
  PERFORM make_it_func.

* Instanciate PF-STATUS & TITLEBAR.
  IF w_title IS INITIAL.
    w_title = 'Tool Information Management'.
  ENDIF.

  CREATE OBJECT crv_ps
    EXPORTING im_ps      = 'PS'     "PF-STATUS
              im_it_func = it_func  "Excluding func
              im_tb      = 'TB'     "TITLEBAR
              im_title   = w_title.   "TITLE
  CLEAR it_func.

* Dynamic Function Code Text
  IF dynftext IS INITIAL.
    dynftext  = c_tree_off.
  ENDIF.
ENDMODULE.                 " status  OUTPUT
*&-------------------------------------------------------
*&      Module  pbo_100  OUTPUT
*&-------------------------------------------------------
*       text
*--------------------------------------------------------
MODULE pbo_100 OUTPUT.
**** Create the application object
* This object is needed to handle the ABAP Objects Events
* of Controls
  IF crv_h_tree IS INITIAL.
    CREATE OBJECT crv_h_tree.
  ELSE.
  ENDIF.

* Docking Container
  IF crv_docking_container IS INITIAL.
    CREATE OBJECT crv_docking_container
      EXPORTING repid     = pgm
                dynnr     = scr
                side      = crv_docking_container->dock_at_left
                extension = 250. "width of crv_docking_container
  ELSE.
  ENDIF.

* Create the tree model
  IF crv_tree_model IS INITIAL.
    " The Tree Model has not been created yet.
    " Create a Tree Model and insert nodes into it.
    PERFORM create_and_init_tree.
  ELSE.
  ENDIF.
ENDMODULE.                 " pbo_100  OUTPUT
*&-------------------------------------------------------
*&      Module  set_data_to_sa_0110  OUTPUT
*&-------------------------------------------------------
*       text
*--------------------------------------------------------
MODULE set_data_to_sa_0110 OUTPUT.

* Export data to function group (for display on subscreen)
  CASE ss_0110.

    WHEN 9001.
      CALL FUNCTION 'Z_FMM_6000_01_SET_DATA'
           EXPORTING
                im_ok_code       = save_ok_code
                im_bukrs         = w_bukrs
                im_licode        = w_licode
                im_opcode        = w_opcode
                im_cblicode      = cblicode
                im_werks         = w_werks
                im_fl_once       = w_once_flg
                im_visiblelines  = w_visiblelines
                im_lines         = w_lines
                im_top_line      = w_top_line
           TABLES
                imt_zsmm_6000_01 = it_exdata_zsmm_6000_01.
    WHEN 9002.

      CALL FUNCTION 'Z_FMM_6000_02_SET_DATA'
           EXPORTING
                im_ok_code         = save_ok_code
                im_bukrs           = w_bukrs
                im_matnr           = w_matnr
                im_werks           = w_werks
                im_ztconodisp      = w_ztconodisp
                im_ztcono          = w_ztcono
                im_zapplied_date   = w_zapplied_date
                im_erdat           = w_erdat
                im_zch_desc_cd     = w_zch_desc_cd
                im_zch_reason_cd   = w_zch_reason_cd
                im_zinv_process_cd = w_zinv_process_cd
                im_zch_matnr1      = w_zch_matnr1
                im_zch_matnr2      = w_zch_matnr2
                im_zxfer_matnr     = w_zxfer_matnr
                im_fl_once         = w_once_flg
                im_visiblelines    = w_visiblelines
                im_lines           = w_lines
                im_top_line        = w_top_line
                im_trtyp           = w_trtyp
           TABLES
                imt_zsmm_6000_02   = it_exdata_zsmm_6000_02.
    WHEN 9003.
      CALL FUNCTION 'Z_FMM_6000_03_SET_DATA'
           EXPORTING
                im_matnr         = w_matnr
                im_werks         = w_werks
                im_ztcono        = w_ztcono
                im_fl_once       = w_once_flg
           TABLES
                imt_zsmm_6000_03 = it_exdata_zsmm_6000_03.
    WHEN 9004.
      CALL FUNCTION 'Z_FMM_6000_04_SET_DATA'
           EXPORTING
                im_matnr         = w_matnr
                im_werks         = w_werks
                im_licode        = w_licode
                im_opcode        = w_opcode
                im_fl_once       = w_once_flg
           TABLES
                imt_zsmm_6000_04 = it_exdata_zsmm_6000_04.

    WHEN 9005.
      CALL FUNCTION 'Z_FMM_6000_05_SET_DATA'
           EXPORTING
                im_ok_code       = save_ok_code
                im_matnr         = w_matnr
                im_werks         = w_werks
                im_licode        = w_licode
                im_opcode        = w_opcode
                im_fl_once       = w_once_flg
           TABLES
                imt_zsmm_6000_05 = it_exdata_zsmm_6000_05.
    WHEN 9006.
      CALL FUNCTION 'Z_FMM_6000_06_SET_DATA'
           EXPORTING
                im_matnr         = w_matnr
                im_werks         = w_werks
                im_licode        = w_licode
                im_opcode        = w_opcode
                im_fl_once       = w_once_flg
           TABLES
                imt_zsmm_6000_06 = it_exdata_zsmm_6000_06.

  ENDCASE.
ENDMODULE.                 " set_data_to_sa_0110  OUTPUT
