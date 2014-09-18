*----------------------------------------------------------------------*
*   INCLUDE MZEMMGM01E_6007O01                                         *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  initial_data  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE initial_data OUTPUT.
  CASE sy-dynnr.
    WHEN 0100.
      GET PARAMETER ID 'ZPI_SUBPART' FIELD ztmm_6007_01-subpart.
    WHEN 0200.
      PERFORM get_unit_of_matnr USING    ztmm_6007_01-rawmat
                                CHANGING ztmm_6007_01-meins.
    WHEN 9100.
      GET PARAMETER ID 'ZPI_ENDPART' FIELD ztmm_6007_02-endpart.
    WHEN 9200.
  ENDCASE.
ENDMODULE.                 " initial_data  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  status  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status OUTPUT.
  PERFORM make_it_func.

* Instanciate PF-STATUS & TITLEBAR.
  CASE sy-dynnr.
    WHEN 0100.
      CASE sy-tcode.
        WHEN 'ZMME81'.   "Create (Sub_Raw)
     title = 'Create BOM Registration Request(Sub_Raw): Initial Screen'.
        WHEN 'ZMME82'.   "Change (Sub_Raw)
     title = 'Change BOM Registration Request(Sub_Raw): Initial Screen'.
        WHEN 'ZMME83'.   "Display (Sub_Raw)
    title = 'Display BOM Registration Request(Sub_Raw): Initial Screen'.
      ENDCASE.
    WHEN 0200.
      CASE sy-tcode.
        WHEN 'ZMME81'.   "Create (Sub_Raw)
          title = 'Create BOM Registration Request(Sub_Raw)'.
        WHEN 'ZMME82'.   "Change (Sub_Raw)
          title = 'Change BOM Registration Request(Sub_Raw)'.
        WHEN 'ZMME83'.   "Display (Sub_Raw)
          title = 'Display BOM Registration Request(Sub_Raw)'.
      ENDCASE.
    WHEN 9100.
      CASE sy-tcode.
    title = 'Display BOM Registration Request(Sub_Raw): Initial Screen'.
        WHEN 'ZMME84'.   "Create (End_Sub)
     title = 'Create BOM Registration Request(End_Sub): Initial Screen'.
        WHEN 'ZMME85'.   "Change (End_Sub)
     title = 'Change BOM Registration Request(End_Sub): Initial Screen'.
        WHEN 'ZMME86'.   "Display (End_Sub)
    title = 'Display BOM Registration Request(End_Sub): Initial Screen'.
      ENDCASE.
    WHEN 9200.
      CASE sy-tcode.
        WHEN 'ZMME84'.   "Create (End_Sub)
          title = 'Create BOM Registration Request(End_Sub)'.
        WHEN 'ZMME85'.   "Change (End_Sub)
          title = 'Change BOM Registration Request(End_Sub)'.
        WHEN 'ZMME86'.   "Display (End_Sub)
          title = 'Display BOM Registration Request(End_Sub)'.
      ENDCASE.
  ENDCASE.

  CREATE OBJECT crv_ps
    EXPORTING im_ps      = 'PS'             "PF-STATUS
              im_it_func = it_func          "Excluding func
              im_tb      = 'TB'             "TITLEBAR
              im_title   = title.           "TITLE
  CLEAR it_func.

ENDMODULE.                 " status  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  desc  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE desc OUTPUT.

ENDMODULE.                 " desc  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  modify_screen  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_screen OUTPUT.
  CASE sy-tcode.
    WHEN 'ZMME81'.    "Create
    WHEN 'ZMME82'.    "Change
    WHEN 'ZMME83'.    "Display
      LOOP AT SCREEN.
        IF screen-group1 = 'TRT'.
          screen-input = 0.
        ENDIF.
        MODIFY SCREEN.
      ENDLOOP.
    WHEN 'ZMME84'.    "Create
    WHEN 'ZMME85'.    "Change
    WHEN 'ZMME86'.    "Display
      LOOP AT SCREEN.
        IF screen-group1 = 'TRT'.
          screen-input = 0.
        ENDIF.
        MODIFY SCREEN.
      ENDLOOP.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " modify_screen  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  describe_table  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE describe_table OUTPUT.
  DESCRIBE TABLE it_zsmm_6007_02 LINES tc_9200-lines.
  " If you omit above line, vertical scroll would be not available.
  " Number of lines to display.
ENDMODULE.                 " describe_table  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  get_visiblelines  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_visiblelines OUTPUT.
  visiblelines = sy-loopc.
  "Screens, number of lines visible in table
ENDMODULE.                 " get_visiblelines  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  wa_to_sf  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE wa_to_sf OUTPUT.
  PERFORM get_unit_of_matnr USING    wa_zsmm_6007_02-subpart
                            CHANGING wa_zsmm_6007_02-meins.

  MOVE-CORRESPONDING wa_zsmm_6007_02 TO zsmm_6007_02.
ENDMODULE.                 " wa_to_sf  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  make_tc  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE make_tc OUTPUT.
  PERFORM make_tc_9200 TABLES it_zsmm_6007_02
                       USING  15       "visiblelines
                              tc_9200.
ENDMODULE.                 " make_tc  OUTPUT
