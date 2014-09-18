*&--------------------------------------------------------------------&*
* Program Name      : ZAFIU139
* Author            : Valerian Utama
* Creation Date     : 03/15/2010
* Specifications By : Calvin Kong
* Pattern           :
* Addl Documentation:
* Description       : Upload Reclaim Data into table ZTFI_RECLAIM_DAT
*
*&--------------------------------------------------------------------&*
* Modification Logs
* Date       Developer    Description
* 03/15/2010 VALERIAN     Initial Program Development
*            HIS20094
*
*&--------------------------------------------------------------------&*

REPORT zafiu139 MESSAGE-ID zmfi.

*ALV Data
TYPE-POOLS: slis.
INCLUDE <icon>.
DATA : gt_fieldcat TYPE slis_t_fieldcat_alv.

* Program Data
CONSTANTS: c_row_max TYPE i VALUE '65536',
           c_row_inc TYPE i VALUE '10000'.

TABLES: ztfi_reclaim_dat.

FIELD-SYMBOLS: <fs> TYPE ANY.

DATA: BEGIN OF t_file OCCURS 0,
        corp  TYPE ztfi_reclaim_dat-corp,
        doex  TYPE ztfi_reclaim_dat-doex,
        vndr  TYPE ztfi_reclaim_dat-vndr,
        issu  TYPE ztfi_reclaim_dat-issu,
        serl  TYPE ztfi_reclaim_dat-serl,
        rono  TYPE ztfi_reclaim_dat-rono,
        opcd  TYPE ztfi_reclaim_dat-opcd,
        vdgb  TYPE ztfi_reclaim_dat-vdgb,
        carc  TYPE ztfi_reclaim_dat-carc,
        waers TYPE t001-waers,
        bjpt  TYPE ztfi_reclaim_dat-bjpt,
        bjla  TYPE ztfi_reclaim_dat-bjla,
        bjmi  TYPE ztfi_reclaim_dat-bjmi,
        fldt  TYPE ztfi_reclaim_dat-fldt,
        aedat TYPE ztfi_reclaim_dat-aedat,
      END OF t_file.

DATA: BEGIN OF t_data OCCURS 0,
      icon(4) TYPE c.
        INCLUDE STRUCTURE ztfi_reclaim_dat.
DATA  END OF t_data.

DATA: t_excel     TYPE alsmex_tabline OCCURS 0 WITH HEADER LINE,
      i_begin_col TYPE i VALUE 1,
      i_begin_row TYPE i VALUE 2,
      i_end_col   TYPE i VALUE 15,
      i_end_row   TYPE i VALUE 2.

DATA: v_file_table TYPE filetable,
      v_rc TYPE i.

SELECTION-SCREEN BEGIN OF BLOCK sel WITH FRAME TITLE text-001.
PARAMETERS: p_file TYPE rlgrap-filename OBLIGATORY,
            p_upld TYPE c AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK sel.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = 'test'
      initial_directory       = 'C:\'
    CHANGING
      file_table              = v_file_table
      rc                      = v_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      OTHERS                  = 4.

  READ TABLE v_file_table INDEX 1 INTO p_file.

START-OF-SELECTION.
* Read Data from excel file
  PERFORM get_data.

* Process/Update Data
  PERFORM process_data.

* Display data using ALV
  PERFORM display_data.


*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCAT[]  text
*----------------------------------------------------------------------*
FORM fieldcat_init USING ft_fieldcat TYPE slis_t_fieldcat_alv .

  DATA: l_pos TYPE i,
        gs_fieldcat TYPE slis_fieldcat_alv.

  DEFINE __catalog.
    l_pos = l_pos + 1.
    clear gs_fieldcat.
    gs_fieldcat-col_pos       = l_pos.
    gs_fieldcat-key           = &1.
    gs_fieldcat-fieldname     = &2.
    gs_fieldcat-seltext_s     = &3.        " Column heading
    gs_fieldcat-seltext_m     = &3.        " Column heading
    gs_fieldcat-outputlen     = &4.        " Column width
    gs_fieldcat-datatype      = &5.        " Data type
    gs_fieldcat-emphasize     = &6.
    gs_fieldcat-cfieldname    = &7.
    gs_fieldcat-lowercase     = 'X'.
    append gs_fieldcat to  ft_fieldcat.
  END-OF-DEFINITION.

  __catalog :
    ' '  'ICON'      'Status'            4   'CHAR' '' '',
    'X'  'CORP'      'Company Code'      2   'CHAR' '' '',
    'X'  'DOEX'      'Country code'      1   'CHAR' '' '',
    'X'  'VNDR'      'Vendor Code'       12  'CHAR' '' '',
    'X'  'ISSU'      'ISSU'              10  'CHAR' '' '',
    'X'  'SERL'      'SERL'              7   'NUMC' '' '',
    'X'  'RONO'      'RONO'              19  'CHAR' '' '',
    ' '  'OPCD'      'OP Code'           8   'CHAR' '' '',
    ' '  'VDGB'      'VBGB'              1   'CHAR' '' '',
    ' '  'CARC'      'CARC'              3   'CHAR' '' '',
    ' '  'NATN'      'Nation'            5   'CHAR' '' '',
    ' '  'BJPT'      'BJPT Amnt'         13  'CURR' '' '',
    ' '  'BJLA'      'BJLA Amnt'         13  'CURR' '' '',
    ' '  'BJMI'      'BJMI Amnt'         13  'CURR' '' '',
    ' '  'OJPT'      'OJPT Amnt'         13  'CURR' '' '',
    ' '  'OJLA'      'OJLA Amnt'         13  'CURR' '' '',
    ' '  'OJMI'      'OJMI Amnt'         13  'CURR' '' '',

    ' '  'ISDT'      'Date'    12  'CHAR' '' '',
    ' '  'FLDT'      'DATA CREATION DATE'    8  'DATS' '' '',
    ' '  'ERZET'      'Entry time'  6  'TIMS' '' '',
    ' '  'AENAM'      'Changed By'       12  'CHAR' '' '',
    ' '  'AEDAT'      'Changed Date'     8   'DATS' '' '',
    ' '  'AEZET'      'Change Time'      6   'TIMS' '' '',
    ' '  'FISCAL_YR'  'Fiscal year'      4   'NUMC' '' '',
    ' '  'POSTING_DT' 'Posting date'     8   'DATS' '' '',
    ' '  'TYPE'       'Message type'     1   'CHAR' '' '',
    ' '  'MESSAGE'    'Description1'     80  'CHAR' '' '',
    ' '  'MSGRVS'     'Description2'     80  'CHAR' '' ''.

ENDFORM.                    " fieldcat_init

*&---------------------------------------------------------------------*
*&      Form  ALV_GRID_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_grid_display TABLES ft_outtab.

  DATA : gs_layout TYPE slis_layout_alv,
         l_repid TYPE sy-repid.

  l_repid = sy-repid.

  gs_layout-colwidth_optimize = 'X'.
  gs_layout-zebra = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program = l_repid
            is_layout          = gs_layout
            it_fieldcat        = gt_fieldcat
       TABLES
            t_outtab           = ft_outtab
       EXCEPTIONS
            program_error      = 1
            OTHERS             = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " ALV_GRID_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data .
  PERFORM fieldcat_init     USING gt_fieldcat[].
  PERFORM alv_grid_display  TABLES t_data.
ENDFORM.                    " DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       Get data from excel file
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  DATA: l_row_inc TYPE i VALUE '1',
        l_tot_rec TYPE i.

  WHILE i_begin_row <= c_row_max.

* Optimize the reading process
    l_row_inc = l_row_inc * 10.
    IF l_row_inc GT 10000.
      l_row_inc = 10000.
    ENDIF.

    i_end_row = i_begin_row + l_row_inc - 1.

    IF i_end_row GT c_row_max.
      i_end_row = c_row_max.
    ENDIF.

    CLEAR: t_excel, l_tot_rec.
    REFRESH t_excel.

    CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
         EXPORTING
              filename                = p_file
              i_begin_col             = i_begin_col
              i_begin_row             = i_begin_row
              i_end_col               = i_end_col
              i_end_row               = i_end_row
         TABLES
              intern                  = t_excel
         EXCEPTIONS
              inconsistent_parameters = 1
              upload_ole              = 2
              OTHERS                  = 3.

    IF sy-subrc <> 0.
      MESSAGE s000 WITH text-m04 '' '' ''.
      LEAVE PROGRAM.
    ENDIF.

    SORT t_excel BY row col.
    LOOP AT t_excel.
      ASSIGN COMPONENT t_excel-col OF STRUCTURE t_file TO <fs>.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      IF t_excel-col EQ 11 OR
         t_excel-col EQ 12 OR
         t_excel-col EQ 13.

        TRANSLATE t_excel-value USING ', '.
        CONDENSE t_excel-value NO-GAPS.

      ELSEIF t_excel-col EQ 14 OR
         t_excel-col EQ 15.

        CALL FUNCTION 'CONVERT_DATE_TO_INTERN_FORMAT'
             EXPORTING
                  datum = t_excel-value
                  dtype = 'DATS'
             IMPORTING
                  idate = t_excel-value.
      ENDIF.

      <fs> = t_excel-value.

      AT END OF row.
        APPEND t_file.
        l_tot_rec = l_tot_rec + 1.
      ENDAT.
    ENDLOOP.

    IF l_tot_rec LT l_row_inc.
      EXIT.
    ENDIF.

    i_begin_row = i_begin_row + l_row_inc.
  ENDWHILE.

  FREE t_excel.
ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  process_data
*&---------------------------------------------------------------------*
*       Process/Update data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_data.
  DATA: l_tabix TYPE sy-tabix,
        l_rec   TYPE i.

  LOOP AT t_file.
    l_tabix = sy-tabix.
    CLEAR t_data.

    MOVE-CORRESPONDING t_file TO t_data.

    SELECT SINGLE *
      FROM ztfi_reclaim_dat
      WHERE corp = t_data-corp
        AND doex = t_data-doex
        AND vndr = t_data-vndr
        AND issu = t_data-issu
        AND serl = t_data-serl
        AND rono = t_data-rono.

    IF sy-subrc = 0.
      t_data-icon = icon_red_light.
*      MOVE-CORRESPONDING t_data TO ztfi_reclaim_dat.  "VALERIAN
*      DELETE ztfi_reclaim_dat.                        "VALERIAN

    ELSE.
      t_data-icon = icon_green_light.

      IF NOT p_upld IS INITIAL.
        MOVE-CORRESPONDING t_data TO ztfi_reclaim_dat.
        INSERT ztfi_reclaim_dat.
        IF sy-subrc = 0.
          COMMIT WORK.
          l_rec = l_rec + 1.

        ELSE.
          ROLLBACK WORK.
          t_data-icon = icon_red_light.
          MODIFY t_data INDEX l_tabix.
        ENDIF.
      ENDIF.
    ENDIF.

    APPEND t_data.
  ENDLOOP.

  FREE t_file.

  IF NOT p_upld IS INITIAL.
    MESSAGE s000 WITH text-m01 l_rec text-m02 ''.
  ELSE.
    DESCRIBE TABLE t_data LINES l_rec.
    MESSAGE s000 WITH text-m01 l_rec text-m03 p_file.
  ENDIF.

ENDFORM.                    " process_data
