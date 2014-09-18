*----------------------------------------------------------------------*
*   INCLUDE ZACOU102_ALV                                               *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
*       Exclude function code
*----------------------------------------------------------------------*
form exclude_functions using p_tabname.
  perform append_exclude_functions
          tables gt_exclude[]
           using: cl_gui_alv_grid=>mc_fc_loc_undo,
                  cl_gui_alv_grid=>mc_fc_average,
                  cl_gui_alv_grid=>mc_fc_graph,
                  cl_gui_alv_grid=>mc_fc_info,
                  cl_gui_alv_grid=>mc_fc_refresh.

endform.                    " EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
*&      Form  CREATE_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*       Create ALV control: Field catalog
*----------------------------------------------------------------------*
form create_field_category.
  perform fill_field_category using:
          1  'LIFNR'     'Vendor'       '10'  'CHAR',
          2  'NAME1'     'Description'  '50'  'CHAR',
          3  'QTA'       'Quota'        '5'   'CHAR',
          4  'PWERTN'    'Prv.Price'    '15'  'CURR',
          5  'WERTN'     'Price'        '15'  'CURR',
          6  'KZUST1_IN' 'RS1'          '3'   'CHAR',
          7  'WERTN1'    'RS1 $'        '15'  'CURR',
          8  'KZUST2_IN' 'RS2'          '3'   'CHAR',
          9  'WERTN2'    'RS2 $'        '15'  'CURR'.

  loop at gt_fcat into gs_fcat.
    if gs_fcat-datatype = 'CURR'.
      gs_fcat-currency = 'USD'.
      gs_fcat-ref_field = 'WERTN1'.
      gs_fcat-ref_table = 'ZTCOU102'.

      modify gt_fcat from gs_fcat
             transporting currency ref_field ref_table
             where datatype = 'CURR'.

    else.
      case gs_fcat-fieldname.
        when 'LIFNR'.
          gs_fcat-ref_field = 'LIFNR'.
          gs_fcat-ref_table = 'LFA1'.
          modify gt_fcat from gs_fcat
                 transporting ref_field ref_table
                 where fieldname = 'LIFNR'.

        when 'QTA'.
          gs_fcat-just = 'R'.
          modify gt_fcat from gs_fcat transporting just
             where fieldname = 'QTA'.

        when 'KZUST1_IN' or 'KZUST2_IN'.
          gs_fcat-f4availabl = 'X'.
          modify gt_fcat from gs_fcat transporting f4availabl
             where fieldname = gs_fcat-fieldname.

      endcase.

    endif.

  endloop.

endform.                    " CREATE_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*&      Form  SET_LVC_LAYOUT
*&---------------------------------------------------------------------*
*       Setting for layout
*----------------------------------------------------------------------*
form set_lvc_layout.
  gs_layo-edit       = 'X'.
  gs_layo-cwidth_opt = 'X'.
  gs_layo-sel_mode   = 'A'.       " Column and row selection
  gs_layo-stylefname = 'CELLTAB'.

endform.                    " SET_LVC_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED
*&---------------------------------------------------------------------*
*       Event of changed data
*----------------------------------------------------------------------*
*      -->RR_DATA_CHANGED  Log is Visible
*----------------------------------------------------------------------*
form data_changed using rr_data_changed
                        type ref to cl_alv_changed_data_protocol.
  data: ls_mod_cells type lvc_s_modi,
        ls_cells     type lvc_s_modi,
        lt_values    type table of bapi_char_values with header line.
  data run_update.
  data l_ix type i.
  loop at rr_data_changed->mt_good_cells into ls_mod_cells.

    if ls_mod_cells-fieldname eq 'WERTN'.
      gt_out-price_manual = 'X'.
      run_update = 'X'.
      l_ix = ls_mod_cells-row_id.

      gt_out-wertn = ls_mod_cells-value.
      modify gt_out index ls_mod_cells-row_id transporting
                                              price_manual wertn.

    endif.
    read table gt_dtl index ls_mod_cells-row_id.
    if sy-subrc <> 0.
      perform change_vendor_info using rr_data_changed
                                       ls_mod_cells.
      exit.
    else.
      case ls_mod_cells-fieldname.
        when 'LIFNR'.               " Vendor
          perform change_lifnr using rr_data_changed
                                     ls_mod_cells.

        when 'QTA'.                 " Quata
          call method rr_data_changed->modify_cell
                 exporting i_row_id    = ls_mod_cells-row_id
                           i_fieldname = ls_mod_cells-fieldname
                           i_value     = ls_mod_cells-value.

          gt_dtl-qta = ls_mod_cells-value.

        when 'WERTN'.                " Price
          call method rr_data_changed->modify_cell
                 exporting i_row_id    = ls_mod_cells-row_id
                           i_fieldname = ls_mod_cells-fieldname
                           i_value     = ls_mod_cells-value.

          gt_dtl-wertn = ls_mod_cells-value.

*         RS1 price : Info.price - Prv.Info.price
          data lv_wertn1 type ck_kwt.

          clear lv_wertn1.
          lv_wertn1 = ls_mod_cells-value - gt_dtl-pwertn.

          call method rr_data_changed->modify_cell
                 exporting i_row_id    = ls_mod_cells-row_id
                           i_fieldname = 'WERTN1'
                           i_value     = lv_wertn1.

          gt_dtl-wertn1 = lv_wertn1.

        when 'KZUST1_IN'.                                   " RS1
          call method rr_data_changed->modify_cell
                 exporting i_row_id    = ls_mod_cells-row_id
                           i_fieldname = ls_mod_cells-fieldname
                           i_value     = ls_mod_cells-value.

          perform change_kzust using rr_data_changed
                                     ls_mod_cells
                                     'GT_DTL'.

        when 'WERTN1'.              " RS2: Price
          if not ls_mod_cells-value is initial.
            call method rr_data_changed->modify_cell
                   exporting i_row_id    = ls_mod_cells-row_id
                             i_fieldname = ls_mod_cells-fieldname
                             i_value     = ls_mod_cells-value.

            gt_dtl-wertn1 = ls_mod_cells-value.
          endif.

        when 'KZUST2_IN'.                                   " RS2
          call method rr_data_changed->modify_cell
                 exporting i_row_id    = ls_mod_cells-row_id
                           i_fieldname = ls_mod_cells-fieldname
                           i_value     = ls_mod_cells-value.

          perform change_kzust using rr_data_changed
                                     ls_mod_cells
                                     'GT_DTL'.

        when 'WERTN2'.              " RS2: Price
          perform change_wertn2 using rr_data_changed
                                      ls_mod_cells.

      endcase.
      gt_dtl-price_manual = 'X'.
      modify gt_dtl index ls_mod_cells-row_id.

    endif.

  endloop.

  if run_update = 'X'.
    perform get_abp_ldc_gt_out.
    modify gt_out index l_ix transporting duty frg oth.
    perform refresh_field1.
  endif.

endform.                    " DATA_CHANGED
*&---------------------------------------------------------------------*
*&      Form  BUILD_CELL_ATTR
*&---------------------------------------------------------------------*
*       Create attributes of cell
*----------------------------------------------------------------------*
form build_cell_attr.
  data: lt_celltab type lvc_t_styl,
        ls_celltab type lvc_s_styl,
        l_cnt type i.

  clear l_cnt.
  describe table gt_dtl lines l_cnt.

  clear lt_celltab.
  refresh lt_celltab.

  clear gs_fcat.
  if gv_lock = 'X' or
     gv_level <> 'COADM'.
    if gv_lock = 'X'.
      message s000 with gv_user 'using this program.'.
    else.
      message s000 with 'You have no authorization.'.
    endif.

    loop at gt_fcat into gs_fcat.
      ls_celltab-fieldname = gs_fcat-fieldname.
      ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.

      insert ls_celltab into table lt_celltab.
    endloop.

  else.
    loop at gt_fcat into gs_fcat.
      ls_celltab-fieldname = gs_fcat-fieldname.

      if ls_celltab-fieldname = 'QTA' or
         ls_celltab-fieldname = 'WERTN'  or
         ls_celltab-fieldname = 'LIFNR' or
         ls_celltab-fieldname = 'KZUST1_IN' or
         ls_celltab-fieldname = 'KZUST2_IN' or
         ls_celltab-fieldname = 'WERTN1' or
         ls_celltab-fieldname = 'WERTN2' or
         ls_celltab-fieldname = 'EKGRP'.
        ls_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
      else.
        ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
      endif.

      insert ls_celltab into table lt_celltab.
    endloop.

  endif.

  loop at gt_dtl.
    clear: gt_dtl-celltab, gt_dtl-celltab[].
    modify gt_dtl.
  endloop.

  insert lines of lt_celltab into table gt_dtl-celltab.
  modify gt_dtl transporting celltab where celltab is initial.

endform.                    " BUILD_CELL_ATTR
*&---------------------------------------------------------------------*
*&      Form  CREATE_ALV_CONTROL
*&---------------------------------------------------------------------*
*       Display [Change InforMation]
*----------------------------------------------------------------------*
form create_alv_control.
  if g_custom_container is initial.
    clear: gs_layo, gt_exclude, gs_variant, gt_fcat[].

*   Create object
    perform create_object.

*   Exclude toolbar
    perform exclude_functions using 'GT_EXCLUDE'.

*   Create field category
    perform create_field_category.

*   Setting for layout
    perform set_lvc_layout.

*   Define possible entry fields
    perform create_f4_fields using 'G_GRID'.

*   Define editable field
    call method g_grid->set_ready_for_input
      exporting
        i_ready_for_input = 1.

    call method g_grid->register_edit_event
         exporting i_event_id = cl_gui_alv_grid=>mc_evt_modified.

*   Setting for event
    create object g_event_receiver.
    set handler g_event_receiver->handle_data_changed  for g_grid.
    set handler g_event_receiver->on_f4                for g_grid.

*   Define cell attribute
    perform build_cell_attr.

*   Define variant
    gs_variant-report = sy-repid.

*   Display alv grid
    call method g_grid->set_table_for_first_display
         exporting is_layout            = gs_layo
                   it_toolbar_excluding = gt_exclude
                   i_save               = gc_var_save
*                   IS_VARIANT           = GS_VARIANT
         changing  it_outtab            = gt_dtl[]
                   it_fieldcatalog      = gt_fcat[].

  endif.

endform.                    " CREATE_ALV_CONTROL
*&---------------------------------------------------------------------*
*&      Form  CREATE_ALV_CONTROL1
*&---------------------------------------------------------------------*
form create_alv_control1.
  if g_custom_container1 is initial.
    clear: gs_layo1, gt_exclude, gs_variant1, gt_fcat1[].

*   Create object
    perform create_object1.

*   Exclude toolbar
    perform exclude_functions using 'GT_EXCLUDE'.

*   Create field category
    perform create_field_category1.

*   Setting for layout
    gs_layo1-edit       = 'X'.
    gs_layo1-cwidth_opt = 'X'.
    gs_layo1-sel_mode   = 'A'.       " Column and row selection
    gs_layo1-zebra      = 'X'.
    gs_layo1-stylefname = 'CELLTAB'.

*   Define possible entry fields
    perform create_f4_fields using 'G_GRID1'.

*   Define editable field
    call method g_grid1->set_ready_for_input
      exporting
        i_ready_for_input = 1.

    call method g_grid1->register_edit_event
         exporting i_event_id = cl_gui_alv_grid=>mc_evt_modified.

*   Setting for event
    create object g_event_receiver1.
    set handler g_event_receiver1->handle_data_changed  for g_grid1.
    set handler g_event_receiver1->handle_double_click  for g_grid1.

    set handler g_event_receiver1->on_f4 for g_grid1.

*   Define cell attribute
    perform build_cell_attr1.

*   Define variant
    gs_variant1-report = sy-repid.

*   Display alv grid
    call method g_grid1->set_table_for_first_display
         exporting is_layout            = gs_layo1
                   it_toolbar_excluding = gt_exclude
                   i_save               = gc_var_save
                   is_variant           = gs_variant1
         changing  it_outtab            = gt_out[]
                   it_fieldcatalog      = gt_fcat1[].
  endif.

endform.                    " CREATE_ALV_CONTROL1
*&---------------------------------------------------------------------*
*&      Form  CREATE_OBJECT1
*&---------------------------------------------------------------------*
form create_object1.
* create a custom container control for our alv control
  create object g_custom_container1
      exporting
          container_name = container1.

* Create an Instance of ALV Control
  create object g_grid1
        exporting i_parent = g_custom_container1.

endform.                    " CREATE_OBJECT1
*&---------------------------------------------------------------------*
*&      Form  CREATE_FIELD_CATEGORY1
*&---------------------------------------------------------------------*
form create_field_category1.
  perform fill_field_category1 using:

    01  'ICNLCK'    'St.'               4   'ICON',
    02  'MATKL'     'Mat.Group'         9   'CHAR',
    03  'MATNR'     'End Item'          18  'CHAR',
    04  'MAKTG'     'Description'       50  'CHAR',
    05  'BKLAS'     'Val.Class'         4   'CHAR',
    06  'WERTN'     'Net Price'         11  'CURR',
    07  'PEINH'     'Per'               2   'DEC',
    08  'PMEHT'     'UoM'               4   'UNIT',
    09  'PROFL'     'Src'               1   'CHAR',
    10  'LIFNR'     '1st Vendor'        10  'CHAR',
    11  'KZUST1_IN' 'RSN'               3   'CHAR',
    12  'QTA'       'Quota'             3   'DEC',
    13  'DIFF'      'Info Dif'          8   'CURR',
    14  'PWERTN'    'Prv.info'          11  'CURR',
    15  'DUTY'      'Duty$'             8   'CURR',
    16  'FRG'       'Freight$'          8   'CURR',
    17  'OTH'       'Other$'            8   'CURR',
    18  'GPRC'      'Grs.Prc$'          8   'CURR',
    19  'ZTEXT'     'Reason desc.'      15  'CHAR',
    20  'EKGRP'     'Pur.grp'           3   'CHAR',
    21  'LIFNR2'    '2nd Vendor'        10  'CHAR',
    22  'ICON'      'Trf.Plan$'         6   'ICON',
    23  'EC_P'      'E:P'               3   'CHAR',
    24  'EC_A'      'E:A'               3   'CHAR',
    25  'EC_S'      'E:S'               3   'CHAR',
    26  'EC_D'      'E:D'               3   'CHAR',
    27  'EC_V'      'E:V'               3   'CHAR',
    28  'EC_G'      'E:G'               3   'CHAR',
    29  'EC_R'      'E:R'               3   'CHAR',
    30  'STAT'      'Status'            1   'CHAR',
    31  'ESTAT'     'Exception'         4   'ICON',
    32  'VERPR'     'MAP'               11  'CURR',
    33  'STPRS'     'STD'               11  'CURR',
    34  'AMT_DIF'   'Diff$'             15  'CURR',
    35  'RATE_DIF'  'Diff%'             15  'DEC',
    36  'STLAN'     'BoM'               1   'CHAR',
    37  'WERKS'     'SrcPt'             4   'CHAR',
    38  'MTART'     'MType'             4   'CHAR',
    39  'STAWN'     'Commodity Code'    17  'CHAR',
    40  'INFNR'     'Info.record No.'   10  'CHAR',
    41  'AENAM'     'Chgd by'           10  'CHAR',
    42  'AEDAT'     'Changed on'        10  'DATS',
    43  'MESS'      'Error Text'        80  'CHAR',
    44  'STRAT'     'S'                  1  'CHAR',
    45  'SUBSTRAT'  'S'                  1  'CHAR',
    46  'ELEMT'     'ELT'                3  'NUMC',
    47  'ELEMTNS'   'ELS'                3  'NUMC',
    48  'PRICE_MANUAL'   'P'             1  'CHAR',

    49  'BWDAT'     'Val.date'          8   'DATS',

    50  'MATNR2'    'Level II'          18  'CHAR',         "UD1K949919
    51  'MATNR1'    'Level I'           18  'CHAR',         "UD1K949919
    52  'MAKTX'     'Mat.Description'   40  'CHAR',         "UD1K949919
    53  'ZCATX'     'Categ.for MCVS'     8  'CHAR'.         "UD1K949919


  perform modify_gt_fcat1.

endform.                    " CREATE_FIELD_CATEGORY1
*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED1
*&---------------------------------------------------------------------*
form data_changed1 using rr_data_changed
                        type ref to cl_alv_changed_data_protocol.
  data: ls_mod_cells type lvc_s_modi,
        ls_cells     type lvc_s_modi,
        lt_values type table of bapi_char_values with header line.

  data: l_field type lvc_fname,
        l_wertn type zwertn.

  field-symbols <fs> type any.

  loop at rr_data_changed->mt_good_cells into ls_mod_cells.
    read table gt_out index ls_mod_cells-row_id.

    if sy-subrc = 0.
      if ls_mod_cells-fieldname = 'MATNR'  or
         ls_mod_cells-fieldname = 'PROFL'  or
         ls_mod_cells-fieldname = 'EKGRP'  or
         ls_mod_cells-fieldname = 'BKCLS'  or
*        LS_MOD_CELLS-FIELDNAME = 'PEINH'  OR
         ls_mod_cells-fieldname = 'EKORG'  or
         ls_mod_cells-fieldname = 'QTA'    or
         ls_mod_cells-fieldname = 'LIFNR'  or
         ls_mod_cells-fieldname = 'LIFNR2' or
         ls_mod_cells-fieldname = 'WERTN'  or
         ls_mod_cells-fieldname = 'KZUST1_IN'.

        if ls_mod_cells-fieldname = 'WERTN'.
          clear l_wertn.
          l_wertn = gt_out-wertn.
        endif.

        call method rr_data_changed->modify_cell
               exporting i_row_id    = ls_mod_cells-row_id
                         i_fieldname = ls_mod_cells-fieldname
                         i_value     = ls_mod_cells-value.

        case ls_mod_cells-fieldname.
          when 'WERTN'.                " Price
            perform change_wertn using rr_data_changed
                                       ls_mod_cells
                                       'GT_OUT'
                                       l_wertn.

          when 'KZUST1_IN'.                                 " RS1
            perform change_kzust using rr_data_changed
                                       ls_mod_cells
                                       'GT_OUT'.

          when 'LIFNR'.
            call method rr_data_changed->modify_cell
                 exporting i_row_id    = ls_mod_cells-row_id
                           i_fieldname = ls_mod_cells-fieldname
                           i_value     = ls_mod_cells-value.

            gt_out-lifnr = ls_mod_cells-value.

            select single land1 into gt_out-land1
              from lfa1
             where lifnr = gt_out-lifnr.

          when others.
            concatenate 'GT_OUT-' ls_mod_cells-fieldname
                   into l_field.
            assign (l_field) to <fs>.
            <fs> = ls_mod_cells-value.

            if ls_mod_cells-fieldname = 'MATNR'.
              select single maktg into gt_out-maktg
                from makt
               where matnr = ls_mod_cells-value.

              call method rr_data_changed->modify_cell
                     exporting i_row_id    = ls_mod_cells-row_id
                               i_fieldname = 'MAKTG'
                               i_value     = gt_out-maktg.
            endif.
        endcase.

        modify gt_out index ls_mod_cells-row_id.

      endif.
    endif.
  endloop.

* by IG.MOON 5/9/2007
  __set_refresh_mode 'X'.
  call method g_grid1->refresh_table_display
       exporting is_stable = stable.
* end

*  CALL METHOD G_GRID1->REFRESH_TABLE_DISPLAY.

endform.                    " DATA_CHANGED1
*&---------------------------------------------------------------------*
*&      Form  MODIFY_GT_FCAT1
*&---------------------------------------------------------------------*
form modify_gt_fcat1.
  loop at gt_fcat1 into gs_fcat1.
    if gs_fcat1-datatype = 'CURR'.
      gs_fcat1-just = 'R'.
      gs_fcat1-currency = 'USD'.
*      GS_FCAT1-REF_FIELD = 'WERTN1'.
*      GS_FCAT1-REF_TABLE = 'ZTCOU102'.
      modify gt_fcat1 from gs_fcat1
             transporting just currency ref_field ref_table
             where datatype = 'CURR'.
    else.
      case gs_fcat1-fieldname.
        when 'MATNR' or 'MAKTG' or 'MATKL'.

          gs_fcat1-fix_column = 'X'.

          modify gt_fcat1 from gs_fcat1
             transporting fix_column
             where fieldname = 'MATNR'.

          if gs_fcat1-fieldname = 'MATNR' .
            gs_fcat1-ref_field = 'MATNR'.
            gs_fcat1-ref_table = 'MARA'.
            gs_fcat1-f4availabl = 'X'.

            modify gt_fcat1 from gs_fcat1
               transporting ref_field ref_table f4availabl
               where fieldname = 'MATNR'.
          endif.

        when 'PROFL'.
          gs_fcat1-ref_field = 'PROFL'.
          gs_fcat1-ref_table = 'MARA'.
          gs_fcat1-f4availabl = 'X'.
          gs_fcat1-just = 'C'.

          modify gt_fcat1 from gs_fcat1
             transporting ref_field ref_table f4availabl just
             where fieldname = 'PROFL'.

        when 'LIFNR' or 'LIFNR2'.
          gs_fcat1-ref_field = 'LIFNR'.
          gs_fcat1-ref_table = 'LFA1'.
          gs_fcat1-f4availabl = 'X'.

          modify gt_fcat1 from gs_fcat1
             transporting ref_field ref_table f4availabl
             where fieldname = gs_fcat1-fieldname.

        when 'BKLAS'.
          gs_fcat1-ref_field = 'BKLAS'.
          gs_fcat1-ref_table = 'T025T'.
          gs_fcat1-f4availabl = 'X'.
          gs_fcat1-just = 'C'.

          modify gt_fcat1 from gs_fcat1
             transporting ref_field ref_table f4availabl just
             where fieldname = gs_fcat1-fieldname.

        when 'PMEHT'.
          gs_fcat1-ref_field = gs_fcat1-fieldname.
          gs_fcat1-ref_table = 'ZTCOU102'.
          gs_fcat1-just = 'C'.
          gs_fcat1-convexit = 'CUNIT'.

        when 'BWDAT'.
          gs_fcat1-ref_field = gs_fcat1-fieldname.
          gs_fcat1-ref_table = 'ZTCOU102'.
        when 'STRAT' or 'SUBSTRAT' or 'ELEMT' or 'ELEMTNS'
            or 'PRICE_MANUAL'.
          gs_fcat1-ref_field = gs_fcat1-fieldname.
          gs_fcat1-ref_table = 'ZTCOU102'.

          modify gt_fcat1 from gs_fcat1
             transporting ref_field ref_table just convexit
             where fieldname = gs_fcat1-fieldname.

        when 'EKGRP'.
          gs_fcat1-ref_field = 'EKGRP'.
          gs_fcat1-ref_table = 'MARC'.
          gs_fcat1-f4availabl = 'X'.
          gs_fcat1-just = 'C'.

          modify gt_fcat1 from gs_fcat1
             transporting ref_field ref_table f4availabl just
             where fieldname = gs_fcat1-fieldname.

        when 'QTA'.
          gs_fcat-just = 'R'.

          modify gt_fcat1 from gs_fcat1
             transporting just where fieldname = 'QTA'.

        when 'KZUST1_IN'.
          gs_fcat1-f4availabl = 'X'.
          modify gt_fcat1 from gs_fcat1 transporting f4availabl
             where fieldname = gs_fcat1-fieldname.

        when 'EC_G'.
          gs_fcat1-reptext = 'Error Category-Purchasing Group'.
          gs_fcat1-outputlen = 1.
          gs_fcat1-just = 'C'.

          modify gt_fcat1 from gs_fcat1
            transporting reptext outputlen just
             where fieldname = gs_fcat1-fieldname.

        when 'EC_A'.
          gs_fcat1-reptext = 'Error Category-Account'.
          gs_fcat1-outputlen = 1.
          gs_fcat1-just = 'C'.

          modify gt_fcat1 from gs_fcat1
            transporting reptext outputlen just
             where fieldname = gs_fcat1-fieldname.

        when 'EC_S'.
          gs_fcat1-reptext = 'Error Category-Source'.
          gs_fcat1-outputlen = 1.
          gs_fcat1-just = 'C'.

          modify gt_fcat1 from gs_fcat1
            transporting reptext outputlen just
             where fieldname = gs_fcat1-fieldname.

        when 'EC_Q'.
          gs_fcat1-reptext = 'Error Category-Quota'.
          gs_fcat1-outputlen = 1.
          gs_fcat1-just = 'C'.

          modify gt_fcat1 from gs_fcat1
            transporting reptext outputlen just
             where fieldname = gs_fcat1-fieldname.

        when 'EC_V'.
          gs_fcat1-reptext = 'Error Category-Vendor'.
          gs_fcat1-outputlen = 1.
          gs_fcat1-just = 'C'.

          modify gt_fcat1 from gs_fcat1
            transporting reptext outputlen just
             where fieldname = gs_fcat1-fieldname.

        when 'EC_P'.
          gs_fcat1-reptext = 'Error Category-Price'.
          gs_fcat1-outputlen = 1.
          gs_fcat1-just = 'C'.

          modify gt_fcat1 from gs_fcat1
            transporting reptext outputlen just
             where fieldname = gs_fcat1-fieldname.

        when 'EC_R'.
          gs_fcat1-reptext = 'Error Category-Reason'.
          gs_fcat1-outputlen = 1.
          gs_fcat1-just = 'C'.

          modify gt_fcat1 from gs_fcat1
            transporting reptext outputlen just
             where fieldname = gs_fcat1-fieldname.

        when 'STAT' or 'ESTAT'.
          gs_fcat1-just = 'C'.

          modify gt_fcat1 from gs_fcat1 transporting just
             where fieldname = gs_fcat1-fieldname.

        when 'ICON'.
          gs_fcat1-just = 'C'.

          modify gt_fcat1 from gs_fcat1
             transporting just
             where fieldname = gs_fcat1-fieldname.

      endcase.

    endif.

  endloop.

endform.                    " MODIFY_GT_FCAT1
*&---------------------------------------------------------------------*
*&      Form  FILL_FIELD_CATEGORY1
*&---------------------------------------------------------------------*
form fill_field_category1 using p_pos   type lvc_colpos
                                p_fname type lvc_fname
                                p_txt   type lvc_txtcol
                                p_len   type lvc_outlen
                                p_type  type datatype_d.

  clear gs_fcat1.

  gs_fcat1-col_pos   = p_pos.     " Column position
  gs_fcat1-fieldname = p_fname.   " Field name
  gs_fcat1-coltext   = p_txt.     " Column heading
  gs_fcat1-outputlen = p_len.     " Column width
  gs_fcat1-datatype  = p_type.    " Data type

  append gs_fcat1 to gt_fcat1.

endform.                    " FILL_FIELD_CATEGORY1
*&---------------------------------------------------------------------*
*&      Form  DOUBLE_CLICK
*&---------------------------------------------------------------------*
*       move the detail screen when double click
*----------------------------------------------------------------------*
form double_click using  e_row     type lvc_s_row
                         e_column  type lvc_s_col
                         es_row_no type lvc_s_roid.
  clear gv_index.
  gv_index = e_row-index.

  read table gt_out index gv_index.
  if sy-subrc = 0.
    case e_column.
      when 'MATNR'.
        set parameter id 'MAT'  field gt_out-matnr.
        call transaction 'MM03' and skip first screen.
      when 'WERTN'.
        set parameter id: 'MAT'  field gt_out-matnr,
                          'WRK'  field gt_out-werks,
*                        'KRT'  FIELD GT_OUT-
*                        'VSN'  FIELD
                          'KAD'  field gt_out-kadky.
        call transaction 'CK13N' and skip first screen.
      when others.
        perform call_change_info.
    endcase.
  endif.

endform.                    " DOUBLE_CLICK
*&---------------------------------------------------------------------*
*&      Form  BUILD_CELL_ATTR1
*&---------------------------------------------------------------------*
*       Create attributes of cell
*----------------------------------------------------------------------*
form build_cell_attr1.
  data: lt_celltab type lvc_t_styl,
        ls_celltab type lvc_s_styl.

  clear lt_celltab.
  refresh lt_celltab.

  clear gs_fcat1.

* PU user : PUUSR : no change
  if gv_level = 'PUUSR'.
    loop at gt_fcat1 into gs_fcat1.
      ls_celltab-fieldname = gs_fcat1-fieldname.
      ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
      insert ls_celltab into table lt_celltab.
    endloop.

  else.
    loop at gt_fcat1 into gs_fcat1.
      ls_celltab-fieldname = gs_fcat1-fieldname.

      if ls_celltab-fieldname = 'PROFL'  or
         ls_celltab-fieldname = 'WERKS'  or
         ls_celltab-fieldname = 'EKGRP'  or
         ls_celltab-fieldname = 'BKCLS'  or
*        LS_CELLTAB-FIELDNAME = 'PEINH'  OR
         ls_celltab-fieldname = 'EKORG'  or
         ls_celltab-fieldname = 'QTA'    or
         ls_celltab-fieldname = 'LIFNR'  or
         ls_celltab-fieldname = 'LIFNR2' or
         ls_celltab-fieldname = 'ZTEXT'  or
         ls_celltab-fieldname = 'WERTN'  or
         ls_celltab-fieldname = 'STAWN'.     "DUTY CODE changeable
        ls_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
      elseif ls_celltab-fieldname = 'KZUST1_IN'.
        if s_kalka-low <> 'BP'.
          ls_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
        endif.
      else.
        ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
      endif.

      insert ls_celltab into table lt_celltab.
    endloop.
  endif.

  clear gt_out-celltab.
  insert lines of lt_celltab into table gt_out-celltab.
  modify gt_out transporting celltab where celltab is initial
                                      and  zlock eq space.

  perform build_cell_attr1_lock.

endform.                    " BUILD_CELL_ATTR1
*&---------------------------------------------------------------------*
*&      Form  REFRESH_FIELD1
*&---------------------------------------------------------------------*
form refresh_field1.
*  CALL METHOD G_GRID1->SET_FRONTEND_FIELDCATALOG
*       EXPORTING
*         IT_FIELDCATALOG = GT_FCAT1.

  call method g_grid1->set_frontend_layout
       exporting
         is_layout = gs_layo1.

  __set_refresh_mode 'X'.

  call method g_grid1->refresh_table_display
              exporting is_stable = stable.

  call method cl_gui_cfw=>flush.

*  DATA: LS_MOD_CELLS TYPE LVC_S_MODI,
*        LS_CELLS     TYPE LVC_S_MODI,
*        LT_VALUES    TYPE TABLE OF BAPI_CHAR_VALUES WITH HEADER LINE.

*  LOOP AT RR_DATA_CHANGED->MT_GOOD_CELLS INTO LS_MOD_CELLS.
*
*  endloop.

endform.                    " REFRESH_FIELD1
*&---------------------------------------------------------------------*
*&      Form  CREATE_F4_FIELDS
*&---------------------------------------------------------------------*
*       Define possible entry fields
*----------------------------------------------------------------------*
form create_f4_fields using p_grid.
  clear: gs_f4, gt_f4, gt_f4[].

* F4 FIELD
  gs_f4-fieldname  = 'KZUST1_IN'.
  gs_f4-register   = 'X'.
  append gs_f4 to gt_f4.

  gs_f4-fieldname  = 'KZUST2_IN'.
  gs_f4-register   = 'X'.
  append gs_f4 to gt_f4.

  if p_grid = 'G_GRID1'.
    call method g_grid1->register_f4_for_fields
           exporting it_f4 = gt_f4.
  else.
    call method g_grid->register_f4_for_fields
           exporting it_f4 = gt_f4.
  endif.

endform.                    " CREATE_F4_FIELDS
*&---------------------------------------------------------------------*
*&      Form  ON_F4
*&---------------------------------------------------------------------*
*       Define possible entries
*----------------------------------------------------------------------*
form on_f4 using sender         type ref to cl_gui_alv_grid
                 e_fieldname    type lvc_fname
                 e_fieldvalue   type lvc_value
                 es_row_no      type lvc_s_roid
                 er_event_data  type ref to cl_alv_event_data
                 et_bad_cells   type lvc_t_modi
                 e_display      type char01
                 p_tab.

  data lt_f4 type table of ddshretval.

* Call my personal f4-help
  clear: lt_f4, lt_f4[].

  if p_tab = 'GT_OUT'.
    call method g_event_receiver1->my_f4
      exporting
        sender        = sender
        es_row_no     = es_row_no
        er_event_data = er_event_data
        et_bad_cells  = et_bad_cells
        e_display     = e_display
        e_fieldname   = e_fieldname
      importing
        lt_f4         = lt_f4.

  else.
    call method g_event_receiver->my_f4
      exporting
        sender        = sender
        es_row_no     = es_row_no
        er_event_data = er_event_data
        et_bad_cells  = et_bad_cells
        e_display     = e_display
        e_fieldname   = e_fieldname
      importing
        lt_f4         = lt_f4.
  endif.

* Assign the cell table fieldsymbol to the dereferenced data table and
* fill the table.
  assign er_event_data->m_data->* to <f4tab>.

  read table lt_f4 into ls_f4 index 1.

  check not ls_f4 is initial.

  perform f4_aply using es_row_no-row_id
                        ls_f4-fieldname
                        p_tab.

* To avoid standard f4-help.
  er_event_data->m_event_handled = 'X'.

endform.                                                    " ON_F4
*&---------------------------------------------------------------------*
*&      Form  F4_APLY
*&---------------------------------------------------------------------*
form f4_aply using  es_row_no_row_id
                    e_fieldname type fieldname
                    p_tab.
  ls_modi-row_id    = es_row_no_row_id.
  ls_modi-fieldname = e_fieldname.
  ls_modi-value     = ls_f4-fieldval.
  append ls_modi to <f4tab>.

  case e_fieldname.
    when 'KZUST1_IN'.
      if p_tab = 'GT_OUT'.
        gt_out-kzust1_in = ls_f4-fieldval.
      else.
        gt_dtl-kzust1_in = ls_f4-fieldval.
      endif.

    when 'KZUST2_IN'.
      gt_dtl-kzust2_in = ls_f4-fieldval.
  endcase.

  read table gt_out index es_row_no_row_id.

endform.                                                    " F4_APLY
*&---------------------------------------------------------------------*
*&      Form  MY_F4
*&---------------------------------------------------------------------*
form my_f4 tables et_f4         structure ddshretval
           using  sender        type ref to cl_gui_alv_grid
                  et_bad_cells  type lvc_t_modi
                  es_row_no     type lvc_s_roid
                  er_event_data type ref to cl_alv_event_data
                  e_display     type c
                  e_fieldname   type lvc_fname
                  p_tab.

  data : ls_out        like line of gt_out,
         ls_dtl        like line of gt_dtl,
         lt_fcat       type lvc_t_fcat,
         ls_fieldcat   type lvc_s_fcat,
         lv_tabname    type dd03v-tabname,
         lv_fieldname  type dd03v-fieldname,
         lv_help_value type help_info-fldvalue,
         lt_bad_cell   type lvc_t_modi,
         l_wa          type ref to data.

  field-symbols : <l_field_value> type any,
                  <ls_wa>         type any.

  call method sender->get_frontend_fieldcatalog
    importing
      et_fieldcatalog = lt_fcat.

  if p_tab = 'GT_OUT'.
    read table gt_out index es_row_no-row_id into ls_out.

    if sy-subrc = 0.
      create data l_wa like line of gt_out.
      assign l_wa->* to <ls_wa>.
      <ls_wa> = ls_out.
    endif.

  else.
    read table gt_dtl index es_row_no-row_id into ls_dtl.

    if sy-subrc = 0.
      create data l_wa like line of gt_dtl.
      assign l_wa->* to <ls_wa>.
      <ls_wa> = ls_dtl.
    endif.
  endif.

  read table lt_fcat with key fieldname = e_fieldname
                     into ls_fieldcat.
  if sy-subrc = 0.
    lv_tabname = ls_fieldcat-ref_table.
    lv_fieldname = ls_fieldcat-fieldname.

    assign component ls_fieldcat-fieldname
                  of structure ls_out to <l_field_value>.

    write <l_field_value> to lv_help_value.
  endif.

  perform f4_set in program bcalv_f4
                 using sender
                       lt_fcat
                       lt_bad_cell
                       es_row_no-row_id
                       <ls_wa>.

  if e_fieldname = 'KZUST1_IN' or e_fieldname = 'KZUST2_IN'.
    perform f4_reason using e_fieldname.
  endif.

  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
       exporting
            retfield        = e_fieldname
       tables
            field_tab       = gt_fields
            value_tab       = gt_values
            return_tab      = et_f4[]
       exceptions
            parameter_error = 1
            no_values_found = 2
            others          = 3.

  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

endform.                                                    " MY_F4
