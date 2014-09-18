*----------------------------------------------------------------------
* Program ID        : ZACOU119
* Title             : [CO] Material Change History
* Created on        : 12/18/2006
* Created by        : Michelle Jeong
* Specifications By : Andy Choi
* Description       : Report for Material Change History
*                     Copy & change program ZCOMMCHGH.
*                     Change doc listing
*                     Grouped into 3 chg types:
*                      0. Part Creation
*                      1. Part revision
*                      2. Price change
*                      3. Others
*----------------------------------------------------------------------
report zacou119 no standard page heading
                line-size 195 line-count 60.

tables: cdhdr,     " Change document header
        mara,      " General Material Data
        mard.      " Storage Location Data for Material

include zacoui00.

types: begin of ty_object,
         matnr type matnr,
         maktx type maktx,
        end of ty_object.

types: begin of ty_chgdoc.
include type cdred.
types:  maktx   type maktx,
        werks type werks_d,
       end of ty_chgdoc.

types: begin of ty_out,
        matnr1    type cdobjectv,
        objectid  type cdobjectv,
        maktx     type maktx,
        chgtype(1),
        chgtypet(15),
        werks     type werks_d,
        changenr  type  cdchangenr,
        tcode     type  cdtcode,
        username  type  cdusername,
        udate     type  cddatum,
        UTIME     type  cdtime,
        ftext     type  ddtext,
        f_old     type  cdfldvalo,
        f_new     type  cdfldvaln,
      end of ty_out.

data: gt_object type table of ty_object with header line,
      gt_chgdoc type table of ty_chgdoc with header line,
      gt_out    type table of ty_out    with header line.

*----------------------------------------------------------------------
* Selection-Screen
*----------------------------------------------------------------------
select-options: s_matnr  for cdhdr-objectid,    " Material
                s_udate  for cdhdr-udate,       " Change Date
                s_uname  for cdhdr-username,    " User Name
                s_tcode  for cdhdr-tcode,       " Transaction Code
                s_werks  for mard-werks,        " Plants
                s_mtart  for mara-mtart,        " Material type
                s_matkl  for mara-matkl.        " Material group
selection-screen skip.

* Filter change type
selection-screen begin of block chg0 with frame title text-001.
parameters : p_chg0 as checkbox default 'X',
             p_chg1 as checkbox default 'X',
             p_chg2 as checkbox default 'X',
             p_chg3 as checkbox default 'X'.
selection-screen end of block chg0.

* Layout
selection-screen begin of block b1 with frame title text-001.
parameter p_vari type slis_vari.
selection-screen end of block b1.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
at selection-screen on value-request for p_vari.
  perform alv_variant_f4 changing p_vari.

*----------------------------------------------------------------------*
* Start of selection
*----------------------------------------------------------------------*
start-of-selection.
  perform get_data.

*----------------------------------------------------------------------*
* End of selection
*----------------------------------------------------------------------*
end-of-selection.
  perform disp_result.

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       Get data
*----------------------------------------------------------------------*
form get_data.
  perform get_material.
  perform get_change_history.
  perform get_gt_out_for_disp.

endform.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_MATERIAL
*&---------------------------------------------------------------------*
*       Get Material
*----------------------------------------------------------------------*
form get_material.
  clear gt_object.
  refresh gt_object.

  select mara~matnr makt~maktx into table gt_object
    from mara
    join makt
      on makt~matnr = mara~matnr
     and makt~spras = sy-langu
   where mara~matnr in s_matnr
     and mtart in s_mtart
     and matkl in s_matkl.

endform.                    " GET_MATERIAL
*&---------------------------------------------------------------------*
*&      Form  GET_CHANGE_HISTORY
*&---------------------------------------------------------------------*
*       Get Material changed history
*----------------------------------------------------------------------*
form get_change_history.
  data: l_objectid type cdobjectv,
        lt_chgdoc type table of cdred with header line.

  clear gt_chgdoc.
  refresh gt_chgdoc.

  loop at gt_object.
    clear: l_objectid, lt_chgdoc.
    refresh lt_chgdoc.

    l_objectid = gt_object-matnr.

    call function 'CHANGEDOCUMENT_READ'
         exporting
              objectclass                = 'MATERIAL'
              objectid                   = l_objectid
         tables
              editpos                    = lt_chgdoc
         exceptions
              no_position_found          = 1
              wrong_access_to_archive    = 2
              time_zone_conversion_error = 3
              others                     = 4.

    loop at lt_chgdoc.
      move-corresponding lt_chgdoc to gt_chgdoc.
      gt_chgdoc-maktx = gt_object-maktx.

      append gt_chgdoc.
      clear gt_chgdoc.
    endloop.

  endloop.

  perform check_condition.

endform.                    " GET_CHANGE_HISTORY
*&---------------------------------------------------------------------*
*&      Form  GET_GT_OUT_FOR_DISP
*&---------------------------------------------------------------------*
*       Create Internal Table GT_OUT for display
*----------------------------------------------------------------------*
form get_gt_out_for_disp.
  clear gt_out.
  refresh gt_out.

  sort gt_chgdoc by objectid tcode.

* Filter chg type
  loop at gt_chgdoc.
    if gt_chgdoc-tcode = 'MM01'.
      if p_chg0 = 'X'.
        perform append_chgdoc_to_out.
      endif.

    elseif ( gt_chgdoc-tcode = 'MM02' or gt_chgdoc-tcode = 'MM03' ).
      if p_chg1 = 'X'.
        perform append_chgdoc_to_out.
      endif.

    elseif gt_chgdoc-tcode = 'MR21'.
      if p_chg2 = 'X'.
        perform append_chgdoc_to_out.
      endif.

    else.
      if p_chg3 = 'X'.
        perform append_chgdoc_to_out.
      endif.
    endif.

  endloop.

  sort gt_chgdoc by objectid changenr.

  data l_indx type sytabix.

  clear l_indx.
  loop at gt_out.
    l_indx = sy-tabix.

*  Change type
*    0. Part Creation
*    1. Part revision
*    2. Price change
*    3. Others
    case gt_out-tcode.
      when 'MM01'.
        gt_out-chgtype = '0'.
        gt_out-chgtypet = 'Parts Creation'.
      when 'MM02' or 'MM03'.
        gt_out-chgtype = '1'.
        gt_out-chgtypet = 'Parts Revision'.
      when 'MR21'.
        gt_out-chgtype = '2'.
        gt_out-chgtypet = 'Price Change'.
      when others.
        gt_out-chgtype = '3'.
        gt_out-chgtypet = 'Others'.
    endcase.

    modify gt_out index l_indx
           transporting chgtype chgtypet.
  endloop.

endform.                    " GET_GT_OUT_FOR_DISP
*&---------------------------------------------------------------------*
*&      Form  DISP_RESULT
*&---------------------------------------------------------------------*
*       Diplay result
*----------------------------------------------------------------------*
form disp_result.
  clear: gt_fieldcat, gs_layout, gt_events, gs_variant,
         gt_fieldcat[], gt_events[].

  perform build_field_category using:
   'OBJECTID'  'X'  'Object'         '18'   'CHAR',
   'MAKTX'     'X'  'Material Desc'  '25'   'CHAR',
   'CHANGENR'  'X'  'ChgNo'          '09'   'CHAR',
   'CHGTYPE'   ' '  'C'              '1'    'CHAR',
   'CHGTYPET'  ' '  'Chg Type'       '10'   'CHAR',
   'WERKS'     ' '  'Plant'          '4'    'CHAR',
   'TCODE'     ' '  'Tcd'            '5'    'CHAR',
   'USERNAME'  ' '  'User'           '10'   'CHAR',
   'UDATE'     ' '  'Date'           '10'   'DATS',
   'UTIME'     ' '  'Time'           '10'   'DATS',
   'FTEXT'     ' '  'Fld'            '25'   'CHAR',
   'F_OLD'     ' '  'Old'            '20'   'CHAR',
   'F_NEW'     ' '  'New'            '20'   'CHAR'.

  clear gs_layout.
  gs_layout-zebra             = 'X'.
  gs_layout-colwidth_optimize = 'X'.

  gv_repid = gs_variant-report = sy-repid.

* Set variant
  gs_variant-variant = p_vari.

  call function 'REUSE_ALV_GRID_DISPLAY'
       exporting
            i_callback_program = gv_repid
            is_layout          = gs_layout
            it_fieldcat        = gt_fieldcat[]
            i_save             = 'A'
            is_variant         = gs_variant
       tables
            t_outtab           = gt_out.

endform.                    " DISP_RESULT
*&---------------------------------------------------------------------*
*&      Form  check_condition
*&---------------------------------------------------------------------*
*       Check input conditions
*----------------------------------------------------------------------*
form check_condition.
  data l_indx type sy-tabix.

  clear l_indx.

  loop at gt_chgdoc.
    l_indx = sy-tabix.

    check: gt_chgdoc-udate    in s_udate,
           gt_chgdoc-username in s_uname,
           gt_chgdoc-tcode    in s_tcode.

*   Plant is a substring of tabkey
    gt_chgdoc-werks = gt_chgdoc-tabkey+21(4).

    if not ( s_werks is initial ) and
       not ( gt_chgdoc-werks is initial ).
      if gt_chgdoc-werks in s_werks.
      else.
        continue.
      endif.
    endif.

    modify gt_chgdoc index l_indx transporting werks.
  endloop.

endform.                    " check_condition
*&---------------------------------------------------------------------*
*&      Form  append_chgdoc_to_out
*&---------------------------------------------------------------------*
form append_chgdoc_to_out.

  move-corresponding gt_chgdoc to gt_out.

  shift gt_chgdoc-f_old left deleting leading space.
  shift gt_chgdoc-f_new left deleting leading space.

  gt_out-f_old    = gt_chgdoc-f_old.
  gt_out-f_new    = gt_chgdoc-f_new.

  append gt_out.

endform.                    " append_chgdoc_to_out
