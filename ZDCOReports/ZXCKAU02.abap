*----------------------------------------------------------------------*
*   INCLUDE ZXCKAU02
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
include zxckau00_data.

*Check data saved
data: w_keko  like keko,
      w_werks like keko-werks.
if f_ckiuser-sobes = '7'. "Stock Trf.
  w_werks = f_ckiuser-sowrk.
else.
  w_werks = f_ckiuser-werks.
endif.
select single * into w_keko
     from keko
     where matnr = f_ckiuser-matnr
       and werks = w_werks
       and klvar = f_ckiuser-klvar
       and kadky = f_ckiuser-kadky  "costing from
** Changed on 10/15/13
       and stnum <> ' '.
** end on 10/15/13
if sy-subrc = 0.
  move-corresponding w_keko to f_ckiuser.
  include zxckau00_read_db.
else.
  include zxckau00_read.
endif.

*Module(Color) filtering
include zxckau00_module_color.

*----------------------------------------------------------------------

*--- ALV
type-pools: slis.
data : w_fieldcat type slis_t_fieldcat_alv with header line,
       w_eventcat type slis_t_event with header line,
       w_selfield type slis_selfield,
       w_sortcat  type slis_t_sortinfo_alv with header line,
       w_col_pos  type i,
       w_program  like sy-repid,
       w_top_of_page type slis_t_listheader,
       w_line1 type slis_listheader.

data: gt_fieldcat type slis_t_fieldcat_alv,
      gs_layout   type slis_layout_alv,
      gt_sp_group type slis_t_sp_group_alv,
      gt_events   type slis_t_event,
      gt_sorts    type slis_t_sortinfo_alv with header line,
      gs_prnt     type slis_print_alv,
      g_repid     like sy-repid.
*---- ALV


perform field_setting(zcogsrev) tables gt_fieldcat using :
'KSTAR'     'Cost.Ele'       '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'TYPPS'     'Type'           '01' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'UPGVC'     'UPGVC'          '18' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'UPGTX'     'UPG Desc'       '40' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'COMPN'     'Component'      '19' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'LTEXT'     'Component Desc' '40' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'REQQT'     'ReqQty'         '10' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'MEEHT'     'UoM'            '05' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'WERTN'     'Cst.Amt'        '14' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
'AMTDT'     'KD Duty'        '14' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
'AMTFT'     'KD Freight'     '14' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
'AMTOT'     'KD Other'       '14' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
'TOTAL'     'KD Total'       '14' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
'GPREIS'    'Unit$ '         '16' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'PEINH'     'PrcUnit'        '05' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'STRAT'     'PS'             '01' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'SPLNT'     'SrcPlant'       '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'LAND1'     'Country'        '02' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'LIFNR'     'Vendor'         '10' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'NAME1'     'Name'           '35' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'MENGE'     'Qty'            '10' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'STKKZ'     'Assy Ind'       '01' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'AUSSS'     'Assy.Scrap'     '10' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'KAUSF'     'Comp.Scrap'     '10' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'BAUSF'     'Scrap%'         '10' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'INFNR'     'Info'           '10' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'MTART'     'Mat type'       '05' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'XCHAR'     'BatchMgt'       '01' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'MSTAE'     'Stat1'          '02' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'MMSTA'     'Stat2'          '02' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'SOBSL'     'Sp.Prc'         '02' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'RGEKZ'     'BF ind'         '01' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'LGPRO'     'S.Loc'          '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'WERKS'     'Plant'          '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'MATMK'     'Mat.Grp'        '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'STAWN'     'DutyCd'         '16' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'POSTP'     'Type'           '01' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'SORTF'     'SortStr'        '02' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',

'BKLAS'     'V.Cls'          '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'VERPR'     'MAP'            '14' ' ' 'R'  ' '  ' '  '  ' ' '  ' '.


g_repid = sy-repid.

call function 'REUSE_ALV_GRID_DISPLAY'
     exporting
          i_callback_program = g_repid
          it_fieldcat        = gt_fieldcat
          i_save             = 'A'
     tables
          t_outtab           = itab
     exceptions
          program_error      = 1
          others             = 2.
