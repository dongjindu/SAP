*----------------------------------------------------------------------*
*   INCLUDE ZAPP000L_ALV_TOP                                           *
*----------------------------------------------------------------------*
*****// ALV //**********************************************************
type-pools: slis.
data: gt_fieldcat type slis_t_fieldcat_alv,
      gt_fc       type slis_t_fieldcat_alv,
      g_fieldcat_s like line of gt_fieldcat,
      gs_layout   type slis_layout_alv,
      gs_print    type slis_print_alv,
      gt_sort     type slis_t_sortinfo_alv,
      gt_sp_group type slis_t_sp_group_alv,
      gt_events   type slis_t_event,
      gt_header   type slis_t_listheader,
      gt_header1  type slis_t_listheader,
      gt_colinfo_table type slis_t_specialcol_alv. "line color.

* hierarchy(simple)
data : g_tabname_header       type slis_tabname,       "header part
       g_tabname_item         type slis_tabname,       "detail list
       gs_keyinfo             type slis_keyinfo_alv.   "relation key

* return
data : g_exit_caused_by_caller  type c,
       gs_exit_caused_by_user   type slis_exit_by_user.

data: col_pos type i,
      cnt     type i,
      g_save  type c,
      g_repid like sy-repid,
      g_variant like disvariant.
data: gt_extab type slis_t_extab with header line.
