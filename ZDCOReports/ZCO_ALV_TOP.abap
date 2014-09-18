*----------------------------------------------------------------------*
*   INCLUDE ZCO_ALV_TOP                                                *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Macro
*----------------------------------------------------------------------*
define set_fieldcat.
  &1-seltext_l    = &2.
  &1-seltext_m    = &2.
  &1-seltext_s    = &2.
  &1-reptext_ddic = &2.
end-of-definition.

define set_fieldcat_sum.
  &1-do_sum   = &2.

end-of-definition.

define append_top.
  clear : gs_line.
  if not &3 is initial or not &4 is initial.
    gs_line-typ   = &1.
    gs_line-key   = &2.
    concatenate &3 '~' &4 into gs_line-info separated by space.
    append gs_line to gt_list_top_of_page .
  endif.
end-of-definition.


*-------------------------------------------------------------*
*  TYPES
*-------------------------------------------------------------*
type-pools  slis.

*-------------------------------------------------------------*
*  DATA
*-------------------------------------------------------------*
*-- For ALV LIST
*-- General Work fields
data : g_exit_caused_by_caller  type c,
       g_repid                  type sy-repid,
       g_save                   type c,
       c_progname               like sy-repid,
       c_itabname               type slis_tabname,
       c_inclname               like trdir-name,
       c_struname               like dd02l-tabname.


*-- Structures
data : gs_events               type slis_alv_event,
       gs_fieldcat             type slis_fieldcat_alv,
       gs_alv_sort             type slis_sortinfo_alv,
       gs_layout               type slis_layout_alv,
       gs_exit_caused_by_user  type slis_exit_by_user,
       gs_variant              type disvariant,
       gs_listheader           type slis_listheader.
*-- Internal tables
data : gt_events                type slis_t_event,
       gt_fieldcat              type slis_t_fieldcat_alv,
       gt_alv_sort              type slis_t_sortinfo_alv,
       gt_list_top_of_page      type slis_t_listheader,
       gt_listheader            type slis_t_listheader.

data :  gt_fieldcat2            type slis_t_fieldcat_alv,
        gt_alv_sort2            type slis_t_sortinfo_alv,
        gt_events2              type slis_t_event,
        gt_list_top_of_page2    type slis_t_listheader.

data : gs_line type slis_listheader.
*       gs_top_of_page type slis_t_listheader.



data : w_repid        like sy-repid,
       w_save.



*--------------------------------------------------------------*
*  CONSTANTS:
*--------------------------------------------------------------*
constants : c_status_set   type slis_formname
                                value 'PF_STATUS_SET',
            c_user_command type slis_formname
                               value 'USER_COMMAND',
            c_top_of_page  type slis_formname
                               value 'TOP_OF_PAGE',
            c_top_of_list  type slis_formname
                               value 'TOP_OF_LIST',
            c_end_of_list  type slis_formname
                               value 'END_OF_LIST'.

constants : c_status_set2   type slis_formname
                                value 'PF_STATUS_SET2',
            c_user_command2 type slis_formname
                               value 'USER_COMMAND2'.
