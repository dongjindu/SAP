REPORT rpcpccu0 MESSAGE-ID rp LINE-SIZE 250.
TYPE-POOLS: slis.
TABLES: pcl1, pcl2.


INCLUDE <icon>.                        "Icon List
* International includes
INCLUDE rpc2cd09.                      "Cluster CD Data-Definition
INCLUDE rpc2ruu0.                      "Cluster RU Data-Definition
INCLUDE rpc2rx09.                      "Cluster RU Data-Definition
INCLUDE rpppxd00.                      "Data definition buffer PCL1/PCL2
INCLUDE rpppxd10.                      "Common part buffer PCL1/PCL2
INCLUDE rpppxm00.                      "Buffer handling routine

* Program includes
INCLUDE rpcpcc0d.
INCLUDE rpcpcc0s.
INCLUDE rpcpcc0f.
INCLUDE rpcpcc0o.
INCLUDE rpcpcc0i.

SELECTION-SCREEN FUNCTION KEY 2.


INITIALIZATION.
*  PERFORM determine_role.
  e_mode = 'ADM'.
  IF e_mode IS INITIAL.
    MESSAGE s016 WITH text-001.
    LEAVE PROGRAM.
  ENDIF.
* delete the line below later
*  e_mode = manager.
  IF e_mode EQ manager.
    CLEAR p_ex_acc.
    MOVE 'Cost Center List'(002) TO sscrfields-functxt_02.
    free_sel_button-text = space.
    free_sel_button-icon_id = icon_mapped_relation.
    free_sel_button-icon_text = 'Cost Center List'(002).
    sscrfields-functxt_02 = free_sel_button.
    SET TITLEBAR '001'.
    PERFORM get_cc_for_manager.
    PERFORM move_struc_to_cctab.
  ELSE.
    SET TITLEBAR '002'.
    ex_func-fcode = 'FC02'.
    APPEND ex_func.
    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
         EXPORTING
              p_status  = sy-pfkey
              p_program = sy-cprog
         TABLES
              p_exclude = ex_func.
  ENDIF.

START-OF-SELECTION.
  PERFORM select_employees.
  PERFORM set_flags.
  PERFORM get_runids_for_employee_select.
  PERFORM get_wage_types.
  PERFORM get_cc_and_post_date.
  IF select_kostl[] IS INITIAL.
    MESSAGE i016 WITH 'No Data Selected'(i05).
    STOP.
  ENDIF.

  PERFORM get_payroll_runid_posted.
  PERFORM select_relative_postings.

  IF select_kostl[] IS INITIAL.
    MESSAGE i016 WITH 'No Data Selected'(i05).
    STOP.
  ENDIF.

  PERFORM get_crossref_runid_docnum.
  PERFORM get_detail_empl_info.
  PERFORM get_wage_type_text.
  PERFORM extract_relative_info_only.
*  PERFORM check_authorizations.

  IF NOT list_table[] IS INITIAL.
    EXPORT LIST_TABLE[] TO MEMORY ID 'LIST' .
*    PERFORM display_progress USING 'D' '80'.
*    PERFORM set_default_sort_order
*      USING 'KOSTL' 'PERNR' 'HKONT' 'LGART' 'BUDAT'.
*    PERFORM load_variant.
*    IF sy-batch IS INITIAL.
*      CALL SCREEN '0100'.
*    ELSE.
*      PERFORM create_alv_spool.
*    ENDIF.
  ELSE.
    MESSAGE i016 WITH 'No Data Selected'(i05).
    STOP.
  ENDIF.

END-OF-SELECTION.
