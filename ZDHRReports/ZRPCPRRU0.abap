REPORT rpcprru0  NO STANDARD PAGE HEADING LINE-COUNT 65 LINE-SIZE 132.
*----------------------------------------------------------------------*
*            HRUS - Payroll Reconciliation Report (PRR)                *
*----------------------------------------------------------------------*
* 08.19.2014      Victor     T-code has been deleted for APM         *

INFOTYPES: 0000, 0001, 0002.

TABLES: pernr, pcl1, pcl2, pa0001, pa0002, qppnp.

TABLES: t549a, t549q, t549s, t549t, t529u,t5963, t51t8,
        t596i, t596a, t596b, t596g, t596h, t5uxx, t5uxy, v_5uxy_a,
        t5u0p, t5utw, t5utt, t5utz, t5uto, t5ut7, t5ut8, t512w, t512t.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*


INCLUDE ZRPC2CD09.
*INCLUDE rpc2cd09.                      "Cluster CD Data-Definition
INCLUDE ZRPC2RUU0.
*INCLUDE rpc2ruu0.                      "Cluster RU Data-Definition
INCLUDE ZRPC2RX09.
*INCLUDE rpc2rx09.                      "Cluster RU Data-Definition
INCLUDE ZRPPPXD00.
*INCLUDE rpppxd00.                      "Data definition buffer PCL1/PCL
INCLUDE ZRPPPXD10.
*INCLUDE rpppxd10.                      "Common part buffer PCL1/PCL2
INCLUDE ZRPPPXM00.
*INCLUDE rpppxm00.                      "Buffer handling routine
INCLUDE ZRPUMKC00.
*INCLUDE rpumkc00.                      "FORM re549d - Features
INCLUDE ZRPHRPYUD.
*INCLUDE rphrpyud.                      "Standard PR Report Data Defs.
INCLUDE ZRPCPRRUD.
*INCLUDE rpcprrud.
INCLUDE ZRPCPRRUS.
*INCLUDE rpcprrus.
INCLUDE ZRPHRPTMP.
*INCLUDE rphrptmp.                      "Standard PR Report Forms.
INCLUDE ZRPCPRRUF.
*INCLUDE rpcprruf.                      "Forms

** CE on/off switches
*CLASS: cl_hrce_masterswitches DEFINITION LOAD.
*
*LOAD-OF-PROGRAM.
** Run the CE-enabled report if CE-switch is ON
*  IF cl_hrce_masterswitches=>ce_is_active EQ
*     cl_hrce_masterswitches=>true.
*    SUBMIT rpcprru0_ce VIA SELECTION-SCREEN.
*  ENDIF.

INITIALIZATION.
  PERFORM screen_initialization.

AT SELECTION-SCREEN.
  PERFORM process_selection_screen.

START-OF-SELECTION.
  PERFORM init.

GET pernr.
  PERFORM init_tables_per_ee.
  PERFORM get_payroll_results.
  PERFORM get_original_payrolls.
  PERFORM extract_payroll_data.
* perform reconciliation for state of NY
*  PERFORM ny_reconciliation.
  PERFORM sort_data_tables.
  PERFORM fill_final_tables.

END-OF-SELECTION.                      " End of LDB loop
  PERFORM print_out_details.
  PERFORM print_out_totals.
  PERFORM print_out_log.

TOP-OF-PAGE.
  PERFORM print_page_header.
