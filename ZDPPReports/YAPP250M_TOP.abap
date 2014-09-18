*&---------------------------------------------------------------------*
*& Include YAPP250M_TOP                                                *
*&                                                                     *
*&---------------------------------------------------------------------*

PROGRAM  yapp250m_prog_chg     MESSAGE-ID   zmpp .

TABLES: ztpp_change.

* main internal table
DATA  it_chg  LIKE  ztpp_change  OCCURS 0  WITH HEADER LINE.

CONTROLS: tc100 TYPE TABLEVIEW USING SCREEN 100.

DATA: BEGIN OF key,
       company  LIKE  zspp_app237-company,
       model    LIKE  zspp_app237-model,
       stats    LIKE  ztpp_process-status,
       fdate    LIKE  ztpp_change-cdate,
       tdate    LIKE  ztpp_change-cdate,
       bodyno   LIKE  ztpp_change-bodyno,
      END OF key.
DATA: BEGIN OF sav,
       company  LIKE  zspp_app237-company,
       model    LIKE  zspp_app237-model,
       stats    LIKE  ztpp_process-status,
       fdate    LIKE  ztpp_change-cdate,
       tdate    LIKE  ztpp_change-cdate,
       bodyno   LIKE  ztpp_change-bodyno,
      END OF sav.

* LIST BOX creation ------------------------------------
TYPE-POOLS vrm.
* Progress list
DATA: name   TYPE vrm_id,
      list   TYPE vrm_values,
      value  LIKE LINE OF list.
* Model list
DATA: name1  TYPE vrm_id,
      list1  TYPE vrm_values,
      value1 LIKE LINE OF list.

DATA: BEGIN OF  it_prog  OCCURS  0,
       status   LIKE  ztpp_process-status,
       progress LIKE  ztpp_process-progress,
       vmrp     LIKE  ztpp_process-vmrp,
      END OF it_prog.
DATA: BEGIN OF  it_model  OCCURS  0,
       model    LIKE  ztpp_veh_model-model,
       name     LIKE  ztpp_veh_model-name,
      END OF it_model.
* ------------------------------------------------------
DATA  g_init_skip(1)  TYPE  c.

*
RANGES: r_cdate  FOR  ztpp_change-cdate.
RANGES: r_bodyno FOR  ztpp_change-bodyno.
RANGES: r_brp    FOR  ztpp_change-b_rp.
