************************************************************************
* Program Name      : ZAPP_ZTPPVR_CHANGE_POINTER
* Author            : Chris Li
* Creation Date     : 03/29/2005
* Specifications By : Chris Li
* Pattern           :
* Development Request No:UD1K915213
* Addl Documentation:
* Description       : Creat the change pointers for the records in
*                     production interface table ZTPPVR.
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 03/29/2005 Chris        UD1K915231   DEDAULT DATE: LOW-SYSTEM DATE - 1
*                                                    HIGH- SYSTEM DATE
*
*
************************************************************************

REPORT zapp_ztppvr_change_pointer .

TABLES: ztppvr.

CONSTANTS: c_equi(10)  TYPE c VALUE 'EQUI'.
DATA: it_vr      LIKE ztppvr    OCCURS 0 WITH HEADER LINE.
DATA: it_chptr   LIKE bdi_chptr OCCURS 0 WITH HEADER LINE.
DATA: i_total    TYPE i.

* ig.moon {

DATA: it_scrp        LIKE ztpp_scrap_car OCCURS 0 WITH HEADER LINE.
DATA: i_total_scrp   TYPE i.

DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.
CONSTANTS:  false VALUE ' ',
            true  VALUE 'X'.
* }

* by Wayne Kim to get delta from UM
DATA: it_um        LIKE ztsd_um OCCURS 0 WITH HEADER LINE.
DATA: i_total_um   TYPE i.
*

* by Wayne Kim to get delta from Manifest
DATA: it_vm        LIKE ztppvm OCCURS 0 WITH HEADER LINE.
DATA: i_total_vm   TYPE i.
*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: p_date FOR sy-datum OBLIGATORY .
* by ig.moon 9/24 {
SELECT-OPTIONS: p_time FOR sy-uzeit NO-EXTENSION.
* }
SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.
  p_date-sign = 'I'.
  p_date-option = 'BT'.
  p_date-low = sy-datum - 1.
  p_date-high = sy-datum.
  APPEND p_date.

START-OF-SELECTION.

  PERFORM get_data.
  PERFORM prepare_object.
  IF  i_total > 0.
    PERFORM create_cp.
  ENDIF.

* by ig.moon 7/24/2008 {


  PERFORM get_data_scrp.
  PERFORM prepare_object_scrp.
  IF  i_total_scrp > 0.
    PERFORM create_cp.
  ENDIF.


* }

* by wayne 7/24/2013 {


  PERFORM get_data_um.
  PERFORM prepare_object_um.
  IF  i_total_um > 0.
    PERFORM create_cp.
  ENDIF.


* }

* by wayne 9/23/2013 {


  PERFORM get_data_vm.
  PERFORM prepare_object_vm.
  IF  i_total_vm > 0.
    PERFORM create_cp.
  ENDIF.


* }


END-OF-SELECTION.

  PERFORM write_result.
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  SELECT * INTO TABLE it_vr
    FROM ztppvr
    WHERE zbdat IN p_date  AND
* by ig.moon 9/24 {
          zbtim IN p_time  AND
* }
          zresult EQ 'S'.
  SORT it_vr BY p_model p_body_serial p_plant_no.
  DELETE ADJACENT DUPLICATES FROM it_vr
     COMPARING p_model p_body_serial p_plant_no.
  DESCRIBE TABLE it_vr LINES i_total.
ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  prepare_object
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM prepare_object.

  DATA: l_id LIKE bdi_chptr-tabkey.

  CHECK i_total NE 0.

  LOOP AT it_vr.
    it_chptr-tabname  = c_equi.
    CONCATENATE 'O002'
                it_vr-p_model
                it_vr-p_body_serial
      INTO l_id.
    it_chptr-tabkey   = l_id.
    it_chptr-cdobjcl  = 'CLASSIF'.
    APPEND it_chptr.
    CLEAR: it_chptr.

  ENDLOOP.
ENDFORM.                    " prepare_object
*&---------------------------------------------------------------------*
*&      Form  create_cp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_cp.
  CALL FUNCTION 'CTBW_BW_CHANGE_POINTERS'
    TABLES
      it_ale_stru = it_chptr.
ENDFORM.                    " create_cp
*&---------------------------------------------------------------------*
*&      Form  write_result
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_result.
  WRITE: / 'Total changed equipments: ', i_total.
  WRITE: / 'Total scrapped equipments: ', i_total_scrp.
  WRITE: / 'Total changed UM equipments: ', i_total_um.
  WRITE: / 'Total changed Manifest equipments: ', i_total_vm.
ENDFORM.                    " write_result
*&---------------------------------------------------------------------*
*&      Form  get_data_scrp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_scrp.

  __cls it_scrp.
  SELECT * INTO TABLE it_scrp
  FROM ztpp_scrap_car
  WHERE scr_date IN p_date.

  SORT it_scrp BY model body_ser.
  DELETE ADJACENT DUPLICATES FROM it_scrp COMPARING model body_ser.
  DESCRIBE TABLE it_scrp LINES i_total_scrp.

ENDFORM.                    " get_data_scrp
*&---------------------------------------------------------------------*
*&      Form  prepare_object_scrp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM prepare_object_scrp.

  CHECK i_total_scrp > 0.

  __cls it_chptr.

  LOOP AT it_scrp.

    it_chptr-tabname = c_equi.
    it_chptr-cdobjcl = 'CLASSIF'.

    CONCATENATE 'O002' it_scrp-model it_scrp-body_ser
        INTO it_chptr-tabkey.

    APPEND it_chptr.
    CLEAR it_chptr.
  ENDLOOP.

ENDFORM.                    " prepare_object_scrp
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_UM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_um .

  __cls it_um.
  SELECT * INTO TABLE it_um
  FROM ztsd_um
  WHERE aedat IN p_date
    AND body_no <> '000000'.

  SORT it_um BY model_code body_no.
  DELETE ADJACENT DUPLICATES FROM it_um COMPARING model_code body_no.
  DESCRIBE TABLE it_um LINES i_total_um.

ENDFORM.                    " GET_DATA_UM
*&---------------------------------------------------------------------*
*&      Form  PREPARE_OBJECT_UM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM prepare_object_um .
  CHECK i_total_um > 0.

  __cls it_chptr.

  LOOP AT it_um.

    it_chptr-tabname = c_equi.
    it_chptr-cdobjcl = 'CLASSIF'.

    CONCATENATE 'O002' it_um-model_code it_um-body_no
        INTO it_chptr-tabkey.

    APPEND it_chptr.
    CLEAR it_chptr.
  ENDLOOP.
ENDFORM.                    " PREPARE_OBJECT_UM
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_VM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_vm .

  __cls it_vm.
  SELECT * INTO TABLE it_vm
  FROM ztppvm
  WHERE zedat IN p_date
    AND p_body_serial <> '000000'.

  SORT it_vm BY p_model p_body_serial.
  DELETE ADJACENT DUPLICATES FROM it_vm COMPARING p_model p_body_serial.
  DESCRIBE TABLE it_vm LINES i_total_vm.

ENDFORM.                    " GET_DATA_VM
*&---------------------------------------------------------------------*
*&      Form  PREPARE_OBJECT_VM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM prepare_object_vm .
  CHECK i_total_vm > 0.

  __cls it_chptr.

  LOOP AT it_vm.

    it_chptr-tabname = c_equi.
    it_chptr-cdobjcl = 'CLASSIF'.

    CONCATENATE 'O002' it_vm-p_model it_vm-p_body_serial
        INTO it_chptr-tabkey.

    APPEND it_chptr.
    CLEAR it_chptr.
  ENDLOOP.
ENDFORM.                    " PREPARE_OBJECT_VM
