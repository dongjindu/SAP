************************************************************************
* Program Name      : ZAPP_ZTPPER_CHANGE_POINTER
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
* Date       Developer
* 10/07/2010 Wayne Kim  Copy it from ZAPP_ZTPPVR_CHANGE_POINTER
*
*
************************************************************************

REPORT zapp_ztpper_change_pointer .

TABLES: ztpper.

CONSTANTS: c_equi(10)  TYPE c VALUE 'EQUI'.
DATA: it_er      LIKE ztpper    OCCURS 0 WITH HEADER LINE.
DATA: it_chptr   LIKE bdi_chptr OCCURS 0 WITH HEADER LINE.
DATA: i_total    TYPE i.

* ig.moon {

*DATA: it_scrp        LIKE ztpp_scrap_car OCCURS 0 WITH HEADER LINE.
*DATA: i_total_scrp   TYPE i.

DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.
CONSTANTS:  false VALUE ' ',
            true  VALUE 'X'.
* }

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
  SELECT * INTO TABLE it_er
    FROM ztpper
    WHERE zbdat IN p_date  AND
* by ig.moon 9/24 {
          zbtim IN p_time  AND
* }
          zresult EQ 'S'   AND
          EASSYID ne ' '.
  SORT it_er BY eassyid.
  DELETE ADJACENT DUPLICATES FROM it_er
     COMPARING eassyid.
  DESCRIBE TABLE it_er LINES i_total.
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

  LOOP AT it_er.
    it_chptr-tabname  = c_equi.
    CONCATENATE 'O002'
                it_er-eassyid
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
ENDFORM.                    " write_result
