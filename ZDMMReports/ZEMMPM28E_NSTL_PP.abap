************************************************************************
* Program Name      : ZEMMPM28E_NSTL
* Author            : Sung-Tae, Lim
* Creation Date     : 2003.11.17.
* Specifications By : Sung-Tae, Lim
* Development Request No : UD1K901864
* Addl Documentation:
* Description       : Daily Supply to Line (Non Supply to Line)
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2003.11.17.     Sung-Tae Lim     UD1K901864     Initial Coding
*
*
************************************************************************

REPORT zemmpm28e_nstl NO STANDARD PAGE HEADING
                      LINE-SIZE 255
                      LINE-COUNT 64(1)
                      MESSAGE-ID zmmm.

**---
INCLUDE : zrmmpmxxr_incl.

**--- Tables, Views & Structures


**--- Internal Tables
DATA : it_itab LIKE ztmm_nstl OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_order OCCURS 0,
         matnr LIKE mara-matnr,
         bdmng LIKE resb-bdmng,
         meins LIKE resb-meins,
         atwrt LIKE ausp-atwrt,
         rp06  LIKE ztpp_dvrt1-rp06,
       END OF it_order.


**--- Variables
DATA : w_atinn LIKE ausp-atinn,
       w_rp06  LIKE ztpp_dvrt1-rp06.


FIELD-SYMBOLS : <fsl>,
                <fsh>,
                <fsw>.

DATA : w_index(2) TYPE n VALUE '01',
       w_fnamel(9),
       w_fnameh(9),
       w_fnamew(14).

**--- Constants



**---
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
*SELECT-OPTIONS : s_datum FOR mkpf-budat OBLIGATORY.
PARAMETERS : p_datum TYPE d OBLIGATORY DEFAULT sy-datum.
SELECTION-SCREEN END OF BLOCK block1.


**---
INITIALIZATION.


**---
TOP-OF-PAGE.


**---
START-OF-SELECTION.
  PERFORM get_data.


**---
END-OF-SELECTION.
  IF it_itab[] IS INITIAL.
    MESSAGE s999 WITH text-m01.
  ELSE.
    PERFORM update_table.
*    PERFORM write_data.
*/ Transfer Order Creation & Header Change (2003.12.02 Added by Hakchin)
    SUBMIT zemmpm28e_nstl_tocre
                     AND RETURN.
  ENDIF.

**---


*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
*---
  CLEAR : it_itab, it_itab[], w_atinn, w_rp06, it_order, it_order[].

**--- calculate time
*  PERFORM calc_time.

*---
  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
       EXPORTING
            input  = 'P_PLAN_ORDER'
       IMPORTING
            output = w_atinn.

  CONCATENATE p_datum '%' INTO w_rp06.

*---
  EXEC sql performing append_planned_order.
    select /*+ ordered*/
           w.matnr,  x.bdmng,  x.meins,  y.atwrt,  substr(z.rp06,9,6)
           into :it_order
           from ztpp_dvrt1 z, ausp y, resb x, mara w
*- Z & Y
          where z.mandt = :sy-mandt
            and z.rp06 like :w_rp06
            and y.mandt = z.mandt
*            and y.objek = concat(z.modl,z.body_ser)
            and y.objek = concat('EMF',z.body_ser)
            and y.atinn = :w_atinn
            and y.klart = '002'
*- Y & X
            and x.mandt = y.mandt
*            and x.plnum = concat('0000',substr(y.atwrt,1,6))
*            and x.plnum = substr(y.atwrt,1,10)
            and x.plnum = y.atwrt
*- X & W
            and w.mandt = x.mandt
            and w.matnr = x.matnr
            and w.mtart = 'ROH'
  ENDEXEC.

*---
  LOOP AT it_order.
    MOVE : it_order-matnr TO it_itab-matnr,
           it_order-meins TO it_itab-meins.
    assign_proper_field : it_order-rp06.
    COLLECT it_itab.
    CLEAR : it_itab.
  ENDLOOP.
ENDFORM.                    " get_data

*&---------------------------------------------------------------------*
*&      Form  append_planned_order
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_planned_order.
*---
  APPEND it_order.
  CLEAR : it_order.
ENDFORM.                    " append_planned_order

*&---------------------------------------------------------------------*
*&      Form  calc_time
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calc_time.
*---
  MOVE : '080001' TO w_time01l,
         '090000' TO w_time01h.

  DO 19 TIMES.
    w_index = sy-index + 1.
    CONCATENATE : 'W_TIME' w_index 'L' INTO w_fnamel,
                  'W_TIME' w_index 'H' INTO w_fnameh.
    ASSIGN : (w_fnamel) TO <fsl>,
             (w_fnameh) TO <fsh>.
    <fsl> = w_time01l + ( w_index - 1 ) * 3600.
    <fsh> = w_time01h + ( w_index - 1 ) * 3600.
  ENDDO.
ENDFORM.                    " calc_time

*&---------------------------------------------------------------------*
*&      Form  write_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_data.
*---
  DATA : l_index(2) TYPE n.

  LOOP AT it_itab.
    CLEAR : l_index.
    WRITE : /(18) it_itab-matnr.
    DO 20 TIMES.
      l_index = sy-index.
      CONCATENATE : 'IT_ITAB-TIME' l_index INTO w_fnamew.
      ASSIGN : (w_fnamew) TO <fsw>.
      WRITE : (10) <fsw> CURRENCY it_itab-meins.
    ENDDO.
    WRITE :   it_itab-meins.
  ENDLOOP.
ENDFORM.                    " write_data

*&---------------------------------------------------------------------*
*&      Form  update_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_table.
*---
  DELETE FROM ztmm_nstl WHERE matnr GE space.

*---
  DATA : l_lines TYPE i.

  LOOP AT it_itab.
    MOVE-CORRESPONDING it_itab TO ztmm_nstl.
    ztmm_nstl-erdat = ztmm_nstl-aedat = sy-datum.
    ztmm_nstl-erzet = ztmm_nstl-aezet = sy-uzeit.
    ztmm_nstl-ernam = ztmm_nstl-aenam = sy-uname.
    MODIFY ztmm_nstl.
    CLEAR : ztmm_nstl.
  ENDLOOP.

*---
  DESCRIBE TABLE it_itab LINES l_lines.

  WRITE : / '* Total', l_lines, 'Updated'.
ENDFORM.                    " update_table
