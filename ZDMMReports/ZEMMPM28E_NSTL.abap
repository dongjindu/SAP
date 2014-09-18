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
         datum LIKE ztmm_dvrt-datum,
         matnr LIKE mara-matnr,
         uzeit LIKE ztmm_dvrt-uzeit,
         bdmng LIKE resb-bdmng,
         meins LIKE resb-meins,
*         atwrt LIKE ausp-atwrt,
*         rp06  LIKE ztpp_dvrt1-rp06,
       END OF it_order.

DATA : BEGIN OF it_zspp_vin_info_for_stl OCCURS 0.
        INCLUDE STRUCTURE zspp_vin_info_for_nstl.
DATA : END OF it_zspp_vin_info_for_stl.

DATA : BEGIN OF it_ztmm_dvrt OCCURS 0.
        INCLUDE STRUCTURE ztmm_dvrt.
DATA : END OF it_ztmm_dvrt.

**--- Variables
DATA : w_atinn LIKE ausp-atinn,
       w_rp06  LIKE ztpp_dvrt1-rp06,
       w_subrc LIKE sy-subrc.


FIELD-SYMBOLS : <fsl>,
                <fsh>,
                <fsw>.

DATA : w_index(2) TYPE n VALUE '01',
       w_fnamel(9),
       w_fnameh(9),
       w_fnamew(14).

**--- Constants
CONSTANTS : c_werks LIKE t001w-werks VALUE 'P001',
            c_atwrt LIKE ausp-atwrt VALUE '06',
            c_mtart LIKE mara-mtart VALUE 'ROH',
            c_time_plus TYPE t VALUE '000001'.


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
                     WITH p_tocred = p_datum
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

*--- Read Vehicle Master
  PERFORM read_vehicle_master USING w_subrc.

  CHECK w_subrc EQ 0.

  PERFORM make_mm_dvrt.

**---
*  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
*       EXPORTING
*            input  = 'P_PLAN_ORDER'
*       IMPORTING
*            output = w_atinn.
*
*  CONCATENATE p_datum '%' INTO w_rp06.
*
**---
*  EXEC sql performing append_planned_order.
*    select /*+ ordered*/
*           w.matnr,  x.bdmng,  x.meins,  y.atwrt,  substr(z.rp06,9,6)
*           into :it_order
*           from ztpp_dvrt1 z, ausp y, resb x, mara w
**- Z & Y
*          where z.mandt = :sy-mandt
*            and z.rp06 like :w_rp06
*            and y.mandt = z.mandt
**            and y.objek = concat(z.modl,z.body_ser)
*            and y.objek = concat('EMF',z.body_ser)
*            and y.atinn = :w_atinn
*            and y.klart = '002'
**- Y & X
*            and x.mandt = y.mandt
**            and x.plnum = concat('0000',substr(y.atwrt,1,6))
**            and x.plnum = substr(y.atwrt,1,10)
*            and x.plnum = y.atwrt
**- X & W
*            and w.mandt = x.mandt
*            and w.matnr = x.matnr
*            and w.mtart = 'ROH'
*  ENDEXEC.

*--- read raw data
  EXEC SQL PERFORMING APPEND_PLANNED_ORDER.
    SELECT /*+ ORDERED*/
           Z.DATUM,  X.MATNR,  Z.UZEIT,  Y.BDMNG,  Y.MEINS
           INTO :IT_ORDER
           FROM ZTMM_DVRT Z, RESB Y, MARA X
*- Z & Y
          WHERE Z.MANDT = :SY-MANDT
            AND Y.MANDT = Z.MANDT
            AND Y.RSNUM = Z.RSNUM
*- Y & X
            AND X.MANDT = Y.MANDT
            AND X.MATNR = Y.MATNR
            AND X.MTART = :C_MTART
  ENDEXEC.

*  SELECT datum
*         b~matnr
*         bdmng
*         a~meins
*         uzeit
*               INTO CORRESPONDING FIELDS OF TABLE it_order
*               FROM resb AS a INNER JOIN mara AS b
*                 ON a~mandt EQ b~mandt
*                AND a~matnr EQ b~matnr
*                    INNER JOIN ztmm_dvrt AS c
*                       ON a~mandt EQ c~mandt
*                      AND a~rsnum EQ c~rsnum
*              WHERE b~mtart EQ c_mtart.

  SORT it_order BY datum matnr uzeit.

*---
  LOOP AT it_order.
    MOVE : it_order-datum TO it_itab-datum,
           it_order-matnr TO it_itab-matnr,
           it_order-meins TO it_itab-meins.
*    assign_proper_field : it_order-rp06.
    assign_proper_field : it_order-uzeit.
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

  COMMIT WORK.

*---
  DESCRIBE TABLE it_itab LINES l_lines.

  WRITE : / '* Total', l_lines, 'Updated'.
ENDFORM.                    " update_table

*&---------------------------------------------------------------------*
*&      Form  read_vehicle_master
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_vehicle_master USING p_subrc.
*---
  CLEAR : it_zspp_vin_info_for_stl, it_zspp_vin_info_for_stl[].

  CALL FUNCTION 'Z_FPP_GET_NON_SUPPLY_TO_LINE'
       EXPORTING
            i_werks                  = c_werks
            i_atwrt                  = c_atwrt
            i_date                   = p_datum
       TABLES
            t_supply_info            = it_zspp_vin_info_for_stl
       EXCEPTIONS
            no_data_founded          = 1
            line_info_does_not_exist = 2
            etc_exception            = 3
            OTHERS                   = 4.

  MOVE : sy-subrc TO w_subrc.

  DELETE it_zspp_vin_info_for_stl WHERE rsnum EQ space.
ENDFORM.                    " read_vehicle_master

*&---------------------------------------------------------------------*
*&      Form  make_mm_dvrt
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_mm_dvrt.
*--- delete DVRT Table
  DELETE FROM ztmm_dvrt WHERE plnum NE space.

  COMMIT WORK.

*--- insert DVRT Table
  LOOP AT it_zspp_vin_info_for_stl.
    MOVE-CORRESPONDING it_zspp_vin_info_for_stl TO it_ztmm_dvrt.
    MOVE : it_zspp_vin_info_for_stl-p_rp06(8)   TO it_ztmm_dvrt-datum,
           it_zspp_vin_info_for_stl-p_rp06+8(6) TO it_ztmm_dvrt-uzeit.

    it_ztmm_dvrt-uzeit = it_ztmm_dvrt-uzeit + c_time_plus.

    IF it_ztmm_dvrt-uzeit BETWEEN '000001' AND '040000'.
      it_ztmm_dvrt-datum = it_ztmm_dvrt-datum - 1.
    ENDIF.

    it_ztmm_dvrt-erdat = it_ztmm_dvrt-aedat = sy-datum.
    it_ztmm_dvrt-erzet = it_ztmm_dvrt-aezet = sy-uzeit.
    it_ztmm_dvrt-ernam = it_ztmm_dvrt-aenam = sy-uname.
    APPEND it_ztmm_dvrt.
    CLEAR : it_ztmm_dvrt, it_zspp_vin_info_for_stl.
  ENDLOOP.

  MODIFY ztmm_dvrt FROM TABLE it_ztmm_dvrt.

  COMMIT WORK.
ENDFORM.                    " make_mm_dvrt
