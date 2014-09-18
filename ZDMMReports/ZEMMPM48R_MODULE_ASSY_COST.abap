************************************************************************
* Program Name      : ZEMMPM48R_MODULE_ASSY _COST
* Author            : Byung-sung, Bae
* Creation Date     : 2004.06.28.
* Specifications By : Byung-sung, Bae
* Pattern           : Report 1-1
* Development Request No : UD1K911264
* Addl Documentation:
* Description       : Module assy Cost Estimation
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT zemmpm48r_module_assy_cost NO STANDARD PAGE HEADING
                                   LINE-SIZE 156
                                   LINE-COUNT 58.
TABLES: t024,
        mara,
        t001,
        cabn,
        cawn,
        cawnt,
        ztmm_assy_cost1,
        ztmm_assy_cost2,
        ztmm_asy_cost1_b,
        ztmm_asy_cost2_b,
        zsmm_assy_cost.

*----- Internal Tables
" If you want to append other cost condition type,
" append field in Structure ZSMM_assy_COST.
" And append Table control field.
" But condition type's pattern must be 'ZPxx'.

DATA: it_assy LIKE zsmm_assy_cost OCCURS 0 WITH HEADER LINE.

DATA: it_ztmm_assy_cost2 LIKE ztmm_assy_cost2 OCCURS 0
                                                WITH HEADER LINE,
      it_ztmm_asy_cost2_b LIKE ztmm_asy_cost2_b OCCURS 0
                                                WITH HEADER LINE.

*----- Global variables
DATA: wa_tot_page TYPE i,
      wa_line     TYPE i,
      wa_page     TYPE i.

*----- Constants
DATA: c_bukrs LIKE t001-bukrs VALUE 'H201',
      c_filename LIKE rlgrap-filename VALUE 'C:\TEMP\MODULE_COST.TXT'.

*----- Table controls
CONTROLS : tc_9000 TYPE TABLEVIEW USING SCREEN 9000.

*----- Selection screens
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-t01.
SELECT-OPTIONS: s_vtype FOR ztmm_assy_cost1-vtype
                            NO-EXTENSION NO INTERVALS,
                s_ekgrp FOR ztmm_assy_cost1-ekgrp
                            NO-EXTENSION NO INTERVALS,
                s_mcode FOR ztmm_assy_cost1-mcode.
SELECTION-SCREEN END   OF BLOCK bl1.

*----- Input value check & read data
AT SELECTION-SCREEN.
  CHECK sy-ucomm EQ 'ONLI'.
  PERFORM check_input_value.
  PERFORM read_data.

TOP-OF-PAGE.
  PERFORM display_header.

TOP-OF-PAGE DURING LINE-SELECTION.
  PERFORM display_header.

START-OF-SELECTION.
  SET PF-STATUS '9000'.
  SET TITLEBAR  '9000'.
  PERFORM display_data.

AT USER-COMMAND.
  CASE sy-ucomm.
    WHEN 'CREATE'.
      PERFORM create_rtn.
    WHEN 'CHANGE'.
      PERFORM change_rtn.
    WHEN 'DELETE'.
      PERFORM delete_rtn.
    WHEN 'DOWNLOAD'.
      PERFORM download_rtn.
    WHEN 'PAGE'.
      PERFORM display_total_page.
  ENDCASE.
*&---------------------------------------------------------------------*
*&      Form  CHECK_INPUT_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_input_value.
  PERFORM check_vtype.
  PERFORM check_company.
  PERFORM check_ekgrp.
ENDFORM.                    " CHECK_INPUT_VALUE
*&---------------------------------------------------------------------*
*&      Form  CHECK_EKGRP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_ekgrp.
  SELECT SINGLE * FROM t024 WHERE ekgrp IN s_ekgrp.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m02.
  ENDIF.
ENDFORM.                    " CHECK_EKGRP
*&---------------------------------------------------------------------*
*&      Form  read_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data.
  FIELD-SYMBOLS: <other_cost>.

  DATA: lw_other_cost(50).

  DATA: BEGIN OF lt_assy_cost1 OCCURS 0.
          INCLUDE STRUCTURE ztmm_assy_cost1.
  DATA:   maktx   LIKE   makt-maktx,
          name1   LIKE   lfa1-name1,
        END   OF lt_assy_cost1.

  DATA: lt_assy_cost2 LIKE ztmm_assy_cost2 OCCURS 0 WITH HEADER LINE.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_assy_cost1
    FROM ztmm_assy_cost1 AS a INNER JOIN lfa1 AS c
                              ON a~lifnr = c~lifnr
   WHERE a~vtype IN s_vtype
     AND a~mcode IN s_mcode
     AND a~ekgrp IN s_ekgrp.
  IF sy-subrc NE 0.
    MESSAGE s000(zz) WITH text-m05.
  ENDIF.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_assy_cost2
    FROM ztmm_assy_cost2
     FOR ALL ENTRIES IN lt_assy_cost1
   WHERE vtype EQ lt_assy_cost1-vtype
     AND mcode EQ lt_assy_cost1-mcode
     AND lifnr EQ lt_assy_cost1-lifnr
     AND seqno EQ lt_assy_cost1-seqno.
  IF sy-subrc NE 0.
    MESSAGE s000(zz) WITH text-m05.
  ENDIF.

*----- Set data for display layout
  LOOP AT lt_assy_cost1.
    CLEAR: it_assy.

    MOVE-CORRESPONDING lt_assy_cost1 TO it_assy.
    it_assy-total = it_assy-asytr.
    APPEND it_assy.

    LOOP AT lt_assy_cost2 WHERE vtype EQ lt_assy_cost1-vtype
                            AND mcode EQ lt_assy_cost1-mcode
                            AND lifnr EQ lt_assy_cost1-lifnr
                            AND seqno EQ lt_assy_cost1-seqno.

      READ TABLE it_assy WITH KEY vtype = lt_assy_cost2-vtype
                                  mcode = lt_assy_cost2-mcode
                                  lifnr = lt_assy_cost2-lifnr
                                  seqno = lt_assy_cost2-seqno.
      IF sy-subrc NE 0.
        MESSAGE e000(zz) WITH text-m01.
      ENDIF.

      CONCATENATE 'IT_assy-ZP' lt_assy_cost2-kschl+2(2)
             INTO lw_other_cost.
      ASSIGN (lw_other_cost) TO <other_cost>.

      MOVE lt_assy_cost2-kbetr TO <other_cost>.

      it_assy-total = it_assy-total + <other_cost>.
      MODIFY it_assy INDEX sy-tabix.
    ENDLOOP.
  ENDLOOP.

  SORT it_assy BY vtype mcode lifnr datab.
ENDFORM.                    " read_data
*&---------------------------------------------------------------------*
*&      Module  status  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status OUTPUT.
  CASE sy-dynnr.
    WHEN '9000'.
      SET PF-STATUS '9000'.
      SET TITLEBAR  '9000'.
    WHEN '9100'.
      SET PF-STATUS '9100'.
      SET TITLEBAR  '9100'.
    WHEN '9200'.
      SET PF-STATUS '9200'.
      SET TITLEBAR  '9200'.
    WHEN '9300'.
      SET PF-STATUS '9300'.
      SET TITLEBAR  '9300'.
  ENDCASE.
ENDMODULE.                 " status  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  display_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_9000 OUTPUT.
  READ TABLE it_assy INDEX tc_9000-current_line.
  CHECK sy-subrc EQ 0.
  CLEAR zsmm_assy_cost.
  MOVE-CORRESPONDING it_assy TO zsmm_assy_cost.
ENDMODULE.                 " display_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  CASE sy-ucomm.
    WHEN 'EXIT' OR 'CANC'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      CLEAR: sy-ucomm.
      LEAVE TO SCREEN 0.
    WHEN 'CREATE'.
      CLEAR: sy-ucomm.
      PERFORM create_rtn.
    WHEN 'CHANGE'.
      CLEAR: sy-ucomm.
      PERFORM change_rtn.
    WHEN 'DELETE'.
      CLEAR: sy-ucomm.
      PERFORM delete_rtn.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Form  CREATE_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_rtn.
  CLEAR: zsmm_assy_cost, it_assy.

  MOVE: sy-lilli TO wa_line.
  MOVE: sy-cpage TO wa_page.

  READ TABLE it_assy WITH KEY check = 'X'.
  IF sy-subrc EQ 0.
    MOVE: it_assy-vtype TO zsmm_assy_cost-vtype,
          it_assy-mcode TO zsmm_assy_cost-mcode,
          it_assy-lifnr TO zsmm_assy_cost-lifnr,
          it_assy-name1 TO zsmm_assy_cost-name1,
          it_assy-ekgrp TO zsmm_assy_cost-ekgrp.
  ENDIF.

  CALL SCREEN 9100 STARTING AT  20  1
                   ENDING   AT 120 19.

  PERFORM display_data.

  PERFORM move_current_page.

  sy-lsind = sy-lsind - 1.
ENDFORM.                    " CREATE_RTN
*&---------------------------------------------------------------------*
*&      Form  CHANGE_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_rtn.
  CLEAR: zsmm_assy_cost.

  MOVE: sy-lilli TO wa_line.
  MOVE: sy-cpage TO wa_page.

  READ TABLE it_assy WITH KEY it_assy.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m13.
  ENDIF.

  MOVE: it_assy TO zsmm_assy_cost.

  CALL SCREEN 9200 STARTING AT  20  1
                   ENDING   AT 120 19.

  PERFORM display_data.

  PERFORM move_current_page.

  sy-lsind = sy-lsind - 1.
ENDFORM.                    " CHANGE_RTN
*&---------------------------------------------------------------------*
*&      Form  DELETE_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_rtn.
  CLEAR: zsmm_assy_cost.

  MOVE: sy-lilli TO wa_line.
  MOVE: sy-cpage TO wa_page.

  READ TABLE it_assy WITH KEY it_assy.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m13.
  ENDIF.

  MOVE: it_assy TO zsmm_assy_cost.

  CALL SCREEN 9300 STARTING AT  20  1
                   ENDING   AT 120 19.

  PERFORM display_data.

  PERFORM move_current_page.

  sy-lsind = sy-lsind - 1.
ENDFORM.                    " DELETE_RTN
*&---------------------------------------------------------------------*
*&      Form  CHECK_COMPANY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_company.
  SELECT SINGLE * FROM t001 WHERE bukrs = c_bukrs.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m06.
  ENDIF.
ENDFORM.                    " CHECK_COMPANY
*&---------------------------------------------------------------------*
*&      Module  modify_it_assy  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_it_assy INPUT.
  READ TABLE it_assy INDEX tc_9000-current_line.
  IF sy-subrc EQ 0.
    MOVE: zsmm_assy_cost-check TO it_assy-check.
    MODIFY it_assy INDEX tc_9000-current_line.
  ENDIF.
ENDMODULE.                 " modify_it_assy  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_rtn  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_rtn INPUT.
  PERFORM check_vtype_9100.
  PERFORM check_lifnr_9100.
  PERFORM check_ekgrp_9100.
  PERFORM check_period_9100.
  PERFORM calculate_amount.
ENDMODULE.                 " check_rtn  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_9100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9100 INPUT.
  CASE sy-ucomm.
    WHEN 'SAVE'.
      CLEAR: sy-ucomm.
      PERFORM save_9100_rtn.
  ENDCASE.
ENDMODULE.                 " user_command_9100  INPUT
*&---------------------------------------------------------------------*
*&      Form  check_lifnr_9100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_lifnr_9100.
  SELECT SINGLE name1 INTO zsmm_assy_cost-name1
    FROM lfa1
   WHERE lifnr = zsmm_assy_cost-lifnr.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m07.
  ENDIF.
ENDFORM.                    " check_lifnr_9100
*&---------------------------------------------------------------------*
*&      Form  check_period_9100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_period_9100.
  IF zsmm_assy_cost-datab > zsmm_assy_cost-datbi.
    MESSAGE e000(zz) WITH text-m15.
  ENDIF.

  SELECT SINGLE *
    FROM ztmm_assy_cost1
   WHERE vtype =  zsmm_assy_cost-vtype
     AND mcode =  zsmm_assy_cost-mcode
     AND lifnr =  zsmm_assy_cost-lifnr
     AND datab <= zsmm_assy_cost-datab
     AND datbi >= zsmm_assy_cost-datab.
  IF sy-subrc EQ 0.
    MESSAGE e000(zz) WITH text-m12.
  ENDIF.

  SELECT SINGLE *
    FROM ztmm_assy_cost1
   WHERE vtype =  zsmm_assy_cost-vtype
     AND mcode =  zsmm_assy_cost-mcode
     AND lifnr =  zsmm_assy_cost-lifnr
     AND datab <= zsmm_assy_cost-datbi
     AND datbi >= zsmm_assy_cost-datbi.
  IF sy-subrc EQ 0.
    MESSAGE e000(zz) WITH text-m12.
  ENDIF.

  SELECT SINGLE *
    FROM ztmm_assy_cost1
   WHERE vtype =  zsmm_assy_cost-vtype
     AND mcode =  zsmm_assy_cost-mcode
     AND lifnr =  zsmm_assy_cost-lifnr
     AND datab >= zsmm_assy_cost-datab
     AND datbi <= zsmm_assy_cost-datbi.
  IF sy-subrc EQ 0.
    MESSAGE e000(zz) WITH text-m12.
  ENDIF.
ENDFORM.                    " check_period_9100
*&---------------------------------------------------------------------*
*&      Form  check_ekgrp_9100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_ekgrp_9100.
  SELECT SINGLE * FROM t024 WHERE ekgrp = zsmm_assy_cost-ekgrp.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m02.
  ENDIF.
ENDFORM.                    " check_ekgrp_9100
*&---------------------------------------------------------------------*
*&      Form  calculate_amount
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calculate_amount.
  zsmm_assy_cost-total = zsmm_assy_cost-asytr + zsmm_assy_cost-zp01 +
                         zsmm_assy_cost-zp02  + zsmm_assy_cost-zp03 +
                         zsmm_assy_cost-zp04  + zsmm_assy_cost-zp05 +
                         zsmm_assy_cost-zp06  + zsmm_assy_cost-zp07 +
                         zsmm_assy_cost-zp08  + zsmm_assy_cost-zp09 +
                         zsmm_assy_cost-zp10  + zsmm_assy_cost-zp11 +
                         zsmm_assy_cost-zp12  + zsmm_assy_cost-zp13 +
                         zsmm_assy_cost-zp14  + zsmm_assy_cost-zp15 +
                         zsmm_assy_cost-zp16.



ENDFORM.                    " calculate_amount
*&---------------------------------------------------------------------*
*&      Form  SAVE_9100_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_9100_rtn.
  PERFORM set_table_data_9100.
  PERFORM create_9100.
ENDFORM.                    " SAVE_9100_RTN
*&---------------------------------------------------------------------*
*&      Form  SET_TABLE_DATA_9100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_table_data_9100.
  FIELD-SYMBOLS: <cost>.
  DATA: lw_cost(50),
        lw_index(2) TYPE n.

  CLEAR: ztmm_assy_cost1, ztmm_assy_cost2.

  CLEAR: it_ztmm_assy_cost2, it_ztmm_assy_cost2[].

*----- Set ZTMM_assy_COST1
  SELECT SINGLE MAX( seqno ) INTO ztmm_assy_cost1-seqno
    FROM ztmm_assy_cost1
   WHERE vtype = zsmm_assy_cost-vtype
     AND mcode = zsmm_assy_cost-mcode
     AND lifnr = zsmm_assy_cost-lifnr.

  MOVE: zsmm_assy_cost-vtype TO ztmm_assy_cost1-vtype,
        zsmm_assy_cost-mcode TO ztmm_assy_cost1-mcode,
        zsmm_assy_cost-lifnr TO ztmm_assy_cost1-lifnr,
        zsmm_assy_cost-ekgrp TO ztmm_assy_cost1-ekgrp,
        zsmm_assy_cost-datab TO ztmm_assy_cost1-datab,
        zsmm_assy_cost-datbi TO ztmm_assy_cost1-datbi,
        zsmm_assy_cost-asytr TO ztmm_assy_cost1-asytr,
        sy-uname              TO ztmm_assy_cost1-ernam,
        sy-datum              TO ztmm_assy_cost1-erdat,
        sy-uzeit              TO ztmm_assy_cost1-erzet,
        sy-uname              TO ztmm_assy_cost1-aenam,
        sy-datum              TO ztmm_assy_cost1-aedat,
        sy-uzeit              TO ztmm_assy_cost1-aezet.

  ztmm_assy_cost1-seqno = ztmm_assy_cost1-seqno + 1.

*----- Set ZTMM_assy_COST2
  MOVE: zsmm_assy_cost-vtype  TO it_ztmm_assy_cost2-vtype,
        zsmm_assy_cost-mcode  TO it_ztmm_assy_cost2-mcode,
        zsmm_assy_cost-lifnr  TO it_ztmm_assy_cost2-lifnr,
        ztmm_assy_cost1-seqno TO it_ztmm_assy_cost2-seqno,
        sy-uname               TO it_ztmm_assy_cost2-ernam,
        sy-datum               TO it_ztmm_assy_cost2-erdat,
        sy-uzeit               TO it_ztmm_assy_cost2-erzet,
        sy-uname               TO it_ztmm_assy_cost2-aenam,
        sy-datum               TO it_ztmm_assy_cost2-aedat,
        sy-uzeit               TO it_ztmm_assy_cost2-aezet.

  DO.
    MOVE: sy-index TO lw_index.

    CONCATENATE 'ZSMM_assy_COST-ZP' lw_index INTO lw_cost.
    ASSIGN (lw_cost) TO <cost>.
    IF sy-subrc NE 0. EXIT. ENDIF.

    MOVE: <cost>                TO it_ztmm_assy_cost2-kbetr.
    CONCATENATE 'ZP' lw_index INTO it_ztmm_assy_cost2-kschl.
    APPEND it_ztmm_assy_cost2.
  ENDDO.
ENDFORM.                    " SET_TABLE_DATA_9100
*&---------------------------------------------------------------------*
*&      Form  CREATE_9100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_9100.
  INSERT ztmm_assy_cost1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m08.
  ENDIF.

  INSERT ztmm_assy_cost2 FROM TABLE it_ztmm_assy_cost2
         ACCEPTING DUPLICATE KEYS.
  IF sy-subrc NE 0.
    ROLLBACK WORK.
    MESSAGE e000(zz) WITH text-m08.
  ENDIF.

  COMMIT WORK AND WAIT.

  MOVE: zsmm_assy_cost        TO it_assy,
        ztmm_assy_cost1-seqno TO it_assy-seqno.
  APPEND it_assy.
  SORT it_assy BY vtype mcode lifnr datab.

  MESSAGE s000(zz) WITH text-m09.
  LEAVE TO SCREEN 0.
ENDFORM.                    " CREATE_9100
*&---------------------------------------------------------------------*
*&      Form  check_vtype_9100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_vtype_9100.
  TRANSLATE zsmm_assy_cost-vtype TO UPPER CASE.
ENDFORM.                    " check_vtype_9100

*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data.
  LOOP AT it_assy.
    NEW-LINE.
    RESERVE 3 LINES.

    PERFORM display_first_line.

    AT NEW mcode.
      PERFORM display_mcode.
    ENDAT.

    IF sy-linno EQ 7.
      PERFORM display_mcode.
    ENDIF.

    NEW-LINE.

    PERFORM display_second_line.

    AT NEW mcode.
      PERFORM display_lifnr.
    ENDAT.

    IF sy-linno EQ 8.
      PERFORM display_lifnr.
    ENDIF.

    ULINE.
  ENDLOOP.
  IF sy-subrc NE 0.
    ULINE.
  ENDIF.

  CLEAR: it_assy.
  SET USER-COMMAND 'PAGE'.
ENDFORM.                    " display_data
*&---------------------------------------------------------------------*
*&      Form  display_mcode
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_mcode.
  FORMAT COLOR COL_KEY INTENSIFIED OFF.

  WRITE: 02(03) it_assy-vtype,
           (37) it_assy-mcode NO-GAP,
                '|' NO-GAP.
ENDFORM.                    " display_mcode
*&---------------------------------------------------------------------*
*&      Form  display_lifnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_lifnr.
  FORMAT COLOR COL_KEY INTENSIFIED OFF.
  WRITE: 02 it_assy-lifnr,
            it_assy-name1 NO-GAP,
           '|'  NO-GAP.
ENDFORM.                    " display_lifnr
*&---------------------------------------------------------------------*
*&      Form  display_first_line
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_first_line.
  FORMAT COLOR COL_TOTAL INTENSIFIED OFF.

  WRITE: 01     '|' NO-GAP,
           (41) space COLOR COL_KEY INTENSIFIED OFF NO-GAP,
                '|' NO-GAP,
                it_assy-datab,
           (11) it_assy-datbi NO-GAP,
                '|' NO-GAP.

  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.

  WRITE:   (09) it_assy-asytr,
           (09) it_assy-zp01,
           (09) it_assy-zp02,
           (09) it_assy-zp03,
           (09) it_assy-zp04,
           (09) it_assy-zp05,
           (09) it_assy-zp06,
           (09) it_assy-zp07,
           (09) it_assy-zp08 NO-GAP,
                '|'.

  HIDE it_assy.
ENDFORM.                    " display_first_line
*&---------------------------------------------------------------------*
*&      Form  display_second_line
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_second_line.
  FORMAT COLOR COL_TOTAL INTENSIFIED OFF.

  WRITE: 01     '|' NO-GAP,
           (41) space COLOR COL_KEY INTENSIFIED OFF NO-GAP,
                '|' NO-GAP,
                it_assy-ekgrp,
           (06) t001-waers,
           (11) it_assy-total NO-GAP,
                '|' NO-GAP.

  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.

  WRITE:   (09) it_assy-zp09,
           (09) it_assy-zp10,
           (09) it_assy-zp11,
           (09) it_assy-zp12,
           (09) it_assy-zp13,
           (09) it_assy-zp14,
           (09) it_assy-zp15,
           (09) it_assy-zp16,
           (09) space NO-GAP,
                '|'.

  HIDE it_assy.
ENDFORM.                    " display_second_line
*&---------------------------------------------------------------------*
*&      Form  display_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_header.
  READ TABLE s_ekgrp INDEX 1.
  IF sy-subrc NE 0.
    CLEAR: t024.
  ENDIF.

  WRITE:/01(142) text-h01 CENTERED.

  SKIP.
  WRITE:/2   text-h02, (3) s_vtype-low, cawnt-atwtb,
         118 text-h03, sy-datum.
  WRITE:/2   text-h04, s_ekgrp-low, t024-eknam,
         118 text-h05, sy-uzeit.
  WRITE:/2   text-h06, (18) s_mcode-low, '~',  (18) s_mcode-high,
         118 text-h07, (4) sy-pagno NO-GAP,'/', (4) wa_tot_page NO-ZERO.

  FORMAT COLOR COL_HEADING INTENSIFIED OFF.

  ULINE.
  WRITE:/    text-h08 NO-GAP, text-h09.
  WRITE:/    text-h10 NO-GAP, text-h11.
  ULINE.

  MOVE: sy-pagno TO wa_tot_page.
ENDFORM.                    " display_header
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_TOTAL_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_total_page.
  DO wa_tot_page TIMES.
    READ LINE 5 OF PAGE sy-index.
    MODIFY LINE 5 OF PAGE sy-index
                     FIELD VALUE wa_tot_page FROM wa_tot_page.
    IF sy-subrc NE 0.
      EXIT.
    ENDIF.
  ENDDO.
ENDFORM.                    " DISPLAY_TOTAL_PAGE
*&---------------------------------------------------------------------*
*&      Form  move_current_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM move_current_page.
  DATA : lv_scroll_line TYPE i.

  CALL FUNCTION 'LIST_SCROLL_LINE_TOPMOST'
       EXPORTING
            list_index          = sy-lsind
            list_line           = wa_line  " SY-LILLI
            list_page           = wa_page
       IMPORTING
            scroll_line         = lv_scroll_line
       EXCEPTIONS
            list_index_invalid  = 1
            list_line_not_found = 2
            no_list_active      = 3
            window_too_small    = 4
            OTHERS              = 5.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m01.
  ENDIF.
ENDFORM.                    " move_current_page
*&---------------------------------------------------------------------*
*&      Module  check_rtn_9200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_rtn_9200 INPUT.
  PERFORM check_vtype_9100.
  PERFORM check_ekgrp_9100.
  PERFORM check_period_9200.
  PERFORM calculate_amount.
ENDMODULE.                 " check_rtn_9200  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_9200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9200 INPUT.
  CASE sy-ucomm.
    WHEN 'SAVE'.
      CLEAR: sy-ucomm.
      PERFORM save_9200_rtn.
  ENDCASE.
ENDMODULE.                 " user_command_9200  INPUT
*&---------------------------------------------------------------------*
*&      Form  save_9200_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_9200_rtn.
  PERFORM set_table_data_9200.
  PERFORM create_9200.
ENDFORM.                    " save_9200_rtn
*&---------------------------------------------------------------------*
*&      Form  set_table_data_9200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_table_data_9200.
  FIELD-SYMBOLS: <cost>.
  DATA: lw_cost(50),
        lw_index(2) TYPE n.

*----- Set ZTMM_ASY_COST1_B
  CLEAR: ztmm_assy_cost1, ztmm_asy_cost1_b.

  SELECT SINGLE *
    FROM ztmm_assy_cost1
   WHERE vtype = zsmm_assy_cost-vtype
     AND mcode = zsmm_assy_cost-mcode
     AND lifnr = zsmm_assy_cost-lifnr
     AND seqno = zsmm_assy_cost-seqno.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m01.
  ENDIF.

  MOVE-CORRESPONDING ztmm_assy_cost1 TO ztmm_asy_cost1_b.
  MOVE: sy-uname TO ztmm_asy_cost1_b-aenam,
        sy-datum TO ztmm_asy_cost1_b-aedat,
        sy-uzeit TO ztmm_asy_cost1_b-aezet.

  SELECT SINGLE MAX( hisno ) INTO ztmm_asy_cost1_b-hisno
    FROM ztmm_asy_cost1_b
   WHERE vtype = zsmm_assy_cost-vtype
     AND mcode = zsmm_assy_cost-mcode
     AND lifnr = zsmm_assy_cost-lifnr
     AND seqno = zsmm_assy_cost-seqno.

  ztmm_asy_cost1_b-hisno = ztmm_asy_cost1_b-hisno + 1.

*----- Set ZTMM_ASY_COST2_B
  CLEAR: it_ztmm_assy_cost2, it_ztmm_assy_cost2[],
         it_ztmm_asy_cost2_b, it_ztmm_asy_cost2_b[].

  SELECT * INTO TABLE it_ztmm_assy_cost2
    FROM ztmm_assy_cost2
   WHERE vtype = zsmm_assy_cost-vtype
     AND mcode = zsmm_assy_cost-mcode
     AND lifnr = zsmm_assy_cost-lifnr
     AND seqno = zsmm_assy_cost-seqno.

  LOOP AT it_ztmm_assy_cost2.
    MOVE-CORRESPONDING it_ztmm_assy_cost2 TO it_ztmm_asy_cost2_b.
    MOVE: ztmm_asy_cost1_b-hisno TO it_ztmm_asy_cost2_b-hisno,
          sy-uname TO it_ztmm_asy_cost2_b-aenam,
          sy-datum TO it_ztmm_asy_cost2_b-aedat,
          sy-uzeit TO it_ztmm_asy_cost2_b-aezet.

    APPEND it_ztmm_asy_cost2_b.
  ENDLOOP.

*----- Set ZTMM_assy_COST1
  CLEAR: ztmm_assy_cost1, ztmm_assy_cost2.

  SELECT SINGLE MAX( seqno ) INTO ztmm_assy_cost1-seqno
    FROM ztmm_assy_cost1
   WHERE vtype = zsmm_assy_cost-vtype
     AND mcode = zsmm_assy_cost-mcode
     AND lifnr = zsmm_assy_cost-lifnr.

  MOVE: zsmm_assy_cost-vtype TO ztmm_assy_cost1-vtype,
        zsmm_assy_cost-mcode TO ztmm_assy_cost1-mcode,
        zsmm_assy_cost-lifnr TO ztmm_assy_cost1-lifnr,
        zsmm_assy_cost-ekgrp TO ztmm_assy_cost1-ekgrp,
        zsmm_assy_cost-datab TO ztmm_assy_cost1-datab,
        zsmm_assy_cost-datbi TO ztmm_assy_cost1-datbi,
        zsmm_assy_cost-asytr TO ztmm_assy_cost1-asytr,
        sy-uname              TO ztmm_assy_cost1-ernam,
        sy-datum              TO ztmm_assy_cost1-erdat,
        sy-uzeit              TO ztmm_assy_cost1-erzet,
        sy-uname              TO ztmm_assy_cost1-aenam,
        sy-datum              TO ztmm_assy_cost1-aedat,
        sy-uzeit              TO ztmm_assy_cost1-aezet.

  ztmm_assy_cost1-seqno = ztmm_assy_cost1-seqno + 1.

*----- Set ZTMM_assy_COST2
  CLEAR: it_ztmm_assy_cost2, it_ztmm_assy_cost2[].

  MOVE: zsmm_assy_cost-vtype  TO it_ztmm_assy_cost2-vtype,
        zsmm_assy_cost-mcode  TO it_ztmm_assy_cost2-mcode,
        zsmm_assy_cost-lifnr  TO it_ztmm_assy_cost2-lifnr,
        ztmm_assy_cost1-seqno TO it_ztmm_assy_cost2-seqno,
        sy-uname               TO it_ztmm_assy_cost2-ernam,
        sy-datum               TO it_ztmm_assy_cost2-erdat,
        sy-uzeit               TO it_ztmm_assy_cost2-erzet,
        sy-uname               TO it_ztmm_assy_cost2-aenam,
        sy-datum               TO it_ztmm_assy_cost2-aedat,
        sy-uzeit               TO it_ztmm_assy_cost2-aezet.

  DO.
    MOVE: sy-index TO lw_index.

    CONCATENATE 'ZSMM_assy_COST-ZP' lw_index INTO lw_cost.
    ASSIGN (lw_cost) TO <cost>.
    IF sy-subrc NE 0. EXIT. ENDIF.

    MOVE: <cost>                TO it_ztmm_assy_cost2-kbetr.
    CONCATENATE 'ZP' lw_index INTO it_ztmm_assy_cost2-kschl.
    APPEND it_ztmm_assy_cost2.
  ENDDO.
ENDFORM.                    " set_table_data_9200
*&---------------------------------------------------------------------*
*&      Form  create_9200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_9200.
  INSERT ztmm_assy_cost1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m08.
  ENDIF.

  DELETE FROM ztmm_assy_cost1 WHERE vtype = zsmm_assy_cost-vtype
                                 AND mcode = zsmm_assy_cost-mcode
                                 AND lifnr = zsmm_assy_cost-lifnr
                                 AND seqno = zsmm_assy_cost-seqno.

  INSERT ztmm_asy_cost1_b.
  IF sy-subrc NE 0.
    ROLLBACK WORK.
    MESSAGE e000(zz) WITH text-m08.
  ENDIF.

  INSERT ztmm_assy_cost2 FROM TABLE it_ztmm_assy_cost2
         ACCEPTING DUPLICATE KEYS.
  IF sy-subrc NE 0.
    ROLLBACK WORK.
    MESSAGE e000(zz) WITH text-m08.
  ENDIF.

  DELETE FROM ztmm_assy_cost2 WHERE vtype = zsmm_assy_cost-vtype
                                 AND mcode = zsmm_assy_cost-mcode
                                 AND lifnr = zsmm_assy_cost-lifnr
                                 AND seqno = zsmm_assy_cost-seqno.

  INSERT ztmm_asy_cost2_b FROM TABLE it_ztmm_asy_cost2_b
         ACCEPTING DUPLICATE KEYS.
  IF sy-subrc NE 0.
    ROLLBACK WORK.
    MESSAGE e000(zz) WITH text-m08.
  ENDIF.

  COMMIT WORK AND WAIT.

  PERFORM set_it_assy.

  MESSAGE s000(zz) WITH text-m09.
  LEAVE TO SCREEN 0.
ENDFORM.                    " create_9200
*&---------------------------------------------------------------------*
*&      Form  CHECK_PERIOD_9200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_period_9200.
  DATA: lt_assy LIKE it_assy OCCURS 0 WITH HEADER LINE.

  IF zsmm_assy_cost-datab > zsmm_assy_cost-datbi.
    MESSAGE e000(zz) WITH text-m15.
  ENDIF.

  lt_assy[] = it_assy[].

  DELETE lt_assy WHERE vtype = zsmm_assy_cost-vtype
                    AND mcode = zsmm_assy_cost-mcode
                    AND lifnr = zsmm_assy_cost-lifnr
                    AND seqno = zsmm_assy_cost-seqno.

  LOOP AT it_assy WHERE vtype =  zsmm_assy_cost-vtype
                    AND mcode =  zsmm_assy_cost-mcode
                    AND lifnr =  zsmm_assy_cost-lifnr.
  ENDLOOP.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m16.
  ENDIF.

  LOOP AT lt_assy WHERE datab <= zsmm_assy_cost-datab
                    AND datbi >= zsmm_assy_cost-datab
                    AND vtype =  zsmm_assy_cost-vtype
                    AND mcode =  zsmm_assy_cost-mcode
                    AND lifnr =  zsmm_assy_cost-lifnr.
  ENDLOOP.
  IF sy-subrc EQ 0.
    MESSAGE e000(zz) WITH text-m12.
  ENDIF.

  LOOP AT lt_assy WHERE datab <= zsmm_assy_cost-datbi
                    AND datbi >= zsmm_assy_cost-datbi
                    AND vtype =  zsmm_assy_cost-vtype
                    AND mcode =  zsmm_assy_cost-mcode
                    AND lifnr =  zsmm_assy_cost-lifnr.
  ENDLOOP.
  IF sy-subrc EQ 0.
    MESSAGE e000(zz) WITH text-m12.
  ENDIF.
ENDFORM.                    " CHECK_PERIOD_9200
*&---------------------------------------------------------------------*
*&      Form  set_it_assy
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_it_assy.
  READ TABLE it_assy WITH KEY vtype = zsmm_assy_cost-vtype
                               mcode = zsmm_assy_cost-mcode
                               lifnr = zsmm_assy_cost-lifnr
                               seqno = zsmm_assy_cost-seqno.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m01.
  ENDIF.

  DELETE it_assy INDEX sy-tabix.

  MOVE-CORRESPONDING ztmm_assy_cost1 TO zsmm_assy_cost.

  MOVE zsmm_assy_cost TO it_assy.
  APPEND it_assy.

  SORT it_assy BY vtype mcode lifnr datab.
ENDFORM.                    " set_it_assy
*&---------------------------------------------------------------------*
*&      Module  user_command_9300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9300 INPUT.
  CASE sy-ucomm.
    WHEN 'DELETE'.
      CLEAR: sy-ucomm.
      PERFORM delete_9300_rtn.
  ENDCASE.
ENDMODULE.                 " user_command_9300  INPUT
*&---------------------------------------------------------------------*
*&      Form  DELETE_9200_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_9300_rtn.
  DATA: lw_answer.

  CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_MESSAGE'
       EXPORTING
            defaultoption  = 'N'
            diagnosetext1  = text-b01
            textline1      = text-b02
            titel          = text-h01
            start_column   = 25
            start_row      = 6
            cancel_display = 'X'
       IMPORTING
            answer         = lw_answer.

  CHECK lw_answer = 'J'.

  PERFORM set_table_data_9300.
  PERFORM create_table_9300.
ENDFORM.                    " DELETE_9200_rtn
*&---------------------------------------------------------------------*
*&      Form  set_table_data_9300
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_table_data_9300.
*----- Set ZTMM_ASY_COST1_B
  CLEAR: ztmm_assy_cost1, ztmm_asy_cost1_b.

  SELECT SINGLE *
    FROM ztmm_assy_cost1
   WHERE vtype = zsmm_assy_cost-vtype
     AND mcode = zsmm_assy_cost-mcode
     AND lifnr = zsmm_assy_cost-lifnr
     AND seqno = zsmm_assy_cost-seqno.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m01.
  ENDIF.

  MOVE-CORRESPONDING ztmm_assy_cost1 TO ztmm_asy_cost1_b.
  MOVE: sy-uname TO ztmm_asy_cost1_b-aenam,
        sy-datum TO ztmm_asy_cost1_b-aedat,
        sy-uzeit TO ztmm_asy_cost1_b-aezet.

  SELECT SINGLE MAX( hisno )
    INTO ztmm_asy_cost1_b-hisno
    FROM ztmm_asy_cost1_b
   WHERE vtype = zsmm_assy_cost-vtype
     AND mcode = zsmm_assy_cost-mcode
     AND lifnr = zsmm_assy_cost-lifnr
     AND seqno = zsmm_assy_cost-seqno.

  ztmm_asy_cost1_b-hisno = ztmm_asy_cost1_b-hisno + 1.

*----- Set ZTMM_ASY_COST2_B
  CLEAR: it_ztmm_assy_cost2, it_ztmm_assy_cost2[],
         it_ztmm_asy_cost2_b, it_ztmm_asy_cost2_b[].

  SELECT * INTO TABLE it_ztmm_assy_cost2
    FROM ztmm_assy_cost2
   WHERE vtype = zsmm_assy_cost-vtype
     AND mcode = zsmm_assy_cost-mcode
     AND lifnr = zsmm_assy_cost-lifnr
     AND seqno = zsmm_assy_cost-seqno.

  LOOP AT it_ztmm_assy_cost2.
    MOVE-CORRESPONDING it_ztmm_assy_cost2 TO it_ztmm_asy_cost2_b.
    MOVE: ztmm_asy_cost1_b-hisno TO it_ztmm_asy_cost2_b-hisno,
          sy-uname TO it_ztmm_asy_cost2_b-aenam,
          sy-datum TO it_ztmm_asy_cost2_b-aedat,
          sy-uzeit TO it_ztmm_asy_cost2_b-aezet.

    APPEND it_ztmm_asy_cost2_b.
  ENDLOOP.
ENDFORM.                    " set_table_data_9300
*&---------------------------------------------------------------------*
*&      Form  create_table_9300
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_table_9300.
  DELETE FROM ztmm_assy_cost1 WHERE vtype = zsmm_assy_cost-vtype
                                 AND mcode = zsmm_assy_cost-mcode
                                 AND lifnr = zsmm_assy_cost-lifnr
                                 AND seqno = zsmm_assy_cost-seqno.

  INSERT ztmm_asy_cost1_b.
  IF sy-subrc NE 0.
    ROLLBACK WORK.
    MESSAGE e000(zz) WITH text-m08.
  ENDIF.

  DELETE FROM ztmm_assy_cost2 WHERE vtype = zsmm_assy_cost-vtype
                                 AND mcode = zsmm_assy_cost-mcode
                                 AND lifnr = zsmm_assy_cost-lifnr
                                 AND seqno = zsmm_assy_cost-seqno.

  INSERT ztmm_asy_cost2_b FROM TABLE it_ztmm_asy_cost2_b
         ACCEPTING DUPLICATE KEYS.
  IF sy-subrc NE 0.
    ROLLBACK WORK.
    MESSAGE e000(zz) WITH text-m08.
  ENDIF.

  COMMIT WORK AND WAIT.

  READ TABLE it_assy WITH KEY vtype = zsmm_assy_cost-vtype
                               mcode = zsmm_assy_cost-mcode
                               lifnr = zsmm_assy_cost-lifnr
                               seqno = zsmm_assy_cost-seqno.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m01.
  ENDIF.

  DELETE it_assy INDEX sy-tabix.

  MESSAGE s000(zz) WITH text-m14.
  LEAVE TO SCREEN 0.
ENDFORM.                    " create_table_9300
*&---------------------------------------------------------------------*
*&      Form  download_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM download_rtn.
  CALL FUNCTION 'DOWNLOAD'
       EXPORTING
            filename                = c_filename
            filetype                = 'DAT'
       TABLES
            data_tab                = it_assy
       EXCEPTIONS
            invalid_filesize        = 1
            invalid_table_width     = 2
            invalid_type            = 3
            no_batch                = 4
            unknown_error           = 5
            gui_refuse_filetransfer = 6
            customer_error          = 7
            OTHERS                  = 8.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " download_rtn
*&---------------------------------------------------------------------*
*&      Form  check_vtype
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_vtype.
  LOOP AT s_vtype.
    TRANSLATE s_vtype-low  TO UPPER CASE.
    TRANSLATE s_vtype-high TO UPPER CASE.
    MODIFY s_vtype.
  ENDLOOP.
ENDFORM.                    " check_vtype
