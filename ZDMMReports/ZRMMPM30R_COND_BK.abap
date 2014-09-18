************************************************************************
* Program Name      : ZRMMPM30R_COND
* Author            : Sung-Tae, Lim
* Creation Date     : 2004.02.23.
* Specifications By : Sung-Tae, Lim
* Pattern           : Report 1-1
* Development Request No : UD1K907463
* Addl Documentation:
* Description       : Condition Status by Material/Vendor
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2004.02.23.     Sung-Tae Lim     UD1K907463     Initial Coding
*
************************************************************************

REPORT zrmmpm30r_cond NO STANDARD PAGE HEADING
                     LINE-SIZE 500
                     LINE-COUNT 64(1)
                     MESSAGE-ID zmmm.

**---
INCLUDE : zrmmpmxxr_incl.


**--- Types
TYPES : z_amnt LIKE konp-kbetr,
        z_qnty LIKE ztmm_cond-menge.

**--- Internal Tables
DATA : BEGIN OF it_itab OCCURS 0,
         matnr LIKE mara-matnr,
         maktx LIKE makt-maktx,
         lifnr LIKE lfa1-lifnr,
         name1 LIKE lfa1-name1,
         meins LIKE ztmm_cond-meins,
         waers LIKE eine-waers,
*--- total purchasing amount per vehicle
         tot_u TYPE z_amnt,     " unit price
         tot_q TYPE z_qnty,     " quantity
         tot_a TYPE z_amnt,     " amount
*--- packaging amortization cost - zp01
         zp01a TYPE z_amnt,     " price
         zp01b TYPE z_amnt,     " amount
         zp01c TYPE z_qnty,     " depreciation quantity
         zp01d TYPE z_qnty,     " amortization quantity
*--- tooling amortization cost - zp02
         zp02a TYPE z_amnt,     " price
         zp02b TYPE z_amnt,     " amount
         zp02c TYPE z_qnty,     " depreciation quantity
         zp02d TYPE z_qnty,     " amortization quantity
*--- development amortizatino cost - zp03
         zp03a TYPE z_amnt,     " price
         zp03b TYPE z_amnt,     " amount
         zp03c TYPE z_qnty,     " depreciation quantity
         zp03d TYPE z_qnty,     " amortization quantity
*--- raw materials cost - zp04
         zp04a TYPE z_amnt,     " price
         zp04b TYPE z_amnt,     " amount
*--- freight cost - zp05
         zp05a TYPE z_amnt,     " price
         zp05b TYPE z_amnt,     " amount
*--- CC cost - zp06
         zp06a TYPE z_amnt,     " price
         zp06b TYPE z_amnt,     " amount
*--- sequencing cost - zp07
         zp07a TYPE z_amnt,     " price
         zp07b TYPE z_amnt,     " amount
*--- direct labor cost - zp08
         zp08a TYPE z_amnt,     " price
         zp08b TYPE z_amnt,     " amount
*--- machine cost - zp09
         zp09a TYPE z_amnt,     " price
         zp09b TYPE z_amnt,     " amount
*--- MBE cost - zp10
         zp10a TYPE z_amnt,     " price
         zp10b TYPE z_amnt,     " amount
*--- WBE cost - zp11
         zp11a TYPE z_amnt,     " price
         zp11b TYPE z_amnt,     " amount
*--- NAFTA cost - zp12
         zp12a TYPE z_amnt,     " price
         zp12b TYPE z_amnt,     " amount
*--- AALA cost - zp13
         zp13a TYPE z_amnt,     " price
         zp13b TYPE z_amnt,     " amount
       END OF it_itab.

DATA : it_stpox_alv LIKE stpox_alv OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_temp OCCURS 0,
         matnr LIKE mara-matnr,
         menge LIKE it_stpox_alv-menge,
         meins LIKE it_stpox_alv-meins,
*         konwa LIKE konp-konwa,
       END OF it_temp.

DATA : BEGIN OF it_matnr OCCURS 0,
         matnr LIKE mara-matnr,
       END OF it_matnr.

DATA : BEGIN OF it_bom_where_used OCCURS 0,
         matnr LIKE mara-matnr,
       END OF it_bom_where_used.

DATA : it_mara LIKE mara OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_ibsymbol OCCURS 0,
         atwrt TYPE ibsymbol-atwrt,
         atnam TYPE cabn-atnam,
       END OF it_ibsymbol.

DATA : BEGIN OF it_cond OCCURS 0,
         kschl LIKE konp-kschl,
         kbetr LIKE konp-kbetr,
         konwa LIKE konp-konwa,
       END OF it_cond.

DATA : BEGIN OF it_makt OCCURS 0,
         matnr LIKE mara-matnr,
         maktx LIKE makt-maktx,
         meins LIKE mara-meins,
       END OF it_makt.

DATA : BEGIN OF it_info OCCURS 0,
         matnr LIKE mara-matnr,
       END OF it_info.

DATA : it_ztmm_cond LIKE ztmm_cond OCCURS 0 WITH HEADER LINE.

*--- BOM
DATA : it_wultb LIKE stpov OCCURS 0 WITH HEADER LINE,
       it_matcat LIKE cscmat OCCURS 0 WITH HEADER LINE,
       it_matcat_tot LIKE it_matcat OCCURS 0 WITH HEADER LINE.

DATA : it_equicat LIKE cscequi OCCURS 0 WITH HEADER LINE,
       it_kndcat  LIKE cscknd OCCURS 0 WITH HEADER LINE,
       it_stdcat  LIKE cscstd OCCURS 0 WITH HEADER LINE,
       it_tplcat  LIKE csctpl OCCURS 0 WITH HEADER LINE.

DATA : st_topmat LIKE mc29s.


**--- Ranges
RANGES : r_kschl FOR konp-kschl.

**--- Constants
CONSTANTS : c_werks LIKE mast-werks VALUE 'P001',
            c_stlan LIKE mast-stlan VALUE '1',
            c_kappl LIKE a018-kappl VALUE 'M',
            c_pb00  LIKE a018-kschl VALUE 'PB00',
            c_ekorg LIKE a018-ekorg VALUE 'PU01'.
**---
DATA : w_atwrt(3),     "  LIKE ibsymbol-atwrt,
       w_atwri(3).     "  LIKE ibsymbol-atwrt.

DATA : w_konwa LIKE konp-konwa.


**--- Macro
DEFINE append_fieldcat.
  &1 = &1 + 1.
  w_fieldcat-col_pos    = &1.
  w_fieldcat-fieldname  = &2.
  w_fieldcat-outputlen  = &3.
  w_fieldcat-seltext_l  = &4.
  w_fieldcat-seltext_m  = &4.
  w_fieldcat-seltext_s  = &4.
  w_fieldcat-datatype   = &5.
  w_fieldcat-key        = &6.
*  w_fieldcat-do_sum     = &6.
  w_fieldcat-qfieldname = &7.
  w_fieldcat-cfieldname = &8.
  w_fieldcat-no_out     = &9.
  append w_fieldcat.
  clear : w_fieldcat.
END-OF-DEFINITION.

DEFINE append_top.
  clear : w_line.
  if not &3 is initial or not &4 is initial.
    w_line-typ   = &1.
    w_line-key   = &2.
    concatenate &3 '~' &4 into w_line-info separated by space.
    append w_line to w_top_of_page.
  endif.
END-OF-DEFINITION.

DEFINE append_sortcat.
  w_sortcat-spos      = &1.
  w_sortcat-fieldname = &2.
  w_sortcat-tabname   = &3.
  w_sortcat-up        = &4.
  w_sortcat-subtot    = &5.
  append w_sortcat.
  clear : w_sortcat.
END-OF-DEFINITION.


**---
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
*SELECTION-SCREEN BEGIN OF LINE.
*PARAMETERS : p_matnr RADIOBUTTON GROUP
*  gr1 DEFAULT 'X' USER-COMMAND rd1.
*SELECTION-SCREEN COMMENT (35) text-002 FOR FIELD p_matnr.
*PARAMETERS : p_lifnr RADIOBUTTON GROUP gr1.
*SELECTION-SCREEN COMMENT (35) text-003 FOR FIELD p_lifnr.
*SELECTION-SCREEN END OF LINE.
*
*SELECTION-SCREEN ULINE.

SELECT-OPTIONS : s_matnr FOR mara-matnr.

PARAMETERS :     p_vendr LIKE lfa1-lifnr.     "OBLIGATORY.

PARAMETERS : p_datbi LIKE konh-datbi OBLIGATORY DEFAULT sy-datum,
             p_vehic(2).     " OBLIGATORY.     " DEFAULT 'NF'.

SELECTION-SCREEN FUNCTION KEY 1.

SELECTION-SCREEN END OF BLOCK block1.


**---
AT SELECTION-SCREEN.
  PERFORM bom_explosion.
  PERFORM check_input_data.


**---
AT SELECTION-SCREEN OUTPUT.
*  PERFORM screen_control.


**---
INITIALIZATION.
  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      name                        = 'ICON_TREE'
*     text                        = 'FSC Explosion'
*     INFO                        = ' '
*     ADD_STDINF                  = 'X'
    IMPORTING
      result                      = sscrfields-functxt_01
    EXCEPTIONS
      icon_not_found              = 1
      outputfield_too_short       = 2
      OTHERS                      = 3.

  CONCATENATE sscrfields-functxt_01 'FSC Explosion'
              INTO sscrfields-functxt_01 SEPARATED BY space.

  PERFORM event_build USING w_eventcat[].


**---
TOP-OF-PAGE.
*  PERFORM write_top.
*  PERFORM top_of_page.


**---
START-OF-SELECTION.
  PERFORM get_data.


**---
END-OF-SELECTION.
  IF it_itab[] IS INITIAL.
    MESSAGE s999 WITH text-m01.
  ELSE.
    PERFORM write_top.
    PERFORM write_data.
*    PERFORM comment_build.     " USING w_top_of_page[].
*    PERFORM make_alv_grid.
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
  CLEAR : it_itab, it_itab[], it_temp, it_temp[], it_matnr, it_matnr[].

*  IF p_matnr NE space.
  PERFORM process_by_material.
*    PERFORM read_by_material.
*  ELSEIF p_lifnr NE space.
*    PERFORM process_by_vendor.
*  ENDIF.
ENDFORM.                    " get_data

*&---------------------------------------------------------------------*
*&      Form  comment_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM comment_build.

ENDFORM.                    " comment_build

*&---------------------------------------------------------------------*
*&      Form  make_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_alv_grid.

ENDFORM.                    " make_alv_grid

*&---------------------------------------------------------------------*
*&      Form  screen_control
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM screen_control.
**---
*  LOOP AT SCREEN.
*    IF p_matnr NE space AND screen-name CS 'P_VENDR'.
**      screen-input = 0.
**      screen-invisible = 1.
*    ELSEIF p_lifnr NE space AND screen-name CS 'S_MATNR'.
*      screen-input = 0.
**      screen-invisible = 1.
*    ENDIF.
*    MODIFY SCREEN.
*  ENDLOOP.
ENDFORM.                    " screen_control

*&---------------------------------------------------------------------*
*&      Form  read_by_material
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_by_material.
*---
  SELECT matnr INTO CORRESPONDING FIELDS OF TABLE it_matnr
               FROM mara
              WHERE matnr IN s_matnr
                AND mtart EQ 'ROH'
                AND lvorm EQ space.

*---
  CLEAR : it_matcat_tot, it_matcat_tot[].

  LOOP AT it_matnr.
    CLEAR : it_matcat, it_matcat[], it_wultb, it_wultb[], st_topmat,
            it_equicat, it_equicat[], it_kndcat, it_kndcat[],
            it_stdcat, it_stdcat[], it_tplcat, it_tplcat[].
    PERFORM bom_where_used USING it_matnr-matnr.
    APPEND LINES OF it_matcat TO it_matcat_tot.
  ENDLOOP.

*---
  LOOP AT it_matcat_tot.
    PERFORM bom_where_used USING it_matcat_tot-matnr.
    IF sy-subrc EQ 0.
      APPEND LINES OF it_matcat TO it_matcat_tot.
    ELSE.
      CHECK it_matcat_tot-matnr+6(2) EQ p_vehic.
      MOVE : it_matcat_tot-matnr TO it_bom_where_used-matnr.
      COLLECT it_bom_where_used.
      CLEAR : it_bom_where_used.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " read_by_material

*&---------------------------------------------------------------------*
*&      Form  check_input_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_input_data.
*---
  IF p_vendr EQ space.
    SET CURSOR FIELD 'P_VENDR'.
    MESSAGE e999 WITH text-m04.
  ENDIF.

*---
  IF p_vehic EQ space.
    SET CURSOR FIELD 'P_VEHIC'.
    MESSAGE e999 WITH text-m04.
  ENDIF.

*---
  DATA : l_digit(2) TYPE n.

  CLEAR : l_digit.

  l_digit = strlen( p_vehic ).

  IF l_digit NE 2.
    SET CURSOR FIELD p_vehic.
    MESSAGE e999 WITH text-m02.
  ENDIF.
ENDFORM.                    " check_input_data

*&---------------------------------------------------------------------*
*&      Form  bom_where_used
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bom_where_used USING p_matnr.
*---
  CALL FUNCTION 'CS_WHERE_USED_MAT'
       EXPORTING
            datub                      = sy-datum
            datuv                      = sy-datum
            matnr                      = p_matnr
            stlan                      = '1'
            werks                      = 'P001'
       IMPORTING
            topmat                     = st_topmat
       TABLES
            wultb                      = it_wultb
            matcat                     = it_matcat
            equicat                    = it_equicat
            kndcat                     = it_kndcat
            stdcat                     = it_stdcat
            tplcat                     = it_tplcat
       EXCEPTIONS
            call_invalid               = 1
            material_not_found         = 2
            no_where_used_rec_found    = 3
            no_where_used_rec_selected = 4
            no_where_used_rec_valid    = 5
            OTHERS                     = 6.
ENDFORM.                    " bom_where_used

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
  DATA : l_int(1).

  LOOP AT it_itab.
    PERFORM change_color CHANGING l_int.
    WRITE : /
        '|' NO-GAP,  (18) it_itab-matnr NO-GAP COLOR COL_HEADING,
        '|' NO-GAP,  (30) it_itab-maktx NO-GAP COLOR COL_HEADING,
*        '|' NO-GAP,  (10) it_itab-lifnr NO-GAP,
*        '|' NO-GAP,  (35) it_itab-name1 NO-GAP,
*--- total purchasing amount per vehicle
        '|' NO-GAP,  (10) it_itab-tot_u CURRENCY it_itab-waers NO-GAP,
        '|' NO-GAP,  (10) it_itab-tot_q UNIT it_itab-meins NO-GAP,
        '|' NO-GAP,  (10) it_itab-tot_a CURRENCY it_itab-waers NO-GAP,
*--- packaging amortization cost - zp01
        '|' NO-GAP,  (10) it_itab-zp01a CURRENCY it_itab-waers NO-GAP,
        '|' NO-GAP,  (10) it_itab-zp01b CURRENCY it_itab-waers NO-GAP,
        '|' NO-GAP,  (10) it_itab-zp01c UNIT it_itab-meins NO-GAP,
        '|' NO-GAP,  (10) it_itab-zp01d UNIT it_itab-meins NO-GAP,
*--- tooling amortization cost - zp02
        '|' NO-GAP,  (10) it_itab-zp02a CURRENCY it_itab-waers NO-GAP,
        '|' NO-GAP,  (10) it_itab-zp02b CURRENCY it_itab-waers NO-GAP,
        '|' NO-GAP,  (10) it_itab-zp02c UNIT it_itab-meins NO-GAP,
        '|' NO-GAP,  (10) it_itab-zp02d UNIT it_itab-meins NO-GAP,
*--- development amortizatino cost - zp03
        '|' NO-GAP,  (10) it_itab-zp03a CURRENCY it_itab-waers NO-GAP,
        '|' NO-GAP,  (10) it_itab-zp03b CURRENCY it_itab-waers NO-GAP,
        '|' NO-GAP,  (10) it_itab-zp03c UNIT it_itab-meins NO-GAP,
        '|' NO-GAP,  (10) it_itab-zp03d UNIT it_itab-meins NO-GAP,
*--- raw materials cost - zp04
        '|' NO-GAP,  (10) it_itab-zp04a CURRENCY it_itab-waers NO-GAP,
        '|' NO-GAP,  (10) it_itab-zp04b CURRENCY it_itab-waers NO-GAP,
*--- freight cost - zp05
        '|' NO-GAP,  (10) it_itab-zp05a CURRENCY it_itab-waers NO-GAP,
        '|' NO-GAP,  (10) it_itab-zp05b CURRENCY it_itab-waers NO-GAP,
*--- CC cost - zp06
        '|' NO-GAP,  (10) it_itab-zp06a CURRENCY it_itab-waers NO-GAP,
        '|' NO-GAP,  (10) it_itab-zp06b CURRENCY it_itab-waers NO-GAP,
*--- sequencing cost - zp07
        '|' NO-GAP,  (10) it_itab-zp07a CURRENCY it_itab-waers NO-GAP,
        '|' NO-GAP,  (10) it_itab-zp07b CURRENCY it_itab-waers NO-GAP,
*--- direct labor cost - zp08
        '|' NO-GAP,  (10) it_itab-zp08a CURRENCY it_itab-waers NO-GAP,
        '|' NO-GAP,  (10) it_itab-zp08b CURRENCY it_itab-waers NO-GAP,
*--- machine cost - zp09
        '|' NO-GAP,  (10) it_itab-zp09a CURRENCY it_itab-waers NO-GAP,
        '|' NO-GAP,  (10) it_itab-zp09b CURRENCY it_itab-waers NO-GAP,
*--- MBE cost - zp10
        '|' NO-GAP,  (10) it_itab-zp10a CURRENCY it_itab-waers NO-GAP,
        '|' NO-GAP,  (10) it_itab-zp10b CURRENCY it_itab-waers NO-GAP,
*--- WBE cost - zp11
        '|' NO-GAP,  (10) it_itab-zp11a CURRENCY it_itab-waers NO-GAP,
        '|' NO-GAP,  (10) it_itab-zp11b CURRENCY it_itab-waers NO-GAP,
*--- NAFTA cost - zp12
        '|' NO-GAP,  (10) it_itab-zp12a CURRENCY it_itab-waers NO-GAP,
        '|' NO-GAP,  (10) it_itab-zp12b CURRENCY it_itab-waers NO-GAP,
*--- AALA cost - zp13
        '|' NO-GAP,  (10) it_itab-zp13a CURRENCY it_itab-waers NO-GAP,
        '|' NO-GAP,  (10) it_itab-zp13b CURRENCY it_itab-waers NO-GAP,
        '|' NO-GAP.
  ENDLOOP.

  ULINE.
ENDFORM.                    " write_data

*&---------------------------------------------------------------------*
*&      Form  write_top
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_top.
*---
  WRITE : / text-101,
          / text-102.

  SKIP.

  PERFORM get_vendor_desc USING p_vendr.

  WRITE : / text-119, p_vendr, lfa1-name1.

  SKIP.

  FORMAT COLOR COL_HEADING.     " INTENSIFIED OFF.

  ULINE.

  PERFORM write_first_line_01.
  PERFORM write_second_line_01.
  PERFORM write_third_line_01.

  ULINE.
ENDFORM.                    " write_top

*&---------------------------------------------------------------------*
*&      Form  write_first_line_01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_first_line_01.
*---
*  WRITE : /     '|' NO-GAP,
*             21 '|' NO-GAP,
*             52 '|' NO-GAP,
*             63 '|' NO-GAP,
*             99 '|' NO-GAP, (32) text-103 NO-GAP CENTERED,
*            132 '|' NO-GAP, (43) text-104 NO-GAP CENTERED,
*            176 '|' NO-GAP, (43) text-105 NO-GAP CENTERED,
*            220 '|' NO-GAP, (43) text-106 NO-GAP CENTERED,
*           264 '|' NO-GAP, (21) text-107 NO-GAP CENTERED, " Raw Materia
*            286 '|' NO-GAP, (21) text-108 NO-GAP CENTERED, " Freight
*            308 '|' NO-GAP, (21) text-109 NO-GAP CENTERED, " CC Cost
*            330 '|' NO-GAP, (21) text-110 NO-GAP CENTERED, " Sequencing
*           352 '|' NO-GAP, (21) text-111 NO-GAP CENTERED, " Direct Labo
*            374 '|' NO-GAP, (21) text-112 NO-GAP CENTERED, " Machine
*            396 '|' NO-GAP, (21) text-113 NO-GAP CENTERED, " MBE Cost
*            418 '|' NO-GAP, (21) text-114 NO-GAP CENTERED, " WBE Cost
*            440 '|' NO-GAP, (21) text-115 NO-GAP CENTERED, " NAFTA Cost
*            462 '|' NO-GAP, (21) text-116 NO-GAP CENTERED, " AALA Cost
*            484 '|' NO-GAP.

  WRITE : /     '|' NO-GAP, (18) text-s18 NO-GAP CENTERED,
                '|' NO-GAP, (30) text-s30 NO-GAP CENTERED,
*                '|' NO-GAP, (10) text-s10 NO-GAP CENTERED,
*                '|' NO-GAP, (35) text-s35 NO-GAP CENTERED,
                '|' NO-GAP, (32) text-103 NO-GAP CENTERED,
                '|' NO-GAP, (43) text-104 NO-GAP CENTERED,
                '|' NO-GAP, (43) text-105 NO-GAP CENTERED,
                '|' NO-GAP, (43) text-106 NO-GAP CENTERED,
                '|' NO-GAP, (21) text-107 NO-GAP CENTERED, " Raw Materia
                '|' NO-GAP, (21) text-108 NO-GAP CENTERED, " Freight
                '|' NO-GAP, (21) text-109 NO-GAP CENTERED, " CC Cost
                '|' NO-GAP, (21) text-110 NO-GAP CENTERED, " Sequencing
                '|' NO-GAP, (21) text-111 NO-GAP CENTERED, " Direct Labo
                '|' NO-GAP, (21) text-112 NO-GAP CENTERED, " Machine
                '|' NO-GAP, (21) text-113 NO-GAP CENTERED, " MBE Cost
                '|' NO-GAP, (21) text-114 NO-GAP CENTERED, " WBE Cost
                '|' NO-GAP, (21) text-115 NO-GAP CENTERED, " NAFTA Cost
                '|' NO-GAP, (21) text-116 NO-GAP CENTERED, " AALA Cost
                '|' NO-GAP.
ENDFORM.                    " write_first_line_01

*&---------------------------------------------------------------------*
*&      Form  write_third_line_01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_third_line_01.
*---
  WRITE : /     '|' NO-GAP, (18) text-s18 NO-GAP CENTERED,
                '|' NO-GAP, (30) text-s30 NO-GAP CENTERED,
*                '|' NO-GAP, (10) text-s10 NO-GAP CENTERED,
*                '|' NO-GAP, (35) text-s35 NO-GAP CENTERED,

                '|' NO-GAP, (10) text-121 NO-GAP CENTERED, " unit price
                '|' NO-GAP, (10) text-122 NO-GAP CENTERED, " quantity
                '|' NO-GAP, (10) text-123 NO-GAP CENTERED, " amount

                '|' NO-GAP, (10) text-124 NO-GAP CENTERED, " price
                '|' NO-GAP, (10) text-123 NO-GAP CENTERED, " amount
                '|' NO-GAP, (10) text-125 NO-GAP CENTERED, " Deprc. Qty
                '|' NO-GAP, (10) text-126 NO-GAP CENTERED, " Amort. Qty

                '|' NO-GAP, (10) text-124 NO-GAP CENTERED,
                '|' NO-GAP, (10) text-123 NO-GAP CENTERED,
                '|' NO-GAP, (10) text-125 NO-GAP CENTERED,
                '|' NO-GAP, (10) text-126 NO-GAP CENTERED,

                '|' NO-GAP, (10) text-124 NO-GAP CENTERED,
                '|' NO-GAP, (10) text-123 NO-GAP CENTERED,
                '|' NO-GAP, (10) text-125 NO-GAP CENTERED,
                '|' NO-GAP, (10) text-126 NO-GAP CENTERED,

                '|' NO-GAP, (10) text-124 NO-GAP CENTERED, " Raw Materia
                '|' NO-GAP, (10) text-123 NO-GAP CENTERED,

                '|' NO-GAP, (10) text-124 NO-GAP CENTERED, " Freight
                '|' NO-GAP, (10) text-123 NO-GAP CENTERED,

                '|' NO-GAP, (10) text-124 NO-GAP CENTERED, " CC Cost
                '|' NO-GAP, (10) text-123 NO-GAP CENTERED,

                '|' NO-GAP, (10) text-124 NO-GAP CENTERED, " Sequencing
                '|' NO-GAP, (10) text-123 NO-GAP CENTERED,

                '|' NO-GAP, (10) text-124 NO-GAP CENTERED, " Direct Labo
                '|' NO-GAP, (10) text-123 NO-GAP CENTERED,

                '|' NO-GAP, (10) text-124 NO-GAP CENTERED, " Machine
                '|' NO-GAP, (10) text-123 NO-GAP CENTERED,

                '|' NO-GAP, (10) text-124 NO-GAP CENTERED, " MBE Cost
                '|' NO-GAP, (10) text-123 NO-GAP CENTERED,

                '|' NO-GAP, (10) text-124 NO-GAP CENTERED, " WBE Cost
                '|' NO-GAP, (10) text-123 NO-GAP CENTERED,

                '|' NO-GAP, (10) text-124 NO-GAP CENTERED, " NAFTA Cost
                '|' NO-GAP, (10) text-123 NO-GAP CENTERED,

                '|' NO-GAP, (10) text-124 NO-GAP CENTERED, " AALA Cost
                '|' NO-GAP, (10) text-123 NO-GAP CENTERED,

                '|' NO-GAP.
ENDFORM.                    " write_third_line_01

*&---------------------------------------------------------------------*
*&      Form  write_second_line_01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_second_line_01.
*---
  WRITE : /     '|' NO-GAP,  (18) text-117 NO-GAP CENTERED,
                '|' NO-GAP,  (30) text-118 NO-GAP CENTERED,
*                '|' NO-GAP,  (10) text-119 NO-GAP CENTERED,
*                '|' NO-GAP,  (35) text-120 NO-GAP CENTERED,
                '|' NO-GAP, (384) sy-uline NO-GAP,
*                '|' NO-GAP, (338) sy-uline NO-GAP,
                '|' NO-GAP.
ENDFORM.                    " write_second_line_01

*&---------------------------------------------------------------------*
*&      Form  process_by_material
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_by_material.
*---
  CLEAR : it_temp, it_temp[].

*  SELECT DISTINCT
*         matnr
*         menge
*         meins INTO CORRESPONDING FIELDS OF TABLE it_temp
*               FROM ztmm_bom
*              WHERE vkind EQ p_vehic
*                AND matnr IN s_matnr.

  SELECT matnr
         MAX( meins ) AS meins
         MAX( menge ) AS menge
                      INTO CORRESPONDING FIELDS OF TABLE it_temp
                      FROM ztmm_bom
                     WHERE vkind EQ p_vehic
                       AND matnr IN s_matnr
                  GROUP BY MATNR.

  SORT it_temp BY matnr.

**---
*  CLEAR : it_mara, it_mara[].
*
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_mara
*           FROM mara
*          WHERE mtart EQ 'FERT'
*            AND lvorm EQ space.
*
*  DELETE it_mara WHERE matnr+6(2) NE p_vehic.
*
**---
*  CLEAR : it_mara.
*  READ TABLE it_mara INDEX 1.
*
*  PERFORM color_search USING it_mara-matnr.
*
*---
  PERFORM set_condition_record(zrmmpm32r_cond).

**--- BOM explosion
*  PERFORM call_function_bom TABLES it_stpox_alv it_temp.
*
*---
  PERFORM get_info_record.

*--- get material desc.
  CLEAR : it_makt, it_makt[].
  SELECT a~matnr
         maktx
         meins INTO CORRESPONDING FIELDS OF TABLE it_makt
               FROM makt AS a INNER JOIN mara AS b
                 ON a~mandt EQ b~mandt
                AND a~matnr EQ b~matnr
                FOR ALL ENTRIES IN it_temp
              WHERE a~matnr EQ it_temp-matnr
                AND spras EQ sy-langu
                AND a~matnr IN s_matnr.

*--- get CBO data
  CLEAR : it_ztmm_cond, it_ztmm_cond[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_ztmm_cond
           FROM ztmm_cond
            FOR ALL ENTRIES IN it_temp
          WHERE matnr EQ it_temp-matnr
            AND matnr IN s_matnr
            AND lifnr EQ p_vendr
            AND ekorg EQ c_ekorg.

*---
  DATA : l_knumh LIKE a018-knumh,
         l_datab LIKE a018-datab.

  LOOP AT it_temp.
    MOVE : it_temp-matnr TO it_itab-matnr.
    CLEAR : it_makt, it_ztmm_cond.
    READ TABLE it_makt WITH KEY matnr = it_temp-matnr.
    MOVE : it_makt-maktx TO it_itab-maktx,
           it_makt-meins TO it_itab-meins.
*--- get depreciation qty.
    PERFORM read_depreciation_qty USING it_temp-matnr 'ZP01'.
    PERFORM read_depreciation_qty USING it_temp-matnr 'ZP02'.
    PERFORM read_depreciation_qty USING it_temp-matnr 'ZP03'.
*---
    PERFORM read_a018 USING    it_temp-matnr
                      CHANGING l_knumh l_datab.
    PERFORM read_condition USING  l_knumh.
    PERFORM read_itab_cond USING 'PB00'     CHANGING it_itab-tot_u.
    PERFORM read_itab_cond USING 'ZP01'     CHANGING it_itab-zp01a.
    PERFORM read_itab_cond USING 'ZP02'     CHANGING it_itab-zp02a.
    PERFORM read_itab_cond USING 'ZP03'     CHANGING it_itab-zp03a.
    PERFORM read_itab_cond USING 'ZP04'     CHANGING it_itab-zp04a.
    PERFORM read_itab_cond USING 'ZP05'     CHANGING it_itab-zp05a.
    PERFORM read_itab_cond USING 'ZP06'     CHANGING it_itab-zp06a.
    PERFORM read_itab_cond USING 'ZP07'     CHANGING it_itab-zp07a.
    PERFORM read_itab_cond USING 'ZP08'     CHANGING it_itab-zp08a.
    PERFORM read_itab_cond USING 'ZP09'     CHANGING it_itab-zp09a.
    PERFORM read_itab_cond USING 'ZP10'     CHANGING it_itab-zp10a.
    PERFORM read_itab_cond USING 'ZP11'     CHANGING it_itab-zp11a.
    PERFORM read_itab_cond USING 'ZP12'     CHANGING it_itab-zp12a.
    PERFORM read_itab_cond USING 'ZP13'     CHANGING it_itab-zp13a.
    MOVE : w_konwa TO it_itab-waers.
*---
    it_itab-zp01b = it_temp-menge * it_itab-zp01a.
    it_itab-zp02b = it_temp-menge * it_itab-zp02a.
    it_itab-zp03b = it_temp-menge * it_itab-zp03a.
    it_itab-zp04b = it_temp-menge * it_itab-zp04a.
    it_itab-zp05b = it_temp-menge * it_itab-zp05a.
    it_itab-zp06b = it_temp-menge * it_itab-zp06a.
    it_itab-zp07b = it_temp-menge * it_itab-zp07a.
    it_itab-zp08b = it_temp-menge * it_itab-zp08a.
    it_itab-zp09b = it_temp-menge * it_itab-zp09a.
    it_itab-zp10b = it_temp-menge * it_itab-zp10a.
    it_itab-zp11b = it_temp-menge * it_itab-zp11a.
    it_itab-zp12b = it_temp-menge * it_itab-zp12a.
    it_itab-zp13b = it_temp-menge * it_itab-zp13a.
    MOVE : it_temp-menge TO it_itab-tot_q.
    it_itab-tot_a = it_itab-tot_u * it_itab-tot_q.
*--- get GR qty.
    IF NOT ( it_itab-zp01c EQ 0 AND it_itab-zp02c EQ 0
                                AND it_itab-zp03c EQ 0 ).
      PERFORM get_gr_qty USING it_temp-matnr l_datab.
    ENDIF.
    APPEND it_itab.
    CLEAR : it_itab, it_temp.
  ENDLOOP.
ENDFORM.                    " process_by_material

*&---------------------------------------------------------------------*
*&      Form  process_by_vendor
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_by_vendor.
*---

ENDFORM.                    " process_by_vendor

*&---------------------------------------------------------------------*
*&      Form  color_search
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_MARA_MATNR  text
*----------------------------------------------------------------------*
FORM color_search USING    p_it_mara_matnr.
*---
  CLEAR : it_ibsymbol, it_ibsymbol[], w_atwrt, w_atwri.

  SELECT c~atwrt
         d~atnam
                 INTO TABLE it_ibsymbol
                 FROM marc AS a INNER JOIN ibin AS b
                   ON a~mandt EQ b~mandt
                  AND a~cuobj EQ b~instance
                      INNER JOIN v_ibin_syval AS c
                         ON b~mandt EQ c~mandt
                        AND b~in_recno EQ c~in_recno
                            INNER JOIN cabn AS d
                               ON c~mandt EQ d~mandt
                              AND c~atinn EQ d~atinn
                WHERE d~atnam IN ('COLOREXT', 'COLORINT')
                  AND a~matnr EQ p_it_mara_matnr.

  CLEAR : it_ibsymbol.
  READ TABLE it_ibsymbol WITH KEY atnam = 'COLOREXT'.
  MOVE : it_ibsymbol-atwrt TO w_atwrt.

  CLEAR : it_ibsymbol.
  READ TABLE it_ibsymbol WITH KEY atnam = 'COLORINT'.
  MOVE : it_ibsymbol-atwrt TO w_atwri.

  TRANSLATE : w_atwrt TO UPPER CASE,
              w_atwri TO UPPER CASE.
ENDFORM.                    " color_search

*&---------------------------------------------------------------------*
*&      Form  call_function_bom
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_STPOX_ALV  text
*      -->P_IT_TEMP  text
*----------------------------------------------------------------------*
FORM call_function_bom TABLES   it_stpox_alv STRUCTURE stpox_alv
                                it_temp STRUCTURE it_temp.
*---
  DATA : l_stlal LIKE mast-stlal.

  CLEAR : l_stlal, it_stpox_alv, it_stpox_alv[], it_temp, it_temp[].

*---
  PERFORM get_max_alternative USING it_mara-matnr
                              CHANGING l_stlal.

*---
  CALL FUNCTION 'Z_FMM_BOM_EXPL'
       EXPORTING
            im_matnr      = it_mara-matnr
            im_werks      = c_werks
            im_stlan      = c_stlan
            im_stlal      = l_stlal
            im_atwre      = w_atwrt
            im_atwri      = w_atwri
            im_datuv      = p_datbi
       TABLES
            ext_stpox_alv = it_stpox_alv.

*---
  DELETE it_stpox_alv WHERE mtart NE 'ROH'.

  LOOP AT it_stpox_alv.
    CHECK it_stpox_alv-idnrk IN s_matnr.
    MOVE : it_stpox_alv-idnrk TO it_temp-matnr,
           it_stpox_alv-menge TO it_temp-menge,
           it_stpox_alv-meins TO it_temp-meins.
    COLLECT it_temp.
    CLEAR : it_temp, it_stpox_alv.
  ENDLOOP.
ENDFORM.                    " call_function_bom

*&---------------------------------------------------------------------*
*&      Form  get_max_alternative
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_MARA_MATNR  text
*      <--P_L_STLAL  text
*----------------------------------------------------------------------*
FORM get_max_alternative USING    p_it_mara_matnr
                         CHANGING p_l_stlal.
*---
  SELECT SINGLE MAX( stlal ) INTO p_l_stlal
                             FROM mast
                            WHERE matnr EQ p_it_mara_matnr
                              AND werks EQ c_werks
                              AND stlan EQ c_stlan.
ENDFORM.                    " get_max_alternative

*&---------------------------------------------------------------------*
*&      Form  read_a018
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TEMP_MATNR  text
*      <--P_L_KNUMH  text
*----------------------------------------------------------------------*
FORM read_a018 USING    p_it_temp_matnr
               CHANGING p_l_knumh
                        p_l_datab.
*---
  CLEAR : a018, p_l_knumh.

  SELECT SINGLE knumh
                datab INTO (p_l_knumh, p_l_datab)
                      FROM a018
                     WHERE kappl EQ c_kappl
                       AND kschl EQ c_pb00
                       AND lifnr EQ p_vendr
                       AND matnr EQ p_it_temp_matnr
                       AND ekorg EQ c_ekorg
                       AND datbi GE p_datbi
                       AND datab LE p_datbi.
ENDFORM.                                                    " read_a018

*&---------------------------------------------------------------------*
*&      Form  read_condition
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_KNUMH  text
*----------------------------------------------------------------------*
FORM read_condition USING    p_l_knumh.
*---
  CLEAR : konp, it_cond, it_cond[].

  SELECT kschl kbetr
         konwa       INTO CORRESPONDING FIELDS OF TABLE it_cond
                     FROM konp
                   WHERE knumh EQ p_l_knumh
                     AND kschl IN r_kschl.
ENDFORM.                    " read_condition

*&---------------------------------------------------------------------*
*&      Form  read_itab_cond
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2012   text
*      <--P_IT_ITAB_ZP01A  text
*----------------------------------------------------------------------*
FORM read_itab_cond USING    p_value
                    CHANGING p_p_it_kbetr.
*---
  CLEAR : it_cond.

  READ TABLE it_cond WITH KEY kschl = p_value.

  IF sy-subrc EQ 0.
    MOVE : it_cond-kbetr TO p_p_it_kbetr.
  ENDIF.

*---
  IF p_value EQ c_pb00.
    MOVE : it_cond-konwa TO w_konwa.
  ENDIF.
ENDFORM.                    " read_itab_cond

*&---------------------------------------------------------------------*
*&      Form  change_color
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_INT  text
*----------------------------------------------------------------------*
FORM change_color CHANGING p_l_int.
*---
  IF p_l_int EQ space.
    FORMAT COLOR 2 INTENSIFIED OFF.
    MOVE : 'X' TO p_l_int.
  ELSE.
    FORMAT COLOR 2 INTENSIFIED ON.
    CLEAR : p_l_int.
  ENDIF.
ENDFORM.                    " change_color

*&---------------------------------------------------------------------*
*&      Form  get_info_record
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_info_record.
*---
  DATA : l_tabix1 LIKE sy-tabix,
         l_tabix2 LIKE sy-tabix.

  CLEAR : it_info, it_info[].

  SELECT a~matnr INTO CORRESPONDING FIELDS OF TABLE it_info
                 FROM v_eina AS a INNER JOIN mara AS b
                   ON a~mandt EQ b~mandt
                  AND a~matnr EQ b~matnr
                WHERE ekorg EQ c_ekorg
                  AND lifnr EQ p_vendr
                  AND loekz EQ space
                  AND mtart EQ 'ROH'
                  AND b~matnr IN s_matnr.

  SORT it_info BY matnr.

*---
  LOOP AT it_temp.
    MOVE : sy-tabix TO l_tabix1.
    READ TABLE it_info WITH KEY matnr = it_temp-matnr.
    MOVE : sy-tabix TO l_tabix2.
    IF sy-subrc EQ 0.
      DELETE it_info INDEX l_tabix2.
    ELSE.
      DELETE it_temp INDEX l_tabix1.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " get_info_record

*&---------------------------------------------------------------------*
*&      Form  read_depreciation_qty
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TEMP_MATNR  text
*      -->P_1829   text
*----------------------------------------------------------------------*
FORM read_depreciation_qty USING    p_it_temp_matnr
                                    p_value.
*---
  CLEAR : it_ztmm_cond.

  READ TABLE it_ztmm_cond WITH KEY matnr = p_it_temp_matnr
                                   lifnr = p_vendr
                                   ekorg = c_ekorg
                                   kschl = p_value.

  CASE p_value.
    WHEN 'ZP01'.
      MOVE : it_ztmm_cond-menge TO it_itab-zp01c.
    WHEN 'ZP02'.
      MOVE : it_ztmm_cond-menge TO it_itab-zp02c.
    WHEN 'ZP03'.
      MOVE : it_ztmm_cond-menge TO it_itab-zp03c.
  ENDCASE.
ENDFORM.                    " read_depreciation_qty

*&---------------------------------------------------------------------*
*&      Form  get_gr_qty
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TEMP_MATNR  text
*      -->P_L_DATAB  text
*----------------------------------------------------------------------*
FORM get_gr_qty USING    p_it_temp_matnr
                         p_l_datab.
*---
  DATA : BEGIN OF it_mseg OCCURS 0,
           bwart LIKE mseg-bwart,
           menge LIKE mseg-menge,
         END OF it_mseg.

  DATA : l_plus LIKE mseg-menge,
         l_minus LIKE mseg-menge.

  CLEAR : mseg, it_mseg, it_mseg[], l_plus, l_minus.

  SELECT bwart
         menge INTO CORRESPONDING FIELDS OF TABLE it_mseg
               FROM mseg
              WHERE matnr EQ it_temp-matnr
                AND zbudat GE p_l_datab
                AND bwart IN ('102', '122', '932', '101').

  LOOP AT it_mseg.
    CASE it_mseg-bwart.
      WHEN '102' OR '122' OR '932'.
        l_minus = l_minus + it_mseg-menge.
      WHEN '101'.
        l_plus = l_plus + it_mseg-menge.
    ENDCASE.
  ENDLOOP.

*---
  it_itab-zp01d = it_itab-zp01c + l_minus - l_plus.
  it_itab-zp02d = it_itab-zp02c + l_minus - l_plus.
  it_itab-zp03d = it_itab-zp03c + l_minus - l_plus.
ENDFORM.                    " get_gr_qty

*&---------------------------------------------------------------------*
*&      Form  bom_explosion
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bom_explosion.
*---
  CHECK sy-ucomm EQ 'FC01'.

  DATA : l_answer(1).

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      defaultoption        = 'Y'
      textline1            = 'Execute FSC Explosion ?'
      textline2            = 'It will take several minutes !'
      titel                = 'Confirm'
*     START_COLUMN         = 25
*     START_ROW            = 6
*     CANCEL_DISPLAY       = 'X'
    IMPORTING
      answer               = l_answer.

  IF l_answer EQ 'J'.
    SUBMIT zrmmpm30r_cond_bom AND RETURN.
    MESSAGE i999 WITH text-m03.
  ENDIF.
ENDFORM.                    " bom_explosion
