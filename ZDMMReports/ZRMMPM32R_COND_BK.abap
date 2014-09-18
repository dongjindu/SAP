************************************************************************
* Program Name      : ZRMMPM32R_COND
* Author            : Sung-Tae, Lim
* Creation Date     : 2004.03.25.
* Specifications By : Sung-Tae, Lim
* Pattern           : Report 1-1
* Development Request No : UD1K908580
* Addl Documentation:
* Description       : Rate Status by FSC
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2004.03.25.     Sung-Tae Lim     UD1K908580     Initial Coding
*
************************************************************************

REPORT zrmmpm32r_cond NO STANDARD PAGE HEADING
                     LINE-SIZE 200
                     LINE-COUNT 64(1)
                     MESSAGE-ID zmmm.

**---
INCLUDE : zrmmpmxxr_incl.


**--- Internal Tables
DATA : BEGIN OF it_itab OCCURS 0,
         kschl LIKE t685-kschl,
         vtext LIKE t685t-vtext,
         amt01 LIKE konp-kbetr,
         pct01 LIKE konp-kbetr,
         amt02 LIKE konp-kbetr,
         pct02 LIKE konp-kbetr,
         amt03 LIKE konp-kbetr,
         pct03 LIKE konp-kbetr,
         amt04 LIKE konp-kbetr,
         pct04 LIKE konp-kbetr,
         amt05 LIKE konp-kbetr,
         pct05 LIKE konp-kbetr,
         amt06 LIKE konp-kbetr,
         pct06 LIKE konp-kbetr,
         konwa LIKE konp-konwa,
       END OF it_itab.

DATA : it_stpox_alv LIKE stpox_alv OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_temp OCCURS 0,
         matnr LIKE mara-matnr,
         menge LIKE it_stpox_alv-menge,
         meins LIKE it_stpox_alv-meins,
         zp00  LIKE konp-kbetr,
         zp01  LIKE konp-kbetr,
         zp02  LIKE konp-kbetr,
         zp03  LIKE konp-kbetr,
         zp04  LIKE konp-kbetr,
         zp05  LIKE konp-kbetr,
         zp06  LIKE konp-kbetr,
         zp07  LIKE konp-kbetr,
         zp08  LIKE konp-kbetr,
         zp09  LIKE konp-kbetr,
         zp10  LIKE konp-kbetr,
         zp11  LIKE konp-kbetr,
         zp12  LIKE konp-kbetr,
         zp13  LIKE konp-kbetr,
         konwa LIKE konp-konwa,
       END OF it_temp.

DATA : it_temp_cond LIKE it_temp OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_cond OCCURS 0,
         kschl LIKE konp-kschl,
         kbetr LIKE konp-kbetr,
         konwa LIKE konp-konwa,
       END OF it_cond.


DATA dynpread LIKE dynpread OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF valuetab OCCURS 0,
          value(80).
DATA: END OF valuetab.

DATA: BEGIN OF fields OCCURS 0.
        INCLUDE STRUCTURE help_value.
DATA: END OF fields.

DATA: BEGIN OF dynpfields  OCCURS 0.
        INCLUDE STRUCTURE dynpread.
DATA: END OF dynpfields.

DATA  select_index LIKE sy-tabix.

DATA: BEGIN OF select_values OCCURS 0.
        INCLUDE STRUCTURE help_vtab.
DATA: END OF select_values.

DATA: BEGIN OF it_ibsymbol OCCURS 0,
        atwrt TYPE ibsymbol-atwrt,
        atnam TYPE cabn-atnam,
      END OF it_ibsymbol.


**---
RANGES : r_kschl FOR konp-kschl.


**---
DATA : w_konwa LIKE konp-konwa,
       w_amt01 LIKE konp-kbetr,
       w_amt02 LIKE konp-kbetr,
       w_amt03 LIKE konp-kbetr,
       w_amt04 LIKE konp-kbetr,
       w_amt05 LIKE konp-kbetr,
       w_amt06 LIKE konp-kbetr.


**---
CONSTANTS : c_werks LIKE mast-werks VALUE 'P001',
            c_stlan LIKE mast-stlan VALUE '1',
            c_kappl LIKE a018-kappl VALUE 'M',
            c_pb00  LIKE a018-kschl VALUE 'PB00',
            c_ekorg LIKE a018-ekorg VALUE 'PU01'.

**---
DEFINE selection_screen.
  selection-screen begin of line.
  selection-screen comment (10) &1.
  selection-screen position 31.
  parameters : &2(18) matchcode object mat1.     " obligatory.
  selection-screen position 53.
  parameters : &3(3).
  selection-screen position 60.
  parameters : &4(3).
  selection-screen end of line.
END-OF-DEFINITION.

DEFINE at_selection_screen.

at selection-screen on value-request for &1.
  perform help_request_name using    &2 &3 &4
                            changing &1.
END-OF-DEFINITION.


**---
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
PARAMETERS :      " p_lifnr LIKE lfa1-lifnr OBLIGATORY,
             p_budat LIKE mkpf-budat OBLIGATORY.

*SELECT-OPTIONS : s_budat FOR mkpf-budat OBLIGATORY.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK block11 WITH FRAME TITLE text-002.

selection_screen : text-003 p_fsc01 p_ate01 p_ati01,
                 : text-004 p_fsc02 p_ate02 p_ati02,
                 : text-005 p_fsc03 p_ate03 p_ati03,
                 : text-006 p_fsc04 p_ate04 p_ati04,
                 : text-007 p_fsc05 p_ate05 p_ati05,
                 : text-008 p_fsc06 p_ate06 p_ati06.

SELECTION-SCREEN END OF BLOCK block11.

SELECTION-SCREEN END OF BLOCK block1.


**---
at_selection_screen p_ate01 p_fsc01 'P_FSC01' 'P_ATE01'.
at_selection_screen p_ati01 p_fsc01 'P_FSC01' 'P_ATI01'.

at_selection_screen p_ate02 p_fsc02 'P_FSC02' 'P_ATE02'.
at_selection_screen p_ati02 p_fsc02 'P_FSC02' 'P_ATI02'.

at_selection_screen p_ate03 p_fsc03 'P_FSC03' 'P_ATE03'.
at_selection_screen p_ati03 p_fsc03 'P_FSC03' 'P_ATI03'.

at_selection_screen p_ate04 p_fsc04 'P_FSC04' 'P_ATE04'.
at_selection_screen p_ati04 p_fsc04 'P_FSC04' 'P_ATI04'.

at_selection_screen p_ate05 p_fsc05 'P_FSC05' 'P_ATE05'.
at_selection_screen p_ati05 p_fsc05 'P_FSC05' 'P_ATI05'.

at_selection_screen p_ate06 p_fsc06 'P_FSC06' 'P_ATE06'.
at_selection_screen p_ati06 p_fsc06 'P_FSC06' 'P_ATI06'.


**---
AT SELECTION-SCREEN.
  PERFORM get_default_color USING p_fsc01 CHANGING p_ate01 p_ati01.
  PERFORM get_default_color USING p_fsc02 CHANGING p_ate02 p_ati02.
  PERFORM get_default_color USING p_fsc03 CHANGING p_ate03 p_ati03.
  PERFORM get_default_color USING p_fsc04 CHANGING p_ate04 p_ati04.
  PERFORM get_default_color USING p_fsc05 CHANGING p_ate05 p_ati05.
  PERFORM get_default_color USING p_fsc06 CHANGING p_ate06 p_ati06.


**---
TOP-OF-PAGE.
  PERFORM write_top.


**---
START-OF-SELECTION.
  PERFORM get_data.


**---
END-OF-SELECTION.
  IF it_itab[] IS INITIAL.
    MESSAGE s999 WITH text-m01.
  ELSE.
    PERFORM write_data.
  ENDIF.





*&---------------------------------------------------------------------*
*&      Form  write_top
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_top.

ENDFORM.                    " write_top

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
  PERFORM set_condition_record.
  PERFORM append_itab.

*---
  FIELD-SYMBOLS : <field01>,
                  <field02>,
                  <field03>,
                  <field04>,
                  <field05>.

  DATA : l_field01(7), l_field02(7), l_field03(7),
         l_index(2) TYPE n,
         l_field04(17), l_field05(13),
         l_cond_index(2) TYPE n,
         l_field_index(2) TYPE n.

  DO 6 TIMES.
    MOVE : sy-index TO l_index.
    CONCATENATE : 'P_FSC' l_index INTO l_field01,
                  'P_ATE' l_index INTO l_field02,
                  'P_ATI' l_index INTO l_field03.
    ASSIGN : (l_field01) TO <field01>,
             (l_field02) TO <field02>,
             (l_field03) TO <field03>.
    CHECK <field01> NE space.
    PERFORM call_function_bom TABLES it_stpox_alv it_temp
*                             USING p_fsc01 p_ate01 p_ati01 s_budat-low.
                              USING <field01> <field02> <field03>
                                    p_budat.
    PERFORM calc_condition TABLES it_temp.
*---
    CLEAR : it_temp_cond.
    DELETE it_temp_cond WHERE konwa EQ space.
    READ TABLE it_temp_cond INDEX 1.
    DO 14 TIMES.
      l_cond_index = sy-index - 1.
      MOVE : l_index TO l_field_index.
      READ TABLE it_itab INDEX sy-index.
      MOVE : it_temp_cond-konwa TO it_itab-konwa.
      CONCATENATE : 'IT_TEMP_COND-ZP' l_cond_index  INTO l_field04,
                    'IT_ITAB-AMT'     l_field_index INTO l_field05.
      ASSIGN : (l_field04) TO <field04>,
               (l_field05) TO <field05>.
      MOVE : <field04> TO <field05>.
      MODIFY it_itab INDEX sy-index.
    ENDDO.
  ENDDO.
ENDFORM.                    " get_data

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
  WRITE : AT /75    text-t01 CENTERED,
          AT /74    text-t02 CENTERED.

  SKIP 2.

  FORMAT COLOR COL_HEADING INTENSIFIED ON.

  WRITE : /(148) sy-uline.

  WRITE : /     '|' NO-GAP,  (20) text-s20 CENTERED NO-GAP,
                '|' NO-GAP,  (20) p_fsc01 CENTERED NO-GAP,
                '|' NO-GAP,  (20) p_fsc02 CENTERED NO-GAP,
                '|' NO-GAP,  (20) p_fsc03 CENTERED NO-GAP,
                '|' NO-GAP,  (20) p_fsc04 CENTERED NO-GAP,
                '|' NO-GAP,  (20) p_fsc05 CENTERED NO-GAP,
                '|' NO-GAP,  (20) p_fsc06 CENTERED NO-GAP,
                '|' NO-GAP.

  WRITE : /     '|' NO-GAP,  (20) text-t03 CENTERED NO-GAP,
                '|' NO-GAP, (125) sy-uline NO-GAP,
                '|' NO-GAP.

  WRITE : /     '|' NO-GAP,  (20) text-s20 CENTERED NO-GAP,
                '|' NO-GAP.
  DO 6 TIMES.
    WRITE :                    (12) text-t04 CENTERED NO-GAP,
                  '|' NO-GAP,  (07) text-t05 CENTERED NO-GAP,
                  '|' NO-GAP.
  ENDDO.

  WRITE : /(148) sy-uline.

  FORMAT RESET.

  LOOP AT it_itab.
    IF sy-tabix EQ 1.
      PERFORM write_data_first.
    ELSE.
      PERFORM write_data_others.
    ENDIF.
    WRITE : /(148) sy-uline.
    MODIFY it_itab INDEX sy-tabix.
  ENDLOOP.

  WRITE : /(148) sy-uline.
ENDFORM.                    " write_data

*&---------------------------------------------------------------------*
*&      Form  help_request_name
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0091   text
*      <--P_P_ATE01  text
*----------------------------------------------------------------------*
FORM help_request_name USING    p_param p_value p_colfield
                       CHANGING p_p_color.
*---
  DATA it_ksml   LIKE TABLE OF ksml  WITH HEADER LINE.
  DATA : BEGIN OF it_cawn OCCURS 0,
          atwrt   LIKE  cawn-atwrt,
          atwtb   LIKE  cawnt-atwtb.
  DATA : END OF it_cawn.

  DATA :  l_cuobj   LIKE  inob-cuobj,
          l_clint   LIKE  klah-clint.

  CLEAR dynpread. REFRESH dynpread.
  CLEAR valuetab. REFRESH valuetab.
  CLEAR fields.   REFRESH fields.

  PERFORM value_read USING: p_value.
  LOOP AT dynpread.
    CASE sy-tabix.
      WHEN 1. p_param = dynpread-fieldvalue.
    ENDCASE.
  ENDLOOP.

  SELECT SINGLE cuobj
         INTO l_cuobj
         FROM inob
         WHERE klart EQ '300'
           AND obtab EQ 'MARA'
           AND objek EQ p_param.

  SELECT SINGLE clint
         INTO l_clint
         FROM kssk
         WHERE objek EQ l_cuobj
           AND mafid EQ 'O'
           AND klart EQ '300'.

  SELECT *
         INTO TABLE it_ksml
         FROM ksml
         WHERE clint EQ l_clint.

  DATA l_tabix   LIKE sy-tabix.
  LOOP AT it_ksml.
    l_tabix = sy-tabix.
    IF p_colfield+4(1) EQ 'E'.
      SELECT SINGLE *
                FROM cabn
                WHERE atinn EQ it_ksml-imerk
                  AND atnam EQ 'COLOREXT'.
    ELSEIF p_colfield+4(1) EQ 'I'.
      SELECT SINGLE *
                FROM cabn
                WHERE atinn EQ it_ksml-imerk
                  AND atnam EQ 'COLORINT'.
    ENDIF.
    IF sy-subrc NE 0.
      DELETE it_ksml INDEX l_tabix.
    ENDIF.
  ENDLOOP.

  READ TABLE it_ksml INDEX 1.
  SELECT a~atwrt
         b~atwtb
         INTO TABLE it_cawn
         FROM cawn AS a INNER JOIN cawnt AS b
                        ON  a~atinn EQ b~atinn
                        AND a~atzhl EQ b~atzhl
         WHERE a~atinn EQ it_ksml-omerk.
  SORT it_cawn.
  it_cawn-atwrt = 'No entry'.
  INSERT it_cawn INDEX 1.
  CLEAR: it_cawn.
  LOOP AT it_cawn.
    valuetab-value = it_cawn-atwrt.
    APPEND valuetab. CLEAR valuetab.
    valuetab-value = it_cawn-atwtb.
    APPEND valuetab. CLEAR valuetab.
  ENDLOOP.

  PERFORM add_fields USING: 'CAWN'  'ATWRT' 'X',
                            'CAWNT' 'ATWTB' ' '.

  PERFORM help_values_get.

  IF select_index > 0.
    READ TABLE it_cawn   INDEX select_index.
    PERFORM value_update USING:
            'X'   p_colfield   it_cawn-atwrt 0.
  ENDIF.
ENDFORM.                    " help_request_name

*&---------------------------------------------------------------------*
*&      Form  VALUE_READ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_VALUE  text
*----------------------------------------------------------------------*
FORM value_read USING    p_name.
*---
  dynpread-fieldname = p_name. APPEND dynpread.
  CALL FUNCTION 'DYNP_VALUES_READ'
       EXPORTING
            dyname                   = sy-cprog
            dynumb                   = sy-dynnr
       TABLES
            dynpfields               = dynpread
*      EXCEPTIONS
*           INVALID_ABAPWORKAREA     = 1
*           INVALID_DYNPROFIELD      = 2
*           INVALID_DYNPRONAME       = 3
*           INVALID_DYNPRONUMMER     = 4
*           INVALID_REQUEST          = 5
*           NO_FIELDDESCRIPTION      = 6
*           INVALID_PARAMETER        = 7
*           UNDEFIND_ERROR           = 8
*           DOUBLE_CONVERSION        = 9
*           OTHERS                   = 10
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " VALUE_READ

*&---------------------------------------------------------------------*
*&      Form  add_fields
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0426   text
*      -->P_0427   text
*      -->P_0428   text
*----------------------------------------------------------------------*
FORM add_fields USING    p_tabname p_fieldname p_flag.
*---
  fields-tabname = p_tabname.
  fields-fieldname = p_fieldname.
  fields-selectflag = p_flag.
  APPEND fields.      CLEAR fields.
ENDFORM.                    " add_fields

*&---------------------------------------------------------------------*
*&      Form  value_update
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0449   text
*      -->P_0450   text
*      -->P_IT_CAWN_ATWRT  text
*      -->P_0      text
*----------------------------------------------------------------------*
FORM value_update USING    p_process
                           p_fieldname
                           p_fieldvalue
                           p_stepl.
*---
  CLEAR dynpfields.
  dynpfields-fieldname = p_fieldname.
  dynpfields-fieldvalue = p_fieldvalue.
  IF p_stepl > 0.
    dynpfields-stepl = p_stepl.
  ENDIF.
  APPEND dynpfields.      CLEAR dynpfields.

  IF p_process EQ 'X'.
    CALL FUNCTION 'DYNP_VALUES_UPDATE'
         EXPORTING
              dyname               = sy-cprog
              dynumb               = sy-dynnr
         TABLES
              dynpfields           = dynpfields
         EXCEPTIONS
              invalid_abapworkarea = 1
              invalid_dynprofield  = 2
              invalid_dynproname   = 3
              invalid_dynpronummer = 4
              invalid_request      = 5
              no_fielddescription  = 6
              undefind_error       = 7
              OTHERS               = 8.
    REFRESH dynpfields.
  ENDIF.
ENDFORM.                    " value_update

*&---------------------------------------------------------------------*
*&      Form  help_values_get
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM help_values_get.
*---
  CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
       EXPORTING
            display                   = ' '
       IMPORTING
            index                     = select_index
       TABLES
            fields                    = fields
            select_values             = select_values
            valuetab                  = valuetab
       EXCEPTIONS
            field_not_in_ddic         = 1
            more_then_one_selectfield = 2
            no_selectfield            = 3
            OTHERS                    = 4.
ENDFORM.                    " help_values_get

*&---------------------------------------------------------------------*
*&      Form  get_default_color
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_FSC01  text
*      <--P_P_ATE01  text
*      <--P_P_ATI01  text
*----------------------------------------------------------------------*
FORM get_default_color USING    p_p_fsc
                       CHANGING p_p_ate
                                p_p_ati.
*---
  DATA :   l_atwre  LIKE ibsymbol-atwrt,
           l_atwri  LIKE ibsymbol-atwrt.

*---
  IF p_p_fsc NE space.
    CLEAR : mara.
    SELECT SINGLE matnr INTO mara-matnr
                        FROM mara
                       WHERE matnr EQ p_p_fsc
                         AND mtart EQ 'FERT'.
    IF sy-subrc NE 0.
      MESSAGE e999 WITH text-m01 p_p_fsc text-m02.
    ENDIF.
  ENDIF.

*---
  PERFORM color_search USING    p_p_fsc.

  READ TABLE it_ibsymbol WITH KEY atnam = 'COLOREXT'.
  l_atwre = it_ibsymbol-atwrt.
  READ TABLE it_ibsymbol WITH KEY atnam = 'COLORINT'.
  l_atwri = it_ibsymbol-atwrt.

  IF p_p_ate IS INITIAL.
    p_p_ate = l_atwre.
  ENDIF.

  IF p_p_ati IS INITIAL.
    p_p_ati = l_atwri.
  ENDIF.

  TRANSLATE p_p_ate TO UPPER CASE.
  TRANSLATE p_p_ati TO UPPER CASE.
ENDFORM.                    " get_default_color

*&---------------------------------------------------------------------*
*&      Form  color_search
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_P_FSC  text
*----------------------------------------------------------------------*
FORM color_search USING    p_p_p_fsc.
*---
  REFRESH it_ibsymbol. CLEAR it_ibsymbol.

  SELECT c~atwrt
         d~atnam
       INTO TABLE it_ibsymbol
       FROM marc AS a INNER JOIN ibin AS b
                        ON a~cuobj EQ b~instance
                      INNER JOIN v_ibin_syval AS c
                        ON b~in_recno EQ c~in_recno
                      INNER JOIN cabn AS d
                        ON c~atinn EQ d~atinn
       WHERE d~atnam IN ('COLOREXT', 'COLORINT')
       AND   a~matnr EQ p_p_p_fsc.
ENDFORM.                    " color_search

*&---------------------------------------------------------------------*
*&      Form  call_function_bom
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_FCS01  text
*      -->P_C_WERKS  text
*      -->P_C_STLAN  text
*      -->P_P_ATE01  text
*      -->P_P_ATI01  text
*      -->P_S_BUDAT_LOW  text
*----------------------------------------------------------------------*
FORM call_function_bom TABLES   it_stpox_alv STRUCTURE stpox_alv
                                it_temp STRUCTURE it_temp
                       USING    p_fcs     " FSC
                                p_ate     " ext. color
                                p_ati     " int. color
                                p_budat.     " date
*---
  DATA : l_stlal LIKE mast-stlal.

  CLEAR : l_stlal, it_stpox_alv, it_stpox_alv[], it_temp, it_temp[],
          it_temp_cond, it_temp_cond[].

*---
  PERFORM get_max_alternative USING p_fcs
                              CHANGING l_stlal.

*---
  CALL FUNCTION 'Z_FMM_BOM_EXPL'
       EXPORTING
            im_matnr      = p_fcs
            im_werks      = c_werks
            im_stlan      = c_stlan
            im_stlal      = l_stlal
            im_atwre      = p_ate
            im_atwri      = p_ati
            im_datuv      = p_budat
       TABLES
            ext_stpox_alv = it_stpox_alv.

*---
*  DELETE it_stpox_alv WHERE mtart NE 'ROH'.
*  DELETE it_stpox_alv WHERE eitm NE 'M'.

**--- insert by stlim (2004/04/27)
  DELETE it_stpox_alv WHERE NOT ( eitm EQ 'M' OR eitm EQ 'K' OR
                                  eitm EQ 'G' OR eitm EQ 'C' OR
                                  eitm EQ '1' OR eitm EQ '2' )
                        AND NOT ( zinfo EQ 'ENG' OR zinfo EQ 'TM' ).
**---

*---
  LOOP AT it_stpox_alv.
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
*      -->P_P_P_FCS01  text
*      <--P_L_STLAL  text
*----------------------------------------------------------------------*
FORM get_max_alternative USING    p_p_p_fcs01
                         CHANGING p_l_stlal.
*---
  SELECT SINGLE MAX( stlal ) INTO p_l_stlal
                             FROM mast
                            WHERE matnr EQ p_p_p_fcs01
                              AND werks EQ c_werks
                              AND stlan EQ c_stlan.
ENDFORM.                    " get_max_alternative

*&---------------------------------------------------------------------*
*&      Form  calc_condition
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TEMP  text
*----------------------------------------------------------------------*
FORM calc_condition TABLES   p_it_temp STRUCTURE it_temp.
*---
  DATA : l_knumh LIKE a018-knumh,
         l_tabix LIKE sy-tabix.

  LOOP AT p_it_temp.
    MOVE : sy-tabix TO l_tabix.
    PERFORM read_a018 USING    p_it_temp-matnr
*                               p_lifnr p_it_temp-matnr
                      CHANGING l_knumh.
    PERFORM read_condition USING  l_knumh.
    PERFORM read_itab_cond USING 'PB00'     CHANGING p_it_temp-zp00.
    PERFORM read_itab_cond USING 'ZP01'     CHANGING p_it_temp-zp01.
    PERFORM read_itab_cond USING 'ZP02'     CHANGING p_it_temp-zp02.
    PERFORM read_itab_cond USING 'ZP03'     CHANGING p_it_temp-zp03.
    PERFORM read_itab_cond USING 'ZP04'     CHANGING p_it_temp-zp04.
    PERFORM read_itab_cond USING 'ZP05'     CHANGING p_it_temp-zp05.
    PERFORM read_itab_cond USING 'ZP06'     CHANGING p_it_temp-zp06.
    PERFORM read_itab_cond USING 'ZP07'     CHANGING p_it_temp-zp07.
    PERFORM read_itab_cond USING 'ZP08'     CHANGING p_it_temp-zp08.
    PERFORM read_itab_cond USING 'ZP09'     CHANGING p_it_temp-zp09.
    PERFORM read_itab_cond USING 'ZP10'     CHANGING p_it_temp-zp10.
    PERFORM read_itab_cond USING 'ZP11'     CHANGING p_it_temp-zp11.
    PERFORM read_itab_cond USING 'ZP12'     CHANGING p_it_temp-zp12.
    PERFORM read_itab_cond USING 'ZP13'     CHANGING p_it_temp-zp13.
    MOVE : w_konwa TO p_it_temp-konwa.
    MODIFY p_it_temp INDEX l_tabix.
*---
    it_temp_cond-zp00 = p_it_temp-menge * p_it_temp-zp00.
    it_temp_cond-zp01 = p_it_temp-menge * p_it_temp-zp01.
    it_temp_cond-zp02 = p_it_temp-menge * p_it_temp-zp02.
    it_temp_cond-zp03 = p_it_temp-menge * p_it_temp-zp03.
    it_temp_cond-zp04 = p_it_temp-menge * p_it_temp-zp04.
    it_temp_cond-zp05 = p_it_temp-menge * p_it_temp-zp05.
    it_temp_cond-zp06 = p_it_temp-menge * p_it_temp-zp06.
    it_temp_cond-zp07 = p_it_temp-menge * p_it_temp-zp07.
    it_temp_cond-zp08 = p_it_temp-menge * p_it_temp-zp08.
    it_temp_cond-zp09 = p_it_temp-menge * p_it_temp-zp09.
    it_temp_cond-zp10 = p_it_temp-menge * p_it_temp-zp10.
    it_temp_cond-zp11 = p_it_temp-menge * p_it_temp-zp11.
    it_temp_cond-zp12 = p_it_temp-menge * p_it_temp-zp12.
    it_temp_cond-zp13 = p_it_temp-menge * p_it_temp-zp13.
    MOVE : w_konwa TO it_temp_cond-konwa.
    COLLECT it_temp_cond.
  ENDLOOP.
ENDFORM.                    " calc_condition

*&---------------------------------------------------------------------*
*&      Form  read_a018
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_LIFNR  text
*      -->P_P_IT_TEMP_MATNR  text
*----------------------------------------------------------------------*
FORM read_a018 USING    " p_p_lifnr
                        p_p_it_temp_matnr
               CHANGING p_knumh.
*---
  CLEAR : a018, p_knumh.

  SELECT SINGLE knumh INTO p_knumh
                      FROM a018
                     WHERE kappl EQ c_kappl
                       AND kschl EQ c_pb00
*                       AND lifnr EQ p_p_lifnr
                       AND matnr EQ p_p_it_temp_matnr
                       AND ekorg EQ c_ekorg
                       AND datbi GE p_budat
                       AND datab LE p_budat.
ENDFORM.                                                    " read_a018

*&---------------------------------------------------------------------*
*&      Form  set_condition_record
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_condition_record.
*---
  CLEAR : r_kschl, r_kschl[].

  PERFORM append_condition_record USING : 'PB00',
                                          'ZP01', 'ZP02', 'ZP03',
                                          'ZP04', 'ZP05', 'ZP06',
                                          'ZP07', 'ZP08', 'ZP09',
                                          'ZP10', 'ZP11', 'ZP12',
                                          'ZP13'.
ENDFORM.                    " set_condition_record

*&---------------------------------------------------------------------*
*&      Form  append_condition_record
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1241   text
*----------------------------------------------------------------------*
FORM append_condition_record USING  p_value.
*---
  MOVE : 'I'     TO r_kschl-sign,
         'EQ'    TO r_kschl-option,
         p_value TO r_kschl-low.

  APPEND r_kschl.
ENDFORM.                    " append_condition_record

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
*      -->P_1270   text
*      <--P_P_IT_TEMP_PB00  text
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
*&      Form  append_itab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_itab.
*---
  CLEAR : it_itab, it_itab[].

  SELECT a~kschl
         vtext   INTO CORRESPONDING FIELDS OF TABLE it_itab
                 FROM t685 AS a INNER JOIN t685t AS b
                   ON a~mandt EQ b~mandt
                  AND a~kvewe EQ b~kvewe
                  AND a~kappl EQ b~kappl
                  AND a~kschl EQ b~kschl
                WHERE spras EQ sy-langu
                  AND a~kvewe EQ 'A'
                  AND a~kappl EQ c_kappl
                  AND ( a~kschl EQ c_pb00
                     OR a~kschl LIKE 'ZP%' ).
ENDFORM.                    " append_itab

*&---------------------------------------------------------------------*
*&      Form  write_data_first
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_data_first.
*---
  FORMAT COLOR COL_TOTAL INTENSIFIED OFF.

  WRITE : /     '|' NO-GAP,  (20)  it_itab-vtext NO-GAP,
                '|' NO-GAP,  (15)  it_itab-amt01 NO-GAP
                                           CURRENCY it_itab-konwa,
                             (05)  it_itab-konwa NO-GAP,
                '|' NO-GAP,  (15)  it_itab-amt02 NO-GAP
                                           CURRENCY it_itab-konwa,
                             (05)  it_itab-konwa NO-GAP,
                '|' NO-GAP,  (15)  it_itab-amt03 NO-GAP
                                           CURRENCY it_itab-konwa,
                             (05)  it_itab-konwa NO-GAP,
                '|' NO-GAP,  (15)  it_itab-amt04 NO-GAP
                                           CURRENCY it_itab-konwa,
                             (05)  it_itab-konwa NO-GAP,
                '|' NO-GAP,  (15)  it_itab-amt05 NO-GAP
                                           CURRENCY it_itab-konwa,
                             (05)  it_itab-konwa NO-GAP,
                '|' NO-GAP,  (15)  it_itab-amt06 NO-GAP
                                           CURRENCY it_itab-konwa,
                             (05)  it_itab-konwa NO-GAP,
                '|' NO-GAP.

  CLEAR : w_amt01, w_amt02, w_amt03, w_amt04, w_amt05, w_amt06.

  MOVE : it_itab-amt01 TO w_amt01,
         it_itab-amt02 TO w_amt02,
         it_itab-amt03 TO w_amt03,
         it_itab-amt04 TO w_amt04,
         it_itab-amt05 TO w_amt05,
         it_itab-amt06 TO w_amt06.
ENDFORM.                    " write_data_first

*&---------------------------------------------------------------------*
*&      Form  write_data_others
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_data_others.
*---
  it_itab-pct01 = it_itab-amt01 / w_amt01 * 100.
  it_itab-pct02 = it_itab-amt02 / w_amt02 * 100.
  it_itab-pct03 = it_itab-amt03 / w_amt03 * 100.
  it_itab-pct04 = it_itab-amt04 / w_amt04 * 100.
  it_itab-pct05 = it_itab-amt05 / w_amt05 * 100.
  it_itab-pct06 = it_itab-amt06 / w_amt06 * 100.

  FORMAT RESET.

  WRITE : /     '|' NO-GAP,  (20)  it_itab-vtext NO-GAP
                                   COLOR COL_HEADING INTENSIFIED OFF.

  FORMAT RESET.

  WRITE :       '|' NO-GAP,  (12)  it_itab-amt01 NO-GAP
                                           CURRENCY it_itab-konwa,
                '|' NO-GAP,  (07)  it_itab-pct01 NO-GAP,
                '|' NO-GAP,  (12)  it_itab-amt02 NO-GAP
                                           CURRENCY it_itab-konwa,
                '|' NO-GAP,  (07)  it_itab-pct02 NO-GAP,
                '|' NO-GAP,  (12)  it_itab-amt03 NO-GAP
                                           CURRENCY it_itab-konwa,
                '|' NO-GAP,  (07)  it_itab-pct03 NO-GAP,
                '|' NO-GAP,  (12)  it_itab-amt04 NO-GAP
                                           CURRENCY it_itab-konwa,
                '|' NO-GAP,  (07)  it_itab-pct04 NO-GAP,
                '|' NO-GAP,  (12)  it_itab-amt05 NO-GAP
                                           CURRENCY it_itab-konwa,
                '|' NO-GAP,  (07)  it_itab-pct05 NO-GAP,
                '|' NO-GAP,  (12)  it_itab-amt06 NO-GAP
                                           CURRENCY it_itab-konwa,
                '|' NO-GAP,  (07)  it_itab-pct06 NO-GAP,
                '|' NO-GAP.
ENDFORM.                    " write_data_others
