************************************************************************
* Program Name      : ZRMMPM32R_COND
* Author            : Jae-Sung, Lee
* Creation Date     : 2004.03.25.
* Specifications By : Jae-Sung, Lee
* Pattern           : Report 1-1
* Development Request No : UD1K908634
* Addl Documentation:
* Description       : Price Status per Vehicle by
*                     Condition
* Modification Logs
* Date            Developer        RequestNo      Description
* 2004.03.25.     Jae-Sung Lee     UD1K908634     Initial Coding
*
************************************************************************

REPORT zrmmpm31r_price NO STANDARD PAGE HEADING
                          LINE-SIZE 256
                          LINE-COUNT 65
                          MESSAGE-ID zmmm.

**---
INCLUDE : zrmmpmxxr_incl.


**--- Internal Tables
DATA : BEGIN OF it_itab OCCURS 0,
         char1(1),
       END OF it_itab.

DATA : it_stpox_alv LIKE stpox_alv OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_temp OCCURS 0,
         matnr LIKE mara-matnr,
         menge LIKE it_stpox_alv-menge,
         meins LIKE it_stpox_alv-meins,
         pb00  LIKE konp-kbetr,
         zp01  LIKE konp-kbetr,
       END OF it_temp.

DATA : BEGIN OF it_price OCCURS 0,
         matnr LIKE mara-matnr,
         menge LIKE it_stpox_alv-menge,
         meins LIKE it_stpox_alv-meins,
         pb00  LIKE konp-kbetr,
         zp01  LIKE konp-kbetr,
       END OF it_price.

DATA : BEGIN OF it_list OCCURS 0,
         matnr LIKE mara-matnr,
         maktx LIKE makt-maktx,
         menge LIKE it_stpox_alv-menge,
         meins LIKE it_stpox_alv-meins,
         pb00  LIKE konp-kbetr,
         zp01  LIKE konp-kbetr,
         qfsc1  LIKE it_stpox_alv-menge,
         qfsc2  LIKE it_stpox_alv-menge,
         qfsc3  LIKE it_stpox_alv-menge,
         qfsc4  LIKE it_stpox_alv-menge,
         qfsc5  LIKE it_stpox_alv-menge,
         qfsc6  LIKE it_stpox_alv-menge,
         ufsc1  LIKE konp-kbetr,
         ufsc2  LIKE konp-kbetr,
         ufsc3  LIKE konp-kbetr,
         ufsc4  LIKE konp-kbetr,
         ufsc5  LIKE konp-kbetr,
         ufsc6  LIKE konp-kbetr,
         cfsc1  LIKE konp-kbetr,
         cfsc2  LIKE konp-kbetr,
         cfsc3  LIKE konp-kbetr,
         cfsc4  LIKE konp-kbetr,
         cfsc5  LIKE konp-kbetr,
         cfsc6  LIKE konp-kbetr,
         konwa LIKE konp-konwa,
       END OF it_list.

DATA : wa_sum LIKE it_list.
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

DATA: l1        TYPE  i  VALUE  18,   " Material code
      l2        TYPE  i  VALUE  30,   " Material dec
      l3        TYPE  i  VALUE  6 ,   " Quantity
      l4        TYPE  i  VALUE  41,   " fsc quntity
      l5        TYPE  i  VALUE  9,    " unit price
      l6        TYPE  i  VALUE  65,   " uint price by fsc
      l7        TYPE  i  VALUE  10,    " conditon price
      l8        TYPE  i  VALUE  65 ,  " condtion price by fsc
      l9        TYPE  i  VALUE  255,  "
      l10       TYPE  i  VALUE  4 .     " unit
DATA : wa_color TYPE i.

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
PARAMETERS : p_kschl LIKE t685t-kschl
             AS LISTBOX VISIBLE LENGTH 25 DEFAULT 'ZP01' OBLIGATORY .
PARAMETERS : p_lifnr LIKE lfa1-lifnr ,
             p_budat LIKE mkpf-budat OBLIGATORY DEFAULT sy-datum.

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
  PERFORM check_vendor_code.


**---
AT SELECTION-SCREEN OUTPUT.
  PERFORM fill_dropdown_list USING 'P_KSCHL'.

**---
*TOP-OF-PAGE.
*  PERFORM write_top.


**---
START-OF-SELECTION.
  PERFORM get_data.


**---
END-OF-SELECTION.
  IF it_list[] IS INITIAL.
    MESSAGE s999 WITH text-m01.
  ELSE.
    PERFORM write_data.
  ENDIF.

*END-OF-PAGE.
*  PERFORM end_of_page_write.





*&---------------------------------------------------------------------*
*&      Form  write_top
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_top.
  TABLES : t685t.

  DATA : wa_condtion(65).
  WRITE : AT /(l9) '[ Price Status per Vehicle by Condition ]' CENTERED.
  WRITE : AT /(l9)
'========================================================' CENTERED.

  WRITE :/.
  WRITE :/.
  SELECT SINGLE vtext INTO t685t-vtext
         FROM t685t
         WHERE kschl = p_kschl
           AND spras = sy-langu .

  WRITE :/ 'Condition : ', t685t-vtext.
  CONCATENATE 'Price per ' t685t-vtext 'by FSC' INTO wa_condtion
SEPARATED BY space.
  IF p_lifnr <> ' '.
    PERFORM get_vendor_desc    USING  p_lifnr.

    WRITE :/ 'Vendor    : ','[',p_lifnr,']', lfa1-name1.
  ENDIF.

  WRITE :/ 'Valid Date: ', p_budat .

  WRITE :/ 'Full spec Code ' COLOR 7.
  WRITE :/ 'FSC1 :' ,p_fsc01,'  ', p_ate01,'  ', p_ati01 ,'     ',
           'FSC2 :', p_fsc02,'  ', p_ate02,'  ', p_ati02 .
  WRITE :/ 'FSC3 :' ,p_fsc03,'  ', p_ate03,'  ', p_ati03 ,'     ',
           'FSC4 :', p_fsc04,'  ', p_ate04,'  ', p_ati04 .
  WRITE :/ 'FSC5 :' ,p_fsc05,'  ', p_ate05,'  ', p_ati05 ,'     ',
           'FSC6 :', p_fsc06,'  ', p_ate06,'  ', p_ati06 .

* heading title .


  FORMAT COLOR 1 INTENSIFIED ON.
  WRITE : /.
  WRITE : AT (l9) sy-uline.
  WRITE: /'|' NO-GAP, AT (l1) text-h00 CENTERED NO-GAP,
          '|' NO-GAP, AT (l2) text-h00 CENTERED NO-GAP,
          '|' NO-GAP, AT (l4) text-h01 CENTERED NO-GAP,
          '|' NO-GAP, AT (l5) text-h02 CENTERED NO-GAP,
          '|' NO-GAP, AT (l6) text-h03 CENTERED NO-GAP,
          '|' NO-GAP, AT (l5) text-h04 CENTERED NO-GAP,
          '|' NO-GAP, AT (l8) wa_condtion CENTERED NO-GAP, " VER
          '|' NO-GAP, AT (l10) text-h00 CENTERED NO-GAP,
          '|' NO-GAP, AT (l10) text-h00 CENTERED NO-GAP,
          '|' NO-GAP.

  WRITE: /'|' NO-GAP, AT (l1) text-h06 CENTERED NO-GAP,
          '|' NO-GAP, AT (l2) text-h07 CENTERED NO-GAP,
          '|' NO-GAP, AT (l4) sy-uline CENTERED NO-GAP,
          '|' NO-GAP, AT (l5) text-h00 CENTERED NO-GAP,
          '|' NO-GAP, AT (l6) sy-uline CENTERED NO-GAP,
          '|' NO-GAP, AT (l5) text-h00 CENTERED NO-GAP,
          '|' NO-GAP, AT (l8) sy-uline CENTERED NO-GAP, " VER
          '|' NO-GAP, AT (l10) text-h09 CENTERED NO-GAP,
          '|' NO-GAP, AT (l10) text-h10 CENTERED NO-GAP,

          '|' NO-GAP.

  WRITE: /'|' NO-GAP, AT (l1) text-h00 CENTERED NO-GAP,
          '|' NO-GAP, AT (l2) text-h00 CENTERED NO-GAP,
          '|' NO-GAP, AT (l3) text-003 CENTERED NO-GAP,
          '|' NO-GAP, AT (l3) text-004 CENTERED NO-GAP,
          '|' NO-GAP, AT (l3) text-005 CENTERED NO-GAP,
          '|' NO-GAP, AT (l3) text-006 CENTERED NO-GAP,
          '|' NO-GAP, AT (l3) text-007 CENTERED NO-GAP,
          '|' NO-GAP, AT (l3) text-008 CENTERED NO-GAP,
          '|' NO-GAP, AT (l5) text-h08 CENTERED NO-GAP,
          '|' NO-GAP, AT (l7) text-003 CENTERED NO-GAP,
          '|' NO-GAP, AT (l7) text-004 CENTERED NO-GAP,
          '|' NO-GAP, AT (l7) text-005 CENTERED NO-GAP,
          '|' NO-GAP, AT (l7) text-006 CENTERED NO-GAP,
          '|' NO-GAP, AT (l7) text-007 CENTERED NO-GAP,
          '|' NO-GAP, AT (l7) text-008 CENTERED NO-GAP,
          '|' NO-GAP, AT (l5) text-h08 CENTERED NO-GAP,
          '|' NO-GAP, AT (l7) text-003 CENTERED NO-GAP,
          '|' NO-GAP, AT (l7) text-004 CENTERED NO-GAP,
          '|' NO-GAP, AT (l7) text-005 CENTERED NO-GAP,
          '|' NO-GAP, AT (l7) text-006 CENTERED NO-GAP,
          '|' NO-GAP, AT (l7) text-007 CENTERED NO-GAP,
          '|' NO-GAP, AT (l7) text-008 CENTERED NO-GAP,
          '|' NO-GAP, AT (l10) text-h00 CENTERED NO-GAP,
          '|' NO-GAP, AT (l10) text-h00 CENTERED NO-GAP,

          '|' NO-GAP.


  WRITE : AT (l9) sy-uline.

  FORMAT RESET.
  SET LEFT SCROLL-BOUNDARY COLUMN 52.
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
*  PERFORM set_condition_record.

*---
  FIELD-SYMBOLS : <field01>,
                  <field02>,
                  <field03>.

  DATA : l_field01(7), l_field02(7), l_field03(7),
         l_index(2) TYPE n.

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
                              USING <field01> <field02> <field03>
                                    p_budat.
    PERFORM collect_item USING   l_index .
  ENDDO.

  PERFORM calc_condition .

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
  DATA : l_page_count(5).

  CLEAR wa_color.
*  RESERVE 65 LINES.

  PERFORM write_top.

  FORMAT COLOR 3 INTENSIFIED OFF.

* SUM
  WRITE:
  /'|' NO-GAP, AT (l1) 'Gross Sum'  COLOR 3 INTENSIFIED OFF NO-GAP,
   '|' NO-GAP, AT (l2)  text-h00  COLOR 3 INTENSIFIED OFF NO-GAP,
   '|' NO-GAP, AT (l3) text-h00  NO-GAP,
   '|' NO-GAP, AT (l3) text-h00  NO-GAP,
   '|' NO-GAP, AT (l3) text-h00  NO-GAP,
   '|' NO-GAP, AT (l3) text-h00  NO-GAP,
   '|' NO-GAP, AT (l3) text-h00  NO-GAP,
   '|' NO-GAP, AT (l3) text-h00  NO-GAP,
   '|' NO-GAP, AT (l5) text-h00  CURRENCY it_list-konwa  NO-GAP,
   '|' NO-GAP, AT (l7) wa_sum-ufsc1 CURRENCY 'USD'  NO-GAP,
   '|' NO-GAP, AT (l7) wa_sum-ufsc2 CURRENCY 'USD'  NO-GAP,
   '|' NO-GAP, AT (l7) wa_sum-ufsc3 CURRENCY 'USD'  NO-GAP,
   '|' NO-GAP, AT (l7) wa_sum-ufsc4 CURRENCY 'USD'  NO-GAP,
   '|' NO-GAP, AT (l7) wa_sum-ufsc5 CURRENCY 'USD'  NO-GAP,
   '|' NO-GAP, AT (l7) wa_sum-ufsc6 CURRENCY 'USD'  NO-GAP,
   '|' NO-GAP, AT (l5) text-h00  NO-GAP,
   '|' NO-GAP, AT (l7) wa_sum-cfsc1 CURRENCY 'USD'  NO-GAP,
   '|' NO-GAP, AT (l7) wa_sum-cfsc2 CURRENCY 'USD'  NO-GAP,
   '|' NO-GAP, AT (l7) wa_sum-cfsc3 CURRENCY 'USD'  NO-GAP,
   '|' NO-GAP, AT (l7) wa_sum-cfsc4 CURRENCY 'USD'  NO-GAP,
   '|' NO-GAP, AT (l7) wa_sum-cfsc5 CURRENCY 'USD'  NO-GAP,
   '|' NO-GAP, AT (l7) wa_sum-cfsc6 CURRENCY 'USD'  NO-GAP,
   '|' NO-GAP, AT (l10) it_list-meins   NO-GAP,
   '|' NO-GAP, AT (l10) it_list-konwa   NO-GAP,
   '|' NO-GAP.
  FORMAT RESET.
* LIST
  SORT  it_list BY matnr.

  LOOP AT it_list.

    wa_color = wa_color + 1.

    IF wa_color > 5.
      FORMAT COLOR 2 INTENSIFIED ON.
      IF wa_color = 10.
        wa_color = 0.
      ENDIF.
    ELSE.
      FORMAT COLOR 2 INTENSIFIED OFF.
    ENDIF.

    WRITE:
    /'|' NO-GAP, AT (l1) it_list-matnr  COLOR 1 INTENSIFIED OFF NO-GAP,
     '|' NO-GAP, AT (l2) it_list-maktx  COLOR 5 INTENSIFIED OFF NO-GAP,
     '|' NO-GAP, AT (l3) it_list-qfsc1 UNIT it_list-meins  NO-GAP,
     '|' NO-GAP, AT (l3) it_list-qfsc2 UNIT it_list-meins  NO-GAP,
     '|' NO-GAP, AT (l3) it_list-qfsc3 UNIT it_list-meins  NO-GAP,
     '|' NO-GAP, AT (l3) it_list-qfsc4 UNIT it_list-meins  NO-GAP,
     '|' NO-GAP, AT (l3) it_list-qfsc5 UNIT it_list-meins  NO-GAP,
     '|' NO-GAP, AT (l3) it_list-qfsc6 UNIT it_list-meins  NO-GAP,
     '|' NO-GAP, AT (l5) it_list-pb00  CURRENCY it_list-konwa  NO-GAP,
     '|' NO-GAP, AT (l7) it_list-ufsc1 CURRENCY it_list-konwa  NO-GAP,
     '|' NO-GAP, AT (l7) it_list-ufsc2 CURRENCY it_list-konwa  NO-GAP,
     '|' NO-GAP, AT (l7) it_list-ufsc3 CURRENCY it_list-konwa  NO-GAP,
     '|' NO-GAP, AT (l7) it_list-ufsc4 CURRENCY it_list-konwa  NO-GAP,
     '|' NO-GAP, AT (l7) it_list-ufsc5 CURRENCY it_list-konwa  NO-GAP,
     '|' NO-GAP, AT (l7) it_list-ufsc6 CURRENCY it_list-konwa  NO-GAP,
     '|' NO-GAP, AT (l5) it_list-zp01  CURRENCY it_list-konwa  NO-GAP,
     '|' NO-GAP, AT (l7) it_list-cfsc1 CURRENCY it_list-konwa  NO-GAP,
     '|' NO-GAP, AT (l7) it_list-cfsc2 CURRENCY it_list-konwa  NO-GAP,
     '|' NO-GAP, AT (l7) it_list-cfsc3 CURRENCY it_list-konwa  NO-GAP,
     '|' NO-GAP, AT (l7) it_list-cfsc4 CURRENCY it_list-konwa  NO-GAP,
     '|' NO-GAP, AT (l7) it_list-cfsc5 CURRENCY it_list-konwa  NO-GAP,
     '|' NO-GAP, AT (l7) it_list-cfsc6 CURRENCY it_list-konwa  NO-GAP,
     '|' NO-GAP, AT (l10) it_list-meins   NO-GAP,
     '|' NO-GAP, AT (l10) it_list-konwa   NO-GAP,
     '|' NO-GAP.

    SET LEFT SCROLL-BOUNDARY COLUMN 52.

  ENDLOOP.

  WRITE :/ sy-uline.

*  WRITE :/ sy-uline.
*  FORMAT COLOR 7 ON INVERSE.
*  WRITE: AT /(l9) '>>>>>>===( END OF PAGE )=====<<<<<<<' CENTERED  ,
*            180(15) text-b00 , sy-datum,
*            210(15) sy-pagno , 'of' ,sy-pagno ,'Page'.
*
*** totoal page.
*  WRITE sy-pagno TO l_page_count LEFT-JUSTIFIED.
*
*  DO sy-pagno TIMES.
*    READ LINE 63 OF PAGE sy-index.
*    REPLACE '*****' WITH l_page_count INTO sy-lisel.
*    MODIFY CURRENT LINE.
*  ENDDO.

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

  DATA : wa_fname LIKE  dynpread-fieldname.

  CLEAR dynpread. REFRESH dynpread.
  CLEAR valuetab. REFRESH valuetab.
  CLEAR fields.   REFRESH fields.

  MOVE p_value TO wa_fname .

  PERFORM value_read USING wa_fname.

  LOOP AT dynpread.
    CASE sy-tabix.
      WHEN '1' . p_param = dynpread-fieldvalue.
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
FORM value_read USING   p_name .
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

  CLEAR : l_stlal, it_stpox_alv, it_stpox_alv[], it_temp, it_temp[].

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
**--- end of insert

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
FORM calc_condition .
*---
  DATA : l_knumh LIKE a018-knumh,
         l_tabix LIKE sy-tabix.
  CLEAR : wa_sum.
  LOOP AT it_list.
    MOVE : sy-tabix TO l_tabix.
    PERFORM read_a018 USING    p_lifnr it_list-matnr
                      CHANGING l_knumh.
    PERFORM read_condition USING  l_knumh   .
    PERFORM read_itab_cond USING  c_pb00    CHANGING it_list-pb00
                                                     it_list-konwa.
    PERFORM read_itab_cond USING  p_kschl   CHANGING it_list-zp01
                                                     it_list-konwa.
    PERFORM calulation_condition .
    PERFORM get_matrial_descrition .
    MODIFY it_list.
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
FORM read_a018 USING    p_p_lifnr
                        p_p_it_temp_matnr
               CHANGING p_knumh.
*---
  CLEAR : a018, p_knumh.
  IF p_p_lifnr <> ''.
    SELECT SINGLE knumh INTO p_knumh
                        FROM a018
                       WHERE kappl EQ c_kappl
                         AND kschl EQ c_pb00
                         AND lifnr EQ p_p_lifnr
                         AND matnr EQ p_p_it_temp_matnr
                         AND ekorg EQ c_ekorg
                         AND datbi GE p_budat
                         AND datab LE p_budat.
    IF sy-subrc NE 0.
*      DELETE IT
    ENDIF.

  ELSE.
    SELECT SINGLE knumh INTO p_knumh
                     FROM a018
                    WHERE kappl EQ c_kappl
                      AND kschl EQ c_pb00
                      AND matnr EQ p_p_it_temp_matnr
                      AND ekorg EQ c_ekorg
                      AND datbi GE p_budat
                      AND datab LE p_budat.

  ENDIF.
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

  PERFORM append_condition_record USING : p_kschl .

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
FORM read_condition USING    p_l_knumh  .

*---
  CLEAR : konp, it_cond, it_cond[].

  SELECT kschl kbetr konwa INTO CORRESPONDING FIELDS OF TABLE it_cond
                     FROM konp
                    WHERE knumh EQ p_l_knumh
                      AND ( kschl EQ p_kschl OR
                            kschl EQ c_pb00  ).
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
                    CHANGING p_p_it_kbetr
                             p_p_it_konwa.
*---
  READ TABLE it_cond WITH KEY kschl = p_value.

  IF sy-subrc EQ 0.
    MOVE : it_cond-kbetr TO p_p_it_kbetr,
           it_cond-konwa TO p_p_it_konwa.
  ENDIF.
ENDFORM.                    " read_itab_cond
*&---------------------------------------------------------------------*
*&      Form  FILL_DROPDOWN_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0479   text
*----------------------------------------------------------------------*
FORM fill_dropdown_list USING p_parameter .

  TYPE-POOLS: vrm. " For parameter drop down lists
*-- Definitions for parameter drop down lists
  DATA:    name  TYPE vrm_id,
           list  TYPE vrm_values,
           value LIKE LINE OF list.

  DATA : BEGIN OF it_value OCCURS 0,
         kschl  LIKE t685t-kschl,
         vtext  LIKE t685t-vtext,
         END OF it_value.

  name = p_parameter.

  SELECT a~kschl
         b~vtext
         INTO CORRESPONDING FIELDS OF TABLE it_value
         FROM t685 AS a INNER JOIN t685t AS b
              ON a~mandt EQ b~mandt
         AND a~kvewe EQ b~kvewe
         AND a~kappl EQ b~kappl
         AND a~kschl EQ b~kschl
         WHERE spras EQ sy-langu
            AND a~kvewe EQ 'A'
            AND a~kappl EQ c_kappl
            AND a~kschl LIKE 'ZP%' .

  LOOP AT it_value.
    value-key  = it_value-kschl.
    value-text = it_value-vtext.
    APPEND value TO list. CLEAR value.
  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            id     = name
            values = list.
ENDFORM.                    " FILL_DROPDOWN_LIST
*&---------------------------------------------------------------------*
*&      Form  CHECK_VENDOR_CODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_vendor_code.

  DATA : wa_lifnr LIKE lfa1-lifnr.

  CHECK  p_lifnr NE '' .
  SELECT SINGLE lifnr INTO wa_lifnr
         FROM lfa1
          WHERE lifnr = p_lifnr
            AND loevm EQ ''.
  IF sy-subrc NE 0.
    MESSAGE e163(f2).
  ENDIF.


ENDFORM.                    " CHECK_VENDOR_CODE
*&---------------------------------------------------------------------*
*&      Form  COLLECT_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM collect_item USING  p_index .
  FIELD-SYMBOLS : <fielv1>.

  DATA : l_fielv1(14),
         l_index(1) TYPE n.

  LOOP AT it_temp.
    MOVE : p_index TO l_index.
    CONCATENATE : 'IT_LIST-QFSC' l_index INTO l_fielv1.

    ASSIGN : (l_fielv1) TO <fielv1>.
    MOVE : it_temp-matnr TO it_list-matnr,
           it_temp-meins TO it_list-meins,
           it_temp-menge TO <fielv1> .
    COLLECT it_list. CLEAR it_list.
  ENDLOOP.

ENDFORM.                    " COLLECT_ITEM
*&---------------------------------------------------------------------*
*&      Form  GET_MATRIAL_DESCRITION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_IT_LIST_MAKTX  text
*----------------------------------------------------------------------*
FORM get_matrial_descrition .

  PERFORM get_material_desc USING  it_list-matnr.

  MOVE makt-maktx TO it_list-maktx.

ENDFORM.                    " GET_MATRIAL_DESCRITION
*&---------------------------------------------------------------------*
*&      Form  CALULATION_CONDITION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calulation_condition.
* Price per Vehicle by FSC
  FIELD-SYMBOLS : <qfsc>,
                  <ufsc>,
                  <cfsc>,
                  <usum>,
                  <csum>.

  DATA : l_qfsc(17), l_ufsc(17), l_cfsc(17),
         l_usum(17), l_csum(17),
         l_index(1) TYPE n.

  DO 6 TIMES.
    MOVE : sy-index TO l_index.
    CONCATENATE : 'IT_LIST-QFSC' l_index INTO l_qfsc,
                  'IT_LIST-UFSC' l_index INTO l_ufsc,
                  'IT_LIST-CFSC' l_index INTO l_cfsc,
                  'WA_SUM-UFSC'  l_index INTO l_usum,
                  'WA_SUM-CFSC'  l_index INTO l_csum.

    ASSIGN : (l_qfsc) TO <qfsc>,
             (l_ufsc) TO <ufsc>,
             (l_cfsc) TO <cfsc>,
             (l_usum) TO <usum>,
             (l_csum) TO <csum>.

    <ufsc> = it_list-pb00 * <qfsc>.
    <cfsc> = it_list-zp01 * <qfsc>.
    <usum> = <usum> + <ufsc>.
    <csum> = <csum> + <cfsc>.
  ENDDO.


ENDFORM.                    " CALULATION_CONDITION
*&---------------------------------------------------------------------*
*&      Form  END_OF_PAGE_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM end_of_page_write.
  WRITE : AT /(l9) sy-uline.
  FORMAT COLOR 7 ON INVERSE.
  WRITE: AT /(l9) '>>>>>>===( NEXT PAGE )=====<<<<<<<' CENTERED ,
            180(15) text-b00 , sy-datum,
            210(15) sy-pagno , 'of' ,'*****','Page'.


ENDFORM.                    " END_OF_PAGE_WRITE
