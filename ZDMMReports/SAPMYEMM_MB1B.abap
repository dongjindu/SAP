*&---------------------------------------------------------------------*
*& Report  SAPMYEMM_MB1B                                               *
*&---------------------------------------------------------------------*
REPORT  sapmyemm_mb1b.

*----- Tables
INCLUDE : zrmmpmxxr_incl.

TABLES :  t148.

TYPES: BEGIN OF t_itab,
        matnr LIKE mseg-matnr,
        erfmg LIKE mseg-erfmg,
        erfme LIKE mseg-erfme,
        lgtyp LIKE mseg-lgtyp,
        lgpla LIKE mseg-lgpla,
        lgort LIKE mseg-lgort,
        werks LIKE mseg-werks,
        maktx LIKE makt-maktx,
        zeile TYPE i,
       END OF t_itab.

DATA: itab TYPE STANDARD TABLE OF t_itab  WITH HEADER LINE,
      wa   LIKE LINE OF itab,
      fill TYPE i.
*DATA : it_ymkpf LIKE TABLE OF ymkpfs_01 .

*------ Possible entry button

DATA : xkzbew      TYPE c VALUE ' ', "WA-Pos. im WE-Beleg manuell erf.
       old_bwartwa LIKE rm07m-bwart,    "gem. WA-Bewegungsart
       old_sobkz   LIKE mseg-sobkz,
       old_bwartwe LIKE rm07m-bwart,    "gem. WE-Bewegungsart
       anf_bild    TYPE c VALUE ' ', "Anforderungsbild
       xkzbw       TYPE c,               " KzBwa ist relevant
       xfausw      TYPE c.               " Feldauswahl


CONSTANTS: BEGIN OF hlp,
             b(4) TYPE c VALUE 'HLPB',
             f(4) TYPE c VALUE 'HLPF',
             g(4) TYPE c VALUE 'HLPG',
             m(4) TYPE c VALUE 'HLPM',
             r(4) TYPE c VALUE 'HLPR',
             v(4) TYPE c VALUE 'HLPV',
           END OF hlp.

DATA :" w_okcode like sy-ucomm,
       w_save_okcode LIKE sy-ucomm,
       w_index LIKE sy-index,
       w_loopc LIKE sy-loopc,
       w_top_line TYPE i VALUE 1,
       w_lines TYPE i,
       w_selected(1).
DATA : w_okcode LIKE sy-ucomm.

DATA: idx   TYPE i,
line  TYPE i,
lines TYPE i,
limit TYPE i,
c     TYPE i,
n1    TYPE i VALUE 5,
n2    TYPE i VALUE 25.

DATA : w_erfmg LIKE mseg-erfmg.

*-- BDC
DATA : BEGIN OF bdc_tab OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA : END OF bdc_tab.
DATA : gl_mode TYPE tb_bdcmode VALUE 'E'.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS '100'.
  SET TITLEBAR '100'.

  IF t158 IS INITIAL.
    SELECT SINGLE * FROM t158 WHERE tcode EQ 'MB1B'.
  ENDIF.

  MOVE : 'X' TO anf_bild.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  exit_100  INPUT
*----------------------------------------------------------------------*
MODULE exit_100 INPUT.
  CASE sy-ucomm.
    WHEN 'EXIT' OR 'CANCEL'.
      CLEAR sy-ucomm.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " exit_100  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_0100  INPUT
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      CLEAR sy-ucomm.
      LEAVE TO SCREEN 0.
    WHEN 'ENTER'.
      CLEAR sy-ucomm.
      PERFORM get_data.
  ENDCASE.
ENDMODULE.                 " user_command_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  bwart_hilfe  INPUT
*----------------------------------------------------------------------*
MODULE bwart_hilfe INPUT.
  DATA: BEGIN OF xdynpread OCCURS 2.
          INCLUDE STRUCTURE dynpread.
  DATA: END OF xdynpread.
  DATA: loc_repid LIKE sy-repid.
  DATA: loc_dynnr LIKE sy-dynnr.
  loc_repid = sy-repid.
  loc_dynnr = sy-dynnr.
  IF t158-kzbew IS INITIAL OR xkzbew = 1.
    MOVE 'RM07M-BWARTWA' TO xdynpread-fieldname.
  ELSE.
    MOVE 'RM07M-BWARTWE' TO xdynpread-fieldname.
  ENDIF.
  APPEND xdynpread.

  MOVE 'RM07M-SOBKZ' TO xdynpread-fieldname.
  APPEND xdynpread.
  CALL FUNCTION 'DYNP_VALUES_READ'
       EXPORTING
            dyname               = loc_repid
            dynumb               = loc_dynnr
       TABLES
            dynpfields           = xdynpread
       EXCEPTIONS
            invalid_abapworkarea = 1
            invalid_dynprofield  = 2
            invalid_dynproname   = 3
            invalid_dynpronummer = 4
            invalid_request      = 5
            no_fielddescription  = 6
            invalid_parameter    = 7
            undefind_error       = 8
            OTHERS               = 9.
  LOOP AT xdynpread.
    IF xdynpread-fieldname EQ 'RM07M-SOBKZ'.
      rm07m-sobkz = xdynpread-fieldvalue.
    ELSE.
      rm07m-bwart = xdynpread-fieldvalue.
    ENDIF.
  ENDLOOP.

  IF t158-kzbew IS INITIAL OR xkzbew = 1.

    IF old_bwartwa IS INITIAL.
      old_bwartwa = rm07m-bwart.
      old_sobkz = rm07m-sobkz.
    ENDIF.
  ELSE.
    IF old_bwartwe IS INITIAL.
      old_bwartwe = rm07m-bwart.
    ENDIF.
  ENDIF.
  CALL FUNCTION 'MB_SELECT_MOVEMENT_TYPE'
       EXPORTING
            tcode  = t158-tcode     " T-Code
            hilfe  = hlp-m
            anfbi  = anf_bild                  "X
            xkzbw  = xkzbw
            xkzbew = xkzbew
            kzbew  = t158-kzbew
*            EXT_CALL =  C  DEFAULT SPACE
            ext_call   = 'X'

       IMPORTING
            bwart  = rm07m-bwart
            sobkz  = rm07m-sobkz.
  IF NOT rm07m-bwart IS INITIAL.
    IF t158-kzbew IS INITIAL OR xkzbew = 1.
      rm07m-bwartwa = rm07m-bwart.


      rm07m-bwartwe = rm07m-bwart.
    ENDIF.
  ELSE.
    IF t158-kzbew IS INITIAL OR xkzbew = 1.
      rm07m-bwart = rm07m-bwartwa = old_bwartwa.
      rm07m-sobkz = old_sobkz.
      SET PARAMETER ID 'BWA' FIELD rm07m-bwart.
    ELSE.
      rm07m-bwart = rm07m-bwartwe = old_bwartwe.
      SET PARAMETER ID 'BWE' FIELD rm07m-bwart.
    ENDIF.
  ENDIF.
*    hier dynp_values_update, da pbo ab 3.1 nicht mehr prozessiert wird
  LOOP AT xdynpread.
    IF xdynpread-fieldname EQ 'RM07M-SOBKZ'.
      xdynpread-fieldvalue = rm07m-sobkz.
    ELSE.
      xdynpread-fieldvalue = rm07m-bwart.
    ENDIF.
    MODIFY xdynpread.
  ENDLOOP.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
       EXPORTING
            dyname     = loc_repid
            dynumb     = loc_dynnr
       TABLES
            dynpfields = xdynpread
       EXCEPTIONS
            OTHERS     = 4.
ENDMODULE.                 " bwart_hilfe  INPUT
*&---------------------------------------------------------------------*
*&      Module  datum_hilfe  INPUT
*----------------------------------------------------------------------*
MODULE datum_hilfe INPUT.
* DATA-Definitionen
  DATA: dynp_name  LIKE sy-repid.
  DATA: dynp_numb  LIKE sy-dynnr.
  DATA: field_name LIKE am61k-fname.
  DATA: help_date  LIKE sy-datum.
  DATA: loc_werks  LIKE t001w-werks.
  DATA: BEGIN OF dynp_fields OCCURS 1.
          INCLUDE STRUCTURE dynpread.
  DATA: END OF dynp_fields.
  FIELD-SYMBOLS: <date>.

* Festwerte
  dynp_name = sy-repid.
  dynp_numb = sy-dynnr.
  dynp_fields-fieldname = 'RM07M-WERKS'.
  APPEND dynp_fields.

* Werk vom Dynpro lesen
  CALL FUNCTION 'DYNP_VALUES_READ'
       EXPORTING
            dyname     = dynp_name
            dynumb     = dynp_numb
       TABLES
            dynpfields = dynp_fields
       EXCEPTIONS
            OTHERS     = 1.
  IF sy-subrc IS INITIAL.
    READ TABLE dynp_fields INDEX 1.
    MOVE dynp_fields-fieldvalue TO loc_werks.
  ENDIF.

* Werkskalender lesen
  CLEAR t001w.
  SELECT SINGLE * FROM t001w WHERE werks = loc_werks.

* F4-Datumsauswahl
  CALL FUNCTION 'GET_DATE_FROM_CALID'
       EXPORTING
            calid      = t001w-fabkl
            repid      = dynp_name
            dynnr      = dynp_numb
       IMPORTING
            field_name = field_name
            new_date   = help_date.

* Selektiertes Datum ubernehmen
  ASSIGN TABLE FIELD (field_name) TO <date>.
  MOVE help_date TO <date>.
ENDMODULE.                 " datum_hilfe  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_data_rtn  INPUT
*----------------------------------------------------------------------*
MODULE check_data_rtn INPUT.
  PERFORM check_bwartwa.
  PERFORM check_sobkz.
  PERFORM check_werks.
ENDMODULE.                 " check_data_rtn  INPUT
*&---------------------------------------------------------------------*
*&      Form  check_bwartwa
*----------------------------------------------------------------------*
FORM check_bwartwa.
  CHECK t158b-bwart NE space.
  SELECT SINGLE * FROM t158b WHERE bwart EQ t158b-bwart.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m01.
  ENDIF.
ENDFORM.                    " check_bwartwa
*&---------------------------------------------------------------------*
*&      Form  check_sobkz
*----------------------------------------------------------------------*
FORM check_sobkz.
  CHECK t148-sobkz NE space.
  SELECT SINGLE * FROM t148 WHERE sobkz EQ t148-sobkz.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m02.
  ENDIF.
ENDFORM.                    " check_sobkz
*&---------------------------------------------------------------------*
*&      Module  pickup_init  OUTPUT
*----------------------------------------------------------------------*
MODULE pickup_init OUTPUT.
*-- bldat (Defualt)
  IF mkpf-bldat IS INITIAL.
    MOVE : sy-datum TO mkpf-bldat.
  ENDIF.
*-- budat (Defualt)
  IF mkpf-budat IS INITIAL.
    MOVE : sy-datum TO mkpf-budat.
  ENDIF.
ENDMODULE.                 " pickup_init  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  get_data
*----------------------------------------------------------------------*
FORM get_data.
*-- Msegk-Bwart
  CHECK rm07m-bwartwa NE space.
  MOVE : rm07m-bwartwa TO msegk-bwart.
*-- Msegk-Sobkz
*  CHECK rm07m-sobkz NE space.
  MOVE : rm07m-sobkz TO msegk-sobkz.
*-- T156-btext
  CHECK msegk-bwart NE space.
  SELECT SINGLE btext INTO t156t-btext
      FROM t156t
       WHERE bwart EQ rm07m-bwartwa
         AND spras EQ sy-langu.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m03.
  ENDIF.
*-- Mseg-werks
  CHECK rm07m-werks NE space. "layout -> invisible check
  MOVE : rm07m-werks TO msegk-umwrk.

*  DO 10 TIMES.
*    wa-zeile = sy-index.
*    wa-werks = rm07m-werks.
*    wa-lgort = rm07m-lgort.
*    APPEND wa TO itab.
*  ENDDO.

  DESCRIBE TABLE itab LINES fill.

  CALL SCREEN 200.
ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Module  status_0200  OUTPUT
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS '200'.
  SET TITLEBAR '200'.
ENDMODULE.                 " status_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  exit_200  INPUT
*----------------------------------------------------------------------*
MODULE exit_200 INPUT.
  CASE sy-ucomm.
    WHEN 'EXIT' OR 'CNACEL'.
      CLEAR sy-ucomm.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " exit_200  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_0200  INPUT
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      CLEAR sy-ucomm.
      LEAVE TO SCREEN 0.
    WHEN 'ITEM'.
      CLEAR sy-ucomm.
      PERFORM new_items.
    WHEN 'P--' OR 'P-' OR 'P+' OR 'P++'.     " Page Scroll
      PERFORM page_scroll.
      CLEAR sy-ucomm.
    WHEN 'SAVE'.
      CLEAR sy-ucomm.
      PERFORM material_document_posting.

  ENDCASE.
ENDMODULE.                 " user_command_0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  CHECK_WERKS
*----------------------------------------------------------------------*
FORM check_werks.
  CHECK rm07m-werks NE space.
  SELECT SINGLE * FROM t001w  WHERE werks EQ rm07m-werks.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m04.
  ENDIF.
ENDFORM.                    " CHECK_WERKS
*&---------------------------------------------------------------------*
*&      Module  transp_itab_out  OUTPUT
*----------------------------------------------------------------------*
MODULE transp_itab_out OUTPUT.
  idx = sy-stepl + line.
  READ TABLE itab INDEX idx.
  MOVE-CORRESPONDING : itab TO mseg.

ENDMODULE.                 " transp_itab_out  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  transp_itab_in  INPUT
*----------------------------------------------------------------------*
MODULE transp_itab_in INPUT.
  w_index = w_top_line + sy-stepl - 1.
  READ TABLE  itab INDEX w_index.

  IF sy-subrc EQ 0.
    MOVE-CORRESPONDING mseg TO itab.
    MOVE : makt-maktx TO itab-maktx.
    MODIFY itab INDEX w_index.
  ELSE.
    CHECK mseg-matnr NE space.
    MOVE-CORRESPONDING mseg TO itab.
    APPEND itab.
  ENDIF.
ENDMODULE.                 " transp_itab_in  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_data_0200  INPUT
*----------------------------------------------------------------------*
MODULE check_data_0200 INPUT.
  PERFORM get_data_matnr.
*  PERFORM get_data_erfme.
ENDMODULE.                 " check_data_0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  get_data_matnr
*----------------------------------------------------------------------*
FORM get_data_matnr.
*-- Matnr
  CHECK mseg-matnr NE space.
  SELECT SINGLE * FROM mara WHERE matnr EQ mseg-matnr.
  IF sy-subrc NE 0.
    SET CURSOR FIELD 'MSEG-MATNR' LINE sy-stepl.
    MESSAGE e000(zz) WITH text-m05.
  ELSE.

*-- Maktx (Description)
    SELECT SINGLE maktx INTO makt-maktx
           FROM makt
            WHERE matnr EQ mseg-matnr
              AND spras EQ sy-langu.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH text-m05.
    ENDIF.

*-- Mseg-ERFME
    SELECT SINGLE * FROM mard   "Storage Location Data for Material
                     WHERE matnr EQ mseg-matnr
                       AND werks EQ mseg-werks
                       AND lgort EQ mseg-lgort.

    IF sy-subrc NE 0.
      SET CURSOR FIELD 'MSEG-MATNR' LINE sy-stepl.
      MESSAGE e006(m7) WITH mseg-matnr mseg-werks mseg-lgort.
    ENDIF.
*-- Mseg-erfme
    IF mseg-erfme EQ space.
      MOVE : mara-meins TO mseg-erfme.
    ELSE.
      MOVE : mseg-erfmg TO w_erfmg.
      CALL FUNCTION 'MATERIAL_UNIT_CONVERSION'
           EXPORTING
                input                = w_erfmg
                matnr                = mseg-matnr
                meinh                = mseg-erfme
                meins                = mara-meins
           IMPORTING
                output               = w_erfmg
           EXCEPTIONS
                conversion_not_found = 1
                input_invalid        = 2
                material_not_found   = 3
                meinh_not_found      = 4
                meins_missing        = 5
                no_meinh             = 6
                output_invalid       = 7
                overflow             = 8
                OTHERS               = 9.
      IF sy-subrc NE 0.
        SET CURSOR FIELD 'MSEG-ERFME' LINE sy-stepl.
        MESSAGE e026(m7) WITH mseg-erfme mara-meins.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " get_data_matnr
*&---------------------------------------------------------------------*
*&      Form  get_data_erfme
*----------------------------------------------------------------------*
FORM get_data_erfme.
  CHECK  mseg-erfmg NE space.
  MOVE : mseg-meins TO mseg-erfme.
ENDFORM.                    " get_data_erfmg
*&---------------------------------------------------------------------*
*&      Module  control_screen_scr200  OUTPUT
*----------------------------------------------------------------------*
MODULE control_screen_scr200 OUTPUT.
**--- Screen Field Hide.
  LOOP AT SCREEN.
    IF screen-name EQ 'MSEG-LGNUM' OR screen-name EQ 'MSEG-LGNUM_1'.
      screen-invisible = 1.
      screen-input = 0.  "screen-input value =0 or layout display mode.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

ENDMODULE.                 " control_screen_scr200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  display_scr_200  OUTPUT
*----------------------------------------------------------------------*
MODULE display_scr_200 OUTPUT.
  w_index = w_top_line + sy-stepl - 1.
  READ TABLE  itab INDEX w_index.

  IF sy-subrc EQ 0.
    MOVE-CORRESPONDING itab TO mseg.
    MOVE : itab-maktx TO makt-maktx.
  ELSE.
    MOVE : rm07m-werks TO mseg-werks.
    IF mseg-lgort EQ space.
      MOVE : rm07m-lgort TO mseg-lgort.
    ENDIF.
  ENDIF.
*  IF mseg-lgort EQ space.
*    MOVE : rm07m-lgort TO mseg-lgort.
*  ENDIF.

  MOVE : w_index TO mseg-zeile.

*--- set Warehouse Number
  CLEAR : t320.
  CHECK mseg-lgort NE space.
  SELECT SINGLE lgnum INTO mseg-lgnum
                      FROM t320
                     WHERE werks EQ mseg-werks
                       AND lgort EQ mseg-lgort.
ENDMODULE.                 " display_scr_200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  check_lgort  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_lgort INPUT.
*-- mseg-lgnum
  CHECK rm07m-lgort EQ space.

  SELECT SINGLE lgnum INTO mseg-lgnum
                      FROM t320
                     WHERE werks EQ mseg-werks
                       AND lgort EQ mseg-lgort.
ENDMODULE.                 " check_lgort  INPUT
*&---------------------------------------------------------------------*
*&      Form  NEW_ITEMS
*----------------------------------------------------------------------*
FORM new_items.
  w_top_line = w_lines + 1. "NEXT NO.1 START
ENDFORM.                    " NEW_ITEMS
*&---------------------------------------------------------------------*
*&      Form  material_document_posting
*----------------------------------------------------------------------*
FORM material_document_posting.
  PERFORM generate_bdc_data.
  PERFORM call_transaction_mm01.
ENDFORM.                    " material_document_posting
*&---------------------------------------------------------------------*
*&      Form  generate_bdc_data
*----------------------------------------------------------------------*
FORM generate_bdc_data.
  CLEAR : bdc_tab, bdc_tab[].

  DATA :  l_bldat(10),
          l_budat(10),
          l_erfmg(12),
          l_okcode(4).

  WRITE : mkpf-bldat TO l_bldat,
          mkpf-budat TO l_budat.

  PERFORM dynpro USING : 'X'  'SAPMM07M'        '0400',
                         ' '  'MKPF-BLDAT'      l_bldat,
                         ' '  'MKPF-BUDAT'      l_budat,
                         ' '  'RM07M-MTSNR'     rm07m-mtsnr,
                         ' '  'MKPF-BKTXT'      mkpf-bktxt,
                         ' '  'RM07M-BWARTWA'   rm07m-bwartwa,
                         ' '  'RM07M-SOBKZ'     rm07m-sobkz,
                         ' '  'RM07M-WERKS'     rm07m-werks.

  IF rm07m-grund IS INITIAL.  "Type n  : initial -> space..
    PERFORM dynpro USING : ' '  'RM07M-GRUND'     space.
  ELSE.
    PERFORM dynpro USING : ' '  'RM07M-GRUND'     rm07m-grund.
  ENDIF.

  PERFORM dynpro USING : ' '  'RM07M-LGORT'     rm07m-lgort,
                         ' '  'BDC_OKCODE'      '=NPE'.

  LOOP AT itab.
    MOVE : '=NPE' TO l_okcode.
    AT LAST.
      MOVE : '=BU'  TO l_okcode.
    ENDAT.
    CLEAR : l_erfmg.
    WRITE : itab-erfmg UNIT itab-erfme TO l_erfmg.
    PERFORM dynpro USING : 'X'  'SAPMM07M'        '0410',
                           ' '  'MSEG-MATNR'      itab-matnr,
                           ' '  'MSEG-ERFMG'      l_erfmg,
                           ' '  'MSEG-ERFME'      itab-erfme,
                           ' '  'MSEG-WERKS'      itab-werks,
                           ' '  'MSEG-LGORT'      itab-lgort,
                           ' '  'MSEG-UMLGO'      msegk-umlgo,
                           ' '  'BDC_SUBSCR'      'SAPMM07M',
                           ' '  'BDC_SUBSCR'      'SAPLKACB',
                           ' '  'DKACB-FMORE'     'X',
                           ' '  'BDC_OKCODE'      l_okcode.
    PERFORM dynpro USING : 'X'  'SAPLKACB'        '0002',
                           ' '  'BDC_SUBSCR'      'SAPLKACB',
                           ' '  'BDC_OKCODE'      '=ENTE'.
  ENDLOOP.
ENDFORM.                    " generate_bdc_data
*&---------------------------------------------------------------------*
*&      Form  call_transaction_mm01
*----------------------------------------------------------------------*
FORM call_transaction_mm01.
  DATA : it_msg TYPE TABLE OF bdcmsgcoll  WITH HEADER LINE.
  DATA : lw_msg LIKE LINE OF it_msg.

  CALL TRANSACTION 'MB1B'
            USING   bdc_tab
            MODE    gl_mode
            MESSAGES INTO it_msg
            UPDATE   'S'.

  READ TABLE it_msg WITH KEY msgtyp = 'E'.
  IF sy-subrc EQ 0.
    MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    READ TABLE it_msg WITH KEY msgtyp = 'S'
                               msgnr = '060'.
    IF sy-subrc EQ 0.
      MESSAGE s999(zmmm) WITH text-m06.

      CLEAR : itab, itab[].

      LEAVE TO TRANSACTION 'YEMM_MB1B'.

    ELSE.
      MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.
ENDFORM.                    " call_transaction_mm01
*&---------------------------------------------------------------------*
*&      Form  dynpro
*----------------------------------------------------------------------*
FORM dynpro  USING    dynbegin  pa_name  value.
  IF dynbegin = 'X'.
    CLEAR : bdc_tab.
    MOVE : pa_name  TO bdc_tab-program,
           value    TO bdc_tab-dynpro,
           dynbegin TO bdc_tab-dynbegin.
    APPEND bdc_tab.
  ELSE.
    CLEAR : bdc_tab.
    MOVE : pa_name  TO bdc_tab-fnam,
           value    TO bdc_tab-fval.
    APPEND bdc_tab.
  ENDIF.
ENDFORM.                    " dynpro
*&---------------------------------------------------------------------*
*&      Module  display_count_scr200  OUTPUT
*----------------------------------------------------------------------*
MODULE display_count_scr200 OUTPUT.

  DESCRIBE TABLE itab LINES w_lines.

  MOVE :  w_lines     TO rm07m-posnm,
          w_top_line  TO rm07m-posnr.

ENDMODULE.                 " display_count_scr200  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  page_scroll
*----------------------------------------------------------------------*
FORM page_scroll.
  DATA : lw_itab_lines  LIKE sy-tabix. "ITAB-TABIX

  DESCRIBE TABLE itab LINES lw_itab_lines.

  CASE sy-ucomm.

    WHEN 'P+'.
      w_top_line = w_top_line + 10.

      IF w_top_line > lw_itab_lines.
        w_top_line = lw_itab_lines.
      ENDIF.



    WHEN 'P++'.
      w_top_line = lw_itab_lines.



    WHEN 'P-'.
      w_top_line = w_top_line - 10.

      IF w_top_line < 1.

        w_top_line = 1.
      ENDIF.



    WHEN 'P--'.
      w_top_line = 1.

  ENDCASE.

  MOVE :  w_lines     TO rm07m-posnm,
          w_top_line  TO rm07m-posnr.

  w_save_okcode = sy-ucomm.
  w_lines = lw_itab_lines.
ENDFORM.                    " page_scroll
