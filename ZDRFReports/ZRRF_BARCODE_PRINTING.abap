************************************************************************
* Program Name      : ZRRF_BARCODE_PRINTING
* Author            : Bongsoo, Kim
* Creation Date     : 2004.07.21.
* Specifications By : Bongsoo, Kim
* Development Request No : UD1K911210
* Addl Documentation:
* Description       : Bar code label printing
*
* Modification Logs
* Date       Developer    RequestNo    Description
*07/21/2004  ZDRF         UD1K911525  [BAR CODE PROGRAM]07/21/2004 08:14
*07/27/2004  ZDRF         UD1K911648  [RF_PM]07/28/2004 17:35:38
*10/18/2006  HASEEB       UD1K922618  Print Storage Bin location on code
*
************************************************************************
REPORT zrrf_barcode_printing
                NO STANDARD PAGE HEADING
                LINE-SIZE  1023
                LINE-COUNT 65
                MESSAGE-ID zmrf.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: mard,
        marc,
        mara,
        makt,
        t001l,
        t001w,
        zsrf_barcode.
*----------------------------------------------------------------------*
* INTERNAL TABLES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* DATA
*----------------------------------------------------------------------*
DATA: BEGIN OF it_mard OCCURS 0,
        matnr TYPE mard-matnr,
        maktx TYPE makt-maktx,
        barno TYPE zrf_barno,
        lgort TYPE mard-lgort,
        mblnr TYPE mseg-mblnr,
* HASEEB  UD1K922618  10/18/2006
        lgpbe TYPE mard-lgpbe,
* HASEEB  UD1K922618  10/18/2006
        chec(1),
        werks TYPE t001w-werks,
      END OF it_mard.
DATA: ok_code TYPE sy-ucomm,
      okcode  TYPE sy-ucomm.
DATA: rdo1(1),
      rdo2(1),
      wa_field(20).
*----------------------------------------------------------------------*
* DECLARATION FOR SEARCH HELP
*----------------------------------------------------------------------*
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
*----------------------------------------------------------------------*
* TABLE CONTROLS
*----------------------------------------------------------------------*
CONTROLS tc_bar TYPE TABLEVIEW USING SCREEN 9000.

*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-001.
PARAMETERS: p_rdo1 RADIOBUTTON GROUP r1 DEFAULT 'X'
                             USER-COMMAND sy-ucomm,
            p_rdo2 RADIOBUTTON GROUP r1.
SELECTION-SCREEN END   OF BLOCK b2.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-002.
PARAMETERS: p_mtart TYPE mara-mtart  OBLIGATORY DEFAULT 'ERSA'
                                       MODIF ID gr1.
SELECT-OPTIONS: s_matnr FOR mara-matnr NO INTERVALS NO-EXTENSION
                                       MODIF ID gr1,
                s_werks FOR t001w-werks  OBLIGATORY DEFAULT 'P001'
                           NO INTERVALS NO-EXTENSION MODIF ID gr1,
                s_lgort FOR t001l-lgort   DEFAULT 'P600'
                           NO INTERVALS NO-EXTENSION MODIF ID gr1,
                s_ersda FOR mard-ersda MODIF ID gr1,
                s_lgpbe FOR mard-lgpbe MODIF ID gr1.

PARAMETERS:     p_mblnr LIKE mseg-mblnr MODIF ID gr2 ,
                p_lgort LIKE t001l-lgort MODIF ID gr2,
                p_budat LIKE mkpf-budat  OBLIGATORY MODIF ID gr2
                             DEFAULT sy-datum.
SELECTION-SCREEN END   OF BLOCK b1.


*----------------------------------------------------------------------*
* EVENT
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM screen_modify.
*-----> AT SELECTION-SCREEN ON VALUE-REQUEST
AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_werks-low.
  PERFORM help_request_s_werks.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_lgort-low.
  PERFORM help_request_s_lgort.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_lgort.
  PERFORM help_request_p_lgort.

START-OF-SELECTION.
  PERFORM read_process.
  IF NOT it_mard[] IS INITIAL.
    CALL SCREEN 9000.
  ENDIF.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  HELP_REQUEST_S_LGORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM help_request_s_lgort.
  DATA: BEGIN OF lt_t001l OCCURS 0,
          werks TYPE t001l-werks,
          lgort TYPE t001l-lgort,
          lgobe TYPE t001l-lgobe,
        END OF lt_t001l.
  DATA l_tabix TYPE sy-tabix.
  CLEAR dynpread. REFRESH dynpread.
  CLEAR valuetab. REFRESH valuetab.
  CLEAR fields.   REFRESH fields.

  PERFORM value_read USING: 'S_WERKS-LOW'.
  LOOP AT dynpread.
    CASE sy-tabix.
      WHEN 1. s_werks-low = dynpread-fieldvalue.
    ENDCASE.
  ENDLOOP.

  SELECT werks
         lgort
         lgobe
         INTO TABLE lt_t001l
         FROM t001l
         WHERE werks EQ s_werks-low.
  IF sy-subrc EQ 0.
    LOOP AT lt_t001l.
      l_tabix = sy-tabix.
      IF lt_t001l-werks EQ 'P001'.
        IF NOT lt_t001l-lgort BETWEEN 'P600' AND 'P699'.
          DELETE lt_t001l INDEX l_tabix.
        ENDIF.
      ELSEIF lt_t001l-werks EQ 'E001'.
        IF NOT lt_t001l-lgort BETWEEN 'E600' AND 'E699'.
          DELETE lt_t001l INDEX l_tabix.
        ENDIF.
** Furong on 02/08/12
      ELSEIF lt_t001l-werks EQ 'E002'.
        IF NOT lt_t001l-lgort BETWEEN 'N600' AND 'N699'.
          DELETE lt_t001l INDEX l_tabix.
        ENDIF.
** end on 02/08/12
      ELSE.
        DELETE lt_t001l INDEX l_tabix.
      ENDIF.
    ENDLOOP.
  ENDIF.

  LOOP AT lt_t001l.
    valuetab-value = lt_t001l-werks.
    APPEND valuetab. CLEAR valuetab.
    valuetab-value = lt_t001l-lgort.
    APPEND valuetab. CLEAR valuetab.
    valuetab-value = lt_t001l-lgobe.
    APPEND valuetab. CLEAR valuetab.
  ENDLOOP.

  PERFORM add_fields USING: 'T001L' 'WERKS' ' ',
                            'T001L' 'LGORT' 'X',
                            'T001L' 'LGOBE' ' '.
  PERFORM help_values_get.


  IF select_index > 0.
    READ TABLE lt_t001l   INDEX select_index.
    PERFORM value_update USING:
            'X'   'S_LGORT-LOW' lt_t001l-lgort 0.
  ENDIF.
ENDFORM.                    " HELP_REQUEST_S_LGORT
*&---------------------------------------------------------------------*
*&      Form  VALUE_READ
*&---------------------------------------------------------------------*
FORM value_read USING  p_name.
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
*&      Form  ADD_FIELDS
*&---------------------------------------------------------------------*
FORM add_fields USING  p_tabname p_fieldname p_flag.
  fields-tabname = p_tabname.
  fields-fieldname = p_fieldname.
  fields-selectflag = p_flag.
  APPEND fields.      CLEAR fields.
ENDFORM.                    " ADD_FIELDS
*&---------------------------------------------------------------------*
*&      Form  HELP_VALUES_GET
*&---------------------------------------------------------------------*
FORM help_values_get.
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
ENDFORM.                    " HELP_VALUES_GET
*&---------------------------------------------------------------------*
*&      Form  VALUE_UPDATE
*&---------------------------------------------------------------------*
FORM value_update USING  p_process
                         p_fieldname
                         p_fieldvalue
                         p_stepl.
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

ENDFORM.                    " VALUE_UPDATE
*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
FORM read_process.
  IF p_rdo1 EQ 'X'.
    PERFORM read_mseg.

  ELSEIF p_rdo2 EQ 'X'.
    PERFORM read_mard.
  ENDIF.
ENDFORM.                    " READ_PROCESS
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS '9000'.
  SET TITLEBAR '9000'.

ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  ok_code = okcode.

  CASE ok_code.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  ok_code = okcode.
  CLEAR okcode.
  CASE ok_code.
    WHEN 'PRINT'.
      PERFORM print_process.
    WHEN 'SORTA'.
      PERFORM sorting USING ok_code+4(1).
    WHEN 'SORTD'.
      PERFORM sorting USING ok_code+4(1).
    WHEN 'SELA'.
      PERFORM select_all.
    WHEN 'DSEL'.
      PERFORM deselect_all.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Form  PRINT_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM print_process.
  CASE 'X'.
    WHEN rdo1.
      PERFORM bic_size_bar_code.

    WHEN rdo2.
      PERFORM small_size_bar_code.

  ENDCASE.
ENDFORM.                    " PRINT_PROCESS
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_screen OUTPUT.
  IF p_rdo2 EQ 'X'.
    LOOP AT SCREEN.
      CASE screen-group1.
        WHEN 'GR2'.
          screen-active = 0.
      ENDCASE.
      MODIFY SCREEN.
      CLEAR screen.

    ENDLOOP.
  ENDIF.
ENDMODULE.                 " MODIFY_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY  OUTPUT
*&---------------------------------------------------------------------*
MODULE display OUTPUT.
  MOVE-CORRESPONDING it_mard TO zsrf_barcode.
ENDMODULE.                 " DISPLAY  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify INPUT.
  MOVE-CORRESPONDING zsrf_barcode TO it_mard.
  MODIFY it_mard INDEX tc_bar-current_line.

ENDMODULE.                 " MODIFY  INPUT
*&---------------------------------------------------------------------*
*&      Form  BAR_CODE_PRINT_BIG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_MATNR  text
*      -->P_LEN  text
*      -->P_IT_MARD_MAKTX  text
*----------------------------------------------------------------------*
FORM bar_code_print_big USING    p_matnr
                                 p_maktx
                                 p_lgpbe.
  DATA: l_matnr(20).
  DATA: len  TYPE i,
        len1 TYPE i,
        len2 TYPE i.

*  CONCATENATE '*' P_MATNR '*' INTO L_MATNR .
*  LEN  = STRLEN( L_MATNR ).
  len  = strlen( p_matnr ).

  len1 = strlen( p_maktx ).
  len2 = 40 - len1.
  CLEAR len1.
  IF len2 NE 0.
    len1 = len2 DIV 2 .
  ENDIF.
  DO len1 TIMES.
    CONCATENATE ' ' p_maktx INTO p_maktx SEPARATED BY space.
  ENDDO.

  WRITE: / '^XA' NO-GAP, "XA- Label Start
             '^LL352',
             '^MD15',
           '^FO' NO-GAP, " FO- Field Orign
*             '120' NO-GAP,  " X
             '96' NO-GAP,  " X
             ',' NO-GAP,
*             '80' NO-GAP,   " Y
             '36' NO-GAP,   " Y

           '^BY' NO-GAP, "BY- Barcode
             '3.3' NO-GAP,  "
             ',' NO-GAP,
             '2.2' NO-GAP,  "

             ',' NO-GAP,
             '120' NO-GAP,  "BAR

              '^A0' NO-GAP,
              ',',
              'N' NO-GAP,
              ',' NO-GAP,
              '40' NO-GAP,
              ',' NO-GAP,
              '120' NO-GAP,
** CODE 128
             '^BCN' NO-GAP,  "BCN -
               ',' NO-GAP,
               '160' NO-GAP,   "
               ',' NO-GAP,
               'N' NO-GAP,  "BAR
               ',' NO-GAP,
               'N' NO-GAP,    "BAR
               ',' NO-GAP,
               'N' NO-GAP,    "BAR
            '^FD' NO-GAP, " FD- Field definition
              p_matnr(len) NO-GAP,
*              L_MATNR(LEN) NO-GAP,
            '^FS' NO-GAP,
* MATERIAL CODE WRITE
         /  '^FO' NO-GAP,
*              '292' NO-GAP,
              '268' NO-GAP,
              ',' NO-GAP,
*              '250' NO-GAP,
              '206' NO-GAP,

              '^A0' NO-GAP,
              ',',
              'N' NO-GAP,
              ',' NO-GAP,
              '40' NO-GAP,
              ',' NO-GAP,
              '120' NO-GAP,

             '^FD' NO-GAP,
               p_matnr,
             '^FS' NO-GAP,
* DESCRIPTION WRITE
         /  '^FO' NO-GAP,
*              '190' NO-GAP,
              '166' NO-GAP,
              ',' NO-GAP,
*              '290' NO-GAP,
              '246' NO-GAP,

              '^A0' NO-GAP,
                ',',
                'N' NO-GAP,
                ',' NO-GAP,
                '35' NO-GAP,
                ',' NO-GAP,
                '35' NO-GAP,

             '^FD' NO-GAP,
               p_maktx,
             '^FS' NO-GAP,
* HASEEB  UD1K922618  10/18/2006
* BIN LOCATION PRINT
         /  '^FO' NO-GAP,
              '166' NO-GAP,
              ',' NO-GAP,
              '270' NO-GAP,

              '^A0' NO-GAP,
                ',',
                'N' NO-GAP,
                ',' NO-GAP,
                '35' NO-GAP,
                ',' NO-GAP,
                '35' NO-GAP,

             '^FD' NO-GAP,
               p_lgpbe,
             '^FS' NO-GAP,

* HASEEB  UD1K922618  10/18/2006
           / '^XZ' NO-GAP.

ENDFORM.                    " BAR_CODE_PRINT_BIG
*&---------------------------------------------------------------------*
*&      Form  BIC_SIZE_BAR_CODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bic_size_bar_code.
  LOOP AT it_mard WHERE chec EQ 'X'.

    DO it_mard-barno TIMES.
      NEW-PAGE PRINT ON
         NO-TITLE
         NO-HEADING
         LINE-SIZE 255
         DESTINATION 'RFL'
         IMMEDIATELY 'X'
         KEEP IN SPOOL 'X'
         NEW LIST IDENTIFICATION 'X'
         NO DIALOG.

*      PERFORM BAR_CODE_PRINT_BIG USING IT_MARD-MATNR
*                                       IT_MARD-MAKTX.
* HASEEB  UD1K922618  10/18/2006
      PERFORM bar_code_print_big USING it_mard-matnr
                                       it_mard-maktx
                                       it_mard-lgpbe.

* HASEEB  UD1K922618  10/18/2006

      NEW-PAGE PRINT OFF.
    ENDDO.
*    MESSAGE S001 WITH 'Bar code printing'.
  ENDLOOP.

ENDFORM.                    " BIC_SIZE_BAR_CODE
*&---------------------------------------------------------------------*
*&      Form  SMALL_SIZE_BAR_CODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM small_size_bar_code.
  LOOP AT it_mard WHERE chec EQ 'X'.
    DO it_mard-barno TIMES.
      NEW-PAGE PRINT ON
         NO-TITLE
         NO-HEADING
         LINE-SIZE 255
         DESTINATION 'RFL'
         IMMEDIATELY 'X'
         KEEP IN SPOOL 'X'
         NEW LIST IDENTIFICATION 'X'
         NO DIALOG.

*         PERFORM BAR_CODE_PRINT_SMALL USING IT_MARD-MATNR
*                                         IT_MARD-MAKTX.
* HASEEB  UD1K922618  10/18/2006
      PERFORM bar_code_print_small USING it_mard-matnr
                                         it_mard-maktx
                                         it_mard-lgpbe.
* HASEEB  UD1K922618  10/18/2006

      NEW-PAGE PRINT OFF.
    ENDDO.
*    MESSAGE S001 WITH 'Bar code printing'.
  ENDLOOP.

ENDFORM.                    " SMALL_SIZE_BAR_CODE
*&---------------------------------------------------------------------*
*&      Form  SELECT_ALL
*&---------------------------------------------------------------------*
FORM select_all.
  it_mard-chec = 'X'.
  MODIFY it_mard TRANSPORTING chec
                       WHERE chec EQ space.
ENDFORM.                    " SELECT_ALL
*&---------------------------------------------------------------------*
*&      Form  DESELECT_ALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM deselect_all.
  it_mard-chec = ' '.
  MODIFY it_mard TRANSPORTING chec
                       WHERE chec EQ 'X'.
ENDFORM.                    " DESELECT_ALL
*&---------------------------------------------------------------------*
*&      Form  SORTING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_OK_CODE+4(1)  text
*----------------------------------------------------------------------*
FORM sorting USING    p_code.
  GET CURSOR FIELD wa_field.

  CASE p_code.
    WHEN 'A'.
      CASE wa_field.
        WHEN 'ZSRF_BARCODE-MATNR'.
          SORT it_mard BY matnr.
        WHEN 'ZSRF_BARCODE-MAKTX'.
          SORT it_mard BY maktx matnr.
        WHEN 'ZSRF_BARCODE-BARNO'.
          SORT it_mard BY barno matnr.
        WHEN 'ZSRF_BARCODE-LGORT'.
          SORT it_mard BY lgort matnr mblnr.
        WHEN 'ZSRF_BARCODE-MBLNR'.
          SORT it_mard BY mblnr matnr lgort.
      ENDCASE.
    WHEN 'D'.
      CASE wa_field.
        WHEN 'ZSRF_BARCODE-MATNR'.
          SORT it_mard BY matnr DESCENDING.
        WHEN 'ZSRF_BARCODE-MAKTX'.
          SORT it_mard BY maktx DESCENDING
                          matnr.
        WHEN 'ZSRF_BARCODE-BARNO'.
          SORT it_mard BY barno DESCENDING
                          matnr.
        WHEN 'ZSRF_BARCODE-LGORT'.
          SORT it_mard BY lgort DESCENDING.
        WHEN 'ZSRF_BARCODE-MBLNR'.
          SORT it_mard BY mblnr DESCENDING
                          matnr lgort.
      ENDCASE.
  ENDCASE.

ENDFORM.                    " SORTING
*&---------------------------------------------------------------------*
*&      Form  BAR_CODE_PRINT_SMALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_MARD_MATNR  text
*      -->P_IT_MARD_MAKTX  text
*----------------------------------------------------------------------*
FORM bar_code_print_small USING    p_matnr
                                   p_maktx
                                   p_lgpbe.

  DATA: l_matnr(20).
  DATA: len  TYPE i,
        len1 TYPE i,
        len2 TYPE i.

*  LEN  = STRLEN( P_MATNR ).
  CONCATENATE '*' p_matnr '*' INTO l_matnr .
  len = strlen( l_matnr ).
  len1 = strlen( p_maktx ).
  len2 = 40 - len1.
  CLEAR len1.
  IF len2 NE 0.
    len1 = len2 DIV 2 .
  ENDIF.
  DO len1 TIMES.
    CONCATENATE ' ' p_maktx INTO p_maktx SEPARATED BY space.
  ENDDO.

  WRITE: / '^XA' NO-GAP,
             '^LL352',
           '^FO' NO-GAP,
*             '120' NO-GAP,  " X
             '40' NO-GAP,  " X
             ',' NO-GAP,
*             '80' NO-GAP,   " Y
             '40' NO-GAP,   " Y

*           '^BY' NO-GAP,
*             '2.5' NO-GAP,  "
*             ',' NO-GAP,
*             '2.5' NO-GAP,  "
** CODE 128
*             ',' NO-GAP,
*             '120' NO-GAP,  "BAR
             '^BY' NO-GAP,
               '2.4' NO-GAP,  "
               ',' NO-GAP,
               '2.3' NO-GAP,  "

               ',' NO-GAP,
               '160' NO-GAP,  "BAR
** CODE 128
              '^AE' NO-GAP,
                'N' NO-GAP,
                ',' NO-GAP,
                '10' NO-GAP,
                ',' NO-GAP,
                '10' NO-GAP,

*              '^A0' NO-GAP,
*                ',',
*              'N' NO-GAP,
*              ',' NO-GAP,
*              '40' NO-GAP,
*              ',' NO-GAP,
*              '48' NO-GAP,
** CODE 128
             '^BCN' NO-GAP,  " N"
               ',' NO-GAP,
               '120' NO-GAP,   "M
               ',' NO-GAP,
               'N' NO-GAP,  "BAR
               ',' NO-GAP,
               'N' NO-GAP,    "BAR
               ',' NO-GAP,
               'N' NO-GAP,    "BAR

            '^FD' NO-GAP,
*              L_MATNR(LEN) NO-GAP,
              l_matnr(len) NO-GAP,
            '^FS' NO-GAP,
* MATERIAL CODE WRITE
         /  '^FO' NO-GAP,
*              '190' NO-GAP,
              '110' NO-GAP,
              ',' NO-GAP,
*              '210' NO-GAP,
              '170' NO-GAP,
** CODE 128
              '^A0' NO-GAP,
                ',',
              'N' NO-GAP,
              ',' NO-GAP,
              '40' NO-GAP,
              ',' NO-GAP,
              '48' NO-GAP,

             '^FD' NO-GAP,
               p_matnr,
             '^FS' NO-GAP,
* DESCRIPTION WRITE
         /  '^FO' NO-GAP,
*              '135' NO-GAP,
              '55' NO-GAP,
              ',' NO-GAP,
*              '260' NO-GAP,
              '220' NO-GAP,
** CODE 128
*              '^AD' NO-GAP,
*                'N' NO-GAP,
*                ',' NO-GAP,
*                '10' NO-GAP,
*                ',' NO-GAP,
*                '10' NO-GAP,
              '^A0' NO-GAP,
                ',',
              'N' NO-GAP,
              ',' NO-GAP,
              '30' NO-GAP,
              ',' NO-GAP,
              '38' NO-GAP,

             '^FD' NO-GAP,
               p_maktx,
             '^FS' NO-GAP,
* HASEEB  UD1K922618  10/18/2006
* BIN LOCATION PRINT
         /  '^FO' NO-GAP,
              '55' NO-GAP,
              ',' NO-GAP,
              '260' NO-GAP,
              '^A0' NO-GAP,
                ',',
              'N' NO-GAP,
              ',' NO-GAP,
              '30' NO-GAP,
              ',' NO-GAP,
              '38' NO-GAP,

             '^FD' NO-GAP,
               p_lgpbe,
             '^FS' NO-GAP,

* HASEEB  UD1K922618  10/18/2006
           / '^XZ' NO-GAP.

ENDFORM.                    " BAR_CODE_PRINT_SMALL
*&---------------------------------------------------------------------*
*&      Form  SCREEN_MODIFY
*&---------------------------------------------------------------------*
FORM screen_modify.
  IF p_rdo2 EQ 'X'.
    LOOP AT SCREEN.
*    IF SCREEN-NAME  EQ 'P_RDO1'.
      IF screen-group1 = 'GR2'.
*        SCREEN-INPUT   = 0.
        screen-active  = 0.
      ENDIF.
      MODIFY SCREEN.
      CLEAR screen.
    ENDLOOP.
  ELSEIF p_rdo1 EQ 'X'.
    LOOP AT SCREEN.
*    IF SCREEN-NAME  EQ 'P_RDO1'.
      IF screen-group1 = 'GR1'.
*        SCREEN-INPUT   = 0.
        screen-active  = 0.
      ENDIF.
      MODIFY SCREEN.
      CLEAR screen.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " SCREEN_MODIFY
*&---------------------------------------------------------------------*
*&      Form  READ_MARD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_mard.
  DATA: BEGIN OF lt_mara OCCURS 0,
          matnr TYPE mard-matnr,
          maktx TYPE makt-maktx,
          barno TYPE zrf_barno,
          lgort TYPE mard-lgort,
          mblnr TYPE mseg-mblnr,
          chec(1),
          werks TYPE t001w-werks,
        END OF lt_mara.
  DATA: BEGIN OF lt_mard OCCURS 0,
          matnr TYPE mard-matnr,
          werks TYPE t001w-werks,
          lgort TYPE mard-lgort,
*         HASEEB  UD1K922618  10/18/2006
          lgpbe TYPE mard-lgpbe,
*         HASEEB  UD1K922618  10/18/2006
        END OF lt_mard.

  SELECT a~matnr
         c~maktx
         d~werks
       FROM mara AS a INNER JOIN marc AS d
                      ON a~matnr EQ d~matnr
                      INNER JOIN makt AS c
                      ON  a~matnr EQ c~matnr
                      AND c~spras EQ sy-langu
       INTO CORRESPONDING FIELDS OF TABLE lt_mara
       WHERE a~mtart EQ p_mtart
       AND   d~werks IN s_werks
       AND   a~ersda IN s_ersda
       AND   a~matnr IN s_matnr.

  IF sy-subrc EQ 0.
    IF NOT s_lgort-low IS INITIAL.
      SELECT matnr
             werks
             lgort
* HASEEB  UD1K922618  10/18/2006
             lgpbe
* HASEEB  UD1K922618  10/18/2006
           FROM mard
           INTO TABLE lt_mard
           FOR ALL ENTRIES IN lt_mara
           WHERE matnr EQ lt_mara-matnr
           AND   werks EQ lt_mara-werks
           AND   lgort EQ s_lgort-low
** Furong on 03/01/12
           and   LGPBE IN s_LGPBE.
** on 03/01/12
      IF sy-subrc EQ 0.
        LOOP AT lt_mard.
          READ TABLE lt_mara WITH KEY matnr = lt_mard-matnr
                                      werks = lt_mard-werks.
          IF sy-subrc EQ 0.
            MOVE-CORRESPONDING lt_mara TO it_mard.
            it_mard-lgort = lt_mard-lgort.
* HASEEB  UD1K922618  10/18/2006
            it_mard-lgpbe = lt_mard-lgpbe.
* HASEEB  UD1K922618  10/18/2006
            APPEND it_mard. CLEAR it_mard.
          ELSE.

          ENDIF.

        ENDLOOP.
      ENDIF.
    ELSE.
      it_mard[] = lt_mara[].
    ENDIF.
    SORT it_mard BY matnr .
    it_mard-barno = 1.
    MODIFY it_mard TRANSPORTING barno
                   WHERE barno NE 1.

    IF NOT s_lgort-low IS INITIAL.
      DELETE it_mard WHERE werks NE s_werks-low
                     OR    lgort NE s_lgort-low.
    ENDIF.


    IF it_mard[] IS INITIAL.
      WRITE: / 'NO DATA'.
    ENDIF.

  ELSE.
    WRITE: / 'NO DATA'.
  ENDIF.
ENDFORM.                    " READ_MARD
*&---------------------------------------------------------------------*
*&      Form  READ_MSEG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_mseg.
  RANGES: r_lgort FOR mseg-lgort,
          r_mblnr FOR mseg-mblnr,
          r_bwart FOR mseg-bwart.
  DATA: BEGIN OF lt_mard OCCURS 0,
          matnr TYPE mard-matnr,
          maktx TYPE makt-maktx,
          menge TYPE mseg-menge,
          lgort TYPE mard-lgort,
          mblnr TYPE mseg-mblnr,
          chec(1),
        END OF lt_mard.
  IF p_lgort IS INITIAL.
    r_lgort-low    = 'P600'.
    r_lgort-high   = 'P699'.
    r_lgort-sign   = 'I'.
    r_lgort-option = 'BT'.
    APPEND r_lgort. CLEAR r_lgort.
    r_lgort-low    = 'E600'.
    r_lgort-high   = 'E699'.
    r_lgort-sign   = 'I'.
    r_lgort-option = 'BT'.
    APPEND r_lgort. CLEAR r_lgort.
  ELSE.
    r_lgort-low    = p_lgort.
    r_lgort-sign   = 'I'.
    r_lgort-option = 'EQ'.
    APPEND r_lgort.
  ENDIF.
  IF p_mblnr IS INITIAL.

  ELSE.
    r_mblnr-low    = p_mblnr.
    r_mblnr-sign   = 'I'.
    r_mblnr-option = 'EQ'.
    APPEND r_mblnr.
  ENDIF.
  r_bwart-low    = '101'.
  r_bwart-sign   = 'I'.
  r_bwart-option = 'EQ'.
  APPEND r_bwart.
  r_bwart-low    = '511'.
  r_bwart-sign   = 'I'.
  r_bwart-option = 'EQ'.
  APPEND r_bwart.


  SELECT a~mblnr
         b~matnr
         c~maktx
         b~lgort
         b~menge
       FROM mkpf AS a INNER JOIN mseg AS b
                      ON a~mblnr EQ b~mblnr
                      INNER JOIN makt AS c
                      ON b~matnr EQ c~matnr
       INTO CORRESPONDING FIELDS OF TABLE lt_mard
        WHERE a~budat EQ p_budat
        AND   b~lgort IN r_lgort
        AND   a~mblnr IN r_mblnr
        AND   b~bwart IN r_bwart.
*        AND   B~BWART EQ '101'.
*        AND   B~BWART EQ '511'.
  IF sy-subrc EQ 0.
    LOOP AT lt_mard.
      MOVE-CORRESPONDING lt_mard TO it_mard.
      it_mard-barno = lt_mard-menge.
      APPEND it_mard. CLEAR it_mard.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " READ_MSEG
*&---------------------------------------------------------------------*
*&      Form  HELP_REQUEST_P_LGORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM help_request_p_lgort.
  DATA: BEGIN OF lt_t001l OCCURS 0,
          werks TYPE t001l-werks,
          lgort TYPE t001l-lgort,
          lgobe TYPE t001l-lgobe,
        END OF lt_t001l.
  DATA l_tabix TYPE sy-tabix.
  RANGES rl_werks FOR t001w-werks.
  CLEAR dynpread. REFRESH dynpread.
  CLEAR valuetab. REFRESH valuetab.
  CLEAR fields.   REFRESH fields.

*  PERFORM VALUE_READ USING: 'S_WERKS-LOW'.
*  LOOP AT DYNPREAD.
*    CASE SY-TABIX.
*      WHEN 1. S_WERKS-LOW = DYNPREAD-FIELDVALUE.
*    ENDCASE.
*  ENDLOOP.
  rl_werks-low    = 'P001'.
  rl_werks-sign   = 'I'.
  rl_werks-option = 'EQ'.
  APPEND rl_werks.
  rl_werks-low    = 'E001'.
  rl_werks-sign   = 'I'.
  rl_werks-option = 'EQ'.
  APPEND rl_werks.

  rl_werks-low    = 'E002'.
  rl_werks-sign   = 'I'.
  rl_werks-option = 'EQ'.
  APPEND rl_werks.

  SELECT werks
         lgort
         lgobe
         INTO TABLE lt_t001l
         FROM t001l
         WHERE werks IN rl_werks.
  IF sy-subrc EQ 0.
    LOOP AT lt_t001l.
      l_tabix = sy-tabix.
      IF lt_t001l-werks EQ 'P001'.
        IF NOT lt_t001l-lgort BETWEEN 'P600' AND 'P699'.
          DELETE lt_t001l INDEX l_tabix.
        ENDIF.
      ELSEIF lt_t001l-werks EQ 'E001'.
        IF NOT lt_t001l-lgort BETWEEN 'E600' AND 'E699'.
          DELETE lt_t001l INDEX l_tabix.
        ENDIF.
      ELSEIF lt_t001l-werks EQ 'E002'.
        IF NOT lt_t001l-lgort BETWEEN 'N600' AND 'N699'.
          DELETE lt_t001l INDEX l_tabix.
        ENDIF.

      ELSE.
        DELETE lt_t001l INDEX l_tabix.
      ENDIF.
    ENDLOOP.
  ENDIF.

  LOOP AT lt_t001l.
    valuetab-value = lt_t001l-werks.
    APPEND valuetab. CLEAR valuetab.
    valuetab-value = lt_t001l-lgort.
    APPEND valuetab. CLEAR valuetab.
    valuetab-value = lt_t001l-lgobe.
    APPEND valuetab. CLEAR valuetab.
  ENDLOOP.

  PERFORM add_fields USING: 'T001L' 'WERKS' ' ',
                            'T001L' 'LGORT' 'X',
                            'T001L' 'LGOBE' ' '.
  PERFORM help_values_get.


  IF select_index > 0.
    READ TABLE lt_t001l   INDEX select_index.
    PERFORM value_update USING:
            'X'   'P_LGORT' lt_t001l-lgort 0.
  ENDIF.
ENDFORM.                    " HELP_REQUEST_P_LGORT
*&---------------------------------------------------------------------*
*&      Form  HELP_REQUEST_S_WERKS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM help_request_s_werks.
  DATA: BEGIN OF lt_t001w OCCURS 0,
          werks TYPE t001w-werks,
          name1 TYPE t001w-name1,
        END OF lt_t001w.
  DATA l_tabix TYPE sy-tabix.
  RANGES rl_werks FOR t001w-werks.
  CLEAR dynpread. REFRESH dynpread.
  CLEAR valuetab. REFRESH valuetab.
  CLEAR fields.   REFRESH fields.

*  PERFORM VALUE_READ USING: 'S_WERKS-LOW'.
*  LOOP AT DYNPREAD.
*    CASE SY-TABIX.
*      WHEN 1. S_WERKS-LOW = DYNPREAD-FIELDVALUE.
*    ENDCASE.
*  ENDLOOP.
  rl_werks-low    = 'P001'.
  rl_werks-sign   = 'I'.
  rl_werks-option = 'EQ'.
  APPEND rl_werks.
  rl_werks-low    = 'E001'.
  rl_werks-sign   = 'I'.
  rl_werks-option = 'EQ'.
  APPEND rl_werks.
  rl_werks-low    = 'E002'.
  rl_werks-sign   = 'I'.
  rl_werks-option = 'EQ'.
  APPEND rl_werks.

  SELECT werks
         name1
         INTO TABLE lt_t001w
         FROM t001w
         WHERE werks IN rl_werks.
*  IF SY-SUBRC EQ 0.
*    LOOP AT LT_T001L.
*      L_TABIX = SY-TABIX.
*      IF LT_T001L-WERKS EQ 'P001'.
*        IF NOT LT_T001L-LGORT BETWEEN 'P600' AND 'P699'.
*          DELETE LT_T001L INDEX L_TABIX.
*        ENDIF.
*      ELSEIF LT_T001L-WERKS EQ 'E001'.
*        IF NOT LT_T001L-LGORT BETWEEN 'E600' AND 'E699'.
*          DELETE LT_T001L INDEX L_TABIX.
*        ENDIF.
*      ELSE.
*        DELETE LT_T001L INDEX L_TABIX.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.

  LOOP AT lt_t001w.
    valuetab-value = lt_t001w-werks.
    APPEND valuetab. CLEAR valuetab.
    valuetab-value = lt_t001w-name1.
    APPEND valuetab. CLEAR valuetab.
  ENDLOOP.

  PERFORM add_fields USING: 'T001W' 'WERKS' 'X',
                            'T001W' 'NAME1' ' '.
  PERFORM help_values_get.


  IF select_index > 0.
    READ TABLE lt_t001w   INDEX select_index.
    PERFORM value_update USING:
            'X'   'S_WERKS-LOW' lt_t001w-werks 0.
  ENDIF.
ENDFORM.                    " HELP_REQUEST_S_WERKS
