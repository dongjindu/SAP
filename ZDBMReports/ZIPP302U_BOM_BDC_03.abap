************************************************************************
* Program Name      : ZIPP302U_BOM_BDC_03
* Author            : Bongsoo, Kim
* Creation Date     : 2004.03.25.
* Specifications By : Bongsoo, Kim
* Pattern           : 2.1
* Development Request No : UD1K907658
* Addl Documentation:
* Description       : BOM Structure VERSION 3
*
* Modification Logs
* Date       Developer    RequestNo    Description
*2004.04.01  ZDBM         UD1K908978
*
*
************************************************************************
REPORT zipp302u_bom_bdc_03
                NO STANDARD PAGE HEADING
                LINE-SIZE  1023
                LINE-COUNT 65
                MESSAGE-ID zmbm.
*----------------------------------------------------------------------*
* INCLUDE
*----------------------------------------------------------------------*
INCLUDE zipp302l_bom_bdc_03_t.
INCLUDE zipp302l_bom_bdc_03_f01.
*----------------------------------------------------------------------*
* EVENT
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM initialization.

AT SELECTION-SCREEN OUTPUT.
  PERFORM screen_modify.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM at_sel_screen_on_value_request USING p_file 'O'.

START-OF-SELECTION.

* TABLE SELECTION DATA BDC
  PERFORM table_selection_data_bdc.
*&---------------------------------------------------------------------*
*&      Form  CHECK_COLOR_EXIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_BMCO  text
*----------------------------------------------------------------------*
FORM check_color_exist USING    pa_bmco STRUCTURE wa_bmco.
  DATA: lt_stb LIKE stpox OCCURS 0 WITH HEADER LINE.
  DATA: lw_topmat LIKE cstmat.

  CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
       EXPORTING
*            aumng                 = 0
            capid                 = c_capid
*            cuovs                 = '0'
            datuv                 = sy-datum
*            mktls                 = 'X'
            cuobj                 = '999999999999999999'
            mtnrv                 = pa_bmco-mtno
*            stpst                 = 0
            stlan                 = pa_bmco-usag
            stlal                 = pa_bmco-altn
*            svwvo                 = 'X'
            werks                 = pa_bmco-plnt
*            vrsvo                 = 'X'
            mmory = '0'
       IMPORTING
            topmat                = lw_topmat
       TABLES
            stb                   = lt_stb
       EXCEPTIONS
            alt_not_found         = 1
            call_invalid          = 2
            material_not_found    = 3
            missing_authorization = 4
            no_bom_found          = 5
            no_plant_data         = 6
            no_suitable_bom_found = 7
            conversion_error      = 8
            OTHERS                = 9.
  IF sy-subrc NE 0 OR lw_topmat-stlal NE it_bmco-altn.
    MOVE: text-083 TO it_bmco-zmsg,
          'E'      TO it_bmco-zresult,
          'C'      TO it_bmco-zmode.
    EXIT.
  ENDIF.

  LOOP AT lt_stb WHERE idnrk EQ pa_bmco-comp
                   AND suff  EQ pa_bmco-suff.
    IF lw_topmat-stlal EQ pa_bmco-altn.
      MOVE: text-087 TO it_bmco-zmsg,
            'E'      TO it_bmco-zresult,
            'C'      TO it_bmco-zmode.
      EXIT.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " CHECK_COLOR_EXIST
