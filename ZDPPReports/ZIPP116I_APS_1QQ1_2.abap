REPORT zipp116i_aps_1qq1_2 NO STANDARD PAGE HEADING
                          MESSAGE-ID zmpp.
************************************************************************
* Program Name      : ZIPP116I_APS_1QQ1_2
* Author            :
* Creation Date     : 10/15/08
* Specifications By :
* Addl Documentation: APS II - Engine/Transmission Status
* Description       :
*
************************************************************************
* Modification Logs
* Date        Developer    RequestNo    Description
*
************************************************************************

TABLES: ztpp_pvv11qq.

DATA: it_pvv11qq LIKE TABLE OF ztpp_pvv11qq WITH HEADER LINE.

CONSTANTS: c_dest(10) VALUE 'WMPP01'.   "Outbound Interface Destination

************************************************************************
*              SELECTION SCREEN LAYOUT                                 *
************************************************************************
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME.
*PARAMETERS: P_DATE LIKE SY-DATUM.
PARAMETERS: p_werks LIKE t001w-werks OBLIGATORY.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_eai          TYPE c AS CHECKBOX  DEFAULT 'X'.
SELECTION-SCREEN COMMENT  (55) text-100 FOR FIELD p_eai.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK b1.

INITIALIZATION.
*  PERFORM  INITILIZE.

START-OF-SELECTION.
  PERFORM data_select.
  IF it_pvv11qq[] IS INITIAL.
    MESSAGE i001 WITH text-001.
  ELSE.
    IF p_eai = 'X'.
      PERFORM send_data.
    ENDIF.
    PERFORM data_update.
  ENDIF.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  DATA_SELECT
*&---------------------------------------------------------------------*
FORM data_select.
  DATA: BEGIN OF it_vtentries OCCURS 0.
          INCLUDE STRUCTURE vtentries.
  DATA:  column TYPE vtentries-vtlinnoint ,
        col_name TYPE vtentries-vtcharact .
  DATA: END OF it_vtentries.
  DATA: l_full_code LIKE tablstruct-var_tab,
        p_code LIKE vtentries-vtcharact.

  DATA: l_column TYPE vtentries-vtlineno.
  DATA: l_row TYPE vtentries-vtlineno.
  DATA: l_time LIKE sy-uzeit.

  l_time = sy-uzeit.
  CLEAR: it_pvv11qq.
  REFRESH: it_pvv11qq.

  l_full_code = 'EMF_ALC_U_1'.
  p_code = 'P_ALC_U_1'.
  CALL FUNCTION 'CARD_TABLE_READ_ENTRIES'
    EXPORTING
      var_table       = l_full_code
    TABLES
      var_tab_entries = it_vtentries
    EXCEPTIONS
      error           = 1
      OTHERS          = 2.

  LOOP AT it_vtentries.
    IF it_vtentries-vtcharact = p_code.
      it_pvv11qq-uval = it_vtentries-vtvalue.
      SELECT SINGLE SUM( labst ) INTO it_pvv11qq-stoc
        FROM mard
        WHERE matnr = it_pvv11qq-uval
** FOR E002
*          AND WERKS = 'E001'
          AND werks = p_werks
** END
        GROUP BY matnr werks.
      SELECT SINGLE maktx INTO it_pvv11qq-ntyp
        FROM makt
        WHERE matnr = it_pvv11qq-uval
          AND spras = 'E'.
      SELECT SINGLE profl INTO it_pvv11qq-pnat
        FROM mara
        WHERE matnr = it_pvv11qq-uval.
      CASE it_pvv11qq-pnat.
        WHEN 'K'.
          it_pvv11qq-pnat = 'HMC'.
        WHEN 'M'.
          it_pvv11qq-pnat = 'HMMA'.
      ENDCASE.

      PERFORM read_normal_class USING it_pvv11qq-uval
                                            'EN_VEH_MODEL'
                                            CHANGING it_pvv11qq-modl.
      PERFORM read_normal_class USING it_pvv11qq-uval
                                      'EN_SPC01'
                                      CHANGING it_pvv11qq-encp.
      it_pvv11qq-plnt  = '1'.
      it_pvv11qq-etgu  = 'E'.
      it_pvv11qq-stdt = sy-datum+0(6).
      it_pvv11qq-sttm = sy-datum.
      it_pvv11qq-erzet = l_time.
      APPEND it_pvv11qq.
      CLEAR: it_pvv11qq.
    ENDIF.
  ENDLOOP.

  CLEAR it_vtentries.
  REFRESH it_vtentries.

  l_full_code = 'CRA_ALC_U_1'.
  p_code = 'P_ALC_U_1'.
  CALL FUNCTION 'CARD_TABLE_READ_ENTRIES'
    EXPORTING
      var_table       = l_full_code
    TABLES
      var_tab_entries = it_vtentries
    EXCEPTIONS
      error           = 1
      OTHERS          = 2.

  LOOP AT it_vtentries.
    IF it_vtentries-vtcharact = p_code.
      it_pvv11qq-uval = it_vtentries-vtvalue.
      SELECT SINGLE SUM( labst ) INTO it_pvv11qq-stoc
        FROM mard
        WHERE matnr = it_pvv11qq-uval
** FOR E002
*          AND WERKS = 'E001'
          AND werks = p_werks
** END
        GROUP BY matnr werks.
      SELECT SINGLE maktx INTO it_pvv11qq-ntyp
        FROM makt
        WHERE matnr = it_pvv11qq-uval
          AND spras = 'E'.
      SELECT SINGLE profl INTO it_pvv11qq-pnat
        FROM mara
        WHERE matnr = it_pvv11qq-uval.
      CASE it_pvv11qq-pnat.
        WHEN 'K'.
          it_pvv11qq-pnat = 'HMC'.
        WHEN 'M'.
          it_pvv11qq-pnat = 'HMMA'.
      ENDCASE.

      PERFORM read_normal_class USING it_pvv11qq-uval
                                            'EN_VEH_MODEL'
                                            CHANGING it_pvv11qq-modl.
      PERFORM read_normal_class USING it_pvv11qq-uval
                                      'EN_SPC01'
                                      CHANGING it_pvv11qq-encp.
      it_pvv11qq-plnt  = '1'.
      it_pvv11qq-etgu  = 'E'.
      it_pvv11qq-stdt = sy-datum+0(6).
      it_pvv11qq-sttm = sy-datum.
      it_pvv11qq-erzet = l_time.
      APPEND it_pvv11qq.
      CLEAR: it_pvv11qq.
    ENDIF.
  ENDLOOP.


  CLEAR it_vtentries.
  REFRESH it_vtentries.

  l_full_code = 'EMF_ALC_U_2'.
  p_code = 'P_ALC_U_2'.
  CALL FUNCTION 'CARD_TABLE_READ_ENTRIES'
    EXPORTING
      var_table       = l_full_code
    TABLES
      var_tab_entries = it_vtentries
    EXCEPTIONS
      error           = 1
      OTHERS          = 2.

  LOOP AT it_vtentries.
    IF it_vtentries-vtcharact = p_code.
      it_pvv11qq-uval = it_vtentries-vtvalue.
      SELECT SINGLE SUM( labst ) INTO it_pvv11qq-stoc
        FROM mard
        WHERE matnr = it_pvv11qq-uval
** FOR E002
*          AND WERKS = 'E001'
          AND werks = p_werks
** END
        GROUP BY matnr werks.
      SELECT SINGLE maktx INTO it_pvv11qq-ntyp
        FROM makt
        WHERE matnr = it_pvv11qq-uval
          AND spras = 'E'.
      SELECT SINGLE profl INTO it_pvv11qq-pnat
        FROM mara
        WHERE matnr = it_pvv11qq-uval.
      CASE it_pvv11qq-pnat.
        WHEN 'K'.
          it_pvv11qq-pnat = 'HMC'.
        WHEN 'M'.
          it_pvv11qq-pnat = 'HMMA'.
      ENDCASE.

*      PERFORM READ_NORMAL_CLASS USING IT_PVV11QQ-UVAL
*                                            'EN_VEH_MODEL'
*                                            CHANGING IT_PVV11QQ-MODL.
*      PERFORM READ_NORMAL_CLASS USING IT_PVV11QQ-UVAL
*                                      'EN_SPC01'
*                                      CHANGING IT_PVV11QQ-ENCP.
      it_pvv11qq-plnt  = '1'.
      it_pvv11qq-etgu  = 'T'.
      it_pvv11qq-stdt = sy-datum+0(6).
      it_pvv11qq-sttm = sy-datum.
      it_pvv11qq-erzet = l_time.
      APPEND it_pvv11qq.
      CLEAR: it_pvv11qq.
    ENDIF.
  ENDLOOP.

  CLEAR it_vtentries.
  REFRESH it_vtentries.

  l_full_code = 'CRA_ALC_U_2'.
  p_code = 'P_ALC_U_2'.
  CALL FUNCTION 'CARD_TABLE_READ_ENTRIES'
    EXPORTING
      var_table       = l_full_code
    TABLES
      var_tab_entries = it_vtentries
    EXCEPTIONS
      error           = 1
      OTHERS          = 2.

  LOOP AT it_vtentries.
    IF it_vtentries-vtcharact = p_code.
      it_pvv11qq-uval = it_vtentries-vtvalue.
      SELECT SINGLE SUM( labst ) INTO it_pvv11qq-stoc
        FROM mard
        WHERE matnr = it_pvv11qq-uval
** FOR E002
*          AND WERKS = 'E001'
          AND werks = p_werks
** END
        GROUP BY matnr werks.
      SELECT SINGLE maktx INTO it_pvv11qq-ntyp
        FROM makt
        WHERE matnr = it_pvv11qq-uval
          AND spras = 'E'.
      SELECT SINGLE profl INTO it_pvv11qq-pnat
        FROM mara
        WHERE matnr = it_pvv11qq-uval.
      CASE it_pvv11qq-pnat.
        WHEN 'K'.
          it_pvv11qq-pnat = 'HMC'.
        WHEN 'M'.
          it_pvv11qq-pnat = 'HMMA'.
      ENDCASE.

*      PERFORM READ_NORMAL_CLASS USING IT_PVV11QQ-UVAL
*                                            'EN_VEH_MODEL'
*                                            CHANGING IT_PVV11QQ-MODL.
*      PERFORM READ_NORMAL_CLASS USING IT_PVV11QQ-UVAL
*                                      'EN_SPC01'
*                                      CHANGING IT_PVV11QQ-ENCP.
      it_pvv11qq-plnt  = '1'.
      it_pvv11qq-etgu  = 'T'.
      it_pvv11qq-stdt = sy-datum+0(6).
      it_pvv11qq-sttm = sy-datum.
      it_pvv11qq-erzet = l_time.
      APPEND it_pvv11qq.
      CLEAR: it_pvv11qq.
    ENDIF.
  ENDLOOP.
  SORT it_pvv11qq BY uval.
  DELETE ADJACENT DUPLICATES FROM it_pvv11qq COMPARING uval.
ENDFORM.                    " DATA_SELECT

*---------------------------------------------------------------------*
*       FORM read_normal_class                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_VMNO                                                        *
*  -->  P_CHAR                                                        *
*  -->  P_VALUE                                                       *
*---------------------------------------------------------------------*
FORM read_normal_class USING    p_vmno  p_char
                                CHANGING p_value.
  SELECT SINGLE au~atwrt
    INTO p_value
    FROM ausp AS au
      INNER JOIN cabn AS ca ON au~atinn = ca~atinn
    WHERE objek = p_vmno      AND
          klart = '001'       AND
          ca~atnam = p_char  .
ENDFORM.                    " read_normal_classification

*&---------------------------------------------------------------------*
*&      Form  DATA_UPDATE
*&---------------------------------------------------------------------*
FORM data_update.
  DATA: l_text(60) TYPE c,
        l_int TYPE i.

  DELETE FROM ztpp_pvv11qq CLIENT SPECIFIED WHERE mandt = sy-mandt.

  INSERT ztpp_pvv11qq FROM TABLE it_pvv11qq.

  IF sy-subrc = 0.
    DESCRIBE TABLE it_pvv11qq LINES l_int.
    WRITE l_int TO l_text LEFT-JUSTIFIED.
    COMMIT WORK.
    CONCATENATE 'Created Record count :' l_text
      INTO l_text.
    MESSAGE  s001 WITH l_text.
    MESSAGE  s001 WITH text-002.
  ELSE.
    ROLLBACK WORK.
    MESSAGE  w001 WITH text-003.
  ENDIF.
ENDFORM.                    " DATA_UPDATE
*&---------------------------------------------------------------------*
*&      Form  SEND_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_data.
  DATA : l_msgtxt(100),
         l_result(1).

  CALL FUNCTION 'Z_FPP_SET_PVV11QQ_2'
    DESTINATION c_dest
    IMPORTING
      flag                  = l_result
    TABLES
      i_pvv11qq             = it_pvv11qq
    EXCEPTIONS
      communication_failure = 1  MESSAGE l_msgtxt
      system_failure        = 2  MESSAGE l_msgtxt.

  IF l_result = 'S'.
    MESSAGE i001 WITH 'Successfully sent out'.
  ELSE.
    IF l_result IS INITIAL.
      l_result = 'E'.
    ENDIF.
    MESSAGE i001 WITH l_msgtxt.
  ENDIF.
  LOOP AT it_pvv11qq.
    it_pvv11qq-int_flag = l_result.
    MODIFY it_pvv11qq TRANSPORTING int_flag.
  ENDLOOP.
ENDFORM.                    " SEND_DATA
*&---------------------------------------------------------------------*
*&      Form  INITILIZE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initilize.

ENDFORM.                    " INITILIZE
