************************************************************************
* Program Name      : ZIPP_PT_CODE
* Author            : Furong Wang
* Creation Date     : 07/01/2010
* Specifications By : Daniel Kim
* Development Request No :
* Addl Documentation:
* Description       : PT: Send Engine Production Data
* Modification Logs
* Date       Developer    RequestNo    Description
* 09/23/2010 Daniel Kim   UD1K949836   select active (11) material
*********************************************************************

REPORT zipp_pt_code NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID zmpp.

TABLES: mara.

DATA : l_msgtxt(100),
       l_result(1).

CONSTANTS: c_dest(10) VALUE 'WMHR01'.   "Interface Destination.

DATA: BEGIN OF it_pt_code OCCURS 0.
        INCLUDE STRUCTURE ztpp_pt_code.
DATA: END OF it_pt_code.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
SELECT-OPTIONS: s_matnr FOR mara-matnr,
                s_crdate FOR sy-datum,
                s_chdate FOR sy-datum.
SELECTION-SCREEN SKIP 1.
*PARAMETERS: P_SEND AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.
  PERFORM init_data.

START-OF-SELECTION.
  PERFORM get_data.
  PERFORM send_data.


END-OF-SELECTION.

*---------------------------------------------------------------------*
*       FORM GET_DATA                                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM get_data.
  DATA: BEGIN OF lt_matnr OCCURS 0,
     matnr  LIKE mara-matnr,
     END OF lt_matnr.

  DATA: l_date_c(8),
        l_time_c(6).

  SELECT DISTINCT matnr INTO TABLE lt_matnr
     FROM mara
     WHERE matnr IN s_matnr
       AND ersda IN s_crdate
       AND laeda IN s_chdate.
* by Daniel Kim on 09/23/2010 {
* by Daniel Kim on 02/28/2012 {
*       AND MSTAE = '12'.
* }
* }

  l_date_c = sy-datum.
  l_time_c = sy-uzeit.

  LOOP AT lt_matnr.
    it_pt_code-mip_cd = lt_matnr-matnr.
    it_pt_code-mip_scn_cd = 'EE'.
    it_pt_code-company_code = 'H201'.
    it_pt_code-prdn_plnt_cd = 'HEA1'.

    PERFORM read_normal_class USING lt_matnr-matnr 'EN_SPC14'
                                CHANGING  it_pt_code-mip_ln_cd.

    PERFORM read_normal_class USING lt_matnr-matnr 'EN_SPC15'
                                CHANGING  it_pt_code-mip_ln_nm.

    SELECT SINGLE maktx INTO it_pt_code-mip_nm
     FROM makt
     WHERE matnr = lt_matnr-matnr
       AND spras = 'E'.

*    IT_PT_CODE-ASSY_UNPT_SCN_CD = 'A'.
*
*    PERFORM READ_NORMAL_CLASS USING LT_MATNR-MATNR 'EN_SPC01'
*                               CHANGING  IT_PT_CODE-ENG_XHST_QTY_CD.
*
*    PERFORM READ_NORMAL_CLASS USING LT_MATNR-MATNR 'EN_SPC03'
*                               CHANGING  IT_PT_CODE-ENG_TYPE_CD.
*
*    PERFORM READ_NORMAL_CLASS USING LT_MATNR-MATNR 'EN_SPC02'
*                                 CHANGING  IT_PT_CODE-FUEL_TYPE_CD.
*
*    PERFORM READ_NORMAL_CLASS USING LT_MATNR-MATNR 'EN_SPC05'
*                                 CHANGING  IT_PT_CODE-TM_TYPE_CD.
*
*    PERFORM READ_NORMAL_CLASS USING LT_MATNR-MATNR 'EN_VEH_MODEL'
*                                 CHANGING  IT_PT_CODE-PRDN_CAR_CD_1.

    it_pt_code-mip_wned_scn_cd = 'V'.

    it_pt_code-mtrl_no = lt_matnr-matnr.

    CONCATENATE l_date_c l_time_c INTO it_pt_code-createdate.
    it_pt_code-changedate = it_pt_code-createdate.
    APPEND it_pt_code.
    CLEAR: it_pt_code.

  ENDLOOP.

ENDFORM.                    "GET_DATA

*call function ''.

FORM send_data.
  DATA: it_save LIKE TABLE OF ztpp_pt_code WITH HEADER LINE,
        l_flag(1),
        l_zedat LIKE ztpp_pt_prod-zedat.

  l_zedat = sy-datum - 120.

  IF it_pt_code[] IS INITIAL.
    MESSAGE i000 WITH 'No data'.
    EXIT.
  ENDIF.
*  IF P_SEND IS INITIAL.
*    LOOP AT IT_PT_CODE.
*      MOVE-CORRESPONDING IT_PT_CODE TO IT_SAVE.
*      IT_SAVE-ZSDAT = SY-DATUM.
*      IT_SAVE-ZSTIM = SY-UZEIT.
*      APPEND IT_SAVE.
*      CLEAR: IT_SAVE.
*    ENDLOOP.
*
*    DELETE FROM ZTPP_PT_PROD WHERE MIP_CD IN S_MATNR.
*    INSERT ZTPP_PT_CODE FROM TABLE IT_SAVE.
*    IF SY-SUBRC = 0.
*      COMMIT WORK.
*    ELSE.
*      ROLLBACK WORK.
*      MESSAGE I000 WITH 'Database table update error'.
*    ENDIF.
*  ELSE.
  CALL FUNCTION 'Z_FPP_PT_CODE'
    DESTINATION c_dest
    IMPORTING
      flag                  = l_result
    TABLES
      i_data                = it_pt_code
    EXCEPTIONS
      communication_failure = 1  MESSAGE l_msgtxt
      system_failure        = 2  MESSAGE l_msgtxt.

  IF sy-subrc = 0.
    IF l_result = 'S'.
      l_flag = 'S'.
      l_msgtxt = 'Data successfully sent out'.
      MESSAGE i001 WITH l_msgtxt.

    ELSE.
      l_flag = 'E'.
      l_msgtxt =  'Data unsuccessfully sent out'.
      MESSAGE i001 WITH l_msgtxt.
    ENDIF.
  ELSE.
    l_flag = 'E'.
    MESSAGE i001 WITH l_msgtxt.
  ENDIF.

  LOOP AT it_pt_code.
    MOVE-CORRESPONDING it_pt_code TO it_save.
    it_save-zresult = l_flag.
    it_save-zmsg = l_msgtxt.
    it_save-zsdat = sy-datum.
    it_save-zstim = sy-uzeit.
    it_save-zedat = sy-datum.
    it_save-zetim = sy-uzeit.
    APPEND it_save.
    CLEAR: it_save.
  ENDLOOP.

  DELETE FROM ztpp_pt_prod WHERE zedat < l_zedat.

  INSERT ztpp_pt_code FROM TABLE it_save.

  IF sy-subrc = 0.
    COMMIT WORK.
    IF l_flag EQ 'E'.
      MESSAGE e001 WITH l_msgtxt.
    ENDIF.
  ELSE.
    ROLLBACK WORK.
    MESSAGE i000 WITH 'Table saving error'.
  ENDIF.
*  ENDIF.
ENDFORM.                    "SEND_DATA
*&---------------------------------------------------------------------*
*&      Form  read_normal_class
*&---------------------------------------------------------------------*
FORM read_normal_class USING  p_vmno  p_char
                             CHANGING p_value.
  SELECT SINGLE au~atwrt
    INTO p_value
    FROM ausp AS au
      INNER JOIN cabn AS ca ON au~atinn = ca~atinn
    WHERE objek = p_vmno      AND
          klart = '001'       AND   " material
          ca~atnam = p_char  .
ENDFORM.                    " read_normal_classification
*&---------------------------------------------------------------------*
*&      Form  READ_NORMAL_CLASS_ATFLV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_OBJEK_OBJEK  text
*      -->P_0461   text
*      <--P_L_ATFLV_TEMP  text
*----------------------------------------------------------------------*
FORM read_normal_class_atflv USING  p_vmno  p_char
                             CHANGING p_value.
  SELECT SINGLE au~atflv
      INTO p_value
      FROM ausp AS au
        INNER JOIN cabn AS ca ON au~atinn = ca~atinn
      WHERE objek = p_vmno      AND
            klart = '002'       AND
            ca~atnam = p_char  .
ENDFORM.                    " READ_NORMAL_CLASS_ATFLV
*&---------------------------------------------------------------------*
*&      Form  init_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_data.
  s_crdate-low = sy-datum - 1.
  s_crdate-sign = 'I'.
  s_crdate-option = 'EQ'.
  APPEND s_crdate.
ENDFORM.                    " init_data
