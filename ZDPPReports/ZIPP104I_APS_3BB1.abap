************************************************************************
* Program Name      : ZIPP104I_APS_3BB1
* Author            : Bobby
* Creation Date     : 2003.08.22.
* Specifications By : Bobby
* Pattern           : 1.1
* Development Request No : UD1K902019
* Addl Documentation:
* Description       : 219 ALC CODE
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
************************************************************************
REPORT zipp104i_aps_3bb1 MESSAGE-ID zmpp.

DATA: fl_error                TYPE c           ,
      t_text(100)             TYPE c           ,
      t_index                 TYPE i           ,
      t_count                 TYPE i           .

DATA: it_result           LIKE TABLE OF zspp_vin_value WITH HEADER LINE,
      it_message          LIKE TABLE OF bdcmsgcoll     WITH HEADER LINE.


DATA: BEGIN OF it_mara OCCURS 0,
      matnr LIKE mara-matnr,
      END OF it_mara.
*DATA: BEGIN OF it_mara    OCCURS 0.
*        INCLUDE STRUCTURE mara.
*DATA:   maktx             LIKE makt-maktx,
*      END OF it_mara.

DATA: it_conf_out         LIKE TABLE OF ztpp_pmt03bb   WITH HEADER LINE.

************************************************************************
*              SELECTION SCREEN LAYOUT                                 *
************************************************************************
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME .
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_run          TYPE c AS CHECKBOX  DEFAULT 'X'.
SELECTION-SCREEN COMMENT  (55) text-001 FOR FIELD p_run.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN skip.
SELECTION-SCREEN skip.
SELECTION-SCREEN COMMENT 1(70) text-m01.
SELECTION-SCREEN skip.
SELECTION-SCREEN: END OF BLOCK b1.


************************************************************************
*              START-OF-SELECTION PROCESSING                           *
************************************************************************
START-OF-SELECTION.
  CHECK p_run = 'X'  .
  PERFORM program_description.
  PERFORM collect_wo_data.
  PERFORM display_result .

*&---------------------------------------------------------------------*
*&      Form  PROGRAM_DESCRIPTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM program_description.
* PROCESSING STEPS.
* 1'ST : DATA READ FOR THE INTERFACING (ABOUT THE WO_HEADER)
* 2'ST : SAVING THE DATA INTO THE TABLE NAMED ZTPP_PMT03BB .
* BEFORE THE PROCESSING, CHECK THE THIS PROGRAM'S STATUS   .
ENDFORM.                    " PROGRAM_DESCRIPTION

*&---------------------------------------------------------------------*
*&      Form  COLLECT_WO_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM collect_wo_data.
  PERFORM delete_olddata    .
  PERFORM reading_material  .
* PERFORM CHECK_SUBCOMPONENT.
  PERFORM reading_instance  .
* PERFORM CHECK_DATA        .
ENDFORM.                    " COLLECT_WO_DATA

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_RESULT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_result .
  PERFORM write_process_data    .
ENDFORM.                    " DISPLAY_RESULT

*&---------------------------------------------------------------------*
*&      Form  DELETE_OLDDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_olddata .
  DELETE FROM ztpp_pmt03bb CLIENT SPECIFIED WHERE mandt = sy-mandt.
ENDFORM.                    " DELETE_OLDDATA

*&---------------------------------------------------------------------*
*&      Form  READING_MATERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reading_material.
*  DATA: l_fsc              LIKE mara-matnr.
  DATA: BEGIN OF lt_pmt03fb OCCURS 0,
        usee LIKE ztpp_pmt03fb-usee,
        pack LIKE ztpp_pmt03fb-pack,
        regn LIKE ztpp_pmt03fb-regn,
        serl LIKE ztpp_pmt03fb-dist,
        dist LIKE ztpp_pmt03fb-dist,
        END OF lt_pmt03fb.

** On 11/07/13
  SELECT usee pack regn serl dist
    INTO TABLE lt_pmt03fb
    FROM ztpp_pmt03fb.

  LOOP AT lt_pmt03fb.
    CONCATENATE lt_pmt03fb-usee
                lt_pmt03fb-pack lt_pmt03fb-regn
                lt_pmt03fb-serl lt_pmt03fb-dist
          INTO it_mara-matnr.
    COLLECT it_mara.
  ENDLOOP.

*  SELECT *  INTO CORRESPONDING FIELDS OF TABLE it_mara
*    FROM mara AS a INNER JOIN makt AS t
*      ON a~matnr = t~matnr
*     AND t~spras = sy-langu
*   WHERE a~mtart = 'WOHD'  .
*
*
*  SORT it_mara BY maktx DESCENDING.
*
*  LOOP AT it_mara.
*    IF l_fsc = it_mara-maktx(18).
*      DELETE it_mara.
*    ELSE.
*      l_fsc = it_mara-maktx(18) .
*    ENDIF.
*  ENDLOOP.
** end

*  SORT IT_MARA BY MATNR          .
ENDFORM.                    " READING_MATERIAL

*&---------------------------------------------------------------------*
*&      Form  READING_INSTANCE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reading_instance.
  CLEAR: it_result, it_result[].

** 11/07/13 by Furong
*  LOOP AT it_mara WHERE kzkfg = space   .
  LOOP AT it_mara.
** End
    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
      EXPORTING
        object       = it_mara-matnr
        ctype        = '001'
      TABLES
        val_table    = it_result
      EXCEPTIONS
        no_data      = 1
        error_mode   = 2
        error_object = 3
        OTHERS       = 4.

    IF sy-subrc = 0.
*Requested by bw park
*changed by wskim,on 01/18/2005
*No need to check P_PERF_YN'S VALUE
*----Start
*      READ TABLE it_result WITH KEY atnam = 'P_PERF_YN' .  " 2004/05/12
*      IF sy-subrc = 0 AND it_result-atwrt = 'Y'         .  " 2004/05/12
      PERFORM table_to_ztpp_pmt03bb                   .  " 2004/05/12
*      ENDIF.
*----End                                             " 2004/05/12
      CLEAR: it_result, it_result[].
    ENDIF.
  ENDLOOP .
*  DELETE ADJACENT DUPLICATES FROM it_conf_out
*                        COMPARING mdlg dist bmdl ocnn vers .
ENDFORM.                    " READING_INSTANCE

*&---------------------------------------------------------------------*
*&      Form  TABLE_TO_ZTPP_PMT03BB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM table_to_ztpp_pmt03bb   .
  CLEAR: it_conf_out .
  PERFORM read_value3 USING 'P_MI'   '2'      it_conf_out-mdlg   .
  PERFORM read_value2 USING                   it_conf_out-dist   .
  PERFORM read_value1 USING 'P_MI'            it_conf_out-bmdl   .
  PERFORM read_value1 USING 'P_OCN'           it_conf_out-ocnn   .
  PERFORM read_value1 USING 'P_VERSION'       it_conf_out-vers   .
  PERFORM read_value4 .
  PERFORM read_value5 .
  PERFORM read_value6 .
  PERFORM read_value1 USING 'P_MODEL_YEAR'    it_conf_out-flag   .
**>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
* Start : Added By Tonkey on 03/25/2004.
* Change Request No. : UD1K908691
**>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  it_conf_out-rdat = sy-datum .
**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
* End   : Added By Tonkey on 03/25/2004.
**<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  APPEND it_conf_out  .
ENDFORM.                    " TABLE_TO_ZTPP_PMT03BB

*&---------------------------------------------------------------------*
*&      Form  READ_VALUE1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0183   text
*      -->P_IT_ZTPP_PMT03BB_BMDL  text
*----------------------------------------------------------------------*
FORM read_value1 USING    p_characteristic  p_tablefield .
  READ TABLE it_result  WITH KEY atnam = p_characteristic.
  IF sy-subrc = 0.
    p_tablefield = it_result-atwrt .
  ENDIF.
ENDFORM.                    " READ_VALUE1

*&---------------------------------------------------------------------*
*&      Form  READ_VALUE2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0177   text
*      -->P_0178   text
*      -->P_IT_ZTPP_PMT03BB_DIST  text
*----------------------------------------------------------------------*
FORM read_value2 USING    p_tablefield .
  DATA: l_atwrt           LIKE  conf_out-atwrt .

  READ TABLE it_result  WITH KEY atnam = 'P_NATION' .
  IF sy-subrc = 0.
    l_atwrt  = it_result-atwrt .
  ENDIF.
  READ TABLE it_result  WITH KEY atnam = 'P_DEALER' .
  IF sy-subrc = 0.
    CONCATENATE l_atwrt  it_result-atwrt  INTO  p_tablefield .
  ENDIF.
ENDFORM.                    " READ_VALUE2

*&---------------------------------------------------------------------*
*&      Form  READ_VALUE3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0171   text
*      -->P_0172   text
*      -->P_IT_CONF_OUT_MDLG  text
*----------------------------------------------------------------------*
FORM read_value3 USING    p_charact  p_size  p_tablefield .
  READ TABLE it_result  WITH KEY atnam = p_charact  .
  IF sy-subrc = 0.
    p_tablefield = it_result-atwrt(p_size) .
  ENDIF.
ENDFORM.                    " READ_VALUE3

*&---------------------------------------------------------------------*
*&      Form  READ_VALUE4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_value4.
  READ TABLE it_result  WITH KEY atnam = 'P_219_1'   .
  IF sy-subrc = 0 .
    it_conf_out-ospc001  =  it_result-atwrt            .
  ENDIF.
  READ TABLE it_result  WITH KEY atnam = 'P_219_2'   .
  IF sy-subrc = 0 .
    it_conf_out-ospc002  =  it_result-atwrt            .
  ENDIF.
  READ TABLE it_result  WITH KEY atnam = 'P_219_3'   .
  IF sy-subrc = 0 .
    it_conf_out-ospc003  =  it_result-atwrt            .
  ENDIF.
  READ TABLE it_result  WITH KEY atnam = 'P_219_4'   .
  IF sy-subrc = 0 .
    it_conf_out-ospc004  =  it_result-atwrt            .
  ENDIF.
  READ TABLE it_result  WITH KEY atnam = 'P_219_5'   .
  IF sy-subrc = 0 .
    it_conf_out-ospc005  =  it_result-atwrt            .
  ENDIF.
  READ TABLE it_result  WITH KEY atnam = 'P_219_6'   .
  IF sy-subrc = 0 .
    it_conf_out-ospc006  =  it_result-atwrt            .
  ENDIF.
  READ TABLE it_result  WITH KEY atnam = 'P_219_7'   .
  IF sy-subrc = 0 .
    it_conf_out-ospc007  =  it_result-atwrt            .
  ENDIF.
  READ TABLE it_result  WITH KEY atnam = 'P_219_8'   .
  IF sy-subrc = 0 .
    it_conf_out-ospc008  =  it_result-atwrt            .
  ENDIF.
  READ TABLE it_result  WITH KEY atnam = 'P_219_9'   .
  IF sy-subrc = 0 .
    it_conf_out-ospc009  =  it_result-atwrt            .
  ENDIF.
ENDFORM.                    " READ_VALUE4

*&---------------------------------------------------------------------*
*&      Form  read_value5
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_value5.
  FIELD-SYMBOLS: <field_pos> ,
                 <field_val> .

  DATA: l_name(20)             TYPE c,
        l_cntidx(2)            TYPE n.

  l_cntidx = 9.
  DO 90 TIMES .
    l_cntidx = l_cntidx + 1.
    CONCATENATE 'P_219_' l_cntidx INTO l_name .
    READ TABLE it_result  WITH KEY atnam = l_name .
    CONCATENATE 'IT_CONF_OUT-OSPC0' l_cntidx INTO l_name .
    ASSIGN (l_name)       TO        <field_val>   .
    IF sy-subrc = 0 .
      <field_val>          =  it_result-atwrt            .
    ENDIF.
  ENDDO.
ENDFORM.                    " read_value5

*&---------------------------------------------------------------------*
*&      Form  read_value6
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_value6.
  FIELD-SYMBOLS: <field_pos> ,
                 <field_val> .

  DATA: l_name(20)             TYPE c,
        l_cntidx(3)            TYPE n.

  l_cntidx = 99.
  DO 120 TIMES .
    l_cntidx = l_cntidx + 1.
    CONCATENATE 'P_219_' l_cntidx INTO l_name .
    READ TABLE it_result  WITH KEY atnam = l_name .
    CONCATENATE 'IT_CONF_OUT-OSPC'   l_cntidx INTO l_name .
    ASSIGN (l_name)       TO        <field_val>   .
    IF sy-subrc = 0 .
      <field_val>          =  it_result-atwrt            .
    ENDIF.
  ENDDO.
ENDFORM.                    " read_value6

*&---------------------------------------------------------------------*
*&      Form  write_process_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_process_data.
  DATA: l_text(60) TYPE c,
        l_int TYPE i.
  DELETE it_conf_out WHERE mdlg = space      .
  MODIFY ztpp_pmt03bb FROM TABLE it_conf_out .
  IF sy-subrc = 0.
    COMMIT WORK.
    DESCRIBE TABLE it_conf_out LINES l_int.
    WRITE l_int TO l_text LEFT-JUSTIFIED.
    CONCATENATE 'Created Record count :' l_text
      INTO l_text.
    MESSAGE s001 WITH l_text.
    MESSAGE s001 WITH text-101.
  ELSE.
    MESSAGE w001 WITH text-102.
  ENDIF.
ENDFORM.                    " write_process_data
