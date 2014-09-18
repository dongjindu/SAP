
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
FORM modify_data USING rc
                       us_screen.

  PERFORM make_data.
  PERFORM call_sm USING rc.

ENDFORM.                    " MODIFY_DATA
*
*&---------------------------------------------------------------------*
*&      Form  CALL_SM
*&---------------------------------------------------------------------*
FORM call_sm USING rc.
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
       EXPORTING
            formname           = 'ZSSD07_ISRA_CERTI_ORIGIN'
       IMPORTING
            fm_name            = func_mod_name
       EXCEPTIONS
            no_form            = 1
            no_function_module = 2
            OTHERS             = 3.
  rc = sy-subrc.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.


  control_parameters-no_dialog = 'X'.

*  output_options-tdnoprint  =  'X'.
*  OUTPUT_OPTIONS-TDNOPREV = 'X'.  "No preview
  output_options-tdnewid = 'X'.
  output_options-tdimmed = 'X'.
*  OUTPUT_OPTIONS-TDDELETE = 'X'.

  CLEAR job_output_info.

  CALL FUNCTION func_mod_name
    EXPORTING
      control_parameters = control_parameters
      output_options     = output_options
*      USER_SETTINGS      = ''
      wa_certi           = wa_isra_certi_origin
    IMPORTING
      job_output_info    = job_output_info
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.
  rc = sy-subrc.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.


*  W_SPOOLID[] = JOB_OUTPUT_INFO-SPOOLIDS[].
*  READ TABLE W_SPOOLID INDEX 1.
*  SRCSPOOLID = W_SPOOLID.
*
*  CHECK SRCSPOOLID NE 0.
*
*  CALL FUNCTION 'CONVERT_OTFSPOOLJOB_2_PDF'
*       EXPORTING
*            SRC_SPOOLID              = SRCSPOOLID
*            NO_DIALOG                = ' '
*       IMPORTING
*            PDF_BYTECOUNT            = NUMBYTES
*            PDF_SPOOLID              = PDFSPOOLID
*       TABLES
*            PDF                      = PDF
*       EXCEPTIONS
*            ERR_NO_OTF_SPOOLJOB      = 1
*            ERR_NO_SPOOLJOB          = 2
*            ERR_NO_PERMISSION        = 3
*            ERR_CONV_NOT_POSSIBLE    = 4
*            ERR_BAD_DSTDEVICE        = 5
*            USER_CANCELLED           = 6
*            ERR_SPOOLERROR           = 7
*            ERR_TEMSEERROR           = 8
*            ERR_BTCJOB_OPEN_FAILED   = 9
*            ERR_BTCJOB_SUBMIT_FAILED = 10
*            ERR_BTCJOB_CLOSE_FAILED  = 11.
*  RC = SY-SUBRC.
*  IF SY-SUBRC <> 0.
*    EXIT.
*  ENDIF.
*
*
*  CONCATENATE 'c:\IC_' 'B06' SY-DATUM+2(6) W_CNT '.pdf'
*              into W_FILE.
*
*  CALL FUNCTION 'WS_DOWNLOAD'
*    EXPORTING
*      BIN_FILESIZE            = NUMBYTES
*      FILENAME                = W_FILE
*      FILETYPE                = 'BIN'
**   IMPORTING
**     FILELENGTH              =
*    TABLES
*      DATA_TAB                = PDF
*    EXCEPTIONS
*      FILE_OPEN_ERROR         = 1
*      FILE_WRITE_ERROR        = 2
*      INVALID_FILESIZE        = 3
*      INVALID_TYPE            = 4
*      NO_BATCH                = 5
*      UNKNOWN_ERROR           = 6
*      INVALID_TABLE_WIDTH     = 7
*      GUI_REFUSE_FILETRANSFER = 8
*      CUSTOMER_ERROR          = 9
*      OTHERS                  = 10.
*  RC = SY-SUBRC.
ENDFORM.                    " CALL_SM

*&---------------------------------------------------------------------*
*&      Form  make_data
*&---------------------------------------------------------------------*
FORM make_data.
  DATA : lv_adrnr LIKE vbpa-adrnr.
  DATA : lv_atinn LIKE ausp-atinn.
  DATA : lv_atinn_mod LIKE ausp-atinn.
  DATA : lv_atinn_epi LIKE ausp-atinn.
  DATA : lv_btgew LIKE likp-btgew.
  DATA : lv_slnid LIKE cuvtab_valc-slnid.
  DATA : lv_engine_cap LIKE ztbm_abxopvdt-clnm. " Engine Capa
  DATA : lv_model_desc TYPE zdesc1.
  DATA : lv_series(100).
  CLEAR : wa_list.

*Date / Invoice # / serial
  SELECT SINGLE a~fkdat  a~vbeln  b~arktx
    INTO (wa_list-fkdat, wa_list-vbeln, wa_list-arktx)
  FROM vbrk  AS a INNER JOIN vbrp  AS b
                  ON a~vbeln  = b~vbeln
  WHERE b~vgbel  =  nast-objky.

  IF sy-subrc <> 0.
    MESSAGE e000(zmpp) WITH 'Invoice No. does not exist'.
  ENDIF.

*Vehicle Indetification No.  / shipping Weight
  SELECT SINGLE a~berot  a~btgew   a~gewei
    INTO (wa_list-berot, lv_btgew, wa_list-gewei)
  FROM likp  AS a  INNER JOIN lips   AS b
                ON a~vbeln  = b~vbeln
  WHERE a~vbeln  =  nast-objky.

  wa_list-btgew  =  lv_btgew.

*name of distributor
  SELECT SINGLE adrnr  INTO lv_adrnr
  FROM vbpa
  WHERE vbeln  =     wa_list-vbeln.
  IF sy-subrc = 0.
    SELECT SINGLE name1 name2  city1  post_code1
      INTO (wa_list-name1,  wa_list-name2,  wa_list-city1,
          wa_list-post_code1)
    FROM adrc
    WHERE addrnumber  =  lv_adrnr.


  ENDIF.

*Year
  CLEAR : lv_atinn, wa_ausp, ztbm_abxopvdt.
  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
       EXPORTING
            input  = 'P_219_1'
       IMPORTING
            output = lv_atinn.

  SELECT SINGLE *
  INTO CORRESPONDING FIELDS OF  wa_ausp
  FROM ausp
  WHERE objek  =  nast-objky
    AND atinn  = lv_atinn.

  IF sy-subrc = 0.
    SELECT SINGLE *
    FROM ztbm_abxopvdt
    WHERE carx  =  nast-objky+0(2)
      AND clno  =  '1'
      AND valu  =  wa_ausp-atwrt.
    IF sy-subrc = 0.
      wa_list-year  =  ztbm_abxopvdt-clnm+0(4).
    ENDIF.
  ENDIF.

*Body type
  CLEAR : lv_atinn, wa_ausp, ztbm_abxopvdt.
  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
       EXPORTING
            input  = 'P_219_4'
       IMPORTING
            output = lv_atinn.

  SELECT SINGLE *
  INTO CORRESPONDING FIELDS OF  wa_ausp
  FROM ausp
  WHERE objek  =  nast-objky
    AND atinn  = lv_atinn.

  IF sy-subrc = 0.
    SELECT SINGLE *
    FROM ztbm_abxopvdt
    WHERE carx  =  nast-objky+0(2)
      AND clno  =  '4'
      AND valu  =  wa_ausp-atwrt.
    IF sy-subrc = 0.
      wa_list-body  =  ztbm_abxopvdt-clnm.
    ENDIF.
  ENDIF.

*HP

  CLEAR : lv_atinn, wa_ausp, ztbm_abxopvdt.
  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
       EXPORTING
            input  = 'P_219_9'
       IMPORTING
            output = lv_atinn.

  SELECT SINGLE *
  INTO CORRESPONDING FIELDS OF  wa_ausp
  FROM ausp
  WHERE objek  =  nast-objky
    AND atinn  = lv_atinn.

  IF sy-subrc = 0.
    SELECT SINGLE *
    FROM ztbm_abxopvdt
    WHERE carx  =  nast-objky+0(2)
      AND clno  =  '9'
      AND valu  =  wa_ausp-atwrt.
    IF sy-subrc = 0.
      lv_engine_cap  =  ztbm_abxopvdt-clnm.  "Engine Capa
    ENDIF.

*variant tble
    CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
         EXPORTING
              input  = 'P_MODEL'
         IMPORTING
              output = lv_atinn_mod.

    CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
         EXPORTING
              input  = 'P_EPI_CODE'
         IMPORTING
              output = lv_atinn_epi.

    SELECT SINGLE  b~slnid   d~valc
          INTO (lv_slnid, wa_list-hp)
    FROM cuvtab  AS  a   INNER JOIN cuvtab_valc   AS b
                            ON a~vtint  =  b~vtint
                         INNER JOIN cuvtab_valc AS c
                            ON b~vtint   =  c~vtint
                           AND  b~slnid  =  c~slnid
                         INNER JOIN cuvtab_valc AS d
                            ON b~vtint  =  d~vtint
                           AND b~slnid  =  d~slnid
    WHERE a~vtnam   =  'Z_MODEL_HP_MKTING'
      AND b~atinn   =  lv_atinn_mod
      AND b~valc    =  nast-objky+0(2)
      AND c~atinn   =  lv_atinn
      AND c~valc    =  wa_ausp-atwrt
      AND d~atinn   =  lv_atinn_epi .
  ENDIF.

*GVWR      check item level: aupos
  SELECT SINGLE c~tech_004 INTO wa_list-gvwr
  FROM vbrp  AS a INNER JOIN vbkd AS b
                ON a~aubel  =  b~vbeln
              INNER JOIN ztpp_tech AS c
                ON b~bstkd  =  c~matnr
  WHERE a~vgbel  =  nast-objky.
  IF sy-subrc = 0.
    wa_list-gvwr =  wa_list-gvwr+0(4) .
  ENDIF.

  SELECT SINGLE des1 INTO lv_model_desc
  FROM ztpp0001
  WHERE code  =  '01'
    AND key1  =  nast-objky+0(2).

*--Series of Model
  IF lv_model_desc IS INITIAL OR lv_engine_cap IS INITIAL
      OR wa_list-body IS INITIAL.
    MESSAGE e000(zmpp) WITH 'Series or Model info is missing!'.
  ELSE.
    CONCATENATE lv_model_desc lv_engine_cap  wa_list-body
              INTO wa_list-model  separated by space.
  ENDIF.

*No. of Cycles / Make
  wa_list-cyls  =  '04'.
  wa_list-make  =  'Hyundai'.

*move data
  CLEAR : wa_isra_certi_origin.
  MOVE-CORRESPONDING wa_list TO wa_isra_certi_origin.
ENDFORM.                    " make_data
