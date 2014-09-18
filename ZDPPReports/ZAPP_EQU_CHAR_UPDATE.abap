************************************************************************
* Program Name      : ZAPP_EQU_CHAR_UPDATE_1
* Author            : Furong, Wang
* Creation Date     : 10/2005
* Specifications By :
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       : Module Cost Update
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

REPORT zapp_equ_char_update_1
                NO STANDARD PAGE HEADING
                LINE-SIZE  100
                LINE-COUNT 65
                MESSAGE-ID zmbm.

TABLES: ztpp_pmt07jb_b, ZTPP_SEQ_LOG, sscrfields.

DATA: BEGIN OF it_vin1 OCCURS 0,
      equnr(9),
      seqdate(8),
      ssr1(4),
      rp(2),
      wo(14),
      extc(2),
      intc(2),
      porder(10),
      mi(7),
      ocn(4),
      ver(3),
      vinn(17),
      END OF it_vin1.

DATA: BEGIN OF it_vin         OCCURS 0,
      equnr LIKE equi-equnr,
      seqdate LIKE ztpp_pmt07jb_a-sqdt,
      ssr1 LIKE ztpp_pmt07jb_a-ssr1,
      rp(2),
      wo(14),
      extc LIKE ztpp_pmt07jb_a-extc,
      intc LIKE ztpp_pmt07jb_a-intc,
      porder(10),
      mi(7),
      ocn LIKE ztpp_pmt07jb_a-ocnn,
      ver LIKE ztpp_pmt07jb_a-vers,
      vinn LIKE ztpp_pmt07jb_a-vinn,
      END OF it_vin.

DATA: it_excl             LIKE TABLE OF it_vin1       WITH HEADER LINE,
      it_7jb              LIKE TABLE OF it_vin        WITH HEADER LINE,
      it_msg              LIKE TABLE OF bdcmsgcoll    WITH HEADER LINE,
      it_vmaster          LIKE TABLE OF zspp_vin_value WITH HEADER LINE.

DATA: wa_material             LIKE mara-matnr                 ,
      wa_material_cl          LIKE mara-matnr                 ,
      wa_7jb                  LIKE it_vin                     ,
      wa_7jb_log              LIKE it_vin                     ,
      wa_error                TYPE c                          ,
      wa_date                 TYPE d                          .

DATA: wa_tline TYPE n.

*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-100.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: r_b1 RADIOBUTTON GROUP grp DEFAULT 'X' USER-COMMAND ucom.
SELECTION-SCREEN COMMENT 3(20) text-201.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: r_b2 RADIOBUTTON GROUP grp.
SELECTION-SCREEN COMMENT 3(20) text-202.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END   OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-101.
PARAMETERS:
*  P_GROUP LIKE APQI-GROUPID DEFAULT SY-REPID  OBLIGATORY,
  p_file  LIKE rlgrap-filename DEFAULT 'C:\       .TXT' OBLIGATORY
                                              MODIF ID md1,
  p_filety LIKE rlgrap-filetype DEFAULT 'DAT' MODIF ID md1.
**  MODIF ID gr1.
SELECTION-SCREEN END   OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-102.
SELECT-OPTIONS: s_sqdt FOR sy-datum MODIF ID MD2,
                s_EQUNR FOR ztpp_seq_log-EQUNR MODIF ID MD2.
SELECTION-SCREEN END   OF BLOCK b3.

AT SELECTION-SCREEN.

  PERFORM selection_screen_ucomm.

AT SELECTION-SCREEN OUTPUT.
  PERFORM selection_screen_output.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM at_sel_screen_on_value_request USING p_file 'O'.

START-OF-SELECTION.
  IF r_b1 = 'X'.
    PERFORM upload_process.
    PERFORM read_excel.
  ELSE.
    PERFORM read_from_log.
  ENDIF.
  PERFORM data_process.
*  PERFORM WRITE_PROCESS.

*&---------------------------------------------------------------------*
*&      Form  AT_SEL_SCREEN_ON_VALUE_REQUEST
*&---------------------------------------------------------------------*
FORM at_sel_screen_on_value_request USING def_path LIKE rlgrap-filename
                                          mode     TYPE c.

  DATA: tmp_filename LIKE rlgrap-filename.
  DATA: tmp_mask(80).                  " LIKE GLOBAL_FILEMASK_ALL.
  DATA: fieldln TYPE i.
  FIELD-SYMBOLS: <tmp_sym>.

* Build Filter for Fileselektor

*  IF GLOBAL_FILEMASK_MASK IS INITIAL.
  tmp_mask = ',*.*,*.*.'.
*  ELSE.
*    TMP_MASK = ','.
*    WRITE GLOBAL_FILEMASK_TEXT TO TMP_MASK+1.
*    WRITE ',' TO TMP_MASK+21.
*    WRITE GLOBAL_FILEMASK_MASK TO TMP_MASK+22.
*    WRITE '.' TO TMP_MASK+42.
*    CONDENSE TMP_MASK NO-GAPS.
*  ENDIF.

*  IF NOT GLOBAL_FILEMASK_ALL IS INITIAL.
*    TMP_MASK = GLOBAL_FILEMASK_ALL.
*  ENDIF.
*
  fieldln = strlen( def_path ) - 1.
  ASSIGN def_path+fieldln(1) TO <tmp_sym>.
  IF <tmp_sym> = '/' OR <tmp_sym> = '\'.
    CLEAR <tmp_sym>.
  ENDIF.

  CALL FUNCTION 'WS_FILENAME_GET'
       EXPORTING
            def_filename     = p_file
            def_path         = def_path
*           MASK             = ',*.*,*.*.'
            mask             = tmp_mask
            mode             = mode
*           TITLE            = ' '
       IMPORTING
            filename         = tmp_filename
*         RC               =
       EXCEPTIONS
            inv_winsys       = 01
            no_batch         = 02
            selection_cancel = 03
            selection_error  = 04.

  IF sy-subrc = 0.
    p_file = tmp_filename.
  ELSE.
* IF SY-SUBRC = 01.    "// Does not work, why ???
*   MESSAGELINE = 'Not supported'.
* ENDIF.
  ENDIF.

ENDFORM.                               " AT_SEL_SCREEN_ON_VALUE_REQUEST
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_PROCESS
*&---------------------------------------------------------------------*
FORM upload_process.

  CALL FUNCTION 'UPLOAD'
   EXPORTING
*   CODEPAGE                      = ' '
     filename                      = p_file
     filetype                      = p_filety
*   ITEM                          = ' '
*   FILEMASK_MASK                 = ' '
*   FILEMASK_TEXT                 = ' '
*   FILETYPE_NO_CHANGE            = ' '
*   FILEMASK_ALL                  = ' '
*   FILETYPE_NO_SHOW              = ' '
*   LINE_EXIT                     = ' '
*   USER_FORM                     = ' '
*   USER_PROG                     = ' '
*   SILENT                        = 'S'
* IMPORTING
*   FILESIZE                      =
*   CANCEL                        =
*   ACT_FILENAME                  =
*   ACT_FILETYPE                  =
    TABLES
      data_tab                      = it_excl

* EXCEPTIONS
*   CONVERSION_ERROR              = 1
*   INVALID_TABLE_WIDTH           = 2
*   INVALID_TYPE                  = 3
*   NO_BATCH                      = 4
*   UNKNOWN_ERROR                 = 5
*   GUI_REFUSE_FILETRANSFER       = 6
*   OTHERS                        = 7
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


*  CALL FUNCTION 'WS_UPLOAD'
*       EXPORTING
*            codepage                = ' '
*            filename                = p_file
*            filetype                = p_filety
**           HEADLEN                 = ' '
**           LINE_EXIT               = ' '
**           TRUNCLEN                = ' '
**           USER_FORM               = ' '
**           USER_PROG               = ' '
**      IMPORTING
**           FILELENGTH              =
*       TABLES
*            data_tab                = it_excl
*      EXCEPTIONS
*           conversion_error        = 1
*           file_open_error         = 2
*           file_read_error         = 3
*           invalid_table_width     = 4
*           invalid_type            = 5
*           no_batch                = 6
*           unknown_error           = 7
*           gui_refuse_filetransfer = 8
*           customer_error          = 9
*           OTHERS                  = 10
*            .
  CASE sy-subrc.
    WHEN 0.
      DATA l_text(132).
      CONCATENATE p_file text-001
                  INTO l_text.
      WRITE: / l_text.
      SKIP.
    WHEN 2.
      MESSAGE e000 WITH text-002.
    WHEN 3.
      MESSAGE e000 WITH text-003.
    WHEN OTHERS.
      MESSAGE e000 WITH text-004.
  ENDCASE.

ENDFORM.                               " UPLOAD_PROCESS
*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_excel.
  DATA: l_equnr LIKE equi-equnr.

  LOOP AT it_excl.
*    concatenate it_excl-mode into l_equnr.
    l_equnr = it_excl-equnr.
    SELECT SINGLE equnr
         FROM equi
         INTO l_equnr
*         INTO (L_MATNR, L_WERKS)
         WHERE equnr EQ l_equnr.
    IF sy-subrc EQ 0.
      MOVE-CORRESPONDING it_excl TO it_7jb.
*      it_7jb = it_excl.
*      it_7jb-EQUNR = L_EQUNR.
      APPEND it_7jb.
    ENDIF.
    CLEAR: l_equnr, it_7jb, it_excl.
  ENDLOOP.
ENDFORM.     "read_excel
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
FORM data_process.
*  SORT IT_7jb BY EQUNR.
  DATA: l_vartable        LIKE TABLE OF zspp_vin_value WITH HEADER LINE,
        l_conf            LIKE TABLE OF zspp_vin_value WITH HEADER LINE,
        l_data            LIKE TABLE OF conf_out       WITH HEADER LINE,
        l_evcode(5)       TYPE c           ,
        l_name(30)        TYPE c           ,
        l_no(03)          TYPE n           ,
        l_seq(6)          TYPE n           ,
        l_instance        LIKE inob-cuobj  ,
        l_workcenter      LIKE crhd-arbpl  ,
        l_workorder       LIKE mara-matnr  ,
        l_eqfnr           LIKE itob-eqfnr  ,
        l_mode            LIKE ztpp_common_vals-key2,
        l_equnr           LIKE equi-equnr  ,
        l_recno(4)           TYPE n,
        l_mess(40).

  DESCRIBE TABLE it_7jb LINES wa_tline.

  LOOP AT it_7jb INTO wa_7jb.
*    CONCATENATE wa_7jb-ordr wa_7jb-dist INTO wa_material .
*    CHECK wa_flag NE 'E' .
    CLEAR: wa_material, wa_material_cl,l_vartable,l_vartable[],l_no,
           l_conf,l_conf[].

    wa_material = wa_7jb-wo.
    CONCATENATE wa_material wa_7jb-extc wa_7jb-intc
                  INTO wa_material_cl.

    l_vartable-atnam = 'P_OCN'.
    l_vartable-atwrt =  wa_7jb-ocn .             APPEND l_vartable.
    l_vartable-atnam = 'P_VERSION'.
    l_vartable-atwrt =  wa_7jb-ver.             APPEND l_vartable.

    l_vartable-atnam = 'P_SEQUENCE_DATE'.
    l_vartable-atwrt =  wa_7jb-seqdate .             APPEND l_vartable.

    l_vartable-atnam = 'P_SEQUENCE_SERIAL'.
    l_vartable-atwrt =  wa_7jb-ssr1 .             APPEND l_vartable.

    l_vartable-atnam = 'P_SEQUENCE_CODE'.
    l_vartable-atwrt =  'HA'.                     APPEND l_vartable.

*    l_vartable-atnam = 'P_SALES_ORDER'  .
*    l_vartable-atwrt =  wa_7jb-sorder       .         APPEND l_vartable
    .
    l_vartable-atnam = 'P_PLAN_ORDER'   .
    l_vartable-atwrt =  wa_7jb-porder.             APPEND l_vartable.

    l_vartable-atnam = 'P_STATUS'       .
    l_vartable-atwrt = 'B00'            .         APPEND l_vartable.
    l_vartable-atnam = 'P_RP_STATUS'    .
    l_vartable-atwrt = '00'             .         APPEND l_vartable.
    l_vartable-atnam = 'P_VM_DATE'      .
*    CONCATENATE sy-datum sy-uzeit           INTO  l_vartable-atwrt .
    l_vartable-atwrt = '20050927120000'.
    APPEND l_vartable.

    l_vartable-atnam = 'P_USAGE_CAR'  .
    l_vartable-atwrt = 'P'            .         APPEND l_vartable.

    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              object       = wa_material
              ctype        = '001'
         TABLES
              val_table    = l_conf
         EXCEPTIONS
              no_data      = 1
              error_mode   = 2
              error_object = 3
              error_value  = 4
              OTHERS       = 5.

    CLEAR: l_conf-atwrt .

    READ TABLE l_conf WITH KEY atnam = 'P_NATION'.
    l_vartable-atwrt =  l_conf-atwrt    .        CLEAR: l_conf-atwrt.
    l_vartable-atnam = 'P_NATN_CODE'.      APPEND l_vartable.

    READ TABLE l_conf WITH KEY atnam = 'P_DEALER'.
    l_vartable-atwrt =  l_conf-atwrt.         CLEAR: l_conf-atwrt.
    l_vartable-atnam = 'P_DIST_CODE'.      APPEND l_vartable.

    READ TABLE l_conf WITH KEY atnam = 'P_DESTINATION_CODE'.
    l_vartable-atwrt =  l_conf-atwrt    .         CLEAR: l_conf-atwrt.
    l_vartable-atnam = 'P_DESTINATION_CODE'.      APPEND l_vartable.

    READ TABLE l_conf WITH KEY atnam = 'P_LC_NO'.
    l_vartable-atwrt =  l_conf-atwrt    .         CLEAR: l_conf-atwrt.
    l_vartable-atnam = 'P_LC_NO'         .        APPEND l_vartable.

    READ TABLE l_conf WITH KEY atnam = 'P_MODEL'.
    l_vartable-atwrt =  l_conf-atwrt    .         CLEAR: l_conf-atwrt.
    l_vartable-atnam = 'P_MODEL'         .        APPEND l_vartable.

    l_vartable-atwrt = wa_7jb-equnr+3(6) .        CLEAR: l_conf-atwrt.
    l_vartable-atnam = 'P_BODY_SERIAL'   .        APPEND l_vartable.

    l_vartable-atwrt = wa_material.
    CLEAR: l_conf-atwrt.
    l_vartable-atnam = 'P_WORK_ORDER'    .        APPEND l_vartable.

    l_vartable-atwrt = wa_7jb-extc       .        CLEAR: l_conf-atwrt.
    l_vartable-atnam = 'P_EXT_COLOR'     .        APPEND l_vartable.

    l_vartable-atwrt = wa_7jb-intc       .        CLEAR: l_conf-atwrt.
    l_vartable-atnam = 'P_INT_COLOR'     .        APPEND l_vartable.

    READ TABLE l_conf WITH KEY atnam = 'P_MODEL_YEAR'.
    l_vartable-atwrt =  l_conf-atwrt    .         CLEAR: l_conf-atwrt.
    l_vartable-atnam = 'P_MODEL_YEAR'    .        APPEND l_vartable.

    READ TABLE l_conf WITH KEY atnam = 'P_MI'   .
    l_vartable-atwrt =  l_conf-atwrt    .         CLEAR: l_conf-atwrt.
    l_vartable-atnam = 'P_MI'            .        APPEND l_vartable.

    l_vartable-atwrt = wa_7jb-vinn       .        CLEAR: l_conf-atwrt.
    l_vartable-atnam = 'P_VIN'           .        APPEND l_vartable.

    READ TABLE l_conf WITH KEY atnam = 'P_REGION_PORT'.
    l_vartable-atwrt =  l_conf-atwrt    .         CLEAR: l_conf-atwrt.
    l_vartable-atnam = 'P_REGION_PORT'  .         APPEND l_vartable.

    READ TABLE l_conf WITH KEY atnam = 'P_COLOR_SER'.
    l_vartable-atwrt =  l_conf-atwrt    .         CLEAR: l_conf-atwrt.
    l_vartable-atnam = 'P_COLOR_SER'    .         APPEND l_vartable.

    DO  9 TIMES.
      l_no = l_no + 1.
    CONCATENATE 'P_219_' l_no+2(1)   INTO l_name .  CLEAR: l_conf-atwrt.
      READ TABLE l_conf    WITH KEY    atnam = l_name.
      l_vartable-atnam = l_name.       l_vartable-atwrt = l_conf-atwrt .
      APPEND l_vartable        .
    ENDDO.

    DO 90 TIMES.
      l_no = l_no + 1.
    CONCATENATE 'P_219_' l_no+1(2)   INTO l_name .  CLEAR: l_conf-atwrt.
      READ TABLE l_conf    WITH KEY    atnam = l_name.
      l_vartable-atnam = l_name.       l_vartable-atwrt = l_conf-atwrt .
      APPEND l_vartable        .
    ENDDO.

    DO 120 TIMES.
      l_no = l_no + 1.
    CONCATENATE 'P_219_' l_no        INTO l_name .  CLEAR: l_conf-atwrt.
      READ TABLE l_conf    WITH KEY    atnam = l_name.
      l_vartable-atnam = l_name.       l_vartable-atwrt = l_conf-atwrt .
      APPEND l_vartable        .
    ENDDO.

    CLEAR: l_conf, l_conf[].

    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              object       = wa_material_cl
              ctype        = '001'
         TABLES
              val_table    = l_conf
         EXCEPTIONS
              no_data      = 1
              error_mode   = 2
              error_object = 3
              error_value  = 4
              OTHERS       = 5.

    READ TABLE l_conf WITH KEY atnam = 'P_FLEET'.
    l_vartable-atwrt =  l_conf-atwrt    .         CLEAR: l_conf-atwrt.
    l_vartable-atnam = 'P_FLEET'         .        APPEND l_vartable.

    READ TABLE l_conf WITH KEY atnam = 'P_ORDER_ZONE'.
    l_vartable-atwrt =  l_conf-atwrt    .         CLEAR: l_conf-atwrt.
    l_vartable-atnam = 'P_ORDER_ZONE'   .         APPEND l_vartable.

    READ TABLE l_conf WITH KEY atnam = 'P_MANUAL_ORDER'.
    l_vartable-atwrt =  l_conf-atwrt    .         CLEAR: l_conf-atwrt.
    l_vartable-atnam = 'P_MANUAL_ORDER' .         APPEND l_vartable.

    READ TABLE l_conf WITH KEY atnam = 'P_SALES_ORDER'.
    l_vartable-atwrt =  l_conf-atwrt.
    l_vartable-atnam = 'P_SALES_ORDER'.           APPEND l_vartable.

    l_equnr = wa_7jb-equnr.

    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              object       = l_equnr
              mode         = 'W'
         TABLES
              val_table    = l_vartable
         EXCEPTIONS
              no_data      = 1
              error_mode   = 2
              error_object = 3
              error_value  = 4
              OTHERS       = 5.

    IF sy-subrc <> 0.
      PERFORM create_log USING '81' text-001 .
      wa_error = 'X'      .
    ELSE.
      WRITE:/ wa_7jb-equnr, 'updated'.
*      delete ZTPP_SEQ_LOG from wa_7jb.
      COMMIT WORK.
    ENDIF.
    CLEAR wa_7jb.
    l_recno = l_recno + 1.
*    if l_recno = 50 or l_recno = 100 or l_recno = 150.
    CONCATENATE 'processed rec.' l_recno wa_7jb-equnr INTO l_mess.
    PERFORM display_progress_bar USING l_mess.
*    endif.
  ENDLOOP.
*  commit work.
ENDFORM.                    " DATA_PROCESS
*&---------------------------------------------------------------------*
*&      Form  WRITE_PROCESS
*&---------------------------------------------------------------------*
*FORM WRITE_PROCESS.
*  SORT IT_CHEC BY CHECK MATNR WERKS.
*    WRITE: /(18) 'MATERIAL',
*            (10) 'PLANT',
*            (10) 'CHECK',
*            (50) 'MESSAGE'.
*  FORMAT COLOR 6.
*  WRITE: / 'MARC NO DATA LIST'.
*  LOOP AT IT_CHEC WHERE CHECK EQ 'X'.
*    WRITE: /(18) IT_CHEC-MATNR,
*            (10) IT_CHEC-WERKS,
*            (10) IT_CHEC-CHECK,
*            (50) IT_CHEC-MESSA.
*  ENDLOOP.
*  FORMAT COLOR 5.
*  WRITE: / 'MARC DATA LIST'.
*  LOOP AT IT_CHEC WHERE CHECK EQ ' '.
*    WRITE: /(18) IT_CHEC-MATNR,
*            (10) IT_CHEC-WERKS,
*            (10) IT_CHEC-CHECK,
*            (50) IT_CHEC-MESSA.
*  ENDLOOP.
*
*ENDFORM.                    " WRITE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  SELECTION_SCREEN_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM selection_screen_output.
  LOOP AT SCREEN.
    IF screen-group1 = 'MD1'.
      IF r_b2 = 'X'.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
    IF screen-group1 = 'MD2'.
      IF r_b1 = 'X'.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " SELECTION_SCREEN_OUTPUT

*---------------------------------------------------------------------*
*       FORM create_log                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  PA_STEP                                                       *
*  -->  PA_MSG                                                        *
*---------------------------------------------------------------------*
FORM create_log  USING pa_step pa_msg .
  DATA: l_log                LIKE ztpp_rep_seq .

  CLEAR: l_log.
  SELECT MAX( sequence ) INTO l_log-sequence
    FROM ztpp_rep_seq
   WHERE wk_date  = wa_date  .

  l_log-wk_date   = wa_date             .
  l_log-sequence  = l_log-sequence + 1 .
  l_log-msg       = pa_msg             .
  l_log-step      = pa_step            .
  l_log-status    = 'E'                .
  l_log-logtype   = 'E'                .
  MOVE-CORRESPONDING wa_7jb    TO l_log .
  INSERT INTO ztpp_rep_seq VALUES l_log.
  wa_error = 'X'                       .
ENDFORM.                    " create_log
*&---------------------------------------------------------------------*
*&      Form  display_progress_bar
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_MESS  text
*----------------------------------------------------------------------*
FORM display_progress_bar USING    p_text.
  DATA: lw_text(50).

  MOVE: p_text TO lw_text.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            text = lw_text.

ENDFORM.                    " display_progress_bar
*&---------------------------------------------------------------------*
*&      Form  read_from_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_from_log.
  SELECT * from ZTPP_SEQ_LOG
           into corresponding fields of table it_7jb
           where EQUNR in s_EQUNR
             and SEQDATE in s_sqdt.
  if sy-subrc ne 0.
     message e000 with text-300.
     exit.
  endif.
*  ZTPP_SEQ_LOG
ENDFORM.                    " read_from_log
*&---------------------------------------------------------------------*
*&      Form  selection_screen_readibutton
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM selection_screen_ucomm.
  IF sscrfields-ucomm = 'UCOM'.
    LOOP AT SCREEN.
      IF r_b2 = 'X' AND screen-name = 'R_B2'.
        screen-input = '1'.
      ELSE.
        screen-input = '0'.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " selection_screen_readibutton
