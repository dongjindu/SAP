REPORT zapp_equ_char_update_1
                NO STANDARD PAGE HEADING
                LINE-SIZE  100
                LINE-COUNT 65
                MESSAGE-ID zmbm.

TABLES: ztpp_pmt07jb_b.
*DATA: BEGIN OF IT_EXCL OCCURS 0.
*      include structure ZTPP_PMT07JB_B.
*data: END   OF IT_EXCL.

data: begin of it_vin1 occurs 0,
      equnr(9),
      seqdate(8),
      SSR1(4),
      rp(2),
      wo(14),
      EXTC(2),
      INTC(2),
      porder(10),
      MI(7),
      OCN(4),
      VER(3),
      vinn(17),
**      VINN like ztpp_pmt07jb_a-VINN,
*      ORDR like ztpp_pmt07jb_a-ordr,
*      nation(3),
*      dealer(2),
*      INTC like ztpp_pmt07jb_a-INTC,
*      EXTC like ztpp_pmt07jb_a-EXTC,
*      SSR1 like ztpp_pmt07jb_a-SSR1,
*      seqdate(12),
*      SQDT like ztpp_pmt07jb_a-SQDT,
      end of it_vin1.
DATA: BEGIN OF it_vin         OCCURS 0,
*        INCLUDE STRUCTURE     ztpp_pmt07jb_b .
      equnr LIKE equi-equnr,
      seqdate like ztpp_pmt07jb_a-SQDT,
      SSR1 like ztpp_pmt07jb_a-SSR1,
      rp(2),
      wo(14),
      EXTC LIKE ztpp_pmt07jb_a-EXTC,
      INTC like ztpp_pmt07jb_a-INTC,
      porder(10),
      MI(7),
      OCN LIKE ztpp_pmt07jb_a-OCNN,
      VER LIKE ztpp_pmt07jb_a-VERS,
      vinn like ztpp_pmt07jb_a-vinn,
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

data: wa_tline type n.

*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-100.
PARAMETERS:
*  P_GROUP LIKE APQI-GROUPID DEFAULT SY-REPID  OBLIGATORY,
  p_file  LIKE rlgrap-filename DEFAULT 'C:\       .TXT' OBLIGATORY,
  p_filety LIKE rlgrap-filetype DEFAULT 'DAT'.

**  MODIF ID gr1.

SELECTION-SCREEN END   OF BLOCK b1.

AT SELECTION-SCREEN OUTPUT.
  PERFORM selection_screen_output.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM at_sel_screen_on_value_request USING p_file 'O'.

START-OF-SELECTION.
  PERFORM upload_process.
  PERFORM read_process.
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
   FILENAME                      = p_file
   FILETYPE                      = p_filety
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
FORM read_process.
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
      move-corresponding it_excl to it_7jb.
*      it_7jb = it_excl.
*      it_7jb-EQUNR = L_EQUNR.
      APPEND it_7jb.
    ENDIF.
    CLEAR: l_equnr, it_7jb, it_excl.
  ENDLOOP.
ENDFORM.                    " READ_PROCESS
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
        l_recno           type n,
        l_mess(40).

  describe table it_7jb lines wa_tline.

  LOOP AT it_7jb INTO wa_7jb.
*    CONCATENATE wa_7jb-ordr wa_7jb-dist INTO wa_material .
*    CHECK wa_flag NE 'E' .
    clear: wa_material, wa_material_cl, l_vartable, l_vartable[].

    wa_material = wa_7jb-WO.
    CONCATENATE wa_material wa_7jb-EXTC wa_7jb-INTC
                  INTO wa_material_cl.

   CLEAR: l_conf-atwrt .

    l_vartable-atwrt = wa_material.
    CLEAR: l_conf-atwrt.
    l_vartable-atnam = 'P_WORK_ORDER'    .        APPEND l_vartable.

    l_vartable-atwrt = wa_7jb-extc       .        CLEAR: l_conf-atwrt.
    l_vartable-atnam = 'P_EXT_COLOR'     .        APPEND l_vartable.

    l_vartable-atwrt = wa_7jb-intc       .        CLEAR: l_conf-atwrt.
    l_vartable-atnam = 'P_INT_COLOR'     .        APPEND l_vartable.

    clear: l_conf, l_conf[].

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
      PERFORM create_log USING '8' text-001 .
      wa_error = 'X'      .
    ELSE.
      WRITE:/ wa_7jb-equnr, 'updated'.
      commit work.
    ENDIF.
    CLEAR wa_7jb.
    l_recno = l_recno + 1.
*    if l_recno = 50 or l_recno = 100 or l_recno = 150.
       concatenate 'processed rec.' l_recno wa_7jb-equnr into l_mess.
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
    IF screen-group1 = 'GR1'.
      CHECK screen-name = 'P_FILETY'.
      screen-input = '0'.
      MODIFY SCREEN.
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
form display_progress_bar using    p_text.
  DATA: lw_text(50).

  MOVE: p_text TO lw_text.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            text = lw_text.

endform.                    " display_progress_bar
