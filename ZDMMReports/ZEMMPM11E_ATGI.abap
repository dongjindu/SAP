************************************************************************
* Program Name      : ZEMMPM11E_ATGI
* Author            : SeungJae, Lee
* Creation Date     : 2003.08.05.
* Specifications By : SeungJae, Lee
* Development Request No : UD1K901842
* Addl Documentation:
* Description       : Batch Program for Automatic Goods Issue
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT  zemmpm11e_atgi NO STANDARD PAGE HEADING  MESSAGE-ID zmmm.

TYPE-POOLS: slis, kkblo.

TABLES: t001,  " Company Codes
        t158b, " Check Table: Movement Type for Transaction Code.
        mara,  " Material Master
        t001w, " Plants/Branches
        t001l, " Storage Locations
        ska1,  " G/L Account Master (Chart of Accounts)
        csks,  " Cost Center Master Data
        t157d. " Reason for Movement

*  Internal table for File upload
DATA: BEGIN OF it_records OCCURS 0,
        bwart(3),                 "Mvmt Type
        bldat(10),                "Doc.Date
        budat(10),                "Posting Date
        mtsnr(16),                "Material Slip
        grund(4),                 "Reason for movement
        matnr(18),                "Material Number
        erfmg(17),                "Quantity
        werks(4),                 "Plant
        lgort(4),                 "Storage Locaton
        wempf(12),                "Recipient
        saknr(10),                "GL Account
        kostl(10),                "Cost Center
        name1 LIKE t001w-name1,   "Plant Text
        lgobe LIKE t001l-lgobe,   "Storage Location Text
      END   OF it_records.

*   main Internal table
TYPES: BEGIN OF it_main_type,
        bldat LIKE mkpf-bldat,    "(key)Doc.Date
        budat LIKE mkpf-budat,    "(key)Posting Date
        bwart LIKE mseg-bwart,    "(key)Mvmt Type
        werks LIKE mseg-werks,    "(key)Plant
        lgort LIKE mseg-lgort,    "(key)Storage Location
        mtsnr LIKE gohead-mtsnr,  "(key)Material Slip
        grund LIKE mseg-grund,    "(key)Reason for movement
        matnr LIKE mseg-matnr,    "Material Number
        maktx LIKE makt-maktx,    "Material Text
        erfmg LIKE mseg-erfmg,    "Quantity
        name1 LIKE t001w-name1,   "Plant Text
        lgobe LIKE t001l-lgobe,   "Storage Location Text
        wempf LIKE mseg-wempf,    "Recipient
        saknr LIKE mseg-sakto,    "GL Account
        txt20 LIKE skat-txt20,    "GL Account Text
        kostl LIKE mseg-kostl,    "Cost Center
        ktext LIKE cskt-ktext,    "Cost Center Text
        mblnr LIKE mkpf-mblnr,    "Number of material document
        mjahr LIKE mkpf-mjahr,    "Material doc. year
        linecolor(4),             "ALV color
      END   OF it_main_type.
DATA: it_main TYPE STANDARD TABLE OF it_main_type
      WITH KEY budat bldat bwart werks lgort mtsnr grund matnr
      WITH HEADER LINE.

*   Material Text
DATA: BEGIN OF it_matnr OCCURS 0,
        matnr LIKE mara-matnr,
      END   OF it_matnr.
DATA: BEGIN OF it_maktx OCCURS 0,
        matnr LIKE mara-matnr,
        maktx LIKE makt-maktx,
      END   OF it_maktx.

*    Cost Center Text
DATA: BEGIN OF it_kostl OCCURS 0,
        kostl LIKE csks-kostl,
      END   OF it_kostl.
DATA: BEGIN OF it_ktext OCCURS 0,
        kostl LIKE csks-kostl,
        ktext LIKE cskt-ktext,
      END   OF it_ktext.

*    GL Account Text
DATA: BEGIN OF it_saknr OCCURS 0,
        saknr LIKE ska1-saknr,
      END   OF it_saknr.
DATA: BEGIN OF it_sktxt OCCURS 0,
        saknr LIKE ska1-saknr,
        txt20 LIKE skat-txt20,
      END   OF it_sktxt.

*    ALV Structure
DATA: rep         LIKE sy-repid,
      wa_layout   TYPE slis_layout_alv,
      it_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
      it_sort     TYPE slis_t_sortinfo_alv WITH HEADER LINE.

CONSTANTS : c_bukrs LIKE t001-bukrs  VALUE 'H201', " HYUNDAE Co code
            c_kokrs LIKE tka01-kokrs VALUE 'H201', " Hyundai Motor
            c_red(4)                 VALUE 'C610', " Red Color
            c_green(4)               VALUE 'C510'. " Green Color

*    BAPI Structure
DATA: wa_goodsmvt_header  LIKE bapi2017_gm_head_01,
      wa_goodsmvt_code    LIKE bapi2017_gm_code,
      p_testrun           LIKE bapi2017_gm_gen-testrun,
      wa_goodsmvt_headret LIKE bapi2017_gm_head_ret,
      p_materialdocument  LIKE bapi2017_gm_head_ret-mat_doc,
      p_matdocumentyear   LIKE bapi2017_gm_head_ret-doc_year,
      it_goodsmvt_item
           LIKE TABLE OF bapi2017_gm_item_create  WITH HEADER LINE,
      it_goodsmvt_serialnumber
           LIKE TABLE OF bapi2017_gm_serialnumber WITH HEADER LINE,
      it_return
           LIKE TABLE OF bapiret2                 WITH HEADER LINE.

*     BAPI Return Deep Structure
DATA BEGIN OF it_return_deep OCCURS 0.
DATA:   bldat LIKE mkpf-bldat,    "Doc.Date
        budat LIKE mkpf-budat,    "Posting Date
        bwart LIKE mseg-bwart,    "Mvmt Type
        werks LIKE mseg-werks,    "Plant
        lgort LIKE mseg-lgort,    "Storage Location
        mtsnr LIKE gohead-mtsnr,  "Material Slip
        grund LIKE mseg-grund,    "Reason for movement
        mblnr LIKE mkpf-mblnr,    "Number of material document
        mjahr LIKE mkpf-mjahr.    "Material doc. year
DATA   return LIKE TABLE OF it_return.
DATA END   OF it_return_deep.

DATA: BEGIN OF it_log OCCURS 0,
        line(255),
      END   OF it_log.

*     Text Editor
DATA: it_text(220) OCCURS 0 WITH HEADER LINE.
DATA: p_text(50).

*     Others
DATA: p_run_flg,
      p_filename LIKE rlgrap-filename.

START-OF-SELECTION.

  PERFORM upload.

  PERFORM check_records.

  PERFORM build_main_itab.

END-OF-SELECTION.
  CLEAR: p_run_flg.
  PERFORM display_list_first.

*&---------------------------------------------------------------------*
*&      Form  UPLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM upload.
  DATA: p_cancel.


  CLEAR:  it_records, p_filename.
  REFRESH it_records.

  CALL FUNCTION 'UPLOAD'
    EXPORTING
*     CODEPAGE                      = ' '
      filename                      = 'c:\'
      filetype                      = 'DAT'
      item                          = text-010
*     filemask_mask                 = ' '
*     FILEMASK_TEXT                 =
*     FILETYPE_NO_CHANGE            = ' '
*     FILEMASK_ALL                  = ' '
      filetype_no_show              = 'X'
*     LINE_EXIT                     = ' '
*     USER_FORM                     = ' '
*     USER_PROG                     = ' '
*     SILENT                        = 'S'
    IMPORTING
*     FILESIZE                      =
      cancel                        = p_cancel
      act_filename                  = p_filename
*     ACT_FILETYPE                  =
    TABLES
      data_tab                      = it_records
    EXCEPTIONS
      conversion_error              = 1
      invalid_table_width           = 2
      invalid_type                  = 3
      no_batch                      = 4
      unknown_error                 = 5
      gui_refuse_filetransfer       = 6
      OTHERS                        = 7.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    IF p_cancel = space.
      IF it_records[] IS INITIAL.
        MESSAGE i999 WITH text-003.
        LEAVE PROGRAM.
      ENDIF.
    ELSE.
      LEAVE PROGRAM.
    ENDIF.
  ENDIF.
ENDFORM.                    " UPLOAD
*&---------------------------------------------------------------------*
*&      Form  CONVERT_TO_DATE_INTERN_FORMAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_RECORDS_BUDAT  text
*      <--P_IT_RECORDS_BUDAT  text
*      <--P_WA_ERROR_FLG  text
*----------------------------------------------------------------------*
FORM convert_to_date_intern_format USING    p_input_date
                                   CHANGING p_output_date
                                            p_error_flg.
  CLEAR: p_error_flg.
  CALL FUNCTION 'CONVERT_DATE_TO_INTERN_FORMAT'
    EXPORTING
      datum         = p_input_date
      dtype         = 'DATS'
    IMPORTING
      error         = p_error_flg
      idate         = p_output_date
*      MESSG         =
*      MSGLN         =
  .
ENDFORM.                    " CONVERT_TO_DATE_INTERN_FORMAT
*&---------------------------------------------------------------------*
*&      Form  CONVERSION_MATN2_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_RECORDS_MATNR  text
*      <--P_IT_RECORDS_MATNR  text
*----------------------------------------------------------------------*
FORM conversion_matn2_input USING    input
                            CHANGING output.
  CALL FUNCTION 'CONVERSION_EXIT_MATN2_INPUT'
       EXPORTING
            input            = input
       IMPORTING
            output           = output
       EXCEPTIONS
            number_not_found = 1
            length_error     = 2
            OTHERS           = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " CONVERSION_MATN2_INPUT
*&---------------------------------------------------------------------*
*&      Form  get_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_text.

  CLEAR:   it_maktx, it_ktext, it_sktxt.
  REFRESH: it_maktx, it_ktext, it_sktxt.

* Material Text
  IF NOT it_matnr[] IS INITIAL.
    SELECT DISTINCT matnr maktx FROM makt
      INTO TABLE it_maktx
       FOR ALL ENTRIES IN it_matnr
     WHERE matnr = it_matnr-matnr
       AND spras = sy-langu.
  ENDIF.

* GL Account Text
  IF NOT it_saknr[] IS INITIAL.
    SELECT DISTINCT saknr txt20 FROM skat
      INTO TABLE it_sktxt
       FOR ALL ENTRIES IN it_saknr
     WHERE spras = sy-langu
       AND ktopl = t001-ktopl
       AND saknr = it_saknr-saknr.
  ENDIF.

* Cost Center Text
  IF NOT it_kostl[] IS INITIAL.
    SELECT DISTINCT kostl ktext FROM cskt
      INTO TABLE it_ktext
       FOR ALL ENTRIES IN it_kostl
     WHERE spras = sy-langu
       AND kokrs = c_kokrs
       AND kostl = it_kostl-kostl.
  ENDIF.
ENDFORM.                    " get_text
*&---------------------------------------------------------------------*
*&      Form  check_records
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_records.
  DATA: p_error_flg,
        p_index TYPE i.

  CLEAR:   it_matnr, it_saknr, it_kostl.
  REFRESH: it_matnr, it_saknr, it_kostl.

  SELECT SINGLE * FROM t001 WHERE bukrs = c_bukrs.

  LOOP AT it_records.
    p_index = sy-tabix.

*   CHECK Mvmt Type
    SELECT SINGLE * FROM t158b
     WHERE tcode = 'MIGO'
       AND bwart = it_records-bwart
       AND xkzbew <> '2'   "Manual input not allowed
       AND bwart  <> '605'
       AND bwart  <> '606'.
    IF sy-subrc NE 0.
      MESSAGE e999 WITH text-001 p_index text-002.
    ENDIF.
*   CHECK DATE
    PERFORM convert_to_date_intern_format USING    it_records-bldat
                                          CHANGING it_records-bldat
                                                   p_error_flg.
    IF p_error_flg NE space.
      MESSAGE e999 WITH text-001 p_index text-009.
    ENDIF.

    PERFORM convert_to_date_intern_format USING    it_records-budat
                                          CHANGING it_records-budat
                                                   p_error_flg.
    IF p_error_flg NE space.
      MESSAGE e999 WITH text-001 p_index text-009.
    ENDIF.
*   CHECK Material No.
    TRANSLATE it_records-matnr TO UPPER CASE.
    PERFORM conversion_matn2_input USING    it_records-matnr
                                   CHANGING it_records-matnr.

    SELECT SINGLE * FROM mara
     WHERE matnr = it_records-matnr.
    IF sy-subrc NE 0.
      MESSAGE e999 WITH text-001 p_index text-004.
    ENDIF.
    it_matnr = it_records-matnr. COLLECT it_matnr. CLEAR it_matnr.
*   CHECK Plant
    IF it_records-werks NE space.
      TRANSLATE it_records-werks TO UPPER CASE.
      SELECT SINGLE * FROM t001w
       WHERE werks = it_records-werks.
      IF sy-subrc NE 0.
        MESSAGE e999 WITH text-001 p_index text-006.
      ENDIF.
      it_records-name1 = t001w-name1.
    ENDIF.
*   CHECK Storage Location
    IF it_records-lgort NE space.
      TRANSLATE it_records-lgort TO UPPER CASE.
      SELECT SINGLE * FROM t001l
       WHERE werks = it_records-werks
         AND lgort = it_records-lgort.
      IF sy-subrc NE 0.
        MESSAGE e999 WITH text-001 p_index text-005.
      ENDIF.
      it_records-lgobe = t001l-lgobe.
    ENDIF.
*   CHECK Recipient
*   CHECK GL Account
    IF it_records-saknr NE space.
      PERFORM conversion_alpha_input USING    it_records-saknr
                                     CHANGING it_records-saknr.
      SELECT SINGLE * FROM ska1
         WHERE ktopl = t001-ktopl
           AND saknr = it_records-saknr.
*           AND xloev = space
*           AND xspea = space
*           AND xspeb = space
*           AND xspep = space.
      IF sy-subrc NE 0.
        MESSAGE e999 WITH text-001 p_index text-007.
      ENDIF.
      it_saknr = it_records-saknr. COLLECT it_saknr. CLEAR it_saknr.
    ENDIF.
*   CHECK Cost Center
    IF it_records-kostl NE space.
      PERFORM conversion_alpha_input USING    it_records-kostl
                                     CHANGING it_records-kostl.
      SELECT SINGLE * FROM csks
        WHERE kokrs = c_kokrs
          AND kostl = it_records-kostl
          AND datbi => sy-datum
          AND datab =< sy-datum.
*          AND bkzks = space
*          AND bkzer = space
*          AND bkzob = space
*          AND pkzks = space
*          AND pkzer = space.
      IF sy-subrc NE 0.
        MESSAGE e999 WITH text-001 p_index text-008.
      ENDIF.
      it_kostl = it_records-kostl. COLLECT it_kostl. CLEAR it_kostl.
    ENDIF.
*   Check Quantity
    TRANSLATE it_records-erfmg USING ', '.
    CONDENSE it_records-erfmg.
*   CHECK Reason for Movement
    IF it_records-grund > 0.
      SELECT SINGLE * FROM t157d
       WHERE bwart = it_records-bwart
         AND grund = it_records-grund.
      IF sy-subrc NE 0.
        MESSAGE e999 WITH text-001 p_index text-011.
      ENDIF.
    ENDIF.
    MODIFY it_records.
  ENDLOOP.

ENDFORM.                    " check_records
*&---------------------------------------------------------------------*
*&      Form  build_main_itab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_main_itab.
  CLEAR:   it_main.
  REFRESH: it_main.
  PERFORM get_text.

  LOOP AT it_records.
    MOVE-CORRESPONDING it_records TO it_main.
    READ TABLE it_maktx WITH KEY matnr = it_main-matnr.
    READ TABLE it_ktext WITH KEY kostl = it_main-kostl.
    READ TABLE it_sktxt WITH KEY saknr = it_main-saknr.
    it_main-maktx = it_maktx-maktx.
    it_main-ktext = it_ktext-ktext.
    it_main-txt20 = it_sktxt-txt20.
    APPEND it_main.
  ENDLOOP.

*  SORT it_main  BY budat bldat bwart werks lgort mtsnr matnr.

ENDFORM.                    " build_main_itab
*&---------------------------------------------------------------------*
*&      Form  display_list_first
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_list_first.
  CLEAR: rep.
  rep = sy-repid.

  wa_layout-info_fieldname = 'LINECOLOR'.
*  wa_layout-no_toolbar = 'X'.
  PERFORM build_fieldcat.

  PERFORM build_sort.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
  EXPORTING
*     I_INTERFACE_CHECK              = ' '
     i_bypassing_buffer             = 'X'
     i_buffer_active                = 'X'
     i_callback_program             = rep
     i_callback_pf_status_set       = 'STATUS_SET'
     i_callback_user_command        = 'USER_COMMAND'
*     I_STRUCTURE_NAME               =
     is_layout                      = wa_layout
     it_fieldcat                    = it_fieldcat[]
*     IT_EXCLUDING                   =
*     IT_SPECIAL_GROUPS              =
     it_sort                        = it_sort[]
*     IT_FILTER                      =
*     IS_SEL_HIDE                    =
     i_default                      = 'X'
     i_save                         = 'A'
*     IS_VARIANT                     =
*     IT_EVENTS                      =
*     IT_EVENT_EXIT                  =
*     IS_PRINT                       =
*     IS_REPREP_ID                   =
*     I_SCREEN_START_COLUMN          = 0
*     I_SCREEN_START_LINE            = 0
*     I_SCREEN_END_COLUMN            = 0
*     I_SCREEN_END_LINE              = 0
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER        =
*     ES_EXIT_CAUSED_BY_USER         =
  TABLES
     t_outtab                       = it_main
  EXCEPTIONS
     program_error                  = 1
     OTHERS                         = 2.            .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " display_list_first
*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcat.

  CLEAR   it_fieldcat.
  REFRESH it_fieldcat.
  CHECK it_fieldcat[] IS INITIAL.
  PERFORM append_fieldcat USING 'MBLNR'
                                10
                                'Material Doc.No'
                                'CHAR'
                                ''
                                'X'.
  PERFORM append_fieldcat USING 'MJAHR'
                                4
                                'Material doc. year'
                                'NUMC'
                                ''
                                'X'.
  PERFORM append_fieldcat USING 'BWART'
                                3
                                'Mvmt Type'
                                'CHAR'
                                'X'
                                ''.
  PERFORM append_fieldcat USING 'WERKS'
                                4
                                'Plant'
                                'CHAR'
                                ''
                                ''.
  PERFORM append_fieldcat USING 'NAME1'
                                30
                                'Plant Text'
                                'CHAR'
                                ''
                                ''.
  PERFORM append_fieldcat USING 'LGORT'
                                4
                                'Storage Location'
                                'CHAR'
                                ''
                                ''.
  PERFORM append_fieldcat USING 'LGOBE'
                                16
                                'Storage Location Text'
                                'CHAR'
                                ''
                                ''.
  PERFORM append_fieldcat USING 'BLDAT'
                                10
                                'Doc.Date'
                                'DATS'
                                'X'
                                ''.
  PERFORM append_fieldcat USING 'BUDAT'
                                10
                                'Posting Date'
                                'DATS'
                                'X'
                                ''.
  PERFORM append_fieldcat USING 'MTSNR'
                                16
                                'Material Slip'
                                'CHAR'
                                ''
                                ''.
  PERFORM append_fieldcat USING 'GRUND'
                                4
                                'Reason for movement'
                                'NUMC'
                                ''
                                ''.
  PERFORM append_fieldcat USING 'MATNR'
                                18
                                'Material Number'
                                'CHAR'
                                ''
                                ''.
  PERFORM append_fieldcat USING 'MAKTX'
                                40
                                'Material Text'
                                'CHAR'
                                ''
                                ''.
  PERFORM append_fieldcat USING 'ERFMG'
                                13
                                'Quantity'
                                'QUAN'
                                ''
                                ''.
  PERFORM append_fieldcat USING 'WEMPF'
                                12
                                'Recipient'
                                'CHAR'
                                ''
                                ''.
  PERFORM append_fieldcat USING 'SAKNR'
                                10
                                'GL Account'
                                'CHAR'
                                ''
                                ''.
  PERFORM append_fieldcat USING 'TXT20'
                                20
                                'GL Account Text'
                                'CHAR'
                                ''
                                ''.
  PERFORM append_fieldcat USING 'KOSTL'
                                10
                                'Cost Center'
                                'CHAR'
                                ''
                                ''.
  PERFORM append_fieldcat USING 'KTEXT'
                                20
                                'Cost Center Text'
                                'CHAR'
                                ''
                                ''.

ENDFORM.                    " build_fieldcat
*&---------------------------------------------------------------------*
*&      Form  CONVERSION_ALPHA_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_RECORDS_SAKNR  text
*      <--P_IT_RECORDS_SAKNR  text
*----------------------------------------------------------------------*
FORM conversion_alpha_input USING    p_input
                            CHANGING p_output.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
            input  = p_input
       IMPORTING
            output = p_output.
ENDFORM.                    " CONVERSION_ALPHA_INPUT
*&---------------------------------------------------------------------*
*&      Form  append_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0918   text
*      -->P_10     text
*      -->P_0920   text
*      -->P_0921   text
*      -->P_0922   text
*      -->P_0923   text
*----------------------------------------------------------------------*
FORM append_fieldcat USING    p_fieldname
                              p_outputlen
                              p_text
                              p_datatype
                              p_key
                              p_no_out.
  it_fieldcat-fieldname = p_fieldname.
  it_fieldcat-outputlen = p_outputlen.
  it_fieldcat-seltext_l = p_text.
  it_fieldcat-seltext_m = p_text.
  it_fieldcat-seltext_s = p_text.
  it_fieldcat-datatype  = p_datatype.
  it_fieldcat-key       = p_key.
  it_fieldcat-no_out    = p_no_out.
  APPEND it_fieldcat. CLEAR it_fieldcat.
ENDFORM.                    " append_fieldcat
*--------------------------------------------------------------------*
*
*  FORM status_set
*--------------------------------------------------------------------*
*
*
*--------------------------------------------------------------------*
FORM status_set USING rt_extab TYPE slis_t_extab.
  DATA: p_numbering TYPE i.
  DESCRIBE TABLE it_main LINES p_numbering.
  SET TITLEBAR  'ALV001' WITH p_numbering.
  IF p_run_flg = space.
    SET PF-STATUS 'ALV001' EXCLUDING 'LOG'.
  ELSE.
    SET PF-STATUS 'ALV001' EXCLUDING 'REGIST'.
  ENDIF.

ENDFORM.                               " STATUS_SET
*---------------------------------------------------------------------
*
*       FORM USER_COMMAND
*
*---------------------------------------------------------------------
*
*       ........
*
*---------------------------------------------------------------------
*
*  -->  UCOMM
*
*  -->  SELFIELD
*
*---------------------------------------------------------------------
*
FORM user_command USING ucomm LIKE sy-ucomm
                       selfield TYPE slis_selfield.
  CASE ucomm.
    WHEN 'REGIST'.
      PERFORM call_bapi_gi_create.
*     Refresh Table
      selfield-refresh = 'X'.
      it_fieldcat-no_out = ''.
      MODIFY it_fieldcat TRANSPORTING  no_out
       WHERE fieldname = 'MBLNR'
          OR fieldname = 'MJAHR'.
      CALL FUNCTION 'REUSE_ALV_LIST_LAYOUT_INFO_SET'
        EXPORTING
*         IS_LAYOUT            =
          it_fieldcat          = it_fieldcat[]
*         IT_SORT              =
*         IT_FILTER            =
*         IS_LIST_SCROLL       =
                .

    WHEN '&IC1'. " Double Click
      PERFORM display_return USING selfield-tabindex.
    WHEN 'LOG'.  " Log File
      PERFORM display_log_download.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  call_bapi_gi_create
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_bapi_gi_create.
  CLEAR:   wa_goodsmvt_header,  wa_goodsmvt_code,   p_testrun,
           wa_goodsmvt_headret, p_materialdocument, p_matdocumentyear,
           it_goodsmvt_item,    it_goodsmvt_serialnumber, it_return,
           it_return_deep.
  REFRESH: it_goodsmvt_item,    it_goodsmvt_serialnumber, it_return,
           it_return_deep.

  PERFORM build_bapi_goodsmvt_parameter.
ENDFORM.                    " call_bapi_gi_create
*&---------------------------------------------------------------------*
*&      Form  build_BAPI_goodsmvt_parameter
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_bapi_goodsmvt_parameter.
  DATA: p_new_flg,
        p_end_flg.

  CONSTANTS c_gm_code LIKE wa_goodsmvt_code-gm_code VALUE '03'.
  CLEAR:   wa_goodsmvt_header,  wa_goodsmvt_code,
           it_goodsmvt_item,    it_goodsmvt_serialnumber, it_return,
           p_run_flg.

*  SORT it_main  BY budat bldat bwart werks lgort mtsnr matnr.

  LOOP AT it_main WHERE linecolor = space.
    CLEAR: p_new_flg, p_end_flg.
    AT NEW budat. p_new_flg = 'X'. ENDAT.
    AT NEW bldat. p_new_flg = 'X'. ENDAT.
    AT NEW bwart. p_new_flg = 'X'. ENDAT.
    AT NEW werks. p_new_flg = 'X'. ENDAT.
    AT NEW lgort. p_new_flg = 'X'. ENDAT.
    AT NEW mtsnr. p_new_flg = 'X'. ENDAT.
    AT NEW grund. p_new_flg = 'X'. ENDAT.
    IF p_new_flg = 'X'.
      CLEAR:   wa_goodsmvt_header, wa_goodsmvt_code, it_goodsmvt_item,
               it_return.
      REFRESH: it_goodsmvt_item, it_return.
      wa_goodsmvt_header-pstng_date    = it_main-budat. "Post date
      wa_goodsmvt_header-doc_date      = it_main-bldat. "Doc date
      wa_goodsmvt_header-ref_doc_no    = it_main-mtsnr. "Material Slip
      wa_goodsmvt_header-pr_uname      = sy-uname.      "System User
      wa_goodsmvt_code-gm_code         = c_gm_code."Goods Movement code
    ENDIF.
    it_goodsmvt_item-move_reas      = it_main-grund. "Rsn for movement
    it_goodsmvt_item-material       = it_main-matnr. "Material No.
    it_goodsmvt_item-plant          = it_main-werks. "Plant
    it_goodsmvt_item-stge_loc       = it_main-lgort. "StorageLocation
    it_goodsmvt_item-move_type      = it_main-bwart. "Movement Type
    it_goodsmvt_item-entry_qnt      = it_main-erfmg. "Quantity
    it_goodsmvt_item-costcenter     = it_main-kostl. "Cost Center
    it_goodsmvt_item-gl_account     = it_main-saknr. "GL Account
    it_goodsmvt_item-gr_rcpt        = it_main-wempf. "Recipient
    APPEND it_goodsmvt_item. CLEAR it_goodsmvt_item.

    AT END OF budat. p_end_flg = 'X'. ENDAT.
    AT END OF bldat. p_end_flg = 'X'. ENDAT.
    AT END OF bwart. p_end_flg = 'X'. ENDAT.
    AT END OF werks. p_end_flg = 'X'. ENDAT.
    AT END OF lgort. p_end_flg = 'X'. ENDAT.
    AT END OF mtsnr. p_end_flg = 'X'. ENDAT.
    AT END OF grund. p_end_flg = 'X'. ENDAT.

    CHECK p_end_flg = 'X'.
    PERFORM bapi_goodsmvt_create.

  ENDLOOP.
  p_run_flg = 'X'.
ENDFORM.                    " build_BAPI_goodsmvt_parameter
*&---------------------------------------------------------------------*
*&      Form  BAPI_GOODSMVT_CREATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bapi_goodsmvt_create.
  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
       EXPORTING
            goodsmvt_header       = wa_goodsmvt_header
            goodsmvt_code         = wa_goodsmvt_code
            testrun               = p_testrun
       IMPORTING
            goodsmvt_headret      = wa_goodsmvt_headret
            materialdocument      = p_materialdocument
            matdocumentyear       = p_matdocumentyear
       TABLES
            goodsmvt_item         = it_goodsmvt_item
            goodsmvt_serialnumber = it_goodsmvt_serialnumber
            return                = it_return.
  READ TABLE it_return WITH KEY type = 'E'.
  IF sy-subrc = 0.
    REFRESH: it_return_deep-return.
    ROLLBACK WORK.
    it_main-linecolor = c_red.  "red
    MODIFY it_main TRANSPORTING linecolor
     WHERE budat = it_main-budat
       AND bldat = it_main-bldat
       AND bwart = it_main-bwart
       AND werks = it_main-werks
       AND lgort = it_main-lgort
       AND mtsnr = it_main-mtsnr.
    MOVE-CORRESPONDING it_main TO it_return_deep.
    it_return_deep-return[] = it_return[].
    APPEND it_return_deep. CLEAR it_return_deep.
  ELSE.
    COMMIT WORK AND WAIT.

    it_main-linecolor = c_green.  "green
    it_main-mblnr = p_materialdocument.
    it_main-mjahr = p_matdocumentyear.
    MODIFY it_main TRANSPORTING linecolor mblnr mjahr
     WHERE budat = it_main-budat
       AND bldat = it_main-bldat
       AND bwart = it_main-bwart
       AND werks = it_main-werks
       AND lgort = it_main-lgort
       AND mtsnr = it_main-mtsnr.
    MOVE-CORRESPONDING it_main TO it_return_deep.
    it_return_deep-mblnr = p_materialdocument.
    it_return_deep-mjahr = p_matdocumentyear.
    APPEND it_return_deep. CLEAR it_return_deep.
  ENDIF.
ENDFORM.                    " BAPI_GOODSMVT_CREATE
*&---------------------------------------------------------------------*
*&      Form  display_return
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_return USING p_tabix.
  CLEAR   it_return.
  REFRESH it_return.

  READ TABLE it_main INDEX p_tabix.
  CHECK sy-subrc = 0.

  CASE it_main-linecolor.
    WHEN c_red.
      READ TABLE it_return_deep WITH KEY budat = it_main-budat
                                         bldat = it_main-bldat
                                         bwart = it_main-bwart
                                         werks = it_main-werks
                                         lgort = it_main-lgort
                                         mtsnr = it_main-mtsnr.
      it_return[] = it_return_deep-return[].

      CLEAR:  it_text, p_text.
      REFRESH it_text.
      LOOP AT it_return.
        CONCATENATE it_return-type '-' it_return-message
               INTO it_text.
        APPEND it_text. CLEAR it_text.
      ENDLOOP.
      CONCATENATE it_return_deep-bldat '|' it_return_deep-budat '|'
                  it_return_deep-bwart '|' it_return_deep-werks '|'
                  it_return_deep-lgort '|' it_return_deep-mtsnr '|'
                  it_return_deep-grund
             INTO p_text.
      CALL FUNCTION 'Z_FMM_TEXTEDIT'
        EXPORTING
*         EDIT               =
          title              = p_text
          line_length        = 120
          start_column       = 10
          start_row          = 3
          end_column         = 130
          end_row            = 10
        TABLES
          texttab            = it_text.

    WHEN c_green.
      READ TABLE it_return_deep WITH KEY budat = it_main-budat
                                         bldat = it_main-bldat
                                         bwart = it_main-bwart
                                         werks = it_main-werks
                                         lgort = it_main-lgort
                                         mtsnr = it_main-mtsnr
                                         grund = it_main-grund.
      MESSAGE i999 WITH 'Material Document : ' it_return_deep-mblnr
                        'Is Created'.
    WHEN OTHERS.
      EXIT.
  ENDCASE.

ENDFORM.                    " display_return
*&---------------------------------------------------------------------*
*&      Form  BUILD_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_sort.
  CLEAR   it_sort.
  REFRESH it_sort.
  CHECK it_sort[] IS INITIAL.
  it_sort-spos = '1'.
  it_sort-fieldname = 'BUDAT'.
  it_sort-up = 'X'.
  APPEND it_sort.
  it_sort-spos = '2'.
  it_sort-fieldname = 'BLDAT'.
  it_sort-up = 'X'.
  APPEND it_sort.
  it_sort-spos = '3'.
  it_sort-fieldname = 'BWART'.
  it_sort-up = 'X'.
  APPEND it_sort.
  it_sort-spos = '4'.
  it_sort-fieldname = 'WERKS'.
  it_sort-up = 'X'.
  APPEND it_sort.
  it_sort-spos = '5'.
  it_sort-fieldname = 'LGORT'.
  it_sort-up = 'X'.
  APPEND it_sort.
  it_sort-spos = '6'.
  it_sort-fieldname = 'MTSNR'.
  it_sort-up = 'X'.
  APPEND it_sort.
  it_sort-spos = '7'.
  it_sort-fieldname = 'GRUND'.
  it_sort-up = 'X'.
  APPEND it_sort.
  it_sort-spos = '8'.
  it_sort-fieldname = 'MATNR'.
  it_sort-up = 'X'.
  APPEND it_sort.
ENDFORM.                    " BUILD_SORT
*&---------------------------------------------------------------------*
*&      Form  Display_Log_Download
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_log_download.
  CLEAR   it_log.
  REFRESH it_log.
  LOOP AT it_return_deep.
    CONCATENATE '******'
                'Doc.Date:'            it_return_deep-bldat ','
                'Posting Date:'        it_return_deep-budat ','
                'Plant:'               it_return_deep-werks ','
                'Storage Loc:'         it_return_deep-lgort ','
                'Material Slip:'       it_return_deep-mtsnr ','
                'Reason for movement:' it_return_deep-grund
                '******'
           INTO it_log SEPARATED BY space.
    APPEND it_log. CLEAR it_log.
    IF NOT it_return_deep-mblnr IS INITIAL.
      CONCATENATE '       '
                  'Material Doc.No-' it_return_deep-mblnr
             INTO it_log SEPARATED BY space.
      APPEND it_log. CLEAR it_log.
    ENDIF.
    IF NOT it_return_deep-return[] IS INITIAL.
      CLEAR it_return. REFRESH it_return.
      it_return[] = it_return_deep-return[].
      LOOP AT it_return.
        CONCATENATE '       '     it_return-type '-'
                    it_return-message
               INTO it_log SEPARATED BY space.
        APPEND it_log. CLEAR it_log.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

* Write Log
  LOOP AT it_log.
    WRITE: / it_log.
  ENDLOOP.
* Download
  PERFORM download.

ENDFORM.                    " Display_Log_Download
*&---------------------------------------------------------------------*
*&      Form  download
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM download.
  DATA: p_file_path     LIKE p_filename,
        p_stripped_name LIKE p_filename.
  CHECK NOT it_log[] IS INITIAL.
  PERFORM split_file_path_and_name USING    p_filename
                                   CHANGING p_file_path
                                            p_stripped_name.
  CONCATENATE p_file_path '\' 'ATGI_Log' sy-datum '_' sy-uzeit '.txt'
         INTO p_filename.
  CONDENSE p_filename.
  CALL FUNCTION 'DOWNLOAD'
    EXPORTING
*     BIN_FILESIZE                  = ' '
*     CODEPAGE                      = ' '
      filename                      = p_filename
      filetype                      = 'DAT'
      item                          = text-010
*     MODE                          = ' '
*     WK1_N_FORMAT                  = ' '
*     WK1_N_SIZE                    = ' '
*     WK1_T_FORMAT                  = ' '
*     WK1_T_SIZE                    = ' '
*     FILEMASK_MASK                 = ' '
*     FILEMASK_TEXT                 = ' '
*     FILETYPE_NO_CHANGE            = ' '
*     FILEMASK_ALL                  = ' '
      filetype_no_show              = 'X'
*     SILENT                        = 'S'
*     COL_SELECT                    = ' '
*     COL_SELECTMASK                = ' '
*     NO_AUTH_CHECK                 = ' '
*   IMPORTING
*     ACT_FILENAME                  =
*     ACT_FILETYPE                  =
*     FILESIZE                      =
*     CANCEL                        =
    TABLES
      data_tab                      = it_log
*     FIELDNAMES                    =
   EXCEPTIONS
     invalid_filesize              = 1
     invalid_table_width           = 2
     invalid_type                  = 3
     no_batch                      = 4
     unknown_error                 = 5
     gui_refuse_filetransfer       = 6
     customer_error                = 7
     OTHERS                        = 8
            .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " download
*&---------------------------------------------------------------------*
*&      Form  split_file_path_and_name
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM split_file_path_and_name USING p_filename
                              CHANGING p_file_path
                                       p_stripped_name.
  CALL FUNCTION 'SO_SPLIT_FILE_AND_PATH'
       EXPORTING
            full_name     = p_filename
       IMPORTING
            stripped_name = p_stripped_name
            file_path     = p_file_path
       EXCEPTIONS
            x_error       = 1
            OTHERS        = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " split_file_path_and_name
