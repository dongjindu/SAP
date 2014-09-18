************************************************************************
* Program Name      : ZACO31U_SKF4_NEW_XLS
* Author            : Hyesun , Jung
* Creation Date     : 2006.11.16.
* Specifications By : Andy Choi
* Description       : Create No of Persons by Department(Actual&Plan)
*                     (Excel upload & Posting )
************************************************************************

REPORT  zaco31u_skf4_upload MESSAGE-ID zmco.

** type-pools
TYPE-POOLS: slis.

TABLES : csks, pa0001, pa0000.   ", ZTCO_SKF_MANAGE.

* Constants
CONSTANTS: c_skf TYPE stagr VALUE 'CS001'.


* Main internal table
DATA : BEGIN OF it_upload OCCURS 0,
        skf(10),
        verid(3),
        kostl   LIKE  csks-kostl.
        INCLUDE STRUCTURE ZSCO_COSS_MEG01.  "1~12 QTY
DATA : END OF  it_upload.

DATA : BEGIN OF it_post_act OCCURS 0,
         cgroup(15) type c,  " LIKE it_upload-kostl,
         count      LIKE it_upload-meg001.
DATA : END OF  it_post_act.

DATA : it_post_pln LIKE it_upload OCCURS 0 WITH HEADER LINE.

DATA : first_day TYPE sy-datum,
       last_day TYPE sy-datum.
** For BAPI

* Macro For Transferring value in BAPI
DEFINE trans_value.
  it_pervalue-quantity_per&1  =  it_post_pln-meg0&1.
END-OF-DEFINITION.

DATA : it_return         LIKE STANDARD TABLE OF bapiret2
                         WITH HEADER LINE.
*Plan posting
DATA : wa_headerinfo     LIKE bapiplnhdr_skf.

*Actual posting
data : wa_doc_header     like bapidochdrp .
data : it_doc_items  like standard table of bapiskfitm
                     with header line.

DATA : it_indexstructure LIKE STANDARD TABLE OF bapiacistru
                         WITH HEADER LINE.
DATA : it_coobject       LIKE STANDARD TABLE OF bapiskfobj
                         WITH HEADER LINE.
DATA : it_pervalue       LIKE STANDARD TABLE OF bapiskfval
                         WITH HEADER LINE.
DATA : it_totvalue       LIKE STANDARD TABLE OF bapiskftot
                         WITH HEADER LINE.


** For ALV
DATA : gv_repid LIKE sy-repid.
DATA : gv_status       TYPE slis_formname VALUE 'PF_STATUS'.
DATA : gv_user_command TYPE slis_formname VALUE 'USER_COMMAND'.
DATA : it_sort         TYPE slis_t_sortinfo_alv WITH HEADER LINE .
DATA : gv_col_pos TYPE i.
DATA : it_fieldcat          TYPE slis_t_fieldcat_alv,
       wa_fieldcat          LIKE LINE OF it_fieldcat,
       it_eventcat          TYPE slis_t_event,
       wa_eventcat          LIKE LINE OF it_eventcat.
DATA : it_events	          TYPE slis_t_event,
       it_event_exit	    TYPE slis_t_event_exit.

* Global Variant
DATA : gv_post_date LIKE coheader-budat.

*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-100.
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.
PARAMETERS :  p_kokrs LIKE csks-kokrs MEMORY ID cac OBLIGATORY,
              p_gjahr LIKE cobk-gjahr MEMORY ID gjr OBLIGATORY,
              p_perid LIKE cosp-perbl MEMORY ID bpe OBLIGATORY,
              p_trun(1).
SELECTION-SCREEN END   OF BLOCK bl1.

SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK bl3 WITH FRAME TITLE text-022.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  1(13)  text-020. "
SELECTION-SCREEN POSITION 15.
PARAMETERS : p_act RADIOBUTTON GROUP ra01
             USER-COMMAND  cty.
SELECTION-SCREEN COMMENT  25(13) text-021.
SELECTION-SCREEN POSITION 39.
PARAMETERS : p_pln  RADIOBUTTON GROUP ra01.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK bl3.
SELECTION-SCREEN SKIP 1.
PARAMETERS: p_file  LIKE rlgrap-filename OBLIGATORY
                    default 'c:\temp\mhos.txt'.
SELECTION-SCREEN END   OF BLOCK b1.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM at_sel_screen_on_value_request USING p_file 'O'.

*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Set Global ALV Parameter
  gv_repid = sy-repid.

  PERFORM upload_file.

* Preparation of ALV
  PERFORM pre_report_adj.

*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.
* Call ALV LIST
  PERFORM call_alv_list.

*&---------------------------------------------------------------------*
*&      Form  DIS_BAPI_MESSAGE
*&---------------------------------------------------------------------*
*        Display BAPI Message
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM dis_bapi_message.
  IF NOT it_return[] IS INITIAL.
    LOOP AT   it_return.
      MESSAGE ID     it_return-id
              TYPE   it_return-type
              NUMBER it_return-number
              WITH   it_return-message_v1
                     it_return-message_v2
                     it_return-message_v3
                     it_return-message_v4.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " DIS_BAPI_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  READ_CEGRP_GROUP
*&---------------------------------------------------------------------*
*       Search Help for Cost element Group / CCTr Group
*----------------------------------------------------------------------*
*  -->  p_class      Class Name
*  <--  P_SET_NAME   Result Group Name
*----------------------------------------------------------------------*
FORM read_cegrp_group USING   p_class
                               p_set_name.

  CALL FUNCTION 'K_GROUP_SELECT'
    EXPORTING
*     BUTTONS                  = 'X'
      class                    = p_class
*     CRUSER                   = '*'
      field_name               = space
*     SEARCHFLD                = '    '
*     SEARCHFLD_INPUT          = 'X'
      searchfld_required       = 'X'
*     SET                      = '*'
*     START_COLUMN             = 10
*     START_ROW                = 5
*     TABLE                    = 'CCSS'
*     TYPELIST                 = 'BS'
*     UPDUSER                  = '*'
*     KOKRS                    =
*     KTOPL                    =
    IMPORTING
*     CLASS_NAME               =
      set_name                 = p_set_name
*     SET_TITLE                =
*     TABLE_NAME               =
*     SETID                    =
    EXCEPTIONS
      no_set_picked            = 1
      OTHERS                   = 2.

* No error check for F4  SH
  IF sy-subrc <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    " READ_CEGRP_GROUP

*-----------------------------------------------------------------------
*    FORM PF_STATUS
*-----------------------------------------------------------------------
FORM pf_status USING  extab TYPE slis_t_extab.
  SET PF-STATUS 'LIST' EXCLUDING extab .
ENDFORM.


*---------------------------------------------------------------------*
*       FORM USER_COMMAND                                             *
*---------------------------------------------------------------------*
*       For User_command - AT User Command                            *
*---------------------------------------------------------------------*
*  -->  UCOMM                                                         *
*  -->  SELFIELD                                                      *
*---------------------------------------------------------------------*
FORM user_command USING ucomm    LIKE sy-ucomm
                        selfield TYPE slis_selfield.

  CLEAR :  first_day, last_day.

  CONCATENATE p_gjahr p_perid+1(2) '01' INTO first_day.

  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
       EXPORTING
            day_in            = first_day
       IMPORTING
            last_day_of_month = last_day.

  CASE ucomm.
* Important part !
* POST PLAN data to STD
    WHEN 'POST'.
*      PERFORM post_std  USING ucomm.
      IF p_act = 'X'.
        PERFORM posting_fm_act .
      ELSE.
        PERFORM posting_fm_pln .
      ENDIF.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  POST_STD
*&---------------------------------------------------------------------*
*       Preparation posting
*&---------------------------------------------------------------------*
*&      Form  PRE_REPORT_ADJ
*&---------------------------------------------------------------------*
*       The Preparation for ALV List
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pre_report_adj.

* Building Field Cat.
  PERFORM fieldcat_init .

ENDFORM.                    " PRE_REPORT_ADJ
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_INIT
*&---------------------------------------------------------------------*
*       Building Field Cat
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fieldcat_init.
  DATA : l_cnt(3) TYPE n,
         l_field(10),
         l_text(10).
  CLEAR : gv_col_pos, it_fieldcat, it_fieldcat[].

  PERFORM build_fieldcat USING 'IT_UPLOAD' 'SKF'    'X' '' '' ''
                               '5'         'SKF'        '' '' ''.

  PERFORM build_fieldcat USING 'IT_UPLOAD' 'VERID'  'X' '' '' ''
                               '6'         'Verison'    '' '' ''.

  PERFORM build_fieldcat USING 'IT_UPLOAD' 'KOSTL'  'X' '' '' ''
                               '10'        'CostCenter' '' '' ''.

  IF p_act = 'X'.
    PERFORM build_fieldcat USING 'IT_UPLOAD' 'MEG001' '' 'X' '' ''
                                 '15'        'Value'  '' '' ''.
  ELSE.
    CLEAR l_cnt.
    DO 12 TIMES.
      l_cnt  = l_cnt + 1.
      CONCATENATE 'MEG' l_cnt INTO l_field.
      CONCATENATE 'Qty_' l_cnt+1(2) INTO l_text.

      PERFORM build_fieldcat USING 'IT_UPLOAD' l_field '' 'X' '' ''
                                   '15'        l_text  '' '' ''.
    ENDDO.
  ENDIF.

* Set Event
  DATA : wa_l_event  TYPE slis_alv_event.
  wa_l_event-name = slis_ev_top_of_page.
  wa_l_event-form = 'BASIC_TOP_OF_PAGE'.
  APPEND wa_l_event TO it_events.


ENDFORM.                    " FIELDCAT_INIT
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0675   text
*      -->P_0676   text
*      -->P_0677   text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_0681   text
*      -->P_0682   text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*----------------------------------------------------------------------*
FORM build_fieldcat USING   value(p_0100)
                             value(p_0101)
                             value(p_0102)
                             value(p_0103)
                             value(p_0104)
                             value(p_0105)
                             value(p_0106)
                             value(p_0107)
                             value(p_0108)
                             value(p_0109)
                             value(p_0110).

  ADD 1 TO gv_col_pos.
  wa_fieldcat-tabname     = p_0100.
  wa_fieldcat-fieldname   = p_0101.
  wa_fieldcat-key         = p_0102.
  wa_fieldcat-do_sum      = p_0103.
  wa_fieldcat-cfieldname  = p_0104.
  wa_fieldcat-ctabname    = p_0105.
  wa_fieldcat-outputlen   = p_0106.
  wa_fieldcat-seltext_l   = p_0107.
  wa_fieldcat-datatype    = p_0108.
  wa_fieldcat-qfieldname  = p_0109.
  wa_fieldcat-qtabname    = p_0110.
  wa_fieldcat-col_pos     = gv_col_pos.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

ENDFORM.                    " BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  CALL_ALV_LIST
*&---------------------------------------------------------------------*
*       CALL ALV LIST
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_alv_list.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
     EXPORTING
*     I_INTERFACE_CHECK              = ' '
*     I_BYPASSING_BUFFER             =
*     I_BUFFER_ACTIVE                = ' '
       i_callback_program             = gv_repid
       i_callback_pf_status_set       = gv_status
       i_callback_user_command        = gv_user_command
*     I_STRUCTURE_NAME               =
*     IS_LAYOUT                      =
       it_fieldcat                    = it_fieldcat[]
*     IT_EXCLUDING                   =
*     IT_SPECIAL_GROUPS              =
*       IT_SORT                        = IT_SORT[]        "
*     IT_FILTER                      =
*     IS_SEL_HIDE                    =
*     I_DEFAULT                      = 'X'
       i_save                         = 'A'
*     IS_VARIANT                     =
       it_events                      = it_events     "
       it_event_exit                  = it_event_exit   "
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
       t_outtab                       = it_upload
       EXCEPTIONS
       program_error                  = 1
       OTHERS                         = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " CALL_ALV_LIST

*---------------------------------------------------------------------*
*       FORM BASIC_TOP_OF_PAGE                                        *
*---------------------------------------------------------------------*
*       Display Title                                                 *
*---------------------------------------------------------------------*
FORM basic_top_of_page.

  WRITE : / 'Controlling Area      : ', p_kokrs .
  WRITE : / 'Fiscal Year/Period    : '
            , p_gjahr, '/', p_perid.
**  WRITE : / 'Version               : ', p_versn .
*  WRITE : / 'Posting Date          : ', gv_post_date.
*  WRITE : / 'SKF                   : ', 'CS001-No.of Persons by Team.'
  .
*
** CCTR
*  IF p_ncoal NE space.
*    WRITE : / 'Cost Center Group     : ', p_ncoal.
*  ENDIF.
*
**  IF NOT S_KOSTL[] IS INITIAL.
**    LOOP AT S_KOSTL.
**      AT FIRST.
**        WRITE : / 'Cost Center           :     '.
**      ENDAT.
**      WRITE : / '                        ', S_KOSTL-LOW, '~',
**S_KOSTL-HIGH.
**    ENDLOOP.
**  ENDIF.

  WRITE : / 'Test Run              : ', p_trun.
  SKIP 1.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CALL_POST_FM
*&---------------------------------------------------------------------*
*       CALL_POST_FM
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_post_fm.

  CALL FUNCTION 'BAPI_COSTACTPLN_POSTKEYFIGURE'
    EXPORTING
      headerinfo           = wa_headerinfo
*   DELTA                = ' '
    TABLES
      indexstructure       = it_indexstructure
      coobject             = it_coobject
     pervalue             = it_pervalue
*   TOTVALUE             =
      return               = it_return.

* Check error
  CLEAR  it_return.
  LOOP AT it_return  WHERE type CA 'AE'.
    MESSAGE ID     it_return-id
            TYPE   it_return-type
            NUMBER it_return-number
            WITH   it_return-message_v1
                   it_return-message_v2
                   it_return-message_v3
                   it_return-message_v4.
    CLEAR it_return.
  ENDLOOP.


ENDFORM.                    " CALL_POST_FM
*&---------------------------------------------------------------------*
*&      Form  upload_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM upload_file.

  CALL FUNCTION 'WS_UPLOAD'
       EXPORTING
            codepage                = ' '
            filename                = p_file
            filetype                = 'DAT'
*           HEADLEN                 = ' '
*           LINE_EXIT               = ' '
*           TRUNCLEN                = ' '
*           USER_FORM               = ' '
*           USER_PROG               = ' '
*      IMPORTING
*           FILELENGTH              =
       TABLES
            data_tab                = it_upload
      EXCEPTIONS
           conversion_error        = 1
           file_open_error         = 2
           file_read_error         = 3
           invalid_table_width     = 4
           invalid_type            = 5
           no_batch                = 6
           unknown_error           = 7
           gui_refuse_filetransfer = 8
           customer_error          = 9
           OTHERS                  = 10
            .
  CASE sy-subrc.
    WHEN 0.
      DATA l_text(132).
      CONCATENATE p_file ' is loaded'
                  INTO l_text.
      MESSAGE s000 WITH l_text.
    WHEN OTHERS.
      MESSAGE e000 WITH 'Error during file upload'.
  ENDCASE.

ENDFORM.                    " upload_file
*&---------------------------------------------------------------------*
*&      Form  AT_SEL_SCREEN_ON_VALUE_REQUEST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_FILE  text
*      -->P_0290   text
*----------------------------------------------------------------------*
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

ENDFORM.                    " AT_SEL_SCREEN_ON_VALUE_REQUEST
*&---------------------------------------------------------------------*
*&      Form  posting_fm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_UCOMM  text
*----------------------------------------------------------------------*
FORM posting_fm USING    p_ucomm.



ENDFORM.                    " posting_fm
*&---------------------------------------------------------------------*
*&      Form  posting_fm_Act
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_UCOMM  text
*----------------------------------------------------------------------*
FORM posting_fm_act.
  SORT it_upload.
  LOOP AT it_upload.
    it_post_act-cgroup  = it_upload-kostl.
    it_post_act-count   = it_upload-meg001.
    APPEND it_post_act. CLEAR it_post_act.
    AT END OF skf.
      PERFORM posting_act  TABLES it_post_act
                           USING  it_upload-skf.
      CLEAR : it_post_act, it_post_act[].
    ENDAT.
  ENDLOOP.
ENDFORM.                    " posting_fm_Act
*&---------------------------------------------------------------------*
*&      Form  posting_fm_pln
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM posting_fm_pln.

  SORT it_upload.
  LOOP AT it_upload.
    MOVE-CORRESPONDING it_upload TO it_post_pln.
    APPEND it_post_pln. CLEAR it_post_pln.
    AT END OF verid.
      PERFORM posting_std_pln USING  it_upload-skf
                                     it_upload-verid.
      CLEAR : it_post_pln, it_post_pln[].
    ENDAT.
  ENDLOOP.


ENDFORM.                    " posting_fm_pln
*&---------------------------------------------------------------------*
*&      Form  posting_std_pln
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_POST_PLN  text
*      -->P_IT_UPLOAD_SKF  text
*      -->P_IT_UPLOAD_VERID  text
*----------------------------------------------------------------------*
FORM posting_std_pln USING    p_skf
                              p_verid.
* Init. Message TAB
  CLEAR : it_return, it_return[].

* Fill Header DATA
  CLEAR wa_headerinfo.
  wa_headerinfo-co_area        = p_kokrs.
  wa_headerinfo-fisc_year      = p_gjahr.
  wa_headerinfo-period_from	    = '001' . "P_FRPER.
  wa_headerinfo-period_to	    = '012' . "P_TOPER.
  wa_headerinfo-version        = p_verid.
* WA_HEADERINFO-DOC_HDR_TX	 =    .

* Fill Object List and Plan Values per Period
  CLEAR : it_indexstructure, it_indexstructure[].
  CLEAR : it_coobject,       it_coobject[].
  CLEAR : it_pervalue,       it_pervalue[].
  CLEAR : it_totvalue,       it_totvalue[].

* Sort to post data.
  SORT it_post_pln BY kostl.

  LOOP AT it_post_pln.
* Obj
    ON CHANGE OF  it_post_pln-kostl.
* Index of Object Key
      it_indexstructure-object_index
           = it_indexstructure-object_index + 1 .

      CLEAR it_coobject.
      it_coobject-object_index = it_indexstructure-object_index.

      it_coobject-costcenter   = it_post_pln-kostl.

      APPEND it_coobject.
      CLEAR  it_coobject.
    ENDON.

* Value.
* Index of Value
    it_indexstructure-value_index
         = it_indexstructure-value_index + 1.

    CLEAR it_pervalue.
    it_pervalue-value_index = it_indexstructure-value_index.
    it_pervalue-statkeyfig     = p_skf.

    PERFORM set_value_amt.
    APPEND it_pervalue.
    CLEAR  it_pervalue.

* append Index
    APPEND it_indexstructure.
    CLEAR it_post_pln.
  ENDLOOP.

* Call BAPI FM
  PERFORM call_post_fm.

* Commit
  IF p_trun = 'X'.
  ELSE.
    COMMIT WORK.
    MESSAGE s009(zmco) ."WITH P_UCOMM.

*       Consolidation management for the SKF
    CALL FUNCTION 'Z_FCO_MANAGEMENT_SKF'
      EXPORTING
        im_pgmno         =   sy-tcode
        im_kokrs         =   p_kokrs
        im_gjahr         =   p_gjahr
        im_perbl         =   p_perid
        im_perbl_t       =   p_perid
        im_versn         =   p_verid.

  ENDIF.

ENDFORM.                    " posting_std_pln
*&---------------------------------------------------------------------*
*&      Form  SET_VALUE_AMT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_value_amt.
* Fixed Cost
* Variable Cost
  trans_value 01. trans_value 02. trans_value 03. trans_value 04.
  trans_value 05. trans_value 06. trans_value 07. trans_value 08.
  trans_value 09. trans_value 10. trans_value 11. trans_value 12.
  trans_value 13. trans_value 14. trans_value 15. trans_value 16.

ENDFORM.                    " SET_VALUE_AMT
*&---------------------------------------------------------------------*
*&      Form  posting_act
*&---------------------------------------------------------------------*
form posting_act tables   p_post structure it_post_act
                 using    f_skf.

  data : lv_conf_text(50).

* Init. Message TAB
  clear : it_return, it_return[].

** <  Posting , hard coding > **
* doc date / posting date   =  LAST DAY OF THE MONTH
* TEXT
  clear lv_conf_text.
  concatenate sy-uname  sy-datum  sy-repid
         into lv_conf_text separated by '/'.

* Fill Header DATA
  clear wa_doc_header.
  wa_doc_header-co_area       = p_kokrs.
  wa_doc_header-docdate       = last_day.
  wa_doc_header-postgdate     = last_day.
  wa_doc_header-version       = '000'.
  wa_doc_header-variant       = 'SAP01'.
  wa_doc_header-doc_hdr_tx    = lv_conf_text.
  wa_doc_header-username      = sy-uname.

* Fill Object List
  clear : it_doc_items, it_doc_items[].
  loop at p_post.
    it_doc_items-statkeyfig = f_skf.
    it_doc_items-stat_qty   = p_post-count.
* -> Alpha Numeric Conversion
    it_doc_items-rec_cctr   = p_post-cgroup.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
         exporting
              input  = it_doc_items-rec_cctr
         importing
              output = it_doc_items-rec_cctr.
**// End of Mod.
    append it_doc_items.
  endloop.

* Call BAPI FM
  perform call_post_fm_act.

* Commit
  if p_trun = 'X'.
    read table it_return  index 1.
    message s000(zmco) with it_return-message.
  else.
    commit work.
    read table it_return  index 1.
    message s000(zmco) with it_return-message.
*       Consolidation management for the SKF
    call function 'Z_FCO_MANAGEMENT_SKF'
      exporting
        im_pgmno         =   sy-tcode
        im_kokrs         =   p_kokrs
        im_gjahr         =   p_gjahr
        im_perbl         =   p_perid
*           IM_PERBL_T       =
        im_versn         =   '000'.
*           IM_KOSTL_F        =    S_KOSTL-LOW
*           IM_KOSTL_T        =    S_KOSTL-HIGH
*       im_gname          =    p_ncoal.
*           IM_PRZNR_F       =
*           IM_PRZNR_T       =
*         IMPORTING
*           SUBRC            =

  endif.

endform.                    " posting_act
*&---------------------------------------------------------------------*
*&      Form  call_post_fm_act
*&---------------------------------------------------------------------*
form call_post_fm_act.

  call function 'BAPI_ACC_STAT_KEY_FIG_POST'
       exporting
            doc_header = wa_doc_header
       tables
            doc_items  = it_doc_items
            return     = it_return.


* Check error
  clear  it_return.
  loop at it_return  where type ca 'AE'.
    message id     it_return-id
            type   it_return-type
            number it_return-number
            with   it_return-message_v1
                   it_return-message_v2
                   it_return-message_v3
                   it_return-message_v4.
    clear it_return.
  endloop.

endform.                    " call_post_fm_act
