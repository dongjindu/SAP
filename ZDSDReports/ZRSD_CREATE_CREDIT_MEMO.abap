REPORT ZRSD_CREATE_CREDIT_MEMO .

* UD1K940629

DATA : BEGIN OF it_upload OCCURS 0,
        bill_type(4),
        Item(6),
        amt(10),
        pdate(10),
        inv(10),
 END OF  it_upload.

DATA : BEGIN OF it_data OCCURS 0,
        inv(10),
        Item(6),
        amt(10),
        pdate(10),
        bill_type(4),
END OF  it_data.


data : begin of bdc_tab occurs 0.
        include structure bdcdata.
data : end of bdc_tab.

data : begin of mess_tab occurs 0.
        include structure bdcmsgcoll.
data : end of mess_tab.


PARAMETERS: p_file  LIKE rlgrap-filename OBLIGATORY.
*                    default 'c:\temp\mhos.txt'.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM at_sel_screen_on_value_request USING p_file 'O'.


start-of-selection.

  perform upload_file.

  CALL FUNCTION 'BDC_OPEN_GROUP'
       EXPORTING
            CLIENT              = SY-MANDT
            GROUP               = 'VF01_CR'
            KEEP                = ''
            USER                = SY-UNAME
       EXCEPTIONS
            RUNNING             = 1
            QUEUE_ERROR         = 2
            CLIENT_INVALID      = 3
            GROUP_INVALID       = 4
            USER_INVALID        = 5
            HOLDDATE_INVALID    = 6
            DESTINATION_INVALID = 7.

* CALL BDC
  perform create_credit_memos.

  CALL FUNCTION 'BDC_CLOSE_GROUP'.

  write :/ 'Check BDC session VF01_CR for any errors'.

end-of-selection.
*&---------------------------------------------------------------------*
*&      Form  at_sel_screen_on_value_request
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_FILE  text
*      -->P_0018   text
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




ENDFORM.                    " at_sel_screen_on_value_request
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
*  CASE sy-subrc.
*    WHEN 0.
*      DATA l_text(132).
*      CONCATENATE p_file ' is loaded'
*                  INTO l_text.
*      MESSAGE s000 WITH l_text.
*    WHEN OTHERS.
*      MESSAGE e000 WITH 'Error during file upload'.
*  ENDCASE.
  loop at it_upload.
    move-corresponding it_upload to it_data.
    append it_data.
  endloop.
ENDFORM.                    " upload_file
*&---------------------------------------------------------------------*
*&      Form  create_credit_memos
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_credit_memos.
  data : www(1),
         cnt(2) type n,
         l_date(10),
         l_date1(10),
         l_bill(4) type c,
         f_flag type c,
         l_cnt  type i,
         l_cnt1(6)   type c,
         l_vbeln(10) type c,
         l_posnr(6) type c,
         P_posnr(20) type c,
         l_sel(20) type c,
         flag type c,
          W_MESSAGE(200) type c.

  data : begin of it_index occurs 0,
          ind(6) type c,
          amt(15) type c,
         end of it_index.

  www = 'N'.
  write sy-datum to l_date1.
  sort it_data by inv Item.
  loop at it_data.
    write it_data-pdate  to l_date.

    l_cnt = l_cnt + 1.
    l_vbeln = it_data-INV.
    l_posnr = it_data-item.
    l_bill  = it_data-bill_type.
    it_index-ind = l_cnt.
    shift it_index-ind  left deleting leading space.
    it_index-amt = it_data-amt.
    append it_index.

    at end of inv.
      flag = 'X'.
    endat.
* Initial screen
    at new INV.
      perform bdc_fill using :
              'X' 'SAPMV60A'             '0102',
              ' ' 'BDC_CURSOR'           'KOMFK-VBELN(01)',
              ''  'BDC_OKCODE'       '=PICK',
*              ' ' 'RV60A-FKART'          'ZVL2',
               ' ' 'RV60A-FKART'      l_bill,
              ' ' 'RV60A-FKDAT'           l_date,
              ' ' 'RV60A-PRSDT'           l_date1,
              ''  'RV60A-SELKZ(01)'       'X',
              ' ' 'KOMFK-VBELN(01)'     l_vbeln.


      perform bdc_fill using :
            'X'  'SAPLV60P'  '4413',
            ''   'BDC_CURSOR' '%#AUTOTEXT001'    ,
            ''   'BDC_OKCODE' '=POPO'.

* Select Line item
      perform bdc_fill using :
              'X'    'SAPLV60P'  '0251',
              ''     'BDC_CURSOR'  'RV45A-POSNR',
              ''     'BDC_OKCODE'    '=POSI' ,
               ''    'RV45A-POSNR'  l_posnr.

* Mark
      perform  bdc_fill using :
              'X'   'SAPLV60P' '4413',
              ''    'BDC_CURSOR'  '%#AUTOTEXT001',
              ''    'BDC_OKCODE'  '=MARK'.

* Choose anonther Line Item
      perform  bdc_fill using :
              'X'   'SAPLV60P' '4413',
              ''    'BDC_CURSOR'  '%#AUTOTEXT001',
              ''    'BDC_OKCODE'  '=POPO'.

      continue.
    endat.

* select line item
    perform bdc_fill using :
            'X'    'SAPLV60P'  '0251',
            ''     'BDC_CURSOR'  'RV45A-POSNR',
            ''     'BDC_OKCODE'    '=POSI' ,
             ''    'RV45A-POSNR'  it_data-ITEM.

* Mark
    perform  bdc_fill using :
            'X'   'SAPLV60P' '4413',
            ''    'BDC_CURSOR'  '%#AUTOTEXT001',
            ''    'BDC_OKCODE'  '=MARK'.

* Choose other line
    if flag is initial.
      perform  bdc_fill using :
              'X'   'SAPLV60P' '4413',
*              ''    'BDC_CURSOR'  'VDICS-POSNR(01)',
              ''    'BDC_CURSOR'  '%#AUTOTEXT001',
              ''    'BDC_OKCODE'  '=POPO'.
    endif.

    at end of inv.

      perform  bdc_fill using :
          'X'   'SAPLV60P' '4413',
          ''    'BDC_OKCODE'  '/00'.

      perform  bdc_fill using :
              'X'  'SAPLV60P'   '4413',
              ''   'BDC_CURSOR'  'VDICS-POSNR(01)',
              ''    'BDC_OKCODE' '=RUEB',
              ''    'RV60A-SELKZ(01)' 'X'.

* Added
*         perform  bdc_fill using :
*              'X'   'SAPLV60P' '4413',
*              ''    'BDC_OKCODE'  '/00'.

      perform  bdc_fill using :
                  'X'  'SAPMV60A' '0102',
                  ''   'BDC_CURSOR' 'KOMFK-VBELN(01)',
                  ''   'BDC_OKCODE' '=FAKT',
*                  ''   'RV60A-FKART' 'ZVL2',
                 ' ' 'RV60A-FKART'      l_bill,   "UD1K940681
                  ''    'RV60A-SELKZ(01)'  'X'.

      perform  bdc_fill using :
                    'X'  'SAPMV60A' '0103',
                     ''  'BDC_CURSOR' '*TVFKT-VTEXT(02)',
                     ''  'BDC_OKCODE' '=UEBP'.

* Change Price for each of the line item selected
*Repeat for All line items
       f_flag = 'X'.
      loop at it_index.
        l_cnt1 = l_cnt1 + 1.

        it_index-IND  = l_cnt1.
        shift it_index-ind  left deleting leading space.
        CONCATENATE 'VBRP-POSNR(' it_index-IND ')' INTO P_posnr.
        CONCATENATE 'RV60A-SELKZ(' it_index-IND ')' INTO l_sel.

         if  f_flag = 'X'.
              perform  bdc_fill using :  'X'  'SAPMV60A' '0104',
                         ''   'BDC_CURSOR' 'VBRK-FKART',
                        ''   'BDC_OKCODE'  '=MKAL'.
           clear f_flag.
           endif.

        if l_cnt1 <= 20 .

                  perform  bdc_fill using :  'X'  'SAPMV60A' '0104',
                        ''   'BDC_CURSOR'  'VBRK-FKART',
                        ''   'BDC_OKCODE'  '=PFKO',
                         'X' 'SAPMV60A' '6002',
                         ''  'BDC_OKCODE' '/00',
                         ''  'BDC_CURSOR' 'KOMV-KBETR(01)',
                         '' 'KOMV-KSCHL(07)'  'ZZ01',
                         '' 'KOMV-KBETR(01)'  '0',
                         ''  'KOMV-KBETR(07)'  it_index-amt,
                         'X'  'SAPMV60A' '6002',
                          ''  'BDC_OKCODE'  '=UEBP',
                          '' 'BDC_CURSOR' 'KOMV-KSCHL(06)'.

         else.

                    perform  bdc_fill using :  'X'  'SAPMV60A' '0104',
                        ''   'BDC_CURSOR'  'VBRK-FKART',
                        ''   'BDC_OKCODE'  '=P+',
                        ''   'BDC_CURSOR'  'VBRK-FKART',
                        ''   'BDC_OKCODE'  '=PFKO',
                         'X' 'SAPMV60A' '6002',
                         ''  'BDC_OKCODE' '/00',
                         ''  'BDC_CURSOR' 'KOMV-KBETR(01)',
                         '' 'KOMV-KSCHL(07)'  'ZZ01',
                         '' 'KOMV-KBETR(01)'  '0',
                         ''  'KOMV-KBETR(07)'  it_index-amt,
                         'X'  'SAPMV60A' '6002',
                          ''  'BDC_OKCODE'  '=UEBP',
                          '' 'BDC_CURSOR' 'KOMV-KSCHL(06)'.

              l_cnt1 = 0.
*          perform  bdc_fill using :  'X'  'SAPMV60A' '0104',
**                    ''   'BDC_CURSOR' 'VBRP-POSNR(01)',
*                         ''   'BDC_CURSOR' p_posnr,
*                        ''   'BDC_OKCODE'  '=PFKO',
**                    ''   'RV60A-SELKZ(01)' 'X',
*                         ''   l_sel          'X',
*                         'X' 'SAPMV60A' '6002',
*                         ''  'BDC_OKCODE' '/00',
*                         ''  'BDC_CURSOR' 'KOMV-KBETR(01)',
*                         '' 'KOMV-KSCHL(05)'  'ZZ01',
*                         '' 'KOMV-KBETR(01)'  '0',
*                         ''  'KOMV-KBETR(05)'  it_index-amt,
*                         'X'  'SAPMV60A' '6002',
*                          ''  'BDC_OKCODE'  '=UEBP',
*                          '' 'BDC_CURSOR' 'KOMV-KSCHL(06)'.
** Page Down after 15 line items
*        else.
*          l_cnt1 = 1.
*          shift l_cnt1  left deleting leading space.
*          CONCATENATE 'VBRP-POSNR(' l_cnt1 ')' INTO P_posnr.
*          CONCATENATE 'RV60A-SELKZ(' l_cnt1 ')' INTO l_sel.
*          perform  bdc_fill using :  'X'  'SAPMV60A' '0104',
*                         ''  'BDC_CURSOR'  'VBRK-FKART',
*                         ''  'BDC_OKCODE'  '=P+',
*                         ''   'BDC_CURSOR' p_posnr,
*                         ''   'BDC_OKCODE'  '=PFKO',
*                         ''   l_sel          'X',
*                         'X' 'SAPMV60A' '6002',
*                         ''  'BDC_OKCODE' '/00',
*                         ''  'BDC_CURSOR' 'KOMV-KBETR(01)',
*                         '' 'KOMV-KSCHL(05)'  'ZZ01',
*                         '' 'KOMV-KBETR(01)'  '0',
*                         ''  'KOMV-KBETR(05)'  it_index-amt,
*                         'X'  'SAPMV60A' '6002',
*                          ''  'BDC_OKCODE'  '=UEBP',
*                          '' 'BDC_CURSOR' 'KOMV-KSCHL(06)'.
*
        endif.
      endloop.

      perform  bdc_fill using :
           'X'  'SAPMV60A' '0104',
           ''   'BDC_CURSOR' 'VBRK-FKART',
           ''    'BDC_OKCODE' '=SICH'.

* Create Separate credit memo's for each of Invoice
      call transaction 'VF01' using bdc_tab mode www
                                    update 'S'
                                    messages into mess_tab.

      IF SY-SUBRC ne  0.

        CALL FUNCTION 'BDC_INSERT'
             EXPORTING
                  TCODE     = 'VF01'
             TABLES
                  DYNPROTAB = BDC_TAB.

        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
             EXPORTING
                  MSGID               = MESS_TAB-MSGID
                  MSGNR               = MESS_TAB-MSGNR
                  MSGV1               = MESS_TAB-MSGV1
                  MSGV2               = MESS_TAB-MSGV2
                  MSGV3               = MESS_TAB-MSGV3
                  MSGV4               = MESS_TAB-MSGV4
             IMPORTING
                  MESSAGE_TEXT_OUTPUT = W_MESSAGE.
        write:/ w_message.
      else.
        write :/ 'Documents created / changed ' , sy-MSGV1.
      endif.
      refresh bdc_tab[]. clear: bdc_tab, it_index[], l_cnt,l_cnt1,flag.

    endat.
  endloop.
ENDFORM.                    " create_credit_memos
*&---------------------------------------------------------------------*
*&      Form  bdc_fill
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0253   text
*      -->P_0254   text
*      -->P_0255   text
*----------------------------------------------------------------------*
FORM bdc_fill USING    p1 p2 p3.

  clear bdc_tab.
  if p1 = 'X'.
    bdc_tab-dynbegin = p1.
    bdc_tab-program  = p2.
    bdc_tab-dynpro   = p3.
  else.
    bdc_tab-dynbegin = p1.
    bdc_tab-fnam     = p2.
    bdc_tab-fval     = p3.
  endif.
  append bdc_tab.


ENDFORM.                    " bdc_fill
