*&---------------------------------------------------------------------*
*& Report  ZRSD15R_ORDER_UPDATE                                        *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  ZRSD15R_ORDER_UPDATE  NO STANDARD PAGE HEADING
                                MESSAGE-ID ZMSD.



******************** Declaration ***************************

TABLES: ZTPP_WOSUM, VBKD.

DATA : w_int1 TYPE i,
       w_int2 TYPE i.

DATA:  p_fname LIKE rlgrap-filename,
       answer.

* Batch Data Structure
DATA : BEGIN OF bdc_tab OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA : END OF bdc_tab.

DATA : BEGIN OF it_file OCCURS 0,
       VBELN LIKE VBAK-VBELN,
       NATION LIKE ZTPP_WOSUM-NATION, " material
       DEALER like ZTPP_WOSUM-DEALER,
       DEST(5),
       END OF it_file.
data : begin of it_file2 occurs 0,
       zterm like vbkd-zterm,
       prsdt like vbkd-prsdt,
       end of it_file2.

data: Destination(5).
DATA: it_header LIKE it_file OCCURS 0 WITH HEADER LINE.
DATA : normt(18).
DATA : gl_mode TYPE tb_bdcmode VALUE 'E'.
DATA: itab TYPE TABLE OF bdcmsgcoll.
****************** get file ****************




PERFORM get_file.
PERFORM write_list.
*&--------------------------------------------------------------------
*
*&      Form  get_file
*&--------------------------------------------------------------------
*
*       text
*---------------------------------------------------------------------
*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------
*


FORM get_file.
  CLEAR :p_fname, it_file[], it_file,itab[].

*get file

  CALL FUNCTION 'WS_FILENAME_GET'
       EXPORTING
            mask             = ',*.txt,*.txt.'
            mode             = 'O'
            title            = 'PC FILE'
       IMPORTING
            filename         = p_fname
       EXCEPTIONS
            inv_winsys       = 1
            no_batch         = 2
            selection_cancel = 3
            selection_error  = 4
            OTHERS           = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ELSE.

*************** FILE UPLOAD*************************

    CALL FUNCTION 'WS_UPLOAD'
         EXPORTING
              filename                = p_fname
              filetype                = 'DAT'
         TABLES
              data_tab                = it_file
         EXCEPTIONS
              conversion_error        = 01
              file_open_error         = 02
              file_read_error         = 03
              invalid_type            = 04
              no_batch                = 05
              unknown_error           = 06
              invalid_table_width     = 07
              gui_refuse_filetransfer = 08
              customer_error          = 09
              OTHERS                  = 10.
    DATA : dest(5).




  CONCATENATE it_file-nation it_file-dealer INTO dest.



  ENDIF.
ENDFORM.

*&--------------------------------------------------------------------
*
*&      Form  bdc_processing
*&--------------------------------------------------------------------
*
*       text
*---------------------------------------------------------------------
*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------
*

FORM bdc_processing.


  IF IT_FILE-DEST EQ 'B28AA' AND IT_FILE-DEST EQ 'B28AB'.
     PERFORM bdc_data_formatting.
  ELSE.
     PERFORM bdc_data_formatting2.
  ENDIF.

  PERFORM bdc_tab_transaction.


  COMMIT WORK AND WAIT.

ENDFORM.                               " bdc_processing

*&--------------------------------------------------------------------
*
*&      Form  bdc_tab_mapping
*&--------------------------------------------------------------------
*
*       text
*---------------------------------------------------------------------
*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------
*
FORM bdc_data_formatting.
  CLEAR : bdc_tab[], bdc_tab.
  PERFORM dynpro USING :
     'X'  'SAPLMGMM'              '0102',			
     ' '  'BDC_OKCODE'	           '/00',
     ' '  'VBAK-VBELN'  	     it_file-vbeln,


     'X'  'SAPLMGMM'              '4001',
     ' '  'BDC_OKCODE'             '=SICH',

     ' '  'VBKD-ZTERM'            'PO30'.


ENDFORM.                               " bdc_tab_mapping_top

FORM bdc_data_formatting2.

 CLEAR : bdc_tab[], bdc_tab.
  PERFORM dynpro USING :
     'X'  'SAPLMGMM'              '0102',			
     ' '  'BDC_OKCODE'	           '/00',
     ' '  'VBAK-VBELN'  	     it_file-vbeln,


     'X'  'SAPLMGMM'              '4001',
     ' '  'BDC_OKCODE'             '=SICH',

     ' '  'VBKD-ZTERM'            'PO15'.


ENDFORM.

*&--------------------------------------------------------------------
*
*&      Form  dynpro
*&--------------------------------------------------------------------
*
*       text
*---------------------------------------------------------------------
*
*      -->P_0195   text
*      -->P_0196   text
*      -->P_0197   text
*---------------------------------------------------------------------
*
FORM dynpro USING   dynbegin  pa_name  value.

  IF dynbegin = 'X'.
    CLEAR : bdc_tab.
    MOVE : pa_name  TO bdc_tab-program,
           value    TO bdc_tab-dynpro,
           dynbegin TO bdc_tab-dynbegin.
    APPEND bdc_tab.
  ELSE.
    CLEAR : bdc_tab.
    MOVE : pa_name  TO bdc_tab-fnam,
           value    TO bdc_tab-fval.
    APPEND bdc_tab.
  ENDIF.

ENDFORM.                               " dynpro

" bdc_tab_mapping_body
*&--------------------------------------------------------------------
*
*&      Form  bdc_tab_mapping_update
*&--------------------------------------------------------------------
*
*       text
*---------------------------------------------------------------------
*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------
*
FORM bdc_tab_transaction.
  CALL TRANSACTION 'VA02'
          USING bdc_tab
          MODE gl_mode
          UPDATE 'S'
          MESSAGES INTO itab.
ENDFORM.                               " bdc_tab_transaction
**&--------------------------------------------------------------------
**
**&      Form  less_than_nine
**&--------------------------------------------------------------------
**
**       text
**---------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**---------------------------------------------------------------------
*
**&--------------------------------------------------------------------
**
**&      Form  confirm_step
**&--------------------------------------------------------------------
**
**       text
**---------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**---------------------------------------------------------------------
**
*FORM confirm_step.
*  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
*       EXPORTING
*            defaultoption  = 'Y'
*            textline1      = 'BDC MODE CHOICE'
*            textline2      = ' '
*            titel          = 'BDC MODE'
*            start_column   = 25
*            start_row      = 6
*            cancel_display = 'X'
*       IMPORTING
*            answer         = answer.
*
*  CASE answer.
*    WHEN 'J'.
*      gl_mode  ='A'.
*    WHEN 'N'.
*      gl_mode  ='E'.
*    WHEN 'C'.
*      gl_mode  = 'N'.
*  ENDCASE.
*ENDFORM.                               " confirm_step



*
*&--------------------------------------------------------------------
*
*&      Form  write_list
*&--------------------------------------------------------------------
*
*       text
*---------------------------------------------------------------------
*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------
*
FORM write_list.
  DATA : wa TYPE bdcmsgcoll.
  DATA : BEGIN OF it_temp OCCURS 0,
          dest(5),
         END OF it_temp.
  CLEAR it_temp[].

  LOOP AT itab INTO wa WHERE msgtyp = 'S'.
    MOVE wa-msgv1 TO it_temp-dest.
    APPEND it_temp.
    CLEAR it_temp.
  ENDLOOP.


  LOOP AT it_file.
    READ TABLE it_temp WITH KEY dest = it_file-dest.
    IF sy-subrc = 0.
      MOVE it_temp-dest TO it_file-dest.
      MODIFY it_file INDEX sy-tabix.
      CLEAR : it_file, it_temp.
    ELSE.
      WRITE : / it_file-dest.
    ENDIF.
  ENDLOOP.
  CLEAR w_int2.
  DESCRIBE TABLE it_temp LINES w_int2.
  SKIP 2.
  WRITE : / w_int1, w_int2.

ENDFORM.                               " write_list
