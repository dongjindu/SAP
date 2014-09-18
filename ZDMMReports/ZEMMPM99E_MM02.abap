REPORT zemmpm99e_mm02 NO STANDARD PAGE HEADING
                      LINE-SIZE 200
                      LINE-COUNT 65
                      MESSAGE-ID zmmm.

**---
INCLUDE : zrmmpmxxr_incl.


**---
DATA : BEGIN OF it_itab OCCURS 0,
         matnr LIKE mara-matnr,
         werks LIKE t001w-werks,
         lgort LIKE t001l-lgort,
         lgnum LIKE rmmg1-lgnum,
         lgtyp LIKE rmmg1-lgtyp,
         mtart LIKE mara-mtart,
         mbrsh LIKE mara-mbrsh,
         lgpla LIKE mlgt-lgpla,
         message(80),
         msgtyp LIKE bdcmsgcoll-msgtyp,
       END OF it_itab.

*----- BDC
DATA : BEGIN OF it_bdc OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA : END OF it_bdc.

DATA : BEGIN OF it_mess OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA : END OF it_mess.

DATA : it_message LIKE it_mess OCCURS 0 WITH HEADER LINE.


**---
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
PARAMETERS : p_upfile LIKE rlgrap-filename OBLIGATORY.
SELECTION-SCREEN ULINE.
PARAMETERS : p_mode LIKE ctu_params-dismode DEFAULT 'N'.
SELECTION-SCREEN END OF BLOCK block1.


**---
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_upfile.
  PERFORM f4_p_upfile.


**---
START-OF-SELECTION.
  PERFORM read_data.
  PERFORM call_transaction.

**---
END-OF-SELECTION.
  PERFORM display_result.





**---





*&---------------------------------------------------------------------*
*&      Form  f4_p_upfile
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4_p_upfile.
*---
  CALL FUNCTION 'WS_FILENAME_GET'
       EXPORTING
            def_path         = p_upfile  "* File Name
            mask             = ',*.*,*.*.'
            mode             = 'O'
       IMPORTING
            filename         = p_upfile
       EXCEPTIONS
            inv_winsys       = 1
            no_batch         = 2
            selection_cancel = 3
            selection_error  = 4
            OTHERS           = 5.
ENDFORM.                    " f4_p_upfile

*&---------------------------------------------------------------------*
*&      Form  read_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data.
*---
  DATA : l_filesize(10),
         l_cancel(1).

  CLEAR : it_itab, it_itab[].

  CALL FUNCTION 'UPLOAD'
       EXPORTING
            filename                = p_upfile
            filetype                = 'DAT'
       IMPORTING
            filesize                = l_filesize
            cancel                  = l_cancel
       TABLES
            data_tab                = it_itab
       EXCEPTIONS
            conversion_error        = 1
            invalid_table_width     = 2
            invalid_type            = 3
            no_batch                = 4
            unknown_error           = 5
            gui_refuse_filetransfer = 6
            OTHERS                  = 7.

  DELETE it_itab WHERE matnr EQ space.
ENDFORM.                    " read_data

*&---------------------------------------------------------------------*
*&      Form  call_transaction
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_transaction.
*---
  DATA : l_text(100),
         l_tabix LIKE sy-tabix,
         l_progress(80),
         l_lines TYPE i,
         l_temp_01(10),
         l_temp_02(10).

  DESCRIBE TABLE it_itab LINES l_lines.
  MOVE : l_lines TO l_temp_02.

  LOOP AT it_itab.
    MOVE : sy-tabix TO l_tabix.
    MOVE : l_tabix TO l_temp_01.

    CLEAR : l_progress.

    CONCATENATE l_temp_01 '/' l_temp_02 INTO l_progress
                                             SEPARATED BY space.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
*       PERCENTAGE       = 0
        text             = l_progress.

    CLEAR : it_bdc, it_bdc[], it_mess, it_mess[].

    PERFORM dynpro USING : 'X'  'SAPLMGMM'      '0060',
                           ' '  'RMMG1-MATNR'   it_itab-matnr,
                           ' '  'BDC_SUBSCR'    'SAPLMGV_MATERIAL_SPEC',
                           ' '  'BDC_OKCODE'    '/00'.

    PERFORM dynpro USING : 'X'  'SAPLMGMM'                 '0070',
                           ' '  'MSICHTAUSW-KZSEL(01)'     'X',
                           ' '  'BDC_OKCODE'               '=ENTR'.

    PERFORM dynpro USING : 'X'  'SAPLMGMM'              '4004',
                           ' '  'BDC_SUBSCR'            'SAPLMGMM',
                           ' '  'BDC_SUBSCR'            'SAPLMGD1',
                           ' '  'BDC_OKCODE'            '=SP22'.

    PERFORM dynpro USING : 'X'  'SAPLMGMM'              '0081',
                           ' '  'RMMG1-WERKS'           it_itab-werks,
                           ' '  'RMMG1-LGNUM'           it_itab-lgnum,
                           ' '  'RMMG1-LGTYP'           it_itab-lgtyp,
                           ' '  'BDC_OKCODE'            '=ENTR'.

    PERFORM dynpro USING : 'X'  'SAPLMGMM'              '4000',
                           ' '  'BDC_SUBSCR'            'SAPLMGMM',
                           ' '  'BDC_SUBSCR'            'SAPLMGD1',
                           ' '  'MLGT-LGPLA'            it_itab-lgpla,
                           ' '  'BDC_SUBSCR'            'SAPLMGD1',
                           ' '  'BDC_OKCODE'            '=BU'.

    CALL TRANSACTION 'MM02' USING it_bdc
                        MODE p_mode
                        UPDATE 'S'
                        MESSAGES INTO it_mess.

    READ TABLE it_mess INDEX 1.

    CLEAR : l_text.

    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
         EXPORTING
              msgid               = it_mess-msgid
              msgnr               = it_mess-msgnr
              msgv1               = it_mess-msgv1
              msgv2               = it_mess-msgv2
              msgv3               = it_mess-msgv3
              msgv4               = it_mess-msgv4
         IMPORTING
              message_text_output = l_text.

    MOVE : l_text TO it_itab-message,
           it_mess-msgtyp TO it_itab-msgtyp.
    MODIFY it_itab INDEX l_tabix.
  ENDLOOP.
ENDFORM.                    " call_transaction

*&---------------------------------------------------------------------*
*&      Form  display_result
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_result.
*---
  ULINE.

  LOOP AT it_itab.
    IF it_itab-msgtyp EQ 'E'.
      FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
    ELSE.
      FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
    ENDIF.
    WRITE : / '|', sy-tabix,
              '|', it_itab-matnr,
              '|', it_itab-werks,
              '|', it_itab-lgnum,
              '|', it_itab-lgtyp,
              '|', it_itab-lgpla,
              '|', it_itab-message,
              '|'.
  ENDLOOP.

  ULINE.
ENDFORM.                    " display_result

*&---------------------------------------------------------------------*
*&      Form  dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0194   text
*      -->P_0195   text
*      -->P_0196   text
*----------------------------------------------------------------------*
FORM dynpro USING    dynbegin name value.
*---
  IF dynbegin  = 'X'.
    CLEAR it_bdc.
    MOVE : name  TO it_bdc-program,
           value TO it_bdc-dynpro,
           'X'   TO it_bdc-dynbegin.
    APPEND it_bdc.
  ELSE.
    CLEAR it_bdc.
    MOVE : name  TO it_bdc-fnam,
           value TO it_bdc-fval.
    APPEND it_bdc.
  ENDIF.
ENDFORM.                    " dynpro
