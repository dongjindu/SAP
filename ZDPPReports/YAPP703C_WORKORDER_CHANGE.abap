************************************************************************
* Program Name      : YAPP703C_WORKORDER_CHANGE
* Author            : Bobby
* Creation Date     : 2003.09.16.
* Specifications By : Bobby
* Pattern           : 2.1
* Development Request No : UD1K902220
* Addl Documentation:
* Description       : Interface Work Order from Legacy System
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT  yapp703c_workorder_change  LINE-SIZE  700
                                   MESSAGE-ID zmpp.

DATA: it_msg                 LIKE TABLE OF bdcmsgcoll  WITH HEADER LINE,
      it_bdcdata             LIKE TABLE OF bdcdata     WITH HEADER LINE,
      it_mara                LIKE TABLE OF mara        WITH HEADER LINE,
      wa_mode                TYPE c VALUE 'N'.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS: p_coment          TYPE c  AS CHECKBOX   DEFAULT 'X'.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME.
PARAMETERS: p_char            LIKE cabn-atnam ,
            p_vals            LIKE ausp-atwrt ,
            p_head            TYPE c  AS CHECKBOX   DEFAULT 'X',
            p_col             TYPE c  AS CHECKBOX   DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b2.
SELECTION-SCREEN END OF BLOCK b1.

*************** DO NOT USE!!!! *****************************************
DATA: it_rec                  LIKE TABLE OF mara ,
      p_tcode                 LIKE  tstc-tcode                ,
      p_cmode                 TYPE  c                         ,
      p_pmode                 TYPE  c   VALUE   'N'           ,
      wa_filename             LIKE  rlgrap-filename,
      wa_filetype             LIKE  rlgrap-filetype VALUE 'DAT',
      wa_bdcgroup             LIKE  sy-uname.          " APQI-GROUPID
************************************************************************

INITIALIZATION.
  p_char = 'P_ORDER_PACK'.

LOAD-OF-PROGRAM.

START-OF-SELECTION.
  CHECK p_coment = 'X' .
  PERFORM get_data.
  PERFORM write_start      .
  PERFORM bdc_process_update_matnr.
  PERFORM write_end        .

END-OF-SELECTION.

  INCLUDE zcpp103_common_routine .


*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  IF p_head = 'X'.
    SELECT * INTO      CORRESPONDING FIELDS OF TABLE it_mara
      FROM mara
     WHERE satnr NE space
       AND mtart = 'WOHD' .
  ENDIF.

  IF p_col  = 'X'.
    SELECT * APPENDING CORRESPONDING FIELDS OF TABLE it_mara
      FROM mara
     WHERE satnr NE space
       AND mtart = 'WOCL' .
  ENDIF.

  IF sy-subrc NE 0.
    MESSAGE i001 WITH text-015.
    STOP.
  ENDIF.
ENDFORM.                    " GET_DATA

*&---------------------------------------------------------------------*
*&      Form  BDC_PROCESS_UPDATE_MATNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bdc_process_update_matnr.
  DATA: l_text(225)          TYPE c,
        l_matnr              LIKE mara-matnr,
        l_pack(25)           TYPE c         .

  TRANSLATE p_char TO UPPER CASE.
  LOOP AT it_mara .
    l_matnr = it_mara-matnr.
    IF p_vals = space .
      l_pack  = it_mara-matnr(5) .
    ENDIF.
    PERFORM bdc_dynpro_processing USING :
                           'X'  'SAPLMGMM'             '0060',
                           ' '  'BDC_OKCODE'           '=AUSW' ,
                           ' '  'RMMG1-MATNR'           l_matnr,

                           'X'  'SAPLMGMM'             '0070',
                           ' '  'BDC_OKCODE'           '=ENTR',
                           ' '  'MSICHTAUSW-KZSEL(02)' 'X'    ,

                           'X'  'SAPLMGMM'             '5004',
                           ' '  'BDC_OKCODE'           '=PB21' ,

                           'X'  'SAPLCEI0'             '0109',
                           ' '  'BDC_OKCODE'           '=BACK' ,
                           ' '  'RCTMS-MNAME(01)'       p_char ,
                           ' '  'RCTMS-MWERT(01)'       l_pack        ,

                           'X'  'SAPLMGMM'             '5004',
                           ' '  'BDC_OKCODE'           '=BU' .

    CALL TRANSACTION 'MM02'  USING it_bdcdata           MODE wa_mode
                             MESSAGES INTO              it_msg .

    LOOP AT it_msg WHERE msgtyp = 'E' .
      CLEAR: l_text .
      PERFORM create_message    USING  l_text        it_msg-msgid
                                       it_msg-msgnr  it_msg-msgv1
                                       it_msg-msgv2  it_msg-msgv3
                                       it_msg-msgv4  .
      IF sy-batch = 'X'.
        WRITE: / it_mara-matnr, 'Error:', l_text.
      ELSE.
        MESSAGE i001 WITH l_text     .
      ENDIF.
    ENDLOOP.
    CLEAR: it_msg, it_msg[], it_bdcdata, it_bdcdata[].
  ENDLOOP.
ENDFORM.                    " BDC_PROCESS_UPDATE_MATNR

*&---------------------------------------------------------------------*
*&      Form  write_start
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_start.
  WRITE :/ text-201 .
  WRITE AT: /001(030) text-202                    ,
             031(010) sy-datum                    ,
             042(010) sy-uzeit                    .
  WRITE :/ text-201 .
  SKIP 1.
ENDFORM.                    " write_start

*&---------------------------------------------------------------------*
*&      Form  WRITE_END
*&---------------------------------------------------------------------*
FORM write_end.
  GET TIME.
  SKIP 2.
  WRITE :/ text-203 .
  WRITE AT: /001(030) text-204                    ,
             031(010) sy-datum                    ,
             042(010) sy-uzeit                    .
  WRITE :/ text-203 .
ENDFORM.                    " WRITE_END
