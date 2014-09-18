************************************************************************
* Program Name      : ZCPP102C_CL01
* Author            : Bobby
* Creation Date     : 2003.08.13.
* Specifications By : Bobby
* Pattern           : 1.1
* Development Request No : UD1K901939
* Addl Documentation:
* Description       : [BDC] CLASSES UPLOAD
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
************************************************************************
REPORT ZCPP102C_CL01 MESSAGE-ID ZMPP
                     NO STANDARD PAGE HEADING LINE-SIZE 255.

PARAMETERS:
      P_HD_LIN                TYPE  i   DEFAULT 2   NO-DISPLAY,
                                             " HEAD LINE FOR EXCEL FILE.
      P_TCODE                 LIKE  tstc-tcode                ,
      P_CMODE                 TYPE  c                         ,
      P_PMODE                 TYPE  c   DEFAULT 'N'           .

DATA: WA_FILENAME             LIKE  rlgrap-filename,
      WA_FILETYPE             LIKE  rlgrap-filetype VALUE 'DAT',
      WA_BDCGROUP             LIKE  sy-uname.          " APQI-GROUPID

DATA: BEGIN OF IT_REC OCCURS 0,
        klart(013),                       " CLASS TYPE
        class(018),                       " CLASS
        klbez(040),                       " CLASS DESCRIBTION
        klagr(010),                       " CLASS GROUP
        kschl1(40),                       " KEYWORD 1
        kschl2(40),                       " KEYWORD 2
        merkma(30),                       " CHARACTERISTICS
        vondt(018),                       " VALID FROM
      END OF IT_REC.

DATA: IT_MSG                  LIKE TABLE OF BDCMSGCOLL WITH HEADER LINE,
      IT_BDCDATA              LIKE TABLE OF BDCDATA    WITH HEADER LINE.

DATA: WA_tmp_count(3)         TYPE n,
      WA_tmp_flag             TYPE c,
      WA_tmp_TEXT(225)        TYPE c,
      WA_class                LIKE rmclm-class,
      WA_DATE                 TYPE D,
      WA_TMP_DATE(10)         TYPE C,
      WA_fl_exist             TYPE c,
      WA_TMP_TOTAL(06)        TYPE N,
      WA_TMP_PROCESS(06)      TYPE N.


INITIALIZATION.
  WA_bdcgroup = sy-uname.
  P_TCODE  = 'CL01' .
  IF sy-uname = 'BOBBY'.
    P_PMODE = 'A'.
  ENDIF.

START-OF-SELECTION.

  PERFORM bdc_upload_data.
  IF P_Cmode = 'X'.
    PERFORM bdc_open_group.
  ENDIF.

  DELETE IT_REC INDEX 1 .
  DELETE IT_REC INDEX 1 .

  LOOP AT IT_REC .
    WA_TMP_TOTAL = WA_TMP_TOTAL + 1  .
    IF IT_REC-class = WA_class .
      PERFORM cl01_transaction .       " PROCESSING THE REMAIN PART
      CONTINUE.
    ELSE.
      CLEAR: WA_fl_exist.
      WA_TMP_PROCESS = WA_TMP_PROCESS + 1 .
      WA_tmp_count = 1 .
      IF WA_class = space .
      ELSE.
        PERFORM remain_routine .
      ENDIF.
      REFRESH IT_Bdcdata.
      WA_class = IT_REC-class .
    ENDIF.

    WA_DATE = IT_REC-VONDT(8) .
    WRITE WA_DATE TO WA_TMP_DATE .

    PERFORM bdc_dynpro_processing USING:
                       'X' 'SAPLCLMO'             '0100',
                       ' ' 'BDC_OKCODE'           '=SCHL',
                       ' ' 'RMCLM-CLASS'          IT_REC-class,
                       ' ' 'RMCLM-KLART'          IT_REC-klart(3),

                       'X' 'SAPLCLMO'             '0100',
                       ' ' 'BDC_OKCODE'           '=SCHL',

                       'X' 'SAPLCLMO'             '7777',
                       ' ' 'BDC_OKCODE'           '=BASD',
                       ' ' 'RMCLM-KSCHL(01)'      IT_REC-kschl1,
                       ' ' 'RMCLM-KSCHL(02)'      IT_REC-kschl2,

                       'X' 'SAPLCLMO'             '7777',
                       ' ' 'BDC_OKCODE'           '=MERK',
                       ' ' 'RMCLM-KLBEZ'          IT_REC-klbez,
                       ' ' 'RMCLM-STATU'          '1'         ,
                       ' ' 'RMCLM-KLAGR'          IT_REC-klagr,
                       ' ' 'RMCLM-VONDT'          WA_TMP_DATE    ,

                       'X' 'SAPLCLMO'             '7777',
                       ' ' 'BDC_OKCODE'           '/00' ,
                       ' ' 'RMCLM-MERKMA(01)'     IT_REC-merkma,

                       'X' 'SAPLCLMO'             '7777',
                       ' ' 'BDC_OKCODE'           '/00' ,
                       ' ' 'RMCLM-EINTRAG'        WA_tmp_count.
  ENDLOOP.

  PERFORM remain_routine .

  IF P_cmode = 'X'.
    PERFORM bdc_close_group.
  ENDIF.

END-OF-SELECTION.

  INCLUDE zCpp103_common_routine .

*&---------------------------------------------------------------------*
*&      Form  CS02_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cl01_transaction.
  WA_tmp_count = WA_tmp_count + 1 .
  PERFORM bdc_dynpro_processing USING:
                       'X' 'SAPLCLMO'             '7777',
                       ' ' 'BDC_OKCODE'           '/00' ,
                       ' ' 'RMCLM-MERKMA(02)'     IT_REC-merkma,

                       'X' 'SAPLCLMO'             '7777',
                       ' ' 'BDC_OKCODE'           '/00' ,
                       ' ' 'RMCLM-EINTRAG'        WA_tmp_count.
ENDFORM.                    " CL01_TRANSACTION

*&---------------------------------------------------------------------*
*&      Form  REMAIN_ROUTINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM remain_routine.
  PERFORM bdc_dynpro_processing USING:
                 'X' 'SAPLCLMO'             '7777',
                 ' ' 'BDC_OKCODE'           '=SAVE'.

  IF P_cmode = space .
    CALL TRANSACTION 'CL01' USING IT_bdcdata MODE p_Pmode
                            MESSAGES INTO    IT_MSG    .

    LOOP AT IT_MSG .
      CLEAR: WA_TMP_TEXT .
      PERFORM CREATE_MESSAGE    USING  WA_TMP_text   IT_MSG-MSGID
                                       IT_MSG-MSGNR  IT_MSG-MSGV1
                                       IT_MSG-MSGV2  IT_MSG-MSGV3
                                       IT_MSG-MSGV4  .
      PERFORM DISPLAY_MESSAGE   USING  WA_TMP_text   IT_MSG-MSGTYP
                                       WA_TMP_TOTAL  WA_TMP_PROCESS  .
    ENDLOOP.

  ELSE.
    P_tcode = 'CL01' .
    PERFORM bdc_insert_transaction .
  ENDIF.
ENDFORM.                    " REMAIN_ROUTINE
