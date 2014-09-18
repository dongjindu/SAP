************************************************************************
* Program Name      : ZCPP105C_CU01
* Author            : Bobby
* Creation Date     : 2003.12.16.
* Specifications By : Bobby
* Pattern           :
* Development Request No : UD1K901939
* Addl Documentation:
* Description       : [BDC] OBJECT DEPENDENCY
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT zcpp105c_cu01 MESSAGE-ID zmpp
                     NO STANDARD PAGE HEADING LINE-SIZE 255.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS:
      p_hd_lin                TYPE  i   DEFAULT 2   NO-DISPLAY,
                                             " HEAD LINE FOR EXCEL FILE.
      p_tcode                 LIKE  tstc-tcode                ,
      p_cmode                 TYPE  c                         ,
      p_pmode                 TYPE  c   DEFAULT 'N'           .
SELECTION-SCREEN END OF BLOCK b1.

DATA: wa_filename             LIKE  rlgrap-filename,
      wa_filetype             LIKE  rlgrap-filetype VALUE 'DAT',
      wa_bdcgroup             LIKE  sy-uname.          " APQI-GROUPID

DATA: BEGIN OF it_rec         OCCURS 0,
        t_name(18),                              " Table Name
        vtint                 LIKE cuvtab-vtint, " Internal Code
      END OF it_rec.

DATA: BEGIN OF it_cuvtab      OCCURS 0.
        INCLUDE STRUCTURE     cuvtab_ind.
DATA:   t_name(18),                       " Table Name
      END OF it_cuvtab.

DATA: it_msg                  LIKE TABLE OF bdcmsgcoll WITH HEADER LINE,
      it_bdcdata              LIKE TABLE OF bdcdata    WITH HEADER LINE.

DATA: wa_tmp_text(225)        TYPE c.

INITIALIZATION.
  wa_bdcgroup = sy-uname.
  p_tcode  = 'CU01' .
  IF sy-uname = 'BOBBY'.
    p_pmode = 'A'.
  ENDIF.

START-OF-SELECTION.
  PERFORM bdc_upload_data.
  PERFORM get_it_rec     .

  IF p_cmode = 'X'.
    PERFORM bdc_open_group.
  ENDIF.

  PERFORM bdc_processing.

  IF p_cmode = 'X'.
    PERFORM bdc_close_group.
  ENDIF.

END-OF-SELECTION.

  INCLUDE zcpp103_common_routine .


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
                 'X' 'SAPLCUKD'             '0110',
                 ' ' 'BDC_OKCODE'           '=SICH',
                 ' ' 'RCUKD-KNSTA'          '1'   .

  IF p_cmode = space .
    CALL TRANSACTION 'CU01' USING it_bdcdata MODE p_pmode
                            MESSAGES INTO    it_msg    .

    LOOP AT it_msg .
      CLEAR: wa_tmp_text .
      PERFORM create_message    USING  wa_tmp_text   it_msg-msgid
                                       it_msg-msgnr  it_msg-msgv1
                                       it_msg-msgv2  it_msg-msgv3
                                       it_msg-msgv4  .
     " Create the Log Message...
    ENDLOOP.
  ELSE.
    p_tcode = 'CU01' .
    PERFORM bdc_insert_transaction .
  ENDIF.
ENDFORM.                    " REMAIN_ROUTINE

*&---------------------------------------------------------------------*
*&      Form  GET_IT_REC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_it_rec.
  DATA: l_vtint              LIKE cuvtab-vtint.

  DELETE it_rec INDEX 1 .
  DELETE it_rec INDEX 1 .

  LOOP AT it_rec.
    CLEAR: it_rec-vtint.
    SELECT SINGLE vtint INTO it_rec-vtint
      FROM cuvtab
     WHERE vtnam = it_rec-t_name .

    SELECT *  APPENDING TABLE it_cuvtab
      FROM cuvtab_ind
     WHERE vtint = it_rec-vtint.
    MODIFY it_rec.
  ENDLOOP.
ENDFORM.                    " GET_IT_REC

*&---------------------------------------------------------------------*
*&      Form  BDC_PROCESSING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bdc_processing.
  DATA: l_head(40)           TYPE c,
        l_atnam(80)          TYPE c,
        l_foot(80)           TYPE c,
        L_TNAME              LIKE IT_REC-t_name,
        l_name               LIKE cuvtab-vtnam,
        l_table(40)          TYPE c.

  LOOP AT it_rec  WHERE vtint > 0  .
   CLEAR: it_bdcdata, it_bdcdata[].
   CONCATENATE 'D_'  it_rec-t_name       INTO l_table.
   CONCATENATE 'Table' it_rec-t_name '(' INTO l_head SEPARATED BY space.

    PERFORM bdc_dynpro_processing USING:
                         'X' 'SAPLCUKD'             '0100',
                         ' ' 'BDC_OKCODE'           '=BASI',
                         ' ' 'RCUKD-KNNAM'          l_table,

                         'X' 'SAPLCUKD'             '0110',
                         ' ' 'BDC_OKCODE'           '=KNED',
                         ' ' 'RCUKD-KNKTX'          'ENGINE',
                         ' ' 'RCUKD-KNSTA'          '3'     ,
                         ' ' 'RCUKD-KNBED'          ' '     ,
                         ' ' 'RCUKD-KNPRZ'          'X'     ,

                         'X' 'SAPLEDITOR_START'     '2210',
                         ' ' 'BDC_OKCODE'           '=ACIL',
                         ' ' 'RSTXP-TDLINE(01)'     l_head .

    LOOP AT  it_cuvtab  WHERE VTINT = IT_REC-VTINT.
      CALL FUNCTION 'CONVERSION_EXIT_ATINN_OUTPUT'
           EXPORTING
                input  = it_cuvtab-atinn
           IMPORTING
                output = l_atnam.

    CONCATENATE l_atnam '=' l_atnam ',' INTO l_atnam SEPARATED BY space.

      PERFORM bdc_dynpro_processing USING:
                         'X' 'SAPLEDITOR_START'     '2210',
                         ' ' 'BDC_OKCODE'           '=ACIL',
                         ' ' 'RSTXP-TDLINE(02)'     l_atnam.
    ENDLOOP.

    CONCATENATE 'P_'  it_rec-t_name+4(14) INTO l_name  .
    CONCATENATE '= $SELF.' l_name   ')'   INTO l_foot  .
    CONCATENATE l_name      l_foot        INTO l_atnam
                                          SEPARATED BY space.

    PERFORM bdc_dynpro_processing USING:
                   'X' 'SAPLEDITOR_START'     '2210',
                   ' ' 'BDC_OKCODE'           '=VASV',
                   ' ' 'RSTXP-TDLINE(02)'     l_atnam.

    PERFORM remain_routine .
  ENDLOOP.
ENDFORM.                    " BDC_PROCESSING
