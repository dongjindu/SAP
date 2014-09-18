*&--------------------------------------------------------------------
*& Author                 : HS.JEONG
*& Creation Date          : 09/20/2003
*& Specification By       : Andy Choi
*& Addl documentation     :
*& Description  : AR  Upload
*&
*& Modification Log
*& Date     Developer      Request ID      Description
*&--------------------------------------------------------------------
* Memo
*  1. AR variant planning starting; approval year in AR master - 1
*
REPORT  zcfii11 NO STANDARD PAGE HEADING
                LINE-SIZE 255
                LINE-COUNT 255
                MESSAGE-ID zmfi.

INCLUDE : z_moon_alv_top.
INCLUDE : z_moon_alv_fnc.
DATA g_error.
DATA p_vari.
*----- Internal Table
DATA : BEGIN OF it_data OCCURS 0,
        ivart(2),     "type
*       aposnr(12),   "number
        aposnr  LIKE bapi_appreq_id-appreq,
        txt50(40),    "text
        izwek1(2),    "reason for investment
        aproz1(5),                                          "Rate1
        izwek2(2),     "reason for investment
        aproz2(5),                                          "Rate2
        izwek3(2),     "reason for investment
        aproz3(5),                                          "Rate3
        umwkz(5),      "Environment
        sizecl(2),     "Scal
        priori(1),     "Priority
        gjahr(4),      "Approval Year
        date1(8),      "Start date
        usr09(8),      "End date
        usr00(10),     "type: 1-new, 2-addition, 3-expense, 4-grant
        usr02(10),     "Source
        usr03(8),      "Asset Class
        usr04(13),     "Qty
        parnr_vera(12), "Applicant
*--MATERIAL GROUP
        mgrp1(3),     "material group
        mgrp1_rat(5),                                       "Rate2
        mgrp2(3),     "material group
        mgrp2_rat(5),                                       "Rate2

        akostl(10),    "Req. cost center
        vkostl(10),    "Resp. CC
        vbukrs(4),     "Resp. company code
        werks(4),      "Plant
*        date2 LIKE bapiappreqvarnt-completion_date, "Completion Date
*        aktiv LIKE bapiappreqvarnt-completion_date, "
*        aktiv(8), " LIKE bapiappreqvarnt-completion_date, "
        bwert(4),      "Assessment
*---investment cost
        invkos1(15),    "Overall  Total
        invkos2(15),                                        "2002
        invkos3(15),                                        "2003
        invkos4(15),                                        "2004
        invkos5(15),                                        "2005
        invkos6(15),                                        "2006
        invkos7(15),                                        "2007
        invkos8(15),                                        "2008
        invkos9(15),                                        "2009
        invkos10(15),                                       "2010
*         INVKOS1_8(15),    "2008
*         INVKOS1_9(15),    "2009
*         INVKOS1_10(15),    "2010
*         INVKOS1_11(15),    "2011
*         INVKOS1_12(15),    "2012
*         INVKOS1_13(15),    "2013
*---overhead cost
        gemkos1(15),    "Overall  Total
        gemkos2(15),    "year
        gemkos3(15),    "year
        gemkos4(15),    "year
        gemkos5(15),    "year
        gemkos6(15),    "year
        gemkos7(15),    "year
        gemkos8(15),    "year
        gemkos9(15),    "year
        gemkos10(15),   "year
*---Revenu cost
        ertrag2(15),                                        "2002
        ertrag3(15),                                        "2003
        ertrag4(15),                                        "2004
        ertrag5(15),                                        "2005
        ertrag6(15),                                        "2006
        ertrag7(15),                                        "2007
        ertrag8(15),                                        "2008
        ertrag9(15),                                        "2009
        ertrag10(15),                                       "2010
       END OF it_data.

*- BDC TABLE
DATA  BEGIN OF bdc_tab OCCURS 0.     " BDCDATA TABLE.
        INCLUDE STRUCTURE bdcdata.
DATA  END OF bdc_tab.

DATA  BEGIN OF messtab OCCURS 0.     " BDC MESSAGE TABLE.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA  END OF messtab.

DATA : message(100),
           bdc_mode VALUE 'A'.
DATA : BEGIN OF it_mess OCCURS 0,
        message(100),
       END OF it_mess.
*====FOR BDC
DATA : it_bdc      LIKE bdcdata OCCURS 0 WITH HEADER LINE.
DATA:  it_messtab  LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
DATA : tcode LIKE tstc-tcode.

*----- Global Variable
DATA: g_pagno(4),  g_lines TYPE i,
      g_position   LIKE  sy-linno,      " Total Page Position
      g_length(10),
      g_cancel,
      g_prctr LIKE cobl-prctr,
      g_gsber LIKE anlz-gsber.

*----- Constant
DATA: g_bukrs LIKE t001-bukrs VALUE 'KRA',
      g_waers LIKE bkpf-waers VALUE 'KRW',
      g_type  LIKE rlgrap-filetype VALUE 'DAT',
      wa_test,
      wa_subrc.
*=============BAPI FUNCTION ====================================*
DATA : wa_master        LIKE   bapiappreqmaster,
       wa_user_fields   LIKE   bapiapprequser,
       wa_variant       LIKE   bapiappreqvarnt,
       wa_plan_total    LIKE   bapiappreqplantotal,
       wa_externalnumber LIKE  bapi_appreq_id-appreq ,
       wa_appropriat  LIKE bapi_appreq_id-appreqvrnt.
*===========================================================*
DATA :
 it_org_units      LIKE bapiappreqorgunit OCCURS 0 WITH HEADER LINE,
 it_division       LIKE bapiappreqdivision OCCURS 0 WITH HEADER LINE,
 it_invest_reason  LIKE bapiappreqinvreason OCCURS 0 WITH HEADER LINE,
 it_partner        LIKE  bapiappreqpartner OCCURS 0 WITH HEADER LINE,
 it_variant        LIKE bapiappreqvarntassign OCCURS 0 WITH HEADER LINE,
 it_plan_year      LIKE bapiappreqplanyear OCCURS 0 WITH HEADER LINE,
 it_material_grp   LIKE bapiappreqmatgroup  OCCURS 0 WITH HEADER LINE,
 it_version        LIKE bapiappreqvarntassign OCCURS 0 WITH HEADER LINE,
 i_msg             LIKE bapiret2 OCCURS 0 WITH HEADER LINE,
 g_msg            LIKE bapiret2 OCCURS 0 WITH HEADER LINE.

*---
DATA : plan_tot LIKE bapiappreqplantotal.
*------
DATA : wa_s_cnt TYPE i,
       wa_f_cnt TYPE i,
       wa_t_cnt TYPE i.

DATA : wk_date LIKE bapiappreqvarntassign-appr_year.
DATA : wk_date1 LIKE sy-datum.

DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.

DATA gt_out LIKE it_data OCCURS 0 WITH HEADER LINE.

DATA   num(12) VALUE ' 0123456789.'.

*----- Selection-Screen
PARAMETERS : p_year(4) TYPE n OBLIGATORY.


PARAMETERS : p_upfile LIKE rlgrap-filename OBLIGATORY
                      DEFAULT 'c:\temp\ar.txt'.
PARAMETERS :
  p_bukrs LIKE imak-abukrs OBLIGATORY,
  p_versn LIKE imavz-versi DEFAULT 'IM'.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-001.
PARAMETERS : p_r1 RADIOBUTTON GROUP rad ,
             p_r2 RADIOBUTTON GROUP rad,
             p_r3 RADIOBUTTON GROUP rad,
             p_r4 RADIOBUTTON GROUP rad DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK b11 WITH FRAME TITLE text-002.
PARAMETERS : p_gjahr    LIKE imak-gjahr.
*            p_varnt    like bapi_appreq_id-appreqvrnt.
SELECTION-SCREEN END OF BLOCK b11.

*PARAMETERS : p_create AS CHECKBOX default 'X'.
*PARAMETERS : p_delete AS CHECKBOX.
SELECTION-SCREEN SKIP.
PARAMETERS : p_test AS CHECKBOX.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_upfile.
  PERFORM f4_p_upfile.

AT SELECTION-SCREEN.
  CHECK sy-ucomm = 'ONLI'.
  PERFORM read_data USING g_lines.

START-OF-SELECTION.
  IF p_test = 'X'.
    wa_test = 'X'.
  ELSE.
    wa_test = ' '.
  ENDIF.
  IF g_cancel = 'x'.
    STOP.
  ENDIF.

  IF wa_subrc <> 'X'.
*    WRITE : / 'Upload Success'.
*    SET PF-STATUS '9999'.
  ELSE.
    WRITE : / 'Upload Error'.
    STOP.
  ENDIF.


  IF wa_subrc <> 'X'.
    PERFORM move_to_out.
  ENDIF.

END-OF-SELECTION.
  PERFORM set_output .

*====================================*


*AT USER-COMMAND.
*  CASE sy-ucomm.
*    WHEN 'PROC'.
*      sy-lsind = sy-lsind - 1.
*      IF p_r2 = 'X'.          "delete
*        LOOP AT it_data.
*          PERFORM call_bapi_delete.
*          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
*        ENDLOOP.
*      ELSEIF p_r1 = 'X'.      "create
*        LOOP AT it_data.
*          PERFORM convert_data.
*          PERFORM call_bapi_function.
*        ENDLOOP.
*      ELSEIF p_r3 = 'X'.       " Create after delete
*        LOOP AT it_data.
*          PERFORM call_bapi_delete.
*          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
*        ENDLOOP.
*        WAIT UP TO '5.3' SECONDS.
*        LOOP AT it_data.
*          PERFORM convert_data.
*          PERFORM call_bapi_function.
*          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
*        ENDLOOP.
*      ELSEIF p_r4 = 'X'.
*        LOOP AT it_data.
*          PERFORM call_bapi_change.
*          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
*        ENDLOOP.
*      ENDIF.
*
**---2003/12/11 Approval
*    WHEN 'APPROVAL'.
*      CLEAR : wa_t_cnt,
*              wa_s_cnt,
*              wa_f_cnt.
*      DESCRIBE TABLE it_data LINES wa_t_cnt.
*      LOOP AT it_data.
*        PERFORM approval_update USING it_data-aposnr.
*      ENDLOOP.
*      WRITE : / 'Total count :' , wa_t_cnt,
*             /  'Success     :' , wa_s_cnt,
*             /  'Failure     :' , wa_f_cnt.
*  ENDCASE.
*&---------------------------------------------------------------------*
*&      Form  F4_P_UPFILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4_p_upfile.

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

ENDFORM.                    " F4_P_UPFILE

*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_G_LINES  text
*----------------------------------------------------------------------*
FORM read_data USING    l_lines.
  REFRESH : it_data.
  CLEAR : it_data, it_data[], g_cancel, wa_subrc.
  CALL FUNCTION 'UPLOAD'
       EXPORTING
            filename = p_upfile
            filetype = g_type
       IMPORTING
            filesize = g_length
            cancel   = g_cancel
       TABLES
            data_tab = it_data.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    MESSAGE s000(zmfi) WITH 'Upload error'.
    EXIT.
    wa_subrc = 'X'.
  ENDIF.



ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_FB01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM get_data_bdc.
*  CLEAR : bdc_tab, bdc_tab[].
*
*  PERFORM dynpro USING :
*        'X' 'SAPLAIA1'      '0105',
*        ' ' 'IMAK-IVART'    it_data-ivart,
*        ' ' 'BDC_OKCODE'    '/00'.
*
*  PERFORM dynpro USING :
*        'X' 'SAPLAIA1'      '0200',
*        ' ' 'IMAKT-TXT50'   it_data-txt50,  "Desc
*        ' ' 'IMAK-SIZECL'   it_data-sizecl,
*        ' ' 'IMAK-PRIORI'   it_data-priori,
*        ' ' 'IMAKPI-IZWEK'  it_data-izwek1,
*        ' ' 'IMAK-GJAHR'    it_data-gjahr,
*        ' ' 'RAIA1-APOSNR'  it_data-aposnr,
*        ' ' 'RQM02-PARNR_VERA'   it_data-parnr_vera, "Applicant
*        ' ' 'IMAK-USR02'    it_data-usr02,   "Source
*        ' ' 'IMAK-USR03'    IT_DATA-USR03,    "ASSERT
*        ' ' 'IMAK-USR04'    IT_DATA-USR04,    "QUETY
*        ' ' 'IMAK-USE04'    'EA',   "EA
*        ' ' 'IMAK-USR09'    IT_DATA-USR09,   "CPA DATE
*        ' ' 'BDC_OKCODE'  '=AIZW'.
*
*  PERFORM dynpro USING :
*        'X' 'SAPLAIA1'    '0220',
*        ' ' 'IMAKPI-IZWEK(02)'  it_data-izwek2,
*        ' ' 'IMAKPI-APROZ(01)'  it_data-aproz1,
*        ' ' 'IMAKPI-APROZ(02)'  it_data-aproz2,
*        ' ' 'BDC_OKCODE'  '=BACK'.
*
*  PERFORM dynpro USING :
*        'X' 'SAPLAIA1'      '0200',
*        ' ' 'IMAKT-TXT50'   it_data-txt50,
*        ' ' 'IMAK-SIZECL'   it_data-sizecl,
*        ' ' 'IMAK-PRIORI'   it_data-priori,
*        ' ' 'IMAK-GJAHR'    it_data-gjahr,
*        ' ' 'RAIA1-APOSNR'   it_data-aposnr,
*        ' ' 'RQM02-PARNR_VERA'   it_data-parnr_vera,
*        ' ' 'IMAK-USR02'       it_data-usr02,
*        ' ' 'BDC_OKCODE'  '=ANFO'.
*
*  PERFORM dynpro USING :
*        'X' 'SAPLAIA1'      '0205',
*        ' ' 'IMAK-ABUKRS'   'H201',
*        ' ' 'IMAKPA-AKOSTL' it_data-akostl,
*        ' ' 'IMAK-VKOKRS'   'H201',
*        ' ' 'IMAK-VBUKRS'   it_data-vbukrs,
*        ' ' 'IMAK-VKOSTL'   it_data-vkostl,
*        ' ' 'IMAK-SCOPE'    'INVST',
*        ' ' 'IMAK-WERKS'    it_data-werks,
*        ' ' 'BDC_OKCODE'  '=VARN'.
*
*  PERFORM dynpro USING :
*        'X' 'SAPLAIA1'      '0400',
*        ' ' 'IMAVT-TXV50(01)'   it_data-txt50,
*        ' ' 'IMAV-BWERT(01)'    it_data-bwert,
**        ' ' 'IMAV-FDATU(01)'    it_data-date1,
**        ' ' 'IMAV-IDATU(01)'    it_data-date2,
*        ' ' 'VARI_MARK(01)'   'X',
*        ' ' 'BDC_OKCODE'  '=AVAR'.
*
*  PERFORM dynpro USING :
*        'X' 'SAPLAIA1'      '0320',
*        ' ' 'IMAVT-TXV50'   it_data-txt50,
*        ' ' 'IMAV-BWERT'    it_data-bwert,
**        ' ' 'IMAV-FDATU'    it_data-date1,
**        ' ' 'IMAV-IDATU'    it_data-date2,
**        ' ' 'ANIA-ANLKL'    it_data-anlkl,
**        ' ' 'ANIA-AKTIV'    it_data-aktiv,
*        ' ' 'BDC_OKCODE'  '=PLAN'.
*
*  PERFORM dynpro USING :
*        'X' 'SAPLAIA9'      '0100',
*        ' ' 'RAIA9-INVKOS(01)'   it_data-invkos1,
*        ' ' 'RAIA9-INVKOS(02)'   it_data-invkos2,
*        ' ' 'RAIA9-INVKOS(03)'   it_data-invkos3,
*        ' ' 'RAIA9-INVKOS(04)'   it_data-invkos4,
*        ' ' 'RAIA9-INVKOS(05)'   it_data-invkos5,
*        ' ' 'RAIA9-INVKOS(06)'   it_data-invkos6,
*        ' ' 'RAIA9-INVKOS(07)'   it_data-invkos7,
**        ' ' 'RAIA9-INVKOS(08)'   IT_DATA-INVKOS8,
**        ' ' 'RAIA9-INVKOS(09)'   IT_DATA-INVKOS9,
**        ' ' 'RAIA9-INVKOS(10)'   IT_DATA-INVKOS10,
**        ' ' 'RAIA9-INVKOS(11)'   IT_DATA-INVKOS11,
**        ' ' 'RAIA9-INVKOS(12)'   IT_DATA-INVKOS12,
**        ' ' 'RAIA9-INVKOS(13)'   IT_DATA-INVKOS13,
*        ' ' 'RAIA9-GEMKOS(01)'   it_data-gemkos1,
*        ' ' 'RAIA9-GEMKOS(02)'   it_data-gemkos2,
*        ' ' 'RAIA9-GEMKOS(03)'   it_data-gemkos3,
*        ' ' 'RAIA9-GEMKOS(04)'   it_data-gemkos4,
*        ' ' 'RAIA9-GEMKOS(05)'   it_data-gemkos5,
*        ' ' 'RAIA9-GEMKOS(06)'   it_data-gemkos6,
*        ' ' 'RAIA9-GEMKOS(07)'   it_data-gemkos7,
**        ' ' 'RAIA9-GEMKOS(08)'   IT_DATA-GEMKOS8,
**        ' ' 'RAIA9-GEMKOS(09)'   IT_DATA-GEMKOS9,
**        ' ' 'RAIA9-GEMKOS(10)'   IT_DATA-GEMKOS10,
**        ' ' 'RAIA9-GEMKOS(11)'   IT_DATA-GEMKOS11,
**        ' ' 'RAIA9-GEMKOS(12)'   IT_DATA-GEMKOS12,
**        ' ' 'RAIA9-GEMKOS(13)'   IT_DATA-GEMKOS13,
*        ' ' 'RAIA9-ERTRAG(02)'   it_data-ertrag2,
*        ' ' 'RAIA9-ERTRAG(03)'   it_data-ertrag3,
*        ' ' 'RAIA9-ERTRAG(04)'   it_data-ertrag4,
*        ' ' 'RAIA9-ERTRAG(05)'   it_data-ertrag5,
*        ' ' 'RAIA9-ERTRAG(06)'   it_data-ertrag6,
*        ' ' 'RAIA9-ERTRAG(07)'   it_data-ertrag7,
**        ' ' 'RAIA9-ERTRAG(08)'   IT_DATA-ERTRAG8,
**        ' ' 'RAIA9-ERTRAG(09)'   IT_DATA-ERTRAG9,
**        ' ' 'RAIA9-ERTRAG(10)'   IT_DATA-ERTRAG10,
**        ' ' 'RAIA9-ERTRAG(11)'   IT_DATA-ERTRAG11,
**        ' ' 'RAIA9-ERTRAG(12)'   IT_DATA-ERTRAG12,
**        ' ' 'RAIA9-ERTRAG(13)'   IT_DATA-ERTRAG13,
*        ' ' 'BDC_OKCODE'  '=UEBE'.
*
*  PERFORM dynpro USING :
*        'X' 'SAPLAIA1'      '0320',
*        ' ' 'IMAVT-TXV50'        it_data-txt50,
*        ' ' 'IMAV-BWERT'         it_data-bwert,
**        ' ' 'IMAV-FDATU'         it_data-date1,
**        ' ' 'IMAV-IDATU'         it_data-date2,
**        ' ' 'ANIA-ANLKL'         it_data-anlkl,
**        ' ' 'ANIA-AKTIV'         it_data-aktiv,
*        ' ' 'BDC_OKCODE'  '=BUCH'.
*
*
*
*ENDFORM.                    " GET_DATA_BDC
*
*&---------------------------------------------------------------------*
*&      Form  CALL_BDC_FB01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_bdc_call.
  CLEAR : message.

  CALL TRANSACTION 'IMA1'  USING bdc_tab
                           MODE   bdc_mode
                           UPDATE 'S'
                           MESSAGES INTO messtab.

  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
       EXPORTING
            msgid               = sy-msgid
            msgnr               = sy-msgno
            msgv1               = sy-msgv1
            msgv2               = sy-msgv2
            msgv3               = sy-msgv3
            msgv4               = sy-msgv4
       IMPORTING
            message_text_output = message.

  IF sy-msgno = '312'. "?? ?? ?? ???

  ENDIF.
  it_mess-message = message.
  APPEND it_mess.
  CLEAR it_mess.

ENDFORM.                    " CALL_BDC_CALL
*&---------------------------------------------------------------------*
*&      Form  DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0399   text
*      -->P_0400   text
*      -->P_0401   text
*----------------------------------------------------------------------*
FORM dynpro USING  dynbegin name value.

  IF dynbegin  = 'X'.
    CLEAR bdc_tab.
    MOVE : name  TO bdc_tab-program,
           value TO bdc_tab-dynpro,
           'X'   TO bdc_tab-dynbegin.
    APPEND bdc_tab.
  ELSE.
    CLEAR bdc_tab.
    MOVE : name  TO bdc_tab-fnam,
           value TO bdc_tab-fval.
    APPEND bdc_tab.
  ENDIF.

ENDFORM.                    " DYNPRO
*&---------------------------------------------------------------------*
*&      Form  CONVERSION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM conversion.

*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*         EXPORTING
*              INPUT  = IT_DATA-ANLN1
*         IMPORTING
*              OUTPUT = IT_DATA-ANLN1.
*

ENDFORM.                    " CONVERSION
*&---------------------------------------------------------------------*
*&      Form  CALL_BAPI_FUNCTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_bapi_function.
  REFRESH: i_msg. CLEAR: i_msg.

  PERFORM fill_bapi_data.
* just add variant
  IF p_gjahr <> ' '.
* add variant
    it_version-appr_year    = p_gjahr.
    it_version-plan_version = p_versn.
    APPEND it_version.

    CALL FUNCTION 'BAPI_APPREQUEST_ADDVARIANT'
      EXPORTING
        externalnumber                       = it_data-aposnr
*   APPROPRIATIONREQUESTVARIANT_IN       = '0010'
        variant                              = wa_variant
        plan_total                           = wa_plan_total
        test_run                             = p_test
* IMPORTING
*   APPROPRIATIONREQUESTVARIANTOUT       =
     TABLES
       variant_to_version                   = it_version
       plan_year                            = it_plan_year
       return                               = i_msg.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
* EXPORTING
*  WAIT          =
     IMPORTING
       return        =  i_msg.

*------------------------------------------*
  ELSE.
    CALL FUNCTION 'BAPI_APPREQUEST_CREATE'
      EXPORTING
        appropriationrequest_in              =  it_data-aposnr
        apprequest_type                      =  it_data-ivart
*   APPROPRIATIONREQUESTVARIANT_IN       =
        controlling_area                     = p_bukrs
        master_data                          = wa_master
        user_fields                          = wa_user_fields
        variant                              = wa_variant
        plan_total                           = wa_plan_total
        language                             = sy-langu
*   LANGUAGE_ISO                         =
      test_run                             = wa_test
     IMPORTING
       externalnumber                       = wa_externalnumber
       appropriationrequestvariantout       = wa_appropriat
     TABLES
       org_units                            =  it_org_units
*   DIVISION                             =
     material_grp                         =    it_material_grp
        investment_reason                    = it_invest_reason
*   ENVIRONMNT_INVEST                    =
*   ASSETS_EQUIS                         =
*   ORDERS                               =
*   WBS_ELEMENT                          =
        partner                              = it_partner
        variant_to_version                   = it_variant
        plan_year                            = it_plan_year
*   ASSIGNMENT_TO_POS                    =
*   ASSIGNMENT_TO_BUDG_CATEG             =
       return                               = i_msg.

  ENDIF.

  PERFORM check_bapi_message.
ENDFORM.                    " CALL_BAPI_FUNCTION
*&---------------------------------------------------------------------*
*&      Form  CONVERT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM convert_data.
*  CLEAR it_data-ivart.
*  it_data-ivart = it_data-aposnr+4(1).
*  if it_data-ivart = '5'.
*    it_data-ivart = '2'.
*  endif.

  PERFORM fill_data USING it_data-ivart
                    CHANGING it_data-ivart.
  PERFORM fill_data USING it_data-sizecl
                    CHANGING it_data-sizecl.
  PERFORM fill_data USING it_data-akostl
                    CHANGING it_data-akostl.
  PERFORM fill_data USING it_data-vkostl
                    CHANGING it_data-vkostl.
  PERFORM fill_data USING it_data-bwert
                    CHANGING it_data-bwert.
*  PERFORM fill_data USING it_data-anlkl
*                    CHANGING it_data-anlkl.
ENDFORM.                    " CONVERT_DATA
*&---------------------------------------------------------------------*
*&      Form  FILL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_DATA_IVART  text
*      <--P_IT_DATA_IVART  text
*----------------------------------------------------------------------*
FORM fill_data USING    u_field
               CHANGING c_field.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
            input  = u_field
       IMPORTING
            output = c_field.
ENDFORM.                    " FILL_DATA
*&---------------------------------------------------------------------*
*&      Form  call_bapi_DELETE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_bapi_delete.
  CALL FUNCTION 'BAPI_APPREQUEST_DELETE'
       EXPORTING
            externalnumber = it_data-aposnr
            test_run       = wa_test
       TABLES
            return         = i_msg.

ENDFORM.                    " call_bapi_DELETE
*&---------------------------------------------------------------------*
*&      Form  APPROVAL_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM approval_update USING u_aposnr.
  REFRESH : it_bdc.
  CLEAR   : it_bdc.

  tcode = 'IMA2'.
  PERFORM make_bdc_rtn USING :
                     'X'  'SAPLAIA1'        '0100',
                     ' '  'IMAK-POSNR'      u_aposnr,
                     ' '  'BDC_OKCODE'      '/00'.
  PERFORM make_bdc_rtn USING :
                      'X'  'SAPLAIA1'       '0200',
                     ' '  'BDC_OKCODE'      '=STAV'.
  PERFORM make_bdc_rtn USING :
                      'X'  'SAPLAIA1'       '0200',
                     ' '  'BDC_OKCODE'      '=STAV'.
  PERFORM make_bdc_rtn USING :
                     'X'  'SAPLAIA1'        '0200',
                     ' '  'BDC_OKCODE'      '=BUCH'.
  CALL TRANSACTION tcode   USING it_bdc
                           MODE   'E'
                           UPDATE 'S'
*                    OPTIONS  FROM CTU_PARAMS
                    MESSAGES INTO it_messtab.
  READ TABLE it_messtab WITH KEY msgtyp = 'S'
                                 msgid = 'AO'
                                 msgnr = '010'.

  IF sy-subrc = 0.
    wa_s_cnt = wa_s_cnt + 1.
  ELSE.
    wa_f_cnt = wa_f_cnt + 1.
  ENDIF.
ENDFORM.                    " APPROVAL_UPDATE
*&---------------------------------------------------------------------*
*&      Form  make_bdc_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1410   text
*      -->P_1411   text
*      -->P_1412   text
*----------------------------------------------------------------------*
FORM make_bdc_rtn USING   dynbegin program dynpro.
  CLEAR it_bdc.

  IF dynbegin = 'X'.
    it_bdc-program  = program.
    it_bdc-dynpro   = dynpro.
    it_bdc-dynbegin = 'X'.
  ELSE.
    it_bdc-fnam     = program.
    it_bdc-fval     = dynpro.
  ENDIF.

  APPEND it_bdc.

ENDFORM.                    " make_bdc_rtn
*&---------------------------------------------------------------------*
*&      Form  call_bapi_change
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_bapi_change.

*====================*
*--PLAN TOTAL
  wa_plan_total-overhead_costs =   it_data-gemkos1.         "1000.
  wa_plan_total-investment_costs = it_data-invkos1.         "10000.
*=========================*
*----IT_PLAN_YEAR
  REFRESH : it_plan_year.
  CLEAR it_plan_year.
  IF it_data-gjahr < '1111'.
    MESSAGE i000(zmfi) WITH 'check approval year'.
    EXIT.
  ENDIF.
  IF it_data-aposnr  = ' '.
    MESSAGE i000(zmfi) WITH 'check app. number'.
    EXIT.
  ENDIF.
  wk_date = it_data-gjahr - 1.
  DO 9 TIMES.
    IF sy-index = 1.
      MOVE wk_date TO it_plan_year-fiscal_year.
      MOVE it_data-gemkos2 TO it_plan_year-overhead_costs.
      MOVE it_data-invkos2 TO it_plan_year-investment_costs.
      MOVE it_data-ertrag2 TO it_plan_year-revenue.
    ELSEIF sy-index = 2.
      MOVE wk_date TO it_plan_year-fiscal_year.
      MOVE it_data-gemkos3 TO it_plan_year-overhead_costs.
      MOVE it_data-invkos3 TO it_plan_year-investment_costs.
      MOVE it_data-ertrag3 TO it_plan_year-revenue.
    ELSEIF sy-index = 3.
      MOVE wk_date TO it_plan_year-fiscal_year.
      MOVE it_data-gemkos4 TO it_plan_year-overhead_costs.
      MOVE it_data-invkos4 TO it_plan_year-investment_costs.
      MOVE it_data-ertrag4 TO it_plan_year-revenue.
    ELSEIF sy-index = 4.
      MOVE wk_date TO it_plan_year-fiscal_year.
      MOVE it_data-gemkos5 TO it_plan_year-overhead_costs.
      MOVE it_data-invkos5 TO it_plan_year-investment_costs.
      MOVE it_data-ertrag5 TO it_plan_year-revenue.
    ELSEIF sy-index = 5.
      MOVE wk_date TO it_plan_year-fiscal_year.
      MOVE it_data-gemkos6 TO it_plan_year-overhead_costs.
      MOVE it_data-invkos6 TO it_plan_year-investment_costs.
      MOVE it_data-ertrag6 TO it_plan_year-revenue.
    ELSEIF sy-index = 6.
      MOVE wk_date TO it_plan_year-fiscal_year.
      MOVE it_data-gemkos7 TO it_plan_year-overhead_costs.
      MOVE it_data-invkos7 TO it_plan_year-investment_costs.
      MOVE it_data-ertrag7 TO it_plan_year-revenue.
    ELSEIF sy-index = 7.
      MOVE wk_date TO it_plan_year-fiscal_year.
      MOVE it_data-gemkos8 TO it_plan_year-overhead_costs.
      MOVE it_data-invkos8 TO it_plan_year-investment_costs.
      MOVE it_data-ertrag8 TO it_plan_year-revenue.
    ELSEIF sy-index = 8.
      MOVE wk_date TO it_plan_year-fiscal_year.
      MOVE it_data-gemkos9 TO it_plan_year-overhead_costs.
      MOVE it_data-invkos9 TO it_plan_year-investment_costs.
      MOVE it_data-ertrag9 TO it_plan_year-revenue.
    ELSEIF sy-index = 9.
      MOVE wk_date TO it_plan_year-fiscal_year.
      MOVE it_data-gemkos10 TO it_plan_year-overhead_costs.
      MOVE it_data-invkos10 TO it_plan_year-investment_costs.
      MOVE it_data-ertrag10 TO it_plan_year-revenue.
    ENDIF.
    APPEND it_plan_year.
    CLEAR it_plan_year.
    wk_date = wk_date + 1.

  ENDDO.

  DATA: l_vrnt LIKE bapiappreqvarntassignmulti-appreqvrnt.
  PERFORM get_variant_number USING it_data-aposnr l_vrnt.

*======================================*
  CALL FUNCTION 'BAPI_APPREQUEST_SETPLANVALUES'
    EXPORTING
      externalnumber                    = it_data-aposnr
      appropriationrequestvariant       = l_vrnt
      plan_total                        =  wa_plan_total
*   TEST_RUN                          = ' '
   TABLES
      plan_year                         = it_plan_year
      return                            = i_msg.

  PERFORM check_bapi_message.

ENDFORM.                    " call_bapi_change
*&---------------------------------------------------------------------*
*&      Form  fill_bapi_data
*&---------------------------------------------------------------------*
FORM fill_bapi_data.
  DATA : wk_date LIKE bapiappreqvarntassign-appr_year.
  DATA : wk_date1 LIKE sy-datum.

  CLEAR : wa_master, wa_user_fields, wa_variant, wa_plan_total.
*=========--- IMPORT VALUE
* Master_DATA
  wa_master-req_txt         = it_data-txt50.  "BAPITest'.
  wa_master-req_comp_code   = it_data-vbukrs. "Company code
  wa_master-rsp_comp_code   = it_data-vbukrs.
  wa_master-plant           = it_data-werks.                "P001'.
  wa_master-scale           = it_data-sizecl.               "01'.
  wa_master-priority        = it_data-priori. "1'.
  wa_master-rsp_cost_center = it_data-vkostl.  "0000022001'.
  wa_master-orig_appr_year  = it_data-gjahr.                "2003'.
  wa_master-desired_start   = it_data-date1.  "im start date 20030101'.
  wa_master-objectclass     = 'IV'.
*FIXIT currency
  wa_master-object_currency = 'USD'.
*--User_fields
* wa_user_fields-user00     =  it_data-usr00.   "1'.
  wa_user_fields-user02     =  it_data-usr02.   "1'.
  wa_user_fields-user03     =  it_data-usr03.   "ASSERT
  wa_user_fields-user04_quantity     =  it_data-usr04.   "qty
  wa_user_fields-user04_unit         =  'EA'.
  wa_user_fields-user09_date         =  it_data-usr09.   "cap date

*  WA_USER_FIELDS-USER03     =  IT_DATA-ANLKL.
*--Variant
  wa_variant-description        = 'Initial Plan'.
  wa_variant-assessmnt_crit     =  it_data-bwert.           "0001'.
*  wa_variant-completion_date    =  it_data-date2.           "20040101'.
*  wa_variant-start_up_date      =  it_data-aktiv.           "20040101'.
  wk_date = it_data-gjahr + 1.
  CONCATENATE wk_date '0101' INTO wk_date1.
  wa_variant-value_date   =  wk_date1.
*--PLAN TOTAL
  wa_plan_total-overhead_costs =   it_data-gemkos1.         "1000.
  wa_plan_total-investment_costs = it_data-invkos1.         "10000.
*--- Table  ORG_UNITS
  REFRESH : it_org_units.
  CLEAR it_org_units.
  MOVE  it_data-akostl TO  it_org_units-req_cost_center.
  MOVE '100'           TO  it_org_units-percentage.
  APPEND it_org_units.
*--it_invest_reason .
  REFRESH : it_invest_reason.
  CLEAR it_invest_reason.
*--- it_material group
  REFRESH : it_material_grp.
  CLEAR   : it_material_grp.
*---jhs modify 2003/12/05
  IF it_data-mgrp1 <> ' '.
    MOVE it_data-mgrp1     TO it_material_grp-req_material_grp.
    MOVE it_data-mgrp1_rat TO it_material_grp-percentage.
    APPEND it_material_grp.
    CLEAR  it_material_grp.
  ENDIF.
  IF it_data-mgrp2 <> ' '.
    MOVE it_data-mgrp2     TO it_material_grp-req_material_grp.
    MOVE it_data-mgrp2_rat TO it_material_grp-percentage.
    APPEND it_material_grp.
    CLEAR  it_material_grp.
  ENDIF.

  IF NOT it_data-izwek1 IS INITIAL.
    MOVE it_data-izwek1 TO it_invest_reason-inv_reason.
    MOVE it_data-aproz1 TO it_invest_reason-percentage.
*    MOVE 'ZZ'    TO  it_invest_reason-inv_reason.
*    MOVE '100'    TO  it_invest_reason-percentage.
    APPEND it_invest_reason.
    CLEAR  it_invest_reason.
  ENDIF.
  IF NOT it_data-izwek2 IS INITIAL.
    MOVE it_data-izwek2 TO it_invest_reason-inv_reason.
    MOVE it_data-aproz2 TO it_invest_reason-percentage.
    APPEND it_invest_reason.
    CLEAR  it_invest_reason.
  ENDIF.
  IF NOT it_data-izwek3 IS INITIAL.
    MOVE it_data-izwek3 TO it_invest_reason-inv_reason.
    MOVE it_data-aproz3 TO it_invest_reason-percentage.
    APPEND it_invest_reason.
    CLEAR  it_invest_reason.
  ENDIF.

*--- IT_PARTNER.
  REFRESH : it_partner.
  CLEAR it_partner.
  MOVE 'I1'                TO it_partner-partner_function.
  MOVE it_data-parnr_vera  TO it_partner-partner.
*  MOVE sy-uname            TO it_partner-partner.
  APPEND it_partner.

  CLEAR  it_partner.
  MOVE 'I4'                TO it_partner-partner_function.
  MOVE it_data-parnr_vera  TO it_partner-partner.
*  MOVE sy-uname            TO it_partner-partner.
  APPEND it_partner.

*---VARIANT_TO_VERSION.
  REFRESH : it_variant.
  CLEAR it_variant.
  MOVE it_data-gjahr  TO it_variant-appr_year.
  MOVE p_versn   TO it_variant-plan_version.
  APPEND it_variant.
*----IT_PLAN_YEAR
  REFRESH : it_plan_year.
  CLEAR it_plan_year.
  wk_date = it_data-gjahr - 1.
  DO 9 TIMES.
    IF sy-index = 1.
      MOVE wk_date TO it_plan_year-fiscal_year.
      MOVE it_data-gemkos2 TO it_plan_year-overhead_costs.
      MOVE it_data-invkos2 TO it_plan_year-investment_costs.
      MOVE it_data-ertrag2 TO it_plan_year-revenue.
    ELSEIF sy-index = 2.
      MOVE wk_date TO it_plan_year-fiscal_year.
      MOVE it_data-gemkos3 TO it_plan_year-overhead_costs.
      MOVE it_data-invkos3 TO it_plan_year-investment_costs.
      MOVE it_data-ertrag3 TO it_plan_year-revenue.
    ELSEIF sy-index = 3.
      MOVE wk_date TO it_plan_year-fiscal_year.
      MOVE it_data-gemkos4 TO it_plan_year-overhead_costs.
      MOVE it_data-invkos4 TO it_plan_year-investment_costs.
      MOVE it_data-ertrag4 TO it_plan_year-revenue.
    ELSEIF sy-index = 4.
      MOVE wk_date TO it_plan_year-fiscal_year.
      MOVE it_data-gemkos5 TO it_plan_year-overhead_costs.
      MOVE it_data-invkos5 TO it_plan_year-investment_costs.
      MOVE it_data-ertrag5 TO it_plan_year-revenue.
    ELSEIF sy-index = 5.
      MOVE wk_date TO it_plan_year-fiscal_year.
      MOVE it_data-gemkos6 TO it_plan_year-overhead_costs.
      MOVE it_data-invkos6 TO it_plan_year-investment_costs.
      MOVE it_data-ertrag6 TO it_plan_year-revenue.
    ELSEIF sy-index = 6.
      MOVE wk_date TO it_plan_year-fiscal_year.
      MOVE it_data-gemkos7 TO it_plan_year-overhead_costs.
      MOVE it_data-invkos7 TO it_plan_year-investment_costs.
      MOVE it_data-ertrag7 TO it_plan_year-revenue.
    ELSEIF sy-index = 7.
      MOVE wk_date TO it_plan_year-fiscal_year.
      MOVE it_data-gemkos8 TO it_plan_year-overhead_costs.
      MOVE it_data-invkos8 TO it_plan_year-investment_costs.
      MOVE it_data-ertrag8 TO it_plan_year-revenue.
    ELSEIF sy-index = 8.
      MOVE wk_date TO it_plan_year-fiscal_year.
      MOVE it_data-gemkos9 TO it_plan_year-overhead_costs.
      MOVE it_data-invkos9 TO it_plan_year-investment_costs.
      MOVE it_data-ertrag9 TO it_plan_year-revenue.
    ELSEIF sy-index = 9.
      MOVE wk_date TO it_plan_year-fiscal_year.
      MOVE it_data-gemkos10 TO it_plan_year-overhead_costs.
      MOVE it_data-invkos10 TO it_plan_year-investment_costs.
      MOVE it_data-ertrag10 TO it_plan_year-revenue.
    ENDIF.
    APPEND it_plan_year.
    CLEAR it_plan_year.
    wk_date = wk_date + 1.

  ENDDO.
ENDFORM.                    " fill_bapi_data
*&---------------------------------------------------------------------*
*&      Form  get_variant_number
*&---------------------------------------------------------------------*
FORM get_variant_number USING    f_posnr  f_vrnt.
  DATA : i_variant LIKE bapiappreqvarntassignmulti OCCURS 0
                                                   WITH HEADER LINE.

  CALL FUNCTION 'BAPI_APPREQUEST_GETDETAIL'
       EXPORTING
            externalnumber     = f_posnr
       TABLES
            variant_to_version = i_variant.

  READ TABLE i_variant WITH KEY appr_year    = p_gjahr
                                plan_version = p_versn.


  f_vrnt = i_variant-appreqvrnt.
ENDFORM.                    " get_variant_number
*&---------------------------------------------------------------------*
*&      Form  check_bapi_message
*&---------------------------------------------------------------------*
FORM check_bapi_message.
  READ TABLE i_msg WITH KEY type = 'E' TRANSPORTING NO FIELDS.
  IF sy-subrc NE 0.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
  ELSE.
    WRITE:/ '***Error:', it_data-aposnr.
  ENDIF.

*  LOOP AT i_msg.
*    WRITE:/ i_msg-type, i_msg-id, i_msg-number,
*            i_msg-message.
*  ENDLOOP.
*
  append lines of i_msg to g_msg .

ENDFORM.                    " check_bapi_message
*&---------------------------------------------------------------------*
*&      Form  move_to_out
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM move_to_out.

  DATA $ix TYPE i.

  __cls gt_out.

  LOOP AT it_data.
    $ix = sy-tabix.
    IF it_data-ivart IS INITIAL AND
        it_data-aposnr IS INITIAL.
      DELETE it_data INDEX $ix.
      CONTINUE.
    ENDIF.

*        invkos1(15),    "Overall  Total
*        invkos2(15),                                        "2002
*        invkos3(15),                                        "2003
*        invkos4(15),                                        "2004
*        invkos5(15),                                        "2005
*        invkos6(15),                                        "2006
*        invkos7(15),                                        "2007
*        invkos8(15),                                        "2008
*        invkos9(15),                                        "2009
*        invkos10(15),                                       "2010

    IF it_data-invkos1 CN num OR
       it_data-invkos2 CN num OR
       it_data-invkos3 CN num OR
       it_data-invkos4 CN num OR
       it_data-invkos5 CN num OR
       it_data-invkos6 CN num OR
       it_data-invkos7 CN num OR
       it_data-invkos8 CN num OR
       it_data-invkos9 CN num OR
       it_data-invkos10 CN num.
      DELETE it_data INDEX $ix.
      CONTINUE.
    ENDIF.

    MOVE-CORRESPONDING it_data TO gt_out.
    APPEND gt_out.
  ENDLOOP.

ENDFORM.                    " move_to_out
*&---------------------------------------------------------------------*
*&      Form  set_output
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_output.
  CHECK g_error IS INITIAL.

  PERFORM show_progress     USING 'Preparing screen...' '95'.
  PERFORM init_alv_parm.
  PERFORM fieldcat_init     USING gt_fieldcat[].
  PERFORM sort_build        USING gt_sort[].
*  PERFORM alv_events_get    USING:  'P', 'T'.
  PERFORM alv_grid_display  TABLES  gt_out USING ''.


ENDFORM.                    " set_output

*---------------------------------------------------------------------*
*       FORM show_progress                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  PF_TEXT                                                       *
*  -->  VALUE(PF_VAL)                                                 *
*---------------------------------------------------------------------*
FORM show_progress USING    pf_text
                            value(pf_val).

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            percentage = pf_val
            text       = pf_text.

ENDFORM.                    " SHOW_PROGRESS
**&---------------------------------------------------------------------*
*&      Form  init_alv_parm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_alv_parm.
  __cls   :  gt_fieldcat, gt_sort, gt_events, gt_listheader,
             gt_sp_group.

  CLEAR   :  gs_layout.

  gs_layout-colwidth_optimize = 'X'.

*   Set variant
  gv_repid = gs_variant-report = sy-repid.
  gs_variant-variant = p_vari.

ENDFORM.                    " init_alv_parm
*&---------------------------------------------------------------------*
*&      Form  fieldcat_init
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCAT[]  text
*----------------------------------------------------------------------*
FORM fieldcat_init USING ft_fieldcat TYPE slis_t_fieldcat_alv .

  DATA: l_pos TYPE i.

  __cls ft_fieldcat.

  DEFINE __catalog.
    l_pos = l_pos + 1.
    clear gs_fieldcat.
    gs_fieldcat-col_pos       = l_pos.
    gs_fieldcat-key           = &1.
    gs_fieldcat-fieldname     = &2.
    gs_fieldcat-seltext_m     = &3.        " Column heading
    gs_fieldcat-outputlen     = &4.        " Column width
    gs_fieldcat-datatype      = &5.        " Data type
    gs_fieldcat-emphasize     = &6.
    gs_fieldcat-cfieldname    = &7.
    gs_fieldcat-no_zero       = &8.
    append gs_fieldcat to  ft_fieldcat.
  END-OF-DEFINITION.

  __catalog :

'X'  'IVART'      'type'                     2  'CHAR' '' '' '',
'X'  'APOSNR'     'Request Number'          24  'CHAR' '' '' '',
' '  'TXT50'      'text'  40 'CHAR' '' '' '',
' '  'IZWEK1'     'reason for investment'  2 'CHAR' '' '' '',
' '  'APROZ1'     'Rate1	' 5   'CHAR' '' '' '',
' '  'IZWEK2'     'reason for investment' 2 'CHAR' '' '' '',
' '  'APROZ2'     'Rate2' 5   'CHAR' '' '' '',
' '  'IZWEK3'     'reason for investment' 2 'CHAR' '' '' '',
' '  'APROZ3'     'Rate3' 5   'CHAR' '' '' '',
' '  'UMWKZ'	    'Environment' 5 'CHAR' '' '' '',
' '  'SIZECL'     'Scal' 2   'CHAR' '' '' '',
' '  'PRIORI'     'Priority' 1   'CHAR' '' '' '',
' '  'GJAHR'	    'Approval Year' 4 'CHAR' '' '' '',
' '  'DATE1'	    'Start date' 8   'CHAR' '' '' '',
' '  'USR09'	    'End date' 8   'CHAR' '' '' '',
' '  'USR00'      'type' 10 'CHAR' '' '' '',
' '  'USR02'	    'Source' 10 'CHAR' '' '' '',
' '  'USR03'	    'Asset Class' 8 'CHAR' '' '' '',
' '  'USR04'	    'Qty' 13  'CHAR' '' '' '',
' '  'PARNR_VERA' 'Applicant' 12  'CHAR' '' '' '',
' '  'MGRP1'	    'material group' 3 'CHAR' '' '' '',
' '  'MGRP1_RAT'  'Rate2' 5   'CHAR' '' '' '',
' '  'MGRP2'	    'material group' 3 'CHAR' '' '' '',
' '  'MGRP2_RAT'  'Rate2' 5   'CHAR' '' '' '',
' '  'AKOSTL'     'Req. cost center' 10  'CHAR' '' '' '',
' '  'VKOSTL'     'Resp. CC' 10  'CHAR' '' '' '',
' '  'VBUKRS'     'Resp. company code' 4   'CHAR' '' '' '',
' '  'WERKS'	    'Plant' 4 'CHAR' '' '' '',
' '  'BWERT'	    'Assessment' 4   'CHAR' '' '' '',
' '  'INVKOS1'    'Overall  Total' 15 'CHAR' '' '' ''.

  DATA $text(4) TYPE n.
  DATA $title(8) TYPE c.
  DATA idx TYPE i.
  DATA $idx(10) TYPE c.

  $text = p_year - 2.

  DO 9 TIMES.
    idx = sy-index + 1.
    $idx = idx.
    CONDENSE $idx.
    CONCATENATE 'INVKOS' $idx INTO $title.
    CONDENSE $title.
    ADD 1 TO $text.
    __catalog :
    ' '  $title       $text 15  'CHAR' '' '' ''.
  ENDDO.

  __catalog :
' '  'GEMKOS1'       'Overall  Total				    ' 15
'CHAR' '' '' ''.

  $text = p_year - 2.

  DO 9 TIMES.
    idx = sy-index + 1.
    $idx = idx.
    CONDENSE $idx.
    CONCATENATE 'GEMKOS' $idx INTO $title.
    CONDENSE $title.
    ADD 1 TO $text.
    __catalog :
    ' '  $title       $text 15  'CHAR' '' '' ''.
  ENDDO.

  $text = p_year - 2.

  DO 9 TIMES.
    idx = sy-index + 1.
    $idx = idx.
    CONDENSE $idx.
    CONCATENATE 'ERTRAG' $idx INTO $title.
    CONDENSE $title.
    ADD 1 TO $text.
    __catalog :
    ' '  $title       $text 15  'CHAR' '' '' ''.
  ENDDO.


ENDFORM.                    " fieldcat_init
*&---------------------------------------------------------------------*
*&      Form  sort_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_SORT[]  text
*----------------------------------------------------------------------*
FORM sort_build USING    ft_sort TYPE slis_t_sortinfo_alv.

  DEFINE sort_tab.
    clear gs_sort.
    gs_sort-fieldname = &1.
    gs_sort-spos      = &2.
    gs_sort-up        = &3.
    gs_sort-group     = &4.
    gs_sort-comp      = &5.
    append gs_sort to ft_sort.
  END-OF-DEFINITION.

*  sort_tab :
*     'ivart'        ' ' 'X' 'X' 'X'.
*     'aposnr'        ' ' 'X' 'X' 'X'.
ENDFORM.                    " SORT_BUILD

*---------------------------------------------------------------------*
*       FORM user_command                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  FP_UCOMM                                                      *
*  -->  FS                                                            *
*---------------------------------------------------------------------*
FORM user_command USING fp_ucomm LIKE sy-ucomm
                        fs       TYPE slis_selfield.
  CLEAR : g_error.

__cls g_msg.

  CASE fp_ucomm.
    WHEN 'PROC'.
      sy-lsind = sy-lsind - 1.
*      loop at it_data.
*      endloop.
      IF p_r2 = 'X'.          "delete
        LOOP AT it_data.
          PERFORM call_bapi_delete.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
        ENDLOOP.
      ELSEIF p_r1 = 'X'.      "create
        LOOP AT it_data.
          PERFORM convert_data.
          PERFORM call_bapi_function.
        ENDLOOP.
      ELSEIF p_r3 = 'X'.       " Create after delete
        LOOP AT it_data.
          PERFORM call_bapi_delete.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
        ENDLOOP.
        WAIT UP TO '5.3' SECONDS.
        LOOP AT it_data.
          PERFORM convert_data.
          PERFORM call_bapi_function.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
        ENDLOOP.
      ELSEIF p_r4 = 'X'.
        LOOP AT it_data.
          PERFORM call_bapi_change.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
        ENDLOOP.
      ENDIF.

*---2003/12/11 Approval
    WHEN 'APPROVAL'.
      CLEAR : wa_t_cnt,
              wa_s_cnt,
              wa_f_cnt.
      DESCRIBE TABLE it_data LINES wa_t_cnt.
      LOOP AT it_data.
        PERFORM approval_update USING it_data-aposnr.
      ENDLOOP.

      WRITE : / 'Total count :' , wa_t_cnt,
             /  'Success     :' , wa_s_cnt,
             /  'Failure     :' , wa_f_cnt.

    when 'LOGV'.
      call screen '300'.

  ENDCASE.

ENDFORM.                    "USER_COMMAND

*---------------------------------------------------------------------*
*       FORM top_of_page                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM top_of_page.
  DATA l_text(60).
  REFRESH gt_listheader.

  l_text = 'IM-AR Mass Upload'.
  PERFORM set_header_line USING:
          'P' 'H' ''      l_text       ''.
*          'D' 'S' 'Period'   s_date-low  s_date-high.
*
*  IF p_op1 EQ true.
*    PERFORM set_header_line USING:
*            'S' 'S' 'Cost.C'   s_kostl-low  ''.
*  ENDIF.
*
*  IF p_op2 EQ true.
*    PERFORM set_header_line USING:
*            'S' 'S' 'Cost.C'   s_orgeh-low  ''.
*  ENDIF.
*
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            it_list_commentary = gt_listheader.

ENDFORM.                    "top_of_page

**---------------------------------------------------------------------*
**       FORM PF_STATUS_SET
**---------------------------------------------------------------------*
FORM pf_status_set USING  ft_extab TYPE slis_t_extab.
  SET PF-STATUS '100'.
ENDFORM.                    "PF_STATUS_SET
*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module STATUS_0300 output.
  set pf-status 'ZLOG'.
  sy-title = 'Error log...'.
  suppress dialog.
  leave to list-processing and return to screen 0.
  perform error_list.

endmodule.                 " STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  error_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form error_list.
read table g_msg index 1.
if sy-subrc ne 0.
    WRITE:/ 'No log was found!'.
endif.
exit.
  LOOP AT g_msg.
    WRITE:/ g_msg-type, g_msg-id, g_msg-number,
            g_msg-message.
  ENDLOOP.


*  loop at g_msg.
*    write:/ g_msg-message_v1(20),
*            g_msg-message_v2(10),
*            g_msg-message(40).
*  endloop.
*
endform.                    " error_list
