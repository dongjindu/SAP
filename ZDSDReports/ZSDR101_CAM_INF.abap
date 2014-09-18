************************************************************************
* Program Name      : ZSDR101_CAM_INF
* Author            : Furong Wang
* Creation Date     : 01/05/2006
* Specifications By :
* Development Request No :
* Addl Documentation:
* Description       : Monthly Good Received
* Modification Logs
* Date       Developer    RequestNo    Description
*
************************************************************************
REPORT zsdr101_cam_inf NO STANDARD PAGE HEADING LINE-SIZE 255
                       MESSAGE-ID zmsd.

*---// Constants
CONSTANTS: c_filename LIKE rlgrap-filename VALUE
                      '/usr/sap/EDI_SAP/cam_info_'.

*---// For FTP file creation
DATA: w_filename LIKE rlgrap-filename.

DATA: BEGIN OF it_data OCCURS 0,
      zcpis LIKE ztsd_cam_inf-zcpis,
      zcpnm LIKE ztsd_cam_inf-zcpnm,
      zcptn LIKE ztsd_cam_inf-zcptn,
      zptnm LIKE ztsd_cam_inf-zptnm,
      zcaus LIKE ztsd_cam_inf-zcaus,
      znatr LIKE ztsd_cam_inf-znatr,
      zvhqt LIKE ztsd_cam_inf-zvhqt,
      zeffm LIKE ztsd_cam_inf-zeffm,
      zefto LIKE ztsd_cam_inf-zefto,
      zvseq LIKE ztsd_cam_inf-zvseq,
      zvend LIKE ztsd_cam_inf-zvend,
      zvprt LIKE ztsd_cam_inf-zvprt,
      zvlrt LIKE ztsd_cam_inf-zvlrt,
      zvsrt LIKE ztsd_cam_inf-zvsrt,
      END OF it_data.

DATA: BEGIN OF it_out OCCURS 0,
      hkgb(01),   "'H'
      doex(2),    "'A'
      zcpis(10),  " LIKE ztsd_cam_inf-zcpis,
      natn(5),
      zcpnm(40),  " LIKE ztsd_cam_inf-zcpnm,
      zcptn(15),  " LIKE ztsd_cam_inf-zcptn,
      zptnm(30),   " LIKE ztsd_cam_inf-zptnm,
      zcaus(3),   "LIKE ztsd_cam_inf-zcaus,
      znatr(3),   "LIKE ztsd_cam_inf-znatr,
      zvhqt(7),
      zeffm(8),   "LIKE ztsd_cam_inf-zeffm,
      zefto(8),   "LIKE ztsd_cam_inf-zefto,
*      zvseq_1(3),
      zvend_1(4), " LIKE ztsd_cam_inf-zvend,
      bjr_1(5),
*      zvseq_2(3),
      zvend_2(4), "LIKE ztsd_cam_inf-zvend,
      bjr_2(5),
*      zvseq_3(3),
      zvend_3(4), " LIKE ztsd_cam_inf-zvend,
      bjr_3(5),
*      zvseq_4(3),
      zvend_4(4), " LIKE ztsd_cam_inf-zvend,
      bjr_4(5),
*      zvseq_5(3),
      zvend_5(4), " LIKE ztsd_cam_inf-zvend,
      bjr_5(5),
*      zvseq_6(3),
      zvend_6(4), " LIKE ztsd_cam_inf-zvend,
      bjr_6(5),
      isdt(8),
      istm(6),
      issb(12),
      END OF it_out.

*---// Selection screens
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-t01.
SELECT-OPTIONS: s_datum FOR sy-datum.
SELECTION-SCREEN END OF BLOCK bl1.

INITIALIZATION.
  PERFORM init_data.

START-OF-SELECTION.
  PERFORM read_data.
  PERFORM process_data.
  PERFORM send_data.

*&---------------------------------------------------------------------*
*&      Form  SEND_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_data.
  PERFORM send_by_ftp.
ENDFORM.                    " SEND_RTN
*&---------------------------------------------------------------------*
*&      Form  send_by_FTP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM send_by_ftp.
  PERFORM write_file.
ENDFORM.                    " send_by_FTP
*&---------------------------------------------------------------------*
*&      Form  WRITE_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_file.
  DATA: l_char(8).

  l_char = sy-datum.

  CONCATENATE c_filename l_char '.txt'
         INTO w_filename.

  OPEN DATASET w_filename IN TEXT MODE FOR OUTPUT.
  IF sy-subrc <> 0.
    MESSAGE e000 WITH text-m12.
  ENDIF.

  LOOP AT it_out.
    OPEN DATASET w_filename IN TEXT MODE FOR APPENDING.
    TRANSFER it_out TO w_filename.
  ENDLOOP.

  CLOSE DATASET w_filename.

  IF sy-subrc = 0.
    CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
      EXPORTING
       titel              = 'Information'
        textline1          = text-m11
*     TEXTLINE2          = ' '
       start_column       = 25
       start_row          = 6
            .
  ENDIF.

  CLEAR: it_out, it_out[].

ENDFORM.                    " WRITE_FILE

*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data.
  DATA  l_count TYPE i.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_data
  FROM ztsd_cam_inf
  WHERE erdat IN s_datum.

  DESCRIBE TABLE it_data LINES l_count.
  IF l_count <= 0.
    MESSAGE e001 WITH text-m01.
  ENDIF.
ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  process_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_data.
  DATA: l_sratio LIKE it_data-zvprt,
        l_sratio_int(3).

  LOOP AT it_data.
    MOVE-CORRESPONDING it_data TO it_out.
*    it_out-zvseq_1 = it_data-zvseq.
    it_out-zvend_1 = it_data-zvend.
    l_sratio = ( it_data-zvprt + it_data-zvlrt + it_data-zvsrt ) / 3.
    l_sratio_int = trunc( l_sratio ).
    it_out-bjr_1 = l_sratio_int.
    it_out-bjr_2 = 0.
    it_out-bjr_3 = 0.
    it_out-bjr_4 = 0.
    it_out-bjr_5 = 0.
    it_out-bjr_6 = 0.
    it_out-zvhqt = trunc( it_data-zvhqt ).
*    it_out-name = it_data-zcpnm.
    it_out-hkgb = 'H'.
    it_out-doex = 'A1'.
    it_out-isdt = sy-datum.
    it_out-istm = sy-uzeit.
    it_out-issb = sy-uname.
    APPEND it_out.
    CLEAR: l_sratio, it_out, it_data.
  ENDLOOP.
ENDFORM.                    " process_data
*&---------------------------------------------------------------------*
*&      Form  init_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_data.

ENDFORM.                    " init_data
