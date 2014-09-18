************************************************************************
* Program Name      : ZMMR207_MTHLY_GR
* Author            : Furong Wang
* Creation Date     : 01/04/2006
* Specifications By :
* Development Request No :
* Addl Documentation:
* Description       : Monthly Good Receiving
* Modification Logs
* Date       Developer    RequestNo    Description
* 04/30/2007 Manju        UD1K940443   Maintain Vendor list  table
*                                      which will only be transmitted
*                                      to GQRS system
************************************************************************
REPORT zmmr207_mthly_gr NO STANDARD PAGE HEADING LINE-SIZE 255
                       MESSAGE-ID zmpp.

*---// Constants
CONSTANTS: c_filename LIKE rlgrap-filename VALUE
                      '/usr/sap/EDI_SAP/claim_pur_'.

*---// For FTP file creation
DATA: w_filename LIKE rlgrap-filename.
DATA: W_CHECK_PERIOD(1).

DATA: BEGIN OF it_data OCCURS 0,
      lifnr LIKE mseg-lifnr,
      matnr LIKE mara-matnr,
      maktx LIKE makt-maktx,
      erfmg LIKE mseg-erfmg,
      shkzg LIKE mseg-shkzg,
      END OF it_data.

DATA: BEGIN OF it_out OCCURS 0,
      hkgb(01),   "'H'
      doex(2),    "'A'
      yymm(6),
      vndr(4),
      part(18),
      pnam(20),
      ipgo(9),
      isdt(8),
      istm(6),
      issb(12),
      END OF it_out.
data : begin of it_vendlist occurs 0,
        lifnr like ZTMM_GQRS_VENDTB-lifnr,
        flag  like ZTMM_GQRS_VENDTB-ZTRANSMIT_FLAG,
       end of it_vendlist.

*---// Selection screens
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-t01.
PARAMETERS: p_yymm TYPE char6.
SELECTION-SCREEN END OF BLOCK bl1.

INITIALIZATION.
  PERFORM set_data.

START-OF-SELECTION.
  PERFORM check_data.
  CHECK W_CHECK_PERIOD = ' '.
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
  DATA: l_char(6).

  l_char = p_yymm.

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
  DATA: l_sdate TYPE d,
        l_edate TYPE d.

  CONCATENATE p_yymm '01' INTO l_sdate.

  CALL FUNCTION 'HR_JP_MONTH_BEGIN_END_DATE'
    EXPORTING
      iv_date                   = l_sdate
   IMPORTING
*   EV_MONTH_BEGIN_DATE       =
     ev_month_end_date         = l_edate.

  SELECT a~lifnr a~matnr d~maktx a~erfmg a~shkzg
    INTO TABLE it_data
    FROM mseg AS a
    INNER JOIN mkpf AS b
    ON a~mblnr = b~mblnr
    AND a~mjahr = b~mjahr
    INNER JOIN mara AS c
    ON c~matnr = a~matnr
    INNER JOIN makt AS d
    ON c~matnr = d~matnr
    WHERE b~blart = 'WE'
      AND c~mtart = 'ROH'
      AND b~budat BETWEEN l_sdate AND l_edate
      AND a~bwart IN (101, 102, 122, 123).


  DESCRIBE TABLE it_data LINES l_count.
  IF l_count <= 0.
    MESSAGE e001 WITH text-m01.
  ENDIF.
ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  SET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_data.
  data: l_year(4),
        l_month(2).
  l_year = sy-datum+0(4).
  l_month = sy-datum+4(2).
  if l_month = '01'.
    l_year = l_year - 1.
    l_month = '12'.
    concatenate l_year l_month into p_yymm.
  else.
    p_yymm = sy-datum+00(06) - 1.
  endif.
ENDFORM.                    " SET_DATA
*&---------------------------------------------------------------------*
*&      Form  process_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_data.
  DATA: l_lifnr LIKE it_data-lifnr,
        l_matnr LIKE it_data-matnr,
        l_qty LIKE it_data-erfmg,
        l_qty_char(9),
        L_index like sy-index.

  SORT it_data BY lifnr matnr.
  it_out-hkgb = 'H'.
  it_out-doex = 'A1'.
  it_out-yymm = p_yymm.
  it_out-isdt = sy-datum.
  it_out-istm = sy-uzeit.
  it_out-issb = sy-uname.

* Begin of changes -   UD1K940443
* Only Transmit Vendor data which are maintained in
* ZTMM_GQRS_VENDTB TABLE

  select lifnr ZTRANSMIT_FLAG into table it_vendlist
    from ZTMM_GQRS_VENDTB.

*    where ZTRANSMIT_FLAG eq 'I'.  "Include

  sort  it_vendlist by lifnr.
  LOOP AT it_data.
    L_index = sy-tabix.

     read table it_vendlist with key lifnr = it_data-lifnr.
    if sy-subrc  eq 0.
    case it_vendlist-flag.
* Include
      when 'I'.
          continue.
* Exclude
      when 'E'.
          delete  it_data index L_index.
          continue.
      when others.
* for Others if first character is not A then Delete
          if it_data-lifnr+0(1) ne 'A'.
           delete  it_data index L_index.
          endif.
    endcase.
    endif.
* for Others if first character is not A then Delete
          if it_data-lifnr+0(1) ne 'A'.
           delete  it_data index L_index.
          endif.

  endloop.
* ENd of changes -   UD1K940443


  LOOP AT it_data.

    AT NEW lifnr.
      it_out-vndr = it_data-lifnr.
    ENDAT.
    AT NEW matnr.
      CLEAR l_qty.
      it_out-part = it_data-matnr.
    ENDAT.
    IF it_data-shkzg ='S'.
      l_qty = l_qty + it_data-erfmg.
    ELSE.
      l_qty = l_qty - it_data-erfmg.
    ENDIF.
    it_out-pnam = it_data-maktx.
    AT END OF matnr.
      IF l_qty > 0.
        l_qty_char = trunc( l_qty ).
        it_out-ipgo = l_qty_char.
        APPEND it_out.
      ENDIF.
      CLEAR: it_out-part, it_out-pnam,
             it_out-ipgo, it_data, l_qty_char.
    ENDAT.
  ENDLOOP.
ENDFORM.                    " process_data
*&---------------------------------------------------------------------*
*&      Form  check_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_data.
  DATA: l_year LIKE t001b-frye1,
        l_period LIKE t001b-frpe1,
        l_period_char(2),
        l_curr_yymm(6).

  SELECT SINGLE frye1 frpe1 INTO (l_year, l_period)
   FROM t001b
   WHERE bukrs = 'H201'
     AND mkoar = '+'.
  l_period_char = l_period+1(2).

  CONCATENATE l_year l_period_char INTO l_curr_yymm.

  IF p_yymm >= l_curr_yymm.
    W_CHECK_PERIOD = 'X'.
    MESSAGE i001 WITH text-m02.
  ENDIF.

ENDFORM.                    " check_data
