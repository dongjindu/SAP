
*&---------------------------------------------------------------------*
*& Report  ZCAUBI000
*&
*&---------------------------------------------------------------------*
*& T-Code : ZCAUGBI_INIT - For setting start transaction of each user
*&          ZCAUGBI
*&---------------------------------------------------------------------*

report  zcaubi002 message-id sy.

include ole2incl.
tables : zcaubit0000.
data: gbi_id  type string,
      encode  type string,
      lv_pass type string,
      lv_exist type c,
      lv_url type string.

data : ls_usr like zcaubit0000.
data obj  type ref to cl_http_utility.

data : ip(20) type c.

data : list like table of uinfo  with header line.

*&---------------------------------------------------------------------*
start-of-selection.
*&---------------------------------------------------------------------*

  create object : obj.
*
  clear ls_usr.
  select single * into ls_usr
     from zcaubit0000
    where zename = sy-uname.

  if sy-subrc = 0 and ls_usr-zgname is not initial.
  else.
    if sy-tcode = 'ZCAUGBI_INIT'.
    else.
      message i002 with 'Your GBI ID is not registered in ERP !!'.
    endif.
    exit.
  endif.
*----------------------------------------------------------------------
* Get IP address
*----------------------------------------------------------------------
  call function 'TH_USER_LIST'
    tables
      list = list.

  read table list with key bname = sy-uname.

  call function 'GWY_IPADR2STRING'
    exporting
      ipadr  = list-hostadr
    importing
      string = ip.
*----------------------------------------------------------------------

*----------------------------------------------------------------------
* Encript ID|IP Address
*----------------------------------------------------------------------
  concatenate ls_usr-zgname ip into gbi_id
                               separated by '|'.
*  gbi_id = ls_usr-zgname.
  encode = obj->if_http_utility~encode_base64( gbi_id ).
*----------------------------------------------------------------------

*----------------------------------------------------------------------
* Make link
*----------------------------------------------------------------------

  if ls_usr-zflang = ''.
    move : 'EN' to ls_usr-zflang.
  endif.

  translate ls_usr-zflang to upper case.

  concatenate 'https://gbi.hmc.co.kr/jsp/autologin.jsp?userId='
              encode '&locale=' ls_usr-zflang into lv_url.
*----------------------------------------------------------------------
  if sy-tcode = 'ZCAUGBI_INIT'.

    if sy-datum = ls_usr-zlogdt and ls_usr-zcount >= ls_usr-zincnt.
      exit.
    endif.

    if ls_usr-zpath is not initial.
      lv_pass = ls_usr-zpath.
    else.
      lv_pass = 'C:\Program Files\Internet Explorer\iexplore.exe'.
    endif.

    clear lv_exist.
    call method cl_gui_frontend_services=>file_exist
      exporting
        file   = lv_pass
      receiving
        result = lv_exist
      exceptions
        others = 1.

    if sy-subrc = 0 and lv_exist = 'X'.
    else.
      exit.
    endif.

    call method cl_gui_frontend_services=>execute
      exporting
        application            = lv_pass
        parameter              = lv_url
        minimized              = 'X'
        operation              = ' '
      exceptions
        file_extension_unknown = 1
        file_not_found         = 1
        path_not_found         = 1
        error_execute_failed   = 1
        error_no_gui           = 2
        others                 = 3.

    add 1 to ls_usr-zcount .
    update zcaubit0000 set zlogdt = sy-datum
                           zlogtm = sy-uzeit
                           zcount = ls_usr-zcount
            where zename = sy-uname.

  else.   "OTHER T-CODE
    call function 'SPI_CALL_BROWSER'
      exporting
        url = lv_url.
  endif.

  exit.
