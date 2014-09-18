FUNCTION zh_hr_if_pa_photo.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_BEGDA) TYPE  BEGDA DEFAULT '18000101'
*"     VALUE(IV_ENDDA) TYPE  ENDDA DEFAULT '99991231'
*"  TABLES
*"      T_PERSNO STRUCTURE  ZGHRSS0006 OPTIONAL
*"      T_PHOTOS STRUCTURE  ZGHRSS0005 OPTIONAL
*"----------------------------------------------------------------------

  DATA: l_exist         TYPE c.
  DATA: l_doc_type      TYPE toadd-doc_type.
  DATA: ls_connect_info TYPE toav0.
  DATA: it_bin_data     TYPE TABLE OF tbl1024 WITH HEADER LINE.

  CLEAR: t_photos[].

  LOOP AT t_persno.

    CLEAR: l_exist, ls_connect_info.

    CALL FUNCTION 'HR_IMAGE_EXISTS'
      EXPORTING
        p_pernr               = t_persno-pernr
        p_tclas               = 'A'
        p_begda               = iv_begda
        p_endda               = iv_endda
      IMPORTING
        p_exists              = l_exist
        p_connect_info        = ls_connect_info
      EXCEPTIONS
        error_connectiontable = 1
        OTHERS                = 2.

    CHECK sy-subrc = 0 AND l_exist = 1.

    CHECK t_persno-ar_date <> ls_connect_info-ar_date.

    CLEAR: it_bin_data[].

    l_doc_type = ls_connect_info-reserve.

    CALL FUNCTION 'ARCHIVOBJECT_GET_TABLE'
      EXPORTING
        archiv_id                = ls_connect_info-archiv_id
        document_type            = l_doc_type
        archiv_doc_id            = ls_connect_info-arc_doc_id
      TABLES
        binarchivobject          = it_bin_data[]
      EXCEPTIONS
        error_archiv             = 1
        error_communicationtable = 2
        error_kernel             = 3
        OTHERS                   = 4.

    t_photos-pernr   = ls_connect_info-object_id+0(8).
    t_photos-ar_date = ls_connect_info-ar_date.
    t_photos-reserve = ls_connect_info-reserve.

    LOOP AT it_bin_data.
      t_photos-photo = it_bin_data-line.
      APPEND t_photos.
    ENDLOOP.
  ENDLOOP.
ENDFUNCTION.
