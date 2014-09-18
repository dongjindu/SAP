FUNCTION z_fca_eai_interface_log.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(I_ZTCA_IF_LOG) LIKE  ZTCA_IF_LOG STRUCTURE
*"        ZTCA_IF_LOG
*"  EXPORTING
*"     REFERENCE(E_ZTCA_IF_LOG) LIKE  ZTCA_IF_LOG STRUCTURE
*"        ZTCA_IF_LOG
*"  EXCEPTIONS
*"      UPDATE_FAILED
*"      NUMBER_RANGE_ERROR
*"      TCODE_DOES_NOT_EXIST
*"----------------------------------------------------------------------

  SELECT SINGLE * FROM tstc WHERE tcode = i_ztca_if_log-tcode.
  IF sy-subrc NE 0.
    RAISE tcode_does_not_exist.
  ENDIF.

  MOVE: i_ztca_if_log TO e_ztca_if_log.

  PERFORM get_document_number.

  MOVE: w_ifdoc  TO e_ztca_if_log-ifdoc.

  IF e_ztca_if_log-erdat IS INITIAL.
    MOVE: sy-uname TO e_ztca_if_log-ernam,
          sy-datum TO e_ztca_if_log-erdat,
          sy-uzeit TO e_ztca_if_log-erzet,
          sy-uname TO e_ztca_if_log-aenam,
          sy-datum TO e_ztca_if_log-aedat,
          sy-uzeit TO e_ztca_if_log-aezet.
  ENDIF.

  IF e_ztca_if_log-total EQ e_ztca_if_log-zsucc.
    MOVE: 'S' TO e_ztca_if_log-sflag.
  ELSE.
    MOVE: 'F' TO e_ztca_if_log-sflag.
  ENDIF.

  INSERT ztca_if_log FROM e_ztca_if_log.
  IF sy-subrc NE 0.
    RAISE update_failed.
  ENDIF.
ENDFUNCTION.
