FUNCTION z_fmm_get_asn_iv.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      IT_ASN_IV STRUCTURE  ZTMM_ASN_IV
*"----------------------------------------------------------------------
  DATA : total LIKE ztca_if_log-total    ,
         tcode LIKE sy-tcode VALUE 'ME21',
         i_ztca_if_log LIKE ztca_if_log  .

  DATA : serialno LIKE ztca_if_log-zslno .

  DESCRIBE TABLE it_asn_iv LINES total.
  CHECK total <> 0.

  SELECT SINGLE * FROM tstc WHERE tcode = tcode.
  IF sy-subrc NE 0.
    MESSAGE e010 WITH tcode.
  ENDIF.

*[ 0 ] Get Serial Number
  CALL FUNCTION 'NUMBER_GET_NEXT'
       EXPORTING
            nr_range_nr = '01'
            object      = 'ZCCTR'
       IMPORTING
            number      = serialno.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*[ 1 ] Data Insert to table ZTMM_ASN_IV.
  LOOP AT it_asn_iv.
    it_asn_iv-zuser = sy-uname.
    it_asn_iv-zsdat = sy-datum.
    it_asn_iv-zstim = sy-uzeit.
    it_asn_iv-zedat = sy-datum.
    it_asn_iv-zetim = sy-uzeit.
    UPDATE ztmm_asn_iv FROM it_asn_iv.
    IF sy-subrc <> 0.
      INSERT ztmm_asn_iv FROM it_asn_iv.
    ENDIF.
    it_asn_iv-zzret    = 'S'.
    it_asn_iv-zslno    = serialno.
    MODIFY it_asn_iv.
  ENDLOOP.

*[ 2 ] Job open.
  DATA : jobname LIKE tbtcjob-jobname VALUE 'C/CTR ASN_IV -> SAP' ,
         jobcnt  LIKE tbtcjob-jobcount                            .
  PERFORM job_open USING jobname
                   CHANGING jobcnt.

  UPDATE ztmm_asn_iv SET: zslno        = serialno ,
                          jobcount     = jobcnt
                WHERE po_no       = it_asn_iv-po_no
                  AND doc_type    = it_asn_iv-doc_type
                  AND depen_no    = it_asn_iv-depen_no
                  AND item_no     = it_asn_iv-item_no
                  AND depen_place = it_asn_iv-depen_place.

*[ 3 ] EMMGM08 Create Inbound Delivery From ASN & IV.
  DATA : prgid LIKE sy-repid VALUE 'ZEMMGM08E_INBOUND_ASN_IV'.
  SUBMIT (prgid) WITH s_zslno = serialno
                 WITH r1 = 'X'
                 USER sy-uname
                 VIA JOB jobname
                 NUMBER  jobcnt
                 AND RETURN.
*[ 4 ] Job Close.
  PERFORM job_close USING jobname
                          jobcnt.
ENDFUNCTION.
