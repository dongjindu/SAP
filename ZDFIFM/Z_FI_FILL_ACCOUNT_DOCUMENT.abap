FUNCTION z_fi_fill_account_document.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(BUKRS) TYPE  BUKRS
*"     VALUE(GJAHR) TYPE  GJAHR
*"     VALUE(BELNR) TYPE  BELNR_D OPTIONAL
*"     VALUE(REF_BERNR) TYPE  BELNR_D OPTIONAL
*"     VALUE(P_PARKED) TYPE  CHAR1 OPTIONAL
*"     VALUE(P_POSTED) TYPE  CHAR1 OPTIONAL
*"     VALUE(P_OWN) TYPE  CHAR1 OPTIONAL
*"     VALUE(R_REPOSTING) TYPE  CHAR1 OPTIONAL
*"  TABLES
*"      ZSFIIVPRNHD STRUCTURE  ZSFIIVPRNHD
*"      ZSFIIVPRNDT STRUCTURE  ZSFIIVPRNDT
*"      ZSFIIVPRNMEMO STRUCTURE  ZSFIIVPRNMEMO
*"      ZSFIIVPRNETC STRUCTURE  ZSFIIVPRNETC
*"  EXCEPTIONS
*"      NO_DOC_FOUND
*"      INVALID_COMAPNY
*"----------------------------------------------------------------------
* Modification Logs
* Date       Developer  Request ID  Description
* 06/12/2011 VALERIAN   UD1K951972  Add Group Key into Acct.Doc. Print.
* 10/04/2011 VALERIAN   UD1K953079  Fix fund center assignment in park
*                                   documents.
* 05/14/2012 VALERIAN   UD1K954591  Put additional information in print
*                                   document memo for G/L Acct.'151020'
* 05/23/2013 I.G Moon   UD1K957161  Check I/V party differs from purchase order vendor
*-----------------------------------------------------------------------

  DATA: wa_l_name1(40), wa_l_name2(40), wa_l_company_name(80).
  __cls it_bkpf.

  SELECT SINGLE * FROM t001
         WHERE  bukrs = bukrs.

  IF sy-subrc NE 0.
    RAISE invalid_comapny.
  ENDIF.

  SELECT SINGLE name1 name2 INTO (wa_l_name1, wa_l_name2)
    FROM adrc
    WHERE addrnumber = t001-adrnr AND
          date_from <= sy-datum.

  wa_l_company_name =  wa_l_name1.
  DATA: l_pos TYPE i,
        l_idx LIKE sy-tabix.

  l_pos = strlen( wa_l_company_name ) + 1.
  wa_l_company_name+l_pos = wa_l_name2.

  IF ref_bernr IS INITIAL.
    CLEAR bkpf.
    SELECT SINGLE * FROM bkpf WHERE BUKRS eq BUKRS
                                and belnr EQ belnr
                                AND gjahr EQ gjahr.
    IF sy-subrc NE 0.
      RAISE no_doc_found.
    ENDIF.
    PERFORM read_bkpf USING p_parked p_posted p_own .
    READ TABLE it_bkpf INDEX 1  .
    IF sy-subrc NE 0.
      EXIT.
    ENDIF.
    PERFORM read_bseg.
  ELSE.
    PERFORM read_rseg USING p_parked.
  ENDIF.
  __cls : zsfiivprnhd, zsfiivprndt, zsfiivprnmemo.
  IF NOT r_reposting IS INITIAL.
    PERFORM write_internal_documents TABLES zsfiivprnhd
                                            zsfiivprndt
                                            zsfiivprnmemo.

  ELSE.
    PERFORM write_documents TABLES zsfiivprnhd
                                   zsfiivprndt
                                   zsfiivprnmemo
                             using REF_BERNR       .

  ENDIF.

  perform write_etc TABLES zsfiivprnmemo
                           zsfiivprnetc.

* Check I/V party differs from purchase order vendor

  data : a_ebeln type ebeln,l_lifnr type ELIFN.

  select single * from rbkp where belnr eq bkpf-awkey(10)
                       and GJAHR eq bkpf-awkey+10(4).

  if sy-subrc eq 0.
    select ebeln into a_ebeln from rseg where BELNR eq bkpf-awkey(10)
                         and GJAHR eq GJAHR.
        select single lifnr into l_lifnr from ekko where ebeln eq a_ebeln.
        if sy-subrc eq 0 and l_lifnr ne rbkp-lifnr.
            WRITE 'Note : Invoicing party differs from purchase order vendor' TO zsfiivprnetc-text77.
            APPEND zsfiivprnetc.
            exit.
        endif.
    endselect.
  endif.

ENDFUNCTION.
