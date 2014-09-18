*
* Andy Choi
* - 2003.04
*
REPORT Z_GLFM_COMPARE LINE-SIZE 120
                      LINE-COUNT 58
                      NO STANDARD PAGE HEADING.


tables: t001, skb1, bsis, bseg, FMIFIIT.


parameters: p_gjahr like bkpf-gjahr,
            p_bukrs like bkpf-bukrs,
            p_belnr like bkpf-belnr,
            p_test(1)  type c default 'X'.

check sy-uname = 'ANDY'.

CALL FUNCTION 'FM_DOCUMENT_DELETE'
     EXPORTING
          u_gjahr              = p_gjahr
          u_bukrs              = p_bukrs
          u_knbelnr            = p_belnr
          u_test               = p_test
     EXCEPTIONS
          no_bukrs_no_firks    = 1
          no_document_number   = 2
          no_document_found    = 3
          document_not_deleted = 4
          document_acnew       = 5
          document_cfnew       = 6
          document_trnew       = 7
          OTHERS               = 8.

* should use commit work...!!
commit work.

write:/ 'FM Document Deleted...'.
