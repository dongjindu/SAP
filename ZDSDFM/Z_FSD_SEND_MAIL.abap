FUNCTION Z_FSD_SEND_MAIL.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(GUBUN) TYPE  C
*"     REFERENCE(KUNNR) TYPE  C
*"  EXPORTING
*"     REFERENCE(RETURN) LIKE  SY-SUBRC
*"  TABLES
*"      ATTAFILE STRUCTURE  ZSSD_MAIL_ATTA OPTIONAL
*"      SMTP_ADDR STRUCTURE  ZSSD_MAIL_ADDR OPTIONAL
*"----------------------------------------------------------------------

g_user-sapname = sy-uname.

call function 'SO_USER_READ_API1'
exporting
   user                            = g_user
*    PREPARE_FOR_FOLDER_ACCESS       = ' '
importing
   user_data                       = g_user_data
*  EXCEPTIONS
*    USER_NOT_EXIST                  = 1
*    PARAMETER_ERROR                 = 2
*    X_ERROR                         = 3
*    OTHERS                          = 4
          .
if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
endif.

fold_type = g_user_data-outboxfol+0(3).
fold_yr = g_user_data-outboxfol+3(2).
fold_number =  g_user_data-outboxfol+5(12).

clear g_files.

refresh : g_objcnt,
          g_objhead,
          g_objpara,
          g_objparb,
          g_receipients,
          g_attachments,
          g_references,
          g_files.
***** 031204 junho
clear : g_document,
        g_header.
*****

method1 = 'SAVE'.

g_document-foltp  = fold_type.
g_document-folyr   = fold_yr.
g_document-folno   = fold_number.
g_document-objtp   = g_user_data-object_typ.
*g_document-OBJYR   = '27'.
*g_document-OBJNO   = '000000002365'.
*g_document-OBJNAM = 'MESSAGE'.
g_document-objdes   = 'test'.
g_document-folrg   = 'O'.
*g_document-okcode   = 'CHNG'.
g_document-objlen = '0'.
g_document-file_ext = 'TXT'.

case gubun.
when '1'.
  g_header-objdes =  'Approved Credit Memo'.
when '2'.
  g_header-objdes =  'Notification of Remittance'.
when '3'.
  g_header-objdes =  'Warranty Claim Invoice'.
when '4'.
  g_header-objdes =  'Preinform of Remittance'.
endcase.

g_header-file_ext = 'TXT'.

call function 'SO_DOCUMENT_REPOSITORY_MANAGER'
  exporting
    method             = method1
   office_user        = sy-uname
   ref_document       = g_ref_document
   new_parent         =  g_new_parent
importing
   authority          =  g_authority
tables
   objcont            = g_objcnt
   objhead            = g_objhead
   objpara            = g_objpara
   objparb            = g_objparb
   recipients         = g_receipients
   attachments        = g_attachments
   references         = g_references
   files              = g_files
  changing
    document           = g_document
   header_data        = g_header
*   FOLMEM_DATA        =
*   RECEIVE_DATA       =
          .

* File from the pc to send...
method1 = 'ATTCREATEFROMPC'.

loop at attafile.
  g_files-text = attafile-attafile.
  collect g_files.
endloop.

call function 'SO_DOCUMENT_REPOSITORY_MANAGER'
  exporting
    method             = method1
   office_user        = g_owner
   ref_document       = g_ref_document
   new_parent         =  g_new_parent
importing
   authority          =  g_authority
tables
   objcont            = g_objcnt
   objhead            = g_objhead
   objpara            = g_objpara
   objparb            = g_objparb
   recipients         = g_receipients
   attachments        = g_attachments
   references         = g_references
   files              = g_files
  changing
    document           = g_document
   header_data        = g_header
          .

method1 = 'SEND'.

loop at smtp_addr.
  g_receipients-recextnam = smtp_addr-smtp_addr.
  g_receipients-recesc = 'U'.
  g_receipients-sndex = 'X'.
  collect  g_receipients.
endloop.
if not kunnr is initial.
  refresh mail_addr.
  CALL FUNCTION 'Z_FSD_MAIL_ADDR'
    EXPORTING
      KUNNR                = KUNNR
    TABLES
      MAIL_ADDR            = MAIL_ADDR
    EXCEPTIONS
      NOT_FOUND_KNA1       = 1
      NOT_FOUND_KNVK       = 2
      OTHERS               = 3.
* fix by 100565
  clear: g_receipients,g_receipients[].
* end fix.
  loop at mail_addr.
    g_receipients-recextnam = mail_addr-smtp_addr.
    g_receipients-recesc = 'U'.
    g_receipients-sndex = 'X'.
    collect  g_receipients.
  endloop.
endif.

call function 'SO_DOCUMENT_REPOSITORY_MANAGER'
  exporting
    method             = method1
   office_user        = g_owner
   ref_document       = g_ref_document
   new_parent         =  g_new_parent
importing
   authority          =  g_authority
tables
   objcont            = g_objcnt
   objhead            = g_objhead
   objpara            = g_objpara
   objparb            = g_objparb
   recipients         = g_receipients
   attachments        = g_attachments
   references         = g_references
   files              = g_files
  changing
    document           = g_document
   header_data        = g_header.

if g_document-okcode = 'ESC'.
  return = 9.
elseif g_document-okcode = 'CHNG'.
  return = 0.
endif.

ENDFUNCTION.
