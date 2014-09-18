FUNCTION ZIM_CHANGE_DOCUMENT_CCHD.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(UPD_CHNGIND) LIKE  CDPOS-CHNGIND
*"     REFERENCE(N_ZTIV) LIKE  ZTIV STRUCTURE  ZTIV
*"     REFERENCE(O_ZTIV) LIKE  ZTIV STRUCTURE  ZTIV
*"----------------------------------------------------------------------
   data : objecid      like    CDHDR-OBJECTID.

   refresh : IT_CDTXT.
   clear : IT_CDTXT.
   objecid = N_ZTIV+3(10).

   CALL FUNCTION 'ZICCHD_WRITE_DOCUMENT'
       EXPORTING
            OBJECTID           =    objecid
            TCODE              =    SY-TCODE
            UTIME              =    SY-UZEIT
            UDATE              =    SY-DATUM
            USERNAME           =    SY-UNAME
            N_ZTIV             =    N_ZTIV
            O_ZTIV             =    O_ZTIV
            UPD_ZTIV           =    UPD_CHNGIND
*           cdoc_upd_object    =    UPD_CHNGIND
            object_change_indicator = 'U'
            PLANNED_OR_REAL_CHANGES = ''
            NO_CHANGE_POINTERS      = ''
            UPD_ICDTXT_ZICCHD       = ''
      tables
            ICDTXT_ZICCHD           =    IT_CDTXT.

ENDFUNCTION.
