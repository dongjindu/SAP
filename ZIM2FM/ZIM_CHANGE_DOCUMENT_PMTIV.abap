FUNCTION ZIM_CHANGE_DOCUMENT_PMTIV.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(UPD_CHNGIND) LIKE  CDPOS-CHNGIND
*"     REFERENCE(N_ZTPMTIV) LIKE  ZTPMTIV STRUCTURE  ZTPMTIV
*"     REFERENCE(O_ZTPMTIV) LIKE  ZTPMTIV STRUCTURE  ZTPMTIV
*"----------------------------------------------------------------------
   data : objecid      like    CDHDR-OBJECTID.

   refresh : it_CdtXT.
   clear : it_CdtXT.
   objecid = N_ZTPMTIV+3(15).

   CALL FUNCTION 'ZIPMTIV_WRITE_DOCUMENT'
       EXPORTING
            OBJECTID           =    objecid
            TCODE              =    SY-TCODE
            UTIME              =    SY-UZEIT
            UDATE              =    SY-DATUM
            USERNAME           =    SY-UNAME
            N_ZTPMTIV          =    N_ZTPMTIV
            O_ZTPMTIV          =    O_ZTPMTIV
            UPD_ZTPMTIV        =    UPD_CHNGIND
*           cdoc_upd_object    =    UPD_CHNGIND
            object_change_indicator = 'U'
            PLANNED_OR_REAL_CHANGES = ''
            NO_CHANGE_POINTERS      = ''
            UPD_ICDTXT_ZIPMTIV      = ''
      tables
            ICDTXT_ZIPMTIV     =    it_CdtXT.

ENDFUNCTION.
