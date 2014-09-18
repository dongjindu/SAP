FUNCTION ZIM_CHANGE_DOCUMENT_PMTHD.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(UPD_CHNGIND) LIKE  CDPOS-CHNGIND
*"     REFERENCE(N_ZTPMTHD) LIKE  ZTPMTHD STRUCTURE  ZTPMTHD
*"     REFERENCE(O_ZTPMTHD) LIKE  ZTPMTHD STRUCTURE  ZTPMTHD
*"----------------------------------------------------------------------
   data : objecid      like    CDHDR-OBJECTID.

   refresh : it_CdtXT.
   clear : it_CdtXT.
   objecid = N_ZTPMTHD+3(10).

   CALL FUNCTION 'ZIPMTHD_WRITE_DOCUMENT'
       EXPORTING
            OBJECTID           =    objecid
            TCODE              =    SY-TCODE
            UTIME              =    SY-UZEIT
            UDATE              =    SY-DATUM
            USERNAME           =    SY-UNAME
            N_ZTPMTHD          =    N_ZTPMTHD
            O_ZTPMTHD          =    O_ZTPMTHD
            UPD_ZTPMTHD        =    UPD_CHNGIND
*           cdoc_upd_object    =    UPD_CHNGIND
            object_change_indicator = 'U'
            PLANNED_OR_REAL_CHANGES = ''
            NO_CHANGE_POINTERS      = ''
            UPD_ICDTXT_ZIPMTHD      = ''
      tables
            ICDTXT_ZIPMTHD     =    it_CdtXT.

ENDFUNCTION.
