FUNCTION ZIM_CHANGE_DOCUMENT_CGIT.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(UPD_CHNGIND) LIKE  CDPOS-CHNGIND
*"     REFERENCE(N_ZTCGIT) LIKE  ZTCGIT STRUCTURE  ZTCGIT
*"     REFERENCE(O_ZTCGIT) LIKE  ZTCGIT STRUCTURE  ZTCGIT
*"----------------------------------------------------------------------
   data : objecid      like    CDHDR-OBJECTID.

   refresh : it_CdtXT.
   clear : it_CdtXT.
   objecid = N_ZTCGIT+3(15).

   CALL FUNCTION 'ZICGIT_WRITE_DOCUMENT'
       EXPORTING
            OBJECTID           =    objecid
            TCODE              =    SY-TCODE
            UTIME              =    SY-UZEIT
            UDATE              =    SY-DATUM
            USERNAME           =    SY-UNAME
            N_ZTCGIT           =    N_ZTCGIT
            O_ZTCGIT           =    O_ZTCGIT
            UPD_ZTCGIT         =    UPD_CHNGIND
*           cdoc_upd_object    =    UPD_CHNGIND
            object_change_indicator = 'U'
            PLANNED_OR_REAL_CHANGES = ''
            NO_CHANGE_POINTERS      = ''
            UPD_ICDTXT_ZICGIT       = ''
      tables
            ICDTXT_ZICGIT     =    it_CdtXT.

ENDFUNCTION.
