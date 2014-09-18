FUNCTION ZIM_CHANGE_DOCUMENT_ZTIDS.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(UPD_CHNGIND) LIKE  CDPOS-CHNGIND
*"     REFERENCE(N_ZTIDS) LIKE  ZTIDS STRUCTURE  ZTIDS
*"     REFERENCE(O_ZTIDS) LIKE  ZTIDS STRUCTURE  ZTIDS
*"----------------------------------------------------------------------
   data : objecid      like    CDHDR-OBJECTID.

   refresh : it_CdtXT.
   clear : it_CdtXT.
   objecid = N_ZTIDS+3(15).

   CALL FUNCTION 'ZTIDS_WRITE_DOCUMENT'
       EXPORTING
            OBJECTID           =    objecid
            TCODE              =    SY-TCODE
            UTIME              =    SY-UZEIT
            UDATE              =    SY-DATUM
            USERNAME           =    SY-UNAME
            N_ZTIDS            =    N_ZTIDS
            O_ZTIDS            =    O_ZTIDS
            UPD_ZTIDS          =    UPD_CHNGIND
*           cdoc_upd_object    =    UPD_CHNGIND
            object_change_indicator = 'U'
            PLANNED_OR_REAL_CHANGES = ''
            NO_CHANGE_POINTERS      = ''
            UPD_ICDTXT_ZTIDS        = ''
      tables
            ICDTXT_ZTIDS            =    it_CdtXT.

ENDFUNCTION.
