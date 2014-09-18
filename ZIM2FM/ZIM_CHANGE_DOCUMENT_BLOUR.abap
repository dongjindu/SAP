FUNCTION ZIM_CHANGE_DOCUMENT_BLOUR.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(UPD_CHNGIND) LIKE  CDPOS-CHNGIND
*"     REFERENCE(N_ZTBLOUR) LIKE  ZTBLOUR STRUCTURE  ZTBLOUR
*"     REFERENCE(O_ZTBLOUR) LIKE  ZTBLOUR STRUCTURE  ZTBLOUR
*"----------------------------------------------------------------------
   data : objecid      like    CDHDR-OBJECTID.

   refresh : it_CdtXT.
   clear : it_CdtXT.
   objecid = N_ZTBLOUR+3(15).

   CALL FUNCTION 'ZIBLOUR_WRITE_DOCUMENT'
       EXPORTING
            OBJECTID           =    objecid
            TCODE              =    SY-TCODE
            UTIME              =    SY-UZEIT
            UDATE              =    SY-DATUM
            USERNAME           =    SY-UNAME
            N_ZTBLOUR          =    N_ZTBLOUR
            O_ZTBLOUR          =    O_ZTBLOUR
            UPD_ZTBLOUR        =    UPD_CHNGIND
*           cdoc_upd_object    =    UPD_CHNGIND
            object_change_indicator = 'U'
            PLANNED_OR_REAL_CHANGES = ''
            NO_CHANGE_POINTERS      = ''
            UPD_ICDTXT_ZIBLOUR      = ''
      tables
            ICDTXT_ZIBLOUR     =    it_CdtXT.

ENDFUNCTION.
