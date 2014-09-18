FUNCTION ZIM_CHANGE_DOCUMENT_BLINOU.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(UPD_CHNGIND) LIKE  CDPOS-CHNGIND
*"     REFERENCE(N_ZTBLINOU) LIKE  ZTBLINOU STRUCTURE  ZTBLINOU
*"     REFERENCE(O_ZTBLINOU) LIKE  ZTBLINOU STRUCTURE  ZTBLINOU
*"----------------------------------------------------------------------
   data : objecid      like    CDHDR-OBJECTID.

   refresh : it_CdtXT.
   clear : it_CdtXT.
   objecid = N_ZTBLINOU+3(15).

   CALL FUNCTION 'ZIBLINOU_WRITE_DOCUMENT'
       EXPORTING
            OBJECTID           =    objecid
            TCODE              =    SY-TCODE
            UTIME              =    SY-UZEIT
            UDATE              =    SY-DATUM
            USERNAME           =    SY-UNAME
            N_ZTBLINOU         =    N_ZTBLINOU
            O_ZTBLINOU         =    O_ZTBLINOU
            UPD_ZTBLINOU       =    UPD_CHNGIND
*           cdoc_upd_object    =    UPD_CHNGIND
            object_change_indicator = 'U'
            PLANNED_OR_REAL_CHANGES = ''
            NO_CHANGE_POINTERS      = ''
            UPD_ICDTXT_ZIBLINOU     = ''
      tables
            ICDTXT_ZIBLINOU     =    it_CdtXT.

ENDFUNCTION.
