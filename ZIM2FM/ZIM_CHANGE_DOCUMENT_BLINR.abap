FUNCTION ZIM_CHANGE_DOCUMENT_BLINR.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(UPD_CHNGIND) LIKE  CDPOS-CHNGIND
*"     REFERENCE(N_ZTBLINR) LIKE  ZTBLINR STRUCTURE  ZTBLINR
*"     REFERENCE(O_ZTBLINR) LIKE  ZTBLINR STRUCTURE  ZTBLINR
*"----------------------------------------------------------------------
   data : objecid      like    CDHDR-OBJECTID.

   refresh : it_CdtXT.
   clear : it_CdtXT.
   objecid = N_ZTBLINR+3(15).

   CALL FUNCTION 'ZIBLINR_WRITE_DOCUMENT'
       EXPORTING
            OBJECTID           =    objecid
            TCODE              =    SY-TCODE
            UTIME              =    SY-UZEIT
            UDATE              =    SY-DATUM
            USERNAME           =    SY-UNAME
            N_ZTBLINR          =    N_ZTBLINR
            O_ZTBLINR          =    O_ZTBLINR
            UPD_ZTBLINR        =    UPD_CHNGIND
*           cdoc_upd_object    =    UPD_CHNGIND
            object_change_indicator = 'U'
            PLANNED_OR_REAL_CHANGES = ''
            NO_CHANGE_POINTERS      = ''
            UPD_ICDTXT_ZIBLINR      = ''
      tables
            ICDTXT_ZIBLINR     =    it_CdtXT.

ENDFUNCTION.
