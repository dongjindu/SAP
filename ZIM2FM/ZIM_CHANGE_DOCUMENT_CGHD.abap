FUNCTION ZIM_CHANGE_DOCUMENT_CGHD.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(UPD_CHNGIND) LIKE  CDPOS-CHNGIND
*"     REFERENCE(N_ZTCGHD) LIKE  ZTCGHD STRUCTURE  ZTCGHD
*"     REFERENCE(O_ZTCGHD) LIKE  ZTCGHD STRUCTURE  ZTCGHD
*"----------------------------------------------------------------------
   data : objecid      like    CDHDR-OBJECTID.

   refresh : it_CdtXT.
   clear : it_CdtXT.
   objecid = N_ZTCGHD+3(10).

   CALL FUNCTION 'ZICGHD_WRITE_DOCUMENT'
       EXPORTING
            OBJECTID           =    objecid
            TCODE              =    SY-TCODE
            UTIME              =    SY-UZEIT
            UDATE              =    SY-DATUM
            USERNAME           =    SY-UNAME
            N_ZTCGHD           =    N_ZTCGHD
            O_ZTCGHD           =    O_ZTCGHD
            UPD_ZTCGHD         =    UPD_CHNGIND
*           cdoc_upd_object    =    UPD_CHNGIND
            object_change_indicator = 'U'
            PLANNED_OR_REAL_CHANGES = ''
            NO_CHANGE_POINTERS      = ''
            UPD_ICDTXT_ZICGHD      = ''
      tables
            ICDTXT_ZICGHD     =    it_CdtXT.

ENDFUNCTION.
