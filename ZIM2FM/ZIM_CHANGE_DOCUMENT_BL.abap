FUNCTION ZIM_CHANGE_DOCUMENT_BL.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(UPD_CHNGIND) LIKE  CDPOS-CHNGIND
*"     REFERENCE(N_ZTBL) LIKE  ZTBL STRUCTURE  ZTBL
*"     REFERENCE(O_ZTBL) LIKE  ZTBL STRUCTURE  ZTBL
*"----------------------------------------------------------------------
   data : objecid      like    CDHDR-OBJECTID.

   refresh : it_CdtXT.
   clear : it_CdtXT.
   objecid = N_ZTBL+3(10).

   CALL FUNCTION 'ZIBLDOC_WRITE_DOCUMENT'
       EXPORTING
            OBJECTID           =    objecid
            TCODE              =    SY-TCODE
            UTIME              =    SY-UZEIT
            UDATE              =    SY-DATUM
            USERNAME           =    SY-UNAME
            N_ZTBL             =    N_ZTBL
            O_ZTBL             =    O_ZTBL
            UPD_ZTBL           =    UPD_CHNGIND
*           cdoc_upd_object    =    UPD_CHNGIND
            object_change_indicator = 'U'
            PLANNED_OR_REAL_CHANGES = ''
            NO_CHANGE_POINTERS      = ''
            UPD_ICDTXT_ZIBLDOC      = ''
      tables
            ICDTXT_ZIBLDOC     =    it_CdtXT.

ENDFUNCTION.
