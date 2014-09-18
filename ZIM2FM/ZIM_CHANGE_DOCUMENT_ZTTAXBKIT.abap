FUNCTION ZIM_CHANGE_DOCUMENT_ZTTAXBKIT.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(UPD_CHNGIND) LIKE  CDPOS-CHNGIND
*"     VALUE(N_ZTTAXBKIT) LIKE  ZTTAXBKIT STRUCTURE  ZTTAXBKIT
*"     VALUE(O_ZTTAXBKIT) LIKE  ZTTAXBKIT STRUCTURE  ZTTAXBKIT
*"----------------------------------------------------------------------
   DATA : OBJECID   LIKE   CDHDR-OBJECTID.

   REFRESH : IT_CDTXT.
   CLEAR : IT_CDTXT.
   OBJECID = N_ZTTAXBKIT+3(15).

   CALL FUNCTION 'ZTTAXBKIT_WRITE_DOCUMENT'
       EXPORTING
            OBJECTID           =    OBJECID
            TCODE              =    SY-TCODE
            UTIME              =    SY-UZEIT
            UDATE              =    SY-DATUM
            USERNAME           =    SY-UNAME
            N_ZTTAXBKIT        =    N_ZTTAXBKIT
            O_ZTTAXBKIT        =    O_ZTTAXBKIT
            UPD_ZTTAXBKIT      =    UPD_CHNGIND
*           cdoc_upd_object    =    UPD_CHNGIND
            object_change_indicator = 'U'
            PLANNED_OR_REAL_CHANGES = ''
            NO_CHANGE_POINTERS      = ''
            UPD_ICDTXT_ZTTAXBKIT    = ''
      tables
            ICDTXT_ZTTAXBKIT     =    IT_CDTXT.

ENDFUNCTION.
