FORM CD_CALL_ZTTAXBKIT                     .
   IF   ( UPD_ZTTAXBKIT                      NE SPACE )
     OR ( UPD_ICDTXT_ZTTAXBKIT       NE SPACE )
   .
     CALL FUNCTION 'SWE_REQUESTER_TO_UPDATE'.
     CALL FUNCTION 'ZTTAXBKIT_WRITE_DOCUMENT      ' IN UPDATE TASK
        EXPORTING OBJECTID              = OBJECTID
                  TCODE                 = TCODE
                  UTIME                 = UTIME
                  UDATE                 = UDATE
                  USERNAME              = USERNAME
                  PLANNED_CHANGE_NUMBER = PLANNED_CHANGE_NUMBER
                  OBJECT_CHANGE_INDICATOR = CDOC_UPD_OBJECT
                  PLANNED_OR_REAL_CHANGES = CDOC_PLANNED_OR_REAL
                  NO_CHANGE_POINTERS = CDOC_NO_CHANGE_POINTERS
                  O_ZTTAXBKIT
                      = *ZTTAXBKIT
                  N_ZTTAXBKIT
                      = ZTTAXBKIT
                  UPD_ZTTAXBKIT
                      = UPD_ZTTAXBKIT
                  UPD_ICDTXT_ZTTAXBKIT
                      = UPD_ICDTXT_ZTTAXBKIT
          TABLES  ICDTXT_ZTTAXBKIT
                      = ICDTXT_ZTTAXBKIT
     .
   ENDIF.
   CLEAR PLANNED_CHANGE_NUMBER.
ENDFORM.
