FORM CD_CALL_ZICIVIT                       .
   IF   ( UPD_ZTCIVIT                        NE SPACE )
     OR ( UPD_ICDTXT_ZICIVIT         NE SPACE )
   .
     CALL FUNCTION 'ZICIVIT_WRITE_DOCUMENT        ' IN UPDATE TASK
        EXPORTING OBJECTID              = OBJECTID
                  TCODE                 = TCODE
                  UTIME                 = UTIME
                  UDATE                 = UDATE
                  USERNAME              = USERNAME
                  PLANNED_CHANGE_NUMBER = PLANNED_CHANGE_NUMBER
                  OBJECT_CHANGE_INDICATOR = CDOC_UPD_OBJECT
                  PLANNED_OR_REAL_CHANGES = CDOC_PLANNED_OR_REAL
                  NO_CHANGE_POINTERS = CDOC_NO_CHANGE_POINTERS
                  O_ZTCIVIT
                      = *ZTCIVIT
                  N_ZTCIVIT
                      = ZTCIVIT
                  UPD_ZTCIVIT
                      = UPD_ZTCIVIT
                  UPD_ICDTXT_ZICIVIT
                      = UPD_ICDTXT_ZICIVIT
          TABLES  ICDTXT_ZICIVIT
                      = ICDTXT_ZICIVIT
     .
   ENDIF.
   CLEAR PLANNED_CHANGE_NUMBER.
ENDFORM.
