*$*$--------------------------------------------------------------$*$*
*$ Correction Inst.         0120061532 0000458360                     $*
*$------------------------------------------------------------------$*
*$ Valid for       :                                                  $*
*$ Software Component   SAP_APPL   SAP Application                    $*
*$  Release 46A          To SAPKH46A34                                $*
*$  Release 46B          All Support Package Levels                   $*
*$  Release 46C          All Support Package Levels                   $*
*$  Release 470          All Support Package Levels                   $*
*$------------------------------------------------------------------$*
*$ Changes/Objects Not Contained in Standard SAP System               $*
*$*$--------------------------------------------------------------$*$*
*&-------------------------------------------------------------------*
*& Object          REPS ZCODEUSAGE
*& Object Header   PROG ZCODEUSAGE
*&-------------------------------------------------------------------*
*& REPORT ZCODEUSAGE
*&-------------------------------------------------------------------*
*>>>> START OF INSERTION <<<<
*&---------------------------------------------------------------------*
*& Report  ZCODEUSAGE                                             *
*& 20.09.2002                                                     *
*&---------------------------------------------------------------------*
*& This report deletes the usage indicator of codes, without checking  *
*& the usage.                                                     *
*&---------------------------------------------------------------------*

*---------------------------------------------------------------------*
* Request No. : UD1K903420  SLLEE  [Notes] 556218 : QM - 10/29/2003   *
*---------------------------------------------------------------------*

 REPORT ZCODEUSAGE    .

 TABLES QPCD.

 PARAMETERS: P_KATART LIKE QPCD-KATALOGART OBLIGATORY.
 SELECT-OPTIONS: P_CODEGR FOR QPCD-CODEGRUPPE,
                 P_CODE   FOR QPCD-CODE,
                 P_ERSTEL FOR QPCD-ERSTELLER,
                 P_EDATUM FOR QPCD-E_DATUM.
 PARAMETERS: P_TEST TYPE C DEFAULT 'X'.

 DATA: L_QPCD_TAB TYPE TABLE OF QPCD,
       L_QPCD TYPE QPCD,
       L_LINES    TYPE I,
       L_SUBRC    TYPE SY-SUBRC.

 START-OF-SELECTION.

* Preselection for protocol
   SELECT MANDT KATALOGART CODEGRUPPE CODE VERSION
     INTO TABLE L_QPCD_TAB FROM QPCD
        WHERE KATALOGART = P_KATART
        AND   CODEGRUPPE IN P_CODEGR
        AND   CODE   IN P_CODE
        AND   ERSTELLER  IN P_ERSTEL
        AND   E_DATUM    IN P_EDATUM
        AND   VERWENDUNG EQ 'X'.
   DESCRIBE TABLE L_QPCD_TAB LINES L_LINES.

   IF P_TEST IS INITIAL.
*   Make changes on the database

     UPDATE QPCD SET VERWENDUNG = SPACE
      WHERE KATALOGART = P_KATART
      AND   CODEGRUPPE IN P_CODEGR
      AND   CODE   IN P_CODE
      AND   ERSTELLER  IN P_ERSTEL
      AND   E_DATUM    IN P_EDATUM
      AND   VERWENDUNG EQ 'X'.
     IF SY-DBCNT = L_LINES.
       COMMIT WORK.
       WRITE: 'Number of updated codes: ',L_LINES.
     ELSE.
       ROLLBACK WORK.
       WRITE
        'Update abort due to inconsitencies. Please try it later'.
       L_SUBRC = 1.
     ENDIF.
   ENDIF.

*>>>> END OF INSERTION <<<<<<
   ...
*&-------------------------------------------------------------------
