************************************************************************
* Author                 : bong soo kim
* Creation Date          : 2003-08-12
* Specifications By      :
* Development Request No : UD1K901904
* Addl documentation     :
* Description            : MATERIAL IS CONFIGURABLE CHANGING
*
*
* Modification Log
* Date       Developer    Request ID Description
*
************************************************************************
REPORT ZRBM_MARA_CHANGING
                 NO STANDARD PAGE HEADING
                 LINE-SIZE  255
                 LINE-COUNT 65
                 MESSAGE-ID ZMBM.

TABLES: MARA.

DATA: IT_MARA TYPE MARA OCCURS 0 WITH HEADER LINE.
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.
PARAMETERS: P_MATNR LIKE MARA-MATNR,
            P_KZKFG LIKE MARA-KZKFG.
SELECTION-SCREEN END   OF BLOCK B1.

START-OF-SELECTION.
  PERFORM READ_PROCESS.
  PERFORM DATA_PROCESS.
  PERFORM UPLOAD_PROCESS.

*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
FORM READ_PROCESS.
  SELECT *
       FROM MARA
       INTO TABLE IT_MARA
       WHERE MATNR EQ P_MATNR.
  IF SY-SUBRC NE 0.
    MESSAGE I001 WITH 'NO DATA'.
  ENDIF.
ENDFORM.                    " READ_PROCESS
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
FORM DATA_PROCESS.
  DATA: L_TABIX LIKE SY-TABIX.
  LOOP AT IT_MARA.
    L_TABIX = SY-TABIX.
    IT_MARA-KZKFG = P_KZKFG.
    MODIFY IT_MARA INDEX L_TABIX TRANSPORTING KZKFG.
  ENDLOOP.
ENDFORM.                    " DATA_PROCESS
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_PROCESS
*&---------------------------------------------------------------------*
FORM UPLOAD_PROCESS.
  UPDATE MARA FROM TABLE IT_MARA.
  IF SY-SUBRC EQ 0.
    COMMIT WORK.
    WRITE: 'SUCCESS CREATE'.
  ELSE.
    ROLLBACK WORK.
    WRITE: 'ERROR'.
  ENDIF.
ENDFORM.                    " UPLOAD_PROCESS
