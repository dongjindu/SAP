*----------------------------------------------------------------------*
*   INCLUDE ZXQPLU04                                                   *
*----------------------------------------------------------------------*

**-- MOVE Inspection purpose and Vehicle Data to Screen and Screen Stru
**-- cture ZSQM_CI_QALS.
TABLES : ZSQM_CI_QALS.

*-- Include Program ( Include Constants or etc)
INCLUDE : ZQM_INCLUDE_POOL01. "/Inspection Constants and etc

MOVE-CORRESPONDING I_QALS TO ZSQM_CI_QALS.

ZSQM_CI_QALS-KATALOGART = C_KATALOGART. "/'P'-Inspection purpose


*-- Inspection Schedulig Default Value for Validation Purpose Code.
CASE I_QALS-ART.
  WHEN C_INSP_TYPE_ISIR.
    ZSQM_CI_QALS-CODEGRUPPE = C_CODEGRUPPE_ISIR.
  WHEN C_INSP_TYPE_REGULAR.
    ZSQM_CI_QALS-CODEGRUPPE = C_CODEGRUPPE_REGU.
  WHEN C_INSP_TYPE_MS.
    ZSQM_CI_QALS-CODEGRUPPE = C_CODEGRUPPE_MS.
ENDCASE.

*-- Vehicle/Engine type Default value
 IF ZSQM_CI_QALS-KATART_VH IS INITIAL.
   ZSQM_CI_QALS-KATART_VH = C_VE_ENG_CAT_TYPE .
 ENDIF.


IF NOT  ZSQM_CI_QALS-CODEGRUPPE IS INITIAL AND
   NOT  ZSQM_CI_QALS-CODE       IS INITIAL.


  SELECT SINGLE B~KURZTEXT
    INTO ZSQM_CI_QALS-KURZTEXT
      FROM  QPGR AS A INNER JOIN QPCT AS B
        ON  A~KATALOGART = B~KATALOGART
        AND A~CODEGRUPPE = B~CODEGRUPPE
      WHERE A~KATALOGART = 'P'
        AND A~CODEGRUPPE = ZSQM_CI_QALS-CODEGRUPPE
        AND B~CODE       = ZSQM_CI_QALS-CODE
        AND B~SPRACHE    = 'E'
        AND B~VERSION    = '000001'.
ELSE.
  CLEAR : ZSQM_CI_QALS-KURZTEXT.
*             ZSQM_CI_QALS-CODEGRUPPE,
*             ZSQM_CI_QALS-CODE.
ENDIF.


*<<< Start of Delete : 10/17/2003 - sllee
*IF   NOT  ZSQM_CI_QALS-VEHICLE IS INITIAL.
*
*   SELECT SINGLE B~MAKTX  INTO ZSQM_CI_QALS-MAKTX
*      FROM MARA AS A  INNER JOIN MAKT AS B
*        ON  A~MATNR = B~MATNR
*      WHERE A~MATNR = ZSQM_CI_QALS-VEHICLE
*        AND B~SPRAS = 'E'.
*ELSE.
*  CLEAR : ZSQM_CI_QALS-MAKTX,
*          ZSQM_CI_QALS-VEHICLE.
*
*
*ENDIF.
*<<< End of Delete : 10/17/2003 - sllee


*<<< Start of  Added : 10/17/2003 - sllee
*- Short Text for Code of Vehicle/Engine type
IF NOT ZSQM_CI_QALS-CODE_VH IS INITIAL.
  PERFORM GET_TEXT_OF_CODE_QPCT      USING ZSQM_CI_QALS-KATART_VH
                                           ZSQM_CI_QALS-CODEGRP_VH
                                           ZSQM_CI_QALS-CODE_VH
                                  CHANGING ZSQM_CI_QALS-KURZTEXT_VH.


ELSE.

ENDIF.
*<<< End of Added : 10/17/2003 - sllee
