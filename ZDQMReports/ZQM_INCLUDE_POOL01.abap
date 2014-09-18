*----------------------------------------------------------------------*
*   INCLUDE ZQM_INCLUDE_POOL01                                         *
*----------------------------------------------------------------------*
*//Constants ;(C_) ==> True:'X' or '1' False:Space or '0'

*-- Inspection Type Constants
CONSTANTS : C_INSP_TYPE_ISIR    TYPE QPART VALUE '8910',
            C_INSP_TYPE_REGULAR TYPE QPART VALUE '8920',
            C_INSP_TYPE_MS      TYPE QPART VALUE '8930'.

*-- Scheduling Status Constants
CONSTANTS : C_CREATION  TYPE ZQINSPSTATUS VALUE '1',
            C_RELEASE   TYPE ZQINSPSTATUS VALUE '2',
            C_DONTUSE   TYPE ZQINSPSTATUS VALUE '3'.

*-- Vehicle/Engine Type  Constants.
CONSTANTS : C_VE_ENG_CAT_TYPE  TYPE QKATART VALUE 'Q'.

*-- Vehicle/Engine type Code Group
*HMMA QM Engine type
CONSTANTS : C_VE_ENG_CG_ENG TYPE QCODEGRP VALUE 'QQEN'.
*HMMA QM Vehicle type
CONSTANTS : C_VE_ENG_CG_VEH TYPE QCODEGRP VALUE 'QQVE'.

*-- Plant
CONSTANTS : C_PLANT_ENGINE  TYPE WERKS_D VALUE 'E001',
            C_PLANT_VEHICLE TYPE WERKS_D VALUE 'P001'.

*-- Inspection Purpose Code Group and Code
*--  - ISIR    : KATALOGART = 'P',  CODEGRUPPE = 'QPIS', CODE= *
*--  - REGULAR : KATALOGART = 'P',  CODEGRUPPE = 'QPRE', CODE='01'
CONSTANTS : C_KATALOGART      TYPE QKATART   VALUE 'P',
            C_CODEGRUPPE_ISIR TYPE QCODEGRP  VALUE 'QPIS',
            C_CODEGRUPPE_REGU TYPE QCODEGRP  VALUE 'QPRE',
            C_CODEGRUPPE_MS   TYPE QCODEGRP  VALUE 'QPMS'.

CONSTANTS : C_CODE_REGU       TYPE QCODE     VALUE '01',
            C_CODE_MS_ISIR    TYPE QCODE     VALUE '01',
            C_CODE_MS_REGU    TYPE QCODE     VALUE '02'.




CONSTANTS : C_CODE_REGU_BDC     TYPE QCODE  VALUE '1',
            C_CODE_MS_ISIR_BDC  TYPE QCODE  VALUE '1',
            C_CODE_MS_REGU_BDC  TYPE QCODE  VALUE '2'.

CONSTANTS : C_VORGLFNR_ISIR_REG  TYPE QLFNKN VALUE '00000001',
            C_VORGLFNR_MS_1      TYPE QLFNKN VALUE '00000001',
            C_VORGLFNR_MS_2      TYPE QLFNKN VALUE '00000002'.

Constants:  C_HERKUNFT TYPE QHERK VALUE '89'. "/Inspection Lot Origin.

*-- Inspection Scheduling Quantity Default Value.
*--  Quantity =  1 , so it's type is N, length is 1.
Constants : C_INSPECTION_QUANTITY  TYPE N VALUE '1'.


*-- Usage Decision
CONSTANTS : C_UD_CODE_KATART  TYPE QKATART  VALUE '3',  "/Catalog
            C_UD_CODE_GRUPPE  TYPE QCODEGRP VALUE 'Q3'. "/Usage Decision

CONSTANTS : C_UD_CODE_ACCEPT TYPE QCODE  VALUE '01', "/Accept
            C_UD_CODE_MD     TYPE QCODE  VALUE '02', "/MD
            C_UD_CODE_REJECT TYPE QCODE  VALUE '09'. "/Reject

*-- Inspection Lot status value
CONSTANTS :
     C_INSP_STATUS_REL  TYPE J_STATUS VALUE 'I0002', "/Released
     C_INSP_STATUS_RREC TYPE J_STATUS VALUE 'I0213', "/Results confirmed
     C_INSP_STATUS_UD   TYPE J_STATUS VALUE 'I0218'. "/UD has been made

*-- MIC count for Inspection Scheduling
*-- (quantity of available MIC by type and plant)
CONSTANTS :
     C_INSP_ISP_CNT TYPE I  VALUE 22, "/ISIR-P001
     C_INSP_ISE_CNT TYPE I  VALUE 17, "/ISIR-E001
     C_INSP_REG_CNT TYPE I  VALUE 3,  "/Regular-P001/E001
     C_INSP_MS_CNT  TYPE I  VALUE 3.  "/MS OF ISIR/Regular
*
*-- Material Type for ISIR Inspection
CONSTANTS :
     C_MTART_ISIR  TYPE MTART  VALUE 'QCIS', "/Development material
     C_MTART_ROH   TYPE MTART  VALUE 'ROH',
     C_MTART_HALB  TYPE MTART  VALUE 'HALB'.  "/
