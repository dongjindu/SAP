*----------------------------------------------------------------------*
*   INCLUDE ZQM_INCLUDE_POOL03                                         *
*----------------------------------------------------------------------*
*//// Equipment Include module : Constants and etc

*//-- Equipment
*- Calibration Equipment Type(category).
CONSTANTS : C_EQTYP_CALIBRATION  TYPE EQTYP VALUE 'Z',
*-  Class type : equipment class
            C_EQUIP_CLASS_TYPE   TYPE KLASSENART VALUE '002'.

*// Equipment Status
*-  Equipment User status : 'IDLE' - E0003
CONSTANTS :  C_EQUIP_STATUS_IDLE TYPE J_STATUS   VALUE 'E0003'.
*- Equipment User Status : 'ERIN' - E0001.
CONSTANTS :  C_EQUIP_STATUS_ERIN TYPE J_STATUS   VALUE 'E0001'.

*- Equipment User Status : 'ERIN' - E0001.
*-   => ABC Indicator - Internal  : '1'
*                     - External  : '2'
*                     - Patrol    : '3'
CONSTANTS : C_EQUIP_ABC_INTERNAL TYPE ABCKZ VALUE '1',
            C_EQUIP_ABC_EXTERNAL TYPE ABCKZ VALUE '2',
            C_EQUIP_ABC_PATROL   TYPE ABCKZ VALUE '3'.

*//- Order Status
*- Confirm
CONSTANTS : C_ORDER_STATUS_CONFIRM TYPE J_STATUS  VALUE 'I0009'.

*-- Partner Function for Equipment
CONSTANTS : C_EQUIP_PARVW_DEPT TYPE PARVW VALUE 'AB', "/Department
            C_EQUIP_PARVW_PERSON TYPE PARVW VALUE 'VW'. "/Person
