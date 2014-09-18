************************************************************************
* Program Name      : ZEMMPM10E_6006
* Author            : Hakchin Kim
* Creation Date     : 2003.09.30.
* Specifications By : Hakchin Kim
* Development Request No : EMMPM10
* Addl Documentation: F/S - EMMPM10 Pull List & Transfer Order
*                                   Creation(Supply to Line)
* Description       : This program is intended to meet the requirement
*                     of HMMA plant to supply the unique material
*                     (around 300 material) by hour on the production
*                     line.
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
************************************************************************
REPORT  zemmpm10e_6006.
************************************************************************
INCLUDE zemmpm10e_6006top.       "Data Declaration
INCLUDE zemmpm10e_6006f01.       "Perform Library.
************************************************************************
START-OF-SELECTION.
  PERFORM get_data_from_table.            "get data
