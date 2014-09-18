*----------------------------------------------------------------------*
*   INCLUDE ZXL01U08                                                   *
*----------------------------------------------------------------------*
*Authored by Hakchin Kim (20030930)
*Related to SAP Note 63720

*Only numeric storage units are provided as standard. Using our
*user exit MWMK0001 (transaction /nCMOD -> Utilities -> SAP enhancements
*for the development class WM) you can define your own conversions for
*this.

*/Begin of a Developer code
DATA: lv_dummynumc(20) TYPE n VALUE '12345678901234567890'.
*cf_output = '1234567890123456'.  "Dummy numeric value!
cf_output = lv_dummynumc(lngth).  "Dummy numeric value!
*/End of a Developer code
