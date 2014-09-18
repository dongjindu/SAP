*----------------------------------------------------------------------*
*   INCLUDE ZXTRKU06                                                   *
*----------------------------------------------------------------------*

*&--------------------------------------------------------------------&*
*  Program: ZXTRKU06
*  Author: Shiva Gnanaguru
*  Specification: To capture Estimated delivery departure and
*                  arrival date and time.
*
*&--------------------------------------------------------------------&*
*  Date         User Id  Transport    Description
* 03/16/2004    100471   UD1K908246   initial program.
*&--------------------------------------------------------------------&*
data:wa_ze1edl1 like ze1edl1.

if idoc_segment-segnam = 'ZE1EDL1'.
  wa_ze1edl1 = idoc_segment-sdata.
  export wa_ze1edl1 to memory id 'ZEDADT'.
endif.
