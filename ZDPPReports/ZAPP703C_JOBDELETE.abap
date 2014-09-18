************************************************************************
* Program Name      : ZAPP703C_JOBDELETE
* Author            : Bobby
* Creation Date     : 2003.09.16.
* Specifications By : Bobby
* Pattern           : 2.1
* Development Request No : UD1K902220
* Addl Documentation:
* Description       : Interface Work Order from Legacy System
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT  zapp703c_jobdelete                   .

TABLES tbtcp.

DATA: it_tbtcp LIKE TABLE OF tbtcp WITH HEADER LINE.
DATA: wa_msg(100).

SELECT-OPTIONS: s_jobnam FOR tbtcp-jobname,
                s_pronam FOR tbtcp-progname,
                s_date FOR tbtcp-sdldate,
                s_time FOR tbtcp-sdltime.

START-OF-SELECTION.
  SELECT * FROM tbtcp
           INTO TABLE it_tbtcp
           WHERE jobname IN s_jobnam
             AND progname IN s_pronam
             AND sdldate IN s_date
             AND sdltime IN s_time.

  IF sy-subrc = 0.
    DELETE tbtcp FROM TABLE it_tbtcp.
  ENDIF.
END-OF-SELECTION.
