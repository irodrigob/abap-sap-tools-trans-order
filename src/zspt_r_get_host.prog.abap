*&---------------------------------------------------------------------*
*& Report ZSPT_R_GET_HOST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zspt_r_get_host.
DATA mv_host TYPE string.

START-OF-SELECTION.

  cl_http_server=>get_extension_info( EXPORTING extension_class = 'CL_HTTP_EXT_BSP' IMPORTING urls = DATA(lt_info) ).

  TRY.
      ASSIGN lt_info[ protocol = 'http' ] TO FIELD-SYMBOL(<ls_info>).
      mv_host = |{ <ls_info>-protocol }://{ <ls_info>-host }:{ <ls_info>-port }|.
      WRITE:/ 'HTTP URL: '.
      WRITE:/ mv_host.

    CATCH cx_sy_itab_line_not_found.
  ENDTRY.
  SKIP.
  TRY.
      ASSIGN lt_info[ protocol = 'https' ] TO <ls_info>.
      mv_host = |{ <ls_info>-protocol }://{ <ls_info>-host }:{ <ls_info>-port }|.
      WRITE:/ 'HTTPS URL: '.
      WRITE:/ mv_host.
    CATCH cx_sy_itab_line_not_found.
  ENDTRY.
