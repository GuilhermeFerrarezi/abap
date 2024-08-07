*&---------------------------------------------------------------------*
*&  Include           ZTSGF_SIMPLEX_PBO
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_9000 output.
  set pf-status 'ZSTS_INITIAL_SCREEN'.
  set titlebar 'ZUTS_INITIAL_SCREEN'.

endmodule.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_9010  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_9010 output.
  set pf-status 'ZSTS_SECOND_SCREEN'.
  set titlebar 'ZUTS_SECOND_SCREEN'.

  perform zf_prepare_equality_symbols.

endmodule.                 " STATUS_9010  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_9030  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_9020 output.
  set pf-status 'ZSTS_RESULT_SCREEN'.
  set titlebar 'ZUTS_RESULT_SCREEN'.

endmodule.                 " STATUS_9030  OUTPUT
