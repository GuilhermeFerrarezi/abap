*&---------------------------------------------------------------------*
*&  Include           ZTSGF_ALV_SPLIT_PBO
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
module status_9000 output.
  perform zf_set_status_9000.
endmodule.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  ZM_CREATE_ALV_9000  OUTPUT
*&---------------------------------------------------------------------*
module zm_create_alv_9000 output.
  perform zf_build_alv.
endmodule.                 " ZM_CREATE_ALV_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  ZM_CREATE_CONTAINER_9000  OUTPUT
*&---------------------------------------------------------------------*
module zm_create_container_9000 output.
  perform zf_create_containers.
endmodule.                 " ZM_CREATE_CONTAINER_9000  OUTPUT
