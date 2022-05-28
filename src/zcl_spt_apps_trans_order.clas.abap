CLASS zcl_spt_apps_trans_order DEFINITION
  PUBLIC
  INHERITING FROM zcl_spt_apps_base
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ts_user_orders,
             order            TYPE trkorr,
             order_desc       TYPE string,
             order_user       TYPE uname,
             order_type       TYPE trfunction,
             order_type_desc  TYPE val_text,
             task             TYPE trkorr,
             task_user        TYPE uname,
             task_status      TYPE trstatus,
             task_status_desc TYPE val_text,
           END OF ts_user_orders.
    TYPES: tt_user_orders TYPE STANDARD TABLE OF ts_user_orders WITH EMPTY KEY.
    TYPES: BEGIN OF ts_systems_transport,
             system_name TYPE sysname,
             system_desc TYPE string,
           END OF ts_systems_transport.
    TYPES: tt_systems_transport TYPE STANDARD TABLE OF ts_systems_transport WITH EMPTY KEY.
    METHODS zif_spt_core_app~get_app_type REDEFINITION.
    "! <p class="shorttext synchronized">Devuelve las ordenes de un usuario </p>
    "! @parameter iv_username | <p class="shorttext synchronized">Usuario</p>
    "! @parameter iv_type_workbench | <p class="shorttext synchronized">Ordenes workbench</p>
    "! @parameter iv_type_customizing | <p class="shorttext synchronized">Ordenes customizing</p>
    "! @parameter iv_type_transport | <p class="shorttext synchronized">Transporte de copias</p>
    "! @parameter iv_status_modif | <p class="shorttext synchronized">Status modificable</p>
    "! @parameter iv_status_release | <p class="shorttext synchronized">Status liberadas</p>
    "! @parameter iv_release_from_data | <p class="shorttext synchronized">Ordenes liberadas desde</p>
    "! @parameter iv_release_from_to | <p class="shorttext synchronized">Ordenes liberadas desde</p>
    "! @parameter et_orders | <p class="shorttext synchronized">Ordenes</p>
    METHODS get_user_orders
      IMPORTING
        !iv_username          TYPE syuname DEFAULT sy-uname
        !iv_type_workbench    TYPE sap_bool DEFAULT abap_true
        !iv_type_customizing  TYPE sap_bool DEFAULT abap_true
        !iv_type_transport    TYPE sap_bool DEFAULT abap_true
        !iv_status_modif      TYPE sap_bool DEFAULT abap_true
        !iv_status_release    TYPE sap_bool DEFAULT abap_false
        !iv_release_from_data TYPE sy-datum OPTIONAL
        !iv_release_from_to   TYPE sy-datum OPTIONAL
      EXPORTING
        et_orders             TYPE tt_user_orders.
    "! <p class="shorttext synchronized">Sistemas de transporte</p>
    "! @parameter rt_systems | <p class="shorttext synchronized">Sistemas</p>
    METHODS get_systems_transport
      RETURNING VALUE(rt_systems) TYPE tt_systems_transport.
    "! <p class="shorttext synchronized">Hacer transporte de copia</p>
    "! @parameter it_orders | <p class="shorttext synchronized">Ordenes</p>
    "! @parameter iv_system | <p class="shorttext synchronized">Sistema</p>
    "! @parameter iv_description | <p class="shorttext synchronized">Descripción</p>
    "! @parameter et_return | <p class="shorttext synchronized">Retorno del proceso</p>
    "! @parameter ev_order | <p class="shorttext synchronized">Orden creada</p>
    METHODS do_transport_copy
      IMPORTING
        it_orders      TYPE zif_spt_trans_order_data=>tt_orders
        iv_system      TYPE sysname
        iv_description TYPE string
      EXPORTING
        et_return      TYPE zif_spt_core_data=>tt_return
        ev_order       TYPE trkorr.
  PROTECTED SECTION.
    "! <p class="shorttext synchronized">Parámetros de selección</p>
    "! @parameter iv_username | <p class="shorttext synchronized">Usuario</p>
    "! @parameter iv_type_workbench | <p class="shorttext synchronized">Ordenes workbench</p>
    "! @parameter iv_type_customizing | <p class="shorttext synchronized">Ordenes customizing</p>
    "! @parameter iv_type_transport | <p class="shorttext synchronized">Transporte de copias</p>
    "! @parameter iv_status_modif | <p class="shorttext synchronized">Status modificable</p>
    "! @parameter iv_status_release | <p class="shorttext synchronized">Status liberadas</p>
    "! @parameter iv_release_from_data | <p class="shorttext synchronized">Ordenes liberadas desde</p>
    "! @parameter iv_release_from_to | <p class="shorttext synchronized">Ordenes liberadas desde</p>
    "! @parameter et_orders | <p class="shorttext synchronized">Ordenes</p>
    METHODS fill_selections_orders
      IMPORTING
        iv_type_workbench    TYPE sap_bool DEFAULT abap_true
        iv_type_customizing  TYPE sap_bool DEFAULT abap_true
        iv_type_transport    TYPE sap_bool DEFAULT abap_true
        iv_status_modif      TYPE sap_bool DEFAULT abap_true
        iv_status_release    TYPE sap_bool DEFAULT abap_false
        iv_release_from_data TYPE sy-datum OPTIONAL
        iv_release_from_to   TYPE sy-datum OPTIONAL
      RETURNING
        VALUE(rs_selections) TYPE trwbo_selection.
    "! <p class="shorttext synchronized">Lectura de textos de dominio</p>
    "! @parameter iv_username | <p class="shorttext synchronized">Usuario</p>
    "! @parameter iv_type_workbench | <p class="shorttext synchronized">Ordenes workbench</p>
    METHODS load_domain_texts
      EXPORTING
        et_functions TYPE dd07v_tab
        et_status    TYPE dd07v_tab.
    "! <p class="shorttext synchronized">Creación de orden de transporte</p>
    "! @parameter iv_type | <p class="shorttext synchronized">Tipo de orden</p>
    "! @parameter iv_system | <p class="shorttext synchronized">Sistema</p>
    "! @parameter iv_description | <p class="shorttext synchronized">Descripción</p>
    "! @parameter iv_user | <p class="shorttext synchronized">Usuario</p>
    "! @parameter es_return | <p class="shorttext synchronized">Retorno de la creación</p>
    "! @parameter ev_order | <p class="shorttext synchronized">Orden creada</p>
    METHODS create_order
      IMPORTING
        iv_type        TYPE trfunction
        iv_description TYPE string
        iv_system      TYPE sysname OPTIONAL
        iv_user        TYPE syuname DEFAULT sy-uname
      EXPORTING
        es_return      TYPE zif_spt_core_data=>ts_return
        ev_order       TYPE trkorr.
    "! <p class="shorttext synchronized">Copia el contenido de unas ordenes a otra orden</p>
    "! @parameter it_from_orders | <p class="shorttext synchronized">Ordenes origen</p>
    "! @parameter iv_to_order | <p class="shorttext synchronized">Orden destino</p>
    METHODS copy_content_orders_2_order
      IMPORTING
        it_from_orders TYPE zif_spt_trans_order_data=>tt_orders
        iv_to_order    TYPE trkorr.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_SPT_APPS_TRANS_ORDER IMPLEMENTATION.


  METHOD copy_content_orders_2_order.
    LOOP AT it_from_orders ASSIGNING FIELD-SYMBOL(<ls_orders>).
      CALL FUNCTION 'TR_COPY_COMM'
        EXPORTING
          wi_dialog                = space
          wi_trkorr_from           = <ls_orders>
          wi_trkorr_to             = iv_to_order
          wi_without_documentation = abap_true
        EXCEPTIONS
          OTHERS                   = 1.
    ENDLOOP.
  ENDMETHOD.


  METHOD create_order.

    CLEAR: es_return.

    DATA(lv_order_text) = CONV e07t-as4text( iv_description ).

    CALL FUNCTION 'TRINT_INSERT_NEW_COMM'
      EXPORTING
        wi_kurztext   = lv_order_text
        wi_trfunction = iv_type
        iv_username   = sy-uname
        iv_tarsystem  = iv_system
        wi_client     = sy-mandt
      IMPORTING
        we_trkorr     = ev_order
      EXCEPTIONS
        OTHERS        = 1.
    IF sy-subrc = 0.

    ELSE.
      DATA(ls_return_bapi) = zcl_spt_utilities=>fill_return( iv_type = zif_spt_core_data=>cs_message-type_error
                                                           iv_id = sy-msgid
                                                           iv_number = sy-msgno
                                                           iv_message_v1 = sy-msgv1
                                                           iv_message_v2 = sy-msgv2
                                                           iv_message_v3 = sy-msgv3
                                                           iv_message_v4 = sy-msgv4
                                                           iv_langu      = mv_langu ).
      es_return-type = zif_spt_core_data=>cs_message-type_error.
      es_return-message = ls_return_bapi-message.

    ENDIF.

  ENDMETHOD.


  METHOD do_transport_copy.

    " Se crea la orden donde se pondrán los objetos
    create_order( EXPORTING iv_type = zif_spt_trans_order_data=>orders_type-transport_copies
                            iv_description = iv_description
                            iv_system = iv_system
                  IMPORTING es_return = DATA(ls_return)
                            ev_order = ev_order ).

    " Se pasa el contenido de las ordenes pasadas a la nueva orden
    copy_content_orders_2_order( EXPORTING it_from_orders = it_orders
                                             iv_to_order = ev_order ).


  ENDMETHOD.


  METHOD fill_selections_orders.
    CLEAR rs_selections.

    IF iv_type_workbench = abap_true.
      rs_selections-reqfunctions(1)     = sctsc_type_workbench.
    ENDIF.
    IF iv_type_customizing = abap_true.
      rs_selections-reqfunctions+1(1)   = sctsc_type_customizing.
    ENDIF.
    IF iv_type_transport = abap_true.
      rs_selections-reqfunctions+2(1)   = sctsc_type_transport.
    ENDIF.

* Types of assigned tasks
    rs_selections-taskfunctions      = sctsc_types_tasks.

* Status para ordenes modificables
    IF iv_status_modif = abap_true.
      rs_selections-taskstatus(1)     = sctsc_state_protected.
      rs_selections-taskstatus+1(1)   = sctsc_state_changeable.
    ENDIF.

* Status para ordenes liberadas
    IF iv_status_release = abap_true.
      rs_selections-taskstatus+2(1)   = sctsc_state_released.
      rs_selections-taskstatus+3(1)   = sctsc_state_notconfirmed.
    ENDIF.

    IF iv_status_modif = abap_true AND iv_status_release = abap_false.
      rs_selections-reqstatus(1)   = sctsc_state_protected.
      rs_selections-reqstatus+1(1) = sctsc_state_changeable.
*    r_selections-reqstatus+2(1) = sctsc_state_export_started.
    ELSEIF iv_status_modif = abap_false AND iv_status_release = abap_true.
      rs_selections-reqstatus(1)   = sctsc_state_released.
      rs_selections-reqstatus+1(1) = sctsc_state_export_started.
    ELSEIF iv_status_modif = abap_true AND iv_status_release = abap_true.
      rs_selections-reqstatus      = sctsc_states_all.
    ENDIF.
  ENDMETHOD.


  METHOD get_systems_transport.
    DATA lv_version TYPE tcevers-version.

    CLEAR rt_systems.

* Version activa del sistema de transporte
    CALL FUNCTION 'TR_GET_CONFIG_VERSION'
      IMPORTING
        ev_active_version       = lv_version
      EXCEPTIONS
        no_active_version_found = 1.

    IF sy-subrc = 0.
      SELECT sysname AS system_name ddtext AS system_desc INTO TABLE rt_systems
              FROM  tcesystt
               WHERE version = lv_version
               AND   spras  = mv_langu.
      IF sy-subrc NE 0.
        " Si no hay en el idioma global busco en el de logon
        SELECT sysname AS system_name ddtext AS system_desc INTO TABLE rt_systems
                      FROM  tcesystt
                       WHERE version = lv_version
                       AND   spras  = sy-langu.
        IF sy-subrc NE 0.
          " Si no hay busco directamente el codig, y la descripcion será el mismo codigo
          SELECT sysname AS system_name sysname AS system_desc INTO TABLE rt_systems
                        FROM  tcesyst
                         WHERE version = lv_version.
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD get_user_orders.
    DATA lt_request TYPE trwbo_request_headers.

    " relleno de los datos de seleccion
    DATA(ls_selection) = fill_selections_orders( EXPORTING iv_type_workbench = iv_type_workbench
                                                     iv_type_customizing = iv_type_customizing
                                                     iv_type_transport = iv_type_transport
                                                     iv_status_modif = iv_status_modif
                                                     iv_status_release = iv_status_release
                                                     iv_release_from_data = iv_release_from_data
                                                     iv_release_from_to = iv_release_from_to ).

    " Carga de los textos
    load_domain_texts( IMPORTING et_functions = DATA(lt_functions_txt)
                                 et_status    = DATA(lt_status_txt) ).

    " Lectura de las ordenes
    CALL FUNCTION 'TRINT_SELECT_REQUESTS'
      EXPORTING
        iv_username_pattern  = iv_username
        is_selection         = ls_selection
        iv_complete_projects = 'X'
      IMPORTING
        et_requests          = lt_request.


    SORT lt_request BY trfunction DESCENDING as4date DESCENDING as4time DESCENDING.

    " Sacamos las padres para ir obteniendo los hijos
    LOOP AT lt_request ASSIGNING FIELD-SYMBOL(<ls_request>)
                                 WHERE strkorr IS INITIAL.

      " Relleno los campos base de la orden
      DATA(ls_orders) = VALUE ts_user_orders( order = <ls_request>-trkorr
                                              order_user = <ls_request>-as4user
                                              order_desc = <ls_request>-as4text
                                              order_type = <ls_request>-trfunction ).
      READ TABLE lt_functions_txt ASSIGNING FIELD-SYMBOL(<ls_dd07v>) WITH KEY domvalue_l = <ls_request>-trfunction.
      IF sy-subrc = 0.
        ls_orders-order_type_desc = <ls_dd07v>-ddtext.
      ENDIF.

      " Ahora las tareas de la orden
      LOOP AT lt_request ASSIGNING FIELD-SYMBOL(<ls_tasks>) WHERE strkorr = <ls_request>-trkorr.
        INSERT ls_orders INTO TABLE et_orders ASSIGNING FIELD-SYMBOL(<ls_orders>).
        <ls_orders>-task = <ls_tasks>-trkorr.
        <ls_orders>-task_user = <ls_tasks>-as4user.
        <ls_orders>-task_status = SWITCH #( <ls_tasks>-trstatus
                                             WHEN sctsc_state_protected OR sctsc_state_changeable THEN sctsc_state_changeable
                                             WHEN sctsc_state_released OR sctsc_state_export_started THEN sctsc_state_released ).
        READ TABLE lt_status_txt ASSIGNING <ls_dd07v> WITH KEY domvalue_l = <ls_orders>-task_status.
        IF sy-subrc = 0.
          <ls_orders>-task_status_desc = <ls_dd07v>-ddtext.
        ENDIF.
      ENDLOOP.
      IF sy-subrc NE 0.
        INSERT ls_orders INTO TABLE et_orders.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD load_domain_texts.
    DATA lt_functions_n TYPE dd07v_tab.
    DATA lt_status_n TYPE dd07v_tab.

    CLEAR: et_functions, et_status.

    " Textos de tipo de orden
    IF et_functions IS SUPPLIED.
      CALL FUNCTION 'DD_DOMA_GET'
        EXPORTING
          domain_name   = 'TRFUNCTION'
          langu         = mv_langu
        TABLES
          dd07v_tab_a   = et_functions
          dd07v_tab_n   = lt_functions_n
        EXCEPTIONS
          illegal_value = 1
          op_failure    = 2
          OTHERS        = 3.
    ENDIF.

    " Textos de status
    IF et_status IS SUPPLIED.
      CALL FUNCTION 'DD_DOMA_GET'
        EXPORTING
          domain_name   = 'TRSTATUS'
          langu         = mv_langu
        TABLES
          dd07v_tab_a   = et_status
          dd07v_tab_n   = lt_status_n
        EXCEPTIONS
          illegal_value = 1
          op_failure    = 2
          OTHERS        = 3.
    ENDIF.

  ENDMETHOD.


  METHOD zif_spt_core_app~get_app_type.
    CLEAR: es_app.

    es_app-app = 'TRANS_ORDER'.
    es_app-app_desc = 'Transport order'(t01).
    es_app-service = '/ZSAP_TOOLS_TRANS_ORDER_SRV'.
    es_app-frontend_page = '/trans_order'.
  ENDMETHOD.
ENDCLASS.
