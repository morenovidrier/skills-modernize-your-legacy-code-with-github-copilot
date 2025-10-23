# Test Plan - Account Management COBOL App

Este documento contiene un plan de pruebas orientado a validar la lógica de negocio del aplicativo COBOL existente. Está pensado para ser revisado con stakeholders y luego transformado en pruebas automatizadas (unitarias e integración) durante la migración a Node.js.

Cada caso de prueba contiene los siguientes campos: Test Case ID, Test Case Description, Pre-conditions, Test Steps, Expected Result, Actual Result, Status (Pass/Fail), Comments.

> Notas generales:
> - El saldo inicial en `DataProgram` es 1000.00
> - Formato de monto: PIC 9(6)V99 (hasta 6 dígitos enteros y 2 decimales)
> - Entradas inválidas deben ser manejadas por la UI (MainProgram) o por validaciones antes de escribir en `DataProgram`.

## Casos de Prueba

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status (Pass/Fail) | Comments |
|--------------|-----------------------|----------------|------------|-----------------|---------------|--------------------|----------|
| TC-01 | Ver balance inicial | El sistema compilado y ejecutable; saldo inicial en DataProgram = 1000.00 | 1. Ejecutar aplicación. 2. Seleccionar opción 1 (View Balance). 3. Observar salida. | El sistema muestra "Current balance: 001000.00" o equivalente formateado a 2 decimales con padding correcto. |  |  | Verificar formato de salida (p.ej. padding) con stakeholders |
| TC-02 | Crédito simple - aumentar saldo | Saldo inicial 1000.00 | 1. Ejecutar aplicación. 2. Seleccionar opción 2 (Credit). 3. Ingresar monto: 250.00. 4. Confirmar salida. | El sistema lee saldo (1000.00), suma 250.00, escribe 1250.00 y muestra "Amount credited. New balance: 001250.00" |  |  | Confirmar que `DataProgram` persiste el nuevo saldo para llamadas posteriores |
| TC-03 | Débito simple con fondos suficientes | Saldo >= monto (ej. 1000.00) | 1. Ejecutar aplicación. 2. Seleccionar opción 3 (Debit). 3. Ingresar monto: 200.00. 4. Confirmar salida. | El sistema lee saldo, verifica que saldo >= monto, resta 200.00, escribe nuevo saldo y muestra "Amount debited. New balance: 000800.00" |  |  | Confirmar persistencia post-débito |
| TC-04 | Débito con fondos insuficientes | Saldo < monto (ej. saldo=100.00, intento de débito 200.00) | 1. Asegurar saldo 100.00 (via crédito/débito previo o reinicio). 2. Ejecutar aplicación. 3. Seleccionar opción 3 (Debit). 4. Ingresar monto: 200.00. | El sistema muestra "Insufficient funds for this debit." y no modifica `STORAGE-BALANCE`. |  |  | Validar que `STORAGE-BALANCE` permanece igual tras intento fallido |
| TC-05 | Múltiples operaciones encadenadas | Saldo inicial 1000.00 | 1. Ejecutar aplicación. 2. Seleccionar crédito 500.00. 3. Seleccionar débito 300.00. 4. Seleccionar ver balance. | Tras crédito, saldo 1500.00; tras débito, saldo 1200.00; ver balance muestra 001200.00 |  |  | Probar secuencia en una sola sesión y con reinicio entre sesiones para validar persistencia si aplica |
| TC-06 | Entrada inválida en menú | Sistema en ejecución | 1. Ejecutar aplicación. 2. Ingresar opción fuera de 1-4 (por ejemplo 9 o letra). | El sistema muestra "Invalid choice, please select 1-4." y continúa mostrando el menú. No se realizan llamadas a `Operations`. |  |  | Confirmar comportamiento ante caracteres no numéricos también |
| TC-07 | Entrada inválida para monto | Seleccionar crédito/débito | 1. Seleccionar Credit o Debit. 2. Introducir entrada no válida para monto (p.ej. 'abc' o número con formato inválido). | El comportamiento actual depende de la implementación de `ACCEPT` en el entorno COBOL: el sistema puede setear AMOUNT a 0 o bloquear. Registrar comportamiento actual; idealmente, el sistema debe rechazar la entrada y solicitar reingreso. |  |  | Priorizar definición con stakeholders para migración a Node.js (validación más estricta) |
| TC-08 | Formato y límites del monto máximo | Saldo y formato PIC 9(6)V99 | 1. Intentar acreditar con monto máximo permitible (999999.99). 2. Intentar acreditar con monto superior (por ejemplo 1000000.00). | Para 999999.99 la suma deberá comportarse correctamente (puede causar overflow según implementación). Para montos mayores, la aplicación debería manejar o reportar error; si no lo hace, documentarlo como riesgo. |  |  | Identificar comportamiento en overflow y definir límites en la migración |
| TC-09 | Persistencia entre sesiones (si aplica) | `DataProgram` con STORAGE-BALANCE persistente en ejecución | 1. Ejecutar crédito y escribir nuevo saldo. 2. Detener aplicación. 3. Ejecutar de nuevo y ver balance. | Si `STORAGE-BALANCE` es persistente entre ejecuciones (no lo es en el código actual cuando se usa memoria sólo en tiempo de ejecución), entender que el valor puede resetear a 1000.00 en cada ejecución. Documentar comportamiento: actualmente `STORAGE-BALANCE` es WORKING-STORAGE con valor inicial, por lo que no persistirá entre ejecuciones en este ejemplo.
| TC-10 | Salir correctamente | Aplicación en ejecución | 1. Seleccionar opción 4 (Exit) desde el menú. | El sistema muestra "Exiting the program. Goodbye!" y finaliza el proceso con código de salida 0 (si posible medir). |  |  | Verificar código de salida si es importante para integración con otros procesos |


## Cómo usar este Test Plan

- Revisa cada caso con los stakeholders y ajusta formatos de salida y mensajes esperados (por ejemplo: padding, decimales, mensajes exactos).
- Usa la columna "Actual Result" para registrar lo observado durante la ejecución manual o automatizada.
- Marca "Status" como Pass/Fail tras la verificación.
- Para la migración a Node.js, convierte cada caso en tests unitarios (por ejemplo, usando Mocha/Jest) apuntando a funciones puras que implementen la lógica de `Operations` y `DataProgram`.


## Consideraciones para migración a Node.js

- Implementar validaciones de entrada robustas antes de convertir strings a montos.
- Decidir la estrategia de persistencia (archivo, base de datos, memoria en proceso) y adaptar TC-09 en consecuencia.
- Añadir tests de límite y de overflow (TC-08) en la capa de negocio.

