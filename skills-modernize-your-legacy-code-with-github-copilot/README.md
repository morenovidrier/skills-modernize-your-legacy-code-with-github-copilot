# Contenido de /docs/README.md

# Documentación del Proyecto: Sistema de Gestión de Cuentas

## Propósito de los Archivos COBOL

### 1. `data.cob`
Este archivo contiene el programa `DataProgram`, que maneja las operaciones de lectura y escritura del saldo de una cuenta. Tiene variables para el saldo y el tipo de operación, y utiliza la sección de procedimiento para realizar las operaciones según el tipo recibido.

### 2. `main.cob`
Este archivo es el punto de entrada del sistema de gestión de cuentas. Presenta un menú al usuario y permite seleccionar entre ver el saldo, acreditar o debitar la cuenta. Llama al programa `Operations` según la elección del usuario.

### 3. `operations.cob`
Este archivo contiene el programa `Operations`, que gestiona las operaciones de crédito y débito. Llama al `DataProgram` para leer y escribir el saldo, y maneja la lógica para asegurar que no se debite más de lo que hay en la cuenta.

## Reglas de Negocio Específicas

- **Saldo Inicial**: El saldo inicial de la cuenta es de 1000.00.
- **Operaciones Permitidas**: Las operaciones permitidas son `CREDIT` y `DEBIT`. No se puede debitar más de lo que hay en la cuenta.
- **Visualización del Saldo**: El saldo actual se puede visualizar en cualquier momento a través del menú principal.