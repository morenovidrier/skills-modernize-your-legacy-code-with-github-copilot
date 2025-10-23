# Diagrama Visual de Relaciones entre Programas COBOL

## Estructura del Sistema

```ascii
┌────────────────────────────────────────────────┐
│                 MainProgram                    │
│ ┌────────────────────────────────────────────┐ │
│ │              Interfaz Usuario              │ │
│ │        - Menú de opciones (1-4)           │ │
│ │        - Control de flujo principal        │ │
│ └────────────────────────────────────────────┘ │
└─────────────────────┬──────────────────────────┘
                      │
                      │ CALL 'Operations' USING
                      ▼
┌────────────────────────────────────────────────┐
│                 Operations                      │
│ ┌────────────────────────────────────────────┐ │
│ │           Lógica de Negocio               │ │
│ │     ┌─────────────────────────┐           │ │
│ │     │      Ver Balance        │           │ │
│ │     │   ('TOTAL' operation)   │           │ │
│ │     └─────────────────────────┘           │ │
│ │     ┌─────────────────────────┐           │ │
│ │     │   Crédito a Cuenta      │           │ │
│ │     │  ('CREDIT' operation)   │           │ │
│ │     └─────────────────────────┘           │ │
│ │     ┌─────────────────────────┐           │ │
│ │     │    Débito de Cuenta     │           │ │
│ │     │   ('DEBIT' operation)   │           │ │
│ │     └─────────────────────────┘           │ │
│ └────────────────────────────────────────────┘ │
└─────────────────────┬──────────────────────────┘
                      │
                      │ CALL 'DataProgram' USING
                      ▼
┌────────────────────────────────────────────────┐
│                 DataProgram                     │
│ ┌────────────────────────────────────────────┐ │
│ │         Persistencia de Datos              │ │
│ │                                            │ │
│ │     ┌─────────────────────────┐           │ │
│ │     │    Lectura Balance      │           │ │
│ │     │   ('READ' operation)    │           │ │
│ │     └─────────────────────────┘           │ │
│ │     ┌─────────────────────────┐           │ │
│ │     │  Escritura Balance      │           │ │
│ │     │   ('WRITE' operation)   │           │ │
│ │     └─────────────────────────┘           │ │
│ │                                            │ │
│ │     STORAGE-BALANCE: 9(6)V99              │ │
│ └────────────────────────────────────────────┘ │
└────────────────────────────────────────────────┘
```

## Descripción de las Relaciones

1. **MainProgram → Operations**
   - Inicia todas las operaciones del sistema
   - Parámetros de llamada:
     - 'TOTAL' : Consulta de balance
     - 'CREDIT': Operación de crédito
     - 'DEBIT' : Operación de débito

2. **Operations → DataProgram**
   - Gestiona la persistencia de datos
   - Parámetros de llamada:
     - 'READ' + FINAL-BALANCE : Lectura del balance
     - 'WRITE' + FINAL-BALANCE: Actualización del balance

## Variables Clave

### MainProgram

- `USER-CHOICE`: PIC 9
- `CONTINUE-FLAG`: PIC X(3)

### Operations

- `OPERATION-TYPE`: PIC X(6)
- `AMOUNT`: PIC 9(6)V99
- `FINAL-BALANCE`: PIC 9(6)V99

### DataProgram

- `STORAGE-BALANCE`: PIC 9(6)V99
- `OPERATION-TYPE`: PIC X(6)
