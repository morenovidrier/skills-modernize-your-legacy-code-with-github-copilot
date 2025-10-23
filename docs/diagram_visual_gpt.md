# Diagrama Visual (GPT) de Llamadas entre Programas COBOL

A continuación tienes un diagrama orgánico en formato visual (cajas y flechas) que muestra las llamadas entre los programas COBOL del proyecto.

```ascii
                                    ┌────────────────────────────┐
                                    │         Usuario            │
                                    │ - Interactúa con el menú   │
                                    └─────────────┬──────────────┘
                                                  │
                                                  │ selecciona opción
                                                  ▼
┌────────────────────────────────────────────────────────────────────────────┐
│                               MainProgram                                  │
│ ┌────────────────────────────────────────────────────────────────────────┐ │
│ │   - Muestra menú (1:Total, 2:Credit, 3:Debit, 4:Exit)                   │ │
│ │   - Acepta `USER-CHOICE`                                                │ │
│ │   - Controla `CONTINUE-FLAG`                                            │ │
│ └────────────────────────────────────────────────────────────────────────┘ │
└───────────────┬───────────────────────────────┬────────────────────────────┘
                │                               │
                │ CALL 'Operations' USING       │ CALL 'Operations' USING
                │ 'TOTAL'                       │ 'CREDIT' / 'DEBIT'
                ▼                               ▼
      ┌──────────────────────────┐      ┌────────────────────────────────┐
      │       Operations         │      │            Notes               │
      │  - MOVE PASSED-OPERATION │      │  - OPERATION-TYPE: PIC X(6)    │
      │  - IF 'TOTAL' -> READ    │      │  - AMOUNT: PIC 9(6)V99         │
      │  - IF 'CREDIT' -> READ   │      │  - FINAL-BALANCE: PIC 9(6)V99  │
      │    ACCEPT AMOUNT         │      └────────────────────────────────┘
      │    ADD AMOUNT, WRITE     │
      │  - IF 'DEBIT' -> READ    │
      │    ACCEPT AMOUNT         │
      │    IF FINAL-BALANCE >=   │
      │        AMOUNT -> SUBTRACT│
      │        WRITE             │
      │    ELSE DISPLAY ERROR    │
      └──────────┬───────────────┘
                 │
                 │ CALL 'DataProgram' USING 'READ'/'WRITE', FINAL-BALANCE
                 ▼
      ┌──────────────────────────┐
      │       DataProgram        │
      │  - STORAGE-BALANCE:      │
      │    PIC 9(6)V99 VALUE     │
      │    1000.00               │
      │  - PROCEDURE USING       │
      │    PASSED-OPERATION,     │
      │    BALANCE               │
      │  - IF 'READ' -> MOVE     │
      │    STORAGE-BALANCE TO    │
      │    BALANCE               │
      │  - IF 'WRITE' -> MOVE    │
      │    BALANCE TO STORAGE-   │
      │    BALANCE               │
      └──────────────────────────┘
```

## Flujo resumido

- El usuario selecciona una opción en `MainProgram`.
- `MainProgram` llama a `Operations` con el tipo de operación.
- `Operations` lee el balance de `DataProgram` cuando es necesario.
- Para créditos/débitos, `Operations` modifica `FINAL-BALANCE` y escribe de vuelta a `DataProgram`.
- `DataProgram` mantiene el `STORAGE-BALANCE` persistente (valor inicial 1000.00).

## Notas

- El diagrama está pensado para ser simple y fácil de leer en texto plano.
- Si quieres una versión exportable (SVG/PNG) o más detallada (por ejemplo, mostrando parámetros exactos en cada llamada), puedo generarla a partir de este diagrama.
