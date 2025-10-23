# Diagrama de Relaciones entre Programas COBOL

Este diagrama muestra la estructura orgánica y las relaciones entre los diferentes programas COBOL del sistema.

```mermaid
graph TD
    classDef mainProgram fill:#f9f,stroke:#333,stroke-width:2px
    classDef operations fill:#bbf,stroke:#333,stroke-width:2px
    classDef dataProgram fill:#bfb,stroke:#333,stroke-width:2px

    Main[MainProgram]:::mainProgram
    Ops[Operations]:::operations
    Data[DataProgram]:::dataProgram

    %% Relaciones desde MainProgram
    Main -->|CALL 'Operations' USING 'TOTAL'| Ops
    Main -->|CALL 'Operations' USING 'CREDIT'| Ops
    Main -->|CALL 'Operations' USING 'DEBIT'| Ops

    %% Relaciones desde Operations
    Ops -->|CALL 'DataProgram' USING 'READ', FINAL-BALANCE| Data
    Ops -->|CALL 'DataProgram' USING 'WRITE', FINAL-BALANCE| Data

    %% Notas informativas
    subgraph MainProgram
        direction LR
        M1[Menú Principal]
        M2[Control de Flujo]
        M3[Interfaz Usuario]
    end

    subgraph Operations
        direction LR
        O1[Ver Balance]
        O2[Crédito]
        O3[Débito]
        O4[Validación de Fondos]
    end

    subgraph DataProgram
        direction LR
        D1[Almacenamiento]
        D2[Lectura/Escritura]
        D3[Balance Persistente]
    end

    %% Estilos
    style Main fill:#f9f,stroke:#333,stroke-width:2px
    style Ops fill:#bbf,stroke:#333,stroke-width:2px
    style Data fill:#bfb,stroke:#333,stroke-width:2px
    
    %% Leyenda
    subgraph Leyenda
        L1[Programa Principal]:::mainProgram
        L2[Lógica de Negocio]:::operations
        L3[Persistencia de Datos]:::dataProgram
    end
```

## Descripción del Diagrama

- **MainProgram (Rosa)**: El punto de entrada del sistema que maneja la interfaz de usuario y el control de flujo principal.
- **Operations (Azul)**: El módulo de lógica de negocio que procesa todas las operaciones de la cuenta.
- **DataProgram (Verde)**: La capa de persistencia que maneja el almacenamiento del balance.

### Flujos de Llamadas

1. **MainProgram -> Operations**:
   - Llama a Operations con 'TOTAL' para consultar el balance
   - Llama a Operations con 'CREDIT' para realizar créditos
   - Llama a Operations con 'DEBIT' para realizar débitos

2. **Operations -> DataProgram**:
   - Llama a DataProgram con 'READ' para obtener el balance actual
   - Llama a DataProgram con 'WRITE' para actualizar el balance

### Responsabilidades

- **MainProgram**: Interfaz de usuario y control de flujo
- **Operations**: Lógica de negocio y validaciones
- **DataProgram**: Persistencia y gestión de datos
