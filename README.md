# Timetabling Problem

This project addresses the **timetabling problem**, a common scheduling challenge, using **Prolog**. The goal is to create an efficient and conflict-free timetable by assigning resources (e.g., rooms, instructors, and time slots) to events (e.g., classes or exams) while satisfying a set of constraints.

## Features

- **Constraint-driven scheduling**: Guarantees adherence to all defined constraints, such as preventing overlapping events and minimizing conflicts, to ensure an optimal timetable.
- **Prolog-based solution**: Leverages Prolog's declarative nature for solving complex scheduling problems.
- **Customizable constraints**: Easily adapt the solution to different timetabling scenarios.

## Prerequisites

To run this project, you need:

- **SWI-Prolog** (or any compatible Prolog interpreter)
  - Installation instructions: [SWI-Prolog Downloads](https://www.swi-prolog.org/Download.html)

## Getting Started

1. Clone the repository:
```bash
   git clone https://github.com/Thuzys/timetabling_problem.git
   cd timetabling_problem
```

2. Change to the `main` directory:
```bash
   cd src/main
```

3. Start the Prolog interpreter:
```bash
    swipl
```

4. Load the main script:
```prolog
    ?- [timetabler].
```

5. Run the solution for the timetabling problem: 
- 1. Run first epoch solution:
```prolog
    ?- solve_1Epoch(Schedule, Cost, Time).
```
- 2. Run second epoch solution:
```prolog
    ?- solve_2Epoch(Schedule, Cost, Time).
```
**Note**: `Time` is the maximum time allowed for the solution to run (in seconds).
