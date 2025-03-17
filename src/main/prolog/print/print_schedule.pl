:- module(print_schedule, [write_csv_tables/2, print_tables/1]).
:- use_module('../timetabler').

courses("LEETC", ["ALGA", "Pg", "AM1", "FAE", "ACir", "POO", "AM2", "LSD", "E1", "MAT", "PE", "ACp", "EA", "E2", "SS", "RCp", "PICC_CPg", "PR", "FT", "SEAD1", "ST", "RCom", "RI", "SE1", "AVE", "SCDig", "SOt", "PI", "SCDist", "EGP", "OGE", "SG"]).
courses("LEIC", ["ALGA", "Pg", "LSD", "M1", "Elctr", "POO", "PE", "ACp", "M2", "PSC", "AED", "Com", "CG", "SI1", "LC", "PF", "EGP", "OGE", "SG", "RCp", "AVE", "SOi", "PI", "SI2", "PC", "SI", "RI", "SE1", "Cpl", "SD"]).
courses("LERCM", ["ALGA", "Pg", "AM1", "F1", "ITI", "POO", "PE", "AM2", "F2", "PDSr", "SCDig", "MNO", "AIEC", "PICC_CPg", "CSDin", "RCp", "CSM", "FIA", "MSr", "SOt", "BD", "CGAV", "AA", "RI", "SCDist", "ES", "EGP", "OGE", "RSCM", "PCM", "PIV"]).
courses("MEET", ["AVE", "SE1", "RI", "SEAD2", "ST", "RCom", "PIV", "SCDig", "SEAS", "CEE", "OE", "PRC", "RSCM", "SET", "Ant", "CMov", "STBL", "CSDist", "BD", "CSM", "PI", "SCDist", "EGP", "OGE", "SG", "SCO", "PCI", "IRS", "RDC", "SEADI", "RM", "STDS_PSTR"]).
courses("MEIC", ["SI", "SD", "ES", "RI", "SE1", "Cpl", "CCD", "CSO", "CSI", "RSCM", "CAC", "CIA", "AA", "ASI", "GSI", "PSTR", "IRS", "IS", "EGP", "OGE", "SG"]).

write_csv_tables(FileNamePrefix, MaxDay) :-
    forall(
        courses(Department, Courses),
        (
            atom_concat(FileNamePrefix, '-', TempFilename1),
            atom_concat(TempFilename1, Department, TempFilename2),
            atom_concat(TempFilename2, '.csv', Filename),
            open(Filename, write, Stream),
            forall(
                member(Course, Courses),
                (
                    write(Stream, Course),
                    write(Stream, ','),
                    forall(
                        between(1, MaxDay, D),
                        (   
                            (timetabler:exam(Course, D) -> 
                                write(Stream, 'x')
                            ;
                                write(Stream, '')
                            ),
                            write(Stream, ',')
                        )
                    ),
                    nl(Stream)
                )
            ),
            close(Stream)
        )
    ).

print_header(Department, MaxLength, MaxDay) :-
    Padding is MaxLength + 2,
    format('~w~t~*+  ', [Department, Padding]),
    forall(
        between(1, MaxDay, D),
        format('| ~w ', [D])  % Print each day
    ),
    nl.

print_row(Course, MaxLength, MaxDay) :-
    Padding is MaxLength + 2, 
    format('~|~w~t~*+  ', [Course, Padding]),  
    forall(
        between(1, MaxDay, D),
        (
            (timetabler:exam(Course, D) -> 
                (D < 10 -> format('| x ') ; format('|  x '))
            ; 
                (D < 10 -> format('|   ') ; format('|    '))
            )
        )
    ),
    nl.

print_tables(MaxDay) :-
    forall(
        courses(Department, Courses),
        (
            max_course_length(Courses, MaxLength),
            print_header(Department, MaxLength, MaxDay),
            forall(
                member(Course, Courses),
                print_row(Course, MaxLength, MaxDay)
            ),
            nl
        )
    ).

max_course_length(Courses, MaxLength) :-
    maplist(atom_length, Courses, Lengths),
    max_list(Lengths, MaxLength).