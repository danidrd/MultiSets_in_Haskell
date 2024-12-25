#!/usr/bin/awk -f

BEGIN {
    FS = " - "  # Separatore di campo
}

# Legge il primo file e memorizza gli elementi in un array
FILENAME == ARGV[1] {
    file1[$1] = 1
    next
}

# Legge il secondo file e memorizza gli elementi in un altro array
FILENAME == ARGV[2] {
    file2[$1] = 1
    next
}

END {
    # Controllo che ogni elemento del primo file sia nel secondo
    for (elem in file1) {
        if (!(elem in file2)) {
            print "Elemento \"" elem "\" presente nel primo file ma non nel secondo."
        }
    }

    # Controllo che ogni elemento del secondo file sia nel primo
    for (elem in file2) {
        if (!(elem in file1)) {
            print "Elemento \"" elem "\" presente nel secondo file ma non nel primo."
        }
    }
}
