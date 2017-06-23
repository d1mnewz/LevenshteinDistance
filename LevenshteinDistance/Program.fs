let inline minOf3 one two three =
    if one < two && one < three then one
    elif two < three then two
    else three

let wagnerFischerLazy (s: string) (t: string) : int =
    let m = s.Length
    let n = t.Length
    let d = Array2D.create (m+1) (n+1) -1
    let rec dist =
        function
        | i, 0 -> i
        | 0, j -> j
        | i, j when d.[i,j] <> -1 -> d.[i,j]
        | i, j ->
            let dval =
                if s.[i-1] = t.[j-1] then dist (i-1, j-1)
                else
                    minOf3
                        (dist (i-1, j)   + 1) // delete
                        (dist (i,   j-1) + 1) // insert
                        (dist (i-1, j-1) + 1) // substitute
            d.[i, j] <- dval; dval
    dist (m, n)

let similarityPercent (source:string) (target:string) :float =
    let dist = wagnerFischerLazy source target
    (1.0 - (float dist / float (max source.Length target.Length))) * 100.0

similarityPercent "Dmytro Zhluktenko" "Dmitro Zluknetko"

// https://stackoverflow.com/questions/35908166/whats-the-difference-between-levenshtein-distance-and-the-wagner-fischer-algori

// The Levenshtein distance is a concept that is defined by a mathematical formula.
// The Wagner-Fischer is a dynamic programming algorithm to compute it efficiently.