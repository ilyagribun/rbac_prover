
data Principal a = Atom a
    | And (Principal a) (Principal a)
    | As (Principal a) (Principal a)
