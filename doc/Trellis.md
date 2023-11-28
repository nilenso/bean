# Trellis

## Leaf file format

Trellis runs .leaf files. This format is going to change considerably over the
next couple of months. The current usecase it solves is representing a sheet with
its associated tests.

The file consists of three sections separated by `\n\n%\n\n`:

1. Code section
2. Grid section
3. Tests section

### Code section

This section has the code as text.

eg.

```
foo:10
add:{x+y}
```

### Grid section

This section has the sheet's grid contents in CSV form.

eg.

```
1,,3,4
5,,7,8
=A1:A3,,,
```

Due to quirks with the current parser implementation, each row must have atleast
one `,`.

### Tests section

This section defines a new language for the test definitions. Each line
represents a test statement of the form `expression = expression`. eg.

```
A1 = 1
B1 = ""
C1=3
D1=4
A3=1
A1+A2=6
```

## Example

All together, a .leaf looks like...

```
namedten: 10
namedfifty: 50

%

0,1,=A1+B1,A1+B1+C1
4,5,6,7

%

A1 = 0
B1 = C1
C1 = 1
D1 = "A1+B1+C1"
```

Note that the parser is currently quite unforgiving and you'll need to be
specific about not leaving stray newlines or spaces around.