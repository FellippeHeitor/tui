Option _Explicit
Dim result As Long

result = tui("reset")
result = tui("add;type=form;name=form1;caption=Hello, world!;metrics={center,w=50,h=10}")
result = tui("add;type=button;name=button1;parent=form1;caption=Click me;metrics={x=10,y=20,w=50,h=1}")
Sleep
Do

    result = tui("update")
    If tui("clicked") Then
    End If
    _Limit 10
    _Display
Loop

Function tui& (action$) Static
    Type newControl
        As Long id, x, y, w, h: As String caption
    End Type

    Dim As String result
    Dim As _Byte setup
    If setup = 0 Then
        ReDim ctl(100) As newControl
        setup = -1
    End If

    Select Case getAction$(action$)
        Case "reset"
            Print "reset"
            ReDim ctl(100) As newControl
        Case "add"
            Print "add"
            Print getParam$(action$, "type")
            Print getParam$(action$, "name")
            Print getParam$(action$, "parent")
            Print getParam$(action$, "caption")
            result = getParam$(action$, "metrics")
            If isGroup(result) Then
                Print "group: "; result
            Else
                Print result
            End If
        Case "update"
            Print "update"
        Case "clicked"
            Print "clicked"
        Case Else
            Print "unknown verb: "; getAction$(action$)
    End Select

End Function

Function isGroup& (__s$)
    isGroup& = Left$(__s$, 1) = "{" And Right$(__s$, 1) = "}"
End Function

Function getAction$ (__a$)
    Dim As Long position
    Dim As String result

    position = InStr(__a$, ";")
    If position = 0 Then
        getAction$ = __a$
    Else
        result = LCase$(Left$(__a$, position - 1))
        If InStr(result, "=") > 0 Then Exit Function
        getAction$ = result
    End If
End Function

Function getParam$ (__s$, __p$)
    Dim s$, p$, os$, result$
    Dim As Long position

    os$ = ";" + __s$ + ";"
    s$ = LCase$(os$)
    p$ = ";" + LCase$(__p$) + "="

    position = InStr(s$, p$)
    If position = 0 Then Exit Function

    result$ = Mid$(os$, position + Len(p$))
    getParam$ = Left$(result$, InStr(result$, ";") - 1)
End Function

