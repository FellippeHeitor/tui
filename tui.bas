Option _Explicit
Dim result As Long

result = tui("reset")
result = tui("set;color={fore=0,back=7}")
result = tui("add;type=form;name=form1;caption=Hello, world!;metrics={special=center,w=50,h=10}")
result = tui("add;type=button;name=button1;parent=form1;caption=Click me;metrics={x=10,y=20,w=auto,h=1}")

Do
    Cls
    result = tui("update")
    If tui("clicked") Then
    End If
    _Limit 30
    _Display
Loop

Function tui& (action$) Static
    Type newControl
        As Long type, parent, x, y, w, h
        As String name, caption, special
    End Type

    Dim As String result, temp
    Dim As Long i, totalControls, this
    Dim As _Byte setup

    If setup = 0 Then
        ReDim control(100) As newControl
        setup = -1
    End If

    Select Case getAction$(action$)
        Case "reset"
            totalControls = 0
        Case "add"
            totalControls = totalControls + 1
            this = totalControls
            If totalControls > UBound(control) Then
                ReDim _Preserve control(UBound(control) + 100) As newControl
            End If

            control(this).type = ctlType(getParam$(action$, "type"))
            control(this).name = getParam$(action$, "name")
            control(this).parent = Val(getParam$(action$, "parent"))
            control(this).caption = getParam$(action$, "caption")

            result = Ungroup$(getParam$(action$, "metrics"))
            temp = getParam$(result, "w")
            If temp = "auto" Then
            ElseIf Val(temp) > 0 Then
                control(this).w = Val(temp)
            End If
            control(this).h = Val(getParam$(result, "h"))
            control(this).x = Val(getParam$(result, "x"))
            control(this).y = Val(getParam$(result, "y"))
            result = getParam$(result, "special")
            Select Case result
                Case "center"
                    control(this).x = (_Width - control(this).w) \ 2
                    control(this).y = (_Height - control(this).h) \ 2
                Case Else
                    control(this).special = result
            End Select
        Case "set"
            'result = tui("set;color={fore=0,back=7}")
            result = getNextParam$(action$)
            While Len(result)
                result = getNextParam$(action$)
            Wend
        Case "update"
            For i = 1 To totalControls
                Select Case control(i).type
                    Case ctlType("form")
                        boxShadow control(i).x, control(i).y, control(i).w, control(i).h
                    Case ctlType("button")
                        _PrintString (control(i).x, control(i).y), control(i).caption
                End Select
            Next
        Case "clicked"
            tui& = 0
        Case Else
            Cls
            Print "unknown verb: "; getAction$(action$)
            End
    End Select

End Function

Function ctlType& (__a$)
    Dim typeList$
    typeList$ = "@form@button@"

    ctlType& = InStr(typeList$, LCase$("@" + __a$ + "@"))
End Function

Function Ungroup$ (__s$)
    Dim As String result
    Dim As Long position

    result = Mid$(__s$, 2, Len(__s$) - 2)
    position = InStr(result, ",")
    While position
        Mid$(result, position, 1) = ";"
        position = InStr(result, ",")
    Wend
    Ungroup$ = result
End Function

Function getAction$ (__a$)
    Dim As Long position
    Dim As String result, sep

    sep = ";"
    position = InStr(__a$, sep)
    If position = 0 Then
        getAction$ = __a$
    Else
        result = LCase$(Left$(__a$, position - 1))
        If InStr(result, "=") > 0 Then Exit Function
        getAction$ = result
    End If
End Function

Function getParam$ (__action$, __parameter$)
    Dim As String s, p, os, result, sep
    Dim As Long position

    sep = ";"
    os = sep + __action$ + sep
    s = LCase$(os)
    p = sep + LCase$(__parameter$) + "="

    position = InStr(s$, p$)
    If position = 0 Then Exit Function

    result$ = Mid$(os$, position + Len(p$))
    getParam$ = Left$(result$, InStr(result$, sep$) - 1)
End Function

Function getNextParam$ (__action$) Static
    Dim As String previousAction
    Dim As Long position

    If __action$ <> previousAction Then
        previousAction = __action$
        position = 0
    End If
End Function


Sub box (x As Long, y As Long, w As Long, h As Long)
    Dim As Long y2

    _PrintString (x, y), Chr$(218) + String$(w - 2, 196) + Chr$(191)
    For y2 = y + 1 To y + h - 2
        _PrintString (x, y2), Chr$(179) + Space$(w - 2) + Chr$(179)
    Next
    _PrintString (x, y + h - 1), Chr$(192) + String$(w - 2, 196) + Chr$(217)
End Sub

Sub boxShadow (x As Long, y As Long, w As Long, h As Long)
    box x, y, w, h

    Dim As Long y2, x2

    'shadow
    Color 8, 0
    For y2 = y + 1 To y + h - 1
        For x2 = x + w To x + w + 1
            If x2 <= _Width And y2 <= _Height Then
                _PrintString (x2, y2), Chr$(Screen(y2, x2))
            End If
        Next
    Next

    y2 = y + h
    If y2 <= _Height Then
        For x2 = x + 2 To x + w + 1
            If x2 <= _Width Then
                _PrintString (x2, y2), Chr$(Screen(y2, x2))
            End If
        Next
    End If
End Sub
