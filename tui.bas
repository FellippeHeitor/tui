Option _Explicit
Dim As Long result
Dim As Long form, button1, button2, check1, label1

form = tui("add;type=form;shadow=true;name=form1;caption=Hello, world!;align=center;w=50;h=12;fg=0;bg=15")
button1 = tui("add;type=button;shadow=true;parent=form1;name=button1;caption=Click me;align=center;fg=0;bg=7;hoverfg=15;hoverbg=2")
check1 = tui("add;type=checkbox;parent=form1;name=check1;caption=Check me;x=10;y=10;color=inherit")
label1 = tui("add;type=label;parent=form1;name=label1;caption=Nothing to show;align=center;y=8;fg=7;bg=8")
button2 = tui("add;shadow=true;parent=form1;type=button;name=button2;caption=Close;x=40;y=9;fg=0;bg=7;hoverfg=15;hoverbg=2")

Do
    Color , 1
    Cls
    If tui("clicked") Then
        Select Case tui("control")
            Case button1
                If tui("get;control=check1;value") Then
                    result = tui("set;control=label1;caption=The box is checked.;fg=7")
                Else
                    result = tui("set;control=label1;caption=The box is unchecked.;fg=7")
                End If
            Case button2
                System
            Case label1
                result = tui("set;control=label1;caption=This is not a button!;fg=4")
            Case check1
                If tui("get;control=check1;value") Then
                    result = tui("set;control=check1;caption=I'm set!")
                Else
                    result = tui("set;control=check1;caption=I'm not set...")
                End If
        End Select
    End If
    _Display
    _Limit 30
Loop

Function tui& (action As String) Static
    Type newControl
        As Long type, parent, x, y, w, h, value
        As Integer fg, bg, hoverfg, hoverbg
        As String name, caption
        As _Byte shadow
    End Type

    Dim As String result, temp
    Dim As Long i, j, totalControls, this, k
    Dim As Long x, y, mx, my, mb, hover, mouseDownOn, clicked, focus
    Dim As Long mouseDownX, mouseDownY
    Dim As Integer prevFG, prevBG
    Dim As _Byte setup, mouseDown, fetchMouse, showFocus, fetchedKeyboard
    Dim As _Byte draggingForm

    If setup = 0 Then
        ReDim control(100) As newControl
        fetchMouse = -1
        showFocus = -1
        setup = -1
    End If

    Select Case getAction$(action)
        Case "reset"
            totalControls = 0
        Case "add"
            totalControls = totalControls + 1
            this = totalControls
            If totalControls > UBound(control) Then
                ReDim _Preserve control(UBound(control) + 100) As newControl
            End If

            control(this).type = controlType(getParam(action, "type"))
            control(this).name = getParam(action, "name")
            temp = getParam(action, "parent")
            If Len(temp) Then GoSub addParent

            control(this).shadow = (getParam(action, "shadow") = "true")

            control(this).caption = getParam(action, "caption")
            If control(this).type = controlType("form") Then
                control(this).caption = " " + control(this).caption + " "
            End If

            temp = getParam(action, "w")
            If Len(temp) = 0 Then
                control(this).w = Len(control(this).caption) + 2
                If control(this).type = controlType("checkbox") Then
                    control(this).w = control(this).w + 2
                End If
            ElseIf Val(temp) > 0 Then
                control(this).w = Val(temp)
            End If
            temp = getParam(action, "h")
            If Len(temp) = 0 Then
                control(this).h = 1
            ElseIf Val(temp) > 0 Then
                control(this).h = Val(temp)
            End If

            result = getParam(action, "align")
            Select Case result
                Case "center"
                    If control(this).parent = 0 Then
                        control(this).x = (_Width - control(this).w) \ 2
                        control(this).y = (_Height - control(this).h) \ 2
                    Else
                        control(this).x = (control(control(this).parent).w - control(this).w) \ 2
                        control(this).y = (control(control(this).parent).h - control(this).h) \ 2
                    End If
            End Select

            temp = getParam(action, "x")
            If Len(temp) > 0 Then control(this).x = Val(temp)
            temp = getParam(action, "y")
            If Len(temp) > 0 Then control(this).y = Val(temp)

            result = getParam(action, "color")
            If result = "" Then result = "inherit"
            If result = "inherit" And control(this).parent > 0 Then
                control(this).fg = control(control(this).parent).fg
                control(this).bg = control(control(this).parent).bg
                control(this).hoverfg = control(control(this).parent).hoverfg
                control(this).hoverbg = control(control(this).parent).hoverbg
            End If

            control(this).fg = paramVal(getParam(action, "fg"))
            control(this).hoverfg = paramVal(getParam(action, "hoverfg"))
            control(this).bg = paramVal(getParam(action, "bg"))
            If control(this).bg > 7 Then
                'enable high intensity bg colors
                control(this).fg = control(this).fg + 16
                control(this).bg = control(this).bg - 8
                _Blink Off
            End If
            control(this).hoverbg = paramVal(getParam(action, "hoverbg"))
            If control(this).hoverbg > 7 Then
                'enable high intensity bg colors
                control(this).hoverfg = control(this).hoverfg + 16
                control(this).hoverbg = control(this).hoverbg - 8
                _Blink Off
            End If

            tui& = this
        Case "clicked"
            If fetchMouse Then While _MouseInput: Wend
            mx = _MouseX
            my = _MouseY
            mb = _MouseButton(1)
            hover = 0
            fetchedKeyboard = 0
            prevFG = _DefaultColor
            prevBG = _BackgroundColor
            For i = 1 To totalControls
                x = control(i).x + control(control(i).parent).x
                y = control(i).y + control(control(i).parent).y

                If control(i).parent > 0 Then
                    Color control(control(i).parent).fg, control(control(i).parent).bg
                End If

                If control(i).fg > -1 Then Color control(i).fg
                If control(i).bg > -1 Then Color , control(i).bg

                If mx >= x And mx <= x + control(i).w - 1 And my >= y And my <= y + control(i).h - 1 Then
                    hover = i
                    If control(i).hoverfg > -1 Then Color control(i).hoverfg
                    If control(i).hoverbg > -1 Then Color , control(i).hoverbg
                End If

                Select Case control(i).type
                    Case controlType("form")
                        If control(i).shadow Then
                            boxShadow control(i).x, control(i).y, control(i).w, control(i).h
                        Else
                            box control(i).x, control(i).y, control(i).w, control(i).h
                        End If
                        If Len(control(i).caption) Then
                            If (hover = i And my = control(i).y) Or draggingForm Then
                                Color 15, 0
                            Else
                                If control(i).fg > -1 Then Color control(i).fg
                                If control(i).bg > -1 Then Color , control(i).bg
                            End If

                            _PrintString (control(i).x + (control(i).w - Len(control(i).caption)) \ 2, control(i).y), control(i).caption
                        End If
                        If focus = i Then Locate , , 0
                        showFocus = -1 'if a form is up, focus is always shown
                        k = _KeyHit 'read keyboard input if a form is up
                        fetchedKeyboard = -1
                    Case controlType("button")
                        If mouseDownOn = i And hover = i And control(i).shadow Then x = x + 1
                        _PrintString (x, y), " " + control(i).caption + " "
                        If control(i).shadow And (hover <> i Or (hover = i And mouseDownOn <> i)) Then
                            If _Blink Then j = 0 Else j = 16
                            If control(i).parent > 0 Then
                                Color j, control(control(i).parent).bg
                            Else
                                Color j, prevBG
                            End If
                            _PrintString (x + Len(control(i).caption) + 2, y), Chr$(220)
                            _PrintString (x + 1, y + 1), String$(Len(control(i).caption) + 2, 223)
                        End If
                        If showFocus And focus = i Then Locate y, x + 1, 1
                    Case controlType("checkbox")
                        If control(i).value Then
                            temp = "[X] "
                        Else
                            temp = "[ ] "
                        End If
                        _PrintString (x, y), temp + control(i).caption
                        If showFocus And focus = i Then Locate y, x + 1, 1
                    Case controlType("label")
                        _PrintString (x, y), control(i).caption
                    Case controlType("textbox")
                        If focus = i And fetchedKeyboard = 0 Then
                            k = _KeyHit 'read keyboard input for textbox control
                        End If
                End Select
            Next

            Select Case k
                Case -9
                    Do
                        focus = focus + 1
                        If focus > totalControls Then focus = 1
                    Loop While control(focus).type = controlType("form") Or control(focus).type = controlType("label")
                Case -32
                    Select Case control(focus).type
                        Case controlType("button")
                            clicked = focus
                        Case controlType("checkbox")
                            control(focus).value = Not control(focus).value
                            clicked = focus
                    End Select
            End Select

            If mb Then
                If mouseDown Then
                    'drag
                    If draggingForm Then
                        control(mouseDownOn).x = control(mouseDownOn).x - (mouseDownX - mx)
                        control(mouseDownOn).y = control(mouseDownOn).y - (mouseDownY - my)
                        If control(mouseDownOn).x < 1 Then control(mouseDownOn).x = 1
                        If control(mouseDownOn).y < 1 Then control(mouseDownOn).y = 1
                        If control(mouseDownOn).x + control(mouseDownOn).w > _Width Then control(mouseDownOn).x = _Width - control(mouseDownOn).w + 1
                        If control(mouseDownOn).y + control(mouseDownOn).h > _Height Then control(mouseDownOn).y = _Height - control(mouseDownOn).h + 1
                        mouseDownX = mx
                        mouseDownY = my
                    End If
                Else
                    mouseDown = -1
                    mouseDownOn = hover
                    If control(mouseDownOn).type = controlType("form") Then
                        If my = control(mouseDownOn).y Then draggingForm = -1
                    Else
                        draggingForm = 0
                    End If
                    mouseDownX = mx
                    mouseDownY = my
                    focus = hover
                End If
            Else
                If mouseDown Then
                    If mouseDownOn > 0 And mouseDownOn = hover Then
                        clicked = mouseDownOn
                        focus = clicked
                        If control(clicked).type = controlType("checkbox") Then
                            control(clicked).value = Not control(clicked).value
                        End If
                    ElseIf mouseDownOn = 0 Then
                        focus = 0
                    End If
                End If
                mouseDown = 0
                mouseDownOn = 0
                draggingForm = 0
            End If

            tui& = clicked > 0
        Case "control"
            tui& = clicked
            clicked = 0
        Case "get"
            temp = getParam(action, "control")
            GoSub getControlID

            For i = 1 To 3
                temp = getNextParam(action)
            Next

            Select Case temp
                Case "parent": tui& = control(this).parent
                Case "x": tui& = control(this).x
                Case "y": tui& = control(this).y
                Case "w": tui& = control(this).w
                Case "h": tui& = control(this).h
                Case "value": tui& = control(this).value
                Case "fg": tui& = control(this).fg
                Case "bg": tui& = control(this).bg
                Case "hoverbg": tui& = control(this).hoverbg
                Case "name": action = control(this).name
                Case "caption": action = control(this).caption
            End Select
        Case "set"
            Do
                temp = getNextParam(action)
                If Len(temp) = 0 Then Exit Do
                result = getParam(action, temp)
                Select Case temp
                    Case "fetchmouse"
                        fetchMouse = (LCase$(result) = "true")
                    Case "showfocus"
                        showFocus = (LCase$(result) = "true")
                    Case "control"
                        temp = getParam(action, temp)
                        GoSub getControlID

                        result = getParam(action, "caption")
                        If Len(result) > 0 Then control(this).caption = result

                        temp = getParam(action, "w")
                        If Val(temp) > 0 Then control(this).w = Val(temp)
                        If Len(temp) = 0 Then
                            Select EveryCase control(this).type
                                Case controlType("button")
                                    control(this).w = Len(control(this).caption) + 2
                                Case controlType("checkbox")
                                    control(this).w = Len(control(this).caption) + 4
                            End Select
                        End If

                        temp = getParam(action, "h")
                        If Val(temp) > 0 Then control(this).h = Val(temp)
                        temp = getParam(action, "x")
                        If Val(temp) > 0 Then control(this).x = Val(temp)
                        temp = getParam(action, "y")
                        If Val(temp) > 0 Then control(this).y = Val(temp)

                        result = getParam(action, "color")
                        If result = "inherit" And control(this).parent > 0 Then
                            control(this).fg = control(control(this).parent).fg
                            control(this).bg = control(control(this).parent).bg
                            control(this).hoverbg = control(control(this).parent).hoverbg
                        End If

                        temp = getParam(action, "fg")
                        If Len(temp) > 0 Then control(this).fg = Val(temp)
                        temp = getParam(action, "bg")
                        If Len(temp) > 0 Then control(this).bg = Val(temp)
                        temp = getParam(action, "hoverbg")
                        If Len(temp) > 0 Then control(this).hoverbg = Val(temp)

                        If control(this).bg > 7 Then
                            'enable high intensity bg colors
                            control(this).fg = control(this).fg + 16
                            control(this).bg = control(this).bg - 8
                            _Blink Off
                        End If

                        If control(this).hoverbg > 7 Then
                            'enable high intensity bg colors
                            control(this).hoverfg = control(this).hoverfg + 16
                            control(this).hoverbg = control(this).hoverbg - 8
                            _Blink Off
                        End If
                End Select
            Loop
        Case Else
            Cls
            Print "unknown action: "; getAction(action)
            End
    End Select

    Exit Function

    addParent:
    'temp contains the name of the parent control
    For i = 1 To totalControls
        If control(i).name = temp Then
            control(this).parent = i
            Return
        End If
    Next
    Return

    getControlID:
    'temp contains the name of the control we're looking for
    this = 0
    For i = 1 To totalControls
        If control(i).name = temp Then
            this = i
            Return
        End If
    Next
    Return

End Function

Function controlType& (__a$)
    Dim typeList$
    typeList$ = "@form@button@checkbox@label@"

    controlType& = InStr(typeList$, LCase$("@" + __a$ + "@"))
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

    position = _InStrRev(s$, p$)
    If position = 0 Then Exit Function

    result$ = Mid$(os$, position + Len(p$))
    getParam$ = _Trim$(Left$(result$, InStr(result$, sep$) - 1))
End Function

Function paramVal& (param$)
    If Len(param$) = 0 Then
        paramVal& = -1
    Else
        paramVal& = Val(param$)
    End If
End Function

Function getNextParam$ (__action$) Static
    Dim As String lastAction, thisAction, sep, temp
    Dim As Long position, prevPosition, findEqual

    sep = ";"

    If __action$ <> lastAction Then
        lastAction = __action$
        thisAction = sep + __action$ + sep
        position = 1
    End If

    prevPosition = position
    position = InStr(prevPosition + 1, thisAction, sep)
    If position Then
        temp = Mid$(thisAction, prevPosition + 1, position - prevPosition - 1)
        findEqual = InStr(temp, "=")
        If findEqual Then
            getNextParam$ = Left$(temp, findEqual - 1)
        Else
            getNextParam$ = temp
        End If
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
