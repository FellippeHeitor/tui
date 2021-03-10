Option _Explicit
Dim As Long result
Dim As Long form, button1, button2, check1, label1

result = tui("set highintensity=true")
result = tui("set defaults;fg=0;bg=7;fghover=16;bghover=2")
form = tui("add type=form;name=form1;caption=Hello, world!;align=center;w=50;h=8")

result = tui("set defaults;parent=form1")
check1 = tui("add type=checkbox;value=-1;name=check1;caption=I'm set!;x=2;y=2;fghover=-1;bghover=-1")
label1 = tui("add type=label;name=label1;caption=Nothing to show;x=2;y=3;bghover=-1")
button1 = tui("add type=button;name=button1;caption=Click me;align=center;y=5;w=20;fg=31;bg=9")
button2 = tui("add type=button;name=button2;caption=Close;x=40;y=5;fg=31;bg=8")

result = tui("set focus;control=check1")

Do
    Color 31, 0
    Cls
    If tui("clicked") Then
        Select Case tui("control")
            Case button1
                If tui("get control=check1;value") Then
                    result = tui("set control=label1;caption=The box is checked.;fg=-1")
                Else
                    result = tui("set control=label1;caption=The box is unchecked.;fg=-1")
                End If
            Case button2
                System
            Case label1
                result = tui("set control=label1;caption=This is not a button!;fg=4;fghover=20")
            Case check1
                If tui("get control=check1;value") Then
                    result = tui("set control=check1;caption=I'm set!")
                Else
                    result = tui("set control=check1;caption=I'm not set...")
                End If
        End Select
    End If
    _Display
    _Limit 30
Loop

Function tui& (action As String) Static
    Type newControl
        As Long type, parent, x, y, w, h, value
        As Integer fg, bg, fghover, bghover
        As String name, caption, text
        As _Byte shadow
    End Type

    Dim As String result, temp
    Dim As Long i, j, totalControls, this, k
    Dim As Long x, y, mx, my, mb, hover, mouseDownOn, clicked, focus
    Dim As Long mouseDownX, mouseDownY
    Dim As Integer prevFG, prevBG
    Dim As _Byte setup, mouseDown, fetchMouse, showFocus, fetchedKeyboard
    Dim As _Byte draggingForm, highIntensity, captionSet

    If setup = 0 Then
        ReDim control(100) As newControl
        Dim defaults As newControl
        defaults.shadow = -1
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

            control(this) = defaults

            If passed(action, "type") Then control(this).type = controlType(getParam(action, "type"))
            If passed(action, "name") Then control(this).name = getParam(action, "name")
            If passed(action, "parent") Then
                temp = getParam(action, "parent")
                GoSub getParentID
                control(i).parent = j
            End If
            If passed(action, "shadow") Then control(this).shadow = (LCase$(getParam(action, "shadow")) = "true")
            If passed(action, "caption") Then
                temp = getParam(action, "caption")
                control(this).caption = temp
                If control(this).type = controlType("form") Then
                    control(this).caption = " " + control(this).caption + " "
                End If
            End If
            If passed(action, "text") Then control(this).text = getParam(action, "text")

            If passed(action, "w") Then
                temp = getParam(action, "w")
                If temp = "auto" Then
                    GoSub setAutoWidth
                ElseIf Val(temp) > 0 Then
                    control(this).w = Val(temp)
                End If
            Else
                GoSub setAutoWidth
            End If

            If passed(action, "h") Then
                control(this).h = Val(getParam(action, "h"))
            Else
                control(this).h = 1
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

            If passed(action, "x") Then control(this).x = Val(getParam(action, "x"))
            If passed(action, "y") Then control(this).y = Val(getParam(action, "y"))

            result = getParam(action, "color")
            If result = "inherit" And control(this).parent > 0 Then
                control(this).fg = control(control(this).parent).fg
                control(this).bg = control(control(this).parent).bg
                control(this).fghover = control(control(this).parent).fghover
                control(this).bghover = control(control(this).parent).bghover
            End If

            If passed(action, "fg") Then control(this).fg = Val(getParam(action, "fg"))
            If passed(action, "bg") Then control(this).bg = Val(getParam(action, "bg"))
            If passed(action, "fghover") Then control(this).fghover = Val(getParam(action, "fghover"))
            If passed(action, "bghover") Then control(this).bghover = Val(getParam(action, "bghover"))

            If passed(action, "value") Then control(this).value = Val(getParam(action, "value"))

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
                    Select Case control(i).type
                        Case controlType("form")
                        Case Else
                            If control(i).fghover > -1 Then Color control(i).fghover
                            If control(i).bghover > -1 Then Color , control(i).bghover
                    End Select
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
                        If control(i).shadow And ((focus = i And _KeyDown(32)) Or (mouseDownOn = i And hover = i)) Then
                            x = x + 1
                        End If
                        _PrintString (x, y), Space$(control(i).w)
                        _PrintString (x + (control(i).w - Len(control(i).caption)) \ 2, y), control(i).caption
                        If control(i).shadow And (hover <> i Or (hover = i And mouseDownOn <> i)) And (focus <> i Or (focus = i And _KeyDown(32) = 0)) Then
                            If control(i).parent > 0 Then
                                Color 0, control(control(i).parent).bg
                            Else
                                Color 0, prevBG
                            End If
                            _PrintString (x + control(i).w, y), Chr$(220)
                            _PrintString (x + 1, y + 1), String$(control(i).w, 223)
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
            Color prevFG, prevBG

            Select Case k
                Case -9
                    Do
                        focus = focus + 1
                        If focus > totalControls Then focus = 1
                    Loop While control(focus).type = controlType("form") Or control(focus).type = controlType("label")
                Case -13
                    Select Case control(focus).type
                        Case controlType("button")
                            clicked = focus
                    End Select
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
                        focus = hover
                    End If
                    mouseDownX = mx
                    mouseDownY = my
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

            For i = 1 To 2
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
                Case "fghover": tui& = control(this).fghover
                Case "bghover": tui& = control(this).bghover
                Case "name": action = control(this).name
                Case "caption": action = control(this).caption
                Case "text": action = control(this).text
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
                    Case "highintensity"
                        highIntensity = (LCase$(result) = "true")
                        If highIntensity Then _Blink Off Else _Blink On
                    Case "focus"
                        temp = getParam(action, "control")
                        GoSub getControlID
                        focus = this
                    Case "defaults"
                        temp = getParam(action, "parent")
                        If Len(temp) Then
                            GoSub getParentID
                            defaults.parent = j
                        End If

                        If passed(action, "w") Then defaults.w = Val(getParam(action, "w"))
                        If passed(action, "h") Then defaults.h = Val(getParam(action, "h"))
                        If passed(action, "x") Then defaults.x = Val(getParam(action, "x"))
                        If passed(action, "y") Then defaults.y = Val(getParam(action, "y"))
                        If passed(action, "value") Then defaults.value = Val(getParam(action, "value"))

                        If passed(action, "fg") Then defaults.fg = Val(getParam(action, "fg"))
                        If passed(action, "bg") Then defaults.bg = Val(getParam(action, "bg"))
                        If passed(action, "fghover") Then defaults.fghover = Val(getParam(action, "fghover"))
                        If passed(action, "bghover") Then defaults.bghover = Val(getParam(action, "bghover"))

                        If passed(action, "shadow") Then defaults.shadow = (LCase$(getParam(action, "shadow")) = "true")
                    Case "control"
                        temp = getParam(action, temp)
                        GoSub getControlID

                        captionSet = 0
                        If passed(action, "caption") Then
                            control(this).caption = getParam(action, "caption")
                            captionSet = -1
                        End If

                        If passed(action, "text") Then
                            control(this).text = getParam(action, "text")
                        End If

                        If passed(action, "w") Then
                            control(this).w = Val(getParam(action, "w"))
                        ElseIf captionSet Then
                            Select Case control(this).type
                                Case controlType("button")
                                    control(this).w = Len(control(this).caption) + 2
                                Case controlType("checkbox")
                                    control(this).w = Len(control(this).caption) + 4
                            End Select
                        End If

                        If passed(action, "h") Then control(this).h = Val(getParam(action, "h"))
                        If passed(action, "x") Then control(this).x = Val(getParam(action, "x"))
                        If passed(action, "y") Then control(this).y = Val(getParam(action, "y"))

                        result = getParam(action, "color")
                        If result = "inherit" And control(this).parent > 0 Then
                            control(this).fg = control(control(this).parent).fg
                            control(this).bg = control(control(this).parent).bg
                            control(this).bghover = control(control(this).parent).bghover
                        End If

                        If passed(action, "fg") Then control(this).fg = Val(getParam(action, "fg"))
                        If passed(action, "bg") Then control(this).bg = Val(getParam(action, "bg"))
                        If passed(action, "fghover") Then control(this).fghover = Val(getParam(action, "fghover"))
                        If passed(action, "bghover") Then control(this).bghover = Val(getParam(action, "bghover"))

                        If passed(action, "value") Then control(this).value = Val(getParam(action, "value"))

                        If passed(action, "shadow") Then control(this).shadow = (LCase$(getParam(action, "shadow")) = "true")
                End Select
            Loop
        Case Else
            Cls
            Print "unknown action: "; getAction(action)
            End
    End Select

    Exit Function

    getParentID:
    'temp contains the name of the parent control
    For j = 1 To totalControls
        If control(j).name = temp Then
            Return
        End If
    Next
    j = 0
    Return

    getControlID:
    'temp contains the name of the control we're looking for
    this = 0
    For j = 1 To totalControls
        If control(j).name = temp Then
            this = j
            Return
        End If
    Next
    Return

    setAutoWidth:
    control(this).w = Len(control(this).caption) + 2
    If control(this).type = controlType("checkbox") Then
        control(this).w = control(this).w + 2
    End If
    Return
End Function

Function controlType& (__a$)
    Dim typeList$
    typeList$ = "@form@button@checkbox@label@textbox@"

    controlType& = InStr(typeList$, LCase$("@" + __a$ + "@"))
End Function

Function getAction$ (__a$)
    Dim As Long position
    Dim As String result, sep

    sep = " "
    position = InStr(__a$, sep)
    If position = 0 Then
        getAction$ = __a$
        __a$ = ""
    Else
        result = LCase$(Left$(__a$, position - 1))
        If InStr(result, "=") > 0 Then Exit Function
        __a$ = Mid$(__a$, position + 1)
        getAction$ = result
    End If
End Function

Function passed%% (__action$, __parameter$)
    Dim As String s, p, os, sep
    Dim As Long position

    sep = ";"
    os = sep + __action$ + sep
    s = LCase$(os)
    p = sep + LCase$(__parameter$) + "="

    position = _InStrRev(s, p)
    passed%% = position > 0
End Function

Function getParam$ (__action$, __parameter$)
    Dim As String s, p, os, result, sep
    Dim As Long position

    sep = ";"
    os = sep + __action$ + sep
    s = LCase$(os)
    p = sep + LCase$(__parameter$) + "="

    position = _InStrRev(s, p)
    If position = 0 Then Exit Function

    result = Mid$(os, position + Len(p))
    getParam$ = _Trim$(Left$(result, InStr(result, sep) - 1))
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
