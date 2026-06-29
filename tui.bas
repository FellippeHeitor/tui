$Debug
Option _Explicit
On Error GoTo oops

$Resize:On
Dim As String tempStr
Dim As Long form, closebutton, button1, check1, label1, label2, label3, textbox1, textbox2, textarea1, listbox1
Dim As Long filemenu, filemenunew, filemenuexit
Dim As Long editmenu, editmenuundo, editmenuredo, editmenuproperties
Dim As Long viewmenu, viewmenusubs, viewmenulinenumbers, viewmenuwarnings
Dim As Long viewmenulinenumbersshowhide, viewmenulinenumbersshowbackground, viewmenulinenumbersshowseparator
Dim As Long statusbar

tui "set highintensity=true"
statusbar = tui("add type=label;name=statusbar;caption= Ready.;x=1;y=25;w=80;h=1;fg=0;bg=3")

tui "set defaults;fg=0;bg=7;fghover=7;bghover=0;fghotkey=15"

filemenu = tui("add type=menubar;parent=0;name=filemenu;caption=&File")
tui "set defaults;parent=filemenu"
filemenunew = tui("add type=menuitem;name=filemenunew;caption=&New  Ctrl+N")
tui "add type=menuitem;caption=-"
filemenuexit = tui("add type=menuitem;name=filemenuexit;caption=E&xit")

editmenu = tui("add type=menubar;parent=0;name=editmenu;caption=&Edit")
tui "set defaults;parent=editmenu"
editmenuundo = tui("add type=menuitem;name=editmenuundo;caption=&Undo  Ctrl+Z")
editmenuredo = tui("add type=menuitem;name=editmenuredo;caption=&Redo  Ctrl+Y;disabled=true")
tui "add type=menuitem;caption=-"
editmenuproperties = tui("add type=menuitem;name=editmenuproperties;caption=&Properties...")

viewmenu = tui("add type=menubar;parent=0;name=viewmenu;caption=&View")
tui "set defaults;parent=viewmenu"
viewmenusubs = tui("add type=menuitem;name=viewmenusubs;caption=&SUBs...  F2")
viewmenulinenumbers = tui("add type=menuitem;name=viewmenulinenumbers;caption=&Line Numbers;special=submenu")
tui "add type=menuitem;caption=-"
viewmenuwarnings = tui("add type=menuitem;name=viewmenuwarnings;caption=Compiler &Warnings...  Ctrl+W")

tui "set defaults;parent=viewmenulinenumbers"
viewmenulinenumbersshowhide = tui("add type=menuitem;name=viewmenulinenumbersshowhide;caption=&Show Line Numbers")
viewmenulinenumbersshowbackground = tui("add type=menuitem;name=viewmenulinenumbersshowbackground;caption=&Background Color;special=submenu")
viewmenulinenumbersshowseparator = tui("add type=menuitem;name=viewmenulinenumbersshowseparator;caption=Sho&w Separator")

tui "set defaults;parent=viewmenulinenumbersshowbackground"
tui "add type=menuitem;name=viewmenubgbright;caption=&Bright mode"
tui "add type=menuitem;name=viewmenubgdark;caption=&Dark side of the moon"

Dim As _Byte updateLabel
Dim As Long i
Dim As Integer newWidth, newHeight
Dim As _Byte willResize

updateLabel = -1
Do
    While _Resize
        newWidth = _ResizeWidth
        newHeight = _ResizeHeight
        willResize = -1
    Wend

    If willResize Then
        willResize = 0
        Width newWidth \ 8, newHeight \ 16
        tui "set control=statusbar;y=" + Str$(_Height) + ";w=" + Str$(_Width)
    End If

    Color 25, 0
    Cls
    For i = 1 To _Height
        _PrintString (1, i), String$(_Width, 176)
    Next

    If updateLabel Then
        If tui("get control=check1;value") Then
            tui "set control=label1;caption=The box is checked.;color=inherit"
        Else
            tui "set control=label1;caption=The box is unchecked.;color=inherit"
        End If
    End If

    tempStr = tuiGet$("get hover")
    tui "set control=label2;caption=Hover: " + tempStr + ";color=inherit"

    tempStr = tuiGet$("get focus")
    tui "set control=label3;caption=Focus: " + tempStr + ";color=inherit"

    If tui("clicked") Then
        tui "set control=statusbar;caption= Ready."
        Select Case tui("control")
            Case button1
                If tui("get control=editmenu;disabled") Then
                    tui "set control=editmenu;disabled=false"
                    tui "set control=statusbar;caption= Edit menu enabled."
                Else
                    tui "set control=editmenu;disabled=true"
                    tui "set control=statusbar;caption= Edit menu disabled."
                End If
            Case check1
                updateLabel = -1
            Case listbox1
                tempStr = tuiGet$("get control=listbox1;selected")
                tui "set control=statusbar;caption= Selected: " + tempStr
            Case filemenuexit
                System
            Case closebutton
                tui "delete control=form1"
                form = 0
            Case filemenunew
                '---------------------------------------
                If form <> 0 Then
                    tui "set control=statusbar;caption= Form already displayed."
                    Exit Case
                End If
                tui "set defaults;parent=0"
                form = tui("add type=form;name=form1;caption=Hello, world!;align=center;fghover=16;bghover=7;w=60;h=17")

                tui "set defaults;parent=form1"
                closebutton = tui("add type=button;name=closebutton;caption=[X];fg=20;fghover=28;y=0;align=top-right;shadow=false;canreceivefocus=false")
                check1 = tui("add type=checkbox;value=-1;name=check1;caption=&I'm a check box.;x=2;y=2")
                label1 = tui("add type=label;name=label1;caption=Nothing to show;x=2;y=3;bghover=-1;special=autosize")
                label2 = tui("add type=label;name=label2;caption=Hover:;x=2;y=4;bghover=-1;special=autosize")
                label3 = tui("add type=label;name=label3;caption=Focus:;x=2;y=5;bghover=-1;special=autosize")
                textbox1 = tui("add type=textbox;name=textbox1;text=Edit me...;x=2;y=6;w=44")
                textbox2 = tui("add type=textbox;name=textbox2;text=Custom colors;x=2;y=7;w=44;fg=18;bg=8;fghover=31;bghover=9")
                textarea1 = tui("add type=textarea;name=textarea1;text=Line 1 of text.\nLine 2 of text.\nLine 3 of text.;x=2;y=8;w=26;h=4;fg=15;bg=0;fghover=15;bghover=4")
                tui "add type=label;name=listboxlabel;caption=&List:;x=29;y=2;special=autosize"
                listbox1 = tui("add type=listbox;name=listbox1;text=Apple\nBanana\nCherry\n-\nDate\nElderberry\nFig\nGrape;x=29;y=3;w=18;h=7;value=1;fg=31;bg=9;fghover=0;bghover=7")
                button1 = tui("add type=button;name=button1;caption=Click &me;align=center;y=13;w=20;fg=31;bg=9;fghover=16;bghover=7")

                tui "set focus;control=check1"
                '---------------------------------------
            Case label1
                tui "set control=label1;caption=This is not a button!;fg=4;fghover=20"
                updateLabel = 0
            Case viewmenusubs
                tui "reset"
            Case Else
                tempStr = tuiGet$("get control=" + Str$(tui("control")) + ";name")
                tui "set control=statusbar;caption= This control has no action assigned to it: " + tempStr
        End Select
    End If
    _Display
    _Limit 30
Loop

oops:
Resume Next

' === WRAPPERS & METHODS ===

Sub tui (action As String)
    Dim As Long result
    result = tui&(action)
End Sub

Function tuiGet$ (action As String)
    Dim tempAction As String
    Dim result As Long
    tempAction = action
    result = tui&(tempAction)
    tuiGet$ = tempAction
End Function

Function tui& (action As String) Static
    Type newControl
        As Long type, parent, x, y, w, h, value, keybind, cursorPos, selectionStart, selectionEnd, scrollTop
        As Integer fg, bg, fghover, bghover, fghotkey, hotkeypos
        As _Byte hasFg, hasBg, hasFghover, hasBghover
        As String name, special, caption, text, hotkey
        As _Byte canReceiveFocus, active, disabled, hidden, shadow
    End Type

    Dim As String result, temp
    Dim As Long i, j, this, k, modalForm
    Dim As Long menuPanel(100), totalMenuPanels, totalMenuPanelItems
    Dim As String menuPanelParents
    Dim As Long x, y, mx, my, oldmx, oldmy, mb, hover, mouseDownOn, clicked, lastClickedControl, focus, prevFocus
    Dim As Long mouseDownX, mouseDownY, hotkeyX, hotkeyY, cursorX, displayStart, lineStart, lineEnd, lineNum, lineCount, row, listTextWidth
    Dim As Long selLo, selHi, visibleLeft, visibleRight, selectionPos, selectionLen, prevLineStart, prevLineLen, nextLineStart, nextLineEnd, nextLineLen
    Dim As Long textWidth, visualLine, totalVisualLines, targetVisualLine, scrollbarTop, scrollbarThumb, scrollbarTrack, mouseTextCol, segmentAdvance
    Dim As String listItemText
    Dim As Integer prevFG, prevBG
    Dim As _Byte setup, mouseDown, fetchMouse, showFocus, fetchedKeyboard
    Dim As _Byte draggingForm, highIntensity, captionSet, hasMenuBar
    Dim As _Byte keyboardControl, showHotKey, prevShowHotKey, willActivateMenu, shiftDown

    If setup = 0 Then
        ReDim control(100) As newControl
        Dim defaults As newControl
        defaults.shadow = -1
        defaults.fg = -1
        defaults.bg = -1
        defaults.fghover = -1
        defaults.bghover = -1
        fetchMouse = -1
        showFocus = -1
        hasMenuBar = 0
        setup = -1
    End If

    Select Case getAction$(action)
        Case "reset"
            ReDim control(100) As newControl
            modalForm = 0
            hasMenuBar = 0
        Case "add"
            this = 0
            For i = 1 To UBound(control)
                If control(i).active = 0 Then this = i: Exit For
            Next

            If this = 0 And i > UBound(control) Then
                ReDim _Preserve control(UBound(control) + 100) As newControl
                this = i
            End If

            control(this) = defaults

            If passed(action, "type") Then control(this).type = controlType(getParam(action, "type"))
            Select Case getParam(action, "type")
                Case "button", "checkbox", "textbox", "textarea", "listbox"
                    control(this).canReceiveFocus = -1
            End Select

            If passed(action, "name") Then control(this).name = getParam(action, "name")
            If passed(action, "parent") Then
                temp = getParam(action, "parent")
                GoSub getParentID
                control(i).parent = j
            End If
            If passed(action, "shadow") Then control(this).shadow = (LCase$(getParam(action, "shadow")) = "true")
            If passed(action, "canreceivefocus") Then control(this).canReceiveFocus = (LCase$(getParam(action, "canreceivefocus")) = "true")
            If passed(action, "hidden") Then control(this).hidden = (LCase$(getParam(action, "hidden")) = "true")
            If passed(action, "disabled") Then control(this).disabled = (LCase$(getParam(action, "disabled")) = "true")
            If passed(action, "caption") Then
                temp = getParam(action, "caption")
                control(this).caption = temp
                If control(this).type <> controlType("form") Then
                    control(this).hotkeypos = InStr(control(this).caption, "&")
                    If control(this).hotkeypos Then
                        control(this).caption = Left$(control(this).caption, control(this).hotkeypos - 1) + Mid$(control(this).caption, control(this).hotkeypos + 1)
                        control(this).hotkey = Mid$(control(this).caption, control(this).hotkeypos, 1)
                    End If
                End If
            End If
            If passed(action, "text") Then
                temp = getParam(action, "text")
                temp = Replace$(temp, "\r\n", Chr$(10))
                temp = Replace$(temp, "\r", Chr$(10))
                temp = Replace$(temp, "\n", Chr$(10))
                control(this).text = temp
                If control(this).type = controlType("listbox") Then
                    control(this).scrollTop = 1
                Else
                    control(this).cursorPos = Len(control(this).text) + 1
                    control(this).selectionStart = control(this).cursorPos
                    control(this).selectionEnd = control(this).cursorPos
                    control(this).scrollTop = 1
                End If
            End If
            If control(this).type = controlType("textbox") Or control(this).type = controlType("textarea") Then
                If control(this).cursorPos < 1 Then control(this).cursorPos = 1
                If control(this).selectionStart < 1 Then control(this).selectionStart = 1
                If control(this).selectionEnd < 1 Then control(this).selectionEnd = 1
                If control(this).scrollTop < 1 Then control(this).scrollTop = 1
            End If

            If passed(action, "special") Then control(this).special = getParam(action, "special")
            Select Case control(this).special
                Case "autosize"
                    control(this).w = Len(control(this).caption)
            End Select

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
            ElseIf control(this).type = controlType("listbox") Then
                control(this).h = 7
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
                    While control(this).x < 1
                        control(this).x = control(this).x + 1
                    Wend
                    While control(this).y < 1
                        control(this).y = control(this).y + 1
                    Wend
                Case "bottom-center"
                    If control(this).parent = 0 Then
                        control(this).x = (_Width - control(this).w) \ 2
                        control(this).y = (_Height - control(this).h)
                    Else
                        control(this).x = (control(control(this).parent).w - control(this).w) \ 2
                        control(this).y = (control(control(this).parent).h - control(this).h) - 2
                    End If
                Case "bottom-right"
                    If control(this).parent = 0 Then
                        control(this).x = (_Width - control(this).w)
                        control(this).y = (_Height - control(this).h)
                    Else
                        control(this).x = (control(control(this).parent).w - control(this).w) - 2
                        control(this).y = (control(control(this).parent).h - control(this).h) - 2
                    End If
                Case "bottom-left"
                    control(this).x = 2
                    If control(this).parent = 0 Then
                        control(this).y = (_Height - control(this).h)
                    Else
                        control(this).y = (control(control(this).parent).h - control(this).h) - 2
                    End If
                Case "top-center"
                    control(this).y = 1
                    If control(this).parent = 0 Then
                        control(this).x = (_Width - control(this).w) \ 2
                    Else
                        control(this).x = (control(control(this).parent).w - control(this).w) \ 2
                    End If
                Case "top-right"
                    control(this).y = 1
                    If control(this).parent = 0 Then
                        control(this).x = (_Width - control(this).w)
                    Else
                        control(this).x = (control(control(this).parent).w - control(this).w) - 2
                    End If
                Case "top-left"
                    control(this).x = 2
                    control(this).y = 1
            End Select

            If passed(action, "x") Then control(this).x = Val(getParam(action, "x"))
            If passed(action, "y") Then control(this).y = Val(getParam(action, "y"))

            result = getParam(action, "color")
            If result = "inherit" And control(this).parent > 0 Then
                control(this).fg = control(control(this).parent).fg
                control(this).bg = control(control(this).parent).bg
                control(this).fghover = control(control(this).parent).fghover
                control(this).bghover = control(control(this).parent).bghover
                control(this).hasFg = -1
                control(this).hasBg = -1
                control(this).hasFghover = -1
                control(this).hasBghover = -1
            ElseIf result = "defaults" Then
                control(this).fg = defaults.fg
                control(this).bg = defaults.bg
                control(this).fghover = defaults.fghover
                control(this).bghover = defaults.bghover
                control(this).fghotkey = defaults.fghotkey
                control(this).hasFg = -1
                control(this).hasBg = -1
                control(this).hasFghover = -1
                control(this).hasBghover = -1
            End If

            If passed(action, "fg") Then control(this).fg = Val(getParam(action, "fg")): control(this).hasFg = -1
            If passed(action, "bg") Then control(this).bg = Val(getParam(action, "bg")): control(this).hasBg = -1
            If passed(action, "fghover") Then control(this).fghover = Val(getParam(action, "fghover")): control(this).hasFghover = -1
            If passed(action, "bghover") Then control(this).bghover = Val(getParam(action, "bghover")): control(this).hasBghover = -1

            If passed(action, "value") Then control(this).value = Val(getParam(action, "value"))
            If passed(action, "keybind") Then control(this).keybind = Val(getParam(action, "keybind"))

            If control(this).type = controlType("listbox") Then
                If control(this).scrollTop < 1 Then control(this).scrollTop = 1
                If control(this).value < 1 And Len(control(this).text) > 0 Then control(this).value = 1
            End If

            If control(this).type = controlType("menubar") Then
                If hasMenuBar = 0 Then
                    hasMenuBar = -1
                    Dim As Long lastMenuBarX, lastMenuBarLen
                    lastMenuBarX = 3
                End If
                control(this).y = 1
                control(this).x = lastMenuBarX + lastMenuBarLen
                lastMenuBarX = control(this).x
                lastMenuBarLen = Len(control(this).caption) + 2
            End If

            control(this).active = -1
            tui& = this
        Case "clicked"
            PCopy 0, 127
            Do
                PCopy 127, 0
                If fetchMouse Or (control(focus).type = controlType("menubar") Or control(menuPanel(totalMenuPanels)).active) Then
                    While _MouseInput: Wend
                End If
                mx = _MouseX
                my = _MouseY

                If keyboardControl Then
                    If mx <> oldmx Or my <> oldmy Then
                        keyboardControl = 0
                    End If
                End If

                mb = _MouseButton(1)
                clicked = 0
                hover = 0
                k = 0
                fetchedKeyboard = 0
                prevFG = _DefaultColor
                prevBG = _BackgroundColor
                showHotKey = _KeyDown(100308) Or _KeyDown(100307)

                If showHotKey Then
                    If prevShowHotKey = 0 Then
                        prevShowHotKey = -1
                        willActivateMenu = -1
                    End If
                Else
                    prevShowHotKey = 0
                    If willActivateMenu = -1 And modalForm = 0 Then
                        willActivateMenu = 0
                        If control(focus).type = controlType("menubar") Then
                            If control(prevFocus).type <> controlType("menubar") Then focus = prevFocus
                        ElseIf control(focus).type <> controlType("menupanel") And control(focus).type <> controlType("menuitem") Then
                            For i = 1 To UBound(control)
                                If control(i).type = controlType("menubar") Then
                                    If control(focus).type <> controlType("menubar") Then prevFocus = focus
                                    focus = i
                                    Exit For
                                End If
                            Next
                        ElseIf control(focus).type = controlType("menuitem") Then
                            GoSub closeMenuPanel
                            focus = control(menuPanel(totalMenuPanels)).parent
                        End If
                    End If
                End If

                For i = 1 To UBound(control)
                    If control(i).active = 0 Then _Continue

                    If modalForm > 0 Then
                        'modal forms and their controls are drawn exclusively
                        If control(i).type = controlType("form") And i <> modalForm Then _Continue
                        If control(i).type <> controlType("form") And control(i).parent <> modalForm Then _Continue
                    End If

                    Select Case control(i).type
                        Case controlType("menubar"), controlType("menuitem")
                            'deal with menus last
                            _Continue
                    End Select

                    x = 0
                    y = 0
                    hotkeyX = 0
                    hotkeyY = 0
                    this = i
                    Do
                        x = x + control(this).x
                        y = y + control(this).y
                        this = control(this).parent
                    Loop While this > 0

                    If control(i).parent > 0 Then
                        tuiSetColor control(control(i).parent).fg, control(control(i).parent).bg
                    End If

                    tuiSetColor control(i).fg, control(i).bg

                    If keyboardControl = 0 And mx >= x And mx <= x + control(i).w - 1 And my >= y And my <= y + control(i).h - 1 Then
                        If Not draggingForm And Not control(menuPanel(totalMenuPanels)).active Then
                            hover = i
                            Select Case control(i).type
                                Case controlType("form")
                                Case Else
                                    tuiSetColor control(i).fghover, control(i).bghover
                            End Select
                        End If
                    End If

                    Select Case control(i).type
                        Case controlType("form")
                            If control(i).shadow Then
                                boxShadow x, y, control(i).w, control(i).h
                            Else
                                box x, y, control(i).w, control(i).h
                            End If
                            If Len(control(i).caption) Then
                                tuiSetColor control(i).fghover, control(i).bghover
                                _PrintString (control(i).x, control(i).y), Space$(control(i).w)
                                _PrintString (x + (control(i).w - (Len(control(i).caption)) + 2) \ 2, y), " " + control(i).caption + " "
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
                            temp = Left$(control(i).caption, control(i).w)
                            _PrintString (x + (control(i).w - Len(temp)) \ 2, y), temp
                            hotkeyX = (x + (control(i).w - Len(temp)) \ 2) + control(i).hotkeypos - 1
                            hotkeyY = y
                            If control(i).shadow And (hover <> i Or (hover = i And mouseDownOn <> i)) And (focus <> i Or (focus = i And _KeyDown(32) = 0)) Then
                                If control(i).parent > 0 Then
                                    tuiSetColor 0, control(control(i).parent).bg
                                Else
                                    tuiSetColor 0, prevBG
                                End If
                                _PrintString (x + control(i).w, y), Chr$(220)
                                _PrintString (x + 1, y + 1), String$(control(i).w, 223)
                            End If
                            If showFocus And focus = i Then Locate y, x + (control(i).w - Len(control(i).caption)) \ 2, 1
                        Case controlType("checkbox")
                            If control(i).value Then
                                temp = "[X] "
                            Else
                                temp = "[ ] "
                            End If
                            _PrintString (x, y), temp + Left$(control(i).caption, control(i).w - 4)
                            hotkeyX = x + Len(temp) + control(i).hotkeypos - 1
                            hotkeyY = y
                            If showFocus And focus = i Then Locate y, x + 1, 1
                        Case controlType("listbox")
                            Dim As Integer listFg, listBg, listSelFg, listSelBg
                            temp = control(i).text
                            GoSub countListboxItems
                            If lineCount < 0 Then lineCount = 0
                            If control(i).value > lineCount Then control(i).value = lineCount
                            If control(i).value < 1 And lineCount > 0 Then control(i).value = 1
                            If control(i).scrollTop < 1 Then control(i).scrollTop = 1
                            this = i
                            GoSub listboxSyncScroll
                            listTextWidth = control(i).w
                            If lineCount > control(i).h - 2 Then listTextWidth = control(i).w - 1
                            If listTextWidth < 1 Then listTextWidth = 1
                            If focus = i Then
                                listFg = 31
                                listBg = 8
                                listSelFg = 0
                                listSelBg = 7
                                If control(i).hasFghover Then listFg = control(i).fghover
                                If control(i).hasBghover Then listBg = control(i).bghover
                                If control(i).hasFg Then listSelFg = control(i).fg
                                If control(i).hasBg Then listSelBg = control(i).bg
                            Else
                                listFg = 7
                                listBg = 8
                                listSelFg = 0
                                listSelBg = 7
                                If control(i).hasFg Then listFg = control(i).fg
                                If control(i).hasBg Then listBg = control(i).bg
                            End If
                            box x, y, control(i).w, control(i).h
                            row = 1
                            Do While row <= control(i).h - 2
                                lineNum = control(i).scrollTop + row - 1
                                If lineNum > lineCount Then
                                    tuiSetColor listFg, listBg
                                    _PrintString (x + 1, y + row), Space$(listTextWidth - 2)
                                Else
                                    GoSub getListboxItem
                                    If listItemText = "-" Then
                                        tuiSetColor listFg, listBg
                                        _PrintString (x, y + row), Chr$(195) + String$(listTextWidth - 1, 196) + Chr$(180)
                                    Else
                                        If lineNum = control(i).value Then
                                            tuiSetColor listSelFg, listSelBg
                                        Else
                                            tuiSetColor listFg, listBg
                                        End If
                                        listItemText = " " + listItemText
                                        If Len(listItemText) > listTextWidth - 2 Then listItemText = Left$(listItemText, listTextWidth - 3) + Chr$(26)
                                        listItemText = listItemText + Space$(listTextWidth - 2)
                                        listItemText = Left$(listItemText, listTextWidth - 2)
                                        _PrintString (x + 1, y + row), listItemText
                                    End If
                                End If
                                row = row + 1
                            Loop
                            If lineCount > control(i).h - 2 Then
                                scrollbarTrack = control(i).h - 2
                                If scrollbarTrack < 1 Then scrollbarTrack = 1
                                scrollbarThumb = (scrollbarTrack * scrollbarTrack) \ lineCount
                                If scrollbarThumb < 1 Then scrollbarThumb = 1
                                If scrollbarThumb > scrollbarTrack Then scrollbarThumb = scrollbarTrack
                                If lineCount > scrollbarTrack Then
                                    scrollbarTop = ((control(i).scrollTop - 1) * (scrollbarTrack - scrollbarThumb)) \ (lineCount - scrollbarTrack) + 1
                                Else
                                    scrollbarTop = 1
                                End If
                                For row = 1 To scrollbarTrack
                                    If row >= scrollbarTop And row < scrollbarTop + scrollbarThumb Then
                                        _PrintString (x + control(i).w - 1, y + row), Chr$(219)
                                    Else
                                        _PrintString (x + control(i).w - 1, y + row), Chr$(176)
                                    End If
                                Next
                            End If
                            If focus = i And control(i).value >= control(i).scrollTop And control(i).value <= control(i).scrollTop + control(i).h - 3 Then
                                Locate y + control(i).value - control(i).scrollTop + 1, x + 2, 1
                            End If
                        Case controlType("label")
                            _PrintString (x, y), Space$(control(i).w)
                            _PrintString (x, y), Left$(control(i).caption, control(i).w)
                            hotkeyX = x + control(i).hotkeypos - 1
                            hotkeyY = y
                        Case controlType("textbox"), controlType("textarea")
                            Dim As Integer textFg, textBg
                            Dim As Long cursorLine, cursorCol, visibleLineStart, visibleLineEnd, cursorY, lineLen, segmentStart, segmentLen, wrapPos, rawSegmentStart, rawSegmentEnd, rawSegmentLen
                            Dim As String lineText, displayText, rawLineText, remaining
                            If focus = i Then
                                textFg = 15
                                textBg = 8
                                If control(i).hasFghover Then textFg = control(i).fghover
                                If control(i).hasBghover Then textBg = control(i).bghover
                                If Not control(i).hasFghover And control(i).hasFg Then textFg = control(i).fg
                                If Not control(i).hasBghover And control(i).hasBg Then textBg = control(i).bg
                            Else
                                textFg = 7
                                textBg = 8
                                If control(i).hasFg Then textFg = control(i).fg
                                If control(i).hasBg Then textBg = control(i).bg
                            End If
                            tuiSetColor textFg, textBg
                            If control(i).type = controlType("textbox") Then
                                _PrintString (x, y), Space$(control(i).w)
                                temp = control(i).text
                                If control(i).cursorPos < 1 Then control(i).cursorPos = 1
                                If control(i).cursorPos > Len(temp) + 1 Then control(i).cursorPos = Len(temp) + 1
                                displayStart = 1
                                If Len(temp) > control(i).w Then
                                    displayStart = control(i).cursorPos - control(i).w + 1
                                    If displayStart < 1 Then displayStart = 1
                                    If displayStart + control(i).w - 1 > Len(temp) Then displayStart = Len(temp) - control(i).w + 1
                                End If
                                _PrintString (x, y), Mid$(temp, displayStart, control(i).w)
                                If focus = i And control(i).selectionStart <> control(i).selectionEnd Then
                                    selLo = control(i).selectionStart
                                    selHi = control(i).selectionEnd
                                    If selLo > selHi Then
                                        selectionPos = selLo
                                        selLo = selHi
                                        selHi = selectionPos
                                    End If
                                    If selLo < 1 Then selLo = 1
                                    If selHi > Len(temp) + 1 Then selHi = Len(temp) + 1
                                    visibleLeft = selLo
                                    visibleRight = selHi - 1
                                    If visibleLeft < displayStart Then visibleLeft = displayStart
                                    If visibleRight > displayStart + control(i).w - 1 Then visibleRight = displayStart + control(i).w - 1
                                    If visibleLeft <= visibleRight Then
                                        selectionLen = visibleRight - visibleLeft + 1
                                        If focus = i Then
                                            tuiSetColor 0, 7
                                        Else
                                            tuiSetColor 8, 7
                                        End If
                                        _PrintString (x + visibleLeft - displayStart, y), Mid$(temp, visibleLeft, selectionLen)
                                    End If
                                End If
                                If focus = i Then
                                    cursorX = x + control(i).cursorPos - displayStart
                                    If cursorX < x Then cursorX = x
                                    If cursorX > x + control(i).w Then cursorX = x + control(i).w
                                    Locate y, cursorX, 1
                                End If
                            Else
                                temp = control(i).text
                                If control(i).cursorPos < 1 Then control(i).cursorPos = 1
                                If control(i).cursorPos > Len(temp) + 1 Then control(i).cursorPos = Len(temp) + 1
                                If control(i).scrollTop < 1 Then control(i).scrollTop = 1
                                textWidth = control(i).w - 1
                                If textWidth < 1 Then textWidth = 1

                                cursorLine = 1
                                cursorCol = 1
                                visualLine = 1
                                lineStart = 1
                                Do
                                    lineEnd = InStr(lineStart, temp, Chr$(10))
                                    If lineEnd = 0 Then lineEnd = Len(temp) + 1
                                    rawLineText = Mid$(temp, lineStart, lineEnd - lineStart)
                                    lineLen = Len(rawLineText)
                                    If lineLen = 0 Then
                                        If control(i).cursorPos = lineStart Then
                                            cursorLine = visualLine
                                            cursorCol = 1
                                        End If
                                        visualLine = visualLine + 1
                                    Else
                                        segmentStart = 1
                                        While segmentStart <= lineLen
                                            remaining = Mid$(rawLineText, segmentStart)
                                            If Len(remaining) <= textWidth Then
                                                segmentLen = Len(remaining)
                                                segmentAdvance = segmentLen
                                            Else
                                                wrapPos = _InStrRev(Left$(remaining, textWidth + 1), " ")
                                                If wrapPos <= 1 Then
                                                    segmentLen = textWidth
                                                    segmentAdvance = segmentLen
                                                Else
                                                    segmentLen = wrapPos - 1
                                                    segmentAdvance = wrapPos
                                                End If
                                            End If
                                            rawSegmentStart = lineStart + segmentStart - 1
                                            rawSegmentEnd = rawSegmentStart + segmentLen - 1
                                            If control(i).cursorPos >= rawSegmentStart And control(i).cursorPos <= rawSegmentEnd + 1 Then
                                                cursorLine = visualLine
                                                cursorCol = control(i).cursorPos - rawSegmentStart + 1
                                                If cursorCol < 1 Then cursorCol = 1
                                                If cursorCol > segmentLen + 1 Then cursorCol = segmentLen + 1
                                            End If
                                            visualLine = visualLine + 1
                                            segmentStart = segmentStart + segmentAdvance
                                        Wend
                                    End If
                                    If lineEnd > Len(temp) Then Exit Do
                                    lineStart = lineEnd + 1
                                Loop
                                totalVisualLines = visualLine - 1
                                If totalVisualLines < 1 Then totalVisualLines = 1
                                If cursorLine < control(i).scrollTop Then control(i).scrollTop = cursorLine
                                If cursorLine > control(i).scrollTop + control(i).h - 1 Then control(i).scrollTop = cursorLine - control(i).h + 1
                                If control(i).scrollTop > totalVisualLines - control(i).h + 1 Then control(i).scrollTop = totalVisualLines - control(i).h + 1
                                If control(i).scrollTop < 1 Then control(i).scrollTop = 1

                                lineStart = 1
                                row = 1
                                visualLine = 1
                                Do
                                    If lineStart > Len(temp) + 1 Then Exit Do
                                    lineEnd = InStr(lineStart, temp, Chr$(10))
                                    If lineEnd = 0 Then lineEnd = Len(temp) + 1
                                    rawLineText = Mid$(temp, lineStart, lineEnd - lineStart)
                                    lineLen = Len(rawLineText)
                                    If lineLen = 0 Then
                                        If visualLine >= control(i).scrollTop And row <= control(i).h Then
                                            _PrintString (x, y + row - 1), Space$(textWidth)
                                            row = row + 1
                                        End If
                                        visualLine = visualLine + 1
                                    Else
                                        segmentStart = 1
                                        While segmentStart <= lineLen
                                            remaining = Mid$(rawLineText, segmentStart)
                                            If Len(remaining) <= textWidth Then
                                                segmentLen = Len(remaining)
                                                segmentAdvance = segmentLen
                                            Else
                                                wrapPos = _InStrRev(Left$(remaining, textWidth + 1), " ")
                                                If wrapPos <= 1 Then
                                                    segmentLen = textWidth
                                                    segmentAdvance = segmentLen
                                                Else
                                                    segmentLen = wrapPos - 1
                                                    segmentAdvance = wrapPos
                                                End If
                                            End If
                                            If visualLine >= control(i).scrollTop And row <= control(i).h Then
                                                displayText = Mid$(rawLineText, segmentStart, segmentLen)
                                                _PrintString (x, y + row - 1), displayText + Space$(textWidth - Len(displayText))
                                                If focus = i And control(i).selectionStart <> control(i).selectionEnd Then
                                                    selLo = control(i).selectionStart
                                                    selHi = control(i).selectionEnd
                                                    If selLo > selHi Then
                                                        selectionPos = selLo
                                                        selLo = selHi
                                                        selHi = selectionPos
                                                    End If
                                                    rawSegmentStart = lineStart + segmentStart - 1
                                                    rawSegmentEnd = rawSegmentStart + segmentLen - 1
                                                    If selHi - 1 >= rawSegmentStart And selLo <= rawSegmentEnd Then
                                                        visibleLeft = selLo
                                                        If visibleLeft < rawSegmentStart Then visibleLeft = rawSegmentStart
                                                        visibleRight = selHi - 1
                                                        If visibleRight > rawSegmentEnd Then visibleRight = rawSegmentEnd
                                                        visibleLeft = visibleLeft - rawSegmentStart + 1
                                                        visibleRight = visibleRight - rawSegmentStart + 1
                                                        If visibleLeft < 1 Then visibleLeft = 1
                                                        If visibleRight > Len(displayText) Then visibleRight = Len(displayText)
                                                        If visibleLeft <= visibleRight Then
                                                            selectionLen = visibleRight - visibleLeft + 1
                                                            tuiSetColor 0, 7
                                                            _PrintString (x + visibleLeft - 1, y + row - 1), Mid$(displayText, visibleLeft, selectionLen)
                                                            tuiSetColor textFg, textBg
                                                        End If
                                                    End If
                                                End If
                                                row = row + 1
                                            End If
                                            visualLine = visualLine + 1
                                            segmentStart = segmentStart + segmentAdvance
                                        Wend
                                    End If
                                    If row > control(i).h Then Exit Do
                                    If lineEnd > Len(temp) Then Exit Do
                                    lineStart = lineEnd + 1
                                Loop
                                While row <= control(i).h
                                    _PrintString (x, y + row - 1), Space$(textWidth)
                                    row = row + 1
                                Wend

                                If totalVisualLines > control(i).h Then
                                    scrollbarTrack = control(i).h
                                    scrollbarThumb = (control(i).h * control(i).h) \ totalVisualLines
                                    If scrollbarThumb < 1 Then scrollbarThumb = 1
                                    If scrollbarThumb > control(i).h Then scrollbarThumb = control(i).h
                                    scrollbarTop = ((control(i).scrollTop - 1) * (scrollbarTrack - scrollbarThumb)) \ (totalVisualLines - control(i).h) + 1
                                Else
                                    scrollbarThumb = control(i).h
                                    scrollbarTop = 1
                                End If
                                For row = 1 To control(i).h
                                    If row >= scrollbarTop And row < scrollbarTop + scrollbarThumb Then
                                        _PrintString (x + control(i).w - 1, y + row - 1), Chr$(219)
                                    Else
                                        _PrintString (x + control(i).w - 1, y + row - 1), Chr$(176)
                                    End If
                                Next

                                If focus = i Then
                                    cursorY = y + cursorLine - control(i).scrollTop
                                    cursorX = x + cursorCol - 1
                                    If cursorX < x Then cursorX = x
                                    If cursorX > x + textWidth - 1 Then cursorX = x + textWidth - 1
                                    If cursorY < y Then cursorY = y
                                    If cursorY > y + control(i).h - 1 Then cursorY = y + control(i).h - 1
                                    Locate cursorY, cursorX, 1
                                End If
                            End If
                    End Select

                    If control(i).hotkeypos > 0 And showHotKey And control(menuPanel(totalMenuPanels)).active = 0 Then
                        tuiSetColor control(i).fghotkey, -1
                        _PrintString (hotkeyX, hotkeyY), control(i).hotkey
                    End If
                Next

                If hasMenuBar Then
                    Dim firstMenuFound As _Byte
                    firstMenuFound = 0
                    For i = 1 To UBound(control)
                        If control(i).type = controlType("menubar") Then
                            If control(i).hidden Or control(i).active = 0 Then _Continue
                            If focus = i Then Locate , , 0
                            If firstMenuFound = 0 Then
                                x = control(i).x
                                tuiSetColor control(i).fg, control(i).bg
                                _PrintString (1, 1), Space$(_Width)
                                firstMenuFound = -1
                            Else
                                x = x + control(i).w + 2
                                control(i).x = x
                            End If
                            If modalForm Then
                                tuiSetColor 8, control(i).bg
                            Else
                                If keyboardControl = 0 And (modalForm = 0 And my = 1 And mx >= control(i).x And mx < control(i).x + Len(control(i).caption) + 2) Then
                                    If draggingForm = 0 And control(i).disabled = 0 Then
                                        tuiSetColor control(i).fghover, control(i).bghover
                                        hover = i
                                        If control(focus).type = controlType("menubar") Then focus = i
                                        If totalMenuPanels > 0 And control(menuPanel(totalMenuPanels)).parent <> focus Then GoSub openMenuPanel
                                    ElseIf control(i).disabled Then
                                        tuiSetColor 8, control(i).bg
                                    End If
                                    If focus = i Then Locate , , 0
                                ElseIf focus = i Or InStr(menuPanelParents, MKL$(i) + MKL$(-1)) > 0 Then
                                    tuiSetColor control(i).fghover, control(i).bghover
                                Else
                                    If control(menuPanel(totalMenuPanels)).parent <> i Or control(menuPanel(totalMenuPanels)).active = 0 Then
                                        If control(i).disabled Then
                                            tuiSetColor 8, control(i).bg
                                        Else
                                            tuiSetColor control(i).fg, control(i).bg
                                        End If
                                    ElseIf totalMenuPanels Then
                                        tuiSetColor control(i).fghover, control(i).bghover
                                    End If
                                End If
                            End If
                            _PrintString (x, 1), " " + control(i).caption + " "
                            If (control(focus).type = controlType("menubar") And control(i).disabled = 0) Or (modalForm = 0 And control(i).hotkeypos > 0 And showHotKey And totalMenuPanels = 0 And control(i).disabled = 0) Then
                                tuiSetColor control(i).fghotkey, -1
                                _PrintString (x + control(i).hotkeypos, 1), control(i).hotkey
                            End If
                        End If
                    Next
                End If

                If totalMenuPanels > 0 Then
                    Dim As String menuCaption, menuShortcut
                    Dim As Long willActivateMenuPanel
                    Dim As Single activateMenuPanelTimer

                    For this = 1 To totalMenuPanels
                        tuiSetColor control(menuPanel(this)).fg, control(menuPanel(this)).bg
                        boxShadow control(menuPanel(this)).x, control(menuPanel(this)).y, control(menuPanel(this)).w, control(menuPanel(this)).h
                        If keyboardControl = 0 And mx >= control(menuPanel(this)).x And mx <= control(menuPanel(this)).x + control(menuPanel(this)).w - 1 And my >= control(menuPanel(this)).y And my <= control(menuPanel(this)).y + control(menuPanel(this)).h - 1 Then
                            hover = menuPanel(this)
                        End If
                        For i = 1 To UBound(control)
                            If control(i).type = controlType("menuitem") And control(i).parent = control(menuPanel(this)).parent Then
                                If focus = i Then Locate , , 0
                                If control(i).caption = "-" Then
                                    tuiSetColor control(menuPanel(this)).fg, control(menuPanel(this)).bg
                                    _PrintString (control(i).x - 2, control(i).y), Chr$(195) + String$(control(menuPanel(this)).w - 2, 196) + Chr$(180)
                                Else
                                    menuShortcut = ""
                                    j = InStr(control(i).caption, Space$(2))
                                    If j > 0 And control(i).special <> "submenu" Then
                                        menuCaption = Left$(control(i).caption, j - 1)
                                        menuShortcut = Mid$(control(i).caption, j + 2)
                                    ElseIf control(i).special = "submenu" Then
                                        menuCaption = control(i).caption
                                        menuShortcut = Chr$(16)
                                    Else
                                        menuCaption = control(i).caption
                                    End If

                                    If keyboardControl = 0 And (mx >= control(i).x - 1 And mx <= control(menuPanel(this)).x + control(menuPanel(this)).w - 2 And my = control(i).y) Then
                                        hover = i
                                        focus = i
                                    End If

                                    If (focus = i And control(i).parent = control(menuPanel(totalMenuPanels)).parent) Or InStr(menuPanelParents, MKL$(i) + MKL$(-1)) > 0 Then
                                        tuiSetColor control(menuPanel(this)).fghover, control(menuPanel(this)).bghover
                                        _PrintString (control(i).x - 1, control(i).y), Space$(control(menuPanel(this)).w - 2)
                                        If focus = i And control(i).special = "submenu" And willActivateMenuPanel = 0 Then
                                            willActivateMenuPanel = i: activateMenuPanelTimer = Timer
                                        ElseIf focus = i And control(i).special <> "submenu" And willActivateMenuPanel > 0 Then
                                            willActivateMenuPanel = 0
                                        End If
                                    Else
                                        If control(i).disabled Then
                                            tuiSetColor 8, control(menuPanel(this)).bg
                                        Else
                                            tuiSetColor control(menuPanel(this)).fg, control(menuPanel(this)).bg
                                        End If
                                    End If
                                    _PrintString (control(i).x, control(i).y), menuCaption
                                    If Len(menuShortcut) Then
                                        _PrintString (control(menuPanel(this)).x + control(menuPanel(this)).w - Len(menuShortcut) - 2, control(i).y), menuShortcut
                                    End If
                                    If control(i).hotkeypos > 0 And control(i).disabled = 0 Then
                                        Color control(i).fghotkey
                                        _PrintString (control(i).x + control(i).hotkeypos - 1, control(i).y), control(i).hotkey
                                    End If
                                End If
                            End If
                        Next
                    Next
                End If

                If timeElapsedSince(activateMenuPanelTimer) >= .5 And willActivateMenuPanel > 0 And InStr(menuPanelParents, MKL$(willActivateMenuPanel) + MKL$(-1)) = 0 And keyboardControl = 0 Then
                    focus = willActivateMenuPanel
                    GoSub openMenuPanel
                    willActivateMenuPanel = 0
                ElseIf timeElapsedSince(activateMenuPanelTimer) >= .5 And willActivateMenuPanel > 0 Then
                    willActivateMenuPanel = 0
                End If

                If control(focus).type = controlType("menuitem") Then
                    While totalMenuPanels > 0 And control(menuPanel(totalMenuPanels)).parent <> control(focus).parent And control(menuPanel(totalMenuPanels)).parent <> focus
                        GoSub closeMenuPanel
                    Wend
                End If

                Color prevFG, prevBG

                If fetchedKeyboard = 0 And k = 0 Then
                    k = _KeyHit
                    fetchedKeyboard = -1
                End If

                If k Then GoSub enableKeyboardControl
                Select EveryCase k
                    Case -9, -25
                        this = focus
                        If _KeyDown(100304) Or _KeyDown(100303) Then
                            Do
                                focus = focus - 1
                                If focus < 1 Then focus = UBound(control)
                                If focus = this Then Exit Do
                            Loop While control(focus).canReceiveFocus = 0
                        Else
                            Do
                                focus = focus + 1
                                If focus > UBound(control) Then focus = 1
                                If focus = this Then Exit Do
                            Loop While control(focus).canReceiveFocus = 0
                        End If
                    Case -13
                        Select Case control(focus).type
                            Case controlType("textarea")
                                If Not showHotKey And control(menuPanel(totalMenuPanels)).active = 0 Then
                                    temp = control(focus).text
                                    If control(focus).selectionStart <> control(focus).selectionEnd Then
                                        selLo = control(focus).selectionStart
                                        selHi = control(focus).selectionEnd
                                        If selLo > selHi Then selLo = selLo + selHi: selHi = selLo - selHi: selLo = selLo - selHi
                                        control(focus).text = Left$(temp, selLo - 1) + Mid$(temp, selHi)
                                        control(focus).cursorPos = selLo
                                    End If
                                    control(focus).text = Left$(control(focus).text, control(focus).cursorPos - 1) + Chr$(10) + Mid$(control(focus).text, control(focus).cursorPos)
                                    control(focus).cursorPos = control(focus).cursorPos + 1
                                    control(focus).selectionStart = control(focus).cursorPos
                                    control(focus).selectionEnd = control(focus).cursorPos
                                    k = 0
                                End If
                            Case controlType("textbox")
                                this = focus
                                Do
                                    focus = focus + 1
                                    If focus > UBound(control) Then focus = 1
                                    If focus = this Then Exit Do
                                Loop While control(focus).canReceiveFocus = 0
                                k = 0
                            Case controlType("listbox")
                                If control(focus).disabled = 0 Then clicked = focus
                                k = 0
                            Case controlType("button"), controlType("menuitem")
                                If control(focus).disabled = 0 Then clicked = focus Else Exit Case
                                If control(focus).type = controlType("menuitem") Then
                                    If control(focus).special = "submenu" Then
                                        clicked = 0
                                        GoSub openMenuPanel
                                    Else
                                        While totalMenuPanels
                                            GoSub closeMenuPanel
                                        Wend
                                    End If
                                End If
                            Case controlType("menubar")
                                GoSub openMenuPanel
                        End Select
                    Case -32
                        Select Case control(focus).type
                            Case controlType("button")
                                clicked = focus
                            Case controlType("checkbox")
                                control(focus).value = Not control(focus).value
                                clicked = focus
                            Case controlType("listbox")
                                If control(focus).disabled = 0 Then clicked = focus
                        End Select
                    Case 27
                        If totalMenuPanels Then focus = control(menuPanel(totalMenuPanels)).parent: GoSub closeMenuPanel
                        k = 0
                    Case 18432 'up
                        Select Case control(focus).type
                            Case controlType("menubar")
                                GoSub openMenuPanel
                            Case controlType("menuitem")
                                this = focus
                                Do
                                    this = this - 1
                                    If this < 1 Then this = UBound(control)
                                    If this = focus Then Exit Do
                                    If control(this).type = controlType("menuitem") And control(this).parent = control(focus).parent And control(this).caption <> "-" And control(this).hidden = 0 Then
                                        focus = this
                                        Exit Do
                                    End If
                                Loop
                            Case controlType("listbox")
                                temp = control(focus).text
                                GoSub countListboxItems
                                If lineCount > 0 And control(focus).value > 1 Then
                                    control(focus).value = control(focus).value - 1
                                    this = focus
                                    GoSub listboxSyncScroll
                                End If
                                k = 0
                            Case controlType("textbox"), controlType("textarea")
                                shiftDown = _KeyDown(100304) Or _KeyDown(100303)
                                If control(focus).type = controlType("textarea") Then
                                    temp = control(focus).text
                                    cursorLine = 1
                                    cursorCol = 1
                                    textWidth = control(focus).w - 1
                                    If textWidth < 1 Then textWidth = 1
                                    visualLine = 1
                                    lineStart = 1
                                    Do
                                        lineEnd = InStr(lineStart, temp, Chr$(10))
                                        If lineEnd = 0 Then lineEnd = Len(temp) + 1
                                        rawLineText = Mid$(temp, lineStart, lineEnd - lineStart)
                                        lineLen = Len(rawLineText)
                                        If lineLen = 0 Then
                                            If control(focus).cursorPos = lineStart Then cursorLine = visualLine: cursorCol = 1
                                            visualLine = visualLine + 1
                                        Else
                                            segmentStart = 1
                                            While segmentStart <= lineLen
                                                remaining = Mid$(rawLineText, segmentStart)
                                                If Len(remaining) <= textWidth Then
                                                    segmentLen = Len(remaining)
                                                    segmentAdvance = segmentLen
                                                Else
                                                    wrapPos = _InStrRev(Left$(remaining, textWidth + 1), " ")
                                                    If wrapPos <= 1 Then
                                                        segmentLen = textWidth
                                                        segmentAdvance = segmentLen
                                                    Else
                                                        segmentLen = wrapPos - 1
                                                        segmentAdvance = wrapPos
                                                    End If
                                                End If
                                                rawSegmentStart = lineStart + segmentStart - 1
                                                rawSegmentEnd = rawSegmentStart + segmentLen - 1
                                                If control(focus).cursorPos >= rawSegmentStart And control(focus).cursorPos <= rawSegmentEnd + 1 Then
                                                    cursorLine = visualLine
                                                    cursorCol = control(focus).cursorPos - rawSegmentStart + 1
                                                    If cursorCol < 1 Then cursorCol = 1
                                                    If cursorCol > segmentLen + 1 Then cursorCol = segmentLen + 1
                                                End If
                                                visualLine = visualLine + 1
                                                segmentStart = segmentStart + segmentAdvance
                                            Wend
                                        End If
                                        If lineEnd > Len(temp) Then Exit Do
                                        lineStart = lineEnd + 1
                                    Loop
                                    totalVisualLines = visualLine - 1
                                    targetVisualLine = cursorLine - 1
                                    If targetVisualLine < 1 Then targetVisualLine = 1
                                    visualLine = 1
                                    lineStart = 1
                                    Do
                                        lineEnd = InStr(lineStart, temp, Chr$(10))
                                        If lineEnd = 0 Then lineEnd = Len(temp) + 1
                                        rawLineText = Mid$(temp, lineStart, lineEnd - lineStart)
                                        lineLen = Len(rawLineText)
                                        If lineLen = 0 Then
                                            If visualLine = targetVisualLine Then control(focus).cursorPos = lineStart: Exit Do
                                            visualLine = visualLine + 1
                                        Else
                                            segmentStart = 1
                                            While segmentStart <= lineLen
                                                remaining = Mid$(rawLineText, segmentStart)
                                                If Len(remaining) <= textWidth Then
                                                    segmentLen = Len(remaining)
                                                    segmentAdvance = segmentLen
                                                Else
                                                    wrapPos = _InStrRev(Left$(remaining, textWidth + 1), " ")
                                                    If wrapPos <= 1 Then
                                                        segmentLen = textWidth
                                                        segmentAdvance = segmentLen
                                                    Else
                                                        segmentLen = wrapPos - 1
                                                        segmentAdvance = wrapPos
                                                    End If
                                                End If
                                                If visualLine = targetVisualLine Then
                                                    control(focus).cursorPos = lineStart + segmentStart + cursorCol - 2
                                                    If control(focus).cursorPos > lineStart + segmentStart + segmentLen - 1 Then control(focus).cursorPos = lineStart + segmentStart + segmentLen - 1
                                                    If cursorCol > segmentLen Then control(focus).cursorPos = lineStart + segmentStart + segmentLen - 1
                                                    If control(focus).cursorPos < lineStart + segmentStart - 1 Then control(focus).cursorPos = lineStart + segmentStart - 1
                                                    Exit Do
                                                End If
                                                visualLine = visualLine + 1
                                                segmentStart = segmentStart + segmentAdvance
                                            Wend
                                        End If
                                        If lineEnd > Len(temp) Then Exit Do
                                        lineStart = lineEnd + 1
                                    Loop
                                End If
                                If shiftDown Then
                                    control(focus).selectionEnd = control(focus).cursorPos
                                Else
                                    control(focus).selectionStart = control(focus).cursorPos
                                    control(focus).selectionEnd = control(focus).cursorPos
                                End If
                                k = 0
                        End Select
                    Case 20480 'down
                        Select Case control(focus).type
                            Case controlType("menubar")
                                GoSub openMenuPanel
                            Case controlType("menuitem")
                                this = focus
                                Do
                                    this = this + 1
                                    If this > UBound(control) Then this = 1
                                    If this = focus Then Exit Do
                                    If control(this).type = controlType("menuitem") And control(this).parent = control(focus).parent And control(this).caption <> "-" And control(this).hidden = 0 Then
                                        focus = this
                                        Exit Do
                                    End If
                                Loop
                            Case controlType("listbox")
                                temp = control(focus).text
                                GoSub countListboxItems
                                If lineCount > 0 And control(focus).value < lineCount Then
                                    control(focus).value = control(focus).value + 1
                                    this = focus
                                    GoSub listboxSyncScroll
                                End If
                                k = 0
                            Case controlType("textbox"), controlType("textarea")
                                shiftDown = _KeyDown(100304) Or _KeyDown(100303)
                                If control(focus).type = controlType("textarea") Then
                                    temp = control(focus).text
                                    cursorLine = 1
                                    cursorCol = 1
                                    textWidth = control(focus).w - 1
                                    If textWidth < 1 Then textWidth = 1
                                    visualLine = 1
                                    lineStart = 1
                                    Do
                                        lineEnd = InStr(lineStart, temp, Chr$(10))
                                        If lineEnd = 0 Then lineEnd = Len(temp) + 1
                                        rawLineText = Mid$(temp, lineStart, lineEnd - lineStart)
                                        lineLen = Len(rawLineText)
                                        If lineLen = 0 Then
                                            If control(focus).cursorPos = lineStart Then cursorLine = visualLine: cursorCol = 1
                                            visualLine = visualLine + 1
                                        Else
                                            segmentStart = 1
                                            While segmentStart <= lineLen
                                                remaining = Mid$(rawLineText, segmentStart)
                                                If Len(remaining) <= textWidth Then
                                                    segmentLen = Len(remaining)
                                                    segmentAdvance = segmentLen
                                                Else
                                                    wrapPos = _InStrRev(Left$(remaining, textWidth + 1), " ")
                                                    If wrapPos <= 1 Then
                                                        segmentLen = textWidth
                                                        segmentAdvance = segmentLen
                                                    Else
                                                        segmentLen = wrapPos - 1
                                                        segmentAdvance = wrapPos
                                                    End If
                                                End If
                                                rawSegmentStart = lineStart + segmentStart - 1
                                                rawSegmentEnd = rawSegmentStart + segmentLen - 1
                                                If control(focus).cursorPos >= rawSegmentStart And control(focus).cursorPos <= rawSegmentEnd + 1 Then
                                                    cursorLine = visualLine
                                                    cursorCol = control(focus).cursorPos - rawSegmentStart + 1
                                                    If cursorCol < 1 Then cursorCol = 1
                                                    If cursorCol > segmentLen + 1 Then cursorCol = segmentLen + 1
                                                End If
                                                visualLine = visualLine + 1
                                                segmentStart = segmentStart + segmentAdvance
                                            Wend
                                        End If
                                        If lineEnd > Len(temp) Then Exit Do
                                        lineStart = lineEnd + 1
                                    Loop
                                    totalVisualLines = visualLine - 1
                                    If totalVisualLines < 1 Then totalVisualLines = 1
                                    targetVisualLine = cursorLine + 1
                                    If targetVisualLine > totalVisualLines Then targetVisualLine = totalVisualLines
                                    visualLine = 1
                                    lineStart = 1
                                    Do
                                        lineEnd = InStr(lineStart, temp, Chr$(10))
                                        If lineEnd = 0 Then lineEnd = Len(temp) + 1
                                        rawLineText = Mid$(temp, lineStart, lineEnd - lineStart)
                                        lineLen = Len(rawLineText)
                                        If lineLen = 0 Then
                                            If visualLine = targetVisualLine Then control(focus).cursorPos = lineStart: Exit Do
                                            visualLine = visualLine + 1
                                        Else
                                            segmentStart = 1
                                            While segmentStart <= lineLen
                                                remaining = Mid$(rawLineText, segmentStart)
                                                If Len(remaining) <= textWidth Then
                                                    segmentLen = Len(remaining)
                                                    segmentAdvance = segmentLen
                                                Else
                                                    wrapPos = _InStrRev(Left$(remaining, textWidth + 1), " ")
                                                    If wrapPos <= 1 Then
                                                        segmentLen = textWidth
                                                        segmentAdvance = segmentLen
                                                    Else
                                                        segmentLen = wrapPos - 1
                                                        segmentAdvance = wrapPos
                                                    End If
                                                End If
                                                If visualLine = targetVisualLine Then
                                                    control(focus).cursorPos = lineStart + segmentStart + cursorCol - 2
                                                    If control(focus).cursorPos > lineStart + segmentStart + segmentLen - 1 Then control(focus).cursorPos = lineStart + segmentStart + segmentLen - 1
                                                    If cursorCol > segmentLen Then control(focus).cursorPos = lineStart + segmentStart + segmentLen - 1
                                                    If control(focus).cursorPos < lineStart + segmentStart - 1 Then control(focus).cursorPos = lineStart + segmentStart - 1
                                                    Exit Do
                                                End If
                                                visualLine = visualLine + 1
                                                segmentStart = segmentStart + segmentAdvance
                                            Wend
                                        End If
                                        If lineEnd > Len(temp) Then Exit Do
                                        lineStart = lineEnd + 1
                                    Loop
                                End If
                                If shiftDown Then
                                    control(focus).selectionEnd = control(focus).cursorPos
                                Else
                                    control(focus).selectionStart = control(focus).cursorPos
                                    control(focus).selectionEnd = control(focus).cursorPos
                                End If
                                k = 0
                        End Select
                    Case 19200 'left
                        Select EveryCase control(focus).type
                            Case controlType("menubar"), controlType("menuitem")
                                If control(focus).type = controlType("menuitem") Then
                                    If control(control(menuPanel(totalMenuPanels)).parent).type = controlType("menuitem") Then
                                        focus = control(menuPanel(totalMenuPanels)).parent
                                        GoSub closeMenuPanel
                                        Exit Case
                                    Else
                                        focus = control(focus).parent
                                    End If
                                End If
                                this = focus
                                Do
                                    this = this - 1
                                    If this < 1 Then this = UBound(control)
                                    If this = focus Then Exit Do
                                    If control(this).type = controlType("menubar") And control(this).disabled = 0 And control(this).hidden = 0 Then
                                        focus = this
                                        If control(menuPanel(totalMenuPanels)).active Then GoSub openMenuPanel
                                        Exit Do
                                    End If
                                Loop
                            Case controlType("textbox"), controlType("textarea")
                                shiftDown = _KeyDown(100304) Or _KeyDown(100303)
                                If control(focus).cursorPos > 1 Then control(focus).cursorPos = control(focus).cursorPos - 1
                                If shiftDown Then
                                    control(focus).selectionEnd = control(focus).cursorPos
                                Else
                                    control(focus).selectionStart = control(focus).cursorPos
                                    control(focus).selectionEnd = control(focus).cursorPos
                                End If
                                k = 0
                        End Select
                    Case 19712 'right
                        Select EveryCase control(focus).type
                            Case controlType("menubar"), controlType("menuitem")
                                If control(focus).type = controlType("menuitem") Then
                                    If control(focus).special = "submenu" Then
                                        GoSub openMenuPanel
                                        Exit Case
                                    Else
                                        focus = control(focus).parent
                                    End If
                                End If
                                this = focus
                                Do
                                    this = this + 1
                                    If this > UBound(control) Then this = 1
                                    If this = focus Then Exit Do
                                    If control(this).type = controlType("menubar") And control(this).disabled = 0 And control(this).hidden = 0 Then
                                        focus = this
                                        If control(menuPanel(totalMenuPanels)).active Then GoSub openMenuPanel
                                        Exit Do
                                    End If
                                Loop
                            Case controlType("textbox"), controlType("textarea")
                                shiftDown = _KeyDown(100304) Or _KeyDown(100303)
                                If control(focus).cursorPos <= Len(control(focus).text) Then control(focus).cursorPos = control(focus).cursorPos + 1
                                If shiftDown Then
                                    control(focus).selectionEnd = control(focus).cursorPos
                                Else
                                    control(focus).selectionStart = control(focus).cursorPos
                                    control(focus).selectionEnd = control(focus).cursorPos
                                End If
                                k = 0
                        End Select
                    Case 32 To 126
                        If (control(focus).type = controlType("textbox") Or control(focus).type = controlType("textarea")) And Not showHotKey And control(menuPanel(totalMenuPanels)).active = 0 And control(focus).type <> controlType("menubar") Then
                            temp = control(focus).text
                            If control(focus).selectionStart <> control(focus).selectionEnd Then
                                selLo = control(focus).selectionStart
                                selHi = control(focus).selectionEnd
                                If selLo > selHi Then selLo = selLo + selHi: selHi = selLo - selHi: selLo = selLo - selHi
                                control(focus).text = Left$(temp, selLo - 1) + Mid$(temp, selHi)
                                control(focus).cursorPos = selLo
                            End If
                            control(focus).text = Left$(control(focus).text, control(focus).cursorPos - 1) + Chr$(k) + Mid$(control(focus).text, control(focus).cursorPos)
                            control(focus).cursorPos = control(focus).cursorPos + 1
                            control(focus).selectionStart = control(focus).cursorPos
                            control(focus).selectionEnd = control(focus).cursorPos
                            k = 0
                        ElseIf showHotKey Or control(menuPanel(totalMenuPanels)).active Or control(focus).type = controlType("menubar") Then
                            Dim As String hotkeySearch
                            hotkeySearch = UCase$(Chr$(k))
                            For i = 1 To UBound(control)
                                If UCase$(control(i).hotkey) = hotkeySearch Then
                                    If control(menuPanel(totalMenuPanels)).active = 0 Or (control(menuPanel(totalMenuPanels)).active And control(i).parent = control(menuPanel(totalMenuPanels)).parent) Or (control(menuPanel(totalMenuPanels)).active And control(i).type = controlType("menubar")) Then
                                        'alt+hotkey emulates click on control
                                        If control(i).type = controlType("menubar") Then
                                            If control(i).disabled = 0 And control(i).hidden = 0 Then
                                                mb = 0
                                                mouseDown = -1
                                                mouseDownOn = i
                                                hover = i
                                                prevFocus = focus
                                                GoSub openMenuPanel
                                            End If
                                        Else
                                            mb = 0
                                            mouseDown = -1
                                            mouseDownOn = i
                                            hover = i
                                            focus = i
                                        End If
                                        willActivateMenu = 0
                                        Exit For
                                    End If
                                End If
                            Next
                        End If
                        If k > 0 Then
                            For i = 1 To UBound(control)
                                If control(i).keybind = k Then
                                    'hitting a control's keybind emulates click
                                    mb = 0
                                    mouseDown = -1
                                    mouseDownOn = i
                                    hover = i
                                    focus = i
                                    k = 0
                                    Exit For
                                End If
                            Next
                        End If
                    Case 8
                        If (control(focus).type = controlType("textbox") Or control(focus).type = controlType("textarea")) And Not showHotKey And control(menuPanel(totalMenuPanels)).active = 0 Then
                            temp = control(focus).text
                            If control(focus).selectionStart <> control(focus).selectionEnd Then
                                selLo = control(focus).selectionStart
                                selHi = control(focus).selectionEnd
                                If selLo > selHi Then selLo = selLo + selHi: selHi = selLo - selHi: selLo = selLo - selHi
                                control(focus).text = Left$(temp, selLo - 1) + Mid$(temp, selHi)
                                control(focus).cursorPos = selLo
                            ElseIf control(focus).cursorPos > 1 Then
                                control(focus).text = Left$(temp, control(focus).cursorPos - 2) + Mid$(temp, control(focus).cursorPos)
                                control(focus).cursorPos = control(focus).cursorPos - 1
                            End If
                            control(focus).selectionStart = control(focus).cursorPos
                            control(focus).selectionEnd = control(focus).cursorPos
                            k = 0
                        End If
                    Case 127, 21184, 21248
                        If control(focus).type = controlType("textbox") Or control(focus).type = controlType("textarea") Then
                            temp = control(focus).text
                            If control(focus).selectionStart <> control(focus).selectionEnd Then
                                selLo = control(focus).selectionStart
                                selHi = control(focus).selectionEnd
                                If selLo > selHi Then selLo = selLo + selHi: selHi = selLo - selHi: selLo = selLo - selHi
                                control(focus).text = Left$(temp, selLo - 1) + Mid$(temp, selHi)
                                control(focus).cursorPos = selLo
                            ElseIf control(focus).cursorPos <= Len(temp) Then
                                control(focus).text = Left$(temp, control(focus).cursorPos - 1) + Mid$(temp, control(focus).cursorPos + 1)
                            End If
                            control(focus).selectionStart = control(focus).cursorPos
                            control(focus).selectionEnd = control(focus).cursorPos
                            k = 0
                        End If
                    Case 18176, 19968
                        If control(focus).type = controlType("listbox") And Not showHotKey And control(menuPanel(totalMenuPanels)).active = 0 Then
                            temp = control(focus).text
                            GoSub countListboxItems
                            If lineCount > 0 Then
                                control(focus).value = 1
                                this = focus
                                GoSub listboxSyncScroll
                            End If
                            k = 0
                        ElseIf (control(focus).type = controlType("textbox") Or control(focus).type = controlType("textarea")) And Not showHotKey And control(menuPanel(totalMenuPanels)).active = 0 Then
                            shiftDown = _KeyDown(100304) Or _KeyDown(100303)
                            If control(focus).type = controlType("textarea") Then
                                temp = control(focus).text
                                textWidth = control(focus).w - 1
                                If textWidth < 1 Then textWidth = 1
                                lineStart = 1
                                Do
                                    lineEnd = InStr(lineStart, temp, Chr$(10))
                                    If lineEnd = 0 Then lineEnd = Len(temp) + 1
                                    rawLineText = Mid$(temp, lineStart, lineEnd - lineStart)
                                    lineLen = Len(rawLineText)
                                    If lineLen = 0 Then
                                        If control(focus).cursorPos = lineStart Then
                                            control(focus).cursorPos = lineStart
                                            Exit Do
                                        End If
                                    Else
                                        segmentStart = 1
                                        While segmentStart <= lineLen
                                            remaining = Mid$(rawLineText, segmentStart)
                                            If Len(remaining) <= textWidth Then
                                                segmentLen = Len(remaining)
                                                segmentAdvance = segmentLen
                                            Else
                                                wrapPos = _InStrRev(Left$(remaining, textWidth + 1), " ")
                                                If wrapPos <= 1 Then
                                                    segmentLen = textWidth
                                                    segmentAdvance = segmentLen
                                                Else
                                                    segmentLen = wrapPos - 1
                                                    segmentAdvance = wrapPos
                                                End If
                                            End If
                                            rawSegmentStart = lineStart + segmentStart - 1
                                            rawSegmentEnd = rawSegmentStart + segmentLen - 1
                                            If control(focus).cursorPos >= rawSegmentStart And control(focus).cursorPos <= rawSegmentEnd + 1 Then
                                                control(focus).cursorPos = rawSegmentStart
                                                Exit Do
                                            End If
                                            segmentStart = segmentStart + segmentAdvance
                                        Wend
                                    End If
                                    If lineEnd > Len(temp) Then Exit Do
                                    lineStart = lineEnd + 1
                                Loop
                            Else
                                control(focus).cursorPos = 1
                            End If
                            If shiftDown Then
                                control(focus).selectionEnd = control(focus).cursorPos
                            Else
                                control(focus).selectionStart = control(focus).cursorPos
                                control(focus).selectionEnd = control(focus).cursorPos
                            End If
                            k = 0
                        End If
                    Case 20224, 20416
                        If control(focus).type = controlType("listbox") And Not showHotKey And control(menuPanel(totalMenuPanels)).active = 0 Then
                            temp = control(focus).text
                            GoSub countListboxItems
                            If lineCount > 0 Then
                                control(focus).value = lineCount
                                this = focus
                                GoSub listboxSyncScroll
                            End If
                            k = 0
                        ElseIf (control(focus).type = controlType("textbox") Or control(focus).type = controlType("textarea")) And Not showHotKey And control(menuPanel(totalMenuPanels)).active = 0 Then
                            shiftDown = _KeyDown(100304) Or _KeyDown(100303)
                            If control(focus).type = controlType("textarea") Then
                                temp = control(focus).text
                                textWidth = control(focus).w - 1
                                If textWidth < 1 Then textWidth = 1
                                lineStart = 1
                                Do
                                    lineEnd = InStr(lineStart, temp, Chr$(10))
                                    If lineEnd = 0 Then lineEnd = Len(temp) + 1
                                    rawLineText = Mid$(temp, lineStart, lineEnd - lineStart)
                                    lineLen = Len(rawLineText)
                                    If lineLen = 0 Then
                                        If control(focus).cursorPos = lineStart Then
                                            control(focus).cursorPos = lineStart
                                            Exit Do
                                        End If
                                    Else
                                        segmentStart = 1
                                        While segmentStart <= lineLen
                                            remaining = Mid$(rawLineText, segmentStart)
                                            If Len(remaining) <= textWidth Then
                                                segmentLen = Len(remaining)
                                                segmentAdvance = segmentLen
                                            Else
                                                wrapPos = _InStrRev(Left$(remaining, textWidth + 1), " ")
                                                If wrapPos <= 1 Then
                                                    segmentLen = textWidth
                                                    segmentAdvance = segmentLen
                                                Else
                                                    segmentLen = wrapPos - 1
                                                    segmentAdvance = wrapPos
                                                End If
                                            End If
                                            rawSegmentStart = lineStart + segmentStart - 1
                                            rawSegmentEnd = rawSegmentStart + segmentLen - 1
                                            If control(focus).cursorPos >= rawSegmentStart And control(focus).cursorPos <= rawSegmentEnd + 1 Then
                                                control(focus).cursorPos = rawSegmentEnd + 1
                                                Exit Do
                                            End If
                                            segmentStart = segmentStart + segmentAdvance
                                        Wend
                                    End If
                                    If lineEnd > Len(temp) Then Exit Do
                                    lineStart = lineEnd + 1
                                Loop
                            Else
                                control(focus).cursorPos = Len(control(focus).text) + 1
                            End If
                            If shiftDown Then
                                control(focus).selectionEnd = control(focus).cursorPos
                            Else
                                control(focus).selectionStart = control(focus).cursorPos
                                control(focus).selectionEnd = control(focus).cursorPos
                            End If
                            k = 0
                        End If
                    Case Else
                        If k > 0 Then
                            For i = 1 To UBound(control)
                                If control(i).keybind = k Then
                                    'hitting a control's keybind emulates click
                                    mb = 0
                                    mouseDown = -1
                                    mouseDownOn = i
                                    hover = i
                                    focus = i
                                    Exit For
                                End If
                            Next
                        End If
                End Select

                If mb Then
                    If mouseDown Then
                        'drag
                        If draggingForm Then
                            control(mouseDownOn).x = control(mouseDownOn).x - (mouseDownX - mx)
                            control(mouseDownOn).y = control(mouseDownOn).y - (mouseDownY - my)
                            If control(mouseDownOn).x < 1 Then control(mouseDownOn).x = 1
                            If hasMenuBar Then
                                If control(mouseDownOn).y < 2 Then control(mouseDownOn).y = 2
                            Else
                                If control(mouseDownOn).y < 1 Then control(mouseDownOn).y = 1
                            End If
                            If control(mouseDownOn).x + control(mouseDownOn).w > _Width Then control(mouseDownOn).x = _Width - control(mouseDownOn).w + 1
                            If control(mouseDownOn).y + control(mouseDownOn).h > _Height Then control(mouseDownOn).y = _Height - control(mouseDownOn).h + 1
                            mouseDownX = mx
                            mouseDownY = my
                        ElseIf mouseDownOn > 0 And (control(mouseDownOn).type = controlType("textbox") Or control(mouseDownOn).type = controlType("textarea")) Then
                            GoSub setTextboxCursorFromMouse
                            control(mouseDownOn).selectionEnd = control(mouseDownOn).cursorPos
                        ElseIf mouseDownOn > 0 And control(mouseDownOn).type = controlType("listbox") Then
                            GoSub setListboxFromMouse
                        End If
                    Else
                        mouseDown = -1
                        mouseDownOn = hover
                        If hover = 0 Then
                            While totalMenuPanels
                                GoSub closeMenuPanel
                            Wend
                        ElseIf control(hover).type = controlType("form") Then
                            If my = control(hover).y Then draggingForm = -1
                        ElseIf control(hover).type = controlType("menubar") Then
                            If control(menuPanel(totalMenuPanels)).active And hover = control(menuPanel(totalMenuPanels)).parent Then
                                While totalMenuPanels
                                    GoSub closeMenuPanel
                                Wend
                            Else
                                GoSub openMenuPanel
                            End If
                        Else
                            draggingForm = 0
                            If control(mouseDownOn).type = controlType("textbox") Or control(mouseDownOn).type = controlType("textarea") Then
                                focus = hover
                                GoSub setTextboxCursorFromMouse
                                shiftDown = _KeyDown(100304) Or _KeyDown(100303)
                                If Not shiftDown Then
                                    control(focus).selectionStart = control(focus).cursorPos
                                End If
                                control(focus).selectionEnd = control(focus).cursorPos
                            ElseIf control(mouseDownOn).type = controlType("listbox") Then
                                focus = hover
                                GoSub setListboxFromMouse
                            ElseIf control(mouseDownOn).canReceiveFocus Then
                                focus = hover
                            End If
                        End If
                        mouseDownX = mx
                        mouseDownY = my
                    End If
                Else
                    If mouseDown Then
                        If mouseDownOn > 0 And mouseDownOn = hover Then
                            If control(mouseDownOn).disabled = 0 Then
                                clicked = mouseDownOn

                                Select Case control(clicked).type
                                    Case controlType("checkbox")
                                        control(clicked).value = Not control(clicked).value
                                    Case controlType("menuitem")
                                        If control(clicked).special <> "submenu" Then
                                            While totalMenuPanels
                                                GoSub closeMenuPanel
                                            Wend
                                        Else
                                            GoSub openMenuPanel
                                        End If
                                End Select
                            End If
                        ElseIf mouseDownOn = 0 Then
                            focus = 0
                        End If
                        If focus = 0 And control(menuPanel(totalMenuPanels)).active Then GoSub closeMenuPanel
                    End If
                    mouseDown = 0
                    mouseDownOn = 0
                    draggingForm = 0
                End If

                If clicked Then
                    lastClickedControl = clicked
                    tui& = -1
                    Exit Function
                Else
                    If modalForm = 0 And control(focus).type <> controlType("menubar") And control(menuPanel(totalMenuPanels)).active = 0 Then
                        Exit Function
                    End If
                End If
                _Display
                _Limit 30
            Loop
        Case "control"
            tui& = lastClickedControl
        Case "get"
            temp = getParam(action, "control")

            If Len(temp) = 0 Then
                Select Case getNextParam(action)
                    Case "hover"
                        tui = hover
                        action = control(hover).name
                        Exit Function
                    Case "focus"
                        tui = focus
                        action = control(focus).name
                        Exit Function
                End Select
            End If

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
                Case "shadow": tui& = control(this).shadow
                Case "disabled": tui& = control(this).disabled
                Case "hidden": tui& = control(this).hidden
                Case "canreceivefocus": tui& = control(this).canReceiveFocus
                Case "name": action = control(this).name
                Case "caption": action = control(this).caption
                Case "text": action = control(this).text
                Case "selected"
                    If control(this).type = controlType("listbox") Then
                        temp = control(this).text
                        lineNum = control(this).value
                        GoSub getListboxItem
                        action = listItemText
                    Else
                        action = ""
                    End If
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
                    Case "modal"
                        temp = getParam(action, "control")
                        GoSub getControlID
                        If this = 0 Then
                            modalForm = 0
                        ElseIf control(this).type = controlType("form") Then
                            modalForm = this
                        End If
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
                        If passed(action, "fghotkey") Then defaults.fghotkey = Val(getParam(action, "fghotkey"))

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
                            temp = getParam(action, "text")
                            temp = Replace$(temp, "\r\n", Chr$(10))
                            temp = Replace$(temp, "\r", Chr$(10))
                            control(this).text = Replace$(temp, "\n", Chr$(10))
                        End If

                        If passed(action, "w") Then
                            control(this).w = Val(getParam(action, "w"))
                        ElseIf captionSet And control(this).special = "autosize" Then
                            Select Case control(this).type
                                Case controlType("label")
                                    control(this).w = Len(control(this).caption)
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
                            control(this).fghover = control(control(this).parent).fghover
                            control(this).bghover = control(control(this).parent).bghover
                        ElseIf result = "defaults" Then
                            control(this).fg = defaults.fg
                            control(this).bg = defaults.bg
                            control(this).fghover = defaults.fghover
                            control(this).bghover = defaults.bghover
                            control(this).fghotkey = defaults.fghotkey
                        End If

                        If passed(action, "fg") Then control(this).fg = Val(getParam(action, "fg"))
                        If passed(action, "bg") Then control(this).bg = Val(getParam(action, "bg"))
                        If passed(action, "fghover") Then control(this).fghover = Val(getParam(action, "fghover"))
                        If passed(action, "bghover") Then control(this).bghover = Val(getParam(action, "bghover"))

                        If passed(action, "value") Then control(this).value = Val(getParam(action, "value"))
                        If passed(action, "keybind") Then control(this).keybind = Val(getParam(action, "keybind"))

                        If passed(action, "shadow") Then control(this).shadow = (LCase$(getParam(action, "shadow")) = "true")
                        If passed(action, "disabled") Then control(this).disabled = (LCase$(getParam(action, "disabled")) = "true")
                        If passed(action, "hidden") Then control(this).hidden = (LCase$(getParam(action, "hidden")) = "true")
                        If passed(action, "canreceivefocus") Then control(this).canReceiveFocus = (LCase$(getParam(action, "canreceivefocus")) = "true")
                End Select
            Loop
        Case "delete"
            temp = getParam(action, "control")
            If Len(temp) Then
                GoSub getControlID
                If this Then
                    control(this).active = 0
                    If modalForm = this Then modalForm = 0
                    For i = this + 1 To UBound(control)
                        If control(i).parent = this Then control(i).active = 0
                    Next
                End If
            End If
        Case Else
            Cls
            Print "unknown action: "; getAction(action)
            End
    End Select

    Exit Function

    getParentID:
    'temp contains the name of the parent control
    For j = 1 To UBound(control)
        If control(j).name = temp Then
            Return
        End If
    Next
    j = 0
    Return

    getControlID:
    'temp contains the name of the control we're looking for
    If Val(temp) > 0 Then
        this = Val(temp)
    Else
        this = 0
        For j = 1 To UBound(control)
            If control(j).name = temp Then
                this = j
                Return
            End If
        Next
    End If
    Return

    setAutoWidth:
    Select Case control(this).type
        Case controlType("textbox")
            control(this).w = Len(control(this).text)
            If control(this).w < 10 Then control(this).w = 10
        Case controlType("checkbox")
            control(this).w = Len(control(this).caption) + 4
        Case controlType("listbox")
            control(this).w = 20
        Case Else
            control(this).w = Len(control(this).caption)
    End Select
    Return

    openMenuPanel:
    If control(hover).type <> controlType("menubar") And control(hover).type <> controlType("menuitem") Then hover = focus
    If control(hover).type <> controlType("menubar") And control(hover).type <> controlType("menuitem") Then Return
    If modalForm Then Return

    If control(menuPanel(totalMenuPanels)).parent = hover Then Return
    If control(hover).type = controlType("menuitem") And control(hover).special <> "submenu" Then Return

    If control(hover).type = controlType("menubar") Then
        While totalMenuPanels
            GoSub closeMenuPanel
        Wend
    End If

    totalMenuPanels = totalMenuPanels + 1
    menuPanel(totalMenuPanels) = tui("add type=menupanel;name=tuimenupanel" + Str$(totalMenuPanels))
    menuPanelParents = menuPanelParents + MKL$(hover) + MKL$(-1)
    control(menuPanel(totalMenuPanels)).fg = control(hover).fg
    control(menuPanel(totalMenuPanels)).bg = control(hover).bg
    control(menuPanel(totalMenuPanels)).fghover = control(hover).fghover
    control(menuPanel(totalMenuPanels)).bghover = control(hover).bghover

    If control(hover).type = controlType("menuitem") Then
        control(menuPanel(totalMenuPanels)).x = control(hover).x + control(menuPanel(totalMenuPanels - 1)).w - 2
        control(menuPanel(totalMenuPanels)).y = control(hover).y - 1
    Else
        control(menuPanel(totalMenuPanels)).x = control(hover).x
        control(menuPanel(totalMenuPanels)).y = control(hover).y + 1
    End If

    control(menuPanel(totalMenuPanels)).active = -1
    control(menuPanel(totalMenuPanels)).w = 4
    control(menuPanel(totalMenuPanels)).parent = hover

    totalMenuPanelItems = 0
    focus = 0
    For j = 1 To UBound(control)
        If control(j).type = controlType("menuitem") And control(j).parent = hover Then
            If focus = 0 Then focus = j
            totalMenuPanelItems = totalMenuPanelItems + 1
            control(j).x = control(menuPanel(totalMenuPanels)).x + 2
            control(j).y = control(menuPanel(totalMenuPanels)).y + totalMenuPanelItems
            If control(j).special = "submenu" And Right$(control(j).caption, 3) <> Space$(3) Then
                control(j).caption = control(j).caption + Space$(3)
            End If
            If control(menuPanel(totalMenuPanels)).w < Len(control(j).caption) + 4 Then control(menuPanel(totalMenuPanels)).w = Len(control(j).caption) + 4
        End If
    Next
    control(menuPanel(totalMenuPanels)).h = totalMenuPanelItems + 2

    While control(menuPanel(totalMenuPanels)).x + control(menuPanel(totalMenuPanels)).w > _Width
        control(menuPanel(totalMenuPanels)).x = control(menuPanel(totalMenuPanels)).x - 1
        If control(menuPanel(totalMenuPanels)).x < 1 Then
            Color 7, 0
            Cls
            Print "Error positioning menu on screen."
            End
        End If
        For j = 1 To UBound(control)
            If control(j).type = controlType("menuitem") And control(j).parent = control(menuPanel(totalMenuPanels)).parent Then control(j).x = control(j).x - 1
        Next
    Wend
    Return

    closeMenuPanel:
    If totalMenuPanels > 0 Then
        control(menuPanel(totalMenuPanels)).active = 0
        totalMenuPanels = totalMenuPanels - 1
        menuPanelParents = Left$(menuPanelParents, Len(menuPanelParents) - 8)
        If totalMenuPanels = 0 Then focus = prevFocus
    End If
    Return

    enableKeyboardControl:
    keyboardControl = -1
    oldmx = mx
    oldmy = my
    Return

    setTextboxCursorFromMouse:
    this = mouseDownOn
    If this = 0 Then Return

    temp = control(this).text
    x = control(this).x
    y = control(this).y
    this = control(this).parent
    While this > 0
        x = x + control(this).x
        y = y + control(this).y
        this = control(this).parent
    Wend

    If control(mouseDownOn).type = controlType("textbox") Then
        displayStart = 1
        If Len(temp) > control(mouseDownOn).w Then
            displayStart = control(mouseDownOn).cursorPos - control(mouseDownOn).w + 1
            If displayStart < 1 Then displayStart = 1
            If displayStart + control(mouseDownOn).w - 1 > Len(temp) Then displayStart = Len(temp) - control(mouseDownOn).w + 1
        End If

        control(mouseDownOn).cursorPos = mx - x + displayStart
        If control(mouseDownOn).cursorPos < 1 Then control(mouseDownOn).cursorPos = 1
        If control(mouseDownOn).cursorPos > Len(temp) + 1 Then control(mouseDownOn).cursorPos = Len(temp) + 1
    Else
        If control(mouseDownOn).scrollTop < 1 Then control(mouseDownOn).scrollTop = 1
        textWidth = control(mouseDownOn).w - 1
        If textWidth < 1 Then textWidth = 1
        row = my - y + 1
        If row < 1 Then row = 1
        If row > control(mouseDownOn).h Then row = control(mouseDownOn).h
        targetVisualLine = control(mouseDownOn).scrollTop + row - 1

        If mx >= x + control(mouseDownOn).w - 1 Then
            totalVisualLines = 0
            lineStart = 1
            Do
                lineEnd = InStr(lineStart, temp, Chr$(10))
                If lineEnd = 0 Then lineEnd = Len(temp) + 1
                lineLen = lineEnd - lineStart
                If lineLen = 0 Then
                    totalVisualLines = totalVisualLines + 1
                Else
                    segmentStart = 1
                    While segmentStart <= lineLen
                        remaining = Mid$(temp, lineStart + segmentStart - 1)
                        remaining = Left$(remaining, lineLen - segmentStart + 1)
                        If Len(remaining) <= textWidth Then
                            segmentLen = Len(remaining)
                            segmentAdvance = segmentLen
                        Else
                            wrapPos = _InStrRev(Left$(remaining, textWidth + 1), " ")
                            If wrapPos <= 1 Then
                                segmentLen = textWidth
                                segmentAdvance = segmentLen
                            Else
                                segmentLen = wrapPos - 1
                                segmentAdvance = wrapPos
                            End If
                        End If
                        totalVisualLines = totalVisualLines + 1
                        segmentStart = segmentStart + segmentAdvance
                    Wend
                End If
                If lineEnd > Len(temp) Then Exit Do
                lineStart = lineEnd + 1
            Loop
            If totalVisualLines < 1 Then totalVisualLines = 1
            If totalVisualLines > control(mouseDownOn).h And control(mouseDownOn).h > 1 Then
                control(mouseDownOn).scrollTop = ((row - 1) * (totalVisualLines - control(mouseDownOn).h)) \ (control(mouseDownOn).h - 1) + 1
                If control(mouseDownOn).scrollTop < 1 Then control(mouseDownOn).scrollTop = 1
                If control(mouseDownOn).scrollTop > totalVisualLines - control(mouseDownOn).h + 1 Then control(mouseDownOn).scrollTop = totalVisualLines - control(mouseDownOn).h + 1
            End If
            Return
        End If

        mouseTextCol = mx - x + 1
        If mouseTextCol < 1 Then mouseTextCol = 1
        If mouseTextCol > textWidth + 1 Then mouseTextCol = textWidth + 1

        visualLine = 1
        lineStart = 1
        Do
            lineEnd = InStr(lineStart, temp, Chr$(10))
            If lineEnd = 0 Then
                lineEnd = Len(temp) + 1
            End If
            lineLen = lineEnd - lineStart
            If lineLen = 0 Then
                If visualLine = targetVisualLine Then
                    control(mouseDownOn).cursorPos = lineStart
                    Return
                End If
                visualLine = visualLine + 1
            Else
                segmentStart = 1
                While segmentStart <= lineLen
                    remaining = Mid$(temp, lineStart + segmentStart - 1)
                    remaining = Left$(remaining, lineLen - segmentStart + 1)
                    If Len(remaining) <= textWidth Then
                        segmentLen = Len(remaining)
                        segmentAdvance = segmentLen
                    Else
                        wrapPos = _InStrRev(Left$(remaining, textWidth + 1), " ")
                        If wrapPos <= 1 Then
                            segmentLen = textWidth
                            segmentAdvance = segmentLen
                        Else
                            segmentLen = wrapPos - 1
                            segmentAdvance = wrapPos
                        End If
                    End If
                    If visualLine = targetVisualLine Then
                        control(mouseDownOn).cursorPos = lineStart + segmentStart + mouseTextCol - 2
                        If control(mouseDownOn).cursorPos > lineStart + segmentStart + segmentLen - 1 Then control(mouseDownOn).cursorPos = lineStart + segmentStart + segmentLen - 1
                        If mouseTextCol > segmentLen Then control(mouseDownOn).cursorPos = lineStart + segmentStart + segmentLen - 1
                        If control(mouseDownOn).cursorPos < lineStart + segmentStart - 1 Then control(mouseDownOn).cursorPos = lineStart + segmentStart - 1
                        Return
                    End If
                    visualLine = visualLine + 1
                    segmentStart = segmentStart + segmentAdvance
                Wend
            End If
            If lineEnd > Len(temp) Then Exit Do
            lineStart = lineEnd + 1
        Loop
        control(mouseDownOn).cursorPos = Len(temp) + 1
    End If
    Return

    countListboxItems:
    lineCount = 0
    If Len(temp) = 0 Then Return
    lineStart = 1
    Do
        lineCount = lineCount + 1
        lineEnd = InStr(lineStart, temp, Chr$(10))
        If lineEnd = 0 Then Exit Do
        lineStart = lineEnd + 1
    Loop
    Return

    getListboxItem:
    listItemText = ""
    If lineNum < 1 Then Return
    lineStart = 1
    For j = 1 To lineNum - 1
        lineEnd = InStr(lineStart, temp, Chr$(10))
        If lineEnd = 0 Then Return
        lineStart = lineEnd + 1
    Next
    lineEnd = InStr(lineStart, temp, Chr$(10))
    If lineEnd = 0 Then
        listItemText = Mid$(temp, lineStart)
    Else
        listItemText = Mid$(temp, lineStart, lineEnd - lineStart)
    End If
    Return

    listboxSyncScroll:
    row = control(this).h - 2
    If row < 1 Then row = 1
    If control(this).value < 1 Then Return
    If control(this).scrollTop < 1 Then control(this).scrollTop = 1
    If control(this).value >= control(this).scrollTop + row Then control(this).scrollTop = control(this).value - row + 1
    If control(this).value < control(this).scrollTop Then control(this).scrollTop = control(this).value
    If lineCount > row Then
        If control(this).scrollTop > lineCount - row + 1 Then control(this).scrollTop = lineCount - row + 1
    End If
    If control(this).scrollTop < 1 Then control(this).scrollTop = 1
    Return

    setListboxFromMouse:
    this = mouseDownOn
    If this = 0 Then Return
    temp = control(this).text
    GoSub countListboxItems
    x = control(this).x
    y = control(this).y
    j = control(this).parent
    While j > 0
        x = x + control(j).x
        y = y + control(j).y
        j = control(j).parent
    Wend
    row = control(this).h - 2
    If row < 1 Then row = 1
    If mx >= x + control(this).w - 1 And lineCount > row Then
        targetVisualLine = my - y
        If targetVisualLine < 1 Then targetVisualLine = 1
        If targetVisualLine > row Then targetVisualLine = row
        If lineCount > row And row > 1 Then
            control(this).scrollTop = ((targetVisualLine - 1) * (lineCount - row)) \ (row - 1) + 1
            If control(this).scrollTop < 1 Then control(this).scrollTop = 1
            If control(this).scrollTop > lineCount - row + 1 Then control(this).scrollTop = lineCount - row + 1
        End If
        Return
    End If
    targetVisualLine = my - y
    If targetVisualLine < 1 Then targetVisualLine = 1
    If targetVisualLine > row Then targetVisualLine = row
    lineNum = control(this).scrollTop + targetVisualLine - 1
    If lineNum >= 1 And lineNum <= lineCount Then
        control(this).value = lineNum
        GoSub listboxSyncScroll
    End If
    Return

End Function

Sub tuiSetColor (fg As Integer, bg As Integer)
    If fg > -1 Then Color fg
    If bg > -1 Then Color , bg
End Sub

Function controlType& (__a$)
    Dim typeList$
    typeList$ = "@form@button@checkbox@label@textbox@textarea@listbox@menubar@menuitem@menupanel@"

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
    getParam$ = Left$(result, InStr(result, sep) - 1)
End Function

Function Replace$ (__base$, __search$, __replace$)
    Dim As String result, segment
    Dim As Long position, start

    If Len(__search$) = 0 Then
        Replace$ = __base$
        Exit Function
    End If

    result$ = ""
    start = 1
    position = InStr(start, __base$, __search$)
    While position > 0
        result$ = result$ + Mid$(__base$, start, position - start) + __replace$
        start = position + Len(__search$)
        position = InStr(start, __base$, __search$)
    Wend
    result$ = result$ + Mid$(__base$, start)
    Replace$ = result$
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

Function timeElapsedSince! (startTime!)
    If startTime! > Timer Then startTime! = startTime! - 86400
    timeElapsedSince! = Timer - startTime!
End Function
