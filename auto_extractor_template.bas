Type Archive_Header
    As String * 4 SIGNATURE
    As _Unsigned Long ENTRY_COUNT
End Type
Type ENTRY
    As _Unsigned _Byte TYPE, CTYPE
    As _Unsigned Integer FILE_NAME_LENGTH
    As _Unsigned Long FILE_SIZE, C_FILE_SIZE
    As _Unsigned Long FILE_HASH
End Type

Const SIGNATURE = "QBA2"
Dim As Archive_Header Archive_Header
Dim As ENTRY Entry, EmptyEntry
Dim As String ENTRY_FILE_NAME, ENTRY_FILE_CONTENT

Screen _NewImage(640, 240, 32)
_Title "Auto-Extractor"

Cls , -1
Color _RGB32(0), 0

ARCHIVE$ = ""
EXTRACT_PATH$ = ""

If _FileExists(ARCHIVE$) = 0 Then Dialog "Archive " + ARCHIVE$ + " does not exists": End
Open ARCHIVE$ For Binary As #1
Get #1, , Archive_Header
If Archive_Header.SIGNATURE <> SIGNATURE Then Dialog "Corrupted Archive File": End

If Len(EXTRACT_PATH$) Then
    If _DirExists(EXTRACT_PATH$) = 0 Then MkDir EXTRACT_PATH$
    ChDir EXTRACT_PATH$
End If
For I = 1 To Archive_Header.ENTRY_COUNT
    Get #1, , Entry
    ENTRY_FILE_NAME = String$(Entry.FILE_NAME_LENGTH, 0)
    Get #1, , ENTRY_FILE_NAME
    FILE_NAME$ = ENTRY_FILE_NAME
    Cls , -1
    _PrintString (264, 16), "Auto Extractor"
    _PrintString (10, 48), "Extracting in '" + EXTRACT_PATH$ + "'"
    P = I / Archive_Header.ENTRY_COUNT
    ShowProgressBar 10, 220, 620, 230, P
    P = P * 100
    P = Int(P * 100) / 100
    _Title "Extracting " + _Trim$(Str$(P)) + "%"
    Select Case Entry.TYPE
        Case 1
            _PrintString (10, 64), "Extracting File: " + EXTRACT_PATH$ + "\" + FILE_NAME$
            If Entry.FILE_SIZE Then
                ENTRY_FILE_CONTENT = String$(Entry.C_FILE_SIZE, 0)
                Get #1, , ENTRY_FILE_CONTENT
                Select Case Entry.CTYPE
                    Case 0: FILE$ = ENTRY_FILE_CONTENT
                    Case 1: FILE$ = FrequencyDeCompress$(ENTRY_FILE_CONTENT)
                    Case 2: FILE$ = OneByteDecode$(ENTRY_FILE_CONTENT)
                    Case 3: FILE$ = _Inflate$(ENTRY_FILE_CONTENT)
                    Case 4: FILE$ = RLEDecode$(ENTRY_FILE_CONTENT)
                End Select
                ENTRY_FILE_CONTENT = ""
                If CRC32(FILE$) <> Entry.FILE_HASH Then
                    Dialog "File '" + FILE_NAME$ + "' is corrupted"
                    End
                End If
            End If
            Open FILE_NAME$ For Binary As #2
            If Entry.FILE_SIZE Then Put #2, , FILE$
            Close #2
            If _FileExists(FILE_NAME$) = 0 Then
                Dialog "Extracting Failed: '" + FILE_NAME$ + "' to '" + EXTRACT_PATH$ + "'."
                End
            End If
            FILE$ = ""
        Case 2
            _PrintString (10, 64), "Creating Directory: " + EXTRACT_PATH$ + "\" + FILE_NAME$
            If _DirExists(FILE_NAME$) = 0 Then MkDir FILE_NAME$
    End Select
    _Display
Next I
Cls , -1
_PrintString (264, 16), "Auto Extractor"
_PrintString (10, 48), "Extracting in '" + EXTRACT_PATH$ + "'"
_Title "Extraction Complete"
Dialog "Extraction Complete"
End

Sub ShowProgressBar (X1, Y1, X2, Y2, P)
    Line (X1 - 1, Y1 - 1)-(X2 + 1, Y2 + 1), _RGB32(0), B
    X = (X2 - X1) * P + X1
    Line (X1, Y1)-(X, Y2), _RGB32(0, 255, 0), BF
End Sub

Sub Dialog (Text$)
    Line (0, 0)-(_Width - 1, _Height - 1), _RGB32(0, 63), BF
    hW = _Width / 2
    hH = _Height / 2
    hFH = _FontHeight / 2
    hL = Len(Text$) * _FontWidth / 2
    Line (hW - hL - 8, hH - hFH - 8)-(hW + hL + 8, hH + hFH + 8), _RGB32(0), BF
    Line (hW - hL - 4, hH - hFH - 4)-(hW + hL + 4, hH + hFH + 4), -1, BF
    Color _RGB32(0), -1
    _PrintString (hW - hL, hH - hFH), Text$
    _Display
End Sub

Function CeilDivision~& (A~&, B~&)
    CeilDivision~& = A~& \ B~& + Sgn(A~& Mod B~&)
End Function
Function OneByteEncode$ (__I$)
    Dim As _Unsigned _Byte __ONEBYTE, __C
    Dim As _Unsigned Long __BYTE_BUFFER_OFFSET, __POSITION_BUFFER_OFFSET, __I, __LENA, __Frequency_Table(0 To 255)
    Dim __J As _Unsigned _Bit * 3
    Dim As String __BYTE_BUFFER, __POSITION_BUFFER
    __LENA = Len(__I$)
    For __I = 1 To __LENA
        __BYTE~%% = Asc(__I$, __I)
        __Frequency_Table(__BYTE~%%) = __Frequency_Table(__BYTE~%%) + 1
    Next __I
    For __BI~%% = 0 To 255
        If __Frequency_Table(__BI~%%) > __Frequency_Table(__ONEBYTE) Then __ONEBYTE = __BI~%%
    Next __BI~%%
    __BYTE_BUFFER = String$(Len(__I$), 0): __POSITION_BUFFER = String$(CeilDivision~&(Len(__I$), 8) + 1, 0)
    For __I = 1 To Len(__I$)
        __C = Asc(__I$, __I): If __J = 0 Then __POSITION_BUFFER_OFFSET = __POSITION_BUFFER_OFFSET + 1
        If __C <> __ONEBYTE Then
            Asc(__POSITION_BUFFER, __POSITION_BUFFER_OFFSET) = _SetBit(Asc(__POSITION_BUFFER, __POSITION_BUFFER_OFFSET), __J)
            __BYTE_BUFFER_OFFSET = __BYTE_BUFFER_OFFSET + 1: Asc(__BYTE_BUFFER, __BYTE_BUFFER_OFFSET) = __C
        End If
        __J = __J + 1
    Next __I
    __POSITION_BUFFER = _Deflate$(Left$(__POSITION_BUFFER, __POSITION_BUFFER_OFFSET))
    __BYTE_BUFFER = _Deflate$(Left$(__BYTE_BUFFER, __BYTE_BUFFER_OFFSET))
    OneByteEncode$ = MKL$(Len(__I$)) + MKL$(Len(__POSITION_BUFFER)) + MKL$(Len(__BYTE_BUFFER)) + Chr$(__ONEBYTE) + __POSITION_BUFFER + __BYTE_BUFFER
    __POSITION_BUFFER = ""
    __BYTE_BUFFER = ""
End Function
Function OneByteDecode$ (__I$)
    Dim As _Unsigned Long __I, __BYTE_BUFFER_OFFSET, __POSITION_BUFFER_OFFSET
    Dim As _Unsigned _Bit * 3 __J
    Dim As String __BYTE_BUFFER, __POSITION_BUFFER, __OUT_BUFFER
    __OUT_LENGTH~& = CVL(Left$(__I$, 4))
    __POSITION_BUFFER_DEFLATE_LENGTH~& = CVL(Mid$(__I$, 5, 4))
    __BYTE_BUFFER_DEFLATE_LENGTH~& = CVL(Mid$(__I$, 9, 4))
    __ONEBYTE~%% = Asc(__I$, 13)
    __POSITION_BUFFER = _Inflate$(Mid$(__I$, 14, __POSITION_BUFFER_DEFLATE_LENGTH~&))
    __BYTE_BUFFER = _Inflate$(Mid$(__I$, 14 + __POSITION_BUFFER_DEFLATE_LENGTH~&, __BYTE_BUFFER_DEFLATE_LENGTH~&))
    __OUT_BUFFER = String$(__OUT_LENGTH~&, 0)
    __POSITION_BUFFER_OFFSET = 0
    __BYTE_BUFFER_OFFSET = 0
    For __I = 1 To __OUT_LENGTH~&
        If __J = 0 Then __POSITION_BUFFER_OFFSET = __POSITION_BUFFER_OFFSET + 1
        If _ReadBit(Asc(__POSITION_BUFFER, __POSITION_BUFFER_OFFSET), __J) Then
            __BYTE_BUFFER_OFFSET = __BYTE_BUFFER_OFFSET + 1
            Asc(__OUT_BUFFER, __I) = Asc(__BYTE_BUFFER, __BYTE_BUFFER_OFFSET)
        Else
            Asc(__OUT_BUFFER, __I) = __ONEBYTE~%%
        End If
        __J = __J + 1
    Next __I
    __POSITION_BUFFER = ""
    __BYTE_BUFFER = ""
    OneByteDecode = __OUT_BUFFER
End Function
Function RLEEncode$ (__I$)
    Dim As _Unsigned _Byte __CB, __LB, __C
    Dim As Long __I
    Dim As String __OUT_BUFFER
    __OUT_BUFFER = String$(Len(__I$) * 2, 0)
    __LB = Asc(__I$, 1)
    __C = 1
    For __I = 2 To Len(__I$)
        __CB = Asc(__I$, __I)
        If __CB = __LB And __C < 255 Then
            __C = __C + 1
        Else
            __OUT_BUFFER_OFFSET = __OUT_BUFFER_OFFSET + 1
            Asc(__OUT_BUFFER, __OUT_BUFFER_OFFSET) = __LB
            __OUT_BUFFER_OFFSET = __OUT_BUFFER_OFFSET + 1
            Asc(__OUT_BUFFER, __OUT_BUFFER_OFFSET) = __C
            __C = 1
            __LB = __CB
        End If
    Next __I
    __OUT_BUFFER_OFFSET = __OUT_BUFFER_OFFSET + 1
    Asc(__OUT_BUFFER, __OUT_BUFFER_OFFSET) = __LB
    __OUT_BUFFER_OFFSET = __OUT_BUFFER_OFFSET + 1
    Asc(__OUT_BUFFER, __OUT_BUFFER_OFFSET) = __C
    If 5 + __OUT_BUFFER_OFFSET > Len(__I$) Then
        RLEEncode$ = Chr$(0) + __I$
    Else
        RLEEncode$ = Chr$(1) + MKL$(Len(__I$)) + Left$(__OUT_BUFFER, __OUT_BUFFER_OFFSET + 1)
    End If
    __OUT_BUFFER = ""
End Function
Function RLEDecode$ (__I$)
    Dim As _Unsigned _Byte __B, __C
    Dim As Long __I, __OUT_BUFFER_OFFSET
    Dim As String __OUT_BUFFER
    If Asc(__I$, 1) = 0 Then
        RLEDecode$ = Mid$(__I$, 2)
        Exit Function
    End If
    __OUT_LENGTH~& = CVL(Mid$(__I$, 2, 4))
    __OUT_BUFFER = String$(__OUT_LENGTH~&, 0)
    __OUT_BUFFER_OFFSET = 1
    For __I = 6 To Len(__I$) - 1
        __B = Asc(__I$, __I): __I = __I + 1: __C = Asc(__I$, __I)
        Mid$(__OUT_BUFFER, __OUT_BUFFER_OFFSET, __C) = String$(__C, __B)
        __OUT_BUFFER_OFFSET = __OUT_BUFFER_OFFSET + __C
    Next __I
    RLEDecode$ = __OUT_BUFFER
    __OUT_BUFFER = ""
End Function
Function FrequencyCompress$ (__I$)
    Dim As _Unsigned _Byte __Code_Table(0 To 255), __Inverse_Code_Table(0 To 255)
    Dim As _Unsigned Long __Frequency_Table(0 To 255)
    Dim As _Unsigned Long __I, __LENA
    __LENA = Len(__I$)
    For __I = 1 To __LENA
        __BYTE~%% = Asc(__I$, __I)
        __Frequency_Table(__BYTE~%%) = __Frequency_Table(__BYTE~%%) + 1
    Next __I
    For __BJ~%% = 0 To 255
        For __BI~%% = 0 To 255
            If __Frequency_Table(__BI~%%) > __Frequency_Table(__MAXBYTE~%%) Then __MAXBYTE~%% = __BI~%%
        Next __BI~%%
        __Code_Table(__MAXBYTE~%%) = __BJ~%%
        __Inverse_Code_Table(__BJ~%%) = __MAXBYTE~%%
        __Frequency_Table(__MAXBYTE~%%) = 0
        __MAXBYTE~%% = __MAXBYTE~%% + 1
    Next __BJ~%%
    __B$ = String$(256 + __LENA, 0)
    For __I = 0 To 255
        Asc(__B$, __I + 1) = __Inverse_Code_Table(__I)
    Next __I
    For __I = 1 To __LENA
        Asc(__B$, 256 + __I) = __Code_Table(Asc(__I$, __I))
    Next __I
    __B$ = ZeroByteEncode$(__B$)
    FrequencyCompress$ = __B$
    __B$ = ""
End Function
Function FrequencyDeCompress$ (__I$)
    Dim __Inverse_Code_Table(0 To 255) As _Unsigned _Byte
    Dim As _Unsigned Long __I, __LENA
    __B$ = ZeroByteDecode$(__I$)
    __LENA = Len(__B$) - 256
    For __I = 0 To 255
        __Inverse_Code_Table(__I) = Asc(__B$, __I + 1)
    Next __I
    __O$ = String$(__LENA, 0)
    For __I = 1 To __LENA
        Asc(__O$, __I) = __Inverse_Code_Table(Asc(__B$, __I + 256))
    Next __I
    FrequencyDeCompress$ = __O$
    __O$ = ""
End Function
Function ZeroByteEncode$ (__I$)
    Dim As _Unsigned _Byte __C
    Dim As _Unsigned Long __BYTE_BUFFER_OFFSET, __POSITION_BUFFER_OFFSET, __I
    Dim __J As _Unsigned _Bit * 3
    Dim As String __BYTE_BUFFER, __POSITION_BUFFER
    __BYTE_BUFFER = String$(Len(__I$), 0): __POSITION_BUFFER = String$(CeilDivision~&(Len(__I$), 8) + 1, 0)
    For __I = 1 To Len(__I$)
        __C = Asc(__I$, __I): If __J = 0 Then __POSITION_BUFFER_OFFSET = __POSITION_BUFFER_OFFSET + 1
        If __C Then
            Asc(__POSITION_BUFFER, __POSITION_BUFFER_OFFSET) = _SetBit(Asc(__POSITION_BUFFER, __POSITION_BUFFER_OFFSET), __J)
            __BYTE_BUFFER_OFFSET = __BYTE_BUFFER_OFFSET + 1: Asc(__BYTE_BUFFER, __BYTE_BUFFER_OFFSET) = __C
        End If
        __J = __J + 1
    Next __I
    __POSITION_BUFFER = _Deflate$(Left$(__POSITION_BUFFER, __POSITION_BUFFER_OFFSET))
    __BYTE_BUFFER = _Deflate$(Left$(__BYTE_BUFFER, __BYTE_BUFFER_OFFSET))
    ZeroByteEncode$ = MKL$(Len(__I$)) + MKL$(__POSITION_BUFFER_OFFSET) + MKL$(__BYTE_BUFFER_OFFSET) + MKL$(Len(__POSITION_BUFFER)) + MKL$(Len(__BYTE_BUFFER)) + __POSITION_BUFFER + __BYTE_BUFFER
    __POSITION_BUFFER = ""
    __BYTE_BUFFER = ""
End Function
Function ZeroByteDecode$ (__I$)
    Dim As _Unsigned Long __I, __BYTE_BUFFER_OFFSET, __POSITION_BUFFER_OFFSET
    Dim As _Unsigned _Byte __C
    Dim As _Unsigned _Bit * 3 __J
    Dim As String __BYTE_BUFFER, __POSITION_BUFFER, __OUT_BUFFER
    __OUT_LENGTH~& = CVL(Left$(__I$, 4))
    __POSITION_BUFFER_LENGTH~& = CVL(Mid$(__I$, 5, 4))
    __BYTE_BUFFER_LENGTH~& = CVL(Mid$(__I$, 9, 4))
    __POSITION_BUFFER_DEFLATE_LENGTH~& = CVL(Mid$(__I$, 13, 4))
    __BYTE_BUFFER_DEFLATE_LENGTH~& = CVL(Mid$(__I$, 17, 4))
    __POSITION_BUFFER = _Inflate$(Mid$(__I$, 21, __POSITION_BUFFER_DEFLATE_LENGTH~&))
    __BYTE_BUFFER = _Inflate$(Mid$(__I$, 21 + __POSITION_BUFFER_DEFLATE_LENGTH~&, __BYTE_BUFFER_DEFLATE_LENGTH~&))
    __OUT_BUFFER = String$(__OUT_LENGTH~&, 0)
    __POSITION_BUFFER_OFFSET = 0
    __BYTE_BUFFER_OFFSET = 0
    For __I = 1 To __OUT_LENGTH~&
        If __J = 0 Then __POSITION_BUFFER_OFFSET = __POSITION_BUFFER_OFFSET + 1
        If _ReadBit(Asc(__POSITION_BUFFER, __POSITION_BUFFER_OFFSET), __J) Then
            __BYTE_BUFFER_OFFSET = __BYTE_BUFFER_OFFSET + 1
            Asc(__OUT_BUFFER, __I) = Asc(__BYTE_BUFFER, __BYTE_BUFFER_OFFSET)
        End If
        __J = __J + 1
    Next __I
    ZeroByteDecode = __OUT_BUFFER
End Function
Function CRC32~& (__IN$)
    Dim As _Unsigned Long __CRC32_POLY, __CRC, __I
    Dim As _Unsigned _Byte __J
    __CRC32_POLY = &HEDB88320
    __CRC = &HFFFFFFFF
    For __I = 1 To Len(__IN$)
        __CRC = __CRC Xor Asc(__IN$, __I)
        For __J = 1 To 8
            If __CRC And 1 Then __CRC = (__CRC \ 2) Xor __CRC32_POLY Else __CRC = __CRC \ 2
        Next __J
    Next __I
    CRC32~& = Not __CRC
End Function
Function CountChars~& (A$, B~%%)
    Dim As _Unsigned Long Count, I
    For I = 1 To Len(A$)
        If Asc(A$, I) = B~%% Then Count = Count + 1
    Next I
    CountChars~& = Count
End Function
Function LongToHex$ (A As _Unsigned Long)
    H$ = Hex$(A)
    LongToHex$ = String$(8 - Len(H$), "0") + H$
End Function
Function FILENAME$ (I$)
    If InStr(I$, "\") Then FILENAME$ = Mid$(I$, _InStrRev(I$, "\") + 1) Else FILENAME$ = I$
End Function
