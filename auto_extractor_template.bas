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

Const SIGNATURE = "QBA3"
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
                    Case 2: FILE$ = _Inflate$(ENTRY_FILE_CONTENT)
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
