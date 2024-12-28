$Console:Only
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


Const SIGNATURE = "QBA1"
Dim As Archive_Header Archive_Header
Dim As ENTRY Entry, EmptyEntry
Dim As String ENTRY_FILE_NAME, ENTRY_FILE_CONTENT

Dim As String FILEStack, RELATIVE_PATH
RELATIVE_PATH = _StartDir$ + "\"

ARCHIVE$ = Command$(2)

If _StriCmp(Command$(1), "-p") = 0 Then MODE = 1
If _StriCmp(Command$(1), "-u") = 0 Then MODE = 2
If _StriCmp(Command$(1), "-l") = 0 Then MODE = 3

If _CommandCount < 2 Or MODE = 0 Then
    HELP:
    Print "Multi Compressor Archive"
    Print
    Print "Pack:   archive -p [ARCHIVE NAME] [FILEs / FOLDERs] ..."
    Print "List:   archive -l"
    Print "Unpack: archive -u [ARCHIVE NAME] [DIRECTORY]"
    System
End If

If MODE <> 1 Then
    Open ARCHIVE$ For Binary As #1
    Get #1, , Archive_Header
    If Archive_Header.SIGNATURE <> SIGNATURE Then Print "Invalid Archive File": System
End If

Select Case MODE
    Case 1:
        If _FileExists(ARCHIVE$) Then
            Open ARCHIVE$ For Binary As #1
            Get #1, , Archive_Header
            If LOF(1) > 4 Then If Archive_Header.SIGNATURE <> SIGNATURE Then Print "Invalid Archive File": System
            Seek #1, LOF(1) + 1
        Else
            Open ARCHIVE$ For Binary As #1
            Archive_Header.SIGNATURE = SIGNATURE
        End If
        FILEStack = ListNew$
        For FILEI = 3 To _CommandCount
            FILEStack = StackAdd$(FILEStack, RELATIVE_PATH + Command$(FILEI))
        Next FILEI
        Seek #1, 1
        Put #1, , Archive_Header
        Seek #1, LOF(1) + 1
        Do
            If ListLength~&(FILEStack) = 0 Then Exit Do
            FILE_NAME$ = StackDelete$(FILEStack)
            If _DirExists(FILE_NAME$) Then
                Print "Entry "; LongToHex$(Archive_Header.ENTRY_COUNT); ": Adding Folder '"; FILE_NAME$; "' to Stack"
                Entry.TYPE = 2 'Dir
                ENTRY_FILE_NAME = _Deflate$(Mid$(FILE_NAME$, Len(RELATIVE_PATH) + 1))
                Entry.FILE_NAME_LENGTH = Len(ENTRY_FILE_NAME)
                Shell "dir /b /o:n " + Chr$(34) + FILE_NAME$ + Chr$(34) + " > tmp.txt"
                FILE_COUNT = 0
                Open "tmp.txt" For Input As #3
                Do
                    If LOF(3) = 0 Then Exit Do
                    Line Input #3, L$
                    FILEStack = StackAdd$(FILEStack, FILE_NAME$ + "\" + L$)
                    FILE_COUNT = FILE_COUNT + 1
                Loop Until EOF(3)
                Close #3
                Kill "tmp.txt"
                Entry.FILE_SIZE = FILE_COUNT
                Entry.C_FILE_SIZE = 0
                Entry.CTYPE = 0
                Entry.FILE_HASH = 0
                Put #1, , Entry
                Put #1, , ENTRY_FILE_NAME
                Archive_Header.ENTRY_COUNT = Archive_Header.ENTRY_COUNT + 1
            Else
                Entry.TYPE = 1 'File
                Print "Entry "; LongToHex$(Archive_Header.ENTRY_COUNT); ": Adding File '"; FILENAME$(FILE_NAME$); "' to Archive";
                ENTRY_FILE_NAME = _Deflate$(Mid$(FILE_NAME$, Len(RELATIVE_PATH) + 1))
                Entry.FILE_NAME_LENGTH = Len(ENTRY_FILE_NAME)
                Open FILE_NAME$ For Binary As #2
                FILE$ = String$(LOF(2), 0)
                Get #2, , FILE$
                Close #2
                If Len(FILE$) Then
                    Entry.FILE_HASH = CRC32(FILE$)
                    Print ", File Hash: "; LongToHex$(Entry.FILE_HASH);
                    FC$ = FrequencyCompress$(FILE$)
                    OBC$ = OneByteEncode$(FILE$)
                    DC$ = _Deflate$(FILE$)
                    RLEC$ = RLEEncode$(FILE$)
                    FCL& = Len(FC$)
                    OBCL& = Len(OBC$)
                    DCL& = Len(DC$)
                    RLECL& = Len(RLEC$)
                    Entry.FILE_SIZE = Len(FILE$)
                    Entry.C_FILE_SIZE = Entry.FILE_SIZE
                    Entry.CTYPE = 0
                    If Entry.C_FILE_SIZE > FCL& Then Entry.C_FILE_SIZE = FCL&: Entry.CTYPE = 1 'f
                    If Entry.C_FILE_SIZE > OBCL& Then Entry.C_FILE_SIZE = OBCL&: Entry.CTYPE = 2 '1B
                    If Entry.C_FILE_SIZE > DCL& Then Entry.C_FILE_SIZE = DCL&: Entry.CTYPE = 3 'deflate
                    If Entry.C_FILE_SIZE > RLECL& Then Entry.C_FILE_SIZE = RLECL&: netry.ctype = 4 'rle
                    Print ", Compression: ";
                    Select Case Entry.CTYPE
                        Case 0: ENTRY_FILE_CONTENT = FILE$
                            Print "None"
                        Case 1: ENTRY_FILE_CONTENT = FC$
                            Print "Frequency"
                        Case 2: ENTRY_FILE_CONTENT = OBC$
                            Print "One Byte"
                        Case 3: ENTRY_FILE_CONTENT = DC$
                            Print "Deflate"
                        Case 4: ENTRY_FILE_CONTENT = RLEC$
                            Print "RLE"
                    End Select
                    FILE$ = ""
                    FC$ = ""
                    OBC$ = ""
                    DC$ = ""
                    RLEC$ = ""
                Else
                    Entry.FILE_SIZE = 0
                    Entry.C_FILE_SIZE = 0
                End If
                Put #1, , Entry
                Put #1, , ENTRY_FILE_NAME
                Put #1, , ENTRY_FILE_CONTENT
                Archive_Header.ENTRY_COUNT = Archive_Header.ENTRY_COUNT + 1
            End If
            Entry = EmptyEntry
            ENTRY_FILE_NAME = ""
            ENTRY_FILE_CONTENT = ""
        Loop
        Seek #1, 1
        Put #1, , Archive_Header
    Case 2: If _DirExists(Command$(3)) = 0 Then MkDir Command$(3)
        ChDir Command$(3)
        For I = 1 To Archive_Header.ENTRY_COUNT
            Get #1, , Entry
            ENTRY_FILE_NAME = String$(Entry.FILE_NAME_LENGTH, 0)
            Get #1, , ENTRY_FILE_NAME
            FILE_NAME$ = _Inflate$(ENTRY_FILE_NAME)
            Select Case Entry.TYPE
                Case 1
                    Print "Extracting File: "; FILE_NAME$;
                    If Entry.FILE_SIZE Then
                        ENTRY_FILE_CONTENT = String$(Entry.C_FILE_SIZE, 0)
                        Get #1, , ENTRY_FILE_CONTENT
                        Select Case Entry.CTYPE
                            Case 1: FILE$ = FrequencyDeCompress$(ENTRY_FILE_CONTENT)
                            Case 2: FILE$ = OneByteDecode$(ENTRY_FILE_CONTENT)
                            Case 3: FILE$ = _Inflate$(ENTRY_FILE_CONTENT)
                            Case 4: FILE$ = RLEDecode$(ENTRY_FILE_CONTENT)
                        End Select
                        ENTRY_FILE_CONTENT = ""
                        If CRC32(FILE$) <> Entry.FILE_HASH Then Print "File is corrupted": System
                    End If
                    Open FILE_NAME$ For Binary As #2
                    If Entry.FILE_SIZE Then Put #2, , FILE$
                    Close #2
                    FILE$ = ""
                    Print " done"
                Case 2
                    Print "Creating Directory: "; FILE_NAME$;
                    MkDir FILE_NAME$
            End Select
        Next I
    Case 3: Print "Listing "; ARCHIVE$: Print
        For I = 1 To Archive_Header.ENTRY_COUNT
            Get #1, , Entry
            ENTRY_FILE_NAME = String$(Entry.FILE_NAME_LENGTH, 0)
            Get #1, , ENTRY_FILE_NAME
            FILE_NAME$ = _Inflate$(ENTRY_FILE_NAME)
            Select Case Entry.TYPE
                Case 1: Color 15, 0: Print String$(2 * CountChars(FILE_NAME$, 92), 32); FILENAME$(FILE_NAME$)
                    Seek #1, Seek(1) + Entry.C_FILE_SIZE
                Case 2: Color 9, 0: Print String$(2 * CountChars(FILE_NAME$, 92), 32); FILENAME$(FILE_NAME$); "\"
            End Select
        Next I
End Select
System

Function StackAdd$ (__Stack As String, __Item As String)
    StackAdd$ = ListAdd$(__Stack, __Item)
End Function
Function StackSee$ (__Stack As String)
    StackSee$ = ListGet$(__Stack, ListLength~&(__Stack))
End Function
Function StackDelete$ (__Stack As String)
    StackDelete$ = ListGet$(__Stack, ListLength~&(__Stack))
    __Stack = ListDelete$(__Stack, ListLength~&(__Stack))
End Function
Function ListNew$
    ListNew$ = MKL$(0)
End Function
Function ListLength~& (__List As String)
    ListLength~& = CVL(Mid$(__List, 1, 4))
End Function
Function ListAdd$ (__List As String, __Item As String)
    ListAdd$ = MKL$(CVL(Mid$(__List, 1, 4)) + 1) + Mid$(__List, 5) + MKI$(Len(__Item)) + __Item
End Function
Function ListGet$ (__List As String, __ItemNumber As _Unsigned Long)
    Dim As _Unsigned Long __nItems, __I, __OFFSET
    Dim As _Unsigned Integer __LEN
    __nItems = CVL(Mid$(__List, 1, 4))
    If __ItemNumber > __nItems Then Exit Function
    __OFFSET = 5
    For __I = 1 To __nItems
        __LEN = CVI(Mid$(__List, __OFFSET, 2))
        If __I = __ItemNumber Then ListGet$ = Mid$(__List, __OFFSET + 2, __LEN): Exit Function
        __OFFSET = __OFFSET + __LEN + 2
    Next __I
End Function
Function ListDelete$ (__List As String, __ItemNumber As _Unsigned Long)
    Dim As _Unsigned Long __nItems, __I, __OFFSET
    Dim As _Unsigned Integer __LEN
    __nItems = CVL(Mid$(__List, 1, 4))
    __OFFSET = 5
    For __I = 1 To __nItems
        __LEN = CVI(Mid$(__List, __OFFSET, 2))
        If __I = __ItemNumber Then
            ListDelete$ = MKL$(__nItems - 1) + Mid$(__List, 5, __OFFSET - 5) + Mid$(__List, __OFFSET + __LEN + 2)
            Exit Function
        End If
        __OFFSET = __OFFSET + __LEN + 2
    Next __I
End Function
Sub ListPrint (__List As String)
    Dim As _Unsigned Long __nItems, __I, __OFFSET
    Dim As _Unsigned Integer __LEN
    __nItems = CVL(Mid$(__List, 1, 4))
    __OFFSET = 5
    Print "[";
    For __I = 1 To __nItems
        __LEN = CVI(Mid$(__List, __OFFSET, 2))
        Print Mid$(__List, __OFFSET + 2, __LEN);
        If __I < __nItems Then Print ",";
        __OFFSET = __OFFSET + __LEN + 2
    Next __I
    Print "]"
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