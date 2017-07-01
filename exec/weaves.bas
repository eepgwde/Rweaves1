Attribute VB_Name = "weaves"
Function getDocuments(base0 As Variant, tail0 As Variant) As String
    If IsEmpty(base0) Then
        base0 = Environ$("USERPROFILE")
    End If
    If IsEmpty(tail0) Then
        tail0 = CStr("Documents")
    End If
    getDocuments = CStr(base0) & "\" & CStr(tail0)
End Function

Sub ToCSV()
Attribute ToCSV.VB_Description = "Save to CSV"
Attribute ToCSV.VB_ProcData.VB_Invoke_Func = "C\n14"
'
' ToCSV Macro
' Save to CSV
'
' Keyboard Shortcut: Ctrl+Shift+C
'

    Dim wb As Workbook
    Dim sh As Worksheet
    
    Dim path0 As String
    
    path0 = getDocuments(Empty, Empty)
    
    Dim updating0 As Boolean
    
    updating0 = Application.ScreenUpdating
    Application.ScreenUpdating = False
    
    Set sh = ActiveSheet
    
    path1 = path0 & "\" & sh.Name & ".csv"

    Set wb = Workbooks.Add
    sh.Activate
    sh.Select
    sh.Copy Before:=wb.Sheets(1)
    
    Application.DisplayAlerts = False
    
    wb.SaveAs Filename:=path1, FileFormat:=xlCSV, CreateBackup:=True, _
        ConflictResolution:=Excel.xlLocalSessionChanges
        
    Application.DisplayAlerts = True
    
    wb.Close SaveChanges:=False
    Application.ScreenUpdating = updating0
    sh.Activate
    
End Sub
