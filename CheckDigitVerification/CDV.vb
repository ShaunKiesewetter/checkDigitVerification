Imports System.Text
Imports System.Diagnostics

Public Class CDV

    Private FOrigAcctNumber As String
    Private FAcctNumber As String
    Private FBranchCode As String
    Private FAcctType As Integer
    Private FnbOld As Boolean

    Private Function AcctDigit(ByVal Index As Integer) As Integer
        Return Val(FAcctNumber.Chars(Index))
    End Function


    'private int ExtractSingelDigit(int number, int modifier)
    '{
    '	if (modifier == 1)
    '				return number;
    '			else
    '			{
    '				int basen = number * 2;
    '				if (basen >= 10)
    '				{
    '					string snum = Convert.ToString(basen);
    '					int x = Convert.ToInt32(snum.Substring(0,1));
    '					int y = Convert.ToInt32(snum.Substring(1,1));
    '					return x + y;
    '				} 
    '				else
    '				{
    '					return basen;
    '				}
    '			}
    '		}

    Private Function ExtractSingelDigit(ByVal number As Integer, ByVal modifier As Integer) As Integer
        If (modifier = 1) Then
            Return number
        Else
            Dim basen As Integer = number * 2
            If (basen >= 10) Then
                Dim snum As String = Convert.ToString(basen)
                Dim x As Integer = Convert.ToInt32(snum.Substring(0, 1))
                Dim y As Integer = Convert.ToInt32(snum.Substring(1, 1))
                Return x + y
            Else
                Return basen
            End If
        End If
    End Function

    Public Function CreditCardCheck(ByVal CardNumber As String, ByVal expireyDate As DateTime) As Integer

        Dim thisMonth As Integer = DateTime.Today.Month
        Dim thisYear As Integer = DateTime.Today.Year
        ' Check expirey date
        If Not (expireyDate > DateTime.Today) Then
            Return 2 'Card expired
        End If
        'Check the cardnumber length
        If (CardNumber.Length <> 16) Then
            Return 1 ' Invalid card number format
        End If

        Dim cardnumbers(15) As Int32
        Try
            cardnumbers(0) = ExtractSingelDigit(System.Convert.ToInt32(CardNumber.Substring(0, 1)), 2)
            cardnumbers(1) = ExtractSingelDigit(System.Convert.ToInt32(CardNumber.Substring(1, 1)), 1)
            cardnumbers(2) = ExtractSingelDigit(System.Convert.ToInt32(CardNumber.Substring(2, 1)), 2)
            cardnumbers(3) = ExtractSingelDigit(System.Convert.ToInt32(CardNumber.Substring(3, 1)), 1)
            cardnumbers(4) = ExtractSingelDigit(System.Convert.ToInt32(CardNumber.Substring(4, 1)), 2)
            cardnumbers(5) = ExtractSingelDigit(System.Convert.ToInt32(CardNumber.Substring(5, 1)), 1)
            cardnumbers(6) = ExtractSingelDigit(System.Convert.ToInt32(CardNumber.Substring(6, 1)), 2)
            cardnumbers(7) = ExtractSingelDigit(System.Convert.ToInt32(CardNumber.Substring(7, 1)), 1)
            cardnumbers(8) = ExtractSingelDigit(System.Convert.ToInt32(CardNumber.Substring(8, 1)), 2)
            cardnumbers(9) = ExtractSingelDigit(System.Convert.ToInt32(CardNumber.Substring(9, 1)), 1)
            cardnumbers(10) = ExtractSingelDigit(System.Convert.ToInt32(CardNumber.Substring(10, 1)), 2)
            cardnumbers(11) = ExtractSingelDigit(System.Convert.ToInt32(CardNumber.Substring(11, 1)), 1)
            cardnumbers(12) = ExtractSingelDigit(System.Convert.ToInt32(CardNumber.Substring(12, 1)), 2)
            cardnumbers(13) = ExtractSingelDigit(System.Convert.ToInt32(CardNumber.Substring(13, 1)), 1)
            cardnumbers(14) = ExtractSingelDigit(System.Convert.ToInt32(CardNumber.Substring(14, 1)), 2)

            Dim total As Integer

            For i As Integer = 0 To 14
                total = total + cardnumbers(i)
            Next i

            Dim dcv As Integer = System.Convert.ToInt32(CardNumber.Substring(15, 1))
            If ((dcv + total) Mod 10 = 0) Then
                Return -1
            End If


        Catch ex As Exception
            Return 1
        End Try

        Return 1




    End Function



    Private Sub MessageFNBAcctNumber()
        Trace.WriteLine("FNB Account number formatting")
        If FOrigAcctNumber.Length = 10 Then
            FAcctNumber = "0" + FOrigAcctNumber
            FnbOld = True
            Trace.WriteLine("Old FNB Number = True , formatted number = " + FAcctNumber.ToString())
            Exit Sub
        ElseIf FOrigAcctNumber.Length < 10 Then
            Trace.WriteLine("Old FNB Number = True , Unformatted account number is less than 10")
            FnbOld = True
        Else
            Trace.WriteLine("Old FNB Number = false")
            FnbOld = False
        End If
    End Sub

    Private Function BranchCodeFormatIsGood() As Boolean
        Try
            Trace.WriteLine("Branch Code format check")
            Dim lBranchCode As Integer = Val(FBranchCode)
            If lBranchCode = 0 Then
                Trace.WriteLine("Branch Code check Fails")
                Return False
            End If
            Trace.WriteLine("Branch Code is in good format")
            Return True
        Catch ex As Exception
            Trace.WriteLine("Branch code Does not convert to ordinal value")
            Return False
        End Try
    End Function

    Private Function ZeroFrontPadAccountNumber(ByVal AccountNumber As String, ByVal RequiredLength As Integer) As String
        Dim lStringBuilder As New StringBuilder(AccountNumber)
        Dim lRequiredPadLength As Integer = RequiredLength - lStringBuilder.Length
        If (lRequiredPadLength > 0) Then
            lStringBuilder.Insert(0, "0", lRequiredPadLength)
        End If
        Return lStringBuilder.ToString
    End Function

    Private Function TotalProducts(ByVal Weighting As String) As Integer
        Dim Result As Integer = 0
        For li As Integer = 0 To FAcctNumber.Length - 1
            Result = (Val(FAcctNumber.Chars(li)) * Val(Weighting.Chars(li))) + Result
        Next
        Trace.WriteLine("Totol Products: Each Account digit multiplied by corresponding weight sum = " + Result.ToString())
        Return Result
    End Function

    Private Function FastCalculate(ByVal Fudge As Integer, ByVal ModValue As Integer, ByVal Weighting As String) As Integer

        Trace.WriteLine("Fast Calculate: Fudge = " + Fudge.ToString() + " Weighting  = " + Weighting + " Mod = " + ModValue.ToString())
        Dim lProductTotal As Integer = TotalProducts(Weighting)
        Dim retval As Integer = (lProductTotal + Fudge) Mod ModValue
        Trace.WriteLine("Fast Calculate Answer = " + retval.ToString())
        Return retval
    End Function

    Private Function ExceptionCode_A() As Integer
        Trace.WriteLine("** Enter Exception Code A")
        Dim result As Integer = gcInternalError
        If FOrigAcctNumber.Length = 10 Then
            FAcctNumber = FBranchCode & FOrigAcctNumber
            Dim Awnser As Integer = FastCalculate(0, 10, "1212121212121212")
            If (FAcctNumber.Chars(6) = "1") And ((Awnser = 1) Or (Awnser = 0)) Then
                Trace.WriteLine("** Exception Code A PASSED")
                Return gcSuccess
            End If
            If Awnser = 0 Then
                Return (gcSuccess)
            End If
        End If
        Return result
    End Function

    Private Function ExceptionCode_B(ByVal Remainder As Integer) As Integer
        Trace.WriteLine("** Enter Exception Code B")
        If (AcctDigit(10) = 1 Or AcctDigit(10) = 0) And Remainder = 1 Then
            Trace.WriteLine("** Exception Code B PASSED")
            Return gcSuccess
        End If
    End Function

    Private Function ExceptionCode_C() As Integer
        Trace.WriteLine("** Enter Exception Code C")
        Dim result As Integer = gcInternalError
        If FOrigAcctNumber.Length = 10 Then
            Return result
        End If

        If FOrigAcctNumber.Length = 11 Then
            If FastCalculate(0, 10, "12121212121") = 0 Then
                Trace.WriteLine("** Exception Code C Passed")
                Return gcSuccess
            End If
        End If

        Return result

    End Function

    Private Function ExceptionCode_D() As Integer
        Trace.WriteLine("** Enter Exception Code D")
        Dim result As Integer = gcInternalError

        If FOrigAcctNumber.Length = 11 Then
            If FAcctType = 1 Then ' for current accounts
                If (AcctDigit(9) = 1) And (AcctDigit(10) = 1) Then
                    Trace.WriteLine("** Enter Exception Code D PASSED")
                    Return gcSuccess
                End If
            End If

            If FAcctType = 1 Then ' for savings accounts
                If (AcctDigit(9) = 1) And (AcctDigit(10) = 3) Then
                    Trace.WriteLine("** Exception Code D PASSED")
                    Return gcSuccess
                End If
            End If

        Else
            Return gcSuccess ' OLD ACCOUNT NUMBER NO VALIDATION REQUIRED
        End If


        Return result
    End Function

    Private Function ExceptionCode_E() As Integer
        Trace.WriteLine("** Enter Exception Code E")
        Dim result As Integer = gcInternalError
        If AcctDigit(0) > 0 And AcctDigit(1) > 0 Then
            If AcctDigit(10) = 0 And AcctDigit(9) > 0 Then
                Trace.WriteLine("** Exception Code E PASSED")
                Return gcSuccess
            End If
        End If
        Return result
    End Function

    Private Function ExceptionCode_G() As Integer
        Trace.WriteLine("** Enter Exception Code G")
        Dim result As Integer = gcInternalError
        If FOrigAcctNumber.Length < 8 Then
            Return gcAccountNumberError
        End If
        FAcctNumber = "111" + FOrigAcctNumber.Substring(0, 7)

        Dim answer As Integer = (29 * AcctDigit(3) + 23 * AcctDigit(4) + 19 * AcctDigit(5) _
                    + 17 * AcctDigit(6) + 13 * AcctDigit(7) + 7 * AcctDigit(8) + 3 * AcctDigit(9) _
                    + AcctDigit(10)) Mod 11

        If answer = 0 Then
            Trace.WriteLine("** Exception Code G PASSED")
            result = gcSuccess
        Else
            If AcctDigit(9) = AcctDigit(10) Then
                answer = (29 * AcctDigit(3) + 23 * AcctDigit(4) + 19 * AcctDigit(5) _
                + 17 * AcctDigit(6) + 13 * AcctDigit(7) + 7 * AcctDigit(8) + 3 * AcctDigit(9) _
                + 10) Mod 11
                If answer = 0 Then
                    Trace.WriteLine("** Exception Code G PASSED")
                    result = gcSuccess
                End If
            End If
        End If

        Return result

    End Function

    Private Function ExceptionCode_F() As Integer
        Trace.WriteLine("** Enter Exception Code F")
        Dim i, answer As Integer
        Dim s As String
        Dim LastDigitOfAbsaAccount As Integer
        Dim LastDigitIsZeroOrOne As Boolean
        Dim FOriganalAccountNunber As String
        Dim Result As Integer

        Result = gcInternalError

        If FAcctType = 2 Then
            If FAcctNumber.Length = 11 Then
                If FAcctNumber.Substring(0, 2) = "53" Then
                    Trace.WriteLine("** Exception Code F Passed on, Length = 11 account type = 2and account number starts with '53'")
                    Return gcSuccess

                End If
            End If
        End If





        LastDigitOfAbsaAccount = Val(FAcctNumber.Chars(FAcctNumber.Length - 1))
        LastDigitIsZeroOrOne = (LastDigitOfAbsaAccount = 0) Or (LastDigitOfAbsaAccount = 1)

        answer = FastCalculate(0, 10, "17329874321")

        If (answer = 0) Then
            Trace.WriteLine("** Exception Code F Passed")
            Return gcSuccess
        Else
            answer = FastCalculate(0, 11, "14327654321")
            If (answer = 0) Then
                Trace.WriteLine("** Exception Code F Passed")
                Return gcSuccess
            Else
                answer = FastCalculate(0, 11, "54327654321")
                If (answer = 0) Or ((answer = 1) And (LastDigitIsZeroOrOne)) Then
                    Trace.WriteLine("** Exception Code F Passed 0 or 1 result to last digit 0 or 1")
                    Return gcSuccess
                Else
                    answer = FastCalculate(0, 10, "14329874321")
                    If answer = 0 Then
                        Trace.WriteLine("** Exception Code F Passed")
                        Return gcSuccess
                    Else
                        answer = FastCalculate(0, 11, "11327654321")
                        If answer = 0 Then
                            Trace.WriteLine("** Exception Code F Passed")
                            Return gcSuccess
                        Else
                            If FOrigAcctNumber.Length < 10 Then

                                FOriganalAccountNunber = FAcctNumber
                                i = Val(FAcctNumber.Chars(10)) + 6
                                If (i >= 10) Then
                                    i = i - 10
                                End If
                                FAcctNumber = FAcctNumber.Substring(0, 10) & Str(i).TrimStart
                                answer = FastCalculate(0, 11, "11327654321")
                                If (answer = 0) Or ((answer = 1) And (LastDigitIsZeroOrOne)) Then
                                    Trace.WriteLine("** Exception Code F Passed Acct Len less than 10")
                                    Return gcSuccess
                                Else
                                    FAcctNumber = FOriganalAccountNunber
                                    answer = FastCalculate(0, 10, "14329874321")
                                    If (answer = 0) Or ((answer = 1) And (LastDigitIsZeroOrOne)) Then
                                        Trace.WriteLine("** Exception Code F Passed Acct Len less than 10")
                                        Return gcSuccess
                                    End If
                                End If
                            End If
                        End If
                    End If
                End If
            End If
        End If

        Return Result

    End Function

    Private Function ExceptionCode_i() As Integer
        Trace.WriteLine("** Enter Exception Code I")
        If (FOrigAcctNumber.Length = 13) Then
            If (FOrigAcctNumber.Substring(2, 1) = "2" Or FOrigAcctNumber.Substring(2, 1) = "4") Then
                Trace.WriteLine("** Exception Code I Passed, third digit = 2 or 4")
                Return gcSuccess
            End If
        Else
            If (FOrigAcctNumber.Substring(0, 1) = "2" Or FOrigAcctNumber.Substring(0, 1) = "4") Then
                Trace.WriteLine("** Enter Exception Code I Passed, first digit = 2 or 4")
                Return gcSuccess
            End If
        End If

        Return gcInternalError
    End Function
    'An additional check is only performed if the account number fails the CDV for habib bank
    Private Function ExceptionCode_J() As Integer
        Trace.WriteLine("** Enter Exception Code J")
        If (FOrigAcctNumber.Length = 11) Then
            If (FOrigAcctNumber.StartsWith("0")) Then
                Trace.WriteLine("Exception code J: This is a old current or savings account")
                Return gcInternalError
            Else
                Trace.WriteLine("Exception code J: this is a new current or savings account")
                Return gcSuccess
            End If
        Else
            Trace.WriteLine("Exception code J: The account number length is not = to 11")
            Return gcInternalError
        End If

    End Function

    Private Function ExceptionCode_m() As Integer
        Trace.WriteLine("** Enter Exception Code M")
        If (Long.Parse(FOrigAcctNumber) > 10000000000 And Long.Parse(FOrigAcctNumber) < 19999999999) Then
            Dim answer As Integer = (13 * AcctDigit(0) + 12 * AcctDigit(1) + 9 * AcctDigit(2) _
                                + 8 * AcctDigit(3) + 7 * AcctDigit(4) + 6 * AcctDigit(5) + 5 * AcctDigit(6) _
                                + 4 * AcctDigit(7) + 3 * AcctDigit(8) + 2 * AcctDigit(9) + AcctDigit(10)) Mod 11
            If (answer = 0) Then
                Return gcSuccess
            Else
                Return gcInternalError
            End If
        Else
            Return gcInternalError
        End If

    End Function

    'Tested and correct
    Protected Function CDV_StandardBankSA() As Integer
        Trace.WriteLine("STANDARD BANK SA VALIDATION:")

        Dim OrigLength As Integer = FOrigAcctNumber.Length

        If (OrigLength > 9 And FBranchCode <> "051001") Then
            Trace.WriteLine("Unformatted Account number length is greater than 9")
            If Not ((FOrigAcctNumber.Chars(0) = "0") And (FOrigAcctNumber.Chars(1) = "0")) Then
                Trace.WriteLine("FAIL ACCOUNT NUMBER ERROR: Reason 1st and second digits of unformatted account number is 0")
                Return gcAccountNumberError
            End If
        End If




        If (FAcctType = 1) Or (FAcctType = 2) Or (FAcctType = 3) Then
            Trace.WriteLine("Validation for account type 1, 2 or 3")
            If FastCalculate(0, 11, "11987654321") = 0 Then 'Now the old number will still pass here ....
                Trace.WriteLine("@Result = Success")
                Return gcSuccess
            Else
                'If the above fails then check if the branch code falls into a range where
                'a secondry exception rules applies. We only apply the exception in this case
                'U see this? yea 
                If (FBranchCode = "051001") Then
                    Trace.WriteLine("Applying Standard bank new exception code")
                    If (StandardBankSA_ExceptionM() = gcSuccess) Then
                        Return gcSuccess
                    Else
                        Trace.WriteLine("@Result = Account Number Error")
                        Return gcAccountNumberError
                    End If
                Else
                    Trace.WriteLine("@Result = Account Number Error")
                    Return gcAccountNumberError
                End If
            End If
        Else
            Trace.WriteLine("@Result = Account Type Error")
            Return gcAccountTypeError
        End If


    End Function


    Protected Function CDV_StandardBankLesotho() As Integer
        Trace.WriteLine("STANDARD BANK LESOTHO VALIDATION:")
        If (FAcctType = 1) Or (FAcctType = 2) Then
            Trace.WriteLine("Validation for account type 1 and 2")
            If (FOrigAcctNumber.Length = 13) Then
                Trace.WriteLine("Unformatted account number length is 13")
                If (ExceptionCode_i() = gcInternalError) Then
                    Trace.WriteLine("@Result = Account Number Error")
                    Return gcAccountNumberError
                End If
            Else
                If (ExceptionCode_i() = gcInternalError) Then
                    Trace.WriteLine("@Result = Account number error")
                    Return gcAccountNumberError
                End If
            End If
            Trace.WriteLine("Exception code I Passes")
            Trace.WriteLine("@Result = Success")
            Return gcSuccess
        Else
            Trace.WriteLine("@Result = Account number error")
            Return gcAccountTypeError
        End If

    End Function

    Protected Function StandardBankSA_ExceptionM() As Integer
        Trace.WriteLine("NEW STANDARD BANK SA VALIDATION:")
        If (FAcctType = 1) Or (FAcctType = 2) Or (FAcctType = 3) Then
            Trace.WriteLine("Validation for account type 1, type 2 and type 3")
            If (FOrigAcctNumber.Length = 11) Then
                Trace.WriteLine("Unformatted account number length is 11")
                If (ExceptionCode_m() = gcInternalError) Then
                    Trace.WriteLine("Exception code M Failed")
                    Trace.WriteLine("@Result = Account Number Error")
                    Return gcAccountNumberError
                Else
                    If (ExceptionCode_m() = gcSuccess) Then
                        Trace.WriteLine("Exception code M Passes")
                        Trace.WriteLine("@Result = Success")
                        Return gcSuccess
                    End If
                End If
            Else
                Trace.WriteLine("@Result = Account number error")
                Return gcAccountNumberError
            End If
        Else
            Trace.WriteLine("@Result = Account type error")
            Return gcAccountTypeError
        End If

    End Function

    'Tested result exactley the same as Cdv 5.0.0.8 but need to talk to goolam
    'about account type errors and account number errors
    'I think I have fixed it by making lvalidaccountnumber equal to option1 or option2
    'Alternatly just set lValidAccountNumber to false to give same report as 5.0.0.8
    Protected Function CDV_NedBank() As Integer

        Trace.WriteLine("NEDBANK VALIDATION: ")

        If FAcctType > 2 Or FAcctType < 1 Then
            Trace.WriteLine("@Result = Account type error: Reason ---- Account type was greater than 2 or less than 1")
            Return gcAccountTypeError
        End If

        Trace.WriteLine("Select validation path: ")
        Dim Option1 As Boolean = FastCalculate(9, 11, "11987654321") = 0

        Dim Option2 As Boolean = FastCalculate(18, 11, "11987654321") = 0


        'Talk to goolam
        Dim lValidAccountNumber As Boolean = False 'Option1 Or Option2

        If FAcctType = 1 Then
            Trace.WriteLine("Account type 1 validation ... ")
            If Option1 Then
                Trace.WriteLine("Option 1 check (9,11,11987654321)")
                If FAcctNumber.Chars(1) = "1" Then
                    Trace.WriteLine("@Result = Success, Reason = second digit 1 and fast calc")
                    Return gcSuccess
                Else
                    If lValidAccountNumber Then
                        Trace.WriteLine("@Result = FAIL ACCOUNT TYPE ERROR Reason: Although ...Success under Option2, fails char 1 check, report account tyope")
                        Return gcAccountTypeError
                    Else
                        Trace.WriteLine("@Result = FAIL, Invalid account number")
                        Return gcAccountNumberError
                    End If
                End If
            Else
                Trace.WriteLine("Option 2 check (18,11,11987654321)")
                If lValidAccountNumber Then

                    Trace.WriteLine("@Result = FAIL, Invalid account type")
                    Return gcAccountTypeError
                Else
                    Trace.WriteLine("@Result = FAIL, Invalid account number")
                    Return gcAccountNumberError
                End If
            End If
        End If

        If FAcctType = 2 Then
            Trace.WriteLine("Account type 2 check")
            If Option2 And FAcctNumber.Chars(1) = "2" Then
                Trace.WriteLine("@Result = Success as 2nd digit = 2")
                Return gcSuccess
            Else
                If lValidAccountNumber Then
                    Trace.WriteLine("@Result = FAIL Account type error")
                    Return gcAccountTypeError
                Else
                    Trace.WriteLine("@Result = FAIL Account number incorrect")
                    Return gcAccountNumberError
                End If
            End If
        End If



    End Function

    Protected Function CDV_CapeOfGoodHope() As Integer

        Trace.WriteLine("CAPE OF GOODHOPE VALIDATION:")
        If FAcctType = 2 Or FAcctType = 3 Or FAcctType = 4 Then
            Trace.WriteLine("Account type is 2 , 3 or 4")
            Dim Answer As Integer = (AcctDigit(0) + AcctDigit(1) + AcctDigit(2) + AcctDigit(3) + 19 _
                * AcctDigit(4) + 17 * AcctDigit(5) + 13 * AcctDigit(6) + 7 * AcctDigit(7) + 5 _
                * AcctDigit(8) + 3 * AcctDigit(9) + AcctDigit(10)) Mod 11
            Trace.WriteLine("Calculation = (AcctDigit(0) + AcctDigit(1) + AcctDigit(2) + AcctDigit(3) + 19 * AcctDigit(4) + 17 * AcctDigit(5) + 13 * AcctDigit(6) + 7 * AcctDigit(7) + 5 * AcctDigit(8) + 3 * AcctDigit(9) + AcctDigit(10)) Mod 11")
            If Answer = 0 Then
                Trace.WriteLine("@Result = Success")
                Return gcSuccess
            Else
                If ExceptionCode_B(Answer) = gcSuccess Then
                    Trace.WriteLine("@Result = Success")
                    Return gcSuccess
                Else
                    Trace.WriteLine("@Result = FAILED ACCOUNT NUMBER")
                    Return gcAccountNumberError
                End If

            End If

        Else
            Trace.WriteLine("@Result = FAILED, Account type must be 2 3 or 4")
            Return gcAccountTypeError
        End If

    End Function

    Protected Function CDV_NedBankBond() As Integer
        Trace.WriteLine("NEDBANK BOND ACCOUNT VALIDATION")
        Dim Answer As Integer = gcInternalError

        If FAcctType = 1 Then
            Trace.WriteLine("Account type 1 validation")
            If (FOrigAcctNumber.Length = 13) And (FOrigAcctNumber.Chars(12) = "8") Then
                Trace.WriteLine("Apply exception code G, as account number is 13 and the last digit is 8")
                Answer = ExceptionCode_G()
                If Answer = gcInternalError Then
                    Trace.WriteLine("@Result = FAILED. Reason exception code G, Incorrect account number")
                    Return gcAccountNumberError
                Else
                    Trace.WriteLine("@Result = Success")
                    Return gcSuccess
                End If
            Else
                Trace.WriteLine("@Result = FAILED, ACCOUNT NUMBER ERROR, Must qualify length 13 and last digit 8")
                Return gcAccountNumberError
            End If
        Else
            Trace.WriteLine("@Result = FAILED ACCOUNT TYPE ERROR ... only account type 1 supported")
            Return gcAccountTypeError
        End If
    End Function



    Protected Function CDV_NEW_FNB() As Integer

        Trace.WriteLine("NEW FNB VALIDATION")

        Dim retVal As Integer = gcInternalError

        ' Branch Specific verification
        If FAcctType = 3 And FBranchCode = "250006" Then
            Trace.WriteLine("Account type = 3 and branch code is 250006")
            If FastCalculate(0, 11, "12121212121") = 0 Then
                Trace.WriteLine("@Result = Success")
                retVal = gcSuccess
            Else
                Trace.WriteLine("@Result = FAILED, Account number incorrect")
                retVal = gcAccountNumberError
            End If
        End If

        If FAcctType = 1 Or FAcctType = 2 Then
            Trace.WriteLine("Validation for account type 1 and 2")
            If FastCalculate(0, 11, "12121212121") = 0 Then
                Trace.WriteLine("@Result = Success")
                retVal = gcSuccess
            Else
                Trace.WriteLine("@Result = Failing pending account type 1 personal loan check")
                retVal = gcAccountNumberError
            End If
        Else
            Trace.WriteLine("Account type error")
            retVal = gcAccountTypeError
        End If

        'This may be a bond or personal loan account
        If FAcctType = 1 And retVal <> gcSuccess Then
            Trace.WriteLine("Account type 1 pesonal loan validation check")
            Dim Awnser As Integer = (13 * AcctDigit(0) + 10 * AcctDigit(1) + _
            9 * AcctDigit(2) + 8 * AcctDigit(3) + 7 * AcctDigit(4) _
            + 6 * AcctDigit(5) + 5 * AcctDigit(6) + 4 * AcctDigit(7) _
            + 3 * AcctDigit(8) + 2 * AcctDigit(9) + AcctDigit(10)) Mod 11

            If Awnser = 0 Then
                Trace.WriteLine("@Result = Success -- Personal loan validation")
                retVal = gcSuccess
            Else
                Trace.WriteLine("@Result = FAILED - Incorrect account number")
                retVal = gcAccountNumberError
            End If

        End If

        Return retVal

    End Function

    'Tested and correct
    Protected Function CDV_FNB() As Integer

        Trace.WriteLine("FNB CDV VALIDATION:")


        Dim lResult As Integer = gcInternalError

        MessageFNBAcctNumber()

        If FAcctType > 2 Then
            Trace.WriteLine("Account type must be greater than 2 FAIL")
            Return gcAccountTypeError
        End If

        If FAcctType = 2 Then
            Trace.WriteLine("Validation for account type 2")
            Dim Awnser As Integer = FastCalculate(10, 11, "11987654321")
            If Awnser = 0 Then
                Trace.WriteLine("@Result = Success")
                Return gcSuccess
            Else
                Trace.WriteLine("@Result = Does not pass BUT if not old number ...")
                If Not FnbOld Then

                    If FOrigAcctNumber.Length = 11 Then
                        Trace.WriteLine("Length 11 excecute exception code C")
                        lResult = ExceptionCode_C()
                        If lResult = gcInternalError Then
                            Trace.WriteLine("@Result = FAILED, for exception Code C")
                            Return gcAccountNumberError
                        Else
                            Trace.WriteLine("@Result = Success")
                            Return gcSuccess
                        End If
                    End If 'If FOrigAcctNumber.Length = 11 Then

                    If FOrigAcctNumber.Length < 10 Then
                        Trace.WriteLine("Unformatted account number length is less than 10, apply exception A")
                        lResult = ExceptionCode_A()
                        If lResult = gcInternalError Then
                            Trace.WriteLine("@Result = FAIL, account number error")
                            Return gcAccountNumberError
                        Else
                            Trace.WriteLine("@Result = Success")
                            Return gcSuccess
                        End If

                    End If 'If FOrigAcctNumber.Length < 10 Then
                Else
                    Trace.WriteLine("@Result = FAILED, Account number error, this is not an old account ...")
                    Return gcAccountNumberError
                End If
            End If
        End If

        Trace.WriteLine("Attempt account type 1 VALIDATION")
        'Speak with goolam as this situation is both impossible with the new and the old CDV
        If (FAcctType = 1) And (FOrigAcctNumber.Length = 11) And (FnbOld) Then
            Trace.WriteLine("Account type is 1, then unformatted account length is 11, and this is an old account ???")

            Dim Awnser As Integer = (13 * AcctDigit(0) + 10 * AcctDigit(1) + _
            9 * AcctDigit(2) + 8 * AcctDigit(3) + 7 * AcctDigit(4) _
            + 6 * AcctDigit(5) + 5 * AcctDigit(6) + 4 * AcctDigit(7) _
            + 3 * AcctDigit(8) + 2 * AcctDigit(9) + AcctDigit(10)) Mod 11

            Trace.WriteLine("Calucution = 13 * AcctDigit(0) + 10 * AcctDigit(1) + 9 * AcctDigit(2) + 8 * AcctDigit(3) + 7 * AcctDigit(4) + 6 * AcctDigit(5) + 5 * AcctDigit(6) + 4 * AcctDigit(7) + 3 * AcctDigit(8) + 2 * AcctDigit(9) + AcctDigit(10)) Mod 11 ")

            If Awnser = 0 Then
                Trace.WriteLine("Result = success")
                Return gcSuccess
            Else
                Trace.WriteLine("Account number error")
                Return gcAccountNumberError
            End If

        End If

        If (FAcctType = 1) And (FAcctNumber.Length < 12) Then
            Trace.WriteLine("Account type is 1 and Length account number is less than 12")
            Dim awnser As Integer = FastCalculate(0, 11, "10117654321")
            If Awnser = 0 Then
                Trace.WriteLine("Passed Innitial check ... 0,11,10117654321")

                If FOrigAcctNumber.Length = 11 Then
                    Trace.WriteLine("If unformatted length is 11 then ...")
                    Awnser = ExceptionCode_C()
                    If Awnser = gcInternalError Then
                        Trace.WriteLine("@Result = FAILED, Account number error")
                        Return gcAccountNumberError
                    Else
                        Trace.WriteLine("@Result = Success, Passed Exception code C")
                        Return gcSuccess
                    End If
                End If

                If FOrigAcctNumber.Length < 10 Then
                    Trace.WriteLine("Unformatted Length is less than 10 so ...")
                    Awnser = ExceptionCode_A()
                    If Awnser = gcInternalError Then
                        Trace.WriteLine("@Result = FAILED, failed on exception code A Check")
                        Return gcAccountNumberError
                    Else
                        Trace.WriteLine("@Result = Success")
                        Return gcSuccess
                    End If
                End If

                If Awnser = 0 Then
                    Trace.WriteLine("@Result = Success")
                    Return gcSuccess
                End If

            Else
                Trace.WriteLine("@Result = FAILED, account number error")
                Return gcAccountNumberError
            End If
        End If




    End Function

    Protected Function CDV_ABSA() As Integer
        'Dim OrigLength As Integer = FOrigAcctNumber.Length

        'If ((OrigLength > 11) Or (OrigLength < 8)) Then
        'Return gcAccountNumberError
        ' End If

        Trace.WriteLine("ABSA CDV VALIDATION")
        If (FAcctType = 1) Or (FAcctType = 1) Or (FAcctType = 2) Or (FAcctType = 3) Or (FAcctType = 4) Or (FAcctType = 6) Then
            Trace.WriteLine("Validate for account number 1,2,3,4,6")
            If FastCalculate(0, 11, "14327654321") = 0 Then
                Trace.WriteLine("@Result = Success")
                Return gcSuccess
            Else
                Trace.WriteLine("Fails calculation ... 0,11,14327654321")
                If ExceptionCode_F() = gcSuccess Then
                    Trace.WriteLine("@Result = Success")
                    Return gcSuccess
                Else
                    Trace.WriteLine("@Result = FAILED, account number error")
                    Return gcAccountNumberError
                End If
            End If
        Else
            Trace.WriteLine("Validation fails invalid account number")
            Return gcAccountTypeError
        End If
    End Function


    'Not yet tested
    Protected Function CDV_CitiBank() As Integer
        Trace.WriteLine("CITI BANK VALIDATION")
        If FAcctType = 1 Then
            Trace.WriteLine("Account type 1 pass by default --- NO CHECK")
            Return gcSuccess
        Else
            Trace.WriteLine("Not Account type 1 fail by default --- NO CHECK")
            Return gcAccountTypeError
        End If

    End Function

    'Not yet tested
    Protected Function CDV_NedbankLesotho() As Integer
        Trace.WriteLine("NEDBANK LESOTHO VALIDATION")
        If FAcctType = 1 Or FAcctType = 2 Then
            Trace.WriteLine("Success by virtue of account type = 1 or 2 -- no further check")
            Return gcSuccess
        Else
            Trace.WriteLine("Failed by virtue of account type not 1 or 2 -- no further check")
            Return gcAccountTypeError
        End If
    End Function

    'Not yet tested
    Protected Function CDV_PEP() As Integer
        Trace.WriteLine("PEP CDV VALIDATION")

        If FAcctType = 1 Or FAcctType = 2 Or FAcctType = 3 Or FAcctType = 4 Then
            Trace.WriteLine("Account type is 1,2,3,4")
            Dim lAwnser As Integer = FastCalculate(2, 11, "18765432100")
            If lAwnser <> 0 Then
                If ExceptionCode_E() = gcInternalError Then
                    Trace.WriteLine("@Result Failed: Incorrect account number fails Exception C as well")
                    Return gcAccountNumberError
                Else
                    Trace.WriteLine("@Result = success based on ExceptionCode C")
                    Return gcSuccess
                End If
            Else
                Trace.WriteLine("@Result = success based on fast caluction")
                Return gcSuccess
            End If
        Else
            Trace.WriteLine("Account type error only 1,2,3,4 accepted")
            Return gcAccountTypeError
        End If

    End Function

    'Not yet tested
    Protected Function CDV_BankOfAthens() As Integer

        Trace.WriteLine("BANK OF ATHENS CDV")

        ' Dim LOriganalLength As Integer = FOrigAcctNumber.Length

        ' If ((LOriganalLength <> 7) Or (LOriganalLength <> 11)) Then
        ' Return gcAccountNumberError
        ' End If

        If FAcctType = 1 Or FAcctType = 2 Then
            Trace.WriteLine("Account type is 1 or 2")
            If FastCalculate(0, 11, "11987654321") = 0 Then
                Trace.WriteLine("CDV passed")
                Return gcSuccess
            Else
                Trace.WriteLine("CDV Fails")
                Return gcAccountNumberError
            End If
        Else
            Trace.WriteLine("Invalid account type, only handles account type 1 and 2")
            Return gcAccountTypeError
        End If

    End Function

    Protected Function CDV_AfricanBankLimited() As Integer

        Trace.WriteLine("AFRICAN BANK LMTD CDV")
        If FAcctType = 2 Then
            If FastCalculate(0, 11, "12121212100") = 0 Then
                Trace.WriteLine("@Result = Success")
                Return gcSuccess
            Else
                Trace.WriteLine("Account number error = FAILED")
                Return gcAccountNumberError
            End If
        Else
            Trace.WriteLine("@Result = Failed, Account type error, only accoun type 2 supported")

            Return gcAccountTypeError
        End If

    End Function

    'Not yet tested
    Protected Function CDV_MTN() As Integer
        Trace.WriteLine("MTN CDV VALIDATION")
        If FAcctType = 3 Then
            If FastCalculate(0, 11, "13971379131") = 0 Then
                Return gcSuccess
                Trace.WriteLine("@Result = success based on fast caluction")
            Else
                Trace.WriteLine("@Result = Failed")
                Return gcAccountNumberError
            End If
        Else
            Trace.WriteLine("@Result = Failed, only account type 3 accepted")
            Return gcAccountTypeError
        End If

    End Function

    Protected Function CDV_TebaBank() As Integer
        Trace.WriteLine("TEBA BANK CDV VALIDATION")
        If FAcctType = 1 Or FAcctType = 2 Or FAcctType = 3 Then
            If FastCalculate(0, 11, "19876543211") = 0 Then
                Trace.WriteLine("@Result = success based on fast caluction")
                Return gcSuccess
            Else
                Trace.WriteLine("@Result = Failed")
                Return gcAccountNumberError
            End If
        Else
            Trace.WriteLine("@Result = Only account type 1,2, 3 are accepted")
            Return gcAccountTypeError
        End If
    End Function

    Protected Function CDV_TebaBankExtension() As Integer
        Trace.WriteLine("TEBA BANK EXTENTION VALIDTION")
        If FAcctType = 1 Or FAcctType = 2 Or FAcctType = 3 Then
            If FastCalculate(0, 11, "27654321000") = 0 Then
                Trace.WriteLine("@Result = success based on fast caluction")
                Return gcSuccess
            Else
                Trace.WriteLine("@Result = Failed based on fast caluction")
                Return gcAccountNumberError
            End If
        Else
            Trace.WriteLine("@Result = Failed, only account type 1,2,3")
            Return gcAccountTypeError
        End If
    End Function

    Protected Function CDV_Mercantile() As Integer
        Trace.WriteLine("MERCANTILE CDV VALIDATION")
        If FAcctType = 1 Or FAcctType = 2 Then
            Dim Awnser As Integer = _
            (AcctDigit(0) + 10 * AcctDigit(1) + 9 * AcctDigit(2) _
                + 8 * AcctDigit(3) + 7 * AcctDigit(4) _
                + 6 * AcctDigit(5) + 5 * AcctDigit(6) _
                + 4 * AcctDigit(7) + 3 * AcctDigit(8) _
                + 2 * AcctDigit(9) + AcctDigit(10)) Mod 11
            Trace.WriteLine("Calculation =  (AcctDigit(0) + 10 * AcctDigit(1) + 9 * AcctDigit(2) + 8 * AcctDigit(3) + 7 * AcctDigit(4) + 6 * AcctDigit(5) + 5 * AcctDigit(6) + 4 * AcctDigit(7) + 3 * AcctDigit(8) + 2 * AcctDigit(9) + AcctDigit(10)) Mod 11")
            If Awnser = 0 Then
                Trace.WriteLine("@Result = Success based on calculation")
                Return gcSuccess
            Else
                Trace.WriteLine("@Result = failed based on calculation, attempting Code B")
                If ExceptionCode_B(Awnser) = gcSuccess Then
                    Trace.WriteLine("@Result = Success based on calculation")
                    Return gcSuccess
                Else
                    Trace.WriteLine("@Result = Failed both calculation ... FAILED")
                    Return gcAccountNumberError
                End If
            End If
        Else
            Trace.WriteLine("@Result = Failed only account types 1 and 2")
            Return gcAccountTypeError
        End If
    End Function

    Protected Function CDV_BOE() As Integer
        Trace.WriteLine("BOE CDV VALIDATION")
        If FAcctType = 1 Or FAcctType = 2 Or FAcctType = 3 Or FAcctType = 4 Then
            If FastCalculate(0, 11, "18765432100") = 0 Then
                Trace.WriteLine("@Result = Success based on calculation")
                Return gcSuccess
            Else
                Trace.WriteLine("@Result = failed try exception E")
                If ExceptionCode_E() = gcSuccess Then
                    Trace.WriteLine("@Result = Success based on calculation")
                    Return gcSuccess
                Else
                    Trace.WriteLine("@Result = Failed both caculations FAILED")
                    Return gcAccountNumberError
                End If
            End If
        Else
            Trace.WriteLine("@Result = Failed only accept account types 1,2,3,4 ")
            Return gcAccountTypeError
        End If
    End Function

    Protected Function CDV_PostOffice() As Integer
        Trace.WriteLine("POST OFFICE CDV VALIDATION")
        If FAcctType = 2 Then
            If FastCalculate(0, 10, "42184218421") = 0 Then
                Trace.WriteLine("@Result = Success based on calculation")
                Return gcSuccess
            Else
                Trace.WriteLine("@Result = failed based on calculation")
                Return gcAccountNumberError
            End If
        Else
            Trace.WriteLine("@Result = Failed only accept account type 2")
            Return gcAccountTypeError
        End If
    End Function

    Protected Function CDV_NedBankNamibia() As Integer
        Trace.WriteLine("NEDBANK NAMIBIA CDV")

        'Structure check .....
        'If (FAcctType = 1) Then
        'Trace.WriteLine("NEDBANK NAMIBIA CDV -- Checking last 2 digits = 11")
        'If (FOrigAcctNumber.Substring(9, 1) = "1" And FOrigAcctNumber.Substring(10, 1) = "1") Then
        'Trace.WriteLine("NEDBANK NAMIBIA CDV -- Structure test passed")
        'Else
        '    Trace.WriteLine("Account number structure incorrect, please get new account number")
        '    Return gcAccountNumberError
        'End If
        'End If


        'If (FAcctType = 2) Then
        'Trace.WriteLine("NEDBANK NAMIBIA CDV -- Checking last 2 digits = 21")
        'If (FOrigAcctNumber.Substring(9, 1) = "2" And FOrigAcctNumber.Substring(10, 1) = "1") Then
        'Trace.WriteLine("NEDBANK NAMIBIA CDV -- Structure test passed")
        'Else
        '    Trace.WriteLine("Account number structure incorrect, please get new account number")
        '    Return gcAccountNumberError
        'End If
        'End If

        'If (FAcctType = 4) Then
        'Trace.WriteLine("NEDBANK NAMIBIA CDV -- Checking last 2 digits = 21")
        'If (FOrigAcctNumber.Substring(9, 1) = "3" And FOrigAcctNumber.Substring(10, 1) = "1") Then
        'Trace.WriteLine("NEDBANK NAMIBIA CDV -- Structure test passed")
        'Else
        '    Trace.WriteLine("Account number structure incorrect, please get new account number")
        '   Return gcAccountNumberError
        'End If

        'End If


        Trace.WriteLine("NEDBANK NAMIBIA CDV")
        If FAcctType = 1 Or FAcctType = 2 Or FAcctType = 4 Then
            If FastCalculate(0, 11, "54327654321") = 0 Then
                Trace.WriteLine("@Result = Success based on calculation")
                Return gcSuccess
            Else
                Trace.WriteLine("@Result = Failed based on calculation")
                Return gcAccountNumberError
            End If
        Else
            Trace.WriteLine("@Result = Failed only account types 1 and 2 accepted")
            Return gcAccountTypeError
        End If
    End Function

    Protected Function CDV_Rennies() As Integer
        Trace.WriteLine("RENNIES CDV")
        If FAcctType = 1 Or FAcctType = 2 Or FAcctType = 3 Then
            If FastCalculate(0, 11, "27654321000") = 0 Then
                Trace.WriteLine("@Result = Success based on calculation")
                Return gcSuccess
            Else
                Trace.WriteLine("@Result = Failed based on calculation")
                Return gcAccountNumberError
            End If
        Else
            Trace.WriteLine("@Result = Failed, only account type 1,2,3 accepted")
            Return gcAccountTypeError
        End If
    End Function

    Protected Function CDV_Capitec() As Integer
        Trace.WriteLine("CAPITEC CDV")
        If FAcctType = 1 Or FAcctType = 2 Or FAcctType = 3 Or FAcctType = 4 Or FAcctType = 6 Then
            If FastCalculate(0, 11, "21987654321") = 0 Then
                Trace.WriteLine("@Result = Success based on calculation")
                Return gcSuccess
            Else
                Trace.WriteLine("@Result = Failed based on calculation")
                Return gcAccountNumberError
            End If
        Else
            Trace.WriteLine("@Result = Failed, account type must be 1,2,3,4,6")
            Return gcAccountTypeError
        End If
    End Function

    Protected Function CDV_BankOfWindHoek() As Integer
        Trace.WriteLine("BANK OF WINDHOEK")
        If FAcctType = 1 Or FAcctType = 2 Or FAcctType = 3 Then
            Trace.WriteLine("@Result = Success based on account type being 1,2,3 -- NO CDV")
            Return gcSuccess
        Else
            Trace.WriteLine("@Result = Failed account type must be 1,2 or 3")
            Return gcAccountTypeError
        End If
    End Function

    Protected Function CDV_HBZ() As Integer
        Trace.WriteLine("HBZ CDV VALIDATION")
        If (FAcctType = 1 Or FAcctType = 2) And (FAcctNumber.Length < 11) Then
            Trace.WriteLine("@Result = Success, Because account type is 1 or 2 and the unformatted length is less than 11")
            Return gcSuccess
        End If


        If (FAcctType = 1 Or FAcctType = 2) And (FAcctNumber.Length = 11) Then
            If FastCalculate(0, 11, "00000137131") = 0 Then
                Trace.WriteLine("@Result = Success based on calculation")
                If ExceptionCode_D() = gcSuccess Then
                    Trace.WriteLine("@Result = Success based on calculation and Exception D")
                    Return gcSuccess
                Else
                    Trace.WriteLine("@Result = Success based on calculation, but failed exception D")
                    Return gcAccountNumberError
                End If
            Else
                Trace.WriteLine("@Result = Failed first account calculation")
                Return gcAccountNumberError
            End If
        Else
            Trace.WriteLine("@Result = Failed Only account type 1 and 2 accepted")
            Return gcAccountTypeError
        End If
    End Function

    Protected Function CDV_HSBC() As Integer
        Trace.WriteLine("HSBC CDV")
        If FAcctType = 1 Then
            Trace.WriteLine("@Result = Success based on HSBC Account type 1")
            Return gcSuccess
        Else
            Trace.WriteLine("Not account type 1")
            Return gcAccountNumberError
        End If
    End Function


    Protected Function CDV_Investec() As Integer
        Trace.WriteLine("INVESTEC BANK CDV")
        If FAcctType = 1 Or FAcctType = 2 Or FAcctType = 4 Then
            Dim awnser As Integer = _
            (23 * AcctDigit(3) + 19 * AcctDigit(4) + _
            17 * AcctDigit(5) + 13 * AcctDigit(6) + _
            7 * AcctDigit(7) + 5 * AcctDigit(8) + _
            3 * AcctDigit(9) + AcctDigit(10)) Mod 11

            Trace.WriteLine("Calucation = (23 * AcctDigit(3) + 19 * AcctDigit(4) + 17 * AcctDigit(5) + 13 * AcctDigit(6) + 7 * AcctDigit(7) + 5 * AcctDigit(8) + 3 * AcctDigit(9) + AcctDigit(10)) Mod 11")
            If awnser = 0 Then
                Trace.WriteLine("@Result = Success based on calculation")
                Return gcSuccess
            Else
                Trace.WriteLine("@Result = Failed based on calculation")
                Return gcAccountNumberError
            End If
        Else
            Trace.WriteLine("@Result = Failed Only account type 1 , 2 and 4 accepted")
            Return gcAccountTypeError
        End If
    End Function

    Protected Function CDV_Barclays() As Integer
        Trace.WriteLine("Barclays Bank CDV")
        If FAcctType = 1 Then
            Dim Result As Integer = _
            ((AcctDigit(0) * 1) + _
            (AcctDigit(1) * 2) + _
            (AcctDigit(2) * 4) + _
            (AcctDigit(3) * 8) + _
            (AcctDigit(4) * 5) + _
            (AcctDigit(5) * 10) + _
            (AcctDigit(6) * 9) + _
            (AcctDigit(7) * 7) + _
            (AcctDigit(8) * 3) + _
            (AcctDigit(9) * 6) + _
            (AcctDigit(10) * 1)) Mod 11

            Trace.WriteLine("Caluculation = ((AcctDigit(0) * 1) + (AcctDigit(1) * 2) + (AcctDigit(2) * 4) + (AcctDigit(3) * 8) + (AcctDigit(4) * 5) +(AcctDigit(5) * 10) + (AcctDigit(6) * 9) + `(AcctDigit(7) * 7) + (AcctDigit(8) * 3) + (AcctDigit(9) * 6) + (AcctDigit(10) * 1)) Mod 11")

            If Result = gcSuccess Then
                Trace.WriteLine("@Result = Success based on calculation")
                Return gcSuccess
            Else
                Trace.WriteLine("@Result = Failed based on calculation")
                Return gcAccountNumberError
            End If

        Else
            Trace.WriteLine("@Result = Failed, only account type 1 accpeted")
            Return gcAccountTypeError
        End If
    End Function

    Protected Function CDV_SWABou() As Integer
        Trace.WriteLine("SWA BOU")
        Trace.WriteLine("@No cdv just account type check for 1 or 2")
        If FAcctType = 1 Or FAcctType = 2 Then
            Return gcSuccess
        Else
            Return gcAccountTypeError
        End If
    End Function

    Protected Function CDV_Habib() As Integer
        Trace.WriteLine("HABIB - CDV Starting")
        If (FAcctType = 1) Or (FAcctType = 2) Then
            Dim result As Integer = FastCalculate(0, 11, "00007654321")
            If (result = 0) Then
                Trace.WriteLine("HABIB - New account number accepted")
                Return gcSuccess
            Else
                If (result = 1) And (ExceptionCode_J() = gcSuccess) Then
                    Trace.WriteLine("HABIB - Return value is 1 but passed on exception code j")
                    Return gcSuccess
                Else
                    Return gcAccountNumberError
                End If

            End If
        Else
            Trace.WriteLine("HABIB - Return account type error only 1 and 2 accepted")
            Return gcAccountTypeError
        End If
    End Function

    Protected Function CDV_LesothoBank() As Integer
        Trace.WriteLine("LESOTHO BANK")
        If FAcctType = 1 Or FAcctType = 2 Then
            If ExceptionCode_i() = gcInternalError Then
                Trace.WriteLine("@Result = Success based on calculation")
                Return gcInternalError
            Else
                Trace.WriteLine("@Result = Failed based on calculation")
                Return gcSuccess
            End If
        Else
            Trace.WriteLine("@Result = Account type must be 1 or 2")
            Return gcAccountTypeError
        End If
    End Function

    Protected Function CDV_NBS() As Integer
        Trace.WriteLine("NBS CDV VALIDATION")
        If FAcctType = 2 Or FAcctType = 3 Or FAcctType = 4 Or FAcctType = 6 Then
            If FastCalculate(0, 10, "14329874321") = 0 Then
                Trace.WriteLine("@Result = Success based on calculation")
                Return gcSuccess
            Else
                Trace.WriteLine("@Result = Failed based on calculation")
                Return gcAccountNumberError
            End If
        Else
            Trace.WriteLine("@Result = Only 2,3,4,6 accepted ")
            Return gcAccountTypeError
        End If
    End Function

    Protected Function CDV_Twenty20() As Integer
        Trace.WriteLine("TWENTY20 CDV")
        If FAcctType = 1 Or FAcctType = 2 Or FAcctType = 3 Or FAcctType = 4 Or FAcctType = 6 Then
            If FastCalculate(0, 11, "27654321000") = 0 Then
                Trace.WriteLine("@Result = Success based on calculation")
                Return gcSuccess
            Else
                Trace.WriteLine("@Result = Failed based on calculation")
                Return gcAccountNumberError
            End If
        Else
            Trace.WriteLine("@Result = Failed, only accept 1,2,3,4,6")
            Return gcAccountTypeError
        End If
    End Function

    Protected Function CDV_ABN() As Integer
        Trace.WriteLine("ABN CDV VALIDATION")
        If FAcctType = 1 Then
            If FastCalculate(0, 11, "00987654321") = 0 Then
                Trace.WriteLine("@Result = Success based on calculation")
                Return gcSuccess
            Else
                Trace.WriteLine("@Result = Failed based on calculation")
                Return gcAccountNumberError
            End If
        Else
            Trace.WriteLine("@Result = Only account type 1 accepted")
            Return gcAccountTypeError
        End If
    End Function

    Protected Function CDV_IthalaBank() As Integer
        Trace.WriteLine("ITHALA BANK CDV")
        If FAcctType = 2 Or FAcctType = 3 Or FAcctType = 4 Or FAcctType = 6 Then
            If FastCalculate(0, 10, "12121212121") = 0 Then
                Trace.WriteLine("@Result = Success based on calculation")
                Return gcSuccess
            Else
                Trace.WriteLine("@Result = Failed based on calculation")
                Return gcAccountNumberError
            End If
        Else
            Trace.WriteLine("@Result = Only 2,3,4,6")
            Return gcAccountTypeError
        End If
    End Function

    Protected Function CDV_FNB_PERMAccounts() As Integer
        Trace.WriteLine("FNB PERM ACCOUNTS")
        If FAcctType = 4 Or FAcctType = 6 Then
            ZeroFrontPadAccountNumber(FAcctNumber, 13)
            If ExceptionCode_G() = gcSuccess Then
                Trace.WriteLine("@Result = Success based on calculation")
                Return gcSuccess
            Else
                Trace.WriteLine("@Result = Failed based on calculation")
                Return gcAccountNumberError
            End If
        Else
            Trace.WriteLine("@Result = Failed account type must be 4 or 6")
            Return gcAccountTypeError
        End If
    End Function

    Protected Function CDV_PermAndPeoplesBank() As Integer
        Trace.WriteLine("PERM AND PEOPLES BANK")
        If (FAcctType <> 4) And (FBranchCode = 760005) Then
            Trace.WriteLine("@Result = Failed Only account type 4 accepted")
            Return gcAccountTypeError
        End If

        If (FAcctType = 4 Or FAcctType = 6) Then
            Return CDV_FNB_PERMAccounts()
        Else
            Trace.WriteLine("@Result = Incorrect account type must be 4 or 6 and only 4 in case of b code 760005")
            Return gcAccountTypeError
        End If

    End Function

    Protected Function CDV_FBC() As Integer
        Trace.WriteLine("FBC CDV VALIDATION")
        If FAcctType = 2 Or FAcctType = 3 Or FAcctType = 4 Or FAcctType = 6 Then
            If FastCalculate(0, 10, "13579135791") = 0 Then
                Trace.WriteLine("@Result = Success based on calculation")
                Return gcSuccess
            Else
                Trace.WriteLine("@Result = Failed based on calculation")
                Return gcAccountNumberError
            End If
        Else
            Trace.WriteLine("@Result = Failed only 2,3,4,6 types allowed")
            Return gcAccountTypeError
        End If
    End Function

    Protected Function CDV_UniBank() As Integer
        Trace.WriteLine("UNIBAMK CDV")
        Trace.WriteLine("No CDV - Just check account number is not zero filled and the type is 3")
        If (FAcctType = 3 And FAcctNumber <> "00000000000") Then
            Return gcSuccess
        Else
            Return gcAccountTypeError
        End If
    End Function


    Protected Function CDV_AbaricaBank() As Integer
        Trace.WriteLine("CDV Abarica Bank")
        If FAcctType = 1 Or FAcctType = 2 Or FAcctType = 3 Or FAcctType = 4 Then
            If FastCalculate(0, 10, "78624354321") = 0 Then
                Trace.WriteLine("@Result = Success based on calculation")
                Return gcSuccess
            Else
                Trace.WriteLine("@Result = failed based on calculation")
                Return gcAccountNumberError
            End If
        Else
            Trace.WriteLine("@Result = Failed only account type 1 is allowed")
            Return gcAccountTypeError
        End If
    End Function

    Protected Function CDV_ReserveBank() As Integer
        Trace.WriteLine("Reserve Bank")
        If FAcctType = 1 Then
            If FastCalculate(0, 11, "11187654321") = 0 Then
                Trace.WriteLine("@Result = Success based on calculation")
                Return gcSuccess
            Else
                Trace.WriteLine("@Result = failed based on calculation")
                Return gcAccountNumberError
            End If
        Else
            Trace.WriteLine("@Result = Failed only account type 1 is allowed")
            Return gcAccountTypeError
        End If
    End Function



    Protected Function CheckDigitVerification() As Integer

        Trace.WriteLine("Internal check engine started")

        Select Case Val(FBranchCode)

            'Passed Testing
        Case 0 To 60666, 61168 To 99999
                Return CDV_StandardBankSA()

                'Passed Testing
            Case 60667 To 61167, 660000 To 669999
                Return CDV_StandardBankLesotho()

                'Passed Testing
            Case 100000 To 101608, 101610 To 170304, 170306 To 199999
                Return CDV_NedBank()

                'Waiting Data from Goolam
            Case 101609
                Return CDV_CapeOfGoodHope()

                'Passed Testing
            Case 200000 To 299999
                Return CDV_FNB() ' CDV_NEW_FNB()

                'Passed Testing
            Case 300000 To 349999, 420000 To 429999, 500000 To 569999, 630000 To 659999
                Return CDV_ABSA()

                'Needs Testing
            Case 350000 To 359999
                Return CDV_CitiBank()

                'Needs Testing
            Case 360000 To 360999, 390000 To 390999
                Return CDV_NedbankLesotho()

                'Needs Testing
            Case 400000 To 400999
                Return CDV_PEP()

                'Needs Testing
            Case 410000 To 419999
                Return CDV_BankOfAthens()

            Case 430000 To 430999
                Return CDV_AfricanBankLimited()

                'Needs Testing
            Case 431000 To 431979
                Return CDV_TebaBank()

                'Needs Testing
            Case 431980 To 431999
                Return CDV_TebaBankExtension()

                'Needs(testing)
            Case 440000 To 449999
                Return CDV_BOE()


            Case 450236
                If FAcctType = 2 Then
                    Return gcSuccess
                Else
                    Return gcAccountTypeError
                End If


                'Needs Testing
            Case 450000 To 450235, 450237 To 459999
                Return CDV_Mercantile()


                'Needs Testing
            Case 460000 To 460999
                Return CDV_PostOffice()

            Case 461000 To 461999
                Return CDV_NedBankNamibia()

            Case 462000 To 462999
                Return CDV_Rennies()

                'Needs Testing
            Case 470000 To 479999
                Return CDV_Capitec()


                'Needs Testing
            Case 480000 To 489999
                Return CDV_BankOfWindHoek()

            Case 490000 To 490999
                Return CDV_MTN()


                'Needs Testing
            Case 570000 To 570999
                Return CDV_HBZ()


                'Needs Testing
            Case 580000 To 580999
                Return CDV_Investec()

      Case 587000 To 587999
                Return CDV_HSBC()

        'Needs Testing
            Case 590000 To 590999
                Return CDV_Barclays()

                'Needs Testing
            Case 690000 To 699999
                Return CDV_SWABou()

                'Needs Testing
            Case 700000 To 709999
                Return CDV_Habib()

                'Needs Testing
            Case 710000 To 719999
                Return CDV_LesothoBank()

                'Needs Testing
            Case 720000 To 720999
                Return CDV_NBS()

                'Needs Testing
            Case 730000 To 730044, 730046 To 730999
                Return CDV_Twenty20()

                'Needs Testing
            Case 740000 To 740999
                Return CDV_ABN()


                'Needs Testing
            Case 750000 To 759999
                Return CDV_IthalaBank()


                'Needs testing
            Case 760005
                Return CDV_FNB_PERMAccounts()

                'Needs Testing
            Case 760000 To 760004, 760006 To 769999
                Return CDV_PermAndPeoplesBank()


                'Needs Testing
            Case 780000 To 789999
                Return CDV_FBC()


                'Needs Testing
            Case 790000 To 799999
                Return CDV_UniBank()

            Case 800000 To 800999
                Return CDV_AbaricaBank()
                'Needs Testing
            Case 900000 To 999999
                Return CDV_ReserveBank()


            Case Else
                Trace.WriteLine("The branch code specified is unhandled by this CDV")
                Return gcBranchNumError
        End Select


    End Function

    Public Function PureDigitString(ByVal AccountNumber As String) As Boolean
        Dim result As Boolean = True
        For Each Character As String In AccountNumber
            result = (Character = "0" Or Character = "1" Or Character = "2" Or Character = "3" Or Character = "4" Or Character = "5" Or Character = "6" Or Character = "7" Or Character = "8" Or Character = "9")
            If (result = False) Then
                Return False
            End If
        Next
        Return True
    End Function

    Private Sub TraceEnd()
        Trace.WriteLine("")
        Trace.WriteLine("==================== END CDV VALIDATION ==========================")
    End Sub

    Public Function Validate(ByVal AccountNumber As String, ByVal BranchCode As String _
     , ByVal AccountType As Integer) As Integer

        Trace.WriteLine("==================== Start CDV VALIDATION ==========================")
        Trace.WriteLine("")
        Trace.WriteLine("Account Number = " + AccountNumber)
        Trace.WriteLine("BranchCode = " + BranchCode)
        Trace.WriteLine("Account Type = " + System.Convert.ToString(AccountType))
        Trace.WriteLine("")

        Dim lResult As Integer = -1

        Trace.WriteLine("Formatting account number ...")

        FBranchCode = BranchCode
        FOrigAcctNumber = AccountNumber

        FAcctNumber = ZeroFrontPadAccountNumber(AccountNumber, cStandardAcctNumberLength)
        Trace.WriteLine("Formatted account number = " + FAcctNumber)
        Trace.WriteLine("")

        FAcctType = AccountType

        If Not (PureDigitString(FAcctNumber)) Then
            Trace.WriteLine("CDV Fail account number is not pure digit.")
            TraceEnd()
            Return gcAccountNumberError
        End If


        'Check the format of the branch code
        If Not (BranchCodeFormatIsGood()) Then
            Trace.WriteLine("CDV Fail Branch number format is not good.")
            TraceEnd()
            Return gcBranchNumError
        End If




        Return CheckDigitVerification()



    End Function



    Public Sub New()
    End Sub

    Protected Overrides Sub Finalize()
        MyBase.Finalize()
    End Sub

End Class
