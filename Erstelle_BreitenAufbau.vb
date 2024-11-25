Function Erstelle_BreitenAufbau(ByVal Ort As Short, ByVal blnkoppel As Boolean, ByVal blbRahmEinl As Boolean, Optional ByRef tmpBild As String = "", Optional ByRef blnRVNurMontiert As Boolean = False) As String

 

            Dim blnTeilBearbeiten As Boolean

            Dim z As Integer, z1 As Integer, Fz As Short, T As Short, Anz As Integer, FlgNr As Short, RH As Short, Ez As Short, IndexWerk As Short, IndexVormont As Short, f_t_rh As Short, Se As Short

            Dim SBreite As Double, Br As Double, KFalz As Double, GBreite As Double, Mass As Double

            Dim tmpRVZ As Short

            Dim dummy As String = ""

            Dim RVNr(30) As Short

            Dim Breite() As Double, tmpBreite(,) As Double

            Dim blnOk As Boolean, blnSF_LO As Boolean, blnSF_RO As Boolean

 

            On Error GoTo ERROR_ErrF

 

            ' Initialisierung (TX 670)

            dummy = ""

            tmpBild = ""

            Erstelle_BreitenAufbau = ""

 

            If Pos_Mode = PosModeInformationen.Pos_Sonderfenster Then

                For z = 1 To TGZ

                    If Not T_G(z).blnSegment Then

                        If T_G(z).Tag = Rahmen_Tag Then

                            If T_G(z).Ort = Ort Then

                                RH = T_G(z).FD.RH

                                Ez = T_G(z).Ez

                                GBreite = T_G(z).Breite

                                For z1 = 1 To TGZ

                                    If T_G(z1).Tag = RahEinl_Tag And T_G(z1).FD.RH = RH And T_G(z1).rd.Platte <> Kein_Eintrag Then

                                        GBreite += (T_G(z1).Breite - T_G(z1).K_Falz)

                                    End If

                                Next z1

                                For z1 = 1 To TGZ

                                    If T_G(z).Tag = RahVerbr_Tag Then

                                        If T_G(z1).Ort = Ort And T_G(z1).Ez = Ez And T_G(z1).FD.RH = RH And T_G(z1).rv.Proforma = False Then

                                            If z1 = Breiteste_RV_auf_Ebene_T_G(T_G(z1).Ez, T_G(z1).Ort, T_G(z1).rv.Ebene) Then

                                                GBreite += (T_G(z1).Breite - T_G(z1).K_Falz)

                                            End If

                                        End If

                                    End If

                                Next z1

                                Str3e.AddString(dummy, CStr(Set_Measure_Mass(genu_Grund_Masstypen.MTyp_mm, GBreite)), "/")

                            End If

                        End If

                    End If

                Next z

                Erstelle_BreitenAufbau = dummy

                Exit Function

            End If

 

            '-- Koppelelement nicht aufteilen

            If EFeldZ > 0 Then

                Anz = EFeldZ

                ReDim Breite(EFeldZ)

                For z = 1 To TGZ

                    If T_G(z).Tag = Rahmen_Tag Then

                        If T_G(z).Ez > EFeldZ Then

                            ReDim Breite(T_G(z).Ez)

                        End If

                    End If

                Next

            Else

                Anz = 1

                ReDim Breite(1)

            End If

 

            '-- Rahmenteile

            For z = 1 To TGZ

                If T_G(z).Tag = Rahmen_Tag Then

                    If T_G(z).Ort = Ort Then

                        If Breite(T_G(z).Ez) = 0 Then

                            Breite(T_G(z).Ez) = T_G(z).Breite - T_G(z).K_Falz '(RQN 041718)

                            If Breite(T_G(z).Ez) = 0 And T_G(z).Form.SForm = Sonderform.rund Then '(INA 10718)

                                Breite(T_G(z).Ez) = T_G(z).Breite

                            End If

                            If gblnRahProfilAnzeig Then

                                tmpBild = ""

                                If SystemArt = SysHolz Then

                                    If Lese_Profile(RAPr_Tag, T_G(z).TProf.APr) <> Kein_Eintrag Then

                                        tmpBild = St_Profile.Bild

                                    End If

                                End If

                            End If

                        Else

                            Exit For

                        End If

                    End If

                End If

            Next z

 

            '-- Rahmeneinleimer als Rahmenprofil seperat betrachten

            For z = 1 To TGZ

                If T_G(z).Tag = RahEinl_Tag And T_G(z).rd.Platte <> Kein_Eintrag Then

                    If T_G(z).Ort = Ort Then

                        Mass = T_G(z).Breite - T_G(z).K_Falz

                        Breite(T_G(z).Ez) += Mass

                        'Exit For (RQN 084363)

                    End If

                End If

            Next z

 

            '-- ggf. Rahmeneinleimer

            If blbRahmEinl Then

                ReDim tmpBreite(Anz, RFeZ)

                For z = 1 To RFeZ

                    FlgNr = -1

                    '(RQN 081116)

                    For Fz = 0 To RFZ

                        If F_D(Fz).RF_Fz = z Then

                            Select Case R_D(F_D(Fz).fp(Ort).RH).Tag

                                Case Is = Rahmen_Tag, RahEinl_Tag

 

                                    FlgNr = Fz

                            End Select

                        End If

                    Next Fz

 

                    If FlgNr > -1 Then

                        For T = 1 To TGZ

                            If T_G(T).Tag = RahEinl_Tag And T_G(T).rd.Platte = Kein_Eintrag Then

                                blnOk = False

                                If T_G(T).Ort = Ort Then

                                    blnOk = True

                                End If

                                If Not blnOk Then '(RQN 057498)

                                    If f_t_rh > -1 Then

                                        If T_G(T).Lage > 3 Then

                                            If T_G(f_t_rh).Lage = T_G(T).Lage Then

                                                blnOk = True

                                            End If

                                        End If

                                    End If

                                End If

                                If T_G(T).Aufteil.ATeil > 0 Then 'RQN 089243

                                    blnOk = False

                                End If

                                If blnOk Then

                                    If Ermittle_EinlFeld(Get_TNr(T), T_G(T).Se) = FlgNr Then

                                        Mass = R_D(Get_TNr(T)).Breite - R_D(Get_TNr(T)).FalzKor

                                        tmpBreite(T_G(z).Ez, z) += Mass

                                    Else

                                        '(RQN 040580)

                                        If T_G(T).Se = 0 Then

                                            Se = 1

                                        Else

                                            Se = 0

                                        End If

                                        If Ermittle_EinlFeld(Get_TNr(T), Se) = FlgNr Then

                                            Mass = R_D(Get_TNr(T)).Breite - R_D(Get_TNr(T)).FalzKor

                                            tmpBreite(T_G(z).Ez, z) += Mass

                                        End If

                                    End If

                                End If

                            End If

                        Next T

                    Else

                        '-- Rahmenfelder die nicht berücksichtigt

                        '-- werden müssen, hier mit einem Defaultwert vorbelegen (RQN 024640)

                        For Fz = 1 To Anz

                            tmpBreite(Fz, z) = 9999

                        Next Fz

                    End If

                Next z

 

                '-- nun die kleineste Breite mit verrechnen

                For Fz = 1 To Anz

                    tmpBreite(Fz, 0) = 9999

                    For z = 1 To RFeZ

                        If tmpBreite(Fz, z) < tmpBreite(Fz, 0) Then

                            tmpBreite(Fz, 0) = tmpBreite(Fz, z)

                        End If

                    Next z

                    If tmpBreite(Fz, 0) < 9999 Then

                        Breite(Fz) += tmpBreite(Fz, 0)

                    End If

                Next Fz

            End If

 

            '-- Rahmenverbreiterungen zurücksetzen

            Anz = UBound(Breite) '(RQN 085316)

            ReDim tmpBreite(Anz, 4)

            For z = 1 To RVZ

                RV_D(z).dummy = 0

            Next z

 

            '-- Indexe für Montageart Werk und Vormontiert ermitteln

            IndexWerk = Ermittle_MontIndex(AuswahlMontiert.Werk)

            IndexVormont = Ermittle_MontIndex(AuswahlMontiert.Vormontiert)

 

            '-- Rahmenverbreiterungen

            For z = 1 To RVZ

                blnTeilBearbeiten = False '--RQN hsc

                If RV_D(z).Art <> Roll_Tag Then

                    If RV_D(z).Typ <> Kein_Eintrag Then

                        If RV_D(z).dummy = 0 Then

                            If RV_D(z).Ort = Ort Then

                                blnTeilBearbeiten = True

                            Else

                                '-- schräge Verbreiterungen beachten (RQN 023061)

                                Select Case Ort

                                    Case Is = un

 

                                        If RV_D(z).Ort = SF_LU Then

                                            blnTeilBearbeiten = True

                                        End If

                                        If RV_D(z).Ort = SF_RU Then

                                            blnTeilBearbeiten = True

                                        End If

                                    Case Is = ob

                                        If SF_oH(ob) Then '(RQN 041288)

                                            If RV_D(z).Ort = SF_LO And RV_D(z).Ri = 0 And Not blnSF_RO Then

                                                blnTeilBearbeiten = True

                                                blnSF_LO = True '(RQN 078541)

                                            End If

                                            If RV_D(z).Ort = SF_RO And RV_D(z).Ri = 0 And Not blnSF_LO Then

                                                blnTeilBearbeiten = True

                                                blnSF_RO = True '(RQN 078541)

                                            End If

                                        End If

                                    Case Is = li

                                        If RV_D(z).Ort = SF_LO And RV_D(z).Ri = 1 Then '(RQN 023993)

                                            blnTeilBearbeiten = True

                                        End If

                                    Case Is = re

                                        If RV_D(z).Ort = SF_RO And RV_D(z).Ri = 1 Then '(RQN 023993)

                                            blnTeilBearbeiten = True

                                        End If

                                End Select

                            End If

 

                            '-- ggf. nur Rahmenverbreiterungen nehmen die am Profil montiert sind (PA 02629)

                            If blnRVNurMontiert Then

                                If RV_D(z).MontLose <> IndexWerk And RV_D(z).MontLose <> IndexVormont Then

                                    blnTeilBearbeiten = False

                                End If

                            End If

 

                            If blnTeilBearbeiten Then

                                ' Breite = Breite vom Rahmenholz + (Breite der Verbr. - Falzkor. der Verbr.)

                                SBreite = (RV_D(z).Breite - RV_D(z).K_Falz)

                                If RV_D(z).Ez = 0 Then '(RQN 043752)

                                    If RV_D(z).GlobE = 0 Then '(RQN 055094)

                                        For z1 = 1 To UBound(Breite)

                                            tmpBreite(z1, RV_D(z).Nut) += SBreite '(RQN 085316)

                                        Next z1

                                    Else

                                        Breite(RV_D(z).GlobE) += SBreite '(RQN 053473)

                                    End If

 

                                Else

                                    If RV_D(z).Ez <= UBound(Breite) Then

                                        tmpBreite(RV_D(z).Ez, RV_D(z).Nut) += SBreite

                                    End If

                                End If

                            End If

                        End If

                    End If

                End If

            Next z

 

            ' Hier die größe Breite innerhalb einer Nut ermitteln und dann zuweisen (RQN 085316)

            If RVZ > 0 Then

                For z = 1 To UBound(Breite)

                    SBreite = 0

                    ' über alle 4 Nuten gehen

                    For z1 = 1 To 4

                        If tmpBreite(z, z1) > SBreite Then

                            SBreite = tmpBreite(z, z1)

                        End If

                    Next z1

                    ' Verbreiterungen mit Nut = (kein) dazu addieren

                    SBreite += tmpBreite(z, 0)

                    Breite(z) += SBreite

                Next z

            End If

 

            For z = 1 To Anz

                If Breite(z) > 0 Then

                    Str3e.AddString(dummy, Set_Measure_ErfassFormat(genu_Grund_Masstypen.MTyp_mm, Breite(z)), "/")

                End If

            Next z

 

            Erstelle_BreitenAufbau = dummy

 

            On Error GoTo 0

            Exit Function

 

ERROR_ErrF:

 

            errhandler.GlobalErrorHandler(Err.Number, Err.Description, basModuleName_Gldbauft, "Erstelle_BreitenAufbau$")

            GlobalErrorOutput(ErrLogCls.MessageBox)

            Resume Next

        End Function

 
