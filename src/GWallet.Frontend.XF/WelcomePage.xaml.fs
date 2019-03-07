﻿namespace GWallet.Frontend.XF

open System
open System.Linq
open System.Globalization
open Plugin.Connectivity

open Xamarin.Forms
open Xamarin.Forms.Xaml

open GWallet.Backend

type WelcomePage(state: FrontendHelpers.IGlobalAppState) =
    inherit ContentPage()

    let _ = base.LoadFromXaml(typeof<WelcomePage>)

    let mainLayout = base.FindByName<StackLayout> "mainLayout"
    let passphraseEntry = mainLayout.FindByName<Entry> "passphraseEntry"
    let passphraseConfirmationEntry = mainLayout.FindByName<Entry> "passphraseConfirmationEntry"

    let emailEntry = mainLayout.FindByName<Entry> "emailEntry"
    let dobDatePicker = mainLayout.FindByName<DatePicker> "dobDatePicker"
    let nextButton = mainLayout.FindByName<Button> "nextButton"

    let MaybeEnableCreateButton() =
        let isEnabled = passphraseEntry.Text <> null && passphraseEntry.Text.Length > 0 &&
                        passphraseConfirmationEntry.Text <> null && passphraseConfirmationEntry.Text.Length > 0 &&
                        emailEntry.Text <> null && emailEntry.Text.Length > 0 &&
                        dobDatePicker.Date >= dobDatePicker.MinimumDate && dobDatePicker.Date < dobDatePicker.MaximumDate

        nextButton.IsEnabled <- isEnabled

    let LENGTH_OF_CURRENT_UNSOLVED_WARPWALLET_CHALLENGE = 8

    let VerifyPassphraseIsGoodAndSecureEnough(): Option<string> =
        let containsASpaceAtLeast = passphraseEntry.Text.Contains " "
        let containsADigitAtLeast = passphraseEntry.Text.Any(fun c -> Char.IsDigit c)
        let containsPunctuation = passphraseEntry.Text.Any(fun c -> c = ',' ||
                                                                    c = ';' ||
                                                                    c = '.' ||
                                                                    c = '?' ||
                                                                    c = '!' ||
                                                                    c = ''')
        let IsColdStorageMode() =
            use conn = CrossConnectivity.Current
            not conn.IsConnected

        let AllWordsInPassphraseExistInDictionaries(passphrase: string): bool =
            let words = passphrase.Split([|","; "."; " "; "-"; "_"|], StringSplitOptions.RemoveEmptyEntries)
            let result: bool = words

                               |> Seq.map (fun word -> word.ToLower())

                               // TODO: instead of using NBitcoin, we should use a proper dictionary library
                               //       (i.e. which might have more languages and more complete dictionaries)
                               |> Seq.forall(fun word -> not ((NBitcoin.Wordlist.AutoDetectLanguage word)
                                                                 .Equals NBitcoin.Language.Unknown)
                                            )
            result

        if passphraseEntry.Text <> passphraseConfirmationEntry.Text then
            Some "Seed passphrases don't match, please try again"
        elif passphraseEntry.Text.Length < LENGTH_OF_CURRENT_UNSOLVED_WARPWALLET_CHALLENGE then
            Some (sprintf "Seed passphrases should contain %d or more characters, for security reasons"
                          LENGTH_OF_CURRENT_UNSOLVED_WARPWALLET_CHALLENGE)
        elif (not containsASpaceAtLeast) && (not containsADigitAtLeast) then
            Some "If your passphrase doesn't contain spaces (thus only a password), at least use numbers too"
        elif passphraseEntry.Text.ToLower() = passphraseEntry.Text || passphraseEntry.Text.ToUpper() = passphraseEntry.Text then
            Some "Mix lowercase and uppercase characters in your seed phrase please"
        elif containsASpaceAtLeast && (not containsADigitAtLeast) && (not containsPunctuation) then
            Some "For security reasons, please include numbers or punctuation in your passphrase (to increase entropy)"
        elif IsColdStorageMode() && AllWordsInPassphraseExistInDictionaries passphraseEntry.Text then
            Some "For security reasons, please include at least one word that does not exist in any dictionary (to increase entropy)"
        else
            None

    let ToggleInputWidgetsEnabledOrDisabled (enabled: bool) =
        let newCreateButtonCaption =
            if enabled then
                "Next"
            else
                "Loading..."

        Device.BeginInvokeOnMainThread(fun _ ->
            passphraseEntry.IsEnabled <- enabled
            passphraseConfirmationEntry.IsEnabled <- enabled
            dobDatePicker.IsEnabled <- enabled
            emailEntry.IsEnabled <- enabled
            nextButton.IsEnabled <- enabled
            nextButton.Text <- newCreateButtonCaption
        )

    do
        let todayDate = DateTime.Now.Date
        dobDatePicker.MinimumDate <- todayDate.Subtract(TimeSpan.FromDays(120. * 365.))
        dobDatePicker.MaximumDate <- todayDate

        Caching.Instance.BootstrapServerStatsFromTrustedSource()
            |> FrontendHelpers.DoubleCheckCompletionAsync

    [<Obsolete(DummyPageConstructorHelper.Warning)>]
    new() = WelcomePage(DummyPageConstructorHelper.GlobalFuncToRaiseExceptionIfUsedAtRuntime())

    member this.OnNextButtonClicked(sender: Object, args: EventArgs) =
        match VerifyPassphraseIsGoodAndSecureEnough() with
        | Some warning ->
            this.DisplayAlert("Alert", warning, "OK") |> ignore

        | None ->
                ToggleInputWidgetsEnabledOrDisabled false
                let dateTime = dobDatePicker.Date
                async {
                    let masterPrivKeyTask =
                        Account.GenerateMasterPrivateKey passphraseEntry.Text dateTime (emailEntry.Text.ToLower())
                            |> Async.StartAsTask

                    Device.BeginInvokeOnMainThread(fun _ ->
                        FrontendHelpers.SwitchToNewPageDiscardingCurrentOne this
                                                                            (WelcomePage2 (state, masterPrivKeyTask))
                    )
                } |> FrontendHelpers.DoubleCheckCompletionAsync


    member this.OnDobDateChanged (sender: Object, args: DateChangedEventArgs) =
        MaybeEnableCreateButton()

    member this.OnEmailTextChanged(sender: Object, args: EventArgs) =
        MaybeEnableCreateButton()

    member this.OnPassphraseTextChanged(sender: Object, args: EventArgs) =
        MaybeEnableCreateButton()

