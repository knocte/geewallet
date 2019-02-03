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

    let mainLayout = base.FindByName<StackLayout>("mainLayout")
    let passphrase = mainLayout.FindByName<Entry>("passphraseEntry")
    let passphraseConfirmation = mainLayout.FindByName<Entry>("passphraseEntryConfirmation")

    let email = mainLayout.FindByName<Entry>("emailEntry")
    let dob = mainLayout.FindByName<Entry>("dobEntry")
    let nextButton = mainLayout.FindByName<Button> "nextButton"

    let MaybeEnableCreateButton() =
        if (passphrase.Text <> null && passphrase.Text.Length > 0 &&
            passphraseConfirmation.Text <> null && passphraseConfirmation.Text.Length > 0 &&
            email.Text <> null && email.Text.Length > 0 &&
            dob.Text <> null && dob.Text.Length > 0) then
            nextButton.IsEnabled <- true

    let LENGTH_OF_CURRENT_UNSOLVED_WARPWALLET_CHALLENGE = 8

    let VerifyPassphraseIsGoodAndSecureEnough(): Option<string> =
        let containsASpaceAtLeast = passphrase.Text.Contains " "
        let containsADigitAtLeast = passphrase.Text.Any(fun c -> Char.IsDigit c)
        let containsPunctuation = passphrase.Text.Any(fun c -> c = ',' ||
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

        if (passphrase.Text <> passphraseConfirmation.Text) then
            Some "Seed passphrases don't match, please try again"
        elif (passphrase.Text.Length < LENGTH_OF_CURRENT_UNSOLVED_WARPWALLET_CHALLENGE) then
            Some (sprintf "Seed passphrases should contain %d or more characters, for security reasons"
                          LENGTH_OF_CURRENT_UNSOLVED_WARPWALLET_CHALLENGE)
        elif (not containsASpaceAtLeast) && (not containsADigitAtLeast) then
            Some "If your passphrase doesn't contain spaces (thus only a password), at least use numbers too"
        elif (passphrase.Text.ToLower() = passphrase.Text || passphrase.Text.ToUpper() = passphrase.Text) then
            Some "Mix lowercase and uppercase characters in your seed phrase please"
        elif (containsASpaceAtLeast && (not containsADigitAtLeast) && (not containsPunctuation)) then
            Some "For security reasons, please include numbers or punctuation in your passphrase (to increase entropy)"
        elif IsColdStorageMode() && AllWordsInPassphraseExistInDictionaries passphrase.Text then
            Some "For security reasons, please include at least one word that does not exist in any dictionary (to increase entropy)"
        else
            None

    let ToggleInputWidgetsEnabledOrDisabled (enabled: bool) =
        let entry1 = mainLayout.FindByName<Entry> "passphraseEntry"
        let entry2 = mainLayout.FindByName<Entry> "passphraseEntryConfirmation"
        let entry3 = mainLayout.FindByName<Entry> "dobEntry"
        let entry4 = mainLayout.FindByName<Entry> "emailEntry"

        let newCreateButtonCaption =
            if enabled then
                "Next"
            else
                "Loading..."

        Device.BeginInvokeOnMainThread(fun _ ->
            entry1.IsEnabled <- enabled
            entry2.IsEnabled <- enabled
            entry3.IsEnabled <- enabled
            entry4.IsEnabled <- enabled
            nextButton.IsEnabled <- enabled
            nextButton.Text <- newCreateButtonCaption
        )

    do
        Caching.Instance.BootstrapServerStatsFromTrustedSource()
            |> FrontendHelpers.DoubleCheckCompletionAsync

    [<Obsolete(DummyPageConstructorHelper.Warning)>]
    new() = WelcomePage(DummyPageConstructorHelper.GlobalFuncToRaiseExceptionIfUsedAtRuntime())

    member this.OnNextButtonClicked(sender: Object, args: EventArgs) =
        match VerifyPassphraseIsGoodAndSecureEnough() with
        | Some warning ->
            this.DisplayAlert("Alert", warning, "OK") |> ignore

        | None ->
                let dateFormat = "dd/MM/yyyy"
                match DateTime.TryParseExact(dob.Text, dateFormat, CultureInfo.InvariantCulture, DateTimeStyles.None) with
                | false,_ ->
                    this.DisplayAlert("Alert", sprintf "Invalid date or invalid date format (%s)" dateFormat, "OK") |> ignore

                | true,dateTime ->

                    ToggleInputWidgetsEnabledOrDisabled false

                    async {
                        let masterPrivKeyTask =
                            Account.GenerateMasterPrivateKey passphrase.Text dateTime (email.Text.ToLower())
                                |> Async.StartAsTask

                        Device.BeginInvokeOnMainThread(fun _ ->
                            FrontendHelpers.SwitchToNewPageDiscardingCurrentOne this
                                                                                (WelcomePage2 (state, masterPrivKeyTask))
                        )
                    } |> FrontendHelpers.DoubleCheckCompletionAsync

    member this.OnDobTextChanged(sender: Object, args: EventArgs) =
        MaybeEnableCreateButton()

    member this.OnEmailTextChanged(sender: Object, args: EventArgs) =
        MaybeEnableCreateButton()

    member this.OnPassphraseTextChanged(sender: Object, args: EventArgs) =
        MaybeEnableCreateButton()
