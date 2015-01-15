{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Yesod.Auth.Autotool.Message where

import Data.Text

import Control.Types

import Yesod.Core

data YesodAuthAutotoolMessage =
    EmailAddress |
    EmailSent Text MNr |
    ErrorInvalidEmail |
    ErrorNoSchool |
    FirstName |
    ForgotPassword |
    Login |
    LoginTitle |
    MatriculationNumber |
    Password |
    Register |
    RegisterTitle |
    ResetPassword |
    ResetPasswordDescription |
    ResetPasswordTitle |
    Surname

defaultAuthAutotoolMessage :: YesodAuthAutotoolMessage -> Text
defaultAuthAutotoolMessage = englishAuthAutotoolMessage

englishAuthAutotoolMessage :: YesodAuthAutotoolMessage -> Text
englishAuthAutotoolMessage m =
    case m of
      EmailAddress -> "Email address"
      EmailSent u (MNr mat) -> concat
           ["Email was sent to the user of school ", u,
            " with ", englishAuthAutotoolMessage MatriculationNumber,
            " ", pack mat, "."]
      ErrorInvalidEmail -> "Error: Could not send email, because the provided email address is not valid. You may contact an admin for assistance."
      ErrorNoSchool -> "Error: There seems to not exist any school you could log in. You should contact the administrator of this website to resolve the issue."
      FirstName -> "First name"
      ForgotPassword -> "Forgot password?"
      Login -> "Sign in"
      LoginTitle -> "Login"
      MatriculationNumber -> "Matriculation number"
      Password -> "Password"
      Register -> "Register"
      RegisterTitle -> "Create Account"
      ResetPassword -> "Send password"
      ResetPasswordDescription -> "Did you forget your password and need a new one? Then submit the button and you will get an email with a new password."
      ResetPasswordTitle -> "Request a new password"
      Surname -> "Surname"

germanAuthAutotoolMessage :: YesodAuthAutotoolMessage -> Text
germanAuthAutotoolMessage m =
    case m of
      EmailAddress -> "E-Mail-Adresse"
      EmailSent u (MNr mat) -> concat
           ["Eine E-Mail wurde an den Studenten mit der ",
            germanAuthAutotoolMessage MatriculationNumber,
            " ", pack mat, " der Schule ",
            u, " gesendet."]
      ErrorInvalidEmail -> "Fehler: E-Mail konnte nicht gesendet werden, weil die hinterlegte E-Mail-Adresse nicht gültig ist. Sie könnten einen Adminstrator um Hilfe bitten."
      ErrorNoSchool -> "Fehler: Es scheint keine Schule zu existieren, über welche man sich einloggen könnte. Sie sollten einen Administrator dieser Webseite kontaktieren, um das Problem beheben zu lassen."
      FirstName -> "Vorname"
      ForgotPassword -> "Passwort vergessen?"
      Login -> "Einloggen"
      LoginTitle -> "Anmeldung"
      MatriculationNumber -> "Matrikelnummer"
      Password -> "Passwort"
      Register -> "Registrieren"
      RegisterTitle -> "Account erstellen"
      ResetPassword -> "Passwort zusenden"
      ResetPasswordDescription -> "Haben Sie ihr Passwort vergessen und benötigen ein neues? Dann betätigen Sie den Button, um eine E-Mail mit einem neuen Passwort zugestellt zu bekommen."
      ResetPasswordTitle -> "Neues Passwort anfordern"
      Surname -> "Familienname"
