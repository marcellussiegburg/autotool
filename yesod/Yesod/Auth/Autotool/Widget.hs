{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Yesod.Auth.Autotool.Widget (authAutotoolWidget, createAccountWidget, errorWidget, invalidEmailWidget, mailSentWidget, resetPasswordWidget) where

import Yesod.Auth.Autotool.Type
import qualified Yesod.Auth.Autotool.Message as Msg

import Yesod.Auth
import Yesod.Core

import Control.Monad.Catch
import qualified Data.List as L
import Data.Function
import Data.Maybe
import qualified Data.Text as T
import System.IO

import qualified Control.Schule.Typ as Schule
import Control.Student.Type as Stud
import Control.Types

authAutotoolWidget ::
    (PathPiece AuthSchool,
     PathPiece AuthStudent, YesodAuthAutotool site,
     RenderMessage site YesodAuthAutotoolMessage) =>
    Maybe AuthSchool
    -> Maybe AuthSchool
    -> Maybe AuthStudent
    -> Maybe Student
    -> (AuthRoute -> Route site)
    -> WidgetT site IO ()
authAutotoolWidget u mschool mstud s tm = do
  schools <- getSchools
  theSchool <- getSchool u
  setTitleI Msg.LoginTitle
  let login = case u of
                 Nothing -> tm signinR
                 Just u' -> tm $ signinSchoolR u'
  [whamlet|
  $newline never
  <div .container>
    $if L.null schools
      <p>_{Msg.ErrorNoSchool}
    $else
      <form method="post" action=@{login} .form-signin role=form>
        <h2 .form-signin-heading>_{Msg.LoginTitle}
        $maybe u' <- theSchool
          <input type="hidden" name="school" value=#{toString $ Schule.unr u'}>
        $nothing
          <select .form-control name="school">
            $forall school <- schools
              <option value=#{toString $ Schule.unr school}>
                #{toString $ Schule.name school}
        $maybe s' <- s
          <input type="text" name="matriculationnumber" .form-control placeholder=_{Msg.MatriculationNumber} required autofocus spellcheck="false" value=#{toString $ mnr s'}>
        $nothing
          <input type="text" name="matriculationnumber" .form-control placeholder=_{Msg.MatriculationNumber} required autofocus spellcheck="false">
        <input type="password" name="password" .form-control placeholder=_{Msg.Password} required>
        $maybe s' <- mstud
          $maybe u' <- mschool
            <p><a href=@{tm $ resetPasswordR u' s'}>_{Msg.ForgotPassword}
        <button type=submit .btn .btn-lg .btn-primary .btn-block>_{Msg.Login}
  |]

createAccountWidget :: (YesodAuthAutotool site, PathPiece AuthSchool,
                        RenderMessage site YesodAuthAutotoolMessage) =>
                       Maybe AuthSchool
                    -> (AuthRoute -> Route site) -> WidgetT site IO ()
createAccountWidget u tp = do
  schools <- getSchools
  theSchool <- getSchool u
  let create = case u of
                 Nothing -> tp createAccountR
                 Just u' -> tp $ createAccountSchoolR u'
  setTitleI Msg.RegisterTitle
  [whamlet|
  $newline never
  <div .container>
    $if L.null schools
      _{Msg.ErrorNoSchool}
    $else
      <form method="post" action=@{create} .form-signin role=form>
        <h2 .form-signin-heading>_{Msg.RegisterTitle}
        $maybe u' <- theSchool
          <input type="hidden" name="school" value=#{toString $ Schule.name u'}>
        $nothing
          <select .form-control name="school">
            $forall school <- schools
              <option value=#{toString $ Schule.unr school}>
                #{toString $ Schule.name school}
        <input type="text" name="matriculationnumber" .form-control placeholder=_{Msg.MatriculationNumber} required autofocus>
        <input type="text" name="firstname" .form-control placeholder=_{Msg.FirstName} required>
        <input type="text" name="surname" .form-control placeholder=_{Msg.Surname} required>
        <input type="text" name="email" .form-control placeholder=_{Msg.EmailAddress} required>
        <button type=submit .btn .btn-lg .btn-primary .btn-block>_{Msg.Register}
  |]

errorWidget :: 
    (MonadBaseControl IO m, MonadIO m, MonadThrow m,
     RenderMessage site YesodAuthAutotoolMessage) =>
    WidgetT site m ()
errorWidget = do
  setTitle "Error"
  [whamlet|
   $newline never
   <p>Error
  |]

invalidEmailWidget :: 
    (MonadBaseControl IO m, MonadIO m, MonadThrow m,
     RenderMessage site YesodAuthAutotoolMessage) =>
    WidgetT site m ()
invalidEmailWidget = do
  setTitleI Msg.ErrorInvalidEmail
  [whamlet|
   $newline never
   <p>_{Msg.ErrorInvalidEmail}
  |]

mailSentWidget :: 
    (MonadBaseControl IO m, MonadIO m, MonadThrow m,
     RenderMessage site YesodAuthAutotoolMessage) =>
    T.Text -> MNr ->
    WidgetT site m ()
mailSentWidget u m = do
  setTitleI Msg.ResetPasswordTitle
  [whamlet|
   $newline never
   <p>_{Msg.EmailSent u m}
  |]

resetPasswordWidget ::
    (YesodAuthAutotool master, PathPiece AuthSchool,
     PathPiece AuthStudent,
     RenderMessage master YesodAuthAutotoolMessage) =>
    AuthSchool
    -> AuthStudent
    -> (AuthRoute -> Route master)
    -> WidgetT master IO ()
resetPasswordWidget u m tp = do
  setTitleI Msg.ResetPasswordTitle
  [whamlet|
   $newline never
   <div .container>
     <form method="post" action=@{tp $ resetPasswordR u m} .form-signin role=form>
       <h2 .form-signin-heading>_{Msg.ResetPasswordTitle}
       <p>_{Msg.ResetPasswordDescription}
       <button type=submit .btn .btn-lg .btn-primary .btn-block autofocus>_{Msg.ResetPassword}
  |]
