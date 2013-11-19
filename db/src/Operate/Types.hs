{-# language DeriveDataTypeable #-}
{-# language StandaloneDeriving #-}
{-# language Rank2Types #-}

module Operate.Types where

import Types.TT
import Types.Basic
import Types.Signed as S
import Types.Documented as D
import Types.ServerInfo
import Types.TaskTree
import Types.TaskDescription
import Types.Config
import Types.Instance as I
import Types.Solution

import qualified Service.Interface as SI

import qualified Control.Aufgabe.Typ as A
import qualified Control.Aufgabe.DB as A

import Control.Types ( toString, fromCGI, ok )

import Control.Applicative
import Data.Tree
import Data.Typeable

import Gateway.CGI ( embed, io, open, close, row, plain, textarea, submit, html, Form  )
import Autolib.ToDoc ( ToDoc, toDoc, text, Doc )
import Autolib.Reporter.IO.Type ( inform, reject )
import Autolib.Output as O
import qualified Autolib.Multilingual as M
import qualified Autolib.Multilingual.Html as H

import Inter.Types ( CacheFun )
import qualified Text.XHtml as X
import Util.Xml.Output
import Control.Monad ( mzero)


type Server = String


data Make = Make { server :: Server
                 , task :: Task  
                 }          
    deriving Typeable              
            
instance Show Make where show m = task m

make = Make            

get_task_tree server = do
    tts <- SI.get_task_types server
    return $ Category 
           { category_name = "all"
           , sub_trees = tts 
           }              

tt_to_tree server tt = case tt of 
  Task {} -> Data.Tree.Node ( Right ( task_name tt
                                    , make server (task_name tt) ) 
                            )  []
  Category {} -> Data.Tree.Node ( Left $ category_name tt )
      $ map (tt_to_tree server) $ sub_trees  tt
                 
get_task_config mk = do
  td <- io $ SI.get_task_description ( server mk ) ( task mk )
  let conf = task_sample_config td
  -- embed $ inform $ toDoc $ D.documentation conf
  return $ D.contents conf
  

verify_task_config mk conf = do
    SI.verify_task_config ( server mk ) ( task mk ) conf

instance ToDoc Description where  
    toDoc (DString s) = text s

task_config_editor title mk mauf = do
    open row
    plain title
    CString conf <- case mauf of
        Nothing  -> get_task_config mk
        Just auf -> 
            return $ CString $ toString $ A.config auf
    ms <- textarea $ conf
    sconf <- submit "submit"
    close -- row
    ver <- io $ verify_task_config mk $ CString $ case ms of
         Nothing -> conf
         Just s  -> s
    case ver of     
        Left err -> do 
            html $ M.specialize M.DE 
                 $ ( O.render  ( descr err) :: H.Html )
            mzero
        Right stc -> do 
            return stc

signed_task_config auf = 
    Signed { S.contents = ( toString $ A.typ auf
                          , CString $ toString $ A.config auf 
                          )
           , S.signature = toString $ A.signature auf     
           }              

update_signature_if_missing auf = 
    if toString (A.signature auf) == "missing"
    then do   
        ver <- io $ SI.verify_task_config 
           ( toString $ A.server auf ) 
           ( toString $ A.typ auf ) 
           ( CString $ toString $ A.config auf )
        case ver of       
            Left err -> do
                html $ M.specialize M.DE 
                      $ ( O.render  ( descr err) :: H.Html )
                mzero
            Right stc -> do
                plain $ "warning: update_signature_if_missing"
                let auf' = auf { A.signature = fromCGI $ signature stc }
                io $ A.put_signature (Just $ A.anr auf) auf'
                return auf'
    else return auf            

-- FIXME: where's the cache used?
generate :: A.Aufgabe 
         -> Integer
         -> CacheFun
         -> Form IO ( Signed (Task,Instance), Doc, Output )
generate auf seed cache = do
    auf <- update_signature_if_missing auf
    
    ( sti, desc, docsol ) <- do
        mres <- io $ 
             -- SI.get_task_instance_or_fail (toString $ A.server auf) 
             fmap Right $ SI.get_task_instance (toString $ A.server auf) 
             ( signed_task_config auf )
             ( show seed ) -- ?
        case mres of
            Left err -> do
                html $ M.specialize M.DE 
                      $ ( O.render  ( descr err) :: H.Html )
                mzero
            Right res -> return res
    let 
        SString sol = D.contents docsol
    return ( sti, text sol , descr desc  )
    
{-
evaluate :: A.Aufgabe    
         -> Signed (Task,Instance)
         -> String
         -> 
-}
evaluate auf sti s = do
    result <- 
       SI.grade_task_solution (toString $ A.server auf) 
           sti $ SString s
    case result of  
       Left err -> return ( Nothing, descr err )
       Right dd -> return ( Just $ ok $ round $ D.contents dd
                          , descr $ D.documentation dd
                          )  

-- doc = descr . D.documentation

descr desc = 
    let DString d = desc
    in xmlStringToOutput d

