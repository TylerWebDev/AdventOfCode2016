module TLS where

import IPV7

data TLSSupport
    = Inconsequential
    | Yes
    | No
    deriving (Show)

instance Monoid TLSSupport where
    mempty =
        Inconsequential
    mappend Inconsequential n =
        n
    mappend No _ =
        No
    mappend Yes No =
        No
    mappend Yes _ =
        Yes

componentsSupportTLS :: IPV7Component -> TLSSupport
componentsSupportTLS (Letter _) =
    Inconsequential
componentsSupportTLS (Palindrome _) =
    Yes
componentsSupportTLS (Nested n) =
    mconcat (map nestedComponentsSupportTLS n)

nestedComponentsSupportTLS :: IPV7Component -> TLSSupport
nestedComponentsSupportTLS (Letter _) =
    Inconsequential
nestedComponentsSupportTLS _ =
    No

ipSupportsTLS :: IPV7 -> Bool
ipSupportsTLS ip =
    case mconcat (map componentsSupportTLS ip) of
        Yes ->
            True
        _ ->
            False
