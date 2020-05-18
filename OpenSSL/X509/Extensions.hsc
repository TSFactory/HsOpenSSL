{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module OpenSSL.X509.Extensions
  (
    X509_EXTENSION (..)
  , v3ExtConf
  )
  where

#include "HsOpenSSL.h"
import Foreign.C.String
#if MIN_VERSION_base(4,4,0)
import Foreign.ForeignPtr.Unsafe as Unsafe
#else
import Foreign.ForeignPtr as Unsafe
#endif
import Foreign.Ptr
import Foreign.C
import OpenSSL.Utils
import OpenSSL.X509

data X509_EXTENSION =
    X509_EXTENSION CInt
  | NID_basic_constraints
  | NID_key_usage
  | NID_ext_key_usage
  | NID_subject_key_identifier
  | NID_authority_key_identifier
  | NID_private_key_usage_period
  | NID_subject_alt_name
  | NID_issuer_alt_name
  | NID_info_access
  | NID_sinfo_access
  | NID_name_constraints
  | NID_certificate_policies
  | NID_policy_mappings
  | NID_policy_constraints
  | NID_inhibit_any_policy
  deriving (Show, Eq)

foreign import ccall unsafe "HsOpenSSL_X509V3_EXT_conf"
        _V3_ext_conf :: Ptr X509_ -> CInt -> CString -> IO CInt

-- PKIX Certificate Extensions
foreign import capi "openssl/objects.h value NID_basic_constraints"
        c_NID_basic_constraints :: CInt
foreign import capi "openssl/objects.h value NID_key_usage"
        c_NID_key_usage :: CInt
foreign import capi "openssl/objects.h value NID_ext_key_usage"
        c_NID_ext_key_usage :: CInt

foreign import capi "openssl/objects.h value NID_subject_key_identifier"
        c_NID_subject_key_identifier :: CInt
foreign import capi "openssl/objects.h value NID_authority_key_identifier"
        c_NID_authority_key_identifier :: CInt

foreign import capi "openssl/objects.h value NID_private_key_usage_period"
        c_NID_private_key_usage_period :: CInt

foreign import capi "openssl/objects.h value NID_subject_alt_name"
        c_NID_subject_alt_name :: CInt
foreign import capi "openssl/objects.h value NID_issuer_alt_name"
        c_NID_issuer_alt_name :: CInt

foreign import capi "openssl/objects.h value NID_info_access"
        c_NID_info_access :: CInt
foreign import capi "openssl/objects.h value NID_sinfo_access"
        c_NID_sinfo_access :: CInt

foreign import capi "openssl/objects.h value NID_name_constraints"
        c_NID_name_constraints :: CInt

foreign import capi "openssl/objects.h value NID_certificate_policies"
        c_NID_certificate_policies :: CInt
foreign import capi "openssl/objects.h value NID_policy_mappings"
        c_NID_policy_mappings :: CInt
foreign import capi "openssl/objects.h value NID_policy_constraints"
        c_NID_policy_constraints :: CInt
foreign import capi "openssl/objects.h value NID_inhibit_any_policy"
        c_NID_inhibit_any_policy :: CInt

v3ExtConf :: X509 -> X509_EXTENSION -> String -> IO ()
v3ExtConf x509 ext val
  = withCString val $ \ valPtr ->
    withX509Ptr x509 $ \ x509Ptr ->
    _V3_ext_conf x509Ptr (x509ExtToNID ext) valPtr
        >>= failIf (/= 1)
        >>  return ()

x509ExtToNID :: X509_EXTENSION -> CInt
x509ExtToNID x = case x of
  X509_EXTENSION i -> i
  NID_basic_constraints -> c_NID_basic_constraints
  NID_key_usage -> c_NID_key_usage
  NID_ext_key_usage -> c_NID_ext_key_usage
  NID_subject_key_identifier -> c_NID_subject_key_identifier
  NID_authority_key_identifier -> c_NID_authority_key_identifier
  NID_private_key_usage_period -> c_NID_private_key_usage_period
  NID_subject_alt_name -> c_NID_subject_alt_name
  NID_issuer_alt_name -> c_NID_issuer_alt_name
  NID_info_access -> c_NID_info_access
  NID_sinfo_access -> c_NID_sinfo_access
  NID_name_constraints -> c_NID_name_constraints
  NID_certificate_policies -> c_NID_certificate_policies
  NID_policy_mappings -> c_NID_policy_mappings
  NID_policy_constraints -> c_NID_policy_constraints
  NID_inhibit_any_policy -> c_NID_inhibit_any_policy
