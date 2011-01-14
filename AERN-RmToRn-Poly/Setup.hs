import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.Utils (rawSystemExit)

main = defaultMainWithHooks simpleUserHooks
    { preConf = \a b -> makeLib a b >> preConf simpleUserHooks a b }

makeLib :: Args -> ConfigFlags -> IO ()
makeLib _ flags =
    rawSystemExit (fromFlag $ configVerbosity flags) "make"
        ["cbits/EvalExport_stub.h"]
        