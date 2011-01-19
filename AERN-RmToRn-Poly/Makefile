cbits/EvalExport_stub.h: src/Numeric/AERN/RmToRn/Basis/Polynomial/Internal/EvalExport.hs src/Numeric/AERN/RmToRn/Basis/Polynomial/Internal/Basics.hsc
	-rm cbits/EvalExport_stub.h
	hsc2hs -I cbits src/Numeric/AERN/RmToRn/Basis/Polynomial/Internal/Basics.hsc
	ghc --make -isrc -odir .pre -hidir .pre -stubdir .pre Numeric.AERN.RmToRn.Basis.Polynomial.Internal.EvalExport
	rm src/Numeric/AERN/RmToRn/Basis/Polynomial/Internal/Basics.hs
	(cd cbits; ln -s ../.pre/Numeric/AERN/RmToRn/Basis/Polynomial/Internal/EvalExport_stub.h .)
