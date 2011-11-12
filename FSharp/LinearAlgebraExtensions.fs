namespace MathNet.Numerics.LinearAlgebra.Double

open MathNet.Numerics.LinearAlgebra.Double
open MathNet.Numerics.LinearAlgebra.Generic

[<AutoOpen>]
module VectorOperators = 

    let (.^) (v: #Vector<float>) exp = 
        Vector.map (fun x -> x ** exp) v

    let (./) (v: #Vector<float>) divider = 
        v.PointwiseDivide(divider)

    let (.*) (v: #Vector<float>) divider = 
        v.PointwiseMultiply(divider)   

module Vector = 
    let inline length (v: #Vector<float>) = v.Count
    let inline Σ (v: #Vector<float>) = v.Sum()

module Matrix =
    let inline rowCount (m: #Matrix<float>) = m.RowCount
    let inline columnCount (m: #Matrix<float>) = m.ColumnCount
    
    let inline Σrows f A = Matrix.sumRowsBy f A
    let inline Σcols f A = Matrix.sumColsBy f A