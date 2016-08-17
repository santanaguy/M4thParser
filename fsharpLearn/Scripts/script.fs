module scripts = 
    type OrderLine = {
        ProductCode: string
        LineQty: int
        Price: float
        LineTotal: float
        }

    type OrderLineStats = {
        Qty: int 
        Total:float
        }

    let add a b = {Qty = a.Qty + b.Qty; Total = a.Total + b.Total }

    let toOrderLine ol = {Qty=ol.LineQty; Total=ol.LineTotal}
    let orderLines = [{ProductCode= "P1"; LineQty= 1; Price= 1.0; LineTotal= 10.0}; 
                      {ProductCode= "P2"; LineQty= 5; Price= 55.3; LineTotal= 11.0}]

    orderLines
    |> List.map toOrderLine
    |> List.reduce add


        