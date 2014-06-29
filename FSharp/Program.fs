
type Stock = string
type IndexFund = string
type MutualFund = string
type Bond = string

type StockType = 
     | Stock 
     | IndexFund 
     | MutualFund 
     | Bond 

type Security = { Type:StockType; 
                  CompanyName:string;
                  Ticker:string; 
                  Value:float 
                  NumberOfShares:float }

type Portfolio = Security list

type Account = {Investments:Portfolio; Number:int }

type Person = { FirstName:string; 
                LastName:string;
                Account: Account; }

let google = {Type = Stock; CompanyName = "Google Inc."; Ticker = "GOOG"; Value = 846.90; NumberOfShares = 1000.00}
let microsoft = {Type = Stock; CompanyName = "Microsoft"; Ticker = "MSFT"; Value = 35.57 ; NumberOfShares = 150.00}
let apple = {Type = Stock; CompanyName = "Apple"; Ticker = "AAPL"; Value = 529.88; NumberOfShares = 55.00;}
let facebook = {Type = Stock; CompanyName = "Facebook"; Ticker = "FB"; Value = 50.23; NumberOfShares = 600.50}
let equityIndex = {Type = IndexFund; CompanyName = "TRowePrice"; Ticker = "PREIX"; Value = 47.80; NumberOfShares = 30.00 }
let newEra = {Type = MutualFund; CompanyName = "TRowePrice"; Ticker = "PRNEX"; Value = 47.40; NumberOfShares = 30.00 }

let addStockToPortfolio portfolio security = security::portfolio
                                   
let willsPortfolio = [google; microsoft]

let willsaccount = { Investments = willsPortfolio; 
                     Number = 123456789 } 

let will = { FirstName = "William"; LastName = "Lane"; Account = willsaccount; }

addStockToPortfolio will.Account.Investments apple |> ignore
addStockToPortfolio will.Account.Investments facebook |> ignore
addStockToPortfolio will.Account.Investments equityIndex |> ignore
addStockToPortfolio will.Account.Investments newEra |> ignore

let sumOfSecurities list = 
    let rec sum list acc = 
        match list with
        |[] -> acc
        |head::tail -> sum tail (acc + head.Value * head.NumberOfShares)
    sum list 0.00

let listOfStocksOwned list = 
    let rec buildlist list acc = 
        match list with
        |[] -> acc
        |head::tail -> buildlist tail (acc + head.CompanyName + "\n")
    buildlist list ""

let listOfWillMoneyStocks = List.fold (fun acc sec -> acc + sec.CompanyName + "\n") "" willsaccount.Investments




//printfn "%b" (System.Object.ReferenceEquals(willsaccount, willsbetteraccount))

//let willsmutualfunds = List.map (fun x -> x.Name) List.filter (fun x -> x.Type = MutualFund) willbetter.Account.Investments 

let willsmutualfunds =  will.Account.Investments 
                        |> List.filter (fun x -> x.Type = MutualFund) 
                        |> List.fold (fun acc sec -> acc + sec.CompanyName + "" + "\n") ""

let willsfilteredlistByType stype list = 
        let predicate = 
            match stype with
            | Stock -> (fun x -> x.Type = Stock)
            | IndexFund -> (fun x -> x.Type = IndexFund)
            | MutualFund -> (fun x -> x.Type = MutualFund)
            | Bond -> (fun x -> x.Type = Bond) 
        list |> List.filter predicate |> List.fold (fun acc sec -> acc + sec.CompanyName + "\n") ""

let willsStocks = willsfilteredlistByType Stock will.Account.Investments

willsStocks |> printfn "Will' stock portfolio includes \n%s" |> ignore       



let ints = [ 1 .. 10 ]
printfn "Before squares and evens are called"

let squares = List.map(fun x -> x * x) ints
let evens = List.filter(fun x -> x % 2 = 0) ints

printfn "After squares and evens are called"
ints |> List.iter (fun x -> printf "%i " x)



    
     
       

               
