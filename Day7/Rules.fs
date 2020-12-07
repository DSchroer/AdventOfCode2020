module Day7.Rules

type Rule(name: string, contain: Map<string, int>) =
    member this.Name = name
    member this.Contain = contain

let ruleMap (rules: Rule []) =
        rules
        |> Array.map (fun r -> (r.Name, r))
        |> Map<string, Rule>

let canHold (rules: Rule []) (target: string) =
    let ruleMap = (ruleMap rules)

    let rec canHold storage target =
        let rule = ruleMap.[storage]
        if rule.Contain.ContainsKey(target) then
            true
        else
            rule.Contain
            |> Map.toSeq
            |> Seq.map (fun (name, _) -> canHold name target)
            |> Seq.fold (||) false

    rules
    |> Array.filter (fun r -> canHold r.Name target)

let holdsHowMany (rules: Rule []) (target: string) =
    let ruleMap = (ruleMap rules)

    let rec holdAmount target =
        let rule = ruleMap.[target]

        let amount =
            rule.Contain
            |> Map.toSeq
            |> Seq.map (fun (name, amount) -> amount + (amount * (holdAmount name)))
            |> Seq.sum

        amount

    holdAmount target
