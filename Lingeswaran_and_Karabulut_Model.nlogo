globals [
  years
  months
]

;;Introduction of the breeds

;;This first batch is the agents of the model
breed [municipalities municipality]
breed [households household]
breed [recycling-companies recycling-company]

;;This second batch is about the visualisation. Normally these are not identified as the agents.
breed [campaigns campaign]
breed [investments investment]

;;Contracts are the visible link is the model, while the investments are introduced as links again only for visualisation purposes.
undirected-link-breed[contracts contract]
undirected-link-breed[investment-links investment-link]


;;identification of the attributes of breeds identified above.
municipalities-own[
  ID
  number-of-households
  budget-mun
  target-recycle-waste
  monthly-waste-accumulated
  monthly-plastic-waste-accumulated
  monthly-recyclable-plastic-waste-accumulated
  type-of-infrastructure
  costs
  estimated-waste-amount
  contract-waste-amount
  frequency-of-survey
  number-of-perception-campaigns
  number-of-knowledge-campaigns
  weight-price
  weight-fine
  weight-target
  muni-available-to-contract?
  existing-contracts
  current-recycling-performance
  budget-per-household
  cost-perc-campaign
  cost-know-campaign
]

households-own[
  which-municipality
  household-type
  factor-of-perception
  factor-of-knowledge
  sensitivity-to-campaigns
  sensitivity-to-infrastructure
  monthly-waste-produced
  percentage-plastic
  amount-plastic
  percentage-recyclable-plastic
  amount-recyclable-plastic
]

recycling-companies-own[
  budget-rc
  technological-efficiency
  maximum-capacity
  current-capacity-available
  recycling-cost
  collection-cost
  revenue-recycled-plastic
  revenue-contract
  profit
  base-expectation-waste-to-recyclable
  contract-fixed-waste
  company-available-to-contract?
]

contracts-own[
  ;;request part
  estimated-waste-supply
  expected-recycle-target
  provided-infrastructure
  desired-duration
  distance-between-partners

  ;;bidding part
  capable-waste-amount
  capable-recycling-target
  offered-unit-price
  offered-unit-fine

  ;;selection part
  normalized-unit-fine
  normalized-price
  normalized-capable-recycling-target
  attractiveness-for-muni
  best-for-muni?

  ;;agreement part
  duration
  fine
  price
  promised-waste-amount
  promised-recycling-percentage
  waste-facility-fraction
  waste-municipality-fraction

  ;;operational part
  age
]

investments-own[
  current-implementation-time
]


;;forming the model structure at the beginning.
;; setup function initializes the agents
;;also it updates the perception of the households with respect to the infrastructure they have in their municipality.
to setup
  clear-all
  reset-ticks

  set months 0
  set years 0

  initialize-municipalities
  initialize-households
  initialize-recycling-companies

  update-perception-wrt-infrastructure

end


;;go function is repeated each time, and it is the main operational function of the model.
to go

  produce-waste
  conduct-survey
  pay-tax
  disappear-star
  calculate-monthly-accumulated-waste

  ;;contract related functions
  if (remainder months contract-duration = 0) or ( months = 0 )[

    ;;make the municipalities estimate how much waste they will have during the upcoming years.
    estimate-municipal-waste

    ;;the existing contracts are terminated
    demolish-contracts

    ;;contract formation process takes place
    ;;first municipalities request, then companies bid, then municipalities choose the best bid among the normalized bids, and lastly the companies choose their business partner.
    ;;this process continues until all the municipal waste is covered, or the capacity of the companies is completely allocated to the municipalities.
    while [ ( any? municipalities with [muni-available-to-contract? = True] ) and ( any? recycling-companies with [company-available-to-contract? = True] ) ][
      muni-request
      bid-to-request
      normalize-the-bids
      set-the-best-bid
      choose-partner-municipality
    ]

    ;;identifies how much waste is allocated to whom with respect to the total waste produced in a municipality.
    fraction-of-waste-to-contract
  ]

  ;;the monthly operations are coded in this function, basically the collection of waste, recycling process, selling plastic, getting money from municipalities etc.
  monthly-operation

  ;;increasing the contract age by 1
  update-contract-age

  ;;investment decisions are made by the companies and the existing decisions are implemented respectively
  decide-investment
  update-investment
  finish-implementation-investment

  ;;the municipalities' performance is measured for data collection purposes
  update-recycling-performance

  ;;month and year info is updated
  set months months + 1
  if ( remainder months 12 = 0 ) and ( months > 0 ) [set years years + 1]

  ;;municipality budget per household is updated for data collection purposes
  ask municipalities[set budget-per-household budget-mun / number-of-households]

  ;;model termination condition
  ifelse years <= 19
  [ tick ]
  [ stop ]
end

to initialize-municipalities
  create-municipalities number-municipalities [set shape "house two story"]

  ask municipalities[
    ;;identify the number of households placed within a municipality's borders
    set number-of-households (random (max-number-households - min-number-households) + min-number-households) * 100

    ;;place the municipalities, and arrange the size and colour
    set color one-of remove 33 base-colors
    set size (number-of-households / (min-number-households * 100) )
    setxy random-xcor random-ycor

    ;;identify the municipality
    set ID who

    ;;To save computational power, the hatched households is 1 100th of the total number of households
    ;;Basically, we just assumed that there are 100 identical households with respect to the attributes we identify.
    hatch-households number-of-households / 100 [set which-municipality [ID] of myself]

    ;;set the budget of municipality as a random value between min & max
    set budget-mun (random(max-budget-municipalities - min-budget-municipalities) + min-budget-municipalities) * 1000

    ;;set the municipal recycling target based on the governmental target. Some takes it seriously, some does not.
    set target-recycle-waste max(list 0 (min (list 100 ceiling ( random-normal recycling-target 10 ))))

    ;;initialize the monthly accumulated waste
    set monthly-waste-accumulated 0
    set monthly-plastic-waste-accumulated 0
    set monthly-recyclable-plastic-waste-accumulated 0

    ;;set the condition about centralized or decentralized infrastructure
    set type-of-infrastructure one-of ["decentralized" "centralized"]

    ;;initialize the costs
    set costs 0

    ;;identify the frequency of a municipality to conduct a campaign.
    set frequency-of-survey ((random 10) + 3)

    ;;initialize the number of campaigns conducted
    set number-of-perception-campaigns 0
    set number-of-knowledge-campaigns 0

    ;;set the weight with respect to the chosen slider values
    set weight-price weight-of-price / ( weight-of-price + weight-of-fine + weight-of-recycling-target )
    set weight-fine weight-of-fine / ( weight-of-price + weight-of-fine + weight-of-recycling-target )
    set weight-target weight-of-recycling-target / ( weight-of-price + weight-of-fine + weight-of-recycling-target )

    ;;initialize the availability to setup contracts
    set muni-available-to-contract? True

    ;;initialize the empty set of existing links
    set existing-contracts []

    ;; initialize the cost of conducting campaigns
    set cost-perc-campaign unit-cost-perc-campaign * number-of-households
    set cost-know-campaign unit-cost-know-campaign * number-of-households
  ]
end

to initialize-households
  ;;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ;;!!!!!!EACH HOUSEHOLD IS A REPRESENTATIVE OF 100 HOUSEHOLDS!!!!!!
  ;;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ask households[
    hide-turtle

    ;;identify the type of a household with respect to the rates determined by the interface.
    let household-type-list (n-values single-rate ["single"])
    set household-type-list (sentence household-type-list  (n-values couple-rate ["couple"]))
    set household-type-list (sentence household-type-list  (n-values family-rate ["family"]))
    set household-type-list (sentence household-type-list  (n-values old-rate ["old"]))
    set household-type one-of household-type-list

    ;;identify the factors of perception and knowledge
    if household-type = "single" [set factor-of-perception random-normal single-perception-coef 0.05]
    if household-type = "couple" [set factor-of-perception random-normal couple-perception-coef 0.05]
    if household-type = "family" [set factor-of-perception random-normal family-perception-coef 0.05]
    if household-type = "old" [set factor-of-perception random-normal old-perception-coef 0.05]

    if household-type = "single" [set factor-of-knowledge random-normal single-knowledge-coef 0.05]
    if household-type = "couple" [set factor-of-knowledge random-normal couple-knowledge-coef 0.05]
    if household-type = "family" [set factor-of-knowledge random-normal family-knowledge-coef 0.05]
    if household-type = "old" [set factor-of-knowledge random-normal old-knowledge-coef 0.05]

    ;;identify the sensitivity to campaigns and infrastructure itself
    if household-type = "single" [set sensitivity-to-campaigns random-normal single-sensitivity-campaign 0.05]
    if household-type = "couple" [set sensitivity-to-campaigns random-normal couple-sensitivity-campaign 0.05]
    if household-type = "family" [set sensitivity-to-campaigns random-normal family-sensitivity-campaign 0.05]
    if household-type = "old" [set sensitivity-to-campaigns random-normal old-sensitivity-campaign 0.05]

    if household-type = "single" [set sensitivity-to-infrastructure random-normal single-sensitivity-infrastructure 0.05]
    if household-type = "couple" [set sensitivity-to-infrastructure random-normal couple-sensitivity-infrastructure 0.05]
    if household-type = "family" [set sensitivity-to-infrastructure random-normal family-sensitivity-infrastructure 0.05]
    if household-type = "old" [set sensitivity-to-infrastructure random-normal old-sensitivity-infrastructure 0.05]

    ;;initialize the waste produced
    set monthly-waste-produced 0

    ;;initialize the plastic percentage of the waste produced
    set percentage-plastic min(list 1 (base-plastic-rate * factor-of-perception))
    set amount-plastic monthly-waste-produced * percentage-plastic

    ;;initialize the recyclable plastic percentage of the waste produced
    set percentage-recyclable-plastic min(list 1 (base-recyclable-plastic-rate * factor-of-knowledge))
    set amount-recyclable-plastic amount-plastic * percentage-recyclable-plastic
  ]
end


to initialize-recycling-companies

  ;;place the recycling companies, and arrange their size
  create-recycling-companies number-recycling-companies [set shape "factory" set size 2 setxy random-xcor random-ycor]

  ask recycling-companies[
    ;; set the color of the factories
    set color 33 - 2 + random 5

    ;;identify the technological capabilities of the facilities
    set technological-efficiency max(list 0 (min (list 1 ( random-normal mean-efficiency-level 0.1 ))))

    ;;identify the monthly processing capacity of the facilities
    set maximum-capacity max(list 1000000 (min (list 8000000 ceiling ( random-normal mean-capacity-level 1000000 ))))

    ;;identify the amount of capacity used
    set current-capacity-available maximum-capacity

    ;;identify the cost of recycling processes
    set recycling-cost 0

    ;;identify the cost of collection processes
    set collection-cost 0

    ;;identify the revenue from contracts
    set revenue-contract 0

    ;;identify the total revenue from sold recycled plastic
    set revenue-recycled-plastic 0

    ;;identify the profit and the budget of the companies
    set profit ((revenue-contract + revenue-recycled-plastic) - (recycling-cost + collection-cost))
    set budget-rc 0

    ;;identify the base expectations for the ratio of waste to the recyclable plastic amount (for the unknown municipalities)
    set base-expectation-waste-to-recyclable max(list 0 (min (list 1 ( random-normal mean-waste-to-recyclable-ratio-expected 0.1 ))))

    ;;initialize the availability to setup contracts for companies
    set company-available-to-contract? True

  ]
end




;;the need for a campaign is detected by the help of surveys
;;each campaign increases the perception or knowledge with respect to the sensitivity of a particular household.
;;the focus is on the knowledge campaigns, there is no restriction, but for the perception campaigns, there has to be
;;knowledge campaign conducted already, so that the number of perception campaigns is smaller than the number of knowledge
;;campaigns.

to conduct-survey
  ask municipalities[
    if ((remainder months frequency-of-survey) = 0) and (months > 0) [
      if ( mean [percentage-plastic * percentage-recyclable-plastic] of n-of ( (number-of-households / 100) * 0.15 ) households with [which-municipality = [ID] of myself] ) * 100 < target-recycle-waste [



        ;; to conduct knowledge related campaigns
        if number-of-perception-campaigns >= number-of-knowledge-campaigns[
          if cost-know-campaign <= budget-mun [

            let temp-muni-list []

            create-knowledge-book

            set budget-mun budget-mun - cost-know-campaign
            set number-of-knowledge-campaigns number-of-knowledge-campaigns + 1

            set temp-muni-list lput who temp-muni-list
            ask households [
              if (member? which-municipality temp-muni-list) [
                set factor-of-knowledge (factor-of-knowledge * (1 + (sensitivity-to-campaigns / 10)))
                set percentage-recyclable-plastic min(list 1 (base-recyclable-plastic-rate * factor-of-knowledge))]
            ]
          ]
        ]

        ;; to conduct perception related campaigns
        if number-of-perception-campaigns < number-of-knowledge-campaigns[
          if cost-perc-campaign <= budget-mun [

            let temp-muni-list []

            create-perception-star

            set budget-mun budget-mun - cost-perc-campaign
            set number-of-perception-campaigns number-of-perception-campaigns + 1

            set temp-muni-list lput who temp-muni-list
            ask households [
              if (member? which-municipality temp-muni-list) [
                set factor-of-perception (factor-of-perception * (1 + (0.1 * sensitivity-to-campaigns)))
                set percentage-plastic min(list 1 (base-plastic-rate * factor-of-perception))]
            ]
          ]
        ]
      ]
    ]
  ]
end

;;stars are used to visualize the perception campaigns.
to disappear-star
  ask campaigns[
    ifelse (size < 0.1)
    [ die ]
    [
      set heading (atan (random-float 0.5 - 0.25) 1)
      jump 0.3
      set size size - 0.1
      set color color - 0.25
    ]
  ]
end

to create-perception-star
  hatch-campaigns 1 [
    set color 45
    set shape "star"
    set size 2.5
    set label ""
  ]
end

;;books are used to visualize the knowledge campaigns
to create-knowledge-book
  hatch-campaigns 1 [
    set color 65
    set shape "book"
    set size 1.5
    set label ""
  ]
end

;;It is assumed that the effect of the infrastructure on the perception is as follows:
;; if decentralized, perception stays the same, if centralized, perception is 80% of the expected base perception.
;; Then we include the effect of sensitivity
to update-perception-wrt-infrastructure
  ask households[
    ifelse ([type-of-infrastructure] of municipalities with [ID = [which-municipality] of myself]) = "decentralized"
    [set factor-of-perception factor-of-perception]
    [if household-type = "single" [set factor-of-perception factor-of-perception * (1 - (0.20 * single-sensitivity-infrastructure))]
     if household-type = "couple" [set factor-of-perception factor-of-perception * (1 - (0.20 * couple-sensitivity-infrastructure))]
     if household-type = "family" [set factor-of-perception factor-of-perception * (1 - (0.20 * family-sensitivity-infrastructure))]
     if household-type = "old" [set factor-of-perception factor-of-perception * (1 - (0.20 * old-sensitivity-infrastructure))]
    ]
  ]
end

;;monthly waste production function, it is provided in the problem description.
;; 100 in the equation is for 100 households.
to produce-waste
  ask households[
    set monthly-waste-produced (40 - 0.04 * months - exp(-0.01 * months) * sin (0.3 * months)) * 100
    set amount-plastic monthly-waste-produced * percentage-plastic
    set amount-recyclable-plastic amount-plastic * percentage-recyclable-plastic
  ]
end

;;monthly tax payment function
;; 100 in the equation is for 100 households.
to pay-tax
  ask municipalities[
    set budget-mun budget-mun + waste-tax-monthly * (count households with [which-municipality = [ID] of myself] * 100 )
  ]
end

;;if the municipality is informed about the waste production habits, then it can estimate the average value for
;;contract duration so that the fine can be decreased, if it is unknown, then the current value is taken as the promised value.
to estimate-municipal-waste
  ask municipalities[
    ifelse (waste-production-knowledge = TRUE)
      ;;estimated waste amount is also monthly
      [set estimated-waste-amount round ( (((40 - 0.04 * months - exp(-0.01 * months) * sin (0.3 * months)) * number-of-households) + ((40 - 0.04 * (months + contract-duration) - exp(-0.01 * (months + contract-duration)) * sin (0.3 * (months + contract-duration))) * number-of-households)) / 2 )]
      [set estimated-waste-amount round (((40 - 0.04 * months - exp(-0.01 * months) * sin (0.3 * months)) * number-of-households) ) ]

    set contract-waste-amount estimated-waste-amount
  ]
end

;;setting the monthly accumulated amounts for a municipality.
to calculate-monthly-accumulated-waste
  ask municipalities[
    set monthly-waste-accumulated sum([monthly-waste-produced] of households with [which-municipality = [ID] of myself])
    set monthly-plastic-waste-accumulated sum([amount-plastic] of households with [which-municipality = [ID] of myself])
    set monthly-recyclable-plastic-waste-accumulated sum([amount-recyclable-plastic] of households with [which-municipality = [ID] of myself])
  ]
end

;;municipalities request offers from companies.
to muni-request
  ask municipalities[

    if muni-available-to-contract? [
      let available-companies recycling-companies with [company-available-to-contract? = True]

      create-contracts-with available-companies [
        set estimated-waste-supply [estimated-waste-amount] of myself
        set expected-recycle-target [target-recycle-waste] of myself
        set provided-infrastructure [type-of-infrastructure] of myself
        set desired-duration contract-duration

        ;; 1 unit of distance is assumed to be 10km
        set distance-between-partners link-length * 10
      ]
    ]
  ]
end

;;recycling companies bid after they receive the request from municipalities.
;;we assume the municipalities do not change the offered amount, instead the tendency is to have the least number of
;;business partners to have a leaner business. Therefore, every actor has the tendency to go for all, if possible.
;;also, the municipalities do not counter-offer the first offers from the companies, but choose the most suitable one.
to bid-to-request
  ask recycling-companies[

    if company-available-to-contract? [
      ask my-contracts[
        if color != 55 [
          ;;calculate the amount that you can process as the facility
          ifelse [current-capacity-available] of myself > estimated-waste-supply
            [set capable-waste-amount estimated-waste-supply]
            [set capable-waste-amount [current-capacity-available] of myself]

          ;;set the recycling target that you can achieve
          set capable-recycling-target [technological-efficiency] of myself

          ;;calculate the offered price
          ifelse provided-infrastructure = "centralized"
            [set offered-unit-price ( unit-recycling-cost ) + ( unit-distance-cost * distance-between-partners )]
            [set offered-unit-price ( unit-recycling-cost ) + ( unit-distance-cost * distance-between-partners * unrest-due-to-decentralized )]

          ;;calculate the fine (assumes there is an amount of recyclable plastic in 1 kg of waste with respect to the base-expectation of the company
          ;;the company will be able to process that recyclable amount with respect to its efficiency. The extracted amount will be sold in the market with the determined market price.
          set offered-unit-fine ( [base-expectation-waste-to-recyclable] of myself * [technological-efficiency] of myself * recycled-plastic-market-price)
        ]
      ]
    ]
  ]
end

;;normalization of the bids by the municipalities
;;so that the municipalities can rank the offers with respect to the importance they give to "fine", "price", and "target".
to normalize-the-bids
  ask contracts[

    if color != 55 [

      let max-unit-fine max [offered-unit-fine] of contracts
      let min-unit-fine min [offered-unit-fine] of contracts
      set normalized-unit-fine ( offered-unit-fine - min-unit-fine ) / ( max-unit-fine - min-unit-fine )

      let max-price max [offered-unit-price] of contracts
      let min-price min [offered-unit-price] of contracts
      set normalized-price ( offered-unit-price - min-price ) / ( max-price - min-price )

      let max-target max [capable-recycling-target] of contracts
      let min-target min [capable-recycling-target] of contracts
      ifelse max-target != min-target
        [set normalized-capable-recycling-target ( capable-recycling-target - min-target ) / ( max-target - min-target )]
        [set normalized-capable-recycling-target capable-recycling-target]
    ]
  ]
end

;;the municipality selects the most attractive offer among the others.
to set-the-best-bid
  ask municipalities[

    if muni-available-to-contract? [
      ask my-contracts[
        if color != 55 [
          set attractiveness-for-muni [weight-target] of myself * normalized-capable-recycling-target - [weight-fine] of myself * normalized-unit-fine - [weight-price] of myself * normalized-price
        ]
      ]

      let temp-yellow-list my-contracts with [color != 55]

      if any? temp-yellow-list[
        ask max-one-of temp-yellow-list [attractiveness-for-muni][
          if color != 55 [
            set color 45
          ]
        ]
      ]

      ask my-contracts[
        if color != 55 [
          if color != 45 [die]
        ]
      ]
    ]
  ]
end

;;now the company chooses whom it wants to work with
to choose-partner-municipality

  ;; this part of the function is about the company side of the contract formation
  ask recycling-companies[
    if company-available-to-contract?[
      if any? my-contracts with [color != 55][

        let non-green-contracts my-contracts with [color != 55]

        ask max-one-of non-green-contracts [capable-waste-amount] [
          set color 55
          set promised-waste-amount capable-waste-amount
          set price offered-unit-price * promised-waste-amount
        ]

        ask my-contracts[
          if color != 55 [die]
        ]

        set current-capacity-available maximum-capacity - sum [capable-waste-amount] of my-contracts with [color = 55]

        if current-capacity-available < 1 [
          set company-available-to-contract? False
        ]
      ]
    ]
  ]

  ;;this part of the function is about the municipality side of the contract formation.
  ask municipalities[
    if muni-available-to-contract?[
      if any? my-contracts [

        let temp-new-contract []

        ask my-contracts [
          if not member? self [existing-contracts] of myself[
            set temp-new-contract lput self temp-new-contract
          ]
        ]

        if not empty? temp-new-contract [
          set estimated-waste-amount estimated-waste-amount - [capable-waste-amount] of item 0 temp-new-contract
          set existing-contracts sentence existing-contracts temp-new-contract
        ]
      ]
    ]

    if estimated-waste-amount < 1 [
      set muni-available-to-contract? False
    ]
  ]
end

;;operational function to increase the contract age in each year.
to update-contract-age
  ask contracts[
    set age age + 1
  ]
end

;;this is used for the terminated contracts.
;;it first identifies the contracts which need to be terminated, then it sets the affiliated municipalities available for further contract formation processes.
;;then the affiliated companies are set available, too. Lastly, this particular contract is killed, so that the related company-municipality pair can start looking for
;;new contracts.
to demolish-contracts
  if months != 0[
    ask municipalities with [any? link-neighbors][
      let flag-termination false

      ask my-contracts[
        if age = contract-duration [
          set flag-termination true
        ]
      ]

      set muni-available-to-contract? flag-termination
    ]

    ask recycling-companies[
      let flag-termination false
      let capacity-to-release 0

      ask my-contracts[
        if age = contract-duration [
          set flag-termination true
          set capacity-to-release capacity-to-release + capable-waste-amount
        ]
      ]

      if flag-termination = true [
        set company-available-to-contract? true
        set current-capacity-available current-capacity-available + capacity-to-release
      ]
    ]

    ask contracts[
      if age = contract-duration [
        die
      ]
    ]
  ]
end

;;this function is used especially to calculate the fine later on. Each municipality pays the fine by distributing it over the companies
;;with respect to the fraction of contracted waste amount to the overall waste produced.
to fraction-of-waste-to-contract
  ask recycling-companies[
    set contract-fixed-waste sum [promised-waste-amount] of my-contracts
    ask my-contracts[
    set waste-facility-fraction promised-waste-amount / [contract-fixed-waste] of myself
    ]
  ]
  ask municipalities[
    ask my-contracts[
    set waste-municipality-fraction promised-waste-amount / [contract-waste-amount] of myself
    ]
  ]
end


;;the monthly operations like waste collection, recycling, selling plastic, getting revenue, paying fine etc.
to monthly-operation
  ;;monthly operations related to municipalities
  ask municipalities[
    ask my-contracts[
      set fine ((promised-waste-amount - [monthly-waste-accumulated] of myself * waste-municipality-fraction) * offered-unit-fine)
      ]
    set costs sum [price] of my-contracts
    set budget-mun budget-mun - sum [fine] of my-contracts - costs
  ]

  ;;monthly operations related to companies
  ask recycling-companies[
    let outflow-waste sum [monthly-waste-accumulated] of link-neighbors with [not (color = 45 or color = 25 or color = 15) ]
    let outflow-recyclable-plastic sum [monthly-recyclable-plastic-waste-accumulated] of link-neighbors with [not (color = 45 or color = 25 or color = 15) ]
    let collection-distance-between-partners sum [distance-between-partners] of my-contracts
    set revenue-contract sum [price] of my-contracts
    set revenue-recycled-plastic outflow-recyclable-plastic * technological-efficiency * recycled-plastic-market-price
    set recycling-cost unit-recycling-cost * outflow-waste
    set collection-cost unit-distance-cost * collection-distance-between-partners * 10 * 2

    set profit ((revenue-contract + revenue-recycled-plastic) - recycling-cost - collection-cost)
    set budget-rc budget-rc + profit
  ]
end

;;this function is used for data collection purposes. We track down the performance of the municipalities by conducting artificial surveys.
to update-recycling-performance
  ask municipalities[
    set current-recycling-performance ( ( mean [percentage-plastic * percentage-recyclable-plastic] of n-of ( (number-of-households / 100) * 0.15 ) households with [which-municipality = [ID] of myself] ) * 100 )
  ]
end


;;the companies decide investment when they are not able to get any contract with any municipality.
;;the companies check the market averages, and respectively identifies the aspects which are open for improvement.
;;then they invest in those aspects.
;;the investment is visualized by using a "lightning bolt". The lightnin bolt remains during the implementation phase.
;;when the implementation is over, the lightning bolt is removed.
to decide-investment
  ask recycling-companies[
    let loser-flag false
    let winning-contracts my-contracts with [color = 55]

    let lack-of-tech-flag false
    let lack-of-capacity-flag false

    if not any? winning-contracts [set loser-flag true]

    if not any? ( my-investment-links with [color = 45 or color = 25 or color = 15] ) [
      if loser-flag = true[

        if technological-efficiency < mean [technological-efficiency] of recycling-companies [set lack-of-tech-flag true]
        if maximum-capacity < mean [maximum-capacity] of recycling-companies [set lack-of-capacity-flag true]

        if lack-of-tech-flag = true and lack-of-capacity-flag = false [
          hatch-investments 1 [
            create-investment-link-with myself [set color 45 hide-link]
            set current-implementation-time 0
            set color 45
            set shape "lightning"
            set size 2.5
            set label ""
          ]
        ]

        if lack-of-tech-flag = false and lack-of-capacity-flag = true [
          hatch-investments 1 [
            create-investment-link-with myself [set color 25 hide-link]
            set current-implementation-time 0
            set color 25
            set shape "lightning"
            set size 2.5
            set label ""
          ]
        ]

        if lack-of-tech-flag = true and lack-of-capacity-flag = true [
          hatch-investments 1 [
            create-investment-link-with myself [set color 15 hide-link]
            set current-implementation-time 0
            set color 15
            set shape "lightning"
            set size 2.5
            set label ""
          ]
        ]
      ]
    ]
  ]
end

;;increasing the investment time by 1 in each year.
to update-investment
  ask investments[
    set current-implementation-time current-implementation-time + 1
  ]
end

;;when the implementation time is over, company's efficiency level and capacity are updated
to finish-implementation-investment
  let related-company ""

  ask investments [
    ask my-investment-links[
      set related-company other-end
    ]

    if current-implementation-time = tech-implementation-time and color = 45[ ask related-company [ set technological-efficiency min(list 1 ( technological-efficiency * 1.2 ))] ]
    if current-implementation-time = tech-implementation-time and color = 25[ ask related-company [ set maximum-capacity maximum-capacity * 1.2 ] ]
    if current-implementation-time = tech-implementation-time and color = 15[ ask related-company [ set technological-efficiency min(list 1 ( technological-efficiency * 1.2 )) set maximum-capacity maximum-capacity * 1.2 ] ]

    if current-implementation-time = tech-implementation-time [die]
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
95
10
606
522
-1
-1
15.242424242424242
1
10
1
1
1
0
0
0
1
-16
16
-16
16
0
0
1
ticks
30.0

SLIDER
461
988
722
1021
tech-implementation-time
tech-implementation-time
0
36
12.0
1
1
NIL
HORIZONTAL

SLIDER
14
623
283
656
recycling-target
recycling-target
0
100
60.0
1
1
%
HORIZONTAL

SLIDER
68
1151
329
1184
unit-cost-know-campaign
unit-cost-know-campaign
1
30
9.0
1
1
NIL
HORIZONTAL

BUTTON
8
10
88
43
NIL
Setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
8
47
88
80
NIL
Go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
8
85
88
118
Go-once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
68
1187
329
1220
contract-duration
contract-duration
0
60
12.0
1
1
month
HORIZONTAL

SLIDER
285
658
554
691
number-municipalities
number-municipalities
0
20
6.0
1
1
NIL
HORIZONTAL

SLIDER
68
818
329
851
max-number-households
max-number-households
0
5000
1150.0
50
1
00 household
HORIZONTAL

SLIDER
68
854
329
887
min-number-households
min-number-households
1
2000
251.0
50
1
00 household
HORIZONTAL

SLIDER
68
890
329
923
max-budget-municipalities
max-budget-municipalities
200
1000
600.0
10
1
000€
HORIZONTAL

SLIDER
68
926
329
959
min-budget-municipalities
min-budget-municipalities
0
300
230.0
10
1
000€
HORIZONTAL

SLIDER
823
778
995
811
single-rate
single-rate
0
100
31.0
1
1
NIL
HORIZONTAL

SLIDER
823
814
995
847
couple-rate
couple-rate
0
100
22.0
1
1
NIL
HORIZONTAL

SLIDER
823
850
995
883
family-rate
family-rate
0
100
84.0
1
1
NIL
HORIZONTAL

SLIDER
823
887
995
920
old-rate
old-rate
0
100
34.0
1
1
NIL
HORIZONTAL

MONITOR
999
778
1074
823
single %
( count households with [ household-type = \"single\" ] ) / ( count households ) * 100
2
1
11

MONITOR
999
825
1074
870
couple %
( count households with [ household-type = \"couple\" ] ) / ( count households ) * 100
2
1
11

MONITOR
1076
778
1151
823
family %
( count households with [ household-type = \"family\" ] ) / ( count households ) * 100
2
1
11

MONITOR
1076
825
1150
870
old %
( count households with [ household-type = \"old\" ] ) / ( count households ) * 100
2
1
11

TEXTBOX
1085
725
1432
774
- Household parameters -
30
0.0
1

SLIDER
823
932
997
965
single-perception-coef
single-perception-coef
0
2
0.82
0.01
1
NIL
HORIZONTAL

SLIDER
823
968
997
1001
couple-perception-coef
couple-perception-coef
0
2
1.12
0.01
1
NIL
HORIZONTAL

SLIDER
823
1005
997
1038
family-perception-coef
family-perception-coef
0
2
1.3
0.01
1
NIL
HORIZONTAL

SLIDER
823
1040
997
1073
old-perception-coef
old-perception-coef
0
2
0.7
0.01
1
NIL
HORIZONTAL

SLIDER
1004
932
1177
965
single-knowledge-coef
single-knowledge-coef
0
2
1.11
0.01
1
NIL
HORIZONTAL

SLIDER
1004
968
1177
1001
couple-knowledge-coef
couple-knowledge-coef
0
2
0.8
0.01
1
NIL
HORIZONTAL

SLIDER
1004
1005
1177
1038
family-knowledge-coef
family-knowledge-coef
0
2
1.0
0.01
1
NIL
HORIZONTAL

SLIDER
1005
1040
1177
1073
old-knowledge-coef
old-knowledge-coef
0
2
0.93
0.01
1
NIL
HORIZONTAL

SLIDER
1183
932
1365
965
single-sensitivity-campaign
single-sensitivity-campaign
0
2
0.69
0.01
1
NIL
HORIZONTAL

SLIDER
1183
968
1365
1001
couple-sensitivity-campaign
couple-sensitivity-campaign
0
2
1.09
0.01
1
NIL
HORIZONTAL

SLIDER
1183
1005
1365
1038
family-sensitivity-campaign
family-sensitivity-campaign
0
2
1.28
0.01
1
NIL
HORIZONTAL

SLIDER
1183
1040
1365
1073
old-sensitivity-campaign
old-sensitivity-campaign
0
2
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
1370
932
1576
965
single-sensitivity-infrastructure
single-sensitivity-infrastructure
0
2
0.67
0.01
1
NIL
HORIZONTAL

SLIDER
1370
968
1576
1001
couple-sensitivity-infrastructure
couple-sensitivity-infrastructure
0
2
0.39
0.01
1
NIL
HORIZONTAL

SLIDER
1370
1005
1576
1038
family-sensitivity-infrastructure
family-sensitivity-infrastructure
0
2
1.04
0.01
1
NIL
HORIZONTAL

SLIDER
1370
1040
1576
1073
old-sensitivity-infrastructure
old-sensitivity-infrastructure
0
2
1.48
0.01
1
NIL
HORIZONTAL

SLIDER
823
1089
1120
1122
base-plastic-rate
base-plastic-rate
0
1
0.35
0.01
1
of total waste
HORIZONTAL

SLIDER
823
1130
1120
1163
base-recyclable-plastic-rate
base-recyclable-plastic-rate
0
1
0.77
0.01
1
of plastic waste
HORIZONTAL

SLIDER
461
778
722
811
mean-efficiency-level
mean-efficiency-level
0
1
0.61
0.01
1
NIL
HORIZONTAL

TEXTBOX
431
726
756
766
- Company Parameters -
30
0.0
1

SLIDER
461
813
722
846
mean-capacity-level
mean-capacity-level
0
5000000
2640000.0
10000
1
kg (monthly)
HORIZONTAL

PLOT
687
10
1048
340
Amount of Waste Produced
months
monthly-waste-accumulated
0.0
10.0
0.0
10.0
true
true
"" "ask municipalities [\n  create-temporary-plot-pen (word who)\n  set-plot-pen-color color\n  plotxy months monthly-waste-accumulated\n]"
PENS

MONITOR
492
522
549
567
NIL
months
17
1
11

SLIDER
461
883
722
916
unit-recycling-cost
unit-recycling-cost
0
0.2
0.08
0.01
1
€ per kg
HORIZONTAL

SLIDER
461
918
722
951
unit-distance-cost
unit-distance-cost
0
0.01
0.004
0.001
1
€ per km per kg
HORIZONTAL

SLIDER
68
962
329
995
waste-tax-monthly
waste-tax-monthly
0
100
20.0
1
1
€
HORIZONTAL

SWITCH
68
782
329
815
waste-production-knowledge
waste-production-knowledge
0
1
-1000

SLIDER
285
623
554
656
number-recycling-companies
number-recycling-companies
0
20
10.0
1
1
NIL
HORIZONTAL

SLIDER
68
1115
329
1148
unit-cost-perc-campaign
unit-cost-perc-campaign
1
30
10.0
1
1
NIL
HORIZONTAL

MONITOR
549
522
606
567
NIL
years
17
1
11

SLIDER
461
953
722
986
unrest-due-to-decentralized
unrest-due-to-decentralized
0
2
1.43
0.01
1
NIL
HORIZONTAL

SLIDER
461
848
722
881
mean-waste-to-recyclable-ratio-expected
mean-waste-to-recyclable-ratio-expected
0
1
0.24
0.01
1
NIL
HORIZONTAL

SLIDER
68
1007
329
1040
weight-of-price
weight-of-price
0
100
80.0
1
1
NIL
HORIZONTAL

SLIDER
68
1043
329
1076
weight-of-fine
weight-of-fine
0
100
20.0
1
1
NIL
HORIZONTAL

SLIDER
68
1079
329
1112
weight-of-recycling-target
weight-of-recycling-target
0
100
20.0
1
1
NIL
HORIZONTAL

SLIDER
14
658
283
691
recycled-plastic-market-price
recycled-plastic-market-price
0
0.2
0.126
0.001
1
€ per kg
HORIZONTAL

MONITOR
1172
777
1271
822
single-perception
precision mean [factor-of-perception] of households with [household-type = \"single\"] 3
17
1
11

MONITOR
1173
825
1285
870
couple-perception
precision mean [factor-of-perception] of households with [household-type = \"couple\"] 3
17
1
11

MONITOR
1277
778
1377
823
family-perception
precision mean [factor-of-perception] of households with [household-type = \"family\"] 3
17
1
11

MONITOR
1277
825
1377
870
old-perception
precision mean [factor-of-perception] of households with [household-type = \"old\"] 3
17
1
11

MONITOR
1409
824
1521
869
couple-knowledge
precision mean [factor-of-knowledge] of households with [household-type = \"couple\"] 3
17
1
11

MONITOR
1409
779
1517
824
single-knowledge
precision mean [factor-of-knowledge] of households with [household-type = \"single\"] 3
17
1
11

MONITOR
1509
779
1618
824
family-knowledge
precision mean [factor-of-knowledge] of households with [household-type = \"family\"] 3
17
1
11

MONITOR
1509
824
1617
869
old-knowledge
precision mean [factor-of-knowledge] of households with [household-type = \"old\"] 3
17
1
11

PLOT
1051
10
1434
340
Budget of Municipalities
months
profit
0.0
10.0
0.0
10.0
true
true
"" "ask municipalities[\n  create-temporary-plot-pen (word who)\n  set-plot-pen-color color\n  plotxy months budget-mun\n]"
PENS

PLOT
1438
10
1805
340
Budget of Companies
months
budget
0.0
10.0
0.0
10.0
true
true
"" "ask recycling-companies[\n  create-temporary-plot-pen (word who)\n  set-plot-pen-color color\n  plotxy months budget-rc\n]"
PENS

PLOT
688
342
1048
669
Recycling Cost for Companies
months
recycling-cost
0.0
10.0
0.0
10.0
true
true
"" "ask recycling-companies[\n  create-temporary-plot-pen (word who)\n  set-plot-pen-color color\n  plotxy months revenue-recycled-plastic\n]"
PENS

PLOT
1051
342
1434
669
Recycling Performance of Municipalities
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" "ask municipalities[\n  create-temporary-plot-pen (word who)\n  set-plot-pen-color color\n  plotxy months current-recycling-performance\n]"
PENS

PLOT
1438
342
1805
669
Efficiency Level of Companies
NIL
NIL
0.0
10.0
0.0
1.0
true
true
"" "ask recycling-companies[\n  create-temporary-plot-pen (word who)\n  set-plot-pen-color color\n  plotxy months technological-efficiency\n]\n\n"
PENS

TEXTBOX
24
727
387
771
- Municipality Parameters -
30
0.0
1

TEXTBOX
24
573
391
618
- External Parameters -
30
0.0
1

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

book
false
0
Polygon -7500403 true true 30 195 150 255 270 135 150 75
Polygon -7500403 true true 30 135 150 195 270 75 150 15
Polygon -7500403 true true 30 135 30 195 90 150
Polygon -1 true false 39 139 39 184 151 239 156 199
Polygon -1 true false 151 239 254 135 254 90 151 197
Line -7500403 true 150 196 150 247
Line -7500403 true 43 159 138 207
Line -7500403 true 43 174 138 222
Line -7500403 true 153 206 248 113
Line -7500403 true 153 221 248 128
Polygon -1 true false 159 52 144 67 204 97 219 82

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

factory
false
0
Rectangle -7500403 true true 76 194 285 270
Rectangle -7500403 true true 36 95 59 231
Rectangle -16777216 true false 90 210 270 240
Line -7500403 true 90 195 90 255
Line -7500403 true 120 195 120 255
Line -7500403 true 150 195 150 240
Line -7500403 true 180 195 180 255
Line -7500403 true 210 210 210 240
Line -7500403 true 240 210 240 240
Line -7500403 true 90 225 270 225
Circle -1 true false 37 73 32
Circle -1 true false 55 38 54
Circle -1 true false 96 21 42
Circle -1 true false 105 40 32
Circle -1 true false 129 19 42
Rectangle -7500403 true true 14 228 78 270

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

house two story
false
0
Polygon -7500403 true true 2 180 227 180 152 150 32 150
Rectangle -7500403 true true 270 75 285 255
Rectangle -7500403 true true 75 135 270 255
Rectangle -16777216 true false 124 195 187 256
Rectangle -16777216 true false 210 195 255 240
Rectangle -16777216 true false 90 150 135 180
Rectangle -16777216 true false 210 150 255 180
Line -16777216 false 270 135 270 255
Rectangle -7500403 true true 15 180 75 255
Polygon -7500403 true true 60 135 285 135 240 90 105 90
Line -16777216 false 75 135 75 180
Rectangle -16777216 true false 30 195 93 240
Line -16777216 false 60 135 285 135
Line -16777216 false 255 105 285 135
Line -16777216 false 0 180 75 180
Line -7500403 true 60 195 60 240
Line -7500403 true 154 195 154 255

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

lightning
false
0
Polygon -7500403 true true 120 135 90 195 135 195 105 300 225 165 180 165 210 105 165 105 195 0 75 135

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.4
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment_target80fine20price20" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>[budget-per-household] of municipality 0</metric>
    <metric>[budget-per-household] of municipality 1</metric>
    <metric>[budget-per-household] of municipality 2</metric>
    <metric>[budget-per-household] of municipality 3</metric>
    <metric>[budget-per-household] of municipality 4</metric>
    <metric>[budget-per-household] of municipality 5</metric>
    <metric>[current-recycling-performance] of municipality 0</metric>
    <metric>[current-recycling-performance] of municipality 1</metric>
    <metric>[current-recycling-performance] of municipality 2</metric>
    <metric>[current-recycling-performance] of municipality 3</metric>
    <metric>[current-recycling-performance] of municipality 4</metric>
    <metric>[current-recycling-performance] of municipality 5</metric>
    <metric>[frequency-of-survey] of municipality 0</metric>
    <metric>[frequency-of-survey] of municipality 1</metric>
    <metric>[frequency-of-survey] of municipality 2</metric>
    <metric>[frequency-of-survey] of municipality 3</metric>
    <metric>[frequency-of-survey] of municipality 4</metric>
    <metric>[frequency-of-survey] of municipality 5</metric>
    <metric>[number-of-households] of municipality 0</metric>
    <metric>[number-of-households] of municipality 1</metric>
    <metric>[number-of-households] of municipality 2</metric>
    <metric>[number-of-households] of municipality 3</metric>
    <metric>[number-of-households] of municipality 4</metric>
    <metric>[number-of-households] of municipality 5</metric>
    <metric>[type-of-infrastructure] of municipality 0</metric>
    <metric>[type-of-infrastructure] of municipality 1</metric>
    <metric>[type-of-infrastructure] of municipality 2</metric>
    <metric>[type-of-infrastructure] of municipality 3</metric>
    <metric>[type-of-infrastructure] of municipality 4</metric>
    <metric>[type-of-infrastructure] of municipality 5</metric>
    <enumeratedValueSet variable="weight-of-fine">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="waste-production-knowledge">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-of-recycling-target">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-of-price">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="waste-tax-monthly">
      <value value="10"/>
      <value value="15"/>
      <value value="20"/>
      <value value="25"/>
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contract-duration">
      <value value="12"/>
      <value value="36"/>
      <value value="60"/>
    </enumeratedValueSet>
    <steppedValueSet variable="random-seed" first="1" step="1" last="100"/>
  </experiment>
  <experiment name="experiment_target20fine20price80" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>[budget-per-household] of municipality 0</metric>
    <metric>[budget-per-household] of municipality 1</metric>
    <metric>[budget-per-household] of municipality 2</metric>
    <metric>[budget-per-household] of municipality 3</metric>
    <metric>[budget-per-household] of municipality 4</metric>
    <metric>[budget-per-household] of municipality 5</metric>
    <metric>[current-recycling-performance] of municipality 0</metric>
    <metric>[current-recycling-performance] of municipality 1</metric>
    <metric>[current-recycling-performance] of municipality 2</metric>
    <metric>[current-recycling-performance] of municipality 3</metric>
    <metric>[current-recycling-performance] of municipality 4</metric>
    <metric>[current-recycling-performance] of municipality 5</metric>
    <metric>[frequency-of-survey] of municipality 0</metric>
    <metric>[frequency-of-survey] of municipality 1</metric>
    <metric>[frequency-of-survey] of municipality 2</metric>
    <metric>[frequency-of-survey] of municipality 3</metric>
    <metric>[frequency-of-survey] of municipality 4</metric>
    <metric>[frequency-of-survey] of municipality 5</metric>
    <metric>[number-of-households] of municipality 0</metric>
    <metric>[number-of-households] of municipality 1</metric>
    <metric>[number-of-households] of municipality 2</metric>
    <metric>[number-of-households] of municipality 3</metric>
    <metric>[number-of-households] of municipality 4</metric>
    <metric>[number-of-households] of municipality 5</metric>
    <metric>[type-of-infrastructure] of municipality 0</metric>
    <metric>[type-of-infrastructure] of municipality 1</metric>
    <metric>[type-of-infrastructure] of municipality 2</metric>
    <metric>[type-of-infrastructure] of municipality 3</metric>
    <metric>[type-of-infrastructure] of municipality 4</metric>
    <metric>[type-of-infrastructure] of municipality 5</metric>
    <enumeratedValueSet variable="weight-of-fine">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="waste-production-knowledge">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-of-recycling-target">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weight-of-price">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="waste-tax-monthly">
      <value value="10"/>
      <value value="15"/>
      <value value="20"/>
      <value value="25"/>
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contract-duration">
      <value value="12"/>
      <value value="36"/>
      <value value="60"/>
    </enumeratedValueSet>
    <steppedValueSet variable="random-seed" first="1" step="1" last="100"/>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
