namespace Experiment

open WebSharper.Html.Server
open WebSharper
open WebSharper.Sitelets


module Site =
    type Action = | Index

    let IndexContent : Content<Action> =
        PageContent <| fun context ->
            {Page.Default with
                Title = Some "Particle Sim"
                Body = [Div [Style "text-align: center;"]
                            -< [new ParticleControl()]]}
        
    type Site() =
        interface IWebsite<Action> with
            member this.Sitelet = Sitelet.Content "/" Index IndexContent
            member this.Actions = [Index]

[<assembly: Website(typeof<Site.Site>)>]
do ()

