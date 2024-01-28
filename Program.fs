open System
open System.Collections.Generic

open FSharp.Control
open IcedTasks

open Hox
open Hox.Core
open System.Text
open System.Web
open System.Threading
open System.Text.RegularExpressions

type DeferredNode =
    | Async of CancellableValueTask<Node>
    | Seq of IAsyncEnumerable<Node>

let getAttributes (attributes: AttributeNode list) =
    cancellableValueTask {
        let clsSeq = ResizeArray()
        let attrSeq = ResizeArray()
        let mutable id = ValueNone

        // ids and classes have to be HtmlAttributeEncode'ed because we're
        // handling them separately, attributes are handled by the renderAttr function
        // which will HtmlAttributeEncode them.
        for attribute in attributes do
            match attribute with
            | AttributeNode.Attribute { name = ""; value = value } -> ()
            | AttributeNode.Attribute { name = "class"; value = value } ->
                clsSeq.Add(value |> HttpUtility.HtmlAttributeEncode)
            | AttributeNode.Attribute { name = "id"; value = value } ->
                id <- id |> ValueOption.orElse (ValueSome(value |> HttpUtility.HtmlAttributeEncode))
            | AttributeNode.Attribute attribute -> attrSeq.Add(attribute)
            | AttributeNode.AsyncAttribute asyncAttribute ->
                let! { name = name; value = value } = asyncAttribute

                if name = String.Empty then
                    ()
                elif name = "id" then
                    id <- id |> ValueOption.orElse (ValueSome(value |> HttpUtility.HtmlAttributeEncode))
                elif name = "class" then
                    clsSeq.Add(value |> HttpUtility.HtmlAttributeEncode)
                else
                    attrSeq.Add({ name = name; value = value })

        return id, clsSeq, attrSeq
    }

let renderNode (node: Node, queue: Queue<Guid * DeferredNode>) =
    cancellableValueTask {
        let sb = StringBuilder()
        let stack = Stack<Node * bool>()
        stack.Push(node, false)

        while stack.Count > 0 do
            let node, isClosing = stack.Pop()

            match node with
            | Element { tag = tag } when isClosing -> sb.Append("</").Append(tag).Append(">") |> ignore
            | Text text -> sb.Append(text |> HttpUtility.HtmlEncode) |> ignore
            | Comment text -> sb.Append("<!--").Append(text |> HttpUtility.HtmlEncode).Append("-->") |> ignore
            | Raw text -> sb.Append(text) |> ignore
            | Element { tag = tag
                        attributes = attrs
                        children = children } ->
                sb.Append("<").Append(tag) |> ignore

                let! id, clsSeq, attrSeq = getAttributes attrs

                if id |> ValueOption.isSome then
                    sb.Append(" id=\"").Append(id.Value).Append("\"") |> ignore

                if clsSeq.Count > 0 then
                    sb.Append(" class=\"").Append(String.Join(" ", clsSeq)).Append("\"") |> ignore

                for attribute in attrSeq do
                    sb
                        .Append(" ")
                        .Append(attribute.name)
                        .Append("=\"")
                        .Append(attribute.value |> HttpUtility.HtmlAttributeEncode)
                        .Append("\"")
                    |> ignore

                sb.Append(">") |> ignore

                stack.Push(node, true)

                for child in children.Length - 1 .. -1 .. 0 do
                    stack.Push(children[child], false)
            | Fragment children ->
                for child in children.Length - 1 .. -1 .. 0 do
                    stack.Push(children[child], false)
            | AsyncNode work ->
                let marker = Guid.NewGuid()
                sb.Append($"<ho-marker id='{marker}' class='ho-loading'></ho-marker>") |> ignore
                queue.Enqueue(marker, Async work)
            | AsyncSeqNode work ->
                let marker = Guid.NewGuid()
                sb.Append($"<ho-marker id='{marker}' class='ho-loading'></ho-marker>") |> ignore
                queue.Enqueue(marker, Seq work)

        return sb.ToString()
    }

let renderDeferredNode (deferred: DeferredNode, queue: Queue<Guid * DeferredNode>) =
    cancellableValueTask {
        let sb = StringBuilder()

        match deferred with
        | Async work ->
            let! node = work
            let! html = renderNode (node, queue)
            sb.Append(html) |> ignore
        | Seq nodes ->
            for node in nodes do
                let! html = renderNode (node, queue)
                sb.Append(html) |> ignore

        return sb.ToString()
    }


let render (node: Node) =
    cancellableValueTask {
        let deferredQueue = Queue<Guid * DeferredNode>()
        let! html = renderNode (node, deferredQueue)
        return html, deferredQueue
    }

let renderDeferred (queue: Queue<Guid * DeferredNode>) =
    cancellableValueTask {
        let stack = Stack<Guid * string>()

        while queue.Count > 0 do
            let marker, deferred = queue.Dequeue()
            let! html = renderDeferredNode (deferred, queue)
            stack.Push(marker, html)

        return stack
    }


let testNode =
    h (
        "div",
        h (
            async {
                do! Async.Sleep(1000)
                return h ("div", "Hello")
            }
        ),
        h ("ul", h ("li", "Item 1"), h ("li", "Item 2"), h ("li", "Item 3")),
        h (
            async {
                do! Async.Sleep(1000)
                return h ("div", "World")
            }
        ),
        h ("ul", h ("li", "Item 4"), h ("li", "Item 5"), h ("li", "Item 6")),
        h (
            "ul",
            fragment (
                taskSeq {
                    do! Async.Sleep(1000) |> Async.StartAsTask
                    h ("li", "Item 7")
                    do! Async.Sleep(1000) |> Async.StartAsTask
                    h ("li", "Item 8")
                    do! Async.Sleep(1000) |> Async.StartAsTask
                    h ("li", "Item 9")
                }
            )
        )
    )

task {
    let token = CancellationToken.None
    let! html, deferredQueue = render testNode token
    // do stuff with html
    // maybe even take it back to the client
    printfn "Initially rendered: \n\n%s" html

    printfn "Starting deferred rendering..."
    // somewhere else, run this to get the deferred nodes
    let! stack = renderDeferred deferredQueue token

    let mutable html = html

    printfn "Deferred rendering complete, replacing markers..."

    while stack.Count > 0 do
        let marker, deferredHtml = stack.Pop()

        html <- html.Replace($"<ho-marker id='{marker}' class='ho-loading'></ho-marker>", deferredHtml)

    printfn "Resolved render: \n\n%s" html
}
|> Async.AwaitTask
|> Async.RunSynchronously
