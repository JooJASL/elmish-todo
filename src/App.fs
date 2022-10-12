module App

open Elmish
open Elmish.React
open Feliz
open System

type Todo =
  {
    Id : int
    Description : string
    Completed : bool
  }

type FilterMode =
  | All
  | Complete
  | Incomplete

type TodoBeingEdited =
  {
    Id : int
    Description : string
  }
type State =
    {
      TodoList : Todo list
      NewTodo : string // Text user is typing into the input box.
      TodoBeingEdited : TodoBeingEdited option
      FilterListMode : FilterMode
    }

let init() =
    { 
      TodoList = []
      NewTodo = ""
      TodoBeingEdited = None
      FilterListMode = All
    }

type Msg =
  | SetNewTodo of string
  | AddNewTodo
  | ToggleCompleted of int
  | DeleteTodo of int
  | CancelEdit
  | ApplyEdit
  | StartEditingTodo of int
  | SetEditedDescription of string
  | SetFilterMode of FilterMode



let update (msg: Msg) (state: State): State =
    match msg with
    | SetNewTodo desc -> { state with NewTodo = desc }
    | DeleteTodo id ->
      let nextTodoList = 
        state.TodoList 
        |> List.filter (fun todo -> todo.Id <> id)
      { state with TodoList = nextTodoList }
    | ToggleCompleted todoId ->
      let nextTodoList =
        state.TodoList
        |> List.map (fun todo -> 
          if todo.Id = todoId
          then { todo with Completed = not todo.Completed}
          else todo)
      
      { state with TodoList = nextTodoList }
    | AddNewTodo when state.NewTodo = "" -> state
    | AddNewTodo ->
      let nextTodoId = 
        match state.TodoList with
        | [ ] -> 1
        | elems ->
          elems
          |> List.maxBy (fun todo -> todo.Id)
          |> fun todo -> todo.Id + 1
      
      let nextTodo = 
        { Id = nextTodoId; Description = state.NewTodo; Completed = false }

      { state with
          NewTodo = ""
          TodoList = List.append state.TodoList [nextTodo]}
    | StartEditingTodo todoId ->
      let nextEditModel =
        state.TodoList
        |> List.tryFind (fun todo -> todo.Id = todoId)
        |> Option.map (fun todo -> { Id = todoId; Description = todo.Description })
      
      { state with TodoBeingEdited = nextEditModel }
    | CancelEdit -> { state with TodoBeingEdited = None }
    | ApplyEdit ->
      match state.TodoBeingEdited with
      | None -> state
      | Some todoBeingEdited when todoBeingEdited.Description = "" -> state
      | Some todoBeingEdited -> 
        let nextTodoList =
          state.TodoList
          |> List.map (fun todo ->
            if todo.Id = todoBeingEdited.Id
            then { todo with Description = todoBeingEdited.Description }
            else todo)
        
        { state with TodoList = nextTodoList; TodoBeingEdited = None }
    | SetEditedDescription newText ->
      let nextEditModel =
        state.TodoBeingEdited
        |> Option.map (fun todoBeingEdited -> 
          { todoBeingEdited with Description = newText })
      
      { state with TodoBeingEdited = nextEditModel }
    | SetFilterMode newFilterMode -> 
      { state with FilterListMode = newFilterMode }
        
let appTitle : ReactElement = 
  Html.p [
    prop.className "title"
    prop.text "Elmish To-Do"
  ]

let inputField (state: State) (dispatch: Msg -> unit) : ReactElement =
  Html.div [
    prop.classes [ "field"; "has-addons" ]
    prop.children [
      Html.div [
        prop.classes [ "control"; "is-expanded"]
        prop.children [
          Html.input [
            prop.classes [ "input"; "is-medium" ]
            prop.valueOrDefault state.NewTodo
            prop.onChange (SetNewTodo >> dispatch)
            prop.onKeyDown (fun event ->
              if event.key = "Enter" then dispatch AddNewTodo)
          ]
        ]
      ]

      Html.div [
        prop.className "control"
        prop.children [
          Html.button [
            prop.classes [ "button"; "is-primary"; "is-medium" ]
            prop.onClick (fun _ -> dispatch AddNewTodo)
            prop.children [
              Html.i [ prop.classes [ "fa"; "fa-plus" ] ]
            ]
          ]
        ]
      ]
    ]
  ]

let div (classes: string list) (children: Fable.React.ReactElement list) =
    Html.div [
        prop.classes classes
        prop.children children
    ]

let renderFilterTabs (state: State) (dispatch : Msg -> unit) =
  if state.TodoList = [ ]
  then Html.div []
  else
  div ["control"; "buttons"; "is-fullwidth"; "tabs"] [
    Html.li [
      prop.classes [ "button"; if state.FilterListMode = All then "is-primary"]
      prop.onClick (fun _ -> dispatch (SetFilterMode All))
      prop.children [
        Html.p "All"
      ]
    ]
    Html.li [
      prop.classes [ "button"; if state.FilterListMode = Complete then "is-primary"]
      prop.onClick (fun _ -> dispatch (SetFilterMode Complete))
      prop.children [
        Html.p "Completed"
      ]
    ]
    Html.li [
      prop.classes [ "button"; if state.FilterListMode = Incomplete then "is-primary"]
      prop.onClick (fun _ -> dispatch (SetFilterMode Incomplete))
      prop.children [
        Html.p "Incomplete"
      ]
    ]
  ]

let renderEditForm (todoBeingEdited : TodoBeingEdited) (dispatch : Msg -> unit) =
  div ["box"] [
    div ["field"; "is-grouped"] [
      Html.input [
        prop.id "input-being-edited"
        prop.classes ["input"; "is-medium"]
        prop.valueOrDefault todoBeingEdited.Description
        prop.autoFocus true
        prop.onTextChange (SetEditedDescription >> dispatch)
        prop.onKeyDown (fun key ->
          if key.key = "Enter" then dispatch ApplyEdit)
      ]
    ]

    div ["control"; "buttons"] [
      Html.button [
        prop.classes ["button"; "is-primary"]
        prop.onClick (fun _ -> dispatch ApplyEdit)
        prop.children [
          Html.i [ prop.classes ["fa";"fa-save"]]
        ]
      ]
      Html.button [
        prop.classes ["button"; "is-primary"]
        prop.onClick (fun _ -> dispatch CancelEdit)
        prop.children [
          Html.i [ prop.classes ["fa";"fa-arrow-right"]]
        ]
      ]
    ]
  ]

let renderTodo (todo: Todo) (dispatch: Msg -> unit) =
  div [ "box" ] [
    div [ "columns"; "is-mobile"; "is-vcentered" ] [
      div [ "column" ] [
        Html.p [
          prop.className "subtitle"
          prop.text todo.Description
        ]
      ]

      div [ "column"; "is-narrow" ] [
        div [ "buttons" ] [
          Html.button [
            prop.classes [ "button"; if todo.Completed then "is-success"]
            prop.onClick (fun _ -> dispatch (ToggleCompleted todo.Id))
            prop.children [
              Html.i [ prop.classes [ "fa"; "fa-check" ] ]
            ]
          ]

          Html.button [
            prop.classes [ "button"; "is-primary"]
            prop.onClick (fun _ -> dispatch (StartEditingTodo todo.Id))
            prop.children [
              Html.i [ prop.classes [ "fa"; "fa-edit" ] ]
            ]
          ]

          Html.button [
            prop.classes [ "button"; "is-danger" ]
            prop.onClick (fun _ -> dispatch (DeleteTodo todo.Id))
            prop.children [
              Html.i [ prop.classes [ "fa"; "fa-times" ] ]
            ]
          ]
        ]
      ]
    ]
  ]  

let renderTodoList (state: State) (dispatch: Msg -> unit) : ReactElement =
  Html.ul [
    prop.children [
      let filteredList =
        match state.FilterListMode with
        | All -> state.TodoList
        | Complete -> 
          state.TodoList 
          |> List.filter (fun item -> item.Completed = true)
        | Incomplete -> 
          state.TodoList
          |> List.filter (fun item -> item.Completed = false)

      for todo in filteredList ->
        match state.TodoBeingEdited with
        | Some todoBeingEdited 
          when todoBeingEdited.Id = todo.Id -> renderEditForm todoBeingEdited dispatch
        | _ -> renderTodo todo dispatch
        
    ]
  ]

let render (state: State) (dispatch: Msg -> unit) =
  Html.div [
    prop.style [ style.padding 20 ]
    prop.children [
      appTitle
      renderFilterTabs state dispatch
      renderTodoList state dispatch
      Html.div [ prop.style [ style.height 20 ] ] // Some spacing.
      inputField state dispatch
    ]
    
  ]

Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run
