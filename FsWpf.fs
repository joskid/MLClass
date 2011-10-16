//from http://v2matveev.blogspot.com/2010/03/f-and-wpf-or-how-to-make-life-bit.html

module FsWpf
 
open System
open System.IO
open System.Reflection
open System.Windows
open System.Windows.Markup
 
[<AttributeUsage(AttributeTargets.Field, AllowMultiple = false)>]
type UiElementAttribute(name : string) = 
    inherit System.Attribute()
    new() = new UiElementAttribute(null)
    member this.Name = name
 
[<AbstractClass>]
type FsUiObject<'T when 'T :> FrameworkElement> (xamlPath) as this = 
    
    let loadXaml () = 
        XamlReader.Parse(File.ReadAllText(xamlPath))

    let uiObj = loadXaml() :?> 'T
     
    let flags = BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public
     
    do 
        let fields = 
            this.GetType().GetFields(flags) 
            |> Seq.choose(fun f -> 
                let attrs =  f.GetCustomAttributes(typeof<UiElementAttribute>, false)
                if attrs.Length = 0 then 
                    None
                else
                    let attr = attrs.[0] :?> UiElementAttribute
                    Some(f, if String.IsNullOrEmpty(attr.Name) then 
                                f.Name 
                            else 
                                attr.Name))
        for field, name in fields do
            let value = uiObj.FindName(name)
            if value <> null then
                field.SetValue(this, value)
            else
                failwithf "Ui element %s not found" name
 
    member x.UiObject = uiObj

