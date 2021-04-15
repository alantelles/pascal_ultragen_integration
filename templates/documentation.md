# Documentation
This page is dedicated to explain how this app was built.
## About
FreePascal/Lazarus is well knwown as a powerful all purpose tool for building GUI applications as well console applications, including web applications. In this tutotrial we see two of these possibilities using FPWeb, a simple framework to build web applications and UltraGen, a template engine/scripting language totally built with FreePascal.

It's fully possible to build a website with UltraGen. It has APIs to handle headers, static serving, sessions, routing, and other common stuff you need to build a web app. However, another possibility is to integrate your FreePascal app written with any framework with UltraGen processing. UltraGen has internally some interfaces that allows you to "deliver" data directly from your program to a UltraGen script.

This example is built with FPWeb but you'll be able to figure how you can apply the concept to any FreePascal program, even not in web applications.

## 1 - The Pascal part
### Setting routes
The Pascal backend consist of a tiny application where we will consume some endpoints from the [Ghibli studio API](ghibliapi.herokuapp.com) and pass it to a UltraGen template to show data response on the page. I will not be so minuscious in explain how to build a FPWeb app as this is ot the focus. You can refer to an excellent and referenced tutorial about it at [this page](https://wiki.freepascal.org/fpWeb_Tutorial).

In FPWeb we have a routing mechanism where we use a procedure to register a route, a handler procedure to be used as callback and a method to accept. Is this app only four routes are declared.

*pascal_ultragen_integration.lpr*
```pascal
HTTPRouter.RegisterRoute('/', rmGet, @RouteHandlers.index);
HTTPRouter.RegisterRoute('/film/:id', rmGet, @RouteHandlers.show);
HTTPRouter.RegisterRoute('/doc', rmGet, @RouteHandlers.documentation);
if GetEnv('CAN_TURN_OFF') <> '' then
  HTTPRouter.RegisterRoute('/off', rmDelete, @TurnOffServer);
```

The "/off" route enabled by the "CAN_TURN_OFF" env variable exists because FPWeb web server can't be stopped with `Ctrl+C`. So, it's a common practice to use a protected route to stop the server. In production, of course, you won't set this variable and this route won't be registered.

Each one of the others routes will be handled for the declared procedures that are part of `RouteHandlers` unit. The `:id` in second route is a route parameter and can be used in handler body. Let's get to the handlers unit.

### Explaining handlers

The unit uses FPHttpClient to making the requests needed to the Ghibli API. Again, it's not the focus bu if you feel interested you can see more of it [here](https://wiki.lazarus.freepascal.org/fphttpclient).

*index* at *RouteHandlers.pas*

```pascal
procedure index(ARequest: TRequest; AResponse: TResponse);
var
  Output: string;
  Adapter: TUltraAdapter;
  Response: string;
begin
  Response := TFpHttpClient.SimpleGet(APIURL + 'films');
  Adapter := TUltraAdapter.Create('fromApp');
  Adapter.AddMember('template', 'list_films');
  Adapter.AddMember('response', Response);
  Output := ProcessTemplate(Adapter);
  AResponse.Content := Output;
end;
```

Here the word "Ultra" comes to us at the first time. What are we doing here. We:
1. Get the information from endpoint
1. Create an Ultra Adapter
1. Process a template using he adapter we created and get the process output (a string)
1. Set that string as the request response

But what the heck is an "UltraAdapter"?

UltraGen has its internal data types that it knows how to handle. Some of they are wrapper around basic Pascal data types but we don't want you Pascal developer to be worry about this. So a unit called "UltraGenInterfaceClass" feature some methods/classes so you have to mess the minimum possible with UltraGen implementation directly. One of these classes is the `TUltraAdapter`. It returns an object that internally handles a `TActivationRecord` instance. The default type for a hashmap with values in a scope for UltraGen. The `TUltraAdapter` provides overloaded methods to add members to the map, receiving an UltraGen allowed qualifier name and a value that can be (at the moment of writing this) a string, an integer, a float or a boolean. When you create an adapter you must set its name. This name also must be an UltraGen valid qualifier name, as it will be passed to script. This adapter is passed to the template processor and these values will be available in the script as a UltraGen `Dict` - the type for dictionaries.

That explained, we are:
1. Creating an adapter called "fromApp", that is, this will be the name by we will get this data in script as a dictionary.
1. Adding a key 'template' with 'list_films' value, that we will get in script using `fromApp['template']` or `fromApp[:template]` using the more semantic *idStrings* syntax
1. Adding a key 'response' with the `Response` value that holds the response from Ghibli API request
1. Process a template using he adapter we created and get the process output (a string)
1. Set that string as the request response

The `RouteHandlers.show` handler is very similar with the difference that we pass the `:id` parameter of route as one of the values.

We have in common in all of three handlers the fact that we pass the template we want to be rendered and the name of adapter is always 'fromApp'. Let's now see the `ProcessTemplate` function.

### The Processor unit

Only for segregation purposes the processor is a function in another unit. The `ProcessTemplate` method receives a `TUltraAdapter` as parameter and process the script in `templates/index.ultra` and returns the result (a string). Here things start to be a little "UltraGenistic". 
```pascal
function ProcessTemplate(Adapter: TUltraAdapter):string;
var
  PreludeLines: TStringList;
  UHome: string;
begin
  UHome := ParamStr(2);
  PreludeLines := TStringList.Create;
  PreludeLines.Add('addModulePath(["'+UHome + '", "modules"].path())');
  PreludeLines.Add('include @Core');
  Result := TUltraInterface.InterpretScript('templates/index.ultra', PreludeLines, Adapter);
end;
```
`UHome` is a variable to hold the `ULTRAGEN_HOME` the place where your UltraGen build is located. It will be used to register a **module path** that allows a different, more simple and semantic syntax to include scripts in your page. Why you need it? UltraGen has the capability of extend class "on-the-fly" like JavaScript prototype.

The API for processing a template provides the `InterpretScript` method. It handles all settings needed by UltraGen `TInterpreter` class. This method, in this overload version, the most abstract of three, receives a path of the script, a `TUltraAdapter` and a `TStringList` which lines hold valid UltraGen code. We call this script **prelude**. The script contained in this string list will be parsed and inserted before the script passed in `FilePath` parameter. We will use this feature to add the ULTRAGEN_HOME/modules to the module path, so we can include scripts in that path using the **module include syntax**. If ULTRAGEN_HOME is for example */my/path/to/ultragen*, `@Core` will be resolved to `/my/path/to/ultragen/modules/Core.ultra` or `/my/path/to/ultragen/modules/Core/_init.ultra`. In this case, the second option will be used as the `@Core` module is a folder which contains all the methods used in UltraGen built-in classes. And why you *should* include `@Core`? As said before, UltraGen allows insert methods to a class in any place of code. The `@Core` module take advantage of this and define some additional methods to built-in types. For example, `append` method for `List` is implemented internally, with FreePascal. However, `each`, `map` and some other methods are implemented in UltraGen itself. You can use methods implemented in any of these environments seamlessly.

Another point worthy to note is about the file path. Notice that every process will be handled by *templates/index.ultra*. Why can't I pass the path as argument to processor? You can perfectly do this. I myself, personally think it's more useful use the same script and handle the rest of process as from this script. You will probably get less *code repetition* doing this.

Well, let's take a look in what we got:
1. You know how to handle UltraGen interface to process your template
1. You know what is a prelude and understand that it will be called before your script
1. You understand why is a good practice to add the ULTRAGEN_HOME to modules paths
1. You know why is a good practice to include the `@Core` module.

Now we have seen all FreePascal part of the application. The presented here is enough to understand the others route handlers. Let's move forward to UltraGen part of application

## 2 - The UltraGen part

Now let's see how UltraGen acts on this app by processing the view and using its scripting features.

### The "index"

This is a tiny script and we'll explain it in parts.

Here, a function to return the full template path.
```
function templateName(name)
    full = 'templates/' + name + '.ultra.html'
    return full
end
```

This line includes a template. A template, essentially is a UltraGen script with support to plain text
```
include 'templates/base.ultra.html'
```

The full template path makes use of the function described above and use as name the value received in `fromApp`. Yes, it's exactly the `fromApp` adapter you created in "FreePascal part" of tutorial. The data you added to `fromApp` is available here as a dictionary. One possible syntax to get dictionary values is the old and good `variable["key"]` scheme.
```
template = templateName(fromApp['template'])
```

Here, things get interesting. We re using two cool features of UltraGen: lambda functions and decorators.
`Base` is a decorator defined in `templates/base.ultra.html`. If receives at least one function. In this case, the function is a `lambda`, a syntax to declare a function with only one line. `view` variable will be a function decorated with `Base`.
```
view = Base(lambda () : include template)
```

Here a conditional variable initialization and a list of links to be used in navbar. A point to note is the other syntax for getting a key of a dictionary. The *idString*. You can write strings with `:some_string` since the word is a valid UltraGen identifier. It's a little more semantic since the lack of quotation marks takes away the appearance of free text in dictionaries with keys that looks like class attributes.
```
if ((fromApp.hasKey(:response)))
    data = JSON.parse(fromApp[:response])
end

links = [
    {:text: 'Home', :link: '/'},
    {:text: 'GitHub', :link: 'https://github.com/alantelles/pascal_ultragen_integration'},
    {:text: 'Documentation', :link: '/doc'}
]
```

At last, the `live` function. This command append its argument to the response. In this case, we are "living" the content returned by `view()` function. And `view` is a function that outputs the `template`'s *live* decorated by `base.ultra.html`.
```
live view()
```

### The live output

UltraGen has two output streams. The *stdout* which is writable by `print` or `inline` and the *live* output that holds the result of template processing. Both scripts and templates have a *live* output, even out a web application context. You can use UltraGen for processing, for example, XML with a dictionary as data input - UltraGen was first conceived for this. The `live` output always "exists". It is always the final product of a execution. You can add text to *live* output using the keyword `live`. Let's check the `base.ultra.html` template to see the concet in action.
```
@decorator Base(content)
<!DOCTYPE html>
```
At first line we se a new symbol: `@`. An `@` in the start of a line in a template declares that this line will be parsed as script. Following, we see the declaration of a decorator called `Base` which receives an argument called `content` that must be a function.

Advancing we see another `@` and more UltraGen code
```
@           for(links, link)
            <li class="nav-item">
                <a class="nav-link" href="{{ link[:link] }}">{{ link[:text] }}</a>
            </li>
@           end
```

Now we are using `for`, a typical *for* iterator which is used to repeat the snippet inside its block to print some list items - the links we declared in `index.ultra`. Another new element is the embedding of UltraGen values in the text. It's done by using the "double mustache" syntax. Any valid **expression** is allowed inside it. Statements are not allowed. Any value given is automatically casted to `String`.

Moving forward, we have the insertion of content in the base template.
```
<div class="col-lg-12">{{ content() }}</div>
```
The embedding will output the result of the function given in content. As we have seen before, this will be the live output of the template given in `fromApp[:template]`.

> Notes:
>
>1. All the plain text in a template is sent to live.
>1. Functions also has its live outputs. By default, the return of any function is its live output, that is, if  function doesn't have any `return` statement, what is returned from it is its live.

Due to the mentioned behaviour, the `view` function, when executed returns the live output of `content` function decorated by `Base` template.

As we use the index route to explain FreePascal part, let's use the list_films template to show the UltraGen part of listing Ghibli films.

This template is a simple table. All we see here is seen before. We have a `@` to declare we're using a line as script and embedding parts to print the films attributes. 

```
@       for(data, film)
        <tr>
            <td><a href="/film/{{ film[:id] }}">{{ film[:title] }}</a></td>
            <td>{{ film[:director] }}</td>
            <td>{{ film[:release_date] }}</td>
        </tr>
@       end
```
Again we are using *idStrings* syntax. Note that `data` is a variable we defined in `index` and used `fromApp[:response]` as source. Note also that UltraGen includes scripts at the same scope level by default. Furthermore, UltraGen names lookup is  backward recursive. This is why `data` is visible by this template.

Just to show another method let's see the template for `show` handler.
```
@data.localize()
<div class="card" style="margin-bottom: 10px">
    <div class="card-body">
        <h2 class="display-4">{{ title }}</h2>
        <table class="table no-border">
            
            <tr>
                <th style="width: 160px">Directed by</th>
                <td>{{ director }}</td>
            </tr>
```
If you look at response, you will see that `data` has *title*, *director* between other keys. So these values should be accessed with `data[:title]`, `data[:director]` and so on. Now get to the scene the `localize` method. It declares all the keys of a `Dict` as variables at the current scope. If some key is not UltraGen valid identifier no error is raised but that value will never be accessed. Anyway, the original dictionary is not affected. The value will still be accessible with normal syntax.

The third view - that you are viewing now - shows the built-in UltraGen Markdown parser in action.

```
@include @Core.Markdown
@live Markdown.parseFile('templates/documentation.md')
```

Here we see two things previously said in action:
1. The importance of setup Core module
1. The way that a value can be sent to live

The `Markdown` is a class capable of parse Markdown text using the [CommonMark](https://commonmark.org/) spec.

## Conclusion

This app/tutotial was intended to show an example of how to use the UltraGen powerful scripting and template processing possibilities with a FreePascal application. FPWeb and FPHttpClient were used just for using some "native" unit. You can use it with any FreePascal project based in any framework like the famous [BrookFramework](https://github.com/risoflora/brookframework) based apps. You can find more details about the UltraGen part of this app in the UltraGen documentation.