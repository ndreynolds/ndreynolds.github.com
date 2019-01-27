---
title: Getting AngularJS and Rails talking
---

In this post, I want to explore a few of the ways to exchange data between
a Rails app and an AngularJS module---looking at the pros and cons of each
approach. While Angular sees a lot of usage in single-page applications (SPAs),
I've found it just as useful for enhancing certain pages of a Rails application
with more dynamic interfaces, while sticking to plain, scaffoldable CRUD screens for
the rest of the app.

<!--more-->

With this approach, it doesn't make sense to kick HAML to the curb, turn your
Rails app into a JSON API, and render everything on the client side. Rails may
not be the new kid on the block anymore, but it has a lot of mature
gems that make putting together a complex site a breeze.

With that in mind, it's not always clear how best to combine the two
technologies:

* When do you use Angular templates and when do you stick with HAML/ERB?
* How do you pass the data from the Rails side to your Angular controllers?


### JSON endpoints in the Rails controller

The first approach is to provide a JSON format for the resource in your
controller. Imagine we wanted to implement client-side filtering of
a short list of items in a store's inventory. We can easily add a JSON format
to the index action:

```ruby
# app/controllers/items_controller.rb
class ItemsController < ApplicationController
  def index
    @items = Item.all
    respond_to do |format|
      format.html # index.html.erb
      format.json { render json: @items }
    end
  end

  # other actions...
end
```

Then from our HAML template, we'll provide the Angular templating to
dynamically render the items:

```haml
# app/views/items/index.html.haml
%input(type="text" ng-model="search")

%table(ng-controller="ItemsController")
  %thead
    %tr
      %th Name
      %th Price
  %tbody
    %tr(ng-repeat="item in items | filter : search")
      %td {{ item.name }}
      %td {{ item.price | currency }}
```

Lastly, we'll initialize the scope's `items` array by requesting the data as
JSON from the Rails server using the `$http` service.

```javascript
// app/assets/javascripts/items/items_controller.js
angular.module('inventoryManager')
  .controller('ItemsController', function($scope, $http) {
    $scope.items = [];

    $http.get('/items.json')
      .success(function(data) { $scope.items = data.items; });
  });
```

I like this approach, but it has a few issues:

* The items are requested in an additional HTTP request and we're only handling
  the success case. We've introduced the possibility that the second request
  fails and the user sees an empty list of items. In a production app, we'd have
  to add error-handling for that case, which comes with some added complexity.
* It's not always practical to create ad-hoc JSON routes for backend data. In
  this case, as `items` is clearly a collection of resources, it works well. If
  it were a list of filter options, adding a new JSON route becomes a little
  questionable.

### ngInit (and ngInitial)

When building forms, it's common to want to show or hide an option or when
something else is selected. Say our inventory app collects some basic
information about how the item is taxed when an item is added. If the item is
marked as taxable, the "tax rate" field should appear. If it is not taxable,
the field should be hidden.

The is definitely a job for Angular, but I'm not willing to give up
`simple_form` in the process. Let's see if we can make them play together.

The item form partial will be used for both the `new` and `edit` actions. The
show/hide behavior must work for both a new item as well as for an existing item
that has already been marked as taxable.

The `ngInit` directive evaluates an expression in the current scope. After
assigning the taxable checkbox to the `$scope.taxable` via `ng-model`, we can
use `ng-init` it to set its initial value:

```haml
# app/views/items/_form.html.haml
%div(ng-controller="ItemFormController")
  = simple_form_for @item do |f|
    = f.input :taxable, 'ng-model' => 'taxable', 'ng-init' => "taxable = #{@item.taxable.to_json}"
    = f.input :tax_rate, 'ng-if' => 'taxable'
```

Then from the Angular controller, there's not much left to do:

```javascript
// app/assets/javascripts/items/item_form_controller.js
angular.module('inventoryViewer')
  .controller('ItemFormController', function($scope, $http) {
    // We don't need to add any code here for the above to work.
  });
```

While this a decent strategy for simple use cases like above, it's easy to
abuse and mixing Ruby and Angular templates can get ugly. A slightly cleaner
approach is to use something like the `ngInitial` directive from [this
StackOverflow answer][1]. This directive sets the initial value of the `$scope`
variable using the `value` attribute, which will be set automatically by the
form helper. I've slightly modified the directive below to also handle checkbox
inputs:

```javascript
angular.module('inventoryManager').directive('ngInitial', function() {
  return {
    restrict: 'A',
    controller: [
      '$scope', '$element', '$attrs', '$parse', function($scope, $element, $attrs, $parse) {

        function parseVal() {
          if ($attrs.type === 'checkbox') {
            return !!$attrs.checked;
          } else {
            return $attrs.value;
          }
        }

        var getter, setter, val;
        val = $attrs.ngInitial || parseVal();
        getter = $parse($attrs.ngModel);
        setter = getter.assign;
        setter($scope, val);
      }
    ]
  };
});
```

It can then be used in the above HAML view like so:

```haml
# app/views/items/_form.html.haml
%div(ng-controller="ItemFormController")
  = simple_form_for @item do |f|
    = f.input :taxable, input_html: { 'ng-model' => 'taxable', 'ng-initial' => '' }
    = f.input :tax_rate, input_html: { 'ng-if' => 'taxable' }
```

(Unfortunately, I don't know of a cleaner way to define valueless attributes.)

### Using script tags

When mixing Backbone.js and Rails, it's common to [bootstrap Backbone models
and collections][2] from a script tag at the bottom of the page. It's possible
to do something similar in Angular by putting the data somewhere in the global
scope and accessing it from the Angular controller.

```haml
# app/views/items/index.html.haml
:javascript
  window.items = #{@items.to_json};
```

```javascript
// app/assets/javascripts/items/items_controller.js
angular.module('inventoryManager')
  .controller('ItemsController', function($scope) {
    $scope.items = window.items;
  });
```

While this is a bit simpler than requesting the data with the `$http` service
and does work quite well, using global variables means the angular app loses
its nice encapsulation, which can make testing problematic. Global variables
also of course come with problems of their own.

### JSON data attributes

Another approach is to render the data as JSON to data attributes. By adding a
`data-items` attribute to the element with the `ng-controller` attribute, you
can easily access the data with an injected `$element` dependency.

```haml
# app/views/items/index.html.haml
%ul{ 'ng-controller' => 'ItemsController', 'data-items' => items.to_json }
  %li(ng-repeat="item in items") {{ item.name }}
```

```javascript
// app/assets/javascripts/items/items_controller.js
angular.module('inventoryManager')
  .controller('ItemsController', function($scope, $element) {
    $scope.items = $element.data('items');
  });
```

This is my favorite approach in cases where a JSON endpoint doesn't make sense.
The `$element` dependency can be mocked, so the code is easily-testable. It
doesn't rely on global variables, so the app is still nicely encapsulated. On top
of that, we also avoid making another request.

[1]: http://stackoverflow.com/a/17823590
[2]: http://backbonejs.org/#FAQ-bootstrap
