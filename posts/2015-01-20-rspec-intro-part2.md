---
title: Testing Rails 4 apps with RSpec 3: Part II
---

In this two-part series, I cover testing a Rails application using RSpec and
some other popular gems. If you missed Part 1, you can catch up here.

In Part 2, with the setup out of the way, we'll dive into writing tests for the
various components of a Rails app. 

Before diving in, I will say that some of this may be a bit opinionated. When
I'm testing a Rails app, my goal is to get the most bang for my buck---that is,
the most test coverage for the fewest lines of test code. If you find that
a test requiring extensive mocking or brittle networking logic begins taking up
more than its share of your development time, in my book it's fine to just 
`git rm` it and move on. Likewise, while getting to 100% coverage is a noble goal,
it isn't always as realistic (in the face of schedule and budget constraints)
or as helpful (think: future refactoring) as it might seem. The real objective
isn't to hit some arbitrary percentage---it's to make your app more reliable
for your users and prevent regressions as you develop it.

## Overview

In this article, we'll write tests that touch on each part of the Rails MVC
architecture:

Model Specs
  ~ Test your Rails models---scopes, validations, custom methods
  ~ `spec/models/post_spec.rb`

Controller Specs
  ~ Test your Rails controllers---CRUD, requests, sessions, param shuffling, formats
  ~ `spec/controllers/posts_controller_spec.rb`

Feature (a.k.a. Acceptance) Specs
  ~ Test your Rails app from the browser as a user---use cases, interactions, and maybe even some JavaScript
  ~ `spec/features/post_management_spec.rb

In addition to those, while not covered here, you might also decide to write
unit tests for other parts of your app:

* library code
* helpers and concerns
* JavaScript


## Model specs

If you're familiar with unit testing, testing Rails models isn't much
different. Model tests---unlike controller and feature tests---don't require
much environment setup or mocking. This is one of the reasons it's a good idea
to move logic out of the controller and into the model (or maybe a service
object)---it's much easier to test that way.

In a model spec, I'll generally have expectations for validations and any
non-trivial class or instance methods. The goal of each example in a model spec
is to verify the behavior of the method or validation. When thinking about what
to test, we want to both make sure that it works in the normal case, as well as
in certain exceptional cases. Consider some example "what-ifs" when a plain old
Ruby method is called:

* If the method takes an argument, what happens if it's nil? 
* If the method takes an argument, what happens if the wrong type is passed or
  the argument is invalid in some other way (e.g., empty, wrong encoding)?
* If the method has any special behavior based on the arguments or instance state, 
  what happens when that special behavior runs? Does it work correctly?
* If a number is to be divided by another number, what if the divisor is 0?

While we won't (and can't) think of everything that could go wrong, it's
a good idea to touch on common failure points.

In these examples, imagine we're building an app to compare cars, trucks, and
SUVs. To that end, we'll have a `Vehicle` model with attributes like the
vehicle's make, model, year and style.

### Defining Factories

To make it easier to generate instances of our model in specs, we'll create
factories using the `factory_girl` gem. Here's an example factory definition
for our `Vehicle` model that we'll use in future examples: 

```ruby
# spec/factories/vehicles.rb
FactoryGirl.define do
  factory :vehicle do
    model   'Prius'
    make    'Toyota'
    year    2014
    style   'Car'

    trait :truck do
      model  'F-150'
      make   'Ford'
      style  'Truck'
    end

    trait :suv do
      model  'Escalade'
      make   'Cadilac'
      style  'SUV'
    end
  end
end
```

In the factory definition above, we describe how to create a vehicle by
assigning some default values for each attribute. To use the new factory, we'll
just need to call `build(:vehicle)` (to make an instance) or `create(:vehicle)`
(to make an instance and persist it to the data store) inside the specs. More
on that in a moment.

We also define two **traits**, one for trucks and one for SUVs. Traits allow
you to easily apply a group of attributes to your factory. How you use them is
largely up to you. In short, traits let us write this:

```ruby
create(:vehicle, :truck)
```

...instead of this:

```ruby
create(:vehicle, model: 'F-150', make: 'Ford', style: 'Truck')
```

One approach you might also consider is generating random data in your factories.
This had the advantage of making sure your app works with a broader range of inputs.
The `ffaker` gem is great for this purpose. In essence, `ffaker` generates fake data
for a number of common fields like names, phone numbers, addresses, and as it happens:
vehicles. Here's how we might re-define the vehicle factory with `ffaker`:

```ruby
# spec/factories/vehicles.rb
FactoryGirl.define do
  factory :vehicle do
    model   { Faker::Vehicle.model }
    make    { Faker::Vehicle.make  }
    year    { Faker::Vehicle.year  }
    style   'Car'
  end
end
```

Notice that we need to use blocks for these calls so that they're reevaluated
each time. To be completely correct here, you could add additional logic to
ensure that the make, model, year and style all agree. With this definition, we
might generate a 1950 Tesla F-150 car, but for some tests that's okay. Here's how
you could go improving this:

```ruby
factory :vehicle do
  model { Faker::Vehicle.model }
  make  { |vehicle| make_for_model(vehicle.model)  }
end
```

### Testing Validations

Imagine we'd like to require that all vehicles have a year. When users enter new 
vehicles without a year, validation should fail and they should be required to
enter it in order to continue. To make sure that happens, we'll need to ensure that
our validation in the `Vehicle` model is working correctly.

The model definition:

```ruby
# app/models/vehicle.rb
class Vehicle < ActiveRecord::Base
  validates :year, presence: true
end
```

The model spec:

```ruby
# spec/models/vehicle_spec.rb
require 'rails_helper'

describe Vehicle do
  it 'has a valid factory' do
    expect(build(:vehicle)).to be_valid
  end

  it 'is invalid without a year' do
    expect(build(:vehicle, year: nil)).to_not be_valid
  end
end
```

In case you're not familiar with RSpec's DSL, we'll take a quick detour and
cover the three important pieces here:

* `describe` defines a group of examples and takes the entity (e.g., model, method, etc.) 
  being specified.
* `it` defines an example and takes a description of the example. You'll want to phrase the
  description in a way that it reads like English (e.g., `it 'raises an exception when...'`).
* `expect` makes an expectation (a.k.a. an assertion in other testing frameworks) about
  its argument. The `expect` method takes an object or block and is typically
  used with `to` or `to_not` and a matcher (e.g., `be_valid`, `eq(42)`, or `raise_error`).

Going back to the example spec, our first example ("it has a valid factory")
is the control. We want to make sure that a model without a year is invalid,
but first we need to make sure that there are any valid inputs at all. Here we
verify that the model generated by our factory is valid.

In the second example, we build another model, but this time override the year
and set it to nil. With the year is missing, we expect the model to be invalid.


### Testing Methods

Imagine our app will display the average fuel-efficiency (MPG) for each vehicle
based on user-submitted values. There's now an `MpgSubmission` model and a
`has_many` relationship defined in the `Vehicle` model. To quickly get the
average MPG for a vehicle, we'll add an `average_mpg` method to the `Vehicle`
model that will average the MPG submissions. 

To make it interesting, we'll add an additional constraint: if there are fewer
than 10 submissions, the method should return nil to indicate insufficient data.

```ruby
# app/models/vehicle.rb
class Vehicle
  has_many :mpg_submissions

  MPG_SUBMISSIONS_NEEDED = 10

  def average_mpg
    if mpg_submissions.count >= MPG_SUBMISSIONS_NEEDED
      mpg_submissions.average(:mpg)
    else
      nil
    end
  end
end
```

```ruby
# spec/models/vehicle_spec.rb
require 'rails_helper'

describe Vehicle do
  subject { create(:vehicle) }

  describe '#average_mpg'
    it 'returns nil if there are fewer submissions than required'
      9.times.each { create(:mpg_submission, mpg: 25, vehicle: subject) }
      expect(subject.average_mpg).to be_nil
    end

    it 'returns the average if there are enough submissions'
      5.times.each { create(:mpg_submission, mpg: 25, vehicle: subject) }
      5.times.each { create(:mpg_submission, mpg: 30, vehicle: subject) }
      expect(subject.average_mpg).to eq(27.5)
    end
  end
end
```

I like to create a `describe` block for each method that will be tested, using
the syntax `#instance_method` and `.class_method` to indicate what is being
described. This helps with quickly associating example groups with the source
code being tested.

In the above spec, we test both sides of the if-expression---an example for
the case that there are fewer MPG submissions than required and an example
for the case that the requirement is met. In the latter case, we also verify
that the average agrees with our test data. 

While there may be other scenarios we could test here, these examples cover
this method fairly well---all branches are tested and there are no obvious nil
issues (`ActiveRecord::Calculations.average` will handle nil `mpg` values and we'd
likely have a database constraint to eliminate that possibility anyway).

## Controller specs

If most of the logic is kept out of your controllers, writing controller specs
is easy. Controller specs test your Rails application at the request level. Here
are some of the questions you should ask when testing controller actions:

* If the action should render a view, does it do so?
* If the action should redirect to another action, does it do so?
* If the action creates, updates, or deletes a resource, does this functionality work?
    * Specifically, how does creating, updating or deleting items affect the
      number of records after the request is completed?
* If the session and/or cookie should be updated, is that working correctly?
* If the request takes or requires certain parameters, what happens if these are
  missing or invalid?
* If access to the action is restricted, does the authentication and
  authorization logic work as expected? 
* Does the action return the correct status code? (important for json format)


```ruby
# app/controllers/vehicles_controller.rb
class VehiclesController < ApplicationController
  def create
    @vehicle = Vehicle.new(vehicle_params)
    
    respond_to do |format|
      if @vehicle.save
        format.html { redirect_to vehicle_path(@vehicle), notice: 'Vehicle was successfully created.' }
        format.json { render json: @vehicle, status: :created, location: @vehicle }
      else
        format.html { render action: 'new' }
        format.json { render json: @vehicle.errors, status: :unprocessable_entity }
      end
    end
  end

  # of course you'd probably define other actions as well...
end
```

```ruby
# spec/controllers/vehicles_controller_spec.rb
require 'rails_helper'

describe VehiclesController do
  describe 'POST #create' do
    context 'html'
      context 'with valid attributes' do
        it 'creates the vehicle' do
          post :create, vehicle: attributes_for(:vehicle)
          expect(Vehicle.count).to eq(1)
        end
  
        it 'redirects to the "show" action for the new vehicle' do
          post :create, vehicle: attributes_for(:vehicle)
          expect(response).to redirect_to Vehicle.first
        end
      end
  
      context 'with invalid attributes' do
        it 'does not create the vehicle' do
          post :create, vehicle: attributes_for(:vehicle, year: nil)
          expect(Vehicle.count).to eq(0)
        end
  
        it 're-renders the "new" view' do
          post :create, vehicle: attributes_for(:vehicle, year: nil)
          expect(response).to render_template :new
        end
      end
    end

    context 'json' do
      context 'with valid attributes' do
        it 'creates the vehicle' do
          post :create, vehicle: attributes_for(:vehicle), format: :json
          expect(Vehicle.count).to eq(1)
        end

        it 'responds with 201' do
          post :create, vehicle: attributes_for(:vehicle), format: :json
          expect(response.status).to eq(201)
        end
      end
  
      context 'with invalid attributes' do
        it 'does not create the vehicle' do
          post :create, vehicle: attributes_for(:vehicle, year: nil), format: :json
          expect(Vehicle.count).to eq(0)
        end

        it 'responds with 422' do
          post :create, vehicle: attributes_for(:vehicle, year: nil), format: :json
          expect(response.status).to eq(422)
        end
      end
    end
  end
end
```

There's a lot going on in the above specs. We use the `context` 


## Feature (a.k.a, Acceptance) specs

Feature specs are a type of integration test. Whereas unit tests are concerned with
individual components, integration tests focus on how they work together. An app
with unit tests but no integration tests is like a pile of individually-tested
car parts with no assurance that they can work together to make a drivable car.

In that sense, feature specs are the place to ensure that all the pieces of
your Rails app work together and achieve the functionality you built it for. In
feature specs, we assume the role of the user and play out various scenarios.
For example, some common scenarios in many applications:

* A user signs up for a new account.
* A user logs in to the site.
* A user submits a form (with or without all required fields).
* An admin manages content.

To write feature specs for Rails, we'll use Capybara, a Ruby gem which lets us
automate the browser to interact with web pages like a user would---that is, by
clicking, typing and selecting.

The following is a basic feature spec for logging in to the site. We'll visit
the sign in url, fill in credentials, and submit the form. To verify that we were
successful, we'll then look for a success message afterwards.

```ruby
# spec/features/user_sign_in_spec.rb

require 'rails_helper'

feature 'User signs in' do
  scenario 'with valid credentials' do
    visit sign_in_path
    fill_in 'Username', with: 'joe.example'
    fill_in 'Password', with: 'passw0rd' 
    click_on 'Sign In'

    expect(page).to have_content('You have successfully signed in!')
  end
end
```

With feature specs, it's important to maintain a certain level of abstraction.
If you've written good model and controller specs, you don't need to be
concerned with the low-level details here. While we might take a cursory glance
at the database to ensure that the expected change was actually made, we're
primarily concerned with the user seeing what they should be seeing. In this
case: seeing is believing.

### Interacting with JavaScript

It's likely that at one point or another, your feature specs will need to
interact with JavaScript-dependent functionality---maybe there's a confirmation
dialog that needs to be accepted or content that is loaded dynamically.

Although Capybara's default `Rack::Test` driver does not support JavaScript,
it's easy enough to switch to one that does for feature specs requiring it. You
can read more about the available drivers [here][1]. If you plan to run specs
on a server without X11 (or an alternative) installed, PhantomJS via
Poltergeist is a good choice.

The below example, while contrived, demonstrates how expectations can be made about
dynamic content, as well as how JavaScript code can be executed from the
feature spec.

```ruby
feature 'User views dynamic Hello World message' do
  it 'displays Hello World', js: true do
    visit root_path
    page.execute_script "document.write('Hello World!')" 
    expect(page).to have_content('Hello World!')
  end
end
```

As an aside, trying to test JS-heavy apps from Capybara can unfortunately be
a world of pain. As the setup typically involves multiple threads or
processes---your Rails app being one and the browser being the other---it leads
to all sorts of timing issues. Race conditions, driver differences, and random
AJAX failures all conspire to make these tests brittle and hard to debug. I've
started to avoid these types of tests altogether and rely instead on
client-side testing with [Mocha](http://mochajs.org/) and
[Karma](http://karma-runner.github.io/0.12/index.html) for JS-heavy pages.

## Wrapping Up

One thing you may have noticed is that controller specs have a good deal of
overlap with model and feature specs. While there are advantages to having
controller specs, I've found that most things they test can be covered by
feature specs. If you only have time for one or the other, go with feature
specs. They prove that your app works in an actual browser---rather than just
from a cURL client, which is as far as controller specs will get you.

## More Resources

* [Better Specs](http://betterspecs.org/) --- Best practices for RSpec. You may
  not always agree with the advice here, but I've generally found it to be pretty
  solid.
* [Everyday Rails: How I learned to test my Rails applications](http://everydayrails.com/2012/03/12/testing-series-intro.html) ---
  a great series that goes more in depth than I could here.
* [Thoughtbot's Blog](http://robots.thoughtbot.com/tags/testing) --- Excellent
  posts on many of the concepts and technologies covered here with good advice
  on overcoming the various hurdles you'll run up against.

[1]: https://github.com/jnicklas/capybara#drivers
