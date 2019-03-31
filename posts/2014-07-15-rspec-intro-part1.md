---
title: "Testing Rails apps with RSpec: Part I"
---

In this two-part series, I'll cover testing a Rails application---from how to
get set up with the latest toolchain, to writing those first tests.

<!--more-->

In Part 1, I'll look at how to set up and configure [RSpec 3][6] (with [Factory
Girl][7] and [Capybara][8]) for a Rails 4 app, and how to generate and run your
first specs. Next time, in Part 2, we'll look at writing model, controller and
feature specs.

## But first, why write tests?

I was initially very wary of unit-testing. While I knew that I needed to make
sure the app worked, I was skeptical of the time tradeoff. The first few tests can
seem like more trouble than they're worth, and quite honestly, those first few
probably are more trouble than they're worth. But after a while, writing tests
becomes second nature. I think it can easily take less time to write automated
tests than to manually test every feature before every release.

It's no silver bullet. There will probably still be bugs from scenarios you
never thought to test. But even if they can't catch every bug, tests provide
you with confidence that your app works as intended on a basic level. This is
all the more important if you didn't write the app to begin with, or if you're
part of a big team---you won't always know what effects your changes might
have. There might be features you don't even know about. Ultimately, a solid
test suite means less to worry about for you, the developer, and with any luck,
a more stable product for the user.

## Testing a Rails 4 app with RSpec 3

I really like RSpec. If you're more of a minimalist, you can always skip the
extra gems and use the built-in minitest, but I like the additional features and
clean DSL that RSpec provides. For example, to test that a variable is nil:

```ruby
# minitest
assert_nil obj

# RSpec
expect(obj).to be_nil
```

It's a matter of taste, but I prefer RSpec's more human-readable style. Here,
we'll use RSpec 3.0, which landed back in May, and brings a few new changes
into the fold (See here for more info: [Notable Changes in RSpec 3][1]).

In addition to RSpec, we'll also set up Factory Girl as a fixture replacement,
Capybara for browser-level acceptance (a.k.a. integration) tests, and Database
Cleaner for cleaning up between test scenarios.

When you write tests, you'll need test data. As a fixture replacement, **Factory
Girl** helps you build instances of your models for use in test scenarios. This
is accomplished through factories in which you define how to build the instances.
Although it's nothing you couldn't do yourself with plain old ActiveRecord
methods and some helper methods, Factory Girl provides a nice system for setting defaults
(and then overriding them) that keeps a lot of boilerplate out of your code.

Acceptance tests look at how the system as a whole functions. For a Rails app,
that usually means typing and clicking on things in a web browser and seeing
some sort of response. **Capybara** provides one DSL for automating various
browsers to simulate user interactions with your app, which you can then make
expectations about using RSpec. Because it provides a single interface to
various browsers, it gives you the freedom to do things like use Selenium to run
certain tests while using headless webkit to run others.

By default, test scenarios are wrapped in a database transaction so that each
scenario's changes can be rolled back, so as to not cause side effects when
running other scenarios later. But when using Capybara's JavaScript driver,
this causes some trouble, as these scenarios are run in another thread which
doesn't share the database connection, leading to unexpected behavior. To avoid
headaches down the road, we'll also use the **`database_cleaner`** gem to
adjust the way the database cleanup is done. (See [this post][2] for more
details.)

## Configure your Gemfile

Let's get started. Assuming you're working inside a new Rails 4 app, add the
following to your `Gemfile` and then `bundle install`.

```ruby
group :development, :test do
  gem 'rspec-rails', '~> 3.0.0'
  gem 'factory_girl_rails'
  gem 'capybara'
  gem 'database_cleaner'
end
```

## Run generators

Next, run the RSpec generator to create the initial skeleton:

```
rails generate rspec:install
```

## Configure Capybara

To make Capybara available from within RSpec specs, add the following line to `spec/rails_helper.rb`:

```ruby
# spec/rails_helper.rb
require 'capybara/rails'
```

Your Capybara feature specs will also need a home, add the `spec/features/` directory:

```
mkdir spec/features
```

## Configure `database_cleaner`

Next, make the following adjustments to `spec/rails_helper.rb` to integrate the `database_cleaner` gem:

```ruby
config.use_transactional_fixtures = false
```

```ruby
RSpec.configure do |config|

  config.before(:suite) do
    DatabaseCleaner.clean_with(:truncation)
  end

  config.before(:each) do
    DatabaseCleaner.strategy = :transaction
  end

  config.before(:each, :js => true) do
    DatabaseCleaner.strategy = :truncation
  end

  config.before(:each) do
    DatabaseCleaner.start
  end

  config.after(:each) do
    DatabaseCleaner.clean
  end

end
```

## The `.rspec` file

By default, RSpec generates a `.rspec` file in your app's root. This allows you
to set different command line options (see `rspec -h`) that will be
automatically picked up. The default `.rspec` includes the `--warnings` option,
which can be really noisy even in a brand new Rails app. If you see a lot of
warnings later on, you can hide them by removing this line.

## Generating specs

The next time you run `rails generate resource`, Rails should already be
configured to generate specs under `spec/` and factories under
`spec/factories`.

Let's try that out with the proverbial Rails blog example:

```
rails generate resource post title:string content:text published:boolean
```

If everything's correctly configured, you should see something close to this:

```
invoke  active_record
create    db/migrate/20140630160246_create_posts.rb
create    app/models/post.rb
invoke    rspec
create      spec/models/post_spec.rb
invoke      factory_girl
create        spec/factories/posts.rb
invoke  controller
create    app/controllers/posts_controller.rb
invoke    erb
create      app/views/posts
invoke    rspec
create      spec/controllers/posts_controller_spec.rb
invoke    helper
create      app/helpers/posts_helper.rb
invoke      rspec
create        spec/helpers/posts_helper_spec.rb
invoke    assets
invoke      coffee
create        app/assets/javascripts/posts.js.coffee
invoke      scss
create        app/assets/stylesheets/posts.css.scss
invoke  resource_route
 route    resources :posts
```

Note the factory at `spec/factories/posts.rb`, the model spec at
`specs/models/post_spec.rb`, and the controller spec at
`specs/controllers/post_controller_spec.rb`.

With the new specs in place, try running `rspec` (you can also run `rake`), to
see the results. So far, you should only see a few pending specs:

```
**

Pending:
  PostsHelper add some examples to (or delete) ./spec/helpers/posts_helper_spec.rb
    # Not yet implemented
    # ./spec/helpers/posts_helper_spec.rb:14
  Post add some examples to (or delete) ./spec/models/post_spec.rb
    # Not yet implemented
    # ./spec/models/post_spec.rb:4
```

## Wrapping Up

Now that everything is set up, we're ready to write some tests. Stay tuned for Part 2.
In the meantime, here are a few more resources worth a read:

- [A Guide to Testing Rails Application][3] --- the Rails Guide to testing
- Thoughbot's *[How We Test Rails Applications][4]*
- DHH's *[TDD is dead. Long live testing.][5]*

[1]: http://myronmars.to/n/dev-blog/2014/05/notable-changes-in-rspec-3
[2]: http://devblog.avdi.org/2012/08/31/configuring-database_cleaner-with-rails-rspec-capybara-and-selenium/
[3]: http://guides.rubyonrails.org/testing.html
[4]: http://robots.thoughtbot.com/how-we-test-rails-applications
[5]: http://david.heinemeierhansson.com/2014/tdd-is-dead-long-live-testing.html
[6]: https://github.com/rspec/rspec-rails
[7]: https://github.com/thoughtbot/factory_girl_rails
[8]: https://github.com/jnicklas/capybara
