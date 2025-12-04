**Status**: 🚧 [Scrappy Fiddle](https://dundalek.com/entropic/scrappy-fiddles) 🚧 incomplete demo exploring abstractions, further work needed to make it into usable library

Dealing with concurrent async operations is challenging.
The [talk from EmberConf](https://www.youtube.com/watch?v=VEzVDOmY-dc) by the [ember-concurrency](https://ember-concurrency.com/docs/introduction) author describes an [example](https://ember-concurrency.com/docs/tutorial/) how even basic data loading in a web app requires careful considerations of many details to make it behave correctly.

This is an implementation of [ember-concurrency](https://github.com/machty/ember-concurrency) ideas in Clojure/Script.

The [online demo](https://scrappy-concurrency.pages.dev/) showcases examples from ember-concurrency [reference docs](https://ember-concurrency.com/docs/task-function-syntax).

Demo of the [Happy Eyeballs](https://en.wikipedia.org/wiki/Happy_Eyeballs) algorithm:

[screenshot here]

## Concepts

Instead of introducing custom task abstraction, it builds on [leonoel task](https://github.com/leonoel/task) which defines a task as a 2-arity function.
This pairs well with [missionary](https://github.com/leonoel/missionary), but it does not depend on it.

Instead of [Task Modifiers](https://ember-concurrency.com/docs/task-concurrency) there are executors managing tasks:

- unbound -> unbounded
- restartable
- enqueue -> enqueued
- drop -> dropping
- keepLatest -> keeping-latest

Instead of [Properties](https://ember-concurrency.com/docs/derived-state) there are state tracking wrappers (should we call them trackers or monitors?) which manage atom, the app can add watches to subscribe to changes.
These provide attributes like `:started?`,
`:canceled?`,
`:error?`,
`:finished?`,
`:successful?`.

Example defining using a task:

```clojure
(defnc DefiningTasks []
  (let [[status set-status] (hooks/use-state nil)
        [perform] (hooks/use-state #(core/unbounded))
        task (m/sp
              (set-status "Gimme one second...")
              (m/? (m/sleep 1000))
              (set-status "Gimme one more second...")
              (m/? (m/sleep 1000))
              (set-status "OK, I'm done."))]
    (d/div
     (d/button {:on-click #(perform task)}
               "Wait A Few Seconds")
     (d/span status))))
```

## TODOs / Limitations

- Unfinished demo [derived-state](https://ember-concurrency.com/docs/derived-state)
 - Introduce trackers for executors similarly to how trackers work on instances?
- Add locking for Clojure on JVM support
  - the current implementation is in cljc files, but JVM si used only to run tests for code coverage reporting. It is not thread-safe.
- Polish API
  - could get inspired by [ExecutorService](https://docs.oracle.com/en/java/javase/21/docs/api/java.base/java/util/concurrent/ExecutorService.html) which seems to be a similar concept, has submit method
  - ensure good interop (provide just examples?) with Promises, fetch API, CompleatableFuture, etc.
- Is the trackers good abstraction/concept? monitors? wrappers/middlewares?
- [Structured Concurrency](https://vorpus.org/blog/notes-on-structured-concurrency-or-go-statement-considered-harmful/) - can our executors be considered Nurseries? are the some functions we need to implement to be considered a Structured Concurrency implementation?

## Development

Install dependencies

```
npm install
```

Run dev build in watch mode and open [localhost:8888](http://localhost:8888):

```
bb dev
```

### Testing

While dev process is running open http://localhost:8888/test for cljs test runner.

Run cljs tests from the CLI:

```
clojure -M:shadow -m shadow.cljs.devtools.cli compile ci
CHROME_BIN=brave npm run test
```

Run tests on the JVM:

```
bb test
```

Generate test coverage report:

```
bb test:coverage
```
