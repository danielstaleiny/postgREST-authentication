'use strict'
var Control_Applicative = require('../Control.Applicative/index.js')
var Control_Bind = require('../Control.Bind/index.js')
var Data_Functor = require('../Data.Functor/index.js')
var Data_Maybe = require('../Data.Maybe/index.js')
var Data_Show = require('../Data.Show/index.js')
var Effect = require('../Effect/index.js')
var Effect_Aff = require('../Effect.Aff/index.js')
var Effect_Class = require('../Effect.Class/index.js')
var Effect_Console = require('../Effect.Console/index.js')
var Web_DOM_DOMTokenList = require('../Web.DOM.DOMTokenList/index.js')
var Web_DOM_Element = require('../Web.DOM.Element/index.js')
var Web_DOM_Node = require('../Web.DOM.Node/index.js')
var Web_DOM_NonElementParentNode = require('../Web.DOM.NonElementParentNode/index.js')
var Web_HTML = require('../Web.HTML/index.js')
var Web_HTML_HTMLDocument = require('../Web.HTML.HTMLDocument/index.js')
var Web_HTML_HTMLElement = require('../Web.HTML.HTMLElement/index.js')
var Web_HTML_Window = require('../Web.HTML.Window/index.js')
var toggleHidden = function (elem) {
  var htmlele_ = Web_HTML_HTMLElement.fromElement(elem)
  if (htmlele_ instanceof Data_Maybe.Nothing) {
    return Effect_Console.log("Didn't find element")
  }
  if (htmlele_ instanceof Data_Maybe.Just) {
    return function __do() {
      var b = Web_HTML_HTMLElement.hidden(htmlele_.value0)()
      var a = (function () {
        if (b) {
          return Web_HTML_HTMLElement.setHidden(false)(htmlele_.value0)()
        }
        if (!b) {
          return Web_HTML_HTMLElement.setHidden(true)(htmlele_.value0)()
        }
        throw new Error(
          'Failed pattern match at Main (line 85, column 27 - line 86, column 73): ' +
            [b.constructor.name]
        )
      })()
      return a
    }
  }
  throw new Error(
    'Failed pattern match at Main (line 82, column 3 - line 87, column 28): ' +
      [htmlele_.constructor.name]
  )
}
var toggleHidden_ = function (v) {
  if (v instanceof Data_Maybe.Nothing) {
    return Effect_Console.log("Didn't find element")
  }
  if (v instanceof Data_Maybe.Just) {
    return toggleHidden(v.value0)
  }
  throw new Error(
    'Failed pattern match at Main (line 89, column 1 - line 89, column 46): ' +
      [v.constructor.name]
  )
}

// fadeIn_ elem_ -- Maybe Effect unit
// elem1 <- fadeIn <$> elem
// fromMaybe "ok" <$> elem1 >>> pure
// Console.log
var getById = function (fallback) {
  return function (id) {
    return Control_Bind.bind(Effect.bindEffect)(
      Control_Bind.bind(Effect.bindEffect)(
        Control_Bind.bind(Effect.bindEffect)(
          Control_Bind.bind(Effect.bindEffect)(Web_HTML.window)(
            Web_HTML_Window.document
          )
        )(
          (function () {
            var $15 = Control_Applicative.pure(Effect.applicativeEffect)
            return function ($16) {
              return $15(Web_HTML_HTMLDocument.toNonElementParentNode($16))
            }
          })()
        )
      )(Web_DOM_NonElementParentNode.getElementById(id))
    )(
      (function () {
        var $17 = Data_Maybe.fromMaybe(
          Control_Applicative.pure(Effect.applicativeEffect)(fallback)
        )
        var $18 = Data_Functor.map(Data_Maybe.functorMaybe)(
          Web_DOM_Node.textContent
        )
        var $19 = Data_Functor.map(Data_Maybe.functorMaybe)(
          Web_DOM_Element.toNode
        )
        return function ($20) {
          return $17($18($19($20)))
        }
      })()
    )
  }
}

// import Web.DOM.Document (createElement)
// >>= bind
// <$> map or fmap from haskell
// >>> pipe
// String <- Effect String
// name <- value
// M a | a = String, Maybe etc.
// let name = value
// getJson = unit
// post = unit
// get = unit
//
// void :: forall f a. Functor f => f a -> f Unit
//
var fadeToggle = function (elem) {
  return function __do() {
    var classList = Web_DOM_Element.classList(elem)()
    var bool = Web_DOM_DOMTokenList.toggle(classList)('opacity-0')()
    return Effect_Console.logShow(Data_Show.showBoolean)(bool)()
  }
}

// toggle element add or remove element and return bool value if it added or remove it.
// remove false
// added true
var fadeToggle_ = function (v) {
  if (v instanceof Data_Maybe.Nothing) {
    return Effect_Console.log("couldn't find it")
  }
  if (v instanceof Data_Maybe.Just) {
    return fadeToggle(v.value0)
  }
  throw new Error(
    'Failed pattern match at Main (line 54, column 1 - line 54, column 44): ' +
      [v.constructor.name]
  )
}

// expect to have opacity-0 on element
// alternatively add opacity-100 instead
// test if you have opacity-0 and opacity-100 what has preccedance.
// el.classList.add('transition-opacity');
// el.classList.add('duration-300');
// el.classList.remove('opacity-0');
// resolve Nothing = pure "ok"
// resolve _ = pure "ok"
var main = Effect_Aff.launchAff_(
  Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(function __do() {
    var doc = Control_Bind.bind(Effect.bindEffect)(Web_HTML.window)(
      Web_HTML_Window.document
    )()
    var elem_ = Web_DOM_NonElementParentNode.getElementById('hide-elem')(
      Web_HTML_HTMLDocument.toNonElementParentNode(doc)
    )()
    fadeToggle_(elem_)()
    return toggleHidden_(elem_)()
  })
)
var fadeOut = function (elem) {
  return function __do() {
    var classList = Web_DOM_Element.classList(elem)()
    return Web_DOM_DOMTokenList.add(classList)('opacity-0')()
  }
}
var fadeOut_ = function (v) {
  if (v instanceof Data_Maybe.Nothing) {
    return Effect_Console.log("couldn't find it")
  }
  if (v instanceof Data_Maybe.Just) {
    return fadeOut(v.value0)
  }
  throw new Error(
    'Failed pattern match at Main (line 74, column 1 - line 74, column 41): ' +
      [v.constructor.name]
  )
}
var fadeIn = function (elem) {
  return function __do() {
    var classList = Web_DOM_Element.classList(elem)()
    return Web_DOM_DOMTokenList.remove(classList)('opacity-0')()
  }
}
var fadeIn_ = function (v) {
  if (v instanceof Data_Maybe.Nothing) {
    return Effect_Console.log("couldn't find it")
  }
  if (v instanceof Data_Maybe.Just) {
    return fadeIn(v.value0)
  }
  throw new Error(
    'Failed pattern match at Main (line 64, column 1 - line 64, column 40): ' +
      [v.constructor.name]
  )
}
module.exports = {
  fadeToggle: fadeToggle,
  fadeToggle_: fadeToggle_,
  fadeIn: fadeIn,
  fadeIn_: fadeIn_,
  fadeOut: fadeOut,
  fadeOut_: fadeOut_,
  toggleHidden: toggleHidden,
  toggleHidden_: toggleHidden_,
  main: main,
  getById: getById,
}
