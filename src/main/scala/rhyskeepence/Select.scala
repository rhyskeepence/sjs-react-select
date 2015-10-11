package rhyskeepence

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom.raw.{HTMLElement, HTMLInputElement}

import scala.scalajs.js.timers._

object Select {
  type EventHandler = ReactEventI => Unit

  val inputRef = Ref[HTMLInputElement]("inputRef")
  val menuRef = Ref[HTMLElement]("menuRef")
  val focusedRef = Ref[HTMLElement]("focusedRef")

  case class Props[A](selectClass: String,
                      placeholder: String,
                      initialValue: Option[A],
                      allValues: List[A],
                      query: String => List[A],
                      onChange: Option[A] => Unit,
                      renderTitle: A => String,
                      renderValue: A => ReactElement,
                      keyOf: A => String)

  case class State[A](inputValue: String,
                      isOpen: Boolean,
                      isFocused: Boolean,
                      filteredValues: List[A],
                      focusedValue: Option[A],
                      selectedValue: Option[A])

  case class Backend[A]($: BackendScope[Props[A], State[A]]) {


    def fireChangeEvent(newSelectedValue: Option[A]): Unit = {
      if ($.state.selectedValue != newSelectedValue)
        $.props.onChange(newSelectedValue)
      else
        ()
    }

    def selectValue(value: A): Unit = {
      $.modState(_.copy(
        isOpen = false,
        isFocused = false,
        selectedValue = Some(value),
        inputValue = ""
      ))
    }

    def selectFocusedValue(): Unit = {
      if ($.state.isOpen)
        $.state.focusedValue.foreach(selectValue)
      else
        ()
    }

    def clearValueIfOpen(): Unit = {
      if ($.state.isOpen) clearValue()
      else ()
    }

    def clearValue(): Unit = {
      $.modState(_.copy(
        selectedValue = None
      ))
    }

    def handleInputFocus(event: ReactEventI): Unit = {
      $.modState(_.copy(
        isOpen = true,
        isFocused = true
      ))
    }

    def handleInputBlur(event: ReactEventI): Unit = {
      setTimeout(50) {
        $.modState(_.copy(
          isOpen = false,
          isFocused = false
        ))
      }
    }

    def handleInputChange(event: ReactEventI): Unit = {
      val inputValue = event.target.value

      val newFilteredValues =
        if (inputValue.trim != "") $.props.query(inputValue)
        else $.props.allValues

      val newFocusedValue =
        if ($.state.focusedValue.exists(newFilteredValues.contains))
          $.state.focusedValue
        else
          newFilteredValues.headOption

      $.modState(_.copy(
        inputValue = inputValue,
        isOpen = true,
        filteredValues = newFilteredValues,
        focusedValue = newFocusedValue
      ))
    }

    def focusPreviousOption(): Unit = {
      focusAdjacentOption(-1)
    }

    def focusNextOption(): Unit = {
      focusAdjacentOption(1)
    }

    def focusAdjacentOption(index: Int): Unit = {

      val default =
        if ($.state.filteredValues.isEmpty)
          None
        else if (index > 0)
          Some(0)
        else
          Some($.state.filteredValues.length - 1)

      val adjacentIndex =
        $.state.focusedValue.map(a => {
          val currentIndex = $.state.filteredValues.indexOf(a)

          if ($.state.filteredValues.isDefinedAt(currentIndex + index))
            Some(currentIndex + index)
          else
            default
        }).getOrElse({
          default
        })

      if ($.state.isOpen)
        $.modState(_.copy(
          focusedValue = adjacentIndex.map($.state.filteredValues)
        ))
      else
        $.modState(_.copy(
          focusedValue = adjacentIndex.map($.state.filteredValues),
          isOpen = true,
          inputValue = ""
        ))
    }

    def revealFocusedOption(): Unit = {
      focusedRef($).map(focused => {
        menuRef($).map(menu => {
          val focusedDom = focused.getDOMNode()
          val focusedRect = focusedDom.getBoundingClientRect()
          val menuDom = menu.getDOMNode()
          val menuRect = menuDom.getBoundingClientRect()

          println(focusedRect.bottom)
          println(menuRect.bottom)

          if (focusedRect.bottom > menuRect.bottom || focusedRect.top < menuRect.top)
            menuDom.scrollTop = focusedDom.offsetTop + focusedDom.clientHeight - menuDom.offsetHeight
        })
      })
    }


    def handleKeyDown(event: ReactKeyboardEvent): Unit = {
      event.nativeEvent.keyCode match {
        case 9 => // tab
          selectFocusedValue()
          event.preventDefault()

        case 13 => // enter
          selectFocusedValue()
          event.preventDefault()

        case 27 => // escape
          clearValueIfOpen()
          event.preventDefault()

        case 38 => // up
          focusPreviousOption()
          event.preventDefault()

        case 40 => // down
          focusNextOption()
          event.preventDefault()

        case _ =>
          ()
      }
    }

    def handleMouseDown(event: ReactMouseEvent): Unit = {
      event.stopPropagation()
      event.preventDefault()

      if ($.state.isFocused)
        $.modState(_.copy(
          isOpen = true
        ))
      else
        focusInput()
    }

    def focusInput(): Unit = {
      inputRef($).tryFocus()
    }

    def focusValue(value: A): Unit = {
      $.modState(_.copy(
        focusedValue = Some(value)
      ))
    }

    def unfocusValue(value: A): Unit = {
      if ($.state.focusedValue == value)
        $.modState(_.copy(focusedValue = None))
      else ()
    }

    def buildMenu() = {
      $.state.filteredValues.map(a => {
        val isSelected = $.state.selectedValue.contains(a)
        val isFocused = $.state.focusedValue.contains(a)

        val classNames = "Select-option" +
          (if (isSelected) " is-selected" else "") +
          (if (isFocused) " is-focused" else "")

        SelectOption(
          className = classNames,
          mouseEnter = () => focusValue(a),
          mouseLeave = () => unfocusValue(a),
          mouseDown = () => selectValue(a),
          key = $.props.keyOf(a),
          focused = isFocused,
          value = $.props.renderValue(a)
        )
      })
    }

    def render = {
      val value: TagMod =
        if ($.state.inputValue.trim() == "")
          $.state.selectedValue.map($.props.renderTitle).fold(
            <.div(^.className := "Select-placeholder", ^.title := "", $.props.placeholder)
          )(selectedValue =>
            <.div(^.className := "Select-value", ^.title := selectedValue, selectedValue)
          )
        else
          <.div()

      val input =
        <.div(
          ^.className := "Select-input",
          ^.onFocus ==> handleInputFocus,
          ^.onBlur ==> handleInputBlur,
          <.input(
            ^.`type` := "text",
            ^.className := "",
            ^.ref := inputRef,
            ^.onChange ==> handleInputChange,
            ^.value := $.state.inputValue))

      val clear =
        <.span(
          ^.className := "Select-clear",
          ^.title := "",
          ^.onMouseDown --> clearValue,
          ^.onClick --> clearValue, "×")

      val menu =
        $.state.isOpen ?=
          <.div(
            ^.className := "Select-menu-outer",
            <.div(
              ^.className := "Select-menu",
              ^.ref := menuRef,
              buildMenu()))

      val classNames = "Select " + $.props.selectClass +
        (if ($.state.isOpen) " is-open" else "") +
        (if ($.state.isFocused) " is-focused" else "") +
        (if ($.state.selectedValue.nonEmpty) " has-value" else "")

      <.div(^.className := classNames,
        <.div(^.className := "Select-control", ^.onKeyDown ==> handleKeyDown, ^.onMouseDown ==> handleMouseDown,
          value,
          input,
          clear
        ),
        menu)
    }
  }

  def apply[A](selectClass: String, placeholder: String, initialValue: Option[A], values: List[A], query: String => List[A], onChange: Option[A] => Unit, renderTitle: A => String, renderValue: A => ReactElement, keyOf: A => String) = {
    ReactComponentB[Props[A]]("Select")
      .getInitialState(props => State[A](props.initialValue.map(props.renderTitle).getOrElse(""), isOpen = false, isFocused = false, props.allValues, None, props.initialValue))
      .backend(Backend.apply)
      .render(_.backend.render)
      .componentDidUpdate((scope, _, _) => scope.backend.revealFocusedOption())
      .build
      .apply(Props[A](selectClass, placeholder, initialValue, values, query, onChange, renderTitle, renderValue, keyOf))
  }
}

object SelectOption {
  case class Props(className: String, mouseEnter: () => Unit, mouseLeave: () => Unit, mouseDown: () => Unit, focused: Boolean, value: ReactElement)

  case class Backend($: BackendScope[Props, Unit]) {
    def render =
      <.div(
        ^.className := $.props.className,
        ^.onMouseOver --> $.props.mouseEnter(),
        ^.onMouseOut --> $.props.mouseLeave(),
        ^.onMouseDown --> $.props.mouseDown(),
        ^.onClick --> $.props.mouseDown(),
        $.props.value)
  }

  def apply(className: String, mouseEnter: () => Unit, mouseLeave: () => Unit, mouseDown: () => Unit, key: String, focused: Boolean, value: ReactElement) =
    ReactComponentB[Props]("SelectOption")
      .stateless
      .backend(Backend.apply)
      .render(_.backend.render)
      .build
      .withKey(key)
      .withRef(if (focused) "focusedRef" else null)
      .apply(Props(className, mouseEnter, mouseLeave, mouseDown, focused, value))
}