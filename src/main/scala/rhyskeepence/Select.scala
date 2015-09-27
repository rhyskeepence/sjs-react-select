package rhyskeepence

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._

import scala.scalajs.js.timers._

object Select {
  type EventHandler = ReactEventI => Unit

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
        selectedValue = Some(value)
      ))
    }

    def selectFocusedValue(): Unit = {
      if ($.state.isOpen)
        $.state.focusedValue.foreach(selectValue)
      else
        ()
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
      val previousIndex =
        $.state.focusedValue.map(a => {
          val index = $.state.filteredValues.indexOf(a)

          if ($.state.filteredValues.isDefinedAt(index - 1)) index - 1
          else $.state.filteredValues.length - 1
        }).getOrElse({
          $.state.filteredValues.length - 1
        })

      $.state.filteredValues(previousIndex)
    }

    def focusNextOption(): Unit = {
      val nextIndex =
        $.state.focusedValue.map(a => {
          val index = $.state.filteredValues.indexOf(a)

          if ($.state.filteredValues.isDefinedAt(index + 1)) index + 1
          else 0
        }).getOrElse({
          0
        })

      $.state.filteredValues(nextIndex)
    }

    def handleKeyDown(event: ReactKeyboardEvent): Unit = {
      event.nativeEvent.keyCode match {
        case 8 => // backspace
          clearValue()
        case 9 => // tab
          selectFocusedValue()
        case 13 => // enter
          selectFocusedValue()
        case 27 => // escape
          clearValue()
        case 38 => // up
          focusPreviousOption()
        case 40 => // down
          focusNextOption()
        case _ =>
          ()
      }
      event.preventDefault()
    }

    def handleMouseDown(event: ReactMouseEvent): Unit = {
      event.stopPropagation()
      event.preventDefault()

      $.modState(_.copy(
        isOpen = true
      ))
    }

    def focusValue(value: A): Unit = {
      println("focus " + value)
      $.modState(_.copy(
        focusedValue = Some(value)
      ))
    }

    def unfocusValue(value: A): Unit = {
      println("unfocus "+ value)
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
          value = $.props.renderValue(a)
        )
      })
    }

    def render = {
      val value: ReactTag =
        $.state.selectedValue.map($.props.renderTitle).fold(
          <.div(^.className := "Select-placeholder", ^.title := "", $.props.placeholder)
        )(selectedValue =>
          <.div(^.className := "Select-value", ^.title := selectedValue, selectedValue)
        )
      val input = <.div(^.className := "Select-input", ^.onFocus ==> handleInputFocus, ^.onBlur ==> handleInputBlur, <.input(^.`type` := "text", ^.className := "", ^.onChange ==> handleInputChange, ^.value := $.state.inputValue))
      val clear = <.span(^.className := "Select-clear", ^.title := "", ^.onMouseDown --> clearValue, ^.onClick --> clearValue, "×")
      val menu = $.state.isOpen ?= <.div(^.className := "Select-menu-outer", <.div(^.className := "Select-menu", buildMenu()))

      <.div(^.className := "Select " + $.props.selectClass,
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
      .build
      .apply(Props[A](selectClass, placeholder, initialValue, values, query, onChange, renderTitle, renderValue, keyOf))
  }
}

object SelectOption {
  case class Props(className: String, mouseEnter: () => Unit, mouseLeave: () => Unit, mouseDown: () => Unit, value: ReactElement)

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

  def apply(className: String, mouseEnter: () => Unit, mouseLeave: () => Unit, mouseDown: () => Unit, key: String, value: ReactElement) =
    ReactComponentB[Props]("SelectOption")
      .stateless
      .backend(Backend.apply)
      .render(_.backend.render)
      .build
      .withKey(key)
      .apply(Props(className, mouseEnter, mouseLeave, mouseDown, value))
}