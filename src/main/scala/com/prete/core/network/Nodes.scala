package com.prete.core.network

import com.prete.core.fact.FactInstance

object Nodes {

  trait Memory

  trait NodeNotifier {
    protected var subscribers: List[NodeSubscriber] = List.empty

    def subscribe(s: NodeSubscriber): Unit = subscribers = subscribers :+ s

    def unsubscribe(s: NodeSubscriber): Unit = {
      val index = subscribers.indexOf(s)
      if (index >= -1) {
        val pair = subscribers.splitAt(index)
        subscribers = pair._1 ++ pair._2.tail
      }
    }

    def notify(value: Any): Unit = subscribers.foreach(_.notify(this, value))
  }

  trait NodeSubscriber {
    def notify(sender: NodeNotifier, value: Any): PartialFunction[Any, Any]
  }

  trait AlphaNode {
    val input: AlphaMemory
  }

  trait AlphaMemory extends AlphaNode with Memory with NodeNotifier

  trait BetaNode extends NodeSubscriber with NodeNotifier

  trait BetaMemory extends AlphaNode with Memory with NodeSubscriber {

  }


  class TypeFilterNode(val input: AlphaMemory, typeName: String) extends AlphaNode with NodeSubscriber {
    input.subscribe(this)

    def notify(sender: NodeNotifier, value: Any) = {
      case f@FactInstance(name, _) if name == typeName => f
    }
  }

}