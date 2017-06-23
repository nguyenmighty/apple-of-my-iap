package com.meetup.iap

import unfiltered.filter.Planify
import unfiltered.request._
import unfiltered.response._
import unfiltered.directives._

import org.json4s.JsonAST.{JString, JInt}
import org.json4s.JsonDSL._
import org.json4s.{DefaultFormats, JValue}
import org.json4s.native.JsonMethods._
import org.json4s.native.Serialization.writePretty

import scala.util.Try
import scala.io.Source
import com.meetup.iap.receipt.{ReceiptRenderer, ReceiptGenerator}
import org.slf4j.LoggerFactory
import com.meetup.iap.AppleApi.ReceiptResponse

object IAPPlan {
  implicit val formats = DefaultFormats
  val log = LoggerFactory.getLogger(IAPPlan.getClass)

  private def getOrBad[T](opt: Option[T]) = Directives.getOrElse(opt, BadRequest)

  private def getProductId(json: JValue) = {
    val productId: Option[String] =
      json \ "productId" match {
        case JString(id) => Some(id)
        case _ => None
      }

    getOrBad(productId)
  }

  private def getIntFromString(code: String) = {
    getOrBad(Try(code.toInt).toOption)
  }

  private def getStatusCode(json: JValue) = {
    val statusCode: Option[Int] =
      json \ "status" match {
        case JString(id) => Try(id.toInt).toOption
        case JInt(id) => Some(id.toInt)
        case _ => None
      }

    getOrBad(statusCode)
  }

  private def getReceiptData(json: JValue) = {
    val receiptOpt = json \ "receipt-data" match {
      case JString(receipt) => Some(receipt)
      case _ => None
    }

    getOrBad(receiptOpt)
  }

  def plan = Planify { Directive.Intent {
    case GET(Path("/")) => Directives.success(page)

    case GET(Path("/plans")) => Directives.success {
      val json = pretty(render(
        Biller.plans.map { plan =>
          ("productId" -> plan.productId) ~
            ("name" -> plan.name) ~
            ("description" -> plan.description)
        }))

      log.info(s"Serving up ${Biller.plans.size} plans")
      JsonContent ~> ResponseString(json)
    }

    case POST(Path("/subs/clear")) => Directives.success {
      Biller.clearSubs()
      JsonContent ~> Ok
    }

    case req @ POST(Path(Seg("subs" :: receiptEncoded :: Nil))) =>
      for {
        json <- getOrBad(parseOpt(Body.string(req)))
        status <- getStatusCode(json)
        sub <- getOrBad(Biller.subscriptions.get(receiptEncoded))
      } yield {
        Biller.setSubStatus(sub, status)
        JsonContent ~> Ok
      }

    // curl -d '' http://localhost:9090/receipts/abcd/renew
    case POST(Path(Seg("subs" :: receiptEncoded :: "renew" :: Nil))) =>
      for {
        sub <- getOrBad(Biller.subscriptions.get(receiptEncoded))
      } yield {
        Biller.renewSub(sub)
        JsonContent ~> Ok
      }

    case POST(Path(Seg("subs" :: receiptEncoded :: "cancel" :: Nil))) =>
      for {
        sub <- getOrBad(Biller.subscriptions.get(receiptEncoded))
      } yield {
        Biller.cancelSub(sub)
        JsonContent ~> Ok
      }


    case POST(Path(Seg("subs" :: receiptEncoded :: "refund" :: transactionId :: Nil))) =>
      for {
        sub <- getOrBad(Biller.subscriptions.get(receiptEncoded))
        receiptInfo <- getOrBad(sub.transactionMap.get(transactionId))
      } yield {
        Biller.refundTransaction(sub, receiptInfo)
        NoContent ~> Ok
      }


    case GET(Path("/subs")) => Directives.success {
      val sortedSubs =
        Biller.subscriptions.values
          .toList
          .sortBy(_.originalReceiptInfo.purchaseDate.getTime)
          .reverse

      JsonContent ~> ResponseString(writePretty(sortedSubs))
    }

    // curl -d '{"productId":"", "status":""}' http://localhost:9090/receipts
    case req @ POST(Path("/subs")) =>
      for {
        json <- getOrBad(parseOpt(Body.string(req)))
        productId <- getProductId(json)
        plan <- getOrBad(Biller.plansByProductId.get(productId))
      } yield {
        log.info(s"Creating receipt for plan '${plan.name}'")
        val sub = Biller.createSub(plan)
        JsonContent ~> ResponseString(writePretty(sub))
      }

    //curl -d '{"receipt-data":"abcd"}' http://localhost:9090/verifyRceipt
    case req @ POST(Path("/verifyReceipt")) =>
      for {
        json <- getOrBad(parseOpt(Body.string(req)))
        receipt <- getReceiptData(json)
      } yield {
        val response = Biller.subscriptions.get(receipt).map { sub =>
          log.info(s"attempting to verifyReceipt for $receipt")
          ReceiptRenderer(ReceiptGenerator(sub))
        }.getOrElse {
          log.info(s"Failed to find receipt: $receipt, returning BadReceipt.")
          ReceiptRenderer(ReceiptResponse(receipt = null, statusCode = UnauthorizedReceipt.code))
        }

        JsonContent ~> ResponseString(response)
      }
  } }

  private def page = {
    val stream = getClass.getResourceAsStream("/template/page.html")
    val html = Source.fromInputStream(stream).mkString
    ResponseString(html)
  }
}
