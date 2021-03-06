package com.meetup.iap.receipt

import java.text.SimpleDateFormat

import com.meetup.iap.AppleApi
import AppleApi.{FullReceiptInfo, ReceiptInfo, ReceiptResponse}
import java.util.{Date, TimeZone}

import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods._
import org.json4s.JsonAST.JValue
import org.slf4j.LoggerFactory

object ReceiptRenderer {
  val log = LoggerFactory.getLogger(ReceiptRenderer.getClass)

  private def appleDateFormat(date: Date): String = {
    val sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss 'Etc/GMT'")
    sdf.setTimeZone(TimeZone.getTimeZone("UTC"))
    sdf.format(date)
  }

  private def appleDateFormatPST(date: Date): String = {
    val sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss 'America/Los_Angeles'")
    sdf.setTimeZone(TimeZone.getTimeZone("PST"))
    sdf.format(date)
  }

  def apply(response: ReceiptResponse): String = {
    val latest_receipt_info = response.latestReceiptInfo.reverse.map(renderReceipt)

    pretty(render(
      ("status" -> response.statusCode) ~
        ("receipt" -> renderFullReceipt(latest_receipt_info, response.receipt)) ~
        ("latest_receipt_info" -> latest_receipt_info) ~
        ("latest_receipt" -> response.latestReceipt))
    )
  }

  private def renderFullReceipt(latestReceiptInfo: List[JValue], receipt: FullReceiptInfo): JValue = {
    val requestDate = receipt.requestDate
    val requestDateStr = appleDateFormat(requestDate)
    val requestDatePSTStr = appleDateFormatPST(requestDate)
    val requestDateMs = requestDate.getTime

    val receiptCreationDate = receipt.receiptCreationDate
    val receiptCreationDateStr = appleDateFormat(receiptCreationDate)
    val receiptCreationDatePSTStr = appleDateFormatPST(receiptCreationDate)
    val receiptCreationDateMs = receiptCreationDate.getTime

    val origPurchaseDate = receipt.originalPurchaseDate
    val origPurchaseDateStr = appleDateFormat(origPurchaseDate)
    val origPurchaseDatePSTStr = appleDateFormatPST(origPurchaseDate)
    val origPurchaseDateMs = origPurchaseDate.getTime

    ("receipt_type" -> receipt.receiptType) ~
      ("adam_id" -> receipt.adamId) ~
      ("app_item_id" -> receipt.appItemId) ~
      ("bundle_id" -> receipt.bundleId) ~
      ("application_version" -> receipt.applicationVersion) ~
      ("download_id" -> receipt.downloadId) ~
      ("versionExternal_identifier" -> receipt.versionExternalIdentifier) ~
      ("request_date" -> requestDateStr) ~
      ("request_date_ms" -> requestDateMs) ~
      ("request_date_pst" -> requestDatePSTStr) ~
      ("receipt_creation_date" -> receiptCreationDateStr) ~
      ("receipt_creation_date_ms" -> receiptCreationDateMs.toString) ~
      ("receipt_creation_date_pst" -> receiptCreationDatePSTStr) ~
      ("original_purchase_date" -> origPurchaseDateStr) ~
      ("original_purchase_date_ms" -> origPurchaseDateMs) ~
      ("original_purchase_date_pst" -> origPurchaseDatePSTStr) ~
      ("original_application_version" -> receipt.originalApplicationVersion) ~
      ("original_transaction_id" -> receipt.originalTransactionId) ~
      ("in_app" -> latestReceiptInfo)
  }

  private def renderReceipt(receiptInfo: ReceiptInfo): JValue = {
    val origPurchaseDate = receiptInfo.originalPurchaseDate
    val origPurchaseDateStr = appleDateFormat(origPurchaseDate)
    val origPurchaseDateMs = origPurchaseDate.getTime

    val purchaseDate = receiptInfo.purchaseDate
    val purchaseDateStr = appleDateFormat(purchaseDate)
    val purchaseDateMs = purchaseDate.getTime

    val expiresDate = receiptInfo.expiresDate
    val expiresDateStr = appleDateFormat(expiresDate)
    val expiresDateMs = expiresDate.getTime

    val cancellationDate = receiptInfo.cancellationDate.map { date =>
      appleDateFormat(date)
    }
    ("quantity" -> "1") ~
      ("product_id" -> receiptInfo.productId) ~
      ("transaction_id" -> receiptInfo.transactionId) ~
      ("original_transaction_id" -> receiptInfo.originalTransactionId) ~
      ("purchase_date" -> purchaseDateStr) ~
      ("purchase_date_ms" -> purchaseDateMs.toString) ~
      ("original_purchase_date" -> origPurchaseDateStr) ~
      ("original_purchase_date_ms" -> origPurchaseDateMs.toString) ~
      ("expires_date" -> expiresDateStr) ~
      ("expires_date_ms" -> expiresDateMs.toString) ~
      ("is_trial_period" -> receiptInfo.isTrialPeriod.toString) ~ //We mimic Apple's weird json here by converting the boolean type to a string
      ("cancellation_date" -> cancellationDate)
  }
}
