package sensala.web

import javax.inject.Singleton

import play.api.http.{DefaultHttpErrorHandler, Status}
import play.api.mvc.Results.NotFound
import play.api.mvc.{RequestHeader, Result}

import scala.concurrent.Future

@Singleton
class ErrorHandler extends DefaultHttpErrorHandler {
  override def onClientError(
    request: RequestHeader,
    statusCode: Int,
    message: String
  ): Future[Result] = {
    if (statusCode == Status.NOT_FOUND) {
      Future.successful(NotFound(views.html.errors.notFound()))
    } else {
      super.onClientError(request, statusCode, message)
    }
  }
}
