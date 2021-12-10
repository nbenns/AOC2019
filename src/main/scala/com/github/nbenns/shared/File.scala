package com.github.nbenns.shared

import zio.*
import zio.nio.channels.AsynchronousFileChannel
import zio.stream.*

import java.io.IOException
import java.net.URL

object File {
  extension (self: ZIO.type) {
    def attemptNullable[A](a: A | Null): Task[A] =
      if (a == null) ZIO.fail(new NullPointerException("Object is null"))
      else ZIO.attempt(a)
  }

  extension (self: ZStream.type) {
    def attemptNullable[A](a: A | Null) =
      if (a == null) ZStream.fail(new NullPointerException("Object is null"))
      else ZStream.from(a)
  }

  def fromResource(file: String): Task[String] =
    ZIO
      .attemptNullable(this.getClass.getClassLoader)
      .flatMap(a => ZIO.attemptNullable(a.getResource(file)))
      .flatMap(a => ZIO.attemptNullable(a.getFile))

  def fileReadStreamByChunk(chunkSize: Int, fileChannel: AsynchronousFileChannel): Stream[Exception, Byte] = {
    val zSize: ZIO[Any, IOException, Long] = fileChannel.size.memoize.flatten

    def readFromFile(place: Long, size: Int): ZIO[Any, Exception, Option[(Chunk[Byte], Long)]] =
      fileChannel
        .readChunk(size, place)
        .map { chunk =>
          if (chunk.isEmpty) None
          else Some(chunk, place + chunk.size)
        }

    ZStream.unfoldChunkZIO(0L) { place =>
      zSize
        .map(_ - place)
        .flatMap { remaining =>
          if (remaining > 0) readFromFile(place, Math.min(chunkSize, remaining).toInt)
          else ZIO.none
        }
    }
  }

  def fileReadStreamByLine(chunkSize: Int, fileChannel: AsynchronousFileChannel): Stream[Exception, String] =
    fileReadStreamByChunk(chunkSize, fileChannel)
      .via(ZPipeline.utf8Decode)
      .via(ZPipeline.splitLines)

  def fileReadStreamCommaSep(chunkSize: Int, fileChannel: AsynchronousFileChannel): Stream[Exception, String] =
    fileReadStreamByChunk(chunkSize, fileChannel)
      .via(ZPipeline.utf8Decode)
      .via(ZPipeline.splitLines)
      .via(ZPipeline.splitOn(","))
      .flatMap(str => ZStream.attemptNullable(str.trim))
}
