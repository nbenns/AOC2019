package com.github.nbenns.shared

import zio.Chunk
import zio.nio.channels.AsynchronousFileChannel
import zio.stream.{Stream, ZStream, ZTransducer}

object File {
  def fromResource(file: String): String = this.getClass.getClassLoader.getResource(file).getFile

  def fileReadStreamByChunk(chunkSize: Int, fileChannel: AsynchronousFileChannel): Stream[Exception, Chunk[Byte]] =
    ZStream.unfoldM(0) { place =>
      fileChannel
        .read(chunkSize, place)
        .map { chunk =>
          if (chunk.isEmpty) None
          else Some(chunk, place + chunkSize)
        }
    }

  def fileReadStreamByLine(chunkSize: Int, fileChannel: AsynchronousFileChannel): Stream[Exception, String] =
    fileReadStreamByChunk(chunkSize, fileChannel)
      .mapConcatChunk(identity)
      .aggregate(ZTransducer.utf8Decode)
      .aggregate(ZTransducer.splitLines)

  def fileReadStreamCommaSep(chunkSize: Int, fileChannel: AsynchronousFileChannel): Stream[Exception, String] =
    fileReadStreamByChunk(chunkSize, fileChannel)
      .mapConcatChunk(identity)
      .aggregate(ZTransducer.utf8Decode)
      .aggregate(ZTransducer.splitLines)
      .aggregate(ZTransducer.splitOn(","))
      .map(_.trim)
}
