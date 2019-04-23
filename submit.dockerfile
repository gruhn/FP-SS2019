FROM alpine:latest

RUN apk --no-cache add zip

CMD ["./generate-submissions.sh"]
