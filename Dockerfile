FROM clojure as build
RUN mkdir -p /usr/src/app
WORKDIR /usr/src/app
COPY project.clj ./
RUN lein deps
COPY . ./
RUN lein uberjar

FROM eclipse-temurin as runtime
COPY --from=build /usr/src/app/target/mrrrp.jar /opt/mrrrp.jar
ENTRYPOINT ["java", "-Xmx256m", "-jar", "/opt/mrrrp.jar"]
