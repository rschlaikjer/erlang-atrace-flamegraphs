node {

    def app

    stage('Clone repository') {
        checkout scm
        sh "git rev-parse origin/${sha1} > .commit"
    }

    stage('Build image') {
        app = docker.build("erlang-aflame")
    }

    stage('Push image') {
        def commit = readFile('.commit').trim()
        docker.withRegistry('http://docker-registry:5000') {
            app.push("${commit}")
            app.push("${sha1}")
        }

        sh "docker image prune -fa --filter 'until=240h'"
    }

    stage('Kubernetes Deploy') {
        def commit = readFile('.commit').trim()
        sh "docker run --rm \
        --mount type=bind,source=/creds/kubectl/,target=/root/.kube/ \
        lachlanevenson/k8s-kubectl:v1.10.2 \
        set image deployment/aflame-deployment \
        aflame=docker-registry:5000/erlang-aflame:${commit}"

        sh "docker run --rm \
        --mount type=bind,source=/creds/kubectl/,target=/root/.kube/ \
        lachlanevenson/k8s-kubectl:v1.10.2 \
        rollout status deployment/aflame-deployment"
    }

}

