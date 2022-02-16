// Template Jenkinsfile for R CI/CD Tasks
// url: https://github.com/INWT/inwt-templates/blob/master/jenkins/r-ci.Jenkinsfile
// Author: Sebastian Warnholz
pipeline {
    agent none
    options { disableConcurrentBuilds() }
    environment {
        CUR_PROJ = 'mpi-fruits' // github repo name
        CUR_PKG = 'ReSources' // r-package name
        CUR_PKG_FOLDER = '.' // defaults to root
        DOCKER_REPO = 'registry.core.inwt.de'
        BASE_IMAGE = 'resources-isoapp-base:latest'
        DOCKER_REGISTRY = credentials('core-docker-registry-user')
    }
    stages {
        stage('Testing with') {
            agent { label 'docker1' }
            when { not { branch 'depl' } }
            environment {
                TMP_SUFFIX = """${sh(returnStdout: true, script: 'echo `cat /dev/urandom | tr -dc \'a-z\' | fold -w 6 | head -n 1`')}"""
            }
            steps {
                sh '''
                echo $DOCKER_REGISTRY_PSW | docker login --username $DOCKER_REGISTRY_USR --password-stdin $DOCKER_REPO
                docker pull $DOCKER_REPO/$BASE_IMAGE
                docker tag $DOCKER_REPO/$BASE_IMAGE $BASE_IMAGE
                docker build -t tmp-$CUR_PROJ-$TMP_SUFFIX .
                docker run --rm --network host tmp-$CUR_PROJ-$TMP_SUFFIX check
                docker rmi tmp-$CUR_PROJ-$TMP_SUFFIX
                docker rmi $DOCKER_REPO/$BASE_IMAGE || :
                docker rmi $BASE_IMAGE || :
                '''
            }
        }

        stage('Deploy R-package') {
            agent { label 'eh2' }
            when { branch 'master' }
            steps {
                sh '''
                rm -vf *.tar.gz
                docker pull inwt/r-batch:latest
                docker run --rm --network host -v $PWD:/app --user `id -u`:`id -g` inwt/r-batch:latest R CMD build $CUR_PKG_FOLDER
                docker run --rm -v $PWD:/app -v /var/www/html/r-repo:/var/www/html/r-repo inwt/r-batch:latest R -e "drat::insertPackage(dir(pattern='.tar.gz'), '/var/www/html/r-repo'); drat::archivePackages(repopath = '/var/www/html/r-repo')"
                '''
            }
        }

        stage('Deploy Docker-image') {
            agent { label 'docker1' }
            when  { branch 'master' }
            steps {
                sh '''
                echo $DOCKER_REGISTRY_PSW | docker login --username $DOCKER_REGISTRY_USR --password-stdin $DOCKER_REPO
                docker pull $DOCKER_REPO/$BASE_IMAGE
                docker tag $DOCKER_REPO/$BASE_IMAGE $BASE_IMAGE
                docker build -t $DOCKER_REPO/$CUR_PROJ:latest .
                docker push $DOCKER_REPO/$CUR_PROJ:latest
                docker rmi $DOCKER_REPO/$CUR_PROJ:latest
                docker rmi $DOCKER_REPO/$BASE_IMAGE || :
                docker rmi $BASE_IMAGE || :
                '''
            }
        }
    }
    post {
        always {
            script {
                if (env.BRANCH_NAME != 'master' && env.BRANCH_NAME != 'depl') {
                    emailext (
                        attachLog: true,
                        body: "Build of job ${env.JOB_NAME} (No. ${env.BUILD_NUMBER}) has completed\n\nBuild status: ${currentBuild.currentResult}\n\n${env.BUILD_URL}\n\nSee attached log file for more details of the build process.",
                        recipientProviders: [[$class: 'DevelopersRecipientProvider'], [$class: 'RequesterRecipientProvider']],
                        subject: "Jenkins Build ${currentBuild.currentResult}: Job ${env.JOB_NAME}"
                    )
                }
            }
        }
        failure {
           script {
                if (env.BRANCH_NAME == 'master' || env.BRANCH_NAME == 'depl') {
                    emailext (
                        attachLog: true,
                        body: "Build of job ${env.JOB_NAME} (No. ${env.BUILD_NUMBER}) has completed\n\nBuild status: ${currentBuild.currentResult}\n\n${env.BUILD_URL}\n\nSee attached log file for more details of the build process.",
                        recipientProviders: [[$class: 'DevelopersRecipientProvider'], [$class: 'RequesterRecipientProvider']],
                        to: "andreas.neudecker@inwt-statistics.de,marcus.gross@inwt-statistics.de",
                        subject: "Jenkins Build ${currentBuild.currentResult}: Job ${env.JOB_NAME}"
                    )
                }
            }
        }
    }
}
