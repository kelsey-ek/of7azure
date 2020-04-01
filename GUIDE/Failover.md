
# Fail-over test with OpenFrame

## Table of Contents

+ [1. Azure Storage service](#step-1-create-image-of-openframe)
  + [1.1 Install docker](#11-install-docker)
  + [1.2 Get centos container](#12-get-centos-container)
  + [1.3 Install OpenFrame](#13-install-openframe)
      + [1.3.1 Pre settings](#131-pre-settings)
      + [1.3.2 JAVA installation](#132-java-installation)
      + [1.3.3 Tibero installation](#133-tibero-installation)
      + [1.3.4 UnixODBC installation](#134-unixodbc-installation)
      + [1.3.5 OFCOBOL installation](#135-ofcobol-installation)
      + [1.3.6 PROSORT installation](#136-prosort-installation)
      + [1.3.7 Base installation](#137-base-installation)
      + [1.3.8 Batch installation](#138-batch-installation)
      + [1.3.9 TACF installation](#139-tacf-installation)
      + [1.3.10 OSC installation](#1310-osc-installation)
      + [1.3.11 JEUS installation](#1311-jeus-installation)
      + [1.3.12 OFGW installation](#1312-ofgw-installation)
      + [1.3.13 OFManager installation](#1313-ofmanager-installation)
  + [1.4 Create OpenFrame image](#14-create-openframe-image)
  + [1.5 Use OpenFrame image](#15-use-openframe-image)
+ [2. Azure Service](#step-2-azure-service)
  + [2.1 Add Azure Kubernetes service(AKS)](#21-add-azure-kubernetes-serviceaks)
  + [2.2 Set pods](#22-set-pods)
  + [2.3 Set services](#23-set-services)
  + [2.4 Network configuration](#24-network-configuration)
  + [2.5 Azure monitoring service](#25-azure-monitoring-service)

## Step 1. Create image of OpenFrame

### 1.1 Install docker

Applications that run in Azure Kubernetes Service (AKS) may need to store and retrieve data. For some application workloads, this data storage can use local, fast storage on the node that is no longer needed when the pods are deleted. Other application workloads may require storage that persists on more regular data volumes within the Azure platform. Multiple pods may need to share the same data volumes, or reattach data volumes if the pod is rescheduled on a different node. Finally, you may need to inject sensitive data or application configuration information into pods.


