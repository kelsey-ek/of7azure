# Azure service with OpenFrame by Kelsey

## Table of Contents

+ [1. DNS setting](#step-1-divided-modules-of-openframe-as-pods)
  + [1.1 Concept of divided modules](#11-concept-of-divided-modules)
      + [1.1.1 TIbero(DB server)](#111-tiberodb-server)
      + [1.2.2 Online/Batch](#112-online--batch)
      + [1.2.3 UI tool](#113-ui-tool)
  + [1.2 Configuration of each modules](#12-configuration-of-each-modules)
      + [1.2.1 TIbero(DB server)](#121-tiberodb-server)
      + [1.2.2 Online/Batch](#122-online--batch)
      + [1.2.3 UI tool](#123-ui-tool)

+ [2. Fail-over with PODs of divided modules](#1-fail-over-environment-setting)
  + [2.1 Risk analysis]()
  + [2.2 Suggestion]()
  
## Step 1. DNS setting of OpenFrame as PODs

### 1.1 Concept of divided modules

**Use four containers(work as seperate VMs) for installing OSC, Batch, Tibero and JEUS for each.**

**picture**

#### 1.1.1 Tibero(DB server)


kelsey@Azure:~$  kubectl get po -n kube-system -l k8s-app=kube-dns
NAME                       READY   STATUS    RESTARTS   AGE
coredns-869cb84759-m9854   1/1     Running   0          6d7h
coredns-869cb84759-zg89s   1/1     Running   0          6d7h


kelsey@Azure:~$ kubectl get svc -n kube-system -l k8s-app=kube-dns
NAME       TYPE        CLUSTER-IP   EXTERNAL-IP   PORT(S)         AGE
kube-dns   ClusterIP   10.0.0.10    <none>        53/UDP,53/TCP   17d
