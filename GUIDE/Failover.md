# Fail-over test with Azure Kubernetes service(AKS)

## Table of Contents

+ [1. Fail-over Environment setting](#step-1-fail-over-environment-setting)
  + [1.1 Fail-over concept](#11-fail-over-concept)
  + [1.2 Storage setting](#12-storage-setting)
      + [1.2.1 Persistant Volume Claim](#121-persistant-volume-claim)
      + [1.2.2 Persistant Volume](#122-persistant-volume)
      + [1.2.3 Storage Class](#123-storage-class)
  + [1.3 Deployment with replicated Pods](#13-deployment-with-replicated-pods)
+ [2. Fail-over test](#step-2-azure-service)
  + [2.1 Test senario](#21-add-azure-kubernetes-serviceaks)
  + [2.2 Test results](#22-set-pods)

## Step 1. Fail-over Environment setting

### 1.1 Fail-over concept

* A Pod which has OpenFrame container is running in NODE1. NODE2 is a back up Node which is empty.

<img src="./reference_images/fail01.PNG" title="fail01">

* When NODE1 is dead, the Pod will be terminated from NODE1 but created in NODE2.

<img src="./reference_images/fail02.PNG" title="fail02">

*A Pod is the basic execution unit of a Kubernetes application–the smallest and simplest unit in the Kubernetes object model that you create or deploy. A Pod represents processes running on your Cluster.*

**Here are two check points for doing this fail-over test.**

When NODE1 dies,

1) A new Pod should not lose the critical data of the old Pod.
- Persistant Vloume and Persistant Volume Claim will be used.

2) A Pod should be automatically created in a different Node and run successfully.
- Deployment with replicated Pods will be used(in this case, only one Pod is needed).

### 1.2 Storage setting

__A.__ Storag eClass (SC) 

* A claim can request a particular class by specifying the name of a StorageClass using the attribute storageClassName. Only PVs of the requested class, ones with the same storageClassName as the PVC, can be bound to the PVC.

* Access Modes

    ```bash
    ReadWriteOnce – the volume can be mounted as read-write by a single node (RWO)
    ReadOnlyMany – the volume can be mounted read-only by many nodes (ROX) 
    ReadWriteMany – the volume can be mounted as read-write by many nodes (RWX)
    ```
* A PersistentVolume can be mounted on a host in any way supported by the resource provider. As shown in the table below, providers will have different capabilities and each PV’s access modes are set to the specific modes supported by that particular volume. For example, NFS can support multiple read/write clients, but a specific NFS PV might be exported on the server as read-only. Each PV gets its own set of access modes describing that specific PV’s capabilities.

   <img src="./reference_images/access.PNG" title="access">

```vi azure_sc.yaml```
```bash
apiVersion: storage.k8s.io/v1
kind: StorageClass
metadata:
  name: azurefile
provisioner: kubernetes.io/azure-file
mountOptions:
  - dir_mode=0777
  - file_mode=0777
  - uid=0
  - gid=0
  - mfsymlinks
  - cache=strict
parameters:
  skuName: Standard_LRS
```

__B.__ Persistent Volume (PV) 

* A PersistentVolume (PV) is a piece of storage in the cluster that has been provisioned by an administrator or dynamically provisioned using Storage Classes. It is a resource in the cluster just like a node is a cluster resource.

- **PVs are volume plugins like Volumes, but have a lifecycle independent of any individual Pod that uses the PV.** This API object captures the details of the implementation of the storage, be that NFS, iSCSI, or a cloud-provider-specific storage system.

__C.__ Persistent Volume Claim (PVC) 

* A Persistent Volume Claim (PVC) is a request for storage by a user. It is similar to a Pod. Pods consume node resources and PVCs consume PV resources. Pods can request specific levels of resources (CPU and Memory). Claims can request specific size and access modes (e.g., they can be mounted once read/write or many times read-only).

* While PersistentVolumeClaims allow a user to consume abstract storage resources, it is common that users need PersistentVolumes with varying properties, such as performance, for different problems. Cluster administrators need to be able to offer a variety of PersistentVolumes that differ in more ways than just size and access modes, without exposing users to the details of how those volumes are implemented. For these needs, there is the StorageClass resource.

### 1.3 Use Persistent Volume with pod replication

### 1.3.1 Use Persistent Volume with Azure Kubernetes Service

1) Check Storage Class

    ```kubectl get sc```
    ```bash
    NAME                PROVISIONER                AGE
    azurefile           kubernetes.io/azure-file   6d1h
    azurefile-premium   kubernetes.io/azure-file   6d1h
    default (default)   kubernetes.io/azure-disk   6d1h
    managed-premium     kubernetes.io/azure-disk   6d1h
    ```
* Those four Storage Classes are provided by Azure service. You can create a custom Storage Class when you use custom Persistent Volume.

* In this case, I will use managed-premium to use Azure Kubernetes Service(AKS). 

    ```kubectl describe sc managed-premium```
    ```bash
    Name:            managed-premium
    IsDefaultClass:  No
    Annotations:     kubectl.kubernetes.io/last-applied-configuration={"allowVolumeExpansion":true,"apiVersion":"storage.k8s.io/v1beta1","kind":"StorageClass","metadata":{"annotations":{},"labels":{"kubernetes.io/cluster-service":"true"},"name":"managed-premium"},"parameters":{"cachingmode":"ReadOnly","kind":"Managed","storageaccounttype":"Premium_LRS"},"provisioner":"kubernetes.io/azure-disk"}

    Provisioner:           kubernetes.io/azure-disk
    Parameters:            cachingmode=ReadOnly,kind=Managed,storageaccounttype=Premium_LRS
    AllowVolumeExpansion:  True
    MountOptions:          <none>
    ReclaimPolicy:         Delete
    VolumeBindingMode:     Immediate
    Events:                <none>
    ```

2) Create Persistent Volume Claim (PVC)

    ```vi volumeclaim.yaml```
    ```bash 
    apiVersion: v1
    kind: PersistentVolumeClaim
    metadata:
      name: of7storage
    spec:
      accessModes:
      - ReadWriteOnce
      storageClassName: managed-premium  -> If not set, it is set to default
      resources:
        requests:
          storage: 500Gi
    ```
    ``` kubectl create -f volumeclaim.yaml```
    
    ``` kubectl get pvc```
    ```bash
    NAME         STATUS   VOLUME                                     CAPACITY   ACCESS MODES   STORAGECLASS      AGE
    of7storage   Bound    pvc-a0a48609-8975-433f-9b73-bc371cbb0702   500Gi      RWO            managed-premium   16h
    ```
    
    ``` kubectl describe pvc of7storage ```
    ```bash
    Name:          of7storage
    Namespace:     default
    StorageClass:  managed-premium
    Status:        Bound
    Volume:        pvc-a0a48609-8975-433f-9b73-bc371cbb0702
    Labels:        <none>
    Annotations:   pv.kubernetes.io/bind-completed: yes
                   pv.kubernetes.io/bound-by-controller: yes
                   volume.beta.kubernetes.io/storage-provisioner: kubernetes.io/azure-disk
    Finalizers:    [kubernetes.io/pvc-protection]
    Capacity:      500Gi
    Access Modes:  RWO
    VolumeMode:    Filesystem
    Mounted By:    <none>  ->  changes to the pod name when the pod is attached
    Events:        <none>
    ```
    
*Clean it*

    kubectl delete pvc of7storage
    
3) Persistent Volume is automatically generated with Azure Kubernetes Service

- From the PVC above, it uses managed-premium storageClass whose provisioner is kubernetes.io/**azure-disk**. 
- It automatically generates **AzureDisk**(Persistant Volume) in Azure service.
    
    <img src="./reference_images/disk01.PNG" title="disk01">
    
- You can also check the created PV with kubectl commands.
     
    ``` kubectl get pv ```
    ```bash
    NAME                                     CAPACITY ACCESS MODES RECLAIM POLICY STATUS CLAIM              STORAGECLASS   REASON AGE
    pvc-a0a48609-8975-433f-9b73-bc371cbb0702 500Gi    RWO          Delete         Bound  default/of7storage managed-premium       16h
    ```
    
    ``` kubectl describe pv pvc-a0a48609-8975-433f-9b73-bc371cbb0702 ```
    ```bash
    Name:            pvc-a0a48609-8975-433f-9b73-bc371cbb0702
    Labels:          <none>
    Annotations:     pv.kubernetes.io/bound-by-controller: yes
                     pv.kubernetes.io/provisioned-by: kubernetes.io/azure-disk
                     volumehelper.VolumeDynamicallyCreatedByKey: azure-disk-dynamic-provisioner
    Finalizers:      [kubernetes.io/pv-protection]
    StorageClass:    managed-premium
    Status:          Bound
    Claim:           default/of7storage
    Reclaim Policy:  Delete
    Access Modes:    RWO
    VolumeMode:      Filesystem
    Capacity:        500Gi
    Node Affinity:   <none>
    Message:
    Source:
        Type:         AzureDisk (an Azure Data Disk mount on the host and bind mount to the pod)
        DiskName:     kubernetes-dynamic-pvc-a0a48609-8975-433f-9b73-bc371cbb0702
        DiskURI:      /subscriptions/9c327935-ea7c-4dfe-a425-f45aee2a1959/resourceGroups/mc_of7azure_kelsey_aksof7azure_northcentralus/providers/Microsoft.Compute/disks/kubernetes-dynamic-pvc-a0a48609-8975-433f-9b73-bc371cbb0702
        Kind:         Managed
        FSType:
        CachingMode:  ReadOnly
        ReadOnly:     false
    Events:           <none>
    ```
    
*Clean it*

    kubectl delete pv pvc-a0a48609-8975-433f-9b73-bc371cbb0702
    
4) Create a Pod using the Persistent Volume

*The reason why I chose Deployment for creating replicated Pods is - updating the Deployment(in this case, OpenFrame) is more suitable than using Replication controller.(It only replicates the Pods, do not supports rolling-back and rolling-out for updating the application.)*

- In this fail-over test case, replicas is set to one.

    ```vi deployment.yaml```
    ```bash
    apiVersion: extensions/v1beta1
    kind: Deployment
    metadata:
      name: of7azure
    spec:
      replicas: 1
      template:
        metadata:
          name: of7azure
          labels:
            of7azurefinal: of7azure
        spec:
          hostname: of7azure
          containers:
          - name: of7azure
            image: kelsey92/of7azurefinal:of7azure
            ports:
            - containerPort: 6066
            command: ["/bin/sh", "-ec", "while :; do echo '.'; sleep 5 ; done"]
            volumeMounts:
            - name: sharedvolume
              mountPath: /mnt/azure
          volumes:
          - name: sharedvolume
            persistentVolumeClaim:
              claimName: of7storage
    ```
    ```kubectl get deployments```
    ```bash
    NAME       READY   UP-TO-DATE   AVAILABLE   AGE
    of7azure   1/1     1            1           167m
    ```

    ```kubectl describe deployment of7azure```
    ```bash
    Name:                   of7azure
    Namespace:              default
    CreationTimestamp:      Thu, 02 Apr 2020 04:48:36 +0000
    Labels:                 of7azurefinal=of7azure
    Annotations:            deployment.kubernetes.io/revision: 1
    Selector:               of7azurefinal=of7azure
    Replicas:               1 desired | 1 updated | 1 total | 1 available | 0 unavailable
    StrategyType:           RollingUpdate
    MinReadySeconds:        0
    RollingUpdateStrategy:  1 max unavailable, 1 max surge
    Pod Template:
      Labels:  of7azurefinal=of7azure
      Containers:
       of7azure:
        Image:      kelsey92/of7azurefinal:of7azure
        Port:       6066/TCP
        Host Port:  0/TCP
        Command:
          /bin/sh
          -ec
          while :; do echo '.'; sleep 5 ; done
        Environment:  <none>
        Mounts:
          /mnt/azure from sharedvolume (rw)
      Volumes:
       sharedvolume:
        Type:       PersistentVolumeClaim (a reference to a PersistentVolumeClaim in the same namespace)
        ClaimName:  of7storage
        ReadOnly:   false
    Conditions:
      Type           Status  Reason
      ----           ------  ------
      Available      True    MinimumReplicasAvailable
    OldReplicaSets:  <none>
    NewReplicaSet:   of7azure-76db5dbccb (1/1 replicas created)
    Events:          <none>
    ```
    
    ```kubectl get pods```
    ```bash
    NAME                        READY   STATUS    RESTARTS   AGE
    of7azure-76db5dbccb-96q4k   1/1     Running   0          171m
    ```
    
    ```kubectl describe pod of7azure-76db5dbccb-96q4k```
    ```bash
    Name:           of7azure-76db5dbccb-96q4k
    Namespace:      default
    Priority:       0
    Node:           aks-agentpool-24893396-1/10.240.0.35
    Start Time:     Thu, 02 Apr 2020 04:48:36 +0000
    Labels:         of7azurefinal=of7azure
                    pod-template-hash=76db5dbccb
    Annotations:    <none>
    Status:         Running
    IP:             10.240.0.40
    IPs:            <none>
    Controlled By:  ReplicaSet/of7azure-76db5dbccb
    Containers:
      of7azure:
        Container ID:  docker://575d75a1938c3e5d1d469f33b5d61dcaa1e9bb7fc9995b5cd805b73f44e2f81a
        Image:         kelsey92/of7azurefinal:of7azure
        Image ID:      docker-pullable://kelsey92/of7azurefinal@sha256:9942ac999d9f51ed163cc68f05fbef93d0317135a86d567c106747d07e1b4527
        Port:          6066/TCP
        Host Port:     0/TCP
        Command:
          /bin/sh
          -ec
          while :; do echo '.'; sleep 5 ; done
        State:          Running
          Started:      Thu, 02 Apr 2020 04:48:57 +0000
        Ready:          True
        Restart Count:  0
        Environment:    <none>
        Mounts:
          /mnt/azure from sharedvolume (rw)
          /var/run/secrets/kubernetes.io/serviceaccount from default-token-lxwlr (ro)
    Conditions:
      Type              Status
      Initialized       True
      Ready             True
      ContainersReady   True
      PodScheduled      True
    Volumes:
      sharedvolume:
        Type:       PersistentVolumeClaim (a reference to a PersistentVolumeClaim in the same namespace)
        ClaimName:  of7storage
        ReadOnly:   false
      default-token-lxwlr:
        Type:        Secret (a volume populated by a Secret)
        SecretName:  default-token-lxwlr
        Optional:    false
    QoS Class:       BestEffort
    Node-Selectors:  <none>
    Tolerations:     node.kubernetes.io/not-ready:NoExecute for 300s
                     node.kubernetes.io/unreachable:NoExecute for 300s
    Events:
      Type     Reason                  Age    From                               Message
      ----     ------                  ----   ----                               -------
      Normal   Scheduled               4m41s  default-scheduler                  Successfully assigned default/of7azure-76db5dbccb-4d69f to aks-agentpool-24893396-1
      Normal   SuccessfulAttachVolume  57s    attachdetach-controller            AttachVolume.Attach succeeded for volume "pvc-a0a48609-8975-433f-9b73-bc371cbb0702"
      Normal   Pulled                  26s    kubelet, aks-agentpool-24893396-1  Container image "kelsey92/of7azurefinal:of7azure" already present on machine
      Normal   Created                 26s    kubelet, aks-agentpool-24893396-1  Created container of7azure
      Normal   Started                 26s    kubelet, aks-agentpool-24893396-1  Started container of7azure
    ```

*Clean it*

```kubectl delete deployment of7azure```

```kubectl delete pod of7azure-76db5dbccb-96q4k```

- **Disk state** changes from Unattached to Attached 
- **Owner VM** changes from --(none) to the VM where the Pod is running
      
    <img src="./reference_images/disk02.PNG" title="disk02">
     

### 1.3.2 Use the custom Persistent Volume with repliated pod

1) Check the access mode you want to configure and choose the storage service

- Access Modes

    ```bash
    ReadWriteOnce – the volume can be mounted as read-write by a single node (RWO)
    ReadOnlyMany – the volume can be mounted read-only by many nodes (ROX) 
    ReadWriteMany – the volume can be mounted as read-write by many nodes (RWX)
    ```
- Storage services with access modes

    <img src="./reference_images/access.PNG" title="access">

2) Create a storage class with the provisioner you want

    ```vi custom_sc.yaml```
    ```bash
    apiVersion: storage.k8s.io/v1
    kind: StorageClass
    metadata:
      name: glustersc
    provisioner: kubernetes.io/glusterfs
    parameters:
      resturl: "http://192.168.10.100:8080"
      restuser: ""
      secretNamespace: ""
      secretName: ""
    allowVolumeExpansion: true
    ~
    ```
    ```kubectl create -f custom_sc.yaml```

    - Parameters may vary depends on the provisioner.

    ```kubectl get sc glustersc```
    ```bash
    NAME        PROVISIONER               AGE
    glustersc   kubernetes.io/glusterfs   16m
    ```
    ```kubectl describe sc glustersc```
    ```bash
    Name:                  glustersc
    IsDefaultClass:        No
    Annotations:           <none>
    Provisioner:           kubernetes.io/glusterfs
    Parameters:            resturl=http://192.168.10.100:8080,restuser=,secretName=,secretNamespace=
    AllowVolumeExpansion:  True
    MountOptions:          <none>
    ReclaimPolicy:         Delete
    VolumeBindingMode:     Immediate
    Events:                <none>
    ```

*Clean it*

 ```kubectl delete sc glustersc```

3) Create a Persistent Volume with the Storage class you created.

    ```vi custom_volume.yaml```
    ```bash
    apiVersion: v1
    kind: PersistentVolume
    metadata:
      name: customvolume
      labels:
        of7azurefinal: of7azure
    spec:
      storageClassName: glustersc
      capacity:
        storage: 50Gi
      accessModes:
        - ReadWriteMany
      hostPath:
        path: "/mnt/gluster"
    ```
    ```kubectl create -f custom_volume.yaml```

- Please double check service provider, which provisioner you use for the storage class.

    <img src="./reference_images/access.PNG" title="access">

    *Providers will have different capabilities and each PV’s access modes are set to the specific modes supported by that particular volume.* 

    ```kubectl get pv customvolume```
    ```bash
    NAME          CAPACITY  ACCESS MODES RECLAIM POLICY  STATUS  CLAIM              STORAGECLASS  REASON  AGE
    customvolume  50Gi      RWX          Retain          Bound   default/custompvc  glustersc             18m
    ```
    ``` kubectl describe pv customvolume```
    ```bash
    Name:            customvolume
    Labels:          of7azurefinal=of7azure
    Annotations:     pv.kubernetes.io/bound-by-controller: yes
    Finalizers:      [kubernetes.io/pv-protection]
    StorageClass:    glustersc
    Status:          Bound
    Claim:           default/custompvc
    Reclaim Policy:  Retain
    Access Modes:    RWX
    VolumeMode:      Filesystem
    Capacity:        50Gi
    Node Affinity:   <none>
    Message:
    Source:
        Type:          HostPath (bare host directory volume)
        Path:          /mnt/gluster
        HostPathType:
    Events:            <none>
    ```

*Clean it*

```kubectl delete pv customvolume```

4) Create a Persistent Volume Claim with the Storage class you created.

    ```vi custom_claim.yaml```
    ```bash
    apiVersion: v1
    kind: PersistentVolumeClaim
    metadata:
      name: custompvc
    spec:
      accessModes:
      - ReadWriteMany
      storageClassName: glustersc
      resources:
        requests:
          storage: 50Gi
    ```

    ```kubectl create -f custom_claim.yaml```

- Please match the storage class name with the Persistent Volume you want to use.

    ```kubectl get pvc custompvc```
    ```bash
    NAME        STATUS   VOLUME         CAPACITY   ACCESS MODES   STORAGECLASS   AGE
    custompvc   Bound    customvolume   50Gi       RWX            glustersc      25m
    ```
    ``` kubectl describe pvc custompvc```
    ```bash
    Name:          custompvc
    Namespace:     default
    StorageClass:  glustersc
    Status:        Bound
    Volume:        customvolume
    Labels:        <none>
    Annotations:   pv.kubernetes.io/bind-completed: yes
                   pv.kubernetes.io/bound-by-controller: yes
    Finalizers:    [kubernetes.io/pvc-protection]
    Capacity:      50Gi
    Access Modes:  RWX
    VolumeMode:    Filesystem
    Mounted By:    <none>   ->  changes to the pod name when the pod is attached
    Events:        <none>
    ```

*Clean it*

```kubectl delete pvc custompvc```
    
    
    
    
From https://kubernetes.io/docs/concepts/storage/persistent-volumes

From https://docs.microsoft.com/en-us/azure/aks/azure-files-dynamic-pv

