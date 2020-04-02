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

### 1.2.1 Persistant Volume Claim

**A PersistentVolumeClaim (PVC) is a request for storage by a user. It is similar to a Pod. Pods consume node resources and PVCs consume PV resources. Pods can request specific levels of resources (CPU and Memory). Claims can request specific size and access modes (e.g., they can be mounted once read/write or many times read-only).**

1) Check StorageClasss

    ```kubectl get sc```
    ```bash
    NAME                PROVISIONER                AGE
    azurefile           kubernetes.io/azure-file   6d1h
    azurefile-premium   kubernetes.io/azure-file   6d1h
    default (default)   kubernetes.io/azure-disk   6d1h
    managed-premium     kubernetes.io/azure-disk   6d1h
    ```
* Those four storage classes are provided by Azure service. You can create custom storage class. StorageClass will be discussed in [1.2.3](#123-storage-class).

* In this case, I will use managed-premium to use Azure Kubernetes Service(AKS).

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
      storageClassName: managed-premium
      resources:
        requests:
          storage: 500Gi
    ```
    ``` kubectl create -f volumeclaim.yaml```
    
    ``` kubectl get pvc``` -> Check Persistent Volume Claim (PVC)
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
    Mounted By:    <none>
    Events:        <none>
    ```
-> Now it is ready to mount a pod to Persistent Volume.
    
- While PersistentVolumeClaims allow a user to consume abstract storage resources, it is common that users need PersistentVolumes with varying properties, such as performance, for different problems. Cluster administrators need to be able to offer a variety of PersistentVolumes that differ in more ways than just size and access modes, without exposing users to the details of how those volumes are implemented. For these needs, there is the StorageClass resource. [1.2.3](#123-storage-class)

### 1.2.2 Persistent Volume

**A PersistentVolume (PV) is a piece of storage in the cluster that has been provisioned by an administrator or dynamically provisioned using Storage Classes. It is a resource in the cluster just like a node is a cluster resource.**

1) Create Persistent Volume with Azure Kubernetes Service.

- From the PVC above, it uses managed-premium storageClass whose provisioner is kubernetes.io/**azure-disk**. It automatically generates **AzureDisk**(Persistant Volume) in Azure service.
    
    <img src="./reference_images/disk01.PNG" title="disk01">
     
- When you create a Pod using the PV, **Disk state** changes from Unattached to Attached & **Owner VM** changes from --(none) to the VM where the Pod is running. (Creating a Deployment which uses the PVC will be discussed in [1.3](#13-deployment-with-replicated-pods).
        
    <img src="./reference_images/disk02.PNG" title="disk02">
     
- You can also check PV with kubectl commands
     
    ``` kubectl get pv ```
    ```bash
    NAME                                     CAPACITY ACCESS MODES RECLAIM POLICY STATUS CLAIM              STORAGECLASS   REASON   AGE
    pvc-a0a48609-8975-433f-9b73-bc371cbb0702 500Gi    RWO          Delete         Bound  default/of7storage managed-premium         16h
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
    
2) Create a custom Persistent Volume with Kubernetes.

    ``` vi volume.yaml```
    ```bash
    apiVersion: v1
    kind: PersistentVolume
    metadata:
      name: customvolume
    spec:
      capacity:
        storage: 500Gi
      volumeMode: Filesystem
      accessModes:
        - ReadWriteOnce
      persistentVolumeReclaimPolicy: Recycle
      storageClassName: customclass
      mountOptions:
        - hard
        - nfsvers=4.1
      nfs:
        path: /tmp
        server: 172.17.0.2
    ```

**PVs are volume plugins like Volumes, but have a lifecycle independent of any individual Pod that uses the PV.** This API object captures the details of the implementation of the storage, be that NFS, iSCSI, or a cloud-provider-specific storage system.

### 1.2.3 Storage Class

* A claim can request a particular class by specifying the name of a StorageClass using the attribute storageClassName. Only PVs of the requested class, ones with the same storageClassName as the PVC, can be bound to the PVC.

* Access Modes

    ```bash
    ReadWriteOnce – the volume can be mounted as read-write by a single node (RWO)
    ReadOnlyMany – the volume can be mounted read-only by many nodes (ROX) 
    ReadWriteMany – the volume can be mounted as read-write by many nodes (RWX)
    ```
A PersistentVolume can be mounted on a host in any way supported by the resource provider. As shown in the table below, providers will have different capabilities and each PV’s access modes are set to the specific modes supported by that particular volume. For example, NFS can support multiple read/write clients, but a specific NFS PV might be exported on the server as read-only. Each PV gets its own set of access modes describing that specific PV’s capabilities.

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

*Mount options* 

 ```bash
dir_mode=0777
file_mode=0777
uid=0
gid=0
mfsymlinks
cache=strict
```

```kubectl apply -f azure_sc.yaml```

``` vi custom_sc.yaml ```
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
```

```kubectl create -f custom_sc.yaml```

### Use the custom Persistent Volume tutorial

1) Check the access mode you want to configure and choose the storage service.

- Access Modes

    ```bash
    ReadWriteOnce – the volume can be mounted as read-write by a single node (RWO)
    ReadOnlyMany – the volume can be mounted read-only by many nodes (ROX) 
    ReadWriteMany – the volume can be mounted as read-write by many nodes (RWX)
    ```
- Storage services with access modes

<img src="./reference_images/access.PNG" title="access">

2) Create a storage class with the provisioner you want.

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
    Mounted By:    <none>
    Events:        <none>
    ```
      
    *Clean it*
    
    ```kubectl delete pvc custompvc```
    
### 1.3 Deployment with replicated Pods

-> The reason why Deployment for creating replicated Pods will be used is - updating the Deployment(in this case, OpenFrame) is more suitable than using Replication controller.(It only replicates the Pods, do not supports rolling-back and rolling-out for updating the application.) It will be discussed later.




```
Other docker commands :

| COMMAND                    | DESCRIPTION                     |
|----------------------------|---------------------------------|
| docker ps                  | check running containers        |
| docker ps -a               | check all containers            |
| docker exec [container ID] | execute a running container     |
| docker stop [container ID] | stop the container              |
| docker rm   [container ID] | remove the container            |
```

* Extra Packages if needed
```bash
yum install strace
yum install ltrace
yum install gdb 
yum install nano 
yum install vim-enhanced 
yum install git 
yum install htop
```

__b.__ Create symbolic link
```bash
ln -s /usr/lib64/libncurses.so.5.9 /usr/lib/libtermcap.so
ln -s /usr/lib64/libncurses.so.5.9 /usr/lib/libtermcap.so.2
```

__c.__ Kernel Parameters Modification 

vi /etc/sysctl.conf  

```bash
kernel.shmall = 2097152
kernel.shmmax = 4294967295
kernel.shmmni = 4096
kernel.sem = 100000 32000 10000 10000
fs.file-max = 65536
net.ipv4.ip_local_port_range = 1024 65000  
```
* The parameters below are not supported in a container environment, so you can discard those.
```bash
net.core.rmem_default=262144
net.core.wmem_default=262144
net.core.rmem_max=262144
net.core.wmem_max=262144
```
```bash
sysctl: cannot stat /proc/sys/net/core/rmem_default: No such file or directory
sysctl: cannot stat /proc/sys/net/core/wmem_default: No such file or directory
sysctl: cannot stat /proc/sys/net/core/rmem_max: No such file or directory
sysctl: cannot stat /proc/sys/net/core/wmem_max: No such file or directory
```

* Refresh the kernel parameters.
```bash
/sbin/sysctl –p 
```

__d.__ Firewall setting
* Firewall does not work in the container. Instead, you can use port forwarding option(-p) when you run the container. I will talk about this later in 'use OpenFrame image' part.

__e.__ Prepare licenses from Technet
* Use the correct hostname for downloading license files from Technet website.
* You need to check hostname and the number of cores.

__f.__ Set hostname
* Use -h option when you run the container. It automatically sets the hostname for the container.
* Check /etc/hosts file to see if the hostname sets correctly.



From https://kubernetes.io/docs/concepts/storage/persistent-volumes

From https://docs.microsoft.com/en-us/azure/aks/azure-files-dynamic-pv

