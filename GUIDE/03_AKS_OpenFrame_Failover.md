# Fail-over test with Azure Kubernetes service(AKS) by Kelsey

## Table of Contents

+ [1. Fail-over Environment setting](#1-fail-over-environment-setting)
  + [1-1. Fail-over concept](#1-1-fail-over-concept)
  + [1-2. Configure data Volume](#1-2-configure-data-volume)
     + [A. Storage Class (SC)](#a-storage-class-sc)
     + [B. Persistent Volume (PV)](#b-persistent-volume-pv)
     + [C. Persistent Volume Claim (PVC)](#c-persistent-volume-claim-pvc)
  + [1-3 Use Persistent Volume with Pod replication](#1-3-use-persistent-volume-with-pod-replication)
     + [1-3.1 Use Persistent Volume with Azure Kubernetes Service](#1-31-use-persistent-volume-with-azure-kubernetes-service)
     + [1-3.2 Use the custom Persistent Volume with replicated Pod](#1-32-use-the-custom-persistent-volume-with-replicated-pod)
       + [1-3.2.1 Create NFS Server in Azure](#1-321-create-nfs-server-in-azure)
       + [1-3.2.2 Create NFS Share in NFS Server](#1-322-create-nfs-share-in-nfs-server)
       + [1-3.2.3 Use NFS Server in Azure service](#1-323-use-nfs-server-in-azure-service)
+ [2. Fail-over Test](#2-fail-over-test)
  + [2-1. Test Senario & Result](#2-1-test-senario--result)

# 1. Fail-over Environment setting

## 1-1. Fail-over concept

* A Pod which has OpenFrame container is running in NODE1. NODE2 is a back up(empty) Node.

<img src="./reference_images/fail01.PNG" title="fail01">

* When NODE1 dies, the Pod will be terminated from NODE1 and a new one will be created in NODE2.

<img src="./reference_images/fail02.PNG" title="fail02">

*A Pod is the basic execution unit of a Kubernetes application–the smallest and simplest unit in the Kubernetes object model that you create or deploy. A Pod represents processes running on your Cluster.*

**Here are two check points for doing the fail-over test.**

When NODE1 dies,

1) A new Pod should not lose the critical data of the old Pod.
- Persistent Volume and Persistent Volume Claim will be used(Dynamic Provisioning).

2) A new Pod should automatically be created in a different Node and run successfully.
- Deployment with replicated Pods will be used(in this case, only one Pod is needed).

## 1-2. Configure data Volume

### A. Storage Class (SC) 

* A claim can request a particular class by specifying the name of a StorageClass using the attribute storageClassName. Only PVs of the requested class, ones with the same storageClassName as the PVC, can be bound to the PVC.

* Access Modes

    ```bash
    ReadWriteOnce – the volume can be mounted as read-write by a single node (RWO)
    ReadOnlyMany  – the volume can be mounted read-only by many nodes        (ROX) 
    ReadWriteMany – the volume can be mounted as read-write by many nodes    (RWX)
    ```
* A PersistentVolume can be mounted on a host in any way supported by the resource provider. As shown in the table below, providers will have different capabilities and each PV’s access modes are set to the specific modes supported by that particular volume. For example, NFS can support multiple read/write clients, but a specific NFS PV might be exported on the server as read-only. Each PV gets its own set of access modes describing that specific PV’s capabilities.

   <img src="./reference_images/access.PNG" title="access">

* Storage Class sample yaml file

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

### B. Persistent Volume (PV) 

- A PersistentVolume (PV) is a piece of storage in the cluster that has been provisioned by an administrator or dynamically provisioned using Storage Classes. It is a resource in the cluster just like a node is a cluster resource.

- **PVs are volume plugins like Volumes, but have a lifecycle independent of any individual Pod that uses the PV.** This API object captures the details of the implementation of the storage, be that NFS, iSCSI, or a cloud-provider-specific storage system.

### C. Persistent Volume Claim (PVC) 

* A Persistent Volume Claim (PVC) is a request for storage by a user. It is similar to a Pod. Pods consume node resources and PVCs consume PV resources. Pods can request specific levels of resources (CPU and Memory). Claims can request specific size and access modes (e.g., they can be mounted once read/write or many times read-only).

* While PersistentVolumeClaims allow a user to consume abstract storage resources, it is common that users need PersistentVolumes with varying properties, such as performance, for different problems. Cluster administrators need to be able to offer a variety of PersistentVolumes that differ in more ways than just size and access modes, without exposing users to the details of how those volumes are implemented. For these needs, there is the StorageClass resource.

## 1-3 Use Persistent Volume with Pod replication

**Dynamic provisioning should be used for fail-over test.**

*If you use Static provisioning, you need to create a volume for each Pod. For fail-over test, Pods will be deleted and created multiple times. A Volume shoule be automatically attached to the running Pod. So, Dynamic provisioning is suitable for fail-over test.*

### 1-3.1 Use Persistent Volume with Azure Kubernetes Service

1) Check Storage Class.

    ```kubectl get sc```
    ```bash
    NAME                PROVISIONER                AGE
    azurefile           kubernetes.io/azure-file   6d1h
    azurefile-premium   kubernetes.io/azure-file   6d1h
    default (default)   kubernetes.io/azure-disk   6d1h
    managed-premium     kubernetes.io/azure-disk   6d1h
    ```
* Those four Storage Classes are provided by Azure Service. You can create a custom Storage Class when you use custom Persistent Volume.

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

2) Create a Persistent Volume Claim with the chosen Storage Class.

    ```vi volumeclaim.yaml```
    ```bash 
    apiVersion: v1
    kind: PersistentVolumeClaim
    metadata:
      name: of7storage
    spec:
      accessModes:
      - ReadWriteOnce
      storageClassName: managed-premium  # If you skip the setting, it is set to default
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
    Mounted By:    <none>  # it will be changed to the name of the Pod when the Pod is attached
    Events:        <none>
    ```
    
*Clean it*

    kubectl delete pvc of7storage
    
3) Persistent Volume is automatically generated with Azure Kubernetes Service.

- From the PVC above, it uses managed-premium Storage Class whose Provisioner is kubernetes.io/**azure-disk**. 
- It automatically generates **AzureDisk**(Persistent Volume) in Azure service.
    
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
    
4) Create a Pod using the Persistent Volume.

*The reason why I chose Deployment for creating replicated Pods is - updating the Deployment(in this case, OpenFrame) is more suitable than using Replication Controller.(It only replicates the Pods, do not supports rolling-back and rolling-out for updating the application.)*

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
              mountPath: /mnt/azure # /mnt/azure directory is created in a container to use the volume.
          volumes:
          - name: sharedvolume
            persistentVolumeClaim:
              claimName: of7storage
    ```
    ```kubectl get deployments```
    ```bash
    NAME       READY   UP-TO-DATE   AVAILABLE   AGE
    of7azure   1/1     1            1           7m4s
    ```

    ```kubectl describe deployment of7azure```
    ```bash
    Name:                   of7azure
    Namespace:              default
    CreationTimestamp:      Thu, 02 Apr 2020 12:37:03 +0000
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
    Events:
      Type    Reason             Age   From                   Message
      ----    ------             ----  ----                   -------
      Normal  ScalingReplicaSet  10m   deployment-controller  Scaled up replica set of7azure-76db5dbccb to 1
    ```
    
    ```kubectl get pods```
    ```bash
    NAME                        READY   STATUS    RESTARTS   AGE
    of7azure-76db5dbccb-brgrs   1/1     Running   0          9m
    ```
    
    ```kubectl describe pod of7azure-76db5dbccb-brgrs```
    ```bash
    Name:           of7azure-76db5dbccb-brgrs
    Namespace:      default
    Priority:       0
    Node:           aks-agentpool-24893396-1/10.240.0.35
    Start Time:     Thu, 02 Apr 2020 12:37:03 +0000
    Labels:         of7azurefinal=of7azure
                    pod-template-hash=76db5dbccb
    Annotations:    <none>
    Status:         Running
    IP:             10.240.0.40
    IPs:            <none>
    Controlled By:  ReplicaSet/of7azure-76db5dbccb
    Containers:
      of7azure:
        Container ID:  docker://cd6add2e2d48d92c2ab04ffb0b35491b475b7a049161175789302ee2c7b9a3f5
        Image:         kelsey92/of7azurefinal:of7azure
        Image ID:      docker-pullable://kelsey92/of7azurefinal@sha256:8e707e0444eec3af2842a34de8360781f0dd9ed85ad620b0856a8c7368029603
        Port:          6066/TCP
        Host Port:     0/TCP
        Command:
          /bin/sh
          -ec
          while :; do echo '.'; sleep 5 ; done
        State:          Running
          Started:      Thu, 02 Apr 2020 12:42:40 +0000
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
      Type    Reason                  Age    From                               Message
      ----    ------                  ----   ----                               -------
      Normal  Scheduled               7m52s  default-scheduler                  Successfully assigned default/of7azure-76db5dbccb-brgrs to aks-agentpool-24893396-1
      Normal  SuccessfulAttachVolume  7m41s  attachdetach-controller            AttachVolume.Attach succeeded for volume "pvc-a0a48609-8975-433f-9b73-bc371cbb0702"
      Normal  Pulling                 7m32s  kubelet, aks-agentpool-24893396-1  Pulling image "kelsey92/of7azurefinal:of7azure"
      Normal  Pulled                  2m48s  kubelet, aks-agentpool-24893396-1  Successfully pulled image "kelsey92/of7azurefinal:of7azure"
      Normal  Created                 2m16s  kubelet, aks-agentpool-24893396-1  Created container of7azure
      Normal  Started                 2m15s  kubelet, aks-agentpool-24893396-1  Started container of7azure
    ```
- Check the Persistent Volume from the container.

    *This Volume will use the Azure Disk which does not vanish when the container is dead.*

    ```kubectl exec -it [pod name] -- /bin/bash```

    ```cd /mnt/azure/```
    ```bash
    [of7azure@of7azure mnt]$ ls -rtl
    total 4
    drwxrwxrwx 11 root root 4096 Apr  1 12:49 azure
    ```

- Check the PV status from Azure Service.

    <img src="./reference_images/disk02.PNG" title="disk02">

    **Disk state** changes from Unattached to Attached.
    
    **Owner VM**   changes from --(none)   to the VM where the Pod is running.

     
*Clean it*

```kubectl delete deployment of7azure```

```kubectl delete pod of7azure-76db5dbccb-96q4k```

### 1-3.2 Use the custom Persistent Volume with replicated Pod

**Check the Access Mode you want to configure and choose the right storage service.**

- Access Modes

    ```bash
    ReadWriteOnce – the volume can be mounted as read-write by a single node (RWO)
    ReadOnlyMany – the volume can be mounted read-only by many nodes (ROX) 
    ReadWriteMany – the volume can be mounted as read-write by many nodes (RWX)
    ```
- Storage Services with Access Modes

    *Providers will have different capabilities and each PV’s access modes are set to the specific modes supported by that particular volume.* 
    <img src="./reference_images/access.PNG" title="access">

**In the following part, I will use NFS Server for ReadWriteMany access**
    
#### 1-3.2.1 Create NFS Server in Azure 

1) Add resource as NFS Server

- Find the resource and click the create button.
    
    <img src="./reference_images/NFS_add.PNG" title="NFS_add">
    
- Customize the size by clicking "Change size" from Size section and create Administrator account by filling in Username and Password fields.
    
    <img src="./reference_images/NFS_VM.PNG" title="NFS_VM">

- Create a disk attaching the VM.
    
    <img src="./reference_images/NFS_VM02.PNG" title="NFS_VM02">
    
    <img src="./reference_images/NFS_VM03.PNG" title="NFS_VM03">
    
- Check Networking configuration.
    
    <img src="./reference_images/NFS_VM04.PNG" title="NFS_VM04">
     
- Check Management configuration.
    
    <img src="./reference_images/NFS_VM05.PNG" title="NFS_VM05">
    
- Check Advanced setting.
    
    <img src="./reference_images/NFS_VM06.PNG" title="NFS_VM06">
    
*Completed creating NFS Server!*
    
2) Connect to NFS Server

    **Connect with RDP**
    
- NFSserver VM - Connect - RDP - Download RDP File - Open the file.
    
    <img src="./reference_images/NFS_VM07.PNG" title="NFS_VM07">
    
- Type the Admministrator account(set in 1-b step) information to login.
    
    <img src="./reference_images/NFS_VM08.PNG" title="NFS_VM08">
    
 - Connected to NFS Server.
    
    <img src="./reference_images/NFS_VM09.PNG" title="NFS_VM089">
       
#### 1-3.2.2 Create NFS Share in NFS Server

Reference : https://cloudinfrastructureservices.co.uk/how-to-setup-nfs-server-2016-2019-in-azure-aws-gcp/

1) Create NFS Share using Server Manager
    
- Dashboard
    
    <img src="./reference_images/NFS_server.PNG" title="NFS_server">

- Click **File and Storage Services - Shares** and hit **New Share** button.
    
    <img src="./reference_images/NFS_server01.PNG" title="NFS_server01">
    
- Select **NFS Share - Quick**.
    
    <img src="./reference_images/NFS_server02.PNG" title="NFS_server02">
    
- Create a folder you want to share beforehand and click **"Type a custom path" - "Browse"** button and select the foder.(it will be a local path in NFS Server.)
    
    <img src="./reference_images/NFS_server03.PNG" title="NFS_server03">
    
- **Remote path to share** is important.
    
    <img src="./reference_images/NFS_server04.PNG" title="NFS_server04">
    
- Check all boxes in Authentication setting.
    
    <img src="./reference_images/NFS_server_checkallboxes.PNG" title="NFS_server_checkallboxes">
    
- Customize the Share Permissions(click Add button) & Permissions.(click Customize permissions button) (*I checked All Machines in this setting*)
    
    <img src="./reference_images/NFS_server_permmision.PNG" title="NFS_server_permmision">
    
    <img src="./reference_images/NFS_server_permmision01.PNG" title="NFS_server_permmision01">
    
    <img src="./reference_images/NFS_server_permmision02.PNG" title="NFS_server_permmision02">
    
- Confirm it and complete the setting.
    
    <img src="./reference_images/NFS_server_permmision03.PNG" title="NFS_server_permmision03">
    
    <img src="./reference_images/NFS_server_permmision_last.PNG" title="NFS_server_permmision_last">
    
    <img src="./reference_images/NFS_server_result.PNG" title="NFS_server_result">
    
#### 1-3.2.3 Use NFS Server in Azure service

Reference : https://gruuuuu.github.io/cloud/k8s-volume/#

1) Create a Service Account which the Provisioner will use.

	```vi nfs-prov-sa.yaml```

	```bash
	kind: ServiceAccount
	apiVersion: v1
	metadata:
	  name: nfs-pod-provisioner-sa
	---
	kind: ClusterRole # Role of kubernetes
	apiVersion: rbac.authorization.k8s.io/v1 # auth API
	metadata:
	  name: nfs-provisioner-clusterrole
	rules:
	  - apiGroups: [""] # rules on persistentvolumes
	    resources: ["persistentvolumes"]
	    verbs: ["get", "list", "watch", "create", "delete"]
	  - apiGroups: [""]
	    resources: ["persistentvolumeclaims"]
	    verbs: ["get", "list", "watch", "update"]
	  - apiGroups: ["storage.k8s.io"]
	    resources: ["storageclasses"]
	    verbs: ["get", "list", "watch"]
	  - apiGroups: [""]
	    resources: ["events"]
	    verbs: ["create", "update", "patch"]
	---
	kind: ClusterRoleBinding
	apiVersion: rbac.authorization.k8s.io/v1
	metadata:
	  name: nfs-provisioner-rolebinding
	subjects:
	  - kind: ServiceAccount
	    name: nfs-pod-provisioner-sa
	    namespace: default
	roleRef: # binding cluster role to service account
	  kind: ClusterRole
	  name: nfs-provisioner-clusterrole # name defined in clusterRole
	  apiGroup: rbac.authorization.k8s.io
	---
	kind: Role
	apiVersion: rbac.authorization.k8s.io/v1
	metadata:
	  name: nfs-pod-provisioner-otherroles
	rules:
	  - apiGroups: [""]
	    resources: ["endpoints"]
	    verbs: ["get", "list", "watch", "create", "update", "patch"]
	---
	kind: RoleBinding
	apiVersion: rbac.authorization.k8s.io/v1
	metadata:
	  name: nfs-pod-provisioner-otherroles
	subjects:
	  - kind: ServiceAccount
	    name: nfs-pod-provisioner-sa # same as top of the file
	    # replace with namespace where provisioner is deployed
	    namespace: default
	roleRef:
	  kind: Role
	  name: nfs-pod-provisioner-otherroles
	  apiGroup: rbac.authorization.k8s.io
	  ```
  
	```kubectl create -f nfs-prov-sa.yaml```
  
	*serviceaccount/nfs-pod-provisioner-sa created*

	*clusterrole.rbac.authorization.k8s.io/nfs-provisioner-clusterrole created*

	*clusterrolebinding.rbac.authorization.k8s.io/nfs-provisioner-rolebinding created*

	*role.rbac.authorization.k8s.io/nfs-pod-provisioner-otherroles created*

	*rolebinding.rbac.authorization.k8s.io/nfs-pod-provisioner-otherroles created*
 
2) Create a Storage Class with the Provisioner you want to use.

- Persistent Volume Claim(PVC) will request the Volume by Storage Class name.

    ```vi nfs_sc.yaml```
    ```bash
    apiVersion: storage.k8s.io/v1
    kind: StorageClass
    metadata:
      name: nfs-storageclass # IMPORTANT pvc needs to mention this name
    provisioner: nfs-of7azure # name can be anything
    parameters:
      archiveOnDelete: "false"
    ```
    ```kubectl create -f nfs_sc.yaml```

    - Parameters may vary depends on the Provisioner.

    ```kubectl get sc nfs-storageclass```
    ```bash
    NAME               PROVISIONER    AGE
    nfs-storageclass   nfs-of7azure   26h
    ```
    ```kubectl describe sc nfs-storageclass```
    ```bash
    Name:                  nfs-storageclass
    IsDefaultClass:        No
    Annotations:           <none>
    Provisioner:           nfs-of7azure
    Parameters:            archiveOnDelete=false
    AllowVolumeExpansion:  <unset>
    MountOptions:          <none>
    ReclaimPolicy:         Delete
    VolumeBindingMode:     Immediate
    Events:                <none>
    ```

*Clean it*

 ```kubectl delete sc nfs-storageclass```

3) Create a Provisioner which can automatically generates Persistent Volume(PV).

	```vi nfs_provisioner.yaml```

	```bash
	kind: Deployment
	apiVersion: apps/v1
	metadata:
	  name: nfs-pod-provisioner
	spec:
	  selector:
	    matchLabels:
	      app: nfs-pod-provisioner
	  replicas: 1
	  strategy:
	    type: Recreate
	  template:
	    metadata:
	      labels:
		app: nfs-pod-provisioner
	    spec:
	      serviceAccountName: nfs-pod-provisioner-sa # name of service account
	      containers:
		- name: nfs-pod-provisioner
		  image: quay.io/external_storage/nfs-client-provisioner:latest
		  volumeMounts:
		    - name: nfs-provisioner-vol
		      mountPath: "/mnt/azure"
		  env:
		    - name: PROVISIONER_NAME # do not change
		      value: nfs-of7azure # SAME AS PROVISIONER NAME VALUE IN STORAGECLASS
		    - name: NFS_SERVER # do not change
		      value: 65.52.2.96 # Ip of the NFS SERVER (NFS VM public IP) 
		    - name: NFS_PATH # do not change
		      value: "/azure_share" # path to nfs directory setup (Remote path to share from NFS share)
	      volumes:
	       - name: nfs-provisioner-vol # same as volumemouts name
		 nfs:
		   server: 65.52.2.96 #same as above 
		   path: "/azure_share" #same as above 
	```

	```kubectl create -f nfs_provisioner.yaml```
	
	```kubectl describe deployment nfs-pod-provisioner```

	```bash
	Name:               nfs-pod-provisioner
	Namespace:          default
	CreationTimestamp:  Tue, 07 Apr 2020 07:38:51 +0000
	Labels:             <none>
	Annotations:        deployment.kubernetes.io/revision: 1
	Selector:           app=nfs-pod-provisioner
	Replicas:           1 desired | 1 updated | 1 total | 1 available | 0 unavailable
	StrategyType:       Recreate
	MinReadySeconds:    0
	Pod Template:
	  Labels:           app=nfs-pod-provisioner
	  Service Account:  nfs-pod-provisioner-sa
	  Containers:
	   nfs-pod-provisioner:
	    Image:      quay.io/external_storage/nfs-client-provisioner:latest
	    Port:       <none>
	    Host Port:  <none>
	    Environment:
	      PROVISIONER_NAME:  nfs-of7azure
	      NFS_SERVER:        65.52.2.96
	      NFS_PATH:          /azure_share
	    Mounts:
	      /mnt/azure from nfs-provisioner-vol (rw)
	  Volumes:
	   nfs-provisioner-vol:
	    Type:      NFS (an NFS mount that lasts the lifetime of a pod)
	    Server:    65.52.2.96
	    Path:      /azure_share
	    ReadOnly:  false
	Conditions:
	  Type           Status  Reason
	  ----           ------  ------
	  Available      True    MinimumReplicasAvailable
	  Progressing    True    NewReplicaSetAvailable
	OldReplicaSets:  <none>
	NewReplicaSet:   nfs-pod-provisioner-56f87f4bc6 (1/1 replicas created)
	Events:
	  Type    Reason             Age   From                   Message
	  ----    ------             ----  ----                   -------
	  Normal  ScalingReplicaSet  41m   deployment-controller  Scaled up replica set nfs-pod-provisioner-56f87f4bc6 to 1
	```
4) Check the created Pod(replica) from the deployment.
    
	```kubectl describe pod nfs-pod-provisioner-56f87f4bc6-n6nfv```

	```bash
	Name:           nfs-pod-provisioner-56f87f4bc6-n6nfv
	Namespace:      default
	Priority:       0
	Node:           aks-agentpool-24893396-0/10.240.0.4
	Start Time:     Tue, 07 Apr 2020 07:38:51 +0000
	Labels:         app=nfs-pod-provisioner
			pod-template-hash=56f87f4bc6
	Annotations:    <none>
	Status:         Running
	IP:             10.240.0.22
	IPs:            <none>
	Controlled By:  ReplicaSet/nfs-pod-provisioner-56f87f4bc6
	Containers:
	  nfs-pod-provisioner:
	    Container ID:   docker://5e31e64d642d6c835f7ef0c5b903d5fe74be9ef278a1e045f1d46b32a74e544e
	    Image:          quay.io/external_storage/nfs-client-provisioner:latest
	    Image ID:       docker-pullable://quay.io/external_storage/nfs-client-provisioner@sha256:022ea0b0d69834b652a4c53655d78642ae23f0324309097be874fb58d09d2919
	    Port:           <none>
	    Host Port:      <none>
	    State:          Running
	      Started:      Tue, 07 Apr 2020 07:38:56 +0000
	    Ready:          True
	    Restart Count:  0
	    Environment:
	      PROVISIONER_NAME:  nfs-of7azure
	      NFS_SERVER:        65.52.2.96
	      NFS_PATH:          /azure_share
	    Mounts:
	      /mnt/azure from nfs-provisioner-vol (rw)
	      /var/run/secrets/kubernetes.io/serviceaccount from nfs-pod-provisioner-sa-token-f8zzb (ro)
	Conditions:
	  Type              Status
	  Initialized       True
	  Ready             True
	  ContainersReady   True
	  PodScheduled      True
	Volumes:
	  nfs-provisioner-vol:
	    Type:      NFS (an NFS mount that lasts the lifetime of a pod)
	    Server:    65.52.2.96
	    Path:      /azure_share
	    ReadOnly:  false
	  nfs-pod-provisioner-sa-token-f8zzb:
	    Type:        Secret (a volume populated by a Secret)
	    SecretName:  nfs-pod-provisioner-sa-token-f8zzb
	    Optional:    false
	QoS Class:       BestEffort
	Node-Selectors:  <none>
	Tolerations:     node.kubernetes.io/not-ready:NoExecute for 300s
			 node.kubernetes.io/unreachable:NoExecute for 300s
	Events:
	  Type    Reason     Age   From                               Message
	  ----    ------     ----  ----                               -------
	  Normal  Scheduled  41m   default-scheduler                  Successfully assigned default/nfs-pod-provisioner-56f87f4bc6-n6nfv to aks-agentpool-24893396-0
	  Normal  Pulling    41m   kubelet, aks-agentpool-24893396-0  Pulling image "quay.io/external_storage/nfs-client-provisioner:latest"
	  Normal  Pulled     41m   kubelet, aks-agentpool-24893396-0  Successfully pulled image "quay.io/external_storage/nfs-client-provisioner:latest"
	  Normal  Created    41m   kubelet, aks-agentpool-24893396-0  Created container nfs-pod-provisioner
	  Normal  Started    41m   kubelet, aks-agentpool-24893396-0  Started container nfs-pod-provisioner
	```

*Clean it*

```kubectl delete deployment nfs-pod-provisioner```

```kubectl delete pod nfs-pod-provisioner-56f87f4bc6-n6nfv```

5) Create Persistent Volume Claim(PVC) with the correct Storage Class name.

- Use the custom Storage Class

    ```vi nfs_pvc.yaml```
    ```bash
    apiVersion: v1
    kind: PersistentVolumeClaim
    metadata:
      name: nfs-pvc
    spec:
      storageClassName: nfs-storageclass # SAME NAME AS THE STORAGECLASS
      accessModes:
        - ReadWriteMany
      resources:
        requests:
          storage: 200Gi
    ```

    ```kubectl create -f nfs_pvc.yaml```

    ```kubectl get pvc nfs-pvc```
    ```bash
	NAME      STATUS   VOLUME                                     CAPACITY   ACCESS MODES   STORAGECLASS       AGE
	nfs-pvc   Bound    pvc-6633fc99-4e54-45e6-8e3b-e322a52817ad   200Gi      RWX            nfs-storageclass   2m38s
    ```
    ``` kubectl describe pvc nfs-pvc```
    ```bash
	Name:          nfs-pvc
	Namespace:     default
	StorageClass:  nfs-storageclass
	Status:        Bound
	Volume:        pvc-6633fc99-4e54-45e6-8e3b-e322a52817ad # This volume is created automatically
	Labels:        <none>
	Annotations:   pv.kubernetes.io/bind-completed: yes
		       pv.kubernetes.io/bound-by-controller: yes
		       volume.beta.kubernetes.io/storage-provisioner: nfs-of7azure
	Finalizers:    [kubernetes.io/pvc-protection]
	Capacity:      200Gi
	Access Modes:  RWX
	VolumeMode:    Filesystem
	Mounted By:    <none> # it will be changed to the name of the Pod when the Pod is attached (ex.nfsof7azure-848d8d6cc7-vvbnq)
	Events:
	  Type    Reason                 Age   From                                                                                    Message
	  ----    ------                 ----  ----                                                                                    -------
	  Normal  ExternalProvisioning   20s   persistentvolume-controller                                                             waiting for a volume to be created, either by external provisioner "nfs-of7azure" or manually created by system administrator
	  Normal  Provisioning           20s   nfs-of7azure_nfs-pod-provisioner-56f87f4bc6-n6nfv_d660b65f-78a2-11ea-8d62-9eea44f1ea23  External provisioner is provisioning volume for claim "default/nfs-pvc"
	  Normal  ProvisioningSucceeded  20s   nfs-of7azure_nfs-pod-provisioner-56f87f4bc6-n6nfv_d660b65f-78a2-11ea-8d62-9eea44f1ea23  Successfully provisioned volume pvc-6633fc99-4e54-45e6-8e3b-e322a52817ad
    ```

*Clean it*

```kubectl delete pvc nfs-pvc```

6) From step 5(from aboove), Persistent Volume(PV) is automatically created and Persistent Volume Claim(PVC) will be bounded successfully.

- Check the created Persistent Volume(PV).

    ```kubectl get pv pvc-6633fc99-4e54-45e6-8e3b-e322a52817ad```
    ```bash
    NAME                                     CAPACITY ACCESS MODES RECLAIM POLICY  STATUS CLAIM           STORAGECLASS    REASON AGE
    pvc-6633fc99-4e54-45e6-8e3b-e322a52817ad 200Gi    RWX          Delete          Bound  default/nfs-pvc nfs-storageclass       4m56s
    ```
    ```kubectl describe pv pvc-6633fc99-4e54-45e6-8e3b-e322a52817ad```
    ```bash
	Name:            pvc-6633fc99-4e54-45e6-8e3b-e322a52817ad
	Labels:          <none>
	Annotations:     pv.kubernetes.io/provisioned-by: nfs-of7azure
	Finalizers:      [kubernetes.io/pv-protection]
	StorageClass:    nfs-storageclass
	Status:          Bound
	Claim:           default/nfs-pvc
	Reclaim Policy:  Delete
	Access Modes:    RWX
	VolumeMode:      Filesystem
	Capacity:        200Gi
	Node Affinity:   <none>
	Message:
	Source:
	    Type:      NFS (an NFS mount that lasts the lifetime of a pod)
	    Server:    65.52.2.96
	    Path:      /azure_share/default-nfs-pvc-pvc-6633fc99-4e54-45e6-8e3b-e322a52817ad # This will be the shared path from NFS Server.
	    ReadOnly:  false
	Events:        <none>
    ```
    
- Create a folder with the name of PV under the Remote shared path.

    ```Path:      /azure_share/default-nfs-pvc-pvc-6633fc99-4e54-45e6-8e3b-e322a52817ad```
    
    <img src="./reference_images/NFS_folder.PNG" title="NFS_folder">
        
*Clean it*

```kubectl delete pv pvc-6633fc99-4e54-45e6-8e3b-e322a52817ad```

``` delete the folder under the Remote shared path```

7) Create a Pod using the Persistent Volume Claim(PVC).

    ```vi nfs_deployment.yaml```

	```bash
	apiVersion: extensions/v1beta1
	kind: Deployment
	metadata:
	  name: nfsof7azure
	spec:
	  replicas: 1
	  template:
	    metadata:
	      name: of7azure
	      labels:
		of7azurefinal: nfsof7azure # Labels are the mechanism you use to organize Kubernetes objects 
	    spec:
	      hostname: of7azure
	      containers:
	      - name: of7azure
		image: kelsey92/of7azurefinal:of7azure
		ports:
		- containerPort: 6066
		command: ["/bin/sh", "-ec", "while :; do echo '.'; sleep 5 ; done"]
		lifecycle:
		  preStop:
		    exec:
		      command: [ "/bin/sleep", "120" ]
		volumeMounts:
		- name: sharedvolume
		  mountPath: /mnt/azure 
	      volumes:
	      - name: sharedvolume
		persistentVolumeClaim:
		  claimName: nfs-pvc # use the created PVC 
	```
	
	```kubectl create -f nfs_deployment.yaml```
	
	```kubectl describe deployment nfsof7azure```
	```bash
	Name:                   nfsof7azure
	Namespace:              default
	CreationTimestamp:      Wed, 08 Apr 2020 05:47:51 +0000
	Labels:                 of7azurefinal=nfsof7azure
	Annotations:            deployment.kubernetes.io/revision: 1
	Selector:               of7azurefinal=nfsof7azure
	Replicas:               1 desired | 1 updated | 1 total | 1 available | 0 unavailable
	StrategyType:           RollingUpdate
	MinReadySeconds:        0
	RollingUpdateStrategy:  1 max unavailable, 1 max surge
	Pod Template:
	  Labels:  of7azurefinal=nfsof7azure
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
	    ClaimName:  nfs-pvc
	    ReadOnly:   false
	Conditions:
	  Type           Status  Reason
	  ----           ------  ------
	  Available      True    MinimumReplicasAvailable
	OldReplicaSets:  <none>
	NewReplicaSet:   nfsof7azure-848d8d6cc7 (1/1 replicas created) # The deployment created the replica set and it created a Pod
	Events:
	  Type    Reason             Age    From                   Message
	  ----    ------             ----   ----                   -------
	  Normal  ScalingReplicaSet  8m11s  deployment-controller  Scaled up replica set nfsof7azure-848d8d6cc7 to 1
	```
	
	```kubectl get pod nfsof7azure-848d8d6cc7-vvbnq```
	```bash
	NAME                           READY   STATUS    RESTARTS   AGE
	nfsof7azure-848d8d6cc7-vvbnq   1/1     Running   0          9m46s
	
	```kubectl describe pod nfsof7azure-848d8d6cc7-vvbnq```
	```bash
	Name:           nfsof7azure-848d8d6cc7-vvbnq
	Namespace:      default
	Priority:       0
	Node:           aks-agentpool-24893396-1/10.240.0.35
	Start Time:     Wed, 08 Apr 2020 05:47:51 +0000
	Labels:         of7azurefinal=nfsof7azure
			pod-template-hash=848d8d6cc7
	Annotations:    <none>
	Status:         Running
	IP:             10.240.0.47
	IPs:            <none>
	Controlled By:  ReplicaSet/nfsof7azure-848d8d6cc7
	Containers:
	  of7azure:
	    Container ID:  docker://182c53714c953f53ffb79d721a230a4d02b8ef5611d383d788436aebf0e697ea
	    Image:         kelsey92/of7azurefinal:of7azure
	    Image ID:      docker-pullable://kelsey92/of7azurefinal@sha256:e72e08b8f6b8533b18453f0ef053bb5b0886dc4d3d8f3d63b5ef8ff7734f8f6d
	    Port:          6066/TCP
	    Host Port:     0/TCP
	    Command:
	      /bin/sh
	      -ec
	      while :; do echo '.'; sleep 5 ; done
	    State:          Running
	      Started:      Wed, 08 Apr 2020 05:47:54 +0000
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
	    ClaimName:  nfs-pvc
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
	  Type    Reason     Age    From                               Message
	  ----    ------     ----   ----                               -------
	  Normal  Scheduled  9m56s  default-scheduler                  Successfully assigned default/nfsof7azure-848d8d6cc7-vvbnq to aks-agentpool-24893396-1
	  Normal  Pulled     9m54s  kubelet, aks-agentpool-24893396-1  Container image "kelsey92/of7azurefinal:of7azure" already present on machine
	  Normal  Created    9m54s  kubelet, aks-agentpool-24893396-1  Created container of7azure
	  Normal  Started    9m53s  kubelet, aks-agentpool-24893396-1  Started container of7azure
	```
<details>
		<summary>PVC is attached to the Pod</summary>

```kubectl describe pvc nfs-pvc```

```bash
Name:          nfs-pvc
Namespace:     default
StorageClass:  nfs-storageclass
Status:        Bound
Volume:        pvc-6633fc99-4e54-45e6-8e3b-e322a52817ad
Labels:        <none>
Annotations:   pv.kubernetes.io/bind-completed: yes
               pv.kubernetes.io/bound-by-controller: yes
               volume.beta.kubernetes.io/storage-provisioner: nfs-of7azure
Finalizers:    [kubernetes.io/pvc-protection]
Capacity:      200Gi
Access Modes:  RWX
VolumeMode:    Filesystem
Mounted By:    nfsof7azure-848d8d6cc7-vvbnq
Events:        <none>
```
</details>

<details>
	<summary>Check the Persistent Volume from the container.</summary>

*This Volume will use the NFS share which does not vanish when the container is dead.*

```kubectl exec -it [pod name] -- /bin/bash```

```cd /mnt/azure/```
```bash
[of7azure@of7azure mnt]$ ls -rtl
total 4
drwxrwxrwx 11 root root 4096 Apr  1 12:49 azure
```
    
</details>

*Clean it*

```kubectl delete deployment nfsof7azure```

```kubectl delete pod nfsof7azure-848d8d6cc7-vvbnq```

# 2. Fail-over Test

## 2-1. Test Senario & Result

**Both Pods work - NFS Server & Azure Disk**

__a.__ Move the actual directories in [Persistent Volume-4](#1-31-use-persistent-volume-with-azure-kubernetes-service).

- Run the container first, then move the following directories to the Persistent Volume path.

```kubectl exec -it [pod name] -- /bin/bash```

*In this case, the path is set to /mnt/azure*

```cd /mnt/azure```

```bash    
[of7azure@of7azure azure]$ ls -rtl
total 52
drwxrwxr-x  4 of7azure of7azure  4096 Mar 21 08:46 shared
drwxrwxr-x  2 of7azure of7azure  4096 Mar 21 08:48 spunpack
drwxrwxr-x  2 of7azure of7azure  4096 Mar 21 08:48 spbackup
drwxrwxr-x  2 of7azure of7azure  4096 Mar 21 08:48 outputq
drwxrwxr-x  8 of7azure of7azure  4096 Mar 21 08:48 volume_default
drwxrwxr-x 13 of7azure of7azure  4096 Mar 24 15:05 osc
drwxrwxr-x  8 of7azure of7azure  4096 Mar 24 15:07 spool
drwxrwxr-x 11 of7azure of7azure  4096 Mar 25 11:20 tibero6
drwxrwxr-x  7 of7azure of7azure  4096 Mar 27 08:01 temp
drwx------  2 root     root     16384 Apr  1 12:29 lost+found
```

__b.__ Make an OpenFrame image which uses the Persistent Volume.

**Make links to the Persistent Volume path.**

*Image - kelsey92/of7azurefinal:of7azure*

- Tibero 
    
    ```bash
    [of7azure@of7azure ~]$ ls -rtl
    total 76
    -rwxrwxrwx  1     1001     1001   257 Mar 20 08:48 odbcinst.ini
    -rwxrwxrwx  1     1001     1001   127 Mar 20 08:49 odbc.ini
    drwxrwxr-x 20 of7azure of7azure  4096 Mar 21 07:14 unixODBC-2.3.2
    drwxrwxr-x  7 of7azure of7azure  4096 Mar 21 07:20 unixODBC
    drwxrwxr-x 11 of7azure of7azure  4096 Mar 21 07:23 OFCOBOL
    -rw-rw-r--  1 of7azure of7azure   207 Mar 21 07:25 HELLO.cob
    -rwxrwxr-x  1 of7azure of7azure 18072 Mar 21 07:25 HELLO
    drwxrwxr-x  9 of7azure of7azure  4096 Mar 21 07:25 prosort
    drwxrwxrwx  5     1001     1001  4096 Mar 21 12:37 LICENSE
    drwxrwxr-x 14 of7azure of7azure  4096 Mar 21 12:37 jeus7
    drwxrwxr-x  2 of7azure of7azure  4096 Mar 27 07:55 binary
    lrwxrwxrwx  1 of7azure of7azure    18 Apr  1 10:17 tibero6 -> /mnt/azure/tibero6
    drwxrwxr-x  1 of7azure of7azure  4096 Apr  2 09:07 MOUNT
    drwxrwxr-x  1 of7azure of7azure  4096 Apr  2 09:08 OpenFrame
    ```

- OpenFrame

    ```bash
    [of7azure@of7azure OpenFrame]$ ls -rtl
    total 84
    drwxrwxr-x  4 of7azure of7azure  4096 Mar 21 08:46 webde
    drwxrwxr-x  5 of7azure of7azure  4096 Mar 21 08:46 tsam
    drwxrwxr-x  2 of7azure of7azure  4096 Mar 21 08:46 schema
    drwxrwxr-x  2 of7azure of7azure  4096 Mar 21 08:46 profile
    drwxrwxr-x  2 of7azure of7azure  4096 Mar 21 08:46 include
    drwxrwxr-x  2 of7azure of7azure  4096 Mar 21 08:46 cpm
    drwxrwxr-x  2 of7azure of7azure  4096 Mar 21 08:46 license
    drwxrwxr-x 23 of7azure of7azure  4096 Mar 21 08:46 core
    drwxrwxr-x  2 of7azure of7azure  4096 Mar 21 08:48 util
    drwxrwxr-x  2 of7azure of7azure  4096 Mar 21 08:48 sample
    drwxrwxr-x  7 of7azure of7azure  4096 Mar 24 15:05 UninstallerData
    drwxrwxr-x  2 of7azure of7azure  4096 Mar 24 15:05 data
    drwxrwxr-x  2 of7azure of7azure  4096 Mar 24 15:05 bin
    drwxrwxr-x  8 of7azure of7azure  4096 Mar 24 15:06 log
    drwxrwxr-x  2 of7azure of7azure  4096 Mar 24 15:06 scripts
    drwxrwxr-x  2 of7azure of7azure 20480 Mar 24 15:06 lib
    drwxrwxr-x  2 of7azure of7azure  4096 Mar 26 01:08 config
    lrwxrwxrwx  1 of7azure of7azure    16 Apr  1 10:18 spool -> /mnt/azure/spool
    lrwxrwxrwx  1 of7azure of7azure    25 Apr  1 10:18 volume_default -> /mnt/azure/volume_default
    lrwxrwxrwx  1 of7azure of7azure    17 Apr  1 10:18 shared -> /mnt/azure/shared
    lrwxrwxrwx  1 of7azure of7azure    18 Apr  1 10:18 outputq -> /mnt/azure/outputq
    lrwxrwxrwx  1 of7azure of7azure    15 Apr  2 09:07 temp -> /mnt/azure/temp
    lrwxrwxrwx  1 of7azure of7azure    14 Apr  2 09:08 osc -> /mnt/azure/osc
    lrwxrwxrwx  1 of7azure of7azure    19 Apr  2 09:08 spunpack -> /mnt/azure/spunpack
    lrwxrwxrwx  1 of7azure of7azure    19 Apr  2 09:08 spbackup -> /mnt/azure/spbackup
    ```
    
__c.__ Check the current status of the Pod.

**Both Pods work - NFS Server & Azure Disk**

*The following case is the Pod using Azure Disk*

```kubectl get pods```
```bash
NAME                        READY   STATUS    RESTARTS   AGE
of7azure-76db5dbccb-brgrs   1/1     Running   0          22m
```

```kubectl describe pod of7azure-76db5dbccb-brgrs```
```bash
Name:           of7azure-76db5dbccb-brgrs
Namespace:      default
Priority:       0
Node:           aks-agentpool-24893396-1/10.240.0.35
Start Time:     Thu, 02 Apr 2020 12:37:03 +0000
Labels:         of7azurefinal=of7azure
                pod-template-hash=76db5dbccb
Annotations:    <none>
Status:         Running
IP:             10.240.0.40
IPs:            <none>
Controlled By:  ReplicaSet/of7azure-76db5dbccb
Containers:
  of7azure:
    Container ID:  docker://cd6add2e2d48d92c2ab04ffb0b35491b475b7a049161175789302ee2c7b9a3f5
    Image:         kelsey92/of7azurefinal:of7azure
    Image ID:      docker-pullable://kelsey92/of7azurefinal@sha256:8e707e0444eec3af2842a34de8360781f0dd9ed85ad620b0856a8c7368029603
    Port:          6066/TCP
    Host Port:     0/TCP
    Command:
      /bin/sh
      -ec
      while :; do echo '.'; sleep 5 ; done
    State:          Running
      Started:      Thu, 02 Apr 2020 12:42:40 +0000
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
  Type    Reason                  Age   From                               Message
  ----    ------                  ----  ----                               -------
  Normal  Scheduled               23m   default-scheduler                  Successfully assigned default/of7azure-76db5dbccb-brgrs to aks-agentpool-24893396-1
  Normal  SuccessfulAttachVolume  23m   attachdetach-controller            AttachVolume.Attach succeeded for volume "pvc-a0a48609-8975-433f-9b73-bc371cbb0702"
  Normal  Pulling                 22m   kubelet, aks-agentpool-24893396-1  Pulling image "kelsey92/of7azurefinal:of7azure"
  Normal  Pulled                  18m   kubelet, aks-agentpool-24893396-1  Successfully pulled image "kelsey92/of7azurefinal:of7azure"
  Normal  Created                 17m   kubelet, aks-agentpool-24893396-1  Created container of7azure
  Normal  Started                 17m   kubelet, aks-agentpool-24893396-1  Started container of7azure
```

__d.__ Use OpenFrame function.

**Both Pods work - NFS Server & Azure Disk**

*The following case is the Pod using Azure Disk*

```kubectl exec -it of7azure-76db5dbccb-brgrs -- /bin/bash```

*Fisrt, Tibero and OpenFrame are successfully booted.*

*Second, run a JOB.*

```bash 
[of7azure@of7azure OpenFrame]$ ls -rtl test.jcl
-rw-rw-r-- 1 of7azure of7azure   133 Apr  2 13:02 test.jcl

[of7azure@of7azure OpenFrame]$ tjesmgr r $PWD/test.jcl
Input USERNAME  : ROOT
>
Command : [r /home/of7azure/OpenFrame/test.jcl]
Node name :  A N Y
(JOB00002) /home/of7azure/OpenFrame/test.jcl is submitted as TEST(JOB00002).

[of7azure@of7azure OpenFrame]$ tjesmgr ps
Input USERNAME  : ROOT
>
Command : [ps]
 JOBNAME  JOBID    CLASS   STATUS   RC     NODE     START-TIME        END-TIME          JCL
------------------------------------------------------------------------------------------------------------------
 IDCAMS01 JOB00001   A     Done     R00008 NODE1    20200402/12:02:37 20200402/12:02:37  IDCAMS01
 TEST     JOB00002   A     Error    R00127 NODE1    20200402/13:03:48 20200402/13:03:49  test.jcl
```

*Third, create a dataset.*

```bash
dscreate KELSEY.TMAX.TEST
dscreate version 7.0.3(7) obuild@tplinux64:ofsrc7/base(#1) 2019-12-10 15:05:02
Create a New Dataset or a Member of PDS Dataset

DSCREATE DSNAME=KELSEY.TMAX.TEST,CATALOG=,VOLSER=,MEMBER=
Input USERNAME  : ROOT
Input PASSWORD  :
OFRUISVRDSCRE: Dataset Create OK. dsn=KELSEY.TMAX.TEST
COMPLETED SUCCESSFULLY.
```

*Lastly, boot up JEUS, Webterminal and OFmanager.*

__e.__ Kill the NODE and see if a new Pod is created another NODE and running successfully.

*I stopped aks-agentpool-24893396-1 NODE.*

**Both Pods work - NFS Server & Azure Disk**

*The following case is the Pod using Azure Disk*

```kubectl get nodes```
```bash
NAME                       STATUS     ROLES   AGE     VERSION
aks-agentpool-24893396-0   Ready      agent   6d10h   v1.15.10
aks-agentpool-24893396-1   NotReady   agent   6d3h    v1.15.10
```

```kubectl get pods```
```bash
NAME                        READY   STATUS              RESTARTS   AGE
of7azure-76db5dbccb-6fbtc   0/1     ContainerCreating   0          6s
of7azure-76db5dbccb-brgrs   1/1     Terminating         0          35m

NAME                        READY   STATUS    RESTARTS   AGE
of7azure-76db5dbccb-6fbtc   1/1     Running   0          13m
```

__f.__ Check the current status of the new Pod.

*Now the new Pod is running in aks-agentpool-24893396-0 NODE.*

**When the Pod is created in the NODE for the first time, it pulls the image from Docker. If it isn't the first time, Kubenetes will create the container with the previously pulled image in the NODE. Be careful not to use the same name & tag when you update the image. If you want to use the same name & tag when you update the image, you need to delete the old one in the NODE.**  

```kubectl describe pod of7azure-76db5dbccb-6fbtc```
```bash
Name:           of7azure-76db5dbccb-6fbtc
Namespace:      default
Priority:       0
Node:           aks-agentpool-24893396-0/10.240.0.4
Start Time:     Thu, 02 Apr 2020 13:12:20 +0000
Labels:         of7azurefinal=of7azure
                pod-template-hash=76db5dbccb
Annotations:    <none>
Status:         Running
IP:             10.240.0.27
IPs:            <none>
Controlled By:  ReplicaSet/of7azure-76db5dbccb
Containers:
  of7azure:
    Container ID:  docker://daa1832aff46f4fc6a44360000387b3a215506769683cfae3aaa00016b0373c6
    Image:         kelsey92/of7azurefinal:of7azure
    Image ID:      docker-pullable://kelsey92/of7azurefinal@sha256:8e707e0444eec3af2842a34de8360781f0dd9ed85ad620b0856a8c7368029603
    Port:          6066/TCP
    Host Port:     0/TCP
    Command:
      /bin/sh
      -ec
      while :; do echo '.'; sleep 5 ; done
    State:          Running
      Started:      Thu, 02 Apr 2020 13:21:48 +0000
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
  Normal   Scheduled               12m    default-scheduler                  Successfully assigned default/of7azure-76db5dbccb-6fbtc to aks-agentpool-24893396-0
  Normal   SuccessfulAttachVolume  9m53s  attachdetach-controller            AttachVolume.Attach succeeded for volume "pvc-a0a48609-8975-433f-9b73-bc371cbb0702"
  Normal   Pulling                 7m58s  kubelet, aks-agentpool-24893396-0  Pulling image "kelsey92/of7azurefinal:of7azure"
  Normal   Pulled                  3m17s  kubelet, aks-agentpool-24893396-0  Successfully pulled image "kelsey92/of7azurefinal:of7azure"
  Normal   Created                 2m45s  kubelet, aks-agentpool-24893396-0  Created container of7azure
  Normal   Started                 2m45s  kubelet, aks-agentpool-24893396-0  Started container of7azure
```

__g.__ Boot up Tibero and OpenFrame.

```kubectl exec -it of7azure-76db5dbccb-6fbtc -- /bin/bash```

**Since the NODE was killed while everything was runnning, booting up process is somewhat different from the normal one.**

- Tibero

    ```
    tbdown clean
    tbboot
    ```

- OpenFrame

    **osctdlinit** is neccessary.
    
    ```
    [of7azure@of7azure ~]$ osctdlinit OSCOIVP1
    TDLINIT.691.064701:(I) TDLUTIL0046 TDLDIR initialization complete [TDL0331]
    ```

    **OSC regions should be booted with -a option (Do not create OSC DB Tables).**

    ```
    [of7azure@of7azure ~]$ oscboot -r OSCOIVP1
    OSCBOOT : OSC RTSD loading(OSCOIVP1)                                  [fail]
    OSCBOOT : cics_ctrl_boot(-52906) error: Check oscmgr's log file
    OSCBOOT : OSC Region(OSCOIVP1)                                        [fail]
    ```
    
    ```
    [of7azure@of7azure ~]$ oscboot -r OSCOIVP1 -a
    OSCBOOT : OSC RTSD loading(OSCOIVP1)                                  [ OK ]
    OSCBOOT : OSC region server(OSCOIVP1TL)                               [ OK ]
    OSCBOOT : OSC region server(OSCOIVP1OMC)                              [ OK ]
    OSCBOOT : OSC region server(OSCOIVP1C)                                [ OK ]
    OSCBOOT : OSC region server(OSCOIVP1)                                 [ OK ]
    OSCBOOT : OSC tranclass server(OSCOIVP1_TCL1)                         [ OK ]
    OSCBOOT : OSC PLTPI loading(OSCOIVP1)                                 [ OK ]
    OSCBOOT : OSC Region(OSCOIVP1)                                        [ OK ]
    ```
    
    *Error messages*

	<details>
		<summary>oscmgr04022020.out</summary>

	```
	132805 I OSC0013I [10:OSCMGR:0:0] oscmgr version 7.0.3(15)
	132805 I OSC0011I [10:OSCMGR:0:0] oscmgr server boots
	132823 E OSC0009E [10:CICSDB:0:0] SQLExecDirect: 72000 error(-7102)
	132823 E OSC0010E [10:CICSDB:0:0] SQLExecDirect: 72000 error -  Duplicate schema object 'TIBERO.OSCOIVP1_CONNECTION' exists.
	132823 E OSC0009E [10:CICSCTRL:0:0] cics_rtsd_create: cics_rtsd_create error(-52709)
	132823 E OSC3501E [10:CICSCTRL:0:0] Creating sys memory error(-52906)
	```
	</details>

	<details>
		<summary>oscmgr04022020.err</summary>

	```
	cics_db_create_table() failed: rc(-52709), table type(0)sqlrc=-1, SQL : [INSERT INTO VTAM_ACTIVE_LU VALUES (  ?, ?, ?, ?, ?, ?, ?, ? ) ]
	SQLExecute failed
	State: 23000
	Native Error: -10007
	Error Message:  UNIQUE constraint violation ('TIBERO'.'TIBERO_CON86000512').
	132835 E VTM0125E Error processing function: vtam_appl_db_register_region(), ERROR CODE=-109957
	```
	</details>

    *You can use **-m option(Remove OSC resources) when you shut down the region.** It will delete the Tibero region table.*

__h.__ Check if the new Pod has the data before the NODE dies.

*A new Pod lost the files I created under container directories, but not under Persistent Volume.*

```kubectl exec -it of7azure-76db5dbccb-6fbtc -- /bin/bash```

```bash
[of7azure@of7azure OpenFrame]$ ls -rtl  test.jcl
ls: cannot access 'test.jcl': No such file or directory

[of7azure@of7azure OpenFrame]$  tjesmgr r $PWD/test.jcl
Input USERNAME  : ROOT
>
Command : [r /home/of7azure/OpenFrame/test.jcl]
Node name :  A N Y
OBMJMSVRJSUBMIT tpcall failed - service failure(TPESVCFAIL)
load_inpjcl() failed. rc=-9800
Tmax return code : -9800

[of7azure@of7azure OpenFrame]$ tjesmgr ps
Input USERNAME  : ROOT
>
Command : [ps]
 JOBNAME  JOBID    CLASS   STATUS   RC     NODE     START-TIME        END-TIME          JCL
------------------------------------------------------------------------------------------------------------------
 IDCAMS01 JOB00001   A     Done     R00008 NODE1    20200402/12:02:37 20200402/12:02:37  IDCAMS01
 TEST     JOB00002   A     Error    R00127 NODE1    20200402/13:03:48 20200402/13:03:49  test.jcl

[of7azure@of7azure ~]$ dslist KELSEY.TMAX.TEST
dslist version 7.0.3(7) obuild@tplinux64:ofsrc7/base(#1) 2019-12-10 15:05:02
Print Dataset List and Information

-----------------------------------------------------------------------------
  SYS1.VTOC.VDEFVOL                             VOLUME  DSORG  RECFM  LRECL
-----------------------------------------------------------------------------
  KELSEY.TMAX.TEST                              DEFVOL  PS     FB     80
-----------------------------------------------------------------------------
* Total 1 entries in volume DEFVOL printed.
```

__i.__ Connect to JEUS, Webterminal, OFmanager with the ports of current NODE.

1) Add all Nodeports services

	Check how to from - [Nodeport Services](https://github.com/kelsey-ek/of7azure/blob/master/GUIDE/Azuretest.md#23-set-services)

	```kubectl get services```

	```bash
	NAME             TYPE        CLUSTER-IP     EXTERNAL-IP   PORT(S)          AGE
	kubernetes       ClusterIP   10.0.0.1       <none>        443/TCP          12d
	jeus             NodePort    10.0.112.142   <none>        9736:31088/TCP   12d
	ofmanager        NodePort    10.0.236.26    <none>        8087:31874/TCP   12d
	webterminal      NodePort    10.0.68.39     <none>        8088:30856/TCP   12d
	nfsjeus          NodePort    10.0.241.220   <none>        9736:31310/TCP   4h13m
	nfsofmanager     NodePort    10.0.156.126   <none>        8087:32667/TCP   4h14m
	nfswebterminal   NodePort    10.0.230.65    <none>        8088:30320/TCP   4h14m
	```

2) Add all Inbound NAT rules and register all inbound ports

	Check how to from - [Inbound NAT rules](https://github.com/kelsey-ek/of7azure/blob/master/GUIDE/Azuretest.md#24-network-configuration) & [Network configuration](https://github.com/kelsey-ek/of7azure/blob/master/GUIDE/Azuretest.md#24-network-configuration)

	- Inbound NAT rules
	
	<img src="./reference_images/inbound_all.PNG" title="inbound_all">
	
	- VM Networking - inbound port rule : **Add all nodeports as inbound ports**
	
	<img src="./reference_images/Nodeport.PNG" title="Nodeport">

3) Check if the Pod is running in aks-agentpool-24893396-0(NODE A) or aks-agentpool-24893396-1(NODEB) and use the url below.

	**Find aks-agentpool-24893396-0(NODE A) or aks-agentpool-24893396-1(NODE B) from 'Target virtual machine' in the configuration.**

	- The Pod using Azure Disk
	
	   - NODE A 

		   - Jeus : http://52.141.172.195:9736/webadmin/

		   <img src="./reference_images/NAT01_jeus.PNG" title="NAT01_jeus">

		   - Webterminal : http://52.141.172.195:8088/webterminal/

		   <img src="./reference_images/NAT01_webterminal.PNG" title="NAT01_webterminal">

		   - OFmanager : http://52.141.172.195:8087/ofmanager/

	           <img src="./reference_images/NAT01_ofmanager.PNG" title="NAT01_ofmanager">

	    - NODE B

		    - Jeus : http://52.141.172.195:19736/webadmin/

		    <img src="./reference_images/NAT02_jeus.PNG" title="NAT02_jeus">

		    - Webterminal : http://52.141.172.195:18088/webterminal/

		    <img src="./reference_images/NAT02_webterminal.PNG" title="NAT02_webterminal">

		    - OFmanager : http://52.141.172.195:18087/ofmanager/

		    <img src="./reference_images/NAT02_ofmanager.PNG" title="NAT02_ofmanager">
		
		
	- The Pod using NFS Server (setting process is the same as above)
	
	   - NODE A 

		   - Jeus : http://52.141.172.195:29736/webadmin/

		   - Webterminal : http://52.141.172.195:28088/webterminal/

		   - OFmanager : http://52.141.172.195:28087/ofmanager/

	    - NODE B

		    - Jeus : http://52.141.172.195:39736/webadmin/

		    - Webterminal : http://52.141.172.195:38088/webterminal/

		    - OFmanager : http://52.141.172.195:38087/ofmanager/		
		
 
# Copyrighted by Kelsey

*References*

https://kubernetes.io/docs/concepts/storage/persistent-volumes

https://docs.microsoft.com/en-us/azure/aks/azure-disks-dynamic-pv

https://kubernetes.io/docs/concepts/storage/dynamic-provisioning/

https://gruuuuu.github.io/cloud/k8s-volume/#


