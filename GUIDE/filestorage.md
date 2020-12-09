

https://docs.microsoft.com/en-us/azure/storage/files/storage-how-to-create-premium-fileshare?tabs=azure-portal


[VM]
sudo yum update

- Install Azure CLI

```
sudo rpm --import https://packages.microsoft.com/keys/microsoft.asc
```

```
bash_profile

echo -e "[azure-cli]
name=Azure CLI
baseurl=https://packages.microsoft.com/yumrepos/azure-cli
enabled=1
gpgcheck=1
gpgkey=https://packages.microsoft.com/keys/microsoft.asc" | sudo tee /etc/yum.repos.d/azure-cli.repo
```

```
sudo yum install azure-cli
```

```
az login
To sign in, use a web browser to open the page https://microsoft.com/devicelogin and enter the code S7QNPWTRB to authenticate.
The following tenants don't contain accessible subscriptions. Use 'az login --allow-no-subscriptions' to have tenant level access.
693358f3-dc90-4442-aeec-274d3289a40e
[
  {
    "cloudName": "AzureCloud",
    "homeTenantId": "ef439009-53c1-4a0c-8012-edabcdeb7e05",
    "id": "9c327935-ea7c-4dfe-a425-f45aee2a1959",
    "isDefault": true,
    "managedByTenants": [],
    "name": "Pay-As-You-Go",
    "state": "Enabled",
    "tenantId": "ef439009-53c1-4a0c-8012-edabcdeb7e05",
    "user": {
      "name": "kelsey.lee@tmaxsoft.com",
      "type": "user"
    }
  }
]
```

```
az logout
```


sudo yum install cifs-utils 

[Azure Command Line Interface (CLI)]

```
resourceGroupName="<your-resource-group>"
storageAccountName="<your-storage-account>"

# This command assumes you have logged in with az login
httpEndpoint=$(az storage account show \
    --resource-group $resourceGroupName \
    --name $storageAccountName \
    --query "primaryEndpoints.file" | tr -d '"')
smbPath=$(echo $httpEndpoint | cut -c7-$(expr length $httpEndpoint))
fileHost=$(echo $smbPath | tr -d "/")

nc -zvw3 $fileHost 445
```

Connection to <your-storage-account> 445 port [tcp/microsoft-ds] succeeded!

```
resourceGroupName="<your-resource-group>"
storageAccountName="<your-storage-account>"
fileShareName="<your-file-share>"

mntPath="/mnt/$storageAccountName/$fileShareName"

sudo mkdir -p $mntPath
```


```
# This command assumes you have logged in with az login
httpEndpoint=$(az storage account show \
    --resource-group $resourceGroupName \
    --name $storageAccountName \
    --query "primaryEndpoints.file" | tr -d '"')
smbPath=$(echo $httpEndpoint | cut -c7-$(expr length $httpEndpoint))$fileShareName

storageAccountKey=$(az storage account keys list \
    --resource-group $resourceGroupName \
    --account-name $storageAccountName \
    --query "[0].value" | tr -d '"')

sudo mount -t cifs $smbPath $mntPath -o vers=3.0,username=$storageAccountName,password=$storageAccountKey,serverino
```


















```
sudo modinfo -p cifs | grep disable_legacy_dialects
```

disable_legacy_dialects: To improve security it may be helpful to restrict the ability to override the default dialects (SMB2.1, SMB3 and SMB3.02) on mount with old dialects (CIFS/SMB1 and SMB2) since vers=1.0 (CIFS/SMB1) and vers=2.0 are weaker and less secure. Default: n/N/0 (bool)








```
sudo modprobe cifs
cat /sys/module/cifs/parameters/disable_legacy_dialects
```






