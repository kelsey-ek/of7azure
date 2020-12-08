

https://docs.microsoft.com/en-us/azure/storage/files/storage-how-to-create-premium-fileshare?tabs=azure-portal


[VM]
sudo yum update

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



