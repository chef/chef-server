# Veil

Veil is a library for securely creating, storing, and rotating Chef
Server secrets. Prior to Veil, Chef Server secrets we're generated
on the bootstrap backend node and we're long lived for the duration
of the Chef Server install. The secrets generated were completely
random so there was no way to add a new service or credential without
copying the secrets file between each Chef Server in the cluster.
Veil solves this problem by creating a master secret key and salt
which it uses, along with the service name, credential name, and
credential version to create service crendentials. By knowing the
secret key and salt, any Chef Server in the fleet with the given
set of secrets can create the same service credentials without
requiring the the secrets file to be copied.

## Credential Generation

Credentials are generated using:

    sha_512(
      secure_hash(
        sha_512(shared_secret + service_name + credential_name + credential_version),
        shared_salt
      )
    )

where

- `sha_512` is an SHA512 of the given data.

- `secure_hash` is a secure hashing algorithm such as BCrypt
  or PBKDF2 with their relevant work-factor tunables set very high.

- `shared_secret` is a random 1024 byte secret that is generated at installation
  time.

- `service_name` is the name of the service for which we are generating a
  credential.

- `credential_name` is the name of the credential.

- `credential_version` is an integer starting at 0 that is incremented
  each time the credential is rotated.

- `shared_salt` is a shared secret salt required for the secure hashing algorithm

## Installation Scenarios

### New Installation

For new installations, all credentials will be generated in this
fashion, with the bootstrap node being responsible for creating the
shared_secret and salt.  Generated credentials will be stored in
private-chef-secrets.json in the same legacy format:

    {
      "SERVICE_NAME": {
        "PASS_KEY_NAME": "GENERATED_PASSWORD",
      }
    }

There is also an additional key in the private-chef-secrets.json called 'veil'
that contains the shared secret, shared salt, and metadata for each of
the credentials:

    {
      "veil": {
        "type": "Veil::CredentialCollection::ChefSecretsFile",
        "hasher": {
          "type": "Veil::Hasher::PBKDF2",
          "secret": "392a555c7ad563a79f6ecdd3afab37b1072cebfcbf6bcef8b8dfc579fc4f65ab4f14f541dce541f2674f5e1678ed39f23e6cffe77c2b492f677c9f2b0f767709639304868e76919c9c8df27296a30038e0d
    7e0d5d283d8b862e11cdf621076ebbc5a84a67d8b61ffdfac4edce177356dcb05e05f8ba0c09fc6022d793f35d374041ac8a9a1b845d80fba2fa06f00e1d332d360371e58d52fe3d0946d0a00e8b6ff9a475f6970d8359ad2cdf
    db027e14fa1a8d55b2c507df57257cc16a0539f6782c754cc8bc750c7f49aee77aa28ee67ee78ee11b169ec72ab16f5daa4c6ebf638d5c6e267fdb1a2cecacd4dcf42924ce5c9af3e648951e28c735e1334ec8c9821c9708cc19
    2164f207a4cdb4dba2707d47831dc46fee8c51fbc8fe6c0b15bfd7c669e0a2d59fc109658585a882394dda29c7442bc629b5cd181005aa08bc742fb97f02205ca2ff6cfa91671fc7c1e602e153921e0854f63d5a3e5de7dfd1f1
    2ab9f6b34881915fddf8a672eedac6725479ed01d611aa6596e687455c75d3601c67a62164da1a2de16f5e6612d78a909af399cd96aa27bad574cc73edd92377be0020a7d5ef8e969a68c93a0c8fbfb8ed30402a93595ae663ed
    efb3dfd55a059c1547ea0fbc943d1d55d6d8d87769f5cea722c0265ad033e77af08197d33fcb90cb78d7ec4ae56115e1598fbb3c441ee38019b13b6df396408e6ebcb10d541da",
          "salt": "dfaefd9ffff3948469358cffb29f18d78215b31656c06becbe50c17e2a3c741fceffea28c31178adc7cd0764f9e01490a7e460adcd755e514f86b1ad4fed5e33bcee7411c821f390b4936cecd83b771d48e9e
    d4ceba21285667d5ce32d91d8bbd70f9e09f96877f9ec894572cdb08959639edf12ca183ba0c03e336745230fc2",
          "iterations": 10000,
          "hash_function": "OpenSSL::Digest::SHA512"
        },
        "credentials": {
          "redis_lb": {
            "password": {
              "type": "Veil::Credential",
              "name": "password",
              "value": "64556621e95910b63a290a2972ef60f1877ac83704a9644631",
              "version": 3,
              "length": 50
            }
          },
          "rabbitmq": {
            "password": {
              "type": "Veil::Credential",
              "name": "password",
              "value": "64556621e95910b63a290a2972ef60f1877ac83704a9644631",
              "version": 3,
              "length": 50
            },
            "actions_password": {
              "type": "Veil::Credential",
              "name": "actions_password",
              "value": "2935210e844f35fdbc6749f70231466c67fcee6c4d8293425b",
              "version": 3,
              "length": 50
            },
            "management_password": {
              "type": "Veil::Credential",
              "name": "management_password",
              "value": "5dcd1c77c1fbad79d92f474964e909aa35abc18e52772b559a",
              "version": 3,
              "length": 50
            }
          }
        }
      }
    }

### Existing Installations

For existing installation, a shared_credential will be generated on
the bootstrap node and the user will need to distribute it to the rest
of the nodes on the first upgrade to a version with this feature.

Existing secrets will not be regenerated unless the user explicityly initiates
the regeneration with one of the key rotation commands.

## Credential Rotation

Secrets rotation is still a downtime operation. However, it would only
happen at the explicit request of the user.

## Rotating All Service Credentials

Rotating all over the service credentials will iterate over all credentials,
bump their credential_version, and rehash their password with the new version.

  ```shell
  $: chef-server-ctl rotate-all-credentials
  ...
  All credentials have been rotated!
  Run 'chef-server-ctl rotate-all-credentials' on each Chef Server
  ```

## Individual Credential Rotation

Rotate the service credentials for a single Chef Server subsystem

  ```shell
  $: chef-server-ctl rotate-credentials $SERVICE_NAME
  ...
  All credentials have been rotated!
  Run 'chef-server-ctl rotate-credentials SERVICE_NAME' on each Chef Server
  ```

## Shared Secret Rotation

Regenerating the shared secrets regenerates the shared services, all credentials
and resets their credential_version to 0. In this scenario the user needs to to
redistribute /etc/opscode/private-chef-secrets.json:

  ```shell
  $: chef-server-ctl rotate-shared-secrets
  ...
  The shared secrets and all service credentials have been rotated!
  Please copy /etc/opscode/private-chef-secrets.json to each Chef Server and
  run 'chef-server-ctl reconfigure'
  ```
