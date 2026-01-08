# Happening - Development Notes

## Build Process

The project uses SBCL (Steel Bank Common Lisp) and ASDF for building.

### Building

```bash
# Use system SBCL (not linuxbrew) for portable binaries
make clean && make
```

The Makefile uses `/usr/bin/sbcl` to ensure the binary works on target systems.

### Important: pure-tls/acme is in ocicl

The `pure-tls` library (including ACME support) lives in two places:
- **Source**: `/home/green/git/pure-tls/acme/` - where you edit
- **Build copy**: `/home/green/git/happening/ocicl/pure-tls-*/acme/` - what gets compiled

**After editing pure-tls sources, you MUST sync to ocicl:**
```bash
cp /home/green/git/pure-tls/acme/*.lisp /home/green/git/happening/ocicl/pure-tls-*/acme/
```

### Clearing ASDF Cache (if changes don't take effect)

```bash
rm -rf ~/.cache/common-lisp/sbcl-*/home/green/git/happening/ocicl/pure-tls-*/acme/*.fasl
```

## Deployment to EC2

### Copy binary to server
```bash
scp -o IdentitiesOnly=yes -i ~/happening.pem /home/green/git/happening/happening fedora@ec2-18-219-1-5.us-east-2.compute.amazonaws.com:~/happening
```

### Run setup (clean state)
```bash
ssh -o IdentitiesOnly=yes -i ~/happening.pem fedora@ec2-18-219-1-5.us-east-2.compute.amazonaws.com \
  'sudo rm -rf  ~/data /root/certs && sudo ~/happening setup -a admin -P testpass123 -u https://happening.labdroid.net -e green@moxielogic.com'
```

### Quick SSH access
```bash
ssh -o IdentitiesOnly=yes -i ~/happening.pem fedora@ec2-18-219-1-5.us-east-2.compute.amazonaws.com
```

## ACME/TLS Notes

### Let's Encrypt URLs
- **Staging** (default): `https://acme-staging-v02.api.letsencrypt.org/directory`
- **Production**: `https://acme-v02.api.letsencrypt.org/directory`

### RSA Key Generation
Ironclad generates random public exponents by default. X.509/Let's Encrypt requires e=65537.
Custom key generation is in `csr.lisp:generate-rsa-key-with-e65537`.

### Debug Files on Server
- Validation cert: `/root/certs/acme-validation-cert-DEBUG.pem`
- CSR: `/root/certs/debug-csr.der`

### Verify CSR
```bash
openssl req -in /root/certs/debug-csr.der -inform DER -noout -text
```

### Verify validation certificate
```bash
openssl x509 -in /root/certs/acme-validation-cert-DEBUG.pem -noout -text
openssl asn1parse -in /root/certs/acme-validation-cert-DEBUG.pem
```

## Key Files

- `src/main.lisp` - CLI entry point with setup subcommand
- `src/tls.lisp` - TLS server integration, `obtain-certificate-for-domain`
- `ocicl/pure-tls-*/acme/csr.lisp` - CSR generation, RSA key generation
- `ocicl/pure-tls-*/acme/client.lisp` - ACME protocol client
- `ocicl/pure-tls-*/acme/challenges.lisp` - TLS-ALPN-01 challenge handler
- `ocicl/pure-tls-*/acme/asn1.lisp` - ASN.1/DER encoding utilities

## Common Issues

### "invalid RSA public exponent" from Let's Encrypt
The RSA key has a non-standard exponent. Ensure `generate-rsa-key-with-e65537` is being used.

### Rate limiting (503 errors)
Wait a few minutes between attempts. Staging and production have separate rate limits.

### Binary won't run on target
Check dynamic linker: `file happening` and `ldd happening`
Build with system SBCL (`/usr/bin/sbcl`), not linuxbrew.
