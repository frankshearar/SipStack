Note to the security conscious:

Yes, that's a private key in newkey.pem. Don't worry. We only use the keys and certs (you noticed the CA cert & key, yes?) when we test the TLS transport.

If you DO use the private key (and why on Earth would you? If you want a self-signed CA then generate one yourself!) then, well, you deserve what you get.