# This file contains values that need to be changed at runtime. E.g. `env` and `aws_region` need to be set before running terraform apply
variable "aws_region" {}

variable "env" {}

variable "output_path" {
}

variable "zerepoch_tld" {
  default = "zerepoch.tbcodev.io"
}

variable "zerepoch_full_domain" {
  default = ""
}

variable "zerepoch_public_zone" {
  default = "ZBC2AQBA8QH4G"
}

variable "simeon_tld" {
  default = "simeon.tbcodev.io"
}

variable "simeon_full_domain" {
  default = ""
}

variable "simeon_public_zone" {
  default = "Z1VIYCTCY8RMLZ"
}

variable "simeon_dash_tld" {
  default = "simeon-dash.tbcodev.io"
}

variable "simeon_web_public_zone" {
  default = "Z09016162N4S3NFVWHXYP"
}

variable "simeon_web_tld" {
  default = "simeon-web.tbcodev.io"
}

variable "simeon_dash_public_zone" {
  default = "Z04600362E06M9P9U3Y12"
}

variable "simeon_finance_io_public_zone" {
  default = "Z005888925CITFPGLQVVQ"
}

variable "bastion_instance_type" {
  default = "t3.micro"
}

variable "webghc_instance_type" {
  default = "t3.large"
}

variable "playgrounds_instance_type" {
  default = "t3.small"
}

variable "simeon_dash_instance_type" {
  default = "t3.small"
}

variable "vpc_cidr" {
  default = "10.0.0.0/16"
}

variable "public_subnet_cidrs" {
  default = ["10.0.1.0/24", "10.0.2.0/24", "10.0.3.0/24"]
}

variable "private_subnet_cidrs" {
  default = ["10.0.4.0/24", "10.0.5.0/24", "10.0.6.0/24"]
}

# The public ip address of production.simeon.tbcodev.io which
# is used to create a route53 A record for simeon-finance.io.
variable "simeon_finance_production_ip" {
  default = "52.213.243.4"
}

variable "azs" {
  default = ["a", "b"]
}
