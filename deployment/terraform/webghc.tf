# Security Group
resource "aws_security_group" "webghc" {
  vpc_id = aws_vpc.zerepoch.id
  name   = "${local.project}_${var.env}_webghc"

  ingress {
    from_port   = 22
    to_port     = 22
    protocol    = "TCP"
    cidr_blocks = concat(var.public_subnet_cidrs, var.private_subnet_cidrs)
  }

  ## inbound (world): http

  ingress {
    from_port   = 80
    to_port     = 80
    protocol    = "TCP"
    cidr_blocks = concat(var.public_subnet_cidrs, var.private_subnet_cidrs)
  }

  ## outgoing: all
  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  tags = {
    Name        = "${local.project}_${var.env}_webghc"
    Project     = local.project
    Environment = var.env
  }
}

data "template_file" "webghc_user_data" {
  template = file("${path.module}/templates/default_configuration.nix")

  vars = {
    root_ssh_keys = join(" ", formatlist("\"%s\"", local.root_ssh_keys))
  }
}

resource "aws_instance" "webghc_a" {
  ami = module.nixos_image.ami

  instance_type = var.webghc_instance_type
  subnet_id     = aws_subnet.private.*.id[0]
  user_data     = data.template_file.webghc_user_data.rendered

  vpc_security_group_ids = [
    aws_security_group.webghc.id,
  ]

  root_block_device {
    volume_size = "20"
  }

  tags = {
    Name        = "${local.project}_${var.env}_webghc_a"
    Project     = local.project
    Environment = var.env
  }
}

resource "aws_route53_record" "webghc_internal_a" {
  zone_id = aws_route53_zone.zerepoch_private_zone.zone_id
  type    = "A"
  name    = "webghc-a.${aws_route53_zone.zerepoch_private_zone.name}"
  ttl     = 300
  records = [aws_instance.webghc_a.private_ip]
}
