# Public ALB

# Security Group
resource "aws_security_group" "public_alb" {
  vpc_id = aws_vpc.zerepoch.id

  ## inbound (world): ICMP 3:4 "Fragmentation Needed and Don't Fragment was Set"
  ingress {
    from_port   = "3"
    to_port     = "4"
    protocol    = "ICMP"
    cidr_blocks = ["0.0.0.0/0"]
  }

  ## inbound (world): https
  ingress {
    from_port   = "443"
    to_port     = "443"
    protocol    = "TCP"
    cidr_blocks = ["0.0.0.0/0"]
  }

  ## inbound (world): http
  ingress {
    from_port   = "80"
    to_port     = "80"
    protocol    = "TCP"
    cidr_blocks = ["0.0.0.0/0"]
  }

  egress {
    from_port   = 80
    to_port     = 80
    protocol    = "TCP"
    cidr_blocks = var.private_subnet_cidrs
  }

  egress {
    # both PAB and zerepoch playground use the same port
    from_port   = local.zerepoch_playground_port
    to_port     = local.zerepoch_playground_port
    protocol    = "TCP"
    cidr_blocks = var.private_subnet_cidrs
  }

  egress {
    from_port   = local.simeon_playground_port
    to_port     = local.simeon_playground_port
    protocol    = "TCP"
    cidr_blocks = var.private_subnet_cidrs
  }

  egress {
    from_port   = local.simeon_web_port
    to_port     = local.simeon_web_port
    protocol    = "TCP"
    cidr_blocks = var.private_subnet_cidrs
  }

  tags = {
    Name        = "${local.project}_${var.env}_public_alb"
    Project     = local.project
    Environment = var.env
  }
}

resource "aws_alb" "zerepoch" {
  subnets         = aws_subnet.public.*.id
  security_groups = [aws_security_group.public_alb.id]
  internal        = false

  tags = {
    Name        = "${local.project}_${var.env}_public_alb"
    Project     = local.project
    Environment = var.env
  }
}

resource "aws_lb_listener" "redirect" {
  load_balancer_arn = aws_alb.zerepoch.arn
  port              = "80"
  protocol          = "HTTP"

  default_action {
    type = "redirect"

    redirect {
      port        = "443"
      protocol    = "HTTPS"
      status_code = "HTTP_301"
    }
  }
}

resource "aws_alb_listener" "playground" {
  load_balancer_arn = aws_alb.zerepoch.arn
  port              = "443"
  protocol          = "HTTPS"
  certificate_arn   = aws_acm_certificate.zerepoch_private.arn

  default_action {
    target_group_arn = aws_alb_target_group.webghc.arn
    type             = "forward"
  }
}

resource "aws_lb_listener_certificate" "simeon_finance_io" {
  listener_arn    = aws_alb_listener.playground.arn
  certificate_arn = aws_acm_certificate.simeon_finance_io.arn
}

resource "aws_lb_listener_certificate" "simeon_web" {
  listener_arn    = aws_alb_listener.playground.arn
  certificate_arn = aws_acm_certificate.simeon_web_private.arn
}

resource "aws_lb_listener_certificate" "simeon" {
  listener_arn    = aws_alb_listener.playground.arn
  certificate_arn = aws_acm_certificate.simeon_private.arn
}

resource "aws_lb_listener_certificate" "simeon_dash" {
  listener_arn    = aws_alb_listener.playground.arn
  certificate_arn = aws_acm_certificate.simeon_dash_private.arn
}

resource "aws_alb_listener_rule" "simeon-web" {
  listener_arn = aws_alb_listener.playground.arn
  action {
    type             = "forward"
    target_group_arn = aws_alb_target_group.simeon_web.id
  }

  condition {
    host_header {
      values = [local.simeon_web_domain_name]
    }
  }
}

resource "aws_alb_target_group" "simeon_web" {
  port     = "80"
  protocol = "HTTP"
  vpc_id   = aws_vpc.zerepoch.id
}

resource "aws_alb_target_group_attachment" "simeon_web" {
  target_group_arn = aws_alb_target_group.simeon_web.arn
  target_id        = aws_instance.playgrounds_a.id
  port             = local.simeon_web_port
}

resource "aws_route53_record" "simeon_web_alb" {
  zone_id = var.simeon_web_public_zone
  name    = local.simeon_web_domain_name
  type    = "A"

  alias {
    name                   = aws_alb.zerepoch.dns_name
    zone_id                = aws_alb.zerepoch.zone_id
    evaluate_target_health = true
  }
}

## ALB rule for web-ghc
resource "aws_alb_target_group" "webghc" {
  # ALB is taking care of SSL termination so we listen to port 80 here
  port     = "80"
  protocol = "HTTP"
  vpc_id   = aws_vpc.zerepoch.id

  health_check {
    path = "/health"
  }

  stickiness {
    type = "lb_cookie"
  }
}

resource "aws_alb_listener_rule" "runghc" {
  depends_on   = [aws_alb_target_group.webghc]
  listener_arn = aws_alb_listener.playground.arn
  priority     = 100

  action {
    type             = "forward"
    target_group_arn = aws_alb_target_group.webghc.id
  }

  condition {
    path_pattern {
      values = ["/runghc"]
    }
  }
}

resource "aws_alb_target_group_attachment" "webghc_a" {
  target_group_arn = aws_alb_target_group.webghc.arn
  target_id        = aws_instance.webghc_a.id
  port             = "80"
}

## ALB rule for simeon-dashboard
resource "aws_alb_target_group" "simeon_dash" {
  # ALB is taking care of SSL termination so we listen to port 80 here
  port     = "80"
  protocol = "HTTP"
  vpc_id   = aws_vpc.zerepoch.id

  health_check {
    path = "/"
  }

  stickiness {
    type = "lb_cookie"
  }
}

resource "aws_alb_listener_rule" "simeon_dash" {
  depends_on   = [aws_alb_target_group.simeon_dash]
  listener_arn = aws_alb_listener.playground.arn
  priority     = 114

  action {
    type             = "forward"
    target_group_arn = aws_alb_target_group.simeon_dash.id
  }

  condition {
    host_header {
      values = [local.simeon_dash_domain_name]
    }
  }
}

resource "aws_alb_target_group_attachment" "simeon_dash_a" {
  target_group_arn = aws_alb_target_group.simeon_dash.arn
  target_id        = aws_instance.simeon_dash_a.id
  port             = local.pab_port
}

resource "aws_route53_record" "simeon_dash_alb" {
  zone_id = var.simeon_dash_public_zone
  name    = local.simeon_dash_domain_name
  type    = "A"

  alias {
    name                   = aws_alb.zerepoch.dns_name
    zone_id                = aws_alb.zerepoch.zone_id
    evaluate_target_health = true
  }
}

## ALB rule for simeon-playground
resource "aws_alb_target_group" "simeon_playground" {
  # ALB is taking care of SSL termination so we listen to port 80 here
  port     = "80"
  protocol = "HTTP"
  vpc_id   = aws_vpc.zerepoch.id

  health_check {
    path = "/version"
  }

  stickiness {
    type = "lb_cookie"
  }
}

resource "aws_alb_listener_rule" "simeon_playground" {
  depends_on   = [aws_alb_target_group.simeon_playground]
  listener_arn = aws_alb_listener.playground.arn
  priority     = 115

  action {
    type             = "forward"
    target_group_arn = aws_alb_target_group.simeon_playground.id
  }

  condition {
    host_header {
      values = [local.simeon_domain_name]
    }
  }
}

resource "aws_alb_target_group_attachment" "simeon_playground_a" {
  target_group_arn = aws_alb_target_group.simeon_playground.arn
  target_id        = aws_instance.playgrounds_a.id
  port             = local.simeon_playground_port
}

resource "aws_route53_record" "simeon_playground_alb" {
  zone_id = var.simeon_public_zone
  name    = local.simeon_domain_name
  type    = "A"

  alias {
    name                   = aws_alb.zerepoch.dns_name
    zone_id                = aws_alb.zerepoch.zone_id
    evaluate_target_health = true
  }
}

## ALB rule for zerepoch-playground
resource "aws_alb_target_group" "zerepoch_playground" {
  # ALB is taking care of SSL termination so we listen to port 80 here
  port     = "80"
  protocol = "HTTP"
  vpc_id   = aws_vpc.zerepoch.id

  health_check {
    path = "/version"
  }

  stickiness {
    type = "lb_cookie"
  }
}

resource "aws_alb_listener_rule" "zerepoch_playground" {
  depends_on   = [aws_alb_target_group.zerepoch_playground]
  listener_arn = aws_alb_listener.playground.arn
  priority     = 116

  action {
    type             = "forward"
    target_group_arn = aws_alb_target_group.zerepoch_playground.id
  }

  condition {
    host_header {
      values = [local.zerepoch_domain_name]
    }
  }
}

resource "aws_alb_target_group_attachment" "zerepoch_playground_a" {
  target_group_arn = aws_alb_target_group.zerepoch_playground.arn
  target_id        = aws_instance.playgrounds_a.id
  port             = local.zerepoch_playground_port
}

resource "aws_route53_record" "zerepoch_playground_alb" {
  zone_id = var.zerepoch_public_zone
  name    = local.zerepoch_domain_name
  type    = "A"

  alias {
    name                   = aws_alb.zerepoch.dns_name
    zone_id                = aws_alb.zerepoch.zone_id
    evaluate_target_health = true
  }
}


#
# Production: simeon-finance.io forwarding
#

resource "aws_alb_listener_rule" "simeon-finance-simeon-web" {
  listener_arn = aws_alb_listener.playground.arn
  action {
    type             = "forward"
    target_group_arn = aws_alb_target_group.simeon_web.id
  }

  condition {
    host_header {
      values = ["simeon-finance.io"]
    }
  }
}

resource "aws_alb_listener_rule" "simeon-finance-simeon-dash" {
  depends_on   = [aws_alb_target_group.simeon_dash]
  listener_arn = aws_alb_listener.playground.arn

  action {
    type             = "forward"
    target_group_arn = aws_alb_target_group.simeon_dash.id
  }

  condition {
    host_header {
      values = ["run.simeon-finance.io"]
    }
  }
}

resource "aws_alb_listener_rule" "simeon-finance-simeon-playground" {
  depends_on   = [aws_alb_target_group.simeon_playground]
  listener_arn = aws_alb_listener.playground.arn

  action {
    type             = "forward"
    target_group_arn = aws_alb_target_group.simeon_playground.id
  }

  condition {
    host_header {
      values = ["play.simeon-finance.io"]
    }
  }
}

