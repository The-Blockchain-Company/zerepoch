# Zerepoch Playground SSL Certificate
resource "aws_acm_certificate" "zerepoch_private" {
  domain_name       = "*.${var.zerepoch_tld}"
  validation_method = "DNS"
}

resource "aws_route53_record" "zerepoch_private" {
  for_each = {
    for dvo in aws_acm_certificate.zerepoch_private.domain_validation_options : dvo.domain_name => {
      name   = dvo.resource_record_name
      record = dvo.resource_record_value
      type   = dvo.resource_record_type
    }
  }

  allow_overwrite = true
  name            = each.value.name
  records         = [each.value.record]
  ttl             = 60
  type            = each.value.type
  zone_id         = var.zerepoch_public_zone
}

resource "aws_acm_certificate_validation" "zerepoch_private" {
  certificate_arn         = aws_acm_certificate.zerepoch_private.arn
  validation_record_fqdns = [for record in aws_route53_record.zerepoch_private : record.fqdn]
}


# Simeon Playground SSL Certificate
resource "aws_acm_certificate" "simeon_private" {
  domain_name       = "*.${var.simeon_tld}"
  validation_method = "DNS"
}

resource "aws_route53_record" "simeon_private" {
  for_each = {
    for dvo in aws_acm_certificate.simeon_private.domain_validation_options : dvo.domain_name => {
      name   = dvo.resource_record_name
      record = dvo.resource_record_value
      type   = dvo.resource_record_type
    }
  }

  allow_overwrite = true
  name            = each.value.name
  records         = [each.value.record]
  ttl             = 60
  type            = each.value.type
  zone_id         = var.simeon_public_zone
}

resource "aws_acm_certificate_validation" "simeon_private" {
  certificate_arn         = aws_acm_certificate.simeon_private.arn
  validation_record_fqdns = [for record in aws_route53_record.simeon_private : record.fqdn]
}

# Simeon Dash SSL Certificate
resource "aws_acm_certificate" "simeon_dash_private" {
  domain_name       = "*.${var.simeon_dash_tld}"
  validation_method = "DNS"
}

resource "aws_route53_record" "simeon_dash_private" {
  for_each = {
    for dvo in aws_acm_certificate.simeon_dash_private.domain_validation_options : dvo.domain_name => {
      name   = dvo.resource_record_name
      record = dvo.resource_record_value
      type   = dvo.resource_record_type
    }
  }

  allow_overwrite = true
  name            = each.value.name
  records         = [each.value.record]
  ttl             = 60
  type            = each.value.type
  zone_id         = var.simeon_dash_public_zone
}

resource "aws_acm_certificate_validation" "simeon_dash_private" {
  certificate_arn         = aws_acm_certificate.simeon_dash_private.arn
  validation_record_fqdns = [for record in aws_route53_record.simeon_dash_private : record.fqdn]
}

# Simeon Web SSL Certificate
resource "aws_acm_certificate" "simeon_web_private" {
  domain_name       = "*.${var.simeon_web_tld}"
  validation_method = "DNS"
}

resource "aws_route53_record" "simeon_web_private" {
  for_each = {
    for dvo in aws_acm_certificate.simeon_web_private.domain_validation_options : dvo.domain_name => {
      name   = dvo.resource_record_name
      record = dvo.resource_record_value
      type   = dvo.resource_record_type
    }
  }

  allow_overwrite = true
  name            = each.value.name
  records         = [each.value.record]
  ttl             = 60
  type            = each.value.type
  zone_id         = var.simeon_web_public_zone
}

resource "aws_acm_certificate_validation" "simeon_web_private" {
  certificate_arn         = aws_acm_certificate.simeon_web_private.arn
  validation_record_fqdns = [for record in aws_route53_record.simeon_web_private : record.fqdn]
}

#
# simeon-finance.io certificates
#

resource "aws_acm_certificate" "simeon_finance_io" {
  domain_name               = "simeon-finance.io"
  validation_method         = "DNS"
  subject_alternative_names = ["*.simeon-finance.io"]
}

resource "aws_route53_record" "simeon_finance_io" {
  for_each = {
    for dvo in aws_acm_certificate.simeon_finance_io.domain_validation_options : dvo.domain_name => {
      name   = dvo.resource_record_name
      record = dvo.resource_record_value
      type   = dvo.resource_record_type
    }
  }

  allow_overwrite = true
  name            = each.value.name
  records         = [each.value.record]
  ttl             = 60
  type            = each.value.type
  zone_id         = var.simeon_finance_io_public_zone
}

resource "aws_acm_certificate_validation" "simeon_finance_io" {
  certificate_arn         = aws_acm_certificate.simeon_finance_io.arn
  validation_record_fqdns = [for record in aws_route53_record.simeon_finance_io : record.fqdn]
}
