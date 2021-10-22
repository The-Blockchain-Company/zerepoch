data "template_file" "ssh_config_section_webghc_a" {
  template = file("${path.module}/templates/ssh-config")

  vars = {
    full_hostname    = "webghc-a.${aws_route53_zone.zerepoch_private_zone.name}"
    short_hostname   = "webghc-a.${local.project}"
    ip               = aws_instance.webghc_a.private_ip
    bastion_hostname = aws_instance.bastion.*.public_ip[0]
    user_name        = "root"
  }
}

data "template_file" "ssh_config_section_simeon_dash_a" {
  template = file("${path.module}/templates/ssh-config")

  vars = {
    full_hostname    = "simeon-dash-a.${aws_route53_zone.zerepoch_private_zone.name}"
    short_hostname   = "simeon-dash-a.${local.project}"
    ip               = aws_instance.simeon_dash_a.private_ip
    bastion_hostname = aws_instance.bastion.*.public_ip[0]
    user_name        = "root"
  }
}

data "template_file" "ssh_config_section_playgrounds_a" {
  template = file("${path.module}/templates/ssh-config")

  vars = {
    full_hostname    = "playgrounds-a.${aws_route53_zone.zerepoch_private_zone.name}"
    short_hostname   = "playgrounds-a.${local.project}"
    ip               = aws_instance.playgrounds_a.private_ip
    bastion_hostname = aws_instance.bastion.*.public_ip[0]
    user_name        = "root"
  }
}

data "template_file" "ssh_config" {
  template = <<EOT
$${webghc_a}

$${simeon_dash_a}

$${playgrounds_a}

Host $${bastion_hostname}
  StrictHostKeyChecking no
EOT

  vars = {
    webghc_a         = data.template_file.ssh_config_section_webghc_a.rendered
    simeon_dash_a   = data.template_file.ssh_config_section_simeon_dash_a.rendered
    playgrounds_a    = data.template_file.ssh_config_section_playgrounds_a.rendered
    bastion_hostname = aws_instance.bastion.*.public_ip[0]
  }
}

resource "local_file" "ssh_config" {
  content  = data.template_file.ssh_config.rendered
  filename = "${pathexpand(var.output_path)}/${local.project}.${var.env}.conf"
}
