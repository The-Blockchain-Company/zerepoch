# This produces a json file with the names and addresses of all the EC2 instances that can then be used by morph
locals {
  webghcA = {
    name = "webghcA"
    ip   = "${element(concat(aws_instance.webghc_a.*.private_ip, list("")), 0)}"
    dns  = "webghc-a.${element(concat(aws_route53_zone.zerepoch_private_zone.*.name, list("")), 0)}"
  }

  simeonDashA = {
    name = "simeonDashA"
    ip   = "${element(concat(aws_instance.simeon_dash_a.*.private_ip, list("")), 0)}"
    dns  = "simeon-dash-a.${element(concat(aws_route53_zone.zerepoch_private_zone.*.name, list("")), 0)}"
  }

  playgroundsA = {
    name = "playgroundsA"
    ip   = "${element(concat(aws_instance.playgrounds_a.*.private_ip, list("")), 0)}"
    dns  = "playgrounds-a.${element(concat(aws_route53_zone.zerepoch_private_zone.*.name, list("")), 0)}"
  }

  machines = {
    webghcA      = "${local.webghcA}"
    simeonDashA = "${local.simeonDashA}"
    playgroundsA = "${local.playgroundsA}"
    rootSshKeys  = local.root_ssh_keys
    awsRegion    = "${var.aws_region}"
    environment  = "${var.env}"
    project      = "${local.project}"
    tld          = "${var.zerepoch_tld}"
    zerepochTld    = "${var.zerepoch_tld}"
    simeonTld   = "${var.simeon_tld}"
  }
}

resource "local_file" "machines" {
  content  = jsonencode(local.machines)
  filename = "${pathexpand(var.output_path)}/machines.json"
}
