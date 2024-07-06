table.insert (alsa_monitor.rules, {
  matches = {
    {
      { "node.name", "matches", "alsa_output.pci-0000_00_1f.3-platform-skl_hda_dsp_generic.HiFi__hw_sofhdadsp_3__sink" },
    },
    {
      { "node.name", "matches", "alsa_output.pci-0000_00_1f.3-platform-skl_hda_dsp_generic.HiFi__hw_sofhdadsp_4__sink" },
    },
    {
      { "node.name", "matches", "alsa_output.pci-0000_00_1f.3-platform-skl_hda_dsp_generic.HiFi__hw_sofhdadsp_5__sink" },
    },
  },
  apply_properties = {
    ["node.disabled"] = true,
  }
})
