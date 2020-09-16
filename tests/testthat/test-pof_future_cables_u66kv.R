

cable_types = c("33kV UG Cable (Gas)",
                "66kV UG Cable (Gas)",
                "33kV UG Cable (Non Pressurised)",
                "66kV UG Cable (Non Pressurised)",
                "33kV UG Cable (Oil)",
                "66kV UG Cable (Oil)")

sub_divisions = c("Aluminium sheath - Aluminium conductor",
                  "Aluminium sheath - Copper conductor",
                  "Lead sheath - Aluminium conductor",
                  "Lead sheath - Copper conductor")

sheath_tests = c("Pass",
                 "Failed Minor",
                 "Failed Major",
                 "Default")

partial_discharges = c("Low", "Medium", "High", "Default")

leakages = c("No (or very low) historic leakage recorded",
             "Low/ moderate", "High", "Very High", "Default")
age = 1:10

simulation_end_year = 100

liste_cabler <- list()

for (c in cable_types) {
  for (s in sub_division) {
    for (st in sheath_tests) {
      for (pd in partial_discharges) {
        for (l in leakages) {
          for (a in age) {
            res <- pof_future_cables_66_33kv(cable_type = c,
                                             sub_division = s,
                                             utilisation_pct = "default",
                                             operating_voltage_pct = "default",
                                             sheath_test = st,
                                             partial_discharge = pd,
                                             fault_hist = "No historic faults recorded",
                                             leakage = l,
                                             age = a,
                                             simulation_end_year = 100)


            liste_cabler[[paste0(c,s,st,pd,l,a)]] <- res
          }
        }
      }
    }
  }
}

