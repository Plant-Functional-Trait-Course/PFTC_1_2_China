#### HARMONIZE TAXONOMY BETWEEN TRAIT AND COMMUNITY DATA ####

# Taxon = old names in trait data
# speciesName = old names in community data
# new_taxon = new name used in both data sets

taxon_harmonizer <- tibble(Taxon = c("Anaphalis flavescens", "Gentiana pseudoaquatica", "Halenia elliptica", "Juncus leucomelas", "Luzula multiflora", "Maianthemum henryi", "Poa polycolea", "Prunella vulgaris", "Salix souliei", "Carex nibigella") ,
                           
                           speciesName = c("Anaphalis aureopunctata", "Gentiana aquatica var. pseudoaquatica", "Halenia corniculata", "Juncus leucanthus", "Luzula spp", "Maianthemum japonicum", "Poa spp", "Prunella hispida", "Salix brachista", "Carex nibigella"),
                           
                           new_taxon = c("Anaphalis flavescens", "Gentiana pseudoaquatica", "Halenia elliptica", "Juncus leucomelas", "Luzula spp", "Maianthemum henryi", "Poa polycolea", "Prunella vulgaris subsp. hispida", "Salix souliei", "Carex spectabilis"))