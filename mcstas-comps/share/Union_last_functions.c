/*
TODO

update Union master to use the flexible functions

deal with loggers and conditionals
*/

int physics_my(enum process choice, double *my,double *k_initial, union data_transfer_union data_transfer, struct focus_data_struct *focus_data, _class_particle *_particle) {

    int output = 0; // Error return value
    #ifdef PROCESS_DETECTOR
    switch(choice) {
        #ifdef PROCESS_INCOHERENT_DETECTOR
        case Incoherent:
            output = Incoherent_physics_my(my, k_initial, data_transfer, focus_data, _particle);
            break;
        #endif
        #ifdef PROCESS_POWDER_DETECTOR
        case Powder:
            output = Powder_physics_my(my, k_initial, data_transfer, focus_data, _particle);
            break;
        #endif
        #ifdef PROCESS_SINGLE_CRYSTAL_DETECTOR
        case Single_crystal:
            output = Single_crystal_physics_my(my, k_initial, data_transfer, focus_data, _particle);
            break;
        #endif
        #ifdef PROCESS_AF_HB_1D_DETECTOR
        case AF_HB_1D:
            output = AF_HB_1D_physics_my(my, k_initial, data_transfer, focus_data, _particle);
            break;
        #endif
        #ifdef PROCESS_PHONONSIMPLE_DETECTOR
        case PhononSimple:
            output = PhononSimple_physics_my(my, k_initial, data_transfer, focus_data, _particle);
            break;
        #endif
        #ifdef PROCESS_TEXTURE_DETECTOR
        case Texture:
            output = Texture_physics_my(my, k_initial, data_transfer, focus_data, _particle);
            break;
        #endif
        #ifdef PROCESS_INCOHERENTPHONON_DETECTOR
        case IncoherentPhonon:
            output = IncoherentPhonon_physics_my(my, k_initial, data_transfer, focus_data, _particle);
            break;
        #endif
        #ifdef PROCESS_NCRYSTAL_DETECTOR
        case NCrystal:
            output = NCrystal_physics_my(my, k_initial, data_transfer, focus_data, _particle);
            break;
        #endif
        #ifdef PROCESS_TEMPLATE_DETECTOR
        case Template:
            output = Template_physics_my(my, k_initial, data_transfer, focus_data, _particle);
            break;
        #endif
        default:
            printf("physics_my: No scattering process matches input!\n");
            break;
    }
    #endif
    return output;
}
  

int physics_scattering(enum process choice, double *k_final, double *k_initial, double *weight, union data_transfer_union data_transfer, struct focus_data_struct *focus_data, _class_particle *_particle) {

    int output = 0; // Error return value
    #ifdef PROCESS_DETECTOR
    switch(choice) {
        #ifdef PROCESS_INCOHERENT_DETECTOR
        case Incoherent:
            output = Incoherent_physics_scattering(k_final, k_initial, weight, data_transfer, focus_data, _particle);
            break;
        #endif
        #ifdef PROCESS_POWDER_DETECTOR
        case Powder:
            output = Powder_physics_scattering(k_final, k_initial, weight, data_transfer, focus_data, _particle);
            break;
        #endif
        #ifdef PROCESS_SINGLE_CRYSTAL_DETECTOR
        case Single_crystal:
            output = Single_crystal_physics_scattering(k_final, k_initial, weight, data_transfer, focus_data, _particle);
            break;
        #endif
        #ifdef PROCESS_AF_HB_1D_DETECTOR
        case AF_HB_1D:
            output = AF_HB_1D_physics_scattering(k_final, k_initial, weight, data_transfer, focus_data, _particle);
            break;
        #endif
        #ifdef PROCESS_PHONONSIMPLE_DETECTOR
        case PhononSimple:
            output = PhononSimple_physics_scattering(k_final, k_initial, weight, data_transfer, focus_data, _particle);
            break;
        #endif
        #ifdef PROCESS_TEXTURE_DETECTOR
        case Texture:
            output = Texture_physics_scattering(k_final, k_initial, weight, data_transfer, focus_data, _particle);
            break;
        #endif
        #ifdef PROCESS_INCOHERENTPHONON_DETECTOR
        case IncoherentPhonon:
            output = IncoherentPhonon_physics_scattering(k_final, k_initial, weight, data_transfer, focus_data, _particle);
            break;
        #endif
        #ifdef PROCESS_NCRYSTAL_DETECTOR
        case NCrystal:
            output = NCrystal_physics_scattering(k_final, k_initial, weight, data_transfer, focus_data, _particle);
            break;
        #endif
        #ifdef PROCESS_TEMPLATE_DETECTOR
        case Template:
            output = Incoherent_physics_scattering(k_final, k_initial, weight, data_transfer, focus_data, _particle);
            break;
        #endif
        default: printf("physics_scattering: No scattering process matches input!\n");
            break;
    }
    #endif
    return output;
}
