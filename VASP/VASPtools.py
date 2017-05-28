from collections import OrderedDict

def vasp_incar_json():
    jstring = """{
    "SYSTEM":"filename",

    "#1 ":"Startparameter for this Run:",
    "NWRITE":"2                   ! LPETIM=F    write-flag & timer",
    "ISTART":"0                   ! job   : 0-new  1-contEcut  2-sameBS",
    "INIWAV":"1                   ! 0-jellium  1-random",
    "IWAVPR":"1                   ! prediction:  0-non 1-charg 2-wave 3-comb",
    "ICHARG":"2                   ! 0-from WF  1-from CHGCAR  2-from atom  11-12-fixed",
    "LWAVE": ".FALSE.             ! determines whether the wavefunctions are written to the WAVECAR file",

    "#2 ":"Electronic Relaxation 1",
    "NELM":"100                   ! number of iterations",
    "EDIFF":"1E-04                ! stopping-criterion for ELM",
    "BMIX":"3.00                  ! sets the cutoff wave vector for Kerker mixing for the magnetization density",
    "ENCUT":"500                  ! Cut-Off Energy",

    "#3 ":"Electronic Relaxation 1",
    "# ALGO":"48                  ! algorithm for the e-relax",
    "LDIAG":"T                    ! sub-space diagonalisation",
    "LREAL":"auto                 ! real-space projection",
    "PREC":"med                   ! accuracy",
    "# NBANDS  ":"30              ! number of bands for diagonalization",

    "#4 ":"Ionic Relaxation",
    "NSW":"200                    ! number of steps for IOM",
    "NBLOCK":"1                   ! inner block",
    "KBLOCK":"10                  ! outer block",
    "IBRION":"2                   ! ionic relax: 0-MD 1-quasi-New 2-CG",
    "ISIF":"3                     ! ion&cell relax: 0-MD 2-ion&stress 3-ion&cell&stress",
    "ISYM":"2                     ! switch symmetry stuff ON (1 or 2) or OFF (0)",
    "# SYMPREC ":" 1e-6",
    "LCORR":"T                    ! Harris-correction to forces",
    "EDIFFG":"-0.04               ! Criterion for geom opt (eV/Ang)",
    "POTIM":"0.50                 ! time-step for ionic motion (fs)",
    "SMASS":"3.00                 ! Nose mass-parameter (am)",

    "#5 DOS":" related values",
    "ISMEAR":"0                   ! Broadening methode -5-tet -1-fermi 0-gaus 1-mp 2-mp2",
    "SIGMA":"0.05                 ! Broadening in eV",
    "LORBIT":"11                  ! l-decomposed DOS",
    "# RWIGS":"1.63  1.00         ! Wigner-Zeits radius",
    "# EMIN":"                    ! Minimum energy for DOS",
    "# EMAX":"                    ! Maximum energy for DOS",
    "# NEDOS":"1001               ! Number of DOS points",
    "# NELECT":"100               ! Total number of electrons",
    "# NUPDOWN":"2                ! Difference between UP&DOWN electrons",

    "#6 Parallelization":"option",
    "LPLANE":"T                   ! Parallelization for PWs",
    "NCORE":"8",
    "LSCALU":"F",
    "NSIM":"4",

    "ISPIN":"1                    ! spin polarized = 2, non spin polarized = 1",
    "# MAGMOM":" 8*0 10*4.5 8*4.5 ! initial magnetic moment for the atoms in the cell",

    "#7 optB86b-vdW functional requires":" vdw_kernel.bindat",
    "# GGA":"MK",
    "# PARAM1":"0.1234",
    "# PARAM2":"1.0000",
    "# LUSE_VDW":".TRUE.",
    "# AGGAC":"0.0000",

    "#8 TS calculation":"         ! default:Nudged Elestic Band method",
    "# ICHAIN":"0                 ! Method (0=NEB, 1=Dynamical matrix, 2=Dimer, 3=Lanczos)",
    "# SPRING":"-5                ! in eV/Ang*2 (sping constant)",
    "# IMAGES":"3                 ! Number of images btw Reactant & Product",
    "# LCLIMB":".true.            ! cNEB: driven up to the saddle point",
    "# LTANGENTOLD":".true.       ! Old central difference tangent",
    "# LDNEB":".true.             ! Modified doubble nudging",
    "# NEBCELL":".true.           ! NEB for variable cell (w/ ISIF=3)",

    "#9 Dipole Correction":"option",
    "# IDIPOL":"3",
    "# LDIPOL":"",

    "#10 lda+u":"parameters",
    "# LMAXMIX":"4",
    "# LDAU":".TRUE.              ! or .FALSE.",
    "# LDAUTYPE":"1               ! or 2",
    "# LDAUL":"2 2 2              ! l-quantum number on which U acts ((1._q,0._q) for each type",
    "# LDAUU":"0 5.00 7.0         ! U coefficient (coulomb interaction) for each species",
    "# LDAUJ":"0 1 1              ! J coefficient (exchange) for each species"
}
"""
    return jstring

def magmom_parameters():
    magmom = {'Mn3+': 4, 'Ni4+': 0.6, 'Cr': 5, 'Mn4+': 3, 'Ta': 5, 'Ni3+': 1, 'Mo': 5,
              'Ni': 2, 'V': 5, 'Mn2+': 5, 'Co': 5, 'Co4+': 1, 'W': 5, 'Fe3+': 5, 'Fe2+': 4,
              'Mn': 5, 'Fe4+': 4, 'Fe': 5, 'Co3+': 0.6,'Li': 0.6, 'O': 0.6}
    return magmom

def ldauu_parameters():
    LDAUU = {'Mo': 4.38, 'V': 3.1, 'Cu': 4, 'W': 4.0, 'Ag': 1.5, 'Cr': 3.5, 'Ta': 2,
             'Nb': 1.5, 'Mn': 3.9, 'Re': 2, 'Co': 3.4, 'Ni': 6, 'Fe': 4.0,
             'Li': 0, 'O': 0}
    return LDAUU

def ldauj_parameters():
    LDAUJ = {'Mo': 0, 'V': 0, 'Cu': 0, 'W': 0, 'Ag': 0, 'Cr': 0, 'Ta': 0,
             'Nb': 0, 'Mn': 0, 'Re': 0, 'Co': 0, 'Ni': 0, 'Fe': 0,
             'Li': 0, 'O': 0}
    return LDAUJ

def ldaul_parameters():
    LDAUL = {'Mo': 2, 'V': 2, 'Cu': 2, 'W': 2, 'Ag': 2, 'Cr': 2, 'Ta': 2,
             'Nb': 2, 'Mn': 2, 'Re': 2, 'Co': 2, 'Ni': 2, 'Fe': 2,
             'Li': 0, 'O': 0}
    return LDAUL

def vasp_grimme_parameters():
    vdw_C6 = {'H': 0.14, 'He': 0.08, 'Li': 1.61, 'Be': 1.61, 'B': 3.13, 'C': 1.75, 'N': 1.23, 'O': 0.70, 'F': 0.75,
              'Ne': 0.63, 'Na': 5.71,
              'Mg': 5.71, 'Al': 10.79, 'Si': 9.23, 'P': 7.84, 'S': 5.57, 'Cl': 5.07, 'Ar': 4.61, 'K': 10.80,
              'Ca': 10.80,
              'Sc': 10.80, 'Ti': 10.80, 'V': 10.80, 'Cr': 10.80, 'Mn': 10.80, 'Fe': 10.80, 'Co': 10.80, 'Ni': 10.80,
              'Cu': 10.80, 'Zn': 10.80,
              'Ga': 16.99, 'Ge': 17.10, 'As': 16.37, 'Se': 12.64, 'Br': 12.47, 'Kr': 12.01, 'Rb': 24.67,
              'Sr': 24.67,
              'Y': 24.67, 'Zr': 24.67, 'Nb': 24.67, 'Mo': 24.67, 'Tc': 24.67, 'Ru': 24.67, 'Rh': 24.67, 'Pd': 24.67,
              'Ag': 24.67, 'Cd': 24.67,
              'In': 37.32, 'Sn': 38.71, 'Sb': 38.44, 'Te': 31.74, 'I': 31.50, 'Xe': 29.99}

    vdw_R0 = {'H': 1.001, 'He': 1.012, 'Li': 0.825, 'Be': 1.408, 'B': 1.485, 'C': 1.452, 'N': 1.397, 'O': 1.342,
              'F': 1.287, 'Ne': 1.243, 'Na': 1.144,
              'Mg': 1.364, 'Al': 1.716, 'Si': 1.716, 'P': 1.705, 'S': 1.683, 'Cl': 1.639, 'Ar': 1.595, 'K': 1.485,
              'Ca': 1.474,
              'Sc': 1.562, 'Ti': 1.562, 'V': 1.562, 'Cr': 1.562, 'Mn': 1.562, 'Fe': 1.562, 'Co': 1.562, 'Ni': 1.562,
              'Cu': 1.562, 'Zn': 1.562,
              'Ga': 1.650, 'Ge': 1.727, 'As': 1.760, 'Se': 1.771, 'Br': 1.749, 'Kr': 1.727, 'Rb': 1.628,
              'Sr': 1.606,
              'Y': 1.639, 'Zr': 1.639, 'Nb': 1.639, 'Mo': 1.639, 'Tc': 1.639, 'Ru': 1.639, 'Rh': 1.639, 'Pd': 1.639,
              'Ag': 1.639, 'Cd': 1.639,
              'In': 1.672, 'Sn': 1.804, 'Sb': 1.881, 'Te': 1.892, 'I': 1.892, 'Xe': 1.881}

    return vdw_C6, vdw_R0