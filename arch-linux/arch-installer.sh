# █████╗ ██████╗  ██████╗██╗  ██╗
#██╔══██╗██╔══██╗██╔════╝██║  ██║
#███████║██████╔╝██║     ███████║
#██╔══██║██╔══██╗██║     ██╔══██║
#██║  ██║██║  ██║╚██████╗██║  ██║
#╚═╝  ╚═╝╚═╝  ╚═╝ ╚═════╝╚═╝  ╚═╝

#██╗███╗   ██╗███████╗████████╗ █████╗ ██╗     ██╗
#██║████╗  ██║██╔════╝╚══██╔══╝██╔══██╗██║     ██║
#██║██╔██╗ ██║███████╗   ██║   ███████║██║     ██║
#██║██║╚██╗██║╚════██║   ██║   ██╔══██║██║     ██║
#██║██║ ╚████║███████║   ██║   ██║  ██║███████╗███████╗
#╚═╝╚═╝  ╚═══╝╚══════╝   ╚═╝   ╚═╝  ╚═╝╚══════╝╚══════╝
#
#TODO Terminar o script

#Variables
$BIOS
$home
$swap
$filesystem

#func to break line with echo command
break_line(){
    echo ""
}

#func to get users options of install
choices(){

    #Filesystem
    echo " Do you want to do a cryptsetup?(answer with 'y'or 'n')" && break_line
    echo "->  "
    read crypt_option;
    if [[ $crypt_option -eq "y" ]]; then $filesystem = 1;
    else $filesystem = 0; fi && break_line

    #BIOS
    echo "What type of BIOS your system suport?"
    echo "Please answer  'u'  for UEFI  or  'l'  for LEGACY" && break_line
    echo "->  "
    read bios_option;
    if [[ $bios_option -eq "u" ]]; then $BIOS = 0;
    else $bios_option = 1; fi && break_line

    #root together with home or not
    echo "Do you want a separete HOME particion?(answer with 'y' or 'n')" && break_line
    echo "->  "
    read option;
    if [[ $option -eq "y" ]]; then $home = 1;
    else $home = 0; fi && break_line
}


#func to define BIOS type(UEFI or LEGACY)
bios_type(){

}

#func to create particions
particions(){

}

cryptsetup(){

    echo " Please "

}
